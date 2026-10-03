//! Natural JSON adapters, serde/schemars annotations and exact-array descriptors.

use super::*;

/// The serde/schemars position an `any`-carrying field or arm occupies, selecting which natural
/// adapter (the natural-rendering JSON surface) steers its JSON. Bare `AnyCbor` (`Direct`); a `Vec`
/// element (`Seq`); a stringifiable-keyed `BTreeMap` value (`Map`, non-preserve) or `OrderedHashMap`
/// value (`OrderedMap`, preserve); and the `Option<…>` counterpart of each (paired with
/// `#[serde(default)]`).
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NaturalAnyPosition {
    Direct,
    Optional,
    Seq,
    NonEmptySeq,
    OptSeq,
    StaticSeq(usize),
    OptStaticSeq(usize),
    BoundedSeq(u64, u64),
    OptBoundedSeq(u64, u64),
    BoundedUniqueSeq(u64, u64),
    OptBoundedUniqueSeq(u64, u64),
    Map,
    OptMap,
    OrderedMap,
    OptOrderedMap,
}

/// The natural-JSON adapter a generated record field needs, if its Rust type carries CDDL `any`
/// at a position whose tagged `AnyCbor` schema would misdescribe the JSON surface. Kept beside the
/// adapter enum so every consumer of that emitted-schema boundary uses one classifier.
pub fn natural_any_position(
    ty: &RustType,
    optional: bool,
    cli: &Cli,
) -> Option<NaturalAnyPosition> {
    use NaturalAnyPosition as P;
    let resolves_any = |ty: &RustType| {
        matches!(
            ty.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Any
        )
    };
    match ty.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Any => Some(if optional { P::Optional } else { P::Direct }),
        ConceptualRustType::Array(inner) if resolves_any(inner) => {
            // Alias-aware: a named `[2*3 any]` field resolves shallowly to Array but its checked
            // bounds live on the RustType configuration. The bounded adapter keeps natural JSON's
            // fallible AnyCbor walk AND re-enters the BoundedVec TryFrom door instead of pretending
            // this is Vec<AnyCbor>.
            ty.exact_homogeneous_array_len_checked().map_or_else(
                || {
                    ty.type_enforced_bounded_array_u64_bounds().map_or_else(
                        || Some(if optional { P::OptSeq } else { P::Seq }),
                        |(min, max)| {
                            Some(match (ty.duplicates_reject(), optional) {
                                (true, false) => P::BoundedUniqueSeq(min, max),
                                (true, true) => P::OptBoundedUniqueSeq(min, max),
                                (false, false) => P::BoundedSeq(min, max),
                                (false, true) => P::OptBoundedSeq(min, max),
                            })
                        },
                    )
                },
                |len| {
                    Some(if optional {
                        P::OptStaticSeq(len)
                    } else {
                        P::StaticSeq(len)
                    })
                },
            )
        }
        // An `any`-keyed table stays tagged (its key already errors at runtime per RFC 8949 §6.1),
        // so require a non-`any` key. Preserve → `OrderedHashMap`, else `BTreeMap`.
        ConceptualRustType::Map(key, value) if resolves_any(value) && !resolves_any(key) => {
            Some(match (cli.preserve_encodings, optional) {
                (false, false) => P::Map,
                (false, true) => P::OptMap,
                (true, false) => P::OrderedMap,
                (true, true) => P::OptOrderedMap,
            })
        }
        _ => None,
    }
}

/// The `#[serde(with = …)]` / `#[schemars(schema_with = …)]` / `#[serde(default)]` annotation lines
/// that route a serde field/arm carrying `any` through the NATURAL JSON walk instead of
/// `AnyCbor`'s tagged codec (which stays `AnyCbor`'s own serde). Returns empty when
/// neither json flag is on. The adapter module / schema fn live in the `any_cbor` runtime module,
/// reached through the same common-import glue as the `AnyCbor` type itself (`common_import_rust`),
/// so `--common-import-override` split crates spell the shared-core path.
pub fn natural_any_serde_annotations(cli: &Cli, pos: NaturalAnyPosition) -> Vec<String> {
    use NaturalAnyPosition::*;
    let mut out = Vec::new();
    let base = format!("{}::any_cbor", cli.common_import_rust());
    // (serde adapter module, permissive schema fn, needs `#[serde(default)]`). One permissive schema
    // serves both required and optional (an empty/array/object-with-any schema accepts null/absent);
    // required-ness is derived from the field's `Option<..>`-ness, not from `schema_with`.
    let (with_mod, schema_fn, optional) = match pos {
        Direct => (
            "natural_any_cbor",
            "natural_any_cbor_schema".to_owned(),
            false,
        ),
        Optional => (
            "natural_any_cbor_opt",
            "natural_any_cbor_schema".to_owned(),
            true,
        ),
        Seq => (
            "natural_any_cbor_seq",
            "natural_any_cbor_seq_schema".to_owned(),
            false,
        ),
        NonEmptySeq => (
            "natural_any_cbor_non_empty_seq",
            "natural_any_cbor_non_empty_seq_schema".to_owned(),
            false,
        ),
        OptSeq => (
            "natural_any_cbor_opt_seq",
            "natural_any_cbor_seq_schema".to_owned(),
            true,
        ),
        StaticSeq(len) => (
            "natural_any_cbor_static_seq",
            format!("natural_any_cbor_static_seq_schema::<{len}>"),
            false,
        ),
        OptStaticSeq(len) => (
            "natural_any_cbor_opt_static_seq",
            format!("natural_any_cbor_opt_static_seq_schema::<{len}>"),
            true,
        ),
        BoundedSeq(min, max) => (
            "natural_any_cbor_bounded_seq",
            format!("natural_any_cbor_bounded_seq_schema::<{min}, {max}>"),
            false,
        ),
        OptBoundedSeq(min, max) => (
            "natural_any_cbor_opt_bounded_seq",
            format!("natural_any_cbor_bounded_seq_schema::<{min}, {max}>"),
            true,
        ),
        BoundedUniqueSeq(min, max) => (
            "natural_any_cbor_bounded_ordered_set",
            format!("natural_any_cbor_bounded_ordered_set_schema::<{min}, {max}>"),
            false,
        ),
        OptBoundedUniqueSeq(min, max) => (
            "natural_any_cbor_opt_bounded_ordered_set",
            format!("natural_any_cbor_bounded_ordered_set_schema::<{min}, {max}>"),
            true,
        ),
        Map => (
            "natural_any_cbor_btreemap",
            "natural_any_cbor_map_schema".to_owned(),
            false,
        ),
        OptMap => (
            "natural_any_cbor_opt_btreemap",
            "natural_any_cbor_map_schema".to_owned(),
            true,
        ),
        OrderedMap => (
            "natural_any_cbor_orderedmap",
            "natural_any_cbor_map_schema".to_owned(),
            false,
        ),
        OptOrderedMap => (
            "natural_any_cbor_opt_orderedmap",
            "natural_any_cbor_map_schema".to_owned(),
            true,
        ),
    };
    if cli.json_serde_derives {
        out.push(format!("#[serde(with = \"{base}::{with_mod}\")]"));
        if optional {
            // A `#[serde(with)]` field is otherwise required on read; `default` restores the
            // ordinary "missing optional key ⇒ None" behavior the plain derive gives.
            out.push("#[serde(default)]".to_owned());
        }
    }
    if cli.json_schema_export {
        out.push(format!(
            "#[schemars(schema_with = \"{base}::{schema_fn}\")]"
        ));
    }
    out
}

/// JSON annotations for a recursive static-array sequence tree. The pinned serde/schemars only
/// implement array traits through length 32, so the descriptor owns every sequence carrier between
/// a field/payload and an exact array. Restricted carriers decode through their native `TryFrom`
/// door; `any` leaves use their natural adapter rather than `AnyCbor`'s tagged codec.
pub(crate) fn recursive_exact_array_descriptor(
    types: &IntermediateTypes,
    ty: &RustType,
    field_optional: bool,
    prefer_legacy_direct_typed_sequence: bool,
    cli: &Cli,
) -> Option<(String, String)> {
    fn alias_base<'a>(types: &'a IntermediateTypes, ty: &'a RustType) -> Option<&'a RustType> {
        match ty.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Rust(ident) => types
                .type_aliases()
                .get(&AliasIdent::Rust(ident.clone()))
                .map(|alias| &alias.base_type),
            _ => None,
        }
    }

    fn contains_exact_natural_any(
        types: &IntermediateTypes,
        ty: &RustType,
        under_exact: bool,
    ) -> bool {
        if let Some(base) = alias_base(types, ty) {
            return contains_exact_natural_any(types, base, under_exact);
        }
        match ty.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Any => under_exact,
            ConceptualRustType::Array(inner) => contains_exact_natural_any(
                types,
                inner,
                under_exact || ty.exact_homogeneous_array_len_checked().is_some(),
            ),
            ConceptualRustType::Optional(inner) => {
                contains_exact_natural_any(types, inner, under_exact)
            }
            ConceptualRustType::Map(key, value) => {
                contains_exact_natural_any(types, key, under_exact)
                    || contains_exact_natural_any(types, value, under_exact)
            }
            _ => false,
        }
    }

    fn contains_wide_static_array(types: &IntermediateTypes, ty: &RustType) -> bool {
        if let Some(base) = alias_base(types, ty) {
            return contains_wide_static_array(types, base);
        }
        ty.exact_homogeneous_array_len_checked()
            .is_some_and(|len| len > 32)
            || match ty.conceptual_type.resolve_alias_shallow() {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    contains_wide_static_array(types, inner)
                }
                ConceptualRustType::Map(key, value) => {
                    contains_wide_static_array(types, key)
                        || contains_wide_static_array(types, value)
                }
                _ => false,
            }
    }

    fn contains_exact_node(types: &IntermediateTypes, ty: &RustType) -> bool {
        if let Some(base) = alias_base(types, ty) {
            return contains_exact_node(types, base);
        }
        if ty.exact_homogeneous_array_len_checked().is_some() {
            return true;
        }
        match ty.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                contains_exact_node(types, inner)
            }
            ConceptualRustType::Map(key, value) => {
                contains_exact_node(types, key) || contains_exact_node(types, value)
            }
            _ => false,
        }
    }

    // Preserve the long-standing direct typed `Vec<[T; N]>` callback. It is narrower than the
    // recursive descriptor: the outer carrier is an ordinary required Vec, its immediate exact
    // element is typed, and anything beneath that element still has ordinary serde/schemars traits.
    // In particular, `T` may be a map — `static_array_seq` adapts only the exact-array handover and
    // never tries to model the map itself. Optional/direct-any/deeper wide-or-natural-exact shapes
    // need the descriptor instead.
    fn is_legacy_direct_typed_static_array_sequence(
        types: &IntermediateTypes,
        ty: &RustType,
        field_optional: bool,
    ) -> bool {
        if field_optional
            || ty.duplicates_reject()
            || !matches!(ty.config.occurrence_bounds(), None | Some((None, None)))
        {
            return false;
        }
        let ConceptualRustType::Array(element) = ty.conceptual_type.resolve_alias_shallow() else {
            return false;
        };
        let ConceptualRustType::Array(inner) = element.conceptual_type.resolve_alias_shallow()
        else {
            return false;
        };
        element.exact_homogeneous_array_len_checked().is_some()
            && !contains_wide_static_array(types, inner)
            && !contains_exact_natural_any(types, ty, false)
    }

    fn shape(types: &IntermediateTypes, ty: &RustType, base: &str) -> Option<String> {
        if let Some(alias) = alias_base(types, ty) {
            // A named table's occurrence/duplicate policy lives on its RustStruct config rather
            // than the transparent alias base. Restore it before structural dispatch: otherwise
            // `named_preserve = {* K => V} ; @duplicates preserve` would incorrectly select the
            // object-map descriptor when referenced through a second alias.
            if let ConceptualRustType::Rust(ident) = &ty.conceptual_type
                && let Some(owner) = types.rust_struct(ident)
                && let RustStructType::Table { bounds, .. } = owner.variant()
            {
                let mut configured = alias.clone();
                configured.config.duplicates = owner.config().duplicates;
                if let Some(bounds) = bounds {
                    configured.config.bounds =
                        Some(crate::intermediate::TypeBounds::Occurrence(*bounds));
                }
                return shape(types, &configured, base);
            }
            return shape(types, alias, base);
        }
        match ty.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Any => Some(format!("{base}::NaturalAny")),
            ConceptualRustType::Array(inner) => {
                // Exact recognition must precede the restricted-sequence cases: `1*1` is the
                // native `[T; 1]`, never `NonEmptyVec<T>`. Exact reject sets intentionally fall
                // through to their bounded ordered-set descriptor: `[T; N]` cannot be unique.
                if let Some(len) = ty.exact_homogeneous_array_len_checked() {
                    Some(format!(
                        "{base}::Exact<{}, {len}>",
                        shape(types, inner, base)?
                    ))
                } else if ty.duplicates_reject() {
                    let inner = shape(types, inner, base)?;
                    match ty.config.occurrence_bounds() {
                        None | Some((None, None)) => Some(format!("{base}::RejectSet<{inner}>")),
                        Some((Some(1), None)) => {
                            Some(format!("{base}::RejectSetNonEmpty<{inner}>"))
                        }
                        Some((min, max)) => Some(format!(
                            "{base}::RejectSetBounded<{inner}, {}, {}>",
                            min.unwrap_or(0),
                            max.unwrap_or(i128::from(u64::MAX))
                        )),
                    }
                } else {
                    let inner = shape(types, inner, base)?;
                    match ty.config.occurrence_bounds() {
                        None | Some((None, None)) => Some(format!("{base}::Loose<{inner}>")),
                        Some((Some(1), None)) => Some(format!("{base}::NonEmpty<{inner}>")),
                        Some((min, max)) => Some(format!(
                            "{base}::Bounded<{inner}, {}, {}>",
                            min.unwrap_or(0),
                            max.unwrap_or(i128::from(u64::MAX))
                        )),
                    }
                }
            }
            ConceptualRustType::Optional(inner) => {
                Some(format!("{base}::Optional<{}>", shape(types, inner, base)?))
            }
            ConceptualRustType::Map(key, value) => {
                // Ordinary tables serialize as JSON objects, so their keys retain serde's native
                // member-name path. A recursively adapted key would be an array (or natural-any
                // array) and has no object-member representation; the IR preclaim keeps that
                // boundary loud. Pair maps are positional JSON arrays of pairs, so both halves
                // compose through the descriptor.
                // Transparent named-table aliases retain their inner `Map` node but carry the
                // table policy on the owning struct. Recover it here as well as for a bare Rust
                // ident: an alias-of-a-preserve-table must remain a positional PairMap.
                let alias_table = {
                    let mut current = &ty.conceptual_type;
                    let mut found = None;
                    while let ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) = current {
                        if let Some(alias) =
                            types.type_aliases().get(&AliasIdent::Rust(ident.clone()))
                        {
                            found = Some((
                                alias.base_type.config.duplicates,
                                alias.base_type.config.occurrence_bounds(),
                            ));
                        }
                        if let Some(owner) = types.rust_struct(ident)
                            && let RustStructType::Table { bounds, .. } = owner.variant()
                        {
                            found = Some((
                                owner.config().duplicates,
                                bounds.map(crate::intermediate::OccurrenceWindow::raw),
                            ));
                        }
                        current = inner;
                    }
                    found
                };
                let pair_map = ty.is_preserve_pair_map()
                    || alias_table.is_some_and(|(duplicates, _)| {
                        duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    });
                let bounds = alias_table
                    .and_then(|(_, bounds)| bounds)
                    .or_else(|| {
                        ty.type_enforced_bounded_map_u64_bounds().map(|(min, max)| {
                            (
                                Some(i128::from(min)),
                                (max != u64::MAX).then_some(i128::from(max)),
                            )
                        })
                    })
                    .or(ty.config.occurrence_bounds());
                let key = if pair_map {
                    shape(types, key, base)?
                } else if contains_wide_static_array(types, key)
                    || contains_exact_natural_any(types, key, false)
                {
                    return None;
                } else {
                    format!("{base}::Leaf")
                };
                let value = shape(types, value, base)?;
                if pair_map {
                    if let Some((min, max)) = bounds.and_then(|(min, max)| {
                        let min = min.unwrap_or(0).try_into().ok()?;
                        let max = max
                            .map(|max| max.try_into().ok())
                            .unwrap_or(Some(u64::MAX))?;
                        ((min, max) != (1, u64::MAX)).then_some((min, max))
                    }) {
                        Some(format!(
                            "{base}::BoundedPairMap<{key}, {value}, {min}, {max}>"
                        ))
                    } else if bounds == Some((Some(1), None)) || ty.is_type_enforced_non_empty() {
                        Some(format!("{base}::NonEmptyPairMap<{key}, {value}>"))
                    } else {
                        Some(format!("{base}::PairMap<{key}, {value}>"))
                    }
                } else if let Some((min, max)) = bounds.and_then(|(min, max)| {
                    let min = min.unwrap_or(0).try_into().ok()?;
                    let max = max
                        .map(|max| max.try_into().ok())
                        .unwrap_or(Some(u64::MAX))?;
                    ((min, max) != (1, u64::MAX)).then_some((min, max))
                }) {
                    Some(format!("{base}::BoundedMap<{value}, {min}, {max}>"))
                } else if bounds == Some((Some(1), None)) || ty.is_type_enforced_non_empty() {
                    Some(format!("{base}::NonEmptyMap<{value}>"))
                } else {
                    Some(format!("{base}::Map<{value}>"))
                }
            }
            _ => Some(format!("{base}::Leaf")),
        }
    }

    let root_is_direct_exact = match ty.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Array(inner) => {
            ty.exact_homogeneous_array_len_checked().is_some()
                && !contains_exact_node(types, inner)
                && !contains_exact_natural_any(types, ty, false)
        }
        _ => false,
    };
    if root_is_direct_exact
        || (prefer_legacy_direct_typed_sequence
            && is_legacy_direct_typed_static_array_sequence(types, ty, field_optional))
        || !contains_exact_node(types, ty)
        || (!contains_wide_static_array(types, ty) && !contains_exact_natural_any(types, ty, false))
    {
        return None;
    }
    let base = format!("{}::static_array", cli.common_import_rust());
    let mut descriptor = shape(types, ty, &base)?;
    let mut member_type = ty.for_rust_member(types, false, cli);
    if field_optional {
        descriptor = format!("{base}::Optional<{descriptor}>");
        member_type = format!("Option<{member_type}>");
    }
    Some((descriptor, member_type))
}

/// The hand-written JSON implementations for dynamic map rows do not have a field attribute on
/// which the legacy direct `[T; N]` callback can sit. Give those seams a recursive descriptor even
/// for that otherwise-preserved direct shape.
pub(crate) fn dynamic_row_exact_array_descriptor(
    types: &IntermediateTypes,
    ty: &RustType,
    cli: &Cli,
) -> Option<(String, String)> {
    if let Some(descriptor) = recursive_exact_array_descriptor(types, ty, false, true, cli) {
        return Some(descriptor);
    }
    let ConceptualRustType::Array(inner) = ty.conceptual_type.resolve_alias_shallow() else {
        return None;
    };
    let len = ty.exact_homogeneous_array_len_checked()?;
    if len <= 32
        && !matches!(
            inner.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Any
        )
    {
        return None;
    }
    let base = format!("{}::static_array", cli.common_import_rust());
    let inner = if matches!(
        inner.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Any
    ) {
        format!("{base}::NaturalAny")
    } else {
        format!("{base}::Leaf")
    };
    Some((
        format!("{base}::Exact<{inner}, {len}>"),
        ty.for_rust_member(types, false, cli),
    ))
}

pub fn static_array_serde_annotations(
    types: &IntermediateTypes,
    ty: &RustType,
    optional: bool,
    prefer_legacy_direct_typed_sequence: bool,
    cli: &Cli,
) -> Vec<String> {
    if let Some((descriptor, member_type)) = recursive_exact_array_descriptor(
        types,
        ty,
        optional,
        prefer_legacy_direct_typed_sequence,
        cli,
    ) {
        let base = format!("{}::static_array", cli.common_import_rust());
        let mut out = Vec::new();
        if cli.json_serde_derives {
            out.push(format!(
                "#[serde(serialize_with = \"{base}::serialize_recursive::<{descriptor}, _, _>\", deserialize_with = \"{base}::deserialize_recursive::<{descriptor}, _, _>\")]"
            ));
            if optional {
                out.push("#[serde(default)]".to_owned());
            }
        }
        if cli.json_schema_export {
            out.push(format!(
                "#[schemars(schema_with = \"{base}::recursive_schema::<{descriptor}, {member_type}>\")]"
            ));
        }
        return out;
    }
    let Some(len) = ty.exact_homogeneous_array_len_checked() else {
        return Vec::new();
    };
    let ConceptualRustType::Array(element) = ty.conceptual_type.resolve_alias_shallow() else {
        return Vec::new();
    };
    let base = format!("{}::static_array", cli.common_import_rust());
    let mut out = Vec::new();
    if cli.json_serde_derives {
        let module = if optional {
            "static_array_opt"
        } else {
            "static_array"
        };
        out.push(format!("#[serde(with = \"{base}::{module}\")]"));
        if optional {
            out.push("#[serde(default)]".to_owned());
        }
    }
    if cli.json_schema_export {
        let element = element.for_rust_member(types, false, cli);
        let schema_fn = if optional {
            "static_array_opt_schema"
        } else {
            "static_array_schema"
        };
        out.push(format!(
            "#[schemars(schema_with = \"{base}::{schema_fn}::<{element}, {len}>\")]"
        ));
    }
    out
}

/// JSON annotations for the one field shape that combines two independent `Option` meanings:
/// `? f: (T / null)` stores `Option<Option<T>>`. The recursive descriptor already describes the
/// INNER nullable `Option<T>`; this callback owns only the OUTER presence option. It deliberately
/// replaces, rather than accompanies, the ordinary `double_option` callback because serde allows
/// one field callback. The schema callback likewise describes the inner member type, leaving
/// optional-property requiredness to the field's default/skip shape.
pub fn static_array_double_option_serde_annotations(
    types: &IntermediateTypes,
    ty: &RustType,
    cli: &Cli,
) -> Vec<String> {
    let Some((descriptor, member_type)) =
        recursive_exact_array_descriptor(types, ty, false, true, cli)
    else {
        return Vec::new();
    };
    let base = format!("{}::static_array", cli.common_import_rust());
    let mut out = Vec::new();
    if cli.json_serde_derives {
        out.push(format!(
            "#[serde(serialize_with = \"{base}::serialize_optional_recursive::<{descriptor}, _, _>\", deserialize_with = \"{base}::deserialize_optional_recursive::<{descriptor}, _, _>\")]"
        ));
        out.push("#[serde(default)]".to_owned());
        out.push("#[serde(skip_serializing_if = \"Option::is_none\")]".to_owned());
    }
    if cli.json_schema_export {
        out.push(format!(
            "#[schemars(schema_with = \"{base}::recursive_schema::<{descriptor}, {member_type}>\")]"
        ));
    }
    out
}

/// JSON annotations for a loose homogeneous collection whose elements are exact static arrays.
/// A derive for `Vec<[T; N]>` still asks serde/schemars for traits on the wide inner array, so the
/// sequence adapter owns each element's list-to-array handover instead.
pub fn static_array_sequence_serde_annotations(
    types: &IntermediateTypes,
    ty: &RustType,
    optional: bool,
    cli: &Cli,
) -> Vec<String> {
    // A recursive descriptor owns every non-legacy sequence tree. Required direct typed
    // `Vec<[T; N]>` fields retain their established callback, so the sequence helper is the sole
    // serde/schemars annotation on that narrow surface.
    if recursive_exact_array_descriptor(types, ty, optional, true, cli).is_some() {
        return Vec::new();
    }
    let ConceptualRustType::Array(element) = ty.conceptual_type.resolve_alias_shallow() else {
        return Vec::new();
    };
    let Some(len) = element.exact_homogeneous_array_len_checked() else {
        return Vec::new();
    };
    let ConceptualRustType::Array(inner) = element.conceptual_type.resolve_alias_shallow() else {
        return Vec::new();
    };
    let base = format!("{}::static_array", cli.common_import_rust());
    let mut out = Vec::new();
    if cli.json_serde_derives {
        out.push(format!("#[serde(with = \"{base}::static_array_seq\")]"));
    }
    if cli.json_schema_export {
        let inner = inner.for_rust_member(types, false, cli);
        out.push(format!(
            "#[schemars(schema_with = \"{base}::static_array_seq_schema::<{inner}, {len}>\")]"
        ));
    }
    out
}

/// The serde field annotations for a member that is BOTH optional and nullable
/// (`? f: (T / null)` → a nested `Option<Option<T>>` — `RustField::is_double_option`). serde's plain
/// derive collapses the two `Option`s in both directions: a JSON `null` reads back as the OUTER
/// `None` (absent), and an absent member WRITES as `null` — so the JSON surface loses the
/// present-null value AND cannot distinguish absent from present-null, while the CBOR surface keeps
/// all three states. The three attributes restore them: `with` supplies the adapter (present `null`
/// → `Some(None)`), `default` restores the missing-key ⇒ outer `None` reading a `with` field
/// otherwise loses (a `#[serde(with)]` field is REQUIRED on read), and `skip_serializing_if` writes
/// absent as an OMITTED key rather than `null`. Returns empty without `--json-serde-derives` — a
/// `#[serde(…)]` attribute with no serde derive in scope does not compile.
///
/// The `schemars` half is a NEUTRALIZER, not a schema change. `schemars`' derive reads
/// `#[serde(with = …)]` as its OWN `with`, whose argument is a TYPE — so the adapter's module path
/// reaches it as `expected type, found module` (E0573, a crate that does not compile) whenever both
/// json flags are on. `#[schemars(with = "<the field's own rust type>")]` takes precedence and hands
/// it back exactly the type it would have read without the serde attribute, so the emitted schema is
/// byte-for-byte the one the plain derive produced: nullable-`T`, non-required (`schemars` reads the
/// `default` / `skip_serializing_if` pair for required-ness, which `with` does not affect). Emitted
/// only when BOTH flags are on — there is no `#[serde(with)]` to neutralize otherwise.
///
/// The adapter module lives in the `double_option` runtime module, reached through the same common-
/// import glue as the other runtimes (`common_import_rust`) so `--common-import-override` split
/// crates spell the shared-core path.
pub fn double_option_serde_annotations(cli: &Cli, member_type: &str) -> Vec<String> {
    if !cli.json_serde_derives {
        return Vec::new();
    }
    let mut out = vec![
        format!(
            "#[serde(with = \"{}::double_option\")]",
            cli.common_import_rust()
        ),
        "#[serde(default)]".to_owned(),
        "#[serde(skip_serializing_if = \"Option::is_none\")]".to_owned(),
    ];
    if cli.json_schema_export {
        out.push(format!("#[schemars(with = \"{member_type}\")]"));
    }
    out
}
