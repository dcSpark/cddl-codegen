use super::*;

impl<'a> IntermediateTypes<'a> {
    /// Whether `pred` holds for any `RustType` position `visit_all_rust_types` reaches. Every
    /// position is visited; this is the shared fold behind the `uses_*` runtime-usage gates.
    fn any_rust_type(&self, pred: impl Fn(&RustType) -> bool) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= pred(rt));
        found
    }

    /// Whether `pred` holds for any registered Rust struct.
    fn any_struct(&self, pred: impl FnMut(&RustStruct) -> bool) -> bool {
        self.rust_structs.values().any(pred)
    }

    /// Whether `pred` holds for any record's dynamic (typed or rest) row. Rows store their inner
    /// types flat, so the gates that must see a row's container recover it here.
    fn any_dynamic_row(&self, pred: impl Fn(&RestRow) -> bool) -> bool {
        self.any_struct(|rs| {
            matches!(rs.variant(), RustStructType::Record(record)
                if record.dynamic_rows().any(&pred))
        })
    }

    /// Whether ANY generated type uses CDDL `any` (the `AnyCbor` runtime type), so `export`/import
    /// wiring pulls in the `any_cbor` runtime module + `AnyCbor` import only for crates that need it
    /// (keeping every non-`any` crate's output byte-identical — the usage-gating invariant). Folds
    /// `contains_any_cbor` over `visit_all_rust_types`, the same superset walk `uses_non_empty_map`
    /// uses (reaches type-alias base types, record fields, table domain AND range, wrapper inners,
    /// array elements, tagged inners, and enum variants).
    pub fn uses_any_cbor(&self) -> bool {
        self.any_rust_type(RustType::contains_any_cbor)
    }

    /// Whether ANY generated type uses the `[+ T]` NonEmptyVec shape, so `export`/import wiring can
    /// pull in the `non_empty` runtime module + `NonEmptyVec` import only for crates that need it
    /// (keeping every non-`+` crate's output byte-identical).
    ///
    /// Detection folds `contains_non_empty_array` over `visit_all_rust_types`, which reaches EVERY
    /// `RustType` position in the IR — type-alias base types, record fields, table domain AND range,
    /// wrapper inners, named-array element types, and enum variants (incl. inlined records) —
    /// recursing into Array/Optional/Map inners at each node. This is a strict superset of a
    /// per-variant hand-walk: it can't miss a nested occurrence such as an inline `[+ x]` buried in a
    /// named array's element type (`[* [+ uint]]`) or in a table's domain.
    ///
    /// The named-rule bounds special case is kept deliberately. A named `[+ …]` rule registers as a
    /// `RustStructType::Array` whose `>= 1` lower bound lives on the STRUCT's `bounds`, not on the
    /// `element_type` `RustType` the visitor walks — so the visitor alone would not see it. Every
    /// such rule ALSO registers a transparent alias (`pub type Foo = NonEmptyVec<…>`), which the
    /// visitor's alias-base walk does cover today, so this check is redundant in every shape observed;
    /// but that redundancy is unproven across all IR shapes, and dropping a cheap belt-and-suspenders
    /// guard on an unverified premise is how a latent regression ships — so it stays.
    pub fn uses_non_empty_vec(&self) -> bool {
        self.any_rust_type(RustType::contains_non_empty_array)
            || self.any_struct(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Array { bounds, .. } if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                )
            })
            // A one-or-more open-array tail stores its inner type flat in `RestRow`; its composite
            // `NonEmptyVec<T>` is recovered by `RestRow::container_type`, so the generic type walk
            // deliberately does not see it. Keep runtime provisioning tied to that real container.
            || self.any_dynamic_row(RestRow::is_non_empty_array_tail)
    }

    /// Whether any generated type uses a bounded homogeneous ARRAY occurrence. This mirrors the
    /// non-empty runtime gate and deliberately walks every IR position, including nested aliases.
    pub fn uses_bounded_vec(&self) -> bool {
        self.any_rust_type(RustType::contains_bounded_array)
            || self.any_struct(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Array { bounds: Some(bounds), .. }
                        if !bounds.is_loose() && !bounds.is_non_empty()
                            && bounds.exact_len().is_none()
                            && !rs.config().duplicates_reject()
                )
            })
            // An open-array rest tail stores its element flat in RestRow, so the generic type walk
            // does not see the reconstructed BoundedVec container. Provision the runtime from the
            // same row-local occurrence that `RestRow::container_type` uses for emitted members.
            || self.any_dynamic_row(|row| {
                row.is_array_tail()
                    && row.is_restricted()
                    && !row.is_non_empty_array_tail()
                    && !row.container_type().is_type_enforced_exact_homogeneous_array()
            })
    }

    /// Whether the generated Rust surface contains an ordinary/preserve exact homogeneous array.
    /// These arrays need the generic serde/schemars adapter on dependency pins that only implement
    /// trait derives through length 32; exact bytes and reject sets are intentionally excluded.
    pub fn uses_static_exact_array(&self) -> bool {
        self.any_rust_type(RustType::is_type_enforced_exact_homogeneous_array)
            || self.any_struct(|rs| {
                matches!(rs.variant(), RustStructType::Array { bounds: Some(bounds), .. }
                    if bounds.exact_len().is_some()
                        && !rs.config().duplicates_reject())
            })
            || self.any_dynamic_row(|row| {
                row.is_array_tail()
                    && row
                        .container_type()
                        .is_type_enforced_exact_homogeneous_array()
            })
    }

    /// Whether ANY generated type uses the `{+ k => v}` NonEmptyMap shape, so `export`/import wiring
    /// can pull in the `non_empty_map` runtime module + `NonEmptyMap` import only for crates that need
    /// it (keeping every non-`+`-table crate's output byte-identical).
    ///
    /// Detection folds `contains_non_empty_map` over `visit_all_rust_types`, which reaches EVERY
    /// `RustType` position in the IR — type-alias base types, record fields, table domain AND range,
    /// wrapper inners, named-array element types, and enum variants (incl. inlined records) —
    /// recursing into Array/Optional/Map inners at each node. This is a strict superset of a
    /// per-variant hand-walk: it can't miss a nested occurrence such as an inline `{+ k => v}` buried
    /// in a table's DOMAIN (`{ * {+ uint => uint} => text }`) or in a named array's element type.
    ///
    /// The named-rule bounds special case is kept deliberately, for the same reason as
    /// `uses_non_empty_vec`: a named `{+ …}` table rule registers as a `RustStructType::Table` whose
    /// `>= 1` lower bound lives on the STRUCT's `bounds`, not on the `domain`/`range` `RustType`s the
    /// visitor walks. The transparent alias every such rule also registers covers it today, but that
    /// redundancy is unproven across all IR shapes, so the cheap guard stays.
    pub fn uses_non_empty_map(&self) -> bool {
        self.any_rust_type(RustType::contains_non_empty_map)
            || self.any_struct(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Table { bounds, .. } if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                )
            })
            // An open table's typed row stores its key/value flat in `RestRow`; its composite
            // `NonEmptyMap<K, V>` is recovered by `RestRow::container_type`, so the generic walk
            // deliberately cannot see it. Preserve rows are provisioned by `uses_pair_map` instead
            // (they use `NonEmptyPairMap`, never this runtime/import).
            || self.any_dynamic_row(|row| {
                row.is_non_empty()
                    && !row.is_array_tail()
                    && row.duplicates() != Some(DuplicatesPolicy::Preserve)
            })
    }

    /// Whether any owned type needs the finite/exact BoundedMap runtime.
    pub fn uses_bounded_map(&self) -> bool {
        self.any_rust_type(RustType::contains_bounded_map)
            || self.any_struct(|rs| matches!(
                rs.variant(),
                RustStructType::Table { bounds: Some(bounds), .. }
                    if !bounds.is_loose() && !bounds.is_non_empty()
                        && !rs.config().duplicates_preserve()
            ))
            // Dynamic map rows store their K/V types flat, so the generic walker intentionally
            // cannot see the BoundedMap composite. Recover it through the row's single carrier
            // source, mirroring the non-empty runtime gate above.
            || self.any_dynamic_row(|row| {
                !row.is_array_tail()
                    && row.container_type().is_bounded_map()
                    && row.duplicates() != Some(DuplicatesPolicy::Preserve)
            })
    }

    /// Whether ANY generated type uses the `@duplicates reject` `OrderedSet`/`NonEmptyOrderedSet`
    /// shape, so `export`/import wiring pulls in the `ordered_set` runtime module + imports only for
    /// crates that need it (keeping every non-reject crate's output byte-identical). Detection folds
    /// `contains_ordered_set` over `visit_all_rust_types` (the same superset walk as
    /// `uses_non_empty_vec`), plus the deliberate belt-and-suspenders on the struct config: a
    /// reject-mode `Array` rule's policy lives on the STRUCT config (and its registered alias), so the
    /// alias-base walk covers it today, but the cheap guard stays for the same unproven-across-shapes
    /// reason as the non-empty twins.
    pub fn uses_ordered_set(&self) -> bool {
        self.any_rust_type(RustType::contains_ordered_set)
            || self.any_struct(|rs| {
                matches!(rs.variant(), RustStructType::Array { .. })
                    && rs.config().duplicates_reject()
            })
    }

    /// Whether ANY generated type uses the `@duplicates preserve` `PairMap`/`NonEmptyPairMap` shape,
    /// so `export`/import wiring pulls in the `pair_map` runtime module + imports only for crates that
    /// need it. The pair-map analog of `uses_ordered_set`: folds `contains_pair_map` over
    /// `visit_all_rust_types` plus the belt-and-suspenders on the struct config (a preserve-mode
    /// `Table` rule's policy lives on the STRUCT config and its registered alias).
    pub fn uses_pair_map(&self) -> bool {
        self.any_rust_type(|rt| rt.contains_pair_map() || rt.contains_bounded_pair_map())
            || self.any_struct(|rs| {
                matches!(rs.variant(), RustStructType::Table { .. })
                    && rs.config().duplicates_preserve()
            })
            // An open struct-map rest row with `@duplicates preserve` lowers to the `PairMap` twin
            // (its `Map` type carries the policy only at emit time — `rest.domain`/`range` visited
            // above are the K/V, not the Map — so check the rest row's policy directly here).
            || self.any_dynamic_row(|row| {
                row.duplicates() == Some(DuplicatesPolicy::Preserve)
            })
    }

    /// Whether a generated type needs the bounded preserve-table carrier. Kept distinct from
    /// `uses_pair_map` so loose preserve tables do not receive an unused `BoundedPairMap` import.
    pub fn uses_bounded_pair_map(&self) -> bool {
        self.any_rust_type(RustType::contains_bounded_pair_map)
            || self.any_dynamic_row(|row| {
                !row.is_array_tail() && row.container_type().is_bounded_pair_map()
            })
    }

    /// Whether ANY generated record carries a CAPTURING open struct-map rest row (`* k => v` after
    /// fixed keys, default flavor). Gates the standalone `open_struct_rest_json` runtime module (the
    /// flatten helpers `serialize_flattened_rest` / `read_flattened_rest_pairs`) under
    /// `--json-serde-derives` — the helpers are `any`-free so they cannot live in `any_cbor.rs` (a
    /// fully-typed `* uint => text` rest row does not pull in the `AnyCbor` runtime). An `@ignore`
    /// (tolerate-and-drop) row emits no captured field, so its JSON is a closed struct's — it needs
    /// none of the flatten machinery and does not count here.
    pub fn uses_open_struct_rest(&self) -> bool {
        self.any_struct(
            |rs| matches!(rs.variant(), RustStructType::Record(record) if record.captured_dynamic_rows().next().is_some()),
        )
    }

    /// Whether ANY generated record is an OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`). Gates
    /// the open-table fragments of the `open_struct_rest_json` runtime module — the hand-written
    /// serde pair's helpers and the two-range schema — so a crate with only ordinary rest rows keeps
    /// its output byte-identical. The module itself is already present for such a crate
    /// (`uses_open_struct_rest` counts an open table's two rows), which is why these are fragments
    /// of it rather than a module of their own: a new static module would oblige every
    /// `--export-static-crate` consumer to hand-add a `pub mod` line for a shape they may not use.
    pub fn uses_open_table(&self) -> bool {
        self.any_struct(
            |rs| matches!(rs.variant(), RustStructType::Record(record) if record.is_open_table()),
        )
    }

    /// Whether ANY generated record carries a member that is BOTH optional and nullable
    /// (`? f: (T / null)`), whose rust member is therefore a nested `Option<Option<T>>`. Gates the
    /// standalone `double_option` runtime module (the `#[serde(with)]` adapter that keeps the JSON
    /// surface's absent / present-null / present-value distinction) under `--json-serde-derives`.
    /// This is intentionally a coarse spec-level module gate: an exact-array member uses the
    /// `static_array` presence-aware callback instead, but a record with such a member still emits
    /// the compatibility module alongside it.
    ///
    /// The struct-field position is the WHOLE reachable universe for a nested `Option`: every other
    /// spelling collapses one of the two `Option`s before it reaches a serde surface — a table value
    /// / array element carries no presence-`Option` of its own (container membership is the presence
    /// bit), a wrapper body and a type-choice arm hold the nullable directly, and a group-choice arm
    /// either becomes a record of its own (this position) or is inlined into an enum variant, where
    /// the variant tag IS the presence bit (`can_embed_fields`, ≤1 non-fixed field). A member with a
    /// `.default` is not `Option`-wrapped at all, so it is excluded here exactly as it is at the
    /// emission site.
    pub fn uses_double_option(&self) -> bool {
        self.any_struct(|rs| {
            matches!(rs.variant(), RustStructType::Record(record)
                if record.fields.iter().any(RustField::is_double_option))
        })
    }

    /// The first authored (non-anonymous) rule, in `RustIdent` order, whose struct satisfies `owns`.
    /// A SYNTHESIZED anonymous collection instance lowers to its structural wrapper and never owns
    /// the named slot.
    fn named_collection_owner(&self, owns: impl Fn(&RustStruct) -> bool) -> Option<&RustIdent> {
        self.rust_structs
            .iter()
            .find(|(ident, rs)| !self.is_anonymous_collection_instance(ident) && owns(rs))
            .map(|(ident, _)| ident)
    }

    /// The NAMED `{+ k => v}` table rule that owns the wasm surface for an inline `{+ k => v}` of the
    /// same domain/range, if any — the design doc's inline-dedup-to-named rule (the spec author's
    /// chosen name wins over a synthesized `NonEmptyMapKToV`). Domain/range equality is alias-resolved
    /// so spelling differences can't defeat the dedup. Deterministic (`rust_structs` is a `BTreeMap`).
    pub fn non_empty_map_named_owner(
        &self,
        key: &RustType,
        value: &RustType,
    ) -> Option<&RustIdent> {
        let key_resolved = key.clone().resolve_aliases();
        let value_resolved = value.clone().resolve_aliases();
        self.named_collection_owner(|rs| {
            matches!(rs.variant(),
                // A SYNTHESIZED anonymous instance carries no author name worth surfacing — it lowers
                // to the structural `NonEmpty<MapKToV>` wrapper (see the anonymous-collapse
                // convergence), so it must NOT win the owner slot the way an authored `{+ …}` rule does.
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                    // A `@duplicates preserve` named `{+ …}` rule's wasm class wraps `NonEmptyPairMap`,
                    // but an inline `{+ …}` occurrence carries no directive (inline occurrences are
                    // directive-less), so its rust member is the loose `NonEmptyMap`. Capturing the
                    // inline surface onto the preserve rule would name it after a wrapper of the wrong
                    // core type — a loud-but-broken wasm crate (`From<NonEmptyMap>` for the preserve
                    // wrapper does not exist). The map-side of the reject-set guard in
                    // `non_empty_named_owner`: only a non-preserve named rule may own the loose inline.
                    && !rs.config().duplicates_preserve()
                    && domain.resolves_equal(&key_resolved)
                    && range.resolves_equal(&value_resolved))
        })
    }

    /// The NAMED `[+ elem]` rule that owns the wasm surface for an inline `[+ elem]` of the same
    /// element, if any — the design doc's inline-dedup-to-named rule: the spec author's chosen name
    /// wins over a synthesized `NonEmpty<Elem>List`. Element equality is alias-resolved so spelling
    /// differences can't defeat the dedup. Deterministic: `rust_structs` is a `BTreeMap`, so the
    /// lexicographically-first matching rule ident wins when several same-shape rules exist.
    pub fn non_empty_named_owner(&self, element: &RustType) -> Option<&RustIdent> {
        let resolved = element.clone().resolve_aliases();
        self.named_collection_owner(|rs| {
            matches!(rs.variant(),
                // A SYNTHESIZED anonymous instance is excluded (see the map twin above): it lowers to
                // the structural `NonEmpty<Elem>List`, so a `nonempty_set<key_hash>` instance never
                // shadows the inline `[+ key_hash]`'s synthesized wrapper name with its own ident.
                RustStructType::Array {
                    element_type,
                    bounds,
                } if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                    // A `@duplicates reject` named rule's wasm class wraps `NonEmptyOrderedSet`, but
                    // an inline `[+ elem]` occurrence carries no directive (inline occurrences are
                    // always preserve-policy) so its rust member is `NonEmptyVec`. Capturing the
                    // preserve inline surface onto the reject rule would name it after a wrapper of
                    // the wrong core type — a loud-but-broken wasm crate (`From<NonEmptyVec>` for the
                    // reject wrapper does not exist). Only a preserve-policy named rule may own it.
                    && !rs.config().duplicates_reject()
                    && element_type.resolves_equal(&resolved))
        })
    }

    /// The authored bounded-array rule owning the wasm class for an inline occurrence with the
    /// identical element and inclusive window.  The bounds are part of the identity: unlike a
    /// loose list, `[2*5 T]` and `[2*6 T]` cannot share a class without losing the checked door.
    pub fn bounded_array_named_owner(
        &self,
        element: &RustType,
        bounds: IntWindow,
    ) -> Option<&RustIdent> {
        let normalized = Self::normalized_bounded_window(bounds)?;
        let resolved = element.clone().resolve_aliases();
        self.named_collection_owner(|rs| {
            matches!(rs.variant(),
                RustStructType::Array {
                    element_type,
                    bounds: Some(candidate),
                } if Self::normalized_bounded_window(candidate.raw()) == Some(normalized)
                    // A reject-mode bounded rule owns an `OrderedSet` class, not the ordinary
                    // BoundedVec wasm class an inline preserve-policy occurrence needs. It cannot
                    // be a dedup owner without crossing incompatible core representations.
                    && !rs.config().duplicates_reject()
                    && element_type.resolves_equal(&resolved))
        })
    }

    /// The authored bounded-table rule owning the wasm class for an inline occurrence with the
    /// identical domain, range, and inclusive window. As with bounded arrays, the window is part
    /// of the identity: `{2*3 K => V}` cannot share a class with `{2*4 K => V}`.
    pub fn bounded_map_named_owner(
        &self,
        key: &RustType,
        value: &RustType,
        bounds: IntWindow,
        preserve: bool,
    ) -> Option<&RustIdent> {
        let normalized = Self::normalized_bounded_window(bounds)?;
        let key_resolved = key.clone().resolve_aliases();
        let value_resolved = value.clone().resolve_aliases();
        self.named_collection_owner(|rs| {
            matches!(rs.variant(),
                RustStructType::Table {
                    domain,
                    range,
                    bounds: Some(candidate),
                } if Self::normalized_bounded_window(candidate.raw()) == Some(normalized)
                    && rs.config().duplicates_preserve() == preserve
                    && domain.resolves_equal(&key_resolved)
                    && range.resolves_equal(&value_resolved))
        })
    }

    /// Canonicalize an array or table occurrence window before comparing ownership or rendering a
    /// request: absent endpoints are the real `0` / unbounded values, and the two loose shapes are
    /// not bounded owners (`*` stays loose, `+` stays NonEmptyVec / NonEmptyMap).  Keeping this here
    /// makes `[? T]`/`[0*1 T]` and `[*5 T]`/`[0*5 T]` one identity even though the parser preserves
    /// the source spelling.
    pub(super) fn normalized_bounded_window(bounds: IntWindow) -> Option<(u64, u64)> {
        occurrence_window_u64(bounds)
            .filter(|&window| window != (0, u64::MAX) && window != (1, u64::MAX))
    }

    /// Visit every `RustType` occurrence in the IR — record fields, table domain/range, wrapper
    /// inners, enum variants (incl. inlined records), named-array element types, and type-alias
    /// base types — recursing into Array/Optional/Map inners at the RustType level, so occurrence
    /// bounds (which live on `RustType`, not the conceptual type) stay visible to the visitor
    /// (the conceptual `visit_types` strips them at every step).
    pub fn visit_all_rust_types<F: FnMut(&RustType)>(&self, f: &mut F) {
        fn walk<F: FnMut(&RustType)>(rt: &RustType, f: &mut F) {
            f(rt);
            match &rt.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, f)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, f);
                    walk(v, f);
                }
                _ => {}
            }
        }
        for alias in self.type_aliases.values() {
            walk(&alias.base_type, f);
        }
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Record(record) => {
                    record.fields.iter().for_each(|fl| walk(&fl.rust_type, f));
                    // An open rest (map `* k => v` row or array `* t` tail) carries RustType(s)
                    // (loose CBOR) that are NOT a `RustField`, so walk them explicitly — else usage
                    // detectors (`uses_any_cbor`, the collection-twin detectors) miss an
                    // `any`/collection type that appears only in the rest inner type(s).
                    for rest in record.dynamic_rows() {
                        match &rest.kind {
                            RestKind::MapEntries { domain, range, .. } => {
                                walk(domain, f);
                                walk(range, f);
                            }
                            RestKind::ArrayTail { element, .. } => walk(element, f),
                        }
                    }
                }
                RustStructType::Table { domain, range, .. } => {
                    walk(domain, f);
                    walk(range, f);
                }
                RustStructType::Wrapper { wrapped, .. } => walk(wrapped, f),
                RustStructType::Array { element_type, .. } => walk(element_type, f),
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    variants.iter().for_each(|v| match &v.data {
                        EnumVariantData::RustType(t) => walk(t, f),
                        EnumVariantData::Inlined(rec) => {
                            rec.fields.iter().for_each(|fl| walk(&fl.rust_type, f))
                        }
                    })
                }
                RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    }
}
