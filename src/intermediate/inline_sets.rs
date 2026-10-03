use super::*;

impl<'a> IntermediateTypes<'a> {
    /// Phase 2.4 consolidation seam: rewrite every INLINE `#6.258([* T])` occurrence in the finalized
    /// construction products into a reference to a shape-derived nominal set wrapper (`SetU64`,
    /// `SetNonEmptyText`, `SetSetU64`, …), minting one wrapper per DEDUPED shape. This is the SINGLE
    /// place inline set nominalization + effective-policy (258 ⇒ reject) resolution happens; the
    /// `Type2::TaggedData` arm builds only the plain tagged occurrence, so a discarded transient
    /// type-choice arm can never mint a spurious nominal (the class the old `SUPPRESS_INLINE_TAG_DEFAULT`
    /// thread-local worked around, now structurally impossible).
    ///
    /// Walks both `rust_structs` (record fields, table domain/range, named-array elements, enum arms,
    /// wrapper inners) and `type_aliases` — the alias walk is the hook for the `T / null` corner (a
    /// `foo = #6.258([* uint]) / null` collapses to an `Optional(..)` ALIAS that bypasses
    /// `register_rust_struct`, so it would otherwise never be visited).
    pub(super) fn nominalize_inline_sets(
        &mut self,
        parent_visitor: &ParentVisitor,
        cli: &Cli,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut minted: BTreeMap<RustIdent, RustStruct> = BTreeMap::new();
        // `mem::take` to sidestep the borrow: the rewrite is a pure function of each product's own
        // types, accumulating minted wrappers on the side.
        let mut structs = std::mem::take(&mut self.rust_structs);
        for rs in structs.values_mut() {
            rewrite_inline_sets_in_struct(rs, &mut minted);
        }
        self.rust_structs = structs;
        let mut aliases = std::mem::take(&mut self.type_aliases);
        for info in aliases.values_mut() {
            rewrite_inline_sets_in_type(&mut info.base_type, &mut minted);
        }
        self.type_aliases = aliases;
        // Register each minted nominal once (`minted` is already deduped by canonical ident, and it is
        // a `BTreeMap`, so registration order is deterministic). `register_rust_struct` threads the
        // reject policy onto the wrapped array (selecting the `OrderedSet` twin); the minted wrapper
        // contains no further inline set, so there is no re-entrancy.
        for (ident, nominal) in minted {
            // A shape-derived ident that already names a registered struct OR a user type alias is a
            // genuine collision — a user rule (`set_u64 = [x: uint]` → a `SetU64` struct, or
            // `set_u64 = text` → a `SetU64` type alias) or a generic instantiation (`set<uint>` →
            // `SetU64`, a DIFFERENT nominal: two-arm optional-tag record vs this inline mandatory-tag
            // record). Overwriting would silently re-point the rewritten reference; refuse loudly with a
            // SET-nominal-specific message (the per-kind sibling of the duplicate-top-level-ident
            // backstop, whose generic "list/map wrapper families" text is misleading here).
            if self.rust_structs.contains_key(&ident)
                || self
                    .type_aliases
                    .contains_key(&AliasIdent::Rust(ident.clone()))
            {
                return Err(format!(
                    "name collision: the shape-derived nominal `{ident}` synthesized for an inline \
                     `#6.258([* …])` set occurrence collides with an already-defined rule or generic \
                     set instantiation of the same name — rename the colliding rule, or hoist the \
                     inline occurrence to a distinctly-named rule"
                )
                .into());
            }
            // The nominal defaults to `@duplicates reject` (IANA set semantics) — a decode-behavior
            // change (loose historical bytes with duplicate elements now fail `DuplicateKey`). Notice
            // it once per minted nominal, naming the type and the hoist-to-named-rule opt-out (inline
            // positions have no comment slot for `@duplicates`).
            crate::warn!(
                "Inline #6.258 set occurrence nominalized to `{ident}` (defaults to @duplicates reject, IANA set semantics) — hoist it to a named rule with `; @duplicates preserve` to opt out"
            );
            self.register_rust_struct(parent_visitor, nominal, cli);
        }
        Ok(())
    }
}

/// The shape-derived ident an inline `#6.258([* Elem])` set occurrence nominalizes into (Phase 2.4):
/// `Set<Elem-variant>`, with a `NonEmpty` infix for the `[+]` bound. The prefix `Set` is the registry
/// entry for tag 258; the element spelling reuses `for_variant()` — the SAME element-spelling scheme
/// the wasm structural collection names (`<Elem>List` / `<Elem>OrderedSet`) and the generic
/// instantiation names use — so it is deterministic. `RustIdent::new` camel-cases the underscore form
/// (`set_u_64` never arises — `for_variant` yields `U64`, so `set_U64` → `SetU64`; `set_non_empty_Text`
/// → `SetNonEmptyText`). Distinct shapes yield distinct names; a name that nonetheless collides with a
/// user rule / generic instantiation is caught in `nominalize_inline_sets`.
fn inline_set_nominal_ident(element: &ConceptualRustType, non_empty: bool) -> RustIdent {
    let variant = element.for_variant();
    let raw = if non_empty {
        format!("set_non_empty_{variant}")
    } else {
        format!("set_{variant}")
    };
    RustIdent::new(CDDLIdent::new(raw))
}

/// Recurse into every `RustType` a `ConceptualRustType` transitively holds — including through an
/// `Alias`'s resolved base, which is where a `T / null` collapse hides its inline set: a use-site
/// field typed as such an alias carries `Alias(MaybeSet, Optional(#6.258([* uint])))` INLINE (its own
/// snapshot of the base, separate from the registered `type_aliases` entry), so both copies must be
/// rewritten to stay consistent (else the field's serialize inlines the old array shape while the
/// field TYPE names the nominal — an E0308).
fn rewrite_inline_sets_in_conceptual(
    ct: &mut ConceptualRustType,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    match ct {
        ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
            rewrite_inline_sets_in_type(inner, minted);
        }
        ConceptualRustType::Map(k, v) => {
            rewrite_inline_sets_in_type(k, minted);
            rewrite_inline_sets_in_type(v, minted);
        }
        ConceptualRustType::Alias(_, inner) => rewrite_inline_sets_in_conceptual(inner, minted),
        ConceptualRustType::Fixed(_)
        | ConceptualRustType::Primitive(_)
        // `any` is opaque — no inline IR sets to rewrite.
        | ConceptualRustType::Any
        | ConceptualRustType::Rust(_) => {}
    }
}

/// Recursively rewrite every INLINE `#6.258([* T])` occurrence WITHIN `rt` into a reference to its
/// shape-derived nominal set wrapper, minting the wrapper (once per deduped ident) into `minted`. See
/// `IntermediateTypes::nominalize_inline_sets`. Recurses into container inners FIRST so a nested inline
/// set inside the element is nominalized before the outer occurrence captures the (already-rewritten)
/// element.
fn rewrite_inline_sets_in_type(rt: &mut RustType, minted: &mut BTreeMap<RustIdent, RustStruct>) {
    rewrite_inline_sets_in_conceptual(&mut rt.conceptual_type, minted);
    // An inline set occurrence is a homogeneous `Array` whose INNERMOST encoding is a mandatory
    // `Tagged(258)`. (A named set nominal's wrapped array carries the tag on the STRUCT, not the
    // `RustType`, so it never matches here; a non-258 inline tag has no registry set default.)
    if !matches!(rt.conceptual_type, ConceptualRustType::Array(_))
        || rt.encodings.first() != Some(&CBOREncodingOperation::Tagged(258))
    {
        return;
    }
    let ConceptualRustType::Array(element) = &rt.conceptual_type else {
        unreachable!()
    };
    let non_empty = rt.is_non_empty_array();
    let ident = inline_set_nominal_ident(&element.conceptual_type, non_empty);
    // The wrapper owns a CLEAN array (the 258 becomes the struct's mandatory `Option<Sz>` tag record);
    // preserve the occurrence-count bound so `[+]` selects the `NonEmptyOrderedSet` twin.
    let mut array_type: RustType = ConceptualRustType::Array(element.clone()).into();
    if let Some(bounds) = rt.config.occurrence_bounds() {
        array_type = array_type.with_occurrence_bounds(bounds);
    }
    // 258 ⇒ `@duplicates reject` (IANA set semantics); `register_rust_struct` reads this off the
    // wrapper config to swap the inner to the `OrderedSet`/`NonEmptyOrderedSet` twin.
    let effective_metadata = RuleMetadata {
        duplicates: Some(DuplicatesPolicy::Reject),
        ..Default::default()
    };
    let nominal = RustStruct::new_wrapper(
        ident.clone(),
        Some(258),
        Some(&effective_metadata),
        array_type,
        None,
    )
    .as_set_nominal();
    minted.entry(ident.clone()).or_insert(nominal);
    // Reference the nominal; any OUTER encodings that sat ON TOP of the 258 (an `#6.24(#6.258(...))`
    // double tag) stay on the reference — the nominal owns only the innermost 258.
    let outer_encodings = rt.encodings[1..].to_vec();
    *rt = RustType {
        conceptual_type: ConceptualRustType::Rust(ident),
        encodings: outer_encodings,
        config: RustTypeSerializeConfig::default(),
        generic_param_binding: None,
    };
}

/// Rewrite inline set occurrences in every `RustType` a registered `RustStruct` holds. See
/// `rewrite_inline_sets_in_type`.
fn rewrite_inline_sets_in_struct(
    rs: &mut RustStruct,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    match &mut rs.variant {
        RustStructType::Record(record) => rewrite_inline_sets_in_record(record, minted),
        RustStructType::Table { domain, range, .. } => {
            rewrite_inline_sets_in_type(domain, minted);
            rewrite_inline_sets_in_type(range, minted);
        }
        RustStructType::Array { element_type, .. } => {
            rewrite_inline_sets_in_type(element_type, minted);
        }
        RustStructType::TypeChoice { variants }
        | RustStructType::GroupChoice { variants, .. }
        | RustStructType::CStyleEnum { variants } => {
            for variant in variants.iter_mut() {
                match &mut variant.data {
                    EnumVariantData::RustType(rt) => rewrite_inline_sets_in_type(rt, minted),
                    EnumVariantData::Inlined(record) => {
                        rewrite_inline_sets_in_record(record, minted)
                    }
                }
            }
        }
        RustStructType::Wrapper { wrapped, .. } => rewrite_inline_sets_in_type(wrapped, minted),
        RustStructType::Extern | RustStructType::RawBytesType => {}
    }
}

fn rewrite_inline_sets_in_record(
    record: &mut RustRecord,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    for field in record.fields.iter_mut() {
        rewrite_inline_sets_in_type(&mut field.rust_type, minted);
    }
}
