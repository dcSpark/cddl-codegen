use super::*;

impl<'a> IntermediateTypes<'a> {
    /// Whether `name` is claimed by an exported source rule this crate owns — the namespace a
    /// synthesized wasm wrapper must not silently shadow. Read source-rule ownership from `scopes`,
    /// not the finalized struct/alias registries: those also contain generator-synthesized
    /// structural wrappers, and two compatible uses of one structural class must unify rather than
    /// mistake the first synthesis for a user claim when the second registers.
    pub(super) fn wasm_ident_claimed_by_user_rule(&self, name: &str) -> bool {
        let ident = RustIdent::from_formatted(name);
        self.is_toplevel_rule(&ident) && self.scope(&ident).export()
    }

    /// Whether rule `name` provides a COMPATIBLE loose list wrapper for `element_resolved`: only a
    /// genuinely unbounded default/preserve Array of that same element is the `Vec` builder a
    /// restricted wrapper may borrow. Bounded `BoundedVec` and reject `OrderedSet` rules share none
    /// of that representation, even if their bounds are not `[+]`.
    fn provides_compatible_loose_list(&self, name: &str, element_resolved: &RustType) -> bool {
        let ident = RustIdent::from_formatted(name);
        self.rust_structs.get(&ident).is_some_and(|rs| {
            matches!(
                rs.variant(),
                RustStructType::Array {
                    element_type,
                    bounds,
                } if bounds.is_none_or(crate::intermediate::OccurrenceWindow::is_loose)
                    && element_type.resolves_equal(element_resolved)
            ) && !rs.config().duplicates_reject()
        })
    }

    /// Whether rule `name` is the SELF-NAMED `[+ elem]` rule of the element whose loose list class is
    /// spelled `name` (`nev_q_list = [+ nev_q]` -> `NevQList`). Such a rule legitimately owns the
    /// ident for its RESTRICTED wrapper, and the self-named leg of
    /// `non_empty_wrapper_name_collisions` reports the resulting conflict with the `[+ …]` rule as
    /// the named claimant — so the direct-claim leg must not report the same conflict a second time
    /// in a voice that would tell the author to rename a rule the other message already names.
    fn claims_ident_as_self_named_non_empty_list(&self, name: &str) -> bool {
        let ident = RustIdent::from_formatted(name);
        matches!(
            self.rust_structs.get(&ident).map(|rs| rs.variant()),
            Some(RustStructType::Array {
                element_type,
                bounds,
            }) if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                && element_type.name_as_wasm_array(self) == name
        )
    }

    /// The map-side twin of `claims_ident_as_self_named_non_empty_list`: whether rule `name` is the
    /// SELF-NAMED `{+ k => v}` table whose own loose-builder name is `name`. Owned by the self-named
    /// leg of `non_empty_map_wrapper_name_collisions`, so the direct-claim leg skips it.
    fn claims_ident_as_self_named_non_empty_table(&self, name: &str) -> bool {
        let ident = RustIdent::from_formatted(name);
        self.rust_structs.get(&ident).is_some_and(|rs| {
            let preserve =
                rs.config().duplicates_preserve();
            matches!(
                rs.variant(),
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                    && RustType::wasm_structural_map_name_for(domain, range, preserve, self).to_string()
                        == name
            )
        })
    }

    /// Whether rule `name` provides a COMPATIBLE loose table wrapper for `(key, value)` (a plain
    /// `{* k => v}` Table rust struct of the same domain/range — its wasm class IS the loose `MapKToV`
    /// builder the `{+ …}` restricted wrapper's `try_from` source needs, so it is shared, not a
    /// collision). Non-empty tables are excluded (their class is the restricted wrapper, not the
    /// loose builder).
    fn provides_compatible_loose_table(
        &self,
        name: &str,
        key_resolved: &RustType,
        value_resolved: &RustType,
        // the flavor the caller needs: a `@duplicates preserve` builder is a `PairMap`-backed class,
        // structurally incompatible with the keyed default, so the rule must match on BOTH
        preserve: bool,
    ) -> bool {
        let ident = RustIdent::from_formatted(name);
        let rule_preserve = self
            .rust_structs
            .get(&ident)
            .is_some_and(|rs| rs.config().duplicates_preserve());
        rule_preserve == preserve
            && matches!(
                self.rust_structs.get(&ident).map(|rs| rs.variant()),
                Some(RustStructType::Table {
                    domain,
                    range,
                    bounds,
                }) if !bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                    && domain.resolves_equal(key_resolved)
                    && range.resolves_equal(value_resolved)
            )
    }

    /// Detect wasm-class name conflicts the finite/zero-minimum `BoundedVec` emission would
    /// otherwise turn into a non-compiling wasm crate. This is deliberately a per-kind sibling of
    /// the NonEmpty detector: its name embeds MIN/MAX and its pinned diagnostic tells an author
    /// which restricted representation they shadowed. In addition to the restricted class's own
    /// name, a non-exposable bounded wrapper needs the loose `<Elem>List` builder as its checked
    /// `try_from` source. A positive-minimum self-named rule cannot use `new()` and cannot borrow
    /// that same ident as a loose source, so it is rejected rather than exposing an unconstructible
    /// JS class. Zero-minimum self-named rules remain constructible by `new()` + `add`.
    pub(super) fn bounded_array_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        let check_loose_source = |wrapper_ident: &str,
                                  element: &RustType,
                                  min: u64,
                                  context: &str,
                                  msgs: &mut BTreeSet<String>| {
            // Unlike the NonEmpty/reject twins, a bounded OUTER can and must take a loose source
            // even when its element is another restricted collection: `generate_array_type` gives
            // that source wrapper its own checked element boundary.
            if element.vec_of_self_directly_wasm_exposable(self) {
                return;
            }
            let loose = element.name_as_wasm_array(self);
            if loose == wrapper_ident {
                if min != 0 {
                    msgs.insert(format!(
                        "name collision: positive-minimum bounded rule '{wrapper_ident}' owns the \
                         loose '{loose}' list-builder ident it needs as its `try_from` source — \
                         rename the rule so the loose builder class can exist"
                    ));
                }
            } else if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_list(&loose, &element.clone().resolve_aliases())
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' list \
                     builder needs as the `try_from` source of {context} — rename the rule (or \
                     make it `[* …]` of the same element, which IS that builder)"
                ));
            }
        };
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Array(elem) = &rt.conceptual_type else {
                return;
            };
            if !(rt.is_type_enforced_exact_homogeneous_array() || rt.is_bounded_array())
                || rt.is_bounded_reject_ordered_set()
            {
                return;
            }
            let (min, _) = rt
                .exact_homogeneous_array_u64_bounds()
                .or_else(|| rt.bounded_array_u64_bounds())
                .expect("bounded occurrence bounds were validated during parsing");
            if self
                .bounded_array_named_owner(elem, rt.config.occurrence_bounds().unwrap())
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded array occurrence — rename the rule to \
                     avoid shadowing the restricted BoundedVec wrapper"
                ));
            }
            check_loose_source(
                &restricted,
                elem,
                min,
                &format!("the inline bounded wrapper '{restricted}'"),
                &mut msgs,
            );
        });
        // A named bounded rule uses its own ident for the restricted class and is not visited as a
        // raw Array occurrence above. It has the same loose-source need, including the positive-min
        // self-named dead end, so audit it explicitly.
        for (ident, rs) in &self.rust_structs {
            let RustStructType::Array {
                element_type,
                bounds: Some(bounds),
            } = rs.variant()
            else {
                continue;
            };
            if rs.config().duplicates_reject() {
                continue;
            }
            let Some((min, _)) = Self::normalized_bounded_window(bounds.raw()) else {
                continue;
            };
            check_loose_source(
                ident.as_ref(),
                element_type,
                min,
                &format!("the named bounded rule '{ident}'"),
                &mut msgs,
            );
        }
        // A bounded final open-array tail stores only its element in RestRow, so it is absent from
        // the generic RustType walk above. Its wasm getter nevertheless mints exactly the same
        // structural BoundedVec wrapper as an inline `[2*3 T]`; audit its restricted name and loose
        // builder source here rather than letting a user rule shadow either class during emission.
        for rs in self.rust_structs.values() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            for rest in record.dynamic_rows().filter(|row| {
                row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail()
            }) {
                let container = rest.container_type();
                let ConceptualRustType::Array(element) = &container.conceptual_type else {
                    unreachable!("an array rest tail's container is an Array");
                };
                let (min, _) = container
                    .exact_homogeneous_array_u64_bounds()
                    .or_else(|| container.bounded_array_u64_bounds())
                    .expect("bounded array-tail occurrence bounds were validated during parsing");
                if self
                    .bounded_array_named_owner(
                        element,
                        container.config.occurrence_bounds().unwrap(),
                    )
                    .is_some()
                {
                    continue;
                }
                let restricted = container.bounded_wasm_wrapper_name(self);
                if self.wasm_ident_claimed_by_user_rule(&restricted) {
                    msgs.insert(format!(
                        "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                         wrapper generated for a bounded open-array rest tail — rename the rule to \
                         avoid shadowing the restricted BoundedVec wrapper"
                    ));
                }
                check_loose_source(
                    &restricted,
                    element,
                    min,
                    &format!("the bounded open-array wrapper '{restricted}'"),
                    &mut msgs,
                );
            }
        }
        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `[+ elem]` (NonEmptyVec) emission would otherwise turn
    /// into a non-compiling wasm crate — every leg rejects gracefully rather than silently shadow
    /// or emit malformed code. Scans EVERY RustType position (incl. named-array element types and
    /// alias base types) via `visit_all_rust_types`. Four conflict classes:
    ///
    /// 1. An inline `[+ elem]` with no named owner (see `non_empty_named_owner` — when an owner
    ///    exists the inline use dedups to the named rule and mints nothing) mints a synthesized
    ///    `NonEmpty<Elem>List` class: a user rule claiming that ident collides.
    /// 2. A restricted wrapper whose element is non-exposable needs the LOOSE `<Elem>List` builder
    ///    as its `try_from` source (synthesized mints and free-named `[+ elem]` rules alike): a
    ///    user rule claiming that ident with any shape OTHER than a same-element loose Array rule
    ///    (which IS the builder, shared) collides.
    /// 3. A self-named rule (`bar_list = [+ bar]` — the rule ident IS the element's loose-builder
    ///    name) legitimately claims the name for its RESTRICTED wrapper (it emits with no
    ///    `try_from`; construction is `new(first)` + `add`), but then no OTHER use may need the
    ///    loose `<Elem>List` builder: a plain non-exposable `[* elem]` mint, a map-key list
    ///    wrapper of the same element, or an open struct's rest row (a `* K => V` row's `keys()`
    ///    wrapper, a loose `* T` tail's own getter, or the `try_from` source of a non-empty `+ T`
    ///    tail) would reference a class of the wrong shape.
    /// 4. A DIRECT claim: any of those plain uses MINTS the loose `<Elem>List` on its own, with no
    ///    `[+ …]` shape anywhere, and a user rule of the same ident and an incompatible shape
    ///    shadows it. Classes 2 and 3 both arrive through a `[+ …]` wrapper, so this is the leg a
    ///    spec containing no `[+ …]` at all reaches. The table-keys member must stay here even
    ///    though synthesis now leaves an authored claimant in place: this detector owns the
    ///    family-specific explanation of why that claimant cannot serve the keys() wrapper.
    pub(super) fn non_empty_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty shape + every loose-builder need from PLAIN array shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose <Elem>List classes needed by plain (non-`+`) uses: name -> (a use description, the
        // ELEMENT the class wraps). The element rides along so the direct-claim leg below can ask
        // `provides_compatible_loose_list` whether a rule of that ident IS the builder.
        let mut plain_loose_needs: BTreeMap<String, (String, RustType)> = BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_array() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Array(elem) = &rt.conceptual_type
                && !rt.directly_wasm_exposable(self)
            {
                plain_loose_needs.insert(
                    elem.name_as_wasm_array(self),
                    (
                        "a plain (`*`-occurrence) array use".to_owned(),
                        (**elem).clone(),
                    ),
                );
            }
            if let ConceptualRustType::Map(k, _v) = &rt.conceptual_type {
                // table wrappers mint a keys() list wrapper over the KEY type
                if !ConceptualRustType::Array(Box::new((**k).clone()))
                    .directly_wasm_exposable_ct(self)
                {
                    let loose_key = k.loosened_for_wasm_table_boundary_key();
                    plain_loose_needs.insert(
                        loose_key.name_as_wasm_array(self),
                        ("a map keys() wrapper".to_owned(), loose_key),
                    );
                }
            }
        });
        // named tables' keys() wrappers (Table structs aren't visited as Map RustTypes)
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Table { domain, .. } => {
                    if !ConceptualRustType::Array(Box::new(domain.clone()))
                        .directly_wasm_exposable_ct(self)
                    {
                        let loose_domain = domain.loosened_for_wasm_table_boundary_key();
                        plain_loose_needs.insert(
                            loose_domain.name_as_wasm_array(self),
                            ("a table keys() wrapper".to_owned(), loose_domain),
                        );
                    }
                }
                // An open struct's rest row names a list wrapper the same way a field of the row's
                // CONTAINER type would, and the IR stores the row's inner types flat, so neither the
                // `visit_all_rust_types` walk above nor the Table arm sees the claim: a `* K => V`
                // row's wasm class needs the loose `<K>List` for its `keys()`, and an array tail's
                // getter needs its loose builder or checked list class. Only a CAPTURED row mints anything — an
                // `@ignore` row has no field and no getter.
                //
                // BOTH dynamic rows, because an open table's TYPED row claims a `<K_t>List` too: its
                // map surface is FLATTENED onto the minted struct's class, so the `keys()` that
                // returns that list is the STRUCT's own method — a walk reading only the catch-all
                // would let a user rule of that ident shadow it silently.
                RustStructType::Record(record) => {
                    for rest in record.captured_dynamic_rows() {
                        if rest.is_array_tail() {
                            // A final `+ T` tail is an inline restricted Array RustType even though
                            // the generic type walk sees only its element. Its getter mints the same
                            // NonEmpty<Elem>List class as an inline `[+ T]`, so it must enter the
                            // existing restricted-wrapper collision leg before generation gets a
                            // chance to silently shadow a user rule of that ident. Bounded tails
                            // enter the sibling bounded-wrapper audit above.
                            if rest.is_non_empty_array_tail() {
                                inline_non_empty.push(rest.container_type());
                            }
                            if !rest.container_type().directly_wasm_exposable(self) {
                                plain_loose_needs.insert(
                                    rest.element().name_as_wasm_array(self),
                                    (
                                        if rest.is_non_empty_array_tail() {
                                            "a one-or-more open array `+ …` rest tail".to_owned()
                                        } else if rest.is_restricted() {
                                            "a bounded open array rest tail".to_owned()
                                        } else {
                                            "an open array `* …` rest tail".to_owned()
                                        },
                                        rest.element().clone(),
                                    ),
                                );
                            }
                        } else if !ConceptualRustType::Array(Box::new(rest.domain().clone()))
                            .directly_wasm_exposable_ct(self)
                        {
                            let loose_domain = rest.domain().loosened_for_wasm_table_boundary_key();
                            plain_loose_needs.insert(
                                loose_domain.name_as_wasm_array(self),
                                (
                                    if record.is_typed_row(rest) {
                                        "an open table's keys() wrapper".to_owned()
                                    } else {
                                        "an open struct-map rest row's keys() wrapper".to_owned()
                                    },
                                    loose_domain,
                                ),
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        // shared leg: the loose-builder need of a restricted wrapper (synthesized or free-named)
        let check_loose_need = |element: &RustType,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            if element.vec_of_self_directly_wasm_exposable(self) || element.is_non_empty_array() {
                return; // bare-Vec door: try_from takes it directly; nested: no loose source at all
            }
            let loose = element.name_as_wasm_array(self);
            if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_list(&loose, &element.clone().resolve_aliases())
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' list \
                     builder needs as the `try_from` source of {needed_by} — rename the rule (or \
                     make it `[* …]` of the same element, which IS that builder)"
                ));
            }
        };

        // (1) + (2) for inline `[+ elem]` shapes that actually mint a synthesized class
        for rt in &inline_non_empty {
            let ConceptualRustType::Array(elem) = &rt.conceptual_type else {
                unreachable!("is_non_empty_array implies an Array conceptual type");
            };
            if self.non_empty_named_owner(elem).is_some() {
                continue; // dedups to the named rule's class — nothing synthesized, no conflict
            }
            let restricted = rt.non_empty_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline `[+ …]` occurrence — rename the rule to \
                     avoid shadowing the restricted NonEmptyVec wrapper"
                ));
            }
            check_loose_need(
                elem,
                &format!("the inline `[+ …]` wrapper '{restricted}'"),
                &mut msgs,
            );
        }

        // (2) + (3) for named `[+ elem]` rules
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Array {
                element_type,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if !bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty) {
                continue;
            }
            if element_type.vec_of_self_directly_wasm_exposable(self)
                || element_type.is_non_empty_array()
            {
                continue;
            }
            let loose = element_type.name_as_wasm_array(self);
            if loose == ident.to_string() {
                // self-named rule: it owns the ident as its RESTRICTED class (no try_from); any
                // OTHER use needing the loose builder of this element now has no class to name
                if let Some((need, _elem)) = plain_loose_needs.get(&loose) {
                    msgs.insert(format!(
                        "name collision: rule '{ident}' (`[+ …]`) claims the ident that {need} of \
                         the same element needs for its loose '{loose}' list wrapper — rename the \
                         rule so the loose builder class can exist"
                    ));
                }
            } else {
                check_loose_need(
                    element_type,
                    &format!("the named `[+ …]` rule '{ident}'"),
                    &mut msgs,
                );
            }
        }

        // (4) DIRECT claims. Every leg above reaches `plain_loose_needs` through a `[+ …]` shape —
        // as a try_from source or as a self-named rule — so a spec with no `[+ …]` anywhere never
        // consults it, and the plain mints it collects go unchecked. A named table's keys-list
        // participates here even when its structural name is already author-claimed: the synthesis
        // leaves that owner intact and this leg gives the table-specific remedy. The other plain
        // mints (rest rows, rest tails, inline `[* …]` uses) arrive through the same collected needs.
        // One leg covers all of them in this family's voice, exactly as the map side's rest-row leg
        // does for `MapKToV`.
        //
        // A rule that IS `[* elem]` of the same element is NOT a collision: it is that very builder,
        // shared by the table keys() accessor. A SELF-NAMED `[+ elem]` rule is skipped
        // too: leg (3) above owns it, with a message that names the `[+ …]` rule as the claimant.
        for (loose, (need, element)) in &plain_loose_needs {
            if self.wasm_ident_claimed_by_user_rule(loose)
                && !self.provides_compatible_loose_list(loose, &element.clone().resolve_aliases())
                && !self.claims_ident_as_self_named_non_empty_list(loose)
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' collides with the '{loose}' wasm wrapper \
                     generated for {need} of the same element — rename the rule to avoid shadowing \
                     the loose list wrapper (or make it a `[* …]` of the same element, which IS \
                     that wrapper)"
                ));
            }
        }

        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `{+ k => v}` (NonEmptyMap) emission would otherwise turn
    /// into a non-compiling wasm crate — the map-side twin of `non_empty_wrapper_name_collisions`.
    /// Ordinary loose-table paths use `MapKToV` (`wasm_structural_map_name_for`); a bounded table
    /// uses its separately named loose direct-key source (`wasm_loose_table_builder_name_for`). A
    /// map is never directly exposable, so (unlike arrays) each such builder is a `try_from` source.
    /// Six classes:
    ///
    /// 1. An inline `{+ k => v}` with no named owner (see `non_empty_map_named_owner`) mints a
    ///    synthesized `NonEmptyMapKToV` class: a user rule claiming that ident collides.
    /// 2. A restricted wrapper (inline-synth or named non-self-named) needs the loose `MapKToV`
    ///    builder as its `try_from` source: a user rule claiming that ident with any shape OTHER than
    ///    a same-shape plain `{* k => v}` table rule (which IS the builder, shared) collides.
    /// 3. A self-named rule (`map_k_to_v = {+ k => v}` — the rule ident IS the loose-builder name)
    ///    legitimately claims the name for its RESTRICTED wrapper (it emits with no `try_from`;
    ///    construction is `new(first_key, first_value)` + `insert`), but then no OTHER use may need
    ///    the loose `MapKToV` builder: a plain `{* k => v}` use, an anonymous same-shape map, or an
    ///    open struct-map rest row of the same key/value would reference a class of the wrong shape.
    /// 4. A DEFAULT-flavored open struct-map REST ROW mints the loose `MapKToV` its wasm getter
    ///    returns: a user rule claiming that ident with any shape other than the shared plain
    ///    `{* k => v}` table collides. This is the default-flavor twin of the Record leg in
    ///    `preserve_pair_map_loose_wrapper_name_collisions`.
    /// 5. A DIRECT claim on the loose `MapKToV` a plain `{* k => v}` USE or TABLE RULE mints — the
    ///    symmetric sibling of the list side's class 4. Rest rows are class 4 and preserve-flavored
    ///    shapes are `preserve_pair_map_loose_wrapper_name_collisions`, so this leg's source set is
    ///    restricted to keep one collision reported once, in one kind's voice.
    /// 6. A bounded OPEN-TABLE TYPED row stays flattened on its owner but its fallible constructor
    ///    needs one loose builder. It mints that builder only; no restricted whole-row wrapper
    ///    exists for a user rule to shadow.
    pub(super) fn non_empty_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty map shape + every loose-builder need from PLAIN map shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose MapKToV classes needed by plain (non-`+`) uses: name -> a use description
        let mut plain_loose_needs: BTreeMap<String, String> = BTreeMap::new();
        // The subset of those needs the DIRECT-claim leg owns: name -> (use description, key,
        // value). DEFAULT-flavored plain map uses and plain table rules only — a rest row is leg (4)
        // and a `@duplicates preserve` shape is
        // `preserve_pair_map_loose_wrapper_name_collisions`, so restricting the set here is what
        // keeps one collision to one message in one kind's voice.
        let mut direct_claim_needs: BTreeMap<String, (String, RustType, RustType)> =
            BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_map() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Map(k, v) = &rt.conceptual_type {
                let preserve = rt.is_preserve_pair_map();
                let name = rt.wasm_structural_map_name(self).to_string();
                plain_loose_needs
                    .insert(name.clone(), "a plain (`*`-occurrence) map use".to_owned());
                if !preserve {
                    direct_claim_needs.insert(
                        name,
                        (
                            "a plain (`*`-occurrence) map use".to_owned(),
                            (**k).clone(),
                            (**v).clone(),
                        ),
                    );
                }
            }
        });
        // Named table structs are not visited as Map RustTypes. An unbounded table mints its native
        // structural map class, while a bounded table mints the explicitly loose source its
        // `try_from` door names. Non-empty tables keep their existing restricted-wrapper path.
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    if !bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty) {
                        let preserve = rs.config().duplicates_preserve();
                        let bounded_source = bounds.is_some_and(|candidate| {
                            type_enforced_bounded_window(candidate.raw(), false).is_some()
                        });
                        let (name, builder_key, need) = if bounded_source {
                            (
                                RustType::wasm_loose_table_builder_name_for(
                                    domain, range, preserve, self,
                                )
                                .to_string(),
                                domain.loosened_for_wasm_table_boundary_key(),
                                "a bounded table rule's loose `try_from` source",
                            )
                        } else {
                            (
                                RustType::wasm_structural_map_name_for(
                                    domain, range, preserve, self,
                                )
                                .to_string(),
                                domain.clone(),
                                "a plain (`*`-occurrence) table rule",
                            )
                        };
                        plain_loose_needs.insert(name.clone(), need.to_owned());
                        if !preserve {
                            direct_claim_needs
                                .insert(name, (need.to_owned(), builder_key, range.clone()));
                        }
                    }
                }
                // An open struct-map rest row mints the loose builder its wasm getter returns, in
                // the flavor the row carries — invisible to the walk above because the IR stores the
                // row's key/value flat. Read the name off `RestRow::container_type`, the one
                // container spelling the emitter and the scope walk also use, so a row's claim
                // cannot drift from the class it actually mints.
                //
                // `captured_rest()`, deliberately, and NOT `captured_dynamic_rows()`: for an open
                // table that IS the catch-all row, the only one of its two rows that mints a map
                // class. The TYPED row's map surface is FLATTENED onto the minted struct's own wasm
                // class, so no `MapKToV`/`PairMapKToV` is minted for it and there is nothing for a
                // user rule to shadow — the collision is unrepresentable rather than rejected, the
                // same move that retired this family's one wrapper-vs-wrapper detector.
                RustStructType::Record(record) => {
                    let Some(rest) = record.captured_rest().filter(|r| !r.is_array_tail()) else {
                        continue;
                    };
                    let container = rest.container_type();
                    let ConceptualRustType::Map(_, _) = &container.conceptual_type else {
                        unreachable!("a map rest row's container is a Map");
                    };
                    // A bounded row's checked wrapper takes its *loose direct-key builder*, not
                    // the raw structural spelling.  The rest row stores K/V flat, so recover this
                    // source here exactly as `generate_bounded_map_type` does.  In particular a
                    // bounded collection KEY must not accidentally name its restricted key class
                    // as the loose source.
                    let preserve = container.is_preserve_pair_map();
                    let structural = if container.is_bounded_map() {
                        RustType::wasm_loose_table_builder_name_for(
                            rest.domain(),
                            rest.range(),
                            preserve,
                            self,
                        )
                        .to_string()
                    } else {
                        container.wasm_structural_map_name(self).to_string()
                    };
                    plain_loose_needs.insert(
                        structural,
                        if container.is_bounded_map() {
                            "a bounded open struct-map rest row's loose `try_from` source"
                                .to_owned()
                        } else {
                            "an open struct-map rest row".to_owned()
                        },
                    );
                    // The `+` map-row carrier has its own RESTRICTED structural class as well as
                    // the loose source above.  A captured open-struct row or open-table catch-all
                    // crosses wasm through that class; the typed row is intentionally absent here
                    // because its `+` surface is flattened on the record class and mints no map
                    // wrapper.  Reuse the inline restricted leg below so default and preserve
                    // rows receive the same flavor-correct collision diagnostic.
                    if rest.is_non_empty() {
                        inline_non_empty.push(container);
                    }
                }
                _ => {}
            }
        }

        // A bounded TYPED row has a deliberately narrow exception to the flattened-table rule:
        // wasm `new(entries_builder)` accepts one LOOSE structural builder and immediately enters
        // the native BoundedMap/Pairs `TryFrom` door. It mints neither a `Map…Min…` class nor a
        // whole-row getter, so only the builder's structural ident belongs in this detector. The
        // complete checked carrier is instead represented on the native, JSON, and component faces.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(typed) = record
                .typed_row()
                .filter(|row| row.container_type().bounded_map_u64_bounds().is_some())
            else {
                continue;
            };
            let builder = typed.staging_container_type();
            let ConceptualRustType::Map(key, value) = &builder.conceptual_type else {
                unreachable!("an open table typed row's staging carrier is a Map");
            };
            let structural = builder.wasm_structural_map_name(self).to_string();
            let need = format!("the bounded typed row of open table '{ident}'");
            plain_loose_needs.insert(structural.clone(), need.clone());
            if !builder.is_preserve_pair_map() {
                direct_claim_needs.insert(structural, (need, (**key).clone(), (**value).clone()));
            }
        }

        // shared leg: the loose-builder need of a restricted map wrapper (synthesized or named)
        let check_loose_need = |key: &RustType,
                                value: &RustType,
                                preserve: bool,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            let loose =
                RustType::wasm_structural_map_name_for(key, value, preserve, self).to_string();
            if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_table(
                    &loose,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    preserve,
                )
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' table \
                     builder needs as the `try_from` source of {needed_by} — rename the rule (or \
                     make it `{{* …}}` of the same key/value, which IS that builder)"
                ));
            }
        };

        // (1) + (2) for inline `{+ k => v}` shapes that actually mint a synthesized class
        for rt in &inline_non_empty {
            let ConceptualRustType::Map(k, v) = &rt.conceptual_type else {
                unreachable!("is_non_empty_map implies a Map conceptual type");
            };
            if self.non_empty_map_named_owner(k, v).is_some() {
                continue; // dedups to the named rule's class — nothing synthesized, no conflict
            }
            let restricted = rt.non_empty_wasm_map_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline `{{+ …}}` map occurrence — rename the rule to \
                     avoid shadowing the restricted NonEmptyMap wrapper"
                ));
            }
            check_loose_need(
                k,
                v,
                rt.is_preserve_pair_map(),
                &format!("the inline `{{+ …}}` wrapper '{restricted}'"),
                &mut msgs,
            );
        }

        // (2) + (3) for named `{+ k => v}` table rules
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if !bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty) {
                continue;
            }
            let preserve = rs.config().duplicates_preserve();
            let loose =
                RustType::wasm_structural_map_name_for(domain, range, preserve, self).to_string();
            if loose == ident.to_string() {
                // self-named rule: it owns the ident as its RESTRICTED class (no try_from); any
                // OTHER use needing the loose builder of this shape now has no class to name
                if let Some(need) = plain_loose_needs.get(&loose) {
                    msgs.insert(format!(
                        "name collision: rule '{ident}' (`{{+ …}}`) claims the ident that {need} of \
                         the same key/value needs for its loose '{loose}' table wrapper — rename \
                         the rule so the loose builder class can exist"
                    ));
                }
            } else {
                check_loose_need(
                    domain,
                    range,
                    preserve,
                    &format!("the named `{{+ …}}` rule '{ident}'"),
                    &mut msgs,
                );
            }
        }

        // (4) a DEFAULT-flavored open struct-map rest row MINTS the loose `MapKToV` class its wasm
        // getter returns, so a user rule spelling that ident shadows it. The `@duplicates preserve`
        // twin of this leg lives in `preserve_pair_map_loose_wrapper_name_collisions` (the flavor is
        // part of the structural name, so the two rows can never contend for one class) — per-kind
        // siblings with deliberately distinct texts, so a failing spec points at the right flavor.
        // A rule that IS a plain `{* k => v}` table of the same key/value is not a collision: it
        // solely owns the shape and the row's getter returns it through its `pub type` alias.
        //
        // For an OPEN TABLE this leg covers the CATCH-ALL row and only it — see the note on the
        // `plain_loose_needs` Record arm above for why the typed row mints no class of its own. The
        // message names the open table's catch-all rather than a struct-map rest row so the remedy
        // reads against the shape the author actually wrote.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record.captured_rest().filter(|r| {
                !r.is_array_tail() && r.duplicates() != Some(DuplicatesPolicy::Preserve)
            }) else {
                continue;
            };
            let container = rest.container_type();
            let bounded = container.is_bounded_map();
            let (structural, key, row_kind) = if bounded {
                (
                    RustType::wasm_loose_table_builder_name_for(
                        rest.domain(),
                        rest.range(),
                        false,
                        self,
                    )
                    .to_string(),
                    rest.domain().loosened_for_wasm_table_boundary_key(),
                    "bounded",
                )
            } else {
                (
                    RustType::wasm_structural_map_name_for(
                        rest.domain(),
                        rest.range(),
                        false,
                        self,
                    )
                    .to_string(),
                    rest.domain().clone(),
                    "loose",
                )
            };
            if self.wasm_ident_claimed_by_user_rule(&structural)
                && !self.provides_compatible_loose_table(
                    &structural,
                    &key.resolve_aliases(),
                    &rest.range().clone().resolve_aliases(),
                    false,
                )
            {
                let row = if record.is_open_table() {
                    format!("the open table catch-all row of '{ident}'")
                } else {
                    format!("the open struct-map rest row of '{ident}'")
                };
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for {row} — rename the rule to avoid shadowing the {row_kind} \
                     map wrapper (or make it a `{{* …}}` table of the same key/value, which IS that \
                     wrapper)"
                ));
            }
        }

        // (5) DIRECT claims by a plain `{* k => v}` use or table rule — the symmetric sibling of the
        // list side's direct-claim leg. Without it these reach only the generic duplicate-ident
        // backstop, which is loud but reports the ident rather than the claim; the shape here is the
        // one leg (4) already uses for a rest row, with the plain use named instead of the row.
        for (loose, (need, key, value)) in &direct_claim_needs {
            if self.wasm_ident_claimed_by_user_rule(loose)
                && !self.provides_compatible_loose_table(
                    loose,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    false,
                )
                && !self.claims_ident_as_self_named_non_empty_table(loose)
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' collides with the '{loose}' wasm wrapper \
                     generated for {need} of the same key/value — rename the rule to avoid \
                     shadowing the loose map wrapper (or make it a `{{* …}}` table of the same \
                     key/value, which IS that wrapper)"
                ));
            }
        }

        msgs.into_iter().collect()
    }

    /// Detect an authored rule shadowing the synthesized wasm class for an inline bounded table.
    /// This is deliberately the bounded-table sibling of the bounded-array and NonEmptyMap
    /// detectors: a `MapKToVMinN/MaxN` wrapper owns a BoundedMap and its one checked `try_from`
    /// door, so silently resolving that name to an unrelated rule would expose the wrong class.
    ///
    /// A same-shape named bounded table is not a collision. It is the explicit owner of the inline
    /// occurrence's surface, including through nested/generic occurrences, and therefore no
    /// structural class is synthesized for it to shadow.
    pub(super) fn bounded_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Map(key, value) = &rt.conceptual_type else {
                return;
            };
            let Some(bounds) = rt.bounded_map_u64_bounds() else {
                return;
            };
            if rt.is_bounded_pair_map() {
                return;
            }
            let source_bounds = rt
                .config
                .occurrence_bounds()
                .expect("bounded map occurrence carries bounds");
            if self
                .bounded_map_named_owner(key, value, source_bounds, rt.is_preserve_pair_map())
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = bounds;
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded `{{{window} …}}` table occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedMap wrapper"
                ));
            }
        });
        // Rest rows keep K/V flat in the Record IR, so the generic RustType walk above cannot see
        // the restricted BoundedMap class their wasm getter/constructor actually names.  Only a
        // CAPTURED open-struct rest or open-table catch-all reaches that class; a typed row remains
        // flattened on its owner and is deliberately covered only by its loose-builder audit.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record.captured_rest().filter(|row| {
                !row.is_array_tail()
                    && row.container_type().is_bounded_map()
                    && !row.container_type().is_bounded_pair_map()
            }) else {
                continue;
            };
            let container = rest.container_type();
            let source_bounds = container
                .config
                .occurrence_bounds()
                .expect("bounded rest map carries occurrence bounds");
            if self
                .bounded_map_named_owner(rest.domain(), rest.range(), source_bounds, false)
                .is_some()
            {
                continue;
            }
            let restricted = container.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = container
                    .bounded_map_u64_bounds()
                    .expect("bounded rest map has representable occurrence bounds");
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                let row = if record.is_open_table() {
                    format!("the bounded catch-all row of open table '{ident}'")
                } else {
                    format!("the bounded open struct-map rest row of '{ident}'")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for {row} (`{window}`) — rename the rule to avoid \
                     shadowing the restricted BoundedMap wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// Bounded preserve tables mint a distinct `PairMap…MinN/MaxN` class. This stays a parallel
    /// detector so its remedy names the duplicate-preserving carrier rather than the keyed-map twin.
    pub(super) fn bounded_pair_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Map(key, value) = &rt.conceptual_type else {
                return;
            };
            let Some(bounds) = rt.bounded_map_u64_bounds() else {
                return;
            };
            if !rt.is_bounded_pair_map() {
                return;
            }
            let source_bounds = rt
                .config
                .occurrence_bounds()
                .expect("bounded pair-map occurrence carries bounds");
            // A same-shape preserve table is the authored owner of this class, just as a bounded
            // unique-key table is for `BoundedMap`; only an unrelated rule is a collision.
            if self
                .bounded_map_named_owner(key, value, source_bounds, true)
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = bounds;
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded `@duplicates preserve` `{{{window} …}}` \
                     table occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedPairMap wrapper"
                ));
            }
        });
        // The duplicate-preserving rest-row twin is separate on purpose: its structural name and
        // remediation name the BoundedPairMap carrier, not the unique-key BoundedMap above.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record
                .captured_rest()
                .filter(|row| !row.is_array_tail() && row.container_type().is_bounded_pair_map())
            else {
                continue;
            };
            let container = rest.container_type();
            let source_bounds = container
                .config
                .occurrence_bounds()
                .expect("bounded preserve rest map carries occurrence bounds");
            if self
                .bounded_map_named_owner(rest.domain(), rest.range(), source_bounds, true)
                .is_some()
            {
                continue;
            }
            let restricted = container.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = container
                    .bounded_map_u64_bounds()
                    .expect("bounded preserve rest map has representable occurrence bounds");
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                let row = if record.is_open_table() {
                    format!(
                        "the bounded `@duplicates preserve` catch-all row of open table '{ident}'"
                    )
                } else {
                    format!(
                        "the bounded `@duplicates preserve` open struct-map rest row of '{ident}'"
                    )
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for {row} (`{window}`) — rename the rule to avoid \
                     shadowing the restricted BoundedPairMap wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `@duplicates reject` uniqueness-twin emission would turn
    /// into a non-compiling wasm crate — the third container kind's sibling of
    /// `non_empty_wrapper_name_collisions` / `non_empty_map_wrapper_name_collisions`. An INLINE
    /// (anonymous generic-instance) reject set mints a synthesized `<Elem>OrderedSet` /
    /// `NonEmpty<Elem>OrderedSet` wasm class (`reject_ordered_set_wasm_wrapper_name`); a user rule
    /// claiming that ident would silently collide (a plain `pub struct`/`pub type` of the wrong shape).
    /// NAMED reject rules mint under their own rule ident (never a synthesized structural name), so
    /// they are not a source here — only anonymous instances are. The message text is deliberately
    /// distinct from the two NonEmpty siblings' (it names the reject twin, not the NonEmptyVec/Map
    /// wrapper) so a failing spec points at the right container kind.
    pub(super) fn reject_ordered_set_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Array {
                element_type,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            // Only INLINE (anonymous instance) reject sets synthesize a structural class; a named
            // reject rule owns its rule ident and mints there.
            if !rs.config().duplicates_reject() || !self.is_anonymous_collection_instance(ident) {
                continue;
            }
            let mut reject_array =
                RustType::new(ConceptualRustType::Array(Box::new(element_type.clone())))
                    .with_duplicates_policy(Some(DuplicatesPolicy::Reject));
            if let Some(bounds) = bounds {
                reject_array = reject_array.with_occurrence_bounds(*bounds);
            }
            let structural = if reject_array.is_bounded_reject_ordered_set() {
                reject_array.bounded_reject_ordered_set_wasm_wrapper_name(self)
            } else {
                reject_array.reject_ordered_set_wasm_wrapper_name(self)
            };
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an inline `@duplicates reject` set occurrence — rename \
                     the rule to avoid shadowing the restricted OrderedSet wrapper"
                ));
            }
        }
        // A SET NOMINAL wrapper (Phase 2.2 named rule, Phase 2.3 generic instantiation) whose inner is
        // the reject uniqueness twin mints the SAME structural `<Elem>OrderedSet` /
        // `NonEmpty<Elem>OrderedSet` wasm class for its `new()`/`get()` boundary. A user rule claiming
        // that ident silently collides (a `pub struct`/`pub type` of the wrong shape) — the nominal
        // sibling of the inline occurrence above. Message names the reject twin (the "OrderedSet
        // wrapper" pinned substring), distinct from the NonEmpty siblings.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Wrapper { wrapped, .. } = rs.variant() else {
                continue;
            };
            if !rs.config().set_nominal || !wrapped.duplicates_reject() {
                continue;
            }
            let ConceptualRustType::Array(_) = &wrapped.conceptual_type else {
                continue;
            };
            let structural = if wrapped.is_bounded_reject_ordered_set() {
                wrapped.bounded_reject_ordered_set_wasm_wrapper_name(self)
            } else {
                wrapped.reject_ordered_set_wasm_wrapper_name(self)
            };
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for the nominal `@duplicates reject` set `{ident}`'s \
                     boundary — rename the rule to avoid shadowing the restricted OrderedSet wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// Bounded reject sets have their own structural wasm class: both the uniqueness flavor and
    /// occurrence endpoints are encoded, so the class cannot be confused with `BoundedVec` or an
    /// unbounded `OrderedSet`. Keep this a per-kind sibling of the other collision detectors: the
    /// pinned remedy must name the carrier an author is actually shadowing.
    pub(super) fn bounded_reject_ordered_set_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            if !rt.is_bounded_reject_ordered_set() {
                return;
            }
            let structural = rt.bounded_reject_ordered_set_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an inline bounded `@duplicates reject` set occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedOrderedSet wrapper"
                ));
            }
        });
        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the LOOSE `@duplicates preserve` pair-map wrapper
    /// (`PairMapKToV`) would otherwise turn into a non-compiling wasm crate — the pair-map sibling of
    /// `non_empty_wrapper_name_collisions` / `non_empty_map_wrapper_name_collisions` /
    /// `reject_ordered_set_wrapper_name_collisions`. The container flavor is part of the structural
    /// name, so a preserve map and a default map of the identical key/value derive DIFFERENT classes
    /// and can never be asked to be one shape; what remains is the same hazard every other kind has —
    /// a user rule whose ident happens to spell the synthesized class name.
    ///
    /// Sources that mint or alias the loose `PairMapKToV` class: a preserve open-map REST ROW (its
    /// capture field's wasm getter returns the structural class), an ANONYMOUS preserve table instance
    /// (`ptbl<uint, tstr>`, which routes through the structural wrapper via its passthrough alias), a
    /// NAMED preserve `{* …}` rule that solely owns the shape (its `pub type PairMapKToV = <Owner>;`
    /// alias claims the ident beside the class), a named preserve restricted table whose `try_from`
    /// source is the loose pair-map builder (including its bounded direct-key source), and a
    /// `@newtype`/TAG-forced WRAPPER over an inline
    /// `{* k => v} ; @duplicates preserve` (its `new`/getter boundary names the structural class,
    /// which the wasm struct walk mints for exactly that inner). A user rule that IS a plain preserve
    /// `{* k => v}` table of
    /// the same key/value is not a collision — that rule IS the builder (shared, exactly as the
    /// default-flavored sibling shares it); a rule that is the WRAPPER is, because a wrapper is a
    /// nominal type of its own and cannot double as the builder class. Message text is deliberately
    /// distinct from the other kinds'
    /// (it names the pair-map twin) so a failing spec points at the right container kind.
    pub(super) fn preserve_pair_map_loose_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        let check = |structural: String,
                     key: &RustType,
                     value: &RustType,
                     minted_by: String,
                     msgs: &mut BTreeSet<String>| {
            if self.wasm_ident_claimed_by_user_rule(&structural)
                && !self.provides_compatible_loose_table(
                    &structural,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    true,
                )
            {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for {minted_by} — rename the rule to avoid shadowing the \
                     loose `@duplicates preserve` PairMap wrapper (or make it a `{{* …}}` \
                     `@duplicates preserve` table of the same key/value, which IS that wrapper)"
                ));
            }
        };
        for (ident, rs) in self.rust_structs.iter() {
            match rs.variant() {
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    if !rs.config().duplicates_preserve() {
                        continue;
                    }
                    let bounded_source = bounds.is_some_and(|candidate| {
                        type_enforced_bounded_window(candidate.raw(), false).is_some()
                    });
                    let (structural, builder_key, source) = if bounded_source {
                        (
                            RustType::wasm_loose_table_builder_name_for(domain, range, true, self)
                                .to_string(),
                            domain.loosened_for_wasm_table_boundary_key(),
                            true,
                        )
                    } else {
                        (
                            RustType::wasm_structural_map_name_for(domain, range, true, self)
                                .to_string(),
                            domain.clone(),
                            bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty),
                        )
                    };
                    // A self-named loose or `{+}` rule legitimately owns the ident for its own
                    // class. A bounded rule does not: its restricted wrapper and loose checked
                    // source would otherwise claim the same ident with incompatible carriers.
                    if !bounded_source && structural == ident.to_string() {
                        continue;
                    }
                    let minted_by = if bounds
                        .is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                    {
                        format!(
                            "the `@duplicates preserve` `{{+ …}}` rule '{ident}'s `try_from` source"
                        )
                    } else if source {
                        format!(
                            "the bounded `@duplicates preserve` rule '{ident}'s loose `try_from` source"
                        )
                    } else {
                        format!("the `@duplicates preserve` table rule '{ident}'")
                    };
                    check(structural, &builder_key, range, minted_by, &mut msgs);
                }
                RustStructType::Record(record) => {
                    // CAPTURED rows only: an `@ignore` row has no field and no getter, so it mints
                    // no wrapper and can claim no ident (same gate as the mint itself).
                    //
                    // `captured_rest()` and not `captured_dynamic_rows()`, for the reason its
                    // default-flavored twin in `non_empty_map_wrapper_name_collisions` records: an
                    // open table's TYPED row is flattened onto the minted struct's own wasm class
                    // and mints no PairMap container, so it has no ident to be shadowed.
                    let Some(rest) = record.captured_rest().filter(|r| {
                        !r.is_array_tail() && r.duplicates() == Some(DuplicatesPolicy::Preserve)
                    }) else {
                        continue;
                    };
                    let container = rest.container_type();
                    let bounded = container.is_bounded_map();
                    let (structural, builder_key) = if bounded {
                        (
                            RustType::wasm_loose_table_builder_name_for(
                                rest.domain(),
                                rest.range(),
                                true,
                                self,
                            )
                            .to_string(),
                            rest.domain().loosened_for_wasm_table_boundary_key(),
                        )
                    } else {
                        (
                            RustType::wasm_structural_map_name_for(
                                rest.domain(),
                                rest.range(),
                                true,
                                self,
                            )
                            .to_string(),
                            rest.domain().clone(),
                        )
                    };
                    check(
                        structural,
                        &builder_key,
                        rest.range(),
                        if record.is_open_table() {
                            if bounded {
                                format!(
                                    "the bounded `@duplicates preserve` catch-all row of '{ident}' \
                                     as its loose `try_from` source"
                                )
                            } else {
                                format!("the `@duplicates preserve` catch-all row of '{ident}'")
                            }
                        } else {
                            if bounded {
                                format!(
                                    "the bounded `@duplicates preserve` rest row of '{ident}' as \
                                     its loose `try_from` source"
                                )
                            } else {
                                format!("the `@duplicates preserve` rest row of '{ident}'")
                            }
                        },
                        &mut msgs,
                    );
                }
                // A `@newtype`- or TAG-forced wrapper over an inline `{* k => v} ; @duplicates
                // preserve`: the wasm struct walk mints the structural `PairMapKToV` class its
                // `new`/getter boundary names. The `{+ …}` flavor is NOT here — its restricted
                // `NonEmptyPairMapKToV` class and its loose `try_from` source are both claimed by
                // `non_empty_map_wrapper_name_collisions`' inline leg, whose naming is flavor-aware,
                // so routing it here too would emit two messages for one collision.
                RustStructType::Wrapper { wrapped, .. } => {
                    if !wrapped.is_preserve_pair_map() || wrapped.is_non_empty_map() {
                        continue;
                    }
                    let ConceptualRustType::Map(domain, range) = &wrapped.conceptual_type else {
                        unreachable!("is_preserve_pair_map implies a Map conceptual type");
                    };
                    let structural = wrapped.wasm_structural_map_name(self).to_string();
                    check(
                        structural,
                        domain,
                        range,
                        format!("the `@duplicates preserve` table wrapped by rule '{ident}'"),
                        &mut msgs,
                    );
                }
                _ => {}
            }
        }
        // A bounded TYPED row is flattened on its record class, but its fallible wasm constructor
        // receives this one loose PairMap builder. It mints no restricted whole-row class, so audit
        // precisely that builder ident and no fictional `PairMap…Min…` wrapper. The default-flavored
        // sibling is the final Record loop in
        // `non_empty_map_wrapper_name_collisions`.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(typed) = record.typed_row().filter(|row| {
                row.container_type().bounded_map_u64_bounds().is_some()
                    && row.duplicates() == Some(DuplicatesPolicy::Preserve)
            }) else {
                continue;
            };
            let builder = typed.staging_container_type();
            let ConceptualRustType::Map(key, value) = &builder.conceptual_type else {
                unreachable!("an open table typed row's staging carrier is a Map");
            };
            check(
                builder.wasm_structural_map_name(self).to_string(),
                key,
                value,
                format!(
                    "the bounded `@duplicates preserve` typed row of open table \
                     '{ident}'"
                ),
                &mut msgs,
            );
        }
        msgs.into_iter().collect()
    }

    /// The open table's own member-name check, and the only wasm-surface hazard its minted class
    /// creates. Flattening the TYPED row's map surface onto the class puts `len`/`insert`/`get`/
    /// `has`/`keys` on the same `#[wasm_bindgen]` impl the CATCH-ALL row's getter lands on, and that
    /// getter is named by the row (`rest` by default, anything under `@name`). A row named for one
    /// of the five would emit two methods of one name — E0592 in the generated wasm crate, at the
    /// exact remove where the spec author cannot see it.
    ///
    /// All five are reserved unconditionally, `has` included even though it is emitted only for a
    /// nullable typed value: making the reservation depend on the VALUE's nullability would mean a
    /// row name that generates today stops generating when an unrelated `/ null` is added to the
    /// typed row, which is a worse surprise than the flat rule.
    pub(super) fn open_table_flattened_accessor_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        const FLATTENED_ACCESSORS: &[&str] = &["get", "has", "insert", "keys", "len"];
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            if !record.is_open_table() {
                continue;
            }
            let Some(rest) = record.captured_rest() else {
                continue;
            };
            if FLATTENED_ACCESSORS.contains(&rest.field_name.as_str()) {
                let reserved = FLATTENED_ACCESSORS
                    .iter()
                    .map(|a| format!("`{a}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                msgs.insert(format!(
                    "name collision: the open table '{ident}' names its catch-all row '{}', which \
                     is one of the accessors ({reserved}) its wasm class flattens onto itself from \
                     the TYPED row — rename the row with `@name` so the catch-all getter and the \
                     flattened accessor do not claim one method",
                    rest.field_name,
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// The min-1 sibling of `preserve_pair_map_loose_wrapper_name_collisions`: an ANONYMOUS
    /// `@duplicates preserve` `{+ k => v}` table instance (a generic instantiation like
    /// `pnetbl<uint, tstr>`) mints the synthesized `NonEmptyPairMapKToV` class and routes to it through
    /// its passthrough alias, so a user rule claiming that ident silently shadows it. Named preserve
    /// `{+ …}` rules mint under their own rule ident and are not a source here — the same
    /// anonymous-instance-only scope as `reject_ordered_set_wrapper_name_collisions`' first leg. INLINE
    /// `{+ …}` occurrences are covered by the default-flavored twin
    /// (`non_empty_map_wrapper_name_collisions`), whose inline leg reads the flavor off the
    /// occurrence — which is what covers the one preserve-flavored inline shape that IS expressible:
    /// the inner of a `@newtype`/tag-forced wrapper over `{+ k => v} ; @duplicates preserve`, whose
    /// restricted class and loose `try_from` source that leg claims together (one collision, one
    /// message).
    pub(super) fn preserve_pair_map_non_empty_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if !rs.config().duplicates_preserve()
                || !bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                || !self.is_anonymous_collection_instance(ident)
            {
                continue;
            }
            let structural = format!(
                "NonEmpty{}",
                RustType::wasm_structural_map_name_for(domain, range, true, self)
            );
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an anonymous `@duplicates preserve` `{{+ …}}` table \
                     instance — rename the rule to avoid shadowing the restricted NonEmptyPairMap \
                     wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// `@extern_companions` names classes that live in a SIBLING crate, so this crate must not also
    /// define one. A top-level rule of this crate's own spec whose ident equals a listed class is
    /// that contradiction: the deferral emits `use <prefix>::<Class>;` while the rule mints a class
    /// of the same name, which is rustc E0255 in the generated wasm crate — a failure whose message
    /// names neither the directive nor the rule. Reported here instead, in the spec's own terms.
    ///
    /// The claim test is "is a top-level rule of an EXPORTED scope" (`scopes`, populated once per
    /// parsed rule) rather than the `rust_structs`/`type_aliases` membership the structural detectors
    /// use: those registries also hold generator-SYNTHESIZED wrappers, and a synthesized wrapper of a
    /// listed name is precisely what the directive suppresses — reading it as a claim would reject
    /// every correct use. A same-named rule in a dependency scope is a different crate's business.
    pub(super) fn extern_companion_rule_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        for (owner, companions) in &self.rule_directives.extern_companions {
            for class in &companions.classes {
                let ident = RustIdent::new(CDDLIdent::new(class.clone()));
                if !self.is_toplevel_rule(&ident) || !self.scope(&ident).export() {
                    continue;
                }
                let claimant = self.source_rule_name(&ident).unwrap_or(class);
                let prefix = &companions.path_prefix;
                msgs.insert(format!(
                    "@extern_companions on `{owner}` declares that `{class}` already exists in \
                     `{prefix}`, but this crate's own rule `{claimant}` also defines `{class}` — the \
                     generated wasm crate would both `use {prefix}::{class};` and define it (E0255). \
                     Either drop `{class}` from the directive's class list (this crate owns it, and \
                     the sibling's class is a DIFFERENT type across the package boundary), or rename \
                     the rule."
                ));
            }
        }
        msgs.into_iter().collect()
    }
}
