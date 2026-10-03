use super::name_rejections::custom_codec_zero_demand_rejection;
use super::*;

impl<'a> IntermediateTypes<'a> {
    /// Refuse a JSON-derived surface before it reaches rustc when a wide native exact array sits in
    /// a containing shape the current static-array adapters do not own. The representation itself
    /// remains valid without JSON; direct and all sequence/set-tree field/newtype and open-array
    /// forms stay accepted, along with map values and positional preserve-pair entries. Object-map
    /// keys remain outside this JSON handover boundary.
    fn reject_unadapted_wide_static_array_json_shapes(&mut self, cli: &Cli) {
        if !(cli.json_serde_derives || cli.json_schema_export) {
            return;
        }
        fn legacy_direct_typed_static_array_sequence(ty: &RustType, field_optional: bool) -> bool {
            if field_optional
                || ty.duplicates_reject()
                || !matches!(ty.config.raw_bounds(), None | Some((None, None)))
            {
                return false;
            }
            let ConceptualRustType::Array(element) = ty.conceptual_type.resolve_alias_shallow()
            else {
                return false;
            };
            let ConceptualRustType::Array(inner) = element.conceptual_type.resolve_alias_shallow()
            else {
                return false;
            };
            element.exact_homogeneous_array_len_checked().is_some()
                && !inner.contains_wide_static_array()
                && !ty.contains_exact_natural_any_static_array()
        }

        fn check(
            types: &IntermediateTypes,
            rejections: &mut BTreeSet<String>,
            rule: &RustIdent,
            site: &str,
            ty: &RustType,
            field_optional: bool,
        ) {
            // Every recursive value and positional pair-map entry is adapted. The remaining
            // rejected shape is an exact-array tree used as a JSON OBJECT member name. Keep this
            // alias-aware rather than exempting a whole tree merely because it contains PairMap.
            fn contains_unadapted_object_map_key(types: &IntermediateTypes, ty: &RustType) -> bool {
                match &ty.conceptual_type {
                    ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => types
                        .type_aliases()
                        .get(&AliasIdent::Rust(ident.clone()))
                        .is_some_and(|alias| {
                            contains_unadapted_object_map_key(types, &alias.base_type)
                        }),
                    ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                        contains_unadapted_object_map_key(types, inner)
                    }
                    ConceptualRustType::Map(key, value) => {
                        (!ty.is_preserve_pair_map()
                            && (key.contains_wide_static_array()
                                || key.contains_exact_natural_any_static_array()))
                            || contains_unadapted_object_map_key(types, key)
                            || contains_unadapted_object_map_key(types, value)
                    }
                    _ => false,
                }
            }
            if ty.has_unadapted_wide_static_array_json_shape()
                && contains_unadapted_object_map_key(types, ty)
                && !legacy_direct_typed_static_array_sequence(ty, field_optional)
            {
                rejections.insert(format!(
                    "rule `{rule}`: {site} contains a wide exact homogeneous array in a JSON shape \
                     the generated adapters do not yet compose through. The pinned serde/schemars \
                     versions implement array traits only through length 32, so emitting this crate \
                     would fail to compile. Generate without --json-serde-derives/--json-schema-export, \
                     or avoid an object-map key containing that exact array. Loose, nonempty, and \
                     bounded sequence, map, duplicate-reject set, pair-map, and dynamic-row values \
                     are supported."
                ));
            }
            if ty.has_unadapted_natural_any_static_array_json_shape()
                && contains_unadapted_object_map_key(types, ty)
            {
                rejections.insert(format!(
                    "rule `{rule}`: {site} nests an exact CDDL `any` array behind a JSON container \
                     the natural-JSON adapters do not yet compose through. Emitting it would route \
                     each AnyCbor element through its tagged AnyCbor JSON codec instead of the required \
                     natural JSON value. Generate without --json-serde-derives/--json-schema-export, \
                     or avoid an object-map key containing that exact array. Sequence, map value, \
                     duplicate-reject set, pair-map, and dynamic-row positions are supported."
                ));
            }
        }
        let mut rejections = BTreeSet::new();
        for rust_struct in self.rust_structs.values() {
            if rust_struct.config().custom_json {
                continue;
            }
            let rule = rust_struct.ident();
            match rust_struct.variant() {
                RustStructType::Record(record) => {
                    for field in &record.fields {
                        // Optional+nullable exact-array fields have a presence-aware recursive
                        // adapter: the descriptor owns the inner nullable value and the callback
                        // owns the outer presence Option. They are ordinary collection trees here;
                        // map/table and dynamic-row containment remains below this field path's
                        // adapter boundary.
                        check(
                            self,
                            &mut rejections,
                            rule,
                            &format!("field `{}`", field.name),
                            &field.rust_type,
                            field.optional,
                        );
                    }
                    // Array occurrence segments are derive-owned struct fields and use the same
                    // recursive/legacy selector as a declared field at their dedicated emission
                    // seam. Dynamic MAP rows still lack a map descriptor, so the shared predicate
                    // accepts the supported sequence/set tree and refuses only map containment.
                    for row in record.dynamic_rows() {
                        let container = row.container_type();
                        let site = if row.is_array_tail() {
                            format!("open-array segment `{}`", row.field_name)
                        } else {
                            format!("dynamic map row `{}`", row.field_name)
                        };
                        check(self, &mut rejections, rule, &site, &container, false);
                    }
                }
                RustStructType::Wrapper { wrapped, .. } => {
                    check(
                        self,
                        &mut rejections,
                        rule,
                        "wrapper payload",
                        wrapped,
                        false,
                    );
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    for variant in variants {
                        match &variant.data {
                            EnumVariantData::RustType(ty) => check(
                                self,
                                &mut rejections,
                                rule,
                                "type-choice payload",
                                ty,
                                false,
                            ),
                            EnumVariantData::Inlined(record) => {
                                for field in &record.fields {
                                    check(
                                        self,
                                        &mut rejections,
                                        rule,
                                        &format!("type-choice field `{}`", field.name),
                                        &field.rust_type,
                                        field.optional,
                                    );
                                }
                            }
                        }
                    }
                }
                RustStructType::Array { .. }
                | RustStructType::Table { .. }
                | RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
        for rejection in rejections {
            self.record_rejection(rejection);
        }
    }

    /// Converge each SYNTHESIZED anonymous generic-collection instance onto the anonymous INLINE
    /// collection path for the wasm boundary. Wasm only.
    ///
    /// An anonymous instance (`[a: set<key_hash>]` → `SetKeyHash`) that resolves to a transparent
    /// collection is registered — exactly like a bare `foo = #6.258([* key_hash])` rule — as an
    /// `Array`/`Table`-variant `RustStruct` PLUS a transparent alias whose `gen_wasm_alias` is `false`,
    /// so the wasm struct walk mints a `#[wasm_bindgen]` class under the RULE ident (`SetKeyHash`).
    /// That is wrong for a synthesized name: the equivalent inline `[* key_hash]` field mints its
    /// wrapper under the STRUCTURAL name (`KeyHashList`). Two spellings of one anonymous shape then
    /// define two wasm classes for one concept — and a `--wrapper-requests` consumer importing the
    /// structural name hard-errors (the synthesized name sits in the own-spec shape projection).
    ///
    /// Fix: flip the instance alias's `gen_wasm_alias` to `true` and record the ident here. The alias
    /// loop then emits `pub type SetKeyHash = KeyHashList;` (`for_wasm_member` on the alias base is
    /// bounds-aware, so `[+ …]` yields the `NonEmpty…List` name and a directly-exposable element
    /// yields the bare `Vec<…>`), the base-type walk mints the STRUCTURAL wrapper (recording it in
    /// the own-spec shape projection under the structural name), and the struct walk is told to SKIP the
    /// rule-named class mint (via `is_anonymous_collection_instance`). The rust side is untouched — the
    /// transparent `pub type SetKeyHash = Vec<KeyHash>;` alias and every rust reference to it stay
    /// byte-identical. NAMED instance rules (`named_set = set<key_hash>`, ident from the author's rule)
    /// are NOT anonymous, keep their own wasm class, and keep the criterion-8 `--wrapper-requests`
    /// contract. Runs alongside `resolve_late_alias_product_leaves` (both after the late
    /// instance aliases exist); order between them does not matter — this only edits alias flags.
    fn converge_anonymous_collection_instance_wasm(&mut self, cli: &Cli) {
        if !cli.wasm {
            return;
        }
        // (ident, gets_wrapper) for every anonymous instance resolving to a collection.
        // `gets_wrapper` is true for a `[+ …]` (always wrapped), a `@duplicates reject` set (always
        // crosses through its `OrderedSet`/`NonEmptyOrderedSet` wrapper), or a non-exposable element
        // (`set<key_hash>`), and false for a directly-exposable loose collection (`set<uint>` → bare
        // `Vec<u64>`). Every wrapper-getting instance routes through the structural-wrapper alias
        // passthrough below — for a reject set that structural wrapper is the uniqueness twin.
        let anon_collection: Vec<(RustIdent, bool)> = self
            .generic_instances
            .values()
            .filter(|i| i.anonymous)
            .filter_map(|i| {
                let rt = self.resolve_alias(&AliasIdent::Rust(i.instance_ident.clone()))?;
                let shallow = rt.conceptual_type.resolve_alias_shallow();
                // Non-collection anonymous instances (a generic EXTERN, `extern_generic<foo>`, whose
                // alias base is a bare `Rust(...)`) are out of scope AND must not reach the
                // exposability probe below: `directly_wasm_exposable_ct` calls `is_enum`, whose
                // rust-struct/generic-instance assertion an extern's synthesized element ident would
                // trip. Screen them out first.
                if !matches!(
                    shallow,
                    ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
                ) {
                    return None;
                }
                // Exposability is tested on the RESOLVED collection (a `[+ …]` is never exposable;
                // otherwise ask the shallow-resolved shape) — NOT on the `Alias` wrapper, whose own
                // rust-struct entry would wrongly report "wrapped" for the very type we are converging.
                // A reject-mode set is never directly exposable (it crosses through its OrderedSet
                // wrapper, like `[+ …]`), so it always gets a wrapper — the conceptual exposability
                // probe below can't see the policy (it lives on the RustType config), so add it here.
                let gets_wrapper = rt.is_type_enforced_non_empty()
                    || rt.duplicates_reject()
                    || !shallow.directly_wasm_exposable_ct(self);
                Some((i.instance_ident.clone(), gets_wrapper))
            })
            .collect();
        for (ident, gets_wrapper) in anon_collection {
            // Every anonymous collection instance skips the rule-named class mint (recorded here). The
            // WRAPPER subset additionally routes through a `gen_wasm_alias` passthrough to the
            // STRUCTURAL wrapper (`pub type SetKeyHash = KeyHashList;` for the loose case, or
            // `pub type OsetU64 = U64OrderedSet;` for a `@duplicates reject` set — `for_wasm_member`
            // on the alias base picks the twin name). The exposable
            // subset needs no alias: `resolve_late_alias_product_leaves` lowers its field to
            // the bare inline collection (`Vec<u64>`), so the wasm boundary crosses by value exactly
            // like an inline `[* uint]` — no wrapper class, no `&Vec` ctor param (no `RefFromWasmAbi`).
            if gets_wrapper
                && let Some(alias) = self.type_aliases.get_mut(&AliasIdent::Rust(ident.clone()))
            {
                alias.enable_wasm_passthrough();
            }
            self.anonymous_collection_instances.insert(ident);
        }
    }

    /// Re-resolve finalized product leaves whose transparent alias registered after the parse-time
    /// use-site was built.
    ///
    /// A non-generic collection rule (`foo = #6.258([* uint])`) registers its transparent alias in
    /// `type_aliases` at PARSE time, so a use-site field referencing it resolves through `new_type`
    /// to the structural `Alias(Foo, Array(..))` conceptual type right where the field is built. A
    /// generic instance's alias (`xs_int = xs<uint>`) is instead registered only in `finalize` (the
    /// `GenericResolved::Resolved` → [`Self::register_rust_struct`] Array/Table arms), AFTER every
    /// use-site field was already built — so such a field keeps an unresolved `Rust(xs_int)`
    /// conceptual type, which generation turns into `self.field.serialize(..)` /
    /// `XsInt::deserialize(..)` method calls a bare `Vec`/`BTreeMap` alias has no impls for.
    ///
    /// Now that the late aliases exist, re-run alias substitution on the affected product leaves so
    /// the generic path converges on the SAME `Alias(ident, Array/Map)` type the non-generic path
    /// gets — one collection code path (inline serialize, per-field len/elem/tag encoding vars), not
    /// a parallel one. Scoped to instances whose alias base is a structural `Array`/`Map`: generic
    /// EXTERN instances (alias base `Rust(real_ident)`, registered above with the same
    /// `gen_rust_alias=true`) resolve to a `Rust` type, are excluded here, and stay byte-identical.
    ///
    /// An ordinary forward alias needs the same one-step repair when auto-`@newtype` turns its target
    /// into a wrapper: the wrapper's parse-time `Rust(Alias)` leaf otherwise names no struct even
    /// though the finalized alias table can resolve it. Include every emitted rust alias with no
    /// `RustStruct`, using `resolve_alias` exactly as `new_type` would have if registration had
    /// preceded the use. The walk intentionally does not rewrite alias-table bases or recurse into a
    /// replacement in this pass: one `Alias` node is the terminating named boundary, and alias cycles
    /// remain the recursive-type boundary's responsibility.
    fn resolve_late_alias_product_leaves(&mut self) {
        // Snapshot the resolved alias `RustType` for each generic-collection instance (base resolves
        // to Array/Map), cloned out first so the field walk can borrow `rust_structs` mutably. The
        // snapshot value is exactly what `new_type` would have returned at parse time: the alias's
        // `Alias(ident, Array/Map)` conceptual type carrying the optional-tag encoding op and the
        // occurrence bounds.
        let mut resolved: BTreeMap<RustIdent, RustType> = BTreeMap::new();
        let idents: Vec<RustIdent> = self.generic_instances.keys().cloned().collect();
        for ident in idents {
            let Some(rt) = self.resolve_alias(&AliasIdent::Rust(ident.clone())) else {
                continue;
            };
            let shallow = rt.conceptual_type.resolve_alias_shallow();
            // A NAMED set-nominal binding (`named_set = set<key_hash>`) resolves to
            // `Alias(NamedSet, Rust(SetKeyHash))` whose base is the instantiation NOMINAL struct — not
            // a transparent Array/Map. Its use-site fields were built as a bare `Rust(NamedSet)` at
            // parse time (before the late alias existed), which generation would look up as a struct
            // and panic. Re-resolve those leaves to the `Alias` so they name the nominal through the
            // `pub type NamedSet = SetKeyHash;` binding (Phase 2.3). Anonymous instances already carry
            // `Rust(SetKeyHash)` at their fields (the canonical IS a registered struct) and need no
            // re-resolution here.
            let is_set_nominal_alias = matches!(
                shallow,
                ConceptualRustType::Rust(id)
                    if self.rust_struct(id).map(|rs| rs.config().set_nominal).unwrap_or(false)
            );
            if is_set_nominal_alias {
                resolved.insert(ident, rt);
                continue;
            }
            if !matches!(
                shallow,
                ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
            ) {
                continue;
            }
            // A SYNTHESIZED anonymous instance that resolves to a DIRECTLY-exposable collection
            // (`set<uint>` → `Vec<u64>`, populated by `converge_anonymous_collection_instance_wasm`)
            // lowers its field to the BARE collection — the alias wrapper is dropped, so the field's
            // type is exactly the inline `[* uint]` shape (`Vec<u64>`, not `Alias(SetU64, …)`). That
            // makes it cross the wasm boundary by value with no wrapper class, byte-identical to the
            // inline equivalent, instead of a `&SetU64` ref param with no `RefFromWasmAbi`. The bare
            // base keeps the collapsed-set encoding op (the optional tag rides on `base_type`), so the
            // rust CBOR bytes are unchanged; only the rust field-type SPELLING becomes `Vec<u64>`
            // (the same type the `pub type SetU64 = Vec<u64>` alias still names). Wrapper-getting
            // anonymous instances (non-exposable / `[+ …]`) and NAMED instances keep the `Alias`.
            let exposable_anon = self.is_anonymous_collection_instance(&ident)
                && !rt.is_type_enforced_non_empty()
                && !rt.duplicates_reject()
                && shallow.directly_wasm_exposable_ct(self);
            let replacement = if exposable_anon {
                self.type_aliases
                    .get(&AliasIdent::Rust(ident.clone()))
                    .map(|info| info.base_type.clone())
            } else {
                Some(rt)
            };
            if let Some(replacement) = replacement {
                resolved.insert(ident, replacement);
            }
        }
        // Rules are normally resolved when their use-site parses. The exception here is a forward
        // alias whose target becomes a wrapper only after the recursive-type boundary asks for an
        // auto-`@newtype` re-pass: its earlier leaf stayed `Rust(Alias)` because no alias existed at
        // that point. Add every finalized emitted alias with no struct owner through the SAME
        // `resolve_alias` path, while leaving the generic-specific convergence decisions above in
        // charge when they already supplied a replacement.
        for (alias_ident, alias_info) in &self.type_aliases {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            if !alias_info.keeps_alias_node() || self.rust_struct(ident).is_some() {
                continue;
            }
            if let Some(replacement) = self.resolve_alias(alias_ident) {
                resolved.entry(ident.clone()).or_insert(replacement);
            }
        }
        if resolved.is_empty() {
            return;
        }
        // The DANGLING, encoding-free subset of `resolved`: aliases that name no registered struct,
        // so a surviving `Rust(<alias>)` leaf refers to nothing generation can look up. This includes
        // a NAMED set-nominal binding (`xs_int = xs<uint>` over a tag-258 set idiom), whose resolution
        // mints the struct under the INSTANTIATION canonical (`XsU64`) and gives the binding ident
        // only a transparent `pub type XsInt = XsU64;` alias, plus the ordinary forward alias above.
        // A transparent-collection instance and a generic EXTERN instance both DO register a struct
        // under their own ident, so their leaves are well-formed and stay untouched — which is why
        // the `Alias`-box descent below is restricted to this subset rather than run over all of
        // `resolved`: repairing only what dangles keeps every already-working shape's emitted bytes
        // identical. The conceptual type alone is carried because an `Alias` box holds a
        // `ConceptualRustType` with nowhere to put encodings; the filter demands the resolved type
        // have none, so nothing can be dropped silently.
        let dangling: BTreeMap<RustIdent, ConceptualRustType> = resolved
            .iter()
            .filter(|(ident, rt)| self.rust_struct(ident).is_none() && rt.encodings.is_empty())
            .map(|(ident, rt)| (ident.clone(), rt.conceptual_type.clone()))
            .collect();
        // Replace a `Rust(alias)` leaf with the one-step resolved alias, keeping any
        // reference-site encodings (`#6.24(xs_int)`-style outer wraps) OUTSIDE the alias's own by
        // appending them. Recurses into structural children first so `[* xs_int]` / `? xs_int` reach
        // the leaf.
        //
        // An `Alias` box is descended too, for the `dangling` subset only. A SECOND alias hop
        // (`bar = xs_int`, then `[b: bar]` / `[* bar]`) registers `Bar`'s own base as the bare
        // instance leaf built at parse time, so the use site arrives here as
        // `Alias(Bar, Rust(XsInt))` — the leaf the one-hop case exposes at top level is one box
        // deeper, and leaving it there aborted generation on the set-idiom flavor. Chains of any
        // depth and leaves under a container inside the box (`Alias(Bar, Array(Rust(XsInt)))`)
        // recurse back through here.
        fn walk(
            rt: &mut RustType,
            resolved: &BTreeMap<RustIdent, RustType>,
            dangling: &BTreeMap<RustIdent, ConceptualRustType>,
        ) {
            match &mut rt.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, resolved, dangling)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, resolved, dangling);
                    walk(v, resolved, dangling);
                }
                ConceptualRustType::Alias(_, inner) => walk_ct(inner, resolved, dangling),
                _ => {}
            }
            let replacement = match &rt.conceptual_type {
                ConceptualRustType::Rust(ident) => resolved.get(ident).cloned(),
                _ => None,
            };
            if let Some(mut new_rt) = replacement {
                new_rt.encodings.append(&mut rt.encodings);
                *rt = new_rt;
            }
        }
        /// The inside-an-`Alias`-box half of `walk`. Only `dangling` leaves are substituted here
        /// (see its definition); structural children are full `RustType`s and hand back to `walk`,
        /// which applies the encodings-preserving replacement they can hold.
        fn walk_ct(
            ct: &mut ConceptualRustType,
            resolved: &BTreeMap<RustIdent, RustType>,
            dangling: &BTreeMap<RustIdent, ConceptualRustType>,
        ) {
            match ct {
                ConceptualRustType::Alias(_, inner) => walk_ct(inner, resolved, dangling),
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, resolved, dangling)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, resolved, dangling);
                    walk(v, resolved, dangling);
                }
                _ => {}
            }
            if let ConceptualRustType::Rust(ident) = ct
                && let Some(replacement) = dangling.get(ident)
            {
                *ct = replacement.clone();
            }
        }
        for rust_struct in self.rust_structs.values_mut() {
            match &mut rust_struct.variant {
                RustStructType::Record(record) => {
                    for field in record.fields.iter_mut() {
                        walk(&mut field.rust_type, &resolved, &dangling);
                    }
                }
                RustStructType::Table { domain, range, .. } => {
                    walk(domain, &resolved, &dangling);
                    walk(range, &resolved, &dangling);
                }
                RustStructType::Array { element_type, .. } => {
                    walk(element_type, &resolved, &dangling);
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    for variant in variants.iter_mut() {
                        match &mut variant.data {
                            EnumVariantData::RustType(ty) => walk(ty, &resolved, &dangling),
                            EnumVariantData::Inlined(rec) => {
                                for field in rec.fields.iter_mut() {
                                    walk(&mut field.rust_type, &resolved, &dangling);
                                }
                            }
                        }
                    }
                }
                RustStructType::Wrapper { wrapped, .. } => walk(wrapped, &resolved, &dangling),
                RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    }

    /// Mint the keys-list array wrapper for each exported table whose domain was not final when the
    /// table registered (deferred from `register_rust_struct`; the two classes are
    /// `table_keys_list_mint_must_defer`'s). Runs in `finalize` AFTER
    /// `resolve_late_alias_product_leaves` has rewritten each generic-instance domain to its
    /// resolved collection, and after every rule in the spec has registered — so the wrapper name
    /// derives from the FINAL domain and matches the wasm `keys()` accessor. Wasm-only (rust maps use
    /// native `.keys()`; the wrapper exists only to cross the wasm boundary). Guarded by "not already
    /// registered": a table whose domain was final at parse minted its keys-list there and is a no-op
    /// here — so the byte output for any spec without a deferred domain is unchanged. Deterministic
    /// (`BTreeMap` order).
    /// If two deferred tables resolve to the SAME boundary name, both pass the not-registered filter
    /// before either mints; the table-keys registration mode retains the first synthesized loose
    /// boundary carrier. Bounds and encodings on the original key occurrence are deliberately not
    /// properties of the returned keys-list class.
    fn finalize_deferred_table_keys_lists(&mut self, parent_visitor: &ParentVisitor, cli: &Cli) {
        if !cli.wasm {
            return;
        }
        let deferred: Vec<RustType> = self
            .rust_structs
            .iter()
            .filter_map(|(ident, rs)| match rs.variant() {
                RustStructType::Table { domain, .. } if self.scope(ident).export() => {
                    let loose_domain = domain.loosened_for_wasm_table_boundary_key();
                    let name = loose_domain.name_as_wasm_array(self);
                    // A directly-exposable KEYS-LIST (`{ * uint => v }` -> bare `Vec<u64>` keys) mints
                    // no wrapper; `create_and_register_array_type` returns early on it, so only a real
                    // wrapper name that is not already registered identifies a deferred mint.
                    (!ConceptualRustType::Array(Box::new(domain.clone()))
                        .directly_wasm_exposable_ct(self)
                        && !self
                            .rust_structs
                            .contains_key(&RustIdent::from_formatted(name.clone())))
                    .then(|| domain.clone())
                }
                _ => None,
            })
            .collect();
        for domain in deferred {
            let name = domain
                .loosened_for_wasm_table_boundary_key()
                .name_as_wasm_array(self);
            self.create_and_register_array_type(parent_visitor, domain, &name, cli);
        }
    }

    // Within records and choices, visit only fields and typed enum payloads, excluding dynamic
    // rows, inlined records, and group choices. Alias bodies also stay outside this local walk.
    // Append in child order, retaining duplicates; callers own the reachable-template closure.
    pub(super) fn collect_generic_placeholders(
        rust_struct: &RustStruct,
        template_idents: &BTreeSet<RustIdent>,
        output: &mut Vec<RustIdent>,
    ) {
        match rust_struct.variant() {
            RustStructType::Record(record) => {
                for field in &record.fields {
                    Self::collect_generic_placeholders_in_type(
                        &field.rust_type,
                        template_idents,
                        output,
                    );
                }
            }
            RustStructType::Table { domain, range, .. } => {
                Self::collect_generic_placeholders_in_type(domain, template_idents, output);
                Self::collect_generic_placeholders_in_type(range, template_idents, output);
            }
            RustStructType::Array { element_type, .. } => {
                Self::collect_generic_placeholders_in_type(element_type, template_idents, output)
            }
            RustStructType::TypeChoice { variants } | RustStructType::CStyleEnum { variants } => {
                for variant in variants {
                    if let EnumVariantData::RustType(ty) = &variant.data {
                        Self::collect_generic_placeholders_in_type(ty, template_idents, output);
                    }
                }
            }
            RustStructType::Wrapper { wrapped, .. } => {
                Self::collect_generic_placeholders_in_type(wrapped, template_idents, output)
            }
            RustStructType::GroupChoice { .. }
            | RustStructType::Extern
            | RustStructType::RawBytesType => {}
        }
    }

    pub(super) fn collect_generic_placeholders_in_type(
        ty: &RustType,
        template_idents: &BTreeSet<RustIdent>,
        output: &mut Vec<RustIdent>,
    ) {
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident) if template_idents.contains(ident) => {
                output.push(ident.clone())
            }
            ConceptualRustType::Array(element) | ConceptualRustType::Optional(element) => {
                Self::collect_generic_placeholders_in_type(element, template_idents, output);
            }
            ConceptualRustType::Map(domain, range) => {
                Self::collect_generic_placeholders_in_type(domain, template_idents, output);
                Self::collect_generic_placeholders_in_type(range, template_idents, output);
            }
            ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_)
            | ConceptualRustType::Rust(_)
            | ConceptualRustType::Alias(_, _)
            | ConceptualRustType::Any => {}
        }
    }

    // call this after all types have been registered
    /// Derive the dispatch major of every open table's TYPED row (`RustRecord::typed_row`), and
    /// reject — gracefully, never silently — every shape whose major is not statically knowable.
    ///
    /// The two-stage staticness rule (the naive `cbor_types().len() == 1` test is WRONG on its own):
    ///
    /// 1. if the key's alias chain carries a `@custom_serialize`/`@custom_deserialize` pair, the
    ///    codec OWNS the wire and `cbor_types()` answers about the REPLACED type — a codec over a
    ///    raw-bytes marker reports `Bytes` while writing text. There the `@custom_wire_major`
    ///    DECLARATION is required, and it is the answer;
    /// 2. otherwise `cbor_types()` must yield exactly one major. Primitives, primitive-bodied
    ///    aliases, raw-bytes markers, aliases of markers, `.size`-constrained bytes and tagged types
    ///    qualify; plain externs (`Array`+`Map`), the reserved `Int` extern, multi-major unions,
    ///    `any` and optionally-tagged types report more than one and reject naturally.
    ///
    /// Plus the complement check: a catch-all whose admissible majors are EXHAUSTED by the typed row
    /// can never see an entry, so it is rejected rather than emitted as dead code.
    ///
    /// Plus, under the JSON flags only, the BARE-TEXT typed key check. A `K_t` that transparently
    /// resolves to `String` admits EVERY JSON object member name, so the typed-first partition binds
    /// every member to the typed row: the catch-all is provably unreachable through JSON, and
    /// `from_json` refuses what `to_json` wrote for any captured entry (the member rebinds typed and
    /// `V_t` refuses the value, or worse, silently accepts it into the wrong row). That is the T1
    /// fixed point failing on EVERY document with a captured entry, not on an edge case, so the shape
    /// is refused rather than documented. TRANSPARENT resolution only: an opaque `K_t` (an extern or
    /// a `@newtype` whose hand-written serde happens to read every string) is undecidable from here
    /// and stays the documented hazard it already is.
    fn derive_open_table_dispatch_majors(
        &mut self,
        cli: &Cli,
        consumed: &mut BTreeSet<AliasIdent>,
    ) {
        // Two passes, because `cbor_types()` on a `Rust(ident)` key reads `rust_structs` — so the
        // derivation cannot hold a mutable borrow of it. Pass 1 is a pure read of the whole IR
        // producing one verdict per open table; pass 2 applies the verdicts.
        let mut derived: BTreeMap<RustIdent, CBORType> = BTreeMap::new();
        let mut rejections: Vec<String> = Vec::new();
        // The shared no-silent-directive ledger may already contain successful variable-middle
        // array boundaries; typed rows add their own consumers before its final check below.
        for (rule_ident, rust_struct) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rust_struct.variant() else {
                continue;
            };
            if !record.is_open_table() {
                continue;
            }
            let typed_domain = record.typed_row().unwrap().domain();
            let catch_all_domain = record.rest.as_ref().unwrap().domain();
            if (cli.json_serde_derives || cli.json_schema_export)
                && matches!(
                    typed_domain.conceptual_type.resolve_alias_shallow(),
                    ConceptualRustType::Primitive(Primitive::Str)
                )
            {
                rejections.push(format!(
                    "rule `{rule_ident}`: an open table keyed on bare `text` is a CBOR-ONLY shape \
                     and cannot be generated with a JSON face. In JSON both rows share one object \
                     and a member name binds the TYPED row first, so a `String` key — which admits \
                     every member name there is — leaves the catch-all row unreachable and makes \
                     `from_json` refuse documents `to_json` itself wrote. Either generate this spec \
                     without `--json-serde-derives`/`--json-schema-export`, or key the typed row on \
                     a type whose admissible names are a proper subset (a raw-bytes marker or a \
                     `@newtype` whose serde writes a fixed-width hex/bech32 image, or a numeric \
                     key), or spell it as a plain table (`t = {{ * text => v }}`) if one row is all \
                     you need."
                ));
                continue;
            }
            let has_custom_codec = custom_codec_on_alias_chain(&typed_domain.conceptual_type, self);
            let declared = declared_wire_major_on_alias_chain(&typed_domain.conceptual_type, self);
            let major = if has_custom_codec {
                match declared {
                    Some(major) => {
                        mark_wire_major_consumed(&typed_domain.conceptual_type, self, consumed);
                        wire_major_to_cbor_type(major)
                    }
                    None => {
                        rejections.push(format!(
                            "rule `{rule_ident}`: the open table's typed-row key is written by a \
                             `@custom_serialize`/`@custom_deserialize` pair, so the generator cannot \
                             infer which CBOR major type the wire starts with — the codec owns that \
                             wire, and the type it replaces answers about a wire nobody writes. \
                             Declare it beside the pair with `@custom_wire_major <major>` (one of \
                             `uint` / `nint` / `bytes` / `text` / `array` / `map` / `tag` / \
                             `simple`)."
                        ));
                        continue;
                    }
                }
            } else {
                if declared.is_some() {
                    mark_wire_major_consumed(&typed_domain.conceptual_type, self, consumed);
                }
                let majors = typed_domain.cbor_types(self);
                match majors.as_slice() {
                    [only] => *only,
                    _ => {
                        rejections.push(format!(
                            "rule `{rule_ident}`: the open table's typed row must be keyed on a type \
                             whose CBOR major type is statically known — it claims exactly that one \
                             major and the catch-all row sees the complement — but this key admits \
                             {} majors ({}). Use a single-major key (a primitive, an alias of one, a \
                             raw-bytes marker or an alias of one, a `.size`-constrained bytes, or a \
                             tagged type); a key whose wire a `@custom_serialize` / \
                             `@custom_deserialize` pair owns declares its major with \
                             `@custom_wire_major <major>` instead.",
                            majors.len(),
                            majors
                                .iter()
                                .map(|m| format!("{m:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                        continue;
                    }
                }
            };
            // The catch-all sees the COMPLEMENT of the typed row's major. If its key admits nothing
            // else, it can never capture an entry — dead code standing in for a table.
            let catch_all_majors = catch_all_domain.cbor_types(self);
            if catch_all_majors.iter().all(|m| *m == major) {
                rejections.push(format!(
                    "rule `{rule_ident}`: the open table's catch-all row can never capture an entry \
                     — every CBOR major type its key admits ({major:?}) is already claimed by the \
                     typed row. Widen the catch-all's key type, or spell this as a plain table (`t = \
                     {{ * k => v }}`) if one row is all you need."
                ));
                continue;
            }
            derived.insert(rule_ident.clone(), major);
        }
        for (rule_ident, major) in derived {
            if let Some(RustStructType::Record(record)) = self
                .rust_structs
                .get_mut(&rule_ident)
                .map(|rs| &mut rs.variant)
                && let Some(typed) = record.typed_row.as_mut()
            {
                typed.dispatch_major = Some(major);
            }
        }
        // no-silent-directive: a `@custom_wire_major` nobody consumed declares a fact about a wire
        // no boundary reads. Consumed SOMEWHERE is enough — one alias may key an open table's typed
        // row or prove a variable middle array boundary and also appear at an ordinary field.
        //
        // Only an AUTHORED declaration is checked. An entry that inherited its wire facts across a
        // registration strip (`wire_metadata_inherited_from`) carries a copy nobody wrote there, so
        // "nobody consumes it" says nothing about anyone's spec — the copy exists precisely so a
        // member declared through the re-alias reaches the right codec, and demanding that every
        // re-alias of a table key also key a table would refuse specs that are correct today. The
        // author's own rule is still checked, and a dispatch reading through an inheritor marks it
        // consumed (`mark_wire_major_consumed`).
        for (alias_ident, info) in self.type_aliases.iter() {
            if info.wire_metadata_inherited_from.is_some() {
                continue;
            }
            if info
                .rule_metadata
                .as_ref()
                .and_then(|rmd| rmd.custom_wire_major)
                .is_some()
                && !consumed.contains(alias_ident)
            {
                rejections.push(format!(
                    "`@custom_wire_major` on rule `{alias_ident}`: nothing consumes the declared \
                     major. It is read when this transparent alias keys an OPEN TABLE's typed row \
                     (`t = {{ * {alias_ident} => v, * k2 => v2 }}`), or participates in a generator-proven \
                     possible-next boundary of a variable ARRAY occurrence (`m = [prefix, * t, * {alias_ident}]`). Remove the \
                     directive, or use this rule at one of those boundaries."
                ));
            }
        }
        for msg in rejections {
            self.record_rejection(msg);
        }
    }

    /// The statically-known CBOR majors a variable middle array occurrence may use as its greedy
    /// boundary discriminator. Mandatory generator-owned framing wins before a custom codec can
    /// own the inner value; otherwise a complete custom pair on a transparent alias needs its
    /// declared major, and every other custom/opaque head remains unproven. This is deliberately
    /// distinct from optional-field lookahead, whose established proof remains generator-owned.
    pub(crate) fn effective_wire_majors(&self, ty: &RustType) -> Option<Vec<CBORType>> {
        match ty.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_)) => return Some(vec![CBORType::Tag]),
            Some(CBOREncodingOperation::CBORBytes) => return Some(vec![CBORType::Bytes]),
            // An optional generator-owned tag can expose either the tag or its inner built-in
            // head.  Preserve the full set so a greedy middle loop recognizes both wire forms.
            // A custom inner value remains unproven: declared custom heads deliberately do not
            // extend through this optional framing, whose absent arm would expose user-owned wire.
            Some(CBOREncodingOperation::OptionallyTagged(_)) => {
                let mut inner = ty.clone();
                inner.encodings.pop();
                return (!self.type_has_unproven_wire_head(&inner)).then(|| ty.cbor_types(self));
            }
            None => {}
        }
        if custom_codec_on_alias_chain(&ty.conceptual_type, self) {
            return declared_wire_major_on_alias_chain(&ty.conceptual_type, self)
                .map(wire_major_to_cbor_type)
                .map(|major| vec![major]);
        }
        if self.type_has_unproven_wire_head(ty) {
            None
        } else {
            Some(ty.cbor_types(self))
        }
    }

    /// The complete CDDL-value domain of a conservative fixed-value boundary item.  This is
    /// intentionally narrower than `cbor_types()`: it proves every value the generated decoder can
    /// accept, rather than merely the item's outer CBOR major.  It admits only untagged,
    /// field-codec-free fixed literals and named all-fixed choices/singletons through transparent
    /// aliases.  Floats stay out even when their source spellings differ, because `f64::PartialEq`
    /// is not a CDDL-semantic proof for NaN values.
    pub(crate) fn fixed_value_domain(&self, ty: &RustType) -> Option<Vec<FixedValue>> {
        self.fixed_value_domain_inner(ty, &mut BTreeSet::new(), &mut BTreeSet::new())
    }

    /// Whether a variable middle boundary needs the finite-domain retry strategy rather than the
    /// established major peek.  Keep this derivation pure and recomputable from finalized IR so it
    /// does not become a presentation-only field in debug/snapshot IR.
    pub(crate) fn has_disjoint_fixed_domain_middle_boundary(
        &self,
        repeated: &RustType,
        suffix: &RustType,
    ) -> bool {
        let Some(repeated_majors) = self.effective_wire_majors(repeated) else {
            return false;
        };
        let Some(suffix_majors) = self.effective_wire_majors(suffix) else {
            return false;
        };
        if !repeated_majors
            .iter()
            .any(|major| suffix_majors.contains(major))
        {
            return false;
        }
        self.fixed_value_domain(repeated)
            .as_ref()
            .zip(self.fixed_value_domain(suffix).as_ref())
            .is_some_and(|(repeated, suffix)| {
                repeated
                    .iter()
                    .all(|value| !suffix.iter().any(|other| other == value))
            })
    }

    fn fixed_value_domain_inner(
        &self,
        ty: &RustType,
        seen_structs: &mut BTreeSet<RustIdent>,
        seen_aliases: &mut BTreeSet<AliasIdent>,
    ) -> Option<Vec<FixedValue>> {
        // Bounds, collection policy, defaulting, or framing change the construction/wire story
        // from the plain fixed literals this proof deliberately owns.
        if !ty.encodings.is_empty()
            || ty.config.default.is_some()
            || ty.config.bounds.is_some()
            || ty.config.float_bounds.is_some()
            || ty.config.duplicates.is_some()
            || ty.config.basic_override
        {
            return None;
        }
        match &ty.conceptual_type {
            ConceptualRustType::Fixed(value) => {
                (!matches!(value, FixedValue::Float(_))).then(|| vec![value.clone()])
            }
            ConceptualRustType::Alias(alias, _inner) => {
                let info = self.type_aliases.get(alias)?;
                if info.carries_custom_pair()
                    || info.rule_metadata.as_ref().is_some_and(|metadata| {
                        metadata.custom_encodings.is_some() || metadata.custom_wire_major.is_some()
                    })
                {
                    return None;
                }
                // `AliasInfo::base_type`, not `inner`, is the transparent alias source of truth:
                // registration can carry bounds/encodings/configuration there which the conceptual
                // inner deliberately does not store.  A classifier that reconstructed a fresh
                // `RustType` from `inner` could falsely prove a configured alias fixed-domain.
                if !seen_aliases.insert(alias.clone()) {
                    return None;
                }
                let result =
                    self.fixed_value_domain_inner(&info.base_type, seen_structs, seen_aliases);
                seen_aliases.remove(alias);
                result
            }
            ConceptualRustType::Rust(ident) => {
                if !seen_structs.insert(ident.clone()) {
                    return None;
                }
                let result = (|| {
                    let rust_struct = self.rust_struct(ident)?;
                    if rust_struct.tag.is_some()
                        || rust_struct.tag_optional()
                        || rust_struct.config().custom_serialize.is_some()
                        || rust_struct.config().custom_deserialize.is_some()
                        || rust_struct.config().custom_encodings.is_some()
                        || rust_struct.config().custom_wire_major.is_some()
                    {
                        return None;
                    }
                    let variants = match rust_struct.variant() {
                        RustStructType::TypeChoice { variants }
                        | RustStructType::CStyleEnum { variants } => variants,
                        _ => return None,
                    };
                    let mut values = Vec::new();
                    for variant in variants {
                        if variant.serialize_as_embedded_group || variant.key.is_some() {
                            return None;
                        }
                        values.extend(self.fixed_value_domain_inner(
                            variant.rust_type(),
                            seen_structs,
                            seen_aliases,
                        )?);
                    }
                    (!values.is_empty()).then_some(values)
                })();
                seen_structs.remove(ident);
                result
            }
            ConceptualRustType::Primitive(_)
            | ConceptualRustType::Any
            | ConceptualRustType::Optional(_)
            | ConceptualRustType::Array(_)
            | ConceptualRustType::Map(_, _) => None,
        }
    }

    /// Whether the effective-major derivation for this bare middle-boundary item actually READS a
    /// transparent alias's declaration. Mandatory generated outer framing wins without consulting
    /// it, so such a declaration stays subject to the no-silent-directive check.
    fn middle_boundary_consumes_wire_major_declaration(&self, ty: &RustType) -> bool {
        ty.encodings.is_empty()
            && custom_codec_on_alias_chain(&ty.conceptual_type, self)
            && declared_wire_major_on_alias_chain(&ty.conceptual_type, self).is_some()
    }

    /// Whether `ty` can write a head the generator cannot prove. Mandatory tags and `.cbor` own
    /// a stable outer head; optionally-tagged values may still expose their inner head. The visited
    /// set makes recursive type-choice/wrapper graphs conservative rather than recursive forever:
    /// a custom/unproven owner is found on its first visit.
    pub(crate) fn type_has_unproven_wire_head(&self, ty: &RustType) -> bool {
        self.type_has_unproven_wire_head_inner(ty, &mut BTreeSet::new())
    }

    fn type_has_unproven_wire_head_inner(
        &self,
        ty: &RustType,
        seen: &mut BTreeSet<RustIdent>,
    ) -> bool {
        match ty.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_) | CBOREncodingOperation::CBORBytes) => {
                return false;
            }
            Some(CBOREncodingOperation::OptionallyTagged(_)) => {
                let mut inner = ty.clone();
                inner.encodings.pop();
                return self.type_has_unproven_wire_head_inner(&inner, seen);
            }
            None => {}
        }
        match &ty.conceptual_type {
            ConceptualRustType::Alias(alias_ident, inner) => {
                let codec_owned = self
                    .type_aliases()
                    .get(alias_ident)
                    .and_then(|info| info.rule_metadata.as_ref())
                    .is_some_and(|metadata| {
                        metadata.custom_serialize.is_some() || metadata.custom_deserialize.is_some()
                    });
                codec_owned
                    || self
                        .type_has_unproven_wire_head_inner(&RustType::new((**inner).clone()), seen)
            }
            ConceptualRustType::Optional(inner) => {
                self.type_has_unproven_wire_head_inner(inner, seen)
            }
            ConceptualRustType::Rust(ident) => {
                if !seen.insert(ident.clone()) {
                    return false;
                }
                // An opaque extern can reach this classifier directly (rather than through a
                // registered generated struct). Its head is intentionally unproven; report the
                // surrounding greedy-boundary rejection instead of aborting while looking it up.
                let Some(rust_struct) = self.rust_struct(ident) else {
                    return true;
                };
                let config = rust_struct.config();
                if config.custom_serialize.is_some() || config.custom_deserialize.is_some() {
                    return true;
                }
                if rust_struct.tag.is_some() && !rust_struct.tag_optional() {
                    return false;
                }
                match rust_struct.variant() {
                    RustStructType::Wrapper { wrapped, .. } => {
                        self.type_has_unproven_wire_head_inner(wrapped, seen)
                    }
                    RustStructType::TypeChoice { variants }
                    | RustStructType::CStyleEnum { variants } => variants.iter().any(|variant| {
                        matches!(
                            &variant.data,
                            EnumVariantData::RustType(variant_ty)
                                if self.type_has_unproven_wire_head_inner(
                                    variant_ty, seen,
                                )
                        )
                    }),
                    RustStructType::Extern if ident.to_string() != "Int" => true,
                    // These variants emit a stable outer CBOR head; their nested codecs cannot
                    // affect membership at this array position.
                    RustStructType::Record(_)
                    | RustStructType::Table { .. }
                    | RustStructType::Array { .. }
                    | RustStructType::GroupChoice { .. }
                    | RustStructType::Extern
                    | RustStructType::RawBytesType => false,
                }
            }
            // Fixed, primitive, any, array, and map concepts own a built-in outer head.
            ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_)
            | ConceptualRustType::Any
            | ConceptualRustType::Array(_)
            | ConceptualRustType::Map(_, _) => false,
        }
    }

    /// Validate every non-final array occurrence segment after aliases and generic products have
    /// settled. RFC 8610 repetition is greedy: a variable segment may stop only at the owner-array
    /// boundary or when every possible-next live member is major-disjoint or has a disjoint,
    /// generator-owned finite fixed-value domain.
    ///
    /// Parse records the segment's flattened source position; finalized fields retain theirs.  This
    /// pass is deliberately before every code-generation walk: `cbor_types()` and
    /// `expanded_field_count()` may inspect referenced structs, so the parser cannot soundly make
    /// this decision while forward references and generic instances are unresolved.
    fn validate_array_middle_occurrence_segments(&mut self) -> BTreeSet<AliasIdent> {
        enum PossibleNext<'a> {
            Segment(&'a RestRow),
            Field(&'a RustField),
            Forbidden,
        }

        let mut rejections = Vec::new();
        // A declaration becomes live only after its successful variable-middle boundary actually
        // reads the effective major. The open-table pass extends this same ledger before its one
        // no-silent-directive rejection runs.
        let mut consumed = BTreeSet::new();
        for (rule_ident, rust_struct) in &self.rust_structs {
            let RustStructType::Record(record) = rust_struct.variant() else {
                continue;
            };
            for segment in record.dynamic_rows().filter(|row| row.is_array_tail()) {
                let segment_index = segment
                    .array_source_index()
                    .expect("array occurrence segment has a source index");

                // An exact window owns its complete wire boundary by count.  This check must precede
                // the fixed-suffix lookup: adjacent exact segments are valid even though the next
                // authored member is another segment rather than a RustField.
                if segment.has_exact_occurrence_window() {
                    continue;
                }

                let source_rule = self
                    .source_rule_name(rule_ident)
                    .unwrap_or(rule_ident.as_ref());
                let mut later = record
                    .fields
                    .iter()
                    .filter(|field| field.source_index > segment_index)
                    .map(|field| (field.source_index, PossibleNext::Field(field)))
                    .chain(
                        record
                            .forbidden_fields
                            .iter()
                            .filter(|field| field.source_index > segment_index)
                            .map(|field| (field.source_index, PossibleNext::Forbidden)),
                    )
                    .chain(
                        record
                            .dynamic_rows()
                            .filter(|row| {
                                row.is_array_tail()
                                    && row
                                        .array_source_index()
                                        .is_some_and(|index| index > segment_index)
                            })
                            .map(|row| {
                                (
                                    row.array_source_index()
                                        .expect("array occurrence segment has a source index"),
                                    PossibleNext::Segment(row),
                                )
                            }),
                    )
                    .collect::<Vec<_>>();
                later.sort_by_key(|(index, _)| *index);
                if later.is_empty() {
                    continue;
                }

                let mut possible_next_majors = Vec::new();
                // The no-silent-directive ledger records only declarations this proof actually
                // reads. An optional fixed field is deliberately generator-owned-only: its
                // presence decoder cannot use an alias-level declared custom head, so it must
                // neither authorize that boundary nor consume the declaration.
                let mut boundary_types = Vec::new();
                let mut boundary_failure = None;
                for (_, next) in later {
                    match next {
                        PossibleNext::Segment(next) => {
                            let (minimum, maximum) = next
                                .occurrence
                                .map(crate::intermediate::RestOccurrenceWindow::raw)
                                .unwrap_or((0, u64::MAX));
                            if maximum == 0 {
                                continue;
                            }
                            let Some(majors) = self.effective_wire_majors(next.element()) else {
                                boundary_failure = Some(format!(
                                    "rule `{source_rule}`: the possible-next occurrence segment `{}` after the occurrence-bearing array member at position {} has a custom- or extern-owned, otherwise-unproven wire head. Greedy decoding must know every possible-next CBOR major before it can prove the boundary.",
                                    next.field_name,
                                    segment_index + 1,
                                ));
                                break;
                            };
                            possible_next_majors.extend(majors);
                            boundary_types.push((next.element(), true));
                            if minimum > 0 {
                                break;
                            }
                        }
                        PossibleNext::Field(next) => {
                            if next.rust_type.expanded_field_count(self) != Some(1) {
                                boundary_failure = Some(format!(
                                    "rule `{source_rule}`: the possible-next fixed field `{}` after the occurrence-bearing array member at position {} can splice multiple CBOR items. Frame the repeated part as its own array, move it final, or use a single-item major-disjoint boundary.",
                                    next.name,
                                    segment_index + 1,
                                ));
                                break;
                            }
                            // An optional field's own generated presence peek cannot consume an
                            // alias-level `@custom_wire_major`: only a generator-owned outer head
                            // is proof at this boundary. Mandatory fields retain the established
                            // effective-major proof, including a declared transparent custom head.
                            // A field-local pair has no transparent alias metadata channel in
                            // either case, so it remains a graceful refusal even when the replaced
                            // Rust type has a known major.
                            let majors = if next.optional {
                                (next.rule_metadata.custom_serialize.is_none()
                                    && next.rule_metadata.custom_deserialize.is_none()
                                    && !self.type_has_unproven_wire_head(&next.rust_type))
                                .then(|| next.rust_type.cbor_types(self))
                            } else {
                                if next.rule_metadata.custom_serialize.is_some()
                                    || next.rule_metadata.custom_deserialize.is_some()
                                {
                                    None
                                } else {
                                    self.effective_wire_majors(&next.rust_type)
                                }
                            };
                            let Some(majors) = majors else {
                                if next.optional
                                    && self.middle_boundary_consumes_wire_major_declaration(
                                        &next.rust_type,
                                    )
                                {
                                    rejections.push(format!(
                                        "`@custom_wire_major` at optional possible-next fixed field `{}` in rule `{source_rule}`: nothing consumes the declared major. Optional-field lookahead uses only generator-owned heads.",
                                        next.name,
                                    ));
                                }
                                boundary_failure = Some(format!(
                                    "rule `{source_rule}`: the possible-next fixed field `{}` after the occurrence-bearing array member at position {} has a custom- or extern-owned, otherwise-unproven wire head. Greedy decoding must know every possible-next CBOR major before it can prove the boundary.",
                                    next.name,
                                    segment_index + 1,
                                ));
                                break;
                            };
                            possible_next_majors.extend(majors);
                            boundary_types.push((&next.rust_type, !next.optional));
                            // An optional fixed field can be absent and expose a later member,
                            // so its head is one possible-next boundary, never the end of the
                            // walk. A mandatory fixed field is one required CBOR item and ends it.
                            if !next.optional {
                                break;
                            }
                        }
                        PossibleNext::Forbidden => {
                            boundary_failure = Some(format!(
                                "rule `{source_rule}`: an unsupported intervening array member follows the occurrence-bearing member at position {}. Frame the repeated part as its own array or move it final.",
                                segment_index + 1,
                            ));
                            break;
                        }
                    }
                }
                if let Some(rejection) = boundary_failure {
                    rejections.push(rejection);
                    continue;
                }
                // Zero-maximum segments contribute no wire head. If they were the only later
                // members, this variable segment is wire-final and needs no discriminator.
                if possible_next_majors.is_empty() {
                    continue;
                }
                let Some(repeated_majors) = self.effective_wire_majors(segment.element()) else {
                    rejections.push(format!(
                        "rule `{source_rule}`: the repeated element at occurrence-bearing array member position {} has a custom- or extern-owned, otherwise-unproven wire head. Greedy decoding must know every possible-next CBOR major before it can prove the boundary.",
                        segment_index + 1,
                    ));
                    continue;
                };
                let overlap = repeated_majors
                    .iter()
                    .filter(|major| possible_next_majors.contains(major))
                    .map(|major| format!("{major:?}"))
                    .collect::<Vec<_>>();
                if !overlap.is_empty()
                    && !boundary_types.iter().all(|(boundary, _)| {
                        self.has_disjoint_fixed_domain_middle_boundary(segment.element(), boundary)
                            || self.effective_wire_majors(boundary).is_some_and(|majors| {
                                !repeated_majors.iter().any(|major| majors.contains(major))
                            })
                    })
                {
                    rejections.push(format!(
                        "rule `{source_rule}`: the occurrence-bearing array member at position {} shares CBOR major type(s) {} with a possible-next live member. RFC 8610 repetition is greedy and does not backtrack, so the generator will not guess where the repeated part ends. Each same-major possible-next boundary is admitted only when BOTH sides have generator-owned, untagged finite fixed-value domains with no shared CDDL value; these boundaries do not prove that. Frame the repeated part as its own array, move it final, choose major-disjoint possible-next heads, or make every overlapping fixed-value domain disjoint.",
                        segment_index + 1,
                        overlap.join(", "),
                    ));
                    continue;
                }
                // Every possible-next boundary either proved major-disjoint or had disjoint finite
                // fixed-value domains. Mark every transparent alias declaration the proof actually
                // reads, including zero-skippable possible-next members; mandatory outer framing
                // remains generator-proven and inert.
                if self.middle_boundary_consumes_wire_major_declaration(segment.element()) {
                    mark_wire_major_consumed(
                        &segment.element().conceptual_type,
                        self,
                        &mut consumed,
                    );
                }
                for (boundary, may_consume_wire_major_declaration) in boundary_types {
                    if may_consume_wire_major_declaration
                        && self.middle_boundary_consumes_wire_major_declaration(boundary)
                    {
                        mark_wire_major_consumed(&boundary.conceptual_type, self, &mut consumed);
                    }
                }
            }
        }
        for rejection in rejections {
            self.record_rejection(rejection);
        }
        consumed
    }

    pub fn finalize(
        &mut self,
        parent_visitor: &ParentVisitor,
        cli: &Cli,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Surface any deferred rejections BEFORE any resolution runs, so nothing downstream
        // operates on the incomplete IR a skipped-field record leaves behind.
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        self.resolve_generic_instances(parent_visitor, cli)?;
        // Generic resolution registered each generic COLLECTION instance's transparent alias
        // (`xs_int = xs<uint>` → `pub type XsInt = Vec<u64>;`) only just now — AFTER every use-site
        // field was built at parse time. Re-resolve those fields so the generic path converges on
        // the SAME structural collection field type the non-generic path already has (see the method
        // doc); must run BEFORE the key-demand / encoding analysis below so they see the collection.
        // Classify each SYNTHESIZED anonymous collection instance for the wasm boundary FIRST (this
        // populates `anonymous_collection_instances` and flips `gen_wasm_alias` for the wrapper
        // subset), so the field re-resolution below can see the classification and lower the
        // directly-exposable subset onto the bare inline collection. Wasm-only; non-wasm untouched.
        self.converge_anonymous_collection_instance_wasm(cli);
        self.resolve_late_alias_product_leaves();
        // Array middle-occurrence safety depends on finalized aliases/generics and must reject
        // before any later IR walk or code emitter can build a non-round-tripping decoder.
        let mut wire_major_consumed = self.validate_array_middle_occurrence_segments();
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        // Mint the wasm keys-list wrappers whose owning table had a GENERIC-COLLECTION-instance
        // domain — deferred from `register_rust_struct` until now, so they name from the resolved
        // domain (see the deferral comment there). Idempotent by the not-yet-registered guard: a
        // non-generic table already minted its keys-list at parse and is skipped here.
        self.finalize_deferred_table_keys_lists(parent_visitor, cli);
        // Phase 2.4: nominalize INLINE `#6.258([* T])` occurrences into shape-derived `Set<Elem>`
        // wrappers, at the ONE post-collapse seam (over the finalized construction PRODUCTS, never in
        // `rust_type_from_type2`). Must run BEFORE the key-demand analysis below so the minted
        // wrappers' elements get the full comparison bundle via the set-nominal block there, and after
        // the generic resolution/re-resolution above so every registered product (incl. resolved
        // generic instances) is seen in final shape.
        self.nominalize_inline_sets(parent_visitor, cli)?;
        // Phase 2.5: derive each OPEN TABLE's typed-row dispatch major, and police the
        // `@custom_wire_major` declarations that feed it. Runs HERE — not in the parse walk — for the
        // same two reasons the float-key instruments below do: `cbor_types()` does
        // `rust_struct(ident).unwrap()` and would panic on a not-yet-registered forward reference,
        // and only a post-generic-resolution pass sees a key hidden behind a resolved generic
        // instance. Parse decides SHAPE, finalize decides STATICNESS.
        self.derive_open_table_dispatch_majors(cli, &mut wire_major_consumed);
        self.reject_unadapted_wide_static_array_json_shapes(cli);
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        self.propagate_key_demand();
        self.reject_generic_definition_directive_placements();
        self.reject_wasm_collisions_and_exposable_elements(cli);
        self.reject_component_scope_cycles(cli);
        self.reject_structless_rule_directives();
        self.reject_unhonored_custom_codecs();
        self.reject_custom_codecs_without_encoding_demand(cli);
        // The final post-construction floor is deliberately last: generic resolution, deferred
        // wrappers and inline-set nominalization all mutate the IR name surface. It validates the
        // names that actually survive those passes; `nominal_mint_claims` above separately retains
        // claims discarded before they could survive.
        for message in self.validate_emitted_name_surface() {
            self.record_rejection(message);
        }
        // Surface any rejection recorded DURING finalize (e.g. the float-key check above, which can
        // only run post-generic-resolution). Without this the entry-point check at the top of
        // finalize would silently swallow anything recorded here.
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        Ok(())
    }

    fn resolve_generic_instances(
        &mut self,
        parent_visitor: &ParentVisitor,
        cli: &Cli,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Resolve concrete generic instances in deterministic waves. A definition-owned child
        // application can register a new ordinary instance while its parent resolves, so a
        // one-shot snapshot would leave that child dangling. `BTreeSet` gives source-order-free
        // work selection and `completed` makes repeated compatible children a no-op.
        let mut pending_generics = self
            .generic_instances
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>();
        let mut completed_generics = BTreeSet::new();
        // Dedup guard for generic SET-NOMINAL instantiations: every spelling of `set<key_hash>`
        // resolves to the same `canonical_ident` (`SetKeyHash`), which must mint exactly ONE nominal
        // wrapper struct. A named binding whose own ident differs (`named_set` → `NamedSet`) then
        // aliases transparently to it.
        let mut minted_set_nominals: BTreeSet<RustIdent> = BTreeSet::new();
        while let Some(instance_ident) = pending_generics.pop_first() {
            if !completed_generics.insert(instance_ident.clone()) {
                continue;
            }
            let resolved_instance = self
                .generic_instances
                .get(&instance_ident)
                .expect("queued generic instance must remain registered")
                .resolve(self, cli)?;
            match resolved_instance {
                GenericResolved::Resolved {
                    mut resolved,
                    inline_type_choices,
                    child_instances,
                } => {
                    // Resolve child templates from their private dependency graph before deriving
                    // an identity. A nested `outer<inner<p>>` first resolves `inner<uint>`, then
                    // uses that ordinary concrete ident in `outer<…>`'s canonical fragment.
                    let mut child_replacements = BTreeMap::new();
                    drain_in_dependency_order(
                        child_instances,
                        |child| child.placeholder.clone(),
                        |child, pending_placeholders| {
                            !child.generic_args.iter().any(|arg| {
                                let mut dependencies = Vec::new();
                                Self::collect_generic_placeholders_in_type(
                                    arg,
                                    pending_placeholders,
                                    &mut dependencies,
                                );
                                !dependencies.is_empty()
                            })
                        },
                        |mut child| {
                            for arg in &mut child.generic_args {
                                GenericInstance::rewrite_deferred_placeholders_in_type(
                                    arg,
                                    &child_replacements,
                                );
                            }
                            let canonical_ident = RustIdent::new(
                                crate::parsing::generic_instance_canonical_cddl_ident(
                                    &CDDLIdent::new(child.generic_ident.to_string()),
                                    &child.generic_args,
                                ),
                            );
                            let registered_child =
                                self.register_generic_instance(GenericInstance::new(
                                    canonical_ident.clone(),
                                    child.generic_ident,
                                    child.generic_args,
                                    true,
                                    canonical_ident.clone(),
                                ));
                            if registered_child && !completed_generics.contains(&canonical_ident) {
                                pending_generics.insert(canonical_ident.clone());
                            }
                            child_replacements.insert(child.placeholder, canonical_ident);
                        },
                        "generic child-instance templates contain a cyclic placeholder dependency",
                    )?;
                    // Register each concrete anonymous choice before the generic root that refers
                    // to it.  This is the ordinary anonymous-choice ownership order, delayed only
                    // until the exact lexical bindings have concrete arguments.  The shared chooser
                    // preserves compatible reuse and deterministic incompatible siblings across
                    // distinct generic instances as well as within one definition.
                    let mut replacements = BTreeMap::new();
                    drain_in_dependency_order(
                        inline_type_choices,
                        |choice| choice.placeholder.clone(),
                        |choice, pending_placeholders| {
                            let mut dependencies = Vec::new();
                            Self::collect_generic_placeholders(
                                &choice.resolved,
                                pending_placeholders,
                                &mut dependencies,
                            );
                            dependencies.is_empty()
                        },
                        |mut inline_choice| {
                            GenericInstance::rewrite_inline_choice_placeholders(
                                &mut inline_choice.resolved,
                                &child_replacements,
                            );
                            // A parent template may name a child template. Materialize children first,
                            // then rewrite the parent before choosing its normal anonymous-owner name.
                            GenericInstance::rewrite_inline_choice_placeholders(
                                &mut inline_choice.resolved,
                                &replacements,
                            );
                            inline_choice.resolved.ident =
                                GenericInstance::anonymous_type_choice_base_ident(
                                    &inline_choice.resolved,
                                );
                            let base_ident = inline_choice.resolved.ident().clone();
                            let concrete_ident = self
                                .anonymous_type_choice_ident(&base_ident, &inline_choice.resolved);
                            inline_choice.resolved.ident = concrete_ident.clone();
                            self.register_rust_struct(parent_visitor, inline_choice.resolved, cli);
                            replacements.insert(inline_choice.placeholder, concrete_ident);
                        },
                        "generic inline type-choice templates contain a cyclic placeholder dependency",
                    )?;
                    GenericInstance::rewrite_inline_choice_placeholders(
                        &mut resolved,
                        &child_replacements,
                    );
                    GenericInstance::rewrite_inline_choice_placeholders(
                        &mut resolved,
                        &replacements,
                    );
                    self.register_rust_struct(parent_visitor, resolved, cli);
                }
                GenericResolved::SetNominal {
                    instance_ident,
                    canonical_ident,
                    resolved,
                } => {
                    if minted_set_nominals.insert(canonical_ident.clone()) {
                        self.register_rust_struct(parent_visitor, resolved, cli);
                    }
                    // A named binding (`named_set = set<key_hash>`) becomes a transparent alias TO the
                    // instantiation nominal: `pub type NamedSet = SetKeyHash;` (rust AND wasm — wasm
                    // keeps ONE class + this passthrough alias). An anonymous instance's ident already
                    // IS the canonical, so it needs no alias.
                    if instance_ident != canonical_ident {
                        // `@custom_json` on the BINDING has nothing to act on: the binding emits
                        // `pub type NamedSet = SetKeyHash;` and every derive it would suppress
                        // belongs to the nominal, whose config comes from the generic DEFINITION.
                        // The transparent-alias family's usual `@newtype` remedy does not apply —
                        // a set nominal already IS a wrapper and `@newtype` on the binding is an
                        // accepted no-op — so this shape carries its own message, naming the
                        // definition as the rule that owns the derives (probed: `@custom_json` on
                        // the generic set def drops the nominal's `Serialize`/`JsonSchema` impls).
                        if self
                            .rule_directives
                            .custom_json_rules
                            .contains(&instance_ident)
                        {
                            let source = self
                                .source_rule_name(&instance_ident)
                                .unwrap_or(instance_ident.as_ref())
                                .to_owned();
                            self.record_rejection(format!(
                                "@custom_json on `{source}`: this rule binds a generic set nominal, \
                                 so it emits a transparent `pub type {instance_ident} = \
                                 {canonical_ident};` and mints no type of its own — the \
                                 serde/schemars derives it would suppress are on `{canonical_ident}`, \
                                 whose config comes from the generic DEFINITION. Put `@custom_json` \
                                 on the definition instead (`<def><T> = #6.258([* T]) / [* T] ; \
                                 @custom_json`), and hand-write the impls for the nominal."
                            ));
                        }
                        self.register_type_alias(
                            instance_ident,
                            AliasInfo::rust_and_wasm(
                                ConceptualRustType::Rust(canonical_ident).into(),
                            ),
                        );
                    }
                }
                GenericResolved::Extern {
                    instance_ident,
                    real_ident,
                    flavored_base,
                } => {
                    // `@raw_bytes_flavor` selected the `<Base>RawBytes` wrapper for this instance;
                    // record the base so the extern re-export glue emits `pub use crate::<Base>RawBytes;`
                    // (in addition to the plain `pub use crate::<Base>;` the base extern carries).
                    if let Some(base) = flavored_base {
                        self.mark_raw_bytes_flavor_emitted(base);
                    }
                    // must be generic extern - register it so other lookups don't fail
                    self.register_rust_struct(
                        parent_visitor,
                        RustStruct::new_extern(instance_ident.clone()),
                        cli,
                    );
                    // we do direct rust alias replacing (gen_rust_alias=false) since no problems with generics in rust
                    // but wasm_bindgen can't work with it directly we assume the user will supply the correct mappings
                    self.register_type_alias(
                        instance_ident,
                        AliasInfo::rust_only(ConceptualRustType::Rust(real_ident).into()),
                    );
                }
            }
        }
        Ok(())
    }

    fn propagate_key_demand(&mut self) {
        // recursively check all types used as keys or contained within a type used as a key
        // this is so we only derive comparison or hash traits for those types. Demand is propagated as
        // SETS (`DemandSet`), union-merged: a tagged root spreads ITS flavor to every contained type;
        // an auto-detected internal map key spreads the mode-dependent `bare` internal bundle. Flavors
        // can only ADD to `bare`, never narrow it, so a type that is both is safe.
        let mut key_demand: BTreeMap<RustIdent, DemandSet> = BTreeMap::new();
        fn accumulate_key_demand(
            ty: &ConceptualRustType,
            key_demand: &mut BTreeMap<RustIdent, DemandSet>,
            demand: DemandSet,
        ) {
            if let ConceptualRustType::Rust(ident) = ty {
                let e = key_demand.entry(ident.clone()).or_default();
                *e = e.union(demand);
            }
        }
        // An auto-detected internal map key demands today's `bare` internal bundle (mode-dependent).
        let bare = DemandSet {
            bare: true,
            hash: false,
            ord: false,
        };
        // A `@duplicates reject` set's element type goes through the uniqueness twin's
        // `TryFrom<Vec<T>>` door, whose hybrid `scan_unique` (linear below a small-size threshold,
        // sorted-index above) is bounded `T: Ord` — so demand the `ord` flavor
        // (`Eq/PartialEq/Ord/PartialOrd`). (`accumulate_key_demand` marks `Rust(ident)` nodes only;
        // primitive/std elements carry `Ord` intrinsically EXCEPT floats, which are rejected
        // gracefully below like float map keys.)
        let ord = DemandSet {
            bare: false,
            hash: false,
            ord: true,
        };
        // A SET NOMINAL's element needs the FULL comparison bundle: the wrapper's always-on
        // `PartialEq/Eq/PartialOrd/Ord/Hash` derives flow through its `OrderedSet`/`Vec` inner onto
        // the element type. Matches the demand the wrapper forces on ITSELF (`wrappers.rs`).
        let full_set_demand = DemandSet {
            bare: true,
            hash: true,
            ord: true,
        };
        fn check_used_as_key(
            ty: &ConceptualRustType,
            types: &IntermediateTypes<'_>,
            key_demand: &mut BTreeMap<RustIdent, DemandSet>,
            bare: DemandSet,
        ) {
            if let ConceptualRustType::Map(k, _v) = ty {
                k.conceptual_type
                    .visit_types(types, &mut |ty| accumulate_key_demand(ty, key_demand, bare));
            }
        }
        // A map key that is (or recursively contains) a float compiles to a `BTreeMap<f64, _>` (or
        // an `OrderedHashMap` bounded `K: Hash + Eq + Ord` under --preserve-encodings); floats
        // implement none of Eq/Ord/Hash, so the emitted crate always fails to build (E0277). Such
        // rules are rejected gracefully at generation below. Collected into a local set (recorded
        // after the loop) to sidestep the borrow checker, like `used_as_key`, since the loop borrows
        // `self` immutably. `visit_types` guards recursion with a visited-ident set, so a
        // self-referential key type can't loop.
        let mut float_key_rejections = BTreeSet::new();
        fn key_contains_float(ty: &ConceptualRustType, types: &IntermediateTypes<'_>) -> bool {
            let mut found = false;
            ty.visit_types(types, &mut |t| {
                if matches!(
                    t,
                    ConceptualRustType::Primitive(p) if p.is_float()
                ) {
                    found = true;
                }
            });
            found
        }
        fn float_key_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: table key type contains a float (floats have no total order, so they cannot be map keys) — use an integer/text/bytes key domain instead"
            )
        }
        // The rest-row twin of `float_key_msg`: an open struct-map's captured entries live in the same
        // `BTreeMap`/`OrderedHashMap` a table's do, so a float key domain is the same E0277 — named
        // for the position so the remedy points at the rest row rather than at a table rule.
        fn float_rest_key_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: open struct-map rest-row key type contains a float (floats have no total order, so they cannot be map keys) — use an integer/text/bytes key domain instead"
            )
        }
        // The set-side twin of `float_key_msg`: a set's uniqueness door and (for a tag-258
        // nominal) always-on comparison derives need `Ord` on the element, which floats don't
        // have. A named tag-258 set stays a comparison-bearing wrapper even under `preserve`, so
        // that policy can never be offered as a float repair.
        fn float_set_elem_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: set element type contains a float (floats have no total order, so set elements cannot be compared for uniqueness) — use a non-float element type. A tag-258 set nominal always requires comparison derives, even with `@duplicates preserve`, so preserve cannot repair it; to keep float elements, rewrite that tag-258 set as a plain array (`foo = [* float64]`). A plain `@duplicates reject` array can instead drop that directive and use normal Vec semantics"
            )
        }
        // do a recursive check on the ones explicitly tagged as keys using @used_as_key: each tagged
        // root spreads its OWN flavor to every type it (transitively) contains. Iterating the roots map
        // (not the full `key_demand`, which finalize is about to expand) keeps the propagated flavor
        // exactly what the tag declared.
        for (ident, demand) in &self.key_demand_roots {
            if let Some(rust_struct) = self.rust_struct(ident) {
                let demand = *demand;
                rust_struct.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, demand)
                });
            }
        }
        // check all other places used as keys
        for rust_struct in self.rust_structs().values() {
            let rule_ident = rust_struct.ident().clone();
            rust_struct.visit_types(self, &mut |ty| {
                check_used_as_key(ty, self, &mut key_demand, bare);
                // A nested/inline map (`{ number => uint }` as an array element or map value)
                // surfaces as a Map conceptual type rather than a Table struct, so its float key is
                // rejected here — the Table branch below only sees top-level `x = { k => v }` rules.
                if let ConceptualRustType::Map(k, _v) = ty
                    && key_contains_float(&k.conceptual_type, self)
                {
                    float_key_rejections.insert(float_key_msg(&rule_ident));
                }
            });
            // A reject-mode set's element type gets the `ord` demand so the twin's uniqueness scan
            // compiles. The policy lives on the struct config (and its alias). A float element can
            // never satisfy that `Ord` bound, so it is rejected gracefully (the set-side analog of
            // the float-key rejection above) instead of emitting a non-compiling crate.
            if let RustStructType::Array { element_type, .. } = rust_struct.variant()
                && rust_struct.config().duplicates_reject()
            {
                element_type.conceptual_type.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, ord)
                });
                if key_contains_float(&element_type.conceptual_type, self) {
                    float_key_rejections.insert(float_set_elem_msg(&rule_ident));
                }
            }
            // A nominal wrapper over an ARRAY still routes its inner through the reject-set
            // uniqueness carrier when the rule selected `@duplicates reject`. The ordinary Array
            // branch above cannot see that element because this surface is a Wrapper (not a
            // transparent alias), notably the flat repeated-group carrier. Its element needs the
            // same `Ord` demand for `OrderedSet::try_from(Vec<_>)` to compile.
            if let RustStructType::Wrapper { wrapped, .. } = rust_struct.variant()
                && rust_struct.config().duplicates_reject()
                && !rust_struct.config().set_nominal
                && let ConceptualRustType::Array(element_type) = &wrapped.conceptual_type
            {
                element_type.conceptual_type.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, ord)
                });
                if key_contains_float(&element_type.conceptual_type, self) {
                    float_key_rejections.insert(float_set_elem_msg(&rule_ident));
                }
            }
            // A SET NOMINAL wrapper (Phase 2.2/2.3) derives always-on encodings-ignored
            // `PartialEq/Eq/PartialOrd/Ord/Hash`, and its inner collection (`OrderedSet<Elem>` under
            // reject, `Vec<Elem>` under preserve) propagates every one of those bounds onto `Elem`.
            // So the element needs the FULL demand (`bare + hash + ord`), regardless of policy —
            // otherwise a Rust-struct element (`set<key_hash>`) fails to satisfy `Eq/Ord/Hash` and the
            // crate does not compile. Primitive/std elements carry the bounds intrinsically (no-op).
            if let RustStructType::Wrapper { wrapped, .. } = rust_struct.variant()
                && rust_struct.config().set_nominal
                && let ConceptualRustType::Array(element_type) = &wrapped.conceptual_type
            {
                element_type.conceptual_type.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, full_set_demand)
                });
                // The wrapper's always-on `Ord`/`Hash` derives (and, under reject, the uniqueness
                // door's `T: Ord`) flow onto the element regardless of policy, so a float element
                // can never compile — reject gracefully like the reject-array branch above.
                if key_contains_float(&element_type.conceptual_type, self) {
                    float_key_rejections.insert(float_set_elem_msg(&rule_ident));
                }
            }
            // An open struct-map's CAPTURED rest row (`{ 1: uint, * K => V }`) keys the very same
            // container a table rule does, but the IR stores its `K` FLAT (`RestKind::MapEntries`),
            // never as a `Map(k, v)` node — so neither `check_used_as_key` above nor the `Table`
            // branch below ever sees it, and without this branch a typed `K` reaches
            // `BTreeMap<K, V>`/`OrderedHashMap<K, V>` with no `Eq`/`Ord`/`Hash` derives (E0277 in the
            // generated crate, and — for a dep-owned `K` — no `borrowed_key_types.rs` row for the
            // dependency to satisfy either, since that file is built from this same map).
            //
            // Gated on the TYPED path: a bare `uint`/`text`/`any` domain keys nothing that could be
            // marked (`accumulate_key_demand` marks `Rust(ident)` nodes only), so gating keeps every
            // existing spec's derives byte-identical rather than relying on that coincidence.
            // BOTH dynamic rows: an open table's TYPED row keys the same container kind, so a walk
            // that reads only the catch-all leaves `K_t` without its comparison derives (E0277 in the
            // generated crate, and no `borrowed_key_types.rs` row for a dep-owned `K_t`).
            for rest in match rust_struct.variant() {
                RustStructType::Record(record) => Some(record),
                _ => None,
            }
            .into_iter()
            .flat_map(|record| record.captured_dynamic_rows())
            .filter(|rest| !rest.is_array_tail() && !rest.map_key_uses_peeked_path(self))
            {
                // Same relaxation as the `Table` branch: a `@duplicates preserve` row's keys live in
                // a `PairMap`, compared by a linear `PartialEq` scan rather than hashed/ordered, so
                // the `ord` (Eq-containing) flavor suffices where the loose container needs `bare`.
                let key_flavor = if rest.duplicates() == Some(DuplicatesPolicy::Preserve) {
                    ord
                } else {
                    bare
                };
                rest.domain().conceptual_type.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, key_flavor)
                });
                // Walked directly (not as a `Map` node), so the float check is this branch's own —
                // and, running after generic resolution, it also catches a float behind a resolved
                // generic instance (`* gen<float64> => v`).
                if key_contains_float(&rest.domain().conceptual_type, self) {
                    float_key_rejections.insert(float_rest_key_msg(&rule_ident));
                }
            }
            if let RustStructType::Table { domain, .. } = rust_struct.variant() {
                // A `@duplicates preserve` table's key is compared with the pair-map's linear
                // `contains`/`find` scan (`K: PartialEq`), NOT hashed or ordered like a `BTreeMap`/
                // `OrderedHashMap` key — so it needs only the `ord` (Eq-containing) flavor, not the
                // full `bare` (`Hash + Eq + Ord`) bundle the loose table forces on its key. This is
                // the map-side of the reject-set `ord` relaxation above.
                let key_flavor = if rust_struct.config().duplicates_preserve() {
                    ord
                } else {
                    bare
                };
                domain.conceptual_type.visit_types(self, &mut |ty| {
                    accumulate_key_demand(ty, &mut key_demand, key_flavor)
                });
                // A top-level table rule's key is its `domain`, walked directly (not as a Map node),
                // so it needs its own check. This runs AFTER generic resolution, so it also catches
                // float keys hidden behind a resolved generic instance (`{ gen<float64> => uint }`),
                // the one seam that sees such instances. The marking above is left intact (harmless —
                // the crate never generates once we reject) so this is a pure add-on.
                if key_contains_float(&domain.conceptual_type, self) {
                    float_key_rejections.insert(float_key_msg(&rule_ident));
                }
            }
        }
        // we use a separate one here to get around the borrow checker in the above visit_types
        for (ident, demand) in key_demand {
            self.union_key_demand(ident, demand);
        }
        for msg in float_key_rejections {
            self.record_rejection(msg);
        }
    }

    fn reject_generic_definition_directive_placements(&mut self) {
        // `@used_as_key` / `@used_as_elem` ask for a wasm surface keyed on the rule's OWN type, and a
        // generic DEFINITION has none — only its instantiations name concrete types. `@used_as_key`
        // was dropped silently (the demand-propagation walk skips a root with no `rust_structs`
        // entry), and `@used_as_elem` was worse: the exposable-element check below resolves the
        // marked ident's element type, whose `directly_wasm_exposable` walk asserts that a
        // non-struct ident is a generic INSTANCE — so a marked generic DEF aborted the run at exit
        // 101 with an `assertion failed` and no diagnosis. Both refuse here, in the house style,
        // naming the instantiating rule as the placement that works.
        //
        // Placed before the `cli.wasm` block (which owns the abort site) and flag-independently,
        // like every sibling placement rejection: whether a directive may sit somewhere is a
        // property of the spec, not of the build profile. Keyed on `generic_defs` rather than on
        // "absent from `rust_structs`" so it covers every generic-def body spelling at once — the
        // record body the parse walk marks from, and the tag-set idiom the choice path marks from —
        // and refuses nothing else. Determinism: `BTreeSet`/`BTreeMap` iteration.
        // Named by their CDDL SOURCE spelling: a generic definition mints no rust type, and the
        // remedy is CDDL the author writes back into the spec.
        let generic_def_source = |ident: &RustIdent| {
            self.source_rule_name(ident)
                .unwrap_or(ident.as_ref())
                .to_owned()
        };
        let generic_def_elem = self
            .rule_directives
            .used_as_elem
            .iter()
            .filter(|ident| self.generic_defs.contains_key(*ident))
            .map(generic_def_source)
            .collect::<Vec<_>>();
        let generic_def_key = self
            .key_demand_roots
            .keys()
            .filter(|ident| self.generic_defs.contains_key(*ident))
            .map(generic_def_source)
            .collect::<Vec<_>>();
        for ident in generic_def_key {
            self.record_rejection(format!(
                "@used_as_key on `{ident}`: a generic DEFINITION names no concrete type — only its \
                 instantiations do — so there is no type for the map-key comparison derives to be \
                 demanded on, and the demand is dropped. Put the directive on the instantiating \
                 rule instead (`inst = {ident}<uint> ; @used_as_key`), which is where the concrete \
                 type is minted."
            ));
        }
        for ident in generic_def_elem {
            self.record_rejection(format!(
                "@used_as_elem on `{ident}`: a generic DEFINITION names no concrete type — only its \
                 instantiations do — so there is no element type for a loose-list wrapper to hold. \
                 Put the directive on the instantiating rule instead (`inst = {ident}<uint> ; \
                 @used_as_elem`), which is where the concrete type is minted."
            ));
        }
    }

    fn reject_wasm_collisions_and_exposable_elements(&mut self, cli: &Cli) {
        // NonEmptyVec wasm-wrapper name collisions: an inline `[+ elem]` mints a `NonEmpty<Elem>List`
        // wasm class; if a user rule already OWNS that identifier, silently sharing it would emit a
        // wrapper of the wrong shape (loose `Vec` vs restricted `NonEmptyVec`). Reject clearly rather
        // than shadow. Only relevant with wasm bindings (the collision is on the wasm class name).
        if cli.wasm {
            for msg in self.non_empty_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_array_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_reject_ordered_set_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // NonEmptyMap wasm-wrapper name collisions — the map-side twin of the above.
            for msg in self.non_empty_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // Finite/exact/lower-bounded unique-key table wrapper names are their own family:
            // `MapKToVMinN/MaxN` cannot share the NonEmptyMap detector because their checked door
            // and structural identity include both occurrence endpoints.
            for msg in self.bounded_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_pair_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // Keep `@duplicates reject` uniqueness-twin wasm-wrapper collision detection as a
            // per-kind sibling: its diagnostic differs from the other containers' messages.
            // See docs/development/decisions.md, "WASM wrapper-name collisions".
            for msg in self.reject_ordered_set_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // `@duplicates preserve` pair-map wrapper-name collisions — the fourth container kind's
            // siblings (loose `PairMapKToV`, restricted `NonEmptyPairMapKToV`). The flavored
            // structural names make the preserve-vs-default SHAPE collision unrepresentable, so what
            // is left is the same rule-ident-vs-wrapper-ident hazard the other kinds guard.
            for msg in self.preserve_pair_map_loose_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.preserve_pair_map_non_empty_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // The OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`) is the fifth container kind, and
            // it is the one that gets NO sibling of its own — recorded here so the standing ruling
            // reads as satisfied rather than skipped. Two independent reasons, both structural:
            //   * its minted struct is named by the RULE IDENT, which is the author's own name by
            //     construction. The four siblings above each guard a name the generator DERIVES
            //     (`NonEmpty<Elem>List`, `MapKToV`, `<Elem>OrderedSet`, `PairMapKToV`) against a
            //     rule that shadows it; an open table synthesizes no such name because the shape is
            //     a NAMED-RULE concession (an inline anonymous open table is refused at
            //     recognition, naming the named-rule form). If that concession is ever lifted, the
            //     synthesized name arrives with it and so does the fifth sibling.
            //   * its TYPED row mints no container class at all — the map surface is flattened onto
            //     the struct's own class — so the `MapKToV`/`PairMapKToV` hazard is unrepresentable
            //     for it, the same move that retired the family's wrapper-vs-wrapper detector.
            // What the open table DOES claim is covered by legs on the detectors above: the
            // `<K_t>List` its flattened `keys()` returns, and the catch-all row's own map class in
            // whichever flavor the row carries.
            //
            // What flattening DOES create is a MEMBER-name hazard on one class rather than a class-
            // name one, which is why it is checked here and not in that family: the accessors the
            // typed row contributes and the getter the catch-all contributes land on the SAME wasm
            // impl, so a `@name`d catch-all spelling one of the five reserved accessor names would
            // emit two methods of one name (rustc E0592 in the wasm crate).
            for msg in self.open_table_flattened_accessor_name_collisions() {
                self.record_rejection(msg);
            }
            // `@extern_companions` names classes this crate must NOT define, so a same-crate RULE of
            // one of those names is a contradiction the deferral cannot resolve: the `use
            // <prefix>::<Class>;` and the rule's own class would claim one identifier (rustc E0255).
            // Sibling in spirit to the four wrapper-name detectors above — a rule ident contending
            // with a name the generator routes elsewhere — but its own function because the contested
            // name comes from the SPEC's declaration rather than a structural derivation, so it needs
            // neither shape reconstruction nor a per-container-kind twin.
            for msg in self.extern_companion_rule_name_collisions() {
                self.record_rejection(msg);
            }
            // `@used_as_elem` mints the loose-list wasm wrapper `<Elem>List` for each tagged
            // element. A directly-wasm-exposable element (e.g. a transparent `coin = uint` alias)
            // has NO such wrapper — the list lowers to a bare `Vec<..>` at the wasm boundary — so
            // the tag has nothing to mint. Reject gracefully here (mirroring the `--wrapper-requests`
            // exposable diagnostic) rather than silently no-op. Collected into a local set to
            // sidestep the borrow checker, like the float-key rejections above.
            let mut exposable_elem_rejections = BTreeSet::new();
            for ident in &self.rule_directives.used_as_elem {
                // A generic DEFINITION is refused earlier in this fn (it names no concrete type),
                // and the resolution below cannot survive one: its exposability walk asserts that a
                // non-struct ident is a generic INSTANCE, which a definition is not. Skipping keeps
                // that assert an unreachable re-earning guard instead of the abort it used to be.
                if self.generic_defs.contains_key(ident) {
                    continue;
                }
                let element_type = self.used_as_elem_element_type(ident);
                if ConceptualRustType::Array(Box::new(element_type.conceptual_type.clone().into()))
                    .directly_wasm_exposable_ct(self)
                {
                    let member = element_type.name_as_wasm_array(self);
                    exposable_elem_rejections.insert(format!(
                        "@used_as_elem on `{ident}`: the loose list `[* {ident}]` is directly \
                         wasm-exposable — it lowers to `{member}` with no wrapper class, so there \
                         is no wrapper for this tag to mint. Remove `@used_as_elem` (the element \
                         already crosses the wasm boundary as a bare `{member}`)."
                    ));
                }
            }
            for msg in exposable_elem_rejections {
                self.record_rejection(msg);
            }
        }
    }

    fn reject_component_scope_cycles(&mut self, cli: &Cli) {
        // The component face's own detector family, on exactly the terms the wasm block above
        // states: a name that is legal on the rust and wasm faces can be broken on the WIT one, so
        // the check is flag-gated on the face that has the restriction.
        //
        // Placed HERE — after every `register_rust_struct` in this fn (they all run in the generic
        // resolution at the top) — because the detector walks `rust_structs` and `scopes`, which are
        // complete from that point on.
        //
        // Its SIBLING — the strong-uniqueness name-collision detector — deliberately does NOT run
        // here: its verdict depends on which types the rust face gives a `Deserialize` impl (a
        // `from-cbor-bytes` static the tool never emits cannot collide with anything), which only
        // that face's own walk reaches. It runs in `GenerationScope::generate` instead and surfaces
        // through the graceful error channel `generated_files`/`export` already carry. A spec with
        // BOTH a cycle and a collision therefore reports the cycle first, which is correct: a cyclic
        // package has no resolvable WIT to have collisions in.
        if cli.component {
            // WIT requires interfaces linked with `use` to be acyclic, and each exported module
            // scope becomes one interface. Cyclic cross-scope references generate fine on the rust
            // face, so this restriction arrives with `--component` and nowhere else.
            for msg in crate::generation::wit::wit_scope_cycles(self) {
                self.record_rejection(msg);
            }
        }
    }

    fn reject_structless_rule_directives(&mut self) {
        // `@no_json_schema_export` suppresses a rule's schema-registration row. A rule that registers
        // NO `RustStruct` at all — a transparent alias (`x = uint`), a `@no_alias` alias, a named
        // binding to a set nominal, a generic DEFINITION (only its instantiations are types), a
        // plain group no rule splices — has no row for the directive to
        // suppress, so it would be silently dead: reject it in the house style of the other
        // directive-misplacement rejections. Deliberately NOT rejected on a rule that registers a
        // struct the row loop skips for other reasons (an `Array`/`Table` typedef, a generic-extern
        // base): those are redundant-but-honest annotations, and keeping the rule "valid wherever a
        // rust type is produced" keeps it simple and flag-independent. Deferred to here rather than
        // the parse walk because a generic INSTANCE (`my_foo = foo<uint>`) only registers its struct
        // during the generic resolution above. Flag-independent (outside the `cli.wasm` block above):
        // the directive means the same thing under every flag set. Determinism: `BTreeSet` iteration.
        let struct_less_no_json_schema_export = self
            .rule_directives
            .no_json_schema_export
            .iter()
            .filter(|ident| !self.rust_structs.contains_key(ident))
            .cloned()
            .collect::<Vec<_>>();
        for ident in struct_less_no_json_schema_export {
            self.record_rejection(format!(
                "@no_json_schema_export on `{ident}`: this rule registers no rust struct, so there \
                 is no schema-registration row to suppress and the directive would silently do \
                 nothing. Either it is a transparent alias (a plain type alias `{ident} = uint`, a \
                 `@no_alias` alias, or a named binding to a generic instantiation), or it is a \
                 generic DEFINITION whose instantiations own the types (annotate the instance — \
                 `inst = {ident}<uint> ; @no_json_schema_export` — not the definition), or it is a \
                 plain group no rule splices. Remove it from this rule, or move it to the rule that \
                 actually produces the type."
            ));
        }
        // A plain GROUP rule becomes a rust type only by being SPLICED into a rule that materializes
        // it (`holder = [foo]`); a group nothing splices emits no struct and no fields, so every
        // rule-position directive written on it is inert — under the rule reading AND under the
        // field reading of the slot cddl binds it to. One uniform refusal covers the whole
        // vocabulary rather than thirteen per-directive sites, because the reason is the same for
        // all of them and does not depend on which directive it is.
        //
        // Deferred to here for the reason `@no_json_schema_export` above is: splicedness is a
        // whole-spec property, decided by rules the parse seam that reads the directives has not
        // reached yet. Two directives are excluded from the list — `@name`, which gets its own
        // long-standing message just below (one misplacement, one wording), and
        // `@no_json_schema_export`, whose refusal right above already names this exact shape.
        // `@rust_name` is excluded because a NON-exported (extern-deps) scope honors it there;
        // in an exported scope the parse walk has already refused it and finalize never runs.
        // Determinism: `BTreeMap` iteration, and each directive list is sorted at its source.
        // Named by its CDDL SOURCE spelling throughout, not its `RustIdent`: an unspliced group
        // materializes no rust type, so there is no rust name to report, and every remedy below is
        // CDDL the author writes back into the spec.
        let unspliced_annotated_groups = self
            .rule_directives
            .plain_group_rule_directives
            .iter()
            .filter(|(ident, _)| !self.rust_structs.contains_key(*ident))
            .map(|(ident, directives)| {
                (
                    self.source_rule_name(ident)
                        .unwrap_or(ident.as_ref())
                        .to_owned(),
                    directives.clone(),
                )
            })
            .collect::<Vec<_>>();
        for (ident, directives) in unspliced_annotated_groups {
            if directives.contains(&"@name") {
                self.record_rejection(crate::parsing::rule_position_name_message(&ident));
            }
            let remaining = directives
                .iter()
                .copied()
                .filter(|directive| {
                    !matches!(
                        *directive,
                        "@name" | "@no_json_schema_export" | "@rust_name"
                    )
                })
                .collect::<Vec<_>>();
            if !remaining.is_empty() {
                self.record_rejection(format!(
                    "{} on `{ident}`: the plain group `{ident}` is never spliced into any rule, so \
                     it materializes no rust type and no fields — a rule-position directive on it \
                     has nothing to act on and would be silently dropped. Splice the group into a \
                     rule that materializes it (`holder = [{ident}]` for an array shape, \
                     `holder = {{{ident}}}` for a map shape), which is where a rule-position \
                     directive on a group is read, or remove the directive.",
                    remaining.join(" / ")
                ));
            }
        }
    }

    fn reject_unhonored_custom_codecs(&mut self) {
        // The `@custom_serialize`/`@custom_deserialize` pair is a TYPE-level override: it replaces
        // the codec of the rust type a rule resolves to. The parse-walk rejections cover the
        // placements that DELETE or BYPASS the node it keys on (`@no_alias`, `@newtype`, an extern /
        // raw-bytes marker, a row-entry slot). The struct-kind checks below cover the remaining
        // placements that cannot honor the pair, while preserving the audited complete-pair owners:
        //
        //   - an ENUM rule (type choice, group choice, or the fixed-value C-style enum): its
        //     serialize side is generated unconditionally while `generate_deserialize`'s
        //     `Root(Rust(ident))` arm rewrites every embed site to the named reader — the same
        //     read-one-format/write-another asymmetry `@newtype` is rejected for.
        //   - a RECORD rule carrying only ONE half. Serialize-only emits no `Serialize` impl and
        //     never calls the named function (an undiagnosed non-compiling crate);
        //     deserialize-only keeps the type's own generated `Deserialize` impl while rewriting
        //     every embed site, so one type decodes the same bytes two ways — and the rule projects
        //     OPAQUELY across the extern-interface seam, carrying the divergence to consumers.
        //     BOTH halves on a record rule is deliberately NOT rejected: it suppresses the generated
        //     impls for the author to hand-own, which is unspecified-and-at-risk rather than wrong
        //     (see `docs/docs/comment_dsl.mdx`).
        //   - a TABLE rule (`t = { * k => v }`) carrying a LONE half. A complete pair takes the
        //     separately audited implicit map-wrapper owner; only a table left as `Table` lowers
        //     through `AliasInfo::new_manual`, whose `rule_metadata` is hardcoded `None`, so its
        //     remaining half is unhonored and rejects.
        //
        // Deferred to here rather than the parse walk for the same reason `@no_json_schema_export`
        // above is: the struct KIND decides, and a generic instance only materializes its struct
        // during the resolution above. Collected into a `BTreeSet` (determinism + no duplicate line
        // if two registrations ever land on one ident), like the float-key rejections.
        let mut custom_codec_rejections = BTreeSet::new();
        // The COLLECTION-RULE flavors of the transparent-alias `@custom_json` refusal
        // (`register_type_alias` owns the rest of that family). A named table or array rule DOES
        // register a `RustStruct`, so its config carries the flag — but the struct only exists to
        // drive the wasm wrapper and the keys-list mint; the rust rule itself lowers to a
        // transparent `pub type` alias (registered through `AliasInfo::new_manual`, which drops the
        // metadata, so `register_type_alias` cannot see these two). No consumer of `custom_json`
        // reads either shape, on either the rust or the wasm side — same inert class, same message,
        // same `@newtype` remedy. Collected here and recorded once the `&self` borrow ends.
        let mut custom_json_alias_rejections: BTreeSet<RustIdent> = BTreeSet::new();
        for (ident, rust_struct) in &self.rust_structs {
            let config = rust_struct.config();
            if config.custom_json
                && matches!(
                    rust_struct.variant(),
                    RustStructType::Table { .. } | RustStructType::Array { .. }
                )
            {
                custom_json_alias_rejections.insert(ident.clone());
            }
            let enum_shape = match rust_struct.variant() {
                RustStructType::TypeChoice { .. } => Some("a type-choice rule (`a / b`)"),
                RustStructType::GroupChoice { .. } => {
                    Some("a group-choice rule (`{ … } // { … }`)")
                }
                RustStructType::CStyleEnum { .. } => {
                    Some("a fixed-value type-choice rule (`0 / 1`, a C-style enum)")
                }
                _ => None,
            };
            if let Some(shape) = enum_shape {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: {shape} mints an enum whose serialize side is \
                             generated unconditionally, while the deserialize CALL SITES do route \
                             through the custom reader — so the pair would make the enum read one \
                             wire format and write another. Put the pair on the rule of the variant \
                             type that needs the custom format, or declare `{ident}` as a \
                             {EXTERN_MARKER} rule and hand-write the type in full."
                        ));
                    }
                }
            }
            if matches!(rust_struct.variant(), RustStructType::Table { .. }) {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: this table has only one custom-codec half; a \
                             complete pair would self-nominalize as the supported whole-table owner, \
                             but a lone half remains a transparent map alias with no codec to \
                             override and is dropped rather than honored. Put it on the rule that defines the table's \
                             KEY or VALUE type (`k = bytes ; {directive} …`, then `{ident} = \
                             {{ * k => v }}`), or declare `{ident}` as a {EXTERN_MARKER} rule and \
                             hand-write the type in full."
                        ));
                    }
                }
            }
            // The ARRAY sibling of the table rule above, and unhonored for the same reason: a named
            // collection rule (`items = [* uint]`, `[+ uint]`, `[3*5 uint]`, and both `@duplicates`
            // flavors) lowers to a transparent collection TYPEDEF registered through
            // `AliasInfo::new_manual`, whose `rule_metadata` is hardcoded `None` — so the pair
            // reaches neither the collection's standalone codec nor a holder's field call sites, and
            // ANY presence rejects rather than only a lone half. Keyed on the `Array` struct variant,
            // which is exactly the family that lowers this way (a `[a: uint]` RECORD body mints
            // `Record` and is handled below); the flavors differ only in the container the typedef
            // names (`Vec` / `NonEmptyVec` / `OrderedSet`), never in the metadata drop.
            if matches!(rust_struct.variant(), RustStructType::Array { .. }) {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: a named collection rule (`{ident} = [* t]`) \
                             lowers to a transparent collection typedef that owns no codec for the \
                             directive to override, so it is dropped rather than honored — in both \
                             directions, whichever half is written. Put it on the rule that defines \
                             the collection's ELEMENT type (`t = bytes ; {directive} …`, then \
                             `{ident} = [* t]`), or declare `{ident}` as a {EXTERN_MARKER} rule and \
                             hand-write the type in full to own the whole collection's wire."
                        ));
                    }
                }
            }
            // A TAGGED wrapper — a tag-head rule (`x = #6.42(uint)`), and the tag-258 set idiom,
            // which nominalizes into one — is outside the one wrapper contract B3-026 audited: an
            // implicit, untagged homogeneous-table map owner with a COMPLETE pair. Do not infer the
            // semantics of tag framing, set policy, encoding preservation, or cross-face projections
            // from that narrow owner; reject either half here. Rejected on tag presence rather than
            // on `Wrapper` at large so range-bounded wrappers remain an explicit unexpanded surface.
            // `@newtype` wrappers never reach here — their parse-walk rejection short-circuits
            // `finalize` — so one misplacement still reports once.
            if let RustStructType::Wrapper { wrapped, .. } = rust_struct.variant()
                && (rust_struct.tag().is_some()
                    || wrapped
                        .encodings
                        .iter()
                        .any(|op| matches!(op, CBOREncodingOperation::Tagged(_))))
            {
                let shape = if config.set_nominal {
                    "the tag-258 set idiom, which nominalizes into a set wrapper,"
                } else {
                    "a tag-head rule (`#6.n(…)`)"
                };
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: {shape} is a tagged wrapper, while this \
                             delivery supports and audits a complete pair only on the implicit \
                             homogeneous-table map owner. Its custom-codec contract (tag framing, \
                             encoding preservation, and cross-face behavior) is not defined here. \
                             Declare `{ident}` \
                             as a {EXTERN_MARKER} rule and hand-write the type in full, or give the \
                             rule a body that resolves to a transparent alias and write the wire \
                             framing in your own codec (`{ident} = <inner> ; @custom_serialize \
                             <fn> @custom_deserialize <fn>`)."
                        ));
                    }
                }
            }
            // BOTH halves on a record rule, and the complete pair's implicit whole-table map
            // wrapper, are the accepted rule-position pairs (each gets thin generated impls
            // delegating to the named functions). They are the only struct owners where a
            // `@custom_encodings` declaration would be read into rule metadata and then have
            // nowhere to go: a struct carries its encoding metadata INSIDE itself, so no
            // codec-visible tuple crosses the boundary. Other wrapper forms remain rejected by the
            // pair checks above; this fires once and only for an accepted owner that would otherwise
            // drop the declaration silently. (A declaration with one half or none is the parse
            // walk's `reject_custom_encodings_without_pair`, so it cannot double-report here.)
            // Only parsing's complete homogeneous-table path creates an untagged, non-`@newtype`
            // map wrapper. Explicit/newtype and tagged map wrappers are rejected elsewhere and must
            // not be treated as this accepted owner merely because they wrap a map.
            let is_complete_pair_map_wrapper = matches!(
                rust_struct.variant(),
                RustStructType::Wrapper {
                    wrapped,
                    ..
                } if matches!(wrapped.conceptual_type, ConceptualRustType::Map(_, _))
                    && rust_struct.tag().is_none()
                    && config.newtype_getter.is_none()
                    && !wrapped
                        .encodings
                        .iter()
                        .any(|op| {
                            matches!(
                                op,
                                CBOREncodingOperation::Tagged(_)
                                    | CBOREncodingOperation::OptionallyTagged(_)
                            )
                        })
            );
            if config.custom_encodings.is_some()
                && config.custom_serialize.is_some()
                && config.custom_deserialize.is_some()
                && (matches!(rust_struct.variant(), RustStructType::Record(_))
                    || is_complete_pair_map_wrapper)
            {
                custom_codec_rejections.insert(format!(
                    "@custom_encodings on `{ident}`: this rule mints a STRUCT, whose encoding \
                     metadata lives inside the struct itself (its `encodings` member) — the custom \
                     pair on a record rule delegates through generated thin impls, and \
                     hands no encoding tuple across the call, so there is nothing for a declaration \
                     to describe. Put the declaration where the pair takes encoding arguments: \
                     beside a FIELD's pair, or on a transparent alias rule's pair \
                     (`<rule> = <inner> ; @custom_serialize <fn> @custom_deserialize <fn> \
                     @custom_encodings <kinds>`)."
                ));
            }
            // The `@custom_wire_major` sibling of the check above, and for the same reason: the
            // declared major is read only through the ALIAS channel (`AliasInfo::rule_metadata`),
            // when the rule keys an open table's typed row or proves a variable middle array
            // boundary. A struct-minting rule has no such channel, so the declaration would be read
            // into the rule's metadata and dropped. (A declaration with one half of the pair or
            // none is the parse walk's
            // `reject_custom_encodings_without_pair`, so it cannot double-report here.)
            if config.custom_wire_major.is_some()
                && config.custom_serialize.is_some()
                && config.custom_deserialize.is_some()
            {
                custom_codec_rejections.insert(format!(
                    "@custom_wire_major on `{ident}`: this rule mints a STRUCT, and the declared \
                     major is read only where a transparent ALIAS keys an OPEN TABLE's typed row or \
                     proves a variable middle ARRAY boundary; a struct-minting rule has no such \
                     alias entry. Put the declaration on the alias rule whose codec writes that \
                     boundary item (`<wire> = <inner> ; @custom_serialize <fn> \
                     @custom_deserialize <fn> @custom_wire_major <major>`)."
                ));
            }
            if matches!(rust_struct.variant(), RustStructType::Record(_)) {
                if config.custom_serialize.is_some() && config.custom_deserialize.is_none() {
                    custom_codec_rejections.insert(format!(
                        "@custom_serialize alone on `{ident}`: a record rule with only the serialize \
                         half emits no `Serialize` impl for the type and never calls the named \
                         function, so the generated crate does not compile — every site holding a \
                         `{ident}` calls `.serialize(..)` on a type that has no impl. Move the pair \
                         to the field (or to the type rule of the member) that needs the custom \
                         format, or declare `{ident}` as a {EXTERN_MARKER} rule and hand-write the \
                         type in full."
                    ));
                }
                if config.custom_deserialize.is_some() && config.custom_serialize.is_none() {
                    custom_codec_rejections.insert(format!(
                        "@custom_deserialize alone on `{ident}`: a record rule with only the \
                         deserialize half still emits the type's own generated `Deserialize` impl, \
                         while every site holding a `{ident}` is rewritten to call the named function \
                         — so `{ident}::from_cbor_bytes` and a field of type `{ident}` decode the \
                         same bytes differently. The rule also projects OPAQUELY across the \
                         extern-interface seam, so a consumer decodes it the generated way. Move the \
                         pair to the field (or to the type rule of the member) that needs the custom \
                         format, or declare `{ident}` as a {EXTERN_MARKER} rule and hand-write the \
                         type in full."
                    ));
                }
            }
        }
        // A single half on a TRANSPARENT ALIAS rule — the alias twin of the record rule's
        // single-half rejection above, refused for that rejection's own stated reason: one type
        // decodes the same bytes two ways. An alias's lone half is the more insidious shape, because
        // unlike serialize-only on a record (no `Serialize` impl at all, so the crate does not
        // compile) it COMPILES and routes — `generate_serialize`/`generate_deserialize` lift each
        // half independently, so every embed site is rewritten in the declared direction while the
        // opposite direction keeps the aliased type's generated codec. The alias then writes one wire
        // format and reads another.
        //
        // Walked over the alias table rather than the struct loop above because the alias ENTRY is
        // what "lowers to a transparent alias" means — the collection and table rules register
        // through `AliasInfo::new_manual` (whose `rule_metadata` is `None`, so they are skipped here
        // and rejected by their own kind arms), and the struct-minting kinds have no entry at all.
        // Only a rule's OWN declaration reports: `strip_alias_for_registration` copies both halves
        // wholesale, so an INHERITED single half can only descend from an origin that is itself
        // reported here, and reporting each link would name rules nobody wrote the directive on.
        let mut single_half_alias_rejections = BTreeSet::new();
        for (alias_ident, info) in &self.type_aliases {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            if info.wire_metadata_inherited_from.is_some() {
                continue;
            }
            let Some(metadata) = info.rule_metadata.as_ref() else {
                continue;
            };
            let (directive, half, rewritten, missing) =
                match (&metadata.custom_serialize, &metadata.custom_deserialize) {
                    (Some(_), None) => (
                        "@custom_serialize",
                        "serialize",
                        "WRITES",
                        "@custom_deserialize",
                    ),
                    (None, Some(_)) => (
                        "@custom_deserialize",
                        "deserialize",
                        "READS",
                        "@custom_serialize",
                    ),
                    _ => continue,
                };
            single_half_alias_rejections.insert(format!(
                "{directive} alone on `{ident}`: a transparent alias rule with only the {half} \
                 half rewrites every embed site that {rewritten} through the named function, while \
                 the opposite direction keeps the aliased type's own generated codec — so `{ident}` \
                 reads one wire format and writes another, at every position that reaches it. Write \
                 both halves (`{ident} = <body> ; @custom_serialize <fn> @custom_deserialize \
                 <fn>`), adding the missing {missing}, or drop the directive."
            ));
        }
        for msg in single_half_alias_rejections {
            self.record_rejection(msg);
        }
        for msg in custom_codec_rejections {
            self.record_rejection(msg);
        }
        for ident in custom_json_alias_rejections {
            self.record_custom_json_on_transparent_alias_rejection(&ident);
        }
    }

    fn reject_custom_codecs_without_encoding_demand(&mut self, cli: &Cli) {
        // A custom codec whose replaced type demands NO encoding variables, under
        // `--preserve-encodings`, and with no `@custom_encodings` declaration to say what its own wire
        // needs. The pair replaces the codec, so the CODEC owns the wire — but the signature and the
        // sidecar slots are inferred from the REPLACED type, and a self-carrying leaf (an extern, a
        // record, `bool`, `any`, a `null`-fixed) infers NOTHING. Every framing byte the custom wire
        // writes is then unrecorded, and the round trip silently NORMALIZES it — invisible to a
        // round-trip test (both directions agree), visible only as a re-encoded artifact whose bytes
        // no longer hash the same. The declaration makes that state representable, so refusing the
        // undeclared spelling makes the silent one unrepresentable.
        //
        // Asked of `generation::encoding_fields_decls` — the SAME function the emission sites use to
        // build the argument list — rather than a twin predicate, so "empty demand" cannot come to
        // mean two different things. `Blind` because a pair governs its whole subtree (a declaration
        // beneath describes a codec this one's wire has swallowed). Gated on `--preserve-encodings`:
        // without it no encoding variable exists anywhere and the directive family is inert (one
        // spec, many flag sets). Skipped when rejections already exist — the demand walk reads
        // registered structs, which a failed registration may have left absent.
        if cli.preserve_encodings && !self.has_rejections() {
            let mut zero_demand_rejections = BTreeSet::new();
            for (ident, alias_info) in &self.type_aliases {
                let Some(rmd) = alias_info.rule_metadata.as_ref() else {
                    continue;
                };
                if rmd.custom_serialize.is_none()
                    || rmd.custom_deserialize.is_none()
                    || rmd.custom_encodings.is_some()
                {
                    continue;
                }
                // The codec-visible type is the alias's INNER type: the pair is lifted AT the alias
                // node, so any encoding operation the rule itself owns (`x = bytes .cbor y`) has
                // already been written by the enclosing generated code and is not the codec's to
                // record. Same slice the emission site sees one recursion level down.
                let mut codec_visible = alias_info.base_type.clone();
                codec_visible.encodings.clear();
                if crate::generation::custom_codec_demand_is_empty(self, &codec_visible, cli) {
                    zero_demand_rejections.insert(custom_codec_zero_demand_rejection(
                        &format!("rule `{ident}`"),
                        matches!(
                            codec_visible.conceptual_type.clone().resolve_aliases(),
                            ConceptualRustType::Rust(_)
                        ),
                    ));
                }
            }
            for (struct_ident, rust_struct) in &self.rust_structs {
                let RustStructType::Record(record) = rust_struct.variant() else {
                    continue;
                };
                for field in &record.fields {
                    let rmd = &field.rule_metadata;
                    if rmd.custom_serialize.is_none()
                        || rmd.custom_deserialize.is_none()
                        || rmd.custom_encodings.is_some()
                    {
                        continue;
                    }
                    // A FIELD-level pair fires at the top of the member's recursion, so its
                    // codec-visible list is the member's WHOLE type — encoding operations included
                    // (a `#6.9(uint)` field hands its tag width to the custom writer).
                    if crate::generation::custom_codec_demand_is_empty(self, &field.rust_type, cli)
                    {
                        zero_demand_rejections.insert(custom_codec_zero_demand_rejection(
                            &format!("field `{}` of `{struct_ident}`", field.name),
                            matches!(
                                field.rust_type.clone().resolve_aliases().conceptual_type,
                                ConceptualRustType::Rust(_)
                            ),
                        ));
                    }
                }
            }
            for msg in zero_demand_rejections {
                self.record_rejection(msg);
            }
        }
    }
}

/// Drain template dependencies in authored Vec order, retaining the first ready item at each step.
/// Rebuild the pending placeholder set after each application so dependencies become ready in place.
fn drain_in_dependency_order<T>(
    mut pending: Vec<T>,
    placeholder: impl Fn(&T) -> RustIdent,
    ready: impl Fn(&T, &BTreeSet<RustIdent>) -> bool,
    mut apply: impl FnMut(T),
    cycle_message: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    while !pending.is_empty() {
        let pending_placeholders = pending.iter().map(&placeholder).collect::<BTreeSet<_>>();
        let ready = pending
            .iter()
            .position(|item| ready(item, &pending_placeholders));
        let Some(ready) = ready else {
            return Err(cycle_message.into());
        };
        apply(pending.remove(ready));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone)]
    struct DependencyItem {
        name: &'static str,
        dependencies: Vec<&'static str>,
    }

    fn drain_items(
        items: Vec<DependencyItem>,
        cycle: &'static str,
    ) -> (Vec<&'static str>, Result<(), Box<dyn std::error::Error>>) {
        let mut order = Vec::new();
        let result = drain_in_dependency_order(
            items,
            |item| RustIdent::new(CDDLIdent::new(item.name)),
            |item, pending| {
                !item
                    .dependencies
                    .iter()
                    .any(|dep| pending.contains(&RustIdent::new(CDDLIdent::new(*dep))))
            },
            |item| order.push(item.name),
            cycle,
        );
        (order, result)
    }

    #[test]
    fn dependency_drain_keeps_nonlexical_ready_order() {
        let (order, result) = drain_items(
            ["zeta", "alpha", "middle"]
                .into_iter()
                .map(|name| DependencyItem {
                    name,
                    dependencies: Vec::new(),
                })
                .collect(),
            "unexpected ready-item cycle",
        );
        result.unwrap();
        assert_eq!(order, ["zeta", "alpha", "middle"]);
    }

    #[test]
    fn dependency_drain_reconsiders_earlier_blocked_items_before_ready_ties() {
        let (order, result) = drain_items(
            vec![
                DependencyItem {
                    name: "root",
                    dependencies: vec!["middle"],
                },
                DependencyItem {
                    name: "zeta",
                    dependencies: Vec::new(),
                },
                DependencyItem {
                    name: "middle",
                    dependencies: vec!["leaf"],
                },
                DependencyItem {
                    name: "leaf",
                    dependencies: Vec::new(),
                },
                DependencyItem {
                    name: "alpha",
                    dependencies: Vec::new(),
                },
            ],
            "unexpected dependency-chain cycle",
        );
        result.unwrap();
        assert_eq!(order, ["zeta", "leaf", "middle", "root", "alpha"]);
    }

    #[test]
    fn dependency_drain_preserves_both_template_cycle_messages() {
        for cycle in [
            "generic child-instance templates contain a cyclic placeholder dependency",
            "generic inline type-choice templates contain a cyclic placeholder dependency",
        ] {
            let (order, result) = drain_items(
                vec![
                    DependencyItem {
                        name: "ready",
                        dependencies: Vec::new(),
                    },
                    DependencyItem {
                        name: "a",
                        dependencies: vec!["b"],
                    },
                    DependencyItem {
                        name: "b",
                        dependencies: vec!["a"],
                    },
                ],
                cycle,
            );
            assert_eq!(order, ["ready"]);
            assert_eq!(result.unwrap_err().to_string(), cycle);
        }
    }
}
