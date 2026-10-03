use super::*;

impl<'a> IntermediateTypes<'a> {
    /// Which named `Table` rule solely owns each structural wasm-map shape, keyed by the structural
    /// `wasm_structural_map_name_for` string (that string IS the shape identity). A shape owned by EXACTLY ONE
    /// table rule has its wasm class plus the structural `pub type MapKToV = <Owner>;` alias minted in
    /// that owner's module (`mint_sole_owner_table` in generation/collections.rs); zero-owner (anonymous-only) and
    /// multi-owner (same-shape rule pair) shapes keep the structural fallback class at the crate root.
    /// Both the wasm emit path AND `scope_references`'s Map arm consult this single helper so import
    /// placement and emission placement CANNOT disagree. Iterates `rust_structs()` (a BTreeMap) so the
    /// result depends only on the SET of table rules, never on visit order.
    pub fn table_shape_sole_owners(&self) -> BTreeMap<String, RustIdent> {
        let mut owners: BTreeMap<String, Vec<RustIdent>> = BTreeMap::new();
        for (ident, rust_struct) in self.rust_structs() {
            // A table rule defined inside an extern-deps stub (non-exported scope) describes a
            // dep-owned type and must never be recorded as the owner of a structural map shape: the
            // sole-owner class is minted in the owner's scope, and a non-exported scope emits
            // nothing, so a consumer's OWN anonymous use of the same shape would silently lose its
            // wrapper. Only crate-owned table rules can own a shape.
            if !self.scope(ident).export() {
                continue;
            }
            // A SYNTHESIZED anonymous collection instance (a generic table instance like
            // `tbl<uint, tstr>` -> `TblU64Text`) must NOT own a structural map shape: it lowers to the
            // structural `MapKToV` wrapper through its `gen_wasm_alias` passthrough
            // (`pub type TblU64Text = MapU64ToText;`), exactly as an inline `{* k => v}` does. Recording
            // it as the sole owner would ALSO mint `mint_sole_owner_table`'s `pub struct TblU64Text` +
            // `pub type MapU64ToText = TblU64Text;`, colliding with the passthrough alias on BOTH idents
            // (the export.rs duplicate-ident backstop). This mirrors the anonymous-instance exclusion in
            // `non_empty_map_named_owner` / `non_empty_named_owner`.
            if self.is_anonymous_collection_instance(ident) {
                continue;
            }
            if let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rust_struct.variant()
            {
                // A non-empty `{+ k => v}` table does NOT own the loose structural `MapKToV` shape —
                // its JS class is the distinct restricted `NonEmptyMapKToV` (or rule-ident) wrapper.
                // Excluding it keeps anonymous plain `{* k => v}` uses of the same shape from being
                // (wrongly) folded onto the restricted class.
                if bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty) {
                    continue;
                }
                // The shape identity is the FLAVORED structural name, so a `@duplicates preserve`
                // rule owns `PairMapKToV` while a default rule of the identical key/value owns
                // `MapKToV` — two independent sole-owner entries that cannot interfere. The flavor is
                // read from this rule's own config: local information, never a crate-wide lookup.
                let structural = RustType::wasm_structural_map_name_for(
                    domain,
                    range,
                    rust_struct.config().duplicates_preserve(),
                    self,
                )
                .to_string();
                owners.entry(structural).or_default().push(ident.clone());
            }
        }
        owners
            .into_iter()
            .filter_map(|(structural, mut owners)| {
                (owners.len() == 1).then(|| (structural, owners.pop().unwrap()))
            })
            .collect()
    }

    /// Only an unbounded table can host the loose structural alias beside its named class.
    /// Restricted sole owners remain in the shape registry for named-wrapper identity.
    pub(crate) fn is_loose_table_owner(&self, owner: &RustIdent) -> bool {
        matches!(
            self.rust_structs().get(owner).map(|rs| rs.variant()),
            Some(RustStructType::Table { bounds: None, .. })
        )
    }

    fn loose_table_wrapper_scope(
        &self,
        ident: &RustIdent,
        sole_owners: &BTreeMap<String, RustIdent>,
    ) -> ModuleScope {
        sole_owners
            .get(&ident.to_string())
            .filter(|owner| self.is_loose_table_owner(owner))
            .map(|owner| self.scope(owner).clone())
            .unwrap_or_else(|| self.scope(ident).clone())
    }

    /// The wasm wrapper the code emitter (`RustType::for_wasm_member`) names for a collection
    /// occurrence `ty`, paired with the module its class is MINTED in — the import-tracker twin of
    /// the emitter's own name resolution, so a using scope imports EXACTLY the ident the emitter
    /// references, from EXACTLY the module the mint walk / `wasm()` places it. It branches
    /// identically to `for_wasm_member` (reject-set → non-empty-array → non-empty-map → loose
    /// `[* elem]` list), and resolves the home scope the SAME way emission does:
    /// - a LOOSE structural `MapKToV` with a loose sole named owner is minted in that owner's module
    ///   (via `table_shape_sole_owners`, shared with `mint_sole_owner_table`) — the one wrapper whose
    ///   home `types.scope` can't see, since the structural name is never a registered scope;
    /// - every other wrapper lives at `types.scope(wrapper_ident)`: the crate root for a synthesized
    ///   structural name (`NonEmpty<Elem>List` / `MapKToV` / `<Elem>OrderedSet`), or the owner's
    ///   module for a dedup-to-named (`Nums`/`Recs`/`Mp`) or rule-named wrapper.
    ///
    /// `None` for an occurrence that crosses the wasm boundary bare (an exposable `Vec<..>` array, or
    /// a non-collection type) — nothing to import. Callers must be in the wasm pass; the rust pass
    /// names no such wrappers.
    pub(crate) fn wasm_collection_wrapper(
        &self,
        ty: &RustType,
        sole_owners: &BTreeMap<String, RustIdent>,
    ) -> Option<(RustIdent, ModuleScope)> {
        // LOOSE structural map (not reject/non-empty): its emission scope is the sole owner's module
        // when one exists (matching `mint_sole_owner_table`), else `types.scope` (root). Resolve it
        // before the `for_wasm_member` name below because the sole-owner indirection is invisible to
        // `types.scope` — the structural `MapKToV` name is never a registered scope.
        if let ConceptualRustType::Map(_, _) = &ty.conceptual_type
            && !ty.is_non_empty_map()
            && !ty.is_bounded_map()
        {
            // The occurrence's own carried policy selects the flavor (`MapKToV` / `PairMapKToV`) —
            // the same local signal `for_wasm_member` uses, so name resolution here and at the
            // emitter cannot disagree.
            let ident = ty.wasm_structural_map_name(self);
            let scope = self.loose_table_wrapper_scope(&ident, sole_owners);
            return Some((ident, scope));
        }
        // Every remaining wrapper name resolves the same way `for_wasm_member` names it, and its home
        // is `types.scope(ident)` (root for a synthesized name, the owner's module for a dedup/rule
        // ident — a registered rust struct).
        let name = if ty.is_bounded_reject_ordered_set() {
            ty.bounded_reject_ordered_set_wasm_wrapper_name(self)
        } else if ty.is_reject_ordered_set() {
            ty.reject_ordered_set_wasm_wrapper_name(self)
        } else if ty.is_non_empty_array() {
            ty.non_empty_wasm_wrapper_name(self)
        } else if ty.is_type_enforced_exact_homogeneous_array() || ty.is_bounded_array() {
            ty.bounded_wasm_wrapper_name(self)
        } else if ty.is_non_empty_map() {
            ty.non_empty_wasm_map_wrapper_name(self)
        } else if ty.is_bounded_map() {
            ty.bounded_wasm_map_wrapper_name(self)
        } else {
            match &ty.conceptual_type {
                ConceptualRustType::Array(elem) if !ty.directly_wasm_exposable(self) => {
                    elem.name_as_wasm_array(self)
                }
                // exposable `[* uint]` -> bare `Vec`, or a non-collection: no wrapper to import
                _ => return None,
            }
        };
        let ident = RustIdent::from_formatted(name);
        let scope = self.scope(&ident).clone();
        Some((ident, scope))
    }

    /// For each scope, which other scopes are referenced, and which structs are referenced
    ///
    /// `deferred` (wasm pass only) maps every collection-wrapper ident the consumer is NOT minting —
    /// because a mapped dependency's `--extern-wrapper-index` already owns it — to that dependency's
    /// `collections` module scope. A deferred wrapper's referencing sites import it from there (a
    /// plain `use <dep_wasm>::collections::<Name>;` after the `--extern-wasm-crate` remap) from EVERY
    /// using scope, root included, since the class no longer lives locally. Empty for the rust pass
    /// and whenever `--extern-wrapper-index` is unused, so output is byte-identical without the flag.
    ///
    /// `requested` (wasm pass, `--wrapper-requests` host side) is every explicitly requested collection
    /// wrapper actually hosted into `requested_scope` this run as `(structural class ident, requested
    /// RustType)`. Those wrappers are
    /// NOT IR structs, so the struct walk below never sees them; after it runs, each is walked as if it
    /// were a rule emitted at `requested_scope` (mirroring the Array/Table struct-walk arms) so its body
    /// imports EXACTLY the cross-scope element / scoped-extern wasm classes it names. Empty (and
    /// `requested_scope` `None`) for the rust pass and whenever `--wrapper-requests` is unused, so output
    /// is byte-identical without the flag. `requested_hosted` is the full actual-mint ident set at that
    /// scope, including recursive support mints; all requested-scope same-file decisions use it.
    pub fn scope_references(
        &self,
        wasm: bool,
        deferred: &BTreeMap<RustIdent, ModuleScope>,
        requested: &[(RustIdent, RustType)],
        requested_hosted: &BTreeSet<RustIdent>,
        requested_scope: Option<&ModuleScope>,
    ) -> ScopeReferences {
        // we only want to mark TOP-LEVEL references without recursing into those types
        // which is why we don't use visit_types() here
        // Resolve wasm-map wrapper imports to the SAME module emission places them: a shape with a
        // sole owner is minted (class + structural alias) in that owner's module, everything else
        // falls back to the crate root. Computed once via the shared helper so the two sites can't drift.
        let mut walker = ScopeRefWalker::new(self, wasm, deferred);

        for rust_struct in self.rust_structs().values() {
            let current_scope = self.scope(&rust_struct.ident);
            match rust_struct.variant() {
                RustStructType::Array {
                    element_type,
                    bounds,
                } => {
                    // A NAMED rule whose emitted class borrows the LOOSE `<Elem>List` as its
                    // `try_from(&<Elem>List)` source names that builder bare in THIS scope. Three rule
                    // families do so: a restricted `[+ …]` rule (`generate_non_empty_array_type`), an
                    // ordinary/preserve bounded/static rule (`generate_bounded_array_type`), and a
                    // `@duplicates reject` rule of ANY bounds (`generate_reject_ordered_set_type`) —
                    // a plain `[*] reject` set still enters through `try_from(&FooList)`, so gating on
                    // the non-empty bound alone left its loose-source import (at the rule's scope) and
                    // the loose builder's element ref (at ROOT, its emission scope) unregistered
                    // (E0425 on both `FooList` and its element). The two helpers below apply the
                    // element-exposable / non-empty-element / self-named / deferred guards that decide
                    // whether a loose source actually exists, so a plain non-reject `[* foo]` rule
                    // (whose class wraps `Vec<Foo>` directly, no `try_from` source) is correctly a
                    // no-op even when it reaches here.
                    // LOCKSTEP: this gate mirrors the three restricted emitters' `loose_list`
                    // decisions — a reject rule emits `try_from(&Loose)` regardless of its `[*]`/`[+]`
                    // bound, while a bounded/static outer also does so over a constrained element. Change
                    // them together.
                    // A rule whose own wrapper DEFERRED (index mode unifying a rule ident with an
                    // indexed structural name) emits no class and therefore no `try_from` — it still
                    // reaches this gate, and the two helpers' own guards plus the usage-derived
                    // import prune leave it importing nothing, so the gate stays keyed on the rule's
                    // SHAPE rather than on a placement decision made in the generator.
                    let exact_static = bounds
                        .and_then(crate::intermediate::OccurrenceWindow::exact_len)
                        .is_some();
                    let bounded = bounds
                        .is_some_and(|window| !window.is_loose() && !window.is_non_empty())
                        && !exact_static;
                    let always_needs_loose_source =
                        !rust_struct.config().duplicates_reject() && (bounded || exact_static);
                    if wasm
                        && (bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                            || bounded
                            || exact_static
                            || rust_struct.config().duplicates_reject())
                    {
                        // The deferred (`--extern-wrapper-index`) analogue: when the loose `<Elem>List`
                        // is owned by a mapped dependency, import it from the dep's `collections`
                        // module at this rule's scope. Routed for reject rules too, by the same
                        // same-condition principle (a reject rule over a dep-owned element defers its
                        // loose source exactly as a non-empty rule does); a no-op when `deferred` is
                        // empty, so output is byte-identical without the flag.
                        walker.register_deferred_restricted_list_source(
                            &rust_struct.ident,
                            element_type,
                            always_needs_loose_source,
                        );
                        // The non-deferred analogue: the loose `<Elem>List` is a locally (ROOT-)
                        // minted class the rule's `try_from(&<Elem>List)` names bare in THIS scope,
                        // so import it here (E0425 otherwise). Fixes the `necollrec` and `rsetrec`
                        // cells.
                        walker.register_root_restricted_list_source(
                            current_scope,
                            &rust_struct.ident,
                            element_type,
                            always_needs_loose_source,
                        );
                    }
                    walker.mark_refs(current_scope, element_type)
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants, .. } => {
                    let is_group_choice =
                        matches!(rust_struct.variant(), RustStructType::GroupChoice { .. });
                    variants.iter().for_each(|ev| match &ev.data {
                        EnumVariantData::RustType(ty) => {
                            walker.mark_refs(current_scope, ty);
                            // A GROUP choice's `new_<variant>` ctor (both passes) expands a
                            // named-Record variant's fields into direct parameters, so the
                            // emitted code names those FIELD types in THIS scope — a Record
                            // living in another module otherwise only registers them for its
                            // own scope (its Record arm below) and the expanded ctor fails
                            // E0412. Mark exactly the ctor-visible set via the same helper the
                            // emitters use (a TYPE choice never expands — `generate_enum`'s
                            // `rep.and(..)` gate — so marking it here would only add unused
                            // imports).
                            if is_group_choice {
                                for field in ev
                                    .group_ctor_record_fields(self, &rust_struct.ident)
                                    .unwrap_or_default()
                                {
                                    walker.mark_refs(current_scope, &field.rust_type)
                                }
                            }
                        }
                        EnumVariantData::Inlined(record) => record
                            .fields
                            .iter()
                            .for_each(|field| walker.mark_refs(current_scope, &field.rust_type)),
                    })
                }
                RustStructType::Record(record) => {
                    record
                        .fields
                        .iter()
                        .for_each(|field| walker.mark_refs(current_scope, &field.rust_type));
                    // Open rest (map `* k => v` row or array `* t` tail): mark its CONTAINER
                    // (`RestRow::container_type` — the same `Map(k, v)`/`Array(t)` the rest field's
                    // member type and its wasm wrapper mint are built from), not the inner types
                    // flat. The container routes through `mark_refs`' Map/Array arms, so a rest row
                    // gets EXACTLY what a map/array FIELD of the same shape gets: the wrapper class
                    // the rest accessor returns imported into THIS scope from its emission scope,
                    // the `keys()` list wrapper registered at that emission scope, and the inner
                    // types marked THERE (the wrapper body names them, and it may live in another
                    // module). Marking the inners flat at the using scope instead left both sides
                    // dangling (E0425 on the wrapper here and on its key/value at root) for every
                    // rest row whose inner types are not root-scoped — the same-scope case only
                    // worked because `current_scope == emit_scope` made the flat marking
                    // coincidentally correct. Rust-side output is unchanged: with `wasm == false`
                    // both container arms fall through to marking the inners at the using scope,
                    // which is what this did. The wasm record-level `insert_<row>` operation is the
                    // one extra consumer: unlike the snapshot getter it names K and V directly in
                    // the OWNER's signature, so map-row inners are also marked at the owner scope
                    // below. That direct mark is load-bearing for multifile and deferred extern
                    // rows whose wrapper class lives somewhere else.
                    //
                    // The one row that does NOT go through its container is an open table's TYPED
                    // row under wasm: it has no container CLASS to import, because its map surface
                    // is flattened onto this struct's own class. The `keys()` that names the
                    // keys-list wrapper bare is therefore a method of THIS class, in THIS scope —
                    // the named-Table arm's situation, so it takes that arm's registration verbatim
                    // (root-minted `<K_t>List`, or the dep import when deferred). Routing it
                    // through the container arm instead registered the list at the container's
                    // would-be emission scope and left the struct's own `keys()` naming an
                    // unimported class (E0425 in a non-root module).
                    for rest in record.dynamic_rows() {
                        if wasm && record.is_typed_row(rest) && !rest.is_array_tail() {
                            walker.register_deferred_keys_list(current_scope, rest.domain());
                            walker.register_root_keys_list(current_scope, rest.domain());
                            // The struct's field type and its flattened accessors name `K_t`/`V_t`
                            // bare right here, so they are marked at THIS scope rather than at a
                            // container's.
                            for inner in [rest.domain(), rest.range()] {
                                walker.mark_refs(current_scope, inner);
                            }
                            // The keys-list class ITSELF names `K_t` bare in its own body
                            // (`get`/`add`), and it is minted at root rather than registered as an
                            // IR struct — so no struct-walk arm ever reaches it. A named table gets
                            // this for free (its keys-list IS an IR Array struct, minted at parse);
                            // mark the element at the list's scope here on the same condition
                            // `register_root_keys_list` mints under.
                            let loose_domain = rest.domain().loosened_for_wasm_table_boundary_key();
                            let keys_ident =
                                RustIdent::from_formatted(loose_domain.name_as_wasm_array(self));
                            if !ConceptualRustType::Array(Box::new(rest.domain().clone()))
                                .directly_wasm_exposable_ct(self)
                                && !deferred.contains_key(&keys_ident)
                            {
                                walker.mark_refs(&ROOT_SCOPE, &loose_domain);
                            }
                            // Bounded typed rows keep their checked carrier flattened on this
                            // record, but their fallible wasm `new` takes a loose same-flavor
                            // structural builder. Mark only that auxiliary builder: it imports the
                            // class and its key/value references into THIS owner scope without
                            // pretending the forbidden restricted whole-row class exists.
                            if rest.container_type().bounded_map_u64_bounds().is_some() {
                                let builder = rest.staging_container_type();
                                walker.mark_refs(current_scope, &builder);
                            }
                            continue;
                        }
                        walker.mark_refs(current_scope, &rest.container_type());
                        if wasm && !rest.is_array_tail() {
                            for inner in [rest.domain(), rest.range()] {
                                walker.mark_refs(current_scope, inner);
                            }
                        }
                    }
                }
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    // The named table's own wasm class is emitted in `current_scope`; its `keys()`
                    // accessor names the keys-list wrapper bare. Register BOTH homes, exactly as the
                    // inline Map arm above does: a ROOT-minted `<Key>List` for a non-exposable key
                    // (`register_root_keys_list`), OR a `use <dep_wasm>::collections::<KeysList>;` when
                    // the keys-list is workspace/index deferred (`register_deferred_keys_list`). Before
                    // the alias-recursion suppression that removed the accidental import route, a field
                    // referencing this named table recursed into the Map arm, whose call registered the
                    // deferred import; the named-rule arm is the correct home for it (follow the CLASS,
                    // not the using site — same rationale as the existing helpers).
                    walker.register_deferred_keys_list(current_scope, domain);
                    walker.register_root_keys_list(current_scope, domain);
                    // A named restricted table's class borrows a LOOSE structural `MapKToV` as its
                    // `try_from` source; when that source is deferred, import it at THIS rule's
                    // scope. `{+ …}` uses its native direct key; a bounded table uses the same
                    // top-level-loosened key as `generate_bounded_map_type`.
                    let bounded_source = bounds.is_some_and(|candidate| {
                        type_enforced_bounded_window(candidate.raw(), false).is_some()
                    });
                    if wasm
                        && (bounds.is_some_and(crate::intermediate::OccurrenceWindow::is_non_empty)
                            || bounded_source)
                    {
                        // the rule's own `@duplicates` config picks its container flavor, so the
                        // `try_from` source resolved below is the loose wrapper of the SAME flavor
                        let preserve = rust_struct.config().duplicates_preserve();
                        let source_key = if bounded_source {
                            domain.loosened_for_wasm_table_boundary_key()
                        } else {
                            domain.clone()
                        };
                        walker.register_deferred_restricted_map_source(
                            &rust_struct.ident,
                            &source_key,
                            range,
                            preserve,
                        );
                        // The non-deferred analogue: the loose `MapKToV` builder is a locally
                        // (ROOT- or sole-owner-) minted class the rule's `try_from(&MapKToV)` names
                        // bare in THIS scope, so import it here (E0425 otherwise). Fixes the
                        // `nemap`/`nepmap`/`nepmapa` cells.
                        walker.register_root_restricted_map_source(
                            current_scope,
                            &rust_struct.ident,
                            &source_key,
                            range,
                            preserve,
                        );
                    }
                    walker.mark_refs(current_scope, domain);
                    walker.mark_refs(current_scope, range);
                }
                RustStructType::Wrapper { wrapped, .. } => walker.mark_refs(current_scope, wrapped),
                RustStructType::Extern | RustStructType::RawBytesType => {
                    // impossible to know what this refers to - will have to be done afterwards by user
                }
                RustStructType::CStyleEnum { .. } => {
                    // should only refer to constants
                }
            }
        }
        // Type aliases are their own emission surface: a plain alias rule (`bal = st`) emits a
        // `pub type Bal = St;` line naming its TARGET bare in the alias's scope, with no field
        // reference for the struct walk above to see — a cross-scope target under-imported (E0412,
        // hit in production by `policy_id = script_hash`-style domain aliasing; the matrix
        // `aliased` cells pin every shape). Walk each emitted alias's `base_type` through
        // `mark_refs` — the rust line renders `for_rust_member(base_type)`, and the wasm
        // alias-base MINT walk (`generation/mod.rs`, the `visit_types_excluding` loop over
        // `type_aliases`) mints wrappers for the base's structural shapes regardless of what the
        // alias line names, so the import walk covers the base symmetrically (imports follow
        // minting; for a named-collection alias this re-records what its struct twin already did,
        // a `BTreeSet` no-op). The wasm alias line itself substitutes the stripped plain-typename
        // target's wrapper class when `resolved_wasm_alias_target` says so (the emitter consults
        // the SAME helper, so the emitted target and its import cannot drift) — that ident is
        // invisible in the stripped `base_type`, so import it additionally (from the dep's
        // `collections` module when deferred, like every deferred wrapper reference).
        // Their import map records only CROSS-scope idents, so single-module output is
        // byte-identical; in the wasm pass the separate boundary-use set also retains same-scope
        // names for own-spec extern glue.
        for (alias_ident, alias_info) in self.type_aliases() {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            let emitted_this_pass = if wasm {
                alias_info.emits_wasm_alias()
            } else {
                alias_info.emits_rust_alias()
            };
            if !emitted_this_pass {
                continue;
            }
            let current_scope = self.scope(ident);
            // A generic-EXTERN instance's alias base is a `Base<Args>` TYPE EXPRESSION minted by
            // `RustIdent::new_generic_with_base` (`ExtSet<Plain>`, `ExtSetRawBytes<PubKey>`), not an
            // importable path segment — `finalize`'s `GenericResolved::Extern` arm registers it as
            // `Rust(Base<Args>)` (a COLLECTION-bodied generic instance instead registers a transparent
            // structural alias, handled by the shape guard just below). Feeding that opaque ident
            // through `mark_refs`→`set_ref` would land the whole `<…>`-carrying text verbatim in the
            // scope's `use crate::generated::{…}` list (invalid Rust; the rustfmt post-pass aborts).
            // Decompose it instead: import the base at the base extern's DECLARING scope (where the
            // re-export glue in `generation/mod.rs` places `pub use crate::<Base>[RawBytes];` — NOT
            // `self.scope(&base_ident)`, since the flavored name is unregistered and would misroute
            // to root), and walk each argument so the bare arg names the alias line renders resolve
            // too. The wasm pass never reaches here (these aliases are `gen_wasm_alias=false`, gated
            // out above), so no wasm-side handling is needed.
            if let Some(gi) = self.generic_instances.get(ident) {
                let base_ident = gi.extern_base_ident(self);
                // Only a generic-EXTERN instance's alias base is the opaque `<Base>[RawBytes]<Args>`
                // type expression this block must decompose. A generated generic def with a
                // COLLECTION body (`xs<T> = [* T]` / `{* k => T}`, instanced as `xs<uint>`) resolves
                // to a TRANSPARENT structural alias (`Vec<u64>` / `BTreeMap<..>`) — its base has no
                // `<Args>` to strip and imports correctly through the normal `mark_refs` walk below,
                // so fall through rather than misroute it here. The shape guard IS the discriminator:
                // an instance alias whose base is `Rust(base)` prefixed by the extern base name can
                // only be the extern type expression (collection instances register `Array`/`Map`
                // bases, record instances register no alias at all).
                if matches!(
                    &alias_info.base_type.conceptual_type,
                    ConceptualRustType::Rust(base) if base.as_ref().starts_with(base_ident.as_ref())
                ) {
                    let base_scope = self.scope(&gi.generic_ident).clone();
                    if base_scope != *current_scope {
                        walker
                            .refs
                            .add_import(current_scope.clone(), base_scope, base_ident);
                    }
                    for arg in gi.generic_args() {
                        walker.mark_refs(current_scope, arg);
                    }
                    continue;
                }
            }
            if wasm && let Some(target) = alias_info.resolved_wasm_alias_target(self) {
                if let Some(dep_scope) = deferred.get(target) {
                    walker.refs.add_import(
                        current_scope.clone(),
                        dep_scope.clone(),
                        target.clone(),
                    );
                } else {
                    walker.set_ref(current_scope, target);
                }
            }
            walker.mark_refs(current_scope, &alias_info.base_type);
        }
        // W2 dep side (`--wrapper-requests`): the hosted requested wrappers are emitted into
        // `requested_scope` but are NOT in the IR, so the struct walk above never marked the wasm
        // classes their bodies name (element for a list, key/value/keys-list for a map, plus a
        // restricted wrapper's loose `try_from` source). Mirror the Array/Table struct-walk arms with
        // `requested_scope` as the emission scope — a hosted wrapper is exactly a rule emitted there —
        // so a cross-scope element (a struct in a non-root module) or a scoped extern (whose re-export
        // glue lands in its declaring scope, not the root) is imported from its true home instead of
        // being (un)reached by the ROOT-only `use super::*;`. Empty `requested` (rust pass / flag unused)
        // makes this a no-op, so output is byte-identical without the flag.
        if wasm && let Some(req_scope) = requested_scope {
            // Mark the ref a hosted wrapper's body names for one member (element / key / value). A member
            // that is ITSELF a hosted requested collection lives in `requested_scope` too (same file):
            // its wasm class is named bare with nothing to import, and its own body is walked when the
            // loop reaches its entry — so skip it rather than let `wasm_collection_wrapper` misroute the
            // structural name to the crate root (`types.scope` doesn't know the requested wrappers). Every
            // other member routes through the shared `mark_refs`, resolving to the member's true home.

            for (wid, rt) in requested {
                match &rt.conceptual_type {
                    ConceptualRustType::Array(elem) => {
                        // A restricted list (`[+ …]`, bounded/static ordinary list, or `@duplicates reject`) borrows a LOOSE `<Elem>List`
                        // as its `try_from` source, named bare at the emission scope. Import it there —
                        // unless that loose source is itself a hosted requested wrapper (same scope, no
                        // import; `register_root_*` would misroute the structural name to root).
                        if rt.is_restricted_list_occurrence() {
                            walker.register_deferred_restricted_list_source(
                                wid,
                                elem,
                                rt.restricted_list_always_needs_loose_source(),
                            );
                            let loose = RustIdent::from_formatted(elem.name_as_wasm_array(self));
                            if !requested_hosted.contains(&loose) {
                                walker.register_root_restricted_list_source(
                                    req_scope,
                                    wid,
                                    elem,
                                    rt.restricted_list_always_needs_loose_source(),
                                );
                            }
                        }
                        walker.mark_requested_member(req_scope, requested_hosted, elem);
                    }
                    ConceptualRustType::Map(key, value) => {
                        // The map class's `keys()` accessor names the keys-list wrapper bare at the
                        // emission scope (deferred from a dep's `collections`, or a ROOT-minted
                        // `<Key>List` for a non-exposable key). Skip the ROOT-import when the keys-list
                        // is ITSELF a co-hosted requested wrapper (same file): it is minted here, so
                        // `register_root_keys_list` would misroute the structural name to the crate
                        // root (E0432) — the keys-list twin of the loose-`try_from`-source guards on
                        // the list/map non-empty arms above. `register_deferred_keys_list` stays
                        // unguarded: it is a no-op for a locally-hosted keys-list and MUST still run
                        // for a mid-chain host whose keys-list is deferred to a deeper dep.
                        walker.register_deferred_keys_list(req_scope, key);
                        let keys_ident = key.wasm_table_keys_list_ident(self);
                        if !requested_hosted.contains(&keys_ident) {
                            walker.register_root_keys_list(req_scope, key);
                        }
                        // A restricted map borrows a LOOSE `MapKToV` as its `try_from` source named
                        // bare at the emission scope — same requested-source guard as the list arm.
                        // Bounded maps use the emitter's top-level-loosened direct key.
                        if rt.is_non_empty_map() || rt.is_bounded_map() {
                            let source_key = if rt.is_bounded_map() {
                                key.loosened_for_wasm_table_boundary_key()
                            } else {
                                (**key).clone()
                            };
                            walker.register_deferred_restricted_map_source(
                                wid,
                                &source_key,
                                value,
                                rt.is_preserve_pair_map(),
                            );
                            let loose = RustType::wasm_structural_map_name_for(
                                &source_key,
                                value,
                                rt.is_preserve_pair_map(),
                                self,
                            );
                            if !requested_hosted.contains(&loose) {
                                walker.register_root_restricted_map_source(
                                    req_scope,
                                    wid,
                                    &source_key,
                                    value,
                                    rt.is_preserve_pair_map(),
                                );
                            }
                        }
                        walker.mark_requested_member(req_scope, requested_hosted, key);
                        walker.mark_requested_member(req_scope, requested_hosted, value);
                    }
                    // A requested shape is always a collection (guarded in `emit_requested_collections`).
                    _ => {}
                }
            }
        }
        walker.refs
    }
}

struct ScopeRefWalker<'walk, 'ast> {
    types: &'walk IntermediateTypes<'ast>,
    wasm: bool,
    deferred: &'walk BTreeMap<RustIdent, ModuleScope>,
    refs: ScopeReferences,
    sole_owners: BTreeMap<String, RustIdent>,
}

impl<'walk, 'ast> ScopeRefWalker<'walk, 'ast> {
    fn new(
        types: &'walk IntermediateTypes<'ast>,
        wasm: bool,
        deferred: &'walk BTreeMap<RustIdent, ModuleScope>,
    ) -> Self {
        Self {
            types,
            wasm,
            deferred,
            refs: ScopeReferences::default(),
            sole_owners: types.table_shape_sole_owners(),
        }
    }

    fn set_ref(&mut self, current_scope: &ModuleScope, rust_ident: &RustIdent) {
        let types = self.types;
        let wasm = self.wasm;
        if wasm {
            self.refs.wasm_boundary_idents.insert(rust_ident.clone());
        }
        let ref_scope = types.scope(rust_ident);
        if current_scope != ref_scope {
            self.refs
                .add_import(current_scope.clone(), ref_scope.clone(), rust_ident.clone());
        }
    }
    // Register the import of a DEFERRED keys-list wrapper into `emit_scope` (the module a locally
    // minted map class is emitted in — root or the sole owner's). A map's `keys()` accessor names
    // the keys-list wrapper; when that wrapper is deferred to a dependency it must be imported
    // where the map class lives, from the dep's `collections` module. No-op when the keys-list is
    // not deferred (its class is local, same module). Independent of `current_scope`: it follows
    // the map class, not the using site.
    fn register_deferred_keys_list(&mut self, emit_scope: &ModuleScope, key: &RustType) {
        let types = self.types;
        let deferred = self.deferred;
        let keys_ident = key.wasm_table_keys_list_ident(types);
        if let Some(dep_scope) = deferred.get(&keys_ident) {
            self.refs
                .add_import(emit_scope.to_owned(), dep_scope.clone(), keys_ident);
        }
    }
    // Register the import of a DEFERRED loose LIST wrapper that a locally-minted restricted
    // wrapper (`NonEmpty*List`, a bounded/static carrier, or a named restricted rule's class) borrows as its `try_from`
    // source. The `try_from(&<Elem>List)` reference is conversion-internal — invisible to the
    // field walk, the same class of problem as a map's `keys()`-list
    // (`register_deferred_keys_list`), solved the same way: follow the CLASS, not the using
    // site — import at the restricted wrapper's EMISSION scope, from the dep's `collections`
    // module. No-op when: a bare `Vec` of the element crosses the ABI (`try_from` takes that
    // `Vec`, no loose class is named) or the element is itself non-empty (no loose source
    // exists — built incrementally); the
    // loose name equals the wrapper ident (a self-named rule emits no `try_from`); or the
    // loose wrapper is not deferred (it is a local class in the same scope). Empty `deferred`
    // (rust pass / flag unused) makes this a no-op, so output is byte-identical without the flag.
    fn register_deferred_restricted_list_source(
        &mut self,
        wrapper_ident: &RustIdent,
        elem: &RustType,
        always_needs_loose_source: bool,
    ) {
        let types = self.types;
        let deferred = self.deferred;
        if elem.vec_of_self_directly_wasm_exposable(types)
            || (!always_needs_loose_source && elem.is_non_empty_array())
        {
            return;
        }
        let loose = elem.name_as_wasm_array(types);
        if loose == wrapper_ident.as_ref() {
            return;
        }
        let loose_ident = RustIdent::from_formatted(loose);
        if let Some(dep_scope) = deferred.get(&loose_ident) {
            let emit_scope = types.scope(wrapper_ident).clone();
            self.refs
                .add_import(emit_scope, dep_scope.clone(), loose_ident);
        }
    }
    // The map twin of `register_deferred_restricted_list_source`: a locally-minted restricted
    // map class enters via `try_from(&MapKToV)` — when that loose structural table wrapper is
    // deferred, import it at the restricted wrapper's emission scope. The caller passes the
    // exact SOURCE key: native for `{+ …}`, top-level-loosened for a bounded table. Additional
    // no-op case: the loose shape has a SOLE table-rule owner — the `try_from` source is then
    // an actual loose owner's local `pub type MapKToV = <Owner>;` alias, never a deferred class.
    #[allow(clippy::too_many_arguments)]
    fn register_deferred_restricted_map_source(
        &mut self,
        wrapper_ident: &RustIdent,
        key: &RustType,
        value: &RustType,
        // the restricted wrapper's container flavor: its `try_from` source is the loose wrapper of
        // the SAME flavor (`PairMapKToV` for a `@duplicates preserve` `{+ …}`, `MapKToV` otherwise)
        preserve: bool,
    ) {
        let types = self.types;
        let deferred = self.deferred;
        let loose_ident = RustType::wasm_structural_map_name_for(key, value, preserve, types);
        if loose_ident.as_ref() == wrapper_ident.as_ref()
            || self
                .sole_owners
                .get(&loose_ident.to_string())
                .is_some_and(|owner| types.is_loose_table_owner(owner))
        {
            return;
        }
        if let Some(dep_scope) = deferred.get(&loose_ident) {
            let emit_scope = types.scope(wrapper_ident).clone();
            self.refs
                .add_import(emit_scope, dep_scope.clone(), loose_ident);
        }
    }
    // Register the import of a locally ROOT-minted keys-list wrapper into `emit_scope` (the
    // module a table's wasm class is emitted in). A map's `keys()` accessor names the keys-list
    // wrapper BARE (`{Elem}List(...)`) exactly when the key is non-exposable AND the wrapper is
    // not deferred — mirroring `codegen_table_type`'s emission condition. That wrapper is
    // synthesized at ROOT_SCOPE (`create_and_register_array_type`, never `mark_scope`'d), so a
    // class emitted in a non-root module must import it. No-op (matching the emitter naming NO
    // wrapper, or naming one that lives in the same scope) when: not the wasm pass; the emit
    // scope IS root (wrapper minted there too); the key is exposable (bare `Vec` return, no
    // wrapper named); or the keys-list is deferred (`register_deferred_keys_list` imports it from
    // the dep's `collections` module instead). Independent of the using site: it follows the
    // table class, like `register_deferred_keys_list`.
    fn register_root_keys_list(&mut self, emit_scope: &ModuleScope, key: &RustType) {
        let types = self.types;
        let wasm = self.wasm;
        let deferred = self.deferred;
        if !wasm || *emit_scope == *ROOT_SCOPE {
            return;
        }
        // exposable keys return a bare `Vec` — the emitter names no wrapper, so nothing to import
        if ConceptualRustType::Array(Box::new(key.clone())).directly_wasm_exposable_ct(types) {
            return;
        }
        let keys_ident = key.wasm_table_keys_list_ident(types);
        // deferred keys-lists live in a dep's `collections` module — imported by the deferred
        // helper, not from root
        if deferred.contains_key(&keys_ident) {
            return;
        }
        self.refs
            .add_import(emit_scope.to_owned(), ROOT_SCOPE.clone(), keys_ident);
    }
    // The non-deferred analogue of `register_deferred_restricted_list_source`: a restricted list
    // wrapper (`NonEmpty*List`, a bounded/static carrier, a named restricted rule, or a dedup owner) emitted at `emit_scope`
    // borrows a LOOSE `<Elem>List` as its `try_from` source, and that loose builder is a locally
    // minted class (typically ROOT-minted). Its `try_from(&<Elem>List)` names the loose builder
    // bare in `emit_scope`, so import it there — the list twin of `register_root_keys_list`. Also
    // register the loose builder's OWN element ref at the builder's scope (its `get`/`add`
    // accessors name the element bare where the builder lives). No-op when: a bare `Vec` of the
    // element crosses the ABI (`try_from` takes that `Vec`, no loose class) or the element is
    // itself non-empty (built
    // incrementally, no loose source); the loose name equals the wrapper ident (a self-named rule
    // emits no `try_from`); or the loose builder is deferred (the deferred helper imports it from
    // the dep's `collections` module instead).
    #[allow(clippy::too_many_arguments)]
    fn register_root_restricted_list_source(
        &mut self,
        emit_scope: &ModuleScope,
        wrapper_ident: &RustIdent,
        elem: &RustType,
        always_needs_loose_source: bool,
    ) {
        let types = self.types;
        let wasm = self.wasm;
        let deferred = self.deferred;
        if !wasm
            || elem.vec_of_self_directly_wasm_exposable(types)
            || (!always_needs_loose_source && elem.is_non_empty_array())
        {
            return;
        }
        let loose = elem.name_as_wasm_array(types);
        if loose == wrapper_ident.as_ref() {
            return;
        }
        let loose_ident = RustIdent::from_formatted(loose);
        if deferred.contains_key(&loose_ident) {
            return;
        }
        let loose_scope = types.scope(&loose_ident).clone();
        if loose_scope != *emit_scope {
            self.refs
                .add_import(emit_scope.to_owned(), loose_scope.clone(), loose_ident);
        }
        self.mark_refs(&loose_scope, elem);
    }
    // The map twin of `register_root_restricted_list_source`: a restricted map wrapper emitted
    // at `emit_scope` enters via `try_from(&MapKToV)`, naming the LOOSE structural table wrapper
    // bare in `emit_scope`. The caller passes the exact SOURCE key: native for `{+ …}`,
    // top-level-loosened for a bounded table. Import it here, resolving the loose builder's own
    // home the SAME way emission places it (`table_shape_sole_owners`: the owner's
    // `pub type MapKToV = <Owner>;` module when a loose sole owner exists, else root). Also register
    // the loose builder's key/value refs at its scope. No-op when the loose name equals the
    // wrapper ident (self-named rule) or the loose builder is deferred.
    #[allow(clippy::too_many_arguments)]
    fn register_root_restricted_map_source(
        &mut self,
        emit_scope: &ModuleScope,
        wrapper_ident: &RustIdent,
        key: &RustType,
        value: &RustType,
        // the restricted wrapper's container flavor; see the deferred twin above
        preserve: bool,
    ) {
        let types = self.types;
        let wasm = self.wasm;
        let deferred = self.deferred;
        if !wasm {
            return;
        }
        let loose_ident = RustType::wasm_structural_map_name_for(key, value, preserve, types);
        if loose_ident.as_ref() == wrapper_ident.as_ref() || deferred.contains_key(&loose_ident) {
            return;
        }
        let loose_scope = types.loose_table_wrapper_scope(&loose_ident, &self.sole_owners);
        if loose_scope != *emit_scope {
            self.refs.add_import(
                emit_scope.to_owned(),
                loose_scope.clone(),
                loose_ident.clone(),
            );
        }
        self.mark_refs(&loose_scope, key);
        self.mark_refs(&loose_scope, value);
    }
    fn mark_refs(&mut self, current_scope: &ModuleScope, ty: &RustType) {
        let types = self.types;
        let wasm = self.wasm;
        let deferred = self.deferred;
        match &ty.conceptual_type {
            ConceptualRustType::Alias(alias_ident, alias_ty) => {
                if let AliasIdent::Rust(rust_ident) = alias_ident {
                    // A named COLLECTION rule whose ident COINCIDES with a structural wrapper
                    // name a mapped dependency's `--extern-wrapper-index` lists is DEFERRED at
                    // mint time: no class of that name exists locally, so `set_ref`'s
                    // same-scope no-op would leave every by-name reference naming an undefined
                    // type (E0425). Route the import from the dep's `collections` module into
                    // EVERY using scope, root included — the same rule the structural
                    // Array/Map arms below apply, which is the point: the named and inline
                    // reference positions must agree about where a deferred wrapper lives.
                    // Recursion stays suppressed (wrapper and element are both the
                    // dependency's), as it is for the local-rule case just below. `deferred` is
                    // empty for the rust pass and whenever the flag families are unused, so
                    // output is byte-identical without the flag.
                    if let Some(dep_scope) = deferred.get(rust_ident) {
                        self.refs.add_import(
                            current_scope.to_owned(),
                            dep_scope.clone(),
                            rust_ident.clone(),
                        );
                        return;
                    }
                    self.set_ref(current_scope, rust_ident);
                    // A named COLLECTION rule (`recs = [* foo]` / `withdrawals = {* k => v}`, or a
                    // generic instance like `gcn = gcoll<foo>`) registers a transparent alias, so
                    // a field referencing it is `Alias(Recs, Array(Foo))`. In the WASM pass the
                    // rule's OWN class (imported above via `set_ref`) IS the boundary surface —
                    // recursing the collection target would mint a structural-wrapper import
                    // (`FooList` / `MapKToV`) the rule subsumes and nothing else defines: E0432 for
                    // a locally-owned rule, or a dangling `crate::generated::MapKToV` for a
                    // DEP-owned rule (`table_shape_sole_owners` excludes non-exported scopes, so it
                    // falls back to a root structural name with no owner). Suppress the target
                    // recursion for such an alias; its element/key/value are the rule's concern,
                    // imported at the rule's own scope by its Table/Array struct-walk arm. Only the
                    // WASM pass names these structural wrappers, so the rust pass still recurses
                    // (byte-identical output).
                    if wasm
                        && matches!(
                            types.rust_struct(rust_ident).map(|rs| rs.variant()),
                            Some(RustStructType::Array { .. } | RustStructType::Table { .. })
                        )
                    {
                        return;
                    }
                }
                // Also import idents the serialization INLINED through this transparent alias
                // will name. A cross-module NAMED `.cbor` ref (`fb = bytes .cbor foo` in module
                // `a`, referenced by name from `b`) resolves the alias to its target
                // (`pub type Fb = Foo;`), and `b`'s serialization emits `Foo::deserialize(..)`
                // while only `Fb` was imported — E0433 `cannot find type Foo`. `set_ref` records
                // only CROSS-scope idents, so single-module output is byte-identical; an
                // occasionally-unneeded cross-module import is harmless (generated code
                // legitimately over-imports; `unused_imports` is deliberately not denied).
                // Only the conceptual type drives ref-marking, so wrap the alias target (a bare
                // `ConceptualRustType`) in a throwaway `RustType`.
                let alias_target = RustType::new((**alias_ty).clone());
                self.mark_refs(current_scope, &alias_target);
            }
            // No deferred consult here, unlike the Alias arm above: an ident can only enter
            // `deferred` from `try_defer_wrapper`, whose `wrapper_ident` is either a
            // synthesized structural wrapper name or the ident of a rust struct whose variant
            // is `Array` or `Table` — and `register_rust_struct` gives BOTH of those variants a
            // transparent type alias, so every by-name reference to a deferrable ident arrives
            // as `Alias(Rust(ident), …)` and is handled there. A bare `Rust(ident)` is a
            // Record / choice / Wrapper struct, none of which is ever a defer candidate.
            ConceptualRustType::Rust(rust_ident) => self.set_ref(current_scope, rust_ident),
            ConceptualRustType::Array(elem_ty) => {
                // Resolve the wasm wrapper this occurrence crosses the boundary as, and its
                // emission scope, the SAME way the emitter (`for_wasm_member`) names it and the
                // mint walk places it — so a using scope imports EXACTLY the ident the emitter
                // references (`NonEmpty<Elem>List` / a dedup owner / a rule ident / the loose
                // `<Elem>List`), never the pre-NonEmpty spelling, and from the wrapper's TRUE
                // home rather than a hard-coded root.
                if let Some((wrapper, emit_scope)) = wasm
                    .then(|| types.wasm_collection_wrapper(ty, &self.sole_owners))
                    .flatten()
                {
                    if let Some(dep_scope) = deferred.get(&wrapper) {
                        // Deferred to a dependency's `--extern-wrapper-index`: the wrapper class
                        // no longer lives locally — import it from the dep's `collections` module
                        // from EVERY using scope (root included) and do NOT recurse (wrapper and
                        // element are both the dependency's).
                        self.refs
                            .add_import(current_scope.to_owned(), dep_scope.clone(), wrapper);
                        return;
                    }
                    // Import the emitter-named wrapper into the using scope from its emission
                    // scope (a no-op when they coincide, e.g. an anonymous same-shape use inside
                    // the wrapper's own module).
                    if emit_scope != *current_scope {
                        self.refs.add_import(
                            current_scope.to_owned(),
                            emit_scope.clone(),
                            wrapper.clone(),
                        );
                    }
                    // A RESTRICTED wrapper (`[+ …]`, bounded/static ordinary list, or `@duplicates reject`) borrows a LOOSE
                    // `<Elem>List` as its `try_from` source, named bare in its emission scope —
                    // import it there (deferred + non-deferred analogues).
                    if ty.is_restricted_list_occurrence() {
                        self.register_deferred_restricted_list_source(
                            &wrapper,
                            elem_ty,
                            ty.restricted_list_always_needs_loose_source(),
                        );
                        self.register_root_restricted_list_source(
                            &emit_scope,
                            &wrapper,
                            elem_ty,
                            ty.restricted_list_always_needs_loose_source(),
                        );
                    }
                    // The wrapper's emitted code names its ELEMENT type bare in its EMISSION
                    // scope, which may not be this using scope — register the element ref from
                    // there. Recurse (not a single `set_ref`) so a nested anonymous wrapper
                    // resolves its own element too.
                    self.mark_refs(&emit_scope, elem_ty);
                    return;
                }
                // Exposable `[* uint]` (bare `Vec`) or the rust pass: recurse the element at the
                // using scope, as before (rust-side output stays byte-identical).
                self.mark_refs(current_scope, elem_ty);
            }
            // The wasm face spells `any` as bare `AnyCbor`, emitted or re-exported in the root
            // wasm scope. The rust face uses a fully qualified common-crate path.
            ConceptualRustType::Any => {
                if wasm && *current_scope != *ROOT_SCOPE {
                    self.refs.add_import(
                        current_scope.to_owned(),
                        ROOT_SCOPE.clone(),
                        RustIdent::from_formatted("AnyCbor"),
                    );
                }
            }
            ConceptualRustType::Fixed(_) | ConceptualRustType::Primitive(_) => {
                // nothing to import
            }
            ConceptualRustType::Map(key, value) => {
                // Resolve the wasm map wrapper this occurrence crosses as, and its emission scope,
                // the SAME way emission decides both — `for_wasm_member` for the NAME (the
                // restricted `NonEmptyMap*` / a dedup owner / the loose `MapKToV`) and
                // `table_shape_sole_owners` for the loose builder's HOME (the sole owner's module
                // when one exists, else root). One helper, so import placement and emission
                // placement cannot disagree.
                if let Some((wrapper, emit_scope)) = wasm
                    .then(|| types.wasm_collection_wrapper(ty, &self.sole_owners))
                    .flatten()
                {
                    if let Some(dep_scope) = deferred.get(&wrapper) {
                        // The whole map wrapper is deferred to a dependency's
                        // `--extern-wrapper-index`: import it from the dep's `collections` module
                        // from every using scope (root included); wrapper, key, and value are all
                        // the dependency's, so don't recurse.
                        self.refs
                            .add_import(current_scope.to_owned(), dep_scope.clone(), wrapper);
                        return;
                    }
                    if emit_scope != *current_scope {
                        self.refs.add_import(
                            current_scope.to_owned(),
                            emit_scope.clone(),
                            wrapper.clone(),
                        );
                    }
                    // A restricted map wrapper enters via `try_from(&MapKToV)`, naming the loose
                    // structural table wrapper bare in its emission scope. `{+ …}` uses its
                    // native key; a bounded table uses its deliberately loosened direct key.
                    if ty.is_non_empty_map() || ty.is_bounded_map() {
                        let source_key = if ty.is_bounded_map() {
                            key.loosened_for_wasm_table_boundary_key()
                        } else {
                            (**key).clone()
                        };
                        self.register_deferred_restricted_map_source(
                            &wrapper,
                            &source_key,
                            value,
                            ty.is_preserve_pair_map(),
                        );
                        self.register_root_restricted_map_source(
                            &emit_scope,
                            &wrapper,
                            &source_key,
                            value,
                            ty.is_preserve_pair_map(),
                        );
                    }
                    // The map class's `keys()` accessor names the keys-list wrapper bare in its
                    // EMISSION scope — import it there (deferred from the dep's `collections`
                    // module, or a ROOT-minted `<Key>List` when non-exposable).
                    self.register_deferred_keys_list(&emit_scope, key);
                    self.register_root_keys_list(&emit_scope, key);
                    // The wrapper body names its KEY and VALUE types bare in its emission scope —
                    // register their refs from there.
                    self.mark_refs(&emit_scope, key);
                    self.mark_refs(&emit_scope, value);
                    return;
                }
                // The rust pass (maps always cross wasm through a wrapper, so this is rust-only):
                // recurse key/value at the using scope, as before (byte-identical rust output).
                self.mark_refs(current_scope, key);
                self.mark_refs(current_scope, value);
            }
            ConceptualRustType::Optional(inner_ty) => self.mark_refs(current_scope, inner_ty),
        }
    }

    fn mark_requested_member(
        &mut self,
        req_scope: &ModuleScope,
        requested_hosted: &BTreeSet<RustIdent>,
        member: &RustType,
    ) {
        let types = self.types;
        if let Some((wrapper, _)) = types.wasm_collection_wrapper(member, &self.sole_owners)
            && requested_hosted.contains(&wrapper)
        {
            return;
        }
        self.mark_refs(req_scope, member);
    }
}
