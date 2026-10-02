use super::*;

impl GenerationScope {
    /// `--wrapper-requests` (dependency side): the attribution doc for `ident` as a paragraph
    /// PREFIX (trailing blank line) to prepend to an emitter-set struct doc, or `""` when the
    /// wrapper is not requested.
    /// Used by the NonEmpty emitters, whose `.doc()` call would otherwise clobber the attribution
    /// `create_base_wasm_struct` injects.
    pub(super) fn requested_attribution_prefix(&self, ident: &RustIdent) -> String {
        self.requested_attribution
            .get(ident)
            .map(|d| format!("{d}\n\n"))
            .unwrap_or_default()
    }

    /// `--wrapper-requests` (dependency side): read each consumer's committed
    /// `borrowed_collections.rs`, take the entries addressed to THIS dep (dep column == the
    /// normalized `--lib-name`), union the
    /// requested collection-wrapper shapes across consumers, and emit every requested wrapper the dep
    /// does not already produce into `wasm/src/generated/requested_collections.rs` (indexed via
    /// `record_collection_wrapper`, each carrying a sorted-requester attribution doc). Called once,
    /// after the own-spec wasm walk, under `--wasm`. A no-op — output byte-identical to today — when
    /// no `--wrapper-requests` flag is set (the module is not even created).
    ///
    /// Determinism: everything is keyed/sorted (`BTreeMap`/`BTreeSet`), so the union and the emission
    /// order depend on neither the flag order nor the consumers' regen order.
    ///
    /// The sidecar readers' refusals and the `--wrapper-requests` diagnostics this function owns
    /// all come back as `Err`: they travel to `generate_to_disk`'s caller, so `main` reports them
    /// as `Error: …`
    /// (exit 1) and `--config`'s mid-run wrapper can name the crates already regenerated.
    pub(super) fn emit_requested_collections(
        &mut self,
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> Result<(), String> {
        let request_files = cli.wrapper_requests();
        if request_files.is_empty() {
            // No flag => no file, byte-identical to a run without the feature.
            return Ok(());
        }
        let my_lib = cli.lib_name_code();

        // One entry per requested shape after unioning across consumers.
        struct Unioned {
            rt: RustType,
            structural: String,
            requesters: BTreeSet<String>,
        }
        // Keyed by the canonically RE-RENDERED shape (so `stake-credential` ≡ `stake_credential`
        // unify): two consumers requesting the same shape with hyphen/underscore skew collapse here.
        let mut union: BTreeMap<String, Unioned> = BTreeMap::new();

        for (consumer, path) in &request_files {
            let Some(contents) = crate::wrapper_requests::read_request_sidecar(
                "--wrapper-requests",
                consumer,
                path,
            )?
            else {
                continue;
            };
            let entries = crate::wrapper_requests::parse_sidecar(&contents, path)?;
            for entry in entries {
                // Entries addressed to OTHER deps (dep column != this crate's normalized lib name)
                // are silently skipped — a shared sidecar can name several deps.
                if entry.dep.replace('-', "_") != my_lib {
                    continue;
                }
                let rt = parse_requested_shape(types, &entry.shape, consumer, path, &entry.name)?;
                // A requested shape that is DIRECTLY WASM-EXPOSABLE has no wrapper class at all —
                // it lowers to a bare `Vec<…>` at the wasm boundary — so no borrowed wrapper exists
                // or is needed. Such a request is the symptom of an unfaithful consumer stub: the
                // consumer declared its element(s) opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this
                // dep resolves them transparently to a directly-exposable type. Diagnose it here,
                // before deriving the structural name — otherwise a loose list over a transparent
                // primitive alias (`[* coin]` with `coin = uint`) misdiagnoses as a name↔shape
                // disagreement, and a member-form listing (`Vec<u64>` for `[* uint]`) slips past the
                // cross-check and dies later in rustfmt labeled a generator bug.
                if let Some(member) = requested_exposable_member(types, &rt) {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        "its element is a wasm-primitive".to_owned()
                    } else {
                        format!("its element(s) resolve here as {}", leaves.join(", "))
                    };
                    return Err(format!(
                        "--wrapper-requests {consumer} ({path}): the requested wrapper {:?} with \
                         shape {:?} is directly wasm-exposable — it lowers to `{member}` with no \
                         wrapper class, so no borrowed wrapper exists or is needed ({leaf_note}). \
                         This request is the symptom of an unfaithful consumer stub: the consumer \
                         declared the element opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this dep \
                         resolves it transparently. Remedy: fix the consumer's \
                         `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` stub for this dep to declare the element \
                         truthfully (e.g. `coin = uint`) and regenerate the consumer, which will \
                         then stop borrowing this shape.",
                        entry.name, entry.shape
                    ));
                }
                let canonical = render_wrapper_shape(&rt);
                let structural = requested_structural_name(types, &rt, consumer, path)?;
                // Cross-check the derived structural name against the listed name (hard error).
                if structural != entry.name {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        String::new()
                    } else {
                        format!(" Element resolution in this dep: {}.", leaves.join(", "))
                    };
                    return Err(format!(
                        "--wrapper-requests {consumer} ({path}): the borrowed wrapper listed as \
                         {:?} with shape {:?} derives the structural name {:?}, not {:?} — the \
                         sidecar's name and shape columns disagree (a name↔shape mismatch).{leaf_note}",
                        entry.name, entry.shape, structural, entry.name
                    ));
                }
                let u = union.entry(canonical).or_insert_with(|| Unioned {
                    rt: rt.clone(),
                    structural: structural.clone(),
                    requesters: BTreeSet::new(),
                });
                u.requesters.insert(consumer.clone());
            }
        }

        // Criterion 8 #4: two DISTINCT requested shapes deriving the SAME structural name (from any
        // combination of consumers) — one JS class for two concepts. Name both shapes and their
        // requesters.
        let mut by_structural: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for shape in union.keys() {
            by_structural
                .entry(union[shape].structural.clone())
                .or_default()
                .push(shape.clone());
        }
        for (structural, shapes) in &by_structural {
            if shapes.len() > 1 {
                let requesters: BTreeSet<&String> = shapes
                    .iter()
                    .flat_map(|s| union[s].requesters.iter())
                    .collect();
                return Err(format!(
                    "--wrapper-requests: two distinct requested shapes derive the same structural \
                     wrapper name {structural:?}: {shapes:?} (requested by {requesters:?}). These \
                     would define one JS class for two concepts — rename or @name one of the shapes \
                     in the requesting consumers."
                ));
            }
        }

        // Decide, per unioned shape, whether the dep already produces it (skip), produces it under a
        // different rule name (hard error), or must emit it.
        let mut to_emit: Vec<(&String, &Unioned)> = Vec::new();
        for (canonical, u) in &union {
            match self
                .wasm_collection_wrapper_registry
                .own_wrapper_shape(canonical)
            {
                // Own spec already produces this shape under the STRUCTURAL name => request satisfied
                // by the existing indexed wrapper; emit nothing.
                Some(existing) if existing.as_ref() == u.structural => {}
                // Own spec produces this shape under a DIFFERENT (rule-declared) name => hard error.
                Some(existing) => {
                    return Err(format!(
                        "--wrapper-requests: requested shape {canonical:?} (requested by {:?}) is \
                         already produced by this dep's own spec under the non-structural rule name \
                         {existing}, not the structural name {:?} the consumers import. Emitting \
                         both would create two JS classes for one concept. Remedy: rename the rule \
                         {existing} to {}, give it `@name {}`, or drop it.",
                        u.requesters, u.structural, u.structural, u.structural
                    ));
                }
                None => to_emit.push((canonical, u)),
            }
        }

        // Criterion 8 #5: a requested NESTED shape whose inner collection wrapper is neither requested
        // nor own-spec-produced — an integrity check against a hand-edited / truncated sidecar (a real
        // consumer closes over its nested shapes automatically, so the inner should always be present).
        for (canonical, u) in &to_emit {
            for inner in inner_collection_shapes(&u.rt) {
                let requested = union.contains_key(&inner);
                let own = self
                    .wasm_collection_wrapper_registry
                    .own_wrapper_shape(&inner)
                    .is_some();
                if !requested && !own {
                    return Err(format!(
                        "--wrapper-requests: requested shape {canonical:?} nests the collection \
                         wrapper {inner:?}, which is neither requested by any consumer nor produced \
                         by this dep's own spec. The inner collection of an all-one-dep shape is \
                         itself all-one-dep and must be requested too — this sidecar looks truncated \
                         or hand-edited."
                    ));
                }
            }
        }

        // Emit. `to_emit` is in canonical-shape (BTreeMap) order, so loose `[* …]` precedes its
        // NonEmpty `[+ …]` twin (`*` < `+`): a separately-requested loose source is emitted (and gets
        // its attribution) BEFORE the NonEmpty emitter's recursive mint no-ops on it. A NonEmpty
        // support source that is NOT itself requested is minted by the emitter into this same module
        // (indexed, no attribution — a benign transitive superset). Byte-identical under any flag /
        // regen order because the input set is fully sorted.
        let requested_scope = ModuleScope::from(vec!["requested_collections".to_owned()]);
        for (_, u) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(u.structural.clone()));
            let requesters: Vec<&str> = u.requesters.iter().map(String::as_str).collect();
            self.requested_attribution.insert(
                ident,
                format!("Generated at the request of: {}.", requesters.join(", ")),
            );
        }
        self.requested_scope_override = Some(requested_scope.clone());
        // A requested map wrapper's `keys()` accessor names the loose keys-list class even when the
        // sidecar correctly requests only the map class. Own-spec generation mints that companion
        // from the record/table walk; requested wrappers have no IR owner, so the host must mint it
        // explicitly here. Keep one shared set for the whole union — two requested map shapes can
        // return the same `<Key>List`, which is one wasm class in this requested scope.
        let mut requested_keys_lists_generated = BTreeSet::new();
        for (_, u) in &to_emit {
            let rt = &u.rt;
            let ident = RustIdent::new(CDDLIdent::new(u.structural.clone()));
            match &rt.conceptual_type {
                ConceptualRustType::Array(inner) => {
                    if rt.is_reject_ordered_set() {
                        // The `@duplicates reject` uniqueness twin — the wasm class wrapping
                        // `OrderedSet`/`NonEmptyOrderedSet` with the checked `add` door. The
                        // non-empty flavor is chosen exactly as the loose/NonEmpty split below.
                        self.generate_reject_ordered_set_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            rt.is_non_empty_array(),
                            rt.bounded_array_u64_bounds(),
                            // `false`, exactly as the loose/NonEmpty arms below: a hosted request is
                            // not a rule of THIS dep's spec. The defer consult the emitter now makes
                            // is naturally inert here for the same reason it already is for those
                            // siblings — a requested shape's elements are reconstructed as the DEP's
                            // OWN (exported) types, so `wrapper_placement` sees a consumer-owned
                            // leaf and the index path's constituent screen returns local. A host that
                            // itself carries `--workspace-dep`/index flags therefore changes nothing
                            // for reject that it does not already change for the twins.
                            false,
                            cli,
                        );
                    } else if rt.is_non_empty_array() {
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    } else if let Some((min, max)) = rt
                        .exact_homogeneous_array_u64_bounds()
                        .or_else(|| rt.bounded_array_u64_bounds())
                    {
                        self.generate_bounded_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            (min, max),
                            false,
                            cli,
                        );
                    } else {
                        self.generate_array_type(types, (**inner).clone(), &ident, false, cli);
                    }
                }
                ConceptualRustType::Map(k, v) => {
                    if let Some(bounds) = rt.bounded_map_u64_bounds() {
                        self.generate_bounded_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            bounds,
                            false,
                            rt.is_preserve_pair_map(),
                            cli,
                        );
                    } else if rt.is_non_empty_map() {
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            // cross-crate wrapper-request hosting resolves the flavor from the
                            // requested `RustType`; an inline request carries no directive so this is
                            // `false` today (the same anonymous-seam reachability as the inline mint).
                            rt.is_preserve_pair_map(),
                            cli,
                        );
                    } else {
                        codegen_table_type(
                            self,
                            types,
                            &ident,
                            (**k).clone(),
                            (**v).clone(),
                            false,
                            // recover the flavor from the requested `RustType`; an inline cross-crate
                            // request carries no directive, so this is `false` today (same
                            // anonymous-seam reachability as the `{+ …}` request above).
                            rt.is_preserve_pair_map(),
                            cli,
                        );
                    }
                    // A map's body always exposes `keys()`. This mints its non-exposable key's
                    // loose list companion after the map so the map's deferral decision remains
                    // canonical, and makes the requested-scope closure self-contained even when
                    // this dep's own spec never mentions the key as a table domain.
                    mint_wasm_keys_list(self, types, k, &mut requested_keys_lists_generated, cli);
                }
                other => unreachable!("requested shape is not a collection: {other:?}"),
            }
            // An explicit request is only a hosted body when its emitter actually minted it in this
            // scope. Recursive support mints can pre-empt a later explicit row, and a future emitter
            // may decline a candidate; keep this walk list truthful rather than predicting ownership.
            if self
                .wasm_collection_wrapper_registry
                .local_class_scope(&ident)
                == Some(&requested_scope)
            {
                self.requested_wrapper_types.push((ident, rt.clone()));
            }
        }
        self.requested_scope_override = None;

        // A requested NonEmpty wrapper pulls in the NonEmpty runtime the dep's OWN spec may not use;
        // record it so the runtime-provisioning gates (mod decl + static file copy) fire, and import
        // the type into this scope explicitly (the per-scope loop's import gate is keyed off the dep's
        // own IR, which doesn't see the requested wrappers).
        self.requested_non_empty_vec = to_emit.iter().any(|(_, u)| u.rt.contains_non_empty_array());
        self.requested_bounded_vec = to_emit.iter().any(|(_, u)| u.rt.contains_bounded_array());
        self.requested_bounded_map = to_emit.iter().any(|(_, u)| u.rt.contains_bounded_map());
        self.requested_non_empty_map = to_emit.iter().any(|(_, u)| u.rt.contains_non_empty_map());
        // A requested reject wrapper pulls in the `ordered_set` runtime the dep's OWN spec may not
        // use; record it so the runtime-provisioning gates (mod decl + static file copy) fire.
        self.requested_ordered_set = to_emit.iter().any(|(_, u)| u.rt.contains_ordered_set());
        // The map-side twin: a requested `@duplicates preserve` table wraps `PairMap`/`NonEmptyPairMap`,
        // a runtime the dep's OWN spec may never mention — without this the hosted class references a
        // type the dep's crate neither declares nor copies in (E0433 at the dep's build).
        self.requested_pair_map = to_emit.iter().any(|(_, u)| u.rt.contains_pair_map());
        let non_empty_import = self
            .requested_non_empty_vec
            .then(|| format!("{}::non_empty", cli.common_import_wasm()));
        let bounded_import = self
            .requested_bounded_vec
            .then(|| format!("{}::bounded", cli.common_import_wasm()));
        let bounded_map_import = self
            .requested_bounded_map
            .then(|| format!("{}::bounded_map", cli.common_import_wasm()));
        let non_empty_map_import = self
            .requested_non_empty_map
            .then(|| format!("{}::non_empty_map", cli.common_import_wasm()));
        let ordered_set_import = self
            .requested_ordered_set
            .then(|| format!("{}::ordered_set", cli.common_import_wasm()));
        let pair_map_import = self
            .requested_pair_map
            .then(|| format!("{}::pair_map", cli.common_import_wasm()));

        // Ensure the module exists even when nothing is emitted (all requests satisfied by own spec /
        // addressed elsewhere) — stable presence, stable diffs. When non-empty, the
        // wrappers reference the dep's own element WASM wrappers (which live at the generated root or a
        // sibling module); `use super::*;` reaches them, mirroring the emit-tests glob. The per-scope
        // import loop later adds the common wasm imports (wasm_bindgen/JsError/OrderedHashMap/…).
        let scope_content = self.wasm_scopes.entry(requested_scope).or_default();
        if !to_emit.is_empty() {
            scope_content.raw("use super::*;");
        }
        // These NonEmpty imports are pushed whenever the requested wrappers use them; if the file's
        // module family ends up not naming one, the prune pass
        // (`import_prune::prune_generated_files`, in `generated_files`) drops it. Dumb-push +
        // central prune, same as the struct sites.
        if let Some(path) = non_empty_import {
            scope_content.push_import(path, "NonEmptyVec", None);
        }
        if let Some(path) = bounded_import {
            scope_content.push_import(path, "BoundedVec", None);
        }
        if let Some(path) = bounded_map_import {
            scope_content.push_import(path, "BoundedMap", None);
        }
        if let Some(path) = non_empty_map_import {
            scope_content.push_import(path, "NonEmptyMap", None);
        }
        // The reject twin wraps `core::OrderedSet` / `NonEmptyOrderedSet`; the per-scope import loop
        // gates these on the dep's OWN `uses_ordered_set()`, so a dep hosting ONLY a requested reject
        // wrapper needs them pushed here (same dumb-push + central-prune contract as the twins above).
        if let Some(path) = ordered_set_import {
            scope_content.push_import(path.clone(), "OrderedSet", None);
            scope_content.push_import(path.clone(), "NonEmptyOrderedSet", None);
            scope_content.push_import(path, "BoundedOrderedSet", None);
        }
        // The preserve twin wraps `core::PairMap` / `NonEmptyPairMap`, gated the same way (the
        // per-scope loop keys off the dep's OWN `uses_pair_map()`, which a request-only host fails).
        if let Some(path) = pair_map_import {
            scope_content.push_import(path.clone(), "PairMap", None);
            scope_content.push_import(path.clone(), "NonEmptyPairMap", None);
            scope_content.push_import(path, "BoundedPairMap", None);
        }
        Ok(())
    }
}

/// The CDDL prelude spelling of a primitive, for the canonical shape renderer. Kept in lockstep with
/// the wasm-map/list structural naming: the dep re-parses a rendered shape and must derive the SAME
/// structural name, so each primitive renders to a CDDL name whose `for_variant` round-trips (e.g.
/// `uint` -> `U64` -> `MapU64To…`). `u8`/`i8`/… are cddl-codegen's own sized-int spellings.
fn primitive_cddl_name(p: &Primitive) -> &'static str {
    match p {
        Primitive::Bool => "bool",
        Primitive::Float => "float",
        Primitive::F16 => "float16",
        Primitive::F32 => "float32",
        Primitive::F64 => "float64",
        Primitive::F16To32 => "float16-32",
        Primitive::F32To64 => "float32-64",
        Primitive::U8 => "u8",
        Primitive::I8 => "i8",
        Primitive::U16 => "u16",
        Primitive::I16 => "i16",
        Primitive::U32 => "u32",
        Primitive::I32 => "i32",
        Primitive::U64 => "uint",
        Primitive::I64 => "i64",
        Primitive::N64 => "nint",
        Primitive::Str => "text",
        Primitive::Bytes => "bytes",
    }
}

/// Render a collection wrapper's CDDL shape fragment in the canonical `borrowed_collections.rs`
/// shape-column grammar —
/// `[* foo]` / `[+ foo]` / `[*5 foo]` / `[2* foo]` / `[2*5 foo]` for loose, non-empty, and bounded
/// lists, `{* k => v}` / `{+ k => v}` for maps, nesting recursively. Element idents are the dependency's own spec spelling
/// (snake_case of the rust ident, matching the extern-stub naming a dep re-parses after
/// normalization); primitives render as their CDDL prelude name. The occurrence marker is taken from
/// the `RustType`'s own bounds so nested non-empty shapes are honored at every level. This is the
/// single shape renderer shared by the not-in-index warning hint and (later) the request-sidecar
/// machinery, so its output is EXACTLY the format a dep parses back.
pub(crate) fn render_wrapper_shape(rt: &RustType) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            let occ = render_occurrence(rt.config.bounds);
            // A `@duplicates reject` collection appends its policy marker so the shape column
            // round-trips the uniqueness twin (parsed back by `parse_requested_shape`, and matched as
            // a distinct canonical shape from the same loose/non-empty list). Kept byte-identical to
            // the marker `generate_reject_ordered_set_type` records for the dep's own reject wrappers.
            let reject = if rt.duplicates_reject() {
                format!(" {REJECT_MARKER}")
            } else {
                String::new()
            };
            format!("[{occ} {}]{reject}", render_wrapper_shape(inner))
        }
        ConceptualRustType::Map(key, value) => {
            let occ = render_occurrence(rt.config.bounds);
            // The map-side twin of the array arm's reject marker: a `@duplicates preserve` table's
            // backing container (`PairMap`) is part of its structural identity, so the shape column
            // carries the policy and the reconstruction rebuilds the same flavored wrapper.
            let preserve = if rt.is_preserve_pair_map() {
                format!(" {PRESERVE_MARKER}")
            } else {
                String::new()
            };
            format!(
                "{{{occ} {} => {}}}{preserve}",
                render_wrapper_shape(key),
                render_wrapper_shape(value)
            )
        }
        // An optional isn't itself a wrapper occurrence — render its inner shape (only reachable via
        // nesting; the top-level constituents the callers pass are Array/Map/named-leaf).
        ConceptualRustType::Optional(inner) => render_wrapper_shape(inner),
        ConceptualRustType::Rust(ident) => convert_to_snake_case(ident.as_ref()),
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => {
            convert_to_snake_case(ident.as_ref())
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(name), _) => name.clone(),
        ConceptualRustType::Primitive(p) => primitive_cddl_name(p).to_owned(),
        // `any` renders as the CDDL prelude spelling in a cross-crate request shape column.
        ConceptualRustType::Any => "any".to_owned(),
        // Fixed values carry no CDDL ident and never appear as a real wrapper element; render a
        // placeholder rather than panicking so the advisory hint text stays best-effort.
        ConceptualRustType::Fixed(_) => "_".to_owned(),
    }
}

/// The occurrence marker of a collection shape in the `borrowed_collections.rs` shape-column
/// grammar (`*`, `+`, `?`,
/// `*5`, `2*`, `2*5`), shared by the list and map arms of [`render_wrapper_shape`].
fn render_occurrence(bounds: Option<IntWindow>) -> String {
    match bounds {
        Some((Some(1), None)) => "+".to_owned(),
        Some((None, Some(1))) => "?".to_owned(),
        Some((None, Some(max))) => format!("*{max}"),
        Some((Some(min), None)) => format!("{min}*"),
        Some((Some(min), Some(max))) => format!("{min}*{max}"),
        None | Some((None, None)) => "*".to_owned(),
    }
}

/// The trailing shape-column policy markers a flavored collection carries — the array-side
/// uniqueness twin and the map-side pair-map twin. They ride the shape column BARE (no `;`) because
/// the sidecar round-trips them by PARSE (`parse_requested_shape` consumes exactly these strings),
/// and the rendered column is not CDDL to begin with.
pub(crate) const REJECT_MARKER: &str = "@duplicates reject";
pub(crate) const PRESERVE_MARKER: &str = "@duplicates preserve";

/// Split a rendered shape column into its bare CDDL shape and its trailing policy marker, if any.
/// The only consumer is the paste-able rule-line hint: a rule line pasted into a spec must carry the
/// directive in COMMENT position (`foo = {* uint => bar} ; @duplicates preserve`) — the bare marker
/// the sidecar column uses is not valid CDDL after the shape, so pasting it verbatim would fail the
/// dep's parse for exactly the flavored shapes whose structural name encodes the flavor.
pub(crate) fn split_shape_policy_marker(shape: &str) -> (&str, Option<&str>) {
    for marker in [PRESERVE_MARKER, REJECT_MARKER] {
        if let Some(bare) = shape.strip_suffix(marker) {
            return (bare.trim_end(), Some(marker));
        }
    }
    (shape, None)
}

/// Validate `--workspace-dep` values and return the set. Each named dep must be a
/// configured extern dependency (`extern_dep_names()`) AND have an `--extern-wasm-crate` mapping —
/// the deferral imports and the sidecar's `use` lines both need the wasm crate name, so a missing
/// mapping is a hard error rather than a silent fallback. Mirrors `load_extern_wrapper_indices`'
/// startup hardening. The accessor already rejected empty / `=`-bearing values.
pub(super) fn load_workspace_deps(
    types: &IntermediateTypes,
    cli: &Cli,
) -> Result<BTreeSet<String>, String> {
    let deps = cli.workspace_deps();
    if deps.is_empty() {
        return Ok(BTreeSet::new());
    }
    let extern_dep_names = types.extern_dep_names();
    let wasm_crate_map = cli.extern_wasm_crate_map();
    for dep in &deps {
        if !extern_dep_names.contains(dep) {
            return Err(format!(
                "--workspace-dep names dependency {dep:?}, which is not an extern dependency in this \
                 spec. Known extern dependencies: {extern_dep_names:?}"
            ));
        }
        if !wasm_crate_map.contains_key(dep) {
            return Err(format!(
                "--workspace-dep {dep:?} has no --extern-wasm-crate mapping; workspace deferral needs \
                 the dep's wasm crate name for its imports and the borrowed-collections sidecar. Add \
                 --extern-wasm-crate {dep}=<wasm_crate>."
            ));
        }
    }
    Ok(deps)
}

// ===== `--wrapper-requests` (dependency side): shape reconstruction + structural naming =========

/// Reverse of `primitive_cddl_name`: the `Primitive` a shape-column leaf denotes, or `None` for a
/// named-type leaf. Only the exact spellings `render_wrapper_shape` emits for primitive leaves are
/// recognized, so a dep type whose snake-case happens NOT to be a prelude name is correctly treated
/// as a named element.
fn primitive_from_cddl_name(name: &str) -> Option<Primitive> {
    Some(match name {
        "bool" => Primitive::Bool,
        "float" => Primitive::Float,
        "float16" => Primitive::F16,
        "float32" => Primitive::F32,
        "float64" => Primitive::F64,
        "float16-32" => Primitive::F16To32,
        "float32-64" => Primitive::F32To64,
        "u8" => Primitive::U8,
        "i8" => Primitive::I8,
        "u16" => Primitive::U16,
        "i16" => Primitive::I16,
        "u32" => Primitive::U32,
        "i32" => Primitive::I32,
        "uint" => Primitive::U64,
        "i64" => Primitive::I64,
        "nint" => Primitive::N64,
        "text" => Primitive::Str,
        "bytes" => Primitive::Bytes,
        _ => return None,
    })
}

/// Whether `c` can appear in a shape-column leaf token (a CDDL ident or a prelude/sized-int name).
/// The one owner of the leaf-token alphabet, shared by the strict parser's leaf arm and
/// `requested_shape_leaf_resolutions`' diagnostic walk so the two cannot tokenize a shape
/// differently.
fn is_shape_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

/// The sidecar row a requested shape came from. Carried by [`ShapeParser`] only to make its
/// refusals actionable: every message names the consumer, the sidecar path, the whole shape, and
/// the listed wrapper name.
struct ShapeRow<'a> {
    consumer: &'a str,
    path: &'a str,
    shape: &'a str,
    listed_name: &'a str,
}

/// Cursor over one shape column in the `borrowed_collections.rs` shape-column grammar that
/// [`render_wrapper_shape`]
/// emits. The dep's IR is passed to [`Self::fragment`] rather than stored, so the parser owns only
/// the input and its error context.
struct ShapeParser<'a> {
    chars: Vec<char>,
    pos: usize,
    row: ShapeRow<'a>,
}

impl<'a> ShapeParser<'a> {
    fn new(row: ShapeRow<'a>) -> Self {
        Self {
            chars: row.shape.chars().collect(),
            pos: 0,
            row,
        }
    }

    fn skip_ws(&mut self) {
        while self.pos < self.chars.len() && self.chars[self.pos].is_whitespace() {
            self.pos += 1;
        }
    }

    /// Consume `s` if the input continues with it.
    fn eat(&mut self, s: &str) -> bool {
        let want: Vec<char> = s.chars().collect();
        if self.chars[self.pos..].starts_with(&want) {
            self.pos += want.len();
            true
        } else {
            false
        }
    }

    /// The `malformed shape` refusal, naming what the parser expected at this point.
    fn malformed(&self, what: &str) -> String {
        let ShapeRow {
            consumer,
            path,
            shape,
            listed_name,
        } = self.row;
        format!(
            "--wrapper-requests {consumer} ({path}): malformed shape {shape:?} (wrapper \
             {listed_name:?}): {what}."
        )
    }

    /// The occurrence marker after a collection's opening bracket; `expected` is the `malformed`
    /// clause when there is none.
    fn occurrence(&mut self, expected: &str) -> Result<IntWindow, String> {
        read_occurrence(&self.chars, &mut self.pos).ok_or_else(|| self.malformed(expected))
    }

    /// Read a leaf token (possibly empty) in the [`is_shape_ident_char`] alphabet.
    fn leaf_token(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.chars.len() && is_shape_ident_char(self.chars[self.pos]) {
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    /// Parse the whole shape column: one fragment, then optionally one top-level policy marker, then
    /// end of input.
    fn parse(mut self, types: &IntermediateTypes) -> Result<RustType, String> {
        let mut rt = self.fragment(types, 0)?;
        self.skip_ws();
        let ShapeRow {
            consumer,
            path,
            shape,
            listed_name,
        } = self.row;
        // Collection fragments consume their own trailing duplicate-policy marker, including when
        // they are nested. That keeps the parser aligned with the recursive renderer: a nested
        // preserve map must rebuild its PairMap identity before its parent structural name is
        // reconstructed.
        let rest: String = self.chars[self.pos..].iter().collect();
        if rest == REJECT_MARKER {
            if !matches!(rt.conceptual_type, ConceptualRustType::Array(_)) {
                return Err(format!(
                    "--wrapper-requests {consumer} ({path}): `@duplicates reject` on the non-array shape \
                     {shape:?} (wrapper {listed_name:?}) — the reject policy only applies to set/array \
                     collections."
                ));
            }
            rt.config.duplicates = Some(crate::comment_ast::DuplicatesPolicy::Reject);
            self.pos = self.chars.len();
        } else if rest == PRESERVE_MARKER {
            if !matches!(rt.conceptual_type, ConceptualRustType::Map(_, _)) {
                return Err(format!(
                    "--wrapper-requests {consumer} ({path}): `@duplicates preserve` on the non-map shape \
                     {shape:?} (wrapper {listed_name:?}) — the preserve pair-map twin only applies to \
                     table collections."
                ));
            }
            rt.config.duplicates = Some(crate::comment_ast::DuplicatesPolicy::Preserve);
            self.pos = self.chars.len();
        }
        if self.pos != self.chars.len() {
            return Err(format!(
                "--wrapper-requests {consumer} ({path}): trailing content after the shape {shape:?} \
                 (wrapper {listed_name:?})."
            ));
        }
        Ok(rt)
    }

    fn fragment(&mut self, types: &IntermediateTypes, depth: usize) -> Result<RustType, String> {
        if depth > MAX_SHAPE_DEPTH {
            let ShapeRow {
                consumer,
                path,
                shape,
                listed_name,
            } = self.row;
            return Err(format!(
                "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                 (shape {shape:?}) nests collections deeper than the supported limit of \
                 {MAX_SHAPE_DEPTH}. Real wrapper shapes nest only a few levels; this is almost \
                 certainly a malformed hand-edited sidecar."
            ));
        }
        self.skip_ws();
        if self.pos >= self.chars.len() {
            return Err(self.malformed("unexpected end of shape"));
        }
        if self.eat("[") {
            self.skip_ws();
            let occ = self
                .occurrence("expected an array occurrence (`*`, `+`, `?`, `*N`, `N*`, or `N*M`)")?;
            self.skip_ws();
            let inner = self.fragment(types, depth + 1)?;
            self.skip_ws();
            if !self.eat("]") {
                return Err(self.malformed("expected `]`"));
            }
            let mut rt = RustType::new(ConceptualRustType::Array(Box::new(inner))).with_bounds(occ);
            self.skip_ws();
            if self.eat(REJECT_MARKER) {
                rt.config.duplicates = Some(crate::comment_ast::DuplicatesPolicy::Reject);
            }
            return Ok(rt);
        }
        if self.eat("{") {
            self.skip_ws();
            let occ = self
                .occurrence("expected a table occurrence (`*`, `+`, `?`, `*N`, `N*`, or `N*M`)")?;
            self.skip_ws();
            let key = self.fragment(types, depth + 1)?;
            self.skip_ws();
            if !self.eat("=>") {
                return Err(self.malformed("expected `=>`"));
            }
            self.skip_ws();
            let value = self.fragment(types, depth + 1)?;
            self.skip_ws();
            if !self.eat("}") {
                return Err(self.malformed("expected `}`"));
            }
            let mut rt = RustType::new(ConceptualRustType::Map(Box::new(key), Box::new(value)));
            rt = match occ {
                (None, None) => rt,
                bounds => rt.with_bounds(bounds),
            };
            self.skip_ws();
            if self.eat(PRESERVE_MARKER) {
                rt.config.duplicates = Some(crate::comment_ast::DuplicatesPolicy::Preserve);
            }
            return Ok(rt);
        }
        // A named or primitive leaf: read the ident token.
        let token = self.leaf_token();
        if token.is_empty() {
            return Err(self.malformed("expected an element type name"));
        }
        self.leaf(types, token)
    }

    /// Resolve one leaf token against the dep's IR.
    fn leaf(&self, types: &IntermediateTypes, token: String) -> Result<RustType, String> {
        let ShapeRow {
            consumer,
            path,
            shape,
            listed_name,
        } = self.row;
        if let Some(p) = primitive_from_cddl_name(&token) {
            return Ok(RustType::new(ConceptualRustType::Primitive(p)));
        }
        // A reserved CDDL keyword (`biguint`, `bigint`, …) or reserved Rust type name
        // (`option` → `Option`) as a leaf token would trip `RustIdent::new`'s internal asserts
        // — an internal panic reachable only from a hand-edited sidecar (a real consumer never
        // emits these). Pre-check through the reservation rule's one owner
        // (`RustIdent::reserved_reason`, the same predicate `new` asserts on) so external
        // input surfaces the feature's own hard error instead of the assert.
        if RustIdent::reserved_reason(&token).is_some() {
            return Err(format!(
                "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                 (shape {shape:?}) uses the reserved identifier {token:?} as a wrapper element; \
                 reserved CDDL keywords and reserved Rust type names cannot be wrapper elements."
            ));
        }
        let ident = RustIdent::new(CDDLIdent::new(token.clone()));
        if !dep_owns_element(types, &ident) {
            return Err(format!(
                "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                 (shape {shape:?}) references the element type {token:?}, which this dep does not \
                 own. The consumer's extern stub for this dep and the dep's own spec disagree — \
                 the request cannot be satisfied."
            ));
        }
        // Resolve through the pipeline's one alias-substitution rule (`resolve_alias`, shared
        // with `new_type` so this path cannot drift from pipeline resolution): a leaf left as
        // a bare `Rust(ident)` naming an alias (`stake_credential = credential`, `policy_id =
        // script_hash`) panics downstream lookups (`is_enum`, exposability, member naming)
        // that assume `Rust(ident)` names a registered struct. The `Alias` wrapper the rule
        // keeps for rust-alias-generating rules preserves the requested ident for structural
        // naming (the consumer derived `StakeCredentialList` from the alias name) while
        // resolving storage/exposability through the target, matching what the dep's own
        // generation of the same CDDL shape would produce. `dep_owns_element` already required
        // a spec-registered ident, so `new_type`'s unregistered-reserved prelude fallback (the
        // one mutable part) cannot be needed here.
        Ok(types
            .resolve_alias(&AliasIdent::Rust(ident.clone()))
            .unwrap_or_else(|| RustType::new(ConceptualRustType::Rust(ident))))
    }
}

/// Reconstruct a requested wrapper's `RustType` from its canonical shape column, resolving each
/// named leaf against the DEP's own IR after the same normalization (`RustIdent::new`, which
/// camel-cases and folds `-`/`_`) type-name derivation uses. A leaf the dep does not own is a hard
/// error. `consumer`/`path`/`listed_name` are used only for actionable errors.
fn parse_requested_shape(
    types: &IntermediateTypes,
    shape: &str,
    consumer: &str,
    path: &str,
    listed_name: &str,
) -> Result<RustType, String> {
    ShapeParser::new(ShapeRow {
        consumer,
        path,
        shape,
        listed_name,
    })
    .parse(types)
}

/// Depth cap for [`ShapeParser::fragment`]'s recursion. Real wrapper shapes nest 2–3 deep; 32 is a
/// generous ceiling that turns a pathological hand-edited sidecar (thousands of `[* [* …]]` levels)
/// into an actionable hard error instead of a stack-overflow abort.
const MAX_SHAPE_DEPTH: usize = 32;

/// Read the occurrence grammar emitted by [`render_wrapper_shape`], advancing past it. Shared with
/// the lenient key-seed scan (`wrapper_requests::map_key_cddl_idents`) so the two readers of the
/// shape column cannot drift on which occurrences exist.
pub(crate) fn read_occurrence(chars: &[char], pos: &mut usize) -> Option<IntWindow> {
    match chars.get(*pos) {
        Some('+') => {
            *pos += 1;
            Some((Some(1), None))
        }
        Some('?') => {
            *pos += 1;
            Some((None, Some(1)))
        }
        Some('*') => {
            *pos += 1;
            let max = read_occurrence_number(chars, pos)?;
            Some((None, max))
        }
        Some(c) if c.is_ascii_digit() => {
            let min = read_occurrence_number(chars, pos)?;
            if chars.get(*pos) != Some(&'*') {
                return None;
            }
            *pos += 1;
            let max = read_occurrence_number(chars, pos)?;
            Some((min, max))
        }
        _ => None,
    }
}

/// Consume an optional non-negative endpoint. `None` means no digits, not an invalid number.
fn read_occurrence_number(chars: &[char], pos: &mut usize) -> Option<Option<i128>> {
    let start = *pos;
    while chars.get(*pos).is_some_and(|c| c.is_ascii_digit()) {
        *pos += 1;
    }
    if start == *pos {
        return Some(None);
    }
    chars[start..*pos]
        .iter()
        .collect::<String>()
        .parse::<i128>()
        .ok()
        .map(Some)
}

/// The owner-INDEPENDENT structural wrapper name for a reconstructed requested shape — the exact
/// spelling the consumer's emitter passed to `try_defer_wrapper` and recorded in its sidecar. Uses
/// the raw `NonEmpty*List` / `NonEmpty<MapKToV>` forms (NOT `non_empty_wasm_wrapper_name`, which
/// consults named owners) so a dep that authored a `[+ …]` rule surfaces as a name↔shape/own-spec
/// disagreement rather than silently matching. Refuses (as an `Err`) a non-collection top level
/// (a hand-edited sidecar row).
fn requested_structural_name(
    types: &IntermediateTypes,
    rt: &RustType,
    consumer: &str,
    path: &str,
) -> Result<String, String> {
    Ok(match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if rt.is_bounded_reject_ordered_set() {
                rt.bounded_reject_ordered_set_wasm_wrapper_name(types)
            } else if rt.is_reject_ordered_set() {
                // The uniqueness twin's wasm class name (`<Elem>OrderedSet` /
                // `NonEmpty<Elem>OrderedSet`) — the same spelling the dep mints locally, so a request
                // for it resolves to (or subtracts against) the identical structural name.
                rt.reject_ordered_set_wasm_wrapper_name(types)
            } else if rt.is_non_empty_array() {
                format!(
                    "NonEmpty{}List",
                    inner.wasm_boundary_identity_fragment(types)
                )
            } else if rt
                .exact_homogeneous_array_u64_bounds()
                .or_else(|| rt.bounded_array_u64_bounds())
                .is_some()
            {
                rt.bounded_wasm_array_structural_name(types)
            } else {
                inner.name_as_wasm_array(types)
            }
        }
        ConceptualRustType::Map(k, v) => {
            // Owner-independent requested names use the same recursive RustType structural owner
            // as the emitter; named owners are intentionally not consulted here.
            let preserve = rt.is_preserve_pair_map();
            if rt.is_bounded_map() {
                rt.bounded_wasm_map_structural_name(types)
            } else if rt.is_non_empty_map() {
                format!(
                    "NonEmpty{}",
                    RustType::wasm_structural_map_name_for(k, v, preserve, types)
                )
            } else {
                rt.wasm_structural_map_name(types).to_string()
            }
        }
        other => {
            return Err(format!(
                "--wrapper-requests {consumer} ({path}): a requested shape must be a collection \
                 wrapper (list or map), got {other:?}."
            ));
        }
    })
}

/// If a reconstructed requested shape is DIRECTLY WASM-EXPOSABLE (it lowers to a bare `Vec<…>` with
/// no wrapper class), return that member spelling; otherwise `None`. Mirrors `name_as_wasm_array_ct`'s
/// own exposability test exactly (rebuild `Array(inner)` and ask `directly_wasm_exposable_ct`) rather
/// than sniffing a rendered string. A `Map` top level is never directly exposable; a `[+ …]` NonEmpty
/// array always gets a wrapper class, so only the loose-array (`[* …]`) case can be exposable.
fn requested_exposable_member(types: &IntermediateTypes, rt: &RustType) -> Option<String> {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner)
            if !rt.is_non_empty_array()
                && !rt.is_bounded_array()
                && !rt.is_type_enforced_exact_homogeneous_array() =>
        {
            if ConceptualRustType::Array(Box::new(inner.conceptual_type.clone().into()))
                .directly_wasm_exposable_ct(types)
            {
                Some(inner.conceptual_type.name_as_wasm_array_ct(types))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Describe how this dep resolves each NAMED leaf element written in a requested shape's shape column,
/// for the actionable exposable-shape / name↔shape diagnostics. Walks the ORIGINAL shape tokens (not
/// the reconstructed `RustType`, which has already substituted `@no_alias` idents away) so the message
/// names the ident the operator wrote and its resolution target. Primitive leaves contribute nothing.
/// Only reached after a successful `parse_requested_shape`, so every named token is an owned,
/// non-reserved ident — `RustIdent::new` cannot trip.
fn requested_shape_leaf_resolutions(types: &IntermediateTypes, shape: &str) -> Vec<String> {
    let chars: Vec<char> = shape.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if is_shape_ident_char(chars[i]) {
            let start = i;
            while i < chars.len() && is_shape_ident_char(chars[i]) {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            if token.bytes().all(|byte| byte.is_ascii_digit()) {
                continue;
            }
            if primitive_from_cddl_name(&token).is_some() {
                continue;
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            out.push(describe_leaf_resolution(types, &token, &ident));
        } else {
            i += 1;
        }
    }
    out
}

/// One leaf's resolution phrase: a registered struct, a kept alias (rust alias preserving the ident),
/// or a transparent (`@no_alias` / passthrough) substitution to its base. Consults `type_aliases()`,
/// the same table `ShapeParser::leaf` resolves through.
fn describe_leaf_resolution(types: &IntermediateTypes, token: &str, ident: &RustIdent) -> String {
    match types.type_aliases().get(&AliasIdent::Rust(ident.clone())) {
        Some(info) => {
            let target = render_wrapper_shape(&info.base_type);
            if info.emits_rust_alias() {
                format!("`{token}` (a kept alias resolving to `{target}`)")
            } else {
                format!("`{token}` (transparently substituted to `{target}`)")
            }
        }
        None => format!("`{token}` (a registered struct)"),
    }
}

/// The immediate nested collection shapes of a requested wrapper (canonical form), used for the
/// inner-closure integrity check. Only ONE level: deeper nesting is covered
/// transitively because each level is a separately-requested (and separately-checked) entry.
fn inner_collection_shapes(rt: &RustType) -> Vec<String> {
    let is_collection = |rt: &RustType| {
        matches!(
            rt.conceptual_type,
            ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
        )
    };
    let mut out = Vec::new();
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if is_collection(inner) {
                out.push(render_wrapper_shape(inner));
            }
        }
        ConceptualRustType::Map(k, v) => {
            if is_collection(k) {
                out.push(render_wrapper_shape(k));
            }
            if is_collection(v) {
                out.push(render_wrapper_shape(v));
            }
        }
        _ => {}
    }
    out
}

/// Parse every `--extern-wrapper-index <dep>=<path>` file into `dep -> {wrapper class names}`. Each
/// file is a dependency's committed `generated/collections.rs`: `pub use <path>::<Name>;` lines (plus
/// blank / `//` comment lines). Any other non-blank line is a hard error — the format is ours, and a
/// silently-tolerated stray line would let a malformed index disable deferral and reintroduce the
/// duplicate-symbol link error. Mapping keys are validated against `extern_dep_names()` first (a typo
/// there has the same silent-disable failure mode), mirroring `--extern-wasm-crate`.
pub(super) fn load_extern_wrapper_indices(
    types: &IntermediateTypes,
    cli: &Cli,
) -> Result<BTreeMap<String, BTreeSet<String>>, String> {
    let files = cli.extern_wrapper_index_files();
    if files.is_empty() {
        return Ok(BTreeMap::new());
    }
    let extern_dep_names = types.extern_dep_names();
    let mut out = BTreeMap::new();
    for (dep, path) in files {
        if !extern_dep_names.contains(&dep) {
            return Err(format!(
                "--extern-wrapper-index names dependency {dep:?}, which is not an extern dependency \
                 in this spec. Known extern dependencies: {extern_dep_names:?}"
            ));
        }
        let contents = std::fs::read_to_string(&path).map_err(|e| {
            format!("--extern-wrapper-index {dep}={path}: cannot read the index file: {e}")
        })?;
        let mut names = BTreeSet::new();
        for line in contents.lines() {
            // The grammar's one owner is `wrapper_requests::classify_collection_index_line`; the
            // POLICY on an unrecognized line is this reader's own, and it is the strict one.
            match crate::wrapper_requests::classify_collection_index_line(line) {
                crate::wrapper_requests::CollectionIndexLine::Ignored => {}
                crate::wrapper_requests::CollectionIndexLine::Export(name) => {
                    names.insert(name);
                }
                crate::wrapper_requests::CollectionIndexLine::Unknown => {
                    return Err(format!(
                        "--extern-wrapper-index {dep}={path}: unexpected line {:?}; the index is a \
                         generated `collections.rs` of `pub use <path>::<Name>;` re-export lines",
                        line.trim()
                    ));
                }
            }
        }
        out.insert(dep, names);
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::{parse_requested_shape, render_wrapper_shape, requested_structural_name};
    use crate::{
        comment_ast::DuplicatesPolicy,
        intermediate::{ConceptualRustType, IntermediateTypes, Primitive, RustType},
    };

    /// Every refusal the requested-shape parser and the structural-name derivation raise for a
    /// hand-edited sidecar row is an `Err` carrying its diagnostic, never a panic: these run inside
    /// `generate`, whose error channel `main` prints as `Error: …` (exit 1) and `--config` wraps
    /// with the crates already regenerated.
    #[test]
    fn requested_shape_refusals_are_errors() {
        let types = IntermediateTypes::new();
        let deep = format!("{}uint{}", "[* ".repeat(64), "]".repeat(64));
        for (shape, expected) in [
            (
                "[* uint",
                "malformed shape \"[* uint\" (wrapper \"listed\"): expected `]`.",
            ),
            (
                "{* uint => uint} @duplicates reject",
                "`@duplicates reject` on the non-array shape",
            ),
            (
                "[* uint] @duplicates preserve",
                "`@duplicates preserve` on the non-map shape",
            ),
            ("[* uint] junk", "trailing content after the shape"),
            (deep.as_str(), "deeper than the supported limit of 32"),
            ("[* biguint]", "uses the reserved identifier \"biguint\""),
            (
                "[* nope]",
                "references the element type \"nope\", which this dep does not own",
            ),
        ] {
            let err = parse_requested_shape(&types, shape, "consumer", "sidecar", "listed")
                .expect_err(shape);
            assert!(
                err.starts_with("--wrapper-requests consumer (sidecar): ")
                    && err.contains(expected),
                "{shape:?} must be refused with {expected:?}, got: {err}"
            );
        }
        let leaf = parse_requested_shape(&types, "uint", "consumer", "sidecar", "listed").unwrap();
        let err = requested_structural_name(&types, &leaf, "consumer", "sidecar").unwrap_err();
        assert!(
            err.contains("a requested shape must be a collection wrapper (list or map)"),
            "a non-collection top level must be refused, got: {err}"
        );
    }

    #[test]
    fn requested_nested_preserve_map_reconstructs_emitter_structural_name() {
        let types = IntermediateTypes::new();
        let u64_type = || RustType::new(ConceptualRustType::Primitive(Primitive::U64));
        let bounded_array = RustType::new(ConceptualRustType::Array(Box::new(u64_type())))
            .with_bounds((None, Some(5)));
        let nested_preserve = RustType::new(ConceptualRustType::Map(
            Box::new(bounded_array),
            Box::new(u64_type()),
        ))
        .with_bounds((None, Some(3)))
        .with_duplicates_policy(Some(DuplicatesPolicy::Preserve));
        let emitted = RustType::new(ConceptualRustType::Map(
            Box::new(u64_type()),
            Box::new(nested_preserve),
        ))
        .with_duplicates_policy(Some(DuplicatesPolicy::Preserve));
        let shape = render_wrapper_shape(&emitted);
        let reconstructed =
            parse_requested_shape(&types, &shape, "consumer", "sidecar", "listed").unwrap();

        assert_eq!(
            requested_structural_name(&types, &reconstructed, "consumer", "sidecar").unwrap(),
            emitted.wasm_structural_map_name(&types).to_string(),
            "the real sidecar parser must preserve a nested bounded PairMap identity"
        );
        assert_eq!(
            emitted.wasm_structural_map_name(&types).to_string(),
            "PairMapU64ToPairMapU64ListMax5ToU64Max3"
        );
    }
}
