use super::*;

impl GenerationScope {
    /// W2 (`--wrapper-requests`): the attribution doc for `ident` as a paragraph PREFIX (trailing
    /// blank line) to prepend to an emitter-set struct doc, or `""` when the wrapper is not requested.
    /// Used by the NonEmpty emitters, whose `.doc()` call would otherwise clobber the attribution
    /// `create_base_wasm_struct` injects.
    pub(super) fn requested_attribution_prefix(&self, ident: &RustIdent) -> String {
        self.requested_attribution
            .get(ident)
            .map(|d| format!("{d}\n\n"))
            .unwrap_or_default()
    }

    /// W2 dep side (`--wrapper-requests`): read each consumer's committed `borrowed_collections.rs`,
    /// take the entries addressed to THIS dep (dep column == the normalized `--lib-name`), union the
    /// requested collection-wrapper shapes across consumers, and emit every requested wrapper the dep
    /// does not already produce into `wasm/src/generated/requested_collections.rs` (indexed via
    /// `record_collection_wrapper`, each carrying a sorted-requester attribution doc). Called once,
    /// after the own-spec wasm walk, under `--wasm`. A no-op — output byte-identical to today — when
    /// no `--wrapper-requests` flag is set (the module is not even created).
    ///
    /// Determinism: everything is keyed/sorted (`BTreeMap`/`BTreeSet`), so the union and the emission
    /// order depend on neither the flag order nor the consumers' regen order.
    pub(super) fn emit_requested_collections(&mut self, types: &IntermediateTypes, cli: &Cli) {
        let request_files = cli.wrapper_requests();
        if request_files.is_empty() {
            // No flag => no file, byte-identical to today (acceptance criterion 10 analog).
            return;
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
            let contents = std::fs::read_to_string(path).unwrap_or_else(|e| {
                panic!("--wrapper-requests {consumer}={path}: cannot read the sidecar: {e}")
            });
            let entries = crate::wrapper_requests::parse_sidecar(&contents, path);
            for entry in entries {
                // Entries addressed to OTHER deps (dep column != this crate's normalized lib name)
                // are silently skipped — a shared sidecar can name several deps.
                if entry.dep.replace('-', "_") != my_lib {
                    continue;
                }
                let rt = parse_requested_shape(types, &entry.shape, consumer, path, &entry.name);
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
                    panic!(
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
                    );
                }
                let canonical = render_wrapper_shape(&rt);
                let structural = requested_structural_name(types, &rt, consumer, path);
                // Cross-check the derived structural name against the listed name (criterion 8 #2).
                if structural != entry.name {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        String::new()
                    } else {
                        format!(" Element resolution in this dep: {}.", leaves.join(", "))
                    };
                    panic!(
                        "--wrapper-requests {consumer} ({path}): the borrowed wrapper listed as \
                         {:?} with shape {:?} derives the structural name {:?}, not {:?} — the \
                         sidecar's name and shape columns disagree (a name↔shape mismatch).{leaf_note}",
                        entry.name, entry.shape, structural, entry.name
                    );
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
                panic!(
                    "--wrapper-requests: two distinct requested shapes derive the same structural \
                     wrapper name {structural:?}: {shapes:?} (requested by {requesters:?}). These \
                     would define one JS class for two concepts — rename or @name one of the shapes \
                     in the requesting consumers."
                );
            }
        }

        // Decide, per unioned shape, whether the dep already produces it (skip), produces it under a
        // different rule name (criterion 8 #3, hard error), or must emit it.
        let mut to_emit: Vec<(String, RustType, String, Vec<String>)> = Vec::new();
        for (canonical, u) in &union {
            match self.own_wrapper_shapes.get(canonical) {
                // Own spec already produces this shape under the STRUCTURAL name => request satisfied
                // by the existing indexed wrapper; emit nothing.
                Some(existing) if existing.as_ref() == u.structural => {}
                // Own spec produces this shape under a DIFFERENT (rule-declared) name => hard error.
                Some(existing) => {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} (requested by {:?}) is \
                         already produced by this dep's own spec under the non-structural rule name \
                         {existing}, not the structural name {:?} the consumers import. Emitting \
                         both would create two JS classes for one concept. Remedy: rename the rule \
                         {existing} to {}, give it `@name {}`, or drop it.",
                        u.requesters, u.structural, u.structural, u.structural
                    );
                }
                None => {
                    let mut requesters: Vec<String> = u.requesters.iter().cloned().collect();
                    requesters.sort();
                    to_emit.push((
                        canonical.clone(),
                        u.rt.clone(),
                        u.structural.clone(),
                        requesters,
                    ));
                }
            }
        }

        // Criterion 8 #5: a requested NESTED shape whose inner collection wrapper is neither requested
        // nor own-spec-produced — an integrity check against a hand-edited / truncated sidecar (a real
        // consumer closes over its nested shapes automatically, so the inner should always be present).
        for (canonical, rt, _, _) in &to_emit {
            for inner in inner_collection_shapes(rt) {
                let requested = union.contains_key(&inner);
                let own = self.own_wrapper_shapes.contains_key(&inner);
                if !requested && !own {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} nests the collection \
                         wrapper {inner:?}, which is neither requested by any consumer nor produced \
                         by this dep's own spec. The inner collection of an all-one-dep shape is \
                         itself all-one-dep and must be requested too — this sidecar looks truncated \
                         or hand-edited."
                    );
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
        for (_, _, structural, requesters) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
            self.requested_attribution.insert(
                ident,
                format!("Generated at the request of: {}.", requesters.join(", ")),
            );
        }
        self.requested_scope_override = Some(requested_scope.clone());
        for (_, rt, structural, _) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
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
                    } else {
                        self.generate_array_type(types, (**inner).clone(), &ident, false, cli);
                    }
                }
                ConceptualRustType::Map(k, v) => {
                    if rt.is_non_empty_map() {
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
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
                            cli,
                        );
                    }
                }
                other => unreachable!("requested shape is not a collection: {other:?}"),
            }
        }
        self.requested_scope_override = None;

        // A requested NonEmpty wrapper pulls in the NonEmpty runtime the dep's OWN spec may not use;
        // record it so the runtime-provisioning gates (mod decl + static file copy) fire, and import
        // the type into this scope explicitly (the per-scope loop's import gate is keyed off the dep's
        // own IR, which doesn't see the requested wrappers).
        self.requested_non_empty_vec = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_array());
        self.requested_non_empty_map = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_map());
        // A requested reject wrapper pulls in the `ordered_set` runtime the dep's OWN spec may not
        // use; record it so the runtime-provisioning gates (mod decl + static file copy) fire.
        self.requested_ordered_set = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_ordered_set());
        let non_empty_import = self
            .requested_non_empty_vec
            .then(|| format!("{}::non_empty", cli.common_import_wasm()));
        let non_empty_map_import = self
            .requested_non_empty_map
            .then(|| format!("{}::non_empty_map", cli.common_import_wasm()));
        let ordered_set_import = self
            .requested_ordered_set
            .then(|| format!("{}::ordered_set", cli.common_import_wasm()));

        // Ensure the module exists even when nothing is emitted (all requests satisfied by own spec /
        // addressed elsewhere) — stable presence, stable diffs (plan decision 1). When non-empty, the
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
        if let Some(path) = non_empty_map_import {
            scope_content.push_import(path, "NonEmptyMap", None);
        }
        // The reject twin wraps `core::OrderedSet` / `NonEmptyOrderedSet`; the per-scope import loop
        // gates these on the dep's OWN `uses_ordered_set()`, so a dep hosting ONLY a requested reject
        // wrapper needs them pushed here (same dumb-push + central-prune contract as the twins above).
        if let Some(path) = ordered_set_import {
            scope_content.push_import(path.clone(), "OrderedSet", None);
            scope_content.push_import(path, "NonEmptyOrderedSet", None);
        }
    }
}

/// The CDDL prelude spelling of a primitive, for the canonical shape renderer. Kept in lockstep with
/// the wasm-map/list structural naming: the dep re-parses a rendered shape and must derive the SAME
/// structural name, so each primitive renders to a CDDL name whose `for_variant` round-trips (e.g.
/// `uint` -> `U64` -> `MapU64To…`). `u8`/`i8`/… are cddl-codegen's own sized-int spellings.
fn primitive_cddl_name(p: &Primitive) -> &'static str {
    match p {
        Primitive::Bool => "bool",
        Primitive::F64 => "float64",
        Primitive::F32 => "float32",
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

/// Render a collection wrapper's CDDL shape fragment in the canonical W1 shape-column grammar —
/// `[* foo]` / `[+ foo]` for loose / non-empty lists, `{* k => v}` / `{+ k => v}` for maps, nesting
/// recursively (`[* [* foo]]`, `[* [+ foo]]`). Element idents are the dependency's own spec spelling
/// (snake_case of the rust ident, matching the extern-stub naming a dep re-parses after
/// normalization); primitives render as their CDDL prelude name. The occurrence marker is taken from
/// the `RustType`'s own bounds so nested non-empty shapes are honored at every level. This is the
/// single shape renderer shared by the not-in-index warning hint and (later) the request-sidecar
/// machinery, so its output is EXACTLY the format a dep parses back.
pub(crate) fn render_wrapper_shape(rt: &RustType) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            let occ = if rt.is_non_empty_array() { "+" } else { "*" };
            // A `@duplicates reject` collection appends its policy marker so the shape column
            // round-trips the uniqueness twin (parsed back by `parse_requested_shape`, and matched as
            // a distinct canonical shape from the same loose/non-empty list). Kept byte-identical to
            // the marker `generate_reject_ordered_set_type` records for the dep's own reject wrappers.
            let reject = if rt.duplicates_reject() {
                " @duplicates reject"
            } else {
                ""
            };
            format!("[{occ} {}]{reject}", render_wrapper_shape(inner))
        }
        ConceptualRustType::Map(key, value) => {
            let occ = if rt.is_non_empty_map() { "+" } else { "*" };
            format!(
                "{{{occ} {} => {}}}",
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
        // Fixed values carry no CDDL ident and never appear as a real wrapper element; render a
        // placeholder rather than panicking so the advisory hint text stays best-effort.
        ConceptualRustType::Fixed(_) => "_".to_owned(),
    }
}

/// Validate `--workspace-dep` values (plan decision 6) and return the set. Each named dep must be a
/// configured extern dependency (`extern_dep_names()`) AND have an `--extern-wasm-crate` mapping —
/// the deferral imports and the sidecar's `use` lines both need the wasm crate name, so a missing
/// mapping is a hard error rather than a silent fallback. Mirrors `load_extern_wrapper_indices`'
/// startup hardening. The accessor already rejected empty / `=`-bearing values.
pub(super) fn load_workspace_deps(types: &IntermediateTypes, cli: &Cli) -> BTreeSet<String> {
    let deps = cli.workspace_deps();
    if deps.is_empty() {
        return BTreeSet::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let wasm_crate_map = cli.extern_wasm_crate_map();
    for dep in &deps {
        if !extern_dep_names.contains(dep) {
            panic!(
                "--workspace-dep names dependency {dep:?}, which is not an extern dependency in this \
                 spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        if !wasm_crate_map.contains_key(dep) {
            panic!(
                "--workspace-dep {dep:?} has no --extern-wasm-crate mapping; workspace deferral needs \
                 the dep's wasm crate name for its imports and the borrowed-collections sidecar. Add \
                 --extern-wasm-crate {dep}=<wasm_crate>."
            );
        }
    }
    deps
}

// ===== W2 dep side (`--wrapper-requests`): shape reconstruction + structural naming ===============

/// Reverse of `primitive_cddl_name`: the `Primitive` a shape-column leaf denotes, or `None` for a
/// named-type leaf. Only the exact spellings `render_wrapper_shape` emits for primitive leaves are
/// recognized, so a dep type whose snake-case happens NOT to be a prelude name is correctly treated
/// as a named element.
fn primitive_from_cddl_name(name: &str) -> Option<Primitive> {
    Some(match name {
        "bool" => Primitive::Bool,
        "float64" => Primitive::F64,
        "float32" => Primitive::F32,
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

/// Reconstruct a requested wrapper's `RustType` from its canonical shape column, resolving each
/// named leaf against the DEP's own IR after the same normalization (`RustIdent::new`, which
/// camel-cases and folds `-`/`_`) type-name derivation uses. A leaf the dep does not own is a hard
/// error (criterion 8 #1). `consumer`/`path`/`listed_name` are threaded only for actionable errors.
fn parse_requested_shape(
    types: &IntermediateTypes,
    shape: &str,
    consumer: &str,
    path: &str,
    listed_name: &str,
) -> RustType {
    let chars: Vec<char> = shape.chars().collect();
    let mut pos = 0;
    let mut rt = parse_shape_fragment(
        types,
        &chars,
        &mut pos,
        consumer,
        path,
        shape,
        listed_name,
        0,
    );
    while pos < chars.len() && chars[pos].is_whitespace() {
        pos += 1;
    }
    // A `@duplicates reject` collection carries its policy in the shape column as a trailing marker
    // (the exact spelling `render_wrapper_shape` emits — the sidecar round-trips it), so the dep
    // rebuilds the SAME uniqueness twin the consumer borrowed. Consume it before the trailing-content
    // guard and stamp the policy onto the reconstructed `RustType` (only an array-shaped collection
    // ever carries it; the emit dispatch + structural naming key off `is_reject_ordered_set`).
    const REJECT_MARKER: &str = "@duplicates reject";
    let rest: String = chars[pos..].iter().collect();
    if rest == REJECT_MARKER {
        if !matches!(rt.conceptual_type, ConceptualRustType::Array(_)) {
            panic!(
                "--wrapper-requests {consumer} ({path}): `@duplicates reject` on the non-array shape \
                 {shape:?} (wrapper {listed_name:?}) — the reject policy only applies to set/array \
                 collections."
            );
        }
        rt.config.duplicates = Some(crate::comment_ast::DuplicatesPolicy::Reject);
        pos = chars.len();
    }
    if pos != chars.len() {
        panic!(
            "--wrapper-requests {consumer} ({path}): trailing content after the shape {shape:?} \
             (wrapper {listed_name:?})."
        );
    }
    rt
}

/// Depth cap for `parse_shape_fragment`'s recursion. Real wrapper shapes nest 2–3 deep; 32 is a
/// generous ceiling that turns a pathological hand-edited sidecar (thousands of `[* [* …]]` levels)
/// into an actionable hard error instead of a stack-overflow abort.
const MAX_SHAPE_DEPTH: usize = 32;

#[allow(clippy::too_many_arguments)]
fn parse_shape_fragment(
    types: &IntermediateTypes,
    chars: &[char],
    pos: &mut usize,
    consumer: &str,
    path: &str,
    shape: &str,
    listed_name: &str,
    depth: usize,
) -> RustType {
    let skip_ws = |pos: &mut usize| {
        while *pos < chars.len() && chars[*pos].is_whitespace() {
            *pos += 1;
        }
    };
    let bad = |what: &str| -> ! {
        panic!(
            "--wrapper-requests {consumer} ({path}): malformed shape {shape:?} (wrapper \
             {listed_name:?}): {what}."
        );
    };
    if depth > MAX_SHAPE_DEPTH {
        panic!(
            "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
             (shape {shape:?}) nests collections deeper than the supported limit of \
             {MAX_SHAPE_DEPTH}. Real wrapper shapes nest only a few levels; this is almost \
             certainly a malformed hand-edited sidecar."
        );
    }
    skip_ws(pos);
    if *pos >= chars.len() {
        bad("unexpected end of shape");
    }
    match chars[*pos] {
        '[' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let inner = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != ']' {
                bad("expected `]`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Array(Box::new(inner)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        '{' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let key = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if !(chars.get(*pos) == Some(&'=') && chars.get(*pos + 1) == Some(&'>')) {
                bad("expected `=>`");
            }
            *pos += 2;
            skip_ws(pos);
            let value = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != '}' {
                bad("expected `}`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Map(Box::new(key), Box::new(value)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        _ => {
            // A named or primitive leaf: read the ident token.
            let start = *pos;
            while *pos < chars.len()
                && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
            {
                *pos += 1;
            }
            if *pos == start {
                bad("expected an element type name");
            }
            let token: String = chars[start..*pos].iter().collect();
            if let Some(p) = primitive_from_cddl_name(&token) {
                return RustType::new(ConceptualRustType::Primitive(p));
            }
            // A reserved CDDL keyword (`biguint`, `bigint`, …) or reserved Rust type name
            // (`option` → `Option`) as a leaf token would trip `RustIdent::new`'s internal asserts
            // — an internal panic reachable only from a hand-edited sidecar (a real consumer never
            // emits these). Pre-check through the reservation rule's one owner
            // (`RustIdent::reserved_reason`, the same predicate `new` asserts on) so external
            // input surfaces the feature's own hard error instead of the assert.
            if RustIdent::reserved_reason(&token).is_some() {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) uses the reserved identifier {token:?} as a wrapper element; \
                     reserved CDDL keywords and reserved Rust type names cannot be wrapper elements."
                );
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            if !dep_owns_element(types, &ident) {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) references the element type {token:?}, which this dep does not \
                     own. The consumer's extern stub for this dep and the dep's own spec disagree — \
                     the request cannot be satisfied."
                );
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
            types
                .resolve_alias(&AliasIdent::Rust(ident.clone()))
                .unwrap_or_else(|| RustType::new(ConceptualRustType::Rust(ident)))
        }
    }
}

/// Read a `*`/`+` occurrence marker at `chars[*pos]`, advancing past it.
fn read_occurrence(chars: &[char], pos: &mut usize) -> Option<char> {
    match chars.get(*pos) {
        Some('*') => {
            *pos += 1;
            Some('*')
        }
        Some('+') => {
            *pos += 1;
            Some('+')
        }
        _ => None,
    }
}

/// The owner-INDEPENDENT structural wrapper name for a reconstructed requested shape — the exact
/// spelling the consumer's emitter passed to `try_defer_wrapper` and recorded in its sidecar. Uses
/// the raw `NonEmpty*List` / `NonEmpty<MapKToV>` forms (NOT `non_empty_wasm_wrapper_name`, which
/// consults named owners) so a dep that authored a `[+ …]` rule surfaces as a name↔shape/own-spec
/// disagreement rather than silently matching. Panics for a non-collection top level (a hand-edited
/// sidecar row).
fn requested_structural_name(
    types: &IntermediateTypes,
    rt: &RustType,
    consumer: &str,
    path: &str,
) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if rt.is_reject_ordered_set() {
                // The uniqueness twin's wasm class name (`<Elem>OrderedSet` /
                // `NonEmpty<Elem>OrderedSet`) — the same spelling the dep mints locally, so a request
                // for it resolves to (or subtracts against) the identical structural name.
                rt.reject_ordered_set_wasm_wrapper_name(types)
            } else if rt.is_non_empty_array() {
                format!("NonEmpty{}List", inner.conceptual_type.for_variant())
            } else {
                inner.conceptual_type.name_as_wasm_array_ct(types)
            }
        }
        ConceptualRustType::Map(k, v) => {
            if rt.is_non_empty_map() {
                format!("NonEmpty{}", ConceptualRustType::name_for_wasm_map(k, v))
            } else {
                ConceptualRustType::name_for_wasm_map(k, v).to_string()
            }
        }
        other => panic!(
            "--wrapper-requests {consumer} ({path}): a requested shape must be a collection wrapper \
             (list or map), got {other:?}."
        ),
    }
}

/// If a reconstructed requested shape is DIRECTLY WASM-EXPOSABLE (it lowers to a bare `Vec<…>` with
/// no wrapper class), return that member spelling; otherwise `None`. Mirrors `name_as_wasm_array_ct`'s
/// own exposability test exactly (rebuild `Array(inner)` and ask `directly_wasm_exposable_ct`) rather
/// than sniffing a rendered string. A `Map` top level is never directly exposable; a `[+ …]` NonEmpty
/// array always gets a wrapper class, so only the loose-array (`[* …]`) case can be exposable.
fn requested_exposable_member(types: &IntermediateTypes, rt: &RustType) -> Option<String> {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) if !rt.is_non_empty_array() => {
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
        if chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-' {
            let start = i;
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-')
            {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
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
/// the same table `parse_shape_fragment`'s leaf arm resolves through.
fn describe_leaf_resolution(types: &IntermediateTypes, token: &str, ident: &RustIdent) -> String {
    match types.type_aliases().get(&AliasIdent::Rust(ident.clone())) {
        Some(info) => {
            let target = render_wrapper_shape(&info.base_type);
            if info.gen_rust_alias {
                format!("`{token}` (a kept alias resolving to `{target}`)")
            } else {
                format!("`{token}` (transparently substituted to `{target}`)")
            }
        }
        None => format!("`{token}` (a registered struct)"),
    }
}

/// The immediate nested collection shapes of a requested wrapper (canonical form), used for the
/// inner-closure integrity check (criterion 8 #5). Only ONE level: deeper nesting is covered
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
) -> BTreeMap<String, BTreeSet<String>> {
    let files = cli.extern_wrapper_index_files();
    if files.is_empty() {
        return BTreeMap::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let mut out = BTreeMap::new();
    for (dep, path) in files {
        if !extern_dep_names.contains(&dep) {
            panic!(
                "--extern-wrapper-index names dependency {dep:?}, which is not an extern dependency \
                 in this spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        let contents = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("--extern-wrapper-index {dep}={path}: cannot read the index file: {e}")
        });
        let mut names = BTreeSet::new();
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            // Fixed shape: `pub use <path>::<Name>;` — take the segment after the last `::`.
            let name = line
                .strip_prefix("pub use ")
                .and_then(|rest| rest.strip_suffix(';'))
                .and_then(|path| path.rsplit("::").next())
                .filter(|name| {
                    !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_')
                });
            match name {
                Some(name) => {
                    names.insert(name.to_owned());
                }
                None => panic!(
                    "--extern-wrapper-index {dep}={path}: unexpected line {line:?}; the index is a \
                     generated `collections.rs` of `pub use <path>::<Name>;` re-export lines"
                ),
            }
        }
        out.insert(dep, names);
    }
    out
}
