use super::*;

impl GenerationScope {
    /// Record that a collection-wrapper class `ident` was just emitted, for the
    /// `wasm/src/generated/collections.rs` re-export index. Called from each of the four wrapper
    /// emitters right after their shared `already_generated` guard admits the mint, so the index
    /// captures every wrapper class exactly once and never a suppressed one. The recorded
    /// `ModuleScope` is `types.scope(ident)` — the SAME scope `wasm(types, ident)` places the class
    /// in — so the index path derives from the class's real emission location.
    fn record_collection_wrapper(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
        shape: &str,
    ) {
        // The recorded scope is where the class is actually emitted: the requested-collections
        // override when active (so the index re-exports it from that module), else `types.scope`.
        let scope = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).clone(),
        };
        self.wasm_collection_wrappers.insert(ident.clone(), scope);
        // W2 (`--wrapper-requests`): index this crate's OWN collection-wrapper shapes (main walk only,
        // never the requested wrappers being minted under the override) so a dep can tell whether it
        // already produces a requested shape, and under what name.
        if self.requested_scope_override.is_none() {
            self.own_wrapper_shapes
                .insert(shape.to_owned(), ident.clone());
        }
        // The one seam every LOCAL wrapper mint passes, which is why the index-shadowing honesty
        // check lives here rather than on any individual `try_defer_wrapper` decline.
        self.warn_local_mint_shadows_index(ident, shape);
    }

    /// Record that structural wrapper `ident` was deferred to workspace dependency `dep` this run
    /// (`--workspace-dep`), for the `wasm/src/generated/borrowed_collections.rs` sidecar. Idempotent:
    /// the same wrapper is probed from several sites (the loose emitter, a keys-list, a NonEmpty
    /// try_from source), each recording the same `(dep, shape)`. Two DISTINCT shapes deriving the
    /// SAME structural name — the `MapAToBToC` reverse-ambiguity (`{* a => b_to_c}` vs
    /// `{* a_to_b => c}`) — is a hard error naming both shapes: today that pair already fails rustc
    /// (two same-named local mints), so this upgrades a compile failure into an actionable diagnostic.
    pub(crate) fn record_borrowed_wrapper(&mut self, ident: &RustIdent, dep: &str, shape: &str) {
        if let Some((_, existing_shape)) = self.borrowed_wrappers.get(ident)
            && existing_shape != shape
        {
            panic!(
                "two distinct shapes in this crate's spec derive the same borrowed collection wrapper \
                 name {ident}: {existing_shape:?} and {shape:?}. These would define one JS class for \
                 two concepts — rename or @name one of them."
            );
        }
        self.borrowed_wrappers
            .insert(ident.clone(), (dep.to_owned(), shape.to_owned()));
    }

    /// Decide whether a structural collection wrapper the consumer is about to mint should instead be
    /// DEFERRED to a dependency that already owns it (`--extern-wrapper-index`). `structural_name` is
    /// the wrapper's structurally-derived name (`name_as_wasm_array` / `name_for_wasm_map`) and
    /// `constituents` its element (list) or key+value (map) conceptual types.
    ///
    /// Returns `true` when the wrapper is deferred — the caller must emit NO local class and skip
    /// `record_collection_wrapper`, so the deferred wrapper leaves the crate's own `collections.rs`
    /// index (R3e). The ident is recorded in `deferred_wrappers` mapped to the dependency's
    /// `collections` module scope, so `scope_references` routes a plain
    /// `use <dep_wasm>::collections::<Name>;` into every referencing module (R3b) and the keys()
    /// accessors construct via `.into()` cross-crate (R3d). Returns `false` (mint locally) when: the
    /// flag is unused; the ident is not the structural name of these constituents (a rule-declared
    /// wrapper — never suppressed); the constituents are mixed / not all one dependency (R3c); or an
    /// all-extern-of-one-dep candidate is absent from that dep's index (local + one stderr warning
    /// naming the wrapper).
    ///
    /// The first three `false` arms are silent HERE and stay so: each is a correct verdict about the
    /// DEFERRAL. Whether the name the resulting class is emitted under collides with a dependency's
    /// index is a separate question, answered once for every local mint by
    /// `warn_local_mint_shadows_index` at the `record_collection_wrapper` seam.
    #[allow(clippy::too_many_arguments)]
    fn try_defer_wrapper(
        &mut self,
        types: &IntermediateTypes,
        wrapper_ident: &RustIdent,
        structural_name: &str,
        constituents: &[&ConceptualRustType],
        // The wrapper's CDDL shape fragment (canonical renderer output), used to build the paste-able
        // "add this rule" hint on the not-in-index warning AND recorded in the workspace sidecar.
        shape: &str,
        // `true` when this mint request comes from an explicit RULE declaration (`foo_list = [* foo]`
        // reached via the `RustStruct::{Array,Table}` variant arms) rather than a synthesized/inline
        // wrapper. Only meaningful when the rule's ident coincides with the structural name (the
        // common `name != structural` case is already screened below); in workspace mode a
        // rule-declared wrapper is the consumer's OWN class and must NEVER defer — instead it triggers
        // the shadowing warning (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) -> bool {
        // Only structural-named wrappers are defer candidates: a rule-declared wrapper
        // (`foo_list = [* extern_foo]`) whose ident DIFFERS from the structural name is the consumer's
        // OWN class and is never suppressed. (A rule whose ident COINCIDES with the structural name
        // passes this guard; workspace mode distinguishes it via `rule_declared` just below.)
        // Hoisted above the flag fast-out below because the `@extern_companions` arm is FLAGLESS and
        // needs the same screen; both paths returned `false` here either way, so hoisting it changes
        // no output.
        if wrapper_ident.as_ref() != structural_name {
            return false;
        }
        // `@extern_companions` (no flag): the spec itself declares that this structural companion
        // class ALREADY EXISTS in a sibling wasm crate, for a user-defined type marked LOCALLY —
        // either marker flavor, extern or raw-bytes, since this arm keys on the owning IDENT and
        // never on the struct variant (no dep
        // edge, so neither dependency-keyed mechanism below can reach it). Reference it instead of
        // minting a second `#[wasm_bindgen]` class of the same name — two such classes in one cdylib
        // are a `rust-lld: duplicate symbol __wbg_<class>_free`.
        //
        // Three things this arm deliberately does NOT do. It consults no index (there is none — the
        // sibling's class may be HAND-written, which is the reported case), so the not-in-index
        // warning below must not and does not fire for it; the machine check is the consumer's own
        // compile, since the emitted `use <prefix>::<Class>;` fails loudly and near if the class is
        // absent — the same trust-and-compile contract the extern marker itself has. It never
        // suppresses a RULE-declared class (`rule_declared`), matching the workspace arm's criterion
        // 9; a rule claiming a LISTED name is rejected in `IntermediateTypes::finalize` before
        // generation, so this is a belt-and-braces fall-through, not the diagnostic. And it defers
        // ONLY the listed names: an unlisted structural companion of the same extern still mints
        // locally, which is what lets a consumer borrow one family and own another.
        //
        // Flag-off byte-identity does NOT rest on the fast-out below (which this arm precedes) but on
        // the registry being empty for every spec that carries no directive — the directive is new
        // INPUT, so no existing spec can reach the body.
        if !rule_declared
            && !types.extern_companions().is_empty()
            && let Some(owner) = sole_named_leaf(constituents)
            && let Some(prefix) = types.extern_companion_path(&owner, structural_name)
        {
            // A non-exported scope, so `add_imports_from_scope_refs` emits the prefix as the `use`
            // head unrooted (an exported scope would be rooted at `crate::generated`). The same
            // routing the dependency-keyed arms use, with the path coming from the spec instead of a
            // dep edge — including that seam's `--extern-wasm-crate` remap, which rewrites the
            // LEADING component when it names a declared extern dependency. That is reachable here
            // only if the author spells a dep's RUST crate name as the companion path, where the
            // remap yields the dep's wasm crate — the path they meant — so it is left to apply
            // rather than special-cased away (documented on the directive).
            let mut components = vec![crate::parsing::EXTERN_DEPS_DIR.to_owned()];
            components.extend(prefix.split("::").map(str::to_owned));
            self.deferred_wrappers
                .insert(wrapper_ident.clone(), ModuleScope::from(components));
            return true;
        }
        // Fast out only when NEITHER deferral mechanism is active. (Flag-off byte-identity: with both
        // sets empty this is the same early `false` as before — the workspace branch below is dead
        // code, criterion 10.)
        if self.extern_wrapper_index.is_empty() && self.workspace_deps.is_empty() {
            return false;
        }
        // Workspace mode (`--workspace-dep`): an all-one-workspace-dep wrapper DEFERS UNCONDITIONALLY,
        // before any index consult. The placement decision is factored as one function over the
        // transitive element-owner set (plan decision 4: today "exactly one owner ∈ workspace deps →
        // Borrow"; "latest of the element owners" can replace this body later without touching call
        // sites). Ownerless / mixed-dep wrappers fall through to the shipped index/local logic below
        // (criterion 2). A rule-declared wrapper that would otherwise borrow is the consumer's own
        // class: warn (criterion 9) and fall through, never suppress it.
        if !self.workspace_deps.is_empty()
            && let WrapperPlacement::Borrow(dep) = wrapper_placement(
                &transitive_owner_set(types, constituents),
                &self.workspace_deps,
            )
        {
            if rule_declared {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    // The message asserts only what this guard checked. `wrapper_placement` is a pure
                    // function of the constituents' owner set and `--workspace-dep`; it consults no
                    // inventory of the dependency, so whether the dep exports a class of this name is
                    // unknown here. It does so only if its OWN spec produces the wrapper or some
                    // consumer's request sidecar asked it to mint one — and this rule-declared wrapper
                    // falls through WITHOUT `record_borrowed_wrapper`, so it never enters this
                    // consumer's `borrowed_collections.rs` and never becomes such a request. The
                    // type-identity split is therefore the unconditional consequence and the link
                    // collision the conditional one, phrased like the sibling not-in-index warning
                    // below ("a dep that later adds it would duplicate-symbol").
                    crate::warn!(
                        "warning: rule-declared type {structural_name} shadows the collection wrapper \
                         this crate would otherwise borrow from workspace dependency {dep:?}; the \
                         authored class is minted locally, so this crate's {structural_name} and the \
                         dependency's are DISTINCT types across the package boundary even though they \
                         are structurally identical (values cannot be passed between the two packages \
                         as that type), and if the dependency's wasm crate also exports that name — \
                         its own spec declaring it, or another consumer's request sidecar having asked \
                         it to mint one — the two duplicate-symbol at link. Remedy: rename the rule, \
                         or give it a distinct @name."
                    );
                }
                // fall through to the shipped behavior (never a workspace defer)
            } else {
                // Deferred to the workspace dep: record the borrow (idempotent; a same-name/different
                // -shape collision hard-errors inside) and route the import exactly like the index
                // branch does, so `scope_references` emits `use <dep_wasm>::collections::<Name>;`.
                self.record_borrowed_wrapper(wrapper_ident, &dep, shape);
                let dep_scope = ModuleScope::from(vec![
                    crate::parsing::EXTERN_DEPS_DIR.to_owned(),
                    dep,
                    "collections".to_owned(),
                ]);
                self.deferred_wrappers
                    .insert(wrapper_ident.clone(), dep_scope);
                return true;
            }
        }
        // Beyond this point is the shipped `--extern-wrapper-index` path (unchanged). It requires the
        // index; with only `--workspace-dep` set (no index) there is nothing more to do.
        if self.extern_wrapper_index.is_empty() {
            return false;
        }
        // Each named constituent (element / key / value that resolves to a named rule) maps to the
        // dependency owning it (leading component of its non-exported scope), or `None` when it's a
        // consumer-owned (exported) type. Primitives contribute no constituent.
        let mut constituent_deps: Vec<Option<String>> = Vec::new();
        for c in constituents {
            for id in named_constituent_idents(c) {
                let scope = types.scope(&id);
                constituent_deps.push(if scope.export() {
                    None
                } else {
                    scope.components().first().cloned()
                });
            }
        }
        let dep = if constituent_deps.is_empty() {
            // Zero named constituents (e.g. `MapU64ToText`): a defer candidate only if some configured
            // index lists the name. Several listing it would each be a duplicate-symbol link error, so
            // defer to the lexicographically-first dep (BTreeMap iteration order) and warn.
            let matching: Vec<&String> = self
                .extern_wrapper_index
                .iter()
                .filter(|(_, names)| names.contains(structural_name))
                .map(|(dep, _)| dep)
                .collect();
            match matching.as_slice() {
                [] => return false, // owned by no dependency -> local, silent
                [only] => (*only).clone(),
                many => {
                    if self.deferred_warned.insert(wrapper_ident.clone()) {
                        crate::warn!(
                            "warning: collection wrapper {structural_name} is listed in several \
                             --extern-wrapper-index files ({many:?}); deferring to the first ({})",
                            many[0]
                        );
                    }
                    many[0].clone()
                }
            }
        } else {
            // Has named constituents: a defer candidate only if they ALL resolve to extern types of
            // the SAME dependency (R3c: any consumer-owned or cross-dependency constituent -> local,
            // silent here — the mint-seam backstop is what speaks if the emitted NAME is dep-indexed).
            let mut single: Option<String> = None;
            for d in &constituent_deps {
                match d {
                    None => return false,
                    Some(name) => match &single {
                        None => single = Some(name.clone()),
                        Some(s) if s == name => {}
                        Some(_) => return false,
                    },
                }
            }
            let dep = single.unwrap();
            // All-extern-of-one-dep candidate: defer iff that dep's index lists it; otherwise mint
            // locally and warn once (a dep-side inventory change that silently shifted ownership back
            // to the consumer is then loud in the regen log, not only in the diff).
            if !self
                .extern_wrapper_index
                .get(&dep)
                .is_some_and(|names| names.contains(structural_name))
            {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    // Append the exact rule line to paste into the owning dep's spec: declaring it
                    // there lands the wrapper in the dep's collections.rs index (by construction), so
                    // every consumer's index-deferral then picks it up — the shipped manual override
                    // for wrappers no request sidecar covers (hand-written consumer code, mixed-dep
                    // shapes). Rule name = snake_case of the structural name; shape from the canonical
                    // renderer; requester = this consumer's normalized --lib-name.
                    let rule_name = convert_to_snake_case(structural_name);
                    let requester = cli.lib_name_code();
                    // A flavored shape (`@duplicates preserve` table / `@duplicates reject` set)
                    // carries its policy marker BARE in the shape column, which the sidecar
                    // round-trips by parse. A pasted RULE line is CDDL, where a bare marker after
                    // the shape does not parse — and dropping it silently would mint the
                    // WRONG-FLAVOR wrapper under a name that encodes the flavor. So the marker moves
                    // into comment position, where it is the rule's `@duplicates` directive, and the
                    // requester attribution follows it in `@doc` (the DSL's own slot for prose after
                    // a directive). An unflavored shape keeps the plain-prose comment unchanged.
                    let (bare_shape, policy_marker) =
                        crate::generation::requests::split_shape_policy_marker(shape);
                    let rule_line = match policy_marker {
                        Some(marker) => format!(
                            "{rule_name} = {bare_shape} ; {marker} @doc requested by {requester}"
                        ),
                        None => format!("{rule_name} = {bare_shape} ; requested by {requester}"),
                    };
                    crate::warn!(
                        "warning: collection wrapper {structural_name} has only extern elements of \
                         dependency {dep:?} but is absent from its --extern-wrapper-index; minting \
                         it locally (a dep that later adds it would duplicate-symbol at link time)\n\
                         hint: add to {dep}'s spec: {rule_line}"
                    );
                }
                return false;
            }
            dep
        };
        // A RULE-declared wrapper reaching this point had its ident coincide with the structural
        // name (screened at the top) AND that name is listed in `dep`'s index — so the class the
        // rule authored is not minted here at all, and every wasm-side reference to the rule
        // resolves to the DEPENDENCY's class. That unification is the consumer's to accept or
        // refuse, so it is stated rather than performed silently. The message asserts only what
        // this branch checked: the coincidence, the listing, and what the deferral does with them.
        // It deliberately does not claim the two types are structurally identical — the index is
        // name-only and carries no shape column (unlike the workspace sidecar), so nothing here
        // has compared them; the consumer's own wasm build is what checks that.
        if rule_declared && self.deferred_warned.insert(wrapper_ident.clone()) {
            crate::warn!(
                "warning: rule-declared type {structural_name} names the collection wrapper \
                 dependency {dep:?} lists in its --extern-wrapper-index; the authored class is NOT \
                 minted here, so on the wasm surface this rule and the dependency's class are \
                 UNIFIED — every reference to the rule resolves to {dep}'s {structural_name} (the \
                 rust-side `pub type {structural_name}` this rule declares is kept). Remedy, if \
                 this rule was meant to be a DIFFERENT type that merely shares the name: rename the \
                 rule, or give it a distinct @name."
            );
        }
        // Deferred: import from the dep's `collections` module. The non-exported scope
        // `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/collections` is remapped by
        // `add_imports_from_scope_refs` to `<dep_wasm>::collections` when `--extern-wasm-crate` maps
        // the dep, or left as `<dep>::collections` (the dep's rust crate name — the same fallback
        // unmapped extern types get) otherwise.
        let dep_scope = ModuleScope::from(vec![
            crate::parsing::EXTERN_DEPS_DIR.to_owned(),
            dep,
            "collections".to_owned(),
        ]);
        self.deferred_wrappers
            .insert(wrapper_ident.clone(), dep_scope);
        true
    }

    /// The one dep-indexed-name configuration `try_defer_wrapper` never sees: a RULE-declared LOOSE
    /// table. Its class is minted through the `exists_in_rust` path
    /// (`mint_sole_owner_table` / `codegen_table_type(exists_in_rust = true)`), which reaches no
    /// defer seam — so the consumer keeps its own class under the rule's ident while a mapped
    /// dependency's index lists that same name, and the two are duplicate `#[wasm_bindgen]` symbols
    /// the moment both crates link into one cdylib. Nothing in the emitted output says so, so this
    /// says it on stderr.
    ///
    /// Keyed on the RULE IDENT because that is the emitted CLASS name: a rule whose ident DIFFERS
    /// from the structural name exposes the structural name as a `pub type` alias instead (see
    /// `mint_sole_owner_table`), and a type alias carries no `__wbg_*` symbol — nothing to collide.
    /// Once per ident, sharing `deferred_warned` with the `try_defer_wrapper` warnings beside it.
    /// Emits nothing when the index flag is unused, and never changes an emitted byte.
    pub(super) fn warn_rule_declared_table_shadows_index(&mut self, rust_ident: &RustIdent) {
        let name = rust_ident.as_ref();
        // Several deps listing one name is already its own warned condition on the defer path; here
        // the collision is the same whichever dep hosts it, so name the first (BTreeMap order, so
        // the choice is deterministic) rather than reciting the set.
        let Some(dep) = self
            .extern_wrapper_index
            .iter()
            .find(|(_, names)| names.contains(name))
            .map(|(dep, _)| dep.clone())
        else {
            return;
        };
        if !self.deferred_warned.insert(rust_ident.clone()) {
            return;
        }
        crate::warn!(
            "warning: rule-declared table {name} is minted locally, but dependency {dep:?} also \
             lists {name} in its --extern-wrapper-index; a table rule keeps the consumer's own \
             class, so both crates export a #[wasm_bindgen] {name} and the two duplicate-symbol \
             when linked into one cdylib. Remedy: rename the rule, or give it a distinct @name, or \
             drop the rule and let {dep} own the type."
        );
    }

    /// The uniform backstop for the same duplicate-`#[wasm_bindgen]`-symbol configuration
    /// `warn_rule_declared_table_shadows_index` covers, keyed on the EMITTED IDENT rather than on
    /// the reason a particular arm declined to defer. Every local wrapper mint passes
    /// `record_collection_wrapper`, while only some of them consult `try_defer_wrapper` — and of
    /// those, two arms return `false` before any index is consulted:
    ///
    /// * the ident≠structural screen — `arr_idx_foo_list = [* idx_foo_list]` derives the structural
    ///   name `IdxFooListList` from its element, so the rule's own ident `ArrIdxFooList` is not a
    ///   defer candidate at all, yet `ArrIdxFooList` is the name of the class actually emitted;
    /// * the R3c constituent screen — a wrapper whose constituents include a CONSUMER-owned type
    ///   (or types of two different dependencies) is local-and-silent by design, which is right
    ///   about the DEFERRAL and says nothing about the NAME.
    ///
    /// In both, the emitted class collides with a dep-indexed name and nothing at the defer seam
    /// can see it. Closing the family at the mint seam covers those two arms and any future one,
    /// instead of chasing `return false` sites.
    ///
    /// Sibling of `warn_rule_declared_table_shadows_index` (the rule-declared LOOSE table, which
    /// reaches no defer seam at all): that one is emitted BEFORE its mint and both share
    /// `deferred_warned`, so a table warns exactly once, with the more specific text.
    ///
    /// Callers: `record_collection_wrapper` only, i.e. the four wrapper emitters' local-mint paths.
    /// A DEFERRED wrapper never reaches it (the emitters return before recording). A wrapper minted
    /// under `requested_scope_override` — dep-side `--wrapper-requests` hosting — is excluded by
    /// intent, not by the accident that such runs carry no index flag: that class exists because a
    /// CONSUMER asked this crate to host the shape, so the name is not this spec's choice and the
    /// arbitration belongs to the request sidecar rather than to this crate's own honesty warning.
    /// Emits nothing when the index flag is unused, and never changes an emitted byte.
    fn warn_local_mint_shadows_index(&mut self, ident: &RustIdent, shape: &str) {
        if self.extern_wrapper_index.is_empty() || self.requested_scope_override.is_some() {
            return;
        }
        let name = ident.as_ref();
        // Several deps listing one name is its own warned condition on the defer path; here the
        // collision is the same whichever dep hosts it, so name the first (BTreeMap order, so the
        // choice is deterministic) rather than reciting the set — as the table sibling does.
        let Some(dep) = self
            .extern_wrapper_index
            .iter()
            .find(|(_, names)| names.contains(name))
            .map(|(dep, _)| dep.clone())
        else {
            return;
        };
        if !self.deferred_warned.insert(ident.clone()) {
            return;
        }
        crate::warn!(
            "warning: collection wrapper {name} is minted locally from this crate's {shape}, but \
             dependency {dep:?} also lists {name} in its --extern-wrapper-index; this mint reached \
             no deferral decision (its ident is not the structural name of its own constituents, \
             or those constituents are not all extern types of one dependency), so both crates \
             export a #[wasm_bindgen] {name} and the two duplicate-symbol when linked into one \
             cdylib. Remedy: rename the rule, or give it a distinct @name, or settle the name on \
             one owner — declare this shape in {dep}'s spec, or drop {name} from {dep}."
        );
    }

    // generate array type ie [Foo] generates Foos if not already created
    pub(super) fn generate_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        array_type_ident: &RustIdent,
        // `true` when `array_type_ident` is an explicit RULE ident (`foo_list = [* foo]`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index` / `--workspace-dep`: if a dependency already owns (index) or a
        // workspace dep owns (unconditional) this exact list wrapper, defer to it (import from the
        // dep's `collections` module) instead of re-minting a duplicate class.
        let shape = format!("[* {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            array_type_ident,
            &element_type.name_as_wasm_array(types),
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        if self.already_generated.insert(array_type_ident.clone()) {
            // Record for the collections.rs index BEFORE the `--wasm-list-macro` early return: the
            // macro still DEFINES the wrapper class, so it belongs in the index exactly like the
            // inline struct below.
            self.record_collection_wrapper(types, array_type_ident, &shape);
            // --wasm-list-macro: emit a single macro invocation in place of the inline struct +
            // accessor block + conversion impls. The macro also emits the conversions, so we skip
            // building the WasmWrapper entirely (returning early) to avoid double-defining them.
            // Element types whose wasm boundary doesn't reduce to (needs_into, is_copy) - e.g.
            // Optional - fall through to the inline path below.
            if let Some(list_macro) = &cli.wasm_list_macro
                && let Some(needs_into) = element_type.wasm_list_macro_needs_into(types)
            {
                let macro_name = list_macro.split("::").last().unwrap();
                let args = [
                    element_type.for_rust_member(types, true, cli),
                    element_type.for_wasm_return(types),
                    array_type_ident.to_string(),
                    needs_into.to_string(),
                    element_type.is_copy(types).to_string(),
                ];
                // Emit the invocation as a sort-participating item keyed under the wrapper type it
                // defines, so it lands where the equivalent inline struct would (not hoisted to the
                // top above the file header) — see `Scope::raw_sorted`.
                self.wasm(types, array_type_ident).raw_sorted(
                    array_type_ident.as_ref(),
                    &format!("{}!({});", macro_name, args.join(", ")),
                );
                return;
            }
            let inner_type = element_type.name_as_rust_array(types, true, cli);
            let mut wrapper = create_base_wasm_struct(self, array_type_ident, false, cli);
            wrapper.push_inner_field(&inner_type);
            // other functions
            let mut new_func = codegen::Function::new("new");
            new_func.vis("pub").ret("Self");
            new_func.line("Self(Vec::new())");
            wrapper.s_impl.push_fn(new_func);
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            push_list_accessors(&mut wrapper, types, &element_type);
            wrapper.add_conversion_methods(&inner_type, cli);
            wrapper.push(self, types);
        }
    }

    /// Emit the RESTRICTED list wrapper for a `[+ elem]` array — the wasm twin of the loose list
    /// wrapper, but wrapping `core::NonEmptyVec<elem>` instead of `Vec<elem>`. Created via
    /// `try_from` (borrow + clone, so the source loose list/Vec stays valid) or `new(first)`; `add`
    /// stays infallible (a push can't break a `>= 1` bound). `wrapper_ident` is the JS class name —
    /// the synthesized `NonEmpty*List` for inline arrays, or the rule ident for a named `[+ …]`.
    pub(super) fn generate_non_empty_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`foo = [+ foo]`), so a structural-name
        // coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmpty*List` over a mapped dependency's extern
        // element is a defer candidate exactly like the loose list — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (`try_defer_wrapper`'s rule-declared guard: a named `[+ …]` rule keeps its ident,
        // which differs from the structural `NonEmpty*List`, and is never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_wrapper_name`, which cannot be called
        // here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the map twin below).
        let structural_name = format!("NonEmpty{}List", element_type.conceptual_type.for_variant());
        let shape = format!("[+ {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the element itself needs (nested `[+ [+ int]]`) first
        self.ensure_non_empty_wrappers(types, &element_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let elem_rust = element_type.for_rust_member(types, true, cli);
        let inner_type = format!("NonEmptyVec<{elem_rust}>");
        // the element's structural loose-builder name; when it coincides with THIS wrapper's ident
        // (a self-named rule like `bar_list = [+ bar]`), the loose builder cannot exist — the rule
        // legitimately owns the ident for its restricted class (collision-checked in finalize), so
        // the wrapper emits WITHOUT `try_from` and is built incrementally (`new(first)` + `add`).
        let elem_wasm = element_type.for_wasm_member(types);
        let loose_list = (!element_type.vec_of_self_directly_wasm_exposable(types)
            && !element_type.is_non_empty_array())
        .then(|| element_type.name_as_wasm_array(types));
        let self_named = loose_list.as_deref() == Some(wrapper_ident.as_ref());
        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        // Decision 11 (two-type design doc): quote the originating CDDL occurrence so the type
        // name, the doc comment, and the try_from signature are three redundant discovery signals.
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first element (`new(first)` + `add`)."
        } else {
            "Enter via `try_from` or `new(first)`."
        };
        // W2 (`--wrapper-requests`): a requested NonEmpty wrapper sets its own struct doc (above /
        // below), which would clobber the attribution doc `create_base_wasm_struct` injects, so
        // prepend the attribution here. Empty prefix (the common case) leaves output byte-identical.
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`[+ {elem_wasm}]`: at least one element, enforced by the `NonEmptyVec` \
             representation.\n{entry_doc}\n`add` can never violate the bound; removal is checked \
             in the core type."
        ));
        wrapper.push_inner_field(&inner_type);
        // new(first) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first", element_type.for_wasm_param(types))
            .line(format!(
                "Self(NonEmptyVec::new({}))",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "first", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // add stays infallible: a push can never violate the >= 1 lower bound
        push_list_accessors(&mut wrapper, types, &element_type);
        // try_from: the single checked door from the loose form to the restricted wrapper. It
        // BORROWS (and clones) so the source loose list/Vec remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if element_type.vec_of_self_directly_wasm_exposable(types) {
            // exposable element: no loose wrapper exists, so take the bare Vec by value (boundary copy)
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("elements", format!("Vec<{elem_wasm}>"))
                .line(
                    "NonEmptyVec::try_from(elements).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        } else if let Some(loose_list) = loose_list.filter(|_| !self_named) {
            // non-exposable, non-nested element: borrow the loose list wrapper and clone it out.
            // Make sure the loose builder exists (inline arrays already mint it; a named `[+ bar]`
            // rule may not have — minting is idempotent via `already_generated`, and a user rule
            // of incompatible shape claiming this ident was rejected at finalize). This mint runs
            // through `try_defer_wrapper` like any other, so a dep-indexed loose source DEFERS —
            // the `try_from` below then borrows the dep's class, whose import is routed at THIS
            // wrapper's emission scope by `scope_references` (the try_from reference is invisible
            // to the field walk — see `register_deferred_non_empty_list_source`).
            self.generate_array_type(
                types,
                element_type.clone(),
                &RustIdent::new(CDDLIdent::new(loose_list.clone())),
                false,
                cli,
            );
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("list", format!("&{loose_list}"))
                .line(format!(
                    "let inner: {} = list.clone().into();",
                    element_type.name_as_rust_array(types, true, cli)
                ))
                .line(
                    "NonEmptyVec::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        }
        // else: self-named rule (loose ident unavailable — see the doc comment) or a nested
        // nonempty element (no clean loose source): built incrementally via new(first)+add only.
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Emit the RESTRICTED set wrapper for a `@duplicates reject` collection — the wasm twin of the
    /// loose list wrapper, but wrapping `core::OrderedSet<T>` (`non_empty == false`) or
    /// `core::NonEmptyOrderedSet<T>` (`non_empty == true`) so the boundary conversion to the rust core
    /// stays an infallible `From` (exactly why `NonEmptyVec` wraps the restricted type, not `Vec`). The
    /// only surface difference from the NonEmpty twin is `add`: pushing an already-present element would
    /// break uniqueness, so `add` is CHECKED here (returns `Result<_, JsError>` through the same door
    /// the core `push` uses). Construction is via `try_from` (the uniqueness/min-1 door) or, for the
    /// non-empty flavor, `new(first)`.
    pub(super) fn generate_reject_ordered_set_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        wrapper_ident: &RustIdent,
        non_empty: bool,
        cli: &Cli,
    ) {
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        let twin = if non_empty {
            "NonEmptyOrderedSet"
        } else {
            "OrderedSet"
        };
        let shape = format!(
            "[{} {}] @duplicates reject",
            if non_empty { "+" } else { "*" },
            render_wrapper_shape(&element_type)
        );
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        // mint any NonEmpty wrappers the element itself needs first (parity with the twins)
        self.ensure_non_empty_wrappers(types, &element_type, cli);
        let elem_rust = element_type.for_rust_member(types, true, cli);
        let inner_type = format!("{twin}<{elem_rust}>");
        let elem_wasm = element_type.for_wasm_member(types);
        // LOCKSTEP: a `@duplicates reject` rule of ANY bounds enters through `try_from(&<Elem>List)`
        // whenever this `loose_list` is `Some` (non-exposable, non-nested, not self-named element), so
        // the import tracker's struct-walk Array arm (`scope_references`/`mark_refs`, intermediate/mod.rs)
        // registers the loose source for reject rules under the SAME condition — its gate keys on
        // `duplicates == Reject` (not just the non-empty bound). Change the two together.
        let loose_list = (!element_type.vec_of_self_directly_wasm_exposable(types)
            && !element_type.is_non_empty_array())
        .then(|| element_type.name_as_wasm_array(types));
        let self_named = loose_list.as_deref() == Some(wrapper_ident.as_ref());
        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`{shape}`: an insertion-ordered, duplicate-free set (order preserved for \
             byte-exact round-trip). `add` is checked — an already-present element is refused; \
             construct via `try_from` (the uniqueness door). `insert` is the std-set door (returns \
             `false`, set unchanged, for an already-present element); `contains` tests membership."
        ));
        wrapper.push_inner_field(&inner_type);
        if non_empty {
            // new(first) — always valid (length 1, trivially unique)
            let mut new_func = codegen::Function::new("new");
            new_func
                .vis("pub")
                .ret("Self")
                .arg("first", element_type.for_wasm_param(types))
                .line(format!(
                    "Self({twin}::new({}))",
                    ToWasmBoundaryOperations::format(
                        element_type
                            .from_wasm_boundary_clone(types, "first", false)
                            .into_iter()
                    )
                ));
            wrapper.s_impl.push_fn(new_func);
        } else {
            let mut new_func = codegen::Function::new("new");
            new_func
                .vis("pub")
                .ret("Self")
                .line(format!("Self({twin}::new())"));
            wrapper.s_impl.push_fn(new_func);
        }
        // len + get (shared conventions), then a CHECKED add (the uniqueness difference)
        wrapper
            .s_impl
            .new_fn("len")
            .vis("pub")
            .ret("usize")
            .arg_ref_self()
            .line("self.0.len()");
        wrapper
            .s_impl
            .new_fn("get")
            .vis("pub")
            .ret(element_type.for_wasm_return(types))
            .arg_ref_self()
            .arg("index", "usize")
            .line(element_type.to_wasm_boundary(types, "self.0[index]", false));
        wrapper
            .s_impl
            .new_fn("add")
            .vis("pub")
            .ret("Result<(), JsError>")
            .arg_mut_self()
            .arg("elem", element_type.for_wasm_param(types))
            .line(format!(
                "self.0.push({}).map_err(|e| JsError::new(&e.to_string()))",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "elem", false)
                        .into_iter()
                )
            ));
        // insert / contains: the std-set doors mirroring the core `OrderedSet` runtime additions
        // (Phase B). `insert -> bool` is the union-friendly no-panic door (`false` = already present,
        // set unchanged); `contains` tests membership. Both keep the wasm surface telling the same
        // story as the rust set API reachable through `Deref`.
        wrapper
            .s_impl
            .new_fn("insert")
            .vis("pub")
            .ret("bool")
            .arg_mut_self()
            .arg("elem", element_type.for_wasm_param(types))
            .line(format!(
                "self.0.insert({})",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "elem", false)
                        .into_iter()
                )
            ));
        wrapper
            .s_impl
            .new_fn("contains")
            .vis("pub")
            .ret("bool")
            .arg_ref_self()
            .arg("elem", element_type.for_wasm_param(types))
            .line(format!(
                "self.0.contains(&{})",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "elem", false)
                        .into_iter()
                )
            ));
        // try_from: the single checked door from the loose form to the restricted wrapper.
        if element_type.vec_of_self_directly_wasm_exposable(types) {
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("elements", format!("Vec<{elem_wasm}>"))
                .line(format!(
                    "{twin}::try_from(elements).map(Self).map_err(|e| JsError::new(&e.to_string()))"
                ));
        } else if let Some(loose_list) = loose_list.filter(|_| !self_named) {
            self.generate_array_type(
                types,
                element_type.clone(),
                &RustIdent::new(CDDLIdent::new(loose_list.clone())),
                false,
                cli,
            );
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("list", format!("&{loose_list}"))
                .line(format!(
                    "let inner: {} = list.clone().into();",
                    element_type.name_as_rust_array(types, true, cli)
                ))
                .line(format!(
                    "{twin}::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))"
                ));
        }
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Emit the RESTRICTED table wrapper for a `{+ k => v}` map — the wasm twin of the loose table
    /// wrapper (`codegen_table_type`), but wrapping `core::NonEmptyMap<K, V>` instead of the raw map.
    /// Created via `try_from(&MapKToV)` (borrow + clone, so the source loose wrapper stays valid) or
    /// `new(first_key, first_value)`; `insert` stays infallible (an insert can't break a `>= 1`
    /// bound); removal is checked in the core type. `wrapper_ident` is the JS class name — the
    /// synthesized `NonEmptyMapKToV` for inline maps, or the rule ident for a named `{+ …}`. The
    /// `insert`/`get`/`has`/`keys` accessors are minted by the shared `push_table_accessors` (also
    /// used by `codegen_table_type`), delegating to `self.0`, whose `NonEmptyMap` method surface
    /// matches the raw map's `len`/`insert`/`get`/`keys`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn generate_non_empty_map_type(
        &mut self,
        types: &IntermediateTypes,
        key_type: RustType,
        value_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`m = {+ k => v}`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        // `@duplicates preserve`: the wrapped rust core is `NonEmptyPairMap<K, V>` (a non-empty vec of
        // pairs, duplicate-permitting), not the loose `NonEmptyMap` — so `new`/`try_from` construct the
        // pair-map twin and the `try_from` source is the loose `PairMap` wrapper. The accessor surface
        // (`insert`/`get`/`keys`/`len`) is shared: `PairMap` exposes the same methods, and `insert`
        // APPENDS (returning `Option`, always `None`) exactly like the loose table wasm insert.
        preserve_pair_map: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmptyMap*` over a mapped dependency's extern
        // key+value is a defer candidate exactly like the loose table — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (rule-declared `{+ …}` rules keep their ident and are never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_map_wrapper_name`, which cannot be
        // called here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the list twin above).
        let structural_name = format!(
            "NonEmpty{}",
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type, preserve_pair_map)
        );
        // preserve marker: same shape-column contract as the loose twin in `codegen_table_type`
        let shape = format!(
            "{{+ {} => {}}}{}",
            render_wrapper_shape(&key_type),
            render_wrapper_shape(&value_type),
            if preserve_pair_map {
                format!(" {}", crate::generation::requests::PRESERVE_MARKER)
            } else {
                String::new()
            }
        );
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the key/value themselves need (nested `{+ …}`) first
        self.ensure_non_empty_wrappers(types, &key_type, cli);
        self.ensure_non_empty_wrappers(types, &value_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let inner_map =
            ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli);
        // the shared `K, V` spelling — strip the leading table-type token
        // (`BTreeMap<K, V>` / `OrderedHashMap<K, V>`) so the wrapper's inner stays in lockstep with the
        // rust field regardless of table flavor.
        let kv_spelling = {
            let open = inner_map.find('<').expect("map type has generics");
            let close = inner_map.rfind('>').expect("map type has generics");
            inner_map[open + 1..close].to_owned()
        };
        // `@duplicates preserve` wraps the vec-of-pairs twin `NonEmptyPairMap`; the loose flavor wraps
        // `NonEmptyMap`. The core-type token (`NonEmptyPairMap`/`NonEmptyMap`) is what `try_from`/`new`
        // construct and what the parent's `.into()` converts to.
        let core_ctor = if preserve_pair_map {
            "NonEmptyPairMap"
        } else {
            "NonEmptyMap"
        };
        let inner_type = format!("{core_ctor}<{kv_spelling}>");
        // the `try_from` source type the loose wrapper's `.into()` yields: `PairMap<K, V>` for preserve
        // (a duplicate-permitting vec of pairs), the loose keyed table otherwise.
        let source_inner_type = if preserve_pair_map {
            format!("PairMap<{kv_spelling}>")
        } else {
            inner_map.clone()
        };
        // the loose structural table wrapper (`MapKToV`) is the `try_from` source; when its ident
        // coincides with THIS wrapper's ident (a self-named rule like `map_text_to_uint = {+ …}`),
        // the loose builder cannot exist — the rule legitimately owns the ident for its restricted
        // class (collision-checked in finalize), so the wrapper emits WITHOUT `try_from` and is built
        // incrementally (`new(first_key, first_value)` + `insert`).
        // the loose source is the SAME-flavored builder (`PairMapKToV` for preserve, `MapKToV` else)
        let loose_ident =
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type, preserve_pair_map);
        let self_named = loose_ident.to_string() == wrapper_ident.to_string();

        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        let map_wasm =
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type, preserve_pair_map);
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first entry (`new(first_key, first_value)` + \
             `insert`)."
        } else {
            "Enter via `try_from` or `new(first_key, first_value)`."
        };
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        // The non-preserve branch is byte-identical to the pre-pair-map doc; only preserve diverges,
        // naming the vec-of-pairs twin and its appending `insert` (never a replace-on-duplicate).
        let repr_doc = if preserve_pair_map {
            "enforced by the `NonEmptyPairMap` representation (an entry-ordered, DUPLICATE-permitting \
             vec of pairs — `insert` APPENDS and never replaces a key, `get` returns the first match)"
        } else {
            "enforced by the `NonEmptyMap` representation"
        };
        wrapper.s.doc(format!(
            "{attr_prefix}`{{+ k => v}}` (`{map_wasm}`): at least one entry, {repr_doc}.\n{entry_doc}\n\
             `insert` can never violate the bound; removal is checked in the core type."
        ));
        wrapper.push_inner_field(&inner_type);
        // new(first_key, first_value) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first_key", key_type.for_wasm_param(types))
            .arg("first_value", value_type.for_wasm_param(types))
            .line(format!(
                "Self({core_ctor}::new({}, {}))",
                ToWasmBoundaryOperations::format(
                    key_type
                        .from_wasm_boundary_clone(types, "first_key", false)
                        .into_iter()
                ),
                ToWasmBoundaryOperations::format(
                    value_type
                        .from_wasm_boundary_clone(types, "first_value", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // len
        wrapper
            .s_impl
            .new_fn("len")
            .vis("pub")
            .ret("usize")
            .arg_ref_self()
            .line("self.0.len()");
        // insert / get / has / keys are minted by the shared `push_table_accessors` — the single
        // source of the nullable-value flattening convention, called by both this restricted twin and
        // the loose `codegen_table_type`. See that helper for the rationale comments.
        push_table_accessors(
            self,
            &mut wrapper,
            types,
            &key_type,
            &value_type,
            "self.0",
            cli,
        );
        // try_from: the single checked door from the loose table wrapper to the restricted wrapper.
        // It BORROWS (and clones) so the source loose `MapKToV` remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if !self_named {
            // ensure the loose builder exists as the `try_from` source. Inline maps already mint the
            // structural `MapKToV` via the visitor (idempotent with our mint through
            // `already_generated`), and a named `{+ …}` rule may not have — so mint it here. EXCEPT
            // when a PLAIN table rule of the same shape is the SOLE OWNER of `MapKToV`: then the loose
            // builder is that owner's class exposed as a `pub type MapKToV = <Owner>;` alias (emitted
            // by `mint_sole_owner_table`), and minting a second `pub struct MapKToV` here would clash
            // with that alias (E0428). The alias resolves to the owner, whose conversion methods make
            // `map.clone().into()` work, so sharing it is both correct and necessary.
            let shape_has_sole_owner = types
                .table_shape_sole_owners()
                .contains_key(&loose_ident.to_string());
            if !shape_has_sole_owner {
                // This mint runs through `try_defer_wrapper` like any other, so a dep-indexed loose
                // `MapKToV` source DEFERS — the `try_from` below then borrows the dep's class, whose
                // import is routed at THIS wrapper's emission scope by `scope_references` (the
                // try_from reference is invisible to the field walk — see
                // `register_deferred_non_empty_map_source`).
                codegen_table_type(
                    self,
                    types,
                    &loose_ident,
                    key_type.clone(),
                    value_type.clone(),
                    false,
                    // The loose `try_from` source mirrors THIS wrapper's flavor: a `{+ …}` preserve
                    // rule's source is the loose `PairMap` wrapper (so `map.clone().into()` yields a
                    // `PairMap`, which the `NonEmptyPairMap` door below accepts).
                    preserve_pair_map,
                    cli,
                );
            }
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("map", format!("&{loose_ident}"))
                .line(format!("let inner: {source_inner_type} = map.clone().into();"))
                .line(format!(
                    "{core_ctor}::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))"
                ));
        }
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Recursively mint the restricted `NonEmpty*List` wrappers a type (at any nesting level) needs.
    /// Named `[+ …]` rules mint their own wrapper under the rule ident elsewhere, so this only fires
    /// on INLINE array shapes (conceptual `Array` carrying the `(Some(1), None)` bounds) that do NOT
    /// dedup to a named rule.
    pub(super) fn ensure_non_empty_wrappers(
        &mut self,
        types: &IntermediateTypes,
        rt: &RustType,
        cli: &Cli,
    ) {
        match &rt.conceptual_type {
            ConceptualRustType::Array(inner) => {
                if rt.is_reject_ordered_set() {
                    // `@duplicates reject` inline (anonymous generic-instance) set: mint the
                    // uniqueness-twin wrapper under its structural name (`U64OrderedSet` /
                    // `NonEmptyU64OrderedSet`). Named reject rules mint under their rule ident via the
                    // rust-struct walk and are never `is_reject_ordered_set` at a REFERENCE (they are
                    // an `Alias`), so they don't route here. The `[+]` reject flavor is covered by
                    // `generate_reject_ordered_set_type`'s `non_empty` arg (its door composes the
                    // min-1 bound), so no separate NonEmptyVec wrapper is minted for it.
                    let ident = RustIdent::new(CDDLIdent::new(
                        rt.reject_ordered_set_wasm_wrapper_name(types),
                    ));
                    self.generate_reject_ordered_set_type(
                        types,
                        (**inner).clone(),
                        &ident,
                        rt.is_non_empty_array(),
                        cli,
                    );
                } else if rt.is_non_empty_array() {
                    // dedup-to-named: an inline `[+ elem]` whose element has a NAMED `[+ …]` rule
                    // uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here
                    if types.non_empty_named_owner(inner).is_none() {
                        let ident =
                            RustIdent::new(CDDLIdent::new(rt.non_empty_wasm_wrapper_name(types)));
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, inner, cli);
                }
            }
            ConceptualRustType::Optional(inner) => {
                self.ensure_non_empty_wrappers(types, inner, cli)
            }
            ConceptualRustType::Map(k, v) => {
                if rt.is_non_empty_map() {
                    // dedup-to-named: an inline `{+ k => v}` whose shape has a NAMED `{+ …}` table
                    // rule uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here. Its key/value still get their own nested wrappers.
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                    if types.non_empty_map_named_owner(k, v).is_none() {
                        let ident = RustIdent::new(CDDLIdent::new(
                            rt.non_empty_wasm_map_wrapper_name(types),
                        ));
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            // Reached as `true` by exactly one shape: the INNER of a
                            // `@newtype`/tag-forced wrapper over `{+ k => v} ; @duplicates preserve`
                            // (`register_rust_struct` threads the rule's policy onto the wrapper's
                            // stored inner, so the wrapper's boundary names `NonEmptyPairMapKToV`
                            // and this is what mints it). A bare inline (anonymous) `{+ …}`
                            // occurrence carries no directive of its own — the policy is per-rule —
                            // so every other caller passes `false`.
                            rt.is_preserve_pair_map(),
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                }
            }
            _ => (),
        }
    }
}

/// Emit the shared wasm list-wrapper accessor triple — `len`, `get`, `add` — onto `wrapper`'s impl.
/// The loose `Vec` wrapper (`generate_array_type`) and its restricted `NonEmptyVec` twin
/// (`generate_non_empty_array_type`) deliberately expose the SAME method surface, each accessor
/// delegating to `self.0` identically, so both mint these three through here — the conventions live
/// once. Only `new` differs between the twins (loose: `Self(Vec::new())`; NonEmpty: `new(first)`),
/// so it stays at each call site (along with any site-specific rationale) and is emitted before this.
fn push_list_accessors(
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    element_type: &RustType,
) {
    wrapper
        .s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    wrapper
        .s_impl
        .new_fn("get")
        .vis("pub")
        .ret(element_type.for_wasm_return(types))
        .arg_ref_self()
        .arg("index", "usize")
        .line(element_type.to_wasm_boundary(types, "self.0[index]", false));
    wrapper
        .s_impl
        .new_fn("add")
        .vis("pub")
        .arg_mut_self()
        .arg("elem", element_type.for_wasm_param(types))
        .line(format!(
            "self.0.push({});",
            ToWasmBoundaryOperations::format(
                element_type
                    .from_wasm_boundary_clone(types, "elem", false)
                    .into_iter()
            )
        ));
}

/// Emit the shared wasm table-wrapper accessor surface — `insert`, `get`, the conditional `has`, and
/// `keys` — onto `wrapper`'s impl, together with the value-nullable machinery all four depend on. The
/// loose map wrapper (`codegen_table_type`) and its restricted `NonEmptyMap` twin
/// (`generate_non_empty_map_type`) deliberately expose the SAME method surface, each accessor
/// delegating to the same receiver identically, so both mint these through here — the nullable-value
/// flattening convention lives once. `new` differs between the twins and `len` is trivial, so both
/// stay at each call site (emitted before this); the `try_from` / conversion tails stay too.
///
/// `receiver` is the expression the accessors delegate to, evaluated against the emitting class's
/// `self`. The two map-wrapper twins pass `self.0` (the wrapper IS the map). An OPEN TABLE's minted
/// struct passes `self.0.<typed row field>`: its typed row's map surface is FLATTENED onto the
/// struct's own class rather than hung off a whole-map getter, the same call the set nominal makes
/// (`docs/docs/wasm_differences.mdx` § "Sets"), because a wasm class has no `Deref` and a JS read of
/// `t.get(k)` beats `t.entries().get(k)`. Flattening is also what keeps the typed row from minting a
/// `MapKToV` class of its own — see the collision-detector family's note on why no fifth sibling
/// detector is owed.
pub(super) fn push_table_accessors(
    gen_scope: &mut GenerationScope,
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    key_type: &RustType,
    value_type: &RustType,
    receiver: &str,
    cli: &Cli,
) {
    // A nullable value (`* uint => (T / null)` -> `Option<T>`) would make get/insert return
    // `Option<Option<T>>` — which wasm-bindgen can't represent (`Option<T>: OptionIntoWasmAbi` is not
    // satisfied). So when the value is itself an `Option`, we flatten the presence-`Option` these
    // accessors add into it and return a single `Option<T>`. This is the same convention the c-style
    // enum-getter (`add_wasm_enum_getters`) uses; native storage still holds all three states
    // (key-absent / present-null / present-value), so CBOR round-trips are unaffected — only the wasm
    // read conflates absent with present-null.
    let value_nullable = matches!(
        value_type.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    );
    let map_value_ret = || {
        if value_nullable {
            value_type.for_wasm_return(types)
        } else {
            format!("Option<{}>", value_type.for_wasm_return(types))
        }
    };
    let value_flatten = if value_nullable { ".flatten()" } else { "" };
    // When the value is nullable, the stored inner is `Option<InnerRust>`. If that inner is not
    // directly wasm-exposable (a named collection / data-enum), the boundary must convert it —
    // `.map(Into::into)` through the Option — not a blanket `.into()`, which has no
    // `From<Option<Inner>>` impl (wasm E0277/E0308).
    let value_nullable_inner_exposable = match value_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Optional(inner) => {
            inner.conceptual_type.directly_wasm_exposable_ct(types)
        }
        _ => false,
    };
    // insert
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm_param(types))
        .arg("value", value_type.for_wasm_param(types))
        .ret(map_value_ret());
    if value_nullable {
        insert_func.doc("Returns the displaced value, or None if the key was absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    insert_func.line(format!(
        "{receiver}.insert({}, {}){}",
        ToWasmBoundaryOperations::format(
            key_type
                .from_wasm_boundary_clone(types, "key", false)
                .into_iter()
        ),
        ToWasmBoundaryOperations::format(
            value_type
                .from_wasm_boundary_clone(types, "value", false)
                .into_iter()
        ),
        if value_nullable {
            if value_nullable_inner_exposable {
                value_flatten.to_owned()
            } else {
                // displaced value is `Option<InnerRust>` after flatten; convert its inner to wasm.
                format!("{value_flatten}.map(Into::into)")
            }
        } else if value_type.directly_wasm_exposable(types) {
            String::new()
        } else {
            ".map(Into::into)".to_owned()
        }
    ));
    // ^ TODO: support failable types everywhere or just force it to be only a detail in the wrapper?
    wrapper.s_impl.push_fn(insert_func);
    // get
    let get_ret_modifier = if value_type.is_copy(types) {
        ""
    } else if value_nullable {
        // stored value is `Option<InnerRust>`; convert the inner across the boundary (when it is
        // not directly exposable) THROUGH the Option, yielding `Option<Option<Wrapper>>` which the
        // trailing `value_flatten` collapses to `Option<Wrapper>`.
        if value_nullable_inner_exposable {
            ".cloned()"
        } else {
            ".map(|v| v.clone().map(Into::into))"
        }
    } else if value_type.directly_wasm_exposable(types) {
        ".cloned()"
    } else {
        ".map(|v| v.clone().into())"
    };
    let mut getter = codegen::Function::new("get");
    getter
        .arg_ref_self()
        .arg("key", key_type.for_wasm_param(types))
        .ret(map_value_ret())
        .vis("pub");
    if value_nullable {
        getter.doc("Returns None if the key is absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    // The is_copy value returns `.copied()`, else the boundary modifier computed above applies. The
    // two twins spelled this differently in source — codegen_table_type inlined the `if` in each key
    // branch, generate_non_empty_map_type used this closure — but produced the same bytes; the closure
    // is the single spelling here.
    let copied_or = |modifier: &str| {
        if value_type.is_wasm_copy(types) {
            // wasm face IS the rust type (primitive / c-style enum): copy out of the `&V` with no
            // `.into()`.
            ".copied()".to_owned()
        } else if value_type.is_copy(types) {
            // A `@copy` extern value: rust-Copy but wasm-wrapped — deref-copy the `&V` (no clone,
            // clippy::clone_on_copy) then `.into()` to the wasm wrapper.
            ".map(|v| (*v).into())".to_owned()
        } else {
            modifier.to_owned()
        }
    };
    if key_type.directly_wasm_exposable(types) {
        getter.line(format!(
            "{receiver}.get({}){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
            value_flatten
        ));
    } else {
        getter.line(format!(
            "{receiver}.get({}.as_ref()){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
            value_flatten
        ));
    }
    wrapper.s_impl.push_fn(getter);
    // has(key): key-presence accessor, emitted from exactly the `value_nullable` flatten condition
    // above (single source of truth) so it can never drift from `get`. When the value is nullable,
    // `get` collapses Option<Option<T>> -> Option<T>, so a `None` return conflates an absent key with
    // a present-but-null one; `has` exposes the key's presence directly (a direct key lookup, not the
    // `keys()` scan that was the only recovery before). Mirrors `get`'s key-boundary handling.
    //
    // No collision check is needed here (unlike the record `has_<field>` accessor): a table wrapper's
    // method surface is entirely generator-fixed (`len`/`insert`/`get`/`has`/`keys`) with no
    // user-named methods — a map has no named fields, only key/value TYPES — so `has` cannot clash
    // with anything the spec author controls.
    if value_nullable {
        let mut has_func = codegen::Function::new("has");
        has_func
            .arg_ref_self()
            .arg("key", key_type.for_wasm_param(types))
            .ret("bool")
            .vis("pub")
            .doc("Returns whether the key is present, distinguishing an absent key from a present-but-null value (both of which `get` reports as None).");
        if key_type.directly_wasm_exposable(types) {
            has_func.line(format!(
                "{receiver}.get({}).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        } else {
            has_func.line(format!(
                "{receiver}.get({}.as_ref()).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        }
        wrapper.s_impl.push_fn(has_func);
    }
    // keys
    let keys_type = ConceptualRustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys.arg_ref_self()
        .ret(keys_type.for_wasm_return_ct(types))
        .vis("pub");
    let key_clone = if key_type.is_copy(types) {
        ".keys().copied()"
    } else {
        ".keys().cloned()"
    };
    // R3d: decide the keys-list wrapper's deferral BEFORE emitting keys() — the keys-list emitter
    // (`generate_array_type`) may run AFTER this map class, so consulting `deferred_wrappers` alone
    // would miss it. `try_defer_wrapper` is idempotent, so this both records the decision (the later
    // emitter re-runs it, suppresses, and the import is routed) and drives the `.into()` here.
    let keys_deferred = !keys_type.directly_wasm_exposable_ct(types)
        && gen_scope.try_defer_wrapper(
            types,
            &RustIdent::new(CDDLIdent::new(key_type.name_as_wasm_array(types))),
            &key_type.name_as_wasm_array(types),
            &[&key_type.conceptual_type],
            &format!("[* {}]", render_wrapper_shape(key_type)),
            false,
            cli,
        );
    if keys_type.directly_wasm_exposable_ct(types) {
        keys.line(format!("{receiver}{key_clone}.collect::<Vec<_>>()"));
    } else if keys_deferred {
        // R3d: the keys-list wrapper is deferred to a dependency (`--extern-wrapper-index`); its tuple
        // field is private cross-crate, so build it through `From<Vec<_>>` (`.into()`) instead of
        // tuple-struct syntax.
        keys.line(format!("{receiver}{key_clone}.collect::<Vec<_>>().into()"));
    } else {
        keys.line(format!(
            "{}({receiver}{key_clone}.collect::<Vec<_>>())",
            keys_type.for_wasm_return_ct(types)
        ));
    }
    wrapper.s_impl.push_fn(keys);
}

/// The top-level NAMED rust idents of a wrapper constituent (element / key / value) — what the defer
/// decision resolves to a dependency scope. Primitives / fixed values contribute none; an alias
/// contributes its aliased ident; an optional passes through to its inner type.
fn named_constituent_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => named_constituent_idents(&inner.conceptual_type),
        _ => vec![],
    }
}

/// The TRANSITIVE named leaf idents of a wrapper constituent — `named_constituent_idents` extended to
/// descend through nested `Array`/`Map` conceptual types to the named types at the leaves. A
/// `[* [* foo]]` has leaf `foo` (its inner wrapper is classified independently); `{* a => [* b]}` has
/// leaves `a` and `b`. Primitives / fixed values contribute none; alias / optional unwrap to their
/// inner. This is what workspace placement resolves to dependency owners.
fn transitive_named_leaf_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Array(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Map(key, value) => {
            let mut out = transitive_named_leaf_idents(&key.conceptual_type);
            out.extend(transitive_named_leaf_idents(&value.conceptual_type));
            out
        }
        _ => vec![],
    }
}

/// The ONE named type every constituent of a wrapper transitively resolves to, or `None` when there
/// are zero named leaves (a primitives-only wrapper) or more than one distinct leaf. This is the
/// `@extern_companions` arm's candidate test: a companion class is "of" a marked type exactly when
/// that type is the wrapper's sole named constituent, so `[* tm]`, `{* tm => tm}` and
/// `NonEmpty[+ tm]` all qualify while `{* tm => local_thing}` does not. Distinct from
/// `transitive_owner_set`, which resolves leaves to their owning DEPENDENCY (a cross-crate question
/// a local marker has no answer to) rather than keeping their identity.
fn sole_named_leaf(constituents: &[&ConceptualRustType]) -> Option<RustIdent> {
    let mut sole: Option<RustIdent> = None;
    for c in constituents {
        for id in transitive_named_leaf_idents(c) {
            match &sole {
                None => sole = Some(id),
                Some(seen) if *seen == id => {}
                Some(_) => return None,
            }
        }
    }
    sole
}

/// The set of element OWNERS of a wrapper's constituents, computed transitively to the named leaves.
/// Each leaf resolves to `Some(dep)` when it is an extern type (leading component of its non-exported
/// scope) or `None` when it is a consumer-owned (exported) type. An empty set means "ownerless" (no
/// named leaves — a primitives-only wrapper like `{* uint => text}`). This is the input to
/// `wrapper_placement`.
fn transitive_owner_set(
    types: &IntermediateTypes,
    constituents: &[&ConceptualRustType],
) -> BTreeSet<Option<String>> {
    let mut owners = BTreeSet::new();
    for c in constituents {
        for id in transitive_named_leaf_idents(c) {
            let scope = types.scope(&id);
            owners.insert(if scope.export() {
                None
            } else {
                scope.components().first().cloned()
            });
        }
    }
    owners
}

/// Where a collection wrapper is hosted, given its transitive element owners. Factored as one
/// function so the placement rule can generalize (plan decision 4): today `Borrow(dep)` iff the
/// wrapper has EXACTLY ONE owner, that owner is a named dependency, and that dependency is a
/// `--workspace-dep`; every other case (ownerless, mixed-dep, a lone non-workspace owner, any
/// consumer-owned leaf) is `Local`. The future rule ("latest of the element owners" / least upper
/// bound in a DAG) replaces this body without touching call sites.
enum WrapperPlacement {
    Local,
    Borrow(String),
}

fn wrapper_placement(
    owners: &BTreeSet<Option<String>>,
    workspace_deps: &BTreeSet<String>,
) -> WrapperPlacement {
    if owners.len() == 1
        && let Some(Some(dep)) = owners.iter().next()
        && workspace_deps.contains(dep)
    {
        return WrapperPlacement::Borrow(dep.clone());
    }
    WrapperPlacement::Local
}

/// Whether this dep's OWN spec defines `ident` (a generated struct/enum or a user type alias) as an
/// exported, in-crate type. A non-exported (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/…`) scope means the type
/// belongs to one of the DEP's own deps, not the dep itself, so it is NOT owned.
pub(super) fn dep_owns_element(types: &IntermediateTypes, ident: &RustIdent) -> bool {
    let known = types.rust_struct(ident).is_some()
        || types
            .type_aliases()
            .contains_key(&AliasIdent::Rust(ident.clone()));
    known && types.scope(ident).export()
}

/// Mint the wasm structural wrapper class for a single visited `ConceptualRustType` (the per-type body
/// of the wasm-wrapper visit). Shared by the rust-struct walk and the wasm-alias-target walk so both
/// reach identical minting decisions (sole-owner routing, map-key array wrappers). Idempotent via
/// `wasm_wrappers_generated`; every class body is derived purely from the shape, so the result is
/// iteration-order-independent.
#[allow(clippy::too_many_arguments)]
pub(super) fn mint_wasm_wrapper_for_visited_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    ty: &ConceptualRustType,
    wasm_wrappers_generated: &mut BTreeSet<String>,
    table_shape_sole_owner: &BTreeMap<String, RustIdent>,
    // The container flavor of THIS occurrence, supplied by the caller from LOCAL information (a rest
    // row's `duplicates()`, an alias base `RustType`'s carried policy). A `ConceptualRustType` carries
    // no policy of its own, so the conceptual visitor passes `false` — it mints only default-flavored
    // wrappers, and every preserve-flavored mint comes from a RustType-/config-level walk that knows.
    preserve_pair_map: bool,
    cli: &Cli,
) {
    match ty {
        ConceptualRustType::Array(elem) => {
            if !ty.directly_wasm_exposable_ct(types) {
                let array_ident = elem.name_as_wasm_array(types);
                if wasm_wrappers_generated.insert(array_ident.clone()) {
                    gen_scope.generate_array_type(
                        types,
                        *elem.clone(),
                        &RustIdent::new(CDDLIdent::new(array_ident)),
                        false,
                        cli,
                    );
                }
            }
        }
        ConceptualRustType::Map(k, v) => {
            let map_ident = ConceptualRustType::name_for_wasm_map(k, v, preserve_pair_map);
            match table_shape_sole_owner.get(&map_ident.to_string()) {
                // A single named rule owns this shape: this embedded/resolved use
                // shares that rule-named class (JS-visible under the CDDL
                // identifier) rather than minting an anonymous structural class.
                Some(owner) => mint_sole_owner_table(
                    gen_scope,
                    types,
                    owner,
                    &map_ident,
                    wasm_wrappers_generated,
                    cli,
                ),
                // Anonymous-only shape (or a same-shape rule pair): mint the
                // structural class, whose inner is the raw map (not a rust rule).
                None => {
                    if wasm_wrappers_generated.insert(map_ident.to_string()) {
                        codegen_table_type(
                            gen_scope,
                            types,
                            &map_ident,
                            *k.clone(),
                            *v.clone(),
                            false,
                            // The flavor comes from the CALLER's local knowledge (see the parameter's
                            // doc); the visited conceptual `Map` has none of its own. `map_ident`
                            // already encodes it, so the class name and its inner cannot disagree.
                            preserve_pair_map,
                            cli,
                        );
                    }
                }
            }
            mint_wasm_keys_list(gen_scope, types, k, wasm_wrappers_generated, cli);
        }
        _ => (),
    }
}

/// Mint the `<K>List` class a map's `keys()` accessor returns, when `K` is not directly exposable (a
/// wasm-native key returns a bare `Vec` and needs no class). Split out of the `Map` arm above so an
/// OPEN TABLE's typed row can claim JUST this half: its map surface is flattened onto the minted
/// struct's own class, so it mints no `MapKToV` container of its own, but its flattened `keys()` still
/// needs the key list. The deferral decision itself is NOT made here — it is made inside
/// `push_table_accessors` before `keys()` is emitted, and re-made (idempotently) by
/// `generate_array_type`, which is what lets the two run in either order.
pub(super) fn mint_wasm_keys_list(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    key: &RustType,
    wasm_wrappers_generated: &mut BTreeSet<String>,
    cli: &Cli,
) {
    if !ConceptualRustType::Array(Box::new(key.clone())).directly_wasm_exposable_ct(types) {
        let keys_ident = key.name_as_wasm_array(types);
        if wasm_wrappers_generated.insert(keys_ident.clone()) {
            gen_scope.generate_array_type(
                types,
                key.clone(),
                &RustIdent::new(CDDLIdent::new(keys_ident)),
                false,
                cli,
            );
        }
    }
}

/// Mint the JS-visible class for a table shape whose SOLE owner is the named rule `owner`, plus a
/// `pub type <structural> = <owner>;` alias so structural-name reference sites (an anonymous `Map`'s
/// `for_wasm_member`, `@newtype` inner getters, cross-module `mark_refs` imports) still resolve —
/// wasm_bindgen exports no type aliases, so it folds the alias onto the `owner` class in the JS ABI.
/// Idempotent via `generated` (which records BOTH the rule name and the structural name), so the
/// visit arm and the Table arm converge to identical output regardless of which reaches the shape
/// first. The class body always derives from the OWNER's declared `(domain, range)`, keeping the
/// output iteration-order-independent.
pub(super) fn mint_sole_owner_table(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    owner: &RustIdent,
    structural_ident: &RustIdent,
    generated: &mut BTreeSet<String>,
    cli: &Cli,
) {
    if generated.insert(owner.to_string()) {
        let (domain, range, preserve_pair_map) = {
            let owner_struct = types
                .rust_structs()
                .get(owner)
                .expect("sole owner of a table shape must be a rust struct");
            let preserve = owner_struct.config().duplicates
                == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
            match owner_struct.variant() {
                RustStructType::Table { domain, range, .. } => {
                    (domain.clone(), range.clone(), preserve)
                }
                _ => unreachable!("sole owner of a table shape must be a Table rust struct"),
            }
        };
        // `exists_in_rust = true`: the inner is the rust crate's `pub type <owner>` alias (exactly the
        // struct-field role's inner), not the raw inline map. Any CBOR tag on the owner is honored by
        // that rust type's serialization, so it is not threaded into this wasm wrapper.
        codegen_table_type(
            gen_scope,
            types,
            owner,
            domain,
            range,
            true,
            preserve_pair_map,
            cli,
        );
    }
    // Structural alias in the SAME module as the class (`owner`'s scope). Skip a self-alias when the
    // rule ident already equals the structural name.
    if *structural_ident != *owner && generated.insert(structural_ident.to_string()) {
        gen_scope
            .wasm(types, owner)
            .push_type_alias(TypeAlias::new(structural_ident, owner).vis("pub").clone());
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn codegen_table_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    key_type: RustType,
    value_type: RustType,
    exists_in_rust: bool,
    // `@duplicates preserve`: the wrapped rust core is `PairMap<K, V>` (a vec of pairs), not the
    // loose `OrderedHashMap`/`BTreeMap`, so `new()` must construct the pair-map. The accessor surface
    // is shared — `PairMap` exposes the same `insert`/`get`/`keys`/`len` methods the loose table does.
    preserve_pair_map: bool,
    cli: &Cli,
) {
    assert!(cli.wasm);
    // `--extern-wrapper-index`: only the anonymous STRUCTURAL map wrapper (`!exists_in_rust`, name ==
    // `name_for_wasm_map`) is a defer candidate — a rule-owned class (`exists_in_rust`) is the
    // consumer's own type. If a mapped dependency owns this exact structural map wrapper, defer to it
    // (import from the dep's `collections` module) instead of re-minting a duplicate class.
    // The `@duplicates preserve` marker rides the shape column exactly like the reject twin's does on
    // an array shape, so a cross-crate request round-trips the pair-map FLAVOR (which is what the
    // structural name now encodes) and not merely the key/value.
    let shape = format!(
        "{{* {} => {}}}{}",
        render_wrapper_shape(&key_type),
        render_wrapper_shape(&value_type),
        if preserve_pair_map {
            format!(" {}", crate::generation::requests::PRESERVE_MARKER)
        } else {
            String::new()
        }
    );
    if !exists_in_rust
        && gen_scope.try_defer_wrapper(
            types,
            name,
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type, preserve_pair_map)
                .as_ref(),
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            // Only the anonymous STRUCTURAL map wrapper reaches here (`!exists_in_rust`); a
            // rule-declared table is screened out above and never a defer candidate.
            false,
            cli,
        )
    {
        return;
    }
    // Idempotency guard, unified with the array wrappers' `already_generated`: the loose structural
    // `MapKToV` builder can be requested BOTH by the wasm-wrapper visitor (a plain `{* k => v}` use)
    // AND directly by `generate_non_empty_map_type` (as a `{+ k => v}` wrapper's `try_from` source);
    // without a shared guard those two paths would double-define the class (E0428). The callers' own
    // dedup sets (`wasm_wrappers_generated` / `generated`) remain — this only ADDS protection, so
    // every existing single-mint path stays byte-identical (the guard passes on first request).
    if !gen_scope.already_generated.insert(name.clone()) {
        return;
    }
    gen_scope.record_collection_wrapper(types, name, &shape);
    // No `tag` parameter: this emits ONLY the wasm wrapper class (accessors + delegation). When the
    // shape has a CBOR tag (`#6.n({ ... })`), the tag is owned entirely by the rust crate's type,
    // which this wrapper's single tuple field holds (via `rust_crate_struct_from_wasm` when
    // `exists_in_rust`); that type's serialize/deserialize writes/checks the tag. The wrapper adds no
    // serialization of its own, so it has nothing to do with the tag — hence the caller's tag is not
    // threaded here.
    // Special-class (major type 7) keys used to be asserted away here, but the break-byte
    // ambiguity they alluded to lives in the rust-side deserialize loop, which
    // `make_deser_loop_break_check` now handles (definite lengths read exactly `n` entries; the
    // indefinite case errors gracefully). This wasm wrapper emits only accessors — nothing here
    // depends on the key's CBOR class.
    let mut wrapper = create_base_wasm_struct(gen_scope, name, false, cli);

    // new / inner core token: `@duplicates preserve` wraps the vec-of-pairs `PairMap` (not the loose
    // `OrderedHashMap`/`BTreeMap`), so both the inner field TYPE and `new()`'s constructor must name it.
    let table_ctor = if preserve_pair_map {
        "PairMap"
    } else {
        table_type(cli)
    };
    let inner_type = if exists_in_rust {
        rust_crate_struct_from_wasm(types, name, cli)
    } else {
        let loose = ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli);
        if preserve_pair_map {
            // reuse the `K, V` spelling from the loose table type but wrap the pair-map core — this is
            // the loose `try_from` source for a `{+ …}` preserve wrapper (`NePmap::try_from(&MapKToV)`).
            let open = loose.find('<').expect("map type has generics");
            let close = loose.rfind('>').expect("map type has generics");
            format!("PairMap<{}>", &loose[open + 1..close])
        } else {
            loose
        }
    };
    wrapper.push_inner_field(&inner_type);
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self")
        .line(format!("Self({table_ctor}::new())"));
    wrapper.s_impl.push_fn(new_func);
    // len
    wrapper
        .s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    // insert / get / has / keys (and the nullable-value flattening convention they share) are minted
    // by `push_table_accessors`, also called by the restricted `NonEmptyMap` twin
    // (`generate_non_empty_map_type`).
    push_table_accessors(
        gen_scope,
        &mut wrapper,
        types,
        &key_type,
        &value_type,
        "self.0",
        cli,
    );
    wrapper.add_conversion_methods(&inner_type, cli);
    wrapper.push(gen_scope, types);
}
