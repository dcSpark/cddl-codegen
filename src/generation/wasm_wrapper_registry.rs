//! WASM collection-wrapper providers, references and the three rendering doors.

use super::*;
use crate::utils::is_valid_rust_ident;

/// One collection-wrapper definition which makes a wasm type name resolvable.  Classes are kept
/// separate from aliases because only a class is a wasm-bindgen value export and belongs in the
/// `collections.rs` cross-crate index.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[cfg(test)]
#[allow(dead_code)] // Inspected by the bin-only registry test module.
pub(crate) enum WasmCollectionWrapperDefinition {
    LocalClass,
    LocalAlias,
    Deferred,
    /// A named collection class declared in a non-exported extern-dependency scope. Its normal
    /// scope import (rather than the structural-wrapper deferred-import path) resolves it from the
    /// dependency's wasm crate.
    DependencyClass,
    /// A named collection alias declared in a non-exported extern-dependency scope. Like a
    /// dependency class, it is supplied through the dependency's normal scope import and is never
    /// a local `collections.rs` class.
    DependencyAlias,
}

/// One collection class, whether this run emits it locally or observes it in an extern dependency.
/// The local-own-spec bit makes wrapper-request shape ownership a projection of these definitions,
/// rather than a parallel mutable index.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct WasmCollectionWrapperClassDefinition {
    pub(super) scope: ModuleScope,
    shape: String,
    own_spec: bool,
}

/// The exact collection name selected by one wasm type renderer, plus the dependency provider it
/// selected by semantic ownership (if any). A structural spelling does not become dependency-owned
/// merely because an unrelated dependency rule happens to have that spelling.
struct WasmCollectionWrapperResolution {
    wrapper: RustIdent,
    dependency_provider_scope: Option<ModuleScope>,
}

/// An actual spelling of a collection-wrapper name in emitted wasm source.  The ordered fields
/// deliberately make the graceful closure error deterministic: wrapper name, then its emitting
/// owner, then the method/alias door that wrote it.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct WasmCollectionWrapperReference {
    wrapper: RustIdent,
    owner: RustIdent,
    door: String,
    /// A reference emitted by a non-exported extern-dependency scope is dependency owned. It has
    /// no locally emitted provider and must never provoke a consumer-side mint; target ownership
    /// alone is deliberately not enough to classify a reference this way.
    dependency_owned: bool,
    /// The dependency scope which owns the named collection provider this local source explicitly
    /// resolves to (a class or alias).
    /// This differs from `dependency_owned`: the latter is classified from the emitting owner,
    /// while this fact is a provider selected by the rendered wrapper ident. A dependency class
    /// must never satisfy a local reference which did not resolve to that exact dependency scope.
    dependency_provider_scope: Option<ModuleScope>,
}

#[cfg(test)]
#[allow(dead_code)] // The bin-only test suite consumes these accessors.
impl WasmCollectionWrapperReference {
    pub(crate) fn wrapper(&self) -> &RustIdent {
        &self.wrapper
    }

    pub(crate) fn door(&self) -> &str {
        &self.door
    }

    pub(crate) fn dependency_owned(&self) -> bool {
        self.dependency_owned
    }

    pub(crate) fn dependency_provider_scope(&self) -> Option<&ModuleScope> {
        self.dependency_provider_scope.as_ref()
    }
}

/// The complete generated-run registry for the wasm collection-wrapper namespace.
///
/// This folds the former local-class and deferred-provider maps into one deterministic ledger and
/// adds the facts the old maps could not express: aliases which resolve a collection name but are
/// not wasm classes, named collection classes and aliases supplied through ordinary extern imports,
/// and every emitted reference. The index, import routing, and sidecars deliberately read the same
/// maps below rather than carrying parallel state.
#[derive(Debug, Default)]
pub(crate) struct WasmCollectionWrapperRegistry {
    local_classes: BTreeMap<RustIdent, WasmCollectionWrapperClassDefinition>,
    local_aliases: BTreeMap<RustIdent, ModuleScope>,
    deferred: BTreeMap<RustIdent, ModuleScope>,
    dependency_classes: BTreeMap<RustIdent, WasmCollectionWrapperClassDefinition>,
    dependency_aliases: BTreeMap<RustIdent, ModuleScope>,
    references: BTreeSet<WasmCollectionWrapperReference>,
}

impl WasmCollectionWrapperRegistry {
    pub(crate) fn record_local_class(
        &mut self,
        ident: RustIdent,
        scope: ModuleScope,
        shape: String,
        own_spec: bool,
    ) {
        self.local_classes.insert(
            ident.clone(),
            WasmCollectionWrapperClassDefinition {
                scope,
                shape,
                own_spec,
            },
        );
    }

    pub(crate) fn record_local_alias(&mut self, ident: RustIdent, scope: ModuleScope) {
        self.local_aliases.insert(ident, scope);
    }

    pub(crate) fn record_deferred(&mut self, ident: RustIdent, scope: ModuleScope) {
        self.deferred.insert(ident, scope);
    }

    pub(crate) fn record_dependency_class(
        &mut self,
        ident: RustIdent,
        scope: ModuleScope,
        shape: String,
    ) {
        debug_assert!(
            !scope.export(),
            "a dependency collection class must live in a non-exported scope"
        );
        self.dependency_classes.insert(
            ident,
            WasmCollectionWrapperClassDefinition {
                scope,
                shape,
                own_spec: false,
            },
        );
    }

    pub(crate) fn record_dependency_alias(&mut self, ident: RustIdent, scope: ModuleScope) {
        debug_assert!(
            !scope.export(),
            "a dependency collection alias must live in a non-exported scope"
        );
        self.dependency_aliases.insert(ident, scope);
    }

    pub(crate) fn record_reference(
        &mut self,
        wrapper: RustIdent,
        owner: RustIdent,
        door: impl Into<String>,
        dependency_owned: bool,
        dependency_provider_scope: Option<ModuleScope>,
    ) {
        self.references.insert(WasmCollectionWrapperReference {
            wrapper,
            owner,
            door: door.into(),
            dependency_owned,
            dependency_provider_scope,
        });
    }

    pub(crate) fn local_classes(
        &self,
    ) -> &BTreeMap<RustIdent, WasmCollectionWrapperClassDefinition> {
        &self.local_classes
    }

    pub(crate) fn local_class_scope(&self, ident: &RustIdent) -> Option<&ModuleScope> {
        self.local_classes
            .get(ident)
            .map(|definition| &definition.scope)
    }

    /// Return the own-spec class for this canonical collection shape, if any. Requested wrappers
    /// and dependency classes intentionally do not participate: only this crate's authored source
    /// can satisfy a consumer wrapper request without a new requested-scope mint.
    pub(crate) fn own_wrapper_shape(&self, shape: &str) -> Option<&RustIdent> {
        self.local_classes.iter().find_map(|(ident, definition)| {
            (definition.own_spec && definition.shape == shape).then_some(ident)
        })
    }

    pub(crate) fn deferred(&self) -> &BTreeMap<RustIdent, ModuleScope> {
        &self.deferred
    }

    #[cfg(test)]
    #[allow(dead_code)] // Inspected by the bin-only registry test module.
    pub(crate) fn definition_kind(
        &self,
        ident: &RustIdent,
    ) -> Option<WasmCollectionWrapperDefinition> {
        if self.local_classes.contains_key(ident) {
            Some(WasmCollectionWrapperDefinition::LocalClass)
        } else if self.local_aliases.contains_key(ident) {
            Some(WasmCollectionWrapperDefinition::LocalAlias)
        } else if self.deferred.contains_key(ident) {
            Some(WasmCollectionWrapperDefinition::Deferred)
        } else if self.dependency_classes.contains_key(ident) {
            Some(WasmCollectionWrapperDefinition::DependencyClass)
        } else if self.dependency_aliases.contains_key(ident) {
            Some(WasmCollectionWrapperDefinition::DependencyAlias)
        } else {
            None
        }
    }

    #[cfg(test)]
    #[allow(dead_code)] // The bin-only test suite inspects the registered references.
    pub(crate) fn references(&self) -> &BTreeSet<WasmCollectionWrapperReference> {
        &self.references
    }

    #[cfg(test)]
    #[allow(dead_code)] // The bin-only test suite removes providers to exercise closure errors.
    pub(crate) fn remove_local_class_for_test(&mut self, ident: &RustIdent) -> Option<ModuleScope> {
        self.local_classes
            .remove(ident)
            .map(|definition| definition.scope)
    }

    /// Require every actual emitted collection-wrapper reference to have an honest provider before
    /// a source map reaches either output producer.  Dependency-owned extern references are the one
    /// intentionally provider-less class: their source belongs to the dependency, not this crate.
    ///
    /// This is also the generation-side spelling floor for structural wrappers. They are minted
    /// after final IR validation from fixed affixes plus `wasm_boundary_identity_fragment`; that
    /// fragment can recurse through a fixed value, so it must not rely solely on the IR's nominal
    /// name set for lexical safety. Check the actual registry vocabulary before a malformed class
    /// or alias reaches rustfmt.
    pub(crate) fn closure_check(&self) -> std::io::Result<()> {
        let spellings = self
            .local_classes
            .keys()
            .chain(self.local_aliases.keys())
            .chain(self.deferred.keys())
            .chain(self.dependency_classes.keys())
            .chain(self.dependency_aliases.keys())
            .chain(self.references.iter().map(|reference| &reference.wrapper))
            .collect::<BTreeSet<_>>();
        let mut errors = spellings
            .into_iter()
            .filter(|ident| {
                !is_valid_rust_ident(ident.as_ref())
                    || crate::parsing::RUST_KEYWORDS.contains(&ident.as_ref())
            })
            .map(|ident| {
                format!(
                    "wasm collection wrapper `{ident}` is not a spellable Rust identifier. Rename the originating CDDL rule or use `@name <new_name>` where the wrapper follows that rule."
                )
            })
            .collect::<BTreeSet<_>>();
        let missing = self
            .references
            .iter()
            .filter(|reference| {
                if reference.dependency_owned {
                    return false;
                }
                if let Some(scope) = &reference.dependency_provider_scope {
                    // A renderer-selected ordinary dependency provider is exact: a local class,
                    // alias, or structural-index deferral sharing its spelling must not silently
                    // replace the dependency class/alias selected by semantic ownership.
                    return self
                        .dependency_classes
                        .get(&reference.wrapper)
                        .map(|definition| &definition.scope)
                        != Some(scope)
                        && self.dependency_aliases.get(&reference.wrapper) != Some(scope);
                }
                !self.local_classes.contains_key(&reference.wrapper)
                    && !self.local_aliases.contains_key(&reference.wrapper)
                    && !self.deferred.contains_key(&reference.wrapper)
            })
            .map(|reference| {
                format!(
                    "missing wasm collection wrapper `{}` referenced by `{}` via {}",
                    reference.wrapper, reference.owner, reference.door
                )
            })
            .collect::<BTreeSet<_>>();
        errors.extend(missing);
        if errors.is_empty() {
            Ok(())
        } else {
            Err(std::io::Error::other(
                errors.into_iter().collect::<Vec<_>>().join("\n"),
            ))
        }
    }
}

impl GenerationScope {
    /// The collection-wrapper name a wasm type renderer writes directly into a signature or alias,
    /// if any.  This is intentionally a rendering twin, not an IR pre-walk: optional recurses
    /// because its spelling embeds the inner type, while arrays/maps stop at their outer wrapper
    /// because a nested wrapper is named by that wrapper's own emitted methods instead.
    fn wasm_collection_reference_ident(
        &self,
        types: &IntermediateTypes,
        ty: &RustType,
    ) -> Option<RustIdent> {
        self.wasm_collection_reference(types, ty)
            .map(|resolution| resolution.wrapper)
    }

    fn wasm_collection_reference(
        &self,
        types: &IntermediateTypes,
        ty: &RustType,
    ) -> Option<WasmCollectionWrapperResolution> {
        self.wasm_collection_reference_inner(types, ty, &mut BTreeSet::new())
    }

    /// Alias-aware implementation of [`Self::wasm_collection_reference_ident`]. A named alias's
    /// conceptual inner intentionally omits its serialization/configuration facts, so follow the
    /// registered `AliasInfo::base_type` instead. In particular, `[+ T]`, bounded lists, and
    /// `@duplicates reject` sets must remain collection-bearing when their authored alias spelling
    /// appears in a wasm field or method signature. The path set is a conservative recursive-alias
    /// guard: a repeated alias cannot establish a wrapper name by itself.
    fn wasm_collection_reference_inner(
        &self,
        types: &IntermediateTypes,
        ty: &RustType,
        aliases_being_followed: &mut BTreeSet<AliasIdent>,
    ) -> Option<WasmCollectionWrapperResolution> {
        if let Some((ident, _)) =
            types.wasm_collection_wrapper(ty, &self.wasm_collection_reference_sole_owners)
        {
            let dependency_provider_scope =
                self.raw_collection_dependency_provider_scope(types, ty, &ident);
            return Some(WasmCollectionWrapperResolution {
                wrapper: ident,
                dependency_provider_scope,
            });
        }
        match &ty.conceptual_type {
            ConceptualRustType::Optional(inner) => {
                self.wasm_collection_reference_inner(types, inner, aliases_being_followed)
            }
            ConceptualRustType::Rust(ident) => types.rust_struct(ident).and_then(|rust_struct| {
                matches!(
                    rust_struct.variant(),
                    RustStructType::Array { .. } | RustStructType::Table { .. }
                )
                .then(|| WasmCollectionWrapperResolution {
                    wrapper: ident.clone(),
                    dependency_provider_scope: (!types.scope(ident).export())
                        .then(|| types.scope(ident).clone()),
                })
            }),
            ConceptualRustType::Alias(AliasIdent::Reserved(_), inner) => {
                // Reserved aliases have no AliasInfo entry. Preserve the outer RustType facts
                // while peeling just the conceptual spelling, rather than reconstructing a fresh
                // bounds/policy-free type.
                let mut resolved = ty.clone();
                resolved.conceptual_type = (**inner).clone();
                self.wasm_collection_reference_inner(types, &resolved, aliases_being_followed)
            }
            ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) => {
                let alias_ident = AliasIdent::Rust(ident.clone());
                let Some(alias_info) = types.type_aliases().get(&alias_ident) else {
                    // Keep the historical fallback for an unregistered forward placeholder, but
                    // retain any facts carried on this occurrence while peeling its conceptual
                    // alias node.
                    let mut resolved = ty.clone();
                    resolved.conceptual_type = (**inner).clone();
                    return self.wasm_collection_reference_inner(
                        types,
                        &resolved,
                        aliases_being_followed,
                    );
                };
                if !aliases_being_followed.insert(alias_ident.clone()) {
                    return None;
                }
                // `base_type` is the configured source of truth; its bounds and duplicates
                // policy do not survive in the Alias conceptual inner.
                let target = self.wasm_collection_reference_inner(
                    types,
                    &alias_info.base_type,
                    aliases_being_followed,
                );
                aliases_being_followed.remove(&alias_ident);
                if types.alias_projection_suppressed(ident) {
                    return target;
                }
                // A non-suppressed alias is itself the spelling emitted by all three wasm type
                // renderers.  It belongs to the collection namespace exactly when its target does.
                target.map(|_| WasmCollectionWrapperResolution {
                    wrapper: ident.clone(),
                    dependency_provider_scope: (!types.scope(ident).export())
                        .then(|| types.scope(ident).clone()),
                })
            }
            _ => None,
        }
    }

    /// Raw structural spellings may select an ordinary dependency import only when the IR chose an
    /// exact named owner for that shape. Looking up the synthesized spelling in `types.scope` is not
    /// enough: an unrelated dependency collection is allowed to have the same Rust ident. Loose
    /// lists and ordered sets have no named-owner route and therefore require declared deferral.
    fn raw_collection_dependency_provider_scope(
        &self,
        types: &IntermediateTypes,
        ty: &RustType,
        wrapper: &RustIdent,
    ) -> Option<ModuleScope> {
        let (owner, structural_alias) = match &ty.conceptual_type {
            ConceptualRustType::Array(element) if ty.is_non_empty_array() => {
                (types.non_empty_named_owner(element)?, false)
            }
            ConceptualRustType::Array(element)
                if ty.is_bounded_array()
                    || (ty.is_type_enforced_exact_homogeneous_array()
                        && !ty.is_reject_ordered_set()) =>
            {
                (
                    types.bounded_array_named_owner(
                        element,
                        ty.config
                            .occurrence_bounds()
                            .expect("bounded array reference carries its bounds"),
                    )?,
                    false,
                )
            }
            ConceptualRustType::Map(key, value) if ty.is_non_empty_map() => {
                (types.non_empty_map_named_owner(key, value)?, false)
            }
            ConceptualRustType::Map(key, value) if ty.is_bounded_map() => (
                types.bounded_map_named_owner(
                    key,
                    value,
                    ty.config
                        .occurrence_bounds()
                        .expect("bounded map reference carries its bounds"),
                    ty.is_preserve_pair_map(),
                )?,
                false,
            ),
            ConceptualRustType::Map(_, _) => {
                let structural = ty.wasm_structural_map_name(types);
                let owner = (structural == *wrapper)
                    .then(|| {
                        self.wasm_collection_reference_sole_owners
                            .get(&structural.to_string())
                    })
                    .flatten()?;
                (owner, true)
            }
            _ => return None,
        };
        let owner_scope = types.scope(owner);
        let exact_owner = structural_alias || owner == wrapper;
        (exact_owner && !owner_scope.export()).then(|| owner_scope.clone())
    }

    /// Record the real collection-wrapper reference a wasm signature/alias just rendered. A
    /// reference emitted from a non-exported extern scope is dependency-owned: it has no local
    /// provider, does not enter the class index, and must not trigger a companion mint. Requested
    /// wrappers classify from their active emitted scope rather than their IR-ident lookup.
    pub(super) fn record_wasm_type_reference(
        &mut self,
        types: &IntermediateTypes,
        ty: &RustType,
        owner: &RustIdent,
        door: &str,
    ) {
        let Some(resolution) = self.wasm_collection_reference(types, ty) else {
            return;
        };
        // Requested wrappers are deliberately IR-ownerless: their structural ident can collide
        // with a dependency rule, but while the override is active their source is emitted in this
        // crate's exported `requested_collections` module. Classify ownership from that actual
        // emission scope rather than `types.scope(owner)`, which describes the colliding IR rule.
        let emission_scope = self
            .requested_scope_override
            .as_ref()
            .unwrap_or_else(|| types.scope(owner));
        let dependency_owned = !emission_scope.export();
        self.wasm_collection_wrapper_registry.record_reference(
            resolution.wrapper,
            owner.clone(),
            door,
            dependency_owned,
            resolution.dependency_provider_scope,
        );
    }

    /// Record a wasm `pub type` alias definition which resolves a collection-wrapper spelling. It
    /// remains out of `collections.rs`: wasm-bindgen exports values only for wrapper classes, while
    /// this alias is a Rust source-level provider for signatures and other aliases. Its target
    /// reference is recorded by the exact rendering branch that wrote the target spelling.
    pub(super) fn record_wasm_collection_alias_definition(
        &mut self,
        types: &IntermediateTypes,
        alias: &RustIdent,
        target: &RustType,
        scope: ModuleScope,
    ) {
        if self
            .wasm_collection_reference_ident(types, target)
            .is_none()
        {
            return;
        }
        if scope.export() {
            self.wasm_collection_wrapper_registry
                .record_local_alias(alias.clone(), scope);
        } else {
            // Extern-dependency scopes are not emitted into this crate, but an authored alias can
            // still be the exact name a local consumer signature imports from the dependency's wasm
            // crate (`DepAlias = DepMap`). It is an honest provider at that dependency scope, never
            // a local source alias or a collections-index row.
            self.wasm_collection_wrapper_registry
                .record_dependency_alias(alias.clone(), scope);
        }
    }

    pub(super) fn wasm_member_type(
        &mut self,
        types: &IntermediateTypes,
        ty: &RustType,
        owner: &RustIdent,
        door: &str,
    ) -> String {
        let rendered = ty.for_wasm_member(types);
        self.record_wasm_type_reference(types, ty, owner, door);
        rendered
    }

    pub(super) fn wasm_param_type(
        &mut self,
        types: &IntermediateTypes,
        ty: &RustType,
        owner: &RustIdent,
        door: &str,
    ) -> String {
        let rendered = ty.for_wasm_param(types);
        self.record_wasm_type_reference(types, ty, owner, door);
        rendered
    }

    pub(super) fn wasm_return_type(
        &mut self,
        types: &IntermediateTypes,
        ty: &RustType,
        owner: &RustIdent,
        door: &str,
    ) -> String {
        let rendered = ty.for_wasm_return(types);
        self.record_wasm_type_reference(types, ty, owner, door);
        rendered
    }

    #[cfg(test)]
    #[allow(dead_code)] // The bin-only test suite inspects the completed registry.
    pub(crate) fn wasm_collection_wrapper_registry(&self) -> &WasmCollectionWrapperRegistry {
        &self.wasm_collection_wrapper_registry
    }

    #[cfg(test)]
    #[allow(dead_code)] // The bin-only test suite removes providers to exercise closure errors.
    pub(crate) fn remove_wasm_collection_local_class_for_test(
        &mut self,
        ident: &RustIdent,
    ) -> Option<ModuleScope> {
        self.wasm_collection_wrapper_registry
            .remove_local_class_for_test(ident)
    }
}
