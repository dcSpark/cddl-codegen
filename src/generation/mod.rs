use crate::cli::Cli;
use crate::comment_ast::{DemandSet, EncodingKind, RuleMetadata};
use codegen::{Block, TypeAlias};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::intermediate::{
    AliasIdent, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, EnumVariantData,
    FixedValue, IntermediateTypes, ModuleScope, Primitive, ROOT_SCOPE, Representation, RestKind,
    RestRow, RestSemantics, RustField, RustIdent, RustRecord, RustStructCBORLen, RustStructConfig,
    RustStructType, RustType, RustTypeSerializeConfig, ToWasmBoundaryOperations, VariantIdent,
    escape_rust_str,
};
use crate::utils::{
    cbor_type_code_str, convert_to_camel_case, convert_to_snake_case, is_valid_rust_ident,
};

/// Doc-comment marker emitted on the rust `pub type` alias of a generator-SYNTHESIZED anonymous
/// generic-collection/table instance (`gcoll<foo>` → `GcollFoo`, `gcoll<uint>` → `GcollU64`,
/// `gtbl<uint, text>` → `GtblU64Text`) — NOT on any user-authored rule alias (`pt = nums`,
/// `gcn = gcoll<foo>`). Such an instance carries no CDDL rule name: the user wrote an anonymous
/// instance, which crosses the wasm boundary as its inline equivalent's STRUCTURAL class
/// (`FooList` / bare `Vec<u64>` / `MapU64ToText`), documented in `docs/docs/wasm_differences.mdx`.
/// This is a provenance channel: `wasm_api_parity` reads THIS exact string from the rust item's
/// rustdoc to recognise the alias as synthesized-not-a-rule-name and skip its (legitimate,
/// documented) rust→wasm asymmetry — a source-shape heuristic (e.g. "aliases a bare collection")
/// was rejected because a sole-owner named-table alias (`pub type Mp = MapU64ToText;`, rust-side a
/// bare `BTreeMap` alias too) is indistinguishable by shape and must STAY gated. `pub(crate)` and
/// read by the `#[cfg(test)]` parity gate directly (no LOCKSTEP duplicate of the string).
pub(crate) const SYNTHESIZED_INSTANCE_ALIAS_DOC: &str = "Synthesized convenience alias for an anonymous generic-collection instance (not a CDDL rule name).";

/// The rustdoc an OPEN TABLE's TYPED row field carries, and — like the marker above — a provenance
/// channel as well as prose. This field is the one `pub` rust field the generator deliberately gives
/// NO wasm getter: the wasm class flattens its map surface onto itself (`insert`/`get`/`len`/`keys`)
/// rather than hanging a whole-map getter off it, on the set nominal's reasoning. `wasm_api_parity`
/// reads THIS exact string off the field's rustdoc so that asymmetry is recognised STRUCTURALLY, the
/// way the preserve encoding-capture fields are, instead of accruing one ledger entry per fixture ×
/// profile — the ledger is for asymmetries that are accidents of a shape, not for a design decision
/// that holds for every open table there will ever be.
pub(crate) const OPEN_TABLE_TYPED_ROW_DOC: &str = "The open table's TYPED entries (CDDL `* k1 => v1`, the first row): every map entry whose key is of this row's declared CBOR major type. Its map surface is FLATTENED onto the wasm class (`insert`/`get`/`len`/`keys`), so there is no whole-map getter. Defaults empty. `@duplicates preserve` makes this a `PairMap` (duplicate keys kept, in wire order); otherwise the loose table container, which rejects a duplicate key.";

/// The NonEmpty open-table typed-row counterpart. Kept separately from the loose provenance marker
/// above because this rustdoc is user-facing API truth: unlike the loose field, this carrier cannot
/// be cleared or have its final default-map entry removed.
pub(crate) const OPEN_TABLE_NON_EMPTY_TYPED_ROW_DOC: &str = "The open table's required TYPED entries (CDDL `+ k1 => v1`, the first row): every map entry whose key is of this row's declared CBOR major type. This `NonEmptyMap` (or `NonEmptyPairMap` under `@duplicates preserve`) always holds at least one typed entry. Its map surface is FLATTENED onto the wasm class (`insert`/`get`/`len`/`keys`), so there is no whole-map getter.";

/// The bounded/restricted open-table typed-row counterpart. It is a third parity provenance marker,
/// and must stay user-facing API truth: unlike the loose field, the checked Bounded carrier enters
/// the record constructor as a complete value and its finite maximum is enforced by flattened wasm
/// insertion rather than an unchecked public map mutation.
pub(crate) const OPEN_TABLE_BOUNDED_TYPED_ROW_DOC: &str = "The open table's restricted TYPED entries (a row-local CDDL occurrence window other than loose `*` / `0*` or the `+` / `1*` compatibility form): every map entry whose key is of this row's declared CBOR major type. This checked `BoundedMap` (or `BoundedPairMap` under `@duplicates preserve`) is a complete construction input, so its declared minimum and maximum cannot be bypassed. Its map surface is FLATTENED onto the wasm class (`insert`/`get`/`len`/`keys`), and finite-max insertion returns a checked result; there is no whole-map getter.";

// `pub(crate)` on the same terms as `layout`/`extern_interface`/`no_std_check` below: a pinned
// message const in here (`DEPTH_LIMIT_REQUIRES_STD`) is read by a test outside `generation/`.
pub(crate) mod export;
use export::declare_modules;

// The generated workspace's shared layout facts (the paths and package-name suffixes `config.rs`
// derives cross-crate flag values from). `pub(crate)` because the whole point is that the reader
// outside `generation/` uses the same constants the emitter does.
pub(crate) mod layout;

// The dep-side extern-interface export: the IR->CDDL renderer for TRANSPARENT export rows (commit 3)
// plus the projection walk / export emitter (`extern_interface_files`) that `export()` drives
// (commit 4). `pub(crate)` so the test-only `api::extern_interface_strings` helper can reach the
// emitter for snapshot fixtures.
pub(crate) mod extern_interface;

// The WIT face: the naming rules (keyword escaping over the kebab converter), the WIT package
// identifier `--wit-package` parses into, and the two pre-generation detectors that decide whether a
// spec can be projected to WIT at all. `pub(crate)` because both readers sit outside `generation/` —
// `intermediate::finalize` runs the detectors and `cli::Cli::wit_package` mints the package id.
pub(crate) mod wit;

// The component face's guest crate: the `wit_bindgen::generate!` call site and the per-type glue
// bridging the WIT surface to the generated rust crate. Consumes `wit`'s projection value and never
// re-derives one of its names — the dependency runs one way, and a second derivation would drift
// from both the emitted `.wit` and the rust↔WIT parity gate.
pub(crate) mod component;

// The emitted `no-std-check/` shim crate: a second always-clobbered sibling tree, on exactly the
// terms `extern_interface` states, but built from `Cli` alone (no IR). `pub(crate)` so the test-only
// `api::no_std_check_strings` helper can reach the producer for snapshot fixtures.
pub(crate) mod no_std_check;

// The write tail of `export()`: every byte written to disk after the content is decided, and every
// read of prior output the tool performs. `pub(crate)` because it is drivable — deliberately —
// without an IR or a `GenerationScope`, which is what `src/tests/write_tail_tests.rs` does.
pub(crate) mod write_tail;
// Re-exports keeping the pre-split paths (`generation::X`) resolving for callers outside this
// module: the public `rustfmt_generated_string` and the test-only helpers. None are used in the
// crate's non-test compilations, so the aliases read as unused there — allow the lint on them.
#[allow(unused_imports)]
pub use export::rustfmt_generated_string;
#[allow(unused_imports)]
pub(crate) use export::{
    CODEGEN_HEADER, concat_files, is_header_stamped_path, is_preservable_generated_path,
    rustfmt_source_with,
};

mod bounds;
use bounds::{
    CONVERT_ERR_TO_OURS, SignArm, SignArmBounds, bounds_check_expr, bounds_check_expr_non_negative,
    bounds_check_if_block, bounds_check_if_block_float, classify_sign_arm, float_fixed_literal,
    nint_arm_needs_width, non_preserve_bounds_fn, prim_window, primitive_non_negative,
    sign_arm_if_block, uint_arm_needs_width, upper_caps, value_bounds_check_line,
    wasm_bounds_check_line, width_reject,
};
pub(crate) use bounds::{bounds_check_expr_rust_type, bounds_reject_value, nint_bounds_to_u64};

mod deserialize;
mod serialize;
use deserialize::{
    DeserializationCode, DeserializeBeforeAfter, DeserializeConfig,
    add_deserialize_final_len_check, add_deserialize_initial_len_check, create_deserialize_impls,
    make_deser_loop, make_deserialization_function, make_err_annotate_block,
};
use serialize::{
    EncodingVarIsCopy, SerializeConfig, SerializingRustType, create_serialize_impls, end_len,
    make_serialization_function, make_serialization_impl, nominal_collection_cfg, start_len,
    write_string_sz, write_using_sz,
};

mod records;
use records::{
    codegen_struct, generate_array_struct_deserialization, generate_array_struct_serialization,
};

mod enums;
use enums::{
    codegen_group_choices, generate_c_style_enum, make_enum_variant_return_if_deserialized,
};

mod wrappers;
pub(crate) use wrappers::generate_tag_check;
use wrappers::{generate_any_cbor_wasm, generate_int, generate_wrapper_struct};

mod collections;
use collections::{
    codegen_table_type, dep_owns_element, is_loose_table_owner, mint_sole_owner_table,
    mint_wasm_keys_list, mint_wasm_wrapper_for_visited_type, push_table_accessors,
};

mod requests;
pub(crate) use requests::{REJECT_MARKER, render_wrapper_shape};
use requests::{load_extern_wrapper_indices, load_workspace_deps};

/// The contract comment emitted just above each scope's own-spec extern re-export glue group in the
/// generated `mod.rs` (rust and wasm). It restates the boundary contract at the E0432 site itself:
/// the crate's hand-written root `lib.rs` must re-export every glued name from wherever the user
/// defined it, so the `pub use crate::<Name>;` lines below resolve. A plain `//` line group (never a
/// doc comment) so rustfmt keeps it stable and the re-export-only import prune — which keys on
/// `syn::Item`s, and comments are not items — still classifies the file unchanged.
const EXTERN_REEXPORT_CONTRACT_COMMENT: &str = "\
// cddl-codegen extern re-export contract: this crate's hand-written root lib.rs must re-export\n\
// each name below (`pub use <your_module>::<Name>;`) so the generated glue resolves against the\n\
// user-owned definition. See the extern types section of docs/output_format.";

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
    scope: ModuleScope,
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
                            .bounds
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
                        .bounds
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
    fn record_wasm_type_reference(
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
    fn record_wasm_collection_alias_definition(
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

pub struct GenerationScope {
    rust_lib_scope: codegen::Scope,
    rust_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    rust_serialize_lib_scope: codegen::Scope,
    serialize_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    wasm_lib_scope: codegen::Scope,
    wasm_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    /// The component crate's guest glue. ONE scope, not one per module scope: the crate is a
    /// `cdylib` whose entire surface is the WIT world, and `wit_bindgen::generate!` mints one type
    /// tree at one invocation site that `export!` must be invoked beside (see the
    /// [`component`] module doc).
    component_lib_scope: codegen::Scope,
    /// The per-scope companion of [`Self::component_lib_scope`], plumbed through the same
    /// `merge_scopes_to_strings` producer the rust and wasm faces use so a later phase can split the
    /// glue across files without touching the write loop. Empty in phase 1.
    component_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    cbor_encodings_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    json_lines: BlocksOrLines,
    already_generated: BTreeSet<RustIdent>,
    /// The one deterministic registry for wasm collection-wrapper classes, aliases, deferred
    /// providers, dependency-owned extern references, and actual emitted references.  Its local
    /// class map is projected into `collections.rs`; aliases intentionally stay out of that index.
    wasm_collection_wrapper_registry: WasmCollectionWrapperRegistry,
    /// Per-generation cache of the immutable IR's sole table-shape owners. Every wasm rendering
    /// door consults it while identifying its wrapper spelling; caching avoids an O(renders ×
    /// tables) rewalk without creating another wrapper-provider registry.
    wasm_collection_reference_sole_owners: BTreeMap<String, RustIdent>,
    /// Parsed `--extern-wrapper-index` inventories: extern-deps dependency name -> the set of
    /// collection-wrapper class names that dependency's own wasm crate already emits (read from its
    /// committed `generated/collections.rs`). Consulted when deciding whether a wrapper the consumer
    /// would mint should instead be deferred to the dependency. Empty unless the flag is passed.
    extern_wrapper_index: BTreeMap<String, BTreeSet<String>>,
    /// Wrapper idents already named in a `--extern-wrapper-index` "candidate not in the dep's index"
    /// stderr warning, so the diagnostic fires at most once per wrapper across the walk.
    deferred_warned: BTreeSet<RustIdent>,
    /// Parsed `--workspace-dep` set (extern-deps directory names marked co-generated workspace
    /// members). A wrapper whose element types are ALL owned by one of these deps DEFERS
    /// UNCONDITIONALLY (no index consult) and is recorded in `borrowed_wrappers`. Empty unless the
    /// flag is passed; populated (and validated) in `generate()` under `--wasm` only.
    workspace_deps: BTreeSet<String>,
    /// Collection wrappers deferred to a workspace dep this run (`--workspace-dep`), keyed by the
    /// structural wrapper ident and mapped to `(dep rust-crate name, canonical CDDL shape)`. The
    /// mirror image of the registry's local-class map ("what I provide" ↔ "what I borrow, from whom"),
    /// materialized into `wasm/src/generated/borrowed_collections.rs` for the dep's own generation to
    /// read. Recording is idempotent (the same wrapper is probed from several sites); two DISTINCT
    /// shapes deriving the SAME structural name is a hard error (the `MapAToBToC` reverse-ambiguity).
    borrowed_wrappers: BTreeMap<RustIdent, (String, String)>,
    /// W2 dep side: while `Some`, `wasm()` / `record_collection_wrapper` route the wrapper being
    /// emitted into this scope (the `requested_collections` module) instead of `types.scope(ident)` —
    /// the requested wrappers are not in the dep's IR, so they have no natural scope. Set only around
    /// the requested-wrapper emission in `emit_requested_collections`; `None` everywhere else.
    requested_scope_override: Option<ModuleScope>,
    /// W2 dep side (`--wrapper-requests`): every explicitly requested collection wrapper actually
    /// hosted this run, as `(structural class ident, requested RustType)`. The hosted wrappers are
    /// emitted into the `requested_collections` scope but are NOT in the dep's IR, so the per-scope wasm import walk
    /// (`scope_references`, which walks IR structs) never marks the element/key/value wasm classes each
    /// wrapper body names — a bare `use super::*;` reaches only the generated ROOT, not a non-root scope
    /// module nor a scoped extern's re-export glue. `scope_references` consumes this to mark those refs
    /// at the requested scope, mirroring the Array/Table struct-walk arms. Empty except under the flag.
    requested_wrapper_types: Vec<(RustIdent, RustType)>,
    /// W2 dep side: attribution doc text (`Generated at the request of: …`) keyed by requested-wrapper
    /// ident. Consulted by `create_base_wasm_struct` (and prepended by the NonEmpty emitters, which set
    /// their own struct doc). Empty except during requested emission, so own-spec wrappers are
    /// unaffected (flag-off byte-identity).
    requested_attribution: BTreeMap<RustIdent, String>,
    /// W2 dep side: `true` when requested-wrapper emission produced a `[+ …]` / `{+ … => …}` wrapper
    /// whose NonEmpty runtime the dep's OWN spec does not otherwise pull in. The runtime-provisioning
    /// gates (`pub mod non_empty`/`non_empty_map` decl + static file copy) OR these in so the dep
    /// hosts a requested NonEmpty wrapper's `NonEmptyVec`/`NonEmptyMap` type. Never set off the flag.
    requested_non_empty_vec: bool,
    /// W2 dep side: `true` when a requested restricted bounded-array wrapper needs the `BoundedVec`
    /// runtime even though this dep's own spec has no bounded homogeneous array occurrence.
    requested_bounded_vec: bool,
    requested_bounded_map: bool,
    requested_non_empty_map: bool,
    /// W2 dep side, `@duplicates reject` twin: `true` when requested-wrapper emission produced a
    /// reject-mode set wrapper whose `ordered_set` runtime the dep's OWN spec does not otherwise pull
    /// in. ORed into the same runtime-provisioning gates as the NonEmpty flags. Never set off the flag.
    requested_ordered_set: bool,
    /// W2 dep side, `@duplicates preserve` pair-map twin: the map-side analog of
    /// `requested_ordered_set`. `true` when requested-wrapper emission produced a preserve-mode table
    /// wrapper whose `pair_map` runtime the dep's OWN spec does not otherwise pull in.
    requested_pair_map: bool,
    /// Own-spec extern re-export contract: the crate-root re-export names the hand-written thin
    /// `lib.rs` MUST provide (`pub use <your_module>::<Name>;`) for this run's emitted glue to
    /// resolve. Collected at EXACTLY the glue-emission sites in `generate()` (the `externs_by_scope`
    /// `pub use crate::{ident};` loop plus the `<Base>RawBytes` flavor loop) so the surfaced list can
    /// never drift from what the glue actually needs. `BTreeSet` keeps the surfaced order
    /// deterministic; the built-in `Int` extern is already filtered out at the emission site, so it
    /// never appears here. Consumed by the run-output print and the seed-once-`lib.rs`
    /// missing-re-export diagnostic (both in `export`). Empty for any spec with no own-spec externs.
    required_rust_reexports: BTreeSet<String>,
    /// The wasm-crate counterpart of `required_rust_reexports`, collected at the live
    /// `wasm_externs_by_scope` `pub use crate::{ident};` loop after the IR boundary-reference walk
    /// filters out wrappers generated wasm never names. Only populated under `--wasm`.
    required_wasm_reexports: BTreeSet<String>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
    /// Every cross-scope generator-minted type ident pushed into some module by
    /// `add_imports_from_scope_refs` this run (flattened from the rust + wasm `scope_references`
    /// maps). `scope_references` over-approximates (a type referenced by a later-collapsed/deferred
    /// alias still lands here), so a referencing module can carry an import it never names — the
    /// usage-derived prune removes it. Handed to `import_prune::PruneConfig` as name-scan-prunable
    /// candidates (every entry is a concrete generated type, so name-scan is sound). Empty until
    /// `generate()` populates it at the two `add_imports_from_scope_refs` loops.
    scope_ref_import_idents: BTreeSet<String>,
    /// WIT strong-uniqueness collisions found during `generate()`, one message per collision.
    ///
    /// The check cannot run at IR finalization beside `wit_scope_cycles`, because its verdict depends
    /// on which types the rust face gives a `Deserialize` impl — a `from-cbor-bytes` static the tool
    /// never emits cannot collide with anything, and projecting with an empty no-deserialize set
    /// REJECTS a spec whose no-`Deserialize` type happens to carry a field named `from_cbor_bytes`.
    /// Recorded here and drained by `generated_files`/`export`, the two producers that already carry
    /// a graceful error channel. Empty off `--component`.
    component_name_collisions: Vec<String>,
    /// The dependency WIT packages `--component-extern-wit` names, read once at the top of
    /// `generate()` and handed to every consumer of the projection.
    ///
    /// Loaded HERE rather than at each `wit::project` call site because there are three of them
    /// (`wit_files`, `wit_name_collisions`, `component_glue`) and a projection that disagreed with
    /// itself about which deps are in import mode would emit a WIT and a guest crate that do not
    /// match. Empty off the flag, which is what makes the flag's absence byte-identical to today.
    component_dep_wits: crate::component_wit_deps::DepWitPackages,
    /// Cross-crate seam errors: a malformed/unreadable dependency WIT (found at load) and a consumer
    /// signature the dependency's own WIT cannot satisfy (found by the projection). Same recorded-and
    /// -drained shape as `component_name_collisions`, and drained by the same check.
    component_import_errors: Vec<String>,
}

impl Default for GenerationScope {
    fn default() -> Self {
        Self::new()
    }
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            rust_lib_scope: codegen::Scope::new(),
            rust_scopes: BTreeMap::new(),
            rust_serialize_lib_scope: codegen::Scope::new(),
            serialize_scopes: BTreeMap::new(),
            wasm_lib_scope: codegen::Scope::new(),
            wasm_scopes: BTreeMap::new(),
            component_lib_scope: codegen::Scope::new(),
            component_scopes: BTreeMap::new(),
            cbor_encodings_scopes: BTreeMap::new(),
            json_lines: BlocksOrLines::default(),
            already_generated: BTreeSet::new(),
            wasm_collection_wrapper_registry: WasmCollectionWrapperRegistry::default(),
            wasm_collection_reference_sole_owners: BTreeMap::new(),
            extern_wrapper_index: BTreeMap::new(),
            deferred_warned: BTreeSet::new(),
            workspace_deps: BTreeSet::new(),
            borrowed_wrappers: BTreeMap::new(),
            requested_scope_override: None,
            requested_wrapper_types: Vec::new(),
            requested_attribution: BTreeMap::new(),
            requested_non_empty_vec: false,
            requested_bounded_vec: false,
            requested_bounded_map: false,
            requested_non_empty_map: false,
            requested_ordered_set: false,
            requested_pair_map: false,
            required_rust_reexports: BTreeSet::new(),
            required_wasm_reexports: BTreeSet::new(),
            no_deser_reasons: BTreeMap::new(),
            scope_ref_import_idents: BTreeSet::new(),
            component_name_collisions: Vec::new(),
            component_dep_wits: BTreeMap::new(),
            component_import_errors: Vec::new(),
        }
    }

    /// The graceful errors the component face recorded during `generate()`, or `Ok`: the WIT
    /// strong-uniqueness collisions, and the cross-crate import seam's own refusals.
    ///
    /// Consulted by BOTH generated-file producers rather than by one of them: `export` writes to
    /// disk and `generated_files` returns strings, and a spec that fails one must fail the other or
    /// the pinned tests would pass against a tree the tool refuses to write.
    ///
    /// The two classes are joined into one error rather than checked in sequence because they are
    /// independent verdicts about one run: a spec with both should show the user both, not the first
    /// one the check happened to reach.
    /// The dependency WIT packages this run imports, for the producers that emit the WIT tree.
    pub(crate) fn component_dep_wits(&self) -> &crate::component_wit_deps::DepWitPackages {
        &self.component_dep_wits
    }

    pub(crate) fn component_collision_check(&self) -> std::io::Result<()> {
        let msgs = self
            .component_import_errors
            .iter()
            .chain(self.component_name_collisions.iter())
            .cloned()
            .collect::<Vec<_>>();
        if msgs.is_empty() {
            return Ok(());
        }
        Err(std::io::Error::other(msgs.join("\n")))
    }

    /// Generates, i.e. populates the state, based on `types`.
    /// this does not create any files, call export() after.
    pub fn generate(&mut self, types: &IntermediateTypes, cli: &Cli) -> Result<(), String> {
        // `wasm_collection_reference_ident` is reached once per emitted wasm type door. The IR is
        // finalized and immutable for this run, so take its deterministic sole-owner projection
        // once rather than rebuilding it for every signature/alias reference.
        self.wasm_collection_reference_sole_owners = types.table_shape_sole_owners();
        // `--workspace-dep` and `--extern-wrapper-index` both LOAD AND VALIDATE mode-independently, so
        // every documented startup malformation aborts generation whether or not `--wasm` is set; their
        // DEFERRAL EFFECTS differ in scope. `--workspace-dep`'s primary sidecar
        // (`rust/src/generated/borrowed_key_types.rs`) is a RUST-crate concern — map-key derives that
        // the dep must carry or the consumer's rust crate fails to build — so its effect applies in
        // either mode. `--extern-wrapper-index` reads each mapped dependency's committed
        // collection-wrapper index (`generated/collections.rs`) so the wasm struct walk can DEFER any
        // wrapper the dep already owns instead of re-minting it (a wasm duplicate-symbol link error
        // otherwise); that dedup is purely wasm-side with no rust-crate effect, so the loaded index is
        // retained only under `--wasm` and discarded in rust-only mode (the rust output is provably
        // unaffected). But the VALIDATION must fire in every mode: a mapping naming a non-extern
        // dependency (or an index file with a malformed line) is a hard error either way, mirroring
        // `--extern-wasm-crate` — a typo that silently disabled deferral would reintroduce the link
        // error. Both parse once, up front, so the data is available at every emitter's mint point.
        self.workspace_deps = load_workspace_deps(types, cli);
        // Which idents get no `Deserialize`, decided for the whole IR before anything is emitted.
        // A verdict every face then CONSULTS (`deserialize_generated`) rather than accumulates:
        // the emission walk's ident order is alphabetical and unrelated to reference order, so a
        // container asking about a contained type mid-walk would otherwise get an order-dependent
        // answer. See `seed_no_deserialize_verdicts`.
        self.seed_no_deserialize_verdicts(types, cli);
        let extern_wrapper_index = load_extern_wrapper_indices(types, cli);
        if cli.wasm {
            self.extern_wrapper_index = extern_wrapper_index;
        }

        // Type aliases
        for (alias_ident, alias_info) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias_ident {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if alias_info.emits_rust_alias()
                    && !(cli.no_synthesized_rust_collection_aliases
                        && alias_info.synthesized_collection)
                {
                    let mut type_alias = TypeAlias::new(
                        ident,
                        alias_info.base_type.for_rust_member(types, false, cli),
                    );
                    type_alias.vis("pub");
                    // `.doc()` replaces rather than appends, so every doc line is collected here
                    // and attached in ONE call: the user's rule-level `@doc` first, then the
                    // mechanical bound notes (which document generator behavior the user can't
                    // know to write). The user doc has two sources: plain alias rules carry it in
                    // `rule_metadata`, while authored collection rules (`foo_list = [* foo]`)
                    // register their alias via `new_manual` (metadata `None`) but carry the rule's
                    // `@doc` on their RustStruct config.
                    let mut doc_lines: Vec<String> = Vec::new();
                    // Provenance marker for a SYNTHESIZED anonymous generic-collection/table instance
                    // alias (`GcollFoo`/`GcollU64`/`GtblU64Text`, never a user rule like `gcn`): read
                    // by `wasm_api_parity` to skip its legitimate, documented rust→wasm asymmetry (the
                    // instance has no CDDL rule name; it crosses as its inline equivalent's structural
                    // class). Leads the doc so the provenance is the first thing a reader sees.
                    if types.is_anonymous_collection_instance(ident) {
                        doc_lines.push(SYNTHESIZED_INSTANCE_ALIAS_DOC.to_owned());
                    }
                    if let Some(comment) = alias_info
                        .rule_metadata
                        .as_ref()
                        .and_then(|m| m.comment.as_deref())
                        .or_else(|| {
                            types
                                .rust_struct(ident)
                                .and_then(|rs| rs.config().doc.as_deref())
                        })
                        // A named binding to a generic SET NOMINAL has neither: its alias is
                        // registered through `AliasInfo::new_manual` (metadata `None`) and the only
                        // `RustStruct` in play is the shared nominal, whose config belongs to the
                        // generic definition. The IR's per-ident record is the binding rule's own.
                        .or_else(|| types.rule_doc(ident))
                    {
                        doc_lines.push(comment.to_owned());
                    }
                    // Decision 11 (two-type design doc): a named `[+ T]` rule's alias quotes the
                    // originating occurrence — the type name, doc comment, and TryFrom signature
                    // are three redundant discovery signals for the constraint.
                    if alias_info.base_type.is_non_empty_array()
                        && let ConceptualRustType::Array(elem) =
                            &alias_info.base_type.conceptual_type
                    {
                        // The min-1 door is `NonEmptyOrderedSet` under `@duplicates reject` (it
                        // composes uniqueness with the bound), else `NonEmptyVec`.
                        let door = if alias_info.base_type.is_reject_ordered_set() {
                            "NonEmptyOrderedSet"
                        } else {
                            "NonEmptyVec"
                        };
                        doc_lines.push(format!(
                            "`[+ {}]`: at least one element, enforced at the `{door}` \
                             `TryFrom<Vec<_>>` door (the CBOR decoder routes through the same \
                             door, so wire-side and API-side rejection are identical).",
                            elem.for_rust_member(types, false, cli)
                        ));
                    }
                    // The finite/zero-minimum array sibling carries its complete occurrence window
                    // in the chosen bounded carrier's const arguments. Quote the canonical sidecar grammar here
                    // so a named alias is discoverable without following the generated type alias.
                    if alias_info.base_type.is_bounded_array() {
                        let shape = render_wrapper_shape(&alias_info.base_type);
                        let door = if alias_info.base_type.is_bounded_reject_ordered_set() {
                            "BoundedOrderedSet"
                        } else {
                            "BoundedVec"
                        };
                        doc_lines.push(format!(
                            "`{shape}`: inclusive length window enforced at the `{door}` \
                             `TryFrom<Vec<_>>` door (the CBOR decoder routes through the same \
                             door, so wire-side and API-side rejection are identical)."
                        ));
                    }
                    // Exact ordinary/preserve arrays are their own native `[T; N]` carrier rather
                    // than a `BoundedVec`. They keep the established list-shaped wasm class and
                    // cross its `try_from` door from the loose `Vec<T>` builder, where the one
                    // checked Vec-to-array handover enforces the exact cardinality.
                    if alias_info
                        .base_type
                        .is_type_enforced_exact_homogeneous_array()
                        && !alias_info.base_type.is_reject_ordered_set()
                    {
                        let shape = render_wrapper_shape(&alias_info.base_type);
                        doc_lines.push(format!(
                            "`{shape}`: exact static `[T; N]` carrier; the list wrapper's \
                             `TryFrom<Vec<_>>` door performs the checked Vec-to-array handover \
                             (and the CBOR decoder crosses that same door)."
                        ));
                    }
                    // map-side twin: a named `{+ k => v}` rule's alias quotes the occurrence too.
                    if alias_info.base_type.is_non_empty_map()
                        && let ConceptualRustType::Map(k, v) = &alias_info.base_type.conceptual_type
                    {
                        doc_lines.push(format!(
                            "`{{+ {} => {}}}`: at least one entry, enforced at the `NonEmptyMap` \
                             `TryFrom` door (the CBOR decoder routes through the same door, so \
                             wire-side and API-side rejection are identical).",
                            k.for_rust_member(types, false, cli),
                            v.for_rust_member(types, false, cli)
                        ));
                    }
                    // Self-describing doc for the transparent tag-N set idiom (`x = #6.N([* a]) / [* a]`):
                    // the tag is an encoding detail, and the per-rule duplicates policy is spelled out
                    // for BOTH stances so a reader never has to know the default.
                    let set_tag = alias_info.base_type.encodings.iter().find_map(|e| match e {
                        CBOREncodingOperation::OptionallyTagged(n) => Some(*n),
                        _ => None,
                    });
                    if let Some(n) = set_tag {
                        doc_lines.push(format!(
                            "The tag-{n} set idiom: the tag is an encoding detail — both the \
                             `#6.{n}(...)` and the bare-array wire forms are accepted (serialization \
                             defaults to tagged), so either round-trips byte-exactly."
                        ));
                    }
                    // The reject doc is scoped to ARRAY (set) aliases via `is_reject_ordered_set`
                    // (conceptual `Array`): a table carrying `@duplicates reject` is a pure no-op
                    // (today's default), so it must stay byte-identical to the no-directive table.
                    if alias_info.base_type.is_reject_ordered_set() {
                        doc_lines.push(
                            "`@duplicates reject`: a repeated element is refused (a \
                             `DuplicateKey` error) on both the wire and the API; accepted \
                             (duplicate-free) input re-emits byte-exactly in wire order (the set is \
                             order-preserving, never sorted)."
                                .to_owned(),
                        );
                    } else if set_tag.is_some() {
                        doc_lines.push(
                            "Duplicate elements are preserved and re-emitted byte-exactly in wire \
                             order (the default for a set idiom; opt into rejection with \
                             `@duplicates reject`)."
                                .to_owned(),
                        );
                    }
                    // An alias BINDING a generic set-nominal instantiation
                    // (`required_signers = nonempty_set<ed25519_key_hash>` →
                    // `pub type RequiredSigners = NonemptySetEd25519KeyHash;`) carries a bare
                    // `Rust(<nominal>)` base_type — the array/tag/policy predicates above see an
                    // opaque reference and fire nothing. Resolve the bound nominal's REGISTERED
                    // policy and emit the same self-describing door/tag/reject lines the
                    // transparent set alias gets: the rule name hides the nominal's name, so
                    // without this the one decode-time breaking change (uniqueness) goes
                    // undocumented on exactly the rule a consumer reads first. A set nominal is
                    // ALWAYS the uniqueness (`reject`) twin — a `preserve` set stays a transparent
                    // `Vec` alias, never a nominal — so the policy line is always the reject blurb.
                    if let ConceptualRustType::Rust(bound_ident) =
                        &alias_info.base_type.conceptual_type
                        && let Some(bound_struct) = types.rust_struct(bound_ident)
                        && bound_struct.config().set_nominal
                        && let RustStructType::Wrapper { wrapped, .. } = bound_struct.variant()
                        && let ConceptualRustType::Array(elem) = &wrapped.conceptual_type
                    {
                        // The min-1 (`[+]`) nominal's door is `NonEmptyOrderedSet`; a min-0 (`[*]`)
                        // nominal has no non-emptiness to enforce, so it emits no door line (the
                        // same convention as the transparent-alias block above).
                        if wrapped.is_non_empty_array() {
                            doc_lines.push(format!(
                                "`[+ {}]`: at least one element, enforced at the \
                                 `NonEmptyOrderedSet` `TryFrom<Vec<_>>` door (the CBOR decoder \
                                 routes through the same door, so wire-side and API-side rejection \
                                 are identical).",
                                elem.for_rust_member(types, false, cli)
                            ));
                        }
                        if let Some(n) = bound_struct.tag() {
                            doc_lines.push(format!(
                                "The tag-{n} set idiom: the tag is an encoding detail — both the \
                                 `#6.{n}(...)` and the bare-array wire forms are accepted \
                                 (serialization defaults to tagged), so either round-trips \
                                 byte-exactly."
                            ));
                        }
                        doc_lines.push(
                            "`@duplicates reject`: a repeated element is refused (a \
                             `DuplicateKey` error) on both the wire and the API; accepted \
                             (duplicate-free) input re-emits byte-exactly in wire order (the set is \
                             order-preserving, never sorted)."
                                .to_owned(),
                        );
                        if cli.wasm {
                            // wasm-bindgen exports no type aliases, so the rule-name class collapses:
                            // JS/TS callers re-key from the rule name to the nominal class name. A
                            // generated `.d.ts` `export type` keeps TS type positions compiling; JS
                            // value positions (`new`, static methods) must use the nominal class.
                            doc_lines.push(format!(
                                "wasm/JS: this rule has no class of its own — the wasm surface is the \
                                 nominal class `{bound_ident}`. TypeScript keeps `{ident}` as a \
                                 generated type alias (`export type {ident} = {bound_ident};`), but JS \
                                 call sites re-key to `{bound_ident}`."
                            ));
                        }
                    }
                    if !doc_lines.is_empty() {
                        type_alias.doc(doc_lines.join("\n"));
                    }
                    self.rust(types, ident).push_type_alias(type_alias);
                }
                if alias_info.emits_wasm_alias() {
                    // WASM crate
                    if let ConceptualRustType::Fixed(constant) =
                        &alias_info.base_type.conceptual_type
                    {
                        // wasm-bindgen doesn't support const or static vars so we must do a function
                        let (ty, val) = match constant {
                            FixedValue::Null => panic!("null constants not supported"),
                            FixedValue::Undefined => panic!(
                                "undefined constants are nominal unit values, not wasm primitives"
                            ),
                            FixedValue::Bool(b) => ("bool", b.to_string()),
                            FixedValue::Nint(i) => ("i32", i.to_string()),
                            FixedValue::Uint(u) => ("u32", u.to_string()),
                            // float_literal, not Display: a whole-valued f64 would render as an
                            // integer literal in the f64-returning wasm constant fn (E0308).
                            FixedValue::Float(f) => ("f64", float_fixed_literal(*f)),
                            FixedValue::Text(s) => {
                                ("String", format!("\"{}\".to_owned()", escape_rust_str(s)))
                            }
                            FixedValue::Bytes(bytes) => {
                                ("Vec<u8>", FixedValue::bytes_rust_expr(bytes))
                            }
                        };
                        self.wasm(types, ident)
                            .new_fn(convert_to_snake_case(ident.as_ref()))
                            .attr("wasm_bindgen")
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        // A passthrough alias to a named collection (`ptm = mp`) is a transparent
                        // `pub type` in rust but a wrapper struct in wasm; point the wasm alias at that
                        // wrapper rather than `for_wasm_member`'s inline-only `MapU64To…` name (the
                        // wrapper-vs-transparent decision lives in `resolved_wasm_alias_target`, shared
                        // with `scope_references`' type-alias walk so the emitted target and its
                        // cross-module import cannot drift). Maps are never directly exposable, so this
                        // covers `passthrumap` while leaving `passthru` (exposable arrays) on the
                        // transparent `for_wasm_member` path.
                        // Build a RustType for the exact spelling the alias line uses. In the
                        // stripped-named branch that is the resolved wrapper target, NOT the
                        // transparent base shape; registering the latter would check a different
                        // name than `pub type {ident} = {wasm_target}` actually writes.
                        let alias_target = match alias_info.resolved_wasm_alias_target(types) {
                            Some(target) => RustType::new(ConceptualRustType::Rust(target.clone())),
                            None => alias_info.base_type.clone(),
                        };
                        let wasm_target = self.wasm_member_type(
                            types,
                            &alias_target,
                            ident,
                            "wasm pub type alias target",
                        );
                        // A wasm `pub type` alias is a real Rust-source provider for an authored
                        // collection alias or a structural sole-owner alias, but is not a
                        // wasm-bindgen class and therefore must not enter `collections.rs`.
                        // Register its definition at the same emission seam as the alias line.
                        // `wasm_member_type` above registered the exact target reference.
                        self.record_wasm_collection_alias_definition(
                            types,
                            ident,
                            &alias_target,
                            types.scope(ident).clone(),
                        );
                        // A rule-name alias BINDING a set nominal (`required_signers =
                        // nonempty_set<...>` → `pub type RequiredSigners = NonemptySetEd25519KeyHash;`)
                        // gets NO wasm-bindgen class of its own — wasm-bindgen exports no type aliases,
                        // so the rule name would vanish from the generated `.d.ts`. Inject a
                        // `typescript_custom_section` re-exporting the rule name as a TS type alias to
                        // the nominal class, so TS callers keep compiling through the rename. (JS call
                        // sites still re-key to the nominal class name — wasm-bindgen cannot alias a
                        // class as a *value*; the collapse notice + migration docs spell that out.)
                        // Scoped to set nominals: the collapse the CML set-nominalization regen hit.
                        let ts_alias_section = if let ConceptualRustType::Rust(bound_ident) =
                            &alias_info.base_type.conceptual_type
                        {
                            types
                                .rust_struct(bound_ident)
                                .filter(|rs| rs.config().set_nominal)
                                .map(|_| {
                                    let const_name =
                                        convert_to_snake_case(ident.as_ref()).to_uppercase();
                                    format!(
                                        "#[wasm_bindgen(typescript_custom_section)]\nconst TS_ALIAS_{const_name}: &'static str = \"export type {ident} = {wasm_target};\";"
                                    )
                                })
                        } else {
                            None
                        };
                        let wasm_scope = self.wasm(types, ident);
                        wasm_scope
                            .push_type_alias(TypeAlias::new(ident, wasm_target).vis("pub").clone());
                        if let Some(section) = ts_alias_section {
                            wasm_scope.raw(&section);
                        }
                    }
                    // A type-alias BASE can carry an inline `[+ T]` / `{+ k => v}` shape that only
                    // this alias reaches — e.g. `x = bytes .cbor [+ uint]` classifies as a plain
                    // alias (not a `RustStructType::Array`), so the rust_structs minting walk below
                    // never visits it, while the wasm alias line above names the restricted wrapper
                    // (`pub type X = NonEmptyU64List;`). Mint the wrappers the base needs here; the
                    // dedup-to-named and `already_generated` guards inside apply as everywhere else,
                    // so a base whose shape a named rule owns dedups instead of double-minting.
                    // (Found by the recombination wasm sweep: rc1205's `NonEmptyU64List` was
                    // referenced but never emitted — E0425 with generation exit 0.)
                    if cli.wasm {
                        self.ensure_non_empty_wrappers(types, &alias_info.base_type, cli);
                    }
                }
            }
        }

        // Structs
        {
            // we can ignore types already handled by the alias
            // otherwise wasm_wrappers_generated may cause us to pointlessly create aliases to aliases
            let mut existing_aliases = types.type_aliases().iter().fold(
                BTreeSet::<RustIdent>::new(),
                |mut acc, (alias, _)| {
                    match alias {
                        AliasIdent::Reserved(_) => {}
                        AliasIdent::Rust(ident) => {
                            acc.insert(ident.clone());
                        }
                    };
                    acc
                },
            );

            // Shapes owned by EXACTLY ONE named table rule: their embedded/resolved uses share the
            // rule-named class (a real `#[wasm_bindgen]` class under the CDDL identifier), and the
            // structural `MapKToV` name becomes a `pub type` alias to it. Same-shape rule PAIRS (2+
            // owners) and anonymous-only shapes are absent — they keep the structural fallback class
            // at the crate root. Shared with `scope_references`'s Map arm (import placement) via the
            // one helper so emission and import placement CANNOT disagree.
            let table_shape_sole_owner = types.table_shape_sole_owners();

            let mut wasm_wrappers_generated = BTreeSet::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                assert_eq!(rust_ident, rust_struct.ident());
                if cli.wasm {
                    rust_struct.visit_types_excluding(
                        types,
                        &mut |ty| {
                            mint_wasm_wrapper_for_visited_type(
                                self,
                                types,
                                ty,
                                &mut wasm_wrappers_generated,
                                &table_shape_sole_owner,
                                // the conceptual visitor is policy-blind, so it mints only
                                // DEFAULT-flavored wrappers; every `@duplicates preserve` mint comes
                                // from a RustType-/config-level walk that knows its own flavor
                                false,
                                cli,
                            )
                        },
                        &mut existing_aliases,
                    );
                    // The conceptual visitor above can't see array LENGTH bounds (they live on the
                    // RustType, stripped before it recurses), so mint the restricted `NonEmpty*List`
                    // wrappers for inline `[+ T]` shapes from a RustType-level walk that does.
                    match rust_struct.variant() {
                        RustStructType::Record(record) => {
                            for field in &record.fields {
                                self.ensure_non_empty_wrappers(types, &field.rust_type, cli);
                                // The conceptual visitor above intentionally cannot see a field's
                                // `@duplicates preserve` policy. Restricted pair maps route through
                                // `ensure_non_empty_wrappers`, but the loose `{* …}` twin has no
                                // bounds to trigger that path. Mint it from the policy-bearing
                                // RustType so an all-one-dependency field records/defer-imports
                                // `PairMapKToV`, never the default `MapKToV` class.
                                if field.rust_type.is_preserve_pair_map()
                                    && !field.rust_type.is_non_empty_map()
                                    && !field.rust_type.is_bounded_map()
                                {
                                    mint_wasm_wrapper_for_visited_type(
                                        self,
                                        types,
                                        &field.rust_type.conceptual_type,
                                        &mut wasm_wrappers_generated,
                                        &table_shape_sole_owner,
                                        true,
                                        cli,
                                    );
                                }
                            }
                            // Open struct-map rest row (CAPTURE only): its container is a
                            // `Map(domain, range)` the conceptual visitor above never sees as a
                            // composite (it walks domain/range separately). Mint the map's wasm
                            // wrapper explicitly — the rest field's getter returns it — via the SAME
                            // path a map field's wrapper takes, with the flavor read straight off the
                            // row (`RestRow::duplicates()`): a `@duplicates preserve` rest mints the
                            // PairMap-backed `PairMapKToV`, a default row the keyed `MapKToV`, so two
                            // rows of the same key/value and different policies mint two distinct
                            // classes. An `@ignore` row has no field/getter, so no wasm map wrapper is
                            // minted for it (its wasm class is a closed struct's).
                            // An open table's TYPED row is deliberately NOT in this loop: its map
                            // surface is FLATTENED onto the minted struct's own wasm class
                            // (`insert`/`get`/`len`/`keys`, the set-nominal call), so it has no
                            // whole-map getter and mints no container class at all — only the
                            // `<K_t>List` its flattened `keys()` returns, claimed just below. The
                            // CATCH-ALL row keeps its `rest()` getter and therefore its container.
                            for rest in record
                                .captured_dynamic_rows()
                                .filter(|r| !r.is_array_tail() && !record.is_typed_row(r))
                                .collect::<Vec<_>>()
                            {
                                let rest_map = rest.container_type();
                                if rest_map.is_non_empty_map() || rest_map.is_bounded_map() {
                                    // A restricted open-struct row / open-table catch-all crosses
                                    // wasm through its checked structural wrapper, never through a
                                    // loose map that would let JS erase the window before Rust sees
                                    // it. `ensure_non_empty_wrappers` owns both the NonEmpty and
                                    // Bounded map families despite its historical name.
                                    self.ensure_non_empty_wrappers(types, &rest_map, cli);
                                } else {
                                    mint_wasm_wrapper_for_visited_type(
                                        self,
                                        types,
                                        &rest_map.conceptual_type,
                                        &mut wasm_wrappers_generated,
                                        &table_shape_sole_owner,
                                        rest_map.is_preserve_pair_map(),
                                        cli,
                                    );
                                }
                                self.ensure_non_empty_wrappers(types, rest.domain(), cli);
                                self.ensure_non_empty_wrappers(types, rest.range(), cli);
                            }
                            // The open table's TYPED row: the keys-list half of the mint above, and
                            // nothing else. Without it the flattened `keys()` returns an undeclared
                            // class (E0425 in the wasm crate) for every non-exposable `K_t`.
                            if let Some(typed) = record.typed_row().filter(|r| !r.is_array_tail()) {
                                mint_wasm_keys_list(
                                    self,
                                    types,
                                    typed.domain(),
                                    &mut wasm_wrappers_generated,
                                    cli,
                                );
                                self.ensure_non_empty_wrappers(types, typed.domain(), cli);
                                self.ensure_non_empty_wrappers(types, typed.range(), cli);
                                // A bounded typed row remains flattened on its owner class, so it
                                // mints NO restricted whole-row class. Its wasm constructor
                                // nevertheless needs one loose same-flavor builder to present the
                                // complete row at the checked native carrier door — minimum zero
                                // included. This is intentionally the only typed-row map-wrapper
                                // exception.
                                if typed.container_type().bounded_map_u64_bounds().is_some() {
                                    let builder = typed.staging_container_type();
                                    mint_wasm_wrapper_for_visited_type(
                                        self,
                                        types,
                                        &builder.conceptual_type,
                                        &mut wasm_wrappers_generated,
                                        &table_shape_sole_owner,
                                        builder.is_preserve_pair_map(),
                                        cli,
                                    );
                                }
                            }
                            // Open ARRAY `* t` tail (CAPTURE only): its container is an
                            // `Array(element)` the conceptual visitor above never sees as a composite
                            // (it walks only the element). Mint the list's wasm wrapper explicitly —
                            // the tail field's getter returns it — via the SAME path a list field's
                            // wrapper takes. An `@ignore` tail has no field/getter, so nothing is minted
                            // (its wasm class is a closed struct's).
                            for rest in record
                                .captured_dynamic_rows()
                                .filter(|r| r.is_array_tail())
                                .collect::<Vec<_>>()
                            {
                                let rest_list = rest.container_type();
                                mint_wasm_wrapper_for_visited_type(
                                    self,
                                    types,
                                    &rest_list.conceptual_type,
                                    &mut wasm_wrappers_generated,
                                    &table_shape_sole_owner,
                                    // an array tail has no key domain, so no map flavor to carry
                                    false,
                                    cli,
                                );
                                self.ensure_non_empty_wrappers(types, &rest_list, cli);
                                self.ensure_non_empty_wrappers(types, rest.element(), cli);
                            }
                        }
                        RustStructType::Table { domain, range, .. } => {
                            // the named table's OWN restricted wrapper (`{+ k => v}`) is minted in
                            // the variant match below (under the rule ident); here just mint wrappers
                            // its domain/range need (nested `{+ …}` in a key or value position)
                            self.ensure_non_empty_wrappers(types, domain, cli);
                            self.ensure_non_empty_wrappers(types, range, cli);
                        }
                        RustStructType::Wrapper { wrapped, .. } => {
                            // A `@newtype`/TAG-forced wrapper over an INLINE `@duplicates preserve`
                            // table stores a `Map` inner carrying the policy (threaded onto the
                            // wrapped type by `register_rust_struct`). The conceptual visitor above
                            // is policy-blind, so it mints only the DEFAULT-flavored `MapKToV` while
                            // this wrapper's own wasm boundary (`new`/`get`) names the `PairMapKToV`
                            // twin — E0425 on a class nobody minted. Mint that twin here, from the
                            // RustType-level walk that can read the flavor, exactly as the array
                            // sibling's `@duplicates reject` inner reaches
                            // `generate_reject_ordered_set_type` through
                            // `ensure_non_empty_wrappers` below. The `{+ …}` preserve flavor routes
                            // through that same call (its `NonEmptyPairMapKToV` door), so only the
                            // LOOSE `{* …}` shape is claimed here.
                            if wrapped.is_preserve_pair_map() && !wrapped.is_non_empty_map() {
                                mint_wasm_wrapper_for_visited_type(
                                    self,
                                    types,
                                    &wrapped.conceptual_type,
                                    &mut wasm_wrappers_generated,
                                    &table_shape_sole_owner,
                                    true,
                                    cli,
                                );
                            }
                            self.ensure_non_empty_wrappers(types, wrapped, cli);
                        }
                        RustStructType::GroupChoice { variants, .. }
                        | RustStructType::TypeChoice { variants } => {
                            for v in variants {
                                match &v.data {
                                    EnumVariantData::RustType(t) => {
                                        self.ensure_non_empty_wrappers(types, t, cli)
                                    }
                                    EnumVariantData::Inlined(rec) => {
                                        for f in &rec.fields {
                                            self.ensure_non_empty_wrappers(
                                                types,
                                                &f.rust_type,
                                                cli,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        RustStructType::Array { element_type, .. } => {
                            // the named rule's own wrapper is minted in the variant match below;
                            // here just mint wrappers its element needs (nested `[+ [+ int]]`)
                            self.ensure_non_empty_wrappers(types, element_type, cli);
                        }
                        _ => (),
                    }
                }
                match rust_struct.variant() {
                    RustStructType::Record(record) => {
                        codegen_struct(
                            self,
                            types,
                            rust_ident,
                            rust_struct.tag(),
                            record,
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::Table {
                        domain,
                        range,
                        bounds,
                    } => {
                        // A SYNTHESIZED anonymous map instance converges onto the STRUCTURAL map
                        // wrapper (`MapKToV` / `NonEmptyMapKToV`) via its `gen_wasm_alias` passthrough,
                        // exactly as the array arm above does for lists — mint no rule-named class.
                        let anon = types.is_anonymous_collection_instance(rust_ident);
                        if cli.wasm && !anon && *bounds == Some((Some(1), None)) {
                            // named `{+ k => v}` rule: its JS class is the RESTRICTED wrapper
                            // (wrapping core::NonEmptyMap) under the rule ident, not the loose table
                            // wrapper — the map-side twin of the named `[+ T]` array arm.
                            self.generate_non_empty_map_type(
                                types,
                                domain.clone(),
                                range.clone(),
                                rust_ident,
                                true,
                                rust_struct.config().duplicates
                                    == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                                cli,
                            );
                        } else if cli.wasm
                            && !anon
                            && let Some((min, max)) = {
                                let table: crate::intermediate::RustType =
                                    crate::intermediate::ConceptualRustType::Map(
                                        Box::new(domain.clone()),
                                        Box::new(range.clone()),
                                    )
                                    .into();
                                bounds.and_then(|bounds| {
                                    table.with_bounds(bounds).bounded_map_u64_bounds()
                                })
                            }
                        {
                            self.generate_bounded_map_type(
                                types,
                                domain.clone(),
                                range.clone(),
                                rust_ident,
                                (min, max),
                                !types.is_synthesized_collection(rust_ident),
                                rust_struct.config().duplicates
                                    == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                                cli,
                            );
                        } else if cli.wasm && !anon {
                            // A rule-declared LOOSE table never reaches `try_defer_wrapper` (both
                            // mints below are `exists_in_rust` paths), so the one thing the defer
                            // seam would have said about a dep-indexed name has to be said here.
                            self.warn_rule_declared_table_shadows_index(rust_ident);
                            let map_ident = RustType::wasm_structural_map_name_for(
                                domain,
                                range,
                                rust_struct.config().duplicates
                                    == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                                types,
                            );
                            if table_shape_sole_owner.get(&map_ident.to_string())
                                == Some(rust_ident)
                                && is_loose_table_owner(types, rust_ident)
                            {
                                // Sole owner of this shape: emit the real JS class under the rule name
                                // plus the structural alias. Idempotent — the visit arm may have
                                // minted it already for an embedded/resolved use; either order
                                // converges to identical output.
                                mint_sole_owner_table(
                                    self,
                                    types,
                                    rust_ident,
                                    &map_ident,
                                    &mut wasm_wrappers_generated,
                                    cli,
                                );
                            } else if wasm_wrappers_generated.insert(rust_ident.to_string()) {
                                // Shared shape: a same-shape rule PAIR, or a shape also reached by
                                // anonymous/embedded uses. Every named rule STILL surfaces as its own
                                // real JS class under its identifier (unconditionally, independent of
                                // whether a structural twin was minted first); the structural `MapKToV`
                                // class, where referenced, is minted by the visit arm above.
                                codegen_table_type(
                                    self,
                                    types,
                                    rust_ident,
                                    domain.clone(),
                                    range.clone(),
                                    true,
                                    rust_struct.config().duplicates
                                        == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), ConceptualRustType::name_for_rust_map(domain, range, false)));
                    }
                    RustStructType::Array {
                        element_type,
                        bounds,
                    } => {
                        // A SYNTHESIZED anonymous collection instance (`[a: set<key_hash>]` →
                        // `SetKeyHash`) mints NO rule-named class here: its wasm wrapper is the
                        // STRUCTURAL one (`KeyHashList`), emitted through the flipped-on
                        // `gen_wasm_alias` passthrough + base-type walk exactly like an inline
                        // `[* key_hash]`. Skipping the mint (and its `record_collection_wrapper`) is
                        // what keeps the synthesized name out of the own-spec shape projection, so a
                        // `--wrapper-requests` consumer's structural import resolves via own-spec.
                        if cli.wasm && !types.is_anonymous_collection_instance(rust_ident) {
                            let reject = rust_struct.config().duplicates
                                == Some(crate::comment_ast::DuplicatesPolicy::Reject);
                            let non_empty = *bounds == Some((Some(1), None));
                            if reject {
                                // `@duplicates reject` rule: its JS class is the uniqueness-twin
                                // wrapper (wrapping core::OrderedSet / NonEmptyOrderedSet) so the
                                // boundary conversion to the rust core stays an infallible `From`.
                                self.generate_reject_ordered_set_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    non_empty,
                                    bounds.and_then(|bounds| {
                                        let ty: crate::intermediate::RustType =
                                            crate::intermediate::ConceptualRustType::Array(
                                                Box::new(element_type.clone()),
                                            )
                                            .into();
                                        // Preserve the rule's uniqueness policy while reconstructing
                                        // its occurrence carrier. Without it, an exact `0*0` reject
                                        // set looks like an ordinary static array and disappears
                                        // from the bounded-set branch as a loose OrderedSet.
                                        ty.with_bounds(bounds)
                                            .with_duplicates_policy(Some(
                                                crate::comment_ast::DuplicatesPolicy::Reject,
                                            ))
                                            .bounded_array_u64_bounds()
                                    }),
                                    // See the non-empty arm below: a generator-synthesized
                                    // collection (a table rule's keys-list) must never claim
                                    // `rule_declared` — no rule authored that wrapper, so the
                                    // criterion-9 shadow warning would be about nobody's class.
                                    !types.is_synthesized_collection(rust_ident),
                                    cli,
                                );
                            } else if non_empty {
                                // named `[+ T]` rule: its JS class is the RESTRICTED wrapper (wrapping
                                // core::NonEmptyVec) under the rule ident, not the loose list wrapper.
                                self.generate_non_empty_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    // A rule authored the class UNLESS this Array struct was
                                    // generator-synthesized (a table rule's keys-list): a synthesized
                                    // keys-list must never claim `rule_declared` (criterion-9 shadow
                                    // warning over a wrapper no rule declares). A synthesized keys-list
                                    // is always `bounds: None`, so this arm is authored in practice;
                                    // pass the computed value for consistency with the plain arm.
                                    !types.is_synthesized_collection(rust_ident),
                                    cli,
                                );
                            } else if let Some((min, max)) = {
                                let ty: crate::intermediate::RustType =
                                    crate::intermediate::ConceptualRustType::Array(Box::new(
                                        element_type.clone(),
                                    ))
                                    .into();
                                bounds.and_then(|bounds| {
                                    let ty = ty.with_bounds(bounds);
                                    ty.exact_homogeneous_array_u64_bounds()
                                        .or_else(|| ty.bounded_array_u64_bounds())
                                })
                            } {
                                self.generate_bounded_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    (min, max),
                                    !types.is_synthesized_collection(rust_ident),
                                    cli,
                                );
                            } else {
                                self.generate_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    // See the non-empty arm: a generator-synthesized keys-list
                                    // (`create_and_register_array_type`) must not pass
                                    // `rule_declared: true` — no rule declares it, so the workspace
                                    // criterion-9 shadow warning must not fire for it.
                                    !types.is_synthesized_collection(rust_ident),
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), element_type.name_as_rust_array(false)));
                    }
                    RustStructType::TypeChoice { variants } => {
                        self.generate_type_choices_from_variants(
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::GroupChoice { variants, rep } => codegen_group_choices(
                        self,
                        types,
                        rust_ident,
                        variants,
                        *rep,
                        rust_struct.tag(),
                        rust_struct.config(),
                        cli,
                    ),
                    RustStructType::Wrapper {
                        wrapped,
                        min_max,
                        float_min_max,
                    } => match rust_struct.tag() {
                        // A nominalized two-arm set idiom carries an OPTIONAL tag: attach
                        // `OptionallyTagged` (a `TagPresenceEncoding` record) rather than the mandatory
                        // `Tagged`, so either wire arm round-trips byte-exact — grammar decides the tag
                        // record. Every other tagged wrapper (single-arm mandatory-tag set, bare
                        // `@newtype` over a tag) keeps `Tagged`.
                        Some(tag) => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            &if rust_struct.tag_optional() {
                                wrapped.clone().optionally_tag(tag)
                            } else {
                                wrapped.clone().tag(tag)
                            },
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                        None => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            wrapped,
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                    },
                    RustStructType::Extern => {
                        #[allow(clippy::single_match)]
                        match rust_ident.to_string().as_ref() {
                            // Emit `Int` when the spec references it, OR when a `--key-requests` row
                            // demanded it used-as-key (a dep whose own spec never references `int` but
                            // whose consumer keys a map on `int` under `--common-import-override`): the
                            // demand alone must force key-flavored emission, since `is_referenced`'s
                            // reference walk would otherwise skip it.
                            "Int"
                                if types.is_referenced(rust_ident)
                                    || types.used_as_key(rust_ident) =>
                            {
                                generate_int(self, types, cli)
                            }
                            _ => (), /* user-specified external types */
                        }
                    }
                    RustStructType::CStyleEnum { variants } => {
                        generate_c_style_enum(
                            self,
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::RawBytesType => {
                        // nothing to do, user specified
                    }
                }
            }

            // Structural wrappers reachable ONLY through a wasm-emitted plain `pub type` alias, never
            // through any rust struct — e.g. `x = bytes .cbor { bignint => uint }`, where `x` is a type
            // alias (not a struct). Its `Map` target is embedded elsewhere only as `Alias(Rust(x), Map)`,
            // and `x` sits in `existing_aliases`, so the rust-struct walk above never descends into that
            // Map — leaving the emitted `pub type X = MapKToV` alias naming a class no one minted. Walk
            // each wasm-alias base type through the same minting path (shared `wasm_wrappers_generated` /
            // `existing_aliases`, so it stays idempotent with the walk above and self-referential/other
            // named aliases are not re-descended).
            if cli.wasm {
                for (alias_ident, alias_info) in types.type_aliases() {
                    if matches!(alias_ident, AliasIdent::Rust(_)) && alias_info.gen_wasm_alias {
                        let base = &alias_info.base_type;
                        // The base type's OWN top-level map carries the rule's `@duplicates` policy
                        // (`with_duplicates_policy` at registration) — a named/instantiated
                        // `@duplicates preserve` table's alias line names `PairMapKToV`, so the mint
                        // must be the pair-map-flavored one. The conceptual visitor below is
                        // policy-blind, so mint that top level here from the RustType and walk only
                        // the INNER key/value with the visitor (whose nested maps are inline
                        // occurrences, always default-flavored).
                        if base.is_preserve_pair_map()
                            && let ConceptualRustType::Map(k, v) = &base.conceptual_type
                        {
                            mint_wasm_wrapper_for_visited_type(
                                self,
                                types,
                                &base.conceptual_type,
                                &mut wasm_wrappers_generated,
                                &table_shape_sole_owner,
                                true,
                                cli,
                            );
                            for inner in [k, v] {
                                inner.conceptual_type.visit_types_excluding(
                                    types,
                                    &mut |ty| {
                                        mint_wasm_wrapper_for_visited_type(
                                            self,
                                            types,
                                            ty,
                                            &mut wasm_wrappers_generated,
                                            &table_shape_sole_owner,
                                            false,
                                            cli,
                                        )
                                    },
                                    &mut existing_aliases,
                                );
                            }
                            continue;
                        }
                        base.conceptual_type.visit_types_excluding(
                            types,
                            &mut |ty| {
                                mint_wasm_wrapper_for_visited_type(
                                    self,
                                    types,
                                    ty,
                                    &mut wasm_wrappers_generated,
                                    &table_shape_sole_owner,
                                    false,
                                    cli,
                                )
                            },
                            &mut existing_aliases,
                        );
                    }
                }
            }
        }

        // `@used_as_elem`: mint the loose-list wasm wrapper (`<Elem>List`, the `[* elem]` equivalent)
        // for each tagged element, exactly as an inline `[* elem]` usage would. Runs AFTER the
        // own-spec wasm walk (so a real inline usage that already minted the wrapper dedups via the
        // shared `already_generated`) and BEFORE `emit_requested_collections` (so the wrapper is
        // recorded in the own-spec shape projection, letting a consumer's request for the same shape be
        // satisfied by this crate's own spec instead of re-emitted into requested_collections). The
        // mark set is a `BTreeSet`, so this walks idents in sorted order — deterministic output. A
        // directly-wasm-exposable element has no wrapper and is rejected in `finalize`, so nothing
        // exposable reaches here. `try_defer_wrapper` inside applies normally: if a workspace dep
        // owns the element, deferring to the dep is the correct canonical-host semantics.
        if cli.wasm {
            for ident in types.used_as_elem() {
                let element_type = types.used_as_elem_element_type(ident);
                let structural =
                    RustIdent::new(CDDLIdent::new(element_type.name_as_wasm_array(types)));
                self.generate_array_type(types, element_type, &structural, false, cli);
            }
        }

        // W2 dep side (`--wrapper-requests`): now that the OWN-spec wasm wrapper walk is complete
        // (the wrapper registry's local classes and own-spec shape projection fully populated), read the consumer
        // sidecars, union the requested shapes, and emit each requested wrapper the dep does not
        // already produce into the `requested_collections` module. Wasm-only, and a no-op (byte
        // identical) with no `--wrapper-requests` flag.
        if cli.wasm {
            self.emit_requested_collections(types, cli)?;
        }

        // wasm face of the `AnyCbor` runtime type (CDDL `any`). Keyed on `uses_any_cbor()`, not on
        // an ident reference (`AnyCbor` is a static-runtime type, never a `RustStruct`), so it is a
        // direct prelude call rather than a `RustStructType::Extern` match arm like `Int`. Placed
        // after the collection-wrapper walk so a `MapAnyToAny`/`AnyList` wrapper is already minted;
        // the wrapper fn itself branches on `--common-import-override`.
        if cli.wasm && types.uses_any_cbor() {
            generate_any_cbor_wasm(self, types, cli);
        }

        // JSON export crate. `json_lines` is the BODY of the emitted `add_schemas(generator)` — one
        // registration row per exported type. The surrounding `export_schemas()` (which owns the
        // `schemas/` dir and writes the single document) is built in `generation/export.rs`.
        if cli.json_schema_export {
            let mut main_lines_by_file: BTreeMap<ModuleScope, Vec<String>> = BTreeMap::new();
            let mut row_roots = BTreeSet::new();
            // A generic-extern BASE (`ext_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_`) names no concrete
            // type, so a row naming the bare `ExtSet` is E0107 no matter what the user writes —
            // the same class the extern-interface self-check skips (`ExternCheckKind::None`). Its
            // concrete instances (`my_set = ext_set<uint>` -> `MySet`) get their own rows and are
            // kept. Keyed on `generic_extern_base_idents()` (the union of the parse-time record and
            // the usage-site instances) so BOTH a base with ZERO instances (`ext_unused<T>`) and a
            // base declared plain-but-used-generic (`extern_generic<..>`, tests/core) are skipped.
            let generic_bases = types.generic_extern_base_idents();
            for (rust_ident, rust_struct) in types.rust_structs() {
                let is_typedef = matches!(
                    rust_struct.variant(),
                    RustStructType::Array { .. } | RustStructType::Table { .. }
                );
                // The is_referenced check is for things like Int which are included by default
                // in order for the CDDL to parse but might not be used.
                // However, we need to export other root types from the user's spec
                if is_typedef || (rust_ident.as_ref() == "Int" && !types.is_referenced(rust_ident))
                {
                    continue;
                }
                // Skip the generic-extern base (see above).
                if generic_bases.contains(rust_ident) {
                    continue;
                }
                // Skip types owned by a non-exported (cross-crate extern-dep) scope: the emitted
                // path would be `dep_crate::sub::Thing`, but this json-gen crate's `Cargo.toml`
                // depends only on the own rust crate (E0433), and by design each crate's OWN json-gen
                // run exports its own schemas — the consumer must not re-export a dependency's.
                if !types.scope(rust_ident).export() {
                    continue;
                }
                // Skip a rule the SPEC AUTHOR declared out of the published JSON surface
                // (`@no_json_schema_export`). Unlike the four skips above — each a property the tool
                // derives from the IR — this one is not derivable: a type having a derivable JSON
                // schema is not evidence that the derived shape is that type's published encoding. A
                // `serde`/`schemars` derive can exist as an artifact while the real encoding is
                // produced by a PARENT's hand-written impl (the row would then publish a
                // contradictory shape), the type's `JsonSchema` impl can be a deliberate stub (a junk
                // row), or — for an own-spec extern — the hand-written rust type may have no
                // `JsonSchema` impl at all, making the row an `E0277` inside a generated file. The
                // tool cannot tell those apart from a genuine schema root; the spec author can.
                if types.is_no_json_schema_export(rust_ident) {
                    continue;
                }
                main_lines_by_file
                    .entry(types.scope(rust_ident).clone())
                    .or_default()
                    .push(format!(
                        "reg.add::<{}>();",
                        rust_crate_struct_from_wasm(types, rust_ident, cli)
                    ));
                row_roots.insert(rust_ident.clone());
            }
            // A row's `subschema_for` recursively enters definitions while the generated type's
            // schema body runs. Claim the same-crate, nameable candidates first, without invoking
            // schemars' mutating `subschema_for` early: the registrar then catches a collision
            // between two row-less descendants even when their default `schema_id()` silently
            // merges them. This is deliberately separate from CLI roots: their paths have no IR.
            for claim in json_schema_reachable_claims(types, &row_roots, cli) {
                self.json_lines.line(&claim);
            }
            // `AnyCbor` (CDDL `any`) is a static-runtime type, not a `RustStruct`, so the loop above
            // never emits its registration row. Nothing else reaches it either: a GENERATED type
            // describes an `any`-typed member with the NATURAL rendering's permissive schema
            // (`#[schemars(schema_with = "…::natural_any_cbor_schema")]`), which never names
            // `AnyCbor`. So `AnyCbor`'s own tagged-`oneOf` schema — the one describing the `AnyCbor`
            // wasm wrapper's `to_json` surface — enters the document ONLY through this row. Only in
            // the own-static crate: under `--common-import-override` `AnyCbor` lives in the common
            // crate, whose own json-gen run exports its schema (the same "each crate exports only its
            // own schemas" rule the non-export-scope skip enforces).
            if types.uses_any_cbor() && cli.export_static_files() {
                main_lines_by_file
                    .entry((*ROOT_SCOPE).clone())
                    .or_default()
                    .push(format!(
                        "reg.add::<{}::any_cbor::AnyCbor>();",
                        cli.lib_name_code()
                    ));
            }
            let multiple_files = main_lines_by_file.len() > 1;
            for (scope_name, lines) in main_lines_by_file {
                if multiple_files {
                    self.json_lines.line(&format!("// {scope_name}"));
                }
                for line in lines {
                    self.json_lines.line(&line);
                }
            }
            // `--json-schema-root` extra roots: a published type the CDDL never describes (a
            // hand-written address/key type, or one owned by a crate with no spec at all). The value
            // is a user-supplied RUST path emitted verbatim — the flag consults no IR whatsoever, so
            // a path naming a type whose rule carries `@no_json_schema_export` re-registers it, and
            // an unresolvable path is an E0433/E0412 in the consumer's json-gen build rather than a
            // generation-time reject (cddl-codegen does not typecheck Rust).
            //
            // AFTER every spec-derived row: registration order decides which side of a published-name
            // collision the injectivity guard names, so with the CLI roots last a spec-derived row
            // keeps its own name and the guard blames the CLI-supplied path — the one the user can
            // change without touching their spec.
            //
            // FLAG ORDER, never sorted: the flag list is an input, so preserving it keeps "same
            // inputs -> same bytes" while staying readable; sorting would reorder registration, which
            // is observable through the guard's messages.
            //
            // No banner comment above the block: this file is inside the comment-preservation
            // overlay's tree, and a comment above rows that all vanish when the flag is dropped is
            // the stranded-comment/`unpreserved-comment` trap class. The rows carry their own meaning.
            for root in &cli.json_schema_root {
                self.json_lines.line(&format!("reg.add::<{root}>();"));
            }
        }

        // imports / module declarations
        // this is done at the end so we already know all information about output code

        // rust. The codegen provenance header is stamped once per emitted FILE (see
        // `generated_files` / `export`), not per scope — a scope-level raw would hoist above the
        // module-linking raws that `merge_scopes_to_strings` prepends into a merged root file.
        //
        // These lints are module-scoped rather than detected per-site because their triggers are
        // intrinsic to the emitted shape, and the fix each lint suggests would distort the generated
        // public API: CDDL `/` choices become enums whose variant sizes are wildly asymmetric (a bare
        // newtype next to a large record), and boxing the big variant to satisfy `large_enum_variant`
        // would change the type's public shape; every fallible generated API returns
        // `Result<_, DeserializeError>`, a static error type sitting near `result_large_err`'s size
        // threshold that boxing likewise can't fix without altering signatures. Scoping to the
        // generated module (not the crate) keeps all three lints live for hand-written code in
        // consuming crates, matching the `too_many_arguments` precedent.
        self.rust_lib().raw(
            "#![allow(clippy::too_many_arguments, clippy::large_enum_variant, clippy::result_large_err)]\n",
        );

        // declare modules (root lib specific)
        if cli.export_static_files() {
            self.rust_lib().raw("pub mod error;");
            if cli.preserve_encodings {
                self.rust_lib().raw("pub mod ordered_hash_map;");
            }
            // only crates that actually use `[+ T]` pull in the NonEmptyVec runtime — keeps every
            // non-`+` crate's output byte-identical. `--wrapper-requests`: a dep hosting a requested
            // NonEmpty wrapper needs the runtime module even when its own spec has no `[+ …]`.
            if types.uses_non_empty_vec() || self.requested_non_empty_vec {
                self.rust_lib().raw("pub mod non_empty;");
            }
            if types.uses_bounded_vec() || self.requested_bounded_vec {
                self.rust_lib().raw("pub mod bounded;");
            }
            if types.uses_bounded_map() || self.requested_bounded_map {
                self.rust_lib().raw("pub mod bounded_map;");
            }
            // only crates that actually use `{+ k => v}` pull in the NonEmptyMap runtime
            if types.uses_non_empty_map() || self.requested_non_empty_map {
                self.rust_lib().raw("pub mod non_empty_map;");
            }
            // only crates that actually use `@duplicates reject` sets pull in the OrderedSet runtime
            if types.uses_ordered_set() || self.requested_ordered_set {
                self.rust_lib().raw("pub mod ordered_set;");
            }
            // only crates that actually use `@duplicates preserve` tables pull in the PairMap runtime
            if types.uses_pair_map() || self.requested_pair_map {
                self.rust_lib().raw("pub mod pair_map;");
            }
            // only crates that actually use CDDL `any` pull in the AnyCbor runtime — keeps every
            // non-`any` crate's output byte-identical (usage-gating). Present in BOTH modes (the
            // non-preserve variant is a distinct fragment), so gated on usage alone, not preserve.
            if types.uses_any_cbor() {
                self.rust_lib().raw("pub mod any_cbor;");
            }
            if (cli.json_serde_derives || cli.json_schema_export) && types.uses_static_exact_array()
            {
                self.rust_lib().raw("pub mod static_array;");
            }
            // only crates with an open struct-map rest row pull in the flatten JSON helpers, and only
            // under a json flag — keeps every other crate's output byte-identical. Either flag: the
            // module carries the serde flatten mechanics under --json-serde-derives and the rest-row
            // schema helper under --json-schema-export, each its own fragment (see
            // `composed_runtime_static_files`), so a schema-only crate gets the helper without the
            // serde-dependent half.
            if (cli.json_serde_derives || cli.json_schema_export) && types.uses_open_struct_rest() {
                self.rust_lib().raw("pub mod open_struct_rest_json;");
            }
            // only crates with an optional-AND-nullable member pull in the double-`Option` serde
            // adapter — keeps every other crate's output byte-identical. Serde-only (unlike the
            // module above): `--json-schema-export` adds no annotation for the shape, because the
            // schema the derive already produces for it is the honest one.
            if cli.json_serde_derives && types.uses_double_option() {
                self.rust_lib().raw("pub mod double_option;");
            }
            // the honest `serde_json::Value`/`Number` serializer walk. Flag-gated, never spec-gated
            // (like `json_schema_gen` below, unlike the runtimes above): it is a published API for
            // hand-written `Serialize` impls on extern / `@custom_json` types, which need it whether
            // or not the spec uses `any`. The `any_cbor` runtime also routes its natural JSON walk
            // through it.
            if cli.json_serde_derives {
                self.rust_lib().raw("pub mod json_value_ser;");
            }
            // the json-gen crate's row registrar + reference-closure check, which THIS crate never
            // calls — it hosts them so every json-gen crate pointed at this runtime shares one copy.
            // Flag-gated, never spec-gated (unlike the runtimes above): a json-gen crate that imports
            // them exists exactly when `--json-schema-export` is on, whatever the spec holds.
            if cli.json_schema_export {
                self.rust_lib().raw("pub mod json_schema_gen;");
            }
        }
        if cli.preserve_encodings {
            self.rust_lib().raw("extern crate derivative;");
        }
        // declare common modules in each module (struct files). serialization / cbor_encodings are
        // each declared only where the corresponding .rs is actually emitted (mirror the conditions
        // in generated_files / merge_scopes_to_strings): declaring a `pub mod` with no backing file
        // is E0583, uncompilable.
        //   - serialization.rs: the root always materializes one (the static prelude is prepended
        //     unconditionally — merge_scopes_to_strings always writes the root file), and a non-root
        //     scope only when it has generated serialize impls (`serialize_scopes`). An alias/enum-only
        //     non-root module (scalar/collection/table alias, or a c-style enum whose serialization is
        //     emitted elsewhere) produces no serialization.rs, so an unconditional decl was E0583.
        //   - cbor_encodings.rs: a scope with no encoding structs (e.g. a root of only c-style enums)
        //     emits no such file, so the decl is conditioned on `cbor_encodings_scopes` the same way.
        //
        // The root's entry is MATERIALIZED first rather than assumed present: a spec whose rules are
        // ALL `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` markers registers no
        // generated struct, so nothing has created a root `rust_scopes` entry by the time this loop
        // runs — the extern re-export glue below is what creates it. The loop then declared nothing
        // while `merge_scopes_to_strings` still wrote `generated/serialization.rs` AND
        // `extern_interface_check.rs` still named `crate::generated::serialization::RawBytesEncoding`,
        // so the crate failed its own build with E0433 and no user-supplied definition could fix it.
        // `or_default()` on an entry the ordinary path already created is a no-op, so every other
        // crate's emitted byte order is unchanged.
        self.rust_scopes.entry((*ROOT_SCOPE).clone()).or_default();
        for (scope, content) in self.rust_scopes.iter_mut() {
            if *scope == *ROOT_SCOPE || self.serialize_scopes.contains_key(scope) {
                content.raw("pub mod serialization;");
            }
            if cli.preserve_encodings
                && scope.export()
                && self.cbor_encodings_scopes.contains_key(scope)
            {
                content.raw("pub mod cbor_encodings;");
            }
        }

        // Extern-type re-export glue. Generated code refers to each in-crate extern type by its bare
        // ident within the scope that declared it (and cross-scope as `crate::generated::<scope>::Name`;
        // the serializer sees it via `use super::*;`). Under the thin-root split the user cannot inject
        // that definition into `generated/**` (clobbered every run), so the contract is to DEFINE the
        // extern in a hand-written module and RE-EXPORT it at the crate root (`pub use utils::Name;` in
        // the thin `lib.rs`). Re-export it from crate root INTO the declaring scope's generated module so
        // every such bare/`use super::*` reference resolves; the explicit `pub use crate::Name;` binds to
        // the user's definition and beats the `pub use generated::*;` glob cycle. Emitted unconditionally
        // — under `--common-import-override` the extern is still crate-local. Covers BOTH user-supplied
        // extern flavors — `_CDDL_CODEGEN_EXTERN_TYPE_` (`Extern`) and `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
        // (`RawBytesType`) — the contract is identical, and a raw-bytes rule referenced only through
        // `pub type` aliases has NO other resolution path (the alias target is a bare ident, and the
        // struct-field import walk never sees alias-only references — proven by CML cip36's
        // `public_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_` aliases failing E0412 on regen). Skipped:
        //   - the built-in `Int` extern (the tool generates its definition when referenced),
        //   - generic-extern instances that already emit a `pub type` alias in this module (the base
        //     generic extern carries the glue instead — re-exporting the aliased name would collide),
        //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes; those resolve through their dep
        //     crate already — `ModuleScope::export()` is the discriminator).
        let rust_aliased: BTreeSet<&RustIdent> = types
            .type_aliases()
            .iter()
            .filter_map(|(alias_ident, info)| match alias_ident {
                AliasIdent::Rust(ident) if info.emits_rust_alias() => Some(ident),
                _ => None,
            })
            .collect();
        let mut externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> = BTreeMap::new();
        for (rust_ident, rust_struct) in types.rust_structs() {
            if matches!(
                rust_struct.variant(),
                RustStructType::Extern | RustStructType::RawBytesType
            ) && rust_ident.as_ref() != "Int"
                && !rust_aliased.contains(rust_ident)
            {
                let scope = types.scope(rust_ident);
                if scope.export() {
                    externs_by_scope
                        .entry(scope.clone())
                        .or_default()
                        .insert(rust_ident.clone());
                }
            }
        }
        // Scopes that have already received the contract comment, so the `@raw_bytes_flavor` loop
        // below doesn't emit a second comment into a scope whose base extern already carried one.
        let mut rust_glue_commented: BTreeSet<ModuleScope> = BTreeSet::new();
        for (scope, idents) in &externs_by_scope {
            let content = self.rust_scopes.entry(scope.clone()).or_default();
            content.raw(EXTERN_REEXPORT_CONTRACT_COMMENT);
            rust_glue_commented.insert(scope.clone());
            for ident in idents {
                content.raw(format!("pub use crate::{ident};"));
                // Collected at the emission site (single source of truth) so the surfaced required
                // set can never drift from the glue actually emitted.
                self.required_rust_reexports.insert(ident.to_string());
            }
        }
        // `@raw_bytes_flavor` re-export glue. A tagged extern generic instantiated with a raw-bytes
        // argument aliases the user-owned `<Base>RawBytes` wrapper flavor (the `pub type` alias sits
        // in the declaring scope's module), so that scope needs `pub use crate::<Base>RawBytes;` too
        // — in ADDITION to the base `pub use crate::<Base>;` above when other instances use the plain
        // name. The flavored name isn't a registered struct (the flavor is user-owned, like the base
        // generic extern's wasm side), so it's emitted here from the recorded-emitted set rather than
        // by the struct loop; placed in the SAME scope as the base extern so the alias resolves.
        for base in types.raw_bytes_flavor_emitted() {
            let scope = types.scope(base);
            if scope.export() {
                let content = self.rust_scopes.entry(scope.clone()).or_default();
                if rust_glue_commented.insert(scope.clone()) {
                    content.raw(EXTERN_REEXPORT_CONTRACT_COMMENT);
                }
                content.raw(format!("pub use crate::{base}RawBytes;"));
                self.required_rust_reexports
                    .insert(format!("{base}RawBytes"));
            }
        }

        // Declare the per-scope modules AFTER the extern / `@raw_bytes_flavor` glue above: an
        // extern-ONLY scope (all its rules are `_CDDL_CODEGEN_EXTERN_TYPE_`) has no generated struct,
        // so the glue's `rust_scopes.entry(..).or_default()` is the ONLY thing that creates its scope
        // entry — snapshotting `rust_scopes.keys()` before the glue would emit that scope's
        // `generated/<scope>/mod.rs` (the re-export glue) yet never declare `pub mod <scope>;`,
        // leaving the root's `use <scope>::…;` referring to an undeclared module (E0432). `rust_lib`
        // ordering is unchanged: nothing between the old and new positions writes `rust_lib`.
        let scope_names = self
            .rust_scopes
            .keys()
            .filter(|scope| **scope != *ROOT_SCOPE)
            .cloned()
            .collect::<Vec<_>>();
        for scope in scope_names
            .iter()
            .filter_map(|s| {
                if s.export() {
                    s.components().first()
                } else {
                    None
                }
            })
            .collect::<BTreeSet<_>>()
        {
            self.rust_lib().raw(format!("pub mod {scope};"));
        }

        // The extern-interface compiled self-check module (materialized as
        // `generated/extern_interface_check.rs` in `generated_files`). UNCONDITIONAL — declared in
        // every mode, exactly like the extern-interface export it guards (the analogy is the
        // commitment level, not gating): it asserts every exported name is a real, correctly-typed
        // surface in THIS crate, so a hand-edited/stale export or a projection bug fails THIS crate's
        // build. PRIVATE (`mod`): its assertions are compile-time-only self-checks, nothing re-exported.
        self.rust_lib().raw("mod extern_interface_check;");

        // The borrowed-key-types sidecar module (materialized as `generated/borrowed_key_types.rs` in
        // `generated_files`). RUST crate, not wasm — key derives are a rust-crate concern (the
        // consumer's rust crate is what fails to build without them). PRIVATE (`mod`): its
        // `BORROWED_KEY_TYPES` const is `pub(crate)`-machine-read output and the compiled self-check
        // fails THIS crate's build if a dep drops a derive; nothing is re-exported. Declared whenever
        // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is borrowed.
        if !self.workspace_deps.is_empty() {
            self.rust_lib().raw("mod borrowed_key_types;");
        }

        // The key-demand assertions module (materialized as `generated/key_demand_assertions.rs` in
        // `generated_files`), declared only when some `@used_as_key` root — flavored or bare — exists,
        // so a key-free crate emits neither the decl nor the file. PRIVATE (`mod`): its `_demand_*`
        // fns are compile-time-only self-checks.
        if !assertion_roots(types).is_empty() {
            self.rust_lib().raw("mod key_demand_assertions;");
        }

        // general common imports (struct files)
        for content in self.rust_scopes.values_mut() {
            // `error::*` covers the error types the fallible conversions in these files reference.
            // `TryFrom` itself is NOT imported: it lives in the edition-2021+ prelude and every
            // generated crate pins `edition = "2024"` (`static/Cargo_rust.toml`, force-`Set` by the
            // manifest changeset), so an explicit import would only be an unused-import wall in
            // files that never do a fallible conversion.
            content.push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            // in case we store these in enums we're just going to dump them in everywhere
            if cli.preserve_encodings {
                content
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "TagPresenceEncoding",
                        None,
                    );
            }
        }

        // cbor_encodings imports
        if cli.preserve_encodings {
            // `BTreeMap` is pushed into every cbor_encodings file unconditionally; the prune pass
            // (`import_prune::prune_generated_files`, run in `generated_files`) drops it from files
            // whose module family doesn't name it. Dumb-push + central prune — see the struct loop
            // below.
            for content in self.cbor_encodings_scopes.values_mut() {
                content
                    // encoding structs can reference GENERATED types (a table keyed by a
                    // type-choice enum stores `BTreeMap<KeyEnum, StringEncoding>`), so like
                    // serialization.rs this needs the scope module's items — `super::*` also
                    // covers cross-scope keys, since a child glob re-imports the parent struct
                    // file's `use` bindings (the scope_references imports pushed above)
                    .push_import("super", "*", None)
                    .push_import("alloc::collections", "BTreeMap", None)
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "TagPresenceEncoding",
                        None,
                    );
            }
        }

        // import encoding structs (struct files)
        if cli.preserve_encodings {
            for (rust_ident, rust_struct) in types.rust_structs() {
                if match rust_struct.variant() {
                    RustStructType::Record(_) => true,
                    RustStructType::Wrapper { wrapped, .. } => {
                        !(encoding_fields(types, rust_ident.as_ref(), wrapped, true, cli)
                            .is_empty()
                            || (rust_struct.config().custom_serialize.is_some()
                                && rust_struct.config().custom_deserialize.is_some()))
                    }
                    _ => false,
                } {
                    // ALL records have an encoding struct since at minimum they contian
                    // the array or map encoding details so no need to check fields
                    self.rust(types, rust_ident).push_import(
                        "cbor_encodings",
                        format!("{rust_ident}Encoding"),
                        None,
                    );
                }
            }
        }

        fn add_imports_from_scope_refs(
            scope: &ModuleScope,
            content: &mut codegen::Scope,
            imports: &BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            // The crate-root prefix for cross-scope references within the SAME crate: both the rust
            // and wasm crates nest their generated tree one level (`crate::generated`). Root-scope
            // items and non-exported scopes are still reached relatively.
            crate_prefix: &str,
            // Wasm pass only: `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` -> wasm crate name. When a
            // non-exported (cross-crate extern-dep) import scope's leading component is mapped, the
            // wasm import is qualified through the dep's wasm crate instead of its rust crate (the
            // rust type has no wasm-bindgen bindings under the split `<dep>`/`<dep>-wasm` layout).
            // `None` for the rust pass and for unmapped deps => import path stays verbatim.
            extern_wasm_crate_map: Option<&BTreeMap<String, String>>,
            // `@rust_name` pins: a consumer-derived `RustIdent` -> the dependency's own final Rust
            // name. Only extern-dep (non-exported) idents ever appear here. A pinned ident is imported
            // under the dependency's real name and aliased back to the derived spelling
            // (`use <dep>::<Pinned> as <Derived>;`) so every internal reference stays unchanged; the
            // wasm pass aliases identically (the dep's wasm wrapper is named after its rust ident =
            // the pin). Empty map => today's plain imports for every ident.
            rust_name_pins: &BTreeMap<RustIdent, String>,
        ) {
            // might not exist if we don't use stuff from other scopes
            if let Some(scope_imports) = imports.get(scope) {
                for (import_scope, idents) in scope_imports.iter() {
                    let import_scope = if *import_scope == *ROOT_SCOPE {
                        Cow::from(crate_prefix.to_owned())
                    } else if *scope == *ROOT_SCOPE || !import_scope.export() {
                        // Cross-crate extern-dep scopes are non-exported: their leading component is
                        // the dependency crate name. In the wasm pass, remap that component to the
                        // dep's wasm crate when a mapping is present.
                        let components = import_scope.components();
                        match (extern_wasm_crate_map, components.split_first()) {
                            (Some(map), Some((first, rest)))
                                if !import_scope.export() && map.contains_key(first) =>
                            {
                                let wasm_crate = &map[first];
                                if rest.is_empty() {
                                    Cow::from(wasm_crate.clone())
                                } else {
                                    Cow::from(format!("{}::{}", wasm_crate, rest.join("::")))
                                }
                            }
                            _ => Cow::from(import_scope.to_string()),
                        }
                    } else {
                        Cow::from(format!("{crate_prefix}::{import_scope}"))
                    };
                    // Split off `@rust_name`-pinned idents: each is imported under the dependency's
                    // real (pinned) name and aliased back to the consumer-derived spelling, so the
                    // grouped `use` below — and every reference in the emitted body — stay in the
                    // derived name. Only extern-dep idents are ever pinned, so an empty pin map (the
                    // common case) leaves `plain == idents` and the output byte-identical.
                    let mut plain: Vec<&RustIdent> = Vec::new();
                    for ident in idents.iter() {
                        match rust_name_pins.get(ident) {
                            // A pin that MATCHES the consumer-derived spelling imports plainly: an
                            // aliased `use dep::Foo as Foo;` would be noise, and — decisive for the
                            // migration acceptance criterion — a consumer moving from a pinless
                            // hand-stub to a pin-carrying export must produce byte-identical output
                            // whenever the pins agree with today's derivation.
                            Some(pinned) if pinned != ident.as_ref() => {
                                content.push_import(
                                    import_scope.clone(),
                                    pinned.clone(),
                                    Some(ident.as_ref()),
                                );
                            }
                            _ => plain.push(ident),
                        }
                    }
                    #[allow(clippy::comparison_chain)]
                    if plain.len() > 1 {
                        content.push_import(
                            import_scope,
                            format!(
                                "{{{}}}",
                                plain
                                    .iter()
                                    .map(|i| i.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            None,
                        );
                    } else if plain.len() == 1 {
                        content.push_import(import_scope, plain[0].to_string(), None);
                    }
                }
            }
        }
        // imports for generated structs from other files (struct files)
        // The rust pass registers no collection-wrapper class imports (those are wasm-only), so
        // deferral never applies here — pass an empty map so rust output is untouched by the flag.
        let rust_scope_references =
            types.scope_references(false, &BTreeMap::new(), &[], &BTreeSet::new(), None);
        // Record every cross-scope ident these imports push, so the usage-derived prune can name-scan
        // away the ones a referencing module never uses (`scope_references` over-approximates).
        for per_scope in rust_scope_references.imports.values() {
            for idents in per_scope.values() {
                self.scope_ref_import_idents
                    .extend(idents.iter().map(|i| i.to_string()));
            }
        }
        for (scope, content) in self.rust_scopes.iter_mut() {
            add_imports_from_scope_refs(
                scope,
                content,
                &rust_scope_references.imports,
                "crate::generated",
                None,
                types.rust_name_pins(),
            );
            // These collection-type imports are pushed unconditionally (or on spec-global gates)
            // even into files that never reference them: dumb-push here, and the usage-derived
            // prune pass (`import_prune::prune_generated_files`, run once over the whole file map in
            // `generated_files`) removes any that the file's module family doesn't actually name.
            // Deriving the import set from the emitted tokens is sound by construction and lives in
            // one place; predicting per-file need at each of these ~30 sites would have to mirror
            // every local emission decision and drift.
            content.push_import("alloc::collections", "BTreeMap", None);
            if cli.preserve_encodings {
                content.push_import(
                    format!("{}::ordered_hash_map", cli.common_import_rust()),
                    "OrderedHashMap",
                    None,
                );
            }
            if types.uses_non_empty_vec() {
                content.push_import(
                    format!("{}::non_empty", cli.common_import_rust()),
                    "NonEmptyVec",
                    None,
                );
            }
            if types.uses_bounded_vec() || self.requested_bounded_vec {
                content.push_import(
                    format!("{}::bounded", cli.common_import_rust()),
                    "BoundedVec",
                    None,
                );
            }
            if types.uses_bounded_map() || self.requested_bounded_map {
                content.push_import(
                    format!("{}::bounded_map", cli.common_import_rust()),
                    "BoundedMap",
                    None,
                );
            }
            if types.uses_non_empty_map() {
                content.push_import(
                    format!("{}::non_empty_map", cli.common_import_rust()),
                    "NonEmptyMap",
                    None,
                );
            }
            if types.uses_ordered_set() {
                content.push_import(
                    format!("{}::ordered_set", cli.common_import_rust()),
                    "OrderedSet",
                    None,
                );
                content.push_import(
                    format!("{}::ordered_set", cli.common_import_rust()),
                    "NonEmptyOrderedSet",
                    None,
                );
                content.push_import(
                    format!("{}::ordered_set", cli.common_import_rust()),
                    "BoundedOrderedSet",
                    None,
                );
            }
            if types.uses_pair_map() {
                content.push_import(
                    format!("{}::pair_map", cli.common_import_rust()),
                    "PairMap",
                    None,
                );
                content.push_import(
                    format!("{}::pair_map", cli.common_import_rust()),
                    "NonEmptyPairMap",
                    None,
                );
            }
            if types.uses_bounded_pair_map() {
                content.push_import(
                    format!("{}::pair_map", cli.common_import_rust()),
                    "BoundedPairMap",
                    None,
                );
            }
        }

        // serialization
        // The imports every generated serialization.rs needs regardless of scope — the static
        // prelude and all generated impls reference these. Shared by the per-scope loop and the
        // lib-scope fallback below so the set can't drift between the two.
        let push_base_serialize_imports = |scope: &mut codegen::Scope| {
            scope
                .push_import("super", "*", None)
                .push_import("cbor_event::de", "Deserializer", None)
                .push_import("cbor_event::se", "Serializer", None)
                .push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            if !(cli.preserve_encodings && cli.canonical_form) {
                scope.push_import("cbor_event::se", "Serialize", None);
            }
        };
        for (scope, content) in self.serialize_scopes.iter_mut() {
            push_base_serialize_imports(content);
            if let Some(common_import) = cli.common_import_override.as_ref() {
                content.push_import(format!("{}::serialization", common_import), "*", None);
            }
            // Only import cbor_encodings where a cbor_encodings.rs is actually emitted for this
            // scope (same condition as its `pub mod` declaration / generated_files): a scope with
            // serialization but no encoding structs (e.g. a group/type choice) emits no such file,
            // so importing it would be an unresolved import (E0432).
            if cli.preserve_encodings && self.cbor_encodings_scopes.contains_key(scope) {
                content.push_import("super::cbor_encodings", "*", None);
            }
            if *scope != *ROOT_SCOPE {
                content.push_import(
                    format!("{}::serialization", cli.common_import_rust()),
                    "*",
                    None,
                );
            }
        }

        // The static serialization prelude prepended to the root serialization.rs (when we own the
        // static files) references Serializer/Deserializer/DeserializeError/etc. Those
        // imports are added to the ROOT_SCOPE serialize scope by the loop above — but a spec whose
        // root has no per-type serialization (e.g. only c-style enums) produces no ROOT_SCOPE entry,
        // leaving the prelude (and any rust_serialize_lib impls) without imports and the crate
        // uncompilable. Add the base imports to the lib serialize scope in that case. (No
        // cbor_encodings/non-root imports: no ROOT_SCOPE entry means no root struct, so no root
        // encoding struct and nothing cross-module to reach.)
        if cli.export_static_files() && !self.serialize_scopes.contains_key(&*ROOT_SCOPE) {
            push_base_serialize_imports(self.rust_serialize_lib());
        }

        // declare submodules
        // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
        // for these modules when they only exist to support modules nested deeper
        declare_modules(&mut self.rust_scopes, &scope_names);

        // wasm
        if cli.wasm {
            let extern_wasm_crate_map = cli.extern_wasm_crate_map();
            // Validate mapping keys BEFORE emitting: a key that names no accepted crate is almost
            // certainly a typo, and a silent no-op would leave the generated wasm crate pointing at
            // the (non-wasm) rust crate and failing to compile with no hint why.
            //
            // Two key kinds are legitimate:
            //   1. a declared extern dependency (`extern_dep_names()`) — the deferred collection
            //      wrappers route the dep's element/key/value types through the mapped wasm crate;
            //   2. the `--common-import-override` crate — the documented pairing
            //      (`--common-import-override=cml_core --extern-wasm-crate=cml_core=cml_core_wasm`)
            //      routes the built-in `Int`'s WASM face through the mapped wasm crate. That override
            //      crate is common scaffolding, NOT a declared extern dep, so a pure consumer (no
            //      `_CDDL_CODEGEN_EXTERN_DEPS_DIR_`) has an EMPTY `extern_dep_names()` and this is the
            //      only key. `generate_int` is the sole consumer of the override-keyed mapping
            //      (`extern_wasm_crate_map().get(cli.common_import_rust())`); the rust-side
            //      `common_import_wasm()` call sites (serialization / ordered_hash_map / non_empty in
            //      this module and requests.rs) never consult the map, so accepting this key here does
            //      not change any of them.
            if !extern_wasm_crate_map.is_empty() {
                let extern_dep_names = types.extern_dep_names();
                let common_override = cli.common_import_override.as_deref();
                for dep in extern_wasm_crate_map.keys() {
                    let names_extern_dep = extern_dep_names.contains(dep);
                    let names_common_override = common_override == Some(dep.as_str());
                    if !names_extern_dep && !names_common_override {
                        panic!(
                            "--extern-wasm-crate names crate {dep:?}, which is not an extern \
                             dependency in this spec and is not the --common-import-override crate \
                             ({:?}). Accepted keys are the declared extern dependencies {:?} plus \
                             the --common-import-override crate (which routes the built-in Int's \
                             wasm face).",
                            common_override, extern_dep_names
                        );
                    }
                }
            }
            self
            .wasm_lib()
            .raw("#![allow(clippy::len_without_is_empty, clippy::too_many_arguments, clippy::new_without_default)]");
            // wasm imports
            // The registry's deferred-provider map was fully populated during the wasm struct walk above (every
            // deferred wrapper's mint point recorded it), so referencing modules now get a plain
            // `use <dep_wasm>::collections::<Name>;` for each instead of a local class.
            // The `requested_collections` host module (`--wrapper-requests`) hosts wrappers that are not
            // in the IR; hand `scope_references` the hosted set + its scope so their element/key/value
            // wasm classes are imported at that scope (a bare `use super::*;` reaches only the root).
            let requested_scope = ModuleScope::from(vec!["requested_collections".to_owned()]);
            // This is the actual home set, not the requested candidate set: recursive support mints
            // (a NonEmpty wrapper's loose try_from source and a map keys-list) share this file too.
            // Same-file references to any of them must stay bare rather than importing a nonexistent
            // crate-root class. `record_collection_wrapper` is the shared actual-mint seam.
            let requested_hosted: BTreeSet<RustIdent> = self
                .wasm_collection_wrapper_registry
                .local_classes()
                .iter()
                .filter(|(_, definition)| definition.scope == requested_scope)
                .map(|(ident, _)| ident.clone())
                .collect();
            let wasm_scope_references = types.scope_references(
                true,
                self.wasm_collection_wrapper_registry.deferred(),
                &self.requested_wrapper_types,
                &requested_hosted,
                Some(&requested_scope),
            );
            let wasm_imports = &wasm_scope_references.imports;
            let wasm_boundary_idents = &wasm_scope_references.wasm_boundary_idents;
            for per_scope in wasm_imports.values() {
                for idents in per_scope.values() {
                    self.scope_ref_import_idents
                        .extend(idents.iter().map(|i| i.to_string()));
                }
            }
            for (scope, content) in self.wasm_scopes.iter_mut() {
                // imports from other struct modules; the wasm generated tree nests one level under
                // `crate::generated` (same as the rust crate)
                add_imports_from_scope_refs(
                    scope,
                    content,
                    wasm_imports,
                    "crate::generated",
                    Some(&extern_wasm_crate_map),
                    types.rust_name_pins(),
                );
                // common imports. The collection-type imports below (`BTreeMap`/`OrderedHashMap`
                // and the two NonEmpty types) are pushed on spec-global gates even into wasm files
                // that never reference them; the prune pass (`import_prune::prune_generated_files`,
                // in `generated_files`) removes the ones the file's module family doesn't name.
                // Dumb-push + central prune.
                content
                    .push_import("wasm_bindgen::prelude", "wasm_bindgen", None)
                    .push_import("wasm_bindgen::prelude", "JsError", None);
                if cli.json_serde_derives && cli.wasm_cbor_json_api_macro.is_none() {
                    content.push_import("wasm_bindgen::prelude", "JsValue", None);
                }
                if cli.preserve_encodings {
                    content.push_import(
                        format!("{}::ordered_hash_map", cli.common_import_wasm()),
                        "OrderedHashMap",
                        None,
                    );
                } else {
                    content.push_import("std::collections", "BTreeMap", None);
                }
                if types.uses_non_empty_vec() {
                    content.push_import(
                        format!("{}::non_empty", cli.common_import_wasm()),
                        "NonEmptyVec",
                        None,
                    );
                }
                if types.uses_bounded_vec() || self.requested_bounded_vec {
                    content.push_import(
                        format!("{}::bounded", cli.common_import_wasm()),
                        "BoundedVec",
                        None,
                    );
                }
                if types.uses_bounded_map() || self.requested_bounded_map {
                    content.push_import(
                        format!("{}::bounded_map", cli.common_import_wasm()),
                        "BoundedMap",
                        None,
                    );
                }
                if types.uses_non_empty_map() {
                    content.push_import(
                        format!("{}::non_empty_map", cli.common_import_wasm()),
                        "NonEmptyMap",
                        None,
                    );
                }
                if types.uses_ordered_set() {
                    content.push_import(
                        format!("{}::ordered_set", cli.common_import_wasm()),
                        "OrderedSet",
                        None,
                    );
                    content.push_import(
                        format!("{}::ordered_set", cli.common_import_wasm()),
                        "NonEmptyOrderedSet",
                        None,
                    );
                    content.push_import(
                        format!("{}::ordered_set", cli.common_import_wasm()),
                        "BoundedOrderedSet",
                        None,
                    );
                }
                if types.uses_pair_map() {
                    content.push_import(
                        format!("{}::pair_map", cli.common_import_wasm()),
                        "PairMap",
                        None,
                    );
                    content.push_import(
                        format!("{}::pair_map", cli.common_import_wasm()),
                        "NonEmptyPairMap",
                        None,
                    );
                }
                if types.uses_bounded_pair_map() {
                    content.push_import(
                        format!("{}::pair_map", cli.common_import_wasm()),
                        "BoundedPairMap",
                        None,
                    );
                }
                // external macros
                if let Some(cbor_json_macro) = &cli.wasm_cbor_json_api_macro
                    && let Some((path, m)) = cbor_json_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(conversion_macro) = &cli.wasm_conversions_macro
                    && let Some((path, m)) = conversion_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(list_macro) = &cli.wasm_list_macro
                    && let Some((path, m)) = list_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
            }
            // Extern-type re-export glue (wasm crate). The wasm generated code names each REACHABLE
            // in-crate extern by its bare WRAPPER ident within the declaring scope (`req: ExternalFoo`,
            // and via `use super::*;` in nested modules), exactly as the rust crate names the native type
            // — same E0433 shape under the thin-root split, since a crate-root name isn't visible inside
            // `mod generated`. The contract mirrors rust: DEFINE the wasm wrapper in a hand-written
            // wasm-crate module and RE-EXPORT it at the wasm crate root (`pub use utils::Name;`); the tool
            // re-exports it from crate root INTO the declaring scope's generated module only when the IR's
            // wasm boundary-reference walk says generated wasm code names that wrapper. The walk records
            // same-scope references too, while preserving the existing cross-scope import map. Covers BOTH
            // user-supplied extern flavors — `Extern` and `RawBytesType` — exactly like the rust-side glue.
            // Skipped:
            //   - the built-in `Int` extern (the tool generates its own wasm wrapper when referenced, so
            //     `pub use crate::Int;` would collide),
            //   - generic-extern instances that already emit a wasm `pub type` alias here (`gen_wasm_alias`
            //     — the wrapper the alias points at carries the glue instead),
            //   - generic-extern BASES (`Foo` of `Foo<Bar>`): a plain `Extern` rust struct, but wasm never
            //     names it (wasm-bindgen has no generics; the instance collapses to the argument wrapper),
            //     so there is no wasm-crate-root definition to re-export — emitting glue would be an
            //     unresolved import. The rust side keeps the base because its `pub type` alias names it.
            //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes) resolve through their dep crate via
            //     `common_import_wasm()` already — `ModuleScope::export()` is the discriminator.
            let wasm_aliased: BTreeSet<&RustIdent> = types
                .type_aliases()
                .iter()
                .filter_map(|(alias_ident, info)| match alias_ident {
                    AliasIdent::Rust(ident) if info.emits_wasm_alias() => Some(ident),
                    _ => None,
                })
                .collect();
            let generic_bases = types.generic_instance_bases();
            let mut wasm_externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> =
                BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                if matches!(
                    rust_struct.variant(),
                    RustStructType::Extern | RustStructType::RawBytesType
                ) && rust_ident.as_ref() != "Int"
                    && !wasm_aliased.contains(rust_ident)
                    && !generic_bases.contains(rust_ident)
                    && wasm_boundary_idents.contains(rust_ident)
                {
                    let scope = types.scope(rust_ident);
                    if scope.export() {
                        wasm_externs_by_scope
                            .entry(scope.clone())
                            .or_default()
                            .insert(rust_ident.clone());
                    }
                }
            }
            for (scope, idents) in &wasm_externs_by_scope {
                let content = self.wasm_scopes.entry(scope.clone()).or_default();
                content.raw(EXTERN_REEXPORT_CONTRACT_COMMENT);
                for ident in idents {
                    content.raw(format!("pub use crate::{ident};"));
                    // Collected at the emission site (single source of truth), like the rust set.
                    self.required_wasm_reexports.insert(ident.to_string());
                }
            }
            // wasm module declarations. Emitted AFTER the live extern re-export glue above, for the
            // same reason as the rust crate: a scope whose only emitted wasm content is reachable
            // extern glue is created solely by `wasm_scopes.entry(..).or_default()`, so a scope list
            // snapshotted before the glue would materialize that scope's `generated/<scope>/mod.rs` yet
            // never declare `pub mod <scope>;` in the root (E0432). `wasm_lib` ordering is unchanged:
            // nothing between the old and new positions writes `wasm_lib`.
            let wasm_scope_names = self
                .wasm_scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE)
                .cloned()
                .collect::<Vec<_>>();
            for scope in wasm_scope_names
                .iter()
                .filter_map(|s| {
                    if s.export() {
                        s.components().first()
                    } else {
                        None
                    }
                })
                .collect::<BTreeSet<_>>()
            {
                self.wasm_lib().raw(format!("pub mod {scope};"));
            }
            // The collection-wrapper index module (materialized as `generated/collections.rs` in
            // `generated_files`). Declared unconditionally for every wasm run — even one that mints
            // zero wrappers — from the always-regenerated generated root, never the seed-once
            // crate-root lib.rs.
            self.wasm_lib().raw("pub mod collections;");
            // The borrowed-collections sidecar module (materialized as `generated/borrowed_collections.rs`
            // in `generated_files`). PRIVATE (`mod`, never `pub mod`) — its `use` lines only
            // existence-check the borrowed wrapper names; borrowed wrappers are never re-exported (the
            // consumer's own `collections.rs` lists only wrappers it defines). Declared whenever
            // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is
            // borrowed.
            if !self.workspace_deps.is_empty() {
                self.wasm_lib().raw("mod borrowed_collections;");
            }
            // declare submodules
            // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
            // for these modules when they only exist to support modules nested deeper
            declare_modules(&mut self.wasm_scopes, &wasm_scope_names);
        }

        // component crate
        //
        // ONE block, deliberately — not the 30 interleaved `cli.wasm` gates above. The wasm face
        // interleaves because it MINTS types during the walk (collection wrappers whose existence
        // depends on what the walk has already seen); the component face renders from the FINALIZED
        // IR the same way `extern_interface` does, so it needs no walk of its own and gains nothing
        // from being threaded through this one.
        if cli.component {
            // The no-deserialize verdicts are complete by here — the rust face's own walk above is
            // what records them, and the component face runs after it — so the projection can drop
            // the `from-cbor-bytes` seam of a type that has no `Deserialize` impl to bridge to.
            let no_deserialize = self.no_deserialize_idents();
            // WIT strong uniqueness, against the REAL verdict: an interface is one flat namespace
            // and names compare with the `[method]`/`[static]`/`[constructor]` prefixes stripped, so
            // a collision the rust and wasm faces resolve by scoping is a broken WIT package. The
            // `<resource>.<resource>` member case in particular survives BOTH resolve and encode and
            // fails only at binary validation, which is why the tool catches it rather than leaving
            // it to a downstream one. Recorded rather than returned: `generate` populates state and
            // has no error channel; the two producers below it do.
            // The dependencies' committed WIT packages, read ONCE for the whole component face.
            // A read failure is recorded rather than returned for the same reason a collision is:
            // `generate` populates state and has no error channel, and the two producers below it do.
            match crate::component_wit_deps::load(cli) {
                Ok(dep_wits) => self.component_dep_wits = dep_wits,
                Err(msg) => self.component_import_errors.push(msg),
            }
            self.component_name_collisions = super::generation::wit::wit_name_collisions(
                types,
                cli,
                &no_deserialize,
                &self.component_dep_wits,
            );
            let package = super::generation::wit::project(
                types,
                cli,
                &no_deserialize,
                &self.component_dep_wits,
            );
            self.component_import_errors
                .extend(package.import_errors.iter().cloned());
            let glue =
                component::component_glue(types, cli, &no_deserialize, &self.component_dep_wits);
            self.component_lib_scope.raw(glue);
        }

        // optional generated-test module (reject + round-trip halves; off by default, so it
        // doesn't touch the snapshot suite)
        //
        // Multifile output: each test module lands at the generated ROOT (the `raw` below) while
        // the minted values name submodule types bare (`St`, `Bholder`) — `use super::*;` only
        // reaches root-scope items, so the emitters glob-import each declared non-root module
        // (`use super::a::*;`). The lists are derived from the SAME per-crate scope maps the
        // module-declaration loops above consume (`rust_scopes`/`wasm_scopes`, minus root and
        // non-exported extern-dep scopes), so a glob can never name an undeclared module; BTreeMap
        // keys keep the order deterministic. Caveat: glob imports can collide (E0659) if two
        // submodules export the same type name — no matrix cell or corpus fixture does; if a
        // consumer ever hits it, the robust long-term shape is fully-qualified rendering (thread
        // `types.scope(ident)` into `render_rust`/`render_wasm`) instead of globs.
        let submodule_glob_paths = |scopes: &BTreeMap<ModuleScope, codegen::Scope>| -> Vec<String> {
            scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE && scope.export())
                .map(|scope| scope.components().join("::"))
                .collect()
        };
        // Both minters need the no-deserialize verdict: every round-trip they emit goes through
        // `from_cbor_bytes`, so a type the rust face declined to give a `Deserialize` has no
        // round-trip to assert and its mint would not compile.
        let no_deserialize = self.no_deserialize_idents();
        if cli.emit_tests {
            let rust_submodules = submodule_glob_paths(&self.rust_scopes);
            if let Some(test_mod) = crate::emit_tests::emit_generated_tests(
                types,
                cli,
                &rust_submodules,
                &no_deserialize,
            ) {
                self.rust_lib().raw(&test_mod);
            }
        }
        // the wasm-crate counterpart: same MintValue derivation, rendered through the wrapper API +
        // the cddl_lib rust twin (cross-crate byte differential). `#[cfg(test)]` so it's inert for
        // build/check/wasm-pack — only a `cargo test` of the wasm crate compiles and runs it.
        if cli.wasm && cli.emit_tests {
            let wasm_submodules = submodule_glob_paths(&self.wasm_scopes);
            if let Some(test_mod) = crate::emit_tests_wasm::emit_generated_wasm_tests(
                types,
                cli,
                &wasm_submodules,
                &no_deserialize,
            ) {
                self.wasm_lib().raw(&test_mod);
            }
        }
        // The component face has no generated-test renderer yet: a component's surface is only
        // reachable through a runtime that instantiates it, so the round-trip half of the wasm
        // renderer has no in-crate counterpart to assert against. A LOUD skip on the same terms as
        // the wasm module's own (stderr, default verbosity) — a `--emit-tests --component` run that
        // silently emitted nothing would read as a passing test surface that does not exist.
        if cli.component && cli.emit_tests {
            crate::warn!(
                "cddl-codegen --emit-tests: component module skipped (component test emission not yet supported)"
            );
        }
        Ok(())
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated structs and associated traits (besides serialization ones)
    pub fn rust(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.rust_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" rust scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn rust_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_lib_scope
    }

    /// Serialization scope for `ident`
    pub fn rust_serialize(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.serialize_scopes.entry(scope_name).or_default()
    }

    /// Serialization scope for lib.cddl
    /// e.g. for core stuff, or things without an explicit scope like WASM arrays
    pub fn rust_serialize_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_serialize_lib_scope
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated WASM wrapper structs and associated traits
    pub fn wasm(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        // W2 (`--wrapper-requests`): a requested wrapper is not in this dep's IR, so `types.scope`
        // would fall back to the crate root. While the override is set (only around requested-wrapper
        // emission), route it into the dedicated `requested_collections` module instead.
        let scope_name = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).to_owned(),
        };
        self.wasm_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" WASM scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn wasm_lib(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_lib_scope
    }

    /// CBOR encoding scope for `ident` (i.e. *Encoding structs)
    pub fn cbor_encodings(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope = types.scope(ident).clone();
        self.cbor_encodings_scopes.entry(scope).or_default()
    }
}

fn canonical_param(cli: &Cli) -> &'static str {
    if cli.canonical_form {
        ", force_canonical"
    } else {
        ""
    }
}

/// the codegen crate doesn't support proc macros for fields so we need to
/// do this with newlines. codegen takes care of indentation somehow.
fn encoding_var_macros(key_demand: Option<DemandSet>, custom_json: bool, cli: &Cli) -> String {
    let mut ret = if let Some(demand) = key_demand {
        format!(
            "#[derivative({})]\n",
            key_trait_list(demand, true, cli)
                .iter()
                .map(|derive| format!("{derive}=\"ignore\""))
                .collect::<Vec<String>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    if cli.json_serde_derives && !custom_json {
        ret.push_str("#[serde(skip)]\n");
    }
    ret
}

#[derive(Debug, Clone)]
enum BlockOrLine {
    Line(String),
    Block(Block),
}

#[derive(Default, Debug, Clone)]
pub(crate) struct BlocksOrLines(Vec<BlockOrLine>);

impl BlocksOrLines {
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn as_single_line(&self) -> Option<&str> {
        match self.0.len() {
            1 => match &self.0[0] {
                BlockOrLine::Line(line) => Some(line),
                BlockOrLine::Block(_) => None,
            },
            _ => None,
        }
    }
}

impl From<Block> for BlocksOrLines {
    fn from(block: Block) -> Self {
        Self(vec![BlockOrLine::Block(block)])
    }
}

pub(crate) trait CodeBlock {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock;

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock;

    fn push_all(&mut self, contents: BlocksOrLines) -> &mut dyn CodeBlock
    where
        Self: Sized,
    {
        for content in contents.0 {
            match content {
                BlockOrLine::Line(line) => self.line(&line),
                BlockOrLine::Block(block) => self.push_block(block),
            };
        }
        self as &mut dyn CodeBlock
    }
}

impl CodeBlock for codegen::Function {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for Block {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for BlocksOrLines {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Line(line.to_owned()));
        self
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Block(block));
        self
    }
}

trait DataType {
    fn derive(&mut self, derive: &str) -> &mut Self;
}

impl DataType for codegen::Struct {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

impl DataType for codegen::Enum {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

fn create_base_rust_struct(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
    manual_json_impl: bool,
    // A demand UNIONED into the struct's own `key_demand` before deriving. Set nominals pass a full
    // demand (`bare/hash/ord`) so their encodings-ignored `PartialEq/Eq/PartialOrd/Ord/Hash` are
    // always-on — parity with `OrderedSet`'s unconditional derives (rethink fact 5), never dependent
    // on whether the rule is used as a map key. `None` everywhere else (byte-identical).
    force_demand: Option<crate::comment_ast::DemandSet>,
    cli: &Cli,
) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    let key_demand = match (types.key_demand(ident), force_demand) {
        (Some(a), Some(b)) => Some(a.union(b)),
        (a, b) => a.or(b),
    };
    add_struct_derives(&mut s, key_demand, false, false, manual_json_impl, cli);
    let group_impl = codegen::Impl::new(name);
    // TODO: anything here?
    (s, group_impl)
}

/// Formatted string for fully scoped rust crate struct for use from wasm crate
pub fn rust_crate_struct_from_wasm(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    // This full path bypasses the `use`-import seam (which aliases `@rust_name` pins), so it must
    // apply the pin itself: a pinned extern-dep type lives in the dependency's crate under its own
    // (pinned) name, not the consumer-derived one. `rust_crate_struct_scope_from_wasm` yields the
    // dep's `<crate>::<sub>` path; the leaf must be the dep's real name. Pin-less idents (every
    // in-crate type, and hand-stub extern deps) keep the derived spelling.
    let leaf = types.rust_name_pin(ident).unwrap_or(ident.as_ref());
    format!(
        "{}::{}",
        rust_crate_struct_scope_from_wasm(types, ident, cli),
        leaf
    )
}

/// Emit side-effect-free registrar claims for the names schemars can reach while registering the
/// spec's ordinary roots. This intentionally models emitted JSON-schema calls rather than using the
/// general conceptual visitor: hand-written (`@custom_json` / extern) bodies are opaque, map keys
/// enter a schema BODY while values enter through `subschema_for`, wrappers delegate to their inner
/// body's `json_schema`, and natural `any` never reaches `AnyCbor`'s tagged schema.
///
/// The walker is conservative about ownership: it names only crate-exported `RustStruct`s, because
/// json-gen depends only on the current rust crate. A type with no generated struct is followed only
/// through its transparent alias, never guessed from a user-supplied path.
fn json_schema_reachable_claims(
    types: &IntermediateTypes<'_>,
    roots: &BTreeSet<RustIdent>,
    cli: &Cli,
) -> BTreeSet<String> {
    fn claim_named(
        types: &IntermediateTypes<'_>,
        ident: &RustIdent,
        cli: &Cli,
        claims: &mut BTreeSet<String>,
        visited: &mut BTreeSet<RustIdent>,
        generic_bases: &BTreeSet<RustIdent>,
        claim: bool,
    ) {
        if !types.scope(ident).export() || generic_bases.contains(ident) {
            return;
        }
        let Some(rust_struct) = types.rust_struct(ident) else {
            if let Some(alias) = types.resolve_alias(&AliasIdent::Rust(ident.clone())) {
                walk_subschema(types, &alias, cli, claims, visited, generic_bases);
            }
            return;
        };
        if claim {
            claims.insert(format!(
                "reg.claim_reachable::<{}>();",
                rust_crate_struct_from_wasm(types, ident, cli)
            ));
        }
        if !visited.insert(ident.clone())
            || rust_struct.config().custom_json
            || matches!(rust_struct.variant(), RustStructType::Extern)
        {
            return;
        }
        match rust_struct.variant() {
            RustStructType::Record(record) => {
                for field in &record.fields {
                    // A record's natural-any adapter writes the complete field schema itself; it
                    // never invokes the field type's JsonSchema impl. Reuse the emitter's exact
                    // classifier so a later adapter shape cannot make this inventory drift.
                    if !rust_struct.config().custom_json
                        && natural_any_position(&field.rust_type, field.optional, cli).is_none()
                    {
                        walk_subschema(
                            types,
                            &field.rust_type,
                            cli,
                            claims,
                            visited,
                            generic_bases,
                        );
                    }
                }
                for row in record.dynamic_rows() {
                    match &row.kind {
                        // Open-table and general-key rows call only their value helper; the
                        // primitive/any peeked-key path delegates to BTreeMap's key schema body.
                        RestKind::MapEntries { domain, range, .. } => {
                            if !record.is_open_table() && row.map_key_uses_peeked_path(types) {
                                walk_schema_body(
                                    types,
                                    domain,
                                    cli,
                                    claims,
                                    visited,
                                    generic_bases,
                                );
                            }
                            // Both the flattened open-map adapter and the hand-written open-table
                            // schema emit natural JSON directly for an any range, without calling
                            // that alias's tagged JsonSchema implementation.
                            if !matches!(
                                range.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Any
                            ) {
                                walk_subschema(types, range, cli, claims, visited, generic_bases);
                            }
                        }
                        RestKind::ArrayTail { element, .. } => {
                            // The rest-tail field's Seq adapter likewise owns an any element's
                            // schema, including an alias-hidden any.
                            if !matches!(
                                element.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Any
                            ) {
                                walk_subschema(types, element, cli, claims, visited, generic_bases)
                            }
                        }
                    }
                }
            }
            RustStructType::Array { element_type, .. } => {
                walk_subschema(types, element_type, cli, claims, visited, generic_bases)
            }
            RustStructType::Table {
                domain,
                range,
                bounds,
            } => {
                // A named table emits as its registered alias's collection type. Reconstruct that
                // type with the rule-owned bounds/flavor, not merely its raw conceptual Map: the
                // preserve flavor is PairMap and therefore reaches its key through a subschema.
                let mut table = RustType::new(ConceptualRustType::Map(
                    Box::new(domain.clone()),
                    Box::new(range.clone()),
                ));
                if let Some(bounds) = bounds {
                    table = table.with_bounds(*bounds);
                }
                table = table.with_duplicates_policy(rust_struct.config().duplicates);
                walk_schema_body(types, &table, cli, claims, visited, generic_bases);
            }
            RustStructType::TypeChoice { variants, .. }
            | RustStructType::GroupChoice { variants, .. }
            | RustStructType::CStyleEnum { variants } => {
                for variant in variants {
                    match &variant.data {
                        EnumVariantData::RustType(ty) => {
                            // Enum newtype arms have only the direct natural-any adapter (unlike
                            // record fields' broader container classifier). It owns that arm's
                            // complete schema, so the tagged AnyCbor alias is not reachable.
                            if !rust_struct.config().custom_json
                                && matches!(
                                    ty.conceptual_type.resolve_alias_shallow(),
                                    ConceptualRustType::Any
                                )
                            {
                                continue;
                            }
                            walk_subschema(types, ty, cli, claims, visited, generic_bases)
                        }
                        EnumVariantData::Inlined(record) => {
                            for field in &record.fields {
                                walk_subschema(
                                    types,
                                    &field.rust_type,
                                    cli,
                                    claims,
                                    visited,
                                    generic_bases,
                                );
                            }
                        }
                    }
                }
            }
            // Generated wrappers call the wrapped type's `json_schema` body and mirror its
            // `inline_schema`; this follows deeper subschema edges without claiming the wrapped
            // nominal merely because it is stored in the wrapper.
            RustStructType::Wrapper { wrapped, .. } => {
                walk_schema_body(types, wrapped, cli, claims, visited, generic_bases)
            }
            RustStructType::Extern | RustStructType::RawBytesType => {}
        }
    }

    fn walk_subschema(
        types: &IntermediateTypes<'_>,
        ty: &RustType,
        cli: &Cli,
        claims: &mut BTreeSet<String>,
        visited: &mut BTreeSet<RustIdent>,
        generic_bases: &BTreeSet<RustIdent>,
    ) {
        // A recursive static-array descriptor supplies this whole schema body itself. In
        // particular, a nullable alias can lower to `Option<[T; N]>`, which has no standalone
        // JsonSchema implementation on the pinned array-trait versions; following it here would
        // register an impossible `claim_reachable` despite the enclosing callback never asking for
        // that trait.
        if recursive_exact_array_descriptor(types, ty, false, false, cli).is_some() {
            fn walk_descriptor_leaf(
                types: &IntermediateTypes<'_>,
                ty: &RustType,
                cli: &Cli,
                claims: &mut BTreeSet<String>,
                visited: &mut BTreeSet<RustIdent>,
                generic_bases: &BTreeSet<RustIdent>,
            ) {
                match &ty.conceptual_type {
                    // A named transparent alias is only a carrier spelling here. Unfold it so a
                    // `Vec<Alias<Option<[T; N]>>>` reaches a real terminal leaf without claiming
                    // the alias's impossible pinned-array JsonSchema implementation.
                    ConceptualRustType::Rust(ident) => {
                        if let Some(alias) = types.resolve_alias(&AliasIdent::Rust(ident.clone())) {
                            if types.scope(ident).export() {
                                walk_descriptor_leaf(
                                    types,
                                    &alias,
                                    cli,
                                    claims,
                                    visited,
                                    generic_bases,
                                );
                            }
                        } else {
                            claim_named(types, ident, cli, claims, visited, generic_bases, true);
                        }
                    }
                    ConceptualRustType::Alias(_, inner) => {
                        let mut target = ty.clone();
                        target.conceptual_type = (**inner).clone();
                        walk_descriptor_leaf(types, &target, cli, claims, visited, generic_bases);
                    }
                    ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                        walk_descriptor_leaf(types, inner, cli, claims, visited, generic_bases)
                    }
                    // NaturalAny owns a permissive schema directly, so it has no JsonSchema edge.
                    ConceptualRustType::Any
                    | ConceptualRustType::Map(_, _)
                    | ConceptualRustType::Fixed(_)
                    | ConceptualRustType::Primitive(_) => {}
                }
            }
            walk_descriptor_leaf(types, ty, cli, claims, visited, generic_bases);
            return;
        }
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident) => {
                claim_named(types, ident, cli, claims, visited, generic_bases, true)
            }
            ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) => {
                // A generated, export-scope alias is itself a nameable type token at the derived
                // field's `subschema_for` boundary. Claim it, then follow the target as a BODY:
                // aliases share their target's one JsonSchema impl and add no subschema edge.
                // An alias owned by an extern dependency is equally opaque: json-gen does not own
                // that crate's inventory and must not infer claims from the dep-exported target.
                if !types.scope(ident).export() {
                    return;
                }
                // An exact homogeneous array's field annotation supplies its complete schema via
                // the static-array adapter. Its alias has no `JsonSchema` implementation at wide
                // lengths, so claiming it would emit an unsatisfied `claim_reachable::<[T; N]>`.
                if !types.alias_projection_suppressed(ident)
                    && !ty.is_type_enforced_exact_homogeneous_array()
                {
                    claims.insert(format!(
                        "reg.claim_reachable::<{}>();",
                        rust_crate_struct_from_wasm(types, ident, cli)
                    ));
                }
                let mut target = ty.clone();
                target.conceptual_type = (**inner).clone();
                walk_schema_body(types, &target, cli, claims, visited, generic_bases);
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), inner) => {
                let mut target = ty.clone();
                target.conceptual_type = (**inner).clone();
                walk_schema_body(types, &target, cli, claims, visited, generic_bases)
            }
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                walk_subschema(types, inner, cli, claims, visited, generic_bases)
            }
            ConceptualRustType::Map(key, value) => {
                // Schemars' ordinary BTreeMap form invokes the key's schema body and takes a
                // subschema for the value. PairMap is tuple-based and takes subschemas for both.
                if ty.is_preserve_pair_map() {
                    walk_subschema(types, key, cli, claims, visited, generic_bases);
                } else {
                    walk_schema_body(types, key, cli, claims, visited, generic_bases);
                }
                walk_subschema(types, value, cli, claims, visited, generic_bases);
            }
            ConceptualRustType::Any
            | ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_) => {}
        }
    }

    fn walk_schema_body(
        types: &IntermediateTypes<'_>,
        ty: &RustType,
        cli: &Cli,
        claims: &mut BTreeSet<String>,
        visited: &mut BTreeSet<RustIdent>,
        generic_bases: &BTreeSet<RustIdent>,
    ) {
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident) => {
                claim_named(types, ident, cli, claims, visited, generic_bases, false)
            }
            ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) => {
                // A body call on an alias from an extern dependency is still a call into code this
                // crate does not own. Do not turn its transparent target into own-crate claims.
                if !types.scope(ident).export() {
                    return;
                }
                // This is the BODY counterpart of the subschema alias descent above: retain the
                // outer occurrence configuration (pair-map flavor and bounds in particular),
                // replacing only the conceptual node as emitted aliases do.
                let mut target = ty.clone();
                target.conceptual_type = (**inner).clone();
                walk_schema_body(types, &target, cli, claims, visited, generic_bases)
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), inner) => {
                let mut target = ty.clone();
                target.conceptual_type = (**inner).clone();
                walk_schema_body(types, &target, cli, claims, visited, generic_bases)
            }
            // Vec/Option schema bodies reference their element through schemars' subschema path.
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                walk_subschema(types, inner, cli, claims, visited, generic_bases)
            }
            ConceptualRustType::Map(key, value) => {
                if ty.is_preserve_pair_map() {
                    walk_subschema(types, key, cli, claims, visited, generic_bases);
                } else {
                    walk_schema_body(types, key, cli, claims, visited, generic_bases);
                }
                walk_subschema(types, value, cli, claims, visited, generic_bases);
            }
            // Natural any schemas are emitted directly and intentionally never call AnyCbor's
            // tagged schema; there is no named definition to claim.
            ConceptualRustType::Any
            | ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_) => {}
        }
    }

    let mut claims = BTreeSet::new();
    let mut visited = BTreeSet::new();
    let generic_bases = types.generic_extern_base_idents();
    for root in roots {
        // A root itself is claimed by `reg.add`; start at its generated body so a root is not
        // claimed merely for being a root, while a recursive path may still legitimately reach and
        // preclaim it before the rows. Every transitive claim precedes every registration row.
        // `claim_named` with `claim = false` walks every generated schema-body shape without
        // placing the root in the ledger a second time (and stops itself at custom/extern roots).
        claim_named(
            types,
            root,
            cli,
            &mut claims,
            &mut visited,
            &generic_bases,
            false,
        );
    }
    claims
}

pub fn rust_crate_struct_scope_from_wasm(
    types: &IntermediateTypes,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    let scope = types.scope(ident);
    if *scope == *ROOT_SCOPE {
        cli.lib_name_code()
    } else if !scope.export() {
        // A non-exported (cross-crate extern-dep) scope already stores the dependency's crate as its
        // leading component (the `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` prefix is stripped by
        // `ModuleScope::from`), so `dep_crate::sub` is the dep's own rust path. Prefixing the
        // generated crate's own lib name would mint `cddl_lib::dep_crate::sub`, a path that exists in
        // no crate. The rust type lives in the dep's rust crate regardless of the wasm-crate mapping.
        scope.to_string()
    } else {
        format!("{}::{}", cli.lib_name_code(), scope)
    }
}

/// Push a single-field tuple struct's inner field as `pub(crate)`, guarding the over-width case
/// that would otherwise abort generation. This is the ONE owner of that visibility literal and of
/// the `#[rustfmt::skip]` workaround, for BOTH ordinary tuple-struct emission sites: every wasm
/// wrapper (via `WasmWrapper::push_inner_field`) and the rust-crate wrapper under the default
/// profile (`wrappers.rs`). B5-404 scalar windows bypass this helper to remain private. The two
/// callers here are different crates, but the emitted shape — `pub struct <N>(pub(crate) <Type>);`
/// — and therefore the rustfmt hazard are identical, so the predicate and fallback shape have one home.
///
/// `pub(crate)`, not private: on the wasm side wasm_bindgen ignores non-pub fields so the ABI/API
/// surface is unchanged, and ordinary hand-owned wrappers may expose their carrier via `self.0`.
fn push_overwidth_guarded_tuple_field(s: &mut codegen::Struct, ty: codegen::Type) {
    // Render the type exactly as it will be emitted, to measure the one-line width of the tuple
    // field. rustfmt's default max_width is 100; a field line wider than that trips
    // rust-lang/rustfmt#5703 — rustfmt breaks the line right after the field visibility, leaves
    // a trailing space, emits `error[internal]: left behind trailing whitespace`, and exits 1.
    // `rustfmt_generated_string` (export.rs) treats any exit other than 0/3 as fatal, so that
    // aborts the whole generation. The generator targets default rustfmt config (it never reads
    // a consumer's rustfmt.toml), so the literal 100 is correct. The predicate is deliberately
    // `> 100` even though the fatal threshold is a 102-char field line: it is conservative and
    // also suppresses rustfmt's cosmetic double-space artifact on breakable generic types below
    // the fatal threshold.
    let mut rendered_type = String::new();
    ty.fmt(&mut codegen::Formatter::new(&mut rendered_type))
        .expect("Type::fmt into a String is infallible");
    // Field line = 4 (indent) + "pub(crate) " (11) + rendered type + 1 (trailing comma).
    let field_line_width = 4 + "pub(crate) ".len() + rendered_type.len() + 1;
    if field_line_width > 100 {
        // Over-width: freeze the struct with `#[rustfmt::skip]` so rustfmt never gets a chance
        // to hit #5703. The `codegen` builder has no arbitrary-attribute API, so the citation
        // comment and the attribute are smuggled verbatim through the struct's macro slot — the
        // one passthrough that renders its text unwrapped (precedent: the `derivative)]` newline
        // hack elsewhere in this file). This emits directly above `pub struct <N>`; `rustfmt::skip`
        // governs the whole item regardless of its position among the attributes. Removable once
        // the fix PR (#5708) ships and reaches consumers.
        s.r#macro(
            "// rustfmt::skip: rustfmt breaks after the field vis leaving trailing whitespace and errors\n\
             // (rust-lang/rustfmt#5703, fix PR #5708 unmerged). Remove when #5708 ships.\n\
             #[rustfmt::skip]",
        );
        // With the skip in place rustfmt will not lay the field out for us, so we emit the
        // canonical two-line shape ourselves — header line `pub struct <N>(`, the field on its
        // own 4-space-indented line with a trailing comma, then `);` — so a future un-skip (when
        // #5708 ships) is a zero-diff / pure-formatting change. The `codegen` tuple-field emitter
        // puts the whole field on one line, so the leading newline+indent is smuggled through the
        // visibility string and the trailing `,\n` through the type name. The scope emits this
        // struct at column 0, so no extra indentation is added to the continued lines.
        s.tuple_field(
            Some("\n    pub(crate)".to_string()),
            codegen::Type::new(format!("{rendered_type},\n")),
        );
    } else {
        s.tuple_field(Some("pub(crate)".to_string()), ty);
    }
}

#[derive(Debug)]
struct WasmWrapper<'a> {
    ident: &'a RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    // rust -> wasm
    from_wasm: Option<codegen::Impl>,
    // wasm -> rust
    from_native: Option<codegen::Impl>,
    // AsRef
    as_ref: Option<codegen::Impl>,
    // (macro name, macro params)
    macros: Vec<(String, Vec<String>)>,
}

impl<'a> WasmWrapper<'a> {
    fn push(mut self, gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
        // using Scope::raw() for the macro calls would result in them all being included at the top of the
        // file, so we instead use the impl's macro spot to put them before the impl where we want them.
        // (For a standalone invocation with no impl to attach to — the --wasm-list-macro case — the
        // equivalent is Scope::raw_sorted, which sorts the text where a struct of that name would.)
        for (full_name, params) in self.macros {
            let macro_name = full_name.split("::").last().unwrap();
            self.s_impl
                .r#macro(format!("{}!({});\n", macro_name, params.join(", ")));
        }
        self.s_impl.r#macro("#[wasm_bindgen]");
        gen_scope
            .wasm(types, self.ident)
            .push_struct(self.s)
            .push_impl(self.s_impl);
        if let Some(from_wasm) = self.from_wasm {
            gen_scope.wasm(types, self.ident).push_impl(from_wasm);
        }
        if let Some(from_native) = self.from_native {
            gen_scope.wasm(types, self.ident).push_impl(from_native);
        }
        if let Some(as_ref) = self.as_ref {
            gen_scope.wasm(types, self.ident).push_impl(as_ref);
        }
    }

    /// Push the wrapper's single inner tuple field with the uniform wasm-wrapper
    /// visibility policy. This is the ONE owner of the wasm side's CALL into that policy for every
    /// generated wasm wrapper (the named `create_base_wasm_wrapper` path and all four
    /// `collections.rs` wrappers — plain list, non-empty list/map, structural/named map); the
    /// visibility literal itself, and the over-width `#[rustfmt::skip]` workaround around it, live
    /// in `push_overwidth_guarded_tuple_field` below, shared with the rust-crate newtype site.
    ///
    /// `pub(crate)`, not private: wasm_bindgen ignores non-pub fields so the ABI/API surface is
    /// unchanged, while consumer wasm hand files (living outside the always-clobbered generated
    /// subtree under the thin-root layout) can reach the wrapped native value via `self.0`.
    fn push_inner_field<T>(&mut self, ty: T) -> &mut Self
    where
        T: Into<codegen::Type>,
    {
        push_overwidth_guarded_tuple_field(&mut self.s, ty.into());
        self
    }

    /// native_name is &str since we need to possibly prepend namespacing
    /// and where we're calling it we'd have to construct a RustType where we
    /// didn't have to before, but we already had the string.
    fn add_conversion_methods(&mut self, native_name: &str, cli: &Cli) {
        match &cli.wasm_conversions_macro {
            Some(conversion_macro) => {
                self.macros.push((
                    conversion_macro.clone(),
                    vec![native_name.to_owned(), self.ident.to_string()],
                ));
            }
            None => {
                let mut from_wasm = codegen::Impl::new(self.ident.to_string());
                from_wasm
                    .impl_trait(format!("From<{native_name}>"))
                    .new_fn("from")
                    .arg("native", native_name)
                    .ret("Self")
                    .line("Self(native)");
                self.from_wasm = Some(from_wasm);
                let mut from_native = codegen::Impl::new(native_name);
                from_native
                    .impl_trait(format!("From<{}>", self.ident))
                    .new_fn("from")
                    .arg("wasm", self.ident.to_string())
                    .ret("Self")
                    .line("wasm.0");
                self.from_native = Some(from_native);
                let mut as_ref = codegen::Impl::new(self.ident.to_string());
                as_ref
                    .impl_trait(format!("AsRef<{native_name}>"))
                    .new_fn("as_ref")
                    .arg_ref_self()
                    .ret(format!("&{native_name}"))
                    .line("&self.0");
                self.as_ref = Some(as_ref);
            }
        }
    }
}

/// The wasm-bindgen-inherent members that mirror capabilities of the generated rust surface.
///
/// `wasm_api_parity` intentionally ignores trait impls, so this is deliberately a much narrower
/// contract than that general differential: it contains only the runtime/serde doors that
/// `create_base_wasm_struct` re-exports as inherent wasm methods. Keeping the member identity,
/// capability gate, and constructor together means the emitted output and the output-side test
/// cannot each grow an independent vocabulary.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum WasmDoorMember {
    ToCborBytes,
    FromCborBytes,
    ToCanonicalCborBytes,
    ToJson,
    ToJsonValue,
    FromJson,
}

impl WasmDoorMember {
    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::ToCborBytes => "to_cbor_bytes",
            Self::FromCborBytes => "from_cbor_bytes",
            Self::ToCanonicalCborBytes => "to_canonical_cbor_bytes",
            Self::ToJson => "to_json",
            Self::ToJsonValue => "to_json_value",
            Self::FromJson => "from_json",
        }
    }

    #[cfg(test)]
    #[allow(dead_code)] // `wasm_parity_tests` is bin-only.
    pub(crate) const fn is_cbor(self) -> bool {
        matches!(
            self,
            Self::ToCborBytes | Self::FromCborBytes | Self::ToCanonicalCborBytes
        )
    }
}

/// The exact wasm door owed by a rust-backed wrapper in this CLI posture.
///
/// This is the production contract: `wasm_door_functions` constructs every returned member, and
/// `create_base_wasm_struct` pushes that complete returned vector. Tests read this same contract
/// only to compare it with parsed emitted source, never as a substitute for emission.
pub(crate) fn wasm_door_members(cli: &Cli, deserialize_generated: bool) -> Vec<WasmDoorMember> {
    let mut members = Vec::new();
    if cli.to_from_bytes_methods {
        members.push(WasmDoorMember::ToCborBytes);
        if cli.preserve_encodings && cli.canonical_form {
            members.push(WasmDoorMember::ToCanonicalCborBytes);
        }
        if deserialize_generated {
            members.push(WasmDoorMember::FromCborBytes);
        }
    }
    if cli.json_serde_derives {
        members.extend([
            WasmDoorMember::ToJson,
            WasmDoorMember::ToJsonValue,
            WasmDoorMember::FromJson,
        ]);
    }
    members
}

fn wasm_door_functions(
    cli: &Cli,
    name: &str,
    deserialize_generated: bool,
) -> Vec<codegen::Function> {
    wasm_door_members(cli, deserialize_generated)
        .into_iter()
        .map(|member| {
            let mut function = codegen::Function::new(member.name());
            match member {
                WasmDoorMember::ToCborBytes => {
                    function.ret("Vec<u8>").arg_ref_self().vis("pub");
                    if cli.preserve_encodings && cli.canonical_form {
                        function.line(format!(
                            "{}::serialization::Serialize::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                    } else {
                        function.line(format!(
                            "{}::serialization::ToCBORBytes::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                    }
                }
                WasmDoorMember::FromCborBytes => {
                    function
                        .ret(format!("Result<{name}, JsError>"))
                        .arg("cbor_bytes", "&[u8]")
                        .vis("pub")
                        .line(format!(
                            "{}::serialization::Deserialize::from_cbor_bytes(cbor_bytes).map(Self).map_err(|e| JsError::new(&format!(\"from_bytes: {{}}\", e)))",
                            cli.common_import_wasm()
                        ));
                }
                WasmDoorMember::ToCanonicalCborBytes => {
                    function
                        .ret("Vec<u8>")
                        .arg_ref_self()
                        .vis("pub")
                        .line(format!(
                            "{}::serialization::Serialize::to_canonical_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                }
                WasmDoorMember::ToJson => {
                    function
                        .ret("Result<String, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde_json::to_string_pretty(&self.0).map_err(|e| JsError::new(&format!(\"to_json: {}\", e)))");
                }
                WasmDoorMember::ToJsonValue => {
                    function
                        .ret("Result<JsValue, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde::Serialize::serialize(&self.0, &serde_wasm_bindgen::Serializer::json_compatible()).map_err(|e| JsError::new(&format!(\"to_json_value: {}\", e)))");
                }
                WasmDoorMember::FromJson => {
                    function
                        .ret(format!("Result<{name}, JsError>"))
                        .arg("json", "&str")
                        .vis("pub")
                        .line("serde_json::from_str(json).map(Self).map_err(|e| JsError::new(&format!(\"from_json: {}\", e)))");
                }
            }
            function
        })
        .collect()
}

fn create_base_wasm_struct<'a>(
    gen_scope: &GenerationScope,
    ident: &'a RustIdent,
    exists_in_rust: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    s.vis("pub")
        .derive("Clone")
        .derive("Debug")
        .attr("wasm_bindgen");
    // W2 (`--wrapper-requests`): a requested wrapper carries a `/// Generated at the request of: …`
    // attribution doc. Set here so the loose list / map emitters (which set no struct doc of their
    // own) carry it; the NonEmpty emitters set their own struct doc and PREPEND this text via
    // `requested_attribution_prefix` (a `.doc()` call replaces, not appends). Empty map off the flag,
    // so own-spec wrappers are byte-identical.
    if let Some(doc) = gen_scope.requested_attribution.get(ident) {
        s.doc(doc);
    }
    let mut s_impl = codegen::Impl::new(name);
    let mut macros = Vec::new();
    // There are auto-implementing ToCBORBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if exists_in_rust {
        match &cli.wasm_cbor_json_api_macro {
            Some(cbor_json_macro) => {
                macros.push((cbor_json_macro.clone(), vec![name.to_owned()]));
            }
            None => {
                // The vector is deliberately complete before it reaches this loop: a new door is
                // one contract member plus one constructor, not a separately remembered `push_fn`
                // at each conditional branch (the historical canonical door escape).
                for function in
                    wasm_door_functions(cli, name, gen_scope.deserialize_generated(ident))
                {
                    s_impl.push_fn(function);
                }
            }
        }
    }
    WasmWrapper {
        ident,
        s,
        s_impl,
        from_wasm: None,
        from_native: None,
        as_ref: None,
        macros,
    }
}

/// default_structure will have it be a DIRECT wrapper with a tuple field of rust_lib::{ident}
/// this will include generating to/from traits automatically
fn create_base_wasm_wrapper<'a>(
    gen_scope: &GenerationScope,
    types: &IntermediateTypes<'_>,
    ident: &'a RustIdent,
    default_structure: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    assert!(cli.wasm);
    let mut base = create_base_wasm_struct(gen_scope, ident, true, cli);
    if default_structure {
        let native_name = rust_crate_struct_from_wasm(types, ident, cli);
        base.push_inner_field(&native_name);
        base.add_conversion_methods(&native_name, cli);
    }
    base
}

pub fn table_type(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "OrderedHashMap"
    } else {
        "BTreeMap"
    }
}

#[derive(Debug)]
struct EncodingField {
    field_name: String,
    /// The type this encoding field is DECLARED as. Callers that push it into an encoding struct (or
    /// an enum variant's field list) must therefore hand `encoding_fields` the member's **declared**
    /// type — never `.resolve_aliases()`d — so the declaration keeps the alias ident the data-struct
    /// field for the same member already keeps (`docs/docs/output_format.mdx` § "Type spelling at
    /// member positions"). Resolving is a STRUCTURAL-DISPATCH normalization; reusing its result as a
    /// NAMING input is how `BTreeMap<Vec<u8>, ..>` came to index a field typed
    /// `OrderedHashMap<PolicyId, ..>`. Callers that consume only `field_name`/`default_expr` are
    /// spelling-irrelevant and may pass whatever shape is convenient.
    type_name: String,
    /// this MUST be equivalent to the Default trait of the encoding field.
    /// This can be more concise though e.g. None for Option<T>::default()
    default_expr: &'static str,
    enc_conversion_before: &'static str,
    enc_conversion_after: &'static str,
    is_copy: bool,
}

impl EncodingField {
    pub fn enc_conversion(&self, expr: &str) -> String {
        format!(
            "{}{}{}",
            self.enc_conversion_before, expr, self.enc_conversion_after
        )
    }
}

fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    match key {
        FixedValue::Text(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            enc_conversion_before: "StringEncoding::from(",
            enc_conversion_after: ")",
            is_copy: false,
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            enc_conversion_before: "Some(",
            enc_conversion_after: ")",
            is_copy: true,
        },
        _ => unimplemented!(),
    }
}

/// THE mint for a `@custom_encodings` declaration: the codec-visible encoding variables the
/// declaration names, in declared order, under `name`.
///
/// Positional naming — the first slot keeps the bare `{name}_encoding` spelling every inferred
/// single-variable member already has (so a one-`str` declaration over an alias-of-bytes reproduces
/// today's names and types exactly), and further slots append their 1-based index
/// (`{name}_encoding2`, `{name}_encoding3`, …), keeping `_encoding` non-terminal only where a
/// declaration made it ambiguous. Deterministic and derivable from the declaration alone, which is
/// what lets the two carrier channels (the emission configs, and the sidecar/LHS derivation) agree
/// without either consulting the other.
///
/// Types, defaults and `is_copy` are the SAME values `encoding_fields_impl` mints for the inferred
/// flavors of these kinds (`mod.rs`'s `Primitive`/`Array`/`Map` arms) — a declaration fixes WHICH
/// variables a codec sees and in what order, never how one of them is spelled or passed.
fn declared_encoding_fields(name: &str, kinds: &[EncodingKind]) -> Vec<EncodingField> {
    kinds
        .iter()
        .enumerate()
        .map(|(i, kind)| {
            let field_name = if i == 0 {
                format!("{name}_encoding")
            } else {
                format!("{name}_encoding{}", i + 1)
            };
            match kind {
                EncodingKind::Sz => EncodingField {
                    field_name,
                    type_name: "Option<cbor_event::Sz>".to_owned(),
                    default_expr: "None",
                    enc_conversion_before: "Some(",
                    enc_conversion_after: ")",
                    is_copy: true,
                },
                EncodingKind::Str => EncodingField {
                    field_name,
                    type_name: "StringEncoding".to_owned(),
                    default_expr: "StringEncoding::default()",
                    enc_conversion_before: "StringEncoding::from(",
                    enc_conversion_after: ")",
                    is_copy: false,
                },
                EncodingKind::Len => EncodingField {
                    field_name,
                    type_name: "LenEncoding".to_owned(),
                    default_expr: "LenEncoding::default()",
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: true,
                },
            }
        })
        .collect()
}

/// Whether an `Alias` node reached during encoding-variable derivation may honor its own rule's
/// `@custom_encodings` declaration.
///
/// A declaration describes the wire of the codec written BESIDE it, so it is honored at exactly the
/// node where its own pair governs. Once some OUTER pair has taken over the position (a field-level
/// pair shadowing the alias it is written over, or an outer alias's pair), everything under it is
/// inside that codec's opaque wire and its declarations describe a codec nobody calls — so the
/// derivation switches to `Blind` and reports what INFERENCE alone says, which is exactly what the
/// governing codec is handed. `docs/docs/comment_dsl.mdx` states this as the precedence rule.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum AliasDeclarations {
    /// No pair governs above this point: an alias's own declaration is the answer.
    Honor,
    /// A pair already governs: ignore every declaration below (identical to the pre-directive
    /// behaviour, and therefore byte-identical for any spec that declares nothing).
    Blind,
}

fn encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
    cli: &Cli,
) -> Vec<EncodingField> {
    encoding_fields_decls(
        types,
        name,
        ty,
        include_default,
        cli,
        AliasDeclarations::Honor,
    )
}

/// `encoding_fields` for a MEMBER position whose own comment may carry a custom pair — every record
/// field site, which is where a field-level `@custom_serialize`/`@custom_deserialize` is read.
///
/// A field-level pair governs the member from the top of the recursion (it fires BEFORE any encoding
/// operation is consumed, so it is handed the tag/`.cbor` variables too), which makes the three
/// answers here exhaustive:
///   * pair + declaration → the declared list IS the member's whole codec-visible list;
///   * pair, no declaration → inference, blind to any declaration underneath (the pair shadows them);
///   * no pair → ordinary inference, honoring a declaration the member's own TYPE rule carries.
///
/// `_default_present` is appended as `encoding_fields` appends it: it is generated-code-owned, never
/// part of the codec's tuple, so it survives a declaration untouched.
fn field_encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    field_metadata: Option<&RuleMetadata>,
    include_default: bool,
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    let field_pair = field_metadata
        .filter(|rmd| rmd.custom_serialize.is_some() && rmd.custom_deserialize.is_some());
    match field_pair {
        Some(rmd) => match rmd.custom_encodings.as_ref() {
            Some(kinds) => {
                let mut encs = declared_encoding_fields(name, kinds);
                if include_default && ty.config.default.is_some() {
                    encs.push(default_present_encoding_field(name));
                }
                encs
            }
            None => encoding_fields_decls(
                types,
                name,
                ty,
                include_default,
                cli,
                AliasDeclarations::Blind,
            ),
        },
        None => encoding_fields(types, name, ty, include_default, cli),
    }
}

/// The generated-code-owned `{name}_default_present` slot a `.default`-carrying member gets on top of
/// its encoding variables. Never part of a codec's argument or return tuple (the codec is called
/// only when the value is present), so it is minted the same way whether the list around it was
/// inferred or declared.
fn default_present_encoding_field(name: &str) -> EncodingField {
    EncodingField {
        field_name: format!("{name}_default_present"),
        type_name: "bool".to_owned(),
        default_expr: "false",
        enc_conversion_before: "",
        enc_conversion_after: "",
        is_copy: true,
    }
}

/// Whether a custom (de)serializer pair placed over `ty` would be handed NO encoding variables at
/// all — the state `@custom_encodings` exists to make declarable, and which
/// `IntermediateTypes::finalize` refuses under `--preserve-encodings` when nothing is declared.
///
/// This asks the SAME derivation the emission sites build their argument lists from, so "empty
/// demand" cannot come to mean two different things; the alternative (a twin predicate over
/// `encoding_fields_impl`'s empty arms) would be a second, unpaired derivation of the same fact.
/// `Blind` because a pair governs its whole subtree — a declaration underneath describes a codec
/// whose wire this one has swallowed.
pub(crate) fn custom_codec_demand_is_empty(
    types: &IntermediateTypes,
    ty: &RustType,
    cli: &Cli,
) -> bool {
    encoding_fields_decls(types, "wire", ty, false, cli, AliasDeclarations::Blind).is_empty()
}

/// `encoding_fields` with an explicit declaration mode — see [`AliasDeclarations`].
fn encoding_fields_decls(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
    cli: &Cli,
    decls: AliasDeclarations,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    // TODO: how do we handle defaults for nested things? e.g. inside of a ConceptualRustType::Map
    let mut encs = encoding_fields_impl(types, name, ty.into(), cli, 0, 0, decls);
    if include_default && ty.config.default.is_some() {
        encs.push(default_present_encoding_field(name));
    }
    encs
}

/// The tag-level infix for a stacked tag's encoding member name. Tag levels count OUTSIDE-IN:
/// level 1 (the outermost tag) keeps the historical `tag` spelling so all existing single-tag
/// output stays byte-identical; each deeper level appends its 1-based number (`tag2`, `tag3`, …),
/// keeping the `_encoding` suffix terminal like every other member. Callers combine it as
/// `{name}_{infix}_encoding`. Shared by the member declaration (`encoding_fields_impl`), the
/// serialize read, and the deserialize write so the three can never drift on the scheme.
pub(super) fn tag_encoding_infix(tag_level: usize) -> String {
    if tag_level <= 1 {
        "tag".to_owned()
    } else {
        format!("tag{tag_level}")
    }
}

/// The LOCAL a mandatory `Tagged` level's `match .tag_sz()?` pattern binds its head size to, under
/// `--preserve-encodings`. Depth-suffixed for the same reason [`tag_encoding_infix`] is: stacked
/// levels nest their `match` blocks, so an un-suffixed binding would let the inner level shadow the
/// outer and both final exprs would read the innermost size.
///
/// Shared rather than spelled twice because two emitters must agree on it: the `Tagged` arm that
/// BINDS it, and the `Optional` arm's `None` branch, which re-states the already-consumed tag size
/// for a null payload (the `Some` branch gets it threaded through the child's `.map(..)` instead).
/// A drift between the two is not a compile error at generation time — it is an E0425 in the
/// consumer's crate, or worse, a silently dropped head width.
pub(super) fn tag_enc_binding(tag_level: usize) -> String {
    if tag_level <= 1 {
        "tag_enc".to_owned()
    } else {
        format!("tag_enc{tag_level}")
    }
}

/// The `.cbor`-level names a payload's byte string owns, for a chain that crosses more than one
/// `CBORBytes` operation on ONE member name (the INLINE spelling `bytes .cbor (bytes .cbor T)`).
/// Levels count OUTSIDE-IN, exactly as [`tag_encoding_infix`]'s do, and level 1 keeps the historical
/// spelling so all existing single-payload output stays byte-identical; each deeper level appends
/// its 1-based number.
///
/// Four names move together per level, which is why they share one derivation: the encoding member
/// infix (`{name}_bytes_encoding`), the serializer's staging buffer and the byte vector it
/// finalizes into (`{var}_inner_se`, `{var}_bytes`), the deserializer's reader over those bytes
/// (`inner_de`) and the local a non-statement payload is staged in (`{var}_payload`). All of them
/// are minted per OWNING VARIABLE, so at two depths of one chain the undepthed spellings collide:
/// the buffer is used after `finalize()` moved it (E0382), the sidecar declares one field twice
/// (E0124), and the outer reader's leftover-bytes check silently re-reads the INNER reader.
///
/// Shared by the member declaration (`encoding_fields_impl`), the serialize write and the
/// deserialize read so the three can never drift on the scheme — the same reason
/// [`tag_encoding_infix`] is shared.
fn cbor_level_name(base: &str, cbor_level: usize) -> String {
    if cbor_level <= 1 {
        base.to_owned()
    } else {
        format!("{base}{cbor_level}")
    }
}

/// The `.cbor` payload byte string's encoding-member infix: `bytes` / `bytes2` / … Callers combine
/// it as `{name}_{infix}_encoding` (declaration, serialize read) and as `{var}_{infix}` for the
/// serialized/deserialized byte vector itself. See [`cbor_level_name`].
pub(super) fn cbor_bytes_infix(cbor_level: usize) -> String {
    cbor_level_name("bytes", cbor_level)
}

/// The serializer's payload staging buffer suffix: `{var}_inner_se` / `{var}_inner_se2` / …
/// See [`cbor_level_name`].
pub(super) fn cbor_payload_buffer_suffix(cbor_level: usize) -> String {
    cbor_level_name("inner_se", cbor_level)
}

/// The deserializer's reader over the payload bytes: `inner_de` / `inner_de2` / … Unlike the other
/// three this one is NOT prefixed by the owning variable (it is a reader overload, not a member
/// name), so the depth suffix is the only thing keeping two levels of one chain apart. See
/// [`cbor_level_name`].
pub(super) fn cbor_payload_reader(cbor_level: usize) -> String {
    cbor_level_name("inner_de", cbor_level)
}

/// The local a payload read at a non-statement position is staged in: `{var}_payload` /
/// `{var}_payload2` / … See [`cbor_level_name`].
pub(super) fn cbor_payload_binding_suffix(cbor_level: usize) -> String {
    cbor_level_name("payload", cbor_level)
}

/// `tag_depth` is the number of tag levels already crossed on THIS member name (0 at the member
/// root, incremented each time a `Tagged`/`OptionallyTagged` op recurses into its child under the
/// same name). It drives `tag_encoding_infix` so stacked tags get distinct members. Name-changing
/// recursions (array element, map key/value) start a fresh sub-member and reset it to 0.
///
/// `cbor_depth` is the same counter for `.cbor` payload levels, driving `cbor_bytes_infix` so the
/// INLINE `bytes .cbor (bytes .cbor T)` spelling declares one byte-string sidecar per depth instead
/// of the same field twice (E0124). It threads and resets at exactly the same boundaries
/// `tag_depth` does — the two are independent counters over the same name, so a tag between two
/// payloads advances only the tag one.
///
/// `decls` decides whether an `Alias` node may answer with its rule's own `@custom_encodings`
/// declaration instead of recursing — see [`AliasDeclarations`]. It threads UNCHANGED through every
/// recursion (the shadow a governing codec casts covers its whole subtree, including across the
/// array-element / map-key / map-value name boundaries that reset `tag_depth`).
fn encoding_fields_impl(
    types: &IntermediateTypes,
    name: &str,
    ty: SerializingRustType,
    cli: &Cli,
    tag_depth: usize,
    cbor_depth: usize,
    decls: AliasDeclarations,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    match ty {
        SerializingRustType::Root(ConceptualRustType::Array(elem_ty), _cfg) => {
            let base = EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
            };
            let inner_encs = encoding_fields_impl(
                types,
                &format!("{name}_elem"),
                (&**elem_ty).into(),
                cli,
                0,
                0,
                decls,
            );
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = tuple_type_name(&inner_encs);
                vec![
                    base,
                    EncodingField {
                        field_name: format!("{name}_elem_encodings"),
                        type_name: format!("Vec<{type_name_elem}>"),
                        default_expr: "Vec::new()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: false,
                    },
                ]
            }
        }
        SerializingRustType::Root(ConceptualRustType::Map(k, v), cfg) => {
            let mut encs = vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
            }];
            let key_encs = encoding_fields_impl(
                types,
                &format!("{name}_key"),
                (&**k).into(),
                cli,
                0,
                0,
                decls,
            );
            let val_encs = encoding_fields_impl(
                types,
                &format!("{name}_value"),
                (&**v).into(),
                cli,
                0,
                0,
                decls,
            );

            // `@duplicates preserve` (the pair-map twin): a `BTreeMap` keyed by key VALUE is
            // structurally incapable of holding two entries with the same key, so the encoding
            // sidecar must be POSITIONAL — a `Vec<tuple>` parallel to the entries, indexed by
            // position exactly like the array `_elem_encodings` sidecar (serialize reads `.get(i)`,
            // deserialize `.push(..)`s per entry). The loose (reject/default) table stays keyed by
            // key value.
            let preserve_pair_map =
                cfg.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve);

            if !key_encs.is_empty() {
                let type_name_value = tuple_type_name(&key_encs);
                let (type_name, default_expr) = if preserve_pair_map {
                    (format!("Vec<{type_name_value}>"), "Vec::new()")
                } else {
                    (
                        format!(
                            "BTreeMap<{}, {}>",
                            k.for_rust_member(types, false, cli),
                            type_name_value
                        ),
                        "BTreeMap::new()",
                    )
                };
                encs.push(EncodingField {
                    field_name: format!("{name}_key_encodings"),
                    type_name,
                    default_expr,
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
                });
            }

            if !val_encs.is_empty() {
                let type_name_value = tuple_type_name(&val_encs);
                let (type_name, default_expr) = if preserve_pair_map {
                    (format!("Vec<{type_name_value}>"), "Vec::new()")
                } else {
                    (
                        format!(
                            "BTreeMap<{}, {}>",
                            k.for_rust_member(types, false, cli),
                            type_name_value
                        ),
                        "BTreeMap::new()",
                    )
                };
                encs.push(EncodingField {
                    field_name: format!("{name}_value_encodings"),
                    type_name,
                    default_expr,
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
                });
            }
            encs
        }
        SerializingRustType::Root(ConceptualRustType::Primitive(p), _cfg) => match p {
            Primitive::Bytes | Primitive::Str => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "StringEncoding".to_owned(),
                default_expr: "StringEncoding::default()",
                enc_conversion_before: "StringEncoding::from(",
                enc_conversion_after: ")",
                is_copy: false,
            }],
            Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64
            | Primitive::N64
            | Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::Float
            | Primitive::F16
            | Primitive::F32
            | Primitive::F64
            | Primitive::F16To32
            | Primitive::F32To64 => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "Option<cbor_event::Sz>".to_owned(),
                default_expr: "None",
                enc_conversion_before: "Some(",
                enc_conversion_after: ")",
                is_copy: true,
            }],
            Primitive::Bool =>
            /* bool only has 1 encoding */
            {
                vec![]
            }
        },
        SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => match f {
            FixedValue::Bool(_) | FixedValue::Null | FixedValue::Undefined => vec![],
            FixedValue::Nint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::I64)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            ),
            FixedValue::Uint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::U64)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            ),
            FixedValue::Float(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Float)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            ),
            FixedValue::Text(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Str)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            ),
            FixedValue::Bytes(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            ),
        },
        SerializingRustType::Root(ConceptualRustType::Alias(alias_ident, ty), cfg) => {
            // A type-level custom codec OWNS the wire from this node down, so when its rule declares
            // the wire's encoding variables (`@custom_encodings`) the declaration IS the answer here
            // — replacing the whole inferred subtree, which is what makes a zero-demand replaced type
            // (a self-carrying extern, `bool`, `any`) able to carry framing at all. Reached only in
            // `Honor` mode and only for a COMPLETE pair: a lone half is refused elsewhere, and under
            // a governing outer codec (`Blind`) the declaration describes a codec nobody calls.
            // A pair WITHOUT a declaration still shadows its subtree — it governs the position, so
            // what it is handed is what inference alone says.
            let alias_pair = types
                .type_aliases()
                .get(alias_ident)
                .and_then(|info| info.rule_metadata.as_ref())
                .filter(|rmd| rmd.custom_serialize.is_some() && rmd.custom_deserialize.is_some());
            if decls == AliasDeclarations::Honor
                && let Some(rmd) = alias_pair
            {
                if let Some(kinds) = rmd.custom_encodings.as_ref() {
                    return declared_encoding_fields(name, kinds);
                }
                return encoding_fields_impl(
                    types,
                    name,
                    SerializingRustType::Root(ty, cfg),
                    cli,
                    tag_depth,
                    cbor_depth,
                    AliasDeclarations::Blind,
                );
            }
            // Keep the OUTER RustTypeSerializeConfig (`cfg`): an Alias's inner is a bare
            // ConceptualRustType with no config of its own, so recursing with `(&**ty).into()`
            // would DEFAULT the config and drop the per-rule policy the alias carries — notably
            // `@duplicates preserve`, which the `Map` arm above reads to pick the POSITIONAL
            // (`Vec<..>`) encoding sidecar instead of the key-VALUE-keyed `BTreeMap<..>`. Dropping
            // it there is not a spelling difference but a wire-behaviour skew: a `BTreeMap` cannot
            // hold the repeated keys a preserve table exists to round-trip. (`generate_serialize`
            // and `generate_deserialize` keep the config at their own `Alias` arms for the same
            // reason.) Masked for as long as every caller whose `type_name` reaches a declaration
            // pre-resolved aliases; it stops being masked the moment one of them spells the
            // member's declared type instead.
            encoding_fields_impl(
                types,
                name,
                SerializingRustType::Root(ty, cfg),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            )
        }
        SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
            // same-name recursion (a nullable can still carry a tagged inner), so thread the depth
            // rather than resetting it via the `encoding_fields` wrapper.
            encoding_fields_impl(
                types,
                name,
                (&**ty).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            )
        }
        SerializingRustType::Root(ConceptualRustType::Rust(rust_ident), _cfg) => {
            match &types.rust_struct(rust_ident).unwrap().variant() {
                // for c-style enums we push those up to where they are used instead of self-containing
                RustStructType::CStyleEnum { variants } => {
                    // earlier we are guaranteed that all variants will have the same encoding types
                    // or else it wouldn't end up as a c-style enum in the first place in IntermediateTypes
                    encoding_fields_decls(types, name, variants[0].rust_type(), false, cli, decls)
                }
                // also push them out for RawBytesType as they're not stored there, as if we had `bytes` directly here
                RustStructType::RawBytesType => encoding_fields_impl(
                    types,
                    name,
                    (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                    cli,
                    tag_depth,
                    cbor_depth,
                    decls,
                ),
                // a named table/array rule is a bare rust typedef onto a collection — there is no
                // struct for the encodings to live inside, so they must be pushed OUT to the
                // referring member exactly as the CStyleEnum/RawBytesType cases above do. Reached
                // only from a NOMINAL reference to such a rule (parse-order makes one when a rule
                // cycle is entered at the collection rule); the resolved-alias reference path
                // reaches the `Alias` arm and lands on the same `Map`/`Array` arms. Without this
                // the referrer mints no `{name}_encoding` sidecar while serialize (which DOES
                // recurse into the collection) reads one — E0425 on generated code.
                RustStructType::Table { domain, range, .. } => {
                    let structural =
                        ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone()));
                    let cfg = nominal_collection_cfg(types, rust_ident, &_cfg);
                    encoding_fields_impl(
                        types,
                        name,
                        SerializingRustType::Root(&structural, cfg),
                        cli,
                        tag_depth,
                        cbor_depth,
                        decls,
                    )
                }
                RustStructType::Array { element_type, .. } => {
                    let structural = ConceptualRustType::Array(Box::new(element_type.clone()));
                    let cfg = nominal_collection_cfg(types, rust_ident, &_cfg);
                    encoding_fields_impl(
                        types,
                        name,
                        SerializingRustType::Root(&structural, cfg),
                        cli,
                        tag_depth,
                        cbor_depth,
                        decls,
                    )
                }
                // no encodings here. they're contained inside the struct
                _ => vec![],
            }
        }
        // `any` is self-carried: the `AnyCbor` value stores its own encodings, so it contributes no
        // owner encoding fields (the member's ordinary KEY encoding slot mints separately via
        // `key_encoding_field`, so it is unaffected). Mirrors the `Rust(ident)` self-carried case.
        SerializingRustType::Root(ConceptualRustType::Any, _cfg) => vec![],
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
            // This tag is the (tag_depth + 1)th level crossed on this member name; its member keeps
            // `tag` at level 1 and gains a numeric infix deeper, so stacked tags don't collide.
            let tag_level = tag_depth + 1;
            let tag_infix = tag_encoding_infix(tag_level);
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_{tag_infix}"),
                (&ConceptualRustType::Fixed(FixedValue::Uint(*tag as u64))).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            );
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_level, cbor_depth, decls,
            ));
            encs
        }
        SerializingRustType::EncodingOperation(
            CBOREncodingOperation::OptionallyTagged(_tag),
            child,
        ) => {
            // the tri-state tag-presence var (absent | present(sz)); the deserialize preamble
            // produces a fully-formed `TagPresenceEncoding`, so no enc conversion is applied.
            let tag_level = tag_depth + 1;
            let tag_infix = tag_encoding_infix(tag_level);
            let mut encs = vec![EncodingField {
                field_name: format!("{name}_{tag_infix}_encoding"),
                type_name: "TagPresenceEncoding".to_owned(),
                default_expr: "TagPresenceEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
            }];
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_level, cbor_depth, decls,
            ));
            encs
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
            // This byte string is the (cbor_depth + 1)th `.cbor` level crossed on this member name;
            // its member keeps `bytes` at level 1 and gains a numeric infix deeper, so the INLINE
            // `bytes .cbor (bytes .cbor T)` spelling declares one sidecar per depth instead of
            // `{name}_bytes_encoding` twice. The child recurses one level deeper.
            let cbor_level = cbor_depth + 1;
            let bytes_infix = cbor_bytes_infix(cbor_level);
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_{bytes_infix}"),
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                cli,
                tag_depth,
                cbor_depth,
                decls,
            );
            encs.append(&mut encoding_fields_impl(
                types, name, *child, cli, tag_depth, cbor_level, decls,
            ));
            encs
        }
    }
}

fn encoding_var_names_str(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    cli: &Cli,
) -> String {
    encoding_var_names_str_for_field(types, field_name, rust_type, None, cli)
}

/// `encoding_var_names_str` for a position that may carry a FIELD-level custom pair: the tuple LHS a
/// custom deserializer's return is destructured into must name exactly the variables the codec
/// returns, which its own declaration fixes (see [`field_encoding_fields`]).
fn encoding_var_names_str_for_field(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    field_metadata: Option<&RuleMetadata>,
    cli: &Cli,
) -> String {
    assert!(cli.preserve_encodings);
    // `is_fixed_value` is a STRUCTURAL question (does this position bind a value at all), so it
    // still asks the resolved type. The encoding list below deliberately does NOT resolve: a
    // declaration rides on the alias node `resolve_aliases()` deletes, and the `Alias` arm is a pure
    // pass-through for every undeclared type, so the two are identical wherever nothing declares.
    let mut var_names = if rust_type.clone().resolve_aliases().is_fixed_value() {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in
        field_encoding_fields(types, field_name, rust_type, field_metadata, false, cli).into_iter()
    {
        var_names.push(enc.field_name);
    }

    if var_names.len() > 1 {
        format!("({})", var_names.join(", "))
    } else {
        var_names.join(", ")
    }
}

// Value-level twin of `tuple_type_name`: joins encoding VAR names into a parenthesized tuple.
fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

// Type-level twin of `tuple_str`: joins encoding fields' `type_name`s into a parenthesized tuple
// type unless there is exactly one (then the lone type_name stands alone, unparenthesized).
fn tuple_type_name(encs: &[EncodingField]) -> String {
    if encs.len() == 1 {
        encs[0].type_name.clone()
    } else {
        format!(
            "({})",
            encs.iter()
                .map(|enc| enc.type_name.clone())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// True iff every encoding field's `default_expr` is a trivial literal (`None`/`false`) rather than
/// a function call (`LenEncoding::default()`, `Vec::new()`, `BTreeMap::new()`,
/// `StringEncoding::default()`). Trivial-literal tuple defaults may be emitted with `unwrap_or(..)`;
/// a call-bearing default must stay behind `unwrap_or_else(|| ..)` or clippy::or_fun_call fires.
/// Centralized so every tuple-default emission site agrees on the same decision.
fn encoding_defaults_all_trivial(encoding_fields: &[EncodingField]) -> bool {
    encoding_fields
        .iter()
        .all(|enc| matches!(enc.default_expr, "None" | "false"))
}

fn cbor_event_len_n(n: &str, cli: &Cli) -> String {
    if cli.preserve_encodings {
        format!("cbor_event::LenSz::Len({n}, _)")
    } else {
        format!("cbor_event::Len::Len({n})")
    }
}

fn cbor_event_len_indef(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "cbor_event::LenSz::Indefinite"
    } else {
        "cbor_event::Len::Indefinite"
    }
}

/// How to construct a `CBORReadLen` from the freshly-read `len`. In preserve mode `len` is a
/// `cbor_event::LenSz`, matching `CBORReadLen::new`. In non-preserve mode `len` is a
/// `cbor_event::Len`; going through `From<cbor_event::Len>` (instead of `new`) lets the same
/// emission compile against either runtime flavor — crucially a preserve-flavored
/// `--common-import-override` target (e.g. cml_core), whose `new` takes `LenSz`. Preserve stays on
/// `new` because such external cores expose no `From<LenSz>`.
fn cbor_read_len_ctor(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "CBORReadLen::new"
    } else {
        "CBORReadLen::from"
    }
}

fn make_encoding_struct(encoding_name: &str) -> codegen::Struct {
    let mut encoding_struct = codegen::Struct::new(encoding_name.to_string());
    encoding_struct
        .vis("pub")
        .derive("Clone")
        .derive("Debug")
        .derive("Default");
    encoding_struct
}

/// clippy's default `type-complexity-threshold`. A type in a lint-scored position (struct field, fn
/// signature, ...) whose structural score exceeds this trips `clippy::type_complexity`. Type
/// *aliases* are not scored by the lint, so hoisting an over-threshold encoding-struct field type
/// into a `pub type` alias silences it without an `#[allow]` and without changing any emitted bytes
/// or round-trip semantics.
const TYPE_COMPLEXITY_THRESHOLD: u64 = 250;

/// Reproduce clippy's `type_complexity` scoring closely enough to decide, deterministically,
/// whether an emitted encoding field type would trip the lint. clippy walks the type and adds
/// `10 * nest` for every path / tuple / array / slice / reference node, incrementing `nest` by one
/// when descending into that node's children. The emitted encoding types use only paths (`Foo`,
/// `Foo<..>`, `a::b`) and tuples (no refs/slices), so scoring those node kinds suffices.
/// Over-estimating here is harmless (it only mints an extra alias); the clippy gate is the backstop
/// if the real boundary ever shifts.
fn type_complexity_score(ty: &str) -> u64 {
    /// Split `s` on top-level `delim` (bracket depth 0 over `<>` and `()`), trimming each piece.
    fn split_top_level(s: &str, delim: char) -> Vec<&str> {
        let mut depth = 0i32;
        let mut parts = Vec::new();
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => depth -= 1,
                c if c == delim && depth == 0 => {
                    parts.push(s[start..i].trim());
                    start = i + c.len_utf8();
                }
                _ => {}
            }
        }
        parts.push(s[start..].trim());
        parts
    }
    /// True iff every prefix of `s` has non-negative `<>`/`()` depth and the whole is balanced —
    /// i.e. an outermost `(...)` pair actually wraps the entire string.
    fn is_balanced(s: &str) -> bool {
        let mut depth = 0i32;
        for c in s.chars() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => {
                    depth -= 1;
                    if depth < 0 {
                        return false;
                    }
                }
                _ => {}
            }
        }
        depth == 0
    }
    fn score(ty: &str, nest: u64) -> u64 {
        let ty = ty.trim();
        // Parenthesized: a tuple (>=2 top-level elements) is one node whose elements are children;
        // a single `(T)` grouping is just `T` (no HIR node); `()` is a unit.
        if let Some(inner) = ty
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .filter(|inner| is_balanced(inner))
        {
            let parts = split_top_level(inner, ',');
            return if inner.trim().is_empty() {
                1 // unit ()
            } else if parts.len() >= 2 {
                10 * nest + parts.iter().map(|p| score(p, nest + 1)).sum::<u64>()
            } else {
                score(inner, nest) // grouping, not a tuple
            };
        }
        // Path with generics `Ident<..>` / `a::b::Ident<..>`: one node, generic args are children.
        if let (Some(open), Some(close)) = (ty.find('<'), ty.rfind('>')) {
            let args = &ty[open + 1..close];
            return 10 * nest
                + split_top_level(args, ',')
                    .iter()
                    .map(|a| score(a, nest + 1))
                    .sum::<u64>();
        }
        // Plain path node (`u64`, `LenEncoding`, `cbor_event::Sz`, ...).
        10 * nest
    }
    score(ty, 1)
}

/// Add one field to an encoding struct, hoisting an over-`type_complexity` field type into a
/// deterministic `pub type <Owner><FieldCamel> = ..;` alias in the same `cbor_encodings` scope so
/// `clippy::type_complexity` stays quiet without an `#[allow]`. Alias names can't collide with each
/// other: `owner` (the owning encoding struct's base type name) is distinct per struct and
/// `field_name` is distinct within a struct, so identical anonymous shapes in different rules never
/// collide. An alias CAN in principle collide with another rule's encoding-struct name:
/// owner `Foo` + field `bar_encoding` aliases to `FooBarEncoding`, which a rule named `foo-bar`
/// also claims. That needs an over-threshold field AND the exact sibling rule name, and it fails
/// LOUD (E0428 in the generated crate, caught by every compile gate), so it is not disambiguated
/// preemptively.
/// Aliases are collected (not pushed) so the caller can push them into the scope alongside the
/// struct.
fn push_encoding_struct_field(
    encoding_struct: &mut codegen::Struct,
    aliases: &mut Vec<(String, String)>,
    owner: &RustIdent,
    field_name: &str,
    type_name: &str,
) {
    let field_type = if type_complexity_score(type_name) > TYPE_COMPLEXITY_THRESHOLD {
        let alias = format!("{}{}", owner, convert_to_camel_case(field_name));
        aliases.push((alias.clone(), type_name.to_owned()));
        alias
    } else {
        type_name.to_owned()
    };
    encoding_struct.field(format!("pub {field_name}"), field_type);
}

/// the derivative crate doesn't accept Eq="ignore" but omitting it
/// seems to behave correctly
/// The SINGLE demand→traits mapping (pinned semantics 6), used by every derive/ignore emission site so
/// the bare path stays byte-identical. Resolves a `DemandSet` to the comparison/hash traits it demands,
/// in the canonical emission order `Eq, PartialEq, Ord, PartialOrd, Hash`:
/// - `bare` → today's mode-dependent internal bundle (`Eq/PartialEq/Ord/PartialOrd`, plus `Hash` under
///   `--preserve-encodings`);
/// - `hash` → `Hash, Eq, PartialEq` (mode-independent);
/// - `ord` → `Ord, PartialOrd, Eq, PartialEq` (mode-independent).
///
/// `for_ignore` drops `Eq` (the `derivative` field ignore-list has no `Eq` attribute — `Eq` is a
/// fieldless marker), reproducing the old `key_derives(for_ignore=true)` set exactly.
fn key_trait_list(demand: DemandSet, for_ignore: bool, cli: &Cli) -> Vec<&'static str> {
    let mut eq = false;
    let mut ord = false;
    let mut hash = false;
    if demand.bare {
        eq = true;
        ord = true;
        if cli.preserve_encodings {
            hash = true;
        }
    }
    if demand.hash {
        hash = true;
        eq = true;
    }
    if demand.ord {
        ord = true;
        eq = true;
    }
    let mut out = Vec::new();
    if eq && !for_ignore {
        out.push("Eq");
    }
    if eq {
        out.push("PartialEq");
    }
    if ord {
        out.push("Ord");
        out.push("PartialOrd");
    }
    if hash {
        out.push("Hash");
    }
    out
}

/// The `where`-clause trait bound a key demand needs, as used by the `borrowed_key_types.rs`
/// `_assert_key_traits*` self-check carriers. Drops `PartialEq` (a supertrait of `Eq`, redundant as a
/// bound) and maps `Hash` to its full path, so the `bare` bound reproduces the historical
/// `Eq + Ord + PartialOrd + core::hash::Hash` (byte-identical) form.
fn key_bound(demand: DemandSet, cli: &Cli) -> String {
    key_trait_list(demand, false, cli)
        .iter()
        .filter(|t| **t != "PartialEq")
        .map(|t| if *t == "Hash" { "core::hash::Hash" } else { *t })
        .collect::<Vec<_>>()
        .join(" + ")
}

/// The sidecar flavor token for a demand (`bare`/`hash`/`ord`, space-joined when several bits are set).
/// This is the optional 3rd `BORROWED_KEY_TYPES` column; `parse_key_flavor` is its inverse.
fn key_flavor_token(demand: DemandSet) -> String {
    let mut parts = Vec::new();
    if demand.bare {
        parts.push("bare");
    }
    if demand.hash {
        parts.push("hash");
    }
    if demand.ord {
        parts.push("ord");
    }
    parts.join(" ")
}

/// The directly-tagged demand roots that warrant an emitted compile-time assertion: every
/// `@used_as_key` root — flavored or bare — whose type is a generated (non-extern), export-scope
/// struct in THIS crate, so it can be named `crate::generated::…` and its supply proven by the
/// compiler. Bare roots are included as a diagnosis breadcrumb: their derive demand propagates
/// transitively, so a missing-trait failure surfaces at a contained struct with nothing connecting
/// it to the tag — this file is the in-crate record of which tag caused which demand. (Internal
/// auto-detected map keys still emit nothing: their containers' own bounds enforce them in-crate.)
/// Sorted by ident (`BTreeMap` iteration) for deterministic placement.
fn assertion_roots(types: &IntermediateTypes) -> Vec<(RustIdent, DemandSet)> {
    types
        .key_demand_roots()
        .iter()
        .filter(|(ident, _)| {
            types.scope(ident).export()
                && types.rust_struct(ident).is_some_and(|rs| {
                    !matches!(
                        rs.variant(),
                        RustStructType::Extern | RustStructType::RawBytesType
                    )
                })
        })
        .map(|(ident, d)| (ident.clone(), *d))
        .collect()
}

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
fn recursive_exact_array_descriptor(
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
            || !matches!(ty.config.bounds, None | Some((None, None)))
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
                    match ty.config.bounds {
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
                    match ty.config.bounds {
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
            ConceptualRustType::Map(_, _) => None,
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

fn add_struct_derives<T: DataType>(
    data_type: &mut T,
    key_demand: Option<DemandSet>,
    is_enum: bool,
    cstyle_baseline: bool,
    custom_json: bool,
    cli: &Cli,
) {
    data_type.derive("Clone").derive("Debug");
    if !custom_json {
        if cli.json_serde_derives {
            data_type
                .derive("serde::Deserialize")
                .derive("serde::Serialize");
        }
        if cli.json_schema_export {
            data_type.derive("schemars::JsonSchema");
        }
    }
    if let Some(mut demand) = key_demand {
        // A c-style enum's always-on baseline is `Eq/PartialEq/Ord/PartialOrd` (emitted directly when
        // it is NOT a key). When it IS a key, that baseline must be UNIONED with the tag's flavor so a
        // tagged enum never derives LESS than an untagged one (pinned semantics 5). `ord` supplies the
        // whole `Ord/PartialOrd/Eq/PartialEq` family, so forcing it reconstitutes the baseline.
        if cstyle_baseline {
            demand.ord = true;
        }
        let traits = key_trait_list(demand, false, cli);
        if cli.preserve_encodings {
            // there's no way to do non-derive() proc macros in the codegen
            // cate so we must sadly use a newline like this. codegen manages indentation
            data_type.derive(&format!(
                "derivative::Derivative)]\n#[derivative({}",
                traits
                    .iter()
                    .map(|tr| match *tr {
                        // the derivative crate doesn't support enums tagged with ord/partialord yet without this
                        "Ord" | "PartialOrd" if is_enum =>
                            format!("{tr}=\"feature_allow_slow_enum\""),
                        _ => String::from(*tr),
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ));
        } else {
            for key_derive in traits {
                data_type.derive(key_derive);
            }
        }
    }
}
