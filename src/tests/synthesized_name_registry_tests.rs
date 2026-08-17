//! B5-324 — the generated wasm collection-wrapper namespace closes before a source map escapes.
//!
//! These tests inspect the registry at the emission seam rather than inferring it from completed
//! source. That makes a controlled provider removal prove the E0425-prevention check is live.

use crate::cli::Cli;
use crate::generation::{
    GenerationScope, WasmCollectionWrapperDefinition, WasmCollectionWrapperRegistry,
};
use crate::intermediate::{CDDLIdent, ModuleScope, ROOT_SCOPE, RustIdent};
use crate::parsing::EXTERN_DEPS_DIR;
use clap::Parser;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

static SCRATCH_SEQUENCE: AtomicUsize = AtomicUsize::new(0);

fn ident(name: &str) -> RustIdent {
    RustIdent::new(CDDLIdent::new(name.to_owned()))
}

fn scratch(tag: &str) -> PathBuf {
    let sequence = SCRATCH_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_synthesized_registry_{tag}_{}_{}",
        std::process::id(),
        sequence
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap_or_else(|error| {
        panic!(
            "creating synthesized-name registry scratch {}: {error}",
            root.display()
        )
    });
    root
}

fn write(root: &Path, relative: &str, contents: &str) -> PathBuf {
    let path = root.join(relative);
    std::fs::create_dir_all(
        path.parent()
            .unwrap_or_else(|| panic!("scratch file {relative} has a parent")),
    )
    .unwrap_or_else(|error| panic!("creating parent for {}: {error}", path.display()));
    std::fs::write(&path, contents)
        .unwrap_or_else(|error| panic!("writing {}: {error}", path.display()));
    path
}

fn cli(input: &Path, extra: &[String]) -> Cli {
    let mut args = vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        input.display().to_string(),
        "--output".to_owned(),
        "synthesized_registry_unused".to_owned(),
        "--wasm=true".to_owned(),
    ];
    args.extend(extra.iter().cloned());
    Cli::parse_from(args)
}

fn generated_scope(cli: &Cli) -> GenerationScope {
    crate::api::with_types(cli, |types, _| {
        let mut scope = GenerationScope::new();
        scope.generate(types, cli).unwrap_or_else(|error| {
            panic!("generation must populate the wrapper registry: {error}")
        });
        scope
    })
    .unwrap_or_else(|error| panic!("the fixture must parse and finalize: {error}"))
}

fn generated_files_error_after_removing_local_provider(cli: &Cli, provider: &RustIdent) -> String {
    crate::api::with_types(cli, |types, raw_bytes| {
        let mut scope = GenerationScope::new();
        scope.generate(types, cli).unwrap_or_else(|error| {
            panic!("generation must populate the wrapper registry: {error}")
        });
        assert!(
            scope
                .remove_wasm_collection_local_class_for_test(provider)
                .is_some(),
            "the test provider must have been locally minted"
        );
        scope
            .generated_files(types, raw_bytes, cli)
            .expect_err(
                "generated_files must reject an unclosed registry before returning a source map",
            )
            .to_string()
    })
    .unwrap_or_else(|error| panic!("the fixture must parse and finalize: {error}"))
}

fn assert_registered_reference(
    scope: &GenerationScope,
    wrapper: &str,
    door: &str,
    dependency_owned: bool,
) {
    assert!(
        scope
            .wasm_collection_wrapper_registry()
            .references()
            .iter()
            .any(|reference| {
                reference.wrapper().as_ref() == wrapper
                    && reference.door() == door
                    && reference.dependency_owned() == dependency_owned
            }),
        "the emitted {door} seam must record `{wrapper}` as a {} reference; registry: {:#?}",
        if dependency_owned {
            "dependency-owned"
        } else {
            "locally responsible"
        },
        scope.wasm_collection_wrapper_registry()
    );
}

fn borrowed_sidecar(dep: &str, name: &str, shape: &str) -> String {
    format!(
        "// This file was code-generated using an experimental CDDL to rust tool:\n\
         // https://github.com/dcSpark/cddl-codegen\n\
         \n\
         // This file records every collection wrapper this crate borrows from workspace deps.\n\
         // It is machine-read by those deps' generation runs (--wrapper-requests) and compiled\n\
         // here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.\n\
         // Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).\n\
         #[allow(unused_imports)]\n\
         mod borrowed {{\n\
             use {dep}_wasm::collections::{name};\n\
         }}\n\
         #[allow(dead_code)]\n\
         pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n\
             (\"{dep}\", \"{name}\", \"{shape}\"),\n\
         ];\n"
    )
}

#[test]
fn synthesized_name_registry_provider_kinds_and_missing_rows_are_deterministic() {
    let root = (*ROOT_SCOPE).clone();
    let external_scope =
        ModuleScope::from(vec![EXTERN_DEPS_DIR.to_owned(), "declared_dep".to_owned()]);
    let mut registry = WasmCollectionWrapperRegistry::default();

    registry.record_local_class(
        ident("LocalClass"),
        root.clone(),
        "[* uint]".to_owned(),
        true,
    );
    registry.record_local_alias(ident("LocalAlias"), root.clone());
    registry.record_deferred(
        ident("Deferred"),
        ModuleScope::from(vec!["declared_dep".to_owned(), "collections".to_owned()]),
    );
    registry.record_dependency_class(
        ident("DependencyClass"),
        external_scope.clone(),
        "{* uint => uint}".to_owned(),
    );
    registry.record_dependency_alias(ident("DependencyAlias"), external_scope.clone());
    for name in [
        "LocalClass",
        "LocalAlias",
        "Deferred",
        "DependencyClass",
        "DependencyAlias",
    ] {
        registry.record_reference(
            ident(name),
            ident("Owner"),
            "fixture provider door",
            false,
            matches!(name, "DependencyClass" | "DependencyAlias").then(|| external_scope.clone()),
        );
    }
    // A dependency-owned source reference has no locally recorded definition by design.
    registry.record_reference(
        ident("DependencyOnly"),
        ident("DepOwner"),
        "fixture dep door",
        true,
        None,
    );

    registry
        .closure_check()
        .expect("each honest provider kind and a dependency-owned reference must close");
    assert_eq!(
        registry.definition_kind(&ident("Deferred")),
        Some(WasmCollectionWrapperDefinition::Deferred),
        "deferral must remain distinct from a locally indexed class"
    );
    assert!(
        !registry.local_classes().contains_key(&ident("Deferred")),
        "a deferred wrapper must never appear in the local collections index"
    );
    assert_eq!(
        registry.own_wrapper_shape("[* uint]"),
        Some(&ident("LocalClass")),
        "own-spec shape lookup must project from the local class definition"
    );
    assert_eq!(
        registry.definition_kind(&ident("DependencyClass")),
        Some(WasmCollectionWrapperDefinition::DependencyClass),
        "a named extern-dependency collection is a provider without pretending to be local"
    );
    assert_eq!(
        registry.definition_kind(&ident("DependencyAlias")),
        Some(WasmCollectionWrapperDefinition::DependencyAlias),
        "a named extern-dependency collection alias is likewise a non-local provider"
    );

    let mut missing = WasmCollectionWrapperRegistry::default();
    missing.record_reference(ident("ZuluList"), ident("Zed"), "second door", false, None);
    missing.record_reference(ident("AlphaList"), ident("Ada"), "first door", false, None);
    let error = missing
        .closure_check()
        .expect_err("an unprovided local collection reference must stop generation")
        .to_string();
    assert_eq!(
        error,
        "missing wasm collection wrapper `AlphaList` referenced by `Ada` via first door\n\
         missing wasm collection wrapper `ZuluList` referenced by `Zed` via second door",
        "the graceful diagnostic must be sorted by the registry's stable key"
    );

    let mut same_name_dependency_collision = WasmCollectionWrapperRegistry::default();
    same_name_dependency_collision.record_dependency_class(
        ident("SharedName"),
        external_scope.clone(),
        "[* text]".to_owned(),
    );
    same_name_dependency_collision.record_reference(
        ident("SharedName"),
        ident("LocalOwner"),
        "local field",
        false,
        None,
    );
    assert_eq!(
        same_name_dependency_collision
            .closure_check()
            .expect_err("a same-named dependency class must not bless an unrelated local reference")
            .to_string(),
        "missing wasm collection wrapper `SharedName` referenced by `LocalOwner` via local field"
    );

    let mut exact_dependency_collision = WasmCollectionWrapperRegistry::default();
    exact_dependency_collision.record_local_class(
        ident("SharedName"),
        root.clone(),
        "[* uint]".to_owned(),
        true,
    );
    exact_dependency_collision.record_local_alias(ident("SharedName"), root);
    exact_dependency_collision.record_deferred(
        ident("SharedName"),
        ModuleScope::from(vec!["declared_dep".to_owned(), "collections".to_owned()]),
    );
    exact_dependency_collision.record_reference(
        ident("SharedName"),
        ident("LocalOwner"),
        "dependency-selected field",
        false,
        Some(external_scope),
    );
    assert_eq!(
        exact_dependency_collision
            .closure_check()
            .expect_err(
                "local, alias, and deferred providers sharing a dependency-selected spelling must not replace that exact provider",
            )
            .to_string(),
        "missing wasm collection wrapper `SharedName` referenced by `LocalOwner` via dependency-selected field"
    );
}

/// A synthesized spelling is not an ordinary dependency import merely because an unrelated
/// dependency collection owns the same Rust ident. This was the production hole a pure registry
/// collision could not expose: `types.scope(FooList)` points at the dependency even though the
/// local `[* foo]` renderer selected `FooList` structurally.
#[test]
fn unrelated_dependency_class_with_structural_spelling_does_not_close_local_reference() {
    let root = scratch("unrelated_dependency_spelling");
    write(
        &root,
        "lib.cddl",
        "foo = [x: uint]\nholder = [items: [* foo]]\n",
    );
    write(
        &root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/probe_dep/mod.cddl",
        "other = [x: text]\nfoo_list = [* other]\n",
    );
    let cli = cli(
        &root,
        &["--extern-wasm-crate=probe_dep=probe_dep_wasm".to_owned()],
    );
    let scope = generated_scope(&cli);
    let registry = scope.wasm_collection_wrapper_registry();

    assert_eq!(
        registry.definition_kind(&ident("FooList")),
        Some(WasmCollectionWrapperDefinition::DependencyClass),
        "the same-named dependency class remains registered at its real owner"
    );
    assert!(
        registry.references().iter().any(|reference| {
            reference.wrapper().as_ref() == "FooList"
                && !reference.dependency_owned()
                && reference.dependency_provider_scope().is_none()
        }),
        "the local structural list reference must not select the unrelated dependency provider"
    );
    let error = registry
        .closure_check()
        .expect_err("the unrelated dependency class must not satisfy the local structural name")
        .to_string();
    assert!(
        error.contains("missing wasm collection wrapper `FooList`")
            && error.contains("referenced by `Holder`"),
        "the closure error must identify the local owner rather than blessing the collision: {error}"
    );
    let producer_error = crate::api::generated_strings(&cli)
        .expect_err("the shared source producer must stop before returning the broken wasm map")
        .to_string();
    assert!(
        producer_error.contains("missing wasm collection wrapper `FooList`"),
        "the production seam must surface the closure verdict: {producer_error}"
    );
}

/// Cycle 21's own-spec resident. The flattened open-table `keys()` accessor, rather than a
/// whole-map traversal, is what actually writes `MdList` into the wasm signature.
#[test]
fn own_spec_table_keys_reference_is_registered_and_closes_over_md_list() {
    let root = scratch("md_list");
    let input = write(
        &root,
        "input.cddl",
        "md = uint / text\n\
         b5_403_first_key = {\n  + bstr => uint\n  , 2*3 md => md ; @name first_key\n}\n\
         b5_403_first_value = {\n  + bstr => uint ; @name first_key\n  , 2*3 md => md ; @name first_value\n}\n\
         b5_403_typed_builder = {\n  2*3 bstr => uint ; @name entries\n  , 2*3 md => md ; @name entries_builder\n}\n",
    );
    let cli = cli(&input, &[]);
    let mut scope = generated_scope(&cli);
    assert_registered_reference(&scope, "MdList", "table keys return", false);
    scope
        .wasm_collection_wrapper_registry()
        .closure_check()
        .expect("the real MdList mint must satisfy the real keys() reference");

    assert!(
        scope
            .remove_wasm_collection_local_class_for_test(&ident("MdList"))
            .is_some(),
        "the resident must have a real local class provider to remove"
    );
    let error = scope
        .wasm_collection_wrapper_registry()
        .closure_check()
        .expect_err("removing MdList must expose the real keys() reference")
        .to_string();
    assert!(
        error.contains("missing wasm collection wrapper `MdList`")
            && error.contains("via table keys return"),
        "the missing-provider error must name the accessor door: {error}"
    );
    let generated_files_error =
        generated_files_error_after_removing_local_provider(&cli, &ident("MdList"));
    assert!(
        generated_files_error.contains("missing wasm collection wrapper `MdList`")
            && generated_files_error.contains("via table keys return"),
        "the producer seam must reject the missing provider before returning files: {generated_files_error}"
    );
}

#[test]
fn emitted_collection_aliases_register_the_target_spelling_they_write() {
    let root = scratch("collection_alias_targets");
    let input = write(&root, "input.cddl", "mp = { * uint => uint }\nptm = mp\n");
    let scope = generated_scope(&cli(&input, &[]));
    let registry = scope.wasm_collection_wrapper_registry();

    assert_eq!(
        registry.definition_kind(&ident("Ptm")),
        Some(WasmCollectionWrapperDefinition::LocalAlias),
        "the authored wasm alias is a source provider, never a collections-index class"
    );
    assert_eq!(
        registry.definition_kind(&ident("MapU64ToU64")),
        Some(WasmCollectionWrapperDefinition::LocalAlias),
        "the sole-owner structural alias is likewise a local source provider"
    );
    assert_registered_reference(&scope, "Mp", "wasm pub type alias target", false);
    assert_registered_reference(&scope, "Mp", "sole-owner structural alias target", false);
    assert!(
        !registry.references().iter().any(|reference| {
            reference.wrapper().as_ref() == "MapU64ToU64"
                && reference.door() == "wasm pub type alias target"
        }),
        "a stripped named alias must record the named class it writes, not its transparent base shape"
    );
    registry
        .closure_check()
        .expect("both emitted alias definitions and their exact target references must close");
}

/// A named restricted collection is a configured alias: the field signature spells the alias
/// (`Ne`/`Bd`/`Uniq`), while its `AliasInfo::base_type` carries the bound or duplicate policy that
/// proves that spelling belongs to the collection-wrapper namespace. This guards against peeling the
/// conceptual alias inner into a fresh loose array and silently omitting the real field reference.
#[test]
fn named_restricted_list_and_set_alias_fields_register_their_emitted_providers() {
    let root = scratch("named_restricted_aliases");
    let input = write(
        &root,
        "input.cddl",
        "ne = [+ uint]\n\
         bd = [2*3 uint]\n\
         uniq = [* uint] ; @duplicates reject\n\
         holder = [nonempty: ne, bounded: bd, unique: uniq]\n",
    );
    let scope = generated_scope(&cli(&input, &[]));
    let registry = scope.wasm_collection_wrapper_registry();

    for class in ["Ne", "Bd", "Uniq"] {
        assert_eq!(
            registry.definition_kind(&ident(class)),
            Some(WasmCollectionWrapperDefinition::LocalClass),
            "the emitted named restricted collection `{class}` must be a local class provider"
        );
        assert_registered_reference(&scope, class, "record constructor parameter", false);
    }
    registry.closure_check().expect(
        "every named restricted alias field must close over its emitted alias/class provider",
    );
}

/// B5-403's requested bounded-map resident. The request host has no own-spec table traversal, so
/// its `keys()` reference proves the registry is wired at the accessor rendering seam itself.
#[test]
fn requested_bounded_map_keys_reference_is_registered_and_closes_over_its_companion() {
    let root = scratch("requested_bounded_map");
    let input = write(&root, "lib.cddl", "idx_foo = [x: uint]\n");
    let sidecar = write(
        &root,
        "borrowed_collections.rs",
        &borrowed_sidecar(
            "registry_dep",
            "MapIdxFooToIdxFooMin2Max3",
            "{2*3 idx_foo => idx_foo}",
        ),
    );
    // This dependency rule intentionally owns the requested wrapper's STRUCTURAL spelling, but
    // is an unrelated record rather than the requested class. The requested wrapper is emitted in
    // `requested_collections`; its internal `IdxFooList` reference must remain locally responsible
    // despite `types.scope(MapIdxFooToIdxFooMin2Max3)` resolving to this extern scope.
    write(
        &root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/collision_dep/mod.cddl",
        "map_idx_foo_to_idx_foo_min2_max3 = [field: uint]\n",
    );
    let mut scope = generated_scope(&cli(
        &input,
        &[
            "--lib-name=registry-dep".to_owned(),
            format!("--wrapper-requests=registry-consumer={}", sidecar.display()),
            "--common-import-override=collision_dep".to_owned(),
            "--extern-wasm-crate=collision_dep=collision_dep_wasm".to_owned(),
        ],
    ));
    assert_eq!(
        scope
            .wasm_collection_wrapper_registry()
            .definition_kind(&ident("MapIdxFooToIdxFooMin2Max3")),
        Some(WasmCollectionWrapperDefinition::LocalClass),
        "the requested bounded map itself must be hosted as a local class"
    );
    assert_registered_reference(&scope, "IdxFooList", "table keys return", false);
    scope
        .wasm_collection_wrapper_registry()
        .closure_check()
        .expect("the requested host must mint the keys-list companion it references");

    assert!(
        scope
            .remove_wasm_collection_local_class_for_test(&ident("IdxFooList"))
            .is_some(),
        "the requested keys-list must have a local provider"
    );
    let error = scope
        .wasm_collection_wrapper_registry()
        .closure_check()
        .expect_err("removing the requested keys-list must expose its accessor reference")
        .to_string();
    assert!(
        error.contains("missing wasm collection wrapper `IdxFooList`")
            && error.contains("via table keys return"),
        "the requested bounded-map resident must fail at the keys() door: {error}"
    );
}

/// This is deliberately registry-derived rather than source-presence-only: each row proves the
/// class a real shape mints and a real signature/accessor reference resolve through the same local
/// provider. The requested rows exercise the otherwise IR-ownerless hosting path.
#[test]
fn own_and_requested_collection_flavors_have_registered_local_reference_providers() {
    struct Fixture {
        name: &'static str,
        own_spec: Option<&'static str>,
        requested_shape: Option<&'static str>,
        class: &'static str,
        references: &'static [(&'static str, &'static str)],
    }

    let fixtures = [
        Fixture {
            name: "own-loose",
            own_spec: Some(
                "registry_key = [x: uint]\nregistry_map = { * registry_key => registry_key }\nholder = [items: registry_map]\n",
            ),
            requested_shape: None,
            class: "RegistryMap",
            references: &[("RegistryMap", "record constructor parameter")],
        },
        Fixture {
            name: "own-nonempty",
            own_spec: Some(
                "registry_key = [x: uint]\nregistry_map = { + registry_key => registry_key }\nholder = [items: registry_map]\n",
            ),
            requested_shape: None,
            class: "RegistryMap",
            references: &[
                ("RegistryMap", "record constructor parameter"),
                (
                    "MapRegistryKeyToRegistryKey",
                    "non-empty map try_from loose-map source",
                ),
            ],
        },
        Fixture {
            name: "own-bounded",
            own_spec: Some(
                "registry_key = [x: uint]\nregistry_map = { 2*3 registry_key => registry_key }\nholder = [items: registry_map]\n",
            ),
            requested_shape: None,
            class: "RegistryMap",
            references: &[
                ("RegistryMap", "record constructor parameter"),
                (
                    "MapRegistryKeyToRegistryKey",
                    "bounded map try_from loose-map source",
                ),
            ],
        },
        Fixture {
            name: "own-pair-map",
            own_spec: Some(
                "registry_key = [x: uint]\nregistry_map = { * registry_key => registry_key } ; @duplicates preserve\nholder = [items: registry_map]\n",
            ),
            requested_shape: None,
            class: "RegistryMap",
            references: &[("RegistryMap", "record constructor parameter")],
        },
        Fixture {
            name: "requested-loose",
            own_spec: None,
            requested_shape: Some("{* idx_foo => idx_foo}"),
            class: "MapIdxFooToIdxFoo",
            references: &[("IdxFooList", "table keys return")],
        },
        Fixture {
            name: "requested-nonempty",
            own_spec: None,
            requested_shape: Some("{+ idx_foo => idx_foo}"),
            class: "NonEmptyMapIdxFooToIdxFoo",
            references: &[
                ("IdxFooList", "table keys return"),
                (
                    "MapIdxFooToIdxFoo",
                    "non-empty map try_from loose-map source",
                ),
            ],
        },
        Fixture {
            name: "requested-bounded",
            own_spec: None,
            requested_shape: Some("{2*3 idx_foo => idx_foo}"),
            class: "MapIdxFooToIdxFooMin2Max3",
            references: &[
                ("IdxFooList", "table keys return"),
                ("MapIdxFooToIdxFoo", "bounded map try_from loose-map source"),
            ],
        },
        Fixture {
            name: "requested-pair-map",
            own_spec: None,
            requested_shape: Some("{* idx_foo => idx_foo} @duplicates preserve"),
            class: "PairMapIdxFooToIdxFoo",
            references: &[("IdxFooList", "table keys return")],
        },
    ];

    for fixture in fixtures {
        let root = scratch(fixture.name);
        let (input, extra) = match (fixture.own_spec, fixture.requested_shape) {
            (Some(spec), None) => (write(&root, "lib.cddl", spec), Vec::new()),
            (None, Some(shape)) => {
                let input = write(&root, "lib.cddl", "idx_foo = [x: uint]\n");
                let sidecar = write(
                    &root,
                    "borrowed_collections.rs",
                    &borrowed_sidecar("registry_dep", fixture.class, shape),
                );
                (
                    input,
                    vec![
                        "--lib-name=registry-dep".to_owned(),
                        format!("--wrapper-requests={}={}", fixture.name, sidecar.display()),
                    ],
                )
            }
            _ => unreachable!("each fixture is either own-spec or requested-host"),
        };
        let scope = generated_scope(&cli(&input, &extra));
        let registry = scope.wasm_collection_wrapper_registry();
        assert_eq!(
            registry.definition_kind(&ident(fixture.class)),
            Some(WasmCollectionWrapperDefinition::LocalClass),
            "{} must mint its expected wasm class",
            fixture.name
        );
        for (referenced, door) in fixture.references {
            assert_registered_reference(&scope, referenced, door, false);
            assert_eq!(
                registry.definition_kind(&ident(referenced)),
                Some(WasmCollectionWrapperDefinition::LocalClass),
                "{} must resolve its real emitted {door} reference through a local class",
                fixture.name
            );
        }
        registry
            .closure_check()
            .unwrap_or_else(|error| panic!("{} left an unclosed reference: {error}", fixture.name));
    }
}

#[test]
fn declared_deferral_and_extern_dependency_ownership_do_not_create_local_index_rows() {
    let deferred_root = scratch("declared_deferral");
    write(
        &deferred_root,
        "lib.cddl",
        "holder = [items: [* idx_foo]]\n",
    );
    write(
        &deferred_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/registry_dep/mod.cddl",
        "idx_foo = _CDDL_CODEGEN_EXTERN_TYPE_\n",
    );
    let deferred = generated_scope(&cli(
        &deferred_root,
        &[
            "--workspace-dep=registry_dep".to_owned(),
            "--extern-wasm-crate=registry_dep=registry_dep_wasm".to_owned(),
        ],
    ));
    let deferred_registry = deferred.wasm_collection_wrapper_registry();
    assert_registered_reference(
        &deferred,
        "IdxFooList",
        "record constructor parameter",
        false,
    );
    assert_eq!(
        deferred_registry.definition_kind(&ident("IdxFooList")),
        Some(WasmCollectionWrapperDefinition::Deferred),
        "the declared workspace deferral is the provider for the exported owner's list reference"
    );
    assert!(
        !deferred_registry
            .local_classes()
            .contains_key(&ident("IdxFooList")),
        "a deferred wrapper must not be re-exported from this crate's collections index"
    );
    deferred_registry
        .closure_check()
        .expect("a declared deferral must satisfy the closure check");

    // The physical extern-dependency fixture has a table and array rule entirely inside the
    // dependency scope. They are walked to discover their nested type doors, but none becomes a
    // consumer-owned class. This is intentionally not an @extern_companions case: that directive
    // is refused on dependency-scoped externs, and normal dependency scope ownership is the point.
    let external_input = Path::new("tests/extern-deps-wasm/inputs");
    let external = generated_scope(&cli(
        external_input,
        &[
            "--common-import-override=extern_dep_crate".to_owned(),
            "--extern-wasm-crate=extern_dep_crate=extern_dep_crate_wasm".to_owned(),
        ],
    ));
    let external_registry = external.wasm_collection_wrapper_registry();
    assert_registered_reference(&external, "ExternCrateBazList", "table keys return", true);
    assert!(
        !external_registry
            .local_classes()
            .contains_key(&ident("ExternCrateBazList")),
        "a non-exported extern-scope wrapper must not become a consumer collection-index row"
    );
    external_registry
        .closure_check()
        .expect("dependency-owned reference rows intentionally need no local mint");

    // A consumer can also name a collection class the dependency exported under its own (pinned)
    // spelling. This is an honest dependency provider, but it travels through ordinary scope
    // imports rather than `deferred`, whose routing is exclusively structural-wrapper imports.
    let named_external = generated_scope(&cli(
        Path::new("tests/dep-owned-named-collections/inputs"),
        &[
            "--common-import-override=extern_dep_crate".to_owned(),
            "--extern-wasm-crate=extern_dep_crate=extern_dep_crate_wasm".to_owned(),
        ],
    ));
    let named_external_registry = named_external.wasm_collection_wrapper_registry();
    assert_eq!(
        named_external_registry.definition_kind(&ident("DepWithdrawals")),
        Some(WasmCollectionWrapperDefinition::DependencyClass),
        "a named extern collection must be a dependency provider without becoming local"
    );
    assert!(
        !named_external_registry
            .local_classes()
            .contains_key(&ident("DepWithdrawals")),
        "a dependency class must not leak into the consumer's collections index"
    );
    let expected_dependency_scope = ModuleScope::from(vec![
        EXTERN_DEPS_DIR.to_owned(),
        "extern_dep_crate".to_owned(),
    ]);
    assert!(
        named_external_registry
            .references()
            .iter()
            .any(|reference| {
                reference.wrapper().as_ref() == "DepWithdrawals"
                    && !reference.dependency_owned()
                    && reference.dependency_provider_scope() == Some(&expected_dependency_scope)
            }),
        "a local consumer reference must explicitly resolve the exact named dependency provider"
    );
    named_external_registry
        .closure_check()
        .expect("the named dependency provider must close consumer-side references");
}

#[test]
fn named_extern_collection_alias_is_an_exact_dependency_provider() {
    let root = scratch("named_extern_collection_alias");
    write(&root, "lib.cddl", "holder = [field: dep_alias]\n");
    write(
        &root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/extern_dep_crate/mod.cddl",
        "extern_thing = _CDDL_CODEGEN_EXTERN_TYPE_\n\
         dep_map = { * uint => extern_thing } ; @rust_name DepMap\n\
         dep_alias = dep_map ; @rust_name DepAlias\n",
    );
    let cli = cli(
        &root,
        &[
            "--common-import-override=extern_dep_crate".to_owned(),
            "--extern-wasm-crate=extern_dep_crate=extern_dep_crate_wasm".to_owned(),
        ],
    );
    let scope = generated_scope(&cli);
    let registry = scope.wasm_collection_wrapper_registry();
    let expected_dependency_scope = ModuleScope::from(vec![
        EXTERN_DEPS_DIR.to_owned(),
        "extern_dep_crate".to_owned(),
    ]);

    assert_eq!(
        registry.definition_kind(&ident("DepAlias")),
        Some(WasmCollectionWrapperDefinition::DependencyAlias),
        "the local consumer's imported dep alias must remain an exact dependency provider"
    );
    assert!(
        !registry.local_classes().contains_key(&ident("DepAlias")),
        "a dependency-provided alias must not leak into this crate's collections index"
    );
    assert!(
        registry.references().iter().any(|reference| {
            reference.wrapper().as_ref() == "DepAlias"
                && !reference.dependency_owned()
                && reference.dependency_provider_scope() == Some(&expected_dependency_scope)
        }),
        "the local holder must explicitly resolve DepAlias from its declaring dependency scope"
    );
    registry
        .closure_check()
        .expect("the exact dependency alias provider must close the local signature reference");
    let generated = crate::api::generated_strings(&cli)
        .expect("the shared source producer must accept the exact dependency alias provider");
    let wasm = &generated["wasm/src/generated/mod.rs"];
    assert!(
        wasm.contains("use extern_dep_crate_wasm::") && wasm.contains("DepAlias"),
        "the accepted local signature must import the alias from its dependency provider:\n{wasm}"
    );
}

/// The five files are the finite wasm type-rendering emitter registry. The only direct renderer
/// calls permitted inside it are the three helpers in `generation/mod.rs`; all emission doors must
/// go through those helpers so they cannot bypass `record_wasm_type_reference`.
#[test]
fn wasm_type_rendering_doors_are_lockstep_routed_through_the_registry() {
    const WASM_RENDERING_EMITTERS: &[&str] = &[
        "src/generation/mod.rs",
        "src/generation/records.rs",
        "src/generation/enums.rs",
        "src/generation/wrappers.rs",
        "src/generation/collections.rs",
    ];
    let mut direct_calls = Vec::new();
    for path in WASM_RENDERING_EMITTERS {
        let contents =
            std::fs::read_to_string(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(path))
                .unwrap_or_else(|error| panic!("reading wasm emitter {path}: {error}"));
        for line in contents.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            if ["for_wasm_member(", "for_wasm_param(", "for_wasm_return("]
                .iter()
                .any(|call| trimmed.contains(call))
            {
                direct_calls.push(((*path).to_owned(), trimmed.to_owned()));
            }
        }
    }
    assert_eq!(
        direct_calls,
        vec![
            (
                "src/generation/mod.rs".to_owned(),
                "let rendered = ty.for_wasm_member(types);".to_owned(),
            ),
            (
                "src/generation/mod.rs".to_owned(),
                "let rendered = ty.for_wasm_param(types);".to_owned(),
            ),
            (
                "src/generation/mod.rs".to_owned(),
                "let rendered = ty.for_wasm_return(types);".to_owned(),
            ),
        ],
        "a direct wasm type render outside the three registry helpers would bypass reference closure"
    );
}
