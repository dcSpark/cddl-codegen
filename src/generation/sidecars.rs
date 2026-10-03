//! Pure source renderers for generated sidecars and the JSON schema crate.
//! Export retains presence, paths, formatting, manifests and write ordering.
use super::wasm_wrapper_registry::WasmCollectionWrapperClassDefinition;
use super::*;

pub(super) fn render_collections_index(
    classes: &BTreeMap<RustIdent, WasmCollectionWrapperClassDefinition>,
) -> String {
    let mut collections = String::from(
        "// Collection-wrapper index for this crate: one `pub use` re-export per collection\n\
     // wrapper class defined here (list/map wrappers minted from `[* T]` / `{* K => V}`\n\
     // shapes, including their NonEmpty variants). Compiled as part of this crate, so a\n\
     // line naming a removed wrapper fails this crate's own build — the index cannot\n\
     // drift. Downstream crates point `--extern-wrapper-index <dep>=<this file>` here to\n\
     // avoid re-minting these wrappers (duplicate exported class names fail linking or binding generation).\n",
    );
    for (ident, definition) in classes {
        let scope = &definition.scope;
        let path = if *scope == *ROOT_SCOPE {
            format!("crate::generated::{ident}")
        } else if scope.export() {
            format!(
                "crate::generated::{}::{ident}",
                scope.components().join("::")
            )
        } else {
            // Non-exported (extern-dep) scopes are never written to a file by
            // `merge_scopes_to_strings`, so a wrapper there is not part of THIS crate's
            // output and must not appear in its index. Defensive — no wrapper the crate
            // mints lands in a non-exported scope.
            continue;
        };
        collections.push_str(&format!("pub use {path};\n"));
    }
    collections
}

pub(super) fn render_borrowed_collections(
    borrowed_wrappers: &BTreeMap<RustIdent, (String, String)>,
    cli: &Cli,
) -> String {
    let extern_wasm_crate_map = cli.extern_wasm_crate_map();
    let mut entries: Vec<(&str, &str, &str)> = borrowed_wrappers
        .iter()
        .map(|(name, (dep, shape))| (dep.as_str(), name.as_ref(), shape.as_str()))
        .collect();
    entries.sort_unstable();
    // The column legend lives in the banner (anchored to the file, which always exists),
    // NEVER inside the const body: an in-const comment is anchored to a row by the
    // preservation overlay, so deleting that row on an in-place regen (a consumer
    // dropping its last borrow of a shape) trapped the legend in a `compile_error!`
    // block — which the dep-side strict parser then (correctly) refused to consume.
    let mut sidecar = String::from(
        "// This file records every collection wrapper this crate borrows from workspace deps.\n\
     // It is machine-read by those deps' generation runs (--wrapper-requests) and compiled\n\
     // here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.\n\
     // Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).\n\
     #[allow(unused_imports)]\n\
     mod borrowed {\n",
    );
    for (dep, name, _) in &entries {
        let dep_wasm = extern_wasm_crate_map
            .get(*dep)
            .map(String::as_str)
            .unwrap_or(dep);
        sidecar.push_str(&format!("    use {dep_wasm}::collections::{name};\n"));
    }
    sidecar.push_str(
        "}\n\
     #[allow(dead_code)]\n\
     pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n",
    );
    for (dep, name, shape) in &entries {
        sidecar.push_str(&format!("    ({dep:?}, {name:?}, {shape:?}),\n"));
    }
    sidecar.push_str("];\n");
    sidecar
}

pub(super) fn render_key_demand_assertions(
    assertion_roots: &[(RustIdent, DemandSet)],
    types: &IntermediateTypes,
    cli: &Cli,
) -> String {
    // The families each root's demand resolves to in THIS mode (bare is mode-dependent).
    let hash_family = |d: &DemandSet| d.hash || (d.bare && cli.preserve_encodings);
    let ord_family = |d: &DemandSet| d.ord || d.bare;
    let mut file = String::from(
        "// Compile-time key-demand assertions for `@used_as_key` tags. Each\n\
     // `_demand_<rule>` fn makes the Rust compiler prove the tagged type implements the\n\
     // traits its tag demands, turning a distant downstream trait error into a near,\n\
     // named one at the tagged type's definition site.\n",
    );
    if assertion_roots.iter().any(|(_, d)| hash_family(d)) {
        file.push_str("#[allow(dead_code)]\nfn _key_demand_hash<T: core::hash::Hash + Eq>() {}\n");
    }
    if assertion_roots.iter().any(|(_, d)| ord_family(d)) {
        file.push_str("#[allow(dead_code)]\nfn _key_demand_ord<T: Ord>() {}\n");
    }
    for (ident, demand) in assertion_roots {
        let scope = types.scope(ident);
        let path = if *scope == *ROOT_SCOPE {
            format!("crate::generated::{ident}")
        } else {
            format!(
                "crate::generated::{}::{ident}",
                scope.components().join("::")
            )
        };
        // No per-fn comment: the fn name `_demand_<rule>` already names the tagged rule and
        // the banner explains the pattern. A comment here would strand into a
        // `cddl-codegen:unpreserved-comment` compile_error trap whenever the tag (hence the
        // fn) is deleted — the same preservation-overlay hazard the banner-only rule avoids.
        file.push_str(&format!(
            "#[allow(dead_code)]\nfn _demand_{}() {{\n",
            convert_to_snake_case(ident.as_ref()),
        ));
        if hash_family(demand) {
            file.push_str(&format!("    _key_demand_hash::<{path}>();\n"));
        }
        if ord_family(demand) {
            file.push_str(&format!("    _key_demand_ord::<{path}>();\n"));
        }
        file.push_str("}\n");
    }
    file
}

pub(super) fn render_extern_interface_check(
    entries: &[crate::generation::extern_interface::ExternCheckEntry],
    types: &IntermediateTypes,
    cli: &Cli,
    deserialize_generated: impl Fn(&RustIdent) -> bool,
) -> String {
    use crate::generation::extern_interface::ExternCheckKind;
    let common = cli.common_import_rust();
    // The generated `Serialize` bound differs by mode: only the CANONICAL runtime
    // (`--preserve-encodings --canonical-form`) carries a custom `serialization::Serialize`
    // trait (its `serialize` takes a `force_canonical` flag); every other mode — including
    // preserve-without-canonical — serializes through `cbor_event::se::Serialize` directly.
    // `Deserialize` and `RawBytesEncoding` are the crate's own runtime traits in all modes.
    let serialize_bound = if cli.preserve_encodings && cli.canonical_form {
        format!("{common}::serialization::Serialize")
    } else {
        "cbor_event::se::Serialize".to_owned()
    };
    let path_of = |components: &[String], ident: &RustIdent| -> String {
        if components.is_empty() {
            format!("crate::generated::{ident}")
        } else {
            format!("crate::generated::{}::{ident}", components.join("::"))
        }
    };
    // Whole-value `Serialize`/`Deserialize` cover both the opaque `Serialize` rows AND the
    // transparent group-body `EmbeddedGroup` rows: a group-choice arm that splices a plain
    // group calls `.serialize()` on the whole value, so the whole-value bounds must hold for
    // an `EmbeddedGroup` row too (its `Deserialize` gated on the dep generating one, same as
    // `Serialize` rows).
    let deser_asserted = |entry: &crate::generation::extern_interface::ExternCheckEntry| -> bool {
        matches!(
            entry.kind,
            ExternCheckKind::Serialize | ExternCheckKind::EmbeddedGroup
        ) && deserialize_generated(&entry.ident)
    };
    let any_serialize = entries.iter().any(|e| {
        matches!(
            e.kind,
            ExternCheckKind::Serialize | ExternCheckKind::EmbeddedGroup
        )
    });
    let any_deser = entries.iter().any(deser_asserted);
    let any_raw_bytes = entries
        .iter()
        .any(|e| matches!(e.kind, ExternCheckKind::RawBytes));
    // `@copy` roots: every exported extern / raw-bytes ident declared `@copy`. The honesty
    // assertion proves the externally-defined rust type actually derives `Copy` in THIS
    // crate, so a false `@copy` fails the declaring crate's own build with a named error —
    // never a distant consumer's (a consumer imports the tag through a non-exported extern-dep
    // scope, which never reaches these entries).
    let any_copy = entries.iter().any(|e| types.is_copy_extern(&e.ident));
    // The embedded-group surface (`serialize_as_embedded_group` / `deserialize_as_embedded_group`)
    // a spliced record MEMBER delegates through, asserted only for group-body rows. Its
    // `Deserialize` twin is gated per-type on the dep generating one, exactly like the
    // whole-value side.
    let any_embedded_group = entries
        .iter()
        .any(|e| matches!(e.kind, ExternCheckKind::EmbeddedGroup));
    let any_embedded_group_deser = entries.iter().any(|e| {
        matches!(e.kind, ExternCheckKind::EmbeddedGroup) && deserialize_generated(&e.ident)
    });

    let mut file = String::from(
        "// Compiled self-check for the dep-side extern-interface export\n\
     // (`extern-interface/<dep>/**`). Machine-generated from the SAME projection as that\n\
     // export, so the two cannot drift. Every exported name is asserted to be a real,\n\
     // correctly-typed surface in THIS crate: opaque rows implement `Serialize` (and\n\
     // `Deserialize` where the dep generates one), raw-bytes rows `RawBytesEncoding`, and\n\
     // transparent rows (aliases, c-style enums, named collections) must simply exist. A\n\
     // hand-edited or stale export — or a projection bug — therefore fails THIS crate's own\n\
     // build, naming the type. Do not edit.\n\
     // Rows carry NO per-row comments by design: a spec change can delete any row, and a\n\
     // comment stranded on a deleted row is what the edit-preservation overlay turns into a\n\
     // build-breaking sentinel on the next regen. All commentary lives in this fixed banner;\n\
     // each row's type path is its own traceability.\n",
    );
    // Bound-carrier fns, emitted only for the kinds actually present so an absent trait (e.g.
    // `RawBytesEncoding` in a crate with no raw-bytes type) is never named.
    if any_serialize {
        file.push_str(&format!(
            "#[allow(dead_code)]\nfn _assert_serialize<T: {serialize_bound}>() {{}}\n"
        ));
    }
    if any_deser {
        file.push_str(&format!(
        "#[allow(dead_code)]\nfn _assert_deserialize<T: {common}::serialization::Deserialize>() {{}}\n"
    ));
    }
    if any_raw_bytes {
        file.push_str(&format!(
        "#[allow(dead_code)]\nfn _assert_raw_bytes<T: {common}::serialization::RawBytesEncoding>() {{}}\n"
    ));
    }
    if any_copy {
        file.push_str("#[allow(dead_code)]\nfn _assert_copy<T: Copy>() {}\n");
    }
    // The embedded-group traits are the crate's own runtime traits in ALL modes (unlike
    // whole-value `Serialize`, whose custom canonical variant only exists in canonical mode).
    if any_embedded_group {
        file.push_str(&format!(
        "#[allow(dead_code)]\nfn _assert_serialize_embedded_group<T: {common}::serialization::SerializeEmbeddedGroup>() {{}}\n"
    ));
    }
    if any_embedded_group_deser {
        file.push_str(&format!(
        "#[allow(dead_code)]\nfn _assert_deserialize_embedded_group<T: {common}::serialization::DeserializeEmbeddedGroup>() {{}}\n"
    ));
    }
    // Transparent rows: a module-level `use … as _;` existence check (an anonymous import
    // never triggers unused-import warnings, but stay explicit).
    for entry in entries {
        if matches!(entry.kind, ExternCheckKind::Use) {
            file.push_str(&format!(
                "#[allow(unused_imports)]\nuse {} as _;\n",
                path_of(&entry.components, &entry.ident),
            ));
        }
    }
    // Opaque / raw-bytes rows: bound-carrier instantiations inside a never-called fn.
    file.push_str("#[allow(dead_code)]\nfn _extern_interface_self_check() {\n");
    for entry in entries {
        let path = path_of(&entry.components, &entry.ident);
        match entry.kind {
            ExternCheckKind::Serialize => {
                file.push_str(&format!("    _assert_serialize::<{path}>();\n"));
                if deserialize_generated(&entry.ident) {
                    file.push_str(&format!("    _assert_deserialize::<{path}>();\n"));
                }
            }
            ExternCheckKind::EmbeddedGroup => {
                // Both surfaces the consumer's generated code uses for a spliced plain group:
                // whole-value (a group-choice arm's `.serialize()`) and embedded (a record
                // member's `serialize_as_embedded_group`), each `Deserialize` side gated on
                // the dep generating one.
                file.push_str(&format!("    _assert_serialize::<{path}>();\n"));
                file.push_str(&format!(
                    "    _assert_serialize_embedded_group::<{path}>();\n"
                ));
                if deserialize_generated(&entry.ident) {
                    file.push_str(&format!("    _assert_deserialize::<{path}>();\n"));
                    file.push_str(&format!(
                        "    _assert_deserialize_embedded_group::<{path}>();\n"
                    ));
                }
            }
            ExternCheckKind::RawBytes => {
                file.push_str(&format!("    _assert_raw_bytes::<{path}>();\n"));
            }
            ExternCheckKind::Use | ExternCheckKind::None => {}
        }
        // `@copy` honesty assertion, orthogonal to the wire-surface kind above: a `@copy`
        // extern is a Serialize row, a `@copy` raw-bytes type a RawBytes row, and either must
        // actually be `Copy`.
        if types.is_copy_extern(&entry.ident) {
            file.push_str(&format!("    _assert_copy::<{path}>();\n"));
        }
    }
    file.push_str("}\n");
    file
}

pub(super) fn render_borrowed_key_types(
    rows: &[(String, String, String, DemandSet)],
    cli: &Cli,
) -> String {
    // A borrowed key whose demand carries a `hash`/`ord` FLAVOR (a consumer keyed the dep type
    // through a `@used_as_key hash`/`ord` root) needs the flavored 3-column format + per-flavor
    // self-check bound. When every borrowed key is `bare` (the universal pre-flavor case), the
    // legacy 2-column form is emitted BYTE-IDENTICALLY — no banner/type/self-check churn.
    let any_flavored = rows.iter().any(|(_, _, _, d)| d.hash || d.ord);
    if any_flavored {
        let mut s = String::from(
            "// This file records every map-key type this crate borrows from workspace deps.\n\
         // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
         // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
         // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
         // Rows are (dep rust-crate name, cddl ident, demand flavor) of each borrowed map-key type.\n",
        );
        // One bound-carrier per distinct demand (the flavor decides the bound), then a
        // per-row self-check call routed to its flavor's carrier.
        let mut demands: Vec<DemandSet> = rows.iter().map(|(_, _, _, d)| *d).collect();
        demands.sort();
        demands.dedup();
        let assert_fn = |d: DemandSet| {
            format!(
                "_assert_key_traits_{}",
                key_flavor_token(d).replace(' ', "_")
            )
        };
        for d in &demands {
            s.push_str(&format!(
                "#[allow(dead_code)]\nfn {}<K: {}>() {{}}\n",
                assert_fn(*d),
                key_bound(*d, cli)
            ));
        }
        s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
        for (_dep, ident, scope_path, d) in rows {
            let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
            s.push_str(&format!("    {}::<{scope_path}::{ty}>();\n", assert_fn(*d)));
        }
        s.push_str("}\n");
        s.push_str(
        "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[\n",
    );
        for (dep, ident, _scope_path, d) in rows {
            let flavor = key_flavor_token(*d);
            s.push_str(&format!("    ({dep:?}, {ident:?}, {flavor:?}),\n"));
        }
        s.push_str("];\n");
        s
    } else {
        let bound = if cli.preserve_encodings {
            "Eq + Ord + PartialOrd + core::hash::Hash"
        } else {
            "Eq + Ord + PartialOrd"
        };
        let mut s = String::from(
            "// This file records every map-key type this crate borrows from workspace deps.\n\
         // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
         // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
         // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
         // Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.\n",
        );
        s.push_str(&format!(
            "#[allow(dead_code)]\nfn _assert_key_traits<K: {bound}>() {{}}\n"
        ));
        if !rows.is_empty() {
            s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
            for (_dep, ident, scope_path, _) in rows {
                let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
                s.push_str(&format!(
                    "    _assert_key_traits::<{scope_path}::{ty}>();\n"
                ));
            }
            s.push_str("}\n");
        }
        s.push_str(
            "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[\n",
        );
        for (dep, ident, _scope_path, _) in rows {
            s.push_str(&format!("    ({dep:?}, {ident:?}),\n"));
        }
        s.push_str("];\n");
        s
    }
}
