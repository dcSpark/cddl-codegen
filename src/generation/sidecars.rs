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
