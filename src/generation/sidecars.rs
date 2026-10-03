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
