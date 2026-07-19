// Hand-written wasm-bindgen wrapper crate for `extern-dep-crate`, mirroring the module tree a
// cddl-codegen-generated `<dep>-wasm` crate would have (thin root + a `sub::module` leaf). It exists
// so the `extern_deps_wasm` integration test can exercise a dependency with a SPLIT rust/wasm crate
// layout (`extern_dep_crate` / `extern_dep_crate_wasm`) — the case `--extern-wasm-crate` targets.
//
// It is not a test but is instead used by other tests.

use wasm_bindgen::prelude::wasm_bindgen;

pub mod sub;

// The wasm boundary type the generated consumer references (via `--extern-wasm-crate
// extern_dep_crate=extern_dep_crate_wasm`). Wraps the dependency's rust type; the generated
// list/map wrappers store the rust type and convert across the boundary with `.clone().into()`,
// so the `From`/`AsRef` impls below are exactly the contract they rely on.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct ExternCrateFoo(extern_dep_crate::ExternCrateFoo);

impl From<extern_dep_crate::ExternCrateFoo> for ExternCrateFoo {
    fn from(native: extern_dep_crate::ExternCrateFoo) -> Self {
        Self(native)
    }
}

impl From<ExternCrateFoo> for extern_dep_crate::ExternCrateFoo {
    fn from(wasm: ExternCrateFoo) -> Self {
        wasm.0
    }
}

impl AsRef<extern_dep_crate::ExternCrateFoo> for ExternCrateFoo {
    fn as_ref(&self) -> &extern_dep_crate::ExternCrateFoo {
        &self.0
    }
}

// Wasm FACE of the common `Int`. A `--common-import-override` consumer re-exports the RUST `Int`
// from `extern_dep_crate` but its WASM crate re-exports THIS wrapper via `pub use
// extern_dep_crate_wasm::Int;` (routed through the `--extern-wasm-crate` mapping), so a generated
// wasm method returning `Int` gets a `#[wasm_bindgen]` class. Same wrap + `From`/`AsRef` contract as
// `ExternCrateFoo` above; the generated `IntHolder` wrapper crosses the boundary with `.clone().into()`.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct Int(extern_dep_crate::Int);

impl From<extern_dep_crate::Int> for Int {
    fn from(native: extern_dep_crate::Int) -> Self {
        Self(native)
    }
}

impl From<Int> for extern_dep_crate::Int {
    fn from(wasm: Int) -> Self {
        wasm.0
    }
}

impl AsRef<extern_dep_crate::Int> for Int {
    fn as_ref(&self) -> &extern_dep_crate::Int {
        &self.0
    }
}
