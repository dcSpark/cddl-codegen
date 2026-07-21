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

// Wasm FACES of the dep-owned NAMED collection rules (`DepWithdrawals`/`DepCerts`). A consumer
// referencing them by name (from a non-root module) emits a wasm wrapper whose getters/constructor
// cross the boundary with `.clone().into()` — `self.0.wd.clone().into()` (rust
// `extern_dep_crate::DepWithdrawals` -> this wasm class) and `wd.clone().into()` (this wasm class ->
// rust). Same wrap + `From` contract as `ExternCrateFoo`; the `From<Self> for <rust alias>` direction
// is orphan-rule-legal because the local wasm type appears as the `From` type parameter (identical to
// the `From<ExternCrateFoo> for extern_dep_crate::ExternCrateFoo` impl above). The rust aliases are
// `BTreeMap`/`Vec`, so these wrap those directly.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct DepWithdrawals(extern_dep_crate::DepWithdrawals);

impl From<extern_dep_crate::DepWithdrawals> for DepWithdrawals {
    fn from(native: extern_dep_crate::DepWithdrawals) -> Self {
        Self(native)
    }
}

impl From<DepWithdrawals> for extern_dep_crate::DepWithdrawals {
    fn from(wasm: DepWithdrawals) -> Self {
        wasm.0
    }
}

impl AsRef<extern_dep_crate::DepWithdrawals> for DepWithdrawals {
    fn as_ref(&self) -> &extern_dep_crate::DepWithdrawals {
        &self.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct DepCerts(extern_dep_crate::DepCerts);

impl From<extern_dep_crate::DepCerts> for DepCerts {
    fn from(native: extern_dep_crate::DepCerts) -> Self {
        Self(native)
    }
}

impl From<DepCerts> for extern_dep_crate::DepCerts {
    fn from(wasm: DepCerts) -> Self {
        wasm.0
    }
}

impl AsRef<extern_dep_crate::DepCerts> for DepCerts {
    fn as_ref(&self) -> &extern_dep_crate::DepCerts {
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
