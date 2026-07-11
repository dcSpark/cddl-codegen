// Primitive alias, mirroring how a generated `<dep>-wasm` crate emits `pub type` aliases for
// non-struct typedefs — present so the mapped wasm crate covers module-path depth (`sub::module`).
pub type UintTypedef = u64;

use wasm_bindgen::prelude::wasm_bindgen;

// The wasm boundary wrapper for the dep's second extern type, living in a SUBMODULE so the consumer's
// newly-registered wrapper-element import must be remapped through `extern_dep_crate_wasm::sub::module`
// (the exact submodule-path composition the `nested` cells pin). Same From/AsRef contract the generated
// list/map wrappers rely on as `ExternCrateFoo`.
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct ExternCrateBar(extern_dep_crate::sub::module::ExternCrateBar);

impl From<extern_dep_crate::sub::module::ExternCrateBar> for ExternCrateBar {
    fn from(native: extern_dep_crate::sub::module::ExternCrateBar) -> Self {
        Self(native)
    }
}

impl From<ExternCrateBar> for extern_dep_crate::sub::module::ExternCrateBar {
    fn from(wasm: ExternCrateBar) -> Self {
        wasm.0
    }
}

impl AsRef<extern_dep_crate::sub::module::ExternCrateBar> for ExternCrateBar {
    fn as_ref(&self) -> &extern_dep_crate::sub::module::ExternCrateBar {
        &self.0
    }
}
