use wasm_bindgen::prelude::*;

// CBOR has int = int / nint
#[wasm_bindgen]
pub struct Int(core::prelude::Int);

#[wasm_bindgen]
impl Int {
    pub fn new(x: u64) -> Self {
        Self(core::prelude::Int::Uint(x))
    }

    pub fn new_negative(x: u64) -> Self {
        Self(core::prelude::Int::Nint(x))
    }
}

impl From<core::prelude::Int> for Int {
    fn from(native: core::prelude::Int) -> Self {
        Self(native)
    }
}

impl From<Int> for core::prelude::Int {
    fn from(wasm: Int) -> Self {
        wasm.0
    }
}