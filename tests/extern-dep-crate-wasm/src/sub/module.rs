// Primitive alias, mirroring how a generated `<dep>-wasm` crate emits `pub type` aliases for
// non-struct typedefs — present so the mapped wasm crate covers module-path depth (`sub::module`).
pub type UintTypedef = u64;
