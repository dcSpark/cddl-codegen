// Umbrella cdylib for the two-consumer regen-contract gate. It anchors both consumer wasm crates
// (and, transitively, the dep wasm crate they defer to) into a single wasm32 link. Referencing a
// `#[wasm_bindgen]` associated function from each consumer forces that crate's whole wasm-bindgen
// surface — including any locally minted collection wrapper — into the artifact, so:
//   * workspace mode OFF: A and B each mint `FooList` -> two `#[wasm_bindgen] FooList` -> the linker
//     reports a duplicate symbol (the feature's headline failure).
//   * workspace mode ON: the dep hosts each shared wrapper once and both consumers defer -> one
//     definition each -> the umbrella links clean.
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn _umbrella_anchor() -> u32 {
    // Take the address of a wasm-bindgen export from each consumer; the cast keeps this a pure
    // linkage anchor with no runtime dependency on the crates' internals.
    let a = consumer_a_wasm::Alpha::from_cbor_bytes as *const () as usize;
    let b = consumer_b_wasm::Beta::from_cbor_bytes as *const () as usize;
    let d = regen_dep_wasm::Foo::to_cbor_bytes as *const () as usize;
    (a ^ b ^ d) as u32
}
