// Hand-written thin root for the `facade_composition_compiles` gate. Copied VERBATIM over the
// seeded `src/lib.rs` of the generated crate, then the crate is `cargo check`ed. It composes the
// generated output exactly as docs/docs/output_format.mdx prescribes:
//   * § "Per-scope hand modules: the facade pattern" — a facade module for the `assets` scope that
//     shadows the glob-imported generated `assets` (item-beats-glob), re-exports the generated
//     items (incl. `pub mod serialization`), declares the hand file at `src/assets/utils.rs`, and
//     glob-merges the hand items into the scope's public namespace.
//   * § "This is what makes extern types ... durable" — the extern-glue re-export: the `address`
//     scope's generated `mod.rs` emits `pub use crate::Address;`, so `Address` must be defined in a
//     hand module and re-exported at the crate root.
mod generated;
pub use generated::*;

pub mod assets {
    pub use crate::generated::assets::*; // generated items, incl. `pub mod serialization`
    pub mod utils; // hand file at src/assets/utils.rs
    pub use utils::*; // per-scope glob: merge the hand items into `crate::assets::*`
}

pub mod addr_impl;
pub use addr_impl::Address; // resolves the extern glue's `pub use crate::Address;`
