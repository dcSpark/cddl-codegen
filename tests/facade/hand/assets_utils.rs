// Hand-written per-scope module for the `assets` scope, copied to `src/assets/utils.rs` of the
// generated crate. Published under the scope's public path by the facade in `lib.rs`
// (docs/docs/output_format.mdx § "Per-scope hand modules: the facade pattern"). It exercises the
// three halves of the facade contract that the string-level ingredient pins cannot: (a) a hand
// item merged into the scope namespace, (b) an inherent impl on the generated wrapper that reads
// its `pub(crate) inner` field from OUTSIDE the generated subtree, and (c) `_assert_paths`, which
// pins every public path the composition promises.

use crate::assets::Bounded;

// (a) A hand item published under the scope path. The facade's `pub use utils::*;` merges it into
// `crate::assets::*` alongside the generated items.
#[derive(Clone, Copy)]
pub struct AssetTag;

// (b) An inherent impl on the GENERATED wrapper, authored in a hand module living outside
// `src/generated/**`, reading the `pub(crate)` `inner` field (named `inner` under
// --preserve-encodings=true). This is the pub(crate) wrapper-field half of the contract: an
// external crate could not touch `inner`, but this in-crate hand module can.
impl Bounded {
    pub fn hand_first_byte(&self) -> Option<u8> {
        self.inner.first().copied()
    }
}

// (c) Compile-level pins for every public path the facade composition promises. Typed bindings and
// identity functions fail to type-check if any path stops resolving OR starts resolving to a
// different type — which is exactly how a composition regression (generation emitting public
// `crate::assets::…` paths that the facade rebinds, a scope losing its `pub mod`, the root glob
// disappearing) would surface.
#[allow(dead_code)]
fn _assert_paths() {
    // The generated wrapper resolves at `crate::assets::Bounded` THROUGH the facade, and it is the
    // SAME type as the generated definition (identity fn only type-checks if the paths coincide).
    fn _same(x: crate::generated::assets::Bounded) -> crate::assets::Bounded {
        x
    }

    // The hand item resolves at BOTH its defining module path AND the per-scope glob path, and the
    // two are the same type (the assignment type-checks only if they coincide).
    let _hand: crate::assets::utils::AssetTag = crate::assets::AssetTag;

    // The extern resolves at the crate root (the `address` scope's glue re-exports `crate::Address`).
    let _addr: Option<crate::Address> = None;

    // The root record resolves at the crate root through `pub use generated::*;`.
    let _root: Option<crate::Root> = None;

    // A generated scope submodule keeps resolving through the facade's `pub use crate::generated::
    // assets::*;` (a `pub mod serialization` re-exported by the glob).
    use crate::assets::serialization as _;
}
