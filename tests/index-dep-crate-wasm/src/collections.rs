// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// Collection-wrapper index for this crate: one `pub use` re-export per collection
// wrapper class defined here (list/map wrappers minted from `[* T]` / `{* K => V}`
// shapes, including their NonEmpty variants). Compiled as part of this crate, so a
// line naming a removed wrapper fails this crate's own build — the index cannot
// drift. Downstream crates point `--extern-wrapper-index <dep>=<this file>` here to
// avoid re-minting these wrappers (a wasm duplicate-symbol link error otherwise).
//
// Hand-written stand-in mirroring the exact format `generation.rs` emits, so the
// `--extern-wrapper-index` line parser is exercised against real generated shape.
// Deliberately does NOT list MapIdxFooToIdxFoo, so a consumer's all-extern
// `{* idx_foo => idx_foo}` mints locally + warns (the "candidate not in index" path).
// Also deliberately absent: any restricted `[+ idx_bar]` / `[+ idx_baz]` wrapper —
// their LOOSE lists below ARE indexed, so the consumer mints the restricted class
// locally while deferring its `try_from` source here (the deferred-source cells).
pub use crate::ArrIdxFooList;
pub use crate::IdxBarList;
pub use crate::IdxBazList;
pub use crate::IdxFooList;
pub use crate::MapU64ToIdxFoo;
pub use crate::MapU64ToText;
pub use crate::NonEmptyIdxFooList;
