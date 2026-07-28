//! The generated workspace's LAYOUT, as constants — the handful of paths and package-name suffixes
//! that are simultaneously an emitter decision and a cross-crate flag value.
//!
//! # Why these live here rather than at either site
//!
//! Each fact below is written by this crate's emitter and READ BACK, by path, by something outside
//! it: `config.rs` derives `--extern-import`, `--extern-wrapper-index`, `--wrapper-requests`,
//! `--key-requests` and the cargo path dependencies from exactly these spellings, and the
//! committed-state verdict opens two of the files by name. Spelled independently at both ends, a
//! renamed sidecar produces no compile error and no failing unit — it produces a config whose derived
//! flag points at a file nobody writes, which surfaces as a missing wrapper in someone else's build.
//! One `const` is what makes that rename a type-checked edit instead of a grep.
//!
//! This module owns only the SHARED facts. Paths the emitter alone spells (the `rust/`/`wasm/`
//! generated trees, the seed-once crate roots, the json-gen crate's `lib.rs`/`main.rs`/`generated/`)
//! stay at their emission sites: a constant is worth minting when a second file would otherwise have
//! to know the string, and those have no second file.
//!
//! The `--package-json` NESTING RULE is deliberately absent, because it is code and not a string:
//! see the LOCKSTEP pair on `config::crate_relative` and `GenerationScope::export`'s `rust_dir`.

/// The dep-side extern-interface export tree, a sibling of `rust/`/`wasm/` under a crate's
/// `--output`. Emitted in every mode (including rust-only), so it is NOT under the `--package-json`
/// nesting; a consumer names `<dep output>/extern-interface/<dep>` in `--extern-import`.
pub(crate) const EXTERN_INTERFACE_DIR: &str = "extern-interface";

/// The rust-side request sidecar a `--workspace-dep` consumer writes and its dependency reads back
/// through `--key-requests`.
pub(crate) const RUST_BORROWED_KEY_TYPES: &str = "rust/src/generated/borrowed_key_types.rs";

/// The wrapper index a crate publishes: every collection wrapper class its wasm crate re-exports.
/// A consumer reads it through `--extern-wrapper-index`, and the committed-state verdict reads it to
/// decide whether the tree builds.
pub(crate) const WASM_COLLECTIONS_INDEX: &str = "wasm/src/generated/collections.rs";

/// The mirror image of [`WASM_COLLECTIONS_INDEX`]: what a consumer borrows, from whom. The
/// dependency reads it back through `--wrapper-requests`, and the committed-state verdict reads it
/// against the index above.
pub(crate) const WASM_BORROWED_COLLECTIONS: &str = "wasm/src/generated/borrowed_collections.rs";

/// The json-gen crate's directory, under the crate root (so `--package-json` moves it one level down
/// with the others). It exists in `wasm = false` runs too — the json-gen crate follows
/// `--json-schema-export`, not the wasm face.
pub(crate) const JSON_GEN_DIR: &str = "wasm/json-gen";

/// The json-gen crate's manifest, which `--json-gen-dep` writes path dependencies into.
pub(crate) const JSON_GEN_MANIFEST: &str = "wasm/json-gen/Cargo.toml";

/// The wasm crate's cargo package name: `--lib-name` plus this. Spelled in the manifest change log
/// (`static/manifest_changes/`) as part of `cddl-lib-wasm`, which the `cddl-lib` → `--lib-name`
/// substitution turns into the real name; a config deriving a `--wasm-dep` has to predict it.
pub(crate) const WASM_PACKAGE_SUFFIX: &str = "-wasm";

/// The json-gen crate's cargo package name suffix, on exactly the terms [`WASM_PACKAGE_SUFFIX`]
/// states for the wasm crate's.
pub(crate) const JSON_GEN_PACKAGE_SUFFIX: &str = "-json-schema-gen";
