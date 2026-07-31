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
//!
//! # The component face's five
//!
//! The `COMPONENT_*` constants qualify on the same rule read one notch wider: "a second FILE must
//! know the string", not specifically "a file outside `generation/`". Each names a second reader —
//! `COMPONENT_DIR` and `COMPONENT_WIT_DIR` are spelled by the WIT projection, the guest emitter and
//! `export.rs`'s write loop; `COMPONENT_WIT_DEPS_DIR` by the dep-WIT materializer and `export.rs`'s
//! header-stamp exemption; `COMPONENT_MANIFEST` by `cargo_manifest.rs`'s changeset and
//! `export.rs`'s manifest merge; `COMPONENT_PACKAGE_SUFFIX` by the manifest change log's
//! `cddl-lib-component` and by the drift gate that holds the two spellings together, which is the
//! same emitter-half-is-a-data-file problem [`WASM_PACKAGE_SUFFIX`] carries (no cross-crate
//! derivation predicts the component package — see the constant).
//! They are minted together rather than one at a time because they are one layout decision, and a
//! half-constant layout is the state in which a rename silently diverges.

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

// The layout is minted whole rather than one constant at a time, because a half-constant layout is
// the state in which a rename silently diverges. The `#[allow(dead_code)]` on the last of the five
// below is what that costs here: see its own comment for why nothing in production reads it.
/// The component crate's directory, a sibling of `rust/` and `wasm/` under a crate's `--output`.
/// Named by three files: `wit.rs` (which keys the emitted WIT map under it), `component.rs` (the
/// guest glue), and `export.rs` (write loop, stale-file scan, header stamping). Spelled once so
/// moving the face is one edit rather than a grep across three emitters.
pub(crate) const COMPONENT_DIR: &str = "component";

/// The generated WIT package's directory. `wit-bindgen::generate!`'s `path` is resolved against
/// `CARGO_MANIFEST_DIR`, so the emitted macro invocation spells the tail (`"wit"`) while `export.rs`
/// spells the full path — a second reader of the same fact, and the reason it is a constant.
///
/// DELETE-AND-RECREATED each run, like `extern-interface/`, rather than covered by the stale-file
/// scan: that scan's collector is `.rs`-only, and delete-and-recreate cannot orphan by construction.
pub(crate) const COMPONENT_WIT_DIR: &str = "component/wit";

/// Where a dependency's committed WIT package is materialized inside this crate's own WIT tree, one
/// subdirectory per dep (`component/wit/deps/<dep>/…`). Named by `component_wit_deps.rs` (which keys
/// the copied files under it) and by `export.rs` (the header-stamp exemption — a copied file keeps
/// the DEP's provenance banner, and stamping ours on top would misattribute it).
///
/// The `deps/` level is REQUIRED, not cosmetic: a `.wit` sitting flat beside `world.wit` is read as
/// part of THIS package and fails to resolve with a package-identity mismatch. The subdirectory and
/// file names below it are free.
///
/// Under [`COMPONENT_WIT_DIR`], so it inherits that tree's delete-and-recreate treatment for free —
/// which is what it wants: a dep dropped from `--component-extern-wit` must not leave a live package
/// declaration behind, and WIT resolves a whole DIRECTORY.
pub(crate) const COMPONENT_WIT_DEPS_DIR: &str = "component/wit/deps";

/// The component crate's manifest, which `--component-dep` writes path dependencies into and which
/// `cargo_manifest::ops_for_component` addresses.
pub(crate) const COMPONENT_MANIFEST: &str = "component/Cargo.toml";

/// The component crate's cargo package name: `--lib-name` plus this, on exactly the terms
/// [`WASM_PACKAGE_SUFFIX`] states for the wasm crate's. Spelled in the manifest change log as part
/// of `cddl-lib-component`, which the `cddl-lib` → `--lib-name` substitution turns into the real
/// name.
///
/// Unlike its two siblings, NO cross-crate derivation predicts it, and the asymmetry is the
/// component face's shape rather than an omission: a consumer's component crate depends on the
/// dependency's RUST package (the guest glue holds a dependency-typed value natively and converts it
/// across the bytes seam), while the dependency's own component crate is wired by the composer at the
/// component level and never by cargo. The wasm face derives both packages because its pass emits
/// `use <dep>_wasm::…` as well; nothing on this face emits the equivalent. So the constant's only
/// reader is the drift gate that asserts it against the template — which is `#[cfg(test)]`, hence the
/// attribute. It stays spelled here because the layout is one decision, and a suffix living only in a
/// `.toml` is one nobody can assert against.
#[allow(dead_code)]
pub(crate) const COMPONENT_PACKAGE_SUFFIX: &str = "-component";
