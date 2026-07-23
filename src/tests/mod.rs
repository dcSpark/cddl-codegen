//! The app's own test suite (bin-crate only, `#[cfg(test)]` — declared from `main.rs`).
//!
//! Everything under `src/tests/` tests cddl-codegen itself; everything else in `src/` is
//! production code (including `emit_tests.rs`/`emit_tests_wasm.rs`, which are the shipped
//! `--emit-tests` feature that generates tests INTO output crates). The suites live in the bin
//! crate — not `lib.rs`, not cargo `tests/` targets — because they use `#[cfg(test)]`-gated
//! library API (`api::generated_strings`, `api::ir_structs_debug`) and `pub(crate)` internals
//! that external test targets cannot see. See `tests/README.md` for what each suite covers.
//!
//! Module names are load-bearing: CI and documented commands select tests by substring
//! (`cargo insta test -- snapshot_tests robustness`, `cargo test --bin cddl-codegen <name>`), so
//! the paths must keep containing `snapshot_tests`, `robustness`, and the documented fn names.

pub(crate) mod any_cbor_tests;
pub(crate) mod any_choice_tests;
pub(crate) mod dsl_position_tests;
pub(crate) mod extern_import_tests;
pub(crate) mod gate_cache;
pub(crate) mod generic_collection_tests;
pub(crate) mod identifier_hazard_tests;
pub(crate) mod integration_tests;
pub(crate) mod optional_tag_set_tests;
pub(crate) mod preserve_fixture_tests;
pub(crate) mod recombination_tests;
pub(crate) mod robustness_tests;
pub(crate) mod rust_name_tests;
pub(crate) mod snapshot_tests;
pub(crate) mod wasm_parity_tests;

pub(crate) type Profile = (&'static str, &'static [&'static str]);

/// The flag axes that drive meaningfully different generation paths. (`canonical` is a
/// serialization sub-mode of `preserve` and differs only where maps/sets exist, so it's covered
/// once at whole-program scale rather than duplicated per feature.)
///
/// Shared by `snapshot_tests` and `integration_tests::feature_corpus_compiles` so the snapshot
/// axis and the compile gate can never silently diverge (dropping a profile from one must drop it
/// from both).
pub(crate) const ALL_PROFILES: &[Profile] = &[
    ("default", &[]),
    ("preserve", &["--preserve-encodings=true"]),
    (
        "json",
        &["--json-serde-derives=true", "--json-schema-export=true"],
    ),
];
