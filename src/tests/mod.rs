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
pub(crate) mod bounded_map_runtime_tests;
pub(crate) mod component_compose_tests;
pub(crate) mod component_host_tests;
pub(crate) mod component_import_tests;
pub(crate) mod component_jco_tests;
pub(crate) mod component_parity_tests;
pub(crate) mod component_tests;
pub(crate) mod config_tests;
pub(crate) mod declared_spelling_tests;
pub(crate) mod decode_reject_reason_tests;
pub(crate) mod dsl_position_tests;
pub(crate) mod extern_companions_tests;
pub(crate) mod extern_import_tests;
pub(crate) mod gate_cache;
pub(crate) mod generic_collection_tests;
pub(crate) mod identifier_hazard_tests;
pub(crate) mod integration_tests;
pub(crate) mod json_schema_gen_tests;
pub(crate) mod optional_tag_set_tests;
pub(crate) mod ordered_set_runtime_tests;
pub(crate) mod preserve_fixture_tests;
pub(crate) mod recombination_tests;
pub(crate) mod referencing_context_tests;
pub(crate) mod refused_name_closure_tests;
pub(crate) mod regen_over_prior_tests;
pub(crate) mod registration_reference_tests;
pub(crate) mod robustness_tests;
pub(crate) mod rust_name_tests;
pub(crate) mod snapshot_tests;
pub(crate) mod synthesized_name_registry_tests;
pub(crate) mod timing_cells;
pub(crate) mod wasm_parity_tests;
pub(crate) mod wrapper_participation_tests;
pub(crate) mod write_tail_tests;

pub(crate) type Profile = (&'static str, &'static [&'static str]);

/// The flag axes that drive meaningfully different generation paths. (`canonical` is a
/// serialization sub-mode of `preserve` and differs only where maps/sets exist, so it's covered
/// once at whole-program scale rather than duplicated per feature.)
///
/// Shared by `snapshot_tests` and `integration_tests::feature_corpus_compiles` so the snapshot
/// axis and the compile gate can never silently diverge (dropping a profile from one must drop it
/// from both). A consumer that deliberately skips a row does so BY NAME with a stated reason —
/// today only [`COMPONENT_PROFILE`], whose crate targets wasip2 and so is not host-checkable.
pub(crate) const ALL_PROFILES: &[Profile] = &[
    ("default", &[]),
    ("preserve", &["--preserve-encodings=true"]),
    (
        "json",
        &["--json-serde-derives=true", "--json-schema-export=true"],
    ),
    // The wasm COMPONENT face (WIT + wasip2 guest glue), which no other profile reaches at all.
    //
    // `--wasm` is deliberately left at its DEFAULT (`true`) rather than turned off, for four
    // reasons:
    //  1. It is the flag set the repo has already chosen for this face's whole-program snapshots —
    //     `snapshot_tests::WHOLE_PROGRAM_CASES` carries five `component`-labelled rows spelled
    //     exactly `&["--component=true"]`. One posture for both snapshot axes.
    //  2. `--wasm=false` would break the consumers of this const rather than lighten them, and it
    //     would break them EARLIER than "the wasm crate went missing": several sweeps below
    //     hardcode `--wasm=true` before appending a profile's flags, and clap's `ArgAction::Set`
    //     is SET-ONCE, not last-wins — a second `--wasm` is rejected outright with
    //     "the argument '--wasm <WASM>' cannot be used multiple times", so every such cell would
    //     die at argument parsing rather than at a de-gating missing-crate check.
    //     (`component_tests::component_profile_flags` drops this row's own `--component=true` for
    //     exactly the same reason.)
    //  3. It costs no WIT coverage: the emitted `component/wit/**` is byte-identical between the
    //     two wasm postures — asserted on this face's fixtures by
    //     `component_tests::component_wit_is_wasm_posture_independent`, and confirmed at corpus
    //     scale (no fixture differs).
    //  4. Every corpus fixture generates under it.
    //
    // Not every ALL_PROFILES consumer sweeps this row: the ones whose subject is the rust↔wasm
    // boundary, or which compile for the HOST, filter it out by [`COMPONENT_PROFILE`] with their
    // own reason. See that constant.
    ("component", &["--component=true"]),
];

/// The [`ALL_PROFILES`] row whose subject is the wasm COMPONENT face, named once so the consumers
/// that deliberately do NOT sweep it can filter by name rather than by index or count.
///
/// A shared name plus the liveness assertion below is what keeps those filters honest: renaming or
/// deleting the row fails [`component_profile_row_is_live`] loudly instead of turning every filter
/// into a silent no-op that quietly re-widens the sweeps.
pub(crate) const COMPONENT_PROFILE: &str = "component";

/// [`COMPONENT_PROFILE`] must name a live [`ALL_PROFILES`] row — see that constant for why.
#[test]
fn component_profile_row_is_live() {
    assert!(
        ALL_PROFILES
            .iter()
            .any(|(name, _)| *name == COMPONENT_PROFILE),
        "COMPONENT_PROFILE names `{COMPONENT_PROFILE}`, which is not an ALL_PROFILES row — every \
         consumer filtering by that name has silently become a no-op; fix the name or the row"
    );
}
