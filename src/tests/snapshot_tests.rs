//! Fast, in-process golden snapshots of generated output.
//!
//! Unlike the heavy integration tests in `integration_tests.rs` (which shell out to `cargo run`
//! and then compile + round-trip the generated crates), these drive the generator as a library
//! (`crate::api`) and snapshot the post-rustfmt generated source directly. No subprocess, no
//! compilation, no `target/` bloat. They give a localized diff when generation output changes;
//! the integration tests remain the "does it actually compile & round-trip" correctness gate.
//!
//! Four suites:
//! * [`feature_corpus`] — one tiny CDDL file per language construct (in `tests/corpus/`), each
//!   snapshotted under every flag profile plus an IR dump. A one-feature regression yields a
//!   one-file diff. Grouped under `tests/corpus/snapshots/<feature>/`. The generated `Cargo.toml`
//!   and the json-gen `main.rs` are skipped here (near-constant — they're covered by
//!   [`whole_program`], [`cargo_toml_matrix`] and [`serialization_prelude`] instead).
//! * [`whole_program`] — the existing integration inputs (incl. multifile) each under one
//!   known-safe profile, capturing the *full* output (incl. `Cargo.toml`s) to cover cross-feature
//!   interactions, the scope/module path, and the edition/deps logic.
//! * [`cargo_toml_matrix`] — a curated input × profile matrix covering every distinct generated
//!   `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps toggled
//!   independently), which `whole_program` alone does not produce.
//! * [`serialization_prelude`] — the static serialization runtime, once per flag combination.
//!
//! A per-feature `serialization.rs` snapshot is sometimes empty, or just an import block. That's
//! expected: these snapshots capture the *generated-only* code (the static runtime prelude that
//! `export` prepends is excluded — it's the [`serialization_prelude`] suite), and some constructs
//! emit no standalone (de)serialization impl at all (aliases, c-style enums — see
//! `docs/docs/output_format.mdx` for which and why). The root file's lone import block is what that
//! prepended prelude needs.
//!
//! Bless after an intentional change with `INSTA_UPDATE=always cargo test` (or `cargo insta review`).

use super::{ALL_PROFILES, Profile};
use crate::cli::Cli;
use clap::Parser;

/// Build a `Cli` for in-process generation. `--output` is unused (`generated_strings` does no disk
/// I/O) but clap requires it; `--static-dir` defaults to `static/`, read for Cargo.toml/prelude.
fn cli_for(input: &std::path::Path, extra: &[&str]) -> Cli {
    let input = input.to_str().unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input,
        "--output",
        "snapshot_unused",
    ];
    args.extend_from_slice(extra);
    Cli::parse_from(args)
}

/// Near-constant generated files skipped by the per-feature corpus (they don't vary by construct).
fn is_per_feature_noise(path: &str) -> bool {
    path.ends_with("Cargo.toml") || path == "wasm/json-gen/src/main.rs"
}

/// `(corpus fixture stem, profile)` whose GENERATION deliberately aborts under that profile, so it
/// has no snapshot to pin there (the fixture is still snapshotted under its other profiles). Mirrors
/// `integration_tests::feature_corpus_compiles`'s `EXPECTED_GENERATION_FAIL`; a listed stem absent
/// from `tests/corpus` fails as a stale pin (checked in [`feature_corpus`]).
const PROFILE_GENERATION_SKIP: &[(&str, &str)] = &[
    (
        // An optional fixed FLOAT member aborts generation under --preserve-encodings at the float
        // deserialize stub ("preserve_encodings is not implemented for float" — the
        // preserve_encodings_supports_floats stub class); default/json snapshot the presence field.
        "optional_fixed_float",
        "preserve",
    ),
    (
        // An `@ignore` open struct-map is rejected under --preserve-encodings (a preserve crate's
        // byte-exact round-trip contract cannot hold for a type that drops unknown entries); default/
        // json snapshot the closed-struct surface.
        "dsl_ignore",
        "preserve",
    ),
];

/// Snapshot the generated source for `input` under each profile (grouped under
/// `tests/corpus/snapshots/<label>/`). `full` keeps every generated file; otherwise the
/// near-constant manifest/main files are skipped. `with_ir` adds one IR dump.
fn snapshot_input(
    input: &std::path::Path,
    label: &str,
    profiles: &[Profile],
    full: bool,
    with_ir: bool,
) {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots")
        .join(label);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        for (profile, extra) in profiles {
            let cli = cli_for(input, extra);
            let files = crate::api::generated_strings(&cli)
                .unwrap_or_else(|e| panic!("generation failed for {}/{}: {}", label, profile, e));
            assert!(
                !files.is_empty(),
                "no generated files for {}/{}",
                label,
                profile
            );
            for (path, content) in files {
                if !full && is_per_feature_noise(&path) {
                    continue;
                }
                let name = format!("{}__{}", profile, path.replace('/', "__"));
                insta::assert_snapshot!(name, content);
            }
        }
        if with_ir {
            // IR is essentially profile-independent, so snapshot it once under default. Localizes
            // a regression to parsing/IR vs generation.
            let ir = crate::api::ir_structs_debug(&cli_for(input, &[]))
                .unwrap_or_else(|e| panic!("IR build failed for {}: {}", label, e));
            insta::assert_snapshot!("ir", ir);
        }
    });
}

/// The existing integration inputs, each under the flag profile it is known-safe with (the same
/// pairings the heavy integration tests use). Captures the full output to cover cross-feature
/// interactions, the multifile scope/module path, and the edition/deps Cargo.toml logic.
///
/// A corpus file with one of these labels as its stem would clobber its snapshot dir, so
/// [`feature_corpus`] guards against the collision.
const WHOLE_PROGRAM_CASES: &[(&str, &str, Profile)] = &[
    ("core", "tests/core/input.cddl", ("default", &[])),
    (
        "preserve_encodings",
        "tests/preserve-encodings/input.cddl",
        ("preserve", &["--preserve-encodings=true"]),
    ),
    (
        "canonical",
        "tests/canonical/input.cddl",
        (
            "canonical",
            &["--preserve-encodings=true", "--canonical-form=true"],
        ),
    ),
    (
        "json",
        "tests/json/input.cddl",
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ),
    // float JSON emission — split from `json` (that fixture also runs under json_preserve,
    // and preserve-encodings is unimplemented for floats; same reason floats can't be corpus
    // entries, whose snapshots span all three profiles).
    (
        "json_float",
        "tests/json-float/input.cddl",
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ),
    // loose-CBOR `any` positions (member / array element / table domain+range / top-level alias /
    // tagged / last-position choice arm), lowered to the AnyCbor runtime type. All legs pass
    // --wasm=false to isolate the rust and json surfaces here (the wasm AnyCbor surface is pinned
    // by the wasm-parity suite), which is why this is a profile-limited whole_program input rather
    // than an all-profile feature-corpus entry. Pinned under the three rust modes (canonical merge +
    // preserve encoding-field threading) plus the json mode (the serde/schemars surface: the AnyCbor
    // fragments concatenate into the any_cbor module under the JSON flags).
    (
        "any_positions_default",
        "tests/any-positions/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "any_positions_preserve",
        "tests/any-positions/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    (
        "any_positions_canonical",
        "tests/any-positions/input.cddl",
        (
            "canonical",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--wasm=false",
            ],
        ),
    ),
    (
        "any_positions_json",
        "tests/any-positions/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // preserve + json together — the only profile that pins the OrderedHashMap natural-JSON adapters
    // (`natural_any_cbor_orderedmap` / `_opt_orderedmap`) for a preserve `{* K => any}` member; the
    // plain `json` profile above is non-preserve (BTreeMap adapters).
    (
        "any_positions_preserve_json",
        "tests/any-positions/input.cddl",
        (
            "preserve_json",
            &[
                "--preserve-encodings=true",
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // loose-CBOR open struct-maps (a trailing `* k => v` rest row after fixed keys → a `pub rest`
    // capture map). Snapshotted under the plain (`default`), preserve, AND json flavors: the preserve
    // fidelity core (orig_deser_order interleave, per-entry encoding sidecars, the runtime canonical
    // key merge) under `preserve`; the flattened-JSON surface (the rest field's
    // `#[serde(flatten)]` + per-struct serialize_with/deserialize_with helpers + additionalProperties
    // schema) under `json`. These profiles pass `--wasm=false` to isolate the rust/json surfaces (the
    // wasm rest accessor is pinned by the separate `open_struct_map_wasm` profile below) — a
    // profile-limited whole_program input rather than an all-profile feature-corpus entry.
    (
        "open_struct_map_default",
        "tests/open-struct-map/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "open_struct_map_preserve",
        "tests/open-struct-map/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    (
        "open_struct_map_json",
        "tests/open-struct-map/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // The IGNORE flavor (`@ignore` on the rest row): unknown entries are typed-deserialized and
    // DROPPED — no `rest` field, serialize emits declared members only, and JSON/schemars/wasm are a
    // closed struct's (none of capture's flatten/getter/sidecar machinery). Snapshotted non-preserve
    // with `--wasm=false` (`@ignore` is rejected under --preserve-encodings). The generated source
    // pins that these open structs emit as CLOSED structs while the deserialize loop stays
    // dynamic-length (so a definite map with extra entries decodes) and consumes+drops each unknown
    // uint/text/any entry.
    (
        "open_struct_map_ignore",
        "tests/open-struct-map-ignore/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    // The wasm rest surface: each open struct's wasm wrapper gains a `rest` getter
    // returning the captured entries as the minted map wrapper (`MapKToV` / the `@duplicates
    // preserve` PairMap-backed twin). `--wasm` is the default, so this profile pins the emitted wasm
    // getters + minted rest-map wrappers.
    (
        "open_struct_map_wasm",
        "tests/open-struct-map/input.cddl",
        ("wasm", &[]),
    ),
    // loose-CBOR open ARRAYS (a final-position `* t` rest tail after ≥1 fixed member → a `pub rest:
    // Vec<T>` capture, or a dropped `@ignore` tail). Snapshotted under `default` and `json` with
    // `--wasm=false` to isolate the rust/json surfaces. Byte-exact per-element tail encodings under
    // --preserve-encodings are a later work package (so no preserve profile). Covers a typed tail, an
    // `any` tail, a `@name`d tail, an `@ignore` tail, and the degenerate shape combos (all-mandatory
    // prefix + `@ignore` — the empty-conditional `definite_info` path — single fixed member + tail,
    // and an optional fixed-value member + a type-distinct tail).
    (
        "open_array_default",
        "tests/open-array/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "open_array_json",
        "tests/open-array/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // directory input — exercises the multi-file scope/module codegen path.
    ("multifile", "tests/multifile/inputs", ("default", &[])),
    // extern (`_CDDL_CODEGEN_EXTERN_TYPE_`) and raw-bytes (`_CDDL_CODEGEN_RAW_BYTES_TYPE_`)
    // emit paths: they reference user-supplied types so their output can't compile standalone
    // (behavioral coverage is their integration fixtures), but this suite never compiles —
    // it's where their emitted source gets pinned. Same profile pairings the integration
    // tests use.
    (
        "extern_deps",
        "tests/extern-deps/inputs",
        (
            "preserve",
            &[
                "--preserve-encodings=true",
                "--common-import-override=extern_dep_crate",
            ],
        ),
    ),
    // extern-dep types across the WASM boundary (list element / table value) against a split
    // rust/wasm dependency, via `--extern-wasm-crate`. Pins the mapped wasm import + the dep's-rust
    // inner-storage path that the `extern_deps_wasm` integration test then compiles.
    (
        "extern_deps_wasm",
        "tests/extern-deps-wasm/inputs",
        (
            "preserve",
            &[
                "--preserve-encodings=true",
                "--common-import-override=extern_dep_crate",
                "--extern-wasm-crate=extern_dep_crate=extern_dep_crate_wasm",
            ],
        ),
    ),
    ("raw_bytes", "tests/raw-bytes/input.cddl", ("default", &[])),
];

/// One tiny CDDL file per language construct → a localized snapshot per feature, across every
/// flag profile.
#[test]
fn feature_corpus() {
    let corpus_dir = std::path::Path::new("tests/corpus");
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(
        !entries.is_empty(),
        "no corpus files found in {:?}",
        corpus_dir
    );
    let corpus_stems: std::collections::BTreeSet<String> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap().to_owned())
        .collect();
    for (stem, _profile) in PROFILE_GENERATION_SKIP {
        assert!(
            corpus_stems.contains(*stem),
            "PROFILE_GENERATION_SKIP names corpus fixture `{stem}` that no longer exists in \
             tests/corpus — stale pin, remove or fix it"
        );
    }
    for path in entries {
        let label = path.file_stem().unwrap().to_str().unwrap().to_owned();
        assert!(
            !WHOLE_PROGRAM_CASES
                .iter()
                .any(|(whole_program_label, _, _)| *whole_program_label == label),
            "corpus file {:?} collides with a whole_program snapshot dir; rename it",
            path
        );
        // A fixture whose generation aborts under some profile (float + preserve) is snapshotted
        // only under the profiles it generates in.
        let profiles: Vec<Profile> = ALL_PROFILES
            .iter()
            .filter(|(profile, _)| !PROFILE_GENERATION_SKIP.contains(&(label.as_str(), *profile)))
            .copied()
            .collect();
        snapshot_input(&path, &label, &profiles, false, true);
    }
}

/// Generation must be deterministic: same input + flags → byte-identical output every run.
/// Emission is all-`BTreeMap`/`BTreeSet` (never `HashMap`) precisely so output can't depend on
/// hash iteration order. This guards against a future `HashMap` silently reintroducing
/// nondeterminism — which would otherwise surface only as intermittently flaky snapshots. Two
/// runs in one process suffice: each `HashMap` instance gets a fresh random seed, so a hash-order
/// dependency would diverge between the runs. Rich whole-program inputs across the three emission
/// profiles (default/preserve/json) exercise the flag-gated emission paths too.
#[test]
fn generation_is_deterministic() {
    let cases: &[(&str, &[&str])] = &[
        ("tests/core/input.cddl", &[]),
        (
            "tests/preserve-encodings/input.cddl",
            &["--preserve-encodings=true"],
        ),
        (
            "tests/json/input.cddl",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
        // directory input: multi-file enumeration + scope/module emission. The in-process double-run
        // can't vary filesystem enumeration order, so order-independence itself is guaranteed by
        // construction (cddl_paths sorts); this case guards the rest of the multi-file pipeline.
        ("tests/multifile/inputs", &[]),
    ];
    for (input, extra) in cases {
        let cli = cli_for(std::path::Path::new(input), extra);
        let first = crate::api::generated_strings(&cli).unwrap();
        let second = crate::api::generated_strings(&cli).unwrap();
        assert_eq!(
            first, second,
            "nondeterministic generation for {input} — a HashMap on the IR/emission path?"
        );
    }
}

/// The existing integration inputs, each under the flag profile it is known-safe with (the same
/// pairings the heavy integration tests use). Captures the full output to cover cross-feature
/// interactions, the multifile scope/module path, and the edition/deps Cargo.toml logic.
#[test]
fn whole_program() {
    for (label, input, profile) in WHOLE_PROGRAM_CASES {
        snapshot_input(
            &std::path::PathBuf::from(input),
            label,
            std::slice::from_ref(profile),
            true,
            false,
        );
    }
}

/// Content pin for the json-gen extern schema-row fix (feature request 05). Drives the generator
/// in-process (no compile) over `tests/json-extern-rows/inputs` — a directory fixture combining a
/// plain extern, a generic extern + concrete instance, a dep-owned (extern-deps-dir) extern, and an
/// in-crate SCOPED type — and pins exactly which `gen_json_schema!` rows the json-gen crate emits.
/// The full compile proof (that the KEPT rows build against hand-written `schemars::JsonSchema`
/// impls, and the generic-base row's removal fixes an E0107) lives in
/// `integration_tests::json_extern` and `integration_tests::multifile_json_preserve`; this is the
/// cheap fast-tier net for the emitter's row selection.
#[test]
fn json_gen_extern_schema_rows() {
    let cli = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &["--json-serde-derives=true", "--json-schema-export=true"],
    );
    let files = crate::api::generated_strings(&cli).expect("generation must succeed");
    let mod_rs = files
        .get("wasm/json-gen/src/generated/mod.rs")
        .expect("json-gen generated/mod.rs must be emitted under --json-schema-export");

    // KEPT rows: plain extern, concrete generic instance, in-crate root type, and the scoped
    // in-crate type at its REAL module path (the thin root's `pub use generated::*` makes it valid).
    for kept in [
        "gen_json_schema!(cddl_lib::MyExtern);",
        "gen_json_schema!(cddl_lib::MySet);",
        "gen_json_schema!(cddl_lib::BigThing);",
        "gen_json_schema!(cddl_lib::sub::module::ScopedThing);",
    ] {
        assert!(
            mod_rs.contains(kept),
            "expected json-gen row `{kept}` missing:\n{mod_rs}"
        );
    }

    // SKIPPED: the generic-extern BASE (`ExtSet` names no concrete type — E0107 no matter what the
    // user writes).
    assert!(
        !mod_rs.contains("gen_json_schema!(cddl_lib::ExtSet)"),
        "generic-extern base row must be skipped:\n{mod_rs}"
    );

    // SKIPPED: a generic-extern base with ZERO instances (`ext_unused<T>`). `generic_instance_bases`
    // derives bases from instances and is blind to this one, so the skip must key on the parse-time
    // `generic_extern_bases` record.
    assert!(
        !mod_rs.contains("ExtUnused"),
        "a never-instantiated generic-extern base must still be skipped:\n{mod_rs}"
    );

    // SKIPPED: dep-owned types (non-export scope). Their emitted path would be `dep_crate::…`, which
    // this json-gen crate's manifest does not (and must not) depend on — the dep's own json-gen run
    // owns those schemas.
    assert!(
        !mod_rs.contains("dep_crate"),
        "dep-owned (non-export scope) rows must be skipped:\n{mod_rs}"
    );
}

/// Regression pin for feature request 07 (commit `08bc1d9` "scope_references walks type_aliases"):
/// a generic-EXTERN instance named by a rule in a NON-root scope (`required_signers = ext_set<pub_key>`
/// / `my_set = ext_set<plain>` in the `transaction` scope) registers a type alias whose base is a
/// `Base<Args>` TYPE EXPRESSION (`ExtSetRawBytes<PubKey>` / `ExtSet<Plain>`). Before the fix the
/// alias walk fed that opaque ident through `set_ref`, landing the whole `<…>`-carrying text verbatim
/// in the scope's `use crate::generated::{…}` list — invalid Rust, so the rustfmt post-pass aborted
/// generation. This pins the corrected shape directly (fast, no nested cargo); the compile proof is
/// `integration_tests::extern_generic_scoped`.
///
/// Not keyed on `@raw_bytes_flavor`: the plain instance (`my_set = ext_set<plain>`) breaks
/// identically, so both the flavored (`ExtSetRawBytes<PubKey>`) and plain (`ExtSet<Plain>`) shapes
/// are asserted. The base extern lives in the NON-root `crypto` scope, so this also pins that the
/// base import routes to the base's declaring scope (the re-export glue's `pub use crate::…` site),
/// not the default-to-root `set_ref` would misroute to.
#[test]
fn extern_generic_scoped_alias_imports() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-generic-scoped/inputs"),
        &["--wasm=false"],
    );
    let files = crate::api::generated_strings(&cli).expect("generation must succeed");
    let tx = files
        .get("rust/src/generated/transaction/mod.rs")
        .expect("transaction scope module must be emitted");

    // The alias lines render the `Base<Args>` type expression bare in the transaction module.
    for alias in [
        "pub type RequiredSigners = ExtSetRawBytes<PubKey>;",
        "pub type MySet = ExtSet<Plain>;",
    ] {
        assert!(
            tx.contains(alias),
            "expected alias line `{alias}` missing:\n{tx}"
        );
    }

    // Both alias bases and both argument types must be imported — decomposed from the opaque
    // `Base<Args>` ident, never emitted whole. Assertions are robust to brace-grouping: they check
    // that SOME `use` line carries the right path prefix and names the ident.
    let use_lines: Vec<&str> = tx
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("use "))
        .collect();
    let imported_from = |prefix: &str, ident: &str| {
        use_lines
            .iter()
            .any(|l| l.starts_with(prefix) && l.contains(ident))
    };
    // The base externs (`ExtSet`/`ExtSetRawBytes`) and the raw-bytes arg (`PubKey`) all live in the
    // non-root `crypto` scope — imported from there, not root.
    for ident in ["ExtSet", "ExtSetRawBytes", "PubKey"] {
        assert!(
            imported_from("use crate::generated::crypto::", ident),
            "`{ident}` must be imported from the crypto scope:\n{tx}"
        );
    }
    // The plain arg (`plain`) is a ROOT-scope record referenced ONLY as a generic argument — its
    // import is the args-import half of the fix (dropping it would dangle E0412). Root path is
    // `crate::generated::` with the ident/`{` immediately after (never the `crypto::` submodule).
    assert!(
        use_lines.iter().any(|l| {
            l.starts_with("use crate::generated::")
                && !l.starts_with("use crate::generated::crypto::")
                && l.contains("Plain")
        }),
        "`Plain` must be imported from the root scope:\n{tx}"
    );

    // Class-level invariant: a decomposed importer NEVER lets a `<…>` type expression reach a `use`
    // line — in the transaction scope or the root generated module.
    let root = files
        .get("rust/src/generated/mod.rs")
        .expect("root generated module must be emitted");
    for (name, content) in [("transaction/mod.rs", tx), ("generated/mod.rs", root)] {
        for line in content
            .lines()
            .map(str::trim)
            .filter(|l| l.starts_with("use "))
        {
            assert!(
                !line.contains('<'),
                "a `use` line in {name} carries a `<…>` type expression (invalid Rust):\n{line}"
            );
        }
    }
}

/// The static serialization runtime prelude ships verbatim into every generated crate but is
/// assembled differently per flag combination. It's excluded from the per-feature snapshots (it's
/// feature-independent and would be pure repeated noise), so snapshot it once per combination here
/// — this is the fast net for changes to the `static/serialization*.rs` runtime.
#[test]
fn serialization_prelude() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_serialization_prelude");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    // serialization_prelude only reads --static-dir; the input is irrelevant but clap needs one.
    let dummy = std::path::Path::new("tests/corpus/primitives.cddl");
    settings.bind(|| {
        for (name, extra) in [
            ("default", &[][..]),
            ("preserve", &["--preserve-encodings=true"][..]),
            (
                "canonical",
                &["--preserve-encodings=true", "--canonical-form=true"][..],
            ),
        ] {
            let cli = cli_for(dummy, extra);
            let prelude = crate::generation::GenerationScope::serialization_prelude(false, &cli)
                .unwrap_or_else(|e| panic!("prelude failed for {}: {}", name, e));
            insta::assert_snapshot!(name, prelude);
        }
    });
}

/// The generated rust `Cargo.toml`'s dependency set is driven by conditional logic: the edition,
/// flag-deps (serde/schemars/linked-hash-map/derivative), and *type*-conditional deps — `hex` for
/// byte wrappers, `wasm-bindgen` for c-style enums. The per-feature corpus skips `Cargo.toml` as
/// near-constant noise, but the distinct dep *combinations* are not all produced by
/// [`whole_program`]. So cover them here with a curated matrix that toggles each type-conditional
/// dep independently × each profile — the fast net for the deps/edition logic.
#[test]
fn cargo_toml_matrix() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_cargo_toml");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    // inputs chosen so `hex` and `wasm-bindgen` toggle independently (core, which has both, is
    // covered by whole_program):
    let inputs = [
        ("base", "tests/corpus/map_struct.cddl"),   // neither
        ("hex", "tests/corpus/bounded_bytes.cddl"), // byte wrapper -> hex
        ("wasm_bindgen", "tests/corpus/c_style_enum.cddl"), // c-style enum -> wasm-bindgen
    ];
    settings.bind(|| {
        for (label, input) in inputs {
            for (profile, extra) in ALL_PROFILES {
                let cli = cli_for(std::path::Path::new(input), extra);
                let files = crate::api::generated_strings(&cli).unwrap_or_else(|e| {
                    panic!("generation failed for {}/{}: {}", label, profile, e)
                });
                let toml = files
                    .get("rust/Cargo.toml")
                    .unwrap_or_else(|| panic!("no rust/Cargo.toml for {}/{}", label, profile));

                // Each conditional dep is set-or-REMOVE, never set-or-skip: assert every one is
                // present exactly when its flag/type condition holds and ABSENT otherwise. A
                // snapshot alone only pins the bytes; this makes the absence contract a first-class
                // assertion (so a regression that stops emitting the `Remove` op — stranding a
                // stale dep on a flag flip — fails here, not just silently in a diff).
                let has = |key: &str| {
                    toml.lines()
                        .any(|l| l.trim_start().starts_with(&format!("{key} =")))
                };
                let preserve = extra.contains(&"--preserve-encodings=true");
                let serde = extra.contains(&"--json-serde-derives=true");
                let schema = extra.contains(&"--json-schema-export=true");
                // matrix inputs isolate the type-conditional deps to their labels; wasm defaults on.
                let hex = label == "hex";
                let wasm_bindgen = label == "wasm_bindgen";
                for (key, expected) in [
                    ("linked-hash-map", preserve),
                    ("derivative", preserve),
                    ("serde", serde),
                    ("serde_json", serde),
                    ("schemars", schema),
                    ("hex", hex),
                    ("wasm-bindgen", wasm_bindgen),
                ] {
                    assert_eq!(
                        has(key),
                        expected,
                        "conditional dep `{key}` presence wrong for {label}/{profile} \
                         (expected present={expected})\n{toml}"
                    );
                }
                // cbor_event is unconditional; sanity-check the presence helper isn't vacuous.
                assert!(has("cbor_event"), "cbor_event must always be present");

                insta::assert_snapshot!(format!("{}__{}", label, profile), toml);
            }
        }
    });
}

/// Drift gate for the derived `static/Cargo_{rust,wasm,json_gen,static_runtime}.toml` templates.
///
/// The templates are generated snapshots of each manifest's append-only change log
/// (`static/manifest_changes/*.toml`), rendered by the SAME fold+apply the runtime uses — so the
/// committed template is guaranteed to mean exactly what the log means. This pins them byte-for-byte:
/// append a log entry (or edit the header) and forget to regenerate, and this fails with the bless
/// command. `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift` rewrites them.
#[test]
fn manifest_template_drift() {
    let static_dir = std::env::current_dir().unwrap().join("static");
    let bless = std::env::var("BLESS_MANIFEST_TEMPLATES")
        .map(|v| v == "1")
        .unwrap_or(false);
    let cases = [
        ("manifest_changes/rust.toml", "Cargo_rust.toml"),
        ("manifest_changes/wasm.toml", "Cargo_wasm.toml"),
        ("manifest_changes/json_gen.toml", "Cargo_json_gen.toml"),
        (
            "manifest_changes/static_runtime.toml",
            "Cargo_static_runtime.toml",
        ),
    ];
    let mut stale = Vec::new();
    for (log_name, template_name) in cases {
        let raw = std::fs::read_to_string(static_dir.join(log_name)).unwrap();
        let rendered =
            crate::cargo_manifest::render_derived_template(&raw, log_name, template_name).unwrap();
        let template_path = static_dir.join(template_name);
        let committed = std::fs::read_to_string(&template_path).unwrap_or_default();
        if rendered != committed {
            if bless {
                std::fs::write(&template_path, &rendered).unwrap();
            } else {
                stale.push(template_name);
            }
        }
    }
    assert!(
        stale.is_empty(),
        "derived manifest template(s) {stale:?} are stale vs their change logs; regenerate with \
         `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift`"
    );
}

/// The `--wasm-list-macro` flag: each `Vec<T>`-backed list wrapper collapses to a single
/// `impl_wasm_list!(rust_elem, wasm_elem, WasmName, needs_into, is_copy)` invocation (plus the `use`
/// import) in place of the inline struct + accessor + conversion block. The fixture covers every
/// reachable `(needs_into, is_copy)` combination. The flag-*off* output is the inline form the rest
/// of the suite already snapshots (and `whole_program`/`generation_is_deterministic` guard that the
/// gated branch leaves it byte-identical), so this only needs the flag-on wasm `generated/mod.rs`.
#[test]
fn wasm_list_macro() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_wasm_list_macro");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    let input = std::path::Path::new("tests/wasm-list-macro/input.cddl");
    // Generic macro paths — the feature works with any user-supplied macro.
    let cases: &[(&str, &[&str])] = &[
        // list wrappers collapse to impl_wasm_list! invocations
        (
            "list_macro",
            &["--wasm-list-macro=my_crate::impl_wasm_list"],
        ),
        // combined with --wasm-conversions-macro: list wrappers use impl_wasm_list! (which emits
        // their own conversions), while non-list wrappers still use impl_wasm_conversions! — i.e.
        // the list macro supersedes, rather than double-emits with, the conversions macro.
        (
            "list_and_conversions_macro",
            &[
                "--wasm-list-macro=my_crate::impl_wasm_list",
                "--wasm-conversions-macro=my_crate::impl_wasm_conversions",
            ],
        ),
    ];
    settings.bind(|| {
        for (label, extra) in cases {
            let cli = cli_for(input, extra);
            let files = crate::api::generated_strings(&cli).unwrap();
            // The wasm macro invocations live in the generated root scope (`generated/mod.rs`), not
            // the thin seed-once `lib.rs` (which carries only `mod generated; pub use generated::*;`).
            let lib = files
                .get("wasm/src/generated/mod.rs")
                .expect("no wasm/src/generated/mod.rs generated");
            insta::assert_snapshot!(format!("{label}__wasm__src__generated__mod.rs"), lib);
        }
    });
}

/// The two banner lines, derived from the SAME constant the stamper prepends
/// (`generation::CODEGEN_HEADER`) so the gate can't drift from what generation actually emits.
/// Likewise the path family below reuses `generation::is_header_stamped_path` — the stamping is
/// file-level in `generated_files`, and this gate asserts over the identical file set.
fn codegen_header_lines() -> (&'static str, &'static str) {
    let mut lines = crate::generation::CODEGEN_HEADER.lines();
    (lines.next().unwrap(), lines.next().unwrap())
}
use crate::generation::is_header_stamped_path;

/// Intermediate parent-module link files (e.g. `a/c/mod.rs` containing only `pub mod foo;`) may or
/// may not carry the header depending on how they are produced; a file consisting SOLELY of
/// `pub mod` declarations has no orderable content for this gate to judge, so exclude it
/// structurally: every non-blank line is a `pub mod ...;`. This can never exclude a file where the
/// ordering bug could hide, because any file that contains the header (or any other item) fails
/// the all-`pub mod` predicate and is checked.
fn is_module_link_stub(content: &str) -> bool {
    let mut non_blank = content.lines().filter(|l| !l.trim().is_empty()).peekable();
    non_blank.peek().is_some() && non_blank.all(|l| l.trim_start().starts_with("pub mod "))
}

/// Verify the two header lines are present and contiguous, and that everything before them is
/// benign preamble: blank lines, `//` comments, or crate-level `#![...]` attributes (which rustfmt
/// splits across multiple lines, e.g. `#![allow(\n    clippy::…,\n)]`, so an opened attribute is
/// consumed through its `]` closer). Any other content before the header (a macro invocation, an
/// item, an import) is the failure this test exists to catch.
fn check_header_leads(content: &str) -> Result<(), String> {
    let (header_1, header_2) = codegen_header_lines();
    let lines: Vec<&str> = content.lines().collect();
    let header_idx = lines
        .iter()
        .position(|l| l.trim() == header_1)
        .ok_or_else(|| format!("header line 1 missing entirely ({header_1:?})"))?;
    match lines.get(header_idx + 1) {
        Some(l) if l.trim() == header_2 => {}
        other => {
            return Err(format!(
                "header line 2 ({header_2:?}) does not immediately follow line 1; got {other:?}"
            ));
        }
    }
    let mut i = 0;
    while i < header_idx {
        let line = lines[i];
        let trimmed = line.trim_start();
        if line.trim().is_empty() || trimmed.starts_with("//") {
            i += 1;
        } else if trimmed.starts_with("#![") {
            // consume the (possibly multi-line) crate attribute through its `]` closer
            while i < header_idx && !lines[i].trim_end().ends_with(']') {
                i += 1;
            }
            i += 1;
        } else {
            return Err(format!(
                "line {} precedes the header but is not blank / a `//` comment / a crate `#![…]` \
                 attribute: {line:?}",
                i + 1
            ));
        }
    }
    Ok(())
}

/// Every generated `.rs` in a header-stamped scope family (`rust/src/generated/**`,
/// `wasm/src/generated/**`) must LEAD with the code-generation header — only blank lines, `//`
/// comments, or crate `#![…]` attributes may precede it. This guards the invariant that the
/// codegen provenance banner is the first substantive line a reader (or a downstream license/attrib
/// scanner) sees.
///
/// The header is stamped once per emitted FILE (`generation::stamp_codegen_header`), not per scope,
/// so it leads regardless of what a scope contributes. Two failure classes this guards against, both
/// of which a scope-level header raw would reintroduce (raws hoist to the top of a scope in insertion
/// order):
/// 1. `--wasm-list-macro`: each `impl_wasm_list!` invocation defines a wrapper type and is emitted
///    via `Scope::raw_sorted` so it sorts into its item position, not hoisted above the header.
/// 2. Merged root `generated/mod.rs` files: the module-linking declarations (`pub mod error;`,
///    `extern crate derivative;`, `pub mod <child>;`) come from the lib scope, which
///    `merge_scopes_to_strings` prepends ahead of the ROOT-scope content — file-level stamping puts
///    the header above all of it.
#[test]
fn generated_files_start_with_header() {
    // Representative (label, input, flags) set — reuses the existing whole-program table and the
    // wasm-list-macro fixture + both its profiles (the known-red case).
    let mut cases: Vec<(String, String, Vec<&str>)> = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        cases.push((
            format!("{label}/{profile}"),
            input.to_string(),
            extra.to_vec(),
        ));
    }
    for (label, extra) in [
        (
            "list_macro",
            &["--wasm-list-macro=my_crate::impl_wasm_list"][..],
        ),
        (
            "list_and_conversions_macro",
            &[
                "--wasm-list-macro=my_crate::impl_wasm_list",
                "--wasm-conversions-macro=my_crate::impl_wasm_conversions",
            ][..],
        ),
    ] {
        cases.push((
            format!("wasm_list_macro/{label}"),
            "tests/wasm-list-macro/input.cddl".to_owned(),
            extra.to_vec(),
        ));
    }

    let mut failures = Vec::new();
    for (profile, input, extra) in &cases {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {profile}: {e}"));
        for (path, content) in &files {
            if !is_header_stamped_path(path) || is_module_link_stub(content) {
                continue;
            }
            if let Err(msg) = check_header_leads(content) {
                failures.push(format!("[{profile}] {path}: {msg}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) in a header-stamped scope family do not lead with the codegen header:\n{}",
        failures.join("\n")
    );
}

/// A generated error-conversion chain converts a single CBOR read to `DeserializeError` AT MOST
/// ONCE. cbor_event's readers return their own error type; a deserialize path maps it to our
/// `DeserializeError` (`.map_err(Into::<DeserializeError>::into)`) so a downstream `.and_then`
/// closure can return `DeserializeFailure::…into()` with a consistent `E`. Because the conversion is
/// the identity `impl From<T> for T` once `E` is already `DeserializeError`, a second `.map_err` on
/// the same read is a redundant no-op — correct output, but dead code that a reader must puzzle over.
///
/// The failure class this guards: an emission site prepending `convert_err_to_ours` before its
/// `.and_then` WITHOUT checking whether an earlier stage of the same chain (the site's own
/// `error_convert`, or a width guard) already converted the error type. `width_reject` shows the
/// correct pattern — it threads a `converted` flag and emits the conversion only when nothing
/// upstream did; every conversion site owes the same check.
#[test]
fn deserialize_converts_error_at_most_once() {
    // Reuse the whole-program representative set (same table the header gate walks).
    const DOUBLED: &str =
        ".map_err(Into::<DeserializeError>::into).map_err(Into::<DeserializeError>::into)";
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            // rustfmt splits the chain across lines; compare with all whitespace stripped.
            let flat: String = content.chars().filter(|c| !c.is_whitespace()).collect();
            if flat.contains(DOUBLED) {
                failures.push(format!("[{label}/{profile}] {path}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) convert a read's error to DeserializeError more than once (redundant \
         .map_err doubling):\n{}",
        failures.join("\n")
    );
}

/// A generated `Ok` pattern parenthesizes its payload only when it is a real tuple, matching the
/// `final_expr` shaping on the expression side (0 exprs → `Ok(())`, 1 → `Ok(x)`, N → `Ok((a, b))`).
/// `Ok((x))` with a single bare identifier is redundant — parens in a pattern are grouping, not a
/// 1-tuple, so it matches the same values as `Ok(x)` but reads as if the payload were a tuple.
///
/// The failure class this guards: an emission site hardcoding the tuple wrapper in the match
/// pattern (`Ok(({}))` around a joined expr list) without conditioning on the list's length. The
/// non-value enum dispatch site (src/generation/enums.rs, `names_without_outer.len() > 1` around line
/// 8879) shows the correct pattern — wrap only for >1 names, empty case emitted separately as
/// `Ok(())`; every pattern-emitting site owes the same length check.
#[test]
fn ok_pattern_parenthesizes_only_tuples() {
    // Hand-rolled scan (no regex dep): find `Ok((`, then a nonempty run of identifier chars
    // immediately followed by `))` — i.e. `Ok((ident))`. `Ok(())` (empty) and `Ok((a, b))`
    // (comma before the close) don't match.
    fn find_redundant_ok_parens(flat: &str) -> Option<&str> {
        const NEEDLE: &str = "Ok((";
        let bytes = flat.as_bytes();
        let mut from = 0;
        while let Some(pos) = flat[from..].find(NEEDLE).map(|i| from + i) {
            let inner = pos + NEEDLE.len();
            let ident_len = bytes[inner..]
                .iter()
                .take_while(|c| c.is_ascii_alphanumeric() || **c == b'_')
                .count();
            if ident_len > 0 && flat[inner + ident_len..].starts_with("))") {
                return Some(&flat[pos..inner + ident_len + 2]);
            }
            from = inner;
        }
        None
    }
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            // rustfmt may split across lines; compare with all whitespace stripped.
            let flat: String = content.chars().filter(|c| !c.is_whitespace()).collect();
            if let Some(found) = find_redundant_ok_parens(&flat) {
                failures.push(format!("[{label}/{profile}] {path}: {found}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) wrap a single-identifier Ok pattern in redundant tuple parens:\n{}",
        failures.join("\n")
    );
}

/// Text arrays cross the wasm boundary as bare `Vec<String>` — wasm-bindgen supports
/// `Vec<String>` in both parameter and return position (a JS string array), and strings are
/// COPIED at the boundary, so the by-value ownership hazard that justifies struct `*List`
/// wrappers does not apply. Therefore no anonymous text-list wrapper (`TextList`) may be
/// emitted anywhere: not the wasm wrapper class, not the rust-side `pub type` alias that rode
/// along with its registration (e.g. for a text-keyed table's keys type).
///
/// Whole-word scan: `TextList` with no identifier char on either side, so the nested-array
/// wrapper `ArrTextList` (an array of arrays stays non-exposable and KEEPS its wrapper) and
/// user-named idents don't false-positive. NAMED text-array rules (`texts = [* text]`) still
/// generate a wrapper under their own rule-derived ident — governed by the Alias arm's
/// RustStruct check in `directly_wasm_exposable`, not the anonymous Str arm this gate pins.
#[test]
fn no_anonymous_text_list_wrapper() {
    fn contains_word(content: &str, word: &str) -> bool {
        let is_ident = |c: u8| c.is_ascii_alphanumeric() || c == b'_';
        let bytes = content.as_bytes();
        let mut from = 0;
        while let Some(pos) = content[from..].find(word).map(|i| from + i) {
            let before_ok = pos == 0 || !is_ident(bytes[pos - 1]);
            let end = pos + word.len();
            let after_ok = end == bytes.len() || !is_ident(bytes[end]);
            if before_ok && after_ok {
                return true;
            }
            from = pos + 1;
        }
        false
    }
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            if contains_word(content, "TextList") {
                failures.push(format!("[{label}/{profile}] {path}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) emit the anonymous TextList wrapper — text arrays must cross the \
         wasm boundary as bare Vec<String>:\n{}",
        failures.join("\n")
    );
}

/// The generated RUST crate may carry `wasm_bindgen` only in the feature-gated `cfg_attr` form
/// (`generate_c_style_enum` — a c-style enum is the one type kind exposed to wasm by re-export
/// rather than a wasm-crate wrapper), so the crate compiles standalone without the optional
/// `wasm-bindgen` dep. Any other appearance in the rust tree — a bare `#[wasm_bindgen…]` from a
/// new emission site or profile — would force the dep on every rust-crate consumer. This is the
/// corpus-wide PLACEMENT half of the invariant; the COMPILE half (a feature-off `cargo check`,
/// unreachable through feature-unified workspace builds) is
/// `integration_tests::rust_wasm_bindgen_feature_gated_crate_compiles_standalone`. The positive
/// control (`gated_seen`) keeps the sweep honest: it fails if no whole_program input emits the
/// gated form at all — the fixture-blind-spot class that once graded the rust crate bindgen-free
/// from a fixture lacking the one construct that emits the attribute.
#[test]
fn rust_tree_wasm_bindgen_only_feature_gated() {
    let mut failures = Vec::new();
    let mut gated_seen = false;
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            if !path.starts_with("rust/") {
                continue;
            }
            for line in content.lines() {
                if !line.contains("wasm_bindgen") {
                    continue;
                }
                if line.trim_start().starts_with("#[cfg_attr(feature = ") {
                    gated_seen = true;
                } else {
                    failures.push(format!("[{label}/{profile}] {path}: {line}"));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "ungated wasm_bindgen in the generated rust tree — only the c-style-enum \
         `#[cfg_attr(feature = …, wasm_bindgen::prelude::wasm_bindgen)]` form is sanctioned:\n{}",
        failures.join("\n")
    );
    assert!(
        gated_seen,
        "no whole_program input emitted the feature-gated c-style-enum attribute — the sweep \
         lost its positive control (add a c-style enum to a whole_program input)"
    );
}

/// `rustfmt_generated_string` must FAIL LOUD on unparseable output rather than swallowing it and
/// returning the raw source at exit 0 — the swallow is exactly how the JSON-schema turbofish bug
/// (`T<..>::method` in expression position) shipped green. Valid Rust still round-trips to `Ok`.
#[test]
fn rustfmt_rejects_unparseable_source() {
    // valid Rust formats successfully
    crate::generation::rustfmt_generated_string("fn main() {}").expect("valid Rust must format Ok");
    // the turbofish shape (generic type-spelling before `::method`) is a parse error — must be Err
    assert!(
        crate::generation::rustfmt_generated_string(
            "fn f() -> usize { BTreeMap<u64, String>::len() }"
        )
        .is_err(),
        "rustfmt parse failure must propagate as Err, not be swallowed"
    );
}

/// The comment-preserve overlay's markers must survive the tool's OWN rustfmt pass. rustfmt folds a
/// `// cddl-codegen:<tag>` comment trailing the closing `}` of a match's last arm onto that `}` as a
/// trailing comment, so a `preserve → rustfmt_generated_string → preserve` loop would otherwise write
/// an on-disk form the next regen's own-line scan gate can't read. This closes that loop end to end:
/// emit an own-line match-tail replace block, run the tool's EXACT rustfmt pass (folds it on a folding
/// rustfmt, leaves it own-line otherwise — both spellings parse), then re-run `preserve`; the user
/// section and recorded original must survive. Robust across rustfmt versions: the acceptance
/// criterion is only that the rustfmt'd form re-parses, not that it folds.
#[test]
fn preserve_markers_survive_rustfmt_fold_roundtrip() {
    use crate::comment_preserve::preserve;
    // `old` as codegen+preserve would leave it on disk: an own-line match-tail replace block whose
    // user section swaps the generated `unknown => return Err(())` tail arm for a lenient skip.
    let old = "\
// header
impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            // cddl-codegen:replace-start
            unknown => {
                let _ = unknown;
                Ok(key)
            }
            // cddl-codegen:replaces
            // unknown => return Err(()),
            // cddl-codegen:replace-end
        }
    }
}
";
    // pristine regen: the generator re-emits the recorded original tail arm.
    let new = "\
// header
impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            unknown => return Err(()),
        }
    }
}
";
    // The tool's exact pass: `rustfmt --edition 2024` over the preserved on-disk content.
    let formatted =
        crate::generation::rustfmt_generated_string(old).expect("valid Rust must format Ok");
    // The next regen must re-parse the (possibly folded) on-disk form and re-apply the swap.
    let merged = preserve(&formatted, new)
        .expect("the rustfmt'd match-tail replace block must re-parse (fold-tolerant)");
    assert!(
        merged.content.contains("let _ = unknown;"),
        "user section lost across the rustfmt fold round-trip:\n{}",
        merged.content
    );
    assert!(
        merged.content.contains("// unknown => return Err(()),"),
        "recorded original lost across the rustfmt fold round-trip:\n{}",
        merged.content
    );
}

/// `generate_tag_check` (the tag check a directly-deserializing tagged type emits) has two arms
/// selected by `--annotate-fields`. The `annotated=true` arm is exercised end-to-end by the core
/// `tagged_type_choice` fixture (whole_program snapshot + the wrong-tag behavioral test), but that
/// fixture always renders with `annotate_fields=true`; the `annotated=false` name-carrying arm has
/// no *rendered* pin. This unit-renders BOTH arms and asserts the discriminating fragments (robust
/// to rustfmt line-wrapping via whitespace stripping):
/// * `annotated=true` — a bare `raw.tag()?` read and the LOCATIONLESS
///   `DeserializeFailure::TagMismatch{ .. }.into()` (the annotate closure supplies the name), never
///   the name-carrying `DeserializeError::new`.
/// * `annotated=false` — the name-carrying `DeserializeError::new("Ident", ..)` plus a tag read that
///   annotates the ident itself (`.map_err(|e| DeserializeError::from(e).annotate("Ident"))?`), since
///   no closure will add the location.
#[test]
fn generate_tag_check_arms() {
    use crate::generation::generate_tag_check;
    use crate::intermediate::{CDDLIdent, RustIdent};

    let ident = RustIdent::new(CDDLIdent::new("Ident"));
    let render = |annotated: bool| {
        let mut f = codegen::Function::new("deserialize");
        generate_tag_check(&mut f, &ident, Some(11), annotated);
        let mut scope = codegen::Scope::new();
        scope.push_fn(f);
        scope.to_string()
    };
    let flatten = |s: &str| -> String { s.chars().filter(|c| !c.is_whitespace()).collect() };

    let annotated = render(true);
    let flat_annotated = flatten(&annotated);
    assert!(
        flat_annotated.contains("lettag=raw.tag()?;"),
        "annotated arm must read the tag bare (no map_err annotate), got:\n{annotated}"
    );
    assert!(
        flat_annotated.contains("DeserializeFailure::TagMismatch{found:tag,expected:11}.into()"),
        "annotated arm must emit the locationless TagMismatch form, got:\n{annotated}"
    );
    assert!(
        !flat_annotated.contains("DeserializeError::new"),
        "annotated arm must NOT carry the name (the closure supplies it), got:\n{annotated}"
    );

    let named = render(false);
    let flat_named = flatten(&named);
    assert!(
        flat_named.contains(
            "DeserializeError::new(\"Ident\",DeserializeFailure::TagMismatch{found:tag,expected:11})"
        ),
        "unannotated arm must emit the name-carrying error, got:\n{named}"
    );
    assert!(
        flat_named.contains(".map_err(|e|DeserializeError::from(e).annotate(\"Ident\"))?"),
        "unannotated arm must annotate the tag read with the ident, got:\n{named}"
    );
}

// --- Dep-side extern-interface export emitter (commit 4) ----------------------------------------

/// Snapshot the emitted extern-interface tree for the `tests/extern-interface-emit/inputs` fixture,
/// which exercises EVERY projection row: opaque Record/Wrapper/TypeChoice/GroupChoice, raw-bytes,
/// transparent named collections (Array + Table) via the alias spelling, c-style enum, `@no_alias`,
/// an alias chain, a prelude (`bignint`) reference that renders (not excluded), a named generic
/// instance (opaque), a plain group / generic definition / extern-dep-scope rule (all ABSENT), a
/// nested-scope subfile, a REFERENCED plain group (a transparent group-body row) and a
/// never-referenced one (excluded-with-record, Ask 0), a generic definition / extern-dep-scope rule
/// (both ABSENT), and the exclude-with-record + reference-closure paths (custom-serialize alias, its
/// transitive dependent, an anonymous-generic-instance reference). Snapshots live beside
/// the fixture. Bless with `INSTA_UPDATE=always cargo test extern_interface_emit`.
#[test]
fn extern_interface_emit() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli)
        .expect("extern-interface projection must succeed (exclude-with-record, never abort)");

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/extern-interface-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        assert!(!files.is_empty(), "no extern-interface files emitted");
        for (path, content) in &files {
            let name = path.replace('/', "__");
            insta::assert_snapshot!(name, content);
        }
    });
}

/// The projection is deterministic: emit twice, require byte-identical output (all-`BTreeMap`/`BTreeSet`,
/// no `HashMap`). Same guarantee `generation_is_deterministic` gives the main output.
#[test]
fn extern_interface_emit_is_deterministic() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let a = crate::api::extern_interface_strings(&cli).unwrap();
    let b = crate::api::extern_interface_strings(&cli).unwrap();
    assert_eq!(
        a, b,
        "extern-interface export must be byte-identical across runs"
    );
}

/// Emission is UNCONDITIONAL in every mode: the export is byte-identical under `--wasm=false` and
/// `--wasm=true` (it describes named rule surfaces, not the wasm wrapper inventory).
#[test]
fn extern_interface_emit_same_in_both_modes() {
    let base = std::path::Path::new("tests/extern-interface-emit/inputs");
    let rust_only = crate::api::extern_interface_strings(&cli_for(
        base,
        &["--wasm", "false", "--lib-name", "dep"],
    ))
    .unwrap();
    let wasm = crate::api::extern_interface_strings(&cli_for(
        base,
        &["--wasm", "true", "--lib-name", "dep"],
    ))
    .unwrap();
    assert_eq!(
        rust_only, wasm,
        "extern-interface export must not depend on the wasm/rust-only mode"
    );
}

/// A surface with NO included rules still emits a single root file (stable presence, answering "was
/// this dep regenerated?"). The lone rule is a never-referenced plain group, which materializes no
/// shape but — per the excluded-with-record contract (Ask 0) — leaves a `; unexported:` record rather
/// than vanishing, so the root file is the header plus that one record and nothing else.
#[test]
fn extern_interface_emit_empty_surface() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/empty/lib.cddl"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    assert_eq!(
        files.keys().collect::<Vec<_>>(),
        vec!["extern-interface/dep/mod.cddl"],
        "a no-included-rows surface must emit exactly one root file"
    );
    assert_eq!(
        files["extern-interface/dep/mod.cddl"],
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\n\
         ; unexported: helper — plain group never referenced in the dependency's own spec — no \
         materialized shape to project\n",
        "the root file is the header line plus the lone plain group's exclusion record"
    );
}

/// The exclude-with-record semantics, asserted directly (clearer regression messages than the tree
/// snapshot alone): generation SUCCEEDS, a custom-serialize transparent alias is excluded with a
/// record, a rule referencing it is transitively excluded naming the CHAIN ROOT (not its immediate
/// neighbour), an anonymous-generic-instance reference is excluded, and a prelude (`bignint`)
/// reference RENDERS rather than being excluded.
#[test]
fn extern_interface_emit_exclusions_and_closure() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    let root = &files["extern-interface/dep/mod.cddl"];

    // custom-serialize transparent alias -> excluded with a record, generation still succeeded.
    assert!(
        root.contains("; unexported: cs — @custom_serialize"),
        "custom-serialize alias `cs` must be excluded with a record:\n{root}"
    );
    assert!(
        !root.contains("\ncs = "),
        "excluded `cs` must NOT appear as an exported rule:\n{root}"
    );

    // reference-closure: dep_cs references excluded cs -> excluded, naming the chain root `cs`.
    assert!(
        root.contains("; unexported: dep_cs — references excluded cs"),
        "`dep_cs` must be closure-excluded naming root `cs`:\n{root}"
    );

    // anonymous generic instance reference -> excluded (no CDDL ident to spell).
    assert!(
        root.contains("; unexported: anon_arr —"),
        "`anon_arr` (references an anonymous generic instance) must be excluded:\n{root}"
    );

    // a prelude reference RENDERS by prelude name, it is NOT an exclusion.
    assert!(
        root.contains("bn = bytes .cbor {* bignint => uint} ; @rust_name Bn"),
        "`bn` must render the `bignint` prelude reference, not be excluded:\n{root}"
    );
    assert!(
        !root.contains("; unexported: bn"),
        "`bn` must not be excluded:\n{root}"
    );

    // an opaque row that embeds an excluded type is self-contained -> NOT excluded.
    assert!(
        root.contains("holder = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "`holder` (opaque, embeds `cs` internally) must still export:\n{root}"
    );
    assert!(
        !root.contains("; unexported: holder"),
        "`holder` must not be excluded — its opaque marker is self-contained:\n{root}"
    );
}

/// A named rule BINDING a generic set-nominal instantiation used at more than one site
/// (`req_signers = nonempty_set<uint>`, with a second anonymous use forcing the instantiation to
/// mint under its own canonical ident) lowers to a `pub type` alias TO the nominal — and that alias
/// must project into the extern interface as an OPAQUE row, not drop out with a `; unexported:`
/// record. The instantiation-minted nominal (`NonemptySetU64`) has NO source CDDL rule name, so the
/// transparent renderer's `render_rust_ref` once hard-`Err`ed on the unspellable `Rust(<nominal>)`
/// reference and dropped the whole rule (CML's first regen lost its `required_signers` row this
/// way); it now takes the same opaque marker the nominal itself would in pass 1, named by the
/// alias's `@rust_name`. The consumer references it opaquely and never needs the instantiation's
/// spelling.
#[test]
fn extern_interface_projects_alias_to_set_nominal_as_opaque() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_extif_setnominal_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         req_signers = nonempty_set<uint>\n\
         holder = [rs: req_signers, more: nonempty_set<uint>]\n",
    )
    .unwrap();
    let cli = cli_for(&path, &["--wasm", "false", "--lib-name", "dep"]);
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    std::fs::remove_file(&path).ok();
    let root = &files["extern-interface/dep/mod.cddl"];
    assert!(
        root.contains("req_signers = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name ReqSigners"),
        "the alias-to-set-nominal rule must project as an opaque row named by its @rust_name:\n{root}"
    );
    assert!(
        !root.contains("; unexported: req_signers"),
        "the alias-to-set-nominal rule must NOT drop out with an unexported record:\n{root}"
    );
}

// --- Dep-side extern-interface compiled self-check (commit 5) ------------------------------------

/// Snapshot the dep-side compiled self-check (`generated/extern_interface_check.rs`) the emit
/// fixture produces. It is derived from the SAME projection as the export snapshotted above, and
/// covers every assertion kind in one file: `Serialize`(+`Deserialize`) on opaque rows
/// (record/wrapper/type-choice/group-choice/named-generic-instance), `RawBytesEncoding` on the
/// raw-bytes row (`hash`), the `use … as _;` existence check on transparent rows (aliases /
/// c-style enum / named collections), the `@no_alias` skip (`na` gets no assertion), the group-body
/// row's four-bound assertion (`pg` — whole-value + embedded-group Serialize/Deserialize), and a
/// nested-scope path (`sub::module::NestedRec`). Bless with
/// `INSTA_UPDATE=always cargo test extern_interface_check_emit`.
#[test]
fn extern_interface_check_emit() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::generated_strings(&cli)
        .expect("generation must succeed for the emit fixture (string emission is infallible)");
    let content = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("the self-check file must be emitted");

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/extern-interface-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!("extern_interface_check", content);
    });
}

/// The self-check WEAKENS the bound to `Serialize` only for a type with no generated `Deserialize`:
/// the ambiguous-optional array record `ambig` has no deserialize impl (`print_structs_without_deserialize`),
/// so asserting `Deserialize` on it would fail the dep's own build. A normal record (`rec`) keeps
/// both, and a `@no_alias` rule (`raw_index`) — which emits no rust type — is not asserted at all.
#[test]
fn extern_interface_check_weakens_deserialize_bound() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-check/inputs"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        c.contains("_assert_serialize::<crate::generated::Ambig>()"),
        "the deserialize-less `ambig` must still assert Serialize:\n{c}"
    );
    assert!(
        !c.contains("_assert_deserialize::<crate::generated::Ambig>()"),
        "`ambig` has no generated deserialize — its bound must be WEAKENED to Serialize only:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::Rec>()")
            && c.contains("_assert_deserialize::<crate::generated::Rec>()"),
        "a normal record keeps BOTH bounds:\n{c}"
    );
    assert!(
        !c.contains("RawIndex"),
        "a `@no_alias` rule emits no rust type — the self-check must assert nothing for it:\n{c}"
    );
}

/// A self-check assertion on a generic-extern BASE (`ext_set<T>` → rust `ExtSet<T>`) would not
/// compile — bare `ExtSet` names no concrete type — so the base is SKIPPED, while its concrete
/// siblings still assert. The generated crate compiles end-to-end via `extern_generic_raw_bytes`;
/// this pins the skip decision directly (fast, no nested cargo).
#[test]
fn extern_interface_check_skips_generic_base() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-generic-raw-bytes/input.cddl"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        !c.contains("ExtSet"),
        "the generic-extern base `ExtSet<T>` is not a concrete type — it must be skipped:\n{c}"
    );
    assert!(
        c.contains("_assert_raw_bytes::<crate::generated::PubKey>()"),
        "the raw-bytes row must assert RawBytesEncoding:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::Plain>()")
            && c.contains("_assert_serialize::<crate::generated::UsingFlavored>()"),
        "the concrete opaque rows must still assert Serialize:\n{c}"
    );
}

/// The extern-interface self-check counterpart to `extern_interface_check_skips_generic_base` for a
/// generic-extern base with ZERO instances (`ext_unused<T>` in `json-extern-rows`). `ExtSet` there
/// has an instance, so `generic_instance_bases` catches it; `ext_unused` has none, so the `None`
/// check-kind decision must key on the parse-time `generic_extern_bases` record — otherwise the base
/// leaks an `_assert_serialize::<crate::generated::ExtUnused>()` that is E0107 in the dep's build.
#[test]
fn extern_interface_check_skips_generic_base_without_instances() {
    let cli = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        !c.contains("ExtUnused"),
        "a never-instantiated generic-extern base names no concrete type — it must be skipped:\n{c}"
    );
    // The instantiated generic base is likewise skipped, while the concrete rows still assert.
    assert!(
        !c.contains("ExtSet"),
        "the instantiated generic-extern base is also skipped:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::MyExtern>()")
            && c.contains("_assert_serialize::<crate::generated::MySet>()")
            && c.contains("_assert_serialize::<crate::generated::BigThing>()"),
        "the concrete opaque rows must still assert Serialize:\n{c}"
    );
}
