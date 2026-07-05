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
    for path in entries {
        let label = path.file_stem().unwrap().to_str().unwrap().to_owned();
        assert!(
            !WHOLE_PROGRAM_CASES
                .iter()
                .any(|(whole_program_label, _, _)| *whole_program_label == label),
            "corpus file {:?} collides with a whole_program snapshot dir; rename it",
            path
        );
        snapshot_input(&path, &label, ALL_PROFILES, false, true);
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

/// Drift gate for the derived `static/Cargo_{rust,wasm,json_gen}.toml` templates.
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

/// `rustfmt_generated_string` must FAIL LOUD on unparseable output rather than swallowing it and
/// returning the raw source at exit 0 — the swallow is exactly how the JSON-schema turbofish bug
/// (`T<..>::method` in expression position) shipped green. Valid Rust still round-trips to `Ok`.
#[test]
fn rustfmt_rejects_unparseable_source() {
    // valid Rust formats successfully
    assert!(crate::generation::rustfmt_generated_string("fn main() {}").is_ok());
    // the turbofish shape (generic type-spelling before `::method`) is a parse error — must be Err
    assert!(
        crate::generation::rustfmt_generated_string(
            "fn f() -> usize { BTreeMap<u64, String>::len() }"
        )
        .is_err(),
        "rustfmt parse failure must propagate as Err, not be swallowed"
    );
}
