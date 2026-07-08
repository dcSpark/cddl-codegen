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
/// non-value enum dispatch site (src/generation.rs, `names_without_outer.len() > 1` around line
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
