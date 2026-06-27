//! Fast, in-process golden snapshots of generated output.
//!
//! Unlike the heavy integration tests in `integration_tests.rs` (which shell out to `cargo run`
//! and then compile + round-trip the generated crates), these drive the generator as a library
//! (`crate::api`) and snapshot the post-rustfmt generated source directly. No subprocess, no
//! compilation, no `target/` bloat. They give a localized diff when generation output changes;
//! the integration tests remain the "does it actually compile & round-trip" correctness gate.
//!
//! Two layers:
//! * [`feature_corpus`] — one tiny CDDL file per language construct (in `tests/corpus/`), each
//!   snapshotted under the `default` profile plus an IR dump. A one-feature regression yields a
//!   one-file diff.
//! * [`whole_program`] — the existing integration-test inputs, each under the flag profile it is
//!   known-safe with, to catch cross-feature interactions and the preserve/canonical/json paths.
//!
//! Bless after an intentional change with `INSTA_UPDATE=always cargo test` (or `cargo insta review`).

use crate::cli::Cli;
use clap::Parser;

type Profile = (&'static str, &'static [&'static str]);

/// Build a `Cli` for in-process generation. `--output`/`--static-dir` are unused because
/// `generated_strings` does no disk I/O, but clap requires `--output` to be present.
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

fn snapshot_dir() -> std::path::PathBuf {
    std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots")
}

/// Snapshot the generated source for `input` under each given profile, and optionally the IR.
fn snapshot_input(input: &std::path::Path, label: &str, profiles: &[Profile], with_ir: bool) {
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(snapshot_dir());
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
                let name = format!("{}__{}__{}", label, profile, path.replace('/', "__"));
                insta::assert_snapshot!(name, content);
            }
        }
        if with_ir {
            // IR is essentially profile-independent, so snapshot it once under default. Localizes
            // a regression to parsing/IR vs generation.
            let ir = crate::api::ir_structs_debug(&cli_for(input, &[]))
                .unwrap_or_else(|e| panic!("IR build failed for {}: {}", label, e));
            insta::assert_snapshot!(format!("{}__ir", label), ir);
        }
    });
}

const DEFAULT_ONLY: &[Profile] = &[("default", &[])];

/// One tiny CDDL file per language construct → a localized snapshot per feature (under the
/// default profile, which is valid for any well-formed input). Per-feature preserve/json
/// localization is intentionally left to [`whole_program`]: not every construct is valid under
/// every flag (e.g. fixed-value fields + `--preserve-encodings`, see cddl-codegen issue #205),
/// and pairing inputs with safe flags avoids brittle whack-a-mole.
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
        snapshot_input(&path, &label, DEFAULT_ONLY, true);
    }
}

/// The existing integration-test inputs, each under the flag profile it is known-safe with
/// (the same pairings the heavy integration tests use). Covers cross-feature interactions and
/// the preserve / canonical / json codegen paths at scale.
#[test]
fn whole_program() {
    let cases: &[(&str, &str, Profile)] = &[
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
    ];
    for (label, input, profile) in cases {
        snapshot_input(
            &std::path::PathBuf::from(input),
            label,
            std::slice::from_ref(profile),
            false,
        );
    }
}
