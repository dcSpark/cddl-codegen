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
//!   snapshotted under every applicable flag profile plus an IR dump. A one-feature regression
//!   yields a one-file diff. Snapshots are grouped per feature under `tests/corpus/snapshots/<feature>/`.
//! * [`whole_program`] — the existing integration-test inputs, each under one known-safe profile,
//!   to catch cross-feature interactions at scale.
//!
//! By default a corpus file is generated under all of [`ALL_PROFILES`]. A file whose construct is
//! not valid under some flag (e.g. fixed-value fields + `--preserve-encodings`, see cddl-codegen
//! issue #205) opts out via a first-line directive: `; snapshot-profiles: default json`.
//!
//! Bless after an intentional change with `INSTA_UPDATE=always cargo test` (or `cargo insta review`).

use crate::cli::Cli;
use clap::Parser;

type Profile = (&'static str, &'static [&'static str]);

/// The flag axes that drive meaningfully different generation paths.
const ALL_PROFILES: &[Profile] = &[
    ("default", &[]),
    ("preserve", &["--preserve-encodings=true"]),
    (
        "json",
        &["--json-serde-derives=true", "--json-schema-export=true"],
    ),
];

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

/// Profiles a corpus file should be generated under: all of [`ALL_PROFILES`] unless the file's
/// first line is a `; snapshot-profiles: <names...>` directive restricting them.
fn profiles_for(input: &std::path::Path) -> Vec<Profile> {
    let contents = std::fs::read_to_string(input).unwrap();
    let first_line = contents.lines().next().unwrap_or("").trim();
    match first_line.strip_prefix("; snapshot-profiles:") {
        Some(rest) => {
            let wanted: Vec<&str> = rest.split_whitespace().collect();
            let profiles: Vec<Profile> = ALL_PROFILES
                .iter()
                .filter(|(name, _)| wanted.contains(name))
                .copied()
                .collect();
            assert!(
                !profiles.is_empty(),
                "{:?}: snapshot-profiles directive matched no known profiles",
                input
            );
            profiles
        }
        None => ALL_PROFILES.to_vec(),
    }
}

/// Snapshot the generated source for `input` under each profile (grouped under
/// `tests/corpus/snapshots/<label>/`), and optionally the IR.
fn snapshot_input(input: &std::path::Path, label: &str, profiles: &[Profile], with_ir: bool) {
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

/// One tiny CDDL file per language construct → a localized snapshot per feature, across every
/// applicable flag profile.
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
        snapshot_input(&path, &label, &profiles_for(&path), true);
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
