//! Input panic-robustness catalog.
//!
//! Feeds malformed / edge-case spec inputs (`tests/robustness/*.cddl`) to the generator inside
//! `catch_unwind` and snapshots the OUTCOME of each — `ok` / `error (graceful)` / `PANIC`. This is
//! a robustness scorecard, not an output-regression test: it catches a refactor that makes a
//! previously-graceful input newly panic (shows up as a snapshot diff), and when a current panic
//! is fixed its entry flips (re-bless then).
//!
//! NB: several inputs currently record `PANIC` — the generator `unwrap`s/asserts on invalid input
//! instead of returning a clean error (see `draft/prelude-bug-report.md`). Those are real bugs,
//! recorded here so the class is visible and tracked rather than silently lurking. The catalog
//! deliberately records only the outcome *category* (not panic messages/line numbers) so it stays
//! stable across refactors that don't change behaviour.

use crate::cli::Cli;
use clap::Parser;

#[test]
fn input_robustness_catalog() {
    let dir = std::path::Path::new("tests/robustness");
    let mut inputs: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    inputs.sort();
    assert!(!inputs.is_empty(), "no robustness inputs in {:?}", dir);

    // We deliberately trigger panics below; silence the default hook so the test output stays clean.
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let mut catalog =
        String::from("# generator outcome per malformed/edge input\n# PANIC = known bug to fix (see draft/prelude-bug-report.md)\n\n");
    for path in &inputs {
        let name = path.file_stem().unwrap().to_str().unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "robustness_unused",
        ]);
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            crate::api::generated_strings(&cli)
        }));
        let label = match outcome {
            Ok(Ok(_)) => "ok",
            Ok(Err(_)) => "error (graceful)",
            Err(_) => "PANIC",
        };
        catalog.push_str(&format!("{:26} {}\n", name, label));
    }
    std::panic::set_hook(prev_hook);

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/robustness/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}
