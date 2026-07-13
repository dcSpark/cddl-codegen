//! File-fixture harness for the comment/code preservation overlay (`comment_preserve::preserve`).
//!
//! Each case is a directory under `tests/preserve-fixtures/<name>/` holding:
//! * `old.rs` — the prior on-disk file (user comments / insert blocks / carried sentinel blocks);
//! * `new.rs` — the freshly generated pristine content;
//! * exactly one expectation: `expected.rs` (the exact merge output) OR `error.txt` (a substring the
//!   hard [`PreserveError`] message must contain).
//!
//! For an `expected.rs` case the harness asserts `preserve(old, new).content == expected`
//! BYTE-FOR-BYTE — strictly stronger than a `contains()` check — and then runs, independent of the
//! blessed bytes, three cross-cutting properties that make "quietly wrong placement" hard to bless:
//!   (a) idempotent fixed point (pre-rustfmt): `preserve(expected, new).content == expected`;
//!   (b) never-silent: every own-line non-doc user comment / insert block in `old.rs` survives in the
//!       output (placed/re-indented, or `escape_for_rust_string`-transformed inside a `compile_error!`);
//!   (c) `changed == false` ⇒ output is byte-identical to `new`.
//!
//! Bless with `BLESS_PRESERVE_FIXTURES=1 cargo test --bin cddl-codegen preserve_fixtures` (precedent:
//! `BLESS_MANIFEST_TEMPLATES`). Blessing regenerates `expected.rs` files only; it NEVER creates an
//! `error.txt` case (hard-error expectations are authored by hand). Blessed diffs are reviewed like
//! snapshots — the properties above guard against blessing a splice bug.
//!
//! Merge-logic cases live here so they never churn when the generator changes (`preserve` is pure).
//! Lexer-level tests (char-vs-lifetime, raw identifiers, in-string `//`, …) stay inline in
//! `comment_preserve.rs` — they test `lex`, not the merge.

use crate::comment_preserve::{escape_for_rust_string, never_silent_units, preserve};

/// A never-silent unit survives if its whole escaped form appears (the failed-block/`compile_error!`
/// case) or every non-empty trimmed line appears verbatim (the placed case, robust to re-indentation).
fn unit_survives(unit: &str, output: &str) -> bool {
    if output.contains(&escape_for_rust_string(unit)) {
        return true;
    }
    unit.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .all(|l| output.contains(l))
}

#[test]
fn preserve_fixtures() {
    let root = std::env::current_dir()
        .unwrap()
        .join("tests/preserve-fixtures");
    let bless = std::env::var("BLESS_PRESERVE_FIXTURES")
        .map(|v| v == "1")
        .unwrap_or(false);

    let mut cases: Vec<std::path::PathBuf> = std::fs::read_dir(&root)
        .unwrap_or_else(|e| panic!("cannot read fixture root {}: {e}", root.display()))
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    cases.sort();
    assert!(
        !cases.is_empty(),
        "no fixtures found under {}",
        root.display()
    );

    let mut failures: Vec<String> = Vec::new();
    let mut blessed = 0usize;

    for dir in &cases {
        let name = dir.file_name().unwrap().to_string_lossy().into_owned();
        let read = |f: &str| std::fs::read_to_string(dir.join(f));
        let old = match read("old.rs") {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("[{name}] missing old.rs: {e}"));
                continue;
            }
        };
        let new = match read("new.rs") {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("[{name}] missing new.rs: {e}"));
                continue;
            }
        };
        let error_path = dir.join("error.txt");
        let expected_path = dir.join("expected.rs");

        // Hard-error case: `preserve` must return an Err whose message contains error.txt's content.
        if error_path.exists() {
            match preserve(&old, &new) {
                Ok(_) => failures.push(format!(
                    "[{name}] expected a PreserveError (error.txt present) but preserve succeeded"
                )),
                Err(e) => {
                    let want = std::fs::read_to_string(&error_path).unwrap();
                    let want = want.trim();
                    if !e.message.contains(want) {
                        failures.push(format!(
                            "[{name}] error message mismatch\n  want substring: {want}\n  got: {}",
                            e.message
                        ));
                    }
                }
            }
            continue;
        }

        // Expected-output case.
        let result = match preserve(&old, &new) {
            Ok(r) => r,
            Err(e) => {
                failures.push(format!(
                    "[{name}] preserve returned an error but no error.txt is present: {e}"
                ));
                continue;
            }
        };

        if bless {
            std::fs::write(&expected_path, &result.content).unwrap();
            blessed += 1;
        }

        let expected = match std::fs::read_to_string(&expected_path) {
            Ok(s) => s,
            Err(_) => {
                failures.push(format!(
                    "[{name}] no expected.rs and no error.txt — bless with \
                     BLESS_PRESERVE_FIXTURES=1 to create it (bless never creates error.txt)"
                ));
                continue;
            }
        };

        // Primary assertion: byte-for-byte merge output.
        if result.content != expected {
            failures.push(format!(
                "[{name}] merge output does not match expected.rs (byte-for-byte). Re-bless with \
                 BLESS_PRESERVE_FIXTURES=1 after reviewing.\n--- expected ---\n{expected}\n--- got \
                 ---\n{}",
                result.content
            ));
            continue;
        }

        // Property (a): idempotent fixed point (pre-rustfmt).
        match preserve(&expected, &new) {
            Ok(fp) if fp.content == expected => {}
            Ok(fp) => failures.push(format!(
                "[{name}] NOT an idempotent fixed point: preserve(expected, new) != expected\n--- \
                 expected ---\n{expected}\n--- re-run ---\n{}",
                fp.content
            )),
            Err(e) => failures.push(format!(
                "[{name}] preserve(expected, new) errored (expected.rs must round-trip): {e}"
            )),
        }

        // Property (c): changed == false implies the output equals new byte-identically.
        if !result.changed && result.content != new {
            failures.push(format!(
                "[{name}] changed == false but content != new (byte-for-byte)"
            ));
        }

        // Property (b): never-silent — every own-line non-doc user comment / insert block in old.rs
        // survives (placed or trapped in a compile_error!).
        match never_silent_units(&old) {
            Ok(units) => {
                for unit in units {
                    if !unit_survives(&unit, &result.content) {
                        failures.push(format!(
                            "[{name}] never-silent violation: a user comment/block from old.rs is \
                             neither placed nor trapped in a compile_error!:\n{unit}"
                        ));
                    }
                }
            }
            Err(e) => failures.push(format!(
                "[{name}] never_silent_units(old) errored (old.rs must lex/scan cleanly): {e}"
            )),
        }
    }

    if bless {
        eprintln!("blessed {blessed} preserve fixture(s)");
    }
    assert!(
        failures.is_empty(),
        "preserve fixture failures ({}):\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
