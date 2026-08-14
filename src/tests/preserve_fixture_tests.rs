//! File-fixture harness for the comment/code preservation overlay (`comment_preserve::preserve`).
//!
//! Each case is a directory under `tests/preserve-fixtures/<name>/` holding:
//! * `old.rs` — the prior on-disk file (user comments / insert blocks / carried sentinel blocks);
//! * `new.rs` — the freshly generated pristine content;
//! * exactly one expectation: `expected.rs` (the exact merge output) OR `error.txt` (a substring the
//!   hard [`PreserveError`], rendered as the CLI shows it via `render("old.rs")`, must contain — so a
//!   case can pin the clickable `old.rs:<line>:` prefix, not just the bare message).
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

/// Sweeps the whole expected-case corpus for the ON-DISK fixed point: the form the tool actually
/// writes is `rustfmt(preserve(...))`, so the run-twice = run-once property that matters in
/// production is `rustfmt(preserve(rustfmt(expected), new)) == rustfmt(expected)` — a fixed point
/// over the POST-rustfmt bytes, not the pre-rustfmt ones property (a) of `preserve_fixtures` pins.
/// It never asserts any specific folded spelling, so it is robust across rustfmt versions (same
/// acceptance-criterion posture as `preserve_markers_survive_rustfmt_fold_roundtrip`), and a
/// rustfmt bump that starts folding a new construct trips it here rather than in a consumer.
/// The never-silent leg uses `old.rs` as its baseline — the strongest available chain: nothing
/// user-authored is lost across old → merge → format → merge.
///
/// The corpus spans the match-tail fold family plus every block flavor at the last statement of a
/// block, an if/else tail, a struct-literal last field, a last enum variant, and nested-module and
/// impl closing-brace tails. It is a REGRESSION NET over those embodied fold/format classes — plus
/// automatic coverage of every fixture added later and a rustfmt-version-bump tripwire — NOT a
/// discovery instrument for formatter comment behavior outside the corpus.
///
/// It both TESTS and DEPENDS ON the `unfold_trailing_markers` pre-pass: the three
/// `*_rustfmt_folded_tail_*` match-tail fixtures re-fold during the sweep's rustfmt step and unfold
/// at the subsequent `preserve` entry. The positional-diversity keep/insert/replace triples carry
/// the same three flavors through all six additional tail geometries; on the current pinned
/// rustfmt their fixed-point formatting remains own-line, so they are version-bump/re-ownership
/// tripwires rather than claims that every tail currently folds.
///
/// Error cases are exempt by construction: the 20 `error.txt` cases' `old.rs` are user-malformed
/// inputs the tool never wrote, and a `PreserveError` propagates out of `export()` before the write
/// loop, so no on-disk file ever has that provenance. The fold-induced hard-error class — a
/// rustfmt fold that makes the next `preserve` reject its own output — is caught by the
/// expected-case assertions themselves (step 3 must be `Ok`).
#[test]
fn preserve_fixtures_rustfmt_cycle_stability() {
    // Skip under bless: cargo runs tests as parallel threads in ONE process, and the sibling
    // `preserve_fixtures` bless path REWRITES `expected.rs` files while this sweep reads them —
    // a read of a half-written file would be a spurious red.
    if std::env::var("BLESS_PRESERVE_FIXTURES").map(|v| v == "1") == Ok(true) {
        eprintln!(
            "BLESS_PRESERVE_FIXTURES=1: skipping the rustfmt-cycle sweep (expected.rs files are being rewritten concurrently)"
        );
        return;
    }

    let root = std::env::current_dir()
        .unwrap()
        .join("tests/preserve-fixtures");

    let mut cases: Vec<std::path::PathBuf> = std::fs::read_dir(&root)
        .unwrap_or_else(|e| panic!("cannot read fixture root {}: {e}", root.display()))
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    cases.sort();

    let mut failures: Vec<String> = Vec::new();
    let mut swept = 0usize;

    for dir in &cases {
        let name = dir.file_name().unwrap().to_string_lossy().into_owned();
        // Error cases have no on-disk-provenance form to format — see the doc comment.
        if dir.join("error.txt").exists() {
            continue;
        }
        let read = |f: &str| std::fs::read_to_string(dir.join(f));
        let (old, new, expected) = match (read("old.rs"), read("new.rs"), read("expected.rs")) {
            (Ok(o), Ok(n), Ok(e)) => (o, n, e),
            (o, n, e) => {
                failures.push(format!(
                    "[{name}] unreadable fixture files (old: {:?}, new: {:?}, expected: {:?})",
                    o.err(),
                    n.err(),
                    e.err()
                ));
                continue;
            }
        };
        swept += 1;

        // Step 1: the on-disk form the tool would actually write for this merge output.
        let disk1 = match crate::generation::rustfmt_generated_string(&expected) {
            Ok(s) => s.into_owned(),
            Err(e) => {
                failures.push(format!("[{name}] rustfmt(expected.rs) failed: {e}"));
                continue;
            }
        };

        // Step 2: the NEXT regen must re-parse that post-rustfmt on-disk form.
        let merged = match preserve(&disk1, &new) {
            Ok(m) => m,
            Err(e) => {
                failures.push(format!(
                    "[{name}] preserve(rustfmt(expected), new) errored — the tool cannot re-read \
                     its own on-disk output: {e}"
                ));
                continue;
            }
        };

        // Step 3: on-disk fixed point — run twice == run once, over the bytes actually written.
        match crate::generation::rustfmt_generated_string(&merged.content) {
            Ok(disk2) if disk2 == disk1 => {}
            Ok(disk2) => failures.push(format!(
                "[{name}] NOT an on-disk fixed point: rustfmt(preserve(rustfmt(expected), new)) != \
                 rustfmt(expected)\n--- first on-disk form ---\n{disk1}\n--- second on-disk form \
                 ---\n{disk2}"
            )),
            Err(e) => failures.push(format!(
                "[{name}] rustfmt(preserve(rustfmt(expected), new)) failed — the merge produced \
                 unformattable Rust: {e}"
            )),
        }

        // Never-silent across the whole chain, baselined on old.rs.
        match never_silent_units(&old) {
            Ok(units) => {
                for unit in units {
                    if !unit_survives(&unit, &merged.content) {
                        failures.push(format!(
                            "[{name}] never-silent violation across the rustfmt cycle: a user \
                             comment/block from old.rs is neither placed nor trapped in a \
                             compile_error! after old → merge → rustfmt → merge:\n{unit}"
                        ));
                    }
                }
            }
            Err(e) => failures.push(format!(
                "[{name}] never_silent_units(old) errored (old.rs must lex/scan cleanly): {e}"
            )),
        }
    }

    assert!(
        swept > 0,
        "no expected.rs fixture cases found under {} — the sweep would be vacuously green",
        root.display()
    );
    assert!(
        failures.is_empty(),
        "preserve fixture rustfmt-cycle failures ({} over {swept} expected-case fixture(s)):\n{}",
        failures.len(),
        failures.join("\n\n")
    );
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

        // Hard-error case: `preserve` must return an Err whose rendered form contains error.txt.
        if error_path.exists() {
            match preserve(&old, &new) {
                Ok(_) => failures.push(format!(
                    "[{name}] expected a PreserveError (error.txt present) but preserve succeeded"
                )),
                Err(e) => {
                    let want = std::fs::read_to_string(&error_path).unwrap();
                    let want = want.trim();
                    // Match against the RENDERED form the CLI shows — `old.rs` is the fixture's own
                    // on-disk file, the analog of the real generated file — so an error.txt can pin
                    // the clickable `old.rs:<line>:` prefix, not just the bare message.
                    let got = e.render("old.rs");
                    if !got.contains(want) {
                        failures.push(format!(
                            "[{name}] error message mismatch\n  want substring: {want}\n  got: {got}"
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
