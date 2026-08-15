//! A ratchet over the executed fixture tests' NEGATIVE decode vectors.
//!
//! A hand-derived negative byte vector asserted with a bare
//! `assert!(T::from_cbor_bytes(&bytes).is_err())` passes for ANY failure. A vector that fails
//! EARLIER than the boundary it was written to prove — one byte off, a wrong major type reached
//! before the check ever runs — therefore stays green while the pinned boundary goes unexercised:
//! outcome right, provenance wrong, invisible to every gate by construction. That is not
//! hypothetical here; a `bytes .cbor T` leftover vector shipped exactly that way and was caught by
//! re-deriving its bytes by hand, not by any test.
//!
//! The discriminated form is `assert_decode_reject_reason::<T>(&bytes, "<distinctive substring>")`
//! — a per-file helper defined in each fixture `tests.rs` that uses it, spelled identically across
//! files because each is appended standalone into its own generated crate (no module for a shared
//! definition, no import that could reach one). This scan is the other half: it holds the count of
//! UNDISCRIMINATED sites at a committed per-file baseline, so a new bare site has to either use the
//! helper or move the baseline in the same commit.
//!
//! The baseline is an exact match in BOTH directions rather than a ceiling with a prose note beside
//! it, because a note beside a floor records an observation that stops being true without anything
//! failing. Above baseline names the new site; below baseline says lower it here and now.

use std::collections::BTreeMap;

/// Per-file count of same-statement `from_cbor_bytes(..)…is_err()` sites that are still
/// undiscriminated, keyed by the file's path under `tests/`.
///
/// A file absent from this table has an implicit baseline of ZERO — which is what makes adding the
/// first bare site to any fixture (existing or brand new) fail, without obliging a row per fixture.
/// Rows are only for files that still carry such sites; converting a file's last one deletes its
/// row.
///
/// This may only go DOWN. Raising an entry is not a fix — it is the defect this scan exists to
/// name.
const BARE_DECODE_REJECT_BASELINE: &[(&str, usize)] = &[
    ("alias-of-marker-e2e/tests.rs", 2),
    ("canonical/tests.rs", 3),
    ("custom-serialize-canonical-e2e/tests.rs", 2),
    ("golden_hex_preserve/tests.rs", 2),
    ("open-array-e2e/tests.rs", 1),
    ("open-struct-map-e2e/tests.rs", 12),
    ("open-struct-map-ignore-e2e/tests.rs", 3),
    ("open-struct-map-preserve-e2e/tests.rs", 3),
    ("open-table-cip25-acceptance/tests.rs", 6),
    ("open-table-e2e/tests.rs", 3),
    ("raw-bytes-preserve/tests.rs", 2),
    ("recursive-collection-ref/tests.rs", 1),
];

/// Everything under `tests/<dir>/` whose name starts with `tests` and ends in `.rs` — the files
/// the integration harness appends into a generated crate and then runs. A positive enumeration
/// (`read_dir`, twice) rather than a hardcoded list, so a new fixture is covered the day it lands
/// instead of the day someone remembers to register it.
fn fixture_test_files() -> Vec<(String, String)> {
    let mut found = Vec::new();
    let mut dirs: Vec<_> = std::fs::read_dir("tests")
        .expect("tests/ must be readable from the repo root")
        .map(|e| e.expect("tests/ entry").path())
        .filter(|p| p.is_dir())
        .collect();
    dirs.sort();
    for dir in dirs {
        let dir_name = dir
            .file_name()
            .and_then(|n| n.to_str())
            .expect("fixture dir name")
            .to_owned();
        let mut files: Vec<_> = std::fs::read_dir(&dir)
            .unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()))
            .map(|e| e.expect("fixture dir entry").path())
            .filter(|p| {
                p.is_file()
                    && p.file_name()
                        .and_then(|n| n.to_str())
                        .is_some_and(|n| n.starts_with("tests") && n.ends_with(".rs"))
            })
            .collect();
        files.sort();
        for file in files {
            let name = file
                .file_name()
                .and_then(|n| n.to_str())
                .expect("fixture file name")
                .to_owned();
            let src = std::fs::read_to_string(&file)
                .unwrap_or_else(|e| panic!("reading {}: {e}", file.display()));
            found.push((format!("{dir_name}/{name}"), src));
        }
    }
    found
}

/// The match rule, stated exactly because the baseline above is only as honest as this function:
///
/// 1. `//` line comments are removed (from the first `//` on a line to its end), so a site that is
///    only MENTIONED in prose does not count.
/// 2. Every occurrence of the literal `.is_err()` is a candidate.
/// 3. A candidate's enclosing STATEMENT is the text back to the nearest preceding `;`, `{` or `}`.
/// 4. The candidate counts iff that text contains `from_cbor_bytes`.
///
/// Statement-scoped rather than line-scoped on purpose: these files are rustfmt-shaped, so the same
/// site is spelled on one line or wrapped across four depending only on how long the byte vector
/// is, and a line-scoped count would drop a site for free the moment a vector grew.
///
/// Two deliberate bounds. A decode reached through a local closure or helper — the closure body
/// holds the `from_cbor_bytes`, the `is_err()` is on the CALL — is not a same-statement site and is
/// not counted. And `.is_err()` on something that is not a decode at all (a constructor range
/// check) is not counted, which is the point: those assert a `Result`-returning constructor, not a
/// wire boundary.
fn count_bare_decode_reject_sites(src: &str) -> usize {
    // A block comment would let a commented-out site count (rule 1 only strips line comments), and
    // no fixture test file uses one. Fail loudly rather than silently mis-measuring if that changes.
    assert!(
        !src.contains("/*"),
        "this counter only strips `//` line comments; a block comment needs the rule (and this \
         assert) extended"
    );
    let stripped: String = src
        .lines()
        .map(|line| match line.find("//") {
            Some(at) => &line[..at],
            None => line,
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut count = 0;
    let mut from = 0;
    while let Some(rel) = stripped[from..].find(".is_err()") {
        let at = from + rel;
        let statement_start = stripped[..at].rfind([';', '{', '}']).map_or(0, |i| i + 1);
        if stripped[statement_start..at].contains("from_cbor_bytes") {
            count += 1;
        }
        from = at + ".is_err()".len();
    }
    count
}

/// The counter's own rule, pinned on synthetic input — otherwise the baseline could drift with a
/// silently-changed match rule and nothing would notice.
#[test]
fn bare_decode_reject_counter_matches_its_documented_rule() {
    // one line, one site
    assert_eq!(
        count_bare_decode_reject_sites(r#"assert!(Foo::from_cbor_bytes(&b).is_err());"#),
        1
    );
    // wrapped across lines — same statement, still one site
    assert_eq!(
        count_bare_decode_reject_sites(
            "assert!(Foo::from_cbor_bytes(\n    &[a, b].concat()\n)\n.is_err());"
        ),
        1
    );
    // the discriminated form is not a site
    assert_eq!(
        count_bare_decode_reject_sites(
            r#"assert_decode_reject_reason::<Foo>(&b, "not enough bytes");"#
        ),
        0
    );
    // a prose mention is not a site
    assert_eq!(
        count_bare_decode_reject_sites("// Foo::from_cbor_bytes(&b).is_err() would be bare"),
        0
    );
    // a decode in a PRIOR statement does not make the next `is_err()` a site
    assert_eq!(
        count_bare_decode_reject_sites(
            "let f = |b| Foo::from_cbor_bytes(b);\nassert!(f(&x).is_err());"
        ),
        0
    );
    // an `is_err()` that is not a decode at all is not a site
    assert_eq!(
        count_bare_decode_reject_sites("assert!(Hash::new(vec![0x00]).is_err());"),
        0
    );
    // two sites in one file
    assert_eq!(
        count_bare_decode_reject_sites(
            "assert!(Foo::from_cbor_bytes(&a).is_err());\nassert!(Bar::from_cbor_bytes(&b).is_err());"
        ),
        2
    );
}

/// Every fixture test file's undiscriminated decode-reject count must equal its baseline exactly.
#[test]
fn fixture_bare_decode_rejects_hold_at_their_ratchet_baseline() {
    let baseline: BTreeMap<&str, usize> = BARE_DECODE_REJECT_BASELINE.iter().copied().collect();
    assert_eq!(
        baseline.len(),
        BARE_DECODE_REJECT_BASELINE.len(),
        "BARE_DECODE_REJECT_BASELINE has a duplicate key"
    );
    let files = fixture_test_files();
    assert!(
        !files.is_empty(),
        "found no tests/*/tests*.rs — the enumeration is broken, not the fixtures"
    );

    let mut problems = Vec::new();
    let mut seen: BTreeMap<&str, usize> = BTreeMap::new();
    for (path, src) in &files {
        let count = count_bare_decode_reject_sites(src);
        let expected = baseline.get(path.as_str()).copied().unwrap_or(0);
        if count > expected {
            problems.push(format!(
                "tests/{path}: {count} same-statement `from_cbor_bytes(..)…is_err()` sites, \
                 baseline {expected} — a new bare decode-reject site passes for ANY failure. Use \
                 the reason helper: `assert_decode_reject_reason::<T>(&bytes, \"<substring of the \
                 message the vector's own boundary produces>\")`, copying the helper into this \
                 fixture's tests.rs if it has none yet."
            ));
        } else if count < expected {
            problems.push(format!(
                "tests/{path}: {count} same-statement `from_cbor_bytes(..)…is_err()` sites, \
                 baseline {expected} — the ratchet moved. Lower this file's row in \
                 BARE_DECODE_REJECT_BASELINE to {count} in the same commit (delete the row if \
                 {count} is 0)."
            ));
        }
        if let Some(key) = baseline.get_key_value(path.as_str()).map(|(k, _)| *k) {
            *seen.entry(key).or_default() += 1;
        }
    }
    for (path, _) in BARE_DECODE_REJECT_BASELINE {
        if !seen.contains_key(path) {
            problems.push(format!(
                "BARE_DECODE_REJECT_BASELINE names tests/{path}, which no longer exists — the \
                 ratchet moved. Delete the row in the same commit."
            ));
        }
    }

    assert!(
        problems.is_empty(),
        "the fixture decode-reject ratchet moved:\n  {}",
        problems.join("\n  ")
    );
}
