//! In-bin unit tests for the `static/ordered_set.rs` runtime, hosted via the same
//! `include!`-a-static-file pattern as `any_cbor_tests` (the runtime ships into generated crates,
//! so nothing else in THIS crate compiles it).
//!
//! What these pin: `scan_unique` is a hybrid — a linear `contains` scan below
//! `SORTED_SCAN_MIN_LEN`, a sorted-index scan at/above it — and BOTH paths must report the
//! identical `DuplicateKey(Key::Uint(i))` outcome as the original linear scan: `i` is the
//! scan-order-FIRST second occurrence. The linear reference scan below is the oracle; every case
//! battery runs vectors on both sides of the threshold so a regression in either path (or in the
//! sorted path's index reconstruction) fails here without needing a generated-crate e2e run.

// The included runtime files legitimately trigger lints a generated crate silences at crate level:
// `dead_code` (only the doors and accessors are exercised here) and `upper_case_acronyms`
// (`error.rs`'s `CBOR` variant).
#[allow(dead_code, clippy::upper_case_acronyms)]
mod error {
    include!("../../static/error.rs");
}
#[allow(dead_code)]
mod ordered_set {
    include!("../../static/ordered_set.rs");
}

use error::{DeserializeFailure, Key};
use ordered_set::{NonEmptyOrderedSet, OrderedSet};

/// The original linear scan, kept verbatim as the behavioral oracle: the index of the
/// scan-order-first element that repeats an earlier one.
fn oracle<T: PartialEq>(vec: &[T]) -> Option<u64> {
    (0..vec.len())
        .find(|&i| vec[..i].contains(&vec[i]))
        .map(|i| i as u64)
}

/// Run a vector through the `OrderedSet` uniqueness door, mapping the outcome to the oracle's
/// shape (`None` = accepted, `Some(i)` = refused with `DuplicateKey(Uint(i))`).
fn door<T: Ord + Clone + std::fmt::Debug>(vec: &[T]) -> Option<u64> {
    match OrderedSet::try_from(vec.to_vec()) {
        Ok(set) => {
            // An accepted set must hold the input untouched — the sorted path works on an index
            // side-table and may never reorder the accepted elements.
            assert_eq!(
                set.as_slice(),
                vec,
                "accepted set must preserve input order"
            );
            None
        }
        Err(e) => match e.failure() {
            DeserializeFailure::DuplicateKey(Key::Uint(i)) => Some(*i),
            other => panic!("expected DuplicateKey(Uint), got {other:?}"),
        },
    }
}

fn assert_door_matches_oracle<T: Ord + Clone + std::fmt::Debug>(vec: &[T], label: &str) {
    assert_eq!(
        door(vec),
        oracle(vec),
        "door/oracle disagree for {label}: {vec:?}"
    );
}

/// Hand-picked shapes on BOTH sides of the hybrid threshold (32): the small ones exercise the
/// linear path, the >=32 ones the sorted path, and the padded variants plant the same duplicate
/// patterns into large vectors so the sorted path's index reconstruction is checked against the
/// oracle for first-pair, last-pair, multi-duplicate, and equal-run(>2) shapes.
#[test]
fn uniqueness_door_agrees_with_linear_oracle() {
    let small: Vec<Vec<u32>> = vec![
        vec![],
        vec![7],
        vec![1, 2, 3],
        vec![5, 5],
        vec![1, 2, 3, 2, 1],
        vec![7, 1, 7, 1],
        vec![9, 9, 9, 9],
    ];
    for v in &small {
        assert_door_matches_oracle(v, "small (linear path)");
    }
    // Pad each pattern with unique filler (1000..) to push it over the threshold, at three
    // placements: pattern first, pattern last, pattern split around the filler.
    for v in &small {
        let filler: Vec<u32> = (1000..1040).collect();
        let mut lead = v.clone();
        lead.extend(&filler);
        assert_door_matches_oracle(&lead, "pattern-first (sorted path)");
        let mut trail = filler.clone();
        trail.extend(v);
        assert_door_matches_oracle(&trail, "pattern-last (sorted path)");
        if v.len() >= 2 {
            let (a, b) = v.split_at(v.len() / 2);
            let mut split = a.to_vec();
            split.extend(&filler);
            split.extend(b);
            assert_door_matches_oracle(&split, "pattern-split (sorted path)");
        }
    }
    // Threshold boundary: a duplicate-of-first-at-end vector at exactly len 31, 32, 33.
    for n in [31usize, 32, 33] {
        let mut v: Vec<u32> = (0..(n as u32 - 1)).collect();
        v.push(0);
        assert_door_matches_oracle(&v, "boundary dup-of-first-at-end");
        let u: Vec<u32> = (0..n as u32).collect();
        assert_door_matches_oracle(&u, "boundary all-unique");
    }
}

/// Deterministic pseudo-random battery over a SMALL value domain (dense collisions) at sizes
/// spanning the threshold — the shapes hand-picked cases miss.
#[test]
fn uniqueness_door_agrees_with_linear_oracle_randomized() {
    let mut state = 0x243F_6A88_85A3_08D3u64;
    let mut next = move || {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        state
    };
    for n in [8usize, 31, 32, 33, 100, 300] {
        for _ in 0..50 {
            // domain ~n so roughly half the draws contain a duplicate
            let v: Vec<u64> = (0..n).map(|_| next() % (n as u64)).collect();
            assert_door_matches_oracle(&v, "randomized");
        }
    }
}

/// The non-empty twin's door composes min-1 with the SAME scan: an empty vec is `RangeCheck`, and a
/// duplicate reports the identical index as the `OrderedSet` door on both scan paths.
#[test]
fn non_empty_twin_door_matches() {
    let empty: Vec<u32> = vec![];
    match NonEmptyOrderedSet::try_from(empty).unwrap_err().failure() {
        DeserializeFailure::RangeCheck {
            found: 0,
            min: Some(1),
            ..
        } => {}
        other => panic!("expected min-1 RangeCheck, got {other:?}"),
    }
    for n in [5usize, 100] {
        let mut v: Vec<u32> = (0..(n as u32 - 1)).collect();
        v.push(3); // duplicate of index 3, second occurrence at n-1
        let expected = oracle(&v);
        match NonEmptyOrderedSet::try_from(v).unwrap_err().failure() {
            DeserializeFailure::DuplicateKey(Key::Uint(i)) => {
                assert_eq!(Some(*i), expected, "non-empty twin index at n={n}")
            }
            other => panic!("expected DuplicateKey(Uint), got {other:?}"),
        }
    }
}
