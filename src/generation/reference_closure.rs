//! The reference-closure fixpoint shared by the two finalized-IR projections, [`super::wit`] and
//! [`super::extern_interface`].
//!
//! Both stage every item that projects on its own, then must drop any staged item whose projected
//! body names something the output will not define, or the emitted file dangles. The loop is
//! written once here because the exclusion reason (`references excluded <root>`) is emitted text on
//! both faces, and two copies of the scan could drift in iteration order and so in which root a
//! transitively-excluded item names.

use std::collections::{BTreeMap, BTreeSet};

/// Move every item of `staged` whose references escape it into `excluded`, to fixpoint.
///
/// Each round takes the FIRST staged key (`BTreeMap` order) that has a reference which is neither
/// staged nor `resolved_outside`, choosing that key's first such reference in `BTreeSet` order. The
/// chain root is that reference's own recorded root if it is already excluded (`root_of`), and
/// `fallback_root` otherwise; `exclude` turns the removed item, the reason
/// (`references excluded <root>`, the text both faces emit) and the root into its exclusion record.
/// The scan restarts from the first key after every move, so a transitively-excluded item names
/// the ROOT of its chain rather than its immediate neighbour. Monotone (items only leave `staged`),
/// so it terminates.
pub(super) fn exclude_dangling_refs<K: Ord + Clone, S, E>(
    staged: &mut BTreeMap<K, S>,
    excluded: &mut BTreeMap<K, E>,
    refs: impl Fn(&S) -> &BTreeSet<K>,
    resolved_outside: impl Fn(&K) -> bool,
    root_of: impl Fn(&E) -> &str,
    fallback_root: impl Fn(&K) -> String,
    mut exclude: impl FnMut(S, String, String) -> E,
) {
    loop {
        let next = staged.iter().find_map(|(key, item)| {
            refs(item)
                .iter()
                .find(|r| !staged.contains_key(*r) && !resolved_outside(r))
                .map(|r| {
                    let root = excluded
                        .get(r)
                        .map(|e| root_of(e).to_owned())
                        .unwrap_or_else(|| fallback_root(r));
                    (key.clone(), root)
                })
        });
        let Some((key, root)) = next else {
            break;
        };
        let item = staged.remove(&key).expect("just found in the same map");
        let record = exclude(item, format!("references excluded {root}"), root);
        excluded.insert(key, record);
    }
}

#[cfg(test)]
mod tests {
    use super::exclude_dangling_refs;
    use std::collections::{BTreeMap, BTreeSet};

    /// Staged rows are `(key, refs)` and pre-excluded rows are `(key, root)`.
    /// Exclusion records are `(reason, root)`.
    fn run(
        staged: &[(&'static str, &[&'static str])],
        excluded: &[(&'static str, &'static str)],
        outside: &[&'static str],
    ) -> (Vec<&'static str>, BTreeMap<&'static str, (String, String)>) {
        let mut staged: BTreeMap<&str, BTreeSet<&str>> = staged
            .iter()
            .map(|(k, refs)| (*k, refs.iter().copied().collect()))
            .collect();
        let mut excluded: BTreeMap<&str, (String, String)> = excluded
            .iter()
            .map(|(k, root)| (*k, ("direct".to_owned(), (*root).to_owned())))
            .collect();
        exclude_dangling_refs(
            &mut staged,
            &mut excluded,
            |refs| refs,
            |r| outside.contains(r),
            |(_, root)| root,
            |r| format!("fallback:{r}"),
            |_, reason, root| (reason, root),
        );
        (staged.into_keys().collect(), excluded)
    }

    #[test]
    fn chain_names_the_root_not_the_neighbour() {
        // a -> b -> c, c excluded directly: both a and b name c, whatever order the scan meets
        // them.
        let (kept, excluded) = run(
            &[("a", &["b"]), ("b", &["c"]), ("d", &[])],
            &[("c", "c")],
            &[],
        );
        assert_eq!(kept, ["d"]);
        assert_eq!(excluded["a"].0, "references excluded c");
        assert_eq!(excluded["b"].0, "references excluded c");
    }

    #[test]
    fn resolved_outside_references_do_not_exclude() {
        let (kept, excluded) = run(&[("a", &["imported"])], &[], &["imported"]);
        assert_eq!(kept, ["a"]);
        assert!(excluded.is_empty());
    }

    #[test]
    fn an_unrecorded_reference_takes_the_fallback_root() {
        let (kept, excluded) = run(&[("a", &["gone"]), ("b", &["a"])], &[], &[]);
        assert!(kept.is_empty());
        assert_eq!(excluded["a"].1, "fallback:gone");
        assert_eq!(excluded["b"].1, "fallback:gone");
    }

    #[test]
    fn first_dangling_reference_in_set_order_picks_the_root() {
        // Two excluded references with different roots: the smaller key (`x`) decides.
        let (_, excluded) = run(&[("a", &["y", "x"])], &[("x", "rx"), ("y", "ry")], &[]);
        assert_eq!(excluded["a"].1, "rx");
    }
}
