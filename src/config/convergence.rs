//! Consumed-sidecar observations and committed-state verdict.

use super::derive::{normalized, resolve_path};
use super::{Config, CrateEntry, list_or_none};
use crate::cli::Cli;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The convergence check: whether a sidecar this run CONSUMED was rewritten by the same run.
///
/// The two edge kinds want opposite generation orders. `--extern-import`/`--extern-wrapper-index`
/// want the dependency first; `--wrapper-requests`/`--key-requests` want the consumer first. No
/// single pass satisfies both, and this is not a defect to engineer away — both flags document their
/// input as the OTHER crate's *committed* output. Generation order resolves it in the dependency's
/// favour, which leaves the reverse edges reading last run's sidecars.
///
/// So this records each consumed sidecar's bytes BEFORE the run and compares them after. A change
/// means the crate that read it generated against a stale one and is now a run behind.
///
/// [`super::run::generate`] asks that question twice, for two different purposes. Around the first pass it is
/// the TRIGGER for [`super::run::generate`]'s convergence pass — the crates it names are exactly the ones re-run,
/// which is what settles a cold invocation without a second command. Around that pass it is the
/// residual DIAGNOSTIC: a sidecar still moving afterwards would be a feedback path the fixed pass
/// did not bound, and the warning is what says so. It changes no output byte in either role; what it
/// decides is which crates run again, never what any of them generates.
///
/// What it measures is the SIDECAR channel, in both roles and across every crate in the run. The
/// other cross-crate channel — a dependency's `collections.rs` wrapper index, which the convergence
/// pass legitimately rewrites, since hosting the requested wrappers is the whole point of the pass —
/// is deliberately NOT watched: an instrument that fired on every successful convergence would
/// measure nothing. That channel is bounded by the argument at [`super::run::generate`]'s convergence pass
/// instead.
pub struct Convergence {
    /// `(the crate that read it, the sidecar's path, its bytes before the run)`. `None` for bytes
    /// means the file did not exist — a workspace whose consumer has never generated, which converges
    /// the same way any other change does.
    entries: Vec<(String, PathBuf, Option<Vec<u8>>)>,
}

impl Convergence {
    /// Snapshot every sidecar the expanded invocations will consume.
    ///
    /// Read off the expanded `Cli`s rather than off the `deps` edges, so a hand-written
    /// `[crates.<n>.wrapper-requests]` entry pointing at a crate in this run is watched exactly like
    /// a derived one. A sidecar whose owner is NOT in this run cannot change, so it never fires and
    /// needs no special case.
    ///
    /// There is deliberately no restricted form. Both of [`super::run::generate`]'s two questions watch EVERY
    /// consumed sidecar, including those of crates the convergence pass does not re-run: a sidecar
    /// moving under a crate that was not re-run is precisely the feedback path the one-pass argument
    /// claims is unreachable, so an instrument narrowed to the re-run crates could not see the thing
    /// it exists to measure.
    pub fn capture(expanded: &[(String, Cli)]) -> Self {
        let mut entries = Vec::new();
        for (name, cli) in expanded {
            let consumed = cli
                .wrapper_requests()
                .into_values()
                .chain(cli.key_requests().into_values());
            for path in consumed {
                let path = PathBuf::from(path);
                let before = std::fs::read(&path).ok();
                entries.push((name.clone(), path, before));
            }
        }
        Self { entries }
    }

    /// The crates whose consumed sidecars differ now from when they read them.
    pub fn stale_crates(&self) -> BTreeSet<String> {
        self.stale_entries().map(|(name, _)| name.clone()).collect()
    }

    /// The crates this capture is WATCHING, whether or not their sidecars moved — the instrument's
    /// breadth, as opposed to [`Self::stale_crates`]'s findings.
    ///
    /// Exposed so a test can assert the residual check is not narrowed to the crates the convergence
    /// pass re-runs. On any graph two edges deep the two sets come apart (a middle crate can be
    /// re-run while the crate below it is not), and it is exactly there that a narrowed instrument
    /// would stop measuring the sidecar channel.
    ///
    /// Test-only API: the run itself never asks the question, so it is `#[cfg(test)]` rather than a
    /// shipped accessor with no shipped caller.
    #[cfg(test)]
    pub fn watched_crates(&self) -> BTreeSet<String> {
        self.entries
            .iter()
            .map(|(name, _, _)| name.clone())
            .collect()
    }

    /// `(the crate that read it, the sidecar that moved under it)` for every changed entry — what
    /// the convergence pass prints, so a second generation of the same crate is never silent about
    /// the file that caused it.
    pub(super) fn stale_entries(&self) -> impl Iterator<Item = (&String, &PathBuf)> {
        self.entries
            .iter()
            .filter(|(_, path, before)| std::fs::read(path).ok() != *before)
            .map(|(name, path, _)| (name, path))
    }

    /// The per-crate lines the convergence pass prints before re-running, in crate order.
    pub fn rerun_notes(&self) -> Vec<String> {
        let mut by_crate: BTreeMap<&String, BTreeSet<&PathBuf>> = BTreeMap::new();
        for (name, path) in self.stale_entries() {
            by_crate.entry(name).or_default().insert(path);
        }
        by_crate
            .into_iter()
            .map(|(name, paths)| {
                let files: Vec<String> = paths.iter().map(|p| p.display().to_string()).collect();
                format!(
                    "[converge] re-running `{name}`: it read {} before this run rewrote {} \
                     ({}), so what it generated is a pass behind what its consumers now ask for.",
                    if files.len() == 1 {
                        "a sidecar"
                    } else {
                        "sidecars"
                    },
                    if files.len() == 1 { "it" } else { "them" },
                    files.join(", "),
                )
            })
            .collect()
    }

    /// The residual warning: a sidecar that moved AGAIN across the convergence pass, which the pass
    /// therefore did not settle. `None` when the run converged.
    pub fn warning(&self, config_path: &Path, selected: &[String]) -> Option<String> {
        let stale = self.stale_crates();
        if stale.is_empty() {
            return None;
        }
        let names: Vec<&str> = stale.iter().map(String::as_str).collect();
        let mut command = format!("cddl-codegen --config {}", config_path.display());
        for name in selected {
            command.push(' ');
            command.push_str(name);
        }
        Some(format!(
            "warning: a sidecar changed during this run, so {} generated against a stale one \
             ({}). The sidecars a dependency reads are its consumers' COMMITTED output, and this \
             run rewrote one of them after the dependency had already read it. Re-run `{command}` \
             to converge.",
            list_or_none(stale.iter()),
            if names.len() == 1 {
                "it is one run behind".to_owned()
            } else {
                "they are one run behind".to_owned()
            },
        ))
    }
}

impl Config {
    /// The committed-state convergence VERDICT: does the workspace on disk, as it now stands, hold
    /// the collection wrappers its own sidecars ask for?
    ///
    /// # Why this exists beside [`Convergence`]
    ///
    /// They report different facts, and only one of them is a verdict. [`Convergence`] brackets the
    /// run: it says "I rewrote a sidecar something had already read, run me again" — an INSTRUCTION,
    /// legitimately expected on a first run, which re-running satisfies. It is therefore structurally
    /// blind to the case that matters most, because it watches only sidecars THIS RUN consumed:
    /// regenerating one crate of a workspace so that it borrows a new wrapper leaves the dependency
    /// not hosting it, and since the dependency was not in the run there was nothing to watch — the
    /// run prints nothing and exits 0 over a workspace that no longer builds.
    ///
    /// So this reads COMMITTED state instead, over every `deps` edge touching the selection —
    /// selected crates AND their config-declared counterparties. Restricting it to the run's own
    /// crates would leave it silent for exactly the reason the bracketing check already is. What it
    /// asserts is a property of the tree, not of the run: every row of a consumer's committed
    /// `borrowed_collections.rs` compiles to a `use <dep>_wasm::collections::<Name>;` line, so a name
    /// the dependency's committed `collections.rs` index does not re-export is a workspace that does
    /// not build — whatever is run next. That is why it is a nonzero exit and the bracketing warning
    /// is not: an instruction about the run is not a verdict about the tree.
    ///
    /// # It is diagnostic-only
    ///
    /// This reads generated output, so it is bounded exactly as the other diagnostic reads in
    /// `docs/development/generation-contract.md` are: it runs after every file is written, it changes NO generated byte, and nothing it finds
    /// feeds back into what is generated. It is not a prior-output dependence of the generator —
    /// delete it and every emitted file is identical. The only thing it changes is the exit code.
    ///
    /// The scan is deliberately LENIENT about content it cannot read: an absent sidecar borrows
    /// nothing, an absent or hand-mangled index contributes what it can, and a malformed row is not
    /// counted. Under-reading costs a missed verdict; over-reading would cost a build failure this
    /// check has no standing to assert, and a verdict that cries wolf is worse than the silence it
    /// replaces. The strict grammar owner stays `emit_requested_collections`, which the dependency's
    /// own run reaches.
    pub fn committed_verdict(
        &self,
        config_path: &Path,
        selected: &[String],
    ) -> Result<Option<String>, String> {
        let ungraphed = self.ungraphed()?;
        let runtime_choice = self.runtime_carrier(&ungraphed)?;
        // Both paths live in a crate's GRAPHED settings, which is also what makes a hand-written
        // `[crates.<n>.wrapper-requests]` / `.extern-wrapper-index` override honoured: the check
        // reads whatever file the run itself would have read, never a path re-guessed here.
        //
        // Literally the run's own pipeline ([`Self::graphed_settings`]), not a re-derivation of it —
        // including the `[runtime]` fold, which touches neither of these two keys today and which
        // this check therefore does not depend on having. That is the point of sharing the body: it
        // does not have to depend on it.
        let graphed = |name: &str, entry: &CrateEntry| {
            self.graphed_settings(name, entry, &ungraphed, runtime_choice.as_ref())
                .0
        };

        let mut missing: BTreeMap<&String, BTreeMap<&String, BTreeSet<String>>> = BTreeMap::new();
        for (consumer_name, consumer) in &self.crates {
            for dep_name in &consumer.deps {
                // The selection filter, and the whole reason this sees the subset case: an edge is
                // examined when EITHER end is in the run, so regenerating the consumer alone still
                // checks the dependency whose demands it just changed.
                if !selected.is_empty()
                    && !selected.contains(consumer_name)
                    && !selected.contains(dep_name)
                {
                    continue;
                }
                let dep = &self.crates[dep_name];
                let consumer_label = normalized(&consumer.lib_name);
                let dep_label = normalized(&dep.lib_name);
                // Absent on an edge whose either side generates no wasm crate: then no sidecar is
                // written and no index exists, so there is nothing to be inconsistent about.
                let (Some(sidecar), Some(index)) = (
                    graphed(dep_name, dep)
                        .wrapper_requests
                        .remove(&consumer_label),
                    graphed(consumer_name, consumer)
                        .extern_wrapper_index
                        .remove(&dep_label),
                ) else {
                    continue;
                };
                // A sidecar that was never written records "borrows nothing", which is not an error.
                let Ok(contents) = std::fs::read_to_string(resolve_path(&self.base_dir, &sidecar))
                else {
                    continue;
                };
                // A dependency that has never generated provides nothing — which is exactly what the
                // consumer's unresolvable `use` lines already say about it.
                let provided: BTreeSet<String> =
                    std::fs::read_to_string(resolve_path(&self.base_dir, &index))
                        .map(|text| collection_index_names(&text))
                        .unwrap_or_default();
                for row in crate::wrapper_requests::scan_borrowed_rows_lenient(&contents) {
                    // A sidecar can name several dependencies; only this edge's rows are this
                    // dependency's to satisfy.
                    if normalized(&row.dep) != dep_label || provided.contains(&row.name) {
                        continue;
                    }
                    missing
                        .entry(dep_name)
                        .or_default()
                        .entry(consumer_name)
                        .or_default()
                        .insert(row.name);
                }
            }
        }
        if missing.is_empty() {
            return Ok(None);
        }

        let clauses: Vec<String> = missing
            .iter()
            .map(|(dep, by_consumer)| {
                let borrows = by_consumer
                    .iter()
                    .map(|(consumer, names)| {
                        format!("{} borrowed by `{consumer}`", list_or_none(names.iter()))
                    })
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("`{dep}` does not host {borrows}")
            })
            .collect();
        // The crates to re-run are the DEPENDENCIES, named: the party that knows the graph is the
        // party that should say what settles it, and a dependency-alone regen is always safe.
        let mut command = format!("cddl-codegen --config {}", config_path.display());
        for dep in missing.keys() {
            command.push(' ');
            command.push_str(dep);
        }
        Ok(Some(format!(
            "the committed workspace does not build: {}. Every row of a consumer's committed \
             `borrowed_collections.rs` compiles to a `use <dep>_wasm::collections::<Name>;` line, \
             and the dependency's committed `collections.rs` index does not re-export that name. \
             This is a verdict about the tree as it stands rather than about what this run changed, \
             and is reported whether or not the dependency was in this run. Run `{command}` to host \
             them: a dependency-alone regen reads its consumers' committed sidecars, and is always \
             safe.",
            clauses.join(", "),
        )))
    }
}

/// Every wrapper class a dependency's committed `collections.rs` index re-exports.
///
/// Lines the index grammar does not recognize are skipped rather than refused: the strict reader of
/// this file is `load_extern_wrapper_indices`, which the consumer's own run reaches with a hard error
/// — a second, differently-worded rejection from a post-run diagnostic would help nobody.
fn collection_index_names(text: &str) -> BTreeSet<String> {
    use crate::wrapper_requests::{CollectionIndexLine, classify_collection_index_line};
    text.lines()
        .filter_map(|line| match classify_collection_index_line(line) {
            CollectionIndexLine::Export(name) => Some(name),
            CollectionIndexLine::Ignored | CollectionIndexLine::Unknown => None,
        })
        .collect()
}
