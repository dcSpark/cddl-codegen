//! Config invocation orchestration and command-line config surface.

use super::{Config, Convergence, VerdictError, load};
use crate::log::Verbosity;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

impl Config {
    /// The RUN level: the command-line `--verbosity` if given, else `[defaults].verbosity`, else the
    /// built-in default. Shared by [`generate`] and [`print_flags`].
    pub(super) fn run_verbosity(&self) -> Verbosity {
        self.verbosity_override
            .or(self.defaults.verbosity)
            .unwrap_or_default()
    }
}

/// [`load_with`] then [`generate_loaded`].
// The bin target uses `generate_loaded`; library callers and tests also use this path wrapper.
#[allow(dead_code)]
pub fn generate(
    config_path: &Path,
    selected: &[String],
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<(), Box<dyn std::error::Error>> {
    generate_loaded(
        &load_with(config_path, static_dir, verbosity)?,
        config_path,
        selected,
    )
}

/// Run everything a config file describes: expand it, generate each crate in order, then report
/// whether the run converged.
///
/// Expansion happens up front, so every value AND every flag combination is validated before ANY
/// crate generates — a typo in the last crate's table must not leave the first crate's output
/// half-migrated on disk.
pub fn generate_loaded(
    config: &Config,
    config_path: &Path,
    selected: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    // The RUN level, installed before anything is emitted: the command line if it gave one, else
    // `[defaults].verbosity`, else the built-in default.
    //
    // Everything THIS function prints — the `[runtime]` notes, the per-crate `[name] generating …`
    // banner, the `[converge]` re-run notes, the residual convergence warning — is run-level output
    // and runs at this level, unaffected by whichever crate generated last: each crate's own
    // generation installs its own level under a guard that restores this one on exit
    // (`api::generate_to_disk`).
    //
    // Hence one asymmetry, which is the reading that follows from the existing merge model rather
    // than a special case: a `[profiles.*]` or `[crates.*]` verbosity governs only that crate's own
    // generation, and only `[defaults]` moves these run-level lines. `[defaults]` is defined as the
    // value that reaches every crate, and the run is what contains every crate.
    let _run_verbosity = crate::log::scoped(config.run_verbosity());
    let expanded = config
        .expand(selected)
        .map_err(|e| about_the_config(config_path, e))?;
    // Stated before the first crate generates: which crate carries the shared runtime, and what the
    // choice accepted. Silently choosing is what the hand-placed flag already does.
    if let Some(choice) = config
        .runtime_report()
        .map_err(|e| about_the_config(config_path, e))?
    {
        // The export rides the CARRIER's invocation, so a subset that leaves the carrier out does
        // not refresh the runtime — and the notes are written in the present tense. Say which run
        // this is, or the line claims a write that is not happening: the crates in the subset are
        // still pointed at the runtime directory by `--common-import-override`, and on a workspace
        // where it has never been written that is a crate that cannot build.
        if expanded.iter().any(|(name, _)| name == &choice.carrier) {
            for note in &choice.notes {
                crate::note!("{note}");
            }
        } else {
            crate::note!(
                "[runtime] `{}` carries --export-static-crate and is not in this run, so the \
                 runtime is NOT refreshed here — the committed one is used as it stands. Run \
                 without a crate selection, or name `{}`, to refresh it.",
                choice.carrier,
                choice.carrier
            );
        }
    }
    // What this run has already rewritten on disk, in the order it rewrote it — the whole of what a
    // mid-run failure has to report (see [`mid_run_failure`]). Carried ACROSS the passes because the
    // convergence pass is part of the same run: a failure there has the first pass's crates behind it
    // too. A crate re-run by that pass is not listed twice; it is one directory either way.
    let mut regenerated: Vec<String> = Vec::new();
    let mut generate_pass =
        |names: Option<&BTreeSet<String>>| -> Result<(), Box<dyn std::error::Error>> {
            for (name, cli) in &expanded {
                if names.is_some_and(|names| !names.contains(name)) {
                    continue;
                }
                // A per-crate banner, NOT a per-line prefix: the generator's progress output is consumed
                // as-is by humans and by tests, so this adds a line rather than rewriting the existing
                // ones.
                crate::note!(
                    "\n[{name}] generating from {} into {}",
                    cli.input.display(),
                    cli.output.display()
                );
                crate::api::generate_to_disk(cli)
                    .map_err(|e| mid_run_failure(name, &e, &regenerated))?;
                if !regenerated.iter().any(|done| done == name) {
                    regenerated.push(name.clone());
                }
            }
            Ok(())
        };

    let first = Convergence::capture(&expanded);
    generate_pass(None)?;

    // The convergence pass. One extra pass, over exactly the crates whose consumed sidecars this run
    // rewrote, in the same generation order — and then the run is settled.
    //
    // ONE pass rather than a loop to a fixpoint, because a second one provably has nothing to do.
    // The only cross-crate input whose content can change here is a dependency's `collections.rs`
    // wrapper index, and a consumer's output depends on that index through exactly one decision: the
    // OWNERLESS collection-wrapper deferral (`generation::collections::try_defer_wrapper`), since
    // every `deps` edge also derives `--workspace-dep` and an all-one-dep wrapper therefore defers
    // without consulting any index. What this pass adds to a dependency's index is the wrappers its
    // consumers requested, and a requested wrapper is by construction all-one-dep — it names one of
    // the dependency's own types, so it is never an ownerless name. (A requested shape nesting an
    // ownerless collection is a hard error in the dependency's own run, not a silent index
    // addition.) The consumer's other cross-crate input, the `extern-interface/<dep>/**` export, is
    // a pure projection of the dependency's own finalized IR and carries none of the request
    // channel's demands — so the sidecars themselves, being a function of a crate's spec and its
    // dependencies' exports, are already final after the first pass and this one cannot make a new
    // crate stale. Those same two inputs are also exactly what decides WHICH rules of an export a
    // consumer imports (`extern_narrow`: the consumer's own spec references, closed over the export's
    // own bodies), so the narrowing is covered by this argument rather than being a third input to
    // it — nothing this pass adds to a dependency can change what a consumer needs from it.
    //
    // The argument is bounded by export NON-TRANSITIVITY, and it is worth naming the invariant it
    // rests on rather than leaving it implicit: "a dependency's own deps never travel through its
    // export" (`docs/docs/integration-other.mdx`, the extern-import chapter's closing statement). It
    // is what keeps "a crate's sidecars are a function of its own spec and its DIRECT dependencies'
    // exports" true no matter how deep the `deps` graph runs — a chain `app → mid → core` re-runs
    // `mid` in this pass, and `mid`'s own sidecars cannot move because nothing `core` gained here
    // reaches `app` through `mid`'s export. Make exports transitive and this argument is the proof
    // that has been invalidated: a fixpoint loop would then be required, and this pass would be
    // exactly one iteration of it.
    // (Pinned end to end by `a_two_edge_dependency_chain_converges_in_one_invocation` and
    // `a_diamond_dependency_graph_converges_in_one_invocation`.)
    //
    // The residual convergence check below is what states that reasoning as a measurement rather
    // than an assumption: it is captured AROUND this pass — over EVERY crate's consumed sidecars,
    // not only the re-run crates' — so a sidecar that did move again would print the warning instead
    // of being assumed away. The unrestricted capture is what makes that true at depth ≥ 2, where a
    // re-run middle crate sits above a dependency that is not itself re-run.
    let stale = first.stale_crates();
    let residual = if stale.is_empty() {
        first
    } else {
        for note in first.rerun_notes() {
            crate::note!("{note}");
        }
        let residual = Convergence::capture(&expanded);
        generate_pass(Some(&stale))?;
        residual
    };

    if let Some(warning) = residual.warning(config_path, selected) {
        crate::warn!("{warning}");
    }
    // Both signals can fire on one run and they say different things, so neither replaces the other:
    // the warning above is an instruction about THIS run ("run me again"), which after the
    // convergence pass means a feedback path no single extra pass settles, and stays at exit 0; the
    // verdict below is about the TREE ("this does not build"), which no repeat of this command
    // settles. Only the second is a reason to fail. A full run should now trip neither.
    if let Some(verdict) = config
        .committed_verdict(config_path, selected)
        .map_err(|e| about_the_config(config_path, e))?
    {
        // The verdict itself is deliberately NOT wrapped by `about_the_config`. Every other message
        // here is about the config; this one is about the committed TREE, and it already names the
        // files it read. It IS wrapped in `VerdictError`, which changes no byte of the text and
        // carries the one thing the text cannot: the exit code `main` gives it.
        return Err(VerdictError(verdict).into());
    }
    Ok(())
}

/// Every diagnostic a config run produces names the config it came from.
///
/// [`load`] already prefixes what it returns, so parse-time errors carry it; this is the same prefix
/// for everything AFTER load — expansion, the runtime report, the committed-state read — which
/// otherwise reaches `main` as a bare sentence about a `[crates.<name>]` table without saying which
/// file holds that table. That matters most exactly where it is least visible: a repository with
/// several configs, or a wrapper script that picked the path.
///
/// Not applied to a per-crate GENERATION failure: that error is about a CDDL spec, and prefixing it
/// with a TOML path would name the wrong document. [`mid_run_failure`] is that error's own wrapper,
/// and it names the crate rather than the config for the same reason.
fn about_the_config(config_path: &Path, error: impl std::fmt::Display) -> String {
    format!("--config {}: {error}", config_path.display())
}

/// A per-crate generation failure, plus what the run had already rewritten when it happened.
///
/// Generation is not transactional across crates, and cannot be made so cheaply: each crate's output
/// is a committed directory the tool clobbers in place, so by the time the Nth crate fails, the N-1
/// before it are on disk in their new form. The bare error names a CDDL spec and nothing else, which
/// leaves the question a caller actually has — "what state is my tree in now?" — answerable only by
/// knowing the generation order and reading `git status`. The run knows the answer exactly; this is
/// where it says so.
///
/// It promises no more than that. The listed crates FINISHED regenerating; the failing crate's own
/// output may be partly written, since the failure can come from anywhere in its pass. The remedy is
/// not stated as a tool feature but as what committed output already gives you.
fn mid_run_failure(name: &str, error: impl std::fmt::Display, regenerated: &[String]) -> String {
    let already = if regenerated.is_empty() {
        "No crate finished regenerating before this failure".to_owned()
    } else {
        format!(
            "{} crate{} already regenerated in this run before this failure: {}",
            regenerated.len(),
            if regenerated.len() == 1 {
                " was"
            } else {
                "s were"
            },
            regenerated.join(", "),
        )
    };
    format!(
        "[crates.{name}] failed to generate: {error}\n{already}. Generation is not transactional \
         across crates: the crates ordered before this one are on disk in their regenerated form, \
         and `{name}`'s own output may be partly written. Generated output is committed, so \
         `git checkout` is the undo."
    )
}

/// `--with-deps`: resolve the command line's crate selection into the one the run uses.
///
/// Resolved HERE rather than inside [`generate`], because a closed selection is a plain list of crate
/// names — indistinguishable from a typed one, and it must be: the run, the `--print-flags` listing,
/// the convergence warning's "re-run this" command and the committed-state verdict all read the same
/// `selected`, and a closure applied inside only one of them would make them disagree about what the
/// run contained.
pub fn selection_with_deps(
    config: &Config,
    config_path: &Path,
    selected: &[String],
) -> Result<Vec<String>, String> {
    config
        .with_dependencies(selected)
        .map_err(|e| about_the_config(config_path, e))
}

/// [`load`] plus the two command-line overrides, neither of which is a config value and so neither of
/// which can be parsed from the document. `main` calls this once per invocation and passes the
/// loaded [`Config`] to selection, listing, and generation.
pub fn load_with(
    config_path: &Path,
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<Config, String> {
    let mut config = load(config_path)?;
    config.static_dir_override = static_dir.map(|p| p.to_string_lossy().into_owned());
    config.verbosity_override = verbosity;
    Ok(config)
}

/// [`load_with`] then [`print_flags_loaded`].
// The bin target uses `print_flags_loaded`; library callers and tests also use this path wrapper.
#[allow(dead_code)]
pub fn print_flags(
    config_path: &Path,
    selected: &[String],
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<(), Box<dyn std::error::Error>> {
    print_flags_loaded(
        &load_with(config_path, static_dir, verbosity)?,
        config_path,
        selected,
    )
}

/// `--print-flags`: state what a config expands to, and generate nothing.
///
/// The expansion is the same one [`generate`] performs — every path resolution, every derivation and
/// every validation — so a config that would abort a run aborts this with the identical message. The
/// only thing that does not happen is the writing: no crate generates, no file is read from any
/// output tree, and the run exits 0.
///
/// This is the only way to see what a config does short of running it: whether a `[defaults]` key
/// reaches a crate, or a `deps` edge derived the path you expected, is otherwise answerable only by
/// generating and reading the output tree.
pub fn print_flags_loaded(
    config: &Config,
    config_path: &Path,
    selected: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    // The run level, on the same `??` chain [`generate`] uses. The listing itself is the output of a
    // COMMAND rather than logging — like `--help`, it is never gated — but installing the level keeps
    // the two entry points saying the same thing about what this invocation's level is.
    let _run_verbosity = crate::log::scoped(config.run_verbosity());
    let listing = config
        .flag_listing(selected)
        .map_err(|e| about_the_config(config_path, e))?;
    // Deliberately an unconditional `print!` and NOT one of the `log` macros: this is the output of
    // a COMMAND, like `--help`, rather than logging. `--verbosity error` must not suppress the very
    // thing the invocation asked for.
    print!("{listing}");
    Ok(())
}

/// The config-mode command line: `cddl-codegen --config <file> [CRATE...]`.
///
/// A SEPARATE clap struct rather than a `--config` field on [`crate::cli::Cli`], because `Cli` makes
/// `--input`/`--output` required — a `Cli` that could also be a config invocation would have to make
/// them optional, which is the downstream-visible restructuring this feature is not allowed to do.
#[derive(Debug, clap::Parser)]
#[clap(
    about = "Generate every crate a cddl-codegen config file describes.",
    long_about = "Generate the crates a cddl-codegen config file describes.\n\nPaths inside the \
                  config resolve against the CONFIG FILE's directory, not the current one, so the \
                  same command works from anywhere. Naming crates limits the run to those crates; \
                  naming none runs them all. --with-deps adds what the named crates depend on."
)]
pub struct ConfigCli {
    /// Path to the config file.
    #[clap(long = "config", value_parser, value_name = "CONFIG_TOML")]
    pub config: PathBuf,

    /// Print the flags each crate would be generated with, and generate nothing.
    // Everything below the first paragraph is a `//` comment on purpose: clap renders a field's DOC
    // comment into `--help`, so a maintainer's note about which internal function this does not
    // collide with would be printed to every user asking what the flag does.
    //
    // Not a generation flag, so it does not collide with [`super::argv::reject_generation_flags`]: it changes
    // what the run DOES rather than what any crate is generated with, which is the same class as
    // the positional crate selector.
    #[clap(long = "print-flags", action = clap::ArgAction::SetTrue)]
    pub print_flags: bool,

    /// Where the hand-written serialization runtime is read from (overrides any `static-dir` key).
    // The ONE generation flag [`super::argv::reject_generation_flags`] lets through, and the exemption criterion
    // is visible in what it names: a checkout-local location of the TOOL's own inputs, not a
    // property of any crate. That makes it the one flag with no per-crate precedence question to
    // answer — it applies to every crate uniformly, which is why "does this apply to one crate or
    // all of them?" (the question that rules every other flag out) has an answer here. The
    // command-line value wins over a `static-dir` key silently: the key is a committed default and
    // this is the per-machine override of it, so reporting a conflict would report the intended use.
    // Both spellings, because the exemption is by ARG ID and so covers `Cli`'s `-s` as well as its
    // `--static-dir`: a short that passed the rejection only to be an unknown argument here would be
    // a worse error than the one it got through.
    #[clap(
        short = 's',
        long = "static-dir",
        value_parser,
        value_name = "STATIC_DIR"
    )]
    pub static_dir: Option<PathBuf>,

    /// Also generate everything the named crates depend on, transitively.
    // Not a generation flag, and so on the same side of [`super::argv::reject_generation_flags`] as the crate
    // names it modifies: it chooses WHICH crates run, never what any of them is generated with.
    //
    // Dependencies only, never consumers — see [`Config::with_dependencies`] for why the two
    // directions are not symmetric.
    #[clap(long = "with-deps", action = clap::ArgAction::SetTrue)]
    pub with_deps: bool,

    /// How much the run prints (overrides any `verbosity` key).
    // The SECOND generation flag [`super::argv::reject_generation_flags`] lets through — see `argv::EXEMPT_ARG_IDS`
    // for why it meets the same criterion `--static-dir` does. `-v` as well as `--verbosity`, because
    // the exemption is by ARG ID and so covers `Cli`'s short too: a short that passed the rejection
    // only to be an unknown argument here would be a worse error than the one it got through.
    //
    // `Option`, not a defaulted value, because "the command line said nothing" must be
    // distinguishable from "the command line said `warn`" — the run level is
    // `this ?? [defaults].verbosity ?? warn`, and a default here would silently outrank the key.
    #[clap(long = "verbosity", short = 'v', value_enum)]
    pub verbosity: Option<Verbosity>,

    /// Generate only these crates (default: every crate in the config).
    #[clap(value_parser, value_name = "CRATE")]
    pub crates: Vec<String>,
}
