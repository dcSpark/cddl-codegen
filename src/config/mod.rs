//! The `--config <file.toml>` multi-crate front end.
//!
//! # What this module is
//!
//! A pure **expansion layer**: it turns one TOML file into `Vec<(crate name, Cli)>` and hands each
//! `Cli` to the same `api::generate_to_disk` a command line would have reached. Nothing downstream
//! learns the config exists, which is what keeps the config from becoming a second place where
//! codegen semantics live: every key is a flag, so `docs/docs/command_line_flags.mdx` stays the one
//! reference for what a key MEANS and this file only decides where a key's value comes from.
//!
//! # Three properties the implementation is shaped around
//!
//! **1. `Cli` values are built by argv + `Cli::try_parse_from`, never field-by-field.** `Cli` derives
//! `Default`, but a derived default is `false`/`""`/`None` while the real defaults live in clap
//! attributes (`--lib-name` is `cddl-lib`, `--static-dir` is `static`, `--wasm` is true). Struct
//! construction would silently disagree with what the same flags do on a command line, and it would
//! skip clap's value parsers — `parse_json_schema_root`'s emitted-verbatim charset guard and
//! `parse_json_schema_dep`'s `<a>=<b>` split are validation the config must not be able to bypass.
//! Going through clap makes "a config key is its flag" true by construction rather than by test.
//!
//! **2. An unknown key is a hard error.** A typoed key that silently fell back to a default is the
//! config-file equivalent of a misspelled flag, which clap already rejects — except worse, because a
//! wrong flag fails loudly at generation while a wrong key ships a crate built with the wrong flag
//! set. Hence `deny_unknown_fields` on [`Settings`] and an explicit known-key check at every level
//! serde does not reach (the top-level tables, and the per-crate-only keys).
//!
//! This is also why [`Settings`] is NOT `#[serde(flatten)]`ed into a `CrateEntry` struct, which is
//! the obvious spelling: serde's flatten collects every key the outer struct does not name into the
//! flattened field's deserializer in a way that DEFEATS `deny_unknown_fields` on both structs, so
//! `preserv-encodings = true` in a crate table parses clean and is dropped. The crate table is
//! instead split by hand into the four per-crate-only keys and a remainder deserialized as
//! `Settings`, which puts the typo back in front of `deny_unknown_fields`.
//!
//! **3. Paths resolve against the CONFIG FILE's directory, not the process CWD.** This is the point
//! of the feature that a shell script cannot have: a config checked into a repo means the same
//! command works from any CWD, which retires the `--static-dir`-resolved-against-CWD trap (a session
//! whose CWD is a different checkout silently generates with THAT checkout's runtime).

mod argv;
mod convergence;
mod derive;
mod run;
mod schema;
mod validate;
pub use argv::{is_config_mode, reject_generation_flags};
pub use convergence::Convergence;
// Retain public config entry points also unused by the binary surface.
#[allow(unused_imports)]
pub use run::{
    ConfigCli, generate, generate_loaded, load_with, print_flags, print_flags_loaded,
    selection_with_deps,
};

#[allow(unused_imports)]
pub use schema::{CrateEntry, Settings, load, parse_str};
// Preserve crate-visible registry paths in both binary and library builds.
#[allow(unused_imports)]
pub(crate) use schema::{PER_CRATE_ONLY_KEYS, SETTINGS_KEYS, TOP_LEVEL_KEYS};

use crate::cli::Cli;
use crate::log::Verbosity;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::PathBuf;

/// The `[runtime]` table: one shared static runtime crate for every crate in the config.
///
/// Top-level rather than a `[defaults]` key because both halves are statements about the CONFIG, not
/// about a crate. `--export-static-crate` writes a runtime that serves everyone, so it belongs to
/// exactly one invocation and the config picks which; `--common-import-override` pointing at
/// different runtimes within one config is a mistake in every realistic project, so the shared value
/// is the one worth spelling once.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct Runtime {
    /// Where the shared runtime is written. Resolved against the config file's directory, like every
    /// other path key. Expands to `--export-static-crate` on exactly ONE crate's invocation — see
    /// `Config::runtime_carrier` for which.
    pub export_static_crate: Option<String>,
    /// Expands to `--common-import-override <value>` on every crate. It is the LOWEST layer in the
    /// merge: an explicit `common-import-override` in `[defaults]`, a profile, or a crate table wins
    /// for the crates it reaches, which is the exotic case (a crate importing a different runtime)
    /// this key is sugar for the common one of.
    pub common_import: Option<String>,
    /// Name the carrier by hand instead of deriving it, accepting the remaining unsupported
    /// flavor/depth-limit contract that made the derivation refuse. See `Config::runtime_carrier`.
    pub flavor_from: Option<String>,
    /// The cargo PACKAGE name of the co-owned runtime crate `export-static-crate` writes into — the
    /// same vocabulary a `[crates.<name>]` table's `lib-name` uses.
    ///
    /// Naming it is what lets the config derive each crate's dependency ON the runtime:
    /// `--rust-dep <lib-name>=<relative path>` plus `--std-forward-dep <lib-name>`, so a crate built
    /// with `default-features = false` reaches the runtime's `no_std` arm instead of stopping at its
    /// own. `common-import` cannot supply it — an override is a Rust path prefix (`crate::common` is
    /// a legal value), and no cargo package name follows from one.
    ///
    /// Optional by design: absent, nothing is derived and the dependency stays the hand edit it is
    /// today. Adding the key is the one-line opt-in.
    ///
    /// It must MATCH the crate's actual `package.name`. The tool does not read that manifest to
    /// check — a content read of a co-owned file, for a one-line rule — so a mismatch surfaces as
    /// cargo's own "no matching package named X found at path" error.
    pub lib_name: Option<String>,
}

/// The exported runtime's flavor: the `Cli` fields that change a byte of what
/// `--export-static-crate` writes, and the only ones.
///
/// Measured, not read off the flag's documentation: every `Cli` field was flipped one at a time and
/// the exported crate byte-diffed. `lib-name` does not appear because the change log the static
/// runtime's `Cargo.toml` folds carries no `cddl-lib` token to substitute; `static-dir` does change
/// the bytes but is the tool's own installation path rather than a property of a crate, so it is not
/// an axis a carrier can be chosen on.
///
/// The two GROUPS below are what make the derivation possible at all, and they are not
/// interchangeable — see [`Config::runtime_carrier`].
#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeFlavor {
    // --- EQUALITY axes: carrier derivation requires EXACT agreement. A preserve + canonical
    // runtime carries narrow bridges for a reduced `{+ K => V}` and `any`, but that accommodation
    // does not make arbitrary flavor mixtures derivable. `preserve-encodings` swaps
    // `NonEmptyMap`'s inner table for `OrderedHashMap` and re-types `CBORReadLen`;
    // `canonical-form` changes the arity of `fit_sz`, `LenEncoding::to_len_sz` and
    // `SerializeEmbeddedGroup`, and moves `Serialize` between the runtime and `cbor_event`;
    // `deserialize-depth-limit` bakes its VALUE into the exported `AnyCbor` recursion guard, so a
    // mismatch compiles cleanly while silently guarding one crate's `any` values at another crate's
    // limit.
    preserve_encodings: bool,
    canonical_form: bool,
    deserialize_depth_limit: Option<u32>,

    // --- MAX axes: `true` is a superset of `false`. The json/schemars companions are appended to
    // the runtime types, so a runtime carrying them serves a crate that does not, while the reverse
    // leaves the crate's `serde`/`schemars` impls unresolved.
    json_serde_derives: bool,
    json_schema_export: bool,
}

impl RuntimeFlavor {
    /// Read off a fully expanded `Cli` rather than off merged [`Settings`], so clap's defaults are
    /// never restated here — the same rule the graph derivation follows.
    fn of(cli: &Cli) -> Self {
        Self {
            preserve_encodings: cli.preserve_encodings,
            canonical_form: cli.canonical_form,
            deserialize_depth_limit: cli.deserialize_depth_limit,
            json_serde_derives: cli.json_serde_derives,
            json_schema_export: cli.json_schema_export,
        }
    }

    /// The axes every crate sharing one runtime must match EXACTLY, as rendered values.
    fn equality_axes(&self) -> [(&'static str, String); 3] {
        [
            ("preserve-encodings", self.preserve_encodings.to_string()),
            ("canonical-form", self.canonical_form.to_string()),
            (
                "deserialize-depth-limit",
                match self.deserialize_depth_limit {
                    Some(v) => v.to_string(),
                    None => "unset".to_owned(),
                },
            ),
        ]
    }
}

/// One max axis for the no-carrier diagnostic: `(config key, whether the join wants it, how to read
/// it off a flavor)`. Named so the "which crate supplies each axis" loop can stay a table.
type MaxAxis = (&'static str, bool, fn(&RuntimeFlavor) -> bool);

/// Which crate carries `--export-static-crate`, and what the run should say about it.
#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeChoice {
    /// The crate whose invocation gets the flag.
    pub carrier: String,
    /// Lines to print in the existing progress style, before any crate generates. Never empty: a
    /// silently-chosen carrier is what the hand-placed flag already does.
    pub notes: Vec<String>,
}

/// A parsed config document. Crate iteration is `BTreeMap` order (crate name) — never hash order, so
/// the same config produces the same sequence of invocations on every machine.
#[derive(Clone, Debug, PartialEq)]
pub struct Config {
    /// The directory every path key resolves against: the config file's parent.
    pub base_dir: PathBuf,
    pub defaults: Settings,
    pub profiles: BTreeMap<String, Settings>,
    pub crates: BTreeMap<String, CrateEntry>,
    /// The optional `[runtime]` table.
    pub runtime: Option<Runtime>,
    /// A command-line `--static-dir`, which overrides the key of that name for EVERY crate.
    ///
    /// Not parsed from the document — [`parse_str`] always leaves it `None` — because it is not a
    /// config value: it is the one thing a committed config cannot know, this machine's copy of the
    /// tool's hand-written runtime. Set by [`generate`]/[`print_flags`] from [`ConfigCli`].
    ///
    /// Carried VERBATIM rather than resolved against the config file's directory, because it did not
    /// come from the config file. A relative value means what it means on any other command line —
    /// relative to the process CWD — so the flag behaves identically in both modes.
    pub static_dir_override: Option<String>,
    /// A command-line `--verbosity`, which overrides the key of that name for EVERY crate.
    ///
    /// Not parsed from the document — [`parse_str`] always leaves it `None` — for the same reason its
    /// `static_dir` sibling is not: it is not a config value. The key is the project's committed
    /// default; this is THIS INVOCATION's override of it, so the override winning silently is the
    /// intended use rather than a conflict to report. Set by [`generate`]/[`print_flags`] from
    /// [`ConfigCli`].
    ///
    /// It also decides the RUN level — the `[runtime]` notes, the per-crate banner, the convergence
    /// lines — which is why [`generate`] reads it before any crate generates.
    pub verbosity_override: Option<Verbosity>,
}

impl Config {
    /// The `[runtime]` checks that need no expanded `Cli` — an empty table and an unknown
    /// `flavor-from`. The one-export-site rule is [`Self::validate_one_export_site`], which is NOT
    /// here because it must also run when there is no `[runtime]` table.
    ///
    /// The flavor derivation itself is NOT here: it reads each crate's finished `Cli`, so it lives
    /// in [`Self::runtime_carrier`] and runs during expansion — still before any crate generates.
    fn validate_runtime(&self) -> Result<(), String> {
        let Some(runtime) = &self.runtime else {
            return Ok(());
        };

        if runtime.export_static_crate.is_none() && runtime.common_import.is_none() {
            return Err(
                "`[runtime]` sets neither `export-static-crate` nor `common-import`, so it asks for \
                 nothing. An empty table is a typo rather than a request — either give it a key or \
                 delete it. (`flavor-from` only names which crate carries `export-static-crate`; it \
                 is not a request on its own.)"
                    .to_owned(),
            );
        }

        // Same shape as the empty-table rule above: a key that cannot mean anything is a typo, not a
        // request. `lib-name` derives each crate's cargo dependency on the runtime crate, and the
        // path side of that dependency is `export-static-crate` — without it there is no directory
        // to point at, and `common-import` alone points at a crate this config does not write.
        if let Some(lib_name) = &runtime.lib_name
            && runtime.export_static_crate.is_none()
        {
            return Err(format!(
                "`[runtime].lib-name` names `{lib_name}` as the cargo package of the shared runtime \
                 crate, but `[runtime]` sets no `export-static-crate`. The key derives each crate's \
                 dependency on that crate, whose PATH is the export directory — so there is nothing \
                 to point at. Say where the runtime is written, or drop `lib-name` and declare the \
                 dependency by hand."
            ));
        }

        if let Some(from) = &runtime.flavor_from {
            if runtime.export_static_crate.is_none() {
                return Err(format!(
                    "`[runtime].flavor-from` names `{from}` as the crate that carries \
                     `export-static-crate`, but `[runtime]` sets no `export-static-crate`. There is \
                     no export to carry — drop `flavor-from`, or say where the runtime is written."
                ));
            }
            if !self.crates.contains_key(from) {
                return Err(format!(
                    "`[runtime].flavor-from` names `{from}`, which has no `[crates.{from}]` table. \
                     It must name a crate in this config, since it selects whose flag set the \
                     exported runtime is. Configured crates: {}",
                    list_or_none(self.crates.keys())
                ));
            }
        }

        Ok(())
    }

    /// Fold the `[runtime]` table into one crate's merged settings.
    fn apply_runtime(&self, name: &str, settings: &mut Settings, choice: Option<&RuntimeChoice>) {
        let Some(runtime) = &self.runtime else {
            return;
        };
        if let Some(common_import) = &runtime.common_import {
            // Lowest layer: a `common-import-override` the merge already produced was written
            // explicitly somewhere, and an explicit value is the user overriding the sugar rather
            // than a conflict to report.
            settings
                .common_import_override
                .get_or_insert_with(|| common_import.clone());
        }
        if let (Some(path), Some(choice)) = (&runtime.export_static_crate, choice)
            && choice.carrier == name
        {
            settings.export_static_crate = Some(path.clone());
        }
    }

    /// Which crate carries `--export-static-crate`, and what to say about the choice. `None` when
    /// `[runtime]` writes no runtime.
    ///
    /// # Why this is derived rather than a key
    ///
    /// The export is a pure function of the flag set (a run against a different spec at the same
    /// flags writes byte-identical files), so the carrier is not a preference — it is whichever
    /// crate's flag set the shared runtime must have. Naming it by hand is what CML does today, with
    /// a comment explaining that a reduced-flavor crate would export a runtime the others cannot
    /// use; a config already knows every crate's flavor, so it can make that choice instead of
    /// documenting it.
    ///
    /// # The two kinds of axis
    ///
    /// [`RuntimeFlavor`]'s equality axes must be IDENTICAL across every crate. This is a config
    /// contract, not a claim that every mixed pair fails on every spec: a preserve + canonical
    /// runtime deliberately accommodates a reduced crate's `{+ K => V}` and `any`. The remaining
    /// canonical/non-canonical calling conventions differ at `fit_sz`/`to_len_sz`/
    /// `SerializeEmbeddedGroup`, and the depth limit is a contract about which documents are
    /// ACCEPTED, baked by value into the exported `AnyCbor` guard — worse than a compile error
    /// because it compiles while guarding at another crate's limit. The max axes
    /// (`json-serde-derives`, `json-schema-export`) genuinely nest: the json/schemars companions
    /// are appended to the runtime types, so carrying them serves a crate that does not.
    ///
    /// So the carrier is the first crate — in crate-name order, the order this config's tables are
    /// held in — whose flavor equals the agreed equality axes plus the OR of the max axes. Any crate
    /// matching that produces byte-identical output, so which one is picked is unobservable.
    ///
    /// # `flavor-from`
    ///
    /// Declaring the carrier by hand skips both refusals. It fires no per-run warning — the user has
    /// said they know, and a warning that fires forever trains people to ignore warnings — but the
    /// run states once which crates are generated at a flavor the runtime does not match and reminds
    /// them that the remaining flavor/depth-limit contract is unsupported.
    fn runtime_carrier(
        &self,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<Option<RuntimeChoice>, String> {
        let Some(runtime) = &self.runtime else {
            return Ok(None);
        };
        if runtime.export_static_crate.is_none() {
            return Ok(None);
        }
        let flavors: BTreeMap<&str, RuntimeFlavor> = ungraphed
            .iter()
            .map(|(name, cli)| (name.as_str(), RuntimeFlavor::of(cli)))
            .collect();

        if let Some(from) = &runtime.flavor_from {
            // Validated to name a configured crate by `validate_runtime`.
            let carrier_flavor = &flavors[from.as_str()];
            let mut notes = vec![format!(
                "[runtime] `{from}` carries --export-static-crate, declared by `flavor-from`."
            )];
            let mismatched: Vec<&str> = flavors
                .iter()
                .filter(|(name, flavor)| {
                    **name != from.as_str()
                        && flavor.equality_axes() != carrier_flavor.equality_axes()
                })
                .map(|(name, _)| *name)
                .collect();
            if !mismatched.is_empty() {
                notes.push(format!(
                    "[runtime] Generated at a flavor the shared runtime does not match: {}. A \
                     preserve + canonical runtime carries reduced-consumer bridges for `{{+ K => \
                     V}}` (`NonEmptyMap` from `BTreeMap`) and `any` (the one-argument \
                     `AnyCbor::serialize`), but this remains an explicitly accepted mismatch: \
                     automatic carrier derivation still requires identical \
                     preserve-encodings/canonical-form values, and a crate whose \
                     --deserialize-depth-limit differs has its `any` values guarded at `{}`'s \
                     limit rather than its own.",
                    quoted(mismatched.iter().copied()),
                    from
                ));
            }
            return Ok(Some(RuntimeChoice {
                carrier: from.clone(),
                notes,
            }));
        }

        // 1. The equality axes must agree. Reported axis by axis with the crates holding each value,
        //    because a user reading "the flavors disagree" has to diff five keys across N tables by
        //    hand to find which one.
        let mut disagreements: Vec<String> = Vec::new();
        for axis in 0..3 {
            let mut by_value: BTreeMap<String, Vec<&str>> = BTreeMap::new();
            for (name, flavor) in &flavors {
                let (_, value) = flavor.equality_axes()[axis].clone();
                by_value.entry(value).or_default().push(name);
            }
            if by_value.len() > 1 {
                let label = flavors
                    .values()
                    .next()
                    .expect("a config has at least one crate")
                    .equality_axes()[axis]
                    .0;
                let split = by_value
                    .iter()
                    .map(|(value, names)| format!("`{value}` in {}", quoted(names.iter().copied())))
                    .collect::<Vec<_>>()
                    .join(", ");
                disagreements.push(format!("`{label}` ({split})"));
            }
        }
        if !disagreements.is_empty() {
            return Err(format!(
                "`[runtime].export-static-crate` cannot DERIVE one runtime for these crates: they \
                 disagree on {}, and automatic carrier selection requires {} to match EXACTLY. A \
                 preserve + canonical runtime has narrow bridges for a reduced crate's `{{+ K => \
                 V}}` and `any`, but config derivation does not infer arbitrary spec-dependent \
                 flavor compatibility; canonical/non-canonical calling conventions still differ, \
                 and the depth limit is baked by value into the exported `AnyCbor` guard, so a \
                 mismatch there compiles while guarding one crate's `any` values at another's \
                 limit. Give every crate the same value, or accept the gap explicitly with \
                 `[runtime].flavor-from = \"<crate>\"`.",
                disagreements.join("; "),
                if disagreements.len() == 1 {
                    "it"
                } else {
                    "them"
                },
            ));
        }

        // 2. The join: the agreed equality axes plus the OR of the max axes.
        let any = flavors
            .values()
            .next()
            .expect("a config has at least one crate");
        let join = RuntimeFlavor {
            preserve_encodings: any.preserve_encodings,
            canonical_form: any.canonical_form,
            deserialize_depth_limit: any.deserialize_depth_limit,
            json_serde_derives: flavors.values().any(|f| f.json_serde_derives),
            json_schema_export: flavors.values().any(|f| f.json_schema_export),
        };

        // 3. The first crate in crate-name order whose flavor IS the join.
        let carrier = flavors
            .iter()
            .find(|(_, flavor)| **flavor == join)
            .map(|(name, _)| (*name).to_owned());
        let Some(carrier) = carrier else {
            let getters: [MaxAxis; 2] = [
                (
                    "json-serde-derives",
                    join.json_serde_derives,
                    |f: &RuntimeFlavor| f.json_serde_derives,
                ),
                (
                    "json-schema-export",
                    join.json_schema_export,
                    |f: &RuntimeFlavor| f.json_schema_export,
                ),
            ];
            let suppliers = getters
                .into_iter()
                .filter(|(_, wanted, _)| *wanted)
                .map(|(label, _, get)| {
                    let names: Vec<&str> = flavors
                        .iter()
                        .filter(|(_, f)| get(f))
                        .map(|(n, _)| *n)
                        .collect();
                    format!("{label} comes from {}", quoted(names.into_iter()))
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Err(format!(
                "no crate in this config has the flavor the shared runtime needs: {suppliers}, and \
                 no single crate has all of it. `--export-static-crate` exports the flag set of ONE \
                 invocation, so the runtime can only ever be a flavor some crate already has. Turn \
                 the missing keys on for one crate so it can carry the export, or name a carrier \
                 with `[runtime].flavor-from = \"<crate>\"` and accept that the crates it lacks \
                 will not resolve the runtime's json or schemars impls."
            ));
        };

        Ok(Some(RuntimeChoice {
            notes: vec![format!(
                "[runtime] `{carrier}` carries --export-static-crate: its flavor is the join of \
                 every crate's, so the runtime it writes serves all of them."
            )],
            carrier,
        }))
    }

    /// The `[runtime]` decision this config makes, for a run to state before it generates anything.
    ///
    /// Recomputed rather than returned out of [`Self::expand`] so the expansion's signature stays
    /// "a config is a list of invocations"; it is a pure function of the config, so the two cannot
    /// disagree.
    pub fn runtime_report(&self) -> Result<Option<RuntimeChoice>, String> {
        let ungraphed = self.ungraphed()?;
        self.runtime_carrier(&ungraphed)
    }
}

/// `` `a`, `b` `` — a comma-joined, backticked name list for a diagnostic.
fn quoted<'a>(names: impl Iterator<Item = &'a str>) -> String {
    names
        .map(|n| format!("`{n}`"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn list_or_none<'a>(names: impl Iterator<Item = &'a String>) -> String {
    let names: Vec<String> = names.map(|n| format!("`{n}`")).collect();
    if names.is_empty() {
        "(none)".to_owned()
    } else {
        names.join(", ")
    }
}

/// The committed-state verdict ([`Config::committed_verdict`]) as a typed error, so the exit code
/// can say which KIND of failure this was.
///
/// One exit code cannot carry the distinction, and the distinction is the whole point of the verdict.
/// A failed run — a config that would not expand, a spec that would not generate — means the tool did
/// not do what it was asked, and re-running it after fixing the input is the whole remedy. The verdict
/// means the opposite: every file this run was asked to write IS written, and the committed workspace
/// those files sit in does not build. No repeat of this command settles it; the message names the
/// dependency that does. An automated caller has to be able to tell "your inputs are wrong, nothing
/// happened" from "the generation happened and the tree now needs the named regen", and the exit code
/// is the only channel it reliably reads.
///
/// It WRAPS the message rather than restating it: `Display` is the verdict text verbatim, so every
/// assertion on that text still holds and the exit code is the only new fact. In particular the text
/// is still deliberately un-prefixed by `about_the_config` — the verdict is about the TREE, not
/// about the document — and this wrapper must not change that.
#[derive(Debug)]
pub struct VerdictError(String);

impl std::fmt::Display for VerdictError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for VerdictError {}
