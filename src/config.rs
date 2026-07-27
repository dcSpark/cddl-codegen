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

use crate::cli::Cli;
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// Every key a `[crates.<name>]` table holds that a `[defaults]`/`[profiles.*]` table may NOT.
///
/// `input`/`output`/`lib-name` are per-crate by nature — a default for them would make every crate
/// read the same spec or write the same directory, which is never what a multi-crate config means.
/// `profiles` is the reference INTO the layer system, so a profile listing profiles would be the
/// nesting the flat design deliberately excludes. `deps` is an EDGE, and an edge shared by every
/// crate is not a graph: it would make every crate depend on every other one, itself included.
const PER_CRATE_ONLY_KEYS: &[&str] = &["input", "output", "lib-name", "profiles", "deps"];

/// Why a given [`PER_CRATE_ONLY_KEYS`] entry cannot be shared, in the rejection message. Each key
/// gets its own sentence because the reasons are genuinely different — one is "this names a single
/// thing", the other "this names a relation" — and a user reading a generic message has to guess
/// which applies.
fn per_crate_key_reason(key: &str) -> &'static str {
    match key {
        "deps" => {
            "`deps` declares an EDGE from one crate to another. A shared edge is not a graph: every \
             crate would depend on every crate named, itself included."
        }
        "profiles" => {
            "`profiles` selects which shared layers ONE crate applies, so a shared value for it \
             would be a layer selecting layers — the nesting the flat profile design excludes."
        }
        _ => {
            "`input`, `output` and `lib-name` name ONE crate's spec, directory and library, so a \
             shared value for any of them would point every crate at the same thing."
        }
    }
}

/// The tables the document may hold at top level. Anything else is a typo or a feature this version
/// does not have; either way the user must hear about it rather than have the table ignored.
const TOP_LEVEL_KEYS: &[&str] = &["defaults", "profiles", "crates"];

/// Every `Cli` field that is not per-crate, each optional so "absent" is distinguishable from "set to
/// the value that happens to be the built-in default" — the distinction the merge is built on: an
/// absent key contributes nothing to its layer, a present one wins over everything before it.
///
/// Key names are the kebab-case of the `Cli` FIELD name, which coincides with the long flag for every
/// field but one: `preserve_comments`'s flag is the negated `--no-preserve-comments`. The key is
/// `preserve-comments`, a plain boolean whose built-in value is true — TOML has booleans, so a config
/// should not have to spell a negation, and `preserve-comments = false` is what emits the flag. The
/// `Cli`-field-name rule (rather than the flag-name rule) is what the drift gate
/// `config_keys_match_cli_fields` checks, so the two cannot disagree silently.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct Settings {
    // --- paths (resolved against the config file's directory) ---
    pub static_dir: Option<String>,
    pub export_static_crate: Option<String>,

    // --- booleans (clap `ArgAction::Set`, i.e. `--flag true|false`) ---
    pub annotate_fields: Option<bool>,
    pub to_from_bytes_methods: Option<bool>,
    pub binary_wrappers: Option<bool>,
    pub preserve_encodings: Option<bool>,
    pub canonical_form: Option<bool>,
    pub wasm: Option<bool>,
    pub json_serde_derives: Option<bool>,
    pub emit_tests: Option<bool>,
    pub emit_tests_conformance: Option<bool>,
    pub json_schema_export: Option<bool>,
    pub package_json: Option<bool>,
    pub json_schema_scripts: Option<bool>,
    pub no_synthesized_rust_collection_aliases: Option<bool>,
    /// The one key whose flag is the negation (`--no-preserve-comments`); see the struct doc.
    pub preserve_comments: Option<bool>,

    // --- scalars ---
    pub rust_wasm_feature: Option<String>,
    pub deserialize_depth_limit: Option<u32>,
    pub common_import_override: Option<String>,
    pub wasm_cbor_json_api_macro: Option<String>,
    pub wasm_conversions_macro: Option<String>,
    pub wasm_list_macro: Option<String>,

    // --- arrays: CONCATENATED across layers, author order preserved within each ---
    #[serde(default)]
    pub json_schema_root: Vec<String>,
    #[serde(default)]
    pub workspace_dep: Vec<String>,

    // --- `<k>=<v>` sub-tables: per-key UNION across layers, later layer wins per key ---
    #[serde(default)]
    pub extern_import: BTreeMap<String, String>,
    #[serde(default)]
    pub extern_wasm_crate: BTreeMap<String, String>,
    #[serde(default)]
    pub extern_wrapper_index: BTreeMap<String, String>,
    #[serde(default)]
    pub wrapper_requests: BTreeMap<String, String>,
    #[serde(default)]
    pub key_requests: BTreeMap<String, String>,
    #[serde(default)]
    pub json_schema_dep: BTreeMap<String, String>,
}

impl Settings {
    /// Fold `over` onto `self`: `over` is the LATER layer and wins.
    ///
    /// Written as an exhaustive destructure on purpose. A new `Cli` field adds a `Settings` field,
    /// and an exhaustive pattern makes forgetting it here a COMPILE error rather than a key that
    /// parses and is then silently dropped between layers — the one drift this file cannot detect at
    /// runtime, since a dropped key looks exactly like an absent one.
    fn merge_over(&mut self, over: &Settings) {
        let Settings {
            static_dir,
            export_static_crate,
            annotate_fields,
            to_from_bytes_methods,
            binary_wrappers,
            preserve_encodings,
            canonical_form,
            wasm,
            json_serde_derives,
            emit_tests,
            emit_tests_conformance,
            json_schema_export,
            package_json,
            json_schema_scripts,
            no_synthesized_rust_collection_aliases,
            preserve_comments,
            rust_wasm_feature,
            deserialize_depth_limit,
            common_import_override,
            wasm_cbor_json_api_macro,
            wasm_conversions_macro,
            wasm_list_macro,
            json_schema_root,
            workspace_dep,
            extern_import,
            extern_wasm_crate,
            extern_wrapper_index,
            wrapper_requests,
            key_requests,
            json_schema_dep,
        } = over;

        // Scalars: a set value replaces, an absent one leaves the earlier layer alone.
        macro_rules! scalar {
            ($($f:ident),* $(,)?) => {$(
                if let Some(v) = $f { self.$f = Some(v.clone()); }
            )*};
        }
        scalar!(
            static_dir,
            export_static_crate,
            annotate_fields,
            to_from_bytes_methods,
            binary_wrappers,
            preserve_encodings,
            canonical_form,
            wasm,
            json_serde_derives,
            emit_tests,
            emit_tests_conformance,
            json_schema_export,
            package_json,
            json_schema_scripts,
            no_synthesized_rust_collection_aliases,
            preserve_comments,
            rust_wasm_feature,
            deserialize_depth_limit,
            common_import_override,
            wasm_cbor_json_api_macro,
            wasm_conversions_macro,
            wasm_list_macro,
        );

        // Arrays CONCATENATE rather than replace: these are additive per-item lists, and
        // `--json-schema-root` is order-significant (roots emit after every spec-derived row, in flag
        // order), so "later wins" would mean a crate adding one root silently discards the shared
        // list `[defaults]` exists to hold.
        self.json_schema_root.extend(json_schema_root.clone());
        self.workspace_dep.extend(workspace_dep.clone());

        // Sub-tables union per key — the same accumulation a repeated `<k>=<v>` flag already gets by
        // landing in a `BTreeMap`. A later layer overrides only the keys it names.
        macro_rules! table {
            ($($f:ident),* $(,)?) => {$(
                for (k, v) in $f { self.$f.insert(k.clone(), v.clone()); }
            )*};
        }
        table!(
            extern_import,
            extern_wasm_crate,
            extern_wrapper_index,
            wrapper_requests,
            key_requests,
            json_schema_dep,
        );
    }
}

/// One `[crates.<name>]` entry: the per-crate-only keys plus that table's own [`Settings`] layer.
#[derive(Clone, Debug, PartialEq)]
pub struct CrateEntry {
    pub input: String,
    pub output: String,
    /// Defaults to the crate table key — the one place the config is LESS repetitive than the CLI,
    /// where `--lib-name` defaults to `cddl-lib` and so realistically always needs passing.
    pub lib_name: String,
    pub profiles: Vec<String>,
    /// Names of other `[crates.*]` entries this crate's spec depends on. The single piece of
    /// cross-crate sugar: each entry expands to the `<name>=<path>` flag pairs a hand-written
    /// invocation spells on BOTH sides of the edge, and the set of edges is the generation order.
    /// Author order is preserved — it is the order the derived `--workspace-dep` occurrences take.
    pub deps: Vec<String>,
    pub settings: Settings,
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
}

/// Read and parse a config file. Paths inside it resolve against ITS directory, so the caller's CWD
/// never reaches the generated output.
pub fn load(path: &Path) -> Result<Config, String> {
    let text = std::fs::read_to_string(path).map_err(|e| {
        format!(
            "--config {}: cannot read the config file: {e}",
            path.display()
        )
    })?;
    // `parent()` of a bare filename is `Some("")`, which joins as a no-op relative path — exactly the
    // "same directory" answer, so no special case is needed.
    let base_dir = path.parent().unwrap_or(Path::new("")).to_path_buf();
    parse_str(&text, &base_dir).map_err(|e| format!("--config {}: {e}", path.display()))
}

/// Parse config TEXT with an explicit base directory. Split out from [`load`] so the test suite can
/// exercise the schema without a file, and so the base directory is an explicit input rather than
/// something derived from process state.
pub fn parse_str(text: &str, base_dir: &Path) -> Result<Config, String> {
    let doc: toml::Table = toml::from_str(text).map_err(|e| e.to_string())?;

    if let Some(key) = doc.keys().find(|k| !TOP_LEVEL_KEYS.contains(&k.as_str())) {
        return Err(format!(
            "unknown top-level table `{key}`; this version understands {}",
            TOP_LEVEL_KEYS
                .iter()
                .map(|k| format!("`[{k}]`"))
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    let defaults = match doc.get("defaults") {
        Some(v) => settings_from_table(as_table(v, "defaults")?, "[defaults]", false)?,
        None => Settings::default(),
    };

    let mut profiles = BTreeMap::new();
    if let Some(v) = doc.get("profiles") {
        for (name, body) in as_table(v, "profiles")? {
            let label = format!("[profiles.{name}]");
            profiles.insert(
                name.clone(),
                settings_from_table(as_table(body, &label)?, &label, false)?,
            );
        }
    }

    let crate_tables = doc.get("crates").ok_or_else(|| {
        "no `[crates.<name>]` tables; a config generates at least one crate".to_owned()
    })?;
    let crate_tables = as_table(crate_tables, "crates")?;
    if crate_tables.is_empty() {
        return Err(
            "no `[crates.<name>]` tables; a config generates at least one crate".to_owned(),
        );
    }

    let mut crates = BTreeMap::new();
    for (name, body) in crate_tables {
        let label = format!("[crates.{name}]");
        let table = as_table(body, &label)?;
        let settings = settings_from_table(table, &label, true)?;
        let input = required_string(table, "input", &label)?;
        let output = required_string(table, "output", &label)?;
        let lib_name = match table.get("lib-name") {
            Some(v) => v
                .as_str()
                .ok_or_else(|| format!("{label}.lib-name must be a string"))?
                .to_owned(),
            None => name.clone(),
        };
        let profiles = match table.get("profiles") {
            Some(v) => string_array(v, &format!("{label}.profiles"))?,
            None => Vec::new(),
        };
        let deps = match table.get("deps") {
            Some(v) => string_array(v, &format!("{label}.deps"))?,
            None => Vec::new(),
        };
        crates.insert(
            name.clone(),
            CrateEntry {
                input,
                output,
                lib_name,
                profiles,
                deps,
                settings,
            },
        );
    }

    let config = Config {
        base_dir: base_dir.to_path_buf(),
        defaults,
        profiles,
        crates,
    };
    config.validate()?;
    Ok(config)
}

fn as_table<'a>(value: &'a toml::Value, label: &str) -> Result<&'a toml::Table, String> {
    value
        .as_table()
        .ok_or_else(|| format!("`{label}` must be a table"))
}

fn required_string(table: &toml::Table, key: &str, label: &str) -> Result<String, String> {
    match table.get(key) {
        Some(v) => v
            .as_str()
            .map(str::to_owned)
            .ok_or_else(|| format!("{label}.{key} must be a string")),
        None => Err(format!("{label} has no `{key}`; it is required")),
    }
}

fn string_array(value: &toml::Value, label: &str) -> Result<Vec<String>, String> {
    let arr = value
        .as_array()
        .ok_or_else(|| format!("{label} must be an array of strings"))?;
    arr.iter()
        .map(|v| {
            v.as_str()
                .map(str::to_owned)
                .ok_or_else(|| format!("{label} must be an array of strings"))
        })
        .collect()
}

/// Deserialize a table's shared keys as [`Settings`], having first removed (or rejected) the
/// per-crate-only ones.
///
/// The hand split is what puts an unknown key back in front of `deny_unknown_fields`: see the module
/// doc on why `#[serde(flatten)]` cannot be used here.
fn settings_from_table(
    table: &toml::Table,
    label: &str,
    allow_per_crate_keys: bool,
) -> Result<Settings, String> {
    let mut rest = toml::Table::new();
    for (key, value) in table {
        if PER_CRATE_ONLY_KEYS.contains(&key.as_str()) {
            if allow_per_crate_keys {
                continue;
            }
            return Err(format!(
                "`{key}` is a per-crate key and cannot appear in {label}: {} Move it into the \
                 `[crates.<name>]` table it belongs to.",
                per_crate_key_reason(key)
            ));
        }
        rest.insert(key.clone(), value.clone());
    }
    Settings::deserialize(toml::Value::Table(rest))
        .map_err(|e| format!("{label}: {}", e.to_string().trim_end()))
}

impl Config {
    /// Cross-table checks serde cannot express: profile references resolve, a profile is flat, and
    /// the `deps` graph is one a generation order exists for.
    ///
    /// All of it runs at PARSE time, before any crate generates — a graph mistake in the last crate's
    /// table must not be discovered after the first crate's output is already rewritten.
    fn validate(&self) -> Result<(), String> {
        for (name, entry) in &self.crates {
            let mut seen = BTreeSet::new();
            for profile in &entry.profiles {
                if !self.profiles.contains_key(profile) {
                    return Err(format!(
                        "[crates.{name}].profiles names `{profile}`, which has no \
                         `[profiles.{profile}]` table. Configured profiles: {}",
                        list_or_none(self.profiles.keys())
                    ));
                }
                if !seen.insert(profile.clone()) {
                    return Err(format!(
                        "[crates.{name}].profiles lists `{profile}` twice; profiles apply in listed \
                         order and applying one twice cannot mean anything a single mention does not"
                    ));
                }
            }

            let mut seen = BTreeSet::new();
            for dep in &entry.deps {
                if dep == name {
                    return Err(format!(
                        "[crates.{name}].deps lists `{name}` itself. A crate's own types are already \
                         in its spec; a self-edge would derive an --extern-import pointing the crate \
                         at its own committed export."
                    ));
                }
                if !self.crates.contains_key(dep) {
                    return Err(format!(
                        "[crates.{name}].deps names `{dep}`, which has no `[crates.{dep}]` table. \
                         Every derived flag value comes from the dependency's OWN entry (its \
                         `output` and `lib-name`), so a dependency outside this config cannot be \
                         sugar — spell it with the raw `[crates.{name}.extern-import]` sub-table \
                         instead. Configured crates: {}",
                        list_or_none(self.crates.keys())
                    ));
                }
                if !seen.insert(dep.clone()) {
                    return Err(format!(
                        "[crates.{name}].deps lists `{dep}` twice; one edge is one dependency, and \
                         the second mention would derive the same flag values a second time"
                    ));
                }
            }
        }

        // Two crates with one library name would collide on every derived value at once: one
        // `extern-interface/<lib>` directory, one `<lib>_wasm` crate, one `--wrapper-requests`
        // label. Rejected whether or not a `deps` edge exists today, since the two crates could not
        // live in one cargo workspace either.
        let mut by_lib: BTreeMap<String, &String> = BTreeMap::new();
        for (name, entry) in &self.crates {
            let lib = normalized(&entry.lib_name);
            if let Some(first) = by_lib.insert(lib.clone(), name) {
                return Err(format!(
                    "[crates.{first}] and [crates.{name}] both have the library name `{lib}`. Every \
                     cross-crate value is derived from it — the `extern-interface/{lib}` export \
                     directory, the `{lib}_wasm` binding crate, the request labels — so two crates \
                     sharing one is ambiguous, and a cargo workspace could not hold both anyway. \
                     Give one of them its own `lib-name`."
                ));
            }
        }

        self.generation_order().map(|_| ())
    }

    /// The order the crates generate in: a topological sort over `deps`, dependencies FIRST, ties
    /// broken by crate name.
    ///
    /// Dependencies first is what makes the forward edges work within a single run — a consumer's
    /// `--extern-import` and `--extern-wrapper-index` read files the dependency wrote moments
    /// earlier. The reverse edges (`--wrapper-requests`, `--key-requests`) want the opposite order
    /// and do NOT get it: they read the consumer's *committed* sidecar, exactly as their own
    /// documentation specifies, and a run that changes one leaves its dependency one run stale. The
    /// convergence check ([`Convergence`]) is what makes that visible rather than silent.
    ///
    /// The tie-break is what makes the order TOTAL: without it two independent crates would order by
    /// whatever the traversal happened to reach first, and the run's progress output (and any
    /// generation-order-sensitive diagnostic) would differ between two runs of one config.
    pub fn generation_order(&self) -> Result<Vec<String>, String> {
        let mut remaining: BTreeSet<&String> = self.crates.keys().collect();
        let mut done: BTreeSet<&String> = BTreeSet::new();
        let mut order: Vec<String> = Vec::with_capacity(self.crates.len());
        while !remaining.is_empty() {
            // `remaining` is a BTreeSet, so `find` walks it in name order: among the crates whose
            // dependencies are all placed, the alphabetically first one goes next.
            let next = remaining
                .iter()
                .find(|name| {
                    self.crates[**name]
                        .deps
                        .iter()
                        .all(|dep| done.contains(dep))
                })
                .copied();
            let Some(next) = next else {
                return Err(self.cycle_error(&remaining));
            };
            remaining.remove(next);
            done.insert(next);
            order.push(next.clone());
        }
        Ok(order)
    }

    /// Render the cycle inside `remaining` as `a → b → c → a`.
    ///
    /// Reporting that a cycle EXISTS leaves the user to find it by hand across a config where every
    /// crate looks locally fine; the edges are right here, so the message names them. Walking from
    /// the alphabetically first blocked crate along each entry's first still-blocked dependency
    /// reaches a repeat in at most `remaining.len()` steps, and the slice from that repeat IS the
    /// cycle — the walk may start on a crate that merely depends on one, which is why the prefix
    /// before the repeat is dropped rather than printed.
    fn cycle_error(&self, remaining: &BTreeSet<&String>) -> String {
        let start = *remaining.iter().next().expect("a cycle needs a member");
        let mut path: Vec<&String> = Vec::new();
        let mut at = start;
        let repeat = loop {
            if let Some(pos) = path.iter().position(|seen| *seen == at) {
                break pos;
            }
            path.push(at);
            at = self.crates[at]
                .deps
                .iter()
                .find(|dep| remaining.contains(dep))
                .expect("a blocked crate has a blocked dependency");
        };
        let mut cycle: Vec<&str> = path[repeat..].iter().map(|n| n.as_str()).collect();
        cycle.push(cycle[0]);
        format!(
            "`deps` form a cycle: {}. Generation order is a topological sort over `deps`, and a \
             cycle has none — break the edge, or invert it: a dependency's spec cannot reference \
             its consumer's types, since the consumer is the one that imports the export.",
            cycle.join(" → ")
        )
    }

    /// The merged [`Settings`] a crate generates under: built-in (clap, by omission) → `[defaults]`
    /// → each named profile IN LISTED ORDER → the crate's own keys.
    fn merged_settings(&self, entry: &CrateEntry) -> Settings {
        let mut merged = self.defaults.clone();
        for profile in &entry.profiles {
            // Validated by `validate()`; a missing profile here would be a bug, not user input.
            merged.merge_over(&self.profiles[profile]);
        }
        merged.merge_over(&entry.settings);
        merged
    }

    /// Expand to the sequence of invocations this config describes, in generation order.
    ///
    /// `selected` empty means every crate. A name with no `[crates.<name>]` table is a hard error
    /// rather than a silent no-op — a typoed crate name on the command line would otherwise generate
    /// nothing and exit 0.
    ///
    /// Selecting a subset does NOT pull in its dependencies. The unselected dependency's committed
    /// output is trusted exactly as a dependency in another repository's is — the same contract the
    /// cross-crate flags already document — so `--config c.toml ledger` regenerates one crate against
    /// what `core` last wrote, and fails with the flags' own error if `core` never wrote anything.
    pub fn expand(&self, selected: &[String]) -> Result<Vec<(String, Cli)>, String> {
        // EVERY crate is expanded first, selected or not, because the graph derivation reads values
        // (`output`, `lib-name`, `wasm`, `package-json`) out of the OTHER crate's finished `Cli`
        // rather than re-deriving clap's defaults here — reading them back is what stops a default
        // drifting between the two places it would otherwise be written. A dependency of a selected
        // crate need not be selected, and neither need a consumer whose reverse edge a selected
        // dependency carries, so the set that must be expanded is all of them.
        let mut ungraphed: BTreeMap<String, Cli> = BTreeMap::new();
        for (name, entry) in &self.crates {
            let settings = self.merged_settings(entry);
            ungraphed.insert(
                name.clone(),
                build_cli(name, entry, &settings, &self.base_dir)?,
            );
        }

        let order = self.generation_order()?;
        let chosen: Vec<String> = if selected.is_empty() {
            order
        } else {
            for name in selected {
                if !self.crates.contains_key(name) {
                    return Err(format!(
                        "`{name}` is not a crate in this config. Configured crates: {}",
                        list_or_none(self.crates.keys())
                    ));
                }
            }
            // Deduplicated and re-ordered into generation order: the selection picks WHICH crates
            // run, never in what order — that is the config's business — so `a b` and `b a` must
            // generate the same thing.
            let wanted: BTreeSet<&String> = selected.iter().collect();
            order
                .into_iter()
                .filter(|name| wanted.contains(name))
                .collect()
        };

        chosen
            .into_iter()
            .map(|name| {
                let entry = &self.crates[&name];
                let mut settings = self.merged_settings(entry);
                self.apply_graph_edges(&name, entry, &mut settings, &ungraphed);
                let cli = build_cli(&name, entry, &settings, &self.base_dir)?;
                Ok((name, cli))
            })
            .collect()
    }

    /// Fold this crate's `deps` edges — both directions — into its merged settings.
    ///
    /// Every value here is one the config already holds, which is the whole point: hand-maintaining
    /// `<name>=<path>` pairs on both sides of an edge means two files that must agree about a third
    /// crate's `output` and `lib-name`, and nothing checks that they do.
    ///
    /// A hand-written sub-table entry for the same key always wins, silently: an explicit value is
    /// the user overriding the sugar for a case it does not cover, not a conflict to report.
    fn apply_graph_edges(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &mut Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) {
        // FORWARD edges: what this crate needs in order to consume each dependency.
        for dep in &entry.deps {
            let dep_entry = &self.crates[dep];
            let dep_cli = &ungraphed[dep.as_str()];
            let key = normalized(&dep_entry.lib_name);

            // The dependency's committed extern-interface export, a sibling of `rust/`/`wasm/` under
            // its `output` (NOT under the `--package-json` nesting — the export is emitted in every
            // mode, including rust-only, so it does not live inside the npm package's crate root).
            settings
                .extern_import
                .entry(key.clone())
                .or_insert_with(|| join(&dep_entry.output, &format!("extern-interface/{key}")));

            // The remaining three are all about the dependency's WASM face, so all three are emitted
            // exactly when it has one. `--workspace-dep` in particular is not optional here: it is a
            // hard error without an `--extern-wasm-crate` mapping, so a dependency generating no
            // wasm crate must get neither.
            if !dep_cli.wasm {
                continue;
            }
            settings
                .extern_wasm_crate
                .entry(key.clone())
                .or_insert_with(|| format!("{key}_wasm"));
            settings
                .extern_wrapper_index
                .entry(key.clone())
                .or_insert_with(|| {
                    crate_relative(
                        dep_cli,
                        &dep_entry.output,
                        "wasm/src/generated/collections.rs",
                    )
                });
            if !settings.workspace_dep.contains(&key) {
                settings.workspace_dep.push(key);
            }
        }

        // REVERSE edges: the sidecars each consumer of THIS crate emits, which this crate reads so
        // the wrappers and key derives its consumers borrow are hosted here rather than duplicated
        // per consumer. In consumer-name order; the label is the consumer's library name, which is
        // what the attribution comments the dep emits will carry.
        if !ungraphed[name].wasm {
            // Without a wasm crate this crate is never a `--workspace-dep` of anyone, so no consumer
            // emits either sidecar and both derived paths would name files that are never written.
            return;
        }
        for (consumer_name, consumer) in &self.crates {
            if !consumer.deps.iter().any(|dep| dep.as_str() == name) {
                continue;
            }
            let consumer_cli = &ungraphed[consumer_name.as_str()];
            let label = normalized(&consumer.lib_name);
            // The rust-side sidecar rides on `--workspace-dep` alone, so a rust-only consumer still
            // emits it; the wasm-side one exists only when the consumer has a wasm crate to record.
            settings
                .key_requests
                .entry(label.clone())
                .or_insert_with(|| {
                    crate_relative(
                        consumer_cli,
                        &consumer.output,
                        "rust/src/generated/borrowed_key_types.rs",
                    )
                });
            if consumer_cli.wasm {
                settings.wrapper_requests.entry(label).or_insert_with(|| {
                    crate_relative(
                        consumer_cli,
                        &consumer.output,
                        "wasm/src/generated/borrowed_collections.rs",
                    )
                });
            }
        }
    }
}

/// A library name in the form every cross-crate value uses: the rust crate name, which is the
/// `--lib-name` with dashes normalised to underscores (`Cli::lib_name_code`). It is simultaneously
/// the `extern-interface/<dir>` name a dependency exports under and the
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` scope a consumer imports it into — they coincide because
/// the scope's leading component IS the crate the generated `use` line names, so the two cannot be
/// chosen independently.
fn normalized(lib_name: &str) -> String {
    lib_name.replace('-', "_")
}

/// A path under a crate's `output`, left CONFIG-RELATIVE — [`argv_fragments`] resolves it against the
/// config file's directory like every other path value, so resolving here would apply the base
/// directory twice.
fn join(output: &str, tail: &str) -> String {
    Path::new(output).join(tail).to_string_lossy().into_owned()
}

/// A path to a file inside one of a crate's generated CRATES (`rust/…`, `wasm/…`), as opposed to a
/// sibling of them.
///
/// `--package-json` moves the crates one level down: the output root becomes the npm package (its
/// `package.json` and `scripts/`) and the cargo crates land under `<output>/rust/{rust,wasm}`. So
/// every derived path into a crate depends on the OTHER crate's `package-json` value — which is read
/// off its expanded `Cli`, never guessed.
fn crate_relative(cli: &Cli, output: &str, tail: &str) -> String {
    if cli.package_json {
        join(output, &format!("rust/{tail}"))
    } else {
        join(output, tail)
    }
}

fn list_or_none<'a>(names: impl Iterator<Item = &'a String>) -> String {
    let names: Vec<String> = names.map(|n| format!("`{n}`")).collect();
    if names.is_empty() {
        "(none)".to_owned()
    } else {
        names.join(", ")
    }
}

/// Resolve a path-valued config value against the config file's directory.
///
/// An ABSOLUTE path passes through untouched; a relative one is joined. Deliberately no
/// canonicalization: `output` and `export-static-crate` routinely name directories that do not exist
/// yet, and canonicalizing would fail on exactly those, so the resolved value is a lexical join.
fn resolve_path(base_dir: &Path, value: &str) -> String {
    let path = Path::new(value);
    if path.is_absolute() {
        value.to_owned()
    } else {
        base_dir.join(path).to_string_lossy().into_owned()
    }
}

/// The argv fragments a crate's settings expand to, each tagged with the config key that produced it
/// so a clap rejection can be reported against the TOML the user actually wrote.
///
/// Exhaustively destructures `settings` for the same reason [`Settings::merge_over`] does: a new
/// field that nothing emits here would parse, merge, and then vanish.
fn argv_fragments(
    entry: &CrateEntry,
    settings: &Settings,
    base_dir: &Path,
) -> Vec<(&'static str, Vec<String>)> {
    let Settings {
        static_dir,
        export_static_crate,
        annotate_fields,
        to_from_bytes_methods,
        binary_wrappers,
        preserve_encodings,
        canonical_form,
        wasm,
        json_serde_derives,
        emit_tests,
        emit_tests_conformance,
        json_schema_export,
        package_json,
        json_schema_scripts,
        no_synthesized_rust_collection_aliases,
        preserve_comments,
        rust_wasm_feature,
        deserialize_depth_limit,
        common_import_override,
        wasm_cbor_json_api_macro,
        wasm_conversions_macro,
        wasm_list_macro,
        json_schema_root,
        workspace_dep,
        extern_import,
        extern_wasm_crate,
        extern_wrapper_index,
        wrapper_requests,
        key_requests,
        json_schema_dep,
    } = settings;

    let mut out: Vec<(&'static str, Vec<String>)> = Vec::new();
    // A macro rather than a closure so the negated switch below (which pushes a one-token fragment)
    // does not collide with a closure's exclusive borrow of `out`.
    macro_rules! flag {
        ($key:expr, $name:expr, $value:expr $(,)?) => {
            out.push(($key, vec![format!("--{}", $name), $value]))
        };
    }

    // Per-crate keys first, so the produced argv reads like the hand invocation it replaces.
    flag!("input", "input", resolve_path(base_dir, &entry.input));
    flag!("output", "output", resolve_path(base_dir, &entry.output));
    flag!("lib-name", "lib-name", entry.lib_name.clone());

    // Paths — resolved against the config file's directory, never the process CWD.
    if let Some(v) = static_dir {
        flag!("static-dir", "static-dir", resolve_path(base_dir, v));
    }
    if let Some(v) = export_static_crate {
        flag!(
            "export-static-crate",
            "export-static-crate",
            resolve_path(base_dir, v),
        );
    }

    // `ArgAction::Set` booleans take an explicit `true`/`false`, so an absent key is the only way to
    // mean "leave clap's built-in default alone" — which is exactly what `None` does here.
    macro_rules! set_bool {
        ($($opt:ident => $key:literal),* $(,)?) => {$(
            if let Some(v) = $opt { flag!($key, $key, v.to_string()); }
        )*};
    }
    set_bool!(
        annotate_fields => "annotate-fields",
        to_from_bytes_methods => "to-from-bytes-methods",
        binary_wrappers => "binary-wrappers",
        preserve_encodings => "preserve-encodings",
        canonical_form => "canonical-form",
        wasm => "wasm",
        json_serde_derives => "json-serde-derives",
        emit_tests => "emit-tests",
        emit_tests_conformance => "emit-tests-conformance",
        json_schema_export => "json-schema-export",
        package_json => "package-json",
        json_schema_scripts => "json-schema-scripts",
        no_synthesized_rust_collection_aliases => "no-synthesized-rust-collection-aliases",
    );

    // The one negated flag: `--no-preserve-comments` is a `SetFalse` switch with no positive form, so
    // `false` emits it and `true` (the built-in) emits nothing. Writing the key as the POSITIVE
    // `preserve-comments` keeps the config free of double negatives — TOML has booleans.
    if preserve_comments == &Some(false) {
        out.push((
            "preserve-comments",
            vec!["--no-preserve-comments".to_owned()],
        ));
    }

    if let Some(v) = rust_wasm_feature {
        flag!("rust-wasm-feature", "rust-wasm-feature", v.clone());
    }
    if let Some(v) = deserialize_depth_limit {
        flag!(
            "deserialize-depth-limit",
            "deserialize-depth-limit",
            v.to_string(),
        );
    }
    if let Some(v) = common_import_override {
        flag!(
            "common-import-override",
            "common-import-override",
            v.clone()
        );
    }
    if let Some(v) = wasm_cbor_json_api_macro {
        flag!(
            "wasm-cbor-json-api-macro",
            "wasm-cbor-json-api-macro",
            v.clone(),
        );
    }
    if let Some(v) = wasm_conversions_macro {
        flag!(
            "wasm-conversions-macro",
            "wasm-conversions-macro",
            v.clone()
        );
    }
    if let Some(v) = wasm_list_macro {
        flag!("wasm-list-macro", "wasm-list-macro", v.clone());
    }

    // Arrays: one flag occurrence per item, in array order — the order IS the input for
    // `--json-schema-root`, so nothing sorts here.
    for v in json_schema_root {
        flag!("json-schema-root", "json-schema-root", v.clone());
    }
    for v in workspace_dep {
        flag!("workspace-dep", "workspace-dep", v.clone());
    }

    // `<k>=<v>` sub-tables. The right-hand side is a PATH for the four that name a file the tool
    // reads, and a NAME for the two that land in generated rust — hence the split.
    macro_rules! path_table {
        ($($tbl:ident => $key:literal),* $(,)?) => {$(
            for (k, v) in $tbl {
                flag!($key, $key, format!("{k}={}", resolve_path(base_dir, v)));
            }
        )*};
    }
    path_table!(
        extern_import => "extern-import",
        extern_wrapper_index => "extern-wrapper-index",
        wrapper_requests => "wrapper-requests",
        key_requests => "key-requests",
    );
    // `extern-wasm-crate`'s right side is a CRATE name and `json-schema-dep`'s is a rust MODULE PATH
    // emitted verbatim into generated code; neither is a filesystem path, so neither resolves.
    for (k, v) in extern_wasm_crate {
        flag!("extern-wasm-crate", "extern-wasm-crate", format!("{k}={v}"));
    }
    for (k, v) in json_schema_dep {
        flag!("json-schema-dep", "json-schema-dep", format!("{k}={v}"));
    }

    out
}

/// Build one crate's `Cli` through clap, wrapping a rejection with the config key that caused it.
fn build_cli(
    name: &str,
    entry: &CrateEntry,
    settings: &Settings,
    base_dir: &Path,
) -> Result<Cli, String> {
    use clap::Parser;

    let fragments = argv_fragments(entry, settings, base_dir);
    let mut argv: Vec<String> = vec!["cddl-codegen".to_owned()];
    for (_, fragment) in &fragments {
        argv.extend(fragment.iter().cloned());
    }
    match Cli::try_parse_from(&argv) {
        Ok(cli) => Ok(cli),
        Err(err) => {
            // Attribute the rejection to a KEY by replaying the invocation one fragment at a time on
            // top of the required pair. Without this the user is shown a clap error about a flag they
            // never typed; with it they are pointed at the TOML line they did.
            let base: Vec<String> = vec![
                "cddl-codegen".to_owned(),
                "--input".to_owned(),
                resolve_path(base_dir, &entry.input),
                "--output".to_owned(),
                resolve_path(base_dir, &entry.output),
            ];
            for (key, fragment) in &fragments {
                // Already in `base`; re-adding them is clap's "cannot be used multiple times", which
                // would misattribute every rejection to `input`.
                if matches!(*key, "input" | "output") {
                    continue;
                }
                let mut probe = base.clone();
                probe.extend(fragment.iter().cloned());
                if let Err(single) = Cli::try_parse_from(&probe) {
                    return Err(format!(
                        "[crates.{name}].{key}: {}",
                        single.to_string().trim_end()
                    ));
                }
            }
            Err(format!("[crates.{name}]: {}", err.to_string().trim_end()))
        }
    }
}

/// The convergence check: whether a sidecar this run CONSUMED was rewritten by the same run.
///
/// The two edge kinds want opposite generation orders. `--extern-import`/`--extern-wrapper-index`
/// want the dependency first; `--wrapper-requests`/`--key-requests` want the consumer first. No
/// single pass satisfies both, and this is not a defect to engineer away — both flags document their
/// input as the OTHER crate's *committed* output. Generation order resolves it in the dependency's
/// favour, which leaves the reverse edges reading last run's sidecars.
///
/// So this records each consumed sidecar's bytes BEFORE the run and compares them after. A change
/// means the crate that read it generated against a stale one and is now a run behind. It is
/// strictly a diagnostic: it runs after every file is written, changes no output byte, and feeds
/// nothing back into what is generated — deliberately NOT an automatic re-run, which would need a
/// divergence bound and would muddy "run twice = run once = clean run", where a warning makes the
/// two-run case explicit and self-documenting.
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
        self.entries
            .iter()
            .filter(|(_, path, before)| std::fs::read(path).ok() != *before)
            .map(|(name, _, _)| name.clone())
            .collect()
    }

    /// The re-run instruction, or `None` when the run converged.
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

/// Run everything a config file describes: expand it, generate each crate in order, then report
/// whether the run converged.
///
/// Expansion happens up front, so every value is validated before ANY crate generates — a typo in the
/// last crate's table must not leave the first crate's output half-migrated on disk.
pub fn generate(config_path: &Path, selected: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    let config = load(config_path)?;
    let expanded = config.expand(selected)?;
    let convergence = Convergence::capture(&expanded);
    for (name, cli) in &expanded {
        // A per-crate banner, NOT a per-line prefix: the generator's progress output is consumed
        // as-is by humans and by tests, so this adds a line rather than rewriting the existing ones.
        println!(
            "\n[{name}] generating from {} into {}",
            cli.input.display(),
            cli.output.display()
        );
        crate::api::generate_to_disk(cli)?;
    }
    if let Some(warning) = convergence.warning(config_path, selected) {
        eprintln!("{warning}");
    }
    Ok(())
}

/// The config-mode command line: `cddl-codegen --config <file> [CRATE...]`.
///
/// A SEPARATE clap struct rather than a `--config` field on [`Cli`], because `Cli` makes
/// `--input`/`--output` required — a `Cli` that could also be a config invocation would have to make
/// them optional, which is the downstream-visible restructuring this feature is not allowed to do.
#[derive(Debug, clap::Parser)]
#[clap(
    about = "Generate every crate a cddl-codegen config file describes.",
    long_about = "Generate the crates a cddl-codegen config file describes.\n\nPaths inside the \
                  config resolve against the CONFIG FILE's directory, not the current one, so the \
                  same command works from anywhere. Naming crates limits the run to those crates; \
                  naming none runs them all."
)]
pub struct ConfigCli {
    /// Path to the config file.
    #[clap(long = "config", value_parser, value_name = "CONFIG_TOML")]
    pub config: PathBuf,

    /// Generate only these crates (default: every crate in the config).
    #[clap(value_parser, value_name = "CRATE")]
    pub crates: Vec<String>,
}

/// Does this command line ask for config mode?
///
/// A prescan rather than a clap-level decision: the two modes have disjoint, both-required flag sets,
/// so which struct to parse must be known before parsing. `--config=<v>` and `--config <v>` are both
/// spellings clap accepts, so both are recognized here.
pub fn is_config_mode(argv: &[String]) -> bool {
    argv.iter()
        .skip(1)
        .any(|arg| arg == "--config" || arg.starts_with("--config="))
}

/// Reject a generation flag passed alongside `--config`.
///
/// The offending-flag set is read out of `Cli`'s own clap `Command` rather than listed here, so a
/// flag added tomorrow is rejected without anyone remembering to update this.
///
/// There is no flags-override-config precedence story on purpose: every override would have to
/// define whether it applies to one crate or all of them, and the honest answer differs per flag. The
/// config file is the edit loop.
pub fn reject_generation_flags(argv: &[String]) -> Result<(), String> {
    use clap::CommandFactory;

    let command = Cli::command();
    let mut longs: BTreeSet<String> = BTreeSet::new();
    let mut shorts: BTreeSet<char> = BTreeSet::new();
    for arg in command.get_arguments() {
        for long in arg.get_long_and_visible_aliases().unwrap_or_default() {
            longs.insert(long.to_owned());
        }
        for short in arg.get_short_and_visible_aliases().unwrap_or_default() {
            shorts.insert(short);
        }
    }

    for token in argv.iter().skip(1) {
        let offender = if let Some(rest) = token.strip_prefix("--") {
            let name = rest.split('=').next().unwrap_or(rest);
            longs.contains(name).then(|| format!("--{name}"))
        } else if let Some(rest) = token.strip_prefix('-') {
            // A short cluster (`-io out`) is not a shape this tool's flags take, but checking every
            // char costs nothing and catches `-s` in `-is`.
            rest.chars()
                .find(|c| shorts.contains(c))
                .map(|c| format!("-{c}"))
        } else {
            None
        };
        if let Some(offender) = offender {
            return Err(format!(
                "`{offender}` cannot be passed with `--config`: every generation flag lives in the \
                 config file, and mixing the two would need a precedence rule that differs per flag \
                 (does a command-line `{offender}` apply to one crate or all of them?). Set it under \
                 `[defaults]`, a `[profiles.<name>]` table, or the `[crates.<name>]` table it belongs \
                 to. Positional crate names are the only command-line selector."
            ));
        }
    }
    Ok(())
}
