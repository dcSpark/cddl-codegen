//! Config declarations, key vocabulary, parsing and settings merging.

use super::derive::lexically_normalized;
use super::{Config, Runtime, quoted};
use crate::log::Verbosity;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

/// Every key a `[crates.<name>]` table holds that a `[defaults]`/`[profiles.*]` table may NOT.
///
/// `input`/`output`/`lib-name` are per-crate by nature — a default for them would make every crate
/// read the same spec or write the same directory, which is never what a multi-crate config means.
/// `profiles` is the reference INTO the layer system, so a profile listing profiles would be the
/// nesting the flat design deliberately excludes. `deps` is an EDGE, and an edge shared by every
/// crate is not a graph: it would make every crate depend on every other one, itself included.
pub(crate) const PER_CRATE_ONLY_KEYS: &[&str] = &[
    "input",
    "output",
    "lib-name",
    "profiles",
    "deps",
    "wasm-reexports",
    "json-schema-deps",
];

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
        "wasm-reexports" | "json-schema-deps" => {
            "`wasm-reexports` and `json-schema-deps` are EDGES too — the ones that decide whose rows \
             a crate's JSON-schema document threads. A shared edge is not a graph: every crate \
             would thread every crate named, itself included."
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
pub(crate) const TOP_LEVEL_KEYS: &[&str] = &["defaults", "profiles", "crates", "runtime"];

/// Every key [`Settings`] holds, as it is spelled in TOML.
///
/// A hand-written mirror of the struct, which is only safe because it is not hand-MAINTAINED: the
/// drift gate `config_keys_match_cli_fields` parses `struct Settings` out of this file with `syn` and
/// requires the two to be the same set, so a field added without a row here fails there.
///
/// It exists because [`settings_from_table`] must know the key set BEFORE serde does. serde's
/// `deny_unknown_fields` reports the keys of the struct it was handed, and the struct it is handed
/// has already had the per-crate-only keys removed — so its message omits exactly the keys a
/// crate-table typo is most likely to be aiming at, and it offers no nearest match at all.
pub(crate) const SETTINGS_KEYS: &[&str] = &[
    "static-dir",
    "export-static-crate",
    "annotate-fields",
    "to-from-bytes-methods",
    "binary-wrappers",
    "preserve-encodings",
    "canonical-form",
    "wasm",
    "component",
    "json-serde-derives",
    "emit-tests",
    "emit-tests-conformance",
    "json-schema-export",
    "package-json",
    "json-schema-scripts",
    "no-synthesized-rust-collection-aliases",
    "preserve-comments",
    "rust-wasm-feature",
    "deserialize-depth-limit",
    "common-import-override",
    "wasm-cbor-json-api-macro",
    "wasm-conversions-macro",
    "wasm-list-macro",
    "wit-package",
    "json-schema-root",
    "workspace-dep",
    "std-forward-dep",
    "extern-import",
    "component-extern-wit",
    "extern-wasm-crate",
    "extern-wrapper-index",
    "wrapper-requests",
    "key-requests",
    "json-schema-dep",
    "json-gen-dep",
    "wasm-dep",
    "rust-dep",
    "component-dep",
    "verbosity",
];

/// How far a key may be from a known one and still be offered as the thing it meant. Two edits
/// covers the realistic typo (a dropped, doubled, swapped or wrong character, or two of them) without
/// reaching the point where several unrelated keys qualify and the "nearest" is arbitrary.
const SUGGEST_WITHIN: usize = 2;

/// What to say about an unknown key: the nearest known key if there is one within
/// [`SUGGEST_WITHIN`] edits, else the whole expected set.
///
/// The full list is the fallback rather than the answer, because the two cases are different
/// questions. A key one character off is a user who knows the vocabulary and mistyped it — the
/// single key they meant is the whole answer, and a 33-entry list buries it. A key resembling
/// nothing is a user who does not know the vocabulary, and there the list IS the answer.
fn unknown_key_advice(key: &str, known: &[&str]) -> String {
    let nearest = known
        .iter()
        .filter_map(|candidate| {
            let distance = strsim::levenshtein(key, candidate);
            (distance <= SUGGEST_WITHIN).then_some((distance, *candidate))
        })
        // Ties broken by name so the suggestion is the same on every machine, like every other
        // ordering in this file.
        .min_by(|(da, a), (db, b)| da.cmp(db).then_with(|| a.cmp(b)));
    match nearest {
        Some((_, candidate)) => format!("did you mean `{candidate}`?"),
        None => {
            let mut sorted: Vec<&str> = known.to_vec();
            sorted.sort_unstable();
            format!("this table understands {}", quoted(sorted.iter().copied()))
        }
    }
}

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
    /// The third face, independent of `wasm`: the wasip2 component crate and its WIT package.
    pub component: Option<bool>,
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
    /// The generated WIT package id (`<ns>:<name>[@<version>]`). A scalar rather than a derivation:
    /// its default reads `lib-name`, which the flag layer already resolves.
    pub wit_package: Option<String>,

    // --- arrays: CONCATENATED across layers, author order preserved within each ---
    #[serde(default)]
    pub json_schema_root: Vec<String>,
    #[serde(default)]
    pub workspace_dep: Vec<String>,
    /// An array rather than a sub-table because the flag takes a bare package name: it is the
    /// std-forwarding HALF of a `rust-dep` entry, whose path side that other key already carries.
    #[serde(default)]
    pub std_forward_dep: Vec<String>,

    // --- `<k>=<v>` sub-tables: per-key UNION across layers, later layer wins per key ---
    #[serde(default)]
    pub extern_import: BTreeMap<String, String>,
    /// The component face's half of a dependency declaration: the dep's committed `component/wit/`
    /// package. A path the tool READS, so it resolves against the config file's directory exactly as
    /// `extern-import` does.
    ///
    /// Derived from a `deps` edge whose two crates both carry the component face
    /// (`Config::apply_graph_edges`); a hand-written entry wins, and is how a dependency outside
    /// this config, or one whose WIT is vendored, gets import mode.
    #[serde(default)]
    pub component_extern_wit: BTreeMap<String, String>,
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
    /// One of the two sub-tables whose right-hand side is a path that is nevertheless NOT resolved
    /// against the config file: it is a cargo path dependency, which cargo resolves against the
    /// manifest it lands in (`<output>/wasm/json-gen/Cargo.toml`). See `argv::argv_fragments`.
    #[serde(default)]
    pub json_gen_dep: BTreeMap<String, String>,
    /// The second, for `<output>/wasm/Cargo.toml`. Same rule, same reason.
    #[serde(default)]
    pub wasm_dep: BTreeMap<String, String>,
    /// The third, for `<output>/rust/Cargo.toml`. Same rule, same reason.
    #[serde(default)]
    pub rust_dep: BTreeMap<String, String>,
    /// The fourth, for `<output>/component/Cargo.toml`. Same rule, same reason.
    #[serde(default)]
    pub component_dep: BTreeMap<String, String>,

    // --- the level key ---
    /// Typed rather than `Option<String>` for two reasons. A bad value (`verbosity = "loud"`) is
    /// rejected by SERDE, at config-parse time, with the valid variants named — rather than
    /// surfacing later as a clap error about a flag the user never typed. And the editor schema,
    /// which derives itself from these field types, can emit an `enum` of the five names and
    /// therefore autocomplete them.
    ///
    /// Declared LAST because the schema renders properties in declaration order.
    pub verbosity: Option<Verbosity>,
}

impl Settings {
    /// Fold `over` onto `self`: `over` is the LATER layer and wins.
    ///
    /// Written as an exhaustive destructure on purpose. A new `Cli` field adds a `Settings` field,
    /// and an exhaustive pattern makes forgetting it here a COMPILE error rather than a key that
    /// parses and is then silently dropped between layers — the one drift this file cannot detect at
    /// runtime, since a dropped key looks exactly like an absent one.
    pub(super) fn merge_over(&mut self, over: &Settings) {
        let Settings {
            static_dir,
            export_static_crate,
            annotate_fields,
            to_from_bytes_methods,
            binary_wrappers,
            preserve_encodings,
            canonical_form,
            wasm,
            component,
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
            wit_package,
            json_schema_root,
            workspace_dep,
            std_forward_dep,
            extern_import,
            component_extern_wit,
            extern_wasm_crate,
            extern_wrapper_index,
            wrapper_requests,
            key_requests,
            json_schema_dep,
            json_gen_dep,
            wasm_dep,
            rust_dep,
            component_dep,
            verbosity,
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
            component,
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
            wit_package,
            verbosity,
        );

        // Arrays CONCATENATE rather than replace: these are additive per-item lists, and
        // `--json-schema-root` is order-significant (roots emit after every spec-derived row, in flag
        // order), so "later wins" would mean a crate adding one root silently discards the shared
        // list `[defaults]` exists to hold.
        self.json_schema_root.extend(json_schema_root.clone());
        self.workspace_dep.extend(workspace_dep.clone());
        self.std_forward_dep.extend(std_forward_dep.clone());

        // Sub-tables union per key — the same accumulation a repeated `<k>=<v>` flag already gets by
        // landing in a `BTreeMap`. A later layer overrides only the keys it names.
        macro_rules! table {
            ($($f:ident),* $(,)?) => {$(
                for (k, v) in $f { self.$f.insert(k.clone(), v.clone()); }
            )*};
        }
        table!(
            extern_import,
            component_extern_wit,
            extern_wasm_crate,
            extern_wrapper_index,
            wrapper_requests,
            key_requests,
            json_schema_dep,
            json_gen_dep,
            wasm_dep,
            rust_dep,
            component_dep,
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
    /// Names of other `[crates.*]` entries whose WASM classes ship in this crate's package without
    /// this crate's spec referencing them — CML's "not actual dependencies but we re-export these
    /// for the wasm builds", promoted from a comment in a manifest to a declaration.
    ///
    /// It is a packaging fact and nothing else: no rust/extern edge, no generation-order edge. Its
    /// only effect is that it joins `deps` as a source for the JSON-schema threading derivation —
    /// see `Config::threading`, which is where the reason a package's composition (rather than a
    /// spec's references) is the right source lives.
    pub wasm_reexports: Vec<String>,
    /// Explicit override of the threading derivation for this crate. `Some(list)` REPLACES
    /// `deps ∪ wasm-reexports` entirely (`Some(vec![])` threads nothing); `None` derives.
    pub json_schema_deps: Option<Vec<String>>,
    pub settings: Settings,
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
    let base_dir = path.parent().unwrap_or(Path::new(""));
    // Absolutized HERE, at the one place the CWD legitimately participates (it already located the
    // config file this function just read). Every path key then resolves to an absolute path, so no
    // downstream computation — in particular `manifest_relative_path`, whose result lands in a
    // COMMITTED `Cargo.toml` — ever consults the CWD again: with a relative base, a config mixing an
    // absolute `output` with a relative one made the derived manifest path a function of where the
    // tool was invoked from. Lexical, not `canonicalize`: resolving symlinks would rewrite the paths
    // the user spelled, and the join needs no filesystem access.
    let base_dir = if base_dir.is_absolute() {
        base_dir.to_path_buf()
    } else {
        let cwd = std::env::current_dir()
            .map_err(|e| format!("--config {}: cannot read the current directory to resolve the config file's location: {e}", path.display()))?;
        lexically_normalized(&cwd.join(base_dir))
    };
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

    let runtime = match doc.get("runtime") {
        Some(v) => Some(
            Runtime::deserialize(v.clone())
                .map_err(|e| format!("[runtime]: {}", e.to_string().trim_end()))?,
        ),
        None => None,
    };

    const NO_CRATES: &str = "no `[crates.<name>]` tables; a config generates at least one crate";
    let crate_tables = doc.get("crates").ok_or_else(|| NO_CRATES.to_owned())?;
    let crate_tables = as_table(crate_tables, "crates")?;
    if crate_tables.is_empty() {
        return Err(NO_CRATES.to_owned());
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
        let profiles = opt_string_array(table, "profiles", &label)?.unwrap_or_default();
        let deps = opt_string_array(table, "deps", &label)?.unwrap_or_default();
        let wasm_reexports = opt_string_array(table, "wasm-reexports", &label)?.unwrap_or_default();
        // `Option`, unlike its neighbours: an ABSENT key derives while an EMPTY array threads
        // nothing, and those are different requests. `Vec::new()` cannot tell them apart.
        let json_schema_deps = opt_string_array(table, "json-schema-deps", &label)?;
        crates.insert(
            name.clone(),
            CrateEntry {
                input,
                output,
                lib_name,
                profiles,
                deps,
                wasm_reexports,
                json_schema_deps,
                settings,
            },
        );
    }

    let config = Config {
        base_dir: base_dir.to_path_buf(),
        defaults,
        profiles,
        crates,
        runtime,
        static_dir_override: None,
        verbosity_override: None,
    };
    config.validate()?;
    Ok(config)
}

fn as_table<'a>(value: &'a toml::Value, label: &str) -> Result<&'a toml::Table, String> {
    value
        .as_table()
        .ok_or_else(|| format!("`{label}` must be a table"))
}

/// A key whose value is a required, NON-EMPTY string.
///
/// Empty is refused rather than passed on, because neither of the two keys that use this
/// (`input`/`output`) has a defensible meaning for it and the failures are worse than the omission
/// they resemble. An empty `output` resolves to the config file's own directory, which as a component
/// sequence is a prefix of every other crate's output — so the clobber guard reports the config
/// directory containing a crate's output, a diagnostic naming neither the empty value nor the key
/// that holds it. A lone crate escapes the guard entirely and reaches clap, which refuses the empty
/// value against a flag the user never typed.
fn required_string(table: &toml::Table, key: &str, label: &str) -> Result<String, String> {
    match table.get(key) {
        Some(v) => {
            let value = v
                .as_str()
                .ok_or_else(|| format!("{label}.{key} must be a string"))?;
            if value.trim().is_empty() {
                return Err(format!(
                    "{label}.{key} is empty; it must name a path. An empty `{key}` is not the same \
                     as an absent one — it resolves to the config file's own directory."
                ));
            }
            Ok(value.to_owned())
        }
        None => Err(format!("{label} has no `{key}`; it is required")),
    }
}

/// An optional string-array key of `table`: `None` when absent, else [`string_array`] under the
/// `<label>.<key>` label.
fn opt_string_array(
    table: &toml::Table,
    key: &str,
    label: &str,
) -> Result<Option<Vec<String>>, String> {
    table
        .get(key)
        .map(|v| string_array(v, &format!("{label}.{key}")))
        .transpose()
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
///
/// The unknown-key check is ours rather than serde's, because the key set serde can see is the wrong
/// one. By the time it runs, the per-crate-only keys have been split off, so `deny_unknown_fields`
/// would report a crate table's vocabulary MINUS exactly the keys that are per-crate — and a
/// `dep`-for-`deps` typo would be told about every key except the one it meant. Ours knows which
/// table it is in ([`SETTINGS_KEYS`] alone for a shared table, plus [`PER_CRATE_ONLY_KEYS`] for a
/// crate table) and can therefore also offer a nearest match. serde's `deny_unknown_fields` stays on
/// as the backstop: unreachable for KEYS now, still what rejects a value of the wrong shape.
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
        if !SETTINGS_KEYS.contains(&key.as_str()) {
            // The known set is the one THIS table has: a crate table's suggestion may name a
            // per-crate-only key, a shared table's may not — `json-schema-deps` is the nearest
            // neighbour of several plausible typos and suggesting it in `[defaults]` would send the
            // user to a key that table cannot hold.
            let mut known: Vec<&str> = SETTINGS_KEYS.to_vec();
            if allow_per_crate_keys {
                known.extend_from_slice(PER_CRATE_ONLY_KEYS);
            }
            return Err(format!(
                "unknown key `{key}` in {label}: {}",
                unknown_key_advice(key, &known)
            ));
        }
        rest.insert(key.clone(), value.clone());
    }
    Settings::deserialize(toml::Value::Table(rest))
        .map_err(|e| format!("{label}: {}", e.to_string().trim_end()))
}
