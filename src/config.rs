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
use crate::log::Verbosity;
// The generated layout, from the emitter that writes it rather than re-spelled here — see
// `generation::layout` for why every one of these is a shared fact and not a local string.
use crate::generation::layout::{
    COMPONENT_DIR, COMPONENT_WIT_DIR, EXTERN_INTERFACE_DIR, JSON_GEN_DIR, JSON_GEN_PACKAGE_SUFFIX,
    RUST_BORROWED_KEY_TYPES, WASM_BORROWED_COLLECTIONS, WASM_COLLECTIONS_INDEX,
    WASM_PACKAGE_SUFFIX,
};
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

/// The header `--print-flags` opens with. It says the three things that stop the listing being read
/// as a command: the left column is a TOML key rather than an argument, nothing is quoted, and the
/// tool generated nothing. See [`Config::flag_listing`] for why the format is deliberately not
/// pasteable.
const PRINT_FLAGS_PREAMBLE: &str = "\
# The flags each crate WOULD be generated with, and the config key each one comes from.
# This is a listing, not a command line: the left column is a config key rather than an
# argument, nothing is shell-quoted, and a copy of it stops being true at the next edit of
# the config. Nothing was generated.
";

/// The tables the document may hold at top level. Anything else is a typo or a feature this version
/// does not have; either way the user must hear about it rather than have the table ignored.
pub(crate) const TOP_LEVEL_KEYS: &[&str] = &["defaults", "profiles", "crates", "runtime"];

/// The [`Cli`] arguments [`reject_generation_flags`] does NOT harvest, by clap arg id (the `Cli`
/// field name) rather than by long spelling, so a renamed flag keeps its exemption or loses it
/// loudly.
///
/// The criterion is not "harmless" but "has no per-crate precedence question": `--static-dir` names
/// where THIS MACHINE keeps the tool's own hand-written runtime, so there is exactly one answer to
/// "which crate does it apply to" — all of them — and that is what every other generation flag
/// cannot say. It is also the one flag a config file cannot get right by itself: the value is a
/// property of a checkout, and a config is committed.
///
/// `--verbosity` meets the same criterion rather than widening it: there is exactly one answer to
/// "which crate does a command-line `--verbosity` apply to", namely all of them. What differs is
/// only WHY the command line is the right place for it — the key is the project's committed default
/// and the flag is this invocation's override of it ("not this run"), so the override winning
/// silently is the intended use rather than a conflict to report. Unlike `static-dir` it is also a
/// value a config CAN get right, per crate, which is exactly why the key exists too.
const EXEMPT_ARG_IDS: &[&str] = &["static_dir", "verbosity"];

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

/// Levenshtein distance, capped: the caller only ever asks "is this within 2?", so the row-by-row
/// walk bails as soon as the whole row exceeds the cap.
///
/// Implemented here rather than pulled in as a dependency — it is eleven lines, and a new crate in
/// the tree to spell-check config keys is not a trade worth making.
fn edit_distance_within(a: &str, b: &str, cap: usize) -> Option<usize> {
    let b: Vec<char> = b.chars().collect();
    if a.chars().count().abs_diff(b.len()) > cap {
        return None;
    }
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut row: Vec<usize> = vec![0; b.len() + 1];
    for (i, ca) in a.chars().enumerate() {
        row[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let substitute = prev[j] + usize::from(ca != *cb);
            row[j + 1] = substitute.min(prev[j + 1] + 1).min(row[j] + 1);
        }
        if row.iter().min().copied().unwrap_or(0) > cap {
            return None;
        }
        std::mem::swap(&mut prev, &mut row);
    }
    let distance = prev[b.len()];
    (distance <= cap).then_some(distance)
}

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
            edit_distance_within(key, candidate, SUGGEST_WITHIN).map(|d| (d, *candidate))
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
    /// [`Config::runtime_carrier`] for which.
    pub export_static_crate: Option<String>,
    /// Expands to `--common-import-override <value>` on every crate. It is the LOWEST layer in the
    /// merge: an explicit `common-import-override` in `[defaults]`, a profile, or a crate table wins
    /// for the crates it reaches, which is the exotic case (a crate importing a different runtime)
    /// this key is sugar for the common one of.
    pub common_import: Option<String>,
    /// Name the carrier by hand instead of deriving it, accepting the remaining unsupported
    /// flavor/depth-limit contract that made the derivation refuse. See [`Config::runtime_carrier`].
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
    /// ([`Config::apply_graph_edges`]); a hand-written entry wins, and is how a dependency outside
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
    /// manifest it lands in (`<output>/wasm/json-gen/Cargo.toml`). See `argv_fragments`.
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
    /// see [`Config::threading`], which is where the reason a package's composition (rather than a
    /// spec's references) is the right source lives.
    pub wasm_reexports: Vec<String>,
    /// Explicit override of the threading derivation for this crate. `Some(list)` REPLACES
    /// `deps ∪ wasm-reexports` entirely (`Some(vec![])` threads nothing); `None` derives.
    pub json_schema_deps: Option<Vec<String>>,
    pub settings: Settings,
}

/// One argv fragment — a whole flag occurrence (`["--input=<path>"]`, or a switch's lone token)
/// tagged with the config key that produced it. The tag exists so a clap rejection can be reported
/// against the TOML line the user wrote; [`Config::flag_listing`] prints the same tag, which is what
/// turns "what flags does this config use" into "and which key put each one there". A `Vec` although
/// every fragment is one token today: it is what lets a future flag whose spelling clap does not
/// accept after an `=` be emitted without changing the tag's meaning.
///
/// The tag is owned rather than `&'static str` because one of them is not a config key at all —
/// `command line`, for a value passed alongside `--config` — and another names the crate that caused
/// it (see [`Provenance`]).
type Fragment = (String, Vec<String>);

/// Which config key derived a `<k>=<v>` sub-table entry or an array item that a user could equally
/// have written by hand, keyed by `(flag name, the entry's own key or value)`.
///
/// Needed because the sugar writes its derivations into the same [`Settings`] fields a hand-written
/// key lands in, and by the time [`argv_fragments`] walks them the two are indistinguishable. Without
/// this a `deps`-derived `--extern-import` would be tagged `extern-import` — a key the user never
/// wrote and cannot grep for — in the listing AND in a clap rejection. The threading derivations do
/// not appear here: a [`DerivedThread`] carries its own key already, because it never merges into a
/// sub-table.
///
/// The value is an owned `String` rather than a `&'static str` because a REVERSE edge's provenance
/// names the crate that caused it: `deps` is where to look, but not in THIS crate's table — see
/// [`Config::apply_graph_edges`].
type Provenance = BTreeMap<(&'static str, String), String>;

/// One derived JSON-schema thread, as the two flag values it expands to.
///
/// A `Vec` of these rather than entries folded into [`Settings`]'s two sub-tables, because those are
/// `BTreeMap`s: they emit in NAME order, and `--json-schema-dep` is order-significant — flag order
/// is registration order, which decides which crate a published-name collision blames. The ordered
/// forms are the config's arrays (`deps`, `wasm-reexports`, `json-schema-deps`), so the derivation
/// carries their order through to argv instead of losing it in a map.
///
/// Each half is optional so a hand-written sub-table entry can override one of them alone.
#[derive(Clone, Debug, PartialEq)]
struct DerivedThread {
    /// The config key that produced this thread, for attributing a clap rejection to a TOML line.
    key: &'static str,
    /// `--json-schema-dep` value: `<dep lib normalized>=<dep lib normalized>_json_schema_gen`.
    json_schema_dep: Option<String>,
    /// `--json-gen-dep` value: `<dep lib-name>-json-schema-gen=<relative path>`.
    json_gen_dep: Option<String>,
}

/// One derived manifest `[dependencies]` line, as the flag value it expands to: a `--wasm-dep` for
/// the consumer's generated `wasm/Cargo.toml` ([`Config::wasm_deps`]), or a `--rust-dep` for its
/// `rust/Cargo.toml` ([`Config::rust_deps`]).
///
/// One struct for both, because the two carry the identical pair — a config key to attribute a clap
/// rejection to, and a `<package>=<path>` value — and which flag a value belongs to is decided by
/// the list it is emitted from rather than by anything inside it.
struct DerivedManifestDep {
    /// The config key that produced it (`deps` or `wasm-reexports`), for attributing a clap
    /// rejection to a TOML line.
    key: &'static str,
    /// `<cargo package name>=<relative path>`.
    value: String,
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
        let wasm_reexports = match table.get("wasm-reexports") {
            Some(v) => string_array(v, &format!("{label}.wasm-reexports"))?,
            None => Vec::new(),
        };
        // `Option`, unlike its two neighbours: an ABSENT key derives while an EMPTY array threads
        // nothing, and those are different requests. `Vec::new()` cannot tell them apart.
        let json_schema_deps = match table.get("json-schema-deps") {
            Some(v) => Some(string_array(v, &format!("{label}.json-schema-deps"))?),
            None => None,
        };
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

            self.validate_crate_names(name, "wasm-reexports", &entry.wasm_reexports)?;
            if let Some(explicit) = &entry.json_schema_deps {
                self.validate_crate_names(name, "json-schema-deps", explicit)?;
            }

            // One crate reached through both edges is not two facts about it: `deps` already makes
            // the dependency's classes ship in this package, so `wasm-reexports` adds nothing and
            // the derivation would emit the same thread twice — which `--json-schema-dep` itself
            // rejects as an ambiguous label. Caught here so the message names the config keys
            // rather than the flag the user never typed.
            if let Some(both) = entry
                .wasm_reexports
                .iter()
                .find(|name| entry.deps.contains(name))
            {
                return Err(format!(
                    "[crates.{name}] lists `{both}` in both `deps` and `wasm-reexports`. The edge \
                     exists once: `deps` already puts the dependency's classes in this package, so \
                     `wasm-reexports` adds nothing and the JSON-schema thread would be derived \
                     twice — which `--json-schema-dep` rejects as one label under two mappings. \
                     Keep `deps`, which carries the rust/extern edge as well."
                ));
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

        // Two crates writing into one `output` — or one writing inside another's — is the
        // destructive case, and the only one this file can catch before anything is written.
        // Generation replaces a crate's `src/generated/**` wholesale, so whichever crate runs second
        // erases the first's modules while the first's seed-once `lib.rs` survives: a crate root
        // belonging to one spec over a generated tree belonging to another, reported as success. It
        // is also the copy-paste error a multi-crate TOML invites most — duplicate a `[crates.*]`
        // block, edit `input`, forget `output`. Compared lexically on the RESOLVED paths, since
        // neither directory need exist yet; `Path::starts_with` is component-wise, so `gen/ab` is
        // correctly not inside `gen/a`. NORMALIZED before comparing, because a `.` or `..` in an
        // `output` is a spelling: `Path::components` keeps a leading `.` and every `..`, so without
        // normalization `./gen/x` vs `gen/x` (and any `..` spelling) walk past the guard and the
        // second crate silently erases the first's generated tree — the exact destruction this
        // check exists for.
        let resolved: Vec<(&String, PathBuf)> = self
            .crates
            .iter()
            .map(|(name, entry)| {
                (
                    name,
                    lexically_normalized(Path::new(&resolve_path(&self.base_dir, &entry.output))),
                )
            })
            .collect();
        for (index, (name, path)) in resolved.iter().enumerate() {
            for (other_name, other) in &resolved[index + 1..] {
                let (inner, outer, inner_name, outer_name) = if other.starts_with(path) {
                    (other, path, other_name, name)
                } else if path.starts_with(other) {
                    (path, other, name, other_name)
                } else {
                    continue;
                };
                return Err(if inner == outer {
                    format!(
                        "[crates.{name}] and [crates.{other_name}] both generate into `{}`. A \
                         crate's output is regenerated as a whole, so whichever ran second would \
                         erase the other's generated tree while leaving its crate root behind — and \
                         the run would report success. Give each crate its own `output`.",
                        path.display()
                    )
                } else {
                    format!(
                        "[crates.{outer_name}] generates into `{}`, which contains \
                         [crates.{inner_name}]'s `{}`. A crate's output is regenerated as a whole, \
                         so the outer crate's run would clobber the inner crate's tree. Give them \
                         sibling directories instead.",
                        outer.display(),
                        inner.display()
                    )
                });
            }
        }

        // Reuses the label map the duplicate check just built, which is what makes "targets a
        // same-config crate" the SAME resolution the committed-state verdict performs.
        self.validate_same_config_edges_are_deps(&by_lib)?;

        self.validate_runtime()?;
        // Separate from `validate_runtime` because it must run when there is no `[runtime]` table at
        // all: `export-static-crate` is an ordinary `Settings` key, so `[defaults]` alone reaches
        // every crate.
        self.validate_one_export_site()?;

        self.generation_order().map(|_| ())
    }

    /// The three shape checks a crate-name array carries: no self-reference, every name configured,
    /// no name twice.
    ///
    /// Shared by the two THREADING arrays, whose consequence is one sentence either way (a thread
    /// is a registrar call into another crate's document). `deps` keeps its own copies rather than
    /// calling this: each of its messages explains what the derived rust/extern EDGE would have
    /// done, which is a different thing to say at each of the three.
    fn validate_crate_names(&self, owner: &str, key: &str, names: &[String]) -> Result<(), String> {
        let mut seen = BTreeSet::new();
        for named in names {
            if named == owner {
                return Err(format!(
                    "[crates.{owner}].{key} lists `{owner}` itself. A crate's own rows are already \
                     in its own schema document; threading it into itself would register them a \
                     second time."
                ));
            }
            if !self.crates.contains_key(named) {
                return Err(format!(
                    "[crates.{owner}].{key} names `{named}`, which has no `[crates.{named}]` \
                     table. Both derived values come from the named crate's OWN entry (its \
                     `lib-name` and its `output`), so a crate outside this config cannot be sugar \
                     — spell it with the raw `[crates.{owner}.json-schema-dep]` and \
                     `[crates.{owner}.json-gen-dep]` sub-tables instead. Configured crates: {}",
                    list_or_none(self.crates.keys())
                ));
            }
            if !seen.insert(named.clone()) {
                return Err(format!(
                    "[crates.{owner}].{key} lists `{named}` twice; one mention is one thread, and \
                     the second would emit the same registrar call again — which \
                     `--json-schema-dep` rejects as one label under two mappings."
                ));
            }
        }
        Ok(())
    }

    /// Inside one config, an edge onto a crate the SAME config generates is `deps` — and this is
    /// where that standing rule stops being prose.
    ///
    /// # What goes wrong without it
    ///
    /// A crate that hand-spells a cross-crate path at a same-config crate, with no `deps` edge
    /// behind it, is outside every convergence instrument at once and each for its own reason:
    /// [`Convergence`] watches request SIDECARS and this crate neither reads nor writes one, the
    /// convergence pass re-runs only the crates `Convergence` named, and [`Self::committed_verdict`]
    /// walks `deps` edges and there is no edge. So the run exits 0 while the value it read was the
    /// dependency's output MID-RUN: with an `extern-wrapper-index` at an index the dependency has
    /// not populated yet, the consumer mints a wrapper class the dependency ends the same run
    /// hosting too, and run 2 — reading the now-populated index — defers and writes different bytes.
    /// `run twice = run once` is the property the convergence pass exists to make true, and this is
    /// the one shape that reaches around all of it.
    ///
    /// Refused rather than repaired by inferring the edge: an inferred edge would put generation
    /// ORDER and convergence membership under something the user never wrote, so the config's
    /// declared graph would stop matching its effective one. Refused rather than warned because a
    /// warning leaves the broken property reachable — exit 0, different bytes on run 2.
    ///
    /// # Which sub-tables, and why not the others
    ///
    /// The rule covers exactly the entries whose value is a PATH INTO ANOTHER CRATE'S OUTPUT — the
    /// forward reads (`extern-import`, `component-extern-wit`, `extern-wrapper-index`) and the
    /// reverse sidecar reads (`wrapper-requests`, `key-requests`). Those are the cross-crate reads a
    /// same-config crate's own run can move underneath, which is the whole hazard above.
    ///
    /// The two NAME-valued sub-tables are deliberately NOT covered. `extern-wasm-crate` names a
    /// cargo crate and `json-schema-dep` a rust module path emitted verbatim into generated code
    /// (the split [`argv_fragments`] already draws, since neither is path-resolved): nothing of ours
    /// moves underneath either, no index or sidecar is read, so no run of this config can make run 2
    /// differ from run 1 through them. They also have a legitimate same-config population — a crate
    /// whose spec carries a HAND-WRITTEN `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` stub for a type
    /// another crate in this config happens to generate, which needs the wasm face named without an
    /// extern-interface import; there `deps` is not merely unnecessary but refused, since the
    /// `--extern-import` it derives collides with the stub
    /// ([`validate_extern_import_stubs`]). The duplicate-wrapper exposure that population does carry
    /// has its own remedy in the spec's own vocabulary (`@wasm_extern_companions`).
    ///
    /// The four cargo-manifest tables (`json-gen-dep`, `wasm-dep`, `rust-dep`, `component-dep`) are
    /// out of scope for a different reason: they are keyed by cargo PACKAGE name rather than by a
    /// library label, so they resolve through no label map, and a hand path-dep onto a same-config
    /// crate's generated crate is an ordinary manifest fact that creates no cross-crate read.
    ///
    /// Also out of scope, and stated so a reader does not mistake it for an oversight: an entry
    /// keyed by an out-of-config name whose VALUE path happens to point into a same-config crate's
    /// output. Resolution here is by LABEL, exactly as the committed-state verdict resolves `deps`;
    /// a mislabeled path entry is a different defect, and it announces itself in the generated `use`
    /// lines, which carry the wrong crate name.
    ///
    /// Over MERGED settings, so a `[defaults]` or profile entry is judged against every crate it
    /// reaches: the misconfiguration is per merged crate, not per layer. Pure input-side — the
    /// config text and its own crate list — so nothing in the determinism contract is touched.
    fn validate_same_config_edges_are_deps(
        &self,
        by_lib: &BTreeMap<String, &String>,
    ) -> Result<(), String> {
        for (name, entry) in &self.crates {
            let settings = self.merged_settings(entry);
            for (key, table) in [
                ("extern-import", &settings.extern_import),
                ("component-extern-wit", &settings.component_extern_wit),
                ("extern-wrapper-index", &settings.extern_wrapper_index),
            ] {
                for label in table.keys() {
                    // An out-of-config label is what these sub-tables are FOR, so it is the common
                    // case and the one that walks past this check.
                    let Some(target) = by_lib.get(label.as_str()) else {
                        continue;
                    };
                    // The override population: a hand entry for a key a `deps` edge also derives is
                    // the documented way to point one half of an edge somewhere else (a vendored
                    // copy of a dependency's export). The edge exists, so the instruments see it.
                    if entry.deps.iter().any(|dep| dep == *target) {
                        continue;
                    }
                    return Err(format!(
                        "[crates.{name}].{key} names `{label}`, which is `[crates.{target}]` in \
                         this config. Inside one config an edge onto a crate the config itself \
                         generates is `deps`: declare `deps = [\"{target}\"]` in `[crates.{name}]` \
                         and drop the hand-spelled `{key}` entry — the config derives that value \
                         from `{target}`'s own `output` and `lib-name`, and the edge becomes one \
                         the convergence checks can see. Hand-spelled, it is invisible to them: the \
                         path is read while `{target}` is still mid-run, so the run exits 0 and the \
                         next one over the unchanged tree writes different bytes. A hand-spelled \
                         `{key}` is for a dependency this config does NOT generate, whose committed \
                         output cannot move underneath a run."
                    ));
                }
            }

            // The reverse direction: the key names a CONSUMER of this crate, so the edge that must
            // exist is the consumer's, and the remedy is written in the consumer's table.
            for (key, table) in [
                ("wrapper-requests", &settings.wrapper_requests),
                ("key-requests", &settings.key_requests),
            ] {
                for label in table.keys() {
                    let Some(target) = by_lib.get(label.as_str()) else {
                        continue;
                    };
                    if self.crates[*target].deps.iter().any(|dep| dep == name) {
                        continue;
                    }
                    return Err(format!(
                        "[crates.{name}].{key} names `{label}`, which is `[crates.{target}]` in \
                         this config. Inside one config an edge onto a crate the config itself \
                         generates is `deps`, and this edge belongs to the consumer: declare \
                         `deps = [\"{name}\"]` in `[crates.{target}]` and drop the hand-spelled \
                         `{key}` entry — the config derives that value from `{target}`'s own \
                         `output`, and the edge becomes one the convergence checks can see. \
                         Hand-spelled, it is invisible to them: the sidecar is read while \
                         `{target}` is still mid-run, so the run exits 0 and the next one over the \
                         unchanged tree writes different bytes. A hand-spelled `{key}` is for a \
                         consumer this config does NOT generate, whose committed sidecar cannot \
                         move underneath a run."
                    ));
                }
            }
        }
        Ok(())
    }

    /// `wasm-reexports` may only name a crate that HAS a wasm crate.
    ///
    /// The key says one thing: this crate's wasm package ships the named crate's classes as well as
    /// its own. A crate with `wasm = false` generates no wasm crate, so there are no classes to
    /// ship and the declaration is false at the coarsest level at which it could be — the one level
    /// this config can check. Left unchecked it is silent: the named crate is simply skipped by the
    /// threading derivation (which filters on `json-schema-export`), so a user who wrote the key
    /// expecting their package to carry another crate's surface gets no diagnostic and no effect.
    ///
    /// Here rather than in [`Self::validate`] because `wasm` is a merged value — `[defaults]`, a
    /// profile and the crate table can each set it — so the check needs each crate's finished `Cli`,
    /// exactly like the two `json-schema-export` refusals in [`Self::threading`]. Still before any
    /// crate generates, which is the property that matters.
    ///
    /// Only the NAMED side is checked. A `wasm = false` crate *declaring* `wasm-reexports` is a
    /// separate statement (a package with no wasm crate of its own) and is left to the derivation,
    /// which emits nothing for it.
    fn validate_wasm_reexports(&self, ungraphed: &BTreeMap<String, Cli>) -> Result<(), String> {
        for (name, entry) in &self.crates {
            for reexport in &entry.wasm_reexports {
                // Validated to name a configured crate by `validate_crate_names`.
                if !ungraphed[reexport.as_str()].wasm {
                    return Err(format!(
                        "[crates.{name}].wasm-reexports names `{reexport}`, which has `wasm = \
                         false`. The key says `{name}`'s wasm package ships `{reexport}`'s classes \
                         alongside its own, and `{reexport}` generates no wasm crate — there are no \
                         classes to ship. Turn `wasm` on for `{reexport}`, or drop it from the list."
                    ));
                }
            }
        }
        Ok(())
    }

    /// The component face's BYTES SEAM has a precondition, and this is the one place that can check
    /// it: both ends of a `deps` edge must ENCODE THE SAME WAY.
    ///
    /// A dependency-typed value crossing the component boundary is serialized by one crate and
    /// deserialized by the other, so the crossing preserves the value only while the two agree about
    /// what CBOR they write and accept. A mismatch does not fail anything: every crossing silently
    /// re-encodes, which is the failure class that costs the most to find. Config mode sees both
    /// ends of the edge, so it refuses before anything is written; a hand-written flag invocation
    /// sees one crate at a time and can only document the obligation.
    ///
    /// # Scope: seam edges only
    ///
    /// The check applies to a `deps` edge exactly when the edge CARRIES the seam — this crate has
    /// `component`, and the dependency is in import mode ([`Self::component_seam_edge`]). On every
    /// other `deps` edge the dependency's types are reached by ordinary rust linkage: no bytes are
    /// produced at a boundary and none are parsed there, so there is no crossing to re-encode and
    /// nothing for this rule to be about. Widening it would attach a message that explains itself in
    /// terms of crossings to edges that have none, and would newly reject configs that generate
    /// correctly today. If posture skew across a plain `--extern-import` edge is also a problem it is
    /// a different one, with a different mechanism and a different remedy, and folding it under a
    /// component-flavored message would hide it rather than report it.
    ///
    /// # Why [`RuntimeFlavor::equality_axes`] rather than a list of its own
    ///
    /// It answers the same question at a second level. `[runtime]` asks which flags make two crates'
    /// serialization contracts non-interchangeable in SOURCE (one runtime crate compiled into both);
    /// the seam asks it of BYTES (one crate's output parsed by another). Each of the three axes has a
    /// stake in both: `preserve-encodings` and `canonical-form` change what bytes come out, and
    /// `deserialize-depth-limit` changes which bytes are accepted, so a crossing the producer
    /// considers well-formed is one the consumer rejects. A sibling list would be a second copy of a
    /// fact that changes — a fourth axis minted for the runtime is a fourth axis for the seam, and
    /// the copy would silently not get it.
    ///
    /// Over EVERY crate rather than the selection, for the reason [`Self::validate_wasm_reexports`]
    /// states: whether an edge can mean anything is a property of the config, so `--config c.toml
    /// ledger` must reject what a full run rejects.
    fn validate_component_seam_posture(
        &self,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<(), String> {
        for (name, entry) in &self.crates {
            let consumer_cli = &ungraphed[name];
            for dep in &entry.deps {
                // Validated to name a configured crate by the `deps` checks in `validate`.
                let dep_entry = &self.crates[dep];
                let dep_cli = &ungraphed[dep.as_str()];
                if !Self::component_seam_edge(
                    consumer_cli,
                    dep_cli,
                    &normalized(&dep_entry.lib_name),
                ) {
                    continue;
                }
                let ours = RuntimeFlavor::of(consumer_cli).equality_axes();
                let theirs = RuntimeFlavor::of(dep_cli).equality_axes();
                for ((axis, ours), (_, theirs)) in ours.iter().zip(theirs.iter()) {
                    if ours == theirs {
                        continue;
                    }
                    return Err(format!(
                        "[crates.{name}].deps names `{dep}`, and the two disagree on `{axis}`: \
                         `{name}` has `{ours}`, `{dep}` has `{theirs}`. With `component` on both, \
                         `{dep}`'s types cross the component boundary as CBOR bytes — one crate's \
                         serializer writes them and the other's deserializer reads them — and that \
                         round trip preserves a value only while both encode by the same rules. A \
                         mismatch does not fail: every crossing silently re-encodes. Give both \
                         crates the same `{axis}`, or turn `component` off for one of them (without \
                         the seam `{dep}`'s types are reached by ordinary rust linkage and the two \
                         postures are independent)."
                    ));
                }
            }
        }
        Ok(())
    }

    /// Whether a `deps` edge carries the component face's bytes seam: this crate emits a component,
    /// and the dependency is in IMPORT MODE — its types cross as imported WIT resources rather than
    /// being excluded from the projection.
    ///
    /// One predicate for the two readers that must agree about it: [`Self::apply_graph_edges`],
    /// which derives the flags that CREATE the seam, and [`Self::validate_component_seam_posture`],
    /// which refuses one that cannot be byte-exact. The `component_extern_wit` term is what keeps
    /// them agreeing when a user spells the entry by hand for a dependency whose own `component` is
    /// off: the derivation would not write it, but the seam is there all the same.
    fn component_seam_edge(consumer: &Cli, dep: &Cli, key: &str) -> bool {
        consumer.component
            && (dep.component || consumer.component_extern_wit_paths().contains_key(key))
    }

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

    /// At most ONE crate in this config writes the shared static runtime.
    ///
    /// Two export sites over one directory is not two writes of the same bytes. At differing
    /// flavors the second export does not REPLACE the first: the flavor-specific files the first
    /// wrote sit outside the stale-file scan and linger, the exported manifest accumulates the union
    /// of both flavors' dependencies, and the comment-preservation overlay reads the previous
    /// flavor's output, cannot classify it, and injects a fresh `compile_error!` block — every run.
    /// Measured on a two-crate config differing only in `preserve-encodings`: the exported
    /// `any_cbor.rs` grew 62 → 143 → 224 → 305 `compile_error!` blocks over four runs of one
    /// unchanged config (103 K → 509 K bytes), exit 0 each time. That is `run twice = run once =
    /// clean run` broken, so it is refused before anything is written.
    ///
    /// Refused whatever the two flavors are: at equal flavors the second export is a redundant
    /// rewrite of the first, and the flavor is not knowable here anyway — it is read off an
    /// expanded `Cli`, which parse-time validation does not have.
    ///
    /// The two shapes are counted differently on purpose. When `[runtime]` writes the runtime,
    /// exactly one crate carries the flag, so a SECOND site is any layer that also sets the key and
    /// the layer is what the message names. Without `[runtime]`, `export-static-crate` is an
    /// ordinary [`Settings`] key: one `[defaults]` line is a single layer and as many export sites
    /// as there are crates, so the count is over CRATES.
    fn validate_one_export_site(&self) -> Result<(), String> {
        // Rejected rather than resolved by precedence: two static-runtime exports in one config is a
        // mistake, and letting one win would make WHICH runtime survives depend on generation order
        // — the property the `[runtime]` table exists to take out of the user's hands.
        if self
            .runtime
            .as_ref()
            .is_some_and(|runtime| runtime.export_static_crate.is_some())
        {
            let mut layers: Vec<(String, &Settings)> =
                vec![("[defaults]".to_owned(), &self.defaults)];
            for (name, settings) in &self.profiles {
                layers.push((format!("[profiles.{name}]"), settings));
            }
            for (name, entry) in &self.crates {
                layers.push((format!("[crates.{name}]"), &entry.settings));
            }
            if let Some((label, _)) = layers
                .iter()
                .find(|(_, settings)| settings.export_static_crate.is_some())
            {
                return Err(format!(
                    "`{label}` sets `export-static-crate` while `[runtime]` also does. One config \
                     writes one shared runtime: two exports would race for the same role, and \
                     letting either win silently would make which runtime survives depend on \
                     generation order. Keep the `[runtime]` one and delete `{label}.\
                     export-static-crate`, or drop `[runtime].export-static-crate` and place the \
                     key by hand."
                ));
            }
            return Ok(());
        }

        let sites: Vec<String> = self
            .crates
            .iter()
            .filter_map(|(name, entry)| {
                self.export_layer(name, entry)
                    .map(|label| format!("`{name}` (from `{label}`)"))
            })
            .collect();
        if sites.len() > 1 {
            return Err(format!(
                "`export-static-crate` reaches {} crates: {}. One config writes one shared runtime, \
                 and two crates exporting into one directory is not two writes of the same bytes: \
                 whichever runs second overwrites the first at ITS flavor, so the run stops being \
                 idempotent — the first flavor's files sit outside the stale-file scan and linger, \
                 the exported manifest accumulates both flavors' dependencies, and the \
                 comment-preservation overlay cannot classify the previous flavor's output, so it \
                 injects a fresh `compile_error!` block on every run and the exported files grow \
                 without bound. Keep the key on the ONE crate whose flavor the runtime should have, \
                 or lift it to `[runtime].export-static-crate` and let the config derive the carrier.",
                sites.len(),
                sites.join(", "),
            ));
        }
        Ok(())
    }

    /// Which layer supplies a crate's `export-static-crate` — the line a user would delete. Follows
    /// merge precedence, so the answer is the layer that actually WINS: the crate's own table, else
    /// the last of its listed profiles to set it, else `[defaults]`.
    fn export_layer(&self, name: &str, entry: &CrateEntry) -> Option<String> {
        if entry.settings.export_static_crate.is_some() {
            return Some(format!("[crates.{name}]"));
        }
        for profile in entry.profiles.iter().rev() {
            // Validated by `validate()` to name a configured profile.
            if self.profiles[profile].export_static_crate.is_some() {
                return Some(format!("[profiles.{profile}]"));
            }
        }
        self.defaults
            .export_static_crate
            .is_some()
            .then(|| "[defaults]".to_owned())
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

    /// The settings a crate is GENERATED under: [`Self::merged_settings`] folded through the
    /// cross-crate derivations, in the order that produces them.
    ///
    /// One body rather than two, because the second reader is the committed-state VERDICT
    /// ([`Self::committed_verdict`]) and what it reads has to be what the run wrote. Its two inputs —
    /// the `wrapper-requests` sidecar path and the `extern-wrapper-index` path — are written by
    /// [`Self::apply_graph_edges`] and overridable by hand, so a verdict that folded its own copy of
    /// the pipeline would answer about paths no run ever used from the first moment the two copies
    /// disagreed, and would keep answering confidently. Sharing the body makes them the same paths by
    /// construction rather than by inspection.
    ///
    /// The returned [`Provenance`] records only what the derivation itself wrote; a caller that
    /// prints no flag listing can drop it.
    fn graphed_settings(
        &self,
        name: &str,
        entry: &CrateEntry,
        ungraphed: &BTreeMap<String, Cli>,
        runtime_choice: Option<&RuntimeChoice>,
    ) -> (Settings, Provenance) {
        let mut settings = self.merged_settings(entry);
        let derived = self.apply_graph_edges(name, entry, &mut settings, ungraphed);
        self.apply_runtime(name, &mut settings, runtime_choice);
        (settings, derived)
    }

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
    /// This reads generated output, so it is bounded exactly as `AGENTS.md`'s other diagnostic reads
    /// are: it runs after every file is written, it changes NO generated byte, and nothing it finds
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

    /// "You named a crate this config does not have", written once.
    ///
    /// Every selector answers it identically because it IS the same question — a typo on the command
    /// line does not become a different mistake depending on which selector carried it.
    fn unknown_crate(&self, name: &str) -> String {
        format!(
            "`{name}` is not a crate in this config. Configured crates: {}",
            list_or_none(self.crates.keys())
        )
    }

    /// `--with-deps`: the selection closed transitively over `deps`, in generation order.
    ///
    /// The plain selector trusts an unselected dependency's COMMITTED output, which is the right
    /// default and the same contract the cross-crate flags document. But it makes the one workflow
    /// that needs two commands need two commands: change a consumer's spec so it borrows a new
    /// wrapper, regenerate the consumer, and the committed-state verdict correctly reports that the
    /// dependency does not host it — the tree needs the dependency re-run, and the user already knew
    /// that when they typed the consumer's name. This closes the selection instead, so one command
    /// settles it.
    ///
    /// DEPENDENCIES only, never consumers. The two directions are not symmetric: a dependency is
    /// generated so that the named crate's own inputs exist, while a consumer would be generated
    /// because it might want to CHANGE — that is output the user did not ask for, and the verdict is
    /// how they hear about it rather than by having it silently rewritten.
    ///
    /// The closure decides WHICH crates run and never in what order: the result is filtered out of
    /// [`Self::generation_order`], the same total order a full run uses, so `--with-deps a b` and
    /// `--with-deps b a` generate the same thing in the same sequence.
    pub fn with_dependencies(&self, selected: &[String]) -> Result<Vec<String>, String> {
        // Naming nothing already means every crate in the config, which is a superset of any
        // closure — so this spelling has no meaning to give it, and a flag that silently did nothing
        // would read as one that had worked.
        if selected.is_empty() {
            return Err(
                "`--with-deps` closes a crate SELECTION over its dependencies, so it needs at least \
                 one crate name to close over. Naming no crate already runs every crate in the \
                 config, which no closure can add to: drop the flag, or name the crate whose \
                 dependencies you want pulled in."
                    .to_owned(),
            );
        }
        for name in selected {
            if !self.crates.contains_key(name) {
                return Err(self.unknown_crate(name));
            }
        }

        let mut closed: BTreeSet<String> = BTreeSet::new();
        let mut pending: Vec<String> = selected.to_vec();
        while let Some(name) = pending.pop() {
            // The `closed` guard is what terminates the walk, so it does not depend on `validate`'s
            // cycle rejection having run — and every `deps` entry names a real crate for the same
            // reason (`validate` rejects the rest), which is why the indexing below cannot panic.
            if !closed.insert(name.clone()) {
                continue;
            }
            pending.extend(self.crates[&name].deps.iter().cloned());
        }
        Ok(self
            .generation_order()?
            .into_iter()
            .filter(|name| closed.contains(name))
            .collect())
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
    /// [`Self::with_dependencies`] is the opt-in that closes the selection instead; it runs before
    /// this, so what arrives here is a plain list of names either way.
    pub fn expand(&self, selected: &[String]) -> Result<Vec<(String, Cli)>, String> {
        Ok(self
            .expand_each(selected)?
            .into_iter()
            .map(|(name, cli, _)| (name, cli))
            .collect())
    }

    /// [`Self::expand`], keeping each invocation's tagged [`Fragment`]s alongside the `Cli` they
    /// parsed into.
    ///
    /// One body for both callers rather than a second expansion path for the listing: a
    /// `--print-flags` that recomputed the fragments could print a flag list the run does not use,
    /// which is the one thing an inspection surface must never do.
    fn expand_each(
        &self,
        selected: &[String],
    ) -> Result<Vec<(String, Cli, Vec<Fragment>)>, String> {
        let ungraphed = self.ungraphed()?;
        // Over EVERY crate, never over the selection, for the same reason the runtime carrier below
        // is: whether a declaration can mean anything is a property of the config, so a subset run
        // must reject the configs a full run rejects.
        self.validate_wasm_reexports(&ungraphed)?;
        // Over EVERY crate for the same reason, and before the runtime carrier for a second one: a
        // posture the seam cannot survive is a mistake in the crates' own flags, and reporting it
        // first keeps the diagnosis at the edge that has the problem rather than at whatever the
        // flavor join happens to make of it.
        self.validate_component_seam_posture(&ungraphed)?;
        // Derived from EVERY crate, never from the selection: which crate can carry the shared
        // runtime is a property of the config, so `--config c.toml ledger` must reject the same
        // configs a full run rejects rather than pass because the offending crate sat this one out.
        let runtime_choice = self.runtime_carrier(&ungraphed)?;

        let order = self.generation_order()?;
        let chosen: Vec<String> = if selected.is_empty() {
            order
        } else {
            for name in selected {
                if !self.crates.contains_key(name) {
                    return Err(self.unknown_crate(name));
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
                let (settings, derived) =
                    self.graphed_settings(&name, entry, &ungraphed, runtime_choice.as_ref());
                let threads = self.threading(&name, entry, &settings, &ungraphed)?;
                let wasm_deps = self.wasm_deps(&name, entry, &settings, &ungraphed)?;
                let (rust_deps, std_forward_deps) =
                    self.rust_deps(&name, entry, &settings, &ungraphed)?;
                let component_deps = self.component_deps(&name, entry, &settings, &ungraphed)?;
                let fragments = argv_fragments(
                    entry,
                    &settings,
                    &self.base_dir,
                    &threads,
                    &wasm_deps,
                    &rust_deps,
                    &component_deps,
                    &std_forward_deps,
                    &derived,
                    self.static_dir_override.as_deref(),
                    self.verbosity_override,
                );
                let mut cli = build_cli(&name, entry, &self.base_dir, &fragments)?;
                // `[runtime]` carrier selection (including its explicitly accepted `flavor-from`
                // path) is config's closed decision. The hand-flag runtime-flavor record has no
                // config key and must not read a committed file to re-adjudicate that decision.
                // This marker is internal-only: it is neither an argv fragment nor printable.
                cli.config_runtime_decision_owned = true;
                // The generator's own cross-flag rules, run HERE rather than where the generator
                // reaches them. They are a pure function of the `Cli`, and every one of them is
                // reachable from a shared key — `[defaults].json-schema-scripts = true` with one
                // crate lacking `json-schema-export`, say. Left inside the generation loop, such a
                // key regenerates every earlier crate in full before failing, and fails with a bare
                // flag message naming neither the crate nor the TOML line: exactly the shape the
                // key-attribution replay above exists to prevent. Attributed to the crate rather
                // than to a key because a COMBINATION spans two of them, and the message already
                // names both flags.
                crate::api::validate_flag_combinations(&cli)
                    .map_err(|e| format!("[crates.{name}]: {e}"))?;
                validate_extern_import_stubs(&name, &cli, &derived)?;
                Ok((name, cli, fragments))
            })
            .collect()
    }

    /// The flag list each selected crate would be generated with, as text — the whole of
    /// `--print-flags`.
    ///
    /// The expansion behind it is the REAL one ([`Self::expand_each`]), so every validation a run
    /// performs has already run by the time a line is printed: a config that cannot generate cannot
    /// be listed either, and it fails with the identical message.
    ///
    /// # Why the format is not a command line
    ///
    /// The obvious rendering — a copy-pasteable `cddl-codegen --input … --output …` — would be the
    /// wrong thing to build. A pasted flag list is a snapshot: it is accurate on the day it is
    /// copied and silently stops being so at the next config edit, which is the exact duplication
    /// this feature exists to make visible rather than to mint more of. So the listing leads each
    /// line with the CONFIG KEY, which answers "why is this flag here?" as well as "what is here?",
    /// and is not a token sequence any shell would accept. Nothing is shell-quoted, for the same
    /// reason.
    pub fn flag_listing(&self, selected: &[String]) -> Result<String, String> {
        let expanded = self.expand_each(selected)?;
        // Padded to the widest key in THIS listing, so the columns line up without a hard-coded
        // width that a longer key would silently outgrow.
        let width = expanded
            .iter()
            .flat_map(|(_, _, fragments)| fragments.iter())
            .map(|(key, _)| key.len())
            .max()
            .unwrap_or(0);
        let mut out = String::from(PRINT_FLAGS_PREAMBLE);
        for (name, _, fragments) in &expanded {
            out.push_str(&format!("\n[crates.{name}]\n"));
            for (key, fragment) in fragments {
                out.push_str(&format!("  {key:width$}  {}\n", fragment.join(" ")));
            }
        }
        Ok(out)
    }

    /// Every JSON-schema thread this crate's document carries, in emission order.
    ///
    /// # Why the source is the WASM dependency list
    ///
    /// A document must thread the crates whose wasm classes ship in this crate's PACKAGE — not the
    /// crates its spec references. The two are different lists, and the package one is the one that
    /// decides what the published `.d.ts` has to declare: with one document per crate, everything
    /// this crate's own types *reference* is already present through the ref closure, so what a
    /// thread adds is a dependency's UNREFERENCED roots. Those are exactly the types a package
    /// re-exports without naming.
    ///
    /// So the source is `deps ∪ wasm-reexports`: `deps` covers the wasm dependencies that exist
    /// because the spec references them, `wasm-reexports` the ones that exist only because the
    /// package ships them. Both sides of both derived values come from the named crate's own entry,
    /// so a `lib-name` or `output` rename propagates and nothing can drift.
    ///
    /// Reading the generated `wasm/Cargo.toml` instead — where the fact already is — is not an
    /// option: that manifest is co-owned prior output, and deciding WHICH ROWS TO EMIT from prior
    /// output is the one thing the determinism contract does not bend on. Declaring the same fact
    /// in the config makes it an input.
    ///
    /// # What is silent and what is a hard error
    ///
    /// A DERIVED thread whose target has no schema document is a silent skip — that is precisely
    /// what filters hand-written crates out of the intersection, and it is what lets one config
    /// hold both kinds of crate. An EXPLICITLY listed one is a hard error: the user asked for a
    /// call into a crate that generates no json-gen crate for it to reach, and the failure without
    /// this check is a cargo path-resolution error in the consumer's json-gen build, naming a
    /// directory that was simply never written.
    ///
    /// The same split applies to the consumer side. A crate with no document of its own derives
    /// nothing (there is nowhere for the rows to land), while an explicit `json-schema-deps` on
    /// such a crate is the same impossible request and is refused the same way.
    fn threading(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<Vec<DerivedThread>, String> {
        // Read off the expanded `Cli` rather than off merged `Settings`, so clap's default for
        // `--json-schema-export` is never restated here — the rule every derivation in this file
        // follows.
        if !ungraphed[name].json_schema_export {
            if entry
                .json_schema_deps
                .as_ref()
                .is_some_and(|explicit| !explicit.is_empty())
            {
                return Err(format!(
                    "[crates.{name}].json-schema-deps threads other crates' rows into \
                     `{name}`'s schema document, but `{name}` has `json-schema-export = false` and \
                     generates no json-gen crate — there is no document for the rows to land in. \
                     Turn `json-schema-export` on for `{name}`, or drop the key (`json-schema-deps \
                     = []` if you meant to thread nothing)."
                ));
            }
            return Ok(Vec::new());
        }

        // The override REPLACES the derivation rather than adding to it: a crate whose package
        // composition and dependency list genuinely diverge needs to say the whole list, and a key
        // that could only ever add would leave no way to say "not that one".
        let sources: Vec<(&'static str, &String)> = match &entry.json_schema_deps {
            Some(explicit) => explicit
                .iter()
                .map(|dep| ("json-schema-deps", dep))
                .collect(),
            None => entry
                .deps
                .iter()
                .map(|dep| ("deps", dep))
                .chain(
                    entry
                        .wasm_reexports
                        .iter()
                        .map(|dep| ("wasm-reexports", dep)),
                )
                .collect(),
        };

        let consumer_dir = self.json_gen_dir(&ungraphed[name], &entry.output);
        let mut threads = Vec::with_capacity(sources.len());
        for (key, dep) in sources {
            // Validated to name a configured crate by `validate_crate_names`.
            let dep_entry = &self.crates[dep];
            if !ungraphed[dep.as_str()].json_schema_export {
                if key == "json-schema-deps" {
                    return Err(format!(
                        "[crates.{name}].json-schema-deps names `{dep}`, which has \
                         `json-schema-export = false`. That crate generates no json-gen crate, so \
                         there is no `add_schemas` to call and no package to depend on — the call \
                         could never link. Turn `json-schema-export` on for `{dep}`, or drop it \
                         from the list."
                    ));
                }
                continue;
            }
            let lib = normalized(&dep_entry.lib_name);
            // A hand-written sub-table entry for the same key wins, silently, and independently per
            // half: the same rule `apply_graph_edges` follows, for the same reason. An explicit
            // value is the user covering a case the sugar does not, not a conflict — and emitting
            // both would be the flag's own duplicate-label rejection instead.
            let json_schema_dep = (!settings.json_schema_dep.contains_key(&lib))
                .then(|| format!("{lib}={lib}_json_schema_gen"));
            // The cargo PACKAGE name, which is the `--lib-name` verbatim (dashes and all) plus the
            // suffix — the opposite spelling from the rust lib path above. Read off the same
            // `package.name` the json-gen manifest's change log writes.
            let package = format!("{}{JSON_GEN_PACKAGE_SUFFIX}", dep_entry.lib_name);
            let json_gen_dep = if settings.json_gen_dep.contains_key(&package) {
                None
            } else {
                let dep_dir = self.json_gen_dir(&ungraphed[dep.as_str()], &dep_entry.output);
                Some(format!(
                    "{package}={}",
                    manifest_relative_path(&consumer_dir, &dep_dir).map_err(|e| format!(
                        "[crates.{name}].{key} names `{dep}`, whose json-gen crate this crate must \
                         depend on by path: {e}"
                    ))?
                ))
            };
            threads.push(DerivedThread {
                key,
                json_schema_dep,
                json_gen_dep,
            });
        }
        Ok(threads)
    }

    /// A crate's json-gen crate directory, resolved against the config file's directory.
    ///
    /// `wasm/json-gen` under the crate's rust root — which `--package-json` moves one level down,
    /// exactly like the other generated crates, so the layout is read off the OTHER crate's own
    /// expanded `Cli` and never guessed. Note the directory exists in `wasm = false` runs too: the
    /// json-gen crate follows `--json-schema-export`, not the wasm face.
    fn json_gen_dir(&self, cli: &Cli, output: &str) -> PathBuf {
        self.crate_dir(cli, output, JSON_GEN_DIR)
    }

    /// One of a crate's generated cargo crates, resolved against the config file's directory. The
    /// `--package-json` nesting comes off that crate's OWN expanded `Cli`, never guessed.
    fn crate_dir(&self, cli: &Cli, output: &str, tail: &str) -> PathBuf {
        PathBuf::from(resolve_path(
            &self.base_dir,
            &crate_relative(cli, output, tail),
        ))
    }

    /// Every `[dependencies]` entry this crate's generated `wasm/Cargo.toml` needs in order to
    /// resolve the cross-crate names its own wasm pass emits, as `--wasm-dep` values.
    ///
    /// # Why both edge keys feed it, and why they contribute different entries
    ///
    /// `deps` means this crate's SPEC references the dependency's types, and the wasm pass writes
    /// two kinds of reference to such a type: `use <dep>_wasm::…` at the wasm boundary (routed by
    /// `--extern-wasm-crate`, and by `--extern-wrapper-index` for a borrowed wrapper) and the
    /// dependency's plain RUST type as the inner storage of any wrapper this crate mints itself.
    /// Those are two packages, so a `deps` edge contributes both — the dependency's rust package
    /// unconditionally, its wasm package when it generates one. (A dependency with `wasm = false`
    /// keeps its rust crate name for both passes, the single-crate convention `--extern-wasm-crate`
    /// documents, so the rust entry alone is the whole of that edge.)
    ///
    /// `wasm-reexports` says the opposite thing: this crate's spec references nothing, and the
    /// dependency's classes ship in this crate's PACKAGE. No generated line names the dependency at
    /// all — the entry exists so the npm build bundles those classes, which is precisely the
    /// hand-written dependency the key is named after. So it contributes the wasm package only, and
    /// its target is guaranteed to have one by [`Self::validate_wasm_reexports`].
    ///
    /// Nothing is derived for a crate with `wasm = false`: it generates no wasm crate and so no
    /// manifest for an entry to land in — which is what the flag itself refuses.
    ///
    /// A hand-written `[crates.<name>.wasm-dep]` entry for the same PACKAGE wins, silently, per
    /// package: the same rule [`Self::threading`] and [`Self::apply_graph_edges`] follow, for the
    /// same reason — an explicit value is the user covering a case the sugar does not.
    fn wasm_deps(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<Vec<DerivedManifestDep>, String> {
        let consumer_cli = &ungraphed[name];
        if !consumer_cli.wasm {
            return Ok(Vec::new());
        }
        let consumer_dir = self.crate_dir(consumer_cli, &entry.output, "wasm");

        let sources = entry.deps.iter().map(|dep| ("deps", dep)).chain(
            entry
                .wasm_reexports
                .iter()
                .map(|dep| ("wasm-reexports", dep)),
        );

        let mut out = Vec::new();
        for (key, dep) in sources {
            // Validated to name a configured crate by the `deps` checks / `validate_crate_names`.
            let dep_entry = &self.crates[dep];
            let dep_cli = &ungraphed[dep.as_str()];
            // The cargo PACKAGE names: the `--lib-name` verbatim (dashes and all), and that plus
            // `-wasm` — read off the same `package.name` the two change logs write, and the opposite
            // spelling from the underscored crate names the generated `use` lines carry.
            let mut wanted: Vec<(String, &'static str)> = Vec::new();
            if key == "deps" {
                wanted.push((dep_entry.lib_name.clone(), "rust"));
            }
            if dep_cli.wasm {
                wanted.push((
                    format!("{}{WASM_PACKAGE_SUFFIX}", dep_entry.lib_name),
                    "wasm",
                ));
            }
            for (package, tail) in wanted {
                if settings.wasm_dep.contains_key(&package) {
                    continue;
                }
                let dep_dir = self.crate_dir(dep_cli, &dep_entry.output, tail);
                let path = manifest_relative_path(&consumer_dir, &dep_dir).map_err(|e| {
                    format!(
                        "[crates.{name}].{key} names `{dep}`, whose {tail} crate this crate's wasm \
                         crate must depend on by path: {e}"
                    )
                })?;
                out.push(DerivedManifestDep {
                    key,
                    value: format!("{package}={path}"),
                });
            }
        }
        Ok(out)
    }

    /// Every `[dependencies]` entry this crate's generated `rust/Cargo.toml` needs in order to
    /// resolve the cross-crate names its own RUST pass emits, as `--rust-dep` values.
    ///
    /// # Why only `deps`, and why unconditionally
    ///
    /// `deps` derives `--extern-import`, and an imported type is emitted into this crate's rust
    /// source as `use <dep>::<Type>;`. That reference exists in every flavor — the rust crate is the
    /// one crate every run generates — so unlike [`Self::wasm_deps`] this derivation has no `wasm`
    /// gate on either end, and it contributes exactly one entry per edge: the dependency's RUST
    /// package, which is the only package the rust pass can name.
    ///
    /// `wasm-reexports` contributes NOTHING here, and that asymmetry is the key's meaning rather
    /// than an omission: it says a dependency's wasm classes ship in this crate's PACKAGE while this
    /// crate's spec references none of its types, so no rust line names the crate at all.
    ///
    /// A hand-written `[crates.<name>.rust-dep]` entry for the same PACKAGE wins, silently, per
    /// package — the rule every sub-table derivation in this file follows.
    ///
    /// # The std-forwarding half
    ///
    /// Each entry is paired with a `--std-forward-dep <package>`, so the crate takes the dependency
    /// with `default-features = false` and its own `std` feature carries `<package>/std`. Without
    /// that pair, `default-features = false` on THIS crate stops at this crate: the dependency is
    /// still built with its defaults, its `std` is still on, and the `#[cfg(not(feature = "std"))]`
    /// arms it wrote are unreachable from any downstream configuration.
    ///
    /// UNCONDITIONAL per `deps` edge, unlike the `--rust-dep` half a hand-written entry suppresses.
    /// The target is a crate this config generates, and every crate this tool generates declares a
    /// `std` feature — so the forward always resolves, including onto a dependency whose path the
    /// user chose to spell by hand.
    ///
    /// `[runtime].lib-name` adds one more of each, for the shared runtime crate: the same
    /// dependency, on the same reasoning, onto a crate `export-static-crate` writes rather than one
    /// `[crates.*]` declares.
    fn rust_deps(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<(Vec<DerivedManifestDep>, Vec<DerivedManifestDep>), String> {
        let consumer_dir = self.crate_dir(&ungraphed[name], &entry.output, "rust");
        let mut out = Vec::new();
        let mut forwarding = Vec::new();
        // A hand-written `std-forward-dep` array entry for the same package wins, on the same terms
        // the `rust-dep` sub-table's does: the derivation adds what is not already there.
        let mut forward = |package: &str, key: &'static str| {
            if !settings.std_forward_dep.iter().any(|v| v == package)
                && !forwarding
                    .iter()
                    .any(|d: &DerivedManifestDep| d.value == package)
            {
                forwarding.push(DerivedManifestDep {
                    key,
                    value: package.to_owned(),
                });
            }
        };
        for dep in &entry.deps {
            // Validated to name a configured crate by the `deps` checks.
            let dep_entry = &self.crates[dep];
            // The cargo PACKAGE name: the `--lib-name` verbatim (dashes and all), read off the same
            // `package.name` the rust manifest's change log writes — the opposite spelling from the
            // underscored crate name the generated `use` lines carry.
            let package = dep_entry.lib_name.clone();
            forward(&package, "deps");
            if settings.rust_dep.contains_key(&package) {
                continue;
            }
            let dep_dir = self.crate_dir(&ungraphed[dep.as_str()], &dep_entry.output, "rust");
            let path = manifest_relative_path(&consumer_dir, &dep_dir).map_err(|e| {
                format!(
                    "[crates.{name}].deps names `{dep}`, whose rust crate this crate's rust crate \
                     must depend on by path: {e}"
                )
            })?;
            out.push(DerivedManifestDep {
                key: "deps",
                value: format!("{package}={path}"),
            });
        }

        // The shared runtime crate. Only `[runtime].lib-name` can name it: `common-import` is a Rust
        // path prefix (`crate::common` is a legal value), so no cargo package name follows from one,
        // and reading `package.name` out of the co-owned manifest would be a new content-read class
        // for a rule one documented line states.
        if let Some(runtime) = &self.runtime
            && let (Some(lib_name), Some(export)) =
                (&runtime.lib_name, &runtime.export_static_crate)
        {
            forward(lib_name, "runtime");
            if !settings.rust_dep.contains_key(lib_name) {
                let runtime_dir = PathBuf::from(resolve_path(&self.base_dir, export));
                let path = manifest_relative_path(&consumer_dir, &runtime_dir).map_err(|e| {
                    format!(
                        "`[runtime].lib-name` makes every crate depend on the shared runtime at \
                         `{export}` by path, and `[crates.{name}]` cannot reach it: {e}"
                    )
                })?;
                out.push(DerivedManifestDep {
                    key: "runtime",
                    value: format!("{lib_name}={path}"),
                });
            }
        }
        Ok((out, forwarding))
    }

    /// Every `[dependencies]` entry this crate's generated `component/Cargo.toml` needs, as
    /// `--component-dep` values.
    ///
    /// # One package per seam edge, and it is the dependency's RUST crate
    ///
    /// The guest glue holds a dependency-typed value as the NATIVE `<dep>::Foo` and converts it to
    /// an imported WIT resource handle across the bytes seam, so the package the component crate has
    /// to reach is the dependency's rust crate — the same package [`Self::rust_deps`] derives, from
    /// a different manifest's directory. Its COMPONENT crate is deliberately absent: WIT imports are
    /// wired by the composer at the component level, never by cargo, so nothing in this crate's
    /// source names it. That is the whole asymmetry with [`Self::wasm_deps`], which derives two
    /// packages per edge because the wasm pass emits `use <dep>_wasm::…` as well.
    ///
    /// Gated on the SEAM ([`Self::component_seam_edge`]) rather than on this crate's `component`
    /// alone, on exactly the terms `wasm_deps` states for its own gate: without import mode the
    /// dependency's types are excluded from the projection, no glue line names the crate, and the
    /// entry would be a path dependency nothing resolves through.
    ///
    /// A hand-written `[crates.<name>.component-dep]` entry for the same PACKAGE wins, silently, per
    /// package — the rule every sub-table derivation in this file follows.
    fn component_deps(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<Vec<DerivedManifestDep>, String> {
        let consumer_cli = &ungraphed[name];
        if !consumer_cli.component {
            return Ok(Vec::new());
        }
        let consumer_dir = self.crate_dir(consumer_cli, &entry.output, COMPONENT_DIR);
        let mut out = Vec::new();
        for dep in &entry.deps {
            // Validated to name a configured crate by the `deps` checks.
            let dep_entry = &self.crates[dep];
            let dep_cli = &ungraphed[dep.as_str()];
            if !Self::component_seam_edge(consumer_cli, dep_cli, &normalized(&dep_entry.lib_name)) {
                continue;
            }
            // The cargo PACKAGE name: the `--lib-name` verbatim, exactly as the two sibling manifest
            // derivations read it off the rust manifest's change log.
            let package = dep_entry.lib_name.clone();
            if settings.component_dep.contains_key(&package) {
                continue;
            }
            let dep_dir = self.crate_dir(dep_cli, &dep_entry.output, "rust");
            let path = manifest_relative_path(&consumer_dir, &dep_dir).map_err(|e| {
                format!(
                    "[crates.{name}].deps names `{dep}`, whose rust crate this crate's component \
                     crate must depend on by path: {e}"
                )
            })?;
            out.push(DerivedManifestDep {
                key: "deps",
                value: format!("{package}={path}"),
            });
        }
        Ok(out)
    }

    /// Every crate's `Cli` as its own table alone describes it — before any cross-crate derivation.
    ///
    /// EVERY crate is expanded, selected or not, because the derivations read values (`output`,
    /// `lib-name`, `wasm`, `package-json`, and the runtime flavor axes) out of the OTHER crate's
    /// finished `Cli` rather than re-deriving clap's defaults — reading them back is what stops a
    /// default drifting between the two places it would otherwise be written.
    ///
    /// Deliberately NOT [`Self::graphed_settings`], and it is the one place that distinction is not a
    /// duplication: this is the INPUT the graph derivations read, so folding them in here would ask
    /// each crate's derived values to be known before they are derived. It answers "what does this
    /// table alone say?", which is a different question from "what is this crate generated with?".
    fn ungraphed(&self) -> Result<BTreeMap<String, Cli>, String> {
        let mut out: BTreeMap<String, Cli> = BTreeMap::new();
        for (name, entry) in &self.crates {
            let settings = self.merged_settings(entry);
            let fragments = argv_fragments(
                entry,
                &settings,
                &self.base_dir,
                &[],
                &[],
                &[],
                &[],
                &[],
                &Provenance::new(),
                self.static_dir_override.as_deref(),
                self.verbosity_override,
            );
            out.insert(
                name.clone(),
                build_cli(name, entry, &self.base_dir, &fragments)?,
            );
        }
        Ok(out)
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

    /// Fold this crate's `deps` edges — both directions — into its merged settings.
    ///
    /// Every value here is one the config already holds, which is the whole point: hand-maintaining
    /// `<name>=<path>` pairs on both sides of an edge means two files that must agree about a third
    /// crate's `output` and `lib-name`, and nothing checks that they do.
    ///
    /// A hand-written sub-table entry for the same key always wins, silently: an explicit value is
    /// the user overriding the sugar for a case it does not cover, not a conflict to report. The
    /// returned [`Provenance`] records only the entries this actually wrote, so a hand-written one is
    /// still attributed to the flag-named key the user typed.
    fn apply_graph_edges(
        &self,
        name: &str,
        entry: &CrateEntry,
        settings: &mut Settings,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Provenance {
        let mut derived = Provenance::new();
        // Write a sub-table entry only if the merge did not already hold one, recording the config
        // key that produced it. Spelled out rather than through `Entry::or_insert_with` because the
        // recording has to happen exactly when the insertion does.
        //
        // The provenance is a PARAMETER rather than the constant `"deps"` it once was: the two edge
        // directions below are both caused by a `deps` array but not by the SAME one, and a
        // hardcoded tag makes a future third derivation silently claim to come from `deps`.
        macro_rules! derive {
            ($table:ident, $flag:literal, $key:expr, $value:expr, $provenance:expr $(,)?) => {{
                let key: String = $key;
                if !settings.$table.contains_key(&key) {
                    settings.$table.insert(key.clone(), $value);
                    derived.insert(($flag, key), $provenance);
                }
            }};
        }
        // The forward edges' provenance: this crate's OWN `deps` array, which is the table the
        // listing is printed under, so the bare key is the whole answer.
        let own_deps = || "deps".to_owned();

        // FORWARD edges: what this crate needs in order to consume each dependency.
        for dep in &entry.deps {
            let dep_entry = &self.crates[dep];
            let dep_cli = &ungraphed[dep.as_str()];
            let key = normalized(&dep_entry.lib_name);

            // The dependency's committed extern-interface export, a sibling of `rust/`/`wasm/` under
            // its `output` (NOT under the `--package-json` nesting — the export is emitted in every
            // mode, including rust-only, so it does not live inside the npm package's crate root).
            derive!(
                extern_import,
                "extern-import",
                key.clone(),
                join(&dep_entry.output, &format!("{EXTERN_INTERFACE_DIR}/{key}")),
                own_deps(),
            );

            // The dependency's committed WIT package, which puts its types on this crate's component
            // face as IMPORTED resources instead of leaving them excluded from the projection.
            //
            // Under the `--package-json` NESTING, unlike the extern-interface export above: the WIT
            // tree is emitted inside the component CRATE, so its location depends on the dependency's
            // own `package-json` — which is what `crate_relative` reads off its expanded `Cli`. This
            // is the `--extern-wrapper-index` frame, not the `--extern-import` one.
            //
            // Derived whenever both ends carry the component face, because there is nothing to
            // decide: without it the dependency's types are dropped from this crate's WIT and every
            // signature naming one is recorded as unexported, so import mode is the only shape in
            // which the edge means anything at all on this face.
            if Self::component_seam_edge(&ungraphed[name], dep_cli, &key) {
                derive!(
                    component_extern_wit,
                    "component-extern-wit",
                    key.clone(),
                    crate_relative(dep_cli, &dep_entry.output, COMPONENT_WIT_DIR),
                    own_deps(),
                );
            }

            // The remaining three are all about the dependency's WASM face, so all three are emitted
            // exactly when it has one. `--workspace-dep` in particular is not optional here: it is a
            // hard error without an `--extern-wasm-crate` mapping, so a dependency generating no
            // wasm crate must get neither.
            if !dep_cli.wasm {
                continue;
            }
            derive!(
                extern_wasm_crate,
                "extern-wasm-crate",
                key.clone(),
                format!("{key}_wasm"),
                own_deps(),
            );
            derive!(
                extern_wrapper_index,
                "extern-wrapper-index",
                key.clone(),
                crate_relative(dep_cli, &dep_entry.output, WASM_COLLECTIONS_INDEX),
                own_deps(),
            );
            if !settings.workspace_dep.contains(&key) {
                settings.workspace_dep.push(key.clone());
                // Keyed by the VALUE rather than by a map key: `--workspace-dep` is an array, and
                // its items are the only thing distinguishing one occurrence from another.
                derived.insert(("workspace-dep", key), own_deps());
            }
        }

        // REVERSE edges: the sidecars each consumer of THIS crate emits, which this crate reads so
        // the wrappers and key derives its consumers borrow are hosted here rather than duplicated
        // per consumer. In consumer-name order; the label is the consumer's library name, which is
        // what the attribution comments the dep emits will carry.
        //
        // These come from a `deps` array too, but not from THIS crate's — a reverse edge exists
        // because a CONSUMER declared it, and a listing that said only `deps` would send a reader
        // looking for it in a table that does not have it. So the provenance names the consumer's
        // table as well: `deps (from [crates.<consumer>])` answers "which key produced this flag"
        // and "whose" in one line.
        if !ungraphed[name].wasm {
            // Without a wasm crate this crate is never a `--workspace-dep` of anyone, so no consumer
            // emits either sidecar and both derived paths would name files that are never written.
            return derived;
        }
        for (consumer_name, consumer) in &self.crates {
            if !consumer.deps.iter().any(|dep| dep.as_str() == name) {
                continue;
            }
            let consumer_cli = &ungraphed[consumer_name.as_str()];
            let label = normalized(&consumer.lib_name);
            let from_consumer = || format!("deps (from [crates.{consumer_name}])");
            // The rust-side sidecar rides on `--workspace-dep` alone, so a rust-only consumer still
            // emits it; the wasm-side one exists only when the consumer has a wasm crate to record.
            derive!(
                key_requests,
                "key-requests",
                label.clone(),
                crate_relative(consumer_cli, &consumer.output, RUST_BORROWED_KEY_TYPES),
                from_consumer(),
            );
            if consumer_cli.wasm {
                derive!(
                    wrapper_requests,
                    "wrapper-requests",
                    label,
                    crate_relative(consumer_cli, &consumer.output, WASM_BORROWED_COLLECTIONS),
                    from_consumer(),
                );
            }
        }
        derived
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

/// A library name in the form every cross-crate value uses: the rust crate name, which is the
/// `--lib-name` with dashes normalised to underscores (`Cli::lib_name_code`). It is simultaneously
/// the `extern-interface/<dir>` name a dependency exports under and the
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` scope a consumer imports it into — they coincide because
/// the scope's leading component IS the crate the generated `use` line names, so the two cannot be
/// chosen independently.
fn normalized(lib_name: &str) -> String {
    crate::cli::lib_name_code(lib_name)
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
    // LOCKSTEP: this is the emitter's `--package-json` nesting rule, restated for the crate reading
    // ANOTHER crate's output — `GenerationScope::export`'s `rust_dir`, which is where the one-level-
    // down decision is actually made, and `generation::no_std_check::dep_path`, which restates it
    // again for the emitted shim (which stays at the output root and absorbs the nesting into its
    // dep path). It is code rather than a string, so no constant in `generation::layout` can carry it
    // for all three sites. Change them together.
    if cli.package_json {
        join(output, &format!("rust/{tail}"))
    } else {
        join(output, tail)
    }
}

/// The path for one derived cargo path dependency: from the manifest's own directory to the
/// DEPENDENCY crate's, RELATIVE. Shared by `--json-gen-dep` (json-gen manifest) and `--wasm-dep`
/// (wasm manifest), which face the same question about different pairs of directories.
///
/// Relative is a determinism requirement, not a style choice. Most derived paths in this file are
/// read by the tool at generation time and never emitted; these are WRITTEN into a committed
/// `Cargo.toml`. An absolute value would bake this machine's checkout location into a file the
/// project commits, so the same config would produce different bytes in a different clone —
/// "same inputs -> same bytes" broken in the most visible way there is. Relative is also simply what
/// the value MEANS: cargo resolves a path dependency against the manifest holding it.
///
/// Both endpoints are already resolved against the config file's directory, so they normally share a
/// frame and the diff is a pure lexical answer. `pathdiff` is purely lexical too, which is why both
/// endpoints are NORMALIZED first ([`lexically_normalized`]): an `output` of `./gen/core` otherwise
/// diffs to a correct-but-mangled `../../../.././gen/core/wasm/json-gen`, which is the value that
/// lands in the committed manifest, and a `..` component past the common prefix makes the diff
/// unanswerable outright.
///
/// One input shape still has no lexical answer even normalized: one side absolute and the other
/// relative, which a config mixing absolute and relative `output` values produces — as does a
/// relative `output` whose leading `..` climbs out of the config directory, since the name of the
/// directory it climbs out of is not in either string. `pathdiff` reports both by handing back an
/// ABSOLUTE path (or `None`) rather than by failing, so the result is checked rather than the
/// inputs, and the fallback supplies the missing frame from the process CWD. That reconstructs
/// exactly the location the relative side already denoted, so the derived value is the same one an
/// absolute `--config` path would have produced — the join is normalized too, which is what makes
/// the `..` resolvable there.
fn manifest_relative_path(from_dir: &Path, to_dir: &Path) -> Result<String, String> {
    let from_dir = lexically_normalized(from_dir);
    let to_dir = lexically_normalized(to_dir);
    if let Some(relative) = pathdiff::diff_paths(&to_dir, &from_dir).filter(|p| p.is_relative()) {
        return Ok(relative.to_string_lossy().into_owned());
    }
    let cwd = std::env::current_dir().map_err(|e| {
        format!(
            "`{}` and `{}` do not share a frame — one is absolute and the other relative, or one \
             climbs above the config directory — so the relative path between them is only defined \
             against the current directory, which cannot be read: {e}",
            to_dir.display(),
            from_dir.display()
        )
    })?;
    let absolute = |path: &Path| {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            lexically_normalized(&cwd.join(path))
        }
    };
    pathdiff::diff_paths(absolute(&to_dir), absolute(&from_dir))
        .filter(|p| p.is_relative())
        .map(|relative| relative.to_string_lossy().into_owned())
        .ok_or_else(|| {
            format!(
                "no relative path leads from `{}` to `{}`",
                from_dir.display(),
                to_dir.display()
            )
        })
}

/// Resolve `.` and `..` components WITHOUT touching the filesystem.
///
/// Purely lexical because the directories involved routinely do not exist yet — an `output` names
/// where a crate WILL be generated, so `Path::canonicalize` would fail on exactly the inputs this is
/// for. The lexical answer differs from the filesystem one only when a component that a `..` cancels
/// is a symlink, which a generated-output directory is not.
///
/// A leading `..` that nothing precedes is KEPT: there is no name in the string for it to cancel.
/// `/..` is dropped instead, since the root is its own parent. An empty result is `.` rather than the
/// empty string, so the value stays a path a join can be built on.
fn lexically_normalized(path: &Path) -> PathBuf {
    use std::path::Component;
    let mut out: Vec<Component> = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => match out.last() {
                Some(Component::Normal(_)) => {
                    out.pop();
                }
                Some(Component::RootDir | Component::Prefix(_)) => {}
                _ => out.push(component),
            },
            other => out.push(other),
        }
    }
    if out.is_empty() {
        return PathBuf::from(".");
    }
    out.iter().collect()
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
// Every parameter is a distinct INPUT to the expansion — the settings after merging, the derivations
// that are not settings, and the one value that comes from neither — so folding them into a struct
// would rename the list rather than shorten it, and hide from the signature which of them a caller
// legitimately has nothing to pass (`ungraphed` passes four empties).
#[allow(clippy::too_many_arguments)]
fn argv_fragments(
    entry: &CrateEntry,
    settings: &Settings,
    base_dir: &Path,
    threads: &[DerivedThread],
    wasm_deps: &[DerivedManifestDep],
    rust_deps: &[DerivedManifestDep],
    component_deps: &[DerivedManifestDep],
    std_forward_deps: &[DerivedManifestDep],
    derived: &Provenance,
    static_dir_override: Option<&str>,
    verbosity_override: Option<Verbosity>,
) -> Vec<Fragment> {
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
    } = settings;

    let mut out: Vec<Fragment> = Vec::new();
    // A macro rather than a closure so the negated switch below (which pushes a one-token fragment)
    // does not collide with a closure's exclusive borrow of `out`.
    //
    // ONE token, `--name=value`, rather than the two-token `["--name", value]`: clap takes everything
    // after the first `=` on a long option as the value VERBATIM, so a value whose first character is
    // `-` is representable. Split across two tokens it is not — no `Cli` argument sets
    // `allow_hyphen_values` (pinned by `no_cli_argument_accepts_hyphen_led_values`), so
    // `--lib-name -x` is read as the unknown flag `-x` and the config has no spelling at all for a
    // path or a name that starts with a dash. The value needs no escaping: the split is at the FIRST
    // `=`, which is what leaves the `<k>=<v>` sub-table values (`--extern-import=core=../p`) intact.
    macro_rules! flag {
        ($key:expr, $name:expr, $value:expr $(,)?) => {
            out.push(($key.to_string(), vec![format!("--{}={}", $name, $value)]))
        };
    }

    // Per-crate keys first, so the produced argv reads like the hand invocation it replaces.
    flag!("input", "input", resolve_path(base_dir, &entry.input));
    flag!("output", "output", resolve_path(base_dir, &entry.output));
    flag!("lib-name", "lib-name", entry.lib_name.clone());

    // Paths — resolved against the config file's directory, never the process CWD.
    //
    // `static-dir` excepted, and only when it came from the command line: that value is not a config
    // value, so it is emitted verbatim and tagged `command line` rather than with a key, which is
    // what makes `--print-flags` say WHY the committed key is not the value being used.
    match static_dir_override {
        Some(v) => flag!("command line", "static-dir", v),
        None => {
            if let Some(v) = static_dir {
                flag!("static-dir", "static-dir", resolve_path(base_dir, v));
            }
        }
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
        component => "component",
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
            "preserve-comments".to_owned(),
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
    // `wit-package` is a plain scalar: the value is a WIT package IDENTIFIER, not a path, so nothing
    // here resolves it against the config file's directory.
    if let Some(v) = wit_package {
        flag!("wit-package", "wit-package", v.clone());
    }
    // `verbosity`, on the same two-arm shape as `static-dir` above and for the same reason: a
    // command-line `--verbosity` overrides the committed key for every crate, and `--print-flags`
    // must be able to say WHY the key in the file is not the value in use.
    match verbosity_override {
        Some(v) => flag!("command line", "verbosity", v.as_str()),
        None => {
            if let Some(v) = verbosity {
                flag!("verbosity", "verbosity", v.as_str());
            }
        }
    }

    // Arrays: one flag occurrence per item, in array order — the order IS the input for
    // `--json-schema-root`, so nothing sorts here.
    for v in json_schema_root {
        flag!("json-schema-root", "json-schema-root", v.clone());
    }
    // The config key to report for one entry of a sub-table or array: the sugar's key when
    // `apply_graph_edges` wrote it, else the flag-named key, which is what a user writing it by hand
    // typed. Consulted per ENTRY, since one table routinely holds both kinds.
    let tag = |flag: &'static str, entry_key: &str| -> String {
        derived
            .get(&(flag, entry_key.to_owned()))
            .cloned()
            .unwrap_or_else(|| flag.to_owned())
    };
    for v in workspace_dep {
        flag!(tag("workspace-dep", v), "workspace-dep", v.clone());
    }

    // `<k>=<v>` sub-tables. The right-hand side is a PATH for the four that name a file the tool
    // reads, and a NAME for the two that land in generated rust — hence the split.
    macro_rules! path_table {
        ($($tbl:ident => $key:literal),* $(,)?) => {$(
            for (k, v) in $tbl {
                flag!(tag($key, k), $key, format!("{k}={}", resolve_path(base_dir, v)));
            }
        )*};
    }
    path_table!(
        extern_import => "extern-import",
        component_extern_wit => "component-extern-wit",
        extern_wrapper_index => "extern-wrapper-index",
        wrapper_requests => "wrapper-requests",
        key_requests => "key-requests",
    );
    // `extern-wasm-crate`'s right side is a CRATE name and `json-schema-dep`'s is a rust MODULE PATH
    // emitted verbatim into generated code; neither is a filesystem path, so neither resolves.
    for (k, v) in extern_wasm_crate {
        flag!(
            tag("extern-wasm-crate", k),
            "extern-wasm-crate",
            format!("{k}={v}")
        );
    }
    // Derived threads come BEFORE the raw sub-table entries, and this is the whole of the ordering
    // story. `--json-schema-dep` is order-significant — flag order is registration order, which
    // decides which crate a published-name collision blames — and a TOML sub-table is unordered, so
    // the raw entries below emit in NAME order: deterministic, but not the author's. The arrays that
    // produced these threads ARE ordered, so where order matters the ordered forms are the arrays.
    for thread in threads {
        if let Some(v) = &thread.json_schema_dep {
            flag!(thread.key, "json-schema-dep", v.clone());
        }
    }
    for (k, v) in json_schema_dep {
        flag!("json-schema-dep", "json-schema-dep", format!("{k}={v}"));
    }
    // `json-gen-dep`'s right side IS a path — and still does not resolve here, which is why it needs
    // stating rather than reading off the split above. It becomes a cargo PATH DEPENDENCY in
    // `<output>/wasm/json-gen/Cargo.toml`, and cargo resolves such a path against the manifest
    // holding it. Rewriting it against the config file's directory would retarget it somewhere cargo
    // never looks.
    // Derived before raw here too. Nothing observes the order of these — they become
    // `[dependencies]` keys, which `Cli::json_gen_deps` sorts anyway — but matching the sibling
    // above keeps one rule to remember rather than two.
    for thread in threads {
        if let Some(v) = &thread.json_gen_dep {
            flag!(thread.key, "json-gen-dep", v.clone());
        }
    }
    for (k, v) in json_gen_dep {
        flag!("json-gen-dep", "json-gen-dep", format!("{k}={v}"));
    }
    // `wasm-dep`'s right side is a path on exactly the terms `json-gen-dep`'s is, into
    // `<output>/wasm/Cargo.toml` instead — so it does not resolve here either. Derived before raw,
    // matching the sibling.
    for derived_dep in wasm_deps {
        flag!(derived_dep.key, "wasm-dep", derived_dep.value.clone());
    }
    for (k, v) in wasm_dep {
        flag!("wasm-dep", "wasm-dep", format!("{k}={v}"));
    }
    // `rust-dep`'s right side is a path on exactly the same terms, into `<output>/rust/Cargo.toml`.
    // Derived before raw, matching the two siblings.
    for derived_dep in rust_deps {
        flag!(derived_dep.key, "rust-dep", derived_dep.value.clone());
    }
    for (k, v) in rust_dep {
        flag!("rust-dep", "rust-dep", format!("{k}={v}"));
    }
    // `component-dep`'s right side is a path on exactly the same terms, into
    // `<output>/component/Cargo.toml`. Derived before raw, matching the three siblings.
    for derived_dep in component_deps {
        flag!(derived_dep.key, "component-dep", derived_dep.value.clone());
    }
    for (k, v) in component_dep {
        flag!("component-dep", "component-dep", format!("{k}={v}"));
    }
    // `--std-forward-dep` is the other half of a `rust-dep` entry, so it emits right after one, on
    // the same derived-before-raw rule. Its value is a bare package name — the path side is the
    // `rust-dep` entry's — which is why this is an array key rather than a fourth `<k>=<v>` table.
    for derived_dep in std_forward_deps {
        flag!(
            derived_dep.key,
            "std-forward-dep",
            derived_dep.value.clone()
        );
    }
    for v in std_forward_dep {
        flag!(tag("std-forward-dep", v), "std-forward-dep", v.clone());
    }

    out
}

/// Refuse a dependency this crate declares TWICE — once as an `--extern-import` (derived from `deps`,
/// or written by hand) and once as a physical stub directory in the crate's own input tree.
///
/// The generator refuses the same shape (`api::append_extern_imports`), and that check STAYS: it is
/// what a single-crate command line hits, and there the flag vocabulary is the user's own. But it
/// runs mid-generation, so in a config run every crate ordered before the consumer is already fully
/// written to disk when the consumer aborts — and it names `--extern-import <dep>=<path>`, a flag
/// nobody typed and nothing in the config can be grepped for. Same shape as the cross-flag rules
/// beside it: the config is the layer that can see the conflict before anything generates, so it
/// reports it there, against the key that produced the declaration.
///
/// It cannot join [`crate::api::validate_flag_combinations`], whose stated contract is that every
/// rule in it is a pure function of the `Cli` — the property that lets the config run those rules
/// ahead of the generator at all. This one stats a directory, so one `Cli` passes or fails depending
/// on what is on disk.
///
/// Run over the crates this invocation generates rather than over every crate in the config, unlike
/// the config-SHAPE validations ([`Config::validate_wasm_reexports`], [`Config::runtime_carrier`]).
/// Whether a stub directory sits in some crate's input tree is a fact about that tree, not about the
/// config, and a crate sitting this run out never reads it.
fn validate_extern_import_stubs(name: &str, cli: &Cli, derived: &Provenance) -> Result<(), String> {
    // A single-file input has no tree to carry a stub directory — the same gate the generator's
    // check applies, so the two agree about which shapes are even reachable.
    if !cli.input.is_dir() {
        return Ok(());
    }
    for dep in cli.extern_import_paths().into_keys() {
        let stub = cli.input.join(crate::parsing::EXTERN_DEPS_DIR).join(&dep);
        if !stub.is_dir() {
            continue;
        }
        // The sugar's key when `apply_graph_edges` derived the entry, else the flag-named key a
        // hand-written sub-table entry carries — the same attribution `argv_fragments` prints.
        let key = derived
            .get(&("extern-import", dep.clone()))
            .cloned()
            .unwrap_or_else(|| "extern-import".to_owned());
        let drop_the_edge = if key == "deps" {
            format!("drop `{dep}` from `deps`")
        } else {
            format!("delete the `{dep}` entry from the `extern-import` sub-table")
        };
        return Err(format!(
            "[crates.{name}].{key}: `{dep}` is declared twice — this crate consumes that \
             dependency's extern-interface export, and its own input tree hand-declares the same \
             dependency at {}. A dependency is declared exactly once, never merged: delete the stub \
             directory to consume the export, or {drop_the_edge} to keep hand-maintaining it. A stub \
             is the declaration for a dependency that has no export — a hand-written crate, or one \
             you cannot regenerate.",
            stub.display(),
        ));
    }
    Ok(())
}

/// Build one crate's `Cli` through clap, wrapping a rejection with the config key that caused it.
///
/// The fragments are passed IN rather than derived here, so the vector clap parses is the same one
/// `--print-flags` prints — a listing that could differ from the invocation would be worse than no
/// listing.
fn build_cli(
    name: &str,
    entry: &CrateEntry,
    base_dir: &Path,
    fragments: &[Fragment],
) -> Result<Cli, String> {
    use clap::Parser;

    let mut argv: Vec<String> = vec!["cddl-codegen".to_owned()];
    for (_, fragment) in fragments {
        argv.extend(fragment.iter().cloned());
    }
    match Cli::try_parse_from(&argv) {
        Ok(cli) => Ok(cli),
        Err(err) => {
            // Every probe below spells its flags the SINGLE-TOKEN way `argv_fragments` does, and that
            // is load-bearing rather than cosmetic: a probe built as two tokens would reject an
            // `input` of `-x.cddl` — a value the real invocation accepts — so a rejection caused by
            // some OTHER key would be blamed on `input`, which is the misattribution this whole
            // block exists to prevent, inverted.
            //
            // `--input` and `--output` FIRST, one at a time. The replay below probes each remaining
            // fragment on top of a base holding both of them, so when the BASE is what clap rejects
            // every probe fails and the first non-input/output fragment takes the blame. That is
            // always `lib-name`, a key the user may not even have written. Both are required, so
            // each is probed with a placeholder standing in for the other rather than alone. Single-
            // token emission plus the non-empty check on both keys leaves no VALUE clap rejects
            // here today; the pair stays probed because what makes that true is a property of
            // `Cli`'s two value parsers, and a parser added to either would restore the shape.
            let input = resolve_path(base_dir, &entry.input);
            let output = resolve_path(base_dir, &entry.output);
            const PLACEHOLDER: &str = "cddl-codegen-config-probe";
            for (key, argv) in [
                (
                    "input",
                    vec![
                        format!("--input={input}"),
                        format!("--output={PLACEHOLDER}"),
                    ],
                ),
                (
                    "output",
                    vec![
                        format!("--input={PLACEHOLDER}"),
                        format!("--output={output}"),
                    ],
                ),
            ] {
                let probe = std::iter::once("cddl-codegen".to_owned()).chain(argv);
                if let Err(single) = Cli::try_parse_from(probe) {
                    return Err(format!(
                        "[crates.{name}].{key}: {}",
                        single.to_string().trim_end()
                    ));
                }
            }

            // Attribute the rejection to a KEY by replaying the invocation one fragment at a time on
            // top of the required pair. Without this the user is shown a clap error about a flag they
            // never typed; with it they are pointed at the TOML line they did.
            let base: Vec<String> = vec![
                "cddl-codegen".to_owned(),
                format!("--input={input}"),
                format!("--output={output}"),
            ];
            for (key, fragment) in fragments {
                // Already in `base`; re-adding them is clap's "cannot be used multiple times", which
                // would misattribute every rejection to `input`.
                if matches!(key.as_str(), "input" | "output") {
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
/// means the crate that read it generated against a stale one and is now a run behind.
///
/// [`generate`] asks that question twice, for two different purposes. Around the first pass it is
/// the TRIGGER for [`generate`]'s convergence pass — the crates it names are exactly the ones re-run,
/// which is what settles a cold invocation without a second command. Around that pass it is the
/// residual DIAGNOSTIC: a sidecar still moving afterwards would be a feedback path the fixed pass
/// did not bound, and the warning is what says so. It changes no output byte in either role; what it
/// decides is which crates run again, never what any of them generates.
///
/// What it measures is the SIDECAR channel, in both roles and across every crate in the run. The
/// other cross-crate channel — a dependency's `collections.rs` wrapper index, which the convergence
/// pass legitimately rewrites, since hosting the requested wrappers is the whole point of the pass —
/// is deliberately NOT watched: an instrument that fired on every successful convergence would
/// measure nothing. That channel is bounded by the argument at [`generate`]'s convergence pass
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
    /// There is deliberately no restricted form. Both of [`generate`]'s two questions watch EVERY
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
    fn stale_entries(&self) -> impl Iterator<Item = (&String, &PathBuf)> {
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
/// is still deliberately un-prefixed by [`about_the_config`] — the verdict is about the TREE, not
/// about the document — and this wrapper must not change that.
#[derive(Debug)]
pub struct VerdictError(String);

impl std::fmt::Display for VerdictError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for VerdictError {}

/// Run everything a config file describes: expand it, generate each crate in order, then report
/// whether the run converged.
///
/// Expansion happens up front, so every value AND every flag combination is validated before ANY
/// crate generates — a typo in the last crate's table must not leave the first crate's output
/// half-migrated on disk.
pub fn generate(
    config_path: &Path,
    selected: &[String],
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<(), Box<dyn std::error::Error>> {
    let config = load_with(config_path, static_dir, verbosity)?;
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
    let _run_verbosity = crate::log::scoped(
        config
            .verbosity_override
            .or(config.defaults.verbosity)
            .unwrap_or_default(),
    );
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
///
/// It costs a second read of the config file (the graph cannot be known without one), which changes
/// no generated byte: this decides which crates run, exactly as the positional names do.
pub fn selection_with_deps(config_path: &Path, selected: &[String]) -> Result<Vec<String>, String> {
    load(config_path)?
        .with_dependencies(selected)
        .map_err(|e| about_the_config(config_path, e))
}

/// [`load`] plus the two command-line overrides, neither of which is a config value and so neither of
/// which can be parsed from the document.
fn load_with(
    config_path: &Path,
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<Config, String> {
    let mut config = load(config_path)?;
    config.static_dir_override = static_dir.map(|p| p.to_string_lossy().into_owned());
    config.verbosity_override = verbosity;
    Ok(config)
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
pub fn print_flags(
    config_path: &Path,
    selected: &[String],
    static_dir: Option<&Path>,
    verbosity: Option<Verbosity>,
) -> Result<(), Box<dyn std::error::Error>> {
    let config = load_with(config_path, static_dir, verbosity)?;
    // The run level, on the same `??` chain [`generate`] uses. The listing itself is the output of a
    // COMMAND rather than logging — like `--help`, it is never gated — but installing the level keeps
    // the two entry points saying the same thing about what this invocation's level is.
    let _run_verbosity = crate::log::scoped(
        config
            .verbosity_override
            .or(config.defaults.verbosity)
            .unwrap_or_default(),
    );
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
/// A SEPARATE clap struct rather than a `--config` field on [`Cli`], because `Cli` makes
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
    // Not a generation flag, so it does not collide with [`reject_generation_flags`]: it changes
    // what the run DOES rather than what any crate is generated with, which is the same class as
    // the positional crate selector.
    #[clap(long = "print-flags", action = clap::ArgAction::SetTrue)]
    pub print_flags: bool,

    /// Where the hand-written serialization runtime is read from (overrides any `static-dir` key).
    // The ONE generation flag [`reject_generation_flags`] lets through, and the exemption criterion
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
    // Not a generation flag, and so on the same side of [`reject_generation_flags`] as the crate
    // names it modifies: it chooses WHICH crates run, never what any of them is generated with.
    //
    // Dependencies only, never consumers — see [`Config::with_dependencies`] for why the two
    // directions are not symmetric.
    #[clap(long = "with-deps", action = clap::ArgAction::SetTrue)]
    pub with_deps: bool,

    /// How much the run prints (overrides any `verbosity` key).
    // The SECOND generation flag [`reject_generation_flags`] lets through — see [`EXEMPT_ARG_IDS`]
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
///
/// [`EXEMPT_ARG_IDS`] is the one exception, and it is the same class as `--print-flags`: a flag that
/// does not describe a crate.
pub fn reject_generation_flags(argv: &[String]) -> Result<(), String> {
    use clap::CommandFactory;

    let command = Cli::command();
    let mut longs: BTreeSet<String> = BTreeSet::new();
    let mut shorts: BTreeSet<char> = BTreeSet::new();
    for arg in command.get_arguments() {
        if EXEMPT_ARG_IDS.contains(&arg.get_id().as_str()) {
            continue;
        }
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
                 to. Config mode's own arguments — positional crate names, `--with-deps`, \
                 `--print-flags`, `--static-dir`, which names this machine's copy of the tool's \
                 runtime rather than anything about a crate, so it applies to all of them, and \
                 `--verbosity`, which is this run's override of a committed default and likewise \
                 applies to all of them — are the only command-line arguments it takes."
            ));
        }
    }
    Ok(())
}
