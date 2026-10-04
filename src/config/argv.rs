//! Private config argv implementation; public methods remain on `Config`.

use super::derive::{DerivedManifestDep, DerivedThread, Provenance, resolve_path};
use super::{Config, CrateEntry, Settings, list_or_none};
use crate::cli::Cli;
use crate::log::Verbosity;
use std::collections::BTreeSet;
use std::path::Path;

impl Config {
    /// Refuse a selection naming a crate the config does not configure.
    pub(super) fn check_selected(&self, selected: &[String]) -> Result<(), String> {
        for name in selected {
            if !self.crates.contains_key(name) {
                return Err(self.unknown_crate(name));
            }
        }
        Ok(())
    }

    /// "You named a crate this config does not have", written once.
    ///
    /// Every selector answers it identically because it IS the same question — a typo on the command
    /// line does not become a different mistake depending on which selector carried it.
    pub(super) fn unknown_crate(&self, name: &str) -> String {
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
        self.check_selected(selected)?;

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
    pub(super) fn expand_each(
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
            self.check_selected(selected)?;
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
    /// The expansion behind it is the REAL one (`Self::expand_each`), so every validation a run
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
}

/// The argv fragments a crate's settings expand to, each tagged with the config key that produced it
/// so a clap rejection can be reported against the TOML the user actually wrote.
///
/// Exhaustively destructures `settings` for the same reason [`Settings::merge_over`] does: a new
/// field that nothing emits here would parse, merge, and then vanish.
// Every parameter is a distinct INPUT to the expansion — the settings after merging, the derivations
// that are not settings, and the one value that comes from neither — so folding them into a struct
// would rename the list rather than shorten it, and hide from the signature which of them a caller
// legitimately has nothing to pass (`ungraphed` passes five empty slices and an empty `Provenance`).
#[allow(clippy::too_many_arguments)]
pub(super) fn argv_fragments(
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

    // Scalar keys, rendered through `Display`. `ArgAction::Set` booleans take an explicit
    // `true`/`false`, so an absent key is the only way to mean "leave clap's built-in default alone" —
    // which is exactly what `None` does here.
    macro_rules! scalar {
        ($($opt:ident => $key:literal),* $(,)?) => {$(
            if let Some(v) = $opt { flag!($key, $key, v.to_string()); }
        )*};
    }
    scalar!(
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

    scalar!(
        rust_wasm_feature => "rust-wasm-feature",
        deserialize_depth_limit => "deserialize-depth-limit",
        common_import_override => "common-import-override",
        wasm_cbor_json_api_macro => "wasm-cbor-json-api-macro",
        wasm_conversions_macro => "wasm-conversions-macro",
        wasm_list_macro => "wasm-list-macro",
        // `wit-package` is a plain scalar: the value is a WIT package IDENTIFIER, not a path, so
        // nothing here resolves it against the config file's directory.
        wit_package => "wit-package",
    );
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
    // `wasm-dep`, `rust-dep` and `component-dep`'s right sides are paths on exactly the terms
    // `json-gen-dep`'s is, into `<output>/wasm/Cargo.toml`, `<output>/rust/Cargo.toml` and
    // `<output>/component/Cargo.toml` — so they do not resolve here either. Derived before raw,
    // matching the siblings.
    for (derived_deps, raw_deps, key) in [
        (wasm_deps, wasm_dep, "wasm-dep"),
        (rust_deps, rust_dep, "rust-dep"),
        (component_deps, component_dep, "component-dep"),
    ] {
        for derived_dep in derived_deps {
            flag!(derived_dep.key, key, derived_dep.value.clone());
        }
        for (k, v) in raw_deps {
            flag!(key, key, format!("{k}={v}"));
        }
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
pub(super) fn validate_extern_import_stubs(
    name: &str,
    cli: &Cli,
    derived: &Provenance,
) -> Result<(), String> {
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
pub(super) fn build_cli(
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
pub(super) type Fragment = (String, Vec<String>);

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
/// `EXEMPT_ARG_IDS` is the one exception, and it is the same class as `--print-flags`: a flag that
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
