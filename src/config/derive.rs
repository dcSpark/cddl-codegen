//! Private config derive implementation; public methods remain on `Config`.

use super::argv::{argv_fragments, build_cli};
use super::{Config, CrateEntry, RuntimeChoice, Settings};
use crate::cli::Cli;
use crate::generation::layout::{
    COMPONENT_DIR, COMPONENT_WIT_DIR, EXTERN_INTERFACE_DIR, JSON_GEN_DIR, JSON_GEN_PACKAGE_SUFFIX,
    RUST_BORROWED_KEY_TYPES, WASM_BORROWED_COLLECTIONS, WASM_COLLECTIONS_INDEX,
    WASM_PACKAGE_SUFFIX,
};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

impl Config {
    /// The merged [`Settings`] a crate generates under: built-in (clap, by omission) → `[defaults]`
    /// → each named profile IN LISTED ORDER → the crate's own keys.
    pub(super) fn merged_settings(&self, entry: &CrateEntry) -> Settings {
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
    pub(super) fn graphed_settings(
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
    pub(super) fn threading(
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
    pub(super) fn json_gen_dir(&self, cli: &Cli, output: &str) -> PathBuf {
        self.crate_dir(cli, output, JSON_GEN_DIR)
    }

    /// One of a crate's generated cargo crates, resolved against the config file's directory. The
    /// `--package-json` nesting comes off that crate's OWN expanded `Cli`, never guessed.
    pub(super) fn crate_dir(&self, cli: &Cli, output: &str, tail: &str) -> PathBuf {
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
    pub(super) fn wasm_deps(
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
    pub(super) fn rust_deps(
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
    pub(super) fn component_deps(
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
    pub(super) fn ungraphed(&self) -> Result<BTreeMap<String, Cli>, String> {
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
    pub(super) fn apply_graph_edges(
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
pub(super) type Provenance = BTreeMap<(&'static str, String), String>;

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
pub(super) struct DerivedThread {
    /// The config key that produced this thread, for attributing a clap rejection to a TOML line.
    pub(super) key: &'static str,
    /// `--json-schema-dep` value: `<dep lib normalized>=<dep lib normalized>_json_schema_gen`.
    pub(super) json_schema_dep: Option<String>,
    /// `--json-gen-dep` value: `<dep lib-name>-json-schema-gen=<relative path>`.
    pub(super) json_gen_dep: Option<String>,
}

/// One derived manifest `[dependencies]` line, as the flag value it expands to: a `--wasm-dep` for
/// the consumer's generated `wasm/Cargo.toml` ([`Config::wasm_deps`]), or a `--rust-dep` for its
/// `rust/Cargo.toml` ([`Config::rust_deps`]).
///
/// One struct for both, because the two carry the identical pair — a config key to attribute a clap
/// rejection to, and a `<package>=<path>` value — and which flag a value belongs to is decided by
/// the list it is emitted from rather than by anything inside it.
pub(super) struct DerivedManifestDep {
    /// The config key that produced it (`deps` or `wasm-reexports`), for attributing a clap
    /// rejection to a TOML line.
    pub(super) key: &'static str,
    /// `<cargo package name>=<relative path>`.
    pub(super) value: String,
}

/// A library name in the form every cross-crate value uses: the rust crate name, which is the
/// `--lib-name` with dashes normalised to underscores (`Cli::lib_name_code`). It is simultaneously
/// the `extern-interface/<dir>` name a dependency exports under and the
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` scope a consumer imports it into — they coincide because
/// the scope's leading component IS the crate the generated `use` line names, so the two cannot be
/// chosen independently.
pub(super) fn normalized(lib_name: &str) -> String {
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
pub(super) fn lexically_normalized(path: &Path) -> PathBuf {
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

/// Resolve a path-valued config value against the config file's directory.
///
/// An ABSOLUTE path passes through untouched; a relative one is joined. Deliberately no
/// canonicalization: `output` and `export-static-crate` routinely name directories that do not exist
/// yet, and canonicalizing would fail on exactly those, so the resolved value is a lexical join.
pub(super) fn resolve_path(base_dir: &Path, value: &str) -> String {
    let path = Path::new(value);
    if path.is_absolute() {
        value.to_owned()
    } else {
        base_dir.join(path).to_string_lossy().into_owned()
    }
}
