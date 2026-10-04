//! Private config validate implementation; public methods remain on `Config`.

use super::derive::{lexically_normalized, normalized, resolve_path};
use super::{Config, CrateEntry, RuntimeFlavor, Settings, list_or_none};
use crate::cli::Cli;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

impl Config {
    /// Cross-table checks serde cannot express: profile references resolve, a profile is flat, and
    /// the `deps` graph is one a generation order exists for.
    ///
    /// All of it runs at PARSE time, before any crate generates — a graph mistake in the last crate's
    /// table must not be discovered after the first crate's output is already rewritten.
    pub(super) fn validate(&self) -> Result<(), String> {
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
    pub(super) fn validate_crate_names(
        &self,
        owner: &str,
        key: &str,
        names: &[String],
    ) -> Result<(), String> {
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
    /// [`super::convergence::Convergence`] watches request SIDECARS and this crate neither reads nor writes one, the
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
    /// (the split [`super::argv::argv_fragments`] already draws, since neither is path-resolved): nothing of ours
    /// moves underneath either, no index or sidecar is read, so no run of this config can make run 2
    /// differ from run 1 through them. They also have a legitimate same-config population — a crate
    /// whose spec carries a HAND-WRITTEN `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` stub for a type
    /// another crate in this config happens to generate, which needs the wasm face named without an
    /// extern-interface import; there `deps` is not merely unnecessary but refused, since the
    /// `--extern-import` it derives collides with the stub
    /// ([`super::argv::validate_extern_import_stubs`]). The duplicate-wrapper exposure that population does carry
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
    pub(super) fn validate_same_config_edges_are_deps(
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
    pub(super) fn validate_wasm_reexports(
        &self,
        ungraphed: &BTreeMap<String, Cli>,
    ) -> Result<(), String> {
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
    pub(super) fn validate_component_seam_posture(
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
    pub(super) fn component_seam_edge(consumer: &Cli, dep: &Cli, key: &str) -> bool {
        consumer.component
            && (dep.component || consumer.component_extern_wit_paths().contains_key(key))
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
    pub(super) fn validate_one_export_site(&self) -> Result<(), String> {
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
    pub(super) fn export_layer(&self, name: &str, entry: &CrateEntry) -> Option<String> {
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
    /// convergence check ([`super::convergence::Convergence`]) is what makes that visible rather than silent.
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
    pub(super) fn cycle_error(&self, remaining: &BTreeSet<&String>) -> String {
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
}
