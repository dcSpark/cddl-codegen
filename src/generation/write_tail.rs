//! The write tail of [`GenerationScope::export`](super::export::GenerationScope::export): every
//! byte this tool puts on disk after the content is decided, and — the reason it is one module —
//! **every read of prior output the tool performs**.
//!
//! The split is at the content/write seam. Everything that needs the IR
//! (`IntermediateTypes`) or the generator (`GenerationScope`) computes a value and hands it over;
//! nothing here can reach either, so the tail is drivable from a test with a synthetic file map, a
//! temp dir and no CDDL at all (`src/tests/write_tail_tests.rs`). That is what makes the
//! no-prior-output contract directly testable rather than only observable through a full
//! spec-bearing run.
//!
//! The prior-output reads live here and nowhere else — the manifest merges, the
//! comment-preservation overlay, the seed-once existence checks and the two diagnostics that peek
//! at a seed-skipped root, the `--export-static-crate` existence checks and manifest read, the
//! stale-file scan and the surrounding-workspace collision scan. Each is bounded by the contract
//! AGENTS.md states (and the per-site comments below restate): what prior output contributes is
//! comment bytes, tagged regions, a recorded replace-span removal, `SeedOnce`/seed-once existence
//! answers, and stderr text — never a decision about WHAT code is generated.

use crate::cargo_manifest::{KeyPath, ManifestOp};
use crate::import_prune::PruneConfig;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::export::{
    is_preservable_generated_path, missing_reexports, new_static_file_notice,
    rustfmt_generated_string,
};

/// The `--export-static-crate` target's writes: the composed runtime files and the standalone
/// serialization prelude (content composed by the caller, since composing it is a pure function of
/// the flag set), plus the changeset merged onto the target crate's own `Cargo.toml`. The target is
/// a HAND-OWNED crate outside the output tree, which is why it is described here as a whole rather
/// than folded into the fields above.
#[derive(Default)]
pub(crate) struct StaticCrateWrite {
    /// The crate directory `--export-static-crate` names (files land in its `src/`).
    pub dir: PathBuf,
    /// `(bare filename, final content)` for each composed runtime file.
    pub runtime_files: Vec<(String, String)>,
    /// Final content of the standalone `serialization.rs` (the static prelude only).
    pub serialization: String,
    /// The static-runtime manifest changeset.
    pub manifest_ops: Vec<(KeyPath, ManifestOp)>,
}

/// Everything the write tail needs, decided by the caller. Deliberately plain data: no
/// `IntermediateTypes`, no `GenerationScope`, and no `&Cli` — a field here is a value some caller
/// computed, so a test constructs one directly (`..Default::default()` for the parts a case does
/// not exercise).
///
/// This is NOT the `Ctx { types, cli }` param-pair struct AGENTS.md rules against: that ruling is
/// about borrow-splitting the generator pair behind one struct. The point of this one is the
/// opposite — it is what keeps the pair OUT.
#[derive(Default)]
pub(crate) struct WriteTailPlan {
    /// The final in-memory generated file map: rel path under `rust_dir` -> content.
    pub files: BTreeMap<String, String>,
    /// Where the cargo crates live: `<output>/rust`-nesting is already resolved by the caller.
    pub rust_dir: PathBuf,
    /// The output root — parent of the two always-clobbered sibling trees below.
    pub output_dir: PathBuf,
    /// One changeset per generated manifest, keyed by its path relative to `rust_dir`.
    pub manifest_ops: Vec<(&'static str, Vec<(KeyPath, ManifestOp)>)>,
    /// The usage-derived import prune's per-run config, for the post-overlay re-prune.
    pub prune_config: PruneConfig,
    /// `--no-preserve-comments` turns this off (default on).
    pub preserve_comments: bool,
    /// `--component`: gates the emitted WIT package's delete-and-recreate clear.
    pub component: bool,
    /// `(rel path under `rust_dir`, final content)` for the composed runtime statics.
    pub composed_runtime_files: Vec<(String, String)>,
    /// The extern names this run's rust glue needs the crate-root `lib.rs` to re-export.
    pub required_rust_reexports: BTreeSet<String>,
    /// The same for the wasm crate root.
    pub required_wasm_reexports: BTreeSet<String>,
    /// `--export-static-crate`'s writes, when the flag is set.
    pub static_crate: Option<StaticCrateWrite>,
    /// The dep-side extern-interface export, keyed relative to `output_dir`.
    pub extern_interface_files: BTreeMap<String, String>,
    /// The `no-std-check/` shim crate, keyed relative to `output_dir`.
    pub no_std_check_files: BTreeMap<String, String>,
}

impl WriteTailPlan {
    /// Run the tail. Ordering is load-bearing and unchanged from when this lived inline in
    /// `export`: manifests merge before the overlay (the overlay touches `.rs` only), the write
    /// loop precedes the composed statics, the deferred re-export diagnostic resolves only once
    /// every generated `.rs` of this run is on disk, and the two scans run last, after every write.
    pub(crate) fn run(self) -> std::io::Result<()> {
        let WriteTailPlan {
            mut files,
            rust_dir,
            output_dir,
            manifest_ops,
            prune_config,
            preserve_comments,
            component,
            composed_runtime_files,
            required_rust_reexports,
            required_wasm_reexports,
            static_crate,
            extern_interface_files,
            no_std_check_files,
        } = self;

        // Manifests merge into whatever is already on disk (the declarative changeset) rather than
        // clobbering, so user edits to keys the tool doesn't own survive regeneration. This is one of
        // the bounded exceptions where output depends on prior directory contents (the others: the
        // seed-once crate roots below, and the comment-preservation overlay in the write loop), and
        // only as the changeset contract allows: keys no op mentions pass through, `SeedOnce` checks
        // existence. An unparseable
        // existing manifest is a hard error naming the file (see `cargo_manifest::apply`) — never a
        // silent clobber. `generated_files` above produced these same manifests against an empty
        // document; here we re-derive them against the on-disk file before the common write loop.
        for (rel_path, ops) in &manifest_ops {
            if files.contains_key(*rel_path) {
                let existing = std::fs::read_to_string(rust_dir.join(rel_path)).ok();
                let merged = crate::cargo_manifest::apply(ops, existing.as_deref(), rel_path)
                    .map_err(std::io::Error::other)?;
                files.insert((*rel_path).to_owned(), merged);
            }
        }
        // The package names this run ships, read back off the manifests it just decided rather than
        // re-derived from `--lib-name` — the changeset is what actually names the packages, so
        // reading it is the only way the collision scan below cannot drift from them. Collected
        // HERE (the manifests are final; the overlay below touches `.rs` only) and used at the very
        // end, after every write.
        let generated_packages: Vec<(String, String)> = manifest_ops
            .iter()
            .filter_map(|(rel_path, _)| {
                let name = manifest_package_name(files.get(*rel_path)?)?;
                Some(((*rel_path).to_owned(), name))
            })
            .collect();

        // Comment/code-preservation overlay over the in-memory file map, then a post-overlay import
        // re-prune. This is the third bounded exception to the no-prior-output invariant. For each
        // generated `.rs` in the map that ALREADY EXISTS on disk (an existence check only, like the
        // manifest `SeedOnce`), carry the user's prior-run own-line comments and
        // `cddl-codegen:insert`/`:replace` code blocks onto the fresh content
        // (`comment_preserve::preserve`); an unplaceable one becomes a loud `compile_error!` block,
        // never a silent drop. Prior output therefore contributes ONLY comment bytes, the tagged
        // `compile_error!` regions, and the token span a `:replace` block records as removed — never a
        // code token OUTSIDE those tagged blocks.
        //
        // Why the re-prune: the usage-derived import prune's premise is "an import is justified iff
        // the FINAL generated code references the name". `generated_files` already pruned the FRESH
        // content, but a `:replace` block can delete the last user of an import the fresh content still
        // referenced, so the justified set must be recomputed against the POST-overlay content. It runs
        // over the WHOLE map, not per file, because a replace block in a DESCENDANT (e.g.
        // `serialization.rs`) can orphan an import in the parent `mod.rs`, which only a family-wide
        // re-prune sees. The prune is a pure function of the post-overlay content, so "same inputs →
        // same bytes" and run-twice = run-once both still hold: a later run regenerates the same pruned
        // imports in fresh content, re-applies the same replace blocks, and re-removes the same imports.
        //
        // Error semantics are byte-identical to `write_rs_with_preserve`'s (the messages are
        // test-pinned by `comment_preservation_broken_existing_file_hard_errors`): an existing file
        // that cannot be read (not UTF-8) is a hard error naming it; a `PreserveError` renders with the
        // file name.
        let mut overlay_changed_any = false;
        if preserve_comments {
            // Snapshot the preservable keys up front so the map can be mutated in the loop body.
            let preservable: Vec<String> = files
                .keys()
                .filter(|rel_path| is_preservable_generated_path(rel_path))
                .cloned()
                .collect();
            for rel_path in preservable {
                let path = rust_dir.join(&rel_path);
                if !path.exists() {
                    continue;
                }
                let existing = std::fs::read_to_string(&path).map_err(|e| {
                    std::io::Error::other(format!(
                        "{rel_path}: cannot read the existing generated file for comment \
                         preservation: {e}. Fix or delete the file, or pass --no-preserve-comments."
                    ))
                })?;
                let preserved = crate::comment_preserve::preserve(&existing, &files[&rel_path])
                    .map_err(|e| std::io::Error::other(e.render(&rel_path)))?;
                if preserved.changed {
                    files.insert(
                        rel_path,
                        rustfmt_generated_string(&preserved.content)?.into_owned(),
                    );
                    overlay_changed_any = true;
                }
            }
        }
        // Only re-prune when the overlay actually rewrote something: otherwise the fresh map is still
        // the `generated_files` prune fixed point and a second pass would change nothing.
        if overlay_changed_any {
            for (path, pruned) in crate::import_prune::prune_generated_files(&files, &prune_config)
            {
                files.insert(path, rustfmt_generated_string(&pruned)?.into_owned());
            }
            // Recompute the alloc imports too: a `cddl-codegen:replace` block can remove the last
            // user of a name (orphaning an import — an unused-import warning) or introduce a new one
            // (a missing import — an error). The injector's lines are exact known strings, so this
            // recompute both ADDS and REMOVES soundly, which is what lets it cover the trait
            // imports the pruner must never touch.
            for path in crate::alloc_import_inject::inject_generated_files(&mut files) {
                let formatted = rustfmt_generated_string(&files[&path])?.into_owned();
                files.insert(path, formatted);
            }
        }

        // The emitted WIT package is DELETE-AND-RECREATED, on exactly the terms the
        // `extern-interface/` export below states: a `.wit` a prior run wrote and this one no longer
        // does would keep resolving as part of the package (WIT resolves a whole DIRECTORY), so an
        // orphan here is not a dead file but a live declaration nothing generated. Delete-and-recreate
        // cannot orphan by construction, which is why this tree is out of the stale-file scan rather
        // than in it — and the scan's collector is `.rs`-only regardless. The files themselves are
        // written by the common loop below, so this only has to clear the ground first.
        if component {
            let wit_dir = rust_dir.join(crate::generation::layout::COMPONENT_WIT_DIR);
            if wit_dir.exists() {
                std::fs::remove_dir_all(&wit_dir)?;
            }
        }

        // Every generated-tree `.rs` written this run, so the stale-file scan below can tell an
        // orphan (a file a prior run generated but this one no longer does — e.g. a removed/renamed
        // scope) from live output.
        let mut written_generated_rs: BTreeSet<std::path::PathBuf> = BTreeSet::new();
        // Layer-3 re-export diagnostic input, DEFERRED until every generated file is written this
        // run. The warning must not fire for a required name whose glue line the user deleted via a
        // `cddl-codegen:replace` block — and that survives-or-not decision reads the POST-OVERLAY
        // bytes the loop below is still producing, so the seed-once root text is only collected here
        // and the warning is resolved at loop end (see the resolution block after the write loop).
        // Each entry is (seed-once root rel_path, its already-existing text).
        let mut deferred_reexport_candidates: Vec<(String, String)> = Vec::new();
        for (rel_path, content) in &files {
            let path = rust_dir.join(rel_path);
            if is_preservable_generated_path(rel_path) {
                written_generated_rs.insert(path.clone());
            }
            // Seed-once thin roots: each generated crate's root `lib.rs` (rust, wasm, json-gen) is
            // written only if absent (existence check only — the same bounded exception the manifest
            // changeset carves out of the no-prior-output invariant). Everything else under
            // `generated/**` clobbers as always.
            if matches!(
                rel_path.as_str(),
                "rust/src/lib.rs"
                    | "wasm/src/lib.rs"
                    | "wasm/json-gen/src/lib.rs"
                    | "component/src/lib.rs"
            ) && path.exists()
            {
                // Two diagnostics fire here, both reading this already-existing (seed-skipped) root.
                // Like the manifest changeset, this is a bounded existence-adjacent peek: it emits
                // stderr guidance ONLY and writes ZERO bytes (the seed is skipped either way), so the
                // no-prior-output invariant and run-twice-equals-run-once both hold. Never fires on a
                // fresh seed — that path does not `exist`, so this whole branch is skipped and the
                // file is written below.
                if let Ok(existing) = std::fs::read_to_string(&path) {
                    // (1) A root that predates the thin-root split still carries generated type
                    // definitions interleaved with hand wiring; under seed-once the tool leaves it
                    // untouched, so the now-under-`generated/**` types it duplicates produce loud
                    // compile errors. Detect that shape (no `mod generated;`) and name the one-time
                    // migration on stderr.
                    if !existing.contains("mod generated") {
                        crate::warn!(
                            "warning: {rel_path} predates the thin-root layout (no `mod generated;`). \
                             Generated code now lives under `src/generated/**` and this root is \
                             seed-once (never overwritten), so any generated items still in it will \
                             collide with the regenerated subtree. One-time migration: delete the \
                             generated items from {rel_path}, keep your hand wiring, and add \
                             `mod generated;` and `pub use generated::*;`. See the \"Migrating from \
                             pre-split layouts\" section of docs/output_format."
                        );
                    }
                    // (2) Own-spec extern re-export contract: this run's glue needs the crate-root
                    // `lib.rs` to re-export a known set of names. When the seed-once root pre-dates
                    // the current required set (a contract change since it was seeded), the user gets
                    // a bare E0432 with no hint. Deferred to the post-write resolution block below so
                    // the survival scan can subtract names whose glue a `cddl-codegen:replace` block
                    // deleted from this run's output. Only the rust/wasm roots have a required set
                    // (json-gen has none), so only those are collected.
                    if matches!(rel_path.as_str(), "rust/src/lib.rs" | "wasm/src/lib.rs") {
                        deferred_reexport_candidates.push((rel_path.clone(), existing));
                    }
                }
                continue;
            }
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            // The comment/code-preservation overlay and the post-overlay import re-prune already ran
            // over the whole in-memory file map above, so every map entry is final — write it plainly.
            // The overlay is NOT applied per file here (unlike the composed runtime statics below):
            // the re-prune it feeds needs the whole map in view at once, because a `:replace` block in
            // one file can orphan an import in a sibling/parent file.
            std::fs::write(&path, content)?;
        }

        // static files copied/assembled verbatim (only when we own the common types), their content
        // composed by the caller (see `export`) and their final bytes written here. These composed
        // statics are NOT in
        // the generated-file `files` map, so they miss the map-level overlay + re-prune above and get
        // their comment preservation per file here via `write_rs_with_preserve`. That per-file
        // preservation is complete because they carry no PRUNABLE imports — nothing here can orphan
        // an import in a sibling, which is the only thing the map-level re-prune exists for.
        for (rel_path, content) in &composed_runtime_files {
            let path = rust_dir.join(rel_path);
            write_rs_with_preserve(&path, rel_path, content, preserve_comments)?;
            written_generated_rs.insert(path);
        }

        // Layer-3 re-export diagnostic, resolved now that every generated `.rs` for this run has
        // been written (all the glue-carrying scope `mod.rs` files included). A required extern name
        // is nagged as "missing from the seed-once lib.rs" only when its glue line
        // (`pub use crate::<Name>;`) actually SURVIVES in this run's written generated output: when
        // the user deleted that glue via a `cddl-codegen:replace` block, the run no longer requires
        // the re-export, so a warning would be a false nag on every regen. Survival is read from THIS
        // run's OWN just-written files — re-reading our own output is not a prior-output read: it
        // does not feed back into WHAT code is generated, so the no-prior-output invariant and
        // run-twice=run-once both still hold. Deciding from the written bytes (not from
        // `comment_preserve` internals) keeps this self-verifying and robust to future overlay
        // changes. Still emits stderr guidance ONLY and writes ZERO bytes.
        for (rel_path, existing) in &deferred_reexport_candidates {
            let (required, generated_prefix) = match rel_path.as_str() {
                "rust/src/lib.rs" => (&required_rust_reexports, "rust/src/generated"),
                "wasm/src/lib.rs" => (&required_wasm_reexports, "wasm/src/generated"),
                _ => continue,
            };
            // Post-overlay written bytes of this crate's generated `.rs` files (the glue lives in a
            // scope module `mod.rs`). Collect every own-line, non-comment `use` statement once; a
            // required name whose LIVE `pub use crate::<Name>;` glue no longer appears was deleted by
            // the user and is not required this run. The match is on the whole trimmed line, NOT a
            // substring: a `cddl-codegen:replace` deletion leaves the recorded original behind as a
            // `// pub use crate::<Name>;` comment line, which a substring scan would wrongly read as
            // survival — the trimmed-equality test excludes it (a comment line starts with `//`).
            // rustfmt keeps each single-name `pub use crate::X;` on its own line, so this is robust
            // to the files being rustfmt'd.
            let crate_generated_root = rust_dir.join(generated_prefix);
            let live_glue_lines: BTreeSet<String> = written_generated_rs
                .iter()
                .filter(|p| p.starts_with(&crate_generated_root))
                .filter_map(|p| std::fs::read_to_string(p).ok())
                .flat_map(|text| {
                    text.lines()
                        .map(|line| line.trim().to_owned())
                        .collect::<Vec<_>>()
                })
                .collect();
            let surviving: BTreeSet<String> = required
                .iter()
                .filter(|name| live_glue_lines.contains(&format!("pub use crate::{name};")))
                .cloned()
                .collect();
            let missing = missing_reexports(existing, &surviving);
            if !missing.is_empty() {
                crate::warn!(
                    "warning: {rel_path} is missing crate-root re-exports the generated \
                     extern glue requires: {}. Add to your hand-written root (one per \
                     name): `pub use <your_module>::<Name>;`. See the extern types \
                     section of docs/output_format.",
                    missing.join(", ")
                );
            }
        }

        // `--export-static-crate`: ADDITIONALLY write the composed rust runtime into the named
        // crate's `src/`, independent of the in-crate export above (the upgrade path for
        // --common-import-override users). No mod.rs/lib.rs is written — the target crate owns its
        // module declarations;
        // static files reference siblings via `super::…`, so a flat module dir works. This crate is
        // OUTSIDE the output crate, so its paths are deliberately not added to
        // `written_generated_rs` / the stale-file scan.
        if let Some(static_crate) = &static_crate {
            let export_dir = static_crate.dir.join("src");
            std::fs::create_dir_all(&export_dir)?;
            for (filename, content) in &static_crate.runtime_files {
                let path = export_dir.join(filename);
                let is_new = !path.exists();
                write_rs_with_preserve(&path, filename, content, preserve_comments)?;
                warn_new_static_file(is_new, filename);
            }
            let serialization_path = export_dir.join("serialization.rs");
            let serialization_is_new = !serialization_path.exists();
            write_rs_with_preserve(
                &serialization_path,
                "serialization.rs",
                &static_crate.serialization,
                preserve_comments,
            )?;
            warn_new_static_file(serialization_is_new, "serialization.rs");

            // The crate's Cargo.toml gets the static-runtime changeset merged in — the exported
            // source and the manifest that has to satisfy its dependencies are one artifact, so the
            // tool never writes one without the other (the pre-crate-shaped flag left the manifest
            // untouched, and a cbor_event bump in the exported source silently skewed against the
            // target crate's pin). Same declarative-merge contract as the generated crates' three
            // manifests: hand keys the changeset doesn't mention pass through, an unparseable
            // existing manifest is a hard error naming the file.
            let manifest_path = static_crate.dir.join("Cargo.toml");
            let existing = std::fs::read_to_string(&manifest_path).ok();
            let merged = crate::cargo_manifest::apply(
                &static_crate.manifest_ops,
                existing.as_deref(),
                &manifest_path.display().to_string(),
            )
            .map_err(std::io::Error::other)?;
            std::fs::write(&manifest_path, merged)?;
        }

        // Dep-side extern-interface export: a fresh projection over the finalized IR, emitted
        // UNCONDITIONALLY in every mode (rust-only and wasm alike — the analogy to `collections.rs`
        // is the commitment level, not its `if cli.wasm` gating). The tree is delete-and-recreated
        // so a removed rule's stale `.cddl` never lingers (no prior-output read feeds back into what
        // is generated). The projection is infallible: a rule it can't export faithfully is
        // excluded-with-record (a `; unexported:` comment) rather than aborting generation, so a
        // leaf/test spec still regenerates cleanly. Placed under `<output>/extern-interface/`, a
        // sibling of `rust/`/`wasm/`.
        let extern_interface_dir = output_dir.join(crate::generation::layout::EXTERN_INTERFACE_DIR);
        if extern_interface_dir.exists() {
            std::fs::remove_dir_all(&extern_interface_dir)?;
        }
        for (rel_path, content) in &extern_interface_files {
            let path = output_dir.join(rel_path);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(path, content)?;
        }

        // The no-std-check shim crate: the same always-clobbered sibling-tree shape as the
        // extern-interface export directly above, and emitted unconditionally for the same reason
        // (the seeded rust crate root tells every consumer to run it, so it has to be there). No
        // seed, no manifest changeset, no comment/code-preservation overlay, and no prior-output
        // read: both files are a pure function of `Cli`, so a rerun rewrites identical bytes.
        //
        // The stale-file scan below deliberately does NOT cover this tree, on the same argument that
        // exempts `extern-interface/`: the scan exists for the three PARTIALLY-rewritten
        // `src/generated` trees, where a removed rule leaves a `.rs` nothing declares any more.
        // Delete-and-recreate cannot orphan anything — a file this run did not write does not exist
        // after it — so a scan here could only ever report the empty set.
        let no_std_check_dir = output_dir.join(crate::generation::no_std_check::NO_STD_CHECK_DIR);
        if no_std_check_dir.exists() {
            std::fs::remove_dir_all(&no_std_check_dir)?;
        }
        for (rel_path, content) in &no_std_check_files {
            let path = output_dir.join(rel_path);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(path, content)?;
        }

        // Stale-file scan: a `.rs` under a tool-owned generated tree that this run did not produce
        // was generated by a PRIOR run (removed/renamed type or scope). Its `mod` declaration is
        // gone from the regenerated tree, so it (and any user comments in it) silently drops out of
        // the build — the one comment-loss path the per-file overlay cannot see. Diagnostic-only
        // stderr (same bounded read as the legacy-root warning): no output byte depends on it.
        for orphan in stale_orphans(&rust_dir, &written_generated_rs)? {
            crate::warn!(
                "warning: {} was generated by a previous run but is no longer generated; it is \
                 orphaned (nothing declares it as a module). Delete it — any comments you \
                 added there are NOT carried anywhere.",
                orphan.display()
            );
        }

        // Workspace package-name collision scan. Same diagnostic class as the two above and the
        // legacy-root check: it runs AFTER every write, reads only the SURROUNDING workspace (an
        // input, not this run's output), and its whole effect is `warn!` lines. No emitted byte can
        // depend on it — delete the call and every written file is identical.
        warn_on_workspace_package_name_collisions(&rust_dir, &generated_packages);

        Ok(())
    }
}

/// The preserve-or-clobber write for the composed runtime static files (`error.rs`,
/// `ordered_hash_map.rs`, `non_empty.rs`, `non_empty_map.rs`, `ordered_set.rs`, `pair_map.rs`) and
/// the `--export-static-crate` target's files. These are the overlay-covered `.rs` that are NOT in
/// the generated-file map (they are composed from `static/` and written directly), so they cannot
/// ride the map-level overlay + re-prune that the mapped `.rs` files go through in `export` — they
/// carry no prunable imports either, so per-file preservation here is complete for them. The mapped
/// generated `.rs` files instead get their overlay before the common write loop (see `export`), so
/// the "all generated trees uniformly" promise still holds. An existing file that cannot be read
/// (not UTF-8) or lexed is a hard error naming the file, never a silent clobber. Only content that
/// actually received an insertion pays the extra rustfmt pass.
fn write_rs_with_preserve(
    path: &std::path::Path,
    rel_path: &str,
    content: &str,
    preserve: bool,
) -> std::io::Result<()> {
    if preserve && path.exists() {
        let existing = std::fs::read_to_string(path).map_err(|e| {
            std::io::Error::other(format!(
                "{rel_path}: cannot read the existing generated file for comment preservation: \
                 {e}. Fix or delete the file, or pass --no-preserve-comments."
            ))
        })?;
        let preserved = crate::comment_preserve::preserve(&existing, content)
            .map_err(|e| std::io::Error::other(e.render(rel_path)))?;
        if preserved.changed {
            std::fs::write(path, rustfmt_generated_string(&preserved.content)?.as_ref())?;
        } else {
            std::fs::write(path, content)?;
        }
        return Ok(());
    }
    std::fs::write(path, content)
}

/// `--export-static-crate` writes into a crate whose root (`lib.rs`/`mod.rs`) is HAND-OWNED — the
/// tool never touches it (unlike the generated crates, whose roots it seeds and whose `generated/`
/// tree it clobbers). So a static runtime file that did NOT already exist — the incremental case a
/// version bump introduces (`ordered_set.rs` when set nominalization shipped) as well as every file
/// on a first export — needs a matching `pub mod <module>;` line the user must add BY HAND, or the
/// module sits dead in-tree. Generated code reaches these runtime modules TWO ways, and the notice
/// names both error codes because a reader greps for the one they actually saw: a type-bearing
/// module (`ordered_set`, `pair_map`, `non_empty*`) is imported — `use <crate>::<module>::Type;` —
/// so its absence is E0432; a function-bearing helper module (`open_struct_rest_json`, `any_cbor`)
/// is referenced by INLINE path — `<crate>::<module>::helper(…)` — so its absence is E0433 instead.
/// Either unresolved name cascades into a swarm of spurious E0119 "conflicting implementations"
/// errors in generated code (error-type unification against std's blanket
/// `impl<T, U> TryFrom<U> for T`),
/// pointing a reviewer at phantom problems before the one-line real cause. This is a diagnostic-only
/// prior-output read (an existence check; it changes no output byte) — the notice is printed AFTER
/// the write so the file state it reports is the pre-write one. See AGENTS.md's determinism
/// invariant (the enumerated diagnostic-only stderr reads) and the consumer-migration notes.
fn warn_new_static_file(is_new: bool, filename: &str) {
    if is_new {
        crate::warn!("{}", new_static_file_notice(filename));
    }
}

/// The tool-owned generated trees the stale-file scan covers, and the orphans it finds under them:
/// a `.rs` this run did not write, sorted within each tree and returned in tree order (the order
/// the warnings are emitted in). Split out from the warning loop so the "which files count as
/// orphaned" decision is directly assertable without capturing process stderr — the same reason
/// [`new_static_file_notice`] is a function rather than a literal at its warn site.
fn stale_orphans(
    rust_dir: &Path,
    written_generated_rs: &BTreeSet<PathBuf>,
) -> std::io::Result<Vec<PathBuf>> {
    let mut found = Vec::new();
    for tree in [
        "rust/src/generated",
        "wasm/src/generated",
        "wasm/json-gen/src/generated",
        // `component/wit` is deliberately ABSENT: it is delete-and-recreated (above), which
        // cannot orphan by construction, and this scan's collector is `.rs`-only anyway.
        "component/src/generated",
    ] {
        let mut orphans = Vec::new();
        collect_rs_files(&rust_dir.join(tree), &mut orphans)?;
        orphans.retain(|p| !written_generated_rs.contains(p));
        orphans.sort();
        found.extend(orphans);
    }
    Ok(found)
}

/// Recursively collect every `.rs` file under `dir` (absent dir = no files). Drives the stale-file
/// scan at the end of [`GenerationScope::export`].
fn collect_rs_files(
    dir: &std::path::Path,
    out: &mut Vec<std::path::PathBuf>,
) -> std::io::Result<()> {
    if !dir.is_dir() {
        return Ok(());
    }
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_rs_files(&path, out)?;
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
    Ok(())
}

/// `package.name` of a manifest's text, or `None` if it has none (or does not parse — this is a
/// diagnostic reader, and a manifest the tool itself just wrote always parses).
fn manifest_package_name(contents: &str) -> Option<String> {
    contents
        .parse::<toml_edit::DocumentMut>()
        .ok()?
        .get("package")?
        .get("name")?
        .as_str()
        .map(str::to_owned)
}

/// How many member manifests the workspace scan will read before it stops. The scan is an
/// approximation of cargo's own membership resolution, and a bound is what keeps an approximation
/// from becoming a cost: an umbrella whose globs cover a huge tree gets a partial answer (fewer
/// candidate names, so at worst a missed warning) rather than a slow generation.
const WORKSPACE_SCAN_MANIFEST_BUDGET: usize = 512;

/// Diagnostic-only workspace scan: a generated crate's `package.name` that an EXISTING member of the
/// surrounding cargo workspace already claims.
///
/// Why it is owed at all: cargo adopts in-workspace path dependencies as members, so a name a
/// hand-written crate already holds does not surface during generation at all — it surfaces at the
/// consumer's next `cargo metadata` as `error: two packages named X in this workspace`, after the
/// run reported success with an empty stderr. The tool is the only party that knows, at the moment
/// it decides the name, that the name is taken.
///
/// Why a WARNING and never a refusal: detecting this means reading the SURROUNDING workspace, which
/// is legitimate as an INPUT, and a warning is what keeps it structurally incapable of changing
/// emitted bytes. A refusal would make an emitted-output decision depend on a directory the run does
/// not own. That also sets the accuracy bar: workspace membership (globs, `exclude`, nested
/// workspaces, `default-members`) is cargo's own resolution and this only approximates it, which a
/// warning tolerates and a refusal would not.
fn warn_on_workspace_package_name_collisions(
    crate_root: &std::path::Path,
    generated_packages: &[(String, String)],
) {
    if generated_packages.is_empty() {
        return;
    }
    let Some((ws_manifest, ws_doc)) = nearest_workspace_manifest(crate_root) else {
        return;
    };
    let Some(ws_dir) = ws_manifest.parent() else {
        return;
    };
    let members = workspace_member_packages(ws_dir, &ws_doc);
    for (rel_manifest, name) in generated_packages {
        let ours = crate_root.join(rel_manifest);
        let ours_canonical = std::fs::canonicalize(&ours).unwrap_or(ours);
        let Some(other) = members
            .iter()
            .find(|(path, member_name)| member_name == name && *path != ours_canonical)
        else {
            continue;
        };
        // LOAD-BEARING MESSAGE: it must name BOTH manifests (the user has to know which crate to
        // rename) and the flag that renames ours, because the failure it predicts happens in a
        // different tool, later, and quotes neither path.
        crate::warn!(
            "warning: the generated package `{name}` ({}) has the same name as {}, which is \
             already a member of the cargo workspace rooted at {}. Cargo refuses a workspace \
             holding two packages of one name, so the next `cargo metadata`/`cargo build` over \
             this tree fails with \"two packages named `{name}` in this workspace\" — generation \
             itself cannot see that. Remedy: pass --lib-name to give this crate a name the \
             workspace does not already use, or rename the other crate.",
            ours_canonical.display(),
            other.0.display(),
            ws_manifest.display(),
        );
    }
}

/// The nearest ancestor `Cargo.toml` (starting at `from` itself) carrying a `[workspace]` table,
/// with its parsed document. `None` when the output is not inside a workspace at all — the common
/// standalone case, which must stay silent.
fn nearest_workspace_manifest(
    from: &std::path::Path,
) -> Option<(std::path::PathBuf, toml_edit::DocumentMut)> {
    let start = std::fs::canonicalize(from).unwrap_or_else(|_| from.to_path_buf());
    for dir in start.ancestors() {
        let manifest = dir.join("Cargo.toml");
        let Ok(contents) = std::fs::read_to_string(&manifest) else {
            continue;
        };
        let Ok(doc) = contents.parse::<toml_edit::DocumentMut>() else {
            continue;
        };
        if doc.get("workspace").is_some() {
            return Some((manifest, doc));
        }
    }
    None
}

/// `(canonical manifest path, package name)` for every member of the workspace this scan can
/// resolve: the root's own package if it has one, plus each `workspace.members` entry — literal
/// paths directly, and a `*` entry by scanning the directory before the first `*` one level deep.
/// Entries under `workspace.exclude` and anything under a `target/` directory are dropped.
///
/// Deliberately an approximation, per the warning's accuracy bar: cargo also adopts path
/// dependencies it finds transitively, which would mean resolving the whole dependency graph.
/// Under-reading costs a missed warning; it never invents one, because every name reported here was
/// read out of a real manifest on disk.
fn workspace_member_packages(
    ws_dir: &std::path::Path,
    ws_doc: &toml_edit::DocumentMut,
) -> Vec<(std::path::PathBuf, String)> {
    let string_list = |key: &str| -> Vec<String> {
        ws_doc
            .get("workspace")
            .and_then(|w| w.get(key))
            .and_then(|v| v.as_array())
            .map(|a| {
                a.iter()
                    .filter_map(|v| v.as_str())
                    .map(str::to_owned)
                    .collect()
            })
            .unwrap_or_default()
    };
    let excluded: Vec<std::path::PathBuf> = string_list("exclude")
        .iter()
        .map(|e| ws_dir.join(e))
        .filter_map(|p| std::fs::canonicalize(&p).ok())
        .collect();

    let mut candidate_dirs: Vec<std::path::PathBuf> = vec![ws_dir.to_path_buf()];
    for entry in string_list("members") {
        match entry.find('*') {
            None => candidate_dirs.push(ws_dir.join(&entry)),
            Some(star) => {
                // A bounded glob expansion: the literal prefix's directory, one level deep. That
                // covers the `crates/*` spelling workspaces overwhelmingly use, and a deeper or
                // odder pattern simply contributes nothing.
                let prefix = &entry[..star];
                let base = ws_dir.join(prefix.trim_end_matches('/'));
                if let Ok(read) = std::fs::read_dir(&base) {
                    for child in read.flatten() {
                        if child.path().is_dir() {
                            candidate_dirs.push(child.path());
                        }
                    }
                }
            }
        }
    }

    let mut out = Vec::new();
    let mut read_budget = WORKSPACE_SCAN_MANIFEST_BUDGET;
    for dir in candidate_dirs {
        if read_budget == 0 {
            break;
        }
        let Ok(dir) = std::fs::canonicalize(&dir) else {
            continue;
        };
        if dir.components().any(|c| c.as_os_str() == "target")
            || excluded.iter().any(|e| dir.starts_with(e))
        {
            continue;
        }
        let manifest = dir.join("Cargo.toml");
        let Ok(contents) = std::fs::read_to_string(&manifest) else {
            continue;
        };
        read_budget -= 1;
        if let Some(name) = manifest_package_name(&contents) {
            out.push((manifest, name));
        }
    }
    out
}
