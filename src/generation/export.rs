use super::*;

/// The seed-once thin root written to each generated crate's `src/lib.rs` on the first export only
/// (rust, wasm, and json-gen all share this same content). All regenerated code lives under
/// `src/generated/**` (a subtree the tool always clobbers); this root is user-owned after its first
/// write and never overwritten (existence-only, mirroring `ManifestOp::SeedOnce`), so hand-added
/// modules/re-exports/attrs survive every regeneration.
const SEEDED_CRATE_ROOT: &str = "\
// Seeded by cddl-codegen on first export; never overwritten after that.
// All regenerated code lives in the `generated` module. Add your own
// modules/re-exports/attrs here freely (e.g. `pub mod utils;`).
mod generated;
pub use generated::*;
";

// The json-gen crate's runtime helpers — the row `Registrar` (which owns the ledger carrying the
// document's NAME-INJECTIVITY guard) and `check_schema_ref_closure` (the REFERENCE-CLOSURE check) —
// are no longer emitted per json-gen crate. They live in `static/json_schema_gen.rs`, composed into
// the common runtime crate by `composed_runtime_static_files` under `--json-schema-export`, and the
// json-gen crate `use`s them from there (see the emission site below). That file carries the
// rationale for why both checks run in the CONSUMER's own `cargo run` rather than in this tool's
// suite, and the note that their panic wordings are pinned test keys.

/// The `add_schemas` body's first line when the spec registers at least one row: the registrar that
/// owns the published-name ledger every row is threaded through. A LOCAL, never a parameter, because
/// `add_schemas` keeps its exact published signature (`pub fn add_schemas(generator: &mut
/// schemars::SchemaGenerator)`) — cycle 2 shipped it as the cross-crate composition point, the tool
/// emits calls to it under `--json-schema-dep`, and consumers hand-write one for the layouts the flag
/// does not cover. Emitted AFTER the dep calls: the registrar holds the generator's `&mut` borrow for
/// the rest of the body, and each dep call passes that same borrow on.
const REGISTRAR_DECL: &str = "let mut reg = Registrar::new(generator);";

/// The code-generation provenance banner stamped at the top of every generated `.rs` file in the
/// tool-owned generated trees. Ends with a newline so it prepends cleanly onto rustfmt'd content.
/// `pub(crate)` so the `generated_files_start_with_header` gate asserts against the same banner
/// and path family the stamper uses (a private copy in the test would drift silently).
pub(crate) const CODEGEN_HEADER: &str = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n\n";

/// True for the header-stamped scope families: the tool-owned generated trees under
/// `rust/src/generated/` and `wasm/src/generated/`. The seed-once crate roots (`*/src/lib.rs`),
/// the json-gen crate, and every `Cargo.toml` are deliberately left unstamped.
pub(crate) fn is_header_stamped_path(path: &str) -> bool {
    path.ends_with(".rs")
        && (path.starts_with("rust/src/generated/") || path.starts_with("wasm/src/generated/"))
}

/// True for a `.rs` file the comment-preservation overlay runs on: the tool-owned generated trees
/// (rust, wasm, json-gen) plus the json-gen `main.rs`, which is regenerated wholesale every run
/// (it is NOT seed-once, unlike the three `lib.rs` roots — those and every `Cargo.toml` are the
/// files deliberately outside the overlay).
pub(crate) fn is_preservable_generated_path(path: &str) -> bool {
    path == "wasm/json-gen/src/main.rs"
        || (path.ends_with(".rs")
            && (path.starts_with("rust/src/generated/")
                || path.starts_with("wasm/src/generated/")
                || path.starts_with("wasm/json-gen/src/generated/")))
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

/// The composed rust runtime static files (`error.rs`, `ordered_hash_map.rs`, `non_empty.rs`,
/// `non_empty_map.rs`) shared by the in-crate static export and the `--export-static-crate` path so
/// the two can't drift. Each returned entry is (bare filename, rustfmt'd content). The content
/// COMPOSITION (file concatenation, json/schemars companions, the preserve-encodings
/// BTreeMap→OrderedHashMap substitution for non_empty_map) is identical between the two callers —
/// only WHICH files appear differs: `include_non_empty_vec`/`include_non_empty_map` gate the two
/// NonEmpty runtimes on spec usage in-crate but are forced true for the exported dir (a pure
/// function of the flag set, not of the spec that happened to be run). `ordered_hash_map.rs` is
/// gated on `--preserve-encodings` for both. `serialization.rs` is deliberately NOT here: the
/// in-crate path appends the generated per-type impls to the prelude, while the export-dir path
/// writes the prelude only — each composes that file itself.
///
/// The content is rustfmt'd here (not at the write site) so both callers hand the
/// comment-preservation overlay identical, rustfmt-stable bytes: a preserve-rewrite is written
/// rustfmt'd, so raw content whose rustfmt form differs by a token (e.g. a static's block-arm
/// trailing comma) would make a later run's fresh tokens mismatch the written tokens and trap an
/// already-placed comment with no input change (pinned by
/// `comment_preservation_static_files_rustfmt_stable`).
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
        eprintln!("{}", new_static_file_notice(filename));
    }
}

/// The stderr notice body for a newly-written `--export-static-crate` runtime file (see
/// [`warn_new_static_file`] for why it fires). Pure (no I/O) so the message — the required
/// `pub mod <module>;` edit and the E0432→E0119 cascade signature a consumer sees when the edit is
/// missing — is unit-pinnable without a nested export run.
pub(crate) fn new_static_file_notice(filename: &str) -> String {
    let module = filename.strip_suffix(".rs").unwrap_or(filename);
    format!(
        "warning: NEW static file {filename} written to the --export-static-crate target — \
         declare `pub mod {module};` in the target crate root (its hand-owned `lib.rs`/`mod.rs` \
         is not tool-managed, so the module is otherwise dead in-tree: generated code referencing \
         it fails to resolve — `use …::{module}::…` as E0432, an inline `…::{module}::…` path as \
         E0433 — cascading into spurious E0119 conflicting-implementation errors across generated \
         code). See the consumer-migration notes in docs/current_capacities."
    )
}

fn composed_runtime_static_files(
    cli: &Cli,
    include_non_empty_vec: bool,
    include_non_empty_map: bool,
    include_ordered_set: bool,
    include_pair_map: bool,
    include_any_cbor: bool,
    include_open_struct_rest_json: bool,
) -> std::io::Result<Vec<(String, String)>> {
    let mut out = Vec::new();

    // open_struct_rest_json.rs (the flatten JSON helpers for open struct-map rest rows).
    // JSON-only and `any`-free — a fully-typed `* uint => text` rest row needs it without the AnyCbor
    // runtime, so it is its own module rather than a fragment of `any_cbor.rs`.
    if include_open_struct_rest_json && cli.json_serde_derives {
        let content = std::fs::read_to_string(cli.static_dir.join("open_struct_rest_json.rs"))?;
        out.push((
            "open_struct_rest_json.rs".to_owned(),
            rustfmt_generated_string(&content)?.into_owned(),
        ));
    }

    // error.rs — always, verbatim static/error.rs + rustfmt.
    let error_rs = std::fs::read_to_string(cli.static_dir.join("error.rs"))?;
    out.push((
        "error.rs".to_owned(),
        rustfmt_generated_string(&error_rs)?.into_owned(),
    ));

    // ordered_hash_map.rs — iff --preserve-encodings, with the json/schemars companions appended
    // per the json flags.
    if cli.preserve_encodings {
        let mut ordered_hash_map_rs =
            std::fs::read_to_string(cli.static_dir.join("ordered_hash_map.rs"))?;
        if cli.json_serde_derives {
            ordered_hash_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("ordered_hash_map_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            ordered_hash_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("ordered_hash_map_schemars.rs"),
            )?);
        }
        out.push((
            "ordered_hash_map.rs".to_owned(),
            rustfmt_generated_string(&ordered_hash_map_rs)?.into_owned(),
        ));
    }

    // non_empty.rs (the NonEmptyVec runtime). Its json/schemars companions append under the same
    // flags as the ordered_hash_map ones.
    if include_non_empty_vec {
        let mut non_empty_rs = std::fs::read_to_string(cli.static_dir.join("non_empty.rs"))?;
        if cli.json_serde_derives {
            non_empty_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            non_empty_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_schemars.rs"),
            )?);
        }
        out.push((
            "non_empty.rs".to_owned(),
            rustfmt_generated_string(&non_empty_rs)?.into_owned(),
        ));
    }

    // non_empty_map.rs (the NonEmptyMap runtime). Its inner map is the table type: BTreeMap by
    // default, and under --preserve-encodings a targeted substitution swaps it for OrderedHashMap
    // (import + type token + the extra `Hash + Eq` key bound the hash-map flavor requires),
    // following the ordered_hash_map flavoring precedent. Iteration stays deterministic either way.
    if include_non_empty_map {
        let mut non_empty_map_rs =
            std::fs::read_to_string(cli.static_dir.join("non_empty_map.rs"))?;
        if cli.json_serde_derives {
            non_empty_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_map_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            non_empty_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_map_schemars.rs"),
            )?);
        }
        if cli.preserve_encodings {
            non_empty_map_rs = non_empty_map_rs
                .replace(
                    "use std::collections::BTreeMap;",
                    "use super::ordered_hash_map::OrderedHashMap;",
                )
                .replace("K: Ord", "K: Ord + core::hash::Hash + Eq")
                .replace("BTreeMap", "OrderedHashMap");
        }
        out.push((
            "non_empty_map.rs".to_owned(),
            rustfmt_generated_string(&non_empty_map_rs)?.into_owned(),
        ));
    }

    // ordered_set.rs (the OrderedSet / NonEmptyOrderedSet `@duplicates reject` runtime). Its
    // json/schemars companions append under the same flags as the other runtimes.
    if include_ordered_set {
        let mut ordered_set_rs = std::fs::read_to_string(cli.static_dir.join("ordered_set.rs"))?;
        if cli.json_serde_derives {
            ordered_set_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("ordered_set_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            ordered_set_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("ordered_set_schemars.rs"),
            )?);
        }
        out.push((
            "ordered_set.rs".to_owned(),
            rustfmt_generated_string(&ordered_set_rs)?.into_owned(),
        ));
    }

    // pair_map.rs (the PairMap / NonEmptyPairMap `@duplicates preserve` runtime). Its json/schemars
    // companions append under the same flags as the other runtimes.
    if include_pair_map {
        let mut pair_map_rs = std::fs::read_to_string(cli.static_dir.join("pair_map.rs"))?;
        if cli.json_serde_derives {
            pair_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("pair_map_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            pair_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("pair_map_schemars.rs"),
            )?);
        }
        out.push((
            "pair_map.rs".to_owned(),
            rustfmt_generated_string(&pair_map_rs)?.into_owned(),
        ));
    }

    // any_cbor.rs (the AnyCbor runtime — the lowering of CDDL `any`). Own-module route, mirroring
    // ordered_hash_map: the mode-appropriate fragment concat, emitted only when the finalized IR
    // contains `any`. The fragments reference the serialization prelude (StringEncoding/LenEncoding/
    // sz_max/Deserialize/local Serialize) and error types, so the module opens with the same
    // `super::…` imports the standalone serialization prelude uses — in-crate `super` is
    // `generated`, and under `--export-static-crate` it is the target crate's flat module dir.
    // The mode split follows the serialization prelude's own: preserve carries the encoding-bearing
    // base + exactly one canonical fragment; non-preserve is the plain structural variant.
    if include_any_cbor {
        let mut any_cbor_rs = String::from(
            "use super::error::{DeserializeError, DeserializeFailure};\n\
             use super::serialization::*;\n\n",
        );
        // Depth-guard hook: the `read` recursion seam in the fragments calls
        // `any_cbor_recursion_guard!()`. We define that macro per-flag here — an `acquire` of the
        // opt-in `DepthGuard` (baked limit, same value the composite deserializers bake and the same
        // thread-local counter, reached via the `use super::serialization::*` glob) under
        // `--deserialize-depth-limit`, or a no-op otherwise. Defining it BEFORE the `include`d
        // fragment content satisfies macro_rules' define-before-use rule.
        match cli.deserialize_depth_limit {
            Some(limit) => any_cbor_rs.push_str(&format!(
                "macro_rules! any_cbor_recursion_guard {{ () => {{ let _depth_guard = DepthGuard::acquire({limit}usize)?; }}; }}\n\n"
            )),
            None => any_cbor_rs
                .push_str("macro_rules! any_cbor_recursion_guard {\n    () => {};\n}\n\n"),
        }
        if cli.preserve_encodings {
            any_cbor_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("any_cbor_preserve.rs"),
            )?);
            let canonical_fragment = if cli.canonical_form {
                "any_cbor_preserve_force_canonical.rs"
            } else {
                "any_cbor_preserve_non_force_canonical.rs"
            };
            any_cbor_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join(canonical_fragment),
            )?);
        } else {
            any_cbor_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("any_cbor_non_preserve.rs"),
            )?);
        }
        // JSON companions — the mode-agnostic serde + schemars impls, appended under the same flags
        // as the ordered_hash_map ones (a single fragment serves both preserve/non-preserve because
        // it is written against the `kind()`/`as_*`/`new_*` mode-independent surface).
        if cli.json_serde_derives {
            any_cbor_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("any_cbor_json.rs"),
            )?);
            // Preserve-only natural-JSON adapters for `any`-valued MAP members (OrderedHashMap): a
            // `{* K => any}` member is `OrderedHashMap<K, AnyCbor>` under --preserve-encodings.
            // `ordered_hash_map.rs` is emitted unconditionally under preserve (above), so the
            // OrderedHashMap this fragment names is always in scope — this is the existing
            // conditional-assembly mechanism, no parallel path.
            if cli.preserve_encodings {
                any_cbor_rs.push_str(&std::fs::read_to_string(
                    cli.static_dir.join("any_cbor_json_preserve.rs"),
                )?);
            }
        }
        if cli.json_schema_export {
            any_cbor_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("any_cbor_schemars.rs"),
            )?);
        }
        out.push((
            "any_cbor.rs".to_owned(),
            rustfmt_generated_string(&any_cbor_rs)?.into_owned(),
        ));
    }

    // json_schema_gen.rs (the `--json-schema-export` row registrar + reference-closure check the
    // json-gen crates `use` from here). Verbatim, no flavour composition: it is written against
    // `schemars`/`serde_json` only and reads the reference namespace off the generator at runtime,
    // so no flag changes its body. Gated on the FLAG alone and never on spec usage — a json-gen
    // crate importing it exists exactly when `--json-schema-export` is on, whatever the spec holds.
    if cli.json_schema_export {
        let json_schema_gen_rs =
            std::fs::read_to_string(cli.static_dir.join("json_schema_gen.rs"))?;
        out.push((
            "json_schema_gen.rs".to_owned(),
            rustfmt_generated_string(&json_schema_gen_rs)?.into_owned(),
        ));
    }

    Ok(out)
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

/// Prepend the codegen header onto a (already rustfmt'd) generated file's content. The header is
/// pure `//` comments, so it leads the file verbatim regardless of whether the body opens with an
/// inner `#![…]` attribute (both orderings are valid Rust; a comment may precede an inner attr).
fn stamp_codegen_header(content: &str) -> String {
    format!("{CODEGEN_HEADER}{content}")
}

/// If `line` is a line-leading top-level type-namespace definition — `pub struct`/`pub enum`/`pub
/// type` at column 0, exactly how `codegen` emits items at the file root — return the defined
/// ident. Drives the `generated_files` duplicate-ident backstop. The leading-anchor (no
/// `strip_prefix` for indented forms) excludes nested items inside `mod {}` blocks (indented) and
/// the anchor keywords exclude other namespaces (`impl`/`fn`/`use`), which never collide in the
/// type namespace. Returns `None` for anything else.
fn top_level_type_ident(line: &str) -> Option<&str> {
    let rest = line
        .strip_prefix("pub struct ")
        .or_else(|| line.strip_prefix("pub enum "))
        .or_else(|| line.strip_prefix("pub type "))?;
    let ident = rest
        .split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .next()?;
    (!ident.is_empty()).then_some(ident)
}

/// Run-output surfacing helper for the own-spec extern re-export contract (Layer 1). Prints nothing
/// for an empty set (specs with no own-spec externs are silent), otherwise one header line naming
/// the crate root plus one `pub use` hint per required name in deterministic (`BTreeSet`-sorted)
/// order. Stdout, alongside the existing `print!` progress logging.
fn print_required_reexports(root_rel_path: &str, required: &std::collections::BTreeSet<String>) {
    if required.is_empty() {
        return;
    }
    println!(
        "Own-spec extern re-export contract: the hand-written {root_rel_path} must re-export these \
         names for the generated glue to resolve:"
    );
    for name in required {
        println!("    pub use <your_module>::{name};");
    }
}

/// The required-name subset a seed-once `lib.rs` does NOT already provide (Layer 3 diagnostic input).
/// A name counts as provided when it appears in the file text as a WHOLE identifier (word-boundary
/// match) — so `Foo` inside `FooBar` is not a false match. Glob-noise carve-out: if the file
/// re-exports through a glob whose path's last segment is NOT `generated` (`pub use <path>::*;`), the
/// scan cannot see which names that glob provides, so it suppresses ALL per-name warnings rather than
/// risk a false positive. The tool's own `pub use generated::*;` (last segment `generated`) does NOT
/// suppress: the extern is defined OUTSIDE `generated/**`, so that glob provably never provides it.
fn missing_reexports(lib_text: &str, required: &std::collections::BTreeSet<String>) -> Vec<String> {
    if has_foreign_glob(lib_text) {
        return Vec::new();
    }
    required
        .iter()
        .filter(|name| !contains_whole_ident(lib_text, name))
        .cloned()
        .collect()
}

/// True if `text` contains a `use` item ending in a glob (`::*`) whose path's last segment is not
/// `generated` — a re-export the name-based scan can't see through. See [`missing_reexports`].
fn has_foreign_glob(text: &str) -> bool {
    for line in text.lines() {
        let line = line.trim();
        let Some(use_idx) = line.find("use ") else {
            continue;
        };
        // Only treat this as a `use` item when nothing but a visibility modifier precedes `use `
        // (so a `use` appearing inside some other construct's text doesn't trip the carve-out).
        let prefix = line[..use_idx].trim();
        let is_use_item = prefix.is_empty()
            || prefix == "pub"
            || (prefix.starts_with("pub(") && prefix.ends_with(')'));
        if !is_use_item {
            continue;
        }
        let after = &line[use_idx + "use ".len()..];
        if let Some(star_idx) = after.find("::*") {
            let path = after[..star_idx].trim();
            let last_seg = path.rsplit("::").next().unwrap_or("");
            if last_seg != "generated" {
                return true;
            }
        }
    }
    false
}

/// True if `needle` occurs in `haystack` as a whole ASCII identifier (neither neighbouring byte is an
/// identifier byte). Used by [`missing_reexports`] so a required name is only "provided" on a real
/// token match, not a substring of a longer ident.
fn contains_whole_ident(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }
    let hb = haystack.as_bytes();
    let mut search_from = 0;
    while let Some(rel) = haystack[search_from..].find(needle) {
        let start = search_from + rel;
        let end = start + needle.len();
        let before_ok = start == 0 || !is_ident_byte(hb[start - 1]);
        let after_ok = end == hb.len() || !is_ident_byte(hb[end]);
        if before_ok && after_ok {
            return true;
        }
        search_from = start + 1;
    }
    false
}

fn is_ident_byte(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphanumeric()
}

pub(crate) fn concat_files<P: AsRef<Path>>(paths: &Vec<P>) -> std::io::Result<String> {
    let mut buf = String::new();
    for path in paths {
        buf.push_str(&std::fs::read_to_string(path).map_err(|e| {
            std::io::Error::new(
                e.kind(),
                format!("can't read {}: {e}", path.as_ref().display()),
            )
        })?);
    }
    Ok(buf)
}

pub(super) fn declare_modules(
    gen_scopes: &mut BTreeMap<ModuleScope, codegen::Scope>,
    module_scopes: &[ModuleScope],
) {
    for module_scope in module_scopes.iter() {
        if module_scope.export() {
            let components = module_scope.components();
            for (i, component) in components.iter().enumerate().skip(1) {
                gen_scopes
                    .entry(module_scope.parents(i))
                    .or_default()
                    .raw(format!("pub mod {};", component));
            }
        }
    }
}

/// Gets the rustfmt path to rustfmt the generated bindings.
fn rustfmt_path() -> std::io::Result<std::path::PathBuf> {
    if let Ok(rustfmt) = std::env::var("RUSTFMT") {
        return Ok(rustfmt.into());
    }
    #[cfg(feature = "which-rustfmt")]
    match which::which("rustfmt") {
        Ok(p) => Ok(p),
        Err(e) => Err(std::io::Error::other(format!("{e}"))),
    }
    #[cfg(not(feature = "which-rustfmt"))]
    Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        "which wasn't enabled, and no rustfmt binary specified",
    ))
}

/// Runs rustfmt on the string.
///
/// Import pruning is NOT done here: the usage-derived prune (`import_prune.rs`) needs to see a
/// file's descendant modules (a parent module's import can be consumed by a child via
/// `use super::*;`, so per-file "ident absent from this file" does NOT imply unused), so it runs
/// once over the full file map in `generated_files` — see `import_prune::prune_generated_files`.
pub fn rustfmt_generated_string(source: &str) -> std::io::Result<Cow<'_, str>> {
    let mut cmd = Command::new(rustfmt_path().unwrap());
    cmd.stdin(Stdio::piped()).stdout(Stdio::piped());

    // We invoke rustfmt directly on stdin, so it has no Cargo.toml to read the edition from and
    // defaults to 2015. Pass the generated crates' edition explicitly (as `cargo fmt` would) so our
    // output is already canonical and doesn't churn under a downstream `cargo fmt`.
    cmd.args(["--edition", "2024"]);

    let mut child = cmd.spawn()?;
    let mut child_stdin = child.stdin.take().unwrap();
    let mut child_stdout = child.stdout.take().unwrap();

    let source = source.to_owned();

    // Write to stdin in a new thread, so that we can read from stdout on this
    // thread. This keeps the child from blocking on writing to its stdout which
    // might block us from writing to its stdin.
    let stdin_handle = std::thread::spawn(move || {
        let _ = child_stdin.write_all(source.as_bytes());
        source
    });

    let mut output = vec![];
    std::io::copy(&mut child_stdout, &mut output)?;

    let status = child.wait()?;
    let source = stdin_handle.join().expect(
        "The thread writing to rustfmt's stdin doesn't do \
         anything that could panic",
    );

    match String::from_utf8(output) {
        Ok(bindings) => match status.code() {
            Some(0) => Ok(Cow::Owned(bindings)),
            // exit 2 = rustfmt could not PARSE the input: the generator emitted invalid Rust. This
            // used to be swallowed (return the unformatted source, exit 0), which is exactly how the
            // JSON-schema turbofish bug shipped green. Fail loud instead — the rustfmt errors are on
            // stderr (inherited) above; a parse failure is always a generator bug, never benign.
            Some(2) => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "rustfmt rejected the generated source as unparseable — this is a generator bug \
                 (see the rustfmt errors above)",
            )),
            // exit 3 = formatted fine but gave up on SOME lines: the output is still valid Rust, so
            // keep it (not a correctness problem, just cosmetic).
            Some(3) => {
                println!("Rustfmt could not format some lines.");
                Ok(Cow::Owned(bindings))
            }
            // any other exit (rustfmt internal error) — the turbofish bug actually hit this arm, not
            // exit 2 — also indicates the generator fed rustfmt something it couldn't handle. Fatal.
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "rustfmt failed on the generated source (internal error) — likely invalid Rust \
                 emitted; this is a generator bug (see the rustfmt output above)",
            )),
        },
        _ => Ok(Cow::Owned(source)),
    }
}

impl GenerationScope {
    /// Exports all already-generated state to the provided directory.
    /// Call generate() first to populate the generation state.
    pub fn export(
        &self,
        types: &IntermediateTypes,
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<()> {
        // check it exists here to get clearer error message
        assert!(std::path::Path::exists(&cli.static_dir));

        // package.json / scripts
        //
        // The two JSON-schema -> TypeScript scripts are copied independently of the npm manifest, so
        // a consumer that hand-maintains its own `package.json` can still opt into the canonical
        // scripts (`--json-schema-scripts`) instead of forking them. `--package-json
        // --json-schema-export` keeps copying them exactly as before, so passing both flags copies
        // once rather than twice. The scripts resolve their own paths from `<root>/scripts/*.js`, so
        // they work under either layout (see `static/run-json2ts.js`'s `resolveWasmDir`).
        if cli.json_schema_scripts || (cli.package_json && cli.json_schema_export) {
            std::fs::create_dir_all(cli.output.join("scripts"))?;
            std::fs::copy(
                cli.static_dir.join("run-json2ts.js"),
                cli.output.join("scripts/run-json2ts.js"),
            )?;
            std::fs::copy(
                cli.static_dir.join("json-ts-types.js"),
                cli.output.join("scripts/json-ts-types.js"),
            )?;
        }
        let rust_dir = if cli.package_json {
            if cli.json_schema_export {
                std::fs::copy(
                    cli.static_dir.join("package_json_schemas.json"),
                    cli.output.join("package.json"),
                )?;
            } else {
                std::fs::copy(
                    cli.static_dir.join("package.json"),
                    cli.output.join("package.json"),
                )?;
            }
            cli.output.join("rust")
        } else {
            cli.output.clone()
        };

        // Run-output surfacing of the own-spec extern re-export contract. When this spec declares
        // externs in exported scopes, the generated glue (`pub use crate::<Name>;` in each declaring
        // scope's `mod.rs`) resolves only if the hand-written crate-root `lib.rs` re-exports those
        // names — a contract the generator alone knows the exact required set of at emission time.
        // Printing it here (deterministic, sorted; rust then wasm) turns a bare E0432 on a stale root
        // into an actionable checklist. Reads only this run's computed sets, never prior output.
        print_required_reexports("rust/src/lib.rs", &self.required_rust_reexports);
        if cli.wasm {
            print_required_reexports("wasm/src/lib.rs", &self.required_wasm_reexports);
        }

        // All generated files come from the single producer the snapshot tests also use, so the
        // shipped output and the tested output can't drift.
        let mut files = self.generated_files(types, export_raw_bytes_encoding_trait, cli)?;

        // `generated_files` produces serialization.rs generated-only; the shipped root one has the
        // static serialization prelude prepended and is rustfmt'd together (exactly as before).
        if cli.export_static_files() {
            let mut merged = codegen::Scope::new();
            merged.raw(Self::serialization_prelude(
                export_raw_bytes_encoding_trait,
                cli,
            )?);
            merged.append(&self.rust_serialize_lib_scope);
            for (scope, content) in &self.serialize_scopes {
                if *scope == *ROOT_SCOPE {
                    merged.append(&content.clone());
                }
            }
            // Restamp: `generated_files` already stamped its generated-only serialization.rs, but
            // this rebuilt version (static prelude + merged ROOT serialize scope) replaces it, so it
            // needs the header applied here too (this is a header-stamped path).
            files.insert(
                "rust/src/generated/serialization.rs".to_owned(),
                stamp_codegen_header(&rustfmt_generated_string(&merged.to_string())?),
            );
            // The import prune in `generated_files` saw serialization.rs GENERATED-ONLY (no prelude),
            // so a root scope with no per-type serialize impls looked like a bare re-export file and
            // the rebuild above reintroduced the blindly-pushed `use super::*;`. Re-run the prune over
            // the rebuilt map: now serialization.rs carries the prelude (code), so the glob-prune
            // decides `super::*` against real usage (dropped when nothing names a root-module item,
            // kept where the prelude/impls do). Every other file is already a prune fixed point, so
            // only serialization.rs can change here.
            let prune_config = self.build_prune_config(cli);
            for (path, pruned) in crate::import_prune::prune_generated_files(&files, &prune_config)
            {
                files.insert(path, rustfmt_generated_string(&pruned)?.into_owned());
            }
        }

        // Manifests merge into whatever is already on disk (the declarative changeset) rather than
        // clobbering, so user edits to keys the tool doesn't own survive regeneration. This is one of
        // the bounded exceptions where output depends on prior directory contents (the others: the
        // seed-once crate roots below, and the comment-preservation overlay in the write loop), and
        // only as the changeset contract allows: keys no op mentions pass through, `SeedOnce` checks
        // existence. An unparseable
        // existing manifest is a hard error naming the file (see `cargo_manifest::apply`) — never a
        // silent clobber. `generated_files` above produced these same manifests against an empty
        // document; here we re-derive them against the on-disk file before the common write loop.
        let mut manifest_ops = vec![(
            "rust/Cargo.toml",
            crate::cargo_manifest::ops_for_rust(types, export_raw_bytes_encoding_trait, cli)?,
        )];
        if cli.wasm {
            manifest_ops.push(("wasm/Cargo.toml", crate::cargo_manifest::ops_for_wasm(cli)?));
        }
        if cli.json_schema_export {
            manifest_ops.push((
                "wasm/json-gen/Cargo.toml",
                crate::cargo_manifest::ops_for_json_gen(cli)?,
            ));
        }
        for (rel_path, ops) in &manifest_ops {
            if files.contains_key(*rel_path) {
                let existing = std::fs::read_to_string(rust_dir.join(rel_path)).ok();
                let merged = crate::cargo_manifest::apply(ops, existing.as_deref(), rel_path)
                    .map_err(std::io::Error::other)?;
                files.insert((*rel_path).to_owned(), merged);
            }
        }

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
        let prune_config = self.build_prune_config(cli);
        let mut overlay_changed_any = false;
        if cli.preserve_comments {
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
                "rust/src/lib.rs" | "wasm/src/lib.rs" | "wasm/json-gen/src/lib.rs"
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
                        eprintln!(
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

        // static files copied/assembled verbatim (only when we own the common types). The runtime
        // composition (error.rs / ordered_hash_map.rs / non_empty.rs / non_empty_map.rs) is shared
        // with the `--export-static-crate` path via `composed_runtime_static_files` so the two can't
        // drift; the returned content is already rustfmt'd (load-bearing for the overlay — see that
        // helper). In-crate the NonEmpty runtimes are gated on spec usage: only for crates that use
        // `[+ T]` / `{+ k => v}`. `--wrapper-requests`: a dep hosting a requested NonEmpty wrapper
        // needs the runtime file even when its own spec has none. These composed statics are NOT in
        // the generated-file `files` map, so they miss the map-level overlay + re-prune above and get
        // their comment preservation per file here via `write_rs_with_preserve` — sound because they
        // carry no prunable imports (nothing to re-prune), so per-file preservation is complete.
        if cli.export_static_files() {
            let runtime_files = composed_runtime_static_files(
                cli,
                types.uses_non_empty_vec() || self.requested_non_empty_vec,
                types.uses_non_empty_map() || self.requested_non_empty_map,
                types.uses_ordered_set() || self.requested_ordered_set,
                types.uses_pair_map() || self.requested_pair_map,
                types.uses_any_cbor(),
                types.uses_open_struct_rest(),
            )?;
            for (filename, content) in &runtime_files {
                let rel_path = format!("rust/src/generated/{filename}");
                let path = rust_dir.join(&rel_path);
                write_rs_with_preserve(&path, &rel_path, content, cli.preserve_comments)?;
                written_generated_rs.insert(path);
            }
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
                "rust/src/lib.rs" => (&self.required_rust_reexports, "rust/src/generated"),
                "wasm/src/lib.rs" => (&self.required_wasm_reexports, "wasm/src/generated"),
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
                eprintln!(
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
        // --common-import-override users). The exported set is a PURE FUNCTION OF THE FLAG SET,
        // never of the spec: the two NonEmpty runtimes are ALWAYS included (unlike the spec-usage
        // gating in-crate) and serialization.rs always includes raw_bytes_encoding — a shared
        // runtime crate serves many specs, so which spec was run must not change the output.
        // serialization.rs here is the composed static PRELUDE ONLY (no generated per-type impls
        // appended). No mod.rs/lib.rs is written — the target crate owns its module declarations;
        // static files reference siblings via `super::…`, so a flat module dir works. This crate is
        // OUTSIDE the output crate, so its paths are deliberately not added to
        // `written_generated_rs` / the stale-file scan.
        if let Some(export_crate) = &cli.export_static_crate {
            let export_dir = export_crate.join("src");
            std::fs::create_dir_all(&export_dir)?;
            let runtime_files =
                composed_runtime_static_files(cli, true, true, true, true, true, true)?;
            for (filename, content) in &runtime_files {
                let path = export_dir.join(filename);
                let is_new = !path.exists();
                write_rs_with_preserve(&path, filename, content, cli.preserve_comments)?;
                warn_new_static_file(is_new, filename);
            }
            // serialization.rs — the static prelude only. `export_raw_bytes_encoding_trait` is
            // forced true (always include raw_bytes_encoding, per the pure-function-of-flags rule).
            // rustfmt'd before the preserve write, exactly like the composed runtime files.
            //
            // The prelude carries no `use` statements of its own: in-crate it is prepended to the
            // generated root serialization.rs, whose emitted import block serves the whole module
            // (`use` is scope-wide regardless of position). Standalone, the exported file must
            // bring its own imports or it does not compile. Every prelude flavor references all of
            // these (Deserialize/Serialize traits are in the base file).
            let prelude = format!(
                "use super::error::{{DeserializeError, DeserializeFailure}};\n\
                 use cbor_event::de::Deserializer;\n\
                 use cbor_event::se::Serializer;\n\n{}",
                Self::serialization_prelude(true, cli)?
            );
            let serialization_path = export_dir.join("serialization.rs");
            let serialization_is_new = !serialization_path.exists();
            write_rs_with_preserve(
                &serialization_path,
                "serialization.rs",
                rustfmt_generated_string(&prelude)?.as_ref(),
                cli.preserve_comments,
            )?;
            warn_new_static_file(serialization_is_new, "serialization.rs");

            // The crate's Cargo.toml gets the static-runtime changeset merged in — the exported
            // source and the manifest that has to satisfy its dependencies are one artifact, so the
            // tool never writes one without the other (the pre-crate-shaped flag left the manifest
            // untouched, and a cbor_event bump in the exported source silently skewed against the
            // target crate's pin). Same declarative-merge contract as the generated crates' three
            // manifests: hand keys the changeset doesn't mention pass through, an unparseable
            // existing manifest is a hard error naming the file.
            let manifest_path = export_crate.join("Cargo.toml");
            let existing = std::fs::read_to_string(&manifest_path).ok();
            let merged = crate::cargo_manifest::apply(
                &crate::cargo_manifest::ops_for_static_runtime(cli)?,
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
        let extern_interface_files =
            crate::generation::extern_interface::extern_interface_files(types, cli);
        let extern_interface_dir = cli.output.join("extern-interface");
        if extern_interface_dir.exists() {
            std::fs::remove_dir_all(&extern_interface_dir)?;
        }
        for (rel_path, content) in &extern_interface_files {
            let path = cli.output.join(rel_path);
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
        for tree in [
            "rust/src/generated",
            "wasm/src/generated",
            "wasm/json-gen/src/generated",
        ] {
            let mut orphans = Vec::new();
            collect_rs_files(&rust_dir.join(tree), &mut orphans)?;
            orphans.retain(|p| !written_generated_rs.contains(p));
            orphans.sort();
            for orphan in orphans {
                eprintln!(
                    "warning: {} was generated by a previous run but is no longer generated; it is \
                     orphaned (nothing declares it as a module). Delete it — any comments you \
                     added there are NOT carried anywhere.",
                    orphan.display()
                );
            }
        }

        Ok(())
    }

    /// Shared scope-merge producer used by both [`Self::export`] and [`Self::generated_files`]:
    /// merges the root scope into `merged_scope`, emits each non-root module scope as its own
    /// file, and inserts the (rustfmt'd) results into `out` keyed by `<dir>/.../<name>`.
    fn merge_scopes_to_strings(
        out: &mut BTreeMap<String, String>,
        dir: &str,
        mut merged_scope: codegen::Scope,
        other_scopes: &BTreeMap<ModuleScope, codegen::Scope>,
        root_name: &str,
        inner_name: &str,
    ) -> std::io::Result<()> {
        for (scope, content) in other_scopes {
            if *scope == *ROOT_SCOPE {
                merged_scope.append(&content.clone());
            } else if scope.export() {
                let path = format!("{dir}/{}/{inner_name}", scope.components().join("/"));
                out.insert(
                    path,
                    rustfmt_generated_string(&content.to_string())?.into_owned(),
                );
            }
        }
        out.insert(
            format!("{dir}/{root_name}"),
            rustfmt_generated_string(&merged_scope.to_string())?.into_owned(),
        );
        Ok(())
    }

    /// The static serialization runtime prelude (concatenated from `static/serialization*.rs`)
    /// that `export` prepends to the root serialization.rs. Exposed so it can be snapshotted on
    /// its own (it ships verbatim but varies by `--preserve-encodings`/`--canonical-form`).
    pub(crate) fn serialization_prelude(
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<String> {
        let mut serialize_paths = vec![cli.static_dir.join("serialization.rs")];
        if cli.preserve_encodings {
            serialize_paths.push(cli.static_dir.join("serialization_preserve.rs"));
            if cli.canonical_form {
                serialize_paths.push(
                    cli.static_dir
                        .join("serialization_preserve_force_canonical.rs"),
                );
            } else {
                serialize_paths.push(
                    cli.static_dir
                        .join("serialization_preserve_non_force_canonical.rs"),
                );
                serialize_paths.push(cli.static_dir.join("serialization_non_force_canonical.rs"));
            }
        } else {
            serialize_paths.push(cli.static_dir.join("serialization_non_preserve.rs"));
            serialize_paths.push(cli.static_dir.join("serialization_non_force_canonical.rs"));
        }
        if export_raw_bytes_encoding_trait {
            serialize_paths.push(cli.static_dir.join("raw_bytes_encoding.rs"));
        }
        // Opt-in recursion depth guard runtime (the `DepthGuard` RAII type + thread-local counter).
        // Conditioned like the preserve-encodings runtime so crates generated without the flag carry
        // no dead runtime code. The `DepthLimitExceeded` failure variant it constructs lives in the
        // verbatim-copied error.rs (a monolithic pub enum a file-concat can't conditionally extend;
        // a pub variant is not dead code), so only this function/thread-local piece is gated here.
        if cli.deserialize_depth_limit.is_some() {
            serialize_paths.push(cli.static_dir.join("serialization_depth_guard.rs"));
        }
        concat_files(&serialize_paths)
    }

    /// Single producer for every generated source file (post-rustfmt), keyed by path relative to
    /// the crate root (e.g. "rust/src/lib.rs"). Used by BOTH [`Self::export`] (which writes these
    /// to disk, after prepending the static serialization prelude to the root serialization.rs)
    /// and the snapshot tests — so the shipped path and the tested path can't drift. The
    /// serialization.rs here is generated-only; the static prelude and verbatim-copied files
    /// (error.rs, ordered_hash_map.rs, package.json, scripts) are handled directly by `export`.
    pub(crate) fn generated_files(
        &self,
        types: &IntermediateTypes,
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<BTreeMap<String, String>> {
        let mut out = BTreeMap::new();

        // rust generated/mod.rs (merged ROOT_SCOPE content + module decls + inner crate attrs) /
        // generated/{module}/mod.rs. The tool-owned generated tree lives under `generated/`; the
        // crate root `lib.rs` is a seed-once thin root (added below) that the tool never clobbers.
        Self::merge_scopes_to_strings(
            &mut out,
            "rust/src/generated",
            self.rust_lib_scope.clone(),
            &self.rust_scopes,
            "mod.rs",
            "mod.rs",
        )?;

        // The seed-once thin root: written to `rust/src/lib.rs` only if absent (existence-only,
        // mirroring `ManifestOp::SeedOnce`). Included in the producer so clean runs / snapshots carry
        // it, but `export`'s write loop skips it when the file already exists so user edits survive.
        out.insert(
            "rust/src/lib.rs".to_owned(),
            rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
        );

        // serialization.rs (generated impls only; export prepends the static prelude to the root)
        let mut serialize_scope = codegen::Scope::new();
        serialize_scope.append(&self.rust_serialize_lib_scope);
        Self::merge_scopes_to_strings(
            &mut out,
            "rust/src/generated",
            serialize_scope,
            &self.serialize_scopes,
            "serialization.rs",
            "serialization.rs",
        )?;

        // cbor_encodings.rs / {module}/cbor_encodings.rs
        if cli.preserve_encodings {
            for (scope, contents) in self.cbor_encodings_scopes.iter() {
                if scope.export() {
                    let path = if *scope == *ROOT_SCOPE {
                        "rust/src/generated/cbor_encodings.rs".to_owned()
                    } else {
                        format!(
                            "rust/src/generated/{}/cbor_encodings.rs",
                            scope.components().join("/")
                        )
                    };
                    out.insert(
                        path,
                        rustfmt_generated_string(&contents.to_string())?.into_owned(),
                    );
                }
            }
        }

        // rust Cargo.toml — declarative changeset applied to an empty document (pure, so the
        // snapshot tests keep consuming the same producer). `export` re-applies the same ops onto
        // any on-disk manifest so user edits survive.
        out.insert(
            "rust/Cargo.toml".to_owned(),
            crate::cargo_manifest::apply(
                &crate::cargo_manifest::ops_for_rust(types, export_raw_bytes_encoding_trait, cli)?,
                None,
                "rust/Cargo.toml",
            )
            .map_err(std::io::Error::other)?,
        );

        // Borrowed-key-types sidecar (`--workspace-dep`): the rust-crate analog of
        // `borrowed_collections.rs` for the map-key-derive concern. A consumer map keyed on a dep type
        // (`{* dep_key => …}`) marks `dep_key` used-as-key in finalize, but the derive lives in the
        // DEP's crate; when the value is consumer-owned (`{* dep_key => my_local}`) the wrapper is not
        // all-one-dep and never enters `borrowed_collections.rs`, yet the dep must still derive the key
        // traits on `dep_key` or the consumer's rust crate fails to build. This file records every such
        // borrowed key type so the dep can re-read it via `--key-requests`. Emitted whenever the flag
        // is present — INCLUDING when nothing is borrowed (stable presence/diffs) — and never
        // otherwise, mirroring `borrowed_collections.rs`. Fixed format: the four-line banner, a
        // `_assert_key_traits` bound-carrier + a `_borrowed_key_types_self_check` fn (the compiled half
        // — a dep dropping a derive fails THIS crate's build naming the type), and the
        // `#[allow(dead_code)] pub(crate) const BORROWED_KEY_TYPES` machine table (rows sorted by
        // (dep, ident); the first column is the dep's RUST crate name — the extern-deps dir name).
        if !self.workspace_deps.is_empty() {
            // Each row carries `(dep-crate name, cddl ident, self-check module path, demand)`. The
            // machine TABLE only emits `(dep, ident[, flavor])`; the module-path column feeds the
            // compiled self-check ONLY, so a dep type living in a non-root scope is asserted at its
            // TRUE path (`{dep}::{scope…}::{Ident}`) — the same path the consumer's own generated
            // `use` lines take. A dep-ROOT type's scope is just the dep crate name, so its path
            // column equals `{dep}` and root-only sidecars stay byte-identical.
            let mut rows: Vec<(String, String, String, DemandSet)> = Vec::new();
            let int_ident = RustIdent::new(CDDLIdent::new("int"));
            for ident in types.used_as_key_idents() {
                // The built-in `Int` extern lives in ROOT (export) scope, so the scope-attribution
                // skip below never sees it — but under `--common-import-override` this crate re-exports
                // the COMMON crate's `Int` (Phase 1), so a key-demanded `Int` is morally a borrowed key
                // of that crate. Record the row `(<override>, "int", demand)` IFF the override names a
                // configured `--workspace-dep` (the dep column is a crate name, which also excludes a
                // path-form override like `crate::common`). When it does not, no row and no error: the
                // consumer's own map sites fail E0277 naming `Int`, the documented degraded path — the
                // flavor channel requires the common crate to also be a `--workspace-dep`.
                if *ident == int_ident {
                    if !cli.export_static_files() {
                        let common = cli.common_import_rust();
                        if self.workspace_deps.contains(common) {
                            let demand = types.key_demand(ident).unwrap_or_default();
                            // `Int` is root-visible in the common crate, so its self-check path is
                            // just the crate name — `{common}::Int`.
                            rows.push((
                                common.to_owned(),
                                "int".to_owned(),
                                common.to_owned(),
                                demand,
                            ));
                        }
                    }
                    continue;
                }
                let scope = types.scope(ident);
                if scope.export() {
                    continue;
                }
                let Some(dep) = scope.components().first() else {
                    continue;
                };
                if !self.workspace_deps.contains(dep) {
                    continue;
                }
                let demand = types.key_demand(ident).unwrap_or_default();
                // The full non-export scope path (`{dep}::{scope…}`) is what the self-check asserts
                // on — the dep's thin root does not re-export scope contents, so a bare `{dep}::Ident`
                // would be E0412 for a scoped type. For a dep-ROOT type this is just `{dep}`.
                let scope_path = scope.components().join("::");
                rows.push((
                    dep.clone(),
                    convert_to_snake_case(ident.as_ref()),
                    scope_path,
                    demand,
                ));
            }
            rows.sort();
            rows.dedup();
            // A borrowed key whose demand carries a `hash`/`ord` FLAVOR (a consumer keyed the dep type
            // through a `@used_as_key hash`/`ord` root) needs the flavored 3-column format + per-flavor
            // self-check bound. When every borrowed key is `bare` (the universal pre-flavor case), the
            // legacy 2-column form is emitted BYTE-IDENTICALLY — no banner/type/self-check churn.
            let any_flavored = rows.iter().any(|(_, _, _, d)| d.hash || d.ord);
            let sidecar = if any_flavored {
                let mut s = String::from(
                    "// This file records every map-key type this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
                     // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
                     // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
                     // Rows are (dep rust-crate name, cddl ident, demand flavor) of each borrowed map-key type.\n",
                );
                // One bound-carrier per distinct demand (the flavor decides the bound), then a
                // per-row self-check call routed to its flavor's carrier.
                let mut demands: Vec<DemandSet> = rows.iter().map(|(_, _, _, d)| *d).collect();
                demands.sort();
                demands.dedup();
                let assert_fn = |d: DemandSet| {
                    format!(
                        "_assert_key_traits_{}",
                        key_flavor_token(d).replace(' ', "_")
                    )
                };
                for d in &demands {
                    s.push_str(&format!(
                        "#[allow(dead_code)]\nfn {}<K: {}>() {{}}\n",
                        assert_fn(*d),
                        key_bound(*d, cli)
                    ));
                }
                s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
                for (_dep, ident, scope_path, d) in &rows {
                    let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
                    s.push_str(&format!("    {}::<{scope_path}::{ty}>();\n", assert_fn(*d)));
                }
                s.push_str("}\n");
                s.push_str(
                    "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[\n",
                );
                for (dep, ident, _scope_path, d) in &rows {
                    let flavor = key_flavor_token(*d);
                    s.push_str(&format!("    ({dep:?}, {ident:?}, {flavor:?}),\n"));
                }
                s.push_str("];\n");
                s
            } else {
                let bound = if cli.preserve_encodings {
                    "Eq + Ord + PartialOrd + core::hash::Hash"
                } else {
                    "Eq + Ord + PartialOrd"
                };
                let mut s = String::from(
                    "// This file records every map-key type this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
                     // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
                     // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
                     // Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.\n",
                );
                s.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_key_traits<K: {bound}>() {{}}\n"
                ));
                if !rows.is_empty() {
                    s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
                    for (_dep, ident, scope_path, _) in &rows {
                        let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
                        s.push_str(&format!(
                            "    _assert_key_traits::<{scope_path}::{ty}>();\n"
                        ));
                    }
                    s.push_str("}\n");
                }
                s.push_str(
                    "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[\n",
                );
                for (dep, ident, _scope_path, _) in &rows {
                    s.push_str(&format!("    ({dep:?}, {ident:?}),\n"));
                }
                s.push_str("];\n");
                s
            };
            out.insert(
                "rust/src/generated/borrowed_key_types.rs".to_owned(),
                rustfmt_generated_string(&sidecar)?.into_owned(),
            );
        }

        // Key-demand assertions: for each `@used_as_key` root — flavored or bare — emit a named
        // `_demand_<rule>` fn that instantiates a bound-carrier over the tagged type. The Rust
        // compiler — the one component never wrong about trait supply — then converts a distant
        // downstream trait error (e.g. a tx-out struct's extern field lacking `Ord`) into a NEAR, named
        // error at THIS assertion, citing the tag; for demand that fails at a contained struct's own
        // derive, the file is the in-crate breadcrumb from the failing trait back to the causing tag.
        // A bare root asserts the mode-dependent internal bundle it demands (ord family; + hash under
        // --preserve-encodings), mirroring `key_trait_list`. Internal auto-detected keys emit nothing
        // (their containers' own bounds enforce them in-crate).
        let assertion_roots = assertion_roots(types);
        if !assertion_roots.is_empty() {
            // The families each root's demand resolves to in THIS mode (bare is mode-dependent).
            let hash_family = |d: &DemandSet| d.hash || (d.bare && cli.preserve_encodings);
            let ord_family = |d: &DemandSet| d.ord || d.bare;
            let mut file = String::from(
                "// Compile-time key-demand assertions for `@used_as_key` tags. Each\n\
                 // `_demand_<rule>` fn makes the Rust compiler prove the tagged type implements the\n\
                 // traits its tag demands, turning a distant downstream trait error into a near,\n\
                 // named one at the tagged type's definition site.\n",
            );
            if assertion_roots.iter().any(|(_, d)| hash_family(d)) {
                file.push_str(
                    "#[allow(dead_code)]\nfn _key_demand_hash<T: core::hash::Hash + Eq>() {}\n",
                );
            }
            if assertion_roots.iter().any(|(_, d)| ord_family(d)) {
                file.push_str("#[allow(dead_code)]\nfn _key_demand_ord<T: Ord>() {}\n");
            }
            for (ident, demand) in &assertion_roots {
                let scope = types.scope(ident);
                let path = if *scope == *ROOT_SCOPE {
                    format!("crate::generated::{ident}")
                } else {
                    format!(
                        "crate::generated::{}::{ident}",
                        scope.components().join("::")
                    )
                };
                // No per-fn comment: the fn name `_demand_<rule>` already names the tagged rule and
                // the banner explains the pattern. A comment here would strand into a
                // `cddl-codegen:unpreserved-comment` compile_error trap whenever the tag (hence the
                // fn) is deleted — the same preservation-overlay hazard the banner-only rule avoids.
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _demand_{}() {{\n",
                    convert_to_snake_case(ident.as_ref()),
                ));
                if hash_family(demand) {
                    file.push_str(&format!("    _key_demand_hash::<{path}>();\n"));
                }
                if ord_family(demand) {
                    file.push_str(&format!("    _key_demand_ord::<{path}>();\n"));
                }
                file.push_str("}\n");
            }
            out.insert(
                "rust/src/generated/key_demand_assertions.rs".to_owned(),
                rustfmt_generated_string(&file)?.into_owned(),
            );
        }

        // The extern-interface compiled self-check (`generated/extern_interface_check.rs`). Emitted
        // UNCONDITIONALLY in every mode, exactly like the extern-interface export it guards (not
        // wasm-gated, no suppress flag — a flag would just manufacture the stale-export state the
        // design prevents). It is DERIVED FROM THE SAME PROJECTION as that export
        // (`extern_interface_check_entries` shares `project_extern_interface` with the file emitter),
        // so the export and its self-check cannot drift. Each exported name is asserted here to be a
        // real, correctly-typed surface in THIS crate: opaque rows must implement `Serialize` (and
        // `Deserialize` where the dep generates one — the projection weakens the bound per type via
        // `deserialize_generated`), raw-bytes rows `RawBytesEncoding`, and transparent rows (aliases,
        // c-style enums, named collections) must simply exist. A hand-edited or stale export — or a
        // projection bug — therefore fails THIS crate's own build, naming the type.
        {
            use crate::generation::extern_interface::ExternCheckKind;
            let entries =
                crate::generation::extern_interface::extern_interface_check_entries(types, cli);
            let common = cli.common_import_rust();
            // The generated `Serialize` bound differs by mode: only the CANONICAL runtime
            // (`--preserve-encodings --canonical-form`) carries a custom `serialization::Serialize`
            // trait (its `serialize` takes a `force_canonical` flag); every other mode — including
            // preserve-without-canonical — serializes through `cbor_event::se::Serialize` directly.
            // `Deserialize` and `RawBytesEncoding` are the crate's own runtime traits in all modes.
            let serialize_bound = if cli.preserve_encodings && cli.canonical_form {
                format!("{common}::serialization::Serialize")
            } else {
                "cbor_event::se::Serialize".to_owned()
            };
            let path_of = |components: &[String], ident: &RustIdent| -> String {
                if components.is_empty() {
                    format!("crate::generated::{ident}")
                } else {
                    format!("crate::generated::{}::{ident}", components.join("::"))
                }
            };
            // Whole-value `Serialize`/`Deserialize` cover both the opaque `Serialize` rows AND the
            // transparent group-body `EmbeddedGroup` rows: a group-choice arm that splices a plain
            // group calls `.serialize()` on the whole value, so the whole-value bounds must hold for
            // an `EmbeddedGroup` row too (its `Deserialize` gated on the dep generating one, same as
            // `Serialize` rows).
            let deser_asserted =
                |entry: &crate::generation::extern_interface::ExternCheckEntry| -> bool {
                    matches!(
                        entry.kind,
                        ExternCheckKind::Serialize | ExternCheckKind::EmbeddedGroup
                    ) && self.deserialize_generated(&entry.ident)
                };
            let any_serialize = entries.iter().any(|e| {
                matches!(
                    e.kind,
                    ExternCheckKind::Serialize | ExternCheckKind::EmbeddedGroup
                )
            });
            let any_deser = entries.iter().any(deser_asserted);
            let any_raw_bytes = entries
                .iter()
                .any(|e| matches!(e.kind, ExternCheckKind::RawBytes));
            // `@copy` roots: every exported extern / raw-bytes ident declared `@copy`. The honesty
            // assertion proves the externally-defined rust type actually derives `Copy` in THIS
            // crate, so a false `@copy` fails the declaring crate's own build with a named error —
            // never a distant consumer's (a consumer imports the tag through a non-exported extern-dep
            // scope, which never reaches these entries).
            let any_copy = entries.iter().any(|e| types.is_copy_extern(&e.ident));
            // The embedded-group surface (`serialize_as_embedded_group` / `deserialize_as_embedded_group`)
            // a spliced record MEMBER delegates through, asserted only for group-body rows. Its
            // `Deserialize` twin is gated per-type on the dep generating one, exactly like the
            // whole-value side.
            let any_embedded_group = entries
                .iter()
                .any(|e| matches!(e.kind, ExternCheckKind::EmbeddedGroup));
            let any_embedded_group_deser = entries.iter().any(|e| {
                matches!(e.kind, ExternCheckKind::EmbeddedGroup)
                    && self.deserialize_generated(&e.ident)
            });

            let mut file = String::from(
                "// Compiled self-check for the dep-side extern-interface export\n\
                 // (`extern-interface/<dep>/**`). Machine-generated from the SAME projection as that\n\
                 // export, so the two cannot drift. Every exported name is asserted to be a real,\n\
                 // correctly-typed surface in THIS crate: opaque rows implement `Serialize` (and\n\
                 // `Deserialize` where the dep generates one), raw-bytes rows `RawBytesEncoding`, and\n\
                 // transparent rows (aliases, c-style enums, named collections) must simply exist. A\n\
                 // hand-edited or stale export — or a projection bug — therefore fails THIS crate's own\n\
                 // build, naming the type. Do not edit.\n\
                 // Rows carry NO per-row comments by design: a spec change can delete any row, and a\n\
                 // comment stranded on a deleted row is what the edit-preservation overlay turns into a\n\
                 // build-breaking sentinel on the next regen. All commentary lives in this fixed banner;\n\
                 // each row's type path is its own traceability.\n",
            );
            // Bound-carrier fns, emitted only for the kinds actually present so an absent trait (e.g.
            // `RawBytesEncoding` in a crate with no raw-bytes type) is never named.
            if any_serialize {
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_serialize<T: {serialize_bound}>() {{}}\n"
                ));
            }
            if any_deser {
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_deserialize<T: {common}::serialization::Deserialize>() {{}}\n"
                ));
            }
            if any_raw_bytes {
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_raw_bytes<T: {common}::serialization::RawBytesEncoding>() {{}}\n"
                ));
            }
            if any_copy {
                file.push_str("#[allow(dead_code)]\nfn _assert_copy<T: Copy>() {}\n");
            }
            // The embedded-group traits are the crate's own runtime traits in ALL modes (unlike
            // whole-value `Serialize`, whose custom canonical variant only exists in canonical mode).
            if any_embedded_group {
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_serialize_embedded_group<T: {common}::serialization::SerializeEmbeddedGroup>() {{}}\n"
                ));
            }
            if any_embedded_group_deser {
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_deserialize_embedded_group<T: {common}::serialization::DeserializeEmbeddedGroup>() {{}}\n"
                ));
            }
            // Transparent rows: a module-level `use … as _;` existence check (an anonymous import
            // never triggers unused-import warnings, but stay explicit).
            for entry in &entries {
                if matches!(entry.kind, ExternCheckKind::Use) {
                    file.push_str(&format!(
                        "#[allow(unused_imports)]\nuse {} as _;\n",
                        path_of(&entry.components, &entry.ident),
                    ));
                }
            }
            // Opaque / raw-bytes rows: bound-carrier instantiations inside a never-called fn.
            file.push_str("#[allow(dead_code)]\nfn _extern_interface_self_check() {\n");
            for entry in &entries {
                let path = path_of(&entry.components, &entry.ident);
                match entry.kind {
                    ExternCheckKind::Serialize => {
                        file.push_str(&format!("    _assert_serialize::<{path}>();\n"));
                        if self.deserialize_generated(&entry.ident) {
                            file.push_str(&format!("    _assert_deserialize::<{path}>();\n"));
                        }
                    }
                    ExternCheckKind::EmbeddedGroup => {
                        // Both surfaces the consumer's generated code uses for a spliced plain group:
                        // whole-value (a group-choice arm's `.serialize()`) and embedded (a record
                        // member's `serialize_as_embedded_group`), each `Deserialize` side gated on
                        // the dep generating one.
                        file.push_str(&format!("    _assert_serialize::<{path}>();\n"));
                        file.push_str(&format!(
                            "    _assert_serialize_embedded_group::<{path}>();\n"
                        ));
                        if self.deserialize_generated(&entry.ident) {
                            file.push_str(&format!("    _assert_deserialize::<{path}>();\n"));
                            file.push_str(&format!(
                                "    _assert_deserialize_embedded_group::<{path}>();\n"
                            ));
                        }
                    }
                    ExternCheckKind::RawBytes => {
                        file.push_str(&format!("    _assert_raw_bytes::<{path}>();\n"));
                    }
                    ExternCheckKind::Use | ExternCheckKind::None => {}
                }
                // `@copy` honesty assertion, orthogonal to the wire-surface kind above: a `@copy`
                // extern is a Serialize row, a `@copy` raw-bytes type a RawBytes row, and either must
                // actually be `Copy`.
                if types.is_copy_extern(&entry.ident) {
                    file.push_str(&format!("    _assert_copy::<{path}>();\n"));
                }
            }
            file.push_str("}\n");
            out.insert(
                "rust/src/generated/extern_interface_check.rs".to_owned(),
                rustfmt_generated_string(&file)?.into_owned(),
            );
        }

        // wasm crate
        if cli.wasm {
            // Same split as the rust crate: the tool-owned generated tree lives under
            // `wasm/src/generated/` (root scope + inner crate attrs in `mod.rs`), and the crate root
            // `wasm/src/lib.rs` is a seed-once thin root (added below) the tool never clobbers.
            Self::merge_scopes_to_strings(
                &mut out,
                "wasm/src/generated",
                self.wasm_lib_scope.clone(),
                &self.wasm_scopes,
                "mod.rs",
                "mod.rs",
            )?;
            // W2 (`--wrapper-requests`): the synthetic `requested_collections` scope has no
            // submodules, so materialize it as the flat `generated/requested_collections.rs` the
            // cross-crate contract names (its `pub mod requested_collections;` decl and the index's
            // `crate::generated::requested_collections::…` re-exports resolve to either layout). Every
            // other exported scope keeps its `<name>/mod.rs` form (it may nest submodules).
            if let Some(content) = out.remove("wasm/src/generated/requested_collections/mod.rs") {
                out.insert(
                    "wasm/src/generated/requested_collections.rs".to_owned(),
                    content,
                );
            }
            out.insert(
                "wasm/src/lib.rs".to_owned(),
                rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
            );

            // Collection-wrapper index: one `pub use crate::…::<Wrapper>;` per collection wrapper
            // CLASS this crate minted this run (recorded at each emitter's actual-mint point in
            // `wasm_collection_wrappers`). Because these are `pub use` lines compiled as part of
            // THIS crate, the index cannot drift: a line naming a removed wrapper fails this crate's
            // own build. A downstream crate points `--extern-wrapper-index <dep>=<this file>` at it
            // to skip re-minting the same wrappers (a wasm duplicate-symbol link error otherwise).
            // Emitted even when zero wrappers were minted (header comment only). The paths mirror
            // exactly how `merge_scopes_to_strings` lays the wasm generated tree out: ROOT_SCOPE
            // wrappers live in `generated/mod.rs` (`crate::generated::<Name>`); an exported
            // sub-scope's wrappers live in `generated/<scope>/mod.rs`
            // (`crate::generated::<scope>::<Name>`).
            let mut collections = String::from(
                "// Collection-wrapper index for this crate: one `pub use` re-export per collection\n\
                 // wrapper class defined here (list/map wrappers minted from `[* T]` / `{* K => V}`\n\
                 // shapes, including their NonEmpty variants). Compiled as part of this crate, so a\n\
                 // line naming a removed wrapper fails this crate's own build — the index cannot\n\
                 // drift. Downstream crates point `--extern-wrapper-index <dep>=<this file>` here to\n\
                 // avoid re-minting these wrappers (a wasm duplicate-symbol link error otherwise).\n",
            );
            for (ident, scope) in &self.wasm_collection_wrappers {
                let path = if *scope == *ROOT_SCOPE {
                    format!("crate::generated::{ident}")
                } else if scope.export() {
                    format!(
                        "crate::generated::{}::{ident}",
                        scope.components().join("::")
                    )
                } else {
                    // Non-exported (extern-dep) scopes are never written to a file by
                    // `merge_scopes_to_strings`, so a wrapper there is not part of THIS crate's
                    // output and must not appear in its index. Defensive — post-W1 no wrapper the
                    // crate mints lands in a non-exported scope.
                    continue;
                };
                collections.push_str(&format!("pub use {path};\n"));
            }
            out.insert(
                "wasm/src/generated/collections.rs".to_owned(),
                rustfmt_generated_string(&collections)?.into_owned(),
            );

            // Borrowed-collections sidecar (`--workspace-dep`): the mirror image of `collections.rs`
            // ("what I provide" ↔ "what I borrow, from whom"). Emitted whenever the flag is present —
            // INCLUDING when nothing is borrowed (stable presence, stable diffs) — and never
            // otherwise. Fixed format, ALL payload in code (no load-bearing comments the preservation
            // overlay could trap on): a private `#[allow(unused_imports)] mod borrowed` of plain `use`
            // lines (the compile-checked half — a wrapper a dep stops providing fails THIS crate's
            // build naming the type) and a `#[allow(dead_code)] pub(crate) const BORROWED_SHAPES`
            // table (the machine half the dep re-parses). Entries sorted by (dep, name); the `use`
            // paths go through the `--extern-wasm-crate` remap; the const's first column is the dep's
            // RUST crate name (the extern-deps directory name / `--extern-wasm-crate` left side), not
            // the wasm crate name.
            if !self.workspace_deps.is_empty() {
                let extern_wasm_crate_map = cli.extern_wasm_crate_map();
                let mut entries: Vec<(&str, &str, &str)> = self
                    .borrowed_wrappers
                    .iter()
                    .map(|(name, (dep, shape))| (dep.as_str(), name.as_ref(), shape.as_str()))
                    .collect();
                entries.sort_unstable();
                // The column legend lives in the banner (anchored to the file, which always exists),
                // NEVER inside the const body: an in-const comment is anchored to a row by the
                // preservation overlay, so deleting that row on an in-place regen (a consumer
                // dropping its last borrow of a shape) trapped the legend in a `compile_error!`
                // block — which the dep-side strict parser then (correctly) refused to consume.
                let mut sidecar = String::from(
                    "// This file records every collection wrapper this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--wrapper-requests) and compiled\n\
                     // here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.\n\
                     // Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).\n\
                     #[allow(unused_imports)]\n\
                     mod borrowed {\n",
                );
                for (dep, name, _) in &entries {
                    let dep_wasm = extern_wasm_crate_map
                        .get(*dep)
                        .map(String::as_str)
                        .unwrap_or(dep);
                    sidecar.push_str(&format!("    use {dep_wasm}::collections::{name};\n"));
                }
                sidecar.push_str(
                    "}\n\
                     #[allow(dead_code)]\n\
                     pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n",
                );
                for (dep, name, shape) in &entries {
                    sidecar.push_str(&format!("    ({dep:?}, {name:?}, {shape:?}),\n"));
                }
                sidecar.push_str("];\n");
                out.insert(
                    "wasm/src/generated/borrowed_collections.rs".to_owned(),
                    rustfmt_generated_string(&sidecar)?.into_owned(),
                );
            }

            out.insert(
                "wasm/Cargo.toml".to_owned(),
                crate::cargo_manifest::apply(
                    &crate::cargo_manifest::ops_for_wasm(cli)?,
                    None,
                    "wasm/Cargo.toml",
                )
                .map_err(std::io::Error::other)?,
            );
        }

        // json-gen crate for exporting JSON schemas
        if cli.json_schema_export {
            // The `None` here is the FRESH form only — same two-phase model as `rust/Cargo.toml`
            // above: `export` re-applies these ops onto the on-disk manifest so user edits survive.
            // Called out because this file reads as "clobbered" in isolation while a documented
            // promise rests on the opposite: `--json-schema-root` tells users that a dependency they
            // add here by hand (to reach another crate's type) survives regeneration.
            out.insert(
                "wasm/json-gen/Cargo.toml".to_owned(),
                crate::cargo_manifest::apply(
                    &crate::cargo_manifest::ops_for_json_gen(cli)?,
                    None,
                    "wasm/json-gen/Cargo.toml",
                )
                .map_err(std::io::Error::other)?,
            );

            // The json-gen crate writes ONE document per crate — `schemas/<lib>.schema.json`, a pure
            // `$defs` bundle — built by threading a single `schemars::SchemaGenerator` through every
            // exported type. One generator per document is what makes every referenced type a
            // DECLARED entry (a type reached only through another type's schema has no row of its
            // own, and under a per-type-file design ended up referenced-but-never-declared in the
            // shipped `.d.ts`), and what makes schemars' collision suffixes (`{base}{i}`, assigned
            // from a per-generator name set) assigned once from one deterministic row order.
            let lib_name_code = cli.lib_name_code();
            let mut lib_str = String::new();
            // Both helpers come from the common runtime crate's `json_schema_gen` module
            // (`static/json_schema_gen.rs`, composed in by `composed_runtime_static_files` under the
            // same flag), so a workspace of N json-gen crates carries ONE copy instead of N. The
            // prefix is the `--common-import-override` value verbatim when set, else this run's own
            // rust crate reached by package name through the json-gen crate's path dep — resolving
            // into `generated` via that crate's seed-once `pub use generated::*;` root.
            //
            // No banner comment above the two `use` lines: this file is inside the
            // comment-preservation overlay's tree, and a comment above a line that vanishes with a
            // flag is the stranded-comment/`unpreserved-comment` trap class.
            let common = cli.common_import_json_gen();
            // The closure check is imported UNCONDITIONALLY, unlike the row helper below: it belongs
            // to `export_schemas`, which is emitted for every `--json-schema-export` run, and a spec
            // that registers no rows writes a document too. (With no rows the walk finds no
            // references and the check is vacuous — the cost of that is nothing, and the alternative
            // is a check that can be silently absent.)
            lib_str.push_str(&format!(
                "use {common}::json_schema_gen::check_schema_ref_closure;\n"
            ));
            // The row registrar is imported only when this crate has rows of its own, never for
            // `--json-schema-dep`: a dep registrar call goes through the DEP's `add_schemas`, so a
            // crate whose `add_schemas` holds nothing but dep calls constructs no `Registrar` — and
            // an unused import would be a warning in generated code the consumer is told never to
            // hand-edit. `add_schema` itself is NOT imported: emitted rows reach the guard through
            // the registrar, and the helper stays public for hand-written rows only.
            if !self.json_lines.is_empty() {
                lib_str.push_str(&format!("use {common}::json_schema_gen::Registrar;\n"));
            }
            lib_str.push('\n');
            let mut lib_scope = codegen::Scope::new();
            // `add_schemas` is public on purpose: it is the composition point a consumer needs to
            // thread another generated crate's types into one document.
            let mut lib_add_fn = codegen::Function::new("add_schemas");
            lib_add_fn
                .vis("pub")
                .arg("generator", "&mut schemars::SchemaGenerator");
            let json_schema_deps = cli.json_schema_deps();
            if self.json_lines.is_empty() && json_schema_deps.is_empty() {
                // A spec whose every rule is skipped (array/table typedefs only, say) registers
                // nothing, and the parameter would then be an unused-variable warning in generated
                // code the consumer is told never to hand-edit. An unused `reg` local would be a
                // NEW warning of the same class, so the registrar is emitted only when there are
                // rows. A `--json-schema-dep` call USES `generator`, so it suppresses the attribute
                // on its own: an `allow` over a used parameter is inert snapshot noise.
                lib_add_fn.attr("allow(unused_variables)");
            }
            // `--json-schema-dep` registrar calls, FIRST — before the `Registrar` local and before
            // every spec-derived row and every `--json-schema-root` row. Read straight off `cli`: the
            // dep list is a flag, not IR, so it never travels through `json_lines`.
            //
            // FIRST is also what makes this compile at all now that the ledger lives in a registrar:
            // `Registrar::new` takes the generator's `&mut` borrow for the rest of the body, and each
            // dep call passes that same borrow on to the dependency's `add_schemas`.
            //
            // FIRST is the deliberate mirror of why `--json-schema-root` rows come LAST. A dep's
            // published names are already shipped in the dep's own package, so on a cross-crate name
            // collision the CONSUMER's row is the one that should be renamed and blamed — the one its
            // owner can change. A consequence worth stating: with deps registered first, a
            // cross-crate collision whose `schema_id`s DIFFER is caught by the emitted helper's
            // kept-its-own-name check (B), because `subschema_for` then hands the consumer's row
            // `<name>2`. That is reasoned from the mechanism `integration_tests::
            // json_schema_name_stolen_fails` exercises WITHIN one crate — cross-crate it is not run.
            //
            // FLAG ORDER, never sorted, for the same reason the `--json-schema-root` block gives: the
            // flag list is an input, so preserving it keeps "same inputs -> same bytes" while staying
            // readable; sorting would reorder registration, which is observable through the guard's
            // messages.
            //
            // No banner comment above the block, also for the `--json-schema-root` block's reason:
            // this file is inside the comment-preservation overlay's tree, and a comment above lines
            // that all vanish when the flag is dropped is the stranded-comment/`unpreserved-comment`
            // trap class.
            for (_label, lib) in &json_schema_deps {
                lib_add_fn.line(format!("{lib}::add_schemas(generator);"));
            }
            if !self.json_lines.is_empty() {
                lib_add_fn.line(REGISTRAR_DECL);
            }
            lib_add_fn.push_all(self.json_lines.clone());
            lib_scope.push_fn(lib_add_fn);
            let mut lib_export_fn = codegen::Function::new("export_schemas");
            lib_export_fn
                .vis("pub")
                .line("let schema_path = std::path::Path::new(\"schemas\");");
            let mut path_exists = Block::new("if !schema_path.exists()");
            path_exists.line("std::fs::create_dir(schema_path).unwrap();");
            lib_export_fn
                .push_block(path_exists)
                .line("let mut generator = schemars::SchemaGenerator::default();")
                .line("add_schemas(&mut generator);")
                // The meta-schema is read off the generator's own settings rather than hardcoded, so
                // the document always declares the draft schemars actually emitted.
                .line("let meta_schema = generator.settings().meta_schema.clone();")
                // Captured alongside the meta-schema, and for the same reason: the closure check
                // below compares against the namespace schemars ACTUALLY used, never a hardcoded
                // `#/$defs/`.
                .line("let definitions_path = generator.settings().definitions_path.to_string();")
                .line("let mut document = serde_json::Map::new();");
            let mut meta_present = Block::new("if let Some(meta_schema) = meta_schema");
            meta_present
                .line("document.insert(\"$schema\".to_owned(), meta_schema.into_owned().into());");
            lib_export_fn
                .push_block(meta_present)
                .line(format!(
                    "document.insert(\"title\".to_owned(), \"{lib_name_code}\".into());"
                ))
                // `take_definitions(true)` applies the generator's transforms, matching what
                // schemars' own root-schema builders do.
                .line(
                    "document.insert(\"$defs\".to_owned(), generator.take_definitions(true).into());",
                )
                .line("let document = serde_json::Value::Object(document);")
                // After `$defs` is materialised and BEFORE anything is written: a document that
                // cannot resolve its own references must never reach disk, since every cheap
                // downstream verdict ("it generated", "it compiled", "the `.d.ts` type-checks")
                // is satisfied by one.
                .line("check_schema_ref_closure(&document, &definitions_path);")
                .line(format!(
                    "std::fs::write(schema_path.join(\"{lib_name_code}.schema.json\"), serde_json::to_string_pretty(&document).unwrap()).unwrap();"
                ));
            lib_scope.push_fn(lib_export_fn);
            lib_str.push_str(&lib_scope.to_string());
            // Same split as the other crate roots: the generated `add_schemas` + `export_schemas`
            // live under `wasm/json-gen/src/generated/mod.rs`, exposed through the seed-once thin
            // root's glob re-export (so `<lib>_json_schema_gen::export_schemas()` in main.rs still
            // resolves). `main.rs` stays fully tool-owned and unchanged.
            out.insert(
                "wasm/json-gen/src/generated/mod.rs".to_owned(),
                rustfmt_generated_string(&lib_str)?.into_owned(),
            );
            out.insert(
                "wasm/json-gen/src/lib.rs".to_owned(),
                rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
            );

            let mut main_scope = codegen::Scope::new();
            main_scope.new_fn("main").line(format!(
                "{}_json_schema_gen::export_schemas();",
                cli.lib_name_code()
            ));
            out.insert(
                "wasm/json-gen/src/main.rs".to_owned(),
                rustfmt_generated_string(&main_scope.to_string())?.into_owned(),
            );
        }

        // Stamp the codegen header once per emitted file, for the tool-owned generated trees only.
        // File-level (not scope-level) stamping guarantees the banner leads even in merged root
        // files, where the module-linking declarations from the lib scope would otherwise precede a
        // scope-level header raw. `export` restamps the one file it rebuilds after us (the root
        // serialization.rs, which it re-merges with the static prelude).
        for (path, content) in out.iter_mut() {
            if is_header_stamped_path(path) {
                *content = stamp_codegen_header(content);
            }
        }

        // Duplicate-ident backstop: no top-level type-namespace ident (struct/enum/type) may be
        // defined twice within a single generated file. Silent redefinitions arise when a user rule
        // name collides with a generator-synthesized structural ident (list/map wrapper families) —
        // exit-0 today, E0428 in the output crate. Observing the ACTUAL emitted source (not an IR
        // prediction) makes this the backstop for every mint path, present and future. Scoped to the
        // tool-owned `src/generated/**` trees (all three crates); static/template-sourced files are
        // excluded. `out` is a BTreeMap (sorted keys) and per-file idents are collected into a
        // BTreeMap, so the first offending file and its listed idents are deterministic. On a hit
        // this returns an `Err` at the seam (surfaced as `error (graceful)` by the catalogs), never
        // a panic.
        for (path, content) in out.iter() {
            if !path.contains("src/generated/") {
                continue;
            }
            let mut seen: BTreeMap<&str, usize> = BTreeMap::new();
            for line in content.lines() {
                if let Some(ident) = top_level_type_ident(line) {
                    *seen.entry(ident).or_insert(0) += 1;
                }
            }
            let dups: Vec<&str> = seen
                .iter()
                .filter(|&(_, &count)| count > 1)
                .map(|(ident, _)| *ident)
                .collect();
            if !dups.is_empty() {
                let names = dups
                    .iter()
                    .map(|d| format!("'{d}'"))
                    .collect::<Vec<_>>()
                    .join(", ");
                return Err(std::io::Error::other(format!(
                    "duplicate top-level ident{} {names} in {path}: a rule name collides with a \
                     generator-synthesized ident (list/map wrapper families) — rename the rule; if \
                     no user rule is involved this is a cddl-codegen bug",
                    if dups.len() == 1 { "" } else { "s" },
                )));
            }
        }

        // Usage-derived import prune: drop the blindly-pushed collection-type imports, unused
        // `super::*` / `error::*` globs, and cross-scope type imports that a file's module family
        // references nowhere. Runs here, over the WHOLE file map, rather than per-file in
        // `rustfmt_generated_string`, because soundness needs each file's descendant modules in
        // view: a child's `use super::*;` chain can consume the parent's private imports, so a
        // file's import is genuinely unused only when neither the file nor any super-reachable
        // descendant module names the ident (see `import_prune.rs`). The pass returns the changed
        // files' pruned (not-yet-rustfmt'd) content; rustfmt normalizes the splice here. This is
        // still BEFORE the comment-preservation overlay (which runs at `export` write time), so fresh
        // content stays a rustfmt-stable fixed point run-over-run.
        //
        // Beyond the built-in `ALLOWLIST`, the name-scan candidate set carries: the wasm prelude
        // names (`JsError`/`JsValue` concrete types, `wasm_bindgen` attribute macro), the configured
        // `--wasm-*-macro` leaf names (each exercised only via `name!(…)`), and every cross-scope
        // generated type ident `scope_references` over-imported. The configured wasm macros MUST be
        // written fully-qualified (they must not assume `JsError`/`JsValue`/`wasm_bindgen` in scope) —
        // the contract documented next to those flags in `command_line_flags.mdx`.
        let prune_config = self.build_prune_config(cli);
        for (path, pruned) in crate::import_prune::prune_generated_files(&out, &prune_config) {
            let formatted = rustfmt_generated_string(&pruned)?.into_owned();
            out.insert(path, formatted);
        }

        Ok(out)
    }

    /// The name-scan candidate set the usage-derived import prune may remove beyond the built-in
    /// `ALLOWLIST` (see `import_prune::PruneConfig`): the wasm prelude names (`JsError`/`JsValue`
    /// concrete types, the `wasm_bindgen` attribute macro), the `--wasm-*-macro` leaf names (each
    /// exercised only via `name!(…)`), and every cross-scope generated type ident
    /// `scope_references` over-imported. Every entry is a concrete type or a macro — never a trait —
    /// so name-scanning stays sound. Shared by `generated_files` and the post-rebuild re-prune in
    /// `export` so the two can't diverge.
    fn build_prune_config(&self, cli: &Cli) -> crate::import_prune::PruneConfig {
        let mut prune_config = crate::import_prune::PruneConfig::default();
        prune_config.extra_candidates.extend(
            ["JsError", "JsValue", "wasm_bindgen"]
                .into_iter()
                .map(String::from),
        );
        for macro_flag in [
            &cli.wasm_cbor_json_api_macro,
            &cli.wasm_conversions_macro,
            &cli.wasm_list_macro,
        ]
        .into_iter()
        .flatten()
        {
            if let Some((_, leaf)) = macro_flag.rsplit_once("::") {
                prune_config.extra_candidates.insert(leaf.to_owned());
            }
        }
        prune_config
            .extra_candidates
            .extend(self.scope_ref_import_idents.iter().cloned());
        prune_config
    }
}

#[cfg(test)]
mod extern_reexport_diagnostic_tests {
    use super::{contains_whole_ident, has_foreign_glob, missing_reexports};
    use std::collections::BTreeSet;

    fn set(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| (*s).to_owned()).collect()
    }

    #[test]
    fn whole_ident_match_is_not_a_substring_match() {
        // A required `Foo` provided verbatim is present; `Foo` as a substring of `FooBar` is NOT.
        assert!(contains_whole_ident("pub use utils::Foo;", "Foo"));
        assert!(!contains_whole_ident("pub use utils::FooBar;", "Foo"));
        assert!(!contains_whole_ident("pub use utils::BarFoo;", "Foo"));
        assert!(contains_whole_ident("pub use a::Foo as Bar;", "Foo"));
        assert!(!contains_whole_ident("", "Foo"));
    }

    #[test]
    fn seeded_root_glob_is_not_foreign() {
        // The tool's own seed (`pub use generated::*;`) must NOT be treated as a foreign glob —
        // otherwise the diagnostic would be suppressed on EVERY seeded root and never fire.
        assert!(!has_foreign_glob("mod generated;\npub use generated::*;\n"));
        assert!(!has_foreign_glob("pub use crate::generated::*;\n"));
    }

    #[test]
    fn user_glob_is_foreign() {
        assert!(has_foreign_glob("pub mod utils;\npub use utils::*;\n"));
        assert!(has_foreign_glob("pub use crate::hand::exports::*;\n"));
        // A `use` inside some other construct's text is not a top-level use item.
        assert!(!has_foreign_glob("let x = \"use foo::*;\";\n"));
    }

    #[test]
    fn missing_reexports_reports_only_absent_names() {
        let required = set(&["MyExt", "PubKeyRawBytes"]);
        // Seeded root re-exports one of the two; the other is missing.
        let root = "mod generated;\npub use generated::*;\npub use utils::MyExt;\n";
        assert_eq!(missing_reexports(root, &required), vec!["PubKeyRawBytes"]);
        // Both present -> nothing missing.
        let full = format!("{root}pub use utils::PubKeyRawBytes;\n");
        assert!(missing_reexports(&full, &required).is_empty());
    }

    #[test]
    fn foreign_glob_suppresses_all_missing() {
        let required = set(&["MyExt", "Other"]);
        // A non-`generated` glob the scan can't see through: suppress, even though neither name
        // appears verbatim.
        let root = "mod generated;\npub use generated::*;\npub use utils::*;\n";
        assert!(missing_reexports(root, &required).is_empty());
    }
}
