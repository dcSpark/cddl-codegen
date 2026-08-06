//! Library entry points for driving the generator in-process.
//!
//! Both the binary and the tests run the full pipeline (CDDL text -> AST -> `IntermediateTypes`
//! -> generated code) through here, without shelling out to `cargo run` or writing to disk.
//!
//! `IntermediateTypes<'a>` borrows from the parsed CDDL AST, so it cannot be returned from a
//! function that parses internally (the borrow would escape). [`with_types`] is therefore a
//! scoped/callback API: it owns the AST for the duration of the call and hands the caller a
//! borrow, returning only owned data.

use crate::cli::Cli;
use crate::comment_ast::RuleMetadata;
use crate::dep_graph;
use crate::generation::GenerationScope;
use crate::intermediate::{CDDLIdent, IntermediateTypes, PlainGroupInfo, ROOT_SCOPE, RustIdent};
use crate::parsing::{self, parse_rule, rule_ident, rule_is_scope_marker};

fn cddl_paths(
    output: &mut Vec<std::path::PathBuf>,
    cd: &std::path::PathBuf,
) -> std::io::Result<()> {
    // read_dir order is filesystem-dependent, and file order decides the rule order fed to the
    // topological sort (and thus naming/emission tie-breaks) — sort so the same spec directory
    // generates byte-identical output on every machine (the reproducibility invariant).
    let mut paths: Vec<std::path::PathBuf> = std::fs::read_dir(cd)?
        .map(|dir_entry| dir_entry.map(|e| e.path()))
        .collect::<std::io::Result<_>>()?;
    paths.sort();
    for path in paths {
        if path.is_dir() {
            cddl_paths(output, &path)?;
        } else if path.extension().is_some_and(|ext| ext == "cddl") {
            output.push(path);
        } else {
            // extensionless files (README, LICENSE, dotfiles) land here too instead of panicking
            crate::info!("Skipping file: {}", path.as_path().to_str().unwrap());
        }
    }
    Ok(())
}

/// Recognize CDDL-module preprocessor directives (`draft-ietf-cbor-cddl-modules-06`, Appendix A)
/// in a single raw input file BEFORE it is concatenated with the others, so a diagnostic can name
/// the offending file and line.
///
/// Per the draft's ABNF a directive is `";#" RS (%s"import" / %s"include") RS …`, where `RS` is
/// one-or-more spaces (`WS`/`SP` = `%x20` only). We are not a module-aware tool: adopting the
/// directive's real inlining semantics is out of scope. But silently ignoring them is worse than
/// aborting — the concatenated body then references rules the directive was supposed to pull in,
/// yielding a misleading "undefined reference" parse error, or (for `include`-style composition
/// whose rules aren't referenced) silently-incomplete output. So:
///   - `;#` + space(s) + `import`/`include` → hard error naming the file, line and directive.
///   - `;#` + space(s) + anything else → stderr warning (an unrecognized `;# `-form, not fatal).
///   - `;#` NOT followed by a space (e.g. `;#####` banner comments) → legal basic CDDL, silent.
///
/// A `;#`-prefixed line inside a (hypothetical) multi-line CDDL text literal would be mis-scanned
/// as a directive — accepted collateral: the scan is line-oriented and pre-parse, and such a
/// literal spanning a directive-looking line is pathological.
fn scan_module_directives(
    input_file: &std::path::Path,
    content: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    for (idx, line) in content.lines().enumerate() {
        let line_no = idx + 1;
        // Directives may be indented; a `;#` reached only past leading whitespace is still one.
        let Some(after_hash) = line.trim_start().strip_prefix(";#") else {
            continue;
        };
        // `RS` is one-or-more ASCII spaces. No space after `;#` ⇒ a plain `;#…` comment (banner).
        if !after_hash.starts_with(' ') {
            continue;
        }
        let first_word = after_hash.split_whitespace().next().unwrap_or("");
        if first_word == "import" || first_word == "include" {
            return Err(format!(
                "CDDL module directive `;# {first_word} …` at {}:{line_no} — CDDL module directives \
                 (draft-ietf-cbor-cddl-modules) are not supported by cddl-codegen. They cannot be \
                 ignored: doing so would make the concatenated spec reference rules the directive \
                 was meant to pull in (a misleading \"undefined reference\" error) or, for \
                 unreferenced `include`d rules, silently emit incomplete output. Inline the required \
                 rules directly, or resolve the modules with `cddlc` before feeding cddl-codegen.",
                input_file.display()
            )
            .into());
        } else {
            crate::warn!(
                "warning: unrecognized `;# …` directive-shaped comment at {}:{line_no} — \
                 cddl-codegen does not process CDDL module directives; treating it as a comment.",
                input_file.display()
            );
        }
    }
    Ok(())
}

/// Read every `--extern-import <dep>=<path>` export and append the rules `content` NEEDS to it, with
/// EXTERN_DEPS_DIR scope markers (a SEPARATE assembly loop from the main input's — see the call
/// site). `marker_start` is the first free scope-marker index (the main loop used
/// `0..input_files.len()`), so imported markers get distinct indices (a duplicate rule ident would be
/// a parse error). Returns whether any imported file — narrowed or not — carried the raw-bytes
/// marker (see the call site for why that is read off the WHOLE export).
///
/// Only the needed closure of each export is concatenated (`extern_narrow`): the rules `content`
/// references and does not define, plus what those transitively reference through their export
/// bodies. Every file is still read and seam-scanned in full, because the seam's guarantees are
/// per-file — an unused rule carrying a bad annotation must still fail the seam.
///
/// Hard errors (each naming the flag value): a `<dep>` also declared as a physical
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/` input directory (ambiguous double declaration, never a
/// merge); a path that does not exist or contains no `.cddl` files; a flag-fed file missing the
/// versioned seam header, carrying an unknown `@`-annotation (the strict seam — physical stubs stay
/// lenient because they are not routed here), or failing to parse standalone; and the two narrowing
/// errors (a needed rule the consumer also defines, a name needed from two exports).
fn append_extern_imports(
    cli: &Cli,
    extern_imports: &std::collections::BTreeMap<String, String>,
    marker_start: usize,
    content: &mut String,
) -> Result<bool, Box<dyn std::error::Error>> {
    /// One export file, kept from the read/scan pass so the selection pass can slice it.
    struct ImportedFile {
        scope: String,
        raw: String,
        parsed: crate::extern_narrow::ExportFile,
    }
    let mut files_by_dep: std::collections::BTreeMap<String, Vec<ImportedFile>> =
        std::collections::BTreeMap::new();
    let mut raw_bytes_marker_seen = false;
    for (dep, import_path) in extern_imports {
        // Double declaration: a physical extern-deps dir AND the flag is ambiguous — refuse both.
        if cli.input.is_dir() {
            let physical = cli.input.join(parsing::EXTERN_DEPS_DIR).join(dep);
            if physical.is_dir() {
                return Err(format!(
                    "--extern-import {dep}={import_path} conflicts with a physical \
                     {}/{dep}/ directory in the input tree: a dependency is declared exactly once — \
                     either via --extern-import or as an in-tree stub, never both (ambiguous double \
                     declaration, never a merge). Remove one.",
                    parsing::EXTERN_DEPS_DIR
                )
                .into());
            }
        }
        let import_root = std::path::PathBuf::from(import_path);
        if !import_root.exists() {
            return Err(format!(
                "--extern-import {dep}={import_path} — the path does not exist. Point it at the \
                 dependency's committed extern-interface/{dep}/ export tree."
            )
            .into());
        }
        let mut imported = Vec::new();
        if import_root.is_dir() {
            cddl_paths(&mut imported, &import_root)?;
        } else if import_root.extension().is_some_and(|ext| ext == "cddl") {
            imported.push(import_root.clone());
        }
        if imported.is_empty() {
            return Err(format!(
                "--extern-import {dep}={import_path} — no .cddl files found under the path. Point it \
                 at the dependency's committed extern-interface/{dep}/ export tree."
            )
            .into());
        }
        let mut staged = Vec::new();
        for import_file in &imported {
            let raw = std::fs::read_to_string(import_file)?;
            // The general per-file directive guard applies to imported files too (an export never
            // carries `;#` directives, but the check is cheap and keeps the invariant uniform).
            scan_module_directives(import_file, &raw)?;
            // The strict extern-interface seam: header + `@`-token whitelist (flag-fed files ONLY).
            scan_extern_import_seam(import_file, &raw)?;
            raw_bytes_marker_seen |= raw.contains(parsing::RAW_BYTES_MARKER);
            let parsed = crate::extern_narrow::parse_export_file(&raw).map_err(|e| {
                format!(
                    "extern-interface file {} does not parse on its own: {e}. Export files are \
                     self-contained by construction, so this is a defect in the dependency's export \
                     rather than something to route around — regenerate the dependency, and report \
                     the file if the regenerated export still fails.",
                    import_file.display()
                )
            })?;
            staged.push(ImportedFile {
                scope: extern_import_scope(dep, &import_root, import_file),
                raw,
                parsed,
            });
        }
        files_by_dep.insert(dep.clone(), staged);
    }

    // The selection. A consumer whose own content does not parse selects EVERYTHING: the pipeline's
    // checked parse is a few lines away and will report the real error, so falling back here keeps
    // that diagnostic exactly as it is instead of replacing it with a narrowing artifact.
    let surface = crate::extern_narrow::scan_consumer(content);
    let selected = match &surface {
        Some(surface) => {
            let index = files_by_dep
                .iter()
                .map(|(dep, files)| {
                    let rules = files
                        .iter()
                        .flat_map(|f| f.parsed.rules.iter())
                        .map(|r| (r.name.clone(), r.refs.clone()))
                        .collect();
                    (dep.clone(), rules)
                })
                .collect();
            crate::extern_narrow::needed_closure(surface, &index)?
        }
        None => files_by_dep
            .iter()
            .map(|(dep, files)| {
                let all = files
                    .iter()
                    .flat_map(|f| f.parsed.names())
                    .map(str::to_owned)
                    .collect();
                (dep.clone(), all)
            })
            .collect(),
    };

    let mut marker_index = marker_start;
    for (dep, files) in &files_by_dep {
        let names = &selected[dep];
        if names.is_empty() {
            crate::warn!(
                "{}",
                crate::extern_narrow::unused_dependency_note(dep, &extern_imports[dep])
            );
        }
        for file in files {
            let picked = file
                .parsed
                .rules
                .iter()
                .filter(|rule| names.contains(&rule.name))
                .collect::<Vec<_>>();
            // A file contributing no rule contributes no marker either — an empty scope switch is
            // the only thing it could add.
            if picked.is_empty() {
                continue;
            }
            // The header and the `; unexported:` records ride along verbatim, so a file whose every
            // rule is needed re-emits its own bytes exactly.
            let mut body = file.raw[..file.parsed.prefix_end].to_owned();
            for rule in picked {
                body.push_str(&file.raw[rule.span.0..rule.span.1]);
            }
            content.push_str(&format!(
                "\n{}{} = \"{}\"\n{}\n",
                parsing::SCOPE_MARKER,
                marker_index,
                file.scope,
                body
            ));
            marker_index += 1;
        }
    }
    Ok(raw_bytes_marker_seen)
}

/// The EXTERN_DEPS_DIR scope-marker string for a file pulled in via `--extern-import`, in the marker
/// channel's `::`-joined form: `_CDDL_CODEGEN_EXTERN_DEPS_DIR_::<dep>::<subpath>`. Subpath components
/// apply the established pathdiff conventions (strip the `.cddl` extension, drop a trailing `mod`
/// stem), so the export tree (`extern-interface/<dep>/sub/module/mod.cddl`) lands in scope
/// `<dep>::sub::module` — byte-identical to a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/sub/module.cddl`
/// stub. A single-file import maps to the bare `<dep>` root scope.
fn extern_import_scope(
    dep: &str,
    import_root: &std::path::Path,
    import_file: &std::path::Path,
) -> String {
    use std::path::Component;
    let mut components = vec![parsing::EXTERN_DEPS_DIR.to_string(), dep.to_string()];
    if import_root.is_dir()
        && let Some(relative) = pathdiff::diff_paths(import_file, import_root)
    {
        let mut sub = relative
            .components()
            .filter_map(|p| match p {
                Component::Normal(part) => Some(
                    std::path::Path::new(part)
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_owned(),
                ),
                _ => None,
            })
            .collect::<Vec<_>>();
        if let Some(c) = sub.last()
            && *c == "mod"
        {
            sub.pop();
        }
        components.extend(sub);
    }
    components.join("::")
}

/// The strict extern-interface seam check for a file fed via `--extern-import` (a machine-generated
/// dependency export). Two guards: the first line must be the versioned seam header, and every
/// `@`-token in the file must be a recognized comment-DSL annotation. A missing/unknown header, or an
/// unknown `@`-token, is a hard error naming the file (and the offending token). Physical hand-stub
/// files keep today's lenient behavior — they are never routed here.
fn scan_extern_import_seam(
    path: &std::path::Path,
    content: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    use crate::generation::extern_interface::{
        EXTERN_INTERFACE_HEADER, EXTERN_INTERFACE_HEADER_PREFIX, EXTERN_INTERFACE_HEADER_V2,
    };
    let first = content.lines().next().unwrap_or("").trim_end();
    // Both v1 and v2 are accepted: v2 is the conditional bump for `any`-bearing exports, and this
    // reader understands the `any` spelling. A reader predating `any` support accepts only v1,
    // so an `any`-bearing v2 export fails loudly at ITS seam with the version diagnostic below.
    if first != EXTERN_INTERFACE_HEADER && first != EXTERN_INTERFACE_HEADER_V2 {
        let msg = if first
            .trim_start()
            .starts_with(EXTERN_INTERFACE_HEADER_PREFIX)
        {
            format!(
                "extern-interface file {} carries an unsupported version header {first:?}; this \
                 cddl-codegen understands `{EXTERN_INTERFACE_HEADER}` and \
                 `{EXTERN_INTERFACE_HEADER_V2}`. Regenerate the \
                 dependency with a compatible cddl-codegen — the extern-interface seam is versioned.",
                path.display()
            )
        } else {
            format!(
                "extern-interface file {} is missing the required seam header \
                 `{EXTERN_INTERFACE_HEADER}` on its first line. --extern-import only accepts \
                 machine-generated exports carrying the versioned seam; point the flag at a committed \
                 extern-interface/<dep>/ tree, or hand-stub the dependency under {}/<dep>/ instead \
                 (physical stubs are parsed leniently).",
                path.display(),
                parsing::EXTERN_DEPS_DIR
            )
        };
        return Err(msg.into());
    }
    if let Some(bad) = first_unknown_annotation_token(content) {
        return Err(format!(
            "extern-interface file {} contains an unknown annotation token `{bad}` outside the \
             recognized comment-DSL set — the strict extern-interface seam rejects it (a typo, or a \
             newer dialect this cddl-codegen does not understand). Regenerate the dependency with a \
             compatible cddl-codegen.",
            path.display()
        )
        .into());
    }
    Ok(())
}

/// The first `@`-token in `content` that no known rule-metadata DSL tag is a PREFIX of (the strict
/// extern-interface whitelist, matching comment_ast's prefix semantics — `@namefoo` credits `@name`).
/// An `@`-token runs from `@` to the next whitespace or `@`, so a buried `@doc@import` splits and the
/// unknown `@import` is still caught. `None` = every `@`-token is recognized. Reason text in
/// `; unexported:` records only ever contains whitelisted tokens (`@custom_serialize` /
/// `@custom_deserialize`), so a records-carrying export passes.
fn first_unknown_annotation_token(content: &str) -> Option<String> {
    let known = crate::comment_ast::KNOWN_RULE_METADATA_TAGS;
    let mut chars = content.chars().peekable();
    while let Some(&c) = chars.peek() {
        if c != '@' {
            chars.next();
            continue;
        }
        let mut token = String::from('@');
        chars.next();
        while let Some(&next) = chars.peek() {
            if next.is_whitespace() || next == '@' {
                break;
            }
            token.push(next);
            chars.next();
        }
        if !known.iter().any(|tag| token.starts_with(tag)) {
            return Some(token);
        }
    }
    None
}

/// Augment (never swallow) a checked-parse failure with the `--extern-import` staleness hint: the
/// declared dep list, their export paths, and what to do when a referenced ident is undefined. The
/// original parse error stays at the head so its "undefined reference" detail is preserved.
///
/// Every remedy named is on the DEPENDENCY's side, because a per-rule hand stub is not reachable for
/// a dependency this crate imports: a dependency is declared exactly once, so a physical
/// `EXTERN_DEPS_DIR/<dep>/` beside the import is the double-declaration error, and one under a
/// different directory name resolves the rules to a different crate — the directory name IS the
/// crate. The whole-dep stub is still the escape hatch, but taking it means dropping this
/// dependency's `--extern-import` rather than supplementing it.
fn extern_import_staleness_error(
    parse_error: String,
    extern_imports: &std::collections::BTreeMap<String, String>,
) -> String {
    let deps = extern_imports
        .keys()
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    let paths = extern_imports
        .iter()
        .map(|(dep, path)| format!("{dep}={path}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "{parse_error}\n\nnote: --extern-import is in use (declared dependencies: {deps}). If a \
         referenced identifier is undefined above, either the export predates the dependency's \
         current spec, or the rule is recorded as `; unexported:` in it — one the dependency could \
         not project. Both remedies are on the DEPENDENCY's side. Regenerate the dependency so its \
         extern-interface export is fresh; if the ident is `; unexported:`, fix the cause in the \
         dependency's own spec (a type it hand-owns travels once the dependency itself declares it \
         as _CDDL_CODEGEN_EXTERN_TYPE_) or report the projection limitation. Hand-stubbing the one \
         missing rule is not available for a dependency you import: a dependency is declared exactly \
         once, and a stub under any other directory name resolves the rules to a different crate. \
         The whole-dep stub remains the escape hatch for a dependency you cannot regenerate at all, \
         and taking it means dropping its --extern-import rather than supplementing it. Export \
         paths: {paths}."
    )
}

/// Every rule that rejects a COMBINATION of flags, as a pure function of the `Cli`.
///
/// Extracted from [`with_types`] — which still calls it, in the same position, so a single-crate
/// command line behaves byte for byte as it did — because the multi-crate `--config` front end needs
/// to run these rules for EVERY crate before ANY crate generates. Inside the generation loop, a
/// shared key that trips one of them leaves the crates before it fully regenerated on disk and
/// reports a bare flag message naming neither the crate nor the config key that produced it.
///
/// Every rule here reads `cli` alone. That is the property that makes the hoist possible at all, and
/// it is a constraint on what may be added: a rule needing the parsed spec (`--workspace-dep`'s
/// "names a configured extern dependency", say) cannot live here, because there is no spec to
/// consult before generation starts.
/// Whether a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/` stub tree in the input declares this
/// dependency — the alternative to `--extern-import` for a dep that has no export to consume.
///
/// A filesystem existence check, and the one in [`validate_flag_combinations`] that reads anything
/// but `cli`: the two ways to declare a dependency are a flag and a directory, so a rule that
/// consulted only the flag would refuse a legitimate stub-declared dep.
fn stub_dir_declares(cli: &Cli, dep: &str) -> bool {
    cli.input.is_dir() && cli.input.join(parsing::EXTERN_DEPS_DIR).join(dep).is_dir()
}

pub fn validate_flag_combinations(cli: &Cli) -> Result<(), String> {
    // The canonical toggle is emitted as an extra argument on the preserve-encodings `serialize`
    // signatures; without --preserve-encodings those signatures don't take it, so the generator
    // emits `serialize(serializer, force_canonical)` calls against 1-arg methods and references an
    // unbound `force_canonical` — a crate that does not compile. Reject the combination up front
    // rather than emit broken output (the docs likewise require the two together).
    if cli.canonical_form && !cli.preserve_encodings {
        return Err(
            "--canonical-form=true requires --preserve-encodings=true: the canonical toggle rides \
             on the preserve-encodings serialize signatures, so on its own the generated crate does \
             not compile"
                .to_owned(),
        );
    }
    // The conformance oracle is a per-round-trip-case add-on to the --emit-tests module; without
    // --emit-tests there is no module to add the validation calls to. Reject the combination up
    // front rather than silently emit nothing (mirrors the canonical/preserve rule above).
    if cli.emit_tests_conformance && !cli.emit_tests {
        return Err(
            "--emit-tests-conformance=true requires --emit-tests=true: the conformance oracle adds \
             a validation call to each emitted round-trip case, so there is nothing to add without \
             the generated-test module"
                .to_owned(),
        );
    }
    // The copied scripts compile the schema document the `--json-schema-export` json-gen crate
    // writes; without that flag nothing ever writes a `schemas/` dir, so the scripts would be shipped
    // as a toolchain that cannot run (and whose first action is to abort). Reject the combination up
    // front rather than copy dead files (mirrors the two rules above).
    if cli.json_schema_scripts && !cli.json_schema_export {
        return Err(
            "--json-schema-scripts=true requires --json-schema-export=true: the copied scripts read \
             the schema document the json-gen crate exports, so without it there is nothing to \
             compile to TypeScript"
                .to_owned(),
        );
    }
    // An extra schema root is an additional registration row in the json-gen crate's `add_schemas`;
    // without --json-schema-export there is no json-gen crate and no `add_schemas` for the row to
    // land in, so the flag would silently do nothing. Reject the combination up front (mirrors the
    // three rules above).
    if !cli.json_schema_root.is_empty() && !cli.json_schema_export {
        return Err(
            "--json-schema-root requires --json-schema-export=true: the extra root is emitted as a \
             registration row in the json-gen crate's `add_schemas`, so without it there is no \
             crate for the row to land in"
                .to_owned(),
        );
    }
    // Two identical --json-schema-root values are a user mistake with no meaning: the emitted rows
    // are byte-identical, and the second is a silent no-op (the injectivity ledger the runtime
    // `add_schema` keeps is keyed on `std::any::type_name` and only fires when a name is claimed by a
    // DIFFERENT rust type, so re-registering ONE type never trips it). This is exact-string dedup
    // only — two spellings of one type (`crate::Foo` vs `cddl_lib::Foo`) cannot be detected here,
    // and are harmless for that same reason.
    let mut seen_roots = std::collections::BTreeSet::new();
    for root in &cli.json_schema_root {
        if !seen_roots.insert(root.as_str()) {
            return Err(format!(
                "--json-schema-root={root} was passed more than once: each extra JSON-schema root is \
                 registered exactly once, so a repeated value is a no-op rather than anything the \
                 tool could act on (this compares the value verbatim — two spellings of one type are \
                 not detected, and are harmless)"
            ));
        }
    }
    // A dep registrar call is a line in the json-gen crate's `add_schemas`; without
    // --json-schema-export there is no json-gen crate and no `add_schemas` for the call to land in,
    // so the flag would silently do nothing (mirrors the four rules above).
    if !cli.json_schema_dep.is_empty() && !cli.json_schema_export {
        return Err(
            "--json-schema-dep requires --json-schema-export=true: the dependency's registrar is \
             emitted as a call in the json-gen crate's `add_schemas`, so without it there is no \
             crate for the call to land in"
                .to_owned(),
        );
    }
    // Both duplicate checks live here rather than falling out of the accessor's collection type,
    // because `json_schema_deps()` is a Vec on purpose (flag order is load-bearing for the emission).
    let deps = cli.json_schema_deps();
    // One label naming two json-gen crates is ambiguous, not additive: nothing decides which of the
    // two registrars is "the" one for that dependency, and emitting both would silently make the
    // label mean something other than a dependency.
    let mut seen_dep_labels = std::collections::BTreeSet::new();
    for (label, _) in &deps {
        if !seen_dep_labels.insert(label.as_str()) {
            return Err(format!(
                "--json-schema-dep label {label:?} was passed more than once: a dependency has one \
                 json-gen crate, so two mappings for one label are ambiguous rather than additive \
                 (this compares the label verbatim after trimming — two labels naming the SAME \
                 dependency are not detected, and are caught by the lib-name check below whenever \
                 they map to one registrar)"
            ));
        }
    }
    // The same registrar under two labels is a user mistake with no meaning: `add_schemas` registers
    // this crate's types into the generator it is handed, so calling it twice registers nothing new —
    // the second call is a no-op rather than a composition.
    let mut seen_dep_libs = std::collections::BTreeMap::new();
    for (label, lib) in &deps {
        if let Some(previous) = seen_dep_libs.insert(lib.as_str(), label.as_str()) {
            return Err(format!(
                "--json-schema-dep lib name {lib:?} was passed under more than one label \
                 ({previous:?} and {label:?}): calling one crate's `add_schemas` twice registers \
                 nothing the first call did not, so the second is a no-op rather than a composition \
                 (this compares the lib name verbatim after trimming and dash normalisation — two \
                 spellings of one registrar, e.g. `dep_json_schema_gen` and \
                 `crate::vendored_dep_alias`, are not detected, and are harmless for the same \
                 reason)"
            ));
        }
    }
    // A `--json-gen-dep` entry is a `[dependencies]` key in the json-gen crate's manifest; without
    // --json-schema-export that crate is never generated and neither is its manifest, so the flag
    // would silently do nothing (mirrors the five rules above).
    if !cli.json_gen_dep.is_empty() && !cli.json_schema_export {
        return Err(
            "--json-gen-dep requires --json-schema-export=true: the entry is written into the \
             json-gen crate's `Cargo.toml`, so without it there is no crate and no manifest for the \
             dependency to land in"
                .to_owned(),
        );
    }
    // A `--wasm-dep` entry is a `[dependencies]` key in the wasm crate's manifest; without --wasm
    // that crate is never generated and neither is its manifest, so the flag would silently do
    // nothing (the same rule its `--json-gen-dep` sibling above carries, on the other manifest).
    if !cli.wasm_dep.is_empty() && !cli.wasm {
        return Err(
            "--wasm-dep requires --wasm=true: the entry is written into the wasm crate's \
             `Cargo.toml`, so without it there is no crate and no manifest for the dependency to \
             land in"
                .to_owned(),
        );
    }
    // A `--component-dep` entry is a `[dependencies]` key in the component crate's manifest; without
    // --component that crate is never generated and neither is its manifest, so the flag would
    // silently do nothing (the same rule its two siblings above carry, on the third manifest).
    if !cli.component_dep.is_empty() && !cli.component {
        return Err(
            "--component-dep requires --component=true: the entry is written into the component \
             crate's `Cargo.toml`, so without it there is no crate and no manifest for the \
             dependency to land in"
                .to_owned(),
        );
    }
    // `--wit-package` names the generated WIT package, which only the component face emits. Without
    // --component there is no `.wit` for it to title, so the flag would silently do nothing —
    // rejected on exactly the terms the manifest-entry rules above are.
    if cli.wit_package.is_some() && !cli.component {
        return Err(
            "--wit-package requires --component=true: it names the generated WIT package, and \
             without the component face no `.wit` is emitted for it to title"
                .to_owned(),
        );
    }
    // `--component-extern-wit` materializes a dep's WIT into the component crate's own WIT package
    // and puts the co-required `with:` entries into its `generate!` invocation. Without --component
    // neither exists, so the flag would silently do nothing — rejected on exactly the terms its
    // siblings above are.
    if !cli.component_extern_wit.is_empty() && !cli.component {
        return Err(
            "--component-extern-wit requires --component=true: the dep's WIT is copied into the \
             component crate's own WIT package and its interfaces are named by that crate's \
             `wit_bindgen::generate!` invocation, and without the component face neither exists"
                .to_owned(),
        );
    }
    // A `<dep>` whose WIT is supplied but whose RULES are not is a dependency in name only: the WIT
    // says how the dep's types cross the component boundary, while `--extern-import` (or a physical
    // stub tree) is what puts those types in this spec's namespace at all. Only the flag pairing is
    // checkable here — a physical stub declares the dep too, which `component_wit_deps::load` sees
    // and this pre-parse check does not.
    let extern_import_deps = cli.extern_import_paths();
    for (dep, path) in cli.component_extern_wit_paths() {
        if !extern_import_deps.contains_key(&dep) && !stub_dir_declares(cli, &dep) {
            return Err(format!(
                "--component-extern-wit {dep}={path} names a dependency this run does not declare: \
                 no --extern-import {dep}=<path/to/extern-interface/{dep}> and no physical \
                 {stub}/{dep}/ directory in the input tree. The WIT says how {dep}'s types cross \
                 the component boundary; declaring the dependency is what puts them in this spec's \
                 namespace in the first place",
                stub = parsing::EXTERN_DEPS_DIR
            ));
        }
    }
    // `--lib-name` reaches the WIT surface twice under --component — it is the default WIT package
    // name and it is the world name — and unlike every other flag feeding a WIT identifier it has no
    // `value_parser` at all. A cargo package name may legally begin with a digit, and the kebab
    // converter refuses a digit-led word with an `assert!`, so without this rule a legal
    // `--lib-name 4chain --component=true` PANICS inside generation. Flag problems are graceful
    // errors here, never panics.
    if cli.component
        && let Some(problem) = crate::generation::wit::wit_identifier_problem(&cli.lib_name)
    {
        return Err(format!(
            "--lib-name {name:?} cannot be used with --component=true: {problem}. The component \
             face turns --lib-name into the generated WIT package name and the world name. Choose \
             a --lib-name that converts to a WIT identifier, or name the package explicitly with \
             --wit-package (which overrides the derived default).",
            name = cli.lib_name
        ));
    }
    // One package name under two paths is ambiguous, not additive: a manifest holds ONE
    // `[dependencies]` entry per package, so the second value would silently replace the first.
    // Read off the RAW flag lists rather than the `*_deps()` accessors, whose `BTreeMap`s are
    // exactly where a duplicate would disappear.
    //
    // `--rust-dep` joins them here and NOWHERE above: the rust crate is the one crate every run
    // generates, so there is no flag whose absence leaves its manifest unwritten.
    for (flag, entries) in [
        ("--json-gen-dep", &cli.json_gen_dep),
        ("--wasm-dep", &cli.wasm_dep),
        ("--rust-dep", &cli.rust_dep),
        ("--component-dep", &cli.component_dep),
    ] {
        let mut seen = std::collections::BTreeSet::new();
        for entry in entries {
            let name = entry
                .split_once('=')
                .map_or(entry.as_str(), |(name, _)| name)
                .trim();
            if !seen.insert(name) {
                return Err(format!(
                    "{flag} package name {name:?} was passed more than once: a manifest holds \
                     one `[dependencies]` entry per package, so a second path under one name would \
                     silently replace the first rather than adding anything"
                ));
            }
        }
    }
    // `--std-forward-dep` names a `[dependencies]` key twice over: the entry it reshapes to
    // `default-features = false`, and the `<package>/std` entry it adds to the crate's own `std`
    // feature. A package with no `--rust-dep` has no entry to reshape, so the tool would be the
    // AUTHOR of a manifest cargo rejects — either a bare `default-features = false` with no source,
    // or a feature naming a dependency that is not there. Rejected here rather than left to cargo
    // because the tool wrote the manifest and the user wrote the flags.
    let rust_dep_packages: std::collections::BTreeSet<&str> = cli
        .rust_dep
        .iter()
        .map(|entry| {
            entry
                .split_once('=')
                .map_or(entry.as_str(), |(name, _)| name)
                .trim()
        })
        .collect();
    for package in &cli.std_forward_dep {
        let package = package.trim();
        if !rust_dep_packages.contains(package) {
            return Err(format!(
                "--std-forward-dep={package} names a package that no --rust-dep declares: \
                 forwarding takes that dependency with `default-features = false` and adds \
                 `{package}/std` to this crate's `std` feature, and both need the \
                 `[dependencies].{package}` entry only --rust-dep can write (a \
                 `default-features = false` with no path or version, or a feature naming an absent \
                 dependency, is a manifest cargo rejects). Pass \
                 `--rust-dep {package}=<path>` as well, or drop the forwarding"
            ));
        }
    }
    Ok(())
}

/// Parse the CDDL input described by `cli`, build the intermediate representation, and invoke
/// `f` with a borrow of it plus the `export_raw_bytes_encoding_trait` flag. The AST and IR are
/// owned for the duration of the call, so `f` must return owned data (it cannot leak the borrow).
///
/// Deliberately NOT an install point for `cli.verbosity` ([`crate::log::scoped`]), unlike
/// [`generate_to_disk`]: a caller driving the pipeline itself owns its process's logging level, and a
/// library entry point that silently reset it would be taking a decision that is not this function's
/// to take. Wrap the call in a `crate::log::scoped(cli.verbosity)` guard to get
/// `generate_to_disk`'s behaviour.
pub fn with_types<R>(
    cli: &Cli,
    f: impl FnOnce(&IntermediateTypes, bool) -> R,
) -> Result<R, Box<dyn std::error::Error>> {
    validate_flag_combinations(cli)?;
    // Pre-processing files for multi-file support
    let input_files = if cli.input.is_dir() {
        let mut cddl_paths_buf = Vec::new();
        cddl_paths(&mut cddl_paths_buf, &cli.input)?;
        cddl_paths_buf
    } else {
        vec![cli.input.clone()]
    };
    // To get around an issue with cddl where you can't parse a partial cddl fragment
    // we must group all files together. To mark scope we insert string constants with
    // a specific, unlikely to ever be used, prefix. The names contain a number after
    // to avoid a parsing error (rule with same identifier already defined).
    // This approach was chosen over comments as those were finicky when not attached
    // to specific structs, and the existing comment parsing ast was not suited for this.
    // If, in the future, cddl released a feature flag to allow partial cddl we can just
    // remove all this and revert back the commit before this one for scope handling.
    let mut input_files_content = input_files
        .iter()
        .enumerate()
        .map(|(i, input_file)| {
            let scope = if input_files.len() > 1 {
                use std::path::Component;
                let relative = pathdiff::diff_paths(input_file, &cli.input).unwrap();
                let mut components = relative
                    .components()
                    .filter_map(|p| match p {
                        Component::Normal(part) => Some(
                            std::path::Path::new(part)
                                .file_stem()
                                .unwrap()
                                .to_str()
                                .unwrap()
                                .to_owned(),
                        ),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                if let Some(c) = components.last()
                    && *c == "mod"
                {
                    components.pop();
                }
                components.join("::")
            } else {
                ROOT_SCOPE.to_string()
            };
            let raw = std::fs::read_to_string(input_file)?;
            // Recognize (and refuse) CDDL-module directives per-file, before concatenation, so the
            // diagnostic can name the offending file — the concatenated buffer has no provenance.
            scan_module_directives(input_file, &raw)?;
            Ok(format!(
                "\n{}{} = \"{}\"\n{}\n",
                parsing::SCOPE_MARKER,
                i,
                scope,
                raw
            ))
        })
        .collect::<Result<String, Box<dyn std::error::Error>>>()?;
    // Consumer-side consumption of a dependency's committed extern-interface export
    // (`--extern-import <dep>=<path>`). The mapped files are read and appended to the concatenation
    // with EXTERN_DEPS_DIR scope markers, so their rules land in a non-exported `<dep>` scope exactly
    // as a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/` stub tree would — after which the entire
    // downstream pathway (scope filter, extern resolution, wasm crate mapping, request sidecars) is
    // UNCHANGED. Assembled in a SEPARATE loop from the main input's on purpose: the main loop keys
    // scope computation on `input_files.len() > 1` and pathdiffs against `cli.input`, so folding these
    // markers into it would flip a single-file consumer off its ROOT_SCOPE behavior. Appended BEFORE
    // the raw-bytes-marker scan below so a dep's raw-bytes export sets the trait flag identically to a
    // physical stub carrying the same marker.
    let extern_imports = cli.extern_import_paths();
    let imported_raw_bytes = append_extern_imports(
        cli,
        &extern_imports,
        input_files.len(),
        &mut input_files_content,
    )?;
    // The raw-bytes trait flag is read off the WHOLE export, not the narrowed slice. It is a
    // property of the dependency surface a consumer is built against, not of which rules that
    // consumer happens to reach this run — so narrowing must not be able to flip it, and a consumer
    // regenerated after adding or dropping one reference does not gain or lose the trait.
    let export_raw_bytes_encoding_trait =
        imported_raw_bytes || input_files_content.contains(parsing::RAW_BYTES_MARKER);
    // we also need to mark the extern marker to a placeholder struct that won't get codegened
    input_files_content.push_str(&format!("{} = [0]", parsing::EXTERN_MARKER));
    // and a raw bytes one too
    input_files_content.push_str(&format!("{} = [1]", parsing::RAW_BYTES_MARKER));

    // Plain group / scope marking.
    // Note: we use the checked parse entry (validates that every referenced type/group name is defined)
    //       so an undefined reference is a graceful error here rather than downstream panic during IR build
    //       (i.e. we don't use the unchecked `cddl_from_str`)
    let cddl = match cddl::ast::CDDL::from_slice(input_files_content.as_bytes()) {
        Ok(cddl) => cddl,
        // Staleness diagnostic: the checked parse validates every referenced type/group name is
        // defined, so a consumer referencing an ident absent from a dep's export fails HERE with the
        // fork's generic "undefined reference" error, which knows nothing about deps. When
        // `--extern-import` is in use, AUGMENT (never swallow) that error with the declared dep list
        // and the "referenced ident may be `; unexported:` in the export, or predate the dep's current
        // spec — regenerate the dependency / fix the cause in the dependency's own spec" hint.
        Err(e) if !extern_imports.is_empty() => {
            return Err(extern_import_staleness_error(e, &extern_imports).into());
        }
        Err(e) => return Err(e.into()),
    };
    // The one group-rule spelling whose rule-position directive the pinned parser cannot deliver
    // (a trailing comment after a closing paren on its own line) is refused here, from the source
    // buffer the spans index — the AST holds no slot to read it from, so this is the only place the
    // spelling is still visible. Run ONCE, before the IR loop, so the refusal is independent of how
    // many passes the loop takes and lands before any rule is walked.
    if let Some(msg) =
        parsing::multiline_group_trailing_directive_rejection(&cddl, input_files_content.as_str())
    {
        return Err(msg.into());
    }
    let pv = cddl::ast::parent::ParentVisitor::new(&cddl).unwrap();
    // The IR build runs in a LOOP because one of its inputs is decided from its own OUTPUT: the
    // recursive-type boundary (`crate::recursion_boundary`) classifies the finalized IR's cycles,
    // and the repair it can offer — emitting the collection-backed rules of an alias-expansion cycle
    // as `@newtype` wrapper structs rather than transparent `pub type` aliases — has to be applied
    // where a rule's directives are read, which is the parse walk that has already happened. Re-running
    // with the set marked is what makes an auto-nominalized rule identical to one the spec wrote
    // `; @newtype` on, rather than a second implementation of the wrapper that would have to grow its
    // own wasm/preserve/emit-tests surfaces. It terminates because the forced set only grows and is
    // bounded by the rule count, and a pass that adds nothing to it is the last one; in practice a
    // spec with such a cycle takes exactly two passes and every other spec takes one.
    //
    // Passes after the first are SILENCED: they re-derive facts the first pass already reported (the
    // `Recursive type: …` notice above all), and printing them twice would make the notice count
    // depend on whether a repair happened. The announcement of the repair itself is printed once,
    // after the loop, outside the guard.
    let mut auto_newtype_rules: std::collections::BTreeSet<RustIdent> =
        std::collections::BTreeSet::new();
    let mut boundary_announcements: Vec<String> = Vec::new();
    let mut ir_pass = 0usize;
    let types = loop {
        let _quiet_repass = (ir_pass > 0).then(|| crate::log::scoped(crate::log::Verbosity::Error));
        let mut types = IntermediateTypes::new();
        types.set_auto_newtype_rules(auto_newtype_rules.clone());

        // Reserved-name pre-scan — runs BEFORE any `rule_ident` / `RustIdent::new` call (which start
        // in the scope filter just below and recur through the whole IR build). A rule/plain-group
        // whose camel-cased name collides with a reserved Rust type (`option` → `Option`, `box` →
        // `Box`) or is a CDDL keyword (`true` / `false`) hits an `assert!` in `RustIdent::new` — a
        // panic on otherwise-valid CDDL. `RustIdent::new` has no `IntermediateTypes` handle, so it
        // can't record a graceful rejection itself; we catch those user-chosen names here at the seam
        // where they enter and abort through the normal rejection channel. Because a reserved name can
        // also be REFERENCED by another rule (a reference reaches `RustIdent::new` too), we surface the
        // rejection immediately rather than after IR build — nothing may proceed to the assert.
        // Track identifiers already seen in source order so a `/=`/`//=` rule that EXTENDS an
        // already-defined identifier can be rejected loudly. Source-order iteration makes the "already
        // defined" test (and thus determinism) inherent; the FIRST definition of a name via `/=` is
        // valid CDDL (the shelley precedent) and must pass through, so we only reject on a repeat.
        let mut seen_idents = std::collections::BTreeSet::new();
        for cddl_rule in cddl.rules.iter() {
            if rule_is_scope_marker(cddl_rule).is_some() {
                continue;
            }
            if let Some(msg) = crate::intermediate::reserved_ident_rejection(&cddl_rule.name()) {
                types.record_rejection(msg);
            }
            // A dotted rule name (e.g. from cddlc `as`-namespacing, `cose.label`) passes through
            // `convert_to_camel_case` unchanged into invalid Rust; reject it here, at the same seam.
            if let Some(msg) = crate::intermediate::dotted_ident_rejection(&cddl_rule.name()) {
                types.record_rejection(msg);
            }
            // Rule-position `@name` is a directive drop (silent on type rules, mis-applied on plain
            // groups): `@name` renames fields/variants/arms, never the rule identifier itself. Reject
            // it here, alongside the reserved-name pre-scan, rather than emit a surprising type name.
            if let Some(msg) = parsing::rule_position_name_rejection(cddl_rule) {
                types.record_rejection(msg);
            }
            // A generic definition whose body is a plain group (`set<a> = (* a)`, and the bare-paren
            // group-choice spelling that the AST also gives us as a group rule). Refused HERE because
            // every site that would otherwise reach it is an `assert_eq!` abort with no rejection
            // channel — `dep_graph::find_references` (rule ordering, which runs pre-IR) and `parsing`'s
            // own `Rule::Group` arm. Both stay as re-earning guards. The one reach that precedes this
            // loop, `extern_narrow::scan_consumer` (input assembly, before the checked parse), skips
            // the rule by consulting the same predicate.
            if let Some(msg) = parsing::generic_plain_group_def_rejection(cddl_rule) {
                types.record_rejection(msg);
            }
            // Incremental choice extension (`a /= tstr`, `g //= (...)`): `parse_rule` re-registers the
            // identifier on each statement, so the LAST definition wins and every earlier arm is
            // silently dropped. Reject the EXTENSION (identifier already seen) loudly; the initial
            // definition via `/=`/`//=` (identifier not yet seen) is valid CDDL and passes through.
            if !seen_idents.insert(cddl_rule.name())
                && let Some(msg) = parsing::incremental_choice_extension_rejection(cddl_rule)
            {
                types.record_rejection(msg);
            }
        }
        if types.has_rejections() {
            return Err(types.rejections_error());
        }

        // mark scope and filter scope markers
        let mut scope = ROOT_SCOPE.clone();
        let cddl_rules = cddl
            .rules
            .iter()
            .filter(|cddl_rule| {
                // We inserted string constants with specific prefixes earlier to mark scope
                if let Some(new_scope) = rule_is_scope_marker(cddl_rule) {
                    crate::info!("Switching from scope '{scope}' to '{new_scope}'");
                    scope = new_scope;
                    false
                } else {
                    let ident = rule_ident(cddl_rule);
                    types.mark_scope(ident.clone(), scope.clone());
                    // Preserve the source spelling (`-` vs `_`, acronym casing) before `RustIdent`
                    // camel-cases it away — the conformance oracle needs the provable source rule name.
                    types.mark_source_rule_name(ident, cddl_rule.name());
                    true
                }
            })
            .collect::<Vec<_>>();
        // We need to know beforehand which are plain groups so we can serialize them properly
        // e.g. x = (3, 4), y = [1, x, 2] should be [1, 3, 4, 2] instead of [1, [3, 4], 2]
        for cddl_rule in cddl_rules.iter() {
            if let cddl::ast::Rule::Group { rule, .. } = cddl_rule {
                // Freely defined group - no need to generate anything outside of group module
                match &rule.entry {
                    cddl::ast::GroupEntry::InlineGroup {
                        group,
                        comments_after_group,
                        ..
                    } => {
                        assert_eq!(group.group_choices.len(), 1);
                        let rule_metadata = RuleMetadata::from(comments_after_group.as_ref());
                        types.mark_plain_group(
                            RustIdent::new(CDDLIdent::new(rule.name.to_string())),
                            PlainGroupInfo::new(Some(group.clone()), rule_metadata),
                        );
                    }
                    x => panic!("Group rule with non-inline group? {:?}", x),
                }
            }
        }

        // Creating intermediate form from the CDDL
        for cddl_rule in dep_graph::topological_rule_order(&cddl_rules) {
            crate::debug!(
                "\n\n------------------------------------------\n- Handling rule: {}:{}\n------------------------------------",
                scope,
                cddl_rule.name()
            );
            parse_rule(&mut types, &pv, cddl_rule, cli);
        }
        // Pre-finalize seeding of `used_as_key` from cross-crate requests (parsing is complete here, so
        // idents resolve; finalize's closure then expands the seeds transitively through private fields).
        // Both are no-ops — byte-identical output — when their flags are absent:
        //   - map KEYS of `--wrapper-requests` shapes addressed to this dep (lenient: an unparseable
        //     sidecar seeds nothing, leaving strict diagnosis to `emit_requested_collections`);
        //   - the `--key-requests` sidecar rows (strict: an unknown ident is a hard error).
        crate::wrapper_requests::seed_used_as_key_from_wrapper_requests(&mut types, cli);
        crate::wrapper_requests::seed_used_as_key_from_key_requests(&mut types, cli)?;
        types.finalize(&pv, cli)?;
        let verdict = crate::recursion_boundary::classify(&types, &auto_newtype_rules);
        if verdict
            .auto_newtype
            .iter()
            .all(|ident| auto_newtype_rules.contains(ident))
        {
            // No repair left to apply. Whatever the boundary refuses goes out through the ordinary
            // rejection channel — recorded and immediately drained, so nothing reaches the emission
            // seam's drain assertion with a pending rejection.
            for msg in verdict.refusals {
                types.record_rejection(msg);
            }
            if types.has_rejections() {
                return Err(types.rejections_error());
            }
            break types;
        }
        boundary_announcements = verdict.announcements;
        auto_newtype_rules.extend(verdict.auto_newtype);
        ir_pass += 1;
    };
    for announcement in boundary_announcements {
        crate::warn!("{announcement}");
    }

    // A spec whose finalized IR lowers CDDL `any` to the `AnyCbor` runtime type is a full-surface
    // citizen: rust ser/deser, the JSON serde/schemars impls, and the wasm wrapper class
    // (`generate_any_cbor_wasm`). No flag combination is rejected on `any`'s account.
    Ok(f(&types, export_raw_bytes_encoding_trait))
}

/// Run the full pipeline and write the generated crate(s) to `cli.output` (the CLI behaviour).
pub fn generate_to_disk(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    // This crate's own verbosity, installed for the duration of its generation and restored on exit.
    // One line here makes every caller correct by construction — the single-crate CLI path, config
    // mode's per-crate loop, and library users all get the level their own `Cli` asks for — and the
    // restore is what keeps a config run's RUN level intact across a crate that raised its own.
    let _verbosity = crate::log::scoped(cli.verbosity);
    with_types(cli, |types, export_raw_bytes_encoding_trait| {
        crate::info!(
            "\n-----------------------------------------\n- Generating code...\n------------------------------------"
        );
        let mut gen_scope = GenerationScope::new();
        gen_scope.generate(types, cli)?;
        // `finalize` short-circuits on pending rejections before this closure runs, then drains
        // again on exit. Per tests/README.md § "Design rules", keep this emission-site assertion
        // outside guarded branches; every snapshot-corpus fixture exercises this seam.
        assert!(
            !types.has_rejections(),
            "pipeline-boundary rejection-drain invariant violated: a rejection was recorded after \
             finalize's drains and would be silently swallowed; move the record site before \
             finalize or add a new drain"
        );
        gen_scope.export(types, export_raw_bytes_encoding_trait, cli)?;
        // Guarded HERE rather than inside `print_info`, because the cost being avoided is the `{:?}`
        // formatting of every registered struct (215 KB on a 501-line spec), not the writes: the
        // function's own lines are `trace!` too, but only a call-site guard skips the formatting.
        if crate::log::enabled(crate::log::Verbosity::Trace) {
            types.print_info();
        }
        gen_scope.print_structs_without_deserialize();
        Ok(())
    })?
}

/// Run parse + generate in-process and return the generated source (post-rustfmt) keyed by
/// logical path. No disk I/O — for fast snapshot tests. See [`GenerationScope::emit_generated`].
#[cfg(test)]
pub fn generated_strings(
    cli: &Cli,
) -> Result<std::collections::BTreeMap<String, String>, Box<dyn std::error::Error>> {
    with_types(cli, |types, raw_bytes| {
        let mut gen_scope = GenerationScope::new();
        // This closure's error channel is `io::Error` (the file-map producer's); a sidecar refusal
        // is a `String`, so it is carried as an `Error::other` rather than reworded.
        gen_scope
            .generate(types, cli)
            .map_err(std::io::Error::other)?;
        // `finalize` short-circuits on pending rejections before this closure runs, then drains
        // again on exit. Per tests/README.md § "Design rules", keep this emission-site assertion
        // outside guarded branches; every snapshot-corpus fixture exercises this seam.
        assert!(
            !types.has_rejections(),
            "pipeline-boundary rejection-drain invariant violated: a rejection was recorded after \
             finalize's drains and would be silently swallowed; move the record site before \
             finalize or add a new drain"
        );
        gen_scope.generated_files(types, raw_bytes, cli)
    })?
    .map_err(Into::into)
}

/// Parse + build the IR and return the dep-side extern-interface export files (post-projection),
/// keyed by path relative to `<output>` (`extern-interface/<dep_key>/…/mod.cddl`). No disk I/O —
/// the snapshot-fixture analog of [`generated_strings`], driving the SAME projection walk `export`
/// writes to disk so the tested and shipped export can't drift. The projection is infallible
/// (exclude-with-record); the outer `Result` is only the parse/IR-build failure surface.
#[cfg(test)]
pub fn extern_interface_strings(
    cli: &Cli,
) -> Result<std::collections::BTreeMap<String, String>, Box<dyn std::error::Error>> {
    with_types(cli, |types, _| {
        crate::generation::extern_interface::extern_interface_files(types, cli)
    })
}

/// The emitted `.wit` files for a spec, keyed by path relative to `<output>` (`component/wit/…`).
/// The snapshot-fixture analog of [`extern_interface_strings`], driving the SAME producer the
/// component face's export writes to disk so the tested and shipped WIT cannot drift.
///
/// A door onto the WIT ALONE, so the gates over it — the four-stage validity gate and the
/// wasm-posture purity assertion — see nothing else in the tree; the purity assertion in particular
/// must be able to compare two runs whose rust/wasm halves legitimately differ.
///
/// It FILTERS the full producer rather than calling the projection directly. One member of the
/// projection (`from-cbor-bytes`) is gated on a verdict only the rust face's own walk reaches —
/// which types got a `Deserialize` impl — so a shortcut straight to `wit::wit_files` would pin a
/// WIT the tool never ships.
#[cfg(test)]
pub fn wit_strings(
    cli: &Cli,
) -> Result<std::collections::BTreeMap<String, String>, Box<dyn std::error::Error>> {
    let prefix = format!("{}/", crate::generation::layout::COMPONENT_WIT_DIR);
    Ok(generated_strings(cli)?
        .into_iter()
        .filter(|(path, _)| path.starts_with(&prefix))
        .collect())
}

/// The emitted no-std-check shim crate's files, keyed by path relative to `<output>`
/// (`no-std-check/…`). The snapshot-fixture analog of [`extern_interface_strings`], driving the SAME
/// producer `export` writes to disk so the tested and shipped shim can't drift.
///
/// Deliberately NOT shaped like its sibling: no `Result` and no `with_types`, because the producer
/// takes only `&Cli` — the shim asserts a property of the generated CRATE, not of the spec, so there
/// is no IR to build and no parse failure to surface. Adding either would be a signature that lies
/// about what can go wrong here.
#[cfg(test)]
pub fn no_std_check_strings(cli: &Cli) -> std::collections::BTreeMap<String, String> {
    crate::generation::no_std_check::no_std_check_files(cli)
}

/// Parse + build the IR and return a debug dump of the resolved Rust structures, for IR-level
/// snapshot tests (localizes a regression to parsing/IR vs generation). Deliberately excludes
/// the raw cddl AST held in `plain_groups` (noisy byte-span info).
#[cfg(test)]
pub fn ir_structs_debug(cli: &Cli) -> Result<String, Box<dyn std::error::Error>> {
    with_types(cli, |types, _| format!("{:#?}", types.rust_structs()))
}

#[cfg(test)]
mod tests {
    /// A multifile input DIRECTORY routinely contains non-`.cddl` files (README, LICENSE,
    /// dotfiles); discovery must SKIP them rather than feed them to the CDDL parser (which
    /// previously panicked on the first such file). Regression guard for the fix in
    /// `cddl_paths`' extension filter.
    #[test]
    fn directory_input_skips_non_cddl_files() {
        let dir =
            std::env::temp_dir().join(format!("cddl_codegen_ext_skip_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("lib.cddl"), "foo = [x: uint]\n").unwrap();
        std::fs::write(dir.join("README"), "not cddl at all {{{").unwrap();
        std::fs::write(dir.join(".gitignore"), "*").unwrap();
        let mut found = Vec::new();
        let result = super::cddl_paths(&mut found, &dir);
        std::fs::remove_dir_all(&dir).unwrap();
        result.unwrap();
        assert_eq!(found, vec![dir.join("lib.cddl")]);
    }

    /// A rule/plain-group whose name would collide with a reserved Rust type or a CDDL keyword is
    /// rejected GRACEFULLY (a drained rejection → `Err`), never via the `assert!`-panic in
    /// `RustIdent::new`. Pins the message for one `STD_TYPES` case (`option` → `Option`) and one
    /// CDDL-keyword case (`true`), including the `@name`-anchored remedy. Regression guard for the
    /// reserved-name graceful-rejection fix; the full (position × hazard) sweep lives in
    /// `identifier_hazard_robustness_catalog`.
    #[test]
    fn reserved_rule_name_rejects_gracefully_not_panics() {
        use crate::cli::Cli;
        use clap::Parser;
        fn gen_err(spec: &str, tag: &str) -> String {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_reserved_{tag}_{}.cddl",
                std::process::id()
            ));
            std::fs::write(&path, spec).unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "reserved_name_unused",
                "--wasm",
                "false",
            ]);
            let result = super::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            result
                .expect_err("a reserved rule name must reject gracefully, not generate")
                .to_string()
        }

        // STD_TYPES collision: `option` camel-cases to `Option`.
        let opt = gen_err("option = [a: uint]\n", "option");
        assert!(opt.contains("rule `option`"), "must cite the rule: {opt}");
        assert!(
            opt.contains("Option"),
            "must name the reserved Rust type it collides with: {opt}"
        );
        assert!(
            opt.contains("@name"),
            "must point at the @name remedy: {opt}"
        );

        // CDDL keyword: `true`.
        let tru = gen_err("true = [a: uint]\n", "true");
        assert!(tru.contains("rule `true`"), "must cite the rule: {tru}");
        assert!(
            tru.contains("reserved CDDL keyword"),
            "must explain the keyword rejection: {tru}"
        );
        assert!(
            tru.contains("@name"),
            "must point at the @name remedy: {tru}"
        );
    }

    /// Helpers for the float-window parse tests: run the pipeline to either the IR debug dump or the
    /// generated-source concatenation (mirrors the reserved-name test's temp-file scaffolding).
    #[cfg(test)]
    fn float_test_cli(spec: &str, tag: &str) -> (crate::cli::Cli, std::path::PathBuf) {
        use crate::cli::Cli;
        use clap::Parser;
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_float_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "float_window_unused",
            "--wasm",
            "false",
        ]);
        (cli, path)
    }

    /// A top-level literal float range (`c = 0.5..10.5`) must WRAP into a bounds-enforcing newtype
    /// (registered as a `Wrapper`, not dropped into a transparent `pub type` alias) and carry the
    /// window WITHOUT truncation — `10.5`/`0.5` survive exactly. `float64 .le 10` (int literal on a
    /// float head) promotes to a float window (`10.0`), never the integer path.
    #[test]
    fn float_window_parses_wraps_without_truncation() {
        let (cli, path) = float_test_cli("c = 0.5..10.5\n", "range");
        let ir = super::ir_structs_debug(&cli).unwrap();
        std::fs::remove_file(&path).ok();
        // registered as a Wrapper (aliases never appear in rust_structs) carrying a float window
        assert!(
            ir.contains('C'),
            "the wrapper rule C must be registered: {ir}"
        );
        assert!(
            ir.contains("float_min_max: Some"),
            "the window must ride the wrapper's float slot: {ir}"
        );
        assert!(
            ir.contains("10.5") && ir.contains("0.5"),
            "the float endpoints must survive without truncation: {ir}"
        );

        let (cli, path) = float_test_cli("d = float64 .le 10\n", "le_int");
        let ir = super::ir_structs_debug(&cli).unwrap();
        std::fs::remove_file(&path).ok();
        assert!(
            ir.contains("float_min_max: Some"),
            "an int literal on a float head must produce a FLOAT window: {ir}"
        );
        assert!(
            ir.contains("10.0"),
            "the int bound must promote to f64 (10.0), not the integer path: {ir}"
        );
    }

    /// `.ne` over a float and a decimal float bound on an integer-typed head are GRACEFUL rejections
    /// (drained `Err`, never a panic), with actionable messages naming the offending shape + remedy.
    #[test]
    fn float_unsupported_constraints_reject_gracefully() {
        fn gen_err(spec: &str, tag: &str) -> String {
            let (cli, path) = float_test_cli(spec, tag);
            let result = super::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            result
                .expect_err("unsupported float constraint must reject gracefully, not generate")
                .to_string()
        }

        let ne = gen_err("bad = float64 .ne 5.0\n", "ne");
        assert!(ne.contains(".ne"), "must name the `.ne` op: {ne}");
        assert!(
            ne.contains("float"),
            "must explain the float-exclusion limitation: {ne}"
        );

        let dec = gen_err("bad = uint .le 10.5\n", "uint_dec");
        assert!(
            dec.contains("decimal"),
            "must call out the decimal float bound: {dec}"
        );
        assert!(
            dec.contains("integer"),
            "must explain the integer-head mismatch: {dec}"
        );
    }
}
