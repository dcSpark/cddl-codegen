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
            println!("Skipping file: {}", path.as_path().to_str().unwrap());
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
            eprintln!(
                "warning: unrecognized `;# …` directive-shaped comment at {}:{line_no} — \
                 cddl-codegen does not process CDDL module directives; treating it as a comment.",
                input_file.display()
            );
        }
    }
    Ok(())
}

/// Read every `--extern-import <dep>=<path>` export and append it to `content` with EXTERN_DEPS_DIR
/// scope markers (a SEPARATE assembly loop from the main input's — see the call site). `marker_start`
/// is the first free scope-marker index (the main loop used `0..input_files.len()`), so imported
/// markers get distinct indices (a duplicate rule ident would be a parse error).
///
/// Hard errors (each naming the flag value): a `<dep>` also declared as a physical
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/` input directory (ambiguous double declaration, never a
/// merge); a path that does not exist or contains no `.cddl` files; a flag-fed file missing the
/// versioned seam header or carrying an unknown `@`-annotation (the strict seam — physical stubs stay
/// lenient because they are not routed here).
fn append_extern_imports(
    cli: &Cli,
    extern_imports: &std::collections::BTreeMap<String, String>,
    marker_start: usize,
    content: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut marker_index = marker_start;
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
        for import_file in &imported {
            let raw = std::fs::read_to_string(import_file)?;
            // The general per-file directive guard applies to imported files too (an export never
            // carries `;#` directives, but the check is cheap and keeps the invariant uniform).
            scan_module_directives(import_file, &raw)?;
            // The strict extern-interface seam: header + `@`-token whitelist (flag-fed files ONLY).
            scan_extern_import_seam(import_file, &raw)?;
            let scope = extern_import_scope(dep, &import_root, import_file);
            content.push_str(&format!(
                "\n{}{} = \"{}\"\n{}\n",
                parsing::SCOPE_MARKER,
                marker_index,
                scope,
                raw
            ));
            marker_index += 1;
        }
    }
    Ok(())
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
    // Both v1 and v2 are accepted (ruling §10.7): v2 is the conditional bump for `any`-bearing
    // exports, and this reader (A2+) understands the `any` spelling. A PRE-A2 reader accepts only v1,
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
         referenced identifier is undefined above, it may be recorded as `; unexported:` in the \
         dependency's export (a rule the dep could not project — hand-stub it), or the export may \
         predate the dependency's current spec. Regenerate the dependency so its extern-interface \
         export is fresh, check the export's `; unexported:` records, or hand-stub the missing rule. \
         Export paths: {paths}."
    )
}

/// Parse the CDDL input described by `cli`, build the intermediate representation, and invoke
/// `f` with a borrow of it plus the `export_raw_bytes_encoding_trait` flag. The AST and IR are
/// owned for the duration of the call, so `f` must return owned data (it cannot leak the borrow).
pub fn with_types<R>(
    cli: &Cli,
    f: impl FnOnce(&IntermediateTypes, bool) -> R,
) -> Result<R, Box<dyn std::error::Error>> {
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
                .into(),
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
                .into(),
        );
    }
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
    append_extern_imports(
        cli,
        &extern_imports,
        input_files.len(),
        &mut input_files_content,
    )?;
    let export_raw_bytes_encoding_trait = input_files_content.contains(parsing::RAW_BYTES_MARKER);
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
        // spec — regenerate the dependency / check the export's records / hand-stub" hint.
        Err(e) if !extern_imports.is_empty() => {
            return Err(extern_import_staleness_error(e, &extern_imports).into());
        }
        Err(e) => return Err(e.into()),
    };
    let pv = cddl::ast::parent::ParentVisitor::new(&cddl).unwrap();
    let mut types = IntermediateTypes::new();

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
                println!("Switching from scope '{scope}' to '{new_scope}'");
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
        println!(
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
    crate::wrapper_requests::seed_used_as_key_from_key_requests(&mut types, cli);
    types.finalize(&pv, cli)?;

    // A spec whose finalized IR lowers CDDL `any` to the `AnyCbor` runtime type has a full JSON
    // surface (serde `Serialize`/`Deserialize` + `schemars::JsonSchema`, loose-CBOR phase A3 WP2),
    // but no wasm wrapper class yet (WP3). Reject `--wasm=true` gracefully — naming the workaround —
    // rather than emit code that references a nonexistent `AnyCbor` wasm wrapper and fails to
    // compile. The JSON flags are accepted: the static assembly appends the mode-agnostic serde /
    // schemars impls (`export.rs`).
    if types.uses_any_cbor() && cli.wasm {
        return Err(
            "`any` (lowered to the AnyCbor runtime type) has no wasm surface yet: regenerate \
             with --wasm=false. The wasm AnyCbor wrapper class is planned (loose-CBOR phase A3)."
                .into(),
        );
    }

    Ok(f(&types, export_raw_bytes_encoding_trait))
}

/// Run the full pipeline and write the generated crate(s) to `cli.output` (the CLI behaviour).
pub fn generate_to_disk(cli: &Cli) -> Result<(), Box<dyn std::error::Error>> {
    with_types(cli, |types, export_raw_bytes_encoding_trait| {
        println!(
            "\n-----------------------------------------\n- Generating code...\n------------------------------------"
        );
        let mut gen_scope = GenerationScope::new();
        gen_scope.generate(types, cli);
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
        types.print_info();
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
        gen_scope.generate(types, cli);
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
