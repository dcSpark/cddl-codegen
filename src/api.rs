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
            std::fs::read_to_string(input_file).map(|raw| {
                format!(
                    "\n{}{} = \"{}\"\n{}\n",
                    parsing::SCOPE_MARKER,
                    i,
                    scope,
                    raw
                )
            })
        })
        .collect::<Result<String, _>>()?;
    let export_raw_bytes_encoding_trait = input_files_content.contains(parsing::RAW_BYTES_MARKER);
    // we also need to mark the extern marker to a placeholder struct that won't get codegened
    input_files_content.push_str(&format!("{} = [0]", parsing::EXTERN_MARKER));
    // and a raw bytes one too
    input_files_content.push_str(&format!("{} = [1]", parsing::RAW_BYTES_MARKER));

    // Plain group / scope marking.
    // Note: we use the checked parse entry (validates that every referenced type/group name is defined)
    //       so an undefined reference is a graceful error here rather than downstream panic during IR build
    //       (i.e. we don't use the unchecked `cddl_from_str`)
    let cddl = cddl::ast::CDDL::from_slice(input_files_content.as_bytes())?;
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
    for cddl_rule in cddl.rules.iter() {
        if rule_is_scope_marker(cddl_rule).is_some() {
            continue;
        }
        if let Some(msg) = crate::intermediate::reserved_ident_rejection(&cddl_rule.name()) {
            types.record_rejection(msg);
        }
        // Rule-position `@name` is a directive drop (silent on type rules, mis-applied on plain
        // groups): `@name` renames fields/variants/arms, never the rule identifier itself. Reject
        // it here, alongside the reserved-name pre-scan, rather than emit a surprising type name.
        if let Some(msg) = parsing::rule_position_name_rejection(cddl_rule) {
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
    types.finalize(&pv, cli)?;

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
        gen_scope.generated_files(types, raw_bytes, cli)
    })?
    .map_err(Into::into)
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
}
