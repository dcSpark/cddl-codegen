//! `--extern-import <dep>=<path>`: consumer-side consumption of a dependency's committed
//! extern-interface export (commit 6).
//!
//! The flag reads a dep's `extern-interface/<dep>/**` export and concatenates it with EXTERN_DEPS_DIR
//! scope markers, so its rules land in the same non-exported `<dep>` scope a physical hand-stub tree
//! would — after which the whole extern-deps pathway is unchanged. The acceptance criterion is
//! byte-identity: a consumer generated once from a faithful physical stub and once via
//! `--extern-import` at the minted export must produce identical rust output. The stub is the export
//! minus its version header (what a careful human would write TODAY — current derivation, the pins
//! the export carries match the derived names), so the comparison isolates the assembly seam.
//!
//! These tests drive the generator end-to-end over scratch directories (mirroring the
//! `tests/extern-deps/` shape into a scratch dir, per AGENTS.md — never a real consumer checkout) and
//! the committed source specs under `tests/extern-import/`.

use crate::cli::Cli;
use clap::Parser;
use std::collections::BTreeMap;

fn fixture(rel: &str) -> String {
    std::fs::read_to_string(std::path::Path::new("tests/extern-import").join(rel))
        .unwrap_or_else(|e| panic!("reading fixture {rel}: {e}"))
}

/// A unique scratch directory for one test (cleaned first).
fn scratch(tag: &str) -> std::path::PathBuf {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_extern_import_{tag}_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    root
}

fn write(root: &std::path::Path, rel: &str, content: &str) {
    let path = root.join(rel);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(&path, content).unwrap();
}

/// Mint a dependency's extern-interface export in-process (the SAME projection `export` writes to
/// disk), keyed by path relative to `<output>` (`extern-interface/<dep_key>/…/mod.cddl`).
fn mint_export(dep_spec: &str, dep_key: &str, tag: &str) -> BTreeMap<String, String> {
    let root = scratch(&format!("mint_{tag}"));
    write(&root, "lib.cddl", dep_spec);
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        root.join("lib.cddl").to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "false",
        "--lib-name",
        dep_key,
    ]);
    let files = crate::api::extern_interface_strings(&cli)
        .expect("dep export projection must succeed (exclude-with-record, never abort)");
    let _ = std::fs::remove_dir_all(&root);
    files
}

/// Generate a consumer's rust source map from a directory input (physical-stub path) or a single
/// file plus flags (`--extern-import` path). Returns post-rustfmt source keyed by path, or the
/// stringified graceful `Err`.
fn generate(input: &std::path::Path, extra: &[&str]) -> Result<BTreeMap<String, String>, String> {
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "false",
    ];
    args.extend_from_slice(extra);
    let cli = Cli::parse_from(args);
    crate::api::generated_strings(&cli).map_err(|e| e.to_string())
}

/// THE acceptance test. Consume the dep's minted export two ways — a faithful physical hand-stub
/// (export minus the version header) and `--extern-import` at the export tree — and require the
/// consumer's generated rust output byte-identical. This is the series' core criterion: the pins the
/// export carries reproduce the same names a careful stub author would derive today, and the marker
/// assembly lands the imported rules in exactly the scope the physical stub does.
#[test]
fn extern_import_matches_hand_stub_byte_for_byte() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "byteid");
    let consumer = fixture("consumer/lib.cddl");

    // Run A — physical hand-stub: consumer at the tree root (lib.cddl -> ROOT_SCOPE) + a stub under
    // _CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/ = the export minus its header line.
    let stub_root = scratch("byteid_stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        let stub_body = strip_header(content);
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &stub_body,
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("physical-stub generation must succeed");

    // Run B — --extern-import: a single-file consumer + the export tree written verbatim (header
    // intact), pointed at by the flag.
    let flag_root = scratch("byteid_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("byteid_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert_eq!(
        via_flag.keys().collect::<Vec<_>>(),
        via_stub.keys().collect::<Vec<_>>(),
        "the generated file SET must match between --extern-import and the physical stub"
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "byte-identity broke for {path}:\n--- via --extern-import ---\n{}\n--- via physical stub ---\n{stub_content}",
            via_flag.get(path).map(String::as_str).unwrap_or("<absent>")
        );
    }
}

/// The export minus its `; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1` header line — the faithful physical
/// stub a careful human authors today (a hand-stub carries no seam header).
fn strip_header(export_file: &str) -> String {
    let mut lines = export_file.lines();
    let first = lines.next().unwrap_or("");
    assert_eq!(
        first, "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1",
        "minted export must open with the seam header"
    );
    let rest = lines.collect::<Vec<_>>().join("\n");
    format!("{rest}\n")
}

/// A single-file consumer keeps ROOT_SCOPE for its OWN types even while consuming a dep via
/// `--extern-import` (the flag markers are assembled in a separate loop, so they never flip the
/// main-input single-file ROOT_SCOPE behavior). The consumer's `thing` lands in the root module —
/// `rust/src/generated/mod.rs`, never a named submodule.
#[test]
fn extern_import_single_file_consumer_keeps_root_scope() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "rootscope");
    let flag_root = scratch("rootscope_consumer");
    write(&flag_root, "lib.cddl", &fixture("consumer/lib.cddl"));
    let export_dir = scratch("rootscope_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let map = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("generation must succeed");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        map.get("rust/src/generated/mod.rs")
            .is_some_and(|s| s.contains("struct Thing")),
        "the consumer's own `Thing` must land in the ROOT module (rust/src/generated/mod.rs)"
    );
    // A named submodule for the consumer's own type would mean it left ROOT_SCOPE.
    assert!(
        !map.keys()
            .any(|k| k.starts_with("rust/src/generated/thing/")),
        "the consumer's own type must not be pushed into a named submodule: {:?}",
        map.keys().collect::<Vec<_>>()
    );
}

/// Staleness: a consumer referencing an ident absent from the export fails the checked parse, and
/// with `--extern-import` in use that failure is AUGMENTED (not swallowed) with the declared dep
/// list, the export path, and the regenerate-the-dependency / check-`; unexported:`-records / hand-stub
/// hint.
#[test]
fn extern_import_staleness_wraps_undefined_reference() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "stale");
    let flag_root = scratch("stale_consumer");
    // References `missing`, which the export does not define.
    write(&flag_root, "lib.cddl", "bad = [x: missing]\n");
    let export_dir = scratch("stale_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an undefined reference must fail generation");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        err.contains("--extern-import") && err.contains("dep"),
        "the wrapped error must name the flag and the declared dep: {err}"
    );
    assert!(
        err.contains("unexported") && err.contains("Regenerate the dependency"),
        "the wrapped error must carry the staleness hint (records / regenerate): {err}"
    );
    // The original parse error is augmented, not swallowed.
    assert!(
        err.contains("missing definition for rule missing"),
        "the original undefined-reference detail must be preserved: {err}"
    );
    assert!(
        err.contains("extern-interface/dep"),
        "the wrapped error must name the export path: {err}"
    );
}

/// A flag-fed file missing the seam header is a hard error (the flag only accepts real machine-
/// generated exports; a headerless file is not one — hand-stubs go under the extern-deps dir).
#[test]
fn extern_import_missing_header_hard_errors() {
    let flag_root = scratch("noheader_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("noheader_export");
    // A headerless export file fed via the flag.
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("a headerless flag-fed file must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("missing") && err.contains("seam header"),
        "must name the missing seam header: {err}"
    );
    assert!(
        err.contains("mod.cddl"),
        "must name the offending file: {err}"
    );
}

/// A flag-fed file whose header names an unknown version is a hard error distinct from a missing one.
#[test]
fn extern_import_unknown_version_hard_errors() {
    let flag_root = scratch("badver_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("badver_export");
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v999\nfoo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an unknown seam version must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("unsupported version"),
        "must name the unsupported version: {err}"
    );
}

/// A flag-fed file carrying an unknown `@`-annotation is a hard error naming the file and token
/// (a typo or a newer dialect); the strict seam refuses to silently misread it.
#[test]
fn extern_import_unknown_annotation_hard_errors() {
    let flag_root = scratch("badtag_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("badtag_export");
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\nfoo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo @bogus_tag\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an unknown annotation token must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("unknown annotation token") && err.contains("@bogus_tag"),
        "must name the unknown token: {err}"
    );
}

/// An export carrying a `; unexported:` record consumes CLEANLY when the consumer does not reference
/// the excluded ident. The record's reason text contains `@custom_serialize`/`@custom_deserialize`
/// (whitelisted tokens), proving the strict `@`-scan does not false-positive on free-form reason text.
#[test]
fn extern_import_export_with_records_parses_cleanly() {
    let export = mint_export(&fixture("dep-with-records/lib.cddl"), "dep", "records");
    let root_export = &export["extern-interface/dep/mod.cddl"];
    assert!(
        root_export.contains("; unexported: cs — @custom_serialize"),
        "the export must carry the custom-serialize exclusion record: {root_export}"
    );

    let flag_root = scratch("records_consumer");
    // References only `foo` — not the excluded `cs`.
    write(&flag_root, "lib.cddl", "thing = [f: foo]\n");
    let export_dir = scratch("records_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let map = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("an export carrying `; unexported:` records must consume cleanly");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        map.contains_key("rust/src/generated/mod.rs"),
        "generation must produce the consumer's root module"
    );
}

/// Declaring a dep BOTH via `--extern-import` AND as a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/`
/// input directory is an ambiguous double declaration — a hard error, never a merge.
#[test]
fn extern_import_double_declaration_hard_errors() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "double");
    // A directory input carrying BOTH the consumer and a physical stub dir for `dep`.
    let input_root = scratch("double_input");
    write(&input_root, "lib.cddl", &fixture("consumer/lib.cddl"));
    write(
        &input_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/mod.cddl",
        "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\ncoin = uint ; @rust_name Coin\n",
    );
    let export_dir = scratch("double_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(&input_root, &["--extern-import", &import_arg])
        .expect_err("a dep declared both ways must be rejected");
    let _ = std::fs::remove_dir_all(&input_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("double declaration") && err.contains("dep"),
        "must reject the ambiguous double declaration naming the dep: {err}"
    );
}

/// A path with no `.cddl` files under it is a hard error naming the flag value.
#[test]
fn extern_import_empty_path_hard_errors() {
    let flag_root = scratch("emptypath_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let empty_dir = scratch("emptypath_export");
    let import_arg = format!("dep={}", empty_dir.to_str().unwrap());
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an export path with no .cddl files must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&empty_dir);
    assert!(
        err.contains("no .cddl files") && err.contains("dep="),
        "must name the flag value and the empty-path cause: {err}"
    );
}

/// A malformed `--extern-import` value (no `=`) is a hard error, mirroring the other cross-crate
/// flag parsers.
#[test]
#[should_panic(expected = "--extern-import")]
fn extern_import_malformed_flag_value_panics() {
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "unused.cddl",
        "--output",
        "unused",
        "--extern-import",
        "no_equals_sign",
    ]);
    let _ = cli.extern_import_paths();
}
