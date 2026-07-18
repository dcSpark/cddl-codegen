//! `@rust_name` pin annotation: parse (covered in `comment_ast.rs`), validate, and HONOR.
//!
//! `@rust_name <Ident>` pins the FINAL Rust type name a dependency's own codegen version spelled
//! into its artifact, so a consumer reads that name across the crate boundary instead of re-deriving
//! it (killing the cross-version naming-skew class). It is valid ONLY on a rule in a non-exported
//! (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_`) scope; on any exported rule it is a graceful rejection. When
//! honored, every INTERNAL spelling stays the consumer-derived ident and only the crate boundary
//! translates: `use <dep>::<Pinned> as <Derived>;` at the import seam, and the pinned leaf on the
//! wasm→rust full-path sites (`rust_crate_struct_from_wasm`).
//!
//! These tests drive the generator end-to-end over a synthetic extern-deps directory (mirroring the
//! `tests/extern-deps/` shape into a scratch dir, per AGENTS.md — never a real consumer checkout)
//! and assert on the generated source strings, the established in-src pattern for extern behavior.

use crate::cli::Cli;
use clap::Parser;

/// Generate a crate's source map from an in-memory set of `(relative path, contents)` files written
/// into a unique scratch DIRECTORY (so scope markers derive from the tree, exactly as directory
/// input does in production). Returns the post-rustfmt generated source keyed by path, or the
/// stringified graceful `Err`. `--wasm` opt-in emits the wasm files as strings too.
fn generate_dir(
    files: &[(&str, &str)],
    flags: &[&str],
    wasm: bool,
    tag: &str,
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_rustname_{}_{}",
        tag,
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    for (rel, content) in files {
        let path = root.join(rel);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, content).unwrap();
    }
    let mut args = vec![
        "cddl-codegen",
        "--input",
        root.to_str().unwrap(),
        "--output",
        "rust_name_unused",
        "--wasm",
        if wasm { "true" } else { "false" },
    ];
    args.extend_from_slice(flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    let _ = std::fs::remove_dir_all(&root);
    result
}

/// The whole generated source, concatenated (a coarse haystack for `contains` assertions).
fn joined(map: &std::collections::BTreeMap<String, String>) -> String {
    map.values().cloned().collect::<Vec<_>>().join("\n")
}

// A dependency in a non-exported scope pins `foo_bar` to `CustomName` (a name the consumer's own
// camel-casing would NEVER derive — it derives `FooBar`). The consumer references it, so the rust
// import must alias the dependency's real name back to the derived spelling, and the emitted body
// must keep using the derived `FooBar` (the internal spelling never changes).
#[test]
fn rust_name_pin_aliases_rust_import() {
    let map = generate_dir(
        &[
            ("main.cddl", "outer = { x: foo_bar }"),
            (
                "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/mydep/mod.cddl",
                "foo_bar = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name CustomName",
            ),
        ],
        &[],
        false,
        "pin_rust_import",
    )
    .expect("generation should succeed");
    let src = joined(&map);
    assert!(
        src.contains("use mydep::CustomName as FooBar;"),
        "expected aliased import `use mydep::CustomName as FooBar;`, got:\n{src}"
    );
    // The field keeps the derived spelling (the alias makes it resolve); the pinned name must NOT
    // leak into the emitted body as a bare reference.
    assert!(
        src.contains("FooBar"),
        "expected the derived `FooBar` to be used in the body, got:\n{src}"
    );
    assert!(
        !src.contains("use mydep::FooBar;"),
        "the un-aliased import must not appear (the pin must be honored), got:\n{src}"
    );
}

// The wasm crate references the dependency's NATIVE rust struct by full path (bypassing the `use`
// seam), so that path's leaf must be the pinned name too: `mydep::CustomName`, never
// `mydep::FooBar`. Exercises the `rust_crate_struct_from_wasm` bypass site.
#[test]
fn rust_name_pin_applies_to_wasm_full_path() {
    let map = generate_dir(
        &[
            ("main.cddl", "outer = { x: foo_bar }"),
            (
                "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/mydep/mod.cddl",
                "foo_bar = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name CustomName",
            ),
        ],
        &[],
        true,
        "pin_wasm_path",
    )
    .expect("wasm generation should succeed");
    let src = joined(&map);
    assert!(
        src.contains("mydep::CustomName"),
        "expected the pinned native full path `mydep::CustomName` in wasm output, got:\n{src}"
    );
    assert!(
        !src.contains("mydep::FooBar"),
        "the un-pinned native full path `mydep::FooBar` must not appear, got:\n{src}"
    );
}

// `@rust_name` on a normally-generated (exported) rule is a graceful rejection: the consumer IS the
// codegen version that spells the name there, so a pin would silently do nothing.
#[test]
fn rust_name_on_exported_rule_rejected() {
    let err = generate_dir(
        &[("main.cddl", "outer = uint ; @rust_name Renamed")],
        &[],
        false,
        "pin_exported_reject",
    )
    .expect_err("a pin on an exported rule must be rejected");
    assert!(
        err.contains("@rust_name") && err.contains("extern-interface"),
        "expected an extern-interface-reserved rejection, got:\n{err}"
    );
}

// A pin that camel-cases to a reserved Rust std/prelude type (`Option`) is rejected exactly as a
// derived name would be — a dependency could never have emitted a type by that name, so the pin can
// never be honored.
#[test]
fn rust_name_reserved_pin_rejected() {
    let err = generate_dir(
        &[
            ("main.cddl", "outer = { x: foo_bar }"),
            (
                "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/mydep/mod.cddl",
                "foo_bar = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Option",
            ),
        ],
        &[],
        false,
        "pin_reserved_reject",
    )
    .expect_err("a pin to a reserved Rust type must be rejected");
    assert!(
        err.contains("@rust_name") && err.contains("reserved"),
        "expected a reserved-pin rejection, got:\n{err}"
    );
}
