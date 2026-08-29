//! Fast generation-only leg for the multifile shapes excluded from the standalone compile matrix.
//!
//! See `cddl-matrix/project_multifile_excluded_matrix.ts`: extern and raw-bytes inputs need
//! consumer-owned Rust definitions, so compiling them here would prove fixture plumbing instead of
//! scope routing.  The projected grid nevertheless reaches their named-field, alias-target, and
//! unreferenced-owner positions through `api::generated_strings`.

use clap::Parser;

use crate::cli::Cli;

const MATRIX_DIR: &str = "tests/matrix_multifile_excluded";
const EXPECTED_CELLS: &[&str] = &[
    "extern__aliased",
    "extern__named",
    "extern__unref",
    "generic_extern_plain__aliased",
    "generic_extern_plain__named",
    "generic_extern_plain__unref",
    "generic_extern_rawbytes__aliased",
    "generic_extern_rawbytes__named",
    "generic_extern_rawbytes__unref",
    "rawbytes__aliased",
    "rawbytes__named",
    "rawbytes__unref",
];

struct ReferencedCellExpectation {
    stem: &'static str,
    // A type line in module b: the imported spelling must remain used, not merely present.
    type_surface: &'static str,
    // The complete import line proves both the imported identifier and its owner path.
    import_line: &'static str,
}

const REFERENCED_CELL_EXPECTATIONS: &[ReferencedCellExpectation] = &[
    ReferencedCellExpectation {
        stem: "extern__aliased",
        type_surface: "pub type Bal = Ext;",
        import_line: "use crate::generated::a::Ext;",
    },
    ReferencedCellExpectation {
        stem: "extern__named",
        type_surface: "pub field0: Ext,",
        import_line: "use crate::generated::a::Ext;",
    },
    ReferencedCellExpectation {
        stem: "rawbytes__aliased",
        type_surface: "pub type Bal = PubKey;",
        import_line: "use crate::generated::a::PubKey;",
    },
    ReferencedCellExpectation {
        stem: "rawbytes__named",
        type_surface: "pub field0: PubKey,",
        import_line: "use crate::generated::a::PubKey;",
    },
    ReferencedCellExpectation {
        stem: "generic_extern_plain__aliased",
        type_surface: "pub type Bal = ExtSet<Plain>;",
        import_line: "use crate::generated::a::{ExtSet, Plain};",
    },
    ReferencedCellExpectation {
        stem: "generic_extern_plain__named",
        type_surface: "pub field0: ExtSetPlain,",
        import_line: "use crate::generated::ExtSetPlain;",
    },
    ReferencedCellExpectation {
        stem: "generic_extern_rawbytes__aliased",
        type_surface: "pub type Bal = ExtSetRawBytes<PubKey>;",
        import_line: "use crate::generated::a::{ExtSetRawBytes, PubKey};",
    },
    ReferencedCellExpectation {
        stem: "generic_extern_rawbytes__named",
        type_surface: "pub field0: ExtSetPubKey,",
        import_line: "use crate::generated::ExtSetPubKey;",
    },
];

fn cli_for(input: &std::path::Path) -> Cli {
    Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().expect("UTF-8 matrix path"),
        "--output",
        "multifile_excluded_matrix_unused",
        "--wasm=false",
    ])
}

/// The compile/round-trip matrix deliberately excludes user-supplied extern/raw-bytes shapes, but
/// their scope traversal still has to generate correctly.  In particular, an extern generic alias
/// renders `Base<Args>` in the alias line while importing the base and every argument separately.
#[test]
fn multifile_excluded_shape_matrix_generates() {
    let cells: std::collections::BTreeSet<String> = std::fs::read_dir(MATRIX_DIR)
        .expect("projected excluded-shape matrix directory")
        .map(|entry| entry.expect("matrix directory entry"))
        .filter(|entry| entry.file_type().expect("matrix entry type").is_dir())
        .map(|entry| {
            entry
                .file_name()
                .into_string()
                .expect("UTF-8 matrix cell name")
        })
        .collect();
    let expected: std::collections::BTreeSet<String> = EXPECTED_CELLS
        .iter()
        .map(|name| (*name).to_owned())
        .collect();
    assert_eq!(
        cells, expected,
        "excluded-shape matrix membership drifted — update the projection and this exact test pin together"
    );
    let referenced: std::collections::BTreeSet<&str> = EXPECTED_CELLS
        .iter()
        .copied()
        .filter(|stem| !stem.ends_with("__unref"))
        .collect();
    let asserted: std::collections::BTreeSet<&str> = REFERENCED_CELL_EXPECTATIONS
        .iter()
        .map(|expectation| expectation.stem)
        .collect();
    assert_eq!(
        asserted, referenced,
        "every named/aliased excluded-shape cell must have an exact b-module type/import assertion"
    );

    for stem in EXPECTED_CELLS {
        let input = std::path::Path::new(MATRIX_DIR).join(stem);
        let files = crate::api::generated_strings(&cli_for(&input)).unwrap_or_else(|error| {
            panic!("excluded-shape matrix cell {stem} must generate: {error}")
        });

        // This is the class-level guard for feature request 07. A type expression is legal in a
        // type alias but never in a Rust `use`/`pub use` declaration; checking every generated Rust
        // surface makes a future new scope fail locally instead of relying on rustfmt's indirect
        // error.
        for (path, content) in files.iter().filter(|(path, _)| path.starts_with("rust/")) {
            for line in content
                .lines()
                .map(str::trim)
                .filter(|line| line.starts_with("use ") || line.starts_with("pub use "))
            {
                assert!(
                    !line.contains('<') && !line.contains('>'),
                    "{stem}: generated Rust use line in {path} carries a generic type expression: {line}"
                );
            }
        }

        if let Some(expectation) = REFERENCED_CELL_EXPECTATIONS
            .iter()
            .find(|expectation| expectation.stem == *stem)
        {
            let module = files
                .get("rust/src/generated/b/mod.rs")
                .expect("referencing module must be emitted");
            assert!(
                module.contains(expectation.type_surface),
                "{stem}: module b must retain expected type surface `{}`:\n{module}",
                expectation.type_surface,
            );
            assert!(
                module
                    .lines()
                    .map(str::trim)
                    .any(|line| line == expectation.import_line),
                "{stem}: module b must import its expected type from the exact owner path `{}`:\n{module}",
                expectation.import_line,
            );
        }
    }
}
