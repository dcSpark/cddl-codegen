//! Tests for the `--config <file.toml>` front end (`src/config.rs`).
//!
//! The feature's whole claim is "a config key IS its flag", so the suite is organised around the
//! three ways that claim can break: a key that does not reach the flag (the merge and the
//! expansion), a key that reaches the WRONG flag value (path resolution, the negated switch), and a
//! key that exists on one side only (the drift gate). Everything else — what a flag MEANS — is
//! already covered wherever that flag is covered, and is deliberately not re-tested here.

use crate::cli::Cli;
use crate::config::{self, Settings};
use std::path::Path;

/// Parse config text with no base directory, so a relative path key expands to itself and the
/// expansion can be compared against a hand-written flag list character for character.
fn parse(text: &str) -> config::Config {
    config::parse_str(text, Path::new("")).unwrap_or_else(|e| panic!("config must parse: {e}"))
}

fn expand_one(text: &str) -> Cli {
    let config = parse(text);
    let mut expanded = config
        .expand(&[])
        .unwrap_or_else(|e| panic!("must expand: {e}"));
    assert_eq!(expanded.len(), 1, "fixture declares exactly one crate");
    expanded.remove(0).1
}

fn error(text: &str) -> String {
    config::parse_str(text, Path::new(""))
        .err()
        .unwrap_or_else(|| panic!("config must be rejected:\n{text}"))
}

/// The minimum a crate table must carry, so a fixture can be about the key under test.
const MINIMAL_CRATE: &str = "\n[crates.demo]\ninput = \"spec.cddl\"\noutput = \"gen\"\n";

// ---------------------------------------------------------------------------------------------
// D3 — the merge
// ---------------------------------------------------------------------------------------------

/// Built-in → `[defaults]` → profiles in the crate's listed order → the crate's own keys, with the
/// later layer winning. Every layer boundary is exercised by a key that ONLY that boundary decides,
/// so a merge that collapsed two layers into one would fail rather than coincidentally pass.
#[test]
fn merge_applies_defaults_then_profiles_in_order_then_crate_keys() {
    let cli = expand_one(
        r#"
[defaults]
preserve-encodings = true
canonical-form = true
wasm = true
rust-wasm-feature = "from-defaults"

[profiles.first]
canonical-form = false
rust-wasm-feature = "from-first"

[profiles.second]
rust-wasm-feature = "from-second"

[crates.demo]
input = "spec.cddl"
output = "gen"
profiles = ["first", "second"]
wasm = false
"#,
    );
    // Untouched by any later layer: `[defaults]` reaches the crate.
    assert!(cli.preserve_encodings, "a defaults-only key must apply");
    // A profile overrides `[defaults]`.
    assert!(!cli.canonical_form, "a profile must beat [defaults]");
    // Profiles apply in the crate's LISTED order, not table order — `second` is listed last and
    // both profiles set this key, so listing them the other way round must give the other answer.
    assert_eq!(cli.rust_wasm_feature, "from-second");
    // The crate's own key beats everything.
    assert!(!cli.wasm, "a crate key must beat a profile and [defaults]");
    // Never mentioned anywhere: clap's built-in default survives as the base layer, which is the
    // reason expansion goes through clap rather than constructing `Cli` (a derived `Default` would
    // give `""` here, and `false` for `--annotate-fields`).
    assert_eq!(cli.lib_name, "demo");
    assert!(cli.annotate_fields);
}

/// Reversing the listed profile order reverses the answer — the assertion above is about ORDER, not
/// about which profile happens to sort last.
#[test]
fn profile_application_follows_the_crates_listed_order() {
    let template = |order: &str| {
        format!(
            r#"
[profiles.first]
rust-wasm-feature = "from-first"

[profiles.second]
rust-wasm-feature = "from-second"

[crates.demo]
input = "spec.cddl"
output = "gen"
profiles = [{order}]
"#
        )
    };
    assert_eq!(
        expand_one(&template("\"first\", \"second\"")).rust_wasm_feature,
        "from-second"
    );
    assert_eq!(
        expand_one(&template("\"second\", \"first\"")).rust_wasm_feature,
        "from-first"
    );
}

/// Arrays CONCATENATE across layers, earlier layers first, author order preserved within each.
/// `--json-schema-root` is order-significant (roots emit after every spec-derived row, IN FLAG
/// ORDER), so this pins the emitted order end to end — and it pins that a crate adding one root does
/// not silently discard the shared list `[defaults]` exists to hold.
#[test]
fn array_keys_concatenate_across_layers_in_layer_order() {
    let cli = expand_one(
        r#"
[defaults]
json-schema-root = ["d::One", "d::Two"]
workspace-dep = ["dep_a"]

[profiles.extra]
json-schema-root = ["p::Three"]
workspace-dep = ["dep_b"]

[crates.demo]
input = "spec.cddl"
output = "gen"
profiles = ["extra"]
json-schema-root = ["c::Four"]
workspace-dep = ["dep_c"]
"#,
    );
    assert_eq!(
        cli.json_schema_root,
        vec!["d::One", "d::Two", "p::Three", "c::Four"]
    );
    assert_eq!(cli.workspace_dep, vec!["dep_a", "dep_b", "dep_c"]);
}

/// `<k>=<v>` sub-tables union per key and a later layer overrides only the keys it names — the same
/// accumulation a repeated `<k>=<v>` flag already gets by landing in a `BTreeMap`.
#[test]
fn sub_tables_union_per_key_with_the_later_layer_winning_per_key() {
    let cli = expand_one(
        r#"
[defaults.extern-wasm-crate]
core = "core_wasm"
shared = "shared_wasm"

[profiles.p.extern-wasm-crate]
shared = "profile_shared_wasm"
extra = "extra_wasm"

[crates.demo]
input = "spec.cddl"
output = "gen"
profiles = ["p"]

[crates.demo.extern-wasm-crate]
extra = "crate_extra_wasm"
"#,
    );
    // Emitted in `BTreeMap` key order, so the flag sequence is a function of the config's content
    // and not of which layer happened to mention a key first.
    assert_eq!(
        cli.extern_wasm_crate,
        vec![
            // `core` survives from [defaults] — a later layer that does not name a key leaves it.
            "core=core_wasm",
            // the crate overrode the profile for this key only
            "extra=crate_extra_wasm",
            // the profile overrode [defaults] for this key only
            "shared=profile_shared_wasm",
        ]
    );
}

// ---------------------------------------------------------------------------------------------
// D2 — the schema, and what it refuses
// ---------------------------------------------------------------------------------------------

/// A typoed key is a hard error naming it, at every level. This is the whole reason
/// `deny_unknown_fields` is on: a key that silently fell back to a default would ship a crate built
/// with the wrong flag set, whereas a misspelled CLI flag fails before anything is generated.
#[test]
fn an_unknown_key_is_a_hard_error_naming_it() {
    let in_crate =
        error("[crates.demo]\ninput = \"spec.cddl\"\noutput = \"gen\"\npreserv-encodings = true\n");
    assert!(
        in_crate.contains("preserv-encodings") && in_crate.contains("[crates.demo]"),
        "must name the key and the table, got: {in_crate}"
    );

    let in_defaults = error(&format!("[defaults]\nwsam = true\n{MINIMAL_CRATE}"));
    assert!(
        in_defaults.contains("wsam") && in_defaults.contains("[defaults]"),
        "must name the key and the table, got: {in_defaults}"
    );

    let in_profile = error(&format!(
        "[profiles.published]\npackge-json = true\n{MINIMAL_CRATE}"
    ));
    assert!(
        in_profile.contains("packge-json") && in_profile.contains("[profiles.published]"),
        "must name the key and the table, got: {in_profile}"
    );

    // A top-level table serde never reaches, including the ones later phases will add: a config
    // written against a newer version must say so rather than silently ignore the table.
    let top_level = error(&format!(
        "[runtime]\ncommon-import = \"rt\"\n{MINIMAL_CRATE}"
    ));
    assert!(
        top_level.contains("runtime") && top_level.contains("[crates]"),
        "must name the unknown table and what is understood, got: {top_level}"
    );
}

/// A per-crate-only key in a shared table is a hard error naming the key and the table. serde would
/// report these as "unknown field", which is true but unhelpful — the key is real, it is in the
/// wrong table — so they are split out and rejected with the reason.
#[test]
fn per_crate_only_keys_are_rejected_in_shared_tables() {
    for key in ["input", "output", "lib-name"] {
        for table in ["[defaults]", "[profiles.p]"] {
            let text = format!("{table}\n{key} = \"x\"\n{MINIMAL_CRATE}");
            let err = error(&text);
            assert!(
                err.contains(key) && err.contains(table),
                "{table}.{key} must be rejected naming both, got: {err}"
            );
        }
    }
    // `profiles` in a profile is how "a profile includes a profile" would be spelled; profiles are
    // flat, so it is refused by the same rule rather than silently ignored.
    let nested = error(&format!(
        "[profiles.p]\nprofiles = [\"q\"]\n[profiles.q]\nwasm = false\n{MINIMAL_CRATE}"
    ));
    assert!(
        nested.contains("profiles") && nested.contains("[profiles.p]"),
        "a profile referencing a profile must be refused, got: {nested}"
    );
}

/// A profile name with no table, or listed twice, is a hard error naming it. The first is a typo
/// that would otherwise apply nothing; the second cannot mean anything a single mention does not,
/// so accepting it would hide a copy-paste.
#[test]
fn bad_profile_references_are_hard_errors() {
    let unknown = error(
        "[profiles.published]\npackage-json = true\n\
         [crates.demo]\ninput = \"s\"\noutput = \"g\"\nprofiles = [\"publishd\"]\n",
    );
    assert!(
        unknown.contains("publishd") && unknown.contains("published"),
        "must name the bad reference and the configured profiles, got: {unknown}"
    );

    let repeated = error(
        "[profiles.p]\nwasm = false\n\
         [crates.demo]\ninput = \"s\"\noutput = \"g\"\nprofiles = [\"p\", \"p\"]\n",
    );
    assert!(
        repeated.contains('p') && repeated.contains("twice"),
        "must name the repeat, got: {repeated}"
    );
}

/// A config that generates nothing is a mistake, not an empty run.
#[test]
fn a_config_without_crates_is_a_hard_error() {
    for text in ["[defaults]\nwasm = false\n", "[crates]\n"] {
        let err = error(text);
        assert!(
            err.contains("crates"),
            "must say a config generates at least one crate, got: {err}"
        );
    }
}

/// `lib-name` defaults to the crate table key — the one place the config is LESS repetitive than the
/// CLI, where `--lib-name` defaults to `cddl-lib` and so realistically always needs passing.
#[test]
fn lib_name_defaults_to_the_crate_table_key_and_is_overridable() {
    assert_eq!(expand_one(MINIMAL_CRATE).lib_name, "demo");
    assert_eq!(
        expand_one("[crates.demo]\ninput = \"s\"\noutput = \"g\"\nlib-name = \"my-lib\"\n")
            .lib_name,
        "my-lib"
    );
}

// ---------------------------------------------------------------------------------------------
// D5 — expansion
// ---------------------------------------------------------------------------------------------

/// The load-bearing unit test: a config setting EVERY key expands to exactly the `Cli` the
/// equivalent flag list parses to.
///
/// Both sides are destructured exhaustively, so a new `Cli` field fails this test at COMPILE time —
/// which is what stops a field being added with a config key that never reaches it. The drift gate
/// (`config_keys_match_cli_fields`) catches the missing KEY; this catches the missing wiring.
#[test]
fn expansion_equals_the_equivalent_hand_written_flag_list() {
    use clap::Parser;

    let from_config = expand_one(
        r#"
[crates.demo]
input = "spec.cddl"
output = "gen/demo"
lib-name = "demo-lib"
static-dir = "vendor/static"
export-static-crate = "crates/runtime"
annotate-fields = false
to-from-bytes-methods = false
binary-wrappers = true
preserve-encodings = true
canonical-form = true
wasm = false
json-serde-derives = true
emit-tests = true
emit-tests-conformance = true
json-schema-export = true
package-json = true
json-schema-scripts = true
no-synthesized-rust-collection-aliases = true
preserve-comments = false
rust-wasm-feature = "wasm-bindings"
deserialize-depth-limit = 128
common-import-override = "my_runtime"
wasm-cbor-json-api-macro = "foo::bar::cbor_api"
wasm-conversions-macro = "foo::bar::conv"
wasm-list-macro = "foo::bar::list"
json-schema-root = ["demo_lib::hand::Address", "demo_lib::hand::Key"]
workspace-dep = ["core"]

[crates.demo.extern-import]
core = "../core/extern-interface/core"

[crates.demo.extern-wasm-crate]
core = "core_wasm"

[crates.demo.extern-wrapper-index]
core = "../core/wasm/src/generated/collections.rs"

[crates.demo.wrapper-requests]
ledger = "../ledger/wasm/src/generated/borrowed_collections.rs"

[crates.demo.key-requests]
ledger = "../ledger/rust/src/generated/borrowed_key_types.rs"

[crates.demo.json-schema-dep]
core = "core_json_schema_gen"
"#,
    );

    let from_flags = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "spec.cddl",
        "--output",
        "gen/demo",
        "--lib-name",
        "demo-lib",
        "--static-dir",
        "vendor/static",
        "--export-static-crate",
        "crates/runtime",
        "--annotate-fields",
        "false",
        "--to-from-bytes-methods",
        "false",
        "--binary-wrappers",
        "true",
        "--preserve-encodings",
        "true",
        "--canonical-form",
        "true",
        "--wasm",
        "false",
        "--json-serde-derives",
        "true",
        "--emit-tests",
        "true",
        "--emit-tests-conformance",
        "true",
        "--json-schema-export",
        "true",
        "--package-json",
        "true",
        "--json-schema-scripts",
        "true",
        "--no-synthesized-rust-collection-aliases",
        "true",
        "--no-preserve-comments",
        "--rust-wasm-feature",
        "wasm-bindings",
        "--deserialize-depth-limit",
        "128",
        "--common-import-override",
        "my_runtime",
        "--wasm-cbor-json-api-macro",
        "foo::bar::cbor_api",
        "--wasm-conversions-macro",
        "foo::bar::conv",
        "--wasm-list-macro",
        "foo::bar::list",
        "--json-schema-root",
        "demo_lib::hand::Address",
        "--json-schema-root",
        "demo_lib::hand::Key",
        "--workspace-dep",
        "core",
        "--extern-import",
        "core=../core/extern-interface/core",
        "--extern-wasm-crate",
        "core=core_wasm",
        "--extern-wrapper-index",
        "core=../core/wasm/src/generated/collections.rs",
        "--wrapper-requests",
        "ledger=../ledger/wasm/src/generated/borrowed_collections.rs",
        "--key-requests",
        "ledger=../ledger/rust/src/generated/borrowed_key_types.rs",
        "--json-schema-dep",
        "core=core_json_schema_gen",
    ]);

    // Exhaustive on purpose — see the doc comment.
    let Cli {
        input,
        output,
        static_dir,
        lib_name,
        annotate_fields,
        to_from_bytes_methods,
        binary_wrappers,
        preserve_encodings,
        canonical_form,
        wasm,
        rust_wasm_feature,
        json_serde_derives,
        emit_tests,
        emit_tests_conformance,
        deserialize_depth_limit,
        json_schema_export,
        package_json,
        json_schema_scripts,
        json_schema_root,
        json_schema_dep,
        common_import_override,
        wasm_cbor_json_api_macro,
        wasm_conversions_macro,
        preserve_comments,
        wasm_list_macro,
        no_synthesized_rust_collection_aliases,
        extern_wasm_crate,
        extern_wrapper_index,
        workspace_dep,
        wrapper_requests,
        key_requests,
        extern_import,
        export_static_crate,
    } = from_config;

    assert_eq!(input, from_flags.input);
    assert_eq!(output, from_flags.output);
    assert_eq!(static_dir, from_flags.static_dir);
    assert_eq!(lib_name, from_flags.lib_name);
    assert_eq!(annotate_fields, from_flags.annotate_fields);
    assert_eq!(to_from_bytes_methods, from_flags.to_from_bytes_methods);
    assert_eq!(binary_wrappers, from_flags.binary_wrappers);
    assert_eq!(preserve_encodings, from_flags.preserve_encodings);
    assert_eq!(canonical_form, from_flags.canonical_form);
    assert_eq!(wasm, from_flags.wasm);
    assert_eq!(rust_wasm_feature, from_flags.rust_wasm_feature);
    assert_eq!(json_serde_derives, from_flags.json_serde_derives);
    assert_eq!(emit_tests, from_flags.emit_tests);
    assert_eq!(emit_tests_conformance, from_flags.emit_tests_conformance);
    assert_eq!(deserialize_depth_limit, from_flags.deserialize_depth_limit);
    assert_eq!(json_schema_export, from_flags.json_schema_export);
    assert_eq!(package_json, from_flags.package_json);
    assert_eq!(json_schema_scripts, from_flags.json_schema_scripts);
    assert_eq!(json_schema_root, from_flags.json_schema_root);
    assert_eq!(json_schema_dep, from_flags.json_schema_dep);
    assert_eq!(common_import_override, from_flags.common_import_override);
    assert_eq!(
        wasm_cbor_json_api_macro,
        from_flags.wasm_cbor_json_api_macro
    );
    assert_eq!(wasm_conversions_macro, from_flags.wasm_conversions_macro);
    assert_eq!(preserve_comments, from_flags.preserve_comments);
    assert!(
        !preserve_comments,
        "`preserve-comments = false` must reach the negated flag"
    );
    assert_eq!(wasm_list_macro, from_flags.wasm_list_macro);
    assert_eq!(
        no_synthesized_rust_collection_aliases,
        from_flags.no_synthesized_rust_collection_aliases
    );
    assert_eq!(extern_wasm_crate, from_flags.extern_wasm_crate);
    assert_eq!(extern_wrapper_index, from_flags.extern_wrapper_index);
    assert_eq!(workspace_dep, from_flags.workspace_dep);
    assert_eq!(wrapper_requests, from_flags.wrapper_requests);
    assert_eq!(key_requests, from_flags.key_requests);
    assert_eq!(extern_import, from_flags.extern_import);
    assert_eq!(export_static_crate, from_flags.export_static_crate);
}

/// Omitting `preserve-comments` leaves the built-in on: the key is a positive boolean over a negated
/// flag, and getting that inversion backwards would silently disable comment preservation for every
/// config user.
#[test]
fn preserve_comments_omitted_or_true_leaves_the_built_in_alone() {
    assert!(expand_one(MINIMAL_CRATE).preserve_comments);
    assert!(
        expand_one("[crates.demo]\ninput = \"s\"\noutput = \"g\"\npreserve-comments = true\n")
            .preserve_comments
    );
}

/// A value clap rejects is reported against the TOML key that produced it, not against a flag the
/// user never typed. The rejection itself is clap's — the config must not be able to bypass a value
/// parser (here `parse_json_schema_root`'s emitted-verbatim charset guard).
#[test]
fn a_rejected_value_is_reported_against_its_config_key() {
    let config = parse(
        "[crates.demo]\ninput = \"s\"\noutput = \"g\"\njson-schema-root = [\"a::B; let x = 1\"]\n",
    );
    let err = config.expand(&[]).expect_err("clap must reject the value");
    assert!(
        err.contains("[crates.demo].json-schema-root"),
        "must point at the config key, got: {err}"
    );
    assert!(
        err.contains("invalid character"),
        "must carry clap's own reason, got: {err}"
    );
}

// ---------------------------------------------------------------------------------------------
// D4 — path resolution
// ---------------------------------------------------------------------------------------------

/// Path keys resolve against the CONFIG FILE's directory, never the process CWD.
///
/// Discriminating by construction: the config sits in a temp directory and names `tests/core` — a
/// path that ALSO exists relative to the process CWD (the repo root, where the real corpus fixture
/// lives) and resolves to different content there. A CWD-relative implementation therefore does not
/// merely fail, it succeeds with the wrong file, which is exactly the `--static-dir`-against-CWD
/// trap this key resolution retires. Scope: proven for `input`/`output`/`static-dir` and one
/// sub-table right-hand side; the remaining path keys share the one `resolve_path` call site.
#[test]
fn path_keys_resolve_against_the_config_file_not_the_process_cwd() {
    let dir = std::env::temp_dir().join(format!("cddl_config_paths_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("tests/core")).unwrap();
    // Same RELATIVE path as a real repo fixture, different content — so "which one did we read" is
    // observable rather than a matter of trust.
    std::fs::write(
        dir.join("tests/core/input.cddl"),
        "from_the_config_dir = uint\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("codegen.toml"),
        "[crates.demo]\ninput = \"tests/core/input.cddl\"\noutput = \"gen\"\n\
         static-dir = \"vendor/static\"\n\n\
         [crates.demo.extern-import]\ncore = \"../sibling/extern-interface/core\"\n",
    )
    .unwrap();

    let config = config::load(&dir.join("codegen.toml")).unwrap();
    let cli = config.expand(&[]).unwrap().remove(0).1;

    assert_eq!(cli.input, dir.join("tests/core/input.cddl"));
    assert_eq!(cli.output, dir.join("gen"));
    assert_eq!(cli.static_dir, dir.join("vendor/static"));
    assert_eq!(
        cli.extern_import,
        vec![format!(
            "core={}",
            dir.join("../sibling/extern-interface/core").display()
        )]
    );
    // The repo-root-relative reading of the same string is a DIFFERENT, existing file — so the
    // assertions above discriminate rather than merely pass.
    assert_ne!(
        cli.input,
        std::path::PathBuf::from("tests/core/input.cddl"),
        "resolution must not fall back to the process CWD"
    );
    assert!(
        Path::new("tests/core/input.cddl").exists(),
        "the discriminating premise: the same relative path exists under the process CWD too"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// An absolute path passes through untouched — a config may name a vendored runtime by absolute
/// path, and joining it onto the config's directory would corrupt it.
#[test]
fn absolute_path_keys_pass_through_unjoined() {
    let config = config::parse_str(
        "[crates.demo]\ninput = \"/abs/spec.cddl\"\noutput = \"/abs/gen\"\n",
        Path::new("/some/config/dir"),
    )
    .unwrap();
    let cli = config.expand(&[]).unwrap().remove(0).1;
    assert_eq!(cli.input, Path::new("/abs/spec.cddl"));
    assert_eq!(cli.output, Path::new("/abs/gen"));
}

/// A key whose right-hand side is a NAME, not a path, must not be resolved: `--extern-wasm-crate`
/// names a crate and `--json-schema-dep` names a rust module path emitted verbatim into generated
/// code, so joining a directory onto either would emit an unparseable path into a rust file.
#[test]
fn name_valued_sub_table_sides_are_never_path_resolved() {
    let config = config::parse_str(
        "[crates.demo]\ninput = \"s\"\noutput = \"g\"\njson-schema-export = true\n\n\
         [crates.demo.extern-wasm-crate]\ncore = \"core_wasm\"\n\n\
         [crates.demo.json-schema-dep]\ncore = \"core_json_schema_gen\"\n",
        Path::new("/some/config/dir"),
    )
    .unwrap();
    let cli = config.expand(&[]).unwrap().remove(0).1;
    assert_eq!(cli.extern_wasm_crate, vec!["core=core_wasm"]);
    assert_eq!(cli.json_schema_dep, vec!["core=core_json_schema_gen"]);
}

// ---------------------------------------------------------------------------------------------
// D6 — CLI integration
// ---------------------------------------------------------------------------------------------

/// Any generation flag alongside `--config` is a hard error naming it. There is no precedence story
/// on purpose: every override would have to define whether it applies to one crate or all of them.
///
/// The offending-flag set is read out of `Cli`'s own clap `Command`, so this is checked across EVERY
/// flag rather than a sample — a flag added tomorrow is rejected without anyone updating a list.
#[test]
fn a_generation_flag_alongside_config_is_a_hard_error() {
    use clap::CommandFactory;

    for arg in Cli::command().get_arguments() {
        for long in arg.get_long_and_visible_aliases().unwrap_or_default() {
            for spelling in [format!("--{long}"), format!("--{long}=x")] {
                let argv = vec![
                    "cddl-codegen".to_owned(),
                    "--config".to_owned(),
                    "codegen.toml".to_owned(),
                    spelling.clone(),
                ];
                let Err(err) = config::reject_generation_flags(&argv) else {
                    panic!("`{spelling}` must be rejected alongside --config");
                };
                assert!(
                    err.contains(&format!("--{long}")) && err.contains("config file"),
                    "must name the flag and say where it belongs, got: {err}"
                );
            }
        }
        for short in arg.get_short_and_visible_aliases().unwrap_or_default() {
            let argv = vec![
                "cddl-codegen".to_owned(),
                "--config".to_owned(),
                "codegen.toml".to_owned(),
                format!("-{short}"),
            ];
            if config::reject_generation_flags(&argv).is_ok() {
                panic!("-{short} must be rejected alongside --config");
            }
        }
    }

    // Positional crate names and the config path itself are the only things that belong here.
    let clean = vec![
        "cddl-codegen".to_owned(),
        "--config".to_owned(),
        "codegen.toml".to_owned(),
        "ledger".to_owned(),
        "core".to_owned(),
    ];
    config::reject_generation_flags(&clean).expect("a bare selector list is not a generation flag");
}

/// Both spellings clap accepts for a valued flag select config mode; nothing else does.
#[test]
fn config_mode_is_detected_from_either_flag_spelling() {
    let argv = |args: &[&str]| {
        std::iter::once("cddl-codegen")
            .chain(args.iter().copied())
            .map(str::to_owned)
            .collect::<Vec<_>>()
    };
    assert!(config::is_config_mode(&argv(&["--config", "c.toml"])));
    assert!(config::is_config_mode(&argv(&["--config=c.toml"])));
    assert!(!config::is_config_mode(&argv(&[
        "--input", "s.cddl", "--output", "g"
    ])));
    // The binary's own path is not scanned — a checkout literally named `--config` would otherwise
    // put every invocation into config mode.
    assert!(!config::is_config_mode(&["--config".to_owned()]));
}

/// Positional names select a SUBSET; an unknown one is a hard error listing what is configured,
/// because a typo would otherwise generate nothing and exit 0. The selection picks WHICH crates run,
/// never in what order — so two orderings of the same selection expand identically.
#[test]
fn positional_crate_names_select_a_subset_in_config_order() {
    let config = parse(
        "[crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n\
         [crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\n\
         [crates.gamma]\ninput = \"c.cddl\"\noutput = \"gc\"\n",
    );
    let names = |selected: &[&str]| {
        config
            .expand(&selected.iter().map(|s| s.to_string()).collect::<Vec<_>>())
            .unwrap()
            .into_iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>()
    };
    assert_eq!(names(&[]), vec!["alpha", "beta", "gamma"]);
    assert_eq!(names(&["gamma", "alpha"]), vec!["alpha", "gamma"]);
    assert_eq!(names(&["alpha", "gamma"]), vec!["alpha", "gamma"]);

    let err = config
        .expand(&["alfa".to_owned()])
        .expect_err("an unknown crate name must be refused");
    assert!(
        err.contains("alfa") && err.contains("alpha") && err.contains("beta"),
        "must name the bad selector and list the configured crates, got: {err}"
    );
}

// ---------------------------------------------------------------------------------------------
// D8.10 — the acceptance test
// ---------------------------------------------------------------------------------------------

/// End to end: a single-crate config generates byte-identical output to the equivalent flag
/// invocation over a real corpus fixture.
///
/// This is what pins "config = flag expansion, nothing more" at the level that matters — the emitted
/// source — rather than at the `Cli` struct. Run over the `json` profile because it turns on the
/// widest set of emission paths (serde derives + the json-gen crate) of the shipped profiles.
#[test]
fn config_expansion_generates_byte_identical_output_to_the_flag_invocation() {
    use clap::Parser;

    let input = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/core/input.cddl");
    let static_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/static");

    let from_flags = Cli::parse_from([
        "cddl-codegen",
        "--input",
        input,
        "--output",
        "config_acceptance_unused",
        "--lib-name",
        "core",
        "--static-dir",
        static_dir,
        "--json-serde-derives",
        "true",
        "--json-schema-export",
        "true",
    ]);
    let config = config::parse_str(
        &format!(
            "[defaults]\nstatic-dir = \"{static_dir}\"\njson-serde-derives = true\n\
             json-schema-export = true\n\n\
             [crates.core]\ninput = \"{input}\"\noutput = \"config_acceptance_unused\"\n"
        ),
        Path::new(""),
    )
    .unwrap();
    let from_config = config.expand(&[]).unwrap().remove(0).1;

    let flag_files = crate::api::generated_strings(&from_flags).expect("flag run must generate");
    let config_files =
        crate::api::generated_strings(&from_config).expect("config run must generate");
    assert_eq!(
        flag_files.keys().collect::<Vec<_>>(),
        config_files.keys().collect::<Vec<_>>(),
        "config and flag runs must emit the same file set"
    );
    for (path, flag_content) in &flag_files {
        assert_eq!(
            flag_content, &config_files[path],
            "config and flag runs differ in {path}"
        );
    }
    assert!(
        !flag_files.is_empty(),
        "the fixture must generate something"
    );
}

// ---------------------------------------------------------------------------------------------
// D7 — the drift gate
// ---------------------------------------------------------------------------------------------

/// Every `Cli` field has a config key and every config key is a `Cli` field.
///
/// This is what makes "no knob exists only in the config, and no flag is unreachable from it"
/// mechanical rather than a promise: a flag added to `Cli` without a `Settings` field fails here, and
/// so does a key invented in the config with no flag behind it. Both structs are read from SOURCE
/// (via `syn`, the same way the harness-side differentials read generated code) so the gate cannot be
/// satisfied by anything but the real declarations.
///
/// Exclusions, and why each is not drift:
/// - `input` / `output` / `lib-name` are `Cli` fields but live on the per-crate entry rather than on
///   `Settings`, because a shared value for any of them would point every crate at one spec, one
///   directory, or one library. They are checked as present-on-the-crate-entry instead.
/// - `profiles` and `deps` are config keys with no `Cli` field: one selects which shared layers a
///   crate applies, the other declares an edge to another crate. Both are the config's own
///   structure — they have no flag equivalent by construction, and neither is on `Settings`, so
///   neither reaches `config_keys` in the first place.
/// - `--config` itself is on `ConfigCli`, not `Cli`, so it needs no exclusion — it is not a
///   generation flag and cannot be set from inside a config file.
#[test]
fn config_keys_match_cli_fields() {
    use std::collections::BTreeSet;

    fn struct_field_keys(path: &str, struct_name: &str) -> BTreeSet<String> {
        let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
        let file = syn::parse_file(&source).unwrap_or_else(|e| panic!("parse {path}: {e}"));
        let item = file
            .items
            .iter()
            .find_map(|item| match item {
                syn::Item::Struct(s) if s.ident == struct_name => Some(s),
                _ => None,
            })
            .unwrap_or_else(|| panic!("{path} declares no `struct {struct_name}`"));
        let keys: BTreeSet<String> = item
            .fields
            .iter()
            .filter_map(|f| f.ident.as_ref())
            .map(|ident| ident.to_string().replace('_', "-"))
            .collect();
        assert!(
            !keys.is_empty(),
            "parsed zero fields from `{struct_name}` in {path} — the source parse drifted and this \
             gate went vacuous"
        );
        keys
    }

    let cli_keys = struct_field_keys(concat!(env!("CARGO_MANIFEST_DIR"), "/src/cli.rs"), "Cli");
    let settings_keys = struct_field_keys(
        concat!(env!("CARGO_MANIFEST_DIR"), "/src/config.rs"),
        "Settings",
    );
    let per_crate: BTreeSet<String> = ["input", "output", "lib-name"]
        .into_iter()
        .map(str::to_owned)
        .collect();

    let config_keys: BTreeSet<String> = settings_keys.union(&per_crate).cloned().collect();
    if let Some(missing) = cli_keys.difference(&config_keys).next() {
        panic!(
            "`Cli` field `{missing}` has no config key — every flag must be settable from a config \
             file (proposal principle: keys mirror flags 1:1). Add `{}` to `config::Settings` and \
             emit it from `argv_fragments`.",
            missing.replace('-', "_")
        );
    }
    if let Some(extra) = config_keys.difference(&cli_keys).next() {
        panic!(
            "config key `{extra}` has no `Cli` field — no knob may exist only in the config. Either \
             add the flag to `Cli` or remove the key. (`profiles` and `deps` are the structural keys \
             and are excluded here by construction, not by this list.)"
        );
    }
}

/// The exclusion list above is only honest if the per-crate keys really are absent from `Settings`;
/// otherwise a key could be settable in `[defaults]` while the gate still passed.
#[test]
fn per_crate_keys_are_absent_from_the_shared_settings_struct() {
    let settings = Settings::default();
    let rendered = format!("{settings:?}");
    for key in ["input", "output", "lib_name", "profiles", "deps"] {
        assert!(
            !rendered.contains(&format!("{key}:")),
            "`{key}` must not be a `Settings` field — it would become settable in [defaults]"
        );
    }
}

// ---------------------------------------------------------------------------------------------
// The graph — `deps`
// ---------------------------------------------------------------------------------------------

/// The load-bearing graph test: one `deps` edge expands to exactly the flag values a hand-written
/// two-invocation shell script spells, on BOTH sides of it.
///
/// Both sides are asserted against a `Cli::parse_from` of the flags that script passes, so the test
/// pins the flag VALUES rather than the derivation's internal spelling — a derivation that produced a
/// path the tool would reject, or one the emission site does not write, fails here.
#[test]
fn a_deps_edge_expands_to_the_hand_written_flag_values_in_both_directions() {
    use clap::Parser;

    let config = parse(
        r#"
[crates.core]
input = "specs/core.cddl"
output = "gen/core"

[crates.ledger]
input = "specs/ledger.cddl"
output = "gen/ledger"
deps = ["core"]
"#,
    );
    let expanded = config.expand(&[]).expect("must expand");
    let by_name: std::collections::BTreeMap<String, Cli> = expanded.into_iter().collect();

    // The CONSUMER's side: everything it needs to resolve the dependency's types.
    let ledger = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "specs/ledger.cddl",
        "--output",
        "gen/ledger",
        "--lib-name",
        "ledger",
        "--extern-import",
        "core=gen/core/extern-interface/core",
        "--extern-wasm-crate",
        "core=core_wasm",
        "--extern-wrapper-index",
        "core=gen/core/wasm/src/generated/collections.rs",
        "--workspace-dep",
        "core",
    ]);
    assert_eq!(by_name["ledger"].extern_import, ledger.extern_import);
    assert_eq!(
        by_name["ledger"].extern_wasm_crate,
        ledger.extern_wasm_crate
    );
    assert_eq!(
        by_name["ledger"].extern_wrapper_index,
        ledger.extern_wrapper_index
    );
    assert_eq!(by_name["ledger"].workspace_dep, ledger.workspace_dep);

    // The DEPENDENCY's side: the reverse edges, one per consumer.
    let core = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "specs/core.cddl",
        "--output",
        "gen/core",
        "--lib-name",
        "core",
        "--wrapper-requests",
        "ledger=gen/ledger/wasm/src/generated/borrowed_collections.rs",
        "--key-requests",
        "ledger=gen/ledger/rust/src/generated/borrowed_key_types.rs",
    ]);
    assert_eq!(by_name["core"].wrapper_requests, core.wrapper_requests);
    assert_eq!(by_name["core"].key_requests, core.key_requests);
    // The consumer never grows a reverse edge and the dependency never grows a forward one: an edge
    // is directed, and deriving both halves onto both crates would be a cycle in flag form.
    assert!(by_name["ledger"].wrapper_requests.is_empty());
    assert!(by_name["core"].extern_import.is_empty());
}

/// Every cross-crate name is the dependency's `lib-name` NORMALISED (`-` -> `_`), because that single
/// spelling is simultaneously the directory its export lands in and the crate name the generated
/// `use` line carries — they are not independently choosable.
#[test]
fn derived_names_use_the_normalised_library_name() {
    let config = parse(
        "[crates.my-core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.multi-era]\ninput = \"m.cddl\"\noutput = \"gen/multi-era\"\ndeps = [\"my-core\"]\n",
    );
    let by_name: std::collections::BTreeMap<String, Cli> =
        config.expand(&[]).unwrap().into_iter().collect();
    assert_eq!(
        by_name["multi-era"].extern_import,
        vec!["my_core=gen/core/extern-interface/my_core"]
    );
    assert_eq!(
        by_name["multi-era"].extern_wasm_crate,
        vec!["my_core=my_core_wasm"]
    );
    assert_eq!(by_name["multi-era"].workspace_dep, vec!["my_core"]);
    assert_eq!(
        by_name["my-core"].key_requests,
        vec!["multi_era=gen/multi-era/rust/src/generated/borrowed_key_types.rs"]
    );
}

/// A dependency with `wasm = false` derives ONLY `--extern-import`.
///
/// The other three are all about a wasm face it does not have, and `--workspace-dep` is not merely
/// pointless without one — it is a hard error without an `--extern-wasm-crate` mapping, which a
/// crate generating no wasm bindings must not be given. The reverse edges go with them: the sidecars
/// a consumer emits exist only because it has a workspace dependency to record.
#[test]
fn a_dependency_without_wasm_derives_only_the_extern_import() {
    let config = parse(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nwasm = false\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    let by_name: std::collections::BTreeMap<String, Cli> =
        config.expand(&[]).unwrap().into_iter().collect();
    assert_eq!(
        by_name["ledger"].extern_import,
        vec!["core=gen/core/extern-interface/core"]
    );
    assert!(by_name["ledger"].extern_wasm_crate.is_empty());
    assert!(by_name["ledger"].extern_wrapper_index.is_empty());
    assert!(by_name["ledger"].workspace_dep.is_empty());
    assert!(by_name["core"].wrapper_requests.is_empty());
    assert!(by_name["core"].key_requests.is_empty());
}

/// A rust-only CONSUMER of a wasm dependency still gets the rust-side reverse edge and not the
/// wasm-side one: `--workspace-dep` makes it emit `borrowed_key_types.rs` in either mode, while
/// `borrowed_collections.rs` is written only under `--wasm`, so deriving it would name a file that is
/// never written.
#[test]
fn a_rust_only_consumer_derives_only_the_rust_side_reverse_edge() {
    let config = parse(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\nwasm = false\n\
         deps = [\"core\"]\n",
    );
    let by_name: std::collections::BTreeMap<String, Cli> =
        config.expand(&[]).unwrap().into_iter().collect();
    assert_eq!(
        by_name["core"].key_requests,
        vec!["ledger=gen/ledger/rust/src/generated/borrowed_key_types.rs"]
    );
    assert!(by_name["core"].wrapper_requests.is_empty());
    // The consumer still defers to the dependency's wrappers, so it keeps the full forward edge.
    assert_eq!(by_name["ledger"].workspace_dep, vec!["core"]);
}

/// Every derived path INTO a crate follows that crate's own `package-json` value, because the flag
/// nests the cargo crates one level down (`<output>/rust/{rust,wasm}`) to leave the output root to
/// the npm package. The extern-interface export does NOT move: it is emitted in every mode, rust-only
/// included, as a sibling of the crate directories rather than a member of them.
#[test]
fn derived_paths_follow_the_other_crates_package_json_layout() {
    let config = parse(
        "[defaults]\npackage-json = true\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    let by_name: std::collections::BTreeMap<String, Cli> =
        config.expand(&[]).unwrap().into_iter().collect();
    assert_eq!(
        by_name["ledger"].extern_wrapper_index,
        vec!["core=gen/core/rust/wasm/src/generated/collections.rs"]
    );
    assert_eq!(
        by_name["core"].wrapper_requests,
        vec!["ledger=gen/ledger/rust/wasm/src/generated/borrowed_collections.rs"]
    );
    assert_eq!(
        by_name["core"].key_requests,
        vec!["ledger=gen/ledger/rust/rust/src/generated/borrowed_key_types.rs"]
    );
    assert_eq!(
        by_name["ledger"].extern_import,
        vec!["core=gen/core/extern-interface/core"],
        "the export is a sibling of the crates, so the npm nesting does not move it"
    );
}

/// A hand-written sub-table entry for the same key wins over the derived one, silently. An explicit
/// value is the user overriding the sugar for a case it does not cover — a vendored copy of a
/// dependency's export, say — not a conflict to report. Only the key it names is overridden; the rest
/// of the edge still derives.
#[test]
fn a_hand_written_entry_wins_over_the_derived_one() {
    let config = parse(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n\n\
         [crates.ledger.extern-import]\ncore = \"vendor/core-export\"\n",
    );
    let by_name: std::collections::BTreeMap<String, Cli> =
        config.expand(&[]).unwrap().into_iter().collect();
    assert_eq!(
        by_name["ledger"].extern_import,
        vec!["core=vendor/core-export"]
    );
    assert_eq!(
        by_name["ledger"].extern_wrapper_index,
        vec!["core=gen/core/wasm/src/generated/collections.rs"],
        "overriding one key must not disable the rest of the edge"
    );
}

/// Generation order is a topological sort over `deps` with ties broken by crate name.
///
/// The fixture discriminates on both halves at once: `alpha` must come after `zeta` despite sorting
/// before it (topology beats the name), while `beta` and `zeta` — both immediately ready — come in
/// name order (the tie-break makes the order total rather than traversal-dependent).
#[test]
fn generation_order_is_topological_with_a_name_tie_break() {
    let config = parse(
        "[crates.zeta]\ninput = \"z.cddl\"\noutput = \"gz\"\n\
         [crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\ndeps = [\"zeta\"]\n\
         [crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n",
    );
    assert_eq!(
        config.generation_order().unwrap(),
        vec!["beta", "zeta", "alpha"]
    );
    let names: Vec<String> = config
        .expand(&[])
        .unwrap()
        .into_iter()
        .map(|(name, _)| name)
        .collect();
    assert_eq!(names, vec!["beta", "zeta", "alpha"]);
}

/// A cycle is a hard error that NAMES the cycle, at parse time. Reporting only that one exists leaves
/// the user to find it across a config where every crate looks locally fine.
#[test]
fn a_dependency_cycle_is_reported_by_naming_it() {
    let err = error(
        "[crates.a]\ninput = \"a.cddl\"\noutput = \"ga\"\ndeps = [\"c\"]\n\
         [crates.b]\ninput = \"b.cddl\"\noutput = \"gb\"\ndeps = [\"a\"]\n\
         [crates.c]\ninput = \"c.cddl\"\noutput = \"gc\"\ndeps = [\"b\"]\n",
    );
    assert!(
        err.contains("a → c → b → a"),
        "the cycle itself must be in the message, got: {err}"
    );

    // A crate that merely DEPENDS on a cycle is not part of it, so it must not be printed as if it
    // were — `outer` sorts first and is where the walk starts.
    let err = error(
        "[crates.outer]\ninput = \"o.cddl\"\noutput = \"go\"\ndeps = [\"x\"]\n\
         [crates.x]\ninput = \"x.cddl\"\noutput = \"gx\"\ndeps = [\"y\"]\n\
         [crates.y]\ninput = \"y.cddl\"\noutput = \"gy\"\ndeps = [\"x\"]\n",
    );
    assert!(
        err.contains("x → y → x") && !err.contains("outer"),
        "only the cycle's own members belong in it, got: {err}"
    );
}

/// The three ways a `deps` list can name something it must not, each a hard error before any crate
/// generates. An unknown name additionally says why the sugar cannot cover an out-of-config
/// dependency: every derived value comes from the dependency's own entry.
#[test]
fn deps_naming_an_unknown_self_or_duplicate_crate_is_a_hard_error() {
    let unknown = error(
        "[crates.ledger]\ninput = \"l.cddl\"\noutput = \"gl\"\ndeps = [\"kore\"]\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"gc\"\n",
    );
    assert!(
        unknown.contains("kore") && unknown.contains("`core`") && unknown.contains("extern-import"),
        "must name the bad dep, list the configured crates, and point at the raw escape hatch, got: {unknown}"
    );

    let itself = error("[crates.core]\ninput = \"c.cddl\"\noutput = \"gc\"\ndeps = [\"core\"]\n");
    assert!(
        itself.contains("itself"),
        "a self-edge must say so, got: {itself}"
    );

    let twice = error(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gc\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gl\"\ndeps = [\"core\", \"core\"]\n",
    );
    assert!(
        twice.contains("twice"),
        "a duplicate edge must say so, got: {twice}"
    );
}

/// `deps` is per-crate only: an edge shared by every crate is not a graph. Rejected in both shared
/// layers, with a reason specific to what `deps` is rather than the generic per-crate sentence.
#[test]
fn deps_in_a_shared_layer_is_a_hard_error() {
    for layer in ["[defaults]", "[profiles.p]"] {
        let err = error(&format!(
            "{layer}\ndeps = [\"core\"]\n{MINIMAL_CRATE}\n[crates.core]\ninput = \"c\"\noutput = \"gc\"\n"
        ));
        assert!(
            err.contains("`deps`") && err.contains("EDGE"),
            "{layer} must reject `deps` as an edge, got: {err}"
        );
    }
}

/// Two crates cannot share a library name: every cross-crate value is derived from it, so the export
/// directory, the wasm crate and the request labels would all collide — and one cargo workspace could
/// not hold both crates either.
#[test]
fn two_crates_with_one_library_name_are_a_hard_error() {
    let err = error(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gc\"\n\
         [crates.core-v2]\ninput = \"c2.cddl\"\noutput = \"gc2\"\nlib-name = \"core\"\n",
    );
    assert!(
        err.contains("`core`") && err.contains("lib-name"),
        "must name the shared library name and the key that fixes it, got: {err}"
    );
}

/// Selecting a subset picks WHICH crates run, never in what order, and never pulls in a dependency:
/// the unselected dependency's committed output is trusted exactly as a dependency in another
/// repository's is. The selected crate still carries the full derived edge — that is what makes the
/// committed output reachable.
#[test]
fn a_subset_selection_does_not_pull_in_its_dependencies() {
    let config = parse(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    let run = |selected: &[&str]| {
        config
            .expand(&selected.iter().map(|s| s.to_string()).collect::<Vec<_>>())
            .unwrap()
    };
    let only_ledger = run(&["ledger"]);
    assert_eq!(
        only_ledger.iter().map(|(n, _)| n).collect::<Vec<_>>(),
        vec!["ledger"]
    );
    assert_eq!(
        only_ledger[0].1.extern_import,
        vec!["core=gen/core/extern-interface/core"],
        "the edge must survive the selection, or the committed output is unreachable"
    );

    let forwards = run(&["core", "ledger"]);
    let backwards = run(&["ledger", "core"]);
    assert_eq!(
        forwards.iter().map(|(n, _)| n).collect::<Vec<_>>(),
        vec!["core", "ledger"]
    );
    assert_eq!(
        forwards
            .iter()
            .map(|(n, c)| (n, format!("{c:?}")))
            .collect::<Vec<_>>(),
        backwards
            .iter()
            .map(|(n, c)| (n, format!("{c:?}")))
            .collect::<Vec<_>>(),
        "argument order must not reach the generated output"
    );
}

// ---------------------------------------------------------------------------------------------
// Convergence
// ---------------------------------------------------------------------------------------------

/// The convergence warning fires exactly when a sidecar this run CONSUMED was rewritten during it,
/// and is silent otherwise.
///
/// Both directions matter: a warning that never fires leaves the one-run-stale case silent (the whole
/// reason generation order picks the dependency's side of the conflict), and one that always fires is
/// noise a user learns to ignore. The absent-then-written case is the cold workspace, and it converges
/// like any other change rather than being special.
#[test]
fn the_convergence_warning_fires_only_when_a_consumed_sidecar_changed() {
    let dir = std::env::temp_dir().join(format!("cddl_config_converge_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let sidecar = dir.join("gen/ledger/wasm/src/generated/borrowed_collections.rs");
    std::fs::create_dir_all(sidecar.parent().unwrap()).unwrap();

    let config = config::parse_str(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
        &dir,
    )
    .unwrap();
    let expanded = config.expand(&[]).unwrap();
    let config_path = dir.join("codegen.toml");

    // Unchanged: silent. (`borrowed_key_types.rs` is absent in both snapshots, which is "unchanged".)
    std::fs::write(&sidecar, "// first\n").unwrap();
    let converged = config::Convergence::capture(&expanded);
    assert!(converged.stale_crates().is_empty());
    assert!(converged.warning(&config_path, &[]).is_none());

    // Rewritten during the run: the crate that read it is a run behind.
    let stale = config::Convergence::capture(&expanded);
    std::fs::write(&sidecar, "// second\n").unwrap();
    assert_eq!(
        stale.stale_crates(),
        ["core".to_owned()].into_iter().collect()
    );
    let warning = stale
        .warning(&config_path, &["core".to_owned()])
        .expect("a changed sidecar must warn");
    assert!(
        warning.contains("`core`") && warning.contains("re-run") || warning.contains("Re-run"),
        "must name the stale crate and say what to do, got: {warning}"
    );
    assert!(
        warning.contains(&format!("--config {} core", config_path.display())),
        "the instruction must be the command that converges THIS run, got: {warning}"
    );

    // A cold workspace: the sidecar did not exist when the dependency read it.
    std::fs::remove_file(&sidecar).unwrap();
    let cold = config::Convergence::capture(&expanded);
    std::fs::write(&sidecar, "// written by this run\n").unwrap();
    assert_eq!(
        cold.stale_crates(),
        ["core".to_owned()].into_iter().collect()
    );

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// End to end
// ---------------------------------------------------------------------------------------------

/// The whole graph, on real disk: a two-crate config with one `deps` edge, generated from a COLD
/// scratch directory by the same `config::generate` a command line reaches.
///
/// What it asserts is the point of the derivation: not that the argv looked right (the unit tests
/// above pin the flag values), but that the derived paths actually name the files the generator
/// writes and reads — the consumer's generated rust really imports the dependency's type through the
/// dependency's crate, which only happens if the derived `--extern-import` found the export the
/// dependency's own run had just emitted.
#[test]
fn a_two_crate_config_generates_a_consumer_that_imports_its_dependency() {
    let dir = std::env::temp_dir().join(format!("cddl_config_e2e_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(dir.join("specs/core.cddl"), "foo = [a: uint, b: text]\n").unwrap();
    // The consumer both references a dependency type directly and builds a collection over it, which
    // is the shape `--workspace-dep` deferral exists for.
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "bar = [f: foo, l: [* foo]]\n",
    )
    .unwrap();
    let config_path = dir.join("codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}/static\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\n\
             [crates.ledger]\ninput = \"specs/ledger.cddl\"\noutput = \"gen/ledger\"\n\
             deps = [\"core\"]\n",
            env!("CARGO_MANIFEST_DIR")
        ),
    )
    .unwrap();

    config::generate(&config_path, &[])
        .unwrap_or_else(|e| panic!("a cold config run must generate: {e}"));

    let dep_export = dir.join("gen/core/extern-interface/core/mod.cddl");
    assert!(
        dep_export.is_file(),
        "the dependency must export the interface the derived --extern-import names"
    );
    let consumer = std::fs::read_to_string(dir.join("gen/ledger/rust/src/generated/mod.rs"))
        .expect("the consumer must generate a rust crate");
    assert!(
        consumer.contains("use core::Foo;"),
        "the consumer must import the dependency's type from the dependency's crate, got:\n{consumer}"
    );
    assert!(
        dir.join("gen/ledger/wasm/src/generated/borrowed_collections.rs")
            .is_file(),
        "the derived --workspace-dep must make the consumer emit the sidecar its dependency reads"
    );

    // A second run has every sidecar in place, which is what convergence means here.
    config::generate(&config_path, &[]).expect("a warm config run must generate");

    let _ = std::fs::remove_dir_all(&dir);
}
