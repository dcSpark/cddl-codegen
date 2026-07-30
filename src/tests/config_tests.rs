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
# `--json-schema-root` is refused without it, and expansion now runs that rule.
json-schema-export = true
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

/// A `<k>=<v>` sub-table has two TOML spellings — a `[crates.<name>.<key>]` header, and an inline
/// table on the crate's own line — and they must expand to the same flags. The inline form is the
/// one the docs recommend for a short mapping, because it does not end the parent table, so this
/// pins that the recommendation is a spelling choice and never a behaviour change.
#[test]
fn an_inline_sub_table_expands_exactly_as_the_header_spelling_does() {
    let header = expand_one(
        r#"
[crates.demo]
input = "spec.cddl"
output = "gen"
lib-name = "after"

[crates.demo.extern-wasm-crate]
core = "core_wasm"
other = "other_wasm"
"#,
    );
    let inline = expand_one(
        r#"
[crates.demo]
input = "spec.cddl"
output = "gen"
extern-wasm-crate = { core = "core_wasm", other = "other_wasm" }
lib-name = "after"
"#,
    );
    // Non-vacuous: both mappings really reached the flag, so the equality below is between two
    // populated expansions rather than between two empty ones.
    assert_eq!(
        header.extern_wasm_crate,
        vec!["core=core_wasm", "other=other_wasm"],
        "the header spelling must reach the flag"
    );
    assert_eq!(
        inline.extern_wasm_crate, header.extern_wasm_crate,
        "the inline spelling must expand to exactly what the header spelling does"
    );
    // The whole reason the docs recommend it: a key written AFTER an inline table still belongs to
    // the crate, where a key written after a `[crates.demo.extern-wasm-crate]` header would not.
    assert_eq!(inline.lib_name, "after");
    assert_eq!(header.lib_name, "after");
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
        "[wasm-reexports]\nledger = [\"cip25\"]\n{MINIMAL_CRATE}"
    ));
    assert!(
        top_level.contains("wasm-reexports") && top_level.contains("[crates]"),
        "must name the unknown table and what is understood, got: {top_level}"
    );
}

/// An unknown key that resembles a real one says which one, and an unknown key that resembles
/// nothing lists what the table understands.
///
/// The suggestion is computed against THIS table's vocabulary, which is the half serde cannot do:
/// by the time `deny_unknown_fields` runs, the per-crate-only keys have been split off, so its
/// message describes a crate table as understanding everything except the keys that are per-crate —
/// and `dep` for `deps` would be answered with a list that excludes `deps`.
#[test]
fn an_unknown_key_suggests_the_nearest_key_that_table_accepts() {
    // A per-crate-only key is reachable only from a crate table, and that is exactly where its
    // typo has to be answerable.
    let dep = error("[crates.demo]\ninput = \"s\"\noutput = \"g\"\ndep = [\"core\"]\n");
    assert!(
        dep.contains("`dep`") && dep.contains("did you mean `deps`?"),
        "a crate table must suggest its per-crate keys, got: {dep}"
    );

    let typo = error(&format!(
        "[defaults]\npreserv-encodings = true\n{MINIMAL_CRATE}"
    ));
    assert!(
        typo.contains("did you mean `preserve-encodings`?"),
        "a one-character typo must be answered with the key, got: {typo}"
    );

    // The near-collision. `json-schema-dep` (a Settings key) and `json-schema-deps` (per-crate only)
    // differ by one character, so which one is nearest is decided by which table is being read.
    let in_crate =
        error("[crates.demo]\ninput = \"s\"\noutput = \"g\"\njson-schema-depss = [\"core\"]\n");
    assert!(
        in_crate.contains("did you mean `json-schema-deps`?"),
        "a crate table holds both, so the nearest wins, got: {in_crate}"
    );
    let in_defaults = error(&format!(
        "[defaults]\njson-schema-depss = [\"core\"]\n{MINIMAL_CRATE}"
    ));
    assert!(
        in_defaults.contains("did you mean `json-schema-dep`?"),
        "a shared table cannot hold `json-schema-deps`, so it must not be suggested there, got: \
         {in_defaults}"
    );

    // Both spellings really are accepted in a crate table — otherwise the assertion above is about
    // a distinction that does not exist.
    parse(
        "[crates.demo]\ninput = \"s\"\noutput = \"g\"\njson-schema-export = true\n\
         json-schema-deps = []\n\n\
         [crates.demo.json-schema-dep]\ncore = \"core_json_schema_gen\"\n",
    );

    // Nowhere near anything: the whole vocabulary, and still the table.
    let alien = error(&format!("[profiles.p]\nquux = true\n{MINIMAL_CRATE}"));
    assert!(
        alien.contains("[profiles.p]")
            && alien.contains("`quux`")
            && alien.contains("`preserve-encodings`")
            && alien.contains("`wasm`")
            && !alien.contains("did you mean"),
        "an unrecognisable key must list what the table understands, got: {alien}"
    );
    assert!(
        !alien.contains("`deps`"),
        "and a shared table must not advertise the per-crate keys, got: {alien}"
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

/// An empty `input` or `output` is refused at parse, naming the key and the crate table.
///
/// Neither has a defensible meaning for it, and both failure modes without this check name something
/// other than the empty value: an empty `output` resolves to the config file's own directory, which
/// as a component sequence is a prefix of every other crate's output — so the clobber guard fires and
/// reports the config directory as containing a crate's tree, a diagnostic about a relationship the
/// user never wrote. A lone crate slips past that guard and is refused by clap instead, against a
/// flag the user never typed.
#[test]
fn an_empty_input_or_output_is_a_hard_error_naming_the_key() {
    for (key, text) in [
        ("input", "[crates.demo]\ninput = \"\"\noutput = \"gen\"\n"),
        (
            "output",
            "[crates.demo]\ninput = \"s.cddl\"\noutput = \"\"\n",
        ),
        // Whitespace is emptiness with a keystroke in it, and reaches the same two failures.
        (
            "output",
            "[crates.demo]\ninput = \"s.cddl\"\noutput = \"  \"\n",
        ),
    ] {
        let err = error(text);
        assert!(
            err.contains(&format!("[crates.demo].{key}")) && err.contains("empty"),
            "must name the key and say it is empty, got: {err}"
        );
    }

    // The diagnostic this replaces, shown to be the one it replaces: two crates, one of them with an
    // empty `output`, used to be reported as a containment. It must now be reported as the empty
    // value it is, and must not name the OTHER crate at all.
    let two = error(
        "[crates.alpha]\ninput = \"a.cddl\"\noutput = \"\"\n\
         [crates.beta]\ninput = \"b.cddl\"\noutput = \"gen/beta\"\n",
    );
    assert!(
        two.contains("[crates.alpha].output") && !two.contains("contains"),
        "an empty output must be reported as itself, not as a clobber of its siblings, got: {two}"
    );
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
wasm = true
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
std-forward-dep = ["core"]
verbosity = "debug"

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

[crates.demo.json-gen-dep]
core-json-schema-gen = "../../../core/wasm/json-gen"

[crates.demo.wasm-dep]
core-wasm = "../../core/wasm"

[crates.demo.rust-dep]
core = "../../core/rust"
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
        "true",
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
        "--json-gen-dep",
        "core-json-schema-gen=../../../core/wasm/json-gen",
        "--wasm-dep",
        "core-wasm=../../core/wasm",
        "--rust-dep",
        "core=../../core/rust",
        "--std-forward-dep",
        "core",
        "--verbosity=debug",
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
        json_gen_dep,
        wasm_dep,
        rust_dep,
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
        std_forward_dep,
        verbosity,
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
    // The one path-valued right-hand side that must NOT be rewritten against the config file's
    // directory: it is a cargo path dependency, resolved by cargo against the manifest it lands in.
    // The two sides agree here only because the config passes it through verbatim.
    assert_eq!(json_gen_dep, from_flags.json_gen_dep);
    assert_eq!(
        json_gen_dep,
        vec!["core-json-schema-gen=../../../core/wasm/json-gen".to_owned()],
        "a `json-gen-dep` path must reach the flag verbatim, not resolved against the config file"
    );
    // The other one, for the same reason and against the other manifest.
    assert_eq!(wasm_dep, from_flags.wasm_dep);
    assert_eq!(rust_dep, from_flags.rust_dep);
    assert_eq!(
        wasm_dep,
        vec!["core-wasm=../../core/wasm".to_owned()],
        "a `wasm-dep` path must reach the flag verbatim, not resolved against the config file"
    );
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
    assert_eq!(std_forward_dep, from_flags.std_forward_dep);
    assert_eq!(verbosity, from_flags.verbosity);
    // Named rather than left to the equality above, because both sides could agree on the built-in
    // default and the row would then prove nothing: the TOML's `"debug"` and the flag's `debug` have
    // to be the SAME vocabulary, and serde's `rename_all` is what makes them so.
    assert_eq!(verbosity, crate::log::Verbosity::Debug);
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

/// A flag COMBINATION the generator refuses is rejected during expansion, so no crate has generated
/// when the run fails.
///
/// These rules used to run per crate INSIDE the generation loop, which made a shared key destructive
/// out of proportion to the mistake: `[defaults].json-schema-scripts = true` with one crate lacking
/// `json-schema-export` regenerated every earlier crate in full, then failed with a bare flag
/// message naming neither the crate nor the TOML line. Both halves are pinned — the failure is at
/// expansion (`expand` returning `Err` is what "before any crate generates" means here) and the
/// message names the crate.
#[test]
fn a_refused_flag_combination_fails_expansion_and_names_the_crate() {
    for (what, text, expected) in [
        (
            "a shared key tripping a rule on the crate that lacks its partner",
            "[defaults]\njson-schema-scripts = true\n\
             [crates.alpha]\ninput = \"a.cddl\"\noutput = \"gen/a\"\njson-schema-export = true\n\
             [crates.beta]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n",
            "--json-schema-scripts=true requires --json-schema-export=true",
        ),
        (
            "the same for a raw sub-table entry",
            "[defaults.json-gen-dep]\ndep-json-schema-gen = \"../dep/wasm/json-gen\"\n\
             [crates.beta]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n",
            "--json-gen-dep requires --json-schema-export=true",
        ),
        (
            "and for a pair of keys neither of which mentions json-schema",
            "[defaults]\ncanonical-form = true\n\
             [crates.beta]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n",
            "--canonical-form=true requires --preserve-encodings=true",
        ),
        (
            "and for a hand-written std forward with no dependency to forward to",
            "[defaults]\nstd-forward-dep = [\"cml-core\"]\n\
             [crates.beta]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n",
            "--std-forward-dep=cml-core names a package that no --rust-dep declares",
        ),
    ] {
        let err = expand_error(text);
        assert!(
            err.contains("[crates.beta]"),
            "the refusal must name the crate the combination landed on ({what}), got:\n{err}"
        );
        assert!(
            err.contains(expected),
            "and must carry the generator's own reason ({what}), got:\n{err}"
        );
    }
}

/// A value whose first character is `-` reaches its flag verbatim.
///
/// Every valued flag is emitted as ONE token, `--name=value`, because clap takes everything after the
/// first `=` on a long option as the value. Split across two tokens the same value is unrepresentable
/// — nothing here sets `allow_hyphen_values` (pinned below), so `--lib-name -x` is read as the
/// unknown flag `-x` — which would leave a config with no spelling at all for a directory or a
/// library name that starts with a dash. The two sub-table cases are here too because they are the
/// one place the `=` split could have gone wrong in the other direction: the VALUE contains an `=`,
/// and only a first-`=` split leaves it intact.
#[test]
fn a_leading_dash_value_reaches_the_flag_verbatim() {
    let cli = expand_one(
        "[crates.demo]\ninput = \"-x.cddl\"\noutput = \"-gen\"\nlib-name = \"-x\"\n\
         static-dir = \"-vendor/static\"\n\
         [crates.demo.extern-import]\ncore = \"-vendor/core\"\n",
    );
    assert_eq!(cli.input, Path::new("-x.cddl"));
    assert_eq!(cli.output, Path::new("-gen"));
    assert_eq!(cli.lib_name, "-x");
    assert_eq!(cli.static_dir, Path::new("-vendor/static"));
    assert_eq!(cli.extern_import, vec!["core=-vendor/core".to_owned()]);
}

/// No `Cli` argument accepts hyphen-led values, which is WHY the single-token emission above is
/// required rather than merely tidier.
///
/// Established by enumerating clap's own argument list rather than by grepping the derive attributes:
/// absence is only as good as the vocabulary searched for, and `allow_hyphen_values` can arrive
/// through an attribute this file does not spell.
#[test]
fn no_cli_argument_accepts_hyphen_led_values() {
    use clap::CommandFactory;

    let accepting: Vec<String> = Cli::command()
        .get_arguments()
        .filter(|arg| arg.is_allow_hyphen_values_set())
        .map(|arg| arg.get_id().to_string())
        .collect();
    assert!(
        accepting.is_empty(),
        "`{accepting:?}` accept hyphen-led values, so the two-token spelling would work for them — \
         if that is now true of every argument, the single-token emission is no longer load-bearing"
    );
}

/// A rejection is attributed to the key that caused it even when `input` or `output` starts with a
/// dash.
///
/// Discriminating by construction: the replay that attributes a clap rejection to a config key
/// builds a BASE from `input` and `output` and probes every other fragment on top of it. Spelled as
/// two tokens, that base would fail on `-x.cddl` — a value the real invocation accepts — so the
/// blame for the genuinely bad key below would land on `input` instead, which is the misattribution
/// the replay exists to prevent, inverted.
#[test]
fn a_rejection_is_attributed_to_its_key_even_when_a_path_starts_with_a_dash() {
    let err = expand_error(
        "[crates.demo]\ninput = \"-x.cddl\"\noutput = \"-g\"\n\
         json-schema-root = [\"a::B; let x = 1\"]\n",
    );
    assert!(
        err.contains("[crates.demo].json-schema-root"),
        "must point at the key clap actually rejected, got: {err}"
    );
    assert!(
        !err.contains("].input") && !err.contains("].output"),
        "and must not blame a path key the tool accepts, got: {err}"
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

/// `load` pins the config's directory to an ABSOLUTE path before anything resolves against it, so
/// the process CWD participates exactly once — in locating the config file itself, where it is the
/// definition of what a relative argument means — and never again. Without this, a relatively-named
/// config mixing an absolute `output` with a relative one sent `manifest_relative_path` down its
/// current-directory fallback, making a COMMITTED `Cargo.toml` byte a function of where the tool
/// was invoked from.
#[test]
fn load_resolves_a_relative_config_path_to_absolute_keys() {
    let dir = std::env::temp_dir().join(format!("cddl_config_absolutize_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("codegen.toml"),
        "[crates.demo]\ninput = \"spec.cddl\"\noutput = \"gen\"\n",
    )
    .unwrap();

    let cwd = std::env::current_dir().unwrap();
    let relative = pathdiff::diff_paths(dir.join("codegen.toml"), &cwd)
        .expect("both endpoints are absolute, so a relative path between them exists");
    assert!(
        relative.is_relative(),
        "the premise: the config file is named RELATIVELY, so resolving it consults the CWD"
    );

    let config = config::load(&relative).unwrap();
    let cli = config.expand(&[]).unwrap().remove(0).1;
    assert_eq!(cli.input, dir.join("spec.cddl"));
    assert_eq!(cli.output, dir.join("gen"));

    let _ = std::fs::remove_dir_all(&dir);
}

/// The binary prints errors through `Display`. Returning the error from `main` renders a
/// `String`-backed error's `Debug`: the message arrives wrapped in quotes, every `"` inside it
/// backslash-escaped, and each newline as a literal `\n` — mangling exactly the multi-sentence
/// diagnostics this feature writes. Pinned end to end against the real binary because no unit test
/// sees `main`'s termination path.
#[test]
fn binary_errors_print_the_message_verbatim_not_debug_escaped() {
    let dir = std::env::temp_dir().join(format!("cddl_config_display_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    // `preserve-encodings = "yes"` draws a serde type error whose message contains both a quoted
    // string and a newline (`…string "yes", expected a boolean\nin `preserve-encodings``) — one
    // probe covers every Debug-escaping symptom at once.
    std::fs::write(
        dir.join("codegen.toml"),
        "[crates.demo]\ninput = \"spec.cddl\"\noutput = \"gen\"\npreserve-encodings = \"yes\"\n",
    )
    .unwrap();

    let out = super::integration_tests::codegen_cmd()
        .arg("--config")
        .arg(dir.join("codegen.toml"))
        .output()
        .expect("the generator binary must be spawnable");
    assert_eq!(
        out.status.code(),
        Some(1),
        "a config error is an ordinary failure, not a panic"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.starts_with("Error: "),
        "the failure must be labelled, got: {stderr}"
    );
    assert!(
        stderr.contains("preserve-encodings"),
        "the failing key must be named, got: {stderr}"
    );
    assert!(
        stderr.contains("\"yes\"") && !stderr.contains("\\\""),
        "a quote inside the message must arrive unescaped, got: {stderr}"
    );
    assert!(
        stderr.trim_end().contains('\n') && !stderr.contains("\\n"),
        "a newline inside the message must break the line, not print as a literal `\\n`, got: {stderr}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// Every diagnostic a config run produces names the config file it came from, not only the ones
/// produced while parsing it.
///
/// `load` prefixes what it returns, so a bad VALUE has always said which file held it. Everything
/// after `load` — expansion, the runtime report, the committed-state read — reached the user as a
/// bare sentence about a `[crates.<name>]` table, which is the least useful place to omit the file
/// name: a repository with several configs, or a wrapper script that chose the path.
#[test]
fn errors_after_parsing_still_name_the_config_file() {
    let dir = std::env::temp_dir().join(format!("cddl_config_prefix_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("codegen.toml");
    // A cross-flag rule, refused during EXPANSION — after `load` has already returned Ok.
    std::fs::write(
        &path,
        "[defaults]\ncanonical-form = true\n[crates.demo]\ninput = \"s.cddl\"\noutput = \"gen\"\n",
    )
    .unwrap();
    let expected = format!("--config {}: ", path.display());

    let ran = config::generate(&path, &[], None, None)
        .expect_err("the combination must be refused")
        .to_string();
    assert!(
        ran.starts_with(&expected),
        "a run's expansion error must name the config, got: {ran}"
    );
    assert!(
        ran.contains("[crates.demo]"),
        "and must still say which crate, got: {ran}"
    );

    let listed = config::print_flags(&path, &[], None, None)
        .expect_err("--print-flags refuses what a run refuses")
        .to_string();
    assert_eq!(
        listed, ran,
        "the listing must fail with the identical message, prefix included"
    );

    // A PARSE error was already prefixed and must not now be prefixed twice.
    std::fs::write(&path, "[crates.demo]\ninput = \"s.cddl\"\n").unwrap();
    let parse_time = config::generate(&path, &[], None, None)
        .expect_err("a missing `output` must be refused")
        .to_string();
    assert_eq!(
        parse_time.matches("--config").count(),
        1,
        "the prefix must be applied once, got: {parse_time}"
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
/// `--static-dir` and `--verbosity` are the two exemptions, and each is asserted from the same
/// enumeration (below), so neither can silently stop being accepted.
///
/// The exemption list is spelled HERE rather than read from `config::EXEMPT_ARG_IDS`, deliberately:
/// reading the real list would make a widened exemption invisible to this test, whereas a hand list
/// makes adding a member fail loudly and demand the accept-half assertion that goes with it.
#[test]
fn a_generation_flag_alongside_config_is_a_hard_error() {
    use clap::CommandFactory;

    for arg in Cli::command().get_arguments() {
        if matches!(arg.get_id().as_str(), "static_dir" | "verbosity") {
            continue;
        }
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

/// `--static-dir` is the one generation flag config mode accepts, in every spelling, and it applies
/// to every crate in the run.
///
/// The exemption criterion is not "harmless" but "has no per-crate precedence question to answer":
/// the flag names where THIS MACHINE keeps the tool's own hand-written runtime, so "does it apply to
/// one crate or all of them?" — the question that rules out every other flag — has one answer. It is
/// also the one value a committed config cannot get right by itself, which is why an override has to
/// exist somewhere.
#[test]
fn static_dir_is_accepted_alongside_config_and_reaches_every_crate() {
    use clap::Parser;

    for spelling in [
        vec!["--static-dir", "/tmp/s"],
        vec!["--static-dir=/tmp/s"],
        vec!["-s", "/tmp/s"],
    ] {
        let argv: Vec<String> = ["cddl-codegen", "--config", "c.toml"]
            .into_iter()
            .chain(spelling.iter().copied())
            .map(str::to_owned)
            .collect();
        config::reject_generation_flags(&argv)
            .unwrap_or_else(|e| panic!("`{spelling:?}` must be accepted with --config, got: {e}"));
        let invocation = config::ConfigCli::parse_from(&argv);
        assert_eq!(
            invocation.static_dir.as_deref(),
            Some(Path::new("/tmp/s")),
            "`{spelling:?}` must parse into the override"
        );
    }

    // It is still an override of a real key, so the three states are: config only, command line
    // only, both.
    let text = "[defaults]\nstatic-dir = \"vendor/static\"\n\
                [crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\n\
                [crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n";
    let static_dirs = |config: &config::Config| {
        config
            .expand(&[])
            .expect("must expand")
            .into_iter()
            .map(|(name, cli)| (name, cli.static_dir))
            .collect::<Vec<_>>()
    };

    let from_key = parse(text);
    assert_eq!(
        static_dirs(&from_key),
        vec![
            (
                "alpha".to_owned(),
                std::path::PathBuf::from("vendor/static")
            ),
            ("beta".to_owned(), std::path::PathBuf::from("vendor/static")),
        ],
        "the config key alone still reaches every crate"
    );

    let mut overridden = parse(text);
    overridden.static_dir_override = Some("/machine/static".to_owned());
    assert_eq!(
        static_dirs(&overridden),
        vec![
            (
                "alpha".to_owned(),
                std::path::PathBuf::from("/machine/static")
            ),
            (
                "beta".to_owned(),
                std::path::PathBuf::from("/machine/static")
            ),
        ],
        "the command line wins over the committed key, for every crate at once"
    );

    // With no key at all the override is still what every crate gets — otherwise the flag would
    // work only on configs that already spell the value it is replacing.
    let mut no_key = parse(
        "[crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\n\
         [crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n",
    );
    no_key.static_dir_override = Some("/machine/static".to_owned());
    assert_eq!(
        static_dirs(&no_key),
        vec![
            (
                "alpha".to_owned(),
                std::path::PathBuf::from("/machine/static")
            ),
            (
                "beta".to_owned(),
                std::path::PathBuf::from("/machine/static")
            ),
        ]
    );

    // A RELATIVE override is passed through verbatim rather than resolved against the config file's
    // directory: it did not come from the config file. `--static-dir static` therefore means on a
    // config command line exactly what it means on any other one — relative to the process CWD.
    let mut relative = parse(text);
    relative.static_dir_override = Some("vendor/other".to_owned());
    assert_eq!(
        static_dirs(&relative)[0].1,
        std::path::PathBuf::from("vendor/other")
    );
}

/// The override is visible in `--print-flags`, tagged `command line` rather than with a config key —
/// which is the only way the listing can explain why the `static-dir` key in the file is not the
/// value being used.
#[test]
fn print_flags_attributes_the_static_dir_override_to_the_command_line() {
    let text = "[defaults]\nstatic-dir = \"vendor/static\"\n\
                [crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\n";

    let from_key = parse(text).flag_listing(&[]).expect("a valid config lists");
    assert!(
        from_key.lines().any(
            |line| line.trim_start() == "static-dir  --static-dir=vendor/static"
                || (line.trim_start().starts_with("static-dir")
                    && line.ends_with("--static-dir=vendor/static"))
        ),
        "without an override the key is the provenance, got:\n{from_key}"
    );

    let mut overridden = parse(text);
    overridden.static_dir_override = Some("/machine/static".to_owned());
    let listing = overridden.flag_listing(&[]).expect("a valid config lists");
    assert!(
        listing
            .lines()
            .any(|line| line.trim_start().starts_with("command line")
                && line.ends_with("--static-dir=/machine/static")),
        "the override must be listed as coming from the command line, got:\n{listing}"
    );
    assert!(
        !listing.contains("vendor/static"),
        "and the overridden key's value must not also be listed, got:\n{listing}"
    );
}

/// `--verbosity` is the second generation flag config mode accepts, in every spelling — and the
/// rejection it is exempted FROM still works.
///
/// Both halves are asserted on purpose: an exemption test that only checks the accept half cannot
/// tell "exempted" from "the rejection broke and now lets everything through".
#[test]
fn verbosity_passes_through_config_mode() {
    use clap::Parser;

    for spelling in [
        vec!["--verbosity", "trace"],
        vec!["--verbosity=trace"],
        vec!["-v", "trace"],
    ] {
        let argv: Vec<String> = ["cddl-codegen", "--config", "c.toml"]
            .into_iter()
            .chain(spelling.iter().copied())
            .map(str::to_owned)
            .collect();
        config::reject_generation_flags(&argv)
            .unwrap_or_else(|e| panic!("`{spelling:?}` must be accepted with --config, got: {e}"));
        let invocation = config::ConfigCli::parse_from(&argv);
        assert_eq!(
            invocation.verbosity,
            Some(crate::log::Verbosity::Trace),
            "`{spelling:?}` must parse into the override"
        );
    }

    // Absent is distinguishable from `warn`: the run level is
    // `command line ?? [defaults].verbosity ?? warn`, and a defaulted `ConfigCli` field would
    // silently outrank the key.
    let bare: Vec<String> = ["cddl-codegen", "--config", "c.toml"]
        .into_iter()
        .map(str::to_owned)
        .collect();
    assert_eq!(config::ConfigCli::parse_from(&bare).verbosity, None);

    // The other half: a genuinely per-crate flag is still refused, in both spellings.
    for offender in ["--preserve-encodings=true", "--preserve-encodings"] {
        let argv: Vec<String> = ["cddl-codegen", "--config", "c.toml", offender]
            .into_iter()
            .map(str::to_owned)
            .collect();
        let Err(err) = config::reject_generation_flags(&argv) else {
            panic!("`{offender}` must still be rejected alongside --config");
        };
        assert!(
            err.contains("--preserve-encodings") && err.contains("config file"),
            "must name the flag and say where it belongs, got: {err}"
        );
    }
}

/// A command-line `--verbosity` overrides the committed key, and `--print-flags` says so — the only
/// surface on which that override is observable without reading the run's own output.
///
/// Driven through the real binary rather than `flag_listing` directly, because the chain being pinned
/// is longer than the listing: `ConfigCli` -> `Config::verbosity_override` -> the `argv_fragments`
/// override arm, plus the `EXEMPT_ARG_IDS` pass-through that lets the flag reach any of it.
#[test]
fn a_command_line_verbosity_overrides_the_key_in_the_listing() {
    let dir = std::env::temp_dir().join(format!("cddl_config_verbosity_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let config_path = dir.join("codegen.toml");
    std::fs::write(
        &config_path,
        "[defaults]\nverbosity = \"error\"\n\n[crates.demo]\ninput = \"s.cddl\"\noutput = \"g\"\n",
    )
    .unwrap();

    // Without the flag, the committed key is the provenance and the value.
    let from_key = super::integration_tests::codegen_cmd()
        .arg("--config")
        .arg(&config_path)
        .arg("--print-flags")
        .output()
        .expect("the generator binary must be spawnable");
    assert!(from_key.status.success(), "--print-flags must exit 0");
    let listed = String::from_utf8_lossy(&from_key.stdout);
    assert!(
        listed
            .lines()
            .any(|line| line.trim_start().starts_with("verbosity")
                && line.ends_with("--verbosity=error")),
        "the key alone is its own provenance, got:\n{listed}"
    );

    let overridden = super::integration_tests::codegen_cmd()
        .arg("--config")
        .arg(&config_path)
        .arg("--print-flags")
        .args(["--verbosity", "trace"])
        .output()
        .expect("the generator binary must be spawnable");
    assert!(
        overridden.status.success(),
        "`--verbosity` must not be rejected alongside `--config`, got: {}",
        String::from_utf8_lossy(&overridden.stderr)
    );
    let listed = String::from_utf8_lossy(&overridden.stdout);
    assert!(
        listed
            .lines()
            .any(|line| line.trim_start().starts_with("command line")
                && line.ends_with("--verbosity=trace")),
        "the override must be listed as coming from the command line, got:\n{listed}"
    );
    assert!(
        !listed.contains("--verbosity=error"),
        "and the overridden key's value must not also be listed, got:\n{listed}"
    );

    let _ = std::fs::remove_dir_all(&dir);
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
// `--print-flags`
// ---------------------------------------------------------------------------------------------

/// The listing states, per crate and in GENERATION order, every flag the run would use and the
/// config key that put it there.
///
/// The fixture is built so both halves of that sentence can fail. `zeta` depends on `alpha`, so the
/// generation order is `alpha` then `zeta` while the crate tables (and every `BTreeMap` behind them)
/// are in the opposite order — a listing that simply walked the config would print them backwards.
/// And every flag the edge derives is one no key in the file is NAMED after: attributing them to
/// `extern-import`/`workspace-dep`/… would point at TOML lines that do not exist, which is the exact
/// question the key column answers.
#[test]
fn print_flags_lists_every_crate_in_generation_order_keyed_by_the_config_key() {
    let listing = parse(
        "[defaults]\npreserve-encodings = true\n\
         [crates.zeta]\ninput = \"z.cddl\"\noutput = \"gen/zeta\"\ndeps = [\"alpha\"]\n\
         [crates.alpha]\ninput = \"a.cddl\"\noutput = \"gen/alpha\"\n",
    )
    .flag_listing(&[])
    .expect("a valid config must list");

    let blocks: Vec<&str> = listing
        .lines()
        .filter(|line| line.starts_with("[crates."))
        .collect();
    assert_eq!(
        blocks,
        vec!["[crates.alpha]", "[crates.zeta]"],
        "the blocks must be in generation order — dependencies first — not in table order, got:\n\
         {listing}"
    );

    // Every flag line leads with its config key, never with the flag: that is what makes the listing
    // answer "why is this here?", and it is also what stops it being mistaken for something to paste
    // into a script.
    for line in listing.lines() {
        if line.starts_with("  ") {
            assert!(
                !line.trim_start().starts_with("--"),
                "a flag line must lead with the config key, not the flag, got: {line:?}"
            );
        }
    }

    for (key, flag) in [
        ("input", "--input=a.cddl"),
        ("preserve-encodings", "--preserve-encodings=true"),
        // The reverse half of the edge, on the DEPENDENCY's block. `alpha` has no `deps` array at
        // all, so a bare `deps` would send a reader looking in the wrong table: the provenance names
        // the consumer whose `deps` caused it.
        (
            "deps (from [crates.zeta])",
            "--wrapper-requests=zeta=gen/zeta/wasm/src/generated/borrowed_collections.rs",
        ),
        (
            "deps (from [crates.zeta])",
            "--key-requests=zeta=gen/zeta/rust/src/generated/borrowed_key_types.rs",
        ),
        // And the forward half, on the consumer's — this crate's own `deps`, so the bare key is the
        // whole answer.
        (
            "deps",
            "--extern-import=alpha=gen/alpha/extern-interface/alpha",
        ),
    ] {
        assert!(
            listing
                .lines()
                .any(|line| line.trim_start().starts_with(key) && line.ends_with(flag)),
            "the listing must carry `{flag}` tagged `{key}`, got:\n{listing}"
        );
    }

    // The two provenances are genuinely different, not one a prefix of the other by accident: a
    // forward edge stays the bare key, so a `starts_with` on it cannot be what passed above.
    let forward = listing
        .lines()
        .find(|line| line.contains("--extern-import="))
        .expect("the consumer's block carries the forward edge");
    assert_eq!(
        forward.trim_start().split("  ").next(),
        Some("deps"),
        "a forward edge is this crate's own `deps` key and nothing more, got: {forward:?}"
    );

    // A hand-written entry in the same sub-table keeps the flag-named key, because that IS what the
    // user typed — the sugar's key is reported only for what the sugar wrote.
    let hand = parse(
        "[crates.demo]\ninput = \"d.cddl\"\noutput = \"gen/demo\"\n\
         [crates.demo.extern-import]\nother = \"vendor/other\"\n",
    )
    .flag_listing(&[])
    .expect("a valid config must list");
    assert!(
        hand.lines()
            .any(|line| line.trim_start().starts_with("extern-import")),
        "a hand-written sub-table entry stays attributed to the key it was written under, got:\n{hand}"
    );
}

/// Naming crates lists those crates; naming none lists them all. Same selector as a run, so a
/// listing cannot describe a set of crates the same command line would not generate.
#[test]
fn print_flags_lists_only_the_crates_named() {
    let config = parse(
        "[crates.alpha]\ninput = \"a.cddl\"\noutput = \"ga\"\n\
         [crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n",
    );
    let blocks = |selected: &[&str]| {
        config
            .flag_listing(&selected.iter().map(|s| s.to_string()).collect::<Vec<_>>())
            .expect("a valid config must list")
            .lines()
            .filter(|line| line.starts_with("[crates."))
            .map(str::to_owned)
            .collect::<Vec<_>>()
    };
    assert_eq!(blocks(&[]), vec!["[crates.alpha]", "[crates.beta]"]);
    assert_eq!(blocks(&["beta"]), vec!["[crates.beta]"]);
}

/// Listing runs the WHOLE expansion, so a config that could not generate cannot be listed either —
/// and fails with the identical message.
///
/// This is what stops `--print-flags` becoming a second, laxer front door onto the same file: a
/// listing that printed flags for a config a run would refuse would be describing an invocation that
/// never happens.
#[test]
fn print_flags_reports_exactly_what_a_run_would_reject() {
    for text in [
        // A cross-flag rule, refused during expansion.
        "[defaults]\ncanonical-form = true\n[crates.beta]\ninput = \"b.cddl\"\noutput = \"gb\"\n",
        // A graph refusal, refused during validation.
        "[crates.a]\ninput = \"a.cddl\"\noutput = \"ga\"\ndeps = [\"b\"]\n\
         [crates.b]\ninput = \"b.cddl\"\noutput = \"gb\"\ndeps = [\"a\"]\n",
    ] {
        let listed = config::parse_str(text, Path::new(""))
            .and_then(|config| config.flag_listing(&[]))
            .err();
        let ran = config::parse_str(text, Path::new(""))
            .and_then(|config| config.expand(&[]).map(|_| String::new()))
            .err();
        assert!(
            listed.is_some(),
            "a config a run refuses must not list:\n{text}"
        );
        assert_eq!(
            listed, ran,
            "listing and running must refuse a config the same way, or `--print-flags` is a second \
             front door with its own rules:\n{text}"
        );
    }
}

/// `--print-flags` reaches the config parser rather than the generation-flag rejection, and does not
/// consume the positional selectors.
///
/// It is a mode switch, not a generation flag — nothing about it changes what any crate is generated
/// WITH — so it belongs on the same side of `reject_generation_flags` as the crate names.
#[test]
fn print_flags_is_not_a_generation_flag_and_leaves_the_selection_alone() {
    use clap::Parser;

    let argv: Vec<String> = [
        "cddl-codegen",
        "--config",
        "c.toml",
        "--print-flags",
        "core",
    ]
    .into_iter()
    .map(str::to_owned)
    .collect();
    config::reject_generation_flags(&argv)
        .expect("`--print-flags` is a mode switch, not a generation flag");
    let invocation = config::ConfigCli::parse_from(&argv);
    assert!(invocation.print_flags);
    assert_eq!(invocation.crates, vec!["core".to_owned()]);

    let without = config::ConfigCli::parse_from(["cddl-codegen", "--config", "c.toml"]);
    assert!(!without.print_flags, "the default is to generate");
}

/// `--with-deps` closes the selection over `deps`, transitively, in generation order — and over
/// DEPENDENCIES only.
///
/// The graph it closes over is read off the parsed `Config`, before any expansion: the same `deps`
/// edges `generation_order` sorts on, which is what lets the selection be resolved once, on the
/// command line, rather than re-derived by every consumer of it.
#[test]
fn with_deps_closes_the_selection_over_dependencies_only() {
    let config = parse(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gc\"\n\n\
         [crates.mid]\ninput = \"m.cddl\"\noutput = \"gm\"\ndeps = [\"core\"]\n\n\
         [crates.app]\ninput = \"a.cddl\"\noutput = \"ga\"\ndeps = [\"mid\"]\n\n\
         [crates.solo]\ninput = \"s.cddl\"\noutput = \"gs\"\n",
    );

    // TRANSITIVE: naming the top of a two-edge chain pulls in both levels below it, and the result
    // is in generation order — dependencies first — because the selection picks WHICH crates run,
    // never in what order.
    assert_eq!(
        config.with_dependencies(&["app".to_owned()]).unwrap(),
        vec!["core".to_owned(), "mid".to_owned(), "app".to_owned()]
    );
    // DEPENDENCIES ONLY: `mid` does not drag in `app`, which depends on it. A consumer would be
    // output the user did not ask for, and the committed-state verdict is how they hear that it
    // needs regenerating instead.
    assert_eq!(
        config.with_dependencies(&["mid".to_owned()]).unwrap(),
        vec!["core".to_owned(), "mid".to_owned()]
    );
    assert_eq!(
        config.with_dependencies(&["core".to_owned()]).unwrap(),
        vec!["core".to_owned()],
        "a crate with no `deps` closes to itself"
    );
    // Two names whose closures overlap produce ONE list, deduplicated and ordered, and the order of
    // the names on the command line cannot change it.
    let both = vec!["app".to_owned(), "solo".to_owned()];
    let reversed = vec!["solo".to_owned(), "app".to_owned()];
    assert_eq!(
        config.with_dependencies(&both).unwrap(),
        vec![
            "core".to_owned(),
            "mid".to_owned(),
            "app".to_owned(),
            "solo".to_owned()
        ],
        "the closure is deduplicated and delivered in generation order"
    );
    assert_eq!(
        config.with_dependencies(&both).unwrap(),
        config.with_dependencies(&reversed).unwrap(),
        "the selection picks which crates run, never in what order"
    );

    // An unknown name is the SAME hard error the positional selector gives it, character for
    // character — one message, because it is one mistake however it was spelled.
    let closed = config
        .with_dependencies(&["nope".to_owned()])
        .expect_err("an unknown name must be refused");
    let positional = config
        .expand(&["nope".to_owned()])
        .expect_err("an unknown positional name must be refused");
    assert_eq!(closed, positional);
    assert!(closed.contains("`nope` is not a crate in this config"));

    // And the flag with nothing to close over is an error that says exactly that, rather than a
    // silent no-op that reads as having worked.
    let empty = config
        .with_dependencies(&[])
        .expect_err("`--with-deps` with no crate named must be refused");
    assert!(
        empty.contains("--with-deps") && empty.contains("every crate in the config"),
        "the message must name the flag and say why naming nothing has no closure to take, got: \
         {empty}"
    );
}

/// `--with-deps` parses as a selection modifier: it reaches the config parser rather than the
/// generation-flag rejection, and leaves the positional names alone.
#[test]
fn with_deps_is_not_a_generation_flag_and_leaves_the_selection_alone() {
    use clap::Parser;

    let argv: Vec<String> = [
        "cddl-codegen",
        "--config",
        "c.toml",
        "--with-deps",
        "ledger",
    ]
    .into_iter()
    .map(str::to_owned)
    .collect();
    config::reject_generation_flags(&argv)
        .expect("`--with-deps` chooses which crates run, so it is not a generation flag");
    let invocation = config::ConfigCli::parse_from(&argv);
    assert!(invocation.with_deps);
    assert_eq!(
        invocation.crates,
        vec!["ledger".to_owned()],
        "the crate names stay positional; the flag only says what to do with them"
    );

    let without = config::ConfigCli::parse_from(["cddl-codegen", "--config", "c.toml", "ledger"]);
    assert!(
        !without.with_deps,
        "the default is the plain selection, which trusts a dependency's committed output"
    );
}

/// `--help` and `--version` alongside `--config` reach clap, not the generation-flag rejection.
///
/// They are not `Cli` arguments at all — clap synthesises them at build time, so they never appear
/// in the `Cli::command().get_arguments()` enumeration the rejection harvests. That is a fact about
/// clap rather than a decision of ours, which is exactly why it is pinned here: a config user asking
/// what the command does must get the help, not a lecture about where generation flags live.
#[test]
fn help_and_version_are_not_generation_flags() {
    for flag in ["--help", "-h", "--version", "-V"] {
        let argv: Vec<String> = ["cddl-codegen", "--config", "c.toml", flag]
            .into_iter()
            .map(str::to_owned)
            .collect();
        config::reject_generation_flags(&argv)
            .unwrap_or_else(|e| panic!("`{flag}` must reach clap, got: {e}"));
    }
}

/// `--help` shows what the flag DOES and nothing about how it is implemented.
///
/// clap renders a field's doc comment into the help, so a maintainer's note living in a second
/// paragraph is printed to every user. The note that used to be there named an internal function by
/// its rustdoc link, which reads to a user as a broken sentence about something they cannot see.
#[test]
fn config_mode_help_carries_no_maintainer_notes() {
    use clap::CommandFactory;

    let help = config::ConfigCli::command().render_long_help().to_string();
    assert!(
        help.contains("Print the flags each crate would be generated with"),
        "the help must still describe the flag, got:\n{help}"
    );
    assert!(
        help.contains("Where the hand-written serialization runtime is read from"),
        "and the exempted flag, got:\n{help}"
    );
    // Two independent shapes of the same leak: the specific note that used to render, and any
    // rustdoc intra-doc link at all — the latter is what makes this catch the NEXT one rather than
    // only this one.
    assert!(
        !help.contains("reject_generation_flags"),
        "a maintainer's note about an internal function reached --help, got:\n{help}"
    );
    assert!(
        !help.contains("[`"),
        "a rustdoc intra-doc link reached --help, where it renders as a broken reference, got:\n\
         {help}"
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
/// - `profiles`, `deps`, `wasm-reexports` and `json-schema-deps` are config keys with no `Cli`
///   field: the first selects which shared layers a crate applies, the other three declare edges to
///   other crates (a rust/extern one, a packaging one, and an override of what those two derive for
///   the schema document). All four are the config's own structure — they have no flag equivalent
///   by construction, and none is on `Settings`, so none reaches `config_keys` in the first place.
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

    // `SETTINGS_KEYS` spells the same field set a third time, for the unknown-key check that has to
    // know the vocabulary before serde does. It is safe to hand-WRITE only because it is not
    // hand-MAINTAINED: this is where a field added without a row in it fails.
    let listed: BTreeSet<String> = config::SETTINGS_KEYS
        .iter()
        .map(|k| (*k).to_owned())
        .collect();
    assert_eq!(
        listed.len(),
        config::SETTINGS_KEYS.len(),
        "`SETTINGS_KEYS` lists a key twice, which would make the suggestion list repeat itself"
    );
    assert_eq!(
        listed, settings_keys,
        "`config::SETTINGS_KEYS` and `struct Settings` disagree — the unknown-key check would \
         reject a real key, or accept one serde then refuses with a message about a struct the user \
         cannot see"
    );
}

// ---------------------------------------------------------------------------------------------
// The editor schema
// ---------------------------------------------------------------------------------------------

/// The committed JSON Schema an editor loads, relative to the crate root.
const EDITOR_SCHEMA_PATH: &str = "docs/editor/cddl-codegen-config.schema.json";

/// A JSON value in the four shapes a schema document needs, rendered in INSERTION order.
///
/// Hand-built rather than `serde_json::Value`, whose object is a `BTreeMap` unless the
/// `preserve_order` feature happens to be enabled somewhere in the dependency graph: the committed
/// file's key order would then be decided by cargo feature unification rather than by this file,
/// and a schema whose properties follow their struct's declaration order reads like the struct. The
/// rendered string is parsed back as JSON by the test, so the hand emitter cannot ship a malformed
/// document.
#[derive(Clone)]
enum Json {
    Str(String),
    Int(i64),
    Bool(bool),
    Obj(Vec<(String, Json)>),
    Arr(Vec<Json>),
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Pretty-print with two-space indentation — the shape a committed, hand-reviewed JSON file wants.
fn render_json(value: &Json, indent: usize) -> String {
    let pad = "  ".repeat(indent);
    let inner = "  ".repeat(indent + 1);
    match value {
        Json::Str(s) => format!("\"{}\"", escape_json(s)),
        Json::Int(i) => i.to_string(),
        Json::Bool(b) => b.to_string(),
        Json::Obj(entries) if entries.is_empty() => "{}".to_owned(),
        Json::Obj(entries) => {
            let body = entries
                .iter()
                .map(|(k, v)| {
                    format!(
                        "{inner}\"{}\": {}",
                        escape_json(k),
                        render_json(v, indent + 1)
                    )
                })
                .collect::<Vec<_>>()
                .join(",\n");
            format!("{{\n{body}\n{pad}}}")
        }
        Json::Arr(items) if items.is_empty() => "[]".to_owned(),
        Json::Arr(items) => {
            let body = items
                .iter()
                .map(|v| format!("{inner}{}", render_json(v, indent + 1)))
                .collect::<Vec<_>>()
                .join(",\n");
            format!("[\n{body}\n{pad}]")
        }
    }
}

/// One struct's fields as `(TOML key, whitespace-stripped Rust type)`, in DECLARATION order.
///
/// The same `syn`-off-SOURCE read `config_keys_match_cli_fields` uses, for the same reason: the only
/// thing that can satisfy it is the real declaration.
fn struct_field_types(path: &str, struct_name: &str) -> Vec<(String, String)> {
    use quote::ToTokens;

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
    let fields: Vec<(String, String)> = item
        .fields
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|ident| (ident, &f.ty)))
        .map(|(ident, ty)| {
            let rendered: String = ty
                .to_token_stream()
                .to_string()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            (ident.to_string().replace('_', "-"), rendered)
        })
        .collect();
    assert!(
        !fields.is_empty(),
        "parsed zero fields from `{struct_name}` in {path} — the source parse drifted and the \
         schema gate went vacuous"
    );
    fields
}

/// Each `Cli` field's config key mapped to `(long flag, whether the flag NEGATES the key)`, read
/// off clap's attributes rather than assumed to be the field name: `preserve_comments`'s flag is the
/// negated `--no-preserve-comments`, so a description deriving the flag from the field name alone
/// would name a flag that does not exist, and one ignoring the `SetFalse` action would tell the
/// reader the key's `true` value passes it.
fn cli_flag_spellings(path: &str) -> std::collections::BTreeMap<String, (String, bool)> {
    use quote::ToTokens;

    let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    let file = syn::parse_file(&source).unwrap_or_else(|e| panic!("parse {path}: {e}"));
    let item = file
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Struct(s) if s.ident == "Cli" => Some(s),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{path} declares no `struct Cli`"));

    let mut longs = std::collections::BTreeMap::new();
    let mut any_renamed = false;
    for field in &item.fields {
        let Some(ident) = field.ident.as_ref() else {
            continue;
        };
        let key = ident.to_string().replace('_', "-");
        let attrs: Vec<String> = field
            .attrs
            .iter()
            .map(|attr| attr.to_token_stream().to_string())
            .collect();
        let declared = attrs.iter().find_map(|text| {
            let start = text.find("long = \"")? + "long = \"".len();
            let rest = &text[start..];
            rest.find('"').map(|end| rest[..end].to_owned())
        });
        // clap's `SetFalse` is what makes a flag the key's NEGATION: passing it stores `false`, so
        // it is the key's `false` value that emits the flag.
        let negated = attrs.iter().any(|text| text.contains("SetFalse"));
        let long = declared.unwrap_or_else(|| key.clone());
        any_renamed |= long != key;
        longs.insert(key, (long, negated));
    }
    assert!(
        any_renamed && longs.values().any(|(_, negated)| *negated),
        "no `Cli` field declares a `long = \"…\"` spelling different from its field name, or none \
         declares `SetFalse` — either the negated `--no-preserve-comments` stopped being negated, \
         or this attribute read broke and every description now describes a flag guessed from a \
         field name"
    );
    longs
}

/// The JSON-Schema keywords one Rust field type means as a TOML value, or `None` for a type this
/// schema cannot express.
///
/// `None` is the DECISION POINT made mechanical: a `Settings` field of some richer type would be a
/// config value an editor cannot check, and that has to be decided rather than approximated.
fn schema_type_keywords(rust_type: &str) -> Option<Vec<(String, Json)>> {
    let string = || Json::Obj(vec![("type".to_owned(), Json::Str("string".to_owned()))]);
    let entries = match rust_type {
        "Option<bool>" | "bool" => vec![("type".to_owned(), Json::Str("boolean".to_owned()))],
        "Option<u32>" => vec![
            ("type".to_owned(), Json::Str("integer".to_owned())),
            ("minimum".to_owned(), Json::Int(0)),
        ],
        "Option<String>" | "String" => {
            vec![("type".to_owned(), Json::Str("string".to_owned()))]
        }
        "Vec<String>" | "Option<Vec<String>>" => vec![
            ("type".to_owned(), Json::Str("array".to_owned())),
            ("items".to_owned(), string()),
        ],
        "BTreeMap<String,String>" => vec![
            ("type".to_owned(), Json::Str("object".to_owned())),
            ("additionalProperties".to_owned(), string()),
        ],
        // An enum of fixed strings is a string in disguise — the case the panic below invites. The
        // `enum` array is DERIVED from `clap::ValueEnum::value_variants()` rather than written out:
        // a hand list drifts silently the day a level is added, and the point of this document being
        // generated is that it cannot.
        "Option<Verbosity>" => {
            use clap::ValueEnum;
            vec![
                ("type".to_owned(), Json::Str("string".to_owned())),
                (
                    "enum".to_owned(),
                    Json::Arr(
                        crate::log::Verbosity::value_variants()
                            .iter()
                            .map(|v| Json::Str(v.as_str().to_owned()))
                            .collect(),
                    ),
                ),
            ]
        }
        _ => return None,
    };
    Some(entries)
}

/// The one-line description a key carries in the schema, DERIVED rather than written: the flag it
/// is the config key for, or — for the config's own structural keys, which have no flag by
/// construction — a pointer at the document that explains them. Per-key prose would rot silently,
/// since nothing can check it.
fn key_description(
    key: &str,
    longs: &std::collections::BTreeMap<String, (String, bool)>,
) -> String {
    match longs.get(key) {
        Some((long, true)) => format!(
            "Config key for the `--{long}` flag: setting this key to `false` is what passes it. \
             See docs/docs/command_line_flags.mdx."
        ),
        Some((long, false)) => {
            format!("Config key for the `--{long}` flag. See docs/docs/command_line_flags.mdx.")
        }
        None => "Config-structure key with no flag of its own. See docs/docs/config_file.mdx."
            .to_owned(),
    }
}

/// Build the whole schema document from the config surface's own declarations.
fn build_editor_schema() -> String {
    use std::collections::BTreeSet;

    let config_rs = concat!(env!("CARGO_MANIFEST_DIR"), "/src/config.rs");
    let cli_rs = concat!(env!("CARGO_MANIFEST_DIR"), "/src/cli.rs");
    let longs = cli_flag_spellings(cli_rs);

    let described = |key: &str, rust_type: &str, description: String| -> (String, Json) {
        let mut keywords = schema_type_keywords(rust_type).unwrap_or_else(|| {
            panic!(
                "config key `{key}` has Rust type `{rust_type}`, which this schema has no TOML type \
                 for. A config value must be a boolean, an integer, a string, an array of strings \
                 or a table of strings — extend the mapping if the new type is one of those in \
                 disguise, otherwise the key itself needs deciding, not approximating."
            )
        });
        keywords.push(("description".to_owned(), Json::Str(description)));
        (key.to_owned(), Json::Obj(keywords))
    };
    let property =
        |key: &str, rust_type: &str| described(key, rust_type, key_description(key, &longs));

    // `[defaults]`, every `[profiles.<name>]`, and the settings half of every `[crates.<name>]`.
    let settings_fields = struct_field_types(config_rs, "Settings");
    assert_eq!(
        settings_fields
            .iter()
            .map(|(k, _)| k.clone())
            .collect::<BTreeSet<_>>(),
        config::SETTINGS_KEYS
            .iter()
            .map(|k| (*k).to_owned())
            .collect::<BTreeSet<_>>(),
        "`struct Settings` and `SETTINGS_KEYS` disagree; `config_keys_match_cli_fields` says which"
    );
    let settings_props: Vec<(String, Json)> = settings_fields
        .iter()
        .map(|(key, ty)| property(key, ty))
        .collect();

    // The `[crates.<name>]`-only keys, which the shared tables must NOT accept.
    let per_crate_fields: Vec<(String, String)> = struct_field_types(config_rs, "CrateEntry")
        .into_iter()
        // The nested settings layer, not a key of its own.
        .filter(|(key, _)| key != "settings")
        .collect();
    assert_eq!(
        per_crate_fields
            .iter()
            .map(|(k, _)| k.clone())
            .collect::<BTreeSet<_>>(),
        config::PER_CRATE_ONLY_KEYS
            .iter()
            .map(|k| (*k).to_owned())
            .collect::<BTreeSet<_>>(),
        "`struct CrateEntry` and `PER_CRATE_ONLY_KEYS` disagree — one of them has a key the other \
         does not, so the schema would accept or refuse the wrong thing in a crate table"
    );
    let mut crate_props: Vec<(String, Json)> = per_crate_fields
        .iter()
        .map(|(key, ty)| property(key, ty))
        .collect();
    crate_props.extend(settings_props.clone());

    // The two keys `parse_str` demands of every crate table (`required_string`); `lib-name` is not
    // among them because it defaults to the table's own name.
    let required = ["input", "output"];
    for key in required {
        assert!(
            per_crate_fields.iter().any(|(k, _)| k == key),
            "`{key}` is declared required by the schema but is not a `CrateEntry` field"
        );
    }

    // `[runtime]`'s keys are not `Settings` keys, and one of them COLLIDES with a per-crate key:
    // `lib-name` in `[crates.<name>]` is the `--lib-name` flag, while in `[runtime]` it is the cargo
    // package name of the co-owned runtime crate — the same spelling for a different thing, and no
    // flag of its own. So a runtime key is described as a flag key only while it is NOT also a
    // per-crate key; a future collision gets the same treatment without being noticed by hand.
    let runtime_props: Vec<(String, Json)> = struct_field_types(config_rs, "Runtime")
        .iter()
        .map(|(key, ty)| {
            if per_crate_fields.iter().any(|(k, _)| k == key) {
                described(key, ty, key_description(key, &Default::default()))
            } else {
                property(key, ty)
            }
        })
        .collect();

    let definition = |props: Vec<(String, Json)>, required: Option<Vec<&str>>| -> Json {
        let mut entries = vec![
            ("type".to_owned(), Json::Str("object".to_owned())),
            ("additionalProperties".to_owned(), Json::Bool(false)),
        ];
        if let Some(required) = required {
            entries.push((
                "required".to_owned(),
                Json::Arr(
                    required
                        .into_iter()
                        .map(|k| Json::Str(k.to_owned()))
                        .collect(),
                ),
            ));
        }
        entries.push(("properties".to_owned(), Json::Obj(props)));
        Json::Obj(entries)
    };

    let reference = |description: &str, definition: &str| -> Json {
        // `description` beside a bare `$ref` is dropped by a draft-07 reader, so the annotation goes
        // on the wrapper and the reference sits under `allOf` where it keeps its meaning.
        Json::Obj(vec![
            ("description".to_owned(), Json::Str(description.to_owned())),
            (
                "allOf".to_owned(),
                Json::Arr(vec![Json::Obj(vec![(
                    "$ref".to_owned(),
                    Json::Str(format!("#/definitions/{definition}")),
                )])]),
            ),
        ])
    };
    let table_of = |description: &str, definition: &str| -> Json {
        Json::Obj(vec![
            ("description".to_owned(), Json::Str(description.to_owned())),
            ("type".to_owned(), Json::Str("object".to_owned())),
            (
                "additionalProperties".to_owned(),
                Json::Obj(vec![(
                    "$ref".to_owned(),
                    Json::Str(format!("#/definitions/{definition}")),
                )]),
            ),
        ])
    };

    // Driven by `TOP_LEVEL_KEYS` so a table this version gains is a schema entry it cannot be
    // generated without — the same set `parse_str` refuses anything outside of.
    let top_level: Vec<(String, Json)> = config::TOP_LEVEL_KEYS
        .iter()
        .map(|key| {
            let value = match *key {
                "defaults" => reference(
                    "Settings every crate starts from — the lowest layer of the merge.",
                    "settings",
                ),
                "profiles" => table_of(
                    "Named shared layers, applied by the crates whose `profiles` list names them.",
                    "settings",
                ),
                "crates" => table_of(
                    "One table per generated crate, keyed by the crate's name.",
                    "crate",
                ),
                "runtime" => reference(
                    "Config-wide runtime settings: which crate exports the shared serialization \
                     runtime, and what every crate imports.",
                    "runtime",
                ),
                other => panic!(
                    "top-level table `[{other}]` has no schema entry; every entry in \
                     `TOP_LEVEL_KEYS` needs one, or an editor accepts a table it cannot check"
                ),
            };
            ((*key).to_owned(), value)
        })
        .collect();

    let document = Json::Obj(vec![
        (
            "$schema".to_owned(),
            Json::Str("http://json-schema.org/draft-07/schema#".to_owned()),
        ),
        (
            "title".to_owned(),
            Json::Str("cddl-codegen config".to_owned()),
        ),
        (
            "description".to_owned(),
            Json::Str(
                "Schema for a cddl-codegen `--config` TOML file. Generated from `struct Settings`, \
                 `struct CrateEntry` and `struct Runtime` in src/config.rs by the \
                 `editor_schema_matches_the_config_surface` test — edit those and re-bless rather \
                 than editing this file."
                    .to_owned(),
            ),
        ),
        ("type".to_owned(), Json::Str("object".to_owned())),
        ("additionalProperties".to_owned(), Json::Bool(false)),
        ("properties".to_owned(), Json::Obj(top_level)),
        (
            "definitions".to_owned(),
            Json::Obj(vec![
                ("settings".to_owned(), definition(settings_props, None)),
                (
                    "crate".to_owned(),
                    definition(crate_props, Some(required.to_vec())),
                ),
                ("runtime".to_owned(), definition(runtime_props, None)),
            ]),
        ),
    ]);

    format!("{}\n", render_json(&document, 0))
}

/// Drift gate for the committed editor schema (`docs/editor/cddl-codegen-config.schema.json`).
///
/// The schema is what an editor checks a config against, so it is a THIRD spelling of the key set —
/// and the one nothing else would notice going stale, since the tool never reads it. This builds the
/// document from the same `syn` read of `struct Settings` / `struct CrateEntry` / `struct Runtime`
/// that `config_keys_match_cli_fields` uses, and requires the committed file to be it byte for byte.
/// A key added to the config therefore either appears in the schema or fails here.
///
/// `BLESS_EDITOR_SCHEMA=1 cargo test --bin cddl-codegen editor_schema_matches_the_config_surface`
/// rewrites the committed file (the repo's blessing convention, as for `manifest_template_drift`).
#[test]
fn editor_schema_matches_the_config_surface() {
    let built = build_editor_schema();
    // A hand-written emitter must not be able to commit a document no editor can load.
    serde_json::from_str::<serde_json::Value>(&built)
        .unwrap_or_else(|e| panic!("the built schema is not valid JSON: {e}\n{built}"));

    let path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"))).join(EDITOR_SCHEMA_PATH);
    let committed = std::fs::read_to_string(&path).unwrap_or_default();
    if built == committed {
        return;
    }
    if std::env::var("BLESS_EDITOR_SCHEMA")
        .map(|v| v == "1")
        .unwrap_or(false)
    {
        std::fs::create_dir_all(path.parent().unwrap())
            .unwrap_or_else(|e| panic!("create {}: {e}", path.display()));
        std::fs::write(&path, &built).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
        return;
    }
    let first_diff = built
        .lines()
        .zip(committed.lines())
        .position(|(a, b)| a != b)
        .map(|i| {
            format!(
                "first difference at line {}:\n  built:     {}\n  committed: {}",
                i + 1,
                built.lines().nth(i).unwrap_or(""),
                committed.lines().nth(i).unwrap_or("")
            )
        })
        .unwrap_or_else(|| {
            format!(
                "the files agree up to line {}, then one ends: built has {} line(s), committed {}",
                built.lines().count().min(committed.lines().count()),
                built.lines().count(),
                committed.lines().count()
            )
        });
    panic!(
        "{EDITOR_SCHEMA_PATH} is stale against the config surface it describes; {first_diff}\n\
         regenerate with `BLESS_EDITOR_SCHEMA=1 cargo test --bin cddl-codegen \
         editor_schema_matches_the_config_surface`"
    );
}

/// The two `generation::layout` constants whose EMITTER half is a data file rather than code.
///
/// The layout constants are load-bearing at both ends by construction — the emitter writes the path
/// the constant names and the config derives its flag values from the same constant, so a rename is
/// a type-checked edit. The two cargo package suffixes are the exception: the generated
/// `package.name` is written by the manifest change log, whose derived template carries
/// `cddl-lib-wasm` / `cddl-lib-json-schema-gen` as TEXT (`ops_from_log` substitutes `--lib-name` for
/// the `cddl-lib` placeholder). No constant can reach into a `.toml`, so this asserts across the
/// seam instead: a suffix edited on one side without the other makes every derived `--wasm-dep` /
/// `--json-gen-dep` name a package cargo cannot resolve.
#[test]
fn the_package_suffixes_are_the_ones_the_manifest_templates_carry() {
    for (template, suffix) in [
        (
            "Cargo_wasm.toml",
            crate::generation::layout::WASM_PACKAGE_SUFFIX,
        ),
        (
            "Cargo_json_gen.toml",
            crate::generation::layout::JSON_GEN_PACKAGE_SUFFIX,
        ),
    ] {
        let path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/static")).join(template);
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("{} is readable: {e}", path.display()));
        let expected = format!("name = \"cddl-lib{suffix}\"");
        assert!(
            text.contains(&expected),
            "static/{template} does not carry `{expected}`: the emitted package name and the suffix \
             a config predicts for it have drifted"
        );
    }
}

/// `JSON_GEN_MANIFEST` names a file inside `JSON_GEN_DIR` — the one relationship between the layout
/// constants that a single line does not show, and the one a config depends on (it resolves the
/// json-gen crate by DIRECTORY while the emitter writes the manifest by full path).
#[test]
fn the_json_gen_manifest_lives_in_the_json_gen_directory() {
    assert_eq!(
        crate::generation::layout::JSON_GEN_MANIFEST,
        format!("{}/Cargo.toml", crate::generation::layout::JSON_GEN_DIR),
    );
}

/// The exclusion list above is only honest if the per-crate keys really are absent from `Settings`;
/// otherwise a key could be settable in `[defaults]` while the gate still passed.
#[test]
fn per_crate_keys_are_absent_from_the_shared_settings_struct() {
    let settings = Settings::default();
    let rendered = format!("{settings:?}");
    for key in [
        "input",
        "output",
        "lib_name",
        "profiles",
        "deps",
        "wasm_reexports",
        "json_schema_deps",
    ] {
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

/// Two crates cannot generate into one directory, nor one inside another's.
///
/// This is the only destructive shape the config can catch, and it catches it before anything is
/// written. Generation replaces a crate's `src/generated/**` wholesale, so the crate running second
/// erases the first's modules while the first's seed-once `lib.rs` survives — a crate root belonging
/// to one spec sitting over a generated tree belonging to another, reported as success. It is also
/// the copy-paste error a multi-crate TOML invites most: duplicate a `[crates.*]` block, edit
/// `input`, forget `output`.
///
/// The last case is the one a naive string prefix gets wrong: `gen/alphabet` starts with the TEXT of
/// `gen/alpha` while being its sibling, not its child, so it must be accepted.
///
/// A `.` or `..` in an `output` is a SPELLING of the same directory, not a different one (the
/// derivations already treat it so), so the guard compares normalized paths — otherwise
/// `./gen/x` vs `gen/x` walks past the copy-paste check and the second crate silently erases the
/// first's generated tree, which is the exact destruction this guard exists for.
#[test]
fn two_crates_cannot_generate_into_one_directory() {
    let config = |a: &str, b: &str| {
        format!(
            "[crates.alpha]\ninput = \"a.cddl\"\noutput = \"{a}\"\n\
             [crates.beta]\ninput = \"b.cddl\"\noutput = \"{b}\"\n"
        )
    };

    let same = error(&config("gen/shared", "gen/shared"));
    assert!(
        same.contains("[crates.alpha]")
            && same.contains("[crates.beta]")
            && same.contains("output"),
        "an identical output must name both crates and the key that fixes it, got: {same}"
    );

    let nested = error(&config("gen/alpha", "gen/alpha/inner"));
    assert!(
        nested.contains("gen/alpha/inner") && nested.contains("contains"),
        "a nested output must say which directory contains which, got: {nested}"
    );
    // Containment is symmetric: the same pair the other way round is the same refusal.
    assert!(
        error(&config("gen/alpha/inner", "gen/alpha")).contains("contains"),
        "containment must be caught whichever crate declares the outer directory"
    );

    parse(&config("gen/alpha", "gen/alphabet"));

    assert!(
        error(&config("./gen/shared", "gen/shared")).contains("both generate into"),
        "a leading `./` must not disguise an identical output"
    );
    assert!(
        error(&config("gen/extra/../shared", "gen/shared")).contains("both generate into"),
        "a `..` spelling must not disguise an identical output"
    );
    // `.` is the config's own directory, which contains every relative output. Checked under an
    // absolute base directory because that is what `load` now guarantees every real run has.
    assert!(
        config::parse_str(&config(".", "gen/beta_out"), Path::new("/cfgroot"))
            .expect_err("`.` contains every relative output")
            .contains("contains"),
        "an output of `.` must be caught as containing its sibling crates' outputs"
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

/// The committed-state verdict, over hand-written files so each input can be varied one at a time.
///
/// Four of these five cases are about NOT firing, which is the property that matters most: a verdict
/// that fails the build has to be silent on everything it cannot actually read as a broken
/// workspace, or it is worse than the silence it replaces.
#[test]
fn the_committed_verdict_fires_only_on_a_wrapper_the_dependency_does_not_host() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_verdict_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    let sidecar = dir.join("gen/ledger/wasm/src/generated/borrowed_collections.rs");
    let index = dir.join("gen/core/wasm/src/generated/collections.rs");
    for path in [&sidecar, &index] {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    }
    let config = config::parse_str(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
        &dir,
    )
    .unwrap();
    let config_path = dir.join("codegen.toml");
    let verdict = |selected: &[&str]| {
        config
            .committed_verdict(
                &config_path,
                &selected.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
            )
            .expect("the verdict is a read, not a validation")
    };
    let borrows = |rows: &str| {
        std::fs::write(
            &sidecar,
            format!("pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[{rows}];\n"),
        )
        .unwrap();
    };

    // No sidecar at all: a consumer that has never generated borrows nothing.
    std::fs::write(&index, "").unwrap();
    assert_eq!(verdict(&[]), None);

    // Borrowing what the dependency hosts.
    borrows(r#"("core", "CoreThingList", "[* core_thing]")"#);
    std::fs::write(
        &index,
        "// banner\npub use crate::generated::requested_collections::CoreThingList;\n",
    )
    .unwrap();
    assert_eq!(verdict(&[]), None);

    // A row addressed to a DIFFERENT dependency is not this edge's to satisfy — one sidecar can
    // name several deps, and only the ones this config draws an edge to are checked here.
    borrows(
        r#"("core", "CoreThingList", "[* core_thing]"), ("elsewhere", "GhostList", "[* ghost]")"#,
    );
    assert_eq!(verdict(&[]), None);

    // Borrowing what it does not host: the verdict, naming both crates, the wrapper, and the
    // dependency-alone regen that hosts it.
    borrows(r#"("core", "MapU64ToCoreThing", "{* uint => core_thing}")"#);
    let missing = verdict(&[]).expect("an unhosted wrapper is a workspace that does not build");
    for expected in ["`core`", "`ledger`", "MapU64ToCoreThing"] {
        assert!(
            missing.contains(expected),
            "must carry {expected:?}: {missing}"
        );
    }
    assert!(
        missing.contains(&format!("--config {} core", config_path.display())),
        "and the command that settles it: {missing}"
    );

    // The selection reaches the edge from EITHER end. Naming only the consumer is the subset case
    // the bracketing warning is structurally blind to — the dependency is not in the run, so
    // nothing about it is watched — and it is exactly where this must still fire.
    assert!(verdict(&["ledger"]).is_some());
    assert!(verdict(&["core"]).is_some());

    // An index that cannot be read as an index contributes nothing rather than failing the build:
    // the strict reader of that file is the consumer's own run, which hard-errors on it.
    std::fs::write(&index, "this is not a wrapper index\n").unwrap();
    let mangled = verdict(&[]).expect("an unreadable index still provides no wrapper");
    assert!(mangled.contains("MapU64ToCoreThing"));

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

    // ONE cold run, and it settles: the convergence pass re-runs `core` after `ledger` has recorded
    // the wrapper it borrows, so the invocation exits 0 over a workspace that hosts it. The
    // idempotence half is pinned by [`a_config_run_converges_and_then_repeats_byte_for_byte`].
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run must converge and exit 0");

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

    // A second run has nothing left to do, which is what convergence means here.
    config::generate(&config_path, &[], None, None).expect("a warm config run must generate");

    let _ = std::fs::remove_dir_all(&dir);
}

/// A dependency declared BOTH as an extern-interface import AND as a physical stub in the consumer's
/// own input tree is refused during expansion — before any crate generates — and in the config's
/// vocabulary rather than the flag's.
///
/// The generator refuses the same shape, and still does; but it reaches it mid-generation, so the
/// dependency (which generates first) is already fully written to disk when the consumer aborts, and
/// the message names an `--extern-import <dep>=<path>` value the config derived rather than any key
/// the user wrote. Both halves are pinned: nothing may exist under the output root, and the message
/// must name the key that declared the edge.
#[test]
fn a_dependency_declared_twice_is_refused_before_any_crate_generates() {
    let dir =
        std::env::temp_dir().join(format!("cddl_config_stub_conflict_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs/ledger/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/core")).unwrap();
    std::fs::write(dir.join("specs/core.cddl"), "foo = [a: uint, b: text]\n").unwrap();
    std::fs::write(dir.join("specs/ledger/lib.cddl"), "bar = [f: foo]\n").unwrap();
    std::fs::write(
        dir.join("specs/ledger/_CDDL_CODEGEN_EXTERN_DEPS_DIR_/core/mod.cddl"),
        "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n",
    )
    .unwrap();
    let write_config = |file: &str, edge: &str| {
        let path = dir.join(file);
        std::fs::write(
            &path,
            format!(
                "[defaults]\nstatic-dir = \"{}/static\"\n\n\
                 [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\n\
                 [crates.ledger]\ninput = \"specs/ledger\"\noutput = \"gen/ledger\"\n{edge}",
                env!("CARGO_MANIFEST_DIR")
            ),
        )
        .unwrap();
        path
    };

    // The sugar: `deps` derives the `--extern-import` that collides with the stub.
    let derived = write_config("derived.toml", "deps = [\"core\"]\n");
    let err = config::generate(&derived, &[], None, None)
        .expect_err("a dependency declared by `deps` AND by a stub must be refused")
        .to_string();
    assert!(
        err.contains("[crates.ledger].deps"),
        "the refusal must name the key that declared the edge, got:\n{err}"
    );
    assert!(
        err.contains("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/core"),
        "and the stub directory it collides with, got:\n{err}"
    );
    assert!(
        !err.contains("--extern-import"),
        "and must not name a flag this user never typed, got:\n{err}"
    );
    // The promise the move exists to keep. `core` generates first, so with the check left where the
    // generator reaches it, its whole output tree would be on disk by now.
    assert!(
        !dir.join("gen").exists(),
        "no crate may have generated: the config is checked before any crate runs"
    );

    // `--print-flags` performs the real expansion, so it refuses identically.
    let listed = config::print_flags(&derived, &[], None, None)
        .expect_err("--print-flags must refuse what a run refuses")
        .to_string();
    assert_eq!(
        listed, err,
        "--print-flags must fail with the identical message"
    );

    // A hand-written sub-table entry is the same conflict, attributed to the key that user typed.
    let hand = write_config(
        "hand.toml",
        "\n[crates.ledger.extern-import]\ncore = \"gen/core/extern-interface/core\"\n",
    );
    let err = config::generate(&hand, &[], None, None)
        .expect_err("the same conflict declared by hand must be refused")
        .to_string();
    assert!(
        err.contains("[crates.ledger].extern-import"),
        "a hand-written entry is attributed to the flag-named key, got:\n{err}"
    );
    assert!(
        !dir.join("gen").exists(),
        "no crate may have generated for the hand-written spelling either"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// `[runtime]` — one shared static runtime for the whole config
// ---------------------------------------------------------------------------------------------

/// The derivation runs during EXPANSION (it needs each crate's finished `Cli`), so its refusals are
/// not reachable through the parse-time [`error`] helper.
fn expand_error(text: &str) -> String {
    parse(text)
        .expand(&[])
        .err()
        .unwrap_or_else(|| panic!("expansion must be rejected:\n{text}"))
}

fn expand_all(text: &str) -> std::collections::BTreeMap<String, Cli> {
    parse(text)
        .expand(&[])
        .unwrap_or_else(|e| panic!("must expand: {e}"))
        .into_iter()
        .collect()
}

/// Two crates whose flavors agree on everything, so a fixture can be about one key at a time.
const TWO_PLAIN_CRATES: &str = "\n[crates.a]\ninput = \"a.cddl\"\noutput = \"gen/a\"\n\n\
                                [crates.b]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n";

/// The flavor axes are read off an expanded `Cli`, and the join over a config where nobody sets one
/// is that axis's bottom — which is only true while every axis's clap default IS the bottom. Pinned
/// here rather than assumed, because a default flipped upstream would silently invert a join without
/// changing a line of this module.
#[test]
fn every_runtime_flavor_axis_defaults_to_the_bottom_of_that_axis() {
    use clap::Parser;

    let cli = Cli::parse_from(["cddl-codegen", "--input", "s.cddl", "--output", "gen"]);
    assert!(!cli.preserve_encodings, "preserve-encodings");
    assert!(!cli.canonical_form, "canonical-form");
    assert_eq!(cli.deserialize_depth_limit, None, "deserialize-depth-limit");
    assert!(!cli.json_serde_derives, "json-serde-derives");
    assert!(!cli.json_schema_export, "json-schema-export");
}

/// `common-import` reaches every crate, and it is the LOWEST layer: an explicit
/// `common-import-override` anywhere in the merge chain wins for the crates it reaches, which is the
/// exotic case (one crate importing a different runtime) the shared key is sugar for the common one
/// of.
#[test]
fn runtime_common_import_reaches_every_crate_and_an_explicit_key_wins() {
    let expanded = expand_all(
        r#"
[runtime]
common-import = "cddl_runtime"

[crates.a]
input = "a.cddl"
output = "gen/a"

[crates.b]
input = "b.cddl"
output = "gen/b"
common-import-override = "other_runtime"
"#,
    );
    assert_eq!(
        expanded["a"].common_import_override.as_deref(),
        Some("cddl_runtime"),
        "a crate that names no runtime gets the shared one"
    );
    assert_eq!(
        expanded["b"].common_import_override.as_deref(),
        Some("other_runtime"),
        "a crate's own key wins over the shared one"
    );
}

/// `common-import` is expansion, not a second knob: the flag value it produces is the one a hand
/// invocation spells.
#[test]
fn runtime_common_import_expands_to_the_hand_written_flag() {
    use clap::Parser;

    let cli = expand_one(&format!(
        "[runtime]\ncommon-import = \"cddl_runtime\"\n{MINIMAL_CRATE}"
    ));
    let from_flags = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "spec.cddl",
        "--output",
        "gen",
        "--lib-name",
        "demo",
        "--common-import-override",
        "cddl_runtime",
    ]);
    assert_eq!(
        cli.common_import_override,
        from_flags.common_import_override
    );
}

/// The carrier is the crate whose flavor is the join over the MAX axes — not the first crate, and
/// not the one that happens to name the runtime. Only `b` has both json axes, so only `b` can export
/// a runtime `a` also resolves against.
#[test]
fn runtime_carrier_is_the_crate_whose_flavor_is_the_join() {
    let config = parse(
        r#"
[runtime]
export-static-crate = "crates/runtime"
common-import = "cddl_runtime"

[defaults]
preserve-encodings = true
canonical-form = true

[crates.a]
input = "a.cddl"
output = "gen/a"
json-serde-derives = true

[crates.b]
input = "b.cddl"
output = "gen/b"
json-serde-derives = true
json-schema-export = true
"#,
    );
    let choice = config
        .runtime_report()
        .expect("the flavors agree, so a carrier exists")
        .expect("`export-static-crate` is set, so a carrier is chosen");
    assert_eq!(choice.carrier, "b");
    assert!(
        choice.notes.iter().any(|n| n.contains("`b` carries")),
        "the run must say which crate carries the export, got: {:?}",
        choice.notes
    );

    let expanded = expand_all(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[defaults]
preserve-encodings = true
canonical-form = true

[crates.a]
input = "a.cddl"
output = "gen/a"
json-serde-derives = true

[crates.b]
input = "b.cddl"
output = "gen/b"
json-serde-derives = true
json-schema-export = true
"#,
    );
    assert_eq!(
        expanded["b"].export_static_crate,
        Some(std::path::PathBuf::from("crates/runtime")),
        "the carrier's invocation gets the flag"
    );
    assert_eq!(
        expanded["a"].export_static_crate, None,
        "exactly one invocation carries it"
    );
}

/// When several crates tie on the join the choice must be deterministic, since the run's output
/// names it. Crate-name order is the order this config's tables are held in.
#[test]
fn runtime_carrier_ties_break_deterministically() {
    let text = format!("[runtime]\nexport-static-crate = \"crates/runtime\"\n{TWO_PLAIN_CRATES}");
    for _ in 0..3 {
        let choice = parse(&text)
            .runtime_report()
            .expect("identical flavors")
            .expect("a carrier is chosen");
        assert_eq!(choice.carrier, "a");
    }
}

/// The CML shape, which is exactly the configuration this table exists to make honest: one crate at
/// a reduced flavor and one at the full one cannot share a runtime, and the refusal names the axis
/// and which crates hold which value.
#[test]
fn runtime_equality_axis_disagreement_is_an_error_naming_the_axis() {
    let err = expand_error(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[crates.chain]
input = "chain.cddl"
output = "gen/chain"
preserve-encodings = true
canonical-form = true

[crates.cip25]
input = "cip25.cddl"
output = "gen/cip25"
"#,
    );
    assert!(
        err.contains("`preserve-encodings` (`false` in `cip25`, `true` in `chain`)"),
        "the refusal must name the axis and which crates hold which value, got:\n{err}"
    );
    assert!(
        err.contains("`canonical-form` (`false` in `cip25`, `true` in `chain`)"),
        "every disagreeing axis is named, not just the first, got:\n{err}"
    );
    assert!(
        err.contains("flavor-from"),
        "the refusal must name the remedy, got:\n{err}"
    );
}

/// A depth limit is a contract about which documents are ACCEPTED, baked by value into the exported
/// `AnyCbor` guard — a mismatch compiles cleanly and silently guards one crate's `any` values at
/// another crate's limit, which is why it is an equality axis and why "unset" counts as a value.
#[test]
fn runtime_depth_limit_must_be_identical_including_unset() {
    let differing_values = expand_error(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[crates.a]
input = "a.cddl"
output = "gen/a"
deserialize-depth-limit = 16

[crates.b]
input = "b.cddl"
output = "gen/b"
deserialize-depth-limit = 64
"#,
    );
    assert!(
        differing_values.contains("`deserialize-depth-limit` (`16` in `a`, `64` in `b`)"),
        "two different limits must be refused by value, got:\n{differing_values}"
    );

    let against_unset = expand_error(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[crates.a]
input = "a.cddl"
output = "gen/a"
deserialize-depth-limit = 16

[crates.b]
input = "b.cddl"
output = "gen/b"
"#,
    );
    assert!(
        against_unset.contains("`deserialize-depth-limit` (`16` in `a`, `unset` in `b`)"),
        "an absent limit is a VALUE, not an absence to be filled in, got:\n{against_unset}"
    );
}

/// The max axes can be split so that no crate holds the join. That is the honest diagnostic:
/// `--export-static-crate` exports the flag set of one invocation, so a runtime nobody's flags
/// describe cannot be written at all.
#[test]
fn runtime_no_join_crate_is_an_error_naming_which_crate_supplies_each_axis() {
    let err = expand_error(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[crates.a]
input = "a.cddl"
output = "gen/a"
json-serde-derives = true

[crates.b]
input = "b.cddl"
output = "gen/b"
json-schema-export = true
"#,
    );
    assert!(
        err.contains("json-serde-derives comes from `a`"),
        "the refusal must name which crate supplies each axis, got:\n{err}"
    );
    assert!(
        err.contains("json-schema-export comes from `b`"),
        "…and the other one, got:\n{err}"
    );
    assert!(
        err.contains("no single crate has all of it"),
        "the refusal must say WHY there is no carrier, got:\n{err}"
    );
}

/// `flavor-from` overrides the derivation, fires no warning (the user declared this, and a warning
/// that fires on every run of the motivating consumer trains people to ignore warnings), and states
/// once what was accepted: which crates are at a flavor the runtime does not match, and the two
/// constructs that would break them.
#[test]
fn runtime_flavor_from_overrides_the_derivation_and_states_the_gap() {
    let config = parse(
        r#"
[runtime]
export-static-crate = "crates/runtime"
common-import = "cml_core"
flavor-from = "chain"

[crates.chain]
input = "chain.cddl"
output = "gen/chain"
preserve-encodings = true
canonical-form = true

[crates.cip25]
input = "cip25.cddl"
output = "gen/cip25"
"#,
    );
    let choice = config
        .runtime_report()
        .expect("`flavor-from` accepts what the derivation refuses")
        .expect("a carrier is chosen");
    assert_eq!(choice.carrier, "chain");
    let notes = choice.notes.join("\n");
    assert!(
        notes.contains("`chain` carries --export-static-crate, declared by `flavor-from`"),
        "the run must say who carries it and that it was declared, got:\n{notes}"
    );
    assert!(
        notes.contains("`cip25`"),
        "the accepted gap must NAME the crates it applies to, got:\n{notes}"
    );
    assert!(
        notes.contains("{+ K => V}") && notes.contains("NonEmptyMap"),
        "the accepted gap must name the map construct that breaks it, got:\n{notes}"
    );
    assert!(
        notes.contains("`any`") && notes.contains("AnyCbor"),
        "…and the `any` construct, got:\n{notes}"
    );
    assert!(
        !notes.to_lowercase().contains("warning"),
        "a declared choice is a statement, not a warning, got:\n{notes}"
    );

    let expanded = expand_all(
        r#"
[runtime]
export-static-crate = "crates/runtime"
flavor-from = "chain"

[crates.chain]
input = "chain.cddl"
output = "gen/chain"
preserve-encodings = true
canonical-form = true

[crates.cip25]
input = "cip25.cddl"
output = "gen/cip25"
"#,
    );
    assert_eq!(
        expanded["chain"].export_static_crate,
        Some(std::path::PathBuf::from("crates/runtime"))
    );
    assert_eq!(expanded["cip25"].export_static_crate, None);
}

/// `flavor-from` naming the crate the derivation would have picked anyway accepts nothing, so it
/// states nothing beyond who carries the export.
#[test]
fn runtime_flavor_from_on_the_join_crate_states_no_gap() {
    let choice = parse(&format!(
        "[runtime]\nexport-static-crate = \"crates/runtime\"\nflavor-from = \"a\"\n{TWO_PLAIN_CRATES}"
    ))
    .runtime_report()
    .expect("identical flavors")
    .expect("a carrier is chosen");
    assert_eq!(
        choice.notes.len(),
        1,
        "nothing was accepted: {:?}",
        choice.notes
    );
}

/// Which crate can carry the runtime is a property of the CONFIG, so selecting a subset must reject
/// the same configs a full run rejects rather than pass because the offending crate sat this one out.
#[test]
fn runtime_derivation_is_independent_of_the_selection() {
    let config = parse(
        r#"
[runtime]
export-static-crate = "crates/runtime"

[crates.chain]
input = "chain.cddl"
output = "gen/chain"
preserve-encodings = true
canonical-form = true

[crates.cip25]
input = "cip25.cddl"
output = "gen/cip25"
"#,
    );
    let Err(err) = config.expand(&["chain".to_owned()]) else {
        panic!("selecting one crate must not hide the config's flavor split");
    };
    assert!(err.contains("preserve-encodings"), "got:\n{err}");
}

/// Two static-runtime exports in one config is a mistake, not a configuration: letting either win
/// would make which runtime survives depend on generation order. Rejected from every layer a
/// `Settings` can come from.
#[test]
fn a_second_export_static_crate_alongside_runtime_is_an_error() {
    for (label, layer) in [
        (
            "[crates.demo]",
            "[crates.demo]\nexport-static-crate = \"other\"\ninput = \"s.cddl\"\noutput = \"gen\"\n",
        ),
        (
            "[defaults]",
            "[defaults]\nexport-static-crate = \"other\"\n[crates.demo]\ninput = \"s.cddl\"\noutput = \"gen\"\n",
        ),
        (
            "[profiles.p]",
            "[profiles.p]\nexport-static-crate = \"other\"\n[crates.demo]\nprofiles = [\"p\"]\ninput = \"s.cddl\"\noutput = \"gen\"\n",
        ),
    ] {
        let err = error(&format!(
            "[runtime]\nexport-static-crate = \"crates/runtime\"\n{layer}"
        ));
        assert!(
            err.contains(label) && err.contains("generation order"),
            "the refusal must name the offending layer ({label}) and why it is not a precedence \
             question, got:\n{err}"
        );
    }
}

/// Two export SITES with no `[runtime]` table at all. `export-static-crate` is an ordinary
/// `Settings` key, so one `[defaults]` line is a single layer and one export site PER CRATE — the
/// shape the `[runtime]`-gated refusal above never sees, and the one that is destructive: two crates
/// exporting at differing flavors is not idempotent (measured: the exported `any_cbor.rs` grew
/// 62 → 143 → 224 → 305 `compile_error!` blocks over four runs of one unchanged config, exit 0 each
/// time). Every layer a shared value can come from produces it.
#[test]
fn two_export_static_crate_sites_without_a_runtime_table_are_an_error() {
    for (what, text) in [
        (
            "one shared [defaults] value reaching both crates",
            "[defaults]\nexport-static-crate = \"runtime\"\n\
             [crates.a]\ninput = \"a.cddl\"\noutput = \"gen/a\"\n\
             [crates.b]\ninput = \"b.cddl\"\noutput = \"gen/b\"\n",
        ),
        (
            "one shared profile applied to both crates",
            "[profiles.shared]\nexport-static-crate = \"runtime\"\n\
             [crates.a]\ninput = \"a.cddl\"\noutput = \"gen/a\"\nprofiles = [\"shared\"]\n\
             [crates.b]\ninput = \"b.cddl\"\noutput = \"gen/b\"\nprofiles = [\"shared\"]\n",
        ),
        (
            "two crate tables each naming one",
            "[crates.a]\ninput = \"a.cddl\"\noutput = \"gen/a\"\nexport-static-crate = \"runtime\"\n\
             [crates.b]\ninput = \"b.cddl\"\noutput = \"gen/b\"\nexport-static-crate = \"other\"\n",
        ),
    ] {
        let err = error(text);
        assert!(
            err.contains("`a`") && err.contains("`b`"),
            "the refusal must name both exporting crates ({what}), got:\n{err}"
        );
        assert!(
            err.contains("idempotent"),
            "the refusal must say what breaks, not just that it is refused ({what}), got:\n{err}"
        );
    }
}

/// The refusal is about the SECOND site, so a shared value reaching exactly one crate stays legal —
/// otherwise a single-crate config could not set the key from `[defaults]` at all.
#[test]
fn one_export_static_crate_site_from_a_shared_layer_stays_legal() {
    let cli = expand_one(
        "[defaults]\nexport-static-crate = \"crates/runtime\"\n\
         [crates.demo]\ninput = \"s.cddl\"\noutput = \"gen\"\n",
    );
    assert_eq!(
        cli.export_static_crate,
        Some(std::path::PathBuf::from("crates/runtime"))
    );
}

/// A per-crate `export-static-crate` with NO `[runtime].export-static-crate` is the hand-placed flag
/// this table replaces, and stays legal — the refusal above is about two exports, not about the key.
#[test]
fn a_lone_per_crate_export_static_crate_stays_legal() {
    let cli = expand_one(
        "[runtime]\ncommon-import = \"cddl_runtime\"\n[crates.demo]\n\
         export-static-crate = \"crates/runtime\"\ninput = \"s.cddl\"\noutput = \"gen\"\n",
    );
    assert_eq!(
        cli.export_static_crate,
        Some(std::path::PathBuf::from("crates/runtime"))
    );
}

/// `[runtime].export-static-crate` resolves against the config file's directory, like every other
/// path key — the property that makes a checked-in config work from any CWD.
#[test]
fn runtime_export_static_crate_resolves_against_the_config_directory() {
    let config = config::parse_str(
        &format!("[runtime]\nexport-static-crate = \"crates/runtime\"\n{MINIMAL_CRATE}"),
        Path::new("/proj/cfg"),
    )
    .expect("must parse");
    let expanded = config.expand(&[]).expect("must expand");
    assert_eq!(
        expanded[0].1.export_static_crate,
        Some(std::path::PathBuf::from("/proj/cfg/crates/runtime"))
    );
}

/// An empty `[runtime]` asks for nothing, which is a typo rather than a request.
#[test]
fn an_empty_runtime_table_is_an_error() {
    let err = error(&format!("[runtime]\n{MINIMAL_CRATE}"));
    assert!(err.contains("neither"), "got:\n{err}");
}

/// An unknown key in `[runtime]` is rejected like every other typo.
#[test]
fn an_unknown_runtime_key_is_an_error() {
    let err = error(&format!(
        "[runtime]\ncommon-imports = \"x\"\n{MINIMAL_CRATE}"
    ));
    assert!(
        err.contains("[runtime]") && err.contains("common-imports"),
        "got:\n{err}"
    );
}

/// `flavor-from` must name a crate in this config: it selects whose flag set the runtime IS, so a
/// name from outside cannot answer that.
#[test]
fn runtime_flavor_from_must_name_a_configured_crate() {
    let err = error(&format!(
        "[runtime]\nexport-static-crate = \"r\"\nflavor-from = \"nope\"\n{MINIMAL_CRATE}"
    ));
    assert!(
        err.contains("`nope`") && err.contains("`demo`"),
        "the refusal must name the unknown crate and list the configured ones, got:\n{err}"
    );
}

/// `flavor-from` with nothing to carry is a key that cannot mean anything.
#[test]
fn runtime_flavor_from_without_an_export_is_an_error() {
    let err = error(&format!(
        "[runtime]\ncommon-import = \"r\"\nflavor-from = \"demo\"\n{MINIMAL_CRATE}"
    ));
    assert!(err.contains("no export to carry"), "got:\n{err}");
}

/// The `[runtime]` table on real disk, with the leg that no other test in this repo has: a shared
/// runtime exported at ONE crate's flavor, and a crate at a DIFFERENT flavor compiled against it.
///
/// A generation-only assertion cannot catch an inadequate runtime — that is the entire failure this
/// table exists to prevent, and it only shows up at `cargo check`. So this generates a CML-shaped
/// config (a full-flavor crate carrying the export, a reduced-flavor crate importing it via
/// `common-import`), hand-writes the target crate's `lib.rs` exactly as the tool's new-static-file
/// notice instructs, and compiles the whole workspace.
///
/// The MUTATION leg at the end is what makes the run's accepted-gap statement a tested claim rather
/// than prose: adding a `{+ K => V}` to the reduced crate's spec — one of the two constructs that
/// statement names — must break the build, because under `--preserve-encodings` the runtime's
/// `NonEmptyMap` is backed by `OrderedHashMap` while the reduced crate builds a `BTreeMap`.
#[test]
fn a_runtime_table_exports_a_runtime_the_other_flavor_compiles_against() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_runtime_e2e_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    // The FULL-flavor crate: preserve + canonical + json, and a spec that reaches every runtime type
    // the export composes, so the exported files are exercised rather than merely present. The
    // `bw` bytes newtype is there for one of them specifically: its emitted serde deserializer
    // calls `decode_canonical_hex`, which under `--common-import-override` must resolve into the
    // SHARED runtime rather than a crate-local copy — the split layout's leg of that routing.
    std::fs::write(
        dir.join("specs/full.cddl"),
        "bw = bytes ; @newtype\nfull_rec = [ne: [+ uint], nm: {+ uint => text}, m: {* uint => text}, a: any, b: bw]\n",
    )
    .unwrap();
    // The REDUCED-flavor crate: none of those flags, and a spec that avoids the two constructs the
    // accepted-gap statement names.
    std::fs::write(
        dir.join("specs/reduced.cddl"),
        "reduced_rec = [x: uint, m: {* uint => text}, s: text]\n",
    )
    .unwrap();

    let config_path = dir.join("codegen.toml");
    let config_text = format!(
        "[defaults]\nstatic-dir = \"{}/static\"\nwasm = false\n\n\
         [runtime]\nexport-static-crate = \"runtime\"\ncommon-import = \"cddl_runtime\"\n\
         flavor-from = \"full\"\n\n\
         [crates.full]\ninput = \"specs/full.cddl\"\noutput = \"gen/full\"\nlib-name = \"full-lib\"\n\
         preserve-encodings = true\ncanonical-form = true\njson-serde-derives = true\n\n\
         [crates.reduced]\ninput = \"specs/reduced.cddl\"\noutput = \"gen/reduced\"\n\
         lib-name = \"reduced-lib\"\n",
        env!("CARGO_MANIFEST_DIR")
    );
    std::fs::write(&config_path, &config_text).unwrap();

    // The derivation refuses this shape without `flavor-from`; with it, the run states the gap.
    let choice = config::load(&config_path)
        .expect("must load")
        .runtime_report()
        .expect("`flavor-from` accepts the split")
        .expect("a carrier is chosen");
    assert_eq!(choice.carrier, "full");
    assert!(
        choice.notes.join("\n").contains("`reduced`"),
        "the accepted gap must name the reduced crate, got: {:?}",
        choice.notes
    );

    config::generate(&config_path, &[], None, None)
        .unwrap_or_else(|e| panic!("a cold config run must generate: {e}"));

    // The shared runtime really received the files, in the crate shape `--export-static-crate`
    // documents (`<dir>/src/` plus a merged `<dir>/Cargo.toml`).
    let runtime_src = dir.join("runtime/src");
    for expected in [
        "error.rs",
        "serialization.rs",
        "any_cbor.rs",
        "non_empty.rs",
        "non_empty_map.rs",
        "ordered_hash_map.rs",
    ] {
        assert!(
            runtime_src.join(expected).is_file(),
            "the shared runtime must receive {expected}"
        );
    }
    let manifest = std::fs::read_to_string(dir.join("runtime/Cargo.toml"))
        .expect("the exported runtime gets its manifest too — source and deps are one artifact");
    assert!(
        manifest.contains("serde"),
        "the carrier's json flavor must reach the runtime's dependency list, got:\n{manifest}"
    );
    assert!(
        !dir.join("gen/reduced/rust/src/generated/error.rs").exists(),
        "a --common-import-override crate keeps no local copy of the runtime"
    );
    // The canonical-hex door is one of those runtime items: the full crate's bytes newtype calls
    // it, and the import must name the SHARED runtime. (`cargo check --workspace` below is what
    // proves it RESOLVES; this pins that the path is the override's, so a regression to a
    // crate-local spelling fails here with the reason rather than as a bare E0433.)
    let full_mod = std::fs::read_to_string(dir.join("gen/full/rust/src/generated/mod.rs"))
        .expect("the full crate's generated mod.rs must exist");
    assert!(
        full_mod.contains("use cddl_runtime::serialization::decode_canonical_hex;"),
        "the bytes newtype's hex door must be imported from the shared runtime under \
         --common-import-override, got:\n{full_mod}"
    );

    // The target crate root is HAND-owned: the tool never writes it, and its new-static-file notice
    // says exactly this. Write it the way a consumer would.
    let mut modules: Vec<String> = std::fs::read_dir(&runtime_src)
        .unwrap()
        .filter_map(|e| {
            let name = e.unwrap().file_name().to_string_lossy().into_owned();
            name.strip_suffix(".rs").map(str::to_owned)
        })
        .collect();
    modules.sort();
    std::fs::write(
        runtime_src.join("lib.rs"),
        modules
            .iter()
            .map(|m| format!("pub mod {m};\n"))
            .collect::<String>(),
    )
    .unwrap();

    // A workspace over the three crates, with each generated crate depending on the runtime under
    // the code name `common-import` gave it.
    std::fs::write(
        dir.join("Cargo.toml"),
        "[workspace]\nmembers = [\"runtime\", \"gen/full/rust\", \"gen/reduced/rust\"]\n\
         resolver = \"2\"\n",
    )
    .unwrap();
    let add_runtime_dep = |crate_dir: &str| {
        let path = dir.join(crate_dir).join("Cargo.toml");
        let text = std::fs::read_to_string(&path).unwrap();
        assert!(
            !text.contains("cddl_runtime"),
            "the tool does not write the consumer's dependency on the shared runtime — the \
             --common-import-override docs make that the user's line"
        );
        // Line-anchored on purpose: a bare `text.replace("[dependencies]", …)` also rewrites the
        // spelling wherever it appears mid-line — a comment mentioning the header is enough — and
        // corrupts the manifest it is editing.
        assert!(
            text.contains("\n[dependencies]\n"),
            "the generated manifest must carry a [dependencies] header to anchor on, got:\n{text}"
        );
        std::fs::write(
            &path,
            text.replace(
                "\n[dependencies]\n",
                "\n[dependencies]\ncddl_runtime = { package = \"cddl-runtime\", path = \"../../../runtime\" }\n",
            ),
        )
        .unwrap();
    };
    add_runtime_dep("gen/full/rust");
    add_runtime_dep("gen/reduced/rust");

    let target_dir = dir.join("target");
    let check = crate::tests::integration_tests::tool_cmd("cargo")
        .arg("check")
        .arg("--workspace")
        .current_dir(&dir)
        .env("CARGO_TARGET_DIR", &target_dir)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "the exported runtime must compile BOTH flavors that import it:\n{}\n{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );

    // The mutation leg: the accepted gap the run stated is real. `{+ K => V}` in the reduced crate's
    // spec is `NonEmptyMap<_, BTreeMap-backed>` there and `OrderedHashMap`-backed in the runtime the
    // full-flavor crate exported, so the same workspace must now FAIL.
    std::fs::write(
        dir.join("specs/reduced.cddl"),
        "reduced_rec = [x: uint, nm: {+ uint => text}]\n",
    )
    .unwrap();
    config::generate(&config_path, &["reduced".to_owned()], None, None)
        .expect("the reduced crate must still GENERATE — the gap is a compile-time one");
    let mutated = crate::tests::integration_tests::tool_cmd("cargo")
        .arg("check")
        .arg("--workspace")
        .current_dir(&dir)
        .env("CARGO_TARGET_DIR", &target_dir)
        .output()
        .unwrap();
    assert!(
        !mutated.status.success(),
        "a `{{+ K => V}}` in the reduced crate must break against a preserve-encodings runtime — \
         that is the hazard `flavor-from` makes the user accept, and if it stopped being real the \
         accepted-gap statement is now telling users something false"
    );
    assert!(
        String::from_utf8_lossy(&mutated.stderr).contains("NonEmptyMap"),
        "the failure must be the one the statement names, got:\n{}",
        String::from_utf8_lossy(&mutated.stderr)
    );

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// The published JSON surface — `wasm-reexports` and the threading derivation
// ---------------------------------------------------------------------------------------------

/// CML's own shape, which is what the derivation was designed against: a crate whose spec references
/// one generated crate (`deps`) and whose npm package additionally ships two others
/// (`wasm-reexports`), plus crates with no edges at all.
const CML_SHAPED_SURFACE: &str = "\
[defaults]
json-schema-export = true

[crates.chain]
input = \"specs/chain.cddl\"
output = \"gen/chain\"

[crates.cip25]
input = \"specs/cip25.cddl\"
output = \"gen/cip25\"

[crates.cip36]
input = \"specs/cip36.cddl\"
output = \"gen/cip36\"

[crates.multi-era]
input = \"specs/multi-era.cddl\"
output = \"gen/multi-era\"
deps = [\"chain\"]
wasm-reexports = [\"cip25\", \"cip36\"]
";

/// The derivation itself: `deps ∪ wasm-reexports`, in `deps`-then-`wasm-reexports` order, each entry
/// expanding to BOTH flags — the registrar call and the manifest entry that makes it link. A crate
/// with neither edge threads nothing.
///
/// Order is asserted rather than membership, because it is the input the flag consumes: registration
/// order decides which crate a published-name collision blames.
#[test]
fn threading_derives_from_deps_then_wasm_reexports() {
    let by_name = expand_all(CML_SHAPED_SURFACE);
    assert_eq!(
        by_name["multi-era"].json_schema_dep,
        vec![
            "chain=chain_json_schema_gen",
            "cip25=cip25_json_schema_gen",
            "cip36=cip36_json_schema_gen",
        ],
        "the registrar calls must derive in deps-then-wasm-reexports order"
    );
    assert_eq!(
        by_name["multi-era"].json_gen_dep,
        vec![
            "chain-json-schema-gen=../../../chain/wasm/json-gen",
            "cip25-json-schema-gen=../../../cip25/wasm/json-gen",
            "cip36-json-schema-gen=../../../cip36/wasm/json-gen",
        ],
        "each thread must also derive the [dependencies] entry that lets its call link"
    );
    for lonely in ["chain", "cip25", "cip36"] {
        assert!(
            by_name[lonely].json_schema_dep.is_empty() && by_name[lonely].json_gen_dep.is_empty(),
            "`{lonely}` has no edge, so it must thread nothing"
        );
    }
}

/// The two derived spellings are opposite by design and both are read off the generator rather than
/// restated: the registrar call takes the rust LIB path (dashes normalised to underscores, plus
/// `_json_schema_gen`), the manifest entry takes the cargo PACKAGE name (the `lib-name` verbatim,
/// plus `-json-schema-gen`). Writing either the other way round is the mistake whose error names
/// neither cause — a cargo resolution failure, or an `E0433` on a crate whose name looks right.
#[test]
fn the_two_derived_thread_spellings_are_the_lib_path_and_the_package_name() {
    let by_name = expand_all(
        "[defaults]\njson-schema-export = true\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"cml-chain\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    assert_eq!(
        by_name["ledger"].json_schema_dep,
        vec!["cml_chain=cml_chain_json_schema_gen"]
    );
    assert_eq!(
        by_name["ledger"].json_gen_dep,
        vec!["cml-chain-json-schema-gen=../../../core/wasm/json-gen"]
    );
}

/// **The derived `--json-gen-dep` path is RELATIVE**, under every combination of the two crates'
/// `package-json` settings.
///
/// This is the one derived path in the whole config that is WRITTEN into a committed file rather than
/// read at generation time: it becomes a cargo path dependency in `wasm/json-gen/Cargo.toml`. An
/// absolute value there would bake the checkout location into a file the project commits, so the same
/// config would produce different bytes in a different clone — "same inputs -> same bytes" broken in
/// the most visible way there is. Both endpoints move with their own crate's `package-json`, so all
/// four combinations are counted rather than one.
#[test]
fn the_derived_json_gen_dep_path_is_relative_in_every_layout() {
    for (dep_npm, consumer_npm, expected) in [
        (false, false, "../../../core/wasm/json-gen"),
        (true, false, "../../../core/rust/wasm/json-gen"),
        (false, true, "../../../../core/wasm/json-gen"),
        (true, true, "../../../../core/rust/wasm/json-gen"),
    ] {
        let by_name = expand_all(&format!(
            "[defaults]\njson-schema-export = true\n\
             [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\npackage-json = {dep_npm}\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
             package-json = {consumer_npm}\ndeps = [\"core\"]\n"
        ));
        let derived = &by_name["ledger"].json_gen_dep;
        assert_eq!(
            derived,
            &vec![format!("core-json-schema-gen={expected}")],
            "dep package-json={dep_npm}, consumer package-json={consumer_npm}"
        );
        let path = derived[0]
            .split_once('=')
            .expect("the flag is <package>=<path>")
            .1;
        assert!(
            std::path::Path::new(path).is_relative(),
            "a cargo path dependency is resolved against the manifest holding it, and an absolute \
             value would make the committed manifest machine-specific; got {path}"
        );
    }
}

/// The same, for the case a lexical join cannot answer on its own: one crate's `output` is absolute
/// while the other's is relative, so the two live in different frames until the process directory
/// supplies the missing one. The value must still come out relative — that property does not depend
/// on how the endpoints were spelled.
#[test]
fn the_derived_path_stays_relative_across_a_mixed_absolute_and_relative_output() {
    let absolute = std::env::temp_dir().join("cddl_config_absolute_core");
    let by_name = expand_all(&format!(
        "[defaults]\njson-schema-export = true\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"{}\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
        absolute.display()
    ));
    let derived = &by_name["ledger"].json_gen_dep;
    assert_eq!(derived.len(), 1, "one edge derives one entry");
    let path = derived[0]
        .split_once('=')
        .expect("the flag is <package>=<path>")
        .1;
    assert!(
        std::path::Path::new(path).is_relative(),
        "the derived cargo path dependency must be relative however the outputs were spelled, got \
         {path}"
    );
    assert!(
        path.ends_with("cddl_config_absolute_core/wasm/json-gen"),
        "and it must still reach the dependency's json-gen crate, got {path}"
    );

    // And it is not CWD-DEPENDENT, which the mixed spelling is easily assumed to make it: the
    // process directory only supplies the frame the relative side already denoted, so spelling that
    // side out absolutely — the value a run from any other directory reconstructs — derives exactly
    // the same entry.
    let spelled_out = expand_all(&format!(
        "[defaults]\njson-schema-export = true\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"{}\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"{}/gen/ledger\"\ndeps = [\"core\"]\n",
        absolute.display(),
        std::env::current_dir().unwrap().display()
    ));
    assert_eq!(
        &spelled_out["ledger"].json_gen_dep, derived,
        "the derived entry must not depend on which of the two directories was spelled relatively"
    );
}

/// `.` and `..` in an `output` are ordinary spellings of an ordinary directory, and the derived
/// manifest entry must not be able to tell. `pathdiff` is purely lexical, so without normalization a
/// leading `./` produces a correct-but-mangled value that is WRITTEN INTO A COMMITTED FILE
/// (`../../../.././gen/core/wasm/json-gen`), and a `..` past the common prefix has no lexical answer
/// at all — a hard error saying no relative path leads there. Both spellings must derive exactly what
/// the plainly-spelled equivalent derives.
#[test]
fn a_dot_or_dot_dot_in_an_output_derives_the_same_path_the_plain_spelling_does() {
    let derived = |core: &str, ledger: &str| {
        expand_all(&format!(
            "[defaults]\njson-schema-export = true\n\
             [crates.core]\ninput = \"c.cddl\"\noutput = \"{core}\"\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"{ledger}\"\ndeps = [\"core\"]\n"
        ))["ledger"]
            .json_gen_dep
            .clone()
    };

    let plain = derived("gen/core", "gen/ledger");
    assert_eq!(
        plain,
        vec!["core-json-schema-gen=../../../core/wasm/json-gen".to_owned()],
        "the baseline the two spellings below must match"
    );
    assert_eq!(
        derived("./gen/core", "gen/ledger"),
        plain,
        "a leading `./` must not reach the committed manifest"
    );
    assert_eq!(
        derived("gen/sub/../core", "./gen/./ledger"),
        plain,
        "`.` and `..` anywhere in either endpoint resolve before the diff"
    );

    // A `..` that climbs ABOVE the config directory has no lexical answer — the name of the
    // directory it climbs out of is in neither string — so it is answered through the process CWD
    // like the mixed absolute/relative case, rather than refused. The answer must be the one the
    // fully-absolute spelling of the SAME two directories gives.
    let cwd = std::env::current_dir().unwrap();
    let climbing = derived(&format!("{}/gen/core", cwd.display()), "../apps/ledger");
    let spelled_out = derived(
        &format!("{}/gen/core", cwd.display()),
        &format!("{}/apps/ledger", cwd.parent().unwrap().display()),
    );
    assert_eq!(
        climbing, spelled_out,
        "a relative `output` climbing out of the config directory denotes the same directory an \
         absolute one does, so it must derive the same manifest entry"
    );
    let path = climbing[0]
        .split_once('=')
        .expect("the flag is <package>=<path>")
        .1;
    assert!(
        std::path::Path::new(path).is_relative() && path.ends_with("gen/core/wasm/json-gen"),
        "and it must still be a relative path reaching the dependency's json-gen crate, got {path}"
    );
}

/// A dependency that publishes no schema document contributes NOTHING, silently. That silence is
/// what filters hand-written crates out of the intersection in the first place, and it is what lets
/// one config hold both kinds of crate: a rust/extern edge onto a crate with no JSON surface is an
/// ordinary thing to have, not a mistake to report.
///
/// The consumer side is the same rule: a crate with no document of its own derives no thread, because
/// there is nowhere for the rows to land.
#[test]
fn a_crate_without_a_schema_document_neither_threads_nor_is_threaded() {
    let by_name = expand_all(
        "[crates.plain]\ninput = \"p.cddl\"\noutput = \"gen/plain\"\n\
         [crates.published]\ninput = \"s.cddl\"\noutput = \"gen/published\"\n\
         json-schema-export = true\ndeps = [\"plain\"]\n\
         [crates.consumer]\ninput = \"c.cddl\"\noutput = \"gen/consumer\"\ndeps = [\"published\"]\n",
    );
    assert!(
        by_name["published"].json_schema_dep.is_empty()
            && by_name["published"].json_gen_dep.is_empty(),
        "a dependency with no json-gen crate has no `add_schemas` to call"
    );
    assert!(
        by_name["consumer"].json_schema_dep.is_empty()
            && by_name["consumer"].json_gen_dep.is_empty(),
        "a consumer with no document of its own has nowhere for a threaded row to land"
    );
}

/// `json-schema-deps` REPLACES the derivation for that crate rather than adding to it, so a crate
/// whose package composition and dependency list genuinely diverge can state the whole list — and
/// `[]` is how it says "thread nothing" while keeping its edges.
#[test]
fn json_schema_deps_replaces_the_derivation_including_the_empty_list() {
    let fixture = |threading: &str| {
        format!(
            "[defaults]\njson-schema-export = true\n\
             [crates.chain]\ninput = \"c.cddl\"\noutput = \"gen/chain\"\n\
             [crates.cip25]\ninput = \"m.cddl\"\noutput = \"gen/cip25\"\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
             deps = [\"chain\"]\nwasm-reexports = [\"cip25\"]\n{threading}"
        )
    };
    let replaced = expand_all(&fixture("json-schema-deps = [\"cip25\"]\n"));
    assert_eq!(
        replaced["ledger"].json_schema_dep,
        vec!["cip25=cip25_json_schema_gen"],
        "the override replaces the derivation rather than adding to it"
    );
    assert_eq!(
        replaced["ledger"].json_gen_dep,
        vec!["cip25-json-schema-gen=../../../cip25/wasm/json-gen"]
    );

    let nothing = expand_all(&fixture("json-schema-deps = []\n"));
    assert!(
        nothing["ledger"].json_schema_dep.is_empty() && nothing["ledger"].json_gen_dep.is_empty(),
        "an empty list threads nothing"
    );
    assert_eq!(
        nothing["ledger"].workspace_dep,
        vec!["chain"],
        "and it must not disturb the rust/extern edge `deps` carries"
    );
}

/// The raw sub-tables stay the only way to thread a crate that is not in this config, and they union
/// on top of whatever the derivation produced — with the DERIVED entries emitted first.
///
/// Order is the point of the assertion. `--json-schema-dep` is order-significant, and a TOML
/// sub-table is unordered (it deserializes into a `BTreeMap`), so raw entries emit in NAME order:
/// deterministic, but not the author's. The arrays are the ordered forms, so they go first.
#[test]
fn a_raw_sub_table_entry_unions_on_top_of_the_derived_ones() {
    let by_name = expand_all(
        "[defaults]\njson-schema-export = true\n\
         [crates.zed]\ninput = \"z.cddl\"\noutput = \"gen/zed\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"zed\"]\n\n\
         [crates.ledger.json-schema-dep]\nalien = \"alien_json_schema_gen\"\n",
    );
    assert_eq!(
        by_name["ledger"].json_schema_dep,
        vec!["zed=zed_json_schema_gen", "alien=alien_json_schema_gen"],
        "the derived thread comes first even though `alien` sorts before `zed`"
    );
}

/// A raw entry for a label the derivation would also produce WINS, silently and per half — the same
/// rule the `deps` edge follows, for the same reason: an explicit value is the user covering a case
/// the sugar does not (a vendored copy of a dependency's registrar, say). Emitting both would be the
/// flag's own duplicate-label rejection instead of an override.
#[test]
fn a_raw_entry_overrides_the_derived_half_it_names() {
    let by_name = expand_all(
        "[defaults]\njson-schema-export = true\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n\n\
         [crates.ledger.json-schema-dep]\ncore = \"vendored::core_schemas\"\n",
    );
    assert_eq!(
        by_name["ledger"].json_schema_dep,
        vec!["core=vendored::core_schemas"]
    );
    assert_eq!(
        by_name["ledger"].json_gen_dep,
        vec!["core-json-schema-gen=../../../core/wasm/json-gen"],
        "overriding the call must not drop the manifest entry the rest of the edge still needs"
    );
}

/// Every refusal the threading keys carry, each on a fixture that isolates it.
///
/// The parse-time half is the shape of the arrays themselves; the two export-false refusals need
/// each crate's finished `Cli` (the flavor is a merged value, and re-deriving clap's default for it
/// here is exactly the drift the expansion exists to prevent), so they land during expansion — still
/// before any crate generates, which is the property that matters.
#[test]
fn the_threading_keys_refuse_every_edge_that_cannot_mean_anything() {
    let with = |extra: &str| {
        format!(
            "[defaults]\njson-schema-export = true\n\
             [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n{extra}"
        )
    };

    // an unknown crate, in either key
    for key in ["wasm-reexports", "json-schema-deps"] {
        let err = error(&with(&format!("{key} = [\"nope\"]\n")));
        assert!(
            err.contains(&format!("[crates.ledger].{key}"))
                && err.contains("`nope`")
                && err.contains("`core`"),
            "{key} naming an unknown crate must name it and list the configured crates, got: {err}"
        );
    }

    // a crate naming itself
    for key in ["wasm-reexports", "json-schema-deps"] {
        let err = error(&with(&format!("{key} = [\"ledger\"]\n")));
        assert!(
            err.contains("itself"),
            "{key} naming its own crate must say so, got: {err}"
        );
    }

    // a name listed twice
    for key in ["wasm-reexports", "json-schema-deps"] {
        let err = error(&with(&format!("{key} = [\"core\", \"core\"]\n")));
        assert!(
            err.contains("twice"),
            "a repeated {key} entry must be rejected, got: {err}"
        );
    }

    // one crate reached through both edges
    let err = error(&with("deps = [\"core\"]\nwasm-reexports = [\"core\"]\n"));
    assert!(
        err.contains("both `deps` and `wasm-reexports`"),
        "one crate in both edges must be rejected naming both keys, got: {err}"
    );

    // the graph keys are per-crate, so a shared layer cannot hold them
    for layer in ["[defaults]", "[profiles.shared]"] {
        for key in ["wasm-reexports", "json-schema-deps"] {
            let err = error(&format!(
                "{layer}\n{key} = [\"core\"]\n\
                 [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n"
            ));
            assert!(
                err.contains(&format!("`{key}` is a per-crate key"))
                    && err.contains("itself included"),
                "{key} in {layer} must be rejected as a shared EDGE, got: {err}"
            );
        }
    }

    // an EXPLICIT thread onto a crate that publishes no document — the derived one is silent, this
    // one is a request that could never link
    let err = expand_error(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
         json-schema-export = true\njson-schema-deps = [\"core\"]\n",
    );
    assert!(
        err.contains("[crates.ledger].json-schema-deps")
            && err.contains("`core`")
            && err.contains("json-schema-export = false"),
        "an explicit thread onto a document-less crate must name both crates, got: {err}"
    );

    // and a crate that threads while publishing no document of its own
    let err = expand_error(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\njson-schema-export = true\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
         json-schema-deps = [\"core\"]\n",
    );
    assert!(
        err.contains("[crates.ledger].json-schema-deps") && err.contains("no document"),
        "threading into a crate with no document must say there is nowhere for the rows to land, \
         got: {err}"
    );

    // `json-schema-deps = []` on such a crate is not a request at all, so it stays legal
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\njson-schema-export = true\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\njson-schema-deps = []\n",
    );
    assert!(by_name["ledger"].json_schema_dep.is_empty());
}

/// `wasm-reexports` naming a crate that generates no wasm crate: the one case in this family where
/// the declaration is false at the coarsest level at which a config can tell, and the one that was
/// silent.
///
/// The key says a package ships another crate's wasm classes. A `wasm = false` crate has none, so
/// there is nothing the declaration could be about — and the failure without this check is *nothing
/// at all*: the threading derivation skips the named crate on a different axis entirely
/// (`json-schema-export`), so the user gets no diagnostic, no manifest entry and no thread.
///
/// `wasm` is a merged value, so each of the three layers that can set it is exercised: a crate's own
/// key, `[defaults]`, and a profile. All three must reach the same refusal, because the check reads
/// the finished `Cli` and cannot see which layer wrote it.
#[test]
fn wasm_reexports_refuses_a_crate_that_generates_no_wasm_crate() {
    let consumer = "[crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
                    wasm-reexports = [\"core\"]\n";

    for (layer, text) in [
        (
            "the crate's own key",
            format!(
                "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nwasm = false\n{consumer}"
            ),
        ),
        (
            "[defaults]",
            format!(
                "[defaults]\nwasm = false\n\
                 [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n{consumer}\
                 wasm = true\n"
            ),
        ),
        (
            "a profile",
            format!(
                "[profiles.headless]\nwasm = false\n\
                 [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
                 profiles = [\"headless\"]\n{consumer}"
            ),
        ),
    ] {
        let err = expand_error(&text);
        assert!(
            err.contains("[crates.ledger].wasm-reexports")
                && err.contains("`core`")
                && err.contains("wasm = false"),
            "the refusal must name the declaring crate, the named crate and the flavor that makes \
             it impossible ({layer}), got: {err}"
        );
    }

    // The refusal is a property of the CONFIG, so a run that selects neither crate still rejects it
    // — the same rule the runtime carrier follows.
    let err = parse(&format!(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nwasm = false\n{consumer}\
         [crates.other]\ninput = \"o.cddl\"\noutput = \"gen/other\"\n"
    ))
    .expand(&["other".to_owned()])
    .expect_err("a subset run must reject the same config a full run rejects");
    assert!(err.contains("[crates.ledger].wasm-reexports"), "got: {err}");

    // Baseline: the same fixture with the named crate generating a wasm crate is accepted, so it is
    // the flavor that is refused above and not the key.
    assert!(
        expand_all(&format!(
            "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n{consumer}"
        ))
        .contains_key("ledger"),
        "a wasm-generating crate must stay a legal `wasm-reexports` target"
    );
}

// ---------------------------------------------------------------------------------------------
// The wasm manifest — `--wasm-dep` derived from the same two edges
// ---------------------------------------------------------------------------------------------

/// The derivation, on CML's own shape: `deps` then `wasm-reexports`, with the two keys contributing
/// DIFFERENT entries because they state different facts.
///
/// A `deps` edge means the spec references the dependency's types, and the wasm pass writes two
/// kinds of reference to such a type — `use <dep>_wasm::…` at the boundary, and the dependency's
/// plain rust type as a locally minted wrapper's inner storage — so it contributes both packages.
/// `wasm-reexports` means nothing generated here names the crate at all and its classes ship in this
/// package anyway, so it contributes the wasm package alone. That asymmetry is the whole content of
/// this test: getting it backwards writes an entry nothing needs, or omits one everything does.
///
/// The `wasm-reexports` rows are also the point of the key: they are exactly the two lines CML
/// maintains by hand in `multi-era/wasm/Cargo.toml`, under the comment this key is named after.
#[test]
fn wasm_deps_derive_from_deps_then_wasm_reexports() {
    let by_name = expand_all(CML_SHAPED_SURFACE);
    assert_eq!(
        by_name["multi-era"].wasm_dep,
        vec![
            "chain=../../chain/rust",
            "chain-wasm=../../chain/wasm",
            "cip25-wasm=../../cip25/wasm",
            "cip36-wasm=../../cip36/wasm",
        ],
        "a `deps` edge contributes both of the dependency's packages and a `wasm-reexports` edge \
         only the wasm one, in deps-then-wasm-reexports order"
    );
    for lonely in ["chain", "cip25", "cip36"] {
        assert!(
            by_name[lonely].wasm_dep.is_empty(),
            "`{lonely}` has no edge, so its wasm manifest needs nothing"
        );
    }
}

/// Both derived spellings are the cargo PACKAGE name — the `lib-name` verbatim, dashes and all, and
/// that plus `-wasm`. The generated `use` lines carry the UNDERSCORED crate names
/// (`cml_chain_wasm`), so writing either side the other way round is the mistake whose error names
/// neither cause: a cargo resolution failure, or an `E0433` on a crate whose name looks right.
#[test]
fn the_derived_wasm_dep_spellings_are_the_dashed_package_names() {
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"cml-chain\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    assert_eq!(
        by_name["ledger"].wasm_dep,
        vec![
            "cml-chain=../../core/rust",
            "cml-chain-wasm=../../core/wasm",
        ]
    );
    // and the generated code the entries exist to resolve names the underscored forms
    assert_eq!(
        by_name["ledger"].extern_wasm_crate,
        vec!["cml_chain=cml_chain_wasm"]
    );
}

/// **The derived `--wasm-dep` path is RELATIVE**, under every combination of the two crates'
/// `package-json` settings — the same requirement its `--json-gen-dep` sibling carries, and for the
/// same reason: it is written into a committed `Cargo.toml`, so an absolute value would bake this
/// checkout's location into a file the project commits and the same config would produce different
/// bytes in a different clone.
///
/// Both endpoints move with their own crate's `package-json` (which nests the cargo crates one level
/// down), so all four combinations are counted rather than one.
#[test]
fn the_derived_wasm_dep_path_is_relative_in_every_layout() {
    for (dep_npm, consumer_npm, expected_rust, expected_wasm) in [
        (false, false, "../../core/rust", "../../core/wasm"),
        (true, false, "../../core/rust/rust", "../../core/rust/wasm"),
        (false, true, "../../../core/rust", "../../../core/wasm"),
        (
            true,
            true,
            "../../../core/rust/rust",
            "../../../core/rust/wasm",
        ),
    ] {
        let by_name = expand_all(&format!(
            "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\npackage-json = {dep_npm}\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
             package-json = {consumer_npm}\ndeps = [\"core\"]\n"
        ));
        let derived = &by_name["ledger"].wasm_dep;
        assert_eq!(
            derived,
            &vec![
                format!("core={expected_rust}"),
                format!("core-wasm={expected_wasm}"),
            ],
            "dep package-json={dep_npm}, consumer package-json={consumer_npm}"
        );
        for entry in derived {
            let path = entry
                .split_once('=')
                .expect("the flag is <package>=<path>")
                .1;
            assert!(
                std::path::Path::new(path).is_relative(),
                "a cargo path dependency is resolved against the manifest holding it, and an \
                 absolute value would make the committed manifest machine-specific; got {path}"
            );
        }
    }
}

/// `.` and `..` in an `output` are ordinary spellings of an ordinary directory, and the derived
/// manifest entry must not be able to tell — the same normalization
/// `a_dot_or_dot_dot_in_an_output_derives_the_same_path_the_plain_spelling_does` pins for the other
/// manifest, asserted here because this derivation calls the helper on a different pair of
/// directories and a value that skipped it would be committed just the same.
#[test]
fn a_dot_or_dot_dot_in_an_output_derives_the_same_wasm_dep_the_plain_spelling_does() {
    let derived = |core: &str, ledger: &str| {
        expand_all(&format!(
            "[crates.core]\ninput = \"c.cddl\"\noutput = \"{core}\"\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"{ledger}\"\ndeps = [\"core\"]\n"
        ))["ledger"]
            .wasm_dep
            .clone()
    };

    let plain = derived("gen/core", "gen/ledger");
    assert_eq!(
        plain,
        vec![
            "core=../../core/rust".to_owned(),
            "core-wasm=../../core/wasm".to_owned(),
        ],
        "the baseline the two spellings below must match"
    );
    assert_eq!(
        derived("./gen/core", "gen/ledger"),
        plain,
        "a leading `./` must not reach the committed manifest"
    );
    assert_eq!(
        derived("gen/sub/../core", "./gen/./ledger"),
        plain,
        "`.` and `..` anywhere in either endpoint resolve before the diff"
    );
}

/// The two `wasm = false` positions, which contribute differently because they are different facts.
///
/// A `wasm = false` CONSUMER generates no wasm crate, so there is no manifest for an entry to land
/// in and nothing is derived at all — which is also what the flag itself refuses.
///
/// A `wasm = false` DEPENDENCY keeps its rust crate name for both of the consumer's passes (the
/// single-crate convention `--extern-wasm-crate` documents, and the reason `apply_graph_edges`
/// derives no `--extern-wasm-crate` for it), so the edge is its rust package alone. It cannot be
/// reached through `wasm-reexports` at all — that is the refusal above.
#[test]
fn a_wasm_false_crate_on_either_end_derives_what_it_can_and_no_more() {
    let consumer_off = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\nwasm = false\n\
         deps = [\"core\"]\n",
    );
    assert!(
        consumer_off["ledger"].wasm_dep.is_empty(),
        "a crate with no wasm crate has no manifest for an entry to land in"
    );

    let dep_off = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nwasm = false\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    assert_eq!(
        dep_off["ledger"].wasm_dep,
        vec!["core=../../core/rust"],
        "a dependency that generates no wasm crate is named by its rust package for both passes"
    );
    assert!(
        dep_off["ledger"].extern_wasm_crate.is_empty(),
        "and the boundary mapping the second entry would serve is not derived either"
    );
}

/// A hand-written `[crates.<name>.wasm-dep]` entry for the same PACKAGE wins, silently and per
/// package — the rule every sub-table derivation in this file follows. An explicit value is the user
/// covering a case the sugar does not (a vendored checkout, a registry version beside the path), not
/// a conflict to report; emitting both would be the flag's own duplicate-package rejection.
#[test]
fn a_hand_written_wasm_dep_entry_wins_per_package() {
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n\
         [crates.ledger.wasm-dep]\ncore-wasm = \"../../vendor/core-wasm\"\n",
    );
    assert_eq!(
        by_name["ledger"].wasm_dep,
        vec![
            // derived: the package the hand entry does not name
            "core=../../core/rust",
            // hand-written: emitted under its own key, and NOT also derived
            "core-wasm=../../vendor/core-wasm",
        ],
        "the hand-written package must be taken verbatim and the other still derived"
    );
}

/// The `--rust-dep` derivation, and the three ways it differs from its wasm sibling — each a
/// property of what the RUST pass emits rather than a simplification.
///
/// 1. **`deps` alone feeds it.** `wasm-reexports` says a dependency's classes ship in this package
///    while this spec references none of its types, so no rust line names the crate.
/// 2. **One entry per edge**, the dependency's rust package: it is the only package the rust pass
///    can name.
/// 3. **No `wasm` gate on either end.** The rust crate is the one crate every run generates, and
///    `use <dep>::<Type>;` is emitted in every flavor — so the entry a `wasm = false` consumer needs
///    is exactly the entry a `wasm = true` one needs, which is the case that would silently vanish
///    if this derivation were folded into the wasm one.
#[test]
fn a_deps_edge_derives_the_dependency_rust_package_in_every_flavor() {
    // Both `wasm` positions, since a `wasm = false` crate on either end of the edge is exactly the
    // case a wasm-gated derivation would drop. (`wasm-reexports` constrains only its TARGET, which
    // must have a wasm crate for its classes to ship — so `cip25` keeps the default.)
    let config = |dep_wasm: &str, consumer_wasm: &str| {
        format!(
            "[crates.chain]\ninput = \"c.cddl\"\noutput = \"gen/chain\"\nwasm = {dep_wasm}\n\
             [crates.cip25]\ninput = \"2.cddl\"\noutput = \"gen/cip25\"\n\
             [crates.multi-era]\ninput = \"m.cddl\"\noutput = \"gen/multi-era\"\n\
             wasm = {consumer_wasm}\ndeps = [\"chain\"]\nwasm-reexports = [\"cip25\"]\n"
        )
    };
    for (dep_wasm, consumer_wasm) in [
        ("true", "true"),
        ("true", "false"),
        ("false", "true"),
        ("false", "false"),
    ] {
        let by_name = expand_all(&config(dep_wasm, consumer_wasm));
        let label = format!("dep wasm = {dep_wasm}, consumer wasm = {consumer_wasm}");
        assert_eq!(
            by_name["multi-era"].rust_dep,
            vec!["chain=../../chain/rust"],
            "{label}: a `deps` edge derives the dependency's rust package and a `wasm-reexports` \
             edge derives nothing here"
        );
        for lonely in ["chain", "cip25"] {
            assert!(
                by_name[lonely].rust_dep.is_empty(),
                "{label}: `{lonely}` has no `deps` edge, so its rust manifest needs nothing"
            );
        }
    }
}

/// **The derived `--rust-dep` path is RELATIVE**, under every combination of the two crates'
/// `package-json` settings, and its left side is the dashed cargo PACKAGE name — the two properties
/// its siblings carry, for the two reasons they carry them: the value lands in a committed
/// `Cargo.toml`, so an absolute one would make the same config produce different bytes in a
/// different clone; and a manifest key is dashed while the `use` line the entry exists to resolve
/// carries the underscored crate name.
#[test]
fn the_derived_rust_dep_is_a_relative_path_under_a_dashed_package_name() {
    for (dep_npm, consumer_npm, expected) in [
        (false, false, "../../core/rust"),
        (true, false, "../../core/rust/rust"),
        (false, true, "../../../core/rust"),
        (true, true, "../../../core/rust/rust"),
    ] {
        let by_name = expand_all(&format!(
            "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"cml-chain\"\n\
             package-json = {dep_npm}\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
             package-json = {consumer_npm}\ndeps = [\"core\"]\n"
        ));
        assert_eq!(
            by_name["ledger"].rust_dep,
            vec![format!("cml-chain={expected}")],
            "dep package-json={dep_npm}, consumer package-json={consumer_npm}"
        );
        assert!(
            std::path::Path::new(expected).is_relative(),
            "a cargo path dependency is resolved against the manifest holding it, and an absolute \
             value would make the committed manifest machine-specific"
        );
    }
    // and the generated code the entry exists to resolve names the underscored form
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"cml-chain\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n",
    );
    assert_eq!(
        by_name["ledger"].extern_wasm_crate,
        vec!["cml_chain=cml_chain_wasm"]
    );
}

/// A `.` or `..` in an `output` is an ordinary spelling of an ordinary directory, and the derived
/// entry must not be able to tell — the normalization the two sibling manifests pin, asserted here
/// because this derivation calls the helper on a third pair of directories and a value that skipped
/// it would be committed just the same.
#[test]
fn a_dot_or_dot_dot_in_an_output_derives_the_same_rust_dep_the_plain_spelling_does() {
    let derived = |core: &str, ledger: &str| {
        expand_all(&format!(
            "[crates.core]\ninput = \"c.cddl\"\noutput = \"{core}\"\n\
             [crates.ledger]\ninput = \"l.cddl\"\noutput = \"{ledger}\"\ndeps = [\"core\"]\n"
        ))["ledger"]
            .rust_dep
            .clone()
    };
    let plain = derived("gen/core", "gen/ledger");
    assert_eq!(plain, vec!["core=../../core/rust".to_owned()]);
    assert_eq!(
        derived("./gen/core", "gen/ledger"),
        plain,
        "a leading `./` must not reach the committed manifest"
    );
    assert_eq!(
        derived("gen/sub/../core", "./gen/./ledger"),
        plain,
        "`.` and `..` anywhere in either endpoint resolve before the diff"
    );
}

/// A hand-written `[crates.<name>.rust-dep]` entry for the same PACKAGE wins, silently and per
/// package — the rule every sub-table derivation in this file follows.
#[test]
fn a_hand_written_rust_dep_entry_wins_per_package() {
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.extra]\ninput = \"e.cddl\"\noutput = \"gen/extra\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
         deps = [\"core\", \"extra\"]\n\
         [crates.ledger.rust-dep]\ncore = \"../../vendor/core\"\n",
    );
    assert_eq!(
        by_name["ledger"].rust_dep,
        vec![
            // derived: the package the hand entry does not name
            "extra=../../extra/rust",
            // hand-written: emitted under its own key, and NOT also derived
            "core=../../vendor/core",
        ],
        "the hand-written package must be taken verbatim and the other still derived"
    );
}

// ---------------------------------------------------------------------------------------------
// The std-forwarding derivations (`deps` edges and `[runtime].lib-name`)
// ---------------------------------------------------------------------------------------------

/// A `deps` edge derives `--std-forward-dep` beside `--rust-dep`, UNCONDITIONALLY — including for a
/// package whose path a hand-written `rust-dep` entry supplied.
///
/// Unconditional is the point rather than an oversight: the target is a crate this config generates,
/// and every crate this tool generates declares a `std` feature, so the forward always resolves.
/// Without it, `default-features = false` on the consumer stops at the consumer — the dependency is
/// still built with its defaults and its own `no_std` arm is unreachable from any downstream
/// configuration.
#[test]
fn a_deps_edge_derives_the_std_forward_beside_the_rust_dep() {
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"core-lib\"\n\
         [crates.extra]\ninput = \"e.cddl\"\noutput = \"gen/extra\"\nlib-name = \"extra-lib\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\n\
         deps = [\"core\", \"extra\"]\n\
         [crates.ledger.rust-dep]\ncore-lib = \"../../vendor/core\"\n",
    );
    assert_eq!(
        by_name["ledger"].std_forward_dep,
        vec!["core-lib", "extra-lib"],
        "every `deps` edge forwards, whoever supplied the dependency's path"
    );
    for lonely in ["core", "extra"] {
        assert!(
            by_name[lonely].std_forward_dep.is_empty(),
            "`{lonely}` has no `deps` edge, so there is nothing to forward to"
        );
    }
}

/// A hand-written `std-forward-dep` array entry suppresses the derived one, so a package is named
/// once — the rule every derivation in this file follows, on the array shape.
#[test]
fn a_hand_written_std_forward_dep_entry_is_not_derived_twice() {
    let by_name = expand_all(
        "[crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\nlib-name = \"core-lib\"\n\
         [crates.ledger]\ninput = \"l.cddl\"\noutput = \"gen/ledger\"\ndeps = [\"core\"]\n\
         std-forward-dep = [\"core-lib\"]\n",
    );
    assert_eq!(by_name["ledger"].std_forward_dep, vec!["core-lib"]);
}

/// `[runtime].lib-name` derives EVERY crate's dependency on the shared runtime crate — both halves,
/// the path entry and the forward — by a relative path from that crate's own rust directory, under
/// each crate's own `package-json` layout.
///
/// This is the edge `common-import` cannot derive: an override is a Rust path prefix, and no cargo
/// package name follows from one. Naming the package is the one-line opt-in.
#[test]
fn a_runtime_lib_name_derives_every_crate_s_dependency_on_the_shared_runtime() {
    let by_name = expand_all(
        "[runtime]\nexport-static-crate = \"runtime\"\ncommon-import = \"cddl_runtime\"\n\
         lib-name = \"cddl-runtime\"\n\
         [crates.core]\ninput = \"c.cddl\"\noutput = \"gen/core\"\n\
         [crates.npm]\ninput = \"n.cddl\"\noutput = \"gen/npm\"\npackage-json = true\n",
    );
    assert_eq!(
        by_name["core"].rust_dep,
        vec!["cddl-runtime=../../../runtime"]
    );
    assert_eq!(
        by_name["npm"].rust_dep,
        vec!["cddl-runtime=../../../../runtime"],
        "the path is counted from the crate's OWN rust directory, which `package-json` moves down"
    );
    for name in ["core", "npm"] {
        assert_eq!(
            by_name[name].std_forward_dep,
            vec!["cddl-runtime"],
            "{name}: the runtime edge forwards too, or its `no_std` arm is unreachable"
        );
    }
}

/// Without `lib-name` the runtime edge stays the hand edit it has always been — the key is an opt-in,
/// and a `[runtime]` that predates it generates exactly what it did before.
#[test]
fn a_runtime_without_lib_name_derives_no_dependency_on_the_shared_runtime() {
    let cli = expand_one(
        "[runtime]\nexport-static-crate = \"runtime\"\ncommon-import = \"cddl_runtime\"\n\
         [crates.demo]\ninput = \"s.cddl\"\noutput = \"gen\"\n",
    );
    assert!(cli.rust_dep.is_empty() && cli.std_forward_dep.is_empty());
}

/// `lib-name` with no `export-static-crate` is a key that cannot mean anything: the dependency it
/// derives has no directory to point at. Same shape as the empty-table and `flavor-from` refusals.
#[test]
fn a_runtime_lib_name_without_an_export_is_an_error() {
    let err = error(&format!(
        "[runtime]\ncommon-import = \"r\"\nlib-name = \"cddl-runtime\"\n{MINIMAL_CRATE}"
    ));
    assert!(
        err.contains("`cddl-runtime`") && err.contains("nothing to point at"),
        "the refusal must name the package and say why it cannot mean anything, got:\n{err}"
    );
}

/// **The compile proof**: a config-generated workspace with `wasm = true` and a `deps` edge, built.
///
/// This is the deliverable, and the reason is that no manifest-text assertion can see it. A
/// `[dependencies]` entry reads correct whether or not its path resolves and whether or not the
/// package it names is the one the generated `use` lines need — the derivation tests above assert
/// exactly those strings, and would all pass over a workspace that does not compile. Before
/// `--wasm-dep` this one did not: the wasm pass emitted `use <dep>_wasm::…` and
/// `BTreeMap<own::T, <dep>::T>` into a crate whose manifest never named either package, and nothing
/// in the suite built such a workspace (the `[runtime]` compile test runs `wasm = false`, the
/// acceptance test compares bytes without building, and the `deps` e2e asserts the generated SOURCE
/// references the dependency without compiling it).
///
/// The consumer's spec is shaped to need BOTH packages at once: `f`/`l` reach the dependency's types
/// across the wasm boundary (`depcrate_wasm`), while `mixed` is a wrapper over one of its own types
/// and one of the dependency's, which no dependency can host, so it is minted here with
/// `depcrate::Foo` as inner storage. The two mutation legs at the end delete one derived entry each
/// and require the build to fail, which is what stops this test passing for a reason other than the
/// one it is about.
///
/// Two hand edits stand between "generated" and "builds", and each is a documented tool boundary
/// rather than an oversight, so each is written here the way a consumer writes it — and asserted to
/// be absent first, so that a tool that started writing one fails here rather than silently making
/// the edit redundant:
///
/// 1. the workspace `Cargo.toml` (the tool generates crates, not workspaces);
/// 2. the shared runtime's hand-owned `src/lib.rs`, and each crate's dependency on it
///    (`--common-import-override` documents that entry as the user's line — an override is a Rust
///    path prefix, so no package name can be derived from it).
///
/// The **third** entry a `deps` edge needs — the dependency's rust package in the CONSUMER's
/// `rust/Cargo.toml` — used to be a hand edit here and is now `--rust-dep`'s, so it is asserted
/// PRESENT rather than written: this test was built so that the day something derived it, it would
/// fail here rather than let the edit go quietly redundant.
///
/// A `[runtime]` table is not decoration here: without one shared runtime each crate defines its own
/// `Serialize`/`Deserialize`, and a consumer cannot serialize a dependency's type no matter what its
/// manifest says. That is a flavor fact, independent of every entry under test.
///
/// It is also the **headline proof of the convergence pass**, because the workspace it compiles is
/// what ONE invocation leaves on a cold tree. Before that pass this test needed two `generate` calls
/// and threw the first one's result away; a cold tree genuinely did not build until the dependency
/// had heard what its consumer borrows. So the single call, the byte-identity of a second call, and
/// the `cargo check` are one claim in three parts rather than three tests.
#[test]
fn a_config_generated_workspace_builds_with_wasm_on() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_wasm_dep_e2e_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(dir.join("specs/dep.cddl"), "foo = [a: uint, b: text]\n").unwrap();
    std::fs::write(
        dir.join("specs/user.cddl"),
        "own = [z: uint]\nbar = [f: foo, l: [* foo], mixed: {* own => foo}]\n",
    )
    .unwrap();

    let config_path = dir.join("codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}/static\"\n\n\
             [runtime]\nexport-static-crate = \"runtime\"\ncommon-import = \"cddl_runtime\"\n\n\
             [crates.depcrate]\ninput = \"specs/dep.cddl\"\noutput = \"gen/depcrate\"\n\n\
             [crates.usercrate]\ninput = \"specs/user.cddl\"\noutput = \"gen/usercrate\"\n\
             deps = [\"depcrate\"]\n",
            env!("CARGO_MANIFEST_DIR")
        ),
    )
    .unwrap();

    // ONE invocation, on a cold tree. The convergence pass inside it is what makes the workspace
    // this test compiles the output of a single command rather than of a second one — so the two
    // halves of the headline claim are asserted here together: these bytes are what one invocation
    // leaves, a second invocation leaves the same bytes, and they compile.
    //
    // The idempotence half is taken BEFORE the hand edits below, so what it compares is the tool's
    // own output rather than the merge of a hand-edited manifest (which the manifest convergence
    // tests pin separately, and which would confound the claim being made here).
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run must converge and exit 0");
    let after_first = tree_bytes(&dir, &["gen", "runtime"]);
    config::generate(&config_path, &[], None, None).expect("the second run must generate");
    if let Some(difference) = first_tree_difference(
        "first run",
        &after_first,
        "second run",
        &tree_bytes(&dir, &["gen", "runtime"]),
    ) {
        panic!(
            "one invocation must settle the workspace, so a second changes nothing, but \
             {difference}"
        );
    }

    // The entries under test, before anything is built: both of the dependency's packages, by
    // relative path, in the consumer's WASM manifest — and the third entry, the dependency's rust
    // package, in the consumer's RUST manifest. The last is asserted rather than hand-written
    // because `--rust-dep` derives it; this is the inversion the hand edit that used to sit below
    // was built to force.
    //
    // The rust entry additionally carries `default-features = false` and a `depcrate/std` forward:
    // a `deps` edge derives `--std-forward-dep` beside `--rust-dep`, so `default-features = false`
    // on the CONSUMER reaches the dependency's no_std arm instead of stopping at the consumer. The
    // wasm entry has neither — the wasm crate is `std` by nature and declares no `std` feature.
    for (manifest, expected) in [
        (
            "gen/usercrate/wasm/Cargo.toml",
            "depcrate = { path = \"../../depcrate/rust\" }",
        ),
        (
            "gen/usercrate/wasm/Cargo.toml",
            "depcrate-wasm = { path = \"../../depcrate/wasm\" }",
        ),
        (
            "gen/usercrate/rust/Cargo.toml",
            "depcrate = { path = \"../../depcrate/rust\", default-features = false }",
        ),
        ("gen/usercrate/rust/Cargo.toml", "std = [\"depcrate/std\"]"),
    ] {
        let text = std::fs::read_to_string(dir.join(manifest)).expect(manifest);
        assert!(
            text.contains(expected),
            "the derivation must write {expected} into {manifest}:\n{text}"
        );
    }

    // Hand edit 2a: the shared runtime's crate root, which the tool never writes (its new-static-file
    // notice says exactly this).
    let runtime_src = dir.join("runtime/src");
    let mut modules: Vec<String> = std::fs::read_dir(&runtime_src)
        .unwrap()
        .filter_map(|e| {
            let name = e.unwrap().file_name().to_string_lossy().into_owned();
            name.strip_suffix(".rs").map(str::to_owned)
        })
        .collect();
    modules.sort();
    std::fs::write(
        runtime_src.join("lib.rs"),
        modules
            .iter()
            .map(|m| format!("pub mod {m};\n"))
            .collect::<String>(),
    )
    .unwrap();

    // Hand edit 2b, asserted absent first so a tool that starts writing it fails here.
    let hand_edit = |crate_dir: &str, up: &str| {
        let path = dir.join(crate_dir).join("Cargo.toml");
        let text = std::fs::read_to_string(&path).unwrap();
        assert!(
            !text.contains("cddl_runtime"),
            "{crate_dir}: the tool does not write the consumer's dependency on the shared runtime \
             — the --common-import-override docs make that the user's line"
        );
        let added = format!(
            "[dependencies]\ncddl_runtime = {{ package = \"cddl-runtime\", path = \"{up}/runtime\" }}\n"
        );
        std::fs::write(&path, text.replacen("[dependencies]\n", &added, 1)).unwrap();
    };
    for crate_dir in [
        "gen/depcrate/rust",
        "gen/depcrate/wasm",
        "gen/usercrate/rust",
        "gen/usercrate/wasm",
    ] {
        hand_edit(crate_dir, "../../..");
    }

    // Hand edit 1: the workspace.
    std::fs::write(
        dir.join("Cargo.toml"),
        "[workspace]\nmembers = [\"runtime\", \"gen/depcrate/rust\", \"gen/depcrate/wasm\", \
         \"gen/usercrate/rust\", \"gen/usercrate/wasm\"]\nresolver = \"2\"\n",
    )
    .unwrap();

    let target_dir = dir.join("target");
    let check = || {
        crate::tests::integration_tests::tool_cmd("cargo")
            .arg("check")
            .arg("--workspace")
            .current_dir(&dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap()
    };
    let built = check();
    assert!(
        built.status.success(),
        "a config-generated workspace with wasm on must build:\n{}\n{}",
        String::from_utf8_lossy(&built.stdout),
        String::from_utf8_lossy(&built.stderr)
    );

    // The mutation legs: each derived entry is load-bearing, and they are load-bearing for different
    // references — in the WASM manifest, the wasm package for the boundary `use` lines and the rust
    // package for the mixed-dep wrapper's inner storage; in the RUST manifest, the rust package for
    // the `use depcrate::Foo;` the rust pass emits for every imported type. Deleting any of them
    // must break the build on the name it carries.
    for (manifest, entry, missing_name) in [
        (
            "gen/usercrate/wasm/Cargo.toml",
            "depcrate-wasm = { path = \"../../depcrate/wasm\" }\n",
            "depcrate_wasm",
        ),
        (
            "gen/usercrate/wasm/Cargo.toml",
            "depcrate = { path = \"../../depcrate/rust\" }\n",
            "depcrate",
        ),
        (
            "gen/usercrate/rust/Cargo.toml",
            "depcrate = { path = \"../../depcrate/rust\", default-features = false }\n",
            "depcrate",
        ),
    ] {
        let manifest_path = dir.join(manifest);
        let intact = std::fs::read_to_string(&manifest_path).unwrap();
        assert!(
            intact.contains(entry),
            "the mutation leg must actually remove something: `{entry}` is not in {manifest}:\n{intact}"
        );
        std::fs::write(&manifest_path, intact.replace(entry, "")).unwrap();
        let mutated = check();
        assert!(
            !mutated.status.success(),
            "removing `{entry}` from {manifest} must break the build — if it does not, the \
             derivation is writing an entry nothing needs"
        );
        let stderr = String::from_utf8_lossy(&mutated.stderr);
        assert!(
            stderr.contains(missing_name),
            "the failure must name `{missing_name}`, the crate the removed entry provided, got:\n{stderr}"
        );
        std::fs::write(&manifest_path, &intact).unwrap();
    }

    let _ = std::fs::remove_dir_all(&dir);
}

/// The two things no generation-time assertion can reach, on real disk: a derived thread **links**,
/// and when it collides the guard blames the **consumer**.
///
/// Leg 1 — linking. The unit tests above pin the flag values; what they cannot see is whether the
/// derived `[dependencies]` path actually resolves and whether the derived registrar call actually
/// composes. Both halves have to be right at once — a correct call with a wrong path is an `E0433`,
/// a correct path with a wrong lib name is the same — and the observable is the exported document
/// holding a row the consumer's spec never mentions, which can only have arrived through the thread.
/// `wasm-reexports` carries the edge here rather than `deps`, so the fixture holds nothing but the
/// threading edge: no extern import, no workspace dependency, no shared type. The consumer's spec
/// references nothing of the dependency's, so the ref closure cannot supply the row either. The
/// dependency is the one crate here with `wasm = true`, because the key says a package ships its
/// classes and naming a crate that generates none is refused
/// (`wasm_reexports_refuses_a_crate_that_generates_no_wasm_crate`); the CONSUMER stays `wasm =
/// false`, which the key does not constrain and which keeps this fixture about the document alone.
///
/// Leg 2 — the collision. Registration order decides which of two crates publishing one schema name
/// keeps it, and the tool emits dep calls FIRST precisely so the loser is the CONSUMER's row: a
/// dependency's names are already shipped in the dependency's own package, so the consumer's is the
/// one whose owner can rename it. Mutating the consumer's spec to publish a name the dependency
/// already registered must therefore fail the consumer's own `cargo run`, naming the consumer's
/// type — not the dependency's. Until this leg existed that was reasoning from the single-crate
/// mechanism (`integration_tests::json_schema_name_stolen_fails`) rather than a measurement.
#[test]
fn a_derived_thread_links_and_a_collision_blames_the_consumer() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_thread_e2e_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(dir.join("specs/dep.cddl"), "dep_thing = [x: uint]\n").unwrap();
    std::fs::write(dir.join("specs/user.cddl"), "own_thing = [y: text]\n").unwrap();

    let config_path = dir.join("codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}/static\"\nwasm = false\n\
             json-serde-derives = true\njson-schema-export = true\n\n\
             [crates.depcrate]\ninput = \"specs/dep.cddl\"\noutput = \"gen/dep\"\nwasm = true\n\n\
             [crates.usercrate]\ninput = \"specs/user.cddl\"\noutput = \"gen/user\"\n\
             wasm-reexports = [\"depcrate\"]\n",
            env!("CARGO_MANIFEST_DIR")
        ),
    )
    .unwrap();

    config::generate(&config_path, &[], None, None)
        .unwrap_or_else(|e| panic!("a cold config run must generate: {e}"));

    let json_gen_dir = dir.join("gen/user/wasm/json-gen");
    let manifest = std::fs::read_to_string(json_gen_dir.join("Cargo.toml")).expect("manifest");
    assert!(
        manifest.contains("depcrate-json-schema-gen = { path = \"../../../dep/wasm/json-gen\" }"),
        "the derived entry must be the relative path a cargo path dependency means:\n{manifest}"
    );

    let run = crate::tests::integration_tests::tool_cmd("cargo")
        .arg("run")
        .current_dir(&json_gen_dir)
        .output()
        .unwrap();
    assert!(
        run.status.success(),
        "the consumer's json-gen crate must build and run — the derived call and the derived \
         manifest entry have to agree:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );

    let document: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(json_gen_dir.join("schemas/usercrate.schema.json"))
            .expect("the json-gen run must write its document"),
    )
    .expect("the exported document must be valid JSON");
    let defs = document
        .get("$defs")
        .and_then(|d| d.as_object())
        .expect("the exported document must hold a `$defs` object");
    assert!(
        defs.contains_key("DepThing"),
        "the threaded dependency's row can only reach `$defs` through the derived thread; got {:?}",
        defs.keys().collect::<Vec<_>>()
    );
    assert!(
        defs.contains_key("OwnThing"),
        "the consumer's own row must still reach `$defs`; got {:?}",
        defs.keys().collect::<Vec<_>>()
    );

    // Leg 2: the consumer now publishes a name the dependency already registered.
    std::fs::write(dir.join("specs/user.cddl"), "dep_thing = [z: text]\n").unwrap();
    config::generate(&config_path, &["usercrate".to_owned()], None, None)
        .expect("the collision is a RUN-time verdict — generation must still succeed");
    let collided = crate::tests::integration_tests::tool_cmd("cargo")
        .arg("run")
        .current_dir(&json_gen_dir)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&collided.stderr).into_owned();
    assert!(
        !collided.status.success(),
        "two crates publishing one schema name must not produce a document silently:\n{stderr}"
    );
    let blame = stderr
        .lines()
        .find(|line| line.contains("publishes the JSON schema name"))
        .unwrap_or_else(|| {
            panic!("the injectivity guard must be what fails the run, got:\n{stderr}")
        });
    assert!(
        blame.contains("\"DepThing2\""),
        "the guard sees the collision through the name schemars handed the loser, got: {blame}"
    );
    assert!(
        blame.contains("usercrate::") && !blame.contains("depcrate::"),
        "dep calls are emitted first so the CONSUMER's row is the blamed one — the side whose owner \
         can rename it; got: {blame}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// Acceptance — the whole feature, over a whole project
// ---------------------------------------------------------------------------------------------

/// The specs the acceptance fixture generates from, written identically into both roots.
///
/// `ledger` both names a `core` type directly and builds a collection over it: the first is what
/// `--extern-import` resolves, the second is what makes `--workspace-dep` emit the sidecar the
/// reverse edges read.
fn write_acceptance_specs(root: &Path) {
    std::fs::create_dir_all(root.join("specs")).unwrap();
    std::fs::write(root.join("specs/basic.cddl"), "basic_thing = [n: uint]\n").unwrap();
    std::fs::write(
        root.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    std::fs::write(root.join("specs/extras.cddl"), "extra_thing = [z: text]\n").unwrap();
    std::fs::write(
        root.join("specs/ledger.cddl"),
        "ledger_rec = [t: core_thing, l: [* core_thing]]\n",
    )
    .unwrap();
}

/// Every layer of the feature in one file: `[defaults]`, a `[profiles.*]` three of the four crates
/// apply, `[runtime]` (including `lib-name`, so every crate derives its dependency on the shared
/// runtime and the std forward onto it), a `deps` edge, a `wasm-reexports` edge, a
/// `json-schema-root`, and one crate (`basic`) that applies no profile and so deviates from the
/// others' runtime flavor on both max axes.
///
/// `basic` sorts FIRST by name and is deliberately not the runtime carrier: the carrier is derived
/// as the first crate whose flavor is the join, which is `core`, so a derivation that had simply
/// taken the alphabetically-first crate would fail the comparison below.
fn acceptance_config_text() -> String {
    format!(
        "[defaults]\n\
         static-dir = \"{static_dir}\"\n\
         preserve-encodings = true\n\
         canonical-form = true\n\
         \n\
         [profiles.published]\n\
         json-serde-derives = true\n\
         json-schema-export = true\n\
         \n\
         [runtime]\n\
         export-static-crate = \"runtime\"\n\
         common-import = \"shared_runtime\"\n\
         lib-name = \"cddl-runtime\"\n\
         \n\
         [crates.basic]\n\
         input = \"specs/basic.cddl\"\n\
         output = \"gen/basic\"\n\
         lib-name = \"basic-lib\"\n\
         \n\
         [crates.core]\n\
         input = \"specs/core.cddl\"\n\
         output = \"gen/core\"\n\
         lib-name = \"core-lib\"\n\
         profiles = [\"published\"]\n\
         \n\
         [crates.extras]\n\
         input = \"specs/extras.cddl\"\n\
         output = \"gen/extras\"\n\
         lib-name = \"extras-lib\"\n\
         profiles = [\"published\"]\n\
         \n\
         [crates.ledger]\n\
         input = \"specs/ledger.cddl\"\n\
         output = \"gen/ledger\"\n\
         lib-name = \"ledger-lib\"\n\
         profiles = [\"published\"]\n\
         deps = [\"core\"]\n\
         wasm-reexports = [\"extras\"]\n\
         json-schema-root = [\"ledger_lib::HandWritten\"]\n",
        static_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
    )
}

/// The same four invocations spelled as a shell script would spell them, in generation order.
///
/// # How this list is known to be complete
///
/// It is not known by inspection, and it must not be — a hand list that quietly forgot a derived
/// flag would still produce identical trees whenever that flag's effect is unreachable in this
/// fixture, and the acceptance test would pass while proving nothing about it. So the completeness
/// check is mechanical: [`a_whole_config_generates_what_the_hand_written_flags_generate`] compares
/// the ENTIRE expanded `Cli` — every field, derived or not — against `Cli::parse_from` of these
/// argv vectors. A derived flag missing here is a differing struct field, named in the failure,
/// whether or not it changes a single emitted byte.
///
/// What that leaves for a reader is the reverse direction: a flag written here that the config never
/// derives also fails, so this list cannot drift ahead of the config either.
///
/// The derivations it spells, for orientation (all read off `src/config.rs`, not off the docs):
/// `apply_graph_edges` forward (`--extern-import`, `--extern-wasm-crate`, `--extern-wrapper-index`,
/// `--workspace-dep`) and reverse (`--wrapper-requests`, `--key-requests`); `apply_runtime`
/// (`--common-import-override` on every crate, `--export-static-crate` on the derived carrier);
/// `threading` (`--json-schema-dep` + `--json-gen-dep` per edge); and the manifest derivations
/// (`--wasm-dep` per package per edge; `--rust-dep` + `--std-forward-dep` once per `deps` edge, and
/// once more per crate for the `[runtime].lib-name` runtime crate).
fn acceptance_hand_invocations(root: &Path) -> Vec<(&'static str, Vec<String>)> {
    let p = |rel: &str| root.join(rel).to_string_lossy().into_owned();
    let static_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/static").to_owned();
    let shared: Vec<String> = vec![
        "--static-dir".to_owned(),
        static_dir,
        "--preserve-encodings".to_owned(),
        "true".to_owned(),
        "--canonical-form".to_owned(),
        "true".to_owned(),
    ];
    let published: Vec<String> = vec![
        "--json-serde-derives".to_owned(),
        "true".to_owned(),
        "--json-schema-export".to_owned(),
        "true".to_owned(),
    ];
    let runtime: Vec<String> = vec![
        "--common-import-override".to_owned(),
        "shared_runtime".to_owned(),
    ];
    // `[runtime].lib-name`: every crate depends on the shared runtime crate by relative path from
    // its own rust directory, and forwards its `std` onto it. Appended LAST because the per-flag
    // order is what the `Cli` comparison sees, and the config emits the `deps`-derived entries
    // before this one.
    let runtime_dep: Vec<String> = vec![
        "--rust-dep".to_owned(),
        "cddl-runtime=../../../runtime".to_owned(),
        "--std-forward-dep".to_owned(),
        "cddl-runtime".to_owned(),
    ];
    let invocation = |per_crate: Vec<String>, profiled: bool, extra: Vec<String>| {
        let mut argv = vec!["cddl-codegen".to_owned()];
        argv.extend(per_crate);
        argv.extend(shared.iter().cloned());
        if profiled {
            argv.extend(published.iter().cloned());
        }
        argv.extend(runtime.iter().cloned());
        argv.extend(extra);
        argv.extend(runtime_dep.iter().cloned());
        argv
    };

    vec![
        (
            "basic",
            invocation(
                vec![
                    "--input".to_owned(),
                    p("specs/basic.cddl"),
                    "--output".to_owned(),
                    p("gen/basic"),
                    "--lib-name".to_owned(),
                    "basic-lib".to_owned(),
                ],
                false,
                vec![],
            ),
        ),
        (
            "core",
            invocation(
                vec![
                    "--input".to_owned(),
                    p("specs/core.cddl"),
                    "--output".to_owned(),
                    p("gen/core"),
                    "--lib-name".to_owned(),
                    "core-lib".to_owned(),
                ],
                true,
                vec![
                    // `[runtime]`: the carrier's invocation is the one that carries the export.
                    "--export-static-crate".to_owned(),
                    p("runtime"),
                    // The reverse edges: `ledger` is the only consumer of `core`.
                    "--wrapper-requests".to_owned(),
                    format!(
                        "ledger_lib={}",
                        p("gen/ledger/wasm/src/generated/borrowed_collections.rs")
                    ),
                    "--key-requests".to_owned(),
                    format!(
                        "ledger_lib={}",
                        p("gen/ledger/rust/src/generated/borrowed_key_types.rs")
                    ),
                ],
            ),
        ),
        (
            "extras",
            invocation(
                vec![
                    "--input".to_owned(),
                    p("specs/extras.cddl"),
                    "--output".to_owned(),
                    p("gen/extras"),
                    "--lib-name".to_owned(),
                    "extras-lib".to_owned(),
                ],
                true,
                vec![],
            ),
        ),
        (
            "ledger",
            invocation(
                vec![
                    "--input".to_owned(),
                    p("specs/ledger.cddl"),
                    "--output".to_owned(),
                    p("gen/ledger"),
                    "--lib-name".to_owned(),
                    "ledger-lib".to_owned(),
                ],
                true,
                vec![
                    "--json-schema-root".to_owned(),
                    "ledger_lib::HandWritten".to_owned(),
                    // The four forward edges of `deps = ["core"]`.
                    "--workspace-dep".to_owned(),
                    "core_lib".to_owned(),
                    "--extern-import".to_owned(),
                    format!("core_lib={}", p("gen/core/extern-interface/core_lib")),
                    "--extern-wrapper-index".to_owned(),
                    format!(
                        "core_lib={}",
                        p("gen/core/wasm/src/generated/collections.rs")
                    ),
                    "--extern-wasm-crate".to_owned(),
                    "core_lib=core_lib_wasm".to_owned(),
                    // The threading derivation: `deps` first, then `wasm-reexports`, each edge
                    // spelling both the registrar call and the manifest entry that links it.
                    "--json-schema-dep".to_owned(),
                    "core_lib=core_lib_json_schema_gen".to_owned(),
                    "--json-schema-dep".to_owned(),
                    "extras_lib=extras_lib_json_schema_gen".to_owned(),
                    "--json-gen-dep".to_owned(),
                    "core-lib-json-schema-gen=../../../core/wasm/json-gen".to_owned(),
                    "--json-gen-dep".to_owned(),
                    "extras-lib-json-schema-gen=../../../extras/wasm/json-gen".to_owned(),
                    // The wasm manifest entries the same two edges derive, in the same order. A
                    // `deps` edge contributes BOTH of the dependency's packages (its wasm crate for
                    // the boundary, its rust crate for a locally minted wrapper's inner storage);
                    // `wasm-reexports` contributes the wasm one alone, since nothing generated here
                    // names the crate at all.
                    "--wasm-dep".to_owned(),
                    "core-lib=../../core/rust".to_owned(),
                    "--wasm-dep".to_owned(),
                    "core-lib-wasm=../../core/wasm".to_owned(),
                    "--wasm-dep".to_owned(),
                    "extras-lib-wasm=../../extras/wasm".to_owned(),
                    // The RUST manifest entry the same `deps` edge derives — one per edge, and
                    // `wasm-reexports` contributes none, since no rust line names that crate.
                    "--rust-dep".to_owned(),
                    "core-lib=../../core/rust".to_owned(),
                    // …and its std-forwarding half, so `default-features = false` on this crate
                    // reaches `core-lib`'s no_std arm rather than stopping here.
                    "--std-forward-dep".to_owned(),
                    "core-lib".to_owned(),
                ],
            ),
        ),
    ]
}

/// Every file under `subtrees` of `root`, keyed by its path relative to `root`.
fn tree_bytes(root: &Path, subtrees: &[&str]) -> std::collections::BTreeMap<String, Vec<u8>> {
    fn walk(dir: &Path, prefix: &str, out: &mut std::collections::BTreeMap<String, Vec<u8>>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries {
            let entry = entry.unwrap();
            let name = entry.file_name().to_string_lossy().into_owned();
            let rel = format!("{prefix}/{name}");
            if entry.file_type().unwrap().is_dir() {
                walk(&entry.path(), &rel, out);
            } else {
                out.insert(rel, std::fs::read(entry.path()).unwrap());
            }
        }
    }
    let mut out = std::collections::BTreeMap::new();
    for subtree in subtrees {
        walk(&root.join(subtree), subtree, &mut out);
    }
    out
}

/// The first way two generated trees differ, rendered so the answer to "which derivation drifted"
/// is in the failure itself: a file-set difference names the file, a content difference names the
/// file AND the first differing line on both sides.
fn first_tree_difference(
    left_label: &str,
    left: &std::collections::BTreeMap<String, Vec<u8>>,
    right_label: &str,
    right: &std::collections::BTreeMap<String, Vec<u8>>,
) -> Option<String> {
    if let Some(only_left) = left.keys().find(|path| !right.contains_key(*path)) {
        return Some(format!(
            "`{only_left}` was written by the {left_label} run and not by the {right_label} one"
        ));
    }
    if let Some(only_right) = right.keys().find(|path| !left.contains_key(*path)) {
        return Some(format!(
            "`{only_right}` was written by the {right_label} run and not by the {left_label} one"
        ));
    }
    let (path, left_bytes) = left.iter().find(|(path, bytes)| &right[*path] != *bytes)?;
    let right_bytes = &right[path];
    let left_text = String::from_utf8_lossy(left_bytes);
    let right_text = String::from_utf8_lossy(right_bytes);
    let mut report = format!("`{path}` differs:\n");
    let mut lines = left_text.lines().zip(right_text.lines()).enumerate();
    match lines.find(|(_, (l, r))| l != r) {
        Some((n, (l, r))) => report.push_str(&format!(
            "  line {}:\n  - {left_label}: {l}\n  + {right_label}: {r}\n",
            n + 1
        )),
        None => report.push_str(&format!(
            "  the shared prefix agrees; the files differ in length ({} vs {} lines)\n",
            left_text.lines().count(),
            right_text.lines().count()
        )),
    }
    Some(report)
}

/// **The load-bearing test of the config feature**: a whole multi-crate project generated once from
/// a config file and once from the flag invocations it claims to be shorthand for, byte for byte the
/// same.
///
/// Every phase of this feature has asserted "a config key IS its flag, and nothing more" — it is why
/// `Cli` values are produced through clap rather than constructed, why a drift gate keeps the key set
/// in bijection with `Cli`'s fields, and why no phase had to reason about codegen semantics. Until
/// here that claim was tested one derivation at a time, on argv values. This runs the whole thing:
/// four crates, two shared layers, a `deps` edge, a `wasm-reexports` edge, a shared runtime whose
/// carrier is DERIVED, and a crate at a different flavor than the rest — all at once, onto disk.
///
/// Two assertions, and the order matters.
///
/// 1. **The expanded `Cli`s are the hand-written ones.** This is what makes the hand-written side
///    provably complete: it compares every field, so a derived flag the hand list forgot fails here
///    even when its effect is unreachable in this fixture. Without it, an acceptance test that
///    compared only bytes would silently stop covering any derivation whose output effect this
///    particular spec does not reach.
/// 2. **The two trees are byte-identical, file set included.** This is what (1) cannot see: that
///    nothing in the generated bytes depends on WHERE the run happened. The two runs use different
///    absolute roots for every path the tool reads or writes, so any emitted absolute path, any
///    directory name leaking into content, would show up here — which is also why the `--json-gen-dep`
///    value is derived relative rather than absolute.
///
/// Both roots are COLD, so neither run reads prior output: the comment-preservation overlay, the
/// manifest merge, and the `--export-static-crate` new-file notice all see an empty target.
#[test]
fn a_whole_config_generates_what_the_hand_written_flags_generate() {
    use clap::Parser;

    let root = std::env::temp_dir().join(format!(
        "cddl_config_acceptance_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let config_root = root.join("from-config");
    let flags_root = root.join("from-flags");
    write_acceptance_specs(&config_root);
    write_acceptance_specs(&flags_root);
    let config_path = config_root.join("cddl-codegen.toml");
    std::fs::write(&config_path, acceptance_config_text()).unwrap();

    let expanded = config::load(&config_path)
        .expect("the acceptance config must load")
        .expand(&[])
        .expect("the acceptance config must expand");
    let hand = acceptance_hand_invocations(&flags_root);

    // The order is part of what the config decides: dependencies first, ties by crate name.
    assert_eq!(
        expanded.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
        hand.iter().map(|(n, _)| *n).collect::<Vec<_>>(),
        "the config's generation order must be the order the hand script runs in"
    );

    // (1) Completeness: every field of every crate's `Cli`, with each run's own root erased so the
    // only thing that can differ is a flag. Reported line by line rather than through `assert_eq!`,
    // because a `Cli`'s pretty-printed debug is forty fields and the escaped one-line rendering of
    // two of them buries the single field that moved.
    for ((name, config_cli), (_, argv)) in expanded.iter().zip(hand.iter()) {
        let hand_cli = Cli::parse_from(argv);
        let erase = |cli: &Cli, at: &Path| {
            format!("{cli:#?}").replace(&at.to_string_lossy().into_owned(), "<ROOT>")
        };
        let from_config = erase(config_cli, &config_root);
        let from_flags = erase(&hand_cli, &flags_root);
        if from_config != from_flags {
            let context = from_config
                .lines()
                .zip(from_flags.lines())
                .find(|(c, f)| c != f)
                .map(|(c, f)| format!("  - config: {}\n  + flags:  {}", c.trim(), f.trim()))
                .unwrap_or_else(|| {
                    format!(
                        "  one invocation carries {} more lines than the other",
                        from_config
                            .lines()
                            .count()
                            .abs_diff(from_flags.lines().count())
                    )
                });
            panic!(
                "`{name}`: the config expanded to a different invocation than the hand-written flag \
                 list.\n{context}\nEither a derivation changed, or the hand-written side in \
                 `acceptance_hand_invocations` is missing the flag the config now derives — add it \
                 there, or this acceptance test stops covering it."
            );
        }
    }

    // ONE config run over a cold tree, and it exits 0: the convergence pass inside it re-runs the
    // dependencies whose sidecars the run rewrote.
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run over the acceptance fixture must converge and exit 0");

    // The hand-written side has to model that pass, or the two trees differ for a reason that is not
    // a defect. `core` is the crate the config re-runs — the only one carrying reverse edges
    // (`--wrapper-requests` / `--key-requests`), so the only one whose input this run changed — and
    // the re-run is its OWN invocation, unchanged, which is exactly what the config does: the
    // convergence pass re-runs a crate, it does not generate it differently.
    const RERUN: &str = "core";
    let rerun = hand
        .iter()
        .find(|(name, _)| *name == RERUN)
        .map(|(_, argv)| argv.clone())
        .unwrap_or_else(|| panic!("the hand list must contain `{RERUN}` to model the re-run"));
    for (name, argv) in hand.iter().chain(std::iter::once(&(RERUN, rerun))) {
        crate::api::generate_to_disk(&Cli::parse_from(argv)).unwrap_or_else(|e| {
            panic!("the hand-written invocation for `{name}` must generate: {e}")
        });
    }

    // (2) The bytes. `runtime/` is compared alongside `gen/` because `[runtime]` writes OUTSIDE the
    // output directories, and a shared runtime exported at the wrong crate's flavor is exactly the
    // failure that table exists to prevent.
    let from_config = tree_bytes(&config_root, &["gen", "runtime"]);
    let from_flags = tree_bytes(&flags_root, &["gen", "runtime"]);
    assert!(
        from_config.len() > 40,
        "the fixture must generate a whole project, got {} files",
        from_config.len()
    );
    if let Some(difference) = first_tree_difference("config", &from_config, "flags", &from_flags) {
        panic!(
            "a config run and the flag invocations it expands to must produce the same bytes, but \
             {difference}"
        );
    }

    // The carrier derivation, made observable. `basic` sorts first and would export a runtime with
    // neither companion; `core`'s flavor is the join, so the runtime carries both. Asserted on the
    // runtime's own manifest so the fixture cannot quietly stop distinguishing the two.
    let runtime_manifest =
        String::from_utf8(from_config["runtime/Cargo.toml"].clone()).expect("utf-8 manifest");
    for companion in ["serde", "schemars"] {
        assert!(
            runtime_manifest.contains(companion),
            "the shared runtime must be exported at the JOIN of every crate's flavor, not at the \
             alphabetically first crate's — `{companion}` is missing from:\n{runtime_manifest}"
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

/// **One invocation settles a cold workspace, and the next one is a no-op** — the property the
/// convergence pass exists for, and the one the config feature could not previously claim.
///
/// The two edge kinds want opposite generation orders, so no single ordered pass satisfies both: on
/// the first pass `core` reads a `borrowed_collections.rs` that `ledger` writes afterwards. That is
/// not engineered away — it is settled INSIDE the invocation, by re-running exactly the crates whose
/// consumed sidecars the first pass rewrote.
///
/// What is asserted, in order:
///
/// 1. **Run 1 exits 0 over a cold tree.** That is the committed-state verdict pinned directly: it
///    reports a workspace whose consumer imports a wrapper its dependency does not host, and after
///    the convergence pass there is no such wrapper.
/// 2. **The convergence pass really ran**, evidenced on the file it changes: `core`'s wrapper index
///    hosts `CoreThingList`, which it can only do having been generated AFTER `ledger` recorded the
///    borrow. Without this the test would pass over a fixture whose edge carries no payload — the
///    guard the old `assert_ne!(cold, warm)` provided before run 1 and run 2 became the same tree.
/// 3. **Run 2 is byte-identical, and consumes no changed sidecar.** This is also what pins the
///    absence of the residual convergence WARNING on run 1, rather than a proxy for it: that warning
///    fires only if a sidecar moved across run 1's convergence pass, and a crate that read a moved
///    sidecar would generate different bytes when run 2 re-ran it against the settled one. A
///    byte-identical run 2 is therefore incompatible with the warning having fired.
/// 4. **Run 3 too**, so the fixed point is a fixed point rather than a two-cycle.
#[test]
fn a_config_run_converges_and_then_repeats_byte_for_byte() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_idempotence_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "ledger_rec = [t: core_thing, l: [* core_thing]]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.ledger]\ninput = \"specs/ledger.cddl\"\noutput = \"gen/ledger\"\n\
             lib-name = \"ledger-lib\"\ndeps = [\"core\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    let expanded = config::load(&config_path).unwrap().expand(&[]).unwrap();

    // (1) One cold run, exit 0. The bracketing check still reports `core` — the sidecars it consumed
    // went from absent to present across the whole invocation, which is true and is precisely what
    // triggered the convergence pass. What it does NOT mean any more is "run this again".
    let cold = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run must converge inside the invocation and exit 0");
    assert_eq!(
        cold.stale_crates(),
        ["core".to_owned()].into_iter().collect(),
        "the fixture must exercise the reverse edge: `core` has to be the crate whose consumed \
         sidecars this run wrote, or there is nothing for the convergence pass to trigger on"
    );
    let after_first = tree_bytes(&dir, &["gen"]);

    // (2) The pass ran, on the file only it can write: the wrapper `ledger` borrows.
    let index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        index.contains("CoreThingList"),
        "one invocation must leave the dependency hosting what its consumer borrows — `core`'s \
         index can only carry this having been generated after `ledger` recorded the borrow:\n{index}"
    );
    let sidecar =
        std::fs::read_to_string(dir.join("gen/ledger/wasm/src/generated/borrowed_collections.rs"))
            .unwrap();
    assert!(
        sidecar.contains("use core_lib_wasm::collections::CoreThingList;"),
        "and the consumer must be the one asking for it:\n{sidecar}"
    );

    // (3) Run 2: nothing to do, nothing changed.
    let warm = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None).expect("the second run must generate");
    assert!(
        warm.stale_crates().is_empty(),
        "a settled workspace must consume no sidecar this run rewrites — one still moving here \
         means the cross-crate edges feed back into each other, which one extra pass cannot bound. \
         Stale: {:?}",
        warm.stale_crates()
    );
    assert_eq!(
        warm.warning(&config_path, &[]),
        None,
        "and the warning the run prints from that same check must be silent on a full run — it is \
         retained for the subset case the convergence pass cannot reach, not for this one"
    );
    let after_second = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("first run", &after_first, "second run", &after_second)
    {
        panic!(
            "a cold config run must settle the workspace, so the next one changes nothing, but \
             {difference}"
        );
    }

    // (4) And the fixed point is a fixed point.
    config::generate(&config_path, &[], None, None).expect("the repeat run must generate");
    let after_third = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("second run", &after_second, "third run", &after_third)
    {
        panic!("a converged config run must stay converged, but {difference}");
    }

    let _ = std::fs::remove_dir_all(&dir);
}

/// A second generation of the same crate inside one invocation must never be silent about why.
///
/// The convergence pass is the one place a config run generates a crate twice, and a log that showed
/// `[core] generating …` twice with nothing between them would be baffling — the reader cannot tell
/// a re-run from a bug. So the pass prints, per crate, the sidecars that moved under it, and this
/// pins that text on the same [`config::Convergence`] the run itself reads it from.
#[test]
fn the_convergence_pass_says_which_crate_it_re_runs_and_why() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_rerun_notes_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "ledger_rec = [l: [* core_thing]]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.ledger]\ninput = \"specs/ledger.cddl\"\noutput = \"gen/ledger\"\n\
             lib-name = \"ledger-lib\"\ndeps = [\"core\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    let expanded = config::load(&config_path).unwrap().expand(&[]).unwrap();

    let cold = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run must converge and exit 0");
    let notes = cold.rerun_notes();
    assert_eq!(
        notes.len(),
        1,
        "one crate reads sidecars here, so the pass has exactly one crate to announce: {notes:?}"
    );
    for expected in [
        "[converge]",
        "re-running `core`",
        "borrowed_collections.rs",
        "borrowed_key_types.rs",
        "a pass behind",
    ] {
        assert!(
            notes[0].contains(expected),
            "the note must carry {expected:?} — the crate, the files that moved, and why it is \\
             being generated again — got: {}",
            notes[0]
        );
    }

    // A settled workspace announces nothing: no crate is re-run, so there is no line to print.
    let warm = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None).expect("the second run must generate");
    assert!(
        warm.rerun_notes().is_empty(),
        "a converged run must print no convergence line at all: {:?}",
        warm.rerun_notes()
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// The case the whole committed-state verdict exists for, on real disk: a **subset** run that adds a
/// borrow.
///
/// This is the one the bracketing [`config::Convergence`] check cannot see, and the test asserts that
/// blindness rather than assuming it — `stale_crates()` is empty across the subset run, because
/// `Convergence` watches only sidecars THIS run consumed and the dependency was not in the run.
/// Before the verdict existed, the whole sequence below printed nothing and exited 0 over a workspace
/// whose consumer imports a wrapper class no crate defines.
///
/// The sequence is: converge a two-crate workspace, add a second borrow to the consumer's spec,
/// regenerate the consumer ALONE, then follow the verdict's own instruction and watch it clear. That
/// last step is what makes the message actionable rather than merely alarming.
#[test]
fn a_subset_run_that_adds_a_borrow_is_reported_against_the_committed_tree() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_subset_borrow_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    let ledger_spec = dir.join("specs/ledger.cddl");
    std::fs::write(&ledger_spec, "ledger_rec = [l: [* core_thing]]\n").unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.ledger]\ninput = \"specs/ledger.cddl\"\noutput = \"gen/ledger\"\n\
             lib-name = \"ledger-lib\"\ndeps = [\"core\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();

    // Converge, in one invocation: the convergence pass leaves `core` hosting `CoreThingList`.
    config::generate(&config_path, &[], None, None)
        .expect("one cold config run must converge and exit 0");

    // Now the case. The consumer's spec gains a map over the dependency's type, so it borrows a
    // wrapper the dependency has never been asked for, and only the consumer is regenerated.
    std::fs::write(
        &ledger_spec,
        "ledger_rec = [l: [* core_thing], m: {* uint => core_thing}]\n",
    )
    .unwrap();
    let selected = vec!["ledger".to_owned()];
    let expanded = config::load(&config_path)
        .unwrap()
        .expand(&selected)
        .unwrap();
    let bracketing = config::Convergence::capture(&expanded);
    let verdict = config::generate(&config_path, &selected, None, None)
        .expect_err("a subset run that adds a borrow leaves the workspace unbuildable");

    assert!(
        bracketing.stale_crates().is_empty(),
        "the bracketing check must be silent here — that blindness is what the verdict exists for, \
         and a fixture where it fires would not be testing the gap. Stale: {:?}",
        bracketing.stale_crates()
    );
    // The verdict path returns the TYPED error, which is what carries its exit code out to `main`
    // (pinned at the process level by `the_committed_state_verdict_exits_2_and_other_failures_exit_1`).
    // Asserted here because this is the one fixture that reaches the verdict path in-process, and a
    // classification done by message text rather than by type would be one reword away from silently
    // becoming exit 1.
    assert!(
        verdict
            .downcast_ref::<config::VerdictError>()
            .is_some_and(|typed| typed.to_string() == verdict.to_string()),
        "the verdict must arrive as `VerdictError`, displaying the message verbatim, got: {verdict}"
    );
    let verdict = verdict.to_string();
    for expected in ["`core` does not host", "MapU64ToCoreThing", "`ledger`"] {
        assert!(
            verdict.contains(expected),
            "the verdict must name the crate that must change and what it is missing ({expected:?}), \
             got: {verdict}"
        );
    }

    // The tree really is broken: the consumer compiles an import of a class the dependency's index
    // does not re-export. This is what makes the verdict a statement about the tree rather than
    // about the run.
    let sidecar =
        std::fs::read_to_string(dir.join("gen/ledger/wasm/src/generated/borrowed_collections.rs"))
            .unwrap();
    assert!(sidecar.contains("use core_lib_wasm::collections::MapU64ToCoreThing;"));
    let index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(!index.contains("MapU64ToCoreThing"));

    // Follow the instruction the verdict printed: a dependency-alone regen, which clears it.
    assert!(
        verdict.contains(&format!("--config {} core", config_path.display())),
        "the verdict must print the command that settles it, got: {verdict}"
    );
    config::generate(&config_path, &["core".to_owned()], None, None)
        .expect("the dependency-alone regen the verdict names must converge the workspace");
    let index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        index.contains("MapU64ToCoreThing"),
        "the dependency must now host the wrapper, got:\n{index}"
    );
    config::generate(&config_path, &[], None, None).expect("and the converged workspace is silent");

    let _ = std::fs::remove_dir_all(&dir);
}

/// **A `deps` chain two edges deep converges in one invocation** — `app → mid → core`, with a real
/// borrow on BOTH edges.
///
/// Every other on-disk config fixture stops at a single edge, and a single edge cannot distinguish
/// the two claims the convergence pass rests on. At depth 1 the pass re-runs only pure dependencies:
/// crates that consume a sidecar and write none. At depth 2 the pass re-runs a crate that is BOTH —
/// `mid` reads `app`'s request sidecars and writes its own for `core` — which is the only shape where
/// one pass could leave a crate a pass behind, and the only shape where the residual check has a
/// second channel to measure.
///
/// The chain is legal precisely because exports are NOT transitive (`integration-other.mdx`: "a
/// dependency's own deps never travel through its export"), so `app` references `mid`'s own types and
/// never `core`'s. That is the same invariant the one-pass argument in `config::generate` rests on,
/// which is why this fixture is its test rather than merely a bigger example.
#[test]
fn a_two_edge_dependency_chain_converges_in_one_invocation() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_chain_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    // Each level defines a type of its OWN and borrows a wrapper over the level below it, so both
    // edges carry a payload rather than merely existing.
    std::fs::write(
        dir.join("specs/mid.cddl"),
        "mid_thing = [c: uint]\nmid_rec = [l: [* core_thing], k: {* core_thing => uint}]\n",
    )
    .unwrap();
    let app_spec = dir.join("specs/app.cddl");
    std::fs::write(
        &app_spec,
        "app_rec = [l: [* mid_thing], k: {* mid_thing => uint}]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.mid]\ninput = \"specs/mid.cddl\"\noutput = \"gen/mid\"\n\
             lib-name = \"mid-lib\"\ndeps = [\"core\"]\n\n\
             [crates.app]\ninput = \"specs/app.cddl\"\noutput = \"gen/app\"\n\
             lib-name = \"app-lib\"\ndeps = [\"mid\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    let expanded = config::load(&config_path).unwrap().expand(&[]).unwrap();
    assert_eq!(
        expanded.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
        vec!["core", "mid", "app"],
        "the chain must generate dependencies-first, or the fixture is not testing the order the \
         convergence pass compensates for"
    );

    // (1) One cold invocation, exit 0.
    let cold = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None).expect(
        "one cold run over a two-edge chain must converge inside the invocation and exit 0",
    );
    assert_eq!(
        cold.stale_crates(),
        ["core".to_owned(), "mid".to_owned()].into_iter().collect(),
        "both levels that host a borrow must be re-run: `core` because `mid` asked it for one, and \
         `mid` — the crate that is consumer AND dependency at once, which depth 1 cannot produce — \
         because `app` did"
    );
    let notes = cold.rerun_notes();
    assert_eq!(
        notes.len(),
        2,
        "the pass announces one line per re-run crate: {notes:?}"
    );
    assert!(
        notes[0].contains("re-running `core`") && notes[1].contains("re-running `mid`"),
        "and it announces them in generation order, dependencies first: {notes:?}"
    );
    let after_first = tree_bytes(&dir, &["gen"]);

    // (2) The honest-fixture guard, at BOTH levels: each dependency's wrapper index hosts what the
    // level above it borrows, which it can only do having been generated after that borrow was
    // recorded. Without this the test would pass over a chain whose edges carry no payload.
    let core_index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        core_index.contains("CoreThingList") && core_index.contains("MapCoreThingToU64"),
        "`core` must host both wrappers `mid` borrows:\n{core_index}"
    );
    let mid_index =
        std::fs::read_to_string(dir.join("gen/mid/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        mid_index.contains("MidThingList"),
        "and `mid` — itself a consumer — must host what `app` borrows from it:\n{mid_index}"
    );
    let mid_sidecar =
        std::fs::read_to_string(dir.join("gen/mid/wasm/src/generated/borrowed_collections.rs"))
            .unwrap();
    assert!(
        mid_sidecar.contains("use core_lib_wasm::collections::CoreThingList;"),
        "`mid` must be the one asking `core` for it:\n{mid_sidecar}"
    );
    let app_sidecar =
        std::fs::read_to_string(dir.join("gen/app/wasm/src/generated/borrowed_collections.rs"))
            .unwrap();
    assert!(
        app_sidecar.contains("use mid_lib_wasm::collections::MidThingList;"),
        "and `app` the one asking `mid`:\n{app_sidecar}"
    );
    // The other sidecar channel, at both levels: each level's map is keyed ON the type below it, so
    // `borrowed_key_types.rs` carries a real row and `--key-requests` has something to satisfy.
    for (consumer, dep, borrowed) in [
        ("mid", "core_lib", "core_thing"),
        ("app", "mid_lib", "mid_thing"),
    ] {
        let keys = std::fs::read_to_string(dir.join(format!(
            "gen/{consumer}/rust/src/generated/borrowed_key_types.rs"
        )))
        .unwrap();
        assert!(
            keys.contains(&format!("(\"{dep}\", \"{borrowed}\"")),
            "`{consumer}` must record the key type it borrows from `{dep}`, or the key channel of \
             this chain carries nothing:\n{keys}"
        );
    }

    // (3) Run 2 changes nothing, anywhere in the tree, and consumes no sidecar this run rewrote.
    let warm = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None).expect("the second run must generate");
    assert!(
        warm.stale_crates().is_empty(),
        "a settled chain must consume no sidecar this run rewrites — one still moving at depth 2 is \
         the feedback path one extra pass cannot bound. Stale: {:?}",
        warm.stale_crates()
    );
    assert_eq!(
        warm.warning(&config_path, &[]),
        None,
        "and the residual warning must be silent over a settled chain"
    );
    let after_second = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("first run", &after_first, "second run", &after_second)
    {
        panic!(
            "one cold run must settle a two-edge chain, so the next one changes nothing, but \
             {difference}"
        );
    }

    // (4) And the fixed point is a fixed point rather than a two-cycle.
    config::generate(&config_path, &[], None, None).expect("the repeat run must generate");
    let after_third = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("second run", &after_second, "third run", &after_third)
    {
        panic!("a converged chain must stay converged, but {difference}");
    }

    // (5) The residual instrument's BREADTH, on the one reachable shape where it comes apart from
    // the set of re-run crates. A settled chain whose TOP crate gains a borrow leaves `mid` a pass
    // behind (`app`'s sidecar moved under it) while `core` is not — `mid`'s own sidecar did not move,
    // because `mid`'s spec and `core`'s export are both unchanged. So the pass re-runs `{mid}` alone,
    // and an instrument narrowed to the re-run crates would stop watching the `mid → core` edge
    // across exactly the pass that rewrites `mid`. The unrestricted capture the residual check uses
    // keeps watching it, which is what makes the measurement cover the whole sidecar channel rather
    // than the part of it the pass happens to touch.
    std::fs::write(
        &app_spec,
        "app_rec = [l: [* mid_thing], k: {* mid_thing => uint}, m: {* uint => mid_thing}]\n",
    )
    .unwrap();
    let incremental = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None)
        .expect("a settled chain whose top crate gains a borrow must converge in one run");
    assert_eq!(
        incremental.stale_crates(),
        ["mid".to_owned()].into_iter().collect(),
        "only the middle crate is a pass behind here — `core` reads a sidecar that did not move"
    );
    assert_eq!(
        incremental.watched_crates(),
        ["core".to_owned(), "mid".to_owned()].into_iter().collect(),
        "but the check WATCHES both, and that gap is the point: the residual capture is taken over \
         every consumed sidecar, so the `mid → core` edge stays measured across a pass that re-runs \
         `mid` and not `core`"
    );
    // The narrowed instrument spelled out, so the gap above is a comparison rather than a claim: a
    // capture restricted to the re-run crates is a capture over those crates' expansion, and it sees
    // one edge where the unrestricted one sees two.
    let only_rerun = config::load(&config_path)
        .unwrap()
        .expand(&["mid".to_owned()])
        .unwrap();
    assert_eq!(
        config::Convergence::capture(&only_rerun).watched_crates(),
        ["mid".to_owned()].into_iter().collect(),
        "a capture narrowed to the re-run crates watches only what `mid` consumes — the `mid → core` \
         edge is absent from it, which is what the residual check must not be"
    );
    let mid_index =
        std::fs::read_to_string(dir.join("gen/mid/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        mid_index.contains("MapU64ToMidThing"),
        "and the incremental run must really have settled the new borrow:\n{mid_index}"
    );
    let settled = tree_bytes(&dir, &["gen"]);
    config::generate(&config_path, &[], None, None).expect("the run after it must generate");
    if let Some(difference) = first_tree_difference(
        "incremental run",
        &settled,
        "run after it",
        &tree_bytes(&dir, &["gen"]),
    ) {
        panic!("an incremental chain run must settle in one invocation too, but {difference}");
    }

    let _ = std::fs::remove_dir_all(&dir);
}

/// **A diamond converges in one invocation too** — `top → {left, right} → core`, a borrow on all
/// four edges.
///
/// The chain fixture establishes depth; this one establishes FAN, which is a different risk. Two
/// consumers ask `core` for the same wrapper on the same run, and one consumer borrows from two
/// dependencies at once — so the convergence pass re-runs three crates whose requests interleave,
/// and `core`'s index must end up hosting one shared class rather than racing between its two
/// requesters.
#[test]
fn a_diamond_dependency_graph_converges_in_one_invocation() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_diamond_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    // Both middles borrow the SAME wrapper from `core`; `top` borrows one from each middle.
    std::fs::write(
        dir.join("specs/left.cddl"),
        "left_thing = [x: uint]\nleft_rec = [l: [* core_thing]]\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("specs/right.cddl"),
        "right_thing = [y: uint]\nright_rec = [r: [* core_thing]]\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("specs/top.cddl"),
        "top_rec = [l: [* left_thing], r: [* right_thing]]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.left]\ninput = \"specs/left.cddl\"\noutput = \"gen/left\"\n\
             lib-name = \"left-lib\"\ndeps = [\"core\"]\n\n\
             [crates.right]\ninput = \"specs/right.cddl\"\noutput = \"gen/right\"\n\
             lib-name = \"right-lib\"\ndeps = [\"core\"]\n\n\
             [crates.top]\ninput = \"specs/top.cddl\"\noutput = \"gen/top\"\n\
             lib-name = \"top-lib\"\ndeps = [\"left\", \"right\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    let expanded = config::load(&config_path).unwrap().expand(&[]).unwrap();
    assert_eq!(
        expanded.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
        vec!["core", "left", "right", "top"],
        "the diamond must generate dependencies-first with the name tie-break"
    );

    // (1) One cold invocation, exit 0, and every crate that hosts a borrow is re-run.
    let cold = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None)
        .expect("one cold run over a diamond must converge inside the invocation and exit 0");
    assert_eq!(
        cold.stale_crates(),
        ["core".to_owned(), "left".to_owned(), "right".to_owned()]
            .into_iter()
            .collect(),
        "`core` is asked by both middles and each middle by `top`, so three crates are a pass behind"
    );
    let notes = cold.rerun_notes();
    assert_eq!(
        notes.len(),
        3,
        "one announced line per re-run crate: {notes:?}"
    );
    let after_first = tree_bytes(&dir, &["gen"]);

    // (2) The honest-fixture guard on all four edges: one shared class in `core`'s index, and each
    // middle hosting what `top` borrows from it.
    let core_index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        core_index.contains("CoreThingList"),
        "`core` must host the wrapper both middles borrow:\n{core_index}"
    );
    for (crate_name, wrapper) in [("left", "LeftThingList"), ("right", "RightThingList")] {
        let index = std::fs::read_to_string(dir.join(format!(
            "gen/{crate_name}/wasm/src/generated/collections.rs"
        )))
        .unwrap();
        assert!(
            index.contains(wrapper),
            "`{crate_name}` must host what `top` borrows from it:\n{index}"
        );
        let sidecar = std::fs::read_to_string(dir.join(format!(
            "gen/{crate_name}/wasm/src/generated/borrowed_collections.rs"
        )))
        .unwrap();
        assert!(
            sidecar.contains("use core_lib_wasm::collections::CoreThingList;"),
            "and it must be one of the two asking `core` for the shared one:\n{sidecar}"
        );
    }
    let top_sidecar =
        std::fs::read_to_string(dir.join("gen/top/wasm/src/generated/borrowed_collections.rs"))
            .unwrap();
    for import in [
        "use left_lib_wasm::collections::LeftThingList;",
        "use right_lib_wasm::collections::RightThingList;",
    ] {
        assert!(
            top_sidecar.contains(import),
            "`top` must borrow from BOTH of its dependencies ({import}):\n{top_sidecar}"
        );
    }

    // (3) Run 2 changes nothing, and (4) run 3 confirms the fixed point.
    let warm = config::Convergence::capture(&expanded);
    config::generate(&config_path, &[], None, None).expect("the second run must generate");
    assert!(
        warm.stale_crates().is_empty(),
        "a settled diamond must consume no sidecar this run rewrites. Stale: {:?}",
        warm.stale_crates()
    );
    assert_eq!(
        warm.warning(&config_path, &[]),
        None,
        "and the residual warning must be silent over a settled diamond"
    );
    let after_second = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("first run", &after_first, "second run", &after_second)
    {
        panic!(
            "one cold run must settle a diamond, so the next one changes nothing, but {difference}"
        );
    }
    config::generate(&config_path, &[], None, None).expect("the repeat run must generate");
    let after_third = tree_bytes(&dir, &["gen"]);
    if let Some(difference) =
        first_tree_difference("second run", &after_second, "third run", &after_third)
    {
        panic!("a converged diamond must stay converged, but {difference}");
    }

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// The exit-code contract, and the dependency-closing selector
// ---------------------------------------------------------------------------------------------

/// The two-crate workspace the binary-level fixtures below run over: `ledger deps core`, with a real
/// borrow on the edge, written into `dir`. Returns the config path.
///
/// Shared rather than copied because both fixtures need the SAME shape — the one the committed-state
/// verdict fires on — and a drift between them would make the second test's exit 0 mean something
/// different from the first's exit 2.
fn borrowing_pair_fixture(dir: &Path) -> std::path::PathBuf {
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(
        dir.join("specs/core.cddl"),
        "core_thing = [a: uint, b: text]\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "ledger_rec = [l: [* core_thing]]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.core]\ninput = \"specs/core.cddl\"\noutput = \"gen/core\"\n\
             lib-name = \"core-lib\"\n\n\
             [crates.ledger]\ninput = \"specs/ledger.cddl\"\noutput = \"gen/ledger\"\n\
             lib-name = \"ledger-lib\"\ndeps = [\"core\"]\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    config_path
}

/// The committed-state verdict exits **2**. Every other failure exits **1**.
///
/// The two say different things to whoever automates this command, and one exit code cannot carry
/// the difference: `1` means the run did not happen (a config that would not expand, a spec that
/// would not generate), so fixing the input is the remedy; `2` means the run did exactly what it was
/// asked and the committed workspace it wrote into does not build — repeating the command settles
/// nothing, and the message names the dependency that does.
///
/// Pinned against the REAL binary, because `main`'s termination is the only place the distinction
/// exists: every unit-level caller sees one `Err` either way.
#[test]
fn the_committed_state_verdict_exits_2_and_other_failures_exit_1() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_exit_codes_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let config_path = borrowing_pair_fixture(&dir);
    let run = |args: &[&str]| {
        super::integration_tests::codegen_cmd()
            .arg("--config")
            .arg(&config_path)
            .args(args)
            .output()
            .expect("the generator binary must be spawnable")
    };

    // A converged workspace: the run succeeded and the tree it wrote builds.
    let converged = run(&[]);
    assert_eq!(
        converged.status.code(),
        Some(0),
        "a cold config run must converge and exit 0\nstderr:\n{}",
        String::from_utf8_lossy(&converged.stderr)
    );

    // The consumer alone gains a borrow the dependency has never been asked for: the run itself
    // succeeds — every file it was asked to write is written — and the tree no longer builds.
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "ledger_rec = [l: [* core_thing], m: {* uint => core_thing}]\n",
    )
    .unwrap();
    let verdict = run(&["ledger"]);
    let stderr = String::from_utf8_lossy(&verdict.stderr);
    assert_eq!(
        verdict.status.code(),
        Some(2),
        "the verdict is a statement about the TREE, not about the run, so it gets its own exit \
         code\nstderr:\n{stderr}"
    );
    assert!(
        stderr.contains("the committed workspace does not build")
            && stderr.contains("MapU64ToCoreThing"),
        "and exit 2 must be the verdict rather than some other failure, got:\n{stderr}"
    );

    // A failure of the RUN, from the same tree in the same state: exit 1. Without this the exit-2
    // assertion above would pass over a binary that failed everything with 2.
    let unknown = run(&["nosuchcrate"]);
    assert_eq!(
        unknown.status.code(),
        Some(1),
        "an expansion failure is a failed run, not a verdict\nstderr:\n{}",
        String::from_utf8_lossy(&unknown.stderr)
    );
    assert!(
        String::from_utf8_lossy(&unknown.stderr).contains("is not a crate in this config"),
        "and it must be the expansion failure it was asked for"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// A crate that fails to generate says what the run already rewrote before it got there.
///
/// Generation is not transactional across crates and cannot be made so — each crate's output is a
/// committed directory clobbered in place — so the honest thing is to state the boundary. Without
/// this the failure is a bare CDDL diagnostic, and the one question it leaves ("what state is my tree
/// in now?") is answerable only by reading `git status` and knowing the generation order.
///
/// Binary-level, because the message is what a caller reads on stderr, and because the assertion that
/// matters is that the run really did rewrite the earlier crate before failing.
#[test]
fn a_mid_run_generation_failure_names_the_crates_already_regenerated() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_midrun_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    // Two independent crates, so generation order is crate-name order: `aaa` generates, then `zzz`
    // fails on a spec the parser refuses.
    std::fs::write(dir.join("specs/aaa.cddl"), "aaa_rec = [a: uint]\n").unwrap();
    std::fs::write(
        dir.join("specs/zzz.cddl"),
        "zzz_rec = [a: not_a_defined_rule]\n",
    )
    .unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n\n\
             [crates.aaa]\ninput = \"specs/aaa.cddl\"\noutput = \"gen/aaa\"\nlib-name = \"aaa-lib\"\n\n\
             [crates.zzz]\ninput = \"specs/zzz.cddl\"\noutput = \"gen/zzz\"\nlib-name = \"zzz-lib\"\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();

    let out = super::integration_tests::codegen_cmd()
        .arg("--config")
        .arg(&config_path)
        .output()
        .expect("the generator binary must be spawnable");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(1),
        "a spec that will not generate is a failed RUN, not a verdict about the tree\nstderr:\n\
         {stderr}"
    );
    assert!(
        stderr.contains("already regenerated in this run before this failure: aaa"),
        "the failure must name what the run had already rewritten, got:\n{stderr}"
    );
    assert!(
        stderr.contains("zzz") && stderr.contains("not transactional across crates"),
        "and it must name the crate that failed and state the contract, got:\n{stderr}"
    );
    // The claim the message makes is true: `aaa` really is on disk in its regenerated form.
    assert!(
        dir.join("gen/aaa/rust/src/generated/lib.rs").exists()
            || dir.join("gen/aaa/rust/src/lib.rs").exists(),
        "the earlier crate must actually have been written before the failure"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// `--with-deps` settles in ONE command what the plain selector reports and leaves for a second.
///
/// The workflow this is for: a consumer's spec gains a borrow, and the user regenerates the
/// consumer. The plain selection is right to trust the dependency's committed output — but here the
/// user already knows the dependency is affected, and the verdict's own instruction is to run it.
/// Closing the selection over `deps` is that instruction taken up front.
///
/// Binary-level because the closure lives on the command line: it is resolved once, before the run,
/// the listing and the diagnostics that quote the selection back can disagree about what is in it.
///
/// Also the DEFAULT-VERBOSITY visibility pin for the run-level orchestration lines. Every run below
/// passes no `--verbosity`, so what this reads off stdout is what a user sees with no flags: the
/// per-crate `[name] generating …` banners (a multi-crate run's only orientation) and the
/// `[converge] re-running` note. That second one matters beyond readability — AGENTS.md makes those
/// lines the REOPENING SIGNAL for config mode's in-memory fast path ("exceeding roughly half the
/// config's crates on routine edits"), and a signal nobody sees by default cannot fire.
#[test]
fn with_deps_settles_the_subset_case_in_one_command() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_with_deps_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let config_path = borrowing_pair_fixture(&dir);
    let run = |args: &[&str]| {
        super::integration_tests::codegen_cmd()
            .arg("--config")
            .arg(&config_path)
            .args(args)
            .output()
            .expect("the generator binary must be spawnable")
    };
    assert_eq!(
        run(&[]).status.code(),
        Some(0),
        "a cold config run must converge and exit 0"
    );

    // The same edit that makes the plain `ledger` subset run exit 2 (see
    // `the_committed_state_verdict_exits_2_and_other_failures_exit_1`).
    std::fs::write(
        dir.join("specs/ledger.cddl"),
        "ledger_rec = [l: [* core_thing], m: {* uint => core_thing}]\n",
    )
    .unwrap();
    let closed = run(&["--with-deps", "ledger"]);
    let stdout = String::from_utf8_lossy(&closed.stdout);
    assert_eq!(
        closed.status.code(),
        Some(0),
        "one command must settle it — the tree it leaves builds, so there is no verdict\nstdout:\n\
         {stdout}\nstderr:\n{}",
        String::from_utf8_lossy(&closed.stderr)
    );
    assert!(
        stdout.contains("[core] generating") && stdout.contains("[ledger] generating"),
        "the dependency must really have been pulled into the run:\n{stdout}"
    );
    assert!(
        stdout.contains("[converge] re-running `core`"),
        "and the second generation of `core` must announce itself at the DEFAULT verbosity — a \
         crate generated twice in one invocation with nothing between the two banners is baffling, \
         and these lines are the reopening signal for config mode's in-memory fast \
         path:\n{stdout}"
    );
    let index =
        std::fs::read_to_string(dir.join("gen/core/wasm/src/generated/collections.rs")).unwrap();
    assert!(
        index.contains("MapU64ToCoreThing"),
        "and it must host what the consumer now borrows, which is the whole point of closing the \
         selection:\n{index}"
    );

    // Settled, not merely silent: the next run over the same tree changes nothing at all.
    let settled = tree_bytes(&dir, &["gen"]);
    assert_eq!(run(&[]).status.code(), Some(0), "the tree must be settled");
    if let Some(difference) = first_tree_difference(
        "the --with-deps run",
        &settled,
        "the run after it",
        &tree_bytes(&dir, &["gen"]),
    ) {
        panic!("a `--with-deps` run must converge like any other, but {difference}");
    }

    // And the listing composes with it: `--print-flags` lists the CLOSED selection, and still
    // generates nothing.
    let listed = run(&["--print-flags", "--with-deps", "ledger"]);
    let listing = String::from_utf8_lossy(&listed.stdout);
    assert_eq!(listed.status.code(), Some(0), "the listing must succeed");
    assert!(
        listing.contains("[crates.core]") && listing.contains("[crates.ledger]"),
        "the listing must show the crates the run would contain, dependency included:\n{listing}"
    );
    if let Some(difference) = first_tree_difference(
        "before --print-flags",
        &settled,
        "after --print-flags",
        &tree_bytes(&dir, &["gen"]),
    ) {
        panic!("`--print-flags` must generate nothing, but {difference}");
    }

    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// Run verbosity under `--config`: the per-crate reach, the command-line override, and the run-level
// asymmetry. All three drive the real binary as a SUBPROCESS — the level is a process global and
// `cargo test` runs `#[test]`s on parallel threads, so an in-process assertion on printed output
// would race every other test that installs a level. See the same note in `integration_tests.rs`.
// ---------------------------------------------------------------------------------------------

/// A two-crate config whose per-crate `verbosity` values are supplied by the caller, with rule names
/// distinct per crate (`alpha_rec` / `beta_rec`) so a `- Handling rule:` banner can be ATTRIBUTED to
/// the crate that printed it. `extra_defaults` goes verbatim into `[defaults]`.
fn verbosity_pair_fixture(
    dir: &Path,
    extra_defaults: &str,
    alpha_keys: &str,
    beta_keys: &str,
) -> std::path::PathBuf {
    std::fs::create_dir_all(dir.join("specs")).unwrap();
    std::fs::write(dir.join("specs/alpha.cddl"), "alpha_rec = [a: uint]\n").unwrap();
    std::fs::write(dir.join("specs/beta.cddl"), "beta_rec = [b: text]\n").unwrap();
    let config_path = dir.join("cddl-codegen.toml");
    std::fs::write(
        &config_path,
        format!(
            "[defaults]\nstatic-dir = \"{}\"\n{extra_defaults}\n\
             [crates.alpha]\ninput = \"specs/alpha.cddl\"\noutput = \"gen/alpha\"\n\
             lib-name = \"alpha-lib\"\n{alpha_keys}\n\
             [crates.beta]\ninput = \"specs/beta.cddl\"\noutput = \"gen/beta\"\n\
             lib-name = \"beta-lib\"\n{beta_keys}\n",
            concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
        ),
    )
    .unwrap();
    config_path
}

/// Run `--config <path>` plus `args` through the real binary; assert exit 0 and return stdout.
fn config_run_stdout(config_path: &Path, args: &[&str]) -> String {
    let out = super::integration_tests::codegen_cmd()
        .arg("--config")
        .arg(config_path)
        .args(args)
        .output()
        .expect("the generator binary must be spawnable");
    assert_eq!(
        out.status.code(),
        Some(0),
        "the config run must succeed — verbosity decides what is PRINTED, never what \
         happens\nstderr:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// Per-crate verbosity is the reason this is a config key at all, so it is asserted rather than
/// assumed.
///
/// The shape that motivates the key is a multi-crate config where you want to raise the level for the
/// ONE crate you are debugging and leave the rest quiet. A run-global knob cannot express that; a key
/// merged through `[defaults]`/`[profiles.*]`/`[crates.*]` can, and this pins that it really does —
/// `alpha`'s rule banners appear, `beta`'s do not, in one run.
#[test]
fn verbosity_is_per_crate_under_config() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_verbosity_per_crate_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let config_path = verbosity_pair_fixture(&dir, "", "verbosity = \"debug\"\n", "");

    let stdout = config_run_stdout(&config_path, &[]);
    assert!(
        stdout.contains("- Handling rule: lib:alpha_rec"),
        "the crate that asked for `debug` must print its rule banners:\n{stdout}"
    );
    assert!(
        !stdout.contains("- Handling rule: lib:beta_rec"),
        "and the crate that did not ask must stay at the default level — otherwise one crate's key \
         leaks into the whole run:\n{stdout}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// `--config … --verbosity trace` wins over a committed `[defaults] verbosity = "error"`.
///
/// Two things in one behavioural assertion, because neither is worth much alone: the
/// `EXEMPT_ARG_IDS` pass-through that lets `--verbosity` reach a config invocation at all (every
/// other generation flag is refused there), and the precedence — the key is the project's committed
/// default, the command line is this invocation's override.
#[test]
fn command_line_verbosity_overrides_the_config_key() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_verbosity_override_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let config_path = verbosity_pair_fixture(&dir, "verbosity = \"error\"\n", "", "");

    // Baseline: the committed key really is in force, so the override below is proving something.
    let quiet = config_run_stdout(&config_path, &[]);
    assert!(
        quiet.trim().is_empty(),
        "`[defaults] verbosity = \"error\"` must silence the whole run:\n{quiet}"
    );

    let loud = config_run_stdout(&config_path, &["--verbosity", "trace"]);
    assert!(
        loud.contains("RustStructs:"),
        "a command-line `--verbosity trace` must reach the crates and restore the IR \
         dump:\n{loud}"
    );
    assert!(
        loud.contains("[alpha] generating") && loud.contains("[beta] generating"),
        "and it must move the RUN-level lines too — it applies to every crate, which is exactly \
         why it is allowed alongside --config:\n{loud}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}

/// Only `[defaults]` moves the run-level orchestration lines. Pinned so it is a decision rather than
/// an accident.
///
/// `config::generate` installs ONE level for everything it prints itself — the `[runtime]` notes, the
/// `[name] generating …` banner, the `[converge]` re-run notes, the residual convergence warning —
/// and each crate's own generation then installs its own under a guard that restores that one. So a
/// `[crates.*]` (or `[profiles.*]`) value governs only that crate's own output. This is the reading
/// that follows from the existing merge model rather than a special case: `[defaults]` is defined as
/// the value that reaches every crate, and the run is what contains every crate.
#[test]
fn run_level_messages_follow_the_defaults_key() {
    let dir = std::env::temp_dir().join(format!(
        "cddl_config_verbosity_runlevel_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();

    // Under `[defaults]`: the run level moves, so the per-crate banner goes with it.
    let from_defaults = dir.join("defaults");
    let config_path = verbosity_pair_fixture(&from_defaults, "verbosity = \"error\"\n", "", "");
    let stdout = config_run_stdout(&config_path, &[]);
    assert!(
        !stdout.contains("[alpha] generating") && !stdout.contains("[beta] generating"),
        "`[defaults] verbosity = \"error\"` is the value that reaches every crate, so it moves the \
         run-level lines too:\n{stdout}"
    );

    // The same value under `[crates.*]` only: the run level is untouched, so both banners stay —
    // including the banner of the crate that silenced ITSELF. A quiet crate must still be announced,
    // or a multi-crate log loses its only orientation.
    let from_crates = dir.join("crates");
    let config_path = verbosity_pair_fixture(
        &from_crates,
        "",
        "verbosity = \"error\"\n",
        "verbosity = \"error\"\n",
    );
    let stdout = config_run_stdout(&config_path, &[]);
    assert!(
        stdout.contains("[alpha] generating") && stdout.contains("[beta] generating"),
        "a `[crates.*]` verbosity governs only that crate's own generation — the run-level banner \
         is not its to suppress:\n{stdout}"
    );

    let _ = std::fs::remove_dir_all(&dir);
}
