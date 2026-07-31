//! The component face's CROSS-CRATE seam: `--component-extern-wit`, i.e. what happens when a
//! dependency reached through `--extern-import` also publishes a WIT package.
//!
//! Import mode is OPT-IN per dependency, and the two halves are tested as two halves:
//!
//! - WITH the flag, the dependency's types become IMPORTED WIT resources — the consumer's WIT `use`s
//!   them from a materialized copy of the dependency's own package, its `wit_bindgen::generate!`
//!   carries the co-required `with:` map, and dependency-typed values cross the guest boundary
//!   through a CBOR bytes seam whose fallibility reaches the signatures.
//! - WITHOUT it, nothing changes: a dependency type has no WIT projection, so every consumer
//!   signature naming one is recorded as `// unexported:` exactly as it is today. That half is
//!   asserted here rather than assumed, because it is the compatibility promise the whole opt-in
//!   rests on.
//!
//! The fixtures live under `tests/component-extern-import/`: one dependency generated as a real
//! crate (a stub tree has no component face to point at, which is the case the opt-in exists for)
//! and three consumers — the supported positions, the repeated positions this cddl-codegen refuses,
//! and the one naming a type the dependency's own WIT excluded.

use crate::cli::Cli;
use clap::Parser;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

const FIXTURES: &str = "tests/component-extern-import";

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn scratch(label: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "cddl_codegen_component_import_{label}_{}_{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// Generate the dependency to disk and hand back its output root. Written out rather than kept as a
/// string map because the two flags under test take PATHS: the whole point of this suite is the
/// consumer reading files another crate's run committed.
fn generate_dep(label: &str) -> PathBuf {
    let out = scratch(label);
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        &format!("{FIXTURES}/dep/lib.cddl"),
        "--output",
        out.to_str().unwrap(),
        "--wasm",
        "false",
        "--lib-name",
        "dep",
        "--component=true",
    ]);
    crate::api::generate_to_disk(&cli).expect("the dependency must generate");
    out
}

/// The consumer's generated files, or the graceful generation error.
///
/// `import_wit` is the OPT-IN under test: both shapes declare the dependency (`--extern-import`,
/// without which its rules are not in the consumer's namespace at all), and only one supplies the
/// dependency's WIT.
fn generate_consumer(
    spec: &str,
    dep_out: &Path,
    import_wit: bool,
    lib_name: &str,
) -> Result<BTreeMap<String, String>, String> {
    let mut args = vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        format!("{FIXTURES}/{spec}/lib.cddl"),
        "--output".to_owned(),
        "component_import_tests_unused".to_owned(),
        "--wasm".to_owned(),
        "false".to_owned(),
        "--lib-name".to_owned(),
        lib_name.to_owned(),
        "--component=true".to_owned(),
        "--extern-import".to_owned(),
        format!("dep={}", dep_out.join("extern-interface/dep").display()),
    ];
    if import_wit {
        args.push("--component-extern-wit".to_owned());
        args.push(format!("dep={}", dep_out.join("component/wit").display()));
    }
    let cli = Cli::parse_from(args);
    crate::api::generated_strings(&cli).map_err(|e| e.to_string())
}

// -------------------------------------------------------------------------------------------------
// Import mode: the WIT
// -------------------------------------------------------------------------------------------------

/// The `use` line, the materialized dependency package, and the four-stage validity verdict over the
/// two together — because a `use` of a package that is not in the tree resolves to nothing, and a
/// materialized package nothing uses is dead weight. Only the pair is the feature.
#[test]
fn component_import_uses_the_dep_package_from_a_materialized_copy() {
    let dep_out = generate_dep("uses");
    let files = generate_consumer("consumer", &dep_out, true, "consumer").expect("must generate");
    let wit = &files["component/wit/world.wit"];

    assert!(
        wit.contains("use cddl:dep/types@0.1.0.{policy, token};"),
        "the consumer's interface must `use` the dependency's types by their FULLY-QUALIFIED path \
         (package, interface and version), which is the only form that names another package:\n{wit}"
    );
    assert!(
        !wit.contains("coin"),
        "a transparent dependency alias resolves THROUGH to its target and must never reach the \
         `use` line — it names no WIT type on either side:\n{wit}"
    );
    assert!(
        !wit.contains("resource token"),
        "an IMPORTED type must not also be DEFINED here: two independently-defined resources do not \
         unify at composition, which is the whole reason to import one:\n{wit}"
    );

    let copied = files
        .get("component/wit/deps/dep/world.wit")
        .expect("the dependency's WIT must be materialized under the deps/ level");
    assert!(
        copied.contains("package cddl:dep@0.1.0;") && copied.contains("resource token {"),
        "the materialized copy must carry the dependency's own package declaration and types:\n{copied}"
    );

    // The whole tree through the pinned oracle: a cross-package `use` is exactly the shape that can
    // resolve against a package this run wrote and still fail to encode or validate.
    let wit_files: BTreeMap<String, String> = files
        .into_iter()
        .filter(|(path, _)| path.ends_with(".wit"))
        .collect();
    let bytes = crate::tests::component_tests::resolve_and_encode(&wit_files)
        .expect("the consumer's WIT plus its materialized deps must resolve and encode");
    crate::tests::component_tests::validate_component(&bytes)
        .expect("the encoded consumer package must validate");
}

/// The materialized copy is an INPUT from another crate, and its file-class comment has to say so —
/// it is the one file in the output tree that is neither generated here nor a read of this run's own
/// prior output, and that distinction is what keeps the determinism contract legible.
#[test]
fn the_materialized_dep_wit_declares_its_file_class_and_keeps_the_deps_own_banner() {
    let dep_out = generate_dep("fileclass");
    let files = generate_consumer("consumer", &dep_out, true, "consumer").expect("must generate");
    let copied = &files["component/wit/deps/dep/world.wit"];

    assert!(
        copied.contains("explicit cross-crate INPUT")
            && copied.contains("NOT a read of this run's prior output"),
        "the copy must state its determinism class in the terms the project uses for every other \
         cross-crate input:\n{copied}"
    );
    assert_eq!(
        copied.matches("This file was code-generated").count(),
        1,
        "the copy carries the DEPENDENCY's provenance banner and must not also be stamped with this \
         run's — the bytes are another crate's, and a second banner would attribute them here:\n{copied}"
    );
    assert!(
        !crate::generation::export::is_header_stamped_path("component/wit/deps/dep/world.wit"),
        "the deps/ subtree must be exempt from the header stamper for the reason above"
    );
    assert!(
        crate::generation::export::is_header_stamped_path("component/wit/world.wit"),
        "this crate's OWN emitted WIT is still stamped — the exemption is the copies, not the tree"
    );
}

// -------------------------------------------------------------------------------------------------
// Import mode: the guest crate
// -------------------------------------------------------------------------------------------------

/// C-P1: a materialized `wit/deps` tree is necessary but NOT sufficient. Without a `with:` row per
/// imported interface the macro panics at the CONSUMER's build, naming a key nobody wrote — so the
/// row is emitted by the same derivation that produces the copy, spelled with the key read out of
/// the dependency's own WIT.
#[test]
fn component_import_glue_carries_the_co_required_with_map() {
    let dep_out = generate_dep("with");
    let files = generate_consumer("consumer", &dep_out, true, "consumer").expect("must generate");
    let glue = &files["component/src/generated/mod.rs"];

    assert!(
        glue.contains("with: {") && glue.contains("\"cddl:dep/types@0.1.0\": generate,"),
        "the `generate!` invocation must carry one `with:` row per imported interface, keyed \
         exactly as the dependency's WIT spells it:\n{glue}"
    );
    // C-P4's other half, and the visible form of the transitive-import rule: an imported interface's
    // rust module is at the CRATE ROOT, while an exported one lives under `exports::`.
    assert!(
        glue.contains("use cddl::dep::types as wit_dep_dep_types;"),
        "an imported interface is reached from the crate root, not through `exports::`:\n{glue}"
    );
    assert!(
        glue.contains("use exports::cddl::consumer::types as wit_types;"),
        "the consumer's OWN interface is still reached through `exports::`:\n{glue}"
    );
}

/// The bytes seam, in both directions and in both non-repeated positions, plus the fallibility it
/// forces onto the signatures.
///
/// C-P4 is the load-bearing detail on the parameter side: an IMPORTED resource's `borrow<t>` lowers
/// to a plain `&T`, not to the `TBorrow<'_>` newtype an EXPORTED one gets. Reusing the exported
/// template here does not compile, and nothing in the emitted WIT can see the difference.
#[test]
fn component_import_crosses_scalar_dep_values_through_the_cbor_bytes_seam() {
    let dep_out = generate_dep("seam");
    let files = generate_consumer("consumer", &dep_out, true, "consumer").expect("must generate");
    let wit = &files["component/wit/world.wit"];
    let glue = &files["component/src/generated/mod.rs"];

    // The WIT half: every door touching a dependency type is fallible, because `from-cbor-bytes` on
    // the far side of the seam can fail on a value this crate's own serializer produced.
    assert!(
        wit.contains("main: func() -> result<token, string>;"),
        "a getter on a dependency-typed field is FALLIBLE on this face:\n{wit}"
    );
    assert!(
        wit.contains("spare: func() -> result<option<token>, string>;"),
        "an optional dependency-typed field's getter is fallible through the option:\n{wit}"
    );
    assert!(
        wit.contains("set-spare: func(spare: borrow<token>) -> result<_, string>;"),
        "a setter taking a dependency type is fallible on the same grounds:\n{wit}"
    );
    assert!(
        wit.contains("balance: func() -> u64;"),
        "a field whose type resolves through to a primitive is untouched — the seam is a property \
         of dependency HANDLES, not of dependency-declared names:\n{wit}"
    );
    assert!(
        wit.contains("label: func() -> string;"),
        "an own-crate field is untouched:\n{wit}"
    );

    // The guest half, return direction: serialize here, re-read on the dependency's instance.
    assert!(
        glue.contains(
            "wit_dep_dep_types::Token::from_cbor_bytes(\n            &<dep::Token as consumer::serialization::ToCBORBytes>::to_cbor_bytes(&me.main),\n        )\n        .map_err(err)"
        ),
        "the getter must mint the imported handle from THIS crate's serialization of its native \
         value:\n{glue}"
    );
    // ... and parameter direction: the imported handle's own `to-cbor-bytes`, read back natively.
    assert!(
        glue.contains(
            "<dep::Token as consumer::serialization::Deserialize>::from_cbor_bytes(\n            &spare.to_cbor_bytes(),\n        )\n        .map_err(err)"
        ),
        "a dependency-typed parameter must be read back from the handle's own CBOR:\n{glue}"
    );
    // C-P4.
    assert!(
        glue.contains("fn set_spare(&self, spare: &wit_dep_dep_types::Token)"),
        "an imported resource's `borrow<t>` lowers to `&T`, never to a `TBorrow<'_>` newtype — the \
         exported-resource template does not transfer:\n{glue}"
    );
    assert!(
        !glue.contains("TokenBorrow"),
        "no imported type may be spelled with the exported-resource borrow newtype:\n{glue}"
    );
    // The re-entrancy invariant survives the new conversion: the argument is materialized to an
    // owned value in its own statement, and only then is `self` mutably borrowed.
    let setter = glue
        .split("fn set_spare(")
        .nth(1)
        .expect("the setter is emitted");
    let (materialize, mutate) = (
        setter.find("let spare =").expect("the argument is bound"),
        setter.find("borrow_mut()").expect("self is written"),
    );
    assert!(
        materialize < mutate,
        "the dependency-typed argument must be materialized BEFORE `self` is mutably borrowed — two \
         live guards trap at runtime and poison the instance:\n{setter}"
    );
}

// -------------------------------------------------------------------------------------------------
// The two refusals
// -------------------------------------------------------------------------------------------------

/// A dependency type in a REPEATED parameter position. `list<borrow<t>>` over an imported resource
/// is legal WIT that wit-bindgen's Rust backend cannot lower, so emitting it would hand the user a
/// component crate that fails to compile inside a macro expansion. Refused at generation instead,
/// naming the position and the remedy.
#[test]
fn component_import_refuses_a_dep_type_in_a_repeated_parameter_position() {
    let dep_out = generate_dep("repeated");
    let err = generate_consumer("consumer-collections", &dep_out, true, "cc")
        .expect_err("a dependency type in a collection parameter must be refused");

    for position in ["items", "keyed", "valued"] {
        assert!(
            err.contains(&format!("in parameter `{position}`")),
            "every repeated position must be reported, not just the first — `{position}` is \
             missing:\n{err}"
        );
    }
    assert!(
        err.contains(
            "an imported resource may only be borrowed in a NON-REPEATED parameter position"
        ) && err.contains("wit-bindgen's Rust backend cannot lower (E0506)"),
        "the message must name the CAUSE, so a future toolchain fix has something to search \
         for:\n{err}"
    );
    assert!(
        err.contains("drop `--component-extern-wit dep=…`"),
        "the message must name the remedy a user can actually apply today:\n{err}"
    );
}

/// A consumer signature naming a dependency type the dependency's own WIT recorded as
/// `// unexported:`. It parses (the name IS in the dependency's extern-interface export) and only
/// the WIT seam knows there is nothing to `use`, so the refusal quotes the dependency's own recorded
/// reason verbatim rather than inventing one.
#[test]
fn component_import_refuses_a_dep_type_the_deps_own_wit_excluded() {
    let dep_out = generate_dep("excluded");
    // The reason is read out of the dependency's committed WIT — the same file the consumer resolves
    // types from — so the test reads it from there too rather than restating it.
    let dep_wit = std::fs::read_to_string(dep_out.join("component/wit/world.wit")).unwrap();
    let reason = dep_wit
        .lines()
        .find_map(|l| l.trim().strip_prefix("// unexported: Marker — "))
        .expect("the dependency's WIT must record Marker as unexported")
        .to_owned();

    let err = generate_consumer("consumer-unexported", &dep_out, true, "cu")
        .expect_err("a signature naming an unexported dependency type must be refused");
    assert!(
        err.contains(&reason),
        "the refusal must quote the DEPENDENCY's recorded reason verbatim — a reason restated here \
         would drift from the one the dependency actually wrote.\nexpected to contain: \
         {reason}\ngot: {err}"
    );
    assert!(
        err.contains("the dependency `dep` records it as unexported from its own WIT")
            && err.contains("The fix is on the DEPENDENCY's side"),
        "the refusal must name the dependency and where the fix lives:\n{err}"
    );
}

// -------------------------------------------------------------------------------------------------
// The opt-in's other half
// -------------------------------------------------------------------------------------------------

/// The compatibility promise the whole opt-in rests on: a dependency with no `--component-extern-wit`
/// generates exactly what it generates today. Asserted rather than assumed, because the fallback is
/// what makes the flag optional — a dependency with no component face (a hand-written crate, a stub
/// tree) has no WIT to point at, and must still be generatable under `--component`.
#[test]
fn a_dep_without_the_wit_flag_keeps_todays_unprojected_fallback() {
    let dep_out = generate_dep("fallback");
    let files =
        generate_consumer("consumer", &dep_out, false, "consumer").expect("must still generate");
    let wit = &files["component/wit/world.wit"];

    assert!(
        !wit.contains("use cddl:dep/"),
        "without the flag the dependency's package is not imported at all:\n{wit}"
    );
    assert!(
        wit.contains("// unexported: Wallet — references excluded Policy"),
        "without the flag a dependency type has no WIT projection, so a consumer signature naming \
         one is recorded as unexported — today's behavior, unchanged:\n{wit}"
    );
    assert!(
        !files.keys().any(|k| k.starts_with("component/wit/deps/")),
        "nothing is materialized for a dependency that is not in import mode"
    );
}

// -------------------------------------------------------------------------------------------------
// Flag validation
// -------------------------------------------------------------------------------------------------

/// The two combination rules, both refused before any generation starts: the flag needs the face it
/// feeds, and it needs the dependency DECLARATION that puts the dependency's types in this spec's
/// namespace at all (the WIT says how they cross; it does not put them there).
#[test]
fn the_wit_flag_requires_the_component_face_and_a_declared_dependency() {
    let missing_face = Cli::parse_from([
        "cddl-codegen",
        "--input",
        &format!("{FIXTURES}/consumer/lib.cddl"),
        "--output",
        "component_import_tests_unused",
        "--component-extern-wit",
        "dep=/nonexistent",
    ]);
    let err = crate::api::validate_flag_combinations(&missing_face)
        .expect_err("--component-extern-wit without --component must be refused");
    assert!(
        err.contains("--component-extern-wit requires --component=true"),
        "unexpected message: {err}"
    );

    let undeclared = Cli::parse_from([
        "cddl-codegen",
        "--input",
        &format!("{FIXTURES}/consumer/lib.cddl"),
        "--output",
        "component_import_tests_unused",
        "--component=true",
        "--component-extern-wit",
        "dep=/nonexistent",
    ]);
    let err = crate::api::validate_flag_combinations(&undeclared)
        .expect_err("--component-extern-wit for an undeclared dependency must be refused");
    assert!(
        err.contains("names a dependency this run does not declare")
            && err.contains("--extern-import dep="),
        "unexpected message: {err}"
    );
}

/// A path that is not a dependency's WIT directory fails at LOAD, naming the flag value and what to
/// point it at — never silently, and never as a dangling `use` in the emitted WIT.
#[test]
fn a_wit_path_that_is_not_a_dep_package_is_refused_by_name() {
    let dep_out = generate_dep("badpath");
    let empty = scratch("badpath_empty");
    for (path, expected) in [
        (
            dep_out.join("extern-interface/dep"),
            "no `.wit` files found in the directory",
        ),
        (empty.join("does-not-exist"), "the path is not a directory"),
    ] {
        let cli = Cli::parse_from([
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            format!("{FIXTURES}/consumer/lib.cddl"),
            "--output".to_owned(),
            "component_import_tests_unused".to_owned(),
            "--wasm".to_owned(),
            "false".to_owned(),
            "--component=true".to_owned(),
            "--extern-import".to_owned(),
            format!("dep={}", dep_out.join("extern-interface/dep").display()),
            "--component-extern-wit".to_owned(),
            format!("dep={}", path.display()),
        ]);
        let err = crate::api::generated_strings(&cli)
            .err()
            .map(|e| e.to_string())
            .unwrap_or_else(|| panic!("{} must be refused", path.display()));
        assert!(
            err.contains(expected) && err.contains("--component-extern-wit dep="),
            "the refusal must name the flag value and the shape it wants.\nexpected to contain: \
             {expected}\ngot: {err}"
        );
    }
}
