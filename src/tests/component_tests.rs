//! The component face's own gates: the WIT projection's VALIDITY (four stages, in-process), its
//! independence from the wasm posture, the exclude-and-record contract, and the pinned messages of
//! the strong-uniqueness detector.
//!
//! The validity gate's oracle is the pinned `wit-parser` / `wit-component` / `wasmparser` trio, at
//! the `=0.247.0` toolchain floor — never a shelled-out `wasm-tools` binary, whose ambient version
//! (1.231-era) rejects the fallible constructors this face emits for every bounds-validating type.

use crate::cli::Cli;
use clap::Parser;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Every fixture the component gates sweep, with the flags it commits to. `--component=true` is
/// added by the harness.
///
/// `tests/multifile/inputs` is here for two reasons no single-scope fixture can cover: it is a
/// DIRECTORY input, so it exercises the multi-interface projection and the cross-interface `use`
/// edges; and it contains externs and type choices, so it exercises exclude-and-record and the
/// reference closure on a spec nobody wrote for this face.
const COMPONENT_FIXTURES: &[(&str, &[&str])] = &[
    ("tests/component-core/input.cddl", &[]),
    (
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true"],
    ),
    ("tests/multifile/inputs", &[]),
];

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn scratch_dir(label: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "cddl_codegen_component_{label}_{}_{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn cli_for(input: &str, extra: &[&str]) -> Cli {
    let mut args = vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        input.to_owned(),
        "--output".to_owned(),
        "component_tests_unused".to_owned(),
        "--component=true".to_owned(),
    ];
    args.extend(extra.iter().map(|a| (*a).to_owned()));
    Cli::parse_from(args)
}

/// One fixture's `component/wit/**` map, or the generation error.
fn wit_files(input: &str, extra: &[&str]) -> Result<BTreeMap<String, String>, String> {
    crate::api::wit_strings(&cli_for(input, extra)).map_err(|e| e.to_string())
}

/// The single emitted `.wit` of a spec written to a scratch file. Panics on a generation failure —
/// callers testing REJECTION use [`generate_error`].
fn wit_for_spec(spec: &str, extra: &[&str]) -> String {
    let dir = scratch_dir("spec");
    let path = dir.join("input.cddl");
    std::fs::write(&path, spec).unwrap();
    let out = wit_files(path.to_str().unwrap(), extra);
    std::fs::remove_dir_all(&dir).ok();
    out.unwrap_or_else(|e| panic!("generating the spec failed: {e}"))
        .into_values()
        .collect::<Vec<_>>()
        .join("\n")
}

/// The generation error a spec produces under `--component=true`, or `None` if it generates.
fn generate_error(spec: &str) -> Option<String> {
    let dir = scratch_dir("reject");
    std::fs::write(dir.join("input.cddl"), spec).unwrap();
    let out = wit_files(dir.join("input.cddl").to_str().unwrap(), &[]);
    std::fs::remove_dir_all(&dir).ok();
    out.err()
}

// -------------------------------------------------------------------------------------------------
// Gate 1: WIT validity, four stages
// -------------------------------------------------------------------------------------------------

/// Stages 1–3: write the files, resolve them, encode the package. Returns the encoded bytes.
fn resolve_and_encode(files: &BTreeMap<String, String>) -> Result<Vec<u8>, String> {
    let root = scratch_dir("wit");
    for (path, content) in files {
        let full = root.join(path);
        std::fs::create_dir_all(full.parent().unwrap()).unwrap();
        std::fs::write(&full, content).unwrap();
    }
    let wit_dir = root.join(crate::generation::layout::COMPONENT_WIT_DIR);
    let out = resolve_and_encode_dir(&wit_dir);
    std::fs::remove_dir_all(&root).ok();
    out
}

fn resolve_and_encode_dir(wit_dir: &Path) -> Result<Vec<u8>, String> {
    let mut resolve = wit_parser::Resolve::default();
    let (package, _) = resolve
        .push_path(wit_dir)
        .map_err(|e| format!("resolve failed: {e:?}"))?;
    wit_component::encode(&resolve, package).map_err(|e| format!("encode failed: {e:?}"))
}

/// Stage 4: validate the encoded component-model bytes. This is the stage the whole gate exists for
/// — the strong-uniqueness collision class survives resolve AND encode and fails only here.
fn validate_component(bytes: &[u8]) -> Result<(), String> {
    let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all());
    validator
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| format!("validate failed: {e}"))
}

#[test]
fn component_wit_validates() {
    for (input, flags) in COMPONENT_FIXTURES {
        let files = wit_files(input, flags)
            .unwrap_or_else(|e| panic!("generating {input} with {flags:?} failed: {e}"));
        assert!(
            files.contains_key(&format!(
                "{}/world.wit",
                crate::generation::layout::COMPONENT_WIT_DIR
            )),
            "{input} with {flags:?} emitted no world.wit — the gate would have been vacuous"
        );
        let bytes = resolve_and_encode(&files)
            .unwrap_or_else(|e| panic!("{input} with {flags:?}: {e}\n{files:#?}"));
        validate_component(&bytes)
            .unwrap_or_else(|e| panic!("{input} with {flags:?}: {e}\n{files:#?}"));
    }
}

/// The gate's NEGATIVE CONTROL. Without it the four-stage chain can silently degrade to a no-op:
/// the shape it exists to catch is exactly the one that passes the first three stages.
#[test]
fn component_wit_validity_gate_fails_a_strong_uniqueness_collision_at_stage_four() {
    // A resource carrying a member with the resource's OWN name. Legal to parse, legal to resolve,
    // legal to encode — and rejected by component validation, which is what makes the fourth stage
    // load-bearing and the in-generator detector necessary.
    let wit = "package test:collide@0.1.0;\n\
               \n\
               interface types {\n\
               \x20 resource transaction {\n\
               \x20   transaction: func() -> u64;\n\
               \x20 }\n\
               }\n\
               \n\
               world collide-world {\n\
               \x20 export types;\n\
               }\n";
    let dir = scratch_dir("negative");
    std::fs::write(dir.join("world.wit"), wit).unwrap();
    let encoded = resolve_and_encode_dir(&dir);
    std::fs::remove_dir_all(&dir).ok();

    let bytes = encoded.unwrap_or_else(|e| {
        panic!(
            "the collision fixture must pass stages 2-3 — if it now fails earlier, this control no \
             longer proves stage 4 is doing the work: {e}"
        )
    });
    let verdict = validate_component(&bytes);
    let err = verdict.expect_err(
        "the `transaction.transaction` collision validated — stage 4 has gone vacuous and the \
         whole gate would pass an unbuildable package",
    );
    assert!(
        err.contains("conflicts with previous name"),
        "stage 4 failed for an unexpected reason, so the control is not pinning the collision \
         class: {err}"
    );
}

/// A2's keyword list is maintained by hand as the UNION across toolchain versions; this is the pin
/// that every entry, escaped, is accepted by the parser that actually consumes the emitted WIT.
#[test]
fn wit_keywords_round_trip_escaped_through_the_resolver() {
    let mut wit = String::from("package test:keywords@0.1.0;\n\ninterface types {\n");
    for keyword in crate::generation::wit::WIT_KEYWORDS {
        wit.push_str(&format!(
            "  {}: func() -> u32;\n",
            crate::generation::wit::wit_escape(keyword)
        ));
    }
    wit.push_str("}\n\nworld keywords-world {\n  export types;\n}\n");
    let dir = scratch_dir("keywords");
    std::fs::write(dir.join("world.wit"), &wit).unwrap();
    let encoded = resolve_and_encode_dir(&dir);
    std::fs::remove_dir_all(&dir).ok();
    let bytes = encoded.unwrap_or_else(|e| {
        panic!("an escaped WIT_KEYWORDS entry is not accepted by wit-parser 0.247: {e}\n{wit}")
    });
    validate_component(&bytes).unwrap();
}

// -------------------------------------------------------------------------------------------------
// The purity invariant
// -------------------------------------------------------------------------------------------------

/// The emitted WIT must be a pure function of the spec and the component-relevant flags — in
/// particular BYTE-IDENTICAL with `--wasm` on and off.
///
/// The named hazard is real: parts of IR finalization are `cli.wasm`-gated
/// (`converge_anonymous_collection_instance_wasm` early-returns without it), so an anonymous
/// collection's IR spelling differs between the two postures. Resolving named collections and
/// aliases THROUGH is what keeps that difference out of the WIT, and this is the assertion that
/// keeps it that way.
#[test]
fn component_wit_is_wasm_posture_independent() {
    for (input, flags) in COMPONENT_FIXTURES {
        let mut with_wasm: Vec<&str> = flags.to_vec();
        with_wasm.push("--wasm=true");
        let mut without_wasm: Vec<&str> = flags.to_vec();
        without_wasm.push("--wasm=false");
        let a = wit_files(input, &with_wasm).unwrap();
        let b = wit_files(input, &without_wasm).unwrap();
        assert_eq!(
            a, b,
            "{input} with {flags:?} emits different WIT under --wasm=true and --wasm=false — the \
             projection has stopped resolving through the wasm-posture-sensitive IR spellings"
        );
    }
}

// -------------------------------------------------------------------------------------------------
// Exclude-and-record
// -------------------------------------------------------------------------------------------------

/// A spec carrying a phase-2 type class must generate a WIT WITHOUT it — plus a record of why —
/// never a crash and never a silent omission. The reference closure then removes the containers,
/// naming the ROOT of the chain rather than the immediate neighbour.
#[test]
fn component_wit_excludes_a_type_choice_and_everything_that_reaches_it() {
    let wit = wit_for_spec(
        "value = uint / text\n\
         inner = { v: value }\n\
         outer = { i: inner }\n\
         plain = { n: uint }\n",
        &[],
    );
    assert!(
        wit.contains(
            "// unexported: Value — type and group choices are not yet projected (phase 2)"
        ),
        "the type choice itself is not recorded:\n{wit}"
    );
    // A DIRECT reference to an unprojectable shape fails at the field mapping, so it records the
    // shape rather than the neighbour — strictly more informative than the closure's wording.
    assert!(
        wit.contains(
            "// unexported: Inner — references `Value`, a type or group choice, which is not yet \
             projected (phase 2)"
        ),
        "the directly-containing record is not excluded, or does not name the offending shape:\n{wit}"
    );
    // One level further out the closure takes over, and it names the ROOT of the chain rather than
    // its immediate neighbour.
    assert!(
        wit.contains("// unexported: Outer — references excluded Inner"),
        "the transitively-containing record is not excluded by the reference closure:\n{wit}"
    );
    assert!(
        !wit.contains("resource inner") && !wit.contains("resource outer"),
        "an excluded type still rendered a resource:\n{wit}"
    );
    assert!(
        wit.contains("resource plain"),
        "an unrelated type was dropped along with the excluded ones:\n{wit}"
    );
}

// -------------------------------------------------------------------------------------------------
// The strong-uniqueness detector's three pinned messages
// -------------------------------------------------------------------------------------------------

/// PACKAGE level: an interface name against the world name, or two interfaces against each other.
/// The root scope projects to `types`, so an input file literally named `types.cddl` collides with
/// it — the shape the flattening's non-injectivity makes reachable.
#[test]
fn wit_package_name_collision_is_rejected() {
    let dir = scratch_dir("pkgcollide");
    std::fs::write(dir.join("lib.cddl"), "root = { a: uint }\n").unwrap();
    std::fs::write(dir.join("types.cddl"), "leaf = { b: uint }\n").unwrap();
    let err = wit_files(dir.to_str().unwrap(), &[]).unwrap_err();
    std::fs::remove_dir_all(&dir).ok();
    assert!(
        err.contains("WIT package name collision under --component:")
            && err.contains("all convert to the WIT identifier `types`")
            && err.contains("share ONE namespace"),
        "unexpected package-level collision message: {err}"
    );
}

/// INTERFACE level: two type names colliding inside one interface. Reachable today because the
/// synthesized `any-cbor` alias shares the namespace with every user type, so a rule named
/// `any_cbor` in a spec that also uses `any` collides with it.
#[test]
fn wit_interface_type_name_collision_is_rejected() {
    let err = generate_error(
        "any_cbor = { x: uint }\n\
         holder = { m: any }\n",
    )
    .expect("the colliding spec generated");
    assert!(
        err.contains("WIT type name collision under --component:")
            && err.contains("all convert to the WIT identifier `any-cbor`")
            && err.contains("`@name` comment-DSL directive"),
        "unexpected interface-level collision message: {err}"
    );
}

/// RESOURCE level: a member sharing the resource's own name. The one collision the validity gate
/// alone would let through to a wasm-level error naming a mangled `[method]` symbol.
#[test]
fn wit_resource_member_collision_is_rejected() {
    let err = generate_error("transaction = { transaction: uint }\n")
        .expect("the colliding spec generated");
    assert!(
        err.contains("WIT resource member collision under --component:")
            && err.contains("the resource's own name")
            && err.contains("RESOLVES and even ENCODES but fails component validation"),
        "unexpected resource-level collision message: {err}"
    );
}

// -------------------------------------------------------------------------------------------------
// The `--lib-name` guard
// -------------------------------------------------------------------------------------------------

/// `--lib-name` has no `value_parser`, and a cargo package name may legally begin with a digit —
/// which the kebab converter refuses with an `assert!`. Under `--component` that assert is
/// REACHABLE from the command line, so the flag rule has to catch it first: flag problems are
/// graceful errors here, never panics.
#[test]
fn a_lib_name_that_is_not_a_wit_identifier_is_a_flag_error_not_a_panic() {
    let cli = cli_for("tests/component-core/input.cddl", &["--lib-name", "4chain"]);
    let err = crate::api::validate_flag_combinations(&cli)
        .expect_err("--lib-name 4chain --component=true was accepted");
    assert!(
        err.contains("cannot be used with --component=true") && err.contains("--wit-package"),
        "unexpected --lib-name rejection message: {err}"
    );
    // The same name is fine without the component face, which is what makes this a COMBINATION rule.
    let plain = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "tests/component-core/input.cddl",
        "--output",
        "component_tests_unused",
        "--lib-name",
        "4chain",
    ]);
    assert!(crate::api::validate_flag_combinations(&plain).is_ok());
}
