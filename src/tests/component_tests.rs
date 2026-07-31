//! The component face's own gates: the WIT projection's VALIDITY (four stages, in-process), its
//! independence from the wasm posture, the exclude-and-record contract, and the pinned messages of
//! the strong-uniqueness detector.
//!
//! The validity gate's oracle is the pinned `wit-parser` / `wit-component` / `wasmparser` trio, at
//! the `=0.247.0` toolchain floor — never a shelled-out `wasm-tools` binary, whose ambient version
//! (1.231-era) rejects the fallible constructors this face emits for every bounds-validating type.

use crate::cli::Cli;
use crate::tests::gate_cache;
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
/// reference closure on a spec nobody wrote for this face. `tests/component-multifile/inputs` covers
/// the same multi-interface shape with NO externs, which is what lets the build smoke compile it —
/// `tests/multifile`'s own rust crate needs a hand-written extern re-export before it builds at all.
pub(super) const COMPONENT_FIXTURES: &[(&str, &[&str])] = &[
    ("tests/component-core/input.cddl", &[]),
    (
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true"],
    ),
    // The FORCE-CANONICAL posture, swept beside the other two for every fixture that carries a
    // bytes seam: it is the one posture whose composed runtime drops the blanket `ToCBORBytes` impl
    // and puts both `to_cbor_bytes` and `to_canonical_cbor_bytes` on `Serialize`, so it is the one
    // that decides whether `to-canonical-cbor-bytes` is projected at all and which trait the glue
    // names for it.
    (
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    // The JSON posture. A flag, not a spec shape — which is why it is swept as extra rows on the
    // fixtures that already exist rather than as a fixture of its own.
    (
        "tests/component-core/input.cddl",
        &["--json-serde-derives=true"],
    ),
    // Type and group choices. Swept in BOTH encoding postures because a choice's `kind` / `as-`
    // arms are the one glue shape `--preserve-encodings` re-spells (tuple arms become named-field
    // arms), which is exactly the hazard the `int` bridge already carries.
    ("tests/component-choices/input.cddl", &[]),
    (
        "tests/component-choices/input.cddl",
        &["--preserve-encodings=true"],
    ),
    (
        "tests/component-choices/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    // A choice is where the JSON seam lands on a resource that has no constructor, so the seam's
    // two members sit beside the `new-<variant>` statics rather than beside a `constructor`.
    (
        "tests/component-choices/input.cddl",
        &["--json-serde-derives=true"],
    ),
    // Value windows: the one class whose WIT signature is identical whether the glue enforces the
    // window or ignores it, so the WIT sweep alone can never judge it.
    ("tests/component-bounds/input.cddl", &[]),
    (
        "tests/component-bounds/input.cddl",
        &["--preserve-encodings=true"],
    ),
    (
        "tests/component-bounds/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    // The bridging classes — extern, raw bytes, a generic extern base and its instance, and a
    // non-extern generic instance. A DIRECTORY input, so it carries no corpus-parity obligation.
    ("tests/component-extern/inputs", &[]),
    // The bridges in the two flag postures that decide what a bridging resource may NAME: the
    // canonical one adds `to-canonical-cbor-bytes` to the extern's seam (its contract does require
    // `Serialize`) and must leave the raw-bytes bridge alone, and the JSON one must add nothing to
    // either (nothing imposes serde on a user-owned type).
    (
        "tests/component-extern/inputs",
        &["--preserve-encodings=true", "--canonical-form=true"],
    ),
    (
        "tests/component-extern/inputs",
        &["--json-serde-derives=true"],
    ),
    ("tests/component-multifile/inputs", &[]),
    // Cross-scope references that run THROUGH a named collection: the projection resolves the
    // collection through, so the cycle detector must agree about which scope the `use` points at.
    ("tests/component-collection-refs/inputs", &[]),
    // The `@name` remedy every collision message names, applied to the two collision classes a
    // rename can actually move.
    ("tests/component-rename/input.cddl", &[]),
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

/// The single emitted `.wit` of a FIXTURE (a path, not a spec body). Panics on a generation failure.
fn wit_of(input: &str, extra: &[&str]) -> String {
    wit_files(input, extra)
        .unwrap_or_else(|e| panic!("generating {input} with {extra:?} failed: {e}"))
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

/// A spec carrying a still-unprojected type class must generate a WIT WITHOUT it — plus a record of
/// why — never a crash and never a silent omission. The reference closure then removes the
/// containers, naming the ROOT of the chain rather than the immediate neighbour.
///
/// Pointed at the GENERIC EXTERN BASE, which is the class that is still unprojectable now that
/// externs and raw-bytes types are bridged. Re-pointed rather than deleted: the exclude-and-record
/// path must keep a live test, and losing its last subject to an implementation is exactly when it
/// would silently go untested.
///
/// The subject is not arbitrary. A base names no concrete rust type — only `Base<Args>` instances do
/// — so there is nothing for a bridging resource to wrap, and it is SKIPPED from the projection the
/// way a named collection is: neither included nor excluded. That makes a bare reference to one the
/// shape the exclusion machinery has to speak about, and the positive halves below (the extern and
/// the instance beside it, both bridged) are what keep the negative half honest.
#[test]
fn component_wit_excludes_a_generic_extern_base_and_everything_that_reaches_it() {
    let wit = wit_for_spec(
        // The base must be INSTANTIATED somewhere or it is not a generic extern base at all — a
        // plain-declared, never-instantiated extern is an ordinary one and gets bridged.
        "ext = _CDDL_CODEGEN_EXTERN_TYPE_\n\
         extern_generic = _CDDL_CODEGEN_EXTERN_TYPE_\n\
         inst = { g: extern_generic<ext> }\n\
         inner = { b: extern_generic }\n\
         outer = { i: inner }\n\
         plain = { n: uint }\n",
        &[],
    );
    // The base is SKIPPED, not excluded: it owns no WIT type and no exclusion row. A record of it
    // would be a record of a type the WIT was never going to carry.
    assert!(
        !wit.contains("unexported: ExternGeneric ") && !wit.contains("resource extern-generic {"),
        "the generic extern base is being surfaced or recorded rather than skipped:\n{wit}"
    );
    // A DIRECT reference fails at the field mapping, so it records the SHAPE rather than the
    // neighbour — strictly more informative than the closure's wording.
    assert!(
        wit.contains(
            "// unexported: Inner — references the generic extern base `ExternGeneric`, which \
             names no concrete type — only its instances (`ExternGeneric<…>`) are bridged"
        ),
        "the directly-referencing record is not excluded, or does not name the offending shape:\n{wit}"
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
    // The positive control: the ordinary extern and the generic INSTANCE are bridged, so what is
    // excluded above is the base alone and not externs having stopped projecting.
    assert!(
        wit.contains("resource ext {") && wit.contains("resource extern-generic-ext {"),
        "the extern or the generic INSTANCE stopped being bridged, which would make the exclusion \
         above prove something else entirely:\n{wit}"
    );
}

// -------------------------------------------------------------------------------------------------
// Type and group choices
// -------------------------------------------------------------------------------------------------

/// A choice's WIT surface. It is the one resource with NO constructor — there is nothing to build
/// without picking an arm — so the `new-<variant>` statics replace it, `kind` says which arm is
/// live and `as-<variant>` hands the payload back.
///
/// Every name in all three families comes from ONE conversion of `EnumVariant::name_as_var()`, which
/// is also the source of the rust `new_<variant>` the glue calls. Asserting them together is what
/// makes a drift between the three visible here rather than as a missing trait method three stages
/// later.
#[test]
fn component_wit_projects_a_choice_as_a_resource_with_statics_a_kind_and_getters() {
    let wit = wit_of("tests/component-choices/input.cddl", &[]);
    let outcome = wit
        .split("resource outcome {")
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .expect("the type choice must project to a resource");
    assert!(
        !outcome.contains("constructor("),
        "a choice resource carries a constructor — an arm has to be chosen, so there is nothing for \
         one to build:\n{outcome}"
    );
    // A VALUE-BOUNDED arm's rust `new_uint` is fallible, so the static is too; a plain arm's is not.
    assert!(
        outcome.contains("new-uint: static func(uint: u64) -> result<outcome, string>;")
            && outcome.contains("new-text: static func(text: string) -> outcome;"),
        "the statics no longer track the rust constructors' fallibility per arm:\n{outcome}"
    );
    // A class-backed arm borrows IN and hands an owned handle OUT.
    assert!(
        outcome.contains("new-stamp: static func(stamp: borrow<stamp>) -> outcome;")
            && outcome.contains("as-stamp: func() -> option<stamp>;"),
        "the class-backed arm's ownership positions changed:\n{outcome}"
    );
    // A FIXED-value arm takes nothing and has no `as-`: it carries no payload for one to return.
    assert!(
        outcome.contains("new-null: static func() -> outcome;") && !outcome.contains("as-null"),
        "the fixed-value arm no longer has a nullary static and no getter:\n{outcome}"
    );
    assert!(
        outcome.contains("kind: func() -> outcome-kind;"),
        "the choice lost its discriminant:\n{outcome}"
    );
    assert!(
        wit.contains("enum outcome-kind {\n    uint,\n    text,\n    stamp,\n    null,\n  }"),
        "the discriminant enum's cases no longer come from the same source the statics do:\n{wit}"
    );
    // A GROUP choice's multi-field arm mints its own struct, and its static takes that struct's
    // FIELDS — exactly as the rust `new_node1` does — while `as-` hands back the struct.
    let node = wit
        .split("resource node {")
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .expect("the group choice must project to a resource");
    assert!(
        node.contains("new-node1: static func(x: u64, y: string) -> node;")
            && node.contains("as-node1: func() -> option<node1>;"),
        "the multi-field group-choice arm's static no longer takes the arm struct's fields:\n{node}"
    );
    // A DESPECIALIZED arm (`[+ text]` → a plain `list<string>`) makes the static fallible for a
    // reason the rust constructor knows nothing about: the boundary re-enters the `TryFrom` door.
    assert!(
        wit.contains("new-labels: static func(labels: list<string>) -> result<label, string>;"),
        "the despecialized arm's static is no longer fallible:\n{wit}"
    );
}

/// The `<name>-kind` enum lives in the interface's ONE flat type namespace, so a user type
/// converging on it is a collision the detector must report rather than a name silently written
/// twice. Asserted because the enum is minted by the projection rather than by a rule, which is the
/// shape a namespace check is easiest to forget.
#[test]
fn a_choice_kind_enum_is_in_the_interface_type_namespace() {
    let err = generate_error(
        "state = uint / text\n\
         holder = [\n\
         \x20 ; @name state_kind\n\
         \x20 tag: 0, x: uint, y: text //\n\
         \x20 ; @name other\n\
         \x20 tag: 1, z: uint, w: text\n\
         ]\n",
    )
    .expect("the colliding spec generated");
    assert!(
        err.contains("WIT type name collision under --component:")
            && err.contains("the discriminant enum of the choice `State`")
            && err.contains("all convert to the WIT identifier `state-kind`"),
        "the kind enum is not part of the interface-level namespace check: {err}"
    );
}

/// The `kind` / `as-<variant>` arms must match the rust enum's ARM SHAPE, which
/// `--preserve-encodings` changes from a tuple to named fields — per variant, not per enum: a
/// `Rust`-typed arm carries no encoding fields and stays a tuple in the same enum where a primitive
/// arm becomes braced. A `match` written for one shape does not compile against the other, and the
/// user would get the error in GENERATED code. Sibling of
/// `component_glue_matches_the_int_arm_shape_of_the_encoding_posture`, on the shape that made the
/// hazard general.
#[test]
fn component_glue_matches_the_choice_arm_shape_of_the_encoding_posture() {
    let plain = component_glue("tests/component-choices/input.cddl", &[]);
    assert!(
        plain.contains("cddl_lib::Outcome::U64(uint) => Some(*uint),")
            && plain.contains("cddl_lib::Outcome::U64(_) => wit_types::OutcomeKind::Uint,"),
        "the default posture's choice arms no longer match the tuple shape:\n{plain}"
    );
    let preserve = component_glue(
        "tests/component-choices/input.cddl",
        &["--preserve-encodings=true"],
    );
    assert!(
        preserve.contains("cddl_lib::Outcome::U64 { uint, .. } => Some(*uint),")
            && preserve.contains("cddl_lib::Outcome::U64 { .. } => wit_types::OutcomeKind::Uint,"),
        "the preserve posture's choice arms no longer match the NAMED-field shape:\n{preserve}"
    );
    // Same enum, same posture, DIFFERENT arm shape: a `Rust`-typed arm has no encoding fields, so it
    // stays a tuple. A per-enum fork would get this one wrong.
    assert!(
        preserve.contains("cddl_lib::Outcome::Stamp(stamp) =>")
            && preserve.contains("cddl_lib::Outcome::Stamp(_) => wit_types::OutcomeKind::Stamp,"),
        "the arm shape is being decided per ENUM rather than per VARIANT:\n{preserve}"
    );
    // A fixed-value arm binds nothing at all, in either posture.
    for glue in [&plain, &preserve] {
        assert!(
            glue.contains("cddl_lib::Outcome::Null => wit_types::OutcomeKind::Null,"),
            "the fixed-value arm no longer matches as a unit variant:\n{glue}"
        );
    }
}

/// A `new-<variant>` static returns the owned HANDLE (the constructor/static asymmetry), and it
/// unwraps a fallible rust `new_<variant>` rather than wrapping it — the two halves of the shape
/// that a WIT gate cannot see and that a `Result` in the wrong position turns into a type error.
#[test]
fn component_glue_new_variant_statics_unwrap_the_rust_constructor() {
    let glue = component_glue("tests/component-choices/input.cddl", &[]);
    assert!(
        glue.contains(
            "fn new_uint(uint: u64) -> Result<wit_types::Outcome, String> {\n        let inner = \
             cddl_lib::Outcome::new_uint(uint).map_err(err)?;\n        \
             Ok(wit_types::Outcome::new(WitOutcome(RefCell::new(inner))))"
        ),
        "the value-bounded arm's static no longer unwraps the fallible rust constructor:\n{glue}"
    );
    // The other direction: a static that is fallible for a BOUNDARY reason (a despecialized `[+ T]`
    // arm) still calls an INFALLIBLE rust constructor, so it must not `?` it.
    assert!(
        glue.contains("let inner = cddl_lib::Label::new_labels(labels.try_into().map_err(err)?);"),
        "the despecialized arm's static no longer re-enters the `TryFrom` door, or wrongly unwraps \
         an infallible rust constructor:\n{glue}"
    );
}

// -------------------------------------------------------------------------------------------------
// Bridging resources: extern, raw bytes, generics
// -------------------------------------------------------------------------------------------------

/// The four bridging verdicts, on one fixture. Each is a decision about a type whose rust definition
/// the tool does NOT own, so each is only checkable against the contract that type already carries.
///
/// The raw-bytes row is the one that departs from a uniform "everything gets the cbor seam" rule,
/// and the departure is forced: a `_CDDL_CODEGEN_RAW_BYTES_TYPE_`'s contract is `RawBytesEncoding`
/// and nothing requires `Serialize` of it — the emitted extern-interface self-check asserts the
/// former and not the latter — so a `to-cbor-bytes` bridge would name a trait impl that need not
/// exist. That is the compile-error-in-generated-code class the `no_deserialize` fork already
/// exists to prevent, reached from the other direction.
#[test]
fn component_wit_bridges_externs_raw_bytes_and_generic_instances() {
    let wit = wit_of("tests/component-extern/inputs", &[]);
    let body = |resource: &str| {
        wit.split(&format!("resource {resource} {{"))
            .nth(1)
            .and_then(|rest| rest.split('}').next())
            .unwrap_or_else(|| panic!("the WIT carries no `resource {resource}`:\n{wit}"))
            .to_owned()
    };
    // An extern: the cbor seam and NOTHING else. No constructor and no getters — the tool knows
    // nothing about the user's type beyond the contract it already imposes on it.
    let ext = body("ext");
    assert!(
        ext.contains("to-cbor-bytes: func() -> list<u8>;")
            && ext
                .contains("from-cbor-bytes: static func(bytes: list<u8>) -> result<ext, string>;")
            && !ext.contains("constructor("),
        "the extern bridge is no longer the bare cbor seam:\n{ext}"
    );
    // A raw-bytes type: the RAW seam, and no cbor seam at all.
    let raw = body("raw");
    assert!(
        raw.contains("to-raw-bytes: func() -> list<u8>;")
            && raw.contains("from-raw-bytes: static func(bytes: list<u8>) -> result<raw, string>;"),
        "the raw-bytes bridge lost its `RawBytesEncoding` seam:\n{raw}"
    );
    assert!(
        !raw.contains("cbor-bytes"),
        "the raw-bytes bridge grew a cbor seam, which names a `Serialize` impl its contract does \
         not require:\n{raw}"
    );
    // A generic extern: the INSTANCE is bridged under its own ident; the BASE names no concrete
    // type and is skipped entirely.
    assert!(
        wit.contains("resource extern-generic-ext {") && !wit.contains("resource extern-generic {"),
        "the generic extern's instance/base split changed:\n{wit}"
    );
    // A NON-extern generic instance is monomorphized before generation, so it arrives here as an
    // ordinary record with an ordinary constructor and getter — no bridging involved.
    let generic = body("gen-rule-u64");
    assert!(
        generic.contains("constructor(v: u64);") && generic.contains("v: func() -> u64;"),
        "the monomorphized non-extern generic instance is no longer an ordinary record:\n{generic}"
    );
    // And the containing record still projects: bridging exists precisely so one extern field does
    // not take every type that reaches it out of the WIT.
    let holder = body("holder");
    assert!(
        holder.contains(
            "constructor(e: borrow<ext>, r: borrow<raw>, g: borrow<extern-generic-ext>, \
             n: borrow<gen-rule-u64>);"
        ) && holder.contains("r: func() -> raw;"),
        "the record containing the bridged types no longer projects:\n{holder}"
    );
}

/// The glue behind those bridges. Both seams name a TRAIT on a type the tool does not define, so
/// naming the wrong one is a compile error in the USER's crate — and this fixture cannot enter the
/// build smoke (its rust crate needs hand-written types), so the trait paths are asserted here.
#[test]
fn component_glue_bridges_raw_bytes_through_raw_bytes_encoding() {
    let glue = component_glue("tests/component-extern/inputs", &[]);
    assert!(
        glue.contains(
            "<cddl_lib::Raw as cddl_lib::serialization::RawBytesEncoding>::to_raw_bytes(&self.0.borrow())"
        ) && glue.contains(
            "<cddl_lib::Raw as cddl_lib::serialization::RawBytesEncoding>::from_raw_bytes(&bytes)"
        ),
        "the raw-bytes glue no longer goes through `RawBytesEncoding`:\n{glue}"
    );
    // `to_raw_bytes` hands back a borrow of the type's own storage; the owned copy is this face's
    // job, and without it the guest returns a reference the canonical ABI cannot lift.
    assert!(
        glue.contains("::to_raw_bytes(&self.0.borrow())\n            .to_vec()"),
        "the raw-bytes getter no longer copies the borrowed slice into an owned Vec:\n{glue}"
    );
    assert!(
        !glue.contains("cddl_lib::Raw as cddl_lib::serialization::Deserialize")
            && !glue.contains("cddl_lib::Raw as cddl_lib::serialization::ToCBORBytes"),
        "the raw-bytes glue names a cbor trait its contract does not require:\n{glue}"
    );
    // The extern halves, for contrast: the SAME templates every record uses, over the traits the
    // extern contract does require.
    assert!(
        glue.contains(
            "<cddl_lib::Ext as cddl_lib::serialization::ToCBORBytes>::to_cbor_bytes(&self.0.borrow())"
        ) && glue.contains(
            "<cddl_lib::Ext as cddl_lib::serialization::Deserialize>::from_cbor_bytes(&bytes)"
        ),
        "the extern glue no longer bridges the cbor seam:\n{glue}"
    );
    // A generic extern INSTANCE is named through the `pub type <Instance> = <Base><Args>;` alias the
    // rust crate emits, so the glue never has to spell the argument list itself.
    assert!(
        glue.contains("pub struct WitExternGenericExt(pub RefCell<cddl_lib::ExternGenericExt>);"),
        "the generic extern instance is no longer reached through its rust alias:\n{glue}"
    );
}

// -------------------------------------------------------------------------------------------------
// Value windows at the boundary
// -------------------------------------------------------------------------------------------------

/// A bounded field's SETTER is declared `result<_, string>`, and this is the assertion that it earns
/// that signature. A setter is the one member with no rust constructor standing between the caller
/// and the field, so a setter that merely DECLARED fallibility left this face strictly weaker than
/// the wasm one, which emits the range check at the same site.
///
/// Per shape, because the check expression differs per shape and a single-row test would pass while
/// the others silently did nothing. The two controls matter as much as the six checks: a `[+ T]` and
/// an `@duplicates reject` set enforce their invariant in the TYPE system, so they must re-enter
/// their `TryFrom` door and emit NO inline check.
#[test]
fn component_glue_bounded_setters_check_their_window() {
    let glue = component_glue("tests/component-bounds/input.cddl", &[]);
    let body = |setter: &str| {
        glue.split(&format!("fn {setter}("))
            .nth(1)
            .and_then(|rest| rest.split("\n    }").next())
            .unwrap_or_else(|| panic!("the glue carries no `{setter}`:\n{glue}"))
            .to_owned()
    };
    for (setter, cond) in [
        ("set_lim", "if lim > 5 {"),
        (
            "set_window",
            "if !(window >= 0.5f64 && window <= 10.5f64) {",
        ),
        ("set_digest", "if digest.len() != 4 {"),
        ("set_label", "if label.len() < 3 || label.len() > 14 {"),
        ("set_span", "if span.len() < 2 || span.len() > 5 {"),
        ("set_counts", "if counts.len() > 3 {"),
    ] {
        let body = body(setter);
        assert!(
            body.contains(cond),
            "`{setter}` does not emit its window check `{cond}` — the WIT promises a \
             `result<_, string>` the glue never produces:\n{body}"
        );
        // The failure is lifted through `DeserializeError::from` because `DeserializeFailure`
        // implements no `Display`, and this face reports every failure as a `Display`'s `String`.
        assert!(
            body.contains("cddl_lib::error::DeserializeError::from(")
                && body.contains("cddl_lib::error::DeserializeFailure::"),
            "`{setter}`'s failure is no longer the rust crate's own error type:\n{body}"
        );
    }
    // The check precedes the conversion, and that ordering is forced rather than chosen: a `.len()`
    // read off a `collect()`-bound local is E0282, because the container type is pinned only by the
    // assignment that comes after it.
    let span = body("set_span");
    assert!(
        span.find("if span.len()") < span.find("let span = span.into_iter()"),
        "the window check no longer precedes the conversion:\n{span}"
    );
    // The CONTROLS. A type-enforced invariant is re-imposed by its `TryFrom` door, never by an
    // inline check — the invalid state is unrepresentable rather than rejected.
    for (setter, door) in [
        ("set_ids", "let ids: Vec<_> = ids.into_iter().collect();"),
        ("set_tags", "let tags: Vec<_> = tags.into_iter().collect();"),
    ] {
        let body = body(setter);
        assert!(
            body.contains(door) && body.contains(".try_into().map_err(err)?"),
            "`{setter}` no longer re-enters the despecialized type's `TryFrom` door:\n{body}"
        );
        assert!(
            !body.contains("RangeCheck"),
            "`{setter}` emits an inline window check for an invariant its rust type enforces:\n{body}"
        );
    }
}

/// The OTHER door the same rust type decides, and why it is a TYPE question rather than a "validates
/// and is a list" one. A plain bounded array is a `Vec<T>` on both sides, so routing it through
/// `try_into` reaches the identity `TryFrom` (`Error = Infallible`) — it compiles while checking
/// nothing. A bounded MAP is worse: `BTreeMap<K, V>` has no `TryFrom<Vec<(K, V)>>` at all, so the
/// same conflation emitted glue that did not compile.
#[test]
fn component_glue_routes_only_despecialized_params_through_the_try_from_door() {
    let glue = component_glue("tests/component-bounds/input.cddl", &[]);
    for setter in ["set_span", "set_counts"] {
        let body = glue
            .split(&format!("fn {setter}("))
            .nth(1)
            .and_then(|rest| rest.split("\n    }").next())
            .unwrap_or_else(|| panic!("the glue carries no `{setter}`:\n{glue}"));
        assert!(
            !body.contains("try_into"),
            "`{setter}` still routes a merely-BOUNDED parameter through the despecialization \
             door:\n{body}"
        );
    }
    // A MANDATORY bounded field keeps its window enforced by the rust `new`, whose `Result` the
    // guest unwraps — so the constructor adds neither a door nor a second check.
    let ctor = glue
        .split("fn new(fixed_size: Vec<u64>)")
        .nth(1)
        .and_then(|rest| rest.split("\n    }").next())
        .expect("the glue carries no constructor");
    assert!(
        ctor.contains("let inner = cddl_lib::Bounded::new(fixed_size).map_err(err)?;")
            && !ctor.contains("try_into")
            && !ctor.contains("RangeCheck"),
        "the mandatory bounded field is no longer left to the rust constructor's own check:\n{ctor}"
    );
}

// -------------------------------------------------------------------------------------------------
// The two flag-gated seams: canonical bytes and JSON
// -------------------------------------------------------------------------------------------------

/// The body of one `resource <name> { … }` block of an emitted `.wit`.
fn resource_body(wit: &str, resource: &str) -> String {
    wit.split(&format!("resource {resource} {{"))
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .unwrap_or_else(|| panic!("the WIT carries no `resource {resource}`:\n{wit}"))
        .to_owned()
}

/// `to-canonical-cbor-bytes` exists in exactly ONE flag posture, and the gate is not a style choice:
/// the method is declared on the `Serialize` trait, which the runtime composes only from
/// `serialization_preserve_force_canonical.rs`. Every other posture composes a `ToCBORBytes` that
/// declares `to_cbor_bytes` alone — so a row emitted there names a method the runtime does not have,
/// which is a compile error in the user's crate rather than anything a WIT gate can see.
///
/// Swept across the four resource classes at once because they reach the seam through ONE owner
/// (`bytes_members`); the raw-bytes bridge is the control that proves the owner is not simply
/// unconditional, since its contract is `RawBytesEncoding` and carries no cbor seam at all.
#[test]
fn component_wit_carries_the_canonical_seam_only_where_the_runtime_composes_it() {
    const CANONICAL: &[&str] = &["--preserve-encodings=true", "--canonical-form=true"];
    for (posture, flags) in [
        ("default", &[][..]),
        ("preserve", &["--preserve-encodings=true"][..]),
    ] {
        let wit = wit_of("tests/component-core/input.cddl", flags);
        assert!(
            !wit.contains("to-canonical-cbor-bytes"),
            "the {posture} posture projects `to-canonical-cbor-bytes`, whose method lives on a \
             `Serialize` trait this posture's runtime does not compose:\n{wit}"
        );
    }
    // A record and a `@newtype` wrapper.
    let core = wit_of("tests/component-core/input.cddl", CANONICAL);
    for resource in ["%record", "hash"] {
        let body = resource_body(&core, resource);
        assert!(
            body.contains("to-cbor-bytes: func() -> list<u8>;")
                && body.contains("to-canonical-cbor-bytes: func() -> list<u8>;"),
            "`{resource}` lost a half of the bytes seam in the force-canonical posture:\n{body}"
        );
    }
    // A choice, whose seam comes from the same owner even though it has no constructor.
    let choices = wit_of("tests/component-choices/input.cddl", CANONICAL);
    assert!(
        resource_body(&choices, "outcome").contains("to-canonical-cbor-bytes: func() -> list<u8>;"),
        "a choice resource carries no canonical seam:\n{choices}"
    );
    // An EXTERN bridge does carry it — the extern contract requires `Serialize`, which is exactly
    // the trait the method is declared on — and the RAW-BYTES bridge carries no cbor seam at all.
    let extern_wit = wit_of("tests/component-extern/inputs", CANONICAL);
    assert!(
        resource_body(&extern_wit, "ext").contains("to-canonical-cbor-bytes: func() -> list<u8>;"),
        "the extern bridge lost the canonical half of the seam its contract does require:\n{extern_wit}"
    );
    assert!(
        !resource_body(&extern_wit, "raw").contains("cbor-bytes"),
        "the raw-bytes bridge grew a cbor seam in the canonical posture:\n{extern_wit}"
    );
}

/// The glue behind that row names `Serialize` — the trait the method is DECLARED on — rather than
/// the `to_bytes_trait()` fork. The two agree by construction here (the projection's gate is that
/// same flag pair), and naming the owning trait is what keeps the glue honest if either moves.
#[test]
fn component_glue_bridges_the_canonical_seam_through_serialize() {
    let glue = component_glue(
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true", "--canonical-form=true"],
    );
    assert!(
        glue.contains(
            "<cddl_lib::Hash as cddl_lib::serialization::Serialize>::to_canonical_cbor_bytes("
        ),
        "the canonical seam no longer goes through `Serialize::to_canonical_cbor_bytes`:\n{glue}"
    );
    // And the plain half stays on the same trait in this posture, so the two cannot silently name
    // different traits for methods the runtime declares side by side.
    assert!(
        glue.contains("<cddl_lib::Hash as cddl_lib::serialization::Serialize>::to_cbor_bytes("),
        "the plain bytes half stopped forking to `Serialize` in the force-canonical posture:\n{glue}"
    );
}

/// The JSON seam goes to the types the tool DEFINES and to nothing else.
///
/// The exclusion is the load-bearing half. `bytes_members` is shared with the extern bridge and
/// legitimately so — the extern contract already imposes `Serialize`/`Deserialize`, and the emitted
/// `extern_interface_check.rs` asserts them — but NOTHING imposes serde on a user-owned type, so a
/// `to-json` there would name a trait impl that need not exist. That is the same
/// compile-error-in-generated-code class as the `no_deserialize` fork and the raw-bytes seam split,
/// reached a third time, which is why the JSON seam has its own owner rather than joining that one.
#[test]
fn component_wit_projects_the_json_seam_onto_the_types_the_tool_defines() {
    const JSON: &[&str] = &["--json-serde-derives=true"];
    let off = wit_of("tests/component-core/input.cddl", &[]);
    assert!(
        !off.contains("to-json") && !off.contains("cbor-to-json"),
        "the JSON seam is emitted without `--json-serde-derives`:\n{off}"
    );
    let core = wit_of("tests/component-core/input.cddl", JSON);
    for resource in ["%record", "hash"] {
        let body = resource_body(&core, resource);
        assert!(
            body.contains("to-json: func() -> result<string, string>;")
                && body.contains(&format!(
                    "from-json: static func(json: string) -> result<{resource}, string>;"
                )),
            "`{resource}` is missing a half of the JSON seam:\n{body}"
        );
    }
    // `to-json` is FALLIBLE, deliberately departing from the plan's infallible `string`: the wasm
    // face's own `to_json` is `Result<String, JsError>`, and rendering genuinely can fail (the
    // runtime's `AnyCbor` serde fragment reports "key must be a string" for a non-string-keyed map,
    // which reaches any type holding one).
    assert!(
        !core.contains("to-json: func() -> string;"),
        "`to-json` is declared infallible, so a runtime serde failure has nowhere to go:\n{core}"
    );
    // The two free functions on the `any-cbor` alias — the alias is not a resource, so its JSON door
    // lives at interface level exactly as the `cbor-kind` introspection door does.
    assert!(
        core.contains("cbor-to-json: func(v: any-cbor) -> result<string, string>;")
            && core.contains("cbor-from-json: func(json: string) -> result<any-cbor, string>;"),
        "the `any-cbor` JSON doors are missing or changed shape:\n{core}"
    );
    // A choice carries the seam beside its statics, with no constructor in sight.
    let choices = wit_of("tests/component-choices/input.cddl", JSON);
    assert!(
        resource_body(&choices, "outcome").contains("to-json: func() -> result<string, string>;"),
        "a choice resource carries no JSON seam:\n{choices}"
    );
    // The CONTROLS: neither bridging class gets one, and the monomorphized non-extern generic
    // instance beside them does — so the exclusion is about the CONTRACT, not about bridging.
    let extern_wit = wit_of("tests/component-extern/inputs", JSON);
    for bridge in ["ext", "raw", "extern-generic-ext"] {
        let body = resource_body(&extern_wit, bridge);
        assert!(
            !body.contains("json"),
            "the bridging resource `{bridge}` grew a JSON seam, which names a serde impl its \
             contract does not require:\n{body}"
        );
    }
    assert!(
        resource_body(&extern_wit, "gen-rule-u64").contains("to-json:"),
        "a tool-defined type in the bridging fixture lost its JSON seam, so the exclusions above \
         prove nothing:\n{extern_wit}"
    );
}

/// The glue behind the JSON seam. Every line here is a fact about two crates that the WIT cannot
/// express, and the `&*` is the one that bites: `serde_json`'s parameter is GENERIC, so the
/// auto-deref that lets the cbor seam hand a `Ref<T>` to a `&T` parameter does not apply and a bare
/// `&self.0.borrow()` fails to satisfy `Serialize`.
#[test]
fn component_glue_bridges_json_through_serde_json() {
    let glue = component_glue(
        "tests/component-core/input.cddl",
        &["--json-serde-derives=true"],
    );
    assert!(
        glue.contains("serde_json::to_string_pretty(&*self.0.borrow()).map_err(err)"),
        "`to-json` no longer dereferences the guard for the generic serde call:\n{glue}"
    );
    assert!(
        glue.contains("serde_json::from_str::<cddl_lib::Hash>(&json)")
            && glue.contains("    .map(|v| wit_types::Hash::new(WitHash(RefCell::new(v))))"),
        "`from-json` no longer mints the owned handle from a parsed rust value:\n{glue}"
    );
    // The free doors: bytes in, decode, then render — and back.
    assert!(
        glue.contains(
            "<cddl_lib::any_cbor::AnyCbor as cddl_lib::serialization::Deserialize>::from_cbor_bytes(&v)"
        ) && glue.contains(".and_then(|v| serde_json::to_string_pretty(&v).map_err(err))"),
        "`cbor-to-json` no longer decodes before rendering:\n{glue}"
    );
    assert!(
        glue.contains("serde_json::from_str::<cddl_lib::any_cbor::AnyCbor>(&json)")
            && glue.contains(
                ".map(|v| <cddl_lib::any_cbor::AnyCbor as cddl_lib::serialization::ToCBORBytes>::to_cbor_bytes(&v))"
            ),
        "`cbor-from-json` no longer re-encodes the parsed item:\n{glue}"
    );
}

/// The component manifest declares `serde_json` exactly under the flag whose glue names it, and
/// declares neither of `ops_for_wasm`'s other two JSON deps — the emitted guest names neither crate,
/// and an undeclared-but-unused dependency is noise a consumer inherits.
///
/// Both directions are asserted because the op is set-or-REMOVE: a flag flipped back off must strand
/// nothing.
#[test]
fn the_component_manifest_declares_serde_json_exactly_under_the_json_flag() {
    let manifest = |extra: &[&str]| {
        crate::api::generated_strings(&cli_for("tests/component-core/input.cddl", extra))
            .unwrap()
            .get("component/Cargo.toml")
            .expect("the component tree always carries its manifest")
            .clone()
    };
    let on = manifest(&["--json-serde-derives=true"]);
    assert!(
        on.contains("serde_json = { version = \"1.0.57\", features = [\"float_roundtrip\"] }"),
        "the JSON posture's component manifest does not declare `serde_json` (or moved off the \
         version/features `ops_for_wasm` uses):\n{on}"
    );
    assert!(
        !on.contains("\nserde =") && !on.contains("serde-wasm-bindgen"),
        "the component manifest declares a JSON dep its emitted guest never names:\n{on}"
    );
    let off = manifest(&[]);
    assert!(
        !off.contains("serde_json"),
        "`serde_json` is declared without `--json-serde-derives`:\n{off}"
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

/// INTERFACE level again, on the half a type-only walk misses: an interface's free FUNCTIONS share
/// the same flat namespace its types do, and `wit-parser` refuses the package at RESOLVE. So a rule
/// converging on one of the synthesized `any-cbor` doors has to be reported here rather than reach
/// the user as a parse failure against a file they did not write.
///
/// Asserted on `cbor-to-json` — one of the two doors the JSON seam adds — with the always-present
/// `cbor-kind` beside it, so the check is about the FUNCTION namespace and not about one name.
#[test]
fn wit_interface_function_name_collision_is_rejected() {
    for (rule, wit_name, flags) in [
        ("cbor_kind", "cbor-kind", &[][..]),
        (
            "cbor_to_json",
            "cbor-to-json",
            &["--json-serde-derives=true"][..],
        ),
    ] {
        let dir = scratch_dir("funccollide");
        let path = dir.join("input.cddl");
        std::fs::write(
            &path,
            format!("{rule} = {{ x: uint }}\nholder = {{ m: any }}\n"),
        )
        .unwrap();
        let err = wit_files(path.to_str().unwrap(), flags);
        std::fs::remove_dir_all(&dir).ok();
        let err = err.err().unwrap_or_else(|| {
            panic!("the spec colliding with the free function `{wit_name}` generated")
        });
        assert!(
            err.contains("WIT type name collision under --component:")
                && err.contains(&format!("the free function `{wit_name}`"))
                && err.contains(&format!("all convert to the WIT identifier `{wit_name}`")),
            "the free-function namespace is not part of the interface-level check: {err}"
        );
    }
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
    crate::api::validate_flag_combinations(&plain).unwrap();
}

// -------------------------------------------------------------------------------------------------
// The guest glue's two load-bearing emission invariants
// -------------------------------------------------------------------------------------------------

/// The emitted `component/src/generated/mod.rs` of a spec, via the full generated-file producer (so
/// what is asserted is what `export` writes, never a second path).
fn component_glue(input: &str, extra: &[&str]) -> String {
    let files = crate::api::generated_strings(&cli_for(input, extra))
        .unwrap_or_else(|e| panic!("generating {input} with {extra:?} failed: {e}"));
    files
        .get("component/src/generated/mod.rs")
        .unwrap_or_else(|| {
            panic!(
                "{input} with {extra:?} emitted no component glue:\n{:#?}",
                files.keys()
            )
        })
        .clone()
}

/// RE-ENTRANCY. The canonical ABI lets a caller pass the same handle as both receiver and argument
/// (`x.set-children([x])`), and collection-mediated recursion makes that type-legal for any
/// self-referential CDDL type. Glue holding two `RefCell` guards at once compiles clean in debug AND
/// release and traps only on that call — and a trap poisons the whole component instance, so in a
/// composed topology one aliased call kills a shared dependency for every consumer.
///
/// The mechanical form of the invariant: every argument is bound to an OWNED value by its own `let`
/// (each borrow released at that statement's end) before any `borrow_mut`. Asserted structurally —
/// no `borrow_mut` may appear before the last argument materialization in a body.
#[test]
fn component_glue_never_holds_two_refcell_guards() {
    for (input, flags) in COMPONENT_FIXTURES {
        let glue = component_glue(input, flags);
        for (index, line) in glue.lines().enumerate() {
            // The naive shape: a `borrow_mut` of self in the same expression as a `borrow` of an
            // argument. Two guards live at once is exactly what this looks like textually.
            assert!(
                !(line.contains("borrow_mut()") && line.contains(".get::<Wit")),
                "{input} with {flags:?} line {}: a `borrow_mut` in the same statement as an \
                 argument's `.get::<Wit…>()` holds two RefCell guards at once — materialize the \
                 argument into its own `let` first:\n{line}",
                index + 1
            );
        }
        // And the positive half, so the assertion above can never pass vacuously: the recursive
        // fixture's constructor DOES materialize a list of borrows, in its own statement.
        if *input == "tests/component-core/input.cddl" {
            assert!(
                glue.contains(
                    "let children = children\n            .into_iter()\n            .map(|x| x.get::<WitNode>().0.borrow().clone())\n            .collect();"
                ),
                "the recursive fixture no longer materializes its `list<borrow<node>>` argument \
                 into an owned value — the re-entrancy assertion above has gone vacuous:\n{glue}"
            );
        }
    }
}

/// CLONE-AT-BOUNDARY. A getter mints a FRESH owned handle over a CLONE of the field: a snapshot,
/// never an alias into the parent. An aliasing handle would let a caller mutate a field it never
/// asked for, and would reintroduce the two-guard shape through the back door.
#[test]
fn component_glue_getters_return_a_snapshot_not_an_alias() {
    let glue = component_glue("tests/component-core/input.cddl", &[]);
    // Scalar handle field.
    assert!(
        glue.contains("wit_types::Hash::new(WitHash(RefCell::new(me.digest.clone())))"),
        "the `digest` getter no longer clones the field into a fresh handle:\n{glue}"
    );
    // Collection of handles: every element is cloned into its own fresh handle.
    assert!(
        glue.contains(".map(|x| wit_types::Node::new(WitNode(RefCell::new(x.clone()))))"),
        "the `children` getter no longer clones each element into a fresh handle:\n{glue}"
    );
}

/// A fallible CONSTRUCTOR lowers to `fn new(..) -> Result<Self, E>` — the Ok type is the guest REP
/// type, NOT the owned handle a fallible STATIC returns. One emitter template cannot serve both, and
/// getting it backwards is a type error in generated code rather than anything a WIT gate can see.
#[test]
fn component_glue_distinguishes_a_fallible_constructor_from_a_fallible_static() {
    let glue = component_glue("tests/component-core/input.cddl", &[]);
    assert!(
        glue.contains("fn new(inner: Vec<u8>) -> Result<Self, String> {"),
        "the bounded wrapper's fallible constructor no longer returns the REP type:\n{glue}"
    );
    assert!(
        glue.contains("fn from_cbor_bytes(bytes: Vec<u8>) -> Result<wit_types::Hash, String> {"),
        "the fallible static no longer returns the owned handle:\n{glue}"
    );
}

/// A RECORD's `new` is fallible when a mandatory field carries a value bound, and that verdict is
/// reached by a different rule from the `@newtype` wrapper's (which rides the IR). Glue that
/// consulted only the wrapper rule binds a `Result` where the rep expects the value — a type error
/// in generated code that every WIT gate is blind to.
#[test]
fn component_glue_unwraps_a_fallible_record_new() {
    let glue = component_glue("tests/component-core/input.cddl", &[]);
    assert!(
        glue.contains("let inner = cddl_lib::Record::new(")
            && glue.contains("        .map_err(err)?;"),
        "the value-bounded record's fallible rust `new` is no longer unwrapped:\n{glue}"
    );
}

/// A type the rust face declined to give a `Deserialize` impl carries NO `from-cbor-bytes` — and the
/// WIT and the glue must drop it TOGETHER: a func the world declares but the guest does not
/// implement does not satisfy the world, and glue naming a trait impl that does not exist does not
/// compile. The verdict is reached during GENERATION, not at IR finalization, which is why the
/// projection takes it as an input rather than re-deriving it.
#[test]
fn a_type_with_no_deserialize_impl_carries_no_from_cbor_bytes() {
    // An array struct whose optional field has the same CBOR type as the field after it: a peek
    // cannot tell them apart, so the rust face emits `Serialize` and refuses `Deserialize`.
    const SPEC: &str = "ambiguous = [? b: uint, c: uint]\nplain = [n: text]\n";
    let dir = scratch_dir("nodeser");
    let path = dir.join("input.cddl");
    std::fs::write(&path, SPEC).unwrap();
    let files = crate::api::generated_strings(&cli_for(path.to_str().unwrap(), &[])).unwrap();
    let wit = files["component/wit/world.wit"].clone();
    let glue = files["component/src/generated/mod.rs"].clone();
    let rust = files["rust/src/generated/serialization.rs"].clone();
    std::fs::remove_dir_all(&dir).ok();

    assert!(
        !rust.contains("impl Deserialize for Ambiguous"),
        "the fixture no longer reaches the no-deserialize verdict, so this pin is vacuous:\n{rust}"
    );
    let ambiguous_body = wit
        .split("resource ambiguous {")
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .expect("the WIT must still carry the resource itself");
    assert!(
        ambiguous_body.contains("to-cbor-bytes") && !ambiguous_body.contains("from-cbor-bytes"),
        "the WIT still declares `from-cbor-bytes` for a type with no `Deserialize` impl (or lost \
         the `to-` half too):\n{wit}"
    );
    assert!(
        !glue.contains("cddl_lib::Ambiguous as cddl_lib::serialization::Deserialize"),
        "the glue still names a `Deserialize` impl the rust crate does not emit:\n{glue}"
    );
    // The unaffected type keeps both halves, so the gating is per-type and not a blanket drop.
    assert!(
        wit.contains("from-cbor-bytes")
            && glue.contains("cddl_lib::Plain as cddl_lib::serialization::Deserialize"),
        "the deserializable type lost its bytes seam too — the gate is not per-type:\n{wit}\n{glue}"
    );
}

/// The strong-uniqueness detector consults the REAL no-deserialize verdict, which is why it runs at
/// GENERATION time rather than at IR finalization beside the cycle detector. Projected against an
/// empty no-deserialize set it would see the SUPERSET of members and reject this spec for a
/// collision between a getter and a `from-cbor-bytes` static the tool never emits.
///
/// Both halves matter. The no-deserialize verdict is asserted, or the control is vacuous; and the
/// same field on a DESERIALIZABLE type is still refused, or "consult the verdict" could be satisfied
/// by deleting the check.
#[test]
fn a_no_deserialize_type_may_carry_a_from_cbor_bytes_field() {
    // The same ambiguous-array shape `a_type_with_no_deserialize_impl_carries_no_from_cbor_bytes`
    // uses — a peek cannot tell the optional field from the one after it — plus the field whose name
    // converges on the static.
    let dir = scratch_dir("nodeserfield");
    let path = dir.join("input.cddl");
    std::fs::write(
        &path,
        "ambiguous = [? b: uint, c: uint, from_cbor_bytes: text]\n",
    )
    .unwrap();
    let files = crate::api::generated_strings(&cli_for(path.to_str().unwrap(), &[]))
        .expect("a no-`Deserialize` type carrying a `from_cbor_bytes` field must generate");
    let wit = files["component/wit/world.wit"].clone();
    let rust = files["rust/src/generated/serialization.rs"].clone();
    std::fs::remove_dir_all(&dir).ok();
    assert!(
        !rust.contains("impl Deserialize for Ambiguous"),
        "the fixture no longer reaches the no-deserialize verdict, so this control is vacuous:\n{rust}"
    );
    assert!(
        wit.contains("from-cbor-bytes: func() -> string;")
            && !wit.contains("from-cbor-bytes: static func"),
        "the getter and the never-emitted static are not the two names this control is about:\n{wit}"
    );
    // The positive half: the same field on a DESERIALIZABLE type genuinely collides, and the pinned
    // message is unchanged.
    let err = generate_error("plain = [n: text, from_cbor_bytes: text]\n")
        .expect("a deserializable type carrying a `from_cbor_bytes` field generated");
    assert!(
        err.contains("WIT resource member collision under --component:")
            && err.contains("all convert to the WIT identifier `from-cbor-bytes`"),
        "the real collision is no longer reported — the detector has gone vacuous: {err}"
    );
}

/// Every collision message names `@name` as the remedy, and this is the proof that the remedy works
/// — exercised through the SAME converter and the SAME detector the message points at, rather than
/// asserted in prose.
///
/// Both un-renamed twins are checked too: a fixture that generates proves nothing about a remedy
/// unless the thing it remedies is still refused.
#[test]
fn a_rename_remedies_the_collisions_the_detector_messages_name() {
    let wit = wit_of("tests/component-rename/input.cddl", &[]);
    // RESOURCE level: the field renamed off the resource's own name.
    assert!(
        wit.contains("value: func() -> u64;") && !wit.contains("a: func()"),
        "the `@name`d field's accessor is not the renamed one:\n{wit}"
    );
    // INTERFACE level: the arm struct renamed off the choice's discriminant enum.
    assert!(
        wit.contains("resource pair {") && wit.contains("enum state-kind {"),
        "the `@name`d group-choice arm and the discriminant enum no longer coexist:\n{wit}"
    );
    // The un-renamed twins, still refused with the pinned messages.
    let member = generate_error("a = {\n  a: uint,\n}\n").expect("the un-renamed record generated");
    assert!(
        member.contains("WIT resource member collision under --component:")
            && member.contains("the resource's own name"),
        "the un-renamed field no longer collides, so the remedy above proves nothing: {member}"
    );
    let iface = generate_error(
        "state = uint / text\n\
         holder = [\n\
         \x20 ; @name state_kind\n\
         \x20 tag: 0, x: uint, y: text //\n\
         \x20 ; @name single\n\
         \x20 tag: 1, z: uint, w: text\n\
         ]\n",
    )
    .expect("the un-renamed arm generated");
    assert!(
        iface.contains("WIT type name collision under --component:")
            && iface.contains("all convert to the WIT identifier `state-kind`"),
        "the un-renamed arm no longer collides, so the remedy above proves nothing: {iface}"
    );
}

/// The `int` bridge's rust→WIT direction has to match the ARM SHAPE, which `--preserve-encodings`
/// changes from a tuple to named fields. A `match` written for one posture does not compile under
/// the other, and the first user to combine the flags would get the error in GENERATED code.
#[test]
fn component_glue_matches_the_int_arm_shape_of_the_encoding_posture() {
    let plain = component_glue("tests/component-core/input.cddl", &[]);
    assert!(
        plain.contains("cddl_lib::Int::Uint(value) => wit_types::Int::Uint(*value)"),
        "the default posture's `int` bridge no longer matches the tuple arms:\n{plain}"
    );
    let preserve = component_glue(
        "tests/component-core/input.cddl",
        &["--preserve-encodings=true"],
    );
    assert!(
        preserve.contains("cddl_lib::Int::Uint { value, .. } => wit_types::Int::Uint(*value)"),
        "the preserve posture's `int` bridge no longer matches the NAMED-field arms:\n{preserve}"
    );
    // Both postures go through the posture-INDEPENDENT constructors in the other direction.
    for glue in [&plain, &preserve] {
        assert!(
            glue.contains("=> cddl_lib::Int::new_uint(value)"),
            "the WIT→rust `int` bridge no longer goes through `new_uint`:\n{glue}"
        );
    }
}

/// ONE `struct Component` implements every interface's `Guest` trait under ONE `export!`. A second
/// `export!` would emit a second set of canonical-ABI symbols; a per-interface guest type would not
/// satisfy the world at all.
#[test]
fn component_glue_exports_every_interface_through_one_guest_type() {
    let glue = component_glue("tests/component-multifile/inputs", &[]);
    assert_eq!(
        glue.matches("export!(Component);").count(),
        1,
        "the multi-interface glue no longer has exactly one `export!`:\n{glue}"
    );
    assert!(
        glue.contains("impl wit_types::Guest for Component {")
            && glue.contains("impl wit_sub::Guest for Component {"),
        "both interfaces' `Guest` traits are no longer implemented by the one guest type:\n{glue}"
    );
    // A resource DECLARED in one interface and used from another is one rust type, reached through
    // the alias of the interface that declares it.
    assert!(
        glue.contains("tip: wit_sub::LeafBorrow<'_>"),
        "a cross-interface borrow no longer resolves through the DECLARING interface:\n{glue}"
    );
}

// -------------------------------------------------------------------------------------------------
// Output wiring
// -------------------------------------------------------------------------------------------------

/// The component tree is written as a WHOLE: the WIT package, the glue that implements it, the
/// seed-once crate root and the manifest. A half-tree (the `.wit` without the guest, or the guest
/// without its `wit/`) does not build, and `wit_bindgen::generate!` resolves `path` against
/// `CARGO_MANIFEST_DIR`, so the two have to land in one layout.
#[test]
fn component_generated_files_carry_the_whole_crate() {
    let files =
        crate::api::generated_strings(&cli_for("tests/component-core/input.cddl", &[])).unwrap();
    for expected in [
        "component/wit/world.wit",
        "component/src/generated/mod.rs",
        "component/src/lib.rs",
        "component/Cargo.toml",
    ] {
        assert!(
            files.contains_key(expected),
            "the component tree is missing {expected}:\n{:#?}",
            files.keys()
        );
    }
    // And nothing lands off the flag.
    let off = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        "tests/component-core/input.cddl",
        "--output",
        "component_tests_unused",
    ]))
    .unwrap();
    assert!(
        !off.keys().any(|k| k.starts_with("component/")),
        "a component tree was emitted without --component"
    );
}

/// The `.wit` files carry the provenance banner like every other tool-owned generated file, and it
/// is `//`-comment lines, so a stamped file still resolves. Both halves matter: the stamp gates the
/// `generated_files_start_with_header` sweep, and WIT-legality gates the whole face.
#[test]
fn component_wit_is_header_stamped_and_still_resolves() {
    assert!(crate::generation::export::is_header_stamped_path(
        "component/wit/world.wit"
    ));
    assert!(crate::generation::export::is_header_stamped_path(
        "component/src/generated/mod.rs"
    ));
    // The overlay is a rust-token-stream mechanism, so the WIT is deliberately outside it.
    assert!(!crate::generation::export::is_preservable_generated_path(
        "component/wit/world.wit"
    ));
    assert!(crate::generation::export::is_preservable_generated_path(
        "component/src/generated/mod.rs"
    ));
    let files =
        crate::api::generated_strings(&cli_for("tests/component-core/input.cddl", &[])).unwrap();
    let mut stamped = BTreeMap::new();
    let wit = files["component/wit/world.wit"].clone();
    assert!(
        wit.starts_with("// This file was code-generated"),
        "the emitted WIT is not header-stamped:\n{wit}"
    );
    stamped.insert("component/wit/world.wit".to_owned(), wit);
    let bytes = resolve_and_encode(&stamped).expect("a stamped .wit must still resolve and encode");
    validate_component(&bytes).expect("a stamped .wit must still validate");
}

// -------------------------------------------------------------------------------------------------
// The cross-scope cycle detector, end to end
// -------------------------------------------------------------------------------------------------

/// A named collection is RESOLVED THROUGH by the projection, so it owns no WIT type and can be
/// neither end of a `use` edge. The cycle detector has to agree, in BOTH directions: the reference
/// that runs through the collection points at the ELEMENT's scope, and the collection RULE's own
/// element reference points at nothing.
///
/// The failing direction is the FALSE one. A walk that recorded the collection ident instead sees
/// `b → a` and `a → c` beside the real `c → a`, which closes a cycle in a spec whose emitted WIT
/// resolves perfectly — and refuses it naming scopes the package never links, which the user cannot
/// act on.
#[test]
fn a_named_collection_owns_no_cross_scope_edge() {
    let wit = wit_of("tests/component-collection-refs/inputs", &[]);
    let b = wit
        .split("interface b {")
        .nth(1)
        .and_then(|rest| rest.split("\ninterface ").next())
        .expect("the `b` interface must be emitted");
    assert!(
        b.contains("use c.{leaf};") && !b.contains("use a."),
        "the holder's `use` no longer names the ELEMENT's scope (or has grown one naming the \
         collection's):\n{b}"
    );
    assert!(
        !wit.contains("resource names"),
        "the named collection surfaced as a WIT type:\n{wit}"
    );
}

/// The other side of the same agreement: a cycle that closes THROUGH a named collection is still
/// refused, and the message names only the scopes the emitted `use` graph actually links. Without
/// this, "make the detector agree with the projection" could be satisfied by a detector that sees
/// nothing at all.
#[test]
fn a_real_cycle_through_a_named_collection_is_still_rejected() {
    let dir = scratch_dir("collcycle");
    std::fs::write(dir.join("a.cddl"), "names = [* leaf]\n").unwrap();
    std::fs::write(dir.join("b.cddl"), "rec = { n: names }\n").unwrap();
    std::fs::write(dir.join("c.cddl"), "leaf = { back: rec }\n").unwrap();
    std::fs::write(dir.join("lib.cddl"), "root = { r: rec }\n").unwrap();
    let err = wit_files(dir.to_str().unwrap(), &[])
        .expect_err("a cycle closed through a named collection generated")
        .to_string();
    std::fs::remove_dir_all(&dir).ok();
    assert!(
        err.contains("WIT interface cycle under --component:")
            && err.contains("the scopes `b`, `c` reference each other")
            && !err.contains("`a`"),
        "the cycle through the collection is missed, or the message names the collection's scope — \
         which the emitted `use` graph never links: {err}"
    );
}

/// A spec whose SCOPES reference each other generates fine on the rust face and is rejected under
/// `--component`. The cycle here is interface-level and NOT type-level, which is the case a
/// type-level detector would miss.
#[test]
fn a_cross_scope_cycle_is_rejected_under_component() {
    let err = wit_files("tests/component-cycle/inputs", &[])
        .expect_err("the mutually-referencing scopes generated a WIT package");
    assert!(
        err.contains("WIT interface cycle under --component:")
            && err.contains("Move a type so the scopes are acyclic"),
        "unexpected cycle message: {err}"
    );
    // The same spec is fine on the rust face — which is what makes this a `--component`-only rule.
    let plain = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "tests/component-cycle/inputs",
        "--output",
        "component_tests_unused",
    ]);
    crate::api::generated_strings(&plain).unwrap();
}

// -------------------------------------------------------------------------------------------------
// The `--emit-tests` loud skip
// -------------------------------------------------------------------------------------------------

/// The component face has no generated-test renderer yet, and a `--emit-tests --component` run that
/// silently emitted nothing would read as a passing test surface that does not exist. Pinned on the
/// same terms as the wasm module's own loud skip: the MESSAGE TEXT is the contract, and it is
/// asserted against a real run's stderr rather than against the source string, so the pin covers
/// both the wording and the fact that it actually reaches the user.
#[test]
fn emit_tests_with_component_skips_loudly() {
    let dir = scratch_dir("emittests");
    let out = crate::tests::integration_tests::codegen_cmd()
        .args([
            "--input",
            "tests/component-core/input.cddl",
            "--output",
            dir.to_str().unwrap(),
            "--component=true",
            "--emit-tests=true",
        ])
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).ok();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(
            "cddl-codegen --emit-tests: component module skipped (component test emission not yet \
             supported)"
        ),
        "the pinned `--emit-tests --component` loud skip did not reach stderr:\n{stderr}"
    );
}

// -------------------------------------------------------------------------------------------------
// The wasip2 build smoke
// -------------------------------------------------------------------------------------------------

/// The fixtures the build smoke compiles, and why each earns its nested-cargo cost. Deliberately
/// smaller than [`COMPONENT_FIXTURES`]: the WIT gates above are cheap and sweep everything, while
/// this one pays a real link per row.
const BUILD_SMOKE_FIXTURES: &[(&str, &[&str])] = &[
    // Every phase-1 type-mapping row in one scope, in the posture the emitters target. Two of those
    // rows are here for a reason no WIT gate can express, because both are TYPE facts about the
    // generated rust crate: the NonEmpty TABLE (`counts`) makes the guest constructor re-enter the
    // runtime's vec-of-pairs `TryFrom` door, and the value-bounded field (`limit`) makes the rust
    // `Record::new` itself fallible, so the glue must unwrap it rather than wrap it.
    ("tests/component-core/input.cddl", &[]),
    // CHOICES: the largest new glue surface phase 2 adds, and the one no WIT gate can judge. A
    // `kind` / `as-<variant>` arm that does not match the rust enum's ARM SHAPE, or a `new-<variant>`
    // that wraps a `Result` the rust ctor already returns, is a type error in generated code that
    // resolves, encodes and validates perfectly as WIT.
    ("tests/component-choices/input.cddl", &[]),
    // The multi-INTERFACE shape: two `Guest` impls on one guest type under one `export!`, a
    // cross-interface `borrow` parameter, and an `own` handle minted for a resource another
    // interface declares. None of it is reachable from a single-scope fixture, and all of it is a
    // link-time fact no WIT gate can see.
    ("tests/component-multifile/inputs", &[]),
    // VALUE WINDOWS: every row here is a fact about the generated rust crate that the WIT cannot
    // express — a bounded setter's `result<_, string>` reads the same whether the check is emitted
    // or not, and the two despecialization controls decide between a `TryFrom` door and an inline
    // check whose wrong choice is either a silent no-op or a trait impl that does not exist.
    // `tests/component-extern/inputs` is deliberately NOT here: its rust crate names user-owned
    // extern types, so it cannot compile standalone (the same reason `tests/multifile` is absent);
    // its emitted bytes are pinned by the `component_extern` whole-program snapshot instead.
    ("tests/component-bounds/input.cddl", &[]),
    // The two flag-gated SEAMS, in the one posture that carries both: `to-canonical-cbor-bytes`
    // (which names a trait method the runtime composes only here) and the JSON pair (which names
    // `serde_json` and the rust crate's derived serde impls, and needs the dependency the component
    // manifest adds under the same flag). Every one of those is a fact about the two crates that the
    // WIT cannot express — a resource declaring `to-json` reads identically whether the method it
    // bridges exists or not.
    (
        "tests/component-core/input.cddl",
        &[
            "--preserve-encodings=true",
            "--canonical-form=true",
            "--json-serde-derives=true",
        ],
    ),
];

/// THE acceptance gate for the guest emitters: a generated component crate that does not compile is
/// the failure mode the whole face exists to prevent, and every other gate here is blind to it — the
/// WIT can resolve, encode and validate perfectly while the glue implementing it names a trait method
/// that does not exist.
///
/// Nested cargo, so it is memoized per generated-crate content hash by the gate cache; an unchanged
/// tree re-runs as a visible cached PASS. `GATE_CACHE=0` forces the build.
#[test]
fn component_crate_builds_for_wasm32_wasip2() {
    let scratch = std::env::temp_dir().join(format!(
        "cddl_codegen_component_wasip2_{}",
        std::process::id()
    ));
    let target_dir = scratch.join("target");
    let mut failures = Vec::new();
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;

    for (input, flags) in BUILD_SMOKE_FIXTURES {
        let label = format!("{input} {flags:?}");
        let out = scratch.join(format!(
            "{:x}",
            input
                .bytes()
                .chain(flags.iter().flat_map(|f| f.bytes()))
                .fold(0xcbf2_9ce4_8422_2325u64, |h, b| (h ^ b as u64)
                    .wrapping_mul(0x0000_0100_0000_01b3))
        ));
        std::fs::create_dir_all(&out).unwrap();
        let mut args = vec![
            "--input".to_owned(),
            (*input).to_owned(),
            "--output".to_owned(),
            out.to_str().unwrap().to_owned(),
            "--component=true".to_owned(),
            // The rust crate is the component crate's path dependency and nothing else here; the
            // wasm face would only add `__wbindgen_*` imports componentization cannot resolve.
            "--wasm=false".to_owned(),
        ];
        args.extend(flags.iter().map(|f| (*f).to_owned()));
        let generated = crate::tests::integration_tests::codegen_cmd()
            .args(&args)
            .output()
            .unwrap();
        assert!(
            generated.status.success(),
            "{label}: generation failed\n{}",
            String::from_utf8_lossy(&generated.stderr)
        );
        // A workspace root so the two emitted crates share one lock and one target dir. Real
        // consumers own this file; the tool never writes one.
        std::fs::write(
            out.join("Cargo.toml"),
            "[workspace]\nresolver = \"3\"\nmembers = [\"rust\", \"component\"]\n",
        )
        .unwrap();
        // The rust crate's `cdylib` output exists for wasm-bindgen's `wasm32-unknown-unknown`
        // target; asking the wasip2 linker for it is not something the component face needs (the
        // guest consumes the rlib) and `wasm-component-ld` crashes on it for some specs. Narrowing
        // the dependency to `rlib` HERE keeps the gate's verdict about the component crate, which is
        // the thing under test.
        let rust_manifest = out.join("rust/Cargo.toml");
        let narrowed = std::fs::read_to_string(&rust_manifest).unwrap().replace(
            "crate-type = [\"cdylib\", \"rlib\"]",
            "crate-type = [\"rlib\"]",
        );
        std::fs::write(&rust_manifest, narrowed).unwrap();

        let component_dir = out.join("component");
        let outcome = gate_cache::run_cached(
            "component_wasip2_build",
            &label,
            &out,
            &[
                std::path::PathBuf::from("component/Cargo.toml"),
                std::path::PathBuf::from("rust/Cargo.toml"),
            ],
            &[
                "cwd=component".to_owned(),
                "cargo".to_owned(),
                "build".to_owned(),
                "--target".to_owned(),
                "wasm32-wasip2".to_owned(),
            ],
            || {
                let build = crate::tests::integration_tests::tool_cmd("cargo")
                    .args(["build", "--target", "wasm32-wasip2"])
                    .current_dir(&component_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                if !build.status.success() {
                    let stderr = String::from_utf8_lossy(&build.stderr);
                    // The target is declared in `rust-toolchain.toml`, so a rustup-managed checkout
                    // has it; anywhere else this is a provisioning problem, not a code failure, and
                    // it has to say so rather than read as a broken emitter.
                    if stderr.contains("can't find crate for `core`")
                        || stderr.contains("target may not be installed")
                    {
                        failures.push(format!(
                            "{label}: the wasm32-wasip2 target is not installed under the pinned \
                             toolchain — `rustup target add wasm32-wasip2`"
                        ));
                    } else {
                        failures.push(format!("{label}: cargo build failed\n{stderr}"));
                    }
                    return false;
                }
                // A build that produced no COMPONENT would be a vacuous pass: `wasm32-wasip2`
                // artifacts carry the component-model preamble (layer 1), where a core module
                // carries layer 0.
                let artifact = target_dir
                    .join("wasm32-wasip2/debug")
                    .join("cddl_lib_component.wasm");
                match std::fs::read(&artifact) {
                    Ok(bytes) if bytes.starts_with(b"\0asm\x0d\0\x01\0") => true,
                    Ok(bytes) => {
                        failures.push(format!(
                            "{label}: {} is not a component-model binary (preamble {:02x?})",
                            artifact.display(),
                            &bytes[..8.min(bytes.len())]
                        ));
                        false
                    }
                    Err(e) => {
                        failures.push(format!(
                            "{label}: the build reported success but wrote no artifact at {}: {e}",
                            artifact.display()
                        ));
                        false
                    }
                }
            },
        );
        cache_run += outcome.ran();
        cache_hit += outcome.cached();
    }

    if gate_cache::enabled() {
        println!("component_wasip2_build gate-cache: {cache_run} run, {cache_hit} cached");
    }
    let verdict = failures.is_empty();
    std::fs::remove_dir_all(&scratch).ok();
    assert!(
        verdict,
        "the generated component crate does not build for wasm32-wasip2:\n\n{}",
        failures.join("\n\n")
    );
}
