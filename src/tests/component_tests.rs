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
    ("tests/component-multifile/inputs", &[]),
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
    // The multi-INTERFACE shape: two `Guest` impls on one guest type under one `export!`, a
    // cross-interface `borrow` parameter, and an `own` handle minted for a resource another
    // interface declares. None of it is reachable from a single-scope fixture, and all of it is a
    // link-time fact no WIT gate can see.
    ("tests/component-multifile/inputs", &[]),
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
