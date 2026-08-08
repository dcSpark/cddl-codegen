//! The component face's own gates: the WIT projection's VALIDITY (four stages, in-process), its
//! independence from the wasm posture, the exclude-and-record contract, and the pinned messages of
//! the strong-uniqueness detector.
//!
//! The validity gate's oracle is the pinned `wit-parser` / `wit-component` / `wasmparser` trio, at
//! the `=0.247.0` toolchain floor — never a shelled-out `wasm-tools` binary, whose ambient version
//! (1.231-era) rejects the fallible constructors this face emits for every bounds-validating type.

use crate::cli::Cli;
use crate::comment_ast::DuplicatesPolicy;
use crate::intermediate::RustStructType;
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
    // The BEHAVIORAL fixture. It is swept here for the two things the host gate cannot say cheaply:
    // its WIT must resolve/encode/validate like every other fixture's, and its glue must satisfy the
    // never-two-guards invariant STRUCTURALLY — which is the same claim
    // `component_host_tests::component_host_behavior` makes at runtime, from the other side. A
    // fixture whose runtime behaviour is asserted and whose emitted shape is not would be the one
    // place a regression could land with no static gate noticing.
    ("tests/component-host/inputs", &[]),
    ("tests/component-multifile/inputs", &[]),
    // Cross-scope references that run THROUGH a named collection: the projection resolves the
    // collection through, so the cycle detector must agree about which scope the `use` points at.
    ("tests/component-collection-refs/inputs", &[]),
    // The `@name` remedy every collision message names, applied to the two collision classes a
    // rename can actually move.
    ("tests/component-rename/input.cddl", &[]),
    // CDDL `any` reached from a member position through a transparent alias CHAIN, beside the
    // direct spelling as its in-fixture control: the class where the projection's type walk and its
    // FALLIBILITY walk can disagree, producing valid WIT whose glue does not compile.
    ("tests/component-any-alias/input.cddl", &[]),
    // The WIT ident hazard: a rule whose resource name is exactly `t`, which `wit_bindgen`'s macro
    // cannot expand. Swept here for the half no build gate states — that the refusal is an
    // exclusion RECORD in the WIT rather than a missing type, and that the interface survives it.
    ("tests/component-ident-hazard/input.cddl", &[]),
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

/// The `component/wit/**` map of a spec written to a scratch file — what a caller needs when the
/// claim is about the FILES (resolving them as a package) rather than about their text.
fn wit_files_for_spec(spec: &str, extra: &[&str]) -> BTreeMap<String, String> {
    let dir = scratch_dir("spec");
    let path = dir.join("input.cddl");
    std::fs::write(&path, spec).unwrap();
    let out = wit_files(path.to_str().unwrap(), extra);
    std::fs::remove_dir_all(&dir).ok();
    out.unwrap_or_else(|e| panic!("generating the spec failed: {e}"))
}

/// The single emitted `.wit` of a spec written to a scratch file. Panics on a generation failure —
/// callers testing REJECTION use [`generate_error`].
fn wit_for_spec(spec: &str, extra: &[&str]) -> String {
    wit_files_for_spec(spec, extra)
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
pub(super) fn resolve_and_encode(files: &BTreeMap<String, String>) -> Result<Vec<u8>, String> {
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
pub(super) fn validate_component(bytes: &[u8]) -> Result<(), String> {
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

/// The `ALL_PROFILES` component row's flags, as [`cli_for`] needs them. Sourcing them from the row
/// rather than respelling them keeps this sweep and the corpus SNAPSHOT axis on one posture by
/// construction: change the row and both move together.
///
/// The row's own `--component=true` is dropped, because `cli_for` already supplies it and clap
/// REJECTS a repeated `--component` outright ("cannot be used multiple times") — `ArgAction::Set` is
/// set-once, not last-wins. Any other flag the row grows passes straight through.
fn component_profile_flags() -> Vec<&'static str> {
    let flags = crate::tests::ALL_PROFILES
        .iter()
        .find(|(profile, _)| *profile == crate::tests::COMPONENT_PROFILE)
        .map(|(_, flags)| *flags)
        .expect("COMPONENT_PROFILE must name a live ALL_PROFILES row");
    assert!(
        flags.contains(&"--component=true"),
        "the ALL_PROFILES component row no longer turns `--component` on: {flags:?}"
    );
    flags
        .iter()
        .copied()
        .filter(|flag| !flag.starts_with("--component"))
        .collect()
}

/// Corpus-breadth companion to [`component_wit_validates`]: every `tests/corpus/*.cddl` fixture
/// through the SAME four stages (resolve → `wit_component::encode` → `wasmparser` validate), under
/// the `ALL_PROFILES` component row's flags.
///
/// This is what makes the corpus' component profile mean "the corpus is component-CLEAN" rather
/// than "the corpus' component bytes are pinned", and it carries that breadth ALONE:
/// `integration_tests::feature_corpus_compiles` filters the component profile out (the component
/// crate is a wasip2 target and that gate `cargo check`s for the host), so nothing else looks at
/// this face across the whole corpus. In-process and cheap — no cargo, no scratch crate.
///
/// It deliberately does NOT assert a non-empty WORLD. Three corpus fixtures — `int_alias`, `table`
/// and `cbor_nonempty_payload` — project a world with no interface at all (every rule resolves
/// through as an alias or a named collection), and that is CORRECT output: it resolves, encodes and
/// validates. An added "the world must export something" assertion would reject them; do not add
/// one. The vacuity guard that IS wanted here is the emitted-`world.wit` check per fixture plus the
/// corpus-size floor below.
///
/// A fixture that cannot GENERATE under this profile belongs in
/// `snapshot_tests::PROFILE_GENERATION_SKIP` as `(stem, COMPONENT_PROFILE)` with a reason — the
/// same list the snapshot axis reads, so the two can never disagree about which cells exist, and
/// `snapshot_tests::feature_corpus_pins_are_live` already reconciles it against the corpus. A
/// fixture that GENERATES but fails validity is a BUG, never a pin.
#[test]
fn component_wit_validates_the_corpus() {
    let flags = component_profile_flags();
    let mut entries: Vec<PathBuf> = std::fs::read_dir("tests/corpus")
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    let mut swept = 0usize;
    for path in &entries {
        let stem = path.file_stem().unwrap().to_str().unwrap();
        if crate::tests::snapshot_tests::PROFILE_GENERATION_SKIP
            .contains(&(stem, crate::tests::COMPONENT_PROFILE))
        {
            continue;
        }
        let input = path.to_str().unwrap();
        let files = wit_files(input, &flags)
            .unwrap_or_else(|e| panic!("generating corpus fixture {stem} failed: {e}"));
        assert!(
            files.contains_key(&format!(
                "{}/world.wit",
                crate::generation::layout::COMPONENT_WIT_DIR
            )),
            "corpus fixture {stem} emitted no world.wit — its cell would have been vacuous"
        );
        let bytes = resolve_and_encode(&files)
            .unwrap_or_else(|e| panic!("corpus fixture {stem}: {e}\n{files:#?}"));
        validate_component(&bytes)
            .unwrap_or_else(|e| panic!("corpus fixture {stem}: {e}\n{files:#?}"));
        swept += 1;
    }
    // Corpus-size floor: a filter bug that swept nothing (or nearly nothing) would otherwise pass
    // silently. The corpus only grows, so this floor is a lower bound, not a pin.
    assert!(
        swept >= 80,
        "only {swept} corpus fixtures were swept for component-WIT validity (expected >= 80) — the \
         corpus enumeration or the skip filter shrank"
    );
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

/// A WIT `resource` named exactly `t` is refused — through the projection's own exclusion channel,
/// not through an abort.
///
/// The WIT is valid; what cannot survive is `wit_bindgen::generate!`, which binds its own type
/// parameter `T` unhygienically in the scope it expands the guest bindings into, so the resource's
/// rust name `T` resolves to that parameter (E0599 on the pinned 0.57.1). It is the same
/// "valid WIT the guest macro cannot expand" family as the `list<borrow<imported>>` E0506 class, and
/// it takes the same answer: exclude and record, so the rest of the spec's component face still
/// generates AND still builds, and the exclusion lifts by itself when upstream fixes the hygiene.
///
/// The reason is pinned by fragment rather than verbatim because it is a user-facing diagnosis: the
/// rule it names, the CAUSE (so the reader knows it is not their spec's shape), and the REMEDY.
#[test]
fn a_wit_resource_named_t_is_unexported_with_the_wit_bindgen_reason() {
    let wit = wit_for_spec(
        "t = [uint, tstr]\nholder = [0, t]\nsibling = [n: uint]\n",
        &[],
    );
    assert!(
        !wit.contains("resource t {"),
        "the hazardous resource is still exported:\n{wit}"
    );
    let row = wit
        .lines()
        .find(|l| l.trim_start().starts_with("// unexported: T —"))
        .unwrap_or_else(|| {
            panic!("the hazardous resource vanished with no exclusion record:\n{wit}")
        })
        .to_owned();
    for fragment in [
        // The rule, so the reader knows WHICH name to change.
        "`T`",
        // The cause, so it reads as an upstream toolchain fact rather than a defect in their spec.
        "wit_bindgen",
        "type parameter `T`",
        // The remedy. `@name` does NOT rename a top-level rule (the parser refuses it outright), so
        // the message names what actually works: renaming the identifier.
        "rename the identifier",
        "@name",
        // The reassurance that makes the remedy cheap to take.
        "wire format",
    ] {
        assert!(
            row.contains(fragment),
            "the exclusion reason does not name {fragment:?}:\n{row}"
        );
    }
    assert!(
        wit.contains("// unexported: Holder — references excluded T"),
        "the reference closure did not carry the refusal to a type that names the hazard, or does \
         not name the chain root:\n{wit}"
    );
    assert!(
        wit.contains("resource sibling {"),
        "an unrelated type was dropped along with the hazardous one — the refusal is scoped to the \
         name and what reaches it, never to the interface:\n{wit}"
    );
}

/// The remedy the message names, applied. Same spec, one identifier renamed: the full surface
/// projects and nothing is excluded — which is what makes the refusal above a NAME refusal rather
/// than a shape refusal.
#[test]
fn renaming_the_rule_restores_the_resource_the_t_hazard_unexports() {
    let wit = wit_for_spec(
        "tee = [uint, tstr]\nholder = [0, tee]\nsibling = [n: uint]\n",
        &[],
    );
    assert!(
        !wit.contains("unexported"),
        "the renamed twin still excludes something — the refusal is not keyed on the name alone:\n\
         {wit}"
    );
    for resource in ["tee", "holder", "sibling"] {
        assert!(
            wit.contains(&format!("resource {resource} {{")),
            "the renamed twin lost `{resource}`:\n{wit}"
        );
    }
}

/// The refusal is RESOURCE-only, pinned by the three ways a rule can be named `t` and not mint one.
///
/// Each control is a shape that reaches the projection differently, and every one of them compiles
/// clean today — so widening the predicate to "any WIT item named `t`" would cost surface for
/// nothing.
#[test]
fn the_t_refusal_leaves_every_shape_that_mints_no_resource_alone() {
    // A c-style enum is a plain WIT `enum`: a value type the guest bridges by matching, with no rust
    // type of the resource kind for the macro's parameter to shadow.
    let enum_wit = wit_for_spec("t = 1 / 2\nholder = [0, t]\n", &[]);
    assert!(
        enum_wit.contains("enum t {") && !enum_wit.contains("unexported"),
        "a c-style enum named `t` was refused, or stopped projecting:\n{enum_wit}"
    );
    // A transparent alias mints NO WIT type at all — it is resolved through at the use site, so
    // `holder`'s constructor takes the resolved `u64` and nothing claims the name.
    let alias_wit = wit_for_spec("t = uint\nholder = [0, t]\n", &[]);
    assert!(
        !alias_wit.contains("unexported") && alias_wit.contains("constructor(t: u64)"),
        "a transparent alias named `t` was refused, or stopped being resolved through:\n{alias_wit}"
    );
    // A named COLLECTION takes the same resolved-through route, and this is the one committed input
    // that already carries a rule named `t` — so the refusal must be invisible to it.
    let collection_wit = wit_of("tests/corpus/composite_map_key.cddl", &[]);
    assert!(
        !collection_wit.contains("unexported") && collection_wit.contains("resource holder {"),
        "the committed `t = {{ * [+ uint] => uint }}` input started excluding something:\n\
         {collection_wit}"
    );
}

/// The degenerate shape of the refusal: a spec whose ONLY rule is the hazard leaves an interface
/// carrying nothing but the exclusion record. That is still a package `wit-parser` resolves,
/// `wit-component` encodes and `wasmparser` validates — the same four-stage oracle every fixture
/// meets — so the refusal never turns a generable spec into an unusable one.
#[test]
fn a_spec_whose_only_rule_is_the_hazard_still_renders_a_valid_package() {
    let files = wit_files_for_spec("t = [uint, tstr]\n", &[]);
    let joined = files.values().cloned().collect::<Vec<_>>().join("\n");
    // A DECLARATION, not a mention: the exclusion reason names the resource kind in its prose, so
    // the absence claim reads the line's own shape rather than the word.
    let declares_a_resource = joined
        .lines()
        .any(|l| l.trim_start().starts_with("resource "));
    assert!(
        joined.contains("// unexported: T —") && !declares_a_resource,
        "the lone hazardous rule did not leave an empty-but-recorded interface:\n{joined}"
    );
    let bytes = resolve_and_encode(&files).unwrap_or_else(|e| {
        panic!(
            "an interface holding only an exclusion record failed to \
             resolve/encode: {e}\n{joined}"
        )
    });
    validate_component(&bytes)
        .unwrap_or_else(|e| panic!("the encoded component failed validation: {e}\n{joined}"));
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
/// naming the wrong one is a compile error in the USER's crate. The build smoke does compile this
/// fixture (paired with the hand-written definitions a real consumer supplies), and this test still
/// earns its place beside it: it names the exact trait PATHS at in-process cost, so a regression
/// reports which seam moved instead of a nested-cargo type error several minutes later.
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

/// `@duplicates reject` is an explicit, accepted no-op for loose tables: a `BTreeMap` is already
/// key-unique. The component projection still lowers both named and inline tables through
/// `list<tuple<K, V>>`, but neither table has an `OrderedSet`-style `TryFrom` invariant to restore.
/// The inline IR assertion matters independently: output-only coverage would let the parser silently
/// drop the explicit directive again and pass by accident.
#[test]
fn component_reject_tables_stay_plain_maps_while_reject_sets_reenter_try_from() {
    const CDDL: &str = "named = { * uint => text } ; @duplicates reject\n\
                        unique = [* uint] ; @duplicates reject\n\
                        holder = [named: named, inline: { * uint => text ; @duplicates reject\n\
                                  }]\n\
                        set_holder = [unique: unique]\n";

    let glue = component_glue_for_spec(CDDL, &[]);
    let constructor = glue
        .split("impl wit_types::GuestHolder for WitHolder {")
        .nth(1)
        .and_then(|rest| rest.split("    fn named(").next())
        .unwrap_or_else(|| panic!("the component glue carries no holder constructor:\n{glue}"));
    for table in ["named", "inline"] {
        assert!(
            constructor.contains(&format!("let {table} = {table}.into_iter().collect();")),
            "the reject table `{table}` no longer materializes directly as a map:\n{constructor}"
        );
    }
    assert!(
        !constructor.contains("named.try_into") && !constructor.contains("inline.try_into"),
        "a plain reject table still routes through a nonexistent TryFrom door:\n{constructor}"
    );
    assert!(
        constructor.contains(") -> Self {") && !constructor.contains(") -> Result<Self, String> {"),
        "reject tables alone made the holder constructor fallible even though maps carry no dropped \
         uniqueness invariant:\n{constructor}"
    );
    let reject_set_constructor = glue
        .split("impl wit_types::GuestSetHolder for WitSetHolder {")
        .nth(1)
        .and_then(|rest| rest.split("    fn unique(").next())
        .unwrap_or_else(|| {
            panic!("the component glue carries no reject-set holder constructor:\n{glue}")
        });
    assert!(
        reject_set_constructor.contains("unique.try_into().map_err(err)?")
            && reject_set_constructor.contains(") -> Result<Self, String> {"),
        "the reject-set control no longer re-enters OrderedSet's fallible TryFrom door:\n\
         {reject_set_constructor}"
    );

    let dir = scratch_dir("reject-table-ir");
    let input = dir.join("input.cddl");
    std::fs::write(&input, CDDL).unwrap();
    let cli = cli_for(input.to_str().unwrap(), &[]);
    let inline_policy = crate::api::with_types(&cli, |types, _| {
        let holder = types
            .rust_structs()
            .values()
            .find(|rust_struct| rust_struct.ident().to_string() == "Holder")
            .expect("the holder record was not registered");
        let RustStructType::Record(record) = holder.variant() else {
            panic!("holder did not lower to a record: {holder:?}");
        };
        record
            .fields
            .iter()
            .find(|field| field.name == "inline")
            .expect("the inline table field was not retained")
            .rust_type
            .config
            .duplicates
    })
    .unwrap();
    std::fs::remove_dir_all(&dir).ok();
    assert_eq!(
        inline_policy,
        Some(DuplicatesPolicy::Reject),
        "the inline table parser silently dropped its explicit @duplicates reject policy"
    );
}

/// CDDL `any` reached through a transparent alias must take the SAME fallible door as `any` written
/// directly.
///
/// The projection runs two walks over one parameter — the TYPE walk that decides how it is spelled
/// and the FALLIBILITY walk that decides whether its door returns a `result` — and the module's
/// contract is that they agree. The type walk resolves a CDDL alias at the use site (`x = any` is
/// `any-cbor` there, indistinguishable from a directly-written `any`), so a fallibility walk that
/// stops AT the alias produces an infallible `constructor` over a body that decodes the caller's
/// bytes with `?` — valid WIT, exit 0, and E0277 in the guest crate. The chain is two links deep so a
/// walk resolving exactly one is caught here too, and the directly-spelled member is the control that
/// keeps this an AGREEMENT claim rather than a claim about `any` doors in general.
#[test]
fn a_door_taking_any_through_an_alias_is_fallible_like_the_direct_spelling() {
    let wit = wit_for_spec(
        "shallow_any = any\n\
         deep_any = shallow_any\n\
         aliased = [chained: deep_any, single: shallow_any]\n\
         direct = [d: any]\n",
        &[],
    );
    let control = resource_body(&wit, "direct");
    assert!(
        control.contains("constructor(d: any-cbor) -> result<direct, string>;"),
        "the CONTROL door — `any` written directly — is not fallible, so this test can prove \
         nothing about the aliased one:\n{wit}"
    );
    let aliased = resource_body(&wit, "aliased");
    assert!(
        aliased.contains(
            "constructor(chained: any-cbor, single: any-cbor) -> result<aliased, string>;"
        ),
        "a door taking `any` through an alias is INFALLIBLE while the direct spelling is fallible — \
         the fallibility walk stopped at the alias, and the guest body decoding those bytes with \
         `?` will not compile:\n{wit}"
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
            && err.contains("Rename one of the colliding rules in the CDDL spec itself"),
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

/// The emitted glue of a SPEC BODY written to a scratch file, for shapes no committed fixture
/// carries.
fn component_glue_for_spec(spec: &str, extra: &[&str]) -> String {
    component_glue_for_scopes(&[("input.cddl", spec)], extra)
}

/// The emitted glue of one or more spec bodies. A single `input.cddl` is passed as a FILE; two or
/// more are written into a scratch directory rooted at `lib.cddl` and passed as a DIRECTORY, which
/// is how a spec reaches more than one module scope — i.e. more than one WIT interface.
fn component_glue_for_scopes(files: &[(&str, &str)], extra: &[&str]) -> String {
    let dir = scratch_dir("glue-spec");
    for (name, body) in files {
        std::fs::write(dir.join(name), body).unwrap();
    }
    let input = if files.len() == 1 {
        dir.join(files[0].0)
    } else {
        dir.clone()
    };
    let generated = crate::api::generated_strings(&cli_for(input.to_str().unwrap(), extra));
    std::fs::remove_dir_all(&dir).ok();
    generated
        .unwrap_or_else(|e| panic!("generating {files:?} with {extra:?} failed: {e}"))
        .get("component/src/generated/mod.rs")
        .unwrap_or_else(|| panic!("a `--component` run emitted no glue for {files:?}"))
        .clone()
}

/// Every token of the guest block, which stands or falls together.
const GUEST_BLOCK: &[&str] = &[
    "struct Component;",
    "Guest for Component",
    "export!(Component);",
    "fn err<",
];

/// The guest block is emitted only where `wit_bindgen::generate!` mints something for it to name,
/// and suppressing it is what COMPILES rather than a tidiness choice.
///
/// Two ordinary CDDL shapes reach that, and both were corpus-wide wasip2 compile failures before the
/// emitter asked the question. A spec whose every rule RESOLVES THROUGH — a plain alias, a named
/// collection — projects no interface at all, so the world exports nothing and `generate!` mints no
/// `export!` macro for `export!(Component);` to invoke (`cannot find macro export in this scope`).
/// A spec whose only projected type is a VALUE type — a c-style enum — projects an interface of
/// pure type declarations, which `generate!` gives a module and no `Guest` trait, so
/// `impl <iface>::Guest for Component {}` names something that does not exist (E0405).
///
/// What is left once the impls are gone has no possible caller — the guest type, its `export!`, the
/// `err` funnel, the bridges and the interface `use` aliases they are spelled against all exist to
/// serve a `Guest` impl — so the whole block goes, and the emitted file is the `generate!`
/// invocation alone. The WIT surface is untouched by that: a world's exports live in the component
/// type section `generate!` emits whether or not `export!` is invoked.
#[test]
fn component_glue_emits_the_guest_block_only_where_generate_mints_one() {
    let resolved_through = component_glue_for_spec("bare_int = int\nt = { * uint => text }\n", &[]);
    let value_only = component_glue_for_spec("fixed_enum = 0 / 1 / 2\n", &[]);
    for (shape, glue) in [
        ("a world that exports no interface", &resolved_through),
        ("an interface of only value types", &value_only),
    ] {
        for token in GUEST_BLOCK {
            assert!(
                !glue.contains(token),
                "{shape} emitted `{token}`, which `wit_bindgen::generate!` mints nothing for:\n\
                 {glue}"
            );
        }
    }
    // The control: ONE resource is enough to bring the whole block back, so what is asserted above
    // is a condition and not an emitter that stopped emitting.
    let with_resource = component_glue_for_spec("rec = [a: uint]\n", &[]);
    for token in GUEST_BLOCK {
        assert!(
            with_resource.contains(token),
            "a spec with a resource no longer emits `{token}`:\n{with_resource}"
        );
    }
}

/// The `Guest`-impl half of that condition is PER INTERFACE, while the guest block's half is per
/// package: a value-only interface sitting beside one that declares a resource still gets no `impl`,
/// and still gets its module alias and its enum bridges — the resource's own glue is what may need
/// to convert that enum.
#[test]
fn a_value_only_interface_gets_no_guest_impl_beside_an_interface_that_does() {
    let glue = component_glue_for_scopes(
        &[
            ("lib.cddl", "rec = [n: uint, s: shade]\n"),
            ("sub.cddl", "shade = 0 / 1\n"),
        ],
        &[],
    );
    assert!(
        glue.contains("impl wit_types::Guest for Component {"),
        "the interface that declares a resource lost its `Guest` impl:\n{glue}"
    );
    assert!(
        !glue.contains("wit_sub::Guest for Component"),
        "the value-only interface got a `Guest` impl for a trait `generate!` does not mint:\n{glue}"
    );
    assert!(
        glue.contains("as wit_sub;") && glue.contains("fn shade_to_wit("),
        "the value-only interface lost the alias and bridges its NEIGHBOUR's glue converts \
         through:\n{glue}"
    );
}

/// The exact rule the condition above encodes, at the one position no CDDL spec can currently
/// reach: an interface with a FREE FUNCTION and no resource.
///
/// `wit_bindgen` mints the `Guest` trait for it — free functions land on that trait — so a condition
/// spelled "has a resource" would suppress the impl this face's `any-cbor` free functions
/// (`cbor-kind`, `cbor-to-json`/`cbor-from-json`) need, turning one compile error into another.
/// Probed against `wit-bindgen` 0.57 by hand-adding a `func` to a value-only interface's WIT and
/// compiling the impl for `wasm32-wasip2`; pinned here because the projection attaches those
/// functions only to a scope that already stages a resource, so no fixture can hold the rule in
/// place.
#[test]
fn a_free_function_alone_mints_a_guest_trait() {
    use crate::generation::wit::{WitFunc, WitFuncOp, WitInterface, WitType};
    let mut iface = WitInterface {
        name: "types".to_owned(),
        scope: crate::intermediate::ModuleScope::new(vec!["lib".to_owned()]),
        uses: BTreeMap::new(),
        types: Vec::new(),
        funcs: Vec::new(),
    };
    assert!(
        !crate::generation::component::interface_has_guest(&iface),
        "an interface with neither a resource nor a function must get no `Guest` impl"
    );
    iface.funcs.push(WitFunc {
        name: "cbor-kind".to_owned(),
        params: Vec::new(),
        result: Some(WitType::AnyCborKind),
        fallible: true,
        op: WitFuncOp::AnyCborKind,
    });
    assert!(
        crate::generation::component::interface_has_guest(&iface),
        "an interface whose only member is a free function still has a `Guest` trait to implement"
    );
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

/// A complete record-level custom pair owns decoding without consulting the generated record-field
/// decoder. Even when the declared array members are structurally ambiguous, its seeded verdict
/// stays deserializable, so the WIT and guest glue retain the same from-CBOR seam as Rust/wasm.
#[test]
fn a_custom_record_pair_keeps_from_cbor_bytes_despite_ambiguous_fields() {
    const SPEC: &str = "custom_record = [? ignored: uint, value: uint] ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: custom_record]\n";
    let dir = scratch_dir("customnodeser");
    let path = dir.join("input.cddl");
    std::fs::write(&path, SPEC).unwrap();
    let files = crate::api::generated_strings(&cli_for(path.to_str().unwrap(), &[])).unwrap();
    let wit = files["component/wit/world.wit"].clone();
    let glue = files["component/src/generated/mod.rs"].clone();
    std::fs::remove_dir_all(&dir).ok();

    for resource in ["custom-record", "holder"] {
        let body = wit
            .split(&format!("resource {resource} {{"))
            .nth(1)
            .and_then(|rest| rest.split('}').next())
            .unwrap_or_else(|| panic!("missing {resource} resource:\n{wit}"));
        assert!(
            body.contains("to-cbor-bytes") && body.contains("from-cbor-bytes"),
            "the complete pair's {resource} surface lost one CBOR bridge:\n{wit}"
        );
    }
    assert!(
        glue.contains("cddl_lib::CustomRecord as cddl_lib::serialization::Deserialize")
            && glue.contains("cddl_lib::Holder as cddl_lib::serialization::Deserialize"),
        "the component glue lost a pair-owned decode bridge:\n{glue}"
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
///
/// The third column is the hand-written rust definitions of the spec's extern types, appended into
/// the generated crate's THIN ROOT before the cell is keyed (see the extern rows below). `None` for
/// a spec that declares no extern — most of them.
type BuildSmokeRow = (&'static str, &'static [&'static str], Option<&'static str>);

const BUILD_SMOKE_FIXTURES: &[BuildSmokeRow] = &[
    // Every phase-1 type-mapping row in one scope, in the posture the emitters target. Two of those
    // rows are here for a reason no WIT gate can express, because both are TYPE facts about the
    // generated rust crate: the NonEmpty TABLE (`counts`) makes the guest constructor re-enter the
    // runtime's vec-of-pairs `TryFrom` door, and the value-bounded field (`limit`) makes the rust
    // `Record::new` itself fallible, so the glue must unwrap it rather than wrap it.
    ("tests/component-core/input.cddl", &[], None),
    // CHOICES: the largest new glue surface phase 2 adds, and the one no WIT gate can judge. A
    // `kind` / `as-<variant>` arm that does not match the rust enum's ARM SHAPE, or a `new-<variant>`
    // that wraps a `Result` the rust ctor already returns, is a type error in generated code that
    // resolves, encodes and validates perfectly as WIT.
    ("tests/component-choices/input.cddl", &[], None),
    // The multi-INTERFACE shape: two `Guest` impls on one guest type under one `export!`, a
    // cross-interface `borrow` parameter, and an `own` handle minted for a resource another
    // interface declares. None of it is reachable from a single-scope fixture, and all of it is a
    // link-time fact no WIT gate can see.
    ("tests/component-multifile/inputs", &[], None),
    // VALUE WINDOWS: every row here is a fact about the generated rust crate that the WIT cannot
    // express — a bounded setter's `result<_, string>` reads the same whether the check is emitted
    // or not, and the two despecialization controls decide between a `TryFrom` door and an inline
    // check whose wrong choice is either a silent no-op or a trait impl that does not exist.
    ("tests/component-bounds/input.cddl", &[], None),
    // An explicit reject policy on a loose table is a component-specific type error when the
    // policy-only predicate routes its `BTreeMap` through the reject-set TryFrom door. Keep this
    // small corpus cell in the representative wasip2 smoke as well as the corpus-wide full gate:
    // the WIT itself remains valid when the guest Rust does not compile.
    ("tests/corpus/component_reject_table.cddl", &[], None),
    // The BRIDGING classes. Every other row here compiles a crate the tool wrote alone; these two
    // compile the glue that reaches types the tool does NOT define, which is a different failure
    // class entirely — the bridge names a TRAIT on a user-owned type, so naming the wrong one is a
    // compile error in the consumer's crate that no gate reading our output can see. The
    // hand-written definitions in the third column are what a real consumer supplies, appended into
    // the generated crate's thin root exactly as `run_test`'s `is_extern_type_def` path does.
    ("tests/component-extern/inputs", &[], Some(EXTERN_DEFS)),
    // The same bridges in the FORCE-CANONICAL posture, which is the one that moves them: it adds
    // `to-canonical-cbor-bytes` to the extern's seam (its contract does require the runtime's own
    // `Serialize`, on which that method is declared) and must leave the raw-bytes bridge — whose
    // contract is `RawBytesEncoding` and nothing more — untouched. The defs flavor changes with it,
    // because the contract the posture imposes on the USER's type changes with it.
    (
        "tests/component-extern/inputs",
        &["--preserve-encodings=true", "--canonical-form=true"],
        Some(EXTERN_DEFS_CANONICAL),
    ),
    // The two classes whose ONLY symptom is a compile failure: the WIT they produce resolves,
    // encodes and validates, and the tool exits 0, so nothing short of compiling the guest crate can
    // judge either. `component-any-alias` pins that an `any` reached through an alias chain gets the
    // FALLIBLE door its `?`-decoding body needs (an infallible one is E0277);
    // `component-ident-hazard` pins the other direction — that unexporting the resource named `t`
    // leaves a crate that BUILDS, which is what makes exclusion a better answer than an abort.
    ("tests/component-any-alias/input.cddl", &[], None),
    ("tests/component-ident-hazard/input.cddl", &[], None),
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
        None,
    ),
];

/// The hand-written extern definitions the two bridging rows above pair their spec with. Two
/// flavors, because the CONTRACT differs by posture: the default one asks an extern for
/// `cbor_event`'s `Serialize`, the force-canonical one for the runtime's own — which is exactly the
/// difference the canonical row exists to compile.
const EXTERN_DEFS: &str = "tests/component-extern/external_rust_defs";
const EXTERN_DEFS_CANONICAL: &str = "tests/component-extern/external_rust_defs_canonical";

/// THE acceptance gate for the guest emitters: a generated component crate that does not compile is
/// the failure mode the whole face exists to prevent, and every other gate here is blind to it — the
/// WIT can resolve, encode and validate perfectly while the glue implementing it names a trait method
/// that does not exist.
///
/// **Built from the WORKSPACE ROOT, over the manifests exactly as emitted.** A root build compiles
/// every member's own lib targets for wasip2 — the rust crate's `[lib]` included — which is the
/// posture a consumer building their workspace for that target has, and the one that reaches the
/// rust crate's `crate-type`. Building only `component/` would reach the rust crate solely as a
/// dependency and so could not see it. Nothing about the emitted tree is edited first: the assertion
/// below states the narrowing this face's manifests carry, rather than arranging it.
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

    for (input, flags, extern_defs) in BUILD_SMOKE_FIXTURES {
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
        // The emitted contract, asserted rather than arranged: under `--component=true
        // --wasm=false` the tool narrows the rust crate to `crate-type = ["rlib"]` itself, because
        // no face in such a tree wants the cdylib (the guest links the rlib; the cdylib exists for
        // wasm-bindgen's `wasm32-unknown-unknown` target, and asking the wasip2 linker for it has
        // crashed `wasm-component-ld` on some specs). This gate builds the manifest AS EMITTED —
        // if that narrowing ever regresses, the workspace build below is what a consumer would hit,
        // so the assertion has to fail here rather than the gate quietly re-arranging it.
        let rust_manifest = out.join("rust/Cargo.toml");
        let manifest_text = std::fs::read_to_string(&rust_manifest).unwrap();
        assert!(
            manifest_text.contains("crate-type = [\"rlib\"]"),
            "{label}: a component-only tree must be emitted rlib-only, not narrowed by hand:\n\
             {manifest_text}"
        );

        // The extern definitions go into the user-owned THIN ROOT, never `generated/**` — that
        // subtree is clobbered every regen and already carries the glue's own `pub use crate::<Name>;`
        // re-export of each extern, which a definition beside it would collide with (E0255). Written
        // BEFORE `run_cached` on purpose: it is an input the cached cell consumes, so it has to be
        // inside the hashed root or an edit to it would serve the stale PASS forever.
        if let Some(defs_path) = extern_defs {
            let defs = std::fs::read_to_string(defs_path)
                .unwrap_or_else(|e| panic!("{label}: cannot read extern defs {defs_path}: {e}"));
            let lib_rs = out.join("rust/src/lib.rs");
            let mut root = std::fs::read_to_string(&lib_rs).unwrap();
            root.push_str("\n\n");
            root.push_str(&defs);
            std::fs::write(&lib_rs, root).unwrap();
        }

        let outcome = gate_cache::run_cached(
            "component_wasip2_build",
            &label,
            &out,
            &[
                std::path::PathBuf::from("component/Cargo.toml"),
                std::path::PathBuf::from("rust/Cargo.toml"),
            ],
            &[
                // The WORKSPACE ROOT, not `component/`: a root build compiles every member's own
                // lib targets for wasip2, so the rust crate's emitted `[lib]` is under test too
                // rather than being reached only as a dependency. That is the posture a consumer
                // building their workspace for wasip2 actually has.
                "cwd=workspace-root".to_owned(),
                "cargo".to_owned(),
                "build".to_owned(),
                "--target".to_owned(),
                "wasm32-wasip2".to_owned(),
            ],
            || {
                let build = crate::tests::integration_tests::tool_cmd("cargo")
                    .args(["build", "--target", "wasm32-wasip2"])
                    .current_dir(&out)
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

// -------------------------------------------------------------------------------------------------
// The corpus-breadth wasip2 compile gate
// -------------------------------------------------------------------------------------------------

/// Corpus fixtures whose emitted component glue does NOT compile, with the emitter bug each one
/// reaches. Every entry is a FINDING this gate made, not a decision — the ledger exists so the
/// classes are recorded mechanically rather than in prose, and so a fix cannot land unnoticed:
/// staleness is guarded BOTH ways, a listed fixture that starts compiling fails as "the bug is
/// fixed — remove the pin", an unlisted one that stops compiling fails as a regression.
///
/// Two classes remain, both in `component/src/generated/mod.rs` and both reproducing under
/// `--wasm=true` and `--wasm=false` alike. Neither is a nested-position accident the emitter can
/// spell its way out of: each needs a fact the projection does not carry today, which is why they
/// are ledgered rather than fixed.
///
/// 1. **The despecialized NonEmpty in a NESTED position.** A `[+ T]` / `{+ K => V}` reached through
///    a named collection rule — as a list ELEMENT or a map KEY — makes the glue `.collect()`
///    straight into `NonEmptyVec`/`NonEmptyMap`, which have no `FromIterator` (E0277). The
///    `TryFrom` door those types own is re-entered only for a parameter that is despecialized at its
///    TOP level (`materialize` routes off `wit::wit_param_despecialized` of the whole parameter), and
///    the conversion walk below that point sees WIT types only, so a nested despecialization is
///    invisible to it. What remains is exactly that: the rust type threaded through the conversion
///    walk beside the WIT type, so the nested door can re-enter `TryFrom`. The SURFACE half is
///    already in place — `wit_param_validates` resolves through the alias a named collection rule
///    registers, so these doors carry the `result<_, string>` such a re-check has to report
///    through.
/// 2. **`@default` fields.** A `.default`ed field is a PLAIN `T` on the rust side (the default fills
///    absence in), while the projection still treats it as optional — so the glue reads
///    `me.b.as_ref()` on a `u64` (E0599) and writes `field = Some(v)` into a `u64` (E0308). The fix
///    is in the projection's optionality rule, not in the glue.
///
/// A fixture that fails to GENERATE belongs in `snapshot_tests::PROFILE_GENERATION_SKIP` instead;
/// a fixture whose RUST crate cannot compile standalone belongs in
/// `integration_tests::COMPILE_SKIP`, which this gate shares rather than restating.
const EXPECTED_COMPILE_FAIL: &[(&str, &str)] = &[
    (
        "composite_map_key",
        "class 1: the map KEY of a named table rule is a `NonEmptyVec<u64>`, and the glue \
         `.collect()`s into it (E0277) — a nested despecialization the top-level `TryFrom` routing \
         cannot see",
    ),
    (
        "nonempty_nested_positions",
        "class 1: same, in both flavors and both nesting positions — a `NonEmptyVec<u64>` list \
         ELEMENT and a `NonEmptyMap<u64, u64>` map KEY",
    ),
    (
        "default_value",
        "class 2: a `@default`ed scalar is a plain `u64`/`String` in the rust struct, but the \
         projection keeps it optional — `as_ref` on a `u64` (E0599), `= Some(v)` into a `u64` \
         (E0308) and an unannotated binding (E0282)",
    ),
];

/// Corpus-breadth companion to [`component_crate_builds_for_wasm32_wasip2`]: every
/// `tests/corpus/*.cddl` fixture's emitted component crate, type-checked for `wasm32-wasip2`.
///
/// It exists because nothing else can reach this breadth.
/// `integration_tests::feature_corpus_compiles` structurally cannot: it hardcodes
/// `crate_subs = ["rust", "wasm"]` and runs a HOST `cargo check` with no `--target` anywhere, so the
/// component crate is invisible to it — which is why the `ALL_PROFILES` component row filters out of
/// that gate rather than flowing into it. The build smoke above compiles a representative subset;
/// this one asks the same question of the whole corpus, and the answer differs, which is the whole
/// argument for it.
///
/// **`check`, not `build`.** The link is already asserted on representative fixtures by the build
/// smoke, and the class that matters at corpus breadth — glue naming a trait, method or macro that
/// does not exist — is a TYPE-check failure. Probed rather than assumed: `cargo check
/// --target wasm32-wasip2` expands `wit_bindgen::generate!` and reports every one of the four
/// failure classes ledgered above, three of which are exactly "the glue names something the
/// bindings never minted".
///
/// **No sharding.** Measured 89 cells in 100 s wall end to end (generation included): ~10 s for the
/// first cell, which builds the shared dependency graph, then ~0.4 s each. `feature_corpus_compiles`
/// shards because its cells cost seconds apiece; sizing this one from that curve rather than from
/// its own measurement would buy process overhead and nothing else.
///
/// Nested cargo, memoized per generated-crate content hash. The per-cell EXPECTATION is part of the
/// key, so removing a pin re-runs the cell rather than laundering its cached verdict past the new
/// expectation. `GATE_CACHE=0` forces every check.
#[test]
#[ignore]
fn component_corpus_compiles() {
    let flags = component_profile_flags();
    let scratch_name = format!(
        "cddl_codegen_component_corpus_{:016x}",
        crate::tests::integration_tests::checkout_hash()
    );
    let _scratch_lock = crate::tests::integration_tests::acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    // Not removed: `target/` under it (measured ≈400 MiB) is the shared dependency graph whose first
    // build is ~10 s and whose reuse is what makes a cell ~0.4 s.
    let target_dir = root.join("target");
    std::fs::create_dir_all(&target_dir).unwrap();

    let mut entries: Vec<PathBuf> = std::fs::read_dir("tests/corpus")
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();

    let mut failures = Vec::new();
    let mut resurfaced = Vec::new();
    let mut swept = 0usize;
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;

    // EVERY cell GENERATES IMMEDIATELY BEFORE IT CHECKS, and that ordering is load-bearing rather
    // than incidental. All 89 emitted component crates are `cddl-lib-component v0.1.0`, and cargo
    // does not tell two of them apart across the shared target dir: a batch that generated every
    // fixture first and checked afterwards was measured serving one fixture's `Finished` — warnings
    // replayed and all — for the NEXT fixture's check, turning a real failure into a silent pass.
    // Regenerating inside the loop keeps every cell's sources newer than the fingerprint the
    // previous cell wrote, so each one is dirty by construction. Hoisting generation out of this
    // loop (or reusing a tree a previous cell built) makes the sweep vacuous without failing.
    for path in &entries {
        let stem = path.file_stem().unwrap().to_str().unwrap().to_owned();
        // A fixture that cannot GENERATE under this profile is the snapshot axis' business.
        if crate::tests::snapshot_tests::PROFILE_GENERATION_SKIP
            .contains(&(stem.as_str(), crate::tests::COMPONENT_PROFILE))
        {
            continue;
        }
        // A fixture whose RUST crate references user-supplied code that no definition can supply is
        // skipped; one whose user code CAN be written gets it written below, after generation.
        // Shared with `feature_corpus_compiles` rather than restated, so the two gates can never
        // disagree about which fixtures are which — the component crate takes the rust crate as a
        // path dependency, so it needs exactly the same seeding to build.
        if crate::tests::integration_tests::COMPILE_SKIP.contains(&stem.as_str()) {
            continue;
        }
        let expected_fail = EXPECTED_COMPILE_FAIL.iter().any(|(s, _)| *s == stem);
        let out = root.join(&stem);
        // A stale tree would poison the tree hash with files this run did not emit.
        let _ = std::fs::remove_dir_all(&out);
        std::fs::create_dir_all(&out).unwrap();

        let mut args = vec![
            "--input".to_owned(),
            path.to_str().unwrap().to_owned(),
            "--output".to_owned(),
            out.to_str().unwrap().to_owned(),
            "--component=true".to_owned(),
        ];
        args.extend(flags.iter().map(|f| (*f).to_owned()));
        let generated = crate::tests::integration_tests::codegen_cmd()
            .args(&args)
            .output()
            .unwrap();
        assert!(
            generated.status.success(),
            "{stem}: generation failed under the component profile — a fixture that cannot generate \
             belongs in `snapshot_tests::PROFILE_GENERATION_SKIP` with a reason, never here\n{}",
            String::from_utf8_lossy(&generated.stderr)
        );
        // Seed the user-supplied side the fixture's spec names (extern / raw-bytes types, custom
        // codec fns) into the thin crate roots, exactly as `feature_corpus_compiles` does — this
        // gate builds the rust crate as the component crate's path dependency, so without the seed
        // those fixtures would fail on undefined names rather than on anything about the component
        // face. The component profile is neither json nor preserve.
        crate::tests::integration_tests::append_corpus_defs_for(&out, &stem, false, false);
        // A workspace root so the emitted crates share one lock and one target dir. Real consumers
        // own this file; the tool never writes one.
        std::fs::write(
            out.join("Cargo.toml"),
            "[workspace]\nresolver = \"3\"\nmembers = [\"rust\", \"component\"]\n",
        )
        .unwrap();
        // This sweep is a BOTH-FACES tree — the `ALL_PROFILES` component row leaves `--wasm` at its
        // default `true` (see that row for the four reasons), so the tool keeps the cdylib the wasm
        // face's `wasm32-unknown-unknown` build needs and the hand narrowing below is the remedy the
        // flag doc prescribes for exactly this shape. Asserting the wide form first is what keeps
        // the two halves honest: were this row ever to gain `--wasm=false`, the emitted manifest
        // would already be rlib-only and this edit would silently become a no-op whose comment lied.
        let rust_manifest = out.join("rust/Cargo.toml");
        let manifest_text = std::fs::read_to_string(&rust_manifest).unwrap();
        assert!(
            manifest_text.contains("crate-type = [\"cdylib\", \"rlib\"]"),
            "{stem}: the component profile is a both-faces posture, so the rust manifest must still \
             carry the wasm face's cdylib — if it no longer does, drop this narrowing\n\
             {manifest_text}"
        );
        std::fs::write(
            &rust_manifest,
            manifest_text.replace(
                "crate-type = [\"cdylib\", \"rlib\"]",
                "crate-type = [\"rlib\"]",
            ),
        )
        .unwrap();

        let component_dir = out.join("component");
        let mut stderr = String::new();
        let outcome = gate_cache::run_cached(
            "component_corpus_compiles",
            &stem,
            &out,
            &[
                PathBuf::from("component/Cargo.toml"),
                PathBuf::from("rust/Cargo.toml"),
            ],
            &[
                // The cell's EXPECTATION is part of the key: removing a pin must re-run the cell
                // rather than serve a PASS recorded under the old expectation.
                format!("expect={}", if expected_fail { "fail" } else { "pass" }),
                "cwd=component".to_owned(),
                "cargo".to_owned(),
                "check".to_owned(),
                "--target".to_owned(),
                "wasm32-wasip2".to_owned(),
            ],
            || {
                let check = crate::tests::integration_tests::tool_cmd("cargo")
                    .args(["check", "--target", "wasm32-wasip2"])
                    .current_dir(&component_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                stderr = String::from_utf8_lossy(&check.stderr).into_owned();
                if !check.status.success()
                    && (stderr.contains("can't find crate for `core`")
                        || stderr.contains("target may not be installed"))
                {
                    // A provisioning problem must never read as an emitter failure, and must never
                    // be absorbed by an expected-fail pin either.
                    failures.push(format!(
                        "{stem}: the wasm32-wasip2 target is not installed under the pinned \
                         toolchain — `rustup target add wasm32-wasip2`"
                    ));
                    return false;
                }
                check.status.success() != expected_fail
            },
        );
        cache_run += outcome.ran();
        cache_hit += outcome.cached();
        if !outcome.success() {
            if expected_fail {
                resurfaced.push(format!(
                    "{stem}: pinned as EXPECTED_COMPILE_FAIL but its component crate now \
                     type-checks — the emitter bug is fixed, so remove the pin"
                ));
            } else if !failures.iter().any(|f: &String| f.starts_with(&stem)) {
                failures.push(format!(
                    "{stem}: the emitted component crate does not type-check for \
                     wasm32-wasip2\n{stderr}"
                ));
            }
        }
        swept += 1;
        // The per-cell tree is freed; `target/` (a sibling, not a child) is what survives.
        let _ = std::fs::remove_dir_all(&out);
    }

    if gate_cache::enabled() {
        println!("component_corpus_compiles gate-cache: {cache_run} run, {cache_hit} cached");
    }
    // Stale-pin guard, the direction the sweep itself cannot see: an entry naming a fixture that no
    // longer exists (or is skipped) would silently excuse nothing forever.
    let stems: std::collections::BTreeSet<String> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap().to_owned())
        .collect();
    for (stem, _) in EXPECTED_COMPILE_FAIL {
        assert!(
            stems.contains(*stem),
            "EXPECTED_COMPILE_FAIL names corpus fixture `{stem}`, which no longer exists — stale \
             pin, remove or fix it"
        );
    }
    // Vacuity floor: a filter bug that swept nothing would otherwise pass silently. The corpus only
    // grows, so this is a lower bound rather than a pin.
    assert!(
        swept >= 80,
        "only {swept} corpus fixtures were compile-checked for the component face (expected >= 80) \
         — the corpus enumeration or one of the skip filters shrank"
    );
    assert!(
        failures.is_empty() && resurfaced.is_empty(),
        "corpus component crates do not type-check as expected:\n\n{}\n\n{}",
        failures.join("\n\n"),
        resurfaced.join("\n")
    );
}
