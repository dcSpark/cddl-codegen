//! Fast, in-process golden snapshots of generated output.
//!
//! Unlike the heavy integration tests in `integration_tests.rs` (which shell out to `cargo run`
//! and then compile + round-trip the generated crates), these drive the generator as a library
//! (`crate::api`) and snapshot the post-rustfmt generated source directly. No subprocess, no
//! compilation, no `target/` bloat. They give a localized diff when generation output changes;
//! the integration tests remain the "does it actually compile & round-trip" correctness gate.
//!
//! Four suites:
//! * [`feature_corpus`] — one tiny CDDL file per language construct (in `tests/corpus/`), each
//!   snapshotted under every flag profile plus an IR dump. A one-feature regression yields a
//!   one-file diff. Grouped under `tests/corpus/snapshots/<feature>/`. The generated `Cargo.toml`
//!   and the json-gen `main.rs` are skipped here (near-constant — they're covered by
//!   [`whole_program`], [`cargo_toml_matrix`] and [`serialization_prelude`] instead). The
//!   `component` profile snapshots only its `component/**` files and ASSERTS the rest byte-equal
//!   to the `default` profile's — see [`assert_component_face_is_additive`].
//! * [`whole_program`] — the existing integration inputs (incl. multifile) each under one
//!   known-safe profile, capturing the *full* output (incl. `Cargo.toml`s) to cover cross-feature
//!   interactions, the scope/module path, and the edition/deps logic.
//! * [`cargo_toml_matrix`] — a curated input × profile matrix covering every distinct generated
//!   `Cargo.toml` dependency combination (the type-conditional `hex`/`wasm-bindgen` deps toggled
//!   independently), which `whole_program` alone does not produce.
//! * [`serialization_prelude`] — the static serialization runtime, once per flag combination.
//!
//! A per-feature `serialization.rs` snapshot is sometimes empty, or just an import block. That's
//! expected: these snapshots capture the *generated-only* code (the static runtime prelude that
//! `export` prepends is excluded — it's the [`serialization_prelude`] suite), and some constructs
//! emit no standalone (de)serialization impl at all (aliases, c-style enums — see
//! `docs/docs/output_format.mdx` for which and why). The root file's lone import block is what that
//! prepended prelude needs.
//!
//! Bless after an intentional change with `INSTA_UPDATE=always cargo test` (or `cargo insta review`).

use super::{ALL_PROFILES, Profile};
use crate::cli::Cli;
use clap::Parser;

/// Build a `Cli` for in-process generation. `--output` is unused (`generated_strings` does no disk
/// I/O) but clap requires it; `--static-dir` defaults to `static/`, read for Cargo.toml/prelude.
fn cli_for(input: &std::path::Path, extra: &[&str]) -> Cli {
    let input = input.to_str().unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input,
        "--output",
        "snapshot_unused",
    ];
    args.extend_from_slice(extra);
    Cli::parse_from(args)
}

/// Near-constant generated files skipped by the per-feature corpus (they don't vary by construct).
fn is_per_feature_noise(path: &str) -> bool {
    path.ends_with("Cargo.toml") || path == "wasm/json-gen/src/main.rs"
}

/// `(corpus fixture stem, profile)` whose GENERATION deliberately aborts under that profile, so it
/// has no snapshot to pin there (the fixture is still snapshotted under its other profiles). A
/// listed stem absent from `tests/corpus` fails as a stale pin (checked in
/// [`feature_corpus_pins_are_live`]).
///
/// For the `default`/`preserve`/`json` rows this mirrors
/// `integration_tests::feature_corpus_compiles`'s `EXPECTED_GENERATION_FAIL` — the same fixture
/// aborts in both gates, so a pin here has a twin there. The mirror does NOT extend to
/// [`super::COMPONENT_PROFILE`]: that gate filters the component row out (the component crate
/// targets wasip2 and the gate `cargo check`s for the HOST), so a component pin here has no twin
/// and is reconciled against the corpus alone. The component-face breadth check a pin here excuses
/// a fixture from is `component_tests::component_wit_validates_the_corpus`, which shares this list.
pub(super) const PROFILE_GENERATION_SKIP: &[(&str, &str)] = &[(
    // An `@ignore` open struct-map is rejected under --preserve-encodings (a preserve crate's
    // byte-exact round-trip contract cannot hold for a type that drops unknown entries); default/
    // json snapshot the closed-struct surface.
    "dsl_ignore",
    "preserve",
)];

/// The `component/**` path prefix, taken from the generator's own spelling so the additive split
/// below can never drift from where the component face actually emits.
fn is_component_face(path: &str) -> bool {
    path.starts_with(&format!("{}/", crate::generation::layout::COMPONENT_DIR))
}

/// The component face is purely ADDITIVE: `--component=true` mints the `component/**` crate and
/// changes no other emitted byte — not the rust tree, not the wasm tree, not a `Cargo.toml`. Assert
/// exactly that, by comparing the profile's non-`component/**` output against the `default`
/// profile's whole output.
///
/// This is what lets [`snapshot_input`] snapshot only `component/**` under that profile, and it is
/// strictly stronger than snapshotting the rust and wasm trees a fourth time: those bytes are
/// duplicates BY CONSTRUCTION, and pinning them would encode the invariant only implicitly, whereas
/// this states it — and fails loudly the day the component face starts leaking into the other two,
/// which is exactly the day it stops being additive.
fn assert_component_face_is_additive(
    label: &str,
    profile: &str,
    files: &std::collections::BTreeMap<String, String>,
    default_files: &std::collections::BTreeMap<String, String>,
) {
    let rest: std::collections::BTreeMap<&String, &String> = files
        .iter()
        .filter(|(path, _)| !is_component_face(path))
        .collect();
    let explain = "`--component` is no longer purely additive: it changed a file outside \
                   `component/**`. Either the component face has started leaking into the rust or \
                   wasm surface (a bug), or that leak is intended — in which case this profile can \
                   no longer snapshot `component/**` alone and must pin the whole tree";
    for (path, content) in &rest {
        match default_files.get(path.as_str()) {
            None => panic!(
                "{label}/{profile}: `{path}` is emitted under this profile but NOT under \
                 `default` — {explain}"
            ),
            Some(baseline) if baseline != *content => panic!(
                "{label}/{profile}: `{path}` differs from the `default` profile's byte-for-byte — \
                 {explain}"
            ),
            Some(_) => {}
        }
    }
    for path in default_files.keys() {
        assert!(
            rest.contains_key(path),
            "{label}/{profile}: `{path}` is emitted under `default` but NOT under this profile — \
             {explain}"
        );
    }
}

/// Snapshot the generated source for `input` under each profile (grouped under
/// `tests/corpus/snapshots/<label>/`). `full` keeps every generated file; otherwise the
/// near-constant manifest/main files are skipped. `with_ir` adds one IR dump.
///
/// `additive_profile` names a profile whose output is snapshotted only for its `component/**`
/// files, with every other file asserted byte-identical to the `default` profile's
/// ([`assert_component_face_is_additive`]). Only the corpus sweep passes it: the whole-program
/// `component` rows deliberately pin the FULL tree (and some of them carry encoding/json flags on
/// top, so their rust and wasm halves legitimately differ from `default`'s).
fn snapshot_input(
    input: &std::path::Path,
    label: &str,
    profiles: &[Profile],
    full: bool,
    with_ir: bool,
    additive_profile: Option<&str>,
) {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots")
        .join(label);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    let has_default = profiles.iter().any(|(profile, _)| *profile == "default");
    settings.bind(|| {
        let mut default_files: Option<std::collections::BTreeMap<String, String>> = None;
        for (profile, extra) in profiles {
            let cli = cli_for(input, extra);
            let files = crate::api::generated_strings(&cli)
                .unwrap_or_else(|e| panic!("generation failed for {}/{}: {}", label, profile, e));
            assert!(
                !files.is_empty(),
                "no generated files for {}/{}",
                label,
                profile
            );
            let additive = additive_profile == Some(*profile);
            if additive {
                match &default_files {
                    Some(baseline) => {
                        assert_component_face_is_additive(label, profile, &files, baseline)
                    }
                    // No baseline to compare against. Legitimate only when this fixture has no
                    // `default` row at all (a `PROFILE_GENERATION_SKIP` case): the identity check
                    // is skipped and the `component/**` snapshots still land. If `default` IS in
                    // the list, the rows are out of order and the check would silently never run.
                    None => assert!(
                        !has_default,
                        "{label}/{profile}: the `default` profile is swept but produced no \
                         baseline before this row — ALL_PROFILES must keep `default` FIRST, or \
                         the additive check silently never runs"
                    ),
                }
            }
            for (path, content) in &files {
                if additive && !is_component_face(path) {
                    continue;
                }
                if !full && is_per_feature_noise(path) {
                    continue;
                }
                let name = format!("{}__{}", profile, path.replace('/', "__"));
                insta::assert_snapshot!(name, content);
            }
            if *profile == "default" {
                default_files = Some(files);
            }
        }
        if with_ir {
            // IR is essentially profile-independent, so snapshot it once under default. Localizes
            // a regression to parsing/IR vs generation.
            let ir = crate::api::ir_structs_debug(&cli_for(input, &[]))
                .unwrap_or_else(|e| panic!("IR build failed for {}: {}", label, e));
            insta::assert_snapshot!("ir", ir);
        }
    });
}

/// The existing integration inputs, each under the flag profile it is known-safe with (the same
/// pairings the heavy integration tests use). Captures the full output to cover cross-feature
/// interactions, the multifile scope/module path, and the edition/deps Cargo.toml logic.
///
/// A corpus file with one of these labels as its stem would clobber its snapshot dir, so
/// [`feature_corpus`] guards against the collision.
const WHOLE_PROGRAM_CASES: &[(&str, &str, Profile)] = &[
    ("core", "tests/core/input.cddl", ("default", &[])),
    (
        "preserve_encodings",
        "tests/preserve-encodings/input.cddl",
        ("preserve", &["--preserve-encodings=true"]),
    ),
    (
        "canonical",
        "tests/canonical/input.cddl",
        (
            "canonical",
            &["--preserve-encodings=true", "--canonical-form=true"],
        ),
    ),
    (
        "json",
        "tests/json/input.cddl",
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ),
    // float JSON emission — a separate fixture from `json` for the hand-written tests beside it
    // (validation against the emitted schema through a jsonschema dev-dep, and a bit-exact f64 JSON
    // round-trip that holds only while the generated manifest carries serde_json's
    // `float_roundtrip` feature). Floats themselves are ordinary corpus material under every profile.
    (
        "json_float",
        "tests/json-float/input.cddl",
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ),
    // loose-CBOR `any` positions (member / array element / table domain+range / top-level alias /
    // tagged / last-position choice arm), lowered to the AnyCbor runtime type. All legs pass
    // --wasm=false to isolate the rust and json surfaces here (the wasm AnyCbor surface is pinned
    // by the wasm-parity suite), which is why this is a profile-limited whole_program input rather
    // than an all-profile feature-corpus entry. Pinned under the three rust modes (canonical merge +
    // preserve encoding-field threading) plus the json mode (the serde/schemars surface: the AnyCbor
    // fragments concatenate into the any_cbor module under the JSON flags).
    (
        "any_positions_default",
        "tests/any-positions/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "any_positions_preserve",
        "tests/any-positions/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    (
        "any_positions_canonical",
        "tests/any-positions/input.cddl",
        (
            "canonical",
            &[
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--wasm=false",
            ],
        ),
    ),
    (
        "any_positions_json",
        "tests/any-positions/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // preserve + json together — the only profile that pins the OrderedHashMap natural-JSON adapters
    // (`natural_any_cbor_orderedmap` / `_opt_orderedmap`) for a preserve `{* K => any}` member; the
    // plain `json` profile above is non-preserve (BTreeMap adapters).
    (
        "any_positions_preserve_json",
        "tests/any-positions/input.cddl",
        (
            "preserve_json",
            &[
                "--preserve-encodings=true",
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // loose-CBOR open struct-maps (a trailing `* k => v` rest row after fixed keys → a `pub rest`
    // capture map). Snapshotted under the plain (`default`), preserve, AND json flavors: the preserve
    // fidelity core (orig_deser_order interleave, per-entry encoding sidecars, the runtime canonical
    // key merge) under `preserve`; the flattened-JSON surface (the rest field's
    // `#[serde(flatten)]` + per-struct serialize_with/deserialize_with helpers + additionalProperties
    // schema) under `json`. These profiles pass `--wasm=false` to isolate the rust/json surfaces (the
    // wasm rest accessor is pinned by the separate `open_struct_map_wasm` profile below) — a
    // profile-limited whole_program input rather than an all-profile feature-corpus entry.
    (
        "open_struct_map_default",
        "tests/open-struct-map/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "open_struct_map_preserve",
        "tests/open-struct-map/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    (
        "open_struct_map_json",
        "tests/open-struct-map/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // TYPED rest-row key domains (`* K => V` where `K` is not bare `uint`/`text`/`any`): the seek
    // path. These pin the emitted shape the fast peeked path cannot produce — the hoisted
    // `let initial_position = raw.position();`, the rewinding catch-all in each declared-key match
    // arm (including the plain text arm's lifted `let text_key = raw.text()?;`, which ends the
    // scrutinee's borrow before the arm seeks), `K::deserialize` in the Special and `_` arms, and the
    // key's own encoding vars filing into `rest_key_encodings` under preserve. Its own fixture rather
    // than rows on `open-struct-map` because that one's rows are all bare-domain, and the shapes that
    // make the seek path observable (a union K, a sidecar-bearing K, an encoding-op K) have no home
    // there.
    //
    // The `json` profile pins the typed-K JSON face's two emitted routes side by side: a NOMINAL K
    // (the union, the `@duplicates preserve` twin) images through its own CBOR bytes
    // (`typed_rest_key_string` / `rest_key_from_string`), while a PRIMITIVE K (the sized int, the
    // `bytes` row, the encoding-op row) states its image directly — plus the K-free
    // `general_key_rest_map_schema` annotation on BOTH containers, which is the one place a typed row
    // diverges from the bare uint/text rows' schema.
    (
        "open_struct_map_typed_default",
        "tests/open-struct-map-typed/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "open_struct_map_typed_preserve",
        "tests/open-struct-map-typed/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    (
        "open_struct_map_typed_json",
        "tests/open-struct-map-typed/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    // The wasm rest accessor over a typed key domain: the getter returns the structural
    // `MapKToV`/`PairMapKToV` class and its `keys()` mints `<K>List` — both generic over `K` already,
    // so this row is the proof rather than the plumbing. Its `wasm_json` sibling is the one profile
    // where the two surfaces meet: the wasm wrappers over a typed `K` and the json-gen crate that
    // must derive `JsonSchema` over the same shapes, which is where a K-schema obligation the
    // K-free rest-map helper does not impose would show up.
    (
        "open_struct_map_typed_wasm",
        "tests/open-struct-map-typed/input.cddl",
        ("wasm", &[]),
    ),
    (
        "open_struct_map_typed_wasm_json",
        "tests/open-struct-map-typed/input.cddl",
        (
            "wasm_json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ),
    // The IGNORE flavor (`@ignore` on the rest row): unknown entries are typed-deserialized and
    // DROPPED — no `rest` field, serialize emits declared members only, and JSON/schemars/wasm are a
    // closed struct's (none of capture's flatten/getter/sidecar machinery). Snapshotted non-preserve
    // with `--wasm=false` (`@ignore` is rejected under --preserve-encodings). The generated source
    // pins that these open structs emit as CLOSED structs while the deserialize loop stays
    // dynamic-length (so a definite map with extra entries decodes) and consumes+drops each unknown
    // uint/text/any entry.
    (
        "open_struct_map_ignore",
        "tests/open-struct-map-ignore/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    // The wasm rest surface: each open struct's wasm wrapper gains a `rest` getter
    // returning the captured entries as the minted map wrapper (`MapKToV` / the `@duplicates
    // preserve` PairMap-backed twin). `--wasm` is the default, so this profile pins the emitted wasm
    // getters + minted rest-map wrappers.
    (
        "open_struct_map_wasm",
        "tests/open-struct-map/input.cddl",
        ("wasm", &[]),
    ),
    // open ARRAYS (a final-position occurrence-bearing rest tail after ≥1 fixed member → a loose
    // `Vec<T>`, compatibility `NonEmptyVec<T>`, or bounded `BoundedVec<T, MIN, MAX>` capture, or a
    // dropped loose `@ignore` tail). Snapshotted under `default`/`json`/`wasm`; the
    // `default`/`json` rows pass `--wasm=false` to isolate the rust/json surfaces, and the `wasm` row
    // pins the emitted `rest()` list-wrapper getter + minted `TList`/`AnyList` wrappers. Covers a typed
    // tail, a bounded tail, an `any` tail, a `@name`d tail, an `@ignore` tail, and the degenerate shape combos
    // (all-mandatory prefix + `@ignore` — the empty-conditional `definite_info` path — single fixed
    // member + tail, and an optional fixed-value member + a type-distinct tail). The PRESERVE surface
    // (byte-exact per-element tail encodings via the positional `{field}_elem_encodings` sidecar) is
    // pinned separately on a capture-only input, since `@ignore` is rejected under
    // --preserve-encodings and this fixture mixes capture + ignore rules.
    (
        "open_array_default",
        "tests/open-array/input.cddl",
        ("default", &["--wasm=false"]),
    ),
    (
        "open_array_json",
        "tests/open-array/input.cddl",
        (
            "json",
            &[
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
    ),
    (
        "open_array_wasm",
        "tests/open-array/input.cddl",
        ("wasm", &[]),
    ),
    // PRESERVE tail fidelity on a capture-only input (the shared `open-array/input.cddl` mixes
    // `@ignore` rules, which reject under --preserve-encodings). Pins the positional
    // `{field}_elem_encodings` sidecar (typed tail), the self-carried `any` tail (no sidecar), and the
    // canonical per-element normalization.
    (
        "open_array_preserve",
        "tests/open-array-preserve-e2e/input.cddl",
        ("preserve", &["--preserve-encodings=true", "--wasm=false"]),
    ),
    // directory input — exercises the multi-file scope/module codegen path.
    ("multifile", "tests/multifile/inputs", ("default", &[])),
    // extern (`_CDDL_CODEGEN_EXTERN_TYPE_`) and raw-bytes (`_CDDL_CODEGEN_RAW_BYTES_TYPE_`)
    // emit paths: they reference user-supplied types so their output can't compile standalone
    // (behavioral coverage is their integration fixtures), but this suite never compiles —
    // it's where their emitted source gets pinned. Same profile pairings the integration
    // tests use.
    (
        "extern_deps",
        "tests/extern-deps/inputs",
        (
            "preserve",
            &[
                "--preserve-encodings=true",
                "--common-import-override=extern_dep_crate",
            ],
        ),
    ),
    // extern-dep types across the WASM boundary (list element / table value) against a split
    // rust/wasm dependency, via `--extern-wasm-crate`. Pins the mapped wasm import + the dep's-rust
    // inner-storage path that the `extern_deps_wasm` integration test then compiles.
    (
        "extern_deps_wasm",
        "tests/extern-deps-wasm/inputs",
        (
            "preserve",
            &[
                "--preserve-encodings=true",
                "--common-import-override=extern_dep_crate",
                "--extern-wasm-crate=extern_dep_crate=extern_dep_crate_wasm",
            ],
        ),
    ),
    ("raw_bytes", "tests/raw-bytes/input.cddl", ("default", &[])),
    // The WASM COMPONENT face, which no other profile reaches: a `--component=true` run adds a whole
    // fourth crate (`component/Cargo.toml`, the seed-once root, the guest glue) plus the emitted WIT
    // package — and the `.wit` is the ONE generated artifact that is not rust, so this is where its
    // bytes are pinned at all. Profile-limited for the usual reason: the flag posture is specific.
    (
        "component",
        "tests/component-core/input.cddl",
        ("component", &["--component=true"]),
    ),
    // Type and group CHOICES on the component face: the `resource`-with-no-constructor shape, the
    // `new-<variant>` statics, the `<name>-kind` enum and the `as-<variant>` getters — plus the glue
    // whose `match` arms bridge them to the rust DATA enum. Pinned as bytes because the arm spelling
    // is what the encoding posture re-writes and what every WIT gate is blind to.
    (
        "component_choices",
        "tests/component-choices/input.cddl",
        ("component", &["--component=true"]),
    ),
    // The multi-INTERFACE shape (a directory input, one interface per file): the cross-interface
    // `use` edge in the WIT and, on the glue side, two `Guest` impls on one guest type under one
    // `export!`. None of it is reachable from a single-scope input.
    (
        "component_multifile",
        "tests/component-multifile/inputs",
        ("component", &["--component=true"]),
    ),
    // The BRIDGING classes on the component face: an extern's cbor seam, a raw-bytes type's RAW
    // seam, a generic extern base skipped from the projection entirely beside its bridged instance,
    // and a monomorphized non-extern generic instance. Pinned as bytes for what compilation cannot
    // judge: a bridge's WIT reads identically whether it names the right trait or the wrong one, and
    // what is EXCLUDED (the generic base, which names no concrete type) is a decision rather than a
    // build outcome. The build smoke compiles this fixture too, pairing it with the hand-written
    // extern definitions a real consumer supplies — the two halves ask different questions of it
    // rather than one standing in for the other.
    (
        "component_extern",
        "tests/component-extern/inputs",
        ("component", &["--component=true"]),
    ),
    // The two flag-gated SEAMS on the component face, in the one posture that carries both:
    // `to-canonical-cbor-bytes` (projected only where the composed runtime puts it on `Serialize`)
    // and the JSON pair — per-resource `to-json`/`from-json` plus the `cbor-to-json` /
    // `cbor-from-json` free functions on the `any-cbor` alias. Pinned as bytes because this row is
    // also the only place the component manifest's conditional `serde_json` dependency is checked
    // against emitted output rather than against the op list.
    (
        "component_json",
        "tests/component-core/input.cddl",
        (
            "component",
            &[
                "--component=true",
                "--preserve-encodings=true",
                "--canonical-form=true",
                "--json-serde-derives=true",
            ],
        ),
    ),
];

/// One tiny CDDL file per language construct → a localized snapshot per feature, across every
/// flag profile.
fn feature_corpus_entries() -> Vec<std::path::PathBuf> {
    let corpus_dir = std::path::Path::new("tests/corpus");
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(
        !entries.is_empty(),
        "no corpus files found in {:?}",
        corpus_dir
    );
    entries
}

/// The stale-pin half of [`feature_corpus`], which is only correct when ONE test sees EVERY corpus
/// fixture: a shard walking a slice cannot tell "this pin names a fixture that was deleted" from
/// "this pin names a fixture another shard owns", so leaving it in the shards would make it vacuous
/// while the suite stayed green. Reads the corpus directory and nothing else.
#[test]
fn feature_corpus_pins_are_live() {
    let corpus_stems: std::collections::BTreeSet<String> = feature_corpus_entries()
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap().to_owned())
        .collect();
    for (stem, _profile) in PROFILE_GENERATION_SKIP {
        assert!(
            corpus_stems.contains(*stem),
            "PROFILE_GENERATION_SKIP names corpus fixture `{stem}` that no longer exists in \
             tests/corpus — stale pin, remove or fix it"
        );
    }
}

/// How many `#[test]`s the corpus snapshot sweep is split across. Like the parity sweep it is pure
/// in-process generation plus `insta` file comparison — no cargo, no scratch dir, no lock — so
/// libtest's thread pool absorbs the cells directly.
const FEATURE_CORPUS_SNAPSHOT_SHARDS: usize = 8;

macro_rules! feature_corpus_shards {
    ($($name:ident = $shard:expr;)+) => {
        $(
            #[test]
            fn $name() {
                feature_corpus_shard($shard);
            }
        )+
    };
}

feature_corpus_shards! {
    feature_corpus_shard_0 = 0;
    feature_corpus_shard_1 = 1;
    feature_corpus_shard_2 = 2;
    feature_corpus_shard_3 = 3;
    feature_corpus_shard_4 = 4;
    feature_corpus_shard_5 = 5;
    feature_corpus_shard_6 = 6;
    feature_corpus_shard_7 = 7;
}

/// One slice of the corpus. `insta` asserts per snapshot and panics on the first mismatch, so — unlike
/// the batching compile gates — this sweep never reported more than one failure per run anyway; the
/// split strictly widens that (one failure per shard).
fn feature_corpus_shard(shard: usize) {
    let all_entries = feature_corpus_entries();
    // Round-robin over the SORTED list, so which fixture lands in which shard is deterministic.
    let entries: Vec<std::path::PathBuf> = all_entries
        .into_iter()
        .enumerate()
        .filter(|(i, _)| i % FEATURE_CORPUS_SNAPSHOT_SHARDS == shard)
        .map(|(_, p)| p)
        .collect();
    for path in entries {
        let label = path.file_stem().unwrap().to_str().unwrap().to_owned();
        assert!(
            !WHOLE_PROGRAM_CASES
                .iter()
                .any(|(whole_program_label, _, _)| *whole_program_label == label),
            "corpus file {:?} collides with a whole_program snapshot dir; rename it",
            path
        );
        // A fixture whose generation aborts under some profile (float + preserve) is snapshotted
        // only under the profiles it generates in.
        let profiles: Vec<Profile> = ALL_PROFILES
            .iter()
            .filter(|(profile, _)| !PROFILE_GENERATION_SKIP.contains(&(label.as_str(), *profile)))
            .copied()
            .collect();
        snapshot_input(
            &path,
            &label,
            &profiles,
            false,
            true,
            Some(super::COMPONENT_PROFILE),
        );
    }
}

/// Generation must be deterministic: same input + flags → byte-identical output every run.
/// Emission is all-`BTreeMap`/`BTreeSet` (never `HashMap`) precisely so output can't depend on
/// hash iteration order. This guards against a future `HashMap` silently reintroducing
/// nondeterminism — which would otherwise surface only as intermittently flaky snapshots. Two
/// runs in one process suffice: each `HashMap` instance gets a fresh random seed, so a hash-order
/// dependency would diverge between the runs. Rich whole-program inputs across the three emission
/// profiles (default/preserve/json) exercise the flag-gated emission paths too.
#[test]
fn generation_is_deterministic() {
    let cases: &[(&str, &[&str])] = &[
        ("tests/core/input.cddl", &[]),
        (
            "tests/preserve-encodings/input.cddl",
            &["--preserve-encodings=true"],
        ),
        (
            "tests/json/input.cddl",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
        // directory input: multi-file enumeration + scope/module emission. The in-process double-run
        // can't vary filesystem enumeration order, so order-independence itself is guaranteed by
        // construction (cddl_paths sorts); this case guards the rest of the multi-file pipeline.
        ("tests/multifile/inputs", &[]),
    ];
    for (input, extra) in cases {
        let cli = cli_for(std::path::Path::new(input), extra);
        let first = crate::api::generated_strings(&cli).unwrap();
        let second = crate::api::generated_strings(&cli).unwrap();
        assert_eq!(
            first, second,
            "nondeterministic generation for {input} — a HashMap on the IR/emission path?"
        );
    }
}

/// The existing integration inputs, each under the flag profile it is known-safe with (the same
/// pairings the heavy integration tests use). Captures the full output to cover cross-feature
/// interactions, the multifile scope/module path, and the edition/deps Cargo.toml logic.
#[test]
fn whole_program() {
    for (label, input, profile) in WHOLE_PROGRAM_CASES {
        snapshot_input(
            &std::path::PathBuf::from(input),
            label,
            std::slice::from_ref(profile),
            true,
            false,
            // The component rows here pin the FULL tree on purpose: this suite is where the
            // component `Cargo.toml` and the seed-once root are pinned at all, and the
            // `component_json` row's extra flags make its rust/wasm halves legitimately differ
            // from `default`'s.
            None,
        );
    }
}

/// Content pin for the json-gen extern schema-row fix (feature request 05). Drives the generator
/// in-process (no compile) over `tests/json-extern-rows/inputs` — a directory fixture combining a
/// plain extern, a generic extern + concrete instance, a dep-owned (extern-deps-dir) extern, and an
/// in-crate SCOPED type, plus a `@no_json_schema_export`-annotated extern and ordinary rule — and
/// pins exactly which registration rows the json-gen crate's `add_schemas` emits.
/// The full compile proof (that the KEPT rows build against hand-written `schemars::JsonSchema`
/// impls, and the generic-base row's removal fixes an E0107) lives in
/// `integration_tests::json_extern` and `integration_tests::multifile_json_preserve`; this is the
/// cheap fast-tier net for the emitter's row selection.
#[test]
fn json_gen_extern_schema_rows() {
    let cli = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &["--json-serde-derives=true", "--json-schema-export=true"],
    );
    let files = crate::api::generated_strings(&cli).expect("generation must succeed");
    let mod_rs = files
        .get("wasm/json-gen/src/generated/mod.rs")
        .expect("json-gen generated/mod.rs must be emitted under --json-schema-export");

    // KEPT rows: plain extern, concrete generic instance, in-crate root type, and the scoped
    // in-crate type at its REAL module path (the thin root's `pub use generated::*` makes it valid).
    for kept in [
        "reg.add::<cddl_lib::MyExtern>();",
        "reg.add::<cddl_lib::MySet>();",
        "reg.add::<cddl_lib::BigThing>();",
        "reg.add::<cddl_lib::sub::module::ScopedThing>();",
        // A spliced PLAIN GROUP rule does get a row — the unannotated control for `QuietGroup`
        // below, without which "no `QuietGroup` row" would be vacuously true.
        "reg.add::<cddl_lib::LoudGroup>();",
    ] {
        assert!(
            mod_rs.contains(kept),
            "expected json-gen row `{kept}` missing:\n{mod_rs}"
        );
    }

    // SKIPPED: the generic-extern BASE (`ExtSet` names no concrete type — E0107 no matter what the
    // user writes).
    assert!(
        !mod_rs.contains("reg.add::<cddl_lib::ExtSet>"),
        "generic-extern base row must be skipped:\n{mod_rs}"
    );

    // SKIPPED: a generic-extern base with ZERO instances (`ext_unused<T>`). `generic_instance_bases`
    // derives bases from instances and is blind to this one, so the skip must key on the parse-time
    // `generic_extern_bases` record.
    assert!(
        !mod_rs.contains("ExtUnused"),
        "a never-instantiated generic-extern base must still be skipped:\n{mod_rs}"
    );

    // SKIPPED: dep-owned types (non-export scope). Their emitted path would be `dep_crate::…`, which
    // this json-gen crate's manifest does not (and must not) depend on — the dep's own json-gen run
    // owns those schemas.
    assert!(
        !mod_rs.contains("dep_crate"),
        "dep-owned (non-export scope) rows must be skipped:\n{mod_rs}"
    );

    // SKIPPED: `@no_json_schema_export` — the spec author's declaration that a type is not part of
    // the published JSON surface. Pinned on all THREE rule kinds, because they reach the flag by
    // three different routes through `parsing.rs` and a missed route is a SILENT drop: an EXTERN
    // rule (`RustStruct::new_extern` drops rule metadata into `RustStructConfig::default()`, so only
    // the `IntermediateTypes` marker set can carry it), an ORDINARY type rule (`parse_type` /
    // `parse_type_choices`), and a PLAIN GROUP rule (`parse_rule`'s own `Rule::Group` arm, which
    // reaches neither of those and therefore needs its own marking site — it shipped silently
    // dropping the directive until this vector caught it). Each has an unannotated same-shape twin
    // asserted KEPT above (`MyExtern`, `BigThing`, `LoudGroup`), so this cannot pass by suppressing
    // everything. Keep the shapes paired: a new rule kind that reaches the directive by a new route
    // belongs here as a twin pair, not as a lone negative.
    for suppressed in ["QuietExtern", "QuietThing", "QuietGroup"] {
        assert!(
            !mod_rs.contains(suppressed),
            "@no_json_schema_export row for `{suppressed}` must be skipped:\n{mod_rs}"
        );
    }

    // The name-injectivity guard's WIRING (the checks themselves are proven end to end by
    // `integration_tests::json_schema_name_merge_fails` / `..._stolen_fails`, which run a json-gen
    // crate and assert it panics — a local-tier cost). A guard that silently stops being threaded
    // through the rows would leave both those fixtures failing for a DIFFERENT reason, so pin the
    // three pieces that carry it here, in the fast tier: the registrar local (which OWNS the
    // published-name ledger), the IMPORT of the registrar from the common runtime crate, and the
    // `reg.add::<T>()` row call (asserted above with the KEPT rows). The guard's BODY no longer
    // lives in this file — it is `static/json_schema_gen.rs`, hosted once per common crate and
    // compiled/unit-tested in-crate by `json_schema_gen_tests` — so the import is what proves the
    // rows still reach it.
    for wiring in [
        "let mut reg = Registrar::new(generator);",
        "use cddl_lib::json_schema_gen::Registrar;",
        "use cddl_lib::json_schema_gen::check_schema_ref_closure;",
    ] {
        assert!(
            mod_rs.contains(wiring),
            "schema-name guard wiring `{wiring}` missing from the emitted json-gen crate:\n{mod_rs}"
        );
    }
    // …and the bodies must NOT be re-emitted here: hosting them once per common crate is the whole
    // point, so an inlined copy is the regression. One fragment unique to each body stands in for it.
    for inlined in [
        "fn add_schema<T: schemars::JsonSchema>(",
        "struct Registrar<",
        "fn collect_schema_refs(",
    ] {
        assert!(
            !mod_rs.contains(inlined),
            "`{inlined}` must come from the common crate's `json_schema_gen` module, not be inlined \
             into every json-gen crate:\n{mod_rs}"
        );
    }

    // `--json-schema-root` extra roots (feature request 12, Ask A): the same fixture regenerated with
    // two extra roots, pinning the emitted SHAPE the compile proof (`integration_tests::json_extern`)
    // cannot see — that a root is emitted VERBATIM (generic arguments and all), that the roots come
    // AFTER every spec-derived row (registration order decides which side of a published-name
    // collision the injectivity guard names, and blaming the CLI-supplied path is the actionable
    // one), and that FLAG ORDER is preserved rather than sorted (`Zeta` before `Alpha`).
    let with_roots = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &[
            "--json-serde-derives=true",
            "--json-schema-export=true",
            "--json-schema-root=other_crate::Zeta",
            "--json-schema-root=cddl_lib::Alpha<u64>",
        ],
    );
    let root_mod_rs = crate::api::generated_strings(&with_roots)
        .expect("generation must succeed")
        .get("wasm/json-gen/src/generated/mod.rs")
        .expect("json-gen generated/mod.rs must be emitted under --json-schema-export")
        .clone();
    let zeta = root_mod_rs
        .find("reg.add::<other_crate::Zeta>();")
        .unwrap_or_else(|| panic!("extra root row missing verbatim:\n{root_mod_rs}"));
    let alpha = root_mod_rs
        .find("reg.add::<cddl_lib::Alpha<u64>>();")
        .unwrap_or_else(|| {
            panic!("extra root row with generic arguments missing verbatim:\n{root_mod_rs}")
        });
    let last_spec_row = root_mod_rs
        .find("reg.add::<cddl_lib::sub::module::ScopedThing>();")
        .unwrap_or_else(|| panic!("spec-derived rows missing:\n{root_mod_rs}"));
    assert!(
        last_spec_row < zeta && zeta < alpha,
        "extra roots must follow every spec-derived row, in flag order (never sorted):\n{root_mod_rs}"
    );

    // `--json-schema-dep` registrar calls: the same fixture regenerated with two mappings, pinning
    // the OPPOSITE end of the same ordering contract the roots block above pins. One label is
    // `dep_crate` — the fixture's real extern dep, whose `dep_thing` row is skipped above because the
    // dep owns it — so the cell reads as the intended story: the dep's rows are skipped locally and
    // threaded in from the dep's own crate. The second mapping is spelled with dashes, pinning the
    // cargo-package-name normalisation.
    //
    // Deps FIRST is the mirror of roots LAST: a dep's names are already shipped in the dep's package,
    // so on a cross-crate collision the consumer's row is the one that should be renamed and blamed.
    let with_deps = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &[
            "--json-serde-derives=true",
            "--json-schema-export=true",
            "--json-schema-root=other_crate::Zeta",
            "--json-schema-dep=dep_crate=zeta_dep_json_schema_gen",
            "--json-schema-dep=other_dep=alpha-dep-json-schema-gen",
        ],
    );
    let dep_mod_rs = crate::api::generated_strings(&with_deps)
        .expect("generation must succeed")
        .get("wasm/json-gen/src/generated/mod.rs")
        .expect("json-gen generated/mod.rs must be emitted under --json-schema-export")
        .clone();
    let zeta_dep = dep_mod_rs
        .find("zeta_dep_json_schema_gen::add_schemas(generator);")
        .unwrap_or_else(|| panic!("dep registrar call missing verbatim:\n{dep_mod_rs}"));
    let alpha_dep = dep_mod_rs
        .find("alpha_dep_json_schema_gen::add_schemas(generator);")
        .unwrap_or_else(|| {
            panic!("dep registrar call with a dashed cargo package name must be normalised to underscores:\n{dep_mod_rs}")
        });
    // The registrar local, which opens `add_schemas` whenever this crate has rows of its own. Its
    // position is doubly load-bearing now: `Registrar::new` takes the generator's `&mut` borrow, so
    // a dep call emitted after it would not compile.
    let registrar = dep_mod_rs
        .find("let mut reg = Registrar::new(generator);")
        .unwrap_or_else(|| panic!("the registrar local is missing:\n{dep_mod_rs}"));
    // The first spec-derived row. `reg.add::<` cannot match the `use …::Registrar;` import above it,
    // so the position is genuinely the first ROW.
    let first_row = dep_mod_rs
        .find("reg.add::<")
        .unwrap_or_else(|| panic!("spec-derived rows missing:\n{dep_mod_rs}"));
    assert!(
        zeta_dep < alpha_dep,
        "dep registrar calls must be emitted in flag order (never sorted):\n{dep_mod_rs}"
    );
    assert!(
        alpha_dep < registrar && alpha_dep < first_row,
        "dep registrar calls must precede the registrar local and every row of this crate's own:\n{dep_mod_rs}"
    );
}

/// A bounded homogeneous array is already a restricted `BoundedVec` by the time a record or enum
/// constructor receives it. The fallible door is therefore `BoundedVec::try_from`, never the outer
/// constructor. Keep that ownership true across every rust-test-emitting profile, and keep the
/// nested-container renderer routing an inner bounded array through the same door.
#[test]
fn bounded_array_emit_test_probes_follow_the_fallible_door() {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_bounded_array_emit_test_probe_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let input = root.join("input.cddl");
    std::fs::write(
        &input,
        "bounded_record = [values: [2*5 uint]]\n\
         bounded_choice = [2*5 uint] / tstr\n\
         bounded_arm = (code: uint, values: [2*5 uint])\n\
         label_arm = (label: tstr)\n\
         bounded_group_choice = [bounded_arm // label_arm]\n\
         nested_choice = [* [2*5 uint]] / bool\n",
    )
    .unwrap();

    for (profile, flags) in [
        ("default", vec!["--emit-tests=true", "--wasm=false"]),
        (
            "preserve",
            vec![
                "--emit-tests=true",
                "--preserve-encodings=true",
                "--wasm=false",
            ],
        ),
        (
            "json",
            vec![
                "--emit-tests=true",
                "--json-serde-derives=true",
                "--json-schema-export=true",
                "--wasm=false",
            ],
        ),
        ("wasm", vec!["--emit-tests=true", "--wasm=true"]),
    ] {
        let files = crate::api::generated_strings(&cli_for(&input, &flags))
            .unwrap_or_else(|e| panic!("{profile} generation failed: {e}"));
        let rust = files
            .get("rust/src/generated/mod.rs")
            .unwrap_or_else(|| panic!("{profile} omitted the generated rust root"));
        let compact: String = rust.split_whitespace().collect();

        assert!(
            compact.contains(
                "usecrate::generated::error::DeserializeFailureas__CddlTestDeserializeFailure;"
            ),
            "{profile} reject probes must own their error import inside the test module:\n{rust}"
        );

        for (owner, ctor) in [
            ("record", "BoundedRecord::new(__bounded_arg)"),
            ("type choice", "BoundedChoice::new_arr_u64(__bounded_arg)"),
            (
                "group choice",
                "BoundedGroupChoice::new_bounded_arm(0,__bounded_arg)",
            ),
        ] {
            assert!(
                compact
                    .contains("let__bounded_arg=BoundedVec::<_,2,5>::try_from(vec![0;2]).expect(")
                    && compact.contains(&format!("let_={ctor};")),
                "{profile} {owner} accept probe must cross the bounded argument's TryFrom door, then pass the checked value to the infallible outer constructor:\n{rust}"
            );
        }
        assert!(
            compact.contains("BoundedVec::<_,2,5>::try_from(vec![0;1]).unwrap_err().failure()")
                && compact
                    .contains("BoundedVec::<_,2,5>::try_from(vec![0;6]).unwrap_err().failure()"),
            "{profile} reject probes must observe RangeCheck at the BoundedVec door:\n{rust}"
        );
        for bad_owner in [
            "BoundedChoice::new_arr_u64(BoundedVec::<_,2,5>::try_from",
            "BoundedGroupChoice::new_bounded_arm(0,BoundedVec::<_,2,5>::try_from",
        ] {
            assert!(
                !compact.contains(&format!("{bad_owner}vec![0;2]).unwrap()).is_ok()")),
                "{profile} must not attach Result assertions to an infallible enum constructor:\n{rust}"
            );
        }
        assert!(
            compact.contains(
                "NestedChoice::new_arr_arr_u64(vec![BoundedVec::<_,2,5>::try_from(vec![0;2]).unwrap();1])"
            ),
            "{profile} nested bounded-array values must still cross their inner TryFrom door:\n{rust}"
        );
    }

    let _ = std::fs::remove_dir_all(root);
}

/// Byte-stability of the json-gen crate root across regenerations. The schema document is built by
/// threading ONE `schemars::SchemaGenerator` through the rows in the order `add_schemas` emits them,
/// and schemars assigns its collision suffixes (`{base}{i}`) from a per-generator name set in
/// first-encounter order — so the row order is now load-bearing for the CONTENT of the shipped
/// document, not just for the diff of the file. A `HashMap` anywhere on the path from the IR to the
/// rows would make two runs of the same spec publish two different documents.
#[test]
fn json_gen_rows_are_byte_stable() {
    let cli = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &["--json-serde-derives=true", "--json-schema-export=true"],
    );
    let path = "wasm/json-gen/src/generated/mod.rs";
    let first = crate::api::generated_strings(&cli).expect("generation must succeed");
    let second = crate::api::generated_strings(&cli).expect("generation must succeed");
    assert_eq!(
        first.get(path),
        second.get(path),
        "two generations of the same spec must emit a byte-identical {path}"
    );
}

/// Regression pin for feature request 07 (commit `08bc1d9` "scope_references walks type_aliases"):
/// a generic-EXTERN instance named by a rule in a NON-root scope (`required_signers = ext_set<pub_key>`
/// / `my_set = ext_set<plain>` in the `transaction` scope) registers a type alias whose base is a
/// `Base<Args>` TYPE EXPRESSION (`ExtSetRawBytes<PubKey>` / `ExtSet<Plain>`). Before the fix the
/// alias walk fed that opaque ident through `set_ref`, landing the whole `<…>`-carrying text verbatim
/// in the scope's `use crate::generated::{…}` list — invalid Rust, so the rustfmt post-pass aborted
/// generation. This pins the corrected shape directly (fast, no nested cargo); the compile proof is
/// `integration_tests::extern_generic_scoped`.
///
/// Not keyed on `@raw_bytes_flavor`: the plain instance (`my_set = ext_set<plain>`) breaks
/// identically, so both the flavored (`ExtSetRawBytes<PubKey>`) and plain (`ExtSet<Plain>`) shapes
/// are asserted. The base extern lives in the NON-root `crypto` scope, so this also pins that the
/// base import routes to the base's declaring scope (the re-export glue's `pub use crate::…` site),
/// not the default-to-root `set_ref` would misroute to.
#[test]
fn extern_generic_scoped_alias_imports() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-generic-scoped/inputs"),
        &["--wasm=false"],
    );
    let files = crate::api::generated_strings(&cli).expect("generation must succeed");
    let tx = files
        .get("rust/src/generated/transaction/mod.rs")
        .expect("transaction scope module must be emitted");

    // The alias lines render the `Base<Args>` type expression bare in the transaction module.
    for alias in [
        "pub type RequiredSigners = ExtSetRawBytes<PubKey>;",
        "pub type MySet = ExtSet<Plain>;",
    ] {
        assert!(
            tx.contains(alias),
            "expected alias line `{alias}` missing:\n{tx}"
        );
    }

    // Both alias bases and both argument types must be imported — decomposed from the opaque
    // `Base<Args>` ident, never emitted whole. Assertions are robust to brace-grouping: they check
    // that SOME `use` line carries the right path prefix and names the ident.
    let use_lines: Vec<&str> = tx
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("use "))
        .collect();
    let imported_from = |prefix: &str, ident: &str| {
        use_lines
            .iter()
            .any(|l| l.starts_with(prefix) && l.contains(ident))
    };
    // The base externs (`ExtSet`/`ExtSetRawBytes`) and the raw-bytes arg (`PubKey`) all live in the
    // non-root `crypto` scope — imported from there, not root.
    for ident in ["ExtSet", "ExtSetRawBytes", "PubKey"] {
        assert!(
            imported_from("use crate::generated::crypto::", ident),
            "`{ident}` must be imported from the crypto scope:\n{tx}"
        );
    }
    // The plain arg (`plain`) is a ROOT-scope record referenced ONLY as a generic argument — its
    // import is the args-import half of the fix (dropping it would dangle E0412). Root path is
    // `crate::generated::` with the ident/`{` immediately after (never the `crypto::` submodule).
    assert!(
        use_lines.iter().any(|l| {
            l.starts_with("use crate::generated::")
                && !l.starts_with("use crate::generated::crypto::")
                && l.contains("Plain")
        }),
        "`Plain` must be imported from the root scope:\n{tx}"
    );

    // Class-level invariant: a decomposed importer NEVER lets a `<…>` type expression reach a `use`
    // line — in the transaction scope or the root generated module.
    let root = files
        .get("rust/src/generated/mod.rs")
        .expect("root generated module must be emitted");
    for (name, content) in [("transaction/mod.rs", tx), ("generated/mod.rs", root)] {
        for line in content
            .lines()
            .map(str::trim)
            .filter(|l| l.starts_with("use "))
        {
            assert!(
                !line.contains('<'),
                "a `use` line in {name} carries a `<…>` type expression (invalid Rust):\n{line}"
            );
        }
    }
}

/// The static serialization runtime prelude ships verbatim into every generated crate but is
/// assembled differently per flag combination. It's excluded from the per-feature snapshots (it's
/// feature-independent and would be pure repeated noise), so snapshot it once per combination here
/// — this is the fast net for changes to the `static/serialization*.rs` runtime.
#[test]
fn serialization_prelude() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_serialization_prelude");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    // serialization_prelude only reads --static-dir; the input is irrelevant but clap needs one.
    // Both spec-conditional fragments are off here (`raw_bytes_encoding` and the canonical-hex
    // door): this suite covers the FLAG dimension, and those two vary by spec, not by flag — they
    // are exercised end-to-end by the `raw-bytes` / `raw-bytes-preserve` integration fixtures.
    let dummy = std::path::Path::new("tests/corpus/primitives.cddl");
    settings.bind(|| {
        for (name, extra) in [
            ("default", &[][..]),
            ("preserve", &["--preserve-encodings=true"][..]),
            (
                "canonical",
                &["--preserve-encodings=true", "--canonical-form=true"][..],
            ),
        ] {
            let cli = cli_for(dummy, extra);
            let prelude =
                crate::generation::GenerationScope::serialization_prelude(false, false, &cli)
                    .unwrap_or_else(|e| panic!("prelude failed for {}: {}", name, e));
            insta::assert_snapshot!(name, prelude);
        }
    });
}

/// The generated rust `Cargo.toml`'s dependency set is driven by conditional logic: the edition,
/// flag-deps (serde/schemars/hashlink/derivative), and *type*-conditional deps — `hex` for
/// byte wrappers, `wasm-bindgen` for c-style enums. The per-feature corpus skips `Cargo.toml` as
/// near-constant noise, but the distinct dep *combinations* are not all produced by
/// [`whole_program`]. So cover them here with a curated matrix that toggles each type-conditional
/// dep independently × each profile — the fast net for the deps/edition logic.
#[test]
fn cargo_toml_matrix() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_cargo_toml");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    // inputs chosen so `hex` and `wasm-bindgen` toggle independently (core, which has both, is
    // covered by whole_program):
    let inputs = [
        ("base", "tests/corpus/map_struct.cddl"),   // neither
        ("hex", "tests/corpus/bounded_bytes.cddl"), // byte wrapper -> hex
        ("wasm_bindgen", "tests/corpus/c_style_enum.cddl"), // c-style enum -> wasm-bindgen
    ];
    settings.bind(|| {
        for (label, input) in inputs {
            // The component profile is filtered out by name: this gate's subject is
            // `rust/Cargo.toml`'s conditional deps, and `--component` provably does not touch them
            // (asserted per corpus fixture by [`assert_component_face_is_additive`]), so a
            // component column would re-pin the `default` column's bytes. The component crate's own
            // manifest is pinned by
            // `component_tests::component_generated_files_carry_the_whole_crate` and by the
            // whole-program `component` snapshot dir.
            for (profile, extra) in ALL_PROFILES
                .iter()
                .filter(|(profile, _)| *profile != super::COMPONENT_PROFILE)
            {
                let cli = cli_for(std::path::Path::new(input), extra);
                let files = crate::api::generated_strings(&cli).unwrap_or_else(|e| {
                    panic!("generation failed for {}/{}: {}", label, profile, e)
                });
                let toml = files
                    .get("rust/Cargo.toml")
                    .unwrap_or_else(|| panic!("no rust/Cargo.toml for {}/{}", label, profile));

                // Each conditional dep is set-or-REMOVE, never set-or-skip: assert every one is
                // present exactly when its flag/type condition holds and ABSENT otherwise. A
                // snapshot alone only pins the bytes; this makes the absence contract a first-class
                // assertion (so a regression that stops emitting the `Remove` op — stranding a
                // stale dep on a flag flip — fails here, not just silently in a diff).
                let has = |key: &str| {
                    toml.lines()
                        .any(|l| l.trim_start().starts_with(&format!("{key} =")))
                };
                let preserve = extra.contains(&"--preserve-encodings=true");
                let serde = extra.contains(&"--json-serde-derives=true");
                let schema = extra.contains(&"--json-schema-export=true");
                // matrix inputs isolate the type-conditional deps to their labels; wasm defaults on.
                let hex = label == "hex";
                let wasm_bindgen = label == "wasm_bindgen";
                for (key, expected) in [
                    ("hashlink", preserve),
                    // The backing crate `OrderedHashMap` used before hashlink: the tool tombstones
                    // it unconditionally, so no flag combination may bring it back.
                    ("linked-hash-map", false),
                    ("derivative", preserve),
                    ("serde", serde),
                    ("serde_json", serde),
                    ("schemars", schema),
                    ("hex", hex),
                    ("wasm-bindgen", wasm_bindgen),
                ] {
                    assert_eq!(
                        has(key),
                        expected,
                        "conditional dep `{key}` presence wrong for {label}/{profile} \
                         (expected present={expected})\n{toml}"
                    );
                }
                // cbor_event is unconditional; sanity-check the presence helper isn't vacuous.
                assert!(has("cbor_event"), "cbor_event must always be present");

                insta::assert_snapshot!(format!("{}__{}", label, profile), toml);
            }
        }
    });
}

/// Drift gate for the derived `static/Cargo_{rust,wasm,json_gen,static_runtime}.toml` templates.
///
/// The templates are generated snapshots of each manifest's append-only change log
/// (`static/manifest_changes/*.toml`), rendered by the SAME fold+apply the runtime uses — so the
/// committed template is guaranteed to mean exactly what the log means. This pins them byte-for-byte:
/// append a log entry (or edit the header) and forget to regenerate, and this fails with the bless
/// command. `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift` rewrites them.
#[test]
fn manifest_template_drift() {
    let static_dir = std::env::current_dir().unwrap().join("static");
    let bless = std::env::var("BLESS_MANIFEST_TEMPLATES")
        .map(|v| v == "1")
        .unwrap_or(false);
    let cases = [
        ("manifest_changes/rust.toml", "Cargo_rust.toml"),
        ("manifest_changes/wasm.toml", "Cargo_wasm.toml"),
        ("manifest_changes/json_gen.toml", "Cargo_json_gen.toml"),
        ("manifest_changes/component.toml", "Cargo_component.toml"),
        (
            "manifest_changes/static_runtime.toml",
            "Cargo_static_runtime.toml",
        ),
    ];
    let mut stale = Vec::new();
    for (log_name, template_name) in cases {
        let raw = std::fs::read_to_string(static_dir.join(log_name)).unwrap();
        let rendered =
            crate::cargo_manifest::render_derived_template(&raw, log_name, template_name).unwrap();
        let template_path = static_dir.join(template_name);
        let committed = std::fs::read_to_string(&template_path).unwrap_or_default();
        if rendered != committed {
            if bless {
                std::fs::write(&template_path, &rendered).unwrap();
            } else {
                stale.push(template_name);
            }
        }
    }
    assert!(
        stale.is_empty(),
        "derived manifest template(s) {stale:?} are stale vs their change logs; regenerate with \
         `BLESS_MANIFEST_TEMPLATES=1 cargo test manifest_template_drift`"
    );
}

/// The `--wasm-list-macro` flag: each `Vec<T>`-backed list wrapper collapses to a single
/// `impl_wasm_list!(rust_elem, wasm_elem, WasmName, needs_into, is_copy)` invocation (plus the `use`
/// import) in place of the inline struct + accessor + conversion block. The fixture covers every
/// reachable `(needs_into, is_copy)` combination. The flag-*off* output is the inline form the rest
/// of the suite already snapshots (and `whole_program`/`generation_is_deterministic` guard that the
/// gated branch leaves it byte-identical), so this only needs the flag-on wasm `generated/mod.rs`.
#[test]
fn wasm_list_macro() {
    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/corpus/snapshots/_wasm_list_macro");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    let input = std::path::Path::new("tests/wasm-list-macro/input.cddl");
    // Generic macro paths — the feature works with any user-supplied macro.
    let cases: &[(&str, &[&str])] = &[
        // list wrappers collapse to impl_wasm_list! invocations
        (
            "list_macro",
            &["--wasm-list-macro=my_crate::impl_wasm_list"],
        ),
        // combined with --wasm-conversions-macro: list wrappers use impl_wasm_list! (which emits
        // their own conversions), while non-list wrappers still use impl_wasm_conversions! — i.e.
        // the list macro supersedes, rather than double-emits with, the conversions macro.
        (
            "list_and_conversions_macro",
            &[
                "--wasm-list-macro=my_crate::impl_wasm_list",
                "--wasm-conversions-macro=my_crate::impl_wasm_conversions",
            ],
        ),
    ];
    settings.bind(|| {
        for (label, extra) in cases {
            let cli = cli_for(input, extra);
            let files = crate::api::generated_strings(&cli).unwrap();
            // The wasm macro invocations live in the generated root scope (`generated/mod.rs`), not
            // the thin seed-once `lib.rs` (which carries only `mod generated; pub use generated::*;`).
            let lib = files
                .get("wasm/src/generated/mod.rs")
                .expect("no wasm/src/generated/mod.rs generated");
            insta::assert_snapshot!(format!("{label}__wasm__src__generated__mod.rs"), lib);
        }
    });
}

/// The two banner lines, derived from the SAME constant the stamper prepends
/// (`generation::CODEGEN_HEADER`) so the gate can't drift from what generation actually emits.
/// Likewise the path family below reuses `generation::is_header_stamped_path` — the stamping is
/// file-level in `generated_files`, and this gate asserts over the identical file set.
fn codegen_header_lines() -> (&'static str, &'static str) {
    let mut lines = crate::generation::CODEGEN_HEADER.lines();
    (lines.next().unwrap(), lines.next().unwrap())
}
use crate::generation::is_header_stamped_path;

/// Intermediate parent-module link files (e.g. `a/c/mod.rs` containing only `pub mod foo;`) may or
/// may not carry the header depending on how they are produced; a file consisting SOLELY of
/// `pub mod` declarations has no orderable content for this gate to judge, so exclude it
/// structurally: every non-blank line is a `pub mod ...;`. This can never exclude a file where the
/// ordering bug could hide, because any file that contains the header (or any other item) fails
/// the all-`pub mod` predicate and is checked.
fn is_module_link_stub(content: &str) -> bool {
    let mut non_blank = content.lines().filter(|l| !l.trim().is_empty()).peekable();
    non_blank.peek().is_some() && non_blank.all(|l| l.trim_start().starts_with("pub mod "))
}

/// Verify the two header lines are present and contiguous, and that everything before them is
/// benign preamble: blank lines, `//` comments, or crate-level `#![...]` attributes (which rustfmt
/// splits across multiple lines, e.g. `#![allow(\n    clippy::…,\n)]`, so an opened attribute is
/// consumed through its `]` closer). Any other content before the header (a macro invocation, an
/// item, an import) is the failure this test exists to catch.
fn check_header_leads(content: &str) -> Result<(), String> {
    let (header_1, header_2) = codegen_header_lines();
    let lines: Vec<&str> = content.lines().collect();
    let header_idx = lines
        .iter()
        .position(|l| l.trim() == header_1)
        .ok_or_else(|| format!("header line 1 missing entirely ({header_1:?})"))?;
    match lines.get(header_idx + 1) {
        Some(l) if l.trim() == header_2 => {}
        other => {
            return Err(format!(
                "header line 2 ({header_2:?}) does not immediately follow line 1; got {other:?}"
            ));
        }
    }
    let mut i = 0;
    while i < header_idx {
        let line = lines[i];
        let trimmed = line.trim_start();
        if line.trim().is_empty() || trimmed.starts_with("//") {
            i += 1;
        } else if trimmed.starts_with("#![") {
            // consume the (possibly multi-line) crate attribute through its `]` closer
            while i < header_idx && !lines[i].trim_end().ends_with(']') {
                i += 1;
            }
            i += 1;
        } else {
            return Err(format!(
                "line {} precedes the header but is not blank / a `//` comment / a crate `#![…]` \
                 attribute: {line:?}",
                i + 1
            ));
        }
    }
    Ok(())
}

/// Every generated `.rs` in a header-stamped scope family (`rust/src/generated/**`,
/// `wasm/src/generated/**`) must LEAD with the code-generation header — only blank lines, `//`
/// comments, or crate `#![…]` attributes may precede it. This guards the invariant that the
/// codegen provenance banner is the first substantive line a reader (or a downstream license/attrib
/// scanner) sees.
///
/// The header is stamped once per emitted FILE (`generation::stamp_codegen_header`), not per scope,
/// so it leads regardless of what a scope contributes. Two failure classes this guards against, both
/// of which a scope-level header raw would reintroduce (raws hoist to the top of a scope in insertion
/// order):
/// 1. `--wasm-list-macro`: each `impl_wasm_list!` invocation defines a wrapper type and is emitted
///    via `Scope::raw_sorted` so it sorts into its item position, not hoisted above the header.
/// 2. Merged root `generated/mod.rs` files: the module-linking declarations (`pub mod error;`,
///    `extern crate derivative;`, `pub mod <child>;`) come from the lib scope, which
///    `merge_scopes_to_strings` prepends ahead of the ROOT-scope content — file-level stamping puts
///    the header above all of it.
#[test]
fn generated_files_start_with_header() {
    // Representative (label, input, flags) set — reuses the existing whole-program table and the
    // wasm-list-macro fixture + both its profiles (the known-red case).
    let mut cases: Vec<(String, String, Vec<&str>)> = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        cases.push((
            format!("{label}/{profile}"),
            input.to_string(),
            extra.to_vec(),
        ));
    }
    for (label, extra) in [
        (
            "list_macro",
            &["--wasm-list-macro=my_crate::impl_wasm_list"][..],
        ),
        (
            "list_and_conversions_macro",
            &[
                "--wasm-list-macro=my_crate::impl_wasm_list",
                "--wasm-conversions-macro=my_crate::impl_wasm_conversions",
            ][..],
        ),
    ] {
        cases.push((
            format!("wasm_list_macro/{label}"),
            "tests/wasm-list-macro/input.cddl".to_owned(),
            extra.to_vec(),
        ));
    }

    let mut failures = Vec::new();
    for (profile, input, extra) in &cases {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {profile}: {e}"));
        for (path, content) in &files {
            if !is_header_stamped_path(path) || is_module_link_stub(content) {
                continue;
            }
            if let Err(msg) = check_header_leads(content) {
                failures.push(format!("[{profile}] {path}: {msg}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) in a header-stamped scope family do not lead with the codegen header:\n{}",
        failures.join("\n")
    );
}

/// A generated error-conversion chain converts a single CBOR read to `DeserializeError` AT MOST
/// ONCE. cbor_event's readers return their own error type; a deserialize path maps it to our
/// `DeserializeError` (`.map_err(Into::<DeserializeError>::into)`) so a downstream `.and_then`
/// closure can return `DeserializeFailure::…into()` with a consistent `E`. Because the conversion is
/// the identity `impl From<T> for T` once `E` is already `DeserializeError`, a second `.map_err` on
/// the same read is a redundant no-op — correct output, but dead code that a reader must puzzle over.
///
/// The failure class this guards: an emission site prepending `convert_err_to_ours` before its
/// `.and_then` WITHOUT checking whether an earlier stage of the same chain (the site's own
/// `error_convert`, or a width guard) already converted the error type. `width_reject` shows the
/// correct pattern — it threads a `converted` flag and emits the conversion only when nothing
/// upstream did; every conversion site owes the same check.
#[test]
fn deserialize_converts_error_at_most_once() {
    // Reuse the whole-program representative set (same table the header gate walks).
    const DOUBLED: &str =
        ".map_err(Into::<DeserializeError>::into).map_err(Into::<DeserializeError>::into)";
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            // rustfmt splits the chain across lines; compare with all whitespace stripped.
            let flat: String = content.chars().filter(|c| !c.is_whitespace()).collect();
            if flat.contains(DOUBLED) {
                failures.push(format!("[{label}/{profile}] {path}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) convert a read's error to DeserializeError more than once (redundant \
         .map_err doubling):\n{}",
        failures.join("\n")
    );
}

/// A generated `Ok` pattern parenthesizes its payload only when it is a real tuple, matching the
/// `final_expr` shaping on the expression side (0 exprs → `Ok(())`, 1 → `Ok(x)`, N → `Ok((a, b))`).
/// `Ok((x))` with a single bare identifier is redundant — parens in a pattern are grouping, not a
/// 1-tuple, so it matches the same values as `Ok(x)` but reads as if the payload were a tuple.
///
/// The failure class this guards: an emission site hardcoding the tuple wrapper in the match
/// pattern (`Ok(({}))` around a joined expr list) without conditioning on the list's length. The
/// non-value enum dispatch site (src/generation/enums.rs, `names_without_outer.len() > 1` around line
/// 8879) shows the correct pattern — wrap only for >1 names, empty case emitted separately as
/// `Ok(())`; every pattern-emitting site owes the same length check.
#[test]
fn ok_pattern_parenthesizes_only_tuples() {
    // Hand-rolled scan (no regex dep): find `Ok((`, then a nonempty run of identifier chars
    // immediately followed by `))` — i.e. `Ok((ident))`. `Ok(())` (empty) and `Ok((a, b))`
    // (comma before the close) don't match.
    fn find_redundant_ok_parens(flat: &str) -> Option<&str> {
        const NEEDLE: &str = "Ok((";
        let bytes = flat.as_bytes();
        let mut from = 0;
        while let Some(pos) = flat[from..].find(NEEDLE).map(|i| from + i) {
            let inner = pos + NEEDLE.len();
            let ident_len = bytes[inner..]
                .iter()
                .take_while(|c| c.is_ascii_alphanumeric() || **c == b'_')
                .count();
            if ident_len > 0 && flat[inner + ident_len..].starts_with("))") {
                return Some(&flat[pos..inner + ident_len + 2]);
            }
            from = inner;
        }
        None
    }
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            // rustfmt may split across lines; compare with all whitespace stripped.
            let flat: String = content.chars().filter(|c| !c.is_whitespace()).collect();
            if let Some(found) = find_redundant_ok_parens(&flat) {
                failures.push(format!("[{label}/{profile}] {path}: {found}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) wrap a single-identifier Ok pattern in redundant tuple parens:\n{}",
        failures.join("\n")
    );
}

/// Text arrays cross the wasm boundary as bare `Vec<String>` — wasm-bindgen supports
/// `Vec<String>` in both parameter and return position (a JS string array), and strings are
/// COPIED at the boundary, so the by-value ownership hazard that justifies struct `*List`
/// wrappers does not apply. Therefore no anonymous text-list wrapper (`TextList`) may be
/// emitted anywhere: not the wasm wrapper class, not the rust-side `pub type` alias that rode
/// along with its registration (e.g. for a text-keyed table's keys type).
///
/// Whole-word scan: `TextList` with no identifier char on either side, so the nested-array
/// wrapper `ArrTextList` (an array of arrays stays non-exposable and KEEPS its wrapper) and
/// user-named idents don't false-positive. NAMED text-array rules (`texts = [* text]`) still
/// generate a wrapper under their own rule-derived ident — governed by the Alias arm's
/// RustStruct check in `directly_wasm_exposable`, not the anonymous Str arm this gate pins.
#[test]
fn no_anonymous_text_list_wrapper() {
    fn contains_word(content: &str, word: &str) -> bool {
        let is_ident = |c: u8| c.is_ascii_alphanumeric() || c == b'_';
        let bytes = content.as_bytes();
        let mut from = 0;
        while let Some(pos) = content[from..].find(word).map(|i| from + i) {
            let before_ok = pos == 0 || !is_ident(bytes[pos - 1]);
            let end = pos + word.len();
            let after_ok = end == bytes.len() || !is_ident(bytes[end]);
            if before_ok && after_ok {
                return true;
            }
            from = pos + 1;
        }
        false
    }
    let mut failures = Vec::new();
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            if contains_word(content, "TextList") {
                failures.push(format!("[{label}/{profile}] {path}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "generated file(s) emit the anonymous TextList wrapper — text arrays must cross the \
         wasm boundary as bare Vec<String>:\n{}",
        failures.join("\n")
    );
}

/// The generated RUST crate may carry `wasm_bindgen` only in the feature-gated `cfg_attr` form
/// (`generate_c_style_enum` — a c-style enum is the one type kind exposed to wasm by re-export
/// rather than a wasm-crate wrapper), so the crate compiles standalone without the optional
/// `wasm-bindgen` dep. Any other appearance in the rust tree — a bare `#[wasm_bindgen…]` from a
/// new emission site or profile — would force the dep on every rust-crate consumer. This is the
/// corpus-wide PLACEMENT half of the invariant; the COMPILE half (a feature-off `cargo check`,
/// unreachable through feature-unified workspace builds) is
/// `integration_tests::rust_wasm_bindgen_feature_gated_crate_compiles_standalone`. The positive
/// control (`gated_seen`) keeps the sweep honest: it fails if no whole_program input emits the
/// gated form at all — the fixture-blind-spot class that once graded the rust crate bindgen-free
/// from a fixture lacking the one construct that emits the attribute.
#[test]
fn rust_tree_wasm_bindgen_only_feature_gated() {
    let mut failures = Vec::new();
    let mut gated_seen = false;
    for (label, input, (profile, extra)) in WHOLE_PROGRAM_CASES {
        let cli = cli_for(std::path::Path::new(input), extra);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for {label}/{profile}: {e}"));
        for (path, content) in &files {
            if !path.starts_with("rust/") {
                continue;
            }
            for line in content.lines() {
                if !line.contains("wasm_bindgen") {
                    continue;
                }
                if line.trim_start().starts_with("#[cfg_attr(feature = ") {
                    gated_seen = true;
                } else {
                    failures.push(format!("[{label}/{profile}] {path}: {line}"));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "ungated wasm_bindgen in the generated rust tree — only the c-style-enum \
         `#[cfg_attr(feature = …, wasm_bindgen::prelude::wasm_bindgen)]` form is sanctioned:\n{}",
        failures.join("\n")
    );
    assert!(
        gated_seen,
        "no whole_program input emitted the feature-gated c-style-enum attribute — the sweep \
         lost its positive control (add a c-style enum to a whole_program input)"
    );
}

// ---- the emitter-overload bare-token lint -------------------------------------------------------

/// The two OVERLOADABLE emitter parameters and the default name each one falls back to. An emitter
/// that reaches a leaf while one of these is overloaded must spell the name through the accessor
/// (`{deserializer_name}` / `{serializer_use}`), never inline the default — a leaf that inlines it
/// reads the OUTER cursor/buffer under a `bytes .cbor` payload and silently mis-frames everything
/// after it.
const OVERLOADED_DEFAULTS: &[(&str, &str)] = &[
    // (axis label, the default token a leaf must never spell inline)
    ("deserializer", "raw"),
    ("serializer", "serializer"),
];

/// Sites where a scoped emitter spells a default name inline and is RIGHT to. Keyed
/// `(file, enclosing fn, exact literal, justification)` — a per-entry justification, never a
/// file-wide exclusion, so a new leaf in an already-listed file still fails.
const OVERLOAD_LINT_ALLOW: &[(&str, &str, &str, &str)] = &[
    (
        "deserialize.rs",
        "deserializer_name",
        "raw",
        "the accessor's OWN default — this literal is what every other site reads through",
    ),
    (
        "serialize.rs",
        "generate_serialize",
        "serializer",
        "the serializer accessor's own default (the `unwrap_or((\"serializer\", ..))` fallback that \
         `serializer_use`/`serializer_pass` are derived from); it has no accessor fn to hide behind \
         because the derivation is inline at the top of `generate_serialize`",
    ),
];

/// The parameter spellings under which an emitter fn RECEIVES an overloadable name, per axis. This
/// is the scoping rule's own vocabulary: [`overload_scoped_literals`] treats a fn taking one of
/// these as in scope for its axis, and [`emitter_overload_lint_scopes_every_name_param`] fails any
/// name-typed `serializ`-identifier parameter spelled some OTHER way, so the two cannot drift.
const DESERIALIZE_NAME_PARAMS: &[&str] = &["deserializer_name"];
const SERIALIZE_NAME_PARAMS: &[&str] = &["serializer_use", "serializer_pass"];

/// One `fn` in an emitter source: its name, the char range its header+body spans, and its parameter
/// list (masked, so a `(` inside an emitted literal cannot close it early).
struct EmitterFn {
    name: String,
    start: usize,
    end: usize,
    params_no_ws: String,
    is_method: bool,
}

/// Split masked Rust source into `fn` regions (nested fns and closures included — regions nest, and
/// a literal is attributed to EVERY fn enclosing it).
fn emitter_fns(masked: &[char]) -> Vec<EmitterFn> {
    fn matching(masked: &[char], open: usize, o: char, c: char) -> usize {
        let mut depth = 0;
        for (k, ch) in masked.iter().enumerate().skip(open) {
            if *ch == o {
                depth += 1;
            } else if *ch == c {
                depth -= 1;
                if depth == 0 {
                    return k;
                }
            }
        }
        masked.len() - 1
    }
    let mut out = Vec::new();
    for i in 0..masked.len() {
        if !(masked[i] == 'f'
            && masked.get(i + 1) == Some(&'n')
            && (i == 0 || !super::identifier_hazard_tests::is_ident_char(masked[i - 1]))
            && masked.get(i + 2) == Some(&' '))
        {
            continue;
        }
        let name = match super::identifier_hazard_tests::ident_at(masked, i + 3) {
            Some(n) => n,
            None => continue,
        };
        // parameter list
        let mut j = i + 3 + name.chars().count();
        while j < masked.len() && masked[j] != '(' && masked[j] != ';' {
            j += 1;
        }
        if j >= masked.len() || masked[j] == ';' {
            continue;
        }
        let params_end = matching(masked, j, '(', ')');
        let params_no_ws: String = masked[j..=params_end]
            .iter()
            .filter(|c| !c.is_whitespace())
            .collect();
        // body (a trait-decl `fn f();` has none)
        let mut k = params_end + 1;
        while k < masked.len() && masked[k] != '{' && masked[k] != ';' {
            k += 1;
        }
        if k >= masked.len() || masked[k] == ';' {
            continue;
        }
        out.push(EmitterFn {
            name,
            start: i,
            end: matching(masked, k, '{', '}'),
            params_no_ws: params_no_ws.clone(),
            is_method: params_no_ws.starts_with("(&self") || params_no_ws.starts_with("(&mutself"),
        });
    }
    out
}

/// The `impl` blocks whose header names a serialization CONFIG type, so a `&self` method inside one
/// is overload-scoped too (that is where the accessors themselves live).
fn config_impl_ranges(masked: &[char]) -> Vec<(String, usize, usize)> {
    let mut out = Vec::new();
    for i in 0..masked.len() {
        if super::identifier_hazard_tests::ident_at(masked, i).as_deref() != Some("impl")
            || (i > 0 && super::identifier_hazard_tests::is_ident_char(masked[i - 1]))
        {
            continue;
        }
        let open = match (i..masked.len()).find(|&j| masked[j] == '{') {
            Some(o) => o,
            None => continue,
        };
        let header: String = masked[i..open].iter().collect();
        let mut depth = 0;
        let mut end = masked.len() - 1;
        for (k, ch) in masked.iter().enumerate().skip(open) {
            if *ch == '{' {
                depth += 1;
            } else if *ch == '}' {
                depth -= 1;
                if depth == 0 {
                    end = k;
                    break;
                }
            }
        }
        out.push((header, i, end));
    }
    out
}

/// `true` if `lit` contains `token` as a whole identifier (so `raw_bytes`, `inner_de` and
/// `serializer_use` never match).
fn contains_bare_token(lit: &str, token: &str) -> bool {
    let chars: Vec<char> = lit.chars().collect();
    let tok: Vec<char> = token.chars().collect();
    chars.windows(tok.len()).enumerate().any(|(at, w)| {
        w == tok.as_slice()
            && (at == 0 || !super::identifier_hazard_tests::is_ident_char(chars[at - 1]))
            && chars
                .get(at + tok.len())
                .is_none_or(|c| !super::identifier_hazard_tests::is_ident_char(*c))
    })
}

/// Every emitted literal that is in scope of an overloadable name, with the axes it is scoped for.
/// Shared by the lint and its own anti-vacuity guard.
fn overload_scoped_literals() -> Vec<(&'static str, String, usize, String, bool, bool)> {
    let mut out = Vec::new();
    for file in super::identifier_hazard_tests::EMITTER_SOURCES {
        let path = format!("{}/src/generation/{file}", env!("CARGO_MANIFEST_DIR"));
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("cannot read emitter source {path}: {e}"));
        let scanned = super::identifier_hazard_tests::scan_rust(&src);
        let masked: Vec<char> = scanned.masked.chars().collect();
        let fns = emitter_fns(&masked);
        let impls = config_impl_ranges(&masked);
        // char index -> 1-based line
        let mut line_of = Vec::with_capacity(masked.len() + 1);
        let mut line = 1;
        for ch in src.chars() {
            line_of.push(line);
            if ch == '\n' {
                line += 1;
            }
        }
        line_of.push(line);
        for (idx, lit) in scanned.literals {
            let enclosing: Vec<&EmitterFn> = fns
                .iter()
                .filter(|f| f.start <= idx && idx <= f.end)
                .collect();
            if enclosing.is_empty() {
                continue;
            }
            let in_config_impl = |ty: &str| {
                impls
                    .iter()
                    .any(|(h, s, e)| h.contains(ty) && *s <= idx && idx <= *e)
            };
            let scoped = |cfg_ty: &str, params: &[&str]| {
                enclosing.iter().any(|f| {
                    f.params_no_ws.contains(cfg_ty)
                        || params
                            .iter()
                            .any(|p| f.params_no_ws.contains(&format!("{p}:")))
                        || (f.is_method && in_config_impl(cfg_ty))
                })
            };
            let de = scoped("DeserializeConfig", DESERIALIZE_NAME_PARAMS);
            let se = scoped("SerializeConfig", SERIALIZE_NAME_PARAMS);
            if de || se {
                let innermost = enclosing
                    .iter()
                    .max_by_key(|f| f.start)
                    .expect("non-empty")
                    .name
                    .clone();
                out.push((*file, innermost, line_of[idx], lit, de, se));
            }
        }
    }
    out
}

/// LOCKSTEP source lint (FAST tier — this module IS `snapshot_tests`, which is the only cargo-test
/// invocation `check.ts fast` makes: `cargo test --bin cddl-codegen snapshot_tests`).
///
/// `generate_deserialize` threads a deserializer name (`raw` by default, the payload's own cursor
/// under a `bytes .cbor`) and `generate_serialize` threads a serializer name (`serializer` by
/// default, an inner `Serializer::new_vec()` buffer under the same payload, or `buf` for a canonical
/// map key). Both are OVERLOADABLE parameters carried by hand down a recursive emitter, and nothing
/// structurally forces a leaf to read them: a leaf that writes the DEFAULT name inline compiles,
/// snapshots green, and mis-frames the buffer only in the compositions that actually overload —
/// which is why four such deserialize leaves were found by composition luck and code-reading rather
/// than by any gate. This is that gate.
///
/// SCOPE (stated so the gap is visible rather than implied): a fn is overload-scoped when it
/// RECEIVES the name — a `DeserializeConfig`/`SerializeConfig` parameter, a `deserializer_name:` /
/// `serializer_use:` / `serializer_pass:` parameter, or a `&self` method of the config types. Root
/// emitters (`codegen_struct`, `generate_enum`, `generate_wrapper_struct`, …) are deliberately out
/// of scope: they EMIT the `fn deserialize(raw: &mut Deserializer)` / `fn serialize(serializer: &mut
/// Serializer)` signature that binds the name, so spelling it is what they are for. A helper that
/// receives the name under a spelling the rule does not know would be silently out of scope; that
/// is guarded from the other side by [`emitter_overload_lint_scopes_every_name_param`], while
/// [`emitter_overload_lint_sees_its_anchors`] pins that the KNOWN fns stay scoped. The residual
/// neither can see — a scoped fn that builds a FRESH config instead of threading the caller's, so
/// every literal below it correctly spells an accessor resolving to the default — is recorded with
/// its reopening signal in `tests/testing-roadmap.toml` (the overload-lint residuals entry).
#[test]
fn emitter_overload_no_bare_default_tokens() {
    let mut failures = Vec::new();
    for (file, func, line, lit, de, se) in overload_scoped_literals() {
        for (axis, token) in OVERLOADED_DEFAULTS {
            let on = match *token {
                "raw" => de,
                _ => se,
            };
            if !on || !contains_bare_token(&lit, token) {
                continue;
            }
            if OVERLOAD_LINT_ALLOW
                .iter()
                .any(|(f, fun, l, _)| *f == file && *fun == func && *l == lit)
            {
                continue;
            }
            failures.push(format!(
                "  {file}:{line} (fn {func}) emits the default {axis} name `{token}` inline:\n    \
                 {lit:?}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "emitter leaf/leaves spell an OVERLOADABLE default name inline instead of through the \
         config accessor (`{{deserializer_name}}` / `{{serializer_use}}`):\n{}\n\nUnder a `bytes \
         .cbor` payload (or a canonical map key) these read the OUTER cursor/buffer and silently \
         mis-frame every member after them. Thread the accessor; if the site is genuinely correct \
         as written, add it to `OVERLOAD_LINT_ALLOW` with a justification.",
        failures.join("\n")
    );
}

/// Anti-vacuity guard for [`emitter_overload_no_bare_default_tokens`]: the scan must still SEE the
/// helpers that carry an overloadable name, and must still be reading a substantial body of emitted
/// literals. A scoping rule that silently stopped matching would make the lint pass for the wrong
/// reason — the same failure mode the lint exists to prevent, one level up.
#[test]
fn emitter_overload_lint_sees_its_anchors() {
    let scoped = overload_scoped_literals();
    // (file, fn) pairs that MUST be in scope: the two recursive emitters plus every helper that
    // takes the name today. `make_enum_variant_return_if_deserialized` is the c-style-enum sweep
    // whose helper used to drop the overload by building a fresh config.
    for (file, func) in [
        ("deserialize.rs", "generate_deserialize"),
        ("deserialize.rs", "make_deser_loop_break_check"),
        ("enums.rs", "make_enum_variant_return_if_deserialized"),
        ("serialize.rs", "generate_serialize"),
        ("serialize.rs", "start_len"),
        ("serialize.rs", "end_len"),
    ] {
        assert!(
            scoped.iter().any(|(f, fun, ..)| *f == file && fun == func),
            "the overload lint no longer sees `{file}::{func}` as overload-scoped — either the \
             helper stopped taking the name (retire the anchor) or the scoping rule went vacuous"
        );
    }
    assert!(
        scoped.len() > 200,
        "the overload lint scans only {} literals — it has gone vacuous",
        scoped.len()
    );
    for (file, func, lit, _) in OVERLOAD_LINT_ALLOW {
        assert!(
            scoped
                .iter()
                .any(|(f, fun, _, l, ..)| f == file && fun == func && l == lit),
            "stale `OVERLOAD_LINT_ALLOW` entry: {file} (fn {func}) no longer emits {lit:?} — an \
             allowlist entry that matches nothing hides the next real leaf at that site"
        );
    }
}

/// The type spellings a parameter carrying an overloadable NAME can have. The shipped overloads are
/// `Option<&'a str>` (deserializer cursor) and `Option<(&'a str, bool)>` (serializer buffer + its
/// `is_end` flag), so a helper receives one either unwrapped or still wrapped. Everything else is
/// type-distinguished and therefore out of scope by construction — in particular a root-binding
/// `serializer: &mut Serializer` (the emitter's own `Serializer` handle, not a name) and the
/// `serializing_rust_type: SerializingRustType` / `generate_serialize_embedded: bool` parameters
/// whose identifiers merely contain `serializ`.
const NAME_PARAM_TYPES: &[&str] = &["&str", "(&str,bool)", "Option<&str>", "Option<(&str,bool)>"];

/// Split a whitespace-stripped parameter list (`(&self,serializer_use:&str,cli:&Cli)`) into its
/// `(identifier, type)` pairs. Bracket depth is tracked over `<>`/`()`/`[]` so a `,` inside a
/// generic or a tuple type does not split a parameter (a `>` closing an `->` return arrow is not a
/// bracket), and lifetimes are stripped from the type so `&'a str` and `&str` compare equal.
/// A receiver (`&self`) carries no `:` and is dropped.
fn split_params(params_no_ws: &str) -> Vec<(String, String)> {
    let inner = params_no_ws
        .strip_prefix('(')
        .and_then(|s| s.strip_suffix(')'))
        .unwrap_or(params_no_ws);
    let mut pieces = Vec::new();
    let mut depth = 0i32;
    let mut cur = String::new();
    let mut prev = '\0';
    for ch in inner.chars() {
        match ch {
            '<' | '(' | '[' => depth += 1,
            '>' if prev != '-' => depth -= 1,
            ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                pieces.push(std::mem::take(&mut cur));
                prev = ch;
                continue;
            }
            _ => {}
        }
        cur.push(ch);
        prev = ch;
    }
    pieces.push(cur);
    pieces
        .into_iter()
        .filter_map(|p| {
            let at = p.find(':')?;
            let (name, ty) = (p[..at].to_string(), p[at + 1..].to_string());
            // drop `'a` / `'_` lifetime tokens from the type
            let mut stripped = String::new();
            let mut chars = ty.chars().peekable();
            while let Some(c) = chars.next() {
                if c == '\'' {
                    while chars
                        .peek()
                        .is_some_and(|n| super::identifier_hazard_tests::is_ident_char(*n))
                    {
                        chars.next();
                    }
                    continue;
                }
                stripped.push(c);
            }
            Some((name, stripped))
        })
        .collect()
}

/// Fourth-spelling guard for the scoping rule behind [`emitter_overload_no_bare_default_tokens`]
/// (FAST tier, same module). The lint only inspects fns it considers overload-SCOPED, and it
/// recognizes them by the parameter spellings that exist today — so a future helper that receives
/// the name under a spelling the rule does not know is silently unlinted, and the first observable
/// would be the next mis-framed leaf, in a consumer. This closes that by scanning the same emitter
/// sources for the inverse: every fn parameter whose identifier contains `serializ` AND whose type
/// is one a NAME is carried in ([`NAME_PARAM_TYPES`]) must be spelled one of the ways
/// [`DESERIALIZE_NAME_PARAMS`]/[`SERIALIZE_NAME_PARAMS`] list.
#[test]
fn emitter_overload_lint_scopes_every_name_param() {
    let mut failures = Vec::new();
    let mut seen: Vec<String> = Vec::new();
    for file in super::identifier_hazard_tests::EMITTER_SOURCES {
        let path = format!("{}/src/generation/{file}", env!("CARGO_MANIFEST_DIR"));
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("cannot read emitter source {path}: {e}"));
        let masked: Vec<char> = super::identifier_hazard_tests::scan_rust(&src)
            .masked
            .chars()
            .collect();
        for f in emitter_fns(&masked) {
            let line = 1 + masked[..f.start].iter().filter(|c| **c == '\n').count();
            for (name, ty) in split_params(&f.params_no_ws) {
                if !name.contains("serializ") || !NAME_PARAM_TYPES.contains(&ty.as_str()) {
                    continue;
                }
                let known = DESERIALIZE_NAME_PARAMS
                    .iter()
                    .chain(SERIALIZE_NAME_PARAMS)
                    .any(|p| name == *p || name == format!("mut{p}"));
                if known {
                    seen.push(name);
                } else {
                    failures.push(format!(
                        "  {file}:{line} (fn {}) takes `{name}: {ty}`",
                        f.name
                    ));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "an emitter fn receives an overloadable name under a parameter spelling the overload \
         scoping rule does not recognize, so every literal below it is UNLINTED by \
         `emitter_overload_no_bare_default_tokens`:\n{}\n\nTwo remedies, either is fine: teach the \
         scoping rule the new spelling (add it to `DESERIALIZE_NAME_PARAMS` / \
         `SERIALIZE_NAME_PARAMS`), or rename the parameter to one of the spellings already there \
         ({:?} / {:?}).",
        failures.join("\n"),
        DESERIALIZE_NAME_PARAMS,
        SERIALIZE_NAME_PARAMS,
    );
    // Anti-vacuity, and staleness in the same assertion: a scoping spelling that matches no
    // parameter is one a rename already moved past, and it would keep the renamed parameter
    // looking recognized to nobody while this guard reports nothing.
    for p in DESERIALIZE_NAME_PARAMS.iter().chain(SERIALIZE_NAME_PARAMS) {
        assert!(
            seen.iter().any(|s| s == p || s == &format!("mut{p}")),
            "the scoping rule recognizes `{p}` but no emitter fn takes a name-typed parameter \
             spelled that way — either the parameter was renamed (teach the rule the new \
             spelling and drop this one) or the parameter scan has gone vacuous (it found {seen:?})"
        );
    }
}

/// `rustfmt_generated_string` must FAIL LOUD on unparseable output rather than swallowing it and
/// returning the raw source at exit 0 — the swallow is exactly how the JSON-schema turbofish bug
/// (`T<..>::method` in expression position) shipped green. Valid Rust still round-trips to `Ok`.
#[test]
fn rustfmt_rejects_unparseable_source() {
    // valid Rust formats successfully
    crate::generation::rustfmt_generated_string("fn main() {}").expect("valid Rust must format Ok");
    // the turbofish shape (generic type-spelling before `::method`) is a parse error — must be Err
    assert!(
        crate::generation::rustfmt_generated_string(
            "fn f() -> usize { BTreeMap<u64, String>::len() }"
        )
        .is_err(),
        "rustfmt parse failure must propagate as Err, not be swallowed"
    );
}

/// Write an executable `/bin/sh` stub at a unique path and return it. The stub stands in for the
/// rustfmt binary in the tests below, injected through `rustfmt_source_with` rather than through
/// `RUSTFMT`/`PATH`: both are process-global, `cargo test` runs tests as threads of ONE process, and
/// every other test that formats would pick the stub up — a hazard no mutex among these tests can
/// remove, because the other callers do not take it.
#[cfg(unix)]
fn rustfmt_stub(tag: &str, body: &str) -> std::path::PathBuf {
    use std::os::unix::fs::PermissionsExt;
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_rustfmt_stub_{tag}_{}",
        std::process::id()
    ));
    std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();

    // Writing an executable and immediately exec'ing it races every OTHER thread in this test
    // process: `fork` duplicates our still-open write fd into the child, and exec'ing a file that
    // any process holds open for writing fails with ETXTBSY. The window belongs to unrelated
    // concurrent `Command::spawn`s elsewhere in the suite — nothing this function does can close
    // it by ordering, and `std` deliberately does not retry. So wait it out here rather than in
    // `rustfmt_source_with`, whose ETXTBSY on a real installed formatter would be a true error
    // worth surfacing. Waiting is sound because the set of processes holding the fd only shrinks
    // (each releases at its own exec or exit): once one exec succeeds, the window has passed, so
    // the path this returns is known-runnable and the leg under test measures the formatter
    // contract rather than the race.
    for attempt in 0..50 {
        match std::process::Command::new(&path)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
        {
            Ok(_) => return path,
            Err(e) if e.kind() == std::io::ErrorKind::ExecutableFileBusy => {
                std::thread::sleep(std::time::Duration::from_millis(2 * (attempt + 1)));
            }
            Err(e) => panic!("stub {} is not runnable: {e}", path.display()),
        }
    }
    panic!(
        "stub {} stayed ETXTBSY for the whole wait-out budget",
        path.display()
    )
}

/// Non-UTF-8 formatter output is an ERROR, never `Ok` carrying the unformatted input.
///
/// The swallow this replaces was the one hole in the "any exit other than 0/3 is fatal" contract:
/// a rustfmt that crashed mid-write shipped the UNFORMATTED source at exit 0, silently breaking the
/// canonical-layout invariant that two standing gates cite as their entire assertion mechanism. It
/// is never a benign outcome — the input is a Rust `&str`, so a formatter that merely reformatted it
/// emits UTF-8 by construction.
///
/// The control leg is load-bearing: an identical stub that echoes its stdin must still return `Ok`
/// with that content, so the failing leg cannot be passing merely because a stub binary was used.
#[cfg(unix)]
#[test]
fn rustfmt_non_utf8_output_is_an_error() {
    let source = "fn main() {}\n";

    // Control: a well-behaved stub round-trips to Ok.
    let ok_stub = rustfmt_stub("ok", "cat");
    let formatted = crate::generation::rustfmt_source_with(&ok_stub, source)
        .expect("a stub that echoes valid UTF-8 at exit 0 must format Ok");
    assert_eq!(formatted.as_ref(), source);
    std::fs::remove_file(&ok_stub).ok();

    // The leg under test: valid exit status, invalid bytes.
    let bad_stub = rustfmt_stub("badutf8", "cat > /dev/null\nprintf '\\377\\376'");
    let err = crate::generation::rustfmt_source_with(&bad_stub, source)
        .expect_err("non-UTF-8 formatter output must be an Err, never Ok carrying the input");
    std::fs::remove_file(&bad_stub).ok();
    assert_eq!(
        err.kind(),
        std::io::ErrorKind::InvalidData,
        "non-UTF-8 output must be InvalidData, got {err:?}"
    );
    let msg = err.to_string();
    assert!(
        msg.contains("non-UTF-8") && msg.contains("Refusing to write unformatted output"),
        "the error must name the condition and say what it refused to do, got: {msg}"
    );
}

/// A formatter binary that cannot be spawned is a clean `Err`, not an unwrap backtrace.
///
/// `rustfmt_generated_string` used to `.unwrap()` the path lookup, so "no rustfmt on this machine"
/// — a user-facing environment problem, on a call whose whole path is already `io::Result` —
/// surfaced as a panic with no diagnosis. The spawn-failure leg exercises the same `Result` shape
/// the converted lookup now propagates.
///
/// NOT covered here: `rustfmt_path()`'s OWN `Err` (no `RUSTFMT` and `which` fails). Driving it needs
/// a process-global `PATH` with no rustfmt on it, which would race the concurrent tests that spawn
/// `cargo` in the same process.
#[cfg(unix)]
#[test]
fn rustfmt_unspawnable_binary_is_an_error_not_a_panic() {
    let missing = std::env::temp_dir().join(format!(
        "cddl_codegen_rustfmt_absent_{}",
        std::process::id()
    ));
    std::fs::remove_file(&missing).ok();
    let err = crate::generation::rustfmt_source_with(&missing, "fn main() {}\n")
        .expect_err("a formatter binary that does not exist must be an Err, not a panic");
    assert_eq!(
        err.kind(),
        std::io::ErrorKind::NotFound,
        "a missing formatter binary must surface as the spawn's own NotFound, got {err:?}"
    );
}

/// The comment-preserve overlay's markers must survive the tool's OWN rustfmt pass. rustfmt folds a
/// `// cddl-codegen:<tag>` comment trailing the closing `}` of a match's last arm onto that `}` as a
/// trailing comment, so a `preserve → rustfmt_generated_string → preserve` loop would otherwise write
/// an on-disk form the next regen's own-line scan gate can't read. This closes that loop end to end:
/// emit an own-line match-tail replace block, run the tool's EXACT rustfmt pass (folds it on a folding
/// rustfmt, leaves it own-line otherwise — both spellings parse), then re-run `preserve`; the user
/// section and recorded original must survive. Robust across rustfmt versions: the acceptance
/// criterion is only that the rustfmt'd form re-parses, not that it folds.
#[test]
fn preserve_markers_survive_rustfmt_fold_roundtrip() {
    use crate::comment_preserve::preserve;
    // `old` as codegen+preserve would leave it on disk: an own-line match-tail replace block whose
    // user section swaps the generated `unknown => return Err(())` tail arm for a lenient skip.
    let old = "\
// header
impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            // cddl-codegen:replace-start
            unknown => {
                let _ = unknown;
                Ok(key)
            }
            // cddl-codegen:replaces
            // unknown => return Err(()),
            // cddl-codegen:replace-end
        }
    }
}
";
    // pristine regen: the generator re-emits the recorded original tail arm.
    let new = "\
// header
impl Foo {
    fn deserialize(&self, key: u8) -> Result<u8, ()> {
        match key {
            0 => Ok(key),
            unknown => return Err(()),
        }
    }
}
";
    // The tool's exact pass: `rustfmt --edition 2024` over the preserved on-disk content.
    let formatted =
        crate::generation::rustfmt_generated_string(old).expect("valid Rust must format Ok");
    // The next regen must re-parse the (possibly folded) on-disk form and re-apply the swap.
    let merged = preserve(&formatted, new)
        .expect("the rustfmt'd match-tail replace block must re-parse (fold-tolerant)");
    assert!(
        merged.content.contains("let _ = unknown;"),
        "user section lost across the rustfmt fold round-trip:\n{}",
        merged.content
    );
    assert!(
        merged.content.contains("// unknown => return Err(()),"),
        "recorded original lost across the rustfmt fold round-trip:\n{}",
        merged.content
    );
}

/// `generate_tag_check` (the tag check a directly-deserializing tagged type emits) has two arms
/// selected by `--annotate-fields`. The `annotated=true` arm is exercised end-to-end by the core
/// `tagged_type_choice` fixture (whole_program snapshot + the wrong-tag behavioral test), but that
/// fixture always renders with `annotate_fields=true`; the `annotated=false` name-carrying arm has
/// no *rendered* pin. This unit-renders BOTH arms and asserts the discriminating fragments (robust
/// to rustfmt line-wrapping via whitespace stripping):
/// * `annotated=true` — a bare `raw.tag()?` read and the LOCATIONLESS
///   `DeserializeFailure::TagMismatch{ .. }.into()` (the annotate closure supplies the name), never
///   the name-carrying `DeserializeError::new`.
/// * `annotated=false` — the name-carrying `DeserializeError::new("Ident", ..)` plus a tag read that
///   annotates the ident itself (`.map_err(|e| DeserializeError::from(e).annotate("Ident"))?`), since
///   no closure will add the location.
#[test]
fn generate_tag_check_arms() {
    use crate::generation::generate_tag_check;
    use crate::intermediate::{CDDLIdent, RustIdent};

    let ident = RustIdent::new(CDDLIdent::new("Ident"));
    let render = |annotated: bool| {
        let mut f = codegen::Function::new("deserialize");
        generate_tag_check(&mut f, &ident, Some(11), annotated);
        let mut scope = codegen::Scope::new();
        scope.push_fn(f);
        scope.to_string()
    };
    let flatten = |s: &str| -> String { s.chars().filter(|c| !c.is_whitespace()).collect() };

    let annotated = render(true);
    let flat_annotated = flatten(&annotated);
    assert!(
        flat_annotated.contains("lettag=raw.tag()?;"),
        "annotated arm must read the tag bare (no map_err annotate), got:\n{annotated}"
    );
    assert!(
        flat_annotated.contains("DeserializeFailure::TagMismatch{found:tag,expected:11}.into()"),
        "annotated arm must emit the locationless TagMismatch form, got:\n{annotated}"
    );
    assert!(
        !flat_annotated.contains("DeserializeError::new"),
        "annotated arm must NOT carry the name (the closure supplies it), got:\n{annotated}"
    );

    let named = render(false);
    let flat_named = flatten(&named);
    assert!(
        flat_named.contains(
            "DeserializeError::new(\"Ident\",DeserializeFailure::TagMismatch{found:tag,expected:11})"
        ),
        "unannotated arm must emit the name-carrying error, got:\n{named}"
    );
    assert!(
        flat_named.contains(".map_err(|e|DeserializeError::from(e).annotate(\"Ident\"))?"),
        "unannotated arm must annotate the tag read with the ident, got:\n{named}"
    );
}

// --- Dep-side extern-interface export emitter (commit 4) ----------------------------------------

/// Snapshot the emitted extern-interface tree for the `tests/extern-interface-emit/inputs` fixture,
/// which exercises EVERY projection row: opaque Record/Wrapper/TypeChoice/GroupChoice, raw-bytes,
/// transparent named collections (Array + Table) via the alias spelling, c-style enum, `@no_alias`,
/// an alias chain, a prelude (`bignint`) reference that renders (not excluded), a named generic
/// instance (opaque), a plain group / generic definition / extern-dep-scope rule (all ABSENT), a
/// nested-scope subfile, a REFERENCED plain group (a transparent group-body row) and a
/// never-referenced one (excluded-with-record, Ask 0), a generic definition / extern-dep-scope rule
/// (both ABSENT), and the exclude-with-record + reference-closure paths (custom-serialize alias, its
/// transitive dependent, an anonymous-generic-instance reference). Snapshots live beside
/// the fixture. Bless with `INSTA_UPDATE=always cargo test extern_interface_emit`.
#[test]
fn extern_interface_emit() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli)
        .expect("extern-interface projection must succeed (exclude-with-record, never abort)");

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/extern-interface-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        assert!(!files.is_empty(), "no extern-interface files emitted");
        for (path, content) in &files {
            let name = path.replace('/', "__");
            insta::assert_snapshot!(name, content);
        }
    });
}

/// The projection is deterministic: emit twice, require byte-identical output (all-`BTreeMap`/`BTreeSet`,
/// no `HashMap`). Same guarantee `generation_is_deterministic` gives the main output.
#[test]
fn extern_interface_emit_is_deterministic() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let a = crate::api::extern_interface_strings(&cli).unwrap();
    let b = crate::api::extern_interface_strings(&cli).unwrap();
    assert_eq!(
        a, b,
        "extern-interface export must be byte-identical across runs"
    );
}

/// Emission is UNCONDITIONAL in every mode: the export is byte-identical under `--wasm=false` and
/// `--wasm=true` (it describes named rule surfaces, not the wasm wrapper inventory).
#[test]
fn extern_interface_emit_same_in_both_modes() {
    let base = std::path::Path::new("tests/extern-interface-emit/inputs");
    let rust_only = crate::api::extern_interface_strings(&cli_for(
        base,
        &["--wasm", "false", "--lib-name", "dep"],
    ))
    .unwrap();
    let wasm = crate::api::extern_interface_strings(&cli_for(
        base,
        &["--wasm", "true", "--lib-name", "dep"],
    ))
    .unwrap();
    assert_eq!(
        rust_only, wasm,
        "extern-interface export must not depend on the wasm/rust-only mode"
    );
}

/// A surface with NO included rules still emits a single root file (stable presence, answering "was
/// this dep regenerated?"). The lone rule is a never-referenced plain group, which materializes no
/// shape but — per the excluded-with-record contract (Ask 0) — leaves a `; unexported:` record rather
/// than vanishing, so the root file is the header plus that one record and nothing else.
#[test]
fn extern_interface_emit_empty_surface() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/empty/lib.cddl"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    assert_eq!(
        files.keys().collect::<Vec<_>>(),
        vec!["extern-interface/dep/mod.cddl"],
        "a no-included-rows surface must emit exactly one root file"
    );
    assert_eq!(
        files["extern-interface/dep/mod.cddl"],
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\n\
         ; unexported: helper — plain group never referenced in the dependency's own spec — no \
         materialized shape to project\n",
        "the root file is the header line plus the lone plain group's exclusion record"
    );
}

/// The exclude-with-record semantics, asserted directly (clearer regression messages than the tree
/// snapshot alone): generation SUCCEEDS, a custom-serialize transparent alias is excluded with a
/// record, a rule referencing it is transitively excluded naming the CHAIN ROOT (not its immediate
/// neighbour), an anonymous-generic-instance reference is excluded, and a prelude (`bignint`)
/// reference RENDERS rather than being excluded.
#[test]
fn extern_interface_emit_exclusions_and_closure() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    let root = &files["extern-interface/dep/mod.cddl"];

    // custom-serialize transparent alias -> excluded with a record, generation still succeeded.
    assert!(
        root.contains("; unexported: cs — @custom_serialize"),
        "custom-serialize alias `cs` must be excluded with a record:\n{root}"
    );
    assert!(
        !root.contains("\ncs = "),
        "excluded `cs` must NOT appear as an exported rule:\n{root}"
    );

    // reference-closure: dep_cs references excluded cs -> excluded, naming the chain root `cs`.
    assert!(
        root.contains("; unexported: dep_cs — references excluded cs"),
        "`dep_cs` must be closure-excluded naming root `cs`:\n{root}"
    );

    // anonymous generic instance reference -> excluded (no CDDL ident to spell).
    assert!(
        root.contains("; unexported: anon_arr —"),
        "`anon_arr` (references an anonymous generic instance) must be excluded:\n{root}"
    );

    // a prelude reference RENDERS by prelude name, it is NOT an exclusion. The carrier is a
    // transparent named table: a `bytes .cbor` rule body force-wraps, so it projects opaque
    // (asserted just below) and cannot exercise the rendering path.
    assert!(
        root.contains("bn_tbl = {* bignint => uint} ; @rust_name BnTbl"),
        "`bn_tbl` must render the `bignint` prelude reference, not be excluded:\n{root}"
    );
    assert!(
        !root.contains("; unexported: bn_tbl"),
        "`bn_tbl` must not be excluded:\n{root}"
    );

    // a `bytes .cbor` rule body is a WRAPPER struct, so it projects as the opaque marker (the
    // wrapper owns the byte-string framing; a consumer re-deriving it from a transparent body
    // would have had two wire forms for one type).
    assert!(
        root.contains("bn = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "`bn` (a force-wrapped `.cbor` root) must project opaquely:\n{root}"
    );
    assert!(
        !root.contains("; unexported: bn "),
        "`bn` must not be excluded:\n{root}"
    );

    // an opaque row that embeds an excluded type is self-contained -> NOT excluded.
    assert!(
        root.contains("holder = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "`holder` (opaque, embeds `cs` internally) must still export:\n{root}"
    );
    assert!(
        !root.contains("; unexported: holder"),
        "`holder` must not be excluded — its opaque marker is self-contained:\n{root}"
    );
}

/// A named rule BINDING a generic set-nominal instantiation used at more than one site
/// (`req_signers = nonempty_set<uint>`, with a second anonymous use forcing the instantiation to
/// mint under its own canonical ident) lowers to a `pub type` alias TO the nominal — and that alias
/// must project into the extern interface as an OPAQUE row, not drop out with a `; unexported:`
/// record. The instantiation-minted nominal (`NonemptySetU64`) has NO source CDDL rule name, so the
/// transparent renderer's `render_rust_ref` once hard-`Err`ed on the unspellable `Rust(<nominal>)`
/// reference and dropped the whole rule (CML's first regen lost its `required_signers` row this
/// way); it now takes the same opaque marker the nominal itself would in pass 1, named by the
/// alias's `@rust_name`. The consumer references it opaquely and never needs the instantiation's
/// spelling.
#[test]
fn extern_interface_projects_alias_to_set_nominal_as_opaque() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_extif_setnominal_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "nonempty_set<a0> = #6.258([+ a0]) / [+ a0]\n\
         req_signers = nonempty_set<uint>\n\
         holder = [rs: req_signers, more: nonempty_set<uint>]\n",
    )
    .unwrap();
    let cli = cli_for(&path, &["--wasm", "false", "--lib-name", "dep"]);
    let files = crate::api::extern_interface_strings(&cli).unwrap();
    std::fs::remove_file(&path).ok();
    let root = &files["extern-interface/dep/mod.cddl"];
    assert!(
        root.contains("req_signers = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name ReqSigners"),
        "the alias-to-set-nominal rule must project as an opaque row named by its @rust_name:\n{root}"
    );
    assert!(
        !root.contains("; unexported: req_signers"),
        "the alias-to-set-nominal rule must NOT drop out with an unexported record:\n{root}"
    );
}

// --- Dep-side extern-interface compiled self-check (commit 5) ------------------------------------

/// Snapshot the dep-side compiled self-check (`generated/extern_interface_check.rs`) the emit
/// fixture produces. It is derived from the SAME projection as the export snapshotted above, and
/// covers every assertion kind in one file: `Serialize`(+`Deserialize`) on opaque rows
/// (record/wrapper/type-choice/group-choice/named-generic-instance), `RawBytesEncoding` on the
/// raw-bytes row (`hash`), the `use … as _;` existence check on transparent rows (aliases /
/// c-style enum / named collections), the `@no_alias` skip (`na` gets no assertion), the group-body
/// row's four-bound assertion (`pg` — whole-value + embedded-group Serialize/Deserialize), and a
/// nested-scope path (`sub::module::NestedRec`). Bless with
/// `INSTA_UPDATE=always cargo test extern_interface_check_emit`.
#[test]
fn extern_interface_check_emit() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-emit/inputs"),
        &["--wasm", "false", "--lib-name", "dep"],
    );
    let files = crate::api::generated_strings(&cli)
        .expect("generation must succeed for the emit fixture (string emission is infallible)");
    let content = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("the self-check file must be emitted");

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/extern-interface-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!("extern_interface_check", content);
    });
}

/// The self-check WEAKENS the bound to `Serialize` only for a type with no generated `Deserialize`:
/// `ambig` has an optional member adjacent to an open rest tail, so asserting `Deserialize` on it
/// would fail the dep's own build. A normal record (`rec`) keeps
/// both, and a `@no_alias` rule (`raw_index`) — which emits no rust type — is not asserted at all.
#[test]
fn extern_interface_check_weakens_deserialize_bound() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-interface-check/inputs"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        c.contains("_assert_serialize::<crate::generated::Ambig>()"),
        "the deserialize-less `ambig` must still assert Serialize:\n{c}"
    );
    assert!(
        !c.contains("_assert_deserialize::<crate::generated::Ambig>()"),
        "`ambig` has no generated deserialize — its bound must be WEAKENED to Serialize only:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::Rec>()")
            && c.contains("_assert_deserialize::<crate::generated::Rec>()"),
        "a normal record keeps BOTH bounds:\n{c}"
    );
    assert!(
        !c.contains("RawIndex"),
        "a `@no_alias` rule emits no rust type — the self-check must assert nothing for it:\n{c}"
    );
}

/// A self-check assertion on a generic-extern BASE (`ext_set<T>` → rust `ExtSet<T>`) would not
/// compile — bare `ExtSet` names no concrete type — so the base is SKIPPED, while its concrete
/// siblings still assert. The generated crate compiles end-to-end via `extern_generic_raw_bytes`;
/// this pins the skip decision directly (fast, no nested cargo).
#[test]
fn extern_interface_check_skips_generic_base() {
    let cli = cli_for(
        std::path::Path::new("tests/extern-generic-raw-bytes/input.cddl"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        !c.contains("ExtSet"),
        "the generic-extern base `ExtSet<T>` is not a concrete type — it must be skipped:\n{c}"
    );
    assert!(
        c.contains("_assert_raw_bytes::<crate::generated::PubKey>()"),
        "the raw-bytes row must assert RawBytesEncoding:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::Plain>()")
            && c.contains("_assert_serialize::<crate::generated::UsingFlavored>()"),
        "the concrete opaque rows must still assert Serialize:\n{c}"
    );
}

/// The wasm own-spec extern-root obligation follows the names the generated wasm boundary actually
/// uses, rather than every marker in the IR. A generic-extern instance deliberately skips the rust
/// `Base<Args>` alias in the wasm alias walk, so `ext_set<pub_key>` needs only its concrete
/// `ExtSetPubKey` wrapper — not the raw-bytes `PubKey` argument. The nested-list half takes the
/// other route through `scope_references`: its collection wrapper names `PubKey` at its own scope,
/// so the raw-bytes glue must remain even though the referring record only names the list class.
#[test]
fn wasm_extern_reexports_follow_boundary_references() {
    let generic = crate::api::generated_strings(&cli_for(
        std::path::Path::new("tests/extern-generic-raw-bytes/input.cddl"),
        &["--wasm=true"],
    ))
    .expect("generic raw-bytes fixture must generate");
    let generic_wasm = generic
        .get("wasm/src/generated/mod.rs")
        .expect("wasm generated root must be emitted");
    assert!(
        generic_wasm.contains("pub use crate::ExtSetPlain;")
            && generic_wasm.contains("pub use crate::ExtSetPubKey;"),
        "the concrete extern-generic wrappers named by wasm must keep their glue:\n{generic_wasm}"
    );
    assert!(
        !generic_wasm.contains("pub use crate::PubKey;"),
        "a raw-bytes marker reached only as a generic argument is not a wasm boundary name:\n{generic_wasm}"
    );

    let direct = crate::api::generated_strings(&cli_for(
        std::path::Path::new("tests/wasm-extern-reexport-reachability/input.cddl"),
        &["--wasm=true"],
    ))
    .expect("nested raw-bytes fixture must generate");
    let direct_wasm = direct
        .get("wasm/src/generated/mod.rs")
        .expect("wasm generated root must be emitted");
    assert!(
        direct_wasm.contains("pub use crate::PubKey;"),
        "a raw-bytes type named by a nested wasm collection wrapper must keep its glue:\n{direct_wasm}"
    );
}

/// The extern-interface self-check counterpart to `extern_interface_check_skips_generic_base` for a
/// generic-extern base with ZERO instances (`ext_unused<T>` in `json-extern-rows`). `ExtSet` there
/// has an instance, so `generic_instance_bases` catches it; `ext_unused` has none, so the `None`
/// check-kind decision must key on the parse-time `generic_extern_bases` record — otherwise the base
/// leaks an `_assert_serialize::<crate::generated::ExtUnused>()` that is E0107 in the dep's build.
#[test]
fn extern_interface_check_skips_generic_base_without_instances() {
    let cli = cli_for(
        std::path::Path::new("tests/json-extern-rows/inputs"),
        &["--wasm", "false"],
    );
    let files = crate::api::generated_strings(&cli).unwrap();
    let c = files
        .get("rust/src/generated/extern_interface_check.rs")
        .expect("self-check emitted");
    assert!(
        !c.contains("ExtUnused"),
        "a never-instantiated generic-extern base names no concrete type — it must be skipped:\n{c}"
    );
    // The instantiated generic base is likewise skipped, while the concrete rows still assert.
    assert!(
        !c.contains("ExtSet"),
        "the instantiated generic-extern base is also skipped:\n{c}"
    );
    assert!(
        c.contains("_assert_serialize::<crate::generated::MyExtern>()")
            && c.contains("_assert_serialize::<crate::generated::MySet>()")
            && c.contains("_assert_serialize::<crate::generated::BigThing>()"),
        "the concrete opaque rows must still assert Serialize:\n{c}"
    );
}

/// The RAW-BYTES sibling of the two tests above, and the reason they read as a decision rather than
/// an accident: an extern base may legitimately be parameterized (an arbitrary hand-written type),
/// so its generic-ness is RECORDED and the two emitters skip it; a raw-bytes type IS its own bytes
/// and has no element for a parameter to name, so a generic base is refused outright at parse time.
/// Before the refusal it registered a param-less `RawBytesType` and then emitted rows spelling a
/// bare `Foo` — `_assert_raw_bytes::<crate::generated::Foo>()` here, and under
/// `--json-schema-export` the json-gen `reg.add::<cddl_lib::Foo>()` — each E0107 against the
/// parameterized type the marker promises, at exit 0 with empty stderr.
///
/// This lives beside the skip tests (fast tier, CI-visible) so the two dispositions are read
/// together; the message wording and the with/without-instance sweep are pinned by
/// `generic_raw_bytes_base_rejects_gracefully` in the robustness suite.
#[test]
fn extern_interface_check_refuses_generic_raw_bytes_base() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_generic_raw_bytes_base_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nbar = [x: uint]\n",
    )
    .unwrap();
    let cli = cli_for(&path, &["--wasm", "false", "--json-schema-export", "true"]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let msg = result
        .expect_err(
            "a generic raw-bytes base must be a graceful Err, not an exit-0 uncompilable crate",
        )
        .to_string();
    assert!(
        msg.contains("cannot take generic parameters"),
        "the rejection must name the generic-parameter refusal, got: {msg}"
    );
}

/// A spec whose rules are ALL extern / raw-bytes MARKERS registers no generated struct, so nothing
/// creates a root `rust_scopes` entry before the module-declaration pass — while
/// `merge_scopes_to_strings` still writes `generated/serialization.rs` and `extern_interface_check.rs`
/// still names `crate::generated::serialization::RawBytesEncoding`. The crate then failed its OWN
/// build with E0433 at exit 0, and no user-supplied definition could fix it: the missing item is a
/// module declaration in a file the user does not own.
///
/// Pinned per marker flavor because the two reach the module by different routes (raw-bytes through
/// the self-check's `_assert_raw_bytes` bound, extern through a hand-written `Serialize`/`Deserialize`
/// impl the contract asks the user to write against `serialization::Deserialize`), and per
/// `--preserve-encodings` because the preserve root additionally carries `cbor_encodings`.
#[test]
fn marker_only_root_declares_the_serialization_module() {
    for (label, spec) in [
        ("raw-bytes", "h = _CDDL_CODEGEN_RAW_BYTES_TYPE_\n"),
        (
            "raw-bytes @copy",
            "h = _CDDL_CODEGEN_RAW_BYTES_TYPE_ ; @copy\n",
        ),
        ("extern", "e = _CDDL_CODEGEN_EXTERN_TYPE_\n"),
        ("generic extern", "g<T> = _CDDL_CODEGEN_EXTERN_TYPE_\n"),
    ] {
        for flags in [
            &["--wasm", "false"][..],
            &["--wasm", "false", "--preserve-encodings", "true"][..],
        ] {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_marker_only_root_{}_{}.cddl",
                std::process::id(),
                label.replace([' ', '@', '-'], "_")
            ));
            std::fs::write(&path, spec).unwrap();
            let cli = cli_for(&path, flags);
            let files = crate::api::generated_strings(&cli).unwrap();
            std::fs::remove_file(&path).ok();

            assert!(
                files.contains_key("rust/src/generated/serialization.rs"),
                "{label} {flags:?}: the root always materializes serialization.rs"
            );
            let root = files
                .get("rust/src/generated/mod.rs")
                .expect("root module emitted");
            assert!(
                root.contains("pub mod serialization;"),
                "{label} {flags:?}: a marker-only root must still DECLARE the serialization module \
                 it emits — an undeclared one is E0433 against the crate's own self-check:\n{root}"
            );
        }
    }
}

// --- Emitted no-std-check shim crate (D3) --------------------------------------------------------

/// The fixture family's one spec. Every test below points at it, including the ones whose assertion
/// is that the spec makes no difference — a shared constant is what makes that claim checkable
/// rather than a coincidence of two tests happening to use the same file.
const NO_STD_CHECK_INPUTS: &str = "tests/no-std-check-emit/inputs";

/// Snapshot both files of the shim under DEFAULT flags. Bless with
/// `INSTA_UPDATE=always cargo test no_std_check_emit`.
///
/// The shim is not part of the corpus family: that family is built from `api::generated_strings` ->
/// `generated_files()`, which never sees a sibling tree placed by `export()`. So it gets its own
/// fixture family, exactly as the extern-interface export does.
#[test]
fn no_std_check_emit() {
    let cli = cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false"],
    );
    let files = crate::api::no_std_check_strings(&cli);

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/no-std-check-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        assert_eq!(
            files.keys().cloned().collect::<Vec<_>>(),
            vec![
                "no-std-check/Cargo.toml".to_owned(),
                "no-std-check/src/lib.rs".to_owned()
            ],
            "the shim is exactly two files, keyed relative to the OUTPUT ROOT — a key that lost its \
             `no-std-check/` prefix would be written into the rust crate instead"
        );
        for (path, content) in &files {
            let name = path.replace('/', "__");
            insta::assert_snapshot!(name, content);
        }
    });
}

/// Deterministic: emit twice, require byte-identical output. Same guarantee
/// `generation_is_deterministic` gives the main output and `extern_interface_emit_is_deterministic`
/// gives the sibling tree.
#[test]
fn no_std_check_emit_is_deterministic() {
    let cli = cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false"],
    );
    let a = crate::api::no_std_check_strings(&cli);
    let b = crate::api::no_std_check_strings(&cli);
    assert_eq!(a, b, "the shim emission must be byte-identical across runs");
}

/// Emission is byte-identical under `--wasm=false` and `--wasm=true`. The shim depends on the RUST
/// crate only — the wasm crate is std by nature and out of the no_std scope entirely — so the wasm
/// face cannot reach it. A difference here would mean the shim had grown a dependency on the wasm
/// surface, which is the one thing it must never check.
#[test]
fn no_std_check_emit_same_in_both_modes() {
    let base = std::path::Path::new(NO_STD_CHECK_INPUTS);
    let rust_only = crate::api::no_std_check_strings(&cli_for(base, &["--wasm", "false"]));
    let wasm = crate::api::no_std_check_strings(&cli_for(base, &["--wasm", "true"]));
    assert_eq!(
        rust_only, wasm,
        "the shim is rust-crate-only surface and must not vary with --wasm"
    );
}

/// The shim is a function of the CLI, not of the spec: a spec with a wholly different construct set
/// emits the identical two files. This is what lets the fixture above stay trivial, and it is the
/// property that makes the gate's verdict mean "this crate is no_std-clean" rather than "this spec
/// happens to be".
#[test]
fn no_std_check_emit_is_spec_independent() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_nostd_specindep_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "hash28 = bytes .size 28\n\
         tbl = { * uint => text }\n\
         keyed = [ a: uint ] ; @used_as_key hash\n\
         outer = [ h: hash28, t: tbl, k: keyed ]\n",
    )
    .unwrap();
    let other = crate::api::no_std_check_strings(&cli_for(&path, &["--wasm", "false"]));
    std::fs::remove_file(&path).ok();
    let fixture = crate::api::no_std_check_strings(&cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false"],
    ));
    assert_eq!(
        fixture, other,
        "the shim must not vary with the spec — it asserts a property of the crate, not of the rules"
    );
}

/// `--lib-name` flows to all three places that name the crate: the shim's own package name
/// (`<lib-name>-no-std-check`, which is what keeps a `--config` multi-crate tree collision-free), the
/// dependency KEY (the rust crate's cargo package name is `--lib-name` verbatim), and the `use` path
/// (code form, dashes underscored). Bless with
/// `INSTA_UPDATE=always cargo test no_std_check_emit_lib_name`.
#[test]
fn no_std_check_emit_lib_name() {
    let cli = cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false", "--lib-name", "my-chain-lib"],
    );
    let files = crate::api::no_std_check_strings(&cli);

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/no-std-check-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        for (path, content) in &files {
            let name = format!("lib_name__{}", path.replace('/', "__"));
            insta::assert_snapshot!(name, content);
        }
    });
}

/// `--package-json` nests the cargo crates one level down (`<out>/rust/rust`) while the shim stays at
/// the output root, so the dep path becomes `../rust/rust`. The shim is the THIRD reader of that
/// nesting rule (LOCKSTEP with `GenerationScope::export`'s `rust_dir` and `config::crate_relative`),
/// and a disagreement there type-checks — this snapshot is what makes it fail instead.
#[test]
fn no_std_check_emit_package_json() {
    let cli = cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false", "--package-json", "true"],
    );
    let files = crate::api::no_std_check_strings(&cli);
    assert!(
        files["no-std-check/Cargo.toml"].contains("path = \"../rust/rust\""),
        "--package-json must move the dep path one level down:\n{}",
        files["no-std-check/Cargo.toml"]
    );

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/no-std-check-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!(
            "package_json__no-std-check__Cargo.toml",
            files["no-std-check/Cargo.toml"]
        );
    });
}

/// `--deserialize-depth-limit` output is the one shape whose shim CANNOT go green: the crate's
/// recursion guard is `thread_local!`-based, so its serialization prelude carries a
/// `#[cfg(not(feature = "std"))] compile_error!` and the shim's `default-features = false` dependency
/// is exactly what fires it. The shim keeps being emitted (always-emit) and instead explains itself,
/// quoting the message cargo will print — otherwise the header immediately above that paragraph
/// ("caused by hand-written additions") reads as an accusation. Bless with
/// `INSTA_UPDATE=always cargo test no_std_check_emit_depth_limit`.
///
/// The `Cargo.toml` half is deliberately NOT snapshotted here: the note belongs to the file a
/// consumer opens after a red check, and asserting the manifest is byte-identical to the default
/// one is the stronger statement (a flag that leaked into the dependency edge would fail here).
#[test]
fn no_std_check_emit_depth_limit() {
    let base = std::path::Path::new(NO_STD_CHECK_INPUTS);
    let files = crate::api::no_std_check_strings(&cli_for(
        base,
        &["--wasm", "false", "--deserialize-depth-limit", "64"],
    ));
    let default = crate::api::no_std_check_strings(&cli_for(base, &["--wasm", "false"]));

    assert_eq!(
        files["no-std-check/Cargo.toml"], default["no-std-check/Cargo.toml"],
        "the flag changes what the CHECK will say, not what the shim depends on — the manifest must \
         stay byte-identical to the default one"
    );
    assert!(
        files["no-std-check/src/lib.rs"]
            .contains(crate::generation::export::DEPTH_LIMIT_REQUIRES_STD),
        "the shim must quote the compile_error! text verbatim, so a consumer searching the message \
         cargo printed lands on the explanation:\n{}",
        files["no-std-check/src/lib.rs"]
    );
    assert!(
        !default["no-std-check/src/lib.rs"].contains("FAILS BY DESIGN"),
        "a crate generated without the flag must carry no such note:\n{}",
        default["no-std-check/src/lib.rs"]
    );

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/no-std-check-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!(
            "depth_limit__no-std-check__src__lib.rs",
            files["no-std-check/src/lib.rs"]
        );
    });
}

/// Under `--common-import-override` the runtime modules are not emitted (`Cli::export_static_files`
/// is `common_import_override.is_none()`), so there is no `error::DeserializeError` to name and the
/// shim falls back to naming the crate itself. Bless with
/// `INSTA_UPDATE=always cargo test no_std_check_emit_common_import_override`.
#[test]
fn no_std_check_emit_common_import_override() {
    let cli = cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false", "--common-import-override", "cml_core"],
    );
    let files = crate::api::no_std_check_strings(&cli);

    let dir = std::env::current_dir()
        .unwrap()
        .join("tests/no-std-check-emit/snapshots");
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(dir);
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!(
            "common_import_override__no-std-check__src__lib.rs",
            files["no-std-check/src/lib.rs"]
        );
    });
}

/// The default and `--common-import-override` `use`-shapes differ, and differ in the specific way the
/// override forces. Asserted directly rather than left to a reader diffing two snapshots: the reason
/// the default form names a TYPE is that doing so also proves the dependency's seed-once crate root
/// still re-exports `generated::*`, and that assertion is exactly what the override case gives up.
#[test]
fn no_std_check_emit_use_shape_follows_common_import_override() {
    let base = std::path::Path::new(NO_STD_CHECK_INPUTS);
    let default = crate::api::no_std_check_strings(&cli_for(base, &["--wasm", "false"]))
        ["no-std-check/src/lib.rs"]
        .clone();
    let overridden = crate::api::no_std_check_strings(&cli_for(
        base,
        &["--wasm", "false", "--common-import-override", "cml_core"],
    ))["no-std-check/src/lib.rs"]
        .clone();

    assert!(
        default
            .contains("pub type _NoStdCheckDeserializeError = cddl_lib::error::DeserializeError;"),
        "the default shim must name a generated type through the crate root:\n{default}"
    );
    assert!(
        !default.contains("use cddl_lib as _;"),
        "the default shim must NOT fall back to the crate-level form:\n{default}"
    );
    assert!(
        overridden.contains("use cddl_lib as _;"),
        "the override shim must fall back to naming the crate itself:\n{overridden}"
    );
    assert!(
        !overridden.contains("error::DeserializeError"),
        "the override crate emits no `error` module, so the shim must not name one:\n{overridden}"
    );
    assert!(
        default.starts_with("//!") && overridden.starts_with("//!"),
        "both shapes keep the generated-by header first"
    );
}

/// The documented invocation is quoted in three places that a consumer reads in sequence — the seeded
/// rust crate root (hence ~285 blessed corpus snapshots of it), the emitted shim's own two files, and
/// `docs/docs/output_format.mdx`. A consumer copies the line out of the seeded root and runs it
/// against the file this emitter writes, so a drift between them is a command that does not work.
/// This pins the two the tool emits against each other; the docs half is `lint_doc_citations`' and a
/// reader's.
#[test]
fn no_std_check_emit_quotes_the_seeded_roots_command() {
    const COMMAND: &str = "cargo check --manifest-path <output-root>/no-std-check/Cargo.toml \
                           --target thumbv7m-none-eabi";
    let files = crate::api::no_std_check_strings(&cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false"],
    ));
    for (path, content) in &files {
        assert!(
            content.contains(COMMAND),
            "{path} must quote the check command verbatim:\n{content}"
        );
    }
    let seeded = crate::api::generated_strings(&cli_for(
        std::path::Path::new(NO_STD_CHECK_INPUTS),
        &["--wasm", "false"],
    ))
    .unwrap();
    let root = seeded
        .get("rust/src/lib.rs")
        .expect("the seeded rust crate root must be part of the generated files");
    assert!(
        root.contains(COMMAND),
        "the seeded rust crate root must quote the same command verbatim:\n{root}"
    );
}
