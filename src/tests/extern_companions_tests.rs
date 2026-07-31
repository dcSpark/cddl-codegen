//! `@extern_companions` — parse (covered in `comment_ast.rs`), validate, and HONOR.
//!
//! `@extern_companions <path>=<Class>[,<Class>…]` on a LOCALLY-marked extern rule
//! (`x = _CDDL_CODEGEN_EXTERN_TYPE_`) declares that the named STRUCTURAL wasm companion classes of
//! that type already exist in a sibling wasm crate. The generator then REFERENCES them
//! (`use <path>::<Class>;`) instead of minting its own `#[wasm_bindgen]` classes of the same names —
//! two such classes linked into one cdylib are a `rust-lld: duplicate symbol __wbg_<class>_free`.
//!
//! The dependency-keyed mechanisms (`--extern-wrapper-index`, `--workspace-dep`) cannot reach this
//! case: both resolve the wrapper's constituents to an OWNING DEPENDENCY, and a local marker has no
//! dep edge. So this directive's arm in `try_defer_wrapper` is FLAGLESS, and the authority is the
//! committed spec text rather than a dep index — which also means the machine check is the
//! consumer's own compile (an absent class fails the emitted `use` loudly and near), the same
//! trust-and-compile contract the extern marker itself has.
//!
//! These tests drive the generator end-to-end over synthetic scratch trees (never a real consumer
//! checkout) and assert on the generated source strings, the established in-src pattern for extern
//! behavior. The LINK-level property — that the pair actually stops duplicating symbols — is
//! `integration_tests::extern_companions_defers_to_sibling_wasm_crate`, which builds two wasm crates
//! into one `wasm32-unknown-unknown` target both ways.

use crate::cli::Cli;
use clap::Parser;

/// Generate a crate's source map from an in-memory set of `(relative path, contents)` files written
/// into a unique scratch DIRECTORY (so scope markers derive from the tree, exactly as directory
/// input does in production). Mirrors `rust_name_tests::generate_dir`.
fn generate_dir(
    files: &[(&str, &str)],
    flags: &[&str],
    wasm: bool,
    tag: &str,
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_extcomp_{}_{}",
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
        "extern_companions_unused",
        "--wasm",
        if wasm { "true" } else { "false" },
    ];
    args.extend_from_slice(flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    let _ = std::fs::remove_dir_all(&root);
    result
}

fn joined(map: &std::collections::BTreeMap<String, String>) -> String {
    map.values().cloned().collect::<Vec<_>>().join("\n")
}

/// The reported CML shape, end to end: a LOCAL extern marker whose `@duplicates preserve` rest row
/// keys on the extern type. The `keys()` accessor's `<K>List` is the class the sibling already owns,
/// so it defers — imported, not minted, and constructed through `From<Vec<_>>` (`.into()`) because a
/// cross-crate wrapper's tuple field is private. The row's PairMap class is NOT listed and therefore
/// still mints locally, which is the observed collision surface exactly: the sibling's map class is
/// hand-NAMED, so only the List family shares the canonical structural name.
#[test]
fn listed_keys_list_defers_while_unlisted_pair_map_mints() {
    let map = generate_dir(
        &[(
            "main.cddl",
            "transaction_metadatum = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions cml_chain_wasm=TransactionMetadatumList\n\
             meta_holder = {\n  1: uint,\n  * transaction_metadatum => transaction_metadatum ; @duplicates preserve\n}\n",
        )],
        &[],
        true,
        "cml_shape",
    )
    .expect("generation should succeed");
    let src = joined(&map);
    assert!(
        src.contains("use cml_chain_wasm::TransactionMetadatumList;"),
        "the listed companion must be imported from the declared sibling crate, got:\n{src}"
    );
    assert!(
        !src.contains("pub struct TransactionMetadatumList"),
        "the listed companion must NOT be minted locally, got:\n{src}"
    );
    assert!(
        src.contains(".collect::<Vec<_>>().into()"),
        "keys() must build the deferred list through From<Vec<_>>, not tuple-struct syntax, got:\n{src}"
    );
    assert!(
        src.contains("pub struct PairMapTransactionMetadatumToTransactionMetadatum"),
        "an UNLISTED structural companion of the same extern must still mint locally, got:\n{src}"
    );
    // R3e's analog: a deferred wrapper leaves the crate's own collection-wrapper index, so a
    // downstream `--extern-wrapper-index` consumer is never told this crate owns it.
    let index = map
        .iter()
        .find(|(path, _)| path.ends_with("wasm/src/generated/collections.rs"))
        .map(|(_, content)| content.clone())
        .expect("wasm collections index");
    assert!(
        !index.contains("TransactionMetadatumList"),
        "a deferred companion must be excluded from this crate's own collections.rs, got:\n{index}"
    );
    assert!(
        index.contains("PairMapTransactionMetadatumToTransactionMetadatum"),
        "the locally-minted companion must still be indexed, got:\n{index}"
    );
}

/// The class list is a FILTER over the whole structural-wrapper family, not a per-kind switch: a
/// declaration naming both the list and the map class defers both, from the same one directive.
#[test]
fn several_listed_classes_defer_together() {
    let map = generate_dir(
        &[(
            "main.cddl",
            "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=TmList,MapTmToTm\n\
             holder = [items: [* tm], table: { * tm => tm }]\n",
        )],
        &[],
        true,
        "multi_class",
    )
    .expect("generation should succeed");
    let src = joined(&map);
    for class in ["TmList", "MapTmToTm"] {
        assert!(
            src.contains(&format!("use dep_wasm::{class};"))
                || src.contains(&format!("{class}}};"))
                || src.contains(&format!("{class},")),
            "expected an import of the deferred {class}, got:\n{src}"
        );
        assert!(
            !src.contains(&format!("pub struct {class}")),
            "deferred {class} must not be minted locally, got:\n{src}"
        );
    }
}

/// The restricted `[+ …]` twin's structural name is a defer candidate exactly like the loose one —
/// the arm sits in the shared `try_defer_wrapper`, so every wrapper emitter inherits it — and its
/// `try_from` SOURCE (the loose list) defers independently when it is listed too.
#[test]
fn non_empty_twin_and_its_loose_source_both_defer_when_listed() {
    let map = generate_dir(
        &[(
            "main.cddl",
            "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=NonEmptyTmList,TmList\n\
             holder = [items: [+ tm]]\n",
        )],
        &[],
        true,
        "non_empty",
    )
    .expect("generation should succeed");
    let src = joined(&map);
    assert!(
        !src.contains("pub struct NonEmptyTmList") && !src.contains("pub struct TmList"),
        "both the restricted twin and its loose try_from source must defer, got:\n{src}"
    );
    assert!(
        src.contains("NonEmptyTmList"),
        "the deferred restricted twin must still be referenced, got:\n{src}"
    );
}

/// A wrapper whose constituents are NOT all the declaring extern is not "of" that type, so it mints
/// locally — the same silent fall-through the dependency-keyed arms give a mixed-owner wrapper
/// (R3c). Without this the directive would suppress a class the sibling has no reason to own.
#[test]
fn mixed_constituent_wrapper_mints_locally() {
    let map = generate_dir(
        &[(
            "main.cddl",
            "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=MapTmToLocalThing\n\
             local_thing = [a: uint, b: text]\n\
             holder = [table: { * tm => local_thing }]\n",
        )],
        &[],
        true,
        "mixed",
    )
    .expect("generation should succeed");
    let src = joined(&map);
    assert!(
        src.contains("pub struct MapTmToLocalThing"),
        "a wrapper with a consumer-owned constituent must mint locally even when listed, got:\n{src}"
    );
    assert!(
        !src.contains("use dep_wasm::"),
        "no import should be routed for a wrapper that did not defer, got:\n{src}"
    );
}

/// A DEP-scoped extern is a graceful rejection naming the two flags that DO own that case — the
/// asymmetry is the whole reason this directive exists (a local marker has no dep edge for them to
/// key on), so the message has to state which side of it the author is on.
#[test]
fn dep_scoped_extern_rejects_naming_the_flags() {
    let err = generate_dir(
        &[
            ("main.cddl", "holder = [items: [* dep_thing]]"),
            (
                "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep_crate/mod.cddl",
                "dep_thing = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_crate_wasm=DepThingList",
            ),
        ],
        &[],
        true,
        "dep_scoped",
    )
    .expect_err("a dep-scoped extern must reject the directive");
    assert!(
        err.contains("@extern_companions")
            && err.contains("--extern-wrapper-index")
            && err.contains("--workspace-dep"),
        "expected a rejection naming both dependency-keyed mechanisms, got:\n{err}"
    );
}

/// Two independent authorities over one decision must not contradict each other silently. A spec
/// carrying BOTH a local marker with the directive AND a dep-scoped extern under
/// `--extern-wrapper-index` keeps each wrapper on its own authority: the local one's listed class
/// defers to the declared path, the dep's indexed one to the dep's collections module.
#[test]
fn coexists_with_the_dependency_keyed_index() {
    let index = std::env::temp_dir().join(format!(
        "cddl_codegen_extcomp_index_{}.rs",
        std::process::id()
    ));
    std::fs::write(&index, "pub use crate::generated::IdxFooList;\n").unwrap();
    let map = generate_dir(
        &[
            (
                "main.cddl",
                "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions cml_chain_wasm=TmList\n\
                 holder = [own: [* tm], dep: [* idx_foo]]\n",
            ),
            (
                "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/idx_dep/mod.cddl",
                "idx_foo = _CDDL_CODEGEN_EXTERN_TYPE_",
            ),
        ],
        &[
            &format!("--extern-wrapper-index=idx_dep={}", index.to_str().unwrap()),
            "--extern-wasm-crate=idx_dep=idx_dep_wasm",
        ],
        true,
        "coexist",
    )
    .expect("generation should succeed");
    let _ = std::fs::remove_file(&index);
    let src = joined(&map);
    assert!(
        src.contains("use cml_chain_wasm::TmList;"),
        "the directive's class must defer to the declared sibling path, got:\n{src}"
    );
    assert!(
        src.contains("use idx_dep_wasm::collections::IdxFooList;"),
        "the indexed class must defer to the dependency's collections module, got:\n{src}"
    );
    for class in ["pub struct TmList", "pub struct IdxFooList"] {
        assert!(
            !src.contains(class),
            "neither deferred class may be minted locally: found `{class}`"
        );
    }
}
