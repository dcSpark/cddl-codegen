//! Direct coverage of `export()`'s write tail (`generation::write_tail`) — every byte the tool puts
//! on disk after the content is decided, and every read of prior output it performs.
//!
//! These tests drive the REAL write path with a synthetic file map, a temp dir and NO CDDL: no
//! parse, no `IntermediateTypes`, no `GenerationScope`. That is the point of the extraction. Before
//! it, each property here was reachable only through a full spec-bearing generation, so a case had
//! to be expressible as a CDDL spec plus flags, and the assertions ran against whatever tree that
//! spec happened to produce. Here the input IS the case: a two-file map with one import, a prior
//! file carrying exactly one replace block, an orphan planted by hand.
//!
//! What that buys, stated as the contract each test pins:
//!   * seed-once is an existence check and nothing more (W1);
//!   * the manifest changeset merges, and an unparseable manifest is a hard error (W2);
//!   * the comment/code-preservation overlay's re-prune is FAMILY-WIDE — a replace block in a
//!     descendant prunes an import out of the parent's WRITTEN bytes (W3);
//!   * an unplaceable comment fails loudly, never silently (W4);
//!   * run-twice = run-once even with a replace block in play (W5);
//!   * the tail reads no prior output that could change what is written (W6);
//!   * the stale-file scan reports and never deletes (W7);
//!   * the diagnostics are byte-inert — with both of them FIRING, the tree is identical (W8).
//!   * composed runtime statics preserve per-file user material and reach a fixed point (W9);
//!   * hand-owned static-crate writes preserve their root, merge their manifest, and issue
//!     existence-gated module notices (W10).
//!
//! Deliberately not here: what a given CDDL spec generates. That is the snapshot corpus's job.

use crate::cargo_manifest::{KeyPath, ManifestOp};
use crate::generation::rustfmt_generated_string;
use crate::generation::write_tail::{
    StaticCrateWrite, WriteTailPlan, stale_orphans, workspace_package_name_collisions,
};
use crate::tests::integration_tests::checkout_hash;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// A scratch directory of this test's own, suffixed by the checkout hash like every other temp-dir
/// consumer here (two checkouts of this repo run their suites concurrently).
fn scratch(case: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "cddl_codegen_write_tail_{case}_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn rustfmt(source: &str) -> String {
    rustfmt_generated_string(source).unwrap().into_owned()
}

/// Turn a hand-written map into what the CONTENT side hands the tail: alloc imports injected, every
/// `.rs` rustfmt'd. rustfmt-stability is load-bearing for the overlay (an unformatted fixture would
/// make the second run's fresh tokens differ from the written ones and trap comments with no input
/// change), so fixtures go through the same formatter the generator does.
fn finalize(files: &[(&str, &str)]) -> BTreeMap<String, String> {
    let mut map: BTreeMap<String, String> = files
        .iter()
        .map(|(path, content)| {
            (
                (*path).to_owned(),
                if path.ends_with(".rs") {
                    rustfmt(content)
                } else {
                    (*content).to_owned()
                },
            )
        })
        .collect();
    for path in crate::alloc_import_inject::inject_generated_files(&mut map) {
        let formatted = rustfmt(&map[&path]);
        map.insert(path, formatted);
    }
    map
}

/// A plan whose two roots are the same scratch dir (no `--package-json` nesting), preservation on
/// (the shipped default). Everything a case does not exercise stays `Default`.
fn plan(dir: &Path, files: BTreeMap<String, String>) -> WriteTailPlan {
    WriteTailPlan {
        files,
        rust_dir: dir.to_path_buf(),
        output_dir: dir.to_path_buf(),
        preserve_comments: true,
        ..Default::default()
    }
}

/// Every file under `dir`, keyed by its path relative to `dir`, as raw bytes — the unit the
/// byte-identity assertions compare.
fn snapshot(dir: &Path) -> BTreeMap<String, Vec<u8>> {
    fn walk(root: &Path, dir: &Path, out: &mut BTreeMap<String, Vec<u8>>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                let rel = path.strip_prefix(root).unwrap().display().to_string();
                out.insert(rel, std::fs::read(&path).unwrap());
            }
        }
    }
    let mut out = BTreeMap::new();
    walk(dir, dir, &mut out);
    out
}

fn read(dir: &Path, rel: &str) -> String {
    std::fs::read_to_string(dir.join(rel)).unwrap_or_else(|e| panic!("reading {rel}: {e}"))
}

/// A delete-only or swapping `cddl-codegen:replace` block over one whole-line item: the user section
/// (possibly empty) followed by the `//`-commented recorded original the overlay re-finds in the
/// regenerated item.
fn replace_block(user: &str, original: &str) -> String {
    let user_section = if user.is_empty() {
        String::new()
    } else {
        format!("{user}\n")
    };
    format!(
        "// cddl-codegen:replace-start\n\
         {user_section}// cddl-codegen:replaces\n\
         // {original}\n\
         // cddl-codegen:replace-end"
    )
}

/// Rewrite an on-disk generated file by replacing `needle` (once) with `replacement` — how a user's
/// edit to prior output reaches the next run.
fn edit_prior(dir: &Path, rel: &str, needle: &str, replacement: &str) {
    let text = read(dir, rel);
    assert!(
        text.contains(needle),
        "fixture drift: `{needle}` is not in the written {rel}:\n{text}"
    );
    std::fs::write(dir.join(rel), text.replacen(needle, replacement, 1)).unwrap();
}

fn set_op(path: &[&str], value: &str) -> (KeyPath, ManifestOp) {
    (
        path.iter().map(|s| (*s).to_owned()).collect(),
        ManifestOp::Set {
            value: toml_edit::value(value),
            assert_source: false,
        },
    )
}

fn seed_once_op(path: &[&str], value: &str) -> (KeyPath, ManifestOp) {
    (
        path.iter().map(|s| (*s).to_owned()).collect(),
        ManifestOp::SeedOnce(toml_edit::value(value)),
    )
}

const STATIC_CRATE_NOTICE_DIR: &str = "CDDL_CODEGEN_WRITE_TAIL_STATIC_CRATE_NOTICE_DIR";
const STATIC_CRATE_NOTICE_SENTINEL: &str = "cddl-codegen static-crate notice helper ran";

/// The direct `StaticCrateWrite` fixture shared by the parent assertions and the child that
/// captures the write-tail warning stream. Its generated-output root is deliberately distinct
/// from `target`: `--export-static-crate` must never treat the hand-owned target as output.
fn static_crate_plan(output: &Path, target: &Path) -> WriteTailPlan {
    let mut p = plan(output, BTreeMap::new());
    p.static_crate = Some(StaticCrateWrite {
        dir: target.to_path_buf(),
        runtime_files: vec![
            (
                "existing.rs".to_owned(),
                "pub struct ExistingFresh;\n".to_owned(),
            ),
            (
                "new_runtime.rs".to_owned(),
                "pub struct NewRuntime;\n".to_owned(),
            ),
        ],
        serialization: "pub struct SerializationPrelude;\n".to_owned(),
        manifest_ops: vec![set_op(&["dependencies", "cbor_event"], "2.4")],
    });
    p
}

/// Run the one-shot child and, only after its exit status has proved the exact helper ran
/// successfully, return its stderr for notice assertions.
fn static_crate_notice_stderr(dir: &Path) -> String {
    let exe = std::env::current_exe().expect("the running test binary must have a path");
    let output = std::process::Command::new(&exe)
        .args([
            "--exact",
            "tests::write_tail_tests::write_tail_static_crate_notice_helper",
            "--nocapture",
            "--test-threads=1",
        ])
        .env(STATIC_CRATE_NOTICE_DIR, dir)
        .output()
        .unwrap_or_else(|e| panic!("could not spawn {exe:?} for {dir:?}: {e}"));
    assert!(
        output.status.success(),
        "the static-crate notice child failed (status {:?})\\nstdout:\\n{}\\nstderr:\\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains(STATIC_CRATE_NOTICE_SENTINEL),
        "the child exited successfully but did not run the exact static-crate helper; its test \\
         filter may be stale\\nstdout:\\n{}",
        String::from_utf8_lossy(&output.stdout),
    );
    String::from_utf8(output.stderr).expect("the test binary's stderr must be UTF-8")
}

/// W1. The seed-once crate roots are written when ABSENT and never again — an existence check, not
/// a content comparison, so even a hand-mangled root survives untouched. Everything under
/// `generated/**` clobbers regardless of what is there.
#[test]
fn write_tail_seeds_crate_roots_once_and_clobbers_generated() {
    let dir = scratch("seed_once");
    let files = finalize(&[
        (
            "rust/src/lib.rs",
            "pub mod generated;\npub use generated::*;\n",
        ),
        ("rust/src/generated/mod.rs", "pub struct Fresh;\n"),
    ]);

    plan(&dir, files.clone()).run().unwrap();
    assert_eq!(read(&dir, "rust/src/lib.rs"), files["rust/src/lib.rs"]);
    assert_eq!(
        read(&dir, "rust/src/generated/mod.rs"),
        files["rust/src/generated/mod.rs"]
    );

    // A hand-owned root the user has since edited, and a generated file a prior run left behind.
    let hand_root = format!("{}\npub mod hand_wiring;\n", files["rust/src/lib.rs"]);
    std::fs::write(dir.join("rust/src/lib.rs"), &hand_root).unwrap();
    std::fs::write(
        dir.join("rust/src/generated/mod.rs"),
        "pub struct FromAPriorRun;\n",
    )
    .unwrap();

    plan(&dir, files.clone()).run().unwrap();
    assert_eq!(
        read(&dir, "rust/src/lib.rs"),
        hand_root,
        "an existing crate root is seed-SKIPPED: the tool must not rewrite one byte of it"
    );
    assert_eq!(
        read(&dir, "rust/src/generated/mod.rs"),
        files["rust/src/generated/mod.rs"],
        "everything under generated/** clobbers — the seed-once carve-out is the roots only"
    );
}

/// W2. The manifest changeset merges onto whatever is on disk: keys no op mentions pass through,
/// `Set` overwrites, `SeedOnce` writes only when absent, and an unparseable existing manifest is a
/// hard error naming the file rather than a silent clobber of the user's TOML.
#[test]
fn write_tail_manifest_changeset_merges_and_refuses_unparseable() {
    let dir = scratch("manifest");
    let files = finalize(&[
        ("rust/Cargo.toml", "[package]\nname = \"placeholder\"\n"),
        ("rust/src/generated/mod.rs", "pub struct A;\n"),
    ]);
    let ops = vec![
        (
            "rust/Cargo.toml",
            vec![
                set_op(&["package", "name"], "generated-lib"),
                seed_once_op(&["package", "version"], "0.1.0"),
            ],
        ),
        (
            "wasm/Cargo.toml",
            vec![set_op(&["package", "name"], "nope")],
        ),
    ];

    // No manifest on disk: the changeset lands whole.
    let mut p = plan(&dir, files.clone());
    p.manifest_ops = ops.clone();
    p.run().unwrap();
    let fresh = read(&dir, "rust/Cargo.toml");
    assert!(fresh.contains("name = \"generated-lib\""), "{fresh}");
    assert!(fresh.contains("version = \"0.1.0\""), "{fresh}");
    // A changeset for a manifest the file map does not carry is not written at all — the map is
    // what decides which crates this run ships.
    assert!(!dir.join("wasm/Cargo.toml").exists());

    // A manifest the user has edited: their key survives, `Set` re-asserts, `SeedOnce` stands off.
    std::fs::write(
        dir.join("rust/Cargo.toml"),
        "[package]\nname = \"renamed-by-hand\"\nversion = \"9.9.9\"\nauthors = [\"me\"]\n\n\
         [dependencies]\nmine = \"1\"\n",
    )
    .unwrap();
    let mut p = plan(&dir, files.clone());
    p.manifest_ops = ops.clone();
    p.run().unwrap();
    let merged = read(&dir, "rust/Cargo.toml");
    assert!(
        merged.contains("name = \"generated-lib\""),
        "a tool-owned key is re-asserted every run:\n{merged}"
    );
    assert!(
        merged.contains("version = \"9.9.9\""),
        "SeedOnce checks existence only — an existing value is never re-decided:\n{merged}"
    );
    assert!(
        merged.contains("authors = [\"me\"]") && merged.contains("mine = \"1\""),
        "keys the op set does not mention pass through:\n{merged}"
    );

    // Unparseable: a hard error naming the file, and the user's bytes are still there afterwards.
    std::fs::write(dir.join("rust/Cargo.toml"), "this is not ) toml [[[\n").unwrap();
    let mut p = plan(&dir, files);
    p.manifest_ops = ops;
    let err = p.run().unwrap_err();
    assert!(
        err.to_string().contains("rust/Cargo.toml"),
        "the error must name the manifest that failed to parse, got: {err}"
    );
    assert_eq!(read(&dir, "rust/Cargo.toml"), "this is not ) toml [[[\n");
}

/// W3a. A user comment declared with `cddl-codegen:keep` is carried onto the fresh content.
#[test]
fn write_tail_overlay_carries_a_kept_user_comment() {
    let dir = scratch("overlay_comment");
    let files = finalize(&[(
        "rust/src/generated/mod.rs",
        "pub struct A;\npub struct B;\n",
    )]);

    plan(&dir, files.clone()).run().unwrap();
    edit_prior(
        &dir,
        "rust/src/generated/mod.rs",
        "pub struct B;",
        "// cddl-codegen:keep why B is here\npub struct B;",
    );

    plan(&dir, files).run().unwrap();
    let written = read(&dir, "rust/src/generated/mod.rs");
    assert!(
        written.contains("// cddl-codegen:keep why B is here"),
        "a kept user comment must survive a regeneration:\n{written}"
    );
    assert!(
        !written.contains("compile_error!"),
        "a placeable comment must not be trapped:\n{written}"
    );
}

/// W3b. The post-overlay re-prune is FAMILY-WIDE, and that is the property no per-file overlay could
/// have: a `cddl-codegen:replace` block in a DESCENDANT deletes the last user of an import the
/// PARENT `mod.rs` carries, and the parent's written bytes lose that import.
#[test]
fn write_tail_overlay_reprune_drops_a_parent_import_a_descendant_orphaned() {
    let dir = scratch("overlay_reprune");
    let files = finalize(&[
        (
            "rust/src/generated/mod.rs",
            "pub mod sub;\nuse crate::hand::Widget;\npub struct Parent;\n",
        ),
        (
            "rust/src/generated/sub/mod.rs",
            "use super::*;\npub struct Uses(Widget);\n",
        ),
    ]);
    let config = || {
        let mut config = crate::import_prune::PruneConfig::default();
        config.extra_candidates.insert("Widget".to_owned());
        config
    };

    let mut p = plan(&dir, files.clone());
    p.prune_config = config();
    p.run().unwrap();
    assert!(
        read(&dir, "rust/src/generated/mod.rs").contains("use crate::hand::Widget;"),
        "the import is justified while the descendant names it — the negative control"
    );

    // The user replaces the descendant's only user of `Widget` with a hand type that names neither.
    edit_prior(
        &dir,
        "rust/src/generated/sub/mod.rs",
        "pub struct Uses(Widget);",
        &replace_block("pub struct Uses(u32);", "pub struct Uses(Widget);"),
    );

    let mut p = plan(&dir, files);
    p.prune_config = config();
    p.run().unwrap();
    let parent = read(&dir, "rust/src/generated/mod.rs");
    assert!(
        !parent.contains("use crate::hand::Widget;"),
        "a replace block in a DESCENDANT orphaned this import; only a family-wide re-prune over the \
         post-overlay map sees that, and the written parent must reflect it:\n{parent}"
    );
    let child = read(&dir, "rust/src/generated/sub/mod.rs");
    assert!(
        child.contains("pub struct Uses(u32);"),
        "the user's replacement is what ships:\n{child}"
    );
}

/// W3c. The alloc-import recompute after the overlay both ADDS and REMOVES: a replace block that
/// swaps the last `String` for a `Vec` drops the string import and gains the vec one. The pruner
/// cannot do this half of the job (it never adds, and never touches trait imports), which is why
/// the injector is re-run rather than the prune alone.
#[test]
fn write_tail_overlay_recomputes_alloc_imports_both_ways() {
    let dir = scratch("overlay_alloc");
    let files = finalize(&[(
        "rust/src/generated/alloc_demo.rs",
        "pub struct Demo(String);\n",
    )]);
    assert!(
        files["rust/src/generated/alloc_demo.rs"].contains("use alloc::string::String;"),
        "fixture premise: the content side injects the import the fresh content needs"
    );

    plan(&dir, files.clone()).run().unwrap();
    edit_prior(
        &dir,
        "rust/src/generated/alloc_demo.rs",
        "pub struct Demo(String);",
        &replace_block("pub struct Demo(Vec<u8>);", "pub struct Demo(String);"),
    );

    plan(&dir, files).run().unwrap();
    let written = read(&dir, "rust/src/generated/alloc_demo.rs");
    assert!(
        written.contains("use alloc::vec::Vec;"),
        "the replacement introduced a new alloc name — the recompute must ADD its import:\n{written}"
    );
    assert!(
        !written.contains("use alloc::string::String;"),
        "the replacement removed the last `String` — the recompute must REMOVE its import:\n{written}"
    );
}

/// W4. Never-silent, both directions: a comment the overlay cannot re-place becomes a tagged
/// `compile_error!` block (the user reviews it at build time), and a prior file that cannot be READ
/// is a hard error naming it rather than a clobber.
#[test]
fn write_tail_overlay_traps_an_unplaceable_comment_and_refuses_an_unreadable_prior() {
    let dir = scratch("overlay_unplaceable");
    let before = finalize(&[(
        "rust/src/generated/mod.rs",
        "pub struct Keep;\npub struct Gone;\n",
    )]);
    let after = finalize(&[("rust/src/generated/mod.rs", "pub struct Keep;\n")]);

    plan(&dir, before).run().unwrap();
    // A kept comment anchored to an item the next run no longer generates (a removed CDDL rule).
    edit_prior(
        &dir,
        "rust/src/generated/mod.rs",
        "pub struct Gone;",
        "// cddl-codegen:keep this annotates a type that is about to disappear\npub struct Gone;",
    );

    plan(&dir, after.clone()).run().unwrap();
    let written = read(&dir, "rust/src/generated/mod.rs");
    assert!(
        written.contains("cddl-codegen:unpreserved-comment") && written.contains("compile_error!"),
        "an unplaceable comment must fail LOUDLY, never drop silently:\n{written}"
    );
    assert!(
        written.contains("this annotates a type that is about to disappear"),
        "the trapped block must carry the comment's text so the user can act on it:\n{written}"
    );

    // A prior file that is not UTF-8 at all: hard error, naming the file.
    std::fs::write(
        dir.join("rust/src/generated/mod.rs"),
        [0x66, 0x6e, 0xff, 0xfe],
    )
    .unwrap();
    let err = plan(&dir, after).run().unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("rust/src/generated/mod.rs") && msg.contains("cannot read the existing"),
        "an unreadable prior file must be named in the error, got: {msg}"
    );
}

/// W5. Run twice = run once, WITH a replace block in play. This is the fixed point the whole overlay
/// design rests on: the second run regenerates the same fresh content, re-applies the same block,
/// re-prunes the same imports and re-formats to the same bytes.
#[test]
fn write_tail_is_a_fixed_point_over_a_replace_block_bearing_prior() {
    let dir = scratch("fixed_point");
    let files = finalize(&[(
        "rust/src/generated/mod.rs",
        "pub struct Keep;\npub struct Swapped(u32);\n",
    )]);

    plan(&dir, files.clone()).run().unwrap();
    edit_prior(
        &dir,
        "rust/src/generated/mod.rs",
        "pub struct Swapped(u32);",
        &replace_block("pub struct Swapped(u64);", "pub struct Swapped(u32);"),
    );
    // A kept comment as well, so the fixed point covers both overlay channels at once.
    edit_prior(
        &dir,
        "rust/src/generated/mod.rs",
        "pub struct Keep;",
        "// cddl-codegen:keep why Keep is kept\npub struct Keep;",
    );

    plan(&dir, files.clone()).run().unwrap();
    let first = snapshot(&dir);
    plan(&dir, files).run().unwrap();
    let second = snapshot(&dir);
    assert_eq!(
        first, second,
        "regenerating over the tail's own output must reproduce it byte for byte"
    );
    let written = String::from_utf8(first["rust/src/generated/mod.rs"].clone()).unwrap();
    assert!(
        written.contains("pub struct Swapped(u64);") && written.contains("cddl-codegen:replaces"),
        "the block itself is what carries forward:\n{written}"
    );
}

/// W6. The no-prior-output bound: identical inputs into two dirs that have never been written
/// produce byte-identical trees. Nothing the tail reads from a directory can make the same plan
/// emit different bytes.
#[test]
fn write_tail_same_inputs_two_fresh_dirs_are_byte_identical() {
    let a = scratch("fresh_a");
    let b = scratch("fresh_b");
    let files = finalize(&[
        ("rust/Cargo.toml", "[package]\nname = \"x\"\n"),
        ("rust/src/lib.rs", "pub mod generated;\n"),
        ("rust/src/generated/mod.rs", "pub struct A;\n"),
        ("wasm/src/generated/mod.rs", "pub struct W;\n"),
    ]);
    let ops = vec![(
        "rust/Cargo.toml",
        vec![set_op(&["package", "name"], "generated-lib")],
    )];

    for dir in [&a, &b] {
        let mut p = plan(dir, files.clone());
        p.manifest_ops = ops.clone();
        p.extern_interface_files = BTreeMap::from([(
            "extern-interface/dep/x.cddl".to_owned(),
            "x = uint\n".to_owned(),
        )]);
        p.no_std_check_files = BTreeMap::from([(
            "no-std-check/src/lib.rs".to_owned(),
            "#![no_std]\n".to_owned(),
        )]);
        p.run().unwrap();
    }
    assert_eq!(snapshot(&a), snapshot(&b));
}

/// W7. The stale-file scan reports and never deletes, and the two always-clobbered trees are
/// delete-and-recreated so they cannot orphan anything in the first place.
#[test]
fn write_tail_reports_orphans_without_deleting_and_recreates_the_wit_tree() {
    let dir = scratch("stale");
    let files = finalize(&[
        ("rust/src/generated/mod.rs", "pub struct A;\n"),
        ("component/wit/world.wit", "package a:b;\n"),
    ]);

    let mut p = plan(&dir, files.clone());
    p.component = true;
    p.run().unwrap();

    // What a removed rule leaves behind: a `.rs` under a tool-owned tree this run does not write,
    // and a `.wit` in the delete-and-recreated package.
    let orphan = dir.join("rust/src/generated/ghost_from_a_prior_run.rs");
    std::fs::write(
        &orphan,
        "// a comment a user added here\npub struct Ghost;\n",
    )
    .unwrap();
    let stale_wit = dir.join("component/wit/gone.wit");
    std::fs::write(&stale_wit, "package gone:gone;\n").unwrap();

    let mut p = plan(&dir, files);
    p.component = true;
    p.run().unwrap();

    let written = [dir.join("rust/src/generated/mod.rs")]
        .into_iter()
        .collect();
    let reported = stale_orphans(&dir, &written).unwrap();
    assert_eq!(
        reported,
        vec![orphan.clone()],
        "the scan must name the orphan — and only the orphan; a file this run wrote is not stale"
    );
    assert!(
        orphan.exists(),
        "the scan is diagnostic-only: it reports the orphan and leaves it to the user"
    );
    assert!(
        !stale_wit.exists(),
        "the WIT package is delete-and-recreated — a stale `.wit` would keep resolving as part of it"
    );
    assert!(dir.join("component/wit/world.wit").exists());
}

/// W8. The diagnostics that read prior output are byte-INERT. Two runs of the same plan, one of them
/// with a workspace name collision AND a seed-skipped root missing a required re-export — both
/// diagnostics asserted to actually fire — write identical trees. This is the "delete the call and
/// every emitted file is identical" contract, asserted rather than argued.
#[test]
fn write_tail_diagnostic_reads_change_no_written_byte() {
    let quiet_parent = scratch("inert_quiet");
    let loud_parent = scratch("inert_loud");
    let quiet = quiet_parent.join("out");
    let loud = loud_parent.join("out");
    std::fs::create_dir_all(&quiet).unwrap();
    std::fs::create_dir_all(&loud).unwrap();

    let files = finalize(&[
        ("rust/Cargo.toml", "[package]\nname = \"x\"\n"),
        (
            "rust/src/lib.rs",
            "pub mod generated;\npub use generated::*;\n",
        ),
        (
            "rust/src/generated/mod.rs",
            "pub use crate::MyExt;\npub struct A;\n",
        ),
    ]);
    let ops = vec![(
        "rust/Cargo.toml",
        vec![set_op(&["package", "name"], "generated-lib")],
    )];

    // Both sides get the same PRE-EXISTING hand root, so the seed-once skip is identical; only the
    // diagnostic INPUTS differ (the required set, and the surrounding workspace).
    let hand_root = "pub mod generated;\npub use generated::*;\n// hand wiring\n";
    for dir in [&quiet, &loud] {
        std::fs::create_dir_all(dir.join("rust/src")).unwrap();
        std::fs::write(dir.join("rust/src/lib.rs"), hand_root).unwrap();
    }
    // A workspace whose other member already claims the generated package name — above the compared
    // tree, so the collision input is not itself part of what is diffed.
    std::fs::create_dir_all(loud_parent.join("other")).unwrap();
    std::fs::write(
        loud_parent.join("other/Cargo.toml"),
        "[package]\nname = \"generated-lib\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        loud_parent.join("Cargo.toml"),
        "[workspace]\nmembers = [\"other\", \"out/rust\"]\nresolver = \"2\"\n",
    )
    .unwrap();

    let mut p = plan(&quiet, files.clone());
    p.manifest_ops = ops.clone();
    p.run().unwrap();

    let mut p = plan(&loud, files);
    p.manifest_ops = ops;
    p.required_rust_reexports = ["MyExt".to_owned()].into_iter().collect();
    p.run().unwrap();

    // Both diagnostics really fired on the loud side (an inertness claim proved by a run where
    // nothing happened would prove nothing).
    let collisions = workspace_package_name_collisions(
        &loud,
        &[("rust/Cargo.toml".to_owned(), "generated-lib".to_owned())],
    );
    assert_eq!(
        collisions.len(),
        1,
        "the workspace collision scan must have had something to report"
    );
    assert_eq!(
        crate::generation::export::missing_reexports(
            hand_root,
            &["MyExt".to_owned()].into_iter().collect()
        ),
        vec!["MyExt"],
        "the seed-skipped root must have been missing a required re-export"
    );

    assert_eq!(
        snapshot(&quiet),
        snapshot(&loud),
        "a diagnostic read may emit stderr and nothing else — no written byte may depend on it"
    );
}

/// W9. Composed runtime files are outside the map-level overlay/re-prune, so this directly pins
/// their per-file `write_rs_with_preserve` route: the expected relative file is written, a kept
/// comment survives, and a second regeneration is byte-for-byte identical. The small
/// preservation-off control proves this is not merely an accidental fresh-content fixed point.
#[test]
fn write_tail_composed_runtime_files_preserve_per_file_and_fix_point() {
    let dir = scratch("composed_runtime");
    let rel = "rust/src/generated/runtime_piece.rs";
    let fresh = rustfmt("pub struct RuntimePiece;\n");
    std::fs::create_dir_all(dir.join("rust/src/generated")).unwrap();

    let composed = |preserve_comments| {
        let mut p = plan(&dir, BTreeMap::new());
        p.preserve_comments = preserve_comments;
        p.composed_runtime_files = vec![(rel.to_owned(), fresh.clone())];
        p
    };
    composed(true).run().unwrap();
    assert_eq!(
        read(&dir, rel),
        fresh,
        "the composed relative path is written"
    );

    edit_prior(
        &dir,
        rel,
        "pub struct RuntimePiece;",
        "// cddl-codegen:keep runtime rationale\npub struct RuntimePiece;",
    );
    composed(true).run().unwrap();
    let preserved = read(&dir, rel);
    assert!(
        preserved.contains("// cddl-codegen:keep runtime rationale"),
        "the per-file route must carry a valid preserved comment:\n{preserved}"
    );
    composed(true).run().unwrap();
    assert_eq!(
        read(&dir, rel),
        preserved,
        "the per-file preservation route is a fixed point on its own prior output"
    );

    let clobber = scratch("composed_runtime_no_preserve");
    std::fs::create_dir_all(clobber.join("rust/src/generated")).unwrap();
    let mut no_preserve = plan(&clobber, BTreeMap::new());
    no_preserve.preserve_comments = false;
    no_preserve.composed_runtime_files = vec![(rel.to_owned(), fresh.clone())];
    no_preserve.run().unwrap();
    edit_prior(
        &clobber,
        rel,
        "pub struct RuntimePiece;",
        "// cddl-codegen:keep discarded by no-preserve\npub struct RuntimePiece;",
    );
    let mut no_preserve = plan(&clobber, BTreeMap::new());
    no_preserve.preserve_comments = false;
    no_preserve.composed_runtime_files = vec![(rel.to_owned(), fresh.clone())];
    no_preserve.run().unwrap();
    assert_eq!(
        read(&clobber, rel),
        fresh,
        "without preservation, the same composed file clobbers back to fresh content"
    );
}

/// W10 child helper. It is inert in ordinary discovery/execution; the parent invokes this exact
/// fully-qualified name with `--nocapture` and a task-specific scratch dir to capture `warn!`'s
/// stderr without changing the production logging contract.
#[test]
fn write_tail_static_crate_notice_helper() {
    let Ok(dir) = std::env::var(STATIC_CRATE_NOTICE_DIR) else {
        return;
    };
    let dir = PathBuf::from(dir);
    static_crate_plan(&dir.join("generated-output"), &dir.join("static-target"))
        .run()
        .unwrap();
    println!("{STATIC_CRATE_NOTICE_SENTINEL}");
}

/// W10. `--export-static-crate` writes only the hand-owned target's `src/` files and merged
/// manifest. Its root remains byte-untouched, all writes become a fixed point, and existence just
/// before each write is the sole input to the new-static-file notice.
#[test]
fn write_tail_static_crate_preserves_hand_root_merges_manifest_and_gates_notices() {
    let dir = scratch("static_crate");
    let target = dir.join("static-target");
    let hand_root = "// hand-owned root\npub mod existing;\n";
    std::fs::create_dir_all(target.join("src")).unwrap();
    std::fs::write(target.join("src/lib.rs"), hand_root).unwrap();
    // This runtime file predates the export, so the first child must update it silently while it
    // reports both files that are genuinely new.
    std::fs::write(target.join("src/existing.rs"), "pub struct Old;\n").unwrap();
    std::fs::write(
        target.join("Cargo.toml"),
        "[package]\nname = \"hand-static\"\nversion = \"9.9.9\"\n\n\
         [dependencies]\nhand_dep = \"1\"\ncbor_event = \"old\"\n",
    )
    .unwrap();

    let first_stderr = static_crate_notice_stderr(&dir);
    assert_eq!(
        read(&target, "src/lib.rs"),
        hand_root,
        "the hand-owned root is untouched"
    );
    assert_eq!(
        read(&target, "src/existing.rs"),
        "pub struct ExistingFresh;\n",
        "an existing runtime file is still refreshed"
    );
    assert_eq!(
        read(&target, "src/new_runtime.rs"),
        "pub struct NewRuntime;\n"
    );
    assert_eq!(
        read(&target, "src/serialization.rs"),
        "pub struct SerializationPrelude;\n"
    );
    let manifest = read(&target, "Cargo.toml");
    assert!(
        manifest.contains("name = \"hand-static\"")
            && manifest.contains("version = \"9.9.9\"")
            && manifest.contains("hand_dep = \"1\""),
        "package and hand-owned dependency keys survive the manifest changeset:\n{manifest}"
    );
    assert!(
        manifest.contains("cbor_event = \"2.4\""),
        "the tool-owned dependency is reasserted:\n{manifest}"
    );
    assert!(
        !first_stderr.contains(&crate::generation::export::new_static_file_notice(
            "existing.rs"
        )),
        "an already-present runtime file is silent:\n{first_stderr}"
    );
    for filename in ["new_runtime.rs", "serialization.rs"] {
        let notice = crate::generation::export::new_static_file_notice(filename);
        assert!(
            first_stderr.contains(&notice),
            "each newly-created static file warns with its pub-mod guidance ({filename}):\n{first_stderr}"
        );
    }

    let first_tree = snapshot(&target);
    let second_stderr = static_crate_notice_stderr(&dir);
    assert!(
        !second_stderr.contains("NEW static file"),
        "the immediate second static export is notice-silent:\n{second_stderr}"
    );
    assert_eq!(
        snapshot(&target),
        first_tree,
        "the hand-owned static target is a byte-for-byte fixed point on the second run"
    );

    std::fs::remove_file(target.join("src/new_runtime.rs")).unwrap();
    let reintroduced_stderr = static_crate_notice_stderr(&dir);
    assert_eq!(
        reintroduced_stderr.matches("NEW static file").count(),
        1,
        "deleting exactly one runtime file produces exactly one new-static-file notice:\n{reintroduced_stderr}"
    );
    assert!(
        reintroduced_stderr.contains(&crate::generation::export::new_static_file_notice(
            "new_runtime.rs"
        )),
        "the reintroduced file is the one named by the notice:\n{reintroduced_stderr}"
    );
}
