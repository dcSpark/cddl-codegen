//! `--extern-import <dep>=<path>`: consumer-side consumption of a dependency's committed
//! extern-interface export (commit 6).
//!
//! The flag reads a dep's `extern-interface/<dep>/**` export and concatenates it with EXTERN_DEPS_DIR
//! scope markers, so its rules land in the same non-exported `<dep>` scope a physical hand-stub tree
//! would — after which the whole extern-deps pathway is unchanged. The acceptance criterion is
//! byte-identity: a consumer generated once from a faithful physical stub and once via
//! `--extern-import` at the minted export must produce identical rust output. The stub is the export
//! minus its version header (what a careful human would write TODAY — current derivation, the pins
//! the export carries match the derived names), so the comparison isolates the assembly seam.
//!
//! These tests drive the generator end-to-end over scratch directories (mirroring the
//! `tests/extern-deps/` shape into a scratch dir, per AGENTS.md — never a real consumer checkout) and
//! the committed source specs under `tests/extern-import/`.

use crate::cli::Cli;
use clap::Parser;
use std::collections::BTreeMap;

fn fixture(rel: &str) -> String {
    std::fs::read_to_string(std::path::Path::new("tests/extern-import").join(rel))
        .unwrap_or_else(|e| panic!("reading fixture {rel}: {e}"))
}

/// A unique scratch directory for one test (cleaned first).
fn scratch(tag: &str) -> std::path::PathBuf {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_extern_import_{tag}_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    root
}

fn write(root: &std::path::Path, rel: &str, content: &str) {
    let path = root.join(rel);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(&path, content).unwrap();
}

/// Mint a dependency's extern-interface export in-process (the SAME projection `export` writes to
/// disk), keyed by path relative to `<output>` (`extern-interface/<dep_key>/…/mod.cddl`).
fn mint_export(dep_spec: &str, dep_key: &str, tag: &str) -> BTreeMap<String, String> {
    let root = scratch(&format!("mint_{tag}"));
    write(&root, "lib.cddl", dep_spec);
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        root.join("lib.cddl").to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "false",
        "--lib-name",
        dep_key,
    ]);
    let files = crate::api::extern_interface_strings(&cli)
        .expect("dep export projection must succeed (exclude-with-record, never abort)");
    let _ = std::fs::remove_dir_all(&root);
    files
}

/// Generate a consumer's rust source map from a directory input (physical-stub path) or a single
/// file plus flags (`--extern-import` path). Returns post-rustfmt source keyed by path, or the
/// stringified graceful `Err`.
fn generate(input: &std::path::Path, extra: &[&str]) -> Result<BTreeMap<String, String>, String> {
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "false",
    ];
    args.extend_from_slice(extra);
    let cli = Cli::parse_from(args);
    crate::api::generated_strings(&cli).map_err(|e| e.to_string())
}

/// Seam-identity half of the acceptance criterion. Consume the dep's minted export two ways — a
/// physical stub that is the export minus the version header (pins INCLUDED) and `--extern-import`
/// at the export tree — and require the consumer's generated rust output byte-identical. This
/// isolates the marker-assembly seam: identical rule text through either channel must land in the
/// same scopes and produce the same bytes. The migration half (pinless stub) is the test below.
#[test]
fn extern_import_matches_hand_stub_byte_for_byte() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "byteid");
    let consumer = fixture("consumer/lib.cddl");

    // Run A — physical hand-stub: consumer at the tree root (lib.cddl -> ROOT_SCOPE) + a stub under
    // _CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/ = the export minus its header line.
    let stub_root = scratch("byteid_stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        let stub_body = strip_header(content);
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &stub_body,
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("physical-stub generation must succeed");

    // Run B — --extern-import: a single-file consumer + the export tree written verbatim (header
    // intact), pointed at by the flag.
    let flag_root = scratch("byteid_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("byteid_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert_eq!(
        via_flag.keys().collect::<Vec<_>>(),
        via_stub.keys().collect::<Vec<_>>(),
        "the generated file SET must match between --extern-import and the physical stub"
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "byte-identity broke for {path}:\n--- via --extern-import ---\n{}\n--- via physical stub ---\n{stub_content}",
            via_flag.get(path).map(String::as_str).unwrap_or("<absent>")
        );
    }
}

/// The MIGRATION half of the acceptance criterion (plan §6 commit 6: "a consumer migrated from a
/// faithful hand-stub produces byte-identical generated output"). A faithful hand-stub written
/// today carries NO `@rust_name` pins — a careful author derives names the same way the consumer
/// does — so the honest comparison strips the pins from the stub while the export keeps them.
/// Byte-identity then requires that a pin agreeing with today's derivation changes NOTHING (no
/// `use dep::Foo as Foo;` noise, no de-grouped imports); a pin only takes effect when it differs.
#[test]
fn extern_import_matches_pinless_hand_stub_byte_for_byte() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "pinless");
    let consumer = fixture("consumer/lib.cddl");

    // Run A — the genuine migration source: a pinless hand-stub (header AND pins stripped).
    let stub_root = scratch("pinless_stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        let stub_body = strip_pins(&strip_header(content));
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &stub_body,
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("pinless-stub generation must succeed");

    // Run B — `--extern-import` at the export verbatim (header + pins intact).
    let flag_root = scratch("pinless_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("pinless_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert_eq!(
        via_flag.keys().collect::<Vec<_>>(),
        via_stub.keys().collect::<Vec<_>>(),
        "the generated file SET must match between --extern-import and the pinless stub"
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "migration byte-identity broke for {path}:\n--- via --extern-import ---\n{}\n--- via pinless stub ---\n{stub_content}",
            via_flag.get(path).map(String::as_str).unwrap_or("<absent>")
        );
    }
}

/// WP4 (duplicates policy): `@duplicates reject` MUST project into the dep's extern-interface export,
/// so a consumer regenerating from the export rebuilds the SAME uniqueness twins. Without the
/// projected directive the consumer would embed a preserve-mode `Vec`/`NonEmptyVec` that silently
/// ACCEPTS the duplicates the dep rejects — the exact cross-crate skew this seam exists to kill.
/// Four legs: (1) the export carries the directive on both the `[*]` and `[+]` reject rules; (2) the
/// consumer's OWN deserialize routes the collected elements through the reject door (`OrderedSet` /
/// `NonEmptyOrderedSet::try_from`), so duplicate bytes fail IN the consumer — reject on both sides;
/// (3) byte-identity vs a physical stub carrying the directive (the marker-assembly seam roundtrips
/// it); (4) a NEGATIVE control proving the directive is load-bearing — a stub with it stripped
/// rebuilds a preserve-mode `Vec` with no reject door (the skew, made visible).
#[test]
fn extern_import_projects_duplicates_reject_no_cross_crate_skew() {
    let export = mint_export(&fixture("dep-reject/lib.cddl"), "dep", "dupreject");
    let consumer = fixture("consumer-reject/lib.cddl");

    // (1) the export carries `@duplicates reject` on each collection rule (the pin is appended after).
    let export_body = export
        .values()
        .find(|c| c.contains("reject_uints"))
        .expect("the export must contain the reject rules");
    assert!(
        export_body.contains("reject_uints = [* uint] ; @duplicates reject"),
        "the [*] reject rule must project its directive; got:\n{export_body}"
    );
    assert!(
        export_body.contains("reject_nuints = [+ uint] ; @duplicates reject"),
        "the [+] reject rule must project its directive; got:\n{export_body}"
    );

    // Consume the export via --extern-import.
    let flag_root = scratch("dupreject_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("dupreject_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    // (2) the consumer's OWN deserialize routes duplicate-carrying wire through the reject door —
    // duplicate bytes fail IN the consumer, reject on both sides of the crate boundary.
    let ser = via_flag
        .get("rust/src/generated/serialization.rs")
        .expect("the consumer must emit serialization.rs");
    assert!(
        ser.contains("OrderedSet::try_from"),
        "the consumer must deserialize the [*] reject set through the OrderedSet uniqueness door"
    );
    assert!(
        ser.contains("NonEmptyOrderedSet::try_from"),
        "the consumer must deserialize the [+] reject set through the NonEmptyOrderedSet door"
    );

    // (3) byte-identity vs a physical stub carrying the directive (the seam roundtrips it verbatim).
    let stub_root = scratch("dupreject_stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &strip_header(content),
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("physical-stub generation must succeed");
    assert_eq!(
        via_flag, via_stub,
        "--extern-import must produce byte-identical output to a physical stub carrying @duplicates reject"
    );

    // (4) NEGATIVE control: a stub with the directive STRIPPED rebuilds a preserve-mode `Vec` with no
    // reject door — the silent-accept skew, made visible. This proves the projected directive is
    // load-bearing rather than decorative.
    let skew_root = scratch("dupreject_skew");
    write(&skew_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        let stripped = strip_header(content).replace("@duplicates reject ", "");
        write(
            &skew_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &stripped,
        );
    }
    let via_skew = generate(&skew_root, &[]).expect("skew-stub generation must succeed");
    let skew_ser = via_skew
        .get("rust/src/generated/serialization.rs")
        .expect("skew serialization.rs");
    assert!(
        !skew_ser.contains("OrderedSet::try_from"),
        "with @duplicates reject dropped the consumer must NOT rebuild the reject twin — this is the \
         cross-crate skew the projection prevents"
    );

    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&skew_root);
}

/// Phase 2 (duplicates policy): `@duplicates preserve` on a TABLE MUST project into the dep's
/// extern-interface export — the MIRROR of the reject direction. Table `reject` is the map default
/// (a `BTreeMap` cannot hold duplicate keys), so a consumer that regenerates from an export MISSING
/// the directive rebuilds a reject-default map that silently REJECTS the duplicate keys the dep's
/// `PairMap`/`NonEmptyPairMap` preserves — the same cross-crate skew this seam kills, opposite
/// direction. Four legs, symmetric to `extern_import_projects_duplicates_reject_no_cross_crate_skew`:
/// (1) the export carries the directive on both the `{*}` and `{+}` preserve rules; (2) the
/// consumer's OWN deserialize rebuilds the pair-map twins (`PairMap::from` / `NonEmptyPairMap::try_from`)
/// — preserve on both sides; (3) byte-identity vs a physical stub carrying the directive; (4) a
/// NEGATIVE control proving the directive is load-bearing — a stub with it stripped rebuilds a
/// reject-default map with NO pair-map twin (the skew, made visible).
#[test]
fn extern_import_projects_duplicates_preserve_no_cross_crate_skew() {
    let export = mint_export(&fixture("dep-preserve/lib.cddl"), "dep", "duppreserve");
    let consumer = fixture("consumer-preserve/lib.cddl");

    // (1) the export carries `@duplicates preserve` on each table rule (the pin is appended after).
    let export_body = export
        .values()
        .find(|c| c.contains("preserve_map"))
        .expect("the export must contain the preserve rules");
    assert!(
        export_body.contains("preserve_map = {* uint => tstr} ; @duplicates preserve"),
        "the {{*}} preserve table must project its directive; got:\n{export_body}"
    );
    assert!(
        export_body.contains("preserve_nmap = {+ uint => tstr} ; @duplicates preserve"),
        "the {{+}} preserve table must project its directive; got:\n{export_body}"
    );

    // Consume the export via --extern-import.
    let flag_root = scratch("duppreserve_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("duppreserve_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    // (2) the consumer's OWN deserialize rebuilds the pair-map twins — duplicate-keyed wire is
    // preserved IN the consumer, preserve on both sides of the crate boundary.
    let ser = via_flag
        .get("rust/src/generated/serialization.rs")
        .expect("the consumer must emit serialization.rs");
    assert!(
        ser.contains("PairMap::from"),
        "the consumer must deserialize the {{*}} preserve table into the PairMap vec-of-pairs twin"
    );
    assert!(
        ser.contains("NonEmptyPairMap::try_from"),
        "the consumer must deserialize the {{+}} preserve table through the NonEmptyPairMap door"
    );

    // (3) byte-identity vs a physical stub carrying the directive (the seam roundtrips it verbatim).
    let stub_root = scratch("duppreserve_stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &strip_header(content),
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("physical-stub generation must succeed");
    assert_eq!(
        via_flag, via_stub,
        "--extern-import must produce byte-identical output to a physical stub carrying @duplicates preserve"
    );

    // (4) NEGATIVE control: a stub with the directive STRIPPED rebuilds a reject-default map with no
    // pair-map twin — the silent-reject skew, made visible. This proves the projected directive is
    // load-bearing rather than decorative. ("PairMap" as a substring covers NonEmptyPairMap too.)
    let skew_root = scratch("duppreserve_skew");
    write(&skew_root, "lib.cddl", &consumer);
    for (path, content) in &export {
        let sub = path
            .strip_prefix("extern-interface/dep/")
            .expect("export path shape");
        let stripped = strip_header(content).replace("@duplicates preserve ", "");
        write(
            &skew_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &stripped,
        );
    }
    let via_skew = generate(&skew_root, &[]).expect("skew-stub generation must succeed");
    let skew_ser = via_skew
        .get("rust/src/generated/serialization.rs")
        .expect("skew serialization.rs");
    assert!(
        !skew_ser.contains("PairMap"),
        "with @duplicates preserve dropped the consumer must NOT rebuild the pair-map twin — this is \
         the cross-crate skew the projection prevents"
    );

    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&skew_root);
}

/// Strip every `@rust_name <ident>` pin from a stub body, keeping any other annotations on the
/// line (`; @no_alias @rust_name Na` -> `; @no_alias`) and dropping a comment tail left empty by
/// the strip (`coin = uint ; @rust_name Coin` -> `coin = uint`).
fn strip_pins(stub_body: &str) -> String {
    let mut out = String::new();
    for line in stub_body.lines() {
        let stripped = match line.find("@rust_name") {
            Some(at) => {
                let before = &line[..at];
                let after = &line[at + "@rust_name".len()..];
                // drop the pin's single ident argument, keep anything after it
                let after = after.trim_start();
                let rest = after
                    .find(char::is_whitespace)
                    .map(|i| &after[i..])
                    .unwrap_or("");
                let joined = format!("{}{}", before.trim_end(), rest);
                // a comment marker left with no annotation text is dropped entirely
                match joined.trim_end().strip_suffix(';') {
                    Some(code) if !code.trim_end().is_empty() => code.trim_end().to_string(),
                    _ => joined.trim_end().to_string(),
                }
            }
            None => line.to_string(),
        };
        out.push_str(&stripped);
        out.push('\n');
    }
    out
}

fn strip_header(export_file: &str) -> String {
    let mut lines = export_file.lines();
    let first = lines.next().unwrap_or("");
    assert_eq!(
        first, "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1",
        "minted export must open with the seam header"
    );
    let rest = lines.collect::<Vec<_>>().join("\n");
    format!("{rest}\n")
}

/// The conditional `v2` seam header: an extern-interface export whose finalized IR
/// contains CDDL `any` bumps EVERY file to `v2`; an export with no `any` stays `v1`, so unaffected
/// dep/consumer pairs keep working and an `any`-bearing export read by a consumer predating `any`
/// support fails loudly at its own version seam (a reader that understands `any` accepts both — the
/// reader edit in `api::scan_extern_import_seam`).
#[test]
fn extern_interface_v2_header_conditional_on_any() {
    // any-bearing dep → every export file opens with v2.
    let any_export = mint_export("meta = {* uint => any}\n", "anydep", "v2any");
    assert!(
        !any_export.is_empty(),
        "any dep must emit at least one file"
    );
    for (path, content) in &any_export {
        assert_eq!(
            content.lines().next().unwrap_or(""),
            "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v2",
            "an any-bearing export file {path} must carry the v2 seam header"
        );
    }

    // no-any dep → stays v1 (unaffected pairs compatible).
    let plain_export = mint_export("thing = {1: uint, 2: text}\n", "plaindep", "v2plain");
    assert!(!plain_export.is_empty());
    for (path, content) in &plain_export {
        assert_eq!(
            content.lines().next().unwrap_or(""),
            "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1",
            "a no-any export file {path} must stay on the v1 seam header"
        );
    }
}

/// The seam's own spelling must round-trip for a windowless `i64` — the one primitive for which it
/// did not. A bare `int` re-parses as the reserved big-`Int` extern class (only an `int`-HEADED
/// control op maps the spelling to the i64 primitive), so an export that spelled a dependency's
/// `i64` alias `int` handed the consumer a DIFFERENT type than the dependency has. This is the CML
/// `mint` shape in miniature: a full-range i64 alias in the dependency, used by the consumer as a
/// nested map value. Pinned end to end — the dependency's own type is `i64`, the export carries the
/// explicit full range, and the consumer that imports it types the map value through that same
/// `i64` alias and mints no `Int` class of its own.
#[test]
fn extern_import_full_range_i64_round_trips_as_i64_not_int() {
    let dep_spec = "holder = [n: non_zero_int_64]\n\
non_zero_int_64 = -9223372036854775808..9223372036854775807\n";

    // What the dependency's own crate calls the rule — the type the consumer must agree with.
    let dep_root = scratch("i64seam_dep");
    write(&dep_root, "lib.cddl", dep_spec);
    let dep_rust = generate(&dep_root.join("lib.cddl"), &[]).expect("dep generation must succeed");
    assert!(
        dep_rust["rust/src/generated/mod.rs"].contains("pub type NonZeroInt64 = i64;"),
        "the dependency's full-range alias is an i64: {}",
        dep_rust["rust/src/generated/mod.rs"]
    );

    let export = mint_export(dep_spec, "dep", "i64seam");
    let root = &export["extern-interface/dep/mod.cddl"];
    assert!(
        root.contains(
            "non_zero_int_64 = -9223372036854775808..9223372036854775807 ; @rust_name NonZeroInt64"
        ),
        "a windowless i64 must export its explicit full range, never the bare `int` that re-parses \
         as the big-`Int` class: {root}"
    );

    // The consumer: `{* uint => {* bytes => non_zero_int_64}}`, CML's `mint` in miniature.
    let cons_root = scratch("i64seam_cons");
    write(
        &cons_root,
        "lib.cddl",
        "my_mint = {* uint => {* bytes => non_zero_int_64}}\n",
    );
    let export_dir = scratch("i64seam_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let consumer = generate(
        &cons_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");
    let src = &consumer["rust/src/generated/mod.rs"];

    let _ = std::fs::remove_dir_all(&dep_root);
    let _ = std::fs::remove_dir_all(&cons_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        src.contains("use dep::NonZeroInt64;")
            && src.contains("pub type MyMint = BTreeMap<u64, BTreeMap<Vec<u8>, NonZeroInt64>>;"),
        "the consumer must type the map value through the dependency's i64 alias: {src}"
    );
    assert!(
        !src.contains("pub enum Int {"),
        "reading the export must not make the consumer mint the big-`Int` class — that is the \
         bare-`int` misparse this spelling exists to prevent: {src}"
    );
}

/// The plain-group acceptance criterion (the proposal's, CML-shaped): a consumer that hand-copied a
/// dependency's plain groups swaps to `--extern-import` with ZERO generated-output diff. The dep
/// (`dep-groups`) carries a record-member plain group, three group-choice-variant plain groups with
/// fixed tags, a `; @name`-renamed field, an optional field, and a nested group ref; the consumer
/// (`consumer-groups`) splices them as a record member and group-choice variants. Run A feeds a
/// physical hand-stub carrying the dep's ORIGINAL plain-group spelling (the CML hand-copy — including
/// the `; @name` comment and NO `@rust_name` pins, plus opaque markers for the two non-group dep
/// types the export also carries), while Run B feeds `--extern-import` at the minted export (whose
/// group-body rows spell every member with an explicit label and `@rust_name` pins). The two CDDL
/// texts DIFFER on purpose — the property is that both re-derive the identical IR, so the consumer's
/// generated RUST is byte-identical. A pin equal to the derived name changes nothing, and a minted
/// `credential: credential` member re-derives the same field as the bare `credential`.
#[test]
fn extern_import_group_migration_matches_original_hand_stub_byte_for_byte() {
    let export = mint_export(&fixture("dep-groups/lib.cddl"), "dep", "grpmig");
    let consumer = fixture("consumer-groups/lib.cddl");

    // Run A — the CML hand-copy: the dep's plain groups in their ORIGINAL spelling (no pins, `@name`
    // comment intact) plus opaque markers for the two non-group dep types the minted export carries.
    let original_stub = "credential = bytes\n\
protocol_version = (major: uint, minor: uint)\n\
stake_registration = (tag: 0, credential)\n\
stake_delegation = (tag: 2, credential, pool: bytes) ; @name pool_thing\n\
host = (tag: 4, ? port: uint)\n\
dep_holder = _CDDL_CODEGEN_EXTERN_TYPE_\n\
dep_cert = _CDDL_CODEGEN_EXTERN_TYPE_\n";
    let stub_root = scratch("grpmig_stub");
    write(&stub_root, "lib.cddl", &consumer);
    write(
        &stub_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/mod.cddl",
        original_stub,
    );
    let via_stub = generate(&stub_root, &[]).expect("original-hand-stub generation must succeed");

    // Run B — `--extern-import` at the minted export (header + group-body rows + pins intact).
    let flag_root = scratch("grpmig_flag");
    write(&flag_root, "lib.cddl", &consumer);
    let export_dir = scratch("grpmig_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert_eq!(
        via_flag.keys().collect::<Vec<_>>(),
        via_stub.keys().collect::<Vec<_>>(),
        "the generated file SET must match between --extern-import and the original hand-stub"
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "group-migration byte-identity broke for {path}:\n--- via --extern-import ---\n{}\n--- via original hand-stub ---\n{stub_content}",
            via_flag.get(path).map(String::as_str).unwrap_or("<absent>")
        );
    }
}

/// A single-file consumer keeps ROOT_SCOPE for its OWN types even while consuming a dep via
/// `--extern-import` (the flag markers are assembled in a separate loop, so they never flip the
/// main-input single-file ROOT_SCOPE behavior). The consumer's `thing` lands in the root module —
/// `rust/src/generated/mod.rs`, never a named submodule.
#[test]
fn extern_import_single_file_consumer_keeps_root_scope() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "rootscope");
    let flag_root = scratch("rootscope_consumer");
    write(&flag_root, "lib.cddl", &fixture("consumer/lib.cddl"));
    let export_dir = scratch("rootscope_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let map = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("generation must succeed");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        map.get("rust/src/generated/mod.rs")
            .is_some_and(|s| s.contains("struct Thing")),
        "the consumer's own `Thing` must land in the ROOT module (rust/src/generated/mod.rs)"
    );
    // A named submodule for the consumer's own type would mean it left ROOT_SCOPE.
    assert!(
        !map.keys()
            .any(|k| k.starts_with("rust/src/generated/thing/")),
        "the consumer's own type must not be pushed into a named submodule: {:?}",
        map.keys().collect::<Vec<_>>()
    );
}

/// Staleness: a consumer referencing an ident absent from the export fails the checked parse, and
/// with `--extern-import` in use that failure is AUGMENTED (not swallowed) with the declared dep
/// list, the export path, and the regenerate-the-dependency / check-`; unexported:`-records / hand-stub
/// hint.
#[test]
fn extern_import_staleness_wraps_undefined_reference() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "stale");
    let flag_root = scratch("stale_consumer");
    // References `missing`, which the export does not define.
    write(&flag_root, "lib.cddl", "bad = [x: missing]\n");
    let export_dir = scratch("stale_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an undefined reference must fail generation");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        err.contains("--extern-import") && err.contains("dep"),
        "the wrapped error must name the flag and the declared dep: {err}"
    );
    assert!(
        err.contains("unexported") && err.contains("Regenerate the dependency"),
        "the wrapped error must carry the staleness hint (records / regenerate): {err}"
    );
    // The original parse error is augmented, not swallowed.
    assert!(
        err.contains("missing definition for rule missing"),
        "the original undefined-reference detail must be preserved: {err}"
    );
    assert!(
        err.contains("extern-interface/dep"),
        "the wrapped error must name the export path: {err}"
    );
}

/// A flag-fed file missing the seam header is a hard error (the flag only accepts real machine-
/// generated exports; a headerless file is not one — hand-stubs go under the extern-deps dir).
#[test]
fn extern_import_missing_header_hard_errors() {
    let flag_root = scratch("noheader_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("noheader_export");
    // A headerless export file fed via the flag.
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("a headerless flag-fed file must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("missing") && err.contains("seam header"),
        "must name the missing seam header: {err}"
    );
    assert!(
        err.contains("mod.cddl"),
        "must name the offending file: {err}"
    );
}

/// A flag-fed file whose header names an unknown version is a hard error distinct from a missing one.
#[test]
fn extern_import_unknown_version_hard_errors() {
    let flag_root = scratch("badver_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("badver_export");
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v999\nfoo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an unknown seam version must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("unsupported version"),
        "must name the unsupported version: {err}"
    );
}

/// A flag-fed file carrying an unknown `@`-annotation is a hard error naming the file and token
/// (a typo or a newer dialect); the strict seam refuses to silently misread it.
#[test]
fn extern_import_unknown_annotation_hard_errors() {
    let flag_root = scratch("badtag_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let export_dir = scratch("badtag_export");
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\nfoo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo @bogus_tag\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an unknown annotation token must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("unknown annotation token") && err.contains("@bogus_tag"),
        "must name the unknown token: {err}"
    );
}

/// An export carrying a `; unexported:` record consumes CLEANLY when the consumer does not reference
/// the excluded ident. The record's reason text contains `@custom_serialize`/`@custom_deserialize`
/// (whitelisted tokens), proving the strict `@`-scan does not false-positive on free-form reason text.
#[test]
fn extern_import_export_with_records_parses_cleanly() {
    let export = mint_export(&fixture("dep-with-records/lib.cddl"), "dep", "records");
    let root_export = &export["extern-interface/dep/mod.cddl"];
    assert!(
        root_export.contains("; unexported: cs — @custom_serialize"),
        "the export must carry the custom-serialize exclusion record: {root_export}"
    );

    let flag_root = scratch("records_consumer");
    // References only `foo` — not the excluded `cs`.
    write(&flag_root, "lib.cddl", "thing = [f: foo]\n");
    let export_dir = scratch("records_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let map = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("an export carrying `; unexported:` records must consume cleanly");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        map.contains_key("rust/src/generated/mod.rs"),
        "generation must produce the consumer's root module"
    );
}

/// An open struct-map rest-bearing
/// RECORD projects OPAQUE across the crate seam — the ordinary class-backed-types-are-opaque posture
/// — so it needs NO projected field-model / `* K => V` member rendering. A CONCRETE-typed rest row
/// with no `any` anywhere (`* uint => text`) exports under the v1 header (opaque marker), which is
/// v1-compatible and safe, and re-imports cleanly via `--extern-import`; an `any`-containing rest
/// row exports v2 (the existing whole-IR `uses_any_cbor()` bump). Because the record projects OPAQUE
/// (no structural rest spelling crosses the seam), the v2 bump condition needs no widening — this is
/// a pure verification that the opaque projection round-trips.
#[test]
fn extern_import_open_struct_map_rest_row_projects_opaque() {
    // A concrete-typed rest row: no `any` anywhere -> v1 header, opaque marker.
    let concrete = mint_export(
        "concrete_rest = { 1: uint, * uint => text }\n",
        "dep",
        "rest_concrete",
    );
    let concrete_root = &concrete["extern-interface/dep/mod.cddl"];
    assert!(
        concrete_root.contains("; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1"),
        "a concrete-typed (no-`any`) rest-bearing record must export under the v1 header: {concrete_root}"
    );
    assert!(
        concrete_root.contains("concrete_rest = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "a rest-bearing record must project OPAQUE (class-backed-types-are-opaque): {concrete_root}"
    );

    // An `any`-containing rest row: the whole-IR `uses_any_cbor()` bump -> v2 header, still opaque.
    let any = mint_export("any_rest = { 1: uint, * uint => any }\n", "dep", "rest_any");
    let any_root = &any["extern-interface/dep/mod.cddl"];
    assert!(
        any_root.contains("; _CDDL_CODEGEN_EXTERN_INTERFACE_ v2"),
        "an `any`-containing rest-bearing record must export under the v2 header: {any_root}"
    );
    assert!(
        any_root.contains("any_rest = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "the `any`-rest record still projects OPAQUE: {any_root}"
    );

    // The concrete v1 export re-imports cleanly via --extern-import: a consumer referencing the
    // rest-bearing extern type generates without error (the consumer sees an opaque extern).
    let flag_root = scratch("rest_concrete_consumer");
    write(&flag_root, "lib.cddl", "thing = [r: concrete_rest]\n");
    let export_dir = scratch("rest_concrete_export");
    for (path, content) in &concrete {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let map = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("a v1 concrete-rest export must re-import cleanly (opaque extern)");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        map.contains_key("rust/src/generated/mod.rs"),
        "generation must produce the consumer's root module"
    );
}

/// Ask 0 — the excluded-with-record contract holds for the plain-group shapes that DON'T export as a
/// group-body row. A plain group is inlined at its use sites, so it never travels as an opaque class;
/// rather than vanishing silently, a shape with no embedded-group surface leaves a `; unexported:`
/// record. Two here: a NEVER-referenced plain group (no `rust_structs` entry) and a referenced one
/// that materialized as a homogeneous `Array` (`homarr`, no embedded-group surface). The
/// group-body inclusion of a `Record` plain group is covered by
/// `extern_export_materialized_plain_group_is_group_body_row`.
#[test]
fn extern_export_plain_groups_leave_records() {
    let spec = "block = [v: usehom]\n\
homarr = (* uint)\n\
usehom = [homarr]\n\
never_ref = (unused: uint, x: text)\n";
    let export = mint_export(spec, "dep", "plaingrouprec");
    let root = &export["extern-interface/dep/mod.cddl"];
    assert!(
        root.contains("; unexported: never_ref — plain group never referenced in the dependency"),
        "a never-referenced plain group must leave the never-referenced record: {root}"
    );
    assert!(
        root.contains("; unexported: homarr — ") && root.contains("no embedded-group surface"),
        "a homogeneous-array plain group must leave the no-embedded-group-surface record: {root}"
    );
    for name in ["never_ref", "homarr"] {
        assert!(
            !root.lines().any(|l| l
                .split(';')
                .next()
                .unwrap_or("")
                .contains(&format!("{name} ="))),
            "{name} must not be an included rule line: {root}"
        );
    }
}

/// A materialized `Record` plain group exports TRANSPARENTLY as a group-body row: the truthful
/// post-DSL `( … )` body, pinned with `@rust_name`. Array-rep members carry their post-DSL field
/// name as the label (baking in a `@name` rename with no annotation), a fixed tag renders its
/// literal, a bare reference renders the referenced rule's source ident, and an optional field takes
/// a `? ` prefix. The referenced `credential` rule survives as a transparent alias so the closure
/// keeps the group.
#[test]
fn extern_export_materialized_plain_group_is_group_body_row() {
    let spec = "block = [c: cert, d: dele]\n\
credential = bytes\n\
cert = (tag: 0, credential)\n\
dele = (tag: 2, credential, ? pool: bytes) ; @name pool_thing\n";
    let export = mint_export(spec, "dep", "groupbody");
    let root = &export["extern-interface/dep/mod.cddl"];
    // Array-rep record: fixed tag literal, bare-ref labelled by its derived field name, and the
    // `@name`-renamed optional field spelled with its post-DSL label + `? ` prefix.
    assert!(
        root.contains("cert = (tag: 0, credential: credential) ; @rust_name Cert"),
        "the record plain group must export as a pinned group-body row: {root}"
    );
    assert!(
        root.contains(
            "dele = (tag: 2, credential: credential, ? pool_thing: bytes) ; @rust_name Dele"
        ),
        "the @name rename must be baked into the member label and `?` preserved: {root}"
    );
    // neither is an exclusion record
    assert!(
        !root.contains("; unexported: cert") && !root.contains("; unexported: dele"),
        "materialized record plain groups must be included, not excluded: {root}"
    );
}

/// A `@rust_name` pin on a plain GROUP rule in an extern-deps stub is honored (parsing's group-rule
/// path now calls `handle_rust_name_pin`, reading the pin from the last group entry's trailing comment
/// where cddl binds it). A consumer that splices the group delegates to the dep's PINNED type name: a
/// pin DIFFERING from the derived name yields `use <dep>::<Pinned> as <Derived>;`, while a pin EQUAL
/// to the derived name stays alias-free (mirroring `extern_import_matches_pinless_hand_stub…`).
#[test]
fn extern_import_honors_group_rust_name_pin() {
    let consumer = "holder = [h: uint, stake_registration]\n";

    // Differing pin -> aliased import.
    let diff_root = scratch("grouppin_diff");
    write(&diff_root, "lib.cddl", consumer);
    write(
        &diff_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/mod.cddl",
        "stake_registration = (tag: 0, cred: uint) ; @rust_name RenamedReg\n",
    );
    let diff = generate(&diff_root, &[]).expect("differing-pin generation must succeed");
    let _ = std::fs::remove_dir_all(&diff_root);
    let modrs = &diff["rust/src/generated/mod.rs"];
    assert!(
        modrs.contains("use dep::RenamedReg as StakeRegistration;"),
        "a differing group-rule pin must alias the dep's pinned name to the derived name: {modrs}"
    );

    // Equal pin (= the derived name) -> alias-free import.
    let eq_root = scratch("grouppin_eq");
    write(&eq_root, "lib.cddl", consumer);
    write(
        &eq_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/mod.cddl",
        "stake_registration = (tag: 0, cred: uint) ; @rust_name StakeRegistration\n",
    );
    let eq = generate(&eq_root, &[]).expect("equal-pin generation must succeed");
    let _ = std::fs::remove_dir_all(&eq_root);
    let eq_modrs = &eq["rust/src/generated/mod.rs"];
    assert!(
        eq_modrs.contains("use dep::StakeRegistration;")
            && !eq_modrs.contains("use dep::StakeRegistration as "),
        "a pin equal to the derived name must stay alias-free: {eq_modrs}"
    );
}

/// Declaring a dep BOTH via `--extern-import` AND as a physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/`
/// input directory is an ambiguous double declaration — a hard error, never a merge.
#[test]
fn extern_import_double_declaration_hard_errors() {
    let export = mint_export(&fixture("dep/lib.cddl"), "dep", "double");
    // A directory input carrying BOTH the consumer and a physical stub dir for `dep`.
    let input_root = scratch("double_input");
    write(&input_root, "lib.cddl", &fixture("consumer/lib.cddl"));
    write(
        &input_root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/mod.cddl",
        "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\ncoin = uint ; @rust_name Coin\n",
    );
    let export_dir = scratch("double_export");
    for (path, content) in &export {
        write(&export_dir, path, content);
    }
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(&input_root, &["--extern-import", &import_arg])
        .expect_err("a dep declared both ways must be rejected");
    let _ = std::fs::remove_dir_all(&input_root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("double declaration") && err.contains("dep"),
        "must reject the ambiguous double declaration naming the dep: {err}"
    );
}

/// A path with no `.cddl` files under it is a hard error naming the flag value.
#[test]
fn extern_import_empty_path_hard_errors() {
    let flag_root = scratch("emptypath_consumer");
    write(&flag_root, "lib.cddl", "bad = [x: foo]\n");
    let empty_dir = scratch("emptypath_export");
    let import_arg = format!("dep={}", empty_dir.to_str().unwrap());
    let err = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect_err("an export path with no .cddl files must be rejected");
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&empty_dir);
    assert!(
        err.contains("no .cddl files") && err.contains("dep="),
        "must name the flag value and the empty-path cause: {err}"
    );
}

/// A malformed `--extern-import` value (no `=`) is a hard error, mirroring the other cross-crate
/// flag parsers.
#[test]
#[should_panic(expected = "--extern-import")]
fn extern_import_malformed_flag_value_panics() {
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        "unused.cddl",
        "--output",
        "unused",
        "--extern-import",
        "no_equals_sign",
    ]);
    let _ = cli.extern_import_paths();
}

// ============================================================================================
// Transitive fixture (commit 7): a three-crate chain consumer -> mid-dep -> base-dep.
//
// base-dep is a leaf; mid-dep consumes base-dep (via `--extern-import` — the same channel, so its
// own build models the real transitive shape and the depth-1 rule is proven even when the dep is
// pulled through the export channel, not a physical stub); the consumer consumes BOTH mid-dep and
// base-dep via two `--extern-import` flags. The load-bearing invariant is depth-1: a dep's own deps
// never travel through its export ("each export describes one crate's own opaque surface; depth
// never exceeds one"), so a consumer that names a base-dep type must declare base-dep DIRECTLY.
//
// The three fixture specs live under `tests/extern-import-transitive/`. These tests reuse the
// commit-6 helpers (`scratch`, `write`, `generate`, `strip_header`) and add three transitive-only
// helpers below.
// ============================================================================================

/// Read a committed spec from the transitive fixture tree.
fn tfixture(rel: &str) -> String {
    std::fs::read_to_string(std::path::Path::new("tests/extern-import-transitive").join(rel))
        .unwrap_or_else(|e| panic!("reading transitive fixture {rel}: {e}"))
}

/// Mint a dep's extern-interface export while it itself consumes other deps via `--extern-import`
/// (the transitive case — mid-dep consuming base-dep). `extra` carries the `--extern-import` flags.
/// `mint_export` above is the leaf case (no `extra`); this is its flag-carrying generalization.
fn mint_export_flags(
    dep_spec: &str,
    dep_key: &str,
    tag: &str,
    extra: &[&str],
) -> BTreeMap<String, String> {
    let root = scratch(&format!("tmint_{tag}"));
    write(&root, "lib.cddl", dep_spec);
    let input = root.join("lib.cddl");
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "false",
        "--lib-name",
        dep_key,
    ];
    args.extend_from_slice(extra);
    let cli = Cli::parse_from(args);
    let files = crate::api::extern_interface_strings(&cli)
        .expect("dep export projection must succeed (exclude-with-record, never abort)");
    let _ = std::fs::remove_dir_all(&root);
    files
}

/// Write a minted export map to a fresh scratch tree and return the `extern-interface/<dep_key>`
/// directory a `--extern-import <dep>=<path>` flag points at.
fn write_export(export: &BTreeMap<String, String>, dep_key: &str, tag: &str) -> std::path::PathBuf {
    let dir = scratch(&format!("texport_{tag}"));
    for (path, content) in export {
        write(&dir, path, content);
    }
    dir.join(format!("extern-interface/{dep_key}"))
}

/// A wasm-mode consumer generation (the sidecar channels are a wasm/workspace concern, so the
/// rust-only `generate` above cannot exercise them). Returns post-rustfmt source keyed by path.
fn generate_wasm(
    input: &std::path::Path,
    extra: &[&str],
) -> Result<BTreeMap<String, String>, String> {
    let mut args = vec![
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        "extern_import_unused",
        "--wasm",
        "true",
    ];
    args.extend_from_slice(extra);
    let cli = Cli::parse_from(args);
    crate::api::generated_strings(&cli).map_err(|e| e.to_string())
}

/// Mint base-dep's export and mid-dep's export (mid-dep consuming base-dep via `--extern-import`),
/// returning both maps plus the on-disk export directories the consumer's flags point at. Shared by
/// the composition / opaque-boundary / byte-identity tests.
fn mint_chain(
    tag: &str,
) -> (
    BTreeMap<String, String>,
    std::path::PathBuf,
    std::path::PathBuf,
) {
    let base_export = mint_export(
        &tfixture("base-dep/lib.cddl"),
        "base_dep",
        &format!("{tag}b"),
    );
    let base_dir = write_export(&base_export, "base_dep", &format!("{tag}b"));
    let base_import = format!("base_dep={}", base_dir.to_str().unwrap());
    let mid_export = mint_export_flags(
        &tfixture("mid-dep/lib.cddl"),
        "mid_dep",
        &format!("{tag}m"),
        &["--extern-import", &base_import],
    );
    let mid_dir = write_export(&mid_export, "mid_dep", &format!("{tag}m"));
    (base_export, base_dir, mid_dir)
}

/// Item 1 — depth-1 export purity. mid-dep's export (minted WITH base-dep declared via
/// `--extern-import`) must contain ONLY mid-dep's own surface: no base-dep idents appear as rules or
/// in any exported body. The interesting case is `mid_points = [* base_point]`: a TRANSPARENT named
/// collection referencing a base-dep type. Its truthful spelling references `base_point`, which is
/// depth-1-excluded from mid-dep's export, so reference-closure EXCLUDES it with an `; unexported:`
/// record naming the chain root. `mid_record` (opaque) and `mid_label` (base-free alias) survive as
/// the positive control that mid-dep's own surface IS present.
#[test]
fn transitive_mid_dep_export_excludes_base_dep() {
    let base_export = mint_export(&tfixture("base-dep/lib.cddl"), "base_dep", "t1b");
    let base_dir = write_export(&base_export, "base_dep", "t1b");
    let base_import = format!("base_dep={}", base_dir.to_str().unwrap());
    let mid_export = mint_export_flags(
        &tfixture("mid-dep/lib.cddl"),
        "mid_dep",
        "t1m",
        &["--extern-import", &base_import],
    );
    let _ = std::fs::remove_dir_all(&base_dir);

    let root = &mid_export["extern-interface/mid_dep/mod.cddl"];

    // Positive control: mid-dep's OWN surface is present.
    assert!(
        root.contains("mid_record = _CDDL_CODEGEN_EXTERN_TYPE_"),
        "mid-dep's own opaque record must be exported: {root}"
    );
    assert!(
        root.contains("mid_label = tstr"),
        "mid-dep's own base-free transparent alias must be exported: {root}"
    );

    // No base-dep ident appears in any RULE or exported body — checked on the CODE portion of every
    // line (before the first `;`), so the legitimate `; unexported:` record (a comment) that names
    // `base_point` as the exclusion root does not trip the scan.
    for f in mid_export.values() {
        for line in f.lines() {
            let code = line.split(';').next().unwrap_or("");
            for needle in ["base_point", "base_coin", "BasePoint", "BaseCoin"] {
                assert!(
                    !code.contains(needle),
                    "mid-dep export code must not reference base-dep ident `{needle}`: {line:?}"
                );
            }
        }
    }

    // The interesting case: the transparent collection over a base-dep type is EXCLUDED-with-record,
    // and is NOT emitted as an included rule.
    assert!(
        root.contains("; unexported: mid_points \u{2014} references excluded base_point"),
        "mid_points (transparent collection over base_point) must be excluded with a reference-closure record: {root}"
    );
    assert!(
        !root
            .lines()
            .any(|l| l.split(';').next().unwrap_or("").contains("mid_points")),
        "mid_points must be excluded, never an included rule: {root}"
    );
}

/// Item 2 — consumer composition. The consumer generates successfully consuming BOTH exports via two
/// `--extern-import` flags, referencing types from each. The generated `use` statements must target
/// the right crate: base-dep types from `base_dep`, the mid-dep type from `mid_dep` — never crossed.
#[test]
fn transitive_consumer_composes_both_deps() {
    let (_base_export, base_dir, mid_dir) = mint_chain("t2");
    let cons_root = scratch("t2consumer");
    write(&cons_root, "lib.cddl", &tfixture("consumer/lib.cddl"));
    let base_import = format!("base_dep={}", base_dir.to_str().unwrap());
    let mid_import = format!("mid_dep={}", mid_dir.to_str().unwrap());
    let map = generate(
        &cons_root.join("lib.cddl"),
        &[
            "--extern-import",
            &base_import,
            "--extern-import",
            &mid_import,
        ],
    )
    .expect("consumer must generate consuming both deps");
    let _ = std::fs::remove_dir_all(&cons_root);
    let _ = std::fs::remove_dir_all(&base_dir);
    let _ = std::fs::remove_dir_all(&mid_dir);

    let modrs = &map["rust/src/generated/mod.rs"];
    // base-dep's two types import from the base_dep crate; the mid-dep type from the mid_dep crate.
    assert!(
        modrs.contains("use base_dep::{BaseCoin, BasePoint};"),
        "base-dep types must import from the base_dep crate: {modrs}"
    );
    assert!(
        modrs.contains("use mid_dep::MidRecord;"),
        "the mid-dep type must import from the mid_dep crate: {modrs}"
    );
    // Never crossed: base-dep types never come from mid_dep and vice-versa.
    assert!(
        !modrs.contains("use mid_dep::BasePoint")
            && !modrs.contains("use mid_dep::BaseCoin")
            && !modrs.contains("use base_dep::MidRecord"),
        "a dep's type must never be imported from the wrong crate: {modrs}"
    );
}

/// Item 3 — opaque-boundary. A consumer embeds `mid_record`, which in mid-dep's real spec itself
/// embeds `base_point`. Because mid-dep exports `mid_record` opaquely, the consumer resolves the
/// whole chain while declaring ONLY mid-dep — knowing nothing of base-dep. Generation succeeds and
/// no base-dep ident leaks into the consumer's output for that chain.
#[test]
fn transitive_opaque_boundary_hides_base_dep() {
    let (_base_export, base_dir, mid_dir) = mint_chain("t3");
    let _ = std::fs::remove_dir_all(&base_dir); // consumer declares ONLY mid-dep
    let cons_root = scratch("t3consumer");
    write(&cons_root, "lib.cddl", "opaque_holder = [m: mid_record]\n");
    let mid_import = format!("mid_dep={}", mid_dir.to_str().unwrap());
    let map = generate(
        &cons_root.join("lib.cddl"),
        &["--extern-import", &mid_import],
    )
    .expect("the opaque-boundary consumer must generate knowing nothing of base-dep");
    let _ = std::fs::remove_dir_all(&cons_root);
    let _ = std::fs::remove_dir_all(&mid_dir);

    let modrs = &map["rust/src/generated/mod.rs"];
    assert!(
        modrs.contains("use mid_dep::MidRecord;") && modrs.contains("struct OpaqueHolder"),
        "the consumer embeds the mid-dep type opaquely: {modrs}"
    );
    // The whole base-dep surface stays hidden behind mid_record's opaque class.
    for content in map.values() {
        assert!(
            !content.contains("base_dep") && !content.contains("BasePoint"),
            "no base-dep ident may leak into the opaque-boundary consumer: {content}"
        );
    }
}

/// Item 4 — sidecar-channel preservation. A wasm consumer that BORROWS collection/key shapes from a
/// workspace dep consumed via `--extern-import` (`[* base_point]` and `{* base_point => uint}`, both
/// all-one-dep) must record those shapes in `borrowed_collections.rs` / `borrowed_key_types.rs`
/// carrying base-dep's ORIGINAL CDDL idents (`base_point`, not the Rust `BasePoint`) — byte-identical
/// to the physical-stub channel. The `--extern-import` text uses the dep's original idents verbatim,
/// so original-ident resolution through the sidecars is undisturbed.
#[test]
fn transitive_wasm_sidecars_carry_dep_cddl_idents() {
    let base_export = mint_export(&tfixture("base-dep/lib.cddl"), "base_dep", "t4b");
    let base_dir = write_export(&base_export, "base_dep", "t4b");
    let base_import = format!("base_dep={}", base_dir.to_str().unwrap());
    // A named collection over base_point (a wasm borrowed wrapper) AND a map keyed on base_point (a
    // borrowed map key) — one shape into each sidecar channel.
    let spec = "wthing = [pts: [* base_point], m: { * base_point => uint }]\n";

    // Run A — via `--extern-import`.
    let flag_root = scratch("t4flag");
    write(&flag_root, "lib.cddl", spec);
    let via_flag = generate_wasm(
        &flag_root.join("lib.cddl"),
        &[
            "--extern-import",
            &base_import,
            "--workspace-dep",
            "base_dep",
            "--extern-wasm-crate",
            "base_dep=base_dep_wasm",
        ],
    )
    .expect("the wasm consumer must generate via --extern-import");

    // Run B — via a physical stub of base-dep (export minus header).
    let stub_root = scratch("t4stub");
    write(&stub_root, "lib.cddl", spec);
    for (path, content) in &base_export {
        let sub = path
            .strip_prefix("extern-interface/base_dep/")
            .expect("export path shape");
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/base_dep/{sub}"),
            &strip_header(content),
        );
    }
    let via_stub = generate_wasm(
        &stub_root,
        &[
            "--workspace-dep",
            "base_dep",
            "--extern-wasm-crate",
            "base_dep=base_dep_wasm",
        ],
    )
    .expect("the wasm consumer must generate via a physical stub");

    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&base_dir);

    // The borrowed-collections rows carry base-dep's ORIGINAL CDDL idents in the shape column.
    let coll = &via_flag["wasm/src/generated/borrowed_collections.rs"];
    assert!(
        coll.contains(r#"("base_dep", "BasePointList", "[* base_point]")"#),
        "the borrowed loose-list row must carry the dep's original CDDL ident: {coll}"
    );
    assert!(
        coll.contains(r#"("base_dep", "MapBasePointToU64", "{* base_point => uint}")"#),
        "the borrowed map row must carry the dep's original CDDL idents: {coll}"
    );
    // The borrowed-key-types row carries the original CDDL ident and asserts on the dep-crate type.
    let keys = &via_flag["rust/src/generated/borrowed_key_types.rs"];
    assert!(
        keys.contains(r#"("base_dep", "base_point")"#)
            && keys.contains("_assert_key_traits::<base_dep::BasePoint>()"),
        "the borrowed key-type row must carry the dep's original CDDL ident + crate type: {keys}"
    );
    // Byte-identical through either channel — the extern-import assembly seam does not perturb the
    // sidecars (the original-ident resolution the `--wrapper-requests` / `--key-requests` channels
    // read back is undisturbed).
    assert_eq!(
        via_flag.get("wasm/src/generated/borrowed_collections.rs"),
        via_stub.get("wasm/src/generated/borrowed_collections.rs"),
        "borrowed_collections.rs must be identical via --extern-import and via the physical stub"
    );
    assert_eq!(
        via_flag.get("rust/src/generated/borrowed_key_types.rs"),
        via_stub.get("rust/src/generated/borrowed_key_types.rs"),
        "borrowed_key_types.rs must be identical via --extern-import and via the physical stub"
    );
}

/// The borrowed_key_types.rs self-check must assert on the borrowed key type's TRUE module path — the
/// same path the consumer's own generated `use` lines take — not a bare `{dep}::{Ident}` at the dep
/// crate ROOT. A dep type living in a non-root scope (via an extern-deps stub tree
/// `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep_crate/sub/module.cddl`) is reachable only as
/// `dep_crate::sub::module::ScopedKey` (the thin root does not re-export scope contents), so the old
/// bare `dep_crate::ScopedKey` self-check was E0412-class in the consumer build. The machine ROW is
/// unaffected — it stays the bare `(dep, cddl-ident)` the dep resolves scope-agnostically. A dep-ROOT
/// borrowed key (`root_key`) proves the path column collapses to `{dep}` (byte-identical to before).
#[test]
fn borrowed_key_types_self_check_carries_scoped_dep_path() {
    // A consumer keying two maps: one on a SCOPED dep type (`scoped_key`, in `dep_crate::sub::module`)
    // and one on a ROOT dep type (`root_key`, at the dep crate root). Both are borrowed key types.
    let root = scratch("bkt_scoped");
    write(
        &root,
        "lib.cddl",
        "my_local = uint\nkm = {* scoped_key => my_local}\nkm2 = {* root_key => my_local}\n",
    );
    write(
        &root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep_crate/mod.cddl",
        "root_key = _CDDL_CODEGEN_EXTERN_TYPE_\n",
    );
    write(
        &root,
        "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep_crate/sub/module.cddl",
        "scoped_key = _CDDL_CODEGEN_EXTERN_TYPE_\n",
    );
    let map = generate(
        &root,
        &[
            "--workspace-dep",
            "dep_crate",
            "--extern-wasm-crate",
            "dep_crate=dep_crate_wasm",
        ],
    )
    .expect("the scoped-key consumer must generate");
    let _ = std::fs::remove_dir_all(&root);

    let keys = &map["rust/src/generated/borrowed_key_types.rs"];
    // The scoped key's self-check carries its FULL module path — matching the consumer's own `use`.
    assert!(
        keys.contains("_assert_key_traits::<dep_crate::sub::module::ScopedKey>();"),
        "the scoped borrowed key must be self-checked at its TRUE module path:\n{keys}"
    );
    // The old ROOT-path bug form must be gone.
    assert!(
        !keys.contains("_assert_key_traits::<dep_crate::ScopedKey>();"),
        "the scoped key must NOT be asserted at the dep crate root (the E0412 bug):\n{keys}"
    );
    // A dep-ROOT borrowed key keeps its bare `{dep}::{Ident}` self-check (path column == dep name).
    assert!(
        keys.contains("_assert_key_traits::<dep_crate::RootKey>();"),
        "a dep-ROOT borrowed key stays asserted at the crate root:\n{keys}"
    );
    // The machine table ROWS are the bare `(dep, cddl-ident)` regardless of scope — the module path
    // lives only in the self-check, never the table.
    assert!(
        keys.contains(r#"("dep_crate", "scoped_key")"#)
            && keys.contains(r#"("dep_crate", "root_key")"#),
        "the rows must stay bare `(dep, cddl-ident)` with no scope column:\n{keys}"
    );
    assert!(
        keys.contains("pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)]"),
        "the table stays the two-column form (no scope column added):\n{keys}"
    );
}

/// Item 5 — byte-identity at transitive scale. The consumer built once from physical stubs of BOTH
/// deps and once via two `--extern-import` flags must produce byte-identical rust output. This is the
/// commit-6 acceptance pattern scaled to two deps; the pinless-migration variant is unnecessary here
/// — commit 6 already covers pin/derivation-agreement, and these fixtures carry no divergent pins.
#[test]
fn transitive_consumer_byte_identity_stubs_vs_flags() {
    let (base_export, base_dir, mid_dir) = mint_chain("t5");
    // The minted export maps re-read from the on-disk export dirs written by `mint_chain`.
    let mid_export = mint_export_flags(
        &tfixture("mid-dep/lib.cddl"),
        "mid_dep",
        "t5m2",
        &[
            "--extern-import",
            &format!("base_dep={}", base_dir.to_str().unwrap()),
        ],
    );
    let consumer = tfixture("consumer/lib.cddl");

    // Run A — two physical stubs (each export minus its header) under the extern-deps dir.
    let stub_root = scratch("t5stub");
    write(&stub_root, "lib.cddl", &consumer);
    for (path, content) in &base_export {
        let sub = path
            .strip_prefix("extern-interface/base_dep/")
            .expect("base export path shape");
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/base_dep/{sub}"),
            &strip_header(content),
        );
    }
    for (path, content) in &mid_export {
        let sub = path
            .strip_prefix("extern-interface/mid_dep/")
            .expect("mid export path shape");
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/mid_dep/{sub}"),
            &strip_header(content),
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("two-stub generation must succeed");

    // Run B — two `--extern-import` flags at the minted export trees.
    let flag_root = scratch("t5flag");
    write(&flag_root, "lib.cddl", &consumer);
    let base_import = format!("base_dep={}", base_dir.to_str().unwrap());
    let mid_import = format!("mid_dep={}", mid_dir.to_str().unwrap());
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &[
            "--extern-import",
            &base_import,
            "--extern-import",
            &mid_import,
        ],
    )
    .expect("two-flag generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&base_dir);
    let _ = std::fs::remove_dir_all(&mid_dir);

    assert_eq!(
        via_flag.keys().collect::<Vec<_>>(),
        via_stub.keys().collect::<Vec<_>>(),
        "the generated file SET must match between two --extern-import flags and two physical stubs"
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "transitive byte-identity broke for {path}:\n--- via --extern-import ---\n{}\n--- via physical stubs ---\n{stub_content}",
            via_flag.get(path).map(String::as_str).unwrap_or("<absent>")
        );
    }
}

// --- Need-based narrowing ----------------------------------------------------------------------
//
// `--extern-import` concatenates only the rules a consumer NEEDS from a dependency's export: the
// names it references and does not define, plus what those transitively reference. The unit-level
// exact-set pins for the closure itself live in `extern_narrow`; these are the end-to-end legs.

/// Write an export tree to a fresh scratch dir and return the `<dep>=<path>` flag value.
fn stage_export(
    export: &BTreeMap<String, String>,
    dep: &str,
    tag: &str,
) -> (std::path::PathBuf, String) {
    let dir = scratch(tag);
    for (path, content) in export {
        write(&dir, path, content);
    }
    let arg = format!(
        "{dep}={}",
        dir.join(format!("extern-interface/{dep}"))
            .to_str()
            .unwrap()
    );
    (dir, arg)
}

/// THE regression test for the whole feature, in its minimal CML shape. A consumer that hand-owns
/// its own `block` (a re-export of the dependency's Conway block, declared as an own-spec extern)
/// and uses the dependency's `thing` must generate — even though the dependency's export ALSO
/// carries a `block` rule the consumer never means. Before narrowing this aborted with the flat
/// namespace's `rule "block" is already defined`, which no consumer could route around: the export
/// is complete by design and the collision was on a rule the consumer does not need.
///
/// The two halves are pinned separately, because "it generates" alone would pass with the
/// dependency's `block` silently winning: the consumer's `block` must resolve to its OWN
/// `crate::Block`, and `thing` to the dependency's crate.
#[test]
fn extern_import_narrows_past_an_unneeded_name_collision() {
    let export = mint_export(
        "thing = [a: uint]\nblock = [b: uint, c: text]\n",
        "dep",
        "collide",
    );
    let root = scratch("collide_consumer");
    write(
        &root,
        "lib.cddl",
        "block = _CDDL_CODEGEN_EXTERN_TYPE_\nholder = [t: thing, b: block]\n",
    );
    let (export_dir, import_arg) = stage_export(&export, "dep", "collide_export");
    let files = generate(&root.join("lib.cddl"), &["--extern-import", &import_arg])
        .expect("a collision on an UNNEEDED export rule must no longer abort generation");
    let all = files.values().cloned().collect::<String>();
    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        all.contains("use dep::Thing;"),
        "`thing` must resolve to the dependency's crate:\n{all}"
    );
    assert!(
        !all.contains("use dep::Block;"),
        "`block` must resolve to the consumer's OWN extern, never the dependency's:\n{all}"
    );
    assert!(
        all.contains("b: Block"),
        "the consumer's own `block` extern must still type the field:\n{all}"
    );
}

/// Output-neutrality, the property that makes narrowing a pure availability widening: a consumer
/// generated against a dependency's FULL export and against a hand-pre-narrowed copy of it (only
/// the rules the consumer needs) produces byte-identical output. If this ever moved, narrowing
/// would be changing generated code rather than only changing what is available to it.
#[test]
fn extern_import_narrowing_is_output_neutral() {
    let export = mint_export(
        "coin = uint\nfoo = [a: uint]\nspare = [z: text]\nspare_alias = spare\n",
        "dep",
        "neutral",
    );
    let consumer = "thing = [f: foo, c: coin]\n";

    let root_full = scratch("neutral_full_consumer");
    write(&root_full, "lib.cddl", consumer);
    let (dir_full, arg_full) = stage_export(&export, "dep", "neutral_full_export");
    let via_full = generate(&root_full.join("lib.cddl"), &["--extern-import", &arg_full])
        .expect("generation against the full export must succeed");

    // The same export with every rule the consumer does not need removed by hand.
    let pruned = export
        .iter()
        .map(|(path, content)| {
            let kept = content
                .lines()
                .filter(|line| !line.starts_with("spare"))
                .collect::<Vec<_>>()
                .join("\n");
            (path.clone(), format!("{kept}\n"))
        })
        .collect::<BTreeMap<_, _>>();
    let root_narrow = scratch("neutral_narrow_consumer");
    write(&root_narrow, "lib.cddl", consumer);
    let (dir_narrow, arg_narrow) = stage_export(&pruned, "dep", "neutral_narrow_export");
    let via_narrow = generate(
        &root_narrow.join("lib.cddl"),
        &["--extern-import", &arg_narrow],
    )
    .expect("generation against the pre-narrowed export must succeed");

    let _ = std::fs::remove_dir_all(&root_full);
    let _ = std::fs::remove_dir_all(&root_narrow);
    let _ = std::fs::remove_dir_all(&dir_full);
    let _ = std::fs::remove_dir_all(&dir_narrow);

    assert_eq!(
        via_full.keys().collect::<Vec<_>>(),
        via_narrow.keys().collect::<Vec<_>>(),
        "the generated file SET must not depend on the export's unused rules"
    );
    for (path, full) in &via_full {
        assert_eq!(
            via_narrow.get(path),
            Some(full),
            "output-neutrality broke for {path}"
        );
    }
}

/// Determinism: the narrowed pipeline run twice over the same inputs emits the same bytes. The
/// closure is a fixpoint over two sets, so an accidental hash-ordered container inside it would
/// show up here (and in nothing else — a single run is self-consistent).
#[test]
fn extern_import_narrowing_is_reproducible() {
    let export = mint_export(
        "coin = uint\nfoo = [a: uint]\nchain_a = chain_b\nchain_b = uint\nspare = [z: text]\n",
        "dep",
        "repro",
    );
    let root = scratch("repro_consumer");
    write(&root, "lib.cddl", "thing = [f: foo, c: coin, x: chain_a]\n");
    let (export_dir, import_arg) = stage_export(&export, "dep", "repro_export");
    let first = generate(&root.join("lib.cddl"), &["--extern-import", &import_arg]).unwrap();
    let second = generate(&root.join("lib.cddl"), &["--extern-import", &import_arg]).unwrap();
    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert_eq!(first, second, "two narrowed runs must emit identical bytes");
}

/// The honest remainder in the shadowing direction: the consumer NEEDS `outer`, whose export body
/// references `inner`, and the consumer defines `inner` itself. Nothing can decide that silently,
/// so it is a hard error naming the chain — the imported rule that pulled the name in, the
/// consumer's own definition of it, and both remedies.
#[test]
fn extern_import_deep_shadowing_hard_errors() {
    let export = mint_export("inner = uint\nouter = [* inner]\n", "dep", "shadow");
    let root = scratch("shadow_consumer");
    write(
        &root,
        "lib.cddl",
        "inner = _CDDL_CODEGEN_EXTERN_TYPE_\nthing = [o: outer]\n",
    );
    let (export_dir, import_arg) = stage_export(&export, "dep", "shadow_export");
    let err = generate(&root.join("lib.cddl"), &["--extern-import", &import_arg])
        .expect_err("a needed rule the consumer also defines must be refused");
    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&export_dir);

    assert!(
        err.contains("--extern-import dep"),
        "must name the flag and the dependency: {err}"
    );
    assert!(
        err.contains("`outer`") && err.contains("`inner`"),
        "must name both the puller and the shadowed rule: {err}"
    );
    assert!(
        err.contains("rename") && err.contains("_CDDL_CODEGEN_EXTERN_TYPE_"),
        "must carry both remedies (rename yours, or hand-own the type): {err}"
    );
}

/// The honest remainder in the ambiguity direction: one name needed from two dependencies' exports
/// at once. Neither export can be preferred, so the error names both and the rule.
#[test]
fn extern_import_ambiguous_name_hard_errors() {
    let alpha = mint_export("shared = uint\n", "alpha", "amb_a");
    let beta = mint_export("shared = uint\n", "beta", "amb_b");
    let root = scratch("amb_consumer");
    write(&root, "lib.cddl", "thing = [s: shared]\n");
    let (dir_a, arg_a) = stage_export(&alpha, "alpha", "amb_a_export");
    let (dir_b, arg_b) = stage_export(&beta, "beta", "amb_b_export");
    let err = generate(
        &root.join("lib.cddl"),
        &["--extern-import", &arg_a, "--extern-import", &arg_b],
    )
    .expect_err("a name needed from two exports must be refused");
    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&dir_a);
    let _ = std::fs::remove_dir_all(&dir_b);

    assert!(
        err.contains("`shared`"),
        "must name the ambiguous rule: {err}"
    );
    assert!(
        err.contains("alpha") && err.contains("beta"),
        "must name both dependencies: {err}"
    );
}

/// The seam is per-FILE and stays that way: narrowing selects which rules are concatenated, never
/// which files are checked. An export whose UNNEEDED rule carries an unknown annotation still fails
/// the strict seam, so a dependency cannot ship a malformed export that happens to be invisible to
/// one consumer.
#[test]
fn extern_import_seam_still_scans_unselected_rules() {
    let root = scratch("seamscan_consumer");
    write(&root, "lib.cddl", "thing = [f: foo]\n");
    let export_dir = scratch("seamscan_export");
    write(
        &export_dir,
        "extern-interface/dep/mod.cddl",
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\n\
         foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name Foo\n\
         never_used = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name NeverUsed @bogus_tag\n",
    );
    let import_arg = format!(
        "dep={}",
        export_dir.join("extern-interface/dep").to_str().unwrap()
    );
    let err = generate(&root.join("lib.cddl"), &["--extern-import", &import_arg])
        .expect_err("an unknown annotation on an unselected rule must still fail the seam");
    let _ = std::fs::remove_dir_all(&root);
    let _ = std::fs::remove_dir_all(&export_dir);
    assert!(
        err.contains("@bogus_tag"),
        "must name the offending token even though its rule is never imported: {err}"
    );
}

/// A dependency's raw-bytes surface sets the `RawBytesEncoding` trait flag off the WHOLE export, so
/// narrowing cannot flip it: a consumer that imports none of the dependency's raw-bytes types emits
/// exactly what it emitted before narrowing existed. Pinned against a physical stub carrying the
/// same rules, which is the channel that has always imported everything.
#[test]
fn extern_import_raw_bytes_trait_flag_survives_narrowing() {
    let export = mint_export(
        "hash = _CDDL_CODEGEN_RAW_BYTES_TYPE_\ncoin = uint\n",
        "dep",
        "rawflag",
    );
    let consumer = "thing = [c: coin]\n";

    let stub_root = scratch("rawflag_stub");
    write(&stub_root, "lib.cddl", consumer);
    for (path, content) in &export {
        let sub = path.strip_prefix("extern-interface/dep/").unwrap();
        write(
            &stub_root,
            &format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/dep/{sub}"),
            &strip_header(content),
        );
    }
    let via_stub = generate(&stub_root, &[]).expect("physical-stub generation must succeed");

    let flag_root = scratch("rawflag_flag");
    write(&flag_root, "lib.cddl", consumer);
    let (export_dir, import_arg) = stage_export(&export, "dep", "rawflag_export");
    let via_flag = generate(
        &flag_root.join("lib.cddl"),
        &["--extern-import", &import_arg],
    )
    .expect("--extern-import generation must succeed");

    let _ = std::fs::remove_dir_all(&stub_root);
    let _ = std::fs::remove_dir_all(&flag_root);
    let _ = std::fs::remove_dir_all(&export_dir);

    // The flag's in-process footprint is the rust manifest's `hex` dependency (the trait itself is
    // a static runtime file `export` concatenates, outside the generated-source map). A fixture
    // that did not exercise it would make the byte-identity below vacuous.
    assert!(
        via_stub["rust/Cargo.toml"].contains("hex"),
        "the fixture must actually exercise the raw-bytes trait flag:\n{}",
        via_stub["rust/Cargo.toml"]
    );
    for (path, stub_content) in &via_stub {
        assert_eq!(
            via_flag.get(path),
            Some(stub_content),
            "the raw-bytes trait flag must not depend on which rules were selected, at {path}"
        );
    }
}

/// The importer's rule partition must be TOTAL over a minted export: prefix plus every rule's slice
/// is the file, byte for byte. Run over the fixture dependencies whose exports carry the awkward
/// shapes — a plain-group row that is not the last rule in its file, opaque rows, transparent
/// aliases, and records — because that is the shape where a plausible partition silently fails.
///
/// A group rule's AST end offset stops BEFORE its trailing comment, so bounding rules by the AST's
/// own end would drop that row's `@rust_name` pin and weld the following rule onto it — and both
/// losses are invisible to the byte-identity tests above (a pin that agrees with the derived name
/// changes nothing when dropped, and the welded text still parses). Only a totality assertion sees
/// it, so this test asserts totality rather than any downstream symptom.
#[test]
fn extern_import_rule_partition_is_total_over_minted_exports() {
    for (spec, dep, tag) in [
        (fixture("dep-groups/lib.cddl"), "dep", "part_groups"),
        (fixture("dep-with-records/lib.cddl"), "dep", "part_records"),
        (fixture("dep/lib.cddl"), "dep", "part_plain"),
    ] {
        let export = mint_export(&spec, dep, tag);
        for (path, content) in &export {
            let parsed = crate::extern_narrow::parse_export_file(content)
                .unwrap_or_else(|e| panic!("{path} must parse standalone: {e}"));
            let mut rebuilt = content[..parsed.prefix_end].to_string();
            for rule in &parsed.rules {
                rebuilt.push_str(&content[rule.span.0..rule.span.1]);
            }
            assert_eq!(
                &rebuilt, content,
                "the rule partition dropped or duplicated bytes of {path}"
            );
        }
    }
}
