//! End-to-end integration tests: each generates a crate via the CLI (`cargo run`), then compiles
//! and CBOR round-trip-tests it (plus wasm build and json-schema build). This is the correctness
//! gate. Golden snapshots of the generated *source* live in `snapshot_tests.rs`.

use std::io::Write;

/// Fixture-appended tests compile only inside generated crates, outside the workspace clippy
/// `assertions_on_result_states` deny. Positive Result assertions must unwrap/expect so a red
/// generated-crate run carries the error payload; `is_err()` stays allowed because `unwrap_err()`
/// requires generated Ok types to implement `Debug`, which they do not uniformly derive.
#[test]
fn fixture_appended_tests_do_not_assert_is_ok() {
    let tests_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let mut fixture_test_files = std::collections::BTreeSet::new();

    for dir_entry in std::fs::read_dir(&tests_root)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", tests_root.display()))
    {
        let dir_entry = dir_entry
            .unwrap_or_else(|e| panic!("cannot read an entry under {}: {e}", tests_root.display()));
        let dir_path = dir_entry.path();
        if !dir_path.is_dir() {
            continue;
        }

        for file_entry in std::fs::read_dir(&dir_path)
            .unwrap_or_else(|e| panic!("cannot read {}: {e}", dir_path.display()))
        {
            let file_entry = file_entry.unwrap_or_else(|e| {
                panic!("cannot read an entry under {}: {e}", dir_path.display())
            });
            let file_path = file_entry.path();
            let Some(file_name) = file_path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if file_name.starts_with("tests") && file_name.ends_with(".rs") {
                fixture_test_files.insert(file_path);
            }
        }
    }

    let mut violations = Vec::new();
    for file_path in fixture_test_files {
        let contents = std::fs::read_to_string(&file_path)
            .unwrap_or_else(|e| panic!("cannot read {}: {e}", file_path.display()));
        for (line_index, line) in contents.lines().enumerate() {
            if line.contains("assert!") && line.contains(".is_ok()") {
                let display_path = file_path
                    .strip_prefix(env!("CARGO_MANIFEST_DIR"))
                    .unwrap_or(&file_path)
                    .display();
                violations.push(format!("{display_path}:{}", line_index + 1));
            }
        }
    }

    assert!(
        violations.is_empty(),
        "fixture-appended tests must unwrap/expect positive Results so failures carry payloads: {}",
        violations.join(", ")
    );
}

/// Name-level docs-contract gate for `docs/docs/command_line_flags.mdx`, sibling in spirit to
/// `src/tests/dsl_position_tests.rs`'s `comment_dsl.mdx` contract: every clap long flag must have
/// a matching `:::info `--flag`` block, and every documented block must still name a real clap
/// flag. This lints flag NAMES only; prose content stays hand-owned and is not checked.
#[test]
fn command_line_flags_mdx_documents_all_clap_long_flags() {
    use clap::CommandFactory;
    use std::collections::BTreeSet;

    let command = crate::cli::Cli::command();
    let clap_flags = command
        .get_arguments()
        .filter_map(|arg| arg.get_long())
        .filter(|name| !matches!(*name, "help" | "version"))
        .map(str::to_owned)
        .collect::<BTreeSet<_>>();

    let docs_path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/docs/command_line_flags.mdx"
    );
    let docs = std::fs::read_to_string(docs_path)
        .unwrap_or_else(|e| panic!("cannot read {docs_path}: {e}"));
    let doc_flags = docs
        .lines()
        .filter_map(|line| {
            line.trim()
                .strip_prefix(":::info `--")
                .and_then(|rest| rest.split_once('`'))
                .map(|(name, _)| name.to_owned())
        })
        .collect::<BTreeSet<_>>();

    let undocumented = clap_flags
        .difference(&doc_flags)
        .map(|flag| format!("--{flag}"))
        .collect::<Vec<_>>();
    assert!(
        undocumented.is_empty(),
        "new flag is undocumented - add a :::info block to docs/docs/command_line_flags.mdx for: {}",
        undocumented.join(", ")
    );

    let stale = doc_flags
        .difference(&clap_flags)
        .map(|flag| format!("--{flag}"))
        .collect::<Vec<_>>();
    assert!(
        stale.is_empty(),
        "stale doc block - the flag was renamed/removed from clap: {}",
        stale.join(", ")
    );
}

/// If you have multiple tests that use the same directory, please use different export_suffix
/// for each one or else the tests will be flaky as they are run concurrently.
///
/// Stable per-checkout discriminator for scratch dirs under `temp_dir()`: concurrent `cargo test`
/// runs from different checkouts/worktrees (an endorsed workflow) would otherwise share a fixed
/// path and `remove_dir_all` each other's fixtures/target mid-run.
pub(crate) fn checkout_hash() -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::env::current_dir().unwrap().hash(&mut h);
    h.finish()
}

/// Corpus fixtures whose generated crate references user-supplied code (for example,
/// `@custom_serialize` / `@custom_deserialize` functions like `my_ser`/`my_deser`), so they cannot
/// be compiled or round-tripped standalone under any emission profile. They remain covered by
/// source snapshots.
const COMPILE_SKIP: &[&str] = &["dsl_custom"];

/// Wasm-matrix cells that deliberately never compile standalone in this harness. Each entry pairs
/// with a ledger entry in `cddl-matrix/ROADMAP.md` § findings (which shape/role, the exact `E####`,
/// root cause):
/// - `extern__array-element` references a user-supplied type (undefined standalone -> E0425), while
///   the extern emit path is integration-tested separately in `tests/extern-deps`. Because the cell
///   never compiles here, it never round-trips here either.
const WASM_MATRIX_SKIP: &[&str] = &["extern__array-element"];

/// Extract the DISTINCT rustc error codes (`E####`) from compiler output, keyed off the `error[E`
/// prefix. Only a real diagnostic header (`error[E0583]: ...`) carries that prefix; the trailing
/// `For more information about this error, try `rustc --explain E0583`.` summary and any bare code
/// inside a note line do NOT (they lack `error[`), so they are correctly ignored — the set reflects
/// the failure CLASS, not every textual mention. Deterministic `BTreeSet` (repo reproducibility
/// invariant). No new deps — a plain byte scan.
fn rustc_error_codes(stderr: &str) -> std::collections::BTreeSet<String> {
    let mut codes = std::collections::BTreeSet::new();
    let needle = "error[E";
    let mut rest = stderr;
    while let Some(pos) = rest.find(needle) {
        // Advance past `error[` to the digits after `E`.
        let after_e = &rest[pos + needle.len() - 1..]; // starts at 'E'
        let mut chars = after_e.char_indices();
        // first char is 'E'
        let _ = chars.next();
        let mut enddigits = 1;
        for (i, c) in chars {
            if c.is_ascii_digit() {
                enddigits = i + c.len_utf8();
            } else {
                break;
            }
        }
        let code = &after_e[..enddigits];
        // Only accept a well-formed `E` + digits followed by the closing `]`.
        if code.len() > 1 && after_e[enddigits..].starts_with(']') {
            codes.insert(code.to_string());
        }
        rest = &rest[pos + needle.len()..];
    }
    codes
}

/// Pin `rustc_error_codes`: distinct codes only (duplicates dedupe), the `--explain E####` summary
/// line must NOT count (no `error[` prefix), and a bare code inside a note must NOT count either.
#[test]
fn rustc_error_codes_extracts_the_failure_class() {
    let stderr = "\
error[E0583]: file not found for module `serialization`\n\
 --> src/generated/a/mod.rs:1:1\n\
error[E0583]: file not found for module `serialization`\n\
error[E0432]: unresolved import `crate::generated::MapU64ToText`\n\
note: the error code E0433 is only mentioned in prose here, not as a header\n\
For more information about this error, try `rustc --explain E0583`.\n";
    let codes = rustc_error_codes(stderr);
    let expected: std::collections::BTreeSet<String> = ["E0432".to_string(), "E0583".to_string()]
        .into_iter()
        .collect();
    assert_eq!(
        codes, expected,
        "E0583 dedupes to one; E0432 counts; the prose `E0433` note and the `--explain E0583` \
         summary must NOT count (neither carries the `error[` prefix)"
    );
    assert!(rustc_error_codes("no diagnostics here").is_empty());
}

/// Multifile-placement matrix cells (`tests/matrix_multifile/<shape>__<mode>/`) that deliberately
/// do NOT compile. Each `(cell stem, expected rustc error codes, reason)` names the error class the
/// sweep pins while landing green. The three module-placement error classes it originally held
/// (E0583 alias/enum-only non-root module declaring `pub mod serialization;` without the file;
/// E0432 anonymous same-shape table importing the structural name from root scope instead of the
/// sole owner's module; E0433 cross-module named `.cbor` ref omitting the inner-type import) are all
/// fixed in `generation.rs`'s module-declaration loop and `intermediate.rs`'s
/// `scope_references`/`mark_refs`. What it holds now is the ARRAY structural-wrapper placement
/// class (the `collrec` shape, `[* <record>]` — the only SHAPES entry whose wasm representation
/// needs a generated `FooList`-style array wrapper): `mark_refs`' Array arm still hard-codes
/// ROOT_SCOPE (the remaining issue-138 half). Enumerated as cells AFTER review found the SHAPES
/// hole; the fix queue is the cddl-matrix/ROADMAP.md § findings array-wrapper entry. Four-state
/// verdict in
/// `multifile_matrix_compiles`: red+listed = expected (held here) — but ADDITIONALLY the observed
/// rustc error-code set (extracted from the captured cargo stderr) must EQUAL the pinned set, or the
/// cell's failure CLASS changed and the pin is re-triaged loudly; red+unlisted = failure (fix the
/// emitter or, deliberately, pin + ledger); green+listed = "resurfaced — remove the pin (a fix
/// landed)"; green+unlisted = pass. A skip cell whose GENERATION aborts (no rustc compile error at
/// all) is likewise a class mismatch. An up-front stale-key guard rejects a listed stem absent from
/// the projected fixture set, so the list can't rot silently.
const MULTIFILE_MATRIX_SKIP: &[(&str, &[&str], &str)] = &[
    // --- The ARRAY structural-wrapper placement class (`mark_refs`' Array arm still hard-codes
    // ROOT_SCOPE — the remaining issue-138 half; ledgered in cddl-matrix/ROADMAP.md § findings).
    // `collrec` = `recs = [* foo]` with record element `foo` in module `a`. These cells were NEVER
    // green: before the mark_refs alias-recursion (E0433) fix the named cell failed RUST-side with
    // the same inlined-alias-import class; what is pinned here is the class that remains after it.
    (
        "collrec__anon",
        &["E0425"],
        "E0425: root-minted anonymous array wrapper (`FooList`) names its non-root element type \
         `Foo` bare, without importing it from the element's module",
    ),
    (
        "collrec__named",
        &["E0432"],
        "E0432: alias-target recursion imports the structural `FooList` from root scope, but a \
         NAMED collection alias mints only its own wrapper (`Recs`) — the structural name exists \
         nowhere",
    ),
    // --- The two-type-constraint restricted wasm wrappers (`[+ T]` -> NonEmptyVec, `{+ k=>v}` ->
    // NonEmptyMap; draft/two-type-constraint-enforcement.md) reach the SAME mark_refs
    // structural-wrapper ROOT_SCOPE placement class cross-module: the loose builder (`FooList`/
    // `MapU64ToText`) is minted at root, and the restricted wrapper — or the anon dedup-to-named
    // reference — names it (and the element/rule type) bare from a non-root module. E0425 throughout
    // (the collrec Array-arm findings entry in cddl-matrix/ROADMAP.md § findings). NOT fixed in this
    // WI (the placement fix is the pre-existing issue-138 mark_refs work the finding tracks).
    (
        "necoll__anon",
        &["E0425"],
        "E0425: the anonymous `[+ uint]` dedups to module `a`'s `Nums` rule but names it bare in \
         module `b` (the restricted wrapper's anon dedup-to-named cross-module reference) — the \
         structural-wrapper ROOT_SCOPE class in cddl-matrix/ROADMAP.md § findings",
    ),
    (
        "necoll__anonb",
        &["E0425"],
        "E0425: as necoll__anon (ballast variant) — anonymous `[+ uint]` dedups to module `a`'s \
         `Nums` named bare in module `b` (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necollrec__anon",
        &["E0425"],
        "E0425: the root-minted loose `FooList`, element `Foo`, and restricted `Recs` wrappers are \
         named bare from modules `a`/`b` — the `+` analogue of collrec's Array-arm structural-wrapper \
         ROOT_SCOPE class (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necollrec__named",
        &["E0425"],
        "E0425: the restricted wrapper references the root-minted loose `FooList`/element `Foo` by \
         bare name from module `a` — the Array-arm structural-wrapper ROOT_SCOPE class \
         (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necollrec__unref",
        &["E0425"],
        "E0425: as necollrec__named — the restricted+loose array wrappers' root-minted `FooList`/`Foo` \
         named bare from module `a` (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__anon",
        &["E0425"],
        "E0425: the root-minted loose `MapU64ToText` and restricted `Mp` wrappers are named bare from \
         modules `a`/`b` — the map-side manifestation of the structural-wrapper ROOT_SCOPE class \
         (collmap is loose-only and green; cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__anonb",
        &["E0425"],
        "E0425: as nemap__anon (ballast variant) (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__named",
        &["E0425"],
        "E0425: the restricted `Mp::try_from(&MapU64ToText)` references the root-minted loose \
         `MapU64ToText` builder by bare name from module `a` — the structural-wrapper ROOT_SCOPE \
         class reached via the restricted map wrapper (cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__unref",
        &["E0425"],
        "E0425: as nemap__named — `Mp::try_from` names the root-minted loose `MapU64ToText` bare from \
         module `a` (cddl-matrix/ROADMAP.md § findings)",
    ),
];

/// Per-profile round-trip skips for `wasm_matrix_roundtrips` ONLY (never consulted by
/// `wasm_matrix_compiles`, which stays the always-on default-profile floor). Each `(profile, cell
/// stem, reason)` marks a cell whose emitted wasm round-trip surface is a known structural gap
/// UNDER THAT PROFILE — a red the sweep tolerates deliberately, distinct from `WASM_MATRIX_SKIP`'s
/// "red in EVERY profile" (extern). Such a cell COMPILEs (so it can't go in `WASM_MATRIX_SKIP`,
/// which the compile floor also consults and would flag as "resurfaced") — it is listed once per
/// affected profile and ledgered in cddl-matrix/ROADMAP.md § findings. A resurfaced guard fails the
/// gate if a listed cell starts passing, and an up-front stale-pin guard rejects entries naming a
/// dead profile or cell stem, so the list can't rot silently. Currently empty: no cell is
/// profile-specifically red.
const WASM_MATRIX_PROFILE_SKIP: &[(&str, &str, &str)] = &[];

/// Multifile-placement cells whose ROUND-TRIP (`multifile_matrix_roundtrips`) is deliberately red
/// in EVERY profile — `(cell stem, reason)`, the roundtrip precedent's shape (`WASM_MATRIX_SKIP`):
/// no rustc-error-code class assertion here, because the compile floor's `MULTIFILE_MATRIX_SKIP`
/// already pins each cell's exact failure class. The seeds are the compile-floor reds carried
/// over: both `collrec` cells' WASM crate never compiles (the `mark_refs` Array-arm
/// structural-wrapper placement class — ledgered in cddl-matrix/ROADMAP.md § findings, exact
/// E-codes pinned in `MULTIFILE_MATRIX_SKIP`), so their `cargo test` can never go green. Four-state
/// verdict + stale-key guard as the compile floor; a listed cell that starts round-tripping fails
/// the resurfaced guard (remove the pin — a fix landed).
const MULTIFILE_ROUNDTRIP_SKIP: &[(&str, &str)] = &[
    (
        "collrec__anon",
        "wasm crate never compiles: root-minted anonymous array wrapper names its non-root \
         element type bare (the Array-arm structural-wrapper findings entry in \
         cddl-matrix/ROADMAP.md; E0425 class pinned by MULTIFILE_MATRIX_SKIP)",
    ),
    (
        "collrec__named",
        "wasm crate never compiles: alias-target recursion imports the structural wrapper from \
         root scope where a NAMED collection alias mints only its own wrapper (the Array-arm \
         structural-wrapper findings entry in cddl-matrix/ROADMAP.md; E0432 class pinned by \
         MULTIFILE_MATRIX_SKIP)",
    ),
    // The two-type restricted wasm wrappers hit the same structural-wrapper ROOT_SCOPE placement
    // class cross-module (E0425 in every case, pinned by MULTIFILE_MATRIX_SKIP), so their wasm crate
    // never compiles and `cargo test` can never go green (cddl-matrix/ROADMAP.md § findings).
    (
        "necoll__anon",
        "wasm crate never compiles: anonymous `[+ uint]` dedups to module `a`'s `Nums` named bare \
         (E0425 class pinned by MULTIFILE_MATRIX_SKIP; cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necoll__anonb",
        "wasm crate never compiles: as necoll__anon, ballast variant (E0425 class pinned by \
         MULTIFILE_MATRIX_SKIP; cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necollrec__anon",
        "wasm crate never compiles: root-minted `FooList`/`Foo`/`Recs` named bare cross-module — the \
         `+` analogue of collrec's Array-arm class (E0425 class pinned by MULTIFILE_MATRIX_SKIP; \
         cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "necollrec__named",
        "wasm crate never compiles: restricted wrapper references root-minted `FooList`/`Foo` bare \
         from module `a` (E0425 class pinned by MULTIFILE_MATRIX_SKIP; cddl-matrix/ROADMAP.md \
         § findings)",
    ),
    (
        "necollrec__unref",
        "wasm crate never compiles: as necollrec__named (E0425 class pinned by MULTIFILE_MATRIX_SKIP; \
         cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__anon",
        "wasm crate never compiles: root-minted `MapU64ToText`/`Mp` named bare cross-module (E0425 \
         class pinned by MULTIFILE_MATRIX_SKIP; cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__anonb",
        "wasm crate never compiles: as nemap__anon, ballast variant (E0425 class pinned by \
         MULTIFILE_MATRIX_SKIP; cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__named",
        "wasm crate never compiles: restricted `Mp::try_from(&MapU64ToText)` references the \
         root-minted loose builder bare from module `a` (E0425 class pinned by MULTIFILE_MATRIX_SKIP; \
         cddl-matrix/ROADMAP.md § findings)",
    ),
    (
        "nemap__unref",
        "wasm crate never compiles: as nemap__named (E0425 class pinned by MULTIFILE_MATRIX_SKIP; \
         cddl-matrix/ROADMAP.md § findings)",
    ),
];

/// Per-profile round-trip skips for `multifile_matrix_roundtrips` ONLY — `(profile, cell stem,
/// reason)` for cells red under a SPECIFIC profile, distinct from `MULTIFILE_ROUNDTRIP_SKIP`'s
/// "red in every profile". Expected empty: the first full sweep found no profile-specific reds
/// (default/preserve/json all green outside the collrec pins). Same four-state contract; an
/// up-front stale-pin guard rejects entries naming a dead profile or cell stem.
const MULTIFILE_ROUNDTRIP_PROFILE_SKIP: &[(&str, &str, &str)] = &[];

/// Serialize gates that share a per-checkout scratch root under `temp_dir()`: two concurrent runs
/// of the SAME gate from the SAME checkout both `remove_dir_all` that root at start, so the second
/// deletes the first's fixtures/target mid-run (observed for `ir_conformance_corpus`). An advisory
/// flock on a sibling lock file serializes them while KEEPING the path-keyed root — so the
/// shared-cargo-target amortization that path-keying buys survives (a PID/random key would defeat
/// target reuse). The OS releases the lock on process death, so there is no stale-lock failure
/// mode. Returns the held lock; keep the returned handle alive for the whole gate (drop it and the
/// lock releases).
///
/// `scratch_name` is the scratch root's dir name (e.g. `cddl_codegen_ir_conformance_<hash>`); the
/// lock file is its SIBLING `temp_dir()/<scratch_name>.lock`, never inside the root (the root gets
/// `remove_dir_all`'d; unlinking a held lock file would let a third run acquire a fresh inode while
/// an earlier run still holds the old one).
#[must_use = "the lock releases when the returned handle is dropped — bind it for the whole gate"]
fn acquire_scratch_lock(scratch_name: &str) -> std::fs::File {
    let lock_path = std::env::temp_dir().join(format!("{scratch_name}.lock"));
    let file = std::fs::File::options()
        .create(true)
        .truncate(false)
        .write(true)
        .open(&lock_path)
        .unwrap_or_else(|e| panic!("cannot open scratch lock file {lock_path:?}: {e}"));
    match file.try_lock() {
        Ok(()) => {}
        Err(std::fs::TryLockError::WouldBlock) => {
            eprintln!(
                "another run of this gate from this checkout is active — waiting for it to finish \
                 (scratch root for {scratch_name:?} is shared; concurrent runs would clobber each \
                 other's crates). This is expected: same-checkout runs serialize on {lock_path:?}."
            );
            file.lock()
                .unwrap_or_else(|e| panic!("cannot acquire scratch lock {lock_path:?}: {e}"));
        }
        Err(std::fs::TryLockError::Error(e)) => {
            panic!("cannot acquire scratch lock {lock_path:?}: {e}")
        }
    }
    file
}

/// The one branch of `acquire_scratch_lock` worth a runnable check: a held lock is observed as
/// contended by an independent handle to the same file, and releases when dropped. (A whole-gate
/// two-process race isn't worth the machinery — this pins the advisory-flock semantics the gates
/// rely on.)
#[test]
fn acquire_scratch_lock_serializes() {
    let name = format!("cddl_codegen_scratch_lock_test_{:016x}", checkout_hash());
    let lock_path = std::env::temp_dir().join(format!("{name}.lock"));
    let _ = std::fs::remove_file(&lock_path);

    let held = acquire_scratch_lock(&name);

    // An independent handle to the SAME file must see the lock as held (advisory flock is per
    // open-file-description, so this is a genuine second acquirer, not a re-lock of `held`).
    let contender = std::fs::File::options()
        .create(true)
        .truncate(false)
        .write(true)
        .open(&lock_path)
        .unwrap();
    assert!(
        matches!(contender.try_lock(), Err(std::fs::TryLockError::WouldBlock)),
        "a second handle should observe the lock as held while the first is alive"
    );

    // Release the first lock; the contender can now take it. Match rather than `.is_ok()` so a
    // failure names WHICH way it failed: `WouldBlock` (the lock outlived its handle — a real
    // semantics break) vs a syscall error (e.g. ENOLCK under kernel lock-table pressure from the
    // suite's parallel cargo children — a transient environment condition, not a semantics break).
    // This test failed ONCE undiagnosably through the old `.is_ok()` assert (full-suite flake,
    // 2026-07-08, no repro in 60 isolated runs — ledgered in tests/TESTING_ROADMAP.md); the split
    // makes any recurrence attributable.
    std::mem::drop(held);
    match contender.try_lock() {
        Ok(()) => {}
        Err(std::fs::TryLockError::WouldBlock) => panic!(
            "the lock should be acquirable once the first handle is dropped, but try_lock \
             reported it still HELD (WouldBlock) — the advisory-flock release-on-drop semantics \
             the gates rely on are broken"
        ),
        Err(std::fs::TryLockError::Error(e)) => panic!(
            "the lock should be acquirable once the first handle is dropped, but try_lock \
             errored: {e} (raw_os_error {:?}) — a syscall failure, not a lock-semantics break; \
             if transient (e.g. ENOLCK under load), see the flake ledger in \
             tests/TESTING_ROADMAP.md",
            e.raw_os_error()
        ),
    }
    std::mem::drop(contender);
    let _ = std::fs::remove_file(&lock_path);
}

fn tool_exists(bin: &str) -> bool {
    std::process::Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Spawn cargo/wasm-pack for building a *generated* crate. The generated code is the harness's
/// own output and legitimately over-imports; CI's `setup-rust-toolchain` injects
/// `RUSTFLAGS="-D warnings"` into the job env, which nested cargo builds would otherwise inherit
/// and fail on those unused-import warnings. The root workspace keeps `-D warnings` via the
/// dedicated Build/clippy steps; only these nested generated-crate builds must be insulated.
pub(crate) fn tool_cmd(program: &str) -> std::process::Command {
    let mut c = std::process::Command::new(program);
    c.env_remove("RUSTFLAGS");
    c
}

/// Locate the ruby `cddl` gem binary for the decorrelated conformance oracle (`ir_conformance_corpus`),
/// mirroring `cddl-matrix/verify.ts`'s `resolveRubyCddl`: an explicit `RUBY_CDDL` env pin wins (and
/// fails LOUD if it points nowhere — a mispinned oracle must NOT silently fall back to gem discovery,
/// or the run would probe a different validator than the operator intended); otherwise probe the gem
/// install location `$(ruby -e 'puts Gem.user_dir')/bin/cddl`. Returns `None` when neither resolves
/// (ruby absent / gem not installed) — the caller prints a grep-stable SKIPPED marker and the gate's
/// rust half still runs. Deliberately does NOT consult `$PATH`/`which cddl`: on a dev box that is
/// typically the unrelated RUST `cddl` binary (same lineage as the generator — no decorrelation).
fn resolve_ruby_cddl() -> Option<std::path::PathBuf> {
    if let Ok(pin) = std::env::var("RUBY_CDDL") {
        let p = std::path::PathBuf::from(&pin);
        assert!(
            p.exists(),
            "RUBY_CDDL is set to '{pin}' but no such file exists — a pinned ruby cddl oracle that \
             does not exist must fail loud, not silently fall back to gem discovery"
        );
        return Some(p);
    }
    let out = std::process::Command::new("ruby")
        .args(["-e", "puts Gem.user_dir"])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let gem_dir = String::from_utf8_lossy(&out.stdout).trim().to_owned();
    let cand = std::path::PathBuf::from(gem_dir).join("bin").join("cddl");
    cand.exists().then_some(cand)
}

// ===== decode-side reference-codec differential (used by `ir_conformance_corpus`) ===================
// A CDDL-BLIND structural cross-check, complementary to the two spec validators (the rust `cddl`
// validator and the ruby `cddl` gem, both of which prove our bytes match the SPEC). This decodes each
// minted case through TWO independent CBOR codecs — `ciborium` and `minicbor` — and asserts they BOTH
// fully consume the bytes and agree on the decoded structure. It catches a well-formedness/structural
// regression a CDDL validator wouldn't (a validator can accept bytes a raw decoder would choke on, or
// vice-versa), and it has NO external dependency, so it runs even under `CDDL_RUBY_ORACLE=skip` and for
// `RUST_ORACLE_SKIP` fixtures. What it proves: two decorrelated decoders agree on our bytes' structure.
// What it CAN'T: nothing about spec conformance — that's the two cddl oracles' job.
//
// The minted corpus is generated with default flags (no `--preserve-encodings`), so every dumped case
// is a single, canonical, definite-length CBOR item. Indefinite-length handling below is defensive:
// both codecs normalize indefinite byte/text strings to their concatenated definite form, so the trees
// still agree if one ever appears. `undefined` / exotic simple values aren't in the minter's baseline;
// `ciborium::Value` can't represent them, so if one ever surfaced the differential would (correctly)
// flag a structural surprise rather than silently pass.
#[derive(Debug, PartialEq)]
enum CborTree {
    Int(i128),
    Bytes(Vec<u8>),
    Text(String),
    Array(Vec<CborTree>),
    Map(Vec<(CborTree, CborTree)>),
    Tag(u64, Box<CborTree>),
    Bool(bool),
    Null,
    Undefined,
    Float(u64), // f64 bit pattern (f16/f32 widened); NaN canonicalized so bit-noise can't false-diff
    Simple(u8),
}

fn canon_f64_bits(f: f64) -> u64 {
    if f.is_nan() {
        f64::NAN.to_bits()
    } else {
        f.to_bits()
    }
}

fn tree_via_ciborium(bytes: &[u8]) -> Result<CborTree, String> {
    // Read from a slice cursor so we can assert the reader consumed ALL bytes (no trailing garbage):
    // `impl Read for &[u8]` advances the slice, and `from_reader` stops after one item.
    let mut cursor: &[u8] = bytes;
    let value: ciborium::value::Value =
        ciborium::from_reader(&mut cursor).map_err(|e| format!("ciborium decode error: {e}"))?;
    if !cursor.is_empty() {
        return Err(format!(
            "ciborium: {} trailing byte(s) after the item (not a single well-formed item)",
            cursor.len()
        ));
    }
    ciborium_value_to_tree(value)
}

fn ciborium_value_to_tree(v: ciborium::value::Value) -> Result<CborTree, String> {
    use ciborium::value::Value;
    Ok(match v {
        Value::Integer(i) => CborTree::Int(i128::from(i)),
        Value::Bytes(b) => CborTree::Bytes(b),
        Value::Float(f) => CborTree::Float(canon_f64_bits(f)),
        Value::Text(s) => CborTree::Text(s),
        Value::Bool(b) => CborTree::Bool(b),
        Value::Null => CborTree::Null,
        Value::Tag(t, inner) => CborTree::Tag(t, Box::new(ciborium_value_to_tree(*inner)?)),
        Value::Array(items) => CborTree::Array(
            items
                .into_iter()
                .map(ciborium_value_to_tree)
                .collect::<Result<_, _>>()?,
        ),
        Value::Map(entries) => CborTree::Map(
            entries
                .into_iter()
                .map(|(k, val)| Ok((ciborium_value_to_tree(k)?, ciborium_value_to_tree(val)?)))
                .collect::<Result<Vec<_>, String>>()?,
        ),
        // `#[non_exhaustive]`: a variant ciborium adds later (or a value it can't model, e.g.
        // `undefined`) surfaces as a structural surprise rather than a silent pass.
        other => return Err(format!("ciborium: unsupported value variant {other:?}")),
    })
}

fn tree_via_minicbor(bytes: &[u8]) -> Result<CborTree, String> {
    let mut tokens = minicbor::decode::Tokenizer::new(bytes);
    let tree = minicbor_next_item(&mut tokens)?;
    // Full consumption: after one complete item the token stream must be exhausted.
    if let Some(extra) = tokens.next() {
        return Err(format!(
            "minicbor: trailing tokens after the item (not a single well-formed item): {extra:?}"
        ));
    }
    Ok(tree)
}

fn minicbor_pull<'b>(
    it: &mut minicbor::decode::Tokenizer<'_, 'b>,
) -> Result<minicbor::data::Token<'b>, String> {
    match it.next() {
        Some(Ok(t)) => Ok(t),
        Some(Err(e)) => Err(format!("minicbor decode error: {e}")),
        None => Err("minicbor: unexpected end of input".into()),
    }
}

fn minicbor_next_item(it: &mut minicbor::decode::Tokenizer<'_, '_>) -> Result<CborTree, String> {
    let tok = minicbor_pull(it)?;
    minicbor_item_from(tok, it)
}

fn minicbor_item_from(
    tok: minicbor::data::Token<'_>,
    it: &mut minicbor::decode::Tokenizer<'_, '_>,
) -> Result<CborTree, String> {
    use minicbor::data::Token;
    Ok(match tok {
        Token::Bool(b) => CborTree::Bool(b),
        Token::U8(n) => CborTree::Int(i128::from(n)),
        Token::U16(n) => CborTree::Int(i128::from(n)),
        Token::U32(n) => CborTree::Int(i128::from(n)),
        Token::U64(n) => CborTree::Int(i128::from(n)),
        Token::I8(n) => CborTree::Int(i128::from(n)),
        Token::I16(n) => CborTree::Int(i128::from(n)),
        Token::I32(n) => CborTree::Int(i128::from(n)),
        Token::I64(n) => CborTree::Int(i128::from(n)),
        Token::Int(i) => CborTree::Int(i128::from(i)),
        Token::F16(f) => CborTree::Float(canon_f64_bits(f64::from(f))),
        Token::F32(f) => CborTree::Float(canon_f64_bits(f64::from(f))),
        Token::F64(f) => CborTree::Float(canon_f64_bits(f)),
        Token::Bytes(b) => CborTree::Bytes(b.to_vec()),
        Token::String(s) => CborTree::Text(s.to_owned()),
        Token::Null => CborTree::Null,
        Token::Undefined => CborTree::Undefined,
        Token::Simple(n) => CborTree::Simple(n),
        Token::Tag(t) => CborTree::Tag(t.as_u64(), Box::new(minicbor_next_item(it)?)),
        Token::Array(n) => {
            let mut items = Vec::with_capacity(n as usize);
            for _ in 0..n {
                items.push(minicbor_next_item(it)?);
            }
            CborTree::Array(items)
        }
        Token::Map(n) => {
            let mut entries = Vec::with_capacity(n as usize);
            for _ in 0..n {
                let k = minicbor_next_item(it)?;
                let v = minicbor_next_item(it)?;
                entries.push((k, v));
            }
            CborTree::Map(entries)
        }
        Token::BeginArray => {
            let mut items = vec![];
            loop {
                let t = minicbor_pull(it)?;
                if matches!(t, Token::Break) {
                    break;
                }
                items.push(minicbor_item_from(t, it)?);
            }
            CborTree::Array(items)
        }
        Token::BeginMap => {
            let mut entries = vec![];
            loop {
                let t = minicbor_pull(it)?;
                if matches!(t, Token::Break) {
                    break;
                }
                let k = minicbor_item_from(t, it)?;
                let v = minicbor_next_item(it)?;
                entries.push((k, v));
            }
            CborTree::Map(entries)
        }
        Token::BeginBytes => {
            let mut buf = vec![];
            loop {
                match minicbor_pull(it)? {
                    Token::Break => break,
                    Token::Bytes(b) => buf.extend_from_slice(b),
                    other => {
                        return Err(format!(
                            "minicbor: unexpected token {other:?} inside indefinite byte string"
                        ));
                    }
                }
            }
            CborTree::Bytes(buf)
        }
        Token::BeginString => {
            let mut s = String::new();
            loop {
                match minicbor_pull(it)? {
                    Token::Break => break,
                    Token::String(chunk) => s.push_str(chunk),
                    other => {
                        return Err(format!(
                            "minicbor: unexpected token {other:?} inside indefinite text string"
                        ));
                    }
                }
            }
            CborTree::Text(s)
        }
        Token::Break => return Err("minicbor: unexpected Break token".into()),
    })
}

/// Canonicalize the one place the two codecs legitimately MODEL the same well-formed bytes
/// differently: RFC 8949 §3.4.3 bignum tags. `ciborium`'s byte-level decoder folds tag 2 (BIGPOS) /
/// tag 3 (BIGNEG) wrapping a definite byte string of <= 16 bytes into an integer (BIGNEG `b` becomes
/// `-1 - be(b)`, i.e. `raw ^ !0`), while `minicbor`'s token stream leaves them as `Tag(2/3, Bytes)`.
/// Both are correct decodings of the same bytes (our `biguint`/`bignint` prelude types encode as
/// exactly this), so we fold minicbor's tree to match — the divergence is representational, not a
/// structural regression. Applied to BOTH trees: a no-op on ciborium's already-folded tree. A bignum
/// that exceeds i128 (never minted by the corpus) is left as `Tag` and would surface as a divergence
/// to investigate rather than being silently mis-canonicalized.
fn fold_bignums(t: CborTree) -> CborTree {
    fn folded_int(neg: bool, b: &[u8]) -> Option<CborTree> {
        if b.len() > 16 {
            return None;
        }
        let mut buf = [0u8; 16];
        buf[16 - b.len()..].copy_from_slice(b);
        let raw = u128::from_be_bytes(buf);
        let signed = i128::try_from(raw).ok()?;
        Some(CborTree::Int(if neg { signed ^ !0 } else { signed }))
    }
    match t {
        CborTree::Tag(tag @ (2 | 3), inner) => {
            if let CborTree::Bytes(b) = inner.as_ref()
                && let Some(folded) = folded_int(tag == 3, b)
            {
                return folded;
            }
            CborTree::Tag(tag, Box::new(fold_bignums(*inner)))
        }
        CborTree::Tag(tag, inner) => CborTree::Tag(tag, Box::new(fold_bignums(*inner))),
        CborTree::Array(items) => CborTree::Array(items.into_iter().map(fold_bignums).collect()),
        CborTree::Map(entries) => CborTree::Map(
            entries
                .into_iter()
                .map(|(k, v)| (fold_bignums(k), fold_bignums(v)))
                .collect(),
        ),
        other => other,
    }
}

/// Both independent codecs must fully decode `bytes` AND agree on the decoded structure (after
/// bignum-representation canonicalization, see `fold_bignums`). Any error names which codec failed
/// and why; a tree mismatch dumps both trees. Anti-vacuity: a truncated (malformed) case must FAIL
/// this — a decoder that accepts anything can't pass (see the gate's negative control).
fn reference_codec_differential(bytes: &[u8]) -> Result<(), String> {
    let via_cib = fold_bignums(tree_via_ciborium(bytes)?);
    let via_mini = fold_bignums(tree_via_minicbor(bytes)?);
    if via_cib != via_mini {
        return Err(format!(
            "ciborium and minicbor disagree on the decoded structure:\n  ciborium: {via_cib:?}\n  minicbor: {via_mini:?}"
        ));
    }
    Ok(())
}

/// Pins the reference-codec differential's semantics on hand-derived RFC 8949 bytes so a codec bump
/// or a mapping bug is caught fast, and — the teeth — asserts truncated bytes FAIL (a decoder that
/// accepts anything can't pass). Mirrors the anti-vacuity posture the gate's negative control enforces
/// at breadth.
#[test]
fn reference_codec_differential_self_check() {
    // Well-formed vectors both codecs must fully decode and agree on.
    let good: &[(&str, &[u8])] = &[
        ("uint 0", &[0x00]),
        ("uint 100", &[0x18, 0x64]),
        ("nint -1", &[0x20]),
        // negative below i64::MIN exercises minicbor's `Int` token + ciborium's i128 integer.
        (
            "nint -2^63-1",
            &[0x3b, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        ),
        ("text \"a\"", &[0x61, 0x61]),
        ("bytes h'01'", &[0x41, 0x01]),
        ("false", &[0xf4]),
        ("true", &[0xf5]),
        ("null", &[0xf6]),
        ("array [1, 2]", &[0x82, 0x01, 0x02]),
        ("map {1: 2}", &[0xa1, 0x01, 0x02]),
        (
            "tag 0 \"2013-03-21T20:04:00Z\"",
            &[
                0xc0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2d, 0x30, 0x33, 0x2d, 0x32, 0x31, 0x54, 0x32,
                0x30, 0x3a, 0x30, 0x34, 0x3a, 0x30, 0x30, 0x5a,
            ],
        ),
        ("nested [[]]", &[0x81, 0x80]),
        // bignum tags: ciborium folds tag 2/3 + short bytes into an integer, minicbor keeps them as
        // Tag(2/3, Bytes); `fold_bignums` reconciles them (the `biguint`/`bignint` prelude case).
        ("biguint tag2 h'00'", &[0xc2, 0x41, 0x00]),
        ("bignint tag3 h'00'", &[0xc3, 0x41, 0x00]),
        ("biguint tag2 h'0100'", &[0xc2, 0x42, 0x01, 0x00]),
    ];
    for (label, bytes) in good {
        assert!(
            reference_codec_differential(bytes).is_ok(),
            "reference-codec differential should accept well-formed {label}: {}",
            reference_codec_differential(bytes).unwrap_err()
        );
    }

    // Teeth: truncating the final byte of a well-formed multi-byte item yields an incomplete item
    // both codecs must reject — so the differential returns Err (never a silent accept).
    for (label, bytes) in good.iter().filter(|(_, b)| b.len() > 1) {
        let truncated = &bytes[..bytes.len() - 1];
        assert!(
            reference_codec_differential(truncated).is_err(),
            "reference-codec differential must reject truncated (malformed) {label}"
        );
    }

    // Trailing-garbage teeth: a valid item plus an extra byte is NOT a single well-formed item.
    assert!(
        reference_codec_differential(&[0x00, 0x00]).is_err(),
        "two concatenated items must fail the single-item full-consumption check"
    );
}

/// Append the in-repo user-supplied `RawBytesEncoding` defs (`PubKey`) into a freshly generated crate
/// rooted at `out` (rust + wasm), so a `rawbytes__*` wasm-matrix cell — whose `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
/// resolves to that user type — compiles/tests standalone instead of being skipped permanently like `extern`.
/// Mirrors `run_test`'s external-file append (including the `use serialization::*;` the rust def needs for
/// `RawBytesEncoding`/`Deserialize*`); the matrix never passes `--lib-name`, so the wasm def's `cddl_lib`
/// path needs no substitution here.
///
/// `json` selects the json-flavored rust def: the json flags make generated code delegate its JSON
/// representation to the user type (serde::Serialize/Deserialize + schemars::JsonSchema bounds —
/// part of the documented `_CDDL_CODEGEN_RAW_BYTES_TYPE_` contract), so the fixture must model a
/// user who satisfies them. The wasm def has no flavor (its json fns delegate through the rust
/// type's serde).
fn append_raw_bytes_defs(out: &std::path::Path, json: bool) {
    use std::io::Write;
    let rust_def_path = if json {
        "tests/external_rust_raw_bytes_def_json"
    } else {
        "tests/external_rust_raw_bytes_def"
    };
    let rust_def = std::fs::read_to_string(rust_def_path).unwrap();
    // Append into the generated root scope (see `run_test`): the raw-bytes defs need the root scope's
    // imports and `use serialization::*;`, which live in `generated/mod.rs`, not the thin `lib.rs`.
    let mut rust_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(out.join("rust/src/generated/mod.rs"))
        .unwrap();
    rust_lib
        .write_all(b"\n\nuse serialization::*;\n\n")
        .unwrap();
    rust_lib.write_all(rust_def.as_bytes()).unwrap();
    std::mem::drop(rust_lib);
    let wasm_def = std::fs::read_to_string("tests/external_wasm_raw_bytes_def").unwrap();
    // Append into the generated root scope: the wasm defs carry `#[wasm_bindgen]`, whose macro is
    // brought into scope by a private `use` in `generated/mod.rs` (not re-exported to the thin `lib.rs`).
    let mut wasm_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(out.join("wasm/src/generated/mod.rs"))
        .unwrap();
    wasm_lib.write_all(b"\n\n").unwrap();
    wasm_lib.write_all(wasm_def.as_bytes()).unwrap();
}

fn run_test(
    dir: &str,
    options: &[&str],
    export_suffix: Option<&str>,
    external_rust_file_paths: &[std::path::PathBuf],
    external_wasm_file_paths: &[std::path::PathBuf],
    input_is_dir: bool,
    test_deps: &[&str],
) {
    use std::str::FromStr;
    let export_path = match export_suffix {
        Some(suffix) => format!("export_{suffix}"),
        None => "export".to_owned(),
    };
    let test_path = std::path::PathBuf::from_str("tests").unwrap().join(dir);
    println!("--------- running test: {dir} ---------");
    // These export dirs are throwaway regen targets (not user-owned manifests), reused across runs
    // only to amortize each crate's `target/`. Generation now MERGES the manifest instead of
    // clobbering it, so the raw `test_deps` appended into these manifests below would otherwise
    // accumulate (duplicate keys) across runs. Reset the manifests to a clean slate before
    // regenerating so the harness's append model still holds; `target/` is left intact. (The
    // user-facing manifest merge/preservation contract is exercised by
    // `cargo_manifest_disk_round_trip`, not here.)
    for manifest in [
        "rust/Cargo.toml",
        "wasm/Cargo.toml",
        "wasm/json-gen/Cargo.toml",
    ] {
        let _ = std::fs::remove_file(test_path.join(format!("{export_path}/{manifest}")));
    }
    // Each crate root `lib.rs` (rust, wasm, json-gen) is now a seed-once thin root the tool never
    // clobbers. The committed fixture exports still carry the pre-split monolithic `lib.rs` (full
    // generated content); left in place, seed-once would preserve it and it would collide with the
    // regenerated `generated/**` subtree (duplicate definitions). Delete them so the tool re-seeds a
    // clean thin root each run. (The survival-across-edits contract is covered by
    // `thin_root_seed_once`/`thin_root_wiring_survives`.)
    for root in [
        "rust/src/lib.rs",
        "wasm/src/lib.rs",
        "wasm/json-gen/src/lib.rs",
    ] {
        let _ = std::fs::remove_file(test_path.join(format!("{export_path}/{root}")));
    }
    // build and run to generate code
    let mut cargo_run = tool_cmd("cargo");
    cargo_run.arg("run").arg("--").arg(format!(
        "--output={}",
        test_path.join(&export_path).to_str().unwrap()
    ));
    // These reused export dirs accumulate hand-written scaffolding (custom-serialization helpers, the
    // appended `tests.rs`/`deser_test` modules — code the tool never emits, carrying its own comments)
    // in `generated/mod.rs` after each export. The harness's model is pristine clobber-then-append, so
    // regenerate with comment preservation OFF: default-on would (correctly) read those hand-written
    // comments as edits on now-vanished items and trap them in `compile_error!` blocks. The
    // preservation contract itself is covered by `comment_preservation_disk_round_trip`.
    cargo_run.arg("--no-preserve-comments");
    if input_is_dir {
        cargo_run.arg(format!(
            "--input={}",
            test_path.join("inputs").to_str().unwrap()
        ));
    } else {
        cargo_run.arg(format!(
            "--input={}",
            test_path.join("input.cddl").to_str().unwrap()
        ));
    }
    for option in options {
        cargo_run.arg(option);
    }
    println!("   ------ building ------");
    let cargo_run_result = cargo_run.output().unwrap();
    if !cargo_run_result.status.success() {
        eprintln!("{}", String::from_utf8(cargo_run_result.stderr).unwrap());
    }
    assert!(cargo_run_result.status.success());
    // Copy tests into generated code. The generated root scope (with the cross-module `use` imports
    // the appended tests' `use super::*;` relies on) now lives in `generated/mod.rs`, not the thin
    // seed-once `lib.rs`; append the tests there so they see exactly the imports they did when the old
    // monolithic `lib.rs` WAS the root scope. `generated/mod.rs` is regenerated every run, so appends
    // don't accumulate across reruns of the same throwaway export dir.
    let mut generated_mod = std::fs::OpenOptions::new()
        .append(true)
        .open(test_path.join(format!("{export_path}/rust/src/generated/mod.rs")))
        .unwrap();
    // some pasted-in tests need this
    generated_mod
        .write_all("\nuse serialization::*;\n".as_bytes())
        .unwrap();
    // `external_rust_file_paths` carries two kinds of hand-written code that belong in DIFFERENT scopes
    // under the thin-root split:
    //   - extern-TYPE definitions (`external_rust_defs*`): the Rust definition of a type declared
    //     `_CDDL_CODEGEN_EXTERN_TYPE_`. This CANNOT live in `generated/**` (clobbered every run, and the
    //     generator now re-exports each in-crate extern with `pub use crate::Name;` into that subtree, so
    //     a definition there would collide). A real consumer defines the extern in a hand-written module
    //     and re-exports it at the crate root; the glue resolves the bare `generated/**` references back
    //     to it. Model that by appending these into the user-owned thin `lib.rs`.
    //   - generated-scope helpers (`custom_serialization*`, conformance harnesses): free functions the
    //     generated `serialization.rs` calls via `use super::*;`, so they must land in the `generated`
    //     module scope — append them into `generated/mod.rs` exactly as before.
    // Both need `use serialization::*;` (reachable at the crate root through the seeded
    // `pub use generated::*;` glob), written into whichever file receives defs.
    let is_extern_type_def = |path: &std::path::Path| {
        path.file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with("external_rust_defs"))
    };
    if external_rust_file_paths
        .iter()
        .any(|p| is_extern_type_def(p))
    {
        let mut root_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/rust/src/lib.rs")))
            .unwrap();
        root_lib_rs
            .write_all("\nuse serialization::*;\n".as_bytes())
            .unwrap();
        for external_rust_file_path in external_rust_file_paths
            .iter()
            .filter(|p| is_extern_type_def(p))
        {
            let extern_rs = std::fs::read_to_string(external_rust_file_path).unwrap();
            root_lib_rs.write_all("\n\n".as_bytes()).unwrap();
            root_lib_rs.write_all(extern_rs.as_bytes()).unwrap();
        }
    }
    for external_rust_file_path in external_rust_file_paths
        .iter()
        .filter(|p| !is_extern_type_def(p))
    {
        let extern_rs = std::fs::read_to_string(external_rust_file_path).unwrap();
        generated_mod.write_all("\n\n".as_bytes()).unwrap();
        generated_mod.write_all(extern_rs.as_bytes()).unwrap();
    }
    let deser_test_rs = std::fs::read_to_string(
        std::path::PathBuf::from_str("tests")
            .unwrap()
            .join("deser_test"),
    )
    .unwrap();
    generated_mod.write_all("\n\n".as_bytes()).unwrap();
    generated_mod.write_all(deser_test_rs.as_bytes()).unwrap();
    let test_rs = std::fs::read_to_string(test_path.join("tests.rs")).unwrap();
    generated_mod.write_all("\n\n".as_bytes()).unwrap();
    generated_mod.write_all(test_rs.as_bytes()).unwrap();
    std::mem::drop(generated_mod);
    // add extra deps used within tests
    if !test_deps.is_empty() {
        let mut cargo_toml = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/rust/Cargo.toml")))
            .unwrap();
        for dep in test_deps {
            cargo_toml.write_all(dep.as_bytes()).unwrap();
        }
        // copy test deps to wasm too in case they're used (e.g. extern deps dir crates)
        if let Ok(mut cargo_toml_wasm) = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/Cargo.toml")))
        {
            for dep in test_deps {
                cargo_toml_wasm.write_all(dep.as_bytes()).unwrap();
            }
        }
    }
    // run tests in generated code
    println!("   ------ testing ------");
    let cargo_test = tool_cmd("cargo")
        .arg("test")
        .current_dir(test_path.join(format!("{export_path}/rust")))
        .output()
        .unwrap();
    if !cargo_test.status.success() {
        eprintln!(
            "test stderr:\n{}",
            String::from_utf8(cargo_test.stderr).unwrap()
        );
    }
    println!(
        "test stdout:\n{}",
        String::from_utf8(cargo_test.stdout).unwrap()
    );
    assert!(cargo_test.status.success());

    // wasm
    let wasm_export_dir = test_path.join(format!("{export_path}/wasm"));
    let wasm_test_path = test_path.join("tests_wasm.rs");
    // The harness knows from the flags which outputs generation promised; assert instead of gating
    // stages on `.exists()`, so an emission regression fails loudly rather than silently turning
    // the stage into a no-op.
    let wasm_expected = !options.contains(&"--wasm=false");
    if wasm_expected {
        assert!(
            wasm_export_dir.exists(),
            "no wasm crate at {wasm_export_dir:?} (--wasm=false was not passed) — generation stopped emitting it"
        );
    }
    // we must replace the lib name if it's not the default
    let custom_lib_name = options.iter().find_map(|arg: &&str| {
        arg.split_once("--lib-name=")
            .map(|(_, lib_name)| lib_name.replace('-', "_"))
    });
    // copy external wasm defs if they exist. Two kinds land in DIFFERENT scopes under the thin-root
    // split, mirroring the rust-side routing (see the `is_extern_type_def` split above):
    //   - extern-TYPE WRAPPER defs (`external_wasm_defs*`): the `#[wasm_bindgen]` wrapper of a type
    //     declared `_CDDL_CODEGEN_EXTERN_TYPE_`. The generator now re-exports each in-crate extern
    //     wrapper with `pub use crate::Name;` INTO `generated/**`, so a definition there would collide.
    //     A real consumer defines the wrapper in a hand-written wasm module and re-exports it at the
    //     wasm crate root; the glue resolves the bare `generated/**` references back to it. Model that by
    //     appending these into the user-owned thin wasm `lib.rs`. That crate root doesn't see the
    //     `wasm_bindgen`/`JsError` names generated/mod.rs privately `use`s, so add them alongside.
    //   - non-extern wasm helpers (e.g. `external_wasm_raw_bytes_def`): `#[wasm_bindgen]` wrappers for a
    //     `_CDDL_CODEGEN_RAW_BYTES_TYPE_` (NOT an extern, so no re-export glue), referenced by the
    //     generated code via same-module resolution — they stay in `generated/mod.rs` (unchanged).
    let is_extern_wasm_type_def = |path: &std::path::Path| {
        path.file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.starts_with("external_wasm_defs"))
    };
    let append_wasm = |file: &mut std::fs::File, external_wasm_file_path: &std::path::PathBuf| {
        let extern_rs = std::fs::read_to_string(external_wasm_file_path).unwrap();
        file.write_all("\n\n".as_bytes()).unwrap();
        if let Some(custom_lib_name) = &custom_lib_name {
            let replaced_extern_rs = extern_rs.replace("cddl_lib", custom_lib_name);
            file.write_all(replaced_extern_rs.as_bytes()).unwrap();
        } else {
            file.write_all(extern_rs.as_bytes()).unwrap();
        }
    };
    if wasm_expected
        && external_wasm_file_paths
            .iter()
            .any(|p| is_extern_wasm_type_def(p))
    {
        let mut wasm_root_lib = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/lib.rs")))
            .unwrap();
        wasm_root_lib
            .write_all(b"\nuse wasm_bindgen::prelude::{wasm_bindgen, JsError};\n")
            .unwrap();
        for external_wasm_file_path in external_wasm_file_paths
            .iter()
            .filter(|p| is_extern_wasm_type_def(p))
        {
            println!("trying to open (wasm root): {external_wasm_file_path:?}");
            append_wasm(&mut wasm_root_lib, external_wasm_file_path);
        }
    }
    for external_wasm_file_path in external_wasm_file_paths
        .iter()
        .filter(|p| !is_extern_wasm_type_def(p))
    {
        println!("trying to open (generated): {external_wasm_file_path:?}");
        // non-extern wasm helpers reference the generated wrapper types via same-module resolution,
        // both resolved in `generated/mod.rs` (see the wasm half of `append_raw_bytes_defs`).
        let mut wasm_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/generated/mod.rs")))
            .unwrap();
        append_wasm(&mut wasm_lib_rs, external_wasm_file_path);
    }
    if wasm_expected && wasm_test_path.exists() {
        // The hook is only real if the file's contents actually land in the crate: append into the
        // generated root scope (`generated/mod.rs`, the equivalent of the old monolithic wasm root)
        // exactly like tests.rs into `rust/src/generated/mod.rs`. A generated wasm crate ships no
        // #[test]s of its own, so without the append `cargo test` runs zero tests and passes
        // vacuously (which is what this branch silently did before).
        let mut wasm_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/generated/mod.rs")))
            .unwrap();
        let test_wasm_rs = std::fs::read_to_string(&wasm_test_path).unwrap();
        wasm_lib_rs.write_all("\n\n".as_bytes()).unwrap();
        if let Some(custom_lib_name) = &custom_lib_name {
            wasm_lib_rs
                .write_all(test_wasm_rs.replace("cddl_lib", custom_lib_name).as_bytes())
                .unwrap();
        } else {
            wasm_lib_rs.write_all(test_wasm_rs.as_bytes()).unwrap();
        }
        std::mem::drop(wasm_lib_rs);
        println!("   ------ testing (wasm) ------");
        let cargo_test_wasm = tool_cmd("cargo")
            .arg("test")
            .current_dir(&wasm_export_dir)
            .output()
            .unwrap();
        if !cargo_test_wasm.status.success() {
            eprintln!(
                "test stderr:\n{}",
                String::from_utf8(cargo_test_wasm.stderr).unwrap()
            );
        }
        println!(
            "test stdout:\n{}",
            String::from_utf8(cargo_test_wasm.stdout).unwrap()
        );
        assert!(cargo_test_wasm.status.success());
    } else if wasm_expected {
        let cargo_build_wasm = tool_cmd("cargo")
            .arg("build")
            .current_dir(&wasm_export_dir)
            .output()
            .unwrap();
        if !cargo_build_wasm.status.success() {
            eprintln!(
                "wasm build stderr:\n{}",
                String::from_utf8(cargo_build_wasm.stderr).unwrap()
            );
        }
        assert!(cargo_build_wasm.status.success());
    }
    // If the test ships a node round-trip script, build the bindings with wasm-pack and run them
    // under node. This is the ONLY layer that executes generated bindings in a JS engine, so it's
    // what catches Rust<->JS serialization-shape bugs (e.g. serde-wasm-bindgen emitting a JS `Map`
    // where the JSON/TS type says object) that `cargo build` and the snapshot suite can't observe.
    let roundtrip_script = test_path.join("roundtrip.mjs");
    if roundtrip_script.exists() {
        if tool_exists("wasm-pack") && tool_exists("node") {
            println!("   ------ testing (wasm json roundtrip) ------");
            let wasm_pack = tool_cmd("wasm-pack")
                .args(["build", "--target=nodejs", "--dev"])
                .current_dir(&wasm_export_dir)
                .output()
                .unwrap();
            if !wasm_pack.status.success() {
                eprintln!(
                    "wasm-pack stderr:\n{}",
                    String::from_utf8_lossy(&wasm_pack.stderr)
                );
            }
            assert!(wasm_pack.status.success());
            // Absolute path: node's require() treats a bare relative path as a node_modules lookup.
            let pkg_dir = std::fs::canonicalize(wasm_export_dir.join("pkg")).unwrap();
            let node = std::process::Command::new("node")
                .arg(&roundtrip_script)
                .arg(&pkg_dir)
                .output()
                .unwrap();
            print!("{}", String::from_utf8_lossy(&node.stdout));
            if !node.status.success() {
                eprintln!("node stderr:\n{}", String::from_utf8_lossy(&node.stderr));
            }
            assert!(node.status.success());
        } else {
            // Don't let CI silently skip the only JS-execution coverage we have.
            assert!(
                std::env::var_os("CI").is_none(),
                "wasm-pack and node are required to run {roundtrip_script:?} in CI"
            );
            eprintln!("skipping {roundtrip_script:?}: wasm-pack/node not found");
        }
    }
    // Run (not just build) the JSON schema export crate so its `main()` -> `export_schemas()`
    // actually executes: it creates the `schemas/` dir and writes a `<Type>.json` per root type.
    // `cargo build` only typechecked that code; nothing ever ran it, so a runtime panic in the
    // generated schema-export body (e.g. a bad path or `schemars` call) was invisible to CI.
    let json_export_dir = test_path.join(format!("{export_path}/wasm/json-gen"));
    if options.contains(&"--json-schema-export=true") {
        assert!(
            json_export_dir.exists(),
            "no json-gen crate at {json_export_dir:?} (--json-schema-export=true was passed) — generation stopped emitting it"
        );
    }
    if json_export_dir.exists() {
        // Stale schemas from a previous local run would satisfy the `schema_count > 0` assertion
        // below even if the current export writes nothing; start from a clean dir (CI is a fresh
        // checkout, so this only matters for the local signal).
        let schemas_dir = json_export_dir.join("schemas");
        let _ = std::fs::remove_dir_all(&schemas_dir);
        let cargo_run_json = tool_cmd("cargo")
            .arg("run")
            .current_dir(&json_export_dir)
            .output()
            .unwrap();
        if !cargo_run_json.status.success() {
            eprintln!(
                "json-gen run stderr:\n{}",
                String::from_utf8(cargo_run_json.stderr).unwrap()
            );
        }
        assert!(cargo_run_json.status.success());
        // `export_schemas()` succeeding isn't enough: a no-op body would also exit 0. Assert it
        // actually wrote at least one `<Type>.json` into `schemas/`, so an empty/missing dir
        // (export silently producing nothing) fails loudly instead of passing.
        let schema_count = std::fs::read_dir(&schemas_dir)
            .unwrap_or_else(|e| panic!("json-gen wrote no schemas dir {schemas_dir:?}: {e}"))
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("json"))
            .count();
        assert!(
            schema_count > 0,
            "json-gen produced no schema files in {schemas_dir:?}"
        );
    }
}

/// Generate + gate every `tests/corpus/*.cddl` crate under each emission profile. The snapshot
/// suite (`snapshot_tests::feature_corpus`) only pins the generated *source*, so a construct that
/// emits non-compiling Rust would be snapshotted as "correct"; this is the compile gate for it.
/// Runs all three `default`/`preserve`/`json` profiles the corpus is snapshotted under, since
/// non-compiling output can be flag-specific (a bare construct compiled but its preserve/json
/// variant did not). Generates with `--wasm=true` and `cargo check`s BOTH the `rust` and (when
/// emitted) `wasm` crates — the wasm bindings are a whole output mode nothing else systematically
/// compile-gates. One shared `CARGO_TARGET_DIR` so the deps build once. `int` needs no extern defs
/// here — the generator emits its own `Int` type.
///
/// Under the DEFAULT profile this is also the corpus EXECUTION gate (tests/README.md
/// § "Generated-test harness"):
/// generation adds `--emit-tests` and the rust crate runs `cargo test`, executing the emitted
/// round-trip + reject tests — a corpus construct must round-trip byte-identically, not just
/// compile. One profile keeps the wall-clock bounded (preserve/json stay compile-only for now),
/// and the emitted-module count floor keeps the execution half from going vacuous if emission
/// silently shrinks.
#[test]
fn feature_corpus_compiles() {
    use std::str::FromStr;
    let profiles = super::ALL_PROFILES;
    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    let corpus_stems: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap())
        .collect();
    for stem in COMPILE_SKIP {
        assert!(
            corpus_stems.contains(stem),
            "COMPILE_SKIP names corpus fixture `{stem}` that no longer exists in tests/corpus — \
             stale pin, remove or fix it"
        );
    }

    // Scratch dir + one shared target so cbor_event & friends build once (~30 tiny crates × 3).
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_corpus_compile_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![];
    let mut emitted_test_modules = 0usize;
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        if COMPILE_SKIP.contains(&stem) {
            continue;
        }
        for (profile, extra) in profiles {
            let label = format!("{stem}/{profile}");
            let out = root.join(format!("{stem}__{profile}"));
            let emit_tests = *profile == "default";
            // generate rust + wasm so both crates are compile-gated
            let gen_out = tool_cmd("cargo")
                .args(["run", "--"])
                .arg(format!("--input={}", input.to_str().unwrap()))
                .arg(format!("--output={}", out.to_str().unwrap()))
                .arg("--wasm=true")
                .args(if emit_tests {
                    &["--emit-tests=true"][..]
                } else {
                    &[][..]
                })
                .args(*extra)
                .output()
                .unwrap();
            if !gen_out.status.success() {
                failures.push(format!(
                    "{label}: generation failed\n{}",
                    String::from_utf8_lossy(&gen_out.stderr)
                ));
                continue;
            }
            // cargo check the generated rust crate, then the wasm crate, and — under the json
            // profile — the json-gen crate. The wasm crate is a whole output mode nothing else
            // systematically compile-gates; a host `cargo check` catches type/signature errors in
            // the generated bindings (wrong accessor return type, boundary `.into()`/`.clone()`
            // slips) without needing the wasm32 target — `wasm-bindgen` is just a normal dependency
            // and the shared target dir amortizes its build. json-gen is an INDEPENDENT nested
            // crate (not a dependency of wasm/), so checking wasm/ never touches it, yet its
            // per-fixture `export_schemas()` body is snapshot-pinned by feature_corpus — leaving it
            // out re-opens the exact hole this gate exists to close.
            let crate_subs: &[&str] = if *profile == "json" {
                &["rust", "wasm", "wasm/json-gen"]
            } else {
                &["rust", "wasm"]
            };
            for crate_sub in crate_subs.iter().copied() {
                let crate_dir = out.join(crate_sub);
                if !crate_dir.exists() {
                    // A missing crate de-gates the fixture: that's a failure, not a skip (mirrors
                    // wasm_matrix_compiles). Every fixture emits rust/, and with --wasm=true also
                    // wasm/ (+ json-gen under the json profile) — if that ever becomes legitimately
                    // untrue for a fixture, allowlist it explicitly like COMPILE_SKIP.
                    failures.push(format!(
                        "{label} ({crate_sub}): crate dir missing — the fixture is no longer being compile-gated"
                    ));
                    continue;
                }
                // Under the default profile (where `--emit-tests` is passed) both the rust AND the
                // wasm crate EXECUTE their emitted tests (strictly stronger than check: `cargo test`
                // compiles the lib and runs the round-trip/reject module). `cargo check` never
                // compiles `#[cfg(test)]` code, so the wasm crate's emitted `cddl_generated_wasm_tests`
                // module (cross-crate byte differential + wire round-trip + accessor read-back) is only
                // type-checked and executed under `cargo test` — giving the corpus its wasm ROUND-TRIP
                // coverage, not just wasm compile coverage. json-gen stays check-only (json profile,
                // no --emit-tests). preserve/json profiles stay check-only throughout.
                let cargo_cmd = if emit_tests && (crate_sub == "rust" || crate_sub == "wasm") {
                    "test"
                } else {
                    "check"
                };
                let check = tool_cmd("cargo")
                    .arg(cargo_cmd)
                    .current_dir(&crate_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                if !check.status.success() {
                    failures.push(format!(
                        "{label} ({crate_sub}): cargo {cargo_cmd} failed\n{}\n{}",
                        String::from_utf8_lossy(&check.stdout),
                        String::from_utf8_lossy(&check.stderr)
                    ));
                }
            }
            if emit_tests
                && std::fs::read_to_string(out.join("rust/src/generated/mod.rs"))
                    .unwrap_or_default()
                    .contains("mod cddl_generated_tests")
            {
                emitted_test_modules += 1;
            }
        }
    }
    // execution-half vacuous-pass guard: most corpus fixtures mint at least one round-trip/reject
    // test today (41 of 44; the rest are transparent aliases / pure c-enums). A big drop means the
    // emitter's coverage silently shrank, not that the corpus got simpler.
    assert!(
        emitted_test_modules >= 38,
        "only {emitted_test_modules} corpus fixtures emitted a generated-test module (expected >= 38) — emit_tests coverage shrank"
    );
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        failures.is_empty(),
        "corpus crates failed to compile:\n\n{}",
        failures.join("\n\n")
    );
}

/// Pins the documented first-run experience in `docs/docs/getting_started.mdx`: that doc tells a new
/// user to run `cargo run -- --input=example/test.cddl --output=export` (and the release-binary
/// variant) as their very first command, so `example/test.cddl` must always generate a crate that
/// actually compiles — a silent break here is the worst kind of docs rot (it greets every newcomer).
/// This runs that command VERBATIM (no extra flags, so the test can't drift from what the doc
/// promises — wasm is emitted by default, so the wasm crate is gated too), generating into
/// `example/export` just like the doc, then `cargo check`s BOTH the generated rust crate and the
/// generated wasm crate. One shared `CARGO_TARGET_DIR` (the `feature_corpus_compiles`
/// pattern) so the deps build once. Hand-rolled rather than `run_test` on purpose: `run_test`
/// requires a `tests.rs` round-trip fixture, and `example/` deliberately ships only the spec (its
/// round-trip coverage is `--emit-tests`' job) — so this gate is generate + check, nothing appended.
#[test]
fn getting_started_example() {
    let input = std::path::Path::new("example/test.cddl");
    assert!(
        input.exists(),
        "{input:?} is the spec docs/docs/getting_started.mdx runs verbatim — it must exist"
    );
    let out = std::path::Path::new("example/export");

    // Scratch target dir so cbor_event & wasm-bindgen build once for both crate checks below.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_getting_started_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    // The documented command, verbatim.
    let gen_out = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", input.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .output()
        .unwrap();
    assert!(
        gen_out.status.success(),
        "generation from {input:?} failed:\n{}",
        String::from_utf8_lossy(&gen_out.stderr)
    );

    for crate_sub in ["rust", "wasm"] {
        let crate_dir = out.join(crate_sub);
        assert!(
            crate_dir.exists(),
            "no {crate_sub} crate at {crate_dir:?} — generation stopped emitting it"
        );
        let check = tool_cmd("cargo")
            .arg("check")
            .current_dir(&crate_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        assert!(
            check.status.success(),
            "cargo check failed for the {crate_sub} crate generated from {input:?}:\n{}\n{}",
            String::from_utf8_lossy(&check.stdout),
            String::from_utf8_lossy(&check.stderr)
        );
    }
    let _ = std::fs::remove_dir_all(&root);
}

/// The wasm-ABI matrix compile-gate. `cddl-matrix/project_wasm_matrix.ts` enumerates the cross-product
/// {wasm-ABI type-shape} × {boundary role} into `tests/matrix_wasm/*.cddl` — one minimal fixture per
/// cell. This generates each `--wasm=true` and `cargo check`s the wasm crate (which pulls the rust crate
/// in as a path dep, so rust-side errors surface here too). It's the *coverage* counterpart to
/// `feature_corpus_compiles`'s oracle: the CBOR-feature corpus doesn't individuate a type's wasm-ABI
/// representation (`is_copy` × `directly_wasm_exposable` × has-a-wrapper-`RustStruct`), so a whole class
/// of boundary bugs (wrong accessor type, bad `.into()`/`.clone()`/by-ref slips, dangling map typedefs)
/// was invisible. Here an un-covered boundary bug shows up as a specific red cell instead of by luck.
///
/// `WASM_MATRIX_SKIP` holds the deliberately-red cells (pre-existing gaps tracked in
/// `cddl-matrix/ROADMAP.md`, plus `extern`, which references a user-supplied type and can't compile
/// standalone). `rawbytes__*` cells also reference a user-supplied type, but its defs are in-repo —
/// `append_raw_bytes_defs` splices them in per cell (same 2 commands, no extra cargo invocation), so
/// those cells compile for real instead of being skipped. A fix lands by taking its cell off
/// `WASM_MATRIX_SKIP` — and the guard below fails if a `WASM_MATRIX_SKIP` cell starts compiling, so
/// the list can't silently rot. A cell that's red but NOT in `WASM_MATRIX_SKIP` fails the test: it's a
/// new wasm-ABI bug to fix or (deliberately, with a ledger entry) skip-list. `cargo check`s only the
/// wasm crate (single default profile) — lighter than `feature_corpus_compiles`. The round-trip
/// upgrade of this gate exists as `wasm_matrix_roundtrips` (manual, full tier); this compile floor
/// stays always-on beside it.
#[test]
fn wasm_matrix_compiles() {
    use std::str::FromStr;

    let dir = std::path::PathBuf::from_str("tests/matrix_wasm").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(
        !entries.is_empty(),
        "no wasm-matrix fixtures in {dir:?} (run `bun run project_wasm_matrix.ts`)"
    );

    let cell_stems: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap())
        .collect();
    for stem in WASM_MATRIX_SKIP {
        assert!(
            cell_stems.contains(stem),
            "WASM_MATRIX_SKIP names cell `{stem}` that no longer exists in tests/matrix_wasm — \
             stale pin, remove or fix it"
        );
    }

    // Scratch dir + one shared target so cbor_event/wasm-bindgen build once, then each tiny crate checks.
    let root =
        std::env::temp_dir().join(format!("cddl_codegen_wasm_matrix_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT on WASM_MATRIX_SKIP — real bugs
    let mut resurfaced = vec![]; // WASM_MATRIX_SKIP cells that now compile — remove them
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        let skipped = WASM_MATRIX_SKIP.contains(&stem);
        let out = root.join(stem);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .output()
            .unwrap();
        if !gen_out.status.success() {
            // A generation failure is also "red". Only a NON-skipped one is a test failure.
            if !skipped {
                failures.push(format!(
                    "{stem}: generation failed\n{}",
                    String::from_utf8_lossy(&gen_out.stderr)
                ));
            }
            continue;
        }
        let wasm_dir = out.join("wasm");
        if !wasm_dir.exists() {
            // Every cell wraps its shape in a composite `holder`, so a wasm crate is always expected.
            // Treat a missing one symmetrically: for a skip cell it means the red is gone ("resurfaced");
            // for a non-skip cell it's a real coverage regression (the cell silently stops being gated),
            // not a pass — fail loudly rather than count it green.
            if skipped {
                resurfaced.push(format!("{stem} (emits no wasm crate)"));
            } else {
                failures.push(format!(
                    "{stem}: generated no wasm crate (expected a wasm wrapper for every cell — the cell \
                     is no longer being compile-gated)"
                ));
            }
            continue;
        }
        // `rawbytes__*` cells resolve `_CDDL_CODEGEN_RAW_BYTES_TYPE_` to a user-supplied type (`PubKey`),
        // undefined in a bare crate. Unlike `extern` (whose defs live only in tests/extern-deps), the raw-bytes
        // defs are in-repo, so append them and the cell compiles for real instead of being skipped.
        if stem.starts_with("rawbytes__") {
            append_raw_bytes_defs(&out, false);
        }
        let check = tool_cmd("cargo")
            .arg("check")
            .current_dir(&wasm_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        match (skipped, check.status.success()) {
            (false, false) => failures.push(format!(
                "{stem}: cargo check failed (new wasm-ABI red cell — fix the emitter or, deliberately, \
                 add to WASM_MATRIX_SKIP + cddl-matrix/ROADMAP.md)\n{}",
                String::from_utf8_lossy(&check.stderr)
            )),
            (true, true) => resurfaced.push(stem.to_string()),
            _ => {} // (false,true)=green as expected; (true,false)=red as expected
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these WASM_MATRIX_SKIP-listed wasm-matrix cells now compile — remove them from \
         WASM_MATRIX_SKIP (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "wasm-matrix cells failed to compile:\n\n{}",
        failures.join("\n\n")
    );
}

/// The multifile-placement matrix compile-gate. `cddl-matrix/project_multifile_matrix.ts` enumerates
/// {type-shape} × {cross-module reference mode} into two-module DIRECTORY fixtures
/// `tests/matrix_multifile/<shape>__<mode>/` (`lib.cddl` = root scope, `a.cddl` = the shape's defs,
/// `b.cddl` = the reference). It's the *placement* counterpart to the construct gates, which all feed
/// the generator SINGLE-file specs and so only ever verify root-scope placement: multifile emission
/// branches on scope (`mark_refs`' hard-coded ROOT_SCOPE for the generator-invented structural
/// wrappers), and that region has no other coverage.
///
/// Each cell is generated with DIRECTORY input (`--input=tests/matrix_multifile/<cell>`) `--wasm=true`,
/// then only the WASM crate is `cargo check`ed — like `wasm_matrix_compiles`, the wasm crate pulls the
/// rust crate in as a path dep, so rust-side breakage (e.g. an alias/table-only module declaring
/// `pub mod serialization;` without emitting the file, or a cross-module structural/inner-type import
/// landing in the wrong scope) surfaces transitively through the wasm check; one crate keeps the
/// wall-clock bounded.
///
/// `MULTIFILE_MATRIX_SKIP` holds the deliberately-red cells — currently the two `collrec`
/// array-structural-wrapper cells (the `mark_refs` Array-arm placement class; see the const's doc).
/// The three historical module-placement error classes (E0583 alias/table-only serialization stub,
/// E0432 anonymous same-shape table importing the structural name from root scope, E0433
/// cross-module named `.cbor` ref omitting the inner-type import) are all fixed. Four-state verdict per
/// cell: red+listed = expected —
/// but ADDITIONALLY the observed rustc error-code set (extracted from the captured cargo stderr) must
/// EQUAL the pin's declared codes; red-with-the-wrong-class is a loud "the cell's failure class
/// changed — re-triage the pin" (a skip cell whose GENERATION aborts, producing no rustc error at all,
/// is likewise a class mismatch); red+unlisted = a new placement finding to fix or (deliberately, with
/// a ledger entry) pin; green+listed = "resurfaced — remove the pin (a fix landed)"; green+unlisted =
/// pass. An up-front stale-key guard rejects a listed stem absent from the projected set, and a missing
/// wasm crate is handled symmetrically (a red for a non-skip cell, a resurface for a skip cell), so the
/// ledger can't silently rot. Always-on (no `#[ignore]`): it joins the default `cargo test` / check.ts
/// local tier.
#[test]
fn multifile_matrix_compiles() {
    use std::str::FromStr;

    let dir = std::path::PathBuf::from_str("tests/matrix_multifile").unwrap();
    let mut cell_dirs: Vec<std::path::PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    cell_dirs.sort();
    assert!(
        !cell_dirs.is_empty(),
        "no multifile-matrix fixtures in {dir:?} (run `bun run project_multifile_matrix.ts`)"
    );

    let cell_stems: std::collections::BTreeSet<&str> = cell_dirs
        .iter()
        .map(|p| p.file_name().unwrap().to_str().unwrap())
        .collect();
    for (stem, _codes, _reason) in MULTIFILE_MATRIX_SKIP {
        assert!(
            cell_stems.contains(stem),
            "MULTIFILE_MATRIX_SKIP names cell `{stem}` that no longer exists in tests/matrix_multifile \
             — stale pin, remove or fix it"
        );
    }

    // Scratch dir + one shared target so cbor_event/wasm-bindgen build once, then each tiny crate checks.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_multifile_matrix_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT on MULTIFILE_MATRIX_SKIP — real bugs
    let mut resurfaced = vec![]; // MULTIFILE_MATRIX_SKIP cells that now compile — remove them
    for input in &cell_dirs {
        let stem = input.file_name().unwrap().to_str().unwrap();
        let pin = MULTIFILE_MATRIX_SKIP.iter().find(|(s, _, _)| *s == stem);
        let skipped = pin.is_some();
        let out = root.join(stem);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .output()
            .unwrap();
        if !gen_out.status.success() {
            match pin {
                // The pin claims a rustc COMPILE-error class, but generation aborted — there is no
                // rustc error code at all, so the cell's failure class changed.
                Some((_, codes, reason)) => failures.push(format!(
                    "{stem}: generation aborted, but MULTIFILE_MATRIX_SKIP pins a rustc compile-error \
                     class {codes:?} ({reason}) — a generation abort produces no rustc error code, so \
                     the cell's failure class changed — re-triage the pin and its \
                     cddl-matrix/ROADMAP.md finding\n{}",
                    String::from_utf8_lossy(&gen_out.stderr)
                )),
                // A NON-skipped generation failure is a plain red.
                None => failures.push(format!(
                    "{stem}: generation failed\n{}",
                    String::from_utf8_lossy(&gen_out.stderr)
                )),
            }
            continue;
        }
        let wasm_dir = out.join("wasm");
        if !wasm_dir.exists() {
            // Every cell's module `b` is a composite record, so a wasm crate is always expected. Treat a
            // missing one symmetrically (like `wasm_matrix_compiles`): a skip cell's red is gone
            // ("resurfaced"); a non-skip cell silently stops being gated — a coverage regression, not a pass.
            if skipped {
                resurfaced.push(format!("{stem} (emits no wasm crate)"));
            } else {
                failures.push(format!(
                    "{stem}: generated no wasm crate (expected a wasm wrapper for every cell — the cell \
                     is no longer being compile-gated)"
                ));
            }
            continue;
        }
        let check = tool_cmd("cargo")
            .arg("check")
            .current_dir(&wasm_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        match (skipped, check.status.success()) {
            (false, false) => failures.push(format!(
                "{stem}: cargo check failed (new multifile-placement red cell — fix the emitter or, \
                 deliberately, add to MULTIFILE_MATRIX_SKIP + cddl-matrix/ROADMAP.md)\n{}",
                String::from_utf8_lossy(&check.stderr)
            )),
            (true, true) => resurfaced.push(stem.to_string()),
            (true, false) => {
                // Red as expected — but the observed rustc error-code SET must EQUAL the pin's, or
                // the cell's failure CLASS changed and the pin must be re-triaged (class assertion).
                let (_, pinned_codes, reason) = pin.unwrap();
                let stderr = String::from_utf8_lossy(&check.stderr);
                let observed = rustc_error_codes(&stderr);
                let expected: std::collections::BTreeSet<String> =
                    pinned_codes.iter().map(|c| c.to_string()).collect();
                if observed != expected {
                    failures.push(format!(
                        "{stem}: red as pinned, but the observed rustc error-code set {observed:?} \
                         does NOT equal the pinned set {expected:?} ({reason}) — the cell's failure \
                         class changed — re-triage the pin and its cddl-matrix/ROADMAP.md finding. \
                         Captured stderr:\n{stderr}"
                    ));
                }
            }
            (false, true) => {} // green as expected
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these MULTIFILE_MATRIX_SKIP-listed cells now compile — remove them from MULTIFILE_MATRIX_SKIP \
         (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "multifile-matrix cells failed to compile:\n\n{}",
        failures.join("\n\n")
    );
}

/// Always-on pin for the multifile `--emit-tests` scope-import emission. For DIRECTORY input the
/// generated test modules land at the generated ROOT of each crate (`generation.rs` raws them into
/// `rust_lib()`/`wasm_lib()`) while the minted values name submodule types bare (`St`, `Bholder`),
/// so each emitted header must glob-import every declared non-root module (`use super::a::*;`) —
/// without them EVERY multifile `--emit-tests` crate fails `cargo test` with E0433 ("cannot find
/// type `St` in this scope"): malformed emission of a shipped feature. In-process
/// (`api::generated_strings`, no nested cargo build) so it stays cheap enough for the default
/// suite; actually EXECUTING these modules across the matrix is `multifile_matrix_roundtrips`'
/// job (full tier). The single-file half pins the byte-identity guard: no non-root scopes → no
/// glob lines and no injected `#[allow(unused_imports)]`, so single-file output is unchanged.
#[test]
fn emit_tests_multifile_scope_imports() {
    use clap::Parser;

    let get = |files: &std::collections::BTreeMap<String, String>, key: &str| -> String {
        files
            .get(key)
            .unwrap_or_else(|| {
                panic!(
                    "no `{key}` among generated files; got: {:?}",
                    files.keys().collect::<Vec<_>>()
                )
            })
            .clone()
    };

    // Multifile: one placement-matrix cell — module `a` holds the shape (`st`), module `b` the
    // cross-module reference (`bholder = [field0: st]`).
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input=tests/matrix_multifile/struct__named",
        "--output=unused_in_memory_generation",
        "--wasm=true",
        "--emit-tests=true",
    ]);
    let files = crate::api::generated_strings(&cli).unwrap();
    for (which, key, header) in [
        (
            "rust",
            "rust/src/generated/mod.rs",
            "mod cddl_generated_tests",
        ),
        (
            "wasm",
            "wasm/src/generated/mod.rs",
            "mod cddl_generated_wasm_tests",
        ),
    ] {
        let module = get(&files, key);
        assert!(
            module.contains(header),
            "{which}: expected `{header}` in {key} (did --emit-tests stop minting for this cell?)\n{module}"
        );
        for glob in ["use super::a::*;", "use super::b::*;"] {
            assert!(
                module.contains(glob),
                "{which}: the root-level test module names submodule types bare, so {key} must \
                 carry `{glob}` — without it the module is E0433-uncompilable (malformed \
                 emission)\n{module}"
            );
        }
    }

    // Single-file: the same shapes at root scope must stay glob-free — the emitters' non-empty
    // guard keeps single-file `--emit-tests` output byte-identical.
    let spec_path = std::env::temp_dir().join(format!(
        "cddl_codegen_emit_tests_scope_imports_{:016x}.cddl",
        checkout_hash()
    ));
    std::fs::write(
        &spec_path,
        "st = [a: uint, b: text]\nbholder = [field0: st]\n",
    )
    .unwrap();
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        &format!("--input={}", spec_path.to_str().unwrap()),
        "--output=unused_in_memory_generation",
        "--wasm=true",
        "--emit-tests=true",
    ]);
    let files = crate::api::generated_strings(&cli).unwrap();
    let _ = std::fs::remove_file(&spec_path);
    for (which, key, header) in [
        (
            "rust",
            "rust/src/generated/mod.rs",
            "mod cddl_generated_tests",
        ),
        (
            "wasm",
            "wasm/src/generated/mod.rs",
            "mod cddl_generated_wasm_tests",
        ),
    ] {
        let module = get(&files, key);
        assert!(
            module.contains(header),
            "{which}: expected `{header}` in single-file {key}\n{module}"
        );
        assert!(
            !module.contains("use super::a::*;") && !module.contains("use super::b::*;"),
            "{which}: single-file output (all types at root scope) must not grow scope glob \
             imports — the non-empty guard regressed\n{module}"
        );
    }
    let rust_mod = get(&files, "rust/src/generated/mod.rs");
    assert!(
        !rust_mod.contains("#[allow(unused_imports)]\nmod cddl_generated_tests"),
        "rust: single-file output must not grow the glob-only `#[allow(unused_imports)]` — the \
         non-empty guard regressed\n{rust_mod}"
    );
}

/// The wasm-ABI matrix ROUND-TRIP gate — the behavioural upgrade of `wasm_matrix_compiles`. Same cell
/// enumeration (`tests/matrix_wasm/*.cddl`), but each cell is generated with `--wasm=true
/// --emit-tests=true` and `cargo test`ed (not `cargo check`ed): this compiles AND RUNS the emitted
/// `cddl_generated_wasm_tests` module (cross-crate byte differential + wire round-trip + accessor
/// read-back + boundary acceptance — see `src/emit_tests_wasm.rs`). A cell can `cargo check` green
/// (compile gate) while the wrapper API does a semantically wrong same-type conversion; that only
/// surfaces when the emitted assertions RUN, which is what this gate adds.
///
/// Each cell is swept across `super::ALL_PROFILES` (default / preserve / json) — `--preserve-encodings`
/// and the json flags substantially change codegen, so the wasm behavioural verdict must hold under
/// each. This is strictly the WASM round-trip verdict per profile; rust-side non-default round-trip
/// breadth stays `feature_corpus_roundtrips_nondefault_profiles`' job (this gate only `cargo test`s
/// the wasm crate, exactly as the default-only version did). Each `(profile, cell)` output lives in
/// `{stem}__{profile}` under one shared `CARGO_TARGET_DIR`, and its crate dir is freed after its
/// verdict (the disk-space pattern from `feature_corpus_roundtrips_nondefault_profiles`).
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d so it stays out of CI under the feature freeze (`cargo test`
/// per cell across three profiles is materially heavier than the compile gate's per-cell `cargo
/// check`). Run it with `cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored`.
///
/// Two skip lists, both four-state (verdict matrix as `wasm_matrix_compiles`, labels `{stem}/{profile}`):
/// `WASM_MATRIX_SKIP` holds cells red in EVERY profile (extern); `WASM_MATRIX_PROFILE_SKIP` (this gate
/// only) holds `(profile, cell, reason)` cells red under a SPECIFIC profile. A red non-skip cell fails
/// (real finding, or deliberately skip-list it with a ledger reason); a skip cell that now passes fails
/// the resurfaced guard (a fix landed — take it off the list). An up-front stale-pin guard rejects a
/// `WASM_MATRIX_PROFILE_SKIP` entry naming a dead profile or cell stem. `wasm_matrix_compiles` stays
/// byte-for-byte untouched: it remains the always-on default-profile CI compile floor (non-default
/// compile coverage is subsumed by this gate's `cargo test` at full tier, per cost policy); this is
/// the manual per-profile round-trip verdict on top. Its own scratch dir name lets it run beside the
/// compile gate. Note: a cell whose shape mints no wasm test surface (nothing the emitter can
/// faithfully build — e.g. a pure c-enum, or a wrapper/collection ctor arg with no wasm build) simply
/// emits no module and `cargo test` passes with zero emitted tests; that is a legitimate green here
/// (the emitter skips loudly), NOT a false pass — the compile gate already pins that the cell's wasm
/// ABI compiles.
#[test]
#[ignore]
fn wasm_matrix_roundtrips() {
    use std::str::FromStr;

    // The wrapper-collection struct-field cells (`coll__struct-field` `nums = [* uint]`,
    // `collmap__struct-field`,
    // `passthrumap__struct-field`) round-trip green: the emitter builds their `&Nums`/`&Mp` ctor arg
    // through the wrapper's `new`/`add`/`insert` API, taking the wrapper NAME from the UNRESOLVED
    // conceptual type (`emit_tests_wasm::wasm_collection_build`) so it doesn't shallow-resolve the
    // alias into a bare `vec![..]` against the `&Nums` param — so they are NOT
    // `WASM_MATRIX_SKIP`-listed.

    let dir = std::path::PathBuf::from_str("tests/matrix_wasm").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(
        !entries.is_empty(),
        "no wasm-matrix fixtures in {dir:?} (run `bun run project_wasm_matrix.ts`)"
    );

    // Reject `WASM_MATRIX_PROFILE_SKIP` entries naming a dead profile or cell stem up front — a
    // stale pin would rot silently (its resurfaced guard only fires on a (profile, cell) the sweep
    // actually visits). Mirrors the `EXPECTED_GENERATION_FAIL` stale-pin guard in wasm_parity_tests.
    let cell_stems: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap())
        .collect();
    for stem in WASM_MATRIX_SKIP {
        assert!(
            cell_stems.contains(stem),
            "WASM_MATRIX_SKIP names cell `{stem}` that no longer exists in tests/matrix_wasm — \
             stale pin, remove or fix it"
        );
    }
    for (profile, stem, _) in WASM_MATRIX_PROFILE_SKIP {
        assert!(
            super::ALL_PROFILES.iter().any(|(name, _)| name == profile),
            "WASM_MATRIX_PROFILE_SKIP names unknown profile `{profile}` — stale pin, remove or fix it"
        );
        assert!(
            cell_stems.contains(stem),
            "WASM_MATRIX_PROFILE_SKIP names cell `{stem}` that no longer exists in tests/matrix_wasm \
             — stale pin, remove or fix it"
        );
    }

    // Own scratch dir (distinct from wasm_matrix_compiles) + one shared target so cbor_event/
    // wasm-bindgen/the libtest harness build once, then each tiny crate tests incrementally. The
    // shared target survives across all profiles/cells; each per-cell output dir is freed after its
    // verdict (disk-space pattern from feature_corpus_roundtrips_nondefault_profiles).
    let scratch_name = format!("cddl_codegen_wasm_matrix_rt_{:016x}", checkout_hash());
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT skip-listed — real findings
    let mut resurfaced = vec![]; // skip-listed cells that now pass — remove them
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        // Skipped in EVERY profile (extern).
        let skipped_all = WASM_MATRIX_SKIP.contains(&stem);
        for (profile, extra) in super::ALL_PROFILES {
            let label = format!("{stem}/{profile}");
            // Skipped in EVERY profile, or in THIS specific profile.
            let skipped = skipped_all
                || WASM_MATRIX_PROFILE_SKIP
                    .iter()
                    .any(|(p, s, _)| p == profile && s == &stem);
            let out = root.join(format!("{stem}__{profile}"));
            let gen_out = tool_cmd("cargo")
                .args(["run", "--"])
                .arg(format!("--input={}", input.to_str().unwrap()))
                .arg(format!("--output={}", out.to_str().unwrap()))
                .arg("--wasm=true")
                .arg("--emit-tests=true")
                .args(*extra)
                .output()
                .unwrap();
            if !gen_out.status.success() {
                if !skipped {
                    failures.push(format!(
                        "{label}: generation failed\n{}",
                        String::from_utf8_lossy(&gen_out.stderr)
                    ));
                }
                let _ = std::fs::remove_dir_all(&out);
                continue;
            }
            let wasm_dir = out.join("wasm");
            if !wasm_dir.exists() {
                // Every cell wraps its shape in a composite `holder`, so a wasm crate is always expected.
                if skipped {
                    resurfaced.push(format!("{label} (emits no wasm crate)"));
                } else {
                    failures.push(format!(
                        "{label}: generated no wasm crate (expected a wasm wrapper for every cell — the \
                         cell is no longer being round-trip-gated)"
                    ));
                }
                let _ = std::fs::remove_dir_all(&out);
                continue;
            }
            // See wasm_matrix_compiles: append the in-repo raw-bytes defs so `rawbytes__*` cells
            // compile/run under every profile. The wire defs are profile-agnostic (pinned by
            // raw_bytes_preserve); json selects the flavor carrying the serde/schemars derives the
            // json flags impose on user-supplied types.
            if stem.starts_with("rawbytes__") {
                append_raw_bytes_defs(&out, *profile == "json");
            }
            let test = tool_cmd("cargo")
                .arg("test")
                .current_dir(&wasm_dir)
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            match (skipped, test.status.success()) {
                (false, false) => failures.push(format!(
                    "{label}: cargo test failed (wasm round-trip red cell — fix the emitter/generator \
                     or, deliberately, add to WASM_MATRIX_PROFILE_SKIP + a ledger reason)\nstdout:\n{}\nstderr:\n{}",
                    String::from_utf8_lossy(&test.stdout),
                    String::from_utf8_lossy(&test.stderr)
                )),
                (true, true) => resurfaced.push(label),
                _ => {} // (false,true)=green as expected; (true,false)=red as expected
            }
            // Free the per-cell crate dir as we go (keep the shared target) — near disk-full, and
            // 98 cells × 3 profiles of generated crates add up.
            let _ = std::fs::remove_dir_all(&out);
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these skip-listed wasm-matrix cells now round-trip — remove them from WASM_MATRIX_SKIP / \
         WASM_MATRIX_PROFILE_SKIP (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "wasm-matrix cells failed to round-trip:\n\n{}",
        failures.join("\n\n")
    );
}

/// The multifile-placement matrix ROUND-TRIP gate — the behavioural upgrade of
/// `multifile_matrix_compiles`, mirroring `wasm_matrix_roundtrips`. Same cell enumeration (the 46
/// `tests/matrix_multifile/<shape>__<mode>/` directories), but each cell is generated with
/// `--wasm=true --emit-tests=true` across `super::ALL_PROFILES` (default / preserve / json) and
/// `cargo test`ed rather than `cargo check`ed — this compiles AND RUNS the emitted
/// `cddl_generated_tests` / `cddl_generated_wasm_tests` modules, whose minted values construct the
/// CROSS-MODULE wiring (module `b`'s holder built from module `a`'s shape, e.g.
/// `Bholder::new(St::new(..))`; the wasm twin also byte-differentials against the fully-qualified
/// `cddl_lib::b::Bholder`/`cddl_lib::a::St` natives). A placement cell can type-check green under
/// the compile floor while cross-module (de)serialization misbehaves; that only surfaces when the
/// emitted assertions RUN, which is what this gate adds.
///
/// BOTH generated subcrates are `cargo test`ed, `rust/` then `wasm/`: the rust crate's
/// `#[cfg(test)]` module is NOT compiled when the crate is built merely as a dep of the wasm
/// crate, and the proven placement classes are rust-side (`feature_corpus_compiles` is the
/// both-crates precedent). A cell is red if EITHER crate's `cargo test` fails.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d (check.ts `full` tier) under the CI cost policy: 46 cells × 3
/// profiles × 2 `cargo test`s is materially heavier than the always-on compile floor. Run it with
/// `cargo test --bin cddl-codegen multifile_matrix_roundtrips -- --ignored`.
///
/// Two skip ledgers, both four-state (red+listed = expected; red+unlisted = fail — fix or,
/// deliberately, pin + cddl-matrix/ROADMAP.md ledger reason; green+listed = "resurfaced — remove
/// the pin"; green+unlisted = pass) with up-front stale-key guards: `MULTIFILE_ROUNDTRIP_SKIP`
/// (red in EVERY profile — the collrec compile-floor carries) and
/// `MULTIFILE_ROUNDTRIP_PROFILE_SKIP` (profile-specific reds). No rustc-error-code class assertion
/// here — the compile floor's `MULTIFILE_MATRIX_SKIP` pins each collrec cell's exact class.
///
/// Vacuity floor: the sweep counts each generated crate whose root `generated/mod.rs` carries a
/// minted test module (the `feature_corpus_compiles` counting pattern) and asserts a floor, so
/// "all green" can't silently mean "nothing was minted". Loud-skip contract (as
/// `wasm_matrix_roundtrips`): a cell shape minting no test surface emits no module and passes
/// with zero tests — legitimate (the emitter eprintln!s the skip; the compile floor still pins the
/// cell's ABI), and the floor bounds how much of that the sweep tolerates in aggregate.
/// `multifile_matrix_compiles` stays byte-for-byte untouched as the always-on compile floor; its
/// own scratch dir name (+ `acquire_scratch_lock`) lets this gate run beside it, and each per-cell
/// output dir is freed after its verdict (the disk-space pattern from
/// `feature_corpus_roundtrips_nondefault_profiles`).
#[test]
#[ignore]
fn multifile_matrix_roundtrips() {
    use std::str::FromStr;

    let dir = std::path::PathBuf::from_str("tests/matrix_multifile").unwrap();
    let mut cell_dirs: Vec<std::path::PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    cell_dirs.sort();
    assert!(
        !cell_dirs.is_empty(),
        "no multifile-matrix fixtures in {dir:?} (run `bun run project_multifile_matrix.ts`)"
    );

    // Up-front stale-key guards (mirror wasm_matrix_roundtrips): a listed stem/profile that no
    // longer exists would otherwise rot silently (its resurfaced guard only fires on a
    // (profile, cell) the sweep actually visits).
    let cell_stems: std::collections::BTreeSet<&str> = cell_dirs
        .iter()
        .map(|p| p.file_name().unwrap().to_str().unwrap())
        .collect();
    for (stem, _reason) in MULTIFILE_ROUNDTRIP_SKIP {
        assert!(
            cell_stems.contains(stem),
            "MULTIFILE_ROUNDTRIP_SKIP names cell `{stem}` that no longer exists in \
             tests/matrix_multifile — stale pin, remove or fix it"
        );
    }
    for (profile, stem, _reason) in MULTIFILE_ROUNDTRIP_PROFILE_SKIP {
        assert!(
            super::ALL_PROFILES.iter().any(|(name, _)| name == profile),
            "MULTIFILE_ROUNDTRIP_PROFILE_SKIP names unknown profile `{profile}` — stale pin, \
             remove or fix it"
        );
        assert!(
            cell_stems.contains(stem),
            "MULTIFILE_ROUNDTRIP_PROFILE_SKIP names cell `{stem}` that no longer exists in \
             tests/matrix_multifile — stale pin, remove or fix it"
        );
    }

    // Own scratch dir (distinct from multifile_matrix_compiles) + one shared target so
    // cbor_event/wasm-bindgen/the libtest harness build once, then each tiny crate tests
    // incrementally.
    let scratch_name = format!("cddl_codegen_multifile_rt_{:016x}", checkout_hash());
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT skip-listed — real findings
    let mut resurfaced = vec![]; // skip-listed cells that now pass — remove them
    // Vacuity-floor counters: (profile, cell) generations whose crate minted a test module.
    // Counted before the verdict (skip cells still mint), so the floor tracks EMITTER coverage,
    // not the pass rate.
    let mut minted_rust_modules = 0usize;
    let mut minted_wasm_modules = 0usize;
    for input in &cell_dirs {
        let stem = input.file_name().unwrap().to_str().unwrap();
        // Skipped in EVERY profile.
        let skipped_all = MULTIFILE_ROUNDTRIP_SKIP.iter().any(|(s, _)| *s == stem);
        for (profile, extra) in super::ALL_PROFILES {
            let label = format!("{stem}/{profile}");
            // Skipped in EVERY profile, or in THIS specific profile.
            let skipped = skipped_all
                || MULTIFILE_ROUNDTRIP_PROFILE_SKIP
                    .iter()
                    .any(|(p, s, _)| p == profile && s == &stem);
            let out = root.join(format!("{stem}__{profile}"));
            let gen_out = tool_cmd("cargo")
                .args(["run", "--"])
                .arg(format!("--input={}", input.to_str().unwrap()))
                .arg(format!("--output={}", out.to_str().unwrap()))
                .arg("--wasm=true")
                .arg("--emit-tests=true")
                .args(*extra)
                .output()
                .unwrap();
            if !gen_out.status.success() {
                if !skipped {
                    failures.push(format!(
                        "{label}: generation failed\n{}",
                        String::from_utf8_lossy(&gen_out.stderr)
                    ));
                }
                let _ = std::fs::remove_dir_all(&out);
                continue;
            }
            if std::fs::read_to_string(out.join("rust/src/generated/mod.rs"))
                .unwrap_or_default()
                .contains("mod cddl_generated_tests")
            {
                minted_rust_modules += 1;
            }
            if std::fs::read_to_string(out.join("wasm/src/generated/mod.rs"))
                .unwrap_or_default()
                .contains("mod cddl_generated_wasm_tests")
            {
                minted_wasm_modules += 1;
            }
            let wasm_dir = out.join("wasm");
            if !wasm_dir.exists() {
                // Every cell's module `b` is a composite record, so a wasm crate is always
                // expected — a missing one silently de-gates the cell (mirror the compile floor).
                if skipped {
                    resurfaced.push(format!("{label} (emits no wasm crate)"));
                } else {
                    failures.push(format!(
                        "{label}: generated no wasm crate (expected a wasm wrapper for every cell \
                         — the cell is no longer being round-trip-gated)"
                    ));
                }
                let _ = std::fs::remove_dir_all(&out);
                continue;
            }
            // Execute BOTH generated subcrates, rust then wasm (see the gate doc); first failure
            // is the cell's red.
            let mut cell_red: Option<(&str, std::process::Output)> = None;
            for crate_sub in ["rust", "wasm"] {
                let test = tool_cmd("cargo")
                    .arg("test")
                    .current_dir(out.join(crate_sub))
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                if !test.status.success() {
                    cell_red = Some((crate_sub, test));
                    break;
                }
            }
            match (skipped, cell_red) {
                (false, Some((crate_sub, test))) => failures.push(format!(
                    "{label} ({crate_sub}): cargo test failed (multifile round-trip red cell — \
                     fix the emitter/generator or, deliberately, add to MULTIFILE_ROUNDTRIP_SKIP \
                     / MULTIFILE_ROUNDTRIP_PROFILE_SKIP + a cddl-matrix/ROADMAP.md ledger \
                     reason)\nstdout:\n{}\nstderr:\n{}",
                    String::from_utf8_lossy(&test.stdout),
                    String::from_utf8_lossy(&test.stderr)
                )),
                (true, None) => resurfaced.push(label),
                _ => {} // (false, None) = green as expected; (true, Some(_)) = red as expected
            }
            // Free the per-cell crate dir as we go (keep the shared target) — 46 cells × 3
            // profiles of generated crates add up.
            let _ = std::fs::remove_dir_all(&out);
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    eprintln!(
        "multifile_matrix_roundtrips: minted test modules across the sweep — rust {minted_rust_modules}, wasm {minted_wasm_modules}"
    );
    // Vacuity floor, calibrated from the observed green runs: all 144 (48 cells × 3 profiles)
    // generations minted BOTH modules (every cell's module `b` holder is a mintable composite
    // record). A big drop means the emitter's multifile coverage silently shrank, not that the
    // matrix got simpler.
    assert!(
        minted_rust_modules >= 136 && minted_wasm_modules >= 136,
        "only {minted_rust_modules} rust / {minted_wasm_modules} wasm (profile, cell) generations \
         minted a generated-test module (expected >= 136 each of 144) — emit_tests multifile \
         coverage shrank; the sweep's green is going vacuous"
    );
    assert!(
        resurfaced.is_empty(),
        "these skip-listed multifile-matrix cells now round-trip — remove them from \
         MULTIFILE_ROUNDTRIP_SKIP / MULTIFILE_ROUNDTRIP_PROFILE_SKIP (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "multifile-matrix cells failed to round-trip:\n\n{}",
        failures.join("\n\n")
    );
}

/// Compile gate for `--wasm-list-macro` / `--wasm-conversions-macro`. The emitted code references
/// a user-supplied macro (`impl_wasm_list!` invocations replace each list wrapper's inline
/// struct/accessor/conversion block), so it can't compile standalone and was previously
/// snapshot-only — a malformed invocation emission (wrong arg order, wrong `needs_into`/`is_copy`
/// values, an unreachable combination) is a semantic fact a source snapshot can't judge. This
/// wires in `tests/wasm-macro-crate` — real macro definitions matching the emitted signature,
/// written so each of those wrong-emission classes fails to compile (see its README) — the same
/// way extern-deps wires `tests/extern-dep-crate`, and `cargo check`s the generated wasm crate
/// under both flag combinations the snapshot suite pins (`snapshot_tests::wasm_list_macro`).
#[test]
fn wasm_list_macro_compiles() {
    use std::str::FromStr;
    let cases: &[(&str, &[&str])] = &[
        (
            "list_macro",
            &["--wasm-list-macro=wasm_macro_crate::impl_wasm_list"][..],
        ),
        // combined: list wrappers use impl_wasm_list! (which emits its own conversions) while
        // non-list wrappers use impl_wasm_conversions! — compile-proves the two don't double-emit.
        (
            "list_and_conversions_macro",
            &[
                "--wasm-list-macro=wasm_macro_crate::impl_wasm_list",
                "--wasm-conversions-macro=wasm_macro_crate::impl_wasm_conversions",
            ][..],
        ),
    ];
    let test_path = std::path::PathBuf::from_str("tests/wasm-list-macro").unwrap();
    // own scratch target (shared across the two cases) so parallel tests don't collide
    let target_dir = std::env::temp_dir().join(format!(
        "cddl_codegen_wasm_list_macro_{:016x}",
        checkout_hash()
    ));
    for (label, options) in cases {
        let out = test_path.join(format!("export_{label}"));
        let _ = std::fs::remove_dir_all(&out);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!(
                "--input={}",
                test_path.join("input.cddl").to_str().unwrap()
            ))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .args(*options)
            .output()
            .unwrap();
        assert!(
            gen_out.status.success(),
            "{label}: generation failed\n{}",
            String::from_utf8_lossy(&gen_out.stderr)
        );
        // vacuous-pass guard: the gate only gates the macro path if the flag actually collapsed
        // the list wrappers into invocations (5 at landing; see the fixture's header comment).
        let lib = std::fs::read_to_string(out.join("wasm/src/generated/mod.rs")).unwrap();
        let n_invocations = lib.matches("impl_wasm_list!(").count();
        assert!(
            n_invocations >= 5,
            "{label}: only {n_invocations} impl_wasm_list! invocations emitted (expected >= 5) — \
             the flag stopped collapsing list wrappers, so this gate no longer gates the macro path"
        );
        // symmetric vacuous-pass guard for the conversions half: the combined case's docstring claims
        // BOTH flag combinations are compile-gated, but only the list count was asserted — a
        // regression of --wasm-conversions-macro back to inline From/AsRef would still compile green.
        // The fixture emits 2 conversions invocations (list=5, conv=2).
        if options
            .iter()
            .any(|o| o.contains("--wasm-conversions-macro="))
        {
            let n_conv = lib.matches("impl_wasm_conversions!(").count();
            assert!(
                n_conv >= 2,
                "{label}: only {n_conv} impl_wasm_conversions! invocations emitted (expected >= 2) — \
                 the flag stopped collapsing non-list wrappers, so the conversions-macro path is ungated"
            );
        }
        // wire in the real macro definitions the emitted invocations reference
        let mut cargo_toml = std::fs::OpenOptions::new()
            .append(true)
            .open(out.join("wasm/Cargo.toml"))
            .unwrap();
        cargo_toml
            .write_all(b"wasm-macro-crate = { path = \"../../../wasm-macro-crate\" }\n")
            .unwrap();
        std::mem::drop(cargo_toml);
        let check = tool_cmd("cargo")
            .arg("check")
            .current_dir(out.join("wasm"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        assert!(
            check.status.success(),
            "{label}: cargo check failed\n{}",
            String::from_utf8_lossy(&check.stderr)
        );
    }
}

/// Smoke gate for documented flag *values* that no other test or profile exercises (closed the
/// once-open "five documented flag values with zero coverage" gap for the rust-side four). Each selects a whole alternative emit path: `--annotate-fields=false` (a
/// different deserialization / error-emission mode — 13+ branch sites in generation.rs),
/// `--to-from-bytes-methods=false` (drops the `to_bytes`/`from_bytes` API), and
/// `--binary-wrappers=true` (byte strings as new rust types). Before this, a generation regression
/// under any of them compiled nothing anywhere. Cheapest acceptance per the roadmap: generate a
/// rich, extern-free, custom-serialize-free input (tests/canonical — so no default-flag-assuming
/// appends are needed) under each value and `cargo check` the rust crate. Rust-side only
/// (`--wasm=false`).
///
/// The fifth documented value — `--canonical-form=true` *without* `--preserve-encodings` — turned
/// out to emit a crate that doesn't compile (the canonical toggle rides on preserve's serialize
/// signatures, leaving an unbound `force_canonical`). Per the roadmap's own resolution ("a CLI
/// rejection or a ledgered gap"), that combination is now rejected up front (`api::with_types`);
/// `flag_value_rejects_canonical_without_preserve` pins the rejection. The remaining fifth value,
/// `--wasm-cbor-json-api-macro`, is a wasm+macro concern gated by
/// `wasm_cbor_json_api_macro_compiles` below.
#[test]
fn flag_value_smoke() {
    use std::str::FromStr;
    let input = std::path::PathBuf::from_str("tests/canonical/input.cddl").unwrap();
    let cases: &[(&str, &[&str])] = &[
        ("annotate_fields_false", &["--annotate-fields=false"][..]),
        (
            "to_from_bytes_methods_false",
            &["--to-from-bytes-methods=false"][..],
        ),
        ("binary_wrappers_true", &["--binary-wrappers=true"][..]),
    ];
    // shared scratch + target under temp_dir (per-checkout, like the other generate+check gates)
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_flag_smoke_{:016x}", checkout_hash()));
    let target_dir = scratch.join("target");
    let mut failures = Vec::new();
    for (label, options) in cases {
        let out = scratch.join(label);
        let _ = std::fs::remove_dir_all(&out);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=false")
            .args(*options)
            .output()
            .unwrap();
        if !gen_out.status.success() {
            failures.push(format!(
                "{label}: generation failed\n{}",
                String::from_utf8_lossy(&gen_out.stderr)
            ));
            continue;
        }
        let check = tool_cmd("cargo")
            .arg("check")
            .current_dir(out.join("rust"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        if !check.status.success() {
            failures.push(format!(
                "{label}: cargo check failed\n{}",
                String::from_utf8_lossy(&check.stderr)
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// Clippy over generated crates: the emitted deserialize/serialize source must be `clippy::all`-clean
/// (no `clippy::no_effect` degenerate `();` statements, no other default clippy lints), per emission
/// profile. Snapshots pin that the emitted bytes don't *change*, not that they're *idiomatic* — a
/// generator regression that mints lint-worthy code (a standalone `();`, a needless clone, a
/// pointless match) is invisible to them and to the round-trip suites (the code still compiles and
/// works), but it degrades every consumer's `cargo clippy`. This runs `cargo clippy -- -D
/// clippy::all` over a rich, extern-free fixture (`tests/canonical/input.cddl`, which
/// `flag_value_smoke` already relies on for exactly its extern-freedom + breadth) under two
/// representative profiles: default flags, and `--preserve-encodings=true --canonical-form=true`.
///
/// Deny only `clippy::all` plus a curated rustc style-lint set, NOT `-D warnings`: generated code
/// legitimately over-imports and rustc's `unused_imports` / `unused_variables` must stay warnings
/// here (see `tool_cmd`'s doc comment; `tool_cmd` also strips the CI-injected `RUSTFLAGS=-D
/// warnings`). The rustc denies cover emitted source-shape regressions `clippy::all` does not:
/// redundant grouping (`unused_parens`, `unused_braces`) and useless heap allocation
/// (`unused_allocation`), each proven green for both profiles when this axis was added.
/// Generate-into-own-temp-dir shape mirrors `flag_value_smoke` so this gate can't race the fixtures'
/// own `tests/<dir>/export` outputs. `--wasm=true` generates both the rust and wasm crates; the rust
/// output differs from `--wasm=false` only by an extra wasm-support type alias in this fixture, so a
/// single generation run covers the prior rust surface and lets the gate lint the wasm binding crate
/// too. Tier: check.ts `local` (a plain non-ignored test) — measured warm wall-clock stays under the
/// ~90s plain-`#[test]` threshold. NEVER promote to `fast`/CI.
#[test]
fn generated_code_clippy_clean() {
    use std::str::FromStr;
    if !tool_exists("cargo") {
        return;
    }
    const RUSTC_STYLE_DENIES: &[&str] = &[
        "-D",
        "unused_parens",
        "-D",
        "unused_braces",
        "-D",
        "unused_allocation",
    ];
    const PERMANENT_ALLOWS: &[&str] = &["-A", "clippy::disallowed_names"];
    let input = std::path::PathBuf::from_str("tests/canonical/input.cddl").unwrap();
    let cases: &[(&str, &[&str])] = &[
        ("default", &[][..]),
        (
            "preserve_canonical",
            &["--preserve-encodings=true", "--canonical-form=true"][..],
        ),
    ];
    // shared scratch + target under temp_dir (per-checkout), like `flag_value_smoke` and the other
    // generate+check gates — never the committed `tests/<dir>/export` dirs the fixtures reuse.
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_clippy_gate_{:016x}", checkout_hash()));
    let target_dir = scratch.join("target");
    let mut failures = Vec::new();
    for (label, options) in cases {
        let out = scratch.join(label);
        let _ = std::fs::remove_dir_all(&out);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .args(*options)
            .output()
            .unwrap();
        if !gen_out.status.success() {
            failures.push(format!(
                "{label}: generation failed\n{}",
                String::from_utf8_lossy(&gen_out.stderr)
            ));
            continue;
        }
        let clippy = tool_cmd("cargo")
            .arg("clippy")
            .current_dir(out.join("rust"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .args(["--", "-D", "clippy::all"])
            .args(RUSTC_STYLE_DENIES)
            // Input-dependent, permanent allow: the fixture's own `foo`/`bar` rule names become
            // generated parameter names, which clippy::disallowed_names flags — not a generator
            // defect. This is the rust crate's ONLY allow: the emission-quality burn-down list is
            // fully retired, so every other clippy::all lint class (and any NEW one a generator
            // regression might mint) is hard-red on both profiles.
            .args(PERMANENT_ALLOWS)
            .output()
            .unwrap();
        if !clippy.status.success() {
            failures.push(format!(
                "{label}: `cargo clippy -- -D clippy::all` failed on the generated rust crate\n\
                 --- stdout ---\n{}\n--- stderr ---\n{}",
                String::from_utf8_lossy(&clippy.stdout),
                String::from_utf8_lossy(&clippy.stderr)
            ));
        }
        let wasm_clippy = tool_cmd("cargo")
            .arg("clippy")
            .current_dir(out.join("wasm"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .args(["--", "-D", "clippy::all"])
            .args(RUSTC_STYLE_DENIES)
            .args(PERMANENT_ALLOWS)
            // The wasm crate's emission-quality burn-down list is fully retired too. New
            // clippy::all lint classes are hard-red on both profiles and both generated crates.
            .output()
            .unwrap();
        if !wasm_clippy.status.success() {
            failures.push(format!(
                "{label}: `cargo clippy -- -D clippy::all` failed on the generated wasm crate\n\
                 --- stdout ---\n{}\n--- stderr ---\n{}",
                String::from_utf8_lossy(&wasm_clippy.stdout),
                String::from_utf8_lossy(&wasm_clippy.stderr)
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// `--canonical-form=true` without `--preserve-encodings` must be rejected (it otherwise emits a
/// non-compiling crate — see `api::with_types`). Pins the rejection *and* its message so the guard
/// can't silently become a no-op, and confirms the same input with both flags is accepted — so the
/// rejection is specific to the missing `--preserve-encodings`, not the input.
#[test]
fn flag_value_rejects_canonical_without_preserve() {
    let mut cli = crate::cli::Cli {
        input: std::path::PathBuf::from("tests/canonical/input.cddl"),
        output: std::path::PathBuf::from("unused"),
        canonical_form: true,
        preserve_encodings: false,
        ..Default::default()
    };
    let err = crate::api::with_types(&cli, |_, _| ())
        .expect_err("--canonical-form without --preserve-encodings should be rejected");
    let msg = err.to_string();
    assert!(
        msg.contains("--canonical-form") && msg.contains("--preserve-encodings"),
        "rejection message should name both flags, got: {msg}"
    );
    // baseline: with preserve-encodings the same input is accepted (so it's the combination, not
    // the input, that's rejected)
    cli.preserve_encodings = true;
    assert!(
        crate::api::with_types(&cli, |_, _| ()).is_ok(),
        "--canonical-form with --preserve-encodings should be accepted"
    );
}

/// The manifest merge contract on real disk (the `cargo_manifest` changeset applied through
/// `export`): a first run scaffolds `rust/Cargo.toml`; a user then hand-edits it (bumps the seeded
/// `version`, adds their own dep + comments, tampers the version stamp); a regeneration must
/// **preserve** the untouched user content and the `SeedOnce` version, **restore** the tool-owned
/// stamp, and a third regeneration must be a byte-identical fixed point. Driven in-process via
/// `generate_to_disk` (no subprocess/compile — this exercises the disk-merge path, not codegen
/// correctness, which the compile gates cover). `--wasm=false` keeps it to the single rust manifest.
#[test]
fn cargo_manifest_disk_round_trip() {
    use clap::Parser;
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_manifest_rt_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");

    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]);
    let manifest = out.join("rust/Cargo.toml");

    // First run: scaffolds the manifest with tool-owned keys + the write-only version stamp.
    crate::api::generate_to_disk(&cli).unwrap();
    let first = std::fs::read_to_string(&manifest).unwrap();
    let tool_version = env!("CARGO_PKG_VERSION");
    assert!(
        first.contains("cbor_event"),
        "tool-owned dep missing:\n{first}"
    );
    assert!(
        first.contains(&format!("generated-with = \"{tool_version}\"")),
        "version stamp missing:\n{first}"
    );

    // Hand-edit: bump the seeded version, tamper the stamp, add a user dep with an inline comment,
    // prepend a top-of-file comment, and reshape the tool-owned `cbor_event` dep into a table that
    // adds an `optional = false` field with a still-compatible pin. All but the stamp must survive
    // (the reshape exercises the field-level dep merge on a real disk round trip); the stamp must be
    // restored.
    assert!(
        first.contains("cbor_event = \"2.4.0\""),
        "expected the plain-string cbor_event dep to reshape:\n{first}"
    );
    let edited = format!(
        "# hand-written top comment\n{}\nanyhow = \"1\" # user pin\n",
        first
            .replace("version = \"0.1.0\"", "version = \"9.9.9\"")
            .replace(
                "cbor_event = \"2.4.0\"",
                "cbor_event = { version = \"2.4.0\", optional = false }",
            )
            .replace(
                &format!("generated-with = \"{tool_version}\""),
                "generated-with = \"0.0.0-tampered\"",
            )
    );
    std::fs::write(&manifest, &edited).unwrap();

    // Second run: merge onto the edited manifest.
    crate::api::generate_to_disk(&cli).unwrap();
    let second = std::fs::read_to_string(&manifest).unwrap();
    assert!(
        second.contains("version = \"9.9.9\""),
        "SeedOnce version must survive regen:\n{second}"
    );
    assert!(
        second.contains("anyhow = \"1\""),
        "user-added dep must survive regen:\n{second}"
    );
    assert!(
        second.contains("# hand-written top comment") && second.contains("# user pin"),
        "user comments must survive regen:\n{second}"
    );
    assert!(
        second.contains(&format!("generated-with = \"{tool_version}\"")),
        "tool-owned stamp must be restored:\n{second}"
    );
    assert!(
        !second.contains("0.0.0-tampered"),
        "tampered stamp must be overwritten:\n{second}"
    );
    assert!(
        second.contains("cbor_event"),
        "tool-owned dep must persist:\n{second}"
    );
    // the user's reshape (optional field + inline-table shape) survives the field-level dep merge,
    // and the still-compatible pin is kept rather than flattened back to a bare string.
    assert!(
        second.contains("optional = false"),
        "user-added dep field must survive the dep merge:\n{second}"
    );
    assert!(
        second.contains("version = \"2.4.0\""),
        "compatible pin must be kept on the merged dep:\n{second}"
    );

    // Third run: byte-identical fixed point.
    crate::api::generate_to_disk(&cli).unwrap();
    let third = std::fs::read_to_string(&manifest).unwrap();
    assert_eq!(
        second, third,
        "regeneration must reach a byte-identical fixed point"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// The `lib.rs` seed-once contract (the thin-root counterpart of the manifest changeset): a first
/// export seeds a thin root (`mod generated; pub use generated::*;`) and emits every generated file
/// under `rust/src/generated/**`; the root is thereafter user-owned and NEVER rewritten — an
/// existence check only, mirroring `ManifestOp::SeedOnce`. Deleting a generated file and hand-editing
/// the root, then re-exporting, must leave the root byte-identical and restore the subtree; a third
/// run is a byte-identical fixed point. In-process via `generate_to_disk` (no compile — the compile +
/// behavioral coverage of the split lives in the `extern_deps` fixture and the snapshot corpus).
#[test]
fn thin_root_seed_once() {
    use clap::Parser;
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_thin_seed_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=true",
    ]);
    let lib_rs = out.join("rust/src/lib.rs");
    let generated_mod = out.join("rust/src/generated/mod.rs");

    // First run: seeds the thin root and emits the generated subtree.
    crate::api::generate_to_disk(&cli).unwrap();
    let seeded = std::fs::read_to_string(&lib_rs).unwrap();
    assert!(
        seeded.contains("mod generated;"),
        "seeded root must declare the generated module:\n{seeded}"
    );
    assert!(
        seeded.contains("pub use generated::*;"),
        "seeded root must glob-re-export the generated module:\n{seeded}"
    );
    assert!(
        !seeded.contains("struct Foo"),
        "generated type definitions must live under generated/, not the thin root:\n{seeded}"
    );
    assert!(
        generated_mod.exists(),
        "generated/mod.rs must exist after the first export"
    );

    // The wasm crate root gets the identical seed-once split.
    let wasm_seeded = std::fs::read_to_string(out.join("wasm/src/lib.rs")).unwrap();
    assert!(
        wasm_seeded.contains("mod generated;") && wasm_seeded.contains("pub use generated::*;"),
        "the wasm crate root must be seeded with the same thin root:\n{wasm_seeded}"
    );
    assert!(
        out.join("wasm/src/generated/mod.rs").exists(),
        "wasm generated/mod.rs must exist after the first export"
    );

    // The root is now user-owned: hand-edit it and delete a generated file.
    let user_edited = format!("{seeded}\npub mod utils;\n");
    std::fs::write(&lib_rs, &user_edited).unwrap();
    std::fs::remove_file(&generated_mod).unwrap();

    // Second run: existence-only — the root is preserved verbatim, the subtree restored.
    crate::api::generate_to_disk(&cli).unwrap();
    let after = std::fs::read_to_string(&lib_rs).unwrap();
    assert_eq!(
        user_edited, after,
        "seed-once root must survive regeneration byte-for-byte"
    );
    assert!(
        generated_mod.exists(),
        "the generated subtree must be restored on re-export"
    );

    // Third run: byte-identical fixed point.
    crate::api::generate_to_disk(&cli).unwrap();
    let third = std::fs::read_to_string(&lib_rs).unwrap();
    assert_eq!(
        after, third,
        "seed-once root must reach a byte-identical fixed point"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// The wiring-survival regression (the CML cip25 clobber, mechanized): a hand-wired thin root that
/// wires a user-supplied module (`pub mod utils; pub use utils::Helper;` — the extern-type shape)
/// must survive regeneration untouched, and the user module must be left in place while the generated
/// subtree is regenerated beside it. This is the failure the whole feature exists to prevent. Beyond
/// byte survival, the hand-wired crate must actually COMPILE after the regenerated-over run: the
/// design doc's contract is that hand wiring referencing generated types through the crate-root glob
/// (`utils.rs`'s fn takes and returns a generated `Foo`) keeps resolving. Guarded on `cargo` so the
/// byte-level assertions still run on a toolchain-less box.
#[test]
fn thin_root_wiring_survives() {
    use clap::Parser;
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_thin_wire_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]);
    let lib_rs = out.join("rust/src/lib.rs");
    let utils_rs = out.join("rust/src/utils.rs");

    // First run seeds the thin root; the user then hand-wires an extern-type module into it. The
    // module reaches generated types through the crate-root glob (`crate::Foo`) — the exact shape an
    // extern-type consumer (CML's cip25) hand-maintains.
    crate::api::generate_to_disk(&cli).unwrap();
    let hand_wired =
        "// hand-wired root\nmod generated;\npub use generated::*;\npub mod utils;\npub use utils::Helper;\n"
            .to_owned();
    std::fs::write(&lib_rs, &hand_wired).unwrap();
    std::fs::write(
        &utils_rs,
        "use crate::Foo;\npub struct Helper;\npub fn round_trip(f: Foo) -> Foo {\n    f\n}\n",
    )
    .unwrap();

    // Regenerate over the same directory: the hand wiring must survive.
    crate::api::generate_to_disk(&cli).unwrap();
    let after = std::fs::read_to_string(&lib_rs).unwrap();
    assert_eq!(
        hand_wired, after,
        "hand wiring in the thin root must survive regeneration byte-for-byte"
    );
    assert_eq!(
        std::fs::read_to_string(&utils_rs).unwrap(),
        "use crate::Foo;\npub struct Helper;\npub fn round_trip(f: Foo) -> Foo {\n    f\n}\n",
        "the user-supplied module must be left untouched"
    );
    assert!(
        out.join("rust/src/generated/mod.rs").exists(),
        "the generated subtree must be regenerated beside the user module"
    );

    // A further regeneration is still a fixed point for the user-owned root.
    crate::api::generate_to_disk(&cli).unwrap();
    assert_eq!(
        hand_wired,
        std::fs::read_to_string(&lib_rs).unwrap(),
        "thin root must remain a byte-identical fixed point across runs"
    );

    // The regenerated-over crate must compile: the hand-wired root + `utils.rs` referencing a
    // generated type through the glob resolve against the freshly clobbered `src/generated/**`.
    if tool_exists("cargo") {
        let target_dir = scratch.join("target");
        let build = tool_cmd("cargo")
            .arg("build")
            .current_dir(out.join("rust"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        assert!(
            build.status.success(),
            "hand-wired crate must compile after the regenerated-over export\n{}\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
    }

    let _ = std::fs::remove_dir_all(&scratch);
}

/// In-crate extern types must resolve under the thin-root split (the CML cip25 scenario the feature
/// exists for). Given `my_ext = _CDDL_CODEGEN_EXTERN_TYPE_` used by a generated `wrapper`, generated
/// code refers to the extern by its bare ident inside `generated/mod.rs` (and via `use super::*;`
/// inside `generated/serialization.rs`). Pre-split those names resolved because the user's
/// `pub use utils::MyExt;` sat in the SAME root scope (the monolithic `lib.rs`); post-split the user
/// can only edit the thin `lib.rs`, and a parent-module name is NOT visible inside `mod generated`, so
/// the crate failed to build with E0433 "cannot find type `MyExt`". The tool now emits
/// `pub use crate::MyExt;` into the declaring scope's generated module, and the documented contract is:
/// define the extern in a hand-written module and RE-EXPORT it at the crate root — the identical action
/// pre-split consumers already took. This gate hand-wires exactly that shape and asserts the crate both
/// keeps the seeded `lib.rs` byte-identical AND compiles. `cargo`-guarded so the wiring assertions still
/// run on a toolchain-less box. Covers the default and `--preserve-encodings` profiles (preserve reshapes
/// the deserializer's imports/signatures, but the extern's own impls are unchanged — the extern carries
/// no encoding metadata — so the same `utils.rs` serves both).
///
/// A third profile covers the WASM crate, which has the identical latent shape: generated wasm code
/// names the extern's hand-written `#[wasm_bindgen]` WRAPPER bare inside `wasm/src/generated/**`, but
/// the wrapper is user-authored (a real consumer, e.g. CML cip25, defines it in a hand-written wasm
/// module). Under the split those references can't see it — same E0433. The tool emits the same
/// `pub use crate::MyExt;` glue into the wasm generated module; the contract is to define the wrapper in
/// a hand-written wasm-crate module and RE-EXPORT it at the wasm crate root. The wasm crate builds as a
/// host crate (it depends on the rust crate as a path dep, so the rust side is wired too), which the
/// harness already `cargo build`s.
#[test]
fn thin_root_in_crate_extern_type_compiles() {
    use clap::Parser;

    fn run(profile: &str, extra_flags: &[&str]) {
        let scratch = std::env::temp_dir().join(format!(
            "cddl_codegen_thin_extern_{profile}_{:016x}",
            checkout_hash()
        ));
        let _ = std::fs::remove_dir_all(&scratch);
        std::fs::create_dir_all(&scratch).unwrap();
        let input = scratch.join("input.cddl");
        std::fs::write(
            &input,
            "my_ext = _CDDL_CODEGEN_EXTERN_TYPE_\nwrapper = [id: uint, ext: my_ext]\n",
        )
        .unwrap();
        let out = scratch.join("crate");
        let mut args = vec![
            "cddl-codegen",
            "--input",
            input.to_str().unwrap(),
            "--output",
            out.to_str().unwrap(),
            "--wasm=false",
        ];
        args.extend_from_slice(extra_flags);
        let cli = crate::cli::Cli::parse_from(args);
        let lib_rs = out.join("rust/src/lib.rs");
        let utils_rs = out.join("rust/src/utils.rs");

        // First export seeds the thin root; capture it to assert seed-once leaves it byte-identical.
        crate::api::generate_to_disk(&cli).unwrap();
        let seeded = std::fs::read_to_string(&lib_rs).unwrap();

        // Hand-wire the extern exactly as the docs prescribe: a user module that DEFINES the extern, and
        // a crate-root re-export. This is the only edit a post-split consumer can make (the thin root is
        // user-owned; `generated/**` is clobbered every run).
        let hand_wired = format!("{seeded}\npub mod utils;\npub use utils::MyExt;\n");
        std::fs::write(&lib_rs, &hand_wired).unwrap();
        // The extern's Rust definition. Default profile: `Serialize` is `cbor_event::se::Serialize`,
        // `Deserialize` is `crate::serialization::Deserialize`, error type `crate::error::DeserializeError`
        // — all reachable from a crate-root module through the seeded `pub use generated::*;` glob.
        std::fs::write(
            &utils_rs,
            "use crate::error::DeserializeError;\n\
             use crate::serialization::Deserialize;\n\
             \n\
             #[derive(Clone, Debug)]\n\
             pub struct MyExt(pub u64);\n\
             \n\
             impl cbor_event::se::Serialize for MyExt {\n\
             \x20   fn serialize<'se, W: std::io::Write>(\n\
             \x20       &self,\n\
             \x20       serializer: &'se mut cbor_event::se::Serializer<W>,\n\
             \x20   ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {\n\
             \x20       serializer.write_unsigned_integer(self.0)\n\
             \x20   }\n\
             }\n\
             \n\
             impl Deserialize for MyExt {\n\
             \x20   fn deserialize<R: std::io::BufRead + std::io::Seek>(\n\
             \x20       raw: &mut cbor_event::de::Deserializer<R>,\n\
             \x20   ) -> Result<Self, DeserializeError> {\n\
             \x20       Ok(Self(raw.unsigned_integer()?))\n\
             \x20   }\n\
             }\n",
        )
        .unwrap();

        // Regenerate over the same directory: seed-once must leave the hand-wired root byte-identical
        // (the generated subtree is re-clobbered beside it, carrying the fresh `pub use crate::MyExt;`).
        crate::api::generate_to_disk(&cli).unwrap();
        assert_eq!(
            hand_wired,
            std::fs::read_to_string(&lib_rs).unwrap(),
            "[{profile}] the hand-wired thin root must survive regeneration byte-for-byte"
        );

        // The generator must emit the re-export glue into the declaring (root) scope's module so the
        // bare `MyExt` references in `generated/mod.rs` and (via `use super::*;`) the serializer resolve.
        let generated_mod = std::fs::read_to_string(out.join("rust/src/generated/mod.rs")).unwrap();
        assert!(
            generated_mod.contains("pub use crate::MyExt;"),
            "[{profile}] generated/mod.rs must re-export the in-crate extern from the crate root:\n{generated_mod}"
        );

        if tool_exists("cargo") {
            let target_dir = scratch.join("target");
            let build = tool_cmd("cargo")
                .arg("build")
                .current_dir(out.join("rust"))
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            assert!(
                build.status.success(),
                "[{profile}] hand-wired in-crate extern crate must compile\n{}\n{}",
                String::from_utf8_lossy(&build.stdout),
                String::from_utf8_lossy(&build.stderr)
            );
        }

        let _ = std::fs::remove_dir_all(&scratch);
    }

    // The WASM crate carries the identical latent shape — generated wasm code names the extern's
    // hand-written `#[wasm_bindgen]` WRAPPER bare inside `wasm/src/generated/**`, invisible there under
    // the split. Hand-wire the wasm wrapper (define + re-export at the wasm crate root) exactly as the
    // docs prescribe, plus the rust-side wiring (the wasm crate depends on the rust crate), and assert
    // both thin roots survive regeneration byte-for-byte AND the wasm crate compiles as a host crate.
    fn run_wasm() {
        let scratch = std::env::temp_dir().join(format!(
            "cddl_codegen_thin_extern_wasm_{:016x}",
            checkout_hash()
        ));
        let _ = std::fs::remove_dir_all(&scratch);
        std::fs::create_dir_all(&scratch).unwrap();
        let input = scratch.join("input.cddl");
        std::fs::write(
            &input,
            "my_ext = _CDDL_CODEGEN_EXTERN_TYPE_\nwrapper = [id: uint, ext: my_ext]\n",
        )
        .unwrap();
        let out = scratch.join("crate");
        let cli = crate::cli::Cli::parse_from([
            "cddl-codegen",
            "--input",
            input.to_str().unwrap(),
            "--output",
            out.to_str().unwrap(),
            "--wasm=true",
        ]);
        let rust_lib_rs = out.join("rust/src/lib.rs");
        let wasm_lib_rs = out.join("wasm/src/lib.rs");

        // First export seeds both thin roots; capture them to assert seed-once leaves them byte-identical.
        crate::api::generate_to_disk(&cli).unwrap();
        let rust_seeded = std::fs::read_to_string(&rust_lib_rs).unwrap();
        let wasm_seeded = std::fs::read_to_string(&wasm_lib_rs).unwrap();

        // Rust side (the wasm crate's path dep): the same WI-6 wiring — define + re-export the native
        // extern at the crate root so the bare `generated/**` references resolve.
        let rust_hand_wired = format!("{rust_seeded}\npub mod utils;\npub use utils::MyExt;\n");
        std::fs::write(&rust_lib_rs, &rust_hand_wired).unwrap();
        std::fs::write(
            out.join("rust/src/utils.rs"),
            "use crate::error::DeserializeError;\n\
             use crate::serialization::Deserialize;\n\
             \n\
             #[derive(Clone, Debug)]\n\
             pub struct MyExt(pub u64);\n\
             \n\
             impl cbor_event::se::Serialize for MyExt {\n\
             \x20   fn serialize<'se, W: std::io::Write>(\n\
             \x20       &self,\n\
             \x20       serializer: &'se mut cbor_event::se::Serializer<W>,\n\
             \x20   ) -> cbor_event::Result<&'se mut cbor_event::se::Serializer<W>> {\n\
             \x20       serializer.write_unsigned_integer(self.0)\n\
             \x20   }\n\
             }\n\
             \n\
             impl Deserialize for MyExt {\n\
             \x20   fn deserialize<R: std::io::BufRead + std::io::Seek>(\n\
             \x20       raw: &mut cbor_event::de::Deserializer<R>,\n\
             \x20   ) -> Result<Self, DeserializeError> {\n\
             \x20       Ok(Self(raw.unsigned_integer()?))\n\
             \x20   }\n\
             }\n",
        )
        .unwrap();

        // WASM side: define the `#[wasm_bindgen]` wrapper around the rust type (reached as `cddl_lib::MyExt`,
        // the rust crate's path-dep name) and re-export it at the wasm crate root. The generated wasm code
        // converts between wrapper and native via `From`, so supply both directions (+ `AsRef`, matching the
        // real consumer's wrapper shape).
        let wasm_hand_wired = format!("{wasm_seeded}\npub mod utils;\npub use utils::MyExt;\n");
        std::fs::write(&wasm_lib_rs, &wasm_hand_wired).unwrap();
        std::fs::write(
            out.join("wasm/src/utils.rs"),
            "use wasm_bindgen::prelude::wasm_bindgen;\n\
             \n\
             #[wasm_bindgen]\n\
             #[derive(Clone, Debug)]\n\
             pub struct MyExt(cddl_lib::MyExt);\n\
             \n\
             impl From<cddl_lib::MyExt> for MyExt {\n\
             \x20   fn from(native: cddl_lib::MyExt) -> Self {\n\
             \x20       Self(native)\n\
             \x20   }\n\
             }\n\
             \n\
             impl From<MyExt> for cddl_lib::MyExt {\n\
             \x20   fn from(wasm: MyExt) -> Self {\n\
             \x20       wasm.0\n\
             \x20   }\n\
             }\n\
             \n\
             impl AsRef<cddl_lib::MyExt> for MyExt {\n\
             \x20   fn as_ref(&self) -> &cddl_lib::MyExt {\n\
             \x20       &self.0\n\
             \x20   }\n\
             }\n",
        )
        .unwrap();

        // Regenerate over the same directory: seed-once must leave BOTH hand-wired thin roots byte-identical.
        crate::api::generate_to_disk(&cli).unwrap();
        assert_eq!(
            rust_hand_wired,
            std::fs::read_to_string(&rust_lib_rs).unwrap(),
            "[wasm] the hand-wired rust thin root must survive regeneration byte-for-byte"
        );
        assert_eq!(
            wasm_hand_wired,
            std::fs::read_to_string(&wasm_lib_rs).unwrap(),
            "[wasm] the hand-wired wasm thin root must survive regeneration byte-for-byte"
        );

        // The generator must emit the re-export glue into BOTH generated roots so the bare `MyExt`
        // references resolve to the user's definitions.
        let rust_generated_mod =
            std::fs::read_to_string(out.join("rust/src/generated/mod.rs")).unwrap();
        assert!(
            rust_generated_mod.contains("pub use crate::MyExt;"),
            "[wasm] rust generated/mod.rs must re-export the in-crate extern:\n{rust_generated_mod}"
        );
        let wasm_generated_mod =
            std::fs::read_to_string(out.join("wasm/src/generated/mod.rs")).unwrap();
        assert!(
            wasm_generated_mod.contains("pub use crate::MyExt;"),
            "[wasm] wasm generated/mod.rs must re-export the in-crate extern wrapper:\n{wasm_generated_mod}"
        );

        if tool_exists("cargo") {
            let target_dir = scratch.join("target");
            let build = tool_cmd("cargo")
                .arg("build")
                .current_dir(out.join("wasm"))
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            assert!(
                build.status.success(),
                "[wasm] hand-wired in-crate extern WASM crate must compile\n{}\n{}",
                String::from_utf8_lossy(&build.stdout),
                String::from_utf8_lossy(&build.stderr)
            );
        }

        let _ = std::fs::remove_dir_all(&scratch);
    }

    run("default", &[]);
    run("preserve", &["--preserve-encodings=true"]);
    run_wasm();
}

/// The documented migration trap for pre-split consumers must fail LOUD, not silent. A consumer whose
/// `lib.rs` predates the thin-root split still carries `pub mod serialization;` (and inline generated
/// type defs) pointing at siblings the split relocated under `src/generated/**`. Seed-once leaves that
/// stale root untouched (the tool doesn't read it to decide what to emit — regeneration still
/// succeeds), so the breakage surfaces at COMPILE time as a diagnosable error, exactly as the design
/// doc requires. We pin the stable `E0583` module-resolution failure (the relocated `serialization`
/// module is now unresolved from the root) so a future silent-clobber regression can't pass this gate.
#[test]
fn migration_legacy_root_fails_loudly() {
    use clap::Parser;
    let scratch = std::env::temp_dir().join(format!(
        "cddl_codegen_migration_loud_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]);
    let lib_rs = out.join("rust/src/lib.rs");

    crate::api::generate_to_disk(&cli).unwrap();

    // Overwrite the seeded root with a faithful pre-split monolith: crate attrs, a `pub mod
    // serialization;` decl (whose file the split moved under `generated/`), and an inline copy of the
    // generated `Foo` — the shape a legacy consumer carries. Crucially, no `mod generated;`.
    std::fs::write(
        &lib_rs,
        "// legacy pre-split monolithic root\n#![allow(clippy::too_many_arguments)]\npub mod serialization;\n\n#[derive(Clone, Debug)]\npub struct Foo {\n    pub x: u64,\n}\n",
    )
    .unwrap();

    // Re-export must SUCCEED — the tool never reads the root to decide what to emit; it seed-once
    // skips it and regenerates the subtree beside it.
    crate::api::generate_to_disk(&cli).expect("regeneration over a legacy root must still succeed");

    if tool_exists("cargo") {
        let target_dir = scratch.join("target");
        let build = tool_cmd("cargo")
            .arg("build")
            .current_dir(out.join("rust"))
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        assert!(
            !build.status.success(),
            "a legacy pre-split root must NOT silently compile after regeneration"
        );
        let stderr = String::from_utf8_lossy(&build.stderr);
        assert!(
            stderr.contains("E0583"),
            "the legacy-root breakage must be the diagnosable module-resolution failure (E0583: the \
             relocated `serialization` module is unresolved from the root), got:\n{stderr}"
        );
    }

    let _ = std::fs::remove_dir_all(&scratch);
}

/// The legacy-shape stderr warning (diagnostics only — output bytes are unchanged): when a crate-root
/// `lib.rs` already exists and does NOT declare `mod generated`, `export()` prints a one-time-migration
/// warning naming the remedy; a thin root (which does declare it) draws no warning. Captured via a real
/// CLI run (`cargo run`) because the warning is an `eprintln!` on the tool's process stderr.
#[test]
fn legacy_root_warning_fires_only_for_legacy_shape() {
    if !tool_exists("cargo") {
        return;
    }
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_legacy_warn_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let lib_rs = out.join("rust/src/lib.rs");

    let run = || {
        tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=false")
            .output()
            .unwrap()
    };

    // First run seeds a thin root; a second run over it must NOT warn (the root declares `mod
    // generated`).
    let first = run();
    assert!(first.status.success());
    let thin = run();
    assert!(thin.status.success());
    let thin_stderr = String::from_utf8_lossy(&thin.stderr);
    assert!(
        !thin_stderr.contains("predates the thin-root layout"),
        "a thin root (with `mod generated`) must NOT draw the legacy-shape warning, got:\n{thin_stderr}"
    );

    // Now overwrite with a legacy-shaped root (no `mod generated`) and re-run: the warning must fire
    // and name the remedy.
    std::fs::write(
        &lib_rs,
        "// legacy pre-split monolithic root\npub mod serialization;\npub struct Foo;\n",
    )
    .unwrap();
    let legacy = run();
    assert!(
        legacy.status.success(),
        "regeneration over a legacy root still succeeds (warning is diagnostic only)"
    );
    let legacy_stderr = String::from_utf8_lossy(&legacy.stderr);
    assert!(
        legacy_stderr.contains("predates the thin-root layout")
            && legacy_stderr.contains("mod generated;"),
        "a legacy-shaped root must draw the one-time-migration warning naming the remedy, got:\n{legacy_stderr}"
    );
    // Diagnostics only: the legacy root is left byte-for-byte untouched (seed-once still skips it).
    assert_eq!(
        std::fs::read_to_string(&lib_rs).unwrap(),
        "// legacy pre-split monolithic root\npub mod serialization;\npub struct Foo;\n",
        "the legacy root must be left untouched — the warning changes no output bytes"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// A regeneration over an UNPARSEABLE existing manifest must be a hard error that names the file —
/// never a silent clobber (a parse failure is exactly when the user has content we can't preserve).
#[test]
fn cargo_manifest_rejects_unparseable_existing() {
    use clap::Parser;
    let scratch = std::env::temp_dir().join(format!(
        "cddl_codegen_manifest_badtoml_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]);

    crate::api::generate_to_disk(&cli).unwrap();
    let manifest = out.join("rust/Cargo.toml");
    let garbage = "this is [[[ not valid toml";
    std::fs::write(&manifest, garbage).unwrap();

    let err = crate::api::generate_to_disk(&cli)
        .expect_err("regeneration over unparseable manifest must error");
    let msg = err.to_string();
    assert!(
        msg.contains("rust/Cargo.toml"),
        "error must name the offending manifest, got: {msg}"
    );
    // the corrupt content is left untouched (not clobbered)
    assert_eq!(std::fs::read_to_string(&manifest).unwrap(), garbage);

    let _ = std::fs::remove_dir_all(&scratch);
}

/// Compile gate for `--wasm-cbor-json-api-macro` — the third external-macro flag and, unlike its two
/// snapshot+compile-gated siblings (`wasm_list_macro_compiles`), previously had zero coverage
/// anywhere (it is the documented CML invocation path). The flag replaces each wasm wrapper's inline
/// CBOR/JSON API with a `cbor_json_api!(WasmName);` invocation referencing a user-supplied macro, so
/// it can't compile standalone. Wire in `tests/wasm-macro-crate`'s real `cbor_json_api!` definition
/// (whose bodies mirror the inline emission, so a divergent invocation fails to compile) and
/// `cargo check` the generated wasm crate — same pattern as `wasm_list_macro_compiles`.
/// `--emit-tests-conformance` without `--emit-tests` must be rejected up front (there is no
/// generated-test module to add the conformance calls to). Pins the rejection *and* its message so
/// the guard can't silently become a no-op, and confirms the same flags together are accepted — so
/// the rejection is specific to the missing `--emit-tests`. Mirrors
/// `flag_value_rejects_canonical_without_preserve`.
#[test]
fn flag_rejects_conformance_without_emit_tests() {
    let mut cli = crate::cli::Cli {
        input: std::path::PathBuf::from("tests/corpus/exclusive_range.cddl"),
        output: std::path::PathBuf::from("unused"),
        emit_tests_conformance: true,
        emit_tests: false,
        ..Default::default()
    };
    let err = crate::api::with_types(&cli, |_, _| ())
        .expect_err("--emit-tests-conformance without --emit-tests should be rejected");
    let msg = err.to_string();
    assert!(
        msg.contains("--emit-tests-conformance") && msg.contains("--emit-tests=true"),
        "rejection message should name both flags, got: {msg}"
    );
    cli.emit_tests = true;
    assert!(
        crate::api::with_types(&cli, |_, _| ()).is_ok(),
        "--emit-tests-conformance with --emit-tests should be accepted"
    );
}

#[test]
fn wasm_cbor_json_api_macro_compiles() {
    use std::str::FromStr;
    let test_path = std::path::PathBuf::from_str("tests/canonical").unwrap();
    let out = test_path.join("export_cbor_json_api_macro");
    let _ = std::fs::remove_dir_all(&out);
    let gen_out = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!(
            "--input={}",
            test_path.join("input.cddl").to_str().unwrap()
        ))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .arg("--wasm=true")
        .arg("--wasm-cbor-json-api-macro=wasm_macro_crate::cbor_json_api")
        .output()
        .unwrap();
    assert!(
        gen_out.status.success(),
        "generation failed\n{}",
        String::from_utf8_lossy(&gen_out.stderr)
    );
    // vacuous-pass guard: only gates the macro path if the flag actually collapsed the inline API
    // into invocations (11 at landing over tests/canonical).
    let lib = std::fs::read_to_string(out.join("wasm/src/generated/mod.rs")).unwrap();
    let n_invocations = lib.matches("cbor_json_api!(").count();
    assert!(
        n_invocations >= 8,
        "only {n_invocations} cbor_json_api! invocations emitted (expected >= 8) — the flag stopped \
         collapsing the inline API, so this gate no longer gates the macro path"
    );
    let mut cargo_toml = std::fs::OpenOptions::new()
        .append(true)
        .open(out.join("wasm/Cargo.toml"))
        .unwrap();
    cargo_toml
        .write_all(b"wasm-macro-crate = { path = \"../../../wasm-macro-crate\" }\n")
        .unwrap();
    std::mem::drop(cargo_toml);
    let target_dir = std::env::temp_dir().join(format!(
        "cddl_codegen_wasm_cbor_json_api_macro_{:016x}",
        checkout_hash()
    ));
    let check = tool_cmd("cargo")
        .arg("check")
        .current_dir(out.join("wasm"))
        .env("CARGO_TARGET_DIR", &target_dir)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "cargo check failed\n{}",
        String::from_utf8_lossy(&check.stderr)
    );
}

// ---------------------------------------------------------------------------
// wasm three-state read fidelity (`tests/nullable-wasm/`).
//
// wasm-bindgen can't represent a nested `Option<Option<T>>`, so a nullable value (`T / null` ->
// `Option<T>`) sitting where the accessor adds its own presence-`Option` is FLATTENED to a single
// `Option<T>` on read: the plain getter reports `None` for BOTH an absent slot and a present-but-null
// one. Three-state fidelity is restored ADDITIVELY (no existing getter signature changes) by presence
// accessors emitted from exactly the flatten condition: `has_<field>()` beside an optional-nullable
// struct field getter, and `has(key)` on a nullable-value map wrapper. The single-nested enum
// variant needs no new accessor — `kind()` + `as_variant()` is already unambiguous. A DOUBLE-nested
// enum variant (payload resolving to `Option<Option<T>>`) is unreachable: the wasm enum constructor
// panics on such a variant before any getter is emitted, so `add_wasm_enum_getters` fails loud
// (`unreachable!`) instead of silently skipping. The behavioural oracle is the fixture's
// `tests_wasm.rs`, which constructs all three states through the rust API and asserts pairwise-distinct
// observations through the wasm accessors; read protocols live in docs/docs/wasm_differences.mdx.
#[test]
fn nullable_wasm() {
    run_test("nullable-wasm", &[], None, &[], &[], false, &[]);
}

// ---------------------------------------------------------------------------
// Tracked SILENT-WRONG-OUTPUT gaps (compile-green, snapshot-blessed, behaviorally wrong).
//
// These three corpus constructs generate code whose behavior contradicts the CDDL spec, and no
// automated oracle observes it: the corpus's only verdicts are "snapshot unchanged" + "it
// compiles" (`feature_corpus_compiles`), both of which the wrong code passes. Each is ledgered in
// cddl-matrix/ROADMAP.md ("Bugs / gaps surfaced as findings") and flagged ⚠️ in
// tests/corpus/COVERAGE.md, but a hand-authored overlay note can't fail a build — these stubs make
// the gaps visible in the suite itself, per the same convention as the wasm-fidelity pair above.
// Remove #[ignore] and write the real behavioral assertion when the generator is fixed (or when
// the emitted round-trip harness grows to cover the construct).

/// `a...b` must EXCLUDE b (max valid = b-1). `[v: 0...10]` must emit `max: Some(9)` — NOT the old
/// `max: Some(11)` (which accepted the out-of-spec 10 and 11). Asserts on the COMMITTED snapshot so a
/// regression can't slip back in via an unreviewed re-bless, mirroring `corpus_inline_group_members_kept`.
/// The behavioral half (9 round-trips, 10/11 rejected) is owned by the `--emit-tests` reject cases and
/// the `ir_conformance_corpus` oracle.
#[test]
fn corpus_exclusive_range_upper_bound() {
    let lib = std::fs::read_to_string(
        "tests/corpus/snapshots/exclusive_range/default__rust__src__generated__mod.rs.snap",
    )
    .expect("exclusive_range lib snapshot missing");
    assert!(
        lib.contains("max: Some(9)") && lib.contains("if v > 9"),
        "exclusive_range no longer emits the exclusive upper bound 9 — the a...b (max=b-1) bound is wrong"
    );
    assert!(
        !lib.contains("Some(11)") && !lib.contains("> 11"),
        "exclusive_range emits the old inclusive-off-by-one bound 11 — a...b must EXCLUDE b (max=b-1)"
    );
}

/// Occurrence counts (`+`, `n*m`) are LENGTH constraints on the array: embed sites must enforce
/// them on the array's length (deserialize + fallible constructor) and must never misread them
/// as element VALUE bounds (`[+ uint]` once rejected the element value 0 and `[2*5 uint]`
/// rejected element values outside 2..=5, while any length passed — parsing hung the bounds on
/// the element type instead of the array). This asserts on the COMMITTED occurrence snapshots so
/// neither regression can come back via an unreviewed re-bless; the *executed* proof is the
/// fixture's emit-tests run in `feature_corpus_compiles` (its deser-reject cases push each
/// field's length out of bounds — mutation-verified red when the emitted length check is
/// removed).
#[test]
fn corpus_occurrence_bounds_enforced() {
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/occurrence/default__rust__src__generated__serialization.rs.snap",
    )
    .expect("occurrence serialization snapshot missing");
    // `2*5` and `1*3` keep the runtime occurrence-count length checks byte-for-byte (only the exact
    // `+` shape changes representation — WI-1 of two-type-constraint-enforcement).
    for check in [
        "if b_arr.len() < 2 || b_arr.len() > 5 {",
        "if inline_bounded_arr.len() < 1 || inline_bounded_arr.len() > 3 {",
    ] {
        assert!(
            ser.contains(check),
            "occurrence snapshot lost the occurrence-count length check `{check}`"
        );
    }
    // the `+` (`[+ uint]`) shape is now type-enforced: its length check is GONE from the ctor/deser,
    // replaced by the single `NonEmptyVec::try_from` door (identical RangeCheck error). The old
    // inline `if o_arr.len() < 1` runtime check must NOT come back.
    assert!(
        !ser.contains("if o_arr.len() < 1 {"),
        "occurrence `[+ uint]` reverted to an inline length check — it must enforce via NonEmptyVec"
    );
    assert!(
        ser.contains("NonEmptyVec::try_from(o_arr)?"),
        "occurrence `[+ uint]` deserialize must route the collected Vec through NonEmptyVec::try_from"
    );
    // the value-misread form bound each ELEMENT read through `.and_then(|x| if x < ... )` —
    // occurrence bounds must never re-attach to element values
    assert!(
        !ser.contains("found: x as isize"),
        "occurrence snapshot has an element VALUE RangeCheck — occurrence counts are being \
         misread as element value bounds again"
    );
}

/// The named/inline `[+ elem]` wasm-surface contract (two-type design doc, dedup + collision
/// decisions), asserted on the COMMITTED core whole-program wasm snapshot so none of it can
/// regress via an unreviewed re-bless; the *executed* proof is `tests/core/tests_wasm.rs`'s
/// `wasm_non_empty_named_free_selfnamed_and_dedup` in `core_with_wasm`:
/// - an inline `[+ nev_pt]` DEDUPS to the named `nev_pts = [+ nev_pt]` rule's class — no
///   synthesized `NonEmptyNevPtList` may exist, and member surfaces use the named class;
/// - the free-named rule's `try_from` borrows the loose `NevPtList` builder (minted for it);
/// - a SELF-NAMED rule (`nev_q_list = [+ nev_q]`, rule ident == loose-builder name) emits its
///   restricted class WITHOUT `try_from` — the self-referential `try_from(list: &NevQList)`
///   form was the miscompile (the restricted class fed to itself as the loose source, E0277).
#[test]
fn core_non_empty_dedup_and_self_named_wasm_surface() {
    let wasm = std::fs::read_to_string(
        "tests/corpus/snapshots/core/default__wasm__src__generated__mod.rs.snap",
    )
    .expect("core wasm snapshot missing");
    assert!(
        !wasm.contains("NonEmptyNevPtList"),
        "inline `[+ nev_pt]` must dedup to the named NevPts class, not mint NonEmptyNevPtList"
    );
    assert!(
        wasm.contains("pub fn pts_inline(&self) -> NevPts"),
        "the deduped inline field's getter must return the NAMED class NevPts"
    );
    assert!(
        wasm.contains("pub fn try_from(list: &NevPtList) -> Result<NevPts, JsError>"),
        "the free-named rule's try_from must borrow the loose NevPtList builder"
    );
    assert!(
        !wasm.contains("try_from(list: &NevQList)"),
        "self-named `[+ …]` rule emitted a self-referential try_from — the restricted class fed \
         to itself as the loose source (the E0277 miscompile class)"
    );
}

/// Special-class (major-type-7) map KEYS must deserialize through the map loop, not be
/// intercepted as a potential break byte — in EITHER framing. The definite-length loop reads
/// exactly `n` entries (`make_deser_loop_break_check` gates its Special check on the INDEFINITE
/// case only), and the indefinite loop uses the non-consuming `special_break()` probe, so a bool
/// key is left in place and flows straight to `bool::deserialize`. This asserts on the COMMITTED
/// special_map_key snapshots so the interception can't come back via an unreviewed re-bless; the
/// *executed* proof is the fixture's emit-tests round-trip in `feature_corpus_compiles` (it mints
/// a real `(false, 0)` entry — mutation-verified: an unconditional break check fails it with
/// EndingBreakMissing at BkeyHolder.named) plus the golden_hex_preserve indefinite-bool KATs.
#[test]
fn corpus_special_map_key_supported() {
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/special_map_key/default__rust__src__generated__serialization.rs.snap",
    )
    .expect("special_map_key serialization snapshot missing");
    assert!(
        ser.contains("bool::deserialize(raw)?"),
        "special_map_key snapshot no longer deserializes the bool key through the element path"
    );
    // every Special peek in the map loops must be gated on the indefinite case — an ungated
    // `raw.cbor_type()? == cbor_event::Type::Special` check would eat definite-length bool keys.
    // The RHS counts the gate by its indefinite PATTERN (always on one line with its comma —
    // rustfmt wraps the `&&` chain, never the `matches!` args), not by a bare `if matches!(`,
    // so an unrelated future matches-gate can't pad the count.
    assert_eq!(
        ser.matches("raw.cbor_type()? == cbor_event::Type::Special")
            .count(),
        ser.matches(", cbor_event::Len::Indefinite)").count(),
        "special_map_key snapshot has a Special-class peek not gated on an indefinite length — \
         the break-interception bug on definite-length special keys is back"
    );
}

/// Float fields under `--preserve-encodings` abort generation (`unimplemented!` at the
/// generation.rs float serialize arm), which is why no corpus fixture can hold a float in a
/// struct field (the corpus runs every fixture under the preserve profile) and why the float
/// element wire path has no executed preserve/canonical coverage.
#[test]
#[ignore = "float --preserve-encodings is unimplemented: any float struct field aborts generation under the flag (generation.rs 'preserve_encodings is not implemented for float'). Implementing it unblocks float corpus fixtures (see homogeneous_array.cddl's comment) and float KAT vectors."]
fn preserve_encodings_supports_floats() {
    unimplemented!(
        "a float struct field + --preserve-encodings=true panics generation. Implement float \
         encoding preservation (half/single/double header forms are the preserve axis; canonical \
         is smallest-form-that-round-trips per RFC 8949 §4.2.1), add a float field to a preserve \
         fixture + hand-derived golden-hex vectors (major type 7 heads 0xf9/0xfa/0xfb), then \
         assert here and remove #[ignore]."
    );
}

/// `[(uint, tstr)]` must keep BOTH spliced members. This regressed member-dropping data loss for
/// years (a 1-field `InlineGroup { index_0 }` / `read_elems(1)` that parsed, compiled, and
/// round-tripped green) until the cddl-fork AST bump (c505d38) fixed the group's shape. The corpus
/// snapshots pin the fixed form; this asserts on the COMMITTED snapshots so a regression can't
/// slip back in via an unreviewed re-bless (the snapshot suite alone would happily pin the
/// 1-field form again).
#[test]
fn corpus_inline_group_members_kept() {
    let lib = std::fs::read_to_string(
        "tests/corpus/snapshots/inline_group/default__rust__src__generated__mod.rs.snap",
    )
    .expect("inline_group lib snapshot missing");
    assert!(
        lib.contains("index_0") && lib.contains("index_1"),
        "inline_group snapshot no longer keeps both spliced members — the [(uint, tstr)] member-drop bug is back"
    );
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/inline_group/default__rust__src__generated__serialization.rs.snap",
    )
    .expect("inline_group serialization snapshot missing");
    assert!(
        ser.contains("read_elems(2)"),
        "inline_group deserializer no longer reads 2 elements — the [(uint, tstr)] member-drop bug is back"
    );
}

/// A collapsed map-representation group-choice arm (`{ a: uint // b: tstr }`) stores only its
/// VALUE in the enum, so its fixed member key must be WRITTEN on serialize and VERIFIED on
/// deserialize. The historical miscompile dropped the key entirely — emitting `map(1)` + the bare
/// value (malformed/truncated CBOR) and dispatching decode on the VALUE type — so it round-tripped
/// with itself while rejecting the spec-valid `{"a": n}`/`{"b": s}` form. The multi-field arm
/// (`{ a: uint, x: tstr // b: tstr }`) was broken the same way on the decode side: dispatch keyed
/// on the first VALUE type (uint) instead of the first member KEY type (text "a"). This asserts on
/// the COMMITTED group_choice_map snapshots so neither regression can slip back via an unreviewed
/// re-bless; the *executed* proof is the fixture's emit-tests round-trip in `feature_corpus_compiles`
/// plus the decode-conformance accept vectors on the `group.choice` catalog row (a reverted
/// key-dropping decoder mis-decodes the spec-valid `{"a": n}` foreign bytes and fails the replay
/// gate).
#[test]
fn corpus_group_choice_map_key_written_and_verified() {
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/group_choice_map/default__rust__src__generated__serialization.rs.snap",
    )
    .expect("group_choice_map serialization snapshot missing");
    // Serialize side: each collapsed arm writes its fixed member key between the map header and the
    // value (text keys for TextKeyed, uint value-keys for UintKeyed) — dropping any of these is the
    // malformed-CBOR miscompile.
    for key_write in [
        "serializer.write_text(\"a\")?;",
        "serializer.write_text(\"b\")?;",
        "serializer.write_unsigned_integer(1u64)?;",
        "serializer.write_unsigned_integer(2u64)?;",
    ] {
        assert!(
            ser.contains(key_write),
            "group_choice_map serialize no longer writes the collapsed member key `{key_write}` — \
             the map-rep group-choice key-drop miscompile is back"
        );
    }
    // Deserialize side: each collapsed arm reads and VERIFIES its member key (a mismatch is a
    // FixedValueMismatch). These checks vanish if the decoder reverts to dispatching on the value.
    for key_verify in [
        "if a_key != \"a\" {", // TextKeyed A arm
        "if uint_key != 1 {",  // UintKeyed / MixedKeyTypes uint-key arm
        "if text_key != 2 {",  // UintKeyed B arm (uint key 2)
    ] {
        assert!(
            ser.contains(key_verify),
            "group_choice_map deserialize no longer verifies the collapsed member key `{key_verify}` — \
             the decoder is dispatching on the value again, not the key"
        );
    }
    // Multi-field arm: decode must route through the 2-field embedded record (keying on the first
    // member KEY), not dispatch on the first field's VALUE type.
    assert!(
        ser.contains("MultiFieldArm0::deserialize_as_embedded_group"),
        "multi_field_arm no longer decodes via its embedded record — the first-value-type dispatch \
         bug (matching uint before the text key) is back"
    );
}

#[test]
fn core_with_wasm() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_defs");
    let extern_wasm_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_wasm_defs");
    let custom_ser_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("custom_serialization");
    run_test(
        "core",
        &[],
        Some("wasm"),
        &[extern_rust_path, custom_ser_path],
        &[extern_wasm_path],
        false,
        &[],
    );
}

#[test]
fn core_no_wasm() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_defs");
    let custom_ser_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("custom_serialization");
    run_test(
        "core",
        &["--wasm=false"],
        None,
        &[extern_rust_path, custom_ser_path],
        &[],
        false,
        &[],
    );
}

#[test]
fn golden_hex() {
    // Known-answer CBOR vectors hand-derived from RFC 8949 (see tests/golden_hex/tests.rs).
    // Rust-only: the encoding is identical on the wasm side, so skip the wasm build.
    run_test("golden_hex", &["--wasm=false"], None, &[], &[], false, &[]);
}

#[test]
fn golden_hex_preserve() {
    // Known-answer preserve-encodings vectors: irregular RFC 8949 §3 encodings (non-minimal
    // header arguments, indefinite/chunked items, map key order) hand-derived as raw hex —
    // deliberately NOT built with the tests/deser_test cbor_event helpers, which share the
    // write_*_sz layer with the generated code (see tests/golden_hex_preserve/tests.rs).
    run_test(
        "golden_hex_preserve",
        &["--wasm=false", "--preserve-encodings=true"],
        None,
        &[],
        &[],
        false,
        &[],
    );
}

#[test]
fn golden_hex_canonical() {
    // Known-answer canonical-form vectors: the same irregular-encoding family re-encoded to
    // hand-derived RFC 8949 §4.2 minimal bytes — the independent check on cbor_event's
    // Sz::canonical() header-minimality core (see tests/golden_hex_canonical/tests.rs).
    run_test(
        "golden_hex_canonical",
        &[
            "--wasm=false",
            "--preserve-encodings=true",
            "--canonical-form=true",
        ],
        None,
        &[],
        &[],
        false,
        &[],
    );
}

#[test]
fn comment_dsl() {
    run_test(
        "comment-dsl",
        &["--preserve-encodings=true"],
        None,
        &[],
        &[],
        false,
        &[],
    );
}

/// The dcSpark `cddl` fork (already this crate's parser dep) as a test dependency of a *generated*
/// crate, so its round-trips gain the independent conformance oracle (tests/deser_test_conformance.rs).
/// Pinned to the same rev as Cargo.toml — enforced by `cddl_oracle_dep_rev_matches_cargo_toml` below,
/// so a routine cddl bump that updates only Cargo.toml can't silently leave the oracle on a stale rev.
const CDDL_ORACLE_DEP: &str = "\ncddl = { git = \"https://github.com/dcSpark/cddl\", rev = \"ac1b98ec07184236517da4511b1bbea239e35190\" }\n";

#[test]
fn cddl_oracle_dep_rev_matches_cargo_toml() {
    let cargo_toml = include_str!("../../Cargo.toml");
    // the git-dep line (not the `repository = ".../cddl-codegen"` line, which also contains the URL)
    let rev = cargo_toml
        .lines()
        .find(|l| l.contains("dcSpark/cddl\"") && l.contains("rev = \""))
        .and_then(|l| l.split("rev = \"").nth(1))
        .and_then(|s| s.split('"').next())
        .expect("could not find the dcSpark/cddl git-dep rev in Cargo.toml");
    assert!(
        CDDL_ORACLE_DEP.contains(rev),
        "CDDL_ORACLE_DEP is out of sync with Cargo.toml's cddl rev ({rev}) — update the const so the \
         generated-crate conformance oracle validates against the same fork/rev as the generator's parser"
    );
}

fn cddl_oracle_dep_rev() -> &'static str {
    CDDL_ORACLE_DEP
        .split("rev = \"")
        .nth(1)
        .and_then(|s| s.split('"').next())
        .expect("CDDL_ORACLE_DEP must contain a rev")
}

fn rust_source_string_literal(s: &str) -> String {
    format!("{s:?}")
}

fn oracle_fingerprint_probe_string<'a>(probe: &'a serde_json::Value, field: &str) -> &'a str {
    probe
        .get(field)
        .and_then(serde_json::Value::as_str)
        .unwrap_or_else(|| panic!("oracle_fingerprint.json probe has missing/invalid `{field}`"))
}

fn oracle_fingerprint_probe_bool(probe: &serde_json::Value, field: &str) -> bool {
    probe
        .get(field)
        .and_then(serde_json::Value::as_bool)
        .unwrap_or_else(|| panic!("oracle_fingerprint.json probe has missing/invalid `{field}`"))
}

fn oracle_fingerprint_hex_bytes(name: &str, hex: &str) -> Vec<u8> {
    assert!(
        hex.len().is_multiple_of(2),
        "oracle_fingerprint.json probe `{name}` has odd-length cborHex `{hex}`"
    );
    (0..hex.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&hex[i..i + 2], 16).unwrap_or_else(|e| {
                panic!("oracle_fingerprint.json probe `{name}` has invalid cborHex `{hex}`: {e}")
            })
        })
        .collect()
}

fn oracle_fingerprint_byte_slice_literal(bytes: &[u8]) -> String {
    let items = bytes
        .iter()
        .map(|b| format!("0x{b:02x}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("&[{items}]")
}

fn rust_oracle_fingerprint_preflight(scratch_root: &std::path::Path, target_dir: &std::path::Path) {
    let fingerprint_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("cddl-matrix/oracle_fingerprint.json");
    let fingerprint_text = std::fs::read_to_string(&fingerprint_path)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", fingerprint_path.display()));
    let fingerprint: serde_json::Value = serde_json::from_str(&fingerprint_text)
        .unwrap_or_else(|e| panic!("cannot parse {}: {e}", fingerprint_path.display()));
    let probes = fingerprint
        .get("probes")
        .and_then(serde_json::Value::as_array)
        .unwrap_or_else(|| panic!("{} must contain a probes array", fingerprint_path.display()));
    assert!(
        probes.len() >= 5,
        "{} contains only {} probe(s); the oracle fingerprint anti-vacuity floor is 5",
        fingerprint_path.display(),
        probes.len()
    );

    let crate_dir = scratch_root.join("fingerprint_probe");
    let _ = std::fs::remove_dir_all(&crate_dir);
    std::fs::create_dir_all(crate_dir.join("src"))
        .unwrap_or_else(|e| panic!("cannot create {}: {e}", crate_dir.join("src").display()));
    std::fs::write(
        crate_dir.join("Cargo.toml"),
        format!(
            "[package]\nname = \"cddl_oracle_fingerprint_probe\"\nversion = \"0.0.0\"\nedition = \"2021\"\n\n[dependencies]\n{}",
            CDDL_ORACLE_DEP
        ),
    )
    .unwrap_or_else(|e| panic!("cannot write probe Cargo.toml: {e}"));

    let rev = cddl_oracle_dep_rev();
    let mut main_rs =
        String::from("fn main() {\n    let mut failures: Vec<String> = Vec::new();\n");
    for probe in probes {
        let name = oracle_fingerprint_probe_string(probe, "name");
        let spec = oracle_fingerprint_probe_string(probe, "spec");
        let mode = oracle_fingerprint_probe_string(probe, "mode");
        let expect_ok = oracle_fingerprint_probe_bool(probe, "expectOk");
        let why = oracle_fingerprint_probe_string(probe, "why");
        let expected = if expect_ok { "OK" } else { "ERR" };
        match mode {
            "compile" => {
                main_rs.push_str(&format!(
                    "    let observed = cddl::parser::cddl_from_str({}, false).is_ok();\n",
                    rust_source_string_literal(spec)
                ));
                main_rs.push_str(&format!(
                    "    if observed != {expect_ok} {{\n        failures.push(format!(\"  - probe '{{}}': spec {{:?}}; expected {expected}, observed {{}}. {{}}\", {}, {}, if observed {{ \"OK\" }} else {{ \"ERR\" }}, {}));\n    }}\n",
                    rust_source_string_literal(name),
                    rust_source_string_literal(spec),
                    rust_source_string_literal(why)
                ));
            }
            "validate" => {
                let cbor_hex = probe
                    .get("cborHex")
                    .and_then(serde_json::Value::as_str)
                    .unwrap_or_else(|| {
                        panic!("oracle_fingerprint.json validate probe `{name}` is missing cborHex")
                    });
                let bytes = oracle_fingerprint_hex_bytes(name, cbor_hex);
                main_rs.push_str(&format!(
                    "    let bytes: &[u8] = {};\n    let observed = cddl::validate_cbor_from_slice({}, bytes, None).is_ok();\n",
                    oracle_fingerprint_byte_slice_literal(&bytes),
                    rust_source_string_literal(spec)
                ));
                main_rs.push_str(&format!(
                    "    if observed != {expect_ok} {{\n        failures.push(format!(\"  - probe '{{}}': spec {{:?}} cbor 0x{{}}; expected {expected}, observed {{}}. {{}}\", {}, {}, {}, if observed {{ \"OK\" }} else {{ \"ERR\" }}, {}));\n    }}\n",
                    rust_source_string_literal(name),
                    rust_source_string_literal(spec),
                    rust_source_string_literal(cbor_hex),
                    rust_source_string_literal(why)
                ));
            }
            other => panic!("oracle_fingerprint.json probe `{name}` has unknown mode `{other}`"),
        }
    }
    main_rs.push_str(&format!(
        "    if failures.is_empty() {{\n        println!(\"rust cddl crate fingerprint OK ({{}} probes - CDDL_ORACLE_DEP rev {})\", {});\n    }} else {{\n        eprintln!(\"rust cddl crate fingerprint MISMATCH - failing probe(s):\");\n        for failure in failures {{ eprintln!(\"{{failure}}\"); }}\n        std::process::exit(1);\n    }}\n}}\n",
        rev,
        probes.len()
    ));
    std::fs::write(crate_dir.join("src/main.rs"), main_rs)
        .unwrap_or_else(|e| panic!("cannot write probe main.rs: {e}"));

    let output = tool_cmd("cargo")
        .arg("run")
        .current_dir(&crate_dir)
        .env("CARGO_TARGET_DIR", target_dir)
        .output()
        .unwrap_or_else(|e| panic!("failed to run rust oracle fingerprint probe crate: {e}"));
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    if !output.status.success() {
        // Deliberate conflation: a probe-crate BUILD failure (cargo compile error — e.g. a rev
        // bump changed the crate's `cddl_from_str`/`validate_cbor_from_slice` API surface) lands
        // in this same MISMATCH panic as a behavioral mismatch. Both mean "the pinned-oracle
        // contract does not hold at this rev" and both demand the same conscious re-validation;
        // triage which one it is from the attached cargo output below (compile errors vs the
        // probe-name mismatch lines).
        let message = format!(
            "HARNESS FAILURE: rust oracle fingerprint MISMATCH — CDDL_ORACLE_DEP rev {rev} does not \
             behave like the pinned oracle. Failing probe(s):\n{combined}\nThe pinned oracle is the \
             fork's `local-fixes` branch @ ac1b98e, injected through CDDL_ORACLE_DEP. Recover by \
             updating CDDL_ORACLE_DEP only after consciously re-validating the shared probe set in \
             cddl-matrix/oracle_fingerprint.json."
        );
        panic!("{message}");
    }
    print!("{combined}");
}

#[test]
#[ignore = "manual oracle-crate fingerprint preflight: cargo test --bin cddl-codegen rust_oracle_fingerprint -- --ignored --nocapture"]
fn rust_oracle_fingerprint() {
    if !tool_exists("cargo") {
        return;
    }
    let scratch_name = format!(
        "cddl_codegen_rust_oracle_fingerprint_{:016x}",
        checkout_hash()
    );
    let _scratch_lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");
    rust_oracle_fingerprint_preflight(&root, &target_dir);
}

#[test]
fn preserve_encodings() {
    use std::str::FromStr;
    let custom_ser_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("custom_serialization_preserve");
    let conformance_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("deser_test_conformance.rs");
    run_test(
        "preserve-encodings",
        &["--preserve-encodings=true"],
        None,
        &[custom_ser_path, conformance_path],
        &[],
        false,
        &[CDDL_ORACLE_DEP],
    );
}

/// Executes the `--emit-tests` generated-test module end-to-end (tests/README.md § "Generated-test
/// harness"): generate
/// the rich preserve-encodings fixture with the flag on and `cargo test` the generated crate —
/// run_test's test step runs the emitted reject_*/roundtrip_* tests alongside the hand-written
/// suite. This is the emitter's execution gate (it previously had zero CI coverage, and its output
/// only ever compiled inside harness crates that happened to append `use serialization::*`).
/// The floor asserts keep the gate from going vacuous if emission silently shrinks.
#[test]
fn emit_tests_execute() {
    use std::str::FromStr;
    let custom_ser_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("custom_serialization_preserve");
    let conformance_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("deser_test_conformance.rs");
    run_test(
        "preserve-encodings",
        &["--preserve-encodings=true", "--emit-tests=true"],
        Some("emit_tests"),
        &[custom_ser_path, conformance_path],
        &[],
        false,
        &[CDDL_ORACLE_DEP],
    );
    // The emitted generated-test module now lands in the generated root (`generated/mod.rs`), not the
    // thin seed-once `lib.rs`.
    let lib = std::fs::read_to_string(
        "tests/preserve-encodings/export_emit_tests/rust/src/generated/mod.rs",
    )
    .unwrap();
    assert!(
        lib.contains("mod cddl_generated_tests"),
        "--emit-tests emitted no generated-test module"
    );
    let n_roundtrip = lib.matches("fn roundtrip_").count();
    let n_reject = lib.matches("fn reject_").count();
    assert!(
        n_roundtrip >= 20,
        "emitted only {n_roundtrip} roundtrip tests for the preserve fixture — emission silently shrank"
    );
    assert!(
        n_reject >= 3,
        "emitted only {n_reject} reject tests for the preserve fixture — emission silently shrank"
    );
    // Encoding-fidelity oracle (--preserve-encodings): the mutator module + a floor on how many
    // round-trip cases assert the preserve contract on irregular re-encodings, so the layer can't
    // silently vanish. The self-check test (mutator pinned against hand-derived RFC 8949 bytes +
    // the end-to-end `variants()` vacuity pin) must be present and, having run above, green.
    assert!(
        lib.contains("mod cddl_encoding_fidelity"),
        "--emit-tests --preserve-encodings emitted no encoding-fidelity mutator module"
    );
    assert!(
        lib.contains("fn encoding_mutator_self_check"),
        "the encoding-fidelity mutator self-check test is missing"
    );
    let n_fidelity = lib.matches("preserve-encodings must re-encode").count();
    assert!(
        n_fidelity >= 20,
        "emitted only {n_fidelity} encoding-fidelity assertions for the preserve fixture — the oracle silently shrank"
    );
}

/// Executes the `--emit-tests` generated WASM-test module end-to-end (tests/README.md § "wasm-crate
/// test module"): generate the rich `core` fixture with `--wasm=true --emit-tests=true`, then
/// `cargo test` the generated WASM crate so the emitted `wasm_roundtrip_*`/`wasm_reject_*` module runs
/// (alongside the hand-written `tests_wasm.rs` — the plausibility cross-check where the two overlap).
/// The floor asserts keep the gate from going vacuous if emission silently shrinks.
///
/// This deliberately does NOT reuse `run_test`, and does NOT `cargo test` the RUST crate: the rust
/// emitter is gated separately by `emit_tests_execute` (on the preserve fixture). The `core` fixture
/// is not `--emit-tests`-clean on the *rust* side — its hand-written `tests::docs`/`tests::no_alias`
/// truncate `lib.rs` at the first `#[cfg(test)]` (which the injected emitted module now precedes), and
/// its wire-ambiguous `TypeChoice` (uint `0` collides with the fixed `i0` variant) trips the rust
/// value-equality oracle. None of those are wasm concerns: the wasm crate builds the rust crate as a
/// *non-test* dependency, so the rust `#[cfg(test)]` module is never compiled here. `cargo check`
/// never compiles `#[cfg(test)]` code, so a `cargo test` of the wasm crate is the only thing that
/// compiles AND runs the emitted wasm module.
#[test]
fn emit_wasm_tests_execute() {
    use std::str::FromStr;
    if !tool_exists("cargo") {
        return;
    }
    let test_path = std::path::PathBuf::from_str("tests").unwrap().join("core");
    let export = "export_emit_wasm_tests";
    let export_path = test_path.join(export);

    // The rust and wasm crate roots are seed-once thin roots the tool never clobbers; a stale
    // monolithic root left in this persistent export dir would survive and collide with the freshly
    // regenerated `generated/**` subtree (plus the externs appended below). Clear them so the tool
    // re-seeds clean thin roots.
    let _ = std::fs::remove_file(export_path.join("rust/src/lib.rs"));
    let _ = std::fs::remove_file(export_path.join("wasm/src/lib.rs"));

    // generate the crate(s)
    let generate = tool_cmd("cargo")
        .arg("run")
        .arg("--")
        .arg(format!("--output={}", export_path.to_str().unwrap()))
        .arg(format!(
            "--input={}",
            test_path.join("input.cddl").to_str().unwrap()
        ))
        .arg("--wasm=true")
        .arg("--emit-tests=true")
        // Pristine clobber-then-append model (same as `run_test`): the externs/custom serializers
        // appended below carry their own comments, which default-on preservation would trap in
        // `compile_error!` blocks on the next reuse of this persistent dir.
        .arg("--no-preserve-comments")
        .output()
        .unwrap();
    if !generate.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&generate.stderr));
    }
    assert!(generate.status.success());

    // The wasm crate builds the rust crate as a (non-test) dependency, so the rust lib only needs to
    // COMPILE — append just the production externs it references (extern types + custom serializers),
    // NOT the rust test suite (deser_test/tests.rs), whose core-specific `--emit-tests` incompat is
    // out of scope here (see the doc comment). The two go in DIFFERENT scopes under the thin-root split
    // (see `run_test` for the rationale): the extern-TYPE def collides with the generator's
    // `pub use crate::Name;` re-export glue if placed in `generated/**`, so it goes into the user-owned
    // thin `lib.rs`; the custom-serialization helpers are called by `serialization.rs` via
    // `use super::*;`, so they stay in `generated/mod.rs`. Both need `use serialization::*;`.
    let mut root_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(export_path.join("rust/src/lib.rs"))
        .unwrap();
    root_lib.write_all(b"\nuse serialization::*;\n").unwrap();
    root_lib.write_all(b"\n\n").unwrap();
    root_lib
        .write_all(
            std::fs::read_to_string(test_path.parent().unwrap().join("external_rust_defs"))
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    std::mem::drop(root_lib);
    let mut rust_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(export_path.join("rust/src/generated/mod.rs"))
        .unwrap();
    rust_lib.write_all(b"\nuse serialization::*;\n").unwrap();
    rust_lib.write_all(b"\n\n").unwrap();
    rust_lib
        .write_all(
            std::fs::read_to_string(test_path.parent().unwrap().join("custom_serialization"))
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    std::mem::drop(rust_lib);

    // The wasm crate, like the rust crate, splits by scope under the thin-root layout (see `run_test`):
    // the extern-TYPE WRAPPER defs (`external_wasm_defs`) collide with the generator's
    // `pub use crate::Name;` re-export glue if placed in `generated/**`, so they go into the user-owned
    // thin wasm `lib.rs` (which needs `wasm_bindgen`/`JsError` added — generated/mod.rs only `use`s them
    // privately). The hand-written `tests_wasm.rs` runs beside the emitted module as the plausibility
    // cross-check and resolves the wrapper types (re-exported into `generated/**` by the glue), so it
    // stays in `generated/mod.rs`.
    let mut wasm_root_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(export_path.join("wasm/src/lib.rs"))
        .unwrap();
    wasm_root_lib
        .write_all(b"\nuse wasm_bindgen::prelude::{wasm_bindgen, JsError};\n\n")
        .unwrap();
    wasm_root_lib
        .write_all(
            std::fs::read_to_string(test_path.parent().unwrap().join("external_wasm_defs"))
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    std::mem::drop(wasm_root_lib);
    let wasm_lib_path = export_path.join("wasm/src/generated/mod.rs");
    let mut wasm_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(&wasm_lib_path)
        .unwrap();
    wasm_lib.write_all(b"\n\n").unwrap();
    wasm_lib
        .write_all(
            std::fs::read_to_string(test_path.join("tests_wasm.rs"))
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
    std::mem::drop(wasm_lib);

    // cargo test the WASM crate only
    let wasm_test = tool_cmd("cargo")
        .arg("test")
        .current_dir(export_path.join("wasm"))
        .output()
        .unwrap();
    if !wasm_test.status.success() {
        eprintln!(
            "wasm test stderr:\n{}",
            String::from_utf8_lossy(&wasm_test.stderr)
        );
    }
    println!(
        "wasm test stdout:\n{}",
        String::from_utf8_lossy(&wasm_test.stdout)
    );
    assert!(wasm_test.status.success());

    // Floors pinned to the core fixture's minted count; they keep the gate from going vacuous if
    // emission silently shrinks. Ratcheted when the wrapper-collection (new/add, new/insert) and
    // wrapper-`From` ctor-arg builds landed — every core type now mints a wasm surface (no loud skip).
    let lib = std::fs::read_to_string(&wasm_lib_path).unwrap();
    assert!(
        lib.contains("mod cddl_generated_wasm_tests"),
        "--emit-tests emitted no generated WASM-test module"
    );
    let n_roundtrip = lib.matches("fn wasm_roundtrip_").count();
    let n_bounds = lib.matches("fn wasm_bounds_").count();
    assert!(
        n_roundtrip >= 60,
        "emitted only {n_roundtrip} wasm_roundtrip tests for the core fixture — emission silently shrank"
    );
    assert!(
        n_bounds >= 5,
        "emitted only {n_bounds} wasm_bounds tests for the core fixture — emission silently shrank"
    );
}

/// The IR-bug conformance oracle at breadth (`tests/README.md` § "IR-bug conformance oracle at breadth"). The
/// `--emit-tests` round-trip harness mints values from the SAME IR as the code under test, so an
/// IR-level miscompile (a bound/member computed wrong at parse time) mints a spec-violating value
/// and then asserts it round-trips *green*. This gate closes that residual: it generates every
/// `tests/corpus/*.cddl` with `--emit-tests --emit-tests-conformance`, wires in the `cddl` crate
/// (`CDDL_ORACLE_DEP`) + the shared oracle helpers (`deser_test_conformance.rs`) + the source spec,
/// and `cargo test`s each crate — so each minted round-trip value is validated against its SOURCE
/// `.cddl` rule by the `cddl` crate's independent decode+constraint path.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d so it stays out of CI under the feature freeze (it adds the
/// heavy `cddl` dep to every corpus crate; see `tests/README.md`). Run with:
///   `cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture`
///
/// EXPECTED_FAIL: fixtures with a known IR bug whose minted value the oracle MUST reject. Their
/// `cargo test` must FAIL *and* the output must carry the oracle's distinctive message (so it failed
/// for the right reason, not an unrelated break). If an expected-fail fixture PASSES, the gate goes
/// RED ("IR bug apparently fixed or oracle lost teeth — investigate, then remove from EXPECTED_FAIL").
/// Any fixture NOT on the list that fails conformance also goes RED (a new IR miscompile).
///
/// Scope (documented, not solved): minted values are shallow/degenerate (None arms, empty tables,
/// depth-capped recursion) — this validates what the minter mints, at breadth across fixtures, not
/// exhaustive per-type depth; and it shares the dcSpark `cddl` fork's PARSER with the generator, so
/// it catches wrong VALUES, not fork-level misparses (same caveat as `deser_test_conformance.rs`).
#[test]
#[ignore = "manual/local IR-conformance gate (heavy cddl dep, CI feature-frozen): cargo test --bin cddl-codegen ir_conformance_corpus -- --ignored --nocapture"]
fn ir_conformance_corpus() {
    use std::str::FromStr;
    if !tool_exists("cargo") {
        return;
    }

    let scratch_name = format!("cddl_codegen_ir_conformance_{:016x}", checkout_hash());
    // Hold this for the whole gate: same-checkout concurrent runs serialize on it instead of
    // deleting each other's crates via the `remove_dir_all` below.
    let _scratch_lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");
    rust_oracle_fingerprint_preflight(&root, &target_dir);

    // Fixtures whose known IR bug makes the minted value spec-violating: the oracle MUST reject it.
    // (Empirically verified — see this gate's docs and tests/README.md § "IR-bug conformance oracle".)
    //
    // Empty at HEAD — no corpus fixture currently mints a spec-violating value. The machinery stays
    // fully armed: the moment a new IR-level bug lands (a bound or member computed wrong at parse
    // time), its fixture's minted bytes will fail the oracle and this gate turns RED, at which point
    // the fixture is added here. The last resident was `exclusive_range` (`[v: 0...10]` computed the
    // exclusive upper bound as max=b+1 instead of b-1, minting v=11 when the spec max valid is 9);
    // it was removed when parsing.rs was corrected to `range_end - 1` and the oracle stopped
    // rejecting its now-in-spec minted value.
    const EXPECTED_FAIL: &[&str] = &[];

    // Two DIFFERENT kinds of exclusion, deliberately split so a RUST-validator gap doesn't also cost
    // the decorrelated ruby oracle its coverage (they are independent validators — one's blind spot
    // is not the other's).
    //
    // GEN_SKIP: genuinely can't be generated/compiled standalone, so it's skipped ENTIRELY (no
    // generation, no dump, no sweep of any kind).
    //   - dsl_custom: references user-supplied @custom_serialize fns; can't compile standalone (same
    //     reason feature_corpus_compiles skips it).
    const GEN_SKIP: &[&str] = &["dsl_custom"];
    // RUST_ORACLE_SKIP: fixtures whose minted bytes are valid but hit a documented RUST conformance
    // validator gap. Such fixtures still generate, round-trip, and dump, but are generated WITHOUT
    // --emit-tests-conformance (rust validate half off) so the decorrelated ruby gem can continue
    // judging them. A rust-validator gap must not blind the second oracle. Currently empty.
    //   (cbor_bignint_table is a PAST resident: the validator rejected ANY bignint-KEYED map
    //   wholesale — an over-rejection isolated to the key-domain position — until the fork's
    //   bignum-key fix (`local-fixes` @ 4e39d09, which also makes bignum tags ENFORCED in value
    //   position: `bignint` accepts tag 3 only, `biguint` tag 2 only, per the RFC 8610 prelude).
    //   Its ruby half remains unjudgeable — the gem's inline composite control-arg parse gap,
    //   exit 65 — so its minted rules still ride RUBY_EXPECTED_FAIL below.)
    //   (sized_int is a PAST resident, off the list twice over: its negative-lower-bound range
    //   `i_8: -128..127` stopped being a validator gap at the fork's `885c61c` non-uint-range fix,
    //   and its `i_64: int .size 8` member — which the rust validator hard-errors on, an
    //   over-rejection gap per the RFC author's cbor-wg/cddl#32 clarification — was dropped from
    //   the fixture when cddl-codegen made `int .size N` a graceful rejection (the old i{8N}
    //   mapping mis-enforced the clarified uint-window semantics in both directions; scoreboard in
    //   draft/cddl-size-on-int-divergence.md). If upstream ships the per-value semantics and
    //   cddl-codegen supports the construct, its fixture re-grows the member — possibly back onto
    //   this list until the fork fix lands.)
    const RUST_ORACLE_SKIP: &[&str] = &[];

    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    // ===== DECORRELATED (ruby `cddl` gem) conformance oracle ==========================================
    // The rust oracle above shares the dcSpark `cddl` FORK's parser with the generator, so a fork-level
    // grammar/AST MISPARSE corrupts generator IR and oracle spec-interpretation identically and passes
    // green (see this gate's `Scope` note + `deser_test_conformance.rs`). This second sweep re-validates
    // the SAME minted bytes through the ruby `cddl` gem — the RFC author's reference tool, sharing no
    // parser, decoder, language, or lineage with the fork — so a fork misparse that mints
    // well-formed-but-spec-wrong bytes is caught here even when the rust oracle can't see it. The gem is
    // HARNESS-SIDE ONLY (never a crate dep): the `--emit-tests` dump hook (`CDDL_CODEGEN_DUMP_MINTED`,
    // src/emit_tests.rs `roundtrip_body`) writes each minted case to `<rule>__case<i>.cbor`, and this
    // gate sweeps those files.
    //
    // GEM CLI SEMANTICS (probed at implementation time against cddl 0.12.14; do not assume across major
    // bumps):
    //   invocation : `<gem> <spec.cddl> validate <instance.cbor>`  (extension-sensitive: `.cbor` = CBOR)
    //   targeting  : validates the spec's FIRST rule only — so we prepend a synthetic
    //                `__cddl_oracle_root = <rule>` root (same trick as `cddl_oracle_rooted`) to aim it at
    //                any rule while resolving the rest of the spec.
    //   exit codes : 0 = conforms · 1 = validation failure OR malformed/undecodable CBOR · 65 = the gem's
    //                parser rejected the SPEC. We gate on EXIT CODE (0 vs nonzero); a `*** Unused rule`
    //                line the gem prints to stderr on the synthetic root is harmless noise, never an exit.
    //
    // RUBY_EXPECTED_FAIL: (fixture, RULE, reason) triples the gem diverges on for a documented,
    // non-bug reason — a gem construct gap (its parser/validator lacks something the fork legitimately
    // supports; exit 1 or 65 on that rule's cases). Ledgering is PER (fixture, rule), not per fixture:
    // a fixture can have one rule the gem legitimately can't judge while its OTHER rules must still be
    // sound — an unledgered divergence on a sibling rule of a ledgered fixture is a real failure, not
    // swallowed. A DIVERGENCE IS SIGNAL: an unledgered one is either a gem gap to add here WITH a
    // reason, or — the class this whole oracle exists to catch — a fork misparse minting spec-violating
    // bytes. Investigate before ledgering. A ledgered (fixture, rule) that STOPS diverging (all its
    // cases accepted) while still being swept is flagged stale, like the rust oracle's fixed_or_toothless.
    const RUBY_EXPECTED_FAIL: &[(&str, &str, &str)] = &[
        (
            "cbor_wrapped_group_array",
            "holder",
            "gem PARSER gap (cddl 0.12.14): a control operator whose controller is an inline composite \
             type2 (`bytes .cbor [coords]` — equally `{…}` / `~ref`) is a parse error (exit 65), though \
             the same construct via a named ref parses fine; the spec-parse failure poisons EVERY rule \
             in the fixture, so the innocent sibling rule `holder`'s spec-valid `[[0, 0]]` case is \
             rejected without being judged. Repro + upstream steps: \
             draft/ruby-cddl-inline-composite-control-arg-gap.md",
        ),
        (
            "cbor_bignint_table",
            "holder",
            "same gem PARSER gap as cbor_wrapped_group_array's entry, `{…}` flavor: the fixture's \
             `bytes .cbor { * bignint => uint }` inline-map controller is a parse error (exit 65, \
             verified against gem 0.12.14 directly), so the spec-parse failure poisons `holder`'s \
             spec-valid empty-table case without judging it. (The fixture's RUST oracle half is back \
             on since the fork's bignum-key fix — 4e39d09 — so only the ruby half is blind here.) \
             Repro + upstream steps: \
             draft/ruby-cddl-inline-composite-control-arg-gap.md",
        ),
    ];

    // Vacuity floor on total cases the gem actually validated across the corpus. 70 swept at landing;
    // floor kept well below that (same loose-headroom convention as `validated_fixtures`, 20 of 33) so
    // ordinary minter drift doesn't false-fail, while a dump hook that silently stops firing or a sweep
    // that reads an empty dir still fails the gate rather than passing a no-op oracle.
    const RUBY_CASE_FLOOR: usize = 50;

    // Vacuity floor on cases the decode-side reference-codec differential checked. It sweeps the SAME
    // dumped files as the ruby oracle but never rides the `CDDL_RUBY_ORACLE=skip` opt-out (no external
    // dependency), so it gets its own floor with the same loose-headroom convention — a dump hook that
    // silently stops firing (or an empty sweep) fails rather than passing a no-op structural check.
    const DIFF_CASE_FLOOR: usize = 50;

    // DUMP_EXEMPT: (fixture, RULE, reason) triples where a rule the emitter INTENDED to dump (its
    // dump hook is present in lib.rs) legitimately produced no `.cbor` on disk. The per-fixture
    // dump-coverage check below fails the gate on any UN-exempt shortfall, so a dump hook that
    // silently stops firing (or a lossy rule name that drops a top-level rule from the sweep — the
    // very thing the source-rule-name recovery fixed) is visible per fixture, not only via the
    // corpus-wide RUBY_CASE_FLOOR. Empty at HEAD: source rule names are always recoverable, so every
    // dumping test dumps.
    const DUMP_EXEMPT: &[(&str, &str, &str)] = &[];

    let corpus_stems: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap())
        .collect();
    for stem in EXPECTED_FAIL {
        assert!(
            corpus_stems.contains(stem),
            "EXPECTED_FAIL names corpus fixture `{stem}` that no longer exists in tests/corpus — \
             stale pin, remove or fix it"
        );
    }
    for stem in GEN_SKIP {
        assert!(
            corpus_stems.contains(stem),
            "GEN_SKIP names corpus fixture `{stem}` that no longer exists in tests/corpus — stale pin, \
             remove or fix it"
        );
    }
    for stem in RUST_ORACLE_SKIP {
        assert!(
            corpus_stems.contains(stem),
            "RUST_ORACLE_SKIP names corpus fixture `{stem}` that no longer exists in tests/corpus — \
             stale pin, remove or fix it"
        );
    }

    let rule_defined = |stem: &str, rule: &str| {
        let path = corpus_dir.join(format!("{stem}.cddl"));
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("cannot read corpus fixture {path:?}: {e}"));
        src.lines().any(|line| {
            let Some(rest) = line.trim_start().strip_prefix(rule) else {
                return false;
            };
            let rest = rest.trim_start();
            // Ledger semantics name plain rule definitions; allow the CDDL extension forms too so
            // a split rule remains a live target.
            rest.starts_with('=') || rest.starts_with("/=") || rest.starts_with("//=")
        })
    };
    for (stem, rule, _) in RUBY_EXPECTED_FAIL {
        assert!(
            corpus_stems.contains(stem),
            "RUBY_EXPECTED_FAIL names corpus fixture `{stem}` that no longer exists in tests/corpus \
             — stale pin, remove or fix it"
        );
        assert!(
            rule_defined(stem, rule),
            "RUBY_EXPECTED_FAIL names rule `{rule}` in `{stem}` but that rule is no longer defined \
             there — stale pin, remove or fix it"
        );
    }
    for (stem, rule, _) in DUMP_EXEMPT {
        assert!(
            corpus_stems.contains(stem),
            "DUMP_EXEMPT names corpus fixture `{stem}` that no longer exists in tests/corpus — \
             stale pin, remove or fix it"
        );
        assert!(
            rule_defined(stem, rule),
            "DUMP_EXEMPT names rule `{rule}` in `{stem}` but that rule is no longer defined there — \
             stale pin, remove or fix it"
        );
    }

    let conformance_helpers = std::fs::read_to_string("tests/deser_test_conformance.rs").unwrap();

    // The oracle's distinctive panic message (assert_cddl_conforms) — proves an expected-fail
    // fixture failed *for the right reason*, not via some unrelated compile/test break.
    const ORACLE_MSG: &str = "cddl conformance failed for rule";

    let ruby_gem = resolve_ruby_cddl();
    // F7 posture: the decorrelated oracle must not silently, permanently degrade to a no-op just
    // because the gem isn't installed on some machine. Gem absent => this gate FAILS with install
    // instructions, UNLESS the operator explicitly opts out via CDDL_RUBY_ORACLE=skip (which prints a
    // grep-stable SKIPPED marker at the end and runs only the rust half). Documented in
    // tests/README.md and the check.ts gate description.
    let ruby_opt_out = std::env::var("CDDL_RUBY_ORACLE").ok().as_deref() == Some("skip");
    if ruby_gem.is_none() && !ruby_opt_out {
        panic!(
            "RUBY ORACLE REQUIRED but the ruby `cddl` gem was not found. The decorrelated (fork-\
             misparse) conformance oracle cannot run without it. Install it with `gem install \
             --user-install cddl` (or set RUBY_CDDL to a ruby cddl binary). To run this gate WITHOUT \
             the decorrelated oracle — accepting that the fork-misparse class goes uncovered — set \
             CDDL_RUBY_ORACLE=skip."
        );
    }
    let mut ruby_total_cases = 0usize;
    let mut ruby_failures: Vec<String> = vec![]; // unledgered divergences (rule/case/hex/stderr)
    // Per (fixture, rule) sweep bookkeeping for the per-rule ledger + stale check (F4).
    let mut ruby_seen_rules: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new(); // (fixture, rule) pairs with >= 1 case actually swept
    let mut ruby_diverged_rules: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new(); // (fixture, rule) pairs with >= 1 divergence
    // First known-good (rooted-spec, valid-bytes) pair, for the post-sweep negative control.
    let mut ruby_neg_sample: Option<(String, String, Vec<u8>)> = None;

    let mut dump_coverage_failures: Vec<String> = vec![]; // F5: intended-but-undumped rules
    let mut failures = vec![];
    let mut fixed_or_toothless = vec![]; // EXPECTED_FAIL fixtures that unexpectedly passed
    let mut validated_fixtures = 0usize; // vacuity floor: fixtures that actually emitted a conformance call

    // Decode-side reference-codec differential (see `reference_codec_differential`): a CDDL-blind,
    // dependency-free structural cross-check that piggybacks on the same dumped `.cbor` files as the
    // ruby sweep. It runs regardless of the ruby gem (even under CDDL_RUBY_ORACLE=skip) and for
    // RUST_ORACLE_SKIP fixtures — a spec-validator blind spot must not cost the structural oracle its
    // coverage. Failures go into the shared `failures` vec; `diff_total_cases` feeds its own floor and
    // `diff_neg_sample` seeds an anti-vacuity truncation control after the sweep.
    let mut diff_total_cases = 0usize;
    let mut diff_neg_sample: Option<(String, Vec<u8>)> = None; // (label, valid bytes) to truncate
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        if GEN_SKIP.contains(&stem) {
            continue;
        }
        // rust_oracle: emit the rust `cddl_conformance::validate` half? Off for RUST_ORACLE_SKIP
        // fixtures (rust-validator gap) — they still generate, round-trip, dump, and ruby-sweep.
        let rust_oracle = !RUST_ORACLE_SKIP.contains(&stem);
        let expected_fail = EXPECTED_FAIL.contains(&stem);
        // An EXPECTED_FAIL fixture is judged by the rust oracle's message, so it must have that
        // oracle on — the two lists must stay disjoint.
        assert!(
            !expected_fail || rust_oracle,
            "{stem} is on both EXPECTED_FAIL and RUST_ORACLE_SKIP — an expected-fail fixture needs \
             the rust conformance oracle to fail for the right reason"
        );
        let out = root.join(stem);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=false")
            .arg("--emit-tests=true")
            .arg(format!("--emit-tests-conformance={rust_oracle}"))
            .output()
            .unwrap();
        if !gen_out.status.success() {
            failures.push(format!(
                "{stem}: generation failed\n{}",
                String::from_utf8_lossy(&gen_out.stderr)
            ));
            continue;
        }
        let rust_dir = out.join("rust");
        let lib_rs_path = rust_dir.join("src/lib.rs");
        // Only the rust-oracle half needs the shared validator helpers, the source-spec copy, and the
        // cddl crate dependency. A RUST_ORACLE_SKIP fixture is compiled WITHOUT them (it emits no
        // `cddl_conformance::validate` call) but STILL dumps its minted bytes for the ruby sweep.
        if rust_oracle {
            // wire in the shared oracle helpers (cddl_oracle_load_spec / assert_cddl_conforms) that the
            // emitted cddl_conformance::validate calls resolve to. They resolve as `crate::…`
            // (emit_tests.rs), so they belong at the crate root `lib.rs` (which seed-once preserves) —
            // NOT under `generated/`. The emitted validate CALLS themselves live in `generated/mod.rs`.
            let mut lib_rs = std::fs::OpenOptions::new()
                .append(true)
                .open(&lib_rs_path)
                .unwrap();
            lib_rs.write_all(b"\n\n").unwrap();
            lib_rs.write_all(conformance_helpers.as_bytes()).unwrap();
            std::mem::drop(lib_rs);
            // the emitted validate() reads the spec from `cddl_conformance_source.cddl` next to the
            // crate's Cargo.toml (CARGO_MANIFEST_DIR) — copy the fixture there.
            std::fs::copy(input, rust_dir.join("cddl_conformance_source.cddl")).unwrap();
            // add the cddl dep (rev-pinned, synced with Cargo.toml by cddl_oracle_dep_rev_matches_cargo_toml)
            let mut cargo_toml = std::fs::OpenOptions::new()
                .append(true)
                .open(rust_dir.join("Cargo.toml"))
                .unwrap();
            cargo_toml.write_all(CDDL_ORACLE_DEP.as_bytes()).unwrap();
            std::mem::drop(cargo_toml);
        }

        // The emitted generated-test module (and its `cddl_conformance::validate(..)` calls) lives in
        // the generated root, `generated/mod.rs`, not the thin seed-once `lib.rs`.
        let lib_src = std::fs::read_to_string(rust_dir.join("src/generated/mod.rs")).unwrap();
        // vacuity: did this fixture actually emit any conformance call? (a fixture whose only
        // round-trip types are transparent array/table aliases emits none — see occurrence). Only
        // meaningful for rust-oracle fixtures; a RUST_ORACLE_SKIP fixture emits none by design.
        if rust_oracle {
            if lib_src.contains("cddl_conformance::validate(") {
                validated_fixtures += 1;
            } else if expected_fail {
                // an expected-fail fixture that emits no conformance call can never fail for the right
                // reason — the list is wrong.
                failures.push(format!(
                    "{stem}: on EXPECTED_FAIL but emitted no conformance call (nothing to validate) — \
                     the list is stale"
                ));
                continue;
            }
        }

        // Per-fixture minted-bytes dump dir for the ruby sweep. Set unconditionally (harmless when the
        // gem is absent); the emitted dump hook fires whenever this env var points somewhere.
        let dump_dir = out.join("__minted_dump");
        let _ = std::fs::create_dir_all(&dump_dir);
        let test = tool_cmd("cargo")
            .arg("test")
            .current_dir(&rust_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .env("CDDL_CODEGEN_DUMP_MINTED", &dump_dir)
            .output()
            .unwrap();
        let combined = format!(
            "{}\n{}",
            String::from_utf8_lossy(&test.stdout),
            String::from_utf8_lossy(&test.stderr)
        );
        let passed = test.status.success();
        match (expected_fail, passed) {
            (true, true) => fixed_or_toothless.push(format!(
                "{stem}: EXPECTED_FAIL fixture PASSED conformance — IR bug apparently fixed or the \
                 oracle lost teeth. Investigate, then remove it from EXPECTED_FAIL."
            )),
            (true, false) => {
                if !combined.contains(ORACLE_MSG) {
                    failures.push(format!(
                        "{stem}: EXPECTED_FAIL fixture failed, but NOT via the conformance oracle \
                         (missing `{ORACLE_MSG}`) — it broke for an unrelated reason:\n{combined}"
                    ));
                }
            }
            (false, true) => {} // green as expected
            (false, false) => failures.push(format!(
                "{stem}: conformance FAILED for a fixture not on EXPECTED_FAIL — either a new \
                 IR-level miscompile (mints spec-violating bytes), a round-trip failure, or a \
                 validator gap to document + add to RUST_ORACLE_SKIP:\n{combined}"
            )),
        }

        // The dumped rules actually on disk (a fixture only dumps once its tests RUN, i.e. `passed`).
        let dumped_on_disk: std::collections::BTreeSet<String> = std::fs::read_dir(&dump_dir)
            .map(|rd| {
                rd.filter_map(|e| e.ok().map(|e| e.path()))
                    .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cbor"))
                    .filter_map(|p| {
                        p.file_name()
                            .and_then(|f| f.to_str())
                            .and_then(|f| f.rsplit_once("__case").map(|(r, _)| r.to_owned()))
                    })
                    .collect()
            })
            .unwrap_or_default();

        // F5 dump-coverage: every rule the emitter INTENDED to dump (its hook is present in lib.rs)
        // must have landed a `.cbor` on disk — otherwise a hook silently stopped firing (or a
        // top-level rule was dropped from the sweep by a lossy name). Only checked when the fixture's
        // tests passed (a failed suite legitimately dumps nothing). The INTENDED set is derived from
        // the dump-hook format strings `{__dump_dir}/<rule>__case…` the generator emits, so it counts
        // exactly the types with a recoverable source rule (synthesized non-top-level structs emit no
        // hook and are correctly excluded — e.g. prelude's `prelude_*` wrappers).
        if passed && !expected_fail {
            let intended: std::collections::BTreeSet<String> = lib_src
                .match_indices("{__dump_dir}/")
                .filter_map(|(i, m)| {
                    let rest = &lib_src[i + m.len()..];
                    rest.find("__case").map(|j| rest[..j].to_owned())
                })
                .collect();
            for rule in intended.difference(&dumped_on_disk) {
                if DUMP_EXEMPT.iter().any(|(s, r, _)| *s == stem && r == rule) {
                    continue;
                }
                dump_coverage_failures.push(format!(
                    "{stem} / rule `{rule}`: a dump hook for this rule is emitted in lib.rs but no \
                     `{rule}__case*.cbor` was written — the hook silently stopped firing (or the \
                     rule was dropped from the sweep). Fix the dump, or ledger it in DUMP_EXEMPT WITH \
                     a justification."
                ));
            }
            // stale DUMP_EXEMPT: an entry whose rule DID dump no longer needs exempting.
            for (s, r, _) in DUMP_EXEMPT.iter().filter(|(s, _, _)| *s == stem) {
                if dumped_on_disk.contains(*r) {
                    dump_coverage_failures.push(format!(
                        "{s} / rule `{r}`: on DUMP_EXEMPT but the rule now dumps — remove the stale exemption."
                    ));
                }
            }
        }

        // --- decode-side reference-codec differential (CDDL-blind, dependency-free). Independent of
        // the ruby gem: it sweeps the SAME dumped `.cbor` files and decodes each through ciborium +
        // minicbor, requiring both to fully consume the bytes and agree on the structure. Runs for
        // every fixture whose tests passed (RUST_ORACLE_SKIP included), even under
        // CDDL_RUBY_ORACLE=skip. Sorted order for stable diagnostics.
        if passed && !expected_fail {
            let mut cases: Vec<std::path::PathBuf> = std::fs::read_dir(&dump_dir)
                .map(|rd| {
                    rd.filter_map(|e| e.ok().map(|e| e.path()))
                        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cbor"))
                        .collect()
                })
                .unwrap_or_default();
            cases.sort();
            for case in &cases {
                let fname = case.file_name().unwrap().to_str().unwrap().to_owned();
                let bytes = std::fs::read(case).unwrap();
                diff_total_cases += 1;
                if let Err(e) = reference_codec_differential(&bytes) {
                    failures.push(format!(
                        "{stem} / {fname}: reference-codec differential: {e}\n  bytes: {bytes:02x?}"
                    ));
                }
                // Seed the post-sweep truncation control from the first non-trivial case.
                if diff_neg_sample.is_none() && bytes.len() > 1 {
                    diff_neg_sample = Some((format!("{stem}/{fname}"), bytes));
                }
            }
        }

        // --- ruby decorrelated sweep (only when the gem is present and the fixture's tests passed:
        // we re-check the SAME minted bytes through a lineage-decorrelated parser). Sweep the dump dir
        // in SORTED order for stable, reproducible diagnostics. Ledgering is PER (fixture, rule).
        if let Some(gem) = &ruby_gem
            && passed
            && !expected_fail
        {
            let source = std::fs::read_to_string(input).unwrap();
            let mut cases: Vec<std::path::PathBuf> = std::fs::read_dir(&dump_dir)
                .map(|rd| {
                    rd.filter_map(|e| e.ok().map(|e| e.path()))
                        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cbor"))
                        .collect()
                })
                .unwrap_or_default();
            cases.sort();
            let mut rooted_specs: std::collections::BTreeMap<String, std::path::PathBuf> =
                std::collections::BTreeMap::new();
            for case in &cases {
                let fname = case.file_name().unwrap().to_str().unwrap().to_owned();
                // `<rule>__case<i>.cbor` -> rule is everything before the LAST `__case`.
                let rule = fname
                    .rsplit_once("__case")
                    .map(|(r, _)| r)
                    .unwrap_or(&fname)
                    .to_owned();
                ruby_seen_rules.insert((stem.to_owned(), rule.clone()));
                // rooted synthetic-root spec (cached per rule), aiming the gem's first-rule validation
                // at `rule` while resolving the rest of the fixture's references.
                let rooted_path = rooted_specs.entry(rule.clone()).or_insert_with(|| {
                    let p = dump_dir.join(format!("__rooted_{rule}.cddl"));
                    std::fs::write(&p, format!("__cddl_oracle_root = {rule}\n\n{source}")).unwrap();
                    p
                });
                let bytes = std::fs::read(case).unwrap();
                let gem_out = std::process::Command::new(gem)
                    .arg(&*rooted_path)
                    .arg("validate")
                    .arg(case)
                    .output()
                    .unwrap();
                ruby_total_cases += 1;
                if gem_out.status.success() {
                    if ruby_neg_sample.is_none() && bytes.len() > 1 {
                        ruby_neg_sample = Some((
                            rule.clone(),
                            format!("__cddl_oracle_root = {rule}\n\n{source}"),
                            bytes.clone(),
                        ));
                    }
                } else {
                    ruby_diverged_rules.insert((stem.to_owned(), rule.clone()));
                    // Per (fixture, rule) ledger: a divergence on an UN-ledgered rule is a real
                    // failure even if a SIBLING rule of the same fixture is ledgered.
                    let ledgered = RUBY_EXPECTED_FAIL
                        .iter()
                        .any(|(s, r, _)| *s == stem && *r == rule);
                    if !ledgered {
                        ruby_failures.push(format!(
                            "{stem} / rule `{rule}` / {fname}: ruby gem REJECTED (exit {})\n  bytes: {bytes:02x?}\n  gem stderr:\n{}",
                            gem_out.status.code().map(|c| c.to_string()).unwrap_or_else(|| "signal".into()),
                            String::from_utf8_lossy(&gem_out.stderr).trim()
                        ));
                    }
                }
            }
        }
    }

    // Per (fixture, rule) stale-ledger check (F4): a ledgered pair that WAS swept but never diverged
    // means the gem gap it records is gone — flag it stale, like the rust oracle's fixed_or_toothless.
    let ruby_ledger_stale: Vec<String> = RUBY_EXPECTED_FAIL
        .iter()
        .filter(|(s, r, _)| {
            let pair = ((*s).to_owned(), (*r).to_owned());
            ruby_seen_rules.contains(&pair) && !ruby_diverged_rules.contains(&pair)
        })
        .map(|(s, r, _)| format!("{s} / rule `{r}`"))
        .collect();

    // Negative control BEFORE the scratch tree is removed: corrupt a known-good case (truncate the
    // final byte -> guaranteed malformed CBOR) and require the gem to REJECT it. A gem invocation that
    // exits 0 regardless of input (wrong CLI shape / wrong arg) would make the whole sweep vacuous.
    let ruby_neg_control_ok: Option<bool> =
        if let (Some(gem), Some((_, rooted_src, good_bytes))) = (&ruby_gem, &ruby_neg_sample) {
            let rooted_path = root.join("__ruby_neg_rooted.cddl");
            let corrupt_path = root.join("__ruby_neg_control.cbor");
            std::fs::write(&rooted_path, rooted_src).unwrap();
            std::fs::write(&corrupt_path, &good_bytes[..good_bytes.len() - 1]).unwrap();
            let nc = std::process::Command::new(gem)
                .arg(&rooted_path)
                .arg("validate")
                .arg(&corrupt_path)
                .output()
                .unwrap();
            Some(!nc.status.success()) // true = correctly rejected
        } else {
            None
        };

    // Decode-side differential negative control (in-memory, no external dependency): truncating a
    // known-good case's final byte yields an incomplete item both codecs must reject, so the
    // differential must return Err. A structural check that accepted anything would make every PASS
    // above vacuous. Mirrors the ruby negative control at the codec level; runs even under skip.
    let diff_neg_control_ok: Option<bool> = diff_neg_sample.as_ref().map(|(_, good_bytes)| {
        reference_codec_differential(&good_bytes[..good_bytes.len() - 1]).is_err()
    });

    let _ = std::fs::remove_dir_all(&root);
    // vacuity floor: a silent no-op sweep (nothing validated) must not pass. 33 of the 39 non-skip
    // corpus fixtures emit at least one conformance call at landing (the rest are transparent
    // array/table aliases / pure c-enums / extern-only). Floor kept below that for minter headroom.
    assert!(
        validated_fixtures >= 20,
        "only {validated_fixtures} corpus fixtures emitted a conformance call (expected >= 20) — \
         the oracle went vacuous (minter coverage shrank or the flag stopped emitting calls)"
    );
    assert!(
        fixed_or_toothless.is_empty(),
        "EXPECTED_FAIL fixtures no longer fail conformance:\n\n{}",
        fixed_or_toothless.join("\n\n")
    );
    assert!(
        failures.is_empty(),
        "IR-conformance gate failures:\n\n{}",
        failures.join("\n\n")
    );
    // F5 dump-coverage: an intended-but-undumped rule (or a stale DUMP_EXEMPT) fails the gate,
    // independent of whether the gem ran — this catches an emit-side dump-hook regression per fixture.
    assert!(
        dump_coverage_failures.is_empty(),
        "dump-coverage shortfall (a rule the generator intended to dump produced no bytes):\n\n{}",
        dump_coverage_failures.join("\n\n")
    );

    // ===== decode-side reference-codec differential verdicts (dependency-free — always gated) =====
    // (Structural divergences already went into `failures` above; here are its own anti-vacuity teeth.)
    eprintln!(
        "REFERENCE CODEC DIFFERENTIAL: checked {diff_total_cases} minted cases (ciborium vs minicbor)"
    );
    assert!(
        diff_total_cases >= DIFF_CASE_FLOOR,
        "reference-codec differential checked only {diff_total_cases} minted cases (floor \
         {DIFF_CASE_FLOOR}) — the dump hook silently stopped firing or the sweep read an empty dir \
         (vacuous structural oracle)"
    );
    assert_eq!(
        diff_neg_control_ok,
        Some(true),
        "reference-codec differential NEGATIVE CONTROL failed: truncated (malformed) bytes were not \
         rejected by both codecs, so the differential can't tell well-formed from malformed and every \
         structural PASS above is vacuous (or no known-good sample was captured to corrupt)"
    );

    // ===== ruby decorrelated-oracle verdicts =====
    match &ruby_gem {
        // Reached ONLY under the explicit CDDL_RUBY_ORACLE=skip opt-out (gem-absent without opt-out
        // panicked at the top). Prints a grep-stable SKIPPED marker so check.ts can surface it.
        None => eprintln!(
            "RUBY ORACLE: SKIPPED (CDDL_RUBY_ORACLE=skip). The rust conformance + dump-coverage \
             halves above still gated; the lineage-decorrelated (fork-misparse) half did not run."
        ),
        Some(gem) => {
            eprintln!(
                "RUBY ORACLE: swept {ruby_total_cases} minted cases through the ruby cddl gem at {}",
                gem.display()
            );
            assert!(
                ruby_total_cases >= RUBY_CASE_FLOOR,
                "ruby oracle swept only {ruby_total_cases} minted cases (floor {RUBY_CASE_FLOOR}) — \
                 the dump hook silently stopped firing or the sweep read an empty dir (vacuous oracle)"
            );
            assert_eq!(
                ruby_neg_control_ok,
                Some(true),
                "ruby oracle NEGATIVE CONTROL failed: the gem did not reject truncated (malformed) \
                 bytes, so its invocation shape can't tell valid from invalid and every PASS above is \
                 vacuous (or no known-good sample was captured to corrupt)"
            );
            assert!(
                ruby_ledger_stale.is_empty(),
                "RUBY_EXPECTED_FAIL (fixture, rule) entries no longer diverge from the gem (remove \
                 them — the gem gap they recorded is gone):\n{}",
                ruby_ledger_stale.join("\n")
            );
            assert!(
                ruby_failures.is_empty(),
                "RUBY ORACLE divergences — each is EITHER a gem construct gap to add to \
                 RUBY_EXPECTED_FAIL WITH a justification, OR (the class this oracle exists to catch) a \
                 fork misparse minting spec-violating bytes the rust oracle can't see. Investigate \
                 before ledgering:\n\n{}",
                ruby_failures.join("\n\n")
            );
        }
    }
}

#[test]
fn canonical() {
    // `--emit-tests=true` is the one place the canonical differential runs: the emitted round-trip
    // suite's encoding-fidelity block asserts every irregular re-encoding canonicalizes to the same
    // bytes (encoding-invariance) plus a per-case canonical fixed point. Consistent with
    // `src/tests/mod.rs`'s policy that canonical is covered once at whole-program scale rather than
    // as a fourth corpus profile. This fixture has a `tests_wasm.rs` and doesn't pass `--wasm=false`,
    // so `run_test` also `cargo test`s the emitted *wasm* test module under canonical-form.
    run_test(
        "canonical",
        &[
            "--preserve-encodings=true",
            "--canonical-form=true",
            "--emit-tests=true",
        ],
        None,
        &[],
        &[],
        false,
        &[],
    );
}

#[test]
fn rust_wasm_split() {
    run_test("rust-wasm-split", &[], None, &[], &[], false, &[]);
}

#[test]
fn multifile() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_defs");
    let extern_wasm_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_wasm_defs");
    // this tests without preserve-encodings as that can affect imports
    run_test(
        "multifile",
        &[],
        None,
        &[extern_rust_path],
        &[extern_wasm_path],
        true,
        &["hex = \"0.4.3\""],
    );
}

#[test]
fn multifile_json_preserve() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_defs_compiles_with_json_preserve");
    let extern_wasm_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_wasm_defs");
    // json-schema-export / preserve-encodings to ensure that imports/scoping works in both:
    // 1) cbor_encodings.rs
    // 2) json-gen schema export crate
    run_test(
        "multifile",
        &[
            "--lib-name=multi-chain-test",
            "--preserve-encodings=true",
            "--json-serde-derives=true",
            "--json-schema-export=true",
        ],
        Some("json_preserve"),
        &[extern_rust_path],
        &[extern_wasm_path],
        true,
        &[],
    );
}

#[test]
fn raw_bytes() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_raw_bytes_def");
    let extern_wasm_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_wasm_raw_bytes_def");
    run_test(
        "raw-bytes",
        &[],
        None,
        &[extern_rust_path],
        &[extern_wasm_path],
        false,
        &[],
    );
}

#[test]
fn raw_bytes_preserve() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_rust_raw_bytes_def");
    let extern_wasm_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_wasm_raw_bytes_def");
    run_test(
        "raw-bytes-preserve",
        &["--preserve-encodings=true"],
        None,
        &[extern_rust_path],
        &[extern_wasm_path],
        false,
        &[],
    );
}

#[test]
fn json() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_json_impls");
    run_test(
        "json",
        &["--json-serde-derives=true", "--json-schema-export=true"],
        None,
        &[extern_rust_path],
        &[],
        false,
        // schemas_validate_serialization (tests.rs) checks emitted output against emitted schema
        &["jsonschema = { version = \"0.46\", default-features = false }"],
    );
}

/// Float JSON serde/schema, split from `json` because that fixture also runs under
/// `json_preserve` and preserve-encodings is unimplemented for floats.
#[test]
fn json_float() {
    run_test(
        "json-float",
        &["--json-serde-derives=true", "--json-schema-export=true"],
        None,
        &[],
        &[],
        false,
        &["jsonschema = { version = \"0.46\", default-features = false }"],
    );
}

/// Builds the generated wasm bindings with wasm-pack and runs them under node (see the
/// `roundtrip.mjs` hook in `run_test`). Regression test for the serde-wasm-bindgen JSON-shape
/// contract: a CDDL map must come back from `to_json_value()` as an object, not a JS `Map`.
#[test]
fn wasm_json_roundtrip() {
    run_test(
        "wasm_json",
        &["--json-serde-derives=true"],
        None,
        &[],
        &[],
        false,
        &[],
    );
}

/// Smoke-tests the schema → `.d.ts` step: runs the shipped `static/run-json2ts.js` over committed
/// schema fixtures using the pinned `json-schema-to-typescript` from `static/package_json_schemas.json`
/// (installed via `npm install` of that exact file), then asserts the emitted types. This is the only
/// coverage of that script + dependency — a bump there is otherwise invisible to CI, since the rest of
/// the suite only `cargo build`s the json-gen crate and never runs the JS. See `tests/json2ts/README.md`
/// and `tests/README.md` § "JSON-schema → TypeScript JS-side pipeline".
#[test]
fn js_schema_to_ts() {
    use std::str::FromStr;
    let static_dir = std::path::PathBuf::from_str("static").unwrap();
    let fixtures = std::path::PathBuf::from_str("tests/json2ts/schemas").unwrap();
    // gitignored (tests/*/export*/), so it's regenerated each run and never committed.
    let work = std::path::PathBuf::from_str("tests/json2ts/export").unwrap();

    if !(tool_exists("node") && tool_exists("npm")) {
        // Don't let CI silently skip the only schema -> .d.ts coverage we have.
        assert!(
            std::env::var_os("CI").is_none(),
            "node and npm are required to run js_schema_to_ts in CI"
        );
        eprintln!("skipping js_schema_to_ts: node/npm not found");
        return;
    }

    // Lay out what run-json2ts.js expects relative to its cwd: scripts/, package.json (the shipped
    // one, so npm installs the pinned json2ts), and rust/wasm/json-gen/schemas/*.json.
    let _ = std::fs::remove_dir_all(&work);
    let schemas_out = work.join("rust/wasm/json-gen/schemas");
    std::fs::create_dir_all(work.join("scripts")).unwrap();
    std::fs::create_dir_all(&schemas_out).unwrap();
    std::fs::copy(
        static_dir.join("run-json2ts.js"),
        work.join("scripts/run-json2ts.js"),
    )
    .unwrap();
    std::fs::copy(
        static_dir.join("package_json_schemas.json"),
        work.join("package.json"),
    )
    .unwrap();
    for entry in std::fs::read_dir(&fixtures).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|e| e.to_str()) == Some("json") {
            std::fs::copy(&path, schemas_out.join(path.file_name().unwrap())).unwrap();
        }
    }

    let npm = std::process::Command::new("npm")
        .args(["install", "--silent", "--no-audit", "--no-fund"])
        .current_dir(&work)
        .output()
        .unwrap();
    if !npm.status.success() {
        eprintln!(
            "npm install stderr:\n{}",
            String::from_utf8_lossy(&npm.stderr)
        );
    }
    assert!(npm.status.success());

    let node = std::process::Command::new("node")
        .arg("scripts/run-json2ts.js")
        .current_dir(&work)
        .output()
        .unwrap();
    if !node.status.success() {
        eprintln!("node stderr:\n{}", String::from_utf8_lossy(&node.stderr));
    }
    assert!(node.status.success());

    let dts =
        std::fs::read_to_string(work.join("rust/wasm/json-gen/output/json-types.d.ts")).unwrap();
    println!("generated json-types.d.ts:\n{dts}");
    // Identifiers are JSON-suffixed; the cross-file ref resolved (bar -> BarJSON); the enum became a
    // union.
    assert!(dts.contains("export interface FooJSON"), "{dts}");
    assert!(dts.contains("export type BarJSON"), "{dts}");
    assert!(dts.contains("bar: BarJSON"), "{dts}");
    assert!(dts.contains("\"x\" | \"y\""), "{dts}");
    // additionalProperties guard, both sides: injected `false` on the struct dropped its index
    // signature, but the map type's existing `additionalProperties` object was kept (Table.json).
    let foo_block = {
        let start = dts.find("export interface FooJSON").unwrap();
        &dts[start..start + dts[start..].find('}').unwrap()]
    };
    assert!(!foo_block.contains("[k: string]"), "{foo_block}");
    assert!(dts.contains("export interface TableJSON"), "{dts}");
    assert!(dts.contains("[k: string]: number"), "{dts}");
}

/// Covers the shipped `static/json-ts-types.js` (`tests/README.md` § "JSON-schema → TypeScript
/// JS-side pipeline"), which `--package-json`
/// runs after `run-json2ts.js` to (a) type each wasm class's `to_json_value()` with its emitted JSON
/// interface and (b) append those interfaces to the wasm-pack `.d.ts`. It's pure string-munging over
/// two files, so it's exercised in isolation here (no wasm-pack/json2ts needed) with hand-written
/// fixtures. The script hardcodes the default `cddl_lib_wasm` lib name, so that's what we lay out.
#[test]
fn js_d_ts_merge() {
    use std::str::FromStr;
    let static_dir = std::path::PathBuf::from_str("static").unwrap();
    if !tool_exists("node") {
        assert!(
            std::env::var_os("CI").is_none(),
            "node is required to run js_d_ts_merge in CI"
        );
        eprintln!("skipping js_d_ts_merge: node not found");
        return;
    }
    // gitignored (tests/*/export*/); regenerated each run, distinct from js_schema_to_ts's dir so
    // the two can run concurrently.
    let work = std::path::PathBuf::from_str("tests/json2ts/export_dts").unwrap();
    let _ = std::fs::remove_dir_all(&work);
    let pkg = work.join("rust/wasm/pkg");
    let out = work.join("rust/wasm/json-gen/output");
    std::fs::create_dir_all(&pkg).unwrap();
    std::fs::create_dir_all(&out).unwrap();
    std::fs::copy(
        static_dir.join("json-ts-types.js"),
        work.join("json-ts-types.js"),
    )
    .unwrap();
    // The wasm-pack-shaped .d.ts the script reads: a class whose to_json_value() returns `any`.
    std::fs::write(
        pkg.join("cddl_lib_wasm.d.ts"),
        "export class Foo {\n  free(): void;\n  to_json(): string;\n  to_json_value(): any;\n}\n",
    )
    .unwrap();
    // The json-types.d.ts run-json2ts.js would have emitted.
    std::fs::write(
        out.join("json-types.d.ts"),
        "export interface FooJSON {\n  x: number;\n}\n",
    )
    .unwrap();

    let node = std::process::Command::new("node")
        .arg("json-ts-types.js")
        .current_dir(&work)
        .output()
        .unwrap();
    if !node.status.success() {
        eprintln!("node stderr:\n{}", String::from_utf8_lossy(&node.stderr));
    }
    assert!(node.status.success());

    let merged = std::fs::read_to_string(pkg.join("cddl_lib_wasm.d.ts")).unwrap();
    println!("merged d.ts:\n{merged}");
    // to_json_value()'s `any` return was specialized to the class's JSON interface...
    assert!(merged.contains("to_json_value(): FooJSON;"), "{merged}");
    // ...and the JSON type defs were appended.
    assert!(merged.contains("export interface FooJSON"), "{merged}");
}

/// End-to-end validation of the shipped `--package-json --json-schema-export` consumer pipeline
/// (`generation.rs`'s `export` copy block + `static/package_json_schemas.json`). It generates a small
/// fixture with those flags and runs the SHIPPED `npm run rust:build-nodejs` script VERBATIM in the
/// output dir — `wasm-pack build --target=nodejs` -> `js:ts-json-gen` (json-gen `cargo +stable run`
/// -> `run-json2ts.js` -> `json-ts-types.js`) -> `wasm-pack pack`. Running the script line itself
/// (its `cd`/`;` shell shape, its dependency pins, its `cargo +stable`) is the point: replicating the
/// steps in Rust would let the script rot. This is also the ONLY layer that exercises `#[wasm_bindgen]`
/// macro-expansion -> real wasm-pack `.d.ts` -> the JS-side `.d.ts` merge end-to-end; the systematic
/// wasm gates `cargo check` on the host target and can't see any of it. Each output assert pins one
/// stage of the pipeline actually running (see `tests/README.md` § "package_json_pipeline").
///
/// Note: the script builds the GENERATED crate with the user's `+stable` toolchain, not the repo pin —
/// faithful to the shipped consumer experience, so a `+stable` failure here is a real finding about
/// shipped output, not a test bug to paper over.
#[test]
fn package_json_pipeline() {
    use std::str::FromStr;
    let test_path = std::path::PathBuf::from_str("tests/package-json").unwrap();
    // gitignored (tests/*/export*/); regenerated each run.
    let export = test_path.join("export");

    // The shipped `rust:build-nodejs` needs node+npm+wasm-pack, PLUS a rustup `stable` toolchain (the
    // `js:ts-json-gen` script hardcodes `cargo +stable run`). House pattern: assert in CI (its fast
    // tier never reaches this test, but keep the pattern), eprintln+return locally.
    let rustup_stable = std::process::Command::new("rustup")
        .args(["run", "stable", "cargo", "--version"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !(tool_exists("node") && tool_exists("npm") && tool_exists("wasm-pack") && rustup_stable) {
        assert!(
            std::env::var_os("CI").is_none(),
            "node, npm, wasm-pack and a rustup `stable` toolchain are required to run \
             package_json_pipeline in CI"
        );
        eprintln!(
            "skipping package_json_pipeline: need node+npm+wasm-pack + a rustup `stable` toolchain"
        );
        return;
    }

    // Generate the shipped layout: everything under `export/rust/{rust,wasm}`, plus `export/package.json`
    // and `export/scripts/`.
    let _ = std::fs::remove_dir_all(&export);
    let generate = tool_cmd("cargo")
        .arg("run")
        .arg("--")
        .arg("--input=tests/package-json/input.cddl")
        .arg("--output=tests/package-json/export")
        .arg("--wasm=true")
        .arg("--package-json=true")
        .arg("--json-serde-derives=true")
        .arg("--json-schema-export=true")
        .output()
        .unwrap();
    if !generate.status.success() {
        eprintln!(
            "generate stderr:\n{}",
            String::from_utf8_lossy(&generate.stderr)
        );
    }
    assert!(generate.status.success());

    // Layout sanity: pins `generation.rs`'s `--package-json` copy block (the three shipped files).
    assert!(
        export.join("package.json").exists(),
        "no export/package.json"
    );
    assert!(
        export.join("scripts/run-json2ts.js").exists(),
        "no export/scripts/run-json2ts.js"
    );
    assert!(
        export.join("scripts/json-ts-types.js").exists(),
        "no export/scripts/json-ts-types.js"
    );

    // `npm install` the pinned devDeps (rimraf/cross-env/json-schema-to-typescript), mirroring
    // js_schema_to_ts.
    let npm_install = std::process::Command::new("npm")
        .args(["install", "--silent", "--no-audit", "--no-fund"])
        .current_dir(&export)
        .output()
        .unwrap();
    if !npm_install.status.success() {
        eprintln!(
            "npm install stderr:\n{}",
            String::from_utf8_lossy(&npm_install.stderr)
        );
    }
    assert!(npm_install.status.success());

    // THE assertion: run the shipped script verbatim. Strip RUSTFLAGS for the same reason `tool_cmd`
    // does — the nested `cargo +stable` builds the generated crate, which legitimately over-imports,
    // and CI injects `-D warnings` into the job env. ~20s cold here (wasm-pack build + a small nested
    // cargo build); give the tool an extended timeout.
    let build = std::process::Command::new("npm")
        .args(["run", "rust:build-nodejs"])
        .current_dir(&export)
        .env_remove("RUSTFLAGS")
        .output()
        .unwrap();
    if !build.status.success() {
        eprintln!(
            "rust:build-nodejs stdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
    }
    assert!(build.status.success());

    // Output asserts, each pinning one stage actually ran:
    // wasm-pack build ran -> its `.d.ts` (filename derived from the crate name, the default
    // `cddl_lib_wasm` since we pass no --lib-name).
    let dts_path = export.join("rust/wasm/pkg/cddl_lib_wasm.d.ts");
    assert!(dts_path.exists(), "no wasm-pack .d.ts at {dts_path:?}");
    // json-gen `cargo +stable run` wrote schemas.
    let schemas_dir = export.join("rust/wasm/json-gen/schemas");
    let schema_count = std::fs::read_dir(&schemas_dir)
        .unwrap_or_else(|e| panic!("json-gen wrote no schemas dir {schemas_dir:?}: {e}"))
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("json"))
        .count();
    assert!(
        schema_count > 0,
        "json-gen produced no schema files in {schemas_dir:?}"
    );
    // run-json2ts.js + json-ts-types.js merged the REAL wasm-pack output (not a hand-written fixture):
    // to_json_value()'s `any` got specialized AND the JSON interface got appended.
    let dts = std::fs::read_to_string(&dts_path).unwrap();
    println!("merged wasm-pack d.ts:\n{dts}");
    assert!(dts.contains("to_json_value(): FooJSON;"), "{dts}");
    assert!(dts.contains("export interface FooJSON"), "{dts}");
    // wasm-pack pack ran.
    let has_tgz = std::fs::read_dir(export.join("rust/wasm/pkg"))
        .unwrap()
        .filter_map(Result::ok)
        .any(|e| e.path().extension().and_then(|x| x.to_str()) == Some("tgz"));
    assert!(has_tgz, "wasm-pack pack produced no .tgz in the pkg dir");
}

#[test]
fn json_preserve() {
    use std::str::FromStr;
    let extern_rust_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("external_json_impls");
    run_test(
        "json",
        &[
            "--preserve-encodings=true",
            "--json-serde-derives=true",
            "--json-schema-export=true",
        ],
        Some("preserve"),
        &[extern_rust_path],
        &[],
        false,
        // schemas_validate_serialization (tests.rs) checks emitted output against emitted schema
        &["jsonschema = { version = \"0.46\", default-features = false }"],
    );
}

#[test]
fn extern_deps() {
    run_test(
        "extern-deps",
        &[
            "--preserve-encodings=true",
            "--common-import-override=extern_dep_crate",
        ],
        None,
        &[],
        &[],
        true,
        &["extern-dep-crate = { path = \"../../../extern-dep-crate\" }"],
    );
}

/// The preserve-encodings=FALSE cell of the extern-deps surface. `extern_deps` (above) only probes
/// `--common-import-override` WITH `--preserve-encodings=true`, so it never exercised a non-preserve
/// crate targeting a preserve-flavored common crate — the real-world CML shape (a non-preserve crate
/// importing `CBORReadLen` from preserve-flavored cml_core). That cell mismatches `Len` vs `LenSz`
/// at construction and fails E0308 unless generation emits `CBORReadLen::from(len)` (going through
/// `From<cbor_event::Len>`) in non-preserve mode. Same override + preserve-flavored stand-in crate
/// as `extern_deps`, just without `--preserve-encodings`.
#[test]
fn extern_deps_non_preserve() {
    run_test(
        "extern-deps-non-preserve",
        &["--common-import-override=extern_dep_crate"],
        None,
        &[],
        &[],
        true,
        &["extern-dep-crate = { path = \"../../../extern-dep-crate\" }"],
    );
}

/// The opt-in recursion depth guard (`--deserialize-depth-limit`). A terminable recursive type
/// (`tests/corpus/recursive.cddl`: `tree = [value: uint, children: [* tree]]`) compiles a
/// recursive-descent deserializer with no intrinsic depth bound — ~100k-deep hostile CBOR recurses
/// until the stack overflows and the process **aborts (SIGABRT, uncatchable by `catch_unwind`)**, a
/// DoS on any consumer parsing untrusted chain data. There is deliberately no default guard (a depth
/// limit rejects spec-valid documents), so this gate proves the opt-in flag turns that abort into a
/// graceful `DeserializeError` while leaving valid, bounded documents unaffected.
///
/// Four assertions, mirroring the design contract:
/// (a) generate the recursive fixture WITH `--deserialize-depth-limit=64 --emit-tests=true`;
/// (b) a hostile 100_000-deep instance, built programmatically from the array-header prefix, is
///     REJECTED (`from_cbor_bytes` returns `Err` naming the depth limit) — process alive. This is the
///     input that SIGABRTs with the flag OFF (verified manually; an aborting test is never committed);
/// (c) the emitted `--emit-tests` round-trip module (values are shallow, well under the cap) still
///     passes under the flag — the same `cargo test` run that executes (b);
/// (d) the guard line is ABSENT from output generated WITHOUT the flag — the cheap text-level proof
///     of the byte-identical-default requirement (the snapshot suite proves the rest).
#[test]
fn deserialize_depth_limit_guards_recursion() {
    use std::str::FromStr;
    if !tool_exists("cargo") {
        return;
    }
    let input = std::path::PathBuf::from_str("tests/corpus/recursive.cddl").unwrap();
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_depth_limit_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    let target_dir = scratch.join("target");

    // (a) generate WITH the guard + emitted tests
    let out_on = scratch.join("on");
    let gen_on = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", input.to_str().unwrap()))
        .arg(format!("--output={}", out_on.to_str().unwrap()))
        .arg("--wasm=false")
        .arg("--deserialize-depth-limit=64")
        .arg("--emit-tests=true")
        .output()
        .unwrap();
    assert!(
        gen_on.status.success(),
        "generation (guard on) failed\n{}",
        String::from_utf8_lossy(&gen_on.stderr)
    );

    // sanity: the guard acquisition line was actually emitted into the recursive deserializer
    let ser_on =
        std::fs::read_to_string(out_on.join("rust/src/generated/serialization.rs")).unwrap();
    assert!(
        ser_on.contains("DepthGuard::acquire(64usize)?"),
        "guard-on output is missing the depth-guard acquisition — the flag emitted nothing"
    );

    // (b) append a hostile-deep reject test. Built from raw CBOR: each nested tree is
    // `array(2), uint 0, array(1)` (0x82 0x00 0x81); the leaf's children is the empty `array(0)`
    // (0x80). 100_000 levels overflows the stack with the flag OFF — here it must return Err, not
    // abort. Written into the generated crate's tests/ so it has the full public API in scope.
    std::fs::create_dir_all(out_on.join("rust/tests")).unwrap();
    std::fs::write(
        out_on.join("rust/tests/hostile_depth.rs"),
        r#"use cddl_lib::Tree;
use cddl_lib::serialization::Deserialize;

#[test]
fn hostile_deep_rejects_without_aborting() {
    let mut bytes = Vec::new();
    for _ in 0..100_000u32 {
        bytes.extend_from_slice(&[0x82, 0x00, 0x81]); // array(2), uint 0, array(1)
    }
    bytes.extend_from_slice(&[0x82, 0x00, 0x80]); // leaf: array(2), uint 0, array(0)
    match Tree::from_cbor_bytes(&bytes) {
        Ok(_) => panic!("hostile deep input should be rejected by the depth guard, got Ok"),
        Err(e) => {
            let msg = e.to_string();
            assert!(
                msg.contains("depth"),
                "expected a depth-limit failure, got: {msg}"
            );
        }
    }
}
"#,
    )
    .unwrap();

    // (b) + (c): `cargo test` runs BOTH the emitted round-trip module (shallow valid values —
    // the flag must not perturb them) AND the hostile reject test above. If the guard were absent
    // this run would abort (signal 6) instead of reporting a normal test result.
    let test_on = tool_cmd("cargo")
        .arg("test")
        .current_dir(out_on.join("rust"))
        .env("CARGO_TARGET_DIR", &target_dir)
        .output()
        .unwrap();
    assert!(
        test_on.status.success(),
        "guard-on crate tests failed (or aborted)\n{}\n{}",
        String::from_utf8_lossy(&test_on.stdout),
        String::from_utf8_lossy(&test_on.stderr)
    );

    // (d) default-off must not emit the guard anywhere — the cheap byte-identical-default check.
    let out_off = scratch.join("off");
    let gen_off = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", input.to_str().unwrap()))
        .arg(format!("--output={}", out_off.to_str().unwrap()))
        .arg("--wasm=false")
        .output()
        .unwrap();
    assert!(
        gen_off.status.success(),
        "generation (guard off) failed\n{}",
        String::from_utf8_lossy(&gen_off.stderr)
    );
    let ser_off =
        std::fs::read_to_string(out_off.join("rust/src/generated/serialization.rs")).unwrap();
    assert!(
        !ser_off.contains("DepthGuard"),
        "guard-off output must not carry any DepthGuard runtime or acquisition (default-off is byte-identical to today)"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// Breadth companion to `robustness_tests::all_supported_constructs_generate`: run the matrix
/// supported-catalog generation check under ALL THREE `ALL_PROFILES` (default / preserve / json),
/// not just default. "Supported" is otherwise silently a default-profile fact — a construct can
/// generate cleanly bare yet abort under `--preserve-encodings` (the float `unimplemented!` class)
/// or the json flags. Generation-only (in-process `generated_strings`, no compile), so it stays
/// cheap enough to run every fixture × both wasm modes × three profiles.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d so it stays out of CI under the feature freeze (the default
/// profile is already covered by the always-on `all_supported_constructs_generate`). Run it with
/// `cargo test --bin cddl-codegen all_supported_constructs_generate_all_profiles -- --ignored`.
///
/// Expected per-profile failures are DERIVED from the matrix emission axis
/// (`cddl-matrix/annotations/cddl_codegen.toml`: `emission.<profile>.status = "unsupported"` on a
/// default-`supported` row) rather than pinned in a second hand-maintained list, so a new matrix
/// row can't leave a stale duplicate here. Four-state verdict per (profile, fixture), mirroring
/// the `WASM_MATRIX_SKIP` pattern: a NON-expected failure fails the gate (a real
/// regression, or a genuine gap to record on the emission axis via a verify.ts probe); an EXPECTED
/// failure that now generates fine fails the gate as "resurfaced" (the gap closed — re-probe so the
/// emission verdict flips to supported), so the verdicts can't rot.
#[test]
#[ignore]
fn all_supported_constructs_generate_all_profiles() {
    use crate::cli::Cli;
    use clap::Parser;

    // Expected non-default generation failures are the matrix's emission-axis verdicts, not a
    // second hand-maintained list. The default profile has no entries:
    // `all_supported_constructs_generate` already proves every supported fixture generates there.
    let matrix_src = std::fs::read_to_string("cddl-matrix/annotations/cddl_codegen.toml").unwrap();
    let matrix_doc: toml::Value =
        toml::from_str(&matrix_src).expect("cddl_codegen.toml is valid TOML");
    let mut expected_fail: std::collections::BTreeMap<(String, String), String> =
        std::collections::BTreeMap::new();
    for support in matrix_doc
        .get("support")
        .and_then(|v| v.as_array())
        .expect("cddl_codegen.toml has [[support]] entries")
    {
        let id = support
            .get("id")
            .and_then(|v| v.as_str())
            .expect("support row has id");
        if support.get("status").and_then(|v| v.as_str()) != Some("supported") {
            continue;
        }
        let Some(emission) = support.get("emission").and_then(|v| v.as_table()) else {
            continue;
        };
        for (profile, verdict) in emission {
            if verdict.get("status").and_then(|v| v.as_str()) == Some("unsupported") {
                let reason = verdict
                    .get("evidence")
                    .and_then(|v| v.as_str())
                    .unwrap_or("emission profile is unsupported")
                    .to_string();
                expected_fail.insert((profile.to_string(), id.to_string()), reason);
            }
        }
    }

    let dir = std::path::Path::new("tests/matrix_supported");
    let mut inputs: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    inputs.sort();
    assert!(
        !inputs.is_empty(),
        "no supported fixtures in {dir:?} (run `bun run project_robustness.ts`)"
    );

    let profiles = super::ALL_PROFILES;
    let mut failures = Vec::new(); // non-expected generation failures — real regressions
    let mut resurfaced = Vec::new(); // EXPECTED_FAIL cells that now generate — remove them
    for path in &inputs {
        let id = path.file_stem().unwrap().to_str().unwrap();
        for (profile, extra) in profiles {
            let expected = expected_fail.get(&(profile.to_string(), id.to_string()));
            // A construct "fails under this profile" if generation errors/panics under EITHER wasm
            // mode (the float `unimplemented!` class aborts in core generation regardless of wasm;
            // running both modes keeps the wasm-binding emission path in scope like the default gate).
            let mut fail_detail: Option<String> = None;
            for wasm in ["false", "true"] {
                let mut args = vec![
                    "cddl-codegen",
                    "--input",
                    path.to_str().unwrap(),
                    "--output",
                    "matrix_supported_profiles_unused",
                    "--wasm",
                    wasm,
                ];
                args.extend(extra.iter().copied());
                let cli = Cli::parse_from(args);
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    crate::api::generated_strings(&cli)
                })) {
                    Ok(Ok(_)) => {}
                    Ok(Err(e)) => {
                        fail_detail = Some(format!("{id}/{profile} (--wasm {wasm}): error: {e}"));
                        break;
                    }
                    Err(_) => {
                        fail_detail = Some(format!("{id}/{profile} (--wasm {wasm}): PANIC"));
                        break;
                    }
                }
            }
            match (expected, fail_detail) {
                (Some(reason), None) => resurfaced.push(format!("{id}/{profile}: {reason}")),
                (None, Some(detail)) => failures.push(detail),
                _ => {} // (Some,Some)=red as expected; (None,None)=green as expected
            }
        }
    }
    assert!(
        resurfaced.is_empty(),
        "these emission-unsupported cells now generate — the gap closed; re-probe the row \
         (cddl-matrix verify.ts) so its `emission.<profile>` verdict flips to supported:\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "matrix-supported constructs failed to generate under a non-default profile (a \
         regression, or a genuine gap to record as `emission.<profile>.status = \"unsupported\"` \
         in cddl-matrix/annotations/cddl_codegen.toml via a verify.ts probe):\n{}",
        failures.join("\n")
    );
}

/// Behavioural companion to `feature_corpus_compiles`: run the corpus's `--emit-tests` round-trip
/// suite under the NON-default profiles. `feature_corpus_compiles` runs `--emit-tests` + `cargo
/// test` ONLY under the default profile; preserve/json are `cargo check`-only there, so the
/// emitted round-trip/reject tests have never RUN at corpus breadth under either flag. This gate
/// generates every corpus fixture with `--emit-tests=true` under the preserve and json profiles
/// and `cargo test`s the rust AND wasm crates (mirroring the default-profile half of
/// `feature_corpus_compiles`), executing the emitted `cddl_generated_tests` /
/// `cddl_generated_wasm_tests` modules — a construct must round-trip byte-identically under the
/// flag, not merely compile.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d so it stays out of CI under the feature freeze
/// (`feature_corpus_compiles` stays byte-for-byte the always-on compile floor; this is the manual
/// round-trip verdict on top). `cargo test` per fixture × two profiles × two crates is materially
/// heavier than the CI gate's per-profile `cargo check`. Run it with
/// `cargo test --bin cddl-codegen feature_corpus_roundtrips_nondefault_profiles -- --ignored`.
///
/// `SKIP` holds `(profile, fixture)` cells whose emitted test surface is a KNOWN structural gap
/// under that profile (a whole feature class the emitter/minter can't yet faithfully round-trip
/// under the flag), each with a reason. Four-state verdict per cell (same as the wasm-matrix
/// gates): a red NON-skip cell fails (a real emitter/minter miscompile to fix, or deliberately
/// SKIP-list with a reason); a SKIP cell that now passes fails the resurfaced guard (the gap
/// closed — take it off SKIP). The per-profile emitted-module floor keeps the execution half from
/// going vacuous if emission silently shrinks.
#[test]
#[ignore]
fn feature_corpus_roundtrips_nondefault_profiles() {
    use std::str::FromStr;

    // (profile, fixture stem, reason) — cells whose emitted round-trip surface is a known
    // structural gap under that profile. Empirically discovered; a resurfaced guard fails the gate
    // if any starts passing so the list can't rot.
    //
    // The former `homogeneous_array`/`special_map_key` preserve entries are gone: the generated
    // indefinite-length break-check now probes for the `0xff` break with the non-consuming
    // `special_break()` (so a major-type-7 element/key falls through to its deserializer), letting
    // the encoding-fidelity oracle run ALL its variant classes — including the two
    // container-reframing ones (`indef_containers`/`everything`) — on those cells, fully green.
    const SKIP: &[(&str, &str, &str)] = &[];

    // Per-profile floor on how many fixtures emit a generated-test module — anti-vacuity guard
    // mirroring `feature_corpus_compiles`. Discovered empirically (see the assert below).
    fn module_floor(profile: &str) -> usize {
        match profile {
            "preserve" => 38,
            "json" => 38,
            _ => 0,
        }
    }

    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    // Non-default profiles only (default is already emit-tests + cargo test in CI). Derived from
    // ALL_PROFILES so a new profile can't silently escape this round-trip gate.
    let profiles: Vec<&super::Profile> = super::ALL_PROFILES
        .iter()
        .filter(|(p, _)| *p != "default")
        .collect();

    let corpus_stems: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|p| p.file_stem().unwrap().to_str().unwrap())
        .collect();
    for stem in COMPILE_SKIP {
        assert!(
            corpus_stems.contains(stem),
            "COMPILE_SKIP names corpus fixture `{stem}` that no longer exists in tests/corpus — \
             stale pin, remove or fix it"
        );
    }
    for (profile, stem, _) in SKIP {
        assert!(
            profiles.iter().any(|(name, _)| name == profile),
            "SKIP names unknown non-default profile `{profile}` — stale pin, remove or fix it"
        );
        assert!(
            corpus_stems.contains(stem),
            "SKIP names corpus fixture `{stem}` that no longer exists in tests/corpus — stale pin, \
             remove or fix it"
        );
    }

    // Own scratch dir + one shared target so cbor_event/wasm-bindgen/the libtest harness build once.
    let scratch_name = format!(
        "cddl_codegen_corpus_roundtrip_profiles_{:016x}",
        checkout_hash()
    );
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red NON-skip cells — real findings
    let mut resurfaced = vec![]; // SKIP cells that now pass — remove them
    let mut emitted_modules: std::collections::BTreeMap<&str, usize> =
        std::collections::BTreeMap::new();
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        if COMPILE_SKIP.contains(&stem) {
            continue;
        }
        for (profile, extra) in &profiles {
            let label = format!("{stem}/{profile}");
            let skipped = SKIP.iter().any(|(p, s, _)| p == profile && s == &stem);
            let out = root.join(format!("{stem}__{profile}"));
            let gen_out = tool_cmd("cargo")
                .args(["run", "--"])
                .arg(format!("--input={}", input.to_str().unwrap()))
                .arg(format!("--output={}", out.to_str().unwrap()))
                .arg("--wasm=true")
                .arg("--emit-tests=true")
                .args(*extra)
                .output()
                .unwrap();
            if !gen_out.status.success() {
                // A generation failure is "red". Only a NON-skip one fails the gate; a skip cell
                // that still fails is red-as-expected (never counts as resurfaced).
                if !skipped {
                    failures.push(format!(
                        "{label}: generation failed\n{}",
                        String::from_utf8_lossy(&gen_out.stderr)
                    ));
                }
                continue;
            }
            if std::fs::read_to_string(out.join("rust/src/generated/mod.rs"))
                .unwrap_or_default()
                .contains("mod cddl_generated_tests")
            {
                *emitted_modules.entry(profile).or_default() += 1;
            }
            // `cargo test` the rust then wasm crate — mirrors the default-profile half of
            // feature_corpus_compiles, but under this profile's flags. A cell PASSES only if both
            // crates test green (and neither is missing).
            let mut cell_red: Option<String> = None;
            for crate_sub in ["rust", "wasm"] {
                let crate_dir = out.join(crate_sub);
                if !crate_dir.exists() {
                    cell_red = Some(format!(
                        "{label} ({crate_sub}): crate dir missing — the fixture is no longer being round-trip-gated"
                    ));
                    break;
                }
                let test = tool_cmd("cargo")
                    .arg("test")
                    .current_dir(&crate_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                if !test.status.success() {
                    cell_red = Some(format!(
                        "{label} ({crate_sub}): cargo test failed\n{}\n{}",
                        String::from_utf8_lossy(&test.stdout),
                        String::from_utf8_lossy(&test.stderr)
                    ));
                    break;
                }
            }
            match (skipped, cell_red) {
                (false, Some(detail)) => failures.push(detail),
                (true, None) => resurfaced.push(label),
                _ => {} // (false,None)=green as expected; (true,Some)=red as expected
            }
            // Free the per-fixture crate dir as we go (keep the shared target) — the machine runs
            // near disk-full, and 43 fixtures × 2 profiles of generated crates add up.
            let _ = std::fs::remove_dir_all(&out);
        }
    }
    // execution-half vacuous-pass guard, per profile.
    for (profile, _) in &profiles {
        let n = emitted_modules.get(*profile).copied().unwrap_or(0);
        let floor = module_floor(profile);
        assert!(
            n >= floor,
            "only {n} corpus fixtures emitted a generated-test module under {profile} (expected >= {floor}) — emit_tests coverage shrank"
        );
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these SKIP-listed corpus cells now round-trip under their profile — remove them from SKIP (the gap closed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "corpus fixtures failed to round-trip under a non-default profile:\n\n{}",
        failures.join("\n\n")
    );
}

// ===== decode-conformance replay (D5) ===============================================================

// The encoding-fidelity mutator ships (via `include_str!`) into `--emit-tests --preserve-encodings`
// crates; here we `include!` the SAME source harness-side so the D5 replay can precompute spec-equal
// re-encodings of each accept vector (indefinite framing, non-minimal int/len widths, chunked strings,
// reversed maps) and feed them to the DEFAULT decoder — the "foreign-vector × default-decode" gap the
// preserve-side emit-tests loop does not cover. Precomputing (rather than splicing the mutator into
// every generated crate) keeps emitted code simple and puts the variant label in the test name. The
// file's `encoding_mutator_self_check` `#[test]` runs as part of our own suite, and `variants()` is
// exercised on a float head by `encoding_variants_copy_float_heads_verbatim` below.
include!("../../static/emit_tests_encoding_fidelity.rs");

/// Harness-side companion to the shipped mutator's `encoding_mutator_self_check`: confirm a major-7
/// FLOAT head (`fa`/`fb` — absent from minted-under-preserve inputs, but present in this gate's foreign
/// accept vectors) rides through `variants()` byte-for-byte. The `Item::Other` arm copies the whole
/// head+argument verbatim (`read_arg` sizes info 26/27 = 4/8 bytes), so surrounding structure mutates
/// while the float bytes stay untouched. Kept here, not in the shipped file, so no generated-crate
/// snapshot moves (the file ships verbatim via `include_str!`).
#[test]
fn encoding_variants_copy_float_heads_verbatim() {
    // [ fa 3f800000 ] (array(1) holding f32 1.0): widen_step widens the array head 0x81 -> 0x98 0x01
    // and copies the 5-byte float head verbatim.
    let input = vec![0x81, 0xfa, 0x3f, 0x80, 0x00, 0x00];
    let vs = cddl_encoding_fidelity::variants(&input);
    let widen = vs
        .iter()
        .find(|(l, _)| *l == "widen_step")
        .expect("widen_step is non-identity for an array head");
    assert_eq!(widen.1, vec![0x98, 0x01, 0xfa, 0x3f, 0x80, 0x00, 0x00]);
    let indef = vs
        .iter()
        .find(|(l, _)| *l == "indef_containers")
        .expect("indef_containers is non-identity for an array");
    assert_eq!(indef.1, vec![0x9f, 0xfa, 0x3f, 0x80, 0x00, 0x00, 0xff]);
    // an 8-byte f64 head (fb, info 27) is copied verbatim too.
    let f64_input = vec![0x81, 0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let f64_indef = cddl_encoding_fidelity::variants(&f64_input)
        .into_iter()
        .find(|(l, _)| *l == "indef_containers")
        .expect("indef_containers is non-identity for an array");
    assert_eq!(
        f64_indef.1,
        vec![
            0x9f, 0xfb, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff
        ]
    );
}

/// One catalog `[[row]]` vector, distilled to what the replay needs: the `hex` bytes, whether it must
/// decode (`accept`), and — for `class="constraint"` reject vectors — the `expect_err` substring the
/// generated decoder's error Display must contain when it rejects. `expect_err` is `None` for accept
/// vectors and for bug/limitation reject pins (which only assert `is_err`); the drift gate enforces
/// that it is present exactly on the constraint vectors, so `Some` here IS "constraint vector".
#[derive(Clone)]
struct ReplayVector {
    hex: String,
    accept: bool,
    /// `class="over-acceptance"`: spec-INVALID CBOR the decoder CURRENTLY (wrongly) accepts (a certified
    /// silent-acceptance pin, no fix yet). It is `expect="accept"` so `accept` is also true, but it is
    /// NOT spec-valid — the base/encoding-variant/header-mutation/preserve legs must exclude it, and it
    /// replays as its own `over_accept_{i}` test asserting the decoder STILL accepts it.
    over_acceptance: bool,
    expect_err: Option<String>,
}

impl ReplayVector {
    /// A spec-VALID accept vector. The encoding-variant, header-mutation, evidenced-major, and preserve
    /// legs derive ONLY from these: an over-acceptance vector is spec-INVALID and evidences nothing about
    /// the spec's shape (a spec-invalid instance re-encoded / header-mutated / byte-checked is meaningless).
    fn spec_valid_accept(&self) -> bool {
        self.accept && !self.over_acceptance
    }
}

/// One catalog `[[row]]` with vectors, distilled to what the replay needs: `spec` (what codegen
/// consumed), the rust `type_name` the vectors decode through, the `mode` (`standalone` => the item
/// under test starts at byte 0; `holder` => it is wrapped in the `[0, <rule>]` = `82 00 …` preamble,
/// so the item starts at byte 2 — the header-mutation leg needs this offset), and each vector's
/// replay shape. Pinned/vectorless rows are skipped by construction (nothing to replay), so they
/// never reach here.
struct ReplayRow {
    id: String,
    spec: String,
    type_name: String,
    mode: String,
    vectors: Vec<ReplayVector>,
}

/// Turn `"820080"` into the `0x82, 0x00, 0x80` a Rust byte-array literal wants (mirrors verify.ts's
/// `replayInDir`, so the two harnesses feed the decoder byte-identical inputs).
fn hex_to_byte_literals(hex: &str) -> String {
    hex.as_bytes()
        .chunks(2)
        .map(|pair| format!("0x{}", std::str::from_utf8(pair).unwrap()))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Decode `"820080"` to the raw bytes `[0x82, 0x00, 0x80]` — the harness-side form the encoding
/// mutator (`cddl_encoding_fidelity::variants`) consumes to derive spec-equal re-encodings.
fn hex_to_bytes(hex: &str) -> Vec<u8> {
    hex.as_bytes()
        .chunks(2)
        .map(|pair| u8::from_str_radix(std::str::from_utf8(pair).unwrap(), 16).unwrap())
        .collect()
}

/// Format raw bytes as the `0x82, 0x00, 0x80` a Rust byte-array literal wants (sibling to
/// `hex_to_byte_literals`, for mutator-produced variant bytes that never existed as a hex string).
fn bytes_to_byte_literals(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| format!("0x{b:02x}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Merge a CBOR major type into its evidence CLASS: majors 0 and 1 (uint/nint — both "integer" to a
/// CDDL `int`-shaped rule) collapse to class 0, everything else is itself. The same 0/1 merge
/// `project_decode_conformance.ts` § 6 uses for its leading-major shape check; here it feeds the
/// header-mutation leg's evidenced-major skip (`header_mutants`).
fn header_major_class(major: u8) -> u8 {
    if major == 1 { 0 } else { major }
}

/// The `wrong_major` major-type remap for a header-mutation reject vector: flip the leading head's
/// top 3 bits to a DIFFERENT major that yields either a well-formed item of the wrong type (the
/// decoder's annotated type-check must fire) or ill-formed CBOR (must also reject). Deterministic
/// mapping (documented in `header_mutants`): containers swap array↔map, strings swap bstr↔tstr, ints
/// become arrays, tags become arrays, simple/float heads become ints. Never identity (so `wrong_major`
/// is never a no-op / vacuous mutant).
fn header_mutant_flip_major(major: u8) -> u8 {
    match major {
        0 => 4, // uint      -> array   (announces N elems, none follow => ill-formed)
        1 => 4, // nint      -> array
        2 => 3, // bstr      -> tstr    (well-formed string of the wrong type)
        3 => 2, // tstr      -> bstr
        4 => 5, // array     -> map     (odd element count => ill-formed, or wrong type)
        5 => 4, // map       -> array
        6 => 4, // tag       -> array
        7 => 0, // simple/fl -> uint
        _ => unreachable!("CBOR major type is 3 bits (0..=7)"),
    }
}

/// Read the CBOR head at `b[0]`: returns `(major, info, argument_value)`, or `None` when the head is
/// indefinite (info 31) or uses a reserved additional-info (28..=30) — for which `trunc_head` has no
/// well-defined argument to widen. Mint guarantees definite minimal input (info 0..=27), so `None` is
/// only ever reached defensively.
fn header_read_head_arg(b: &[u8]) -> Option<(u8, u8, u64)> {
    let head = b[0];
    let major = head >> 5;
    let info = head & 0x1f;
    if info < 24 {
        Some((major, info, info as u64))
    } else if info <= 27 {
        let n = 1usize << (info - 24); // 24->1, 25->2, 26->4, 27->8 argument bytes
        let mut v = 0u64;
        for i in 0..n {
            v = (v << 8) | b[1 + i] as u64;
        }
        Some((major, info, v))
    } else {
        None // 28..=30 reserved, 31 indefinite
    }
}

/// Header-mutation reject mutants of `bytes` (a spec-VALID accept vector), derived by pure byte
/// transforms of the leading CBOR head of the ITEM UNDER TEST — none are spec-valid for the row.
/// `offset` is 0 for `standalone` rows and 2 for `holder` rows (whose vector is `[0, <rule instance>]`
/// = `82 00 <item>`, mechanically enforced by `project_decode_conformance.ts` § 6); the preamble is
/// asserted defensively before slicing, and the inner item is mutated then re-prepended.
///
/// Two labels:
/// - `wrong_major`: rewrite the head's major (top 3 bits) via `header_mutant_flip_major`, keeping the
///   info bits, argument bytes, and the rest of the buffer. Emitted ONLY when the flipped major's
///   evidence class (`header_major_class` — majors 0/1 merged) is NOT in `evidenced_majors`, the set
///   of leading-major classes the row's own ACCEPT vectors demonstrate the spec accepts. A flip
///   landing on an evidenced major is AMBIGUOUS — the row's accept vectors prove the spec accepts
///   that major, so the mutant has no trustworthy expected outcome (it may be a spec-valid instance,
///   e.g. a `uint / tstr / bytes` type choice's bstr↔tstr flip landing on the other string arm) — and
///   skipping emission is strictly better than (row, label)-wide DecodedOk suppression, which would
///   also swallow a future genuine over-acceptance on the row's NON-ambiguous vectors (the same row's
///   uint-headed vectors' 0→4 array flip is never spec-valid and must stay live).
/// - `trunc_head`: re-encode the head with an 8-byte argument (info 27, big-endian = the head's
///   argument value — numeric value for majors 0/1, length for 2..=5, tag number for 6), DROP
///   everything after the head, then drop the final argument byte. The result announces 8 argument
///   bytes but provides 7 and then EOF — unambiguously ill-formed for EVERY decoder (well-formedness,
///   not semantics), so no legitimate accept is possible regardless of major — `evidenced_majors`
///   does NOT apply. Skipped for major-7 heads (their info bits are not a wideable argument) and for
///   indefinite/reserved heads (mint guarantees definite minimal, but be defensive).
fn header_mutants(
    bytes: &[u8],
    offset: usize,
    evidenced_majors: &std::collections::BTreeSet<u8>,
) -> Vec<(&'static str, Vec<u8>)> {
    if offset == 2 {
        assert_eq!(
            &bytes[..2],
            &[0x82u8, 0x00u8],
            "holder-mode vector must begin with the `82 00` = [0, _] preamble \
             (project_decode_conformance.ts § 6) — got {bytes:02x?}"
        );
    }
    let prefix = &bytes[..offset];
    let inner = &bytes[offset..];
    let head = inner[0];
    let major = head >> 5;
    let info = head & 0x1f;
    let mut out: Vec<(&'static str, Vec<u8>)> = Vec::new();

    // wrong_major: rewrite the top 3 bits, keep the info bits, argument bytes, and the rest verbatim
    // — but only when the flipped major is NOT evidenced-accepted by the row (see the doc comment:
    // an evidenced flip is ambiguous, so it is skipped at derivation rather than ledger-suppressed).
    let flipped = header_mutant_flip_major(major);
    if !evidenced_majors.contains(&header_major_class(flipped)) {
        let new_head = (flipped << 5) | info;
        let mut m = Vec::with_capacity(bytes.len());
        m.extend_from_slice(prefix);
        m.push(new_head);
        m.extend_from_slice(&inner[1..]);
        out.push(("wrong_major", m));
    }

    // trunc_head: 8-byte-argument head carrying the same argument value, payload dropped, final
    // argument byte dropped => 8 announced, 7 present, EOF (ill-formed). Skip major-7 and
    // indefinite/reserved heads.
    if major != 7
        && let Some((_, _, arg)) = header_read_head_arg(inner)
    {
        let mut m = Vec::with_capacity(offset + 8);
        m.extend_from_slice(prefix);
        m.push((major << 5) | 27);
        m.extend_from_slice(&arg.to_be_bytes());
        m.pop(); // drop the final argument byte
        out.push(("trunc_head", m));
    }

    out
}

/// Self-check for the header-mutation transforms, pinned against hand-derived RFC 8949 bytes (mirrors
/// `encoding_variants_copy_float_heads_verbatim`'s ethos: pin the mutator, not just the leg). Covers a
/// uint, a wide (2-byte) uint (to pin `header_read_head_arg`'s multi-byte read), a string, a tag head,
/// a holder-shaped input, the major-7 `trunc_head` skip, and the evidenced-major `wrong_major` skip
/// (both directions: an evidenced FLIP target suppresses `wrong_major`; evidence of the input's OWN
/// major does not).
#[test]
fn header_mutants_pin_hand_derived_bytes() {
    // Most cases use EMPTY evidence: no accept vector demonstrates any major, so wrong_major always
    // emits (the conservative default the evidence skip degrades to).
    let none = std::collections::BTreeSet::new();
    // uint 1 (0x01): wrong_major 0->4 => array(1) head 0x81 (ill-formed: no element follows);
    // trunc_head => 0x1b + 7 arg bytes (announces 8, provides 7).
    assert_eq!(
        header_mutants(&[0x01], 0, &none),
        vec![
            ("wrong_major", vec![0x81]),
            (
                "trunc_head",
                vec![0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
    // uint 256 (0x19 0x01 0x00, info 25 = 2-byte arg): wrong_major keeps the info+arg (0x99 0x01 0x00,
    // array(256)); trunc_head widens the read arg 256 to 8 bytes then drops the last => the `01`
    // survives in byte 7, proving header_read_head_arg read the multi-byte argument.
    assert_eq!(
        header_mutants(&[0x19, 0x01, 0x00], 0, &none),
        vec![
            ("wrong_major", vec![0x99, 0x01, 0x00]),
            (
                "trunc_head",
                vec![0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
            ),
        ]
    );
    // tstr "ab" (0x62 0x61 0x62): wrong_major 3->2 => bstr(2) "ab" (0x42 …, well-formed wrong type);
    // trunc_head widens the length 2.
    assert_eq!(
        header_mutants(&[0x62, 0x61, 0x62], 0, &none),
        vec![
            ("wrong_major", vec![0x42, 0x61, 0x62]),
            (
                "trunc_head",
                vec![0x7b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
    // tag(1) over uint 0 (0xc1 0x00): wrong_major 6->4 => array(1)[0] (0x81 0x00, well-formed wrong
    // type); trunc_head widens the tag number 1 (0xdb + 7 arg bytes, payload dropped).
    assert_eq!(
        header_mutants(&[0xc1, 0x00], 0, &none),
        vec![
            ("wrong_major", vec![0x81, 0x00]),
            (
                "trunc_head",
                vec![0xdb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
    // holder-shaped [0, 1] (0x82 0x00 0x01), offset 2: the `82 00` preamble rides through unmutated,
    // the inner uint 1 is mutated exactly as the standalone uint above.
    assert_eq!(
        header_mutants(&[0x82, 0x00, 0x01], 2, &none),
        vec![
            ("wrong_major", vec![0x82, 0x00, 0x81]),
            (
                "trunc_head",
                vec![0x82, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
    // major-7 true (0xf5): wrong_major 7->0 => uint 21 (0x15); trunc_head SKIPPED (info bits are not a
    // wideable argument), so only ONE mutant is emitted.
    assert_eq!(
        header_mutants(&[0xf5], 0, &none),
        vec![("wrong_major", vec![0x15])]
    );
    // Evidenced-major skip: tstr "ab" with major class 2 (bstr) evidenced — the 3->2 flip lands on an
    // accepted major (the `uint / tstr / bytes` type-choice shape), so wrong_major is SKIPPED and only
    // trunc_head (ill-formed regardless of major) is emitted.
    let bstr_evidenced: std::collections::BTreeSet<u8> = [2u8].into_iter().collect();
    assert_eq!(
        header_mutants(&[0x62, 0x61, 0x62], 0, &bstr_evidenced),
        vec![(
            "trunc_head",
            vec![0x7b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        )]
    );
    // Evidence of the input's OWN major does not suppress: uint 1 with class 0 evidenced (the row's
    // own uint accept vectors) still emits wrong_major — the 0->4 flip target (array) is unevidenced.
    // This is exactly the case (row, label)-wide ledger suppression would have swallowed.
    let uint_evidenced: std::collections::BTreeSet<u8> = [0u8].into_iter().collect();
    assert_eq!(
        header_mutants(&[0x01], 0, &uint_evidenced),
        vec![
            ("wrong_major", vec![0x81]),
            (
                "trunc_head",
                vec![0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
    // The 0/1 merge: nint -24 (0x37) with class 0 evidenced via a UINT accept vector still applies
    // (nint evidences class 0 too); its 1->4 flip target is unevidenced, so wrong_major emits.
    assert_eq!(
        header_mutants(&[0x37], 0, &uint_evidenced),
        vec![
            ("wrong_major", vec![0x97]),
            (
                "trunc_head",
                vec![0x3b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ),
        ]
    );
}

/// Generate a crate from `spec` into `out` (default flags unless `extra` adds e.g.
/// `--preserve-encodings=true`), no `--wasm`, no `--emit-tests` — replay needs only the lib. Returns
/// the `cargo run` result so the caller can tell a generation abort (float `unimplemented!` under
/// preserve) from a later compile/decode outcome. The generator uses the repo's warm `./target`
/// exactly like `feature_corpus_compiles`; only the generated crate's own `cargo test` is redirected
/// to the shared scratch target.
fn decode_replay_generate(
    spec: &str,
    out: &std::path::Path,
    extra: &[&str],
) -> std::process::Output {
    let _ = std::fs::remove_dir_all(out);
    std::fs::create_dir_all(out).unwrap();
    let spec_file = out.join("__spec.cddl");
    std::fs::write(&spec_file, format!("{}\n", spec.trim_end_matches('\n'))).unwrap();
    tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", spec_file.to_str().unwrap()))
        .arg(format!("--output={}", out.join("crate").to_str().unwrap()))
        .arg("--wasm=false")
        .args(extra)
        .output()
        .unwrap()
}

/// Escape a catalog `expect_err` substring into a Rust `&str` literal body (only `\` and `"` can
/// appear troublesome; the corpus values also carry backticks/apostrophes, which are literal). Kept
/// tiny and local — the values are short assertion fragments, not arbitrary source.
fn rust_str_literal(s: &str) -> String {
    let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
    format!("\"{escaped}\"")
}

/// Grep-stable panic markers the per-vector replay `#[test]`s emit (in `decode_replay_run`) and the
/// caller (`decode_conformance_replay`) matches to attribute a FAILED test's cause. Defined ONCE and
/// used at BOTH the emission site and the classifier functions below, so the emitted marker word and
/// the needle that attributes it cannot drift apart.
const MARKER_CONSTRAINT_DECODED_OK: &str = "CONSTRAINT_DECODED_OK";
const MARKER_CONSTRAINT_WRONG_REASON: &str = "CONSTRAINT_WRONG_REASON";
const MARKER_VARIANT_REJECTED: &str = "VARIANT_REJECTED";
const MARKER_VARIANT_VALUE_MISMATCH: &str = "VARIANT_VALUE_MISMATCH";
const MARKER_VAR_ORIG_DECODE_FAILED: &str = "VAR_ORIG_DECODE_FAILED";
const MARKER_HDR_MUTANT_DECODED_OK: &str = "HDR_MUTANT_DECODED_OK";
const MARKER_HDR_MUTANT_NO_LOCATION: &str = "HDR_MUTANT_NO_LOCATION";
const MARKER_DOUBLED_LOCATION: &str = "DOUBLED_LOCATION";
/// An `over_accept_{i}` test's `class="over-acceptance"` vector was REJECTED by the decoder — i.e. the
/// decoder no longer wrongly accepts the spec-INVALID bytes (the fix landed). The pin flip signal.
const MARKER_OVER_ACCEPT_NOW_REJECTED: &str = "OVER_ACCEPT_NOW_REJECTED";
const DOUBLED_LOCATION_HELPER_SELF_CHECK: &str = "doubled_location_helper_self_check";

/// How a FAILED `class="constraint"` reject vector's replay test attributed its cause.
#[derive(Debug, PartialEq)]
enum ConstraintFailureKind {
    /// The vector DECODED Ok — the generated decoder does not enforce the constraint (enforcement gap).
    DecodedOk,
    /// The vector was rejected, but the error Display did not contain the pinned `expect_err`.
    WrongReason,
    /// The vector was rejected with an adjacent-duplicate error location segment.
    DoubledLocation,
    /// Neither marker was found in the captured output — unexpected.
    Unattributed,
}

/// Classify a failed constraint-vector replay test from the captured cargo output. The needle is
/// `"{marker} {test_name}:"` and the trailing ':' is LOAD-BEARING: libtest test names end in decimal
/// indices, so `reject_1` is a PREFIX of `reject_10`; without the delimiter `reject_1`'s needle would
/// substring-match `reject_10`'s marker line and misattribute the failure. This function owns that
/// delimiter so no attribution site can forget it.
fn classify_constraint_failure(output: &str, test_name: &str) -> ConstraintFailureKind {
    if output.contains(&format!("{MARKER_CONSTRAINT_DECODED_OK} {test_name}:")) {
        ConstraintFailureKind::DecodedOk
    } else if output.contains(&format!("{MARKER_CONSTRAINT_WRONG_REASON} {test_name}:")) {
        ConstraintFailureKind::WrongReason
    } else if output.contains(&format!("{MARKER_DOUBLED_LOCATION} {test_name}:")) {
        ConstraintFailureKind::DoubledLocation
    } else {
        ConstraintFailureKind::Unattributed
    }
}

/// How a FAILED encoding-variant replay test (`accept_{i}_var_{label}`) attributed its cause.
#[derive(Debug, PartialEq)]
enum VariantFailureKind {
    /// A spec-EQUAL re-encoding was REJECTED by the default decoder (over-strict — the motivating class).
    Rejected,
    /// The variant decoded to a DIFFERENT value than the original (its default re-encoding differs).
    ValueMismatch,
    /// The ORIGINAL accept vector failed to decode inside the variant test — unexpected.
    OrigDecodeFailed,
    /// No known marker was found in the captured output — unexpected.
    Unattributed,
}

/// Classify a failed encoding-variant replay test from the captured cargo output. Same prefix-collision
/// grammar as `classify_constraint_failure`: the needle is `"{marker} {test_name}:"` and the trailing
/// ':' is what stops a prefix test name (`accept_1_var_widen` is a prefix of `accept_1_var_widen_step`,
/// and `accept_1_...` of `accept_10_...`) from stealing the attribution. This function owns that
/// delimiter so no attribution site can forget it.
fn classify_variant_failure(output: &str, test_name: &str) -> VariantFailureKind {
    if output.contains(&format!("{MARKER_VARIANT_REJECTED} {test_name}:")) {
        VariantFailureKind::Rejected
    } else if output.contains(&format!("{MARKER_VARIANT_VALUE_MISMATCH} {test_name}:")) {
        VariantFailureKind::ValueMismatch
    } else if output.contains(&format!("{MARKER_VAR_ORIG_DECODE_FAILED} {test_name}:")) {
        VariantFailureKind::OrigDecodeFailed
    } else {
        VariantFailureKind::Unattributed
    }
}

/// Prefix-collision pin for `classify_constraint_failure`: libtest names end in decimal indices, so
/// `reject_1` is a PREFIX of `reject_10`. Synthesize output where the two fail in OPPOSITE ways and
/// assert each classifies to ITS OWN kind — a needle missing its trailing ':' would let `reject_1`'s
/// DECODED_OK needle match `reject_10`'s marker line and misclassify.
#[test]
fn classify_constraint_failure_disambiguates_prefix_colliding_names() {
    let output = "\
        thread 'reject_1' panicked: CONSTRAINT_WRONG_REASON reject_1: rejected but Display did not contain\n\
        thread 'reject_10' panicked: CONSTRAINT_DECODED_OK reject_10: a class=constraint vector decoded Ok\n\
        thread 'reject_100' panicked: DOUBLED_LOCATION reject_100: adjacent duplicate location segment\n";
    assert_eq!(
        classify_constraint_failure(output, "reject_1"),
        ConstraintFailureKind::WrongReason
    );
    assert_eq!(
        classify_constraint_failure(output, "reject_10"),
        ConstraintFailureKind::DecodedOk
    );
    assert_eq!(
        classify_constraint_failure(output, "reject_100"),
        ConstraintFailureKind::DoubledLocation
    );

    // Mirror the pairing so neither ordering is privileged.
    let mirrored = "\
        thread 'reject_1' panicked: CONSTRAINT_DECODED_OK reject_1: a class=constraint vector decoded Ok\n\
        thread 'reject_10' panicked: DOUBLED_LOCATION reject_10: adjacent duplicate location segment\n\
        thread 'reject_100' panicked: CONSTRAINT_WRONG_REASON reject_100: rejected but Display did not contain\n";
    assert_eq!(
        classify_constraint_failure(mirrored, "reject_1"),
        ConstraintFailureKind::DecodedOk
    );
    assert_eq!(
        classify_constraint_failure(mirrored, "reject_10"),
        ConstraintFailureKind::DoubledLocation
    );
    assert_eq!(
        classify_constraint_failure(mirrored, "reject_100"),
        ConstraintFailureKind::WrongReason
    );

    // The doubled-location marker owns the same trailing-delimiter grammar: a marker for the longer
    // decimal suffix must NOT attribute to the prefix name.
    let doubled_longer =
        "thread 'reject_10' panicked: DOUBLED_LOCATION reject_10: adjacent duplicate\n";
    assert_eq!(
        classify_constraint_failure(doubled_longer, "reject_1"),
        ConstraintFailureKind::Unattributed
    );
    assert_eq!(
        classify_constraint_failure(doubled_longer, "reject_10"),
        ConstraintFailureKind::DoubledLocation
    );

    // A name absent from the output attributes to nothing.
    assert_eq!(
        classify_constraint_failure(output, "reject_2"),
        ConstraintFailureKind::Unattributed
    );
}

/// Prefix-collision pin for `classify_variant_failure`. Variant test names are `accept_{i}_var_{label}`,
/// so a SHORTER label is a prefix of a longer one (`accept_1_var_widen` is a prefix of
/// `accept_1_var_widen_step`) — the trailing ':' is what stops the prefix label from stealing the
/// attribution. Also a straight three-way check that each marker classifies to its kind.
#[test]
fn classify_variant_failure_owns_the_delimiter_and_maps_each_marker() {
    // Truncation direction: a marker line for the LONGER label must NOT attribute the prefix name.
    let widen = "thread 'x' panicked: VARIANT_REJECTED accept_1_var_widen_step: over-strict\n";
    assert_eq!(
        classify_variant_failure(widen, "accept_1_var_widen"),
        VariantFailureKind::Unattributed
    );
    assert_eq!(
        classify_variant_failure(widen, "accept_1_var_widen_step"),
        VariantFailureKind::Rejected
    );

    // Three-way: each marker classifies to its own kind.
    assert_eq!(
        classify_variant_failure(
            "VARIANT_REJECTED accept_0_var_indefinite: over-strict\n",
            "accept_0_var_indefinite"
        ),
        VariantFailureKind::Rejected
    );
    assert_eq!(
        classify_variant_failure(
            "VARIANT_VALUE_MISMATCH accept_0_var_reversed_maps: different value\n",
            "accept_0_var_reversed_maps"
        ),
        VariantFailureKind::ValueMismatch
    );
    assert_eq!(
        classify_variant_failure(
            "VAR_ORIG_DECODE_FAILED accept_0_var_chunked: orig failed\n",
            "accept_0_var_chunked"
        ),
        VariantFailureKind::OrigDecodeFailed
    );

    // A name absent from the output attributes to nothing.
    assert_eq!(
        classify_variant_failure(widen, "accept_2_var_indefinite"),
        VariantFailureKind::Unattributed
    );
}

/// How a FAILED header-mutant replay test (`accept_{i}_hdr_{label}`) attributed its cause.
#[derive(Debug, PartialEq)]
enum HeaderMutantFailureKind {
    /// The header-mutated (not spec-valid) vector DECODED Ok — over-acceptance, OR a row whose spec
    /// genuinely accepts the mutated bytes (`any`, a multi-major type choice the flipped major lands
    /// on): the legitimate ones are ledgered in `HEADER_MUTANT_ACCEPT_SKIP`.
    DecodedOk,
    /// The vector was rejected, but the error Display carried NO location naming the decoding type
    /// (`failed in {type_name}`) — a generator annotation gap or the locationless `TrailingData` path;
    /// legitimate ones are ledgered in `HEADER_MUTANT_LOCATION_SKIP`.
    NoLocation,
    /// The vector was rejected with an adjacent-duplicate error location segment.
    DoubledLocation,
    /// Neither marker was found in the captured output — unexpected.
    Unattributed,
}

/// Classify a failed header-mutant replay test from the captured cargo output. Same prefix-collision
/// grammar as `classify_constraint_failure` / `classify_variant_failure`: the needle is
/// `"{marker} {test_name}:"` and the trailing ':' is what stops a prefix test name
/// (`accept_1_hdr_wrong_major` is a prefix of nothing, but `accept_1_…` is a prefix of `accept_10_…`)
/// from stealing the attribution. This function owns that delimiter so no attribution site can forget
/// it.
fn classify_header_mutant_failure(output: &str, test_name: &str) -> HeaderMutantFailureKind {
    if output.contains(&format!("{MARKER_HDR_MUTANT_DECODED_OK} {test_name}:")) {
        HeaderMutantFailureKind::DecodedOk
    } else if output.contains(&format!("{MARKER_HDR_MUTANT_NO_LOCATION} {test_name}:")) {
        HeaderMutantFailureKind::NoLocation
    } else if output.contains(&format!("{MARKER_DOUBLED_LOCATION} {test_name}:")) {
        HeaderMutantFailureKind::DoubledLocation
    } else {
        HeaderMutantFailureKind::Unattributed
    }
}

/// Prefix-collision pin for `classify_header_mutant_failure`, mirroring
/// `classify_constraint_failure_disambiguates_prefix_colliding_names`: libtest names end in decimal
/// indices, so `accept_1_hdr_wrong_major` is a PREFIX of `accept_10_hdr_wrong_major`. Synthesize
/// output where the two fail in OPPOSITE ways and assert each classifies to ITS OWN kind (both
/// orderings), each marker maps to its kind, and an absent name attributes to nothing.
#[test]
fn classify_header_mutant_failure_disambiguates_prefix_colliding_names() {
    let output = "\
        thread 'x' panicked: HDR_MUTANT_NO_LOCATION accept_1_hdr_wrong_major: rejected but no location\n\
        thread 'y' panicked: HDR_MUTANT_DECODED_OK accept_10_hdr_wrong_major: header-mutated vector decoded Ok\n\
        thread 'z' panicked: DOUBLED_LOCATION accept_100_hdr_wrong_major: adjacent duplicate location segment\n";
    assert_eq!(
        classify_header_mutant_failure(output, "accept_1_hdr_wrong_major"),
        HeaderMutantFailureKind::NoLocation
    );
    assert_eq!(
        classify_header_mutant_failure(output, "accept_10_hdr_wrong_major"),
        HeaderMutantFailureKind::DecodedOk
    );
    assert_eq!(
        classify_header_mutant_failure(output, "accept_100_hdr_wrong_major"),
        HeaderMutantFailureKind::DoubledLocation
    );

    // Mirror the pairing so neither ordering is privileged.
    let mirrored = "\
        thread 'x' panicked: HDR_MUTANT_DECODED_OK accept_1_hdr_wrong_major: header-mutated vector decoded Ok\n\
        thread 'y' panicked: DOUBLED_LOCATION accept_10_hdr_wrong_major: adjacent duplicate location segment\n\
        thread 'z' panicked: HDR_MUTANT_NO_LOCATION accept_100_hdr_wrong_major: rejected but no location\n";
    assert_eq!(
        classify_header_mutant_failure(mirrored, "accept_1_hdr_wrong_major"),
        HeaderMutantFailureKind::DecodedOk
    );
    assert_eq!(
        classify_header_mutant_failure(mirrored, "accept_10_hdr_wrong_major"),
        HeaderMutantFailureKind::DoubledLocation
    );
    assert_eq!(
        classify_header_mutant_failure(mirrored, "accept_100_hdr_wrong_major"),
        HeaderMutantFailureKind::NoLocation
    );

    // The doubled-location marker owns the same trailing-delimiter grammar: a marker for the longer
    // decimal suffix must NOT attribute to the prefix name.
    let doubled_longer =
        "thread 'x' panicked: DOUBLED_LOCATION accept_10_hdr_wrong_major: adjacent duplicate\n";
    assert_eq!(
        classify_header_mutant_failure(doubled_longer, "accept_1_hdr_wrong_major"),
        HeaderMutantFailureKind::Unattributed
    );
    assert_eq!(
        classify_header_mutant_failure(doubled_longer, "accept_10_hdr_wrong_major"),
        HeaderMutantFailureKind::DoubledLocation
    );

    // A name absent from the output attributes to nothing.
    assert_eq!(
        classify_header_mutant_failure(output, "accept_2_hdr_trunc_head"),
        HeaderMutantFailureKind::Unattributed
    );
}

/// How a FAILED `class="over-acceptance"` replay test (`over_accept_{i}`) attributed its cause. An
/// over-acceptance vector asserts the decoder STILL (wrongly) accepts spec-INVALID bytes, so its ONLY
/// failure mode is the decoder REJECTING them — the pin flip that means a fix landed.
#[derive(Debug, PartialEq)]
enum OverAcceptanceFailureKind {
    /// The vector was REJECTED — the decoder no longer wrongly accepts it (promote to class="constraint").
    NowRejected,
    /// No known marker was found in the captured output — unexpected.
    Unattributed,
}

/// Classify a failed over-acceptance replay test from the captured cargo output. Same prefix-collision
/// grammar as `classify_constraint_failure` / `classify_variant_failure` / `classify_header_mutant_failure`:
/// the needle is `"{marker} {test_name}:"` and the trailing ':' is what stops a prefix test name
/// (`over_accept_1` is a prefix of `over_accept_10`) from stealing the attribution. This function owns
/// that delimiter so no attribution site can forget it.
fn classify_over_acceptance_failure(output: &str, test_name: &str) -> OverAcceptanceFailureKind {
    if output.contains(&format!("{MARKER_OVER_ACCEPT_NOW_REJECTED} {test_name}:")) {
        OverAcceptanceFailureKind::NowRejected
    } else {
        OverAcceptanceFailureKind::Unattributed
    }
}

/// Prefix-collision pin for `classify_over_acceptance_failure`, mirroring
/// `classify_constraint_failure_disambiguates_prefix_colliding_names`: libtest names end in decimal
/// indices, so `over_accept_1` is a PREFIX of `over_accept_10`. Assert a marker for the longer suffix
/// does NOT attribute to the prefix name, the marker maps to its kind, and an absent name attributes to
/// nothing.
#[test]
fn classify_over_acceptance_failure_disambiguates_prefix_colliding_names() {
    let longer =
        "thread 'x' panicked: OVER_ACCEPT_NOW_REJECTED over_accept_10: the fix landed, promote\n";
    assert_eq!(
        classify_over_acceptance_failure(longer, "over_accept_1"),
        OverAcceptanceFailureKind::Unattributed
    );
    assert_eq!(
        classify_over_acceptance_failure(longer, "over_accept_10"),
        OverAcceptanceFailureKind::NowRejected
    );
    assert_eq!(
        classify_over_acceptance_failure(
            "OVER_ACCEPT_NOW_REJECTED over_accept_0: the fix landed\n",
            "over_accept_0"
        ),
        OverAcceptanceFailureKind::NowRejected
    );
    assert_eq!(
        classify_over_acceptance_failure(longer, "over_accept_2"),
        OverAcceptanceFailureKind::Unattributed
    );
}

/// Append the `__foreign_decode_replay` module (one `#[test]` per vector) to the generated lib.rs and
/// `cargo test` it under the shared scratch target, returning the per-test `name -> passed` map — or
/// `None` when the crate did not compile (no result lines), so callers separate "decoder rejected a
/// vector" (a verdict) from "crate didn't build" (a preserve-side generation/compile finding). The
/// module mirrors verify.ts's `replayInDir`: accept => `from_cbor_bytes` must be Ok; reject => must be
/// Err; under preserve each accept ALSO asserts `to_cbor_bytes()` byte-identity (the preserve contract
/// is itself decode-direction evidence — the decoder captured the exact input encoding).
///
/// A `class="over-acceptance"` vector (spec-INVALID CBOR the decoder CURRENTLY wrongly accepts) emits
/// its OWN `over_accept_{i}` `#[test]` asserting decode STAYS Ok; a rejection panics with the
/// grep-stable `OVER_ACCEPT_NOW_REJECTED` marker naming the promotion flow (the pin flipped — the fix
/// landed). These vectors are default-leg only: the caller excludes them from the accept-derived
/// encoding-variant / header-mutation / preserve legs (`ReplayVector::spec_valid_accept`), since a
/// spec-invalid instance evidences nothing about the spec's shape.
///
/// On the DEFAULT leg a reject vector WITH `expect_err` (a `class="constraint"` vector) does more than
/// assert `is_err`: it `match`es the Result (generated types don't uniformly derive Debug, so
/// `.expect_err()` on the Result won't compile) and asserts the error Display CONTAINS the pinned
/// substring — pinning the rejection REASON, not just that it rejects. Two distinct grep-stable panic
/// markers (`CONSTRAINT_DECODED_OK` vs `CONSTRAINT_WRONG_REASON`, each naming its `reject_{i}`) let the
/// caller tell a decoder that failed to reject from one that rejected for the WRONG reason; the
/// wrong-reason panic embeds the captured Display so the failure is actionable from gate output. The
/// preserve-failure panic markers stay distinct for the same reason (`PRESERVE_BYTE_MISMATCH` vs
/// `PRESERVE_DECODE_FAILED`).
///
/// `variant_specs` (DEFAULT leg only; the caller passes `&[]` under preserve) carries precomputed
/// spec-equal re-encodings of the accept vectors as `(accept_vector_index, label, bytes)`. Each becomes
/// an `accept_{i}_var_{label}` `#[test]` that decodes the ORIGINAL (must be Ok) and the VARIANT: an
/// `Err` panics with `VARIANT_REJECTED` (an over-strict decoder rejecting a spec-equal re-encoding — the
/// motivating class), and an `Ok` asserts `to_cbor_bytes()` equals the original's re-encoding with
/// `VARIANT_VALUE_MISMATCH` (a same-value proxy: default-profile re-encoding is a deterministic function
/// of the decoded value, and generated types don't uniformly derive PartialEq).
///
/// `header_specs` (DEFAULT leg only; `&[]` under preserve) carries precomputed HEADER-MUTATION reject
/// mutants of the accept vectors as `(accept_vector_index, label, bytes)` — `wrong_major` / `trunc_head`
/// byte transforms that are NOT spec-valid for the row (`header_mutants`). Each becomes an
/// `accept_{i}_hdr_{label}` `#[test]` asserting the decoder returns `Err` AND the error Display carries
/// a location naming the decoding type (`failed in {type_name}` — NOT a bare `type_name` contains, which
/// single-letter type names like `T` would vacuously match against words like "TagMismatch"). An `Ok`
/// panics with `HDR_MUTANT_DECODED_OK` (over-acceptance or a legitimately-accepting row) and a
/// location-less rejection with `HDR_MUTANT_NO_LOCATION` (a generator annotation gap or the
/// `from_cbor_bytes` `TrailingData` path, which is locationless by construction). The emitted helper
/// also rejects adjacent-duplicate Display location segments with `DOUBLED_LOCATION`. The completeness
/// check counts all three legs plus the helper self-check against
/// `vectors.len() + variant_specs.len() + header_specs.len() + 1`.
fn decode_replay_run(
    out: &std::path::Path,
    type_name: &str,
    vectors: &[ReplayVector],
    preserve: bool,
    target_dir: &std::path::Path,
    variant_specs: &[(usize, String, Vec<u8>)],
    header_specs: &[(usize, String, Vec<u8>)],
) -> (Option<std::collections::BTreeMap<String, bool>>, String) {
    let mut fns = String::new();
    for (i, vector) in vectors.iter().enumerate() {
        let hex = &vector.hex;
        let bytes = hex_to_byte_literals(hex);
        let (name, body) = if vector.over_acceptance {
            // A certified spec-INVALID vector the decoder CURRENTLY (wrongly) accepts: assert it STILL
            // decodes Ok. A rejection is the pin flip — the fix landed. Its own test name (`over_accept_`)
            // and grep-stable marker attribute a FAILED test distinctly, naming the promotion flow.
            let name = format!("over_accept_{i}");
            let body = format!(
                "match {type_name}::from_cbor_bytes(BYTES) {{\n\
                 \x20           Ok(_) => {{}}\n\
                 \x20           Err(e) => panic!(\"{MARKER_OVER_ACCEPT_NOW_REJECTED} {name}: a class=over-acceptance vector was REJECTED — the decoder no longer wrongly accepts this spec-INVALID CBOR (the fix landed): promote this vector to class=\\\"constraint\\\" with an expect_err, move the row id from EXPECTED_ENFORCE_OVERACCEPTS to EXPECTED_ENFORCE_YES in query_q4_directional.ts, update the ROADMAP finding, re-mint — err: {{}}\", e),\n\
                 \x20       }}"
            );
            // Vacuity guard at the emission site (the constraint arm's CONSTRAINT_WRONG_REASON body
            // assert twin): the built body must carry the marker, so a drifted match can't silently emit
            // a body that never exercises the still-accepts contract while the count floor stays green.
            assert!(
                body.contains(MARKER_OVER_ACCEPT_NOW_REJECTED),
                "decode_replay_run built an over-acceptance body missing its marker ({name}) — the \
                 over-acceptance arm regressed"
            );
            (name, body)
        } else if vector.accept {
            let name = format!("accept_{i}");
            let body = if preserve {
                format!(
                    "let decoded = {type_name}::from_cbor_bytes(BYTES).expect(\"PRESERVE_DECODE_FAILED\");\n\
                     \x20       assert_eq!(decoded.to_cbor_bytes(), BYTES.to_vec(), \"PRESERVE_BYTE_MISMATCH\");"
                )
            } else {
                format!(
                    "{type_name}::from_cbor_bytes(BYTES).expect(\"accept vector must decode\");"
                )
            };
            (name, body)
        } else {
            let name = format!("reject_{i}");
            // DEFAULT leg + constraint vector => assert the rejection REASON, not just is_err.
            let body = match (preserve, &vector.expect_err) {
                (false, Some(expect_err)) => {
                    let expect_lit = rust_str_literal(expect_err);
                    format!(
                        "match {type_name}::from_cbor_bytes(BYTES) {{\n\
                         \x20           Ok(_) => panic!(\"{MARKER_CONSTRAINT_DECODED_OK} {name}: a class=constraint vector decoded Ok — the generated decoder does NOT enforce the constraint\"),\n\
                         \x20           Err(e) => {{\n\
                         \x20               let disp = e.to_string();\n\
                         \x20               assert_no_adjacent_duplicate_location(\"{MARKER_DOUBLED_LOCATION}\", \"{name}\", &disp);\n\
                         \x20               assert!(disp.contains({expect_lit}), \"{MARKER_CONSTRAINT_WRONG_REASON} {name}: rejected but Display did not contain {{:?}} — got: {{}}\", {expect_lit}, disp);\n\
                         \x20           }}\n\
                         \x20       }}"
                    )
                }
                _ => format!(
                    "assert!({type_name}::from_cbor_bytes(BYTES).is_err(), \"reject vector must NOT decode\");"
                ),
            };
            // Guard the reason-assert against a silent arm regression: the caller's vacuity floor
            // counts vectors whose CATALOG carries `expect_err`, not what this function emitted, so
            // a drifted match pattern above (constraint vectors falling into the plain-`is_err`
            // fallback) would keep that floor green while the reason pin went vacuous. Assert here —
            // OUTSIDE the match — that a default-leg constraint vector's body really is the
            // reason-asserting form.
            if !preserve && vector.expect_err.is_some() {
                assert!(
                    body.contains(MARKER_CONSTRAINT_WRONG_REASON)
                        && body.contains(MARKER_DOUBLED_LOCATION),
                    "decode_replay_run built a body missing a marker for a default-leg vector with \
                     expect_err ({name}) — the constraint match arm regressed"
                );
            }
            (name, body)
        };
        fns.push_str(&format!(
            "\n    #[test]\n    fn {name}() {{\n        const BYTES: &[u8] = &[{bytes}];\n        {body}\n    }}\n"
        ));
    }
    // DEFAULT-leg encoding-variant tests: for each precomputed spec-equal re-encoding, decode ORIG
    // (must stay Ok) and the VARIANT, with grep-stable markers separating an over-strict rejection
    // from a same-value re-encode mismatch.
    for (i, label, var_bytes) in variant_specs {
        let name = format!("accept_{i}_var_{label}");
        let orig = hex_to_byte_literals(&vectors[*i].hex);
        let var = bytes_to_byte_literals(var_bytes);
        // NB the `.expect` message carries NO trailing ':' yet still satisfies
        // `classify_variant_failure`'s `"{marker} {name}:"` needle — `Result::expect` panics with
        // `{msg}: {err}`, so std's format supplies the colon at runtime (the classifier unit pin's
        // `VAR_ORIG_DECODE_FAILED …: orig failed` fixture is that runtime shape). Rewriting this
        // as a colon-less `panic!` would drop that delimiter and degrade attribution to the
        // unexplained-failure branch; keep the marker inside `.expect` (or emit the ':' yourself).
        let body = format!(
            "let orig_val = {type_name}::from_cbor_bytes(ORIG).expect(\"{MARKER_VAR_ORIG_DECODE_FAILED} {name}\");\n\
             \x20       match {type_name}::from_cbor_bytes(VAR) {{\n\
             \x20           Err(e) => panic!(\"{MARKER_VARIANT_REJECTED} {name}: a spec-equal re-encoding was rejected by the default decoder (over-strict) — err: {{}}\", e),\n\
             \x20           Ok(var_val) => assert_eq!(var_val.to_cbor_bytes(), orig_val.to_cbor_bytes(), \"{MARKER_VARIANT_VALUE_MISMATCH} {name}: variant decoded to a DIFFERENT value than the original\"),\n\
             \x20       }}"
        );
        fns.push_str(&format!(
            "\n    #[test]\n    fn {name}() {{\n        const ORIG: &[u8] = &[{orig}];\n        const VAR: &[u8] = &[{var}];\n        {body}\n    }}\n"
        ));
    }
    // DEFAULT-leg header-mutation reject tests: each precomputed mutant must be REJECTED, and the
    // error Display must carry a location naming the decoding type (`failed in {type_name}` — the
    // annotation analogue of the constraint leg's `expect_err` reason pin). Grep-stable markers
    // separate an over-acceptance (`Ok`), a location-less rejection, and a doubled location chain.
    let loc_needle = format!("failed in {type_name}");
    let loc_lit = rust_str_literal(&loc_needle);
    for (i, label, mut_bytes) in header_specs {
        let name = format!("accept_{i}_hdr_{label}");
        let mutb = bytes_to_byte_literals(mut_bytes);
        let body = format!(
            "match {type_name}::from_cbor_bytes(MUT) {{\n\
             \x20           Ok(_) => panic!(\"{MARKER_HDR_MUTANT_DECODED_OK} {name}: a header-mutated (not spec-valid) vector decoded Ok — over-acceptance or a legitimately-accepting row (triage: HEADER_MUTANT_ACCEPT_SKIP)\"),\n\
             \x20           Err(e) => {{\n\
             \x20               let disp = e.to_string();\n\
             \x20               assert_no_adjacent_duplicate_location(\"{MARKER_DOUBLED_LOCATION}\", \"{name}\", &disp);\n\
             \x20               assert!(disp.contains({loc_lit}), \"{MARKER_HDR_MUTANT_NO_LOCATION} {name}: rejected but the error carries no location naming the type — got: {{}}\", disp);\n\
             \x20           }}\n\
             \x20       }}"
        );
        // Vacuity guard at the emission site (mirrors the constraint arm's CONSTRAINT_WRONG_REASON
        // body assert, per the "vacuity floors must witness the guarded artifact" rule): the built
        // body must carry all markers, so a drifted match/assert can't silently emit a body that
        // never exercises the decode-Err + location contract while the header-test COUNT floor stays
        // green.
        assert!(
            body.contains(MARKER_HDR_MUTANT_DECODED_OK)
                && body.contains(MARKER_HDR_MUTANT_NO_LOCATION)
                && body.contains(MARKER_DOUBLED_LOCATION),
            "decode_replay_run built a header-mutant body missing a marker ({name}) — the \
             emission arm regressed"
        );
        fns.push_str(&format!(
            "\n    #[test]\n    fn {name}() {{\n        const MUT: &[u8] = &[{mutb}];\n        {body}\n    }}\n"
        ));
    }
    let module = format!(
        "\n#[cfg(test)]\n#[allow(clippy::all)]\nmod __foreign_decode_replay {{\n    use super::*;\n    use super::serialization::*;\n\
\n    fn assert_no_adjacent_duplicate_location(marker: &str, test_name: &str, disp: &str) {{\n        let Some(start) = disp.find(\"failed in \").map(|i| i + \"failed in \".len()) else {{\n            return;\n        }};\n        let Some(end) = disp.find(\" because\") else {{\n            return;\n        }};\n        if end <= start {{\n            return;\n        }};\n        let loc = &disp[start..end];\n        let mut prev = None;\n        for segment in loc.split('.') {{\n            if prev == Some(segment) {{\n                panic!(\"{{marker}} {{test_name}}: adjacent duplicate location segment {{segment:?}} in Display: {{disp}}\");\n            }}\n            prev = Some(segment);\n        }}\n    }}\n\n    #[test]\n    fn {DOUBLED_LOCATION_HELPER_SELF_CHECK}() {{\n        let doubled = std::panic::catch_unwind(|| {{\n            assert_no_adjacent_duplicate_location(\n                \"{MARKER_DOUBLED_LOCATION}\",\n                \"{DOUBLED_LOCATION_HELPER_SELF_CHECK}\",\n                \"Deserialization failed in Foo.Foo because: x\",\n            );\n        }});\n        assert!(doubled.is_err(), \"doubled adjacent segment should trip\");\n        assert_no_adjacent_duplicate_location(\n            \"{MARKER_DOUBLED_LOCATION}\",\n            \"{DOUBLED_LOCATION_HELPER_SELF_CHECK}\",\n            \"Deserialization failed in Foo.opt_text.Foo because: x\",\n        );\n        assert_no_adjacent_duplicate_location(\n            \"{MARKER_DOUBLED_LOCATION}\",\n            \"{DOUBLED_LOCATION_HELPER_SELF_CHECK}\",\n            \"Deserialization: x\",\n        );\n    }}\n{fns}}}\n"
    );
    // Append into the generated root scope (`generated/mod.rs`): the replay module's `use super::*;` /
    // `use super::serialization::*;` need the root scope's imports and its sibling serialization
    // module, which live there rather than in the thin seed-once `lib.rs`.
    let lib_path = out.join("crate/rust/src/generated/mod.rs");
    let existing = std::fs::read_to_string(&lib_path).unwrap();
    std::fs::write(&lib_path, existing + &module).unwrap();

    let test = tool_cmd("cargo")
        .arg("test")
        .current_dir(out.join("crate/rust"))
        .env("CARGO_TARGET_DIR", target_dir)
        .arg("--")
        .arg("__foreign_decode_replay")
        .output()
        .unwrap();
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&test.stdout),
        String::from_utf8_lossy(&test.stderr)
    );
    let mut results = std::collections::BTreeMap::new();
    for line in combined.lines() {
        // libtest: "test <path>__foreign_decode_replay::accept_0 ... ok" / "... FAILED". The module
        // is appended into `generated/mod.rs`, so its libtest path is `generated::__foreign_…`; match
        // the module marker anywhere after `test ` so the parse is agnostic to the parent module path.
        let line = line.trim_start();
        if let Some(rest) = line
            .strip_prefix("test ")
            .and_then(|r| r.split_once("__foreign_decode_replay::"))
            .map(|(_, r)| r)
            && let Some((name, tail)) = rest.split_once(" ... ")
        {
            let tail = tail.trim();
            if tail == "ok" {
                results.insert(name.to_string(), true);
            } else if tail == "FAILED" {
                results.insert(name.to_string(), false);
            }
        }
    }
    if results.len() != vectors.len() + variant_specs.len() + header_specs.len() + 1 {
        // No/partial result lines => the crate did not compile (or libtest output drifted).
        return (None, combined);
    }
    (Some(results), combined)
}

/// Deterministic decode-direction replay of the committed `tests/decode_conformance/catalog.toml`
/// corpus (no oracles — the bytes were spec-cross-validated at mint time). Per active row: generate
/// the crate from `spec`, replay every vector through the generated decoder (accept => Ok, reject
/// pin => Err), then regenerate under `--preserve-encodings=true` and replay the ACCEPT vectors
/// asserting decode-Ok AND `to_cbor_bytes()` byte-identity. A reject pin that now decodes Ok fails
/// the gate (re-bless protection); a `PRESERVE_SKIP` row that starts working fails it (stale-entry
/// guard, mirroring `all_supported_constructs_generate_all_profiles`'s EXPECTED_FAIL).
///
/// A `class="constraint"` reject vector additionally pins its rejection REASON: on the default leg its
/// error Display must CONTAIN the catalog's `expect_err` substring. This catches a decoder that rejects
/// the vector for a subtly WRONG reason (a stray length check, an unrelated error path) — which a bare
/// `is_err` assert would pass. A wrong-reason rejection fails the gate with the captured Display in the
/// output; a constraint vector that decodes Ok fails it as an enforcement gap.
///
/// A `class="over-acceptance"` accept vector is the inverse pin: spec-INVALID CBOR the decoder CURRENTLY
/// (wrongly) accepts (a certified silent-acceptance bug, no fix yet). Its own `over_accept_{i}` test on
/// the DEFAULT leg asserts the decoder STILL accepts it — so when a fix lands, the pin flips LOUDLY
/// (`OVER_ACCEPT_NOW_REJECTED`), prompting promotion to `class="constraint"` and the Q4 enforce-green
/// pin. It is one assertion, default-leg only: excluded from the encoding-variant, header-mutation, and
/// preserve legs (a spec-invalid instance evidences nothing about the spec's shape).
///
/// The default leg also feeds each accept vector's mechanically-derived ENCODING VARIANTS
/// (`cddl_encoding_fidelity::variants` — indefinite framing, non-minimal widths, chunked strings,
/// reversed maps) to the decoder: a spec-EQUAL re-encoding that the decoder REJECTS (over-strict — the
/// motivating class) or mis-decodes to a different value fails the gate. `ENCODING_VARIANT_SKIP`
/// (stale-guarded) ledgers any (row, label) that legitimately fails against a `cddl-matrix/ROADMAP.md`
/// finding; it is EMPTY at HEAD (every variant decodes cleanly).
///
/// The default leg ALSO derives HEADER-MUTATION reject mutants of each accept vector (`header_mutants`
/// — pure byte transforms, no oracle): `wrong_major` (flip the leading head's major type) and
/// `trunc_head` (re-encode the head with an 8-byte argument, drop the payload, then drop the final
/// argument byte → ill-formed by construction). A `wrong_major` flip landing on a major the row's OWN
/// accept vectors evidence (majors 0/1 merged) is skipped at DERIVATION time — such a mutant is
/// ambiguous (possibly spec-valid, e.g. type.choice's bstr↔tstr flip landing on the other
/// `uint / tstr / bytes` arm), and derivation-time skipping keeps the row's non-ambiguous mutants live
/// where a (row, label)-wide ledger entry would swallow them. Each emitted mutant must be REJECTED,
/// and the error Display must carry a location naming the decoding type
/// (`disp.contains("failed in {type_name}")` — the annotation analogue of the constraint leg's
/// `expect_err` reason pin; NOT a bare `type_name` contains, which single-letter type names like `T`
/// would vacuously match against words like "TagMismatch"). Two stale-guarded ledgers cover the honest
/// exceptions: `HEADER_MUTANT_ACCEPT_SKIP` (a mutant the row's spec genuinely accepts WITHOUT any
/// accept vector evidencing that major — an `any`-typed row, an unsampled choice arm; EMPTY at HEAD;
/// `trunc_head` can never be here, asserted) and `HEADER_MUTANT_LOCATION_SKIP` (a rejection carrying
/// no location — EMPTY at HEAD now that the newtype-wrapper container reads and embedded/plain-group
/// header scaffolding are annotated; the only known-legitimate locationless resident, the
/// `from_cbor_bytes` `TrailingData` path, is not reached by any header mutant here). A header-mutant
/// vacuity floor keeps the leg live.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d. Measured wall time ~180s warm (104 active rows × up to two full
/// generate+`cargo test` crate builds, the default build now also compiling ~4500 encoding-variant
/// tests plus ~2040 header-mutation tests), well past the ~90s plain-`#[test]` threshold, so it is
/// a `full`-tier check.ts gate rather than riding the always-on `test` gate. Its own scratch dir +
/// `cddl_codegen_decode_conformance` target so it never collides with the corpus/wasm gates when
/// `cargo test` runs tests in parallel; `acquire_scratch_lock` serializes same-checkout runs.
#[ignore = "manual/local decode-conformance replay gate (heavy: per-catalog-row crate builds under two profiles): cargo test --bin cddl-codegen decode_conformance_replay -- --ignored --nocapture"]
#[test]
fn decode_conformance_replay() {
    if !tool_exists("cargo") {
        return;
    }

    // Rows whose generation/compile legitimately fails under `--preserve-encodings=true`. EXPECTED
    // members are the native-float class: a float struct/element field hits the pre-existing
    // `unimplemented!` in generation.rs ("preserve_encodings is not implemented for float"), the same
    // gap the `preserve_encodings_supports_floats` stub tracks. `prelude.float/float32/float64` are
    // floats directly; `prelude.number` (int / float) and `prelude.time` (~= number) carry a float
    // arm. A row here that starts generating+replaying cleanly under preserve is a stale entry and
    // fails the gate (the float gap closed — unblock it and drop it from this list).
    const PRESERVE_SKIP: &[(&str, &str)] = &[
        (
            "prelude.float",
            "native float under --preserve-encodings is unimplemented (generation.rs float arm \
             `unimplemented!`; see the preserve_encodings_supports_floats stub)",
        ),
        (
            "prelude.float32",
            "native float under --preserve-encodings is unimplemented (generation.rs float arm \
             `unimplemented!`; see the preserve_encodings_supports_floats stub)",
        ),
        (
            "prelude.float64",
            "native float under --preserve-encodings is unimplemented (generation.rs float arm \
             `unimplemented!`; see the preserve_encodings_supports_floats stub)",
        ),
        (
            "prelude.number",
            "`number` (int / float) carries the native-float arm that is unimplemented under \
             --preserve-encodings (generation.rs; see the preserve_encodings_supports_floats stub)",
        ),
        (
            "prelude.time",
            "`time` (~= number) carries the native-float arm that is unimplemented under \
             --preserve-encodings (generation.rs; see the preserve_encodings_supports_floats stub)",
        ),
        (
            "rangeop.inclusive.float",
            "a float-range newtype (`0.5..10.5`) wraps an f64 member, which hits the same native-float \
             `unimplemented!` under --preserve-encodings (generation.rs float arm; see the \
             preserve_encodings_supports_floats stub) — default-profile decode still replays its \
             boundary-violation reject vectors",
        ),
        (
            "rangeop.exclusive.float",
            "a float-range newtype (`0.5...10.5`) wraps an f64 member, which hits the same native-float \
             `unimplemented!` under --preserve-encodings (generation.rs float arm; see the \
             preserve_encodings_supports_floats stub) — default-profile decode still replays its \
             boundary-violation reject vectors",
        ),
        (
            "value.number.hexfloat",
            "a fixed float member (`m = [v: 0x1.8p+1]`) panics generation under --preserve-encodings \
             (generation.rs fixed-float deserialize arm `unimplemented!` — the same native-float class \
             as the rangeop.*.float rows; see the preserve_encodings_supports_floats stub) — \
             default-profile decode still replays its accept + FixedValueMismatch reject vectors",
        ),
        // NOT a float — a separate, pre-existing preserve gap surfaced by this gate. A CBOR tag on a
        // TYPE-CHOICE (`t = #6.10(int / tstr)` generates a rust enum) trips an explicit
        // `assert!(!cli.preserve_encodings)` in generation.rs's tagged-enum serialize path, guarding
        // an unimplemented case (its own `// TODO: how to even store these?`): the per-variant
        // encoding metadata preserve needs has no home on the enum. Tags on structs/arrays/maps
        // (contain.tag-content.type2.{array,map}, contain.tag-content.type.choice's non-choice
        // siblings) preserve fine — only the tag-over-choice combination is unimplemented. Default-
        // profile decode of this row is fully replayed above; only its preserve leg is skipped.
        (
            "contain.tag-content.type.choice",
            "tag over a type-choice enum is unimplemented under --preserve-encodings \
             (generation.rs `assert!(!cli.preserve_encodings)` in the tagged-enum serialize path, \
             with a standing `TODO: how to even store these?`) — a pre-existing generator gap, not a \
             decoder issue; the default-profile decode of this row still replays",
        ),
    ];
    // Rows that GENERATE + compile under preserve but re-encode a decoded accept vector to different
    // bytes (decodes Ok, `to_cbor_bytes()` != input). Empty at HEAD — no row exhibits this. A newly-
    // appearing byte-identity mismatch is a FINDING to triage, not something to bury here; adding it
    // needs a reason, and a listed row that starts round-tripping byte-identically fails the gate.
    const EXPECTED_MISMATCH: &[(&str, &str)] = &[];

    // (row id, encoding-variant label, reason) pairs whose DEFAULT-leg variant test legitimately fails
    // — a spec-equal re-encoding (indefinite framing, non-minimal width, chunked string, reversed map)
    // the generated decoder is over-strict about, or mis-decodes. Each entry is an HONEST finding
    // ledgered in `cddl-matrix/ROADMAP.md` § findings (a real decoder gap, NOT fixed here). Stale-
    // guarded: a listed (row, label) whose variant now decodes+re-encodes cleanly fails the gate, so a
    // closed gap can't rot into a silent skip.
    const ENCODING_VARIANT_SKIP: &[(&str, &str, &str)] = &[];

    // (row id, replay test-name, reason) pairs whose DEFAULT-leg replay test legitimately rejects
    // with an adjacent-duplicate error location segment (`Foo.Foo`). EMPTY at HEAD: a newly-appearing
    // doubled location is a generator double-annotation regression to triage, not something to bury
    // without a reason. Stale-guarded like the other replay ledgers.
    const DOUBLED_LOCATION_SKIP: &[(&str, &str, &str)] = &[];

    // (row id, header-mutant label, reason) pairs whose DEFAULT-leg header-mutant test legitimately
    // DECODES the mutated bytes Ok because the row's spec genuinely accepts them WITHOUT that
    // acceptance being evidenced by any committed accept vector — an `any`-typed row, or a choice arm
    // whose major the mint never happened to sample. (A flip landing on an EVIDENCED major — one the
    // row's own accept vectors demonstrate, like type.choice's bstr↔tstr flip — is already skipped at
    // DERIVATION time by `header_mutants`' evidenced-major skip, precisely so this ledger never
    // suppresses (row, label)-wide: a wide entry would also swallow a future genuine over-acceptance
    // on the row's non-ambiguous vectors.) EMPTY at HEAD. Stale-guarded: a listed entry whose mutant
    // no longer decodes Ok fails the gate. `trunc_head` mutants are ill-formed by construction and can
    // NEVER decode Ok — a `trunc_head` entry here is a hard error (asserted below), not a legitimate
    // skip.
    const HEADER_MUTANT_ACCEPT_SKIP: &[(&str, &str, &str)] = &[];
    // (row id, header-mutant label, reason) pairs whose DEFAULT-leg header-mutant test REJECTS the
    // mutated bytes but the error Display carries NO location naming the decoding type. EMPTY at HEAD:
    // the newtype-wrapper container reads (3a) and embedded/plain-group deserialize() header
    // scaffolding (3b) are now each wrapped in an `.annotate(<T>)` closure, so those rejections carry
    // a `failed in <T>` location. Stale-guarded: a listed entry whose mutant no longer rejects WITHOUT
    // a location fails the gate (below). The `from_cbor_bytes` `TrailingData` path (locationless by
    // construction) would belong here if a mutant decoded the item Ok and only the buffer-length check
    // rejected — none does at HEAD (that path is pinned by `error_display_formatting`'s TrailingData
    // no-location case instead).
    const HEADER_MUTANT_LOCATION_SKIP: &[(&str, &str, &str)] = &[];

    let catalog_path = std::path::Path::new("tests/decode_conformance/catalog.toml");
    let catalog_src = std::fs::read_to_string(catalog_path)
        .unwrap_or_else(|e| panic!("cannot read {catalog_path:?}: {e}"));
    let doc: toml::Value = toml::from_str(&catalog_src).expect("catalog.toml is valid TOML");
    let all_rows = doc
        .get("row")
        .and_then(|v| v.as_array())
        .expect("catalog.toml has [[row]] entries");
    // A truncated parse (bad slice, wrong path) must not pass vacuously: the committed corpus has 119
    // rows (104 active + 15 pinned/vectorless), so a read that sees far fewer means something broke.
    assert!(
        all_rows.len() >= 110,
        "catalog parsed only {} rows (expected >= 110) — truncated/incorrect parse",
        all_rows.len()
    );

    // Distil the active (has-vectors) rows; pinned/vectorless rows have nothing to replay.
    let mut rows: Vec<ReplayRow> = Vec::new();
    for row in all_rows {
        let vectors_toml = match row.get("vector").and_then(|v| v.as_array()) {
            Some(v) if !v.is_empty() => v,
            _ => continue,
        };
        let id = row.get("id").and_then(|v| v.as_str()).unwrap().to_string();
        let spec = row
            .get("spec")
            .and_then(|v| v.as_str())
            .unwrap_or_else(|| panic!("active row {id} is missing `spec`"))
            .to_string();
        let type_name = row
            .get("type_name")
            .and_then(|v| v.as_str())
            .unwrap_or_else(|| panic!("active row {id} is missing `type_name`"))
            .to_string();
        // `mode` drives the header-mutation leg's byte offset (holder rows prefix the item with the
        // `82 00` = `[0, _]` preamble); every minted active row carries it (catalog head comment).
        let mode = row
            .get("mode")
            .and_then(|v| v.as_str())
            .unwrap_or_else(|| panic!("active row {id} is missing `mode`"))
            .to_string();
        let vectors = vectors_toml
            .iter()
            .map(|v| {
                let hex = v.get("hex").and_then(|x| x.as_str()).unwrap().to_string();
                let expect = v.get("expect").and_then(|x| x.as_str()).unwrap();
                let class = v.get("class").and_then(|x| x.as_str());
                let expect_err = v
                    .get("expect_err")
                    .and_then(|x| x.as_str())
                    .map(|s| s.to_string());
                ReplayVector {
                    hex,
                    accept: expect == "accept",
                    over_acceptance: expect == "accept" && class == Some("over-acceptance"),
                    expect_err,
                }
            })
            .collect();
        rows.push(ReplayRow {
            id,
            spec,
            type_name,
            mode,
            vectors,
        });
    }

    let active_row_ids: std::collections::BTreeSet<&str> =
        rows.iter().map(|row| row.id.as_str()).collect();
    for (id, _) in PRESERVE_SKIP {
        assert!(
            active_row_ids.contains(id),
            "PRESERVE_SKIP names active catalog row `{id}` that is no longer replayed — stale pin, \
             remove or fix it"
        );
    }
    for (id, _) in EXPECTED_MISMATCH {
        assert!(
            active_row_ids.contains(id),
            "EXPECTED_MISMATCH names active catalog row `{id}` that is no longer replayed — \
             stale pin, remove or fix it"
        );
    }
    for (id, _, _) in ENCODING_VARIANT_SKIP {
        assert!(
            active_row_ids.contains(id),
            "ENCODING_VARIANT_SKIP names catalog row `{id}` that is not an active replayed row — \
             stale pin, remove or fix it"
        );
    }
    for (id, _, _) in DOUBLED_LOCATION_SKIP {
        assert!(
            active_row_ids.contains(id),
            "DOUBLED_LOCATION_SKIP names catalog row `{id}` that is not an active replayed row — \
             stale pin, remove or fix it"
        );
    }
    for (id, label, _) in HEADER_MUTANT_ACCEPT_SKIP {
        assert!(
            *label != "trunc_head",
            "HEADER_MUTANT_ACCEPT_SKIP lists a `trunc_head` mutant for `{id}` — trunc_head is \
             ill-formed by construction and can NEVER decode Ok, so this entry is impossible; a \
             trunc_head that decodes Ok is a real over-acceptance finding, not a skip"
        );
        assert!(
            active_row_ids.contains(id),
            "HEADER_MUTANT_ACCEPT_SKIP names catalog row `{id}` that is not an active replayed row — \
             stale pin, remove or fix it"
        );
    }
    for (id, _, _) in HEADER_MUTANT_LOCATION_SKIP {
        assert!(
            active_row_ids.contains(id),
            "HEADER_MUTANT_LOCATION_SKIP names catalog row `{id}` that is not an active replayed row \
             — stale pin, remove or fix it"
        );
    }

    let scratch_name = format!("cddl_codegen_decode_conformance_{:016x}", checkout_hash());
    // Hold for the whole gate: same-checkout concurrent runs serialize instead of clobbering each
    // other's crates via the `remove_dir_all` below (the `ir_conformance_corpus` pattern).
    let _scratch_lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let preserve_skip: std::collections::BTreeMap<&str, &str> =
        PRESERVE_SKIP.iter().copied().collect();
    let expected_mismatch: std::collections::BTreeMap<&str, &str> =
        EXPECTED_MISMATCH.iter().copied().collect();
    let encoding_variant_skip: std::collections::BTreeMap<(&str, &str), &str> =
        ENCODING_VARIANT_SKIP
            .iter()
            .map(|(r, l, why)| ((*r, *l), *why))
            .collect();
    let doubled_location_skip: std::collections::BTreeMap<(&str, &str), &str> =
        DOUBLED_LOCATION_SKIP
            .iter()
            .map(|(r, l, why)| ((*r, *l), *why))
            .collect();
    // (row, label) pairs whose variant test failed AND was suppressed by an ENCODING_VARIANT_SKIP
    // entry; the stale guard flags any listed entry that is NOT here (the gap closed).
    let mut variant_skip_still_failing: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new();
    // (row, replay test-name) pairs whose replay test failed AND was suppressed by a
    // DOUBLED_LOCATION_SKIP entry; the stale guard flags any listed entry that is NOT here.
    let mut doubled_location_skip_still_failing: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new();
    let header_mutant_accept_skip: std::collections::BTreeMap<(&str, &str), &str> =
        HEADER_MUTANT_ACCEPT_SKIP
            .iter()
            .map(|(r, l, why)| ((*r, *l), *why))
            .collect();
    let header_mutant_location_skip: std::collections::BTreeMap<(&str, &str), &str> =
        HEADER_MUTANT_LOCATION_SKIP
            .iter()
            .map(|(r, l, why)| ((*r, *l), *why))
            .collect();
    // (row, label) pairs whose header-mutant test failed AND was suppressed by a HEADER_MUTANT_*_SKIP
    // entry; the stale guards flag any listed entry that is NOT here (the gap closed).
    let mut header_accept_skip_still_failing: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new();
    let mut header_location_skip_still_failing: std::collections::BTreeSet<(String, String)> =
        std::collections::BTreeSet::new();

    let mut failures: Vec<String> = Vec::new();
    let mut rows_replayed = 0usize;
    let mut vectors_replayed = 0usize;
    // How many DEFAULT-leg encoding-variant tests were emitted (a vacuity floor guards against the
    // variant leg silently emitting nothing — a mutator that returned empty, or a broken loop).
    let mut variant_tests_total = 0usize;
    // How many DEFAULT-leg header-mutation reject tests were emitted (a vacuity floor guards the
    // header-mutation leg the same way).
    let mut header_tests_total = 0usize;
    // How many `class="constraint"` vectors actually had their rejection REASON asserted (Err whose
    // Display contained the pinned `expect_err`). A vacuity floor below guards against this collapsing
    // to near-zero (a broken match body, or the corpus losing its constraint vectors).
    let mut constraint_reason_asserts = 0usize;
    // How many `over_accept_{i}` tests were emitted (default leg). A completeness guard below asserts
    // this equals the catalog's over-acceptance vector count, so an emission arm that mislabels an
    // over-acceptance vector as a plain accept is caught even though the per-crate count is unchanged.
    let mut over_acceptance_tests_emitted = 0usize;
    let over_acceptance_catalog_total: usize = rows
        .iter()
        .flat_map(|r| &r.vectors)
        .filter(|v| v.over_acceptance)
        .count();

    for row in &rows {
        // ---- default profile: accept => Ok, reject pin => Err ----
        // Precompute the encoding-variant re-encodings of every accept vector (mint guarantees
        // definite-length minimal input, so `variants()` never panics — but wrap it in `catch_unwind`
        // and turn a panic into a loud gate failure naming row+hex rather than aborting the whole run).
        let mut variant_specs: Vec<(usize, String, Vec<u8>)> = Vec::new();
        for (i, vector) in row.vectors.iter().enumerate() {
            // SPEC-VALID accepts only — an over-acceptance vector is spec-INVALID, so re-encoding it is
            // meaningless (there is no spec-equal re-encoding of a spec-invalid instance).
            if !vector.spec_valid_accept() {
                continue;
            }
            let bytes = hex_to_bytes(&vector.hex);
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                cddl_encoding_fidelity::variants(&bytes)
            })) {
                Ok(list) => {
                    for (label, var_bytes) in list {
                        variant_specs.push((i, label.to_string(), var_bytes));
                    }
                }
                Err(_) => failures.push(format!(
                    "{}: encoding-fidelity variants() PANICKED on accept vector {} — mint guarantees \
                     definite-length minimal CBOR, so this is a harness/mutator bug, not a decoder \
                     finding",
                    row.id, vector.hex
                )),
            }
        }
        variant_tests_total += variant_specs.len();

        // Precompute the header-mutation reject mutants of every accept vector (`wrong_major` /
        // `trunc_head` pure byte transforms; `holder` rows carry the `82 00` preamble so the item
        // under test starts at byte 2). Wrapped in `catch_unwind` for the same reason as the variant
        // precompute — turn a mutator panic into a loud gate failure naming row+hex.
        //
        // `evidenced_majors` first: the leading-major classes (majors 0/1 merged, the
        // project_decode_conformance.ts § 6 merge) of the row's OWN accept vectors — the majors the
        // spec demonstrably accepts. `header_mutants` skips a `wrong_major` flip landing on an
        // evidenced class at DERIVATION time (the mutant would be ambiguous — possibly spec-valid,
        // e.g. type.choice's bstr↔tstr flip landing on the other `uint / tstr / bytes` string arm),
        // which is strictly better than ledger-suppressing (row, label)-wide: the row's other,
        // NON-ambiguous mutants (its uint vectors' 0→4 array flip) stay live against a future
        // genuine over-acceptance.
        let hdr_offset = if row.mode == "holder" { 2 } else { 0 };
        // SPEC-VALID accepts only evidence the majors the spec accepts — an over-acceptance vector's
        // major must NOT count as evidenced (that would let its own widening shape suppress a mutant).
        let evidenced_majors: std::collections::BTreeSet<u8> = row
            .vectors
            .iter()
            .filter(|v| v.spec_valid_accept())
            .map(|v| header_major_class(hex_to_bytes(&v.hex)[hdr_offset] >> 5))
            .collect();
        let mut header_specs: Vec<(usize, String, Vec<u8>)> = Vec::new();
        for (i, vector) in row.vectors.iter().enumerate() {
            // SPEC-VALID accepts only — header-mutating a spec-INVALID over-acceptance vector evidences
            // nothing (the base bytes are already not spec-valid).
            if !vector.spec_valid_accept() {
                continue;
            }
            let bytes = hex_to_bytes(&vector.hex);
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                header_mutants(&bytes, hdr_offset, &evidenced_majors)
            })) {
                Ok(list) => {
                    for (label, mut_bytes) in list {
                        header_specs.push((i, label.to_string(), mut_bytes));
                    }
                }
                Err(_) => failures.push(format!(
                    "{}: header_mutants() PANICKED on accept vector {} (mode={}) — the mutator is a \
                     pure byte transform over mint-guaranteed definite-minimal CBOR, so this is a \
                     harness/mutator bug, not a decoder finding",
                    row.id, vector.hex, row.mode
                )),
            }
        }
        header_tests_total += header_specs.len();

        let out = root.join(format!("{}__default", foreign_scratch_ident(&row.id)));
        let dgen = decode_replay_generate(&row.spec, &out, &[]);
        if !dgen.status.success() {
            failures.push(format!(
                "{}: default-profile generation failed (an active catalog row must generate)\n{}",
                row.id,
                String::from_utf8_lossy(&dgen.stderr)
            ));
            let _ = std::fs::remove_dir_all(&out);
            continue;
        }
        match decode_replay_run(
            &out,
            &row.type_name,
            &row.vectors,
            false,
            &target_dir,
            &variant_specs,
            &header_specs,
        ) {
            (Some(results), combined) => {
                if results.get(DOUBLED_LOCATION_HELPER_SELF_CHECK).copied() != Some(true) {
                    failures.push(format!(
                        "{}: replay helper self-check `{DOUBLED_LOCATION_HELPER_SELF_CHECK}` failed \
                         — the doubled-location invariant harness regressed. Captured output:\n{combined}",
                        row.id
                    ));
                }
                rows_replayed += 1;
                vectors_replayed += row.vectors.len();
                for (i, vector) in row.vectors.iter().enumerate() {
                    let hex = &vector.hex;
                    let name = if vector.over_acceptance {
                        format!("over_accept_{i}")
                    } else if vector.accept {
                        format!("accept_{i}")
                    } else {
                        format!("reject_{i}")
                    };
                    // Completeness: count over-acceptance vectors whose `over_accept_{i}` test was
                    // actually emitted (present in results). Asserted below == the catalog's
                    // over-acceptance vector count, so an emission arm that mislabeled it as `accept_{i}`
                    // (same total test count, so the per-crate completeness check would miss it) trips.
                    if vector.over_acceptance && results.contains_key(&name) {
                        over_acceptance_tests_emitted += 1;
                    }
                    let passed = results.get(&name).copied().unwrap_or(false);
                    if passed {
                        // A constraint vector that passed did so via the `expect_err` REASON assert
                        // (its test body is the `contains(..)` match) — count it for the vacuity floor.
                        if !vector.accept && vector.expect_err.is_some() {
                            constraint_reason_asserts += 1;
                        }
                        continue;
                    }
                    if vector.over_acceptance {
                        // An over-acceptance vector asserts the decoder STILL wrongly accepts it, so a
                        // FAILED test means the decoder now REJECTS — the pin flip. Attribute distinctly.
                        match classify_over_acceptance_failure(&combined, &name) {
                            OverAcceptanceFailureKind::NowRejected => failures.push(format!(
                                "{}: over-acceptance vector {hex} was REJECTED — the decoder no longer \
                                 wrongly accepts this spec-INVALID CBOR (the fix landed): promote this \
                                 vector to class=\"constraint\" with an expect_err, move the row id from \
                                 EXPECTED_ENFORCE_OVERACCEPTS to EXPECTED_ENFORCE_YES in \
                                 query_q4_directional.ts, update the ROADMAP finding, re-mint. Captured \
                                 output:\n{combined}",
                                row.id
                            )),
                            OverAcceptanceFailureKind::Unattributed => failures.push(format!(
                                "{}: over-acceptance vector {hex} failed but emitted no known marker — \
                                 unexpected. Captured output:\n{combined}",
                                row.id
                            )),
                        }
                    } else if vector.accept {
                        failures.push(format!(
                            "{}: default decode REJECTED a spec-valid accept vector {hex} — the \
                             decoder is over-strict (the exact class this gate exists to catch)",
                            row.id
                        ));
                    } else if vector.expect_err.is_some() {
                        // A constraint vector failed: distinguish "decoded Ok" (enforcement gap) from
                        // "rejected for the WRONG reason" via the grep-stable markers the test emits.
                        // The needle's trailing ':' (prefix-collision guard) lives in the classifier.
                        let expect = vector.expect_err.as_deref().unwrap_or("");
                        match classify_constraint_failure(&combined, &name) {
                            ConstraintFailureKind::DecodedOk => failures.push(format!(
                                "{}: constraint vector {hex} DECODED Ok — the generated decoder does \
                                 NOT enforce the constraint (enforcement gap); expected rejection \
                                 whose Display contains {expect:?}",
                                row.id
                            )),
                            ConstraintFailureKind::WrongReason => failures.push(format!(
                                "{}: constraint vector {hex} was rejected for the WRONG reason — its \
                                 error Display did NOT contain the pinned {expect:?}; either re-author \
                                 the catalog `expect_err` (after confirming the message genuinely \
                                 names the violated constraint) or this is a real wrong-reason \
                                 rejection to report. Captured Display in the run output below:\n{combined}",
                                row.id
                            )),
                            ConstraintFailureKind::DoubledLocation => {
                                if doubled_location_skip
                                    .contains_key(&(row.id.as_str(), name.as_str()))
                                {
                                    doubled_location_skip_still_failing
                                        .insert((row.id.clone(), name.clone()));
                                } else {
                                    failures.push(format!(
                                        "{}: constraint vector {hex} rejected with an adjacent-duplicate \
                                         error location segment — generator double-annotation regression; \
                                         triage: DOUBLED_LOCATION_SKIP. Captured output:\n{combined}",
                                        row.id
                                    ));
                                }
                            }
                            ConstraintFailureKind::Unattributed => failures.push(format!(
                                "{}: constraint vector {hex} failed its reason assert but emitted \
                                 no known marker — unexpected; full output:\n{combined}",
                                row.id
                            )),
                        }
                    } else {
                        failures.push(format!(
                            "{}: reject pin {hex} now DECODES Ok — bug apparently fixed or decoder \
                             loosened; re-triage/unpin the catalog row (re-bless protection)",
                            row.id
                        ));
                    }
                }
                // ---- encoding-variant results (default leg only) ----
                for (i, label, _) in &variant_specs {
                    let name = format!("accept_{i}_var_{label}");
                    let orig_hex = &row.vectors[*i].hex;
                    let passed = results.get(&name).copied().unwrap_or(false);
                    let skipped =
                        encoding_variant_skip.contains_key(&(row.id.as_str(), label.as_str()));
                    if passed {
                        // A passing skip-listed variant is a closed gap — the stale guard (below) fires
                        // because this (row, label) never lands in `variant_skip_still_failing`.
                        continue;
                    }
                    if skipped {
                        variant_skip_still_failing.insert((row.id.clone(), label.clone()));
                        continue;
                    }
                    // The needle's trailing ':' (prefix-collision guard) lives in the classifier.
                    match classify_variant_failure(&combined, &name) {
                        VariantFailureKind::Rejected => failures.push(format!(
                            "{}: encoding variant `{label}` of accept vector {orig_hex} was REJECTED by \
                             the default decoder — a spec-EQUAL re-encoding (indefinite framing / \
                             non-minimal width / chunked string / reversed map) the decoder is \
                             over-strict about (the motivating class). If it is a known decoder gap, \
                             ledger it in cddl-matrix/ROADMAP.md § findings and add ({}, {label}) to \
                             ENCODING_VARIANT_SKIP. Captured output:\n{combined}",
                            row.id, row.id
                        )),
                        VariantFailureKind::ValueMismatch => failures.push(format!(
                            "{}: encoding variant `{label}` of accept vector {orig_hex} decoded to a \
                             DIFFERENT value than the original (its default re-encoding differs) — a \
                             mis-decode of a spec-equal re-encoding. Captured output:\n{combined}",
                            row.id
                        )),
                        VariantFailureKind::OrigDecodeFailed => failures.push(format!(
                            "{}: the ORIGINAL accept vector {orig_hex} failed to decode inside variant \
                             test `{name}` — unexpected (it is an accept vector). Captured output:\n{combined}",
                            row.id
                        )),
                        VariantFailureKind::Unattributed => failures.push(format!(
                            "{}: encoding variant test `{name}` failed but emitted no known marker — \
                             unexpected. Captured output:\n{combined}",
                            row.id
                        )),
                    }
                }
                // ---- header-mutation results (default leg only) ----
                for (i, label, _) in &header_specs {
                    let name = format!("accept_{i}_hdr_{label}");
                    let orig_hex = &row.vectors[*i].hex;
                    let passed = results.get(&name).copied().unwrap_or(false);
                    if passed {
                        // A passing skip-listed mutant is a closed gap — the stale guards (below) fire
                        // because this (row, label) never lands in the still-failing sets.
                        continue;
                    }
                    // The needle's trailing ':' (prefix-collision guard) lives in the classifier.
                    match classify_header_mutant_failure(&combined, &name) {
                        HeaderMutantFailureKind::DecodedOk => {
                            if header_mutant_accept_skip
                                .contains_key(&(row.id.as_str(), label.as_str()))
                            {
                                header_accept_skip_still_failing
                                    .insert((row.id.clone(), label.clone()));
                            } else {
                                failures.push(format!(
                                    "{}: header mutant `{label}` of accept vector {orig_hex} DECODED \
                                     Ok — the header-mutated bytes were accepted, and flips landing \
                                     on a major the row's own accept vectors evidence are ALREADY \
                                     skipped at derivation, so this acceptance is unevidenced. If the \
                                     row's spec genuinely accepts the bytes anyway (an `any`-typed \
                                     row, a choice arm whose major the mint never sampled), ledger \
                                     ({}, {label}) in HEADER_MUTANT_ACCEPT_SKIP with a reason naming \
                                     the accepting spec arm. If you CANNOT justify it from the spec, \
                                     this is a real OVER-ACCEPTANCE finding — do NOT ledger it; report \
                                     it. (A `trunc_head` mutant here is ALWAYS a finding: it is \
                                     ill-formed by construction.) Captured output:\n{combined}",
                                    row.id, row.id
                                ));
                            }
                        }
                        HeaderMutantFailureKind::NoLocation => {
                            if header_mutant_location_skip
                                .contains_key(&(row.id.as_str(), label.as_str()))
                            {
                                header_location_skip_still_failing
                                    .insert((row.id.clone(), label.clone()));
                            } else {
                                failures.push(format!(
                                    "{}: header mutant `{label}` of accept vector {orig_hex} was \
                                     rejected but the error Display carries NO location naming the \
                                     type (`failed in {ty}`). Header scaffolding (records, \
                                     embedded/plain-groups) and newtype-wrapper container reads are \
                                     all annotated now, so this is a real annotation regression — \
                                     investigate before ledgering. The only known-legitimate \
                                     locationless path is `from_cbor_bytes` `TrailingData` (a mutant \
                                     that decodes the item Ok and trips only the buffer-length \
                                     check); if that is genuinely what happened, ledger ({}, {label}) \
                                     in HEADER_MUTANT_LOCATION_SKIP naming that path. \
                                     Captured output:\n{combined}",
                                    row.id,
                                    row.id,
                                    ty = row.type_name
                                ));
                            }
                        }
                        HeaderMutantFailureKind::DoubledLocation => {
                            if doubled_location_skip.contains_key(&(row.id.as_str(), name.as_str()))
                            {
                                doubled_location_skip_still_failing
                                    .insert((row.id.clone(), name.clone()));
                            } else {
                                failures.push(format!(
                                    "{}: header mutant `{label}` of accept vector {orig_hex} rejected \
                                     with an adjacent-duplicate error location segment — generator \
                                     double-annotation regression; triage: DOUBLED_LOCATION_SKIP. \
                                     Captured output:\n{combined}",
                                    row.id
                                ));
                            }
                        }
                        HeaderMutantFailureKind::Unattributed => failures.push(format!(
                            "{}: header mutant test `{name}` failed but emitted no known marker — \
                             unexpected. Captured output:\n{combined}",
                            row.id
                        )),
                    }
                }
            }
            (None, combined) => {
                failures.push(format!(
                    "{}: default-profile crate did not compile / produced no replay results\n{}",
                    row.id, combined
                ));
            }
        }
        let _ = std::fs::remove_dir_all(&out);

        // ---- preserve profile: SPEC-VALID ACCEPT vectors only, decode-Ok AND byte-identity ----
        // Over-acceptance vectors are excluded — the pin is exactly one assertion on the default leg (one
        // flip signal, no preserve-leg noise), and byte-identity of a spec-invalid instance is meaningless.
        let accepts: Vec<ReplayVector> = row
            .vectors
            .iter()
            .filter(|v| v.spec_valid_accept())
            .cloned()
            .collect();
        let skip_reason = preserve_skip.get(row.id.as_str()).copied();
        let mismatch_reason = expected_mismatch.get(row.id.as_str()).copied();

        let pout = root.join(format!("{}__preserve", foreign_scratch_ident(&row.id)));
        let pgen = decode_replay_generate(&row.spec, &pout, &["--preserve-encodings=true"]);
        let preserve_ok: bool;
        if !pgen.status.success() {
            // Generation aborted (the float class). A finding unless allowlisted.
            preserve_ok = false;
            if skip_reason.is_none() {
                failures.push(format!(
                    "{}: preserve-profile generation failed and the row is NOT on PRESERVE_SKIP — a \
                     finding: either a real preserve generation regression, or add it to \
                     PRESERVE_SKIP with an honest reason\n{}",
                    row.id,
                    String::from_utf8_lossy(&pgen.stderr)
                ));
            }
        } else {
            match decode_replay_run(&pout, &row.type_name, &accepts, true, &target_dir, &[], &[]) {
                (Some(results), combined) => {
                    if results.get(DOUBLED_LOCATION_HELPER_SELF_CHECK).copied() != Some(true) {
                        preserve_ok = false;
                        failures.push(format!(
                            "{}: preserve replay helper self-check `{DOUBLED_LOCATION_HELPER_SELF_CHECK}` \
                             failed — the doubled-location invariant harness regressed. Captured \
                             output:\n{combined}",
                            row.id
                        ));
                    } else {
                        let all_pass = results.values().all(|&p| p);
                        preserve_ok = all_pass;
                        if !all_pass {
                            let byte_mismatch = combined.contains("PRESERVE_BYTE_MISMATCH");
                            let decode_failed = combined.contains("PRESERVE_DECODE_FAILED");
                            if mismatch_reason.is_none() && skip_reason.is_none() {
                                let kind = if byte_mismatch {
                                    "re-encodes to DIFFERENT bytes (decodes Ok but `to_cbor_bytes()` != \
                                 input — the preserve byte-identity contract is broken)"
                                } else if decode_failed {
                                    "fails to DECODE an accept vector under preserve"
                                } else {
                                    "fails preserve replay for an unrecognized reason"
                                };
                                failures.push(format!(
                                "{}: preserve profile {kind} — a finding: report it and pin with a \
                                 reason (PRESERVE_SKIP for gen/compile, EXPECTED_MISMATCH for \
                                 byte-identity)\n{combined}",
                                row.id
                            ));
                            }
                        }
                    }
                }
                (None, combined) => {
                    // Compiled-away: no result lines => the preserve crate did not build.
                    preserve_ok = false;
                    if skip_reason.is_none() {
                        failures.push(format!(
                            "{}: preserve-profile crate did not compile and the row is NOT on \
                             PRESERVE_SKIP — a finding: fix it or add it to PRESERVE_SKIP with an \
                             honest reason\n{combined}",
                            row.id
                        ));
                    }
                }
            }
        }
        // Stale-entry guards: an allowlisted row that now fully round-trips under preserve must be
        // removed from its list (the gap it documents has closed).
        if preserve_ok && skip_reason.is_some() {
            failures.push(format!(
                "{}: on PRESERVE_SKIP but now generates + replays cleanly under preserve — the gap \
                 closed; remove it from PRESERVE_SKIP",
                row.id
            ));
        }
        if preserve_ok && mismatch_reason.is_some() {
            failures.push(format!(
                "{}: on EXPECTED_MISMATCH but now re-encodes byte-identically under preserve — \
                 remove it from EXPECTED_MISMATCH",
                row.id
            ));
        }
        let _ = std::fs::remove_dir_all(&pout);
    }

    let _ = std::fs::remove_dir_all(&root);

    // Stale-entry guard for ENCODING_VARIANT_SKIP: a listed (row, label) whose variant test no longer
    // fails (it now decodes+re-encodes cleanly, or the row/label stopped emitting that variant) must be
    // removed — the gap it documents has closed. Mirrors the PRESERVE_SKIP stale guard.
    for (id, label, _reason) in ENCODING_VARIANT_SKIP {
        if !variant_skip_still_failing.contains(&(id.to_string(), label.to_string())) {
            failures.push(format!(
                "ENCODING_VARIANT_SKIP names ({id}, {label}) but that variant no longer FAILS — the \
                 gap closed (or the row/label no longer emits a variant test); remove the entry \
                 (stale pin)"
            ));
        }
    }
    for (id, label, _reason) in DOUBLED_LOCATION_SKIP {
        if !doubled_location_skip_still_failing.contains(&(id.to_string(), label.to_string())) {
            failures.push(format!(
                "DOUBLED_LOCATION_SKIP names ({id}, {label}) but that replay test no longer rejects \
                 with an adjacent-duplicate location segment — the double-annotation gap closed (or \
                 the row/test no longer emits); remove the entry (stale pin)"
            ));
        }
    }
    // Stale-entry guards for the header-mutation ledgers, mirroring the ENCODING_VARIANT_SKIP guard:
    // a listed (row, label) whose mutant test no longer fails IN THAT WAY has had its gap close.
    for (id, label, _reason) in HEADER_MUTANT_ACCEPT_SKIP {
        if !header_accept_skip_still_failing.contains(&(id.to_string(), label.to_string())) {
            failures.push(format!(
                "HEADER_MUTANT_ACCEPT_SKIP names ({id}, {label}) but that mutant no longer DECODES Ok \
                 — the row stopped accepting the mutated bytes (or no longer emits that mutant); \
                 remove the entry (stale pin)"
            ));
        }
    }
    for (id, label, _reason) in HEADER_MUTANT_LOCATION_SKIP {
        if !header_location_skip_still_failing.contains(&(id.to_string(), label.to_string())) {
            failures.push(format!(
                "HEADER_MUTANT_LOCATION_SKIP names ({id}, {label}) but that mutant no longer rejects \
                 WITHOUT a location — the annotation gap closed (or it no longer emits that mutant); \
                 remove the entry (stale pin)"
            ));
        }
    }

    // Vacuity floors from the real minted corpus (104 active rows, 915 vectors at HEAD; floors set
    // just under so ordinary corpus churn doesn't false-fail, while a collapsed parse or a
    // silently-degraded generation loop that replays almost nothing still fails the gate).
    assert!(
        rows_replayed >= 95,
        "only {rows_replayed} catalog rows were replayed (expected >= 95) — the corpus or the \
         generation loop shrank"
    );
    assert!(
        vectors_replayed >= 850,
        "only {vectors_replayed} vectors were replayed (expected >= 850) — the corpus or the \
         generation loop shrank"
    );
    // Reason-assert floor: the corpus holds 44 `class="constraint"` vectors at HEAD, each asserting its
    // rejection REASON (not just is_err). Floor set just under so ordinary churn doesn't false-fail,
    // while a corpus (or a catalog parse) that lost its constraint vectors still trips the gate. The
    // OTHER vacuity channel — decode_replay_run's constraint arm regressing to the plain-is_err body
    // while the catalog field stays present — is guarded at the emission site itself (the
    // CONSTRAINT_WRONG_REASON body assert in decode_replay_run), which this count cannot see.
    assert!(
        constraint_reason_asserts >= 40,
        "only {constraint_reason_asserts} constraint vectors had their rejection REASON asserted \
         (expected >= 40) — the `expect_err` reason pin looks disabled or the corpus lost its \
         constraint vectors"
    );
    // Over-acceptance completeness: the emitted `over_accept_*` test count must equal the catalog's
    // class="over-acceptance" vector count (mirrors the constraint match-arm regression guard). A
    // mismatch means an over-acceptance vector fell through to the plain-accept arm (or a stale count).
    assert_eq!(
        over_acceptance_tests_emitted, over_acceptance_catalog_total,
        "emitted {over_acceptance_tests_emitted} over_accept_* test(s) but the catalog holds \
         {over_acceptance_catalog_total} class=\"over-acceptance\" vector(s) — the over-acceptance \
         emission arm regressed (a vector mislabeled as a plain accept, or the catalog parse drifted)"
    );
    // Variant-test floor: the DEFAULT-leg encoding-variant leg must actually emit its tests (4487 from
    // the 1052 accept vectors when the floor was set — an observed baseline, not a current count).
    // Floor set just under the measured count so ordinary corpus
    // churn doesn't false-fail, while a mutator that returned empty (or a broken variant loop) that
    // emits almost nothing still trips the gate.
    assert!(
        variant_tests_total >= 4200,
        "only {variant_tests_total} encoding-variant tests were emitted (expected >= 4200) — the \
         variant leg looks disabled or the corpus lost its accept vectors"
    );
    // Header-mutant floor: the DEFAULT-leg header-mutation leg must actually emit its tests (2042 at
    // HEAD — 1046 `wrong_major` (one per accept vector, minus the 6 type.choice string vectors whose
    // flip lands on an evidenced major and is skipped at derivation) + 996 `trunc_head` (per accept
    // vector whose head is not major-7/indefinite)). Floor set just under the measured count so
    // ordinary corpus churn doesn't false-fail, while a mutator that returned empty (or a broken
    // header loop) that emits almost nothing still trips the gate.
    assert!(
        header_tests_total >= 1900,
        "only {header_tests_total} header-mutation tests were emitted (expected >= 1900) — the \
         header-mutation leg looks disabled or the corpus lost its accept vectors"
    );
    assert!(
        failures.is_empty(),
        "decode-conformance replay found {} problem(s):\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// A catalog row id (`occur.optional`, `contain.array-element.type.choice`) -> a filesystem-safe
/// scratch dir fragment (the ids carry `.` and `-`, fine for paths but kept tidy/unique here).
fn foreign_scratch_ident(id: &str) -> String {
    id.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Retires the silent half of the AGENTS.md bin/lib module duplication gotcha: production modules
/// must be declared from both crate roots, while `tests` remains bin-only by design.
#[test]
fn bin_and_lib_production_module_declarations_match() {
    use std::collections::BTreeSet;

    fn declared_modules(source: &str) -> BTreeSet<String> {
        source
            .lines()
            .filter_map(|line| {
                let rest = line
                    .strip_prefix("pub mod ")
                    .or_else(|| line.strip_prefix("pub(crate) mod "))
                    .or_else(|| line.strip_prefix("mod "))?;
                let (name, _) = rest.split_once(';')?;
                Some(name.to_owned())
            })
            .filter(|name| name != "tests")
            .collect()
    }

    let main_path = concat!(env!("CARGO_MANIFEST_DIR"), "/src/main.rs");
    let lib_path = concat!(env!("CARGO_MANIFEST_DIR"), "/src/lib.rs");
    let main = std::fs::read_to_string(main_path)
        .unwrap_or_else(|e| panic!("cannot read {main_path}: {e}"));
    let lib =
        std::fs::read_to_string(lib_path).unwrap_or_else(|e| panic!("cannot read {lib_path}: {e}"));

    let main_modules = declared_modules(&main);
    let lib_modules = declared_modules(&lib);
    assert!(
        !main_modules.is_empty() && !lib_modules.is_empty(),
        "parsed zero module declarations from a crate root — the line-based parse drifted from \
         the source format and this gate went vacuous; fix declared_modules"
    );

    if let Some(module) = main_modules.difference(&lib_modules).next() {
        panic!(
            "module `{module}` declared in src/main.rs but missing from src/lib.rs — production \
             modules must be declared in BOTH (see AGENTS.md § bin/lib module duplication); lib \
             omissions are silent, the library ships without the module"
        );
    }
    if let Some(module) = lib_modules.difference(&main_modules).next() {
        panic!(
            "module `{module}` declared in src/lib.rs but missing from src/main.rs — production \
             modules must be declared in BOTH (see AGENTS.md § bin/lib module duplication); bin \
             omissions fail loudly, but the crate roots still need to stay aligned"
        );
    }
}

/// Lexer/self-cancel round-trip over the real generated corpus: for every generated `.rs` under the
/// tool-owned `src/generated/**` trees, `preserve(content, content)` must lex cleanly and be a
/// byte-identical no-op (the CODEGEN_HEADER banner self-cancels; no generated line is mistaken for a
/// user comment). Runs across the flag profiles so the preserve/json codegen shapes are exercised
/// too. In-process via `generated_strings` (no disk/compile — pure string property).
#[test]
fn comment_preserve_lexer_round_trip_over_corpus() {
    use clap::Parser;
    // Each input paired with the flag profile it is known-safe under (core supports only default;
    // preserve/json each have their own fixture) — the same pairings the snapshot suite uses.
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
        // Generator-comment-rich shapes: `--emit-tests` stamps TRAILING `// <cddl>` comments and the
        // comment DSL emits `///` doc blocks — both must self-cancel (not spam compile_error blocks
        // or read as user comments; the exact regression the first-principles design review found).
        ("tests/core/input.cddl", &["--emit-tests=true"]),
        ("tests/comment-dsl/input.cddl", &[]),
    ];
    for (profile, flags) in cases {
        let input_path = format!("{}/{profile}", env!("CARGO_MANIFEST_DIR"));
        let mut args = vec![
            "cddl-codegen",
            "--input",
            &input_path,
            "--output",
            "comment_preserve_roundtrip_unused",
            "--wasm=true",
        ];
        args.extend_from_slice(flags);
        let cli = crate::cli::Cli::parse_from(args);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("generation failed for profile {profile}: {e}"));
        for (path, content) in &files {
            if !crate::generation::is_preservable_generated_path(path) {
                continue;
            }
            let res = crate::comment_preserve::preserve(content, content).unwrap_or_else(|e| {
                panic!("lexer rejected generated file {path} (profile {profile}): {e}")
            });
            assert!(
                !res.changed,
                "self-preserve must be a no-op for {path} (profile {profile}) — a generated line was \
                 mistaken for a user comment"
            );
            assert_eq!(
                res.content, *content,
                "self-preserve altered {path} (profile {profile})"
            );
        }
    }
}

/// End-to-end comment preservation over the real disk export path (`generate_to_disk`): inject
/// own-line comments into a generated file, re-export against the unchanged spec (comments survive
/// byte-stably), then change the spec so one type is rewritten — a comment on an unchanged type
/// survives, while a comment on the rewritten type's changed statement becomes a `compile_error!`
/// block. `--wasm=false` keeps it to the single rust crate.
#[test]
fn comment_preservation_disk_round_trip() {
    use clap::Parser;
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_comment_rt_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint, y: tstr]\nbar = [z: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=true",
    ]);
    let mod_rs = out.join("rust/src/generated/mod.rs");
    let wasm_mod_rs = out.join("wasm/src/generated/mod.rs");
    let error_rs = out.join("rust/src/generated/error.rs");

    // First export: pristine generated output.
    crate::api::generate_to_disk(&cli).unwrap();
    let mut content = std::fs::read_to_string(&mod_rs).unwrap();
    assert!(
        content.contains("pub struct Bar"),
        "unexpected output:\n{content}"
    );

    // Inject own-line comments across the trees: one on the stable `Bar` type and one above the
    // `Self { … }` literal inside `impl Foo`'s constructor (a statement the spec change below will
    // rewrite) in the rust tree; one in the wasm tree; one in the statically-sourced `error.rs`
    // (which takes a different write path in `export` and must be covered by the same overlay).
    content = content.replace("pub struct Bar", "// KEEP BAR\npub struct Bar");
    let foo_pos = content.find("impl Foo {").expect("impl Foo missing");
    let self_rel = content[foo_pos..]
        .find("Self {")
        .expect("Self literal missing");
    let self_abs = foo_pos + self_rel;
    let line = content[..self_abs].rfind('\n').unwrap() + 1;
    content.insert_str(line, "        // FOO NEW NOTE\n");
    std::fs::write(&mod_rs, &content).unwrap();
    let wasm_content = std::fs::read_to_string(&wasm_mod_rs)
        .unwrap()
        .replace("pub struct Bar", "// KEEP WASM BAR\npub struct Bar");
    assert!(
        wasm_content.contains("// KEEP WASM BAR"),
        "wasm wrapper for Bar missing:\n{wasm_content}"
    );
    std::fs::write(&wasm_mod_rs, &wasm_content).unwrap();
    // Anchor inside the Display impl on the block-arm whose trailing comma rustfmt strips from the
    // raw static: this is the token-drift-sensitive spot — if export ever hands the overlay
    // non-rustfmt-stable content again, THIS comment gets trapped on the second regen.
    let mut error_content = std::fs::read_to_string(&error_rs).unwrap();
    let arm = error_content
        .find("DeserializeFailure::DefiniteLenMismatch(found")
        .expect("DefiniteLenMismatch match arm missing from error.rs");
    let arm_line = error_content[..arm].rfind('\n').unwrap() + 1;
    error_content.insert_str(arm_line, "            // KEEP ERROR NOTE\n");
    std::fs::write(&error_rs, &error_content).unwrap();

    // Second export, unchanged spec: all comments survive; nothing fails loudly.
    crate::api::generate_to_disk(&cli).unwrap();
    let second = std::fs::read_to_string(&mod_rs).unwrap();
    assert!(
        second.contains("// KEEP BAR"),
        "comment on Bar lost:\n{second}"
    );
    assert!(
        second.contains("// FOO NEW NOTE"),
        "comment in Foo lost:\n{second}"
    );
    assert!(
        !second.contains("compile_error!"),
        "an unchanged regen must not fail loudly:\n{second}"
    );
    let wasm_second = std::fs::read_to_string(&wasm_mod_rs).unwrap();
    assert!(
        wasm_second.contains("// KEEP WASM BAR"),
        "wasm-tree comment lost:\n{wasm_second}"
    );
    let error_second = std::fs::read_to_string(&error_rs).unwrap();
    assert!(
        error_second.contains("// KEEP ERROR NOTE"),
        "error.rs comment lost (static write path bypassed the overlay):\n{error_second}"
    );

    // Third export, still unchanged: a byte-identical fixed point. error.rs is the load-bearing
    // check: its preserve-rewrite is written rustfmt'd, so if export handed the overlay content
    // whose rustfmt form differs by a token, the comment placed by export 2 would be trapped in a
    // compile_error HERE (run 3, old=rustfmt'd vs new=raw) with zero input changes.
    crate::api::generate_to_disk(&cli).unwrap();
    let third = std::fs::read_to_string(&mod_rs).unwrap();
    assert_eq!(
        second, third,
        "comment preservation must reach a fixed point"
    );
    let error_third = std::fs::read_to_string(&error_rs).unwrap();
    assert_eq!(
        error_second, error_third,
        "error.rs must reach a fixed point (export must hand the overlay rustfmt-stable content)"
    );
    assert!(
        !error_third.contains("compile_error!"),
        "a preserved error.rs comment must not be trapped by an unchanged regen:\n{error_third}"
    );

    // Change the spec so `Foo` is rewritten (extra field) while `Bar` is untouched.
    std::fs::write(
        &input,
        "foo = [x: uint, y: tstr, w: uint]\nbar = [z: uint]\n",
    )
    .unwrap();
    crate::api::generate_to_disk(&cli).unwrap();
    let changed = std::fs::read_to_string(&mod_rs).unwrap();
    assert!(
        changed.contains("// KEEP BAR"),
        "comment on the unchanged Bar type must survive a spec change elsewhere:\n{changed}"
    );
    assert!(
        changed.contains("compile_error!") && changed.contains("cddl-codegen:unpreserved-comment"),
        "comment on the rewritten Foo statement must become a fail-loudly block:\n{changed}"
    );
    assert!(
        changed.contains("FOO NEW NOTE"),
        "the trapped comment must appear in the fail-loudly message:\n{changed}"
    );

    // Re-export with the changed spec untouched: the sentinel block must carry forward VERBATIM —
    // a byte-identical fixed point through the fail-loudly path and the extra rustfmt pass.
    crate::api::generate_to_disk(&cli).unwrap();
    let changed_again = std::fs::read_to_string(&mod_rs).unwrap();
    assert_eq!(
        changed, changed_again,
        "the fail-loudly block must reach a byte-identical fixed point"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// A broken existing generated file is a hard error naming the file — never a silent clobber:
/// both an unlexable one (valid UTF-8, unterminated string) and an unreadable one (not UTF-8).
/// `--no-preserve-comments` restores the plain clobber for the same dir.
#[test]
fn comment_preservation_broken_existing_file_hard_errors() {
    use clap::Parser;
    let scratch = std::env::temp_dir().join(format!(
        "cddl_codegen_comment_broken_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [x: uint]\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]);
    let mod_rs = out.join("rust/src/generated/mod.rs");
    crate::api::generate_to_disk(&cli).unwrap();

    // Unlexable: an unterminated string literal.
    std::fs::write(&mod_rs, "pub const BROKEN: &str = \"unterminated;\n").unwrap();
    let err = crate::api::generate_to_disk(&cli).expect_err("unlexable file must hard-error");
    assert!(
        err.to_string().contains("mod.rs"),
        "error must name the file: {err}"
    );

    // Unreadable: invalid UTF-8.
    std::fs::write(&mod_rs, [0xC3, 0x28, b'\n']).unwrap();
    let err = crate::api::generate_to_disk(&cli).expect_err("non-UTF-8 file must hard-error");
    assert!(
        err.to_string().contains("mod.rs"),
        "error must name the file: {err}"
    );

    // The escape hatch clobbers pristine over the broken file.
    let mut args: Vec<String> = [
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    args.push("--no-preserve-comments".to_owned());
    let cli_off = crate::cli::Cli::parse_from(args);
    crate::api::generate_to_disk(&cli_off).unwrap();
    let content = std::fs::read_to_string(&mod_rs).unwrap();
    assert!(
        content.contains("pub struct Foo"),
        "clobber failed:\n{content}"
    );

    let _ = std::fs::remove_dir_all(&scratch);
}

/// Every statically-sourced `.rs` under the generated trees must be written rustfmt-stable: a
/// preserve-rewrite is written rustfmt'd, so if export hands the overlay content whose rustfmt
/// form differs by even one token (the raw static's block-arm trailing comma was the live case),
/// run N+1's fresh tokens mismatch run N's written tokens and an already-placed comment gets
/// trapped in a compile_error with no input change. The spec uses `[+ T]` + `{+ k => v}` under
/// --preserve-encodings so all four statics (error, ordered_hash_map, non_empty, non_empty_map)
/// are exported.
#[test]
fn comment_preservation_static_files_rustfmt_stable() {
    use clap::Parser;
    let scratch =
        std::env::temp_dir().join(format!("cddl_codegen_static_fmt_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&scratch);
    std::fs::create_dir_all(&scratch).unwrap();
    let input = scratch.join("input.cddl");
    std::fs::write(&input, "foo = [+ uint]\nbar = {+ uint => uint}\n").unwrap();
    let out = scratch.join("crate");
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen",
        "--input",
        input.to_str().unwrap(),
        "--output",
        out.to_str().unwrap(),
        "--wasm=false",
        "--preserve-encodings=true",
    ]);
    crate::api::generate_to_disk(&cli).unwrap();
    let statics = [
        "error.rs",
        "ordered_hash_map.rs",
        "non_empty.rs",
        "non_empty_map.rs",
    ];
    for name in statics {
        let path = out.join("rust/src/generated").join(name);
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("expected static {name} missing: {e}"));
        let formatted = crate::generation::rustfmt_generated_string(&content).unwrap();
        assert_eq!(
            formatted.as_ref(),
            content,
            "{name} was written non-rustfmt-stable — the overlay would trap comments on the \
             second unchanged regen"
        );
    }
    let _ = std::fs::remove_dir_all(&scratch);
}
