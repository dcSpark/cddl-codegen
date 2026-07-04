//! Input panic-robustness catalog.
//!
//! Feeds malformed / edge-case spec inputs (`tests/robustness/*.cddl`) to the generator inside
//! `catch_unwind` and snapshots the OUTCOME of each — `ok` / `error (graceful)` / `PANIC`. This is
//! a robustness scorecard, not an output-regression test: it catches a refactor that makes a
//! previously-graceful input newly panic (shows up as a snapshot diff), and when a current panic
//! is fixed its entry flips (re-bless then).
//!
//! NB: a NEW `PANIC` is a regression — the generator must reject malformed input with a clean
//! error, never `panic!`/`assert!`. A committed `PANIC` entry is a tracked-known rejection whose
//! fixture comments say why it's pinned (e.g. `map_entry_no_key`); making it graceful is a fix.
//! The catalog deliberately records only the outcome *category*
//! (not panic messages/line numbers) so it stays stable across refactors that don't change behaviour.

use crate::cli::Cli;
use clap::Parser;

/// The global panic hook is process-wide, so the two tests that silence it (`input_robustness_catalog`
/// and `unsupported_construct_panic_catalog`) must not run their take/set/restore concurrently — an
/// interleave could leave the silent hook installed for the rest of the run. Serialize them on this lock
/// (poison-tolerant: a panic mid-section only means the *other* test re-silences, which is harmless).
static PANIC_HOOK_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Silence panic output from THIS test's thread for the duration of `f` (the deliberate
/// `catch_unwind` probes would otherwise spew every expected panic). The hook is process-global
/// and `cargo test` runs tests concurrently, so a blanket no-op hook would also eat the panic
/// message of any UNRELATED test that fails during the window — its failure would report with no
/// diagnostics. Filter by thread id instead, delegating other threads' panics to the
/// previously-installed hook.
fn with_thread_silenced_panics<T>(f: impl FnOnce() -> T) -> T {
    let _guard = PANIC_HOOK_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    let prev: std::sync::Arc<dyn Fn(&std::panic::PanicHookInfo) + Send + Sync> =
        std::sync::Arc::from(std::panic::take_hook());
    let silenced = std::thread::current().id();
    let delegate = prev.clone();
    std::panic::set_hook(Box::new(move |info| {
        if std::thread::current().id() != silenced {
            delegate(info)
        }
    }));
    let out = f();
    let _ = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| prev(info)));
    out
}

/// Every construct the matrix marks `status = "supported"` (features + control-ops) must drive the
/// generator without panicking or erroring. Broadens the old hand-synced 16-entry prelude list to the
/// matrix's full supported surface (`tests/matrix_supported/*.cddl`, projected by
/// `cddl-matrix/project_robustness.ts`). A failure is a generator regression *or* matrix↔generator drift.
///
/// Default profile, run under BOTH `--wasm=false` (the flags `verify.ts` probed with, so the outcome
/// tracks the matrix verdict) AND `--wasm=true` (the wasm-binding emission path — the old prelude test
/// ran this, and the corpus compile-gate doesn't cover the rust-only / user-code constructs this guard uniquely
/// holds, e.g. `ext.extern`/`ext.raw_bytes`/`number`/`time`). Default-profile means those four all
/// *generate* fine here (they'd only fail under `--preserve-encodings` / `cargo check` respectively).
#[test]
fn all_supported_constructs_generate() {
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

    // catch_unwind (without touching the global panic hook, to avoid racing other tests) so we report
    // *all* failing constructs at once rather than aborting on the first.
    let mut failures = Vec::new();
    for path in &inputs {
        let id = path.file_stem().unwrap().to_str().unwrap();
        for wasm in ["false", "true"] {
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "matrix_supported_unused",
                "--wasm",
                wasm,
            ]);
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::api::generated_strings(&cli)
            })) {
                Ok(Ok(_)) => {}
                Ok(Err(e)) => failures.push(format!("{id} (--wasm {wasm}): error: {e}")),
                Err(_) => failures.push(format!("{id} (--wasm {wasm}): PANIC")),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "matrix-supported constructs failed to generate (regression, or matrix↔generator drift):\n{}",
        failures.join("\n")
    );
}

/// Unsupported-construct scorecard — a SCORECARD, not a "never panic" contract (that's
/// `input_robustness_catalog`'s job, and this is deliberately a *separate* catalog so it can't weaken
/// it). The fixtures (`tests/matrix_panic/*.cddl`, projected by `cddl-matrix/project_robustness.ts`) are
/// constructs the matrix marks `unsupported` because cddl-codegen PANICS while generating them — *valid*
/// CDDL whose probe was `panic (exit 101)`. We snapshot the CURRENT outcome category so any change is a
/// reviewable diff: PANIC → `error (graceful)` means a gap got fixed (re-bless), while a panic decaying
/// to a silently-wrong `ok`, or a brand-new panic, is a regression. PANIC here is a *tracked-and-visible
/// known gap*, NOT blessed-as-correct.
///
/// Generate-only (no `cargo check`), so it captures ONLY panic-class gaps; compile-class ones (`x = any`,
/// bare `x = int`, `bool` in a type-choice) generate fine and are invisible here — those need a negative
/// compile-gate, a different tool.
#[test]
fn unsupported_construct_panic_catalog() {
    let dir = std::path::Path::new("tests/matrix_panic");
    let mut inputs: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    inputs.sort();
    assert!(
        !inputs.is_empty(),
        "no panic fixtures in {dir:?} (run `bun run project_robustness.ts`)"
    );

    let mut catalog = String::from(
        "# generator outcome per matrix `unsupported` (panic-class) construct — a SCORECARD, not a contract.\n\
         # PANIC = tracked-known gap (the matrix's `panic (exit 101)` verdict). Flipping it to `error (graceful)`\n\
         # is a FIX (re-bless); a new panic, or a panic decaying to a silently-wrong `ok`, is a regression.\n\
         # Generate-only: captures panic-class gaps only, not compile-class. Source: cddl-matrix/project_robustness.ts.\n\n",
    );
    // hook restored (and lock released) before the possibly-panicking snapshot assertion below
    with_thread_silenced_panics(|| {
        for path in &inputs {
            let id = path.file_stem().unwrap().to_str().unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "matrix_panic_unused",
                "--wasm",
                "false",
            ]);
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::api::generated_strings(&cli)
            }));
            let label = match outcome {
                Ok(Ok(_)) => "ok",
                Ok(Err(_)) => "error (graceful)",
                Err(_) => "PANIC",
            };
            catalog.push_str(&format!("{id:34} {label}\n"));
        }
    });

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/matrix_panic/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}

/// Expect-reject scorecard — the THIRD generation-outcome catalog, over `tests/matrix_reject/*.cddl`
/// (projected by `cddl-matrix/project_robustness.ts`). These are constructs the matrix marks off-limits
/// that mint NO test elsewhere: non-panic `unsupported` rows (parse-rejected control ops like
/// `ctl.cborseq`/`ctl.oid`/`ctl.sdnv`; generates-but-doesn't-compile shapes like `prelude.any`) plus the
/// `out_of_profile` rows (which can themselves be panic-class, e.g. `type2.tag_head_type`). The catalog
/// is heterogeneous by construction — under this generate-only pass a parse-reject records
/// `error (graceful)`, a generates-but-doesn't-compile row records `ok`, an out-of-profile panic records
/// `PANIC` — so the drift assertion is the snapshot itself, not a uniform outcome.
///
/// PURPOSE: catch a parser/codegen change that SILENTLY makes a rejected construct parse — the exact
/// thing a past cddl-fork bump did to 14 control ops — which flips a committed `error (graceful)` row to
/// `ok` and surfaces here as a snapshot diff in the DEFAULT `cargo test` suite, instead of waiting for a
/// manual verify.ts run. The `project_robustness.ts --check` cross-check independently pins each row's
/// expected label to its matrix evidence class, so a re-bless that hides such a flip fails that gate.
///
/// Generate-only (no `cargo check`), matching the matrix probe's flags (--wasm=false, default profile).
#[test]
fn unsupported_construct_reject_catalog() {
    let dir = std::path::Path::new("tests/matrix_reject");
    let mut inputs: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    inputs.sort();
    assert!(
        !inputs.is_empty(),
        "no reject fixtures in {dir:?} (run `bun run project_robustness.ts`)"
    );

    let mut catalog = String::from(
        "# generator outcome per matrix off-limits (reject-class) construct — a SCORECARD, not a contract.\n\
         # Heterogeneous by design: parse-rejected rows record `error (graceful)`, generates-but-doesn't-\n\
         # compile rows record `ok` (generate-only can't see the later failure), out-of-profile panics record\n\
         # `PANIC`. A row flipping `error (graceful)` -> `ok` means a rejected construct started PARSING (the\n\
         # cddl-fork-bump regression class) — investigate, don't blindly re-bless. project_robustness.ts --check\n\
         # pins each row's expected label to its matrix evidence class. Source: cddl-matrix/project_robustness.ts.\n\n",
    );
    // hook restored (and lock released) before the possibly-panicking snapshot assertion below
    with_thread_silenced_panics(|| {
        for path in &inputs {
            let id = path.file_stem().unwrap().to_str().unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "matrix_reject_unused",
                "--wasm",
                "false",
            ]);
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::api::generated_strings(&cli)
            }));
            let label = match outcome {
                Ok(Ok(_)) => "ok",
                Ok(Err(_)) => "error (graceful)",
                Err(_) => "PANIC",
            };
            catalog.push_str(&format!("{id:34} {label}\n"));
        }
    });

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/matrix_reject/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}

/// A bareword member key (`a:`) and a quoted text member key (`"a":`) are the same CDDL construct
/// (the quoted form is grammar sugar for the identical text-string wire key), so with matching rule
/// and field names the two forms must generate a byte-identical crate. Asserting the ENTIRE file map
/// (not just a field name) pins that convergence end-to-end — naming, serialization, and JSON — for
/// both the 1-field and 2-field forms, guarding the two paths (`group_entry_to_field_name`,
/// `group_entry_to_key`, `group_entry_to_raw_field_name`) from drifting apart.
#[test]
fn bareword_and_quoted_keys_converge() {
    fn generate(spec: &str, tag: &str) -> std::collections::BTreeMap<String, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_converge_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "converge_unused",
        ]);
        let out = crate::api::generated_strings(&cli).unwrap();
        std::fs::remove_file(&path).ok();
        out
    }

    // 1-field: single bareword map struct vs the equivalent quoted-key struct.
    assert_eq!(
        generate("foo = { a: uint }\n", "bw1"),
        generate("foo = { \"a\": uint }\n", "q1"),
        "1-field bareword and quoted map keys must generate identical output"
    );

    // 2-field: the heterogeneous form, exercising multiple keys.
    assert_eq!(
        generate("foo = { a: uint, b: text }\n", "bw2"),
        generate("foo = { \"a\": uint, \"b\": text }\n", "q2"),
        "2-field bareword and quoted map keys must generate identical output"
    );
}

/// A keyless map entry (`{ bytes, uint }`) is rejected BY DESIGN — each map field needs a key — but
/// via a GRACEFUL `Err` (deferred through `IntermediateTypes::record_rejection` → drained by
/// `finalize`), never a `panic!`. This pins that the error is real and its message is actionable:
/// it names the offending rule and tells the user what to do. The catalog fixtures
/// (`map_entry_no_key`, `map_entry_no_key_single`) pin the OUTCOME category; this pins the message.
#[test]
fn keyless_map_entry_rejects_gracefully() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_keyless_map_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, "m = { bytes, uint }\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "keyless_map_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err =
        result.expect_err("keyless map entry must be a graceful Err, not Ok (and not a panic)");
    let msg = err.to_string();
    assert!(
        msg.contains("needs a key"),
        "rejection message should be actionable (mention that each map field needs a key), got: {msg}"
    );
    // The message cites the SOURCE spelling (`m`), not the camel-cased RustIdent (`M`).
    assert!(
        msg.contains("rule `m`"),
        "rejection message should name the offending rule, got: {msg}"
    );
}

#[test]
fn input_robustness_catalog() {
    let dir = std::path::Path::new("tests/robustness");
    let mut inputs: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    inputs.sort();
    assert!(!inputs.is_empty(), "no robustness inputs in {:?}", dir);

    let mut catalog = String::from(
        "# generator outcome per malformed/edge input\n# A NEW panic is a regression: malformed input must error gracefully. A committed PANIC entry\n# is a tracked-known rejection (see the fixture's comments); flipping it to `error (graceful)` is a fix.\n\n",
    );
    // hook restored (and lock released) before the possibly-panicking snapshot assertion below
    with_thread_silenced_panics(|| {
        for path in &inputs {
            let name = path.file_stem().unwrap().to_str().unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "robustness_unused",
            ]);
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::api::generated_strings(&cli)
            }));
            let label = match outcome {
                Ok(Ok(_)) => "ok",
                Ok(Err(_)) => "error (graceful)",
                Err(_) => "PANIC",
            };
            catalog.push_str(&format!("{:26} {}\n", name, label));
        }
    });

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/robustness/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}
