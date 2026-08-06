//! Input panic-robustness catalog.
//!
//! Feeds malformed / edge-case spec inputs (`tests/robustness/*.cddl`) to a fresh child process and
//! snapshots the OUTCOME of each — `ok` / `error (graceful)` / `PANIC`. This is
//! a robustness scorecard, not an output-regression test: it catches a refactor that makes a
//! previously-graceful input newly panic (shows up as a snapshot diff), and when a current panic
//! is fixed its entry flips (re-bless then).
//!
//! A fourth outcome, `ABORTED (signal <n>)`, records a non-unwinding crash (such as a stack
//! overflow). `catch_unwind` cannot observe one, so every input runs out of process: the parent
//! reads the child's exit status and the crash becomes a snapshot-able row rather than the death of
//! the catalog test binary.
//!
//! NB: a NEW `PANIC` is a regression — the generator must reject malformed input with a clean
//! error, never `panic!`/`assert!`. A committed `PANIC` entry is a tracked-known rejection whose
//! fixture comments say why it's pinned (e.g. `map_entry_no_key`); making it graceful is a fix.
//! An `ABORTED` entry says the same thing about a crash that cannot unwind.
//! The catalog deliberately records only the outcome *category*
//! (not panic messages/line numbers) so it stays stable across refactors that don't change behaviour.
//!
//! Besides the spec-input catalogs, this module also hosts direct error-path unit probes of
//! generator helpers whose failure mode must be a clean `Err`, never a panic (e.g.
//! `concat_files_missing_path_yields_error_not_panic`) — same panic-vs-graceful theme, exercised
//! at the helper level where no spec input can reach the failure.

use crate::cli::Cli;
use clap::Parser;

/// The global panic hook is process-wide, so every test that silences it must not run its
/// take/set/restore concurrently — an interleave could leave the silent hook installed for the rest
/// of the run. Serialize them on this lock (poison-tolerant: a panic mid-section only means the
/// *other* caller re-silences, which is harmless). The lock is per-fn-internal, so any caller of
/// `with_thread_silenced_panics` participates — including callers in other test modules.
static PANIC_HOOK_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Silence panic output from THIS test's thread for the duration of `f` (the deliberate
/// `catch_unwind` probes would otherwise spew every expected panic). The hook is process-global
/// and `cargo test` runs tests concurrently, so a blanket no-op hook would also eat the panic
/// message of any UNRELATED test that fails during the window — its failure would report with no
/// diagnostics. Filter by thread id instead, delegating other threads' panics to the
/// previously-installed hook.
pub(crate) fn with_thread_silenced_panics<T>(f: impl FnOnce() -> T) -> T {
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

    // Every matrix-supported construct generates on BOTH the rust (`--wasm=false`) and wasm
    // (`--wasm=true`) legs. `any` (the `AnyCbor` lowering) is now full-surface — it has a wasm wrapper
    // class — so the former `prelude.any` wasm-graceful-reject
    // exemption is gone: both legs expect `Ok(Ok(_))`, and a panic or a graceful rejection is a
    // failure.
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

    // `@name` on a bareword key must be honored exactly as on a quoted key (it was silently dropped
    // on barewords, unlike the Value/Type1 arms). With the same directive the two spellings converge.
    assert_eq!(
        generate("foo = { a: uint, ; @name renamed\n}\n", "bw_name"),
        generate("foo = { \"a\": uint, ; @name renamed\n}\n", "q_name"),
        "@name on a bareword key must converge with @name on the quoted key"
    );
}

/// `i64::MIN` (`-9223372036854775808`) is the CBOR nint boundary where a same-width negation
/// overflows `i64` (pre-3.x cbor_event rejected it on the plain endpoint). Since `FixedValue::Nint` is `i128`,
/// the generator must emit the width-correct `write_negative_integer_sz` form for this literal —
/// under DEFAULT with a hard-coded `cbor_event::Sz::Eight`, and under `--preserve-encodings` with
/// the `fit_sz(... .unsigned_abs() as u64, ...)` runtime-encoding form. Pinning both spellings
/// guards the boundary against a future width regression (a narrower literal type would either fail
/// to parse the value or silently truncate it before this line is reached).
#[test]
fn i64_min_fixed_value_emits_width_correct_nint() {
    fn generate(flags: &[&str]) -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_imin_{}_{}.cddl",
            flags.len(),
            std::process::id()
        ));
        // Member position: a bare top-level fixed value is (correctly) unsupported, so wrap the
        // literal in an array where it serializes as a fixed element.
        std::fs::write(&path, "foo = [-9223372036854775808]\n").unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "imin_unused",
        ];
        args.extend_from_slice(flags);
        let cli = Cli::parse_from(args);
        let out = crate::api::generated_strings(&cli).unwrap();
        std::fs::remove_file(&path).ok();
        out.into_values().collect::<Vec<_>>().join("\n")
    }

    let default_out = generate(&[]);
    assert!(
        default_out.contains(
            "serializer.write_negative_integer_sz(-9223372036854775808i128, cbor_event::Sz::Eight)"
        ),
        "default profile must emit the width-correct i64::MIN nint call; got:\n{default_out}"
    );

    let preserve_out = generate(&["--preserve-encodings=true"]);
    assert!(
        preserve_out.contains("serializer.write_negative_integer_sz(")
            && preserve_out.contains("-9223372036854775808,")
            && preserve_out.contains("(-9223372036854775808i128 + 1).unsigned_abs() as u64"),
        "preserve profile must emit the runtime-encoding i64::MIN nint call; got:\n{preserve_out}"
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

/// An inline MAP carrying group choices (`{ x: uint // y: tstr }`) used as a member/element type is
/// rejected BY DESIGN — via a GRACEFUL `Err` (deferred through `record_rejection` → drained by
/// `finalize`), never a `panic!`. The NAMED form (`t = { x: uint // y: tstr }` referenced by name)
/// IS supported, so the message points at it. The robustness fixture
/// `inline_group_choice_member.cddl` pins the OUTCOME category; this pins the message.
#[test]
fn inline_map_group_choice_member_rejects_gracefully() {
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_map_gc_{}.cddl", std::process::id()));
    std::fs::write(&path, "a = [{ x: uint // y: tstr }]\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "map_gc_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err = result.expect_err(
        "inline map with group choices as a member type must be a graceful Err, not a panic",
    );
    let msg = err.to_string();
    // Names the construct...
    assert!(
        msg.contains("inline map") && msg.contains("group choices"),
        "rejection message should name the inline-map-group-choices construct, got: {msg}"
    );
    // ...and gives the real remedy (name it as its own rule, then reference it).
    assert!(
        msg.contains("name it as its own rule") && msg.contains("reference"),
        "rejection message should point at the supported named-rule remedy, got: {msg}"
    );
}

/// An inline ARRAY carrying group choices (`[ 0 // 1 ]`) used as a member/element type is rejected
/// BY DESIGN — via a GRACEFUL `Err`, never a `panic!`. The NAMED form (`t = [ 0 // 1 ]` referenced
/// by name) IS supported, so the message points at it. The robustness fixture
/// `inline_array_group_choice_member.cddl` pins the OUTCOME category; this pins the message.
#[test]
fn inline_array_group_choice_member_rejects_gracefully() {
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_arr_gc_{}.cddl", std::process::id()));
    std::fs::write(&path, "bar = { x: [ 0 // 1 ] }\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "arr_gc_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err = result.expect_err(
        "inline array with group choices as a member type must be a graceful Err, not a panic",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("inline array") && msg.contains("group choices"),
        "rejection message should name the inline-array-group-choices construct, got: {msg}"
    );
    assert!(
        msg.contains("name it as its own rule") && msg.contains("reference"),
        "rejection message should point at the supported named-rule remedy, got: {msg}"
    );
}

/// Generate `spec` and return the graceful rejection message, asserting the run neither succeeded
/// nor panicked. `tag` names the temp file (tests share a pid, so it must be unique per vector) and
/// `extra` carries the profile flags.
fn expect_graceful_rejection(tag: &str, spec: &str, extra: &[&str]) -> String {
    let path = std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
    std::fs::write(&path, spec).unwrap();
    let mut argv = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "reject_unused",
    ];
    argv.extend_from_slice(extra);
    let cli = Cli::parse_from(argv);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();
    result
        .expect_err(&format!(
            "spec must be a graceful Err, not a panic and not a success:\n{spec}"
        ))
        .to_string()
}

/// An anonymous nested MAP in a position that requires a TYPE — an array element, a map value, a
/// `.cbor` payload, a `/` choice alternative, a generic argument, an occurrence target, a
/// group-choice arm — is rejected BY DESIGN, via a GRACEFUL `Err` (deferred through
/// `record_rejection` → drained by `finalize`), never a `panic!`. The ARRAY sibling of this shape
/// can be named through a `@name` comment at the type2; the map side has no such door, so the
/// NAMED form (`m = {x: int, y: uint}` referenced by name) is the supported spelling and the
/// message points at it — verified to generate under both profiles for every keyed vector below.
///
/// Both profiles are asserted because the rejection must be profile-INDEPENDENT: `finalize`
/// short-circuits on a recorded rejection before any emission, so no flag can rescue the shape (and
/// a per-profile split would be a support claim this seam cannot make). The keyless vector
/// (`{ g }`) pins that the same arm covers a map whose sole member is a plain-group reference, a
/// spelling no matrix cell expresses; `tests/robustness/inline_map_keyless_member.cddl` pins its
/// outcome category.
#[test]
fn inline_map_member_rejects_gracefully() {
    let vectors = [
        ("anon_map_elem", "a = [{x: int, y: uint}]\n"),
        ("anon_map_val", "m = { outer: { a: int, c: uint } }\n"),
        ("anon_map_cbor", "b = bytes .cbor ({a: int, c: uint})\n"),
        (
            "anon_map_choice",
            "t = {a: int, c: uint} / {b: tstr, d: uint}\n",
        ),
        (
            "anon_map_generic",
            "foo<a> = [a]\nbar = foo<{x: int, y: uint}>\n",
        ),
        ("anon_map_occur", "a = [* {x: int, y: uint}]\n"),
        ("anon_map_garm", "t = [ {a: int, b: uint} // tstr ]\n"),
        ("anon_map_keyless", "g = (x: uint)\na = [{ g }]\n"),
    ];
    for (tag, spec) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            // Names the construct...
            assert!(
                msg.contains("inline map"),
                "rejection should name the inline-map construct ({tag}, {extra:?}), got: {msg}"
            );
            // ...and points at the named-rule spelling that IS supported.
            assert!(
                msg.contains("name it as its own rule") && msg.contains("reference"),
                "rejection should point at the named-rule spelling ({tag}, {extra:?}), got: {msg}"
            );
        }
    }
}

/// An inline GROUP as the sole entry of a group-choice arm (`t = [ (uint, tstr) // bytes ]`, and
/// the map-rep spelling `t = { (a: uint) // b: tstr }`) is rejected BY DESIGN, via a GRACEFUL
/// `Err`, never a `panic!`. Naming the group (`pair = (uint, tstr)`, then `t = [ pair // bytes ]`)
/// IS the supported spelling under both profiles, so the message points at it.
/// `tests/robustness/inline_group_choice_arm_map.cddl` pins the map rep's outcome category.
///
/// The `?`-marked vector matters on its own: the arm reads the entry's TYPE without consulting its
/// occurrence marker, so a shape whose marker is silently dropped must still not generate.
#[test]
fn inline_group_choice_arm_rejects_gracefully() {
    let vectors = [
        ("inline_garm_arr", "t = [ (uint, tstr) // bytes ]\n"),
        ("inline_garm_map", "t = { (a: uint) // b: tstr }\n"),
        ("inline_garm_opt", "t = [ ? (uint, tstr) // bytes ]\n"),
        ("inline_garm_both", "t = [ (uint, tstr) // (bytes, int) ]\n"),
    ];
    for (tag, spec) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains("inline group"),
                "rejection should name the inline-group construct ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains("Name the group instead"),
                "rejection should point at the named-group spelling ({tag}, {extra:?}), got: {msg}"
            );
        }
    }
}

/// Every `type2` construct with no member/element representation is rejected BY DESIGN, via a
/// GRACEFUL `Err`, never a `panic!` — the role-sibling of the rule-body catch-all in `parse_type`.
/// Each vector below reaches `rust_type_from_type2`'s catch-all and must come back naming its OWN
/// construct, so a future arm that silently swallows a neighbour's shape fails here rather than
/// generating something wrong.
///
/// Byte-string literals get three vectors (keyed array element, unkeyed array element, map value)
/// because the three are separate walk paths into the same arm. The `b64'…'` spelling has no vector:
/// the upstream `cddl` fork's parser rejects it before generation (a `missing definition for rule
/// b64` parse error), so it cannot reach this seam today — the arm lists it only so the class stays
/// complete if that gap closes.
///
/// The `#` vector's hint is asserted honest by `tests/robustness/any_member.cddl`: the prelude NAME
/// `any` is supported in exactly the position the grammar sigil is refused in.
#[test]
fn unsupported_member_type2_rejects_gracefully() {
    // (tag, spec, a substring naming the construct, a substring of the honest remedy)
    let vectors = [
        (
            "t2_bytes_elem",
            "a = [v: h'0102', x: uint]\n",
            "a byte-string literal",
            "it is a different spec, not an equivalent one",
        ),
        (
            "t2_bytes_elem_unkeyed",
            "a = [h'0102', x: uint]\n",
            "a byte-string literal",
            "it is a different spec, not an equivalent one",
        ),
        (
            "t2_bytes_map_val",
            "m = { k: h'0102', j: uint }\n",
            "a byte-string literal",
            "it is a different spec, not an equivalent one",
        ),
        (
            "t2_bytes_utf8",
            "a = [v: 'text', x: uint]\n",
            "a byte-string literal",
            "it is a different spec, not an equivalent one",
        ),
        (
            "t2_unwrap",
            "bar = [uint]\nfoo = [v: ~bar, x: uint]\n",
            "an unwrap (`~name`)",
            "inline the referenced rule's definition manually",
        ),
        (
            "t2_any_sigil",
            "a = [v: #, x: uint]\n",
            "the `any` type (`#`)",
            "the prelude name `any` is supported in this position",
        ),
        (
            "t2_major_type",
            "a = [v: #1, x: uint]\n",
            "a bare major-type constraint (`#N` / `#N.M`)",
            "",
        ),
        (
            "t2_choice_from_group",
            "g = (a: uint, b: uint)\na = [v: &g, x: uint]\n",
            "a choice-from-group (`&groupname`)",
            "",
        ),
        (
            "t2_choice_from_inline_group",
            "a = [v: &(a: 1, b: 2), x: uint]\n",
            "a choice-from-inline-group (`&( ... )`)",
            "",
        ),
    ];
    for (tag, spec, construct, remedy) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains(construct),
                "rejection should name `{construct}` ({tag}, {extra:?}), got: {msg}"
            );
            // The seam is member/element-only — `parse_type` owns rule bodies — so the wording may
            // claim the role, and must.
            assert!(
                msg.contains("used as a member or element type is unsupported"),
                "rejection should name the member/element role ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains(remedy),
                "rejection should carry the honest remedy ({tag}, {extra:?}), got: {msg}"
            );
        }
    }
}
/// The CDDL prelude constant `undefined` (major type 7, simple value 23) is rejected BY DESIGN, via
/// a GRACEFUL `Err`, never a `panic!` — in EVERY position, member and rule-body alike. Unlike
/// `null`/`true`/`false` it has no `FixedValue`, so there is no value for a member to hold and no
/// type for a rule to name.
///
/// The refusal lives at `IntermediateTypes::new_type`'s unresolved-reserved fallback, the one seam
/// every position funnels through, which is also why the message is ROLE-NEUTRAL: that seam knows
/// the NAME, never the position it was written in. So this asserts one message text across all
/// three vectors rather than a per-role wording.
///
/// The remedy it advertises is asserted honest by `all_supported_constructs_generate` (the matrix's
/// `prelude.any` row) and by `tests/robustness/any_member.cddl` — `any` really does carry an
/// arbitrary CBOR item, `undefined` included, in member position.
///
/// The top-level vector additionally emits a SECOND, cascade line: the inert `Fixed(Null)`
/// placeholder the refusal returns reaches the bare-fixed-rule guard in `register_type_alias`. That
/// is accepted deliberately — the `undefined` diagnosis leads, and suppressing the cascade would
/// need either a non-inert placeholder (which cascades WORSE in a type-choice arm) or a
/// cross-seam flag. This asserts the ORDER, so a future change that buries the real cause fails.
#[test]
fn undefined_prelude_rejects_gracefully_in_every_position() {
    let vectors = [
        ("undef_elem", "a = [v: undefined, x: uint]\n"),
        ("undef_map_val", "m = { k: undefined, j: uint }\n"),
        ("undef_rule_body", "x = undefined\n"),
    ];
    for (tag, spec) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains(
                    "the CDDL prelude type `undefined` (major type 7, simple value 23) is \
                              unsupported"
                ),
                "rejection should name the `undefined` prelude type ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains("the supported `any` type"),
                "rejection should point at the `any` remedy ({tag}, {extra:?}), got: {msg}"
            );
            // The role-neutral seam can NOT name the position, so it must not pretend to.
            assert!(
                !msg.contains("as a member") && !msg.contains("as a rule body"),
                "role-neutral message must not claim a position it cannot know ({tag}, {extra:?}), \
                 got: {msg}"
            );
        }
    }

    // Cascade order for the rule-body vector: the real cause first, the placeholder's follow-on
    // second.
    let body_msg = expect_graceful_rejection("undef_rule_body_order", "x = undefined\n", &[]);
    let cause = body_msg
        .find("prelude type `undefined`")
        .expect("undefined rejection must be present");
    let cascade = body_msg
        .find("bare fixed value")
        .expect("the placeholder's bare-fixed cascade is expected on the rule-body vector");
    assert!(
        cause < cascade,
        "the `undefined` diagnosis must lead the placeholder's cascade, got: {body_msg}"
    );
}

/// Every float prelude name generates, in every position, under every profile — and the six names
/// are six DISTINCT value classes rather than two carrier widths.
///
/// The narrower names (`float16`, `float16-32`, `float32-64`) were once refused rather than
/// registered, because generated code judged every float the same way regardless of its type. They
/// register now that both directions carry the class. What this pins is the part a rename or a
/// copy-paste would silently break: which CARRIER each name gets, and that no two names collapsed
/// back onto one identity — `float` and `float64` in particular, whose carrier is the same `f64`
/// and whose value sets are not.
///
/// The carrier is read off the generated ctor signature rather than asserted through the IR so the
/// test sees what a CONSUMER sees. The value classes themselves are pinned by byte vectors
/// (`tests/core/tests.rs`, `tests/golden_hex_preserve/tests.rs`).
#[test]
fn every_float_prelude_name_generates_with_its_own_carrier() {
    // (CDDL name, rust carrier)
    let names = [
        ("float16", "f32"),
        ("float32", "f32"),
        ("float16-32", "f32"),
        ("float64", "f64"),
        ("float32-64", "f64"),
        ("float", "f64"),
    ];
    for (name, carrier) in names {
        let vectors = [
            ("elem", format!("a = [v: {name}, x: uint]\n")),
            ("map_val", format!("m = {{ k: {name}, j: uint }}\n")),
            ("rule_body", format!("x = {name}\n")),
        ];
        for (pos, spec) in vectors {
            for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
                let path = std::env::temp_dir().join(format!(
                    "cddl_codegen_float_{}_{pos}_{}.cddl",
                    name.replace('-', "_"),
                    std::process::id()
                ));
                std::fs::write(&path, &spec).unwrap();
                let mut argv = vec![
                    "cddl-codegen",
                    "--input",
                    path.to_str().unwrap(),
                    "--output",
                    "float_names_unused",
                ];
                argv.extend_from_slice(extra);
                let cli = Cli::parse_from(argv);
                let result = crate::api::generated_strings(&cli);
                std::fs::remove_file(&path).ok();
                let files = result.unwrap_or_else(|e| {
                    panic!("`{name}` must generate in every position ({pos}, {extra:?}): {e}")
                });
                if pos == "elem" {
                    let mod_rs = files
                        .iter()
                        .find(|(path, _)| path.ends_with("mod.rs"))
                        .map(|(_, content)| content.clone())
                        .unwrap_or_default();
                    assert!(
                        mod_rs.contains(&format!("pub fn new(v: {carrier},")),
                        "`{name}` must carry a `{carrier}` ({extra:?}), got: {mod_rs}"
                    );
                }
            }
        }
    }

    // `float` and `float64` share the `f64` carrier and must NOT share an identity: a union of the
    // two is two variants, which only holds while they are distinct IR types.
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_float_vs_float64_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, "x = float / float64\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "float_vs_float64_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();
    let files = result.expect("`float / float64` must generate");
    let mod_rs = files
        .iter()
        .find(|(path, _)| path.ends_with("mod.rs"))
        .map(|(_, content)| content.clone())
        .unwrap_or_default();
    assert!(
        mod_rs.contains("Float(f64)") && mod_rs.contains("F64(f64)"),
        "`float` and `float64` must stay separate variants, got: {mod_rs}"
    );
}

/// The CONTROL-OPERATOR path is the one route that reaches a prelude type name without going
/// through `IntermediateTypes::new_type` — a rule-position `x = <name> .size 4` resolves the ident
/// through `ident_to_primitive` directly. Every float prelude name must therefore be mapped THERE
/// too, or a constrained rule resolves to a different type than the same name resolves to
/// everywhere else. Two names had no primitive at all on this path and aborted at a bare
/// `ident_to_primitive` unwrap (`float16-32 .size 4`), and `float16` resolved to the SAME primitive
/// as `float32` — a constrained rule that silently accepted the wrong value class.
///
/// Two halves, asserted together because the second is what stops the first from recurring under a
/// different name: every float name carries its own identity through a constraint (read off the
/// generated carrier, as in the sibling registration sweep), and ANY reserved-but-unmapped head
/// refuses gracefully instead of aborting.
///
/// The control vectors are load-bearing: `uint`/`tstr` with the same operator must keep generating,
/// or the guard could pass by refusing every constrained rule.
#[test]
fn control_operator_path_maps_every_float_name_and_refuses_unmapped_heads() {
    // (name, rust carrier) — the same six the registration sweep covers.
    let names = [
        ("float16", "f32"),
        ("float32", "f32"),
        ("float16-32", "f32"),
        ("float64", "f64"),
        ("float32-64", "f64"),
        ("float", "f64"),
    ];
    // One vector per control-operator flavor a typename head can carry at a rule position: the
    // `.size` window, a value comparison (the float-window route), and `.default`.
    let ops = [".size 4", ".le 3.0", ".default 1.0"];
    for (name, carrier) in names {
        for op in ops {
            let spec = format!("x = {name} {op}\n");
            for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
                let path = std::env::temp_dir().join(format!(
                    "cddl_codegen_ctlfloat_{}_{}_{}.cddl",
                    name.replace('-', "_"),
                    op.len(),
                    std::process::id()
                ));
                std::fs::write(&path, &spec).unwrap();
                let mut argv = vec![
                    "cddl-codegen",
                    "--input",
                    path.to_str().unwrap(),
                    "--output",
                    "ctlfloat_unused",
                ];
                argv.extend_from_slice(extra);
                let cli = Cli::parse_from(argv);
                let result = crate::api::generated_strings(&cli);
                std::fs::remove_file(&path).ok();
                let files = result
                    .unwrap_or_else(|e| panic!("`{name} {op}` must generate ({extra:?}): {e}"));
                let all = files
                    .values()
                    .map(|content| content.as_str())
                    .collect::<Vec<_>>()
                    .join("\n");
                // the shape differs per op (a windowed rule wraps, a `.default` stays a
                // transparent alias), so match the carrier in either spelling
                assert!(
                    all.contains(&format!("inner: {carrier}"))
                        || all.contains(&format!("pub type X = {carrier};")),
                    "`{name} {op}` must keep `{name}`'s own `{carrier}` carrier ({extra:?})"
                );
            }
        }
    }

    // The general half: a reserved prelude name with no rust primitive behind it is a graceful
    // rejection naming the ident, not an abort. `tdate` (#6.0(tstr)) is such a name today; the
    // assertion is about the CLASS, so the next unmapped name cannot re-earn the panic.
    let msg = expect_graceful_rejection("ctl_unmapped_head", "x = tdate .size 4\n", &[]);
    assert!(
        msg.contains("a range or `.size` control operator on `tdate` is unsupported"),
        "an unmapped control head must reject gracefully naming the ident, got: {msg}"
    );
    assert!(
        msg.contains("has no such primitive"),
        "the rejection must say WHY the head cannot carry the constraint, got: {msg}"
    );

    // Control: the constrained shapes that DO work must keep working, under both profiles.
    for spec in ["x = uint .size 4\n", "x = tstr .size 4\n"] {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_ctl_ok_{}_{}.cddl",
                spec.len(),
                std::process::id()
            ));
            std::fs::write(&path, spec).unwrap();
            let mut argv = vec![
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "ctl_ok_unused",
            ];
            argv.extend_from_slice(extra);
            let cli = Cli::parse_from(argv);
            let result = crate::api::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            assert!(
                result.is_ok(),
                "the guard must not swallow supported constrained rules ({spec:?}, {extra:?})"
            );
        }
    }
}

/// The four `any`-content prelude tags — `cbor-any` (#6.55799), `eb64url` (#6.21), `eb64legacy`
/// (#6.22), `eb16` (#6.23) — are refused GRACEFULLY in every position, never aborted. Each tags an
/// arbitrary CBOR item with advice ABOUT that item, so the payload is `any` and the tag constrains
/// nothing a generated type could hold; there is no representation to emit, which is the same
/// no-representation shape `undefined_prelude_rejects_gracefully_in_every_position` pins one seam
/// over. They share that seam (`IntermediateTypes::new_type`'s unresolved-reserved fallback), so
/// the message is likewise ROLE-NEUTRAL — it names the type and its tag, never the position — and
/// this asserts one wording across all three vectors rather than a per-role one.
///
/// The tag NUMBER is asserted per name because it is the part a reader checks the message against;
/// a copy-paste that gave `eb16` the `eb64url` tag would otherwise read fine. The remedy the
/// message advertises is asserted honest by `all_supported_constructs_generate` (the matrix's
/// `prelude.any` row) and by `tests/robustness/any_member.cddl` — `any` really does carry an
/// arbitrary CBOR item in member position.
///
/// The two dispositions are pinned apart because they are decisions, not phrasing: `cbor-any` is a
/// permanent exclusion (`tests/TESTING_ROADMAP.md` § North star's exclude list), while the three
/// `eb*` names are merely unbuilt. A future delivery of `eb*` support flips exactly one of these
/// assertions.
#[test]
fn any_content_prelude_tags_reject_gracefully_in_every_position() {
    let names = [
        ("cbor-any", "#6.55799(any)"),
        ("eb64url", "#6.21(any)"),
        ("eb64legacy", "#6.22(any)"),
        ("eb16", "#6.23(any)"),
    ];
    for (name, tag) in names {
        let vectors = [
            ("elem", format!("a = [v: {name}, x: uint]\n")),
            ("map_val", format!("m = {{ k: {name}, j: uint }}\n")),
            ("rule_body", format!("x = {name}\n")),
        ];
        for (pos, spec) in vectors {
            for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
                let msg = expect_graceful_rejection(&format!("ebtag_{name}_{pos}"), &spec, extra);
                assert!(
                    msg.contains(&format!(
                        "the CDDL prelude type `{name}` ({tag}) is unsupported"
                    )),
                    "rejection should name the type AND its tag ({name}/{pos}, {extra:?}), got: \
                     {msg}"
                );
                assert!(
                    msg.contains("the supported `any` type"),
                    "rejection should point at the `any` remedy ({name}/{pos}, {extra:?}), got: \
                     {msg}"
                );
                // The role-neutral seam can NOT name the position, so it must not pretend to.
                assert!(
                    !msg.contains("as a member") && !msg.contains("as a rule body"),
                    "role-neutral message must not claim a position it cannot know ({name}/{pos}, \
                     {extra:?}), got: {msg}"
                );
                if name == "cbor-any" {
                    assert!(
                        msg.contains("permanently excluded"),
                        "`cbor-any`'s refusal must carry the permanent-exclusion ruling \
                         ({pos}, {extra:?}), got: {msg}"
                    );
                } else {
                    assert!(
                        msg.contains("is not built"),
                        "an `eb*` refusal must read as unbuilt, not as a ruling ({name}/{pos}, \
                         {extra:?}), got: {msg}"
                    );
                }
            }
        }
    }
}

/// A heterogeneous anonymous inline ARRAY in a position that requires a TYPE (`a = [[int]]`, a
/// `.cbor` payload, a `/` choice alternative, a map key, a map value, an occurrence target) is
/// rejected BY DESIGN — a GRACEFUL `Err`, never a `panic!`. This is the BRACKET sibling of
/// `inline_map_member_rejects_gracefully`: the array side has a naming door the map side lacks, so
/// the message carries BOTH remedies (an explicit rule, or a `; @name` on the type2).
///
/// The final vector is a CONTROL, not a rejection: at the choice-member position the `@name`
/// comment does reach the naming site, so the struct is minted and generation succeeds. Without it
/// this test could pass while the rejection had swallowed the naming door whole — the pin would
/// hold for the wrong reason. (Which positions the door does and does not reach is enumerated by
/// the comment-DSL position sweep's `@name` cells, including the member-position one these vectors
/// deliberately do not spell.)
#[test]
fn anonymous_nested_array_rejects_gracefully() {
    let vectors = [
        ("anon_arr_elem", "a = [[int]]\n"),
        ("anon_arr_cbor", "b = bytes .cbor ([uint, uint])\n"),
        ("anon_arr_choice", "t = [int] / [tstr]\n"),
        ("anon_arr_mapkey", "m = { [int] => tstr }\n"),
        ("anon_arr_mapval", "m = { k: [int], j: uint }\n"),
        ("anon_arr_occur", "a = [* [int]]\n"),
    ];
    for (tag, spec) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains("Anonymous groups not allowed"),
                "rejection should name the anonymous-group construct ({tag}, {extra:?}), got: {msg}"
            );
            // Both halves of the advertised remedy, each verified to generate.
            assert!(
                msg.contains("create an explicit rule") && msg.contains("@name"),
                "rejection should carry both remedies ({tag}, {extra:?}), got: {msg}"
            );
        }
    }

    // Control: `@name` at the choice-member position DOES reach the naming site, so the struct is
    // minted and this generates. The rejection above must not have closed that door.
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_anon_arr_named_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, "x = [1, bytes] ; @name arr_variant\n  / uint\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "anon_arr_named_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();
    let generated =
        result.expect("a `; @name`d anonymous array at choice-member position generates");
    assert!(
        generated
            .values()
            .any(|src| src.contains("struct ArrVariant")),
        "the `@name`d anonymous array must still mint its struct"
    );
}

/// `@raw_bytes_flavor` anywhere other than a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition is
/// rejected BY DESIGN — via a GRACEFUL `Err` (deferred through `record_rejection` → drained by
/// `finalize`), never a `panic!` and never a silent no-op. One vector per rejecting seam: a
/// single-choice non-extern type rule, a multi-choice type rule, and a field/member position.
/// This pins that each seam fires and that the message names the tag and the extern-only rule.
/// The `T / null` row is the Option-collapse branch, which shares the multi-choice branch's
/// message through one helper rather than a copy.
#[test]
fn raw_bytes_flavor_misuse_rejects_gracefully() {
    // (seam, cddl, seam-specific message fragment) — the fragment proves the vector reached ITS
    // seam, not just any rejection (the field seam has its own "not a field" wording).
    let vectors = [
        (
            "single-choice non-extern type rule",
            "foo = uint ; @raw_bytes_flavor\n",
            "Remove it from this rule",
        ),
        (
            "multi-choice type rule",
            "foo = uint / text ; @raw_bytes_flavor\n",
            "Remove it from this rule",
        ),
        (
            "T / null Option-collapse rule",
            "foo = uint / null ; @raw_bytes_flavor\n",
            "Remove it from this rule",
        ),
        (
            "field position",
            "s = [\n  x: uint, ; @raw_bytes_flavor\n]\n",
            "not a field",
        ),
    ];
    for (seam, cddl, seam_fragment) in vectors {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rbf_misuse_{}_{}.cddl",
            std::process::id(),
            seam.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rbf_misuse_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();

        let err = result.expect_err(&format!(
            "@raw_bytes_flavor on a {seam} must be a graceful Err, not Ok (and not a panic)"
        ));
        let msg = err.to_string();
        assert!(
            msg.contains("@raw_bytes_flavor") && msg.contains("only valid on"),
            "rejection for {seam} should name the tag and the extern-only rule, got: {msg}"
        );
        assert!(
            msg.contains(seam_fragment),
            "rejection for {seam} should carry its seam-specific wording ({seam_fragment:?}), got: {msg}"
        );
    }
}

/// `@copy` (Copy-ness channel for extern / raw-bytes types) is valid ONLY on a
/// `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule; every other placement is
/// rejected BY DESIGN — a GRACEFUL `Err` (deferred through `record_rejection` → drained by
/// `finalize`), never a `panic!` and never a silent no-op. One vector per rejecting seam
/// (single-choice non-marker type rule, multi-choice type rule, the `T / null` Option collapse,
/// field/member position), mirroring `raw_bytes_flavor_misuse_rejects_gracefully`.
#[test]
fn copy_misuse_rejects_gracefully() {
    let vectors = [
        (
            "single-choice non-marker type rule",
            "foo = uint ; @copy\n",
            "Remove it from this rule",
        ),
        (
            "multi-choice type rule",
            "foo = uint / text ; @copy\n",
            "Remove it from this rule",
        ),
        (
            "T / null Option-collapse rule",
            "foo = uint / null ; @copy\n",
            "Remove it from this rule",
        ),
        (
            "field position",
            "s = [\n  x: uint, ; @copy\n]\n",
            "not a field",
        ),
    ];
    for (seam, cddl, seam_fragment) in vectors {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_copy_misuse_{}_{}.cddl",
            std::process::id(),
            seam.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "copy_misuse_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();

        let err = result.expect_err(&format!(
            "@copy on a {seam} must be a graceful Err, not Ok (and not a panic)"
        ));
        let msg = err.to_string();
        assert!(
            msg.contains("@copy") && msg.contains("only valid on"),
            "rejection for {seam} should name the tag and the extern/raw-bytes-only rule, got: {msg}"
        );
        assert!(
            msg.contains(seam_fragment),
            "rejection for {seam} should carry its seam-specific wording ({seam_fragment:?}), got: {msg}"
        );
    }
}

/// `@no_json_schema_export` suppresses a rule's schema-registration row. On a rule that registers no
/// rust struct AT ALL there is no row to suppress, so the directive would be silently dead — the
/// house style rejects that loudly (`@raw_bytes_flavor`, `@copy`, `@duplicates`, `@ignore` all do).
///
/// Two halves, both asserted here so the bar can't drift in either direction:
/// - REJECTED: the struct-less alias shapes (a plain transparent alias, a `@no_alias` alias).
/// - ACCEPTED: rules that DO register a struct, including the ones the row loop skips for other
///   reasons (an `Array` typedef, a generic-extern base) — those are redundant-but-honest
///   annotations, and the rule "valid wherever a rust type is produced" must stay simple and
///   flag-independent. Accepted WITHOUT the json flags too: a spec is generated under several flag
///   sets and must not have to change between them.
#[test]
fn no_json_schema_export_misuse_rejects_gracefully() {
    let rejected = [
        (
            "plain transparent alias",
            "foo = uint ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        (
            "@no_alias alias",
            "foo = uint ; @no_alias @no_json_schema_export\nroot = [a: foo]\n",
        ),
        // A generic DEFINITION is not itself a type — only its instantiations are, and they do not
        // inherit the directive, so annotating the base is dead. Annotate the instance instead.
        (
            "generic definition",
            "foo<T> = [a: T] ; @no_json_schema_export\ninst = foo<uint>\nroot = [a: inst]\n",
        ),
        // A named binding to a set nominal is a transparent `pub type Foo = SetU64;` alias.
        (
            "named binding to a set nominal",
            "gset<T> = #6.258([* T]) / [* T]\nfoo = gset<uint> ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        // A plain group nobody splices materializes no struct, so there is no row to suppress.
        (
            "unspliced plain group",
            "foo = (a: uint, b: uint) ; @no_json_schema_export\nroot = [z: uint]\n",
        ),
    ];
    let accepted = [
        (
            "extern rule",
            "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        (
            "record rule",
            "foo = [x: uint] ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        (
            "enum rule",
            "foo = uint / text ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        (
            "@newtype wrapper",
            "foo = uint ; @newtype @no_json_schema_export\nroot = [a: foo]\n",
        ),
        (
            "with @custom_json (orthogonal, legally combinable)",
            "foo = [x: uint] ; @custom_json @no_json_schema_export\nroot = [a: foo]\n",
        ),
        // Registers an `Array` typedef struct the row loop already skips — redundant but honest.
        (
            "array typedef",
            "foo = [* uint] ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
        // Registers a generic-extern BASE struct the row loop already skips — likewise.
        (
            "generic-extern base",
            "foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @no_json_schema_export\ninst = foo<uint>\nroot = [a: inst]\n",
        ),
        // A SPLICED plain group registers a struct (and gets a row), so the directive is live —
        // `parse_rule`'s `Rule::Group` arm reaches neither `parse_type` nor `parse_type_choices`, so
        // this shape shipped silently dropping the directive until it was pinned.
        (
            "spliced plain group",
            "foo = (a: uint, b: uint) ; @no_json_schema_export\nroot = [foo]\n",
        ),
        // A generic INSTANCE registers its struct only during finalize's generic resolution — the
        // reason the struct-less check runs at the END of finalize rather than in the parse walk.
        (
            "generic instance",
            "base<T> = [a: T]\nfoo = base<uint> ; @no_json_schema_export\nroot = [a: foo]\n",
        ),
    ];
    let run = |seam: &str, cddl: &str, json: bool| {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_no_json_schema_export_{}_{}_{json}.cddl",
            std::process::id(),
            seam.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let mut args = vec![
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            path.to_str().unwrap().to_owned(),
            "--output".to_owned(),
            "no_json_schema_export_misuse_unused".to_owned(),
        ];
        if json {
            args.push("--json-serde-derives=true".to_owned());
            args.push("--json-schema-export=true".to_owned());
        }
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result
    };
    for (seam, cddl) in rejected {
        for json in [false, true] {
            let err = run(seam, cddl, json).err().unwrap_or_else(|| {
                panic!(
                    "@no_json_schema_export on a {seam} must be a graceful Err, not Ok (json={json})"
                )
            });
            let msg = err.to_string();
            assert!(
                msg.contains("@no_json_schema_export")
                    && msg.contains("registers no rust struct")
                    && msg.contains("silently do nothing"),
                "rejection for {seam} should name the tag and the struct-less cause, got: {msg}"
            );
        }
    }
    for (seam, cddl) in accepted {
        for json in [false, true] {
            assert!(
                run(seam, cddl, json).is_ok(),
                "@no_json_schema_export on a {seam} must be accepted (json={json})"
            );
        }
    }
}

/// `@duplicates` rejection classes that remain after phase 2 made table `preserve` fully live on
/// BOTH boundaries (`{* …}` -> `PairMap`, `{+ …}` -> `NonEmptyPairMap`, wasm included). What survives
/// is only the PERMANENT placement rejection: `@duplicates` on a non-collection rule (aliases,
/// structs, unions, fields) is an "only applies to …" error regardless of policy. The now-LIVE cases
/// (array/set `reject`, table `preserve` on both flavors, array `preserve` / table `reject` no-ops)
/// are covered by `duplicates_directive_accepts_live_and_default_noops`,
/// `duplicates_preserve_nonempty_table_lowers_to_twin_under_wasm`, and the corpus fixtures.
#[test]
fn duplicates_directive_rejects_gracefully() {
    // (seam, cddl, must-contain fragments) — the fragments prove the vector reached ITS seam and
    // carries the class-correct wording. The default CLI leaves `--wasm` ON.
    let permanent = "only applies to";
    let vectors = [
        // --- non-collection rules: permanent placement rejection ---
        ("text alias", "foo = text ; @duplicates reject\n", permanent),
        (
            "struct rule",
            "foo = { a: uint, b: text } ; @duplicates reject\n",
            permanent,
        ),
        (
            "union rule",
            "foo = uint / text ; @duplicates reject\n",
            permanent,
        ),
    ];
    for (seam, cddl, fragment) in vectors {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_dup_misuse_{}_{}.cddl",
            std::process::id(),
            seam.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "dup_misuse_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();

        let err = result.expect_err(&format!(
            "@duplicates on a {seam} must be a graceful Err, not Ok (and not a panic)"
        ));
        let msg = err.to_string();
        assert!(
            msg.contains("@duplicates"),
            "rejection for {seam} should name the directive, got: {msg}"
        );
        assert!(
            msg.contains(fragment),
            "rejection for {seam} should carry its class-specific wording ({fragment:?}), got: {msg}"
        );
    }
}

/// A float set element must reject GRACEFULLY at generation: the uniqueness twins' `TryFrom` door
/// is bounded `T: Ord` (the hybrid `scan_unique`), and a set nominal's always-on comparison derives
/// demand `Ord`/`Hash` on the element regardless of policy — floats satisfy neither, so silently
/// generating would emit a non-compiling crate (E0277 far from the rule). The set-side twin of the
/// float-table-key rejection, covering both seams: a plain reject array (`Array` + reject policy)
/// and a tag-258 set nominal (`Wrapper` + `set_nominal`), directly and via a float-containing
/// element rule. A plain array WITHOUT the uniqueness requirement must keep generating (floats are
/// only unordered, not unserializable).
#[test]
fn float_set_element_rejects_gracefully() {
    let run =
        |tag: &str, cddl: &str| -> Result<std::collections::BTreeMap<String, String>, String> {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_float_set_{}_{}.cddl",
                tag,
                std::process::id()
            ));
            std::fs::write(&path, cddl).unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "float_set_unused",
            ]);
            let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
            std::fs::remove_file(&path).ok();
            result
        };

    let reject_vectors = [
        ("reject_array", "foo = [* float64] ; @duplicates reject\n"),
        (
            "reject_ne_array",
            "foo = [+ float64] ; @duplicates reject\n",
        ),
        ("set_nominal", "foo = #6.258([* float64]) / [* float64]\n"),
        (
            "nested_elem_rule",
            "has_float = [uint, float64]\nfoo = [* has_float] ; @duplicates reject\n",
        ),
    ];
    for (tag, cddl) in reject_vectors {
        let msg = run(tag, cddl).expect_err(&format!(
            "float set element ({tag}) must be a graceful Err, not Ok"
        ));
        assert!(
            msg.contains("rule `Foo`") && msg.contains("float") && msg.contains("total order"),
            "float set rejection for {tag} should name the rule and the float cause, got: {msg}"
        );
    }

    // Control: no uniqueness requirement ⇒ a float element stays supported (plain Vec inner).
    run("plain_array", "foo = [* float64]\n")
        .expect("a plain float array without @duplicates reject must keep generating");
}

/// The `@duplicates` placements generate cleanly (no rejection) and select the right twin. For a
/// tag-258 SET the well-known-tag registry now defaults to `reject`, so: absent ⇒ `OrderedSet` (the
/// new default), explicit `reject` ⇒ byte-identical to absent (self-documentation), explicit
/// `preserve` ⇒ the OBSERVABLE opt-out back to plain `Vec` (today's wire behavior verbatim, no longer
/// byte-identical to absent). TABLE legs are UNCHANGED (a map is not a set, so the registry has no
/// entry): table `reject` stays a default no-op, table `preserve` stays the live `PairMap` twin.
/// Rust-only generation keeps this a fast unit check; the twin runtime + round-trip land in the
/// corpus/integration gates.
#[test]
fn duplicates_directive_accepts_live_and_default_noops() {
    let gen_src = |cddl: &str| -> std::collections::BTreeMap<String, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_dup_accept_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "dup_accept_unused",
            "--wasm=false",
        ]);
        let out = crate::api::generated_strings(&cli).unwrap_or_else(|e| {
            panic!("@duplicates should generate cleanly for {cddl:?}, got: {e}")
        });
        std::fs::remove_file(&path).ok();
        out
    };

    // array/set reject is LIVE: the transparent alias must name the uniqueness twin.
    let reject_set = gen_src("foo = #6.258([* uint]) / [* uint] ; @duplicates reject\n");
    let reject_src = reject_set.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        reject_src.contains("OrderedSet<u64>"),
        "array/set reject must lower to OrderedSet, got:\n{reject_src}"
    );

    let reject_neset = gen_src("foo = #6.258([+ uint]) / [+ uint] ; @duplicates reject\n");
    let reject_neset_src = reject_neset
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        reject_neset_src.contains("NonEmptyOrderedSet<u64>"),
        "non-empty set reject must lower to NonEmptyOrderedSet, got:\n{reject_neset_src}"
    );

    // The tag-258 set default flipped to reject (the well-known-tag registry): a no-directive 258
    // set now lowers to the `OrderedSet` uniqueness twin, NOT `Vec`.
    let absent_set = gen_src("foo = #6.258([* uint]) / [* uint]\n");
    let absent_src = absent_set.values().cloned().collect::<Vec<_>>().join("\n");
    // Phase 2.2: a named non-generic 258 set NOMINALIZES into a wrapper struct (no longer a transparent
    // alias); the reject default selects the `OrderedSet` inner.
    assert!(
        absent_src.contains("pub struct Foo")
            && absent_src.contains("OrderedSet<u64>")
            && !absent_src.contains("pub type Foo ="),
        "a no-directive 258 set nominalizes to a wrapper over the OrderedSet twin:\n{absent_src}"
    );
    // `@duplicates preserve` is now the OBSERVABLE opt-out: it restores the plain `Vec` inner (today's
    // wire behavior verbatim), so it is NO LONGER byte-identical to the absent (defaulted) case.
    let preserve_set = gen_src("foo = #6.258([* uint]) / [* uint] ; @duplicates preserve\n");
    let preserve_src = preserve_set
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        preserve_src.contains("pub struct Foo")
            && preserve_src.contains("Vec<u64>")
            && !preserve_src.contains("OrderedSet")
            && !preserve_src.contains("pub type Foo ="),
        "@duplicates preserve on a 258 set nominalizes with the plain Vec inner:\n{preserve_src}"
    );
    assert_ne!(
        absent_set, preserve_set,
        "post-flip, preserve is an observable opt-out — no longer byte-identical to the absent default"
    );
    // Explicit `@duplicates reject` now EQUALS the absent default (self-documentation): byte-identical.
    assert_eq!(
        reject_set, absent_set,
        "@duplicates reject on a 258 set is now the default: byte-identical to no directive"
    );

    // reject on a table is the default (no-op): byte-identical to no directive.
    assert_eq!(
        gen_src("foo = { * uint => text } ; @duplicates reject\n"),
        gen_src("foo = { * uint => text }\n"),
        "@duplicates reject on a table must be a no-op vs no directive"
    );

    // preserve on a table is LIVE: the transparent alias must name the vec-of-pairs twin.
    let preserve_table = gen_src("foo = { * uint => text } ; @duplicates preserve\n");
    let preserve_table_src = preserve_table
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        preserve_table_src.contains("PairMap<u64, String>"),
        "table preserve must lower to PairMap, got:\n{preserve_table_src}"
    );

    // preserve on a `{+ …}` table composes non-emptiness with the pair-map. This checks the RUST
    // surface (`--wasm=false`); the wasm leg (the `NonEmptyPairMap` wrapper) is pinned separately by
    // `duplicates_preserve_nonempty_table_lowers_to_twin_under_wasm`.
    let preserve_ne_table = gen_src("foo = { + uint => text } ; @duplicates preserve\n");
    let preserve_ne_table_src = preserve_ne_table
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        preserve_ne_table_src.contains("NonEmptyPairMap<u64, String>"),
        "non-empty table preserve must lower to NonEmptyPairMap, got:\n{preserve_ne_table_src}"
    );
}

/// `@duplicates` on a GENERIC def must ride instantiation: the policy is rule metadata on the
/// def, and every instantiation path must resolve to the policy's twin — the named-alias path
/// (`foo = oset<uint>`, also pinned by tests/corpus/tag_set_reject.cddl) AND the anonymous
/// member-position path (`holder = [g: oset<uint>]`, pinned by
/// tests/corpus/tag_set_reject_anon_generic.cddl), which registers the instance at the use site
/// without any rule in between. The tag-258 set default flipped to `reject` (the well-known-tag
/// registry), so `preserve` on the def is now the OBSERVABLE opt-out rather than a no-op: an absent
/// directive instantiates to `OrderedSet`, `@duplicates preserve` opts back out to plain `Vec`, and
/// that difference must ride BOTH instantiation paths.
#[test]
fn duplicates_on_generic_def_rides_instantiation() {
    let gen_src = |cddl: &str| -> std::collections::BTreeMap<String, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_dup_generic_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "dup_generic_unused",
            "--wasm=false",
        ]);
        let out = crate::api::generated_strings(&cli).unwrap_or_else(|e| {
            panic!("@duplicates on a generic def should generate cleanly for {cddl:?}, got: {e}")
        });
        std::fs::remove_file(&path).ok();
        out
    };
    let joined = |out: &std::collections::BTreeMap<String, String>| {
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // reject def + NAMED instance alias: the alias must resolve to the uniqueness twin.
    let named =
        gen_src("oset<a0> = #6.258([* a0]) / [* a0] ; @duplicates reject\nfoo = oset<uint>\n");
    assert!(
        joined(&named).contains("OrderedSet<u64>"),
        "reject on a generic def must reach a named instance, got:\n{}",
        joined(&named)
    );

    // reject def + ANONYMOUS member-position instance: the seam the named-alias corpus vector
    // does NOT cover.
    let anon = gen_src(
        "oset<a0> = #6.258([* a0]) / [* a0] ; @duplicates reject\nholder = [g: oset<uint>]\n",
    );
    assert!(
        joined(&anon).contains("OrderedSet<u64>"),
        "reject on a generic def must reach an anonymous member-position instance, got:\n{}",
        joined(&anon)
    );

    // preserve def is now the OBSERVABLE opt-out (the set default flipped to reject): a no-directive
    // generic 258 set def instantiates to `OrderedSet`, while `@duplicates preserve` on the def opts
    // back out to plain `Vec` — different bytes — and that difference must ride BOTH instantiation
    // paths (the named alias AND the anonymous member position).
    for use_site in [
        "foo = oset<uint>\n",         // named-alias instantiation path
        "holder = [g: oset<uint>]\n", // anonymous member-position instantiation path
    ] {
        let absent = gen_src(&format!("oset<a0> = #6.258([* a0]) / [* a0]\n{use_site}"));
        let preserve = gen_src(&format!(
            "oset<a0> = #6.258([* a0]) / [* a0] ; @duplicates preserve\n{use_site}"
        ));
        assert!(
            joined(&absent).contains("OrderedSet<u64>"),
            "a no-directive generic 258 set def must instantiate to OrderedSet via `{use_site}`:\n{}",
            joined(&absent)
        );
        assert!(
            joined(&preserve).contains("Vec<u64>") && !joined(&preserve).contains("OrderedSet"),
            "@duplicates preserve on a generic set def must opt back out to Vec via `{use_site}`:\n{}",
            joined(&preserve)
        );
        assert_ne!(
            absent, preserve,
            "preserve on a generic set def is now an observable opt-out, not a no-op, via `{use_site}`"
        );
    }
}

/// The well-known-tag registry (`well_known_tag_default_duplicates`) defaults a no-directive tag-258
/// set to `@duplicates reject` ONLY where the shape guard matches — `#6.258` directly wrapping a
/// homogeneous occurrence collection. These are the guard NEGATIVES and the non-258 boundary: none
/// of them may acquire the `OrderedSet` uniqueness twin, so each proves the registry did NOT fire
/// where set semantics are meaningless (a record-shaped array, a tagged primitive, a map) or where
/// the tag is not the IANA set tag (259). The positive flips are pinned by
/// `tests/corpus/tag_set_default.cddl`; the explicit-directive precedence is pinned by
/// `duplicates_directive_accepts_live_and_default_noops`.
#[test]
fn well_known_tag_258_default_shape_guard_negatives() {
    let gen_src = |cddl: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_wk258_guard_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "wk258_guard_unused",
            "--wasm=false",
        ]);
        let out = crate::api::generated_strings(&cli).unwrap_or_else(|e| {
            panic!("guard-negative spec must generate cleanly for {cddl:?}: {e}")
        });
        std::fs::remove_file(&path).ok();
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // `#6.258(text)`: a tagged PRIMITIVE, not a collection — becomes a Wrapper. The registry never
    // sees it (it never reaches the array/table construction sites), so no uniqueness twin.
    let tagged_primitive = gen_src("foo = #6.258(text)\nholder = [f: foo]\n");
    assert!(
        !tagged_primitive.contains("OrderedSet"),
        "a tagged primitive `#6.258(text)` must NOT acquire a set twin:\n{tagged_primitive}"
    );

    // `#6.258([uint, text])`: a RECORD-shaped array (heterogeneous tuple positions) — uniqueness of
    // tuple positions is meaningless, and it becomes a Record struct, never a homogeneous Array.
    let record_shaped = gen_src("foo = #6.258([uint, text])\nholder = [f: foo]\n");
    assert!(
        !record_shaped.contains("OrderedSet"),
        "a record-shaped `#6.258([uint, text])` must NOT acquire a set twin:\n{record_shaped}"
    );

    // `#6.258({* k => v})`: a MAP, not a set — the registry returns `None` for a map inner (`is_array`
    // false), so it stays the plain table representation with no uniqueness twin.
    let tagged_map = gen_src("foo = #6.258({ * uint => text })\nholder = [f: foo]\n");
    assert!(
        !tagged_map.contains("OrderedSet"),
        "a tagged map `#6.258({{* k => v}})` must NOT acquire a set twin (a map is not a set):\n{tagged_map}"
    );

    // Non-258 collapse (`#6.259([* uint]) / [* uint]`): the collapse is tag-agnostic but the registry
    // is not — only 258 carries set semantics, so a 259 idiom keeps today's PRESERVE default (`Vec`).
    // (It WRAPS, like every tagged rule body since T1-13 — the boundary this pins is the INNER
    // representation, not the wrapping.)
    let non_258 = gen_src("foo = #6.259([* uint]) / [* uint]\nholder = [f: foo]\n");
    assert!(
        non_258.contains("pub struct Foo(pub(crate) Vec<u64>)") && !non_258.contains("OrderedSet"),
        "a non-258 collapsed idiom must keep the plain `Vec` preserve default:\n{non_258}"
    );
}

/// A bounded-occurrence tag-258 set (`#6.258([3*5 uint]) / [3*5 uint]`) composes the registry's
/// reject default with a general occurrence bound: the reject twin picker routes the collected `Vec`
/// through `OrderedSet::try_from` (uniqueness) and THEN a runtime length check for the `3*5` window
/// (`src/generation/deserialize.rs`). Bounded-reject is therefore SUPPORTED — no guard-exclusion, no
/// panic; this pins that a bounded 258 set gets the twin AND still enforces its length window.
#[test]
fn well_known_tag_258_bounded_occurrence_composes_with_reject_twin() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_wk258_bounded_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "foo = #6.258([3*5 uint]) / [3*5 uint]\nholder = [f: foo]\n",
    )
    .unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "wk258_bounded_unused",
        "--wasm=false",
    ]))
    .expect("a bounded tag-258 set must generate cleanly (bounded-reject is supported)");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct Foo")
            && src.contains("OrderedSet<u64>")
            && !src.contains("pub type Foo ="),
        "a bounded 258 set nominalizes to a wrapper over the OrderedSet uniqueness twin:\n{src}"
    );
    assert!(
        src.contains("OrderedSet::try_from"),
        "the bounded 258 set must route through the OrderedSet uniqueness door:\n{src}"
    );
    // the `3*5` window survives as a runtime length check on the accepted (unique) collection
    assert!(
        src.contains(".len()") && (src.contains("3") && src.contains("5")),
        "the `3*5` occurrence window must still be enforced as a length check:\n{src}"
    );
}

/// Inline-position tag-258 arrays NOMINALIZE into shape-derived `Set<Elem>` wrappers (Phase 2.4) —
/// minted at the single post-collapse seam (`IntermediateTypes::nominalize_inline_sets`), one wrapper
/// per deduped inline shape, each owning its `{tag, len, elem}` encodings and defaulting to
/// `@duplicates reject` (IANA set semantics). Every inline tagged position feeds it: a member, an
/// optional member, an array element. `#6.258([* uint])` mints `SetU64` (wrapping the `OrderedSet<u64>`
/// reject twin), `#6.258([* text])` mints `SetText`; a repeated inline shape (`a` and `c`'s element
/// here) DEDUPES to one nominal. Holds under BOTH the default and `--preserve-encodings` profiles.
/// Guard NEGATIVES ride along: an inline two-arm choice `#6.258([* uint]) / [* uint]` STAYS a
/// two-variant enum (inline unions do NOT collapse — the documented REQUEST-08 recognition boundary;
/// only the tagged arm nominalizes, the untagged arm stays a plain `Vec<u64>`), and the 258-shape guard
/// still excludes an inline map (`#6.258({* k => v})` — a map is not a set) and an inline record-shaped
/// array (`#6.258(<record>)` — a Record, not a homogeneous collection). The hoist opt-out is pinned
/// separately by `inline_258_reject_opts_out_via_hoist_to_named_rule`.
#[test]
fn inline_258_array_defaults_to_reject() {
    let gen_src = |cddl: &str, preserve: bool| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_inline258_{}_{}_{}.cddl",
            std::process::id(),
            preserve as u8,
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "inline258_unused",
            "--wasm=false",
        ];
        if preserve {
            args.push("--preserve-encodings=true");
        }
        let out = crate::api::generated_strings(&Cli::parse_from(args))
            .unwrap_or_else(|e| panic!("inline 258 spec must generate cleanly for {cddl:?}: {e}"));
        std::fs::remove_file(&path).ok();
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // Every inline seam: `a` a mandatory member, `b` an optional member, `c` an array element. Each is
    // an inline `#6.258([* …])`, so each nominalizes to a shape-derived `Set<Elem>` wrapper.
    const HOLDER: &str =
        "holder = [a: #6.258([* uint]), ? b: #6.258([* text]), c: [* #6.258([* uint])]]\n";
    for preserve in [false, true] {
        let src = gen_src(HOLDER, preserve);
        // `a` (member) and `c` (array element) are `[* uint]` sets → one deduped nominal `SetU64`,
        // referenced directly (`a`) and at element depth (`c`: `Vec<SetU64>`). The nominal wraps the
        // `OrderedSet<u64>` reject twin.
        // (`pub struct SetU64` is a tuple struct by default, a named-field struct with an `encodings`
        // member under `--preserve-encodings` — assert the ident + inner twin, not the struct shape.)
        assert!(
            src.contains("pub struct SetU64") && src.contains("OrderedSet<u64>"),
            "inline 258 member/element sets must nominalize to SetU64 over OrderedSet<u64> (preserve={preserve}):\n{src}"
        );
        assert!(
            src.contains("pub a: SetU64") && src.contains("pub c: Vec<SetU64>"),
            "inline 258 member/element fields must reference the SetU64 nominal (preserve={preserve}):\n{src}"
        );
        // `b` (optional member) is a `[* text]` set → nominal `SetText` (over `OrderedSet<String>`),
        // referenced under an `Option`.
        assert!(
            src.contains("pub struct SetText")
                && src.contains("OrderedSet<String>")
                && src.contains("pub b: Option<SetText>"),
            "inline 258 optional-member set must nominalize to SetText and ride an Option (preserve={preserve}):\n{src}"
        );
    }

    // Guard negative — inline two-arm choice: inline unions do NOT collapse (REQUEST-08 boundary). It
    // stays a two-variant enum; only the tagged arm nominalizes (to `SetU64`), the untagged arm stays a
    // plain `Vec<u64>`.
    let two_arm = gen_src("holder = [x: #6.258([* uint]) / [* uint]]\n", false);
    assert!(
        two_arm.contains("pub enum"),
        "inline two-arm 258 choice must stay a two-variant enum (not collapse to a single set):\n{two_arm}"
    );
    assert!(
        two_arm.contains("(SetU64)") && two_arm.contains("(Vec<u64>)"),
        "the inline two-arm enum's tagged arm nominalizes to SetU64 while the untagged arm stays a plain Vec:\n{two_arm}"
    );

    // Guard negative — inline map: `#6.258({* k => v})` is a Map, not an Array. The registry returns
    // `None` for a map inner, so no uniqueness twin appears.
    let inline_map = gen_src("holder = [m: #6.258({ * uint => text })]\n", false);
    assert!(
        !inline_map.contains("OrderedSet") && !inline_map.contains("pub struct Set"),
        "an inline 258 map must NOT acquire a set twin OR a set nominal (a map is not a set):\n{inline_map}"
    );

    // Guard negative — inline record-shaped array under a 258 tag. A bare inline heterogeneous group
    // (`[uint, text]`) is not an expressible inline shape (it needs a name), so this uses the expressible
    // form: a named record referenced under an inline `#6.258(...)`. Its conceptual type is a Record
    // (`Rust(ident)`), never a homogeneous `Array`, so the registry (`is_array` false) never fires.
    let record_shaped = gen_src(
        "rec_inner = [uint, text]\nholder = [r: #6.258(rec_inner)]\n",
        false,
    );
    assert!(
        !record_shaped.contains("OrderedSet") && !record_shaped.contains("pub struct Set"),
        "an inline record-shaped `#6.258(<record>)` must NOT acquire a set twin OR a set nominal:\n{record_shaped}"
    );
}

/// BOUNDARY-RETIREMENT pin (Phase 2.4): an inline `#6.258` array nested INSIDE a named two-arm idiom
/// rule NOW nominalizes, exactly like any other inline occurrence — the former
/// `SUPPRESS_INLINE_TAG_DEFAULT` suppression boundary is gone. Nominalization runs at a single
/// post-collapse seam (`IntermediateTypes::nominalize_inline_sets`) over the finalized construction
/// products, so a nested inline occurrence inside the OUTER rule's collapsed nominal is reached the
/// same as one in a plain member. The outer rule collapses to nominal `Foo` (Phase 2.2); its element
/// is the inner inline set nominalized to `SetU64`, so `Foo` wraps `OrderedSet<SetU64>` (previously
/// `OrderedSet<Vec<u64>>`, the retired boundary). This replaces the former
/// `nested_inline_258_inside_named_idiom_keeps_vec_documented_boundary` pin, retired together with its
/// TESTING_ROADMAP ledger entry and the `current_capacities.mdx` boundary note.
#[test]
fn nested_inline_258_inside_named_idiom_nominalizes() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_nested258_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "foo = #6.258([* #6.258([* uint])]) / [* #6.258([* uint])]\n",
    )
    .unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "nested258_unused",
        "--wasm=false",
    ]))
    .expect("nested inline 258 inside the named idiom must generate cleanly");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct Foo")
            && src.contains("OrderedSet<SetU64>")
            && src.contains("pub struct SetU64(")
            && !src.contains("pub type Foo ="),
        "the nested inline occurrence must nominalize to SetU64 (boundary retired): Foo wraps \
         OrderedSet<SetU64>, not OrderedSet<Vec<u64>>. Got:\n{src}"
    );
}

/// The hoist recipe the inline-258 generation notice prints actually works: extracting the inline
/// occurrence to a named rule carrying `; @duplicates preserve` opts back out to the plain `Vec` twin
/// (today's wire behavior verbatim), while the same named rule WITHOUT the directive keeps the reject
/// default. This rides the 1a named-rule machinery — the assert is the point (the notice must not point
/// consumers at a recipe that doesn't opt out).
#[test]
fn inline_258_reject_opts_out_via_hoist_to_named_rule() {
    let gen_src = |cddl: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_inline258_hoist_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "inline258_hoist_unused",
            "--wasm=false",
        ]))
        .unwrap_or_else(|e| panic!("hoisted 258 spec must generate cleanly for {cddl:?}: {e}"));
        std::fs::remove_file(&path).ok();
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // Hoisted WITH `; @duplicates preserve`: the named rule opts out to the plain `Vec` twin.
    let opted_out =
        gen_src("named = #6.258([* uint]) ; @duplicates preserve\nholder = [a: named]\n");
    assert!(
        opted_out.contains("pub struct Named")
            && opted_out.contains("Vec<u64>")
            && !opted_out.contains("OrderedSet")
            && !opted_out.contains("pub type Named ="),
        "hoisting to a named rule with `; @duplicates preserve` nominalizes with the plain Vec inner:\n{opted_out}"
    );

    // The SAME hoisted rule WITHOUT the directive keeps the reject default (parity with the inline seam).
    let hoisted_default = gen_src("named = #6.258([* uint])\nholder = [a: named]\n");
    assert!(
        hoisted_default.contains("pub struct Named")
            && hoisted_default.contains("OrderedSet<u64>")
            && !hoisted_default.contains("pub type Named ="),
        "a hoisted named 258 set without a directive nominalizes and keeps the reject default:\n{hoisted_default}"
    );
}

/// The shape-derived name distinguishes the `[+]` non-empty flavor from `[*]`, and DEDUPES one nominal
/// per distinct inline shape: `#6.258([+ text])` mints `SetNonEmptyText` (over `NonEmptyOrderedSet`)
/// while `#6.258([* text])` mints a SEPARATE `SetText` (over `OrderedSet`), and two occurrences of the
/// same shape share one nominal.
#[test]
fn inline_258_nominal_names_distinguish_nonempty_and_dedupe() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_inline258_ne_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "holder = [a: #6.258([+ text]), b: #6.258([* text]), c: #6.258([* text])]\n",
    )
    .unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "inline258_ne_unused",
        "--wasm=false",
    ]))
    .expect("inline 258 non-empty/dedup spec must generate cleanly");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct SetNonEmptyText") && src.contains("NonEmptyOrderedSet<String>"),
        "the `[+ text]` inline set must mint SetNonEmptyText over NonEmptyOrderedSet:\n{src}"
    );
    assert!(
        src.contains("pub struct SetText")
            && src.contains("pub b: SetText")
            && src.contains("pub c: SetText"),
        "the two `[* text]` occurrences must DEDUPE to one SetText nominal both fields reference:\n{src}"
    );
    // exactly one `SetText` struct definition (dedup) — count the tuple/named struct header.
    let set_text_defs = src.matches("pub struct SetText").count();
    assert_eq!(
        set_text_defs, 1,
        "SetText must be minted exactly once (deduped):\n{src}"
    );
}

/// A shape-derived inline nominal name that collides with an already-defined rule / generic set
/// instantiation is REFUSED loudly (the per-kind sibling of the duplicate-top-level-ident backstop),
/// never silently re-pointed at the colliding type. Both a user type alias (`set_u64 = text` →
/// `SetU64`) and a generic instantiation (`set<uint>` → `SetU64`, a structurally-different nominal)
/// trigger the same set-specific message. The pinned substring is a message key.
#[test]
fn inline_258_nominal_name_collision_is_rejected() {
    let gen_err = |cddl: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_inline258_col_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let err = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "inline258_col_unused",
            "--wasm=false",
        ]))
        .expect_err(
            "a shape-derived inline nominal colliding with an existing name must be rejected",
        );
        std::fs::remove_file(&path).ok();
        err.to_string()
    };

    // User type alias claiming `SetU64`.
    let alias_err = gen_err("set_u64 = text\nholder = [a: #6.258([* uint]), b: set_u64]\n");
    assert!(
        alias_err.contains("shape-derived nominal `SetU64`") && alias_err.contains("collides"),
        "user-alias collision must name the shape-derived nominal and say it collides: {alias_err}"
    );

    // Generic instantiation `set<uint>` → `SetU64` (a DIFFERENT, optional-tag nominal).
    let generic_err = gen_err(
        "set<a0> = #6.258([* a0]) / [* a0]\nholder = [a: #6.258([* uint]), b: set<uint>]\n",
    );
    assert!(
        generic_err.contains("shape-derived nominal `SetU64`") && generic_err.contains("collides"),
        "generic-instantiation collision must name the shape-derived nominal and say it collides: {generic_err}"
    );
}

/// A `{+ …}` `@duplicates preserve` table generates a full wasm wrapper (the WP-P2A stopgap that
/// rejected it under `--wasm` is gone). The rule's JS class wraps `NonEmptyPairMap<K, V>` (not the
/// loose `NonEmptyMap`), enters through a `try_from(&loose_pair_map_wrapper)` door, and its `new`
/// builds the pair-map twin — so every shape that generates for rust generates for wasm. The loose
/// `try_from` source wrapper is the pair-map-FLAVORED structural class
/// (`PairMapU64ToText(pub(crate) PairMap<u64, String>)`) — the container flavor is part of the
/// structural name, so it is never the keyed `MapU64ToText`. A scratch e2e (rust+wasm+json-gen cargo-check)
/// backs this; the string pins catch a regression without the crate-build cost.
#[test]
fn duplicates_preserve_nonempty_table_lowers_to_twin_under_wasm() {
    const CDDL: &str = "foo = { + uint => text } ; @duplicates preserve\n\
                        holder = [m: foo]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dup_ne_preserve_wasm_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "dup_ne_preserve_wasm_unused",
        "--wasm=true",
    ]))
    .expect("a `{+ …}` preserve table must GENERATE under --wasm (no rejection)");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // the rule's wasm class wraps the NonEmptyPairMap twin
    assert!(
        src.contains("pub struct Foo(pub(crate) NonEmptyPairMap<u64, String>)"),
        "the `{{+ …}}` preserve rule's wasm class must wrap NonEmptyPairMap, got:\n{src}"
    );
    // it enters through the loose PairMap-wrapper `try_from` door, building the pair-map twin
    assert!(
        src.contains("NonEmptyPairMap::try_from(inner)"),
        "the wasm wrapper must enter through the NonEmptyPairMap try_from door, got:\n{src}"
    );
    // the loose `try_from` source wrapper is the pair-map-flavored class wrapping the PairMap twin
    assert!(
        src.contains("pub struct PairMapU64ToText(pub(crate) PairMap<u64, String>)"),
        "the loose try_from source must be the flavored PairMapU64ToText wrapping PairMap, got:\n{src}"
    );
    // the keyed structural name is NOT minted for a pure-preserve spec
    assert!(
        !src.contains("pub struct MapU64ToText"),
        "a pure-preserve spec must not mint the default-flavored keyed class, got:\n{src}"
    );
}

/// The wasm face of a `@newtype`/TAG-forced WRAPPER over a preserve table: the wrapper's
/// `new`/getter boundary names the flavored structural class, and the crate MINTS that class plus a
/// `collections.rs` index row for it. Both halves are the pin — a boundary naming a class nobody
/// mints is E0425 in the generated wasm crate, at the exact remove where the spec author cannot see
/// it, and an index missing a defined wrapper breaks every downstream `--extern-wrapper-index`
/// deferral. The `{+}` flavor rides the same path through its restricted `NonEmptyPairMapKToV` door.
#[test]
fn tagged_preserve_table_wrapper_boundary_mints_its_pair_map_class() {
    const CDDL: &str = "tagged_pt = #6.24({ * uint => text }) ; @duplicates preserve\n\
                        tagged_pt_ne = #6.25({ + uint => text }) ; @duplicates preserve\n\
                        holder = [a: tagged_pt, b: tagged_pt_ne]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_tagged_pt_wasm_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "tagged_pt_wasm_unused",
        "--wasm=true",
    ]))
    .expect("a tagged preserve table must GENERATE under --wasm");
    std::fs::remove_file(&path).ok();
    let wasm = out
        .get("wasm/src/generated/mod.rs")
        .expect("wasm mod.rs must be emitted");
    for (wrapper, class) in [
        ("TaggedPt", "PairMapU64ToText"),
        ("TaggedPtNe", "NonEmptyPairMapU64ToText"),
    ] {
        assert!(
            wasm.contains(&format!(
                "pub fn new(inner: &{class}) -> Self {{\n        Self(cddl_lib::{wrapper}::new("
            )),
            "`{wrapper}`'s wasm ctor must take the flavored `{class}`, got:\n{wasm}"
        );
        assert!(
            wasm.contains(&format!("pub struct {class}(")),
            "the crate must MINT `{class}` — a boundary naming an unminted class is E0425, \
             got:\n{wasm}"
        );
    }
    // the wrapper classes own their tag, so they expose a real standalone codec
    assert!(
        wasm.contains("pub fn from_cbor_bytes(cbor_bytes: &[u8]) -> Result<TaggedPt, JsError>"),
        "the wasm wrapper class must expose the codec the tag-owning rust wrapper gained, got:\n{wasm}"
    );
    let index = out
        .get("wasm/src/generated/collections.rs")
        .expect("the collection-wrapper index must be emitted");
    for class in ["PairMapU64ToText", "NonEmptyPairMapU64ToText"] {
        assert!(
            index.contains(&format!("pub use crate::generated::{class};")),
            "the wrapper-minted `{class}` must appear in the collections index, got:\n{index}"
        );
    }
}

/// A `@duplicates preserve` construct and a non-preserve construct of the IDENTICAL key/value both
/// cross the wasm boundary, each through its OWN structural class: the container flavor is part of
/// the structural name (`PairMapKToV` vs `MapKToV`), so one name is never asked to be two
/// structurally different types. The regression vector is the mixed-policy open-struct-map pair —
/// two rest rows of the same `K`/`V`, one preserve, one not. Before the flavored names this
/// generated exit-0 with a SINGLE `MapLblToVal` wrapping `PairMap`, both getters returning it, and
/// the emitted wasm crate failed `cargo check` (E0277: the non-preserve getter's
/// `From<OrderedHashMap<..>>` did not exist). A scratch e2e (rust+wasm cargo-check of exactly this
/// spec) backs the compile claim; the string pins catch a regression without the crate-build cost.
#[test]
fn mixed_policy_map_shapes_mint_distinct_flavored_wasm_wrappers() {
    // the rest-row directive trails the WHOLE entry on its own line — a `; @duplicates preserve }`
    // spelling would swallow the closing brace into the comment
    const CDDL: &str = "lbl = uint\n\
                        val = text\n\
                        pres = {\n\
                        1: uint,\n\
                        * lbl => val ; @duplicates preserve\n\
                        }\n\
                        plain = {\n\
                        2: uint,\n\
                        * lbl => val\n\
                        }\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_mixed_policy_flavors_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "mixed_policy_flavors_unused",
        "--wasm=true",
        "--preserve-encodings=true",
        "--canonical-form=true",
    ]))
    .expect("a mixed-policy same-shape spec must GENERATE (the flavors no longer collide)");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // two distinct wasm classes, each wrapping its own container
    assert!(
        src.contains(
            "pub struct PairMapLblToVal(pub(crate) PairMap<cddl_lib::Lbl, cddl_lib::Val>)"
        ),
        "the preserve rest row must mint its own PairMap-backed class, got:\n{src}"
    );
    assert!(
        src.contains(
            "pub struct MapLblToVal(pub(crate) OrderedHashMap<cddl_lib::Lbl, cddl_lib::Val>)"
        ),
        "the non-preserve rest row must keep the keyed structural class, got:\n{src}"
    );
    // each getter returns its OWN class (the E0277 was one name serving both)
    assert!(
        src.contains("pub fn rest(&self) -> PairMapLblToVal"),
        "the preserve struct's rest getter must return the pair-map class, got:\n{src}"
    );
    assert!(
        src.contains("pub fn rest(&self) -> MapLblToVal"),
        "the non-preserve struct's rest getter must return the keyed class, got:\n{src}"
    );
    // each `From` matches its own inner
    assert!(
        src.contains("impl From<PairMap<cddl_lib::Lbl, cddl_lib::Val>> for PairMapLblToVal"),
        "the pair-map class needs its own From, got:\n{src}"
    );
    assert!(
        src.contains("impl From<OrderedHashMap<cddl_lib::Lbl, cddl_lib::Val>> for MapLblToVal"),
        "the keyed class needs its own From, got:\n{src}"
    );
}

/// An ANONYMOUS inline table carrying `@duplicates preserve` on its own row composes with every
/// surface the member-level pair-map has — the point being that the policy now rides a use-site type
/// with no rule of its own, so nothing can look the container up by rule ident.
///
/// The load-bearing position is the TYPE-CHOICE ARM (an enum variant holding the map), because that
/// is the shape the CIP-25 metadatum grammar needs and the one whose wasm surface has to synthesize a
/// wrapper class for a type the author never named. The `{+ …}` arm beside it proves the occurrence
/// bound composes with the flavor rather than replacing it.
///
/// A scratch e2e (rust + wasm + json-gen `cargo check` of this spec under default, preserve,
/// canonical, wasm and json profiles, plus a duplicate-keyed byte-exact round-trip at the arm
/// position) backs the compile claim; the string pins catch a regression without the crate-build
/// cost. The COMPILED wire evidence lives in `integration_tests::open_table_cip25_acceptance`, whose
/// two noisy vectors decode duplicates out of exactly this shape.
#[test]
fn an_inline_preserve_table_arm_carries_the_pair_map_through_every_surface() {
    // the row directive trails the row on its own line — a `; @duplicates preserve }` spelling
    // would swallow the closing brace into the comment
    // `g`'s table is written INLINE at the field too: a NAMED `{+ …}` rule takes its policy from
    // the RULE slot (`ne = { + k => v } ; @duplicates preserve`), which is a different comment.
    const CDDL: &str = "tmd = { * uint => text ; @duplicates preserve\n\
                        } / int\n\
                        holder = [f: tmd, g: { + uint => text ; @duplicates preserve\n\
                        }]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_inline_preserve_arm_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let generate = |extra: &[&str]| {
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "inline_preserve_arm_unused",
        ];
        args.extend_from_slice(extra);
        crate::api::generated_strings(&Cli::parse_from(args))
            .expect("an inline preserve table arm must generate")
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    };

    // rust, default profile: the ARM's payload is the pair-map twin, and the `{+}` field is its
    // non-empty flavor — both at member positions that have no rule ident to look up.
    let rust = generate(&["--wasm=false"]);
    assert!(
        rust.contains("MapU64ToText(PairMap<u64, String>)"),
        "the type-choice arm's payload must be the pair-map twin, got:\n{rust}"
    );
    assert!(
        rust.contains("NonEmptyPairMap<u64, String>"),
        "the `{{+ …}}` inline table must compose the min-1 bound with the flavor, got:\n{rust}"
    );

    // preserve/canonical: the arm gains the per-entry encoding sidecars a pair map needs to replay
    // duplicate-keyed bytes in wire order (a loose table's sidecar is keyed, and cannot).
    let canonical = generate(&[
        "--wasm=false",
        "--preserve-encodings=true",
        "--canonical-form=true",
    ]);
    assert!(
        canonical.contains("map_u64_to_text_key_encodings")
            && canonical.contains("map_u64_to_text_value_encodings"),
        "the preserve arm needs positional key/value encoding sidecars, got:\n{canonical}"
    );

    // wasm: the arm's payload crosses as the FLAVORED structural class, synthesized for a type the
    // author never named — and the enum's accessors on both directions name that same class.
    let wasm = generate(&["--wasm=true", "--preserve-encodings=true"]);
    assert!(
        wasm.contains("pub struct PairMapU64ToText(pub(crate) PairMap<u64, String>)"),
        "the inline preserve arm must mint the flavored loose class, got:\n{wasm}"
    );
    assert!(
        wasm.contains(
            "pub struct NonEmptyPairMapU64ToText(pub(crate) NonEmptyPairMap<u64, String>)"
        ),
        "the inline `{{+ …}}` preserve table must mint the non-empty flavored class, got:\n{wasm}"
    );
    assert!(
        wasm.contains("pub fn as_map_u64_to_text(&self) -> Option<PairMapU64ToText>"),
        "the variant getter must return the flavored class, got:\n{wasm}"
    );
    assert!(
        wasm.contains("pub fn new_map_u64_to_text(map_u64_to_text: &PairMapU64ToText)"),
        "the variant constructor must take the flavored class, got:\n{wasm}"
    );

    // json + schemars: the enum derives them over a variant whose payload is the pair map, whose own
    // JSON image is an array of `[k, v]` pairs (a JSON object cannot carry duplicate keys). The
    // pair-map runtime module itself is a COPIED static file (and its `use` is injected at export),
    // so the derive over the variant is what the string emit can see; the scratch `cargo check` of
    // the json-gen crate over this spec is what proves the two fit together.
    let json = generate(&[
        "--wasm=true",
        "--json-serde-derives=true",
        "--json-schema-export=true",
    ]);
    assert!(
        json.contains(
            "#[derive(Clone, Debug, serde::Deserialize, serde::Serialize, schemars::JsonSchema)]\n\
             pub enum Tmd {\n    MapU64ToText(PairMap<u64, String>),"
        ),
        "the enum must derive serde+schemars over the pair-map variant, got:\n{json}"
    );

    std::fs::remove_file(&path).ok();
}

/// The rule-ident-vs-wrapper-ident sibling for the LOOSE pair-map wrapper name family: a user rule
/// spelling `PairMapKToV` shadows the class a `@duplicates preserve` construct mints under exactly
/// that name. Parallel to the NonEmpty/reject siblings (per-kind detectors with distinct pinned
/// message texts), so a failing spec points at the right container kind.
#[test]
fn preserve_pair_map_loose_wrapper_ident_collision_rejects_gracefully() {
    // `pair_map_u64_to_text` is a plain struct rule claiming the ident the preserve rest row's
    // structural wrapper needs.
    const CDDL: &str = "pair_map_u64_to_text = [x: uint]\n\
                        holder = {\n\
                        1: uint,\n\
                        * uint => text ; @duplicates preserve\n\
                        }\n\
                        user = [p: pair_map_u64_to_text]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_pmap_loose_ident_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "pmap_loose_ident_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result
        .expect_err("a user rule claiming the loose pair-map wrapper ident must be a graceful Err");
    let msg = err.to_string();
    assert!(
        msg.contains("PairMapU64ToText") && msg.contains("PairMap wrapper"),
        "the message must name the claimed ident and the pair-map twin (distinct from the \
         NonEmpty/reject siblings), got: {msg}"
    );
}

/// The second mint source of the same name family: a `@newtype`/TAG-forced WRAPPER over an inline
/// `{* k => v} ; @duplicates preserve` names the structural `PairMapKToV` class at its wasm
/// `new`/getter boundary, and the wasm struct walk mints it there. Without this source enumerated in
/// the detector the collision fell through to the generic `export.rs` duplicate-ident backstop —
/// loud, but in neither the spec author's terms nor the right container kind's voice.
#[test]
fn preserve_pair_map_wrapper_inner_ident_collision_rejects_gracefully() {
    // `pair_map_u64_to_text` is a plain struct rule claiming the ident the tagged preserve table
    // wrapper's boundary names.
    const CDDL: &str = "pair_map_u64_to_text = [x: uint]\n\
                        tagged_pt = #6.24({ * uint => text }) ; @duplicates preserve\n\
                        holder = [t: tagged_pt, p: pair_map_u64_to_text]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_pmap_wrapper_ident_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "pmap_wrapper_ident_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result.expect_err(
        "a user rule claiming the wrapper-minted pair-map ident must be a graceful Err",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("PairMapU64ToText")
            && msg.contains("PairMap wrapper")
            && msg.contains("wrapped by rule 'TaggedPt'"),
        "the message must name the claimed ident, the pair-map twin and the WRAPPER rule that \
         mints it, got: {msg}"
    );
}

/// The DEFAULT-flavored twin of the pin above: an open struct-map rest row with no `@duplicates`
/// directive mints the loose `MapKToV` its wasm getter returns, so a user rule spelling that ident
/// shadows it exactly as the preserve row shadows `PairMapKToV`. Until this leg existed the default
/// flavor fell through to the generic `export.rs` duplicate-ident backstop while its preserve twin
/// got a per-kind message — the message text here is what closes that asymmetry, so it is pinned.
#[test]
fn default_rest_row_loose_map_wrapper_ident_collision_rejects_gracefully() {
    // `map_u64_to_text` is a plain struct rule claiming the ident the DEFAULT rest row's structural
    // wrapper mints.
    const CDDL: &str = "map_u64_to_text = [x: uint]\n\
                        holder = {\n\
                        1: uint,\n\
                        * uint => text\n\
                        }\n\
                        user = [p: map_u64_to_text]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_rest_loose_map_ident_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "rest_loose_map_ident_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result
        .expect_err("a user rule claiming the loose map wrapper ident must be a graceful Err");
    let msg = err.to_string();
    assert!(
        msg.contains("MapU64ToText")
            && msg.contains("open struct-map rest row of 'Holder'")
            && msg.contains("loose map wrapper"),
        "the message must name the claimed ident, the rest row that mints it, and the DEFAULT \
         (non-PairMap) flavor, got: {msg}"
    );
    assert!(
        !msg.contains("duplicate top-level ident"),
        "the per-kind detector must fire BEFORE the generic duplicate-ident backstop, got: {msg}"
    );
}

/// The self-named leg of the same family, from the rest row's side: a `{+ k => v}` rule whose ident
/// IS the loose builder name owns that ident for its RESTRICTED class, so a rest row of the same
/// key/value has no loose class left to return. The row's need is invisible to the conceptual walk
/// (the IR stores its key/value flat), so it is registered from the row's container type.
#[test]
fn rest_row_loose_map_need_vs_self_named_non_empty_rule_rejects_gracefully() {
    const CDDL: &str = "map_u64_to_text = {+ uint => text}\n\
                        holder = {\n\
                        1: uint,\n\
                        * uint => text\n\
                        }\n\
                        user = [p: map_u64_to_text]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_rest_loose_map_need_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "rest_loose_map_need_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result.expect_err(
        "a self-named `{+ …}` rule starving a rest row of its loose builder must be a graceful Err",
    );
    let msg = err.to_string();
    assert!(
        msg.contains(
            "an open struct-map rest row of the same key/value needs for its loose \
                      'MapU64ToText' table wrapper"
        ),
        "the self-named leg must name the rest row as the use that needs the loose builder, got: \
         {msg}"
    );
}

/// The LIST-family sibling of the pin above, both rest shapes: a `* K => V` row's wasm class needs
/// the loose `<K>List` for its `keys()`, and a `* T` tail's getter needs the loose `<T>List` itself.
/// A self-named `[+ elem]` rule owns that ident for its restricted class, leaving neither with a
/// class of the right shape.
#[test]
fn rest_row_loose_list_needs_vs_self_named_non_empty_rule_reject_gracefully() {
    let run = |cddl: &str, tag: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rest_loose_list_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, cddl).unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rest_loose_list_unused",
            "--wasm=true",
        ]));
        std::fs::remove_file(&path).ok();
        result
            .expect_err("a self-named `[+ …]` rule starving a rest row must be a graceful Err")
            .to_string()
    };

    // (a) the map row's keys() wrapper
    let msg = run(
        "md = int / bytes / text\n\
         md_list = [+ md]\n\
         holder = {\n\
         1: uint,\n\
         * md => text\n\
         }\n",
        "keys",
    );
    assert!(
        msg.contains(
            "an open struct-map rest row's keys() wrapper of the same element needs for its loose \
             'MdList' list wrapper"
        ),
        "the keys() leg must name the rest row's keys wrapper as the use, got: {msg}"
    );

    // (b) the array `* t` tail's own getter
    let msg = run(
        "inner = [q: uint, r: text]\n\
         inner_list = [+ inner]\n\
         holder = [x: uint, * inner]\n",
        "tail",
    );
    assert!(
        msg.contains(
            "an open array `* …` rest tail of the same element needs for its loose 'InnerList' \
             list wrapper"
        ),
        "the tail leg must name the rest tail as the use, got: {msg}"
    );
}

/// The DIRECT-claim leg of the loose `<Elem>List` family, over every plain use that mints the class.
/// Every other leg of this detector reaches its needs THROUGH a `[+ …]` shape, so a spec with no
/// `[+ …]` anywhere never consulted them — and the table-sourced member of that gap was fully
/// SILENT: `create_and_register_array_type`'s last-wins registration replaced the user's rule with
/// the keys-list, generation exited 0, and a field of the vanished type serialized as an array of
/// the key element. Each cell here is one claim source, and the messages must differ by source so a
/// failing spec points at the use that mints the class.
#[test]
fn loose_list_direct_claim_rejects_gracefully_per_source() {
    let run = |cddl: &str, tag: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_loose_list_direct_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, cddl).unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "loose_list_direct_unused",
            "--wasm=true",
        ]));
        std::fs::remove_file(&path).ok();
        let msg = result
            .expect_err("an incompatible direct claim on a loose list ident must be a graceful Err")
            .to_string();
        assert!(
            msg.contains("MdList") && msg.contains("loose list wrapper"),
            "{tag}: the message must name the claimed ident and the list family, got: {msg}"
        );
        msg
    };

    // (a) a named TABLE's keys() wrapper — the SILENT source. Both rule orders, because the swallow
    // and the leftover-incompatible-rule are opposite registration orders of one collision.
    for (tag, cddl) in [
        (
            "table",
            "md = [a: uint, b: uint]\n\
             md_list = [x: uint, y: text]\n\
             tbl = {* md => text}\n\
             holder = [m: md_list]\n",
        ),
        (
            "table_rule_last",
            "md = [a: uint, b: uint]\n\
             tbl = {* md => text}\n\
             md_list = [x: uint, y: text]\n\
             holder = [m: md_list]\n",
        ),
    ] {
        let msg = run(cddl, tag);
        assert!(
            msg.contains("a table keys() wrapper of the same element"),
            "{tag}: the table leg must name the keys() wrapper as the use, got: {msg}"
        );
    }

    // (b) an open struct-map rest row's keys() wrapper
    let msg = run(
        "md = [a: uint, b: uint]\n\
         md_list = [x: uint, y: text]\n\
         holder = { 1: uint, * md => text }\n\
         user = [m: md_list]\n",
        "restrow",
    );
    assert!(
        msg.contains("an open struct-map rest row's keys() wrapper of the same element"),
        "the rest-row leg must name the row's keys wrapper as the use, got: {msg}"
    );

    // (c) an open array `* …` rest tail
    let msg = run(
        "md = [a: uint, b: uint]\n\
         md_list = [x: uint, y: text]\n\
         holder = [1: uint, * md]\n\
         user = [m: md_list]\n",
        "tail",
    );
    assert!(
        msg.contains("an open array `* …` rest tail of the same element"),
        "the tail leg must name the rest tail as the use, got: {msg}"
    );

    // (d) a plain `*`-occurrence array use
    let msg = run(
        "md = [a: uint, b: uint]\n\
         md_list = [x: uint, y: text]\n\
         holder = { xs: [* md] }\n\
         user = [m: md_list]\n",
        "plain",
    );
    assert!(
        msg.contains("a plain (`*`-occurrence) array use of the same element"),
        "the plain-array leg must name the array use, got: {msg}"
    );
}

/// The COMPATIBLE half of the same mechanism, pinned so it stays deliberate rather than incidental:
/// a rule that IS `[* elem]` of the claimed ident's element is that very loose builder, so a table
/// keys-list of the same element ALIASES it — one class, shared — instead of colliding. This is why
/// the direct-claim leg asks `provides_compatible_loose_list` rather than rejecting every claim, and
/// why the last-wins re-mint is safe: what it overwrites is byte-identical to what it writes.
#[test]
fn compatible_authored_loose_list_rule_aliases_the_keys_list() {
    // `md_list = [* md]` names the class the table's keys() wrapper mints, with the same element.
    const CDDL: &str = "md = [a: uint, b: uint]\n\
                        md_list = [* md]\n\
                        tbl = {* md => text}\n\
                        holder = [m: md_list]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_loose_list_alias_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let files = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "loose_list_alias_unused",
        "--wasm=true",
    ]))
    .unwrap_or_else(|e| panic!("the compatible aliasing idiom must generate, got: {e}"));
    std::fs::remove_file(&path).ok();
    let find = |suffix: &str| -> String {
        files
            .iter()
            .find(|(p, _)| p.ends_with(suffix))
            .map(|(_, c)| c.clone())
            .unwrap_or_else(|| panic!("expected a generated {suffix}"))
    };

    // ONE transparent rust alias — the authored rule and the keys-list are the same definition, not
    // two (which is what the duplicate-ident backstop would have caught).
    let rust = find("rust/src/generated/mod.rs");
    assert_eq!(
        rust.matches("pub type MdList = Vec<Md>;").count(),
        1,
        "the authored `[* md]` rule and the table's keys-list must be one alias, got:\n{rust}"
    );
    // the authored rule's own NAME survives at the member position that declared it
    assert!(
        rust.contains("pub m: MdList,"),
        "the authored rule's declared spelling must survive at its use site, got:\n{rust}"
    );
    // ONE wasm class, and the table's keys() returns exactly it
    let wasm = find("wasm/src/generated/mod.rs");
    assert_eq!(
        wasm.matches("pub struct MdList(").count(),
        1,
        "the shared loose wrapper must be defined once, got:\n{wasm}"
    );
    assert!(
        wasm.contains("pub fn keys(&self) -> MdList"),
        "the table's keys() must return the shared class, got:\n{wasm}"
    );
}

/// The map-side sibling of the direct-claim leg: a plain `{* k => v}` use or table rule mints the
/// loose `MapKToV` its wasm boundary returns, so a user rule spelling that ident shadows it. These
/// reached only the generic duplicate-ident backstop, which is loud but reports the ident rather
/// than the claim — the asymmetry the DEFAULT rest-row leg had already closed for rows.
#[test]
fn loose_map_direct_claim_rejects_gracefully_per_source() {
    let run = |cddl: &str, tag: &str, ident: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_loose_map_direct_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, cddl).unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "loose_map_direct_unused",
            "--wasm=true",
        ]));
        std::fs::remove_file(&path).ok();
        let msg = result
            .expect_err("an incompatible direct claim on a loose map ident must be a graceful Err")
            .to_string();
        assert!(
            msg.contains(ident) && msg.contains("loose map wrapper"),
            "{tag}: the message must name the claimed ident and the map family, got: {msg}"
        );
        assert!(
            !msg.contains("duplicate top-level ident"),
            "{tag}: the per-kind detector must fire BEFORE the generic backstop, got: {msg}"
        );
        msg
    };

    let msg = run(
        "md = [a: uint, b: uint]\n\
         map_md_to_text = [x: uint, y: text]\n\
         tbl = {* md => text}\n\
         holder = [m: map_md_to_text]\n",
        "table",
        "MapMdToText",
    );
    assert!(
        msg.contains("a plain (`*`-occurrence) table rule of the same key/value"),
        "the table leg must name the table rule as the use, got: {msg}"
    );

    let msg = run(
        "map_u64_to_text = [x: uint, y: text]\n\
         holder = { xs: {* uint => text} }\n\
         user = [m: map_u64_to_text]\n",
        "plain",
        "MapU64ToText",
    );
    assert!(
        msg.contains("a plain (`*`-occurrence) map use of the same key/value"),
        "the plain-map leg must name the map use, got: {msg}"
    );
}

/// The min-1 sibling of the above: an ANONYMOUS `@duplicates preserve` `{+ …}` table instance mints
/// `NonEmptyPairMapKToV`, and a user rule spelling that ident shadows it.
#[test]
fn preserve_pair_map_non_empty_wrapper_ident_collision_rejects_gracefully() {
    const CDDL: &str = "pnetbl<k, v> = { + k => v } ; @duplicates preserve\n\
                        non_empty_pair_map_u64_to_text = [x: uint]\n\
                        holder = [t: pnetbl<uint, tstr>, \
                        c: non_empty_pair_map_u64_to_text]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_pmap_ne_ident_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "pmap_ne_ident_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result.expect_err(
        "a user rule claiming the restricted pair-map wrapper ident must be a graceful Err",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("NonEmptyPairMapU64ToText") && msg.contains("NonEmptyPairMap wrapper"),
        "the message must name the claimed ident and the restricted pair-map twin, got: {msg}"
    );
}

/// A GENERIC table instance (`tbl<uint, tstr>` -> the anonymous `TblU64Text`) generates cleanly under
/// `--wasm`. This was a PRE-EXISTING, policy-independent bug: `table_shape_sole_owners` recorded the
/// anonymous instance as the shape's sole owner, so `mint_sole_owner_table` minted `pub struct
/// TblU64Text` + `pub type MapU64ToText = TblU64Text;` WHILE the anonymous-instance passthrough minted
/// `pub type TblU64Text = MapU64ToText;` — a duplicate-ident collision on both names (the export.rs
/// backstop's "no user rule involved = cddl-codegen bug" arm). The fix excludes anonymous instances
/// from sole-ownership (mirroring the non-empty/reject owner lookups), so the instance routes PURELY
/// through the structural wrapper: one `pub struct MapU64ToText`, one `pub type TblU64Text = …` alias,
/// and NO `pub struct TblU64Text`.
#[test]
fn generic_table_instance_lowers_to_structural_wrapper_under_wasm() {
    const CDDL: &str = "tbl<k, v> = { * k => v }\n\
                        holder = [t: tbl<uint, tstr>]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_generic_tbl_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "generic_tbl_unused",
        "--wasm=true",
        "--preserve-encodings=true",
    ]))
    .expect("a generic table instance must GENERATE under --wasm (no duplicate-ident collision)");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub type TblU64Text = MapU64ToText;"),
        "the instance must route through the structural wrapper via a passthrough alias, got:\n{src}"
    );
    assert!(
        !src.contains("pub struct TblU64Text"),
        "the instance must NOT ALSO mint a rule-named struct (the double-mint collision), got:\n{src}"
    );
}

/// The generic-table wasm fix ALSO unblocks a generic `@duplicates preserve` table instance across the
/// wasm boundary: `ptbl<uint, tstr>` (with the directive on the generic base) lowers to the `PairMap`
/// twin on BOTH sides. Its anonymous structural wrapper takes the preserve flavor from the alias
/// base type's OWN carried policy (LOCAL information, never a crate-wide shape lookup), so the wasm
/// class is the flavored `PairMapU64ToText` wrapping `PairMap` — matching the rust
/// `pub type PtblU64Text = PairMap<u64, String>` (a keyed wrapper here would be a silently-broken
/// wasm crate). A scratch e2e (rust+wasm+json-gen cargo-check) backs this.
#[test]
fn generic_preserve_table_instance_lowers_to_pair_map_under_wasm() {
    const CDDL: &str = "ptbl<k, v> = { * k => v } ; @duplicates preserve\n\
                        holder = [t: ptbl<uint, tstr>]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_generic_ptbl_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "generic_ptbl_unused",
        "--wasm=true",
        "--preserve-encodings=true",
    ]))
    .expect("a generic preserve table instance must GENERATE under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // rust type is the PairMap twin
    assert!(
        src.contains("pub type PtblU64Text = PairMap<u64, String>;"),
        "the generic preserve instance's rust type must be the PairMap twin, got:\n{src}"
    );
    // the anonymous wasm structural wrapper is the FLAVORED class wrapping PairMap
    assert!(
        src.contains("pub struct PairMapU64ToText(pub(crate) PairMap<u64, String>)"),
        "the anonymous structural wasm wrapper must be PairMapU64ToText wrapping PairMap, got:\n{src}"
    );
    // and the instance's passthrough alias points at that flavored name
    assert!(
        src.contains("pub type PtblU64Text = PairMapU64ToText;"),
        "the instance's wasm passthrough alias must name the flavored wrapper, got:\n{src}"
    );
}

/// A map keyed by an EXPOSABLE-element generic-collection instance (`{ * gcoll<uint> => uint }` with
/// `gcoll<e0> = [* e0]`) lowers its wasm `keys()` list wrapper to the STRUCTURAL name (`ArrU64List`),
/// NOT the instance ident (`GcollU64List`). This was an E0425: the keys-list wrapper is minted from
/// the table's domain, which at `register_rust_struct` is still the unresolved `Rust(GcollU64)`
/// instance (naming it `GcollU64List`), but `finalize` then rewrites the exposable domain to bare
/// `Array(u64)` and the `keys()` accessor names the wrapper from THAT resolved form (`ArrU64List`) —
/// so the minted and referenced names diverged. The mint is now DEFERRED to
/// `finalize_deferred_table_keys_lists` (after the domain resolution), so both derive from the final
/// `Array(u64)` domain. A NON-exposable-element instance (`gcoll<foo>`) crosses by its `GcollFoo`
/// wrapper name at both sites and is unaffected — its keys-list stays `GcollFooList`.
#[test]
fn exposable_generic_collection_instance_keyed_map_lowers_keys_list_structurally_under_wasm() {
    const CDDL: &str = "gcoll<e0> = [* e0]\n\
                        holder = { * gcoll<uint> => uint }\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_gcoll_keys_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "gcoll_keys_unused",
        "--wasm=true",
    ]))
    .expect("a map keyed by an exposable generic-collection instance must generate under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // the keys-list wrapper AND the keys() accessor both name the structural `ArrU64List`
    assert!(
        src.contains("pub struct ArrU64List(pub(crate) Vec<Vec<u64>>)")
            && src.contains("pub fn keys(&self) -> ArrU64List"),
        "the keys-list wrapper and keys() accessor must both use the structural `ArrU64List`, got:\n{src}"
    );
    // the instance-ident name must appear NOWHERE (neither a stale mint nor a dangling reference)
    assert!(
        !src.contains("GcollU64List"),
        "the instance-ident keys-list name `GcollU64List` must not be minted or referenced (E0425), got:\n{src}"
    );
}

/// A recursive union used as a table's DOMAIN (`key_map = { * key_val => key_val }` with
/// `key_val = key_map / …`) mints its keys-list wasm wrapper from a domain that names the very
/// struct the table lowers to. The whole class once aborted in `register_rust_struct`'s keys-list
/// synthesis, and the seam is WASM-side (`name_as_wasm_array_ct`), which the
/// `recursive_collection_ref` integration fixture cannot reach — it runs `--wasm=false` under both
/// its profiles. So the collection-rooted spelling that fixture carries is pinned here for the wasm
/// pass: it generates, and the minted wrapper agrees with the accessor that references it.
///
/// The UNION-rooted spelling of the same three rules (`key_holder = [key_val]`) reaches the same
/// mint through the DEFERRED route instead (the table registers before its named domain exists);
/// it is pinned by `union_rooted_recursive_union_keyed_table_mints_its_keys_list`, and this test's
/// scope stays the collection-rooted ordering, which mints at registration time.
#[test]
fn recursive_union_keyed_table_mints_its_keys_list_under_wasm() {
    const CDDL: &str = "key_holder = [key_map]\n\
                        key_val = key_map / int / bytes / text\n\
                        key_map = { * key_val => key_val }\n";
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_reckey_{}.cddl", std::process::id()));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "reckey_unused",
        "--wasm=true",
    ]))
    .expect("a table keyed by a recursive union must generate under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // The keys-list wrapper is minted over the union's rust type, and `keys()` names the same class
    // — the mint/reference agreement that the deferred synthesis exists to keep.
    assert!(
        src.contains("pub struct KeyValList(pub(crate) Vec<cddl_lib::KeyVal>)")
            && src.contains("pub fn keys(&self) -> KeyValList"),
        "the keys-list wrapper and keys() accessor must both name `KeyValList`, got:\n{src}"
    );
}

/// The UNION-rooted ordering of the same recursive-union-keyed table (`u_holder = [u_val]` roots the
/// cycle at the UNION, so dep ordering registers `u_map` while `u_val` is still nothing) generates
/// too. `register_rust_struct` cannot name a keys-list wrapper from a domain naming an ident that is
/// in neither `rust_structs` nor `generic_instances` — `name_as_wasm_array_ct` → `is_enum` asserts —
/// so the mint is DEFERRED to `finalize_deferred_table_keys_lists`, exactly as it already is for a
/// generic-collection-instance domain, and names the wrapper from the (by then registered) union.
///
/// Both `--wasm` legs are exercised because the failing synthesis ran on the PARSE walk, so the
/// abort was never wasm-only; the mint/reference agreement is wasm-side and asserted there.
#[test]
fn union_rooted_recursive_union_keyed_table_mints_its_keys_list() {
    const CDDL: &str = "u_holder = [u_val]\n\
                        u_val = u_map / int / bytes / text\n\
                        u_map = { * u_val => u_val }\n";
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_urooted_{}.cddl", std::process::id()));
    std::fs::write(&path, CDDL).unwrap();
    let run = |wasm: &str| {
        crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "urooted_unused",
            wasm,
        ]))
    };
    let rust_only = run("--wasm=false")
        .expect("the union-rooted recursive-union-keyed table must generate under --wasm=false");
    let out = run("--wasm=true")
        .expect("the union-rooted recursive-union-keyed table must generate under --wasm");
    std::fs::remove_file(&path).ok();
    // the rust leg lowers the table itself, whichever ordering the cycle is rooted at
    let rust_src = rust_only.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        rust_src.contains("pub type UMap = BTreeMap<UVal, UVal>;"),
        "the union-rooted table must still lower to its map typedef, got:\n{rust_src}"
    );
    // and the wasm leg mints the keys-list the `keys()` accessor references (deferred mint)
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct UValList(pub(crate) Vec<cddl_lib::UVal>)")
            && src.contains("pub fn keys(&self) -> UValList"),
        "the keys-list wrapper and keys() accessor must both name `UValList`, got:\n{src}"
    );
}

/// The consumer-shaped spelling of the same class: a NAMED recursive union used as the domain of a
/// `@duplicates preserve` table, with the union carrying an array arm too (`md = md_map / [* md] /
/// int / bytes / text`) — the CIP-25/`transaction_metadata` shape, in the union-rooted ordering that
/// used to abort. Recursion × rule-position directive compose: the deferred keys-list mint does not
/// disturb the policy, so the table still lowers to the `PairMap` twin on both faces.
#[test]
fn union_rooted_recursive_preserve_table_keeps_its_pair_map() {
    const CDDL: &str = "md = md_map / [* md] / int / bytes / text\n\
                        md_map = { * md => md } ; @duplicates preserve\n";
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_mdpres_{}.cddl", std::process::id()));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "mdpres_unused",
        "--wasm=true",
    ]))
    .expect("a preserve-policy table keyed by a recursive union must generate under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // rust: the preserve twin, keyed and valued by the union
    assert!(
        src.contains("pub type MdMap = PairMap<Md, Md>;"),
        "the preserve policy must still select the PairMap twin, got:\n{src}"
    );
    // wasm: the pair-map wrapper class and the keys-list the accessor names
    assert!(
        src.contains("pub struct MdList(pub(crate) Vec<cddl_lib::Md>)")
            && src.contains("pub fn keys(&self) -> MdList"),
        "the keys-list wrapper and keys() accessor must both name `MdList`, got:\n{src}"
    );
}

/// `@duplicates reject` on an ANONYMOUS generic-set instance (`[g: oset<uint>]`) NOMINALIZES per
/// instantiation (Phase 2.3): `oset<uint>` mints one nominal `OsetU64` over the reject uniqueness
/// twin on BOTH sides — rust core `OsetU64(pub(crate) OrderedSet<u64>)` and a wasm class
/// `OsetU64(pub(crate) cddl_lib::OsetU64)` whose `new()`/`get()` boundary rides the structural
/// `U64OrderedSet` twin wrapper, NOT a loose `Vec<u64>`. The `tag_set_reject` corpus fixture
/// exercises the named-instance path plus a full `cargo check`; this pins the anonymous
/// instantiation path.
#[test]
fn duplicates_reject_inline_generic_instance_lowers_to_twin_under_wasm() {
    const CDDL: &str = "oset<a0> = #6.258([* a0]) / [* a0] ; @duplicates reject\n\
                        holder = [g: oset<uint>]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dup_inline_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let run = |wasm: &str| {
        crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "dup_inline_unused",
            wasm,
        ]))
    };

    // --wasm=true: generates cleanly (no rejection); the instance nominalizes to `OsetU64`, and its
    // wasm boundary still rides the structural `U64OrderedSet` uniqueness twin.
    let out = run("--wasm=true")
        .expect("@duplicates reject on an anonymous generic instance must generate under --wasm");
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct U64OrderedSet(pub(crate) OrderedSet<u64>)"),
        "the reject instance's wasm boundary must ride the OrderedSet twin wrapper, not a loose Vec, got:\n{src}"
    );
    assert!(
        src.contains("pub struct OsetU64(pub(crate) cddl_lib::OsetU64)"),
        "the anonymous instance nominalizes to a `OsetU64` wasm class over its rust nominal, got:\n{src}"
    );
    assert!(
        src.contains("pub struct OsetU64(pub(crate) OrderedSet<u64>)"),
        "the rust nominal `OsetU64` must wrap the OrderedSet twin, got:\n{src}"
    );
    assert!(
        !src.contains("pub struct U64List") && !src.contains("pub type OsetU64 = U64List;"),
        "reject must NOT lower to the loose Vec (`U64List`) wrapper, got:\n{src}"
    );

    // --wasm=false: the same shape generates cleanly (rust-only inline reject is supported too)
    let out = run("--wasm=false")
        .expect("the same inline reject shape must generate cleanly with --wasm=false");
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("OrderedSet<u64>"),
        "rust-only inline reject must still lower to OrderedSet, got:\n{src}"
    );
    std::fs::remove_file(&path).ok();
}

/// A `@newtype` wrapper over a PLAIN `[*]` `@duplicates reject` set (`holder = rs ; @newtype` with
/// `rs = [* uint] ; @duplicates reject`) must CONVERT its inner across the wasm boundary — the wasm
/// `new`/`get` cross as the restricted `U64OrderedSet` wrapper (`&U64OrderedSet` in, `U64OrderedSet`
/// out) and must `.clone().into()` it to/from the rust core `OrderedSet<u64>`, exactly as the
/// NonEmpty (`[+]`) reject set already does. The four wasm-boundary conversion helpers
/// (`to_wasm_boundary` / `to_wasm_boundary_optional` / `from_wasm_boundary_clone` /
/// `from_wasm_boundary_ref` on `RustType`) each special-cased `is_non_empty_array()` but omitted the
/// plain `is_reject_ordered_set()` arm, so a `[*]` reject set (a reject set that is NOT a non-empty
/// array) fell through and the newtype wrapper emitted `cddl_lib::Holder::new(inner)` (a `&Wrapper`
/// where the rust ctor wants `OrderedSet<u64>` by value) and `self.0.get().clone()` (an
/// `OrderedSet<u64>` where the getter returns `U64OrderedSet`) — E0308. The `[+]` twin never red
/// because `is_non_empty_array()` already routed it; only the `[*]` flavor exposed the gap. Its
/// sibling type-name methods (`for_wasm_member`/`for_wasm_param`/`directly_wasm_exposable`) already
/// treat both flavors identically, so the conversion helpers now do too. The `rset__newtype-inner`
/// wasm-ABI matrix cell is the per-role compile grid on top of this in-process pin.
#[test]
fn newtype_over_plain_reject_ordered_set_converts_wasm_boundary() {
    const CDDL: &str = "rs = [* uint] ; @duplicates reject\n\
                        holder = rs ; @newtype\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_newtype_reject_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "newtype_reject_unused",
        "--wasm=true",
    ]))
    .expect("a @newtype over a plain [*] reject set must generate under --wasm");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // the wasm ctor converts the `&U64OrderedSet` inner into the rust core `OrderedSet<u64>`
    assert!(
        src.contains("cddl_lib::Holder::new(inner.clone().into())"),
        "the wasm newtype ctor must `.clone().into()` the reject-set wrapper into the rust core, got:\n{src}"
    );
    // and the getter converts the rust core `&OrderedSet<u64>` back into the `U64OrderedSet` wrapper
    assert!(
        src.contains("self.0.get().clone().into()"),
        "the wasm newtype getter must `.clone().into()` the rust core back into the reject-set wrapper, got:\n{src}"
    );
    // the un-converted (E0308) forms must appear NOWHERE
    assert!(
        !src.contains("cddl_lib::Holder::new(inner))"),
        "the wasm ctor must NOT pass the `&U64OrderedSet` wrapper directly (E0308), got:\n{src}"
    );
}

/// Gap 1 (parse-side): a single-arm mandatory-tag `#6.258([* a]) ; @newtype` wrapper picks up the
/// well-known-tag registry's set-semantics default (reject) exactly as the plain single-arm array
/// rule does — its inner is the `OrderedSet` uniqueness twin, never a plain `Vec`. Before the fix the
/// `@newtype` branch passed raw `rule_metadata` (never consulting `single_arm_array_effective_metadata`),
/// so the registry default was silently dropped and the inner stayed `Vec<u64>`.
#[test]
fn single_arm_258_newtype_defaults_to_reject_ordered_set() {
    const CDDL: &str = "foo = #6.258([* uint]) ; @newtype\n\
                        holder = [f: foo]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_single_arm_258_newtype_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "single_arm_258_newtype_unused",
    ]))
    .expect("a single-arm 258 @newtype rule must generate cleanly");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("pub struct Foo(pub(crate) OrderedSet<u64>)"),
        "a single-arm 258 @newtype wrapper must default to the OrderedSet twin (registry set semantics), got:\n{src}"
    );
    assert!(
        !src.contains("pub struct Foo(pub(crate) Vec<u64>)"),
        "the plain-Vec wrapper (silently-dropped registry default) must NOT appear, got:\n{src}"
    );
}

/// Gap 2 (generation-side): an explicit `[* a] ; @newtype @duplicates reject` captures the directive
/// in the wrapper's struct config, but the wrapped inner type must ALSO become the `OrderedSet`
/// uniqueness twin. Before the fix the directive was captured yet never consumed at the wrapper seam:
/// the inner stayed `Vec<u64>` and the generated code contained no `try_from`/`DuplicateKey` door at
/// all. The `[+]` non-empty flavor selects `NonEmptyOrderedSet`.
#[test]
fn newtype_plain_reject_selects_ordered_set_inner() {
    for (occ, twin) in [("*", "OrderedSet<u64>"), ("+", "NonEmptyOrderedSet<u64>")] {
        let cddl = format!("foo = [{occ} uint] ; @newtype @duplicates reject\nholder = [f: foo]\n");
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_newtype_plain_reject_{}_{}.cddl",
            std::process::id(),
            occ
        ));
        std::fs::write(&path, &cddl).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "newtype_plain_reject_unused",
        ]))
        .unwrap_or_else(|e| {
            panic!("a @newtype @duplicates reject rule must generate cleanly: {e}")
        });
        std::fs::remove_file(&path).ok();
        let src = out.values().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            src.contains(&format!("pub struct Foo(pub(crate) {twin})")),
            "a @newtype @duplicates reject `[{occ}]` wrapper must wrap {twin}, got:\n{src}"
        );
        assert!(
            !src.contains("pub struct Foo(pub(crate) Vec<u64>)")
                && !src.contains("pub struct Foo(pub(crate) NonEmptyVec<u64>)"),
            "the duplicate-permitting Vec inner (dropped directive) must NOT appear for `[{occ}]`, got:\n{src}"
        );
    }
}

/// Gap 3 (dispatch-side): `@newtype` on the collapsed two-arm 258 idiom
/// (`#6.258([* a]) / [* a] ; @newtype`) is hard-rejected for this phase. The structural collapse turns
/// Phase 2.2 SUBSUMES the Phase 2.1 gap-3 rejection: the two-arm 258 idiom now nominalizes into a
/// wrapper struct that HAS somewhere to carry a getter, so `@newtype` is accepted rather than rejected.
/// A BARE `@newtype` on a set nominal emits NO inherent getter (a 0-arg `get()` would shadow
/// `OrderedSet::get(index)` through `Deref` — E0061), so it is a no-op ergonomically; a
/// `@newtype <name>` custom getter IS emitted (a custom name doesn't shadow). Both must GENERATE
/// cleanly (no rejection, no silent drop).
#[test]
fn newtype_on_two_arm_258_idiom_is_accepted_and_nominalizes() {
    let gen_src = |cddl: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_newtype_two_arm_258_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "newtype_two_arm_258_unused",
            "--wasm=false",
        ]))
        .unwrap_or_else(|e| {
            panic!("@newtype on the collapsed two-arm 258 idiom must generate cleanly, got: {e}")
        });
        std::fs::remove_file(&path).ok();
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // bare `@newtype`: nominalizes, no inherent getter (would shadow OrderedSet::get(index)).
    let bare = gen_src("foo = #6.258([* uint]) / [* uint] ; @newtype\nholder = [f: foo]\n");
    assert!(
        bare.contains("pub struct Foo")
            && bare.contains("OrderedSet<u64>")
            && !bare.contains("pub type Foo =")
            && !bare.contains("pub fn get("),
        "bare @newtype on the idiom nominalizes with NO inherent get():\n{bare}"
    );

    // `@newtype <name>`: the custom getter is emitted (no shadowing on a custom name).
    let named =
        gen_src("foo = #6.258([* uint]) / [* uint] ; @newtype entries\nholder = [f: foo]\n");
    assert!(
        named.contains("pub struct Foo") && named.contains("pub fn entries("),
        "a custom `@newtype <name>` getter must be emitted on the set nominal:\n{named}"
    );
}

/// `@duplicates` on a `@newtype` TABLE (`{* k => v} ; @newtype @duplicates …`) NOMINALIZES on both
/// policies. `preserve` swaps the wrapper's inner to the `PairMap` vec-of-pairs twin and its wasm
/// boundary to the `PairMapKToV` structural class the wasm struct walk mints for exactly that inner;
/// `reject` is the table default, so the wrapper keeps the loose `BTreeMap` core and the `MapKToV`
/// boundary. Both directions are pinned because the policy is what selects the inner REPRESENTATION:
/// a silently-dropped `preserve` would emit a wrapper that collapses duplicate keys the spec says to
/// keep, and a `preserve` boundary naming the default class is the E0425 the wasm crate cannot build.
#[test]
fn newtype_table_duplicates_nominalizes_on_both_policies() {
    for (policy, rust_inner, wasm_inner) in [
        ("preserve", "PairMap<u64, String>", "PairMapU64ToText"),
        ("reject", "BTreeMap<u64, String>", "MapU64ToText"),
    ] {
        let cddl = format!(
            "foo = {{ * uint => text }} ; @newtype @duplicates {policy}\nholder = [f: foo]\n"
        );
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_newtype_table_dup_{}_{}.cddl",
            std::process::id(),
            policy
        ));
        std::fs::write(&path, &cddl).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "newtype_table_dup_unused",
            "--wasm=true",
        ]))
        .unwrap_or_else(|e| panic!("@newtype @duplicates {policy} table must generate: {e}"));
        std::fs::remove_file(&path).ok();
        let rust = out
            .get("rust/src/generated/mod.rs")
            .expect("rust mod.rs must be emitted");
        assert!(
            rust.contains(&format!("pub struct Foo(pub(crate) {rust_inner})")),
            "`@newtype @duplicates {policy}` must nominalize over `{rust_inner}`:\n{rust}"
        );
        let wasm = out
            .get("wasm/src/generated/mod.rs")
            .expect("wasm mod.rs must be emitted");
        assert!(
            wasm.contains(&format!("pub fn new(inner: &{wasm_inner})"))
                && wasm.contains(&format!("pub fn get(&self) -> {wasm_inner}"))
                && wasm.contains(&format!("pub struct {wasm_inner}(")),
            "the `{policy}` wrapper's wasm boundary must name — and the crate must MINT — \
             `{wasm_inner}`:\n{wasm}"
        );
    }
}

/// A `@duplicates reject` named `[+ elem]` rule must NOT capture an inline `[+ elem]` of the same
/// element for the wasm inline-dedup: inline occurrences are directive-less (always preserve), so
/// their rust member is `NonEmptyVec` while the reject rule's wrapper wraps `NonEmptyOrderedSet` —
/// capturing would name the inline surface after a wrapper of the wrong core type (a loud-but-broken
/// wasm crate, `From<NonEmptyVec>` missing on the reject wrapper). The inline surface must keep the
/// synthesized `NonEmpty<Elem>List` (preserve) wrapper, distinct from the reject rule's class.
#[test]
fn duplicates_reject_named_rule_does_not_capture_preserve_inline_nonempty() {
    const CDDL: &str = "nes = [+ uint] ; @duplicates reject\n\
                        holder = [a: nes, b: [+ uint]]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dup_capture_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "dup_capture_unused",
        "--wasm=true",
    ]))
    .expect(
        "reject-named + inline-preserve nonempty of the same element must generate under --wasm",
    );
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    // the inline `[+ uint]` keeps the preserve NonEmptyVec wrapper, NOT the reject class `Nes`
    assert!(
        src.contains("pub struct NonEmptyU64List(pub(crate) NonEmptyVec<u64>)"),
        "inline preserve `[+ uint]` must keep its own NonEmptyVec wrapper, got:\n{src}"
    );
    assert!(
        src.contains("pub struct Nes(pub(crate) NonEmptyOrderedSet<u64>)"),
        "the named reject rule must wrap NonEmptyOrderedSet, got:\n{src}"
    );
    // holder.b (inline) must be typed as the preserve wrapper, never captured onto the reject class
    assert!(
        src.contains("pub fn b(&self) -> NonEmptyU64List"),
        "the inline field getter must return the preserve wrapper, not the reject class, got:\n{src}"
    );
}

/// The `@duplicates reject` uniqueness-twin wasm wrapper is the THIRD container kind's collision
/// detector (sibling of the two NonEmptyVec/NonEmptyMap detectors). An inline (anonymous instance)
/// reject set synthesizes a `<Elem>OrderedSet` wasm class; a user rule claiming that ident must be
/// caught as a GRACEFUL rejection (not a panic, not a silent-broken crate), with a message naming
/// the reject twin (distinct from the NonEmpty siblings' wording).
#[test]
fn duplicates_reject_structural_wrapper_name_collision_rejects_gracefully() {
    const CDDL: &str = "oset<a0> = #6.258([* a0]) / [* a0] ; @duplicates reject\n\
                        u64_ordered_set = uint\n\
                        holder = [g: oset<uint>, h: u64_ordered_set]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dup_reject_collide_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "dup_reject_collide_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let err = result.expect_err(
        "a user rule colliding with the synthesized reject wrapper name must be a graceful Err",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("U64OrderedSet") && msg.contains("OrderedSet wrapper"),
        "the collision message must name the reject twin (distinct from NonEmptyVec/Map), got: {msg}"
    );
}

/// `@duplicates` at a field/member position is per-rule-only, so it is a graceful placement
/// rejection with its own field-specific wording (not the rule-level "only applies to …").
#[test]
fn duplicates_directive_on_field_rejects_gracefully() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dup_field_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, "s = [\n  x: uint, ; @duplicates reject\n]\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "dup_field_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err = result
        .expect_err("@duplicates on a field must be a graceful Err, not Ok (and not a panic)");
    let msg = err.to_string();
    assert!(
        msg.contains("@duplicates") && msg.contains("per-rule"),
        "field rejection should name the directive and its per-rule nature, got: {msg}"
    );
}

/// Helper for the CDDL-module-directive and dotted-ident vectors: write `spec` to a temp file, run
/// the pipeline, and return the `Result` so the caller can assert success or inspect the `Err`.
fn run_spec(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
    let path = std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
    std::fs::write(&path, spec).unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "directive_scan_unused",
        "--wasm",
        "false",
    ]);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// A `;#####` banner comment is legal basic CDDL (`;#` not followed by a space is not a directive
/// per the modules-draft ABNF), so generation must PROCEED normally — no abort, no directive
/// handling. Guards that the directive scan is scoped strictly to the ABNF and does not blanket-
/// reject every `;#` line.
#[test]
fn module_directive_banner_comment_generates() {
    run_spec(";#####  section banner  #####\nfoo = [x: uint]\n", "banner")
        .expect("a `;#####` banner comment is a plain comment and must not abort generation");
}

/// A `;# import <module>` directive is a CDDL-module preprocessor directive cddl-codegen does not
/// support; it is a HARD abort (not a silent ignore, which would yield a misleading undefined-
/// reference error), with a message naming the directive.
#[test]
fn module_directive_import_aborts() {
    let err = run_spec(";# import foo\nstart = [x: uint]\n", "import")
        .expect_err("a `;# import` module directive must abort generation, not be ignored");
    assert!(
        err.contains("module directive") && err.contains("import"),
        "abort message must name the CDDL module directive and the `import` keyword, got: {err}"
    );
}

/// The `include … from …` directive form aborts identically (`;# include a, b from foo`).
#[test]
fn module_directive_include_aborts() {
    let err = run_spec(";# include a, b from foo\nstart = [x: uint]\n", "include")
        .expect_err("a `;# include` module directive must abort generation, not be ignored");
    assert!(
        err.contains("module directive") && err.contains("include"),
        "abort message must name the CDDL module directive and the `include` keyword, got: {err}"
    );
}

/// A `;# `-prefixed line whose first token is neither `import` nor `include` is an unrecognized
/// directive-shaped comment: it WARNS (to stderr) but must NOT abort — generation proceeds.
#[test]
fn module_directive_nondirective_warns_not_aborts() {
    run_spec(
        ";# something-nondirective here\nfoo = [x: uint]\n",
        "nondirective",
    )
    .expect("an unrecognized `;# …` directive-shaped comment must warn, not abort generation");
}

/// A rule whose name contains `.` (e.g. from cddlc `as`-namespacing, `cose.label`) is rejected
/// GRACEFULLY at the reserved-name pre-scan seam — never flowed through `convert_to_camel_case`
/// into an invalid-Rust crate. The message names the offending dotted ident by source spelling.
#[test]
fn dotted_rule_name_rejects_gracefully() {
    let err = run_spec("cose.label = int\n", "dotted")
        .expect_err("a dotted rule name must reject gracefully, not generate invalid Rust");
    assert!(
        err.contains("cose.label"),
        "rejection must name the offending dotted ident by source spelling, got: {err}"
    );
    assert!(
        err.contains('.') && err.contains("does not support"),
        "rejection must explain that dotted rule names are unsupported, got: {err}"
    );
}

/// An unsupported `type2` construct as a rule body (`foo = #1.2`, a bare major-type constraint;
/// also `~name` unwrap, `&group`, `&( ... )`, `#`) is rejected BY DESIGN — via a GRACEFUL `Err`
/// (deferred through `record_rejection` → drained by `finalize`), never a `panic!`. This pins that
/// the message names the offending rule (by SOURCE spelling `foo`, not the camel-cased `Foo`) and
/// the construct. The moved matrix_reject fixtures (`type2.major`, `type2.unwrap`, …) pin the
/// OUTCOME category; this pins the message.
#[test]
fn unsupported_type2_rule_body_rejects_gracefully() {
    let path = std::env::temp_dir().join(format!("cddl_codegen_major_{}.cddl", std::process::id()));
    std::fs::write(&path, "foo = #1.2\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "major_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err = result.expect_err("unsupported type2 rule body must be a graceful Err, not a panic");
    let msg = err.to_string();
    // Names the rule by its SOURCE spelling, not the camel-cased RustIdent.
    assert!(
        msg.contains("rule `foo`"),
        "rejection message should name the offending rule by source spelling, got: {msg}"
    );
    // Names the construct (a major-type constraint).
    assert!(
        msg.contains("major-type"),
        "rejection message should name the unsupported construct, got: {msg}"
    );
}

/// An `~name` unwrap as a rule body carries an actionable hint (inline the referenced rule's
/// definition manually), on top of the rule name + construct — this pins that honest remedy.
#[test]
fn unsupported_unwrap_rule_body_names_remedy() {
    let path =
        std::env::temp_dir().join(format!("cddl_codegen_unwrap_{}.cddl", std::process::id()));
    std::fs::write(&path, "inner = [uint]\nfoo = ~inner\n").unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "unwrap_unused",
    ]);
    let result = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();

    let err = result.expect_err("unsupported unwrap rule body must be a graceful Err, not a panic");
    let msg = err.to_string();
    assert!(
        msg.contains("rule `foo`") && msg.contains("unwrap"),
        "rejection message should name the rule and the unwrap construct, got: {msg}"
    );
    assert!(
        msg.contains("inline the referenced rule"),
        "unwrap rejection should carry the inline-manually remedy, got: {msg}"
    );
}

/// A two-arm `T / null` choice whose non-`null` arm is a bare fixed value is unsupported at BOTH
/// collapse sites — rejected BY DESIGN via a GRACEFUL `Err`, never the `for_rust_member_ct` abort
/// the shape used to hit under every profile. The catalog above only records the
/// `error (graceful)` LABEL for the two committed fixtures; this pins what each message actually
/// SAYS: the rule-level one names the rule by its SOURCE spelling and quotes the offending value
/// back in CDDL form, the member-level one uses role-generic wording (no rule name exists there),
/// and both carry the shared explanation plus a remedy that was probed to generate.
///
/// Every fixed kind the collapse can carry is swept, not just bool — the guard keys on
/// "is a fixed value", not on any one variant — including the degenerate `null / null`, which gets
/// the distinct sentence it needs (there is no non-`null` arm to widen).
#[test]
fn fixed_inner_null_collapse_rejects_gracefully_at_both_sites() {
    fn run(spec: &str, tag: &str) -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_nullcollapse_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "nullcollapse_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result
            .expect_err(&format!(
                "a fixed inner under the `T / null` collapse ({tag}) must be a graceful Err, not a panic"
            ))
            .to_string()
    }

    // Rule-level site: names the rule by its SOURCE spelling (`t`, not the camel-cased `T`).
    let rule = run("a = [x: uint]\nt = true / null\n", "rule");
    assert!(
        rule.contains("rule `t`: the two-arm choice `true / null` is unsupported"),
        "rule-level rejection should name the rule and quote the choice back, got: {rule}"
    );
    assert!(
        rule.contains("collapses to an `Option<T>` rather than an enum"),
        "rule-level rejection should explain the collapse, got: {rule}"
    );
    assert!(
        rule.contains("`bool / null` lowers to `Option<bool>`")
            && rule.contains("different spec, not an equivalent one"),
        "rule-level rejection should carry the probed remedy and its honesty caveat, got: {rule}"
    );

    // Member-level site: role-generic wording, no rule name available.
    let member = run("a = [v: true / null, x: uint]\n", "member");
    assert!(
        member.contains(
            "a two-arm `true / null` choice used as a member or element type is unsupported"
        ),
        "member-level rejection should use role-generic wording, got: {member}"
    );
    assert!(
        member.contains("collapses to an `Option<T>` rather than an enum")
            && member.contains("`bool / null` lowers to `Option<bool>`"),
        "member-level rejection should share the rule-level explanation and remedy, got: {member}"
    );

    // The guard keys on fixed-ness, not on bool: every other fixed kind refuses the same way, with
    // the value quoted back in its CDDL spelling.
    for (spec, tag, quoted) in [
        (
            "a = [x: uint]\nt = false / null\n",
            "false",
            "`false / null`",
        ),
        ("a = [x: uint]\nt = 5 / null\n", "uint", "`5 / null`"),
        ("a = [x: uint]\nt = -1 / null\n", "nint", "`-1 / null`"),
        ("a = [x: uint]\nt = 3.0 / null\n", "float", "`3.0 / null`"),
        (
            "a = [x: uint]\nt = \"v1\" / null\n",
            "text",
            "`\"v1\" / null`",
        ),
    ] {
        let msg = run(spec, tag);
        assert!(
            msg.contains(quoted),
            "the {tag} rejection should quote {quoted} back in CDDL form, got: {msg}"
        );
    }

    // `null / null` is the degenerate spelling: no non-`null` arm exists, so the widening remedy
    // would be dishonest and the message must NOT offer it.
    let both_null = run("a = [x: uint]\nt = null / null\n", "nullnull");
    assert!(
        both_null.contains("`null / null`") && both_null.contains("`null` on both arms"),
        "the null/null rejection should say both arms are null, got: {both_null}"
    );
    assert!(
        !both_null.contains("Widening the fixed arm"),
        "the null/null rejection must not offer the widening remedy, got: {both_null}"
    );

    // Control: the SAME two-arm shape over a non-fixed inner still collapses to a real `Option<T>`,
    // and a two-arm choice with no `null` arm still takes the enum path — the guard must not widen
    // into either.
    for supported in [
        "a = [x: uint]\nt = bool / null\n",
        "a = [x: uint]\nt = uint / null\n",
        "a = [x: uint]\nt = null / tstr\n",
        "a = [x: uint]\nt = true / false\n",
        "a = [v: bool / null, x: uint]\n",
    ] {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_nullcollapse_ok_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, supported).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "nullcollapse_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "`{}` must still generate, got: {:?}",
            supported.trim(),
            result.err()
        );
    }
}

/// A choice arm whose variant name is DERIVED from a fixed value's LEXEME (`1.5` → `F1.5`, `-1` →
/// `U-1`) is unspellable as a Rust identifier. Rejected BY DESIGN via a GRACEFUL `Err` naming the
/// rule, the arm and the `@name` remedy — previously the invalid name went out to rustfmt, which
/// failed with an error about its own confusion (`expected item, found 5`) and named neither the
/// arm nor a way out.
///
/// Swept across BOTH naming consumers, because they are different code paths onto the same minter:
/// the type-choice one (`create_variants_from_type_choices`, reached by a bare rule-level choice,
/// an all-fixed c-style enum and a nested ANONYMOUS choice) and the group-choice arm loop's
/// BARE-member fallback (`[ true // 1.5 ]`, where no member key exists to name the variant after).
/// The positive controls pin that the predicate is on the minted STRING, not on the value's kind:
/// fixed uint/text arms and any `@name`d arm keep generating.
#[test]
fn lexeme_derived_arm_variant_name_rejects_gracefully_at_both_naming_sites() {
    fn run(spec: &str, tag: &str) -> Result<(), String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_armname_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "armname_unused",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result.map(|_| ()).map_err(|e| e.to_string())
    }

    // Type-choice consumer: rule-level bare choice, naming the rule by its SOURCE spelling (`t`,
    // not the camel-cased `T`), the arm as written, the minted name and the `@name` remedy.
    let float_arm = run("t = 1.5 / tstr\n", "float").expect_err(
        "a fixed float arm minting an invalid variant name must be a graceful Err, not a rustfmt failure",
    );
    assert!(
        float_arm.contains("rule `t`: its arm `1.5` generates the variant name `F1.5`"),
        "rejection should name the rule, the arm and the minted name, got: {float_arm}"
    );
    assert!(
        float_arm.contains("is not a valid Rust identifier"),
        "rejection should say why the minted name is refused, got: {float_arm}"
    );
    assert!(
        float_arm.contains("Name the arm with `; @name <new_name>`"),
        "rejection should name the `@name` remedy, got: {float_arm}"
    );

    // The nint kind mints from the same lexeme path (`U-1` — the `U`-for-nint prefix is existing
    // naming behaviour, not part of what this rejects).
    let nint_arm = run("t = -1 / null / tstr\n", "nint")
        .expect_err("a fixed nint arm must reject gracefully too");
    assert!(
        nint_arm.contains("rule `t`: its arm `-1` generates the variant name `U-1`")
            && nint_arm.contains("`; @name <new_name>`"),
        "the nint rejection should share the shape of the float one, got: {nint_arm}"
    );

    // The all-fixed c-style-enum spelling consumes the same minted variants, so it refuses at the
    // same seam — once per offending arm.
    let c_enum =
        run("t = 1.5 / 2.5\n", "cenum").expect_err("an all-fixed float c-style enum must reject");
    assert!(
        c_enum.contains("its arm `1.5` generates the variant name `F1.5`")
            && c_enum.contains("its arm `2.5` generates the variant name `F2.5`"),
        "the c-style-enum rejection should name every offending arm, got: {c_enum}"
    );

    // The KEYWORD half of the predicate: a fixed TEXT arm camel-cases straight through, so `"self"`
    // mints `Self` — lexically an identifier, but a Rust keyword, and the emitter never raw-escapes
    // (`r#Self`). It died at the same rustfmt seam ("expected identifier, found keyword `Self`") and
    // must refuse with the same message. Both the mixed and the group-choice spellings.
    let keyword_arm = run("t = \"self\" / \"x\" / uint\n", "keyword")
        .expect_err("a fixed text arm minting a Rust keyword must reject gracefully");
    assert!(
        keyword_arm.contains("rule `t`: its arm `\"self\"` generates the variant name `Self`")
            && keyword_arm.contains("is not a valid Rust identifier")
            && keyword_arm.contains("`; @name <new_name>`"),
        "the keyword rejection should share the lexeme rejections' shape, got: {keyword_arm}"
    );
    let keyword_group_arm = run("t = [ true // \"self\" ]\n", "keyword_grparm")
        .expect_err("a bare keyword-minting group-choice member must reject gracefully");
    assert!(
        keyword_group_arm.contains("generates the variant name `Self`"),
        "the group-choice consumer should refuse the keyword too, got: {keyword_group_arm}"
    );

    // Nested ANONYMOUS choice: no rule owns the arms, so the wording is role-generic. This spelling
    // also mints the enclosing RULE ident from the same lexeme (`F1.5OrText`), which is why the
    // rejection has to land before generation rather than at the variant alone.
    let nested = run("x = [1.5 / tstr]\n", "nested")
        .expect_err("a nested anonymous choice with a fixed float arm must reject");
    assert!(
        nested.contains("an inline type choice: its arm `1.5` generates the variant name `F1.5`"),
        "the nested-anonymous rejection should use role-generic wording, got: {nested}"
    );

    // Group-choice consumer: a BARE member has no key to name the variant after, so the name comes
    // from the member's TYPE through the same minter.
    let bare_group_arm = run("t = [ true // 1.5 ]\n", "grparm")
        .expect_err("a bare fixed-float group-choice member must reject gracefully");
    assert!(
        bare_group_arm.contains("rule `t`: its arm `1.5` generates the variant name `F1.5`")
            && bare_group_arm.contains("`; @name <new_name>`"),
        "the group-choice rejection should share the type-choice wording, got: {bare_group_arm}"
    );

    // Positive controls. Every kind whose minted name IS an identifier still generates, the
    // documented `@name` route still overrides the derived name, and the NAMED-member group-choice
    // spelling (whose variant comes from the member key, not the lexeme) is untouched.
    for (supported, tag) in [
        ("t = 5 / null / tstr\n", "uint_ok"),
        ("t = \"x\" / null / tstr\n", "text_ok"),
        // A text arm whose camel-cased lexeme is merely CAPITALIZED, not reserved.
        ("t = \"type\" / null / tstr\n", "text_nonkeyword_ok"),
        ("t = \"self\" ; @name mine\n  / uint\n", "named_keyword_ok"),
        ("t = 1.5 ; @name half\n  / tstr\n", "named_float_ok"),
        ("t = -1 ; @name neg_one\n  / null / tstr\n", "named_nint_ok"),
        ("t = [ true // v: 1.5 ]\n", "named_group_member_ok"),
    ] {
        assert!(
            run(supported, tag).is_ok(),
            "`{}` must still generate, got: {:?}",
            supported.trim(),
            run(supported, tag).err()
        );
    }
}

/// The `.within` / `.and` control operators are unsupported — rejected BY DESIGN via a GRACEFUL
/// `Err`, never `todo!()`. Follows the `.size`-on-`int` sibling in `parse_control_operator`
/// (`record_rejection` + an inert full-range placeholder, drained by `finalize`), including its
/// `reject_rule_prefix` rule naming. Pins the message names the rule and the offending
/// operator spelling. (The `.cbor-seq` third member of the same arm is unreachable — the cddl
/// parser rejects it at parse/lex — so no red fixture is constructible for it.)
#[test]
fn unsupported_control_operator_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_ctlop_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "ctlop_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    let within = run("x = uint .within int\n", "within")
        .expect_err("`.within` must be a graceful Err, not a todo!() panic");
    assert!(
        within.contains("rule `X`") && within.contains(".within"),
        "`.within` rejection should name the rule and the operator, got: {within}"
    );

    let and = run("x = uint .and (0..9)\n", "and")
        .expect_err("`.and` must be a graceful Err, not a todo!() panic");
    assert!(
        and.contains("rule `X`") && and.contains(".and"),
        "`.and` rejection should name the rule and the operator, got: {and}"
    );
}

/// A ZERO-permitting occurrence (`*` / `0*n` / `*n`) on a keyed struct-map field means the entry
/// may be ABSENT (RFC 8610) — silently narrowing it to a mandatory field generates a decoder that
/// rejects valid CBOR, invisible to round-trip tests (only cross-producer data exposes it). This
/// pins the graceful rejection AND the boundary: `+` (lower bound >= 1) must keep generating a
/// mandatory field, because under unique map keys "one or more" collapses to exactly-one — that is
/// honored semantics, not narrowing. The projected matrix reject fixtures pin the outcome category;
/// this pins the message and the `+` boundary.
#[test]
fn zero_permitting_occurrence_on_keyed_map_field_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_zero_occur_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "zero_occur_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    let msg = run("m = { * t: uint }\n", "star").expect_err(
        "`*` on a keyed map field must be a graceful Err (silent narrowing to mandatory is wrong)",
    );
    assert!(
        msg.contains("zero-permitting occurrence") && msg.contains("rule `m`"),
        "rejection message should be actionable and name the rule, got: {msg}"
    );

    run("m = { 0*1 t: uint }\n", "bounded")
        .expect_err("`0*n` permits zero occurrences, so it must reject like `*`");

    // The boundary: `+` collapses to exactly-one under unique map keys, so a mandatory field IS
    // the honored semantics — it must keep generating.
    run("m = { + t: uint }\n", "plus")
        .expect("`+` (lower bound >= 1) must still generate a mandatory field");
}

/// An occurrence marker on a heterogeneous ARRAY-record field — `[uint, tstr, * bytes]` — was
/// silently narrowed to a mandatory exactly-once field, generating a decoder that rejects
/// spec-valid CBOR with any other repetition count (invisible to round-trip tests; surfaced by
/// spec-derived decode vectors). Unlike the keyed-map case above, `+` does NOT collapse in an
/// array (repetitions are real items), so EVERY count-permitting marker must reject. This pins
/// the graceful rejection AND the boundaries the guard must preserve:
///   - `*` / `+` / `2*3`, in any position → Err (the marker admits repetition counts the
///     narrowed field cannot decode);
///   - `1*1` → Ok (exactly-once IS the semantics — same boundary the inline-group guard pins);
///   - `?` → Ok (the supported optional-field path);
///   - `[* bytes]` alone → Ok (single-entry groups take the homogeneous Vec path, not the record
///     path — no narrowing happens there).
#[test]
fn occurrence_on_array_record_field_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_array_occur_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        // `--wasm=false`: the open-array CAPTURE wasm surface (the tail accessor) is a later work
        // package, gated behind a front-door rejection under `--wasm` (which defaults on). This test
        // exercises the RECOGNITION/narrowing boundary in plain mode — the wasm-gate polarity lives in
        // the dedicated open-array front-end fixture.
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "array_occur_unused",
            "--wasm=false",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // A FINAL-position `*` after ≥1 fixed member is now recognized as an open-array rest tail
    // (captured `Vec<T>`), not narrowed — it must GENERATE. (Phase D — open arrays.)
    run("m = [uint, tstr, * bytes]\n", "star").expect(
        "a final-position `* t` after fixed members is an open-array rest tail (captured Vec) — must generate",
    );

    // `+` / `n*m` on a final tail entry stay rejected: only `*` (unbounded capture) is honored on a
    // rest tail (a `+` tail breaks the empty-tail ≡ closed-struct byte invariant).
    let plus = run("m = [uint, + bytes]\n", "plus")
        .expect_err("`+` on the final entry is not a supported rest-tail occurrence — must reject");
    assert!(
        plus.contains("rule `m`") && (plus.contains("rest tail") || plus.contains("`*`")),
        "the `+`-tail rejection should be actionable and name the rule, got: {plus}"
    );
    run("m = [uint, 2*3 bytes]\n", "bounded").expect_err(
        "`2*3` admits 2..=3 repetitions — not a supported rest-tail occurrence (must reject)",
    );
    // A leading/non-final `*` keeps rejecting (the rest tail must be the LAST member).
    let leading = run("m = [* bytes, uint]\n", "leading")
        .expect_err("a non-final `*` narrows identically — must reject (rest tail must be last)");
    assert!(
        leading.contains("rule `m`"),
        "the non-final `*` rejection should name the rule, got: {leading}"
    );

    run("m = [uint, 1*1 bytes]\n", "exactly_once")
        .expect("`1*1` is exactly-once — mandatory IS the honored semantics");
    run("m = [uint, ? bytes]\n", "optional")
        .expect("`?` is the supported optional-field path and must keep generating");
    run("m = [* bytes]\n", "homogeneous").expect(
        "a single-entry `[* bytes]` takes the homogeneous Vec path — no narrowing to guard",
    );
}

/// Open-array rest tail (`[a, * t]`) front-end: every recognition guard, the directive-slot
/// direction/trap fixtures, and the `@ignore`/`@duplicates`/preserve combination rejections. The
/// value-level happy path lives in the compiled e2e `open_array_e2e`; this pins the parse-time
/// boundary in plain mode (`--wasm=false`).
#[test]
fn open_array_front_end() {
    fn gen_flags(
        spec: &str,
        flags: &[&str],
    ) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_open_array_fe_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            path.to_str().unwrap().to_owned(),
            "--output".to_owned(),
            "open_array_fe_unused".to_owned(),
            "--wasm=false".to_owned(),
        ];
        args.extend(flags.iter().map(|s| s.to_string()));
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }
    fn run(spec: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        gen_flags(spec, &[])
    }
    let src = |out: &std::collections::BTreeMap<String, String>| {
        out.values().cloned().collect::<Vec<_>>().join("\n")
    };

    // --- positive: a final `* t` after ≥1 fixed member captures a `Vec<T>` tail ---
    let cap =
        run("a = [uint, tstr, * uint]\n").expect("final-position `* t` is an open-array rest tail");
    assert!(
        src(&cap).contains("pub rest: Vec<u64>"),
        "capture tail is a `Vec<T>` field named `rest`"
    );

    // --- @name renames the captured field (read from the ENTRY-trailing slot) ---
    let named = run("a = [\n  uint,\n  * uint ; @name extras\n]\n")
        .expect("@name on the tail entry renames the captured field");
    assert!(
        src(&named).contains("pub extras: Vec<u64>") && !src(&named).contains("pub rest:"),
        "@name on the tail renames `rest` -> `extras`"
    );

    // --- @ignore (entry-trailing slot) is HONORED: no field, a closed struct ---
    let ign = run("a = [\n  uint,\n  * any ; @ignore\n]\n")
        .expect("@ignore on the tail entry is honored");
    assert!(
        !src(&ign).contains("pub rest") && src(&ign).contains("struct A"),
        "an @ignore tail emits no field (closed struct)"
    );

    // --- slot direction: a RULE-level @ignore on an open-array rule is NOT stolen onto the tail —
    // it is a loud rule-position rejection (the tail's own entry slot is disjoint from the rule slot).
    let rule_ign = run("a = [uint, * any] ; @ignore\n").expect_err(
        "a rule-position @ignore on an open-array rule is rejected, not applied to the tail",
    );
    assert!(
        rule_ign.contains("@ignore") && rule_ign.contains("rule `a`"),
        "rule-position @ignore is a loud rejection naming the rule, got: {rule_ign}"
    );

    // --- marker-slot trap: a directive glued to the `*` marker's OWN comment slot is NOT honored ---
    // (`*  ; @name x` before the entry type). The tail stays a plain capture named `rest`.
    let marker = run("a = [\n  uint,\n  * ; @name x\n  uint\n]\n");
    if let Ok(out) = &marker {
        assert!(
            src(out).contains("pub rest:") && !src(out).contains("pub x:"),
            "a directive on the `*` marker slot is silently NOT honored (field stays `rest`)"
        );
    }

    // --- guards, each a graceful rejection ---
    // non-final `*`
    run("a = [* uint, tstr]\n").expect_err("a non-final `*` must reject (tail must be last)");
    // multiple count-permitting members
    run("a = [uint, * uint, * tstr]\n").expect_err("multiple `*` members must reject");
    // `+` / `n*m` on the final entry
    run("a = [uint, + uint]\n").expect_err("`+` is not a supported rest-tail occurrence");
    run("a = [uint, 2*3 uint]\n").expect_err("`n*m` is not a supported rest-tail occurrence");
    // a fixed-value tail element has no Rust representation (a `Vec<FixedValue>` is not a type), so it
    // is rejected before the homogeneous-array fixed-value panic class
    let fixed = run("a = [uint, * 5]\n").expect_err("a fixed-value tail element must reject");
    assert!(
        fixed.contains("fixed value") && fixed.contains("rule `a`"),
        "the fixed-value-tail rejection names the rule + cause, got: {fixed}"
    );
    run("a = [uint, * null]\n").expect_err("a `* null` tail must reject (fixed value)");
    // choice-arm placement
    run("a = [uint, * uint] // [tstr]\n")
        .expect_err("a rest tail in a group-choice arm must reject");
    // plain group placement (`g = (a, * t)`, embedded via `[g]`)
    run("a = [g]\ng = (uint, * uint)\n").expect_err("a rest tail inside a plain group must reject");

    // --- directive combination rejections ---
    run("a = [\n  uint,\n  * uint ; @ignore @name x\n]\n")
        .expect_err("@ignore + @name on the tail must reject (no field to name)");
    run("a = [\n  uint,\n  * uint ; @ignore @duplicates preserve\n]\n")
        .expect_err("@ignore + @duplicates on the tail must reject (no keys)");
    let dup = run("a = [\n  uint,\n  * uint ; @duplicates preserve\n]\n")
        .expect_err("@duplicates on an array tail must reject (no keys)");
    assert!(
        dup.contains("@duplicates") && dup.contains("no keys"),
        "the @duplicates-on-array rejection explains there are no keys, got: {dup}"
    );

    // --- profiles: CAPTURE under --preserve-encodings GENERATES (byte-exact per-element tail
    // encodings ride a positional `{field}_elem_encodings` sidecar); @ignore under preserve is a
    // PERMANENT graceful rejection (a deliberately-lossy tolerate-and-drop tail undermines a preserve
    // crate's byte-exact contract). ---
    gen_flags("a = [uint, * uint]\n", &["--preserve-encodings=true"])
        .expect("open-array capture generates under --preserve-encodings");
    let ign_pres = gen_flags(
        "a = [\n  uint,\n  * any ; @ignore\n]\n",
        &["--preserve-encodings=true"],
    )
    .expect_err("@ignore under --preserve-encodings rejects (byte-exact contract)");
    assert!(
        ign_pres.contains("@ignore") && ign_pres.contains("preserve-encodings"),
        "the @ignore-preserve rejection names the directive + profile, got: {ign_pres}"
    );
}

/// An occurrence marker on an inline (parenthesized) group — `[* (int, tstr)]`, `{ * (k: int) }` —
/// used to be silently dropped by `flatten_group_entries`, narrowing the group to exactly-once and
/// generating a decoder that rejects spec-valid CBOR with any other repetition count (invisible to
/// round-trip tests). This pins the graceful rejection AND every boundary the fix must preserve:
///   - array `* / + / ? / 2*5` on an inline group → Err (the marker admits ≠ 1 reps);
///   - array `1*1 (…)` → Ok (exactly-once IS the semantics, so flattening stays sound);
///   - map `{ * (k: int) }` / `{ ? (k: int, j: tstr) }` → Err (bypassed the f18d764 keyed-field fix
///     because the inline-group wrapper hid the occurrence);
///   - map `{ + (k: int) }` → Ok (under unique map keys `+` collapses to exactly-one → mandatory
///     is honored semantics, the f18d764 boundary);
///   - map `{ * (int => tstr) }` → Ok (a parenthesized table: flatten leaves the `*`, table
///     detection fires on the inner `k => v`);
///   - named `pair = (int, tstr)` + `a = [* pair]` → Ok (the workaround the message recommends).
#[test]
fn occurrence_marker_on_inline_group_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_inline_occur_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "inline_occur_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // Array side: every occurrence marker admitting ≠ 1 reps must reject, citing the rule + the
    // "inline group" hint so the message is actionable.
    for (spec, tag) in [
        ("a = [* (int, tstr)]\n", "arr_star"),
        ("a = [+ (int, tstr)]\n", "arr_plus"),
        ("a = [? (int, tstr)]\n", "arr_opt"),
        ("a = [2*5 (int, tstr)]\n", "arr_bounded"),
    ] {
        let msg =
            run(spec, tag).expect_err("an occurrence marker on an inline array group must reject");
        assert!(
            msg.contains("inline group") && msg.contains("rule `a`"),
            "rejection should name the rule and the inline group, got: {msg}"
        );
    }

    // Array boundary: `1*1` IS exactly-once, so flattening it away is sound — must still generate.
    run("a = [1*1 (int, tstr)]\n", "arr_exact_one")
        .expect("`1*1` is exactly-once, so the inline group must still flatten and generate");

    // Map side: the inline-group wrapper hid these from the f18d764 keyed-field occurrence fix.
    // The remedy must be map-appropriate: naming the group does NOT help here (a plain-group
    // reference inside a map record is itself unsupported — it hits the "map field has no key"
    // rejection), so the message must point at `?` / the table form, not the array workaround.
    let map_msg = run("a = { * (k: int) }\n", "map_star")
        .expect_err("`{ * (k: int) }` permits absence, so it must reject like a bare `* k: int`");
    assert!(
        map_msg.contains("table") && !map_msg.contains("[* pair]"),
        "map-side rejection must recommend a map remedy, not the array workaround, got: {map_msg}"
    );
    run("a = { ? (k: int, j: tstr) }\n", "map_opt").expect_err(
        "`{ ? (…) }` permits absence, so it must reject rather than narrow to mandatory",
    );

    // Map boundary: `+` collapses to exactly-one under unique map keys → mandatory is honored.
    run("a = { + (k: int) }\n", "map_plus")
        .expect("`+` on a map inline group collapses to exactly-one — must still generate");

    // The parenthesized table form must keep working (flatten drops the `*`, table detection fires).
    run("a = { * (int => tstr) }\n", "map_table")
        .expect("`{ * (int => tstr) }` is a parenthesized table and must still generate");

    // The recommended workaround must generate under DEFAULT (wasm) flags. `generated_strings` runs
    // with wasm on, and a plain group used SOLELY as a `*` array element must register + emit its
    // struct (the array/table element paths call `set_rep_if_plain_group`, mirroring the record
    // path) — otherwise `is_enum`/`for_rust_member` trip a `generic_instances` assert at generation.
    run("pair = (int, tstr)\na = [* pair]\n", "named_workaround")
        .expect("naming the group (`pair`) is the recommended workaround and must generate");
    // Sibling that hits the same plain-group-registration gap and must also generate: a
    // single-element group as a `*` array element.
    run("pair = (int)\na = [* pair]\n", "named_single_element")
        .expect("a single-element plain group as a `*` array element must generate");
    // A plain group as a table VALUE is the one neighbour that does NOT generate: an array element
    // can absorb a spliced group because the emitted length scales with the group's arity, but a
    // CBOR map entry holds exactly one item per slot and cannot. It is refused at both spellings —
    // see `plain_group_table_domain_rejects_gracefully_at_both_spellings`, which owns the message.
    run(
        "pair = (int, tstr)\na = { * int => pair }\n",
        "named_table_value",
    )
    .expect_err("a plain group as a table value must reject gracefully, not splice into the slot");
}

/// A bare plain group in a table's KEY or VALUE domain (`coords = (uint, uint)`,
/// `{ * uint => coords }`) is unsupported and rejected BY DESIGN via a GRACEFUL `Err`.
///
/// A CBOR map entry holds exactly one item per slot and a keyless group has no single-item form, so
/// the only emission available is splicing its members in flat — which contradicts the map's own
/// entry count. The NAMED spelling used to do exactly that at exit 0: one entry serialized to
/// `a2 01 07 08 02 09 0a`, which an interoperating decoder reads as the 2-entry map `{1: 7, 8: 2}`
/// plus trailing bytes, while the INLINE spelling reached a raw `unwrap` at generation. The refusal
/// replaces both, so one spelling can no longer pass what the other panics on and neither ships
/// bytes only this crate can read.
///
/// Pins the message (role, group source name, remedy), that both spellings and both roles refuse,
/// and — the part that keeps the refusal honest — that the remedy the message names actually
/// generates: an array-WRAPPED group carries `basic_override`, serializes as one nested item, and
/// must stay green in every profile.
#[test]
fn plain_group_table_domain_rejects_gracefully_at_both_spellings() {
    fn run(spec: &str, tag: &str, extra: &[&str]) -> Result<(), String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_grouptable_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "grouptable_unused",
        ];
        args.extend_from_slice(extra);
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result.map(|_| ()).map_err(|e| e.to_string())
    }

    const GROUP: &str = "coords = (uint, uint)\n";

    // Named spelling, VALUE role: names the rule by its SOURCE spelling, the entry, the role, the
    // group, and the array-wrapping remedy.
    let named_value = run(
        &format!("{GROUP}t = {{ * uint => coords }}\n"),
        "named_value",
        &[],
    )
    .expect_err("a plain group as a named table's VALUE domain must reject gracefully");
    assert!(
        named_value.contains(
            "rule `t`: the table entry `uint => coords` uses the bare plain group `coords` as its \
             VALUE domain"
        ),
        "rejection should name the rule, entry, group and role, got: {named_value}"
    );
    assert!(
        named_value.contains("a CBOR map entry holds exactly one item in each slot"),
        "rejection should explain why the splice is impossible, got: {named_value}"
    );
    assert!(
        named_value.contains("`{ * uint => [coords] }`"),
        "rejection should name the array-wrapping remedy for this entry, got: {named_value}"
    );

    // Named spelling, KEY role: same message with the role and the remedy side swapped.
    let named_key = run(
        &format!("{GROUP}t = {{ * coords => uint }}\n"),
        "named_key",
        &[],
    )
    .expect_err("a plain group as a named table's KEY domain must reject gracefully");
    assert!(
        named_key.contains("uses the bare plain group `coords` as its KEY domain")
            && named_key.contains("`{ * [coords] => uint }`"),
        "the KEY-role rejection should swap the role and the remedy side, got: {named_key}"
    );

    // Inline spelling — the one that used to panic on a raw unwrap at generation. Role-generic
    // site wording (`inline map`), same body.
    let inline_value = run(
        &format!("{GROUP}t = [{{ * uint => coords }}]\n"),
        "inline_value",
        &[],
    )
    .expect_err("a plain group as an INLINE table's VALUE domain must reject, not panic");
    assert!(
        inline_value.contains(
            "inline map: the table entry `uint => coords` uses the bare plain group `coords` as \
             its VALUE domain"
        ) && inline_value.contains("`{ * uint => [coords] }`"),
        "the inline rejection should share the named one's body, got: {inline_value}"
    );

    // A TAGGED domain: the remedy has to wrap the GROUP REFERENCE, leaving the tag outside
    // (`#6.5([coords])`), because the array is the group's single-item carrier and the tag wraps
    // that carrier — `[#6.5(coords)]` also generates but tags the wrong thing. Pinned on both
    // roles, since each builds its remedy from a different AST node.
    let tagged_value = run(
        &format!("{GROUP}t = {{ * uint => #6.5(coords) }}\n"),
        "tagged_value",
        &[],
    )
    .expect_err("a tagged bare plain group as a table VALUE domain must reject gracefully");
    assert!(
        tagged_value.contains("`{ * uint => #6.5([coords]) }`"),
        "the tagged-VALUE remedy must wrap the group, not the tag, got: {tagged_value}"
    );
    let tagged_key = run(
        &format!("{GROUP}t = {{ * #6.5(coords) => uint }}\n"),
        "tagged_key",
        &[],
    )
    .expect_err("a tagged bare plain group as a table KEY domain must reject gracefully");
    assert!(
        tagged_key.contains("`{ * #6.5([coords]) => uint }`"),
        "the tagged-KEY remedy must wrap the group, not the tag, got: {tagged_key}"
    );

    // Remaining spellings of the same shape: inline key role, both roles at once (which names both
    // sides), the parenthesized table, an alias to the group, and the `+` cardinality.
    for (spec, tag, needle) in [
        (
            format!("{GROUP}t = [{{ * coords => uint }}]\n"),
            "inline_key",
            "as its KEY domain",
        ),
        (
            format!("{GROUP}t = [{{ * coords => coords }}]\n"),
            "inline_both",
            "as its VALUE domain",
        ),
        (
            format!("{GROUP}t = {{ * (uint => coords) }}\n"),
            "paren",
            "as its VALUE domain",
        ),
        (
            format!("{GROUP}c2 = coords\nt = {{ * uint => c2 }}\n"),
            "alias",
            "the bare plain group `coords`",
        ),
        (
            format!("{GROUP}t = {{ + uint => coords }}\n"),
            "plus",
            "as its VALUE domain",
        ),
    ] {
        let msg =
            run(&spec, tag, &[]).expect_err(&format!("`{}` must reject gracefully", spec.trim()));
        assert!(
            msg.contains(needle),
            "the {tag} spelling should reject with `{needle}`, got: {msg}"
        );
    }

    // Profile independence: the refusal is at parsing, so no flag combination reaches an emission
    // that could differ.
    for (extra, tag) in [
        (vec!["--preserve-encodings=true"], "preserve"),
        (vec!["--wasm=false"], "no_wasm"),
        (vec!["--json-serde-derives=true"], "json"),
    ] {
        run(&format!("{GROUP}t = {{ * uint => coords }}\n"), tag, &extra)
            .expect_err("the refusal must fire on every profile");
    }

    // The remedy must actually work — otherwise the message sends the author into another wall.
    // An array-WRAPPED group carries `basic_override`, so it serializes as one nested item and is
    // NOT a bare domain: `{1: Coords(7,8)}` inside a holder emits `81a101820708`, which decodes as
    // `[{1: [7, 8]}]` with no trailing bytes (verified out of tree; here we pin that it generates).
    for (spec, tag) in [
        (
            format!("{GROUP}t = {{ * uint => [coords] }}\n"),
            "wrap_value",
        ),
        (format!("{GROUP}t = {{ * [coords] => uint }}\n"), "wrap_key"),
        (
            format!("{GROUP}arr = [coords]\nt = {{ * uint => arr }}\n"),
            "wrap_named",
        ),
        (
            format!("{GROUP}t = [{{ * uint => [coords] }}]\n"),
            "wrap_inline",
        ),
        // The tagged-domain remedies the messages above print back, verified to generate so the
        // rejection cannot send an author into a second wall.
        (
            format!("{GROUP}t = {{ * uint => #6.5([coords]) }}\n"),
            "wrap_tagged_value",
        ),
        (
            format!("{GROUP}t = {{ * #6.5([coords]) => uint }}\n"),
            "wrap_tagged_key",
        ),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "the remedy `{}` must generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }

    // Neighbours the guard must NOT widen into: a plain table, the array-element workaround (an
    // array's emitted length scales with the group's arity, so a splice there IS conformant), a
    // single-element parenthesized rule (a transparent alias, not a group at all), and a group
    // reference whose target is a real array/map RULE rather than a plain group.
    for (spec, tag) in [
        ("t = { * uint => tstr }\n".to_owned(), "plain_table"),
        ("pair = (int, tstr)\na = [* pair]\n".to_owned(), "arr_elem"),
        (
            "one = (uint)\nt = { * uint => one }\n".to_owned(),
            "alias_one",
        ),
        (
            "coords = [uint, uint]\nt = { * uint => coords }\n".to_owned(),
            "array_rule",
        ),
        (
            "coords = {a: uint}\nt = { * uint => coords }\n".to_owned(),
            "map_rule",
        ),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "`{}` must still generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }
}

/// The struct-map twin of `plain_group_table_domain_rejects_gracefully_at_both_spellings`: a KEYED
/// map-record member whose type resolves to a plain group.
///
/// The key claims one map entry and that entry's value slot holds exactly one item, so the only
/// emission available — `serialize_as_embedded_group` — writes the group's members in flat and
/// overruns the header. `t = { c: kv }` used to do that at exit 0 with NO decoder at all
/// (`map_record_deser_refusals` declined the record), so the crate compiled, round-tripped against
/// nothing, and shipped bytes an interoperating decoder reads as `{'c': 'a'}` plus trailing bytes.
/// The other spellings failed louder but no better: `?` on the member hit
/// `assertion failed: !config.optional_field`, an alias to the group panicked on a struct that was
/// never materialized, and a single-entry group-choice arm emitted a serializer and a deserializer
/// that disagreed with each other.
///
/// Pins the message (rule, member, group source name, named-array remedy) across every keyed
/// spelling and every profile, that the two loud spellings no longer panic, and — the part that
/// keeps the refusal honest — that the remedy the message names actually generates, alongside the
/// three neighbours the guard must not widen into.
#[test]
fn plain_group_keyed_map_member_rejects_gracefully_at_every_spelling() {
    fn run(spec: &str, tag: &str, extra: &[&str]) -> Result<(), String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_groupmapmember_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "groupmapmember_unused",
        ];
        args.extend_from_slice(extra);
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result.map(|_| ()).map_err(|e| e.to_string())
    }

    const GROUP: &str = "kv = (a: uint, b: uint)\n";

    // The bare named member — the silent one. Names the rule by its SOURCE spelling, the member,
    // the group, why the splice cannot fit, and the NAMED-array remedy (not the inline `c: [kv]`,
    // which is a different, separately-refused shape).
    let named = run(&format!("{GROUP}t = {{ c: kv }}\n"), "named", &[])
        .expect_err("a keyed map member whose type is a plain group must reject gracefully");
    assert!(
        named.contains("rule `t`: map field `c` uses the plain group `kv` as its type"),
        "rejection should name the rule, member and group, got: {named}"
    );
    assert!(
        named.contains("a CBOR map entry holds exactly one item in its value slot"),
        "rejection should explain why the splice is impossible, got: {named}"
    );
    assert!(
        named.contains("`w = [kv]`, then `c: w`"),
        "rejection should name the NAMED-array remedy, got: {named}"
    );
    assert!(
        named.contains("Writing the array inline (`c: [kv]`) is not the remedy"),
        "rejection should rule the inline array OUT as the remedy, got: {named}"
    );

    // Every other keyed spelling reaches the same seam and carries the same body. The two marked
    // PANICKED are the reason this is a refusal and not a warning.
    for (spec, tag, needle) in [
        // a TAG around the member: an encoding operation, so the member type is still the group.
        (
            format!("{GROUP}t = {{ c: #6.1(kv) }}\n"),
            "tagged",
            "map field `c` uses the plain group `kv`",
        ),
        // PANICKED on `assertion failed: !config.optional_field`.
        (
            format!("{GROUP}t = {{ ? c: kv }}\n"),
            "optional",
            "map field `c` uses the plain group `kv`",
        ),
        // PANICKED on an unmaterialized struct; caught through shallow alias resolution.
        (
            format!("{GROUP}kv_alias = kv\nt = {{ c: kv_alias }}\n"),
            "alias",
            "map field `c` uses the plain group `kv`",
        ),
        // a single-entry map group-choice arm: the ENUM seam, not the record one.
        (
            format!("{GROUP}t = {{ n: uint // c: kv }}\n"),
            "choice_arm",
            "rule `t`: map field `c` uses the plain group `kv`",
        ),
        // a multi-entry arm, which builds a record and reaches the record seam (cited by the
        // synthesized arm-struct name, which is the struct the member actually lives on).
        (
            format!("{GROUP}t = {{ n: uint // c: kv, d: uint }}\n"),
            "choice_arm_multi",
            "rule `T1`: map field `c` uses the plain group `kv`",
        ),
        // rule ORDER must not matter: the plain-group registry is settled in a pre-pass over every
        // rule before any of them is parsed.
        (
            "t = { c: kv }\nkv = (a: uint, b: uint)\n".to_owned(),
            "reversed",
            "map field `c` uses the plain group `kv`",
        ),
    ] {
        let msg =
            run(&spec, tag, &[]).expect_err(&format!("`{}` must reject gracefully", spec.trim()));
        assert!(
            msg.contains(needle),
            "the {tag} spelling should reject with `{needle}`, got: {msg}"
        );
    }

    // Profile independence: the refusal is at parsing, so no flag combination reaches an emission
    // that could differ. (Under `--preserve-encodings` the group-choice spelling used to abort on a
    // variant-arity `assert_eq!` instead, and `--wasm=true` on a generic-instance assert.)
    for (extra, tag) in [
        (vec!["--preserve-encodings=true"], "preserve"),
        (vec!["--wasm=true"], "wasm"),
        (vec!["--json-serde-derives=true"], "json"),
    ] {
        run(&format!("{GROUP}t = {{ c: kv }}\n"), tag, &extra)
            .expect_err("the refusal must fire on every profile");
        run(&format!("{GROUP}t = {{ n: uint // c: kv }}\n"), tag, &extra)
            .expect_err("the group-choice-arm refusal must fire on every profile too");
    }

    // The remedy must actually work — otherwise the message sends the author into another wall.
    // A NAMED array rule gives the slot one nested item and generates a FULL codec.
    for (spec, tag, extra) in [
        (
            format!("{GROUP}w = [kv]\nt = {{ c: w }}\n"),
            "remedy",
            vec![],
        ),
        (
            format!("{GROUP}w = [kv]\nt = {{ c: w }}\n"),
            "remedy_preserve",
            vec!["--preserve-encodings=true"],
        ),
    ] {
        assert!(
            run(&spec, tag, &extra).is_ok(),
            "the remedy `{}` must generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &extra).err()
        );
    }

    // Neighbours the guard must NOT widen into: the ARRAY-representation placement of the same
    // group (an array's emitted length scales with the group's arity, so a splice there IS
    // conformant), a KEYLESS single-entry map group-choice arm (the referenced struct owns its own
    // keys, so it writes a conformant 2-entry map), a single-element parenthesized rule (a
    // transparent alias, not a group at all), and a member whose target is a real map RULE.
    for (spec, tag) in [
        (
            format!("{GROUP}t = [ c: uint, kv ]\n"),
            "array_rep_placement",
        ),
        (format!("{GROUP}t = {{ n: uint // kv }}\n"), "keyless_arm"),
        (
            "one = (uint)\nt = { c: one }\n".to_owned(),
            "single_elem_alias",
        ),
        ("kv = {a: uint}\nt = { c: kv }\n".to_owned(), "map_rule"),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "`{}` must still generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }

    // The already-refused neighbours keep their OWN messages: the inline array member is a
    // conflicting representation on the group, and the keyless member has no key at all.
    let inline_arr = run(&format!("{GROUP}t = {{ c: [kv] }}\n"), "inline_arr", &[])
        .expect_err("the inline array member must keep its own refusal");
    assert!(
        inline_arr.contains("used with conflicting representations (both array and map)"),
        "the inline array member should keep the conflicting-representations message, got: {inline_arr}"
    );
    let keyless = run(&format!("{GROUP}t = {{ n: uint, kv }}\n"), "keyless", &[])
        .expect_err("a keyless plain-group member must keep its own refusal");
    assert!(
        keyless.contains("map field `kv` has no key"),
        "the keyless member should keep the no-key message, got: {keyless}"
    );
}

/// Collapse runs of blank lines to one and drop trailing blank lines, so a removed line leaves no
/// layout artifact in a source-vs-source comparison.
fn collapse_blank_runs(body: &str) -> String {
    let mut out: Vec<&str> = Vec::new();
    for line in body.lines() {
        if line.trim().is_empty() && out.last().is_some_and(|prev: &&str| prev.trim().is_empty()) {
            continue;
        }
        out.push(line);
    }
    while out.last().is_some_and(|line| line.trim().is_empty()) {
        out.pop();
    }
    out.join("\n")
}

/// The ARRAY-representation counterpart of the map twin above, and its opposite verdict: a plain
/// group referenced through a TRANSPARENT ALIAS in an array position is SUPPORTED, and behaves
/// exactly like the direct reference.
///
/// An array's emitted length scales with the group's arity, so the flat splice the map rep cannot
/// afford is the conformant emission here — and an alias carries one wire form, so `kv_alias`
/// must mean what `kv` means. What stood in the way was purely a registration gap: the rep-stamp
/// sites matched a bare `Rust` ident and so never materialized a group reached through an alias,
/// while `is_basic` downstream DOES shallow-resolve and still selected the splicing emission. The
/// resulting failures had no single signature to pin — the record-field spelling aborted at a
/// different site per profile (`rust struct Kv not found …` on default, an `Option::unwrap()`
/// under `--preserve-encodings`, a generic-instance assert under `--wasm=true`), and the
/// homogeneous-ELEMENT spelling was worse than a panic: exit 0 emitting `pub type KvAlias = Kv;`
/// with no `Kv` at all, a crate that fails `cargo check` with E0425 while the tool reported
/// success.
///
/// Pins every array-position spelling on every profile, at alias depth 2 and under a reversed rule
/// order, and — the part that makes "supported" mean something — that the alias spelling's emitted
/// code is the direct spelling's modulo the alias name, so the two write the same bytes. The map
/// twin's refusal and the table-domain refusal must NOT move: they are the shapes where a splice
/// really is unrepresentable.
#[test]
fn alias_to_plain_group_in_array_positions_matches_the_direct_reference() {
    const GROUP: &str = "kv = (a: uint, b: uint)\n";
    const ALIAS: &str = "kv = (a: uint, b: uint)\nkv_alias = kv\n";

    // (tag, the ALIAS spelling, the DIRECT spelling it must behave like). Each pair differs only in
    // how the group is named at the use site.
    let pairs: &[(&str, String, String)] = &[
        // the record-field splice: the ledgered panic.
        (
            "field",
            format!("{ALIAS}t = [ c: uint, kv_alias ]\n"),
            format!("{GROUP}t = [ c: uint, kv ]\n"),
        ),
        // alias DEPTH: `resolve_alias_shallow` recurses, so one call covers a chain.
        (
            "chain",
            format!("{ALIAS}kv2 = kv_alias\nt = [ c: uint, kv2 ]\n"),
            format!("{GROUP}t = [ c: uint, kv ]\n"),
        ),
        // rule ORDER must not matter: the plain-group registry is settled in a pre-pass.
        (
            "reversed",
            "t = [ c: uint, kv_alias ]\nkv_alias = kv\nkv = (a: uint, b: uint)\n".to_owned(),
            format!("{GROUP}t = [ c: uint, kv ]\n"),
        ),
        // the rule-level homogeneous ELEMENT: the exit-0 non-compiling class.
        (
            "element_rule",
            format!("{ALIAS}a = [* kv_alias]\n"),
            format!("{GROUP}a = [* kv]\n"),
        ),
        // the member-position homogeneous element, a second stamp site.
        (
            "element_member",
            format!("{ALIAS}t = [ x: [* kv_alias] ]\n"),
            format!("{GROUP}t = [ x: [* kv] ]\n"),
        ),
        // a single-entry group-choice ARM, a third: the arm's embedded classification is read off
        // the same ident, so leaving it bare left the arm neither registered nor embedded.
        (
            "choice_arm",
            format!("{ALIAS}t = [ x: uint // kv_alias ]\n"),
            format!("{GROUP}t = [ x: uint // kv ]\n"),
        ),
        // the MAP rep's keyless arm is the same seam: the referenced struct owns its own keys, so
        // this one is supported in the map rep too, and the alias spelling used to fall out of the
        // embedded classification and into the no-key rejection.
        (
            "choice_arm_map_keyless",
            format!("{ALIAS}t = {{ n: uint // kv_alias }}\n"),
            format!("{GROUP}t = {{ n: uint // kv }}\n"),
        ),
        // the anonymous array WRAPPING the aliased group: already supported, and the arm this
        // resolution pattern was copied from. Guards it against sliding back.
        (
            "wrapped",
            format!("{ALIAS}t = [ x: [kv_alias] ]\n"),
            format!("{GROUP}t = [ x: [kv] ]\n"),
        ),
    ];

    for (extra, profile) in [
        (vec![], "default"),
        (vec!["--preserve-encodings=true"], "preserve"),
        (vec!["--wasm=true"], "wasm"),
        (vec!["--json-serde-derives=true"], "json"),
    ] {
        for (tag, alias_spec, direct_spec) in pairs {
            let alias_files =
                expect_generates(&format!("alias_arr_{tag}_{profile}"), alias_spec, &extra);
            let direct_files = expect_generates(
                &format!("alias_arr_{tag}_{profile}_direct"),
                direct_spec,
                &extra,
            );

            // Materialization is the whole fix: the group must exist as a struct, not dangle
            // behind a typedef. (`pub type KvAlias = Kv;` with no `Kv` is what exit-0 broken
            // looked like.)
            let alias_lib = alias_files
                .iter()
                .find(|(name, _)| name.ends_with("rust/src/generated/mod.rs"))
                .map(|(_, body)| body.clone())
                .unwrap_or_else(|| panic!("{tag}/{profile}: no generated rust mod.rs"));
            assert!(
                alias_lib.contains("pub struct Kv"),
                "{tag}/{profile}: the aliased plain group must be materialized as a struct, got:\n{alias_lib}"
            );

            // Behaviour equality: the alias spelling's emitted code is the direct spelling's once
            // the alias NAME is spelled back. `KvAlias` is the alias ident and `kv_alias` the
            // member name the field walk derives from it; nothing else may differ.
            //
            // `serialization.rs` — the WIRE — is compared for every spelling, and that is the
            // property this card is about. `mod.rs` is compared for everything except the
            // group-choice arms, whose ctor SHAPE differs by a standing decision recorded on
            // `EnumVariant::group_ctor_record_fields`: an alias arm deliberately gets the
            // single-argument `new_kv(kv: Kv)` where the direct arm expands the group's fields
            // (`new_kv(a, b)`). Same variant, same bytes, different ergonomics — a compile-visible
            // API difference, not a wire one. It is pinned below rather than normalized away, so a
            // future change to that decision fails here and gets a deliberate update.
            let ctor_shape_differs = tag.starts_with("choice_arm");
            for file in [
                "rust/src/generated/serialization.rs",
                "rust/src/generated/mod.rs",
            ]
            .into_iter()
            .filter(|file| !(ctor_shape_differs && file.ends_with("mod.rs")))
            {
                let (Some(alias_body), Some(direct_body)) = (
                    alias_files
                        .iter()
                        .find(|(name, _)| name.ends_with(file))
                        .map(|(_, body)| body),
                    direct_files
                        .iter()
                        .find(|(name, _)| name.ends_with(file))
                        .map(|(_, body)| body),
                ) else {
                    continue;
                };
                // Spell the alias name back, then drop the alias typedef itself — a
                // `pub type Kv = Kv;` line after the rename, with no counterpart in the direct
                // spelling. Blank-line runs collapse so the removal leaves no layout artifact.
                let respelled = alias_body
                    .replace("KvAlias", "Kv")
                    .replace("Kv2", "Kv")
                    .replace("kv_alias", "kv")
                    .replace("kv2", "kv");
                let normalized = collapse_blank_runs(
                    &respelled
                        .lines()
                        .filter(|line| line.trim() != "pub type Kv = Kv;")
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
                assert_eq!(
                    normalized,
                    collapse_blank_runs(direct_body),
                    "{tag}/{profile}: {file} must match the direct reference's once the alias name \
                     is spelled back"
                );
            }

            // The one carve-out above, pinned in both directions so it stays a KNOWN difference.
            if ctor_shape_differs && profile == "default" {
                let direct_lib = direct_files
                    .iter()
                    .find(|(name, _)| name.ends_with("rust/src/generated/mod.rs"))
                    .map(|(_, body)| body.clone())
                    .unwrap_or_else(|| panic!("{tag}: no direct rust mod.rs"));
                assert!(
                    alias_lib.contains("pub fn new_kv_alias(kv_alias: KvAlias) -> Self"),
                    "{tag}: the alias arm keeps the single-argument ctor, got:\n{alias_lib}"
                );
                assert!(
                    direct_lib.contains("pub fn new_kv(a: u64, b: u64) -> Self"),
                    "{tag}: the direct arm keeps the field-expanded ctor, got:\n{direct_lib}"
                );
            }
        }
    }

    // The refusals this must NOT widen into. A map entry's value slot holds exactly one item, so
    // the KEYED map member and the table domain stay rejected through the alias exactly as they are
    // directly — resolving the alias is what lets those seams SEE the group in the first place.
    for (spec, tag, needle) in [
        (
            format!("{ALIAS}t = {{ c: kv_alias }}\n"),
            "map_member_alias",
            "map field `c` uses the plain group `kv`",
        ),
        (
            format!("{ALIAS}a = {{ * uint => kv_alias }}\n"),
            "table_value_alias",
            "as its VALUE domain",
        ),
    ] {
        let path =
            std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
        std::fs::write(&path, &spec).unwrap();
        let err = crate::api::generated_strings(&Cli::parse_from(vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "alias_arr_unused",
        ]))
        .err()
        .unwrap_or_else(|| panic!("`{}` must still reject", spec.trim()));
        std::fs::remove_file(&path).ok();
        assert!(
            err.to_string().contains(needle),
            "the {tag} spelling must keep its own refusal (`{needle}`), got: {err}"
        );
    }
}

/// The one MODIFIER the array placement cannot honour: an OPTIONAL (`?`) plain-group field in an
/// ARRAY-representation record.
///
/// Everywhere else in an array a plain group is welcome — the array's emitted length scales with
/// the group's arity, so the flat splice is conformant, and that is why the mandatory and
/// alias-indirected spellings are supported rather than refused. `?` is where that stops: the
/// splice writes members with no marker of its own, so the ONLY evidence of presence is the
/// array's length, while an embedded decoder length-checks just the members it consumed. Deciding
/// present-vs-absent needs the group's mandatory member count charged to the ENCLOSING read length
/// before the group is read (or a second embedded deserialize method) — which is the
/// occurrence/bounds program's scope, not a guard's, so this is a refusal and not an
/// implementation.
///
/// It is a refusal that can be made honest, which is the whole reason to prefer it over the
/// `assertion failed: !config.optional_field` abort it replaces: the array-framed remedy the
/// message names is asserted here to generate. Pins the message (rule, field, group source name,
/// remedy) across every spelling the one seam covers — bare, ALIAS, and TAGGED, the last of which
/// used to exit 0 with a codec whose own decoder rejected its own bytes — on every profile, plus
/// the neighbours the guard must not widen into: the mandatory splice, the remedy, the pedantic
/// `1*1`, an optional NON-group field, the inline array-wrapped member, and the map twin's own
/// message.
#[test]
fn optional_plain_group_array_field_rejects_gracefully_at_every_spelling() {
    fn run(spec: &str, tag: &str, extra: &[&str]) -> Result<(), String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_optgrouparr_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "optgrouparr_unused",
        ];
        args.extend_from_slice(extra);
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result.map(|_| ()).map_err(|e| e.to_string())
    }

    const GROUP: &str = "kv = (a: uint, b: uint)\n";
    const ALIAS: &str = "kv = (a: uint, b: uint)\nkv_alias = kv\n";

    // The bare spelling, in full: names the rule by its SOURCE spelling, the field, the group, why
    // the optional splice cannot be decoded, and the array-framed remedy.
    let bare = run(&format!("{GROUP}t = [ c: uint, ? kv ]\n"), "bare", &[])
        .expect_err("an optional plain-group field in an array record must reject gracefully");
    assert!(
        bare.contains(
            "rule `t`: array field `kv` is an OPTIONAL (`?`) reference to the plain group `kv`"
        ),
        "rejection should name the rule, field and group, got: {bare}"
    );
    assert!(
        bare.contains("nothing on the wire marks where the optional group starts"),
        "rejection should explain why presence cannot be decided, got: {bare}"
    );
    assert!(
        bare.contains("`w = [kv]`, then `? w` in place of `? kv`"),
        "rejection should name the array-framed remedy, got: {bare}"
    );
    assert!(
        bare.contains("Dropping the `?`"),
        "rejection should point at the supported mandatory splice, got: {bare}"
    );

    // Every other spelling reaches the same seam and carries the same body, because the guard reads
    // the RESOLVED member type rather than its surface shape.
    for (spec, tag, needle) in [
        // reachable only since the alias-in-array support landed; before that the alias gap fired
        // first, at its own (per-profile) site.
        (
            format!("{ALIAS}t = [ c: uint, ? kv_alias ]\n"),
            "alias",
            "array field `kv_alias` is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
        // a TAG around the member is an encoding operation, so the member type is still the group.
        // This one used to exit 0 emitting a codec that fails its own round-trip.
        (
            format!("{GROUP}t = [ c: uint, ? #6.1(kv) ]\n"),
            "tagged",
            "is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
        // POSITION is irrelevant — the seam is the field, not the tail.
        (
            format!("{GROUP}t = [ c: uint, ? kv, d: uint ]\n"),
            "non_final",
            "array field `kv` is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
        // a group whose own members are ALL optional is reachable and deliberately still refused:
        // the remedy serves it identically, so a narrower guard would buy nothing.
        (
            "kv = (? a: uint, ? b: uint)\nt = [ c: uint, ? kv ]\n".to_owned(),
            "all_optional_members",
            "array field `kv` is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
        // rule ORDER must not matter: the plain-group registry is settled in a pre-pass.
        (
            "t = [ c: uint, ? kv ]\nkv = (a: uint, b: uint)\n".to_owned(),
            "reversed",
            "array field `kv` is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
    ] {
        let msg =
            run(&spec, tag, &[]).expect_err(&format!("`{}` must reject gracefully", spec.trim()));
        assert!(
            msg.contains(needle),
            "the {tag} spelling should reject with `{needle}`, got: {msg}"
        );
    }

    // Profile independence: the refusal is at parsing, so no flag combination reaches an emission
    // that could differ. (All three of these formerly aborted at exit 101.)
    for (extra, tag) in [
        (vec!["--preserve-encodings=true"], "preserve"),
        (vec!["--wasm=true"], "wasm"),
        (vec!["--json-serde-derives=true"], "json"),
    ] {
        for (spec, spelling) in [
            (format!("{GROUP}t = [ c: uint, ? kv ]\n"), "bare"),
            (format!("{ALIAS}t = [ c: uint, ? kv_alias ]\n"), "alias"),
        ] {
            let msg = run(&spec, &format!("{tag}_{spelling}"), &extra)
                .expect_err("the refusal must fire on every profile");
            assert!(
                msg.contains("is an OPTIONAL (`?`) reference to the plain group `kv`"),
                "the {spelling} spelling on {tag} should carry the same body, got: {msg}"
            );
        }
    }

    // The remedy must actually work — otherwise the message sends the author into another wall.
    for (extra, tag) in [
        (vec![], "remedy"),
        (vec!["--preserve-encodings=true"], "remedy_preserve"),
    ] {
        let spec = format!("{GROUP}w = [kv]\nt = [ c: uint, ? w ]\n");
        assert!(
            run(&spec, tag, &extra).is_ok(),
            "the remedy `{}` must generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &extra).err()
        );
    }

    // Neighbours the guard must NOT widen into. The first three are the array placement's supported
    // core (an array's length scales with the group's arity, so a MANDATORY splice is conformant —
    // through an alias too, and `1*1` is mandatory however pedantically spelled); the fourth is an
    // optional NON-group field, which the optionality machinery has always handled; the fifth is the
    // inline array-wrapped member, which carries `basic_override` and so is not a plain group here.
    for (spec, tag) in [
        (format!("{GROUP}t = [ c: uint, kv ]\n"), "mandatory_splice"),
        (
            format!("{ALIAS}t = [ c: uint, kv_alias ]\n"),
            "mandatory_splice_alias",
        ),
        (format!("{GROUP}t = [ c: uint, 1*1 kv ]\n"), "pedantic_1x1"),
        ("t = [ c: uint, ? d: uint ]\n".to_owned(), "optional_scalar"),
        (
            format!("{GROUP}t = [ c: uint, ? x: [kv] ]\n"),
            "inline_array_wrapped",
        ),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "`{}` must still generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }

    // The MAP twin keeps its OWN message: there the splice is unrepresentable at any optionality,
    // so the refusal is about the value slot rather than about presence.
    let map_twin = run(
        &format!("{GROUP}t = {{ c: uint, ? d: kv }}\n"),
        "map_twin",
        &[],
    )
    .expect_err("the map twin must keep rejecting");
    assert!(
        map_twin.contains("map field `d` uses the plain group `kv` as its type"),
        "the map twin should keep its own message, got: {map_twin}"
    );

    // The narrows guard beside this one is untouched: a count-permitting occurrence in NON-final
    // position still rejects on its own terms, and never with this message.
    let narrowed = run(
        &format!("{GROUP}t = [ c: uint, * kv, d: uint ]\n"),
        "narrows",
        &[],
    )
    .expect_err("a non-final count-permitting occurrence must still reject");
    assert!(
        !narrowed.contains("is an OPTIONAL (`?`) reference"),
        "a non-`?` occurrence must not borrow this message, got: {narrowed}"
    );
}

/// An occurrence marker on the single entry of a single-entry group-choice ARM is refused — except
/// where DROPPING it is sound, which `inline_group_occurrence_flattens` already decides and this
/// seam defers to rather than restating.
///
/// A one-entry arm registers no record at all — the entry's TYPE goes straight into the enum
/// variant, and a variant holds exactly one value — so there is nowhere for a repetition count to
/// live and the marker was read by NOTHING. That made it the quietest defect class on the board:
/// `[ x: uint // ? kv ]`, `// * kv`, `// + kv` and `// 2*3 kv` each generated output
/// byte-identical to the unmarked `// kv` at exit 0, in both representations, through an alias and
/// under a tag, and NOT only for plain groups — `[ x: uint // ? a: tstr ]` was byte-identical to
/// its unmarked twin too.
///
/// Where that byte identity is WRONG it is wrong on the wire: the codec rejects exactly the counts
/// the marker admits — the empty encoding a `?` / `*` / `0*n` arm allows comes back as
/// `No variant matched … Definite length mismatch: found 0`, and (in an ARRAY) every 2-or-more
/// encoding fails the same way. Where it is RIGHT it is the f18d764 boundary: under unique map keys
/// a second repetition of a fixed-key alternative would duplicate its keys, so in a MAP every
/// lower-bound-≥1 marker (`+`, `2*3`, `2*`) admits count 1 and nothing else, and dropping it is the
/// HONORED semantics. Those spellings therefore keep generating, and their byte identity with the
/// unmarked twin is pinned below as the contract rather than tolerated as an accident.
///
/// Honoring the markers that DO refuse would need a zero-case variant that is TELLABLE on the wire
/// — the sibling arms' own length checks have to exclude the empty form — which is the
/// occurrence/bounds program's scope. The refusal is honest because the remedy it names is asserted
/// here to generate in BOTH reps: a TYPE choice over one named rule per count. The named-array
/// wrapper (`w = [kv]` from the arm) is deliberately not that remedy — it nests the group in an
/// array of its own and cannot express the empty case — so the message must never name it.
///
/// Pins the message (site, arm source spelling, the rep-scoped marker list, consequence and remedy)
/// across the per-marker × per-rep matrix on every profile, the deliberate non-firings (`1*1`, an
/// absent marker, and the map-side collapse), the verified remedies, and the neighbouring guards
/// this one must not shadow or double-report: the multi-entry arm's optional-plain-group refusal,
/// the rest-tail-in-a-choice-arm refusal, and the inline-group entry-position refusal.
#[test]
fn occurrence_on_single_entry_group_choice_arm_rejects_gracefully() {
    // Returns the whole emitted file map, so a caller can assert BYTE identity between a spelling
    // and its unmarked twin — which is the contract for the markers the map-side collapse honors,
    // not merely an observation about them.
    fn emit(
        spec: &str,
        tag: &str,
        extra: &[&str],
    ) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_armoccur_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "armoccur_unused",
        ];
        args.extend_from_slice(extra);
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        result.map_err(|e| e.to_string())
    }
    fn run(spec: &str, tag: &str, extra: &[&str]) -> Result<(), String> {
        emit(spec, tag, extra).map(|_| ())
    }

    const GROUP: &str = "kv = (a: uint, b: uint)\n";
    const ALIAS: &str = "kv = (a: uint, b: uint)\nkv_alias = kv\n";
    const WHY: &str = "a single-entry arm becomes ONE enum variant holding exactly one value, so \
                       the marker is dropped";

    // The bare ARRAY spelling, in full: the site (rule + the arm exactly as written), why the
    // dropped marker is wire-wrong, the array-framed TYPE-choice remedy, and what stays supported.
    let bare = run(&format!("{GROUP}t = [ x: uint // ? kv ]\n"), "bare", &[])
        .expect_err("an occurrence marker on a single-entry arm must reject gracefully");
    assert!(
        bare.contains("rule `t`: the group-choice arm `? kv` carries an occurrence marker"),
        "rejection should name the rule and the arm as written, got: {bare}"
    );
    assert!(
        bare.contains(WHY),
        "rejection should explain that the marker is dropped, got: {bare}"
    );
    assert!(
        bare.contains("rejected by a decoder the spec says must accept it"),
        "rejection should name the wire consequence, got: {bare}"
    );
    assert!(
        bare.contains("`one = [ … ]`") && bare.contains("`t = one / none / many`"),
        "rejection should name the array TYPE-choice remedy, got: {bare}"
    );
    assert!(
        bare.contains("the pedantic `1*1`, which already means exactly once"),
        "rejection should say what stays supported, got: {bare}"
    );
    // The named-array WRAPPER is not the remedy here: it cannot express the empty case.
    assert!(
        !bare.contains("`w = [kv]`"),
        "the wrapper remedy cannot express the empty case and must not be advertised, got: {bare}"
    );

    // Per-marker × per-rep, the whole matrix in one place. In an ARRAY every marker but an absent
    // one and `1*1` refuses. In a MAP only the ZERO-permitting ones do: `+` / `2*3` / `2*` admit
    // exactly one repetition under unique map keys (a second would duplicate the alternative's
    // fixed keys), so dropping them is the honored semantics — the f18d764 boundary, decided by
    // `inline_group_occurrence_flattens` for both its consumers rather than restated here.
    for (marker, tag, map_refuses) in [
        ("?", "opt", true),
        ("*", "star", true),
        ("*3", "upper_only", true),
        ("0*3", "zero_bounded", true),
        ("+", "plus", false),
        ("2*3", "bounded", false),
        ("2*", "lower_only", false),
    ] {
        // ARRAY: an array's length scales with repetitions, so nothing collapses — all refuse.
        let arr = format!("{GROUP}t = [ x: uint // {marker} kv ]\n");
        let msg = run(&arr, &format!("{tag}_array"), &[])
            .expect_err(&format!("`{}` must reject gracefully", arr.trim()));
        assert!(
            msg.contains(&format!(
                "the group-choice arm `{marker} kv` carries an occurrence marker (`?` / `*` / `+` \
                 / `n*m`)"
            )),
            "the {marker} marker in an array arm should reject naming the arm, got: {msg}"
        );

        let map = format!("{GROUP}t = {{ x: uint // {marker} kv }}\n");
        if map_refuses {
            let msg = run(&map, &format!("{tag}_map"), &[])
                .expect_err(&format!("`{}` must reject gracefully", map.trim()));
            assert!(
                msg.contains(&format!(
                    "the group-choice arm `{marker} kv` carries a zero-permitting occurrence \
                     marker (`?` / `*` / `0*n` / `*n`)"
                )),
                "the {marker} marker in a map arm should reject as zero-permitting, got: {msg}"
            );
        } else {
            // The HONORED half, and its contract is byte identity with the unmarked twin — not
            // merely "it generates". Asserted over the whole emitted file map.
            let marked = emit(&map, &format!("{tag}_map"), &[]).unwrap_or_else(|e| {
                panic!(
                    "`{}` collapses to exactly-once and must generate: {e}",
                    map.trim()
                )
            });
            let unmarked = emit(
                &format!("{GROUP}t = {{ x: uint // kv }}\n"),
                &format!("{tag}_map_twin"),
                &[],
            )
            .expect("the unmarked map arm must generate");
            assert_eq!(
                marked, unmarked,
                "`{marker} kv` in a map arm admits exactly one repetition under unique keys, so it \
                 must emit the mandatory arm's bytes exactly"
            );
        }
    }

    // The map refusal's BODY is rep-scoped too, and for one reason: a map has no 2-or-more encoding
    // to talk about. It claims only the empty case, and its remedy spells members (a MAP-rep record
    // refuses a keyless plain-group member outright, so `{kv}` is not available to it) and offers
    // no repeating alternative. The parenthetical names the collapse as supported.
    let map_msg = run(
        &format!("{GROUP}t = {{ x: uint // ? kv }}\n"),
        "map_body",
        &[],
    )
    .expect_err("a zero-permitting marker in the map rep must reject");
    assert!(
        map_msg.contains("`one = { … }` spelling the alternative's own members")
            && map_msg.contains("`t = one / none`"),
        "the map rep should carry its own remedy text, got: {map_msg}"
    );
    assert!(
        map_msg.contains(
            "a map's keys are unique, so a repeated fixed-key alternative has no 2-or-more \
             encoding in the first place"
        ),
        "the map rep should claim only the empty case, got: {map_msg}"
    );
    assert!(
        map_msg.contains(
            "every lower-bound-≥1 marker — `+`, `2*3`, `2*` — which admit exactly one repetition"
        ),
        "the map rep should name the collapsed markers as supported, got: {map_msg}"
    );
    assert!(
        !map_msg.contains("`many = ") && !map_msg.contains("2-or-more encoding under"),
        "the map rep must not claim a repeating form it has no encoding for, got: {map_msg}"
    );

    // The guard reads the ENTRY's occurrence, not the entry's shape, so every arm shape it can
    // carry lands on the one message — including the ones that are not plain groups at all.
    for (spec, tag, needle) in [
        // an ALIAS to the plain group.
        (
            format!("{ALIAS}t = [ x: uint // ? kv_alias ]\n"),
            "alias",
            "the group-choice arm `? kv_alias` carries an occurrence marker",
        ),
        // a TAG around the group.
        (
            format!("{GROUP}t = [ x: uint // ? #6.10(kv) ]\n"),
            "tagged",
            "the group-choice arm `? #6.10(kv)` carries an occurrence marker",
        ),
        // NOT group-specific: an ordinary keyed member arm dropped its marker identically.
        (
            "t = [ x: uint // ? a: tstr ]\n".to_owned(),
            "keyed_member",
            "the group-choice arm `? a: tstr` carries an occurrence marker",
        ),
        // a BARE typename arm, whose variant name is minted from the type.
        (
            "t = [ x: uint // * tstr ]\n".to_owned(),
            "bare_typename",
            "the group-choice arm `* tstr` carries an occurrence marker",
        ),
        // an explicitly `@name`d arm: the name settles the variant, it does not excuse the marker.
        (
            format!("{GROUP}t = [ x: uint // ; @name Alt\n ? kv ]\n"),
            "named_arm",
            "carries an occurrence marker",
        ),
        // rule ORDER must not matter: the plain-group registry is settled in a pre-pass.
        (
            "t = [ x: uint // ? kv ]\nkv = (a: uint, b: uint)\n".to_owned(),
            "reversed",
            "the group-choice arm `? kv` carries an occurrence marker",
        ),
    ] {
        let msg =
            run(&spec, tag, &[]).expect_err(&format!("`{}` must reject gracefully", spec.trim()));
        assert!(
            msg.contains(needle),
            "the {tag} spelling should reject with `{needle}`, got: {msg}"
        );
    }

    // The map-side collapse is not confined to the plain-group entry shape: it is an argument about
    // KEYS, so every fixed-key alternative gets it. Byte identity with the unmarked twin, again as
    // the contract.
    for (marked, unmarked, tag) in [
        (
            "t = { x: uint // + y: tstr }\n".to_owned(),
            "t = { x: uint // y: tstr }\n".to_owned(),
            "collapse_keyed_member",
        ),
        (
            "t = { x: uint // 2*3 y: tstr }\n".to_owned(),
            "t = { x: uint // y: tstr }\n".to_owned(),
            "collapse_keyed_bounded",
        ),
        (
            format!("{ALIAS}t = {{ x: uint // + kv_alias }}\n"),
            format!("{ALIAS}t = {{ x: uint // kv_alias }}\n"),
            "collapse_alias",
        ),
    ] {
        assert_eq!(
            emit(&marked, tag, &[])
                .unwrap_or_else(|e| panic!("`{}` must generate: {e}", marked.trim())),
            emit(&unmarked, &format!("{tag}_twin"), &[]).expect("the unmarked twin must generate"),
            "`{}` must emit the unmarked arm's bytes exactly",
            marked.trim()
        );
    }

    // Profile independence, in both directions: the refusal is at parsing, so no flag combination
    // reaches an emission that could differ (these formerly exited 0 with the silently-narrowed
    // codec), and the collapse is a parse-time reading of the marker, so it honors on every profile
    // too.
    for (extra, tag) in [
        (vec!["--preserve-encodings=true"], "preserve"),
        (vec!["--wasm=true"], "wasm"),
        (vec!["--json-serde-derives=true"], "json"),
    ] {
        for (spec, spelling, needle) in [
            (
                format!("{GROUP}t = [ x: uint // ? kv ]\n"),
                "array",
                "carries an occurrence marker (`?` / `*` / `+` / `n*m`)",
            ),
            (
                format!("{GROUP}t = {{ x: uint // * kv }}\n"),
                "map",
                "carries a zero-permitting occurrence marker (`?` / `*` / `0*n` / `*n`)",
            ),
        ] {
            let msg = run(&spec, &format!("{tag}_{spelling}"), &extra)
                .expect_err("the refusal must fire on every profile");
            assert!(
                msg.contains(needle),
                "the {spelling} spelling on {tag} should carry its rep's body, got: {msg}"
            );
        }
        assert!(
            run(
                &format!("{GROUP}t = {{ x: uint // + kv }}\n"),
                &format!("{tag}_collapse"),
                &extra
            )
            .is_ok(),
            "the map-side collapse must honor on {tag} too"
        );
    }

    // The remedies the message names must actually work — otherwise it sends the author into
    // another wall. Both reps, and the `*` form the array text offers.
    for (spec, tag) in [
        (
            format!(
                "{GROUP}xarr = [x: uint]\nkvarr = [kv]\nempty = []\nt = xarr / kvarr / empty\n"
            ),
            "remedy_array_optional",
        ),
        (
            format!("{GROUP}xarr = [x: uint]\nkvs = [* kv]\nt = xarr / kvs\n"),
            "remedy_array_repeating",
        ),
        (
            "xmap = {x: uint}\nkvmap = {a: uint, b: uint}\nempty = {}\nt = xmap / kvmap / empty\n"
                .to_owned(),
            "remedy_map",
        ),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "the remedy `{}` must generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }

    // Neighbours the guard must NOT widen into. The unmarked arm is the supported core in both
    // reps, and `1*1` is exactly-once however pedantically spelled, in EITHER rep — the same
    // carve-out the array record-field loop's `narrows` guard makes, so dropping it narrows nothing.
    for (spec, tag) in [
        (format!("{GROUP}t = [ x: uint // kv ]\n"), "mandatory_array"),
        (format!("{GROUP}t = {{ x: uint // kv }}\n"), "mandatory_map"),
        (
            format!("{GROUP}t = [ x: uint // 1*1 kv ]\n"),
            "exact_1x1_array",
        ),
        (
            format!("{GROUP}t = {{ x: uint // 1*1 kv }}\n"),
            "exact_1x1_map",
        ),
        // a MULTI-entry arm carrying `?` on a member routes the record walk, which has always
        // handled optional non-group fields.
        (
            "t = [ x: uint // ? c: uint, d: uint ]\n".to_owned(),
            "multi_entry_optional_scalar",
        ),
        // a count-permitting occurrence in a SINGLE-choice group is the record path's rest tail,
        // a different seam entirely.
        (format!("{GROUP}t = [ * kv ]\n"), "single_choice_rest_tail"),
    ] {
        assert!(
            run(&spec, tag, &[]).is_ok(),
            "`{}` must still generate, got: {:?}",
            spec.trim(),
            run(&spec, tag, &[]).err()
        );
    }

    // One message per problem: the three neighbouring refusals keep their own text and this guard
    // never joins in, because each of their inputs is a shape it does not see (a multi-entry arm,
    // or an inline group).
    for (spec, tag, needle) in [
        // MULTI-entry arm with `?` on a plain-group member: the record path's own refusal.
        (
            format!("{GROUP}t = [ x: uint // ? kv, c: uint ]\n"),
            "multi_entry_optional_group",
            "is an OPTIONAL (`?`) reference to the plain group `kv`",
        ),
        // MULTI-entry arm ending in a rest tail: the choice-arm placement refusal.
        (
            format!("{GROUP}t = [ x: uint // c: uint, * kv ]\n"),
            "multi_entry_rest_tail",
            "an open-array rest tail (`* t`) inside a group-choice arm",
        ),
        // an INLINE group entry is refused in entry position for EVERY marker including none, so
        // it must keep that message rather than be told about its `?` — in either rep.
        (
            "t = [ x: uint // ? (a: uint, b: uint) ]\n".to_owned(),
            "inline_group_arm",
            "in entry position is unsupported",
        ),
        (
            "t = { x: uint // + (a: uint, b: uint) }\n".to_owned(),
            "inline_group_arm_map",
            "in entry position is unsupported",
        ),
    ] {
        let msg = run(&spec, tag, &[]).expect_err(&format!("`{}` must reject", spec.trim()));
        assert!(
            msg.contains(needle),
            "the {tag} neighbour should keep its own message, got: {msg}"
        );
        assert!(
            !msg.contains("occurrence marker"),
            "the {tag} neighbour must not be double-reported by this guard, got: {msg}"
        );
    }
}

/// Fixed member keys on a struct-map record support only uint and text: the map-key write path and
/// (under `--preserve-encodings`) `key_encoding_field` implement nothing else, so a nint/float key
/// (`neg = { -1: uint }`) panicked generation. Reject it gracefully at parsing instead. Because
/// `group_entry_to_field_name` itself panics at parsing.rs:1278 on non-uint Type1 (arrow) member
/// keys, the key must be classified BEFORE field naming — which also converts the arrow-multi and
/// non-fixed-mixed field-naming panics into graceful rejections. Pins the messages, the two arrow
/// spellings, the preserve-encodings profile (which formerly panicked at a DIFFERENT site), and the
/// uint/text/table boundaries that must keep generating. The group-choice arm keeps its own message.
#[test]
fn unsupported_fixed_map_key_on_record_rejects_gracefully() {
    fn run_with(
        spec: &str,
        tag: &str,
        preserve: bool,
    ) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rec_map_key_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rec_map_key_unused",
        ];
        if preserve {
            args.push("--preserve-encodings=true");
        }
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        run_with(spec, tag, false)
    }

    // A nint/float fixed key on a record map must reject gracefully, naming the rule and pointing at
    // the uint/text keys and the table remedy (formerly `unsupported map key type` panic at
    // generation, or `key_encoding_field`'s `unimplemented!` under --preserve-encodings).
    for (spec, tag, rule) in [
        ("neg = { -1: uint }\n", "neg", "neg"),
        ("flt = { 1.5: uint }\n", "flt", "flt"),
        ("m = { -1: uint, 1: uint }\n", "mixed", "m"),
        ("m = { ? -1: uint }\n", "opt", "m"),
    ] {
        let msg = run(spec, tag)
            .expect_err("a nint/float fixed map key on a record must reject gracefully, not panic");
        assert!(
            msg.contains(&format!("rule `{rule}`"))
                && msg.contains("uint")
                && msg.contains("text")
                && msg.contains("table"),
            "rejection should name the rule, cite uint/text, and offer the table remedy, got: {msg}"
        );
    }

    // Arrow spellings that used to panic at field naming (parsing.rs:1278) BEFORE the key match
    // could run — classifying before naming converts them to graceful rejections.
    let arrow_kind = run("m = { -1 => uint, 1: uint }\n", "arrow_nint").expect_err(
        "a nint arrow key mixed into a record map used to panic at field naming; must reject",
    );
    assert!(
        arrow_kind.contains("rule `m`") && arrow_kind.contains("unsupported fixed map key"),
        "arrow nint key should get the unsupported-fixed-kind message (classified Fixed(Nint), \
         not NonFixed), got: {arrow_kind}"
    );
    // A non-fixed arrow key that is NOT the trailing entry is no longer a blanket rejection: it is
    // handled by the open struct-map rest-row front door (loose CBOR). A non-final placement like
    // `{ uint => tstr, 1: uint }` names the rest-row LAST-entry requirement instead of panicking.
    let arrow_nonfixed = run("m = { uint => tstr, 1: uint }\n", "arrow_nonfixed").expect_err(
        "a non-final non-fixed key used to panic at field naming; must reject gracefully",
    );
    assert!(
        arrow_nonfixed.contains("rule `m`") && arrow_nonfixed.contains("rest row"),
        "a non-final non-fixed key should get the open-map rest-row front-door message, got: {arrow_nonfixed}"
    );

    // Under --preserve-encodings the record path formerly panicked at a DIFFERENT site
    // (`key_encoding_field`'s `unimplemented!`). The parse-time rejection must fire before it.
    run_with("neg = { -1: uint }\n", "neg_preserve", true)
        .expect_err("the parse-time rejection must fire before the preserve-encodings panic site");

    // Boundaries that must KEEP generating: supported fixed keys and the printed table remedy.
    run("m = { 1: uint }\n", "uint_ok").expect("a uint fixed map key must still generate");
    run("m = { \"a\": uint }\n", "text_ok").expect("a text fixed map key must still generate");
    run("m = { * nint => uint }\n", "table_remedy")
        .expect("the printed remedy — a nint-keyed table — must generate");

    // The group-choice arm stays graceful with its own arm-specific message.
    let arm = run("neg = { -1: uint // b: tstr }\n", "arm")
        .expect_err("a nint key in a map group-choice arm must reject gracefully");
    assert!(
        arm.contains("group-choice"),
        "the group-choice arm keeps its own arm-specific message, got: {arm}"
    );
}

/// A literal-key arrow entry `k => v` is the SAME wire entry as the colon spelling `k: v` (RFC 8610),
/// so a single-entry fixed-value arrow key routes to the record path instead of table-detecting into
/// a `ConceptualRustType::Fixed` domain (which panicked `for_rust_member`, intermediate/rust_type.rs, for
/// EVERY key kind — even uint/text). This pins that routing by asserting the arrow and colon
/// spellings generate a BYTE-IDENTICAL crate, and that once on the record path every unsupported kind
/// gets f49d862's graceful rejection. The Fixed-domain table detection is gone; a decay back to it
/// would re-introduce the panic.
#[test]
fn fixed_key_arrow_single_entry_routes_to_record_path() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_arrow_route_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "arrow_route_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }
    fn gen_out(spec: &str, tag: &str) -> std::collections::BTreeMap<String, String> {
        run(spec, tag).unwrap_or_else(|e| panic!("`{spec}` must generate, got: {e}"))
    }

    // Byte-exact convergence: the arrow spelling of a literal key produces the identical crate as the
    // colon spelling (single-entry, quoted-text, optional, and the multi-field mixed form — the
    // multi-field arrow was a parsing.rs:1278 field-naming panic before the Type1 TextValue case).
    assert_eq!(
        gen_out("m = { 1 => uint }\n", "u_arrow"),
        gen_out("m = { 1: uint }\n", "u_colon"),
        "a single uint arrow key must converge with the colon spelling"
    );
    assert_eq!(
        gen_out("m = { \"a\" => uint }\n", "t_arrow"),
        gen_out("m = { \"a\": uint }\n", "t_colon"),
        "a single text arrow key must converge with the colon spelling"
    );
    assert_eq!(
        gen_out("m = { ? 1 => uint }\n", "opt_arrow"),
        gen_out("m = { ? 1: uint }\n", "opt_colon"),
        "an optional single arrow key must converge with the colon spelling"
    );
    assert_eq!(
        gen_out("m = { \"a\" => uint, 1: uint }\n", "multi_arrow"),
        gen_out("m = { \"a\": uint, 1: uint }\n", "multi_colon"),
        "a multi-field mixed arrow/colon record must converge with the all-colon spelling"
    );

    // Graceful rejections once on the record path: nint/float get the unsupported-fixed-kind message,
    // bool mentions its value, a zero-permitting occurrence gets f18d764's occurrence message, and an
    // aliased literal domain (`one = 1`) rejects (formerly a for_rust_member panic).
    let nint =
        run("m = { -1 => uint }\n", "nint").expect_err("nint arrow key must reject gracefully");
    assert!(
        nint.contains("unsupported fixed map key"),
        "nint arrow key should get the unsupported-fixed-kind message, got: {nint}"
    );
    let flt =
        run("m = { 1.5 => uint }\n", "flt").expect_err("float arrow key must reject gracefully");
    assert!(
        flt.contains("unsupported fixed map key"),
        "float arrow key should get the unsupported-fixed-kind message, got: {flt}"
    );
    // The float message must NOT advertise the table `{ * k => v }` remedy: a float table key
    // domain is itself rejected (no total order), so that advice would dead-end in a second
    // rejection. It must say floats cannot key a map instead.
    assert!(
        !flt.contains("in its own rule") && flt.contains("either form"),
        "float key remedy must not point at the (also-rejected) table form, got: {flt}"
    );
    let boolean =
        run("m = { true => uint }\n", "bool").expect_err("bool arrow key must reject gracefully");
    assert!(
        boolean.contains("Bool"),
        "bool arrow key should mention Bool in its message, got: {boolean}"
    );
    let star = run("m = { * 1 => uint }\n", "star").expect_err(
        "a zero-permitting occurrence on a routed arrow key must reject (silent narrowing is wrong)",
    );
    assert!(
        star.contains("zero-permitting occurrence"),
        "a `*` arrow key should get the zero-permitting occurrence message, got: {star}"
    );
    // An aliased literal key `one = 1` resolves through the alias to a Fixed domain, so it diverts to
    // the record path where it is classified NonFixed (a Type1 typename key). As the SOLE entry it is
    // now seen by the open struct-map front door (loose CBOR) as a lone non-fixed row — rejected
    // because an open struct needs a fixed key before its rest row (a bare `* k => v` is a table). Any
    // record-path rejection message is acceptable; we pin the fixed-key-prefix one that fires.
    let aliased = run("one = 1\nm = { one => uint }\n", "aliased")
        .expect_err("an aliased literal arrow key domain must reject gracefully, not panic");
    assert!(
        aliased.contains("rule `m`") && aliased.contains("fixed key"),
        "an aliased literal arrow key hits the open-map front door as a lone non-fixed row, got: {aliased}"
    );

    // Boundaries that must KEEP generating: a multi-entry fixed-key arrow map, ordinary tables, and a
    // parenthesized table (the parenthesized-FIXED case `{ * (1 => uint) }` instead falls through to
    // a graceful record-path rejection, tested below).
    run("m = { 1 => uint, 2 => tstr }\n", "two_fixed")
        .expect("a multi-entry fixed-key arrow map must keep generating");
    run("m = { * uint => tstr }\n", "table_uint").expect("a uint-keyed table must keep generating");
    run("m = { * nint => uint }\n", "table_nint").expect("a nint-keyed table must keep generating");
    run("m = { * (int => tstr) }\n", "table_paren")
        .expect("a parenthesized non-fixed table must keep generating");
    // `{ * (1 => uint) }` — a parenthesized FIXED-value key — must fall through to a graceful record
    // rejection (zero-permitting occurrence), not build a Fixed-domain table that panics.
    run("m = { * (1 => uint) }\n", "table_paren_fixed").expect_err(
        "a parenthesized fixed-value key must reject gracefully, not build a Fixed table",
    );
}

/// A no-occurrence type-domain arrow entry — `{ tstr => uint }`, key non-literal — is rejected
/// gracefully: per RFC 8610 an entry with NO occurrence indicator occurs EXACTLY ONCE, but table
/// detection routed it to the same 0..N `BTreeMap` as `{ * tstr => uint }` (generation was
/// byte-identical, verified by diff), silently WIDENING the occurrence — the generated decoder
/// wrongly accepted e.g. the empty map (the certified over-acceptance instance `8200a0`, formerly
/// pinned on `contain.map-key.memberkey.type1.tstr_arrow_nooccur`). This pins the rejection, that
/// the message carries the exactly-once rationale and the `*` remedy, that the remedy generates,
/// and the boundaries the guard must preserve:
///   - fixed/literal arrow keys (`{ 1 => uint }`, `{ "a" => uint }`) still route to the record
///     path (RFC-equal to the colon spelling — the existing arrow-routing test pins equality);
///   - the parenthesized table `{ * (tstr => uint) }` stays supported (the occurrence lives on
///     the inline group; the inner entry's missing occur is NOT the semantic occurrence);
///   - the occur-less parenthesized form `{ (tstr => uint) }` splices into the plain arm and is
///     rejected there (pure grouping — semantically identical to the unparenthesized spelling);
///   - a NESTED anonymous no-occur table (`a = [{ tstr => uint }]`) rejects through the same
///     seam (no rule name to cite — the message still carries the rationale and remedy);
///   - count-permitting markers (`+` / `?` / `n*m`) are OUT of scope here and keep generating
///     (they also table-detect to an unbounded 0..N map today — a separate ledgered finding, the
///     widened-occurrence-marker table class in cddl-matrix/ROADMAP.md § findings, enumerated as the
///     matrix rows `contain.occurrence-target.memberkey.type1.{plus,optional,bounded}_table` with
///     certified `class="over-acceptance"` decode-catalog pins for the out-of-window maps; these
///     legs and those pins flip loudly together when that finding is fixed).
#[test]
fn no_occurrence_arrow_map_entry_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_nooccur_arrow_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "nooccur_arrow_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // The finding's shape: Err with the exactly-once rationale, the rule name, and the `*` remedy.
    let msg = run("m = { tstr => uint }\n", "nooccur").expect_err(
        "a no-occurrence type-domain arrow entry must reject (silent 0..N widening is wrong)",
    );
    assert!(
        msg.contains("exactly once") && msg.contains("rule `m`"),
        "rejection should carry the exactly-once rationale and name the rule, got: {msg}"
    );
    assert!(
        msg.contains("{ * tstr => uint }"),
        "rejection should advertise the explicit `*` table spelling, got: {msg}"
    );

    // Remedy-works: the advertised `*` spelling generates.
    run("m = { * tstr => uint }\n", "star")
        .expect("the advertised `* k => v` remedy must generate");

    // Fixed/literal arrow keys keep routing to the record path.
    run("m = { 1 => uint }\n", "fixed_uint")
        .expect("a fixed uint arrow key is a 1-field struct and must keep generating");
    run("m = { \"a\" => uint }\n", "fixed_text")
        .expect("a fixed text arrow key is a 1-field struct and must keep generating");

    // Parenthesized boundary: `*` on the inline group is the semantic occurrence — supported;
    // the occur-less parenthesized form is pure grouping and rejects like the plain spelling.
    run("m = { * (tstr => uint) }\n", "paren_star").expect(
        "`{ * (k => v) }` carries the occurrence on the inline group and must keep generating",
    );
    run("m = { (tstr => uint) }\n", "paren_nooccur").expect_err(
        "`{ (k => v) }` is pure grouping around an exactly-once entry and must reject like `{ k => v }`",
    );

    // Nested anonymous position rejects through the same seam (no rule name available).
    let nested = run("a = [{ tstr => uint }]\n", "nested")
        .expect_err("a nested anonymous no-occur table must reject through the same seam");
    assert!(
        nested.contains("exactly once"),
        "nested rejection should still carry the exactly-once rationale, got: {nested}"
    );

    // WI-2 (two-type `{+ k => v}`): the count-permitting markers on a table entry are now HONORED or
    // REJECTED — this closes the widened-occurrence-marker over-acceptance class:
    // - `+` / `1*` generate a NonEmptyMap (non-empty table, enforced via the single TryFrom door);
    // - `?` / `n*m` / `*n` (n≥2) / `0*n` reject gracefully (a real bounded cardinality this phase does
    //   not honor — silently widening it to 0..N was the bug).
    run("m = { + tstr => uint }\n", "plus")
        .expect("`+` on a table entry now generates a NonEmptyMap");
    let opt = run("m = { ? tstr => uint }\n", "opt")
        .expect_err("`?` on a table entry now rejects (bounded cardinality not honored)");
    assert!(
        opt.contains("bounded occurrence marker") && opt.contains("rule `m`"),
        "`?` rejection should carry the bounded-marker rationale and name the rule, got: {opt}"
    );
    assert!(
        opt.contains("{ * tstr => uint }") && opt.contains("{ + tstr => uint }"),
        "`?` rejection should advertise both the `*` (unbounded) and `+` (non-empty) remedies, got: {opt}"
    );
    let bounded = run("m = { 2*3 tstr => uint }\n", "bounded")
        .expect_err("`n*m` on a table entry now rejects (bounded cardinality not honored)");
    assert!(
        bounded.contains("bounded occurrence marker"),
        "`2*3` rejection should carry the bounded-marker rationale, got: {bounded}"
    );
}

/// The no-occurrence arrow-entry rejection (`5ef7ed0`) must reach maps instantiated through a
/// GENERIC ARG — a guard-coverage pin. Before that rejection existed, `g<{ int .ne 0 => uint }>`
/// silently generated an unbounded `BTreeMap<i64, u64>` (the exactly-once entry widened to 0..N —
/// the over-acceptance class the guard closes), and nothing pins the generic-instantiation parse
/// path specifically: the sibling `no_occurrence_arrow_map_entry_rejects_gracefully` covers plain
/// rules, parenthesized groups, and nested anonymous maps, but not this reach. This test pins it
/// so the reach cannot silently regress back to widening. (Surfaced by the recombination fuzzer's
/// layer-2 vacuity guard when its `outer=generic_arg inner=map_key filler=ctl.ne.zero` composition
/// stopped reaching layer 2 — that retired `LAYER2_KNOWN_BAD` entry's minter gap is now closed and
/// pinned by `emit_tests_bounded_map_key_execute`.)
#[test]
fn generic_arg_no_occurrence_table_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_genarg_nooccur_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "genarg_nooccur_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // The escape's shape: a no-occurrence arrow table as a generic argument must reject with the
    // same exactly-once rationale and explicit-occurrence remedies as the plain spelling.
    let msg = run("g<a0> = [a0]\nx = g<{ int .ne 0 => uint }>\n", "nooccur").expect_err(
        "a no-occurrence arrow entry inside a generic arg must reject (the baseline silently \
         widened it to an unbounded table)",
    );
    assert!(
        msg.contains("exactly once"),
        "generic-arg rejection should carry the exactly-once rationale, got: {msg}"
    );
    assert!(
        msg.contains("{ * int .ne 0 => uint }") && msg.contains("{ + int .ne 0 => uint }"),
        "generic-arg rejection should advertise the `*` and `+` remedies, got: {msg}"
    );

    // Remedy-works boundaries: both advertised spellings generate through the same generic arg.
    run("g<a0> = [a0]\nx = g<{ * int .ne 0 => uint }>\n", "star")
        .expect("the advertised `*` table spelling must generate through a generic arg");
    run("g<a0> = [a0]\nx = g<{ + int .ne 0 => uint }>\n", "plus")
        .expect("the advertised `+` non-empty table spelling must generate through a generic arg");
}

/// Incremental choice extension (`/=` type-choice, `//=` group-choice) that EXTENDS an
/// already-defined identifier is rejected gracefully: `parse_rule` re-registers the identifier on
/// each statement, so the LAST definition wins and every earlier arm is silently dropped
/// (`a = int` / `a /= tstr` generated a `tstr`-only type, discarding the `int` base arm — a
/// decoder that rejects spec-valid CBOR, invisible to round-trip tests). This pins the graceful
/// rejection, that the message names the operator and an actionable remedy, that the advertised
/// remedy spellings actually generate, and the boundary the guard must preserve: a LONE alternate
/// rule whose identifier is its FIRST definition (the shelley precedent — valid CDDL, equivalent
/// to `=`) must keep generating.
#[test]
fn incremental_choice_extension_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_incr_choice_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "incr_choice_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // `/=` extending an already-defined type: Err naming `/=` and the fold remedy.
    let type_ext = run("a = int\na /= tstr\n", "type_ext").expect_err(
        "`/=` extending an already-defined type must reject (silent arm-drop to the last is wrong)",
    );
    assert!(
        type_ext.contains("/=") && type_ext.contains("rule `a`"),
        "type-choice extension rejection should name the operator and the rule, got: {type_ext}"
    );
    assert!(
        type_ext.contains("a = int / tstr") || type_ext.contains("<arm1> / <arm2>"),
        "type-choice extension remedy should advertise folding into one type-choice rule, got: {type_ext}"
    );

    // `//=` extending an already-defined group: the analogue.
    let group_ext = run("tcpopts = (1: int)\ntcpopts //= (2: tstr)\n", "group_ext").expect_err(
        "`//=` extending an already-defined group must reject (silent arm-drop to the last is wrong)",
    );
    assert!(
        group_ext.contains("//=") && group_ext.contains("rule `tcpopts`"),
        "group-choice extension rejection should name the operator and the rule, got: {group_ext}"
    );

    // Boundary: a LONE `/=` rule (first definition of `b`) is valid CDDL and must keep generating.
    run("b /= tstr\n", "lone_type_alt")
        .expect("a lone `/=` rule (initial definition) must keep generating (shelley precedent)");

    // Remedy-works: the advertised folded/restructured spellings generate ok.
    run("a = int / tstr\n", "type_fold")
        .expect("the folded type-choice remedy `a = int / tstr` must generate");
    run(
        "tcpopts_a = (1: int)\ntcpopts_b = (2: tstr)\nt = [ tcpopts_a // tcpopts_b ]\n",
        "group_usesite",
    )
    .expect("the use-site group-choice remedy `t = [ grpA // grpB ]` must generate");
}

/// A bareword map/array key that is a Rust keyword (`{ if: uint }`, `[if: uint]`, `{ true: uint }`)
/// emits a struct field literally named by the keyword — invalid Rust, formerly caught only by the
/// rustfmt gate as a "generator bug". Reject it at parse time with the `@name` remedy. Honoring
/// `@name` on bareword keys (formerly silently dropped) is what makes the remedy work. Pins the
/// message (rule + field + `@name`), that the remedy actually generates the renamed field, and that
/// ordinary barewords still generate.
#[test]
fn bareword_keyword_field_name_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_kw_field_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "kw_field_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // A keyword field name rejects gracefully, naming the rule, the offending field, and the @name
    // remedy — in BOTH map and array representations (the array shape `[if: uint]` is equally hit).
    for (spec, tag, rule, field) in [
        ("kw = { if: uint }\n", "map_if", "kw", "if"),
        ("kw = { true: uint }\n", "map_true", "kw", "true"),
        ("a = [if: uint]\n", "arr_if", "a", "if"),
        // A bareword `If` snake_cases to `if`, so the CONVERTED (emitted) form must be checked.
        ("kw = { If: uint }\n", "map_Cap_if", "kw", "if"),
    ] {
        let msg = run(spec, tag)
            .expect_err("a keyword field name must reject gracefully, not reach the rustfmt gate");
        assert!(
            msg.contains(&format!("rule `{rule}`"))
                && msg.contains(&format!("`{field}`"))
                && msg.contains("@name"),
            "rejection should name the rule, the offending field, and the @name remedy, got: {msg}"
        );
    }

    // The verified remedy generates: a `; @name branch` directive renames the field to `branch`
    // (the CBOR wire key stays the bareword `if`), and the generated struct must contain `branch`.
    // The struct now lives in the `generated/mod.rs` scope root, not the thin seed-once `lib.rs`.
    let files = run("kw = { if: uint, ; @name branch\n}\n", "remedy")
        .expect("the @name remedy must generate a valid crate");
    let lib = files
        .iter()
        .find(|(name, _)| name.contains("generated/mod.rs"))
        .map(|(_, src)| src.clone())
        .unwrap_or_default();
    assert!(
        lib.contains("branch"),
        "the @name remedy must emit a field named `branch`, got generated/mod.rs without it"
    );

    // Boundary: an ordinary bareword field still generates.
    run("m = { foo: uint }\n", "ordinary")
        .expect("an ordinary bareword field must keep generating");
}

/// A field named by one of the fixed locals the generated serialization bodies bind
/// (`parsing::GENERATED_LOCAL_RESERVED` — `raw`, `len`, `read`, …) used to generate at exit 0 and
/// emit a crate that does not compile: the field's local shadows the emitter's, and the failure
/// surfaced two build steps from the CDDL line that caused it. It is now a parse-time graceful
/// rejection naming the rule, the field, the reserved word and the `@name` remedy.
///
/// Asserted UNIFORMLY across profiles, matching the registry's own rule: `tag` compiles under the
/// default profile and breaks under `--preserve-encodings`, so a per-profile refusal would hand back
/// a spec one flag away from an uncompilable crate.
#[test]
fn generated_local_field_name_rejects_gracefully() {
    for (tag, spec, rule, field, word) in [
        // `raw` — the deserializer parameter, both reps.
        (
            "genloc_raw_arr",
            "a = [pre: uint, raw: bytes]\n",
            "a",
            "raw",
            "raw",
        ),
        (
            "genloc_raw_map",
            "m = { 1: uint, raw: bytes }\n",
            "m",
            "raw",
            "raw",
        ),
        // case-converted: `Raw` snake_cases to the reserved `raw`.
        (
            "genloc_Raw_arr",
            "a = [pre: uint, Raw: bytes]\n",
            "a",
            "raw",
            "raw",
        ),
        // `len` — the array/map length read.
        (
            "genloc_len_arr",
            "a = [pre: uint, len: bytes]\n",
            "a",
            "len",
            "len",
        ),
        (
            "genloc_len_map",
            "m = { 1: uint, len: bytes }\n",
            "m",
            "len",
            "len",
        ),
    ] {
        for extra in [
            &[][..],
            &["--preserve-encodings", "true"][..],
            &["--preserve-encodings", "true", "--canonical-form", "true"][..],
        ] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains(&format!("rule `{rule}`"))
                    && msg.contains(&format!("field `{field}`"))
                    && msg.contains(&format!("`{word}`"))
                    && msg.contains("reserved name")
                    && msg.contains("@name"),
                "rejection should name the rule, the field, the reserved word and the @name remedy \
                 ({tag}, {extra:?}), got: {msg}"
            );
        }
    }
    // The remedy generates, and it is the RESOLVED name that is judged: `; @name payload` on a
    // `raw` field renames the Rust field while the CBOR wire key stays the bareword text.
    let files = {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_genloc_remedy_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, "m = { raw: bytes, ; @name payload\n}\n").unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "genloc_remedy_unused",
        ]);
        let out = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        out.expect("the @name remedy must generate a valid crate")
    };
    let ser = files
        .iter()
        .find(|(name, _)| name.contains("generated/serialization.rs"))
        .map(|(_, src)| src.clone())
        .unwrap_or_default();
    assert!(
        ser.contains("write_text(\"raw\")") && ser.contains("payload"),
        "the @name remedy must rename the Rust field while keeping the `raw` wire key, got: {ser}"
    );

    // The ACCEPT side of the scope rule, and the reason it exists: `tag` is reserved only inside a
    // `#6.n(…)` record (where the deserializer emits the tag read). The `tag: 0` group-choice
    // discriminant — `tests/core` / `tests/preserve-encodings` use it, as do real specs — is an
    // untagged array record and must keep generating under every profile. A uniform-across-reps
    // refusal would have broken it, which is why the registry is scoped by shape.
    for (tag, spec) in [
        (
            "tag_discriminant",
            "a = [x: uint, tag: 0 // y: text, tag: 1]\n",
        ),
        ("tag_plain_array", "a = [pre: uint, tag: bytes]\n"),
        ("tag_plain_map", "m = { 1: uint, tag: bytes }\n"),
        // map-rep-only names in an array record: the emitter binds no such local there
        ("read_array", "a = [pre: uint, read: bytes]\n"),
        ("text_key_array", "a = [pre: uint, text_key: bytes]\n"),
    ] {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let path = std::env::temp_dir()
                .join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
            std::fs::write(&path, spec).unwrap();
            let mut argv = vec![
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "genloc_scope_unused",
            ];
            argv.extend_from_slice(extra);
            let cli = Cli::parse_from(argv);
            let result = crate::api::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            assert!(
                result.is_ok(),
                "{tag} ({extra:?}): must still generate — the reserved-name scope is too wide:\n{spec}"
            );
        }
    }
}

/// The PAIRWISE half of the same class: two fields that are individually fine but stand in an
/// `<f>` / `<f>_encoding` relation collide once `--preserve-encodings` mints the per-field encoding
/// companions. Refused uniformly (the default profile compiles only because it mints no companions),
/// with the accept side pinned so the check cannot quietly over-fire.
#[test]
fn encoding_companion_field_collision_rejects_gracefully() {
    for (tag, spec, a, b) in [
        // the field local shadows the value-encoding companion — both reps
        (
            "enc_pair_arr",
            "a = [foo: bytes, foo_encoding: uint]\n",
            "foo",
            "foo_encoding",
        ),
        (
            "enc_pair_map",
            "m = { 1: uint, foo: bytes, foo_encoding: uint }\n",
            "foo",
            "foo_encoding",
        ),
        // map rep only: two fields mint the same `<f>_key_encoding`
        (
            "key_pair_map",
            "m = { foo: bytes, foo_key: uint }\n",
            "foo",
            "foo_key",
        ),
        // map rep only: the field local shadows the KEY-encoding companion
        (
            "keyenc_map",
            "m = { foo: bytes, foo_key_encoding: uint }\n",
            "foo",
            "foo_key_encoding",
        ),
    ] {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(tag, spec, extra);
            assert!(
                msg.contains(&format!("`{a}`"))
                    && msg.contains(&format!("`{b}`"))
                    && msg.contains("--preserve-encodings")
                    && msg.contains("@name"),
                "rejection should name both fields, the flag that mints the companion and the \
                 @name remedy ({tag}, {extra:?}), got: {msg}"
            );
        }
    }
    // Accept side — the shapes that mint no colliding companion must still generate.
    for (tag, spec) in [
        // a lone `<x>_encoding` with no `<x>` sibling
        ("lone_encoding", "a = [foo_encoding: uint]\n"),
        // `_key` / `_key_encoding` pairs are map-only (array records mint no key encodings)
        ("array_key_pair", "a = [foo: bytes, foo_key: uint]\n"),
        ("array_keyenc", "a = [foo: bytes, foo_key_encoding: uint]\n"),
    ] {
        let path =
            std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "enc_pair_unused",
            "--preserve-encodings",
            "true",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "{tag}: must still generate — the pairwise check is over-firing:\n{spec}"
        );
    }
}

/// Environment variable that turns a run of [`robustness_out_of_process_generation_helper`] into a
/// one-shot generator invocation over the named input. Set only by
/// [`out_of_process_catalog_outcome`]; absent in every ordinary test run, where the helper is a
/// no-op.
const ROBUSTNESS_SUBPROCESS_INPUT: &str = "CDDL_CODEGEN_ROBUSTNESS_SUBPROCESS_INPUT";

/// Test-only switch for [`robustness_out_of_process_generation_helper`]. The abort-classification
/// self-test sets it for one child; normal test discovery never does, so the helper stays inert.
const ROBUSTNESS_SUBPROCESS_ABORT: &str = "CDDL_CODEGEN_ROBUSTNESS_SUBPROCESS_ABORT";

/// Printed by the helper before it exits, and required by the parent. Without it a child that
/// matched NO test would exit 0 and read as an `ok` row — the one way this lane could silently
/// stop testing anything.
const ROBUSTNESS_SUBPROCESS_SENTINEL: &str = "cddl-codegen robustness subprocess ran";

/// The out-of-process half of [`input_robustness_catalog`]: generation for ONE input, in a process
/// of its own, reporting the outcome through the exit code so that a crash which cannot unwind is
/// observable as a signal instead of killing the catalog.
///
/// Implemented as a `#[test]` that self-spawns rather than a nested `cargo run`: the running test
/// binary is already built, so the parent pays one `fork`/`exec` and no cargo invocation. Without
/// the environment variable it returns immediately, so an ordinary `cargo test` run sees it as a
/// trivially-passing test.
#[test]
fn robustness_out_of_process_generation_helper() {
    let Ok(input) = std::env::var(ROBUSTNESS_SUBPROCESS_INPUT) else {
        return;
    };
    if std::env::var_os(ROBUSTNESS_SUBPROCESS_ABORT).is_some() {
        std::process::abort();
    }
    // The child's whole job is the exit code; a panic's own output would only interleave with the
    // parent's test output, and an abort's runtime message is likewise noise here.
    std::panic::set_hook(Box::new(|_| {}));
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        &input,
        "--output",
        "robustness_unused",
    ]);
    let code = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::api::generated_strings(&cli)
    })) {
        Ok(Ok(_)) => 0,
        Ok(Err(_)) => 1,
        Err(_) => 2,
    };
    println!("{ROBUSTNESS_SUBPROCESS_SENTINEL}: {input}");
    // `exit` rather than a return: libtest would otherwise print its own summary and decide the
    // process's exit code, which is the value this lane reads.
    std::process::exit(code);
}

/// Run one catalog input through a fresh process and classify how that process ended.
///
/// The four labels are the same vocabulary the in-process lane uses, plus `ABORTED (signal <n>)`
/// for a death by signal — the outcome `catch_unwind` structurally cannot report.
fn out_of_process_catalog_outcome(path: &std::path::Path) -> String {
    out_of_process_catalog_outcome_with_abort(path, false)
}

/// Shared child-status classifier. The abort switch exists only for the Unix self-test; catalog
/// rows always invoke this through [`out_of_process_catalog_outcome`] with it disabled.
fn out_of_process_catalog_outcome_with_abort(path: &std::path::Path, abort: bool) -> String {
    let exe = std::env::current_exe().expect("the running test binary must have a path");
    let mut command = std::process::Command::new(&exe);
    command
        .args([
            "--exact",
            "tests::robustness_tests::robustness_out_of_process_generation_helper",
            "--nocapture",
            "--test-threads=1",
        ])
        .env(ROBUSTNESS_SUBPROCESS_INPUT, path);
    if abort {
        command.env(ROBUSTNESS_SUBPROCESS_ABORT, "1");
    } else {
        // Never inherit this test-only switch from an enclosing test invocation into real catalog
        // rows, where it would turn every child into a synthetic abort.
        command.env_remove(ROBUSTNESS_SUBPROCESS_ABORT);
    }
    let output = command
        .output()
        .unwrap_or_else(|e| panic!("could not spawn {exe:?} for {path:?}: {e}"));
    let stdout = String::from_utf8_lossy(&output.stdout);
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = output.status.signal() {
            return format!("ABORTED (signal {signal})");
        }
    }
    assert!(
        stdout.contains(ROBUSTNESS_SUBPROCESS_SENTINEL),
        "the out-of-process lane did not run the helper for {path:?} (exit {status:?}); the test \
         filter or the module path is stale, and every row it produces would be meaningless.\n\
         stdout:\n{stdout}\nstderr:\n{stderr}",
        status = output.status.code(),
        stderr = String::from_utf8_lossy(&output.stderr),
    );
    match output.status.code() {
        Some(0) => "ok".to_owned(),
        Some(1) => "error (graceful)".to_owned(),
        Some(2) => "PANIC".to_owned(),
        other => panic!(
            "unexpected exit {other:?} from the out-of-process lane for {path:?}\nstdout:\n{stdout}"
        ),
    }
}

/// A synthetic non-unwinding child must become the parent-owned fourth catalog label instead of
/// terminating this test binary. Its exact helper filter is the ordinary one, which retains the
/// sentinel check for every non-signal child exit.
#[cfg(unix)]
#[test]
fn robustness_out_of_process_abort_is_classified() {
    let outcome = out_of_process_catalog_outcome_with_abort(
        std::path::Path::new("tests/robustness/empty.cddl"),
        true,
    );
    let signal = outcome
        .strip_prefix("ABORTED (signal ")
        .and_then(|rest| rest.strip_suffix(')'))
        .and_then(|signal| signal.parse::<i32>().ok());
    assert!(
        matches!(signal, Some(signal) if signal > 0),
        "abort helper must be classified as its actual Unix signal, got {outcome:?}"
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
        "# generator outcome per malformed/edge input\n# Every row runs in a fresh child process, so a non-unwinding crash is classified as `ABORTED (signal n)`\n# instead of killing this catalog. A NEW panic is a regression: malformed input must error gracefully. A\n# committed PANIC entry is a tracked-known rejection (see the fixture's comments); flipping it to `error\n# (graceful)` is a fix. The parent requires a child sentinel before accepting any ordinary exit status.\n\n",
    );
    for path in &inputs {
        let name = path.file_stem().unwrap().to_str().unwrap();
        let label = out_of_process_catalog_outcome(path);
        catalog.push_str(&format!("{name:26} {label}\n"));
    }

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/robustness/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}

/// Generate `spec` in-process and return the file map, asserting the run succeeded.
fn expect_generates(
    tag: &str,
    spec: &str,
    extra: &[&str],
) -> std::collections::BTreeMap<String, String> {
    let path = std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
    std::fs::write(&path, spec).unwrap();
    let mut argv = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "recursion_boundary_unused",
    ];
    argv.extend_from_slice(extra);
    let result = crate::api::generated_strings(&Cli::parse_from(argv));
    std::fs::remove_file(&path).ok();
    result.unwrap_or_else(|e| panic!("{tag}: spec must generate, got: {e}\n{spec}"))
}

/// The recursive-type boundary's REFUSAL half: a cycle whose emitted Rust cannot compile, and for
/// which no repair is in reach, is a graceful `Err` naming the cycle — never an exit-0 crate that
/// fails `cargo check`, and never a signal death.
///
/// Two classes, with different rustc errors and different honest remedies (see
/// `crate::recursion_boundary`): NOMINAL types containing one another with no heap indirection
/// (E0072 — including the `Option` spelling, which is exactly the remedy the docs used to
/// recommend), and an ALIAS cycle with no named collection in it to nominalize (E0391).
///
/// Every vector is asserted under BOTH rule orderings: the cycle's identity is a canonical property
/// of the SCC, so permuting the spec's rules must not change one byte of the message. That is the
/// property `dep_graph`'s traversal-order-dependent back edge could never have provided, and it is
/// why the boundary is its own classifier rather than a promotion of that notice.
#[test]
fn recursive_type_boundary_refuses_uncompilable_cycles() {
    // (tag, spec, permuted spec, substrings the message must carry)
    let vectors: &[(&str, &str, &str, &[&str])] = &[
        (
            "nominal_pair",
            "a = [b]\nb = [a]\n",
            "b = [a]\na = [b]\n",
            &[
                "recursive rule cycle over `a`, `b`",
                "E0072",
                "`a` field `b` (type `b`)",
                "`b` field `a` (type `a`)",
                "no directive boxes a member",
            ],
        ),
        (
            "self_record",
            "foo = [foo]\n",
            "foo = [foo]\n",
            &[
                "recursive rule cycle over `foo`",
                "E0072",
                "`foo` field `foo`",
            ],
        ),
        (
            "optional_member",
            "a = { ? next: a }\n",
            "a = { ? next: a }\n",
            &[
                "E0072",
                "`a` field `next`",
                // the falsified docs remedy, stated in the message so it cannot be re-derived
                "an `Option` stores its payload inline",
            ],
        ),
        (
            "pure_alias",
            "x = y\ny = x\n",
            "y = x\nx = y\n",
            &[
                "E0391",
                "transparent `pub type` alias",
                "No rule in this cycle names a collection",
            ],
        ),
    ];
    for (tag, spec, permuted, expected) in vectors {
        let msg = expect_graceful_rejection(&format!("recursion_refuse_{tag}"), spec, &[]);
        for needle in *expected {
            assert!(
                msg.contains(needle),
                "{tag}: refusal must carry {needle:?}, got: {msg}"
            );
        }
        let permuted_msg =
            expect_graceful_rejection(&format!("recursion_refuse_{tag}_permuted"), permuted, &[]);
        assert_eq!(
            msg, permuted_msg,
            "{tag}: the refusal must be identical under a permuted rule order — the cycle's \
             identity is the SCC, not a DFS back edge"
        );
    }
}

/// The recursive-type boundary's REPAIR half: an alias-expansion cycle with a named collection in it
/// is auto-`@newtype`d, and the result is BYTE-IDENTICAL to what the same spec with the directive
/// written by hand produces.
///
/// Byte identity is the strongest available statement that the repair reuses the directive's
/// machinery rather than reimplementing it — every downstream surface (wasm classes, encoding
/// sidecars, emit-tests minting) is whatever `; @newtype` already gives, because it IS `; @newtype`.
/// Asserted under both wasm modes, since the wasm face is where a second implementation would most
/// visibly diverge.
#[test]
fn recursive_alias_cycle_auto_newtype_matches_the_hand_written_directive() {
    // (tag, auto spec, the same spec with the directive spelled out)
    let vectors = [
        // the shape that used to ABORT the tool: a self-referential named collection plus a holder
        (
            "holder",
            "x = [* x]\nhold = [a: x]\n",
            "x = [* x] ; @newtype\nhold = [a: x]\n",
        ),
        (
            "standalone",
            "foos = [* foos]\n",
            "foos = [* foos] ; @newtype\n",
        ),
        ("plus", "foos = [+ foos]\n", "foos = [+ foos] ; @newtype\n"),
        (
            "map",
            "mdmap = {* text => mdmap}\n",
            "mdmap = {* text => mdmap} ; @newtype\n",
        ),
        // the alias HOP: only `x` names a collection, so only `x` is nominalized
        (
            "alias_hop",
            "y = x\nx = [* y]\n",
            "y = x\nx = [* y] ; @newtype\n",
        ),
        // Same alias hop entered through the alias-first spelling: its source edge is stripped from
        // the first-pass alias base, so auto-`@newtype` must reconstruct it before either face names
        // the collection.
        (
            "alias_first_hop",
            "hop_alias = hop_arr\nhop_arr = [* hop_alias]\n",
            "hop_alias = hop_arr\nhop_arr = [* hop_alias] ; @newtype\n",
        ),
        // two collection-backed members: BOTH are nominalized, because the repair set is a property
        // of the cycle rather than of whichever member a traversal reached first
        (
            "cross_rule",
            "m = {* text => a}\na = [* m]\n",
            "m = {* text => a} ; @newtype\na = [* m] ; @newtype\n",
        ),
    ];
    for wasm in ["--wasm=false", "--wasm=true"] {
        for (tag, auto, hand) in vectors {
            let auto_files = expect_generates(&format!("recursion_auto_{tag}"), auto, &[wasm]);
            let hand_files = expect_generates(&format!("recursion_hand_{tag}"), hand, &[wasm]);
            assert_eq!(
                auto_files, hand_files,
                "{tag} ({wasm}): the auto-`@newtype` repair must emit exactly what the hand-written \
                 directive emits"
            );
        }
    }
    // Accept side: the SUPPORTED class must not be swept in. A cycle through a nominal node that
    // crosses heap indirection compiles today, so the boundary must leave it a transparent alias /
    // plain record — no wrapper minted, no refusal.
    for (tag, spec) in [
        ("tree", "tree = [value: uint, children: [* tree]]\n"),
        (
            "cycle_entry",
            "h = [mdmap]\nmd = mdmap / int\nmdmap = { * text => md }\n",
        ),
        (
            "union_keyed",
            "u_holder = [u_val]\nu_val = u_map / int / bytes / text\nu_map = { * u_val => u_val }\n",
        ),
    ] {
        let files = expect_generates(
            &format!("recursion_supported_{tag}"),
            spec,
            &["--wasm=false"],
        );
        let src = files.into_values().collect::<Vec<_>>().join("\n");
        assert!(
            !src.contains("pub struct Tree(") && !src.contains("pub struct Mdmap("),
            "{tag}: a supported cycle must be left alone by the boundary"
        );
    }
}

/// Message-identity pin for the DIRECT-claim leg of the loose `<Elem>List` family, from the plain
/// `[* bar]` side. The catalog above only records the `error (graceful)` LABEL; this asserts the
/// message is the per-kind one — it names the claimed ident, the plain use that mints the class, and
/// the two remedies — and that it fires BEFORE the generic `export.rs` duplicate-ident backstop that
/// used to own this class. Same asymmetry-closing shape as
/// `default_rest_row_loose_map_wrapper_ident_collision_rejects_gracefully` did on the map side.
/// Runs the committed `loose_builder_name_claimed_plain` fixture under DEFAULT (wasm) flags — the
/// whole family is a wasm-class-name concern and does not arise with `--wasm=false`.
#[test]
fn loose_builder_name_claimed_plain_message_names_ident_and_use() {
    let path = std::path::Path::new("tests/robustness/loose_builder_name_claimed_plain.cddl");
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "loose_builder_name_claimed_plain_unused",
    ]);
    let msg = crate::api::generated_strings(&cli)
        .map_err(|e| e.to_string())
        .expect_err("a plain-loose ident collision must reject gracefully");
    assert!(
        msg.contains("BarList")
            && msg.contains("a plain (`*`-occurrence) array use")
            && msg.contains("loose list wrapper"),
        "the per-kind leg must name the claimed ident, the minting use, and the list family, got: {msg}"
    );
    assert!(
        !msg.contains("duplicate top-level ident"),
        "the per-kind detector must fire BEFORE the generic duplicate-ident backstop, got: {msg}"
    );
}

/// The generic `export.rs` duplicate-ident backstop still has message coverage of its own. Every
/// loose-list claim that arrives through a RustType the IR scan can see is now the per-kind leg's
/// (the pin above), so the backstop needs a source the scan structurally cannot see: `@used_as_elem`
/// mints the loose `<Elem>List` wasm class from a TAG on the element, with no `[* elem]` RustType
/// anywhere in the IR to collect. Pinning it here keeps the backstop's text — which is also the
/// "this is a cddl-codegen bug" self-report for a collision no rule caused — from going unwitnessed.
#[test]
fn used_as_elem_wrapper_ident_collision_reaches_the_duplicate_ident_backstop() {
    const CDDL: &str = "bar = [x: uint] ; @used_as_elem\n\
                        bar_list = tstr\n\
                        holder = { y: bar }\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_used_as_elem_ident_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let result = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "used_as_elem_ident_unused",
        "--wasm=true",
    ]));
    std::fs::remove_file(&path).ok();
    let msg = result
        .map_err(|e| e.to_string())
        .expect_err("a `@used_as_elem` wrapper-ident collision must reject gracefully");
    assert!(
        msg.contains("duplicate top-level ident")
            && msg.contains("BarList")
            && msg.contains("wasm/src/generated/mod.rs"),
        "the duplicate-ident backstop must name the class, the colliding ident, and the file, got: {msg}"
    );
}

/// A map-representation field with an all-negative signed-int value window (`{ 0: -10..-3 }`) used
/// to `unreachable!()` during generation: the per-CBOR-sign-arm bound partition emitted a
/// `(None, None)` projection for the (empty) uint arm and treated it as a bounds check. This has no
/// execution surface once it panics, so it's pinned generation-side — the fixture generates under
/// both profiles AND the emitted deserializer carries the intended checks: an unconditional reject
/// on the uint arm (no non-negative value is in range) reporting the ORIGINAL window, plus the real
/// window check on the nint arm. String-level because a re-introduced panic or a silently-dropped
/// check can't be seen by any round-trip test that never reaches the excluded arm.
#[test]
fn sign_partition_map_rep_generates_and_checks() {
    fn gen_src(preserve: bool) -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_sign_map_rep_{}_{}.cddl",
            preserve,
            std::process::id()
        ));
        std::fs::write(&path, "m = { 0: -10 .. -3 }\n").unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "sign_map_rep_unused",
            "--preserve-encodings",
            if preserve { "true" } else { "false" },
        ]);
        let files = crate::api::generated_strings(&cli)
            .expect("map-rep all-negative value window must generate, not panic");
        std::fs::remove_file(&path).ok();
        files.into_values().collect::<Vec<_>>().join("\n")
    }

    for preserve in [false, true] {
        let src = gen_src(preserve);
        // uint arm: every non-negative value is out of range -> unconditional reject carrying the
        // ORIGINAL window (`min: Some(-10), max: Some(-3)`), not the empty projection.
        assert!(
            src.contains("if true")
                && src.contains("min: Some(-10)")
                && src.contains("max: Some(-3)"),
            "preserve={preserve}: expected an unconditional uint-arm reject reporting the original window"
        );
        // nint arm: the real two-sided window check on the decoded signed value.
        assert!(
            src.contains("x < -10 || x > -3"),
            "preserve={preserve}: expected the nint-arm window check"
        );
    }
}

/// `.size` over a signed `int` (`i = int .size 8`) must be a GRACEFUL rejection, not a supported
/// mapping: per the RFC author's clarification (cbor-wg/cddl#32), a control distributes over
/// `int = uint / nint` and an undefined application is a per-value NON-match, so `int .size N`
/// matches exactly the `uint .size N` window — the historical `i{8N}` mapping accepted negatives
/// the spec excludes and rejected `[2^(8N-1), 2^8N)` values it admits. We reject rather than
/// aligning because the rust `cddl` oracle (parser dep + conformance validator) hard-errors on the
/// construct, so an aligned implementation would be uncertifiable; revisit when upstream ships the
/// per-value semantics (ledgered in `cddl-matrix/ROADMAP.md`; scoreboard in
/// `draft/cddl-size-on-int-divergence.md`). Pins the message (actionable: names the rule and both
/// conformant spellings) and the boundary: `uint .size N` must KEEP generating.
#[test]
fn size_on_signed_int_rejects_gracefully() {
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_size_int_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "size_int_unused",
        ]);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }

    // top-level rule position: message names the offending rule (in its RustIdent display form,
    // like the sibling float-window rejections sharing `reject_rule_prefix`).
    let msg = run("my_int = int .size 8\n", "top").expect_err(
        "`int .size N` must be a graceful Err, not Ok (the i64 mapping mis-enforces the window)",
    );
    assert!(
        msg.contains("rule `MyInt`"),
        "rejection message should name the offending rule, got: {msg}"
    );
    assert!(
        msg.contains("uint .size") && msg.contains("range"),
        "rejection message should offer both conformant spellings (`uint .size N`, explicit range), got: {msg}"
    );

    // member position rejects too (no rule name available there — message still actionable).
    let msg = run("m = [x: int .size 4]\n", "member")
        .expect_err("`int .size N` in member position must also reject gracefully");
    assert!(
        msg.contains("uint .size"),
        "member-position rejection should carry the same actionable message, got: {msg}"
    );

    // boundary: the uint half of `.size` stays supported.
    run("u = uint .size 2\n", "uint_ok")
        .expect("`uint .size N` is spec-defined and supported — must keep generating");
}

/// Stacked tag encodings (a tag applied to an already-tagged value, reached by writing the outer tag
/// into a MEMBER's own type expression — the rule-BODY spelling `foo = #6.24(#6.100(uint))` is
/// parse-rejected, and a tagged rule body wraps rather than flattening) must give each tag level its
/// OWN encoding member. Levels are counted OUTSIDE-IN: level 1 keeps today's `{name}_tag_encoding`
/// (byte-stability for all existing single-tag output), level k >= 2 mints `{name}_tag{k}_encoding`.
/// Without this both levels reuse `inner_tag_encoding`, emitting a struct with two identically-named
/// members that does not compile — homogeneous (two mandatory tags -> two `Option<cbor_event::Sz>`)
/// and heterogeneous (mandatory outer + optional inner -> `Option<Sz>` at level 1 plus
/// `TagPresenceEncoding` at level 2) alike.
#[test]
fn stacked_tag_encoding_members_are_depth_disambiguated() {
    fn gen_file(spec: &str, tag: &str, file: &str) -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_stacked_tag_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "stacked_tag_unused",
            "--preserve-encodings=true",
            "--wasm=false",
        ]);
        let out = crate::api::generated_strings(&cli).unwrap();
        std::fs::remove_file(&path).ok();
        out.get(file)
            .cloned()
            .unwrap_or_else(|| panic!("preserve-encodings generation must emit {file}"))
    }
    let gen_encodings =
        |spec: &str, tag: &str| gen_file(spec, tag, "rust/src/generated/cbor_encodings.rs");

    // The declaration lines inside `pub struct FooEncoding { .. }` — the members that collide.
    fn foo_member_lines(encodings: &str) -> Vec<String> {
        let start = encodings
            .find("pub struct HolderEncoding {")
            .unwrap_or_else(|| panic!("no HolderEncoding struct in:\n{encodings}"));
        let rest = &encodings[start..];
        let end = rest.find('}').expect("FooEncoding struct must close");
        rest[..end]
            .lines()
            .map(str::trim)
            .filter(|l| l.starts_with("pub ") && l.contains(": "))
            .map(str::to_owned)
            .collect()
    }

    // The carrier is MEMBER position, because a rule BODY no longer flattens a tag onto anything: a
    // tagged rule body of every shape now WRAPS (T1-13), so it owns its tag inside its own encoding
    // struct exactly as a 258 set nominal always did. What still stacks is a tag written into a
    // MEMBER's own type expression over an already-tagged inner — the levels then flatten onto the
    // holder's encoding struct and collide unless depth-suffixed, which is the mechanism this pins.
    // (The rule-body double-tag spelling `foo = #6.24(#6.100(uint))` is parse-rejected outright.)
    // Flavor A (homogeneous): two mandatory tags stack, both lowering to `Option<cbor_event::Sz>`.
    let flavor_a = gen_encodings("holder = [f: #6.24(#6.100(uint))]\n", "a");
    let a_members = foo_member_lines(&flavor_a);
    let mut a_sorted = a_members.clone();
    a_sorted.sort();
    a_sorted.dedup();
    assert_eq!(
        a_sorted.len(),
        a_members.len(),
        "HolderEncoding must have no duplicated member declaration; got:\n{a_members:#?}"
    );
    assert!(
        flavor_a.contains("pub f_tag_encoding: Option<cbor_event::Sz>")
            && flavor_a.contains("pub f_tag2_encoding: Option<cbor_event::Sz>"),
        "homogeneous stacked tags must mint level-1 `f_tag_encoding` and level-2 \
         `f_tag2_encoding`; got:\n{flavor_a}"
    );

    // Flavor B (the heterogeneous case, now settled by construction): a mandatory outer tag over an
    // OPTIONALLY-tagged inner cannot stack at all, because no transparent alias carries an
    // `OptionallyTagged` operation any more — every optional-tag idiom body WRAPS, the tagged
    // PRESERVE table (its last transparent spelling) included, and
    // `IntermediateTypes::assert_no_wire_facts_survive_a_transparent_alias` now refuses the shape
    // unconditionally. The depth mechanism is unchanged; what this half pins is that the OPTIONAL
    // tag stays LEVEL 1 inside the wrapper's OWN encoding struct while the holder's outer tag stays
    // level 1 in the holder's — two namespaces, so nothing to disambiguate and no `f_tag2_encoding`
    // to mint. (Kept as a live carrier rather than deleted: an alias regression that reintroduced
    // the flattening would resurface here as a level-2 member on the holder.)
    let flavor_b = gen_encodings(
        "set = #6.100({* uint => tstr}) / {* uint => tstr} ; @duplicates preserve\n\
         holder = [f: #6.24(set)]\n",
        "b",
    );
    let b_members = foo_member_lines(&flavor_b);
    let mut b_sorted = b_members.clone();
    b_sorted.sort();
    b_sorted.dedup();
    assert_eq!(
        b_sorted.len(),
        b_members.len(),
        "HolderEncoding must have no duplicated member declaration; got:\n{b_members:#?}"
    );
    assert!(
        flavor_b.contains("pub f_tag_encoding: Option<cbor_event::Sz>"),
        "heterogeneous outer mandatory 24 must be level-1 `f_tag_encoding: Option<cbor_event::Sz>`; \
         got:\n{flavor_b}"
    );
    assert!(
        !flavor_b.contains("f_tag2_encoding"),
        "the wrapped inner owns its own optional tag, so nothing stacks onto the holder's member \
         and no level-2 member may be minted there; got:\n{flavor_b}"
    );
    assert!(
        flavor_b.contains("pub struct SetEncoding {")
            && flavor_b.contains("pub inner_tag_encoding: TagPresenceEncoding"),
        "the optional-tag preserve table must WRAP and carry its tag tri-state at level 1 in its \
         own encoding struct; got:\n{flavor_b}"
    );

    // Flavor C (name-boundary reset): an outer mandatory tag 24 over an ARRAY whose element carries
    // its own tag 258. The array element starts a fresh `{field}_elem` name namespace, so the
    // element's tag is LEVEL 1 there (`f_elem_tag_encoding`) even though the field crossed tag 24
    // outside the array. The serialize-side element config must reset tag depth to 0 at that
    // boundary — the same reset `encoding_fields_impl` does — or the write reads a depth-inflated
    // `f_elem_tag2_encoding` the encoding struct never minted (E0425, the generated crate breaks).
    let flavor_c_ser = gen_file(
        "holder = [f: #6.24([* #6.100(uint)])]\n",
        "c",
        "rust/src/generated/serialization.rs",
    );
    assert!(
        flavor_c_ser.contains("f_elem_tag_encoding"),
        "the array element's own tag must ride the level-1 `f_elem_tag_encoding` var; got:\n{flavor_c_ser}"
    );
    assert!(
        !flavor_c_ser.contains("f_elem_tag2_encoding"),
        "the element tag must NOT read a depth-inflated `f_elem_tag2_encoding` (outer-tag depth \
         must reset at the array-element boundary); got:\n{flavor_c_ser}"
    );
}

/// `concat_files` must surface an unreadable path as a real `io::Error` (which callers propagate
/// through `?`), not `panic!` inside its `map_err`. The error message must name the offending path
/// so the failure is actionable.
#[test]
fn concat_files_missing_path_yields_error_not_panic() {
    let missing = "/nonexistent/cddl-codegen/definitely/not/here.rs";
    let err = crate::generation::concat_files(&vec![missing])
        .expect_err("a nonexistent path must yield Err, never panic");
    assert!(
        err.to_string().contains(missing),
        "the io::Error message should name the offending path, got: {err}"
    );
}

/// Open struct-map (rest row) front end: recognition, the graceful-rejection guard set,
/// the two directive-attachment traps (the entry-trailing slot vs the marker-slot trap), and the table-detection
/// no-drift boundary. Message-level pins for the front door plus source-shape assertions for the
/// happy path (the value-level round-trip lives in the compiled e2e `open_struct_map_e2e`). Each
/// guard has BOTH polarities where meaningful (a supported spelling that generates, an unsupported
/// one that rejects with a message naming the supported form).
#[test]
fn open_struct_map_rest_row_front_end() {
    // `run` = default flags EXCEPT --wasm=false (the wasm rest surface is a later WP that rejects);
    // `run_flags` lets a leg add flags (preserve / json) to pin their temporary-front-door rejections.
    fn run_flags(
        spec: &str,
        tag: &str,
        extra: &[&str],
    ) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rest_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            path.to_str().unwrap().to_owned(),
            "--output".to_owned(),
            "rest_unused".to_owned(),
            "--wasm=false".to_owned(),
        ];
        args.extend(extra.iter().map(|s| s.to_string()));
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        run_flags(spec, tag, &[])
    }
    // Concatenate all generated source into one blob for shape assertions.
    fn src(out: &std::collections::BTreeMap<String, String>) -> String {
        out.values().cloned().collect::<Vec<_>>().join("\n")
    }

    // --- happy path: recognition + source shape ---
    let ok = run("foo = { 1: uint, 2: text, * uint => any }\n", "uint_any")
        .expect("an open struct-map with a uint => any rest row must generate (plain flavor)");
    let ok_src = src(&ok);
    assert!(
        ok_src.contains("pub rest: BTreeMap<u64, ") && ok_src.contains("::any_cbor::AnyCbor>"),
        "the rest row must lower to a `pub rest: BTreeMap<u64, AnyCbor>` field, got:\n{ok_src}"
    );
    assert!(
        ok_src.contains("self.rest.len()"),
        "the map header (definite_info) must fold in the rest entry count, got:\n{ok_src}"
    );
    // `new()` must NOT take the rest field (source-compatible when a rest row is added).
    assert!(
        ok_src.contains("pub fn new(key_1: u64, key_2: String)"),
        "new() must exclude the rest field (defaults empty), got:\n{ok_src}"
    );

    // --- table-detection no-drift: a lone `* k => v` map stays a TABLE, not a rest row ---
    let table = run("t = { * uint => any }\n", "table").expect("a lone table must still generate");
    assert!(
        !src(&table).contains("pub rest"),
        "a single-entry `{{ * k => v }}` must stay a TABLE (no rest field)"
    );

    // --- guard: non-final rest row ---
    let non_final = run("foo = { 1: uint, * uint => any, 2: text }\n", "nonfinal")
        .expect_err("a non-final rest row must reject");
    assert!(
        non_final.contains("rule `foo`") && non_final.contains("LAST entry"),
        "non-final rest row must name the LAST-entry requirement, got: {non_final}"
    );

    // --- guard: multiple rest rows ---
    let multiple = run("foo = { 1: uint, * uint => any, * text => any }\n", "multi")
        .expect_err("multiple rest rows must reject");
    assert!(
        multiple.contains("rule `foo`") && multiple.contains("single trailing rest row"),
        "multiple rest rows must name the single-trailing-row rule, got: {multiple}"
    );

    // --- guard: bounded occurrence (`+`) ---
    let plus =
        run("foo = { 1: uint, + uint => any }\n", "plus").expect_err("a `+` rest row must reject");
    assert!(
        plus.contains("rule `foo`") && plus.contains("`*` occurrence"),
        "a bounded (`+`) rest row must name the `*` requirement, got: {plus}"
    );

    // --- general key domain: `bstr` (and every other deserializable key type) GENERATES ---
    let bytes_domain = run("foo = { 1: uint, * bstr => any }\n", "bstr")
        .expect("a bstr key domain rest row generates (typed seek path)");
    assert!(
        src(&bytes_domain).contains("pub rest: BTreeMap<Vec<u8>, "),
        "a typed bstr key domain must land in a `BTreeMap<Vec<u8>, _>` rest field, got:\n{}",
        src(&bytes_domain)
    );

    // --- guard: float key domain (the one type-level domain rejection left) ---
    // Fires from `finalize`'s rest-row float instrument, beside the table/set ones — the domain guard
    // itself no longer restricts the key type.
    let float_domain = run("foo = { 1: uint, * float => any }\n", "float_key")
        .expect_err("a float key domain rest row must reject");
    assert!(
        float_domain.contains("rule `Foo`")
            && float_domain.contains("rest-row key type contains a float")
            && float_domain.contains("no total order"),
        "a float key domain must be rejected naming the missing total order, got: {float_domain}"
    );
    // Reached through a struct too (the visitor walks the domain transitively).
    let float_nested = run(
        "k = [x: float64, y: uint]\nfoo = { 1: uint, * k => any }\n",
        "float_key_nested",
    )
    .expect_err("a float-CONTAINING key domain rest row must reject");
    assert!(
        float_nested.contains("rest-row key type contains a float"),
        "a float-containing key domain must be rejected too, got: {float_nested}"
    );

    // --- guard: null-admitting key domain ---
    // A `null` key and the indefinite-map break are both CBOR special values, so the row's key
    // dispatch cannot tell them apart.
    let null_domain = run(
        "k = text / null\nfoo = { 1: uint, * k => any }\n",
        "null_key",
    )
    .expect_err("a null-admitting key domain rest row must reject");
    assert!(
        null_domain.contains("rule `foo`")
            && null_domain.contains("null-admitting key domain")
            && null_domain.contains("break"),
        "a null-admitting key domain must name the break collision, got: {null_domain}"
    );

    // --- recognition: a typed key domain GENERATES under either JSON flag ---
    // The flattened-rest JSON surface images a rest key as an object MEMBER NAME. For a typed `K`
    // that image is the `any` domain's convention applied to `K`'s own CBOR bytes, so the JSON flags
    // no longer restrict the key domain — a NOMINAL `K` reads its head at runtime, a PRIMITIVE `K`
    // states its image directly, and both routes are pinned here at the emission level (the
    // value-level contract lives in `tests/open-struct-map-json-e2e`).
    let typed_json = run_flags(
        "md = int / bstr\nfoo = { 1: uint, * md => any }\n",
        "typed_json",
        &["--json-serde-derives=true"],
    )
    .expect("a typed key domain generates under --json-serde-derives");
    assert!(
        src(&typed_json).contains("open_struct_rest_json::typed_rest_key_string")
            && src(&typed_json).contains("open_struct_rest_json::rest_key_from_string"),
        "a nominal typed key domain must image through its own CBOR bytes, got:\n{}",
        src(&typed_json)
    );
    let typed_schema = run_flags(
        "md = int / bstr\nfoo = { 1: uint, * md => uint }\n",
        "typed_schema",
        &["--json-schema-export=true"],
    )
    .expect("a typed key domain generates under --json-schema-export");
    assert!(
        src(&typed_schema).contains("open_struct_rest_json::general_key_rest_map_schema::<"),
        "a typed key domain's flattened region must publish the K-free open-object schema, got:\n{}",
        src(&typed_schema)
    );
    // A PRIMITIVE typed key (here a sized int — typed because `.size` keeps it off the peeked path)
    // states its image directly instead: no CBOR round-trip, and the same reading its bare `uint`
    // sibling uses, so which side of the CBOR routing rule a row falls on never changes its JSON.
    let typed_primitive = run_flags(
        "foo = { 1: uint, * uint .size 1 => uint }\n",
        "typed_primitive_json",
        &["--json-serde-derives=true"],
    )
    .expect("a primitive typed key domain generates under --json-serde-derives");
    assert!(
        src(&typed_primitive).contains("|k: &u8| Ok::<String, core::convert::Infallible>")
            && src(&typed_primitive).contains("ks.parse::<u8>()"),
        "a primitive typed key domain must state its image directly, got:\n{}",
        src(&typed_primitive)
    );
    // ...and the same spec WITHOUT the JSON flags generates too, with no flatten machinery at all.
    let typed_plain = run(
        "md = int / bstr\nfoo = { 1: uint, * md => any }\n",
        "typed_plain",
    )
    .expect("a typed key domain generates without the JSON flags");
    assert!(
        src(&typed_plain).contains("pub rest: BTreeMap<Md, ")
            && !src(&typed_plain).contains("open_struct_rest_json"),
        "a typed union key domain must land in a `BTreeMap<Md, _>` rest field, got:\n{}",
        src(&typed_plain)
    );

    // --- guard: lone non-fixed entry (no fixed key before the rest row) ---
    let lone = run(
        "foo = { * uint => any, }\ng = (* uint => any)\n",
        "lone_via_group",
    );
    // (`{ * uint => any }` alone is a table; the lone-non-fixed guard fires for degenerate shapes
    // that reach the record path — covered by the alias-arrow robustness test — so here we only pin
    // that the ordinary lone table is NOT mis-taken as a rest row.)
    let _ = lone;

    // --- guard: rest row in a group-choice arm (`{ …arm1… // …arm2… }`) ---
    let arm = run(
        "foo = { 1: uint, * uint => any // 5: text, 6: text }\n",
        "arm",
    )
    .expect_err("a rest row in a group-choice arm must reject");
    assert!(
        arm.contains("group-choice arm") || arm.contains("group-choice"),
        "a rest row in a group-choice arm must name the choice-arm restriction, got: {arm}"
    );

    // --- guard: rest row inside a PLAIN GROUP (`g = ( 1: a, * k => v )`) — rejected because a
    // materialized plain group exports transparently as an extern-interface group-body row rendered
    // from `fields` only (`project_plain_group`), which would silently drop the rest row across a
    // crate boundary. The named-rule remedy is the supported spelling. ---
    let plain_group = run(
        "g = ( 1: a, * uint => any )\na = uint\nfoo = { g }\n",
        "plain_group",
    )
    .expect_err("a rest row inside a plain group must reject");
    assert!(
        plain_group.contains("rule `g`") && plain_group.contains("plain group"),
        "a rest row in a plain group must name the plain-group restriction + named-rule remedy, got: {plain_group}"
    );

    // --- guard: the MAP `* k => v` arrow rest ROW is map-only. Its array analog is the `* t` rest
    // TAIL — a separate feature whose full guard/polarity matrix lives in `open_array_front_end`. Here
    // just confirm the two do not cross-contaminate: a final-position `* t` in an array (`[uint,
    // * text]`) is recognized as an open-array rest tail and its captured field is a `Vec`, NOT a map
    // container. ---
    let arr = run("foo = [uint, * text]\n", "array")
        .expect("a final-position `* t` after fixed members is an open-array rest tail");
    assert!(
        src(&arr).contains("pub rest: Vec<"),
        "an open-array rest tail captures into a `Vec`, not a `* k => v` map container"
    );

    // --- preserve fidelity core: open structs GENERATE under --preserve-encodings, lowering
    // the rest field to the insertion-ordered `OrderedHashMap` container with per-entry encoding
    // sidecars for concrete key/value domains (a `* uint => any` gets a uint-key sidecar; the `any`
    // value is self-carried). ---
    let preserve = run_flags(
        "foo = { 1: uint, * uint => any }\n",
        "preserve",
        &["--preserve-encodings=true"],
    )
    .expect("open structs GENERATE under --preserve-encodings");
    let preserve_src = src(&preserve);
    assert!(
        preserve_src.contains("pub rest: OrderedHashMap<u64, ")
            && preserve_src.contains("::any_cbor::AnyCbor>"),
        "the preserve rest field must lower to an insertion-ordered `OrderedHashMap`, got:\n{preserve_src}"
    );
    assert!(
        preserve_src.contains("rest_key_encodings"),
        "a concrete uint-key rest row must carry a `rest_key_encodings` sidecar under preserve, got:\n{preserve_src}"
    );
    // --- flattened rest JSON: open structs GENERATE under --json-serde-derives, wiring
    // the rest field to the FLATTENED serde surface (its captured entries render at the same JSON
    // object level as the declared fields). ---
    let json = run_flags(
        "foo = { 1: uint, * uint => any }\n",
        "json",
        &["--json-serde-derives=true"],
    )
    .expect("open structs GENERATE under --json-serde-derives");
    let json_src = src(&json);
    assert!(
        json_src.contains("#[serde(flatten)]")
            && json_src.contains("serialize_flattened_rest")
            && json_src.contains("read_flattened_rest_pairs"),
        "the rest field must wire the flattened-JSON serialize_with/deserialize_with helpers, got:\n{json_src}"
    );
    // An `any` range needs the natural-walk value view, so its entries expression maps each value
    // into `NaturalAnyCborSer`.
    assert!(
        json_src.contains("NaturalAnyCborSer(v)"),
        "an `any`-range rest row must render values through the natural walk, got:\n{json_src}"
    );
    // A TYPED range uses the value's own serde, so it needs NO value view at all: its `(&K, &V)`
    // pairs already match the helper's item type and the entries expression is the bare `.iter()`.
    // Wrapping them in the `any` shape's `.map(|(k, v)| (k, v))` would be an identity map, which
    // `clippy::map_identity` (warn-by-default) flags in every consumer that builds lint-clean.
    let typed = run_flags(
        "foo = { 1: uint, * uint => text }\n",
        "json_typed",
        &["--json-serde-derives=true"],
    )
    .expect("a fully-typed rest row GENERATES under --json-serde-derives");
    let typed_src = src(&typed);
    assert!(
        typed_src.contains("serialize_flattened_rest") && typed_src.contains("rest.iter(),"),
        "a typed-range rest row must pass `rest.iter()` straight to the helper, got:\n{typed_src}"
    );
    assert!(
        !typed_src.contains("map(|(k, v)| (k, v))"),
        "a typed-range rest row must not emit an identity map (clippy::map_identity), got:\n{typed_src}"
    );
    // --- flattened rest SCHEMA ownership (--json-schema-export): the open region's schema comes from
    // the rest-row POSITION (key domain × value type), never from the container's own `JsonSchema`.
    // The `@duplicates preserve` twin is array-shaped (`PairMap` → `Vec<(K, V)>`), so a TYPED preserve
    // row names the position's schema explicitly; its non-preserve twin IS the `BTreeMap`/
    // `OrderedHashMap` the position calls for, so it keeps delegating and emits no attribute; an `any`
    // range keeps the permissive natural-any override in either container. The helper the preserve arm
    // names delegates to `BTreeMap<K, V>`, so the two containers are schema-indistinguishable. ---
    let schema_flags = &["--json-serde-derives=true", "--json-schema-export=true"];
    for (spec, tag, k_v) in [
        (
            "foo = { 1: uint, * uint => text ; @duplicates preserve\n}\n",
            "schema_dup_uint",
            "u64, String",
        ),
        (
            "foo = { 1: uint, * text => uint ; @duplicates preserve\n}\n",
            "schema_dup_text",
            "String, u64",
        ),
    ] {
        let dup = run_flags(spec, tag, schema_flags)
            .expect("a typed `@duplicates preserve` rest row GENERATES under the json flags");
        let dup_src = src(&dup);
        // the attribute text only (rustfmt line-breaks the surrounding `#[schemars(…)]` when the
        // path is long, so the wrapper is not part of the pin).
        let expected = format!(
            "schema_with = \
             \"crate::generated::open_struct_rest_json::typed_rest_map_schema::<{k_v}>\""
        );
        assert!(
            dup_src.contains(&expected),
            "a typed `@duplicates preserve` rest row must own its flattened schema via \
             `{expected}` (the PairMap container's own schema is an array of pairs), got:\n{dup_src}"
        );
    }
    // The non-preserve typed twin delegates to its own container (which IS the `BTreeMap` the
    // position calls for) — no attribute, and the same shape by construction.
    let typed_schema = run_flags(
        "foo = { 1: uint, * uint => text }\n",
        "schema_typed",
        schema_flags,
    )
    .expect("a typed non-preserve rest row GENERATES under the json flags");
    assert!(
        !src(&typed_schema).contains("schemars(schema_with"),
        "a typed NON-preserve rest row must keep delegating to its container's schema (no \
         `schema_with` override), got:\n{}",
        src(&typed_schema)
    );
    // An `any` range takes the permissive natural-any override whichever container holds it — the
    // `PairMap`'s array-of-pairs schema is replaced wholesale there too.
    let any_dup = run_flags(
        "foo = { 1: uint, * uint => any ; @duplicates preserve\n}\n",
        "schema_dup_any",
        schema_flags,
    )
    .expect("an `any`-range `@duplicates preserve` rest row GENERATES under the json flags");
    let any_dup_src = src(&any_dup);
    assert!(
        any_dup_src
            .contains("schema_with = \"crate::generated::any_cbor::natural_any_cbor_map_schema\"")
            && !any_dup_src.contains("typed_rest_map_schema"),
        "an `any`-range rest row must keep the natural-any override in every container, got:\n{any_dup_src}"
    );
    // `--json-schema-export` ALONE (no serde derives): the schema surface is the subject, so the
    // attribute is emitted and the helper's module is declared — from its schemars fragment only, the
    // serde-dependent half staying behind `--json-serde-derives` (that crate declares no `serde`).
    let schema_only = run_flags(
        "foo = { 1: uint, * uint => text ; @duplicates preserve\n}\n",
        "schema_only",
        &["--json-schema-export=true"],
    )
    .expect("a typed `@duplicates preserve` rest row GENERATES under --json-schema-export alone");
    let schema_only_src = src(&schema_only);
    assert!(
        schema_only_src.contains(
            "schema_with = \
             \"crate::generated::open_struct_rest_json::typed_rest_map_schema::<u64, String>\""
        ) && schema_only_src.contains("pub mod open_struct_rest_json;"),
        "under --json-schema-export alone the rest-row schema helper must still be reachable, got:\n{schema_only_src}"
    );
    assert!(
        !schema_only_src.contains("serialize_flattened_rest"),
        "the serde flatten mechanics stay behind --json-serde-derives, got:\n{schema_only_src}"
    );
    // --- wasm rest surface: open structs GENERATE under --wasm (the default), the wasm wrapper
    // gaining a `rest` getter that returns the captured entries as the minted map wrapper.
    // --wasm=true is the default, so pin generation by NOT passing --wasm=false. ---
    {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rest_wasm_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, "foo = { 1: uint, * uint => any }\n").unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rest_wasm_unused",
        ]);
        let wasm = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        let wasm = wasm.expect("open structs GENERATE under --wasm");
        let wasm_src = wasm.values().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            wasm_src.contains("pub fn rest(&self) -> MapU64ToAny")
                && wasm_src.contains("pub struct MapU64ToAny"),
            "the wasm wrapper must expose a `rest` getter returning the minted map wrapper, got:\n{wasm_src}"
        );
    }

    // --- @duplicates preserve on a rest row GENERATES: the vec-of-pairs twin (`PairMap`),
    // accepting + re-emitting duplicate keys in wire order, matching @duplicates preserve tables. ---
    let dup_preserve = run(
        "foo = {\n  1: uint,\n  * uint => any ; @duplicates preserve\n}\n",
        "dup_preserve",
    )
    .expect("@duplicates preserve on a rest row GENERATES the PairMap twin");
    assert!(
        src(&dup_preserve).contains("pub rest: PairMap<u64, ")
            && src(&dup_preserve).contains("::any_cbor::AnyCbor>"),
        "an @duplicates preserve rest row must lower to a `PairMap` (duplicate-permitting), got:\n{}",
        src(&dup_preserve)
    );

    // --- entry-level @name on the rest row IS honored (read from the entry-trailing slot) ---
    let named = run(
        "foo = {\n  1: uint,\n  * uint => any ; @name extra\n}\n",
        "named",
    )
    .expect("@name on the rest row must generate");
    let named_src = src(&named);
    assert!(
        named_src.contains("pub extra: BTreeMap<u64,") && !named_src.contains("pub rest:"),
        "@name on the rest row must rename the capture field to `extra`, got:\n{named_src}"
    );
    // the type name must be unchanged (an entry-level @name must NOT leak to the rule/type name).
    assert!(
        named_src.contains("pub struct Foo"),
        "an entry-level @name must not rename the TYPE, got:\n{named_src}"
    );

    // --- marker-slot trap: a directive on the `*` marker's own comment slot (before the
    // entry type) is NOT honored — the field stays `rest` (current behavior, pinned loud). ---
    let marker = run(
        "foo = {\n  1: uint,\n  *  ; @name marker\n  uint => any\n}\n",
        "marker",
    )
    .expect("a marker-slot directive must not break generation");
    let marker_src = src(&marker);
    assert!(
        marker_src.contains("pub rest:") && !marker_src.contains("pub marker:"),
        "a directive on the `*` marker's comment slot must NOT be honored (field stays `rest`), got:\n{marker_src}"
    );

    // --- rule-trailing slot: a RULE-position @duplicates (same line as `}`) is read at
    // rule level (rejected as not-applicable to a record), NOT mis-read as the rest row's directive
    // (which would be the @duplicates-preserve front-door message instead). ---
    let rule_dup = run(
        "foo = { 1: uint, * uint => any } ; @duplicates preserve\n",
        "rule_dup",
    )
    .expect_err("a rule-position @duplicates on a record must reject");
    assert!(
        rule_dup.contains("rule `foo`") && !rule_dup.contains("open struct-map rest row"),
        "a rule-position @duplicates must be read at rule level, not stolen by the rest row, got: {rule_dup}"
    );

    // ===== IGNORE flavor (`@ignore` on the rest row): tolerate-and-drop =====

    // --- happy path: `@ignore` on an entry-trailing rest row GENERATES a CLOSED struct (no `rest`
    // field, no flatten machinery), while the deserialize loop still consumes+drops each unknown
    // entry (dynamic length so a definite map with extra entries decodes). ---
    let ign = run(
        "foo = {\n  1: uint, 2: text,\n  * uint => any ; @ignore\n}\n",
        "ignore_ok",
    )
    .expect("an `@ignore` rest row must generate (tolerate-and-drop, plain flavor)");
    let ign_src = src(&ign);
    assert!(
        !ign_src.contains("pub rest"),
        "an `@ignore` rest row must emit NO `rest` field (closed struct), got:\n{ign_src}"
    );
    // `new()` matches the closed struct's — the two declared fields, no rest arg.
    assert!(
        ign_src.contains("pub fn new(key_1: u64, key_2: String)"),
        "an `@ignore` struct's new() takes only the declared fields, got:\n{ign_src}"
    );
    // The value is still typed-deserialized then dropped: the drop binding proves the arm runs.
    assert!(
        ign_src.contains("let _rest_value ="),
        "an `@ignore` rest row must typed-deserialize-and-DROP the value, got:\n{ign_src}"
    );

    // --- combination guard: `@ignore` + `--preserve-encodings` REJECTS (a preserve crate's
    // byte-exact round-trip contract can't hold for a deliberately-lossy type). ---
    let ign_preserve = run_flags(
        "foo = {\n  1: uint,\n  * uint => any ; @ignore\n}\n",
        "ignore_preserve",
        &["--preserve-encodings=true"],
    )
    .expect_err("`@ignore` under --preserve-encodings must reject");
    assert!(
        ign_preserve.contains("rule `foo`")
            && ign_preserve.contains("--preserve-encodings")
            && ign_preserve.contains("@custom_serialize"),
        "the preserve rejection must name --preserve-encodings + the custom_serialize remedy, got: {ign_preserve}"
    );

    // --- combination guard: `@ignore` + `@duplicates` REJECTS (a duplicates policy governs a
    // captured container, which `@ignore` does not create). ---
    let ign_dup = run(
        "foo = {\n  1: uint,\n  * uint => any ; @ignore @duplicates preserve\n}\n",
        "ignore_dup",
    )
    .expect_err("`@ignore` + `@duplicates` on a rest row must reject");
    assert!(
        ign_dup.contains("rule `foo`")
            && ign_dup.contains("@ignore")
            && ign_dup.contains("@duplicates"),
        "the @ignore+@duplicates rejection must name both directives, got: {ign_dup}"
    );

    // --- combination guard: `@ignore` + `@name` REJECTS (`@ignore` emits no field to name). ---
    let ign_name = run(
        "foo = {\n  1: uint,\n  * uint => any ; @ignore @name extra\n}\n",
        "ignore_name",
    )
    .expect_err("`@ignore` + `@name` on a rest row must reject");
    assert!(
        ign_name.contains("rule `foo`")
            && ign_name.contains("@ignore")
            && ign_name.contains("@name"),
        "the @ignore+@name rejection must name both directives, got: {ign_name}"
    );

    // --- placement guards fire BEFORE semantics: an `@ignore` on a NON-FINAL rest row
    // still gets the placement (LAST-entry) rejection, not an ignore-specific message. ---
    let ign_nonfinal = run(
        "foo = {\n  1: uint,\n  * uint => any ; @ignore\n  2: text\n}\n",
        "ignore_nonfinal",
    )
    .expect_err("a non-final `@ignore` rest row must reject on placement");
    assert!(
        ign_nonfinal.contains("rule `foo`") && ign_nonfinal.contains("LAST entry"),
        "an `@ignore` on a non-final row must get the placement rejection first, got: {ign_nonfinal}"
    );

    // --- never-silent placement: `@ignore` on a PLAIN TYPE RULE rejects (not applicable). ---
    let ign_type_rule = run("x = uint ; @ignore\n", "ignore_type_rule")
        .expect_err("`@ignore` on a type rule must reject");
    assert!(
        ign_type_rule.contains("@ignore")
            && ign_type_rule.contains("only valid on an open struct-map rest row"),
        "an `@ignore` on a plain type rule must reject naming the one valid placement, got: {ign_type_rule}"
    );

    // --- never-silent placement: `@ignore` on a TABLE RULE (`{ * k => v }`, no fixed keys)
    // rejects — a table is not an open struct-map. ---
    let ign_table = run("t = { * uint => any } ; @ignore\n", "ignore_table")
        .expect_err("`@ignore` on a table rule must reject");
    assert!(
        ign_table.contains("@ignore")
            && ign_table.contains("only valid on an open struct-map rest row"),
        "an `@ignore` on a table rule must reject naming the one valid placement, got: {ign_table}"
    );

    // --- never-silent placement: `@ignore` at a FIELD/member position rejects. ---
    let ign_field = run("foo = {\n  field: uint ; @ignore\n}\n", "ignore_field")
        .expect_err("`@ignore` on a struct field must reject");
    assert!(
        ign_field.contains("@ignore") && ign_field.contains("field"),
        "an `@ignore` on a field must reject naming the field position, got: {ign_field}"
    );

    // --- slot direction: a RULE-position `@ignore` (same line as `}`) is read at
    // rule level and rejected as not-applicable — it is NOT stolen by the rest ENTRY (which would make
    // the row ignore-flavored and generate a closed struct silently). ---
    let ign_rule_pos = run(
        "foo = { 1: uint, * uint => any } ; @ignore\n",
        "ignore_rule_pos",
    )
    .expect_err("a rule-position `@ignore` on an open struct must reject");
    assert!(
        ign_rule_pos.contains("@ignore")
            && ign_rule_pos.contains("only valid on an open struct-map rest row"),
        "a rule-position `@ignore` must be read at rule level, not stolen by the entry, got: {ign_rule_pos}"
    );

    // --- marker-slot trap: an `@ignore` on the `*` marker's own comment slot (before the entry
    // type) is NOT honored — the row stays CAPTURE (a `pub rest` field appears), pinned loud. ---
    let ign_marker = run(
        "foo = {\n  1: uint,\n  *  ; @ignore\n  uint => any\n}\n",
        "ignore_marker",
    )
    .expect("a marker-slot `@ignore` must not break generation");
    assert!(
        src(&ign_marker).contains("pub rest:"),
        "an `@ignore` on the `*` marker's comment slot must NOT be honored (row stays capture, `pub rest` present), got:\n{}",
        src(&ign_marker)
    );
}

/// Open table (`t = { * K_t => V_t, * K_r => V_r }`) front end: recognition of the two-row shape,
/// the SHAPE rejections the parse walk owns, the STATICNESS rejections `finalize` owns, and the
/// `@custom_wire_major` placement policing. Message-level pins for every rejection plus source-shape
/// assertions for the happy path (the value-level round-trip lives in the compiled e2e
/// `open_table_e2e`). Each guard carries both polarities where meaningful.
#[test]
fn open_table_front_end() {
    fn run_flags(
        spec: &str,
        tag: &str,
        extra: &[&str],
    ) -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_opentable_{}_{}.cddl",
            tag,
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let mut args = vec![
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            path.to_str().unwrap().to_owned(),
            "--output".to_owned(),
            "opentable_unused".to_owned(),
            "--wasm=false".to_owned(),
        ];
        args.extend(extra.iter().map(|s| s.to_string()));
        let cli = Cli::parse_from(args);
        let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    }
    fn run(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
        run_flags(spec, tag, &[])
    }
    fn src(out: &std::collections::BTreeMap<String, String>) -> String {
        out.values().cloned().collect::<Vec<_>>().join("\n")
    }
    // `md` is the standing multi-major catch-all key in these legs (majors 0 and 3).
    const MD: &str = "md = uint / text\n";

    // --- happy path: two rows, zero fixed fields, two containers on one struct ---
    let ok = run(
        &format!("{MD}t = {{ * bstr => uint, * md => md }}\n"),
        "happy",
    )
    .expect("an open table with a statically-single-major typed key must generate");
    let ok_src = src(&ok);
    assert!(
        ok_src.contains("pub entries: BTreeMap<Vec<u8>, u64>")
            && ok_src.contains("pub rest: BTreeMap<Md, Md>"),
        "both rows must lower to their own `pub` container field, got:\n{ok_src}"
    );
    assert!(
        ok_src.contains("self.entries.len() as u64 + self.rest.len() as u64"),
        "the map header must fold BOTH rows' live counts, got:\n{ok_src}"
    );
    assert!(
        ok_src.contains("pub fn new() -> Self"),
        "new() takes neither row (both default empty), got:\n{ok_src}"
    );
    assert!(
        ok_src.contains("cbor_event::Type::Bytes =>"),
        "the typed row must claim its declared major as its own dispatch arm, got:\n{ok_src}"
    );

    // --- no-drift: a lone `* k => v` map is still a TABLE, and fixed keys + one row still an open
    // struct-map (neither becomes an open table) ---
    let table = run("t = { * uint => text }\n", "table").expect("a lone table must still generate");
    assert!(
        !src(&table).contains("pub entries"),
        "a single-entry `{{ * k => v }}` must stay a TABLE"
    );
    let open_struct = run("foo = { 1: uint, * uint => any }\n", "openstruct")
        .expect("an open struct-map must still generate");
    assert!(
        !src(&open_struct).contains("pub entries"),
        "fixed keys + one rest row stays an open struct-map, not an open table"
    );

    // --- SHAPE (parse walk) ---
    // >2 non-fixed rows: the re-worded multiplicity guard names BOTH legal shapes.
    let three = run(
        &format!("{MD}t = {{ * bstr => uint, * uint => uint, * md => md }}\n"),
        "three",
    )
    .expect_err("three non-fixed rows must reject");
    assert!(
        three.contains("single trailing rest row")
            && three.contains("open table")
            && three.contains("3 non-fixed rows"),
        "the multiplicity message must name both legal shapes and the count, got: {three}"
    );
    // Fixed keys mixed with two non-fixed rows: the contract covers ZERO-fixed-field open tables.
    let mixed = run(
        &format!("{MD}t = {{ 1: uint, * bstr => uint, * md => md }}\n"),
        "mixed",
    )
    .expect_err("fixed keys beside two non-fixed rows must reject");
    assert!(
        mixed.contains("drop the fixed keys to spell an open table"),
        "the mixed shape must point at the zero-fixed-field spelling, got: {mixed}"
    );
    // INLINE anonymous open table: no structural name is synthesized. The pre-existing inline-map
    // guard fires first (an inline map in member position is unsupported unless it is a table), and
    // it already names the named-rule remedy — so the open-table recognizer's own inline backstop is
    // reachable only if that guard ever stops covering the position. What is pinned is the CONTRACT:
    // an inline spelling rejects gracefully, pointing at the named-rule form.
    let inline = run(
        &format!("{MD}f = {{ x: {{ * bstr => uint, * md => md }} }}\n"),
        "inline",
    )
    .expect_err("an inline open table must reject");
    assert!(
        inline.contains("name it as its own rule") || inline.contains("its own named rule"),
        "the inline rejection must name the named-rule form, got: {inline}"
    );
    // The `{+ …}` NonEmpty twin: min-1 on the TYPED row is the delivered flavor. `+` and `1*` are
    // the same spelling (the table path's rule verbatim), and both mint the two-argument door plus
    // the post-loop bound check.
    for (spec_occ, tag) in [("+", "plus"), ("1*", "onestar")] {
        let ne = run(
            &format!("{MD}t = {{ {spec_occ} bstr => uint, * md => md }}\n"),
            tag,
        )
        .unwrap_or_else(|e| panic!("a NonEmpty open table (`{spec_occ}`) must generate, got: {e}"));
        let ne_src = src(&ne);
        assert!(
            ne_src.contains("pub fn new(first_key: Vec<u8>, first_value: u64) -> Self"),
            "the NonEmpty door must take the first typed entry, got:\n{ne_src}"
        );
        assert!(
            ne_src.contains("if entries.is_empty()")
                && ne_src.contains("min: Some(1)")
                && ne_src.contains("max: None"),
            "the min-1 bound must be enforced after the deserialize loop with `NonEmptyMap`'s own \
             RangeCheck, got:\n{ne_src}"
        );
    }
    // The two door parameters are FIXED emitter names beside a `@name`-settable row name, so the
    // one identifier hazard they create is a row named for a parameter: the seeding block must not
    // bind the field's name as its local, or it would shadow the parameter it is handed (E0308 in
    // the generated crate, where the spec author cannot see it).
    let named_for_param = run(
        &format!(
            "{MD}t = {{\n  + bstr => uint ; @name first_key\n  ,\n  * md => md ; @name first_value\n}}\n"
        ),
        "nameparam",
    )
    .expect("a row named for a door parameter must still generate");
    assert!(
        src(&named_for_param).contains("let mut seed = BTreeMap::new();"),
        "the seeding local must be a fixed name, not the row's, got:\n{}",
        src(&named_for_param)
    );
    // …and `0*` is the same UNBOUNDED row `*` is, so it mints the argument-less door.
    let zero_star = run(
        &format!("{MD}t = {{ 0* bstr => uint, * md => md }}\n"),
        "zerostar",
    )
    .expect("`0*` on the typed row is the unbounded spelling and must generate");
    assert!(
        src(&zero_star).contains("pub fn new() -> Self"),
        "`0*` must stay the unbounded flavor, got:\n{}",
        src(&zero_star)
    );
    // The min-1 counts TYPED entries, so it has no reading on the catch-all row.
    let plus_catch = run(
        &format!("{MD}t = {{ * bstr => uint, + md => md }}\n"),
        "pluscatch",
    )
    .expect_err("`+` on the catch-all row must reject");
    assert!(
        plus_catch.contains("supported only on an open table's TYPED row")
            && plus_catch.contains("minimum of 1 counts TYPED entries"),
        "the catch-all `+` rejection must state where the bound belongs, got: {plus_catch}"
    );
    // `?` on a row is neither shape.
    let opt = run(
        &format!("{MD}t = {{ ? bstr => uint, * md => md }}\n"),
        "opt",
    )
    .expect_err("a `?` row must reject");
    assert!(
        opt.contains("`*` occurrence") && opt.contains("`+` (at least one TYPED entry)"),
        "a bounded row must name the `*` requirement and the one `+` concession, got: {opt}"
    );
    // A genuinely bounded marker (`n*m`) is still a real cardinality this shape does not honor.
    let bounded = run(
        &format!("{MD}t = {{ 2*5 bstr => uint, * md => md }}\n"),
        "bounded",
    )
    .expect_err("a `n*m` row must reject");
    assert!(
        bounded.contains("`*` occurrence"),
        "a bounded row must name the `*` requirement, got: {bounded}"
    );
    // `any` on the TYPED row claims all eight majors, leaving the catch-all nothing.
    let any_typed = run(
        &format!("{MD}t = {{ * any => uint, * md => md }}\n"),
        "anytyped",
    )
    .expect_err("an `any`-keyed typed row must reject");
    assert!(
        any_typed.contains("cannot be keyed on `any`"),
        "an `any` typed key must reject by shape, got: {any_typed}"
    );
    // A bare-`text` typed key is CBOR-only: in JSON it admits every member name, so the typed-first
    // partition leaves the catch-all unreachable and `from_json` refuses what `to_json` wrote. Both
    // JSON flags refuse it; without them the shape is perfectly good.
    for flag in ["--json-serde-derives=true", "--json-schema-export=true"] {
        let msg = run_flags(
            &format!("{MD}t = {{ * text => uint, * md => md }}\n"),
            "baretext",
            &[flag],
        )
        .expect_err("a bare-text typed key under a JSON flag must reject");
        assert!(
            msg.contains("keyed on bare `text` is a CBOR-ONLY shape")
                && msg.contains("leaves the catch-all row unreachable"),
            "the bare-text rejection must name the CBOR-only nature and the reason, got: {msg}"
        );
    }
    run(
        &format!("{MD}t = {{ * text => uint, * md => md }}\n"),
        "baretextok",
    )
    .expect("without a JSON face a bare-text typed key is a supported CBOR shape");
    // The check is TRANSPARENT resolution, so an alias of `text` is caught…
    let alias_text = run_flags(
        &format!("{MD}k = text\nt = {{ * k => uint, * md => md }}\n"),
        "aliastext",
        &["--json-serde-derives=true"],
    )
    .expect_err("an alias of `text` resolves transparently and must reject");
    assert!(alias_text.contains("CBOR-ONLY shape"), "got: {alias_text}");
    // …while a `@newtype` mints its own type, whose serde a hand impl owns — undecidable from here,
    // and the documented hazard rather than a rejection.
    run_flags(
        &format!("{MD}k = text ; @newtype\nt = {{ * k => uint, * md => md }}\n"),
        "newtypetext",
        &["--json-serde-derives=true"],
    )
    .expect("a `@newtype` text key is opaque here and stays a documented hazard");
    // …and `any` on the CATCH-ALL is exactly where it belongs.
    run("t = { * bstr => uint, * any => any }\n", "anycatch")
        .expect("an `any`-keyed catch-all must generate");
    // A null-admitting key collides with the indefinite-map break on both rows.
    let nullable = run(
        "k = text / null\nt = { * k => uint, * uint => uint }\n",
        "nullable",
    )
    .expect_err("a null-admitting key must reject");
    assert!(
        nullable.contains("null-admitting key domain"),
        "a null-admitting key must name the break collision, got: {nullable}"
    );
    // `@ignore` on either row would silently discard half the map.
    let ign = run(
        &format!("{MD}t = {{ * bstr => uint ; @ignore\n, * md => md }}\n"),
        "ignore",
    )
    .expect_err("`@ignore` on an open table row must reject");
    assert!(
        ign.contains("@ignore") && ign.contains("discard half the map"),
        "`@ignore` must reject naming the loss, got: {ign}"
    );
    // Two rows `@name`d onto one field name would be two `pub` fields with one name.
    let same_name = run(
        &format!("{MD}t = {{ * bstr => uint ; @name x\n, * md => md ; @name x\n}}\n"),
        "samename",
    )
    .expect_err("two rows naming one field must reject");
    assert!(
        same_name.contains("their names must differ"),
        "a field-name collision between the rows must reject, got: {same_name}"
    );

    // --- STATICNESS (finalize) ---
    let multi = run(
        &format!("{MD}t = {{ * md => uint, * bstr => uint }}\n"),
        "multi",
    )
    .expect_err("a multi-major typed key must reject");
    assert!(
        multi.contains("statically known")
            && multi.contains("admits 2 majors")
            && multi.contains("@custom_wire_major"),
        "the staticness message must name the rule, the count and the directive, got: {multi}"
    );
    // The complement check: a catch-all whose majors the typed row exhausts can never capture.
    let empty_complement = run(
        "k = text\nt = { * k => uint, * text => uint }\n",
        "complement",
    )
    .expect_err("an exhausted catch-all must reject");
    assert!(
        empty_complement.contains("can never capture an entry"),
        "the empty-complement message must say so, got: {empty_complement}"
    );
    // A custom-codec key: `cbor_types()` answers about the REPLACED type, so the declaration is
    // REQUIRED — its absence is a graceful rejection naming the directive.
    const CODEC: &str =
        "hex28 = bytes ; @custom_serialize write_hex @custom_deserialize read_hex\n";
    let no_decl = run(
        &format!("{MD}{CODEC}t = {{ * hex28 => uint, * md => md }}\n"),
        "nodecl",
    )
    .expect_err("a custom-codec typed key with no declared major must reject");
    assert!(
        no_decl.contains("@custom_wire_major") && no_decl.contains("the codec owns that wire"),
        "the missing-declaration message must name the directive and the reason, got: {no_decl}"
    );
    // …and with the declaration the typed row dispatches on the DECLARED major (text), not on the
    // replaced type's (bytes).
    let declared = run(
        &format!(
            "{MD}hex28 = bytes ; @custom_serialize write_hex @custom_deserialize read_hex @custom_wire_major text\nt = {{ * hex28 => uint, * uint => uint }}\n"
        ),
        "declared",
    )
    .expect("a declared major must be honored");
    assert!(
        src(&declared).contains("cbor_event::Type::Text =>"),
        "the DECLARED major drives the dispatch arm, not the replaced type's, got:\n{}",
        src(&declared)
    );
    // no-silent-directive: a declaration no open-table typed row consumes.
    let unconsumed = run(
        "hex28 = bytes ; @custom_serialize write_hex @custom_deserialize read_hex @custom_wire_major text\nf = { x: hex28 }\n",
        "unconsumed",
    )
    .expect_err("an unconsumed @custom_wire_major must reject");
    assert!(
        unconsumed.contains("nothing consumes the declared major"),
        "an inert declaration must reject loudly, got: {unconsumed}"
    );

    // --- JSON: a HAND-WRITTEN serde pair, never the derives ---
    // Two `#[serde(flatten)]` members cannot express this shape in either direction (serde hands
    // every unmatched member to BOTH on read; both write into one map with no dedup), so the
    // derives are suppressed and the impls emitted. What is asserted here is that SHAPE choice; the
    // behavior it buys is executed in `tests/open-table-json-e2e`.
    let json = run_flags(
        &format!("{MD}t = {{ * bstr => uint, * md => md }}\n"),
        "json",
        &["--json-serde-derives=true", "--json-schema-export=true"],
    )
    .expect("an open table must generate under the JSON flags");
    let json_src = src(&json);
    assert!(
        json_src.contains("impl serde::Serialize for T")
            && json_src.contains("impl<'de> serde::Deserialize<'de> for T")
            && json_src.contains("impl schemars::JsonSchema for T"),
        "the open table must carry hand-written JSON impls, got:\n{json_src}"
    );
    assert!(
        !json_src.contains("#[serde(flatten)]"),
        "no flatten attribute may survive on an open table's rows, got:\n{json_src}"
    );
    assert!(
        !json_src.contains("derive(Clone, Debug, serde::Deserialize, serde::Serialize"),
        "the serde derives must be suppressed on the minted struct, got:\n{json_src}"
    );
    // The schema names BOTH ranges and NEITHER key: that is what keeps a key type with no
    // `schemars::JsonSchema` impl from becoming an E0277 inside a generated file.
    assert!(
        json_src.contains("open_table_schema(typed_range, captured_range)"),
        "the open table's schema must be the two-range open object, got:\n{json_src}"
    );
    // `@custom_json` still wins: the user owns the impls and the tool emits none.
    let custom = run_flags(
        &format!("{MD}t = {{ * bstr => uint, * md => md }} ; @custom_json\n"),
        "customjson",
        &["--json-serde-derives=true"],
    )
    .expect("an open table with @custom_json must generate");
    assert!(
        !src(&custom).contains("impl serde::Serialize for T"),
        "@custom_json must leave the JSON face to the user, got:\n{}",
        src(&custom)
    );
}

/// An open table's COMPONENT face carries a getter for BOTH rows. The WIT projection walked only
/// the catch-all before this shape existed, so a typed row would have crossed the component boundary
/// SILENTLY ABSENT — the cross-crate loss class, which is worse than an unprojectable error because
/// the consumer's build succeeds. Both getters, and the `list<tuple<…>>` spelling each takes.
#[test]
fn open_table_component_face_projects_both_rows() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_open_table_wit_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "pid = bytes .size 4\nmd = uint / text\nlabels = { * pid => uint, * md => md }\n",
    )
    .unwrap();
    let cli = Cli::parse_from(vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        path.to_str().unwrap().to_owned(),
        "--output".to_owned(),
        "open_table_wit_unused".to_owned(),
        "--wasm=true".to_owned(),
        "--component=true".to_owned(),
    ]);
    let out = crate::api::generated_strings(&cli).expect("an open table must project to WIT");
    std::fs::remove_file(&path).ok();
    let wit = out
        .iter()
        .find(|(name, _)| name.ends_with(".wit"))
        .map(|(_, body)| body.clone())
        .unwrap_or_else(|| {
            panic!(
                "no .wit file generated; got {:?}",
                out.keys().collect::<Vec<_>>()
            )
        });
    assert!(
        wit.contains("entries: func() -> list<tuple<pid, u64>>"),
        "the TYPED row needs its own getter in the component face, got:\n{wit}"
    );
    assert!(
        wit.contains("rest: func() -> list<tuple<md, md>>"),
        "the catch-all keeps its getter, got:\n{wit}"
    );
}

/// Generate `cddl` with `--wasm=true` and return the emitted sources joined, for the open-table wasm
/// pins below.
#[cfg(test)]
fn open_table_wasm_src(cddl: &str, tag: &str) -> String {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_open_table_wasm_{tag}_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, cddl).unwrap();
    let cli = Cli::parse_from(vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        path.to_str().unwrap().to_owned(),
        "--output".to_owned(),
        "open_table_wasm_unused".to_owned(),
        "--wasm=true".to_owned(),
    ]);
    let out = crate::api::generated_strings(&cli)
        .unwrap_or_else(|e| panic!("an open table must generate under --wasm=true: {e}"));
    std::fs::remove_file(&path).ok();
    out.values().cloned().collect::<Vec<_>>().join("\n")
}

/// An open table's wasm class carries the TYPED row's map surface FLATTENED onto itself —
/// `len`/`insert`/`get`/`keys` (plus `has` when the value is nullable) delegating to the typed
/// container field — beside the read-only `rest()` getter for the catch-all. The flattening is the
/// set nominal's call, for its reason: a wasm class has no `Deref`, so a container getter would make
/// every JS read two layers deep (`t.entries().get(k)` rather than `t.get(k)`).
///
/// The consequence that matters beyond ergonomics is pinned here too: the typed row mints NO map
/// container class at all, which is what leaves the collision-detector family without a fifth
/// sibling for this kind. The catch-all keeps its container, in whichever flavor the row carries.
#[test]
fn open_table_wasm_class_flattens_the_typed_row() {
    let src = open_table_wasm_src(
        "pid = bytes .size 4\n\
         md = uint / text\n\
         labels = { * pid => uint, * md => md }\n",
        "flatten",
    );
    for member in [
        "pub fn len(&self) -> usize {",
        "pub fn insert(&mut self, key: &Pid, value: u64) -> Option<u64> {",
        "pub fn get(&self, key: &Pid) -> Option<u64> {",
        "pub fn keys(&self) -> PidList {",
        "pub fn rest(&self) -> MapMdToMd {",
    ] {
        assert!(
            src.contains(member),
            "the open table's wasm class must expose `{member}`, got:\n{src}"
        );
    }
    assert!(
        src.contains("self.0.entries.insert(") && src.contains("self.0.entries.keys()"),
        "the flattened accessors must delegate to the TYPED container field, got:\n{src}"
    );
    assert!(
        !src.contains("pub fn entries(&self)"),
        "the typed row has no whole-map getter — its surface is flattened, got:\n{src}"
    );
    assert!(
        !src.contains("MapPidToU64"),
        "the typed row must mint no map container class of its own (that is what makes its \
         `MapKToV` collision leg unrepresentable), got:\n{src}"
    );
}

/// The flavors and shapes the flattened surface has to keep working across: a `@duplicates preserve`
/// catch-all returns the PairMap-backed twin from `rest()`; `@name` renames the catch-all's getter
/// without touching the flattened members (which are named by the ACCESSOR, not the row); a nullable
/// typed value grows the `has` accessor exactly as a table wrapper's does; and a wasm-native typed
/// key returns a bare `Vec` from `keys()` rather than a minted class.
#[test]
fn open_table_wasm_class_carries_every_row_flavor() {
    let preserve = open_table_wasm_src(
        "pid = bytes .size 4\n\
         md = uint / text\n\
         dup = {\n  * pid => uint ; @duplicates preserve\n  ,\n  * md => md ; @duplicates preserve\n}\n",
        "preserve",
    );
    assert!(
        preserve.contains("pub fn rest(&self) -> PairMapMdToMd {"),
        "a `@duplicates preserve` catch-all returns the PairMap-backed twin, got:\n{preserve}"
    );
    assert!(
        preserve.contains("pub fn keys(&self) -> PidList {"),
        "a preserve TYPED row keeps the flattened surface, got:\n{preserve}"
    );

    let named = open_table_wasm_src(
        "pid = bytes .size 4\n\
         md = uint / text\n\
         named = {\n  * pid => uint ; @name typed\n  ,\n  * md => md ; @name captured\n}\n",
        "named",
    );
    assert!(
        named.contains("pub fn captured(&self) -> MapMdToMd {")
            && named.contains("self.0.typed.insert("),
        "`@name` renames the catch-all's getter and the typed row's backing field, never the \
         flattened accessor names, got:\n{named}"
    );

    let nullable = open_table_wasm_src(
        "pid = bytes .size 4\n\
         md = uint / text\n\
         nl = { * pid => (uint / null), * md => md }\n",
        "nullable",
    );
    assert!(
        nullable.contains("pub fn has(&self, key: &Pid) -> bool {"),
        "a nullable typed value grows `has`, the same flatten convention a table wrapper uses, \
         got:\n{nullable}"
    );

    let native = open_table_wasm_src(
        "md = uint / text\nnk = { * text => uint, * md => md }\n",
        "native",
    );
    assert!(
        native.contains("pub fn keys(&self) -> Vec<String> {"),
        "a wasm-native typed key returns a bare Vec, got:\n{native}"
    );
}

/// The typed row's keys list is named off the key's USE-SITE ident, never its resolved one: two
/// aliases of one underlying type key two open tables and mint two DISTINCT list classes. This is
/// what keeps a locally-minted `<AliasV1>List` from collapsing onto a `<Base>List` that an
/// `@extern_companions` filing has deferred to a dependency — normalizing `K_t` before naming its
/// keys list would silently defer the local one too.
#[test]
fn open_table_keys_list_is_named_off_the_typed_key_alias() {
    let src = open_table_wasm_src(
        "pid = bytes .size 4\n\
         pid_v1 = pid\n\
         md = uint / text\n\
         v2 = { * pid => uint, * md => md }\n\
         v1 = { * pid_v1 => uint, * md => md }\n",
        "alias",
    );
    assert!(
        src.contains("pub fn keys(&self) -> PidList {")
            && src.contains("pub fn keys(&self) -> PidV1List {"),
        "each open table's keys() must name its OWN key ident's list, got:\n{src}"
    );
    assert!(
        src.contains("pub struct PidV1List(") && src.contains("pub struct PidList("),
        "both list classes must be minted, got:\n{src}"
    );
}

/// The three collision legs an open table owes the wasm wrapper-name detector family. It gets no
/// sibling detector of its own — its class is named by the rule ident, which is the author's own
/// name — but it DOES mint two structural classes, and each is a leg on an existing detector:
/// the `<K_t>List` its flattened `keys()` returns, and the catch-all row's map class in whichever
/// flavor the row carries.
#[test]
fn open_table_wasm_wrapper_ident_collisions_reject_gracefully() {
    let run = |cddl: &str, tag: &str| -> String {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_open_table_collide_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, cddl).unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "open_table_collide_unused",
            "--wasm=true",
        ]));
        std::fs::remove_file(&path).ok();
        result
            .expect_err("a rule claiming an open table's wasm wrapper ident must be a graceful Err")
            .to_string()
    };

    // (a) the TYPED row's keys() list — the direct-claim leg. Only the flattened `keys()` mints
    // this class, so a walk reading the catch-all alone would let the rule shadow it silently.
    let msg = run(
        "pid = bytes .size 4\n\
         pid_list = [x: uint]\n\
         md = uint / text\n\
         labels = { * pid => uint, * md => md }\n\
         user = [p: pid_list]\n",
        "keys",
    );
    assert!(
        msg.contains("PidList")
            && msg.contains("an open table's keys() wrapper of the same element"),
        "the direct-claim leg must name the open table's keys() wrapper, got: {msg}"
    );

    // (b) the CATCH-ALL row's default-flavored map class.
    let msg = run(
        "pid = bytes .size 4\n\
         map_u64_to_text = [x: uint]\n\
         labels = { * pid => uint, * uint => text }\n\
         user = [p: map_u64_to_text]\n",
        "catchall",
    );
    assert!(
        msg.contains("MapU64ToText")
            && msg.contains("the open table catch-all row of 'Labels'")
            && msg.contains("loose map wrapper"),
        "the catch-all leg must name the open table's catch-all row and the DEFAULT flavor, got: \
         {msg}"
    );

    // (c) the CATCH-ALL row's `@duplicates preserve` map class — the flavored twin, in the
    // pair-map detector's own voice.
    let msg = run(
        "pid = bytes .size 4\n\
         pair_map_u64_to_text = [x: uint]\n\
         labels = {\n  * pid => uint\n  ,\n  * uint => text ; @duplicates preserve\n}\n\
         user = [p: pair_map_u64_to_text]\n",
        "catchallpreserve",
    );
    assert!(
        msg.contains("PairMapU64ToText")
            && msg.contains("the `@duplicates preserve` catch-all row of 'Labels'")
            && msg.contains("PairMap wrapper"),
        "the preserve catch-all leg must name the flavored row and the PairMap twin, got: {msg}"
    );
}

/// `--emit-tests` mints an entry into BOTH rows, not just the catch-all. Without the typed half every
/// generated round-trip of the shape the feature exists for would carry an empty typed region — the
/// wire-major dispatch would never execute — and without the COMBINED case nothing would put two
/// dynamic sequences on the wire at once, which is what the tagged order encoding and the
/// both-regions canonical key merge exist for. The standalone round-trip arm itself needs no new
/// code: the minted struct is a Record, so it has a real `Serialize`/`Deserialize` rather than the
/// transparent `pub type` a table lowers to (which is why the Table arm's skip is permanent).
#[test]
fn open_table_emit_tests_mint_both_rows() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_open_table_emit_{}.cddl",
        std::process::id()
    ));
    std::fs::write(
        &path,
        "md = uint / text\nlabels = { * bstr => uint, * md => md }\n",
    )
    .unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "open_table_emit_unused",
        "--wasm=false",
        "--emit-tests=true",
    ]))
    .expect("an open table must generate under --emit-tests");
    std::fs::remove_file(&path).ok();
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("fn roundtrip_labels()"),
        "the minted struct owns a REAL standalone round-trip arm (it is a Record, not a \
         transparent table alias), got:\n{src}"
    );
    // one mint per row plus the combined case, on top of the empty baseline
    let mints = src.matches("let mut v = Labels::new();").count();
    assert!(
        mints == 3,
        "expected a typed-row case, a catch-all case and a both-rows case (3 mints), got {mints} \
         in:\n{src}"
    );
    assert!(
        src.contains("v.entries.insert(") && src.contains("v.rest.insert("),
        "each row is minted through its OWN container field, got:\n{src}"
    );
}

/// The MEMBER-name hazard flattening creates, and the only one it does: the typed row's accessors
/// and the catch-all row's getter land on ONE wasm impl, so a `@name`d catch-all spelling a
/// flattened accessor name would emit two methods of one name (E0592 in the generated crate). All
/// five names are reserved unconditionally, and only when wasm bindings are generated — a rust-only
/// crate has no such class and the row name is free.
#[test]
fn open_table_catch_all_named_for_a_flattened_accessor_rejects_gracefully() {
    let run = |wasm: &str| -> Result<std::collections::BTreeMap<String, String>, String> {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_open_table_accessor_{}_{}.cddl",
            wasm.len(),
            std::process::id()
        ));
        std::fs::write(
            &path,
            "pid = bytes .size 4\n\
             md = uint / text\n\
             clash = {\n  * pid => uint\n  ,\n  * md => md ; @name keys\n}\n",
        )
        .unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "open_table_accessor_unused",
            wasm,
        ]))
        .map_err(|e| e.to_string());
        std::fs::remove_file(&path).ok();
        result
    };
    let msg = run("--wasm=true")
        .expect_err("a catch-all named for a flattened accessor must be a graceful Err");
    assert!(
        msg.contains("the open table 'Clash' names its catch-all row 'keys'")
            && msg.contains("`get`, `has`, `insert`, `keys`, `len`"),
        "the message must name the row, the offending name and the full reserved set, got: {msg}"
    );
    run("--wasm=false")
        .expect("without wasm bindings there is no class to collide on, so the name is free");
}

/// A rest row's per-entry VALUE encoding sidecar is populated from the LOCAL vars the value's
/// deserialize bound (named off the fixed `rest_value` binding), never from the sidecar
/// DECLARATION's names (named off the row's field name). The two coincide only for the default
/// field name `rest`, so a `@name`d row with a concrete (sidecar-bearing) value type emitted an
/// undefined variable and the generated crate failed to compile (E0425). Pinned at the source level
/// because the shape is one line of emitted code and the failure is a compile error in the OUTPUT
/// crate, not in this one.
#[test]
fn named_rest_row_value_sidecar_reads_the_bound_locals() {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_named_rest_sidecar_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, "foo = { 1: uint, * uint => text ; @name extra\n}\n").unwrap();
    let cli = Cli::parse_from(vec![
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        path.to_str().unwrap().to_owned(),
        "--output".to_owned(),
        "named_rest_unused".to_owned(),
        "--wasm=false".to_owned(),
        "--preserve-encodings=true".to_owned(),
    ]);
    let out = crate::api::generated_strings(&cli).expect("a @name'd rest row must generate");
    let src = out.values().cloned().collect::<Vec<_>>().join("\n");
    assert!(
        src.contains("extra_value_encodings.insert(rest_key, rest_value_encoding);"),
        "the sidecar must be keyed by its DECLARATION name and fed the BOUND local, got:\n{src}"
    );
    assert!(
        !src.contains("extra_value_encodings.insert(rest_key, extra_value_encoding)"),
        "the declaration's name is not a local binding here, got:\n{src}"
    );
}

/// A multi-arm group-choice arm builds its record through the normal registration path, so the arm's
/// name occupies a Rust struct ident while it is parsed. When that ident is already claimed and the
/// arm's record SURVIVES parsing (a non-embeddable arm — it is emitted as a real type under that
/// name), two types demand one name and the spec is rejected GRACEFULLY (`record_rejection` →
/// drained by `finalize`), never a `panic!` and never a silent winner.
///
/// Before this was detected, the loser was decided by rule ORDER: the second claimant's registration
/// overwrote the first, so the surviving type carried one arm's name and the other's wire shape, and
/// a spec generated wrong-but-compiling code with no diagnostic. The detection is therefore
/// order-INDEPENDENT — see `arm_ident_collision` — which the reordered vectors below pin: the same
/// two claimants must reject whichever of them the topological rule order reaches first.
///
/// The EMBEDDABLE counterpart is legal and must keep generating; that is
/// `embeddable_group_choice_arm_may_share_a_rule_name` below.
#[test]
fn group_choice_arm_ident_collision_rejects_gracefully() {
    // Every arm below carries 2 non-fixed fields, which is what makes it non-embeddable
    // (`EnumVariant::can_embed_fields` allows at most 1) and so an emitted type of its own.
    let vectors = [
        (
            "arm_vs_rule",
            // `holder` forces `target` to be parsed BEFORE `second` (the order that used to delete
            // the `target` enum outright and panic at the first lookup of it).
            "target = [ ; @name a\n m: uint, tag: 0 //\n ; @name b\n n: uint, tag: 1 ]\n\
             holder = [ t: target ]\n\
             second = [ ; @name target\n x: uint, z: uint, tag: 0 //\n ; @name other\n y: holder, tag: 1 ]\n",
            "rule `second`",
        ),
        (
            "arm_vs_rule_reordered",
            // Same two claimants, opposite order: `second` is parsed first. Must still reject.
            "second = [ ; @name target\n x: uint, z: uint, tag: 0 //\n ; @name other\n y: uint, tag: 1 ]\n\
             holder = [ s: second ]\n\
             target = [ ; @name a\n m: holder, tag: 0 //\n ; @name b\n n: uint, tag: 1 ]\n",
            "`Target`",
        ),
        (
            // Neither claimant is a rule ident, so a check that consulted only the rule names would
            // miss this one and emit `Alpha::Shared` carrying `beta`'s arm shape AND `beta`'s tag.
            "arm_vs_arm",
            "alpha = [ ; @name shared\n a: uint, b: uint, tag: 0 //\n ; @name alpha_other\n c: uint, tag: 1 ]\n\
             holder = [ t: alpha ]\n\
             beta = [ ; @name shared\n p: text, q: text, tag: 2 //\n ; @name beta_other\n r: holder, tag: 3 ]\n",
            "rules `alpha` and `beta`",
        ),
        (
            // Both arms in ONE rule: same conflict, phrased for the single-rule case.
            "same_rule_two_arms",
            "foo = [ ; @name a\n x: uint, z: uint, tag: 0 //\n ; @name a\n y: text, w: uint, tag: 1 ]\n",
            "two of its group-choice arms",
        ),
        (
            "arm_vs_own_rule",
            "foo = [ ; @name foo\n x: uint, z: uint, tag: 0 //\n ; @name other\n y: uint, tag: 1 ]\n",
            "the same name as the rule itself",
        ),
        (
            // No `@name` anywhere: an arm with no directive is named `{rule}{index}`, which claims an
            // ident just as an `@name`d one does.
            "default_arm_name_vs_rule",
            "second0 = [ ; @name a\n m: uint, tag: 9 //\n ; @name b\n n: uint, tag: 8 ]\n\
             holder = [ t: second0 ]\n\
             second = [ x: uint, z: uint, tag: 0 //\n y: holder, w: uint, tag: 1 ]\n",
            "`Second0`",
        ),
    ];
    for (tag, spec, expected) in vectors {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains(expected),
            "[{tag}] rejection must name the conflict (`{expected}`), got:\n{err}\nspec:\n{spec}"
        );
        assert!(
            err.contains("Two types cannot share one name"),
            "[{tag}] rejection must explain the conflict, got:\n{err}"
        );
    }
}

/// An EMBEDDABLE group-choice arm may share a name with a rule: its record is pulled straight back
/// out (`remove_rust_struct`) and inlined into the enum variant, so it is never emitted under that
/// name and nothing is contested — only the variant DISPLAY name survives, and that lives in its own
/// namespace. This shape is shipped public API for consumers (CML spells `credential`'s and `d_rep`'s
/// arms `@name Script` alongside a `script` rule, generating `Credential::Script` / `DRep::Script`),
/// so it must keep generating.
///
/// It kept generating only by ORDER luck: the arm borrowed the contested ident to build its record
/// and then DELETED it, so an arm parsed after the rule erased that rule from the IR. Both orders are
/// asserted here, since a reference edge added anywhere else in the spec can flip which comes first.
#[test]
fn embeddable_group_choice_arm_may_share_a_rule_name() {
    // Each `@name script` arm has exactly ONE non-fixed field, which is what makes it embeddable.
    let vectors = [
        (
            // `script` parsed BEFORE the arm that shares its name (via the `.cbor` edge through
            // `script_ref`) — the ordering that deleted the `Script` enum.
            "rule_first",
            "script_hash = bytes .size 28\n\
             script_ref = #6.24(bytes .cbor script)\n\
             script = [ ; @name native\n tag: 0, s: uint //\n ; @name plutus\n tag: 1, s: text ]\n\
             d_rep = [ ; @name key\n 0, pool: script_ref //\n ; @name script\n 1, script_hash ]\n",
        ),
        (
            // The arm parsed first — the order this shape survived under before the fix.
            "arm_first",
            "script_hash = bytes .size 28\n\
             d_rep = [ ; @name key\n 0, pool: uint //\n ; @name script\n 1, script_hash ]\n\
             holder = [ d: d_rep ]\n\
             script = [ ; @name native\n tag: 0, s: holder //\n ; @name plutus\n tag: 1, s: text ]\n",
        ),
    ];
    for (tag, spec) in vectors {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_{tag}_arm_shadow_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "arm_shadow_unused",
        ]));
        std::fs::remove_file(&path).ok();
        let src = out
            .unwrap_or_else(|e| panic!("[{tag}] an embeddable same-named arm must generate: {e}"))
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        // the RULE keeps the contested name, with its own arms
        assert!(
            src.contains("pub enum Script {") && src.contains("Native(") && src.contains("Plutus("),
            "[{tag}] the `script` RULE's enum must survive intact, got:\n{src}"
        );
        // the ARM keeps its display name, inlined (not a reference to the rule's type)
        assert!(
            src.contains("Script(ScriptHash)"),
            "[{tag}] the arm's variant must stay named `Script` and carry its OWN field type, got:\n{src}"
        );
        // no synthesized registration name may reach the emitted source
        assert!(
            !src.contains("GroupChoiceArm"),
            "[{tag}] the synthesized registration ident must never be emitted, got:\n{src}"
        );
    }
}

/// Generic arm names recur across rules by nature (`first`/`second`, `key`/`value`), so two
/// non-embeddable arms in different rules routinely want one struct ident. When their records are
/// STRUCTURALLY IDENTICAL they are one type spelled twice, not two types fighting over a name: they
/// share the single generated struct, and no rejection fires. `tests/core/input.cddl` relies on this
/// — `non_overlap_basic_embed_multi_fields`, `non_overlap_basic_embed_mixed` and
/// `non_overlap_basic_embed_mixed_explicit` all spell a `; @name second` arm as `y: text, z: uint`.
///
/// Its counterpart, differing arms under one name, is a real conflict and is rejected — the
/// `arm_vs_arm` vector of `group_choice_arm_ident_collision_rejects_gracefully`. That pairing is the
/// whole point: name overlap alone is not the defect, a name carrying two different wire shapes is.
#[test]
fn identical_group_choice_arms_in_different_rules_share_one_struct() {
    const CDDL: &str = "alpha = [ ; @name shared\n a: uint, b: text, tag: 0 //\n \
                        ; @name alpha_other\n c: uint, d: uint, tag: 1 ]\n\
                        holder = [ t: alpha ]\n\
                        beta = [ ; @name shared\n a: uint, b: text, tag: 0 //\n \
                        ; @name beta_other\n r: holder, s: uint, tag: 3 ]\n";
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_identical_arms_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, CDDL).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "identical_arms_unused",
    ]));
    std::fs::remove_file(&path).ok();
    let src = out
        .expect("structurally identical same-named arms must share a struct, not reject")
        .values()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n");
    // exactly ONE struct carries the shared name, and both enums reference it
    assert_eq!(
        src.matches("pub struct Shared {").count(),
        1,
        "the shared arm must generate exactly one struct, got:\n{src}"
    );
    assert!(
        src.contains("Shared(Shared)"),
        "both enums must reference the shared struct by name, got:\n{src}"
    );
    // and the synthesized registration name never reaches the output
    assert!(
        !src.contains("GroupChoiceArm"),
        "the synthesized registration ident must never be emitted, got:\n{src}"
    );
}

/// The arms of one group choice all name variants of ONE generated enum, so their names share a
/// single namespace and two arms landing on the same one is a Rust `E0428` — a crate that does not
/// compile, with no diagnostic from this tool. When BOTH claimants are names the author spelled
/// (`; @name`), that is an authoring error and is rejected GRACEFULLY (`record_rejection` → drained
/// by `finalize`), naming both arms and the remedy: a variant name is public API of the generated
/// crate, so renaming one would silently ship a name nobody asked for.
///
/// This is the VARIANT-namespace sibling of
/// `group_choice_arm_ident_collision_rejects_gracefully`, which guards the STRUCT namespace. The two
/// are genuinely distinct seams and the vectors below are exactly the arms the struct-side check
/// cannot see: an EMBEDDABLE arm is pulled back out of the IR and inlined, so it registers no struct
/// and claims no struct ident — only its variant name survives. Structurally IDENTICAL arms are the
/// sharpest case: the struct side deliberately lets them SHARE one generated struct (see
/// `identical_group_choice_arms_in_different_rules_share_one_struct`), which is right across rules
/// but within ONE rule still leaves the enum declaring the variant twice.
#[test]
fn group_choice_arm_variant_name_collision_rejects_gracefully() {
    let vectors = [
        (
            // Two EMBEDDABLE arms (1 non-fixed field each, so no struct is ever registered) — the
            // struct-namespace check never fires and the enum got two `Foo::A` variants.
            "embeddable_arms",
            "foo = [ ; @name a\n x: uint, tag: 0 //\n ; @name a\n y: text, tag: 1 ]\n",
        ),
        (
            // The single-entry arm branch: no record is built at all, the arm's type goes straight
            // into the variant. Its own name is likewise only ever a variant name.
            "single_entry_arms",
            "foo = [ ; @name a\n uint //\n ; @name a\n text ]\n",
        ),
        (
            // MIXED: an embeddable arm and a non-embeddable one under one name. The embeddable arm
            // claims no struct ident, so the non-embeddable one sees a free name and emits
            // `A(A)` — a struct that is fine and a variant name that is already taken.
            "embeddable_vs_non_embeddable",
            "foo = [ ; @name a\n x: uint, tag: 0 //\n ; @name a\n y: text, w: uint ]\n",
        ),
        (
            // Structurally identical arms in ONE rule: the struct side shares the single `A` struct
            // by design, which leaves `A(A)` declared twice.
            "identical_arms_one_rule",
            "foo = [ ; @name a\n x: uint, z: text //\n ; @name a\n x: uint, z: text ]\n",
        ),
        (
            // The two spellings camel-case to one variant name, so the collision is real even
            // though the source names differ — the check must key on the GENERATED name.
            "differing_spellings_one_variant",
            "foo = [ ; @name my_arm\n uint //\n ; @name myArm\n text ]\n",
        ),
    ];
    for (tag, spec) in vectors {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains("Two variants cannot share one name"),
            "[{tag}] rejection must explain the conflict, got:\n{err}\nspec:\n{spec}"
        );
        assert!(
            err.contains("rule `foo`"),
            "[{tag}] rejection must name the owning rule, got:\n{err}"
        );
    }
}

/// A group-choice arm the author did NOT name carries a name the GENERATOR derived — from the arm's
/// sole member key, from its type, or from the arm's position (`{rule}{index}`). There is no
/// authorial intent behind such a name, so when it collides it yields: it takes a numeric suffix,
/// exactly as the type-choice path (`create_variants_from_type_choices`) already does for its own
/// derived names. Rejecting here would refuse a spec whose author named nothing.
///
/// Which of a colliding explicit/derived pair keeps the plain name is decided BEFORE the arm loop
/// runs — every arm's `@name` is reserved up front — so it never depends on the order the author
/// happened to write the two arms in. The author's name always wins.
#[test]
fn derived_group_choice_arm_variant_names_deduplicate() {
    let vectors = [
        (
            // Two arms whose names both come from their member key: nothing was authored, so both
            // are free to be renumbered and the spec must still generate.
            "member_key_derived",
            "foo = [ x: uint // x: text ]\n",
            vec!["X(u64)", "X2(String)"],
        ),
        (
            // An explicit `@name x` against a derived `x:` sibling — the authored name is kept and
            // the derived one moves.
            "explicit_beats_derived",
            "foo = [ ; @name x\n uint //\n x: text ]\n",
            vec!["X(u64)", "X2(String)"],
        ),
        (
            // Reversed source order, same outcome: reserving the `@name`s before the loop is what
            // makes the authored name win from either position.
            "explicit_beats_derived_reordered",
            "foo = [ x: text //\n ; @name x\n uint ]\n",
            vec!["X2(String)", "X(u64)"],
        ),
        (
            // The positional name a multi-entry arm gets (`{rule}{index}`) is derived too, so an
            // `@name` that happens to spell it keeps it and the positional one moves.
            "explicit_beats_positional",
            "foo = [ ; @name foo1\n x: uint, tag: 0 //\n y: text, tag: 1 ]\n",
            vec!["Foo1(u64)", "Foo12(String)"],
        ),
    ];
    for (tag, spec, expected) in vectors {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_variant_dedup_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let out = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "variant_dedup_unused",
        ]));
        std::fs::remove_file(&path).ok();
        let src = out
            .unwrap_or_else(|e| panic!("[{tag}] derived names must dedup, not reject: {e}\n{spec}"))
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        for variant in expected {
            assert!(
                src.contains(variant),
                "[{tag}] expected variant `{variant}` in the generated enum, got:\n{src}"
            );
        }
    }
}

/// Generate `spec` expecting SUCCESS, returning the concatenated generated source. The positive
/// control the `@custom_serialize`/`@custom_deserialize` placement rejections below each pair with:
/// a rejection is only attributable to the PLACEMENT if the same directives in their honored
/// position still generate their call sites.
fn expect_custom_codec_source(tag: &str, spec: &str) -> String {
    let path = std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
    std::fs::write(&path, spec).unwrap();
    let out = crate::api::generated_strings(&Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "custom_codec_unused",
    ]));
    std::fs::remove_file(&path).ok();
    out.unwrap_or_else(|e| panic!("[{tag}] spec must generate, got a rejection: {e}\n{spec}"))
        .into_values()
        .collect::<Vec<_>>()
        .join("\n")
}

/// The custom (de)serializer pair on a `_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
/// rule is rejected BY DESIGN — via a GRACEFUL `Err` (deferred through `record_rejection` → drained
/// by `finalize`), never a `panic!`. Either marker names a type this crate does not define, and
/// `RustStruct::new_extern` / `new_raw_bytes` both store `RustStructConfig::default()`, so the pair
/// never reaches generation: both directions emit the named type's own impls while the spec claims a
/// custom codec. The two markers are one class here (as they are for `@copy`), but the message names
/// the marker the rule actually spells — this says "invalid HERE", not `@copy`'s "valid only on X or
/// Y". The remedy the message advertises — a real CDDL body carrying the pair — is the control
/// asserted here, so the rejection is attributable to the MARKER rather than to the directives.
#[test]
fn custom_codec_pair_on_extern_rule_rejects_gracefully() {
    for (tag, marker, ident) in [
        ("custom_extern", "_CDDL_CODEGEN_EXTERN_TYPE_", "Ext"),
        ("custom_raw_bytes", "_CDDL_CODEGEN_RAW_BYTES_TYPE_", "Rb"),
    ] {
        let err = expect_graceful_rejection(
            tag,
            &format!(
                "{} = {marker} ; @custom_serialize my_ser @custom_deserialize my_deser\n\
                 holder = [f: {}]\n",
                ident.to_lowercase(),
                ident.to_lowercase()
            ),
            &[],
        );
        assert!(
            err.contains(&format!(
                "@custom_serialize on `{ident}`: a {marker} rule names a type this crate \
                 does not define, so that type owns its own serialization impls and the custom \
                 (de)serializer pair never reaches generation."
            )),
            "[{tag}] the rejection must name the directive, the marker and why it cannot be \
             honored, got:\n{err}"
        );
        // Both halves of the pair are reported, so an author who wrote only one is not left guessing.
        assert!(
            err.contains(&format!("@custom_deserialize on `{ident}`:")),
            "[{tag}] each half of the pair gets its own rejection line, got:\n{err}"
        );
        assert!(
            err.contains(
                "Give the rule a real CDDL body and put the pair there (`<rule> = text ; \
                 @custom_serialize <fn> @custom_deserialize <fn>`)"
            ),
            "[{tag}] the rejection must advertise the type-level alias spelling as the remedy, \
             got:\n{err}"
        );
        // The SECOND road, which keeps the marker's rust type: the pair on an alias OF this rule.
        // Advertised because it is what a consumer replacing only the WIRE of a hand-written type
        // wants — the body road would change the rust type too.
        assert!(
            err.contains(
                "put the pair on an ALIAS of it (`<alias> = <rule> ; @custom_serialize <fn> \
                 @custom_deserialize <fn>`)"
            ),
            "[{tag}] the rejection must advertise the alias-of-marker spelling as the second \
             remedy, got:\n{err}"
        );
    }
    // CONTROL: the advertised remedy really does route both directions through the custom fns.
    let src = expect_custom_codec_source(
        "custom_extern_control",
        "ext = text ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ext]\n",
    );
    assert!(
        src.contains("my_ser(") && src.contains("my_deser("),
        "the remedy spelling must emit both custom call sites, got:\n{src}"
    );
    // CONTROL for the second remedy, both marker flavors: the alias keeps the marker's rust type
    // (`pub type <Alias> = <Marker>;`) while the pair owns the wire. This is the "this rule IS that
    // type, written differently" spelling — executed end to end in `tests/alias-of-marker-e2e`
    // (raw-bytes flavor) and `tests/custom-encodings-e2e` (extern flavor, which additionally
    // declares its wire because a self-carrying type demands no encoding variables).
    for (tag, marker, ident) in [
        (
            "custom_extern_alias_control",
            "_CDDL_CODEGEN_EXTERN_TYPE_",
            "Ext",
        ),
        (
            "custom_raw_bytes_alias_control",
            "_CDDL_CODEGEN_RAW_BYTES_TYPE_",
            "Rb",
        ),
    ] {
        let base = ident.to_lowercase();
        let src = expect_custom_codec_source(
            tag,
            &format!(
                "{base} = {marker}\n\
                 {base}_v1 = {base} ; @custom_serialize my_ser @custom_deserialize my_deser\n\
                 holder = [f: {base}_v1]\n"
            ),
        );
        assert!(
            !src.contains(&format!("pub type {ident}V1"))
                && src.contains(&format!("pub f: {ident},"))
                && src.contains("my_ser(")
                && src.contains("my_deser("),
            "[{tag}] the alias road must resolve to the marker's type at the member position and \
             route both directions through the custom fns, while minting NO type of its own — a \
             `pub type {ident}V1 = {ident};` would hand the CDDL name a standalone codec (the \
             marker's built-in one) contradicting the wire every embed site writes, got:\n{src}"
        );
    }
}

/// The custom (de)serializer pair written in a collection ROW-ENTRY comment slot — a table row, an
/// open struct-map rest row, an open-array rest tail — is rejected BY DESIGN, via a GRACEFUL `Err`
/// (`record_rejection` → drained by `finalize`), never a `panic!`. That slot legitimately carries
/// `@name`/`@duplicates`/`@ignore`, all of which are row-SCOPED; the pair is a type-level override
/// keyed on a type the row does not declare, so it was read into the row's metadata and dropped.
///
/// The controls are what make this a PLACEMENT rejection rather than a directive one: the same
/// directives keep working at field position and on a key/value RULE (the spelling the message
/// advertises), and the row slot's other directives keep working on the very rows rejected here.
#[test]
fn custom_codec_pair_in_row_entry_slot_rejects_gracefully() {
    let vectors = [
        (
            "map_rest_row",
            "opn = {\n  1: uint,\n  * text => uint ; @custom_serialize my_ser @custom_deserialize my_deser\n}\n",
            "@custom_serialize on the open struct-map rest row (`* k => v`) of rule `opn`:",
            "Name the row's key or value type as its own rule and put the pair there (`k = text ; \
             @custom_serialize <fn> @custom_deserialize <fn>`, then `* k => v`).",
        ),
        (
            "array_rest_tail",
            "opa = [\n  a: uint,\n  * uint ; @custom_serialize my_ser @custom_deserialize my_deser\n]\n",
            "@custom_serialize on the open-array rest tail (`* t`) of rule `opa`:",
            "Name the tail element type as its own rule and put the pair there (`e = uint ; \
             @custom_serialize <fn> @custom_deserialize <fn>`, then `* e`).",
        ),
        (
            "table_row",
            "t = {\n  * text => uint ; @custom_serialize my_ser @custom_deserialize my_deser\n}\nholder = [f: t]\n",
            "@custom_serialize on the table row (`* k => v`) of rule `t`:",
            "Name the table's key or value type as its own rule and put the pair there (`k = text \
             ; @custom_serialize <fn> @custom_deserialize <fn>`, then `{ * k => v }`).",
        ),
    ];
    for (tag, spec, head, remedy) in vectors {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains(head),
            "[{tag}] the rejection must name the directive and the row shape it sits on, got:\n{err}"
        );
        assert!(
            err.contains(
                "the custom (de)serializer pair is a TYPE-level override keyed on the type whose \
                 codec it replaces, and a row entry declares no type of its own, so it is not \
                 honored in this slot."
            ),
            "[{tag}] the rejection must explain why the slot cannot honor it, got:\n{err}"
        );
        assert!(
            err.contains(remedy),
            "[{tag}] the rejection must advertise the key/value/element rule spelling, got:\n{err}"
        );
        assert!(
            err.contains("@custom_deserialize on the "),
            "[{tag}] each half of the pair gets its own rejection line, got:\n{err}"
        );
    }
    // CONTROL 1: the remedy — the pair on the row's key RULE — generates and calls both fns.
    let src = expect_custom_codec_source(
        "custom_row_entry_control_rule",
        "k = text ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         t = { * k => uint }\nholder = [f: t]\n",
    );
    assert!(
        src.contains("my_ser(") && src.contains("my_deser("),
        "the advertised key-rule spelling must emit both custom call sites, got:\n{src}"
    );
    // CONTROL 2: the SAME comment placement in the FIELD slot is honored, so the rejection is about
    // the row entry, not about a comment the DSL never sees.
    let src = expect_custom_codec_source(
        "custom_row_entry_control_field",
        "holder = [\n  f: bytes, ; @custom_serialize my_ser @custom_deserialize my_deser\n]\n",
    );
    assert!(
        src.contains("my_ser(") && src.contains("my_deser("),
        "the field slot must still honor the pair, got:\n{src}"
    );
    // CONTROL 3: the row slot's own directives still work on the very rows rejected above — only the
    // custom pair is refused there.
    let src = expect_custom_codec_source(
        "custom_row_entry_control_slot",
        "opn = {\n  1: uint,\n  * text => uint ; @name extras\n}\n",
    );
    assert!(
        src.contains("pub extras"),
        "`@name` in the rest-row slot must still rename the captured field, got:\n{src}"
    );
}

/// `@no_alias` beside the custom (de)serializer pair is ACCEPTED and REDUNDANT: it asks for exactly
/// what the pair already guarantees, and both are honored.
///
/// It used to be refused, because `resolve_alias` stripped the alias node whenever the rule emitted
/// no `pub type` — and that node is the key the emitters look the pair up by, so `@no_alias` really
/// did take the pair with it and drop BOTH directions to the default wire (a symmetric drop no
/// round-trip test can see). Node survival no longer keys on emission: an entry keeps its node if it
/// emits a type OR carries a pair. A pair-carrying alias then suppresses its own projection anyway,
/// which is precisely what the directive was asking for.
///
/// The acceptance criterion is byte-identity, not merely "generates": the directive must be a no-op
/// in the presence of the pair, on every emitted file. Asserting the whole file map equal is what
/// makes that a statement about the OUTPUT rather than about the two lines a substring check would
/// look at.
#[test]
fn no_alias_beside_a_custom_codec_pair_is_accepted_and_generates_byte_identically() {
    let with_directive = expect_custom_codec_source(
        "custom_no_alias",
        "cb = bytes ; @no_alias @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: cb]\n",
    );
    let without = expect_custom_codec_source(
        "custom_no_alias_control",
        "cb = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: cb]\n",
    );
    assert_eq!(
        with_directive, without,
        "`@no_alias` beside a complete pair must be a byte-identical no-op: the pair already \
         suppresses the type projection the directive asks to remove"
    );
    assert!(
        with_directive.contains("my_ser(") && with_directive.contains("my_deser("),
        "control: the combination must still route BOTH directions through the pair — the retired \
         refusal existed because the node carrying that routing was dropped, so its survival is \
         what the acceptance rests on, got:\n{with_directive}"
    );
    assert!(
        !with_directive.contains("pub type Cb"),
        "control: the suppression the directive redundantly requests is genuinely in effect, so \
         the byte-identity above is not two copies of an emitted alias, got:\n{with_directive}"
    );
}

/// `@newtype` together with the custom (de)serializer pair is rejected BY DESIGN, via a GRACEFUL
/// `Err` (`record_rejection` → drained by `finalize`), never a `panic!`. This one is not a drop but
/// an ASYMMETRY: the deserialize call sites do route through the custom reader while the wrapper
/// writes through its generated `Serialize` impl (`wrappers.rs` has no custom handling), so the
/// generated type reads one wire format and writes another — silent wire divergence that a
/// generated-crate round-trip cannot expose either.
///
/// Both wrapper flavors are covered: the primitive newtype and the collection newtype (`[* uint]`),
/// which reaches the wrapper through `parse_group_choice` rather than the leaf `parse_type` arm.
#[test]
fn custom_codec_pair_with_newtype_rejects_gracefully() {
    let vectors = [
        (
            "custom_newtype_primitive",
            "nt = bytes ; @newtype @custom_serialize my_ser @custom_deserialize my_deser\n\
             holder = [f: nt]\n",
        ),
        (
            "custom_newtype_collection",
            "nt = [* uint] ; @newtype @custom_serialize my_ser @custom_deserialize my_deser\n\
             holder = [f: nt]\n",
        ),
    ];
    for (tag, spec) in vectors {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains(
                "@custom_serialize together with `@newtype` on `Nt`: a `@newtype` wrapper writes \
                 through its own generated serialize impl while the deserialize CALL SITES do \
                 route through the custom reader, so the pair would make the wrapper read one wire \
                 format and write another."
            ),
            "[{tag}] the @newtype rejection must state the round-trip asymmetry, got:\n{err}"
        );
        assert!(
            err.contains(
                "Drop `@newtype` and use the plain alias spelling (`<rule> = <body> ; \
                 @custom_serialize <fn> @custom_deserialize <fn>`), or declare the type \
                 `_CDDL_CODEGEN_EXTERN_TYPE_` and hand-write it in full."
            ),
            "[{tag}] the rejection must offer the alias spelling and the hand-owned type, got:\n{err}"
        );
        assert!(
            err.contains("@custom_deserialize together with `@newtype` on `Nt`:"),
            "[{tag}] each half of the pair gets its own rejection line, got:\n{err}"
        );
    }
    // CONTROL: without `@newtype` the identical rule honors the pair.
    let src = expect_custom_codec_source(
        "custom_newtype_control",
        "nt = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: nt]\n",
    );
    assert!(
        src.contains("my_ser(") && src.contains("my_deser("),
        "the same rule without @newtype must emit both custom call sites, got:\n{src}"
    );
}

/// The custom (de)serializer pair on an ENUM rule — a type choice, a group choice, or the
/// fixed-value C-style enum — is rejected BY DESIGN in EVERY spelling (single-half or both), via a
/// GRACEFUL `Err` (`record_rejection` → drained by `finalize`), never a `panic!`. This is the same
/// class `@newtype` is rejected for and not a drop: the enum's serialize side is generated
/// unconditionally while `generate_deserialize`'s `Root(Rust(ident))` arm rewrites every embed site
/// to the named reader, so the type would read one wire format and write another. Unlike the
/// parse-walk rejections this one keys on the minted struct's KIND, so it lives in `finalize` —
/// which is also what makes it see a generic instance's struct.
///
/// The control is the remedy the message advertises: the pair on the rule of a VARIANT's type,
/// which emits both call sites inside the enum's own arms.
#[test]
fn custom_codec_pair_on_enum_rule_rejects_gracefully() {
    let vectors = [
        (
            "custom_type_choice_both",
            "ch = uint ; @name a\n   / text ; @name b @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ch]\n",
            "a type-choice rule (`a / b`)",
            "Ch",
            true,
        ),
        (
            // SINGLE-HALF: an enum rejects either half on its own too (a record does not — see the
            // sibling test — because a record's both-set spelling has its own suppressed-impls story).
            "custom_type_choice_ser_only",
            "ch = uint ; @name a\n   / text ; @name b @custom_serialize my_ser\nholder = [f: ch]\n",
            "a type-choice rule (`a / b`)",
            "Ch",
            false,
        ),
        (
            "custom_type_choice_deser_only",
            "ch = uint ; @name a\n   / text ; @name b @custom_deserialize my_deser\nholder = [f: ch]\n",
            "a type-choice rule (`a / b`)",
            "Ch",
            false,
        ),
        (
            "custom_group_choice_both",
            "gc = [ ; @name a\n  x: uint //\n  ; @name b\n  y: text ] ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: gc]\n",
            "a group-choice rule (`{ … } // { … }`)",
            "Gc",
            true,
        ),
        (
            // The dataless C-style shape mints a different `RustStructType` than the data-carrying
            // type choice above, so it needs its own vector to prove the match arm covers it.
            "custom_c_style_enum_both",
            "ce = 0 ; @name zero\n   / 1 ; @name one @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ce]\n",
            "a fixed-value type-choice rule (`0 / 1`, a C-style enum)",
            "Ce",
            true,
        ),
    ];
    for (tag, spec, shape, ident, both) in vectors {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains(&format!(
                "on `{ident}`: {shape} mints an enum whose serialize side is generated \
                 unconditionally, while the deserialize CALL SITES do route through the custom \
                 reader — so the pair would make the enum read one wire format and write another."
            )),
            "[{tag}] the rejection must name the enum shape and state the round-trip asymmetry, \
             got:\n{err}"
        );
        assert!(
            err.contains(&format!(
                "Put the pair on the rule of the variant type that needs the custom format, or \
                 declare `{ident}` as a _CDDL_CODEGEN_EXTERN_TYPE_ rule and hand-write the type in \
                 full."
            )),
            "[{tag}] the rejection must offer the variant-rule spelling and the hand-owned type, \
             got:\n{err}"
        );
        if both {
            assert!(
                err.contains(&format!("@custom_serialize on `{ident}`:"))
                    && err.contains(&format!("@custom_deserialize on `{ident}`:")),
                "[{tag}] each half of the pair gets its own rejection line, got:\n{err}"
            );
        }
    }
    // CONTROL: the advertised remedy — the pair on a VARIANT's own type rule — generates, and the
    // custom calls land inside the enum's serialize/deserialize arms.
    let src = expect_custom_codec_source(
        "custom_enum_control",
        "inner = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         ch = uint ; @name a\n   / inner ; @name b\nholder = [f: ch]\n",
    );
    assert!(
        src.contains("Ch::B(b) => my_ser(serializer, b)") && src.contains("Ch::B(my_deser(raw)?)"),
        "the variant-rule spelling must route both directions inside the enum arms, got:\n{src}"
    );
}

/// A SINGLE HALF of the custom (de)serializer pair on a named RECORD rule is rejected BY DESIGN, via
/// a GRACEFUL `Err` (`record_rejection` → drained by `finalize`), never a `panic!`. The two halves
/// fail differently and get distinct messages:
///
/// * `@custom_serialize` alone emits NO `Serialize` impl for the type and never calls the named
///   function — the generated crate does not compile (probed: E0599/E0277 at every embed site), with
///   no diagnostic from this tool.
/// * `@custom_deserialize` alone keeps the type's own generated `Deserialize` impl while rewriting
///   every embed site to the named function, so `Foo::from_cbor_bytes` and a field of type `Foo`
///   decode the same bytes differently — and the rule projects OPAQUELY across the extern-interface
///   seam (verified: it exports as a `_CDDL_CODEGEN_EXTERN_TYPE_` row, so `CustomSerializeTransparent`
///   never fires), carrying the divergence to consumers.
///
/// The BOTH-SET spelling is deliberately NOT rejected — it gets thin generated impls delegating to
/// the named pair, so standalone APIs and embed sites own the same complete-item wire contract.
#[test]
fn single_half_custom_codec_on_record_rule_rejects_gracefully() {
    let err = expect_graceful_rejection(
        "custom_record_ser_only",
        "myrec = [a: uint] ; @custom_serialize my_ser\nholder = [f: myrec]\n",
        &[],
    );
    assert!(
        err.contains(
            "@custom_serialize alone on `Myrec`: a record rule with only the serialize half emits \
             no `Serialize` impl for the type and never calls the named function, so the generated \
             crate does not compile — every site holding a `Myrec` calls `.serialize(..)` on a type \
             that has no impl."
        ),
        "the serialize-only rejection must name the non-compiling outcome, got:\n{err}"
    );

    let err = expect_graceful_rejection(
        "custom_record_deser_only",
        "myrec = [a: uint] ; @custom_deserialize my_deser\nholder = [f: myrec]\n",
        &[],
    );
    assert!(
        err.contains(
            "@custom_deserialize alone on `Myrec`: a record rule with only the deserialize half \
             still emits the type's own generated `Deserialize` impl, while every site holding a \
             `Myrec` is rewritten to call the named function — so `Myrec::from_cbor_bytes` and a \
             field of type `Myrec` decode the same bytes differently."
        ),
        "the deserialize-only rejection must name the two-ways-to-decode divergence, got:\n{err}"
    );
    assert!(
        err.contains(
            "The rule also projects OPAQUELY across the extern-interface seam, so a consumer \
             decodes it the generated way."
        ),
        "the deserialize-only rejection must name the cross-crate leg, got:\n{err}"
    );
    for err in [
        expect_graceful_rejection(
            "custom_record_ser_only_remedy",
            "myrec = [a: uint] ; @custom_serialize my_ser\nholder = [f: myrec]\n",
            &[],
        ),
        expect_graceful_rejection(
            "custom_record_deser_only_remedy",
            "myrec = [a: uint] ; @custom_deserialize my_deser\nholder = [f: myrec]\n",
            &[],
        ),
    ] {
        assert!(
            err.contains(
                "Move the pair to the field (or to the type rule of the member) that needs the \
                 custom format, or declare `Myrec` as a _CDDL_CODEGEN_EXTERN_TYPE_ rule and \
                 hand-write the type in full."
            ),
            "both single-half rejections must offer the same two remedies, got:\n{err}"
        );
    }

    // CONTROL 1: BOTH halves on a record rule are NOT rejected; both thin trait impls delegate to
    // the named pair and the ordinary record field walk is absent.
    let src = expect_custom_codec_source(
        "custom_record_both_set_control",
        "myrec = [a: uint] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: myrec]\n",
    );
    assert!(
        src.contains("pub struct Myrec")
            && src.contains("Serialize for Myrec")
            && src.contains("Deserialize for Myrec")
            && src.contains("my_ser(serializer, self)")
            && src.contains("my_deser(raw)")
            && !src.contains("serializer.write_unsigned_integer(self.a")
            && src.contains("_assert_serialize::<crate::generated::Myrec>();")
            && src.contains("_assert_deserialize::<crate::generated::Myrec>();"),
        "the both-set record spelling must generate thin symmetric delegation, without the ordinary \
         record field walk while retaining the opaque extern-interface trait contract, got:\n{src}"
    );
    // A complete pair owns the whole item, including a record shape whose generated field decoder
    // is ambiguous. Its no-deserialize verdict must stay clear so the direct, holder, extern, and
    // wasm from-CBOR surfaces all agree with the thin impl.
    let src = expect_custom_codec_source(
        "custom_ambiguous_record_both_set_control",
        "myrec = [? ignored: uint, value: uint] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: myrec]\n",
    );
    assert!(
        src.contains("impl Deserialize for Myrec")
            && src.contains("impl Deserialize for Holder")
            && src.contains("_assert_deserialize::<crate::generated::Myrec>();")
            && src.contains("_assert_deserialize::<crate::generated::Holder>();")
            && src.contains("Result<Myrec, JsError>")
            && src.contains("Result<Holder, JsError>"),
        "a complete custom record pair must supersede generated-only decoder refusals across every \
         shared no-deserialize consumer, got:\n{src}"
    );
    // Control: the same ambiguous shape with no complete pair still has no generated decoder, so
    // the verdict bypass is not a blanket relaxation of record safety.
    let src = expect_custom_codec_source(
        "ambiguous_record_no_pair_control",
        "ambiguous = [? ignored: uint, value: uint]\nholder = [f: ambiguous]\n",
    );
    assert!(
        !src.contains("impl Deserialize for Ambiguous")
            && !src.contains("impl Deserialize for Holder"),
        "an unannotated ambiguous record must retain its no-deserialize verdict, got:\n{src}"
    );
    // The pair also owns a record whose FIELD type has no generated decoder. The nested Inner
    // verdict must remain true for Inner itself, while it must not propagate through the pair to
    // Myrec or its holder.
    let src = expect_custom_codec_source(
        "custom_record_undecodable_field_both_set_control",
        "inner = [? ignored: uint, value: uint]\n\
         myrec = [i: inner] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: myrec]\n",
    );
    assert!(
        !src.contains("impl Deserialize for Inner")
            && src.contains("impl Deserialize for Myrec")
            && src.contains("impl Deserialize for Holder")
            && src.contains("_assert_deserialize::<crate::generated::Myrec>();")
            && src.contains("_assert_deserialize::<crate::generated::Holder>();")
            && src.contains("Result<Myrec, JsError>")
            && src.contains("Result<Holder, JsError>"),
        "a complete custom record pair must block an undecodable field verdict from propagating \
         to its direct, holder, extern, and wasm decode surfaces, got:\n{src}"
    );
    // The map-representation sibling reaches the same `codegen_struct` path. Its ordinary key/value
    // walk must be absent too, while both standalone traits still delegate to the complete-item pair.
    let src = expect_custom_codec_source(
        "custom_map_record_both_set_control",
        "myrec = { a: uint } ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: myrec]\n",
    );
    assert!(
        src.contains("pub struct Myrec")
            && src.contains("Serialize for Myrec")
            && src.contains("Deserialize for Myrec")
            && src.contains("my_ser(serializer, self)")
            && src.contains("my_deser(raw)")
            && !src.contains("serializer.write_text(\"a\")?;"),
        "the map-rep both-set record spelling must generate thin symmetric delegation, without the \
         ordinary map field walk, got:\n{src}"
    );
    // A generic RECORD DEFINITION carries its config into each concrete record it mints. The
    // instance binding itself owns no config slot, but its Foo struct still takes this shared path.
    let src = expect_custom_codec_source(
        "custom_generic_record_definition_both_set_control",
        "base<T> = [x: T] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         foo = base<uint>\n\
         holder = [f: foo]\n",
    );
    assert!(
        src.contains("pub struct Foo")
            && src.contains("Serialize for Foo")
            && src.contains("Deserialize for Foo")
            && src.contains("my_ser(serializer, self)")
            && src.contains("my_deser(raw)")
            && src.contains("my_ser(serializer, &self.f)")
            && !src.contains("serializer.write_unsigned_integer(self.x"),
        "a generic record definition's concrete instance must get thin delegation and route its \
         holder through the same pair, got:\n{src}"
    );
    // Scope boundary: named collections are not records and own no generated class impl, so
    // whole-record delegation must not reach them by routing their `Root(Rust)` reference through
    // `my_ser`. The boundary is now enforced by REFUSING the spelling outright rather than by
    // generating it with both halves dropped — a collection typedef has nothing for either half to
    // displace, so the pair cannot be honored there in any direction.
    let err = expect_graceful_rejection(
        "custom_named_array_nonrecord_boundary",
        "items = [* uint] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: items]\n",
        &[],
    );
    assert!(
        err.contains("@custom_serialize on `Items`: a named collection rule")
            && err.contains("@custom_deserialize on `Items`: a named collection rule"),
        "a named collection must refuse the pair in BOTH directions rather than drop it, got:\n{err}"
    );
    // CONTROL 2: a PLAIN GROUP rule's trailing comment binds to its LAST MEMBER's slot (the
    // `@name plain-group-trailing` seam), where the pair is a FIELD-level directive and IS honored.
    // The record-kind check must not reach it — probed to still emit the field call sites. The pair
    // is COMPLETE because a lone half at any field slot is its own rejection
    // (`single_half_custom_codec_on_record_field_rejects_gracefully`), which this control must not
    // trip over: what it isolates is the record-kind check's reach, not the pair's completeness.
    let src = expect_custom_codec_source(
        "custom_plain_group_trailing_control",
        "pg = (a: uint, b: text) ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [pg]\n",
    );
    assert!(
        src.contains("let b = my_deser(raw)") && src.contains("my_ser(serializer, &self.b)"),
        "a plain group's trailing pair is a field-level directive on its last member and must stay \
         honored in both directions, got:\n{src}"
    );
}

/// A SINGLE HALF of the custom (de)serializer pair at a FIELD/MEMBER slot is rejected BY DESIGN, via
/// a GRACEFUL `Err` (`record_rejection` → drained by `finalize`), never a `panic!` and never a
/// generated crate. It is the field twin of the record-rule and transparent-alias single-half
/// rejections, and it fails the way the ALIAS one does rather than the record one: the field slot's
/// two directions are lifted independently, so the declared half routes the named function while the
/// opposite direction keeps the FIELD TYPE's generated codec. The crate compiles, and the field
/// writes bytes it cannot read back — probed at exit 0 in every rep before this rejection existed
/// (array `ws_only(serializer, &self.f)` beside `raw.bytes()`, and the map-rep mirror).
///
/// Both reps are asserted because the seam is the shared field-metadata walk, and the divergence
/// text is direction-SPECIFIC (each half names which path was rewritten and which kept the generated
/// codec) while the remedy is shared. The complete pair stays accepted in both reps — it owns both
/// directions of the field — and is the control that makes each rejection attributable to the
/// MISSING half rather than to the field position.
#[test]
fn single_half_custom_codec_on_record_field_rejects_gracefully() {
    let err = expect_graceful_rejection(
        "custom_field_ser_only",
        "t = [\n  f: bytes ; @custom_serialize ws_only\n]\n",
        &[],
    );
    assert!(
        err.contains(
            "@custom_serialize alone on field `f` of rule `t`: the field's serialize path writes \
             through the named function while its deserialize path keeps the field type's own \
             generated codec — so the bytes this field writes are not the bytes it reads back."
        ),
        "the serialize-only field rejection must name its own direction's divergence, got:\n{err}"
    );

    let err = expect_graceful_rejection(
        "custom_field_deser_only",
        "t = [\n  f: bytes ; @custom_deserialize rd_only\n]\n",
        &[],
    );
    assert!(
        err.contains(
            "@custom_deserialize alone on field `f` of rule `t`: the field's deserialize path reads \
             through the named function while its serialize path keeps the field type's own \
             generated codec — so the bytes this field writes are not the bytes it reads back."
        ),
        "the deserialize-only field rejection must name the mirror divergence, got:\n{err}"
    );

    // The MAP rep reaches the same field-metadata walk, so it must reject identically — the
    // asymmetry was probed there too (`ws_only` write beside a `raw.bytes()` read).
    let err = expect_graceful_rejection(
        "custom_map_field_ser_only",
        "t = {\n  f: bytes ; @custom_serialize ws_only\n}\n",
        &[],
    );
    assert!(
        err.contains("@custom_serialize alone on field `f` of rule `t`"),
        "the map-rep field rejection must fire at the same seam, got:\n{err}"
    );

    // The remedy is shared by both halves and by both reps: write the pair on THIS entry, or move it
    // to the member's type rule.
    for (tag, spec) in [
        (
            "custom_field_ser_only_remedy",
            "t = [\n  f: bytes ; @custom_serialize ws_only\n]\n",
        ),
        (
            "custom_field_deser_only_remedy",
            "t = [\n  f: bytes ; @custom_deserialize rd_only\n]\n",
        ),
        (
            "custom_map_field_ser_only_remedy",
            "t = {\n  f: bytes ; @custom_serialize ws_only\n}\n",
        ),
    ] {
        let err = expect_graceful_rejection(tag, spec, &[]);
        assert!(
            err.contains(
                "Write both halves on this entry (`; @custom_serialize <fn> @custom_deserialize \
                 <fn>`), adding the missing "
            ) && err.contains(
                "or move the pair to the member's TYPE rule if the format belongs to the type."
            ),
            "[{tag}] every single-half field rejection must offer the same two remedies, got:\n{err}"
        );
    }

    // The rejection is at PARSE, before any emission, so no profile can rescue the shape — asserted
    // rather than assumed, because a per-profile split would be a support claim this seam cannot
    // make.
    for flags in [
        &["--preserve-encodings=true"][..],
        &["--preserve-encodings=true", "--canonical-form=true"][..],
    ] {
        let err = expect_graceful_rejection(
            "custom_field_ser_only_profile",
            "t = [\n  f: bytes ; @custom_serialize ws_only\n]\n",
            flags,
        );
        assert!(
            err.contains("@custom_serialize alone on field `f` of rule `t`"),
            "the field rejection must be profile-independent ({flags:?}), got:\n{err}"
        );
    }

    // CONTROL: the COMPLETE pair at the same slot stays accepted in both reps, routing BOTH
    // directions through the named functions.
    for (tag, spec, key_write) in [
        (
            "custom_field_both_set_control",
            "t = [\n  f: bytes ; @custom_serialize ws_only @custom_deserialize rd_only\n]\n",
            None,
        ),
        (
            "custom_map_field_both_set_control",
            "t = {\n  f: bytes ; @custom_serialize ws_only @custom_deserialize rd_only\n}\n",
            Some("serializer.write_text(\"f\")?;"),
        ),
    ] {
        let src = expect_custom_codec_source(tag, spec);
        assert!(
            src.contains("ws_only(serializer, &self.f)")
                && src.contains("rd_only(raw)")
                && !src.contains("serializer.write_bytes(&self.f)")
                && !src.contains("raw.bytes()"),
            "[{tag}] the complete field pair must own both directions, got:\n{src}"
        );
        if let Some(key_write) = key_write {
            assert!(
                src.contains(key_write),
                "[{tag}] the pair replaces the VALUE codec only — the map key is still written by \
                 generated code, got:\n{src}"
            );
        }
    }
}

/// A generic DEFINITION works by substituting the instance's arguments into ONE registered
/// `RustStruct`, so a body that registers no struct — or an enum of them — has nowhere to put the
/// parameters. Four such bodies used to abort on valid CDDL rather than say so: a `T / null`
/// collapse (`todo!`), a body naming another type (`todo!`), a body carrying GROUP choices
/// (`todo!`), and a non-idiom TYPE choice, which is the worst of the four because it parsed
/// cleanly and only died downstream in the serialize walk (`Option::unwrap`, exit 101, no
/// diagnosis). All four are now parse-time graceful rejections naming the offending rule and the
/// bodies that ARE supported.
///
/// The last vector is the load-bearing control: the ONE choice-bodied generic that works — the
/// transparent tag-set idiom — must keep generating, or the rejection could pass by refusing every
/// choice-bodied generic def. The non-idiom vector is asserted with and WITHOUT an instance,
/// because only the instanced form ever reached the downstream abort and a rejection keyed on
/// instantiation would leave the bare definition silently dead.
#[test]
fn unsupported_generic_def_bodies_reject_gracefully() {
    let vectors = [
        (
            "tnull",
            "foo<T> = T / null\ny = [v: foo<uint>]\n",
            "a `T / null` body collapses to a transparent `Option<T>` alias",
        ),
        (
            "typename",
            "foo<T, U> = [T, U]\nbar<V> = foo<V, uint>\ny = [v: bar<uint>]\n",
            "a body that is another named type",
        ),
        (
            "group_choice",
            "g<T> = [ ( a: T ) // ( b: uint ) ]\ny = [v: g<uint>]\n",
            "group choices (`//`) in a generic definition are not supported",
        ),
        (
            "type_choice_instanced",
            "xs<a0> = #6.258([+ a0]) / [* a0]\ny = [v: xs<uint>]\n",
            "a type-choice body is supported only for the transparent tag-set idiom",
        ),
        (
            "type_choice_uninstanced",
            "xs<a0> = #6.258([+ a0]) / [* a0]\ny = [v: uint]\n",
            "a type-choice body is supported only for the transparent tag-set idiom",
        ),
    ];
    for (tag, spec, fragment) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(&format!("generic_body_{tag}"), spec, extra);
            assert!(
                msg.contains(fragment),
                "the rejection must diagnose THIS body shape ({tag}, {extra:?}), got: {msg}"
            );
            // Every one of them carries the same remedy list, so a user reading any of the four
            // learns the same three supported shapes.
            assert!(
                msg.contains("A generic definition's body must be a shape that registers a struct"),
                "the rejection must name the supported generic-def bodies ({tag}, {extra:?}), \
                 got: {msg}"
            );
        }
    }

    // Control: the transparent tag-set idiom is the one supported choice-bodied generic def and
    // must keep generating (instanced and used), under both profiles.
    for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_generic_idiom_ok_{}.cddl",
            std::process::id()
        ));
        std::fs::write(
            &path,
            "xs<a0> = #6.258([* a0]) / [* a0]\ny = [v: xs<uint>]\n",
        )
        .unwrap();
        let mut argv = vec![
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "generic_idiom_unused",
        ];
        argv.extend_from_slice(extra);
        let cli = Cli::parse_from(argv);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "the tag-set idiom generic def must keep generating ({extra:?}) — it is the shape the \
             rejection advertises as supported"
        );
    }
}

/// The fifth unsupported generic-def body, and the only one whose abort was reached BEFORE the IR
/// existed: a PLAIN GROUP body (`set<a> = (* a)`). A plain group registers no struct of its own —
/// its contents are spliced into each rule that references it — so an instance's arguments have
/// nowhere to substitute, exactly like the four bodies above. What made it the remainder of that
/// conversion is the seam: `dep_graph::find_references` asserts on it, and every caller of that
/// runs before any `record_rejection` channel exists, so the only outcome available there was
/// `exit 101`. The refusal therefore lives in the `api::with_types` pre-scan; the one reach that
/// PRECEDES even that (`extern_narrow::scan_consumer`, which runs during input assembly on every
/// generation, imports or not) skips the rule by consulting the same predicate.
///
/// Both spellings are vectors because the AST gives us both as a `Rule::Group` — the parenthesized
/// body AND the bare-paren group-choice form (`g<T> = ((a: T) // (b: uint))`, which is NOT the
/// bracketed `g<T> = [ (a: T) // (b: uint) ]` the group-choice rejection above covers). One check
/// at the pre-scan covers both; a fix keyed on the first spelling alone would leave the second
/// aborting.
///
/// The controls are load-bearing in two directions: the generic ARRAY / MAP / record / tag-set
/// bodies must keep generating (a refusal keyed on `generic_params.is_some()` alone would refuse
/// every generic def), and a NON-generic plain group must keep generating (a refusal keyed on the
/// group RULE alone would refuse the ordinary spliced-group spelling this repo's own fixtures use).
#[test]
fn generic_plain_group_def_rejects_gracefully() {
    let vectors = [
        ("paren_body", "set<a> = (* a)\nfoo = [set<uint>]\n", "set"),
        (
            "group_choice_body",
            "g<T> = ((a: T) // (b: uint))\nfoo = [g<uint>]\n",
            "g",
        ),
    ];
    for (tag, spec, rule) in vectors {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let msg = expect_graceful_rejection(&format!("generic_plain_group_{tag}"), spec, extra);
            assert!(
                msg.contains(&format!("generic rule `{rule}`")),
                "the rejection must name the offending rule ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains("a plain-group body")
                    && msg.contains("registers no struct of its own"),
                "the rejection must diagnose the plain-group body ({tag}, {extra:?}), got: {msg}"
            );
            // Same remedy list as its four siblings — one vocabulary for every generic-def refusal.
            assert!(
                msg.contains("A generic definition's body must be a shape that registers a struct"),
                "the rejection must name the supported generic-def bodies ({tag}, {extra:?}), \
                 got: {msg}"
            );
        }
    }

    // Controls: everything adjacent to the refused shape must still generate, under both profiles.
    let controls = [
        ("generic_array", "g<T> = [* T]\ny = [v: g<uint>]\n"),
        ("generic_map", "g<T> = {a: T}\ny = [v: g<uint>]\n"),
        (
            "generic_record",
            "g<T> = [a: T, b: uint]\ny = [v: g<uint>]\n",
        ),
        (
            "generic_tag_set",
            "xs<a0> = #6.258([* a0]) / [* a0]\ny = [v: xs<uint>]\n",
        ),
        ("plain_group_nongeneric", "grp = (* uint)\nfoo = [grp]\n"),
    ];
    for (tag, spec) in controls {
        for extra in [&[][..], &["--preserve-encodings", "true"][..]] {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_gpg_ctl_{tag}_{}.cddl",
                std::process::id()
            ));
            std::fs::write(&path, spec).unwrap();
            let mut argv = vec![
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "gpg_ctl_unused",
            ];
            argv.extend_from_slice(extra);
            let cli = Cli::parse_from(argv);
            let result = crate::api::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            assert!(
                result.is_ok(),
                "control `{tag}` ({extra:?}) must keep generating — the plain-group refusal must \
                 not widen to generic defs that DO register a struct, nor to non-generic plain \
                 groups: {:?}",
                result.err().map(|e| e.to_string())
            );
        }
    }
}

/// A rule-position directive on a `T / null` rule used to be SILENTLY DROPPED in every spelling:
/// the Option-collapse branch built its `RuleMetadata` from the inner arm's `Type1` comment slot,
/// which the pinned cddl fork never populates for a type-choice arm, so even the `@duplicates` /
/// `@ignore` rejections written at that branch could not fire. It now reads the same merged
/// rule-position slots (the LAST arm's trailing comment) its sibling branch reads.
///
/// One vector per directive that the collapse can reach, asserted in BOTH rule-position spellings
/// (`T / null ; @x` and `null / T ; @x` — the parser binds the trailing comment to the last arm's
/// `TypeChoice` slot either way), because a fix keyed on arm ORDER would pass a single-spelling
/// test. Every directive must land in one of three states — never a silent no-op:
/// honored, or rejected as inapplicable to a transparent `Option<T>` alias.
#[test]
fn option_collapse_reads_rule_position_directives() {
    // (directive, expected-rejection fragment) — each is INAPPLICABLE to the collapse's transparent
    // `Option<T>` alias, and each says so in its own words rather than sharing one catch-all.
    let rejected = [
        (
            "@duplicates reject",
            "only applies to set/array collection rules",
        ),
        ("@ignore", "@ignore on rule"),
        (
            "@no_json_schema_export",
            "this rule registers no rust struct, so there is no schema-registration row to suppress",
        ),
        ("@raw_bytes_flavor", "this tag is only valid on a"),
        ("@copy", "this tag is only valid on a"),
        ("@extern_companions sib=Cls", "@extern_companions on"),
        ("@rust_name Pinned", "@rust_name on"),
        ("@used_as_key", "@used_as_key on"),
        ("@used_as_elem", "@used_as_elem on"),
        ("@newtype", "@newtype on"),
    ];
    for (directive, fragment) in rejected {
        for (tag, spec) in [
            (
                "null_last",
                format!("opt = uint / null ; {directive}\nuse = [f: opt]\n"),
            ),
            (
                "null_first",
                format!("opt = null / uint ; {directive}\nuse = [f: opt]\n"),
            ),
        ] {
            let msg = expect_graceful_rejection(
                &format!("optcollapse_{tag}_{}", directive.len()),
                &spec,
                &[],
            );
            assert!(
                msg.contains(fragment),
                "`{directive}` on a `T / null` rule ({tag}) must reject loudly, never drop \
                 silently, got: {msg}"
            );
        }
    }

    // `@no_alias` and `@doc` are the two that are HONORED rather than rejected: both ride the
    // registered alias. Asserted by effect on the emitted source, so a regression to the dead slot
    // fails here rather than passing as "no rejection".
    let src = |spec: &str, tag: &str| -> String {
        let path =
            std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "optcollapse_unused",
            "--wasm",
            "false",
        ]);
        let files = crate::api::generated_strings(&cli).expect("must generate");
        std::fs::remove_file(&path).ok();
        files
            .get("rust/src/generated/mod.rs")
            .expect("rust mod emitted")
            .clone()
    };
    let plain = src("opt = uint / null\nuse = [f: opt]\n", "optcollapse_plain");
    assert!(
        plain.contains("pub type Opt = Option<u64>"),
        "baseline: the collapse emits a transparent alias, got:\n{plain}"
    );
    let no_alias = src(
        "opt = uint / null ; @no_alias\nuse = [f: opt]\n",
        "optcollapse_no_alias",
    );
    assert!(
        !no_alias.contains("pub type Opt"),
        "`@no_alias` on a `T / null` rule must strip the alias line, got:\n{no_alias}"
    );
    let doc = src(
        "opt = uint / null ; @doc collapsed optional\nuse = [f: opt]\n",
        "optcollapse_doc",
    );
    assert!(
        doc.contains("collapsed optional"),
        "`@doc` on a `T / null` rule must reach the emitted source, got:\n{doc}"
    );

    // A directive on the NON-rule-position arm is a misplacement, not a silent drop: the collapse
    // has no variants, so an arm carries nothing of its own.
    let misplaced = expect_graceful_rejection(
        "optcollapse_nonlast",
        "opt = uint ; @no_alias\n    / null\nuse = [f: opt]\n",
        &[],
    );
    assert!(
        misplaced.contains("on a non-last arm of the `T / null` rule")
            && misplaced.contains("its arms are not variants"),
        "a directive on the non-rule-position arm must reject naming the misplacement, got: \
         {misplaced}"
    );
}

/// `_CDDL_CODEGEN_RAW_BYTES_TYPE_` names a type that IS its own bytes, so it has no element type
/// for a generic parameter to name. A generic base registered anyway (the `RawBytesType` struct has
/// no params to keep) emitted rows spelling a BARE `Foo` — the extern-interface self-check's
/// `_assert_raw_bytes::<crate::generated::Foo>()` and, under `--json-schema-export`, the json-gen
/// `reg.add::<cddl_lib::Foo>()` — each E0107 against the parameterized `Foo<T>` the marker
/// promises, shipped at exit 0 with empty stderr. It is now a parse-time rejection.
///
/// The sibling `_CDDL_CODEGEN_EXTERN_TYPE_` base is deliberately NOT rejected — an extern names an
/// arbitrary hand-written type, which may legitimately be parameterized, and its generic-ness is
/// RECORDED so the same two emitters skip it (`extern_interface_check_skips_generic_base_without_
/// instances`). That asymmetry is asserted here so a later sweep cannot "unify" the two.
#[test]
fn generic_raw_bytes_base_rejects_gracefully() {
    for (tag, extra) in [
        ("plain", &["--wasm", "false"][..]),
        (
            "schema",
            &["--wasm", "false", "--json-schema-export", "true"][..],
        ),
    ] {
        // With and WITHOUT an instance: the broken rows are emitted for the bare base, so a
        // rejection keyed on instantiation would leave the exit-0 uncompilable case live.
        for (shape, spec) in [
            (
                "no_instance",
                "foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nbar = [x: uint]\n",
            ),
            (
                "instanced",
                "foo<T> = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nbar = [x: foo<uint>]\n",
            ),
        ] {
            let msg =
                expect_graceful_rejection(&format!("rawbytes_generic_{tag}_{shape}"), spec, extra);
            assert!(
                msg.contains("cannot take generic parameters")
                    && msg.contains("_CDDL_CODEGEN_RAW_BYTES_TYPE_"),
                "the rejection must name the marker and the generic-parameter refusal \
                 ({tag}/{shape}), got: {msg}"
            );
            assert!(
                msg.contains("carries no element type for a parameter to name"),
                "the rejection must say WHY a raw-bytes base cannot be generic ({tag}/{shape}), \
                 got: {msg}"
            );
        }
    }

    // Controls: the NON-generic raw-bytes rule still generates, and the generic EXTERN base — the
    // sibling that records rather than refuses — still generates too.
    for (tag, spec) in [
        (
            "nongeneric_raw_bytes",
            "foo = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nbar = [x: foo]\n",
        ),
        (
            "generic_extern_base",
            "foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_\nbar = [x: uint]\n",
        ),
    ] {
        let path =
            std::env::temp_dir().join(format!("cddl_codegen_{tag}_{}.cddl", std::process::id()));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rawbytes_control_unused",
            "--wasm",
            "false",
            "--json-schema-export",
            "true",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "the {tag} control must keep generating — only a GENERIC raw-bytes base is refused"
        );
    }
}

/// A `.cbor` payload applied to a target that is ALREADY a `.cbor` payload — the INLINE spelling
/// `bytes .cbor (bytes .cbor uint)` — used to generate at exit 0 and emit a crate that cannot
/// build: the serialize walk names the payload buffer after the OWNING variable, so every depth in
/// one chain mints `<var>_inner_se` and the outer write borrows what the inner `finalize()` moved
/// (E0382, once per extra depth). Now a parse-time graceful rejection at both seams that can apply
/// the operation: the rule-BODY registration (which has a rule name to prefix) and
/// `rust_type_from_type1` (every member / element / choice-arm position, which does not).
///
/// NAMING the inner payload is the boundary, and it is why the alias-flattened spelling
/// (`inner = bytes .cbor uint` + `bytes .cbor inner`) is in the CONTROLS rather than the vectors: a
/// `.cbor` rule body mints a wrapper struct with its own serialize fn and its own buffer, so the
/// reference crosses a real type instead of copying a chain that already carried a `CBORBytes`. The
/// transparent-alias flattening that made the two spellings one chain is unrepresentable now
/// (`register_type_alias`'s wire-facts assert).
///
/// The controls are the load-bearing half, because the refusal keys on the SAME-CHAIN composition
/// only and a sloppier key would take the whole `.cbor` feature down with it. Nesting through a
/// named payload (with or without `@newtype`) and nesting inside a payload's COLLECTION
/// (`tests/corpus/cbor_payload_nested.cddl`'s shape) both give the inner payload its own serialize
/// fn and its own buffer, emit the same nested wire shape, and must keep generating; so must a
/// single payload, a payload over a tag, and a payload over a struct.
#[test]
fn nested_cbor_payload_rejects_gracefully() {
    // Rule-BODY spellings carry the rule-name prefix; every other position does not.
    let vectors = [
        ("inline_body", "b = bytes .cbor (bytes .cbor uint)\n", true),
        (
            "inline_member",
            "foo = [b: bytes .cbor (bytes .cbor uint)]\n",
            false,
        ),
        (
            "triple_member",
            "foo = [b: bytes .cbor (bytes .cbor (bytes .cbor uint))]\n",
            false,
        ),
        (
            "inline_choice_arm",
            "a = bytes .cbor (bytes .cbor uint) / tstr\n",
            false,
        ),
        (
            "inline_array_element",
            "a = [* bytes .cbor (bytes .cbor uint)]\n",
            false,
        ),
    ];
    // Profile-INDEPENDENT: `finalize` short-circuits on a recorded rejection before any emission,
    // so no flag can rescue the shape.
    for (tag, spec, rule_prefixed) in vectors {
        for extra in [
            &["--wasm", "false"][..],
            &["--wasm", "true"][..],
            &["--wasm", "false", "--preserve-encodings", "true"][..],
        ] {
            let msg = expect_graceful_rejection(&format!("nested_cbor_{tag}"), spec, extra);
            assert!(
                msg.contains(
                    "a `.cbor` payload whose own target is already a `.cbor` payload is unsupported"
                ),
                "the rejection must name the composition ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains("This is the INLINE spelling"),
                "the rejection must name the composition it keys on ({tag}, {extra:?}), got: {msg}"
            );
            assert!(
                msg.contains("`inner = bytes .cbor T`")
                    && msg.contains("bytes .cbor [* bytes .cbor T]"),
                "the rejection must point at the two supported ways to nest a payload ({tag}, \
                 {extra:?}), got: {msg}"
            );
            assert_eq!(
                msg.starts_with("rule `B`: "),
                rule_prefixed,
                "only the rule-BODY seam knows a rule name to prefix ({tag}, {extra:?}), got: {msg}"
            );
        }
    }

    // Controls: every currently-working `.cbor` shape must keep generating. `nested_collection`
    // mirrors tests/corpus/cbor_payload_nested.cddl and `newtype_boundary` is the remedy the
    // rejection advertises, so both are asserted rather than assumed.
    for (tag, spec) in [
        ("single", "x = bytes .cbor uint\n"),
        (
            "nested_collection",
            "p = [e: bytes .cbor [* bytes .cbor uint], v: bytes .cbor {* uint => bytes .cbor uint}]\n",
        ),
        (
            "newtype_boundary",
            "inner = bytes .cbor uint ; @newtype\nb = [f: bytes .cbor inner]\n",
        ),
        // The same boundary WITHOUT `@newtype`: a `.cbor` rule body force-wraps either way, so
        // these four (rule body, member, choice arm, array element) are the spellings that used to
        // be same-chain vectors and are supported now.
        (
            "named_boundary_body",
            "inner = bytes .cbor uint\nb = bytes .cbor inner\n",
        ),
        (
            "named_boundary_member",
            "inner = bytes .cbor uint\nfoo = [b: bytes .cbor inner]\n",
        ),
        (
            "named_boundary_chain_member",
            "i1 = bytes .cbor uint\ni2 = i1\nfoo = [b: bytes .cbor i2]\n",
        ),
        (
            "named_boundary_choice_arm",
            "inner = bytes .cbor uint\na = bytes .cbor inner / tstr\n",
        ),
        (
            "named_boundary_array_element",
            "inner = bytes .cbor uint\na = [* bytes .cbor inner]\n",
        ),
        (
            "struct_target",
            "inner = [a: uint]\nb = [f: bytes .cbor inner]\n",
        ),
        ("over_tag", "b = bytes .cbor (#6.10(uint))\n"),
        ("over_choice", "b = bytes .cbor (int / tstr)\n"),
    ] {
        for extra in [
            &["--wasm", "false"][..],
            &["--wasm", "false", "--preserve-encodings", "true"][..],
        ] {
            let path = std::env::temp_dir().join(format!(
                "cddl_codegen_nested_cbor_ok_{tag}_{}.cddl",
                std::process::id()
            ));
            std::fs::write(&path, spec).unwrap();
            let mut argv = vec![
                "cddl-codegen",
                "--input",
                path.to_str().unwrap(),
                "--output",
                "nested_cbor_control_unused",
            ];
            argv.extend_from_slice(extra);
            let cli = Cli::parse_from(argv);
            let result = crate::api::generated_strings(&cli);
            std::fs::remove_file(&path).ok();
            assert!(
                result.is_ok(),
                "the {tag} control must keep generating ({extra:?}) — only a payload nested in the \
                 SAME chain is refused, got: {:?}",
                result.err().map(|e| e.to_string())
            );
        }
    }
}

/// A CBOR tag riding an ANONYMOUS choice RULE under `--preserve-encodings` — `t = #6.10(int / tstr)`
/// and its group-choice and all-fixed spellings — used to abort at exit 101 on an explicit
/// `assert!(!cli.preserve_encodings)` in the tagged-enum serialize path. An assert on a FLAG is a
/// crash, not a boundary: the shape is valid CDDL and the profile is a user choice. It is now a
/// graceful rejection recorded in `IntermediateTypes::finalize`, on the same struct-KIND walk the
/// `@custom_serialize`/`@custom_deserialize` enum rejections use, and keyed on exactly the assert's
/// own predicate — `generate_enum` is reached from precisely two places, the `TypeChoice` and
/// `GroupChoice` arms of the rust-struct dispatch, each passing `rust_struct.tag()` straight
/// through. The assert stays in place as the guard that re-earns the retired
/// `PRESERVE_ONLY_PANIC_CLASSES` entry if some future path reaches it another way.
///
/// The all-fixed vector is not redundant with the other two: `RustStruct::new_type_choice` denies a
/// TAGGED fixed-value choice the `CStyleEnum` lowering under this profile (`cant_store_tag`), so
/// `#6.10(0 / 1 / 2)` registers as a `TypeChoice` and reaches the same path — while the SAME choice
/// named and referenced from a tagged member is the supported spelling the message advertises. Both
/// advertised remedies are asserted as controls, and the tagged-wrapper one was additionally probed
/// to build and round-trip byte-exact through a generated preserve crate (a non-minimal `d8 0a` tag
/// head over a 2-byte uint, and an indefinite-length text arm).
#[test]
fn tagged_anonymous_choice_rejects_gracefully_under_preserve() {
    let vectors = [
        ("type_choice", "t = #6.10(int / tstr)\n", "a type choice"),
        (
            "type_choice_three_arm",
            "t = #6.10(int / tstr / bytes)\n",
            "a type choice",
        ),
        (
            "type_choice_all_fixed",
            "t = #6.10(1 / 2 / 3)\n",
            "a type choice",
        ),
        (
            "type_choice_referenced",
            "t = #6.10(int / tstr)\nholder = [x: t]\n",
            "a type choice",
        ),
        (
            "group_choice_array",
            "t = #6.10([ a: uint // b: tstr ])\n",
            "a group choice",
        ),
        (
            "group_choice_map",
            "t = #6.10({ a: uint // b: tstr })\n",
            "a group choice",
        ),
    ];
    for (tag, spec, shape) in vectors {
        let msg = expect_graceful_rejection(
            &format!("tagged_anon_choice_{tag}"),
            spec,
            &["--wasm", "false", "--preserve-encodings", "true"],
        );
        assert!(
            msg.starts_with("rule `T`: a CBOR tag (`#6.10`) directly over ") && msg.contains(shape),
            "the rejection must name the rule, the tag and the choice shape ({tag}), got: {msg}"
        );
        assert!(
            msg.contains("is unsupported under `--preserve-encodings`")
                && msg.contains("the encoding metadata preserve records is per-VARIANT"),
            "the rejection must name the profile and why the enum cannot carry the tag ({tag}), \
             got: {msg}"
        );
        assert!(
            msg.contains("Name the choice and tag the NAME instead")
                && msg.contains("Tags over structs, arrays and maps are unaffected")
                && msg.contains("without `--preserve-encodings` this rule generates"),
            "the rejection must point at the working alternatives and the profile that works \
             ({tag}), got: {msg}"
        );
        // The DEFAULT profile is untouched: this is a preserve-only refusal, so every vector above
        // must still generate without the flag. A profile-blind key would silently drop support.
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_tagged_anon_choice_default_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "tagged_anon_choice_default_unused",
            "--wasm",
            "false",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "{tag} must keep generating WITHOUT --preserve-encodings — the refusal is profile-keyed, \
             got: {:?}",
            result.err().map(|e| e.to_string())
        );
    }

    // Controls under PRESERVE: the two remedies the message names, plus the tag targets the
    // roadmap entry proves unaffected, plus the untagged choices (the tag is what breaks, not the
    // choice) and the `T / null` collapse (which never mints an enum at all).
    for (tag, spec) in [
        (
            "named_choice_tagged",
            "inner = int / tstr\nt = #6.10(inner)\n",
        ),
        (
            "named_cstyle_tagged_member",
            "inner = 0 / 1 / 2\nt = [f: #6.42(inner)]\n",
        ),
        ("tag_over_struct", "s = [a: uint]\nt = #6.10(s)\n"),
        ("tag_over_array", "t = #6.10([* uint])\n"),
        ("tag_over_map", "t = #6.10({* uint => tstr})\n"),
        (
            "named_group_choice_tagged",
            "g = [ a: uint // b: tstr ]\nt = #6.10(g)\n",
        ),
        ("untagged_type_choice", "t = int / tstr\n"),
        ("untagged_group_choice", "t = [ a: uint // b: tstr ]\n"),
        ("tagged_optional", "t = #6.10(uint / null)\n"),
    ] {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_tagged_anon_choice_ok_{tag}_{}.cddl",
            std::process::id()
        ));
        std::fs::write(&path, spec).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "tagged_anon_choice_control_unused",
            "--wasm",
            "false",
            "--preserve-encodings",
            "true",
        ]);
        let result = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        assert!(
            result.is_ok(),
            "the {tag} control must keep generating under preserve — only a tag DIRECTLY over an \
             anonymous choice is refused, got: {:?}",
            result.err().map(|e| e.to_string())
        );
    }
}

/// `@no_alias` must suppress the emitted `pub type` on EVERY rule kind that registers a transparent
/// alias, not only the ones whose alias is built from the rule's metadata.
///
/// Three kinds register through `AliasInfo::new_manual`, whose `rule_metadata` is `None` by
/// construction — a table rule and an array typedef (registered from the `finalize` kind-walk, where
/// only the `RustStruct` is in scope) and a named binding to a generic set nominal (registered from
/// the generic-resolution arm). On all three the directive was silently dropped: the rule kept
/// emitting the `pub type` it asks to suppress, and a member site kept referring to it. The
/// enforcement therefore lives in `register_type_alias`, keyed on the per-ident record the parse seam
/// writes, so a future registration path honors it without a fourth place to remember.
///
/// Asserted on the emitted source in both directions per kind: the `pub type` is gone AND the member
/// site inlines the structural type it used to name. The scalar alias is the control that already
/// worked; a struct-registering rule is the control for inertness (no alias exists, so nothing
/// changes and nothing breaks).
#[test]
fn no_alias_suppresses_the_pub_type_on_every_alias_registering_kind() {
    let emit = |cddl: &str| {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_no_alias_kinds_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "no_alias_kinds_unused",
            "--wasm",
            "false",
        ]);
        let out = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        out.unwrap_or_else(|e| panic!("generation must succeed for:\n{cddl}\ngot: {e}"))
            .get("rust/src/generated/mod.rs")
            .expect("mod.rs")
            .clone()
    };
    // (kind, body lines, the `pub type` line the rule emits WITHOUT the directive, the member
    // spelling it must fall back to WITH it)
    let kinds = [
        (
            "table rule",
            "foo = { * uint => tstr }@\nholder = [f: foo]\n",
            "pub type Foo = BTreeMap<u64, String>;",
            "pub f: BTreeMap<u64, String>,",
        ),
        (
            "array typedef",
            "foo = [* uint]@\nholder = [f: foo]\n",
            "pub type Foo = Vec<u64>;",
            "pub f: Vec<u64>,",
        ),
        (
            "named binding to a generic set nominal",
            "gset<T> = #6.258([* T]) / [* T]\nfoo = gset<uint>@\nholder = [f: foo]\n",
            "pub type Foo = GsetU64;",
            "pub f: GsetU64,",
        ),
        // The control that already worked before the enforcement moved — kept so a regression that
        // breaks the ORIGINAL carrier fails here too.
        (
            "scalar transparent alias",
            "foo = uint@\nholder = [f: foo]\n",
            "pub type Foo = u64;",
            "pub f: u64,",
        ),
    ];
    for (kind, template, pub_type, member) in kinds {
        let without = emit(&template.replace('@', ""));
        assert!(
            without.contains(pub_type),
            "{kind}: without the directive the rule must emit `{pub_type}` — otherwise this vector \
             proves nothing:\n{without}"
        );
        let with = emit(&template.replace('@', " ; @no_alias"));
        assert!(
            !with.contains(pub_type),
            "{kind}: @no_alias must suppress `{pub_type}`:\n{with}"
        );
        assert!(
            with.contains(member),
            "{kind}: with the alias suppressed the member site must inline `{member}`:\n{with}"
        );
    }
    // Inertness control: a rule that registers a STRUCT has no alias for the mark to reach, so the
    // directive changes nothing rather than breaking the emission.
    let record = emit("foo = [x: uint]\nholder = [f: foo]\n");
    let record_no_alias = emit("foo = [x: uint] ; @no_alias\nholder = [f: foo]\n");
    assert_eq!(
        record, record_no_alias,
        "@no_alias on a struct-registering rule must be inert, not destructive"
    );
}

/// `@doc` must reach the construct a rule emits, including the two kinds whose construct is built
/// from someone else's metadata.
///
/// A generic INSTANCE binding (`foo = base<uint>`) mints a struct whose `RustStructConfig` is the
/// generic DEFINITION's, and a named binding to a generic SET NOMINAL registers its alias through
/// `AliasInfo::new_manual` (metadata `None`) beside a nominal that belongs to the definition too. On
/// both, the rule emitted a perfectly documentable construct — a `pub struct` and a `pub type` — and
/// discarded the doc. The record is per-ident, written at the parse seam and read where each
/// construct is built, so the rule that WROTE the doc is the one that owns it.
#[test]
fn rule_doc_reaches_the_constructs_built_from_borrowed_metadata() {
    let emit = |cddl: &str| {
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_rule_doc_{}_{}.cddl",
            std::process::id(),
            cddl.len()
        ));
        std::fs::write(&path, cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "rule_doc_unused",
            "--wasm",
            "false",
        ]);
        let out = crate::api::generated_strings(&cli);
        std::fs::remove_file(&path).ok();
        out.unwrap_or_else(|e| panic!("generation must succeed for:\n{cddl}\ngot: {e}"))
            .get("rust/src/generated/mod.rs")
            .expect("mod.rs")
            .clone()
    };
    const DOC: &str = "the rule's own prose";
    let kinds = [
        (
            "generic instance binding (struct built from the definition's config)",
            "base<T> = [x: T]\nfoo = base<uint>@\nholder = [f: foo]\n",
        ),
        (
            "named binding to a generic set nominal (alias registered without metadata)",
            "gset<T> = #6.258([* T]) / [* T]\nfoo = gset<uint>@\nholder = [f: foo]\n",
        ),
        // The control that already worked — a rule whose own metadata builds its own struct.
        (
            "record rule (own config)",
            "foo = [x: uint]@\nholder = [f: foo]\n",
        ),
    ];
    for (kind, template) in kinds {
        let without = emit(&template.replace('@', ""));
        assert!(
            !without.contains(DOC),
            "{kind}: the undocumented spec must not contain the doc text — fixture bug"
        );
        let with = emit(&template.replace('@', &format!(" ; @doc {DOC}")));
        assert!(
            with.contains(&format!("/// {DOC}")),
            "{kind}: @doc must reach the emitted construct:\n{with}"
        );
    }
}

/// Generate `cddl` with `extra` flags, returning `rust/src/generated/mod.rs` on success or the
/// graceful rejection text on failure. Shared by the two rule-position-directive tests below, which
/// need both outcomes from the same shapes.
fn rule_directive_emit(cddl: &str, extra: &[&str]) -> Result<String, String> {
    // A monotonic per-call suffix, not a content hash: `cargo test` runs these tests concurrently
    // and two cells with the same spec LENGTH would otherwise share a path — one call deleting the
    // file the other is mid-read (an ENOENT masquerading as a rejection).
    static SEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_rule_directive_{}_{}.cddl",
        std::process::id(),
        SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    ));
    std::fs::write(&path, cddl).unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "rule_directive_unused",
    ];
    args.extend_from_slice(extra);
    // `--wasm false` unless the cell asked for the wasm face itself (clap refuses a repeated flag).
    if !extra.contains(&"--wasm") {
        args.extend_from_slice(&["--wasm", "false"]);
    }
    let cli = Cli::parse_from(args);
    let out = crate::api::generated_strings(&cli);
    std::fs::remove_file(&path).ok();
    match out {
        Ok(files) => Ok(files
            .get("rust/src/generated/mod.rs")
            .expect("mod.rs")
            .clone()),
        Err(e) => Err(e.to_string()),
    }
}

/// `@custom_json` and `@used_as_key` must reach the STRUCT a rule mints, including the two kinds
/// whose struct is built from metadata that is not the rule's own.
///
/// A generic INSTANCE binding (`foo = base<uint>`) mints a struct whose `RustStructConfig` is the
/// generic DEFINITION's, and a plain GROUP rule's struct is built from `PlainGroupInfo`'s metadata —
/// read off `comments_after_group`, the slot cddl leaves empty for the single-line spelling it
/// actually binds to the last entry's trailing comment. Both accepted the directives and suppressed
/// / demanded nothing. Each cell asserts the WITHOUT-directive spec has the opposite property first,
/// so a fixture that stopped exercising the shape fails rather than passing vacuously.
#[test]
fn custom_json_and_key_demand_reach_the_structs_built_from_borrowed_metadata() {
    const JSON: &[&str] = &[
        "--json-serde-derives",
        "true",
        "--json-schema-export",
        "true",
    ];
    // (kind, spec template with `@` marking the directive slot, directive, emitted marker, flags)
    let cells: [(&str, &str, &str, &str, &[&str]); 3] = [
        (
            "generic instance binding, @custom_json",
            "base<T> = [x: T]\nfoo = base<uint>@\nholder = [f: foo]\n",
            "@custom_json",
            "serde::Serialize",
            JSON,
        ),
        (
            "spliced plain group, @custom_json",
            "foo = (a: uint, b: uint)@\nholder = [foo]\n",
            "@custom_json",
            "serde::Serialize",
            JSON,
        ),
        (
            "spliced plain group, @used_as_key",
            "foo = (a: uint, b: uint)@\nholder = [foo]\n",
            "@used_as_key",
            "PartialOrd",
            &[],
        ),
    ];
    for (kind, template, directive, marker, flags) in cells {
        let without = rule_directive_emit(&template.replace('@', ""), flags)
            .unwrap_or_else(|e| panic!("{kind}: the undirected spec must generate, got: {e}"));
        let with = rule_directive_emit(&template.replace('@', &format!(" ; {directive}")), flags)
            .unwrap_or_else(|e| panic!("{kind}: the directed spec must generate, got: {e}"));
        // Counted rather than searched: the marker also appears on the HOLDER struct in every
        // spec, so presence says nothing — what the directive changes is how many structs carry
        // it. `@custom_json` SUPPRESSES the marker on its own struct, `@used_as_key` ADDS it.
        let count = |src: &str| src.matches(marker).count();
        if directive == "@custom_json" {
            assert!(
                count(&without) > count(&with),
                "{kind}: {directive} must suppress `{marker}` on the rule's own struct \
                 (baseline {}, directed {}):\n{with}",
                count(&without),
                count(&with)
            );
        } else {
            assert!(
                count(&with) > count(&without),
                "{kind}: {directive} must add `{marker}` to the rule's own struct \
                 (baseline {}, directed {}):\n{with}",
                count(&without),
                count(&with)
            );
        }
    }
}

/// Every rule kind that CANNOT honor a rule-position directive must say so, not accept it and
/// generate as if it were absent. Each cell pins the message's load-bearing phrase — the diagnosis a
/// user reads — for a shape the directive×rule-shape sweep measured as a silent drop.
///
/// The remedies are probed, not asserted here: each was generated once and read (see the delivery's
/// commit message). What this test owns is that the refusal FIRES on the shape, and with the wording
/// its family already uses — `@name`'s message is shared verbatim with the three other seams that
/// recognize the same misplacement.
#[test]
fn rule_kinds_that_cannot_honor_a_directive_refuse_it() {
    let cells: [(&str, &str, &str, &[&str]); 12] = [
        (
            "@name on the collapsed two-arm 258 set idiom (no variants to name)",
            "foo = #6.258([* uint]) / [* uint] ; @name renamed\nholder = [f: foo]\n",
            "does not rename a top-level rule or group",
            &[],
        ),
        (
            "@name on a plain group nothing splices",
            "foo = (a: uint, b: uint) ; @name renamed\nholder = [z: uint]\n",
            "does not rename a top-level rule or group",
            &[],
        ),
        (
            "@used_as_elem on a generic definition (was an exit-101 assertion)",
            "foo<T> = [x: T] ; @used_as_elem\ninst = foo<uint>\nholder = [f: inst]\n",
            "@used_as_elem on `foo`: a generic DEFINITION names no concrete type",
            &["--wasm", "true"],
        ),
        (
            "@used_as_key on a generic definition",
            "foo<T> = [x: T] ; @used_as_key\ninst = foo<uint>\nholder = [f: inst]\n",
            "@used_as_key on `foo`: a generic DEFINITION names no concrete type",
            &[],
        ),
        (
            "@custom_serialize on a tag-head wrapper rule",
            "foo = #6.42(uint) ; @custom_serialize my_ser\nholder = [f: foo]\n",
            "@custom_serialize on `Foo`: a tag-head rule",
            &[],
        ),
        (
            "@custom_deserialize on the nominalized two-arm 258 set idiom",
            "foo = #6.258([* uint]) / [* uint] ; @custom_deserialize my_deser\nholder = [f: foo]\n",
            "@custom_deserialize on `Foo`: the tag-258 set idiom",
            &[],
        ),
        (
            "@custom_serialize on a generic instance binding",
            "base<T> = [x: T]\nfoo = base<uint> ; @custom_serialize my_ser\nholder = [f: foo]\n",
            "@custom_serialize on `Foo`: this rule binds a generic instantiation",
            &[],
        ),
        (
            "@custom_deserialize on a named binding to a generic set nominal",
            "gset<T> = #6.258([* T]) / [* T]\nfoo = gset<uint> ; @custom_deserialize my_deser\nholder = [f: foo]\n",
            "@custom_deserialize on `Foo`: this rule binds a generic instantiation",
            &[],
        ),
        (
            "@custom_json on an extern marker rule",
            "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @custom_json\nholder = [f: foo]\n",
            "@custom_json on `Foo`: a _CDDL_CODEGEN_EXTERN_TYPE_ rule names a type this crate does not define",
            &[],
        ),
        (
            "@custom_json on a named binding to a generic set nominal",
            "gset<T> = #6.258([* T]) / [* T]\nfoo = gset<uint> ; @custom_json\nholder = [f: foo]\n",
            "@custom_json on `foo`: this rule binds a generic set nominal",
            &[],
        ),
        (
            "@extern_companions on a generic extern base",
            "foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=FooList\ninst = foo<uint>\nholder = [f: inst, g: [* inst]]\n",
            "@extern_companions on `Foo`: a generic extern BASE names no concrete type",
            &["--wasm", "true"],
        ),
        (
            "@ignore on the collapsed two-arm 258 set idiom",
            "foo = #6.258([* uint]) / [* uint] ; @ignore\nholder = [f: foo]\n",
            "@ignore on rule `foo`: this directive is only valid on an open struct-map rest row",
            &[],
        ),
    ];
    for (kind, spec, needle, flags) in cells {
        match rule_directive_emit(spec, flags) {
            Ok(_) => panic!("{kind}: must be refused, but generated"),
            Err(e) => assert!(
                e.contains(needle),
                "{kind}: the rejection must name `{needle}`, got:\n{e}"
            ),
        }
    }
}

/// The uniform never-spliced plain-group refusal covers the WHOLE rule-position vocabulary, not the
/// one directive a hand test would sample. A group no rule splices emits neither a struct nor a
/// field, so every directive written in its trailing slot is inert under both readings — and the
/// message names each one it found, so the author sees what was dropped.
///
/// `@name` is excluded (it has its own long-standing message, pinned above), as are `@rust_name` and
/// `@no_json_schema_export` (each already refused by its own seam, so one misplacement reports once).
#[test]
fn a_plain_group_no_rule_splices_refuses_every_rule_position_directive() {
    // Every entry of `KNOWN_RULE_METADATA_TAGS` except the three the refusal deliberately leaves to
    // their own seams, each with a canonical argument where one is required.
    let directives = [
        "@newtype",
        "@no_alias",
        "@used_as_key",
        "@used_as_elem",
        "@copy",
        "@raw_bytes_flavor",
        "@ignore",
        "@duplicates reject",
        "@custom_json",
        "@custom_serialize my_ser",
        "@custom_deserialize my_deser",
        "@extern_companions dep_wasm=FooList",
        "@doc some prose",
    ];
    for directive in directives {
        let spec = format!("foo = (a: uint, b: uint) ; {directive}\nholder = [z: uint]\n");
        let spelling = directive.split_whitespace().next().unwrap();
        match rule_directive_emit(&spec, &[]) {
            Ok(_) => panic!("`{directive}` on a never-spliced plain group must be refused"),
            Err(e) => {
                assert!(
                    e.contains("the plain group `foo` is never spliced into any rule"),
                    "`{directive}`: expected the never-spliced refusal, got:\n{e}"
                );
                assert!(
                    e.contains(spelling),
                    "`{directive}`: the refusal must name the directive it dropped, got:\n{e}"
                );
            }
        }
    }
    // The placement control: the SAME group, SPLICED, is where a rule-position directive is read —
    // so the refusal keys on splicedness and not merely on the shape being a group.
    assert!(
        rule_directive_emit(
            "foo = (a: uint, b: uint) ; @doc some prose\nholder = [foo]\n",
            &[]
        )
        .is_ok_and(|out| out.contains("/// some prose")),
        "a SPLICED plain group must honor its rule-position @doc, not refuse it"
    );
}

/// The multi-line group-rule trailing-directive spelling — `grp = (\n a: uint\n) ; @rust_name Foo`
/// — is REFUSED, not silently dropped. The pinned `cddl` parser emits no trailing anchor of the
/// group rule's own on the closing-paren line, so its comment-binding trivia merge hands that
/// comment to the FOLLOWING rule's `comments_before_rule` (or orphans it when the group rule is
/// last). Nothing in this repo reads either position, so every rule-position directive a plain
/// group can carry vanished on formatting alone. `parsing::multiline_group_trailing_directive_rejection`
/// refuses the spelling with the two spellings the parser DOES bind.
///
/// Both misbinding cases are covered: a FOLLOWING rule exists to receive the comment, and the group
/// rule is the document's LAST (the comment reaches no AST slot at all).
#[test]
fn multiline_group_rule_trailing_directive_is_refused_not_dropped() {
    let cases = [
        (
            "following rule",
            "grp = (\n  a: uint\n) ; @rust_name Foo\nkeeper = [grp]\n",
        ),
        (
            "last rule of the document",
            "keeper = [grp]\ngrp = (\n  a: uint\n) ; @rust_name Foo\n",
        ),
    ];
    for (kind, spec) in cases {
        match rule_directive_emit(spec, &[]) {
            Ok(out) => {
                panic!("{kind}: the lossy multi-line spelling must be refused, generated:\n{out}")
            }
            Err(e) => {
                assert!(
                    e.contains("group rule `grp`"),
                    "{kind}: the refusal must name the offending rule, got:\n{e}"
                );
                assert!(
                    e.contains("@rust_name"),
                    "{kind}: the refusal must name the directive it found, got:\n{e}"
                );
                assert!(
                    e.contains("ONE line") && e.contains("closing paren on the LAST ENTRY's line"),
                    "{kind}: the refusal must give both supported spellings, got:\n{e}"
                );
            }
        }
    }
}

/// The boundary control in the direction of over-firing: a group written across lines whose closing
/// paren stays on the LAST ENTRY's line binds its trailing comment to that entry's trailing slot,
/// which `group_rule_pin_metadata` reads — so the directive is HONORED and generation succeeds.
/// This is one of the two remedies the refusal advertises, so it has to keep working.
///
/// Two halves, because "honored" has two observable faces here: `@doc` reaches the emitted
/// construct (generation succeeds and the doc line appears), and `@rust_name` reaches
/// `handle_rust_name_pin` — whose own extern-only refusal for this shape is what proves the
/// directive arrived as RULE metadata rather than being intercepted by the multi-line scan.
#[test]
fn group_rule_directive_on_the_last_entry_line_is_honored() {
    const DOC: &str = "the paren stayed on the last entry's line";
    let out = rule_directive_emit(
        &format!("grp = (a: uint,\n  b: uint) ; @doc {DOC}\nkeeper = [grp]\n"),
        &[],
    )
    .expect("the paren-on-last-entry-line spelling must generate");
    assert!(
        out.contains(&format!("/// {DOC}")),
        "the directive must reach the emitted construct:\n{out}"
    );
    // The same spelling delivers `@rust_name` to the rule seam, which refuses it on an exported
    // rule for its OWN (extern-only) reason — never with the multi-line refusal.
    let err = rule_directive_emit(
        "grp = (a: uint,\n  b: uint) ; @rust_name RenamedGrp\nkeeper = [grp]\n",
        &[],
    )
    .expect_err("@rust_name on an exported rule is refused by its own seam");
    assert!(
        err.contains("reserved for extern-interface / stub files")
            && !err.contains("group rule `grp`"),
        "the paren-on-last-entry-line spelling must reach the rule seam, not the multi-line \
         refusal, got:\n{err}"
    );
}

/// The boundary control in the direction of under-accepting: a PROSE trailing comment in the same
/// multi-line position carries no directive, so nothing is lost and nothing is refused. The scan
/// keys on `metadata_from_comments` returning a non-default `RuleMetadata`, not on the comment's
/// mere presence.
#[test]
fn multiline_group_rule_trailing_prose_comment_is_accepted() {
    rule_directive_emit(
        "grp = (\n  a: uint\n) ; not a directive, just prose\nkeeper = [grp]\n",
        &[],
    )
    .expect("a prose trailing comment in the multi-line position must generate");
}

/// The refusal covers the WHOLE rule-position vocabulary, not the one directive a hand test would
/// sample: the scan fires pre-IR off `metadata_from_comments`, so every `KNOWN_RULE_METADATA_TAGS`
/// member is refused uniformly in this position — including the four the single-line slot honors.
/// The list is READ from the authority so a new directive flows in automatically.
#[test]
fn every_known_directive_is_refused_in_the_multiline_group_position() {
    // Canonical argument spellings for the directives whose argument is required (comment_ast
    // panics on a missing one) — keyed by tag so a new authority member surfaces as a miss here.
    let spelling = |tag: &str| -> String {
        match tag {
            "@name" => "@name renamed_foo".to_string(),
            "@rust_name" => "@rust_name RenamedFoo".to_string(),
            "@duplicates" => "@duplicates reject".to_string(),
            "@custom_serialize" => "@custom_serialize my_serialize".to_string(),
            "@custom_deserialize" => "@custom_deserialize my_deserialize".to_string(),
            "@custom_encodings" => "@custom_encodings sz".to_string(),
            "@custom_wire_major" => "@custom_wire_major text".to_string(),
            "@extern_companions" => "@extern_companions dep_wasm=FooList".to_string(),
            "@doc" => "@doc explains the rule".to_string(),
            other => other.to_string(),
        }
    };
    for tag in crate::comment_ast::KNOWN_RULE_METADATA_TAGS {
        let spec = format!(
            "grp = (\n  a: uint\n) ; {}\nkeeper = [grp]\n",
            spelling(tag)
        );
        match rule_directive_emit(&spec, &[]) {
            Ok(_) => {
                panic!("`{tag}` in the multi-line group position must be refused, but generated")
            }
            Err(e) => {
                assert!(
                    e.contains("group rule `grp`") && e.contains(tag),
                    "`{tag}`: expected the multi-line group refusal naming the tag, got:\n{e}"
                );
            }
        }
    }
}

/// Directory (multi-module) input: the offending rule can sit in any module of the concatenated
/// buffer, and the rule identifier still identifies the site (duplicate identifiers across modules
/// are a parse error, so the ident IS unique across the concatenation).
#[test]
fn multiline_group_rule_refusal_names_the_rule_in_a_multi_module_input() {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_mlgroup_dir_{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("a.cddl"), "first = [x: uint]\n").unwrap();
    std::fs::write(
        root.join("b.cddl"),
        "second_grp = (\n  a: uint\n) ; @rust_name Renamed\nkeeper = [second_grp]\n",
    )
    .unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        root.to_str().unwrap(),
        "--output",
        "multiline_group_dir_unused",
        "--wasm",
        "false",
    ]);
    let out = crate::api::generated_strings(&cli);
    std::fs::remove_dir_all(&root).ok();
    let err = out
        .expect_err("the lossy spelling in the second module must be refused")
        .to_string();
    assert!(
        err.contains("group rule `second_grp`") && err.contains("@rust_name"),
        "the refusal must name the rule in the second module and its directive, got:\n{err}"
    );
}
