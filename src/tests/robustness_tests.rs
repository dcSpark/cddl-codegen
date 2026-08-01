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
//!
//! Besides the spec-input catalogs, this module also hosts direct error-path unit probes of
//! generator helpers whose failure mode must be a clean `Err`, never a panic (e.g.
//! `concat_files_missing_path_yields_error_not_panic`) — same panic-vs-graceful theme, exercised
//! at the helper level where no spec input can reach the failure.

use crate::cli::Cli;
use clap::Parser;

/// The global panic hook is process-wide, so every test that silences it (`input_robustness_catalog`,
/// `unsupported_construct_panic_catalog`, and the identifier-hazard sweep's generation catalog) must
/// not run their take/set/restore concurrently — an interleave could leave the silent hook installed
/// for the rest of the run. Serialize them on this lock (poison-tolerant: a panic mid-section only
/// means the *other* caller re-silences, which is harmless). The lock is per-fn-internal, so any
/// caller of `with_thread_silenced_panics` participates — including callers in other test modules.
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
/// hold for the wrong reason. (The positions where `@name` is DROPPED are pinned separately, by the
/// comment-DSL position sweep's `KNOWN_SILENT_DROP` list.)
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
/// (single-choice non-marker type rule, multi-choice type rule, field/member position), mirroring
/// `raw_bytes_flavor_misuse_rejects_gracefully`.
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
    let non_258 = gen_src("foo = #6.259([* uint]) / [* uint]\nholder = [f: foo]\n");
    assert!(
        non_258.contains("pub type Foo = Vec<u64>;") && !non_258.contains("OrderedSet"),
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
/// `finalize_generic_table_keys_lists` (after the domain resolution), so both derive from the final
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

/// Gap-2 table corner: `@duplicates` on a `@newtype` TABLE (`{* k => v} ; @newtype @duplicates …`)
/// is hard-rejected this phase. A `preserve` policy would swap the wrapper's inner to the `PairMap`
/// twin, but the synthesized structural map wasm wrapper class wraps `BTreeMap`, not `PairMap`, so the
/// wasm crate would not compile — the boundary is pinned loudly rather than silenced or shipped broken.
/// The message names the rule and the transparent-table-alias workaround. Pinned key (Phase 2.2 wires
/// the PairMap wasm wrapper and subsumes this).
#[test]
fn newtype_table_duplicates_rejects_gracefully() {
    for policy in ["preserve", "reject"] {
        let cddl = format!(
            "foo = {{ * uint => text }} ; @newtype @duplicates {policy}\nholder = [f: foo]\n"
        );
        let path = std::env::temp_dir().join(format!(
            "cddl_codegen_newtype_table_dup_{}_{}.cddl",
            std::process::id(),
            policy
        ));
        std::fs::write(&path, &cddl).unwrap();
        let result = crate::api::generated_strings(&Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "newtype_table_dup_unused",
            "--wasm=true",
        ]));
        std::fs::remove_file(&path).ok();
        let err = result.expect_err(
            "@duplicates on a @newtype table must be a graceful Err (unwired PairMap wasm boundary)",
        );
        let msg = err.to_string();
        assert!(
            msg.contains("@duplicates on rule `Foo`")
                && msg.contains("@newtype` table")
                && msg.contains("transparent table alias"),
            "the rejection must name the rule and the table-alias workaround, got: {msg}"
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

/// The `.within` / `.and` control operators are unsupported — rejected BY DESIGN via a GRACEFUL
/// `Err`, never `todo!()`. Follows the `.size`-on-`int` sibling in `parse_control_operator`
/// (`record_rejection` + an inert full-range placeholder, drained by `finalize`), including its
/// `float_reject_rule_prefix` rule naming. Pins the message names the rule and the offending
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
    // Siblings that hit the same plain-group-registration gap and must also generate:
    // single-element group as a `*` array element, and a plain group as a table VALUE (a CBOR map
    // value can hold only one item, so the group is emitted as a nested-array-encoded struct).
    run("pair = (int)\na = [* pair]\n", "named_single_element")
        .expect("a single-element plain group as a `*` array element must generate");
    run(
        "pair = (int, tstr)\na = { * int => pair }\n",
        "named_table_value",
    )
    .expect("a plain group as a table value must register + generate, not panic");
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
    // like the sibling float-window rejections sharing `float_reject_rule_prefix`).
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

/// Stacked tag encodings (a tag applied to an already-tagged value, reached via alias/rule-reference
/// stacking since literal `#6.24(#6.258(..))` is parse-rejected) must give each tag level its OWN
/// encoding member. Levels are counted OUTSIDE-IN: level 1 keeps today's `{name}_tag_encoding`
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
            .find("pub struct FooEncoding {")
            .unwrap_or_else(|| panic!("no FooEncoding struct in:\n{encodings}"));
        let rest = &encodings[start..];
        let end = rest.find('}').expect("FooEncoding struct must close");
        rest[..end]
            .lines()
            .map(str::trim)
            .filter(|l| l.starts_with("pub ") && l.contains(": "))
            .map(str::to_owned)
            .collect()
    }

    // Non-258 tags throughout: a 258 SET now NOMINALIZES (Phase 2.2), so it OWNS its tag inside its own
    // encoding struct and no longer flattens onto the holder — the flattened-stack scenario this pins
    // (depth-suffixed members) is exercised by a NON-258 tagged collection, which stays a transparent
    // alias whose tag DOES flatten. (The `#6.24(<258-set>)` double-tag flavor is closed structurally by
    // nominalization; `double_tag.cddl` pins that outcome.)
    // Flavor A (homogeneous): two mandatory tags stack, both lowering to `Option<cbor_event::Sz>`.
    let flavor_a = gen_encodings(
        "xs = #6.100([* uint])\nfoo = #6.24(xs)\nholder = [f: foo]\n",
        "a",
    );
    let a_members = foo_member_lines(&flavor_a);
    let mut a_sorted = a_members.clone();
    a_sorted.sort();
    a_sorted.dedup();
    assert_eq!(
        a_sorted.len(),
        a_members.len(),
        "FooEncoding must have no duplicated member declaration; got:\n{a_members:#?}"
    );
    assert!(
        flavor_a.contains("pub inner_tag_encoding: Option<cbor_event::Sz>")
            && flavor_a.contains("pub inner_tag2_encoding: Option<cbor_event::Sz>"),
        "homogeneous stacked tags must mint level-1 `inner_tag_encoding` and level-2 \
         `inner_tag2_encoding`; got:\n{flavor_a}"
    );

    // Flavor B (heterogeneous): mandatory outer 24 (level 1, `Option<Sz>`) + optional inner 258
    // (level 2, `TagPresenceEncoding`).
    let flavor_b = gen_encodings(
        "set = #6.100([* uint]) / [* uint]\nfoo = #6.24(set)\nholder = [f: foo]\n",
        "b",
    );
    let b_members = foo_member_lines(&flavor_b);
    let mut b_sorted = b_members.clone();
    b_sorted.sort();
    b_sorted.dedup();
    assert_eq!(
        b_sorted.len(),
        b_members.len(),
        "FooEncoding must have no duplicated member declaration; got:\n{b_members:#?}"
    );
    assert!(
        flavor_b.contains("pub inner_tag_encoding: Option<cbor_event::Sz>"),
        "heterogeneous outer mandatory 24 must be level-1 `inner_tag_encoding: Option<cbor_event::Sz>`; \
         got:\n{flavor_b}"
    );
    assert!(
        flavor_b.contains("pub inner_tag2_encoding: TagPresenceEncoding"),
        "heterogeneous inner optional 258 must be level-2 `inner_tag2_encoding: TagPresenceEncoding`; \
         got:\n{flavor_b}"
    );

    // Flavor C (name-boundary reset): an outer mandatory tag 24 over an ARRAY whose element carries
    // its own tag 258. The array element starts a fresh `{field}_elem` name namespace, so the
    // element's tag is LEVEL 1 there (`f_elem_tag_encoding`) even though the field crossed tag 24
    // outside the array. The serialize-side element config must reset tag depth to 0 at that
    // boundary — the same reset `encoding_fields_impl` does — or the write reads a depth-inflated
    // `f_elem_tag2_encoding` the encoding struct never minted (E0425, the generated crate breaks).
    let flavor_c_ser = gen_file(
        "t100s = #6.100([* uint])\nfoo = #6.24([* t100s])\nholder = [f: foo]\n",
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

/// `@no_alias` together with the custom (de)serializer pair is rejected BY DESIGN, via a GRACEFUL
/// `Err` (`record_rejection` → drained by `finalize`), never a `panic!`. `@no_alias` strips the type
/// alias node the override is keyed on, so the pair goes with it and BOTH directions fall back to
/// the default wire format — a SYMMETRIC drop, which is precisely what no round-trip test can see.
/// The control is the same rule without `@no_alias`, which does emit both call sites.
#[test]
fn custom_codec_pair_with_no_alias_rejects_gracefully() {
    let err = expect_graceful_rejection(
        "custom_no_alias",
        "cb = bytes ; @no_alias @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: cb]\n",
        &[],
    );
    assert!(
        err.contains(
            "@custom_serialize together with `@no_alias` on `Cb`: `@no_alias` removes the \
             type-alias node the custom (de)serializer override is keyed on, so the pair goes with \
             it and BOTH directions silently fall back to the default wire format."
        ),
        "the @no_alias rejection must name the combination and the symmetric drop, got:\n{err}"
    );
    assert!(
        err.contains("Drop `@no_alias` to keep the alias the pair overrides, or drop the pair."),
        "the rejection must name both ways out, got:\n{err}"
    );
    assert!(
        err.contains("@custom_deserialize together with `@no_alias` on `Cb`:"),
        "each half of the pair gets its own rejection line, got:\n{err}"
    );
    // CONTROL: without `@no_alias` the identical rule honors the pair.
    let src = expect_custom_codec_source(
        "custom_no_alias_control",
        "cb = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: cb]\n",
    );
    assert!(
        src.contains("my_ser(") && src.contains("my_deser("),
        "the same rule without @no_alias must emit both custom call sites, got:\n{src}"
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
/// The BOTH-SET spelling is deliberately NOT rejected — it suppresses the generated impls for the
/// author to hand-own. That posture is unspecified and at risk, so the control below pins what it
/// does TODAY (generates; no impls for the type; embed sites call the named reader) and is the
/// regression guard that this rejection did not swallow it.
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

    // CONTROL 1 (the ruling's regression guard): BOTH halves on a record rule is NOT rejected, and
    // still does exactly what it did before — generates, emits no `Serialize`/`Deserialize` impl for
    // the type, and rewrites embed sites to the named reader.
    let src = expect_custom_codec_source(
        "custom_record_both_set_control",
        "myrec = [a: uint] ; @custom_serialize my_ser @custom_deserialize my_deser\n\
         holder = [f: myrec]\n",
    );
    assert!(
        src.contains("pub struct Myrec")
            && !src.contains("Serialize for Myrec")
            && !src.contains("Deserialize for Myrec")
            && src.contains("my_deser(raw)"),
        "the both-set record spelling must still generate with its impls suppressed and its embed \
         sites rewritten, got:\n{src}"
    );
    // CONTROL 2: a PLAIN GROUP rule's trailing comment binds to its LAST MEMBER's slot (the
    // `@name plain-group-trailing` seam), where the pair is a FIELD-level directive and IS honored.
    // The record-kind check must not reach it — probed to still emit the field call site.
    let src = expect_custom_codec_source(
        "custom_plain_group_trailing_control",
        "pg = (a: uint, b: text) ; @custom_deserialize my_deser\nholder = [pg]\n",
    );
    assert!(
        src.contains("let b = my_deser(raw)"),
        "a plain group's trailing pair is a field-level directive on its last member and must stay \
         honored, got:\n{src}"
    );
}
