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

    // `@name` on a bareword key must be honored exactly as on a quoted key (it was silently dropped
    // on barewords, unlike the Value/Type1 arms). With the same directive the two spellings converge.
    assert_eq!(
        generate("foo = { a: uint, ; @name renamed\n}\n", "bw_name"),
        generate("foo = { \"a\": uint, ; @name renamed\n}\n", "q_name"),
        "@name on a bareword key must converge with @name on the quoted key"
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

/// A ZERO-permitting occurrence (`*` / `0*n` / `*n`) on a keyed struct-map field means the entry
/// may be ABSENT (RFC 8610) — silently narrowing it to a mandatory field generates a decoder that
/// rejects valid CBOR, invisible to round-trip tests (only cross-producer data exposes it). This
/// pins the graceful rejection AND the boundary: `+` (lower bound >= 1) must keep generating a
/// mandatory field, because under unique map keys "one or more" collapses to exactly-one — that is
/// honored semantics, not narrowing. The catalog fixture (`map_field_zero_occurrence`) pins the
/// outcome category; this pins the message and the `+` boundary.
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
    let arrow_nonfixed = run("m = { uint => tstr, 1: uint }\n", "arrow_nonfixed").expect_err(
        "a non-fixed key mixed into a record map used to panic at field naming; must reject",
    );
    assert!(
        arrow_nonfixed.contains("rule `m`") && arrow_nonfixed.contains("non-fixed"),
        "a non-fixed key mixed into a record map should get the non-fixed message, got: {arrow_nonfixed}"
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
/// a `ConceptualRustType::Fixed` domain (which panicked `for_rust_member`, intermediate.rs ~1876, for
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
    // the record path where it is classified NonFixed (a Type1 typename key) and rejected — any
    // record-path rejection message is acceptable; we pin the non-fixed one that actually fires.
    let aliased = run("one = 1\nm = { one => uint }\n", "aliased")
        .expect_err("an aliased literal arrow key domain must reject gracefully, not panic");
    assert!(
        aliased.contains("rule `m`") && aliased.contains("non-fixed"),
        "an aliased literal arrow key is classified NonFixed on the record path, got: {aliased}"
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
    // (the CBOR wire key stays the bareword `if`), and the generated lib.rs must contain `branch`.
    let files = run("kw = { if: uint, ; @name branch\n}\n", "remedy")
        .expect("the @name remedy must generate a valid crate");
    let lib = files
        .iter()
        .find(|(name, _)| name.contains("lib.rs"))
        .map(|(_, src)| src.clone())
        .unwrap_or_default();
    assert!(
        lib.contains("branch"),
        "the @name remedy must emit a field named `branch`, got lib.rs without it"
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
