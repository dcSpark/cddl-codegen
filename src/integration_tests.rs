//! End-to-end integration tests: each generates a crate via the CLI (`cargo run`), then compiles
//! and CBOR round-trip-tests it (plus wasm build and json-schema build). This is the correctness
//! gate. Golden snapshots of the generated *source* live in `snapshot_tests.rs`.

use std::io::Write;

/// If you have multiple tests that use the same directory, please use different export_suffix
/// for each one or else the tests will be flaky as they are run concurrently.
///
/// Stable per-checkout discriminator for scratch dirs under `temp_dir()`: concurrent `cargo test`
/// runs from different checkouts/worktrees (an endorsed workflow) would otherwise share a fixed
/// path and `remove_dir_all` each other's fixtures/target mid-run.
fn checkout_hash() -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::env::current_dir().unwrap().hash(&mut h);
    h.finish()
}

fn tool_exists(bin: &str) -> bool {
    std::process::Command::new(bin)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
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
    // build and run to generate code
    let mut cargo_run = std::process::Command::new("cargo");
    cargo_run.arg("run").arg("--").arg(format!(
        "--output={}",
        test_path.join(&export_path).to_str().unwrap()
    ));
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
    // copy tests into generated code
    let mut lib_rs = std::fs::OpenOptions::new()
        .append(true)
        .open(test_path.join(format!("{export_path}/rust/src/lib.rs")))
        .unwrap();
    // some external files/tests pasted in might need this
    lib_rs
        .write_all("\nuse serialization::*;\n".as_bytes())
        .unwrap();
    // copy external files in too (if needed) too
    for external_rust_file_path in external_rust_file_paths {
        let extern_rs = std::fs::read_to_string(external_rust_file_path).unwrap();
        lib_rs.write_all("\n\n".as_bytes()).unwrap();
        lib_rs.write_all(extern_rs.as_bytes()).unwrap();
    }
    let deser_test_rs = std::fs::read_to_string(
        std::path::PathBuf::from_str("tests")
            .unwrap()
            .join("deser_test"),
    )
    .unwrap();
    lib_rs.write_all("\n\n".as_bytes()).unwrap();
    lib_rs.write_all(deser_test_rs.as_bytes()).unwrap();
    let test_rs = std::fs::read_to_string(test_path.join("tests.rs")).unwrap();
    lib_rs.write_all("\n\n".as_bytes()).unwrap();
    lib_rs.write_all(test_rs.as_bytes()).unwrap();
    std::mem::drop(lib_rs);
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
    let cargo_test = std::process::Command::new("cargo")
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
    // copy external wasm defs if they exist
    for external_wasm_file_path in external_wasm_file_paths {
        println!("trying to open: {external_wasm_file_path:?}");
        let mut wasm_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/lib.rs")))
            .unwrap();
        let extern_rs = std::fs::read_to_string(external_wasm_file_path).unwrap();
        wasm_lib_rs.write_all("\n\n".as_bytes()).unwrap();
        if let Some(custom_lib_name) = &custom_lib_name {
            let replaced_extern_rs = extern_rs.replace("cddl_lib", custom_lib_name);
            wasm_lib_rs
                .write_all(replaced_extern_rs.as_bytes())
                .unwrap();
        } else {
            wasm_lib_rs.write_all(extern_rs.as_bytes()).unwrap();
        }
    }
    if wasm_expected && wasm_test_path.exists() {
        // The hook is only real if the file's contents actually land in the crate: append into
        // wasm/src/lib.rs exactly like tests.rs into rust/src/lib.rs. A generated wasm crate ships
        // no #[test]s of its own, so without the append `cargo test` runs zero tests and passes
        // vacuously (which is what this branch silently did before).
        let mut wasm_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/lib.rs")))
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
        let cargo_test_wasm = std::process::Command::new("cargo")
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
        let cargo_build_wasm = std::process::Command::new("cargo")
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
            let wasm_pack = std::process::Command::new("wasm-pack")
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
        let cargo_run_json = std::process::Command::new("cargo")
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
/// Under the DEFAULT profile this is also the corpus EXECUTION gate (TESTING_ROADMAP item 1 / c6):
/// generation adds `--emit-tests` and the rust crate runs `cargo test`, executing the emitted
/// round-trip + reject tests — a corpus construct must round-trip byte-identically, not just
/// compile. One profile keeps the wall-clock bounded (preserve/json stay compile-only for now),
/// and the emitted-module count floor keeps the execution half from going vacuous if emission
/// silently shrinks.
#[test]
fn feature_corpus_compiles() {
    use std::str::FromStr;
    let profiles = crate::snapshot_tests::ALL_PROFILES;
    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    // Scratch dir + one shared target so cbor_event & friends build once (~30 tiny crates × 3).
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_corpus_compile_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    // Fixtures whose generated crate references user-supplied code (e.g. @custom_serialize /
    // @custom_deserialize functions like `my_ser`/`my_deser`) and so can't `cargo check` standalone —
    // same reason extern / raw-bytes live outside the corpus. Still source-snapshotted by feature_corpus.
    const COMPILE_SKIP: &[&str] = &["dsl_custom"];

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
            let gen_out = std::process::Command::new("cargo")
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
                // the default-profile rust crate EXECUTES its emitted tests (strictly stronger
                // than check: `cargo test` compiles the lib and runs the round-trip/reject module)
                let cargo_cmd = if emit_tests && crate_sub == "rust" {
                    "test"
                } else {
                    "check"
                };
                let check = std::process::Command::new("cargo")
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
                && std::fs::read_to_string(out.join("rust/src/lib.rs"))
                    .unwrap_or_default()
                    .contains("mod cddl_generated_tests")
            {
                emitted_test_modules += 1;
            }
        }
    }
    // execution-half vacuous-pass guard: most corpus fixtures mint at least one round-trip/reject
    // test today (32 of 39 at landing; the rest are transparent aliases / pure c-enums). A big drop
    // means the emitter's coverage silently shrank, not that the corpus got simpler.
    assert!(
        emitted_test_modules >= 25,
        "only {emitted_test_modules} corpus fixtures emitted a generated-test module (expected >= 25) — emit_tests coverage shrank"
    );
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        failures.is_empty(),
        "corpus crates failed to compile:\n\n{}",
        failures.join("\n\n")
    );
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
/// `SKIP` holds the deliberately-red cells (pre-existing gaps tracked in `cddl-matrix/ROADMAP.md`, plus
/// `extern`, which references a user-supplied type and can't compile standalone). A fix lands by taking
/// its cell off `SKIP` — and the guard below fails if a `SKIP` cell starts compiling, so the list can't
/// silently rot. A cell that's red but NOT in `SKIP` fails the test: it's a new wasm-ABI bug to fix or
/// (deliberately, with a ledger entry) skip-list. `cargo check`s only the wasm crate (single default
/// profile) — lighter than `feature_corpus_compiles`; upgrade to a round-trip oracle once that harness
/// lands (see `tests/TESTING_ROADMAP.md`).
#[test]
fn wasm_matrix_compiles() {
    use std::str::FromStr;

    // Deliberately-red cells (`<shape>__<role>`), each tracked in cddl-matrix/ROADMAP.md.
    const SKIP: &[&str] = &[
        // extern references a user-supplied type (undefined standalone -> E0425); the extern emit path
        // is integration-tested in tests/extern-deps. Permanent skip (never compiles here).
        "extern__array-element",
    ];

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

    // Scratch dir + one shared target so cbor_event/wasm-bindgen build once, then each tiny crate checks.
    let root =
        std::env::temp_dir().join(format!("cddl_codegen_wasm_matrix_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT on SKIP — real bugs
    let mut resurfaced = vec![]; // SKIP cells that now compile — remove them from SKIP
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        let skipped = SKIP.contains(&stem);
        let out = root.join(stem);
        let gen_out = std::process::Command::new("cargo")
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
        let check = std::process::Command::new("cargo")
            .arg("check")
            .current_dir(&wasm_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        match (skipped, check.status.success()) {
            (false, false) => failures.push(format!(
                "{stem}: cargo check failed (new wasm-ABI red cell — fix the emitter or, deliberately, \
                 add to SKIP + cddl-matrix/ROADMAP.md)\n{}",
                String::from_utf8_lossy(&check.stderr)
            )),
            (true, true) => resurfaced.push(stem.to_string()),
            _ => {} // (false,true)=green as expected; (true,false)=red as expected
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these SKIP-listed wasm-matrix cells now compile — remove them from SKIP (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "wasm-matrix cells failed to compile:\n\n{}",
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
        let gen_out = std::process::Command::new("cargo")
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
        let lib = std::fs::read_to_string(out.join("wasm/src/lib.rs")).unwrap();
        let n_invocations = lib.matches("impl_wasm_list!(").count();
        assert!(
            n_invocations >= 5,
            "{label}: only {n_invocations} impl_wasm_list! invocations emitted (expected >= 5) — \
             the flag stopped collapsing list wrappers, so this gate no longer gates the macro path"
        );
        // wire in the real macro definitions the emitted invocations reference
        let mut cargo_toml = std::fs::OpenOptions::new()
            .append(true)
            .open(out.join("wasm/Cargo.toml"))
            .unwrap();
        cargo_toml
            .write_all(b"wasm-macro-crate = { path = \"../../../wasm-macro-crate\" }\n")
            .unwrap();
        std::mem::drop(cargo_toml);
        let check = std::process::Command::new("cargo")
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

// ---------------------------------------------------------------------------
// Tracked wasm-ABI SEMANTIC gaps (compile-green but not ideal).
//
// wasm-bindgen can't represent a nested `Option<Option<T>>`, so a nullable value
// (`T / null` -> `Option<T>`) sitting where the accessor adds its own presence-`Option` is FLATTENED to
// a single `Option<T>` at the wasm boundary. That compiles (the matrix gate is green), and the native
// rust types keep all three states so CBOR round-trips are unaffected — but the wasm READ conflates
// "absent" with "present-but-null". This is a behavioural/round-trip property the compile gate can't
// see; verifying it mechanically needs the round-trip harness (TESTING_ROADMAP item 1), so until then
// these stand as tracked, skipped failing tests (remove `#[ignore]` + write the real round-trip
// assertion once the harness or a fidelity fix lands). The ideal: the getter alone is unambiguous —
// via a presence accessor, or by exposing the nullable as `Option<wrapper-struct>`.
// See tests/TESTING_ROADMAP.md item 2 ("behavioural frontier").

/// UNRECOVERABLE gap: an optional nullable struct field (`[pre: uint, ? field0: (uint / null)]`).
/// The wasm getter `field0() -> Option<u64>` returns `None` for BOTH absent and present-null, and there
/// is no presence accessor, so a JS consumer cannot tell them apart. (The map-value flatten is the same
/// read-conflation but is recoverable via `keys()`.)
#[test]
#[ignore = "wasm three-state fidelity: optional-nullable field getter conflates absent vs present-null (unrecoverable). See tests/TESTING_ROADMAP.md item 2."]
fn wasm_optional_nullable_field_three_state_fidelity() {
    unimplemented!(
        "Optional-nullable struct field flattens Option<Option<T>> -> Option<T> at the wasm boundary, \
         conflating absent with present-null on read with no way to distinguish them. Add three-state \
         fidelity (a presence accessor, or expose the nullable as Option<wrapper-struct>) or wire this \
         to the round-trip harness (TESTING_ROADMAP item 1), then assert distinguishability and remove \
         #[ignore]."
    );
}

/// Ambiguous (recoverable) + one unrecoverable sub-case, in the enum getters (`add_wasm_enum_getters`).
/// A type/group-choice enum with a nullable variant (`… / (text / null)`) emits `as_variant() ->
/// Option<T>` flattened — recoverable via `kind()` but ambiguous from the getter alone. A *double*-nested
/// optional variant is worse: the `as_variant()` getter is silently skipped (build-time `println!`
/// only), so that variant's value is unreadable from wasm.
#[test]
#[ignore = "wasm three-state fidelity: enum nullable-variant getter is ambiguous (recoverable via kind()); a double-nested variant getter is silently skipped. See tests/TESTING_ROADMAP.md item 2."]
fn wasm_enum_nullable_variant_three_state_fidelity() {
    unimplemented!(
        "Enum `as_variant()` for a nullable variant flattens Option<Option<T>> -> Option<T> (ambiguous \
         without kind()); a double-nested optional variant getter is emitted as nothing at all. Give the \
         getter unambiguous three-state fidelity (or emit the skipped double-nested getter) or wire this \
         to the round-trip harness (TESTING_ROADMAP item 1), then assert and remove #[ignore]."
    );
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
// the round-trip harness, TESTING_ROADMAP item 1, lands and covers it).

/// `a...b` must EXCLUDE b (max valid = b-1) but the generator emits `max = b+1`:
/// `[v: 0...10]` generates `max: Some(11)`, accepting both 10 and 11 (two invalid values).
#[test]
#[ignore = "exclusive range upper bound mis-computed: a...b emits max=b+1 instead of b-1 (accepts two out-of-range values). Pinned by tests/corpus/snapshots/exclusive_range; ledgered in cddl-matrix/ROADMAP.md."]
fn corpus_exclusive_range_upper_bound() {
    unimplemented!(
        "0...10 generates `if v > 11` / `max: Some(11)`; the correct exclusive max is 9. Fix the \
         bound in parsing.rs (range_end + 1 -> range_end - 1), re-bless the exclusive_range corpus \
         snapshots, then assert here that 9 round-trips and 10/11 are rejected, and remove #[ignore]."
    );
}

/// `[+ uint]` (>=1) and `[2*5 uint]` (2..=5) both emit a bare `Vec<u64>` with no length check —
/// out-of-bounds lengths serialize and deserialize without error.
#[test]
#[ignore = "occurrence-count constraints (+, n*m) are not enforced on homogeneous arrays: emitted Vec has no length check. Pinned by tests/corpus/snapshots/occurrence; ledgered in cddl-matrix/ROADMAP.md."]
fn corpus_occurrence_bounds_enforced() {
    unimplemented!(
        "[+ uint] and [2*5 uint] emit bare Vec<u64> with no length validation in serialize or \
         deserialize. Add bounds enforcement, re-bless the occurrence corpus snapshots, then assert \
         here that an empty / 6-element vector is rejected, and remove #[ignore]."
    );
}

/// `[(uint, tstr)]` generates a 1-field struct (`read_elems(1)`) — the `tstr` member is silently
/// dropped: data loss that parses, compiles, snapshots, and round-trips green.
#[test]
#[ignore = "inline-group splice drops members: [(uint, tstr)] emits a 1-field struct, silently losing tstr. Pinned by tests/corpus/snapshots/inline_group; ledgered in cddl-matrix/ROADMAP.md."]
fn corpus_inline_group_members_kept() {
    unimplemented!(
        "[(uint, tstr)] generates InlineGroup {{ index_0: u64 }} and never reads the tstr. Flatten \
         inline-group entries into the record, re-bless the inline_group corpus snapshots, then \
         assert here that both members round-trip, and remove #[ignore]."
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
/// Pinned to the same rev as Cargo.toml so the two never diverge.
const CDDL_ORACLE_DEP: &str =
    "\ncddl = { git = \"https://github.com/dcSpark/cddl\", rev = \"d6cad9ee99f732e2ecb330a373c6a68f4e2860b7\" }\n";

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

/// Executes the `--emit-tests` generated-test module end-to-end (TESTING_ROADMAP item 1): generate
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
    let lib = std::fs::read_to_string("tests/preserve-encodings/export_emit_tests/rust/src/lib.rs")
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
}

#[test]
fn canonical() {
    run_test(
        "canonical",
        &["--preserve-encodings=true", "--canonical-form=true"],
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
/// and `tests/TESTING_ROADMAP.md` item 7.
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

/// Covers the shipped `static/json-ts-types.js` (TESTING_ROADMAP.md item 7), which `--package-json`
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
