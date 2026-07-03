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

/// Spawn cargo/wasm-pack for building a *generated* crate. The generated code is the harness's
/// own output and legitimately over-imports; CI's `setup-rust-toolchain` injects
/// `RUSTFLAGS="-D warnings"` into the job env, which nested cargo builds would otherwise inherit
/// and fail on those unused-import warnings. The root workspace keeps `-D warnings` via the
/// dedicated Build/clippy steps; only these nested generated-crate builds must be insulated.
fn tool_cmd(program: &str) -> std::process::Command {
    let mut c = std::process::Command::new(program);
    c.env_remove("RUSTFLAGS");
    c
}

/// Append the in-repo user-supplied `RawBytesEncoding` defs (`PubKey`) into a freshly generated crate
/// rooted at `out` (rust + wasm), so a `rawbytes__*` wasm-matrix cell — whose `_CDDL_CODEGEN_RAW_BYTES_TYPE_`
/// resolves to that user type — compiles/tests standalone instead of being a permanent SKIP like `extern`.
/// Mirrors `run_test`'s external-file append (including the `use serialization::*;` the rust def needs for
/// `RawBytesEncoding`/`Deserialize*`); the matrix never passes `--lib-name`, so the wasm def's `cddl_lib`
/// path needs no substitution here.
fn append_raw_bytes_defs(out: &std::path::Path) {
    use std::io::Write;
    let rust_def = std::fs::read_to_string("tests/external_rust_raw_bytes_def").unwrap();
    let mut rust_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(out.join("rust/src/lib.rs"))
        .unwrap();
    rust_lib
        .write_all(b"\n\nuse serialization::*;\n\n")
        .unwrap();
    rust_lib.write_all(rust_def.as_bytes()).unwrap();
    std::mem::drop(rust_lib);
    let wasm_def = std::fs::read_to_string("tests/external_wasm_raw_bytes_def").unwrap();
    let mut wasm_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(out.join("wasm/src/lib.rs"))
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
    // build and run to generate code
    let mut cargo_run = tool_cmd("cargo");
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
/// Under the DEFAULT profile this is also the corpus EXECUTION gate (TESTING_ROADMAP item 1 / c6):
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
                && std::fs::read_to_string(out.join("rust/src/lib.rs"))
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
/// `extern`, which references a user-supplied type and can't compile standalone). `rawbytes__*` cells also
/// reference a user-supplied type, but its defs are in-repo — `append_raw_bytes_defs` splices them in per
/// cell (same 2 commands, no extra cargo invocation), so those cells compile for real instead of SKIP-ing.
/// A fix lands by taking
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
        // defs are in-repo, so append them and the cell compiles for real instead of being a SKIP.
        if stem.starts_with("rawbytes__") {
            append_raw_bytes_defs(&out);
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

/// The wasm-ABI matrix ROUND-TRIP gate — the behavioural upgrade of `wasm_matrix_compiles`. Same cell
/// enumeration (`tests/matrix_wasm/*.cddl`), but each cell is generated with `--wasm=true
/// --emit-tests=true` and `cargo test`ed (not `cargo check`ed): this compiles AND RUNS the emitted
/// `cddl_generated_wasm_tests` module (cross-crate byte differential + wire round-trip + accessor
/// read-back + boundary acceptance — see `src/emit_tests_wasm.rs`). A cell can `cargo check` green
/// (compile gate) while the wrapper API does a semantically wrong same-type conversion; that only
/// surfaces when the emitted assertions RUN, which is what this gate adds.
///
/// MANUAL/LOCAL ONLY — `#[ignore]`d so it stays out of CI under the feature freeze (`cargo test`
/// per cell is materially heavier than the compile gate's per-cell `cargo check`). Run it with
/// `cargo test --bin cddl-codegen wasm_matrix_roundtrips -- --ignored`.
///
/// `SKIP` holds the deliberately-red cells with a per-entry reason. Same four-state verdict matrix as
/// `wasm_matrix_compiles`: a red non-SKIP cell fails (real finding, or deliberately SKIP-list it with a
/// ledger reason); a SKIP cell that now passes fails the resurfaced guard (a fix landed — take it off
/// SKIP). `wasm_matrix_compiles` stays byte-for-byte untouched: the compile verdict remains the
/// always-on CI floor, this is the manual round-trip verdict on top. Its own scratch dir name lets it
/// run beside the compile gate. Note: a cell whose shape mints no wasm test surface (nothing the
/// emitter can faithfully build — e.g. a pure c-enum, or a wrapper/collection ctor arg with no wasm
/// build) simply emits no module and `cargo test` passes with zero emitted tests; that is a
/// legitimate green here (the emitter skips loudly), NOT a false pass — the compile gate already
/// pins that the cell's wasm ABI compiles.
#[test]
#[ignore]
fn wasm_matrix_roundtrips() {
    use std::str::FromStr;

    // Deliberately-red cells (`<shape>__<role>`), each with its reason. The wrapper-collection
    // struct-field cells (`coll__struct-field` `nums = [* uint]`, `collmap__struct-field`,
    // `passthrumap__struct-field`) round-trip green: the emitter builds their `&Nums`/`&Mp` ctor arg
    // through the wrapper's `new`/`add`/`insert` API, taking the wrapper NAME from the UNRESOLVED
    // conceptual type (`emit_tests_wasm::wasm_collection_build`) so it doesn't shallow-resolve the
    // alias into a bare `vec![..]` against the `&Nums` param — so they are NOT SKIP-listed.
    const SKIP: &[&str] = &[
        // extern references a user-supplied type (undefined standalone -> E0425); the extern emit path
        // is integration-tested in tests/extern-deps. Permanent skip (never compiles, so never tests).
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

    // Own scratch dir (distinct from wasm_matrix_compiles) + one shared target so cbor_event/
    // wasm-bindgen/the libtest harness build once, then each tiny crate tests incrementally.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_wasm_matrix_rt_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![]; // red cells NOT on SKIP — real findings
    let mut resurfaced = vec![]; // SKIP cells that now pass — remove them from SKIP
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        let skipped = SKIP.contains(&stem);
        let out = root.join(stem);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=true")
            .arg("--emit-tests=true")
            .output()
            .unwrap();
        if !gen_out.status.success() {
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
            if skipped {
                resurfaced.push(format!("{stem} (emits no wasm crate)"));
            } else {
                failures.push(format!(
                    "{stem}: generated no wasm crate (expected a wasm wrapper for every cell — the cell \
                     is no longer being round-trip-gated)"
                ));
            }
            continue;
        }
        // See wasm_matrix_compiles: append the in-repo raw-bytes defs so `rawbytes__*` cells compile/run.
        if stem.starts_with("rawbytes__") {
            append_raw_bytes_defs(&out);
        }
        let test = tool_cmd("cargo")
            .arg("test")
            .current_dir(&wasm_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
            .output()
            .unwrap();
        match (skipped, test.status.success()) {
            (false, false) => failures.push(format!(
                "{stem}: cargo test failed (wasm round-trip red cell — fix the emitter/generator or, \
                 deliberately, add to SKIP + a ledger reason)\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&test.stdout),
                String::from_utf8_lossy(&test.stderr)
            )),
            (true, true) => resurfaced.push(stem.to_string()),
            _ => {} // (false,true)=green as expected; (true,false)=red as expected
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "these SKIP-listed wasm-matrix cells now round-trip — remove them from SKIP (a fix landed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "wasm-matrix cells failed to round-trip:\n\n{}",
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
        let lib = std::fs::read_to_string(out.join("wasm/src/lib.rs")).unwrap();
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

/// Smoke gate for documented flag *values* that no other test or profile exercises (closes the
/// TESTING_ROADMAP "five documented flag values with zero coverage" pending decision for the
/// rust-side four). Each selects a whole alternative emit path: `--annotate-fields=false` (a
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
    let lib = std::fs::read_to_string(out.join("wasm/src/lib.rs")).unwrap();
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

/// `a...b` must EXCLUDE b (max valid = b-1). `[v: 0...10]` must emit `max: Some(9)` — NOT the old
/// `max: Some(11)` (which accepted the out-of-spec 10 and 11). Asserts on the COMMITTED snapshot so a
/// regression can't slip back in via an unreviewed re-bless, mirroring `corpus_inline_group_members_kept`.
/// The behavioral half (9 round-trips, 10/11 rejected) is owned by the `--emit-tests` reject cases and
/// the `ir_conformance_corpus` oracle.
#[test]
fn corpus_exclusive_range_upper_bound() {
    let lib = std::fs::read_to_string(
        "tests/corpus/snapshots/exclusive_range/default__rust__src__lib.rs.snap",
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
        "tests/corpus/snapshots/occurrence/default__rust__src__serialization.rs.snap",
    )
    .expect("occurrence serialization snapshot missing");
    for check in [
        "if o_arr.len() < 1 {",
        "if b_arr.len() < 2 || b_arr.len() > 5 {",
        "if inline_bounded_arr.len() < 1 || inline_bounded_arr.len() > 3 {",
    ] {
        assert!(
            ser.contains(check),
            "occurrence snapshot lost the occurrence-count length check `{check}`"
        );
    }
    // the value-misread form bound each ELEMENT read through `.and_then(|x| if x < ... )` —
    // occurrence bounds must never re-attach to element values
    assert!(
        !ser.contains("found: x as isize"),
        "occurrence snapshot has an element VALUE RangeCheck — occurrence counts are being \
         misread as element value bounds again"
    );
}

/// Special-class (major-type-7) map KEYS must deserialize through the map loop, not be
/// intercepted as a potential break byte. The definite-length loop reads exactly `n` entries
/// (`make_deser_loop_break_check` gates its Special peek on the INDEFINITE case only — the same
/// fix as the array-element half, 2a50524), so a bool key flows straight to `bool::deserialize`.
/// This asserts on the COMMITTED special_map_key snapshots so the interception can't come back
/// via an unreviewed re-bless; the *executed* proof is the fixture's emit-tests round-trip in
/// `feature_corpus_compiles` (it mints a real `(false, 0)` entry — mutation-verified: an
/// unconditional break check fails it with EndingBreakMissing at BkeyHolder.named).
#[test]
fn corpus_special_map_key_supported() {
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/special_map_key/default__rust__src__serialization.rs.snap",
    )
    .expect("special_map_key serialization snapshot missing");
    assert!(
        ser.contains("bool::deserialize(raw)?"),
        "special_map_key snapshot no longer deserializes the bool key through the element path"
    );
    // every Special peek in the map loops must be gated on the indefinite case — an ungated
    // `raw.cbor_type()? == cbor_event::Type::Special` check would eat definite-length bool keys
    assert_eq!(
        ser.matches("if raw.cbor_type()? == cbor_event::Type::Special")
            .count(),
        ser.matches("if let cbor_event::Len::Indefinite = ").count(),
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
        "tests/corpus/snapshots/inline_group/default__rust__src__lib.rs.snap",
    )
    .expect("inline_group lib snapshot missing");
    assert!(
        lib.contains("index_0") && lib.contains("index_1"),
        "inline_group snapshot no longer keeps both spliced members — the [(uint, tstr)] member-drop bug is back"
    );
    let ser = std::fs::read_to_string(
        "tests/corpus/snapshots/inline_group/default__rust__src__serialization.rs.snap",
    )
    .expect("inline_group serialization snapshot missing");
    assert!(
        ser.contains("read_elems(2)"),
        "inline_group deserializer no longer reads 2 elements — the [(uint, tstr)] member-drop bug is back"
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
const CDDL_ORACLE_DEP: &str = "\ncddl = { git = \"https://github.com/dcSpark/cddl\", rev = \"d6cad9ee99f732e2ecb330a373c6a68f4e2860b7\" }\n";

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

/// Executes the `--emit-tests` generated WASM-test module end-to-end (TESTING_ROADMAP item 2, the
/// behavioural frontier): generate the rich `core` fixture with `--wasm=true --emit-tests=true`, then
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
        .output()
        .unwrap();
    if !generate.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&generate.stderr));
    }
    assert!(generate.status.success());

    // The wasm crate builds the rust crate as a (non-test) dependency, so the rust lib only needs to
    // COMPILE — append just the production externs it references (extern types + custom serializers),
    // NOT the rust test suite (deser_test/tests.rs), whose core-specific `--emit-tests` incompat is
    // out of scope here (see the doc comment).
    let mut rust_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(export_path.join("rust/src/lib.rs"))
        .unwrap();
    rust_lib.write_all(b"\nuse serialization::*;\n").unwrap();
    for f in ["external_rust_defs", "custom_serialization"] {
        rust_lib.write_all(b"\n\n").unwrap();
        rust_lib
            .write_all(
                std::fs::read_to_string(test_path.parent().unwrap().join(f))
                    .unwrap()
                    .as_bytes(),
            )
            .unwrap();
    }
    std::mem::drop(rust_lib);

    // The wasm crate: append the extern wasm defs it references + the hand-written tests_wasm.rs
    // (which runs beside the emitted module as the plausibility cross-check).
    let wasm_lib_path = export_path.join("wasm/src/lib.rs");
    let mut wasm_lib = std::fs::OpenOptions::new()
        .append(true)
        .open(&wasm_lib_path)
        .unwrap();
    for f in [
        test_path.parent().unwrap().join("external_wasm_defs"),
        test_path.join("tests_wasm.rs"),
    ] {
        wasm_lib.write_all(b"\n\n").unwrap();
        wasm_lib
            .write_all(std::fs::read_to_string(&f).unwrap().as_bytes())
            .unwrap();
    }
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

/// The IR-bug conformance oracle at breadth (TESTING_ROADMAP "IR-bug oracle at breadth"). The
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

    // Fixtures excluded from the conformance sweep (generated WITHOUT --emit-tests-conformance) —
    // each with a concrete reason the oracle can't soundly judge it. Kept honest: a fixture only
    // belongs here for a *validator/minter* gap, never to paper over a real bug.
    //   - dsl_custom: references user-supplied @custom_serialize fns; can't compile standalone
    //     (same reason feature_corpus_compiles skips it).
    //   - sized_int: VALIDATOR GAP. Its spec has `i_8: -128..127` and `i_64: int .size 8`; the cddl
    //     validator can't parse a range whose lower bound is a negative int ("lower value must be a
    //     uint type. got -128") nor `.size` on a signed `int` ("target for .size must a string or
    //     uint data type, got int"). Our minted values are in-spec (all zeros) — this is a limitation
    //     of the oracle's constraint evaluator, not an encoder bug (see tests/README.md).
    const CONFORMANCE_SKIP: &[&str] = &["dsl_custom", "sized_int"];

    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    let conformance_helpers = std::fs::read_to_string("tests/deser_test_conformance.rs").unwrap();

    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_ir_conformance_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    // The oracle's distinctive panic message (assert_cddl_conforms) — proves an expected-fail
    // fixture failed *for the right reason*, not via some unrelated compile/test break.
    const ORACLE_MSG: &str = "cddl conformance failed for rule";

    let mut failures = vec![];
    let mut fixed_or_toothless = vec![]; // EXPECTED_FAIL fixtures that unexpectedly passed
    let mut validated_fixtures = 0usize; // vacuity floor: fixtures that actually emitted a conformance call
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        if CONFORMANCE_SKIP.contains(&stem) {
            continue;
        }
        let expected_fail = EXPECTED_FAIL.contains(&stem);
        let out = root.join(stem);
        let gen_out = tool_cmd("cargo")
            .args(["run", "--"])
            .arg(format!("--input={}", input.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .arg("--wasm=false")
            .arg("--emit-tests=true")
            .arg("--emit-tests-conformance=true")
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
        // wire in the shared oracle helpers (cddl_oracle_load_spec / assert_cddl_conforms) that the
        // emitted cddl_conformance::validate calls resolve to — append them to lib.rs, like run_test
        // appends deser_test_conformance.rs into the preserve fixture.
        let lib_rs_path = rust_dir.join("src/lib.rs");
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

        // vacuity: did this fixture actually emit any conformance call? (a fixture whose only
        // round-trip types are transparent array/table aliases emits none — see occurrence)
        let lib_src = std::fs::read_to_string(&lib_rs_path).unwrap();
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

        let test = tool_cmd("cargo")
            .arg("test")
            .current_dir(&rust_dir)
            .env("CARGO_TARGET_DIR", &target_dir)
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
                 IR-level miscompile (mints spec-violating bytes) or a validator gap to document \
                 + add to CONFORMANCE_SKIP:\n{combined}"
            )),
        }
    }

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
    let ser_on = std::fs::read_to_string(out_on.join("rust/src/serialization.rs")).unwrap();
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
    let ser_off = std::fs::read_to_string(out_off.join("rust/src/serialization.rs")).unwrap();
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
/// `EXPECTED_FAIL` pins the known per-profile generation failures with a reason each (the default
/// profile has none — every supported construct generates there). Four-state verdict per
/// (profile, fixture), mirroring the wasm-matrix SKIP pattern: a NON-expected failure fails the
/// gate (a real regression, or deliberately add it here with a reason); an EXPECTED failure that
/// now generates fine fails the gate as "resurfaced" (remove it — the gap closed), so the list
/// can't rot.
#[test]
#[ignore]
fn all_supported_constructs_generate_all_profiles() {
    use crate::cli::Cli;
    use clap::Parser;

    // (profile, fixture stem, reason) — constructs that FAIL to generate under a non-default
    // profile. The default profile has no entries: `all_supported_constructs_generate` already
    // proves every fixture generates there. A cell fails "as expected" if generation errors OR
    // panics under EITHER wasm mode for that (profile, fixture). Only preserve has entries today,
    // and both are the SAME float `unimplemented!` class (generation.rs "preserve_encodings is not
    // implemented for float"): `number = int / float` and `time` (a float epoch). Tracked by the
    // `preserve_encodings_supports_floats` stub; when that lands, these clear and become resurfaced.
    const EXPECTED_FAIL: &[(&str, &str, &str)] = &[
        (
            "preserve",
            "prelude.number",
            "number = int / float; float aborts generation under --preserve-encodings \
             (generation.rs 'preserve_encodings is not implemented for float'). See the \
             preserve_encodings_supports_floats stub.",
        ),
        (
            "preserve",
            "prelude.time",
            "time is a float epoch; float aborts generation under --preserve-encodings \
             (generation.rs 'preserve_encodings is not implemented for float'). See the \
             preserve_encodings_supports_floats stub.",
        ),
    ];

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
            let expected = EXPECTED_FAIL
                .iter()
                .find(|(p, i, _)| p == profile && i == &id)
                .map(|(_, _, reason)| *reason);
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
                (Some(_), None) => resurfaced.push(format!("{id}/{profile}")),
                (None, Some(detail)) => failures.push(detail),
                _ => {} // (Some,Some)=red as expected; (None,None)=green as expected
            }
        }
    }
    assert!(
        resurfaced.is_empty(),
        "these EXPECTED_FAIL supported constructs now generate — remove them from EXPECTED_FAIL \
         (the gap closed):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "matrix-supported constructs failed to generate under a non-default profile (regression, \
         or deliberately add to EXPECTED_FAIL with a reason):\n{}",
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

    // Same as `feature_corpus_compiles`: references user-supplied @custom_serialize functions, so
    // its crate can't build standalone under any profile.
    const COMPILE_SKIP: &[&str] = &["dsl_custom"];

    // (profile, fixture stem, reason) — cells whose emitted round-trip surface is a known
    // structural gap under that profile. Empirically discovered; a resurfaced guard fails the gate
    // if any starts passing so the list can't rot.
    const SKIP: &[(&str, &str, &str)] = &[
        // Both cells fail the encoding-fidelity `indef_containers` variant with EndingBreakMissing:
        // a bool element/key is CBOR major type 7 (Special), the same major type as the
        // indefinite-length break byte, and `make_deser_loop_break_check` can only peek
        // `cbor_type()` (a `fill_buf` byte peek needs a `BufRead` bound the type-erased choice
        // closures can't carry — see that fn's docs). So an INDEFINITE container of specials
        // mis-reads its first element as the break. Pre-existing limitation documented in both
        // fixtures' comments; surfaced at scale by the fidelity oracle. Definite-length framing
        // (every other mutation class) is unaffected.
        (
            "preserve",
            "homogeneous_array",
            "indefinite container of bool (major-7) elements: break-check consumes the element (EndingBreakMissing) — pre-existing limitation, see fixture comment",
        ),
        (
            "preserve",
            "special_map_key",
            "indefinite map with bool (major-7) keys: break-check consumes the key (EndingBreakMissing) — pre-existing limitation, see fixture comment",
        ),
    ];

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

    // Own scratch dir + one shared target so cbor_event/wasm-bindgen/the libtest harness build once.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_corpus_roundtrip_profiles_{:016x}",
        checkout_hash()
    ));
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
            if std::fs::read_to_string(out.join("rust/src/lib.rs"))
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
