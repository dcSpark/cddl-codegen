//! End-to-end integration tests: each generates a crate via the CLI (`cargo run`), then compiles
//! and CBOR round-trip-tests it (plus wasm build and json-schema build). This is the correctness
//! gate. Golden snapshots of the generated *source* live in `snapshot_tests.rs`.

use std::io::Write;

/// If you have multiple tests that use the same directory, please use different export_suffix
/// for each one or else the tests will be flaky as they are run concurrently.
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
    let wasm_test_dir = test_path.join("tests_wasm.rs");
    // copy external wasm defs if they exist
    for external_wasm_file_path in external_wasm_file_paths {
        println!("trying to open: {external_wasm_file_path:?}");
        let mut wasm_lib_rs = std::fs::OpenOptions::new()
            .append(true)
            .open(test_path.join(format!("{export_path}/wasm/src/lib.rs")))
            .unwrap();
        let extern_rs = std::fs::read_to_string(external_wasm_file_path).unwrap();
        wasm_lib_rs.write_all("\n\n".as_bytes()).unwrap();
        // we must replace the lib name if it's not the default
        if let Some(custom_lib_name) = options.iter().find_map(|arg: &&str| {
            arg.split_once("--lib-name=")
                .map(|(_, lib_name)| lib_name.replace('-', "_"))
        }) {
            let replaced_extern_rs = extern_rs.replace("cddl_lib", &custom_lib_name);
            wasm_lib_rs
                .write_all(replaced_extern_rs.as_bytes())
                .unwrap();
        } else {
            wasm_lib_rs.write_all(extern_rs.as_bytes()).unwrap();
        }
    }
    if wasm_test_dir.exists() {
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
    } else if wasm_export_dir.exists() {
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
    // check that the JSON schema export crate builds
    let json_export_dir = test_path.join(format!("{export_path}/wasm/json-gen"));
    if json_export_dir.exists() {
        let cargo_build_json = std::process::Command::new("cargo")
            .arg("build")
            .current_dir(json_export_dir)
            .output()
            .unwrap();
        if !cargo_build_json.status.success() {
            eprintln!(
                "wasm build stderr:\n{}",
                String::from_utf8(cargo_build_json.stderr).unwrap()
            );
        }
        assert!(cargo_build_json.status.success());
    }
}

/// Generate + `cargo check` every `tests/corpus/*.cddl` crate under each emission profile. The
/// snapshot suite (`snapshot_tests::feature_corpus`) only pins the generated *source*, so a
/// construct that emits non-compiling Rust would be snapshotted as "correct"; this is the compile
/// gate for it. Runs all three `default`/`preserve`/`json` profiles the corpus is snapshotted
/// under, since non-compiling output can be flag-specific (a bare construct compiled but its
/// preserve/json variant did not). Rust-only (`--wasm=false`) and one shared `CARGO_TARGET_DIR` so
/// the deps build once. `int` needs no extern defs here — the generator emits its own `Int` type.
#[test]
fn feature_corpus_compiles() {
    use std::str::FromStr;
    // Mirrors snapshot_tests::ALL_PROFILES (kept in sync by hand — both are tiny).
    let profiles: &[(&str, &[&str])] = &[
        ("default", &[]),
        ("preserve", &["--preserve-encodings=true"]),
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"],
        ),
    ];
    let corpus_dir = std::path::PathBuf::from_str("tests/corpus").unwrap();
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&corpus_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("cddl"))
        .collect();
    entries.sort();
    assert!(!entries.is_empty(), "no corpus files in {corpus_dir:?}");

    // Scratch dir + one shared target so cbor_event & friends build once (~30 tiny crates × 3).
    let root = std::env::temp_dir().join("cddl_codegen_corpus_compile");
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures = vec![];
    for input in &entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        for (profile, extra) in profiles {
            let label = format!("{stem}/{profile}");
            let out = root.join(format!("{stem}__{profile}"));
            // generate (rust only)
            let gen_out = std::process::Command::new("cargo")
                .args(["run", "--"])
                .arg(format!("--input={}", input.to_str().unwrap()))
                .arg(format!("--output={}", out.to_str().unwrap()))
                .arg("--wasm=false")
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
            // cargo check the generated rust crate
            let check = std::process::Command::new("cargo")
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
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(
        failures.is_empty(),
        "corpus crates failed to compile:\n\n{}",
        failures.join("\n\n")
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

#[test]
fn preserve_encodings() {
    use std::str::FromStr;
    let custom_ser_path = std::path::PathBuf::from_str("tests")
        .unwrap()
        .join("custom_serialization_preserve");
    run_test(
        "preserve-encodings",
        &["--preserve-encodings=true"],
        None,
        &[custom_ser_path],
        &[],
        false,
        &[],
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
