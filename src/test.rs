use std::io::Write;

/// If you have multiple tests that use the same directory, please use different export_suffix
/// for each one or else the tests will be flaky as they are run concurrently.
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
            .current_dir(wasm_export_dir)
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
            .current_dir(wasm_export_dir)
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
        &[],
    );
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
        &[],
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
