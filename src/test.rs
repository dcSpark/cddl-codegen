use std::io::Write;

fn run_test(dir: &str, options: &[&str], export_suffix: Option<&str>) {
    use std::str::FromStr;
    let export_path = match export_suffix {
        Some(suffix) => format!("export_{}", suffix),
        None => "export".to_owned()
    };
    let test_path = std::path::PathBuf::from_str("tests").unwrap().join(dir);
    println!("--------- running test: {} ---------", dir);
    // build and run to generate code
    let mut cargo_run = std::process::Command::new("cargo");
    cargo_run
        .arg("run")
        .arg("--")
        .arg(format!("--input={}", test_path.join("input.cddl").to_str().unwrap()))
        .arg(format!("--output={}", test_path.join(&export_path).to_str().unwrap()));
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
        .write(true)
        .append(true)
        .open(test_path.join(format!("{}/core/src/lib.rs", export_path)))
        .unwrap();
    let deser_test_rs = std::fs::read_to_string(std::path::PathBuf::from_str("tests").unwrap().join("deser_test")).unwrap();
    lib_rs.write("\n\n".as_bytes()).unwrap();
    lib_rs.write_all(deser_test_rs.as_bytes()).unwrap();
    let test_rs = std::fs::read_to_string(test_path.join("tests.rs")).unwrap();
    lib_rs.write("\n\n".as_bytes()).unwrap();
    lib_rs.write_all(test_rs.as_bytes()).unwrap();
    // run tests in generated code
    println!("   ------ testing ------");
    let cargo_test = std::process::Command::new("cargo")
        .arg("test")
        .current_dir(test_path.join(format!("{}/core", export_path)))
        .output()
        .unwrap();
    if !cargo_test.status.success() {
        eprintln!("test stderr:\n{}", String::from_utf8(cargo_test.stderr).unwrap());
    }
    println!("test stdout:\n{}", String::from_utf8(cargo_test.stdout).unwrap());
    assert!(cargo_test.status.success());
    let wasm_export_dir = test_path.join(format!("{}/wasm", export_path));
    let wasm_test_dir = test_path.join("tests_wasm.rs");
    if wasm_test_dir.exists() {
        println!("   ------ testing (wasm) ------");
        let cargo_test_wasm = std::process::Command::new("cargo")
            .arg("test")
            .current_dir(wasm_export_dir)
            .output()
            .unwrap();
        if !cargo_test_wasm.status.success() {
            eprintln!("test stderr:\n{}", String::from_utf8(cargo_test_wasm.stderr).unwrap());
        }
        println!("test stdout:\n{}", String::from_utf8(cargo_test_wasm.stdout).unwrap());
        assert!(cargo_test_wasm.status.success());
    } else if wasm_export_dir.exists() {
        let cargo_build_wasm = std::process::Command::new("cargo")
            .arg("build")    
            .current_dir(wasm_export_dir)
            .output()
            .unwrap();
        if !cargo_build_wasm.status.success() {
            eprintln!("wasm build stderr:\n{}", String::from_utf8(cargo_build_wasm.stderr).unwrap());
        }
        assert!(cargo_build_wasm.status.success());
    }
}

#[test]
fn core_with_wasm() {
    run_test("core", &[], Some("wasm"));
}

#[test]
fn core_no_wasm() {
    run_test("core", &["--wasm=false"], None);
}

#[test]
fn preserve_encodings() {
    run_test("preserve-encodings", &["--preserve-encodings=true"], None);
}

#[test]
fn canonical() {
    run_test("canonical", &["--preserve-encodings=true", "--canonical-form=true"], None);
}

#[test]
fn rust_wasm_split() {
    run_test("rust-wasm-split", &[], None);
}
