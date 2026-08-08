//! The component face's BEHAVIORAL gate: a real `wasm32-wasip2` component, loaded into wasmtime and
//! driven through every assertion class the boundary owns.
//!
//! Every other component gate judges emitted bytes. This one is the only place the face is asked
//! what it DOES — and the classes it covers are exactly the ones no static reading can reach: that a
//! fallible door returns `Err` instead of trapping (a trap poisons the instance, so every later
//! caller dies with it), that a getter hands back a snapshot rather than an alias, that
//! `option<option<T>>` really has three states, that the boundary's bytes are byte-identical to the
//! rust crate's own, and — the class with no wasm-face precedent — that lending the same handle as
//! both receiver and argument does not trap.
//!
//! # Shape
//!
//! A NESTED SCRATCH CRATE built by this gate, never a dev-dep of `cddl-codegen`: wasmtime in the bin
//! crate's dev-graph would tax every `local`-tier compile. The host crate's sources live in
//! `tests/component-host/host/**` and are COPIED into the generated output root before the cache key
//! is taken.
//!
//! # Why the host crate is copied INSIDE the hashed root
//!
//! The gate-cache input-closure rule (`tests/README.md` § "The gate cache"): *every input a cached
//! cell reads from scratch must live inside the hashed root, including files the gate itself
//! writes.* A host crate parked beside the hashed tree would be invisible to the key, so editing an
//! assertion would serve the stale PASS forever — and the closure audit cannot flag it, because its
//! allowed-read classes treat everything under scratch as derived-from-hashed. `no_std_check.ts` is
//! the shipped exemplar of the rule applied; this gate follows it, and the copied crate's two path
//! deps stay RELATIVE so the hashed bytes are run-independent.
//!
//! The cached closure asserts more than a cargo exit code (it also checks the built artifact is a
//! component-model binary), so that extra verdict logic is versioned into the key as an explicit
//! argv marker — otherwise a change to what the closure checks would launder old PASSes past it.
//!
//! # Why the scratch root survives between runs
//!
//! wasmtime is a ~60 s cold build. The per-cell OUTPUT trees are freed after their verdict, but the
//! shared `target/` (measured ≈ 3.5 GiB) is kept, so a cache MISS pays only for the local crates.
//! Same-checkout runs serialize on `acquire_scratch_lock`, exactly as `wasm_matrix_roundtrips` does.

use crate::tests::gate_cache;
use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};
use std::path::{Path, PathBuf};

/// Bump on any change to what the cached closure CHECKS (not to the bytes it checks, which the tree
/// hash already covers). Without it, tightening an assertion would be laundered past every
/// previously-cached cell.
const VERDICT_MARKER: &str = "host-behavior-v1";

/// Free scratch below this and the nested build is a coin flip between ENOSPC and a machine-wide
/// stall, so the gate says so and stops rather than reporting a code verdict it did not reach.
///
/// Shared with the composition gate, which builds the same wasmtime-linked shape into its own root.
pub(crate) const SCRATCH_FLOOR_GIB: u64 = 6;
/// Same, for memory: a wasmtime build plus a wasip2 build is the shape that has taken a box
/// unresponsive.
pub(crate) const MEMORY_FLOOR_GIB: u64 = 2;

/// The fixtures this gate RUNS. Deliberately one: the fixture is designed to carry every assertion
/// class at once (see `tests/component-host/inputs/lib.cddl`'s header), and each row here pays a
/// wasmtime-linked `cargo test`. A second row buys breadth only if it carries a class this one
/// cannot express.
const HOST_FIXTURES: &[(&str, &[&str])] = &[("tests/component-host/inputs", &[])];

/// Recursive copy. The host crate is a handful of files, so this is deliberately the simplest thing
/// that puts them inside the hashed root.
pub(crate) fn copy_tree(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).unwrap();
    for entry in std::fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let src = entry.path();
        let dest = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&src, &dest);
        } else {
            std::fs::copy(&src, &dest).unwrap();
        }
    }
}

/// Free bytes on the scratch volume, in GiB. `None` when it cannot be measured — the caller then
/// proceeds with a loud warning rather than blocking on an unmeasurable machine.
fn free_scratch_gib() -> Option<u64> {
    let out = std::process::Command::new("df")
        .args(["-k", "--output=avail"])
        .arg(std::env::temp_dir())
        .output()
        .ok()?;
    let text = String::from_utf8_lossy(&out.stdout);
    let kib: u64 = text.lines().next_back()?.trim().parse().ok()?;
    Some(kib / 1024 / 1024)
}

/// Available memory in GiB, read from `/proc/meminfo`'s `MemAvailable` (the field that already
/// accounts for reclaimable cache, which `free` alone does not).
fn available_memory_gib() -> Option<u64> {
    let text = std::fs::read_to_string("/proc/meminfo").ok()?;
    let line = text.lines().find(|l| l.starts_with("MemAvailable:"))?;
    let kib: u64 = line.split_whitespace().nth(1)?.parse().ok()?;
    Some(kib / 1024 / 1024)
}

/// `Some(reason)` when the machine cannot afford the nested build. Never silent: the caller prints
/// the reason and skips, so an unrun gate is visible in the log rather than indistinguishable from a
/// pass.
///
/// `gate` names the caller in the unmeasurable-machine warnings, so a log line attributes to the
/// gate that printed it. Shared with the composition gate rather than copied: the two build the same
/// wasmtime-linked shape, so the floors that bound one bound the other, and a floor raised in one
/// place must not leave the other at the old number.
pub(crate) fn resource_preflight(gate: &str) -> Option<String> {
    match free_scratch_gib() {
        Some(gib) if gib < SCRATCH_FLOOR_GIB => {
            return Some(format!(
                "only {gib} GiB free on the scratch volume ({:?}); floor is {SCRATCH_FLOOR_GIB} \
                 GiB. The shared target dir alone is ≈3.5 GiB — clear stale scratch \
                 (`rm -rf {}/cddl_codegen_*`) and re-run",
                std::env::temp_dir(),
                std::env::temp_dir().display()
            ));
        }
        Some(_) => {}
        None => eprintln!(
            "{gate}: could not measure free scratch space — proceeding without the ENOSPC preflight"
        ),
    }
    match available_memory_gib() {
        Some(gib) if gib < MEMORY_FLOOR_GIB => {
            return Some(format!(
                "only {gib} GiB available memory; floor is {MEMORY_FLOOR_GIB} GiB. A wasmtime build \
                 beside a wasip2 build is the shape that takes a box unresponsive rather than \
                 failing a gate"
            ));
        }
        Some(_) => {}
        None => eprintln!(
            "{gate}: could not read /proc/meminfo — proceeding without the memory preflight"
        ),
    }
    None
}

/// THE behavioral acceptance gate for this face. Nested cargo, memoized per generated-crate content
/// hash by the gate cache; an unchanged tree re-runs as a visible cached PASS. `GATE_CACHE=0` forces
/// the build.
#[test]
fn component_host_behavior() {
    if let Some(reason) = resource_preflight("component_host") {
        println!("component_host: SKIPPED — {reason}");
        return;
    }

    let scratch_name = format!("cddl_codegen_component_host_{:016x}", checkout_hash());
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    // The root is deliberately NOT removed: `target/` under it is what keeps wasmtime built.
    let target_dir = root.join("target");
    std::fs::create_dir_all(&target_dir).unwrap();

    let mut failures = Vec::new();
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;

    for (input, flags) in HOST_FIXTURES {
        let label = format!("{input} {flags:?}");
        let out = root.join(
            input
                .replace(['/', '\\'], "__")
                .replace(|c: char| !c.is_ascii_alphanumeric() && c != '_', ""),
        );
        // A stale tree would poison the tree hash with files this run did not emit.
        let _ = std::fs::remove_dir_all(&out);
        std::fs::create_dir_all(&out).unwrap();

        let mut args = vec![
            "--input".to_owned(),
            (*input).to_owned(),
            "--output".to_owned(),
            out.to_str().unwrap().to_owned(),
            "--component=true".to_owned(),
            // The rust crate is the component crate's path dependency and nothing else here; the
            // wasm face would only add `__wbindgen_*` imports componentization cannot resolve.
            "--wasm=false".to_owned(),
        ];
        args.extend(flags.iter().map(|f| (*f).to_owned()));
        let generated = codegen_cmd().args(&args).output().unwrap();
        assert!(
            generated.status.success(),
            "{label}: generation failed\n{}",
            String::from_utf8_lossy(&generated.stderr)
        );

        // A workspace root so the three crates share one lock and one target dir. Real consumers own
        // this file; the tool never writes one.
        std::fs::write(
            out.join("Cargo.toml"),
            "[workspace]\nresolver = \"3\"\nmembers = [\"rust\", \"component\", \"host\"]\n",
        )
        .unwrap();
        // The emitted contract, asserted rather than arranged: a component-only tree
        // (`--wasm=false` above) is narrowed to `crate-type = ["rlib"]` by the tool itself — the
        // guest links the rlib, and the cdylib exists only for wasm-bindgen's
        // `wasm32-unknown-unknown` target.
        let rust_manifest = out.join("rust/Cargo.toml");
        let manifest_text = std::fs::read_to_string(&rust_manifest).unwrap();
        assert!(
            manifest_text.contains("crate-type = [\"rlib\"]"),
            "{label}: a component-only tree must be emitted rlib-only, not narrowed by hand:\n\
             {manifest_text}"
        );

        // INSIDE the hashed root, before the key is taken — see this module's header.
        copy_tree(Path::new("tests/component-host/host"), &out.join("host"));

        let component_dir = out.join("component");
        let host_dir = out.join("host");
        let artifact = target_dir
            .join("wasm32-wasip2/debug")
            .join("cddl_lib_component.wasm");
        let outcome = gate_cache::run_cached(
            "component_host",
            &label,
            &out,
            &[
                PathBuf::from("component/Cargo.toml"),
                PathBuf::from("rust/Cargo.toml"),
                PathBuf::from("host/Cargo.toml"),
            ],
            &[
                format!("verdict={VERDICT_MARKER}"),
                "cwd=component".to_owned(),
                "cargo".to_owned(),
                "build".to_owned(),
                "--target".to_owned(),
                "wasm32-wasip2".to_owned(),
                "cwd=host".to_owned(),
                "cargo".to_owned(),
                "test".to_owned(),
            ],
            || {
                let build = tool_cmd("cargo")
                    .args(["build", "--target", "wasm32-wasip2"])
                    .current_dir(&component_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .output()
                    .unwrap();
                if !build.status.success() {
                    let stderr = String::from_utf8_lossy(&build.stderr);
                    // The target is declared in `rust-toolchain.toml`, so a rustup-managed checkout
                    // has it; anywhere else this is a provisioning problem, not a code failure.
                    if stderr.contains("can't find crate for `core`")
                        || stderr.contains("target may not be installed")
                    {
                        failures.push(format!(
                            "{label}: the wasm32-wasip2 target is not installed under the pinned \
                             toolchain — `rustup target add wasm32-wasip2`"
                        ));
                    } else {
                        failures.push(format!("{label}: the guest build failed\n{stderr}"));
                    }
                    return false;
                }
                // A build that produced no COMPONENT would make every assertion below run against
                // the wrong thing: `wasm32-wasip2` artifacts carry the component-model preamble
                // (layer 1), where a core module carries layer 0.
                match std::fs::read(&artifact) {
                    Ok(bytes) if bytes.starts_with(b"\0asm\x0d\0\x01\0") => {}
                    Ok(bytes) => {
                        failures.push(format!(
                            "{label}: {} is not a component-model binary (preamble {:02x?})",
                            artifact.display(),
                            &bytes[..8.min(bytes.len())]
                        ));
                        return false;
                    }
                    Err(e) => {
                        failures.push(format!(
                            "{label}: the guest build reported success but wrote no artifact at \
                             {}: {e}",
                            artifact.display()
                        ));
                        return false;
                    }
                }
                let test = tool_cmd("cargo")
                    .arg("test")
                    .current_dir(&host_dir)
                    .env("CARGO_TARGET_DIR", &target_dir)
                    .env("CDDL_COMPONENT_WASM", &artifact)
                    .output()
                    .unwrap();
                if !test.status.success() {
                    // A dependency that cannot be fetched has to name itself rather than read as a
                    // behavioral failure of the boundary.
                    let stderr = String::from_utf8_lossy(&test.stderr);
                    let cause = if stderr.contains("failed to download")
                        || stderr.contains("no matching package")
                        || stderr.contains("network failure")
                        || stderr.contains("unable to get packages from source")
                    {
                        "the host crate's dependencies (wasmtime 47) could not be fetched — this is \
                         a provisioning failure, not a boundary failure"
                    } else {
                        "the component's behavior does not match the boundary's contract"
                    };
                    failures.push(format!(
                        "{label}: {cause}\n--- stdout ---\n{}\n--- stderr ---\n{stderr}",
                        String::from_utf8_lossy(&test.stdout)
                    ));
                    return false;
                }
                true
            },
        );
        cache_run += outcome.ran();
        cache_hit += outcome.cached();
        // The per-cell tree is freed; `target/` (a sibling, not a child) is what survives.
        let _ = std::fs::remove_dir_all(&out);
    }

    if gate_cache::enabled() {
        println!("component_host gate-cache: {cache_run} run, {cache_hit} cached");
    }
    assert!(
        failures.is_empty(),
        "the generated component does not behave as the boundary promises:\n\n{}",
        failures.join("\n\n")
    );
}

/// The host crate is not compiled by anything in THIS crate's build, so a file added to it and never
/// copied would silently drop out of the gate's input closure — and, being outside the hashed root,
/// would not even change the key. This is the cheap in-process check that the tree the gate copies
/// is the tree the crate needs.
#[test]
fn the_host_crate_carries_the_files_the_gate_copies() {
    for expected in [
        "tests/component-host/host/Cargo.toml",
        "tests/component-host/host/src/lib.rs",
        "tests/component-host/host/tests/behavior.rs",
        "tests/component-host/inputs/lib.cddl",
    ] {
        assert!(
            Path::new(expected).is_file(),
            "{expected} is missing — the behavioral gate would build an incomplete host crate"
        );
    }
    // The two run-independence properties the gate-cache key rests on, asserted against the source
    // rather than trusted: the WIT is reached by a RELATIVE path and the artifact by an env var, so
    // no scratch path is ever baked into the hashed bytes.
    let lib = std::fs::read_to_string("tests/component-host/host/src/lib.rs").unwrap();
    assert!(
        lib.contains("path: \"../component/wit\""),
        "the host crate's `bindgen!` no longer resolves the WIT by a relative path — an absolute \
         scratch path would make every gate-cache key unique to its run"
    );
    assert!(
        lib.contains("CDDL_COMPONENT_WASM"),
        "the host crate no longer takes the component's path from the environment"
    );
    let manifest = std::fs::read_to_string("tests/component-host/host/Cargo.toml").unwrap();
    assert!(
        manifest.contains("path = \"../rust\""),
        "the host crate's path dep on the generated rust crate is gone or absolute — it is the \
         oracle the byte-equality class compares against"
    );
}
