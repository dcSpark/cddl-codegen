//! THE acceptance gate for the component face's cross-crate story: two independently generated
//! crates, built as two components, COMPOSED into one world, and driven through the flow the whole
//! feature exists to make possible.
//!
//! Everything upstream of here stops one step short of the payoff. The WIT gates judge a projection.
//! `component_import_wasip2_build` judges that a consumer's imported-resource glue COMPILES.
//! `component_host` judges what ONE component does at runtime. Only this gate makes two separately
//! generated crates agree at runtime — which is the claim the face exists for, because without it a
//! consumer's dependency types are its own private resources, structurally identical to the
//! dependency's and interchangeable with nothing.
//!
//! # What it asserts, and why each needs a run
//!
//! - **The composed world exports BOTH interfaces.** Read back out of the encoded artifact, not out
//!   of the composer's own graph. This is what `wac plug` destroys and what a host needs in order to
//!   mint a dependency object at all.
//! - **A dependency object crosses in and comes back live.** Minted on the dependency's exported
//!   interface, passed into a consumer constructor, returned by a consumer getter, and then read AND
//!   MUTATED through the dependency's own interface. The mutation is the part that distinguishes a
//!   live resource in the dependency instance's table from a value the host is holding.
//! - **The repeated position works end to end**, through the accumulator the Rust lowering forces.
//! - **The composed boundary's bytes are the native crates' bytes.**
//! - **The instantiate-once mistake is invisible at compose time and fatal at runtime.** Both halves
//!   are pinned, because the first is the reason the second has to be documented rather than left to
//!   the toolchain.
//!
//! # Shape
//!
//! A NESTED SCRATCH CRATE built by this gate, never a dev-dep of `cddl-codegen`: wasmtime AND
//! wac-graph in the bin crate's dev-graph would tax every `local`-tier compile. The composer runs
//! IN-PROCESS inside that crate (`wac-graph`, which resolves `wasmparser`/`wasm-encoder`/
//! `wasm-metadata` at the 0.247 floor the WIT gates already pin) rather than shelling out to a `wac`
//! binary, whose version would sit outside the lockfile between the gate and its verdict.
//!
//! The host crate's sources live in `tests/component-compose/host/**` and are COPIED into the
//! generated output root before the cache key is taken — the gate-cache input-closure rule
//! (`tests/README.md` § "The gate cache"): every input a cached cell reads from scratch must live
//! inside the hashed root, or an edit to an assertion serves the stale PASS forever. Its three path
//! deps stay RELATIVE so the hashed bytes are run-independent, and the two generated WIT packages are
//! copied into `host/wit/deps/**` inside the same root, so a change to either projection reaches the
//! composed world the host binds against.
//!
//! The cached closure asserts far more than a cargo exit code, so that verdict logic is versioned
//! into the key as an explicit argv marker — otherwise tightening an assertion would be laundered
//! past every previously-cached cell.
//!
//! # Why the scratch root survives between runs
//!
//! wasmtime is a ~60 s cold build. The per-cell OUTPUT tree is freed after its verdict; the shared
//! `target/` (measured 3.3 GiB with both the wasip2 and native graphs in it) is kept, so a cache
//! MISS pays only for the local crates — 91 s cold against 7 s once the root is warm. The root is
//! this gate's own rather than shared with `component_host`: the two hash different trees under
//! different gate ids, so sharing would buy only
//! a build cache while giving `component_host`'s "the root deliberately survives" contract a second
//! owner. Same-checkout runs serialize on `acquire_scratch_lock`.

use crate::tests::component_host_tests::{copy_tree, resource_preflight};
use crate::tests::gate_cache;
use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};
use std::path::{Path, PathBuf};

/// Bump on any change to what the cached closure CHECKS (not to the bytes it checks, which the tree
/// hash already covers). Without it, tightening an assertion would be laundered past every
/// previously-cached cell.
const VERDICT_MARKER: &str = "compose-acceptance-v2";

const FIXTURES: &str = "tests/component-compose";

/// The two crates' library names. They are not decoration: `--lib-name X` is what makes the emitted
/// WIT package `cddl:X@0.1.0`, which is the identifier the hand-written composed world in
/// `tests/component-compose/host/wit/world.wit` names and the identifier the host crate registers
/// each built component under. All three move together — `the_compose_fixture_carries_the_files_the_gate_copies`
/// is what says so out loud.
const DEP_LIB: &str = "chain";
const CONSUMER_LIB: &str = "wallet";

/// THE acceptance gate for the cross-crate face. Nested cargo, memoized per generated-crate content
/// hash by the gate cache; an unchanged tree re-runs as a visible cached PASS. `GATE_CACHE=0` forces
/// the build.
#[test]
fn component_compose_acceptance() {
    if let Some(reason) = resource_preflight("component_compose") {
        println!("component_compose: SKIPPED — {reason}");
        return;
    }

    let scratch_name = format!("cddl_codegen_component_compose_{:016x}", checkout_hash());
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    // The root is deliberately NOT removed: `target/` under it is what keeps wasmtime built.
    let target_dir = root.join("target");
    std::fs::create_dir_all(&target_dir).unwrap();

    let out = root.join("acceptance");
    // A stale tree would poison the tree hash with files this run did not emit.
    let _ = std::fs::remove_dir_all(&out);
    std::fs::create_dir_all(&out).unwrap();

    let dep_out = out.join(DEP_LIB);
    let consumer_out = out.join(CONSUMER_LIB);

    // The dependency FIRST: the consumer reads two of its committed outputs (the extern-interface
    // export and the WIT package), so generation order is a real dependency, not a convention.
    let generated = codegen_cmd()
        .args([
            "--input",
            &format!("{FIXTURES}/dep/lib.cddl"),
            "--output",
            dep_out.to_str().unwrap(),
            "--wasm=false",
            "--component=true",
            "--lib-name",
            DEP_LIB,
        ])
        .output()
        .unwrap();
    assert!(
        generated.status.success(),
        "generating the dependency failed\n{}",
        String::from_utf8_lossy(&generated.stderr)
    );

    let generated = codegen_cmd()
        .args([
            "--input",
            &format!("{FIXTURES}/consumer/lib.cddl"),
            "--output",
            consumer_out.to_str().unwrap(),
            "--wasm=false",
            "--component=true",
            // ONE serialization runtime for both crates, so the dependency's types implement the
            // same `Deserialize`/`ToCBORBytes` the consumer's glue names across the seam — the
            // precondition the bytes seam inherits from every other cross-crate consumer here.
            "--common-import-override",
            DEP_LIB,
            "--extern-import",
            &format!(
                "{DEP_LIB}={}",
                dep_out.join("extern-interface").join(DEP_LIB).display()
            ),
            "--component-extern-wit",
            &format!("{DEP_LIB}={}", dep_out.join("component/wit").display()),
            // Cargo path dependencies, RELATIVE (they land in committed manifests).
            "--rust-dep",
            &format!("{DEP_LIB}=../../{DEP_LIB}/rust"),
            "--component-dep",
            &format!("{DEP_LIB}=../../{DEP_LIB}/rust"),
            "--lib-name",
            CONSUMER_LIB,
        ])
        .output()
        .unwrap();
    assert!(
        generated.status.success(),
        "generating the consumer failed\n{}",
        String::from_utf8_lossy(&generated.stderr)
    );

    // A workspace root so the five crates share one lock and one target dir. Real consumers own this
    // file; the tool never writes one. The host is a member so it resolves against the same lock —
    // and is NOT among the packages built for wasip2 below, because wasmtime does not cross-compile
    // to it (its `cc` build script wants a wasi sysroot's `stdlib.h`).
    std::fs::write(
        out.join("Cargo.toml"),
        format!(
            "[workspace]\nresolver = \"3\"\nmembers = [\"{DEP_LIB}/rust\", \"{DEP_LIB}/component\", \
             \"{CONSUMER_LIB}/rust\", \"{CONSUMER_LIB}/component\", \"host\"]\n"
        ),
    )
    .unwrap();
    // The rust crates' `cdylib` output exists for wasm-bindgen's `wasm32-unknown-unknown` target;
    // the guest consumes the rlib, and asking the wasip2 linker for a cdylib is not what this gate
    // is about. Same narrowing the other component build gates do, for the same reason.
    for lib in [DEP_LIB, CONSUMER_LIB] {
        let manifest = out.join(lib).join("rust/Cargo.toml");
        let narrowed = std::fs::read_to_string(&manifest).unwrap().replace(
            "crate-type = [\"cdylib\", \"rlib\"]",
            "crate-type = [\"rlib\"]",
        );
        std::fs::write(&manifest, narrowed).unwrap();
    }

    // INSIDE the hashed root, before the key is taken — see this module's header.
    let host_dir = out.join("host");
    copy_tree(Path::new(&format!("{FIXTURES}/host")), &host_dir);
    // ... and so are the two generated WIT packages the hand-written composed world resolves
    // against. Copied rather than checked in, so a change to either projection reaches the world the
    // host binds against instead of drifting from it.
    for lib in [DEP_LIB, CONSUMER_LIB] {
        let deps = host_dir.join("wit/deps").join(lib);
        std::fs::create_dir_all(&deps).unwrap();
        std::fs::copy(
            out.join(lib).join("component/wit/world.wit"),
            deps.join("world.wit"),
        )
        .unwrap();
    }

    let dep_artifact = target_dir
        .join("wasm32-wasip2/debug")
        .join(format!("{DEP_LIB}_component.wasm"));
    let consumer_artifact = target_dir
        .join("wasm32-wasip2/debug")
        .join(format!("{CONSUMER_LIB}_component.wasm"));

    let mut failure = None;
    let outcome = gate_cache::run_cached(
        "component_compose",
        "chain+wallet",
        &out,
        &[
            PathBuf::from(format!("{DEP_LIB}/rust/Cargo.toml")),
            PathBuf::from(format!("{DEP_LIB}/component/Cargo.toml")),
            PathBuf::from(format!("{CONSUMER_LIB}/rust/Cargo.toml")),
            PathBuf::from(format!("{CONSUMER_LIB}/component/Cargo.toml")),
            PathBuf::from("host/Cargo.toml"),
        ],
        &[
            format!("verdict={VERDICT_MARKER}"),
            "cargo".to_owned(),
            "build".to_owned(),
            "--target".to_owned(),
            "wasm32-wasip2".to_owned(),
            format!("-p={DEP_LIB}-component"),
            format!("-p={CONSUMER_LIB}-component"),
            "cwd=host".to_owned(),
            "cargo".to_owned(),
            "test".to_owned(),
        ],
        || {
            let build = tool_cmd("cargo")
                .args([
                    "build",
                    "--target",
                    "wasm32-wasip2",
                    "-p",
                    &format!("{DEP_LIB}-component"),
                    "-p",
                    &format!("{CONSUMER_LIB}-component"),
                ])
                .current_dir(&out)
                .env("CARGO_TARGET_DIR", &target_dir)
                .output()
                .unwrap();
            if !build.status.success() {
                let stderr = String::from_utf8_lossy(&build.stderr);
                // The target is declared in `rust-toolchain.toml`, so a rustup-managed checkout has
                // it; anywhere else this is a provisioning problem, not a code failure.
                failure = Some(
                    if stderr.contains("can't find crate for `core`")
                        || stderr.contains("target may not be installed")
                    {
                        "the wasm32-wasip2 target is not installed under the pinned toolchain — \
                         `rustup target add wasm32-wasip2`"
                            .to_owned()
                    } else {
                        format!("the two guest builds failed\n{stderr}")
                    },
                );
                return false;
            }
            // A build that produced no COMPONENT would make every assertion below run against the
            // wrong thing, and composition would fail with a message about the wrong layer:
            // `wasm32-wasip2` artifacts carry the component-model preamble (layer 1) where a core
            // module carries layer 0.
            for artifact in [&dep_artifact, &consumer_artifact] {
                match std::fs::read(artifact) {
                    Ok(bytes) if bytes.starts_with(b"\0asm\x0d\0\x01\0") => {}
                    Ok(bytes) => {
                        failure = Some(format!(
                            "{} is not a component-model binary (preamble {:02x?})",
                            artifact.display(),
                            &bytes[..8.min(bytes.len())]
                        ));
                        return false;
                    }
                    Err(e) => {
                        failure = Some(format!(
                            "the guest build reported success but wrote no artifact at {}: {e}",
                            artifact.display()
                        ));
                        return false;
                    }
                }
            }
            // Composition AND the behavioral flow both live in the host crate's own tests, so the
            // composer's dependency stays out of this crate's graph and a failure names the
            // assertion class rather than a step number.
            let test = tool_cmd("cargo")
                .arg("test")
                .current_dir(&host_dir)
                .env("CARGO_TARGET_DIR", &target_dir)
                .env("CDDL_CHAIN_WASM", &dep_artifact)
                .env("CDDL_WALLET_WASM", &consumer_artifact)
                .output()
                .unwrap();
            if !test.status.success() {
                // A dependency that cannot be fetched has to name itself rather than read as a
                // failure of the composed boundary.
                let stderr = String::from_utf8_lossy(&test.stderr);
                let cause = if stderr.contains("failed to download")
                    || stderr.contains("no matching package")
                    || stderr.contains("network failure")
                    || stderr.contains("unable to get packages from source")
                {
                    "the host crate's dependencies (wasmtime 47, wac-graph 0.10) could not be \
                     fetched — this is a provisioning failure, not a composition failure"
                } else {
                    "two generated components do not compose into a world that behaves as the \
                     cross-crate seam promises"
                };
                failure = Some(format!(
                    "{cause}\n--- stdout ---\n{}\n--- stderr ---\n{stderr}",
                    String::from_utf8_lossy(&test.stdout)
                ));
                return false;
            }
            true
        },
    );
    if gate_cache::enabled() {
        println!(
            "component_compose gate-cache: {} run, {} cached",
            outcome.ran(),
            outcome.cached()
        );
    }
    let verdict = failure.is_none();
    let message = failure.unwrap_or_default();
    // The per-cell tree is freed; `target/` (a sibling, not a child) is what survives.
    let _ = std::fs::remove_dir_all(&out);
    assert!(verdict, "{message}");
}

/// The compose fixture is not compiled by anything in THIS crate's build, so a file added to it and
/// never copied would silently drop out of the gate's input closure — and, being outside the hashed
/// root, would not even change the key. This is the cheap in-process check that the tree the gate
/// copies is the tree the crate needs.
#[test]
fn the_compose_fixture_carries_the_files_the_gate_copies() {
    for expected in [
        "tests/component-compose/dep/lib.cddl",
        "tests/component-compose/consumer/lib.cddl",
        "tests/component-compose/host/Cargo.toml",
        "tests/component-compose/host/wit/world.wit",
        "tests/component-compose/host/src/lib.rs",
        "tests/component-compose/host/tests/acceptance.rs",
    ] {
        assert!(
            Path::new(expected).is_file(),
            "{expected} is missing — the acceptance gate would build an incomplete host crate"
        );
    }

    // The run-independence properties the gate-cache key rests on, asserted against the source
    // rather than trusted: the WIT is reached by a RELATIVE path and both artifacts by env vars, so
    // no scratch path is ever baked into the hashed bytes.
    let lib = std::fs::read_to_string("tests/component-compose/host/src/lib.rs").unwrap();
    assert!(
        lib.contains("path: \"wit\""),
        "the host crate's `bindgen!` no longer resolves the composed world by a relative path — an \
         absolute scratch path would make every gate-cache key unique to its run"
    );
    for var in ["CDDL_CHAIN_WASM", "CDDL_WALLET_WASM"] {
        assert!(
            lib.contains(var),
            "the host crate no longer takes {var} from the environment"
        );
    }
    let manifest = std::fs::read_to_string("tests/component-compose/host/Cargo.toml").unwrap();
    for dep in [
        &format!("path = \"../{DEP_LIB}/rust\""),
        &format!("path = \"../{CONSUMER_LIB}/rust\""),
    ] {
        assert!(
            manifest.contains(dep.as_str()),
            "the host crate's path dep `{dep}` is gone or absolute — both are the oracle the \
             byte-differential class compares against"
        );
    }

    // The three places the two package identifiers are spelled have to agree, and only one of them
    // is derived: `--lib-name X` (this module) mints `cddl:X@0.1.0` (the emitted WIT), which the
    // hand-written composed world and the host crate's composer both name literally. A rename that
    // moved only one would fail deep inside a nested cargo build with a message about a WIT package
    // nobody wrote; this says it here instead.
    let world = std::fs::read_to_string("tests/component-compose/host/wit/world.wit").unwrap();
    for lib_name in [DEP_LIB, CONSUMER_LIB] {
        let iface = format!("cddl:{lib_name}/types@0.1.0");
        assert!(
            world.contains(&format!("export {iface};")),
            "the composed world must export `{iface}` — the gate generates that crate with \
             `--lib-name {lib_name}`, which is what names its WIT package"
        );
        assert!(
            lib.contains(&format!("\"{iface}\"")),
            "the host crate's composer must wire `{iface}` — the same identifier the world exports"
        );
        assert!(
            lib.contains(&format!("Package::from_file(\"cddl:{lib_name}\"")),
            "the host crate's composer must register the built component under `cddl:{lib_name}`, \
             the package name `--lib-name {lib_name}` mints"
        );
    }
}
