//! The component face's JS-HOST gate: real `wasm32-wasip2` components transpiled with `jco` and
//! driven from node.
//!
//! Every other component gate judges the face from Rust — emitted bytes, a WIT projection, or a
//! wasmtime host. But the motivating consumer for this whole feature is a JS dApp, and a JS host
//! reaches the component through a TRANSPILER rather than a runtime: `jco` rewrites the component
//! into an ES module graph, and the surface it synthesizes is not the wasmtime one. The deltas are
//! real and consumer-visible — a WIT `enum` arrives as a STRING label and rejects the numeric
//! discriminant the wasm-bindgen face takes; a fallible door THROWS rather than returning; resource
//! disposal hangs off an own property rather than a prototype. None of that is observable from any
//! gate above this one, and all of it is a claim `docs/docs/component_differences.mdx` makes.
//!
//! # The three legs
//!
//! 1. **surface** — `tests/component-host/inputs/lib.cddl`, one component, transpiled alone. Every
//!    row of the JS-face table in `component_differences.mdx`, asserted at run time rather than read
//!    off the emitted `.d.ts`: what jco INTENDS and what the boundary DOES disagreed on the enum
//!    face, which is exactly the class a `.d.ts` reading cannot catch.
//! 2. **crosscrate** — `tests/component-compose/{dep,consumer}/lib.cddl`, the same two specs
//!    `component_compose` drives through wasmtime, here transpiled SEPARATELY and wired by jco's
//!    `--map`. This is the payoff leg and the shape the docs prescribe. Instantiate-once — which
//!    `component_compose` has to pin by hand because a composer cannot promise it — comes free from
//!    ES module semantics here.
//! 3. **composed-is-broken** — a KNOWN-BROKEN PIN. The `wac`-composed dual-export world that
//!    `component_compose` drives correctly through wasmtime is BROKEN under jco 1.26.1, and its worse
//!    symptom is silent (a dependency-typed getter returns a different object). Pinned so the docs'
//!    "do not transpile a composed artifact" instruction has an owner, and so a jco fix is reported
//!    as the good news it is rather than discovered years later.
//!
//! **Fixtures are REUSED, never duplicated.** The two CDDL specs above are the ones the wasmtime
//! gates already drive — so a disagreement between this gate and `component_host` /
//! `component_compose` is a finding about the HOST, not about the emitter. Only the JS drivers in
//! `tests/component-jco/js/**` are new.
//!
//! # Cost, and why `local`
//!
//! Cheaper than either wasmtime gate: no wasmtime, no composer crate, no native host crate — the only
//! cargo work is three wasip2 guest builds, and `jco transpile` is sub-second. Its provisioning is
//! more fragile than theirs, though (node, npm, and a first run that needs the NETWORK), which is why
//! the skip is TIER-DEPENDENT rather than unconditional: see [`missing_provisioning`].
//!
//! # Scratch layout, and the gate-cache input closure
//!
//! ```text
//! <root>/                          persistent; survives between runs
//!   node_modules/                  `npm ci` installs HERE — huge, and must NOT be hashed
//!   package.json, package-lock.json    copied from the fixture; what `npm ci` consumes
//!   target/                        shared cargo target dir (wasip2 only)
//!   out/                           THE HASHED TREE — deleted and recreated each run
//!     cddl-lib/ chain/ wallet/     the generated crates
//!     js/                          `tests/component-jco/js/**`, incl. package.json + lockfile
//!     compose.wac                  the composition script for leg 3
//!     transpiled/                  jco output
//! ```
//!
//! Node resolves `@bytecodealliance/*` by walking UP from the transpiled modules, so `node_modules`
//! at `<root>` serves the tests while staying outside the hashed tree — which it must, being a
//! ~200 MB install that has nothing to do with what the gate asserts.
//!
//! The fixture's `package.json` and `package-lock.json` therefore land in TWO places: at `<root>`,
//! where `npm ci` consumes them, and inside `out/js/`, where the tree hash sees them. That second
//! copy is the gate-cache input-closure rule (`tests/README.md` § "The gate cache"): a pinned
//! dependency version is an input to every verdict here, so a lockfile bump MUST move the key. If it
//! did not, bumping jco would serve the stale PASS forever — the cached cell would be answering a
//! question about a version nobody runs any more.
//!
//! The cached closure asserts far more than a cargo exit code, so that verdict logic is versioned
//! into the key as an explicit argv marker. The ambient `wac` version is in the key too, because it
//! decides WHICH legs ran: a cell cached without `wac` covers two legs, and installing `wac` has to
//! re-run rather than inherit that verdict.
//!
//! # Why the scratch root survives between runs
//!
//! `npm ci` needs the network on a cold cache and the three guest builds are the whole cargo cost, so
//! the per-cell OUTPUT tree is freed after its verdict while `node_modules/` and `target/` (siblings,
//! not children) are kept. Same-checkout runs serialize on `acquire_scratch_lock`.

use crate::tests::component_host_tests::{copy_tree, resource_preflight};
use crate::tests::gate_cache;
use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};
use std::path::{Path, PathBuf};

/// Bump on any change to what the cached closure CHECKS (not to the bytes it checks, which the tree
/// hash already covers). Without it, tightening an assertion would be laundered past every
/// previously-cached cell.
const VERDICT_MARKER: &str = "jco-three-legs-v1";

/// The fixture tree copied WHOLE into the hashed root: `js/**` plus the `.wac` composition script.
const FIXTURES: &str = "tests/component-jco";

/// The surface leg's spec — a DIRECTORY input, the same one `component_host` drives.
const SURFACE_INPUT: &str = "tests/component-host/inputs";
/// The cross-crate leg's specs — the same pair `component_compose` drives.
const COMPOSE_FIXTURES: &str = "tests/component-compose";

/// The surface leg's crate directory. `--lib-name` is left at its default, so the emitted WIT package
/// is `cddl:cddl-lib@0.1.0` and the built artifact is `cddl_lib_component.wasm` — both spelled
/// literally in `js/surface.test.mjs`, which is what
/// `the_jco_fixture_carries_the_files_the_gate_copies` says out loud.
const SURFACE_LIB: &str = "cddl-lib";
/// The two cross-crate library names. As in `component_compose`, `--lib-name X` is what makes the
/// emitted WIT package `cddl:X@0.1.0` — the identifier the JS drivers and `compose.wac` name.
const DEP_LIB: &str = "chain";
const CONSUMER_LIB: &str = "wallet";

/// The pinned npm dependencies, spelled here so the fixture-inventory test can hold the lockfile to
/// them and so the docs quote ONE source. Exact, never a range: a range would let a background npm
/// release change this gate's verdict without changing a byte in the repo.
const JCO_VERSION: &str = "1.26.1";
const SHIM_VERSION: &str = "0.19.0";

/// The `wac` floor for leg 3. Below this the composed leg loud-skips ALONE — it is the gate's only
/// ambient-binary dependency and the other two legs must never wait on it.
const WAC_FLOOR: (u32, u32) = (0, 9);

/// The `full` tier sets this, and it turns every loud skip below into a hard failure.
///
/// The reason the skip is tier-dependent at all: a silent (or merely loud) skip in the tier that
/// SHIPS a feature voids the guarantee that tier exists to give, while `local` is run dozens of times
/// a day on machines that may legitimately lack node or a network. Same posture, and the same
/// mechanism, as `no_std_check`'s absent-target outcome.
const REQUIRED_ENV: &str = "CDDL_JCO_REQUIRED";

fn required() -> bool {
    matches!(std::env::var(REQUIRED_ENV), Ok(v) if v == "1" || v.eq_ignore_ascii_case("true"))
}

/// A provisioning outcome: loud SKIP at `local`, hard FAIL at `full`. Never silent either way — an
/// unrun gate has to be visible in the log rather than indistinguishable from a pass.
fn missing_provisioning(reason: &str) {
    assert!(
        !required(),
        "component_jco: {reason}\n\nThis is a hard failure because {REQUIRED_ENV}=1 — the `full` \
         tier ships the feature, so its JS-host leg may not be skipped. At `local` the same \
         condition is a loud SKIP."
    );
    println!("component_jco: SKIPPED — {reason}");
}

/// `Some(version)` when `program --version` runs, else `None`.
fn version_of(program: &str) -> Option<String> {
    let out = tool_cmd(program).arg("--version").output().ok()?;
    out.status
        .success()
        .then(|| String::from_utf8_lossy(&out.stdout).trim().to_owned())
}

/// The first `MAJOR.MINOR` in a version banner (`wac-cli 0.9.0` -> `(0, 9)`).
fn major_minor(banner: &str) -> Option<(u32, u32)> {
    let digits = banner.split(|c: char| !(c.is_ascii_digit() || c == '.'));
    for token in digits {
        let mut parts = token.split('.');
        if let (Some(Ok(major)), Some(Ok(minor))) = (
            parts.next().map(str::parse::<u32>),
            parts.next().map(str::parse::<u32>),
        ) {
            return Some((major, minor));
        }
    }
    None
}

/// THE JS-host gate. Nested cargo plus a node run, memoized per generated-crate content hash by the
/// gate cache; an unchanged tree re-runs as a visible cached PASS. `GATE_CACHE=0` forces the work.
#[test]
fn component_jco_js_host() {
    if let Some(reason) = resource_preflight("component_jco") {
        println!("component_jco: SKIPPED — {reason}");
        return;
    }
    let (Some(node), Some(npm)) = (version_of("node"), version_of("npm")) else {
        missing_provisioning(
            "node and npm are required to drive the transpiled component — install node 22 or \
             newer (which ships npm)",
        );
        return;
    };
    println!("component_jco: node {node}, npm {npm}");

    // Leg 3's ambient binary, preflighted SEPARATELY so its absence never touches legs 1 and 2.
    let wac = version_of("wac").filter(|banner| match major_minor(banner) {
        Some(found) if found >= WAC_FLOOR => true,
        found => {
            println!(
                "component_jco: the composed-artifact leg is SKIPPED — `wac` {} is below the \
                 {}.{} floor (`cargo install wac-cli`). The surface and cross-crate legs do not \
                 need it.",
                found.map_or_else(|| "(unparseable)".to_owned(), |(a, b)| format!("{a}.{b}")),
                WAC_FLOOR.0,
                WAC_FLOOR.1
            );
            false
        }
    });
    match &wac {
        Some(banner) => println!("component_jco: composing leg 3 with {banner}"),
        None => println!(
            "component_jco: the composed-artifact leg is SKIPPED — no usable `wac` on PATH \
             (`cargo install wac-cli`). It is the only ambient binary this gate wants; the other \
             two legs run without it."
        ),
    }

    let scratch_name = format!("cddl_codegen_component_jco_{:016x}", checkout_hash());
    let _scratch_lock = acquire_scratch_lock(&scratch_name); // serialize same-checkout runs
    let root = std::env::temp_dir().join(&scratch_name);
    // The root is deliberately NOT removed: `node_modules/` and `target/` under it are what keep a
    // warm re-run off the network and off a cold guest build.
    let target_dir = root.join("target");
    std::fs::create_dir_all(&target_dir).unwrap();

    let out = root.join("out");
    // A stale tree would poison the tree hash with files this run did not emit.
    let _ = std::fs::remove_dir_all(&out);
    std::fs::create_dir_all(&out).unwrap();

    // ---- generation -----------------------------------------------------------------------------
    // The surface leg: one crate, everything at its defaults, exactly as `component_host` generates
    // it. `--wasm=false` because the rust crate is the component crate's path dependency and nothing
    // else here — the wasm face would only add `__wbindgen_*` imports componentization cannot resolve.
    let surface_out = out.join(SURFACE_LIB);
    generate(
        "the surface fixture",
        &[
            "--input",
            SURFACE_INPUT,
            "--output",
            surface_out.to_str().unwrap(),
            "--component=true",
            "--wasm=false",
        ],
    );

    // The cross-crate legs: the DEPENDENCY first, because the consumer reads two of its committed
    // outputs (the extern-interface export and the WIT package). Mirrors `component_compose`.
    let dep_out = out.join(DEP_LIB);
    let consumer_out = out.join(CONSUMER_LIB);
    generate(
        "the dependency",
        &[
            "--input",
            &format!("{COMPOSE_FIXTURES}/dep/lib.cddl"),
            "--output",
            dep_out.to_str().unwrap(),
            "--wasm=false",
            "--component=true",
            "--lib-name",
            DEP_LIB,
        ],
    );
    generate(
        "the consumer",
        &[
            "--input",
            &format!("{COMPOSE_FIXTURES}/consumer/lib.cddl"),
            "--output",
            consumer_out.to_str().unwrap(),
            "--wasm=false",
            "--component=true",
            // ONE serialization runtime for both crates, so the dependency's types implement the
            // same `Deserialize`/`ToCBORBytes` the consumer's glue names across the seam.
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
        ],
    );

    // A workspace root so the six crates share one lock and one target dir. Real consumers own this
    // file; the tool never writes one.
    let members = [SURFACE_LIB, DEP_LIB, CONSUMER_LIB]
        .iter()
        .flat_map(|lib| [format!("\"{lib}/rust\""), format!("\"{lib}/component\"")])
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(
        out.join("Cargo.toml"),
        format!("[workspace]\nresolver = \"3\"\nmembers = [{members}]\n"),
    )
    .unwrap();
    // The rust crates' `cdylib` output exists for wasm-bindgen's `wasm32-unknown-unknown` target; the
    // guest consumes the rlib, and asking the wasip2 linker for a cdylib is not what this gate is
    // about. Same narrowing the other component build gates do, for the same reason.
    for lib in [SURFACE_LIB, DEP_LIB, CONSUMER_LIB] {
        let manifest = out.join(lib).join("rust/Cargo.toml");
        let narrowed = std::fs::read_to_string(&manifest).unwrap().replace(
            "crate-type = [\"cdylib\", \"rlib\"]",
            "crate-type = [\"rlib\"]",
        );
        std::fs::write(&manifest, narrowed).unwrap();
    }

    // The whole fixture tree, INSIDE the hashed root and before the key is taken — see this module's
    // header. `js/**` (drivers + package.json + lockfile) and `compose.wac` land together.
    copy_tree(Path::new(FIXTURES), &out);

    // ---- npm provisioning, OUTSIDE the cached closure --------------------------------------------
    // Outside on purpose: a registry failure is a provisioning outcome (loud skip / hard fail), not a
    // verdict about the face, and a closure that returned `false` for it would be indistinguishable
    // from a real failure at the assert below.
    let js_dir = out.join("js");
    if let Some(reason) = provision_node_modules(&root, &js_dir) {
        let _ = std::fs::remove_dir_all(&out);
        missing_provisioning(&reason);
        return;
    }

    let jco = root.join("node_modules/.bin/jco");
    let artifacts: Vec<PathBuf> = [SURFACE_LIB, DEP_LIB, CONSUMER_LIB]
        .iter()
        .map(|lib| {
            target_dir
                .join("wasm32-wasip2/debug")
                .join(format!("{}_component.wasm", lib.replace('-', "_")))
        })
        .collect();
    let transpiled = out.join("transpiled");

    let mut failure = None;
    let outcome = gate_cache::run_cached(
        "component_jco",
        "surface+crosscrate+composed",
        &out,
        &[SURFACE_LIB, DEP_LIB, CONSUMER_LIB]
            .iter()
            .flat_map(|lib| {
                [
                    PathBuf::from(format!("{lib}/rust/Cargo.toml")),
                    PathBuf::from(format!("{lib}/component/Cargo.toml")),
                ]
            })
            .collect::<Vec<_>>(),
        &[
            format!("verdict={VERDICT_MARKER}"),
            // WHICH legs a cell covers depends on the ambient composer, so it is part of the key:
            // installing `wac` must re-run rather than inherit a two-leg verdict.
            format!("wac={}", wac.as_deref().unwrap_or("absent")),
            "cargo".to_owned(),
            "build".to_owned(),
            "--target".to_owned(),
            "wasm32-wasip2".to_owned(),
            format!("-p={SURFACE_LIB}-component"),
            format!("-p={DEP_LIB}-component"),
            format!("-p={CONSUMER_LIB}-component"),
            "jco".to_owned(),
            "transpile".to_owned(),
            "cwd=out/js".to_owned(),
            "node".to_owned(),
            "--test".to_owned(),
        ],
        || {
            let build = tool_cmd("cargo")
                .args(["build", "--target", "wasm32-wasip2"])
                .args(
                    [SURFACE_LIB, DEP_LIB, CONSUMER_LIB]
                        .iter()
                        .flat_map(|lib| ["-p".to_owned(), format!("{lib}-component")]),
                )
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
                        format!("the three guest builds failed\n{stderr}")
                    },
                );
                return false;
            }
            // A build that produced no COMPONENT would make every assertion below run against the
            // wrong thing, and jco would fail with a message about the wrong layer: `wasm32-wasip2`
            // artifacts carry the component-model preamble (layer 1) where a core module carries 0.
            for artifact in &artifacts {
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

            // ---- transpile ------------------------------------------------------------------
            // No `--map` for the 14 `wasi:*` imports: jco rewrites them to
            // `@bytecodealliance/preview2-shim` BY DEFAULT, and a hand-written `wasi:*=…/*` map
            // breaks the output (the shim exposes one module per SUBSYSTEM with named exports, not
            // one per interface). The instruction to a consumer is "install the shim, map nothing".
            //
            // `--name` on every call: the transpiled module's basename otherwise derives from the
            // wasm FILENAME, and the JS drivers import it by a literal path.
            let transpile = |wasm: &Path, dir: &str, name: &str, map: Option<&str>| {
                let mut cmd = tool_cmd(jco.to_str().unwrap());
                cmd.arg("transpile")
                    .arg(wasm)
                    .arg("-o")
                    .arg(transpiled.join(dir))
                    .args(["--name", name]);
                if let Some(map) = map {
                    cmd.args(["--map", map]);
                }
                let done = cmd.current_dir(&out).output().unwrap();
                done.status.success().then_some(()).ok_or_else(|| {
                    format!(
                        "`jco transpile` rejected {}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                        wasm.display(),
                        String::from_utf8_lossy(&done.stdout),
                        String::from_utf8_lossy(&done.stderr)
                    )
                })
            };

            if let Err(e) = transpile(&artifacts[0], "host", "host", None) {
                failure = Some(e);
                return false;
            }
            if let Err(e) = transpile(&artifacts[1], DEP_LIB, DEP_LIB, None) {
                failure = Some(e);
                return false;
            }
            // The cross-crate wiring, and the whole point of leg 2. The `--map` target is RELATIVE
            // (resolved from the transpiled consumer's own directory), so no scratch path is ever
            // baked into a hashed byte — the same run-independence property the wasmtime gates get
            // from relative path deps.
            if let Err(e) = transpile(
                &artifacts[2],
                CONSUMER_LIB,
                CONSUMER_LIB,
                Some(&format!(
                    "cddl:{DEP_LIB}/types@0.1.0=../{DEP_LIB}/{DEP_LIB}.js#types"
                )),
            ) {
                failure = Some(e);
                return false;
            }

            // ---- leg 3's artifact, only when the ambient composer is usable ------------------
            let mut legs = vec!["surface.test.mjs", "crosscrate.test.mjs"];
            if wac.is_some() {
                let composed = out.join("composed.wasm");
                let compose = tool_cmd("wac")
                    .args(["compose", "--dep"])
                    .arg(format!("cddl:{DEP_LIB}={}", artifacts[1].display()))
                    .arg("--dep")
                    .arg(format!("cddl:{CONSUMER_LIB}={}", artifacts[2].display()))
                    .arg("-o")
                    .arg(&composed)
                    .arg(out.join("compose.wac"))
                    .current_dir(&out)
                    .output()
                    .unwrap();
                if !compose.status.success() {
                    failure = Some(format!(
                        "`wac compose` could not build the dual-export world the pin needs\n{}",
                        String::from_utf8_lossy(&compose.stderr)
                    ));
                    return false;
                }
                if let Err(e) = transpile(&composed, "composed", "composed", None) {
                    failure = Some(e);
                    return false;
                }
                legs.push("composed.test.mjs");
            }

            // ---- drive it -------------------------------------------------------------------
            // node's BUILT-IN runner: the only npm dependencies this gate has are jco and its shim,
            // so a test framework would be a third for no coverage. cwd is `out/js/`, from which node
            // walks up to `<root>/node_modules` for the shim the transpiled modules import.
            let run = tool_cmd("node")
                .arg("--test")
                .args(&legs)
                .current_dir(&js_dir)
                .output()
                .unwrap();
            if !run.status.success() {
                failure = Some(format!(
                    "the transpiled component does not behave as the JS face promises \
                     (legs: {})\n--- stdout ---\n{}\n--- stderr ---\n{}",
                    legs.join(", "),
                    String::from_utf8_lossy(&run.stdout),
                    String::from_utf8_lossy(&run.stderr)
                ));
                return false;
            }
            true
        },
    );
    if gate_cache::enabled() {
        println!(
            "component_jco gate-cache: {} run, {} cached",
            outcome.ran(),
            outcome.cached()
        );
    }
    let verdict = failure.is_none();
    let message = failure.unwrap_or_default();
    // The per-cell tree is freed; `node_modules/` and `target/` (siblings, not children) survive.
    let _ = std::fs::remove_dir_all(&out);
    assert!(verdict, "{message}");
}

/// Generate one crate, naming the leg in the failure so a spec change does not read as a JS failure.
fn generate(what: &str, args: &[&str]) {
    let generated = codegen_cmd().args(args).output().unwrap();
    assert!(
        generated.status.success(),
        "generating {what} failed\n{}",
        String::from_utf8_lossy(&generated.stderr)
    );
}

/// Put `node_modules/` at the scratch ROOT, from the committed lockfile. `Some(reason)` on a
/// provisioning failure.
///
/// `npm ci`, never `npm install`: `ci` REQUIRES the lockfile and installs exactly it, where `install`
/// would silently resolve a newer jco and answer a question about a version nobody committed.
///
/// The install is skipped when the root already carries the same lockfile bytes and a `jco` binary —
/// a scratch-state read of the same class as reusing `target/`, and the reason a warm run costs
/// nothing here. A lockfile edit changes those bytes, so the reinstall follows the bump.
fn provision_node_modules(root: &Path, js_dir: &Path) -> Option<String> {
    let lock_src = js_dir.join("package-lock.json");
    let lock_dest = root.join("package-lock.json");
    let lock = std::fs::read(&lock_src).unwrap();
    let fresh = std::fs::read(&lock_dest).is_ok_and(|prior| prior == lock)
        && root.join("node_modules/.bin/jco").is_file();
    if fresh {
        return None;
    }
    std::fs::write(&lock_dest, &lock).unwrap();
    std::fs::copy(js_dir.join("package.json"), root.join("package.json")).unwrap();

    let install = tool_cmd("npm")
        .args(["ci", "--no-audit", "--no-fund"])
        .current_dir(root)
        .output()
        .unwrap();
    if !install.status.success() {
        // A cold `npm ci` needs the registry. That is a machine/network condition, not a statement
        // about the component face, so it names itself rather than reading as a JS failure.
        let stderr = String::from_utf8_lossy(&install.stderr);
        return Some(format!(
            "`npm ci` could not install jco {JCO_VERSION} + preview2-shim {SHIM_VERSION} into \
             {} — a cold run needs the npm registry\n{stderr}",
            root.display()
        ));
    }
    None
}

/// The JS fixture is not compiled by anything in THIS crate's build, so a file added to it and never
/// copied would silently drop out of the gate's input closure. This is the cheap in-process check
/// that the tree the gate copies is the tree the drivers need — plus the two properties the
/// gate-cache key rests on, asserted against the source rather than trusted.
#[test]
fn the_jco_fixture_carries_the_files_the_gate_copies() {
    for expected in [
        "tests/component-jco/js/package.json",
        "tests/component-jco/js/package-lock.json",
        "tests/component-jco/js/surface.test.mjs",
        "tests/component-jco/js/crosscrate.test.mjs",
        "tests/component-jco/js/composed.test.mjs",
        "tests/component-jco/compose.wac",
        // REUSED, never duplicated — the specs the wasmtime gates already drive.
        "tests/component-host/inputs/lib.cddl",
        "tests/component-compose/dep/lib.cddl",
        "tests/component-compose/consumer/lib.cddl",
    ] {
        assert!(
            Path::new(expected).is_file(),
            "{expected} is missing — the JS-host gate would drive an incomplete fixture"
        );
    }

    // The two npm pins are what make this gate's verdict a statement about a KNOWN jco vintage, and
    // the docs quote them. An `npm install` that rewrote the lockfile would move the answer without
    // moving the claim, so the pin is asserted here and not merely committed.
    let manifest = std::fs::read_to_string("tests/component-jco/js/package.json").unwrap();
    let lock = std::fs::read_to_string("tests/component-jco/js/package-lock.json").unwrap();
    for (package, version) in [
        ("@bytecodealliance/jco", JCO_VERSION),
        ("@bytecodealliance/preview2-shim", SHIM_VERSION),
    ] {
        assert!(
            manifest.contains(&format!("\"{package}\": \"{version}\"")),
            "package.json must pin {package} at EXACTLY {version} (no `^`, no `~`) — a range lets \
             a background npm release change this gate's verdict with no change in the repo"
        );
        assert!(
            lock.contains(&format!("\"node_modules/{package}\"")),
            "the lockfile does not carry {package} — `npm ci` installs the lockfile, so a package \
             missing from it is a package the gate never gets"
        );
    }
    assert!(
        lock.contains(&format!("\"version\": \"{JCO_VERSION}\"")),
        "the lockfile no longer resolves jco to {JCO_VERSION}, which is the version \
         `component_differences.mdx` quotes as the probed one"
    );

    // Run-independence: the cross-crate wiring is a RELATIVE specifier in the transpiled consumer, so
    // no scratch path can reach the hashed bytes. The `--map` target is spelled by the gate; what the
    // driver spells is the import side of the same edge.
    let crosscrate = std::fs::read_to_string("tests/component-jco/js/crosscrate.test.mjs").unwrap();
    let surface = std::fs::read_to_string("tests/component-jco/js/surface.test.mjs").unwrap();
    let composed = std::fs::read_to_string("tests/component-jco/js/composed.test.mjs").unwrap();
    for (name, source) in [
        ("surface.test.mjs", &surface),
        ("crosscrate.test.mjs", &crosscrate),
        ("composed.test.mjs", &composed),
    ] {
        for line in source
            .lines()
            .filter(|l| l.trim_start().starts_with("import "))
        {
            assert!(
                !line.contains("\"/") && !line.contains("'/"),
                "{name} imports by an ABSOLUTE path (`{}`) — every module it reaches lives inside \
                 the hashed root, and an absolute one would make the gate-cache key unique to its \
                 run",
                line.trim()
            );
        }
    }

    // The three interface identifiers the drivers name literally. Only one of them is derived:
    // `--lib-name X` (this module) mints `cddl:X@0.1.0` (the emitted WIT), which the JS drivers and
    // `compose.wac` then spell by hand. A rename that moved only one would fail inside node with a
    // message about an undefined interface; this says it here instead.
    let wac = std::fs::read_to_string("tests/component-jco/compose.wac").unwrap();
    for (lib, driver, source) in [
        (SURFACE_LIB, "surface.test.mjs", &surface),
        (DEP_LIB, "crosscrate.test.mjs", &crosscrate),
        (CONSUMER_LIB, "crosscrate.test.mjs", &crosscrate),
    ] {
        let iface = format!("cddl:{lib}/types@0.1.0");
        assert!(
            source.contains(&iface),
            "{driver} must reach `{iface}` — the gate generates that crate with `--lib-name {lib}` \
             (or its default), which is what names its WIT package"
        );
    }
    for lib in [DEP_LIB, CONSUMER_LIB] {
        assert!(
            wac.contains(&format!("cddl:{lib}")),
            "compose.wac must name the `cddl:{lib}` package — the gate passes it as `--dep \
             cddl:{lib}=<artifact>`"
        );
        assert!(
            composed.contains(&format!("cddl:{lib}/types@0.1.0")),
            "composed.test.mjs must reach `cddl:{lib}/types@0.1.0` — the bare `types` alias \
             resolves to only the FIRST interface on a composed artifact, which is one of the \
             defects this leg pins"
        );
    }
}

/// The version-banner parser, which decides whether leg 3 runs at all. Pure, so it is checked here
/// rather than by installing three `wac` builds.
#[test]
fn a_version_banner_yields_its_major_minor() {
    assert_eq!(major_minor("wac-cli 0.9.0"), Some((0, 9)));
    assert_eq!(major_minor("wac 1.0"), Some((1, 0)));
    assert_eq!(major_minor("v22.21.1"), Some((22, 21)));
    assert_eq!(major_minor("nothing here"), None);
    // A banner whose leading token has no minor must not read as `(major, 0)` and pass a floor it
    // was never measured against.
    assert_eq!(major_minor("wac 9"), None);
    assert!(major_minor("wac-cli 0.9.0").unwrap() >= WAC_FLOOR);
    assert!(major_minor("wac-cli 0.8.9").unwrap() < WAC_FLOOR);
}
