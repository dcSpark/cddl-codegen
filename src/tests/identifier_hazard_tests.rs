//! Identifier-hazard sweep — a NAME-shaped enumeration no construct axis can catch.
//!
//! These are collisions between a user-chosen CDDL *name* and the Rust the generator *emits*, not
//! construct shapes: the axis IS the name, so `verify.ts`'s construct probes can never surface them.
//! Two prior instances motivated the sweep — a bareword map key that is a Rust keyword (`{ if: uint }`,
//! now rejected gracefully at parse time, pinned in `robustness_tests`) and single-letter rule names
//! colliding with the emitted reader/writer generics (`r`/`w` vs `R`/`W`, still open compile
//! failures — SHAPE-DEPENDENT, see `EXPECTED_COMPILE_FAIL`). This module sweeps a hazard list ×
//! name-position table so the whole keyword list (and the std/prelude type names) get verdicted
//! alongside the cases we already knew about, instead of each being rediscovered by hand.
//!
//! The rule-name position carries a type-SHAPE axis (record struct AND type-choice enum), because the
//! collision surface differs per emitted shape: a struct's serialize body never names its own type, but
//! an enum's serialize matches `Self::Variant` arms spelled `W::Variant`-adjacent — so a struct-only
//! sweep laundered enum-shaped `w` as "compiles clean" (found in review; the shape axis exists so the
//! sweep itself can't under-enumerate the very hazard class it was built for). Only the rule-name
//! position gets the axis: the other positions don't emit the hazard as a TYPE name, and a plain group
//! can't be a type-choice enum.
//!
//! Two layers, mirroring the robustness catalogs' split:
//!   1. `identifier_hazard_robustness_catalog` (default `cargo test`) — per-cell GENERATION outcome
//!      (`ok` / `error (graceful)` / `PANIC`) via `catch_unwind`, snapshotted. A NEW `PANIC` is a
//!      regression; the keyword-in-bareword cells pin the FIXED graceful rejection. (The fn name
//!      carries `robustness` so the `cargo insta test -- snapshot_tests robustness` orphan gate
//!      selects it and its snapshot isn't flagged unreferenced.)
//!   2. `identifier_hazard_crates_compile` (`#[ignore]`, full tier) — for the `ok` cells, the
//!      generated crate must pass a standalone `cargo check`. `ok` hazards of one position are bundled
//!      into ONE crate (rule names are distinct by construction) to avoid ~hundreds of cargo checks,
//!      minus a pinned `EXPECTED_COMPILE_FAIL` list asserted to fail INDIVIDUALLY so the pin flips
//!      loudly when the generic-collision fix lands.

use crate::cli::Cli;
use crate::parsing::RUST_KEYWORDS;
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

/// Hazards beyond `RUST_KEYWORDS` (reused from `parsing.rs`, never re-typed):
/// - `r` / `w` collide with the emitted reader/writer generics `R` / `W` (the open compile failure).
/// - std/prelude type names: camel-cased they shadow `Option` / `String` / `Vec` / … in the generated
///   module, so a rule/group named `option` emits `pub …Option…` that shadows the prelude the emitted
///   code itself uses.
///
/// The list is `["r", "w"]` (emitted-generic collisions) followed by the std/prelude type names.
/// `box` overlaps `RUST_KEYWORDS`; the dedup in `hazards()` keeps the first (keyword) occurrence.
const EXTRA_HAZARDS: &[&str] = &[
    "r", "w", // emitted reader/writer generics
    "option", "some", "none", "result", "ok", "err", "vec", "string", "box", "int", "error",
];

/// The full hazard list: `RUST_KEYWORDS` in its authored order, then `EXTRA_HAZARDS`, skipping any
/// already seen (`box`). Deterministic order → stable snapshot. Nothing is filtered down to what is
/// convenient: a keyword the `cddl` parser itself rejects records that rejection as its verdict.
fn hazards() -> Vec<&'static str> {
    let mut seen = std::collections::BTreeSet::new();
    let mut out = Vec::new();
    for &h in RUST_KEYWORDS.iter().chain(EXTRA_HAZARDS.iter()) {
        if seen.insert(h) {
            out.push(h);
        }
    }
    out
}

/// A name position — a template that lands `hazard` in emitted Rust. `i` disambiguates the wrapper
/// rule name so a whole position's `ok` cells can be concatenated into one bundle crate (§ layer 2)
/// with distinct rule names; the same `build(hazard, i)` is used for the single-cell layer-1 pass so
/// the two layers can never test different specs.
struct Position {
    name: &'static str,
    build: fn(&str, usize) -> String,
}

/// The rule name of a composite rule, RECORD-STRUCT shape → the emitted struct name is the
/// camel-cased hazard. A struct's serialize body never names its own type, so this shape alone
/// misses serialize-side collisions — the enum shape below covers those.
fn build_rule_name(h: &str, _i: usize) -> String {
    format!("{h} = [a: uint]\n")
}
/// The rule name of a type-choice rule, ENUM shape → the emitted `pub enum` name is the camel-cased
/// hazard, and BOTH serialize and deserialize bodies name it (`Self::`-equivalent `<Name>::Variant`
/// paths inside `fn serialize<'se, W: Write>` / `fn deserialize<R: BufRead + Seek>`), so a name
/// camel-casing to `W` or `R` resolves to the fn's type parameter instead (E0599). This shape is why
/// the axis exists: the struct template alone verdicted `w` as compiling.
fn build_rule_name_enum(h: &str, _i: usize) -> String {
    format!("{h} = uint / tstr\n")
}
/// A bareword map key → the emitted (snake_cased) struct FIELD identifier is the hazard.
fn build_map_key(h: &str, i: usize) -> String {
    format!("holder{i} = {{ {h}: uint }}\n")
}
/// A bareword array key → same field-identifier landing as the map key, different rep.
fn build_array_key(h: &str, i: usize) -> String {
    format!("holder{i} = [{h}: uint]\n")
}
/// A plain group name referenced from another rule → the group registers as a struct whose name is
/// the camel-cased hazard (the sole-use plain-group registration path).
fn build_group_name(h: &str, i: usize) -> String {
    format!("{h} = (a: uint, b: uint)\nholder{i} = [{h}]\n")
}
/// The `@name` directive value on a keyed field → renames the emitted field to the hazard (the CBOR
/// wire key stays the integer `0`). The comment must sit AFTER the comma per the DSL.
fn build_name_directive(h: &str, i: usize) -> String {
    format!("holder{i} = {{ 0: uint, ; @name {h}\n}}\n")
}

const POSITIONS: &[Position] = &[
    Position {
        name: "rule-name",
        build: build_rule_name,
    },
    Position {
        name: "rule-name-enum",
        build: build_rule_name_enum,
    },
    Position {
        name: "map-key",
        build: build_map_key,
    },
    Position {
        name: "array-key",
        build: build_array_key,
    },
    Position {
        name: "group-name",
        build: build_group_name,
    },
    Position {
        name: "name-directive",
        build: build_name_directive,
    },
];

/// Generate a standalone crate's source map for `spec` (writes to a unique temp `.cddl`).
/// `--wasm=false` EXPLICITLY (the CLI default is true): both layers must classify under the same
/// flags layer 2's `cargo check` crate is built with (`--wasm=false`, the matrix-probe idiom the
/// sibling robustness catalogs also pin) — otherwise a cell that panics only on the wasm emission
/// path would silently drop out of the compile bundle. A wasm-side hazard sweep is a separate axis.
fn generate(spec: &str, tag: &str) -> Result<std::collections::BTreeMap<String, String>, String> {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_idhazard_{}_{}.cddl",
        tag,
        std::process::id()
    ));
    std::fs::write(&path, spec).unwrap();
    let cli = Cli::parse_from([
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "identifier_hazard_unused",
        "--wasm",
        "false",
    ]);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// LAYER 1 — the generation-outcome catalog (default `cargo test`). A snapshot of `ok` /
/// `error (graceful)` / `PANIC` per (position × hazard) cell. A NEW `PANIC` is a regression: the
/// generator must reject a hazardous name with a clean error, never `panic!`/`assert!`. The
/// keyword-in-bareword cells (map-key / array-key / name-directive positions) pin the FIXED graceful
/// rejection; the rule-name/group-name cells that GENERATE (even ones that won't *compile*) record
/// `ok` here — the compile verdict is layer 2's job, invisible to this generate-only pass.
#[test]
fn identifier_hazard_robustness_catalog() {
    let hz = hazards();
    assert!(!hz.is_empty(), "hazard list is empty");

    let mut catalog = String::from(
        "# identifier-hazard sweep: generation outcome per (name-position × hazardous identifier) — a\n\
         # SCORECARD, not a contract. A committed PANIC is a TRACKED-KNOWN gap (today: rule/group names\n\
         # whose camel-cased form is a reserved Rust type name or a CDDL keyword hit the `assert!` guards\n\
         # in `RustIdent::new`, intermediate.rs:1146/1152 — a deliberate reservation, but via panic\n\
         # rather than a graceful `record_rejection`; candidate fix, not blessed-correct). A NEW panic —\n\
         # a cell FLIPPING to PANIC, or a panic decaying to a silently-wrong `ok` — is a regression:\n\
         # hazardous names must reject gracefully, never `panic!`/`assert!`. `ok` is generate-only — a\n\
         # rule/group name that generates but does NOT compile (the r/w generic collision) still records\n\
         # `ok`; the compile verdict is the `identifier_hazard_crates_compile` gate (full tier).\n\
         # Source: src/tests/identifier_hazard_tests.rs.\n\n",
    );
    with_thread_silenced_panics(|| {
        for pos in POSITIONS {
            for (i, h) in hz.iter().enumerate() {
                let spec = (pos.build)(h, i);
                let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    generate(&spec, &format!("cat_{}_{i}", pos.name))
                }));
                let label = match outcome {
                    Ok(Ok(_)) => "ok",
                    Ok(Err(_)) => "error (graceful)",
                    Err(_) => "PANIC",
                };
                catalog.push_str(&format!("{:15} {:10} {label}\n", pos.name, h));
            }
        }
    });

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/identifier_hazard/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("catalog", catalog));
}

// ---- layer 2: the standalone compile gate ---------------------------------------------------------

/// Spawn cargo for a *generated* crate, insulated from the workspace's `-D warnings` (generated code
/// legitimately over-imports). Local copy of `integration_tests::tool_cmd` to keep that module's
/// visibility unchanged.
fn tool_cmd(program: &str) -> std::process::Command {
    let mut c = std::process::Command::new(program);
    c.env_remove("RUSTFLAGS");
    c
}

/// Per-checkout scratch discriminator (concurrent runs from different checkouts must not share a path).
fn checkout_hash() -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::env::current_dir().unwrap().hash(&mut h);
    h.finish()
}

/// Cells that GENERATE but whose crate does NOT `cargo check` today — asserted to fail INDIVIDUALLY so
/// the pin is honest and flips loudly when a fix lands. Each entry is `(position, hazard)` with a
/// reason. This is the sweep's whole payoff: the collision failures we already knew about, plus any the
/// sweep turned up — NOT a license to fix the generator here.
///
/// The `r`/`w` vs `R`/`W` generic collision is SHAPE-DEPENDENT (the sweep's shape axis exists exactly
/// because a struct-only pass laundered `w` as clean — found in review):
/// - `r` fails on BOTH shapes: struct-shaped, the deserialize body's `R::from(..)`-style type position
///   hits E0574 ("expected struct, found type parameter `R`"); enum-shaped, the `R::U64`/`R::Text`
///   variant paths hit E0599 (no associated item on the type parameter).
/// - `w` fails on the ENUM shape only (E0599: `W::U64`/`W::Text` inside `fn serialize<'se, W: Write>`
///   resolve to the type param, the ROADMAP finding's original "pub enum W" citation). Struct-shaped
///   `w` genuinely compiles — a struct's serialize body never names its own type — so it rides in the
///   rule-name bundle as a documented shape boundary, NOT a narrowing of the finding.
///
/// The group-name occupant registers as a struct, so it mirrors the struct-shape verdicts (`r` fails,
/// `w` compiles). The candidate fix (collision-proof generic names, ROADMAP § 1 / Item C) turns all
/// four pins green and trips the `resurfaced` guard below.
const EXPECTED_COMPILE_FAIL: &[(&str, &str, &str)] = &[
    (
        "rule-name",
        "r",
        "camel-cased `R` collides with the deserializer reader generic `R` (E0574)",
    ),
    (
        "rule-name-enum",
        "r",
        "enum variant paths `R::U64`/`R::Text` resolve to the reader type parameter `R` (E0599)",
    ),
    (
        "rule-name-enum",
        "w",
        "enum variant paths `W::U64`/`W::Text` resolve to the writer type parameter `W` inside \
         `fn serialize<'se, W: Write>` (E0599)",
    ),
    (
        "group-name",
        "r",
        "plain group `r` registers as struct `R`, colliding with the reader generic `R` (E0574)",
    ),
];

/// Generate a crate for `spec` into `out` and `cargo check` its rust crate against `target_dir`.
/// Returns `Ok(())` if generation AND check both succeed, else `Err(reason)`.
fn gen_and_check(
    spec: &str,
    out: &std::path::Path,
    target_dir: &std::path::Path,
) -> Result<(), String> {
    let spec_path = out.with_extension("cddl");
    std::fs::create_dir_all(out.parent().unwrap()).ok();
    std::fs::write(&spec_path, spec).map_err(|e| e.to_string())?;
    let gen_out = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", spec_path.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .arg("--wasm=false")
        .output()
        .unwrap();
    if !gen_out.status.success() {
        return Err(format!(
            "generation failed\n{}",
            String::from_utf8_lossy(&gen_out.stderr)
        ));
    }
    let crate_dir = out.join("rust");
    if !crate_dir.exists() {
        return Err(format!("no rust crate at {crate_dir:?}"));
    }
    let check = tool_cmd("cargo")
        .arg("check")
        .current_dir(&crate_dir)
        .env("CARGO_TARGET_DIR", target_dir)
        .output()
        .unwrap();
    if check.status.success() {
        Ok(())
    } else {
        Err(format!(
            "cargo check failed\n{}\n{}",
            String::from_utf8_lossy(&check.stdout),
            String::from_utf8_lossy(&check.stderr)
        ))
    }
}

/// LAYER 2 — MANUAL/LOCAL ONLY (`#[ignore]`d, full tier via check.ts): the standalone compile gate.
/// For every position, split the hazards whose single-cell generation is `ok` into (a) the pinned
/// `EXPECTED_COMPILE_FAIL` cells — each generated as its own crate and asserted to FAIL `cargo check`
/// — and (b) the rest, bundled into ONE crate per position (distinct rule names by construction) that
/// must `cargo check` clean. A pinned cell that starts compiling trips `resurfaced` (a fix landed —
/// re-pin); a non-pinned bundle failing to compile is a NEW hazard finding (report it, add a pin — do
/// NOT fix the generator here). Shared `CARGO_TARGET_DIR` so deps build once (the
/// `feature_corpus_compiles` pattern).
///
/// Run: `cargo test --bin cddl-codegen identifier_hazard_crates_compile -- --ignored`.
#[test]
#[ignore]
fn identifier_hazard_crates_compile() {
    let hz = hazards();
    let root = std::env::temp_dir().join(format!("cddl_codegen_idhazard_{:016x}", checkout_hash()));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut failures: Vec<String> = Vec::new(); // non-pinned red cells — real findings
    let mut resurfaced: Vec<String> = Vec::new(); // pinned cells that now compile — a fix landed

    // Only cells that GENERATE (`ok`) have a crate to compile — some cells PANIC generation (the
    // reserved-name asserts), so detect ok-ness under `catch_unwind` + a silenced hook (in-process,
    // matching layer 1's classification) BEFORE any cargo work. `PANIC`/`error` cells are excluded.
    let ok_by_position: Vec<Vec<(usize, &'static str)>> = with_thread_silenced_panics(|| {
        POSITIONS
            .iter()
            .map(|pos| {
                hz.iter()
                    .enumerate()
                    .map(|(i, h)| (i, *h))
                    .filter(|(i, h)| {
                        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            generate(&(pos.build)(h, *i), &format!("gen_{}_{i}", pos.name)).is_ok()
                        }))
                        .unwrap_or(false)
                    })
                    .collect()
            })
            .collect()
    });

    for (pos, ok_hazards) in POSITIONS.iter().zip(ok_by_position.iter()) {
        let is_pinned = |h: &str| {
            EXPECTED_COMPILE_FAIL
                .iter()
                .find(|(p, ph, _)| *p == pos.name && *ph == h)
                .map(|(_, _, reason)| *reason)
        };

        // (a) pinned expected-fail cells for this position — generate + check EACH individually.
        for (i, h) in ok_hazards {
            if let Some(reason) = is_pinned(h) {
                let out = root.join(format!("{}__pin__{i}", pos.name));
                match gen_and_check(&(pos.build)(h, *i), &out, &target_dir) {
                    Err(_) => {} // red as pinned — good
                    Ok(()) => resurfaced.push(format!(
                        "{}/{h}: now compiles (pinned as: {reason}) — remove it from EXPECTED_COMPILE_FAIL",
                        pos.name
                    )),
                }
            }
        }

        // (b) the rest → one bundle crate that must compile clean.
        let bundle_specs: Vec<String> = ok_hazards
            .iter()
            .filter(|(_, h)| is_pinned(h).is_none())
            .map(|(i, h)| (pos.build)(h, *i))
            .collect();
        if bundle_specs.is_empty() {
            continue;
        }
        let bundle = bundle_specs.join("");
        let out = root.join(format!("{}__bundle", pos.name));
        if let Err(reason) = gen_and_check(&bundle, &out, &target_dir) {
            failures.push(format!(
                "{} bundle failed to compile (a NON-pinned hazard broke the crate — a NEW finding; \
                 bisect the position's hazards, add the culprit to EXPECTED_COMPILE_FAIL with a \
                 reason, and report it — do NOT fix the generator here):\n{reason}",
                pos.name
            ));
        }
    }

    let _ = std::fs::remove_dir_all(&root);
    assert!(
        resurfaced.is_empty(),
        "pinned identifier-hazard cells now compile (a fix landed — re-pin):\n{}",
        resurfaced.join("\n")
    );
    assert!(
        failures.is_empty(),
        "identifier-hazard crates failed to compile:\n\n{}",
        failures.join("\n\n")
    );
}
