//! Identifier-hazard sweep — a NAME-shaped enumeration no construct axis can catch.
//!
//! These are collisions between a user-chosen CDDL *name* and the Rust the generator *emits*, not
//! construct shapes: the axis IS the name, so `verify.ts`'s construct probes can never surface them.
//! Two prior instances motivated the sweep — a bareword map key that is a Rust keyword (`{ if: uint }`,
//! now rejected gracefully at parse time, pinned in `robustness_tests`) and single-letter rule names
//! colliding with the reader/writer generics the pre-cbor_event-3.x emission carried (`r`/`w` vs
//! `R`/`W`; the 3.x de-generified `Serializer`/`Deserializer` removed those fn generics, and the
//! collision class with them — see the empty `EXPECTED_COMPILE_FAIL`). This module sweeps a hazard list ×
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
//!      minus a pinned `EXPECTED_COMPILE_FAIL` list (currently empty — the generic-collision fix
//!      landed) asserted to fail INDIVIDUALLY so the pin flips loudly if a NEW does-not-compile
//!      hazard is added.

use crate::cli::Cli;
use crate::intermediate::Representation;
use crate::parsing::{
    GENERATED_LOCAL_PROBED_SAFE, GENERATED_LOCAL_RESERVED, RUST_KEYWORDS, ReservedScope,
};
// `tool_cmd` insulates a nested *generated-crate* cargo build from the workspace's `-D warnings`
// (generated code legitimately over-imports traits/globs — see its doc comment); `codegen_cmd`
// spawns the generator binary directly, so no generation call takes the repo `target/` build lock.
use crate::tests::integration_tests::{codegen_cmd, tool_cmd};
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

/// Hazards beyond `RUST_KEYWORDS` (reused from `parsing.rs`, never re-typed):
/// - `r` / `w` camel-case to `R` / `W` — single-letter type names that collided with the emitted
///   reader/writer fn generics before cbor_event 3.x de-generified `Serializer`/`Deserializer`.
///   The generics are gone, but the cells stay swept: they cost nothing and would catch any future
///   emission that reintroduces a single-letter identifier.
/// - std/prelude type names: camel-cased they shadow `Option` / `String` / `Vec` / … in the generated
///   module, so a rule/group named `option` emits `pub …Option…` that shadows the prelude the emitted
///   code itself uses.
///
/// `box` overlaps `RUST_KEYWORDS`; the dedup in `hazards()` keeps the first (keyword) occurrence.
const EXTRA_HAZARDS: &[&str] = &[
    "r", "w", // historical reader/writer-generic collisions (see above)
    "option", "some", "none", "result", "ok", "err", "vec", "string", "box", "int", "error",
];

/// The full hazard list: `RUST_KEYWORDS` in its authored order, then `EXTRA_HAZARDS`, skipping any
/// already seen (`box`). Deterministic order → stable snapshot. Nothing is filtered down to what is
/// convenient: a keyword the `cddl` parser itself rejects records that rejection as its verdict.
/// `pub(crate)` so the recombination fuzzer (`recombination_tests`) draws its low-weight
/// hazardous-identifier axis from this table instead of rediscovering it.
pub(crate) fn hazards() -> Vec<&'static str> {
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
/// paths inside the emitted `serialize`/`deserialize` fns). Under the pre-cbor_event-3.x generic
/// signatures a name camel-casing to `W`/`R` resolved to the fn's type parameter instead (E0599) —
/// this shape is why the axis exists: the struct template alone verdicted `w` as compiling.
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

/// Concatenate every generated file's source for `spec` (or panic with the generation error) — a
/// coarse string surface for the fast-tier collision-proofing assertions below.
fn generated_source(spec: &str, tag: &str) -> String {
    generate(spec, tag)
        .unwrap_or_else(|e| panic!("generation failed for {tag}: {e}"))
        .into_values()
        .collect::<Vec<_>>()
        .join("\n")
}

/// FAST-TIER guard: the emitted `serialize`/`deserialize` fns carry NO reader/writer type
/// parameters (cbor_event 3.x's `Serializer`/`Deserializer` are concrete), so a rule camel-casing
/// to `R`/`W` defines an ordinary type with nothing to shadow. Pin the absence — a reintroduced fn
/// generic would silently resurrect the whole shape-dependent collision class this sweep launched
/// with. String check so it runs in the default `cargo test` tier; the standalone `cargo check`
/// proof rides the `#[ignore]` compile gate's `r`/`w` bundle cells.
#[test]
fn emitted_signatures_carry_no_reader_writer_generics() {
    for (spec, tag) in [
        ("r = uint / tstr\n", "unit_r_enum"),
        ("w = uint / tstr\n", "unit_w_enum"),
        (
            "foo = uint / tstr\nholder = [a: uint]\n",
            "unit_no_collision",
        ),
    ] {
        let src = generated_source(spec, tag);
        assert!(
            !src.contains(": BufRead") && !src.contains(": Write"),
            "{tag}: emitted signatures grew a reader/writer generic bound again — that reintroduces \
             the `r`/`w` ident-collision class (E0574/E0599):\n{src}"
        );
    }
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
         # SCORECARD, not a contract. A NEW panic — a cell FLIPPING to PANIC, or a panic decaying to a\n\
         # silently-wrong `ok` — is a regression: hazardous names must reject gracefully, never\n\
         # `panic!`/`assert!`. Reserved-name rule/group definitions (a name camel-casing to a reserved\n\
         # Rust std/prelude type, or a CDDL keyword) reject gracefully via a pre-scan in\n\
         # `api::with_types` (`intermediate::reserved_ident_rejection`); the `RustIdent::new` asserts\n\
         # remain a backstop for synthesized idents. The one committed PANIC left — `group-name int` —\n\
         # is a DISTINCT collision: a plain group named `int` clashes with the project's pre-registered\n\
         # extern `int` struct and trips the plain-group representation assert, not a reserved-name\n\
         # guard (`int` is special-cased out of the reserved check, so it is not pre-scanned). `ok` is\n\
         # generate-only — a rule/group name that generates but does NOT compile (the historical r/w\n\
         # generic collision, now fixed) still records `ok`; the compile verdict is the\n\
         # `identifier_hazard_crates_compile` gate (full tier). Source: src/tests/identifier_hazard_tests.rs.\n\n",
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

/// Per-checkout scratch discriminator (concurrent runs from different checkouts must not share a path).
fn checkout_hash() -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::env::current_dir().unwrap().hash(&mut h);
    h.finish()
}

/// Cells that GENERATE but whose crate does NOT `cargo check` — asserted to fail INDIVIDUALLY so the
/// pin is honest and flips loudly when a fix lands. Each entry is `(position, hazard, reason)`. This
/// is the sweep's whole payoff: a red cell the bundle would otherwise launder, held explicit — NOT a
/// license to fix the generator here.
///
/// EMPTY: the `r`/`w` reader/writer-generic collisions that formerly lived here are GONE — the
/// emitted `serialize`/`deserialize` fns carry no type parameters since cbor_event 3.x
/// (`emitted_signatures_carry_no_reader_writer_generics` pins the absence), so all four cells
/// `cargo check` clean and ride the position bundles below. The pinning machinery stays wired: a
/// NEW hazard that generates but does not compile gets an entry here, asserted red individually,
/// and the `resurfaced` guard flips loudly the day it's fixed.
const EXPECTED_COMPILE_FAIL: &[(&str, &str, &str)] = &[];

/// Generate a crate for `spec` into `out` and `cargo check` its rust crate against `target_dir`.
/// Returns `Ok(())` if generation AND check both succeed, else `Err(reason)`.
fn gen_and_check(
    spec: &str,
    out: &std::path::Path,
    target_dir: &std::path::Path,
) -> Result<(), String> {
    gen_and_check_with(spec, out, target_dir, &["--wasm=false".to_owned()])
}

/// `gen_and_check` with the generator flags spelled explicitly — the profile axis the
/// out-of-scope compile gate needs.
fn gen_and_check_with(
    spec: &str,
    out: &std::path::Path,
    target_dir: &std::path::Path,
    flags: &[String],
) -> Result<(), String> {
    let spec_path = out.with_extension("cddl");
    std::fs::create_dir_all(out.parent().unwrap()).ok();
    std::fs::write(&spec_path, spec).map_err(|e| e.to_string())?;
    let gen_out = codegen_cmd()
        .arg(format!("--input={}", spec_path.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .args(flags)
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
    for (position, hazard, _) in EXPECTED_COMPILE_FAIL {
        assert!(
            POSITIONS.iter().any(|pos| pos.name == *position),
            "EXPECTED_COMPILE_FAIL names position `{position}` that is no longer swept — stale pin, \
             remove or fix it"
        );
        assert!(
            hz.iter().any(|h| h == hazard),
            "EXPECTED_COMPILE_FAIL names hazard `{hazard}` that is no longer swept — stale pin, \
             remove or fix it"
        );
    }

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

// ---- the generated-local vocabulary: LOCKSTEP source scan + hazard sweep ------------------------

/// The emitter sources whose string literals become generated fn BODIES (where a record's field
/// locals are in scope). `export.rs` and `component.rs` are deliberately absent: their emitted
/// bodies are the json-schema generator and the wit-bindgen guest glue, neither of which puts a
/// user field name in scope beside a fixed local.
pub(crate) const EMITTER_SOURCES: &[&str] = &[
    "deserialize.rs",
    "serialize.rs",
    "records.rs",
    "enums.rs",
    "collections.rs",
    "wrappers.rs",
];

/// One lexical pass over Rust source, feeding both source-scan gates over the emitters.
pub(crate) struct ScannedRust {
    /// `src` with every comment body, char literal and string literal body blanked to spaces
    /// (newlines kept, so char indices and line numbers still line up with `src`). This is what
    /// lets a caller match `fn`/`impl` headers and brace extents without a real parser: a `{` or a
    /// `fn` inside an EMITTED literal cannot be mistaken for the emitter's own code.
    pub(crate) masked: String,
    /// `(char index of the literal's first content char in `src`, decoded content)` for every
    /// string literal, normal and raw.
    pub(crate) literals: Vec<(usize, String)>,
}

/// Scan `src` for [`ScannedRust`]: string literal contents (normal and raw) plus the masked source,
/// skipping line and block comments and char literals so a quote inside a comment cannot swallow
/// real code. Deliberately simple: it only has to be right about [`EMITTER_SOURCES`], and two gates
/// pin that it still finds what we know is there — `emitter_local_scan_finds_the_known_anchors`
/// (here) and `snapshot_tests::emitter_overload_lint_sees_its_anchors` (the fast-tier bare-token
/// lint, this scan's other consumer).
pub(crate) fn scan_rust(src: &str) -> ScannedRust {
    let b: Vec<char> = src.chars().collect();
    let mut masked = b.clone();
    /// Blank `[from, to)` in the mask, keeping newlines so indices/lines still align with `src`.
    fn blank(mask: &mut [char], from: usize, to: usize) {
        for c in mask.iter_mut().take(to).skip(from) {
            if *c != '\n' {
                *c = ' ';
            }
        }
    }
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
        let start = i;
        match b[i] {
            '/' if i + 1 < b.len() && b[i + 1] == '/' => {
                while i < b.len() && b[i] != '\n' {
                    i += 1;
                }
                blank(&mut masked, start, i);
            }
            '/' if i + 1 < b.len() && b[i + 1] == '*' => {
                let mut depth = 1;
                i += 2;
                while i + 1 < b.len() && depth > 0 {
                    if b[i] == '/' && b[i + 1] == '*' {
                        depth += 1;
                        i += 2;
                    } else if b[i] == '*' && b[i + 1] == '/' {
                        depth -= 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                blank(&mut masked, start, i);
            }
            // a char literal (`'x'`, `'\n'`) — a lifetime (`'a`) has no closing quote, so only
            // consume when the closing quote is where a char literal would put it.
            '\'' => {
                let close = if i + 2 < b.len() && b[i + 1] == '\\' {
                    (i + 3..b.len().min(i + 8)).find(|&j| b[j] == '\'')
                } else if i + 2 < b.len() && b[i + 2] == '\'' {
                    Some(i + 2)
                } else {
                    None
                };
                i = close.map(|j| j + 1).unwrap_or(i + 1);
                if close.is_some() {
                    blank(&mut masked, start, i);
                }
            }
            'r' if i + 1 < b.len() && (b[i + 1] == '"' || b[i + 1] == '#') => {
                let mut hashes = 0;
                let mut j = i + 1;
                while j < b.len() && b[j] == '#' {
                    hashes += 1;
                    j += 1;
                }
                if j >= b.len() || b[j] != '"' {
                    i += 1;
                    continue;
                }
                let terminator: String = std::iter::once('"')
                    .chain(std::iter::repeat_n('#', hashes))
                    .collect();
                let rest: String = b[j + 1..].iter().collect();
                match rest.find(&terminator) {
                    Some(end) => {
                        out.push((j + 1, rest[..end].to_owned()));
                        i = j + 1 + rest[..end + terminator.len()].chars().count();
                    }
                    None => i = b.len(),
                }
                blank(&mut masked, start, i);
            }
            '"' => {
                let mut lit = String::new();
                i += 1;
                let content_start = i;
                while i < b.len() && b[i] != '"' {
                    if b[i] == '\\' {
                        i += 1;
                        if i < b.len() {
                            // keep the escaped char so `\n`-joined emitted lines still tokenize
                            lit.push(if b[i] == 'n' { '\n' } else { b[i] });
                        }
                    } else {
                        lit.push(b[i]);
                    }
                    i += 1;
                }
                out.push((content_start, lit));
                i += 1;
                blank(&mut masked, start, i);
            }
            _ => i += 1,
        }
    }
    ScannedRust {
        masked: masked.into_iter().collect(),
        literals: out,
    }
}

pub(crate) fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Read the identifier starting at `chars[at]`, or `None` if that is not an identifier start.
pub(crate) fn ident_at(chars: &[char], at: usize) -> Option<String> {
    let first = *chars.get(at)?;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    let mut end = at;
    while end < chars.len() && is_ident_char(chars[end]) {
        end += 1;
    }
    Some(chars[at..end].iter().collect())
}

fn skip_spaces(chars: &[char], mut at: usize) -> usize {
    while at < chars.len() && chars[at] == ' ' {
        at += 1;
    }
    at
}

/// Every FIXED name the emitted code binds in a literal: `let`/`let mut` bindings, the ident
/// destructured by an `if let Path(ident)`, `for <ident> in`, closure parameters, and a
/// `<ident>: &mut Deserializer/Serializer` parameter. Interpolated names (`let mut {}`) and
/// wildcards yield nothing — a `{` or `_` is not an identifier start here.
fn emitted_bindings(lit: &str) -> Vec<String> {
    let chars: Vec<char> = lit.chars().collect();
    let mut out = Vec::new();
    let starts_word = |at: usize| at == 0 || !is_ident_char(chars[at - 1]);
    for i in 0..chars.len() {
        if !starts_word(i) {
            continue;
        }
        let word = match ident_at(&chars, i) {
            Some(w) => w,
            None => continue,
        };
        let after = i + word.chars().count();
        match word.as_str() {
            "let" | "for" => {
                let mut at = skip_spaces(&chars, after);
                if word == "let" && ident_at(&chars, at).as_deref() == Some("mut") {
                    at = skip_spaces(&chars, at + 3);
                }
                if let Some(bound) = ident_at(&chars, at) {
                    let bound_end = at + bound.chars().count();
                    if bound.starts_with(|c: char| c.is_ascii_uppercase()) {
                        // a pattern (`let Some(x)`, `if let TagPresenceEncoding::Tagged(sz)`) —
                        // the BINDING is inside the parens, so take that instead.
                        let paren = (bound_end..chars.len().min(bound_end + 40))
                            .find(|&j| chars[j] == '(' || chars[j] == '=' || chars[j] == '\n');
                        if let Some(p) = paren
                            && chars[p] == '('
                            && let Some(inner) = ident_at(&chars, p + 1)
                            && !inner.starts_with(|c: char| c.is_ascii_uppercase())
                        {
                            out.push(inner);
                        }
                    } else if bound != "_" {
                        // require the punctuation a BINDING is followed by, so English prose in an
                        // emitted doc comment ("… for embedding into enums", "… let the caller …")
                        // does not read as one.
                        let next = chars.get(skip_spaces(&chars, bound_end)).copied();
                        let binds = match word.as_str() {
                            "for" => {
                                ident_at(&chars, skip_spaces(&chars, bound_end)).as_deref()
                                    == Some("in")
                            }
                            _ => matches!(next, Some('=') | Some(':') | Some(';')),
                        };
                        if binds {
                            out.push(bound);
                        }
                    }
                }
            }
            _ => {}
        }
        // a `<ident>: &mut Deserializer` / `Serializer` parameter
        if chars[i..].starts_with(&[':']) {
            continue;
        }
        let rest: String = chars[after..chars.len().min(after + 20)].iter().collect();
        if rest.starts_with(": &mut Deserializer") || rest.starts_with(": &mut Serializer") {
            out.push(word);
        }
    }
    // closure parameters: `|x|` / `|x: T|`. `||` (logical or, and the empty param list) has no
    // identifier after the bar, so it never matches.
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '|'
            && (i == 0 || chars[i - 1] != '|')
            && let Some(param) = ident_at(&chars, i + 1)
        {
            let end = i + 1 + param.chars().count();
            let closes = (end..chars.len().min(end + 60)).find(|&j| chars[j] == '|');
            let breaks = (end..chars.len().min(end + 60)).find(|&j| chars[j] == '\n');
            if closes.is_some() && (breaks.is_none() || breaks > closes) && param != "_" {
                out.push(param);
            }
        }
        i += 1;
    }
    out
}

/// The full emitter-local vocabulary: the bindings above, plus the `codegen`-builder fn parameter
/// names (`.arg("serializer", …)`), which are spelled in Rust source rather than in a literal.
fn scan_emitter_locals() -> std::collections::BTreeMap<String, std::collections::BTreeSet<String>> {
    let mut found: std::collections::BTreeMap<String, std::collections::BTreeSet<String>> =
        Default::default();
    for file in EMITTER_SOURCES {
        let path = format!("{}/src/generation/{file}", env!("CARGO_MANIFEST_DIR"));
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("cannot read emitter source {path}: {e}"));
        for (_, lit) in scan_rust(&src).literals {
            for name in emitted_bindings(&lit) {
                found.entry(name).or_default().insert((*file).to_owned());
            }
        }
        let chars: Vec<char> = src.chars().collect();
        for (i, _) in src.match_indices(".arg(\"") {
            let at = src[..i].chars().count() + ".arg(\"".chars().count();
            if let Some(name) = ident_at(&chars, at) {
                found
                    .entry(name)
                    .or_default()
                    .insert(format!("{file} (.arg)"));
            }
        }
    }
    // `mut` survives `let mut {}` (an INTERPOLATED name, not a fixed one) — the only systematic
    // false positive the scan produces, and a name no CDDL field can carry anyway (it is a Rust
    // keyword, already refused by `RUST_KEYWORDS`).
    found.remove("mut");
    found
}

/// Guard against the scan going vacuous: the four locals whose collisions motivated the registry
/// must still be found by it. A scan that silently stops matching would make the LOCKSTEP assertion
/// below pass for the wrong reason.
#[test]
fn emitter_local_scan_finds_the_known_anchors() {
    let found = scan_emitter_locals();
    for anchor in ["raw", "len", "read", "orig_deser_order", "len_encoding"] {
        assert!(
            found.contains_key(anchor),
            "the emitter-local scan no longer finds `{anchor}` — it has gone vacuous (or the \
             emitter renamed the local, in which case retire the registry entry). Found: {:?}",
            found.keys().collect::<Vec<_>>()
        );
    }
    assert!(
        found.len() > 30,
        "the emitter-local scan found only {} names — it has gone vacuous",
        found.len()
    );
}

/// LOCKSTEP (local tier and later — this module is NOT `snapshot_tests`, the one module fast runs):
/// every fixed local the emitters bind into a generated fn body must carry a verdict — either
/// `parsing::GENERATED_LOCAL_RESERVED` (probed to break ≥1 shape × profile, so a field by that name
/// is refused at parse time) or `parsing::GENERATED_LOCAL_PROBED_SAFE` (probed to break nothing, so
/// a field by that name is accepted). A NEW emitter local fails this test until it is swept and
/// verdicted, instead of being discovered by a consumer whose crate will not build.
///
/// Both directions are asserted: an unverdicted local is a missing sweep, and a verdicted name the
/// emitters no longer bind is a stale entry (retiring a RESERVED entry is an acceptance change —
/// make it deliberately, in its own commit).
#[test]
fn generated_local_registry_covers_emitter_locals() {
    let found = scan_emitter_locals();
    let reserved: std::collections::BTreeSet<&str> = GENERATED_LOCAL_RESERVED
        .iter()
        .map(|(n, _, _)| *n)
        .collect();
    let safe: std::collections::BTreeSet<&str> =
        GENERATED_LOCAL_PROBED_SAFE.iter().copied().collect();

    let overlap: Vec<&&str> = reserved.intersection(&safe).collect();
    assert!(
        overlap.is_empty(),
        "a name is both RESERVED and PROBED_SAFE — the verdict is ambiguous: {overlap:?}"
    );

    let unverdicted: Vec<String> = found
        .iter()
        .filter(|(name, _)| !reserved.contains(name.as_str()) && !safe.contains(name.as_str()))
        .map(|(name, files)| format!("  `{name}` — emitted by {files:?}"))
        .collect();
    assert!(
        unverdicted.is_empty(),
        "the emitters bind fixed local(s) with no verdict in `parsing::GENERATED_LOCAL_RESERVED` / \
         `GENERATED_LOCAL_PROBED_SAFE`:\n{}\n\nSweep each one (a field by that name, in each of the \
         array-rep / map-rep / tagged-record / embedded-plain-group / group-choice-arm shapes, \
         under default / --preserve-encodings / --preserve-encodings --canonical-form) and file it \
         under RESERVED (with the shape × profile × error class it breaks) or PROBED_SAFE.",
        unverdicted.join("\n")
    );

    let stale: Vec<String> = reserved
        .iter()
        .chain(safe.iter())
        .filter(|name| !found.contains_key(**name))
        .map(|name| format!("  `{name}`"))
        .collect();
    assert!(
        stale.is_empty(),
        "verdicted name(s) the emitters no longer bind — stale registry entries:\n{}",
        stale.join("\n")
    );
}

/// The generated-local hazard list swept below. SEPARATE from `hazards()` on purpose: that list
/// feeds the recombination fuzzer's deterministic composition set (a pure function of SEED, the
/// committed ingredients and the tables), so extending it would churn every committed recombination
/// outcome. This one is swept only here.
fn generated_local_hazards() -> Vec<&'static str> {
    let mut out: Vec<&'static str> = GENERATED_LOCAL_RESERVED
        .iter()
        .map(|(n, _, _)| *n)
        .collect();
    out.sort_unstable();
    out
}

/// A record inside a `#6.n(…)` tag — the shape whose deserializer emits the tag read, and the only
/// one where the `tag` reservation applies. The other three field positions reuse the templates the
/// keyword sweep already owns, so the two sweeps can never drift on what a "field position" is.
fn build_tagged_array_key(h: &str, i: usize) -> String {
    format!("holder{i} = #6.42([{h}: uint])\n")
}

/// The FIELD positions the generated-local registry governs, with the emitted SHAPE each one lands
/// (`rule-name` / `group-name` land a TYPE name, where these locals cannot collide, so they are
/// absent). The shape is what decides a cell's EXPECTED outcome, so it is recorded here rather than
/// inferred: `(position name, builder, rep, tagged)`.
#[allow(clippy::type_complexity)]
const FIELD_POSITIONS: &[(&str, fn(&str, usize) -> String, Representation, bool)] = &[
    ("map-key", build_map_key, Representation::Map, false),
    ("array-key", build_array_key, Representation::Array, false),
    (
        "name-directive",
        build_name_directive,
        Representation::Map,
        false,
    ),
    (
        "tagged-array-key",
        build_tagged_array_key,
        Representation::Array,
        true,
    ),
];

/// The generated-local sweep's catalog, asserted in BOTH directions against each entry's
/// `ReservedScope`: inside its scope a reserved name must reject gracefully (shipping an
/// uncompilable crate at exit 0 is the defect this registry closes), and OUTSIDE it the same name
/// must still generate (refusing a shape the emitter cannot collide in would break working specs —
/// the `tag: 0` group-choice discriminant is exactly that). `PANIC` is never acceptable in either
/// direction. Snapshotted alongside the keyword catalog so a verdict flip is visible in review
/// rather than only in an assertion message.
#[test]
fn generated_local_hazard_robustness_catalog() {
    let hz = generated_local_hazards();
    assert!(!hz.is_empty(), "generated-local hazard list is empty");
    let scope_of = |name: &str| {
        GENERATED_LOCAL_RESERVED
            .iter()
            .find(|(n, _, _)| *n == name)
            .map(|(_, scope, _)| *scope)
            .unwrap()
    };
    // The scope axis is only meaningful while each NARROWING variant has both a position that
    // exercises it and one that does not — otherwise the "outside its scope it still generates"
    // half is vacuous. `AnyRecord` has no outside by construction.
    for scope in [ReservedScope::MapRep, ReservedScope::Tagged] {
        assert!(
            FIELD_POSITIONS
                .iter()
                .any(|(_, _, rep, tagged)| scope.applies(*rep, *tagged))
                && FIELD_POSITIONS
                    .iter()
                    .any(|(_, _, rep, tagged)| !scope.applies(*rep, *tagged)),
            "no field position distinguishes {scope:?} — the sweep cannot pin its boundary"
        );
    }

    let mut catalog = String::from(
        "# generated-local reserved-field sweep: generation outcome per (field position × reserved\n\
         # generated-local name), against the name's declared `ReservedScope`. Inside its scope a\n\
         # name MUST be `error (graceful)` — the emitted crate would not compile (see\n\
         # parsing::GENERATED_LOCAL_RESERVED for the shape × profile × error class), so shipping it\n\
         # at exit 0 is the defect. OUTSIDE its scope it MUST be `ok`: the emitter binds no such\n\
         # local there, and refusing anyway would break working specs (the `tag: 0` group-choice\n\
         # discriminant). `PANIC` is never acceptable.\n\
         # Source: src/tests/identifier_hazard_tests.rs.\n\n",
    );
    let mut wrong = Vec::new();
    with_thread_silenced_panics(|| {
        for (pos_name, build, rep, tagged) in FIELD_POSITIONS {
            for (i, h) in hz.iter().enumerate() {
                let spec = build(h, i);
                let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    generate(&spec, &format!("genloc_{pos_name}_{i}"))
                }));
                let label = match outcome {
                    Ok(Ok(_)) => "ok",
                    Ok(Err(_)) => "error (graceful)",
                    Err(_) => "PANIC",
                };
                let in_scope = scope_of(h).applies(*rep, *tagged);
                let expected = if in_scope { "error (graceful)" } else { "ok" };
                if label != expected {
                    wrong.push(format!(
                        "{pos_name}/{h}: {label} (expected {expected} — scope {:?})",
                        scope_of(h)
                    ));
                }
                catalog.push_str(&format!(
                    "{pos_name:16} {h:18} {:9} {label}\n",
                    if in_scope { "in-scope" } else { "out" }
                ));
            }
        }
    });
    assert!(
        wrong.is_empty(),
        "generated-local cells disagree with their declared ReservedScope:\n{}",
        wrong.join("\n")
    );

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(
        std::env::current_dir()
            .unwrap()
            .join("tests/identifier_hazard/snapshots"),
    );
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!("generated_local_catalog", catalog));
}

/// The registry's ACCEPT side, which the catalog above cannot show: a name is refused on its
/// RESOLVED spelling, so `; @name` renaming a field OUT of the reserved set must still generate,
/// and the probed-safe locals must still generate in a field position. Without this the refusal
/// could over-fire (refusing working specs) and every catalog cell would still read green.
#[test]
fn generated_local_refusal_keys_on_the_resolved_name_only() {
    // renamed OUT of the reserved set — accepted (the wire key stays `raw`).
    for (tag, spec) in [
        ("renamed_out_map", "m = { raw: bytes ; @name payload\n}\n"),
        ("renamed_out_arr", "h = [raw: bytes ; @name payload\n]\n"),
        // a lone `<x>_encoding` field with no `<x>` sibling mints no colliding companion
        ("lone_encoding", "h = [foo_encoding: uint]\n"),
        // `_key` pairs only collide in the MAP rep (an array record mints no key encodings)
        ("array_key_pair", "h = [foo: bytes, foo_key: uint]\n"),
    ] {
        assert!(
            generate(spec, tag).is_ok(),
            "{tag}: must still generate — the refusal is over-firing:\n{spec}"
        );
    }
    // renamed INTO the reserved set — refused (the directive is the resolved name).
    for (tag, spec) in [
        ("renamed_in", "h = [ 0: uint, ; @name raw\n]\n"),
        // case-converted: `Raw` snake_cases to the reserved `raw`
        ("case_converted", "h = [pre: uint, Raw: bytes]\n"),
    ] {
        assert!(
            generate(spec, tag).is_err(),
            "{tag}: must be refused — the refusal keys on the wrong spelling:\n{spec}"
        );
    }
}

/// MANUAL/LOCAL ONLY (`#[ignore]`d, full tier via check.ts): the compile proof behind every `out`
/// cell of the catalog above. A `ReservedScope` that is too NARROW is invisible to a
/// generation-only sweep — the cell reads `ok` and the crate does not build, which is the exact
/// defect the registry exists to close. So each out-of-scope (position × reserved name) cell is
/// generated and `cargo check`ed, per PROFILE (the scopes are profile-independent by construction,
/// and `--preserve-encodings` is where every companion-minting collision lives).
///
/// A failure here means the named cell's scope must WIDEN (with the shape × profile × error class
/// added to its evidence string) — not that this gate should be relaxed.
///
/// Run: `cargo test --bin cddl-codegen generated_local_out_of_scope_crates_compile -- --ignored`.
#[test]
#[ignore]
fn generated_local_out_of_scope_crates_compile() {
    let hz = generated_local_hazards();
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_genloc_scope_{:016x}",
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);

    let mut failures = Vec::new();
    for (profile, flags) in [
        ("default", &[][..]),
        ("preserve", &["--preserve-encodings=true"][..]),
        (
            "canonical",
            &["--preserve-encodings=true", "--canonical-form=true"][..],
        ),
    ] {
        // one bundle crate per profile — the out-of-scope cells have distinct rule names by
        // construction (`holder{i}` per hazard index, per position).
        let mut bundle = String::new();
        let mut cells = Vec::new();
        for (pos_name, build, rep, tagged) in FIELD_POSITIONS {
            for (i, h) in hz.iter().enumerate() {
                let scope = GENERATED_LOCAL_RESERVED
                    .iter()
                    .find(|(n, _, _)| n == h)
                    .map(|(_, s, _)| *s)
                    .unwrap();
                if scope.applies(*rep, *tagged) {
                    continue;
                }
                // `holder{i}` collides across positions — disambiguate by position index.
                let pos_i = FIELD_POSITIONS
                    .iter()
                    .position(|(n, _, _, _)| n == pos_name)
                    .unwrap();
                bundle.push_str(&build(h, i + 100 * pos_i));
                cells.push(format!("{pos_name}/{h}"));
            }
        }
        assert!(
            !cells.is_empty(),
            "no out-of-scope cells to compile — the scope axis went vacuous"
        );
        let out = root.join(format!("out_of_scope__{profile}"));
        let target = root.join(format!("target__{profile}"));
        let mut spec_flags = vec!["--wasm=false".to_owned()];
        spec_flags.extend(flags.iter().map(|f| (*f).to_owned()));
        if let Err(reason) = gen_and_check_with(&bundle, &out, &target, &spec_flags) {
            failures.push(format!(
                "{profile}: the out-of-scope bundle does not compile — one of these cells needs a \
                 WIDER ReservedScope (bisect, then widen the entry and add the error class to its \
                 evidence): {cells:?}\n{reason}"
            ));
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}
