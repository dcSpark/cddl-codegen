//! Shape-recombination grammar fuzzer — deterministic recombination of the matrix's containment
//! examples into composed CDDL specs, run through the generator with escalating oracles.
//!
//! Every other gate samples ONE minimal example per feature row; the proven gap (the map-rep
//! group-choice fix found three defects hiding in unsampled shape variants of a single "supported"
//! row) lives in the UNsampled shape variants. This module recombines along the axes that matter:
//! (a) multi-member shape variation inside one construct (the member-kind table), (b) depth-2
//! nesting of constructs in container roles (the role-template table), and (c) identifier choice
//! (a low-weight axis drawn from `identifier_hazard_tests::hazards()`, never rediscovered).
//!
//! Ingredients come from the committed `tests/recomb/ingredients.json` (projected from the matrix
//! by `cddl-matrix/project_recombination.ts`; drift-gated by check.ts `project_recombination_check`):
//! per-feature filler expressions + the containment legality data. LEGALITY SEMANTICS: the
//! containment matrix enumerates only structurally interesting cells and omits trivial
//! primitive-as-member cells as implicitly allowed, so the composer treats it as a BLACKLIST — any
//! (role, filler-feature) pair composes unless projected `disallowed`; the `legal` (spec="allowed")
//! pairs are used as template↔matrix drift protection (every role template must name a role with at
//! least one modelled allowed cell).
//!
//! This is a CORPUS GENERATOR, not a CI gate: the standing harness detects NEW divergence classes;
//! each finding is promoted into the existing pinned collections (matrix rows / `tests/robustness/`
//! fixtures / `tests/corpus/`) after review, and its class enters the cited ledgers below.
//!
//! Two layers, mirroring the identifier-hazard split:
//!   1. `recombination_generation_sweep` (default `cargo test`, check.ts `local` tier) — classify
//!      every composition's GENERATION outcome (ok / graceful / PANIC) in-process. A PANIC whose
//!      normalized message matches no `KNOWN_PANIC_CLASSES` entry is a NEW finding and fails the
//!      test with the spec + message + promotion instructions. Budget ~20 s (classification is
//!      parallelized across worker threads; the composition SET is unaffected by thread count).
//!   2. The layer-2 gates (`#[ignore]`, check.ts `full` tier), one per emission profile through the
//!      shared `run_layer2_profile` runner — `recombination_crates_execute` (default),
//!      `recombination_preserve_crates_execute`, `recombination_json_crates_execute` (both
//!      `cargo test` the emitted-tests rust crate under their `ALL_PROFILES` flags), and
//!      `recombination_wasm_crates_check` (`--wasm=true`, `cargo check` the wasm crate) — batch the
//!      profile's `ok` compositions (~`LAYER2_RULES_PER_BATCH` rules/batch; names are collision-free
//!      by construction; per-profile scratch + `CARGO_TARGET_DIR`). Any batch failure is
//!      re-attributed by rerunning members individually; a failing member whose desc matches neither
//!      the shared `LAYER2_KNOWN_BAD` nor the profile's own ledger is a NEW finding, and a
//!      classification panic outside `KNOWN_PANIC_CLASSES` ∪ the profile's panic ledger likewise.
//!      Target < 10 min per gate.
//!
//! Determinism: a fixed seed + splitmix64; enumeration is a systematic cross-product where cheap
//! and seeded sampling where the product explodes (budget constants below). The sweep asserts two
//! back-to-back enumerations are identical, and floors are derived from the EXECUTED artifact
//! (swept/ok counts + every ledger entry actually observed), so a rotted composer or an
//! accidentally-empty ingredients file fails loud rather than passing vacuously.

use crate::cli::Cli;
use crate::tests::gate_cache;
use crate::tests::identifier_hazard_tests::hazards;
use crate::tests::integration_tests::{checkout_hash, tool_cmd};
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;
use std::collections::{BTreeMap, BTreeSet};

// ---- budgets (tune here; floors are asserted from the executed artifact) --------------------------
/// Fixed seed for all sampled choices — change deliberately (it re-rolls every sampled composition).
const SEED: u64 = 0xCDD1_2026_0709_0001;
/// Seeded member-kind triples per construct shape (the 3-member cross-product would explode).
const TRIPLE_SAMPLES_PER_SHAPE: usize = 25;
/// Seeded leaf fillers per (outer template × inner template) depth-2 pair.
const NEST_FILLER_SAMPLES: usize = 2;
/// One field name in every N-th member-kind draw is taken from the hazard table (low-weight axis:
/// the hazard sweep already covers name×position systematically; here it's realistic noise).
const HAZARD_EVERY: u64 = 16;
/// Layer 2 batching: ~this many RULES per generated crate (a composition is 1 root + its aux rules).
///
/// BATCH-MASKING CAVEAT: batching compiles many compositions into ONE crate, so a failure class
/// whose symptom is a missing CRATE-GLOBAL definition can be masked by a batch-mate that happens to
/// define the global — the per-member attribution rerun only fires when the BATCH fails, so a green
/// batch is not a per-composition guarantee for such a class. Consequence: a known-bad class proven
/// by a STANDALONE repro belongs in the ledger even if the current batch boundaries happen to compile
/// it. The precedent was the undefined-`Int` class (`Int` was emitted iff any rule registered a
/// reference): `outer=cbor_payload filler=type2.map` was masked in the default gate and surfaced only
/// by the wasm leg's different batch boundaries. That class is now fixed — the reference walk covers
/// emitted type aliases (pinned by tests/corpus/int_alias.cddl) — so it no longer rides the ledger,
/// but the caveat stands for the next crate-global-definition class.
const LAYER2_RULES_PER_BATCH: usize = 40;

// ---- deterministic rng ----------------------------------------------------------------------------
/// splitmix64 — tiny, seedable, no new deps. Used ONLY during enumeration in a fixed call order,
/// so the composition set is a pure function of `SEED` + the committed ingredients + these tables.
fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

// ---- ingredients ----------------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
struct Filler {
    feature: String,
    expr: String,
    aux: Vec<String>,
}

struct Ingredients {
    fillers: Vec<Filler>,
    legal_roles: BTreeSet<String>,
    disallowed: BTreeSet<(String, String)>, // (role, feature)
}

fn load_ingredients() -> Ingredients {
    let text = std::fs::read_to_string("tests/recomb/ingredients.json")
        .expect("tests/recomb/ingredients.json missing — run `bun run project_recombination.ts` in cddl-matrix/");
    let v: serde_json::Value = serde_json::from_str(&text).unwrap();
    let fillers: Vec<Filler> = v["fillers"]
        .as_array()
        .unwrap()
        .iter()
        .map(|f| Filler {
            feature: f["feature"].as_str().unwrap().to_owned(),
            expr: f["expr"].as_str().unwrap().to_owned(),
            aux: f["aux"]
                .as_array()
                .unwrap()
                .iter()
                .map(|a| a.as_str().unwrap().to_owned())
                .collect(),
        })
        .collect();
    let pair = |p: &serde_json::Value| {
        (
            p["role"].as_str().unwrap().to_owned(),
            p["feature"].as_str().unwrap().to_owned(),
        )
    };
    let legal_roles: BTreeSet<String> = v["legal"]
        .as_array()
        .unwrap()
        .iter()
        .map(|p| pair(p).0)
        .collect();
    let disallowed: BTreeSet<(String, String)> = v["disallowed"]
        .as_array()
        .unwrap()
        .iter()
        .map(pair)
        .collect();
    assert!(
        !fillers.is_empty(),
        "ingredients.json has zero fillers — vacuous"
    );
    Ingredients {
        fillers,
        legal_roles,
        disallowed,
    }
}

// ---- identifier renaming (aux-rule collision proofing) --------------------------------------------
/// Rewrite word-boundary occurrences of `from` to `to` in a CDDL snippet, skipping quoted strings
/// and BAREWORD-KEY positions (an ident directly followed by `:` names a member key, not a type
/// reference — renaming it would change the wire key).
fn rename_ident(text: &str, from: &str, to: &str) -> String {
    let bytes = text.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_' || b == b'-';
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    let mut in_quote = false;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if in_quote {
            out.push(c);
            if c == '"' {
                in_quote = false;
            }
            i += 1;
            continue;
        }
        if c == '"' {
            in_quote = true;
            out.push(c);
            i += 1;
            continue;
        }
        if (c.is_ascii_alphabetic()) && (i == 0 || !is_ident(bytes[i - 1])) {
            let start = i;
            while i < bytes.len() && is_ident(bytes[i]) {
                i += 1;
            }
            let token = &text[start..i];
            // peek past whitespace for a bareword-key `:`
            let mut j = i;
            while j < bytes.len() && (bytes[j] == b' ' || bytes[j] == b'\t') {
                j += 1;
            }
            let is_bareword_key = j < bytes.len() && bytes[j] == b':';
            if token == from && !is_bareword_key {
                out.push_str(to);
            } else {
                out.push_str(token);
            }
            continue;
        }
        out.push(c);
        i += 1;
    }
    out
}

/// Instantiate `filler` for composition `prefix`: aux rule names get `{prefix}_` so batch
/// concatenation can never collide, and references in the expr + aux bodies are rewritten.
fn filler_instance(filler: &Filler, prefix: &str) -> (String, Vec<String>) {
    let mut expr = filler.expr.clone();
    let mut aux = filler.aux.clone();
    for a in &filler.aux {
        // aux rule name = leading ident of the rule text
        let name: String = a
            .chars()
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '-')
            .collect();
        let renamed = format!("{prefix}_{}", name.replace('-', "_"));
        expr = rename_ident(&expr, &name, &renamed);
        for b in aux.iter_mut() {
            *b = rename_ident(b, &name, &renamed);
        }
    }
    (expr, aux)
}

// ---- role templates -------------------------------------------------------------------------------
/// A hole-bearing container-role template. `feature` is the feature id the INSTANTIATED construct
/// represents when it itself fills another template's hole (the depth-2 legality check), and
/// `role` is the containment role its own hole exposes (checked against `disallowed` for whatever
/// fills the hole). `build` returns the type EXPRESSION (not a rule); extra aux rules (the
/// generic-arg template needs one) are pushed with the composition prefix pre-applied.
struct Template {
    name: &'static str,
    role: &'static str,
    /// feature id of the construct this template builds (for legality when nested); "" = never nested.
    feature: &'static str,
    build: fn(hole: &str, prefix: &str, aux: &mut Vec<String>) -> String,
}

const TYPE_TEMPLATES: &[Template] = &[
    Template {
        name: "top_level",
        role: "role.top-level",
        feature: "", // identity — never used as an inner construct
        build: |h, _p, _a| h.to_owned(),
    },
    Template {
        name: "arr_single",
        role: "role.array-element",
        feature: "type2.array",
        build: |h, _p, _a| format!("[{h}]"),
    },
    Template {
        name: "arr_mid",
        role: "role.array-element",
        feature: "type2.array",
        build: |h, _p, _a| format!("[a: uint, {h}, b: tstr]"),
    },
    Template {
        name: "map_value",
        role: "role.map-value",
        feature: "type2.map",
        build: |h, _p, _a| format!("{{ k: {h} }}"),
    },
    Template {
        name: "map_key",
        role: "role.map-key",
        feature: "type2.map",
        build: |h, _p, _a| format!("{{ {h} => uint }}"),
    },
    Template {
        name: "choice_member",
        role: "role.choice-member",
        feature: "type.choice",
        build: |h, _p, _a| format!("{h} / tstr"),
    },
    Template {
        name: "occurrence",
        role: "role.occurrence-target",
        feature: "type2.array",
        build: |h, _p, _a| format!("[* {h}]"),
    },
    Template {
        name: "tag_content",
        role: "role.tag-content",
        feature: "type2.tag",
        build: |h, _p, _a| format!("#6.11({h})"),
    },
    Template {
        name: "cbor_payload",
        role: "role.cbor-payload",
        feature: "type1.ctlop",
        build: |h, _p, _a| format!("bytes .cbor {h}"),
    },
    Template {
        name: "generic_arg",
        role: "role.generic-arg",
        feature: "genericarg.type",
        build: |h, p, a| {
            a.push(format!("{p}_gen<a0> = [a0]"));
            format!("{p}_gen<{h}>")
        },
    },
];

/// Group-hole templates (the `//` arm is a GROUP container, filled by member sequences, not types).
const GROUP_TEMPLATES: &[Template] = &[
    Template {
        name: "garm_map",
        role: "role.group-choice-arm",
        feature: "type2.map",
        build: |g, _p, _a| format!("{{ {g} // fb: tstr }}"),
    },
    Template {
        name: "garm_arr",
        role: "role.group-choice-arm",
        feature: "type2.array",
        build: |g, _p, _a| format!("[ {g} // tstr ]"),
    },
];

// ---- member kinds (the proven multi-member axis) --------------------------------------------------
/// One member shape inside a composite construct. `%K%`/`%K2%` are field-name slots; `%F%` is a
/// filler-expression slot (only `filler` uses it). The known-outcome kinds are deliberately kept in
/// (fixed bool = pinned panic, zero-star = pinned graceful reject): the sweep must OBSERVE the
/// pinned classes, that's the ledger's anti-vacuity floor.
const MEMBER_KINDS: &[(&str, &str)] = &[
    ("fixed_uint", "%K%: 5"),
    ("fixed_text", "%K%: \"x\""),
    ("fixed_bool", "%K%: true"),
    ("fixed_null", "%K%: null"),
    ("scalar", "%K%: uint"),
    ("optional", "? %K%: uint"),
    ("zero_star", "* %K%: uint"),
    ("inline_group", "(%K%: uint, %K2%: tstr)"),
    ("filler", "%K%: %F%"),
];

/// Construct shapes for the member-kind axis: struct map, array record, and the 2-arm group choice
/// in both representations (the arm under variation is the FIRST arm; the fallback arm is fixed).
const SHAPES: &[(&str, &str, &str)] = &[
    // (name, prefix, suffix) — members joined with ", " in between
    ("struct_map", "{ ", " }"),
    ("arr_record", "[ ", " ]"),
    ("gchoice_map", "{ ", " // zz: tstr }"),
    ("gchoice_arr", "[ ", " // tstr ]"),
];

// ---- compositions ---------------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
struct Composition {
    /// Stable id, embedded as the root rule name (`rc<num>`); aux rules are `rc<num>_*`.
    id: String,
    /// Human description for finding reports (axis + parts + involved feature ids) — also the key
    /// space for `LAYER2_KNOWN_BAD` substring matching.
    desc: String,
    /// The full CDDL spec (root rule + aux rules, newline-joined).
    spec: String,
    /// Rule count (root + aux), for layer-2 batching.
    rules: usize,
}

/// Field-name chooser: benign `f{idx}` names, with every `HAZARD_EVERY`-th draw swapped for a
/// hazard-table identifier (the low-weight identifier axis).
fn field_name(rng: &mut u64, idx: usize, hz: &[&'static str]) -> String {
    let r = splitmix64(rng);
    if r.is_multiple_of(HAZARD_EVERY) {
        hz[(r / HAZARD_EVERY) as usize % hz.len()].to_owned()
    } else {
        format!("f{idx}")
    }
}

/// Mutable per-composition state a member instantiation appends into (bundled to keep the arg
/// list within clippy's bound).
struct MemberSink<'a> {
    rng: &'a mut u64,
    aux: &'a mut Vec<String>,
    features: &'a mut Vec<String>,
}

/// Instantiate one member-kind at member position `idx` for composition `prefix`.
fn member_instance(
    kind: &(&str, &str),
    idx: usize,
    sink: &mut MemberSink<'_>,
    fillers: &[Filler],
    prefix: &str,
    hz: &[&'static str],
) -> String {
    let mut m = kind.1.replace("%K2%", &format!("g{idx}"));
    m = m.replace("%K%", &field_name(sink.rng, idx, hz));
    if m.contains("%F%") {
        let f = &fillers[(splitmix64(sink.rng) as usize) % fillers.len()];
        let (expr, mut fa) = filler_instance(f, prefix);
        sink.aux.append(&mut fa);
        sink.features.push(f.feature.clone());
        m = m.replace("%F%", &expr);
    }
    m
}

/// THE composition set — a pure function of `SEED`, the committed ingredients, and the tables
/// above. Systematic cross-products for singles/pairs and the depth-1 template×filler axis;
/// seeded sampling for triples and depth-2 leaf fillers.
fn compositions() -> Vec<Composition> {
    let ing = load_ingredients();
    let hz = hazards();
    // Template↔matrix drift protection, both ways: every template role must have at least one
    // modelled `allowed` containment cell (a template naming a role the matrix dropped is stale).
    for t in TYPE_TEMPLATES.iter().chain(GROUP_TEMPLATES.iter()) {
        assert!(
            ing.legal_roles.contains(t.role),
            "template `{}` names role `{}` with no allowed containment cell — template↔matrix drift",
            t.name,
            t.role
        );
    }
    let blocked = |role: &str, feature: &str| {
        ing.disallowed
            .contains(&(role.to_owned(), feature.to_owned()))
    };

    let mut rng: u64 = SEED;
    let mut out: Vec<Composition> = Vec::new();
    let mut n = 0usize;
    let push = |out: &mut Vec<Composition>,
                n: &mut usize,
                desc: String,
                root_expr: String,
                aux: Vec<String>,
                features: Vec<String>| {
        let id = format!("rc{:04}", *n);
        *n += 1;
        let mut spec = format!("{id} = {root_expr}\n");
        for a in &aux {
            spec.push_str(a);
            spec.push('\n');
        }
        let desc = if features.is_empty() {
            desc
        } else {
            format!("{desc} features=[{}]", features.join(","))
        };
        out.push(Composition {
            id,
            desc,
            spec,
            rules: 1 + aux.len(),
        });
    };

    // -- axis 1: member-kind singles + pairs (systematic) + triples (seeded) per shape ------------
    for (shape, pre, post) in SHAPES {
        for kind in MEMBER_KINDS {
            let prefix = format!("rc{n:04}");
            let mut aux = Vec::new();
            let mut features = Vec::new();
            let m = member_instance(
                kind,
                0,
                &mut MemberSink {
                    rng: &mut rng,
                    aux: &mut aux,
                    features: &mut features,
                },
                &ing.fillers,
                &prefix,
                &hz,
            );
            push(
                &mut out,
                &mut n,
                format!("shape={shape} members=[{}]", kind.0),
                format!("{pre}{m}{post}"),
                aux,
                features,
            );
        }
        for k1 in MEMBER_KINDS {
            for k2 in MEMBER_KINDS {
                let prefix = format!("rc{n:04}");
                let mut aux = Vec::new();
                let mut features = Vec::new();
                let m1 = member_instance(
                    k1,
                    0,
                    &mut MemberSink {
                        rng: &mut rng,
                        aux: &mut aux,
                        features: &mut features,
                    },
                    &ing.fillers,
                    &prefix,
                    &hz,
                );
                let m2 = member_instance(
                    k2,
                    1,
                    &mut MemberSink {
                        rng: &mut rng,
                        aux: &mut aux,
                        features: &mut features,
                    },
                    &ing.fillers,
                    &prefix,
                    &hz,
                );
                push(
                    &mut out,
                    &mut n,
                    format!("shape={shape} members=[{},{}]", k1.0, k2.0),
                    format!("{pre}{m1}, {m2}{post}"),
                    aux,
                    features,
                );
            }
        }
        for _ in 0..TRIPLE_SAMPLES_PER_SHAPE {
            let ks: Vec<&(&str, &str)> = (0..3)
                .map(|_| &MEMBER_KINDS[(splitmix64(&mut rng) as usize) % MEMBER_KINDS.len()])
                .collect();
            let prefix = format!("rc{n:04}");
            let mut aux = Vec::new();
            let mut features = Vec::new();
            let mut ms: Vec<String> = Vec::with_capacity(ks.len());
            for (i, k) in ks.iter().enumerate() {
                ms.push(member_instance(
                    k,
                    i,
                    &mut MemberSink {
                        rng: &mut rng,
                        aux: &mut aux,
                        features: &mut features,
                    },
                    &ing.fillers,
                    &prefix,
                    &hz,
                ));
            }
            push(
                &mut out,
                &mut n,
                format!(
                    "shape={shape} members=[{}]",
                    ks.iter().map(|k| k.0).collect::<Vec<_>>().join(",")
                ),
                format!("{pre}{}{post}", ms.join(", ")),
                aux,
                features,
            );
        }
    }

    // -- axis 2: depth-1 template × filler (systematic, blacklist-filtered) -----------------------
    for t in TYPE_TEMPLATES {
        for f in &ing.fillers {
            if blocked(t.role, &f.feature) {
                continue;
            }
            let prefix = format!("rc{n:04}");
            let mut aux = Vec::new();
            let (fexpr, mut fa) = filler_instance(f, &prefix);
            aux.append(&mut fa);
            let root = (t.build)(&fexpr, &prefix, &mut aux);
            push(
                &mut out,
                &mut n,
                format!("outer={} filler={}", t.name, f.feature),
                root,
                aux,
                vec![f.feature.clone()],
            );
        }
    }

    // -- axis 3: depth-2 nesting — outer(type) ∘ inner(type) ∘ seeded leaf fillers ----------------
    for outer in TYPE_TEMPLATES.iter().filter(|t| t.name != "top_level") {
        for inner in TYPE_TEMPLATES.iter().filter(|t| t.name != "top_level") {
            if blocked(outer.role, inner.feature) {
                continue;
            }
            for _ in 0..NEST_FILLER_SAMPLES {
                // seeded leaf filler, first legal candidate from the seeded start (deterministic).
                let start = (splitmix64(&mut rng) as usize) % ing.fillers.len();
                let f = (0..ing.fillers.len())
                    .map(|k| &ing.fillers[(start + k) % ing.fillers.len()])
                    .find(|f| !blocked(inner.role, &f.feature))
                    .expect("no legal leaf filler for inner template");
                let prefix = format!("rc{n:04}");
                let mut aux = Vec::new();
                let (fexpr, mut fa) = filler_instance(f, &prefix);
                aux.append(&mut fa);
                let inner_expr = (inner.build)(&fexpr, &prefix, &mut aux);
                let root = (outer.build)(&inner_expr, &prefix, &mut aux);
                push(
                    &mut out,
                    &mut n,
                    format!(
                        "outer={} inner={} filler={}",
                        outer.name, inner.name, f.feature
                    ),
                    root,
                    aux,
                    vec![f.feature.clone()],
                );
            }
        }
    }

    // -- axis 4: depth-2 through a group-choice arm — arm member typed by an inner template -------
    for outer in GROUP_TEMPLATES {
        for inner in TYPE_TEMPLATES.iter().filter(|t| t.name != "top_level") {
            let start = (splitmix64(&mut rng) as usize) % ing.fillers.len();
            let f = (0..ing.fillers.len())
                .map(|k| &ing.fillers[(start + k) % ing.fillers.len()])
                .find(|f| !blocked(inner.role, &f.feature))
                .expect("no legal leaf filler for inner template");
            let prefix = format!("rc{n:04}");
            let mut aux = Vec::new();
            let (fexpr, mut fa) = filler_instance(f, &prefix);
            aux.append(&mut fa);
            let inner_expr = (inner.build)(&fexpr, &prefix, &mut aux);
            let root = (outer.build)(&format!("ga: {inner_expr}"), &prefix, &mut aux);
            push(
                &mut out,
                &mut n,
                format!(
                    "outer={} inner={} filler={}",
                    outer.name, inner.name, f.feature
                ),
                root,
                aux,
                vec![f.feature.clone()],
            );
        }
    }

    out
}

// ---- classification -------------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
enum Outcome {
    Ok,
    Graceful(String),
    Panic(String), // normalized "<message> @ <file> @ fn <symbol>" (line dropped: survives
                   // unrelated edits; the frame symbol splits same-file same-message sites by
                   // panicking function — see `production_frame_symbol`)
}

/// Extract the panicking PRODUCTION frame's function symbol from a captured backtrace's Display
/// text. Scans innermost-first (frame 0 = innermost) for the first frame whose symbol is in
/// `cddl_codegen::` but NOT `cddl_codegen::tests` (the sweep's hook/worker frames), then normalizes
/// it: drop `{{closure}}` wrapper segments (a bare `unimplemented!()` inside a `.map`/`match`
/// closure would otherwise key on the closure, not the enclosing fn) and any trailing hash suffix
/// (std's demangled Display carries none, but strip it defensively). Fallback when no production
/// frame is found: `<no production frame>`.
///
/// COLLAPSE BOUNDARY: the key is per-(message, file, FUNCTION). Two bare sites inside the SAME
/// function still share a key — `codegen_struct` and `generate_wrapper_struct` each host two bare
/// `unimplemented!()` sites (`generation/records.rs` / `generation/wrappers.rs`), so a composition newly reaching the *other* site in
/// one of those functions is still absorbed by that function's entry rather than surfacing as a NEW
/// finding. Splitting those would need line numbers, which are deliberately excluded (refactor
/// churn).
fn production_frame_symbol(bt: &str) -> String {
    for line in bt.lines() {
        let t = line.trim_start();
        // frame lines are `<indent><num>: <symbol>`; the `at <file>:<line>` lines have no `<num>:`.
        let Some((num, sym)) = t.split_once(": ") else {
            continue;
        };
        if num.parse::<usize>().is_err() {
            continue;
        }
        if !sym.contains("cddl_codegen::") || sym.contains("cddl_codegen::tests") {
            continue;
        }
        return sym
            .split("::")
            .filter(|seg| *seg != "{{closure}}")
            // a legacy mangled hash tail is `h` + 16 hex digits; real fn names never match that.
            .filter(|seg| {
                !(seg.len() == 17
                    && seg.starts_with('h')
                    && seg[1..].chars().all(|c| c.is_ascii_hexdigit()))
            })
            .collect::<Vec<_>>()
            .join("::");
    }
    "<no production frame>".to_owned()
}

std::thread_local! {
    static LAST_PANIC: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };
}

/// Classify every composition's generation outcome, parallelized across worker threads (the
/// composition SET is fixed beforehand; workers only classify, so thread count never changes WHAT
/// is swept, only how fast). One silenced-hook window for the whole pass: inside it we swap in a
/// CAPTURING hook that records message + file + panicking-function symbol (no line — survives
/// unrelated edits; the symbol splits same-file same-message sites per function, see
/// `production_frame_symbol`) into each REGISTERED worker's
/// thread-local, delegating any other thread's panic to the hook we replaced (unrelated test
/// failures stay visible), and restore before the window closes.
///
/// `extra_args` are appended to the in-process `Cli::parse_from` invocation, selecting the emission
/// PROFILE. Callers must pass the wasm mode explicitly (`--wasm=false` or `--wasm=true`) so profile
/// classification cannot accidentally diverge from the out-of-process generation mode. The
/// composition SET is profile-INDEPENDENT — only classification runs per profile — so the determinism
/// assert in layer 1 keeps holding regardless of `extra_args`.
fn classify_all(comps: &[Composition], extra_args: &[&str]) -> Vec<Outcome> {
    with_thread_silenced_panics(|| {
        let prev: std::sync::Arc<dyn Fn(&std::panic::PanicHookInfo) + Send + Sync> =
            std::sync::Arc::from(std::panic::take_hook());
        // `ThreadId` is not `Ord`, so a plain Vec holds the (≤ 8) registered workers; this set
        // never influences output or iteration order, only hook routing.
        let workers: std::sync::Arc<std::sync::Mutex<Vec<std::thread::ThreadId>>> =
            std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let delegate = prev.clone();
        let wset = workers.clone();
        std::panic::set_hook(Box::new(move |info| {
            let is_worker = wset
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .contains(&std::thread::current().id());
            if is_worker {
                let msg = info
                    .payload()
                    .downcast_ref::<String>()
                    .cloned()
                    .or_else(|| info.payload().downcast_ref::<&str>().map(|s| s.to_string()))
                    .unwrap_or_else(|| "<non-string panic payload>".to_owned());
                let file = info
                    .location()
                    .map(|l| l.file().to_owned())
                    .unwrap_or_default();
                // Symbolication happens ONLY here (on a panic, ~500/sweep), never on the ok path.
                let symbol = production_frame_symbol(
                    &std::backtrace::Backtrace::force_capture().to_string(),
                );
                let norm = format!(
                    "{} @ {file} @ fn {symbol}",
                    msg.split_whitespace().collect::<Vec<_>>().join(" ")
                );
                LAST_PANIC.with(|p| *p.borrow_mut() = Some(norm));
            } else {
                delegate(info)
            }
        }));

        let n_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(8);
        let chunk_size = comps.len().div_ceil(n_workers);
        let mut out: Vec<Outcome> = Vec::with_capacity(comps.len());
        std::thread::scope(|s| {
            let handles: Vec<_> = comps
                .chunks(chunk_size)
                .enumerate()
                .map(|(wi, chunk)| {
                    let workers = workers.clone();
                    s.spawn(move || {
                        // register BEFORE any classification so the hook can never miss us
                        workers
                            .lock()
                            .unwrap_or_else(|e| e.into_inner())
                            .push(std::thread::current().id());
                        let path = std::env::temp_dir().join(format!(
                            "cddl_codegen_recomb_sweep_{}_{wi}.cddl",
                            std::process::id()
                        ));
                        // Identical per composition within a worker (only the spec file content
                        // changes), so build the argv once; `extra_args` selects the profile.
                        let mut argv: Vec<&str> = vec![
                            "cddl-codegen",
                            "--input",
                            path.to_str().unwrap(),
                            "--output",
                            "recomb_unused",
                        ];
                        argv.extend_from_slice(extra_args);
                        let res: Vec<Outcome> = chunk
                            .iter()
                            .map(|c| {
                                std::fs::write(&path, &c.spec).unwrap();
                                let cli = Cli::parse_from(argv.iter().copied());
                                LAST_PANIC.with(|p| *p.borrow_mut() = None);
                                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                    crate::api::generated_strings(&cli)
                                })) {
                                    Ok(Ok(_)) => Outcome::Ok,
                                    Ok(Err(e)) => Outcome::Graceful(e.to_string()),
                                    Err(_) => Outcome::Panic(
                                        LAST_PANIC.with(|p| p.borrow_mut().take()).unwrap_or_else(
                                            || "<panic with no hook capture>".to_owned(),
                                        ),
                                    ),
                                }
                            })
                            .collect();
                        std::fs::remove_file(&path).ok();
                        res
                    })
                })
                .collect();
            for h in handles {
                out.extend(h.join().unwrap());
            }
        });

        let _ = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| prev(info)));
        out
    })
}

// ---- the known-panic-class ledger (layer 1) --------------------------------------------------------
/// Normalized-message substring -> the existing pin that owns the class. EVERY entry must cite a
/// committed pin (a matrix fixture row, a `tests/robustness/` fixture, or a `cddl-matrix/ROADMAP.md`
/// findings-ledger entry) — cite stable identifiers, never positions. Every entry is also asserted
/// OBSERVED by the sweep (stale-pin guard: an entry whose class stops firing must be pruned or the
/// composer fixed). A PANIC matching no entry is a NEW finding and fails the sweep.
///
/// Messages are normalized to `<whitespace-collapsed message> @ <file> @ fn <symbol>` (no line
/// numbers — see `production_frame_symbol`), so a substring may pin the file AND the panicking
/// function when the message alone is ambiguous (`not implemented @ src/generation/mod.rs @ fn
/// <symbol>` matches only a BARE `unimplemented!()` in that specific function — any detailed
/// message breaks the contiguity, and a different function yields a different `<symbol>`). The
/// remaining collapse boundary is per-(message, file, function): two bare sites in the SAME
/// function share one key (`codegen_struct` / `generate_wrapper_struct` each host two).
const KNOWN_PANIC_CLASSES: &[(&str, &str)] = &[
    (
        "Anonymous groups not allowed",
        "anonymous nested composite; pinned by tests/matrix_panic/contain.array-element.type2.map.cddl (and role siblings)",
    ),
    (
        "inline group entries are not implemented",
        "inline group in a group-choice arm; pinned by tests/matrix_panic/contain.group-choice-arm.grpent.inline_group.array.cddl",
    ),
    (
        "TODO: non-table types as types",
        "anonymous composite where a type is required; pinned by tests/matrix_panic/contain.group-choice-arm.type2.map.array.cddl and tests/matrix_panic/contain.generic-arg.type2.map.cddl",
    ),
    (
        "unsupported cddl prelude type:",
        "unsupported prelude types (eb64url/eb64legacy/eb16/cbor-any/undefined); pinned by tests/matrix_panic/prelude.eb64url.cddl and siblings",
    ),
    (
        "should be handled by the alias system instead",
        "float16 / float16-32 / float32-64 (no native Rust f16 / float-choice); pinned by tests/matrix_panic/prelude.float16.cddl and siblings",
    ),
    (
        "Ignoring Type2:",
        "unsupported type2 in MEMBER position (top-level rule bodies now reject gracefully via `record_rejection`; this is the member-side site that still panics); pinned by tests/matrix_panic/contain.array-element.type2.unwrap.cddl and role siblings",
    ),
    (
        "non-literal tag heads (#6.<type>(...)) are not supported",
        "type-valued tag head (RFC 9682); pinned by tests/matrix_reject/type2.tag_head_type.cddl (PANIC row in the reject catalog)",
    ),
    (
        "doubly nested tags are not supported",
        "tag directly inside a tag; pinned by tests/matrix_panic/contain.tag-content.type2.tag.cddl",
    ),
    (
        "failed left: \";\" right: \"\" @ src/generation/deserialize.rs",
        "map-rep group-choice arm with a fixed-value entry; pinned by tests/matrix_panic/contain.group-choice-arm.type2.value.map.cddl",
    ),
    // (retired when `any` gained runtime support) `any` in member/element position no longer panics —
    // it lowers to the `AnyCbor` runtime type (tests/robustness/any_member.cddl is now an `ok`
    // fixture). The former `self.generic_instances.contains_key(ident)` assertion class is gone.
    // (retired in the same work) the `any` type-choice arm no longer reaches the
    // `rust_type.rs` `Option::unwrap()` panic. Support narrowed the former blanket rejection into a
    // last-position rule: a bare `any` catch-all is SUPPORTED in last position (forced-backtracking
    // dispatch — a typed arm matching on type but failing on content falls through), and a non-last
    // bare `any` arm is a graceful rejection ("makes later arms unreachable";
    // tests/robustness/choice_any_arm.cddl is that `error (graceful)` fixture, the last-position
    // support is tests/robustness/choice_last_any_arm.cddl). A `[* any]` (container-of-any) arm
    // generates correctly (tests/robustness/choice_array_any_arm.cddl).
    (
        "should not expose Fixed type",
        "bare fixed value under an occurrence / tagged prelude constant; pinned by tests/robustness/fixed_value_occurrence.cddl and tests/robustness/tagged_prelude_constant.cddl (recombination findings)",
    ),
];
// NOT in the ledger despite being a real, robustness-pinned panic class (the ledger's stale-pin
// guard requires sweep observation, and the CURRENT enumeration does not compose the shape): the
// `.cbor`-over-a-reference type-choice arm (tests/robustness/choice_cbor_ref_arm.cddl, "variant
// ctor refers to undefined ident" — that message was worded lead-constant so a future ledger entry
// can key on it). It was surfaced by a TRANSIENT enumeration (a since-skipped vacuous
// `dsl.rust_name` filler shifted the composition indices), promoted, then the enumeration reverted;
// the robustness catalog row keeps it exercised. If an enumeration change re-composes the shape, the
// sweep fails NEW-finding with the fixture already committed — re-add the entry citing it.
// (The array-of-`any` type-choice arm that used to sit here is gone: `[* any]` is now a supported
// homogeneous array, so `[* any] / tstr` generates correctly — tests/robustness/choice_array_any_arm.cddl
// is an `ok` fixture, not a panic class, since `any` gained runtime support.)

// ---- LAYER 1: the generation-classification sweep ---------------------------------------------------
/// Sweep every composition through in-process generation and classify ok / graceful / PANIC.
/// A PANIC outside `KNOWN_PANIC_CLASSES` fails with promotion instructions. Floors are derived
/// from the executed artifact: swept/ok counts, and every ledger class actually observed.
#[test]
fn recombination_generation_sweep() {
    let t0 = std::time::Instant::now();
    let comps = compositions();
    assert_eq!(
        comps,
        compositions(),
        "composition enumeration must be deterministic (seeded; no hash iteration order)"
    );

    let outcomes = classify_all(&comps, &["--wasm", "false"]);

    let mut ok = 0usize;
    let mut graceful = 0usize;
    let mut panics = 0usize;
    let mut findings: Vec<String> = Vec::new();
    let mut observed_classes: BTreeSet<&str> = BTreeSet::new();
    for (c, o) in comps.iter().zip(outcomes.iter()) {
        match o {
            Outcome::Ok => ok += 1,
            Outcome::Graceful(_) => graceful += 1,
            Outcome::Panic(msg) => {
                panics += 1;
                match KNOWN_PANIC_CLASSES.iter().find(|(sub, _)| msg.contains(sub)) {
                    Some((sub, _)) => {
                        observed_classes.insert(sub);
                    }
                    None => findings.push(format!(
                        "NEW panic class — composition {} ({}):\n--- spec ---\n{}--- panic ---\n{msg}\n\
                         Promotion: minimize by hand; pin it as a matrix containment row (annotations \
                         TOMLs + `bun run project_robustness.ts`) if the matrix can express the cell, \
                         else a tests/robustness/*.cddl catalog fixture with a rationale comment; \
                         ledger the finding in cddl-matrix/ROADMAP.md § findings; then add a \
                         KNOWN_PANIC_CLASSES entry citing that pin.",
                        c.id, c.desc, c.spec
                    )),
                }
            }
        }
    }
    println!(
        "recombination sweep: {} compositions -> ok={ok} graceful={graceful} panic={panics} in {:?}",
        comps.len(),
        t0.elapsed()
    );

    assert!(
        findings.is_empty(),
        "recombination sweep surfaced {} NEW panic class(es):\n\n{}",
        findings.len(),
        findings.join("\n\n")
    );

    // Vacuity floors — from the EXECUTED artifact, not the inputs. Current baseline
    // (1544 swept / 927 ok / 420 panic / 197 graceful); floors sit ~10% under so real shrinkage
    // fails loud while ingredient additions don't churn them. (Generic INSTANTIATION in bare member
    // position moved panic -> ok when the `TypeGroupname` group-entry arm was routed through
    // `generic_instance_or_new_type` — see tests/corpus/generic_call_member.cddl. Earlier, fixed BOOL
    // literals in member position moved panic -> ok when the deserialize `FixedValue::Bool` arm
    // landed — tests/corpus/fixed_bool_member.cddl — and the `[coords] / tstr` choice-arm and the
    // `gen<[coords]>` generic-arg shapes moved panic -> ok when member-position array-of-plain-group
    // promotion landed.)
    assert!(
        comps.len() >= 1400,
        "only {} compositions swept (floor 1400) — the composer rotted or ingredients went missing",
        comps.len()
    );
    assert!(
        ok >= 835,
        "only {ok} compositions generated ok (floor 835) — the ok set collapsed"
    );
    for (sub, cite) in KNOWN_PANIC_CLASSES {
        assert!(
            observed_classes.contains(sub),
            "ledgered panic class `{sub}` was never observed (pin: {cite}) — stale ledger entry or \
             a fix landed; prune/retarget the entry"
        );
    }
}

// ---- LAYER 2: batched compile + emitted-test execution ----------------------------------------------
/// Compositions whose GENERATION is ok but whose generated crate is KNOWN not to compile / not to
/// pass its emitted tests. Keyed by a substring of the composition `desc` (which embeds the axis
/// labels + involved feature ids). Every entry cites the committed pin that owns the class.
/// Excluded from batching (vacuity-guarded: each entry must match >= 1 ok composition, so a fixed
/// class flips loudly).
const LAYER2_KNOWN_BAD: &[(&str, &str)] = &[
    // (retired when `any` gained runtime support) `any` no longer "generates but does not compile" —
    // it lowers to the `AnyCbor` static-runtime type and the generated crate compiles across plain
    // / preserve / preserve+canonical. The former `filler=prelude.any` known-bad class is gone.
    // -- non-final `?` optional field in an array record: Deserialize impl not emitted (E0599) ----
    // (optional-LAST array fields compile and round-trip; the gap is the position.)
    (
        "shape=arr_record members=[optional,",
        "non-final `?` optional array-record field breaks compilation (E0599 from_cbor_bytes); cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "shape=arr_record members=[fixed_null,optional,optional]",
        "non-final `?` optional array-record field breaks compilation (E0599 from_cbor_bytes); cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    // -- array-rep group-choice arm with a `?` optional member (E0599 deserialize_as_embedded_group)
    (
        "shape=gchoice_arr members=[optional,",
        "array-rep group-choice arm with `?` optional member breaks compilation (E0599 deserialize_as_embedded_group); cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "shape=gchoice_arr members=[fixed_null,optional,optional]",
        "array-rep group-choice arm with `?` optional member breaks compilation (E0599 deserialize_as_embedded_group); cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    // Float-family table key domains are now a GRACEFUL generation-time rejection (floats have no
    // total order → no valid map key; pinned by tests/robustness/float_table_key.cddl and
    // float_table_key_composite.cddl), so those compositions never reach layer 2 — no entry needed.
    // A generic instantiation as a homogeneous array element (`[* pair<uint, tstr>]`) now registers
    // its generic instance and compiles + round-trips (pinned by tests/corpus/generic_array_element.cddl),
    // so those compositions execute in layer 2 — no entry needed.
    // A `.cbor` payload wrapping an anonymous array-of-plain-group (`bytes .cbor [coords]`) now
    // promotes the plain group to a Record struct and compiles + round-trips (pinned by
    // tests/corpus/cbor_wrapped_group_array.cddl), so those compositions execute — no entry needed.
    // A `.cbor` payload wrapping an int-valued table (`bytes .cbor { * tstr => int }`, reached via
    // both the `filler=memberkey.type1` and `filler=type2.map` axes) now emits its `Int` extern: the
    // reference walk covers emitted type aliases, so an alias-only `int` reference registers `Int`
    // (pinned by tests/corpus/int_alias.cddl), and those compositions compile + execute — no entry
    // needed. This retired the two undefined-`Int` entries that formerly rode here, one masked in the
    // default gate and surfaced only by the wasm leg's different batch boundaries (see the batch-masking
    // caveat on `LAYER2_RULES_PER_BATCH`).
    // -- tagged fixed value inside a map-rep group-choice arm (E0618) ------------------------------
    (
        "outer=garm_map inner=tag_content filler=type2.value",
        "tagged fixed value in a map-rep group-choice arm emits a call to a non-fn struct (E0618); cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    // -- wire-ambiguous type-choice arms: variant identity is unpreservable under first-match ------
    (
        "outer=choice_member filler=prelude.text",
        "wire-ambiguous choice arms (text / tstr) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "outer=choice_member filler=prelude.tstr",
        "wire-ambiguous choice arms (tstr / tstr) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "outer=choice_member filler=type.choice",
        "wire-ambiguous choice arms (uint / tstr / bytes / tstr) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "outer=choice_member inner=choice_member filler=ctl.ne.zero",
        "wire-ambiguous choice arms (int .ne 0 / tstr / tstr) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "outer=garm_arr inner=choice_member filler=rangeop.exclusive.int",
        "wire-ambiguous group-choice arms ([ ga: -10...10 / tstr // tstr ]) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    (
        "outer=cbor_payload filler=type.choice",
        "wire-ambiguous choice arms (bytes .cbor uint / tstr / bytes) fail emitted variant-identity round-trips; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
    // -- emitted-test minter / baseline decode gaps on nested shapes -------------------------------
    // (The former `outer=generic_arg inner=map_key filler=ctl.ne.zero` entry — the emit-tests minter
    // minting key 0 against an `int .ne 0` table domain — retired when its pinning composition
    // stopped reaching layer 2: the composition's map spelling carries NO occurrence indicator, so
    // it rejects gracefully at generation under the no-occurrence arrow-entry rejection (`5ef7ed0`);
    // the staleness sat latent until the next full-tier run (this sweep is full-tier-only). The
    // generic-instantiation REACH of that rejection is pinned by
    // `generic_arg_no_occurrence_table_rejects_gracefully`. The MINTER gap itself still stands
    // unpinned for `*`-spelled tables — cddl-matrix/ROADMAP.md § findings, recombination layer-2
    // entry.)
    (
        "outer=arr_mid inner=cbor_payload filler=prelude.float64",
        "a bytes .cbor float64 member fails its emitted baseline re-decode; cddl-matrix/ROADMAP.md § findings, recombination layer-2 entry",
    ),
];

// ---- generalized layer-2 runner (shared by every emission profile) --------------------------------
/// A layer-2 execution profile. One runner (`run_layer2_profile`) drives the whole shape:
/// classify under the profile in-process, batch the ok compositions, generate each batch with the
/// profile's flags, run the profile's cargo verb on the profile's generated crate, re-attribute
/// batch failures per member. Items 2/3 (json / wasm) plug in by building a different `Layer2Profile`
/// — no runner change — which is why the exec step is data-driven (`exec_args`/`crate_subdir`/
/// `cargo_subcmd`) rather than a hard-coded rust-`test` path.
struct Layer2Profile<'a> {
    /// Human profile name — labels the scratch root, the target dir, and the summary line.
    name: &'a str,
    /// Profile flags applied to BOTH in-process classification (`classify_all`) and out-of-process
    /// generation. This includes the explicit wasm mode, so the generation path being classified is
    /// the same path later generated.
    profile_args: &'a [&'a str],
    /// Generation-only extras (for example `--emit-tests=true`), appended AFTER `profile_args` when
    /// shelling out to the generator.
    exec_args: &'a [&'a str],
    /// Which generated crate to verify (`rust` for the emit-tests profiles, `wasm` for item 3).
    crate_subdir: &'a str,
    /// The cargo verb run in that crate (`test` executes emitted round-trips; `check` compile-only).
    cargo_subcmd: &'a str,
    /// Panic classes expected under THIS profile beyond the shared `KNOWN_PANIC_CLASSES` (which is
    /// always an allowlist here, never re-vacuity-guarded). Vacuity-guarded within this run.
    panic_ledger: &'a [(&'a str, &'a str)],
    /// Compile/execute known-bad classes specific to this profile. Vacuity-guarded within this run.
    known_bad: &'a [(&'a str, &'a str)],
    /// Whether to vacuity-guard the SHARED `LAYER2_KNOWN_BAD` in this run. TRUE only for the default
    /// profile (its home): a shared entry can legitimately match zero of a non-default profile's
    /// ok compositions because that profile's generation may PANIC for the class earlier, so the
    /// shared ledger is applied (as an exclusion) but not guarded in profile runs.
    guard_shared: bool,
    /// Ok-count floor, ~10% under the observed baseline for this profile.
    ok_floor: usize,
    /// Executed-count floor, ~10% under the observed baseline for this profile.
    executed_floor: usize,
}

/// Generate `spec` with the profile's generation flags, then run the profile's cargo verb on the
/// profile's generated crate. `Err(reason)` on any stage.
fn gen_and_exec(
    spec: &str,
    out: &std::path::Path,
    target_dir: &std::path::Path,
    p: &Layer2Profile,
    cache_run: &mut usize,
    cache_hit: &mut usize,
) -> Result<(), String> {
    let (profile_args, exec_args, crate_subdir, cargo_subcmd) =
        (p.profile_args, p.exec_args, p.crate_subdir, p.cargo_subcmd);
    let spec_path = out.with_extension("cddl");
    std::fs::create_dir_all(out.parent().unwrap()).ok();
    std::fs::write(&spec_path, spec).map_err(|e| e.to_string())?;
    let gen_out = tool_cmd("cargo")
        .args(["run", "--"])
        .arg(format!("--input={}", spec_path.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .args(profile_args)
        .args(exec_args)
        .output()
        .unwrap();
    if !gen_out.status.success() {
        return Err(format!(
            "generation failed\n{}",
            String::from_utf8_lossy(&gen_out.stderr)
        ));
    }
    let crate_dir = out.join(crate_subdir);
    if !crate_dir.exists() {
        return Err(format!("no {crate_subdir} crate at {crate_dir:?}"));
    }
    let argv_for_key = vec![
        format!("cwd={crate_subdir}"),
        "cargo".to_string(),
        cargo_subcmd.to_string(),
    ];
    let manifest_subpaths = vec![std::path::PathBuf::from(crate_subdir).join("Cargo.toml")];
    let mut run_output = None;
    let outcome = gate_cache::run_cached(
        "recombination.gen_and_exec",
        out.file_name().and_then(|n| n.to_str()).unwrap_or("batch"),
        out,
        &manifest_subpaths,
        &argv_for_key,
        || {
            let run = tool_cmd("cargo")
                .arg(cargo_subcmd)
                .current_dir(&crate_dir)
                .env("CARGO_TARGET_DIR", target_dir)
                .output()
                .unwrap();
            let success = run.status.success();
            run_output = Some(run);
            success
        },
    );
    *cache_run += outcome.ran();
    *cache_hit += outcome.cached();
    if outcome.success() {
        Ok(())
    } else {
        let run = run_output.unwrap();
        Err(format!(
            "cargo {cargo_subcmd} failed\n{}\n{}",
            String::from_utf8_lossy(&run.stdout),
            String::from_utf8_lossy(&run.stderr)
        ))
    }
}

/// The shared layer-2 body: classify → panic-ledger check → batch → generate+exec → re-attribute →
/// floors. Behaviour for a profile is entirely the `Layer2Profile` it is handed.
fn run_layer2_profile(p: &Layer2Profile) {
    let t0 = std::time::Instant::now();
    let comps = compositions();
    let outcomes = classify_all(&comps, p.profile_args);

    // Classification under a non-default profile PANICS for classes that are ok/graceful under
    // default. Every panic must be in `KNOWN_PANIC_CLASSES` (allowlist) OR the profile's own
    // `panic_ledger`; anything else is a NEW finding. The profile ledger is vacuity-guarded.
    let mut panic_findings: Vec<String> = Vec::new();
    let mut observed_panic_classes: BTreeSet<&str> = BTreeSet::new();
    let mut ok_comps: Vec<&Composition> = Vec::new();
    let mut graceful = 0usize;
    let mut panics = 0usize;
    for (c, o) in comps.iter().zip(outcomes.iter()) {
        match o {
            Outcome::Ok => ok_comps.push(c),
            Outcome::Graceful(_) => graceful += 1,
            Outcome::Panic(msg) => {
                panics += 1;
                // Shared allowlist is applied but not vacuity-guarded in profile runs; the profile's
                // own panic ledger is guarded (observed set below).
                if let Some((sub, _)) = p.panic_ledger.iter().find(|(sub, _)| msg.contains(sub)) {
                    observed_panic_classes.insert(sub);
                } else if !KNOWN_PANIC_CLASSES.iter().any(|(sub, _)| msg.contains(sub)) {
                    panic_findings.push(format!(
                        "NEW panic class under {} profile — composition {} ({}):\n--- spec ---\n{}--- panic ---\n{msg}\n\
                         Promotion: minimize by hand; pin it (matrix row / tests/robustness/ / \
                         tests/corpus/) or cite an existing ROADMAP § findings entry; ledger it in \
                         cddl-matrix/ROADMAP.md § findings; add a profile panic-ledger entry citing the pin.",
                        p.name, c.id, c.desc, c.spec
                    ));
                }
            }
        }
    }
    assert!(
        panic_findings.is_empty(),
        "recombination {} layer 2 surfaced {} NEW panic class(es):\n\n{}",
        p.name,
        panic_findings.len(),
        panic_findings.join("\n\n")
    );
    for (sub, cite) in p.panic_ledger {
        assert!(
            observed_panic_classes.contains(sub),
            "{} panic-ledger entry `{sub}` matched no composition (pin: {cite}) — stale entry",
            p.name
        );
    }
    assert!(
        ok_comps.len() >= p.ok_floor,
        "only {} ok compositions reached {} layer 2 (floor {})",
        ok_comps.len(),
        p.name,
        p.ok_floor
    );

    // Exclusion set = shared LAYER2_KNOWN_BAD ∪ profile's own known_bad. The shared ledger is
    // guarded only for the default profile (`guard_shared`); the profile's own ledger always is.
    let mut shared_hits: BTreeMap<&str, usize> = BTreeMap::new();
    let mut profile_hits: BTreeMap<&str, usize> = BTreeMap::new();
    let mut executable: Vec<&Composition> = Vec::new();
    for c in &ok_comps {
        if let Some((sub, _)) = LAYER2_KNOWN_BAD
            .iter()
            .find(|(sub, _)| c.desc.contains(sub))
        {
            *shared_hits.entry(sub).or_default() += 1;
        } else if let Some((sub, _)) = p.known_bad.iter().find(|(sub, _)| c.desc.contains(sub)) {
            *profile_hits.entry(sub).or_default() += 1;
        } else {
            executable.push(c);
        }
    }
    if p.guard_shared {
        for (sub, cite) in LAYER2_KNOWN_BAD {
            assert!(
                shared_hits.contains_key(sub),
                "LAYER2_KNOWN_BAD entry `{sub}` matched no ok composition (pin: {cite}) — stale entry"
            );
        }
    }
    for (sub, cite) in p.known_bad {
        assert!(
            profile_hits.contains_key(sub),
            "{} known-bad entry `{sub}` matched no ok composition (pin: {cite}) — stale entry",
            p.name
        );
    }

    // Batch: greedy fill up to LAYER2_RULES_PER_BATCH rules per batch, deterministic order.
    let mut batches: Vec<Vec<&Composition>> = Vec::new();
    let mut cur: Vec<&Composition> = Vec::new();
    let mut cur_rules = 0usize;
    for c in &executable {
        if cur_rules + c.rules > LAYER2_RULES_PER_BATCH && !cur.is_empty() {
            batches.push(std::mem::take(&mut cur));
            cur_rules = 0;
        }
        cur.push(c);
        cur_rules += c.rules;
    }
    if !cur.is_empty() {
        batches.push(cur);
    }

    // Per-profile scratch root + target dir: keeps profiles from clobbering each other and (for the
    // serde/schemars-pulling json profile) stops feature-resolution thrash invalidating the default
    // cache.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_recomb_{}_{:016x}",
        p.name,
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let target_dir = root.join("target");

    let mut findings: Vec<String> = Vec::new();
    let mut executed = 0usize;
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;
    let mut run_batch = |spec: &str, out: &std::path::Path| {
        gen_and_exec(spec, out, &target_dir, p, &mut cache_run, &mut cache_hit)
    };
    for (bi, batch) in batches.iter().enumerate() {
        let spec: String = batch.iter().map(|c| c.spec.as_str()).collect();
        let out = root.join(format!("batch{bi:03}"));
        match run_batch(&spec, &out) {
            Ok(()) => executed += batch.len(),
            Err(batch_reason) => {
                // Attribute: rerun each member individually.
                let mut attributed = false;
                for c in batch {
                    let mout = root.join(format!("batch{bi:03}_{}", c.id));
                    if let Err(reason) = run_batch(&c.spec, &mout) {
                        attributed = true;
                        findings.push(format!(
                            "NEW layer-2 finding under {} profile — composition {} ({}):\n--- spec ---\n{}--- failure ---\n{reason}\n\
                             Promotion: minimize by hand; pin it (matrix row / tests/robustness/ / \
                             tests/corpus/); ledger it in cddl-matrix/ROADMAP.md § findings; add a \
                             profile known-bad entry citing the pin.",
                            p.name, c.id, c.desc, c.spec
                        ));
                    } else {
                        executed += 1;
                    }
                }
                if !attributed {
                    findings.push(format!(
                        "batch {bi} failed but every member passed individually — a CROSS-COMPOSITION \
                         interaction (this is itself a finding; bisect the batch):\n{batch_reason}"
                    ));
                }
            }
        }
    }
    let _ = std::fs::remove_dir_all(&root);
    if gate_cache::enabled() {
        println!(
            "recombination {} gate-cache: {cache_run} run, {cache_hit} cached",
            p.name
        );
    }

    println!(
        "recombination {} layer 2: classified ok={} graceful={} panic={}; {} batches / {} compositions executed ({} known-bad excluded) in {:?}",
        p.name,
        ok_comps.len(),
        graceful,
        panics,
        batches.len(),
        executed,
        ok_comps.len() - executable.len(),
        t0.elapsed()
    );
    assert!(
        findings.is_empty(),
        "recombination {} layer 2 surfaced {} finding(s):\n\n{}",
        p.name,
        findings.len(),
        findings.join("\n\n")
    );
    assert!(
        executed >= p.executed_floor,
        "only {executed} compositions executed in {} layer 2 (floor {}) — batching rotted",
        p.name,
        p.executed_floor
    );
}

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): batch layer 1's ok compositions into
/// ~`LAYER2_RULES_PER_BATCH`-rule specs, generate each with `--emit-tests=true --wasm=false`
/// (default profile), and `cargo test` the generated rust crate (shared `CARGO_TARGET_DIR`, the
/// `feature_corpus_compiles` pattern). A batch failure is re-attributed by rerunning its members
/// individually; a failing member not matching `LAYER2_KNOWN_BAD` is a NEW finding. This is the
/// DEFAULT profile's thin call into the shared `run_layer2_profile` runner.
///
/// Run: `cargo test --bin cddl-codegen recombination_crates_execute -- --exact --ignored --nocapture`.
#[test]
#[ignore]
fn recombination_crates_execute() {
    run_layer2_profile(&Layer2Profile {
        name: "default",
        profile_args: &["--wasm=false"],
        exec_args: &["--emit-tests=true"],
        crate_subdir: "rust",
        cargo_subcmd: "test",
        panic_ledger: &[],
        known_bad: &[],
        guard_shared: true,
        ok_floor: 750,
        executed_floor: 700,
    });
}

// ---- preserve profile: panic ledger + known-bad ledger + the escalation gate ----------------------
/// Panic classes that appear when classifying under `--preserve-encodings=true` but are ok/graceful
/// under the default profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` (which stays the
/// allowlist); a preserve panic matching neither is a NEW finding. Each entry cites an existing
/// `cddl-matrix/ROADMAP.md` § findings entry (stable title, never a position). Vacuity-guarded in
/// `recombination_preserve_crates_execute`.
const PRESERVE_ONLY_PANIC_CLASSES: &[(&str, &str)] = &[
    (
        "preserve_encodings is not implemented for float",
        "native float in member / element / tag / choice-arm position under --preserve-encodings \
         (the deserialize path has no encoding metadata for f16/f32/f64); cddl-matrix/ROADMAP.md § \
         findings, `float16 / float-choice aliases unsupported ... Under --preserve-encodings the float gap is positional` entry",
    ),
    (
        "!cli.preserve_encodings @ src/generation/enums.rs @ fn cddl_codegen::generation::enums::generate_enum",
        "a CBOR tag over a type-choice / enum / group-choice (`#6.11(int / tstr)`, `#6.11(<enum>)`) hits \
         the tagged-enum serialize path's explicit `assert!(!cli.preserve_encodings)` — the per-variant \
         encoding metadata has no home on the enum; cddl-matrix/ROADMAP.md § findings, \
         `A CBOR tag over a type-choice enum is unimplemented under --preserve-encodings` entry",
    ),
    // (retired when `any` gained runtime support) a CBOR tag wrapping `any` (`#6.11(any)`) under
    // --preserve-encodings no longer panics building the tag's encoding field: `any` lowers to the
    // self-carried `AnyCbor` runtime type (contributes ZERO owner encoding fields via
    // `encoding_fields_impl`'s `Root(Any)` arm), so the tag's encoding var mints normally. Verified
    // byte-exact round-trip through a generated preserve+canonical crate.
];

/// Preserve-profile compile/round-trip known-bad classes (generation is ok under preserve, the
/// DEFAULT crate compiles + round-trips, but the preserve crate fails `cargo build`). Desc-substring
/// keyed, each citing its pin; vacuity-guarded in `recombination_preserve_crates_execute`. The shared
/// `LAYER2_KNOWN_BAD` also applies (as an exclusion, un-guarded here).
// Empty: the two former compile-class families (E0308 tag/`.cbor`-wrapped constrained-int deserialize
// tuple arity; E0382 composite map-key move-then-reuse) are both FIXED in `generate_deserialize`, and
// their fixed behavior is pinned by the `tagged_constrained_int` / `composite_map_key` corpus fixtures
// (compile + round-trip under preserve). The freed compositions batch back into the preserve gate. New
// preserve-only compile classes would be caught by that gate as NEW findings and re-ledgered here.
const LAYER2_PRESERVE_KNOWN_BAD: &[(&str, &str)] = &[];

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): the PRESERVE escalation of layer 2.
/// Classifies every composition under `--preserve-encodings=true`, batches the preserve-ok ones,
/// generates `--preserve-encodings=true --emit-tests=true --wasm=false`, and `cargo test`s the rust
/// crate — the leg that would have caught the preserve-only E0308 on tag-wrapped fixed-value members
/// (`[v: #6.1(null)]`) that passed every default-profile gate and was found only by review.
///
/// Preserve panics for classes that are ok/graceful under default (floats as members; tag over a
/// type-choice enum; tag wrapping `any`) — those are in `PRESERVE_ONLY_PANIC_CLASSES` and never
/// reach execution; a NEW preserve panic fails loudly. (Optional non-float fixed-value members now
/// generate and round-trip via a `bool` presence field under both profiles — the former
/// encoding-less optional-fixed preserve assert is gone; the composition set still has no
/// optional-FIXED member kind, so adding one is the extended-member-kind residual in
/// tests/TESTING_ROADMAP.md.) Profile flags are sourced from
/// `crate::tests::ALL_PROFILES` by name (asserted found), never re-hard-coded.
///
/// NAMING/SELECTION GOTCHA: this name must NOT contain the `recombination_crates_execute` needle
/// (cargo's substring test selection would cross-select) — hence `recombination_preserve_crates_execute`.
/// The check.ts gate passes `--exact` for both gates.
///
/// Run: `cargo test --bin cddl-codegen recombination_preserve_crates_execute -- --exact --ignored --nocapture`.
#[test]
#[ignore]
fn recombination_preserve_crates_execute() {
    let (name, preserve_args) = crate::tests::ALL_PROFILES
        .iter()
        .find(|(name, _)| *name == "preserve")
        .expect("`preserve` profile missing from crate::tests::ALL_PROFILES");
    let mut profile_args = preserve_args.to_vec();
    profile_args.push("--wasm=false");
    run_layer2_profile(&Layer2Profile {
        name,
        profile_args: &profile_args,
        exec_args: &["--emit-tests=true"],
        crate_subdir: "rust",
        cargo_subcmd: "test",
        panic_ledger: PRESERVE_ONLY_PANIC_CLASSES,
        known_bad: LAYER2_PRESERVE_KNOWN_BAD,
        guard_shared: false,
        // Observed baseline: 856 preserve-ok / 827 executed (29 known-bad excluded); floors ~10% under.
        ok_floor: 770,
        executed_floor: 735,
    });
}

// ---- json profile: panic ledger + known-bad ledger + the escalation gate --------------------------
/// Panic classes that appear when classifying under
/// `--json-serde-derives=true --json-schema-export=true` but are ok/graceful under the default
/// profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` allowlist; a json panic matching neither
/// is a NEW finding. Each non-empty entry must cite an existing `cddl-matrix/ROADMAP.md` § findings
/// entry or another committed stable pin. Vacuity-guarded in
/// `recombination_json_crates_execute`.
const JSON_ONLY_PANIC_CLASSES: &[(&str, &str)] = &[];

/// Json-profile compile/round-trip known-bad classes. Desc-substring keyed, each citing its pin;
/// vacuity-guarded in `recombination_json_crates_execute`. The shared `LAYER2_KNOWN_BAD` also
/// applies (as an exclusion, un-guarded here).
const LAYER2_JSON_KNOWN_BAD: &[(&str, &str)] = &[];

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): the JSON escalation of layer 2.
/// Classifies every composition under the `json` profile from `crate::tests::ALL_PROFILES`
/// (`--json-serde-derives=true --json-schema-export=true`), batches the json-ok ones, generates
/// `--json-serde-derives=true --json-schema-export=true --emit-tests=true --wasm=false`, and
/// `cargo test`s the generated rust crate. This catches rust-crate failures that only appear once
/// serde derives / schemars schema derives are emitted, while also executing the emitted CBOR tests.
///
/// With `--json-schema-export=true --wasm=false`, generation also emits an independent
/// `wasm/json-gen` crate outside the rust crate (`rust/` and `wasm/json-gen/` output directories).
/// This leg deliberately does not `cargo check` or run that crate: the profile's layer-2 oracle is
/// rust-crate serde/schemars compilation plus emitted-test execution, and the json-gen crate is
/// covered by the existing json profile compile/schema gates (`feature_corpus_compiles`,
/// `json`/`json_float`, and the package-json pipeline) rather than by this recombination gate.
/// Profile flags are sourced by name (asserted found), never re-hard-coded.
///
/// NAMING/SELECTION GOTCHA: this name must NOT contain the `recombination_crates_execute` needle
/// (cargo's substring test selection would cross-select) — hence `recombination_json_crates_execute`.
/// The check.ts gate passes `--exact` for all layer-2 gates.
///
/// Run: `cargo test --bin cddl-codegen recombination_json_crates_execute -- --exact --ignored --nocapture`.
#[test]
#[ignore]
fn recombination_json_crates_execute() {
    let (name, json_args) = crate::tests::ALL_PROFILES
        .iter()
        .find(|(name, _)| *name == "json")
        .expect("`json` profile missing from crate::tests::ALL_PROFILES");
    let mut profile_args = json_args.to_vec();
    profile_args.push("--wasm=false");
    run_layer2_profile(&Layer2Profile {
        name,
        profile_args: &profile_args,
        exec_args: &["--emit-tests=true"],
        crate_subdir: "rust",
        cargo_subcmd: "test",
        panic_ledger: JSON_ONLY_PANIC_CLASSES,
        known_bad: LAYER2_JSON_KNOWN_BAD,
        guard_shared: false,
        // Observed baseline: 927 json-ok / 897 executed (30 known-bad excluded); floors ~10% under.
        ok_floor: 835,
        executed_floor: 808,
    });
}

// ---- wasm profile: panic ledger + known-bad ledger + the escalation gate --------------------------
/// Panic classes that appear when classifying under `--wasm=true` but are ok/graceful under the
/// default `--wasm=false` profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` allowlist; a wasm
/// panic matching neither is a NEW finding. Each non-empty entry must cite an existing
/// `cddl-matrix/ROADMAP.md` § findings entry or another committed stable pin. Vacuity-guarded in
/// `recombination_wasm_crates_check`.
// Empty: the former class (a CBOR tag wrapping a table panicked `codegen_table_type`'s wasm-only
// `assert!(tag.is_none())`) is FIXED — the wasm wrapper is accessors-only and delegates ALL
// serialization (including the tag) to the rust crate's type, so it carries no tag logic and the stale
// assert (plus the now-unused `tag` parameter) was removed. Pinned by the `tagged_table` corpus fixture
// (`#6.11({ * tstr => uint })`, wasm crate compiles across all profiles via `feature_corpus_compiles`;
// rust-side tag round-trip via the tag-writing/`TagMismatch` serialization). The freed compositions
// batch back into the wasm gate; a new wasm-only panic class would be caught there as a NEW finding.
const WASM_ONLY_PANIC_CLASSES: &[(&str, &str)] = &[];

/// Wasm-profile compile known-bad classes. Desc-substring keyed, each citing its pin;
/// vacuity-guarded in `recombination_wasm_crates_check`. The shared `LAYER2_KNOWN_BAD` also applies
/// as an un-guarded exclusion.
// Empty: the former class (a `.cbor` payload wrapping a bignint-key table emitted `pub type X = MapKToV`
// naming a wrapper class no one minted → wasm `cannot find type MapPreludeBignintToU64`) is FIXED — the
// wasm structural-wrapper mint loop now also walks each wasm-emitted plain-alias base type, so a Map
// reachable ONLY through an alias (never a rust struct) gets its wrapper minted. Pinned by the
// `cbor_bignint_table` corpus fixture (wasm crate compiles via `feature_corpus_compiles`). The freed
// compositions batch back into the wasm gate; a new wasm-only compile class would be caught there as a
// NEW finding. (The undefined-`Int` `.cbor`-payload-table class is unrelated and profile-independent —
// it lives in the shared LAYER2_KNOWN_BAD with the batch-masking note.)
const LAYER2_WASM_KNOWN_BAD: &[(&str, &str)] = &[];

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): the WASM escalation of layer 2.
/// Classifies every composition under `--wasm=true`, batches the wasm-ok ones, generates
/// `--wasm=true` without emitted tests, and `cargo check`s the generated `wasm/` crate. The wasm
/// crate depends on the generated rust crate by path, so rust-side compile failures surface through
/// the single check. This is a fuzz-recombination cross-check of the wasm emission path; the
/// wasm-ABI matrix remains the systematic per-shape wasm surface owner.
///
/// Run: `cargo test --bin cddl-codegen recombination_wasm_crates_check -- --exact --ignored --nocapture`.
#[test]
#[ignore]
fn recombination_wasm_crates_check() {
    run_layer2_profile(&Layer2Profile {
        name: "wasm",
        profile_args: &["--wasm=true"],
        exec_args: &[],
        crate_subdir: "wasm",
        cargo_subcmd: "check",
        panic_ledger: WASM_ONLY_PANIC_CLASSES,
        known_bad: LAYER2_WASM_KNOWN_BAD,
        guard_shared: false,
        // Observed baseline: 926 wasm-ok / 897 checked (29 known-bad excluded); floors ~10% under.
        ok_floor: 830,
        executed_floor: 803,
    });
}

// ---- ledger hygiene ---------------------------------------------------------------------------------
/// Ledger-key SHAPE floor — the mechanical guard for a review-caught class: a panic-ledger key that
/// pins ONLY the panicking file/function with no message component (a bare
/// `fn cddl_codegen::generation::<symbol>` substring) silently absorbs every FUTURE distinct panic
/// class in that function, because the normalized panic format is `<msg> @ <file> @ fn <symbol>`
/// and a symbol-only substring matches all of them. Panic-ledger keys must therefore lead with
/// message text (a message-only key is fine — it is the original convention; message+file+fn is the
/// tightest). Layer-2 known-bad keys have the analogous hazard in the desc space: a key with no
/// desc-axis label (`shape=`/`outer=`/`inner=`/`filler=`) could absorb unrelated compositions.
/// Cheap and always-on; vacuity-immune (empty profile ledgers simply contribute no keys).
#[test]
fn ledger_key_shape_floor() {
    let panic_ledgers: &[(&str, &[(&str, &str)])] = &[
        ("KNOWN_PANIC_CLASSES", KNOWN_PANIC_CLASSES),
        ("PRESERVE_ONLY_PANIC_CLASSES", PRESERVE_ONLY_PANIC_CLASSES),
        ("JSON_ONLY_PANIC_CLASSES", JSON_ONLY_PANIC_CLASSES),
        ("WASM_ONLY_PANIC_CLASSES", WASM_ONLY_PANIC_CLASSES),
    ];
    for (name, ledger) in panic_ledgers {
        for (key, cite) in *ledger {
            let k = key.trim_start();
            assert!(
                !k.is_empty()
                    && !k.starts_with("fn ")
                    && !k.starts_with("src/")
                    && !k.starts_with('@'),
                "{name} key `{key}` pins only a file/function (no message component) — it would \
                 absorb every future distinct panic class at that site; lead with the message text \
                 (cite: {cite})"
            );
        }
    }
    let known_bad_ledgers: &[(&str, &[(&str, &str)])] = &[
        ("LAYER2_KNOWN_BAD", LAYER2_KNOWN_BAD),
        ("LAYER2_PRESERVE_KNOWN_BAD", LAYER2_PRESERVE_KNOWN_BAD),
        ("LAYER2_JSON_KNOWN_BAD", LAYER2_JSON_KNOWN_BAD),
        ("LAYER2_WASM_KNOWN_BAD", LAYER2_WASM_KNOWN_BAD),
    ];
    for (name, ledger) in known_bad_ledgers {
        for (key, cite) in *ledger {
            assert!(
                ["shape=", "outer=", "inner=", "filler="]
                    .iter()
                    .any(|axis| key.contains(axis)),
                "{name} key `{key}` carries no desc-axis label (shape=/outer=/inner=/filler=) — \
                 too generic, could absorb unrelated compositions (cite: {cite})"
            );
        }
    }
}
