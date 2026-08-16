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
//!      `recombination_wasm_crates_check` (`--wasm=true`, `cargo check` the wasm crate) — execute the
//!      profile's `ok` compositions under TWO deterministic, decorrelated greedy batch plans
//!      (~`LAYER2_RULES_PER_BATCH` rules/batch; authored root/aux-rule names are collision-free by
//!      construction, while generated names remain a product invariant; per-profile-and-plan scratch
//!      + `CARGO_TARGET_DIR`). Any batch failure is re-attributed by rerunning members individually;
//!        a failing member whose desc matches neither the shared `LAYER2_KNOWN_BAD` nor the profile's
//!        own ledger is a NEW finding, and a classification panic outside `KNOWN_PANIC_CLASSES` ∪ the
//!        profile's panic ledger likewise. Target < 10 min per gate.
//!
//! Determinism: a fixed seed + splitmix64; enumeration is a systematic cross-product where cheap
//! and seeded sampling where the product explodes (budget constants below). The sweep asserts two
//! back-to-back enumerations are identical, and floors are derived from the EXECUTED artifact
//! (swept/ok counts + every ledger entry actually observed), so a rotted composer or an
//! accidentally-empty ingredients file fails loud rather than passing vacuously.

use crate::cli::Cli;
use crate::tests::gate_cache;
use crate::tests::identifier_hazard_tests::hazards;
use crate::tests::integration_tests::{checkout_hash, codegen_cmd, tool_cmd};
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
/// Every profile runs both the natural greedy grouping and a deterministic transpose/re-batch of it.
/// The second plan separates most natural batchmates, breaking most opportunities for a missing
/// crate-global definition to be supplied by a batchmate. The fixed undefined-`Int` predecessor is
/// pinned by `tests/corpus/int_alias.cddl`. Two plans are deliberately not the singleton oracle: a
/// standalone-proven class remains ledger-worthy if its providers share both batches.
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
        // PARENTHESIZED, which is what makes this template compose with ITSELF. RFC 8610's grammar
        // is `type1 = type2 [S (rangeop / ctlop) S type2]`, so a control operator's right-hand side
        // is a `type2`: the unparenthesized `bytes .cbor bytes .cbor uint` is not merely unparsed by
        // our front end, it is illegal CDDL, and the sweep spent that slot on a parse error instead
        // of on the generator. Wrapping the hole makes the RHS a `type2` for every filler.
        //
        // It also FIXES THE MEANING of fillers that are bare type-level expressions, which is a
        // deliberate re-baselining rather than a side effect: unparenthesized, `bytes .cbor x / tstr`
        // parses as a CHOICE whose first arm carries the payload, so the composition was measuring
        // `role.choice-member` while claiming `role.cbor-payload`. Parenthesized, the choice IS the
        // payload — which is the cell this template's role names.
        build: |h, _p, _a| format!("bytes .cbor ({h})"),
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
/// One member shape inside a composite construct: `(name, member template, aux-rule BODY)`.
/// `%K%`/`%K2%` are field-name slots; `%F%` is a filler-expression slot (only `filler` uses it);
/// `%A%` is the kind's OWN aux rule's name. The known-outcome kinds are deliberately kept in
/// (fixed bool = pinned panic, zero-star = pinned graceful reject): the sweep must OBSERVE the
/// pinned classes, that's the ledger's anti-vacuity floor.
///
/// The aux-rule slot is the `filler` mechanism narrowed to a kind's own fixed rule: `filler` DRAWS
/// its aux rules from the ingredients and renames them for collision-proofing, while a `%A%` kind
/// spells one rule whose name is built from the composition prefix AND the member index
/// (`rc0123_ta0`), so the same kind appearing twice in a pair or triple gets two distinct rules
/// rather than one shared or one colliding. Empty body = no aux rule, which is every other kind.
const MEMBER_KINDS: &[(&str, &str, &str)] = &[
    ("fixed_uint", "%K%: 5", ""),
    ("fixed_text", "%K%: \"x\"", ""),
    ("fixed_bool", "%K%: true", ""),
    ("fixed_null", "%K%: null", ""),
    ("scalar", "%K%: uint", ""),
    ("optional", "? %K%: uint", ""),
    ("zero_star", "* %K%: uint", ""),
    ("inline_group", "(%K%: uint, %K2%: tstr)", ""),
    ("filler", "%K%: %F%", ""),
    // A tag head over a NAMED `T / null` rule — the option collapse under a tag, spelled the way
    // the anonymous-choice-under-a-tag refusal tells a consumer to spell it (name the choice, then
    // tag the name). It is not reachable by composing the kinds above: the collapse needs a named
    // rule, and no other kind mints one. The gap was consequential — a `#6.n(T / null)` under
    // --preserve-encodings dropped the tag head width on the null payload, exit 0 with a red
    // emitted round-trip, and no composition spelled it, so the preserve layer-2 sweep was blind
    // to it (fixed with tests/corpus/tagged_nullable.cddl, which pins the shape; this row is what
    // keeps the whole member-kind cross-product honest about it).
    ("tagged_optional", "%K%: #6.10(%A%)", "uint / null"),
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

/// A deterministic greedy partition of `items`: preserve their supplied order and start the next
/// batch only when adding an item would cross `rule_budget`. An oversized item is intentionally a
/// one-item batch; the corpus controls item size and this helper must still preserve membership.
fn greedy_rule_batches<'a, T>(
    items: &[&'a T],
    rule_budget: usize,
    rules: impl Fn(&T) -> usize,
) -> Vec<Vec<&'a T>> {
    assert!(rule_budget > 0, "layer-2 rule budget must be nonzero");
    let mut batches = Vec::new();
    let mut current = Vec::new();
    let mut current_rules = 0usize;
    for &item in items {
        let item_rules = rules(item);
        if current_rules + item_rules > rule_budget && !current.is_empty() {
            batches.push(std::mem::take(&mut current));
            current_rules = 0;
        }
        current.push(item);
        current_rules += item_rules;
    }
    if !current.is_empty() {
        batches.push(current);
    }
    batches
}

/// One labelled layer-2 batch plan over the exact executable composition references.
struct Layer2BatchPlan<'a> {
    label: &'static str,
    batches: Vec<Vec<&'a Composition>>,
}

/// Build the two deterministic layer-2 plans. The transpose is deliberately derived from the
/// NATURAL batches, rather than a random permutation: it walks position zero of every natural
/// batch, then position one, and so on, before greedily re-batching that fixed order.
fn layer2_batch_plans<'a>(
    executable: &[&'a Composition],
    rule_budget: usize,
) -> [Layer2BatchPlan<'a>; 2] {
    let natural = greedy_rule_batches(executable, rule_budget, |c| c.rules);
    let max_len = natural.iter().map(Vec::len).max().unwrap_or(0);
    let transposed_order: Vec<&Composition> = (0..max_len)
        .flat_map(|position| {
            natural
                .iter()
                .filter_map(move |batch| batch.get(position).copied())
        })
        .collect();
    let transposed = greedy_rule_batches(&transposed_order, rule_budget, |c| c.rules);
    [
        Layer2BatchPlan {
            label: "natural",
            batches: natural,
        },
        Layer2BatchPlan {
            label: "transposed",
            batches: transposed,
        },
    ]
}

fn batch_membership<'a>(batches: &[Vec<&'a Composition>]) -> BTreeSet<&'a str> {
    batches
        .iter()
        .flatten()
        .map(|composition| composition.id.as_str())
        .collect()
}

fn batchmate_pairs<'a>(batches: &[Vec<&'a Composition>]) -> BTreeSet<(&'a str, &'a str)> {
    batches
        .iter()
        .flat_map(|batch| {
            batch.iter().enumerate().flat_map(move |(left, a)| {
                batch
                    .iter()
                    .skip(left + 1)
                    .map(move |b| (a.id.as_str(), b.id.as_str()))
            })
        })
        .collect()
}

/// Ensure both plans are real executable-corpus partitions, rather than a second pass that has
/// accidentally converged on the natural grouping. This deliberately asserts relationships, not
/// an incidental batch count: adding/reweighting compositions may change that count legitimately.
fn assert_layer2_batch_plan_integrity(
    plans: &[Layer2BatchPlan<'_>; 2],
    executable: &[&Composition],
) {
    let expected: BTreeSet<&str> = executable.iter().map(|c| c.id.as_str()).collect();
    for plan in plans {
        assert_eq!(
            batch_membership(&plan.batches),
            expected,
            "{} layer-2 plan changed executable membership",
            plan.label
        );
        assert_eq!(
            plan.batches.iter().flatten().count(),
            expected.len(),
            "{} layer-2 plan duplicated an executable composition",
            plan.label
        );
        assert!(
            plan.batches
                .iter()
                .all(|batch| batch.iter().map(|c| c.rules).sum::<usize>() <= LAYER2_RULES_PER_BATCH),
            "{} layer-2 plan crossed the rule budget",
            plan.label
        );
    }
    assert!(
        plans[0].batches.len() > 1,
        "layer-2 executable corpus collapsed to one natural batch; the decorrelation detector is vacuous"
    );
    let natural_pairs = batchmate_pairs(&plans[0].batches);
    let transposed_pairs = batchmate_pairs(&plans[1].batches);
    let split_pairs = natural_pairs
        .iter()
        .filter(|pair| !transposed_pairs.contains(pair))
        .count();
    assert!(
        split_pairs * 2 > natural_pairs.len(),
        "layer-2 transpose split only {split_pairs}/{} natural batchmate pairs; expected a majority so the decorrelation detector remains material",
        natural_pairs.len()
    );
}

#[test]
fn layer2_batch_plans_are_deterministic_budgeted_and_decorrelated() {
    let synthetic: Vec<Composition> = [("a", 2usize), ("b", 2), ("c", 2), ("d", 2), ("e", 2)]
        .into_iter()
        .map(|(id, rules)| Composition {
            id: id.to_owned(),
            desc: id.to_owned(),
            spec: String::new(),
            rules,
        })
        .collect();
    let refs: Vec<&Composition> = synthetic.iter().collect();
    let first = layer2_batch_plans(&refs, 4);
    let second = layer2_batch_plans(&refs, 4);

    assert_eq!(first[0].label, "natural");
    assert_eq!(first[1].label, "transposed");
    assert_eq!(
        first
            .iter()
            .map(|plan| {
                plan.batches
                    .iter()
                    .map(|batch| batch.iter().map(|c| c.id.as_str()).collect::<Vec<_>>())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>(),
        second
            .iter()
            .map(|plan| {
                plan.batches
                    .iter()
                    .map(|batch| batch.iter().map(|c| c.id.as_str()).collect::<Vec<_>>())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>(),
        "plans must be deterministic"
    );
    let expected: BTreeSet<&str> = ["a", "b", "c", "d", "e"].into_iter().collect();
    for plan in &first {
        assert_eq!(batch_membership(&plan.batches), expected);
        assert_eq!(
            plan.batches.iter().flatten().count(),
            expected.len(),
            "{} plan must contain every composition exactly once",
            plan.label
        );
        assert!(
            plan.batches
                .iter()
                .all(|batch| batch.iter().map(|c| c.rules).sum::<usize>() <= 4)
        );
    }
    let natural_pairs = batchmate_pairs(&first[0].batches);
    let transposed_pairs = batchmate_pairs(&first[1].batches);
    assert!(natural_pairs.contains(&("a", "b")));
    assert!(
        !transposed_pairs.contains(&("a", "b")),
        "a known natural batchmate pair must be split by the transpose"
    );
}

#[test]
fn layer2_actual_default_executable_corpus_has_two_decorrelated_plans() {
    let corpus = compositions();
    let outcomes = classify_all(&corpus, &["--wasm=false"]);
    let executable: Vec<&Composition> = corpus
        .iter()
        .zip(outcomes.iter())
        .filter_map(|(composition, outcome)| {
            (matches!(outcome, Outcome::Ok)
                && !LAYER2_KNOWN_BAD
                    .iter()
                    .any(|(key, _)| composition.desc.contains(key)))
            .then_some(composition)
        })
        .collect();
    let plans = layer2_batch_plans(&executable, LAYER2_RULES_PER_BATCH);
    assert_layer2_batch_plan_integrity(&plans, &executable);
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
    kind: &(&str, &str, &str),
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
    if !kind.2.is_empty() {
        // Per-(composition, member index) so the same kind twice in one composition mints two
        // rules; `rc<num>_*` keeps it inside the aux-rule namespace batching already relies on.
        // Deterministic (no rng draw), so adding a `%A%` kind cannot re-roll the sampled draws
        // of the members beside it.
        let aux_name = format!("{prefix}_ta{idx}");
        sink.aux.push(format!("{aux_name} = {}", kind.2));
        m = m.replace("%A%", &aux_name);
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
            let ks: Vec<&(&str, &str, &str)> = (0..3)
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

/// The normalized form every ledger key is written against: `<whitespace-collapsed message> @
/// <file> @ fn <symbol>`. Shared by the sweep's capturing hook and the citation detector so a key
/// that matches in one matches in the other.
fn normalize_panic(info: &std::panic::PanicHookInfo) -> String {
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
    // Symbolication happens ONLY on a panic, never on the ok path.
    let symbol = production_frame_symbol(&std::backtrace::Backtrace::force_capture().to_string());
    format!(
        "{} @ {file} @ fn {symbol}",
        msg.split_whitespace().collect::<Vec<_>>().join(" ")
    )
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
                LAST_PANIC.with(|p| *p.borrow_mut() = Some(normalize_panic(info)));
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
/// committed pin (a matrix fixture row, a `tests/robustness/` fixture, or a `cddl-matrix/roadmap.toml`
/// findings-ledger entry) — cite stable identifiers, never positions. Every entry is also asserted
/// OBSERVED by the sweep (stale-pin guard: an entry whose class stops firing must be pruned or the
/// composer fixed), and its CITATION is asserted to hold by
/// [`known_panic_classes_cite_fixtures_that_produce_them`] (at least one cited fixture must actually
/// produce the class when generated). A PANIC matching no entry is a NEW finding and fails the sweep.
///
/// Messages are normalized to `<whitespace-collapsed message> @ <file> @ fn <symbol>` (no line
/// numbers — see `production_frame_symbol`), so a substring may pin the file AND the panicking
/// function when the message alone is ambiguous (`not implemented @ src/generation/mod.rs @ fn
/// <symbol>` matches only a BARE `unimplemented!()` in that specific function — any detailed
/// message breaks the contiguity, and a different function yields a different `<symbol>`). The
/// remaining collapse boundary is per-(message, file, function): two bare sites in the SAME
/// function share one key (`codegen_struct` / `generate_wrapper_struct` each host two).
const KNOWN_PANIC_CLASSES: &[(&str, &str)] = &[
    // (retired when the two anonymous-composite families and the group-choice-arm inline group
    // became graceful rejections) Three classes lived here, and they split by the composite's
    // BRACKET rather than by the role — which is what made a citation naming the wrong one send a
    // reader to the wrong parse site. `"Anonymous groups not allowed"`: an anonymous nested ARRAY
    // in a position requiring a TYPE (`a = [[int]]`) now records a rejection carrying the same two
    // remedies the panic advertised, so its whole `…type2.array` role-sibling family
    // (array-element, cbor-payload, choice-member, map-key, map-value, occurrence-target) rejects
    // gracefully under both profiles; the `@name` naming door beside it still mints the struct where
    // the comment can reach it, which the `anon-group-choice-member` cell of the comment-DSL
    // position sweep holds. `"TODO: non-table types as types"`: the MAP-bracket sibling of the same
    // family now records a rejection naming the map's supported named form, so every `…type2.map`
    // role sibling (array-element, map-value, cbor-payload, choice-member, generic-arg,
    // occurrence-target, group-choice-arm) rejects gracefully under both profiles too.
    // `"inline group entries are not implemented"`: an inline group as a group-choice arm's sole
    // entry now rejects the same way
    // (message pinned by `inline_group_choice_arm_rejects_gracefully`), in both the array and map
    // reps. Its second site (`group_entry_optional`) was left an abort because the record path
    // rejects every inline group before optionality is read, so no input reaches it; the follow-on
    // `"not implemented (define a new struct for this!)"` site that the arm's walk then hit
    // (`group_entry_to_raw_field_name`) now returns `None` — an inline group genuinely has no
    // explicit field name — and its remaining twin (`group_entry_to_field_name`) is unreachable
    // behind the same record-path guard. Both messages stay worded lead-constant so a future ledger
    // entry can key on them.
    // (retired when `cbor-any` became a graceful rejection and the expected-conversion names became
    // supported) The `"unsupported cddl prelude type:"` class originally held `cbor-any` /
    // `eb64url` / `eb64legacy` / `eb16`. `cbor-any` remains refused at `new_type`'s
    // unresolved-reserved fallback: its #6.55799 tag self-describes the whole serialized CBOR stream,
    // not a value. The three `eb*` names instead expand through `cddl_prelude` to their normative
    // fixed-tag `AnyCbor` wrappers (#6.21/#6.22/#6.23), so their advice stays on CBOR without an
    // invented base64/base16 API. The `cbor-any` message is pinned by
    // `cbor_any_prelude_tag_rejects_gracefully_in_every_position`; the expected-conversion codec and
    // context facts are pinned by the two `expected_conversion_prelude_*` tests. Projection owns the
    // remaining `tests/matrix_reject/prelude.cbor-any.cddl` row. The `panic!` arm in `cddl_prelude`
    // is deliberately LEFT IN PLACE only for `any`/`cbor-any`: it guards a future position routed
    // around `new_type`, and reaching it re-earns this entry rather than being papered over.
    // (retired when the narrower float prelude names became REGISTRATIONS)
    // The `"should be handled by the alias system instead"` class held `float16`, `float16-32` and
    // `float32-64` — the float names that contain only SOME of the float values. Each is now its
    // own registered primitive, and both directions carry the value class: decode accepts any head
    // and refuses a VALUE the class does not contain, and a write emits the value's shortest
    // lossless form (which for a member of the class IS its declared width). `float` keeps the
    // unconstrained behaviour it always had, which is why it needed a separate identity from
    // `float64` — same `f64` carrier, different value set.
    // Registration is pinned by `every_float_prelude_name_generates_with_its_own_carrier` and
    // `control_operator_path_maps_every_float_name_and_refuses_unmapped_heads`; the wire contract
    // by the `float_heads` vectors in tests/core and tests/preserve-encodings and the
    // `float_widths` KATs in tests/golden_hex_preserve. The `unreachable!` arm in `cddl_prelude`
    // is deliberately LEFT IN PLACE for both groups — reaching it re-earns this entry rather than
    // being papered over.
    // (retired when the member-side type2 catch-all became graceful rejections) An unsupported
    // `type2` in MEMBER / ELEMENT position — a byte-string literal (`h'…'` / `'…'`), an unwrap
    // (`~name`), a bare major-type constraint (`#N` / `#N.M`), the `any` sigil (`#`), a
    // choice-from-group (`&g`) or a choice-from-inline-group (`&( … )`) — now records a rejection
    // naming its OWN construct and continues with an inert placeholder, exactly as the rule-body
    // catch-all in `parse_type` already did for the same constructs. The conversion was taken whole
    // rather than per-construct: the class was the unfinished member half of the completed
    // top-level sweep, so leaving any arm panicking would have re-earned the same finding. Message
    // identity per construct is pinned by `unsupported_member_type2_rejects_gracefully`, and the
    // outcome category by tests/robustness/unwrap_member.cddl. The `parse_type` table it mirrors
    // is deliberately a SEPARATE table (its texts are matrix `code_anchor`s), so the two can be
    // reworded independently.
    (
        "non-literal tag heads (#6.<type>(...)) are not supported",
        "type-valued tag head (RFC 9682); pinned by tests/matrix_reject/type2.tag_head_type.cddl (PANIC row in the reject catalog)",
    ),
    (
        "doubly nested tags are not supported",
        "tag directly inside a tag; pinned by tests/matrix_panic/contain.tag-content.type2.tag.cddl",
    ),
    // (retired when a `.cbor` payload over a bare FIXED value gained default-profile SUPPORT) The
    // `assertion left: ";" right: ""` class was the value-less `Fixed` deserialize branch refusing
    // any caller-supplied before/after text, met by the `.cbor` payload arm's staging expression —
    // one spec (`[bytes .cbor 42]`) buildable under `--preserve-encodings` and aborting under the
    // default profile. The branch now evaluates to the unit `()` and emits it through whatever
    // wrapper it is handed, and the payload arm reads a value-less payload without staging it; a
    // caller that supplies no wrapper (which, while the asserts stood, was every caller) gets
    // byte-identical output. Unlike the sibling retirement below, the two `assert_eq!`s could NOT
    // be left standing: they were not a guard against a caller that shouldn't wrap, they were the
    // refusal itself. What replaces them is the discard suppression — a wrapper that binds nothing
    // still emits nothing — so a wrapping caller is served rather than rejected. Generation,
    // build and byte-exact round-trip across the default, `--annotate-fields=false` and preserve
    // profiles are pinned by
    // `cbor_payload_over_fixed_value_generates_and_round_trips_on_every_profile`, and the outcome
    // category by tests/robustness/cbor_fixed_payload.cddl (now an `ok` catalog row).
    // (retired when the fixed-value group-choice arm gained default-profile SUPPORT) A group-choice
    // arm whose whole content is a fixed value (`t = { a: 0 // b: tstr }`, `t = [ a: 0 // b: tstr ]`,
    // `t = [ 0 // tstr ]`) now generates under every profile instead of aborting under all but
    // `--preserve-encodings`. The key carried no `fn` component, so it covered BOTH callers of the
    // `Fixed` deserialize branch, and both were fixed: `generate_enum` dropped the `|| rep.is_some()`
    // that forced a group-choice arm to bind a value it has none of, and
    // `make_keyed_map_variant_deser_code` gained the fixed-value exemption it never had. The
    // `assert_eq!`s at the branch were left standing then as the guard that had caught this, and
    // were retired later by the entry above — the caller they were still rejecting turned out to be
    // a legitimate one. What holds this fix now is that its two call sites pass no wrapper at all,
    // which is the case the branch emits nothing for. The emitted code's shape (a
    // field-less construction, with the constant and the map arm's member key still VERIFIED) is
    // pinned by `group_choice_fixed_value_arm_emits_fieldless_variant`; the two array spellings,
    // which no matrix cell expresses, are `ok` rows in tests/robustness/.
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
    // (retired when the bare-fixed-in-member families became graceful rejections) the
    // `"should not expose Fixed type"` class is gone: a fixed value under a count-permitting
    // occurrence (`[* 5]`, and its table-VALUE sibling `{ * uint => 5 }`) is rejected in the parse
    // walk by `parse_group_type`, and a tag-wrapped prelude constant (`#6.11(true)`) is rejected at
    // the wrapper registration seam through the same shared message the alias seam already used for
    // `#6.5(5)`. Both robustness fixtures (tests/robustness/fixed_value_occurrence.cddl,
    // tests/robustness/tagged_prelude_constant.cddl) are now `error (graceful)` catalog rows. The
    // EXACTLY-ONCE placement stays supported (tests/robustness/fixed_bool_member.cddl).
];
// (Three classes that used to sit here are gone.) The array-of-`any` type-choice arm: `[* any]` is
// now a supported homogeneous array, so `[* any] / tstr` generates correctly —
// tests/robustness/choice_array_any_arm.cddl is an `ok` fixture. The `.cbor`-over-a-reference
// type-CHOICE arm ("variant ctor refers to undefined ident") and its rule-BODY sibling
// (`register_type_alias`'s "wraps automatically in Alias" assertion): a control operator's target
// is a rule-graph dependency and is alias-resolved when parsed, so both generate —
// tests/robustness/choice_cbor_ref_arm.cddl and tests/robustness/cbor_ref_rule_body.cddl are `ok`
// fixtures. Both messages stay worded lead-constant so a future ledger entry can key on them.

// ---- the ledger's CITATION detector ------------------------------------------------------------
/// Every `tests/**/*.cddl` fixture a citation names, in citation order. Citations are prose, so the
/// scan is deliberately shape-based (a whitespace-delimited token under `tests/` ending `.cddl`,
/// stripped of trailing prose punctuation) rather than a format the ledger would have to keep.
fn cited_fixtures(cite: &str) -> Vec<String> {
    cite.split_whitespace()
        .map(|t| t.trim_matches(|c: char| !(c.is_alphanumeric() || "/._-".contains(c))))
        .filter(|t| t.starts_with("tests/") && t.ends_with(".cddl"))
        .map(|t| t.to_owned())
        .collect()
}

/// Generate `path` and return the normalized panic message it produced, or `None` if generation did
/// not panic. Same flags the panic/reject catalogs probe with (`--wasm=false`, default profile), so
/// a fixture's captured message here is the one those catalogs' `PANIC` row records the category of.
fn captured_panic_for_fixture(path: &std::path::Path) -> Option<String> {
    crate::tests::robustness_tests::with_thread_silenced_panics(|| {
        let prev: std::sync::Arc<dyn Fn(&std::panic::PanicHookInfo) + Send + Sync> =
            std::sync::Arc::from(std::panic::take_hook());
        let mine = std::thread::current().id();
        let delegate = prev.clone();
        std::panic::set_hook(Box::new(move |info| {
            if std::thread::current().id() == mine {
                LAST_PANIC.with(|p| *p.borrow_mut() = Some(normalize_panic(info)));
            } else {
                delegate(info)
            }
        }));
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            path.to_str().unwrap(),
            "--output",
            "citation_detector_unused",
            "--wasm",
            "false",
        ]);
        LAST_PANIC.with(|p| *p.borrow_mut() = None);
        let out = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            crate::api::generated_strings(&cli)
        })) {
            Err(_) => Some(
                LAST_PANIC
                    .with(|p| p.borrow_mut().take())
                    .unwrap_or_else(|| "<panic with no hook capture>".to_owned()),
            ),
            Ok(_) => None,
        };
        let _ = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| prev(info)));
        out
    })
}

/// The ledger is guarded on its KEY — the sweep asserts every entry's substring is still OBSERVED —
/// and this guards the other half, the CITATION, which is the half a human reads during triage. A
/// citation naming the wrong fixture resolves (so no citation lint can fire) while sending a reader
/// to the wrong parse site; the instance on record split two anonymous-composite classes by the
/// composite's BRACKET and named the wrong one. So: run each cited fixture and require at least one
/// of them to actually produce the entry's substring.
///
/// A row whose citation names NO runnable fixture fails too. Only a fixture makes the claim
/// mechanically checkable, so a prose-only pin (a findings-ledger entry) has to be accompanied by
/// one rather than standing alone.
#[test]
fn known_panic_classes_cite_fixtures_that_produce_them() {
    let mut failures = Vec::new();
    for (sub, cite) in KNOWN_PANIC_CLASSES {
        let fixtures = cited_fixtures(cite);
        if fixtures.is_empty() {
            failures.push(format!(
                "  `{sub}` cites no `tests/**/*.cddl` fixture, so nothing can check the citation \
                 — pin: {cite}"
            ));
            continue;
        }
        let mut observed = Vec::new();
        let mut hit = false;
        for f in &fixtures {
            let path = std::path::Path::new(f);
            if !path.exists() {
                observed.push(format!("{f}: MISSING"));
                continue;
            }
            match captured_panic_for_fixture(path) {
                Some(msg) if msg.contains(sub) => {
                    hit = true;
                    break;
                }
                Some(msg) => observed.push(format!("{f}: {msg}")),
                None => observed.push(format!("{f}: did not panic")),
            }
        }
        if !hit {
            failures.push(format!(
                "  `{sub}` — no cited fixture produces it:\n    {}\n    pin as written: {cite}",
                observed.join("\n    ")
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "`KNOWN_PANIC_CLASSES` entries whose CITATION does not hold — triage following one of \
         these lands on the wrong site:\n{}\n\nRe-derive each by running the fixture and cite the \
         one that produces the class (or, if the class moved, retarget the entry).",
        failures.join("\n")
    );
}

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
    let mut observed_classes: BTreeMap<&str, usize> = BTreeMap::new();
    for (c, o) in comps.iter().zip(outcomes.iter()) {
        match o {
            Outcome::Ok => ok += 1,
            Outcome::Graceful(_) => graceful += 1,
            Outcome::Panic(msg) => {
                panics += 1;
                match KNOWN_PANIC_CLASSES.iter().find(|(sub, _)| msg.contains(sub)) {
                    Some((sub, _)) => {
                        *observed_classes.entry(sub).or_insert(0) += 1;
                    }
                    None => findings.push(format!(
                        "NEW panic class — composition {} ({}):\n--- spec ---\n{}--- panic ---\n{msg}\n\
                         Promotion: minimize by hand; pin it as a matrix containment row (annotations \
                         TOMLs + `bun run project_robustness.ts`) if the matrix can express the cell, \
                         else a tests/robustness/*.cddl catalog fixture with a rationale comment; \
                         ledger the finding in cddl-matrix/roadmap.toml § findings; then add a \
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

    // Vacuity floors — from the EXECUTED artifact, not the inputs. They answer "did the composer
    // rot"; the CURRENT counts (and their per-class breakdown) live in the committed datum
    // `tests/recombination-counts.json`, asserted exactly below, which answers "did a class
    // migrate". Floors sit well under the datum so real shrinkage fails loud while ingredient
    // additions don't churn them.
    //
    // The panic column is where the outcome split moves, and it moves in ONE direction: every
    // abort-to-rejection conversion migrates a block of compositions panic -> graceful without
    // touching a floor, because the floors bound `ok` and the swept total, and neither changes.
    // Attribution for the largest such movement to date (106 panic / 546 graceful -> 18 / 634):
    // two conversions moved the 88 — the then-four prelude-name refusals at the name-resolution
    // seam (`cbor-any` / `eb64url` / `eb64legacy` / `eb16`, then the narrower float names) and
    // the parse-seam conversions beside them (the four unsupported generic-definition bodies, the
    // control-operator path's own copy of the float refusal, and its unmapped-head sibling). The
    // split between the two was not measured. Later, B3-024D promoted the three `eb*` names to
    // fixed-tag `AnyCbor` wrappers; 32 cells migrated graceful -> ok (556/1053 -> 524/1085), while
    // `cbor-any` remains the one permanent prelude exclusion.
    //
    // The `graceful` column also moves the OTHER way when a refusal becomes support, and that is
    // the only thing that moves `ok` without an ingredient change: 29 compositions migrated
    // graceful -> ok (634/892 -> 605/921) when the three narrower float prelude names became
    // registrations.
    //
    // A MEMBER-KIND addition moves the columns a third way, and the `tagged_optional` row is the
    // worked example (1544/921/605/18 -> 1624/1001/607/16). Two independent effects, and separating
    // them is the point of recording it: (1) the table grows the cross-product by exactly
    // `SHAPES.len()` singles plus `SHAPES.len() * (2*old + 1)` pairs — 4 + 76 = 80 here — and all 80
    // landed `ok`, which is what put the kind in front of the layer-2 profiles at all; (2) the
    // seeded draws downstream of the table RESHUFFLE, so the sampled triples and depth-2 leaf
    // fillers land on different compositions. Effect (2) is why `panic` FELL while a kind was
    // added: the `non-literal tag heads` class lost 3 (14 -> 11) and one previously-unsampled
    // composition (`[bytes .cbor 42]`) surfaced a NEW class, now ledgered. A reshuffle can only
    // re-draw within the same ingredient set, so it moves classes between compositions; it cannot
    // invent an outcome the generator does not already produce.
    //
    // A TEMPLATE SPELLING change moves them a fourth way, without touching the composition SET:
    // parenthesizing the `cbor_payload` hole (`bytes .cbor ({h})`) migrated 14 graceful -> ok
    // (998/611 -> 1012/597) with `swept` and every panic class unchanged. The 14 split cleanly, and
    // the split is the point: 12 were fillers that are `type1`s rather than `type2`s (`int .ne 1`,
    // `uint .size 2`, the six range spellings, `tstr .size 4`, …), whose unparenthesized composition
    // was ILLEGAL CDDL by RFC 8610's `type1 = type2 [S (rangeop / ctlop) S type2]` — so those slots
    // were measuring the `cddl` crate's correct parse rejection, not this generator; the other 2 are
    // the self-composition (`bytes .cbor (bytes .cbor (…))`), which became legal at the same edit and
    // generates because the INLINE nested payload gained support. A further 4 compositions changed
    // MEANING while staying `ok` — those whose filler is a top-level choice (`bytes .cbor uint / tstr
    // / bytes` was a CHOICE whose first arm carried the payload; parenthesized, the payload IS the
    // choice), which is a re-baselining onto the cell this template's role actually names.
    //
    // Earlier panic -> ok movements, kept because each names the fixture that owns the shape:
    // generic INSTANTIATION in bare member position, when the `TypeGroupname` group-entry arm was
    // routed through `generic_instance_or_new_type` — tests/corpus/generic_call_member.cddl; fixed
    // BOOL literals in member position, when the deserialize `FixedValue::Bool` arm landed —
    // tests/corpus/fixed_bool_member.cddl; and the `[coords] / tstr` choice-arm and `gen<[coords]>`
    // generic-arg shapes, when member-position array-of-plain-group promotion landed.
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
            observed_classes.contains_key(sub),
            "ledgered panic class `{sub}` was never observed (pin: {cite}) — stale ledger entry or \
             a fix landed; prune/retarget the entry"
        );
    }

    check_recombination_counts(&RecombinationCounts {
        swept: comps.len(),
        ok,
        graceful,
        panic: panics,
        observed_classes: observed_classes
            .iter()
            .map(|(k, v)| ((*k).to_string(), *v))
            .collect(),
    });
}

// ---- the committed outcome-count datum ---------------------------------------------------------
/// Path (repo-relative) of the committed datum the sweep's outcome counts are held against.
const RECOMB_COUNTS_PATH: &str = "tests/recombination-counts.json";

/// What `recombination_generation_sweep` measured: the three outcome columns, the swept total, and
/// the per-ledger-class panic breakdown.
struct RecombinationCounts {
    swept: usize,
    ok: usize,
    graceful: usize,
    panic: usize,
    /// `KNOWN_PANIC_CLASSES` key -> how many compositions panicked into that class.
    observed_classes: BTreeMap<String, usize>,
}

impl RecombinationCounts {
    /// Render the committed JSON form. Hand-rendered rather than serialized so the committed file's
    /// shape is fixed by this function alone (the `tests/timings.json` house pattern: a leading
    /// `note` that carries the semantics and the bless command).
    fn render(&self) -> String {
        fn esc(s: &str) -> String {
            s.replace('\\', "\\\\").replace('"', "\\\"")
        }
        let mut out = String::new();
        out.push_str("{\n  \"v\": 1,\n  \"note\": \"");
        out.push_str(&esc(
            "Outcome counts measured by src/tests/recombination_tests.rs \
             `recombination_generation_sweep`, asserted EXACTLY (the vacuity floors in that test \
             answer \"did the composer rot\"; this datum answers \"did a class migrate\"). \
             `swept` is the composition count, `ok`/`graceful`/`panic` its three generation \
             outcomes, and `observed_classes` maps each observed KNOWN_PANIC_CLASSES ledger key to \
             the number of compositions that panicked into it. Deterministic: the sweep asserts \
             its enumeration is reproducible. Re-bless after any change that legitimately moves a \
             column (an abort-to-rejection conversion, an ingredient addition) with \
             `BLESS_RECOMB_COUNTS=1 cargo test --bin cddl-codegen recombination_generation_sweep`, \
             and say in the commit message WHY the numbers moved.",
        ));
        out.push_str("\",\n");
        out.push_str(&format!("  \"swept\": {},\n", self.swept));
        out.push_str(&format!("  \"ok\": {},\n", self.ok));
        out.push_str(&format!("  \"graceful\": {},\n", self.graceful));
        out.push_str(&format!("  \"panic\": {},\n", self.panic));
        out.push_str("  \"observed_classes\": {");
        let mut first = true;
        for (k, v) in &self.observed_classes {
            out.push_str(if first { "\n" } else { ",\n" });
            first = false;
            out.push_str(&format!("    \"{}\": {}", esc(k), v));
        }
        out.push_str(if first { "}\n}\n" } else { "\n  }\n}\n" });
        out
    }
}

/// Hold the measured counts against the committed datum, exactly. `BLESS_RECOMB_COUNTS=1` rewrites
/// the file (repo blessing convention, as for `manifest_template_drift` / `editor_schema_…`).
fn check_recombination_counts(measured: &RecombinationCounts) {
    let rendered = measured.render();
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(RECOMB_COUNTS_PATH);
    let committed = std::fs::read_to_string(&path).unwrap_or_default();
    if rendered == committed {
        return;
    }
    if std::env::var("BLESS_RECOMB_COUNTS").map(|v| v == "1") == Ok(true) {
        std::fs::write(&path, &rendered)
            .unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
        return;
    }
    let classes = measured
        .observed_classes
        .iter()
        .map(|(k, v)| format!("    {v:>5}  {k}"))
        .collect::<Vec<_>>()
        .join("\n");
    panic!(
        "{RECOMB_COUNTS_PATH} is stale against what the sweep measured — a class migrated, or an \
         ingredient/composer change moved a column.\n  measured: swept={} ok={} graceful={} \
         panic={}\n  per-class panic counts:\n{classes}\n\nIf the movement is intended, re-bless \
         with `BLESS_RECOMB_COUNTS=1 cargo test --bin cddl-codegen \
         recombination_generation_sweep` and say WHY in the commit message.\n--- committed \
         ---\n{committed}--- measured ---\n{rendered}",
        measured.swept, measured.ok, measured.graceful, measured.panic
    );
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
    // (retired with the no-deserialize propagation) The four E0599 entries that rode here — two
    // `shape=arr_record members=[optional,…` and two `shape=gchoice_arr …` — were both halves of ONE
    // defect: a refused `Deserialize` that did not propagate. A non-final `?` optional array-record
    // field has always been refused loudly (`Not generating Foo::deserialize()`), but the refusal
    // stopped at the struct: `--emit-tests` still minted `from_cbor_bytes` round-trip/reject tests
    // for it (E0599 `from_cbor_bytes`), and a containing enum still emitted its own Deserialize
    // calling the arm's never-emitted fn (E0599 `deserialize_as_embedded_group` for the group-choice
    // flavor, `deserialize` for the type-choice one). The refusal now propagates through both enum
    // flavors transitively and both emit-tests minters skip a type with no decoder, so the shapes
    // compile and their emitted tests pass — the DISAMBIGUATION of a non-final optional remains
    // unbuilt and the field remains undeserializable, which is the honest state, not a defect.
    // Pinned by `integration_tests::deserialize_refusal_propagates_through_enums_and_emitted_tests`.
    // NOTE for whoever runs the full tier: like the fixed-value group-choice retirement below, this
    // was NOT confirmed by `recombination_crates_execute` itself (a full-tier `#[ignore]`d gate). If
    // a class survives, the gate now reports it as an unledgered layer-2 failure naming the
    // composition — the loud direction; the entry staying would instead have silently excluded a
    // passing class forever, since the vacuity guard only fires on an entry matching ZERO ok
    // compositions.
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
    // default gate and surfaced only by the wasm leg's different batch boundaries; every profile now
    // also executes the deterministic transpose plan that separates natural batchmates.
    // (retired with the fixed-value group-choice arm's default-profile support) The tagged fixed
    // value in a map-rep arm (`t = { ga: #6.11(42) // fb: tstr }`) was the COMPILE-side twin of that
    // same defect, not a separate bug: the tag wrapper routed its deserialize down a branch that
    // never reached the `Fixed` assert, so generation succeeded while still emitting
    // `Ok(T::Ga(ga))` — a call on a field-less variant, which is the E0618. The fix emits
    // `Ok(T::Ga)` and the crate now `cargo check`s clean; verified by generating the shape from the
    // parent commit (E0618) and from the fix (clean), the emitted diff being exactly those two
    // lines. NOTE for whoever runs the full tier: this retirement was NOT confirmed by
    // `recombination_crates_execute` itself (a full-tier `#[ignore]`d gate). If the class survives,
    // the gate now reports it as an unledgered layer-2 failure naming this composition, which is the
    // loud direction — the entry staying would instead have silently excluded a passing class
    // forever, since the vacuity guard only fires on an entry matching ZERO ok compositions.
    // (retired with the identical-arm dedup + the emitted first-match assertion) The six
    // wire-ambiguous type-choice entries — `outer=choice_member filler=prelude.text` /
    // `filler=prelude.tstr` / `filler=type.choice`,
    // `outer=choice_member inner=choice_member filler=ctl.ne.zero`,
    // `outer=garm_arr inner=choice_member filler=rangeop.exclusive.int`, and
    // `outer=cbor_payload filler=type.choice` — were ONE defect wearing two faces, and each face got
    // its own half of the fix. Four of the six composed a LITERALLY duplicated arm (`text / tstr`
    // builds the same IR as `tstr / tstr`; `uint / tstr / bytes / tstr` and `int .ne 0 / tstr / tstr`
    // repeat one), which minted a variant no decode could ever produce — those arms now collapse at
    // the IR (`parsing::create_variants_from_type_choices`), loudly, so the junk variant and its
    // impossible test are both gone. The other two overlap for real (`[ ga: -10...10 / tstr // tstr ]`
    // — a `tstr` accepted by both arms; `bytes .cbor uint / tstr / bytes` — a byte string accepted by
    // arm 1 and arm 3) and CANNOT be deduped: the emitted round-trip now asserts the property the
    // wire has (first match: decoded variant index <= minted, value identity only when they are
    // equal, byte-identical re-encode either way) instead of variant identity, which the wire cannot
    // carry. Verified by generating both ledgered non-dedup compositions from the parent commit
    // (`AmbArr (variant Text)`: minted `Text("a")`, decoded `Ga(Text("a"))`; `AmbCbor (variant
    // Bytes)`: minted `Bytes([0])`, decoded `U64(0)` — both the pinned "deserialized value must equal
    // the minted original" failure) and from the fix (both green), plus the four dedup-class arms red
    // at the parent and green after. Pinned by
    // `wire_ambiguous_type_choice_arms_dedup_and_first_match` (`local` tier), which spells both
    // shapes in its OWN fixture text — so the property survived the `cbor_payload` template gaining
    // its parentheses, which stopped the sweep composing the `.cbor` face at all: that composition is
    // now `bytes .cbor (uint / tstr / bytes)`, a payload that IS a choice rather than a choice whose
    // first arm carries a payload, and it has no cross-arm ambiguity to be wrong about.
    // NOTE for whoever runs
    // the full tier: this retirement was NOT confirmed by `recombination_crates_execute` itself (a
    // full-tier `#[ignore]`d gate). If a class survives, the gate now reports it as an unledgered
    // layer-2 failure naming the composition, which is the loud direction — the entries staying would
    // instead have silently excluded passing classes forever, since the vacuity guard only fires on
    // an entry matching ZERO ok compositions.
    // -- emitted-test minter / baseline decode gaps on nested shapes -------------------------------
    // (The former `outer=generic_arg inner=map_key filler=ctl.ne.zero` entry — the emit-tests minter
    // minting key 0 against an `int .ne 0` table domain — retired when its pinning composition
    // stopped reaching layer 2. Its map spelling carries NO occurrence indicator, so it now
    // generates an exact `BoundedMap`; the generic-instantiation reach is pinned by
    // `generic_arg_no_occurrence_table_is_exact_bounded`. The MINTER gap itself is now closed —
    // the minter picks a key base its own emitted bounds check accepts — and is pinned end-to-end
    // for exact and `*`-spelled tables by `emit_tests_bounded_map_key_execute`, arm-by-arm by
    // `bounds_reject_value_agrees_with_emitted_condition`.)
    //
    // (The former `outer=arr_mid inner=cbor_payload filler=prelude.float64` entry — a
    // `bytes .cbor float64` member failing its emitted baseline re-decode — retired when the defect
    // it cited was fixed: every leaf under a `bytes .cbor` overload now reads the payload's own
    // deserializer instead of the outer one. Pinned end-to-end by `cbor_payload_leaves` /
    // `cbor_payload_indefinite_inner` in `tests/core/tests.rs`, which execute a decode and assert the
    // member AFTER the payload — the assertion a snapshot of the emitted text structurally cannot
    // make.)
];

// ---- generalized layer-2 runner (shared by every emission profile) --------------------------------
/// A layer-2 execution profile. One runner (`run_layer2_profile`) drives the whole shape:
/// classify under the profile in-process, execute the ok compositions under natural and transposed
/// deterministic batch plans, generate each batch with the profile's flags, run the profile's cargo
/// verb on the profile's generated crate, and re-attribute batch failures per member. Items 2/3
/// (json / wasm) plug in by building a different `Layer2Profile` — no runner change — which is why
/// the exec step is data-driven (`exec_args`/`crate_subdir`/`cargo_subcmd`) rather than a hard-coded
/// rust-`test` path.
struct Layer2Profile<'a> {
    /// Human profile name — labels the scratch root and summary; each batch plan extends it for its
    /// own target and output cells.
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
    /// Alias-only roots that the discovery pass found and embedded before cargo ran. This is kept
    /// separate from `executed_floor`: holder rules are oracle scaffolding, not compositions.
    embedded_alias_roots_floor: usize,
}

/// Return every authored CDDL rule name in `spec`. Recombination compositions use one rule per
/// line, and accepting generic LHS spellings here keeps holder-name collision avoidance scoped to
/// the complete authored/renamed-rule namespace rather than just roots.
fn authored_rule_names(spec: &str) -> BTreeSet<String> {
    spec.lines()
        .filter_map(|line| {
            let line = line.trim_start();
            let name_len = line
                .bytes()
                .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
                .count();
            if name_len == 0 {
                return None;
            }
            let name = &line[..name_len];
            let rest = line[name_len..].trim_start();
            if rest.starts_with('=') || rest.starts_with("/=") || rest.starts_with('<') {
                Some(name.to_owned())
            } else {
                None
            }
        })
        .collect()
}

/// Discover transparent composition-root aliases from rustfmt-stable generated `mod.rs`, then
/// append one deterministic embed holder per root. A generated alias is useful only when its
/// exact `rc<digits>` CDDL root is present in this batch's authored rules; auxiliary aliases have
/// non-digit suffixes and are deliberately excluded.
fn augment_alias_root_embed_sites(spec: &str, generated_mod: &str) -> (String, usize) {
    let authored = authored_rule_names(spec);
    let roots: BTreeSet<String> = generated_mod
        .lines()
        .filter_map(|line| {
            let rest = line.trim_start().strip_prefix("pub type ")?;
            let (name, _) = rest.split_once('=')?;
            let name = name.trim_end();
            let digits = name.strip_prefix("Rc")?;
            (!digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()))
                .then(|| format!("rc{digits}"))
        })
        .filter(|root| authored.contains(root))
        .collect();
    if roots.is_empty() {
        return (spec.to_owned(), 0);
    }

    let mut used_names = authored;
    let mut augmented = spec.to_owned();
    if !augmented.ends_with('\n') {
        augmented.push('\n');
    }
    for root in &roots {
        let base = format!("{root}_embed");
        let mut holder = base.clone();
        let mut suffix = 2usize;
        while used_names.contains(&holder) {
            holder = format!("{base}_{suffix}");
            suffix += 1;
        }
        used_names.insert(holder.clone());
        augmented.push_str(&format!("{holder} = [e: {root}]\n"));
    }
    (augmented, roots.len())
}

#[test]
fn alias_root_embed_augmentation_selects_exact_public_root_aliases_in_order() {
    let spec = "rc2 = uint\nrc10 = tstr\nrc3_aux = bool\n";
    let generated_mod = "\
pub type Rc2 = u64;
pub type Rc10 = String;
pub type Rc3Aux = bool;
type Rc4 = u64;
pub struct Rc5;
pub type Rc6<T> = Vec<T>;
pub type Rc7 = u64;
";

    let (augmented, embedded) = augment_alias_root_embed_sites(spec, generated_mod);

    assert_eq!(embedded, 2);
    assert!(augmented.ends_with("rc10_embed = [e: rc10]\nrc2_embed = [e: rc2]\n"));
    assert!(!augmented.contains("rc3_aux_embed"));
    assert!(!augmented.contains("rc4_embed"));
    assert!(!augmented.contains("rc5_embed"));
    assert!(!augmented.contains("rc6_embed"));
    assert!(!augmented.contains("rc7_embed"));
}

#[test]
fn alias_root_embed_augmentation_avoids_authored_and_renamed_rule_collisions() {
    let spec = "\
rc2 = uint
rc2_embed = bool
rc2_embed_2 = null
rc2_aux = tstr
rc10<item> = [item]
";
    let (augmented, embedded) = augment_alias_root_embed_sites(spec, "pub type Rc2 = u64;\n");

    assert_eq!(embedded, 1);
    assert!(augmented.ends_with("rc2_embed_3 = [e: rc2]\n"));
}

#[test]
fn alias_root_embed_augmentation_is_a_noop_without_root_aliases() {
    let spec = "rc2 = uint\n";
    let generated_mod = "pub struct Rc2 { value: u64 }\npub type Rc2Aux = u64;\n";

    assert_eq!(
        augment_alias_root_embed_sites(spec, generated_mod),
        (spec.to_owned(), 0)
    );
}

#[test]
fn alias_root_embed_augmentation_recognizes_a_rustfmt_wrapped_alias_rhs() {
    let spec = "rc11 = uint\n";
    let generated_mod = "pub type Rc11 =\n    VeryLongGeneratedAliasThatRustfmtWrapped;\n";

    assert_eq!(
        augment_alias_root_embed_sites(spec, generated_mod),
        ("rc11 = uint\nrc11_embed = [e: rc11]\n".to_owned(), 1)
    );
}

/// Generate `spec` with the profile's generation flags, discover and embed transparent alias
/// roots from that first output when present, then run the profile's cargo verb on the final tree.
/// Returns the number of embedded alias roots; `Err(reason)` on any stage.
fn gen_and_exec(
    spec: &str,
    out: &std::path::Path,
    target_dir: &std::path::Path,
    p: &Layer2Profile,
    cache_run: &mut usize,
    cache_hit: &mut usize,
) -> Result<usize, String> {
    let (profile_args, exec_args, crate_subdir, cargo_subcmd) =
        (p.profile_args, p.exec_args, p.crate_subdir, p.cargo_subcmd);
    let spec_path = out.with_extension("cddl");
    std::fs::create_dir_all(out.parent().unwrap()).ok();
    std::fs::write(&spec_path, spec).map_err(|e| e.to_string())?;
    let discovery_out = codegen_cmd()
        .arg(format!("--input={}", spec_path.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .args(profile_args)
        .args(exec_args)
        .output()
        .unwrap();
    if !discovery_out.status.success() {
        return Err(format!(
            "discovery generation failed\n{}",
            String::from_utf8_lossy(&discovery_out.stderr)
        ));
    }
    let generated_mod = out.join("rust/src/generated/mod.rs");
    let generated_mod = std::fs::read_to_string(&generated_mod).map_err(|e| {
        format!(
            "discovery generation produced no readable rust/src/generated/mod.rs at {}: {e}",
            generated_mod.display()
        )
    })?;
    let (augmented_spec, embedded_alias_roots) =
        augment_alias_root_embed_sites(spec, &generated_mod);
    if embedded_alias_roots > 0 {
        std::fs::remove_dir_all(out).map_err(|e| {
            format!(
                "remove discovery output {} before final alias-root generation: {e}",
                out.display()
            )
        })?;
        std::fs::write(&spec_path, augmented_spec).map_err(|e| e.to_string())?;
        let final_out = codegen_cmd()
            .arg(format!("--input={}", spec_path.to_str().unwrap()))
            .arg(format!("--output={}", out.to_str().unwrap()))
            .args(profile_args)
            .args(exec_args)
            .output()
            .unwrap();
        if !final_out.status.success() {
            return Err(format!(
                "final alias-root generation failed\n{}",
                String::from_utf8_lossy(&final_out.stderr)
            ));
        }
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
        Ok(embedded_alias_roots)
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
                         tests/corpus/) or cite an existing roadmap.toml § findings entry; ledger it in \
                         cddl-matrix/roadmap.toml § findings; add a profile panic-ledger entry citing the pin.",
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

    // Every executable composition runs under BOTH plans. The transpose starts from natural
    // batches, so it deliberately separates their batchmates without any hash/random ordering.
    let plans = layer2_batch_plans(&executable, LAYER2_RULES_PER_BATCH);
    assert_layer2_batch_plan_integrity(&plans, &executable);

    // Per-profile scratch root, with plan-labelled target dirs below it: keeps profiles and their
    // two plans from clobbering each other and (for the serde/schemars-pulling json profile) stops
    // feature-resolution thrash invalidating the default cache.
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_recomb_{}_{:016x}",
        p.name,
        checkout_hash()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let mut findings: BTreeMap<String, String> = BTreeMap::new();
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;
    let mut per_plan: Vec<(&str, usize, usize, usize)> = Vec::new();
    let mut total_oracle_runs = 0usize;
    for plan in &plans {
        // Plan-labelled scratch names and targets prevent an output/cache cell from one grouping
        // being reused by the other merely because their batch index is the same.
        let target_dir = root.join(format!("{}_target", plan.label));
        let mut executed = 0usize;
        let mut embedded_alias_roots = 0usize;
        let mut oracle_runs = 0usize;
        for (bi, batch) in plan.batches.iter().enumerate() {
            let mut run_batch = |spec: &str, out: &std::path::Path| {
                oracle_runs += 1;
                gen_and_exec(spec, out, &target_dir, p, &mut cache_run, &mut cache_hit)
            };
            let spec: String = batch.iter().map(|c| c.spec.as_str()).collect();
            let out = root.join(format!("{}_batch{bi:03}", plan.label));
            match run_batch(&spec, &out) {
                Ok(embedded) => {
                    executed += batch.len();
                    embedded_alias_roots += embedded;
                }
                Err(batch_reason) => {
                    // Attribute: rerun each member individually under the same labelled plan.
                    let mut attributed = false;
                    for c in batch {
                        let mout = root.join(format!("{}_batch{bi:03}_{}", plan.label, c.id));
                        match run_batch(&c.spec, &mout) {
                            Err(reason) => {
                                attributed = true;
                                // A composition can fail under both plans. Keep one complete,
                                // plan-labelled finding rather than duplicating promotion work.
                                findings.entry(c.id.clone()).or_insert_with(|| format!(
                                    "NEW layer-2 finding under {} profile / {} plan — composition {} ({}):\n--- spec ---\n{}--- failure ---\n{reason}\n\
                                     Promotion: minimize by hand; pin it (matrix row / tests/robustness/ / \
                                     tests/corpus/); ledger it in cddl-matrix/roadmap.toml § findings; add a \
                                     profile known-bad entry citing the pin.",
                                    p.name, plan.label, c.id, c.desc, c.spec
                                ));
                            }
                            Ok(embedded) => {
                                executed += 1;
                                embedded_alias_roots += embedded;
                            }
                        }
                    }
                    if !attributed {
                        findings.insert(
                            format!("{}-batch-{bi:03}", plan.label),
                            format!(
                                "{} profile / {} plan batch {bi} failed but every member passed individually — \
                                 a CROSS-COMPOSITION interaction (this is itself a finding; bisect the batch):\n{batch_reason}",
                                p.name, plan.label
                            ),
                        );
                    }
                }
            }
        }
        total_oracle_runs += oracle_runs;
        per_plan.push((
            plan.label,
            plan.batches.len(),
            executed,
            embedded_alias_roots,
        ));
    }
    let _ = std::fs::remove_dir_all(&root);
    if gate_cache::enabled() {
        println!(
            "recombination {} gate-cache: {cache_run} run, {cache_hit} cached",
            p.name
        );
    }

    println!(
        "recombination {} layer 2: TWO deterministic plans; classified ok={} graceful={} panic={}; {} batch specs / {} oracle runs; {} unique compositions ({} known-bad excluded); {} in {:?}",
        p.name,
        ok_comps.len(),
        graceful,
        panics,
        per_plan
            .iter()
            .map(|(_, batches, _, _)| *batches)
            .sum::<usize>(),
        total_oracle_runs,
        executable.len(),
        ok_comps.len() - executable.len(),
        per_plan
            .iter()
            .map(|(label, batches, executed, embedded)| {
                format!("{label}: {batches} batches / {executed} executed / {embedded} alias roots")
            })
            .collect::<Vec<_>>()
            .join("; "),
        t0.elapsed()
    );
    assert!(
        findings.is_empty(),
        "recombination {} layer 2 surfaced {} finding(s):\n\n{}",
        p.name,
        findings.len(),
        findings.values().cloned().collect::<Vec<_>>().join("\n\n")
    );
    for (label, _, executed, embedded_alias_roots) in &per_plan {
        assert!(
            *executed >= p.executed_floor,
            "only {executed} compositions executed in {} layer 2 {label} plan (floor {}) — batching rotted",
            p.name,
            p.executed_floor
        );
        assert!(
            *embedded_alias_roots >= p.embedded_alias_roots_floor,
            "only {embedded_alias_roots} alias roots embedded in {} layer 2 {label} plan (floor {}) — alias-root discovery rotted",
            p.name,
            p.embedded_alias_roots_floor
        );
    }
}

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): batch layer 1's ok compositions into
/// two deterministic sets of ~`LAYER2_RULES_PER_BATCH`-rule specs, generate each with
/// `--emit-tests=true --wasm=false`
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
        embedded_alias_roots_floor: 1,
    });
}

// ---- preserve profile: panic ledger + known-bad ledger + the escalation gate ----------------------
/// Panic classes that appear when classifying under `--preserve-encodings=true` but are ok/graceful
/// under the default profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` (which stays the
/// allowlist); a preserve panic matching neither is a NEW finding. Each entry cites an existing
/// `cddl-matrix/roadmap.toml` § findings entry (stable title, never a position). Vacuity-guarded in
/// `recombination_preserve_crates_execute`.
const PRESERVE_ONLY_PANIC_CLASSES: &[(&str, &str)] = &[
    // (retired when native floats gained preserve support) a float in member / element / tag /
    // choice-arm position under --preserve-encodings no longer panics: the head width (`0xf9`/
    // `0xfa`/`0xfb`) is now an `Option<cbor_event::Sz>` encoding variable read by `float_sz()` and
    // written by the `write_float` runtime helper (`write_float_width` for a name that constrains
    // the value class), so those compositions batch into the preserve gate like any other primitive. Pinned by `preserve_encodings_supports_floats` and the
    // golden_hex_preserve / golden_hex_canonical float KATs.
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
/// Classifies every composition under `--preserve-encodings=true`, executes each preserve-ok
/// composition in both deterministic batch plans, generates
/// `--preserve-encodings=true --emit-tests=true --wasm=false`, and `cargo test`s the rust crate —
/// the leg that would have caught the preserve-only E0308 on tag-wrapped fixed-value members
/// (`[v: #6.1(null)]`) that passed every default-profile gate and was found only by review.
///
/// Preserve panics for classes that are ok/graceful under default go in
/// `PRESERVE_ONLY_PANIC_CLASSES` and never reach execution; a NEW preserve panic fails loudly. That
/// ledger is currently EMPTY — every class it held (floats as members, a tag over a type-choice or
/// group-choice rule, a tag wrapping `any`) either gained preserve support or gained a graceful
/// refusal, and its retirement comment records which. (Optional non-float fixed-value members now
/// generate and round-trip via a `bool` presence field under both profiles — the former
/// encoding-less optional-fixed preserve assert is gone; the composition set still has no
/// optional-FIXED member kind, so adding one is the extended-member-kind residual in
/// tests/testing-roadmap.toml.) Profile flags are sourced from
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
        embedded_alias_roots_floor: 1,
    });
}

// ---- json profile: panic ledger + known-bad ledger + the escalation gate --------------------------
/// Panic classes that appear when classifying under
/// `--json-serde-derives=true --json-schema-export=true` but are ok/graceful under the default
/// profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` allowlist; a json panic matching neither
/// is a NEW finding. Each non-empty entry must cite an existing `cddl-matrix/roadmap.toml` § findings
/// entry or another committed stable pin. Vacuity-guarded in
/// `recombination_json_crates_execute`.
const JSON_ONLY_PANIC_CLASSES: &[(&str, &str)] = &[];

/// Json-profile compile/round-trip known-bad classes. Desc-substring keyed, each citing its pin;
/// vacuity-guarded in `recombination_json_crates_execute`. The shared `LAYER2_KNOWN_BAD` also
/// applies (as an exclusion, un-guarded here).
const LAYER2_JSON_KNOWN_BAD: &[(&str, &str)] = &[];

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): the JSON escalation of layer 2.
/// Classifies every composition under the `json` profile from `crate::tests::ALL_PROFILES`
/// (`--json-serde-derives=true --json-schema-export=true`), executes each json-ok composition in
/// both deterministic batch plans, generates `--json-serde-derives=true
/// --json-schema-export=true --emit-tests=true --wasm=false`, and
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
        embedded_alias_roots_floor: 1,
    });
}

// ---- wasm profile: panic ledger + known-bad ledger + the escalation gate --------------------------
/// Panic classes that appear when classifying under `--wasm=true` but are ok/graceful under the
/// default `--wasm=false` profile. Checked AFTER the shared `KNOWN_PANIC_CLASSES` allowlist; a wasm
/// panic matching neither is a NEW finding. Each non-empty entry must cite an existing
/// `cddl-matrix/roadmap.toml` § findings entry or another committed stable pin. Vacuity-guarded in
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
// compositions return to both wasm batch plans; a new wasm-only compile class would be caught there
// as a NEW finding.
const LAYER2_WASM_KNOWN_BAD: &[(&str, &str)] = &[];

/// MANUAL/LOCAL ONLY (`#[ignore]`, check.ts `full` tier): the WASM escalation of layer 2.
/// Classifies every composition under `--wasm=true`, executes each wasm-ok composition in both
/// deterministic batch plans, generates `--wasm=true` without emitted tests, and `cargo check`s the
/// generated `wasm/` crate. The wasm crate depends on the generated rust crate by path, so rust-side
/// compile failures surface through the single check. This is a fuzz-recombination cross-check of the
/// wasm emission path; the wasm-ABI matrix remains the systematic per-shape wasm surface owner.
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
        embedded_alias_roots_floor: 1,
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
