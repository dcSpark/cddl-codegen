//! Comment-DSL directive × attachment-position sweep — a DOCS-CONTRACT enumeration no execution
//! probe can catch.
//!
//! A comment-DSL directive that silently no-ops in an unenumerated attachment position still
//! generates, compiles, and round-trips, so the matrix's execution-gated per-cell probes (verify.ts)
//! structurally cannot see it — exactly the argument the sibling `identifier_hazard_tests` module
//! makes for the name-shaped axis. The evidence class here is a STRING-level assertion on the
//! generated source: for each (directive, position) cell the expectation IS the claim in
//! `docs/docs/comment_dsl.mdx` (or, for the rule-position `@name` and anonymous-group cells, the
//! remedy an error message advertises).
//!
//! Unlike the hazard catalog (a blessable snapshot scorecard), this sweep is HARD-ASSERTED: each
//! cell's expectation is the docs' promise, and blessing a decay to silent-drop would defeat the
//! purpose. `KNOWN_SILENT_DROP` (mirroring the hazard sweep's `EXPECTED_COMPILE_FAIL`) pins any cell
//! whose directive is currently DROPPED in that position — asserted to STILL be dropped, so the pin
//! flips loudly the day a fix lands. A pin is a FINDING, not a license to fix: do not silence a drop
//! by re-authoring its expectation, and do not fix a newly-found drop beyond the scoped rule-position
//! `@name` rejection that this module's Part 2 lands.
//!
//! Every cell runs generation under `catch_unwind` + `with_thread_silenced_panics` (like the hazard
//! sweep) so a panic is reported as its own failure kind — never a test abort, and never mistaken for
//! a graceful rejection.

use crate::cli::Cli;
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

/// What a cell asserts about the concatenated generated source.
enum Expect {
    /// Substrings that MUST / MUST-NOT appear in the concatenated generated source. Absence-based
    /// assertions (`@no_alias`, `@custom_json`) are paired with a positive control in the same spec
    /// (a sibling rule whose normal output IS present) so the absence is attributable to the
    /// directive, not to a misconfigured flag.
    Effect {
        must: &'static [&'static str],
        must_not: &'static [&'static str],
    },
    /// Generation returns a graceful `Err` whose message contains this substring (never a panic).
    Reject(&'static str),
}

/// A single grid cell: `(directive, position)` uniquely identifies it (so a `KNOWN_SILENT_DROP` pin
/// keys on that pair). `spec` is the minimal CDDL isolating the cell; `flags` are extra CLI args
/// beyond the `--wasm=false` baseline (only `@custom_json` needs `--json-serde-derives=true` for
/// observability — every other cell stays on the baseline so its expectation isn't flag-dependent).
struct Cell {
    directive: &'static str,
    position: &'static str,
    spec: &'static str,
    flags: &'static [&'static str],
    expect: Expect,
}

/// Cells whose directive is currently DROPPED in that position — a FINDING, not a fixture. Each is
/// asserted to STILL fail its docs-claimed expectation (drop present), so the pin flips loudly when a
/// fix lands. `(directive, position, reason)`, mirroring the hazard sweep's `EXPECTED_COMPILE_FAIL`.
///
/// AUTHORING RULE — pins have a vacuity hazard Effect cells don't: a pin asserts "expectation NOT
/// satisfied", which a MISPLACED directive comment satisfies vacuously (the DSL's comma-placement
/// rules are finicky), so the pin would hold for the wrong reason. Only pin a cell after
/// hand-verifying the placement variants against the docs' comma rules (the anon-group pin was
/// probed with and without the trailing comma), beside a control cell using the same placement in
/// a position where the directive DOES work, isolating position as the variable — the anon-group
/// pin's control is the `anon-group-choice-member` cell.
///
/// Two live findings (neither fixed by this task — the scoped fix is rule-position `@name` rejection):
///   - `@name` @ `anon-group-member`: the "Anonymous groups not allowed" panic advertises `@name` as
///     the remedy, but at a MEMBER-position anonymous inline group the comment lands on the enclosing
///     group entry's trailing_comments, which the naming site's `get_comment_after(type2)` never
///     reaches (it ascends only through Type1/TypeChoice). So `@name` is dropped and the anonymous-
///     group panic fires anyway — the advertised remedy does not work in this position.
///   - `@doc` @ `type-choice-variant`: on a FIXED-VALUE type choice (`0 / 1`, a dataless C-style
///     enum) the per-variant `@doc` is captured into the IR (`create_variants_from_type_choices`
///     threads `rule_metadata.comment`) but never emitted — the dataless-variant rendering drops it.
///     `@doc` on DATA-carrying type-choice variants (`uint / tstr`) IS emitted, so the drop is
///     specific to the C-style-enum shape.
const KNOWN_SILENT_DROP: &[(&str, &str, &str)] = &[
    (
        "@name",
        "anon-group-member",
        "@name at a member-position anonymous inline group is unreachable by the naming site's \
         get_comment_after(type2) ascent (Type1/TypeChoice only), so the anonymous-group panic fires \
         despite the panic message advertising @name as the remedy",
    ),
    (
        "@doc",
        "type-choice-variant",
        "@doc on a fixed-value (dataless C-style enum) type-choice variant is captured into the IR \
         but never emitted; only data-carrying type-choice variants render the /// doc comment",
    ),
];

/// The docs-claimed grid. Anchors were verified empirically against emitted source while authoring;
/// the `must` fragments are the load-bearing bits, not guaranteed-verbatim whole lines.
const GRID: &[Cell] = &[
    // ---- @name -------------------------------------------------------------------------------
    // 1. value-key map field, int key → field `inputs` (the dsl.name canonical for maps).
    Cell {
        directive: "@name",
        position: "map-value-key-int",
        spec: "t = {\n  0: uint, ; @name inputs\n}\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub inputs"],
            must_not: &["key_0"],
        },
    },
    // 2. value-key map field, text key → field `renamed` (not the bareword-sugar `k`).
    Cell {
        directive: "@name",
        position: "map-value-key-text",
        spec: "t = {\n  \"k\": uint, ; @name renamed\n}\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub renamed"],
            must_not: &["pub k:"],
        },
    },
    // 3. arrow-key map field (fixed key routes to the record path) → field `x`.
    Cell {
        directive: "@name",
        position: "map-arrow-key",
        spec: "t = {\n  0 => uint, ; @name x\n}\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub x"],
            must_not: &["key_0"],
        },
    },
    // 4. bareword-key map field — the Rust-keyword rejection's advertised remedy: `if:` alone
    //    rejects, but `; @name if_flag` renames the field so it generates.
    Cell {
        directive: "@name",
        position: "map-bareword-key",
        spec: "kw = {\n  if: uint, ; @name if_flag\n}\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub if_flag"],
            must_not: &["r#if"],
        },
    },
    // 5. unkeyed array element → fields `address`, `checksum` (else `index_0`/`index_1`).
    Cell {
        directive: "@name",
        position: "array-element-unkeyed",
        spec: "named = [\n  bytes, ; @name address\n  uint ; @name checksum\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub address", "pub checksum"],
            must_not: &["index_0"],
        },
    },
    // 6. keyed array element → field `renamed` (not `foo`).
    Cell {
        directive: "@name",
        position: "array-element-keyed",
        spec: "t = [foo: uint, ; @name renamed\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub renamed"],
            must_not: &["pub foo"],
        },
    },
    // 7. type-choice variant → variants `Mainnet`/`Testnet` (else `I0`/`I1`).
    Cell {
        directive: "@name",
        position: "type-choice-variant",
        spec: "foo = 0 ; @name mainnet\n    / 1 ; @name testnet\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["Mainnet", "Testnet"],
            must_not: &["I0", "I1"],
        },
    },
    // 8. group-choice arm → variant names from the arms' `@name`s.
    Cell {
        directive: "@name",
        position: "group-choice-arm",
        spec: "script = [\n  ; @name native\n  tag: 0, script_native: uint //\n  ; @name plutus_v1\n  tag: 1, script_v1: bytes\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["Native", "PlutusV1"],
            must_not: &[],
        },
    },
    // 9. anonymous inline composite in a MEMBER position — the "Anonymous groups not allowed"
    //    panic's advertised remedy. PINNED: dropped (fires the panic anyway). See KNOWN_SILENT_DROP.
    Cell {
        directive: "@name",
        position: "anon-group-member",
        spec: "t = [0, [1, bytes] ; @name inner\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["struct Inner"],
            must_not: &[],
        },
    },
    // 9b. PLACEMENT CONTROL for the anon-group-member pin (the KNOWN_SILENT_DROP authoring rule):
    //     the same directive + comment placement (after the inline composite) in the CHOICE-MEMBER
    //     position, where the naming site (`get_comment_after(type2)`, parsing.rs) IS reachable.
    //     This proves the placement parses and the mechanism works, isolating member-position as
    //     the pinned cell's variable — the pin cannot hold vacuously on a placement typo.
    Cell {
        directive: "@name",
        position: "anon-group-choice-member",
        spec: "x = [1, bytes] ; @name arr_variant\n  / uint\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["struct ArrVariant"],
            must_not: &[],
        },
    },
    // 10. rule-position @name, single-type-choice ALIAS rule → graceful Reject (Part 2).
    Cell {
        directive: "@name",
        position: "rule-type-alias",
        spec: "foo = uint ; @name bar\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Reject("does not rename a top-level"),
    },
    // 10b. rule-position @name, single-type-choice STRUCT rule → graceful Reject (the drop is
    //      shape-independent; this cell pins that the seam catches the struct path too).
    Cell {
        directive: "@name",
        position: "rule-type-struct",
        spec: "foo = [a: uint] ; @name bar\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Reject("does not rename a top-level"),
    },
    // 10c/10d. rule-position @name, T/null two-choice rule → graceful Reject. The rule-name
    //          position carries a SHAPE axis: `T / null` collapses to an `Option<T>` alias
    //          (`parse_type_choices`' optional-inner path) instead of an enum, so a `@name` on
    //          either arm has no variant to name — it was silently dropped (probe-verified for
    //          BOTH placements: `pub type Foo = Option<u64>;` emitted untouched) until the
    //          rejection learned the collapse. Both placements pinned so neither decays.
    Cell {
        directive: "@name",
        position: "rule-type-tnull",
        spec: "foo = uint ; @name bar\n    / null\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Reject("does not rename a top-level"),
    },
    Cell {
        directive: "@name",
        position: "rule-type-tnull-trailing",
        spec: "foo = uint / null ; @name bar\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Reject("does not rename a top-level"),
    },
    // 11. rule-position @name, plain-GROUP rule. FINDING: unlike a type rule, a trailing `@name`
    //     on a plain group is NOT a rule-position drop — cddl binds it to the LAST group entry's
    //     trailing comment, so it is consumed by the field-naming site (group_entry_to_field_name)
    //     and renames that field. `grp = (a: uint) ; @name other` therefore renames field `a` to
    //     `other`, AST-indistinguishable from the in-paren field rename `grp = (a: uint ; @name
    //     other)`. Rejecting it would change behavior at that working field-naming site — which the
    //     rejection is explicitly required NOT to do — so the plain-group rejection guards only the
    //     genuine rule-position slot (`comments_after_group`), which a trailing `@name` never
    //     populates. This cell asserts the field-rename behavior so a regression there is caught.
    Cell {
        directive: "@name",
        position: "plain-group-trailing",
        spec: "grp = (a: uint) ; @name other\nholder = [grp]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub other: u64"],
            must_not: &["pub a: u64"],
        },
    },
    // ---- @doc --------------------------------------------------------------------------------
    // 12. map field.
    Cell {
        directive: "@doc",
        position: "map-field",
        spec: "docs = {\n  foo: text, ; @doc field comment\n}\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["/// field comment"],
            must_not: &[],
        },
    },
    // 13. array field.
    Cell {
        directive: "@doc",
        position: "array-field",
        spec: "docs = [\n  foo: text, ; @doc field comment\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["/// field comment"],
            must_not: &[],
        },
    },
    // 14. struct-level.
    Cell {
        directive: "@doc",
        position: "struct-level",
        spec: "docs = [\n  foo: text,\n] ; @doc struct documentation here\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["/// struct documentation here"],
            must_not: &[],
        },
    },
    // 15. type-choice variant, FIXED values → /// per variant. PINNED: dropped for the dataless
    //     C-style enum shape. See KNOWN_SILENT_DROP.
    Cell {
        directive: "@doc",
        position: "type-choice-variant",
        spec: "foo = 0 ; @doc about-first\n    / 1 ; @doc about-second\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["/// about-first", "/// about-second"],
            must_not: &[],
        },
    },
    // 16. group-choice arm → /// per variant + type-level ///.
    Cell {
        directive: "@doc",
        position: "group-choice-arm",
        spec: "docs_groupchoice = [\n  ; @name first @doc comment-about-first\n  0, uint //\n  ; @doc comments about second @name second\n  text\n] ; @doc type-level comment\n",
        flags: &[],
        expect: Expect::Effect {
            must: &[
                "/// comment-about-first",
                "/// comments about second",
                "/// type-level comment",
            ],
            must_not: &[],
        },
    },
    // ---- rule-level-only directives ----------------------------------------------------------
    // 17. @newtype → `pub struct Foo`, no `pub type Foo`.
    Cell {
        directive: "@newtype",
        position: "rule",
        spec: "foo = uint ; @newtype\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub struct Foo"],
            must_not: &["pub type Foo"],
        },
    },
    // 18. @newtype with a getter → `pub fn custom_getter`.
    Cell {
        directive: "@newtype-getter",
        position: "rule",
        spec: "foo = uint ; @newtype custom_getter\nholder = [f: foo]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub fn custom_getter"],
            must_not: &[],
        },
    },
    // 19. @no_alias → holder field is the primitive; `InnerAlias` absent (positive control: the
    //     holder struct `Uses` IS present, so the absence is the directive, not a mis-parse).
    Cell {
        directive: "@no_alias",
        position: "rule",
        spec: "inner_alias = uint ; @no_alias\nuses = [field: inner_alias]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["pub struct Uses"],
            must_not: &["InnerAlias"],
        },
    },
    // 20. @used_as_key → Ord/PartialOrd forced into the derive set.
    Cell {
        directive: "@used_as_key",
        position: "rule",
        spec: "keyed = [a: uint, b: text] ; @used_as_key\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["Ord, PartialOrd"],
            must_not: &[],
        },
    },
    // 21. @custom_json (flags: --json-serde-derives=true) → `Cj` lacks the serde impl; the control
    //     `Ctrl` (same shape, no @custom_json) has it.
    Cell {
        directive: "@custom_json",
        position: "rule",
        spec: "cj = uint ; @newtype @custom_json\nctrl = uint ; @newtype\n",
        flags: &["--json-serde-derives=true"],
        expect: Expect::Effect {
            must: &["impl serde::Serialize for Ctrl"],
            must_not: &["impl serde::Serialize for Cj"],
        },
    },
    // ---- @custom_serialize / @custom_deserialize (string-level only) -------------------------
    // 22. type level → the custom fns are called in the generated (de)serialization source.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "type-level",
        spec: "cb = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: cb]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["my_ser(", "my_deser("],
            must_not: &[],
        },
    },
    // 23. field level → same anchors.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "field-level",
        spec: "holder = [f: bytes, ; @custom_serialize my_ser @custom_deserialize my_deser\n]\n",
        flags: &[],
        expect: Expect::Effect {
            must: &["my_ser(", "my_deser("],
            must_not: &[],
        },
    },
];

/// Generate a standalone crate's source map for `spec` (writes to a unique temp `.cddl`).
/// `--wasm=false` EXPLICITLY (the CLI default is true) — this is the string-emit path, so it needs
/// no static dir; the sweep asserts on the concatenated generated source only.
fn generate(
    spec: &str,
    flags: &[&str],
    tag: &str,
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_dslpos_{}_{}.cddl",
        tag,
        std::process::id()
    ));
    std::fs::write(&path, spec).unwrap();
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().unwrap(),
        "--output",
        "dsl_position_unused",
        "--wasm",
        "false",
    ];
    args.extend_from_slice(flags);
    let cli = Cli::parse_from(args);
    let result = crate::api::generated_strings(&cli).map_err(|e| e.to_string());
    std::fs::remove_file(&path).ok();
    result
}

/// The concatenated generated source, or a distinct marker for a graceful `Err` / a `panic!`.
enum Outcome {
    Source(String),
    Error(String),
    Panic,
}

fn run(cell: &Cell, tag: &str) -> Outcome {
    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        generate(cell.spec, cell.flags, tag)
    }));
    match out {
        Ok(Ok(map)) => Outcome::Source(map.into_values().collect::<Vec<_>>().join("\n")),
        Ok(Err(e)) => Outcome::Error(e),
        Err(_) => Outcome::Panic,
    }
}

/// Whether the cell's expectation is SATISFIED by `outcome` (a panic never satisfies).
fn satisfied(cell: &Cell, outcome: &Outcome) -> bool {
    match (&cell.expect, outcome) {
        (Expect::Effect { must, must_not }, Outcome::Source(src)) => {
            must.iter().all(|m| src.contains(m)) && must_not.iter().all(|m| !src.contains(m))
        }
        (Expect::Reject(sub), Outcome::Error(e)) => e.contains(sub),
        _ => false,
    }
}

fn is_pinned(cell: &Cell) -> Option<&'static str> {
    KNOWN_SILENT_DROP
        .iter()
        .find(|(d, p, _)| *d == cell.directive && *p == cell.position)
        .map(|(_, _, reason)| *reason)
}

/// Describe an outcome for a failure message.
fn describe(outcome: &Outcome) -> String {
    match outcome {
        Outcome::Source(src) => format!("generated OK:\n{src}"),
        Outcome::Error(e) => format!("graceful Err: {e}"),
        Outcome::Panic => "PANIC during generation".to_string(),
    }
}

/// The sweep. Every `(directive, position)` cell is asserted against its docs-claimed expectation;
/// pinned cells are asserted to STILL be dropped so the pin flips loudly when a fix lands. A panic is
/// its own failure kind (never a graceful rejection, never a test abort).
#[test]
fn dsl_directive_position_sweep() {
    let mut failures: Vec<String> = Vec::new();

    with_thread_silenced_panics(|| {
        for cell in GRID {
            let tag = format!("{}_{}", cell.directive, cell.position).replace(['+', ' '], "_");
            let outcome = run(cell, &tag);
            let ok = satisfied(cell, &outcome);
            match is_pinned(cell) {
                Some(reason) => {
                    // Pinned: the drop must STILL be present (expectation NOT satisfied).
                    if ok {
                        failures.push(format!(
                            "[{} @ {}] pinned as a silent drop ({reason}) but now SATISFIES its \
                             docs-claimed expectation — a fix landed; remove it from \
                             KNOWN_SILENT_DROP.\n{}",
                            cell.directive,
                            cell.position,
                            describe(&outcome)
                        ));
                    }
                }
                None => {
                    // Not pinned: the docs claim must hold exactly.
                    if !ok {
                        failures.push(format!(
                            "[{} @ {}] does NOT satisfy its docs-claimed expectation. If the comment \
                             placement is the documented one, this is a NEW directive drop — pin it \
                             in KNOWN_SILENT_DROP with a reason and report it (do NOT re-author the \
                             expectation).\n{}",
                            cell.directive,
                            cell.position,
                            describe(&outcome)
                        ));
                    }
                }
            }
        }
    });

    assert!(
        failures.is_empty(),
        "dsl directive × position sweep failures:\n\n{}",
        failures.join("\n\n")
    );
}
