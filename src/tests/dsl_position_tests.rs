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
//! `@name` rejection this module's Part 2 lands and the non-last-arm rule-level rejection beside it.
//! Both of those are deliberate, triggered fixes rather than opportunistic ones, and each ships with
//! its own placement CONTROL cell isolating position as the variable — which is the bar a third one
//! has to clear too.
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
    /// Opt this cell into `--wasm=true` generation (the baseline is `--wasm=false`). Only the
    /// `@used_as_elem` cells need it — its whole effect is the wasm-side loose-list wrapper class,
    /// invisible under the rust-only baseline. Every other cell stays `false` so its expectation
    /// isn't coupled to the wasm build.
    wasm: bool,
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
/// Four live findings (none fixed by this task — the scoped fix is rule-position `@name` rejection):
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
///   - `@raw_bytes_flavor` @ `non-generic-extern-rule`: the docs say the tag is valid ONLY on an
///     extern GENERIC, but the extern-only validity gate rejects only NON-extern rules, so on a
///     non-generic `_CDDL_CODEGEN_EXTERN_TYPE_` rule the tag is silently ACCEPTED as a no-op (no
///     generic instances exist to flavor). Control: the valid `extern-generic-rule` cell, same
///     rule-trailing placement but generic — it emits the alias, so the pin isn't vacuous.
///   - `@used_as_elem` @ `field`: a rule-level tag read from rule metadata; a field-trailing
///     `; @used_as_elem` binds to the field's trailing_comments (like the `@name plain-group-trailing`
///     seam), which the rule-level detector never reads, so no wrapper is minted even though the
///     element would mint one at rule position. Control: the field-trailing comment slot is proven
///     live by the `@name array-element-*` / `@doc array-field` cells.
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
    (
        "@raw_bytes_flavor",
        "non-generic-extern-rule",
        "@raw_bytes_flavor on a NON-generic _CDDL_CODEGEN_EXTERN_TYPE_ rule is silently accepted as a \
         no-op: the docs say the tag is valid only on an extern GENERIC, but the validity gate rejects \
         only non-extern rules, so there is no generic instance to flavor and no error fires",
    ),
    (
        "@used_as_elem",
        "field",
        "@used_as_elem is a rule-level tag read from rule metadata; a field-trailing comment binds to \
         the field's trailing_comments, which the rule-level detector never reads, so the loose-list \
         wrapper is silently not minted (the field-position tag is dropped)",
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
        expect: Expect::Reject("does not rename a top-level"),
    },
    // 10b. rule-position @name, single-type-choice STRUCT rule → graceful Reject (the drop is
    //      shape-independent; this cell pins that the seam catches the struct path too).
    Cell {
        directive: "@name",
        position: "rule-type-struct",
        spec: "foo = [a: uint] ; @name bar\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
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
        wasm: false,
        expect: Expect::Reject("does not rename a top-level"),
    },
    Cell {
        directive: "@name",
        position: "rule-type-tnull-trailing",
        spec: "foo = uint / null ; @name bar\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "/// comment-about-first",
                "/// comments about second",
                "/// type-level comment",
            ],
            must_not: &[],
        },
    },
    // 16b. rule-position @doc on a plain ALIAS rule → /// on the emitted `pub type`. The two-line
    //      anchor pins the doc to the alias itself, not just presence somewhere in the source.
    Cell {
        directive: "@doc",
        position: "rule-type-alias",
        spec: "foo = uint ; @doc alias-rule doc-comment\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["/// alias-rule doc-comment\npub type Foo = u64;"],
            must_not: &[],
        },
    },
    // 16c. rule-position @doc on a NEWTYPE rule → /// on the emitted wrapper struct.
    Cell {
        directive: "@doc",
        position: "rule-newtype",
        spec: "foo = uint ; @newtype @doc newtype-rule doc-comment\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["/// newtype-rule doc-comment"],
            must_not: &[],
        },
    },
    // 16d. rule-position @doc on a named `[+ T]` rule → COMPOSED doc: user line first, then the
    //      synthesized non-empty bound note, in one doc block on the same alias (the codegen
    //      crate's `.doc()` replaces rather than appends, so composition must join into a single
    //      call — this anchor would catch a second `.doc()` clobbering either half).
    Cell {
        directive: "@doc",
        position: "rule-nonempty-array-alias",
        spec: "foo_list = [+ uint] ; @doc user-half of composed doc\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["/// user-half of composed doc\n/// `[+ u64]`: at least one element"],
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
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
        wasm: false,
        expect: Expect::Effect {
            must: &["impl serde::Serialize for Ctrl"],
            must_not: &["impl serde::Serialize for Cj"],
        },
    },
    // 21a. @custom_json on a SUM-TYPE rule (flags: --preserve-encodings=true --json-serde-derives=true).
    //      The enum gets no serde derive (custom_json suppresses it), so its variant encoding fields
    //      must NOT carry `#[serde(skip)]` — an unregistered serde helper attribute makes the crate
    //      fail `cargo check` ("cannot find attribute `serde`"). Control `CtrlSum` (no @custom_json,
    //      same flags) still emits `#[serde(skip)]` on its encoding fields, attributing the absence to
    //      the directive rather than a missing flag. The `@custom_json` merges onto the whole rule via
    //      the last type-choice variant's trailing comment.
    Cell {
        directive: "@custom_json",
        position: "type-choice-rule",
        spec: "my_sum =\n    uint ; @name integer\n  / bytes ; @name raw @custom_json\nctrl_sum =\n    uint ; @name cint\n  / bytes ; @name craw\n",
        flags: &["--preserve-encodings=true", "--json-serde-derives=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "integer_encoding: Option<cbor_event::Sz>",
                "#[serde(skip)]\n        cint_encoding",
            ],
            must_not: &[
                "#[serde(skip)]\n        integer_encoding",
                "#[serde(skip)]\n        raw_encoding",
            ],
        },
    },
    // 21a-schemars. The SAME type-choice rule as 21a, under the JSON-SCHEMA flags instead of the
    //      preserve ones. 21a pins the SERDE half of the suppression (no `#[serde(skip)]` on the
    //      variant encoding fields); this pins the `schemars::JsonSchema` half. They are one branch in
    //      the emitter (`add_struct_derives`'s single `if !custom_json`), so this is not redundant
    //      coverage of that branch — it is coverage of a distinct promise a consumer depends on
    //      SEPARATELY: a crate hand-writing its JSON impls needs the schemars derive specifically
    //      absent, and 21a's `#[serde(skip)]` anchor is invisible without --preserve-encodings, so
    //      nothing else in the grid would notice the schemars derive coming back. Control `CtrlSum`
    //      (no @custom_json, same flags) keeps the derive, attributing the absence to the directive
    //      rather than to a missing flag. The derive list is emitted in insertion order (the `codegen`
    //      crate pushes onto a `Vec` and never sorts), so `schemars::JsonSchema` is last and the
    //      anchor form mirrors 21b's.
    Cell {
        directive: "@custom_json",
        position: "type-choice-rule-schemars",
        spec: "my_sum =\n    uint ; @name integer\n  / bytes ; @name raw @custom_json\nctrl_sum =\n    uint ; @name cint\n  / bytes ; @name craw\n",
        flags: &["--json-serde-derives=true", "--json-schema-export=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &["schemars::JsonSchema)]\npub enum CtrlSum {"],
            must_not: &["schemars::JsonSchema)]\npub enum MySum {"],
        },
    },
    // 21b. @custom_json on a RECORD-STRUCT (map group) rule, same flags. The struct's serde/schemars
    //      derives AND its preserve-encodings `encodings` field's `#[serde(skip)]` must BOTH be
    //      suppressed — they're only coherent together (derives without the skip demand serde impls
    //      for the encoding struct, E0277; the skip without derives is an unregistered attribute).
    //      Control `CtrlRec` (no @custom_json) keeps both — positive control in the same spec.
    Cell {
        directive: "@custom_json",
        position: "map-group-rule",
        spec: "my_rec = { 0: uint } ; @custom_json\nctrl_rec = { 1: uint }\n",
        flags: &["--preserve-encodings=true", "--json-serde-derives=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "pub encodings: Option<MyRecEncoding>",
                "#[serde(skip)]\n    pub encodings: Option<CtrlRecEncoding>",
                "serde::Serialize)]\npub struct CtrlRec {",
            ],
            must_not: &[
                "#[serde(skip)]\n    pub encodings: Option<MyRecEncoding>",
                "serde::Serialize)]\npub struct MyRec {",
            ],
        },
    },
    // 21c. A rule-level directive on a NON-LAST arm of a multi-choice type rule. The rule slot is
    //      the LAST arm's trailing comment (`parse_type_choices` reads `type_choices.last()` and
    //      nothing else); `create_variants_from_type_choices` consumes only `.name`/`.comment` per
    //      choice, so anything else written on an earlier arm used to generate exit-0 output
    //      identical to omitting it. Now a graceful rejection. The placement CONTROL is cell 21a
    //      (`type-choice-rule`), whose directive sits on the LAST arm of the same rule shape and
    //      takes effect — isolating arm position as the variable, per this module's authoring rule.
    //      `@used_as_key` rather than `@custom_json` because its effect is visible on the
    //      --wasm=false baseline, so the cell is not flag-conditional.
    Cell {
        directive: "@used_as_key",
        position: "type-choice-non-last-arm",
        spec: "my_sum =\n    uint ; @name integer @used_as_key\n  / bytes ; @name raw\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on a non-last arm of the multi-choice type rule"),
    },
    // 21d. The exclusion set of 21c's rejection: `@name` and `@doc` are what a type-choice VARIANT
    //      position legitimately consumes, so they must keep working on a non-last arm. Without this
    //      cell, widening the rejection to every directive would pass 21c and silently break the
    //      overwhelmingly common spelling (three of the four arms of a real `plutus_data` carry
    //      `@name`). Anchored on the generated constructor rather than the variant token: the ctor
    //      name comes from `EnumVariant::name_as_var()`, and the un-named control spelling for a
    //      `uint` arm is `new_uint` — so the must/must_not pair cannot both hold by accident.
    Cell {
        directive: "@name",
        position: "type-choice-non-last-arm-allowed",
        spec: "my_sum =\n    uint ; @name integer\n  / bytes ; @name raw\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub fn new_integer("],
            must_not: &["pub fn new_uint("],
        },
    },
    // ---- @custom_serialize / @custom_deserialize (string-level only) -------------------------
    // 22. type level → the custom fns are called in the generated (de)serialization source.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "type-level",
        spec: "cb = bytes ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: cb]\n",
        flags: &[],
        wasm: false,
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
        wasm: false,
        expect: Expect::Effect {
            must: &["my_ser(", "my_deser("],
            must_not: &[],
        },
    },
    // ---- @raw_bytes_flavor -------------------------------------------------------------------
    // 24. VALID position (the only one): a `_CDDL_CODEGEN_EXTERN_TYPE_` GENERIC. One spec carries
    //     BOTH a raw-bytes-argument instance (`ext_set<pub_key>` → the `ExtSetRawBytes<PubKey>`
    //     flavor) and a non-raw control instance (`ext_set<plain>` → the plain `ExtSet<Plain>`), so
    //     this doubles as the sweep-side positive control: it proves the Reject cells below fail for
    //     the extern-only rule, not for some unrelated parse error. Anchors are chosen so neither can
    //     substring-match the other (`= ExtSetRawBytes<` never contains `= ExtSet<`, and vice-versa).
    //     Rust-only (`wasm: false`) — the fixture `tests/extern-generic-raw-bytes` uses `--wasm=false`
    //     too: the alias/glue is emitted rust-side regardless of the wasm build.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "extern-generic-rule",
        spec: "pub_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_\next_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor\nplain = [a: uint, b: text]\nusing = [keys: ext_set<pub_key>, plains: ext_set<plain>]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["= ExtSetRawBytes<PubKey>", "= ExtSet<Plain>"],
            must_not: &[],
        },
    },
    // 25. array-struct rule → hard error (docs: "hard error on any rule other than an extern").
    //     Not covered by `raw_bytes_flavor_misuse_rejects_gracefully`'s three seams (single-choice
    //     type, multi-choice type, field), so pinned here per attachment position.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "array-struct-rule",
        spec: "foo = [a: uint] ; @raw_bytes_flavor\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only valid on"),
    },
    // 26. map-struct rule → hard error.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "map-struct-rule",
        spec: "foo = {0: uint} ; @raw_bytes_flavor\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only valid on"),
    },
    // 27. group-choice rule → hard error.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "group-choice-rule",
        spec: "foo = [ a: uint // b: tstr ] ; @raw_bytes_flavor\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only valid on"),
    },
    // 28. plain-GROUP rule, trailing position → hard error, but via the FIELD seam: like the
    //     `@name plain-group-trailing` finding, cddl binds the trailing `; @raw_bytes_flavor` to the
    //     LAST group entry (field `a`), so the FIELD rejection ("only valid on … not a field") fires
    //     rather than a rule-level one. The docs' hard-error claim still holds (`only valid on`), so
    //     this is a Reject cell, not a pin. The plain-group-trailing @name cell is the placement
    //     control proving this trailing slot binds to the last field.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "plain-group-trailing",
        spec: "grp = (a: uint) ; @raw_bytes_flavor\nholder = [grp]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only valid on"),
    },
    // 29. NON-generic extern rule. FINDING (pinned in KNOWN_SILENT_DROP): the docs say the tag is
    //     valid ONLY on an extern GENERIC, but on a non-generic `_CDDL_CODEGEN_EXTERN_TYPE_` rule it
    //     is silently ACCEPTED as a no-op — there are no generic instances to flavor, and the
    //     extern-only validity gate only rejects NON-extern rules, so a non-generic extern slips
    //     through unerrored. Docs-claimed expectation (Reject) is NOT satisfied → the pin holds and
    //     flips the day the gate learns to reject the non-generic case. The valid `extern-generic-rule`
    //     cell (same `; @raw_bytes_flavor` rule-trailing placement, but generic) is the placement
    //     control — it emits the flavor alias, so the pin cannot hold vacuously on a placement typo;
    //     the only isolated variable is generic-vs-non-generic.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "non-generic-extern-rule",
        spec: "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only valid on"),
    },
    // ---- @used_as_elem (wasm-side loose-list wrapper; needs `wasm: true`) ---------------------
    // 30. EFFECT, rule position: a non-exposable struct rule tagged `@used_as_elem` with NO inline
    //     `[* x]` usage anywhere mints the loose-list wrapper class + its `collections.rs` index
    //     entry. The untagged sibling `sibling` is the positive control: it mints no `SiblingList`,
    //     attributing the wrapper to the directive rather than the wasm build. `wasm: true` (the whole
    //     effect is wasm-side).
    Cell {
        directive: "@used_as_elem",
        position: "rule",
        spec: "bootstrap_witness = [\n  vkey: bytes,\n  signature: bytes,\n] ; @used_as_elem\nsibling = [\n  a: uint,\n  b: text,\n]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &[
                "pub struct BootstrapWitnessList(",
                "pub use crate::generated::BootstrapWitnessList;",
            ],
            must_not: &["SiblingList"],
        },
    },
    // 31. REJECT, directly-wasm-exposable element: `[* coin]` lowers to a bare `Vec<Coin>` with no
    //     wrapper class, so there is nothing to mint → graceful hard error. `wasm: true` (the
    //     exposability check only runs on the wasm path).
    Cell {
        directive: "@used_as_elem",
        position: "rule-exposable-element",
        spec: "coin = uint ; @used_as_elem\nroot = [c: coin]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Reject("directly wasm-exposable"),
    },
    // 32. NO-OP WITHOUT --wasm: the SAME tagged spec as cell 30 under the `wasm: false` baseline.
    //     Docs: "It is a no-op without `--wasm`." Generation succeeds and the rust struct is present,
    //     but NO list wrapper appears anywhere (the wrapper is a wasm-boundary concern only). Pins the
    //     documented no-op posture — an Effect cell, not a pin, because a no-op here is CORRECT.
    Cell {
        directive: "@used_as_elem",
        position: "rule-no-wasm",
        spec: "bootstrap_witness = [\n  vkey: bytes,\n  signature: bytes,\n] ; @used_as_elem\nsibling = [\n  a: uint,\n  b: text,\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub struct BootstrapWitness {"],
            must_not: &["BootstrapWitnessList"],
        },
    },
    // 33. FIELD position. FINDING (pinned in KNOWN_SILENT_DROP): `@used_as_elem` is a rule-level tag
    //     read from rule metadata; a field-trailing `; @used_as_elem` binds to field `f`'s trailing
    //     comment (like the `@name plain-group-trailing` field seam) which the rule-level detector
    //     never reads, so it is silently DROPPED — no `BwList` wrapper is minted even though the
    //     element (`bw`, a non-exposable struct) WOULD mint one at rule position. Docs-claimed effect
    //     (a wrapper for the tagged element) is NOT satisfied → the pin holds. `wasm: true` so the
    //     wrapper WOULD be observable if the tag were honored — ruling out "invisible because rust-
    //     only". The field-trailing comment slot is proven live by the many `x: T, ; @directive` field
    //     cells above (`@name array-element-*`, `@doc array-field`), so the drop is the rule-level
    //     detector ignoring the field comment, not a placement typo.
    Cell {
        directive: "@used_as_elem",
        position: "field",
        spec: "bw = [vkey: bytes, signature: bytes]\nholder = [\n  f: bw, ; @used_as_elem\n]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &["pub struct BwList("],
            must_not: &[],
        },
    },
];

/// Generate a standalone crate's source map for `spec` (writes to a unique temp `.cddl`).
/// `--wasm` defaults to `false` (the string-emit path needs no static dir; the sweep asserts on the
/// concatenated generated source only) but a cell can opt into `--wasm=true` — `generated_strings`
/// emits the wasm files as strings too, so the wasm-side anchors (`@used_as_elem`'s loose-list
/// wrapper) are reachable without a static dir.
fn generate(
    spec: &str,
    flags: &[&str],
    wasm: bool,
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
        if wasm { "true" } else { "false" },
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
        generate(cell.spec, cell.flags, cell.wasm, tag)
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
    for (directive, position, _) in KNOWN_SILENT_DROP {
        assert!(
            GRID.iter()
                .any(|cell| cell.directive == *directive && cell.position == *position),
            "KNOWN_SILENT_DROP names cell `{directive}` at `{position}` that is no longer swept — \
             stale pin, remove or fix it"
        );
    }

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
