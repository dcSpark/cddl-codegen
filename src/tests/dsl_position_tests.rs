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
//! by re-authoring its expectation, and do not fix a newly-found drop opportunistically. Every fix
//! that has landed here was TRIGGERED — the scoped rule-position `@name` rejection this module's
//! Part 2 lands, the non-last-arm rule-level rejection beside it, and the `@custom_serialize` /
//! `@custom_deserialize` placement rejections (cells 23a–23n) and the `@custom_encodings`
//! declaration's own (cells 23s–23v), each of which was ruled before it was
//! written — and each ships with its own placement CONTROL cell isolating position as the variable.
//! That is the bar the next one has to clear too.
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
/// hand-verifying the placement variants against the docs' comma rules (both the with- and
/// without-trailing-comma spellings), beside a control cell using the same placement in
/// a position where the directive DOES work, isolating position as the variable.
///
/// The list is EMPTY: every cell in the grid now meets its docs-claimed expectation, either by
/// honoring the directive or by refusing it with a message naming the spelling that works. A new
/// entry is therefore a NEW finding — pin it with its reason and report it; do not re-author the
/// cell's expectation to match the drop.
const KNOWN_SILENT_DROP: &[(&str, &str, &str)] = &[];

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
    //    rejection's advertised remedy, working. The comment lands on the enclosing group entry's
    //    trailing slot, which is ALSO the field-rename slot, so the one directive names both the
    //    struct and the field that holds it (`pub inner: Inner`) — both asserted here, since the
    //    dual effect is the position's semantics, not an accident.
    Cell {
        directive: "@name",
        position: "anon-group-member",
        spec: "t = [0, [1, bytes] ; @name inner\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["struct Inner", "pub inner: Inner"],
            must_not: &[],
        },
    },
    // 9c. SCOPE CONTROL for cell 9: the member-position slot is read ONLY when the anonymous array
    //     is the member's WHOLE type. Behind a `.cbor` payload the array is the control operator's
    //     target, so there is no unambiguous construct for the name to land on and the rejection
    //     stands — with its wording unchanged.
    Cell {
        directive: "@name",
        position: "anon-group-member-cbor-payload",
        spec: "t = [0, bytes .cbor [1, bytes] ; @name inner\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("Anonymous groups not allowed"),
    },
    // 9d. SHAPE CONTROL for cell 9: the MAP flavor has no naming door at all (its rejection does
    //     not advertise `@name`), so the member-position slot must not reach it — an inline map
    //     member keeps rejecting with its own message.
    Cell {
        directive: "@name",
        position: "anon-map-member",
        spec: "t = [0, {a: uint} ; @name inner\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("an inline map (`{ a: int, b: uint }`) used as a member"),
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
    // 15. type-choice variant, FIXED values → /// per variant. The dataless (C-style) rendering
    //     builds its `codegen::Variant`s explicitly so each variant's captured `EnumVariant.doc`
    //     lands as a `///` annotation, exactly as the data-carrying rendering does — the codegen
    //     crate has no doc support on variants, so both paths go through `annotation`.
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
    // 15b. TYPE-level `@doc` on the same C-style shape — the sibling of cell 15, and the c-style
    //      analogue of cell 14 (`struct-level`). On a type-choice rule the rule-level doc slot IS
    //      the LAST arm's trailing comment, so one `@doc` there does double duty: it documents the
    //      enum AND that arm's variant (probe-verified against the data-carrying enum, which has
    //      always emitted both). The anchor is the multi-line one (16b's discipline) so it pins the
    //      doc to the ENUM's own position and cannot be satisfied by the variant-level `///` alone.
    Cell {
        directive: "@doc",
        position: "cstyle-rule",
        spec: "foo = 0 ; @name mainnet\n    / 1 ; @name testnet @doc the-enum-doc\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "/// the-enum-doc\n#[derive(Copy, Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]\npub enum Foo {",
            ],
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
    // 21d-21g. REJECT: the TRANSPARENT-ALIAS family. `@custom_json` is consumed exclusively through
    //      `RustStructConfig`, and a rule in this family emits `pub type Foo = …;` — no attribute
    //      site for the derives to be suppressed on, and (the orphan rule) no nominal type a
    //      hand-written `Serialize`/`JsonSchema` could target. The refusal is flag-independent, like
    //      every sibling placement rejection, so these cells stay on the `--wasm=false` baseline with
    //      NO json flags: 21/21a/21b above are the honored controls that carry the flags.
    // 21d. the plain scalar alias.
    Cell {
        directive: "@custom_json",
        position: "rule-type-alias",
        spec: "foo = uint ; @custom_json\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_json on `Foo`: the rule resolves to a transparent alias"),
    },
    // 21e. the `T / null` collapse, which is an `Option<T>` alias rather than an enum. Spelled with
    //      the directive on the LAST arm — the rule slot for a multi-choice rule; the non-last-arm
    //      spelling has its own (earlier, differently-worded) rejection, cell 21c's class.
    Cell {
        directive: "@custom_json",
        position: "option-collapse-rule",
        spec: "foo = uint / null ; @custom_json\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_json on `Foo`: the rule resolves to a transparent alias"),
    },
    // 21f. the TABLE rule. It DOES mint a `RustStruct` (for the wasm wrapper and the keys-list), so
    //      the flag reaches a config — but the rust rule is still the transparent map alias, and no
    //      consumer of `custom_json` reads a table on either side. Caught from the struct config in
    //      the finalize kind-walk, since the table's alias registration drops its metadata.
    Cell {
        directive: "@custom_json",
        position: "table-rule",
        spec: "t = { * uint => bytes } ; @custom_json\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_json on `T`: the rule resolves to a transparent alias"),
    },
    // 21g. the named ARRAY rule — the table's exact sibling (same `new_manual` alias registration,
    //      same inert config), so the same seam and the same message.
    Cell {
        directive: "@custom_json",
        position: "array-rule",
        spec: "al = [* uint] ; @custom_json\nholder = [f: al]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_json on `Al`: the rule resolves to a transparent alias"),
    },
    // 21h. ACCEPTED CONTROL for 21d-21g: the advertised remedy. `@newtype` mints a real wrapper
    //      struct, which is a `RustStructConfig` consumer, so the very same body that rejects one
    //      line up generates with the derives suppressed — attributing 21d-21g to the alias-ness of
    //      the rule and not to the directive being unsupported.
    Cell {
        directive: "@custom_json",
        position: "newtype-over-array-rule",
        spec: "al = [* uint] ; @newtype @custom_json\nctrl = [* uint] ; @newtype\nholder = [f: al, g: ctrl]\n",
        flags: &["--json-serde-derives=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &["impl serde::Serialize for Ctrl"],
            must_not: &["impl serde::Serialize for Al"],
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
    // 23a-f: the six REJECTING placements. The pair is a TYPE-level override keyed on the type whose
    // codec it replaces; each of these positions deletes, bypasses, or never had that type, so the
    // directives used to parse and generate as if absent. Cells 22/23 above are the standing controls
    // (same directives, honored positions) that make each rejection attributable to the PLACEMENT.
    // 23a. REJECT: a `_CDDL_CODEGEN_EXTERN_TYPE_` rule — the named type owns its own impls.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "extern-rule",
        spec: "ext = _CDDL_CODEGEN_EXTERN_TYPE_ ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ext]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject(
            "a _CDDL_CODEGEN_EXTERN_TYPE_ rule names a type this crate does not define",
        ),
    },
    // 23b. REJECT: the open struct-map rest ROW-ENTRY slot (the slot `@name`/`@duplicates`/`@ignore`
    //      legitimately own — those stay live, see the `@name` control in the robustness sibling).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "map-rest-row-entry",
        spec: "opn = {\n  1: uint,\n  * text => uint ; @custom_serialize my_ser @custom_deserialize my_deser\n}\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on the open struct-map rest row (`* k => v`) of rule `opn`"),
    },
    // 23c. REJECT: the open-array rest TAIL entry slot (the array sibling of 23b).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "array-rest-tail-entry",
        spec: "opa = [\n  a: uint,\n  * uint ; @custom_serialize my_ser @custom_deserialize my_deser\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on the open-array rest tail (`* t`) of rule `opa`"),
    },
    // 23d. REJECT: a TABLE's row entry slot. Disjoint from the table RULE's own slot (cell 23g),
    //      which is a separate — and still silent — finding.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "table-row-entry",
        spec: "t = {\n  * text => uint ; @custom_serialize my_ser @custom_deserialize my_deser\n}\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on the table row (`* k => v`) of rule `t`"),
    },
    // 23e. REJECT: with `@no_alias`, which strips the alias node the override keys on (a SYMMETRIC
    //      drop — both directions fall back to default wire, so no round-trip can see it).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "with-no-alias",
        spec: "cb = bytes ; @no_alias @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: cb]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("together with `@no_alias`"),
    },
    // 23f. REJECT: with `@newtype` — not a drop but an ASYMMETRY (deserialize call sites route
    //      through the custom reader, the wrapper writes through its generated impl).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "with-newtype",
        spec: "nt = bytes ; @newtype @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: nt]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("together with `@newtype`"),
    },
    // 23g. REJECT: a TABLE RULE. The rule slot is genuinely read — a rule-trailing
    //      `@duplicates preserve` on this same shape DOES swap in the PairMap twin — so the comment
    //      arrives; what is missing is anything for it to override. A table lowers to a transparent
    //      map alias that owns no codec, so unlike the record rule (23m/23n/23o) there are no impls
    //      for either half to suppress and BOTH halves are equally unhonored, which is why any
    //      presence rejects. The row-entry sibling (23d) is the disjoint slot.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "table-rule",
        spec: "t = {\n  * text => uint\n} ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject(
            "a table rule (`T = { * k => v }`) lowers to a transparent map alias",
        ),
    },
    // 23g-i / 23g-ii. Each half ALONE on a table rule, rejected on its own — the record rule's
    //      both-halves escape (23o) has no table counterpart, so neither spelling may slip through.
    Cell {
        directive: "@custom_serialize",
        position: "table-rule-alone",
        spec: "t = {\n  * text => uint\n} ; @custom_serialize my_ser\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_serialize on `T`: a table rule"),
    },
    Cell {
        directive: "@custom_deserialize",
        position: "table-rule-alone",
        spec: "t = {\n  * text => uint\n} ; @custom_deserialize my_deser\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_deserialize on `T`: a table rule"),
    },
    // 23g-iii. GENERIC table def + one instantiation. The kind-walk runs after generic resolution,
    //      so the instance's materialized struct IS seen and the pair is refused there too. What the
    //      cell pins is WHICH name the message carries: the monomorphized instance
    //      (`PtblU64Bytes`), not the def the directive was written on (`ptbl`) — the same naming a
    //      generic instance gets from every other finalize-seam rejection.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "generic-table-def",
        spec: "ptbl<k0, v0> = {\n  * k0 => v0\n} ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ptbl<uint, bytes>]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on `PtblU64Bytes`: a table rule"),
    },
    // 23h. The pair on the KEY-DOMAIN alias of an open struct-map rest row, honored in BOTH
    //      directions. A custom pair on the domain routes the row to the typed seek path
    //      (`RestRow::map_key_uses_peeked_path` excludes it), so the key is read by
    //      `generate_deserialize` — which is where custom pairs are honored — instead of being
    //      reconstructed from a peek the reader never sees.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "rest-row-key-domain-alias",
        spec: "k = text ; @custom_serialize my_ser @custom_deserialize my_deser\nopn = {\n  1: uint,\n  * k => uint\n}\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["my_ser(", "my_deser("],
            must_not: &[],
        },
    },
    // 23i-23n: the placements whose rejection keys on the MINTED STRUCT's kind rather than on the
    // parse shape, so they fire from `finalize` (which is also what lets them see a generic
    // instance's struct). 23o is the standing accepted-control beside them.
    // 23i. REJECT: a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule — the extern marker's sibling, one class
    //      (as for `@copy`), message naming the marker the rule actually spells.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "raw-bytes-rule",
        spec: "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_ ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: rb]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject(
            "a _CDDL_CODEGEN_RAW_BYTES_TYPE_ rule names a type this crate does not define",
        ),
    },
    // 23j. REJECT: a data-carrying type-choice rule (the `@newtype` asymmetry class, on an enum).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "type-choice-rule",
        spec: "ch = uint ; @name a\n   / text ; @name b @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ch]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("a type-choice rule (`a / b`) mints an enum"),
    },
    // 23k. REJECT: a group-choice rule — a different `RustStructType` than 23j, same class.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "group-choice-rule",
        spec: "gc = [ ; @name a\n  x: uint //\n  ; @name b\n  y: text ] ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: gc]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("a group-choice rule (`{ … } // { … }`) mints an enum"),
    },
    // 23l. REJECT: the dataless C-style enum — a third `RustStructType` reaching the same arm.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "c-style-enum-rule",
        spec: "ce = 0 ; @name zero\n   / 1 ; @name one @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: ce]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("a C-style enum) mints an enum"),
    },
    // 23m. REJECT: `@custom_serialize` ALONE on a record rule — no `Serialize` impl is emitted and the
    //      named function is never called, so the generated crate does not compile.
    Cell {
        directive: "@custom_serialize",
        position: "record-rule-alone",
        spec: "myrec = [a: uint] ; @custom_serialize my_ser\nholder = [f: myrec]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_serialize alone on `Myrec`"),
    },
    // 23n. REJECT: `@custom_deserialize` ALONE on a record rule — the type keeps its generated
    //      `Deserialize` impl while embed sites are rewritten, so one type decodes two ways.
    Cell {
        directive: "@custom_deserialize",
        position: "record-rule-alone",
        spec: "myrec = [a: uint] ; @custom_deserialize my_deser\nholder = [f: myrec]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("@custom_deserialize alone on `Myrec`"),
    },
    // 23o. STILL ACCEPTED (not a pin — an `Effect` cell, which only passes on a SUCCESSFUL
    //      generation): BOTH halves on a record rule suppress the generated impls for the author to
    //      hand-own. That posture is unspecified and at risk, so this cell is doing two jobs — it is
    //      the regression guard that 23m/23n did not swallow the both-set spelling, and it pins what
    //      the spelling does TODAY so a change to it cannot land silently.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "record-rule-both-set",
        spec: "myrec = [a: uint] ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: myrec]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub struct Myrec", "my_deser(raw)"],
            must_not: &["Serialize for Myrec", "Deserialize for Myrec"],
        },
    },
    // ---- @custom_encodings -------------------------------------------------------------------
    // The declaration of a custom codec's own wire. Every cell here runs under
    // `--preserve-encodings=true`: encoding VARIABLES are what it declares, and none exist without
    // that flag (under the block's non-preserve baseline the directive is inert by construction, so
    // a baseline cell would measure nothing). The three honored controls below are what make the
    // rejections attributable to the placement rather than to the flag.
    // 23p. HONORED, type level: the declaration replaces the replaced type's inferred demand. The
    //      anchors are the WHOLE point of the feature — a self-carrying extern infers NOTHING, so
    //      without a declaration there is no `f_encoding` slot at all and the custom wire's header
    //      goes unrecorded (the executed defect this directive dissolves).
    Cell {
        directive: "@custom_encodings",
        position: "type-level",
        spec: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nan_v1 = an ; @custom_serialize my_ser @custom_deserialize my_deser @custom_encodings str\nholder = [f: an_v1]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "pub f_encoding: StringEncoding",
                // split so the anchors survive a rustfmt line wrap between LHS and call
                "let (f, f_encoding) =",
                "my_deser(raw)",
            ],
            must_not: &[],
        },
    },
    // 23q. HONORED, field level, MULTI-variable: `sz,str` mints the positional pair
    //      (`{f}_encoding`, `{f}_encoding2`) and passes both, in declared order.
    Cell {
        directive: "@custom_encodings",
        position: "field-level",
        spec: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nholder = [\n  f: an, ; @custom_serialize my_ser @custom_deserialize my_deser @custom_encodings sz,str\n]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "pub f_encoding: Option<cbor_event::Sz>",
                "pub f_encoding2: StringEncoding",
                // split so the anchors survive a rustfmt line wrap between LHS and call
                "let (f, f_encoding, f_encoding2) =",
                "my_deser(raw)",
            ],
            must_not: &[],
        },
    },
    // 23r. HONORED at a TABLE KEY DOMAIN — the position no record field's config reaches (the table
    //      loop, whose sidecar is keyed by the DECODED key). Pins that the declaration travels
    //      through the same alias lift the pair does, at a position with by-VALUE argument mode.
    Cell {
        directive: "@custom_encodings",
        position: "table-key-domain",
        spec: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nan_v1 = an ; @custom_serialize my_ser @custom_deserialize my_deser @custom_encodings str\nholder = [t: { * an_v1 => uint }]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "pub t_key_encodings: BTreeMap<AnV1, StringEncoding>",
                "let (t_key, t_key_encoding) =",
                "my_deser(raw)",
            ],
            must_not: &[],
        },
    },
    // 23s. REJECT: the declaration with NO pair — it describes the wire of a codec that is not
    //      there, and would otherwise be read into the rule's metadata and dropped.
    Cell {
        directive: "@custom_encodings",
        position: "without-pair",
        spec: "cb = bytes ; @custom_encodings str\nholder = [f: cb]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Reject("no `@custom_serialize`/`@custom_deserialize` is written there"),
    },
    // 23t. REJECT: the declaration with ONE half — the other direction is generated code deriving
    //      the replaced type's own demand, which declared slots contradict slot for slot.
    Cell {
        directive: "@custom_encodings",
        position: "single-half",
        spec: "cb = bytes ; @custom_serialize my_ser @custom_encodings str\nholder = [f: cb]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Reject("only `@custom_serialize` is written there"),
    },
    // 23u. REJECT: the declaration on a rule that mints a STRUCT. Both halves on a record rule is
    //      the one accepted rule-position pair (23o), and the one place a declaration would be read
    //      and then have nowhere to go — a struct carries its encodings inside itself.
    Cell {
        directive: "@custom_encodings",
        position: "record-rule-both-set",
        spec: "myrec = [a: uint] ; @custom_serialize my_ser @custom_deserialize my_deser @custom_encodings sz\nholder = [f: myrec]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Reject("this rule mints a STRUCT"),
    },
    // 23v. REJECT: the ABSENCE of the declaration where it is now required — a pair over a
    //      zero-demand type under `--preserve-encodings`. Keyed on the pair rather than on
    //      `@custom_encodings`, and swept here beside the honored controls because it is the
    //      rejection that MAKES the declaration load-bearing: without it the custom wire's framing
    //      is silently normalized. 23p is the same spec WITH the declaration, so this pair of cells
    //      isolates the declaration as the variable.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "zero-demand-under-preserve",
        spec: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nan_v1 = an ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: an_v1]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Reject("replaces the codec of a type that demands NO encoding variables"),
    },
    // 23w. The same spec WITHOUT `--preserve-encodings`: accepted, generating exactly as it always
    //      has. The refusal above is a preserve-profile contract, not a new ban — "one spec, many
    //      flag sets", the shape `@extern_companions` has without `--wasm`.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "zero-demand-without-preserve",
        spec: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nan_v1 = an ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: an_v1]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["my_ser(", "my_deser("],
            must_not: &[],
        },
    },
    // ---- @custom_wire_major -------------------------------------------------------------------
    // The second member of the wire-facts declaration family: the CBOR major type a custom codec's
    // wire starts with. Its one consumer is an OPEN TABLE's typed-row dispatch, which must know the
    // major BEFORE any deserializer runs — `cbor_types()` there answers about the REPLACED type,
    // whose wire the codec has taken over. Every cell runs under default flags: unlike
    // `@custom_encodings` the declaration's surface does not depend on `--preserve-encodings`.
    // 23x. HONORED: the declared major drives the typed row's dispatch arm — `text`, NOT the `bytes`
    //      the replaced raw-bytes marker would have reported. The two anchors together are the whole
    //      feature: without the declaration this rule is a graceful rejection, and with a naive
    //      `cbor_types()` read it would dispatch on the wrong major and never match.
    Cell {
        directive: "@custom_wire_major",
        position: "open-table-typed-row",
        spec: "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nrb_v1 = rb ; @custom_serialize my_ser @custom_deserialize my_deser @custom_wire_major text\nt = { * rb_v1 => uint, * uint => uint }\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["cbor_event::Type::Text =>", "my_deser(raw)"],
            must_not: &["cbor_event::Type::Bytes =>"],
        },
    },
    // 23y. REJECT: the declaration with NO pair — it declares a fact about a wire no codec writes.
    Cell {
        directive: "@custom_wire_major",
        position: "without-pair",
        spec: "cb = bytes ; @custom_wire_major text\nholder = [f: cb]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("no `@custom_serialize`/`@custom_deserialize` is written there"),
    },
    // 23z. REJECT: the declaration with ONE half.
    Cell {
        directive: "@custom_wire_major",
        position: "single-half",
        spec: "cb = bytes ; @custom_serialize my_ser @custom_wire_major text\nholder = [f: cb]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("only `@custom_serialize` is written there"),
    },
    // 23aa. REJECT: the declaration on a rule that mints a STRUCT — the declared major is read only
    //       through the rule's transparent ALIAS entry, which a struct-minting rule has none of.
    Cell {
        directive: "@custom_wire_major",
        position: "record-rule-both-set",
        spec: "myrec = [a: uint] ; @custom_serialize my_ser @custom_deserialize my_deser @custom_wire_major text\nholder = [f: myrec]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("this rule mints a STRUCT"),
    },
    // 23ab. REJECT (no-silent-directive): the complete pair AND the declaration, on an alias nothing
    //       keys — consumed SOMEWHERE is the contract, and here nothing consumes it.
    Cell {
        directive: "@custom_wire_major",
        position: "unconsumed-alias",
        spec: "cb = bytes ; @custom_serialize my_ser @custom_deserialize my_deser @custom_wire_major text\nholder = [f: cb]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("nothing consumes the declared major"),
    },
    // 23ac. REJECT: the ABSENCE of the declaration where it is REQUIRED — a custom-codec key at an
    //       open table's typed row. 23x is the same spec WITH it, so the pair isolates the
    //       declaration as the variable.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "open-table-typed-row-undeclared-major",
        spec: "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nrb_v1 = rb ; @custom_serialize my_ser @custom_deserialize my_deser\nt = { * rb_v1 => uint, * uint => uint }\nholder = [f: t]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("the codec owns that wire"),
    },
    // ---- @custom_serialize/@custom_deserialize on an ALIAS OF A MARKER RULE ------------------
    // The "this rule IS that type, written differently on the wire" spelling: the pair sits on an
    // alias whose BODY references a marker rule, so it escapes the on-the-marker rejection (23a/23i)
    // and rides the general type-level mechanism — a pair is honored wherever the alias resolves, and
    // this alias resolves to the marker's type. 23a/23i are therefore the placement controls: the
    // SAME directives on the marker RULE reject, while the alias OF it is honored.
    //
    // These two cells are the RAW-BYTES flavor, whose encoding demand is one `StringEncoding` — so
    // the pair infers exactly the signature a string-framed custom wire needs and no declaration is
    // required. The SELF-CARRYING flavor (an alias of a plain `_CDDL_CODEGEN_EXTERN_TYPE_`, demanding
    // nothing) is swept above and NOT duplicated here: 23p/23r are its honored twins at these same two
    // positions WITH `@custom_encodings`, and 23v/23w are its refusal without one. Executed vectors:
    // `tests/alias-of-marker-e2e` (this flavor), `tests/custom-encodings-e2e` (that one).
    // Under `--preserve-encodings=true`, which is where the signature claim lives at all.
    // 23x. HONORED, type level, RECORD FIELD: the field's stored `StringEncoding` is passed by
    //      REFERENCE, and the slot exists because the raw-bytes marker demanded it.
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "raw-bytes-alias-type-level",
        spec: "pid = _CDDL_CODEGEN_RAW_BYTES_TYPE_\npid_v1 = pid ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [f: pid_v1]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                // the alias resolves to the marker's type — no wrapper is minted for the pair to
                // sit on, which is what makes this the Rust-type override
                "pub type PidV1 = Pid;",
                "pub f_encoding: StringEncoding",
                // split so the anchors survive a rustfmt line wrap between LHS and call
                "let (f, f_encoding) =",
                "my_deser(raw)",
                // the by-REFERENCE argument mode this position gives the codec
                ".map(|encs| encs.f_encoding.clone())",
            ],
            must_not: &[],
        },
    },
    // 23y. HONORED at a TABLE KEY DOMAIN: the per-entry sidecar is keyed by the DECODED key (the
    //      marker's type) and passed by VALUE — the mode split that makes one alias reached from both
    //      positions need two functions (`tests/custom-pair-shared-codec` pins that violating it is a
    //      loud generated-crate E0308).
    Cell {
        directive: "@custom_serialize+deserialize",
        position: "raw-bytes-alias-table-key-domain",
        spec: "pid = _CDDL_CODEGEN_RAW_BYTES_TYPE_\npid_v1 = pid ; @custom_serialize my_ser @custom_deserialize my_deser\nholder = [t: { * pid_v1 => uint }]\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &[
                "pub type PidV1 = Pid;",
                "pub t_key_encodings: BTreeMap<PidV1, StringEncoding>",
                "my_ser(serializer, key, t_key_encoding)",
                "let (t_key, t_key_encoding) =",
                "my_deser(raw)",
            ],
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
    // 29. NON-generic extern rule → graceful Reject. The tag names a flavor of a GENERIC instance
    //     (`uses_raw_bytes_flavor` keys on per-instance lookups a non-generic base never gets), so on
    //     a rule that declares no generic parameters there is nothing to flavor and no coherent
    //     honoring exists — the mark was provably inert. The extern-only validity gate (cells 25–28)
    //     rejects NON-extern rules; this one is the second half, gating the extern arm itself on
    //     generic-ness. Its anchor is the generic-ness clause, NOT the shared `only valid on` wording
    //     of cells 25–28 — the two rejections are distinct seams and the substrings must not alias.
    //     The valid `extern-generic-rule` cell (same `; @raw_bytes_flavor` rule-trailing placement,
    //     but generic) is the placement control: it emits the flavor alias, so this cell's rejection
    //     is attributable to generic-vs-non-generic and not to a placement typo.
    Cell {
        directive: "@raw_bytes_flavor",
        position: "non-generic-extern-rule",
        spec: "foo = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("declares no generic parameters"),
    },
    // ---- @extern_companions ------------------------------------------------------------------
    // 34. VALID position (the only one): a LOCALLY-scoped `_CDDL_CODEGEN_EXTERN_TYPE_` rule. The
    //     listed structural companion class is REFERENCED from the declared sibling crate instead of
    //     minted, so the `use` appears and no local `#[wasm_bindgen]` class of that name does — which
    //     is the whole point (two such classes in one cdylib duplicate-symbol at link). `wasm: true`:
    //     the classes it governs are a wasm-boundary concern and do not exist rust-side.
    Cell {
        directive: "@extern_companions",
        position: "local-extern-rule",
        spec: "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=TmList\nholder = [items: [* tm]]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &["use dep_wasm::TmList;", "pub fn items(&self) -> TmList"],
            must_not: &["pub struct TmList"],
        },
    },
    // 34b. UNLISTED companions of the SAME extern still mint locally — the class list is a filter,
    //      not a blanket opt-out, and this is exactly the reported shape (a sibling owns the List
    //      family under its canonical name while the map family is the consumer's). One spec carries
    //      both, so the pair of anchors attributes the difference to the LIST rather than to the
    //      directive's presence.
    Cell {
        directive: "@extern_companions",
        position: "local-extern-rule-unlisted-companion",
        spec: "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=TmList\nholder = {\n  1: uint,\n  * tm => tm\n}\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &["use dep_wasm::TmList;", "pub struct MapTmToTm"],
            must_not: &["pub struct TmList"],
        },
    },
    // 34c. NO-OP WITHOUT --wasm: the SAME tagged spec as cell 34 under the `wasm: false` baseline.
    //      Docs: "inert without `--wasm`". The rust crate names no collection-wrapper CLASS at all
    //      (a `[* tm]` member is a plain `Vec<Tm>`), so there is nothing to defer and nothing to
    //      mint — an Effect cell, not a pin, because the no-op here is CORRECT. The positive control
    //      is cell 34, the same spec with the wasm build on.
    Cell {
        directive: "@extern_companions",
        position: "local-extern-rule-no-wasm",
        spec: "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=TmList\nholder = [items: [* tm]]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub struct Holder"],
            must_not: &["TmList"],
        },
    },
    // 35. REJECT: a rule this crate GENERATES owns its own companions, so the declaration would
    //     silently do nothing (and the classes would still be minted — the failure landing as a
    //     duplicate symbol in a DIFFERENT crate's link).
    Cell {
        directive: "@extern_companions",
        position: "array-struct-rule",
        spec: "foo = [a: uint] ; @extern_companions dep_wasm=FooList\nholder = [f: foo]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject(
            "only valid on a _CDDL_CODEGEN_EXTERN_TYPE_ or _CDDL_CODEGEN_RAW_BYTES_TYPE_ rule",
        ),
    },
    // 36. VALID position (the second one): a LOCALLY-scoped `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule.
    //     A raw-bytes type is user-defined exactly as an extern is, and the generator mints the
    //     SAME structural companion family for it (named from the rule's ident), so a sibling
    //     crate's hand-written `<Name>List` collides with a local mint identically — hence the same
    //     contract. `wasm: true` for the same reason as cell 34: the classes it governs exist only
    //     at the wasm boundary, so a `wasm: false` cell here would read as a silent drop.
    Cell {
        directive: "@extern_companions",
        position: "local-raw-bytes-rule",
        spec: "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_ ; @extern_companions dep_wasm=RbList\nholder = [items: [* rb]]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &["use dep_wasm::RbList;", "pub fn items(&self) -> RbList"],
            must_not: &["pub struct RbList"],
        },
    },
    // 37. REJECT: a multi-choice type rule can never be either marker (its LAST arm is the rule
    //     slot, so the directive IS seen here — this is the directive being invalid, not unseen).
    Cell {
        directive: "@extern_companions",
        position: "type-choice-rule",
        spec: "ch = uint ; @name a\n   / text ; @name b @extern_companions dep_wasm=ChList\nholder = [f: ch]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject(
            "only valid on a _CDDL_CODEGEN_EXTERN_TYPE_ or _CDDL_CODEGEN_RAW_BYTES_TYPE_ rule",
        ),
    },
    // 38. REJECT: a field/member position. This is also the slot a plain-GROUP rule's TRAILING
    //     comment binds to (the `@name plain-group-trailing` seam), so it covers that spelling too.
    Cell {
        directive: "@extern_companions",
        position: "field",
        spec: "tm = _CDDL_CODEGEN_EXTERN_TYPE_\nholder = [\n  f: tm, ; @extern_companions dep_wasm=TmList\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("not a field"),
    },
    // 39. REJECT: a non-last arm of a multi-choice type rule — the shared rejection every rule-level
    //     directive gets from `RuleMetadata::non_variant_directives`, pinned per-directive so a new
    //     directive's omission from that exhaustive list is visible here too.
    Cell {
        directive: "@extern_companions",
        position: "type-choice-non-last-arm",
        spec: "ch = uint ; @name a @extern_companions dep_wasm=ChList\n   / text ; @name b\nholder = [f: ch]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("on a non-last arm of the multi-choice type rule"),
    },
    // 40. REJECT: a listed class that a same-crate RULE also defines. The `use <prefix>::<Class>;`
    //     and the rule's own class would claim one identifier (E0255) — reported in the spec's terms
    //     instead. `wasm: true`: the detector sits in `finalize`'s wasm block beside the four
    //     structural wrapper-name detectors, since the contested name is a wasm class.
    Cell {
        directive: "@extern_companions",
        position: "listed-class-claimed-by-rule",
        spec: "tm = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=TmList\ntm_list = [* tm]\nholder = [f: tm_list]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Reject("also defines `TmList`"),
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
    // 33. FIELD position → graceful Reject. `@used_as_elem` is rule-scoped: it names the TYPE whose
    //     loose-list wasm wrapper to mint. At a field a `; @used_as_elem` binds to field `f`'s
    //     trailing comment, which the rule-level detector never reads — and the tag is only
    //     unambiguous there when the field's type is a bare named reference, so every other member
    //     shape (optional field, inline `[* x]`, primitive) would need its own sub-ruling. A refusal
    //     with a one-line remedy beats a position whose semantics fray, and it matches the family
    //     already rejecting at this exact seam (`@raw_bytes_flavor`, `@copy`, `@extern_companions`,
    //     `@duplicates`, `@ignore` — rule-scoped directives reject at field position, field-scoped
    //     `@name`/`@doc` are honored). The remedy is proven by cell 30 (rule position mints
    //     `BootstrapWitnessList`) and by the same `bw` spec at rule position minting `BwList`.
    //     `wasm: false` (the baseline, unlike cells 30–32): a parse-time rejection fires regardless
    //     of the wasm build, so the cell stays uncoupled to it. The field-trailing comment slot is
    //     proven live by the many `x: T, ; @directive` field cells above (`@name array-element-*`,
    //     `@doc array-field`), so the rejection is attributable to the position, not a placement typo.
    Cell {
        directive: "@used_as_elem",
        position: "field",
        spec: "bw = [vkey: bytes, signature: bytes]\nholder = [\n  f: bw, ; @used_as_elem\n]\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("Put it on the rule that defines the element type"),
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
