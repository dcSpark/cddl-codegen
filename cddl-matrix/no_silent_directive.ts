/**
 * no_silent_directive.ts — the directive×rule-shape silent-drop net.
 *
 * The comment-DSL directives (`@newtype`, `@duplicates`, …) each change codegen, but a directive is
 * only carried to a marking site by the parse path its rule's SHAPE takes, and those paths differ.
 * Three shipped gaps at the `@newtype` wrapper seam shared one symptom: a written directive produced
 * output BYTE-IDENTICAL to omitting it, with no notice or rejection acknowledging it (a silent
 * drop). This gate is the systematic catch that would have flagged all three without predicting
 * them — and it sweeps the whole product rather than a hand corpus, so a shape whose parse path
 * nobody thought about is covered by construction.
 *
 * Mechanism — for each (shape, directive) cell, generate the built binary TWICE into throwaway
 * scratch dirs under the directive's WITNESS PROFILE (the minimal flag set where its surface exists
 * at all): once with the base directives only, once with the toggled directive ADDED. A cell is a
 * silent drop — FAIL — iff:
 *   (a) both runs succeeded and produced BYTE-IDENTICAL generated source, AND
 *   (b) the with-directive run's stdout+stderr contains no mention of the directive (no notice, no
 *       rejection) that would acknowledge it.
 * A byte DIFFERENCE (the directive changed output) or a nonzero with-directive exit (the directive
 * was loudly rejected) is a PASS — the directive was honored, not silenced. Legitimate
 * byte-identical accepted no-ops live on the visible ALLOWLIST below, which doubles as the
 * accepted-no-op inventory — each entry carries a one-line justification.
 *
 * Three axes, each with its own authority:
 *   - DIRECTIVE: extracted at run time from `src/comment_ast.rs`'s `KNOWN_RULE_METADATA_TAGS`, so a
 *     new directive DEMANDS classification (a canonical-spelling row and a witness-profile row)
 *     rather than silently skipping the product. That forcing function is the point.
 *   - SHAPE: hand-enumerated below — the parse paths a rule body can take. Each shape carries a
 *     holder embedding so generation is actually exercised.
 *   - PROFILE: the flag set under which a directive's surface exists (`@used_as_elem` is a
 *     documented no-op without `--wasm`; `@no_json_schema_export` suppresses a json-gen row only).
 *     The byte surface is EVERY generated file under the output dir, so a profile's extra crates are
 *     compared too and no per-directive surface selection is needed.
 *
 * The arm-position axis is folded in as its own shapes (a directive on a NON-LAST arm of a
 * multi-choice rule, and of a `T / null` Option collapse): the rule slot is the LAST arm's trailing
 * comment, so a directive on an earlier arm is built and discarded unless something rejects it.
 *
 * Tier: `local` (never `fast`/CI — CI cost policy). No `--check` mode: this gate has no drift
 * artifact, it just runs.
 */
import { mkdtempSync, rmSync, existsSync, readdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, relative, resolve } from "node:path";

const HERE = import.meta.dir;
const CODEGEN_DIR = resolve(HERE, "..");
const STATIC_DIR = join(CODEGEN_DIR, "static");
const BIN = join(CODEGEN_DIR, "target", "debug", "cddl-codegen");
/** How many generator invocations to keep in flight (each is a short single-threaded process). */
const CONCURRENCY = Number(process.env.NO_SILENT_DIRECTIVE_JOBS ?? "8");

// ---- axis 1: the witness profiles ------------------------------------------------------------

type ProfileId = "default" | "json" | "wasm";

/** The minimal flag set under which each profile's surface exists. */
const PROFILES: Record<ProfileId, string[]> = {
  default: ["--wasm=false"],
  json: ["--wasm=false", "--json-serde-derives=true", "--json-schema-export=true"],
  wasm: ["--wasm=true"],
};

// ---- axis 2: the directives (authority = comment_ast.rs) --------------------------------------

/** Extract `KNOWN_RULE_METADATA_TAGS` from the authority. Deliberately NOT the `tag("@…")` literals
 *  corpus_detect.ts reads: this gate wants the RULE-POSITION vocabulary, which that const is, and
 *  reading a different spelling keeps the two gates from sharing a single point of rot. */
function knownRuleMetadataTags(): string[] {
  const src = readFileSync(join(CODEGEN_DIR, "src", "comment_ast.rs"), "utf8");
  const block = src.match(/pub const KNOWN_RULE_METADATA_TAGS: &\[&str\] = &\[([\s\S]*?)\n\];/);
  if (!block) throw new Error("no_silent_directive: could not find KNOWN_RULE_METADATA_TAGS in src/comment_ast.rs");
  const tags = [...block[1].matchAll(/"(@[a-z_]+)"/g)].map(m => m[1]);
  if (tags.length < 10) throw new Error(`no_silent_directive: extracted only ${tags.length} directive(s) — the extraction went vacuous`);
  return tags;
}

/** Canonical spelling per directive — the form a spec author actually writes. A directive whose
 *  argument is REQUIRED (`@duplicates`, `@extern_companions`, `@custom_serialize`, …) must carry a
 *  valid one: comment_ast PANICS on a missing/unknown argument, which would make every cell for that
 *  directive a fixture bug rather than a measurement. */
const SPELLINGS: Record<string, string> = {
  "@name": "@name renamed_foo",
  "@rust_name": "@rust_name RenamedFoo",
  "@newtype": "@newtype",
  "@no_alias": "@no_alias",
  "@used_as_key": "@used_as_key",
  "@used_as_elem": "@used_as_elem",
  "@copy": "@copy",
  "@raw_bytes_flavor": "@raw_bytes_flavor",
  "@ignore": "@ignore",
  "@duplicates": "@duplicates reject",
  "@custom_json": "@custom_json",
  "@no_json_schema_export": "@no_json_schema_export",
  "@custom_serialize": "@custom_serialize my_serialize",
  "@custom_deserialize": "@custom_deserialize my_deserialize",
  "@extern_companions": "@extern_companions dep_wasm=FooList",
  "@doc": "@doc explains the rule",
};

/** The witness profile per directive: the CHEAPEST flag set under which the directive's surface
 *  exists. Generating a cell outside its witness profile measures nothing — the directive would be
 *  byte-identical by construction, which is an inert cell masquerading as a finding. */
const WITNESS_PROFILE: Record<string, ProfileId> = {
  "@name": "default",
  "@rust_name": "default",
  "@newtype": "default",
  "@no_alias": "default",
  "@used_as_key": "default",
  "@copy": "default",
  "@raw_bytes_flavor": "default",
  "@ignore": "default",
  "@duplicates": "default",
  "@custom_serialize": "default",
  "@custom_deserialize": "default",
  "@doc": "default",
  // json-gen-crate surfaces: `@custom_json` suppresses serde/schemars derives, and
  // `@no_json_schema_export` suppresses a schema-registration row that only exists under the flags.
  "@custom_json": "json",
  "@no_json_schema_export": "json",
  // wasm-only surfaces: `@used_as_elem` mints a wasm list wrapper; `@extern_companions` defers wasm
  // companion classes to a sibling crate.
  "@used_as_elem": "wasm",
  "@extern_companions": "wasm",
};

// ---- axis 3: the rule shapes -------------------------------------------------------------------

interface Shape {
  id: string;
  /** human shape description for the FAIL message */
  desc: string;
  /** lines emitted BEFORE the annotated rule (a generic base to instantiate, …) */
  prelude?: string[];
  /** the annotated rule's head; `foo` unless the rule declares generic parameters */
  head?: string;
  /** the annotated rule's body. Ignored when `arms` is set. */
  body?: string;
  /** render as a MULTI-LINE type choice over `list`, placing the toggled directive on `toggledArm`.
   *  The rule slot is the LAST arm's trailing comment, so `toggledArm < list.length - 1` is the
   *  arm-position axis and `toggledArm === list.length - 1` is the ordinary rule slot. */
  arms?: { list: string[]; toggledArm: number };
  /** the holder embeddings that make generation exercise the rule — one variant per entry. */
  holders: string[][];
}

/** The holder embeddings each shape is swept under. A cell is called a silent drop only if the
 *  directive is inert under EVERY one of them, because the embedding decides whether the thing a
 *  directive acts on exists at all — and the two requirements point in OPPOSITE directions, so no
 *  single embedding can witness both:
 *    - a CONTAINER use (`g: [* foo]`) is what mints the class `@extern_companions` defers, and the
 *      list getter whose boundary clone `@copy` elides. Probed: `@extern_companions` on an extern
 *      rule is byte-identical under `--wasm=true` with a member-only holder, and changes two wasm
 *      files once the array use is there.
 *    - the ABSENCE of one is what `@used_as_elem` exists for — it forces the wasm list wrapper for a
 *      rule nothing uses as an element, so with a container use already present it is a no-op.
 *  Sweeping both removes the need to classify which a directive needs, and makes a FAIL mean
 *  "inert under both", which is what a finding has to mean to be actionable.
 */
const HOLDERS: string[][] = [["holder = [f: foo]"], ["holder = [f: foo, g: [* foo]]"]];
/** The same, for shapes whose annotated rule is a generic BASE and whose usable type is its
 *  instantiation. */
const INST_HOLDERS: string[][] = [
  ["inst = foo<uint>", "holder = [f: inst]"],
  ["inst = foo<uint>", "holder = [f: inst, g: [* inst]]"],
];

/** The shape axis. The first ten are the mandatory parse paths (a rule body reaches a marking site
 *  through exactly one of them); the rest each proved interesting in a prior delivery. Spellings are
 *  reused verbatim from `robustness_tests::no_json_schema_export_misuse_rejects_gracefully`, which
 *  already had to name most of these, so the two enumerations cannot drift on what a shape IS. */
const SHAPES: Shape[] = [
  // -- the mandatory nine (with single-choice split into its transparent and struct halves) -------
  { id: "alias", desc: "scalar transparent alias (`foo = uint`)", body: "uint", holders: HOLDERS },
  { id: "record", desc: "record type rule (`foo = [x: uint]`)", body: "[x: uint]", holders: HOLDERS },
  {
    id: "multi_choice",
    desc: "3-arm multi-choice type rule, directive at the rule slot (the LAST arm)",
    // Three arms rather than two, deliberately: a two-arm choice collides with the `T / null`
    // collapse and the two-arm 258-set idiom, both of which take different paths (each is its own
    // shape below).
    arms: { list: ["uint", "tstr", "bytes"], toggledArm: 2 },
    holders: HOLDERS,
  },
  { id: "tagged", desc: "tagged type rule (`foo = #6.42(uint)`)", body: "#6.42(uint)", holders: HOLDERS },
  { id: "parenthesized", desc: "parenthesized type rule (`foo = (uint)`)", body: "(uint)", holders: HOLDERS },
  {
    id: "plain_group_unspliced",
    desc: "plain group rule nobody splices (materializes no struct)",
    body: "(a: uint, b: uint)",
    holders: [["holder = [z: uint]"]],
  },
  {
    id: "plain_group_spliced",
    desc: "plain group rule SPLICED into a holder (`parse_rule`'s Rule::Group arm)",
    body: "(a: uint, b: uint)",
    holders: [["holder = [foo]"]],
  },
  {
    id: "generic_definition",
    desc: "generic definition (`foo<T> = [x: T]`), annotated on the BASE",
    head: "foo<T>",
    body: "[x: T]",
    holders: INST_HOLDERS,
  },
  {
    id: "generic_instance",
    desc: "generic instance (`foo = base<uint>`), annotated on the INSTANCE",
    prelude: ["base<T> = [x: T]"],
    body: "base<uint>",
    holders: HOLDERS,
  },
  {
    id: "extern",
    desc: "local extern marker rule (`_CDDL_CODEGEN_EXTERN_TYPE_`)",
    body: "_CDDL_CODEGEN_EXTERN_TYPE_",
    holders: HOLDERS,
  },
  {
    id: "raw_bytes",
    desc: "raw-bytes marker rule (`_CDDL_CODEGEN_RAW_BYTES_TYPE_`)",
    body: "_CDDL_CODEGEN_RAW_BYTES_TYPE_",
    holders: HOLDERS,
  },
  // -- the recommended extras ---------------------------------------------------------------------
  { id: "table", desc: "table rule (`foo = { * uint => tstr }`)", body: "{ * uint => tstr }", holders: HOLDERS },
  { id: "array_typedef", desc: "array typedef (`foo = [* uint]`)", body: "[* uint]", holders: HOLDERS },
  {
    id: "option_collapse",
    desc: "`T / null` Option-collapse rule, directive at the rule slot (the LAST arm)",
    arms: { list: ["uint", "null"], toggledArm: 1 },
    holders: HOLDERS,
  },
  {
    id: "two_arm_258_set",
    desc: "collapsed two-arm 258 set idiom (nominalized)",
    body: "#6.258([* uint]) / [* uint]",
    holders: HOLDERS,
  },
  {
    id: "set_nominal_binding",
    desc: "named binding to a set nominal (`foo = gset<uint>`)",
    prelude: ["gset<T> = #6.258([* T]) / [* T]"],
    body: "gset<uint>",
    holders: HOLDERS,
  },
  {
    id: "generic_extern_base",
    desc: "generic-extern base (`foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`)",
    // The raw-bytes rule is in the prelude for every variant so the baselines differ only in the
    // holder; the third variant instantiates OVER it, which is the only context in which
    // `@raw_bytes_flavor` has a witness at all (it selects the `<ExternName>RawBytes` flavor for
    // instances whose argument is a raw-bytes type). Probed: with a `foo<uint>` instantiation the
    // tag is byte-identical, and with `foo<rb>` it renames the emitted `pub type` target — so a
    // uint-only fixture would have reported the tag as silently dropped on the ONE placement its
    // own refusal message advertises as the remedy.
    prelude: ["rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_"],
    head: "foo<T>",
    body: "_CDDL_CODEGEN_EXTERN_TYPE_",
    holders: [...INST_HOLDERS, ["inst = foo<rb>", "holder = [f: inst]"]],
  },
  {
    id: "c_style_enum",
    desc: "c-style enum rule (`foo = 0 / 1 / 2`)",
    body: "0 / 1 / 2",
    holders: HOLDERS,
  },
  {
    id: "open_struct_map",
    desc: "open struct-map rule with a rest entry (`{ 1: uint, * uint => any }`)",
    body: "{ 1: uint, * uint => any }",
    holders: HOLDERS,
  },
  // -- the arm-position axis, folded in as shapes --------------------------------------------------
  {
    id: "multi_choice_non_last_arm",
    desc: "3-arm multi-choice type rule, directive on a NON-LAST arm (variant metadata, not the rule slot)",
    arms: { list: ["uint", "tstr", "bytes"], toggledArm: 0 },
    holders: HOLDERS,
  },
  {
    id: "option_collapse_non_last_arm",
    desc: "`T / null` Option-collapse rule, directive on a NON-LAST arm (the collapse has no variants)",
    arms: { list: ["uint", "null"], toggledArm: 0 },
    holders: HOLDERS,
  },
];

// ---- the hand corpus (kept: each cell pins a specific shipped regression or placement control) ---

/** A hand cell: the toggled directive is APPENDED to `base` (which may already carry some). */
interface HandCell {
  id: string;
  /** the rule body, e.g. `#6.258([* uint])` */
  ruleBody: string;
  /** directives always present (both runs), e.g. `["@newtype"]` */
  base: string[];
  /** the directive under test, ADDED for the second run, e.g. `@duplicates reject` */
  toggled: string;
  /** human shape description for the FAIL message */
  shape: string;
  /** When present, the rule body is rendered as a MULTI-LINE type choice over `arms`, and the
   *  toggled directive is placed on `arms[toggledArm]` instead of at rule position. */
  armPlacement?: { arms: string[]; toggledArm: number };
}

// The first two cells reproduce shipped wrapper-seam gaps (each byte-identical with/without the
// toggled directive BEFORE the Phase-2.1 fixes, distinct AFTER); the third pins that a custom
// `@newtype <name>` getter is honored on a nominalized two-arm 258 set (Phase 2.2 subsumed the gap-3
// rejection — bare `@newtype` on a set nominal is now an accepted no-op, allowlisted below); the
// rest are the allowlisted accepted-no-op controls and the arm-placement controls that prove the
// gate does not simply pass everything. All run under the `default` profile.
const HAND_CORPUS: HandCell[] = [
  {
    id: "single_arm_258_newtype_preserve_optout",
    ruleBody: "#6.258([* uint])",
    base: ["@newtype"],
    toggled: "@duplicates preserve",
    shape: "single-arm #6.258 array @newtype wrapper",
  },
  {
    id: "plain_newtype_reject",
    ruleBody: "[* uint]",
    base: ["@newtype"],
    toggled: "@duplicates reject",
    shape: "plain [* a] @newtype wrapper",
  },
  {
    // Phase 2.2: the two-arm 258 idiom nominalizes; a custom `@newtype <name>` getter is honored on the
    // set nominal (adds `pub fn entries(..)`), so toggling it changes output. (Bare `@newtype` on a set
    // nominal is a no-op — no getter, to avoid the Deref shadow — covered by the allowlist cell below.)
    id: "two_arm_258_idiom_newtype_named_getter",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@newtype entries",
    shape: "collapsed two-arm 258 set idiom (nominalized)",
  },
  {
    id: "plain_array_preserve",
    ruleBody: "[* uint]",
    base: [],
    toggled: "@duplicates preserve",
    shape: "plain non-258 [* a] array",
  },
  {
    id: "two_arm_258_idiom_reject",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@duplicates reject",
    shape: "collapsed two-arm 258 set idiom",
  },
  {
    // `@ignore` (the open struct-map tolerate-and-drop rest-row flavor) is valid ONLY on a `* k => v`
    // rest ENTRY, read from that entry's trailing comment slot. This gate's rendering places the
    // toggled directive at RULE position (`foo = <body> ; @ignore`), where `@ignore` is a
    // misplacement — so this cell documents the LOUD rule-level rejection (honored-not-silenced),
    // exactly the legitimate cell shape for a directive whose only valid slot is not rule position.
    id: "open_struct_rule_position_ignore_rejected",
    ruleBody: "{ 1: uint, * uint => any }",
    base: [],
    toggled: "@ignore",
    shape: "open struct-map at rule-position @ignore (valid only on the rest entry)",
  },
  {
    // `@no_json_schema_export` suppresses a schema-registration row in the json-gen crate. This cell
    // generates RUST-ONLY, where no such row exists — so the directive is legitimately byte-identical
    // here, and the allowlist entry below is the honest record of that (the row-level effect is pinned
    // by `snapshot_tests::json_gen_extern_schema_rows` and `integration_tests::json_extern`, and swept
    // under the `json` profile by the product cells). The cell still earns its place: it proves the
    // directive is ACCEPTED (not rejected) on a struct-registering rule under a flag set that cannot
    // honor it, which is the "one spec, many flag sets" half a rejection test cannot assert.
    id: "record_no_json_schema_export_rust_only",
    ruleBody: "[x: uint]",
    base: [],
    toggled: "@no_json_schema_export",
    shape: "record rule, rust-only generation (the row it suppresses is json-gen-only)",
  },
  {
    // The PLAIN GROUP arm of `parse_rule` reaches neither `parse_type` nor `parse_type_choices`, so
    // it needs its own directive-marking site — and shipped without one, silently dropping
    // `@no_json_schema_export` on a spliced group (which does get a row). Note what this cell can and
    // cannot see: rust-only generation emits no schema-registration rows at all, so it is
    // byte-identical whether or not the arm marks, and it is ALLOWLISTED rather than a catch. It
    // earns its place as the inventory record for the arm, and it flips to a loud PASS-by-rejection
    // if this shape is ever wrongly moved onto the struct-less rejection path. The systematic catch
    // for the row-level drop is the `json`-profile product cell plus
    // `snapshot_tests::json_gen_extern_schema_rows`.
    id: "plain_group_no_json_schema_export_rust_only",
    ruleBody: "(a: uint, b: uint)",
    base: [],
    toggled: "@no_json_schema_export",
    shape: "plain group rule (parse_rule's Rule::Group arm), rust-only generation",
  },
  {
    // Bare `@newtype` on a nominalized 258 set is an ACCEPTED NO-OP: the set already nominalizes into a
    // wrapper, and a bare `@newtype` requests an inherent `get()` that is deliberately suppressed (it
    // would shadow `OrderedSet::get(index)` through `Deref` — E0061). So it is byte-identical with/without
    // the directive; allowlisted below as the documented no-op.
    id: "two_arm_258_idiom_bare_newtype_noop",
    ruleBody: "#6.258([* uint]) / [* uint]",
    base: [],
    toggled: "@newtype",
    shape: "collapsed two-arm 258 set idiom (bare @newtype, no getter)",
  },
  {
    // `@extern_companions` declares that a LOCAL extern's structural WASM companion classes already
    // exist in a sibling wasm crate. This cell generates RUST-ONLY, where no such class exists at
    // all (a `[* foo]` member is a plain `Vec<Foo>`), so the directive is legitimately byte-identical
    // here — the allowlist entry below is the honest record of that, and the wasm-side effect is
    // pinned by `dsl_position_tests`' `local-extern-rule` cell,
    // `integration_tests::extern_companions_defers_to_sibling_wasm_crate`, and the `wasm`-profile
    // product cells. The cell still earns its place: it proves the directive is ACCEPTED (not
    // rejected) on the one placement that honors it, under a flag set that cannot honor it.
    id: "local_extern_companions_rust_only",
    ruleBody: "_CDDL_CODEGEN_EXTERN_TYPE_",
    base: [],
    toggled: "@extern_companions dep_wasm=FooList",
    shape: "local _CDDL_CODEGEN_EXTERN_TYPE_ rule, rust-only generation (the classes it defers are wasm-only)",
  },
  {
    // The placement counterpart: on a rule this crate GENERATES, the directive is a LOUD rejection
    // (a generated rule owns its own companions), which is a PASS — honored-not-silenced. Isolates
    // the rule KIND as the variable against the accepted cell above, whose spec differs only in its
    // body being the extern marker.
    id: "generated_rule_extern_companions_rejected",
    ruleBody: "[x: uint]",
    base: [],
    toggled: "@extern_companions dep_wasm=FooList",
    shape: "record rule at rule-position @extern_companions (valid only on an extern marker)",
  },
  {
    // The rule slot of a multi-choice type rule is the LAST arm's trailing comment. A rule-level
    // directive on an earlier arm is parsed as that variant's own metadata, where only `@name` and
    // `@doc` mean anything, and is discarded — historically byte-identical to omitting it, which is
    // this gate's FAIL condition. It now exits nonzero (loudly rejected), which is a PASS.
    // `@used_as_key` because its effect is visible under this cell's rust-only generation.
    id: "type_choice_non_last_arm_used_as_key",
    ruleBody: "",
    armPlacement: { arms: ["uint", "tstr", "bytes"], toggledArm: 0 },
    base: [],
    toggled: "@used_as_key",
    shape: "multi-choice type rule, directive on a NON-LAST arm",
  },
  {
    // Placement control for the cell above: the SAME directive on the SAME rule shape, at the LAST
    // arm (the rule slot), where it takes effect and changes bytes. Isolates arm position as the
    // variable, so the cell above cannot pass for the wrong reason.
    id: "type_choice_last_arm_used_as_key",
    ruleBody: "",
    armPlacement: { arms: ["uint", "tstr", "bytes"], toggledArm: 2 },
    base: [],
    toggled: "@used_as_key",
    shape: "multi-choice type rule, directive on the LAST arm (the rule slot)",
  },
  {
    // The `T / null` Option collapse is its OWN branch of the multi-choice path — it registers a
    // transparent `Option<T>` alias instead of an enum — and it read the wrong comment slot (the
    // inner arm's `Type1` one, which the parser never populates), so EVERY rule-position directive
    // on such a rule was byte-identical to omitting it: this gate's FAIL condition, for the whole
    // directive vocabulary at once. `@no_alias` because it is the one whose effect is visible under
    // this cell's rust-only generation (it strips the emitted `pub type` line).
    id: "option_collapse_rule_slot_no_alias",
    ruleBody: "",
    armPlacement: { arms: ["uint", "null"], toggledArm: 1 },
    base: [],
    toggled: "@no_alias",
    shape: "T / null Option-collapse rule, directive at the rule slot (the LAST arm)",
  },
  {
    // Placement control for the cell above, and the collapse's own version of the non-last-arm
    // cell: the collapse has no VARIANTS, so an arm carries nothing of its own and the directive is
    // loudly rejected rather than honored. Isolates arm position as the variable.
    id: "option_collapse_non_last_arm_no_alias",
    ruleBody: "",
    armPlacement: { arms: ["uint", "null"], toggledArm: 0 },
    base: [],
    toggled: "@no_alias",
    shape: "T / null Option-collapse rule, directive on a NON-LAST arm",
  },
];

// Legitimate byte-identical accepted no-ops: `<cellId>` => one-line justification. A cell on this list
// is EXPECTED to be byte-identical with/without its directive and to print no acknowledging notice; it
// is exempted from the FAIL condition (and doubles as the accepted-no-op inventory).
const ALLOWLIST: Record<string, string> = {
  // `@duplicates preserve` is already the default for a plain non-258 array (`Vec`), so writing it is a
  // byte-identical self-documenting no-op — the documented opt-out spelling, not a dropped directive.
  plain_array_preserve:
    "explicit @duplicates preserve on a non-258 array = today's default (Vec); byte-identical no-op",
  // `@duplicates reject` is already the registry default for a 258 set idiom (`OrderedSet`), so writing
  // it is a byte-identical self-documenting no-op; the explicit directive also suppresses the defaulting
  // clause of the collapse notice, so the with-run prints no @duplicates mention.
  two_arm_258_idiom_reject:
    "explicit @duplicates reject on a 258 set idiom = registry default (OrderedSet); byte-identical no-op",
  // A named non-generic 258 set NOMINALIZES with or without `@newtype`; a BARE `@newtype` requests an
  // inherent `get()` that is suppressed on set nominals (it would shadow `OrderedSet::get(index)` through
  // `Deref`), so it adds nothing. A custom `@newtype <name>` getter IS honored (see the positive cell).
  two_arm_258_idiom_bare_newtype_noop:
    "bare @newtype on a nominalized 258 set = no getter (suppressed to avoid the Deref shadow); byte-identical no-op",
  // `@no_json_schema_export` only removes a schema-registration row from the json-gen crate, which
  // rust-only generation never emits — so it is byte-identical HERE by construction (and inert under
  // any flag set without `--json-schema-export`, by design: one spec, many flag sets).
  record_no_json_schema_export_rust_only:
    "@no_json_schema_export suppresses a json-gen row only; rust-only generation emits no rows, so byte-identical no-op",
  // Same reason as the record cell — the shape differs (parse_rule's Rule::Group arm), not the
  // rust-only invisibility.
  plain_group_no_json_schema_export_rust_only:
    "@no_json_schema_export on a plain group suppresses a json-gen row only; rust-only generation emits no rows, so byte-identical no-op",
  // `@extern_companions` only changes WHICH crate a wasm companion class comes from; rust-only
  // generation mints no such class at all, so it is byte-identical HERE by construction (and inert
  // under any flag set without `--wasm`, by design: one spec, many flag sets).
  local_extern_companions_rust_only:
    "@extern_companions defers wasm companion classes only; rust-only generation mints none, so byte-identical no-op",

  // ---- @no_alias: the rule mints a TYPE, so there is no `pub type` of its own to suppress -------
  // The directive strips a rule's transparent alias line. Every shape below registers a struct or an
  // enum instead — `register_type_alias` is never reached with the rule's own ident — so the flag has
  // nothing to act on. Inert rather than wrong: the kinds that DO emit an alias honor it (pinned by
  // `no_alias_suppresses_the_pub_type_on_every_alias_registering_kind`), and a struct-registering
  // rule keeping the directive costs nothing. The plain group NOTHING splices is the exception, and
  // is refused (it emits neither) rather than listed here.
  record__no_alias: "a record rule mints `pub struct Foo`, not a `pub type` — nothing to suppress",
  tagged__no_alias: "a tag-head rule mints a wrapper struct, not a `pub type` — nothing to suppress",
  multi_choice__no_alias: "a multi-arm type choice mints an enum, not a `pub type` — nothing to suppress",
  c_style_enum__no_alias: "a fixed-value choice mints a c-style enum, not a `pub type` — nothing to suppress",
  open_struct_map__no_alias: "an open struct-map mints a record with a rest row, not a `pub type` — nothing to suppress",
  two_arm_258_set__no_alias: "the 258 set idiom nominalizes into a wrapper struct, not a `pub type` — nothing to suppress",
  plain_group_spliced__no_alias: "a spliced plain group mints a record struct, not a `pub type` — nothing to suppress",
  generic_instance__no_alias: "a generic instance mints its struct during generic resolution, not a `pub type` — nothing to suppress",
  extern__no_alias: "an extern marker names a type this crate does not define; it registers no alias of its own",
  raw_bytes__no_alias: "a raw-bytes marker names a type this crate does not define; it registers no alias of its own",
  generic_definition__no_alias: "a generic definition names no concrete type — only its instantiations do — so it emits no `pub type`",
  generic_extern_base__no_alias:
    "the `pub type` emitted here belongs to the INSTANCE rule (`inst = foo<uint>`), not to the base — annotate the instance to suppress it",

  // ---- @newtype: the rule is already nominal, or mints no type of its own to wrap ---------------
  // `@newtype` asks a rule that would lower to a transparent alias to mint a wrapper struct instead.
  // Every shape below either already mints a nominal type (so the request is already granted) or
  // mints no type of its own at all (so there is nothing to wrap).
  tagged__newtype: "a tag head already mints a wrapper struct (`pub struct Foo(pub(crate) u64)`) — probed; the request is already granted",
  multi_choice__newtype: "a multi-arm type choice already mints a nominal enum",
  c_style_enum__newtype: "a fixed-value choice already mints a nominal c-style enum",
  two_arm_258_set__newtype:
    "the 258 set idiom already nominalizes; a BARE @newtype's inherent `get()` is suppressed to avoid the `OrderedSet::get(index)` Deref shadow (`@newtype <name>` IS honored — see the positive cell)",
  generic_instance__newtype: "a generic instance already mints a nominal struct during generic resolution",
  plain_group_spliced__newtype: "a spliced plain group already mints a nominal record struct",
  set_nominal_binding__newtype: "the binding aliases a set nominal that already IS a wrapper struct; the binding itself mints no type to wrap",
  extern__newtype: "an extern marker names a type this crate does not define — there is no body here to wrap",
  raw_bytes__newtype: "a raw-bytes marker names a type this crate does not define — there is no body here to wrap",
  generic_extern_base__newtype: "a generic extern base names no concrete type — there is no body here to wrap",

  // ---- @used_as_key: the demand is already satisfied, or belongs to a type this crate cannot -----
  // ---- add derives to -------------------------------------------------------------------------
  // The directive demands the comparison derives a map-key position needs. On a transparent alias it
  // has no struct of its own to derive on, and each swept alias target already satisfies every key
  // bound; on the extern family the type is hand-written elsewhere, so the capability is that type's
  // own and rustc checks it at the use site.
  alias__used_as_key: "`foo = uint` aliases a rust primitive that already satisfies every key bound — the demand adds nothing",
  parenthesized__used_as_key: "`foo = (uint)` aliases the same rust primitive — the demand adds nothing",
  set_nominal_binding__used_as_key:
    "the binding aliases a set nominal, whose comparison derives are always-on (parity with `OrderedSet`) — the demand adds nothing",
  extern__used_as_key: "an extern names a hand-written type; the generator cannot add derives to it, and rustc checks the key bound at the use site",
  raw_bytes__used_as_key: "a raw-bytes type is hand-written the same way; the key bound is checked at the use site",
  generic_extern_base__used_as_key: "a generic extern base names no concrete type; its instances' key bounds are checked at their use sites",

  // ---- @duplicates reject: the explicit spelling of today's default -----------------------------
  // Self-documenting no-ops, the same class as the `plain_array_preserve` hand cell above. Both
  // shapes default to `reject` in the registry, so writing it changes nothing.
  table__duplicates: "explicit @duplicates reject on a table = today's default; byte-identical no-op",
  two_arm_258_set__duplicates:
    "explicit @duplicates reject on a 258 set idiom = registry default (OrderedSet); byte-identical no-op (the explicit directive also suppresses the collapse notice's defaulting clause, so the with-run names no directive)",

  // ---- @no_json_schema_export: registers a struct the row loop skips for other reasons ----------
  // On `robustness_tests::no_json_schema_export_misuse_rejects_gracefully`'s own criterion, a rule
  // that registers NO rust struct is rejected, and one that registers a struct the schema-row loop
  // skips for its own reasons is ACCEPTED — redundant but honest, and flag-independent. All three
  // shapes below register a struct (a table and an array typedef from the finalize kind-walk, a
  // generic extern base as a plain `Extern`), so they sit on the accepted side beside that test's
  // existing `array typedef` entry.
  table__no_json_schema_export: "a table rule registers a `Table` struct the schema-row loop skips; accepted-redundant, not a drop (see no_json_schema_export_misuse_rejects_gracefully)",
  array_typedef__no_json_schema_export: "an array typedef registers an `Array` struct the schema-row loop skips; accepted-redundant, not a drop (same test)",
  generic_extern_base__no_json_schema_export: "a generic extern base registers an `Extern` struct the schema-row loop skips (it names no concrete type); accepted-redundant, not a drop (same test)",

  // ---- @doc on the extern family: the emission site is a deliberate plain-`//` line group -------
  // An extern / raw-bytes rule's only emitted artifact is the extern re-export glue, whose contract
  // comment (`EXTERN_REEXPORT_CONTRACT_COMMENT`) is read as LINES by three mechanisms: the import
  // prune's file-unchanged classification (it keys on `syn::Item`s, and comments are not items),
  // `export.rs`'s whole-line live-glue scan, and the comment-preservation overlay, whose
  // second-regen behaviour depends on rustfmt-stable output. Emitting a `///` doc there would change
  // what all three read. The inertness is a recorded design constraint of that site, not an
  // oversight — so the directive is neither honored nor refused.
  extern__doc: "the extern re-export glue is a deliberate plain-`//` line group three mechanisms read as lines; a doc line there is structurally excluded",
  raw_bytes__doc: "same emission site as the extern rule — the plain-`//` re-export contract group; a doc line there is structurally excluded",
  generic_extern_base__doc: "same emission site again; the base itself emits nothing else (its instances carry the concrete names)",
};

// ---- cell construction -------------------------------------------------------------------------

/** One executable comparison: the same spec without and with the toggled directive, under one
 *  profile. A cell has one variant per holder embedding. */
interface Variant {
  holder: string;
  withoutSpec: string;
  withSpec: string;
}

/** A (shape, directive) cell. `shape` is the human description used in the FAIL message. */
interface Cell {
  id: string;
  shape: string;
  toggled: string;
  profile: ProfileId;
  variants: Variant[];
}

function renderShape(shape: Shape, holder: string[], directives: string[]): string {
  const lines = [...(shape.prelude ?? [])];
  if (shape.arms) {
    const { list, toggledArm } = shape.arms;
    list.forEach((arm, i) => {
      const own = i === toggledArm ? directives : [];
      const c = own.length ? ` ; ${own.join(" ")}` : "";
      lines.push(`${i === 0 ? "foo = " : "  / "}${arm}${c}`);
    });
  } else {
    const c = directives.length ? ` ; ${directives.join(" ")}` : "";
    lines.push(`${shape.head ?? "foo"} = ${shape.body}${c}`);
  }
  lines.push(...holder);
  return lines.join("\n") + "\n";
}

function buildHandRule(cell: HandCell, extra: string[]): string {
  const directives = [...cell.base, ...extra];
  if (cell.armPlacement) {
    // Multi-line type choice. Base directives stay at the rule slot (the LAST arm), so only the
    // toggled directive's POSITION varies between a cell's two runs.
    const { arms, toggledArm } = cell.armPlacement;
    const lines = arms.map((arm, i) => {
      const own = [...(i === arms.length - 1 ? cell.base : []), ...(i === toggledArm ? extra : [])];
      const c = own.length ? ` ; ${own.join(" ")}` : "";
      return `${i === 0 ? "foo = " : "  / "}${arm}${c}`;
    });
    return `${lines.join("\n")}\nholder = [f: foo]\n`;
  }
  const comment = directives.length ? ` ; ${directives.join(" ")}` : "";
  // A holder embedding the rule exercises member position too (the transparent-alias flatten seam).
  return `foo = ${cell.ruleBody}${comment}\nholder = [f: foo]\n`;
}

/** Assemble the full cell list: the hand corpus first (its ids own the ALLOWLIST entries and its
 *  comments pin specific shipped regressions), then the shape×directive product. A cell whose
 *  variant set is LITERALLY identical to an earlier cell's under the same profile is dropped — the
 *  deduplication is by spec text, never by intent, so a hand cell can never silence a product cell
 *  that differs in any byte. */
function buildCells(directives: string[]): Cell[] {
  const cells: Cell[] = [];
  const seen = new Set<string>();
  const push = (c: Cell) => {
    const key = c.profile + " " + c.variants.map(v => v.withoutSpec + "\u0000" + v.withSpec).join("\u0001");
    if (seen.has(key)) return;
    seen.add(key);
    cells.push(c);
  };
  for (const hand of HAND_CORPUS) {
    push({
      id: hand.id,
      shape: hand.shape,
      toggled: hand.toggled,
      profile: "default",
      variants: [{
        holder: "holder = [f: foo]",
        withoutSpec: buildHandRule(hand, []),
        withSpec: buildHandRule(hand, [hand.toggled]),
      }],
    });
  }
  for (const shape of SHAPES) {
    for (const directive of directives) {
      const spelling = SPELLINGS[directive];
      const profile = WITNESS_PROFILE[directive];
      push({
        id: `${shape.id}__${directive.slice(1)}`,
        shape: `${shape.desc} [profile=${profile}]`,
        toggled: spelling,
        profile,
        variants: shape.holders.map(holder => ({
          holder: holder.join(" "),
          withoutSpec: renderShape(shape, holder, []),
          withSpec: renderShape(shape, holder, [spelling]),
        })),
      });
    }
  }
  return cells;
}

// ---- the runner ----------------------------------------------------------------------------------

/** The `@`-prefixed directive token used to detect an acknowledging notice/rejection
 *  (`@duplicates preserve` → `@duplicates`). MUST keep the leading `@`, on precision grounds: a genuine
 *  notice or rejection always spells the directive WITH its `@` (`defaulting to @duplicates reject`,
 *  `@newtype on rule …`), so the `@` is exactly what distinguishes an acknowledgement from any other
 *  appearance of the word. The generator's IR dump is the concrete counter-example the requirement is
 *  measured against — it names struct fields `duplicates:` / `newtype:` (no `@`), so a bare-keyword
 *  match would read that as an acknowledgement. The dump is `--verbosity trace`-only and this gate runs
 *  at the default level, so it does not reach these runs today; the requirement stands regardless,
 *  because "the word appears somewhere in the output" was never evidence that the directive was
 *  honored, and one `--verbosity` change away the dump is back.
 *
 *  What the gate DOES depend on is that the acknowledging notices themselves are default-visible: the
 *  `@duplicates` notices are `warn!`/`note!`, and `output` below reads stdout AND stderr combined, so
 *  neither the level gate nor a stream move can hide one from this match. */
function directiveKeyword(directive: string): string {
  return "@" + directive.replace(/^@/, "").split(/\s+/)[0];
}

interface RunResult { exit: number; output: string; bytes: string | null }

/** Every generated file under `dir`, concatenated in sorted relative-path order — the byte-identity
 *  comparison surface. Deliberately the WHOLE output tree rather than `rust/src/generated`: the wasm
 *  and json-gen crates are where several directives' only surface lives, and a whole-tree read
 *  removes the per-directive surface selection a narrower read would need. The static runtime files
 *  are identical between a cell's two runs, so they never mask a real per-type difference. */
function readTree(dir: string): string {
  const parts: string[] = [];
  const walk = (d: string) => {
    for (const entry of readdirSync(d).sort()) {
      const p = join(d, entry);
      const st = statSync(p);
      if (st.isDirectory()) walk(p);
      else if (st.isFile()) parts.push(`// FILE ${relative(dir, p)}\n` + readFileSync(p, "utf8"));
    }
  };
  walk(dir);
  return parts.join("\n");
}

/** Generate `spec` under `profile` into a throwaway dir; return exit code, combined stdout+stderr,
 *  and the concatenated generated tree (null when the run failed / produced no tree). */
async function generate(spec: string, profile: ProfileId): Promise<RunResult> {
  const dir = mkdtempSync(join(tmpdir(), "no-silent-dir-"));
  try {
    const specPath = join(dir, "in.cddl");
    writeFileSync(specPath, spec);
    const outDir = join(dir, "out");
    const proc = Bun.spawn(
      [BIN, `--input=${specPath}`, `--output=${outDir}`, `--static-dir=${STATIC_DIR}`, ...PROFILES[profile]],
      { cwd: CODEGEN_DIR, stdout: "pipe", stderr: "pipe" },
    );
    const [out, err, exit] = await Promise.all([
      new Response(proc.stdout).text(),
      new Response(proc.stderr).text(),
      proc.exited,
    ]);
    const bytes = exit === 0 && existsSync(outDir) ? readTree(outDir) : null;
    return { exit, output: out + err, bytes };
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

/** Run `tasks` with at most `CONCURRENCY` in flight, preserving result order. */
async function pool<T>(tasks: (() => Promise<T>)[]): Promise<T[]> {
  const results = new Array<T>(tasks.length);
  let next = 0;
  const worker = async () => {
    while (true) {
      const i = next++;
      if (i >= tasks.length) return;
      results[i] = await tasks[i]();
    }
  };
  await Promise.all(Array.from({ length: Math.max(1, Math.min(CONCURRENCY, tasks.length)) }, worker));
  return results;
}

/** The classification forcing function: every directive the authority knows must carry a canonical
 *  spelling AND a witness profile, and neither table may name a directive the authority does not. A
 *  new directive therefore DEMANDS classification instead of silently skipping the product. */
function classifyDirectives(): string[] {
  const tags = knownRuleMetadataTags();
  const problems: string[] = [];
  for (const tag of tags) {
    if (!(tag in SPELLINGS)) problems.push(`  ${tag}: no canonical-spelling row in SPELLINGS`);
    if (!(tag in WITNESS_PROFILE)) problems.push(`  ${tag}: no witness-profile row in WITNESS_PROFILE`);
  }
  for (const spelled of Object.keys(SPELLINGS))
    if (!tags.includes(spelled)) problems.push(`  ${spelled}: SPELLINGS row for a directive comment_ast.rs does not know`);
  for (const profiled of Object.keys(WITNESS_PROFILE))
    if (!tags.includes(profiled)) problems.push(`  ${profiled}: WITNESS_PROFILE row for a directive comment_ast.rs does not know`);
  if (problems.length)
    throw new Error(
      `no_silent_directive: the directive axis is out of lockstep with comment_ast.rs's ` +
      `KNOWN_RULE_METADATA_TAGS:\n${problems.join("\n")}\n\nClassify each one: give it the spelling a ` +
      `spec author writes (with a VALID argument where the argument is required — comment_ast panics ` +
      `otherwise) and the cheapest flag profile under which its surface exists. Skipping the ` +
      `classification is what this check exists to prevent.`,
    );
  return tags;
}

/** What one variant's two runs showed. Ordered best-to-worst: the cell's verdict is its BEST
 *  variant, because "honored under at least one embedding" is enough to say the shape's parse path
 *  carries the directive to a marking site — which is what this gate measures. */
type VariantVerdict =
  | { kind: "effect" }
  | { kind: "rejected" }
  | { kind: "notice" }
  | { kind: "silent" }
  | { kind: "base_failed"; detail: string }
  | { kind: "unexpected_failure"; detail: string };

const VERDICT_RANK: Record<VariantVerdict["kind"], number> = {
  effect: 0, rejected: 1, notice: 2, silent: 3, base_failed: 4, unexpected_failure: 4,
};

async function main(): Promise<number> {
  if (!existsSync(BIN)) {
    // Build once (offline-safe under the runner's CARGO_NET_OFFLINE); the runner's `build` gate usually
    // has done this already, but the gate must be runnable standalone.
    const b = Bun.spawnSync(["cargo", "build", "-q", "--bin", "cddl-codegen"], { cwd: CODEGEN_DIR, stdout: "inherit", stderr: "inherit" });
    if ((b.exitCode ?? 1) !== 0 || !existsSync(BIN)) {
      console.error("no_silent_directive: could not build cddl-codegen");
      return 2;
    }
  }

  const directives = classifyDirectives();
  const cells = buildCells(directives);
  const t0 = Date.now();

  // Flatten to one job per (cell, variant). The WITHOUT-directive run is directive-independent, so
  // it is shared by every variant with the same (spec, profile) — that is what keeps the product's
  // cost at roughly one generator invocation per variant rather than two.
  const jobs: { cell: number; variant: number }[] = [];
  cells.forEach((c, ci) => c.variants.forEach((_, vi) => jobs.push({ cell: ci, variant: vi })));

  const baselineKeys = [...new Set(jobs.map(j => `${cells[j.cell].profile}\u0000${cells[j.cell].variants[j.variant].withoutSpec}`))];
  const baselineResults = await pool(baselineKeys.map(k => () => {
    const [profile, spec] = k.split("\u0000");
    return generate(spec, profile as ProfileId);
  }));
  const baselines = new Map(baselineKeys.map((k, i) => [k, baselineResults[i]]));

  const withResults = await pool(jobs.map(j => () =>
    generate(cells[j.cell].variants[j.variant].withSpec, cells[j.cell].profile)));

  const verdicts: VariantVerdict[][] = cells.map(c => new Array(c.variants.length));
  jobs.forEach((j, i) => {
    const cell = cells[j.cell];
    const variant = cell.variants[j.variant];
    const without = baselines.get(`${cell.profile}\u0000${variant.withoutSpec}`)!;
    const withD = withResults[i];
    const keyword = directiveKeyword(cell.toggled);
    const mentioned = withD.output.toLowerCase().includes(keyword.toLowerCase());

    if (without.exit !== 0) {
      verdicts[j.cell][j.variant] = { kind: "base_failed", detail: `exit ${without.exit}\n${variant.withoutSpec}${without.output}` };
    } else if (withD.exit !== 0) {
      // Loud rejection: the directive was honored (not silenced). Require it name the directive so an
      // UNRELATED failure can't masquerade as "handled".
      verdicts[j.cell][j.variant] = mentioned
        ? { kind: "rejected" }
        : { kind: "unexpected_failure", detail: `exit ${withD.exit}\n${variant.withSpec}${withD.output}` };
    } else if (without.bytes === null || withD.bytes === null || without.bytes !== withD.bytes) {
      verdicts[j.cell][j.variant] = { kind: "effect" };
    } else if (mentioned) {
      verdicts[j.cell][j.variant] = { kind: "notice" };
    } else {
      verdicts[j.cell][j.variant] = { kind: "silent" };
    }
  });

  const failures: string[] = [];
  let passes = 0;
  cells.forEach((cell, ci) => {
    // A fixture bug or an unexpected abort in ANY variant is a failure on its own terms — it is not
    // a measurement, so a sibling variant's PASS must not absorb it.
    const broken = verdicts[ci].find(v => v.kind === "base_failed" || v.kind === "unexpected_failure");
    if (broken && broken.kind === "base_failed") {
      failures.push(`${cell.id}: BASE spec failed to generate — fixture bug: ${broken.detail}`);
      return;
    }
    if (broken && broken.kind === "unexpected_failure") {
      failures.push(`${cell.id}: with-directive run failed but its output never names '${cell.toggled}' — unexpected failure, not a directive rejection: ${broken.detail}`);
      return;
    }
    const best = verdicts[ci].reduce((a, b) => (VERDICT_RANK[b.kind] < VERDICT_RANK[a.kind] ? b : a));
    const holders = cell.variants.map(v => v.holder).join(" | ");
    if (best.kind === "effect") {
      console.log(`  PASS ${cell.id}: '${cell.toggled}' changed generated output on ${cell.shape}`);
      passes++;
    } else if (best.kind === "rejected") {
      console.log(`  PASS ${cell.id}: '${cell.toggled}' loudly rejected on ${cell.shape}`);
      passes++;
    } else if (best.kind === "notice") {
      console.log(`  PASS ${cell.id}: '${cell.toggled}' acknowledged by a notice on ${cell.shape} (byte-identical accepted)`);
      passes++;
    } else if (cell.id in ALLOWLIST) {
      console.log(`  PASS ${cell.id}: allowlisted no-op — ${ALLOWLIST[cell.id]}`);
      passes++;
    } else {
      failures.push(`directive silently ignored: '${cell.toggled}' on ${cell.shape} (${cell.id}) — output byte-identical with/without it, under EVERY holder embedding swept (${holders}), and no notice/rejection names it. Either honor the directive, reject it loudly, or (if a genuine no-op) add it to the ALLOWLIST with a justification.`);
    }
  });

  // A stale ALLOWLIST entry is a claim nothing measures any more — prune it rather than carry it.
  const cellIds = new Set(cells.map(c => c.id));
  for (const id of Object.keys(ALLOWLIST))
    if (!cellIds.has(id)) failures.push(`ALLOWLIST entry '${id}' names no cell — stale, prune it`);

  console.log(
    `\nno_silent_directive: ${passes} passed, ${failures.length} failed ` +
    `(${cells.length} cells / ${jobs.length} variants, ${baselineKeys.length} baselines, ` +
    `${directives.length} directives × ${SHAPES.length} shapes) in ${((Date.now() - t0) / 1000).toFixed(1)}s`,
  );
  for (const f of failures) console.error(`  FAIL ${f}`);
  return failures.length === 0 ? 0 : 1;
}

process.exit(await main());
