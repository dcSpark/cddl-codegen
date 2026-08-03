//! Comment-DSL directive × REFERENCING-CONTEXT sweep — the directive is written on a BASE rule and
//! the assertion is made through a REFERENCE to that rule.
//!
//! The sibling `dsl_position_tests` sweep places the toggled directive ON the rule under test, so it
//! is structurally blind to a directive written on a rule the tested rule REFERENCES: no cell of it
//! annotates a referenced rule, and the whole class of "honored at the base, dropped through the
//! wrapping" is invisible to it by construction. Every instance found so far — the custom codec pair
//! reached through a tag head, through a `.cbor` payload, through a transparent re-alias and through
//! a rule-body `.cbor` alias — was found by a hand probe, one directive at a time. This module is the
//! systematic layer.
//!
//! A cell is `(directive, base shape it is HONORED on, wrapping context)`: [`BASES`] carries one row
//! per directive family (the base CDDL that defines rule `base`, plus whatever controls that base
//! needs), [`CONTEXTS`] carries one row per wrapping context (CDDL that REFERENCES `base` and is
//! itself reached from a `holder` record), and the sweep runs their product.
//!
//! **The verdict is PER-CONTEXT, never best-of-embeddings.** Context is the cell's variable, so an
//! effect observed in one context must never absorb a drop in another — one generation per cell, one
//! verdict per cell. What a cell asserts is that the directive's effect (or a LOUD refusal) is
//! observable through the reference; silence is the failure.
//!
//! **A drop found here is a FINDING, not a license to fix.** [`KNOWN_REFERENCE_DROP`] pins such a
//! cell — asserted to STILL be dropped, so the pin flips loudly the day a fix lands — with the same
//! authoring rules as the sibling sweep's `KNOWN_SILENT_DROP`, including its vacuity hazard: a pin
//! asserts "expectation NOT satisfied", which a mis-authored spec or a mis-typed anchor satisfies
//! vacuously. Two structural guards make that hazard cheap to keep out rather than a matter of care:
//! [`every_context_row_is_live`] requires each context's CDDL to carry at least one directive's
//! effect (so a context that stopped generating at all cannot hide behind a wall of pins), and
//! [`every_base_row_is_live`] requires each base shape to be HONORED through at least one reference
//! (which is the cell definition's "base shape it is honored on", made mechanical).
//!
//! The directive axis is kept in LOCKSTEP with `comment_ast::KNOWN_RULE_METADATA_TAGS` by
//! [`every_directive_is_swept_or_excluded`]: every tag has a cell in every context, or an entry in
//! the reasoned [`EXCLUSIONS`] registry. A NEW directive therefore fails this module until its author
//! classifies it — the forcing shape that constant's other consumers use. Exclusion reasons are
//! STRUCTURAL ("the context cannot syntactically wrap this base", "the context IS the directive's
//! effect, so no anchor can attribute it"), never "untested": the custom-pair history is the standing
//! evidence that "obviously reference-independent" is exactly the wrong intuition.
//!
//! Every cell runs generation under `catch_unwind` + `with_thread_silenced_panics` (like both sibling
//! sweeps) so a panic is reported as its own failure kind — never a test abort, and never mistaken
//! for a graceful rejection.

use crate::comment_ast::KNOWN_RULE_METADATA_TAGS;
use crate::tests::dsl_position_tests::generate;
use crate::tests::robustness_tests::with_thread_silenced_panics;

/// What a cell asserts about the concatenated generated source. Mirrors the sibling sweep's `Expect`
/// (same two shapes, same meaning) rather than sharing it: the two sweeps' cell types differ, and a
/// shared enum would couple their evolution without removing a line of either.
#[derive(Clone, Copy)]
enum Expect {
    /// Substrings that MUST / MUST-NOT appear in the concatenated generated source.
    Effect {
        must: &'static [&'static str],
        must_not: &'static [&'static str],
    },
    /// Generation returns a graceful `Err` whose message contains this substring (never a panic).
    /// A loud refusal through a reference is an ACCEPTED verdict — what this sweep hunts is silence.
    Reject(&'static str),
}

/// One wrapping context: CDDL that REFERENCES the rule named `base` and is itself reached from a
/// `holder` record, so the reference is never dead.
///
/// Auxiliary types inside a context are `bool` throughout, on purpose. A context that introduced a
/// `uint` or a `tstr` of its own would emit the very built-in codec (`write_unsigned_integer`) and
/// the very encoding type (`StringEncoding`) that several directive rows use as their must-not /
/// must anchors, and the cell would then hold — or fail — for the context's reasons instead of the
/// directive's. `bool` shares no anchor with any row.
struct Context {
    id: &'static str,
    cddl: &'static str,
}

/// The context axis. Every row is used by every non-excluded base row (the product IS the sweep), and
/// [`every_context_row_is_live`] proves each row's CDDL actually generates.
///
/// Rows 1, 2 and 4–9 are the member-position boundary list the `@name` door records plus the
/// contexts the custom pair's delivery probed. Rows 2b and 3 are the two the probe did not cover and
/// that later hand fixes had to reach SEPARATELY from each other — which is why they are two rows
/// and not one: a rule-body `.cbor` alias and a bare re-alias hit the same registration seam under
/// different framing, and the pair needed a fix at each.
const CONTEXTS: &[Context] = &[
    // 1. The payload of a tag-head rule. The wrapper owns the tag framing and nothing else, so
    //    anything the base declares about its own value has to survive the wrapping.
    Context {
        id: "tag-head-payload",
        cddl: "ctx = #6.9(base)\nholder = [f: ctx]\n",
    },
    // 2. A `.cbor` payload in a MEMBER's type expression. The member owns the byte-string framing;
    //    the base's value is what goes inside it. Historically the one context the custom pair was
    //    measured INERT in.
    Context {
        id: "cbor-payload",
        cddl: "holder = [f: bytes .cbor base]\n",
    },
    // 2b. A rule-body `.cbor` alias (`ctx = bytes .cbor base`) reached by a member. A DISTINCT seam
    //    from row 2, not a spelling of it: there the `.cbor` belongs to the member's type expression
    //    and the framing is the member's, here the RULE is the bytes-wrapped form and registers as a
    //    transparent alias, so the framing is the rule's and the base's metadata has to be inherited
    //    at registration. The custom pair needed two separate fixes for the two, which is the
    //    evidence that one row cannot stand in for the other. (Row 3 is the same registration seam
    //    without the framing; this row is the two stacked.)
    Context {
        id: "rule-body-cbor-alias",
        cddl: "ctx = bytes .cbor base\nholder = [f: ctx]\n",
    },
    // 3. A transparent re-alias (`ctx = base`) reached by a member. `register_type_alias` refuses an
    //    already-alias-wrapped base, so the re-aliasing rule stores the FLATTENED target and the
    //    emitter's ident lookup lands on `ctx`, not on `base` — the flattening seam, where whatever
    //    the base's own table entry carried has to be inherited at registration or lost.
    Context {
        id: "transparent-re-alias",
        cddl: "ctx = base\nholder = [f: ctx]\n",
    },
    // 4. A generic argument. The instance is monomorphized, so the base is reached through a
    //    substitution rather than through a direct member type.
    Context {
        id: "generic-argument",
        cddl: "gen<T> = [x: T]\nctx = gen<base>\nholder = [f: ctx]\n",
    },
    // 5. A table's VALUE domain (the arrow form the axis allows). Chosen over the fixed-label record
    //    form `{ 0: base }` for two reasons: the record form's member IS the plain member position
    //    that every other context is measured AGAINST (the sibling sweep's own cells cover it), and a
    //    fixed label introduces a key of its own whose codec/encoding anchors alias several rows'.
    Context {
        id: "map-value",
        cddl: "ctx = { * bool => base }\nholder = [f: ctx]\n",
    },
    // 6. A table's KEY domain — by-value argument mode, and the one position no record field's
    //    config reaches.
    Context {
        id: "map-key",
        cddl: "ctx = { * base => bool }\nholder = [f: ctx]\n",
    },
    // 7. A loose-list ELEMENT, written INLINE in a member (`holder = [f: [* base]]`). Two choices
    //    are recorded here. The axis allows `a = [* base]` or the keyed member `a = [x: base]`: the
    //    keyed member has a member-level comment slot of its own and IS the plain member position
    //    that this sweep measures every context AGAINST, so it is the control rather than a context,
    //    while a `*` element has no slot at all and is reached purely through the element type. And
    //    the INLINE spelling is chosen over the named list rule `ctx = [* base]` because the named
    //    rule registers a transparent alias (`pub type Ctx = Vec<Base>`) on top of the list, stacking
    //    the seam context 3 already isolates — and because the structural loose-list wrapper class
    //    (the wasm-side observable two rows depend on) is minted only for the inline usage.
    Context {
        id: "array-element",
        cddl: "holder = [f: [* base]]\n",
    },
    // 8. A type-choice ARM. The base becomes one variant of an enum, reached through the variant's
    //    payload rather than through a field.
    Context {
        id: "type-choice-arm",
        cddl: "ctx = base / bool\nholder = [f: ctx]\n",
    },
    // 9. An OPTIONAL record member. The base is reached through an `Option<..>`, whose serialize and
    //    deserialize are both a branch over the inner codec.
    Context {
        id: "optional-record-member",
        cddl: "ctx = [? x: base]\nholder = [f: ctx]\n",
    },
];

/// One directive family's base shape: CDDL defining a rule named `base` (plus any helper or control
/// rules the shape needs), the flags/wasm posture the directive's effect is observable under, and the
/// anchors that prove the effect crossed the reference.
///
/// `covers` names the `KNOWN_RULE_METADATA_TAGS` members the row accounts for — one row covers two
/// tags where the directive is only meaningful as a pair (`@custom_serialize`/`@custom_deserialize`).
struct BaseShape {
    directive: &'static str,
    covers: &'static [&'static str],
    /// CDDL defining rule `base`. Concatenated with a context's CDDL to form the cell's spec.
    base: &'static str,
    /// Extra CLI args beyond the `--wasm=<wasm>` baseline. A row carries only what its effect is
    /// observable under — the same posture the sibling sweep's cells take.
    flags: &'static [&'static str],
    wasm: bool,
    /// The expectation in every context that has no override.
    expect: Expect,
    /// Per-context replacements, keyed by [`Context::id`]. Used where the context legitimately
    /// changes the SPELLING of the observable (never where it changes the verdict).
    overrides: &'static [(&'static str, Expect)],
}

/// The base axis. One row per directive family; `(directive, context)` identifies a cell.
const BASES: &[BaseShape] = &[
    // ---- @name -------------------------------------------------------------------------------
    // The rule-position `@name` is a graceful rejection (it does not rename a top-level type), so
    // the honored base shape that a reference can observe is the VARIANT-naming one: a type-choice
    // rule whose arms carry `@name`. What must survive every wrapping is the base's own enum keeping
    // the authored variant names instead of the positional `I0`/`I1` fallbacks.
    BaseShape {
        directive: "@name",
        covers: &["@name"],
        base: "base = 0 ; @name mainnet\n     / 1 ; @name testnet\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["Mainnet", "Testnet"],
            must_not: &["I0"],
        },
        overrides: &[],
    },
    // ---- @doc --------------------------------------------------------------------------------
    // A rule-level `@doc` on a plain alias. The doc lands on the alias the rule emits, which is
    // exactly what a flattening context can erase.
    BaseShape {
        directive: "@doc",
        covers: &["@doc"],
        base: "base = uint ; @doc base level doc\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["/// base level doc"],
            must_not: &[],
        },
        overrides: &[],
    },
    // ---- @newtype ----------------------------------------------------------------------------
    // Two anchors, one per half of the claim. `pub struct Base(` / no `pub type Base` is the
    // wrapper surviving the wrapping at all; `Base::deserialize(` is the REFERENCE routing through
    // that wrapper rather than through the primitive it wraps — a context that resolved the base
    // transparently would read the `uint` inline and never name the wrapper, which is the drop
    // shape the first anchor alone cannot see (the wrapper's own impls would still be emitted).
    BaseShape {
        directive: "@newtype",
        covers: &["@newtype"],
        base: "base = uint ; @newtype\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub struct Base(", "Base::deserialize("],
            must_not: &["pub type Base"],
        },
        // The re-alias legitimately renames the observable: `pub type Ctx = Base;` resolves to the
        // WRAPPER (which is the reference-crossing claim — a flattened re-alias would resolve to
        // `u64`), and the member then reads through the alias spelling. A SPELLING change, not a
        // verdict change.
        overrides: &[(
            "transparent-re-alias",
            Expect::Effect {
                must: &[
                    "pub struct Base(",
                    "pub type Ctx = Base;",
                    "Ctx::deserialize(",
                ],
                must_not: &["pub type Base ="],
            },
        )],
    },
    // ---- @no_alias ---------------------------------------------------------------------------
    // The mirror image of `@newtype`: NO alias is emitted and every reference spells the underlying
    // primitive. This is the row whose effect is entirely at the REFERENCE, so its must-not is the
    // alias ident itself — if any context re-introduces `Base`, the emitted crate names a type
    // nothing declares.
    BaseShape {
        directive: "@no_alias",
        covers: &["@no_alias"],
        base: "base = uint ; @no_alias\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["pub struct Holder"],
            must_not: &["Base"],
        },
        overrides: &[],
    },
    // ---- @used_as_key ------------------------------------------------------------------------
    // The comparison traits the directive forces into the derive set.
    BaseShape {
        directive: "@used_as_key",
        covers: &["@used_as_key"],
        base: "base = [a: uint, b: text] ; @used_as_key\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["Ord, PartialOrd"],
            must_not: &[],
        },
        overrides: &[],
    },
    // ---- @used_as_elem -----------------------------------------------------------------------
    // The wasm-side loose-list wrapper class minted for the tagged type even though no rule contains
    // an inline `[* base]`. `sibling` is the untagged positive control in the same spec: it mints no
    // `SiblingList`, attributing the wrapper to the directive rather than to the wasm build.
    // `wasm: true` — the whole effect is wasm-side.
    BaseShape {
        directive: "@used_as_elem",
        covers: &["@used_as_elem"],
        base: "base = [a: uint, b: text] ; @used_as_elem\nsibling = [c: uint, d: text]\n",
        flags: &[],
        wasm: true,
        expect: Expect::Effect {
            must: &["pub struct BaseList("],
            must_not: &["SiblingList"],
        },
        overrides: &[],
    },
    // ---- @copy -------------------------------------------------------------------------------
    // The declaring crate's own honesty assertion is the directive's crate-wide observable, and it
    // is what every context must keep. Its BEHAVIOURAL half — the defensive `.clone()` the directive
    // drops — exists only where a context has a clone site, and on the rust-only baseline that is
    // the map-key deserialize loop, so the reference-crossing anchor lives in that context's
    // override rather than being asserted where there is nothing to assert.
    BaseShape {
        directive: "@copy",
        covers: &["@copy"],
        base: "base = _CDDL_CODEGEN_RAW_BYTES_TYPE_ ; @copy\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["_assert_copy::<crate::generated::Base>()"],
            must_not: &[],
        },
        overrides: &[(
            "map-key",
            Expect::Effect {
                must: &[
                    "_assert_copy::<crate::generated::Base>()",
                    "f_table.insert(f_key, f_value)",
                ],
                must_not: &["f_key.clone()"],
            },
        )],
    },
    // ---- @raw_bytes_flavor -------------------------------------------------------------------
    // The tag flavors generic INSTANCES, so the base shape is the instance alias `base` (an
    // instantiation of the flavored extern generic at a raw-bytes argument) — the thing a reference
    // can reach. `plain`/`unflavored` are the same-spec control: a non-raw-bytes instance keeps the
    // plain name, so the flavor anchor cannot hold for an unrelated reason.
    BaseShape {
        directive: "@raw_bytes_flavor",
        covers: &["@raw_bytes_flavor"],
        base: "pub_key = _CDDL_CODEGEN_RAW_BYTES_TYPE_\next_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_ ; @raw_bytes_flavor\nplain = [a: uint, b: text]\nunflavored = ext_set<plain>\nbase = ext_set<pub_key>\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["= ExtSetRawBytes<PubKey>", "= ExtSet<Plain>"],
            must_not: &[],
        },
        overrides: &[],
    },
    // ---- @ignore -----------------------------------------------------------------------------
    // The tolerate-and-drop flavor of an open struct-map's rest row. Its observable is the lossiness
    // breadcrumb the generated type and its serialize fn carry — the only signal a consumer has that
    // the type deliberately drops data, and therefore the thing whose loss is a wire-visible
    // documentation defect. The closer goes on its own line: a directive on the closing line would
    // be swallowed by the end-of-line comment trap.
    BaseShape {
        directive: "@ignore",
        covers: &["@ignore"],
        base: "base = {\n  1: uint,\n  * uint => any ; @ignore\n}\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["Open struct-map with an ignored rest row"],
            must_not: &[],
        },
        overrides: &[],
    },
    // ---- @duplicates -------------------------------------------------------------------------
    // The pair-map twin a named table's rule slot selects. The base is a TRANSPARENT ALIAS rule
    // (`pub type Base = PairMap<..>`), which is precisely the registration a flattening context can
    // rewrite, so the must-not is the default container it would fall back to.
    BaseShape {
        directive: "@duplicates",
        covers: &["@duplicates"],
        base: "base = { * uint => tstr } ; @duplicates preserve\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["PairMap<u64, String>"],
            must_not: &["BTreeMap<u64, String>"],
        },
        overrides: &[],
    },
    // ---- @custom_json ------------------------------------------------------------------------
    // The suppressed serde impl, with `ctrl` (same shape, no directive) as the same-spec positive
    // control so the absence is attributable to the directive rather than to a missing flag.
    // `@newtype` on both: the directive is rejected on a transparent alias, so the honored base shape
    // is the wrapper struct.
    BaseShape {
        directive: "@custom_json",
        covers: &["@custom_json"],
        base: "base = uint ; @newtype @custom_json\nctrl = uint ; @newtype\n",
        flags: &["--json-serde-derives=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &["impl serde::Serialize for Ctrl"],
            must_not: &["impl serde::Serialize for Base"],
        },
        overrides: &[],
    },
    // ---- @no_json_schema_export --------------------------------------------------------------
    // The registration row the json-gen crate does NOT emit, with `ctrl` as the same-spec control
    // whose row IS emitted.
    BaseShape {
        directive: "@no_json_schema_export",
        covers: &["@no_json_schema_export"],
        base: "base = [a: uint] ; @no_json_schema_export\nctrl = [b: uint]\n",
        flags: &["--json-serde-derives=true", "--json-schema-export=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &["reg.add::<cddl_lib::Ctrl>()"],
            must_not: &["reg.add::<cddl_lib::Base>()"],
        },
        overrides: &[],
    },
    // ---- @custom_serialize / @custom_deserialize ---------------------------------------------
    // The flagship pair on a scalar alias — the one directive family every fixed instance of this
    // class has belonged to, and the reason the sweep exists. Both halves are anchored: honoring only
    // the write half leaves the type writing one form and reading another, which is the divergence
    // the pair's placement rejections exist to prevent. The must-nots are the built-in uint codec the
    // pair replaces, so "called the pair AND kept the built-in" cannot pass either.
    BaseShape {
        directive: "@custom_serialize+deserialize",
        covers: &["@custom_serialize", "@custom_deserialize"],
        base: "base = uint ; @custom_serialize ser_base @custom_deserialize deser_base\n",
        flags: &[],
        wasm: false,
        expect: Expect::Effect {
            must: &["ser_base(", "deser_base("],
            must_not: &["write_unsigned_integer", "unsigned_integer()"],
        },
        overrides: &[],
    },
    // ---- @custom_encodings -------------------------------------------------------------------
    // The wire-facts declaration that gives a self-carrying extern's replaced codec its encoding
    // slots. Only observable under `--preserve-encodings` (encoding VARIABLES are what it declares,
    // and none exist without that flag), so the row carries it — and no context introduces a
    // `StringEncoding` of its own, which is what keeps the anchor the declaration's.
    BaseShape {
        directive: "@custom_encodings",
        covers: &["@custom_encodings"],
        base: "an = _CDDL_CODEGEN_EXTERN_TYPE_\nbase = an ; @custom_serialize ser_base @custom_deserialize deser_base @custom_encodings str\n",
        flags: &["--preserve-encodings=true"],
        wasm: false,
        expect: Expect::Effect {
            must: &["ser_base(", "deser_base(", "StringEncoding"],
            must_not: &[],
        },
        // The `.cbor` payload's own byte-string framing owns a `StringEncoding` slot
        // (`f_bytes_encoding`), so the bare type name cannot attribute the anchor to the
        // declaration there. The PAYLOAD's declared slot is `f_encoding`, a different name.
        overrides: &[(
            "cbor-payload",
            Expect::Effect {
                must: &["ser_base(", "deser_base(", "pub f_encoding: StringEncoding"],
                must_not: &[],
            },
        )],
    },
    // ---- @custom_wire_major ------------------------------------------------------------------
    // The declared CBOR major of a custom codec's wire. Its ONE consumer is an open table's typed-row
    // DISPATCH, which needs the major before any deserializer runs — and no context in this axis is
    // such a dispatch (the `map-key` row is a single-row table, which needs no dispatch at all). So
    // in every context the declaration is unconsumed, and the tool refuses LOUDLY rather than
    // dropping it — which is this sweep's accepted verdict, not a finding.
    //
    // The honored counterpart is not left to the reader: `custom_wire_major_is_honored_through_the
    // _typed_row_reference` below reaches the same base through a two-row open table and asserts the
    // declared major drives the dispatch arm, so these ten refusals are attributable to the contexts
    // having no consumer rather than to the declaration being unreachable through a reference.
    BaseShape {
        directive: "@custom_wire_major",
        covers: &["@custom_wire_major"],
        base: "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_\nbase = rb ; @custom_serialize ser_base @custom_deserialize deser_base @custom_wire_major text\n",
        flags: &[],
        wasm: false,
        expect: Expect::Reject("nothing consumes the declared major"),
        overrides: &[],
    },
    // ---- @extern_companions ------------------------------------------------------------------
    // The directive's promise is negative and crate-wide: the LISTED structural companion class is
    // never minted locally (two such `#[wasm_bindgen]` classes in one cdylib duplicate-symbol at
    // link). That promise is assertable in every context — a context that minted `BaseList` despite
    // the declaration is exactly the defect — while the positive half (the `use` of the sibling
    // crate's class) is only reachable where the context DEMANDS the listed class, which is the
    // `[* base]` one. `wasm: true`: the classes it governs do not exist rust-side.
    BaseShape {
        directive: "@extern_companions",
        covers: &["@extern_companions"],
        base: "base = _CDDL_CODEGEN_EXTERN_TYPE_ ; @extern_companions dep_wasm=BaseList\n",
        flags: &[],
        wasm: true,
        // `pub struct Holder` is the positive control every absence-based assertion in this module
        // carries: it proves the cell generated the spec it was given, so the absence of the class
        // is the directive's doing rather than an empty or truncated generation.
        expect: Expect::Effect {
            must: &["pub struct Holder"],
            must_not: &["pub struct BaseList"],
        },
        overrides: &[(
            "array-element",
            Expect::Effect {
                must: &["pub struct Holder", "use dep_wasm::BaseList;"],
                must_not: &["pub struct BaseList"],
            },
        )],
    },
];

/// `(directive, context-or-`ALL`, reason)` — the directive-axis coverage exemptions.
///
/// A reason must be STRUCTURAL: the directive cannot be written on a base shape any reference
/// observes, or the context cannot syntactically wrap this base, or the context IS the directive's
/// effect so no anchor can attribute it. "Untested" is never a reason — the custom-pair history is
/// the standing evidence that "obviously reference-independent" is the wrong intuition, so when in
/// doubt the cell gets written.
const EXCLUSIONS: &[(&str, &str, &str)] = &[
    (
        "@rust_name",
        ALL_CONTEXTS,
        "no base shape any reference in this crate observes: the directive is REJECTED on a \
         normally-generated rule (there the identifier already is the emitted type name), so it can \
         only be written on a rule in an extern-dependency scope — and its honored effect there is a \
         single crate-boundary import alias (`use dep::PlutusData as PlutusDatum;`). The docs make \
         the reference-independence a CONTRACT rather than an accident: `every internal spelling \
         keeps the consumer-derived name, and only the import seam differs`, so there is no \
         referencing-context-observable effect for a context to drop.",
    ),
    (
        "@used_as_elem",
        "array-element",
        "the context IS the directive's effect: an inline `[* base]` mints the loose-list wrapper on \
         its own (that is the usage the directive exists to simulate when no rule contains it), so \
         `BaseList` is present in this cell whether or not the directive is honored and no anchor \
         can attribute it to the directive. The other eight contexts contain no `[* base]`, so each \
         of them isolates it.",
    ),
];

/// The sentinel [`EXCLUSIONS`] context field uses to exempt a directive from EVERY context.
const ALL_CONTEXTS: &str = "ALL";

/// Cells whose directive's effect is DROPPED through that reference — a FINDING, not a fixture. Each
/// is asserted to STILL fail its expectation, so the pin flips loudly when a fix lands.
/// `(directive, context, reason)`.
///
/// AUTHORING RULE (inherited from the sibling sweep's `KNOWN_SILENT_DROP`, plus this sweep's own
/// guards): a pin asserts "expectation NOT satisfied", which a mis-authored base/context spec or a
/// mis-typed anchor satisfies VACUOUSLY. Before pinning, hand-verify against the generated source
/// that the directive's effect is genuinely absent — and note that the liveness assertions
/// ([`every_context_row_is_live`], [`every_base_row_is_live`]) already prove the pinned cell's base
/// shape is honored SOMEWHERE and its context generates SOMETHING, so a pin can only be vacuous
/// through its own anchor, never through a dead row.
///
/// A new entry is a NEW finding — pin it with its reason and REPORT it; do not re-author the cell's
/// expectation to match the drop, and do not fix the drop opportunistically.
const KNOWN_REFERENCE_DROP: &[(&str, &str, &str)] = &[];

/// The concatenated generated source, or a distinct marker for a graceful `Err` / a `panic!`.
enum Outcome {
    Source(String),
    Error(String),
    Panic,
}

/// A cell's spec: the base shape's CDDL followed by the context's.
fn cell_spec(base: &BaseShape, ctx: &Context) -> String {
    format!("{}{}", base.base, ctx.cddl)
}

/// The expectation for `(base, ctx)` — the row's default unless the row overrides that context.
fn cell_expect(base: &BaseShape, ctx: &Context) -> Expect {
    base.overrides
        .iter()
        .find(|(id, _)| *id == ctx.id)
        .map(|(_, e)| *e)
        .unwrap_or(base.expect)
}

/// Whether `(base, ctx)` is exempted by [`EXCLUSIONS`] — keyed on the TAGS the row covers, so an
/// exclusion is written against the directive vocabulary the lockstep assertion reads.
fn is_excluded(base: &BaseShape, ctx: &Context) -> bool {
    EXCLUSIONS.iter().any(|(directive, context, _)| {
        base.covers.contains(directive) && (*context == ALL_CONTEXTS || *context == ctx.id)
    })
}

fn is_pinned(base: &BaseShape, ctx: &Context) -> Option<&'static str> {
    KNOWN_REFERENCE_DROP
        .iter()
        .find(|(d, c, _)| *d == base.directive && *c == ctx.id)
        .map(|(_, _, reason)| *reason)
}

fn run(base: &BaseShape, ctx: &Context) -> Outcome {
    let tag: String = format!("refctx_{}_{}", base.directive, ctx.id)
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();
    let spec = cell_spec(base, ctx);
    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        generate(&spec, base.flags, base.wasm, &tag)
    }));
    match out {
        Ok(Ok(map)) => Outcome::Source(map.into_values().collect::<Vec<_>>().join("\n")),
        Ok(Err(e)) => Outcome::Error(e),
        Err(_) => Outcome::Panic,
    }
}

/// Whether the cell's expectation is SATISFIED by `outcome` (a panic never satisfies).
fn satisfied(expect: &Expect, outcome: &Outcome) -> bool {
    match (expect, outcome) {
        (Expect::Effect { must, must_not }, Outcome::Source(src)) => {
            must.iter().all(|m| src.contains(m)) && must_not.iter().all(|m| !src.contains(m))
        }
        (Expect::Reject(sub), Outcome::Error(e)) => e.contains(sub),
        _ => false,
    }
}

fn describe(outcome: &Outcome) -> String {
    match outcome {
        Outcome::Source(src) => format!("generated OK:\n{src}"),
        Outcome::Error(e) => format!("graceful Err: {e}"),
        Outcome::Panic => "PANIC during generation".to_string(),
    }
}

/// Every cell's verdict, computed once. `(directive, context, satisfied)`.
fn sweep_verdicts() -> Vec<(&'static str, &'static str, bool, Outcome)> {
    let mut out = Vec::new();
    with_thread_silenced_panics(|| {
        for base in BASES {
            for ctx in CONTEXTS {
                if is_excluded(base, ctx) {
                    continue;
                }
                let outcome = run(base, ctx);
                let ok = satisfied(&cell_expect(base, ctx), &outcome);
                out.push((base.directive, ctx.id, ok, outcome));
            }
        }
    });
    out
}

/// The sweep. Each `(directive, context)` cell asserts that the directive's effect — or a loud
/// refusal — is observable through the reference; pinned cells are asserted to STILL be dropped so
/// the pin flips loudly when a fix lands. A panic is its own failure kind.
#[test]
fn directive_referencing_context_sweep() {
    for (directive, context, _) in KNOWN_REFERENCE_DROP {
        assert!(
            BASES.iter().any(|b| b.directive == *directive)
                && CONTEXTS.iter().any(|c| c.id == *context),
            "KNOWN_REFERENCE_DROP names cell `{directive}` at `{context}` that is no longer swept — \
             stale pin, remove or fix it"
        );
    }

    let mut failures: Vec<String> = Vec::new();
    for (directive, context, ok, outcome) in sweep_verdicts() {
        let pinned = BASES
            .iter()
            .find(|b| b.directive == directive)
            .and_then(|b| {
                CONTEXTS
                    .iter()
                    .find(|c| c.id == context)
                    .and_then(|c| is_pinned(b, c))
            });
        match pinned {
            Some(reason) => {
                if ok {
                    failures.push(format!(
                        "[{directive} @ {context}] pinned as a reference-context drop ({reason}) \
                         but now SATISFIES its expectation — a fix landed; remove it from \
                         KNOWN_REFERENCE_DROP.\n{}",
                        describe(&outcome)
                    ));
                }
            }
            None => {
                if !ok {
                    failures.push(format!(
                        "[{directive} @ {context}] the directive's effect is NOT observable through \
                         this reference. If the base shape is the honored one and the anchor is the \
                         documented effect, this is a NEW referencing-context drop — pin it in \
                         KNOWN_REFERENCE_DROP with a reason and report it (do NOT re-author the \
                         expectation, and do NOT fix it opportunistically).\n{}",
                        describe(&outcome)
                    ));
                }
            }
        }
    }

    assert!(
        failures.is_empty(),
        "directive × referencing-context sweep failures:\n\n{}",
        failures.join("\n\n")
    );
}

/// LOCKSTEP with `comment_ast::KNOWN_RULE_METADATA_TAGS`: every directive has a cell in every
/// context, or a reasoned [`EXCLUSIONS`] entry. A NEW directive fails here until its author
/// classifies it — the same forcing shape that constant's other consumers use.
///
/// The registry is also checked for staleness in both directions, because an exclusion that names
/// nothing is a coverage hole that reads as a decision.
#[test]
fn every_directive_is_swept_or_excluded() {
    let mut problems: Vec<String> = Vec::new();

    for (directive, context, reason) in EXCLUSIONS {
        if !KNOWN_RULE_METADATA_TAGS.contains(directive) {
            problems.push(format!(
                "EXCLUSIONS names `{directive}`, which is not a KNOWN_RULE_METADATA_TAGS member — \
                 stale exclusion, remove it"
            ));
        }
        if *context != ALL_CONTEXTS && !CONTEXTS.iter().any(|c| c.id == *context) {
            problems.push(format!(
                "EXCLUSIONS entry `{directive}` names context `{context}`, which is not a CONTEXTS \
                 row — stale exclusion, remove or retarget it"
            ));
        }
        if reason.trim().is_empty() {
            problems.push(format!(
                "EXCLUSIONS entry `{directive}` @ `{context}` has no reason — an exclusion is a \
                 structural claim, not a TODO"
            ));
        }
        let has_row = BASES.iter().any(|b| b.covers.contains(directive));
        if *context == ALL_CONTEXTS && has_row {
            problems.push(format!(
                "EXCLUSIONS exempts `{directive}` from ALL contexts, but a BASES row covers it — the \
                 blanket exemption is stale; narrow it to the contexts it means"
            ));
        }
        if *context != ALL_CONTEXTS && !has_row {
            problems.push(format!(
                "EXCLUSIONS exempts `{directive}` at `{context}`, but no BASES row covers that \
                 directive at all — it needs the `{ALL_CONTEXTS}` exemption instead"
            ));
        }
    }

    for tag in KNOWN_RULE_METADATA_TAGS {
        for ctx in CONTEXTS {
            let swept = BASES
                .iter()
                .any(|b| b.covers.contains(tag) && !is_excluded(b, ctx));
            let excluded = EXCLUSIONS.iter().any(|(directive, context, _)| {
                directive == tag && (*context == ALL_CONTEXTS || *context == ctx.id)
            });
            if !swept && !excluded {
                problems.push(format!(
                    "`{tag}` has no referencing-context cell at `{}` and no EXCLUSIONS entry — a \
                     new directive is unclassified: give it a BASES row (a base shape the directive \
                     is HONORED on plus the anchor proving the effect crossed the reference), or an \
                     EXCLUSIONS entry with a STRUCTURAL reason.",
                    ctx.id
                ));
            }
        }
    }

    // A row that covers a tag outside the vocabulary is the same drift in the other direction.
    for base in BASES {
        for tag in base.covers {
            assert!(
                KNOWN_RULE_METADATA_TAGS.contains(tag),
                "BASES row `{}` claims to cover `{tag}`, which is not a KNOWN_RULE_METADATA_TAGS \
                 member",
                base.directive
            );
        }
    }

    assert!(
        problems.is_empty(),
        "directive-axis lockstep failures:\n\n{}",
        problems.join("\n\n")
    );
}

/// Anti-vacuity, context side: every [`CONTEXTS`] row must carry at least one directive's effect.
///
/// A context whose CDDL stopped generating (a syntax change, a new rejection) would turn every cell
/// in its column into a "drop" — and a wall of pins would then read as a finding. This is what makes
/// that impossible without noticing.
#[test]
fn every_context_row_is_live() {
    let verdicts = sweep_verdicts();
    for ctx in CONTEXTS {
        assert!(
            verdicts
                .iter()
                .any(|(_, context, ok, _)| *context == ctx.id && *ok),
            "context `{}` satisfies NO cell — its CDDL no longer carries any directive's effect \
             through a reference, so every pin in its column would hold vacuously. Fix the context, \
             not the pins.",
            ctx.id
        );
    }
}

/// Anti-vacuity, base side: every [`BASES`] row must be honored through at least one reference.
///
/// This is the cell definition's "base shape it is HONORED on" made mechanical: a base whose
/// directive is dropped in EVERY context is either a mis-authored base shape or a mis-typed anchor,
/// and in both cases pinning nine cells would record a finding the sweep did not actually make.
#[test]
fn every_base_row_is_live() {
    let verdicts = sweep_verdicts();
    for base in BASES {
        assert!(
            verdicts
                .iter()
                .any(|(directive, _, ok, _)| *directive == base.directive && *ok),
            "base shape `{}` satisfies NO cell — the shape is not honored through ANY reference, so \
             it is not the honored base shape a cell of this sweep is defined against. Fix the base \
             shape or the anchor before pinning anything in its row.",
            base.directive
        );
    }
}

/// The honored counterpart to the `@custom_wire_major` row's ten refusals: the SAME base, reached
/// through the one reference that consumes a declared major — an open table whose typed-row dispatch
/// must know the major before any deserializer runs.
///
/// Without this, those ten refusals would be indistinguishable from "the declaration cannot be
/// reached through a reference at all". With it, they are attributable to the contexts having no
/// consumer: one reference over, the same declaration crosses and drives the dispatch arm.
#[test]
fn custom_wire_major_is_honored_through_the_typed_row_reference() {
    const SPEC: &str = "rb = _CDDL_CODEGEN_RAW_BYTES_TYPE_\n\
                        base = rb ; @custom_serialize ser_base @custom_deserialize deser_base @custom_wire_major text\n\
                        ctx = { * base => bool, * uint => bool }\n\
                        holder = [f: ctx]\n";
    let src = generate(SPEC, &[], false, "refctx_wire_major_control")
        .expect("generation of the typed-row-over-declared-major spec must succeed")
        .into_values()
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        src.contains("cbor_event::Type::Text =>"),
        "the DECLARED major must drive the typed row's dispatch arm through the reference:\n{src}"
    );
    assert!(
        src.contains("deser_base(raw)"),
        "the dispatch arm must route to the referenced alias's @custom_deserialize:\n{src}"
    );
    assert!(
        !src.contains("cbor_event::Type::Bytes =>"),
        "the REPLACED type's own major must not be what the dispatch keys on:\n{src}"
    );
}
