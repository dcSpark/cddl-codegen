//! Refused-name × resolution-context CLOSURE sweep — the closure gate for the side-door class.
//!
//! A refusal recorded at ONE name-resolution seam does not bind the others. The worked example that
//! forced this module: the narrower-float-name refusal shipped at
//! `IntermediateTypes::new_type`'s unresolved-reserved fallback, and `x = float16 .size 4` still
//! generated an `f32`-backed codec at exit 0 — because a control operator resolves its head through
//! `parsing::ident_to_primitive` and never calls `new_type` at all. The per-name position sweeps
//! (`robustness_tests::undefined_prelude_rejects_gracefully_in_every_position` and its siblings) vary
//! the POSITION but hold the resolution MECHANISM constant, so a second resolution path is invisible
//! to them by construction. This module is the systematic layer.
//!
//! A cell is `(refused name, syntactic context)`. The verdict is
//! **{graceful refusal naming the type, loud rejection}** — NEVER exit-0 generation, NEVER a panic.
//! WORDING is not this sweep's business: the per-name message pins named above own that, and they
//! stay. This module owns only the closure property, which is why its cells are cheap (one-rule
//! specs, generation-only, one profile).
//!
//! # Both axes are DERIVED, not transcribed
//!
//! **Names.** [`IntermediateTypes::REFUSED_PRELUDE_NAMES`] is the inventory, read BY the interception
//! arms themselves (not mirrored beside them), and [`the_refused_name_axis_is_the_refusal_inventory`]
//! re-derives it empirically: it probes EVERY member of [`crate::utils::RESERVED_IDENTS`] (plus
//! `any`, which is not reserved but is intercepted one arm earlier) at the canonical member position
//! and requires the names that refuse to be exactly that constant. A new refusal arm therefore fails
//! that derivation until the name is added to the inventory, and adding it to the inventory demands a
//! cell in every context here — the forcing shape `KNOWN_RULE_METADATA_TAGS` uses for directives.
//!
//! **Contexts.** [`SEAMS`] is a registry of the RESOLUTION MECHANISMS with their caller classes
//! listed (enumerated by reading each mechanism's call sites, never by keyword grep — a grep can
//! support a positive finding, never the negative "no other seam exists"), and every [`CONTEXTS`] row
//! names the seam it reaches. [`every_seam_row_is_reached`] keeps the two in lockstep.
//!
//! # What is NOT on this axis (scoping, recorded rather than silently skipped)
//!
//! * **Shape-keyed refusals.** Cycle 6's recursion-refusal delivery refuses a shape (a cycle the
//!   emitted Rust cannot compile), not a name — the same CDDL name is fine outside the cycle, so
//!   there is no name axis for it to join. Its own coverage is the recursive-type boundary's
//!   fixtures (`tests/robustness/recursive_*.cddl`) and the roadmap entry that owns them.
//! * **Declaration-keyed name refusals.** A rule or field whose NAME collides with a Rust keyword or
//!   a `STD_TYPES` name is refused too, but that is a refusal about DECLARING a name, not about
//!   RESOLVING one — a different product with a different context axis. Owned by
//!   `tests::identifier_hazard_tests`.
//! * **Cross-crate name collisions** (`--extern-import`'s "needed from two exports" / "the consumer
//!   also defines"): keyed on a name being defined TWICE, not on which name it is. Owned by
//!   `tests::extern_import_tests`. That seam is still swept here for the property this module owns —
//!   see [`refusal_survives_extern_import_input_assembly`].
//!
//! # Parser rejection vs OUR refusal
//!
//! Only the latter is this tool's refusal surface, so the two are distinguished rather than pooled:
//! a spec the `cddl` crate itself refuses never reaches a resolution seam and proves nothing about
//! one. Every context here is asserted to parse and GENERATE for the control name
//! ([`CONTROL_NAME`]) — which is what makes a refused name's verdict attributable to the NAME. A
//! context where even the control does not generate is [`Attribution::ContextOwned`]: excluded, with
//! the control's own verdict recorded and re-checked, so the exclusion goes stale loudly the day the
//! underlying defect is fixed.

use crate::cli::Cli;
use crate::intermediate::IntermediateTypes;
use crate::tests::dsl_position_tests::generate;
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

/// The head every context is proven LIVE with: a supported prelude name that reaches the same seams
/// the refused names do. `uint` rather than a user-defined rule because a user rule resolves through
/// `resolve_alias` and never reaches the unresolved-reserved fallback at all, so it could not show
/// that the context's route to that fallback is open.
const CONTROL_NAME: &str = "uint";

/// The `any` prelude name — intercepted at the same fallback as the refused names but resolving to
/// the `AnyCbor` runtime type, so it is SUPPORTED. Named here because it is the probe universe's one
/// member that [`crate::utils::RESERVED_IDENTS`] does not carry (it classes as `AliasIdent::Rust`, so
/// a user rule named `any` shadows it), and the derivation would otherwise miss the arm entirely.
const ANY_NAME: &str = "any";

// ---- the seam registry -------------------------------------------------------------------------

/// One RESOLUTION MECHANISM: the function a syntactic position resolves a type NAME through.
///
/// `callers` is the enumerated list of call-site classes in `src/` (production only), which is what
/// makes "these are the seams" a claim about a registry rather than about a search vocabulary.
struct Seam {
    id: &'static str,
    /// The production function, spelled as a reader would grep it.
    mechanism: &'static str,
    /// The call-site classes that reach it, listed.
    callers: &'static [&'static str],
}

/// The resolution seams a written type NAME can be resolved through. Enumerated by reading each
/// mechanism's call sites in `src/` (excluding `src/tests/`), not by searching for a word.
const SEAMS: &[Seam] = &[
    Seam {
        id: "new_type",
        mechanism: "IntermediateTypes::new_type (src/intermediate/mod.rs)",
        callers: &[
            "parsing::parse_type — the rule-body typename arm (`types.new_type(&cddl_ident, cli).tag_if(outer_tag)`)",
            "parsing::generic_instance_or_new_type — the shared helper every TYPE position routes \
             through (`rust_type_from_type2`: member, element, map key, map value, type-choice arm, \
             tag payload, `.cbor` target, generic argument), which is `new_type` verbatim when the \
             reference carries no generic args",
            "parsing::rust_type_from_type2 — the group-entry typename path (`types.new_type(&cddl_ident, cli)`)",
            "parsing — the combined-name mint for a generic INSTANCE (`types.new_type(&CDDLIdent::new(combined_name), cli)`)",
            "IntermediateTypes::new_type itself — the unresolved-reserved fallback's `prelude_<x>` recursion",
        ],
    },
    Seam {
        id: "ident_to_primitive",
        mechanism: "parsing::ident_to_primitive (src/parsing.rs)",
        callers: &[
            "parsing::try_float_or_reject — the float-window pre-pass of `parse_control_operator`",
            "parsing::parse_type — the rule-position `ControlOperator::Range` arm",
            "parsing::parse_type — the rule-position `ControlOperator::RangeFloat` arm",
            "parsing::parse_type — the rule-position `ControlOperator::CBOR` arm",
            "parsing::rust_type_from_type1 — the MEMBER-position range and window arms",
        ],
    },
    Seam {
        id: "field_name",
        mechanism: "parsing::type_to_field_name / group_entry_to_field_name (src/parsing.rs)",
        callers: &[
            "parsing::group_entry_to_field_name — the `None` branch for an entry with no explicit \
             key, which derives the field name from the entry's TYPE name",
            "parsing::type_to_field_name — its own recursion through a collection's element type",
        ],
    },
    Seam {
        id: "scan_consumer",
        mechanism: "extern_narrow::scan_consumer (src/extern_narrow.rs)",
        callers: &[
            "api::append_extern_imports — input assembly, BEFORE the checked parse, reached only \
             under `--extern-import`",
        ],
    },
];

// ---- the context registry ----------------------------------------------------------------------

/// Whether a context's cells can attribute their verdict to the NAME.
enum Attribution {
    /// [`CONTROL_NAME`] GENERATES here, so anything a refused name does differently is the name's
    /// doing. These are the product's live columns.
    Swept,
    /// The context refuses or aborts for its OWN reasons — identically for the supported control
    /// head — so no cell of it says anything about the name. Excluded, with the control's verdict
    /// recorded so [`every_context_owned_exclusion_is_still_context_owned`] fails loudly if the
    /// underlying behaviour changes.
    ContextOwned {
        control: ControlVerdict,
        reason: &'static str,
    },
}

/// What [`CONTROL_NAME`] does in a [`Attribution::ContextOwned`] context.
enum ControlVerdict {
    /// A graceful `Err` containing this substring.
    Refuses(&'static str),
    /// A `panic!` containing this substring — a name-INDEPENDENT abort, which is a finding on the
    /// SHAPE axis rather than on this one. Each such row's reason names where it is pinned.
    Aborts(&'static str),
}

/// One syntactic context: a one-rule spec with `%N%` standing for the type name under test.
struct Context {
    id: &'static str,
    /// The [`SEAMS`] row this position resolves `%N%` through.
    seam: &'static str,
    cddl: &'static str,
    attribution: Attribution,
}

/// The context axis — the syntactic positions that reach the [`SEAMS`] rows. The roadmap entry's
/// list (rule body, member, array element, map key, map value, choice arm, control-operator head,
/// generic argument, tag payload, `.cbor` target) is the spine; the rest are the positions that
/// reach the same seams by a DIFFERENT route (an unnamed member reaches the field-name seam, a
/// spliced group reaches the member seam without a rule of its own, a member-position control
/// operator reaches `ident_to_primitive` through `rust_type_from_type1` rather than through
/// `parse_type`).
///
/// The control-operator family is spelled per ARM rather than once: `.size` and a `.le` window and
/// `.default` and `.eq` are four different arms of `parse_control_operator`, and the delivery that
/// motivated this module is the standing evidence that one arm's verdict does not transfer to
/// another's.
const CONTEXTS: &[Context] = &[
    // ---- the `new_type` seam: the TYPE positions ------------------------------------------------
    Context {
        id: "rule-body",
        seam: "new_type",
        cddl: "x = %N%\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "record-member",
        seam: "new_type",
        cddl: "a = [v: %N%, x: uint]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "optional-member",
        seam: "new_type",
        cddl: "a = [? v: %N%]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "array-element",
        seam: "new_type",
        cddl: "a = [* %N%]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "nested-array-element",
        seam: "new_type",
        cddl: "a = [[* %N%]]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "map-key",
        seam: "new_type",
        cddl: "m = { * %N% => uint }\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "map-value",
        seam: "new_type",
        cddl: "m = { * uint => %N% }\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "fixed-label-map-value",
        seam: "new_type",
        cddl: "m = { 1: %N% }\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "type-choice-arm",
        seam: "new_type",
        cddl: "x = %N% / uint\n",
        attribution: Attribution::Swept,
    },
    // The `T / null` spelling collapses to `Option<T>` at a DIFFERENT arm of the choice walk than
    // the general one above, so it is its own row rather than a spelling of it.
    Context {
        id: "nullable-choice-arm",
        seam: "new_type",
        cddl: "x = %N% / null\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "group-choice-arm",
        seam: "new_type",
        cddl: "x = [ %N% // uint ]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "tag-payload",
        seam: "new_type",
        cddl: "x = #6.9(%N%)\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "cbor-target",
        seam: "new_type",
        cddl: "x = bytes .cbor %N%\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "generic-argument",
        seam: "new_type",
        cddl: "g<T> = [x: T]\nx = g<%N%>\n",
        attribution: Attribution::Swept,
    },
    // A SPLICED group reaches the member seam with no rule of its own between the name and the
    // holder — the one member route that owns no registration.
    Context {
        id: "spliced-group-member",
        seam: "new_type",
        cddl: "g = (v: %N%)\na = [g]\n",
        attribution: Attribution::Swept,
    },
    // A transparent re-alias: the name is resolved once for the alias rule and again for the
    // reference to it, so a refusal must survive the hop rather than being consumed by it.
    Context {
        id: "alias-chain",
        seam: "new_type",
        cddl: "y = %N%\nx = y\n",
        attribution: Attribution::Swept,
    },
    // ---- the field-name seam --------------------------------------------------------------------
    // An entry with NO explicit key: the field name is DERIVED from the type's own name, so the
    // name is read by a second mechanism before (or instead of) being resolved to a type.
    Context {
        id: "unnamed-member",
        seam: "field_name",
        cddl: "a = [%N%, x: uint]\n",
        attribution: Attribution::Swept,
    },
    // ---- the `ident_to_primitive` seam: control-operator HEADS ----------------------------------
    Context {
        id: "rule-ctl-size",
        seam: "ident_to_primitive",
        cddl: "x = %N% .size 4\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "rule-ctl-window",
        seam: "ident_to_primitive",
        cddl: "x = %N% .le 3\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "rule-ctl-eq",
        seam: "ident_to_primitive",
        cddl: "x = %N% .eq 1\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "rule-ctl-ne",
        seam: "ident_to_primitive",
        cddl: "x = %N% .ne 1\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "rule-ctl-default",
        seam: "ident_to_primitive",
        cddl: "x = %N% .default 1\n",
        attribution: Attribution::Swept,
    },
    // The MEMBER-position control arms are a separate route to the same seam
    // (`rust_type_from_type1`, not `parse_type`), which is exactly the kind of second route this
    // module exists to keep honest.
    Context {
        id: "member-ctl-size",
        seam: "ident_to_primitive",
        cddl: "a = [v: %N% .size 4, x: uint]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "member-ctl-window",
        seam: "ident_to_primitive",
        cddl: "a = [v: %N% .le 3, x: uint]\n",
        attribution: Attribution::Swept,
    },
    Context {
        id: "member-ctl-default",
        seam: "ident_to_primitive",
        cddl: "a = [v: %N% .default 1, x: uint]\n",
        attribution: Attribution::Swept,
    },
    // ---- CONTEXT-OWNED columns (excluded, with the control's own verdict re-checked) -------------
    Context {
        id: "rangeop-typename-start",
        seam: "ident_to_primitive",
        cddl: "x = %N%..10\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Aborts("Number expected as range start"),
            reason: "a TYPENAME as a range bound aborts for every head, `uint` included — the range \
                     arm reads its bounds as literal values and panics on anything else, before any \
                     name is resolved. Name-INDEPENDENT, so no cell here is attributable to the \
                     name; it is a finding on the SHAPE axis, pinned as a PANIC row by \
                     tests/robustness/rangeop_typename_start.cddl.",
        },
    },
    Context {
        id: "ctl-cbor-head",
        seam: "ident_to_primitive",
        cddl: "x = %N% .cbor uint\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Aborts(".cbor is only allowed on bytes"),
            reason: "`.cbor` on a non-`bytes` head aborts for every head, `uint` included. \
                     Name-INDEPENDENT; pinned as a PANIC row by \
                     tests/robustness/ctl_cbor_non_bytes_head.cddl.",
        },
    },
    Context {
        id: "ctl-bits",
        seam: "ident_to_primitive",
        cddl: "x = %N% .bits uint\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Aborts("range control operator: .bits"),
            reason: "`.bits` is an unimplemented control operator and aborts before its head is \
                     looked at. Name-INDEPENDENT; already a PANIC catalog row \
                     (tests/matrix_panic/ctl.bits.cddl).",
        },
    },
    Context {
        id: "ctl-regexp",
        seam: "ident_to_primitive",
        cddl: "x = %N% .regexp \"a\"\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Aborts("range control operator: .regexp"),
            reason: "`.regexp` is an unimplemented control operator and aborts before its head is \
                     looked at. Name-INDEPENDENT; already a PANIC catalog row \
                     (tests/matrix_panic/ctl.regexp.cddl).",
        },
    },
    Context {
        id: "ctl-within",
        seam: "ident_to_primitive",
        cddl: "x = %N% .within uint\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Refuses("the `.within` control operator is unsupported"),
            reason: "`.within` is refused for the OPERATOR, whatever its head is — the control's \
                     own graceful rejection fires for `uint` too, so a refused name's `Err` here \
                     would be the context's and not the name's. Pinned by \
                     tests/matrix_reject/ctl.within.cddl.",
        },
    },
    Context {
        id: "ctl-and",
        seam: "ident_to_primitive",
        cddl: "x = %N% .and uint\n",
        attribution: Attribution::ContextOwned {
            control: ControlVerdict::Refuses("the `.and` control operator is unsupported"),
            reason: "`.and` is refused for the OPERATOR, whatever its head is — same shape as \
                     `.within` above. Pinned by tests/matrix_reject/ctl.and.cddl.",
        },
    },
];

// ---- the findings ledger -------------------------------------------------------------------------

/// How a cell BREACHES the closure property.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Breach {
    /// The refused name reached generation and the tool exited 0 — silently wrong output.
    Exit0,
    /// The walk aborted, destroying the graceful refusal the seam had already recorded.
    Panic,
}

/// Cells that BREACH the closure property today — FINDINGS, not fixtures. Each is asserted to STILL
/// breach, in the recorded KIND, with the recorded evidence present, so the pin flips loudly the day
/// a fix lands and cannot hold vacuously through a different failure.
///
/// AUTHORING RULE (the discipline `dsl_position_tests::KNOWN_SILENT_DROP` set): a new entry is a NEW
/// finding — pin it with its reason and REPORT it; never re-author a context to dodge it, and never
/// fix it opportunistically in the same delivery unless the fix is one obviously-correct line at an
/// EXISTING refusal seam. Every entry names where the finding is ledgered.
///
/// `(name, context, kind, evidence substring, reason)`.
const KNOWN_CLOSURE_BREACH: &[(&str, &str, Breach, &str, &str)] = &[
    // The `.default` arm applies the operator to whatever the head resolved to, and
    // `RustType::default` panics when that is not a Primitive matching the default's value class.
    // For a REFUSED name the head resolved to the refusal's inert `Fixed(Null)` placeholder — so the
    // rejection is recorded and then destroyed by an abort one step later, which is precisely the
    // side door this module exists to find. The abort is not exclusive to refused names (`tdate`, a
    // SUPPORTED prelude name with no rust primitive, aborts identically), so the fix is a new
    // refusal at the `.default` application rather than one line at an existing seam — out of this
    // delivery's scope by its own rule. Ledgered in draft/burndown2/ and pinned as a PANIC catalog
    // row by tests/robustness/ctl_default_unmapped_head.cddl.
    (
        "undefined",
        "rule-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "the refusal's inert placeholder reaches `RustType::default`, which aborts before \
         `finalize` can drain the recorded rejection",
    ),
    (
        "undefined",
        "member-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site, reached through `rust_type_from_type1` instead of `parse_type`",
    ),
    (
        "cbor-any",
        "rule-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site as `undefined`'s — the four `any`-content tags share the placeholder",
    ),
    (
        "cbor-any",
        "member-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site, member route",
    ),
    (
        "eb64url",
        "rule-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site as `undefined`'s",
    ),
    (
        "eb64url",
        "member-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site, member route",
    ),
    (
        "eb64legacy",
        "rule-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site as `undefined`'s",
    ),
    (
        "eb64legacy",
        "member-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site, member route",
    ),
    (
        "eb16",
        "rule-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site as `undefined`'s",
    ),
    (
        "eb16",
        "member-ctl-default",
        Breach::Panic,
        ".default Uint(1) invalid for type Fixed(Null)",
        "same site, member route",
    ),
];

// ---- the machinery -------------------------------------------------------------------------------

/// What one generation did.
enum Outcome {
    /// Exited 0 with source.
    Generated,
    /// A graceful `Err`.
    Refused(String),
    /// A `panic!`, with its message.
    Aborted(String),
}

impl Outcome {
    fn describe(&self) -> String {
        match self {
            Outcome::Generated => "GENERATED (exit 0)".to_owned(),
            Outcome::Refused(e) => format!("graceful Err: {}", e.replace('\n', " | ")),
            Outcome::Aborted(m) => format!("PANIC: {m}"),
        }
    }
}

/// Generate `%N%`-substituted `cddl` under the default profile and classify the outcome.
///
/// One profile on purpose: a refusal short-circuits `finalize` BEFORE any emission, so no flag can
/// change whether a name is refused — and the per-name message pins this module generalizes already
/// assert both profiles at the three positions they cover. What varies here is the resolution route,
/// which is a parse-walk property, not an emission one.
fn run(name: &str, ctx: &Context) -> Outcome {
    let spec = ctx.cddl.replace("%N%", name);
    let tag: String = format!("refname_{name}_{}", ctx.id)
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        generate(&spec, &[], false, &tag)
    })) {
        Ok(Ok(_)) => Outcome::Generated,
        Ok(Err(e)) => Outcome::Refused(e),
        Err(payload) => Outcome::Aborted(
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_owned()))
                .unwrap_or_else(|| "<non-string panic payload>".to_owned()),
        ),
    }
}

fn swept_contexts() -> impl Iterator<Item = &'static Context> {
    CONTEXTS
        .iter()
        .filter(|c| matches!(c.attribution, Attribution::Swept))
}

fn pin(
    name: &str,
    ctx: &str,
) -> Option<&'static (
    &'static str,
    &'static str,
    Breach,
    &'static str,
    &'static str,
)> {
    KNOWN_CLOSURE_BREACH
        .iter()
        .find(|(n, c, _, _, _)| *n == name && *c == ctx)
}

// ---- the sweep ---------------------------------------------------------------------------------

/// The product. Every `(refused name, swept context)` cell must end in a graceful refusal NAMING the
/// type, or a loud rejection — never exit-0 generation, never a panic.
///
/// "Naming the type" is asserted as the CDDL name appearing in the message, not as a wording: which
/// sentence a seam uses belongs to the per-name message pins, but a refusal that does not say WHICH
/// type it refused is not actionable from any seam.
#[test]
fn refused_name_closure_sweep() {
    for (name, ctx, _, _, _) in KNOWN_CLOSURE_BREACH {
        assert!(
            IntermediateTypes::REFUSED_PRELUDE_NAMES.contains(name)
                && swept_contexts().any(|c| c.id == *ctx),
            "KNOWN_CLOSURE_BREACH names cell `{name}` @ `{ctx}`, which this sweep no longer runs — \
             stale pin, remove or retarget it"
        );
    }

    let mut failures: Vec<String> = Vec::new();
    with_thread_silenced_panics(|| {
        for name in IntermediateTypes::REFUSED_PRELUDE_NAMES {
            for ctx in swept_contexts() {
                let outcome = run(name, ctx);
                let breach = match &outcome {
                    Outcome::Generated => Some(Breach::Exit0),
                    Outcome::Aborted(_) => Some(Breach::Panic),
                    Outcome::Refused(_) => None,
                };
                match (pin(name, ctx.id), breach) {
                    // Pinned and still breaching in the recorded way: assert the EVIDENCE too, so a
                    // pin cannot hold through a different failure than the one it records.
                    (Some((_, _, kind, evidence, reason)), Some(seen)) => {
                        if *kind != seen {
                            failures.push(format!(
                                "[{name} @ {}] pinned as {kind:?} but breaches as {seen:?} now \
                                 ({reason}) — the finding CHANGED shape; re-triage it.\n{}",
                                ctx.id,
                                outcome.describe()
                            ));
                        } else {
                            let text = match &outcome {
                                Outcome::Aborted(m) => m.clone(),
                                _ => String::new(),
                            };
                            if !text.contains(evidence) {
                                failures.push(format!(
                                    "[{name} @ {}] pinned on evidence `{evidence}`, which the \
                                     observed breach does not carry — the pin would hold \
                                     vacuously.\n{}",
                                    ctx.id,
                                    outcome.describe()
                                ));
                            }
                        }
                    }
                    (Some((_, _, kind, _, reason)), None) => failures.push(format!(
                        "[{name} @ {}] pinned as a {kind:?} breach ({reason}) but now REFUSES \
                         gracefully — a fix landed; remove it from KNOWN_CLOSURE_BREACH and retire \
                         its ledger row.\n{}",
                        ctx.id,
                        outcome.describe()
                    )),
                    (None, Some(seen)) => failures.push(format!(
                        "[{name} @ {}] {seen:?} — a name REFUSED at one resolution seam reached \
                         this context anyway. This is a NEW closure breach: pin it in \
                         KNOWN_CLOSURE_BREACH with its kind, evidence and reason, and REPORT it \
                         (do not re-author the context, and do not fix it opportunistically unless \
                         the fix is one obviously-correct line at an EXISTING refusal seam).\n{}",
                        ctx.id,
                        outcome.describe()
                    )),
                    (None, None) => {
                        if matches!(&outcome, Outcome::Refused(msg) if !msg.contains(name)) {
                            failures.push(format!(
                                "[{name} @ {}] refuses without NAMING the type — a refusal a \
                                 consumer cannot act on.\n{}",
                                ctx.id,
                                outcome.describe()
                            ));
                        }
                    }
                }
            }
        }
    });

    assert!(
        failures.is_empty(),
        "refused-name × resolution-context closure failures ({} of {} cells):\n\n{}",
        failures.len(),
        IntermediateTypes::REFUSED_PRELUDE_NAMES.len() * swept_contexts().count(),
        failures.join("\n\n")
    );
}

/// The name axis is DERIVED from behaviour, not transcribed: probe every reserved ident (plus `any`)
/// at the canonical member position, and require the set that REFUSES to be exactly
/// [`IntermediateTypes::REFUSED_PRELUDE_NAMES`].
///
/// This is what makes the inventory a forcing constant rather than a comment. A new name-keyed
/// refusal arm fails HERE until the inventory names it, and naming it in the inventory immediately
/// demands a cell in every context of the sweep above — the shape `KNOWN_RULE_METADATA_TAGS` uses to
/// make a new directive unclassifiable-by-default.
///
/// The union check below is the other half: the two constants the ARMS read
/// (`UNDEFINED_PRELUDE_NAME`, `ANY_CONTENT_PRELUDE_TAGS`) must together BE the inventory, so a name
/// cannot be added to an arm's key without joining the list the sweep runs.
#[test]
fn the_refused_name_axis_is_the_refusal_inventory() {
    let mut union = vec![IntermediateTypes::UNDEFINED_PRELUDE_NAME];
    union.extend_from_slice(IntermediateTypes::ANY_CONTENT_PRELUDE_TAGS);
    union.sort_unstable();
    assert_eq!(
        union,
        IntermediateTypes::REFUSED_PRELUDE_NAMES.to_vec(),
        "the refusal inventory must be exactly the names the interception ARMS key on — a name in \
         an arm but not in the inventory is refused at one seam and swept at none"
    );

    let mut refused: Vec<&str> = Vec::new();
    with_thread_silenced_panics(|| {
        for name in crate::utils::RESERVED_IDENTS.iter().chain([&ANY_NAME]) {
            // The canonical member position: the plainest route to the resolution fallback, and the
            // one every reserved name can be written at.
            let ctx = Context {
                id: "derivation-probe",
                seam: "new_type",
                cddl: "a = [v: %N%, x: uint]\n",
                attribution: Attribution::Swept,
            };
            match run(name, &ctx) {
                Outcome::Refused(_) => refused.push(name),
                Outcome::Generated => {}
                Outcome::Aborted(m) => panic!(
                    "the derivation probe must not abort — `{name}` at the canonical member \
                     position panicked: {m}"
                ),
            }
        }
    });
    refused.sort_unstable();

    assert_eq!(
        refused,
        IntermediateTypes::REFUSED_PRELUDE_NAMES.to_vec(),
        "the DERIVED refusal inventory (the reserved idents that refuse at the canonical member \
         position) differs from `IntermediateTypes::REFUSED_PRELUDE_NAMES`. A name that refuses but \
         is not in the constant is refused at one seam and swept at NONE — add it to the constant, \
         which is what makes the closure sweep cover it. A name in the constant that no longer \
         refuses is a stale inventory entry — remove it, and retire its cells."
    );
}

/// Anti-vacuity, context side: [`CONTROL_NAME`] must GENERATE in every swept context.
///
/// A context that stopped generating at all — a syntax change, a new rejection, an abort — would
/// turn its whole column into "the name was refused", and the sweep would read green while proving
/// nothing. This is what makes each cell's verdict attributable to the NAME.
#[test]
fn every_swept_context_is_live() {
    let mut dead: Vec<String> = Vec::new();
    with_thread_silenced_panics(|| {
        for ctx in swept_contexts() {
            let outcome = run(CONTROL_NAME, ctx);
            if !matches!(outcome, Outcome::Generated) {
                dead.push(format!(
                    "context `{}` does not generate for the control head `{CONTROL_NAME}` — every \
                     cell in its column would then be the CONTEXT's verdict, not the name's. Either \
                     fix the context's CDDL, or move it to Attribution::ContextOwned with the \
                     control's verdict recorded.\n{}",
                    ctx.id,
                    outcome.describe()
                ));
            }
        }
    });
    assert!(
        dead.is_empty(),
        "dead context rows:\n\n{}",
        dead.join("\n\n")
    );
}

/// The mirror guard: every [`Attribution::ContextOwned`] row must STILL be context-owned, in the
/// recorded way.
///
/// An exclusion is a structural claim ("this column says nothing about the name"), and a claim that
/// silently stops holding is a coverage hole that reads as a decision. So the control head is run
/// there too, and its verdict must match what the row recorded — which means the day one of those
/// name-independent aborts is FIXED, this test fails and the column joins the product.
#[test]
fn every_context_owned_exclusion_is_still_context_owned() {
    let mut stale: Vec<String> = Vec::new();
    with_thread_silenced_panics(|| {
        for ctx in CONTEXTS {
            let Attribution::ContextOwned { control, reason } = &ctx.attribution else {
                continue;
            };
            assert!(
                !reason.trim().is_empty(),
                "context `{}` is excluded with no reason — an exclusion is a structural claim, not \
                 a TODO",
                ctx.id
            );
            let outcome = run(CONTROL_NAME, ctx);
            let ok = match (control, &outcome) {
                (ControlVerdict::Refuses(sub), Outcome::Refused(e)) => e.contains(sub),
                (ControlVerdict::Aborts(sub), Outcome::Aborted(m)) => m.contains(sub),
                _ => false,
            };
            if !ok {
                stale.push(format!(
                    "context `{}` records that the control head `{CONTROL_NAME}` {} — it no longer \
                     does. If the underlying name-independent defect was fixed, delete the \
                     exclusion and sweep the column (its reason: {reason}).\n{}",
                    ctx.id,
                    match control {
                        ControlVerdict::Refuses(sub) => format!("refuses with `{sub}`"),
                        ControlVerdict::Aborts(sub) => format!("aborts with `{sub}`"),
                    },
                    outcome.describe()
                ));
            }
        }
    });
    assert!(
        stale.is_empty(),
        "stale context-owned exclusions:\n\n{}",
        stale.join("\n\n")
    );
}

/// Registry lockstep: every [`SEAMS`] row is reached by at least one [`CONTEXTS`] row, and every
/// context names a seam that exists.
///
/// The registry is the negative claim's evidence — "these are the resolution mechanisms" is only a
/// claim about a registry if the registry's members are each exercised. A seam nothing reaches is
/// either a stale row or an unswept mechanism, and both need saying out loud.
#[test]
fn every_seam_row_is_reached() {
    let mut problems: Vec<String> = Vec::new();
    for ctx in CONTEXTS {
        if !SEAMS.iter().any(|s| s.id == ctx.seam) {
            problems.push(format!(
                "context `{}` names seam `{}`, which is not a SEAMS row",
                ctx.id, ctx.seam
            ));
        }
    }
    for seam in SEAMS {
        assert!(
            !seam.callers.is_empty() && !seam.mechanism.is_empty(),
            "seam `{}` carries no mechanism/callers — the registry's whole point is that its \
             members are LISTED",
            seam.id
        );
        // `scan_consumer` is reached by a dedicated test rather than by the product: it is not a
        // syntactic POSITION, it is a whole input-assembly posture.
        if seam.id == "scan_consumer" {
            continue;
        }
        if !CONTEXTS.iter().any(|c| c.seam == seam.id) {
            problems.push(format!(
                "seam `{}` ({}) is reached by NO context — either it is stale, or the sweep has an \
                 unswept resolution mechanism",
                seam.id, seam.mechanism
            ));
        }
    }
    assert!(
        problems.is_empty(),
        "seam/context registry drift:\n\n{}",
        problems.join("\n")
    );
}

/// The [`SEAMS`] row the product cannot reach as a syntactic position: `extern_narrow::scan_consumer`
/// runs during INPUT ASSEMBLY, before the checked parse, and only under `--extern-import`. It walks
/// the consumer's text collecting rule references, so every refused name passes through it before any
/// resolution seam sees it.
///
/// What must hold is this module's property, not a narrowing claim: with an import active, a refused
/// name still ends in the SAME graceful refusal it gets without one. The failure this guards against
/// is an assembly walk that aborts on a name it cannot classify, or one that swallows the rule and
/// lets generation succeed at exit 0.
///
/// The positive control is in the same spec: the consumer really does import a dependency rule, so a
/// cell cannot pass because the import silently did nothing.
#[test]
fn refusal_survives_extern_import_input_assembly() {
    let root = std::env::temp_dir().join(format!(
        "cddl_codegen_refname_import_{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("export")).unwrap();
    std::fs::write(
        root.join("export/mod.cddl"),
        "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\ndep_thing = _CDDL_CODEGEN_EXTERN_TYPE_ ; @rust_name DepThing\n",
    )
    .unwrap();
    let import_arg = format!("dep={}", root.join("export").to_str().unwrap());

    let mut failures: Vec<String> = Vec::new();
    with_thread_silenced_panics(|| {
        for name in IntermediateTypes::REFUSED_PRELUDE_NAMES {
            let consumer = root.join(format!("consumer_{name}.cddl").replace('-', "_"));
            std::fs::write(
                &consumer,
                format!("a = [d: dep_thing, v: {name}, x: uint]\n"),
            )
            .unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                consumer.to_str().unwrap(),
                "--output",
                "refname_import_unused",
                "--wasm",
                "false",
                "--extern-import",
                &import_arg,
            ]);
            let outcome = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                crate::api::generated_strings(&cli)
            })) {
                Ok(Ok(_)) => Outcome::Generated,
                Ok(Err(e)) => Outcome::Refused(e.to_string()),
                Err(_) => Outcome::Aborted("<panic>".to_owned()),
            };
            match &outcome {
                Outcome::Refused(msg) if msg.contains(name) => {}
                _ => failures.push(format!(
                    "[{name} @ extern-import input assembly] the refusal did not survive the \
                     assembly walk.\n{}",
                    outcome.describe()
                )),
            }
        }

        // Positive control: the same consumer WITHOUT a refused name must generate, and must
        // actually consume the imported rule — otherwise the refusals above could be the import
        // seam failing for its own reasons.
        let consumer = root.join("consumer_control.cddl");
        std::fs::write(&consumer, "a = [d: dep_thing, x: uint]\n").unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            consumer.to_str().unwrap(),
            "--output",
            "refname_import_unused",
            "--wasm",
            "false",
            "--extern-import",
            &import_arg,
        ]);
        match crate::api::generated_strings(&cli) {
            Ok(files) => {
                let src = files.values().cloned().collect::<Vec<_>>().join("\n");
                if !src.contains("DepThing") {
                    failures.push(format!(
                        "the extern-import control generated without the imported type — the \
                         import posture is not actually active:\n{src}"
                    ));
                }
            }
            Err(e) => failures.push(format!(
                "the extern-import control must generate, got a graceful Err: {e}"
            )),
        }
    });

    let _ = std::fs::remove_dir_all(&root);
    assert!(
        failures.is_empty(),
        "extern-import assembly seam failures:\n\n{}",
        failures.join("\n\n")
    );
}
