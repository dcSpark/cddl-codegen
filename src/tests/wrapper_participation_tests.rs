//! The wrapper-participation grid: every collection wrapper a spec implies, crossed with the
//! emission MODE that decides WHERE its `#[wasm_bindgen]` class lives.
//!
//! The ~20 committed mode pins elsewhere in this suite are INCIDENT-shaped — each records the exact
//! cell of one past escape. This module is ENUMERATION-shaped, and the difference is the whole
//! point: a participation table nobody wrote is a table nobody can read, and the failures this area
//! actually produced were never in the primary wrapper but in a COMPANION (a table's synthesized
//! keys-list, a restricted wrapper's loose `try_from` source) or a POSITION (a named-rule reference,
//! a non-root declaring scope) that one mode happened not to walk.
//!
//! Three axes:
//!
//! * MODE — `Local` (control: no dep flags), `IndexDeferred` (`--extern-wrapper-index`),
//!   `WorkspaceBorrowed` (`--workspace-dep`), `RequestedHosted` (`--wrapper-requests`, dep side).
//! * SHAPE — loose list / exact static list / loose map / NonEmpty list / NonEmpty map / named
//!   table rule / `@duplicates reject` set / `@duplicates preserve` pair map.
//! * POSITION — inline-anonymous member / named-rule DECLARATION whose ident equals the structural
//!   name / named-rule REFERENCE from another rule's member / a non-root declaring scope.
//!
//! [`PARTICIPATION_TABLE`] is the grid, as data. Each row states its EXPECTED [`Outcome`] and, when
//! an existing test already pins it, the test that does (`pinned_by`) — those rows are REFERENCED,
//! never rebuilt, so this module's generated crates cover exactly the rows nothing else did. Two
//! participation facts the grid ENCODES rather than assumes:
//!
//! * a `@duplicates reject` set participates in EVERY mode like the loose and NonEmpty twins —
//!   `generate_reject_ordered_set_type` consults the same `try_defer_wrapper` seam, and its shape
//!   column carries the `@duplicates reject` marker so a deferral and its dep-side host agree on the
//!   container. What separates its cells is the POSITION, exactly as for the twins: an inline
//!   (anonymous generic-instance) occurrence carries the structural name and defers, while a
//!   rule-declared set is the consumer's own class — deferred under the index mode's name-only
//!   unification, kept local (criterion 9) under workspace mode. A reject wrapper that DEFERS names
//!   no loose `try_from` source at all (it borrows the dependency's whole class, door included), so
//!   the "loose source deferred" companion belongs only to the rows that mint locally;
//! * the index is NAME-only and therefore flavor-SAFE by CONSTRUCTION: the structural name carries
//!   the container (`PairMapKToV` vs `MapKToV`), so a cross-flavor match is unrepresentable and the
//!   grid needs no hazard cell for it — a preserve table is an ordinary SHAPE ROW, owed one cell per
//!   mode like any other.
//!
//! Cost shape: the always-on sweep is GENERATION only — one generated crate per mode, asserted from
//! the emitted source and the run's stderr. The compile/link floors (the only place duplicate
//! `#[wasm_bindgen]` symbols are observable) are `#[ignore]`d and batched per mode in
//! [`wrapper_participation_mode_floors`], registered as a `full`-tier gate.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use super::integration_tests::{checkout_hash, codegen_cmd};

// ---------------------------------------------------------------------------------------------
// The grid, as data
// ---------------------------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum Mode {
    /// No dependency flags at all. The control: every wrapper is the consumer's, silently.
    Local,
    /// `--extern-wrapper-index=<dep>=<index>`: defer iff the dep's index lists the structural name.
    IndexDeferred,
    /// `--workspace-dep=<dep>`: an all-one-dep wrapper defers UNCONDITIONALLY and is recorded in
    /// `borrowed_collections.rs`.
    WorkspaceBorrowed,
    /// `--wrapper-requests=<label>=<sidecar>`, driven from the DEPENDENCY side: the dep mints the
    /// wrappers a consumer's sidecar recorded, into `requested_collections.rs`.
    RequestedHosted,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum Shape {
    LooseList,
    /// An ordinary/preserve `[N*N elem]`: native `[T; N]`, wasm list-shaped class, and (when this
    /// crate mints it) a loose `<Elem>List` `try_from` source.
    ExactList,
    LooseMap,
    NonEmptyList,
    NonEmptyMap,
    /// A named table RULE keyed by the element type, so the synthesized keys()-list companion exists.
    NamedTable,
    RejectSet,
    PreservePairMap,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum Position {
    /// The shape spelled inline in a record member; no rule bears its name.
    InlineAnonymous,
    /// A root rule whose ident EQUALS the structural name, referenced by nothing.
    NamedDeclaration,
    /// The same rule, referenced ONLY by rule name from another rule's member — the position that
    /// takes the alias-suppression arm plus `set_ref`, which inline pins never touch.
    NamedReference,
    /// The shape spelled inline inside a rule declared in a NON-ROOT module (a sub-file).
    NonRootScope,
    /// `RequestedHosted` has no position axis: a request carries (name, shape) only, so nothing of
    /// the consumer's reference syntax survives into the dep's run. Encoded rather than left blank,
    /// because "the axis does not apply here" is a participation fact.
    NotApplicable,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Outcome {
    /// No local class; a plain `use <dep_wasm>::collections::<Class>;` and no row in this crate's
    /// own `collections.rs`.
    Defer,
    /// Workspace deferral: `Defer`'s emitted shape PLUS a `borrowed_collections.rs` row.
    Borrow,
    /// Dep-side mint into `requested_collections.rs`, re-exported from the dep's own index.
    Host,
    /// A local class, announced on stderr. The payload is the load-bearing text fragment.
    LocalWarned(&'static str),
    /// A local class and nothing on stderr. The payload is WHY silence is correct here.
    LocalSilent(&'static str),
}

pub(crate) struct Row {
    pub(crate) mode: Mode,
    pub(crate) shape: Shape,
    pub(crate) position: Position,
    /// The CDDL element ident this row owns. Unique across the table, so every row's structural
    /// wrapper name is unique too and one generated crate can carry a whole mode's rows.
    pub(crate) elem: &'static str,
    /// The wrapper class the row is ABOUT (the emitted or deferred name).
    pub(crate) class: &'static str,
    pub(crate) expect: Outcome,
    /// An existing test that already pins this row. `Some` ⇒ the sweep references it and builds
    /// nothing; `None` ⇒ this module's per-mode batch covers it.
    pub(crate) pinned_by: Option<&'static str>,
    pub(crate) why: &'static str,
}

/// The dependency every non-`Local` row keys on. `index_dep_crate` is the committed wasm-clean pair
/// (`tests/index-dep-crate` / `tests/index-dep-crate-wasm`) the compile floors link against.
const DEP: &str = "index_dep_crate";
const DEP_WASM: &str = "index_dep_crate_wasm";

/// The grid. Ordered by mode, then shape, then position, so a reader scans one mode at a time and a
/// new shape or mode is one row rather than a new function.
///
/// Probe scope for every `expect` NOT carried by a `pinned_by` test: default profile, root scope
/// except where the position says otherwise, generation output only (the compile/link floors are the
/// `#[ignore]`d gate below). Not probed here: the preserve/json profiles, `--component`, or any
/// combination of two mode flags at once (that pairing is `workspace_dep_named_table_deferred_keys_list`'s
/// criterion-2 cell).
pub(crate) const PARTICIPATION_TABLE: &[Row] = &[
    // ===== Local (control): no dependency exists, so nothing can defer, borrow or host =====
    Row {
        mode: Mode::Local,
        shape: Shape::LooseList,
        position: Position::InlineAnonymous,
        elem: "loc_ll_inl",
        class: "LocLlInlList",
        expect: Outcome::LocalSilent("no dependency is configured, so no name can be contended"),
        pinned_by: None,
        why: "the control every deferring row is read against",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::LooseList,
        position: Position::NamedDeclaration,
        elem: "loc_ll_dec",
        class: "LocLlDecList",
        expect: Outcome::LocalSilent("a rule-declared wrapper is always the consumer's own class"),
        pinned_by: None,
        why: "ident == structural name is the coincidence the deferring modes decide on; here it is inert",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::LooseList,
        position: Position::NamedReference,
        elem: "loc_ll_ref",
        class: "LocLlRefList",
        expect: Outcome::LocalSilent("the by-name reference resolves to the local class"),
        pinned_by: None,
        why: "the reference position whose walk arm never consults the deferred map",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::LooseList,
        position: Position::NonRootScope,
        elem: "loc_ll_scp",
        class: "LocLlScpList",
        expect: Outcome::LocalSilent(
            "a non-root scope changes where the class lives, not whose it is",
        ),
        pinned_by: None,
        why: "the declaring scope the index-mode import routing has to follow",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::ExactList,
        position: Position::NonRootScope,
        elem: "loc_ex_scp",
        class: "LocExScpListMin2Max2",
        expect: Outcome::LocalSilent(
            "the static carrier is locally minted, and its non-root try_from source imports the root loose builder",
        ),
        pinned_by: None,
        why: "an exact static wrapper still needs LocExScpList as its loose Vec-to-array handover source; this row pins the root-to-non-root import",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::LooseMap,
        position: Position::InlineAnonymous,
        elem: "loc_lm_inl",
        class: "MapU64ToLocLmInl",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "map-side control",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::NonEmptyList,
        position: Position::InlineAnonymous,
        elem: "loc_nel_inl",
        class: "NonEmptyLocNelInlList",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "companion: the loose `try_from` source LocNelInlList is minted locally too",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::NonEmptyMap,
        position: Position::InlineAnonymous,
        elem: "loc_nem_inl",
        class: "NonEmptyMapU64ToLocNemInl",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "companion: the loose map source MapU64ToLocNemInl is minted locally too",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::NamedTable,
        position: Position::NamedDeclaration,
        elem: "loc_tbl_dec",
        class: "MapLocTblDecToU64",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "companion: the synthesized keys()-list LocTblDecList is minted locally too",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::RejectSet,
        position: Position::InlineAnonymous,
        elem: "loc_rej_inl",
        class: "LocRejInlOrderedSet",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "the control for the position that CAN defer: an anonymous generic-set instance binds \
              to the structural wrapper, and its loose source LocRejInlList is minted locally too",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::RejectSet,
        position: Position::NamedDeclaration,
        elem: "loc_rej_dec",
        class: "LocRejDecOrderedSet",
        expect: Outcome::LocalSilent("a rule-declared wrapper is always the consumer's own class"),
        pinned_by: None,
        why: "ident == structural name is the coincidence the deferring modes decide on; here it is inert",
    },
    Row {
        mode: Mode::Local,
        shape: Shape::PreservePairMap,
        position: Position::NamedDeclaration,
        elem: "loc_pmp_dec",
        class: "PairMapU64ToLocPmpDec",
        expect: Outcome::LocalSilent("no dependency is configured"),
        pinned_by: None,
        why: "the flavor-encoding name (PairMap… not Map…) is what makes a cross-flavor index match unrepresentable",
    },
    // ===== IndexDeferred: the dep's index lists every structural name this spec derives =====
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::LooseList,
        position: Position::InlineAnonymous,
        elem: "idx_ll_inl",
        class: "IdxLlInlList",
        expect: Outcome::Defer,
        pinned_by: Some("extern_wrapper_index_defers_to_dep"),
        why: "the shipped R3b/R3e cell: import, no local class, absent from this crate's index",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::LooseList,
        position: Position::NamedDeclaration,
        elem: "idx_ll_dec",
        class: "IdxLlDecList",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "a rule whose ident coincides with an indexed structural name unifies with the dep's class",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::LooseList,
        position: Position::NamedReference,
        elem: "idx_ll_ref",
        class: "IdxLlRefList",
        expect: Outcome::Defer,
        pinned_by: Some("extern_wrapper_index_named_rule_reference_unifies_with_dep"),
        why: "the position that once exited 0 over a crate failing cargo check with E0425",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::LooseList,
        position: Position::NonRootScope,
        elem: "idx_ll_scp",
        class: "IdxLlScpList",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "the import must be routed into the NON-ROOT module holding the referencing struct",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::ExactList,
        position: Position::NonRootScope,
        elem: "idx_ex_scp",
        class: "IdxExScpListMin2Max2",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "a wholly deferred static carrier borrows the dependency's class and its Vec-to-array door, so it owes no local IdxExScpList source",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::LooseMap,
        position: Position::InlineAnonymous,
        elem: "idx_lm_inl",
        class: "MapU64ToIdxLmInl",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "map-side of the inline defer, with a primitive key so no keys-list companion exists",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::NonEmptyList,
        position: Position::InlineAnonymous,
        elem: "idx_nel_inl",
        class: "NonEmptyIdxNelInlList",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "companion: the loose source IdxNelInlList defers too, its import routed at the restricted class's scope",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::NonEmptyMap,
        position: Position::InlineAnonymous,
        elem: "idx_nem_inl",
        class: "NonEmptyMapU64ToIdxNemInl",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "map-side twin of the NonEmpty companion seam",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::NamedTable,
        position: Position::NamedDeclaration,
        elem: "idx_tbl_dec",
        class: "MapIdxTblDecToU64",
        expect: Outcome::LocalWarned("rule-declared table"),
        pinned_by: None,
        why: "a rule-declared loose table is screened by exists_in_rust and reaches no defer seam; \
              the keys()-list companion IdxTblDecList still defers",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::RejectSet,
        position: Position::InlineAnonymous,
        elem: "idx_rej_inl",
        class: "IdxRejInlOrderedSet",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "the uniqueness twin over an indexed dependency element defers like its loose and \
              NonEmpty siblings; borrowing the dep's class borrows its `try_from` door too, so no \
              loose source companion is owed here",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::RejectSet,
        position: Position::NamedDeclaration,
        elem: "idx_rej_dec",
        class: "IdxRejDecOrderedSet",
        expect: Outcome::Defer,
        pinned_by: None,
        why: "a reject rule whose ident coincides with an indexed structural name unifies with the \
              dep's class, exactly as the loose-list declaration row does",
    },
    Row {
        mode: Mode::IndexDeferred,
        shape: Shape::PreservePairMap,
        position: Position::NamedDeclaration,
        elem: "idx_pmp_dec",
        class: "PairMapU64ToIdxPmpDec",
        expect: Outcome::LocalWarned("rule-declared table"),
        pinned_by: None,
        why: "an ordinary shape row: the flavored name is listed, and a rule-declared preserve table \
              rides the SAME exists_in_rust screen and the same warning as its unflavored twin — \
              the flavor is in the name, not in the participation",
    },
    // ===== WorkspaceBorrowed: all-one-dep wrappers defer unconditionally, index or no index =====
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::LooseList,
        position: Position::InlineAnonymous,
        elem: "wsp_ll_inl",
        class: "WspLlInlList",
        expect: Outcome::Borrow,
        pinned_by: Some("workspace_dep_defers_to_dep"),
        why: "the shipped W1 cell: deferral plus the borrowed_collections.rs row",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::LooseList,
        position: Position::NamedDeclaration,
        elem: "wsp_ll_dec",
        class: "WspLlDecList",
        expect: Outcome::LocalWarned("shadows the collection wrapper"),
        pinned_by: None,
        why: "criterion 9: a rule-declared wrapper is the consumer's own class and NEVER borrows",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::LooseList,
        position: Position::NamedReference,
        elem: "wsp_ll_ref",
        class: "WspLlRefList",
        expect: Outcome::LocalWarned("shadows the collection wrapper"),
        pinned_by: None,
        why: "the reference position agrees with the declaration position — the index mode's \
              disagreement between the two is the escape this row would catch here",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::LooseList,
        position: Position::NonRootScope,
        elem: "wsp_ll_scp",
        class: "WspLlScpList",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "the borrow's import must be routed into the non-root module, and the sidecar row is \
              scope-independent",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::ExactList,
        position: Position::InlineAnonymous,
        elem: "wsp_ex_inl",
        class: "WspExInlListMin2Max2",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "the borrowed static carrier includes its Vec-to-array conversion door in the dependency, so no local WspExInlList companion is minted or requested",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::LooseMap,
        position: Position::InlineAnonymous,
        elem: "wsp_lm_inl",
        class: "MapU64ToWspLmInl",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "a primitive KEY does not make the wrapper ownerless: the value's owner decides",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::NonEmptyList,
        position: Position::InlineAnonymous,
        elem: "wsp_nel_inl",
        class: "NonEmptyWspNelInlList",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "companion: the loose source WspNelInlList borrows too",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::NonEmptyMap,
        position: Position::InlineAnonymous,
        elem: "wsp_nem_inl",
        class: "NonEmptyMapU64ToWspNemInl",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "map-side twin of the NonEmpty companion seam under workspace mode",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::NamedTable,
        position: Position::NamedDeclaration,
        elem: "wsp_tbl_dec",
        class: "MapWspTblDecToU64",
        expect: Outcome::LocalSilent(
            "a rule-declared table is the consumer's own class and reaches no defer seam; \
             criterion 9's warning belongs to the seam it never reaches",
        ),
        pinned_by: Some("workspace_dep_named_table_deferred_keys_list"),
        why: "the arming instance: the table is local while its synthesized keys-list BORROWS, and \
              only the inline-map reference position used to register that import",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::RejectSet,
        position: Position::InlineAnonymous,
        elem: "wsp_rej_inl",
        class: "WspRejInlOrderedSet",
        expect: Outcome::Borrow,
        pinned_by: None,
        why: "the cell the dep-side hosting leg had no consumer for: the borrow's sidecar row carries \
              the `@duplicates reject` marker, which is what makes the host rebuild the uniqueness \
              twin instead of a loose list",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::RejectSet,
        position: Position::NamedDeclaration,
        elem: "wsp_rej_dec",
        class: "WspRejDecOrderedSet",
        expect: Outcome::LocalWarned("shadows the collection wrapper"),
        pinned_by: None,
        why: "criterion 9 reaches the reject seam too: a rule-declared set is the consumer's own \
              class and NEVER borrows, however unconditional the mode is otherwise",
    },
    Row {
        mode: Mode::WorkspaceBorrowed,
        shape: Shape::PreservePairMap,
        position: Position::NamedDeclaration,
        elem: "wsp_pmp_dec",
        class: "PairMapU64ToWspPmpDec",
        expect: Outcome::LocalSilent(
            "a rule-declared preserve table keeps the consumer's class, like its loose twin",
        ),
        pinned_by: None,
        why: "the flavored name rides the same rule-declared screen as the unflavored one",
    },
    // ===== RequestedHosted: dep side. A request is (name, shape) — POSITION does not survive it =====
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::LooseList,
        position: Position::NotApplicable,
        elem: "req_ll",
        class: "ReqLlList",
        expect: Outcome::Host,
        pinned_by: Some("workspace_requests_hosts_borrowed_wrappers"),
        why: "the shipped W2 cell: (union − own-produced) hosted into requested_collections.rs",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::ExactList,
        position: Position::NotApplicable,
        elem: "req_ex",
        class: "ReqExListMin2Max2",
        expect: Outcome::Host,
        pinned_by: None,
        why: "the host rebuilds the native static carrier and co-hosts ReqExList as its loose Vec-to-array handover source",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::LooseMap,
        position: Position::NotApplicable,
        elem: "req_lm",
        class: "MapU64ToReqLm",
        expect: Outcome::Host,
        pinned_by: None,
        why: "the map-side host, which no committed request cell spells",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::NonEmptyList,
        position: Position::NotApplicable,
        elem: "req_nel",
        class: "NonEmptyReqNelList",
        expect: Outcome::Host,
        pinned_by: None,
        why: "companion: hosting a restricted wrapper obliges its loose source in the same crate",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::NonEmptyMap,
        position: Position::NotApplicable,
        elem: "req_nem",
        class: "NonEmptyMapU64ToReqNem",
        expect: Outcome::Host,
        pinned_by: None,
        why: "map-side twin of the restricted-host companion seam",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::RejectSet,
        position: Position::NotApplicable,
        elem: "idx_foo",
        class: "IdxFooOrderedSet",
        expect: Outcome::Host,
        pinned_by: Some("workspace_requests_hosts_reject_ordered_set_twins"),
        why: "the hosting half of the reject seam: the same shape column a consumer's borrow writes \
              is what the dep rebuilds the uniqueness twin from",
    },
    Row {
        mode: Mode::RequestedHosted,
        shape: Shape::PreservePairMap,
        position: Position::NotApplicable,
        elem: "idx_foo",
        class: "PairMapU64ToIdxFoo",
        expect: Outcome::Host,
        pinned_by: Some("workspace_requests_hosts_preserve_pair_map_twins"),
        why: "the flavor must survive the sidecar's shape column, or the host rebuilds the wrong container",
    },
];

// ---------------------------------------------------------------------------------------------
// Spec construction — each row's CDDL is DERIVED from its axes, so a new row is one table entry
// ---------------------------------------------------------------------------------------------

impl Position {
    /// Whether this position puts a REFERENCE to the wrapper in some emitted struct — i.e. whether
    /// an import is owed at all. A bare DECLARATION nothing references owes none: the class is
    /// suppressed, the rust-side alias is kept, and there is no use site to route to. Encoding that
    /// distinction is what keeps "no import was routed" from reading as a defect in the one position
    /// where it is the correct answer.
    fn references(self) -> bool {
        match self {
            Position::InlineAnonymous | Position::NamedReference | Position::NonRootScope => true,
            Position::NamedDeclaration | Position::NotApplicable => false,
        }
    }
}

impl Shape {
    /// The CDDL fragment for this shape over `elem`, without any rule head.
    fn cddl(self, elem: &str) -> String {
        match self {
            Shape::LooseList => format!("[* {elem}]"),
            Shape::ExactList => format!("[2*2 {elem}]"),
            Shape::LooseMap => format!("{{* uint => {elem}}}"),
            Shape::NonEmptyList => format!("[+ {elem}]"),
            Shape::NonEmptyMap => format!("{{+ uint => {elem}}}"),
            // Keyed BY the element so the synthesized keys()-list companion exists — the companion
            // whose deferred import only one reference position used to register.
            Shape::NamedTable => format!("{{* {elem} => uint}}"),
            Shape::RejectSet => format!("[* {elem}]"),
            Shape::PreservePairMap => format!("{{* uint => {elem}}}"),
        }
    }

    /// The `@duplicates` directive this shape carries, if any.
    fn policy(self) -> Option<&'static str> {
        match self {
            Shape::RejectSet => Some("@duplicates reject"),
            Shape::PreservePairMap => Some("@duplicates preserve"),
            _ => None,
        }
    }

    /// The rule ident that makes a DECLARATION coincide with the structural name — the coincidence
    /// the deferring modes decide on. Derived from the class name the row records, so the table's
    /// `class` column and the emitted spec cannot disagree.
    fn declared_rule_ident(self, class: &str) -> String {
        crate::utils::convert_to_snake_case(class)
    }
}

/// One mode's generated spec: root rules, an optional non-root file, and the extern-dep stub.
struct SpecFiles {
    /// (relative path, contents)
    files: Vec<(String, String)>,
}

/// The generic set def an inline flavored (reject) occurrence is instantiated from. Its own ident
/// never becomes a class: an anonymous instance binds to the STRUCTURAL `<Elem>OrderedSet`.
const REJECT_GENERIC: &str = "wp_oset";

fn build_spec(mode: Mode, rows: &[&Row]) -> SpecFiles {
    let mut root = String::new();
    let mut sub = String::new();
    let mut extern_stub = String::new();
    let mut holder_members: Vec<String> = vec![];
    let mut sub_members: Vec<String> = vec![];
    let mut needs_reject_generic = false;

    for row in rows {
        // The element declaration: consumer-owned in Local mode, a dep extern otherwise.
        if mode == Mode::Local {
            root.push_str(&format!("{} = [x: uint]\n\n", row.elem));
        } else {
            extern_stub.push_str(&format!("{} = _CDDL_CODEGEN_EXTERN_TYPE_\n", row.elem));
        }
        let shape = row.shape.cddl(row.elem);
        let policy = row.shape.policy();
        match row.position {
            Position::InlineAnonymous => match policy {
                None => holder_members.push(format!("  {}_m: {shape}", row.elem)),
                // A flavored shape cannot ride a bare member line — the directive lives in a rule
                // COMMENT, which would swallow the rest of the line. The spelling a real spec uses
                // for an inline flavored occurrence is a GENERIC def carrying the directive,
                // instantiated ANONYMOUSLY at the use site (no named alias rule in between), so the
                // member binds to the STRUCTURAL wrapper rather than to a rule's own class. Only the
                // reject set reaches this arm; a preserve table's inline occurrence carries no
                // directive of its own (the policy is per-rule), so the table keeps it declared.
                Some(_) => {
                    assert_eq!(
                        row.shape,
                        Shape::RejectSet,
                        "only the reject set has an inline flavored spelling; {} must use \
                         NamedDeclaration",
                        row.elem
                    );
                    needs_reject_generic = true;
                    holder_members
                        .push(format!("  {}_m: {REJECT_GENERIC}<{}>", row.elem, row.elem));
                }
            },
            Position::NamedDeclaration | Position::NamedReference => {
                let ident = row.shape.declared_rule_ident(row.class);
                let comment = policy.map(|p| format!(" ; {p}")).unwrap_or_default();
                root.push_str(&format!("{ident} = {shape}{comment}\n\n"));
                if row.position == Position::NamedReference {
                    holder_members.push(format!("  {}_m: {ident}", row.elem));
                }
            }
            Position::NonRootScope => {
                assert!(
                    policy.is_none(),
                    "flavored shapes stay at root in this grid"
                );
                // The exact-list row needs the named-rule route as well as a non-root use: its
                // class's `try_from(&<Elem>List)` source is emitted at the rule's scope, while the
                // loose source itself remains synthesized at root. Other shapes keep the compact
                // inline position this axis conventionally represents.
                if row.shape == Shape::ExactList {
                    let ident = row.shape.declared_rule_ident(row.class);
                    sub.push_str(&format!("{ident} = {shape}\n\n"));
                    sub_members.push(format!("  {}_m: {ident}", row.elem));
                } else {
                    sub_members.push(format!("  {}_m: {shape}", row.elem));
                }
            }
            Position::NotApplicable => unreachable!("request rows build no consumer spec"),
        }
    }
    if needs_reject_generic {
        root.push_str(&format!(
            "{REJECT_GENERIC}<a0> = [* a0] ; @duplicates reject\n\n"
        ));
    }
    if !holder_members.is_empty() {
        root.push_str(&format!("holder = [\n{}\n]\n", holder_members.join(",\n")));
    }
    if !sub_members.is_empty() {
        sub.push_str(&format!("sub_holder = [\n{}\n]\n", sub_members.join(",\n")));
    }

    let mut files = vec![("lib.cddl".to_owned(), root)];
    if !sub.is_empty() {
        files.push(("sub.cddl".to_owned(), sub));
    }
    if !extern_stub.is_empty() {
        files.push((
            format!("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/{DEP}/mod.cddl"),
            extern_stub,
        ));
    }
    SpecFiles { files }
}

/// The scratch root for one mode's batch. Per-checkout so two checkouts never contend, per-mode so
/// the four batches are independent.
fn scratch_root(tag: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "cddl_codegen_wrapper_participation_{tag}_{:016x}",
        checkout_hash()
    ))
}

fn write_files(root: &Path, files: &[(String, String)]) {
    for (rel, body) in files {
        let path = root.join(rel);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, body).unwrap();
    }
}

/// Everything one mode's batch produces: the generated wasm sources (joined), the crate's own
/// collection index, the workspace sidecar when the mode emits one, and the run's stderr.
struct Generated {
    #[allow(dead_code)]
    export: PathBuf,
    wasm_src: String,
    rust_src: String,
    own_index: String,
    sidecar: String,
    stderr: String,
}

impl Generated {
    fn mentions_class_import(&self, class: &str) -> bool {
        self.wasm_src
            .match_indices(&format!("use {DEP_WASM}::collections::"))
            .any(|(start, _)| {
                let rest = &self.wasm_src[start..];
                let stmt = &rest[..=rest.find(';').expect("unterminated use statement")];
                stmt.contains(class)
            })
    }
}

/// Generate one consumer-side mode batch (`Local` / `IndexDeferred` / `WorkspaceBorrowed`).
fn generate_consumer_mode(prefix: &str, mode: Mode, rows: &[&Row]) -> Generated {
    let tag = match mode {
        Mode::Local => "local",
        Mode::IndexDeferred => "index",
        Mode::WorkspaceBorrowed => "workspace",
        Mode::RequestedHosted => unreachable!("the request mode has its own driver"),
    };
    let root = scratch_root(&format!("{prefix}_{tag}"));
    let _ = std::fs::remove_dir_all(&root);
    let input = root.join("inputs");
    let export = root.join("export");
    write_files(&input, &build_spec(mode, rows).files);

    let mut cmd = codegen_cmd();
    cmd.arg(format!("--input={}", input.display()))
        .arg(format!("--output={}", export.display()))
        .arg("--wasm=true");
    if mode != Mode::Local {
        cmd.arg(format!("--common-import-override={DEP}"))
            .arg(format!("--extern-wasm-crate={DEP}={DEP_WASM}"));
    }
    match mode {
        Mode::IndexDeferred => {
            // The dep's index, SYNTHESIZED: it lists exactly the structural name of every row in the
            // batch, which is the "the dependency owns them all" configuration. What the grid then
            // records is which shapes actually consult it.
            let index = root.join("dep_collections.rs");
            let mut body = String::new();
            for row in rows {
                body.push_str(&format!("pub use crate::generated::{};\n", row.class));
                // A restricted wrapper's loose `try_from` SOURCE is a separate defer candidate, and
                // the companion this grid asserts, so the dep lists it too.
                if let Some(source) = loose_source_class(row) {
                    body.push_str(&format!("pub use crate::generated::{source};\n"));
                }
                if let Some(keys) = keys_list_class(row) {
                    body.push_str(&format!("pub use crate::generated::{keys};\n"));
                }
            }
            std::fs::write(&index, body).unwrap();
            cmd.arg(format!("--extern-wrapper-index={DEP}={}", index.display()));
        }
        Mode::WorkspaceBorrowed => {
            cmd.arg(format!("--workspace-dep={DEP}"));
        }
        _ => {}
    }
    let out = cmd.output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "{tag} batch generation must succeed; stderr:\n{stderr}"
    );

    let read = |rel: &str| std::fs::read_to_string(export.join(rel)).unwrap_or_default();
    let mut wasm_src = read("wasm/src/generated/mod.rs");
    wasm_src.push('\n');
    wasm_src.push_str(&read("wasm/src/generated/sub.rs"));
    wasm_src.push('\n');
    wasm_src.push_str(&read("wasm/src/generated/sub/mod.rs"));
    let mut rust_src = read("rust/src/generated/mod.rs");
    rust_src.push('\n');
    rust_src.push_str(&read("rust/src/generated/sub.rs"));
    rust_src.push('\n');
    rust_src.push_str(&read("rust/src/generated/sub/mod.rs"));
    Generated {
        wasm_src,
        rust_src,
        own_index: read("wasm/src/generated/collections.rs"),
        sidecar: read("wasm/src/generated/borrowed_collections.rs"),
        stderr,
        export,
    }
}

/// The loose `try_from` SOURCE a restricted wrapper implies, if this row has one.
fn loose_source_class(row: &Row) -> Option<String> {
    match row.shape {
        // A locally minted/hosted exact wrapper still crosses its one checked Vec-to-array door
        // through the loose element list. A wholly deferred/borrowed wrapper owns that door in the
        // dependency, so this crate owes no companion class or import.
        Shape::ExactList if matches!(row.expect, Outcome::Defer | Outcome::Borrow) => None,
        Shape::ExactList => Some(format!(
            "{}List",
            crate::utils::convert_to_camel_case(row.elem)
        )),
        Shape::NonEmptyList => Some(row.class.trim_start_matches("NonEmpty").to_owned()),
        Shape::NonEmptyMap => Some(row.class.trim_start_matches("NonEmpty").to_owned()),
        // A DEFERRED reject wrapper borrows the dependency's whole class — its `try_from` door
        // included — so it names no loose source in this crate at all and owes no companion. Only a
        // locally-minted one does.
        Shape::RejectSet if matches!(row.expect, Outcome::Defer | Outcome::Borrow) => None,
        Shape::RejectSet => Some(format!("{}List", row.class.trim_end_matches("OrderedSet"))),
        _ => None,
    }
}

/// The synthesized `keys()`-list companion a table keyed by a named type implies.
fn keys_list_class(row: &Row) -> Option<String> {
    match row.shape {
        Shape::NamedTable => Some(format!(
            "{}List",
            crate::utils::convert_to_camel_case(row.elem)
        )),
        _ => None,
    }
}

// ---------------------------------------------------------------------------------------------
// The always-on sweeps
// ---------------------------------------------------------------------------------------------

fn rows_for(mode: Mode) -> Vec<&'static Row> {
    PARTICIPATION_TABLE
        .iter()
        .filter(|r| r.mode == mode && r.pinned_by.is_none())
        .collect()
}

/// Assert one row against the batch its mode generated.
fn assert_row(row: &Row, batch: &Generated) {
    let class = row.class;
    let minted = format!("pub struct {class}(");
    match row.expect {
        Outcome::Defer | Outcome::Borrow => {
            assert!(
                !batch.wasm_src.contains(&minted),
                "{class} ({:?}/{:?}/{:?}) must NOT be minted locally — {}",
                row.mode,
                row.shape,
                row.position,
                row.why
            );
            if row.position.references() {
                assert!(
                    batch.mentions_class_import(class),
                    "{class} ({:?}/{:?}/{:?}) must be imported from the dependency's collections \
                     module — {}\n{}",
                    row.mode,
                    row.shape,
                    row.position,
                    row.why,
                    batch.wasm_src
                );
            } else {
                // A bare declaration nothing references owes no import — but the RULE must still be
                // honored on the rust side, which is the half a deferral never takes away.
                assert!(
                    batch.rust_src.contains(&format!("pub type {class} =")),
                    "a deferred rule-declared wrapper keeps its rust-side alias: {class}\n{}",
                    batch.rust_src
                );
            }
            assert!(
                !batch.own_index.contains(&format!("::{class};")),
                "a deferred wrapper leaves this crate's own collections.rs index: {class}\n{}",
                batch.own_index
            );
            if row.expect == Outcome::Borrow {
                assert!(
                    batch.sidecar.contains(&format!("\"{class}\"")),
                    "a workspace borrow must be recorded in borrowed_collections.rs: {class}\n{}",
                    batch.sidecar
                );
            }
        }
        Outcome::LocalWarned(fragment) => {
            assert!(
                batch.wasm_src.contains(&minted),
                "{class} ({:?}/{:?}/{:?}) must be minted locally — {}",
                row.mode,
                row.shape,
                row.position,
                row.why
            );
            assert!(
                batch.own_index.contains(&format!("::{class};")),
                "a locally-minted wrapper stays in this crate's own index: {class}\n{}",
                batch.own_index
            );
            let warned = batch
                .stderr
                .lines()
                .any(|l| l.contains(class) && l.contains(fragment));
            assert!(
                warned,
                "{class} ({:?}/{:?}) must warn with {fragment:?} — {}\nstderr:\n{}",
                row.mode, row.shape, row.why, batch.stderr
            );
        }
        Outcome::LocalSilent(why_silent) => {
            assert!(
                batch.wasm_src.contains(&minted),
                "{class} ({:?}/{:?}/{:?}) must be minted locally — {}",
                row.mode,
                row.shape,
                row.position,
                row.why
            );
            assert!(
                batch.own_index.contains(&format!("::{class};")),
                "a locally-minted wrapper stays in this crate's own index: {class}\n{}",
                batch.own_index
            );
            assert!(
                !batch.stderr.contains(class),
                "{class} ({:?}/{:?}) must mint SILENTLY ({why_silent}) — stderr names it:\n{}",
                row.mode,
                row.shape,
                batch.stderr
            );
        }
        Outcome::Host => unreachable!("host rows are asserted by the request driver"),
    }
}

/// MODE = `Local`: the control column. Nothing can defer, borrow or host, and nothing may warn —
/// a warning here would be the generator inventing a dependency the run never named.
#[test]
fn wrapper_participation_local_mode() {
    let rows = rows_for(Mode::Local);
    let batch = generate_consumer_mode("sweep", Mode::Local, &rows);
    assert!(
        batch.stderr.is_empty(),
        "the control column must generate with EMPTY stderr:\n{}",
        batch.stderr
    );
    assert!(
        batch.sidecar.is_empty(),
        "no dep flag ⇒ no borrowed_collections.rs at all"
    );
    for row in &rows {
        assert_row(row, &batch);
    }
    let exact_non_root = rows
        .iter()
        .find(|row| row.shape == Shape::ExactList && row.position == Position::NonRootScope)
        .expect("the local exact non-root source-control row");
    let source =
        loose_source_class(exact_non_root).expect("locally minted exact wrapper owes source");
    assert!(
        batch.wasm_src.contains(&format!("pub struct {source}(")),
        "the local exact wrapper must mint its loose Vec-to-array source {source}:\n{}",
        batch.wasm_src
    );
    assert!(
        batch
            .wasm_src
            .lines()
            .any(|line| { line.starts_with("use crate::generated::") && line.contains(&source) }),
        "the non-root exact wrapper's bare try_from(&{source}) source must import from root:\n{}",
        batch.wasm_src
    );
}

/// Two named non-root restricted rules reach the `RustStructType::Array` source-registration arm
/// directly, rather than incidentally through an inline occurrence. The exact outer names the root
/// `FooList` builder; the NonEmpty outer over an inline variable-bounded element names the root
/// `FooListMin2Max3List` builder. The latter is the subtle corrected case: only a nested NonEmpty
/// element suppresses a loose source, never a nested bounded/static one.
#[test]
fn named_restricted_rules_import_nonroot_loose_sources() {
    let root = scratch_root("named_restricted_nonroot_sources");
    let _ = std::fs::remove_dir_all(&root);
    let input = root.join("inputs");
    let export = root.join("export");
    write_files(
        &input,
        &[
            ("lib.cddl".to_owned(), "foo = [x: uint]\n".to_owned()),
            (
                "sub.cddl".to_owned(),
                "exact = [2*2 foo]\n\
                 outer = [+ [2*3 foo]]\n\
                 holder = [field: exact, bounded: outer]\n"
                    .to_owned(),
            ),
        ],
    );
    let out = codegen_cmd()
        .arg(format!("--input={}", input.display()))
        .arg(format!("--output={}", export.display()))
        .arg("--wasm=true")
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "named restricted non-root source resident must generate: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let sub = std::fs::read_to_string(export.join("wasm/src/generated/sub/mod.rs")).unwrap();
    assert!(
        sub.contains("pub struct Exact(")
            && sub
                .lines()
                .any(|line| line.starts_with("use crate::generated::") && line.contains("FooList"))
            && sub.contains("list: &FooList"),
        "the named exact class must import its root loose Vec-to-array source in its declaring scope:\n{sub}"
    );
    assert!(
        sub.contains("pub struct Outer(")
            && sub.contains("try_from(list: &FooListMin2Max3List)")
            && sub.lines().any(|line| {
                line.starts_with("use crate::generated::") && line.contains("FooListMin2Max3List")
            }),
        "the named NonEmpty outer over an inline bounded element must import its root loose source; \
         nested bounded/static elements never take the nested-NonEmpty suppression:\n{sub}"
    );
    let _ = std::fs::remove_dir_all(&root);
}

/// MODE = `IndexDeferred`: the dep's index lists every structural name the spec derives, so what the
/// grid records is which shapes CONSULT it — and, for the ones that cannot, that the collision is
/// announced rather than silent.
#[test]
fn wrapper_participation_index_mode() {
    let rows = rows_for(Mode::IndexDeferred);
    let batch = generate_consumer_mode("sweep", Mode::IndexDeferred, &rows);
    for row in &rows {
        assert_row(row, &batch);
    }
    // COMPANIONS, asserted per row rather than assumed: a restricted wrapper's loose `try_from`
    // source is its own defer candidate, and a table's synthesized keys()-list is another.
    for row in &rows {
        if let Some(source) = loose_source_class(row) {
            assert!(
                !batch.wasm_src.contains(&format!("pub struct {source}(")),
                "the loose try_from source {source} of {} must defer too",
                row.class
            );
            assert!(
                batch.mentions_class_import(&source),
                "the deferred source {source} must have its import routed at {}'s scope:\n{}",
                row.class,
                batch.wasm_src
            );
        }
        if let Some(keys) = keys_list_class(row) {
            assert!(
                !batch.wasm_src.contains(&format!("pub struct {keys}(")),
                "a table's synthesized keys()-list {keys} defers like any structural wrapper"
            );
            assert!(
                batch.mentions_class_import(&keys),
                "the deferred keys()-list {keys} must have its import routed into the module \
                 holding the table's class:\n{}",
                batch.wasm_src
            );
        }
    }
}

/// MODE = `WorkspaceBorrowed`: an all-one-dep wrapper defers UNCONDITIONALLY (no index consulted),
/// and every borrow is recorded in the sidecar the dependency will machine-read.
#[test]
fn wrapper_participation_workspace_mode() {
    let rows = rows_for(Mode::WorkspaceBorrowed);
    let batch = generate_consumer_mode("sweep", Mode::WorkspaceBorrowed, &rows);
    assert!(
        !batch.sidecar.is_empty(),
        "the flag is present, so the sidecar is emitted (empty-but-present is still present)"
    );
    for row in &rows {
        assert_row(row, &batch);
    }
    for row in &rows {
        if let Some(source) = loose_source_class(row)
            && row.expect == Outcome::Borrow
        {
            assert!(
                !batch.wasm_src.contains(&format!("pub struct {source}(")),
                "the loose source {source} of a borrowed {} borrows too",
                row.class
            );
        }
    }
}

/// The `RequestedHosted` fixture: the dependency's own spec (each requested element type, and
/// nothing that would PRODUCE the requested shapes, so every row is genuinely "requested but not
/// own-produced") plus a consumer's `borrowed_collections.rs` synthesized in the committed format —
/// including the flavored shapes' bare policy marker in the shape column, which is the carrier the
/// dep re-parses. Returns `(scratch root, sidecar path)`; `<root>/lib.cddl` is the dep spec.
fn write_request_fixture(prefix: &str, rows: &[&Row]) -> (PathBuf, PathBuf) {
    let root = scratch_root(&format!("{prefix}_requests"));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();

    let mut dep_spec = String::new();
    for row in rows {
        dep_spec.push_str(&format!("{} = [x: uint]\n", row.elem));
    }
    std::fs::write(root.join("lib.cddl"), dep_spec).unwrap();

    let mut sidecar = String::from("#[allow(unused_imports)]\nmod borrowed {\n");
    for row in rows {
        sidecar.push_str(&format!(
            "    use wp_dep_wasm::collections::{};\n",
            row.class
        ));
    }
    sidecar.push_str(
        "}\n#[allow(dead_code)]\npub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n",
    );
    for row in rows {
        let shape = row.shape.cddl(row.elem);
        let marker = row
            .shape
            .policy()
            .map(|p| format!(" {p}"))
            .unwrap_or_default();
        sidecar.push_str(&format!(
            "    (\"wp_dep\", \"{}\", \"{shape}{marker}\"),\n",
            row.class
        ));
    }
    sidecar.push_str("];\n");
    let sidecar_path = root.join("consumer_borrowed_collections.rs");
    std::fs::write(&sidecar_path, sidecar).unwrap();
    (root, sidecar_path)
}

/// MODE = `RequestedHosted`, driven from the DEPENDENCY side. A request carries (name, shape) only,
/// so the position axis does not survive it — the dep rebuilds the wrapper from the shape column
/// alone, which is why the flavored shapes have their own committed cells.
#[test]
fn wrapper_participation_requested_mode() {
    use clap::Parser;
    let rows: Vec<&Row> = PARTICIPATION_TABLE
        .iter()
        .filter(|r| r.mode == Mode::RequestedHosted && r.pinned_by.is_none())
        .collect();
    let (root, sidecar_path) = write_request_fixture("sweep", &rows);

    for (profile, extra) in [
        ("default", &[][..]),
        ("preserve", &["--preserve-encodings=true"][..]),
        (
            "json",
            &["--json-serde-derives=true", "--json-schema-export=true"][..],
        ),
    ] {
        let mut args = vec![
            "cddl-codegen".to_owned(),
            "--input".to_owned(),
            root.join("lib.cddl").display().to_string(),
            "--output".to_owned(),
            "wrapper_participation_unused".to_owned(),
            "--lib-name".to_owned(),
            "wp-dep".to_owned(),
            "--wasm=true".to_owned(),
            format!("--wrapper-requests=wpc={}", sidecar_path.display()),
        ];
        args.extend(extra.iter().map(|flag| (*flag).to_owned()));
        let cli = crate::cli::Cli::parse_from(args);
        let files = crate::api::generated_strings(&cli)
            .unwrap_or_else(|e| panic!("the {profile} dep must host every requested wrapper: {e}"));
        let hosted = files
            .get("wasm/src/generated/requested_collections.rs")
            .expect("a dep with requests emits requested_collections.rs");
        let index = files
            .get("wasm/src/generated/collections.rs")
            .expect("wasm collections index");
        for row in &rows {
            assert!(
                hosted.contains(&format!("pub struct {}(", row.class)),
                "{profile}: {} ({:?}) must be HOSTED — {}\n{hosted}",
                row.class,
                row.shape,
                row.why
            );
            assert!(
                index.contains(&format!(
                    "pub use crate::generated::requested_collections::{};",
                    row.class
                )),
                "{profile}: a hosted wrapper is re-exported from the dep's own index: {}",
                row.class
            );
            // The restricted rows' loose source is a recursive support mint, not an explicit request.
            // It shares requested_collections.rs, so it must be defined and named locally — never
            // imported from the generated root where it does not exist.
            if let Some(source) = loose_source_class(row) {
                assert!(
                    hosted.contains(&format!("pub struct {source}(")),
                    "{profile}: the loose source {source} of hosted {} must be co-hosted:\n{hosted}",
                    row.class
                );
                assert!(
                    !hosted.lines().any(|line| {
                        line.contains("use crate::generated::") && line.contains(&source)
                    }),
                    "{profile}: the co-hosted loose source {source} of {} must not self-import:\n{hosted}",
                    row.class
                );
            }
        }
    }
    let _ = std::fs::remove_dir_all(&root);
}

/// A requested restricted wrapper still needs a REAL root-owned loose source when the dependency's
/// own spec emits that source. The actual-hosted set is precise: it suppresses only same-file imports.
#[test]
fn wrapper_participation_requested_non_empty_root_sources_stay_imported() {
    use clap::Parser;
    let root = scratch_root("requested_root_sources");
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(
        root.join("lib.cddl"),
        "req_nel = [x: uint]\nreq_nem = [x: uint]\nroot = [list: [* req_nel], map: {* uint => req_nem}]\n",
    )
    .unwrap();
    let sidecar = root.join("borrowed.rs");
    std::fs::write(
        &sidecar,
        "#[allow(dead_code)]\npub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n\
         (\"wp_dep\", \"NonEmptyReqNelList\", \"[+ req_nel]\"),\n\
         (\"wp_dep\", \"NonEmptyMapU64ToReqNem\", \"{+ uint => req_nem}\"),\n];\n",
    )
    .unwrap();
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        root.display().to_string(),
        "--output".to_owned(),
        "wrapper_participation_unused".to_owned(),
        "--lib-name".to_owned(),
        "wp-dep".to_owned(),
        "--wasm=true".to_owned(),
        format!("--wrapper-requests=wpc={}", sidecar.display()),
    ]);
    let files = crate::api::generated_strings(&cli).unwrap();
    let _ = std::fs::remove_dir_all(&root);
    let requested = &files["wasm/src/generated/requested_collections.rs"];
    for source in ["ReqNelList", "MapU64ToReqNem"] {
        assert!(
            !requested.contains(&format!("pub struct {source}(")),
            "the own-spec source stays at the generated root, not requested_collections:\n{requested}"
        );
        assert!(
            requested
                .lines()
                .any(|line| { line.contains("use crate::generated::") && line.contains(source) }),
            "the root-owned loose source {source} remains a real requested-scope import:\n{requested}"
        );
    }
}

/// `--workspace-dep` validation and wasm mapping do not change requested-host ownership: its host
/// still defines a recursive loose source locally while retaining a genuine mapped extern import.
#[test]
fn wrapper_participation_requested_workspace_dep_keeps_local_and_extern_homes() {
    use clap::Parser;
    let root = scratch_root("requested_workspace_dep");
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("_CDDL_CODEGEN_EXTERN_DEPS_DIR_").join("other")).unwrap();
    std::fs::write(
        root.join("lib.cddl"),
        "own_ty = [x: uint]\nroot = [local: own_ty, external: other_ty]\n",
    )
    .unwrap();
    std::fs::write(
        root.join("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/other/mod.cddl"),
        "other_ty = _CDDL_CODEGEN_EXTERN_TYPE_\n",
    )
    .unwrap();
    let sidecar = root.join("borrowed.rs");
    std::fs::write(
        &sidecar,
        "#[allow(dead_code)]\npub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n\
         (\"wp_dep\", \"NonEmptyOwnTyList\", \"[+ own_ty]\"),\n];\n",
    )
    .unwrap();
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        root.display().to_string(),
        "--output".to_owned(),
        "wrapper_participation_unused".to_owned(),
        "--lib-name".to_owned(),
        "wp-dep".to_owned(),
        "--wasm=true".to_owned(),
        "--workspace-dep=other".to_owned(),
        "--extern-wasm-crate=other=other_wasm".to_owned(),
        format!("--wrapper-requests=wpc={}", sidecar.display()),
    ]);
    let files = crate::api::generated_strings(&cli).unwrap();
    let _ = std::fs::remove_dir_all(&root);
    let requested = &files["wasm/src/generated/requested_collections.rs"];
    assert!(requested.contains("pub struct OwnTyList("));
    assert!(
        !requested
            .lines()
            .any(|line| line.contains("use crate::generated::") && line.contains("OwnTyList")),
        "the recursive loose source is co-hosted even with --workspace-dep:\n{requested}"
    );
    let root_wasm = &files["wasm/src/generated/mod.rs"];
    assert!(
        root_wasm
            .lines()
            .any(|line| line.contains("use other_wasm::") && line.contains("OtherTy")),
        "the valid workspace-dep mapping's genuine extern import must survive:\n{root_wasm}"
    );
}

/// A requested restricted map can be hosted while its primitive-only loose source is indexed by a
/// different dependency. Unlike a co-hosted source, that class has no local body and must retain the
/// dependency collections import at the requested scope.
#[test]
fn wrapper_participation_requested_non_empty_map_source_can_defer() {
    use clap::Parser;
    let root = scratch_root("requested_deferred_map_source");
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("_CDDL_CODEGEN_EXTERN_DEPS_DIR_").join("other")).unwrap();
    std::fs::write(root.join("lib.cddl"), "root = [x: uint]\n").unwrap();
    std::fs::write(
        root.join("_CDDL_CODEGEN_EXTERN_DEPS_DIR_/other/mod.cddl"),
        "unused = _CDDL_CODEGEN_EXTERN_TYPE_\n",
    )
    .unwrap();
    let index = root.join("other_collections.rs");
    std::fs::write(&index, "pub use crate::generated::MapU64ToU64;\n").unwrap();
    let sidecar = root.join("borrowed.rs");
    std::fs::write(
        &sidecar,
        "#[allow(dead_code)]\npub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n\
         (\"wp_dep\", \"NonEmptyMapU64ToU64\", \"{+ uint => uint}\"),\n];\n",
    )
    .unwrap();
    let cli = crate::cli::Cli::parse_from([
        "cddl-codegen".to_owned(),
        "--input".to_owned(),
        root.display().to_string(),
        "--output".to_owned(),
        "wrapper_participation_unused".to_owned(),
        "--lib-name".to_owned(),
        "wp-dep".to_owned(),
        "--wasm=true".to_owned(),
        "--extern-wasm-crate=other=other_wasm".to_owned(),
        format!("--extern-wrapper-index=other={}", index.display()),
        format!("--wrapper-requests=wpc={}", sidecar.display()),
    ]);
    let files = crate::api::generated_strings(&cli).unwrap();
    let _ = std::fs::remove_dir_all(&root);
    let requested = &files["wasm/src/generated/requested_collections.rs"];
    assert!(requested.contains("pub struct NonEmptyMapU64ToU64("));
    assert!(
        !requested.contains("pub struct MapU64ToU64("),
        "the indexed loose source must not be minted in requested_collections:\n{requested}"
    );
    let root_wasm = &files["wasm/src/generated/mod.rs"];
    assert!(
        root_wasm.lines().any(|line| {
            line.contains("use other_wasm::collections::") && line.contains("MapU64ToU64")
        }),
        "the deferred loose source must retain its dependency collections import (which the requested \
         module reaches through `use super::*;`):\n{root_wasm}"
    );
}

// ---------------------------------------------------------------------------------------------
// The table's own liveness — the guard that keeps a REFERENCED row honest
// ---------------------------------------------------------------------------------------------

/// The grid's self-checks. A `pinned_by` row builds nothing, so its only cost is a claim — and a
/// claim naming a test that was renamed or deleted is a hole in the grid that reads as coverage.
/// Cheap (pure file reads), so it is always-on.
#[test]
fn wrapper_participation_table_is_complete_and_live() {
    // 1. Rows are unique on their full key, and every row owns a distinct class per mode: two rows
    //    sharing a class in one mode would assert twice about one emission and hide a gap.
    let mut keys: BTreeSet<(Mode, Shape, Position, &str)> = BTreeSet::new();
    for row in PARTICIPATION_TABLE {
        assert!(
            keys.insert((row.mode, row.shape, row.position, row.class)),
            "duplicate participation row: {:?}/{:?}/{:?}/{}",
            row.mode,
            row.shape,
            row.position,
            row.class
        );
    }
    let mut per_mode: BTreeSet<(Mode, &str)> = BTreeSet::new();
    for row in PARTICIPATION_TABLE {
        assert!(
            per_mode.insert((row.mode, row.class)),
            "two rows of {:?} contend for the class {}",
            row.mode,
            row.class
        );
    }

    // 2. Every MODE covers every SHAPE that participates in it. `RequestedHosted` is the one mode
    //    with a documented shape gap — a named TABLE rule is a rule, not a wrapper request, so no
    //    sidecar can ask for one — and it is spelled here rather than left as a silent absence.
    for mode in [
        Mode::Local,
        Mode::IndexDeferred,
        Mode::WorkspaceBorrowed,
        Mode::RequestedHosted,
    ] {
        for shape in [
            Shape::LooseList,
            Shape::ExactList,
            Shape::LooseMap,
            Shape::NonEmptyList,
            Shape::NonEmptyMap,
            Shape::NamedTable,
            Shape::RejectSet,
            Shape::PreservePairMap,
        ] {
            if mode == Mode::RequestedHosted && shape == Shape::NamedTable {
                continue; // a table RULE cannot be requested; its keys()-list can, and is a LooseList row
            }
            assert!(
                PARTICIPATION_TABLE
                    .iter()
                    .any(|r| r.mode == mode && r.shape == shape),
                "the grid has no row for {mode:?} × {shape:?} — every mode owes every shape one cell"
            );
        }
    }

    // 3. Every REFERENCED row's pin resolves to a test that still exists.
    let mut suite = String::new();
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/src/tests");
    for entry in std::fs::read_dir(dir).unwrap().flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|e| e == "rs") {
            suite.push_str(&std::fs::read_to_string(&path).unwrap());
        }
    }
    for row in PARTICIPATION_TABLE {
        if let Some(pin) = row.pinned_by {
            assert!(
                suite.contains(&format!("fn {pin}(")),
                "participation row {:?}/{:?}/{:?} cites `{pin}`, which is no test in src/tests — \
                 a referenced row whose pin vanished reads as coverage while asserting nothing",
                row.mode,
                row.shape,
                row.position
            );
        }
    }

    // 4. The sweeps and the table cannot disagree about which rows are built: every unpinned row's
    //    mode must be one a sweep actually drives.
    for row in PARTICIPATION_TABLE {
        if row.pinned_by.is_none() {
            assert!(
                matches!(
                    row.mode,
                    Mode::Local
                        | Mode::IndexDeferred
                        | Mode::WorkspaceBorrowed
                        | Mode::RequestedHosted
                ),
                "unpinned row in a mode no sweep drives: {:?}",
                row.mode
            );
        }
    }
}

// ---------------------------------------------------------------------------------------------
// The compile/link floors — full tier, batched per (mode, floor)
// ---------------------------------------------------------------------------------------------

/// Whether `wasm32-unknown-unknown` is installed. Same house pattern as
/// `extern_wrapper_index_defers_to_dep`: assert in CI, skip loudly locally — a silent skip would
/// turn the one floor that can see duplicate symbols into a no-op nobody notices.
fn wasm32_target_installed() -> bool {
    std::process::Command::new("rustup")
        .args(["target", "list", "--installed"])
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).contains("wasm32-unknown-unknown"))
        .unwrap_or(false)
}

/// Wire the committed wasm-clean dep pair into a generated consumer's two manifests.
/// The paths are ABSOLUTE on purpose: these consumers are generated into a scratch root outside the
/// checkout (so a floor never writes into `tests/`), where the committed fixtures' usual
/// `../../../` relative hop resolves to nothing.
fn wire_dep_manifests(export: &Path) {
    let tests = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/tests"));
    let rust_dep = format!(
        "index-dep-crate = {{ path = \"{}\" }}",
        tests.join("index-dep-crate").display()
    );
    let wasm_dep = format!(
        "index-dep-crate-wasm = {{ path = \"{}\" }}",
        tests.join("index-dep-crate-wasm").display()
    );
    super::integration_tests::append_manifest_deps(&export.join("rust/Cargo.toml"), &[&rust_dep]);
    super::integration_tests::append_manifest_deps(
        &export.join("wasm/Cargo.toml"),
        &[&rust_dep, &wasm_dep],
    );
}

/// Generate one floor consumer into `<scratch>/<leg>/export` and return the export dir.
fn generate_floor_consumer(leg: &str, spec: &[(&str, &str)], mode_flags: &[String]) -> PathBuf {
    let root = scratch_root("floors").join(leg);
    let _ = std::fs::remove_dir_all(&root);
    let input = root.join("inputs");
    let export = root.join("export");
    let files: Vec<(String, String)> = spec
        .iter()
        .map(|(rel, body)| ((*rel).to_owned(), (*body).to_owned()))
        .collect();
    write_files(&input, &files);
    let mut cmd = codegen_cmd();
    cmd.arg(format!("--input={}", input.display()))
        .arg(format!("--output={}", export.display()))
        .arg("--wasm=true");
    for flag in mode_flags {
        cmd.arg(flag);
    }
    let out = cmd.output().unwrap();
    assert!(
        out.status.success(),
        "{leg} floor generation must succeed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    export
}

/// The per-mode COMPILE/LINK floors, the only place the failure this whole grid is about is
/// observable: two `#[wasm_bindgen]` classes of one name are a `rust-lld: duplicate symbol`, which
/// `cargo check`, `cargo test` and every generation assertion above are structurally blind to.
///
/// Batched per (mode, floor) so cost stays bounded, and each leg is memoized by
/// `gate_cache::run_cached` on the generated crate's content hash:
///
/// * `local` — `cargo check` of a standalone generated wasm crate. No dependency exists, so the
///   question is only that a whole shape column compiles; the link property is vacuous.
/// * `index` / `workspace` — `cargo build --target wasm32-unknown-unknown` of consumer + the
///   committed wasm-clean dep pair. GREEN only: the RED leg (deferral off ⇒ duplicate symbol) is
///   already demonstrated once by `extern_wrapper_index_defers_to_dep`, and re-proving it per cell
///   buys nothing. What IS new here is the POSITION crossing — a named-rule declaration, a by-name
///   reference and a NON-ROOT declaring scope have never reached a wasm32 link, and an import routed
///   into the wrong module is exactly the class that survives every host-target check.
///
/// The two deferring legs carry deliberately different specs: under `--extern-wrapper-index` a rule
/// whose ident equals an indexed name DEFERS (so the declaration/reference positions belong in that
/// leg), while under `--workspace-dep` the same rule is criterion 9's shadow — minted locally, which
/// is a genuine duplicate symbol against the dep. Putting it in the workspace leg would assert that
/// the shipped, warned, correct behaviour fails to link.
#[test]
#[ignore]
fn wrapper_participation_mode_floors() {
    // Same-checkout repeat runs share these scratch roots, so serialize on them the way every other
    // heavy gate does — two runs interleaving inside one export directory clobber each other's
    // crates mid-cargo.
    let _lock = super::integration_tests::acquire_scratch_lock(&format!(
        "cddl_codegen_wrapper_participation_floors_{:016x}",
        checkout_hash()
    ));
    // ---- Local: the standalone column compiles -------------------------------------------------
    let local_rows = rows_for(Mode::Local);
    let local_export = {
        let batch = generate_consumer_mode("floor", Mode::Local, &local_rows);
        batch.export
    };
    run_cargo_floor(
        "wrapper_participation_mode_floors",
        "local-wasm-check",
        &local_export,
        &["check".to_owned()],
    );

    // ---- The deferring legs --------------------------------------------------------------------
    // Every wrapper below is one the COMMITTED dep pair really defines and really lists, so a GREEN
    // link is a statement about routing rather than about the fixture.
    const INDEX_SPEC: &[(&str, &str)] = &[
        (
            "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/index_dep_crate/mod.cddl",
            "idx_foo = _CDDL_CODEGEN_EXTERN_TYPE_\n",
        ),
        (
            "lib.cddl",
            // inline-anonymous (list, map, NonEmpty list) + a declaration whose ident equals the
            // indexed structural name + a by-name reference to it.
            "idx_foo_list = [* idx_foo]\n\n\
             holder = [a: [* idx_foo], b: {* uint => idx_foo}, c: [+ idx_foo]]\n\n\
             ref_holder = [x: idx_foo_list]\n",
        ),
        // the non-root declaring scope
        ("sub.cddl", "sub_holder = [m: [* idx_foo]]\n"),
    ];
    const WORKSPACE_SPEC: &[(&str, &str)] = &[
        (
            "_CDDL_CODEGEN_EXTERN_DEPS_DIR_/index_dep_crate/mod.cddl",
            "idx_foo = _CDDL_CODEGEN_EXTERN_TYPE_\n",
        ),
        (
            "lib.cddl",
            "holder = [a: [* idx_foo], b: {* uint => idx_foo}, c: [+ idx_foo]]\n",
        ),
        ("sub.cddl", "sub_holder = [m: [* idx_foo]]\n"),
    ];
    let common: Vec<String> = vec![
        "--preserve-encodings=true".to_owned(),
        format!("--common-import-override={DEP}"),
        format!("--extern-wasm-crate={DEP}={DEP_WASM}"),
    ];
    let mut index_flags = common.clone();
    index_flags.push(format!(
        "--extern-wrapper-index={DEP}=tests/index-dep-crate-wasm/src/collections.rs"
    ));
    let mut workspace_flags = common.clone();
    workspace_flags.push(format!("--workspace-dep={DEP}"));

    let legs = [
        ("index", INDEX_SPEC, index_flags),
        ("workspace", WORKSPACE_SPEC, workspace_flags),
    ];
    let exports: Vec<(&str, PathBuf)> = legs
        .iter()
        .map(|(leg, spec, flags)| {
            let export = generate_floor_consumer(leg, spec, flags);
            wire_dep_manifests(&export);
            (*leg, export)
        })
        .collect();

    if !wasm32_target_installed() {
        assert!(
            std::env::var_os("CI").is_none(),
            "wasm32-unknown-unknown is required to run wrapper_participation_mode_floors' link legs in CI"
        );
        eprintln!(
            "skipping wrapper_participation_mode_floors link legs: wasm32-unknown-unknown target not installed"
        );
        return;
    }
    for (leg, export) in &exports {
        run_cargo_floor(
            "wrapper_participation_mode_floors",
            &format!("{leg}-wasm32-link"),
            export,
            &[
                "build".to_owned(),
                "--target".to_owned(),
                "wasm32-unknown-unknown".to_owned(),
            ],
        );
    }
}

/// The `RequestedHosted` floor, split from the batch above because its subject is a different crate:
/// the HOST's own wasm crate, checked on the host target. A hosted wrapper is emitted into
/// `requested_collections.rs` from a SIDECAR rather than from this crate's spec, so it is the one
/// mint whose runtime provisioning (the `ordered_set` / `pair_map` modules), inner-type paths and
/// `try_from` sources are decided by an input the crate's own rules never mention — one arming
/// instance of this family was observable only in the host's compile. No wasm32 link: nothing links
/// against the host here, and the duplicate-symbol property belongs to the CONSUMER's build.
#[test]
#[ignore]
fn wrapper_participation_requested_host_floor() {
    let _lock = super::integration_tests::acquire_scratch_lock(&format!(
        "cddl_codegen_wrapper_participation_floor_requests_{:016x}",
        checkout_hash()
    ));
    // The requested rows include restricted NonEmpty wrappers and their recursive loose sources.
    // Every class that actually mints in requested_collections.rs is named locally; genuine root and
    // deferred homes retain their respective imports.
    let rows: Vec<&Row> = PARTICIPATION_TABLE
        .iter()
        .filter(|r| r.mode == Mode::RequestedHosted && r.pinned_by.is_none())
        .collect();
    let (root, sidecar_path) = write_request_fixture("floor", &rows);
    let export = root.join("export");
    let out = codegen_cmd()
        .arg(format!("--input={}", root.join("lib.cddl").display()))
        .arg(format!("--output={}", export.display()))
        .arg("--wasm=true")
        .arg("--lib-name=wp-dep")
        .arg(format!("--wrapper-requests=wpc={}", sidecar_path.display()))
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "the host generation must succeed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    run_cargo_floor(
        "wrapper_participation_requested_host_floor",
        "host-wasm-check",
        &export,
        &["check".to_owned()],
    );
}

/// Run one nested-cargo floor over a generated crate's `wasm/` sub-crate, memoized on the crate's
/// content hash so a re-run after an unrelated change costs a cache lookup.
fn run_cargo_floor(gate: &str, cell: &str, export: &Path, args: &[String]) {
    use super::integration_tests::tool_cmd;
    let mut failure = None;
    let outcome = super::gate_cache::run_cached(
        gate,
        cell,
        export,
        &[
            PathBuf::from("rust/Cargo.toml"),
            PathBuf::from("wasm/Cargo.toml"),
        ],
        &std::iter::once("cargo".to_owned())
            .chain(args.iter().cloned())
            .collect::<Vec<_>>(),
        || {
            let run = tool_cmd("cargo")
                .args(args.iter().map(String::as_str))
                .current_dir(export.join("wasm"))
                .output()
                .unwrap();
            if !run.status.success() {
                failure = Some(String::from_utf8_lossy(&run.stderr).into_owned());
            }
            run.status.success()
        },
    );
    assert!(
        outcome.success(),
        "{cell} floor must pass\n{}",
        failure.unwrap_or_default()
    );
}
