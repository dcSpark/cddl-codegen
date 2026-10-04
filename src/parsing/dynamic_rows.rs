// Dynamic record rows: recognition, directives, shape checks and remedies.

use super::{
    MapKeyKind, array_wrapped_domain_src, group_entry_map_key_kind, group_entry_occur,
    group_entry_rule_metadata, group_entry_to_type, normalized_dynamic_sequence_occurrence_window,
    occurrence_permits_count, record_plain_group_rest_row_domain_rejection,
    reject_custom_codec_on_row_entry, reject_custom_encodings_without_pair,
    reject_type_scoped_directives, resolved_plain_group_source_name, rust_type,
    rust_type_from_type1, single_type2, source_rule_name_of,
};
use crate::cli::Cli;
use crate::comment_ast::RuleMetadata;
use crate::intermediate::{
    ConceptualRustType, IntermediateTypes, Representation, RestKind, RestRow, RestSemantics,
    RustIdent,
};
use cddl::ast::parent::ParentVisitor;
use cddl::ast::{GroupEntry, MemberKey, OptionalComma};
use std::collections::BTreeSet;

/// The DYNAMIC (non-fixed) rows a record recognized, plus the flattened indices its fixed-field
/// loop must skip. One row for an open struct-map / open array, TWO for an open table, none for a
/// closed struct. `skip` names every CANDIDATE row — recognized or gracefully rejected — so a
/// rejected row never also becomes a bogus fixed field.
pub(super) struct DynamicRows {
    pub(super) typed_row: Option<Box<RestRow>>,
    pub(super) rest: Option<Box<RestRow>>,
    pub(super) array_segments: Vec<RestRow>,
    pub(super) skip: Vec<usize>,
}

/// Route a record's non-fixed rows to the OPEN TABLE recognizer (`t = { * K_t => V_t, * K_r => V_r }`
/// — two dynamic rows and no fixed key) or to the single-trailing-rest-row recognizer (everything
/// else). The two shapes are disjoint by construction, so this is a pure fork: an open table is not
/// an open struct-map with an extra row, it is a rule of its own kind (ZERO fixed fields, a typed
/// row claiming one wire major and a catch-all seeing the complement).
#[allow(clippy::too_many_arguments)]
pub(super) fn recognize_dynamic_rows(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> DynamicRows {
    let entry_count = flattened.len();
    if rep == Representation::Array {
        let (mut segments, skip) = recognize_array_rest_segments(
            types,
            parent_visitor,
            name,
            flattened,
            in_choice_arm,
            cli,
        );
        let rest = segments.first().cloned().map(Box::new);
        let array_segments = if segments.is_empty() {
            vec![]
        } else {
            segments.drain(1..).collect()
        };
        return DynamicRows {
            typed_row: None,
            rest,
            array_segments,
            skip,
        };
    }
    if rep == Representation::Map
        && entry_count == 2
        && flattened
            .iter()
            .all(|(ge, _)| matches!(group_entry_map_key_kind(ge), MapKeyKind::NonFixed))
    {
        return recognize_open_table(types, parent_visitor, name, flattened, in_choice_arm, cli);
    }
    let (rest, rest_index) =
        recognize_rest_row(types, parent_visitor, name, flattened, in_choice_arm, cli);
    DynamicRows {
        typed_row: None,
        rest,
        array_segments: vec![],
        skip: rest_index.into_iter().collect(),
    }
}

/// Recognize the positional dynamic rows of an ARRAY record.  The historic single-segment path
/// remains untouched below so its zero/one output and diagnostics stay stable. Multiple members
/// must each be a named captured segment; finalization proves every variable boundary from the
/// possible-next effective wire majors, while exact windows retain their count-owned boundary.
#[allow(clippy::too_many_arguments)]
fn recognize_array_rest_segments(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> (Vec<RestRow>, Vec<usize>) {
    let candidates: Vec<usize> = flattened
        .iter()
        .enumerate()
        .filter(|(_, (ge, _))| occurrence_permits_count(ge))
        .map(|(index, _)| index)
        .collect();
    if candidates.len() <= 1 {
        let (rest, skip) =
            recognize_array_rest_tail(types, parent_visitor, name, flattened, in_choice_arm, cli);
        return (
            rest.into_iter().map(|row| *row).collect(),
            skip.into_iter().collect(),
        );
    }

    // Preserve the historic multiplicity boundary for a repeated plain-group reference. A plain
    // group is not an array item, so it cannot be one of the new occurrence segments; routing to
    // the legacy recognizer retains its established "single trailing rest tail" diagnostic before
    // the multi-segment classifier considers its `@name` directives.
    if candidates.iter().any(|&candidate| {
        let (entry, _) = flattened[candidate];
        matches!(entry, GroupEntry::TypeGroupname { ge, .. }
            if types.directly_defined_plain_group_idents().any(|ident|
                types.source_rule_name(ident).is_some_and(|source| source == ge.name.to_string())))
    }) {
        let (rest, skip) =
            recognize_array_rest_tail(types, parent_visitor, name, flattened, in_choice_arm, cli);
        return (
            rest.into_iter().map(|row| *row).collect(),
            skip.into_iter().collect(),
        );
    }

    let src = source_rule_name_of(types, name);
    if reject_row_container_placement(
        types,
        name,
        &src,
        in_choice_arm,
        DynamicRowShape::ArraySegments,
    ) {
        return (vec![], candidates);
    }

    let mut rows = Vec::new();
    let mut names = BTreeSet::new();
    for &candidate in &candidates {
        let (entry, comma) = flattened[candidate];
        let occur = group_entry_occur(entry);
        let occurrence = normalized_dynamic_sequence_occurrence_window(types, occur);
        if let GroupEntry::ValueMemberKey { ge, .. } = entry
            && ge.member_key.is_some()
        {
            types.record_rejection(format!(
                "rule `{src}`: an array occurrence segment is positional and cannot carry a member key. Drop the `key:` label."
            ));
            return (vec![], candidates);
        }
        let metadata = group_entry_rule_metadata(entry, comma);
        let Some(field_name) = metadata.name.clone() else {
            types.record_rejection(format!(
                "rule `{src}`: multiple array occurrence segments require each repeated member to have a unique `@name`. Add `; @name <segment>` to this member."
            ));
            return (vec![], candidates);
        };
        if !names.insert(field_name.clone()) {
            types.record_rejection(format!(
                "rule `{src}`: multiple array occurrence segments would both emit the field `{field_name}`. Give every segment a unique `@name`."
            ));
            return (vec![], candidates);
        }
        reject_type_scoped_directives(
            types,
            &format!("the array occurrence segment of rule `{src}`"),
            &metadata,
        );
        if reject_custom_codec_on_row_entry(
            types,
            &format!("array occurrence segment of rule `{src}`"),
            "Name the segment element type as its own rule and put the pair there.",
            &metadata,
        ) || reject_custom_encodings_without_pair(
            types,
            &format!("the array occurrence segment of rule `{src}`"),
            &metadata,
        ) {
            return (vec![], candidates);
        }
        if metadata.ignore || metadata.duplicates.is_some() {
            types.record_rejection(format!(
                "rule `{src}`: multiple array occurrence segments must be captured named boundaries; `@ignore` and `@duplicates` are not supported on a segment. Remove the directive."
            ));
            return (vec![], candidates);
        }
        let element = group_entry_to_type(types, parent_visitor, entry, cli);
        if element.conceptual_type.is_fixed_value() {
            types.record_rejection(format!(
                "rule `{src}`: an array occurrence segment cannot be a fixed value — use a typed element."
            ));
            return (vec![], candidates);
        }
        if element.is_basic(types)
            && matches!(element.conceptual_type.resolve_alias_shallow(), ConceptualRustType::Rust(ident) if types.is_plain_group(ident))
        {
            types.record_rejection(format!(
                "rule `{src}`: an array occurrence segment cannot capture a plain group, because a group splices several array members rather than one element. Frame it as its own array rule first."
            ));
            return (vec![], candidates);
        }
        rows.push(RestRow {
            kind: RestKind::ArrayTail {
                element,
                source_index: candidate,
            },
            semantics: RestSemantics::Capture,
            field_name,
            dispatch_major: None,
            occurrence: occurrence.map(crate::intermediate::RestOccurrenceWindow::from_raw),
        });
    }
    (rows, candidates)
}

/// Recognize an OPEN TABLE — a NAMED rule spelled `t = { * K_t => V_t, * K_r => V_r }`: one typed
/// table row plus one trailing typed catch-all rest row, and nothing else. The typed row claims
/// exactly its key's single statically-known CBOR major; the catch-all sees only the complement.
///
/// Only the SHAPE is decided here. Whether `K_t`'s major is statically knowable at all — the
/// two-stage staticness rule, and the `@custom_wire_major` declaration a custom-codec key needs — is
/// decided in `IntermediateTypes::finalize`, because it needs `cbor_types()` (which panics on an
/// unregistered ident) and must run after generic resolution. Parse decides SHAPE, finalize decides
/// STATICNESS.
///
/// Both rows are `RestRow`s: the typed row is a dynamic sequence in exactly the sense the delivered
/// capture engine already handles (its own `@duplicates`, its own container, its own encoding
/// sidecars), so it reuses that machinery verbatim rather than minting a new struct kind.
fn recognize_open_table(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> DynamicRows {
    let skip = vec![0usize, 1usize];
    let rejected = || DynamicRows {
        typed_row: None,
        rest: None,
        array_segments: vec![],
        skip: vec![0usize, 1usize],
    };
    let src = source_rule_name_of(types, name);
    // An INLINE anonymous open table (`f: { * k1 => v1, * k2 => v2 }`) is rejected: the shape mints a
    // struct with two container members and a keys-list wrapper, all named off the rule ident, and a
    // synthesized structural name for those would be a new name family with no user-visible source
    // spelling. The named-rule concession is also what keeps the wasm collision story to legs on the
    // existing detectors rather than a new sibling. Point at the named-rule form.
    if !types.is_toplevel_rule(name) {
        types.record_rejection(format!(
            "rule `{src}`: an INLINE open table (`f: {{ * k1 => v1, * k2 => v2 }}`) is unsupported. \
             Give the open table its own named rule (`t = {{ * k1 => v1, * k2 => v2 }}`) and \
             reference it by name — the generated struct, its two containers and its keys list are \
             all named off that rule."
        ));
        return rejected();
    }
    // A group-choice arm and a plain group are rejected for the same reasons the single rest row is
    // (an arm collapses into an enum variant, dropping the open semantics; a materialized plain group
    // exports transparently as a CLOSED group body across a crate boundary).
    if reject_row_container_placement(types, name, &src, in_choice_arm, DynamicRowShape::OpenTable)
    {
        return rejected();
    }
    let Some(typed) = open_table_row(
        types,
        parent_visitor,
        &src,
        flattened[0],
        OpenTableRowKind::Typed,
        cli,
    ) else {
        return rejected();
    };
    let Some(catch_all) = open_table_row(
        types,
        parent_visitor,
        &src,
        flattened[1],
        OpenTableRowKind::CatchAll,
        cli,
    ) else {
        return rejected();
    };
    // The two rows become two `pub` fields on one struct, so their names must differ. Only reachable
    // by `@name`-ing one row onto the other's name (the defaults are distinct).
    if typed.field_name == catch_all.field_name {
        types.record_rejection(format!(
            "rule `{src}`: the open table's typed row and catch-all row would both emit a field \
             named `{}` — the two rows are two separate containers on one struct, so their names \
             must differ. Rename one with a `; @name <other>` directive on that row.",
            typed.field_name
        ));
        return rejected();
    }
    DynamicRows {
        typed_row: Some(Box::new(typed)),
        rest: Some(Box::new(catch_all)),
        array_segments: vec![],
        skip,
    }
}

/// Which of an open table's two rows is being built. They differ only in their default field name
/// and their rejection-message wording; both carry the full normalized dynamic-map-row occurrence
/// vocabulary, with each window counting entries in THAT row alone.
#[derive(Copy, Clone, PartialEq, Eq)]
enum OpenTableRowKind {
    Typed,
    CatchAll,
}

impl OpenTableRowKind {
    /// The slot's name in rejection messages.
    fn slot(self) -> &'static str {
        match self {
            OpenTableRowKind::Typed => "open table typed row (`* k1 => v1`)",
            OpenTableRowKind::CatchAll => "open table catch-all row (`* k2 => v2`)",
        }
    }

    /// The captured field's default Rust name (`@name`-overridable). The catch-all keeps the open
    /// struct-map's `rest` so the two capture surfaces read alike across the two shapes.
    fn default_field_name(self) -> &'static str {
        match self {
            OpenTableRowKind::Typed => "entries",
            OpenTableRowKind::CatchAll => "rest",
        }
    }
}

/// Build ONE row of an open table from its group entry, or record a graceful rejection and return
/// `None`. Shape-level only (see `recognize_open_table`).
fn open_table_row(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    src: &str,
    entry: &(GroupEntry, OptionalComma),
    kind: OpenTableRowKind,
    cli: &Cli,
) -> Option<RestRow> {
    let (ge_entry, comma) = entry;
    let slot = kind.slot();
    let occur = match ge_entry {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        _ => None,
    };
    // Each open-table row owns its own row-local cardinality.  In particular, the catch-all's `+`
    // is not a statement about the typed region: it is `NonEmptyMap` over captured entries, exactly
    // as a `2*3` catch-all is a BoundedMap over captured entries.  Both rows stage loose during
    // decoding and cross their one checked door afterwards.
    let occurrence = normalized_dynamic_sequence_occurrence_window(types, occur);
    let (domain, range) = match ge_entry {
        GroupEntry::ValueMemberKey { ge, .. } => {
            let domain = match &ge.member_key {
                Some(MemberKey::Type1 { t1, .. }) => {
                    rust_type_from_type1(types, parent_visitor, t1, cli)
                }
                _ => {
                    types.record_rejection(format!(
                        "rule `{src}`: unsupported {slot} key spelling (expected `* k => v`)."
                    ));
                    return None;
                }
            };
            let range = rust_type(types, parent_visitor, &ge.entry_type, cli);
            (domain, range)
        }
        _ => {
            types.record_rejection(format!(
                "rule `{src}`: unsupported {slot} spelling (expected `* k => v`)."
            ));
            return None;
        }
    };
    // A null-admitting key domain collides with the break that ends an indefinite-length map (both
    // are CBOR major 7) — the same reason the open struct-map rest row rejects it.
    if matches!(
        domain.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    ) {
        types.record_rejection(format!(
            "rule `{src}`: the {slot} cannot take a null-admitting key domain (`* (t / null) => \
             v`): a `null` key and the break that ends an indefinite-length map are both CBOR \
             special values, so the row's key dispatch cannot tell them apart. Drop the `null` arm \
             from the key type."
        ));
        return None;
    }
    // A bare `any` TYPED key is a shape error, not a staticness one: it would claim all eight
    // majors, leaving the catch-all nothing to see. (The catch-all is exactly the position `any`
    // belongs in.)
    if kind == OpenTableRowKind::Typed
        && matches!(
            domain.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Any
        )
    {
        types.record_rejection(format!(
            "rule `{src}`: the {slot} cannot be keyed on `any` — the typed row claims exactly one \
             CBOR major type and `any` admits all eight, so the catch-all row would never see an \
             entry. Key the typed row on a concrete type and let the catch-all take `any`."
        ));
        return None;
    }
    let metadata = group_entry_rule_metadata(ge_entry, comma);
    if reject_custom_codec_on_row_entry(
        types,
        &format!("{slot} of rule `{src}`"),
        MAP_ROW_CODEC_REMEDY,
        &metadata,
    ) {
        return None;
    }
    if reject_custom_encodings_without_pair(
        types,
        &format!("the {slot} of rule `{src}`"),
        &metadata,
    ) {
        return None;
    }
    // `@ignore` (tolerate-and-drop) has no meaning on either row of an open table: the whole rule IS
    // its two containers, so ignoring one leaves a struct that silently drops half the map — and
    // ignoring the typed one leaves a rule with nothing typed about it.
    if metadata.ignore {
        types.record_rejection(format!(
            "rule `{src}`: `@ignore` (tolerate-and-drop) is not supported on the {slot} — an open \
             table's rows ARE the rule's content, so dropping one would silently discard half the \
             map. Drop the `@ignore` to capture both rows (the default), or use an open struct-map \
             (`{{ 1: a, * k => v ; @ignore }}`) if you want the drop."
        ));
        return None;
    }
    let field_name = metadata
        .name
        .clone()
        .unwrap_or_else(|| kind.default_field_name().to_owned());
    Some(RestRow {
        kind: RestKind::MapEntries {
            domain,
            range,
            duplicates: metadata.duplicates,
        },
        semantics: RestSemantics::Capture,
        field_name,
        // Derived in `finalize` for the typed row (see the field doc); the catch-all never has one.
        dispatch_major: None,
        occurrence: occurrence.map(crate::intermediate::RestOccurrenceWindow::from_raw),
    })
}

/// The four dynamic-row shapes a record body can spell, for the container-placement refusal they
/// share: a dynamic row is open (or multi-segment) semantics a group-choice arm would collapse into
/// an enum variant, and a materialized plain group exports transparently as a CLOSED group body
/// across a crate boundary. Each shape keeps its own message: the texts are what a spec author acts
/// on, and each names its own shape and remedy.
#[derive(Copy, Clone, PartialEq, Eq)]
enum DynamicRowShape {
    /// An open struct-map's trailing rest row (`{ 1: a, * k => v }`).
    MapRest,
    /// An open array's single occurrence-bearing member (`[ a, * t ]`).
    ArrayTail,
    /// Several named array occurrence segments (`[ * a ; @name x, b, * c ; @name y ]`).
    ArraySegments,
    /// An open table's typed row plus catch-all (`{ * k1 => v1, * k2 => v2 }`).
    OpenTable,
}

impl DynamicRowShape {
    fn in_choice_arm_rejection(self, src: &str) -> String {
        match self {
            DynamicRowShape::MapRest => format!(
                "rule `{src}`: an open struct-map rest row (`* k => v`) inside a group-choice arm \
                 (`{{ … }} // {{ … }}`) is unsupported. Give the open map its own named rule and \
                 reference it from the arm."
            ),
            DynamicRowShape::ArrayTail => format!(
                "rule `{src}`: an open-array rest tail (`* t`) inside a group-choice arm \
                 (`[ … ] // [ … ]`) is unsupported. Give the open array its own named rule and reference \
                 it from the arm."
            ),
            DynamicRowShape::ArraySegments => format!(
                "rule `{src}`: multiple array occurrence segments inside a group-choice arm are unsupported. Give the array its own named rule and reference it from the arm."
            ),
            DynamicRowShape::OpenTable => format!(
                "rule `{src}`: an open table (`{{ * k1 => v1, * k2 => v2 }}`) inside a group-choice arm \
                 (`{{ … }} // {{ … }}`) is unsupported. Give the open table its own named rule and \
                 reference it from the arm."
            ),
        }
    }

    fn plain_group_rejection(self, src: &str) -> String {
        match self {
            DynamicRowShape::MapRest => format!(
                "rule `{src}`: an open struct-map rest row (`* k => v`) inside a plain group \
                 (`{src} = ( … * k => v )`, embedded elsewhere) is unsupported. Give the open map its \
                 own named rule (`{src} = {{ … * k => v }}`) and reference it by name."
            ),
            DynamicRowShape::ArrayTail => format!(
                "rule `{src}`: an open-array rest tail (`* t`) inside a plain group \
                 (`{src} = ( … * t )`, embedded elsewhere) is unsupported. Give the open array its own \
                 named rule (`{src} = [ … * t ]`) and reference it by name."
            ),
            DynamicRowShape::ArraySegments => format!(
                "rule `{src}`: multiple array occurrence segments inside a plain group are unsupported. Give the array its own named rule and reference it from the group."
            ),
            DynamicRowShape::OpenTable => format!(
                "rule `{src}`: an open table (`* k1 => v1, * k2 => v2`) inside a plain group (`{src} = \
                 ( … )`, embedded elsewhere) is unsupported. Give the open table its own named rule \
                 (`{src} = {{ * k1 => v1, * k2 => v2 }}`) and reference it by name."
            ),
        }
    }
}

/// Refuse a dynamic row placed in a group-choice arm or in a plain group (`DynamicRowShape`), in
/// that order. Returns whether it refused.
fn reject_row_container_placement(
    types: &mut IntermediateTypes,
    name: &RustIdent,
    src: &str,
    in_choice_arm: bool,
    shape: DynamicRowShape,
) -> bool {
    let refusal = if in_choice_arm {
        shape.in_choice_arm_rejection(src)
    } else if types.is_plain_group(name) {
        shape.plain_group_rejection(src)
    } else {
        return false;
    };
    types.record_rejection(refusal);
    true
}

/// The remedy for a custom (de)serializer pair on a MAP row's entry slot (a rest row or either open
/// table row).
const MAP_ROW_CODEC_REMEDY: &str = "Name the row's key or value type as its own rule and put the \
     pair there (`k = text ; @custom_serialize <fn> @custom_deserialize <fn>`, then `* k => v`).";

/// The two single-row rest shapes whose entry slot honors `@name` and `@ignore`
/// (`rest_row_directives`).
#[derive(Copy, Clone, PartialEq, Eq)]
enum RestRowShape {
    MapRest,
    ArrayTail,
}

impl RestRowShape {
    /// The slot's name in its directive refusals.
    fn slot(self) -> &'static str {
        match self {
            RestRowShape::MapRest => "open struct-map rest row (`* k => v`)",
            RestRowShape::ArrayTail => "open-array rest tail (`* t`)",
        }
    }

    fn codec_remedy(self) -> &'static str {
        match self {
            RestRowShape::MapRest => MAP_ROW_CODEC_REMEDY,
            RestRowShape::ArrayTail => {
                "Name the tail element type as its own rule and put the pair there (`e = uint ; \
                 @custom_serialize <fn> @custom_deserialize <fn>`, then `* e`)."
            }
        }
    }

    /// Tolerate-and-drop re-serializes no captured entries: deliberately lossy, but faithful only
    /// for the loose `*`/`0*` form. Zero violates a positive minimum, and a zero-minimum restricted
    /// row would lose the bounded/exact state its checked carrier holds.
    fn ignore_restricted_rejection(self, src: &str) -> String {
        match self {
            RestRowShape::MapRest => format!(
                "rule `{src}`: `@ignore` cannot apply to a restricted open struct-map rest row — \
                 dropping every captured entry would re-serialize zero occurrences: that violates \
                 a positive minimum, while a zero-minimum restricted window would lose the bounded \
                 or exact state its checked carrier retains. Keep the loose `* k => v ; @ignore` \
                 form, or drop `@ignore` to retain the checked carrier."
            ),
            RestRowShape::ArrayTail => format!(
                "rule `{src}`: `@ignore` cannot apply to a restricted open-array rest tail, because \
                 dropping every captured element would re-serialize zero occurrences: that violates \
                 a positive minimum, while a zero-minimum restricted window would lose the bounded \
                 or exact state its checked carrier retains. Drop `@ignore` to capture the checked \
                 tail."
            ),
        }
    }

    /// `@ignore` + `--preserve-encodings` is PERMANENTLY rejected: a preserve crate's contract is
    /// byte-exact round-trips, which a deliberately-lossy type undermines crate-wide.
    /// `--canonical-form` implies preserve (enforced in `api.rs`), so this covers it transitively.
    fn ignore_preserve_rejection(self, src: &str) -> String {
        match self {
            RestRowShape::MapRest => format!(
                "rule `{src}`: `@ignore` (tolerate-and-drop) on an open struct-map rest row is not \
                 supported under --preserve-encodings, because a preserve crate's contract is \
                 byte-exact round-trips and a silently-lossy type undermines it. Drop the `@ignore` \
                 to capture the unknown entries (the default), or use `@custom_serialize` / \
                 `@custom_deserialize` for a genuine view type."
            ),
            RestRowShape::ArrayTail => format!(
                "rule `{src}`: `@ignore` (tolerate-and-drop) on an open-array rest tail is not \
                 supported under --preserve-encodings, because a preserve crate's contract is \
                 byte-exact round-trips and a silently-lossy type undermines it. Drop the `@ignore` \
                 to capture the trailing elements (the default), or use `@custom_serialize` / \
                 `@custom_deserialize` for a genuine view type."
            ),
        }
    }

    /// `@ignore` + `@name`: `@name` renames the captured field, which an ignore row does not emit.
    fn ignore_name_rejection(self, src: &str) -> String {
        match self {
            RestRowShape::MapRest => format!(
                "rule `{src}`: `@ignore` and `@name` cannot both apply to an open struct-map rest \
                 row — `@ignore` emits no field to name. Drop `@name`, or drop `@ignore` to capture \
                 the entries into the named field."
            ),
            RestRowShape::ArrayTail => format!(
                "rule `{src}`: `@ignore` and `@name` cannot both apply to an open-array rest tail — \
                 `@ignore` emits no field to name. Drop `@name`, or drop `@ignore` to capture the \
                 elements into the named field."
            ),
        }
    }
}

/// Validate a rest row's own entry slot after its placement and slot-shape guards, and return the
/// row's semantics and field name (`@name`, default `rest`), or `None` after recording the refusal.
///
/// The row declares no type of its own, so the TYPE-SCOPED directives are refused (reported, not
/// fatal) and so is an inert custom codec pair or a `@custom_encodings` with no pair to describe.
/// `@ignore` selects the tolerate-and-DROP flavor, which combines with nothing else on the row. The
/// order is the contract and differs per shape only in `@duplicates`: an array tail has no keys, so
/// it refuses `@duplicates` outright before `@ignore` is read, while a map row refuses it only beside
/// `@ignore`, after the restricted-occurrence and preserve checks.
fn rest_row_directives(
    types: &mut IntermediateTypes,
    cli: &Cli,
    shape: RestRowShape,
    src: &str,
    metadata: &RuleMetadata,
    restricted: bool,
) -> Option<(RestSemantics, String)> {
    let slot = shape.slot();
    reject_type_scoped_directives(types, &format!("the {slot} of rule `{src}`"), metadata);
    if reject_custom_codec_on_row_entry(
        types,
        &format!("{slot} of rule `{src}`"),
        shape.codec_remedy(),
        metadata,
    ) || reject_custom_encodings_without_pair(
        types,
        &format!("the {slot} of rule `{src}`"),
        metadata,
    ) {
        return None;
    }
    if shape == RestRowShape::ArrayTail && metadata.duplicates.is_some() {
        types.record_rejection(format!(
            "rule `{src}`: `@duplicates` does not apply to an open-array rest tail — an array tail \
             has no keys, so there is no duplicate policy to govern. Remove `@duplicates`."
        ));
        return None;
    }
    if metadata.ignore {
        let refusal = if restricted {
            Some(shape.ignore_restricted_rejection(src))
        } else if cli.preserve_encodings {
            Some(shape.ignore_preserve_rejection(src))
        } else if metadata.duplicates.is_some() {
            // Only a map row reaches here with `@duplicates`: a duplicates policy governs a
            // captured container, which `@ignore` does not create.
            Some(format!(
                "rule `{src}`: `@ignore` and `@duplicates` cannot both apply to an open struct-map \
                 rest row — `@ignore` drops unknown entries, so there is no container for a \
                 duplicates policy to govern. Keep one: `@ignore` to drop, or `@duplicates` (with \
                 capture) to retain."
            ))
        } else if metadata.name.is_some() {
            Some(shape.ignore_name_rejection(src))
        } else {
            None
        };
        if let Some(refusal) = refusal {
            types.record_rejection(refusal);
            return None;
        }
    }
    let semantics = if metadata.ignore {
        RestSemantics::Ignore
    } else {
        RestSemantics::Capture
    };
    let field_name = metadata.name.clone().unwrap_or_else(|| "rest".to_owned());
    Some((semantics, field_name))
}

/// Recognize a trailing open-map rest row (`* K => V`) in a map-rep record, or reject an
/// unsupported placement/shape gracefully. Returns the built `RestRow` (if recognized and every
/// guard passes) and the flattened index of the rest-CANDIDATE row (so the caller's field loop
/// skips it — whether recognized or rejected). A map with no non-fixed entry returns
/// `(None, None)`. Only `recognize_dynamic_rows` calls it, after the Array case has returned; the
/// array analog (a final-position `* T` tail) is `recognize_array_rest_tail`.
#[allow(clippy::too_many_arguments)]
fn recognize_rest_row(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> (Option<Box<RestRow>>, Option<usize>) {
    let entry_count = flattened.len();
    let nonfixed_indices: Vec<usize> = flattened
        .iter()
        .enumerate()
        .filter(|(_, (ge, _))| matches!(group_entry_map_key_kind(ge), MapKeyKind::NonFixed))
        .map(|(i, _)| i)
        .collect();
    let Some(&candidate) = nonfixed_indices.last() else {
        // No non-fixed entry: an ordinary closed struct. Byte-identical to pre-feature output.
        return (None, None);
    };
    let src = source_rule_name_of(types, name);
    // The row is still skipped when refused, so no fixed field is built from it.
    if reject_row_container_placement(types, name, &src, in_choice_arm, DynamicRowShape::MapRest) {
        return (None, Some(candidate));
    }
    // Multiple non-fixed rows: only a single trailing rest row is supported.
    if nonfixed_indices.len() > 1 {
        types.record_rejection(format!(
            "rule `{src}`: a map supports at most a single trailing rest row (`* k => v`) after \
             its fixed keys, or — with NO fixed keys — an open table's two rows (`{{ * k1 => v1, * \
             k2 => v2 }}`: one typed row plus one trailing catch-all). This map has {} non-fixed \
             rows. Keep one `* k => v` row (last), drop the fixed keys to spell an open table, or \
             move the extras into their own table rules.",
            nonfixed_indices.len()
        ));
        return (None, Some(candidate));
    }
    // Non-final placement: the rest row must be the LAST entry (fixed keys are dispatched first).
    if candidate != entry_count - 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) must be the LAST entry of the \
             map (fixed keys are matched first, then the rest captures the remainder). Move it to \
             the end."
        ));
        return (None, Some(candidate));
    }
    // An open struct-map needs ≥1 fixed key before the rest row (a rest row IS the "open" part of an
    // open MAP). A single `* k => v` entry with nothing before it is a TABLE, which is recognized by
    // `parse_group_type` before this function ever runs — so a lone non-fixed entry here (e.g. an
    // alias-to-literal arrow key) is a degenerate shape, not an open struct.
    if candidate == 0 {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row (`* k => v`) must follow at least one fixed \
             key (`{{ 1: a, * k => v }}`). A map whose only entry is `* k => v` is a table — give it \
             its own rule (`t = {{ * k => v }}`)."
        ));
        return (None, Some(candidate));
    }
    let (candidate_ge, candidate_comma) = flattened[candidate];
    // A dynamic map row carries the same complete occurrence vocabulary as a homogeneous table.
    // Unlike a fixed keyed field, every repetition is an entry of the row's own checked carrier, so
    // a non-loose window is neither silently narrowed nor a statement about the record as a whole.
    let occur = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => ge.occur.as_ref().map(|o| &o.occur),
        _ => None,
    };
    let occurrence_src = occur.map(|occur| format!("{occur} ")).unwrap_or_default();
    let occurrence = normalized_dynamic_sequence_occurrence_window(types, occur);
    // Extract the key (domain) and value (range) types from the arrow entry, keeping the SOURCE
    // spellings of both slots — the slot-shape rejections below print the author's own row back.
    let (domain, range, domain_src, range_src) = match candidate_ge {
        GroupEntry::ValueMemberKey { ge, .. } => {
            let (domain, domain_src) = match &ge.member_key {
                Some(MemberKey::Type1 { t1, .. }) => {
                    (rust_type_from_type1(types, parent_visitor, t1, cli), t1)
                }
                // A non-fixed key that is not a Type1 arrow (`NonMemberKey`) — unreachable for a
                // classified NonFixed arrow row, but reject rather than panic if it ever appears.
                _ => {
                    types.record_rejection(format!(
                        "rule `{src}`: unsupported rest-row key spelling (expected `* k => v`)."
                    ));
                    return (None, Some(candidate));
                }
            };
            let range = rust_type(types, parent_visitor, &ge.entry_type, cli);
            (domain, range, domain_src, &ge.entry_type)
        }
        _ => {
            types.record_rejection(format!(
                "rule `{src}`: unsupported rest-row spelling (expected `* k => v`)."
            ));
            return (None, Some(candidate));
        }
    };
    // A general key domain is supported: bare `uint`/`text`/`any` keep the fast peeked-key dispatch,
    // everything else takes the typed seek path (`RestRow::map_key_uses_peeked_path` routes them, and
    // is the ONE predicate parsing/IR/generation share). Three shapes stay rejected, for reasons the
    // slot type itself carries rather than the row's plumbing:
    //
    //   * a NULL-ADMITTING domain (`k = text / null` → `Optional<..>`), rejected here — a `null` key
    //     arrives as CBOR major type 7, the same dispatch arm that carries the indefinite-map BREAK,
    //     so the row cannot tell "the map ended" from "the next key is null" without deciding one of
    //     them wrong.
    //   * a FLOAT-containing domain, rejected in `IntermediateTypes::finalize` beside the table/set
    //     float instruments (floats have no total order, so they can key nothing) — the one place
    //     that also sees a float hidden behind a resolved generic instance.
    //   * a PLAIN GROUP in EITHER slot, rejected here — the only one of the three that also applies
    //     to the VALUE slot, because it is a property of the map ENTRY (each of its two slots holds
    //     exactly one item) rather than of key dispatch.
    //
    // Fixed-value domains (`* 5 => v`, or an alias to one) never reach here: the zero-permitting
    // occurrence guard and the bare-fixed-value rule guard reject them first.
    if matches!(
        domain.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    ) {
        types.record_rejection(format!(
            "rule `{src}`: an open struct-map rest row cannot take a null-admitting key domain (`* \
             (t / null) => v`): a `null` key and the break that ends an indefinite-length map are \
             both CBOR special values, so the row's key dispatch cannot tell them apart. Drop the \
             `null` arm from the key type (a missing entry already means absent)."
        ));
        return (None, Some(candidate));
    }
    // A plain group in EITHER slot — see `record_plain_group_rest_row_domain_rejection`. The
    // fixed-prefix sibling of the table twin's guard, sharing its
    // `resolved_plain_group_source_name`
    // predicate (`is_basic` over the RESOLVED type), so the bare (`* kv => uint`) and ALIAS
    // spellings land on ONE message on every profile. A TAGGED spelling
    // (`* uint => #6.10(kv)`) reaches the earlier tag-payload semantic refusal; array-WRAPPED forms
    // keep their supported verdicts (an inline `[kv]` carries `basic_override`, so it is not
    // `is_basic`). Both roles are reported when both offend — a silent slot would be the worse
    // failure — and the guard sits BEFORE the row's directive reads, because a directive cannot be
    // judged on a row that has no representable slot.
    let key_group = resolved_plain_group_source_name(types, &domain);
    let value_group = resolved_plain_group_source_name(types, &range);
    if key_group.is_some() || value_group.is_some() {
        let entry_src = format!("{domain_src} => {range_src}");
        if let Some(group_name) = key_group {
            record_plain_group_rest_row_domain_rejection(
                types,
                &src,
                &entry_src,
                "KEY",
                &group_name,
                &format!(
                    "{occurrence_src}{} => {range_src}",
                    array_wrapped_domain_src(&domain_src.to_string(), &domain_src.type2)
                ),
            );
        }
        if let Some(group_name) = value_group {
            record_plain_group_rest_row_domain_rejection(
                types,
                &src,
                &entry_src,
                "VALUE",
                &group_name,
                &format!(
                    "{occurrence_src}{domain_src} => {}",
                    match single_type2(range_src) {
                        Some(t2) => array_wrapped_domain_src(&range_src.to_string(), t2),
                        None => format!("[{range_src}]"),
                    }
                ),
            );
        }
        return (None, Some(candidate));
    }
    // Both open struct-map flavors are now wired end-to-end. CAPTURE (default) has every generated
    // surface: the JSON flattened rest surface (captured entries render at the same object level as
    // declared fields, with the write-side collision check and key-coercing read wrapper), the
    // --preserve-encodings / --canonical-form fidelity path, and the wasm rest accessor (a getter
    // returning the captured entries as the wasm map wrapper). IGNORE (`@ignore` on the row)
    // tolerate-and-drops: the deserialize arms typed-consume each unknown entry and discard it (no
    // field, serialize emits declared members only, JSON/schemars/wasm are a closed struct's), and it
    // is rejected under --preserve-encodings. No front door remains here for either flavor.
    // Entry-level directives are read from the rest row's own trailing slot — NOT rule-position
    // handling: on a map TYPE rule the parser binds a trailing comment written after the closing
    // brace to the RULE slot alone (`u = { 1: a, * k => v } ; @ignore` is refused as a rule-position
    // `@ignore`, and this walk never sees it). The dual-read slot is a plain GROUP rule's last entry,
    // which is a different construct.
    let rest_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    let Some((semantics, field_name)) = rest_row_directives(
        types,
        cli,
        RestRowShape::MapRest,
        &src,
        &rest_metadata,
        occurrence.is_some(),
    ) else {
        return (None, Some(candidate));
    };
    // `@duplicates` policy on the rest row (CAPTURE flavor): default (reject) uses the loose container
    // (value-equality dup check — accept/reject keyed on the wire VALUE, not the domain's spelling);
    // `preserve` uses the vec-of-pairs twin (`PairMap`), matching what `@duplicates preserve` TABLES do
    // — duplicate keys accepted and re-emitted in wire order. `reject` explicit is the same as default.
    // Carried on the `RestRow` for the emitters to select the container. (Rejected for `@ignore`.)
    let rest_row = RestRow {
        kind: RestKind::MapEntries {
            domain,
            range,
            duplicates: rest_metadata.duplicates,
        },
        semantics,
        field_name,
        // Only an open table's TYPED row claims a single major; a catch-all sees the complement.
        dispatch_major: None,
        occurrence: occurrence.map(crate::intermediate::RestOccurrenceWindow::from_raw),
    };
    (Some(Box::new(rest_row)), Some(candidate))
}

/// The array-rep analog of `recognize_rest_row`: recognize one occurrence-bearing array segment
/// (`[a, b, * t]`, `[a, * t, b]`, `[a, b, + t]`, or `[a, b, 2*3 t]`) as the positional sibling of
/// the map rest row, or reject an unsupported placement/shape gracefully. Final segments retain the
/// shipped tail behavior; a non-final candidate is validated after fields and aliases finalize, where
/// its immediate suffix's exact wire facts are available. Returns the built `RestRow` (if recognized)
/// and the flattened index of the candidate (so the caller's field loop skips it — whether recognized
/// or rejected). Arrays have no keys, so there is no key dispatch / duplicate policy / domain typing.
/// `(None, None)` when no count-permitting entry exists (an ordinary closed array).
#[allow(clippy::too_many_arguments)]
fn recognize_array_rest_tail(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    flattened: &[&(GroupEntry, OptionalComma)],
    in_choice_arm: bool,
    cli: &Cli,
) -> (Option<Box<RestRow>>, Option<usize>) {
    let entry_count = flattened.len();
    // Count-permitting occurrences are exactly the markers the field-loop narrowing guard matches:
    // anything present that is NOT `?` (optional) or the pedantic `1*1` (exactly-once). `*` / `+` /
    // `n*m` all qualify as tail CANDIDATES here (every final bare-type window is ultimately honored;
    // unsupported placement/shape boundaries reject below). Only `ValueMemberKey`/`TypeGroupname` carry `ge.occur`;
    // an inline group has none (never count-permitting → never a candidate → its later `* (…)`
    // narrowing rejection in the field loop stands).
    let candidate_indices: Vec<usize> = flattened
        .iter()
        .enumerate()
        .filter(|(_, (ge, _))| occurrence_permits_count(ge))
        .map(|(i, _)| i)
        .collect();
    let Some(&candidate) = candidate_indices.last() else {
        // No count-permitting entry: an ordinary closed array. Byte-identical to pre-feature output.
        return (None, None);
    };
    let src = source_rule_name_of(types, name);
    // The candidate is still skipped when refused, so no fixed field is built from it.
    if reject_row_container_placement(types, name, &src, in_choice_arm, DynamicRowShape::ArrayTail)
    {
        return (None, Some(candidate));
    }
    // Multiple count-permitting entries: only one array occurrence segment is supported. (The
    // field-loop narrowing guard additionally rejects each earlier one by design — never silent.)
    if candidate_indices.len() > 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open array supports a single trailing rest tail (`* t`), but this array \
             has {} occurrence-bearing members. Keep one `* t` member (last), use `?` for optional \
             members, or name the repeated part as its own array rule.",
            candidate_indices.len()
        ));
        return (None, Some(candidate));
    }
    // A lone count-permitting entry is a homogeneous array (`[* t]`), recognized by
    // `parse_group_type` before this runs and never reaching the record path. Keep a graceful
    // fallback for a degenerate path, but a leading segment with a fixed suffix is valid.
    if candidate == 0 && entry_count == 1 {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) must follow at least one fixed member \
             (`[ a, * t ]`). An array whose only member is `* t` is a homogeneous array — write it \
             as `[* t]` (or give it its own rule)."
        ));
        return (None, Some(candidate));
    }
    let (candidate_ge, candidate_comma) = flattened[candidate];
    // Normalize the accepted tail occurrence ONCE, at the parser boundary. `None` is loose
    // `*`/`0*`; `(1, u64::MAX)` retains the shipped NonEmptyVec ABI; every other window is the
    // complete BoundedVec carrier. This is deliberately the same normalizer and graceful u64
    // diagnostic used for homogeneous arrays and dynamic map rows -- emitters must never parse a
    // source occurrence spelling for themselves.
    let candidate_occur = group_entry_occur(candidate_ge);
    let occurrence = normalized_dynamic_sequence_occurrence_window(types, candidate_occur);
    // A member KEY on the tail entry (`* 1: uint` in array rep) is nonsense — an array tail is
    // positional. Reject rather than silently dropping the label. (An inline group never reaches here:
    // it carries no `ge.occur`, so it is never count-permitting → never a candidate.)
    if let GroupEntry::ValueMemberKey { ge, .. } = candidate_ge
        && ge.member_key.is_some()
    {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail (`* t`) is positional and cannot carry a member \
             key. Drop the `key:` label."
        ));
        return (None, Some(candidate));
    }
    let element_type = group_entry_to_type(types, parent_visitor, candidate_ge, cli);
    // A fixed-value tail element (`* 5` / `* null` / `* true`) has no Rust representation (a
    // `Vec<FixedValue>` is not a type). Reject BEFORE the homogeneous-array fixed-value panic class.
    if element_type.conceptual_type.is_fixed_value() {
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail cannot be a fixed value (`* 5`, `* null`, \
             `* true`) — there is no Rust representation for a captured tail of fixed values. Use a \
             typed element (`* uint`, `* t`) or `* any` to capture arbitrary items."
        ));
        return (None, Some(candidate));
    }
    // A PLAIN GROUP tail element (`* kv`, where `kv = (a: uint, b: uint)`). Sibling of the
    // fixed-value guard above and for the same reason: a rest tail collects one Rust value per
    // remaining array element, and a plain group is not one — it splices its members flat into the
    // enclosing array and is never materialized as a type of its own, so the tail emitter's
    // `rust_struct` lookup came back empty and generation aborted on a raw `Option::unwrap()` in
    // `encoding_var_is_copy` (and, under `--preserve-encodings` / `--wasm`, on the plain-group
    // registry assert reached earlier). Honoring the shape means a SPLICING tail that consumes the
    // group's arity worth of elements per repetition — the occurrence/bounds program's territory,
    // not a guard's — so this is a refusal, and one made honest by a remedy verified to generate.
    //
    // Guarded on `is_basic` over the RESOLVED element type — the same predicate and the same
    // one-seam placement as the record-field twin — so the bare and ALIAS (`* kv_alias`) spellings
    // land on ONE message on every profile. A TAGGED spelling (`* #6.10(kv)`) reaches the earlier
    // tag-payload semantic refusal instead. The array-WRAPPED forms keep their own (supported)
    // verdicts: `w = [kv]` is a Record, not a plain group, and an inline `* [kv]` element carries
    // `basic_override`, so neither is `is_basic`.
    if element_type.is_basic(types)
        && let ConceptualRustType::Rust(group_ident) =
            element_type.conceptual_type.resolve_alias_shallow()
    {
        // A plain group cannot be one repeated array item, so the new safe-middle discriminator
        // never applies to it. Preserve the established placement refusal for a non-final spelling:
        // it is the first semantic boundary this shape violates, and the plain-group remedy below
        // remains the final-tail diagnostic where an open tail could otherwise be recognized.
        if candidate != entry_count - 1 {
            types.record_rejection(format!(
                "rule `{src}`: an open-array rest tail (`* t`) must be the LAST member of the array (the \
                 fixed prefix is read first, then the tail captures the remaining elements). Move it to \
                 the end."
            ));
            return (None, Some(candidate));
        }
        let group_name = source_rule_name_of(types, group_ident);
        types.record_rejection(format!(
            "rule `{src}`: an open-array rest tail cannot capture the plain group `{group_name}` \
             — a plain group has no type of its own, it splices its members flat into the \
             enclosing array, while a rest tail collects ONE value per remaining element, so there \
             is nothing for it to collect. Give the group its own array framing and capture that, \
             which makes each repetition exactly ONE array element: `w = [{group_name}]`, then \
             `* w` in place of this tail. (A tag belongs on the framed reference — `* #6.10(w)` — \
             not on the group. A single MANDATORY splice of the group, `{group_name}` with no \
             occurrence, is supported as it stands.)"
        ));
        return (None, Some(candidate));
    }
    // Entry-level directives on the tail (`@name`, `@ignore`; `@duplicates` is rejected — no keys),
    // read from the row's own trailing slot (NOT rule-position handling — see `recognize_rest_row`).
    // Placement/shape guards above fire FIRST, so `@ignore` on a rejected placement gets the
    // placement rejection, not one of the directive refusals.
    let tail_metadata = group_entry_rule_metadata(candidate_ge, candidate_comma);
    let Some((semantics, field_name)) = rest_row_directives(
        types,
        cli,
        RestRowShape::ArrayTail,
        &src,
        &tail_metadata,
        occurrence.is_some(),
    ) else {
        return (None, Some(candidate));
    };
    let rest_row = RestRow {
        kind: RestKind::ArrayTail {
            element: element_type,
            source_index: candidate,
        },
        semantics,
        field_name,
        // An array tail has no keys, so no major-type dispatch and no claimed major.
        dispatch_major: None,
        occurrence: occurrence.map(crate::intermediate::RestOccurrenceWindow::from_raw),
    };
    (Some(Box::new(rest_row)), Some(candidate))
}
