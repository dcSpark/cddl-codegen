//! Named plain-group reference placement × modifier grid.
//!
//! The focused plain-group tests retain the byte, message, remedy, and guard-order contracts for
//! their incidents. This module owns the complementary denominator: every reviewed placement ×
//! modifier coordinate is either an independently generated verdict or an explicit parser/meaning
//! boundary. The accepted subset also reaches both native-only and wasm-bearing cargo checks.

use crate::cli::Cli;
use crate::tests::integration_tests::{acquire_scratch_lock, checkout_hash, codegen_cmd, tool_cmd};
use crate::tests::robustness_tests::with_thread_silenced_panics;
use clap::Parser;

#[derive(Clone, Copy)]
struct Placement {
    id: &'static str,
    representation: &'static str,
    /// The unmodified entry kind. Cells retain a more precise kind when a modifier changes it.
    entry_kind: &'static str,
    render: fn(&str, ModifierRender) -> String,
}

#[derive(Clone, Copy)]
struct Modifier {
    id: &'static str,
    render: fn(&str) -> ModifierRender,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ModifierRender {
    Bare,
    Keyed,
    Optional,
    Rest,
    Tagged,
    Alias,
}

#[derive(Clone, Copy)]
enum Verdict {
    Accept,
    Reject(&'static str),
}

#[derive(Clone, Copy)]
enum RootLiveness {
    Struct,
    Enum,
}

#[derive(Clone, Copy)]
enum CoordinateOutcome {
    Live {
        verdict: Verdict,
        /// The focused test that owns the detailed semantic contract, or the pickup probe where
        /// this grid is the first durable reader.
        evidence: &'static str,
        expected_root: Option<RootLiveness>,
    },
    /// This is still a product coordinate. It is not live only because this spelling is a
    /// different parser node or has no meaning in this placement.
    NotApplicable(&'static str),
}

#[derive(Clone, Copy)]
struct Coordinate {
    /// Stable even for N/A rows, so an axis edit cannot hide a reviewed boundary in a comment.
    id: &'static str,
    placement: &'static str,
    modifier: &'static str,
    representation: &'static str,
    /// The actual parser/entry kind for this coordinate, not a guessed neighbour's kind.
    entry_kind: &'static str,
    /// Every live row has a collision-free prefix. N/A rows never render a CDDL input.
    prefix: Option<&'static str>,
    outcome: CoordinateOutcome,
}

fn bare(_: &str) -> ModifierRender {
    ModifierRender::Bare
}
fn keyed(_: &str) -> ModifierRender {
    ModifierRender::Keyed
}
fn optional(_: &str) -> ModifierRender {
    ModifierRender::Optional
}
fn rest(_: &str) -> ModifierRender {
    ModifierRender::Rest
}
fn tagged(_: &str) -> ModifierRender {
    ModifierRender::Tagged
}
fn alias(_: &str) -> ModifierRender {
    ModifierRender::Alias
}

const MODIFIERS: &[Modifier] = &[
    Modifier {
        id: "direct",
        render: bare,
    },
    Modifier {
        id: "keyed",
        render: keyed,
    },
    Modifier {
        id: "optional",
        render: optional,
    },
    Modifier {
        id: "rest",
        render: rest,
    },
    Modifier {
        id: "tagged",
        render: tagged,
    },
    Modifier {
        id: "alias",
        render: alias,
    },
];

fn prelude(prefix: &str, modifier: ModifierRender) -> String {
    let mut spec = format!("{prefix}group = (a: uint, b: tstr)\n");
    if modifier == ModifierRender::Alias {
        spec.push_str(&format!("{prefix}group_alias = {prefix}group\n"));
    }
    spec
}

fn reference(prefix: &str, modifier: ModifierRender) -> String {
    let group = format!("{prefix}group");
    match modifier {
        ModifierRender::Tagged => format!("#6.1({group})"),
        ModifierRender::Alias => format!("{group}_alias"),
        ModifierRender::Bare
        | ModifierRender::Keyed
        | ModifierRender::Optional
        | ModifierRender::Rest => group,
    }
}

fn homogeneous_array_element(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let body = match modifier {
        ModifierRender::Bare | ModifierRender::Tagged | ModifierRender::Alias => {
            format!("[* {reference}]")
        }
        ModifierRender::Keyed => format!("[* item: {reference}]"),
        ModifierRender::Optional => format!("[? {reference}]"),
        ModifierRender::Rest => unreachable!("N/A coordinate rendered"),
    };
    format!("{}{holder} = {body}\n", prelude(prefix, modifier))
}

fn array_record_field(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let member = match modifier {
        ModifierRender::Bare | ModifierRender::Alias => reference,
        ModifierRender::Keyed => format!("item: {reference}"),
        ModifierRender::Optional => format!("? {reference}"),
        ModifierRender::Rest => format!("* {reference}"),
        ModifierRender::Tagged => reference,
    };
    format!(
        "{}{holder} = [head: uint, {member}]\n",
        prelude(prefix, modifier)
    )
}

fn struct_map_member(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let member = match modifier {
        ModifierRender::Bare | ModifierRender::Alias => reference,
        ModifierRender::Keyed | ModifierRender::Tagged => format!("item: {reference}"),
        ModifierRender::Optional => format!("? {reference}"),
        ModifierRender::Rest => format!("head: uint, * {reference} => uint"),
    };
    format!("{}{holder} = {{ {member} }}\n", prelude(prefix, modifier))
}

fn table_key(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let occurrence = match modifier {
        ModifierRender::Bare | ModifierRender::Tagged | ModifierRender::Alias => "* ",
        ModifierRender::Optional => "? ",
        ModifierRender::Keyed | ModifierRender::Rest => {
            unreachable!("N/A coordinate rendered")
        }
    };
    format!(
        "{}{holder} = {{ {occurrence}{reference} => uint }}\n",
        prelude(prefix, modifier)
    )
}

fn table_value(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let occurrence = match modifier {
        ModifierRender::Bare | ModifierRender::Tagged | ModifierRender::Alias => "* ",
        ModifierRender::Optional => "? ",
        ModifierRender::Keyed | ModifierRender::Rest => {
            unreachable!("N/A coordinate rendered")
        }
    };
    format!(
        "{}{holder} = {{ {occurrence}uint => {reference} }}\n",
        prelude(prefix, modifier)
    )
}

fn array_group_choice_arm(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let arm = match modifier {
        ModifierRender::Bare | ModifierRender::Alias | ModifierRender::Tagged => reference,
        ModifierRender::Keyed => format!("item: {reference}"),
        ModifierRender::Optional => format!("? {reference}"),
        ModifierRender::Rest => format!("* {reference}"),
    };
    format!(
        "{}{holder} = [head: uint // {arm}]\n",
        prelude(prefix, modifier)
    )
}

fn map_group_choice_arm(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    let arm = match modifier {
        ModifierRender::Bare | ModifierRender::Alias | ModifierRender::Tagged => reference,
        ModifierRender::Keyed => format!("item: {reference}"),
        ModifierRender::Optional => format!("? {reference}"),
        ModifierRender::Rest => format!("* {reference}"),
    };
    format!(
        "{}{holder} = {{ head: uint // {arm} }}\n",
        prelude(prefix, modifier)
    )
}

fn type_choice_arm(prefix: &str, modifier: ModifierRender) -> String {
    let reference = reference(prefix, modifier);
    let holder = format!("{prefix}holder");
    match modifier {
        ModifierRender::Bare | ModifierRender::Tagged | ModifierRender::Alias => {
            format!(
                "{}{holder} = {reference} / uint\n",
                prelude(prefix, modifier)
            )
        }
        ModifierRender::Keyed | ModifierRender::Optional | ModifierRender::Rest => {
            unreachable!("N/A coordinate rendered")
        }
    }
}

const PLACEMENTS: &[Placement] = &[
    Placement {
        id: "homogeneous-array-element",
        representation: "homogeneous array",
        entry_kind: "grpent.groupname (sole homogeneous element)",
        render: homogeneous_array_element,
    },
    Placement {
        id: "array-record-field-splice",
        representation: "heterogeneous array record",
        entry_kind: "grpent.groupname (array field splice)",
        render: array_record_field,
    },
    Placement {
        id: "struct-map-member",
        representation: "struct map",
        entry_kind: "grpent.groupname (map member)",
        render: struct_map_member,
    },
    Placement {
        id: "table-key",
        representation: "table map",
        entry_kind: "grpent.groupname (table key domain)",
        render: table_key,
    },
    Placement {
        id: "table-value",
        representation: "table map",
        entry_kind: "grpent.groupname (table value domain)",
        render: table_value,
    },
    Placement {
        id: "array-group-choice-arm",
        representation: "array group-choice arm",
        entry_kind: "grpent.groupname (single-entry arm)",
        render: array_group_choice_arm,
    },
    Placement {
        id: "map-group-choice-arm",
        representation: "map group-choice arm",
        entry_kind: "grpent.groupname (single-entry arm)",
        render: map_group_choice_arm,
    },
    Placement {
        id: "type-choice-arm",
        representation: "type choice",
        entry_kind: "Type2::Typename (type-choice arm)",
        render: type_choice_arm,
    },
];

const HOMOGENEOUS_OCCURRENCE: &str = "homogeneous array occurrence cannot repeat the plain group";
const TAGGED_GROUP_PAYLOAD: &str = "a CBOR tag payload cannot be the plain group";

macro_rules! live {
    ($id:literal, $placement:literal, $modifier:literal, $representation:literal, $entry:literal, $prefix:literal, $verdict:expr, $evidence:literal, $root:expr) => {
        Coordinate {
            id: $id,
            placement: $placement,
            modifier: $modifier,
            representation: $representation,
            entry_kind: $entry,
            prefix: Some($prefix),
            outcome: CoordinateOutcome::Live {
                verdict: $verdict,
                evidence: $evidence,
                expected_root: $root,
            },
        }
    };
}

macro_rules! na {
    ($id:literal, $placement:literal, $modifier:literal, $representation:literal, $entry:literal, $reason:literal) => {
        Coordinate {
            id: $id,
            placement: $placement,
            modifier: $modifier,
            representation: $representation,
            entry_kind: $entry,
            prefix: None,
            outcome: CoordinateOutcome::NotApplicable($reason),
        }
    };
}

// This is one authored 8 × 6 product. N/A cells are deliberate parser/meaning boundaries, not
// skips. Keeping them adjacent to their live neighbours makes a future grammar change require a
// reviewed table edit before it changes the denominator.
const COORDINATES: &[Coordinate] = &[
    // homogeneous-array element
    live!(
        "named-group-homogeneous-direct",
        "homogeneous-array-element",
        "direct",
        "homogeneous array",
        "grpent.groupname (sole homogeneous element)",
        "ngr_homogeneous_direct_",
        Verdict::Reject(HOMOGENEOUS_OCCURRENCE),
        "homogeneous_plain_group_occurrences_reject_the_nested_wire_rewrite",
        None
    ),
    live!(
        "named-group-homogeneous-keyed",
        "homogeneous-array-element",
        "keyed",
        "homogeneous array",
        "ValueMemberKey (homogeneous occurrence with member key)",
        "ngr_homogeneous_keyed_",
        Verdict::Reject(HOMOGENEOUS_OCCURRENCE),
        "homogeneous_plain_group_occurrences_reject_the_nested_wire_rewrite",
        None
    ),
    live!(
        "named-group-homogeneous-optional",
        "homogeneous-array-element",
        "optional",
        "homogeneous array",
        "ValueMemberKey (count-permitting occurrence)",
        "ngr_homogeneous_optional_",
        Verdict::Reject(HOMOGENEOUS_OCCURRENCE),
        "homogeneous_plain_group_occurrences_reject_the_nested_wire_rewrite",
        None
    ),
    na!(
        "named-group-homogeneous-rest",
        "homogeneous-array-element",
        "rest",
        "heterogeneous array record",
        "ValueMemberKey (final rest-tail sibling)",
        "A fixed prefix plus `* group` is the array-record rest-tail placement, not a homogeneous-array element."
    ),
    live!(
        "named-group-homogeneous-tagged",
        "homogeneous-array-element",
        "tagged",
        "homogeneous array with tagged type payload",
        "Type2::TaggedData (type sibling around resolved plain group)",
        "ngr_homogeneous_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-homogeneous-alias",
        "homogeneous-array-element",
        "alias",
        "homogeneous array",
        "grpent.groupname via transparent alias",
        "ngr_homogeneous_alias_",
        Verdict::Reject(HOMOGENEOUS_OCCURRENCE),
        "homogeneous_plain_group_occurrences_reject_the_nested_wire_rewrite",
        None
    ),
    // heterogeneous array-record field/splice
    live!(
        "named-group-array-field-direct",
        "array-record-field-splice",
        "direct",
        "heterogeneous array record",
        "grpent.groupname (array field splice)",
        "ngr_array_field_direct_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Struct)
    ),
    live!(
        "named-group-array-field-keyed",
        "array-record-field-splice",
        "keyed",
        "heterogeneous array record",
        "ValueMemberKey (keyed array field)",
        "ngr_array_field_keyed_",
        Verdict::Accept,
        "cycle25-probe",
        Some(RootLiveness::Struct)
    ),
    live!(
        "named-group-array-field-optional",
        "array-record-field-splice",
        "optional",
        "heterogeneous array record",
        "ValueMemberKey (optional array field)",
        "ngr_array_field_optional_",
        Verdict::Reject("is an OPTIONAL (`?`) reference to the plain group"),
        "optional_plain_group_array_field_rejects_gracefully_at_every_spelling",
        None
    ),
    live!(
        "named-group-array-field-rest",
        "array-record-field-splice",
        "rest",
        "heterogeneous array record",
        "ValueMemberKey (final open-array rest tail)",
        "ngr_array_field_rest_",
        Verdict::Reject("open-array rest tail cannot capture the plain group"),
        "plain_group_array_rest_tail_rejects_gracefully_at_every_spelling",
        None
    ),
    live!(
        "named-group-array-field-tagged",
        "array-record-field-splice",
        "tagged",
        "heterogeneous array record with tagged type payload",
        "Type2::TaggedData (type sibling around resolved plain group)",
        "ngr_array_field_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-array-field-alias",
        "array-record-field-splice",
        "alias",
        "heterogeneous array record",
        "grpent.groupname via transparent alias",
        "ngr_array_field_alias_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Struct)
    ),
    // struct-map member
    live!(
        "named-group-struct-map-direct",
        "struct-map-member",
        "direct",
        "struct map",
        "grpent.groupname (keyless map member)",
        "ngr_struct_map_direct_",
        Verdict::Reject("map field"),
        "cycle25-probe",
        None
    ),
    live!(
        "named-group-struct-map-keyed",
        "struct-map-member",
        "keyed",
        "struct map",
        "ValueMemberKey (keyed map member)",
        "ngr_struct_map_keyed_",
        Verdict::Reject("uses the plain group"),
        "plain_group_keyed_map_member_rejects_gracefully_at_every_spelling",
        None
    ),
    live!(
        "named-group-struct-map-optional",
        "struct-map-member",
        "optional",
        "struct map",
        "ValueMemberKey (optional keyless map member)",
        "ngr_struct_map_optional_",
        Verdict::Reject("map field"),
        "cycle25-probe",
        None
    ),
    live!(
        "named-group-struct-map-rest",
        "struct-map-member",
        "rest",
        "struct map",
        "ValueMemberKey (open struct-map rest row KEY domain)",
        "ngr_struct_map_rest_",
        Verdict::Reject("open struct-map rest row"),
        "plain_group_map_rest_row_rejects_gracefully_at_every_spelling",
        None
    ),
    live!(
        "named-group-struct-map-tagged",
        "struct-map-member",
        "tagged",
        "struct map with tagged type payload",
        "ValueMemberKey whose value is Type2::TaggedData around a resolved plain group",
        "ngr_struct_map_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-struct-map-alias",
        "struct-map-member",
        "alias",
        "struct map",
        "grpent.groupname via transparent alias",
        "ngr_struct_map_alias_",
        Verdict::Reject("map field"),
        "cycle25-probe",
        None
    ),
    // table key
    live!(
        "named-group-table-key-direct",
        "table-key",
        "direct",
        "table map",
        "grpent.groupname (table key domain)",
        "ngr_table_key_direct_",
        Verdict::Reject("as its KEY domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    na!(
        "named-group-table-key-keyed",
        "table-key",
        "keyed",
        "parser-invalid",
        "no table entry (member-key spelling rejects before `=>`)",
        "A member key before a table arrow is parser-invalid, so it never names a table key-domain group reference."
    ),
    live!(
        "named-group-table-key-optional",
        "table-key",
        "optional",
        "table map",
        "grpent.groupname (optional table key domain)",
        "ngr_table_key_optional_",
        Verdict::Reject("as its KEY domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    na!(
        "named-group-table-key-rest",
        "table-key",
        "rest",
        "table map",
        "table occurrence",
        "The table's `*` is its domain-cardinality spelling, not an array final-position rest tail; duplicating it would be a second direct coordinate."
    ),
    live!(
        "named-group-table-key-tagged",
        "table-key",
        "tagged",
        "table map with tagged key type",
        "Type2::TaggedData (key-domain type sibling around resolved plain group)",
        "ngr_table_key_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-table-key-alias",
        "table-key",
        "alias",
        "table map",
        "grpent.groupname via transparent alias (table key domain)",
        "ngr_table_key_alias_",
        Verdict::Reject("as its KEY domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    // table value
    live!(
        "named-group-table-value-direct",
        "table-value",
        "direct",
        "table map",
        "grpent.groupname (table value domain)",
        "ngr_table_value_direct_",
        Verdict::Reject("as its VALUE domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    na!(
        "named-group-table-value-keyed",
        "table-value",
        "keyed",
        "parser-invalid",
        "no table entry (member-key spelling rejects before `=>`)",
        "A member key before a table arrow is parser-invalid, so it never names a table value-domain group reference."
    ),
    live!(
        "named-group-table-value-optional",
        "table-value",
        "optional",
        "table map",
        "grpent.groupname (optional table value domain)",
        "ngr_table_value_optional_",
        Verdict::Reject("as its VALUE domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    na!(
        "named-group-table-value-rest",
        "table-value",
        "rest",
        "table map",
        "table occurrence",
        "The table's `*` is its domain-cardinality spelling, not an array final-position rest tail; duplicating it would be a second direct coordinate."
    ),
    live!(
        "named-group-table-value-tagged",
        "table-value",
        "tagged",
        "table map with tagged value type",
        "Type2::TaggedData (value-domain type sibling around resolved plain group)",
        "ngr_table_value_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-table-value-alias",
        "table-value",
        "alias",
        "table map",
        "grpent.groupname via transparent alias (table value domain)",
        "ngr_table_value_alias_",
        Verdict::Reject("as its VALUE domain"),
        "plain_group_table_domain_rejects_gracefully_at_both_spellings",
        None
    ),
    // array-representation group-choice arm
    live!(
        "named-group-array-arm-direct",
        "array-group-choice-arm",
        "direct",
        "array group-choice arm",
        "grpent.groupname (single-entry arm)",
        "ngr_array_arm_direct_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Enum)
    ),
    live!(
        "named-group-array-arm-keyed",
        "array-group-choice-arm",
        "keyed",
        "array group-choice arm",
        "ValueMemberKey (keyed single-entry arm)",
        "ngr_array_arm_keyed_",
        Verdict::Accept,
        "cycle25-probe",
        Some(RootLiveness::Enum)
    ),
    live!(
        "named-group-array-arm-optional",
        "array-group-choice-arm",
        "optional",
        "array group-choice arm",
        "ValueMemberKey (occurrence-bearing arm)",
        "ngr_array_arm_optional_",
        Verdict::Reject("carries an occurrence marker"),
        "occurrence_on_single_entry_group_choice_arm_rejects_gracefully",
        None
    ),
    live!(
        "named-group-array-arm-rest",
        "array-group-choice-arm",
        "rest",
        "array group-choice arm",
        "ValueMemberKey (occurrence-bearing arm)",
        "ngr_array_arm_rest_",
        Verdict::Reject("carries an occurrence marker"),
        "occurrence_on_single_entry_group_choice_arm_rejects_gracefully",
        None
    ),
    live!(
        "named-group-array-arm-tagged",
        "array-group-choice-arm",
        "tagged",
        "array group-choice arm with tagged type payload",
        "Type2::TaggedData (single-entry arm type sibling around resolved plain group)",
        "ngr_array_arm_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-array-arm-alias",
        "array-group-choice-arm",
        "alias",
        "array group-choice arm",
        "grpent.groupname via transparent alias",
        "ngr_array_arm_alias_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Enum)
    ),
    // map-representation group-choice arm
    live!(
        "named-group-map-arm-direct",
        "map-group-choice-arm",
        "direct",
        "map group-choice arm",
        "grpent.groupname (single-entry arm)",
        "ngr_map_arm_direct_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Enum)
    ),
    live!(
        "named-group-map-arm-keyed",
        "map-group-choice-arm",
        "keyed",
        "map group-choice arm",
        "ValueMemberKey (keyed single-entry arm)",
        "ngr_map_arm_keyed_",
        Verdict::Reject("uses the plain group"),
        "plain_group_keyed_map_member_rejects_gracefully_at_every_spelling",
        None
    ),
    live!(
        "named-group-map-arm-optional",
        "map-group-choice-arm",
        "optional",
        "map group-choice arm",
        "ValueMemberKey (zero-permitting arm occurrence)",
        "ngr_map_arm_optional_",
        Verdict::Reject("zero-permitting occurrence marker"),
        "occurrence_on_single_entry_group_choice_arm_rejects_gracefully",
        None
    ),
    live!(
        "named-group-map-arm-rest",
        "map-group-choice-arm",
        "rest",
        "map group-choice arm",
        "ValueMemberKey (zero-permitting arm occurrence)",
        "ngr_map_arm_rest_",
        Verdict::Reject("zero-permitting occurrence marker"),
        "occurrence_on_single_entry_group_choice_arm_rejects_gracefully",
        None
    ),
    live!(
        "named-group-map-arm-tagged",
        "map-group-choice-arm",
        "tagged",
        "map group-choice arm with tagged type payload",
        "Type2::TaggedData (single-entry arm type sibling around resolved plain group)",
        "ngr_map_arm_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-map-arm-alias",
        "map-group-choice-arm",
        "alias",
        "map group-choice arm",
        "grpent.groupname via transparent alias",
        "ngr_map_arm_alias_",
        Verdict::Accept,
        "alias_to_plain_group_in_array_positions_matches_the_direct_reference",
        Some(RootLiveness::Enum)
    ),
    // type-choice arm
    live!(
        "named-group-type-arm-direct",
        "type-choice-arm",
        "direct",
        "type choice",
        "Type2::Typename (type-choice arm)",
        "ngr_type_arm_direct_",
        Verdict::Reject("a type-choice arm cannot be the plain group"),
        "plain_group_type_choice_arm_rejects_gracefully_at_every_spelling",
        None
    ),
    na!(
        "named-group-type-arm-keyed",
        "type-choice-arm",
        "keyed",
        "parser-invalid",
        "no type-choice arm (member-key spelling)",
        "A member key is not grammatical in a type-choice arm."
    ),
    na!(
        "named-group-type-arm-optional",
        "type-choice-arm",
        "optional",
        "group rule sibling",
        "ValueMemberKey (group-rule entry, not a Type2 arm)",
        "`? group / other` changes the rule body into a group-entry sibling; it is not a type-choice-arm modifier."
    ),
    na!(
        "named-group-type-arm-rest",
        "type-choice-arm",
        "rest",
        "group rule sibling",
        "ValueMemberKey (group-rule entry, not a Type2 arm)",
        "`* group / other` changes the rule body into a group-entry sibling; it is not a type-choice-arm modifier."
    ),
    live!(
        "named-group-type-arm-tagged",
        "type-choice-arm",
        "tagged",
        "type choice with tagged type payload",
        "Type2::TaggedData (type-choice arm sibling around resolved plain group)",
        "ngr_type_arm_tagged_",
        Verdict::Reject(TAGGED_GROUP_PAYLOAD),
        "tagged_plain_group_payloads_reject_gracefully_at_every_placement",
        None
    ),
    live!(
        "named-group-type-arm-alias",
        "type-choice-arm",
        "alias",
        "type choice",
        "Type2::Typename via transparent alias",
        "ngr_type_arm_alias_",
        Verdict::Reject("a type-choice arm cannot be the plain group"),
        "plain_group_type_choice_arm_rejects_gracefully_at_every_spelling",
        None
    ),
];

fn placement(id: &str) -> &'static Placement {
    PLACEMENTS
        .iter()
        .find(|placement| placement.id == id)
        .expect("registry test proves placement is live")
}

fn modifier(id: &str) -> &'static Modifier {
    MODIFIERS
        .iter()
        .find(|modifier| modifier.id == id)
        .expect("registry test proves modifier is live")
}

fn cell_spec(cell: &Coordinate) -> String {
    let prefix = cell.prefix.expect("only live cells render");
    let modifier = (modifier(cell.modifier).render)(prefix);
    (placement(cell.placement).render)(prefix, modifier)
}

enum GenerationOutcome {
    Source(String),
    Error(String),
    Panic,
}

fn generate(spec: &str, tag: &str, flags: &[&str]) -> GenerationOutcome {
    let path = std::env::temp_dir().join(format!(
        "cddl_codegen_named_group_reference_{tag}_{}.cddl",
        std::process::id()
    ));
    std::fs::write(&path, spec).expect("write independent named-group cell input");
    let mut args = vec![
        "cddl-codegen",
        "--input",
        path.to_str().expect("utf8 temp path"),
        "--output",
        "named_group_reference_unused",
        "--wasm=false",
    ];
    args.extend_from_slice(flags);
    let cli = Cli::parse_from(args);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::api::generated_strings(&cli)
            .map(|files| files.into_values().collect::<Vec<_>>().join("\n"))
            .map_err(|error| error.to_string())
    }));
    std::fs::remove_file(path).ok();
    match result {
        Ok(Ok(source)) => GenerationOutcome::Source(source),
        Ok(Err(error)) => GenerationOutcome::Error(error),
        Err(_) => GenerationOutcome::Panic,
    }
}

fn expected_root(prefix: &str, root: RootLiveness) -> String {
    let holder = crate::utils::convert_to_camel_case(&format!("{prefix}holder"));
    match root {
        RootLiveness::Struct => format!("pub struct {holder}"),
        RootLiveness::Enum => format!("pub enum {holder}"),
    }
}

fn live_cells() -> impl Iterator<Item = &'static Coordinate> {
    COORDINATES.iter().filter(|cell| {
        matches!(
            cell.outcome,
            CoordinateOutcome::Live {
                verdict: _,
                evidence: _,
                expected_root: _
            }
        )
    })
}

fn accepted_cells() -> impl Iterator<Item = &'static Coordinate> {
    live_cells().filter(|cell| {
        matches!(
            cell.outcome,
            CoordinateOutcome::Live {
                verdict: Verdict::Accept,
                ..
            }
        )
    })
}

#[test]
fn named_group_reference_registries_are_complete_and_live() {
    const EXPECTED_PLACEMENTS: &[&str] = &[
        "homogeneous-array-element",
        "array-record-field-splice",
        "struct-map-member",
        "table-key",
        "table-value",
        "array-group-choice-arm",
        "map-group-choice-arm",
        "type-choice-arm",
    ];
    const EXPECTED_MODIFIERS: &[&str] = &["direct", "keyed", "optional", "rest", "tagged", "alias"];

    let mut problems = Vec::new();
    let unique = |ids: Vec<&str>, kind: &str, problems: &mut Vec<String>| {
        let mut seen = std::collections::BTreeSet::new();
        for id in ids {
            if !seen.insert(id) {
                problems.push(format!("duplicate {kind} id `{id}`"));
            }
        }
    };
    unique(
        PLACEMENTS.iter().map(|placement| placement.id).collect(),
        "placement",
        &mut problems,
    );
    unique(
        MODIFIERS.iter().map(|modifier| modifier.id).collect(),
        "modifier",
        &mut problems,
    );
    unique(
        COORDINATES.iter().map(|cell| cell.id).collect(),
        "coordinate",
        &mut problems,
    );

    let actual_placements = PLACEMENTS
        .iter()
        .map(|placement| placement.id)
        .collect::<std::collections::BTreeSet<_>>();
    let expected_placements = EXPECTED_PLACEMENTS
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    if actual_placements != expected_placements {
        problems.push(format!(
            "placement axis changed: expected {expected_placements:?}, got {actual_placements:?}"
        ));
    }
    let actual_modifiers = MODIFIERS
        .iter()
        .map(|modifier| modifier.id)
        .collect::<std::collections::BTreeSet<_>>();
    let expected_modifiers = EXPECTED_MODIFIERS
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    if actual_modifiers != expected_modifiers {
        problems.push(format!(
            "modifier axis changed: expected {expected_modifiers:?}, got {actual_modifiers:?}"
        ));
    }

    for placement in PLACEMENTS {
        if placement.representation.trim().is_empty() || placement.entry_kind.trim().is_empty() {
            problems.push(format!(
                "placement `{}` lacks rep/entry-kind evidence",
                placement.id
            ));
        }
    }

    let mut product = std::collections::BTreeSet::new();
    let mut prefixes = std::collections::BTreeSet::new();
    for cell in COORDINATES {
        if !actual_placements.contains(cell.placement) {
            problems.push(format!(
                "coordinate `{}` names dead placement `{}`",
                cell.id, cell.placement
            ));
        }
        if !actual_modifiers.contains(cell.modifier) {
            problems.push(format!(
                "coordinate `{}` names dead modifier `{}`",
                cell.id, cell.modifier
            ));
        }
        if !product.insert((cell.placement, cell.modifier)) {
            problems.push(format!(
                "duplicate coordinate for {} × {}",
                cell.placement, cell.modifier
            ));
        }
        if cell.representation.trim().is_empty() || cell.entry_kind.trim().is_empty() {
            problems.push(format!(
                "coordinate `{}` lacks rep/entry-kind evidence",
                cell.id
            ));
        }
        match cell.outcome {
            CoordinateOutcome::Live {
                verdict,
                evidence,
                expected_root: root_liveness,
            } => {
                if cell.prefix.is_none_or(|prefix| prefix.trim().is_empty()) {
                    problems.push(format!("live coordinate `{}` lacks a prefix", cell.id));
                } else if !prefixes.insert(cell.prefix.expect("checked as present")) {
                    problems.push(format!(
                        "live coordinate `{}` reuses prefix `{}`",
                        cell.id,
                        cell.prefix.expect("checked as present")
                    ));
                }
                if evidence.trim().is_empty() {
                    problems.push(format!("live coordinate `{}` lacks evidence", cell.id));
                }
                if let Verdict::Reject(fragment) = verdict
                    && fragment.trim().is_empty()
                {
                    problems.push(format!(
                        "rejection coordinate `{}` has an empty diagnostic fragment",
                        cell.id
                    ));
                }
                if matches!(verdict, Verdict::Accept) && root_liveness.is_none() {
                    problems.push(format!(
                        "accepted coordinate `{}` lacks an emitted-root liveness predicate",
                        cell.id
                    ));
                }
                if matches!(verdict, Verdict::Reject(_)) && root_liveness.is_some() {
                    problems.push(format!(
                        "rejection coordinate `{}` cannot have an emitted-root predicate",
                        cell.id
                    ));
                }
            }
            CoordinateOutcome::NotApplicable(reason) => {
                if reason.trim().is_empty() {
                    problems.push(format!("N/A coordinate `{}` has an empty reason", cell.id));
                }
                if cell.prefix.is_some() {
                    problems.push(format!(
                        "N/A coordinate `{}` has a rendered prefix",
                        cell.id
                    ));
                }
            }
        }
    }
    for placement in PLACEMENTS {
        for modifier in MODIFIERS {
            if !product.contains(&(placement.id, modifier.id)) {
                problems.push(format!(
                    "missing coordinate for {} × {}",
                    placement.id, modifier.id
                ));
            }
        }
        if !live_cells().any(|cell| cell.placement == placement.id) {
            problems.push(format!("placement `{}` has no live cell", placement.id));
        }
    }
    for modifier in MODIFIERS {
        if !live_cells().any(|cell| cell.modifier == modifier.id) {
            problems.push(format!("modifier `{}` has no live cell", modifier.id));
        }
    }
    assert_eq!(
        COORDINATES.len(),
        PLACEMENTS.len() * MODIFIERS.len(),
        "the coordinate table must cover the exact placement × modifier product"
    );
    let live_count = live_cells().count();
    assert_eq!(
        live_count, 40,
        "the reviewed product has 40 independently generated coordinates; revise this explicit \
         partition with the table when a grammar or meaning boundary changes"
    );
    assert_eq!(
        COORDINATES.len() - live_count,
        8,
        "the reviewed product has 8 explicit parser/meaning or duplicate-placement N/A coordinates"
    );
    assert!(
        problems.is_empty(),
        "named-group reference registry failures:\n{}",
        problems.join("\n")
    );
}

#[test]
fn named_group_reference_live_cells_match_authored_verdicts() {
    let mut failures = Vec::new();
    with_thread_silenced_panics(|| {
        for cell in live_cells() {
            let CoordinateOutcome::Live {
                verdict,
                expected_root: root_liveness,
                ..
            } = cell.outcome
            else {
                unreachable!("live_cells filters N/A rows")
            };
            match (verdict, generate(&cell_spec(cell), cell.id, &[])) {
                (Verdict::Accept, GenerationOutcome::Source(source)) => {
                    let expected = root_liveness
                        .map(|root| expected_root(cell.prefix.expect("live prefix"), root))
                        .expect("accepted rows carry liveness");
                    if !source.contains(&expected) {
                        failures.push(format!(
                            "[{}: {} × {}] authored Accept emitted no `{expected}`; the cell-local \\
                             holder/root may have been silently omitted",
                            cell.id, cell.placement, cell.modifier
                        ));
                    }
                }
                (Verdict::Accept, GenerationOutcome::Error(error)) => failures.push(format!(
                    "[{}: {} × {}] authored Accept but generation gracefully refused:\n{error}",
                    cell.id, cell.placement, cell.modifier
                )),
                (Verdict::Accept, GenerationOutcome::Panic) => failures.push(format!(
                    "[{}: {} × {}] authored Accept but generation PANICKED",
                    cell.id, cell.placement, cell.modifier
                )),
                (Verdict::Reject(expected), GenerationOutcome::Error(actual))
                    if actual.contains(expected) => {}
                (Verdict::Reject(expected), GenerationOutcome::Source(_)) => failures.push(format!(
                    "[{}: {} × {}] authored Reject({expected:?}) but generation succeeded",
                    cell.id, cell.placement, cell.modifier
                )),
                (Verdict::Reject(expected), GenerationOutcome::Error(actual)) => failures.push(format!(
                    "[{}: {} × {}] authored Reject({expected:?}) but got another graceful diagnostic:\n{actual}",
                    cell.id, cell.placement, cell.modifier
                )),
                (Verdict::Reject(expected), GenerationOutcome::Panic) => failures.push(format!(
                    "[{}: {} × {}] authored Reject({expected:?}) but generation PANICKED",
                    cell.id, cell.placement, cell.modifier
                )),
            }
        }
    });
    assert!(
        failures.is_empty(),
        "named-group reference grid failures:\n\n{}",
        failures.join("\n\n")
    );
}

/// Each accepted coordinate first gets the independent generation/liveness verdict above, then
/// contributes to one collision-free crate per face. No generated source is hand-edited.
#[test]
fn named_group_reference_accepted_cells_compile() {
    if std::process::Command::new("cargo")
        .arg("--version")
        .output()
        .is_err()
    {
        return;
    }
    let cells = accepted_cells().collect::<Vec<_>>();
    let cell_ids = cells
        .iter()
        .map(|cell| cell.id)
        .collect::<Vec<_>>()
        .join(", ");
    let scratch_name = format!(
        "cddl_codegen_named_group_reference_{:016x}",
        checkout_hash()
    );
    let _scratch_lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create named-group reference scratch root");
    let input = root.join("all-accepted.cddl");
    let spec = cells.iter().map(|cell| cell_spec(cell)).collect::<String>();
    std::fs::write(&input, spec).expect("write accepted named-group product spec");

    for (profile, wasm, faces) in [
        ("rust-only", "--wasm=false", &["rust"][..]),
        ("wasm-bearing", "--wasm=true", &["rust", "wasm"][..]),
    ] {
        let output = root.join(profile);
        let generated = codegen_cmd()
            .args([
                "--input",
                input.to_str().expect("utf8 input"),
                "--output",
                output.to_str().expect("utf8 output"),
                wasm,
                "--static-dir",
                concat!(env!("CARGO_MANIFEST_DIR"), "/static"),
            ])
            .output()
            .expect("spawn generator for named-group accepted batch");
        assert!(
            generated.status.success(),
            "{profile}: generation failed for accepted cells [{cell_ids}]:\n{}\n{}",
            String::from_utf8_lossy(&generated.stdout),
            String::from_utf8_lossy(&generated.stderr)
        );
        for face in faces {
            let checked = tool_cmd("cargo")
                .arg("check")
                .current_dir(output.join(face))
                // Each profile has its own target tree, so same-name packages never reuse a
                // sibling profile's artifact.
                .env("CARGO_TARGET_DIR", output.join("target").join(face))
                .output()
                .expect("spawn cargo check for named-group accepted batch");
            assert!(
                checked.status.success(),
                "{profile}/{face}: cargo check failed for accepted cells [{cell_ids}]:\n{}\n{}",
                String::from_utf8_lossy(&checked.stdout),
                String::from_utf8_lossy(&checked.stderr)
            );
        }
    }
    let _ = std::fs::remove_dir_all(root);
}
