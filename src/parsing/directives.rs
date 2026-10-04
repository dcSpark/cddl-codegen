//! Shared directive settlement and placement checks.
//! Keep directive ordering and rule/member ownership exact.

use super::comments::combine_comments;
use super::{EXTERN_DEPS_DIR, EXTERN_MARKER, RAW_BYTES_MARKER};
use super::{flatten_group_entries, group_entry_to_raw_field_name, source_rule_name_of};
use crate::comment_ast::{
    Directive, DuplicatesPolicy, RuleMetadata, merge_metadata, metadata_from_comments,
};
use crate::intermediate::{
    AliasIdent, ConceptualRustType, IntermediateTypes, Representation, RustIdent, RustType,
    reserved_pin_rejection,
};
use crate::utils::convert_to_camel_case;
use cddl::ast::{
    Comments, Group, GroupChoice, GroupEntry, OptionalComma, Type1, Type2, TypeChoice,
};

/// `@duplicates` on a rule where the policy can NEVER apply — a non-collection rule (a text/int
/// alias, a struct/group/record, a union, an extern marker, …). Permanent graceful rejection in the
/// house style of the other comment-DSL misuse rejections (`@raw_bytes_flavor`), never a panic and
/// never a silent no-op.
pub(super) fn reject_duplicates_not_applicable(types: &mut IntermediateTypes, name: &RustIdent) {
    let source_name = source_rule_name_of(types, name);
    types.record_rejection(format!(
        "@duplicates on rule `{source_name}`: this directive only applies to set/array collection \
         rules (`[* a]` / `[+ a]`, including the tag-258 set idiom) and table rules \
         (`{{ * k => v }}`); a union's map arm must be a named rule to carry it. Remove it from \
         this rule."
    ));
}

/// Reject a misplaced `@ignore`. The tolerate-and-drop flavor is valid ONLY on a recognized open
/// struct-map rest row (`* k => v` after fixed keys), where it is read from the ENTRY-trailing
/// comment slot — never from a rule's or field's metadata. So any `@ignore` reaching a rule/field
/// metadata consumer is a misplacement, rejected loudly (never silently dropped), naming the one
/// valid placement.
pub(super) fn reject_ignore_not_applicable(types: &mut IntermediateTypes, name: &RustIdent) {
    let source_name = source_rule_name_of(types, name);
    types.record_rejection(format!(
        "@ignore on rule `{source_name}`: this directive is only valid on an open struct-map rest \
         row (`* k => v ; @ignore`) or an open-array rest tail (`* t ; @ignore`), written in the \
         ROW's / TAIL's own trailing comment on its own line inside the container, where it selects \
         the tolerate-and-drop flavor. It does not apply at a rule, alias, union, table, \
         whole-array, or field position. Remove it, or move it onto the `* k => v` row / `* t` tail \
         of an open struct-map / open array."
    ));
}

/// `@newtype` on a rule that already generates its own named type (a record struct or a
/// group-choice enum): there is no transparent alias for the directive to turn into a wrapper.
pub(super) fn reject_newtype_on_nominal_rule(
    types: &mut IntermediateTypes,
    name: &RustIdent,
    shape: &str,
) {
    let source_name = source_rule_name_of(types, name);
    types.record_rejection(format!(
        "@newtype on `{source_name}`: {shape} already generates its own named type, so the \
         directive has no inner type to wrap. Remove @newtype."
    ));
}

/// Strip the `Alias` node a TRANSPARENT-ALIAS REGISTRATION cannot store, carrying the stripped
/// rule's wire-codec metadata across the strip. Returns the stripped base and the rule the metadata
/// was inherited from (`None` when nothing was inherited).
///
/// The strip is mandatory at a registration seam and nowhere else: `resolve_alias` re-wraps an
/// entry's `base_type` in `Alias(<this rule>, …)` on the way out, so a stored `Alias` would
/// double-wrap, and `register_type_alias` refuses one for that reason. The member/arm and WRAPPER
/// seams keep their node instead, because the node is the ONLY thing
/// `generate_serialize`/`generate_deserialize`'s `Alias` arms lift an aliased rule's
/// `@custom_serialize`/`@custom_deserialize` pair from: a stripped node silently re-derives the
/// built-in wire, and one CDDL type ends up with two wire forms in one crate depending on which name
/// reached it. Where the node cannot survive, the FACTS travel instead — the registering rule's own
/// metadata answers the emitter's ident lookup exactly as the stripped rule's would have.
///
/// The wire-facts family moves as ONE declaration or not at all. `@custom_encodings` and
/// `@custom_wire_major` are legal only beside the pair, so a rule that writes its own pair also
/// writes (or deliberately omits) its own framing facts and inherits nothing — outer wins, whole.
/// Naming, doc and structural directives never travel: they describe the rule they are written on.
///
/// Chains cascade because each link inherits at its OWN registration and the rule graph registers a
/// rule after everything it references (`dep_graph::topological_rule_order` pushes in DFS post-order,
/// so source order is irrelevant), which is what
/// `dsl_position_tests::transparent_realias_and_member_agree_on_the_aliass_custom_pair` pins with a
/// deliberately adversarial declaration order.
pub(super) fn strip_alias_for_registration(
    types: &IntermediateTypes,
    mut base_type: RustType,
    rule_metadata: &mut RuleMetadata,
) -> (RustType, Option<AliasIdent>) {
    let ConceptualRustType::Alias(stripped_ident, inner) = base_type.conceptual_type else {
        return (base_type, None);
    };
    base_type.conceptual_type = *inner;
    // Outer wins: a rule with its own pair describes its own wire completely.
    if rule_metadata.custom_serialize.is_some() || rule_metadata.custom_deserialize.is_some() {
        return (base_type, None);
    }
    let Some(source) = types.type_aliases().get(&stripped_ident) else {
        return (base_type, None);
    };
    let Some(source_metadata) = source.rule_metadata.as_ref() else {
        return (base_type, None);
    };
    if source_metadata.custom_serialize.is_none() && source_metadata.custom_deserialize.is_none() {
        return (base_type, None);
    }
    rule_metadata.custom_serialize = source_metadata.custom_serialize.clone();
    rule_metadata.custom_deserialize = source_metadata.custom_deserialize.clone();
    rule_metadata.custom_encodings = source_metadata.custom_encodings.clone();
    rule_metadata.custom_wire_major = source_metadata.custom_wire_major;
    // The ORIGIN, not the previous link: the author's rule is what a no-silent-directive check has
    // to be able to reach in one hop.
    let origin = source
        .wire_metadata_inherited_from
        .clone()
        .unwrap_or(stripped_ident);
    (base_type, Some(origin))
}

/// The `@custom_serialize` / `@custom_deserialize` directive names present in `metadata`, in a
/// stable order. Every placement rejection for the pair reports it one directive at a time, so the
/// message names exactly the spelling the author wrote (and both fire when both are present).
fn custom_codec_directives(metadata: &RuleMetadata) -> Vec<&'static str> {
    let mut found = Vec::new();
    if metadata.custom_serialize.is_some() {
        found.push("@custom_serialize");
    }
    if metadata.custom_deserialize.is_some() {
        found.push("@custom_deserialize");
    }
    found
}

/// The custom-codec pair plus the declarations that describe its wire (`@custom_encodings`,
/// `@custom_wire_major`), in that order.
fn custom_codec_family_directives(metadata: &RuleMetadata) -> Vec<&'static str> {
    let mut found = custom_codec_directives(metadata);
    if metadata.custom_encodings.is_some() {
        found.push("@custom_encodings");
    }
    if metadata.custom_wire_major.is_some() {
        found.push("@custom_wire_major");
    }
    found
}

/// Directive spellings for a diagnostic: each backtick-quoted, joined by `sep`.
pub(super) fn quoted_directive_list(directives: &[&str], sep: &str) -> String {
    directives
        .iter()
        .map(|directive| format!("`{directive}`"))
        .collect::<Vec<_>>()
        .join(sep)
}

/// An exact-zero keyed member (`0*0` / `*0`) deliberately mints no value field: it is record
/// constraint metadata only, used to reject that one CBOR/JSON key and to guard open-map
/// construction.  Most member-scoped directives already take their normal placement refusal
/// before this seam.  These four are ordinarily CONSUMED by an emitted value field, however, so
/// letting them reach the exact-zero early-return would silently drop their only effect.
///
/// `@name` remains meaningful: it names the forbidden JSON property, and hence is retained in
/// `ForbiddenField`.  The rule's own `@doc` remains the place to document the constraint.
pub(super) fn reject_exact_zero_field_only_metadata(
    types: &mut IntermediateTypes,
    field_name: &str,
    record_name: &RustIdent,
    metadata: &RuleMetadata,
    field_type: &RustType,
) {
    let source_name = source_rule_name_of(types, record_name);
    if metadata.doc.is_some() {
        types.record_rejection(format!(
            "@doc on exact-zero field `{field_name}` of rule `{source_name}`: `0*0` / `*0` \
             declares a forbidden key and emits no value field for this doc comment to document. \
             Put the explanation on the enclosing rule (`{source_name} = {{ … }} ; @doc <text>`), \
             or remove the member doc."
        ));
    }
    let codec_directives = custom_codec_family_directives(metadata);
    if !codec_directives.is_empty() {
        let written = quoted_directive_list(&codec_directives, " / ");
        types.record_rejection(format!(
            "{written} on exact-zero field `{field_name}` of rule `{source_name}`: `0*0` / `*0` \
             forbids the member completely and emits no value codec or encoding sidecar for these \
             directives to replace. Remove the codec declaration; a custom codec cannot make a \
             forbidden member constructible."
        ));
    }
    if field_type.config.default.is_some() {
        types.record_rejection(format!(
            "`.default` on exact-zero field `{field_name}` of rule `{source_name}` has no absent \
             VALUE to substitute: `0*0` / `*0` forbids this key rather than making a value optional. \
             Remove `.default`, or use `? {field_name}: …` when an absent value should receive a \
             default."
        ));
    }
}

/// The RULE-SCOPED directives, refused at a MEMBER position — the one list of "which directives
/// does a member position refuse, and with what remedy", shared by every member position rather
/// than restated at each. Two positions call it today: the record field walk
/// (`parse_record_from_group_choice`) and the SINGLE-ENTRY group-choice arm
/// (`reject_field_directives_on_single_entry_arm`), which mints no record and so never reaches the
/// field walk at all. Sharing is what makes "the arm validates a member's directives the way the
/// field walk does" a fact rather than a resemblance: a directive added to this list is refused at
/// both, and neither can quietly grow a position-specific verdict for one of them.
///
/// The list has two halves. The one written out below is the half a ROW-ENTRY slot cannot take
/// wholesale (`@ignore` and `@duplicates` are honored there), so the row seams call the other half —
/// [`reject_type_scoped_directives`] — on its own.
///
/// `site` names the slot the way that slot's other rejections do (`field \`f\` of rule \`t\``), and
/// `position_noun` is the phrase the three "…, not X" messages end on — the only text that varies
/// between positions, because the other three already word themselves position-generically
/// (`a field/member position`).
///
/// `rule_slot_shared` marks the ONE member slot the cddl parser also binds a RULE's trailing comment
/// to — a plain group rule's LAST entry — and suppresses only the [`reject_type_scoped_directives`]
/// half; see that function's doc comment for why the split is exactly there.
pub(super) fn reject_member_scoped_directives(
    types: &mut IntermediateTypes,
    site: &str,
    position_noun: &str,
    metadata: &RuleMetadata,
    rule_slot_shared: bool,
) {
    // `@raw_bytes_flavor` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition, never a
    // field/member position — reject loudly instead of silently ignoring it.
    if metadata.raw_bytes_flavor {
        types.record_rejection(format!(
            "@raw_bytes_flavor on {site}: this tag is only valid on a {EXTERN_MARKER} rule \
             definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@used_as_elem` names the TYPE whose loose-list wasm wrapper to mint, so it is rule-scoped and
    // never applies at a field/member position — reject loudly instead of silently dropping it.
    // Honoring it here would need a sub-ruling per member shape (an optional field, an inline
    // `[* x]`, a primitive): the tag is only unambiguous when the field's type is a bare named
    // reference. The remedy is exact and already proven, so refuse and name it.
    if metadata.used_as_elem {
        types.record_rejection(format!(
            "@used_as_elem on {site}: this directive is rule-scoped — it mints the loose-list wasm \
             wrapper for the TYPE it tags — and does not apply to a field/member position. Put it \
             on the rule that defines the element type (`<type> = … ; @used_as_elem`)."
        ));
    }
    // `@copy` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_` rule
    // definition, never a field/member position — reject loudly instead of ignoring it.
    if metadata.copy {
        types.record_rejection(format!(
            "@copy on {site}: this tag is only valid on a {EXTERN_MARKER} or {RAW_BYTES_MARKER} \
             rule definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@extern_companions` only applies to a `_CDDL_CODEGEN_EXTERN_TYPE_` rule definition, never a
    // field/member position — reject loudly instead of silently ignoring it. This is also the slot a
    // plain-GROUP rule's TRAILING comment binds to (`grp = (a: uint) ; @extern_companions …`, the
    // `@name plain-group-trailing` seam), so it covers that spelling too.
    if metadata.extern_companions.is_some() {
        types.record_rejection(format!(
            "@extern_companions on {site}: this tag is only valid on a {EXTERN_MARKER} rule \
             definition, not {position_noun}. Remove it from this entry."
        ));
    }
    // `@duplicates` is per-rule and never applies at a field/member position — reject loudly instead
    // of silently ignoring it. The remedy names the collection as its own rule.
    if metadata.duplicates.is_some() {
        types.record_rejection(format!(
            "@duplicates on {site}: this directive is per-rule and does not apply to a field/member \
             position. Name the collection as its own rule and put `; @duplicates \
             <preserve|reject>` on that rule. (An inline `#6.258` array in this position already \
             defaults to `@duplicates reject` via the well-known-tag registry — hoisting it to a \
             named rule with `; @duplicates preserve` is exactly how to opt out.)"
        ));
    }
    // `@ignore` is the open struct-map rest-row tolerate-and-drop flavor and never applies at a
    // field/member position — reject loudly instead of silently ignoring it.
    if metadata.ignore {
        types.record_rejection(format!(
            "@ignore on {site}: this directive is only valid on an open struct-map rest row, \
             written in the ROW's own trailing comment (`* k => v ; @ignore`, on the row's own line \
             inside the braces), not at a field/member position. Remove it from this entry."
        ));
    }
    if !rule_slot_shared {
        reject_type_scoped_directives(types, site, metadata);
    }
}

/// The TYPE-SCOPED directives, refused at a MEMBER position. Split out of
/// [`reject_member_scoped_directives`] rather than folded into its list because the two halves have
/// different reach: these six describe the TYPE a name denotes (how it lowers, what it derives, what
/// the json faces do with it), so no member position reads them and every member position refuses
/// them, while the other half includes directives a row-entry slot legitimately HONORS
/// (`@ignore`, `@duplicates`). A row seam can therefore call this one alone.
///
/// The six were read into a member's `RuleMetadata` and dropped at exit 0 — `@newtype` on an
/// ordinary array-record field, `@used_as_key` on a map-record field, and their four siblings, all
/// byte-identical to the undirected spec under default, `--preserve-encodings` and `--wasm`.
///
/// Deliberately NOT called for the one member slot the cddl parser also binds a RULE's trailing
/// comment to: a plain group rule's LAST entry (`pg = (a: uint, b: uint) ; @used_as_key`). That
/// slot is the group rule's documented directive slot — `@used_as_key` there IS honored at rule
/// level, and `comment_dsl.mdx` documents the dual read — so refusing it here would refuse the rule
/// position through the member seam. The OTHER half still fires there, unchanged: a plain group's
/// trailing `@duplicates` has always been refused by member site (`field \`b\` of rule \`pg\``).
/// Non-last plain-group entries carry no rule reading and refuse like any member.
///
/// `site` names the slot the way that slot's other rejections do. Each remedy names the rule that
/// defines the member's type, because that is where the directive's own reader lives.
pub(super) fn reject_type_scoped_directives(
    types: &mut IntermediateTypes,
    site: &str,
    metadata: &RuleMetadata,
) {
    // `@rust_name` pins the FINAL derived Rust type name of a rule in an extern-deps scope, so it is
    // keyed on a rule's type and has no member reading at all.
    if metadata.rust_name.is_some() {
        types.record_rejection(format!(
            "@rust_name on {site}: this directive is rule-scoped — it pins the final Rust type name \
             of a rule in a {EXTERN_DEPS_DIR} scope — and does not apply to a field/member \
             position. Put it on the rule that defines the member's type (`<type> = … ; @rust_name \
             <Pinned>`)."
        ));
    }
    // `@newtype` asks a rule that would lower to a transparent alias to mint a wrapper struct
    // instead. A member declares no rule, so there is no lowering here to change.
    if metadata.newtype.is_some() {
        types.record_rejection(format!(
            "@newtype on {site}: this directive is rule-scoped — it makes a rule mint a wrapper \
             struct instead of a transparent alias — and does not apply to a field/member position. \
             Put it on the rule that defines the member's type (`<type> = … ; @newtype`)."
        ));
    }
    // `@no_alias` suppresses a rule's own `pub type` line. A member emits no alias line.
    if metadata.no_alias {
        types.record_rejection(format!(
            "@no_alias on {site}: this directive is rule-scoped — it suppresses the `pub type` line \
             a rule of its own emits — and does not apply to a field/member position. Put it on the \
             rule that defines the member's type (`<type> = … ; @no_alias`)."
        ));
    }
    // `@used_as_key` is the Ord/Hash derive demand on a TYPE; the derives land on that type's
    // definition, never on a position that uses it.
    if metadata.key_demand.is_some() {
        types.record_rejection(format!(
            "@used_as_key on {site}: this directive is rule-scoped — it demands the comparison \
             derives on the TYPE it tags — and does not apply to a field/member position. Put it on \
             the rule that defines the member's type (`<type> = … ; @used_as_key`)."
        ));
    }
    // `@custom_json` suppresses the serde/schemars derives on a TYPE.
    if metadata.custom_json {
        types.record_rejection(format!(
            "@custom_json on {site}: this directive is rule-scoped — it suppresses the \
             serde/schemars derives on the TYPE it tags — and does not apply to a field/member \
             position. Put it on the rule that defines the member's type (`<type> = … ; \
             @custom_json`)."
        ));
    }
    // `@no_json_schema_export` suppresses a TYPE's schema-registration row in the json-gen crate.
    if metadata.no_json_schema_export {
        types.record_rejection(format!(
            "@no_json_schema_export on {site}: this directive is rule-scoped — it suppresses the \
             json-gen schema-registration row of the TYPE it tags — and does not apply to a \
             field/member position. Put it on the rule that defines the member's type (`<type> = … \
             ; @no_json_schema_export`)."
        ));
    }
}

/// Reject a `@custom_encodings` declaration that is not accompanied by BOTH halves of the pair it
/// describes, at the same position. Returns whether anything was rejected.
///
/// The declaration states the codec-visible encoding variables of the wire a
/// `@custom_serialize`/`@custom_deserialize` pair writes and reads, so it is meaningful only where
/// that pair is. With ONE half the other direction is generated code deriving the REPLACED type's
/// encoding demand, which declared slots would contradict by construction (one side would pass
/// what the other never binds — E0061/E0308 in the generated crate, if it were honored at all).
/// With NO half there is no codec to describe and the declaration would be read into the position's
/// metadata and dropped — the silent-drop class this DSL rejects everywhere.
///
/// `position` names the slot the way that slot's other rejections do.
pub(super) fn reject_custom_encodings_without_pair(
    types: &mut IntermediateTypes,
    position: &str,
    metadata: &RuleMetadata,
) -> bool {
    let declarations: [(&str, bool, &str); 2] = [
        (
            "@custom_encodings",
            metadata.custom_encodings.is_some(),
            "@custom_serialize <fn> @custom_deserialize <fn> @custom_encodings <kinds>",
        ),
        (
            "@custom_wire_major",
            metadata.custom_wire_major.is_some(),
            "@custom_serialize <fn> @custom_deserialize <fn> @custom_wire_major <major>",
        ),
    ];
    if !declarations.iter().any(|(_, written, _)| *written) {
        return false;
    }
    let present = custom_codec_directives(metadata);
    if present.len() == 2 {
        return false;
    }
    let found = if present.is_empty() {
        "no `@custom_serialize`/`@custom_deserialize` is written there".to_owned()
    } else {
        format!("only `{}` is written there", present[0])
    };
    let mut rejected = false;
    for (directive, written, remedy) in declarations {
        if !written {
            continue;
        }
        types.record_rejection(format!(
            "{directive} on {position}: the declaration describes the wire of the custom \
             (de)serializer pair written BESIDE it, and {found}. Both halves are required: with one \
             half the other direction is generated code deriving the replaced type\u{2019}s own \
             inferred facts, which the declaration would contradict. Write the pair here \
             (`{remedy}`), or drop the declaration."
        ));
        rejected = true;
    }
    rejected
}

/// Reject a custom (de)serializer pair sitting in a collection ROW-ENTRY comment slot — a table row,
/// an open struct-map rest row, an open-array rest tail. The pair is a TYPE-level override keyed on
/// the type whose codec it replaces, and a row entry declares no type of its own, so the directives
/// are read into the row's `RuleMetadata` and dropped. (`@name`, `@duplicates` and `@ignore` are the
/// spellings that slot legitimately carries — they are row-scoped by construction, which is exactly
/// what the pair is not.) `position` names the row the way that row's other rejections do (the named
/// rows spell themselves `<shape> of rule <src>`, an anonymous inline table has no rule to name) and
/// `remedy` names the rule to move the pair onto. Returns whether anything was rejected.
pub(super) fn reject_custom_codec_on_row_entry(
    types: &mut IntermediateTypes,
    position: &str,
    remedy: &str,
    metadata: &RuleMetadata,
) -> bool {
    let found = custom_codec_directives(metadata);
    for directive in &found {
        types.record_rejection(format!(
            "{directive} on the {position}: the custom (de)serializer pair is a \
             TYPE-level override keyed on the type whose codec it replaces, and a row entry \
             declares no type of its own, so it is not honored in this slot. {remedy}"
        ));
    }
    !found.is_empty()
}

/// Read the ROW-ENTRY comment slot of an ANONYMOUS inline table (`{ * k => v ; @directive }` used as
/// a member, element, `.cbor` payload or type-choice-arm type) and apply what it declares to the map
/// type just built for it.
///
/// The anonymous twin of a NAMED table's row slot (`register_rust_struct`'s `HomogenousMap` arm):
/// the same entry, read the same way. What the two slots DO with `@duplicates` differs by design and
/// leaves exactly one honored spelling per shape — an anonymous table has no rule slot, so its row
/// is where the policy has to live; a named table has one, so its row REJECTS the directive and
/// points there. `@duplicates preserve` here swaps the member to the `PairMap`/`NonEmptyPairMap`
/// vec-of-pairs twin, the same representation a named table's rule slot selects. An explicit
/// `reject` is that policy's accepted default (a loose table is key-unique by construction), but
/// is still stored exactly as a named table stores it. The WIT boundary decides whether a reject
/// policy carries an invariant from both policy and resolved container shape, so a table remains a
/// plain map rather than being mistaken for an `OrderedSet` despecialization.
///
/// Every other directive the slot can carry declares something a row entry has no place for, so it
/// is rejected with the spelling that works: nothing written here is accepted-and-inert.
///
/// SCOPE — inline shapes exactly. A NAMED table referenced by name never reaches this seam
/// (`Type2::Typename` handles it), so a per-site policy can never fork a shared type's identity,
/// which is why the general member-position `@duplicates` on named references stays unsupported.
pub(super) fn apply_inline_table_row_metadata(
    types: &mut IntermediateTypes,
    group_choice: &GroupChoice,
    map_type: RustType,
) -> RustType {
    let entries = flatten_group_entries(&group_choice.group_entries, Representation::Map);
    // A parenthesized row (`{ * (k => v) }`) has no entry slot of its own, and
    // `group_entry_rule_metadata` panics on an `InlineGroup` — the guard the named seam takes.
    // Probed: the cddl AST binds a comment written there to NOTHING this seam can reach (the
    // `InlineGroup` variant carries no trailing-comment field and the entry's `OptionalComma` comes
    // back `trailing_comments: None`), so there is no directive here to honor OR to reject — the
    // spelling that carries one is the unparenthesized row.
    let [(row_ge, row_comma)] = entries[..] else {
        return map_type;
    };
    if matches!(row_ge, GroupEntry::InlineGroup { .. }) {
        return map_type;
    }
    let metadata = group_entry_rule_metadata(row_ge, row_comma);
    let position = "row entry of an inline table (`{ * k => v }`)";
    reject_custom_codec_on_row_entry(
        types,
        position,
        "Name the table's key or value type as its own rule and put the pair there (`k = text ; \
         @custom_serialize <fn> @custom_deserialize <fn>`, then `{ * k => v }`).",
        &metadata,
    );
    // …and a `@custom_encodings` declaration with no pair to describe is dropped the same way.
    reject_custom_encodings_without_pair(types, &format!("the {position}"), &metadata);
    reject_inert_inline_table_row_directives(types, position, &metadata);
    if let Some(policy) = metadata.duplicates {
        map_type.with_duplicates_policy(Some(policy))
    } else {
        map_type
    }
}

/// Reject every directive an inline table's row-entry slot can carry that the slot does not honor.
///
/// The slot became live when `@duplicates` started being read there, and a live slot must not also
/// be a silent-drop slot — so this enumerates `RuleMetadata` EXHAUSTIVELY (the destructure is the
/// enforcement: a new directive field fails to compile here until it is classified as honored or
/// rejected). The custom-codec pair and its `@custom_encodings`/`@custom_wire_major` declarations are
/// owned by the two helpers that run before this one, so they are excluded here rather than reported
/// twice.
fn reject_inert_inline_table_row_directives(
    types: &mut IntermediateTypes,
    position: &str,
    metadata: &RuleMetadata,
) -> bool {
    let RuleMetadata {
        name,
        rust_name,
        newtype,
        no_alias,
        key_demand,
        used_as_elem,
        copy,
        raw_bytes_flavor,
        ignore,
        // honored here — the whole reason the slot is live
        duplicates: _,
        custom_json,
        no_json_schema_export,
        // owned by `reject_custom_codec_on_row_entry` / `reject_custom_encodings_without_pair`
        custom_serialize: _,
        custom_deserialize: _,
        custom_encodings: _,
        custom_wire_major: _,
        extern_companions,
        doc,
    } = metadata;
    // `@name` is the one with a real alternative spelling worth naming: on a type-choice arm the
    // variant name lives in the slot AFTER the closing brace, which is a different comment entirely.
    let name_remedy = "To name a type-choice variant put `; @name <n>` AFTER the closing brace \
                       (`{ * k => v } ; @name <n> / int`); to rename a field put it on the field \
                       entry. To carry any other directive, name the table as its own rule \
                       (`t = { * k => v } ; @<directive>`) and reference `t`.";
    let rule_remedy = "Name the table as its own rule (`t = { * k => v } ; @<directive>`) and \
                       reference `t`, or remove it.";
    let found: [(&str, bool, &str); 12] = [
        ("@name", name.is_some(), name_remedy),
        ("@rust_name", rust_name.is_some(), rule_remedy),
        ("@newtype", newtype.is_some(), rule_remedy),
        ("@no_alias", *no_alias, rule_remedy),
        ("@used_as_key", key_demand.is_some(), rule_remedy),
        ("@used_as_elem", *used_as_elem, rule_remedy),
        ("@copy", *copy, rule_remedy),
        ("@raw_bytes_flavor", *raw_bytes_flavor, rule_remedy),
        ("@ignore", *ignore, rule_remedy),
        ("@custom_json", *custom_json, rule_remedy),
        (
            "@no_json_schema_export",
            *no_json_schema_export,
            rule_remedy,
        ),
        (
            "@extern_companions",
            extern_companions.is_some(),
            rule_remedy,
        ),
    ];
    let mut rejected = false;
    for (directive, written, remedy) in found {
        if !written {
            continue;
        }
        types.record_rejection(format!(
            "{directive} on the {position}: the row entry declares no rule, field or type of its \
             own, so only `@duplicates` (which selects the row's container) is honored there. \
             {remedy}"
        ));
        rejected = true;
    }
    // `@doc` last: it is the one spelling an author reaches for reflexively, and its own message
    // says where the documentation would have to live to be emitted.
    if doc.is_some() {
        types.record_rejection(format!(
            "@doc on the {position}: an anonymous inline table emits no type of its own to \
             document (it renders as the container type at each use site). Put the `@doc` on the \
             field or element that holds the table, or name the table as its own rule \
             (`t = {{ * k => v }} ; @doc <text>`) and reference `t`."
        ));
        rejected = true;
    }
    rejected
}

/// The field/member DIRECTIVE validation for the SINGLE-ENTRY group-choice arm — the member
/// position that mints no record, and therefore never reaches the record field walk that honors or
/// refuses a member's directives. Without this the whole family was read into the entry's trailing
/// metadata and dropped at exit 0: `[ a: uint // f: bytes ; @custom_serialize ws_only ]` generated
/// the arm's codec in both directions with `ws_only` called nowhere, and the COMPLETE pair,
/// `@raw_bytes_flavor`, `@doc` and the rest behaved identically, in both representations. A silent
/// drop is the one outcome this DSL refuses everywhere else, so every directive the field walk
/// READS is either honored here or refused here.
///
/// Three groups, from the field walk's own reads (a code enumeration of what
/// `parse_record_from_group_choice` does with a member's `RuleMetadata`, not a keyword sweep):
/// - The RULE-SCOPED ones go through `reject_member_scoped_directives`, the list the field walk
///   itself calls — same directives, same remedies, one shared source. That list carries both the
///   directives whose only valid home is a marker/collection RULE (`@raw_bytes_flavor`, `@copy`,
///   `@used_as_elem`, `@extern_companions`, `@duplicates`, `@ignore`) and the TYPE-SCOPED six
///   (`@rust_name`, `@newtype`, `@no_alias`, `@used_as_key`, `@custom_json`,
///   `@no_json_schema_export`).
/// - The custom-codec family (`@custom_serialize` / `@custom_deserialize` and the
///   `@custom_encodings` / `@custom_wire_major` wire facts that describe their wire) is HONORED at
///   an ordinary field and cannot be here: the arm registers no `RustField` for the pair to ride,
///   and the variant's codec is generated by the enum. The remedy is asserted to route — a complete
///   pair on the member's own TYPE rule emits `ws_only(serializer, f)` / `rd_only(raw)` inside this
///   very arm's serialize/deserialize — which is what makes the refusal honest rather than a
///   deferral. ANY presence refuses, one message: at a field a lone half is its own defect (two wire
///   forms at one position) while the complete pair is fine, but here neither reaches emission, so
///   splitting the verdict would report a completeness problem the position does not have.
/// - `@doc` is honored at a field as the field's doc comment; here the arm's own slot
///   (`// ; @doc <text>`, from `comments_before_grpchoice`) already documents the variant and IS
///   read, so the entry slot is refused with that slot as the remedy rather than given a second,
///   colliding meaning.
///
/// `@name` splits, because this slot HAS a reader for exactly one member shape family: the
/// anonymous-inline-composite minting paths take the member's struct name from it, so
/// `[ a: uint // f: [x: uint] ; @name Inner ]` and `{ a: uint // f: {x: uint} ; @name Inner }`
/// mint `pub struct Inner`. That naming door is what
/// the "Anonymous groups not allowed" error advertises and `comment_dsl.mdx` documents, so it is
/// kept; everywhere else the name was read by nothing (`// f: bytes ; @name renamed` still emitted
/// variant `F`), and that is refused with the arm's OWN naming slot as the remedy. Naming the
/// VARIANT from this slot instead was the rejected alternative: it renames variants in specs that
/// generate today (`F(Inner)` → `Inner(Inner)`) and mints a second naming slot beside the arm's
/// documented `// ; @name <n>`.
///
/// Which member shapes the reader covers is NOT restated here. It is OBSERVED, through the reader's
/// only effect — the member's parsed type IS the struct the name mints — which is why this seam runs
/// AFTER the entry's type parse and takes `member_type`. A second spelling of the reader's scope
/// ("sole type choice, operator-free, heterogeneous inline array") is precisely the drift the
/// observation avoids: change the reader and this verdict changes with it, in the same commit and by
/// construction. The one spelling the observation reads as consumed without the reader running is a
/// member whose type is a rule ALREADY named what the directive asks for (`f: inner ; @name inner`),
/// where the name describes the type the member already has and nothing is lost.
pub(super) fn reject_field_directives_on_single_entry_arm(
    types: &mut IntermediateTypes,
    name: &RustIdent,
    group_entry: &GroupEntry,
    optional_comma: &OptionalComma,
    member_type: &RustType,
) {
    // An `InlineGroup` entry panics `group_entry_rule_metadata` and is already refused on its own
    // terms by the entry-position rejection in `group_entry_to_type` — one message per problem, the
    // same carve-out the occurrence guard above makes.
    if matches!(group_entry, GroupEntry::InlineGroup { .. }) {
        return;
    }
    let metadata = group_entry_rule_metadata(group_entry, optional_comma);
    // Name the arm by its MEMBER key where it has one, and by the entry as written where it does
    // not (`// kv`), with the directive comment the entry renders back trimmed off — the author is
    // looking at their CDDL, and the comment is the thing being talked about, not part of the site.
    // A real member KEY (`f: bytes`), as opposed to the typename a keyless `// kv` arm is named
    // after — the two share `group_entry_to_raw_field_name`, but only the first can be re-spelled
    // with a `: inner` in the remedy below.
    let member_key = match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } if ge.member_key.is_some() => {
            group_entry_to_raw_field_name(group_entry)
        }
        _ => None,
    };
    let arm_desc = group_entry_to_raw_field_name(group_entry).unwrap_or_else(|| {
        group_entry
            .to_string()
            .split(';')
            .next()
            .unwrap_or_default()
            .trim()
            .to_owned()
    });
    let site = format!(
        "the single-entry group-choice arm `{arm_desc}` of rule `{}`",
        source_rule_name_of(types, name)
    );
    let codec_directives = custom_codec_family_directives(&metadata);
    if !codec_directives.is_empty() {
        let written = quoted_directive_list(&codec_directives, " / ");
        // The remedy is spelled for the arm as WRITTEN: a keyed member keeps its key, a keyless one
        // (`// kv`) references the new rule directly. Both spellings were asserted to route the
        // pair into this very arm's serialize/deserialize before this text shipped.
        let remedy_arm = match &member_key {
            Some(key) => format!("{key}: inner"),
            None => "inner".to_owned(),
        };
        types.record_rejection(format!(
            "{written} on {site}: the custom (de)serializer pair cannot be honored on a \
             single-entry arm — the arm registers no record, so the entry's type goes straight into \
             the enum variant and the variant's codec is generated by the enum, leaving the \
             directive nothing to replace. Name the member's type as its OWN rule and put the \
             COMPLETE pair there (`inner = <type> ; @custom_serialize <fn> @custom_deserialize \
             <fn>`, then `// {remedy_arm}`), which does route both directions at this arm."
        ));
    }
    if metadata.doc.is_some() {
        types.record_rejection(format!(
            "@doc on {site}: a single-entry arm registers no record, so there is no field for the \
             entry's doc comment to land on. Write it in the ARM's own slot instead, which \
             documents the enum variant the arm becomes: put `; @doc <text>` on the line that OPENS \
             the arm (`// ; @doc <text>`), before the entry."
        ));
    }
    // The `@name` fork: honored where the anonymous-composite reader consumed it, refused where it
    // did not.
    // The condition is the reader's own effect, read off the parsed member type rather than
    // re-derived from the AST — see this function's doc comment.
    if let Some(written) = metadata.name.as_ref() {
        let consumed = matches!(
            &member_type.conceptual_type,
            ConceptualRustType::Rust(ident) if ident.to_string() == convert_to_camel_case(written)
        );
        if !consumed {
            types.record_rejection(format!(
                "@name `{written}` on {site}: this slot names a member-position anonymous inline \
                 array or heterogeneous map (`// f: [x: uint] ; @name Inner` / `// f: {{x: uint}} ; \
                 @name Inner` mints `pub struct Inner` and holds it in the variant), and this \
                 member's type is not one — so the name is read by nothing \
                 here. The arm's naming slot is the one that follows the `//` opening it: write \
                 `// ; @name {written}` on that line to name the enum variant the arm becomes."
            ));
        }
    }
    // Never the dual-read slot: a group-choice arm belongs to a TYPE rule (a multi-choice plain
    // group body is refused before this walk runs), so no rule's trailing comment lands here.
    reject_member_scoped_directives(
        types,
        &site,
        "a group-choice arm's member",
        &metadata,
        false,
    );
}

/// The well-known-tag semantics registry: THE single place mapping a CBOR tag number to the
/// duplicate-handling policy its IANA-registered semantics imply, applied wherever the tag directly
/// wraps a homogeneous occurrence collection (the shape the array/table construction sites already
/// guard — a record-shaped `#6.258([uint, text])` becomes a Record and a primitive `#6.258(text)` a
/// Wrapper, so neither reaches those sites). This is the extension point for future well-known-tag
/// entries (e.g. the bignum tags 2/3): add an arm here rather than scattering tag-number checks
/// through the parser.
///
/// `is_array` distinguishes a set-shaped inner (`#6.258([* a])`) from a map-shaped inner
/// (`#6.258({* k => v})`). Tag 258 is the IANA set tag, so it implies `Reject` (uniqueness) ONLY on
/// an array inner — a map is not a set and gets nothing. The default returned here applies only when
/// the author wrote no explicit `@duplicates` directive; an explicit directive always wins (explicit
/// `reject` is an accepted self-documenting no-op, explicit `preserve` is the per-rule opt-out back
/// to today's plain `Vec`/`NonEmptyVec` behavior verbatim on the wire).
pub(super) fn well_known_tag_default_duplicates(
    tag: usize,
    is_array: bool,
) -> Option<DuplicatesPolicy> {
    match (tag, is_array) {
        (258, true) => Some(DuplicatesPolicy::Reject),
        _ => None,
    }
}

/// Return `rule_metadata` with the well-known-tag registry default injected into its `duplicates`
/// field when (a) the author wrote no explicit directive and (b) the registry has a default for this
/// `(tag, is_array)`. When the default applies and `notice` is `Some`, print it (a one-line
/// generation-time notice; no notice when the directive is explicit, either value). The returned
/// metadata is what the RustStruct constructor reads, so `config().duplicates` reflects the
/// EFFECTIVE policy for every downstream consumer (embed sites, generic use-site re-resolution, the
/// wasm collision detectors, the extern-interface projection).
pub(super) fn with_well_known_tag_default(
    rule_metadata: &RuleMetadata,
    tag: usize,
    is_array: bool,
    notice: Option<&str>,
) -> RuleMetadata {
    let mut effective = rule_metadata.clone();
    if effective.duplicates.is_none()
        && let Some(default) = well_known_tag_default_duplicates(tag, is_array)
    {
        effective.duplicates = Some(default);
        if let Some(notice) = notice {
            // A diagnostic, not progress: it announces a decode-behaviour change the spec did not ask
            // for (loose historical bytes with duplicate elements now fail), so stderr and the default
            // level.
            crate::warn!("{notice}");
        }
    }
    effective
}

/// Effective metadata for a single-arm tagged ARRAY rule (`foo = #6.258([* a])`, mandatory tag):
/// inject the registry's set-semantics default (258 → reject) when no explicit `@duplicates`, and
/// print the single-arm defaulting notice if it applies. The tag stays a mandatory `Option<Sz>`
/// (grammar decides the encoding record); only the inner element type gains uniqueness.
pub(super) fn single_arm_array_effective_metadata(
    rule_metadata: &RuleMetadata,
    tag: Option<usize>,
    name: &RustIdent,
) -> RuleMetadata {
    match tag {
        Some(t) => with_well_known_tag_default(
            rule_metadata,
            t,
            true,
            Some(&format!(
                "Rule `{name}` (single-arm tag {t} set) defaulting to @duplicates reject (IANA set semantics) — write `; @duplicates preserve` on the rule to opt out"
            )),
        ),
        None => rule_metadata.clone(),
    }
}

/// A type-choice arm's metadata, merged across the `Type1` and `TypeChoice` comment slots because
/// the cddl parser may bind an arm's trailing comment to either.
pub(super) fn type_choice_metadata(choice: &TypeChoice) -> RuleMetadata {
    merge_metadata(
        &RuleMetadata::from(choice.type1.comments_after_type.as_ref()),
        &RuleMetadata::from(choice.comments_after_type.as_ref()),
    )
}

/// The RULE-POSITION metadata of a multi-arm type rule: the LAST arm's trailing comment, merged
/// across the `TypeChoice` and `Type1` levels because the cddl parser may bind a rule's trailing
/// comment to either. (In the pinned fork it lands on the `TypeChoice` slot in every spelling
/// dumped from the AST; the `Type1` half is kept for robustness against a parser change and is what
/// makes this identical to the merge every other rule-position site performs.)
///
/// Both choice branches read the last arm through this seam, so nullable collapses and enums
/// agree on the rule slot. The inner arm's Type1 comment slot is not populated by the parser.
pub(super) fn rule_position_metadata(type_choices: &[TypeChoice]) -> RuleMetadata {
    merge_metadata(
        &RuleMetadata::from(
            type_choices
                .last()
                .and_then(|tc| tc.comments_after_type.as_ref()),
        ),
        &RuleMetadata::from(
            type_choices
                .last()
                .and_then(|tc| tc.type1.comments_after_type.as_ref()),
        ),
    )
}

/// `@raw_bytes_flavor` on a rule that is not an extern marker. Shared verbatim by every
/// type-rule seam that can reach the misplacement so the pinned wording cannot drift between them.
fn raw_bytes_flavor_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@raw_bytes_flavor on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} \
         rule — it selects the `<ExternName>RawBytes` wrapper flavor for generic instances \
         whose argument is a {RAW_BYTES_MARKER} type. Remove it from this rule."
    )
}

/// `@raw_bytes_flavor` on an extern marker rule that declares no generic parameters. The companion
/// half of `raw_bytes_flavor_not_extern_rejection`: that one gates the rule KIND (extern), this one
/// gates the extern arm itself on generic-ness. The tag names a per-INSTANCE flavor
/// (`uses_raw_bytes_flavor` is keyed by the generic instance being lowered), so a base that declares
/// no parameters has no instances to flavor and the mark is inert — there is no coherent honoring to
/// fall back on, hence a rejection rather than a warning. Wording is a load-bearing test key
/// (`dsl_position_tests` cell `@raw_bytes_flavor` @ `non-generic-extern-rule` pins
/// "declares no generic parameters"); the anchor deliberately does NOT reuse the `only valid on`
/// phrasing above, so the two seams stay distinguishable in a test's expectation.
pub(super) fn raw_bytes_flavor_non_generic_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@raw_bytes_flavor on `{type_name}`: this tag selects the `<ExternName>RawBytes` wrapper \
         flavor for GENERIC instances of an extern whose argument is a {RAW_BYTES_MARKER} type, \
         but `{type_name}` declares no generic parameters — there are no instances to flavor, so \
         the tag would be silently inert. Declare the rule's generic parameters \
         (`{type_name}<T> = {EXTERN_MARKER} ; @raw_bytes_flavor`) or remove the tag."
    )
}

/// `@copy` on a rule that is neither an extern nor a raw-bytes marker. Shared for the same reason
/// as `raw_bytes_flavor_not_extern_rejection`.
fn copy_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@copy on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} or \
         {RAW_BYTES_MARKER} rule — it declares that the externally-defined rust type derives \
         `Copy` so the generator stops cloning it at boundaries. Remove it from this rule."
    )
}

/// Ownership determines which directives a non-last arm can consume.
#[derive(Clone, Copy)]
pub(super) enum NonLastArmOwner<'a> {
    /// Named enum variants read names and docs.
    TypeChoiceRule(&'a RustIdent),
    /// Nullable aliases have no variants; the rule-name pre-scan handles names.
    NullCollapseRule(&'a RustIdent),
    /// Collapsed tag-sets have no variants; the preceding enum check handles other directives.
    TagSetRule(&'a RustIdent),
    /// Inline enum variants read names and docs.
    InlineTypeChoice,
    /// Inline nullable fields have neither variants nor a rule-name pre-scan.
    InlineNullCollapse,
}

impl NonLastArmOwner<'_> {
    fn refuses(self, directive: Directive) -> bool {
        match self {
            Self::TypeChoiceRule(_) | Self::InlineTypeChoice => !directive.is_variant_legal(),
            Self::NullCollapseRule(_) => directive != Directive::Name,
            Self::TagSetRule(_) => directive.is_variant_legal(),
            Self::InlineNullCollapse => true,
        }
    }

    fn rejection(self, found: &str) -> String {
        match self {
            Self::TypeChoiceRule(name) => format!(
                "{found} on a non-last arm of the multi-choice type rule `{name}`: a rule-level \
                     directive attaches through the LAST arm's trailing comment, which is the \
                     rule-position slot — on any other arm only `@name` and `@doc` are read (they \
                     name and document that variant). Move it to the last arm, or reorder the arms \
                     so the one carrying it is last."
            ),
            Self::NullCollapseRule(name) => format!(
                "{found} on a non-last arm of the `T / null` rule `{name}`: the rule collapses to a \
                     transparent `Option<T>` alias, so its arms are not variants and carry no \
                     directives of their own — a rule-level directive attaches through the LAST \
                     arm's trailing comment. Move it there (`{name} = … / … ; @…`)."
            ),
            Self::TagSetRule(name) => format!(
                "{found} on a non-last arm of the tag-set rule `{name}`: its two arms collapse \
                         into one collection whose tag is optional on the wire, so they are not \
                         enum variants and there is no variant for `@name` to name or `@doc` to \
                         document. Document the rule with `@doc` in the LAST arm's trailing \
                         comment, which is the rule-position slot, and remove `@name`."
            ),
            Self::InlineTypeChoice => format!(
                "{found} on a non-last arm of an inline type choice: the choice lowers to an \
                         anonymous enum whose variants read only `@name` and `@doc`, so that arm \
                         owns no other directive slot. Move a field directive to the entry's \
                         trailing comment, or put the annotation on a named type rule."
            ),
            Self::InlineNullCollapse => format!(
                "{found} on a non-last arm of an inline `T / null` choice: the choice lowers \
                         to an optional field rather than variants, so that arm owns no directive \
                         slot. Move a field directive to the entry's trailing comment, or put the \
                         annotation on a named type rule."
            ),
        }
    }
}

pub(super) fn reject_non_last_arm_directives(
    types: &mut IntermediateTypes,
    arms: &[TypeChoice],
    owner: NonLastArmOwner<'_>,
) {
    if let Some((_, preceding)) = arms.split_last() {
        for arm in preceding {
            let mut directives = type_choice_metadata(arm).directives();
            directives.retain(|directive| owner.refuses(*directive));
            // Stable Directive::ALL order within each class keeps the existing list order.
            directives.sort_by_key(|directive| directive.is_variant_legal());
            if !directives.is_empty() {
                let found = directives
                    .iter()
                    .map(|directive| directive.spelling())
                    .collect::<Vec<_>>()
                    .join(" / ");
                types.record_rejection(owner.rejection(&found));
            }
        }
    }
}

/// The `@extern_companions`-on-a-non-extern-rule rejection message. One owner, because the directive
/// is reachable from three rule shapes (single-choice type rule, multi-choice type rule, plain group)
/// and the text is what a spec author acts on.
fn extern_companions_not_extern_rejection(type_name: &RustIdent) -> String {
    format!(
        "@extern_companions on `{type_name}`: this tag is only valid on a {EXTERN_MARKER} or \
         {RAW_BYTES_MARKER} rule — it declares that the STRUCTURAL wasm companion classes of an \
         externally-defined type (its `<Name>List`, `Map<Name>To…`, …) already exist in a sibling \
         wasm crate, so this crate references them instead of minting duplicate `#[wasm_bindgen]` \
         classes. A rule this crate GENERATES owns its own companions. Remove it from this rule."
    )
}

/// Validate and record an `@extern_companions` declaration for the marker rule `type_name` — either
/// user-supplied flavor (`_CDDL_CODEGEN_EXTERN_TYPE_` or `_CDDL_CODEGEN_RAW_BYTES_TYPE_`; the scope
/// check below is orthogonal to which marker the rule spells, and both name a type this crate does
/// not define while its structural wrappers are named from the rule's ident). Valid
/// only on a LOCALLY-scoped (exported) marker: a DEP-scoped extern (a rule in an
/// `EXTERN_DEPS_DIR` scope) is already served by `--extern-wrapper-index` / `--workspace-dep`, which
/// key on the constituents' owning dependency and can consult the dep's committed index — so a
/// directive there would be a second, weaker authority for the same decision. Graceful
/// `record_rejection` throughout, following the `@rust_name` / `@raw_bytes_flavor` precedent.
///
/// The rule's scope is known here for the same reason it is in `handle_rust_name_pin`:
/// `api::with_types` calls `mark_scope` for every rule before the parse walk runs.
pub(super) fn handle_extern_companions(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
) {
    let Some(companions) = rule_metadata.extern_companions.as_ref() else {
        return;
    };
    if !types.scope(type_name).export() {
        types.record_rejection(format!(
            "@extern_companions on `{type_name}`: this rule lives in a {EXTERN_DEPS_DIR} scope, so \
             its collection wrappers are already owned by the dependency-keyed mechanisms — pass \
             `--extern-wrapper-index=<dep>=<dep>/wasm/src/generated/collections.rs` (defer to the \
             classes the dependency's own generation committed) or `--workspace-dep=<dep>` (defer \
             unconditionally). `@extern_companions` exists for a LOCAL marker \
             (`x = {EXTERN_MARKER}` or `x = {RAW_BYTES_MARKER}` in this crate's own spec), which \
             has no dependency edge for those flags to key on. Remove it from this rule."
        ));
        return;
    }
    types.mark_extern_companions(type_name.clone(), companions.clone());
}

/// Validate and record a `@rust_name` pin for the rule `type_name`, following the
/// `@raw_bytes_flavor`-only-on-extern precedent (graceful `record_rejection`, never a panic).
///
/// `@rust_name` pins the dependency's FINAL Rust type name so a consumer reads it across the crate
/// boundary instead of re-deriving it (killing the cross-version naming-skew class). It is therefore
/// valid ONLY on a rule in a non-exported (`EXTERN_DEPS_DIR`) scope — an extern-interface / stub
/// file. On a normally-generated (exported) rule the consumer IS the version that spells the name,
/// so a pin there would silently do nothing: reject it. A pin that camel-cases to a reserved Rust
/// std/prelude type (or a CDDL keyword) is rejected exactly as a derived name would be, so a
/// `@rust_name Option` pin can't slip past the reserved-ident bar that guards derived names.
///
/// The rule's scope is known here: `api::with_types` calls `mark_scope` for every rule before the
/// parse walk runs, so `types.scope(type_name)` is already populated.
fn handle_rust_name_pin(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
) {
    let Some(pin) = rule_metadata.rust_name.as_ref() else {
        return;
    };
    if types.scope(type_name).export() {
        types.record_rejection(format!(
            "@rust_name on `{type_name}`: reserved for extern-interface / stub files (rules in a \
             {EXTERN_DEPS_DIR} scope). It pins a dependency's final Rust name so a consumer reads it \
             across the crate boundary instead of deriving it; on a normally-generated (exported) \
             rule it would silently do nothing. Remove it."
        ));
    } else if let Some(msg) = reserved_pin_rejection(pin, type_name.as_ref()) {
        types.record_rejection(msg);
    } else {
        types.mark_rust_name_pin(type_name.clone(), pin.clone());
    }
}

/// The rule-position metadata for a plain GROUP rule. cddl binds a group rule's TRAILING comment
/// (`grp = (a: uint) ; @rust_name X`) to the LAST group entry's trailing comment slot, not
/// `comments_after_group` (empirically verified — that slot is `None` for a single-line group rule),
/// the same slot `group_entry_to_field_name` reads for a field-position `@name`. So read from there,
/// falling back to `comments_after_group` for robustness.
///
/// The whole slot is returned; the CALLER decides what to do with each directive, under two
/// different bars. To be **honored** on the rule, a directive must have **no field-position
/// meaning**, because that shared slot makes the two positions indistinguishable here: `@rust_name`
/// (pins the rule's Rust type name), `@no_json_schema_export` (suppresses the rule's
/// schema-registration row), `@custom_json` (suppresses the derives on the struct a SPLICED group
/// mints) and `@used_as_key` (demands comparison derives on that same struct) all qualify — none has
/// any effect at a field. A field-position `@name` sharing the slot legitimately renames that field
/// and is left to the field-naming site, so it must NOT be honored on the rule; the same bar applies
/// to any directive honored here later. To be **reported** — the never-spliced refusal in
/// `IntermediateTypes::finalize` — no bar applies: a group nothing splices emits neither a struct
/// nor a field, so every directive in this slot is inert under BOTH readings and naming it steals no
/// meaning a field would otherwise have had.
///
/// `comments_after_group` is empirically always `None` (see above), so the merge degenerates to the
/// trailing slot; it is written as a merge for the same reason `parse_type` merges its two slots —
/// the cddl parser chooses where to bind, and a directive found in either counts.
///
/// ONE spelling puts a group rule's trailing comment beyond any slot this fn reads: a closing paren
/// on its own line (`grp = (\n a: uint\n) ; @x`). At the pinned fork rev the comment is NOT
/// discarded — the pest bridge's comment binding is a source-position trivia merge, and with no
/// trailing anchor of the group rule's own on that line, the merge binds the comment to the
/// FOLLOWING rule's `comments_before_rule` (or orphans it when the group rule is last). Nothing
/// reads that position, so a directive written there would be lost on formatting alone — which is
/// why [`multiline_group_trailing_directive_rejection`] REFUSES the spelling pre-IR, from the source
/// buffer, before this fn is ever reached for such a rule. Everything below therefore only ever sees
/// a spelling the parser does bind (whole group on one line, or closing paren on the last entry's
/// line). `Rule::Group`'s own `comments_after_rule` is not an escape hatch at this pin: the merge
/// emits no anchor for it (the construction sites' `None`s are pre-merge defaults, not the
/// mechanism), so reading it here is dead code until the fork-side fix is adopted. There is NO
/// second lossy spelling: a last entry's slot cannot be "contended" by a rule-trailing comment on
/// one line, because a CDDL comment runs to end of line — that spelling comments out the closing
/// paren and fails to parse. The fork-side fix (an additive `RuleTrailing` merge fallback) exists on
/// the dcSpark fork's `local-fixes` branch, unadopted by maintainer ruling, and is what would make
/// the refused spelling HONORED rather than merely loud; state and design constraints are tracked in
/// `tests/testing-roadmap.toml` ("Adopt the parser's `RuleTrailing` anchor for multi-line group
/// rules").
pub(super) fn group_rule_pin_metadata(
    group: &Group,
    comments_after_group: Option<&Comments>,
) -> RuleMetadata {
    let mut metadata = RuleMetadata::from(comments_after_group);
    if let Some((entry, optional_comma)) = group
        .group_choices
        .last()
        .and_then(|gc| gc.group_entries.last())
    {
        // An inline-group last entry has no trailing-comment slot in this position (its members are
        // flattened before a record forms) — unreachable in practice; fall back to the empty slot.
        let empty: Option<Comments> = None;
        let entry_trailing = match entry {
            GroupEntry::ValueMemberKey {
                trailing_comments, ..
            } => trailing_comments,
            GroupEntry::TypeGroupname {
                trailing_comments, ..
            } => trailing_comments,
            GroupEntry::InlineGroup { .. } => &empty,
        };
        let combined = combine_comments(entry_trailing, &optional_comma.trailing_comments);
        let trailing = metadata_from_comments(&combined.unwrap_or_default());
        metadata = merge_metadata(&metadata, &trailing);
    }
    metadata
}

/// The rule-body shape determines which checks belong here and which lowering site owns them.
#[derive(Clone, Copy)]
pub(super) enum RuleBodyShape {
    /// Collection bodies route their collection directives to their lowering site.
    SingleType {
        marker: Option<&'static str>,
        generic_instantiation: bool,
        collection_body: bool,
    },
    /// Nullable lowering owns the key, element, and newtype refusals.
    NullCollapse,
    /// Choice lowering owns duplicates, ignore, and name checks once its arms are built.
    TypeChoice,
    /// The slot is shared with the last field; only directives without field meaning are read.
    PlainGroup,
}

impl RuleBodyShape {
    pub(super) fn single_type(type1: &Type1) -> Self {
        let marker = match &type1.type2 {
            Type2::Typename { ident, .. } if ident.ident == EXTERN_MARKER => Some(EXTERN_MARKER),
            Type2::Typename { ident, .. } if ident.ident == RAW_BYTES_MARKER => {
                Some(RAW_BYTES_MARKER)
            }
            _ => None,
        };
        Self::SingleType {
            marker,
            generic_instantiation: matches!(
                &type1.type2,
                Type2::Typename {
                    generic_args: Some(_),
                    ..
                }
            ),
            collection_body: matches!(
                &type1.type2,
                Type2::Map { .. }
                    | Type2::Array { .. }
                    | Type2::TaggedData { .. }
                    | Type2::ParenthesizedType { .. }
            ),
        }
    }
}

/// Record shared marks and check the shape once, after looking through rule wrappers.
pub(super) fn apply_rule_position_directives(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
    shape: RuleBodyShape,
) {
    if let Some(demand) = rule_metadata.key_demand {
        types.mark_key_demand(type_name.clone(), demand);
    }
    if rule_metadata.no_json_schema_export {
        types.mark_no_json_schema_export(type_name.clone());
    }
    if rule_metadata.custom_json {
        types.mark_custom_json_rule(type_name.clone());
    }
    if matches!(shape, RuleBodyShape::PlainGroup) {
        handle_rust_name_pin(types, type_name, rule_metadata);
        return;
    }
    if rule_metadata.used_as_elem {
        types.mark_used_as_elem(type_name.clone());
    }
    if rule_metadata.no_alias {
        types.mark_no_alias_rule(type_name.clone());
    }
    if let Some(doc) = &rule_metadata.doc {
        types.mark_rule_doc(type_name.clone(), doc.clone());
    }
    reject_rule_position_misplacements(types, type_name, rule_metadata, shape);
    handle_rust_name_pin(types, type_name, rule_metadata);
}

/// Use one refusal order across rule shapes; lowering still owns shape-specific checks.
fn reject_rule_position_misplacements(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
    shape: RuleBodyShape,
) {
    let marker = match shape {
        RuleBodyShape::SingleType { marker, .. } => marker,
        _ => None,
    };
    if rule_metadata.custom_json
        && let Some(marker) = marker
    {
        types.record_rejection(format!(
            "@custom_json on `{type_name}`: a {marker} rule names a type this crate does not \
             define, so that type already owns its JSON impls — there are no generated \
             serde/schemars derives here to suppress, and the impls you would hand-write belong \
             with the type itself. Give the rule a real CDDL body and put `@custom_json` there \
             (`{type_name} = <body> ; @newtype @custom_json`), or drop the directive and write the \
             impls beside the externally-defined type."
        ));
    }
    if rule_metadata.raw_bytes_flavor && marker != Some(EXTERN_MARKER) {
        types.record_rejection(raw_bytes_flavor_not_extern_rejection(type_name));
    }
    if rule_metadata.copy && marker.is_none() {
        types.record_rejection(copy_not_extern_rejection(type_name));
    }
    if rule_metadata.extern_companions.is_some() && marker.is_none() {
        types.record_rejection(extern_companions_not_extern_rejection(type_name));
    }
    reject_custom_encodings_without_pair(types, &format!("rule `{type_name}`"), rule_metadata);
    if let RuleBodyShape::SingleType {
        generic_instantiation,
        ..
    } = shape
    {
        reject_single_type_custom_codec(
            types,
            type_name,
            rule_metadata,
            marker,
            generic_instantiation,
        );
    }
    let routes_collection_directives = matches!(
        shape,
        RuleBodyShape::SingleType {
            collection_body: false,
            ..
        } | RuleBodyShape::NullCollapse
    );
    if routes_collection_directives {
        if rule_metadata.duplicates.is_some() {
            reject_duplicates_not_applicable(types, type_name);
        }
        if rule_metadata.ignore {
            reject_ignore_not_applicable(types, type_name);
        }
    }
}

fn reject_single_type_custom_codec(
    types: &mut IntermediateTypes,
    type_name: &RustIdent,
    rule_metadata: &RuleMetadata,
    marker: Option<&'static str>,
    is_generic_instantiation: bool,
) {
    let custom_directives = custom_codec_directives(rule_metadata);
    for directive in &custom_directives {
        // A marker rule has no generated codec carrier. An alias of that marker can own a pair,
        // which is why the refusal offers the alias spelling as its second remedy.
        if let Some(marker) = marker {
            types.record_rejection(format!(
                "{directive} on `{type_name}`: a {marker} rule names a type this crate does \
                 not define, so that type owns its own serialization impls and the custom \
                 (de)serializer pair never reaches generation. Two spellings work. Give the rule a \
                 real CDDL body and put the pair there (`<rule> = text ; @custom_serialize <fn> \
                 @custom_deserialize <fn>`), which states the wire type in the spec; or keep this \
                 rule as the marker and put the pair on an ALIAS of it (`<alias> = <rule> ; \
                 @custom_serialize <fn> @custom_deserialize <fn>`), which keeps the marker's rust \
                 type and overrides only how that type is written here — under \
                 `--preserve-encodings` an alias of {EXTERN_MARKER} must also declare its wire \
                 with `@custom_encodings`, while an alias of {RAW_BYTES_MARKER} infers it. Both \
                 route the pair through the type-level alias override."
            ));
        }
        // Generic instances are built from the definition's config; a pair on the binding has
        // no route into that codec, including bindings to a generic set nominal.
        if is_generic_instantiation {
            types.record_rejection(format!(
                "{directive} on `{type_name}`: this rule binds a generic instantiation, and the \
                 type it mints is built from the generic DEFINITION's config during generic \
                 resolution — so the pair written here never reaches generation and both \
                 directions keep the definition's generated codec. Give the rule a CDDL body of \
                 its own and put the pair there (`{type_name} = <body> ; @custom_serialize <fn> \
                 @custom_deserialize <fn>`), or declare `{type_name}` as a {EXTERN_MARKER} rule \
                 and hand-write the type in full."
            ));
        }
        // A pair-carrying alias keeps its routing node but emits no pub type, so no_alias beside
        // the pair is accepted and redundant.
        // A custom pair defines only the implicit homogeneous-table map wrapper contract.
        // Explicit wrappers have undefined tag, range, set, encoding and cross-face codec contracts.
        if rule_metadata.newtype.is_some() {
            types.record_rejection(format!(
                "{directive} together with `@newtype` on `{type_name}`: complete custom codec pairs are \
                 supported only on the implicit homogeneous-table map owner; it does not \
                 define the custom-codec contract for an explicit wrapper (including its tag, range, \
                 set, preserve-encoding, or cross-face behavior). Drop `@newtype` and use the plain \
                 alias spelling (`<rule> = \
                 <body> ; @custom_serialize <fn> @custom_deserialize <fn>`), or declare the type \
                 `{EXTERN_MARKER}` and hand-write it in full."
            ));
        }
    }
}

pub(super) fn group_entry_rule_metadata(
    entry: &GroupEntry,
    optional_comma: &OptionalComma,
) -> RuleMetadata {
    let entry_trailing_comments = match entry {
        GroupEntry::ValueMemberKey {
            trailing_comments, ..
        } => trailing_comments,
        GroupEntry::TypeGroupname {
            trailing_comments, ..
        } => trailing_comments,
        GroupEntry::InlineGroup { group, .. } => panic!(
            "not implemented (define a new struct for this!) = {}\n\n {:?}",
            group, group
        ),
    };
    let combined_comments =
        combine_comments(entry_trailing_comments, &optional_comma.trailing_comments);
    metadata_from_comments(&combined_comments.unwrap_or_default())
}

/// Read the only two comment slots the pinned CDDL AST can associate with a repeated inline group
/// entry. This is intentionally separate from [`group_entry_rule_metadata`]: an `InlineGroup` has
/// no ordinary entry-trailing slot, and that helper rightly refuses to pretend it does.
pub(super) fn inline_group_occurrence_metadata(
    comments_after_group: &Option<Comments>,
    optional_comma: &OptionalComma,
) -> RuleMetadata {
    let combined_comments =
        combine_comments(comments_after_group, &optional_comma.trailing_comments);
    metadata_from_comments(&combined_comments.unwrap_or_default())
}

/// A repeated inline group's entry is structural syntax, not a separately nameable declaration.
/// Reject every populated metadata field rather than silently assigning a directive to neither the
/// owner wrapper nor its fixed-name synthesized item. The exhaustive destructure is load-bearing:
/// adding a `RuleMetadata` field forces a conscious decision here.
pub(super) fn reject_inline_group_occurrence_directives(
    types: &mut IntermediateTypes,
    owner: &RustIdent,
    item_ident: &RustIdent,
    metadata: &RuleMetadata,
) -> bool {
    let RuleMetadata {
        name,
        rust_name,
        newtype,
        no_alias,
        key_demand,
        used_as_elem,
        copy,
        raw_bytes_flavor,
        ignore,
        duplicates,
        custom_json,
        no_json_schema_export,
        custom_serialize,
        custom_deserialize,
        custom_encodings,
        custom_wire_major,
        extern_companions,
        doc,
    } = metadata;
    let found = [
        ("@name", name.is_some()),
        ("@rust_name", rust_name.is_some()),
        ("@newtype", newtype.is_some()),
        ("@no_alias", *no_alias),
        ("@used_as_key", key_demand.is_some()),
        ("@used_as_elem", *used_as_elem),
        ("@copy", *copy),
        ("@raw_bytes_flavor", *raw_bytes_flavor),
        ("@ignore", *ignore),
        ("@duplicates", duplicates.is_some()),
        ("@custom_json", *custom_json),
        ("@no_json_schema_export", *no_json_schema_export),
        ("@custom_serialize", custom_serialize.is_some()),
        ("@custom_deserialize", custom_deserialize.is_some()),
        ("@custom_encodings", custom_encodings.is_some()),
        ("@custom_wire_major", custom_wire_major.is_some()),
        ("@extern_companions", extern_companions.is_some()),
        ("@doc", doc.is_some()),
    ]
    .into_iter()
    .filter_map(|(directive, written)| written.then_some(directive))
    .collect::<Vec<_>>();
    if found.is_empty() {
        return false;
    }
    let source = source_rule_name_of(types, owner);
    types.record_rejection(inline_group_occurrence_directive_message(
        &source,
        item_ident.as_ref(),
        &quoted_directive_list(&found, ", "),
    ));
    true
}

pub(super) fn inline_group_occurrence_directive_message(
    source: &str,
    item_ident: &str,
    found: &str,
) -> String {
    format!(
        "rule `{source}`: the repeated inline group's entry carries {found}, but that entry declares no \
         independently configurable type. The synthesized item name is fixed as `{item_ident}`. \
         Directives and documentation that apply to the outer rule belong after the closing `]` \
         (for example `{source} = [* (a: uint, b: tstr)] ; @doc <text>`); `@name` cannot rename \
         the synthesized item. To configure a separately named repeated item, define a named plain \
         group and repeat that rule instead."
    )
}

/// The entry naming a plain group is likewise structural in the nominal flat carrier: it repeats
/// values of the named group but creates neither a field nor another configurable type. Reuse
/// `all_directives`, whose own exhaustive metadata classification makes a newly added directive
/// fail closed here too.
pub(super) fn reject_named_plain_group_occurrence_directives(
    types: &mut IntermediateTypes,
    owner: &RustIdent,
    group_source: &str,
    metadata: &RuleMetadata,
) -> bool {
    let directives = metadata.all_directives();
    if directives.is_empty() {
        return false;
    }
    let owner_source = source_rule_name_of(types, owner);
    types.record_rejection(format!(
        "rule `{owner_source}`: the repeated named plain-group occurrence `{group_source}` carries {}, \
         but that entry declares no field or independently configurable type. Directives and \
         documentation that apply to the outer rule belong after the closing `]` (for example \
         `{owner_source} = [* {group_source}] ; @doc <text>`); directives for the repeated item \
         belong on the named plain-group rule `{group_source} = (…)`. Remove `@name` here: it \
         cannot rename either generated surface.",
        quoted_directive_list(&directives, ", ")
    ));
    true
}
