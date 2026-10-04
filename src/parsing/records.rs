//! Fixed record fields and post-field collision checks.
//! Derived encoding suffixes remain with their collision scanner.

use super::{
    DynamicRows, MapKeyKind, RUST_KEYWORDS, flatten_group_entries, generated_local_field_rejection,
    group_entry_map_key_kind, group_entry_optional, group_entry_rule_metadata,
    group_entry_to_field_name, group_entry_to_type, materialize_plain_group_ref,
    occurrence_permits_count, recognize_dynamic_rows, record_plain_group_map_member_rejection,
    reject_custom_encodings_without_pair, reject_exact_zero_field_only_metadata,
    reject_member_scoped_directives, source_rule_name_of,
};
use crate::cli::Cli;
use crate::intermediate::{
    ConceptualRustType, FixedValue, ForbiddenField, IntermediateTypes, Representation, RestRow,
    RestSemantics, RustField, RustIdent, RustRecord,
};
use cddl::ast::parent::ParentVisitor;
use cddl::ast::{GroupChoice, GroupEntry, Occur, OptionalComma};
use std::collections::BTreeMap;

/// The `--preserve-encodings` encoding-companion locals a record mints PER FIELD. A second field
/// whose own name is one of these spellings collides with the companion (or, for the `_key` case,
/// two fields mint the SAME companion name), emitting a crate that does not compile.
///
/// Measured at `b5b6283b`, `--preserve-encodings` (and the `--canonical-form` flavor):
/// - `<f>` + `<f>_encoding` — the field local shadows the value-encoding companion in the
///   struct-init shorthand, E0308. Breaks in array-rep, map-rep, embedded-plain-group and
///   group-choice-arm shapes, so it is checked in BOTH representations.
/// - `<f>` + `<f>_key_encoding` — same shadowing against the KEY-encoding companion, E0308. Map rep
///   only (array records mint no key encodings — probed clean in the array shape).
/// - `<f>` + `<f>_key` — both fields mint `<f>_key_encoding` into the encoding struct: E0124
///   (duplicate field) + E0062. Map rep only (probed clean in the array shape).
///
/// Checked uniformly across profiles for the same reason the single-name registry is: the default
/// profile compiles only because it mints no companions at all.
///
/// MAINTENANCE: this list is a hand-carried mirror of DERIVED naming — the emitters mint the
/// companions via `format!("{}_encoding", …)` / `…_key_encoding` (generation/serialize.rs,
/// deserialize.rs, mod.rs, enums.rs), and the LOCKSTEP scan
/// (`generated_local_registry_covers_emitter_locals`) verdicts FIXED locals only, so it cannot
/// see a companion-naming change. Renaming the companion scheme moves BOTH: these suffixes and
/// the emitters' format strings, or the pairwise refusal rots in both directions (gratuitous
/// refusals of the old spellings, unrefused collisions on the new ones).
const ENCODING_COMPANION_SUFFIXES: &[(&str, bool, &str)] = &[
    // (suffix, map-rep only, what collides)
    (
        "_encoding",
        false,
        "the value-encoding companion minted for `{base}` is spelled `{other}`, so the field's own \
         local shadows it in the struct-init shorthand (E0308)",
    ),
    (
        "_key_encoding",
        true,
        "the key-encoding companion minted for `{base}` is spelled `{other}`, so the field's own \
         local shadows it in the struct-init shorthand (E0308)",
    ),
    (
        "_key",
        true,
        "both fields mint the same encoding companion `{other}_encoding` (`{base}`'s KEY encoding \
         and `{other}`'s VALUE encoding), so the encoding struct declares that field twice \
         (E0124/E0062)",
    ),
];

#[allow(clippy::too_many_arguments)]
fn lower_record_field(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    source_name: &str,
    group_entry: &GroupEntry,
    optional_comma: &OptionalComma,
    index: usize,
    entry_count: usize,
    tagged: bool,
    rest_skip: &[usize],
    generated_fields: &mut BTreeMap<String, u32>,
    forbidden_fields: &mut Vec<ForbiddenField>,
    cli: &Cli,
) -> Option<RustField> {
    // The dynamic-row entries (recognized, or rejected as a candidate) are handled by
    // `recognize_dynamic_rows`; never build a fixed field for one. An open table skips
    // BOTH of its rows, which is why this is an index SET rather than one index.
    if rest_skip.contains(&index) {
        return None;
    }
    // An unflattened `InlineGroup` reaching the record loop is a parenthesized group whose
    // own occurrence marker would be silently narrowed to exactly-once (`[* (int, tstr)]`,
    // `{ * (k: int) }`), or a bare multi-choice group in entry position. All three panic in
    // `group_entry_to_field_name` / `group_entry_to_type` / `group_entry_optional`; reject
    // gracefully here BEFORE they run, citing the rule's SOURCE spelling.
    if let GroupEntry::InlineGroup { occur, .. } = group_entry {
        if occur.is_some() {
            // the remedy differs by representation: naming the group only helps arrays —
            // a plain-group reference inside a map record is itself unsupported (it hits
            // the "map field has no key" rejection), so don't send map users there.
            let remedy = match rep {
                Representation::Array => {
                    "Name the group instead: `pair = (int, tstr)`, `a = [* pair]` — or \
                     drop the parentheses for a single-element group (`[* int]`)."
                }
                Representation::Map => {
                    "Use `?` on each field for optionality, or a table `{ * k => v }`."
                }
            };
            types.record_rejection(format!(
                "rule `{source_name}`: an occurrence marker on an inline group (`* (…)`) \
                 would be silently narrowed to exactly-once (generated decoders would \
                 reject valid CBOR with other repetition counts). {remedy}"
            ));
        } else {
            // The remedy names spellings that GENERATE. Lifting the alternatives into a
            // named group (`g = (a // b)`) is NOT one of them — a group rule's body may
            // carry only one choice (`multi_choice_group_def_rejection`) — so point at the
            // container's own group choices, or at one named group per alternative.
            types.record_rejection(format!(
                "rule `{source_name}`: an inline group choice (`(a // b)`) in entry \
                 position is unsupported. Write the alternatives as the container's own \
                 group choices (`h = [ a: uint // f: bytes ]`), or give each alternative \
                 its own single-choice group rule and reference those as separate arms \
                 (`pga = (a: uint)`, `pgf = (f: bytes)`, `h = [ pga // pgf ]`)."
            ));
        }
        return None;
    }
    // For a map record, classify the member key BEFORE field naming: only uint/text fixed
    // keys are implemented (the map-key write path and, under --preserve-encodings,
    // `key_encoding_field`), and `group_entry_to_field_name` PANICS on a Type1 (arrow)
    // member key other than uint/text — so an unsupported key must be rejected here,
    // before naming runs. `group_entry_map_key_kind` never panics.
    let map_key = if rep == Representation::Map {
        match group_entry_map_key_kind(group_entry) {
            // supported: carry the classified key forward (no separate key lookup needed).
            MapKeyKind::Fixed(key @ (FixedValue::Uint(_) | FixedValue::Text(_))) => Some(key),
            // cite the rule by its SOURCE spelling (`neg`), not the camel-cased RustIdent —
            // the user is looking at their CDDL, not our output.
            MapKeyKind::Fixed(other) => {
                // The table remedy must not be advertised for a FLOAT key: a float-family
                // table key domain is itself rejected (floats have no total order, so they
                // cannot key a BTreeMap) — pointing there would send the user to a second
                // rejection instead of a fix.
                let remedy = if matches!(other, FixedValue::Float(_)) {
                    "Floats cannot key a map in either form (a float table key domain is \
                     rejected too) — use an integer or text key."
                } else {
                    "Use a uint or text key, or a table `{ * k => v }` in its own rule."
                };
                types.record_rejection(format!(
                    "rule `{source_name}`: unsupported fixed map key {other:?} — only uint \
                     and text fixed keys are implemented on the record path (the map-key \
                     write path and `{{name}}_key_encoding`). {remedy}"
                ));
                return None;
            }
            MapKeyKind::NonFixed => {
                // A non-fixed arrow entry (`* k => v` / `k => v`) in a map record is owned by
                // `recognize_rest_row` (run before this loop): a supported trailing `* k => v`
                // becomes the record's rest capture, and every unsupported placement/shape
                // already recorded a graceful rejection there. Either way the entry never
                // becomes a fixed field — skip it here without a second (duplicate) rejection.
                return None;
            }
            // keyless: fall through — the existing "map field has no key" rejection below
            // (which needs the field name) handles it exactly as before.
            MapKeyKind::Keyless => None,
        }
    } else {
        None
    };
    let field_name =
        group_entry_to_field_name(group_entry, index, generated_fields, optional_comma);
    // A field whose EMITTED identifier is a Rust keyword (a bareword `if` key, or `If` which
    // snake_cases to `if`) would emit invalid Rust caught only by the rustfmt gate. Reject it
    // gracefully at parse time in BOTH representations (the array shape `[if: uint]` is equally
    // affected). `field_name` is already the snake_cased emitted form, so checking it directly
    // catches the case-converted hazards too. The remedy renames the field without touching
    // the CBOR wire key (which stays the bareword text).
    if RUST_KEYWORDS.contains(&field_name.as_str()) {
        types.record_rejection(format!(
            "rule `{source_name}`: field `{field_name}` is a Rust keyword and cannot be a \
             struct field identifier. Rename the field with a `; @name <other>` comment \
             directive on that entry — the CBOR wire key is unchanged (it stays the bareword \
             text)."
        ));
        return None;
    }
    // A field whose EMITTED identifier is one of the fixed locals the generated
    // serialization bodies bind (`raw`, `len`, `read`, …) shadows that local: the crate
    // generates at exit 0 and fails `cargo check` two build steps from this CDDL line.
    // Checked on the RESOLVED name for the same reason the keyword guard above is — `Raw:`
    // snake_cases to `raw` and `; @name raw` renames INTO the hazard, while
    // `raw: bytes ; @name raw2` renames OUT of it and must pass.
    if let Some(msg) = generated_local_field_rejection(&field_name, source_name, rep, tagged) {
        types.record_rejection(msg);
        return None;
    }
    let rule_metadata = group_entry_rule_metadata(group_entry, optional_comma);
    // The RULE-SCOPED directives, refused at this member position by the shared seam the
    // single-entry group-choice arm also calls — that arm is a member position which mints
    // no record, so without one shared list the two spellings of "which directives does a
    // member position refuse" drift apart.
    // A plain GROUP rule's LAST entry is the one member slot the parser also binds the
    // RULE's trailing comment to (`pg = (a: uint, b: uint) ; @used_as_key` is the group
    // rule's documented directive slot, honored at rule level), so the type-scoped half is
    // suppressed exactly there and nowhere else — including non-last entries of the same
    // group, which no rule reading reaches.
    let rule_slot_shared = types.is_plain_group(name) && index + 1 == entry_count;
    reject_member_scoped_directives(
        types,
        &format!("field `{field_name}` of rule `{}`", source_name),
        "a field",
        &rule_metadata,
        rule_slot_shared,
    );
    // A wire-facts declaration (`@custom_encodings` / `@custom_wire_major`) is a property OF
    // the pair, and a field carries its own pair — so a declaration here with one half (or
    // none) describes no codec.
    if rule_metadata.custom_encodings.is_some() || rule_metadata.custom_wire_major.is_some() {
        reject_custom_encodings_without_pair(
            types,
            &format!("field `{field_name}` of rule `{source_name}`"),
            &rule_metadata,
        );
        // A field can carry the codec pair and its encoding tuple, but no reader consumes a
        // declared wire MAJOR there: major dispatch exists only before an open TABLE typed
        // row's key deserializer runs. Without this refusal the token parsed successfully
        // and then vanished from emitted code.
        if rule_metadata.custom_wire_major.is_some()
            && rule_metadata.custom_serialize.is_some()
            && rule_metadata.custom_deserialize.is_some()
        {
            types.record_rejection(format!(
                "@custom_wire_major on field `{field_name}` of rule `{source_name}`: nothing consumes the declared major. Only a transparent alias can carry it to an OPEN TABLE typed-row dispatch or a variable middle ARRAY boundary; a field-local codec has no such alias channel. Put the pair and declaration on a named alias (`<wire> = <inner> ; @custom_serialize <fn> @custom_deserialize <fn> @custom_wire_major <major>`) and use it at one of those boundaries, or remove the declaration."
            ));
        }
    }
    // A LONE half of the pair at a field/member position — the field twin of the record-rule
    // and transparent-alias single-half rejections, refused for their stated reason: one
    // position ends up with two wire forms. `generate_serialize`/`generate_deserialize` lift
    // each half independently, so the declared direction routes the named function while the
    // opposite direction keeps the FIELD TYPE's own generated codec, and the crate compiles
    // and ships that asymmetry silently. The complete pair stays accepted — it owns both
    // directions of this field. (A rule-TRAILING comment on a plain-group rule binds to that
    // group's last entry, the `@extern_companions` neighbour's seam, so this names the entry
    // the comment actually reached rather than the rule the author wrote it after.)
    if let Some((directive, declared, kept, missing)) = match (
        &rule_metadata.custom_serialize,
        &rule_metadata.custom_deserialize,
    ) {
        (Some(_), None) => Some((
            "@custom_serialize",
            "serialize path writes through the named function",
            "deserialize path keeps",
            "@custom_deserialize",
        )),
        (None, Some(_)) => Some((
            "@custom_deserialize",
            "deserialize path reads through the named function",
            "serialize path keeps",
            "@custom_serialize",
        )),
        _ => None,
    } {
        types.record_rejection(format!(
            "{directive} alone on field `{field_name}` of rule `{source_name}`: the field's \
             {declared} while its {kept} the field type's own generated codec — so the bytes \
             this field writes are not the bytes it reads back. Write both halves on this \
             entry (`; @custom_serialize <fn> @custom_deserialize <fn>`), adding the missing \
             {missing}, or move the pair to the member's TYPE rule if the format belongs to \
             the type."
        ));
    }
    // Lower and materialize the field type before checking its occurrence.
    // The optional plain-group refusal reads the resolved type through is_basic and resolve_alias_shallow.
    // Keep this order so alias materialization and type-level rejections precede occurrence rejections.
    let mut field_type = group_entry_to_type(types, parent_visitor, group_entry, cli);
    // A field spelled through an alias (`t = [ c: uint, kv_alias ]`) materializes the plain
    // group exactly like the direct `kv` reference: `is_basic`, which DOES shallow-resolve,
    // selects the splicing emission downstream.
    materialize_plain_group_ref(types, parent_visitor, &field_type, rep, cli);
    let mut optional_field = group_entry_optional(group_entry);
    // A count-permitting occurrence (`*`, `+`, `n*m` with bounds ≠ 1*1) on an ARRAY-record
    // field would be silently narrowed to a single mandatory item — a generated decoder
    // that rejects spec-valid CBOR with any other repetition count (invisible to
    // round-trip tests; only cross-producer data exposes it — the array analogue of the
    // map-path guard below). Unlike unique map keys, `+` does not collapse to exactly-one
    // in an array, so every marker except `?` and the pedantic `1*1` rejects.
    if rep == Representation::Array {
        if occurrence_permits_count(group_entry) {
            types.record_rejection(format!(
                "rule `{source_name}`: array field `{field_name}` has an occurrence \
                 (`*` / `+` / `n*m`), which would be silently narrowed to a single \
                 mandatory item (generated decoders would reject valid CBOR with a \
                 different repetition count). Use `?` for an optional item, a final-position \
                 `* t` rest tail after the fixed members, a homogeneous array (`[* t]`), or \
                 name the repeated part as its own array rule."
            ));
            return None;
        }
        // An OPTIONAL (`?`) plain-group field in an ARRAY-rep record. A plain group SPLICES
        // its members flat into the enclosing array, so nothing on the wire marks where the
        // optional group begins, and the embedded decoder length-checks only the members it
        // consumed — telling present from absent needs the group's mandatory member count
        // charged to the ENCLOSING read length before the group is read (either that, or a
        // second embedded deserialize method). That is the occurrence/bounds program's
        // territory, not a guard's; until it lands the shape must not reach emission, where
        // it aborted on `assertion failed: !config.optional_field` naming neither the
        // construct nor a remedy. The named-array remedy IS verified to generate, which is
        // what makes a refusal honest here.
        //
        // Guarded on `is_basic` over the RESOLVED member type, the same predicate and the
        // same one-seam placement as the map twin below: that is what makes the bare and
        // ALIAS (`? kv_alias`) spellings hit ONE message. The formerly silent TAGGED shape
        // (`? #6.1(kv)`) now reaches the earlier tag-payload semantic refusal instead.
        // Deliberately blanket over the group's own shape: a group whose members are ALL
        // optional is reachable and still refused, because the remedy serves it identically
        // and a narrower guard would buy a special case nothing has asked for. The
        // array-WRAPPED forms keep their own verdicts — `w = [kv]` is a Record, not a plain
        // group, and an inline `[kv]` member carries `basic_override` — so both fall outside
        // `is_basic` untouched.
        if optional_field
            && field_type.is_basic(types)
            && let ConceptualRustType::Rust(group_ident) =
                field_type.conceptual_type.resolve_alias_shallow()
        {
            let group_name = source_rule_name_of(types, group_ident);
            types.record_rejection(format!(
                "rule `{source_name}`: array field `{field_name}` is an OPTIONAL (`?`) \
                 reference to the plain group `{group_name}`, which is unsupported — a plain \
                 group splices its members flat into the enclosing array, so nothing on the \
                 wire marks where the optional group starts, and an embedded decoder \
                 length-checks only the members it consumed. Telling present from absent \
                 would need the group's mandatory member count charged to the enclosing \
                 read length before the group is read. Give the group its own array framing \
                 and reference that, which makes the optional item exactly ONE array element \
                 the decoder can test for: `w = [{group_name}]`, then `? w` in place of \
                 `? {group_name}`. (Dropping the `?` — splicing the group as a MANDATORY \
                 field — is supported as it stands.)"
            ));
            return None;
        }
    }
    let key = match rep {
        Representation::Map => {
            // `map_key` was classified before field naming (unsupported/non-fixed keys
            // already returned None); `Some` is a supported uint/text key, `None` is a
            // keyless entry that falls to the "map field has no key" rejection below.
            match map_key {
                Some(key) => {
                    // A zero-permitting occurrence on a unique fixed map key has exactly
                    // the wire states `?` has: the entry is absent, or it is present once.
                    // The unique-key invariant rules out every second occurrence, so even
                    // `*2` collapses faithfully to the existing Option field carrier.
                    // Lower bounds >= 1 remain mandatory for the same reason.
                    //
                    // `0*0` / `*0` are deliberately different: the key is forbidden, not
                    // optional. Mapping that to Option would let public Rust callers create
                    // `Some(value)` which the CDDL forbids. The record instead carries
                    // forbidden-key metadata and exposes no value member.
                    let occurrence = match group_entry {
                        GroupEntry::ValueMemberKey { ge, .. } => {
                            ge.occur.as_ref().map(|o| &o.occur)
                        }
                        _ => None,
                    };
                    let exactly_zero = matches!(
                        occurrence,
                        Some(Occur::Exact {
                            lower: Some(0) | None,
                            upper: Some(0),
                            ..
                        })
                    );
                    if exactly_zero {
                        // Exact zero is not an optional value.  Preserve the declaration as
                        // record-level constraint metadata: emitters omit its value surface,
                        // while the decoder and every open-rest construction door reject its
                        // fixed key before it can be captured.
                        reject_exact_zero_field_only_metadata(
                            types,
                            &field_name,
                            name,
                            &rule_metadata,
                            &field_type,
                        );
                        forbidden_fields.push(ForbiddenField {
                            key,
                            name: field_name,
                            rust_type: field_type,
                            source_index: index,
                        });
                        return None;
                    }
                    let permits_zero = matches!(
                        occurrence,
                        Some(Occur::ZeroOrMore { .. })
                            | Some(Occur::Exact { lower: None, .. })
                            | Some(Occur::Exact { lower: Some(0), .. })
                    );
                    optional_field |= permits_zero;
                    // A keyed member whose type resolves to a plain group can only be
                    // emitted as a flat splice, which writes more items than the key's own
                    // entry promised — refuse every spelling of it here, at the one seam
                    // the named / tagged / optional / alias / multi-entry-choice-arm
                    // members all pass through. `is_basic` is the same predicate
                    // `generate_serialize` uses to pick the splicing emission, so an
                    // array-WRAPPED group (`c: [kv]`, `basic_override`) keeps its own
                    // conflicting-representations refusal and the named-array remedy
                    // (`w = [kv]`, `c: w`) stays green.
                    if field_type.is_basic(types)
                        && let ConceptualRustType::Rust(group_ident) =
                            field_type.conceptual_type.resolve_alias_shallow()
                    {
                        let group_name = source_rule_name_of(types, group_ident);
                        record_plain_group_map_member_rejection(
                            types,
                            &format!("rule `{source_name}`"),
                            &field_name,
                            &group_name,
                        );
                        return None;
                    }
                    Some(key)
                }
                // A map-representation field without a key is unsupported by design (each
                // map field needs a key). This also catches a plain-group reference embedded
                // in a map record, which surfaces here as a keyless `TypeGroupname`. Record a
                // graceful rejection (drained by `finalize`) and drop the field rather than
                // `panic!` — nothing downstream runs on this record once a rejection exists.
                None => {
                    types.record_rejection(format!(
                        "rule `{source_name}`: map field `{field_name}` has no key. Each map field \
                         needs a key: use `k: v` / `k => v`, or a table `{{ * k => v }}`. \
                         (A plain-group reference embedded in a map-representation record hits \
                         this too — it is unsupported today.)"
                    ));
                    return None;
                }
            }
        }
        Representation::Array => None,
    };
    // RFC 8610 §3.8.2: a `.default` is what a decoder substitutes when the member is
    // ABSENT, so it is meaningful only for an OPTIONAL occurrence — on a mandatory member
    // there is no absent case for it to fill. Drop it here, at the one seam where a
    // field's optionality is known (and the seam a plain group's spliced entries arrive
    // through), so a mandatory member emits as a PLAIN mandatory field on every face:
    // the rust `new()` keeps its argument, and the wasm and WIT constructors that mirror
    // `new()` stay in agreement with it. (Left in place, the inert control moved the field
    // out of `new()` on the rust face only, and the mirrored constructors called it with
    // an argument it no longer took.)
    //
    // A warning rather than a refusal, because the default may be legitimately CARRIED
    // rather than spelled: `d = uint .default 0` is well-formed and useful at its optional
    // use sites (`? y: d`), while a mandatory `x: d` reference picks the same type up. This
    // seam cannot tell the two apart, and warning on both is the honest reading — the
    // control is inert either way.
    if !optional_field && field_type.config.default.is_some() {
        crate::warn!(
            "rule `{source_name}`: `.default` on the mandatory member `{field_name}` has \
             no effect (RFC 8610: a default substitutes for an ABSENT value, which is \
             meaningful only for an optional occurrence) — ignored. Mark the member \
             optional (`? {field_name}: …`) if the default was meant to apply."
        );
        field_type.config.default = None;
    }
    Some(
        RustField::new(field_name, field_type, optional_field, key, rule_metadata)
            .with_source_index(index),
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn parse_record_from_group_choice(
    types: &mut IntermediateTypes,
    rep: Representation,
    parent_visitor: &ParentVisitor,
    name: &RustIdent,
    group_choice: &GroupChoice,
    // Whether this record is one arm of a multi-arm group choice (`{ a } // { b }`). A rest row in
    // that position is rejected in v1 (collapsing an open map into an enum variant is unspecified),
    // so recognition is suppressed and the guard fires instead.
    in_choice_arm: bool,
    // Whether this record is the body of a TAGGED type (`#6.n([…])`). The tag read (`let tag =
    // raw.tag()?`) is emitted into this record's own deserializer, so it is the shape condition for
    // the `tag` entry of `GENERATED_LOCAL_RESERVED`.
    tagged: bool,
    cli: &Cli,
) -> RustRecord {
    let mut generated_fields = BTreeMap::<String, u32>::new();
    let mut forbidden_fields = Vec::new();
    let flattened = flatten_group_entries(&group_choice.group_entries, rep);
    let entry_count = flattened.len();
    // Open struct-map recognition (loose CBOR): a trailing `* K => V` arrow row after ≥1 fixed
    // entry becomes the record's `rest` capture instead of a rejected mixed non-fixed key. A
    // single-entry `{ * K => V }` never reaches here — table detection in `parse_group_type`
    // diverts it — so any non-fixed entry here is part of a multi-entry map. `rest_skip` marks the
    // recognized (or CANDIDATE-then-rejected) rows so the field loop skips them.
    let DynamicRows {
        typed_row,
        rest,
        array_segments,
        skip: rest_skip,
    } = recognize_dynamic_rows(
        types,
        rep,
        parent_visitor,
        name,
        &flattened,
        in_choice_arm,
        cli,
    );
    // Rejections cite the rule by its SOURCE spelling (`m`), not the camel-cased RustIdent (`M`).
    let source_name = source_rule_name_of(types, name);
    let fields: Vec<RustField> = flattened
        .into_iter()
        .enumerate()
        .filter_map(|(index, (group_entry, optional_comma))| {
            lower_record_field(
                types,
                rep,
                parent_visitor,
                name,
                &source_name,
                group_entry,
                optional_comma,
                index,
                entry_count,
                tagged,
                &rest_skip,
                &mut generated_fields,
                &mut forbidden_fields,
                cli,
            )
        })
        .collect();
    reject_encoding_companion_collisions(types, rep, name, &fields, &rest, &array_segments);
    reject_wasm_open_map_insert_collisions(types, cli, name, &fields, &rest);
    reject_array_segment_name_collisions(types, rep, name, &fields, &rest, &array_segments);
    RustRecord {
        rep,
        fields,
        forbidden_fields,
        rest,
        array_segments,
        typed_row,
    }
}

/// A captured open-map row exposes a wasm parent-mutation operation named `insert_<row>`.  That
/// public spelling intentionally follows the row name (rather than gaining an opaque suffix), so a
/// declared field whose ordinary wasm getter has that same name would otherwise emit two inherent
/// methods and leave an otherwise-valid CDDL rule as a non-compiling wasm crate.  Reject the wasm
/// profile deterministically and point at `@name`: the wire key stays unchanged, while silently
/// suffixing either public method would make the generated API depend on emitter internals.
fn reject_wasm_open_map_insert_collisions(
    types: &mut IntermediateTypes,
    cli: &Cli,
    name: &RustIdent,
    fields: &[RustField],
    rest: &Option<Box<RestRow>>,
) {
    if !cli.wasm {
        return;
    }
    let source_name = source_rule_name_of(types, name);
    // `rest` is already the sole catch-all: an open table's separate typed row owns the
    // established flattened `insert`, not an `insert_<row>` operation.
    for row in rest
        .iter()
        .filter(|row| !row.is_array_tail() && row.semantics == RestSemantics::Capture)
    {
        let method = format!("insert_{}", row.field_name);
        if let Some(field) = fields.iter().find(|field| {
            field.name == method
                // Mandatory fixed values have no wasm getter, hence no inherent-method clash.
                && (field.optional || !field.rust_type.conceptual_type.is_fixed_value())
        }) {
            types.record_rejection(format!(
                "rule `{source_name}`: captured map row `{}` generates wasm method `{method}()`, \
                 which collides with the wasm getter for field `{}`. Rename either member with \
                 `; @name <other>`; the CBOR key is unchanged. The generator does not suffix this \
                 public mutation door automatically.",
                row.field_name, field.name
            ));
        }
    }
}

/// Multiple array occurrence segments are public struct members, so their `@name` values share the
/// same namespace as fixed fields.  Keep this separate from the map-only wasm collision check:
/// array segment names do not mint mutation methods, but silently renaming either member would
/// change a public API and make preserve sidecars ambiguous.
fn reject_array_segment_name_collisions(
    types: &mut IntermediateTypes,
    rep: Representation,
    name: &RustIdent,
    fields: &[RustField],
    rest: &Option<Box<RestRow>>,
    array_segments: &[RestRow],
) {
    if rep != Representation::Array || array_segments.is_empty() {
        return;
    }
    let source_name = source_rule_name_of(types, name);
    let rows: Vec<&RestRow> = rest
        .iter()
        .filter(|row| row.is_array_tail())
        .map(Box::as_ref)
        .chain(array_segments.iter())
        .collect();
    for row in &rows {
        if RUST_KEYWORDS.contains(&row.field_name.as_str()) {
            types.record_rejection(format!(
                "rule `{source_name}`: array occurrence segment name `{}` is a Rust keyword. Give it a unique non-keyword `@name`.",
                row.field_name
            ));
        }
        if let Some(message) =
            generated_local_field_rejection(&row.field_name, &source_name, rep, false)
        {
            types.record_rejection(message.replace("field", "array occurrence segment"));
        }
        if fields.iter().any(|field| field.name == row.field_name) {
            types.record_rejection(format!(
                "rule `{source_name}`: array occurrence segment `{}` collides with fixed field `{}`. Give every segment a unique `@name`.",
                row.field_name, row.field_name
            ));
        }
    }
    for (index, row) in rows.iter().enumerate() {
        if rows[..index]
            .iter()
            .any(|other| other.field_name == row.field_name)
        {
            types.record_rejection(format!(
                "rule `{source_name}`: array occurrence segments would both emit field `{}`. Give every segment a unique `@name`.",
                row.field_name
            ));
        }
    }
}

/// The PAIRWISE half of the generated-local collision class: a record whose fields are individually
/// fine but whose NAMES stand in an `<f>` / `<f>_encoding` relation collide once
/// `--preserve-encodings` mints the per-field encoding companions (see
/// `ENCODING_COMPANION_SUFFIXES` for the three measured spellings and their error classes). Checked
/// on the RESOLVED names, after the whole field list is built, and uniformly across profiles — the
/// default profile compiles only because it mints no companions at all, so accepting the pair there
/// would hand back a spec that one flag breaks.
fn reject_encoding_companion_collisions(
    types: &mut IntermediateTypes,
    rep: Representation,
    name: &RustIdent,
    fields: &[RustField],
    rest: &Option<Box<RestRow>>,
    array_segments: &[RestRow],
) {
    // A single historic array tail has no companion-name interaction beyond fixed fields. Keep
    // that zero/one path byte-identical. In the multiple-segment form, fixed fields still mint
    // their ordinary `{field}_encoding` companions, while each segment mints a positional
    // `{segment}_elem_encodings` local. Both can collide with any public member name.
    let segments: Vec<&RestRow> = if array_segments.is_empty() {
        vec![]
    } else {
        rest.as_deref()
            .filter(|row| row.is_array_tail())
            .into_iter()
            .chain(array_segments.iter())
            .collect()
    };
    let names: Vec<&str> = fields
        .iter()
        .map(|field| field.name.as_str())
        .chain(segments.iter().map(|row| row.field_name.as_str()))
        .collect();
    let mut collisions = Vec::new();
    // Only fixed fields use the established `{field}_encoding` member/local scheme. A segment
    // named `foo` does NOT mint `foo_encoding`, so treating it as that base would gratuitously
    // reject a safe `foo` / `foo_encoding` segment pair.
    for base in fields.iter().map(|field| field.name.as_str()) {
        for (suffix, map_only, why) in ENCODING_COMPANION_SUFFIXES {
            if *map_only && rep != Representation::Map {
                continue;
            }
            let companion = format!("{base}{suffix}");
            if names.iter().any(|name| *name == companion) {
                collisions.push(((*base).to_owned(), companion, why.replace("{base}", base)));
            }
        }
    }
    // A captured array segment serializes/deserializes element encodings through a positional
    // `{segment}_elem_encodings` local. Its target may be either a fixed field or another occurrence
    // segment, so check the same resolved member namespace as above.
    for segment in &segments {
        let base = segment.field_name.as_str();
        let companion = format!("{base}_elem_encodings");
        if names.iter().any(|name| *name == companion) {
            collisions.push((
                base.to_owned(),
                companion.clone(),
                format!(
                    "the positional element-encoding companion minted for `{base}` is spelled \
                     `{companion}`, so that member's local shadows it in the struct-init shorthand (E0308)"
                ),
            ));
        }
    }
    if collisions.is_empty() {
        return;
    }
    let source_name = source_rule_name_of(types, name);
    for (base, other, why) in collisions {
        let why = why.replace("{other}", &other);
        types.record_rejection(format!(
            "rule `{source_name}`: fields `{base}` and `{other}` collide under \
             `--preserve-encodings` — {why} — so the emitted crate does not compile. Rename one of \
             them with a `; @name <other>` comment directive on that entry — the CBOR wire key is \
             unchanged."
        ));
    }
}
