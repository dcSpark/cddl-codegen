use super::*;
use cbor_event::Type as CBORType;

/// Rustdoc attached to a generated open struct-MAP type whose rest row is `@ignore` (tolerate-and-drop)
/// and to its `serialize` fn. The drop is invisible at the API surface (there is no `rest` field), so a
/// consumer has no other signal that the type is deliberately lossy — state the contract at the point of
/// use.
const IGNORE_LOSSINESS_DOC: &str = "Open struct-map with an ignored rest row: tolerates unknown map entries on deserialize and DROPS \
     them, and re-serializes only the declared fields. Byte round-trips do NOT hold for wire data that \
     carried unknown entries.";

/// The array analog of `IGNORE_LOSSINESS_DOC`: an open ARRAY whose rest tail is `@ignore`. Array-accurate
/// wording ("unknown trailing array elements", not "map entries") — a parallel const, not a reword of the
/// map one (whose text ships verbatim in blessed snapshots).
const IGNORE_LOSSINESS_DOC_ARRAY: &str = "Open array with an ignored rest tail: tolerates unknown trailing array elements on deserialize and \
     DROPS them, and re-serializes only the declared members. Byte round-trips do NOT hold for wire data \
     that carried extra trailing elements.";

/// The `@ignore` lossiness breadcrumb text for a rest row/tail, array-worded for an array tail and
/// map-worded for a map row.
fn ignore_lossiness_doc(rest: &RestRow) -> &'static str {
    if rest.is_array_tail() {
        IGNORE_LOSSINESS_DOC_ARRAY
    } else {
        IGNORE_LOSSINESS_DOC
    }
}

/// Combine a type's optional CDDL-derived doc with the `@ignore` lossiness breadcrumb (a blank line
/// between them when both are present), yielding the doc string to attach to the type / its serialize
/// fn. `ignored_rest` is the `@ignore` rest row/tail (`None` for capture / closed structs), so the
/// breadcrumb wording matches the rep (map entries vs trailing array elements).
fn ignore_aware_doc(base: Option<&str>, ignored_rest: Option<&RestRow>) -> Option<String> {
    match (base, ignored_rest) {
        (Some(d), Some(rest)) => Some(format!("{d}\n\n{}", ignore_lossiness_doc(rest))),
        (Some(d), None) => Some(d.to_owned()),
        (None, Some(rest)) => Some(ignore_lossiness_doc(rest).to_owned()),
        (None, None) => None,
    }
}

// generates serialization code for an array-encoded record into ser_func EXCEPT FOR array length
pub(super) fn generate_array_struct_serialization(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    vars_in_self: bool,
    ser_func: &mut dyn CodeBlock,
    cli: &Cli,
) {
    assert_eq!(record.rep, Representation::Array);
    let opt_self = if vars_in_self { "self." } else { "" };
    for field in record.fields.iter() {
        let field_expr = format!("{}{}", opt_self, field.name);
        if field.optional {
            if field.rust_type.is_fixed_value() {
                // Optional fixed value (any kind, including float): the `bool` presence field guards
                // writing the constant. generate_serialize ignores the data expr for a Fixed type
                // (it writes the literal — for float `write_special(Special::Float(<lit>))`), but
                // still reads the encoding var under --preserve-encodings (float aborts earlier at
                // the deserialize float stub before any of this ships).
                let mut opt_block = Block::new(format!("if {}{}", opt_self, field.name));
                let mut config = SerializeConfig::for_field(&field_expr, field);
                if vars_in_self {
                    config = config.encoding_var_in_option_struct("self.encodings")
                } else {
                    config = config.expr_is_ref(true).encoding_var_is_ref(true)
                }
                gen_scope.generate_serialize(
                    types,
                    (&field.rust_type).into(),
                    &mut opt_block,
                    config,
                    cli,
                );
                ser_func.push_block(opt_block);
                continue;
            }
            let (optional_field_check, field_expr, expr_is_ref) = if let Some(default_value) =
                &field.rust_type.config.default
            {
                (
                    if cli.preserve_encodings {
                        if vars_in_self {
                            // `as_ref()`: the encoding struct is not `Copy`, and a bare `map` over
                            // `self.encodings` behind `&self` moves out of it (E0507).
                            format!(
                                "if {} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false)",
                                field_expr,
                                default_value.to_primitive_str_compare(),
                                field.name
                            )
                        } else {
                            format!(
                                "if {} != {} || {}_default_present",
                                field_expr,
                                default_value.to_primitive_str_compare(),
                                field.name
                            )
                        }
                    } else {
                        format!(
                            "if {}{} != {}",
                            opt_self,
                            field.name,
                            default_value.to_primitive_str_compare()
                        )
                    },
                    field_expr.as_str(),
                    !vars_in_self,
                )
            } else {
                (
                    if vars_in_self {
                        format!("if let Some(field) = &self.{}", field.name)
                    } else {
                        format!("if let Some(field) = {}", field.name)
                    },
                    "field",
                    true,
                )
            };
            let mut optional_array_ser_block = Block::new(optional_field_check);
            let mut config = SerializeConfig::for_field(field_expr, field).expr_is_ref(expr_is_ref);
            if vars_in_self {
                config = config.encoding_var_in_option_struct("self.encodings")
            } else {
                config = config.expr_is_ref(true).encoding_var_is_ref(true)
            }
            gen_scope.generate_serialize(
                types,
                (&field.rust_type).into(),
                &mut optional_array_ser_block,
                config,
                cli,
            );
            ser_func.push_block(optional_array_ser_block);
        } else {
            let mut config = SerializeConfig::for_field(&field_expr, field);
            if vars_in_self {
                config = config.encoding_var_in_option_struct("self.encodings")
            } else {
                config = config.expr_is_ref(true).encoding_var_is_ref(true)
            }
            gen_scope.generate_serialize(types, (&field.rust_type).into(), ser_func, config, cli);
        }
    }
    // Open-array rest tail, CAPTURE: after the declared members, write each captured trailing element
    // into the owner's array (the array header already counts them via `definite_info`'s
    // `+ self.rest.len()` fold). `Vec` order = wire order = re-emit order by construction (no keys, no
    // canonical merge — the elements are positional). Under --preserve-encodings each element's
    // encoding is looked up POSITIONALLY from the `{field}_elem_encodings` sidecar by index (`.get(i)`,
    // exactly the array-FIELD element scheme) so non-canonical widths re-emit byte-exactly; a
    // self-carried `any` element carries its own encoding (no sidecar). Under --canonical-form the
    // per-element serialize normalizes each element recursively-canonically in position order (no sort,
    // no comparator — arrays are positional). IGNORE re-serializes ONLY the declared members (the whole
    // point of the tolerate-and-drop flavor) and has no field to iterate.
    if let Some(rest) = record.captured_rest().filter(|r| r.is_array_tail()) {
        let elem_encs = rest_array_elem_encoding_fields(types, rest, cli);
        let elem_var_name = format!("{}_elem", rest.field_name);
        // Outer config carries the sidecar namespace + the container's var name (`{field}`), so the
        // encoding lookup names `{field}_elem_encodings`.
        let mut outer = SerializeConfig::new("element", &rest.field_name);
        if vars_in_self {
            outer = outer.encoding_var_in_option_struct("self.encodings");
        }
        let mut tail_loop = if !elem_encs.is_empty() {
            let mut block = Block::new(format!(
                "for (i, element) in {opt_self}{}.iter().enumerate()",
                rest.field_name
            ));
            block.line(outer.container_encoding_lookup("elem", &elem_encs, "i"));
            block
        } else {
            Block::new(format!(
                "for element in {opt_self}{}.iter()",
                rest.field_name
            ))
        };
        let elem_config = SerializeConfig::new("element", &elem_var_name)
            .expr_is_ref(true)
            .is_end(false)
            .encoding_var_no_option_struct()
            .encoding_var_is_ref(false)
            .tag_depth(0);
        gen_scope.generate_serialize(
            types,
            rest.element().into(),
            &mut tail_loop,
            elem_config,
            cli,
        );
        ser_func.push_block(tail_loop);
    }
}

#[derive(Default, Debug)]
pub(super) struct ArrayStructDeserializeCode {
    pub(super) deser_code: DeserializationCode,
    // (var, expr)
    pub(super) deser_ctor_fields: Vec<(String, String)>,
    // (var, expr)
    pub(super) encoding_struct_ctor_fields: Vec<(String, String)>,
}

// generates deserialization code for an array-encoded record into deser_code EXCEPT FOR:
// 1) any final length check (so it can be used for generating embedded deserialization impls)
// 2) the final constructor block is not added to deser_code but has the vars/exprs returned in two vectors:
//    i) all root-level vars/exprs
//    ii) if Some, all vars/exprs that need to be put inside of an *Encodings struct's constructor
// so you will need to construct the constructor expression from these
#[allow(clippy::too_many_arguments)]
/// Every deserialize refusal an ARRAY-representation record earns from its own shape, derived
/// PURELY from the finalized IR — no emission, no `GenerationScope`, no dependence on when it runs.
///
/// This has to be a pure predicate rather than something the deserialize emitter records as it
/// goes, because the emission walk visits `rust_structs()` in IDENT order (a `BTreeMap`) and that
/// order bears no relation to reference order: for `ch = foo / tstr` the enum `Ch` is emitted
/// before the arm `Foo`, for `gc = [... // tstr]` the enum `Gc` before its arm `Gc0`. A containing
/// type that must consult "does this arm have a deserialize?" therefore cannot wait for the arm to
/// be emitted — the verdict is seeded for the whole IR up front (`seed_no_deserialize_verdicts`).
///
/// Callers: the record's own struct, and an INLINED group-choice arm record (whose refusals are
/// attributed to the enum that inlines it, since that is the type whose deserialize would break).
///
/// We can support optional fields, but only when they're immediately non-ambiguous, i.e. when the
/// next type (possibly skipping subsequent optional fields) is different from the current type.
/// Supporting the general case 100% is extremely complicated without a combinatorial backtrack but
/// for most sane real-world cases this wouldn't be necessary. Think purposefully written
/// edge-cases with multiple optional fields, possibly nested in other structs, and with many of
/// the same types, e.g. `[ ? uint, uint, ? (uint, text), ? text]`.
pub(super) fn array_record_deser_refusals(
    types: &IntermediateTypes,
    record: &RustRecord,
) -> Vec<String> {
    let mut reasons = Vec::new();
    for (field_index, field) in record.fields.iter().enumerate() {
        if !field.optional {
            continue;
        }
        let field_cbor_types = field.rust_type.cbor_types(types);
        // Whether this optional field is adjacent to the open rest tail (no mandatory declared
        // field sits between it and the tail). The tail joins the ambiguity analysis as a virtual
        // "field after every field": if the tail is reachable right after this optional and their
        // CBOR types overlap (`* any` overlaps EVERYTHING), a peek cannot tell "this optional field"
        // from "the first tail element", so the same refusal fires.
        let mut reaches_tail = true;
        for i in (field_index + 1)..record.fields.len() {
            if record.fields[i]
                .rust_type
                .cbor_types(types)
                .iter()
                .any(|ct| field_cbor_types.contains(ct))
            {
                reasons.push(format!(
                    "Array struct with potentially-ambiguous optional field {}: {:?}",
                    field.name, field.rust_type,
                ));
            }
            if !record.fields[i].optional {
                reaches_tail = false;
                break;
            }
        }
        if reaches_tail
            && let Some(rest) = &record.rest
            && rest
                .element()
                .cbor_types(types)
                .iter()
                .any(|ct| field_cbor_types.contains(ct))
        {
            reasons.push(format!(
                "Array struct optional field {} is ambiguous with the open rest tail element \
                 (overlapping CBOR types): a peek cannot distinguish the optional field from \
                 the first tail element. Make their types distinct, drop the optional, or drop \
                 the tail.",
                field.name,
            ));
        }
    }
    reasons
}

/// The MAP-representation twin of `array_record_deser_refusals`, same purity contract.
///
/// To support maps with plain groups inside is very difficult as we cannot guarantee the order of
/// fields — `foo = {a, b, bar}, bar = (c, d)` could have the order be `{a, d, c, b}`, `{c, a, b,
/// d}`, etc, which doesn't fit with the nature of `deserialize_as_embedded_group`. A possible
/// solution would be to take all fields into one big map, either in generation to begin with, or
/// just for deserialization then constructing at the end with locals like `a`, `b`, `bar_c`,
/// `bar_d`.
///
/// A KEYED plain-group member no longer reaches this: `record_plain_group_map_member_rejection`
/// refuses it in the parse walk, because declining only the DESERIALIZE left a serialize-only crate
/// whose serializer spliced the group's members in flat after a one-entry map header. The branch
/// below is kept as the backstop for anything that still builds a Map-rep record around a
/// plain-group field without passing that seam (a field carrying `basic_override`, which the seam's
/// `is_basic` predicate deliberately excludes) — if it ever fires, the parse-time seam is the thing
/// to widen, not this.
pub(super) fn map_record_deser_refusals(
    types: &IntermediateTypes,
    record: &RustRecord,
    cli: &Cli,
) -> Vec<String> {
    let mut reasons = Vec::new();
    for (_field_index, field) in record.canonical_ordering() {
        if let ConceptualRustType::Rust(ident) = &field.rust_type.conceptual_type
            && types.is_plain_group(ident)
        {
            reasons.push(format!(
                "Map with plain group field {}: {}",
                field.name,
                field.rust_type.for_rust_member(types, false, cli)
            ));
        }
    }
    reasons
}

/// (No `name` parameter: this emitter no longer records deserialize refusals — the verdict for
/// every ident is seeded from the IR before emission, see `array_record_deser_refusals`.)
pub(super) fn generate_array_struct_deserialization(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    tag: Option<usize>,
    in_embedded: bool,
    vars_in_self: bool,
    cli: &Cli,
) -> ArrayStructDeserializeCode {
    assert_eq!(record.rep, Representation::Array);
    let mut deser_code = DeserializationCode::default();
    let mut deser_ctor_fields = vec![];
    let mut encoding_struct_ctor_fields = vec![];
    for (field_index, field) in record.fields.iter().enumerate() {
        // Under preserve-encodings a fixed value with no encoding variation (bool / null) still has
        // NO binding target — `encoding_var_names_str` is empty — so a `let {} = ` LHS would be
        // invalid Rust (`let  = ...`). Gate the preserve branch on a non-empty binding and let those
        // fixed values fall through to the verify-only branch (same as non-preserve fixed values).
        let preserve_binding = cli
            .preserve_encodings
            .then(|| {
                encoding_var_names_str_for_field(
                    types,
                    &field.name,
                    &field.rust_type,
                    Some(&field.rule_metadata),
                    cli,
                )
            })
            .filter(|s| !s.is_empty());
        let (before, after) = if let Some(var_names_str) = preserve_binding {
            if cli.annotate_fields {
                (
                    Cow::from(format!("let {var_names_str} = ")),
                    Cow::from("?;"),
                )
            } else {
                (Cow::from(format!("let {var_names_str} = ")), Cow::from(";"))
            }
        } else if field.rust_type.is_fixed_value() {
            // don't set anything, only verify data
            if cli.annotate_fields {
                (Cow::from(""), Cow::from("?;"))
            } else if cli.preserve_encodings {
                // preserve bool/null: the deserialize emits a value expr only when a wrapping
                // path already pushed encoding exprs; the unbound unit case is suppressed at the
                // source (`DeserializeBeforeAfter::discards_value`) so no degenerate `();`
                // statement reaches the consumer. The `;` terminates whatever IS emitted.
                // (Non-preserve fixed deserialize asserts an empty `after` — it emits no value
                // at all — so keep it empty there.)
                (Cow::from(""), Cow::from(";"))
            } else {
                (Cow::from(""), Cow::from(""))
            }
        } else if cli.annotate_fields {
            (Cow::from(format!("let {} = ", field.name)), Cow::from("?;"))
        } else {
            (Cow::from(format!("let {} = ", field.name)), Cow::from(";"))
        };
        if field.optional {
            // The ambiguity refusals themselves live in `array_record_deser_refusals` (a pure
            // predicate over the IR, seeded before emission) — what stays here is only the codegen
            // this branch needs.
            let field_cbor_types = field.rust_type.cbor_types(types);
            let mut possibly_last_field = true;
            for i in (field_index + 1)..record.fields.len() {
                if !record.fields[i].optional {
                    if i < record.fields.len() - 1 {
                        possibly_last_field = false;
                    }
                    break;
                }
            }
            // we also need to be careful if we're possibly the last field in the CBOR
            // buffer to avoid raw.cbor_type()? throwing an error for CBOR(NotEnough(0, 0))
            let type_check_cond = if field_cbor_types.len() == 1 {
                let type_str = cbor_type_code_str(field_cbor_types[0]);
                if possibly_last_field {
                    // We also need to be careful if the last one is a non-Break special
                    // and the array is encoded using indefinite encoding.
                    // There's no nice way to access this as Deserializer::special_break() consumes
                    // the byte so we'll just inline this ugly code instead
                    if field_cbor_types.contains(&cbor_event::Type::Special) {
                        "if raw.as_slice().first().map(|byte: &u8| cbor_event::Type::from(*byte) == cbor_event::Type::Special && (*byte & 0b0001_1111) != 0x1f).unwrap_or(false)".to_owned()
                    } else {
                        format!("if raw.cbor_type().map(|ty| ty == {type_str}).unwrap_or(false)")
                    }
                } else {
                    format!("if raw.cbor_type()? == {type_str}")
                }
            } else {
                let types_str = field_cbor_types
                    .iter()
                    .map(|ty| cbor_type_code_str(*ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                if possibly_last_field {
                    // We also need to be careful if the last one is a non-Break special
                    // and the array is encoded using indefinite encoding.
                    // There's no nice way to access this as Deserializer::special_break() consumes
                    // the byte so we'll just inline this ugly code instead
                    if field_cbor_types.contains(&cbor_event::Type::Special) {
                        format!(
                            "if raw.as_slice().first().map(|byte: &u8| vec![{types_str}].contains(&cbor_event::Type::from(*byte)) && (*byte & 0b0001_1111) != 0x1f).unwrap_or(false)",
                        )
                    } else {
                        format!(
                            "if raw.cbor_type().map(|ty| vec![{types_str}].contains(&ty)).unwrap_or(false)"
                        )
                    }
                } else {
                    format!("if vec![{types_str}].contains(&raw.cbor_type()?)")
                }
            };
            if field.rust_type.is_fixed_value() {
                // === OPTIONAL FIXED value (any kind, including float) -> `bool` presence field ===
                // Peek the CBOR type; when it matches, verify the constant exactly as the
                // mandatory path does (FixedValueMismatch on the wrong value) and record `true`;
                // otherwise the presence stays `false`. Under --preserve-encodings the fixed value
                // additionally carries encoding var(s) (uint/nint/text widths), threaded into the
                // presence tuple `(true, enc)` alongside the bool exactly like the non-fixed
                // optional path threads `(Some(value), enc)`.
                let enc_fields = if cli.preserve_encodings {
                    field_encoding_fields(
                        types,
                        &field.name,
                        &field.rust_type,
                        Some(&field.rule_metadata),
                        false,
                        cli,
                    )
                } else {
                    vec![]
                };
                let enc_names = enc_fields
                    .iter()
                    .map(|enc| enc.field_name.clone())
                    .collect::<Vec<String>>();
                // LHS binds the presence bool, plus any encoding vars under preserve.
                let lhs = if enc_names.is_empty() {
                    field.name.clone()
                } else {
                    format!("({}, {})", field.name, enc_names.join(", "))
                };
                let defaults = if enc_fields.is_empty() {
                    "false".to_owned()
                } else {
                    format!(
                        "(false, {})",
                        enc_fields
                            .iter()
                            .map(|enc| enc.default_expr.to_owned())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                };
                let type_check_block = Block::new(format!("let {lhs} = {type_check_cond}"));
                let mut type_check_else = Block::new("else");
                let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
                if cli.preserve_encodings {
                    // Preserve: the fixed-value deserialize itself yields a Result whose Ok payload
                    // is the encoding expr(s) (`Some(enc)` for uint/nint/text) or unit `()` for the
                    // encoding-less bool/null. Map it to the presence tuple.
                    let some_map = if enc_names.is_empty() {
                        "|()| true".to_owned()
                    } else {
                        format!(
                            "|{}| (true, {})",
                            tuple_str(enc_names.clone()),
                            enc_names.join(", ")
                        )
                    };
                    if cli.annotate_fields {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", true),
                                deser_config,
                                cli,
                            )
                            .annotate(&field.name, "", &format!(".map({some_map})"))
                            .wrap_in_block(type_check_block)
                            .add_to_code(&mut deser_code);
                        type_check_else.line(format!("Ok({defaults})"));
                        type_check_else.after("?;");
                    } else if enc_names.is_empty() {
                        // encoding-less (bool/null): the deserialize emits its verify and, at this
                        // discarding position, no value expr at all — the appended `true` is the
                        // block's tail expression. The `;` terminates any value a wrapping
                        // encoding path does contribute.
                        let mut present = gen_scope.generate_deserialize(
                            types,
                            (&field.rust_type).into(),
                            DeserializeBeforeAfter::new("", ";", false),
                            deser_config,
                            cli,
                        );
                        present.content.line("true");
                        present
                            .wrap_in_block(type_check_block)
                            .add_to_code(&mut deser_code);
                        type_check_else.line(defaults);
                        type_check_else.after(";");
                    } else {
                        // uint/nint/text: build the `(true, enc)` tuple directly around the
                        // deserialize's encoding value expr.
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("(true, ", ")", false),
                                deser_config,
                                cli,
                            )
                            .wrap_in_block(type_check_block)
                            .add_to_code(&mut deser_code);
                        type_check_else.line(defaults);
                        type_check_else.after(";");
                    }
                } else {
                    // Non-preserve: verify only, no encoding vars — presence is a lone bool.
                    let mut present = gen_scope.generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new("", "", false),
                        deser_config,
                        cli,
                    );
                    if cli.annotate_fields {
                        present.content.line("Ok(true)");
                        present
                            .annotate(&field.name, "", "")
                            .wrap_in_block(type_check_block)
                            .add_to_code(&mut deser_code);
                        type_check_else.line("Ok(false)");
                        type_check_else.after("?;");
                    } else {
                        present.content.line("true");
                        present
                            .wrap_in_block(type_check_block)
                            .add_to_code(&mut deser_code);
                        type_check_else.line("false");
                        type_check_else.after(";");
                    }
                }
                deser_code.content.push_block(type_check_else);
            } else {
                let type_check_block = Block::new(format!("{before}{type_check_cond}"));
                let mut type_check_else = Block::new("else");
                if cli.annotate_fields {
                    let enc_fields = if cli.preserve_encodings {
                        field_encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type,
                            Some(&field.rule_metadata),
                            false,
                            cli,
                        )
                    } else {
                        vec![]
                    };
                    let (some_map, defaults) = if !enc_fields.is_empty() {
                        let enc_names_str = enc_fields
                            .iter()
                            .map(|enc| enc.field_name.clone())
                            .collect::<Vec<String>>()
                            .join(", ");
                        (
                            Cow::from(format!(
                                "|({}, {})| (Some({}), {})",
                                field.name, enc_names_str, field.name, enc_names_str
                            )),
                            Cow::from(format!(
                                "(None, {})",
                                enc_fields
                                    .iter()
                                    .map(|enc| enc.default_expr.to_owned())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            )),
                        )
                    } else {
                        (Cow::from("Some"), Cow::from("None"))
                    };
                    let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
                    gen_scope
                        .generate_deserialize(
                            types,
                            (&field.rust_type).into(),
                            DeserializeBeforeAfter::new("", "", true),
                            deser_config,
                            cli,
                        )
                        .annotate(&field.name, "", &format!(".map({some_map})"))
                        .wrap_in_block(type_check_block)
                        .add_to_code(&mut deser_code);
                    type_check_else.line(format!("Ok({defaults})"));
                } else {
                    // The same enc-field split the annotate branch above makes, and for the same
                    // reason: the `Option` belongs on the VALUE slot alone, never around the whole
                    // encoding tuple the binding pattern destructures. With no annotate closure to
                    // re-shape inside, the re-shape rides the value expression itself, in the
                    // spelling the nullable arm's `Some`-mapping already uses (`Result::<_, _>::Ok`
                    // so the error type is pinned, `.map(..)?` to distribute).
                    let enc_fields = if cli.preserve_encodings {
                        field_encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type,
                            Some(&field.rule_metadata),
                            false,
                            cli,
                        )
                    } else {
                        vec![]
                    };
                    let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
                    let (some_before, some_after, defaults) = if enc_fields.is_empty() {
                        ("Some(".to_owned(), ")".to_owned(), "None".to_owned())
                    } else {
                        let enc_names_str = enc_fields
                            .iter()
                            .map(|enc| enc.field_name.clone())
                            .collect::<Vec<String>>()
                            .join(", ");
                        (
                            "Result::<_, DeserializeError>::Ok(".to_owned(),
                            format!(
                                ").map(|({}, {})| (Some({}), {}))?",
                                field.name, enc_names_str, field.name, enc_names_str
                            ),
                            // Slot order is the binding pattern's, which is the enc-field order —
                            // the same list the `map` binds — so a defaulted absent field can never
                            // land its encodings in a neighbour's slot.
                            format!(
                                "(None, {})",
                                enc_fields
                                    .iter()
                                    .map(|enc| enc.default_expr.to_owned())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ),
                        )
                    };
                    gen_scope
                        .generate_deserialize(
                            types,
                            (&field.rust_type).into(),
                            DeserializeBeforeAfter::new(&some_before, &some_after, false),
                            deser_config,
                            cli,
                        )
                        .wrap_in_block(type_check_block)
                        .add_to_code(&mut deser_code);
                    type_check_else.line(&defaults);
                    if !enc_fields.is_empty() {
                        // the `?` the distribution above appends
                        deser_code.throws = true;
                    }
                }
                type_check_else.after(after);
                deser_code.content.push_block(type_check_else);
                // A `.default` member is stored PLAIN — the default fills the absent case — so the
                // `Option<T>` the presence peek above binds has to collapse before the constructor
                // reads it. This is the twin of the map path's `unwrap_or` (`Ok(Self)` assembly),
                // and it must live here rather than at the constructor because the array path's
                // binding is a tuple under --preserve-encodings.
                if let Some(default_value) = &field.rust_type.config.default {
                    if cli.preserve_encodings {
                        // The serializer skips a member equal to its default UNLESS it was
                        // explicitly present on the wire, which is what keeps a byte-exact re-emit
                        // byte-exact. Computed BEFORE the collapse — after it, present-and-equal
                        // and absent are the same value.
                        deser_code.content.line(&format!(
                            "let {}_default_present = {} == Some({});",
                            field.name,
                            field.name,
                            default_value.to_primitive_str_assign()
                        ));
                    }
                    // `unwrap_or_else` for the one owned kind, to avoid clippy::or_fun_call —
                    // the same split the map path makes.
                    let collapse = match default_value {
                        FixedValue::Text(_) | FixedValue::Bytes(_) => format!(
                            "let {} = {}.unwrap_or_else(|| {});",
                            field.name,
                            field.name,
                            default_value.to_primitive_str_assign()
                        ),
                        FixedValue::Bool(_)
                        | FixedValue::Nint(_)
                        | FixedValue::Null
                        | FixedValue::Undefined
                        | FixedValue::Float(_)
                        | FixedValue::Uint(_) => format!(
                            "let {} = {}.unwrap_or({});",
                            field.name,
                            field.name,
                            default_value.to_primitive_str_assign()
                        ),
                    };
                    deser_code.content.line(&collapse);
                }
            }
        } else {
            // mandatory fields
            if cli.annotate_fields {
                let deser_config = DeserializeConfig::for_field(field, in_embedded, false);
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new("", "", true),
                        deser_config,
                        cli,
                    )
                    .annotate(&field.name, before.as_ref(), after.as_ref())
                    .add_to_code(&mut deser_code);
            } else {
                let deser_config = DeserializeConfig::for_field(field, in_embedded, false);
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new(before.as_ref(), after.as_ref(), false),
                        deser_config,
                        cli,
                    )
                    .add_to_code(&mut deser_code);
            }
        }
        // A non-fixed field (its value) and an optional fixed field of any kind (its `bool`
        // presence) both contribute a struct field to the constructor; a mandatory fixed value
        // (zero information) does not — i.e. only `fixed && !optional` is skipped.
        if !field.rust_type.is_fixed_value() || field.optional {
            deser_ctor_fields.push((field.name.clone(), field.name.clone()));
        }
    }
    // Open-array rest tail: after the straight-line fixed prefix, read the trailing elements. CAPTURE
    // stages each typed element in a `Vec`; a one-or-more tail enters `NonEmptyVec::try_from` exactly
    // once at final assembly, while a loose tail moves that Vec directly. IGNORE typed-deserializes and
    // DROPS it (both advance the stream past nested containers — the stream-position regression class).
    // The loop reads until the definite length is exhausted (`read_len.read() < n`, where `read_len`
    // already accounts the prefix) or, for an indefinite array, the `0xff` break byte is reached — a
    // NON-consuming peek (`raw.as_slice().first()`), so the break stays on the wire for
    // `add_deserialize_final_len_check` to consume (it reads the break itself for indefinite arrays).
    // Under --preserve-encodings the CAPTURE flavor additionally records each element's encoding into a
    // POSITIONAL `{field}_elem_encodings: Vec<..>` sidecar (byte-exact re-emit; a self-carried `any`
    // element needs none). IGNORE is rejected under --preserve-encodings, so it never captures
    // encodings.
    if let Some(rest) = &record.rest {
        let element = rest.element();
        deser_code.read_len_used = true;
        deser_code.len_used = true;
        deser_code.throws = true;
        // Per-element encoding sidecar (preserve + capture + concrete element): mirrors the array-FIELD
        // `_elem_encodings` scheme. Empty otherwise (non-preserve, ignore, or self-carried `any`).
        let elem_encs = if rest.semantics == RestSemantics::Capture {
            rest_array_elem_encoding_fields(types, rest, cli)
        } else {
            vec![]
        };
        let elem_var_name = format!("{}_elem", rest.field_name);
        if rest.semantics == RestSemantics::Capture {
            deser_code
                .content
                .line(&format!("let mut {} = Vec::new();", rest.field_name));
            if !elem_encs.is_empty() {
                deser_code.content.line(&format!(
                    "let mut {}_elem_encodings = Vec::new();",
                    rest.field_name
                ));
            }
        }
        let mut tail_loop = Block::new(format!(
            "while match len {{ {} => read_len.read() < n, {} => raw.as_slice().first() != Some(&0xff), }}",
            cbor_event_len_n("n", cli),
            cbor_event_len_indef(cli)
        ));
        tail_loop.line("read_len.read_elems(1)?;");
        match rest.semantics {
            RestSemantics::Capture => {
                if !elem_encs.is_empty() {
                    // Bind `(value, <enc vars>)` and push each into its parallel `Vec`.
                    let elem_var_names_str =
                        encoding_var_names_str(types, &elem_var_name, element, cli);
                    gen_scope
                        .generate_deserialize(
                            types,
                            element.into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {elem_var_names_str} = "),
                                ";",
                                false,
                            ),
                            DeserializeConfig::new(&elem_var_name),
                            cli,
                        )
                        .add_to(&mut tail_loop);
                    tail_loop.line(format!("{}.push({elem_var_name});", rest.field_name));
                    tail_loop.line(format!(
                        "{}_elem_encodings.push({});",
                        rest.field_name,
                        tuple_str(elem_encs.iter().map(|e| e.field_name.clone()).collect())
                    ));
                } else {
                    gen_scope
                        .generate_deserialize(
                            types,
                            element.into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {elem_var_name} = "),
                                ";",
                                false,
                            ),
                            DeserializeConfig::new(&elem_var_name),
                            cli,
                        )
                        .add_to(&mut tail_loop);
                    tail_loop.line(format!("{}.push({elem_var_name});", rest.field_name));
                }
            }
            RestSemantics::Ignore => {
                gen_scope
                    .generate_deserialize(
                        types,
                        element.into(),
                        DeserializeBeforeAfter::new("let _rest_elem = ", ";", false),
                        DeserializeConfig::new("_rest_elem"),
                        cli,
                    )
                    .add_to(&mut tail_loop);
            }
        }
        deser_code.content.push_block(tail_loop);
        if rest.semantics == RestSemantics::Capture {
            let rest_expr = if rest.is_non_empty_array_tail() {
                format!("NonEmptyVec::try_from({})?", rest.field_name)
            } else {
                rest.field_name.clone()
            };
            deser_ctor_fields.push((rest.field_name.clone(), rest_expr));
            if !elem_encs.is_empty() {
                let sidecar = format!("{}_elem_encodings", rest.field_name);
                if vars_in_self {
                    encoding_struct_ctor_fields.push((sidecar.clone(), sidecar));
                } else {
                    deser_ctor_fields.push((sidecar.clone(), sidecar));
                }
            }
        }
    }
    if cli.preserve_encodings {
        let encoding_vars_output = if vars_in_self {
            &mut encoding_struct_ctor_fields
        } else {
            // no explicit encoding struct - dump in with other regular fields
            &mut deser_ctor_fields
        };
        encoding_vars_output.push(("len_encoding".to_owned(), "len_encoding".to_owned()));
        if tag.is_some() {
            encoding_vars_output.push(("tag_encoding".to_owned(), "Some(tag_encoding)".to_owned()));
        }
        for field in record.fields.iter() {
            for field_enc in field_encoding_fields(
                types,
                &field.name,
                &field.rust_type,
                Some(&field.rule_metadata),
                true,
                cli,
            ) {
                encoding_vars_output
                    .push((field_enc.field_name.clone(), field_enc.field_name.clone()));
            }
        }
    }
    // length checked inside of deserialize() - it causes problems for plain groups nested
    // in other groups otherwise
    ArrayStructDeserializeCode {
        deser_code,
        deser_ctor_fields,
        encoding_struct_ctor_fields,
    }
}

/// Builds one map-record field's deserialize `match` arm (the uint/text key dispatch header,
/// dup-key check, value deserialize + temp-var wiring, and key-encoding capture). Returns the
/// finished `Block`; the caller routes it into `uint_field_deserializers`/`text_field_deserializers`.
/// `deser_code` is threaded only for `mark_and_extract_content` (folding the arm body's throws/
/// read_len bookkeeping into the surrounding deserialize code).
/// Dispatch shape of an open struct-map rest row's key domain.
///
/// The first three are the FAST (peeked-key) path: the record loop's `cbor_type()` dispatch has
/// already read the key, so the capture reconstructs it from the parts in hand. `Typed` is the
/// general path: nothing is reconstructed, `K::deserialize` reads the key itself from `raw` — after a
/// rewind to the loop-body anchor when a declared-key match arm consumed the bytes first.
/// `RestRow::map_key_uses_peeked_path` decides which (see there for why each shape routes as it
/// does).
#[derive(Clone, Copy, PartialEq, Eq)]
enum RestKeyDomain {
    Uint,
    Text,
    Any,
    Typed,
}

/// `@duplicates preserve` on the rest row → the vec-of-pairs twin (`PairMap`), which accepts AND
/// re-emits duplicate keys in wire order (matching `@duplicates preserve` TABLES). Otherwise the
/// loose container (`OrderedHashMap`/`BTreeMap`) with the value-duplicate rejection (accept/reject
/// keyed on CBOR VALUE equality, not the domain's spelling).
fn rest_is_pair_map(rest: &RestRow) -> bool {
    rest.duplicates() == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
}

/// The rest container CONSTRUCTOR token: `Vec` for an array tail's decode staging (the min-one form
/// routes that Vec through `NonEmptyVec::try_from` at final assembly); for a map row
/// `PairMap` (`@duplicates preserve`) / `OrderedHashMap` / `BTreeMap`.
fn rest_container_ctor(rest: &RestRow, cli: &Cli) -> &'static str {
    if rest.is_array_tail() {
        "Vec"
    } else if rest_is_pair_map(rest) {
        "PairMap"
    } else {
        table_type(cli)
    }
}

/// The rest field's member RUST TYPE: `Vec<T>` for an array `* t` tail; for a map row
/// `PairMap<K, V>` / `OrderedHashMap<K, V>` / `BTreeMap<K, V>` (a `@duplicates preserve` policy on the
/// `Map` `RustType` routes `for_rust_member` to the pair-map twin, reusing the table machinery).
///
/// The container spelling itself is `RestRow::container_type` (the IR), shared with the wasm
/// wrapper mint and the dependency walk so the three cannot drift.
fn rest_member_type(rest: &RestRow) -> crate::intermediate::RustType {
    rest.container_type()
}

impl RestKeyDomain {
    fn of(types: &IntermediateTypes, rest: &RestRow) -> Self {
        if !rest.map_key_uses_peeked_path(types) {
            return RestKeyDomain::Typed;
        }
        match rest.domain().conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Any => RestKeyDomain::Any,
            ConceptualRustType::Primitive(Primitive::Str) => RestKeyDomain::Text,
            _ => RestKeyDomain::Uint,
        }
    }
}

/// The encoding sidecars a rest row needs under `--preserve-encodings`: `any`-typed key/value carry
/// their own encodings (self-carried `AnyCbor`), so they contribute NO sidecar; concrete key/value
/// types get a `{restfield}_key_encodings` / `{restfield}_value_encodings` `BTreeMap` keyed by the
/// key VALUE (the owner's map header covers the rest row's own length, so there is no `_encoding`
/// LenEncoding field). Empty vecs under non-preserve or for self-carried `any`.
fn rest_encoding_fields(
    types: &IntermediateTypes,
    rest: &RestRow,
    cli: &Cli,
) -> (Vec<EncodingField>, Vec<EncodingField>) {
    if !cli.preserve_encodings {
        return (vec![], vec![]);
    }
    // DECLARED types, not `.resolve_aliases()`d: both of these `type_name`s reach a type
    // DECLARATION (`{restfield}_{key,value}_encodings`), so they owe the member's declared spelling
    // (see `EncodingField::type_name`). The domain is leaf-only today — a rest row rejects key
    // domains other than `uint`/`text`/`any`, so its encoding fields carry no container to spell —
    // and passes the declared type anyway, so widening the accepted domains cannot silently
    // reintroduce a resolved spelling on one half of this function.
    let key_encs = encoding_fields(
        types,
        &format!("{}_key", rest.field_name),
        rest.domain(),
        false,
        cli,
    );
    let value_encs = encoding_fields(
        types,
        &format!("{}_value", rest.field_name),
        rest.range(),
        false,
        cli,
    );
    (key_encs, value_encs)
}

/// The per-element encoding sidecar an array `* t` rest tail needs under `--preserve-encodings`: the
/// element type's own encoding fields, named under `{restfield}_elem` — mirroring the array-FIELD
/// element-encoding scheme (`{field}_elem_encodings: Vec<..>`), since a tail element is re-serialized
/// exactly like an array element. A self-carried `any` element (`* any`) contributes none (the
/// `AnyCbor` value carries its own encoding). Empty under non-preserve. The tail's own length lives in
/// the owner array's `len_encoding` (no `_encoding` LenEncoding field here).
fn rest_array_elem_encoding_fields(
    types: &IntermediateTypes,
    rest: &RestRow,
    cli: &Cli,
) -> Vec<EncodingField> {
    if !cli.preserve_encodings {
        return vec![];
    }
    // DECLARED type (see `EncodingField::type_name`): this reaches the
    // `{restfield}_elem_encodings` declaration.
    encoding_fields(
        types,
        &format!("{}_elem", rest.field_name),
        rest.element(),
        false,
        cli,
    )
}

/// The rest field's FLATTENED JSON surface (`output_format.mdx` § "Open struct-maps (rest rows)").
/// Emits, into the
/// struct's module, the per-struct `serialize_with`/`deserialize_with` free functions that steer the
/// rest field's serde derive so its captured entries render at the SAME JSON object level as the
/// declared fields (serde `flatten`), and reads them back symmetrically. The write-side collision
/// check needs the DECLARED JSON names — codegen-known but not visible to a generic adapter — so the
/// serialize fn closes over them as a literal slice and delegates the mechanics to the static
/// `serialize_flattened_rest` / `read_flattened_rest_pairs` helpers (composition, not a parallel
/// path: values ride the natural walk via `NaturalAnyCborSer`/`De`). Returns the field-attribute
/// lines to attach to the rest field; a no-op (empty) when neither json flag is on.
fn emit_rest_flatten_json(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    record: &RustRecord,
    rest: &RestRow,
    cli: &Cli,
) -> Vec<String> {
    if !(cli.json_serde_derives || cli.json_schema_export) {
        return Vec::new();
    }
    // The flattened-rest JSON surface is a MAP-only construct (unknown KEYS at the object's top level).
    // An array `* t` rest tail has no keys: its captured `Vec<T>` field renders as an ordinary JSON
    // array under the field name (the honest symmetric rendering — array positions are already erased
    // in JSON), via the field's own plain serde derive. So emit no flatten steering for an array tail.
    if rest.is_array_tail() {
        return Vec::new();
    }
    // The `any`-domain key/value views live in `any_cbor`; the domain-agnostic flatten helpers live
    // in the `any`-free `open_struct_rest_json` module (so a fully-typed rest row needs no AnyCbor).
    let base = format!("{}::any_cbor", cli.common_import_rust());
    let flatten = format!("{}::open_struct_rest_json", cli.common_import_rust());
    let container_ty = rest_member_type(rest).for_rust_member(types, false, cli);
    let range_is_any = matches!(
        rest.range().conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Any
    );
    // snake_case the owner name so the free fns are snake (no `non_snake_case` warning) and unique
    // (struct idents are unique; `convert_to_snake_case` is injective enough here as the field name
    // and fixed suffixes disambiguate).
    let owner_snake = crate::utils::convert_to_snake_case(name.as_ref());
    let ser_fn = format!("{}_{}_flatten_serialize", owner_snake, rest.field_name);
    let deser_fn = format!("{}_{}_flatten_deserialize", owner_snake, rest.field_name);
    let domain = RestKeyDomain::of(types, rest);

    let mut annotations = Vec::new();
    if cli.json_serde_derives {
        annotations.push("#[serde(flatten)]".to_owned());
        annotations.push(format!(
            "#[serde(serialize_with = \"{ser_fn}\", deserialize_with = \"{deser_fn}\")]"
        ));
    }
    if cli.json_schema_export {
        // The rest field's flattened JSON is produced by the GENERATED flatten fns above, identically
        // for every rest container — so the schema of the open region belongs to the rest-row POSITION
        // (key domain × value type), never to the container's own `JsonSchema`. The honest open-map
        // shape either way: for an `any` range the permissive "any JSON value" (json2ts → a
        // `{ [k: string]: unknown }` index signature intersected with the declared `properties`); for
        // a TYPED range the `BTreeMap<K, V>` schema (uint keys → `patternProperties "^\d+$"`, text
        // keys → `additionalProperties`).
        //
        // For the primitive uint/text domains only the loose (non-preserve) container is left to
        // contribute that typed schema itself: it IS the `BTreeMap`/`OrderedHashMap` the position
        // calls for, so its delegation and the position's answer are the same bytes. The
        // `@duplicates preserve` twin is array-shaped (`PairMap` → `Vec<(K, V)>`, honest for a
        // standalone duplicate-permitting table, wrong for a FLATTENED row: no index signature, plus
        // array keywords merged onto the parent object), so it names the position's schema
        // explicitly — via a helper that delegates to the same `BTreeMap<K, V>`, making the two
        // containers schema-indistinguishable by construction.
        //
        // A GENERAL typed `K` breaks that "the loose container speaks for itself" premise, so BOTH
        // containers name the position's schema there: the member names are `K`'s key-string image,
        // which `BTreeMap<K, V>`'s schema (derived from `K`'s own VALUE schema) describes wrongly —
        // see `general_key_rest_map_schema`.
        if range_is_any {
            annotations.push(format!(
                "#[schemars(schema_with = \"{base}::natural_any_cbor_map_schema\")]"
            ));
        } else if domain == RestKeyDomain::Typed {
            let value_ty = rest.range().for_rust_member(types, false, cli);
            annotations.push(format!(
                "#[schemars(schema_with = \"{flatten}::general_key_rest_map_schema::<{value_ty}>\")]"
            ));
        } else if rest_is_pair_map(rest) {
            let key_ty = rest.domain().for_rust_member(types, false, cli);
            let value_ty = rest.range().for_rust_member(types, false, cli);
            annotations.push(format!(
                "#[schemars(schema_with = \"{flatten}::typed_rest_map_schema::<{key_ty}, {value_ty}>\")]"
            ));
        }
    }
    // Nothing more to emit unless the serde derives (not just the schema) are on — the
    // serialize_with/deserialize_with functions only exist under --json-serde-derives.
    if !cli.json_serde_derives {
        return annotations;
    }

    // Declared JSON member names (reserved on the write side): every field that materializes a struct
    // member — i.e. NOT a mandatory fixed value (which carries zero info and emits no JSON key).
    let reserved: Vec<String> = record
        .fields
        .iter()
        .filter(|f| !f.rust_type.is_fixed_value() || f.optional)
        .map(|f| format!("{:?}", f.name.to_string()))
        .collect();
    let reserved_lit = format!("&[{}]", reserved.join(", "));

    let field = &rest.field_name;
    // Per-domain key <-> string (RFC 8949 §6.1 write, §6.2 read).
    let (key_closure, key_coerce) = match domain {
        // A general `K` images through the `any` domain's key convention applied to `K`'s OWN CBOR
        // bytes (text verbatim, uint/nint decimal, everything else a loud write error), and reads
        // back numeric-preferred with a fallback to the text reading — see the two static helpers.
        //
        // Both directions name their trait FULLY QUALIFIED rather than calling a method: the flatten
        // fns are emitted into the generated `mod.rs`, which reaches the traits through
        // `use serialization::*;` — and under `--common-import-override` / an extern dep that glob
        // brings in nothing (the consumer's `serialization.rs` only privately globs the dependency's).
        // A `k.to_cbor_bytes()` spelling would compile in single-crate fixtures and fail in
        // split-crate consumers. The write trait forks with the flags exactly as the wasm
        // `to_cbor_bytes` emission does; `Deserialize` has one shape in every flavor.
        RestKeyDomain::Typed => {
            let key_ty = rest.domain().for_rust_member(types, false, cli);
            typed_rest_key_json_arms(rest, &key_ty, &flatten, cli)
        }
        // A typed-domain key stringify never fails, so the error type is unconstrained by the body —
        // pin it (`Infallible: Display`) so the generic helper's `E: Display` bound resolves.
        RestKeyDomain::Uint => (
            "|k: &u64| Ok::<String, core::convert::Infallible>(k.to_string())".to_owned(),
            "let k = ks.parse::<u64>().map_err(|_| serde::de::Error::custom(\
             format!(\"open struct-map rest key {ks:?} is not a valid uint\")))?;"
                .to_owned(),
        ),
        RestKeyDomain::Text => (
            "|k: &String| Ok::<String, core::convert::Infallible>(k.clone())".to_owned(),
            "let k = ks;".to_owned(),
        ),
        RestKeyDomain::Any => (
            format!("{base}::any_cbor_natural_key_string"),
            format!("let k = {base}::any_cbor_natural_key_from_string(&ks);"),
        ),
    };
    // Per-range value view: an `any` range renders NATURALLY (the natural walk); a typed range uses its own
    // serde (which, if it transitively contains `any`, is itself steered by the natural adapters).
    // A typed range therefore needs NO write-side value view at all: its `(&K, &V)` pairs already match
    // the helper's `Item = (&K, W)`, so the entries expression is the bare `.iter()` — emitting the
    // `any` shape's `.map(|(k, v)| (k, v))` there would be an identity map (`clippy::map_identity` in
    // the generated crate, which consumers build lint-clean).
    let (entries_expr, value_de_ty, value_unwrap) = if range_is_any {
        (
            format!("{field}.iter().map(|(k, v)| (k, {base}::NaturalAnyCborSer(v)))"),
            format!("{base}::NaturalAnyCborDe"),
            "v.0".to_owned(),
        )
    } else {
        (
            format!("{field}.iter()"),
            rest.range().for_rust_member(types, false, cli),
            "v".to_owned(),
        )
    };

    let functions = format!(
        "fn {ser_fn}<S: serde::Serializer>(\n\
         \x20   {field}: &{container_ty},\n\
         \x20   serializer: S,\n\
         ) -> Result<S::Ok, S::Error> {{\n\
         \x20   {flatten}::serialize_flattened_rest(\n\
         \x20       {reserved_lit},\n\
         \x20       {key_closure},\n\
         \x20       {entries_expr},\n\
         \x20       serializer,\n\
         \x20   )\n\
         }}\n\
         \n\
         fn {deser_fn}<'de, D: serde::Deserializer<'de>>(\n\
         \x20   deserializer: D,\n\
         ) -> Result<{container_ty}, D::Error> {{\n\
         \x20   let pairs: Vec<(String, {value_de_ty})> =\n\
         \x20       {flatten}::read_flattened_rest_pairs(deserializer)?;\n\
         \x20   pairs\n\
         \x20       .into_iter()\n\
         \x20       .map(|(ks, v)| {{\n\
         \x20           {key_coerce}\n\
         \x20           Ok((k, {value_unwrap}))\n\
         \x20       }})\n\
         \x20       .collect()\n\
         }}\n"
    );
    gen_scope.rust(types, name).raw(&functions);
    annotations
}

/// The `(key_closure, key_coerce)` pair for a TYPED rest-row key domain — the JSON member-name image
/// of `K` and the reading that recovers `K` from a member name.
///
/// The convention is the `any` domain's applied to `K`'s WIRE image (text verbatim, uint/nint in
/// decimal, every other CBOR major type a loud error at write and an unreadable member name at read),
/// so the two halves are stated per key shape rather than derived from `K`'s JSON serde — a rest key
/// is an object MEMBER NAME, which `K`'s own JSON rendering does not describe.
///
/// Two routes, because a rest key's CBOR shape is known statically for a PRIMITIVE `K` and only
/// dynamically for a nominal one:
///
/// * A primitive `K` states its image directly (no CBOR round-trip). This also keeps the typed
///   `uint`-ish/`text` domains reading exactly like their bare (peeked-path) siblings, so which side
///   of the CBOR routing rule a row falls on never changes its JSON.
/// * A nominal `K` (a generated struct/union/newtype, an extern, a nominalized collection) images
///   through its OWN CBOR bytes: `typed_rest_key_string` reads the head, and `rest_key_from_string`
///   feeds `K`'s decoder the numeric reading first and the text reading as a fallback.
///
/// The nominal route names both traits FULLY QUALIFIED rather than calling a method: the flatten fns
/// are emitted into the generated `mod.rs`, which reaches the traits through `use serialization::*;`
/// — and under `--common-import-override` / an extern dep that glob brings in nothing (the consumer's
/// `serialization.rs` only privately globs the dependency's). A `k.to_cbor_bytes()` spelling would
/// compile in single-crate fixtures and fail in split-crate consumers. The write trait forks with the
/// flags exactly as the wasm `to_cbor_bytes` emission does; `Deserialize` has one shape in every
/// flavor.
///
/// An encoding operation on the domain (`#6.24(uint)`, `bytes .cbor uint`) is NOT part of the image:
/// the image is of `K`'s rust member, and the read side rebuilds that same member, so the row's tag /
/// `.cbor` wrapper is re-applied by the CBOR serializer and the JSON face stays symmetric.
fn typed_rest_key_json_arms(
    rest: &RestRow,
    key_ty: &str,
    flatten: &str,
    cli: &Cli,
) -> (String, String) {
    // A key shape with no member-name image at all (bytes, bool/null/simple): both directions are a
    // pure error surface, worded like the head reader's so the two routes report the same thing.
    let no_image = |cbor_kind: &str| {
        (
            format!(
                "|_k: &{key_ty}| Err::<String, {flatten}::RestKeyImageError>(\
                 {flatten}::RestKeyImageError(String::from(\
                 \"map key of CBOR kind {cbor_kind} is not text/uint/nint\")))"
            ),
            format!(
                "let k = Err::<{key_ty}, D::Error>(serde::de::Error::custom(format!(\
                 \"open struct-map rest key {{ks:?}} is not a valid key: a {cbor_kind} key has no \
                 JSON member-name image\")))?;"
            ),
        )
    };
    match rest.domain().conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Primitive(Primitive::N64) => (
            // `nint`'s rust member holds the ENCODED ARGUMENT (`value = -1 - arg`), so neither
            // direction is the argument's own decimal.
            format!(
                "|k: &u64| Ok::<String, core::convert::Infallible>({flatten}::nint_arg_key_string(*k))"
            ),
            format!(
                "let k = {flatten}::nint_arg_key_from_string(&ks).ok_or_else(|| \
                 serde::de::Error::custom(format!(\
                 \"open struct-map rest key {{ks:?}} is not a valid nint\")))?;"
            ),
        ),
        ConceptualRustType::Primitive(
            Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64,
        ) => (
            format!("|k: &{key_ty}| Ok::<String, core::convert::Infallible>(k.to_string())"),
            format!(
                "let k = ks.parse::<{key_ty}>().map_err(|_| serde::de::Error::custom(format!(\
                 \"open struct-map rest key {{ks:?}} is not a valid {key_ty}\")))?;"
            ),
        ),
        ConceptualRustType::Primitive(Primitive::Str) => (
            format!("|k: &{key_ty}| Ok::<String, core::convert::Infallible>(k.clone())"),
            "let k = ks;".to_owned(),
        ),
        ConceptualRustType::Primitive(Primitive::Bytes) => no_image("Bytes"),
        ConceptualRustType::Primitive(Primitive::Bool) => no_image("Special"),
        // Floats are rejected as a rest-row key domain long before here (no total order).
        ConceptualRustType::Primitive(p) if p.is_float() => unreachable!(
            "a float-containing rest-row key domain is rejected by the key-demand float instrument"
        ),
        _ => {
            let common = cli.common_import_rust();
            let ser_trait = if cli.preserve_encodings && cli.canonical_form {
                "Serialize"
            } else {
                "ToCBORBytes"
            };
            (
                format!(
                    "|k: &{key_ty}| {flatten}::typed_rest_key_string(\
                     &<{key_ty} as {common}::serialization::{ser_trait}>::to_cbor_bytes(k))"
                ),
                format!(
                    "let k = {flatten}::rest_key_from_string(\
                     &ks, <{key_ty} as {common}::serialization::Deserialize>::from_cbor_bytes)\
                     .map_err(|e| serde::de::Error::custom(format!(\
                     \"open struct-map rest key {{ks:?}} is not a valid key: {{e}}\")))?;"
                ),
            )
        }
    }
}

/// An OPEN TABLE's CATCH-ALL region: the write-side key image and the read-side key reading, as an
/// `(image closure, read EXPRESSION)` pair over a member name bound to `ks`.
///
/// The image half is the delivered rest-row convention verbatim (shared with
/// [`emit_rest_flatten_json`] / [`typed_rest_key_json_arms`] — same domains, same helpers, so a row
/// images identically whether it sits under an open struct-map or under an open table). Only the
/// READ half is restated here, and only in shape: the delivered one is a STATEMENT that raises its
/// own final error, while an open table must fold the reading's failure into a message that also
/// names the TYPED attempt (the third clause). So this returns a `Result<K, String>` expression the
/// caller composes, and the delivered statements stay byte-identical for every existing fixture.
fn open_table_captured_key_arms(
    types: &IntermediateTypes,
    rest: &RestRow,
    key_ty: &str,
    base: &str,
    flatten: &str,
    cli: &Cli,
) -> (String, String) {
    let domain = RestKeyDomain::of(types, rest);
    match domain {
        RestKeyDomain::Any => (
            format!("{base}::any_cbor_natural_key_string"),
            format!("Ok::<_, String>({base}::any_cbor_natural_key_from_string(&ks))"),
        ),
        RestKeyDomain::Uint => (
            "|k: &u64| Ok::<String, core::convert::Infallible>(k.to_string())".to_owned(),
            "ks.parse::<u64>().map_err(|_| String::from(\"the member name is not a valid uint\"))"
                .to_owned(),
        ),
        RestKeyDomain::Text => (
            "|k: &String| Ok::<String, core::convert::Infallible>(k.clone())".to_owned(),
            format!("Ok::<{key_ty}, String>(ks.clone())"),
        ),
        RestKeyDomain::Typed => {
            let (image, _) = typed_rest_key_json_arms(rest, key_ty, flatten, cli);
            let read = match rest.domain().conceptual_type.resolve_alias_shallow() {
                ConceptualRustType::Primitive(Primitive::N64) => format!(
                    "{flatten}::nint_arg_key_from_string(&ks).ok_or_else(|| \
                     String::from(\"the member name is not a valid nint\"))"
                ),
                ConceptualRustType::Primitive(
                    Primitive::U8
                    | Primitive::U16
                    | Primitive::U32
                    | Primitive::U64
                    | Primitive::I8
                    | Primitive::I16
                    | Primitive::I32
                    | Primitive::I64,
                ) => format!(
                    "ks.parse::<{key_ty}>().map_err(|_| \
                     String::from(\"the member name is not a valid {key_ty}\"))"
                ),
                ConceptualRustType::Primitive(Primitive::Str) => {
                    format!("Ok::<{key_ty}, String>(ks.clone())")
                }
                ConceptualRustType::Primitive(Primitive::Bytes) => format!(
                    "Err::<{key_ty}, String>(String::from(\
                     \"a Bytes key has no JSON member-name image\"))"
                ),
                ConceptualRustType::Primitive(Primitive::Bool) => format!(
                    "Err::<{key_ty}, String>(String::from(\
                     \"a Special key has no JSON member-name image\"))"
                ),
                ConceptualRustType::Primitive(p) if p.is_float() => unreachable!(
                    "a float-containing rest-row key domain is rejected by the key-demand float instrument"
                ),
                _ => {
                    let common = cli.common_import_rust();
                    format!(
                        "{flatten}::rest_key_from_string(&ks, \
                         <{key_ty} as {common}::serialization::Deserialize>::from_cbor_bytes)\
                         .map_err(|e| format!(\"{{e}}\"))"
                    )
                }
            };
            (image, read)
        }
    }
}

/// The JSON face of an OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`): ONE hand-written
/// `Serialize`/`Deserialize` pair plus a hand-written `JsonSchema`, emitted into the struct's module
/// in place of the derives (which `create_base_rust_struct` is told to skip).
///
/// Hand-written rather than a derive with two `#[serde(flatten)]` members, because serde's flatten
/// machinery cannot express this shape in either direction (verified against serde 1.0.229): on READ
/// every unmatched member is handed to BOTH flattened fields (`FlatMapAccess::next_key_seed` borrows
/// rather than takes), so nothing partitions them; on WRITE both fields forward into one parent map
/// with no dedup, so two regions imaging one member name emit it twice. The partition and the
/// cross-region collision check are exactly what an open table needs to be correct, so they live in
/// ONE emitted impl that owns both regions rather than emerging across two independent fns.
///
/// The two regions read in TYPED-FIRST order and image through two different conventions — see
/// `static/open_table_json.rs` for why that is forced rather than an inconsistency.
///
/// The schema is one open object over BOTH ranges (`additionalProperties: anyOf[V_t, V_r]`), named
/// by a hand impl for the same reason the serde pair is hand-written: schemars merges two flattened
/// object schemas by keeping the FIRST `additionalProperties` and silently dropping the second
/// (`schemars::_private::flatten`'s no-op arm), so a derive would publish one region and omit the
/// other. Neither KEY type is named — the exemption an open struct-map rest row's key domain already
/// enjoys, extended to both of an open table's rows.
fn emit_open_table_json(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    record: &RustRecord,
    cli: &Cli,
) {
    let typed = record.typed_row().expect("open table has a typed row");
    let captured = record
        .rest
        .as_deref()
        .expect("open table has a catch-all row");
    let base = format!("{}::any_cbor", cli.common_import_rust());
    let flatten = format!("{}::open_struct_rest_json", cli.common_import_rust());
    let key_t = typed.domain().for_rust_member(types, false, cli);
    let key_r = captured.domain().for_rust_member(types, false, cli);
    let typed_field = &typed.field_name;
    let captured_field = &captured.field_name;
    let range_is_any = |row: &RestRow| {
        matches!(
            row.range().conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Any
        )
    };
    // Per-region value view, exactly the rest row's rule: an `any` range renders NATURALLY (through
    // the natural adapters), a typed range through its own serde.
    let value_view = |row: &RestRow| -> (String, String, String) {
        if range_is_any(row) {
            (
                format!(".map(|(k, v)| (k, {base}::NaturalAnyCborSer(v)))"),
                format!("{base}::NaturalAnyCborDe"),
                "v.0".to_owned(),
            )
        } else {
            (
                String::new(),
                row.range().for_rust_member(types, false, cli),
                "v".to_owned(),
            )
        }
    };
    let (typed_map, typed_value_de, typed_value_unwrap) = value_view(typed);
    let (captured_map, captured_value_de, captured_value_unwrap) = value_view(captured);
    let (captured_image, captured_read) =
        open_table_captured_key_arms(types, captured, &key_r, &base, &flatten, cli);
    // The NonEmpty flavor cannot open the visitor with `{name}::new()` — its door takes the first
    // typed entry — so the visitor STAGES both regions in read order and assembles at the end,
    // refusing there if the typed region stayed empty. That refusal is the JSON twin of the CBOR
    // face's post-loop `is_empty()` check, and it counts TYPED entries for the same reason. Staging
    // preserves each region's own read order exactly as the direct inserts did (the two regions were
    // never interleaved WITHIN a container), so nothing but the empty case changes.
    let (visitor_open, typed_insert, captured_insert, visitor_close) = if record
        .is_non_empty_open_table()
    {
        (
            "let mut typed_staged = alloc::vec::Vec::new();\n\
                 \x20              let mut captured_staged = alloc::vec::Vec::new();"
                .to_owned(),
            format!("typed_staged.push((k, {typed_value_unwrap}));"),
            format!("captured_staged.push((k, {captured_value_unwrap}));"),
            format!(
                "let mut typed_staged = typed_staged.into_iter();\n\
                     \x20              let (first_key, first_value) = match typed_staged.next() {{\n\
                     \x20                  Some(entry) => entry,\n\
                     \x20                  None => return Err(serde::de::Error::custom({flatten}::open_table_min_one_typed())),\n\
                     \x20              }};\n\
                     \x20              let mut out = {name}::new(first_key, first_value);\n\
                     \x20              for (k, v) in typed_staged {{\n\
                     \x20                  out.{typed_field}.insert(k, v);\n\
                     \x20              }}\n\
                     \x20              for (k, v) in captured_staged {{\n\
                     \x20                  out.{captured_field}.insert(k, v);\n\
                     \x20              }}\n\
                     \x20              Ok(out)"
            ),
        )
    } else {
        (
            format!("let mut out = {name}::new();"),
            format!("out.{typed_field}.insert(k, {typed_value_unwrap});"),
            format!("out.{captured_field}.insert(k, {captured_value_unwrap});"),
            "Ok(out)".to_owned(),
        )
    };

    let mut out = String::new();
    if cli.json_serde_derives {
        out.push_str(&format!(
            "impl serde::Serialize for {name} {{\n\
             \x20   fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {{\n\
             \x20       {flatten}::serialize_open_table(\n\
             \x20           {flatten}::open_table_typed_key_string::<{key_t}>,\n\
             \x20           self.{typed_field}.iter(){typed_map},\n\
             \x20           {captured_image},\n\
             \x20           self.{captured_field}.iter(){captured_map},\n\
             \x20           serializer,\n\
             \x20       )\n\
             \x20   }}\n\
             }}\n\
             \n\
             impl<'de> serde::Deserialize<'de> for {name} {{\n\
             \x20   fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {{\n\
             \x20       struct OpenTableVisitor;\n\
             \x20       impl<'de> serde::de::Visitor<'de> for OpenTableVisitor {{\n\
             \x20           type Value = {name};\n\
             \x20\n\
             \x20           fn expecting(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {{\n\
             \x20               f.write_str(\"an open table JSON object ({name})\")\n\
             \x20           }}\n\
             \x20\n\
             \x20           fn visit_map<A: serde::de::MapAccess<'de>>(self, mut access: A) -> Result<{name}, A::Error> {{\n\
             \x20               {visitor_open}\n\
             \x20               // ONE set over BOTH regions: the partition is a function of the member\n\
             \x20               // NAME, so a repeated name is a repeated member whichever row claims it —\n\
             \x20               // and serde_json's object parser is last-wins, which would silently drop an\n\
             \x20               // entry the CBOR face rejects as a duplicate key.\n\
             \x20               let mut seen = alloc::collections::BTreeSet::new();\n\
             \x20               while let Some(ks) = access.next_key::<String>()? {{\n\
             \x20                   if !seen.insert(ks.clone()) {{\n\
             \x20                       return Err(serde::de::Error::custom({flatten}::open_table_duplicate_member(&ks)));\n\
             \x20                   }}\n\
             \x20                   // TYPED-FIRST: a member name binds the typed row iff `K_t`'s own reading\n\
             \x20                   // admits it. Once it binds, the value is read as `V_t` and a refusing\n\
             \x20                   // value is a HARD error — it does not fall through to the catch-all,\n\
             \x20                   // matching the CBOR face's refinement-not-tolerance posture.\n\
             \x20                   match {flatten}::open_table_typed_key_read::<{key_t}>(&ks) {{\n\
             \x20                       Ok(k) => {{\n\
             \x20                           let v = access.next_value::<{typed_value_de}>()?;\n\
             \x20                           {typed_insert}\n\
             \x20                       }}\n\
             \x20                       Err(typed_err) => match {captured_read} {{\n\
             \x20                           Ok(k) => {{\n\
             \x20                               let v = access.next_value::<{captured_value_de}>()?;\n\
             \x20                               {captured_insert}\n\
             \x20                           }}\n\
             \x20                           Err(captured_err) => {{\n\
             \x20                               let e = {flatten}::OpenTableKeyReadError {{\n\
             \x20                                   typed: typed_err,\n\
             \x20                                   captured: captured_err,\n\
             \x20                               }};\n\
             \x20                               return Err(serde::de::Error::custom(format!(\n\
             \x20                                   \"open table key {{ks:?}} is not a valid key: {{e}}\"\n\
             \x20                               )));\n\
             \x20                           }}\n\
             \x20                       }},\n\
             \x20                   }}\n\
             \x20               }}\n\
             \x20               {visitor_close}\n\
             \x20           }}\n\
             \x20       }}\n\
             \x20       deserializer.deserialize_map(OpenTableVisitor)\n\
             \x20   }}\n\
             }}\n"
        ));
    }
    if cli.json_schema_export {
        let range_schema = |row: &RestRow| {
            if range_is_any(row) {
                format!("{base}::natural_any_cbor_schema(generator)")
            } else {
                format!(
                    "generator.subschema_for::<{}>()",
                    row.range().for_rust_member(types, false, cli)
                )
            }
        };
        let typed_schema = range_schema(typed);
        let captured_schema = range_schema(captured);
        out.push_str(&format!(
            "\nimpl schemars::JsonSchema for {name} {{\n\
             \x20   fn schema_name() -> alloc::borrow::Cow<'static, str> {{\n\
             \x20       alloc::borrow::Cow::Borrowed(\"{name}\")\n\
             \x20   }}\n\
             \x20\n\
             \x20   fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {{\n\
             \x20       let typed_range = {typed_schema};\n\
             \x20       let captured_range = {captured_schema};\n\
             \x20       {flatten}::open_table_schema(typed_range, captured_range)\n\
             \x20   }}\n\
             \x20\n\
             \x20   fn inline_schema() -> bool {{\n\
             \x20       false\n\
             \x20   }}\n\
             }}\n"
        ));
    }
    gen_scope.rust(types, name).raw(&out);
}

/// Emit a rest capture into `block`: account the entry in `read_len`, bind the key (`rest_key`) —
/// either from `key_val_expr` (already read by the record loop's uint/text peek — for an `any`
/// domain this is the reconstructed `AnyCbor`, carrying the peeked wire width under preserve) or by
/// deserializing the domain from `raw` (for `any`-domain other-type / special keys) — bind the
/// value (`rest_value`), populate the per-entry encoding sidecars for concrete domains under
/// preserve, push the wire-position index (`rest_index_base + <container>.len()`) onto
/// `orig_deser_order`, then insert with the default (reject) duplicate check. Duplicate detection
/// is keyed on CBOR VALUE equality (not the domain's spelling): for a concrete
/// key the container `Eq` IS value equality (`insert().is_some()`); for an `any`-domain key under
/// preserve the container `Eq` is REPRESENTATIONAL, so the dup check is a value-normalized
/// `value_eq` side scan (confined to any-domain containers). `key_enc_expr` is the raw peeked-key
/// encoding var (a `Sz`/`StringLenSz`) for a concrete uint/text key under preserve — stored in the
/// key sidecar; `None` for self-carried `any` keys and non-preserve.
#[allow(clippy::too_many_arguments)]
/// How ONE dynamic row's entries index into the `orig_deser_order` wire-position vector.
///
/// The vector stays `Vec<usize>` in every shape — it is a `pub` field of the generated encoding
/// struct, so its type is generated-crate API and changing it for one shape would change it for all.
#[derive(Clone, Copy, PartialEq, Eq)]
enum OrderSlots {
    /// Open struct-map / open array: the declared fields occupy `0..N` (`N` a codegen-time constant)
    /// and the i-th rest entry occupies `N + i`. One dynamic sequence, so one static base suffices.
    AfterDeclared(usize),
    /// Open table: TWO dynamic sequences share one order vector. The second sequence's base would be
    /// `self.<typed>.len()` — a RUNTIME value that is not yet final while the deserialize loop is
    /// still pushing — so `base + i` is not expressible at all. Each slot therefore carries its
    /// SOURCE SEQUENCE as a tag: the typed row's i-th entry is `2*i`, the catch-all's j-th is
    /// `2*j + 1`. Both bases (0, 1) and the stride (2) are codegen-time constants, the tag is the low
    /// bit, and the decoded index is the quotient — so serialize's replay recovers (sequence, index)
    /// from a bare `usize` without consulting either container's length.
    Tagged { odd: bool },
}

impl OrderSlots {
    /// The push expression for the NEXT entry of this row: `<container>.len()` before the insert IS
    /// that entry's positional index within its own sequence.
    fn push_expr(self, field_name: &str) -> String {
        match self {
            OrderSlots::AfterDeclared(base) => format!("{base} + {field_name}.len()"),
            OrderSlots::Tagged { odd: false } => format!("2 * {field_name}.len()"),
            OrderSlots::Tagged { odd: true } => format!("2 * {field_name}.len() + 1"),
        }
    }

    /// The CANONICAL-merge push index for the i-th present entry of this row, given a loop binding
    /// named `i`. Mirrors `push_expr` exactly — the merge and the deserialize loop must agree on the
    /// slot encoding or the replay reads the wrong sequence.
    fn merge_index_expr(self, i: &str) -> String {
        match self {
            OrderSlots::AfterDeclared(base) => format!("{base} + {i}"),
            OrderSlots::Tagged { odd: false } => format!("2 * {i}"),
            OrderSlots::Tagged { odd: true } => format!("2 * {i} + 1"),
        }
    }
}

/// The `cbor_event::Type::…` path for a major, as the emitted match arm spells it.
fn cbor_type_arm_path(ty: CBORType) -> &'static str {
    match ty {
        CBORType::UnsignedInteger => "cbor_event::Type::UnsignedInteger",
        CBORType::NegativeInteger => "cbor_event::Type::NegativeInteger",
        CBORType::Bytes => "cbor_event::Type::Bytes",
        CBORType::Text => "cbor_event::Type::Text",
        CBORType::Array => "cbor_event::Type::Array",
        CBORType::Map => "cbor_event::Type::Map",
        CBORType::Tag => "cbor_event::Type::Tag",
        CBORType::Special => "cbor_event::Type::Special",
    }
}

/// Fill an OPEN TABLE's deserialize dispatch: `match raw.cbor_type()?` with the typed row claiming
/// exactly its declared major and the catch-all taking everything else.
///
/// The peek IS the loop's existing dispatch — no separate lookahead is needed, and nothing is
/// consumed before a row's `K::deserialize` runs, so neither row ever rewinds. Both rows therefore
/// take the TYPED (seek) capture path unconditionally (`key_val_expr: None`), even for a bare
/// `uint`/`text` key: the peeked fast path exists to reuse a key a DECLARED-key arm already read,
/// and an open table has no declared keys.
///
/// Arm layout, in emission order (`Block::push_block` order is emission order, and `_` must be last):
///
/// * the typed row's arm, unless its major is `Special` (which shares major 7 with the
///   indefinite-map BREAK and so must sit after the break check, inside the Special arm);
/// * the `Special` arm: break check first, then whichever row claims major 7;
/// * `_`: the catch-all.
///
/// Refinement failures are HARD parse errors by construction: once the major routes an entry to a
/// row, that row's `K::deserialize`/`V::deserialize` decides, and its `Err` propagates out of the
/// whole open table. That is what makes an open table compose with a backtracking type choice — the
/// enclosing arm fails and the reader rewinds to the choice's anchor.
/// The PRESERVE-path `deser_order` computation for an OPEN TABLE: the two dynamic rows' entries
/// interleaved into one order vector, tagged by source sequence (see [`OrderSlots::Tagged`]).
///
/// Under `--canonical-form` the order depends on RUNTIME keys from BOTH rows, so it cannot be a
/// baked `vec![..]`: two runtime loops serialize every present key canonically into one `key_order`
/// vec, which the shared length-first comparator then sorts — the typed and captured regions merge
/// into ONE sort by encoded bytes, exactly as a table's would. For a custom-codec key the sort key is
/// the bytes the codec WRITES, because the merge calls `generate_serialize` on the domain, which
/// honors the alias's `@custom_serialize`.
///
/// No sidecar lookup here, and every encoding var is bound to its default: under `force_canonical`
/// the write is minimal regardless, so the merge's sort key matches the bytes the replay arm writes.
fn emit_open_table_preserve_order(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    ser_func: &mut codegen::Function,
    cli: &Cli,
) {
    let typed = record.typed_row().expect("open table has a typed row");
    let catch_all = record
        .rest
        .as_deref()
        .expect("open table has a catch-all row");
    let rows = [
        (typed, OrderSlots::Tagged { odd: false }),
        (catch_all, OrderSlots::Tagged { odd: true }),
    ];
    // Self-heal fallback: the recorded order is used only when its length matches the present-entry
    // count (`definite_info`, which folds BOTH rows' lengths); otherwise rebuild it as typed entries
    // first, then captured — each in its own tagged slot space.
    let fallback = format!(
        "(0..self.{}.len()).map(|i| 2 * i).chain((0..self.{}.len()).map(|i| 2 * i + 1)).collect::<Vec<usize>>()",
        typed.field_name, catch_all.field_name
    );
    let orig_or_fallback = format!(
        "self.encodings.as_ref().filter(|encs| encs.orig_deser_order.len() as u64 == {}).map(|encs| encs.orig_deser_order.clone()).unwrap_or_else(|| {})",
        record.definite_info("self", false, types, cli),
        fallback
    );
    if cli.canonical_form {
        let mut merge = Block::new("let deser_order = if force_canonical");
        merge.line("let mut key_order: Vec<(Vec<u8>, usize)> = Vec::new();");
        for (row, slots) in rows {
            // Each loop body is its own scope, so both rows bind `rest_key` without shadowing.
            let mut key_loop = Block::new(format!(
                "for (i, (rest_key, _)) in self.{}.iter().enumerate()",
                row.field_name
            ));
            key_loop.line("let mut buf = cbor_event::se::Serializer::new_vec();");
            let (merge_key_encs, _) = rest_encoding_fields(types, row, cli);
            for enc in &merge_key_encs {
                key_loop.line(format!("let {} = {};", enc.field_name, enc.default_expr));
            }
            let merge_key_config =
                SerializeConfig::new("rest_key", format!("{}_key", row.field_name))
                    .expr_is_ref(true)
                    .is_end(false)
                    .serializer_name_overload(("buf", true))
                    .encoding_var_is_ref(false);
            gen_scope.generate_serialize(
                types,
                (row.domain()).into(),
                &mut key_loop,
                merge_key_config,
                cli,
            );
            key_loop.line(format!(
                "key_order.push((buf.finalize(), {}));",
                slots.merge_index_expr("i")
            ));
            merge.push_block(key_loop);
        }
        merge.line("key_order.sort_by(|(lhs, _), (rhs, _)| cbor_canonical_key_cmp(lhs, rhs));");
        merge.line("key_order.into_iter().map(|(_, idx)| idx).collect::<Vec<usize>>()");
        merge.after(format!(" else {{ {orig_or_fallback} }};"));
        ser_func.push_block(merge);
    } else {
        ser_func.line(format!("let deser_order = {orig_or_fallback};"));
    }
    // `OrderedHashMap`/`PairMap` deref to backing types with no positional `get`, so materialize each
    // row's entries once for the tagged index lookup in the replay.
    for (row, _) in rows {
        ser_func.line(format!(
            "let {}_entries: Vec<_> = self.{}.iter().collect();",
            row.field_name, row.field_name
        ));
    }
}

/// The PRESERVE-path replay arms for an OPEN TABLE: one arm per source sequence, selected by the
/// slot tag (`field_index % 2`), with the entry's own index recovered as the quotient.
///
/// `.get()` returns `Option`, so a stale `orig_deser_order` (a user mutated a container after
/// deserialize, shifting the count) SKIPS rather than panics — serialize's never-panic philosophy.
fn emit_open_table_replay(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    ser_loop_match: &mut Block,
    cli: &Cli,
) {
    let typed = record.typed_row().expect("open table has a typed row");
    let catch_all = record
        .rest
        .as_deref()
        .expect("open table has a catch-all row");
    for (row, arm_head) in [(typed, "0 =>"), (catch_all, "_ =>")] {
        let mut arm = Block::new(arm_head);
        arm.line("let rest_i = field_index / 2;");
        let mut got = Block::new(format!(
            "if let Some(&(key, value)) = {}_entries.get(rest_i)",
            row.field_name
        ));
        // The `Vec` sidecar of a `@duplicates preserve` row is POSITIONAL (its keys repeat); the loose
        // container keys its sidecar by the key VALUE.
        let enc_lookup_var = if rest_is_pair_map(row) {
            "rest_i"
        } else {
            "key"
        };
        emit_rest_entry_serialize(gen_scope, types, row, enc_lookup_var, &mut got, cli);
        arm.push_block(got);
        ser_loop_match.push_block(arm);
    }
    ser_loop_match.after(";");
}

fn append_open_table_dispatch(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    type_match: &mut Block,
    cli: &Cli,
) {
    let typed = record.typed_row().expect("open table has a typed row");
    let catch_all = record
        .rest
        .as_deref()
        .expect("open table has a catch-all row");
    let typed_slots = OrderSlots::Tagged { odd: false };
    let catch_all_slots = OrderSlots::Tagged { odd: true };
    // `finalize` derives this and rejects every shape where it is not statically knowable, so an
    // open table that reaches generation always has one.
    let major = typed
        .dispatch_major
        .expect("open table typed row has a derived dispatch major");
    if major != CBORType::Special {
        let mut typed_arm = Block::new(format!("{} =>", cbor_type_arm_path(major)));
        append_rest_capture(
            gen_scope,
            types,
            typed,
            typed_slots,
            None,
            None,
            &mut typed_arm,
            cli,
        );
        typed_arm.after(",");
        type_match.push_block(typed_arm);
    }
    // Major 7 carries both the indefinite-map BREAK and every special-typed key, so the break check
    // runs first and diverges (return/break); control reaches the capture only when it was NOT a
    // break. `special_break()` advances ONLY on a true break, so a non-break special is left intact
    // for the key deserialize. (A null-admitting key — the one shape where the two readings genuinely
    // collide — is rejected at recognition, on both rows.)
    let mut special_arm = Block::new("cbor_event::Type::Special =>");
    let mut is_break = Block::new("if raw.special_break()?");
    let mut break_len = Block::new("match len");
    break_len.line(format!(
        "{} => return Err(DeserializeFailure::BreakInDefiniteLen.into()),",
        cbor_event_len_n("_", cli)
    ));
    break_len.line(format!("{} => break,", cbor_event_len_indef(cli)));
    is_break.push_block(break_len);
    special_arm.push_block(is_break);
    let (special_row, special_slots) = if major == CBORType::Special {
        (typed, typed_slots)
    } else {
        (catch_all, catch_all_slots)
    };
    append_rest_capture(
        gen_scope,
        types,
        special_row,
        special_slots,
        None,
        None,
        &mut special_arm,
        cli,
    );
    special_arm.after(",");
    type_match.push_block(special_arm);
    let mut rest_arm = Block::new("_ =>");
    append_rest_capture(
        gen_scope,
        types,
        catch_all,
        catch_all_slots,
        None,
        None,
        &mut rest_arm,
        cli,
    );
    rest_arm.after(",");
    type_match.push_block(rest_arm);
}

#[allow(clippy::too_many_arguments)]
fn append_rest_capture(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    rest: &RestRow,
    slots: OrderSlots,
    key_val_expr: Option<String>,
    key_enc_expr: Option<String>,
    block: &mut Block,
    cli: &Cli,
) {
    block.line("read_len.read_elems(1)?;");
    // IGNORE flavor (tolerate-and-drop): consume the entry's key and value to advance the stream, then
    // store NOTHING (no container, no sidecar, no wire-position — `@ignore` is rejected under
    // preserve, so none of that machinery is reachable here). `read_len.read_elems(1)?` above still
    // accounts the entry (the deser loop stays dynamic-length so a definite-length map with extra
    // entries passes — the same length machinery capture uses). When `key_val_expr` is `Some`, the
    // arm already read the key bytes (this expr merely RECONSTRUCTS the value for capture), so binding
    // it to `_` consumes that read; otherwise the key still sits on the wire and must be deserialized.
    if rest.semantics == RestSemantics::Ignore {
        match key_val_expr {
            Some(expr) => {
                block.line(format!("let _ = {expr};"));
            }
            None => {
                gen_scope
                    .generate_deserialize(
                        types,
                        (rest.domain()).into(),
                        DeserializeBeforeAfter::new("let _rest_key = ", ";", false),
                        DeserializeConfig::new("_rest_key"),
                        cli,
                    )
                    .add_to(block);
            }
        }
        gen_scope
            .generate_deserialize(
                types,
                (rest.range()).into(),
                DeserializeBeforeAfter::new("let _rest_value = ", ";", false),
                DeserializeConfig::new("_rest_value"),
                cli,
            )
            .add_to(block);
        return;
    }
    let domain = RestKeyDomain::of(types, rest);
    let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
    // --- key ---
    match key_val_expr {
        Some(expr) => {
            block.line(format!("let rest_key = {expr};"));
        }
        None if cli.preserve_encodings && !key_encs.is_empty() => {
            // A TYPED key that carries its own encoding sidecar (bare `bytes`, a sized int, a c-style
            // enum, a named collection, a tagged/`.cbor` domain, …): bind the key AND its encoding
            // vars from the deserialize itself, exactly as the concrete-VALUE path four blocks below
            // does. The fast peeked-key path can't reach here — it always passes `key_val_expr` —
            // so this is the one shape that used to hit `key_enc_expr.expect(..)`.
            let var_names_str = encoding_var_names_str(types, "rest_key", rest.domain(), cli);
            gen_scope
                .generate_deserialize(
                    types,
                    (rest.domain()).into(),
                    DeserializeBeforeAfter::new(&format!("let {var_names_str} = "), ";", false),
                    DeserializeConfig::new("rest_key"),
                    cli,
                )
                .add_to(block);
        }
        None => {
            gen_scope
                .generate_deserialize(
                    types,
                    (rest.domain()).into(),
                    DeserializeBeforeAfter::new("let rest_key = ", ";", false),
                    DeserializeConfig::new("rest_key"),
                    cli,
                )
                .add_to(block);
        }
    }
    // --- value (with encoding capture under preserve for a concrete range) ---
    if cli.preserve_encodings && !value_encs.is_empty() {
        let var_names_str = encoding_var_names_str(types, "rest_value", rest.range(), cli);
        gen_scope
            .generate_deserialize(
                types,
                (rest.range()).into(),
                DeserializeBeforeAfter::new(&format!("let {var_names_str} = "), ";", false),
                DeserializeConfig::new("rest_value"),
                cli,
            )
            .add_to(block);
    } else {
        gen_scope
            .generate_deserialize(
                types,
                (rest.range()).into(),
                DeserializeBeforeAfter::new("let rest_value = ", ";", false),
                DeserializeConfig::new("rest_value"),
                cli,
            )
            .add_to(block);
    }
    let key_is_copy = rest.domain().is_copy(types);
    let key_for_sidecar = if key_is_copy {
        "rest_key".to_owned()
    } else {
        "rest_key.clone()".to_owned()
    };
    let is_pair_map = rest_is_pair_map(rest);
    // --- per-entry encoding sidecars (concrete domains) ---
    // Loose container: keyed by the key VALUE (`insert`). Pair-map twin: POSITIONAL (`push`, parallel
    // to the pair-list) — its keys repeat, so a keyed sidecar would collide.
    if cli.preserve_encodings {
        if !value_encs.is_empty() {
            // The LOCAL vars the value's deserialize just bound, which are named off the fixed
            // binding `rest_value` — NOT `value_encs`, whose names are the SIDECAR DECLARATION's
            // (`{field}_value_encoding`, named off the row's field name). The two coincide only for
            // the default field name `rest`; with `@name` on the row (or on an open table's typed
            // row, whose default is `entries`) the declaration names are wrong here and the
            // generated crate fails to compile (E0425). The KEY path below already reads the local
            // vars this way — this is the same read.
            let val_tuple = tuple_str(
                encoding_fields(types, "rest_value", rest.range(), false, cli)
                    .into_iter()
                    .map(|e| e.field_name)
                    .collect(),
            );
            block.line(if is_pair_map {
                format!("{}_value_encodings.push({val_tuple});", rest.field_name)
            } else {
                format!(
                    "{}_value_encodings.insert({key_for_sidecar}, {val_tuple});",
                    rest.field_name
                )
            });
        }
        if !key_encs.is_empty() {
            // Two producers, one sidecar. FAST path (bare uint/text): the peeked raw encoding
            // (`key_enc_expr`) converted via the key type's single encoding field — a peeked domain is
            // a leaf, so one field is the only shape there. TYPED path: the encoding vars the key's
            // own deserialize bound above, joined as a tuple exactly like the VALUE path does — which
            // is what lets a composite/sidecar-bearing `K` (bytes, sized ints, a named collection)
            // contribute however many fields it has.
            let key_enc_val = match key_enc_expr.clone() {
                Some(raw) => key_encs[0].enc_conversion(&raw),
                None => tuple_str(
                    encoding_fields(types, "rest_key", rest.domain(), false, cli)
                        .into_iter()
                        .map(|e| e.field_name)
                        .collect(),
                ),
            };
            block.line(if is_pair_map {
                format!("{}_key_encodings.push({key_enc_val});", rest.field_name)
            } else {
                format!(
                    "{}_key_encodings.insert({key_for_sidecar}, {key_enc_val});",
                    rest.field_name
                )
            });
        }
        // wire-position index for this entry: declared fields occupy 0..N, the
        // i-th rest entry occupies N+i. `<container>.len()` before the insert IS i (and the sidecar
        // `Vec`s, pushed just above, are already aligned to it).
        block.line(format!(
            "orig_deser_order.push({});",
            slots.push_expr(&rest.field_name)
        ));
    }
    // --- duplicate handling + insert ---
    if is_pair_map {
        // `@duplicates preserve`: duplicates are the POINT — append every entry (PairMap::insert
        // pushes, never displaces), no dup check, in wire order (matching @duplicates preserve
        // tables). Value-duplicate rejection is the DEFAULT (reject) container's job.
        let insert_key = if key_is_copy {
            "rest_key".to_owned()
        } else {
            "rest_key.clone()".to_owned()
        };
        block.line(format!(
            "{}.insert({insert_key}, rest_value);",
            rest.field_name
        ));
        return;
    }
    // An open table's TYPED row names ITS position: a duplicate there is a duplicate of a key the
    // rule declares a type for, which reads very differently in a caller's error from a duplicate
    // among the captured leftovers. `slots` carries the role (the tagged even sequence IS the typed
    // row — see `OrderSlots::Tagged`).
    let is_open_table_typed_row = slots == OrderSlots::Tagged { odd: false };
    let dup_key_error = match domain {
        _ if is_open_table_typed_row => {
            "Key::Str(String::from(\"<open-table typed key>\"))".to_owned()
        }
        RestKeyDomain::Uint => "Key::Uint(rest_key)".to_owned(),
        RestKeyDomain::Text => "Key::Str(rest_key)".to_owned(),
        // Neither an `AnyCbor` nor a general typed key has a simple `Key` spelling — both name the
        // POSITION instead (the same placeholder, so the two read alike in a caller's error).
        RestKeyDomain::Any | RestKeyDomain::Typed => {
            "Key::Str(String::from(\"<open-map rest key>\"))".to_owned()
        }
    };
    if cli.preserve_encodings && domain == RestKeyDomain::Any {
        // Representational `Eq` here (encoding widths participate), so `insert` would silently accept
        // `0x01` and `0x1801` as two entries. Rejecting a duplicate must key on CBOR VALUE equality: scan
        // for a value-equal existing key first (confined to the any-domain container).
        let mut dup_check = Block::new(format!(
            "if {}.iter().any(|(k, _)| k.value_eq(&rest_key))",
            rest.field_name
        ));
        dup_check.line(format!(
            "return Err(DeserializeFailure::DuplicateKey({dup_key_error}).into());"
        ));
        block.push_block(dup_check);
        block.line(format!("{}.insert(rest_key, rest_value);", rest.field_name));
    } else {
        let insert_key = if key_is_copy {
            "rest_key".to_owned()
        } else {
            "rest_key.clone()".to_owned()
        };
        let mut dup_check = Block::new(format!(
            "if {}.insert({}, rest_value).is_some()",
            rest.field_name, insert_key
        ));
        dup_check.line(format!(
            "return Err(DeserializeFailure::DuplicateKey({dup_key_error}).into());"
        ));
        block.push_block(dup_check);
    }
}

/// A `vec![0x.., ..]` literal of a fixed key's canonical bytes — the codegen-time contribution of a
/// declared field to an open struct's runtime canonical key merge, fed to `cbor_canonical_key_cmp`
/// exactly as `RustRecord::canonical_ordering` sorts the closed-struct baked order (so the two
/// agree; pinned by the `24`-vs-`10` divergence vector).
fn byte_vec_literal(bytes: &[u8]) -> String {
    format!(
        "vec![{}]",
        bytes
            .iter()
            .map(|b| format!("0x{b:02x}"))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// The serialize-time "is this declared field present?" predicate — identical to the replay match
/// arm's guard, so the canonical key merge counts exactly the fields that will be written. `None`
/// for a mandatory field (always present).
fn rest_merge_present_condition(field: &RustField) -> Option<String> {
    if !field.optional {
        None
    } else if field.rust_type.is_fixed_value() {
        Some(format!("self.{}", field.name))
    } else if let Some(default_value) = &field.rust_type.config.default {
        Some(format!(
            "self.{} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false)",
            field.name,
            default_value.to_primitive_str_compare(),
            field.name
        ))
    } else {
        Some(format!("self.{}.is_some()", field.name))
    }
}

/// Emit the key/value serialize of ONE open-struct rest entry into `block`, assuming `key` (`&K`)
/// and `value` (`&V`) refs and (under preserve) `self.encodings` in scope. Reuses the loose-table
/// key/value serialize + `container_encoding_lookup` pattern: concrete key/value pull their wire
/// encoding from the `{restfield}_{key,value}_encodings` sidecar via `enc_lookup_var` — the key
/// VALUE (`"key"`) for the loose `BTreeMap` sidecar, or the positional index (`field_index - N`) for
/// the `@duplicates preserve` `Vec` sidecar (whose keys repeat). Self-carried `any` content emits
/// its own encodings (no sidecar, empty `*_encs`).
fn emit_rest_entry_serialize(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    rest: &RestRow,
    enc_lookup_var: &str,
    block: &mut dyn CodeBlock,
    cli: &Cli,
) {
    let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
    let outer = SerializeConfig::new("key", &rest.field_name)
        .encoding_var_in_option_struct("self.encodings");
    if !key_encs.is_empty() {
        block.line(&outer.container_encoding_lookup("key", &key_encs, enc_lookup_var));
    }
    let key_config = SerializeConfig::new("key", format!("{}_key", rest.field_name))
        .expr_is_ref(true)
        .is_end(false)
        .encoding_var_no_option_struct()
        .encoding_var_is_ref(false)
        .tag_depth(0);
    gen_scope.generate_serialize(types, (rest.domain()).into(), block, key_config, cli);
    if !value_encs.is_empty() {
        block.line(&outer.container_encoding_lookup("value", &value_encs, enc_lookup_var));
    }
    let value_config = SerializeConfig::new("value", format!("{}_value", rest.field_name))
        .expr_is_ref(true)
        .is_end(false)
        .encoding_var_no_option_struct()
        .encoding_var_is_ref(false)
        .tag_depth(0);
    gen_scope.generate_serialize(types, (rest.range()).into(), block, value_config, cli);
}

#[allow(clippy::too_many_arguments)]
fn build_map_field_deser_arm(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    field: &RustField,
    field_index: usize,
    key: &FixedValue,
    in_embedded: bool,
    deser_code: &mut DeserializationCode,
    cli: &Cli,
) -> Block {
    // deserialize key + value
    let mut deser_block = match key {
        FixedValue::Uint(x) => {
            if cli.preserve_encodings {
                Block::new(format!("({x}, key_enc) => "))
            } else {
                Block::new(format!("{x} => "))
            }
        }
        FixedValue::Text(x) => Block::new(format!("\"{}\" => ", escape_rust_str(x))),
        _ => panic!(
            "unsupported map key type for {}.{}: {:?}",
            name, field.name, key
        ),
    };
    deser_block.after(",");
    let mut deser_block_code = DeserializationCode::default();
    let key_in_rust = match key {
        FixedValue::Uint(x) => format!("Key::Uint({x})"),
        FixedValue::Text(x) => {
            format!("Key::Str(\"{}\".into())", escape_rust_str(x))
        }
        _ => unimplemented!(),
    };
    if cli.preserve_encodings {
        let mut dup_check = if field.rust_type.is_fixed_value() {
            Block::new(format!("if {}_present", field.name))
        } else {
            Block::new(format!("if {}.is_some()", field.name))
        };
        dup_check.line(format!(
            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
        ));
        deser_block_code.content.push_block(dup_check);

        let temp_var_prefix = format!("tmp_{}", field.name);
        let var_names_str = encoding_var_names_str_for_field(
            types,
            &temp_var_prefix,
            &field.rust_type,
            Some(&field.rule_metadata),
            cli,
        );
        if cli.annotate_fields {
            let (before, after) = if var_names_str.is_empty() {
                // empty binding == a fixed value with no encoding var (bool / null):
                // there is no `let X =` LHS, so the annotated deserialize is a bare
                // statement and needs its own terminating `;` (the non-empty branch
                // gets it from `?;`). Emitting just `?` drops the semicolon and the
                // next line (`{field}_present = true;`) fails to parse.
                ("".to_owned(), "?;")
            } else {
                (format!("let {var_names_str} = "), "?;")
            };
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            gen_scope
                .generate_deserialize(
                    types,
                    (&field.rust_type).into(),
                    DeserializeBeforeAfter::new("", "", true),
                    deser_config,
                    cli,
                )
                .annotate(&field.name, &before, after)
                .add_to_code(&mut deser_block_code);
        } else {
            let (before, after) = if var_names_str.is_empty() {
                // empty binding == a fixed value with no encoding var (bool / null): the
                // deserialize emits a trailing `()` value expr. Terminate it (`;`) so the following
                // `{field}_present = true;` parses — emitting a bare `()` leaves two adjacent
                // statements without a separator (pre-existing under --annotate-fields=false, the
                // sibling of the annotate branch's `?;`).
                ("".to_owned(), ";")
            } else {
                (format!("let {var_names_str} = "), ";")
            };
            // A deserialize binds its working vars (`{var}_value`, `{var}_encoding`,
            // `{var}_elem_encodings`, …) INLINE — with annotate=false no closure isolates them, so
            // an un-prefixed `{field}_encoding` shadows this match arm's outer accumulator and the
            // trailing reassignment below assigns the SHADOW. The outer accumulator then keeps its
            // `default()` and the constructor records default encodings, which is a
            // preserve-encodings violation wherever the shadow happens to be `mut` (the `Vec`/`Map`
            // sidecars are) and a compile error where it is not (E0384 on the scalar `_encoding`;
            // E0308 `Sz` vs `Option<Sz>` on a fixed value's). Neither half is fixable by typing the
            // binding — a `let {f}_encoding: LenEncoding = len.into();` ascription silences the
            // E0283 the un-pinned `.into()` raises and leaves the loss. Bind the temporaries under
            // the same `tmp_` prefix the binding pattern already uses, for EVERY field type, so the
            // reassignments below reach the accumulators and inference pins `.into()` through them.
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional)
                .overload_var_name(&temp_var_prefix);
            gen_scope
                .generate_deserialize(
                    types,
                    (&field.rust_type).into(),
                    DeserializeBeforeAfter::new(&before, after, false),
                    deser_config,
                    cli,
                )
                .add_to_code(&mut deser_block_code);
        }
        // Due to destructuring assignemnt (RFC 372 / 71156) being unstable we're forced to use temporaries then reassign after
        // which is not ideal but doing the assignment inside the lambda or otherwise has issues where it's putting lots of
        // context-sensitive logic into generate_deserialize and you would need to declare temporaries in most cases anyway
        // as cbor_event encoding-aware functions return tuples which just pushes the problem there instead.
        // We might be able to write a nice way around this in the annotate_fields=false, preserve_encodings=true case
        // but I don't think anyone (or many) would care about this as it's incredibly niche
        // (annotate_fields=false would be for minimizing code size but then preserve_encodings=true generates way more code)
        if field.rust_type.is_fixed_value() {
            deser_block_code
                .content
                .line(&format!("{}_present = true;", field.name));
        } else {
            deser_block_code
                .content
                .line(&format!("{} = Some(tmp_{});", field.name, field.name));
        }
        for enc_field in field_encoding_fields(
            types,
            &field.name,
            &field.rust_type,
            Some(&field.rule_metadata),
            false,
            cli,
        ) {
            deser_block_code.content.line(&format!(
                "{} = tmp_{};",
                enc_field.field_name, enc_field.field_name
            ));
        }
    } else if field.rust_type.is_fixed_value() {
        let mut dup_check = Block::new(format!("if {}_present", field.name));
        dup_check.line(format!(
            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
        ));
        deser_block_code.content.push_block(dup_check);
        // only does verification and sets the field_present bool to do error checking later
        if cli.annotate_fields {
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            let mut err_deser = gen_scope.generate_deserialize(
                types,
                (&field.rust_type).into(),
                DeserializeBeforeAfter::new("", "", false),
                deser_config,
                cli,
            );
            err_deser.content.line("Ok(true)");
            err_deser
                .annotate(&field.name, &format!("{}_present = ", field.name), "?;")
                .add_to_code(&mut deser_block_code);
        } else {
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            gen_scope
                .generate_deserialize(
                    types,
                    (&field.rust_type).into(),
                    DeserializeBeforeAfter::new("", "", false),
                    deser_config,
                    cli,
                )
                .add_to_code(&mut deser_block_code);
            deser_block_code
                .content
                .line(&format!("{}_present = true;", field.name));
        }
    } else {
        let mut dup_check = Block::new(format!("if {}.is_some()", field.name));
        dup_check.line(format!(
            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
        ));
        deser_block_code.content.push_block(dup_check);
        if cli.annotate_fields {
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            gen_scope
                .generate_deserialize(
                    types,
                    (&field.rust_type).into(),
                    DeserializeBeforeAfter::new("", "", true),
                    deser_config,
                    cli,
                )
                .annotate(&field.name, &format!("{} = Some(", field.name), "?);")
                .add_to_code(&mut deser_block_code);
        } else {
            let deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            gen_scope
                .generate_deserialize(
                    types,
                    (&field.rust_type).into(),
                    DeserializeBeforeAfter::new(&format!("{} = Some(", field.name), ");", false),
                    deser_config,
                    cli,
                )
                .add_to_code(&mut deser_block_code);
        }
    }
    if cli.preserve_encodings {
        let key_encoding = key_encoding_field(&field.name, key);
        deser_block_code
            .content
            .line(&format!(
                "{} = {};",
                key_encoding.field_name,
                key_encoding.enc_conversion("key_enc")
            ))
            .line(&format!("orig_deser_order.push({field_index});"));
    }
    deser_block.push_all(deser_block_code.mark_and_extract_content(deser_code));
    deser_block
}

pub(super) fn codegen_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    tag: Option<usize>,
    record: &RustRecord,
    config: &RustStructConfig,
    cli: &Cli,
) {
    // NOTE: mirrored by emit_tests::record_ctor_can_fail — keep the two in sync
    let new_can_fail = record
        .fields
        .iter()
        .any(|f| !f.optional && f.rust_type.has_value_bounds())
        || record
            .captured_dynamic_rows()
            .any(|row| row.is_non_empty_array_tail() && row.element().has_value_bounds());
    // wasm wrapper
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        if new_can_fail {
            wasm_new.ret(format!("Result<{name}, JsError>"));
        } else {
            wasm_new.ret("Self");
        }
        wasm_new.vis("pub");
        let mut wasm_new_args = Vec::new();
        let mut wasm_new_comments = Vec::new();
        for field in &record.fields {
            // Fixed values don't need constructors or getters or fields in the rust code
            if !field.rust_type.is_fixed_value() {
                if field.optional {
                    // setter
                    let mut setter = codegen::Function::new(format!("set_{}", field.name));
                    setter
                        .arg_mut_self()
                        .arg(&field.name, field.rust_type.for_wasm_param(types))
                        .vis("pub");
                    // don't call needs_bounds_check_if_inlined() since if it's a RustType it's checked during that ctor
                    if field.rust_type.has_value_bounds() {
                        setter.ret("Result<(), JsError>");
                        if let Some(line) =
                            value_bounds_check_line(&field.rust_type, &field.name, true)
                        {
                            setter.line(&line);
                        }
                    }
                    if field.rust_type.config.default.is_some() {
                        setter.line(format!(
                            "self.0.{} = {}",
                            field.name,
                            ToWasmBoundaryOperations::format(
                                field
                                    .rust_type
                                    .from_wasm_boundary_clone(types, &field.name, false)
                                    .into_iter()
                            )
                        ));
                    } else {
                        setter.line(format!(
                            "self.0.{} = Some({})",
                            field.name,
                            ToWasmBoundaryOperations::format(
                                field
                                    .rust_type
                                    .from_wasm_boundary_clone(types, &field.name, false)
                                    .into_iter()
                            )
                        ));
                    }

                    wrapper.s_impl.push_fn(setter);
                    // getter
                    // Set true iff the getter takes the flatten path below (nullable optional field
                    // stored as Option<Option<T>>). This is the single source of truth for "this
                    // position is lossy", so the `has_<field>` presence accessor emitted after the
                    // getter can never drift from the flatten emission.
                    let mut field_getter_flattens = false;
                    let mut getter = codegen::Function::new(&field.name);
                    getter.arg_ref_self().vis("pub");
                    if field.rust_type.config.default.is_some() {
                        getter.ret(field.rust_type.for_wasm_return(types)).line(
                            field.rust_type.to_wasm_boundary(
                                types,
                                &format!("self.0.{}", field.name),
                                false,
                            ),
                        );
                    } else if field.is_double_option() {
                        // A nullable optional field is stored as `Option<Option<T>>`, which
                        // wasm-bindgen can't return. Flatten the presence-`Option` into the value's
                        // `Option` and return a single `Option<T>` (same convention as the map
                        // accessors / c-style enum getters). Native storage keeps all three states
                        // (absent / present-null / present-value), so CBOR round-trips are unaffected —
                        // only the wasm read conflates absent with present-null.
                        field_getter_flattens = true;
                        getter
                            .doc("Returns None if the field is absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).")
                            .ret(field.rust_type.for_wasm_return(types))
                            .line(format!(
                                "self.0.{}{}.flatten()",
                                field.name,
                                if field.rust_type.is_copy(types) {
                                    ""
                                } else {
                                    ".clone()"
                                }
                            ));
                    } else {
                        getter
                            .ret(format!(
                                "Option<{}>",
                                field.rust_type.for_wasm_return(types)
                            ))
                            .line(field.rust_type.to_wasm_boundary_optional(
                                types,
                                &format!("self.0.{}", field.name),
                                false,
                            ));
                    }
                    wrapper.s_impl.push_fn(getter);
                    // Presence accessor for the flattened optional-nullable field. The getter above
                    // collapses Option<Option<T>> -> Option<T> (absent and present-null both read
                    // None); `has_<field>()` exposes the outer presence so a JS consumer can tell the
                    // three states apart. Gated on `field_getter_flattens` — the exact flatten
                    // condition — so the accessor and the flatten can never diverge.
                    //
                    // Collision guard: the accessor name `has_<field>` is synthesized, so a sibling
                    // field literally named `has_<field>` (whose own wasm getter is `pub fn
                    // has_<field>`) would make two identically-named methods in one impl —
                    // non-compiling (E0592/E0201) for an otherwise-valid spec. On a clash we SKIP the
                    // disambiguator loudly rather than invent a rename: the flattening getter still
                    // works, only the three-state distinguisher is lost. The wasm getter surface of a
                    // record is exactly one method per non-fixed-value field, named `field.name`, so a
                    // clash is exactly `has_<field>` appearing as a sibling field name.
                    if field_getter_flattens {
                        let has_name = format!("has_{}", field.name);
                        let collides = record
                            .fields
                            .iter()
                            .filter(|f| !f.rust_type.is_fixed_value())
                            .any(|f| f.name == has_name);
                        if collides {
                            crate::warn!(
                                "cddl-codegen --wasm: {name}: presence accessor `{has_name}()` for \
                                 optional-nullable field `{}` collides with a sibling field of the \
                                 same name — skipping the accessor (the flattening getter still \
                                 works; the absent-vs-present-null distinction is lost for this field)",
                                field.name
                            );
                        } else {
                            let mut has_field = codegen::Function::new(&has_name);
                            has_field
                                .arg_ref_self()
                                .vis("pub")
                                .ret("bool")
                                .doc("Returns whether the optional field is present (outer Some), distinguishing an absent field from a present-but-null one (both of which the getter reports as None).")
                                .line(format!("self.0.{}.is_some()", field.name));
                            wrapper.s_impl.push_fn(has_field);
                        }
                    }
                } else {
                    // new
                    wasm_new.arg(&field.name, field.rust_type.for_wasm_param(types));
                    wasm_new_args.push(ToWasmBoundaryOperations::format(
                        field
                            .rust_type
                            .from_wasm_boundary_clone(types, &field.name, false)
                            .into_iter(),
                    ));
                    if let Some(comment) = &field.rule_metadata.comment {
                        wasm_new_comments.push(format!("* `{}` - {}", field.name, comment));
                    }
                    // do we want setters here later for mandatory types covered by new?
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter
                        .arg_ref_self()
                        .ret(field.rust_type.for_wasm_return(types))
                        .vis("pub")
                        .line(field.rust_type.to_wasm_boundary(
                            types,
                            &format!("self.0.{}", field.name),
                            false,
                        ));
                    wrapper.s_impl.push_fn(getter);
                }
            } else if field.optional && field.rust_type.is_fixed_value() {
                // Optional fixed value: the native struct stores presence as a `bool`. Expose that
                // bit across the wasm boundary — getter returns it, setter sets it. (Mandatory
                // fixed values carry no information and get no accessor, same as the rust side.)
                let mut getter = codegen::Function::new(&field.name);
                getter
                    .arg_ref_self()
                    .ret("bool")
                    .vis("pub")
                    .line(format!("self.0.{}", field.name));
                wrapper.s_impl.push_fn(getter);
                let mut setter = codegen::Function::new(format!("set_{}", field.name));
                setter
                    .arg_mut_self()
                    .arg("present", "bool")
                    .vis("pub")
                    .line(format!("self.0.{} = present", field.name));
                wrapper.s_impl.push_fn(setter);
            }
        }
        // An OPEN TABLE's TYPED row: its map surface is FLATTENED onto THIS class instead of hung off
        // a whole-map getter — `insert`/`get`/`len`/`keys` (plus `has` for a nullable value) delegate
        // straight to the typed container field, through the very helper the two map-wrapper twins
        // use. The rationale is the set nominal's, verbatim (`docs/docs/wasm_differences.mdx` §
        // "Sets"): a wasm class has no `Deref`, so a two-layer `t.entries().get(k)` is the cost of a
        // container getter, and a JS caller wants `t.get(k)`. It also means the typed row mints NO
        // `MapKToV` class of its own, which is what leaves the collision-detector family at three new
        // legs rather than four (see the note in `non_empty_map_wrapper_name_collisions`). The
        // CATCH-ALL row keeps its container and its read-only `rest()` getter, below.
        if let Some(typed) = record.typed_row().filter(|r| !r.is_array_tail()) {
            // The NonEmpty flavor's construction door, mirroring the rust `new(first_key,
            // first_value)` it calls: the ONE case where an open table's wasm `new` takes arguments.
            if record.is_non_empty_open_table() {
                wasm_new
                    .arg("first_key", typed.domain().for_wasm_param(types))
                    .arg("first_value", typed.range().for_wasm_param(types));
                wasm_new_args.push(ToWasmBoundaryOperations::format(
                    typed
                        .domain()
                        .from_wasm_boundary_clone(types, "first_key", false)
                        .into_iter(),
                ));
                wasm_new_args.push(ToWasmBoundaryOperations::format(
                    typed
                        .range()
                        .from_wasm_boundary_clone(types, "first_value", false)
                        .into_iter(),
                ));
                wasm_new_comments.push(
                    "* `first_key` - the key of the first typed entry (CDDL `+ k1 => v1`: an open \
                     table spelled with `+` holds at least one typed entry)"
                        .to_owned(),
                );
                wasm_new_comments.push("* `first_value` - its value".to_owned());
            }
            wrapper
                .s_impl
                .new_fn("len")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .doc(
                    "The number of TYPED entries (CDDL `* k1 => v1`, the first row). The catch-all \
                     row's own count is `rest().len()`.",
                )
                .line(format!("self.0.{}.len()", typed.field_name));
            push_table_accessors(
                gen_scope,
                &mut wrapper,
                types,
                typed.domain(),
                typed.range(),
                &format!("self.0.{}", typed.field_name),
                cli,
            );
        }
        // A one-or-more open-array tail has the same valid-by-construction door as its Rust record:
        // take one element here and let the Rust `new(first)` build the restricted `NonEmptyVec`.
        // The name follows the Rust constructor's collision-safe synthesis exactly.
        for rest in record
            .captured_dynamic_rows()
            .filter(|row| row.is_non_empty_array_tail())
        {
            let mut first_arg = format!("first_{}_element", rest.field_name);
            let reserved: Vec<String> = record
                .fields
                .iter()
                .map(|field| field.name.clone())
                .chain(record.dynamic_rows().map(|row| row.field_name.clone()))
                .collect();
            let mut suffix = 2;
            while reserved.iter().any(|name| name == &first_arg) {
                first_arg = format!("first_{}_element_{suffix}", rest.field_name);
                suffix += 1;
            }
            wasm_new.arg(&first_arg, rest.element().for_wasm_param(types));
            wasm_new_args.push(ToWasmBoundaryOperations::format(
                rest.element()
                    .from_wasm_boundary_clone(types, &first_arg, false)
                    .into_iter(),
            ));
            wasm_new_comments.push(format!(
                "* `{first_arg}` - the first trailing element (CDDL `+ t` / `1* t`: the tail holds at least one element)"
            ));
        }
        // Open rest (CAPTURE only): a getter returning the captured content as its minted wasm wrapper
        // — a map wrapper (`MapKToV` / the `@duplicates preserve` PairMap-backed twin) for a `* k => v`
        // row, or a list wrapper (`TList` / `AnyList`) for an array `* t` tail. Deliberately no `new()`
        // arg and no setter — the rest defaults empty and rides the wrapper's own mutation surface
        // (matching the rust side, where `new()` excludes it). The wrapper class is minted in the wasm
        // pass (`mint_wasm_wrapper_for_visited_type` for the rest map/list). An `@ignore` row/tail
        // stores nothing, so it has no getter (its wasm class is a closed struct's). An open table's
        // TYPED row is excluded — its surface is flattened above.
        for rest in record
            .captured_dynamic_rows()
            .filter(|r| !record.is_typed_row(r))
        {
            let rest_ty = rest_member_type(rest);
            let mut getter = codegen::Function::new(&rest.field_name);
            getter
                .arg_ref_self()
                .ret(rest_ty.for_wasm_return(types))
                .vis("pub")
                .doc(if rest.is_non_empty_array_tail() {
                    "The captured one-or-more trailing array elements beyond the declared members \
                     (CDDL `+ t` / `1* t` rest tail), as the restricted wasm list wrapper."
                } else if rest.is_array_tail() {
                    "The captured trailing array elements beyond the declared members (CDDL \
                     `* t` rest tail), as the wasm list wrapper."
                } else {
                    "The captured open-map entries whose keys are not declared fields (CDDL \
                     `* k => v` rest row), as the wasm map wrapper."
                })
                .line(rest_ty.to_wasm_boundary(
                    types,
                    &format!("self.0.{}", rest.field_name),
                    false,
                ));
            wrapper.s_impl.push_fn(getter);
        }
        if new_can_fail {
            wasm_new.line(format!(
                "{}::new({}).map(Into::into).map_err(Into::into)",
                rust_crate_struct_from_wasm(types, name, cli),
                wasm_new_args.join(", ")
            ));
        } else {
            wasm_new.line(format!(
                "Self({}::new({}))",
                rust_crate_struct_from_wasm(types, name, cli),
                wasm_new_args.join(", ")
            ));
        }
        if !wasm_new_comments.is_empty() {
            wasm_new.doc(wasm_new_comments.join("\n"));
        }
        if let Some(doc) = ignore_aware_doc(config.doc.as_deref(), record.ignored_rest()) {
            wrapper.s.doc(&doc);
        }
        wrapper.s_impl.push_fn(wasm_new);
        wrapper.push(gen_scope, types);
    }

    // Rust-only for the rest of this function

    // Struct (fields) + constructor.
    //
    // An OPEN TABLE owns its JSON face BY HAND (`emit_open_table_json`, below): the derives cannot
    // express two flattened regions in one object, so they are suppressed here exactly as
    // `@custom_json` suppresses them — same switch, same three steering sites (the derives, the
    // encodings field's `#[serde(skip)]`, and the rows' flatten attributes) — and the impls are
    // emitted instead. A spec author's own `@custom_json` still wins: there the user owns the impls
    // and the tool emits none.
    let hand_written_open_table_json = record.is_open_table()
        && !config.custom_json
        && (cli.json_serde_derives || cli.json_schema_export);
    let manual_json = config.custom_json || hand_written_open_table_json;
    let (mut native_struct, mut native_impl) =
        create_base_rust_struct(types, name, manual_json, None, cli);
    native_struct.vis("pub");
    if let Some(doc) = ignore_aware_doc(config.doc.as_deref(), record.ignored_rest()) {
        native_struct.doc(&doc);
    }
    let mut native_new = codegen::Function::new("new");
    let (ctor_ret, ctor_before) = if new_can_fail {
        ("Result<Self, DeserializeError>", "Ok(Self")
    } else {
        ("Self", "Self")
    };
    native_new.ret(ctor_ret).vis("pub");
    let mut native_new_block = Block::new(ctor_before);
    if new_can_fail {
        native_new_block.after(")");
    }
    let mut native_new_comments = Vec::new();
    // for clippy we generate a Default impl if new has no args
    let mut new_arg_count = 0;
    for field in &record.fields {
        // (a field whose type has no deserialize refuses this record's too — recorded ahead of the
        // emission walk by `seed_no_deserialize_verdicts`, which is where the reason text lives)
        //
        // Fixed values only exist in (de)serialization code (outside of preserve-encodings=true)
        if !field.rust_type.is_fixed_value() {
            let mut codegen_field = if let Some(default_value) = &field.rust_type.config.default {
                // new
                native_new_block.line(format!(
                    "{}: {},",
                    field.name,
                    default_value.to_primitive_str_assign()
                ));
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false, cli),
                )
            } else if field.optional {
                // new
                native_new_block.line(format!("{}: None,", field.name));
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    format!(
                        "Option<{}>",
                        field.rust_type.for_rust_member(types, false, cli)
                    ),
                )
            } else {
                // new
                native_new.arg(&field.name, field.rust_type.for_rust_move(types, cli));
                if let Some(comment) = &field.rule_metadata.comment {
                    native_new_comments.push(format!("* `{}` - {}", field.name, comment));
                }
                new_arg_count += 1;
                native_new_block.line(format!("{},", field.name));
                if let Some(line) = value_bounds_check_line(&field.rust_type, &field.name, true) {
                    native_new.line(&line);
                }
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false, cli),
                )
            };
            if let Some(comment) = &field.rule_metadata.comment {
                codegen_field.doc(comment);
            }
            // A member CARRYING `any` renders its JSON NATURALLY (not
            // through `AnyCbor`'s tagged codec). Route the serde/schemars derives on this field
            // through the matching natural adapter. Skipped when the struct owns a custom json impl
            // (no derives to steer). Positions covered: a bare `any` (`Direct`), a homogeneous
            // `[* any]` array member (`Seq`), and a `{* K => any}` table member with a stringifiable
            // (non-`any`) key — `Map` (non-preserve `BTreeMap`) / `OrderedMap` (preserve
            // `OrderedHashMap`) — plus the optional (`? N: …` → `Option<…>`) counterpart of each.
            if !config.custom_json {
                use super::NaturalAnyPosition as P;
                let resolves_any = |ty: &crate::intermediate::RustType| {
                    matches!(
                        ty.conceptual_type.resolve_alias_shallow(),
                        ConceptualRustType::Any
                    )
                };
                let opt = field.optional;
                let position = match field.rust_type.conceptual_type.resolve_alias_shallow() {
                    ConceptualRustType::Any => Some(if opt { P::Optional } else { P::Direct }),
                    ConceptualRustType::Array(inner) if resolves_any(inner) => {
                        // Alias-aware: a named `[2*3 any]` field resolves shallowly to Array but
                        // its checked bounds live on the RustType configuration. The bounded
                        // adapter keeps natural JSON's fallible AnyCbor walk AND re-enters the
                        // BoundedVec TryFrom door instead of pretending this is Vec<AnyCbor>.
                        field
                            .rust_type
                            .type_enforced_bounded_array_u64_bounds()
                            .map_or_else(
                                || Some(if opt { P::OptSeq } else { P::Seq }),
                                |(min, max)| {
                                    Some(if opt {
                                        P::OptBoundedSeq(min, max)
                                    } else {
                                        P::BoundedSeq(min, max)
                                    })
                                },
                            )
                    }
                    // An `any`-keyed table stays tagged (its key already errors at runtime per
                    // RFC 8949 §6.1), so require a non-`any` key. Preserve → `OrderedHashMap`, else `BTreeMap`.
                    ConceptualRustType::Map(k, v) if resolves_any(v) && !resolves_any(k) => {
                        Some(match (cli.preserve_encodings, opt) {
                            (false, false) => P::Map,
                            (false, true) => P::OptMap,
                            (true, false) => P::OrderedMap,
                            (true, true) => P::OptOrderedMap,
                        })
                    }
                    _ => None,
                };
                if let Some(position) = position {
                    for annotation in super::natural_any_serde_annotations(cli, position) {
                        codegen_field.annotation(annotation);
                    }
                }
            }
            // A member that is BOTH optional and nullable is stored as a nested `Option<Option<…>>`,
            // which serde's plain derive collapses in both directions (present-null reads back as
            // absent; absent writes as `null`). Steer it through the `double_option` adapter so the
            // JSON surface carries the same three states the CBOR surface does. Disjoint from the
            // natural-`any` block above by construction: that one matches on the field's OUTER
            // conceptual type being `Any`/`Array`/`Map`, this one on its being `Optional`.
            //
            // `manual_json` excludes the two hand-owned JSON faces, and neither is a silent gap: an
            // open table has NO declared fields at all (`is_open_table` requires `fields.is_empty()`),
            // so the shape is unreachable there by construction; and under `@custom_json` the spec
            // author owns both serde impls, so the tool steers nothing.
            if !manual_json && field.is_double_option() {
                // The member type verbatim, spelled exactly as the field declaration above spells
                // it — it is what the `schemars` neutralizer hands back to the derive.
                let member_type = format!(
                    "Option<{}>",
                    field.rust_type.for_rust_member(types, false, cli)
                );
                for annotation in super::double_option_serde_annotations(cli, &member_type) {
                    codegen_field.annotation(annotation);
                }
            }
            native_struct.push_field(codegen_field);
        } else if field.optional && field.rust_type.is_fixed_value() {
            // An OPTIONAL fixed value carries exactly one bit — present or absent — so it needs a
            // struct field to store it (a MANDATORY fixed value carries zero information and gets
            // none). A `bool` (not `Option<()>`) crosses the wasm and serde/schemars boundaries
            // cleanly. Optional fields aren't constructor args, so `new` defaults it to `false`.
            let fixed_lit = match &field.rust_type.clone().resolve_aliases().conceptual_type {
                ConceptualRustType::Fixed(FixedValue::Bool(b)) => b.to_string(),
                ConceptualRustType::Fixed(FixedValue::Uint(u)) => u.to_string(),
                ConceptualRustType::Fixed(FixedValue::Nint(i)) => i.to_string(),
                ConceptualRustType::Fixed(FixedValue::Null) => "null".to_owned(),
                ConceptualRustType::Fixed(FixedValue::Undefined) => "undefined".to_owned(),
                ConceptualRustType::Fixed(FixedValue::Text(s)) => format!("\"{s}\""),
                ConceptualRustType::Fixed(FixedValue::Bytes(bytes)) => format!(
                    "h'{}'",
                    bytes
                        .iter()
                        .map(|byte| format!("{byte:02X}"))
                        .collect::<String>()
                ),
                // float_literal, not Display: `{}` on a whole-valued f64 drops the decimal point
                // (`3.0` -> `3`); the doc string mirrors the CDDL literal (`? f: 2.5`).
                ConceptualRustType::Fixed(FixedValue::Float(f)) => float_fixed_literal(*f),
                _ => unreachable!("is_fixed_value() matched a non-fixed conceptual type"),
            };
            native_new_block.line(format!("{}: false,", field.name));
            let mut codegen_field = codegen::Field::new(format!("pub {}", field.name), "bool");
            codegen_field.doc(format!(
                "Whether the optional fixed value `{fixed_lit}` (CDDL `? {}: {fixed_lit}`) is present; `false` means absent.",
                field.name
            ));
            native_struct.push_field(codegen_field);
        }
    }
    // Open rest (CAPTURE only): a `pub` field holding the captured content — a map container for a
    // `* k => v` rest row, a `Vec<T>` for a loose array `* t` tail, or `NonEmptyVec<T>` for a `+ t`
    // tail. Loose tails stay out of `new()` and default empty; a non-empty tail takes its first
    // element so constructed values cannot violate the schema. Map containers match the table switch (non-preserve `BTreeMap`;
    // `OrderedHashMap` under `--preserve-encodings`). An `@ignore` row/tail emits NO field (it drops
    // unknown entries), so the struct is a closed struct's.
    for rest in record.captured_dynamic_rows() {
        let mut rest_field = codegen::Field::new(
            format!("pub {}", rest.field_name),
            rest_member_type(rest).for_rust_member(types, false, cli),
        );
        rest_field.doc(if rest.is_non_empty_array_tail() {
            "Captured one-or-more trailing array elements beyond the declared members (CDDL `+ t` / \
             `1* t` rest tail). Serialized after the declared members; never empty."
        } else if rest.is_array_tail() {
            "Captured trailing array elements beyond the declared members (CDDL `* t` rest tail). \
             Serialized after the declared members; defaults empty."
        } else if record.is_typed_row(rest) {
            // The one `pub` rust field with no wasm getter by design — the string is a provenance
            // marker the parity differential reads, so it lives beside the marker it belongs with.
            super::OPEN_TABLE_TYPED_ROW_DOC
        } else if record.is_open_table() {
            "The open table's CAPTURED entries (CDDL `* k2 => v2`, the trailing catch-all row): every \
             map entry the typed row did not claim. Defaults empty. `@duplicates preserve` makes this \
             a `PairMap` (duplicate keys kept, in wire order); otherwise the loose table container."
        } else {
            "Captured open-map entries whose keys are not declared fields (CDDL `* k => v` rest row). \
             Serialized after the declared fields; defaults empty. `@duplicates preserve` makes this \
             a `PairMap` (duplicate keys kept, in wire order); otherwise the loose table container."
        });
        // The rest field's JSON surface. Skipped when the struct owns a custom json impl (no derive to
        // steer) — matches the declared-field handling. A MAP `* k => v` row flattens its entries to
        // the object's top level (`emit_rest_flatten_json`). An ARRAY `* t` tail renders as an ORDINARY
        // JSON array under the field name (positions are already erased in JSON, and serde flatten has
        // no array analog): skip-if-empty on write + default-on-read (so an empty tail ≡ closed-struct
        // JSON, mirroring the empty-tail ≡ closed-struct CBOR invariant), and — for an `any`-element
        // tail (`Vec<AnyCbor>`) — the natural-fallible walk reusing the homogeneous `[* any]` member
        // `Seq` adapter (a typed element uses its own serde).
        if !manual_json {
            if rest.is_array_tail() {
                if cli.json_serde_derives && !rest.is_non_empty_array_tail() {
                    rest_field
                        .annotation("#[serde(skip_serializing_if = \"Vec::is_empty\", default)]");
                }
                let elem_is_any = matches!(
                    rest.element().conceptual_type.resolve_alias_shallow(),
                    ConceptualRustType::Any
                );
                if elem_is_any {
                    for annotation in super::natural_any_serde_annotations(
                        cli,
                        if rest.is_non_empty_array_tail() {
                            super::NaturalAnyPosition::NonEmptySeq
                        } else {
                            super::NaturalAnyPosition::Seq
                        },
                    ) {
                        rest_field.annotation(annotation);
                    }
                }
            } else {
                for annotation in emit_rest_flatten_json(gen_scope, types, name, record, rest, cli)
                {
                    rest_field.annotation(annotation);
                }
            }
        }
        native_struct.push_field(rest_field);
        // The NonEmpty open table's typed row is the ONE captured row `new()` does not default empty:
        // its min-1 bound must hold for every constructed value, so the door takes the first entry —
        // `NonEmptyMap::new(first_key, first_value)` verbatim (`static/non_empty_map.rs`), the same
        // door the wasm `{+ k => v}` wrapper offers. Infallible by construction, so `new` keeps its
        // return type and no caller has a `Result` to unwrap.
        if record.is_non_empty_open_table() && record.is_typed_row(rest) {
            let (key_arg, value_arg) = ("first_key", "first_value");
            native_new.arg(key_arg, rest.domain().for_rust_move(types, cli));
            native_new.arg(value_arg, rest.range().for_rust_move(types, cli));
            new_arg_count += 2;
            native_new_comments.push(format!(
                "* `{key_arg}` - the key of the first typed entry (CDDL `+ k1 => v1`: an open table \
                 spelled with `+` holds at least one typed entry)"
            ));
            native_new_comments.push(format!("* `{value_arg}` - its value"));
            // The staging local is a FIXED name, deliberately not the row's field name: the row's
            // name is `@name`-settable, and a row named `first_key` would have the block's own
            // binding shadow the parameter it is being handed.
            let mut seed = Block::new(format!("{}:", rest.field_name));
            seed.line(format!(
                "let mut seed = {}::new();",
                rest_container_ctor(rest, cli)
            ));
            seed.line(format!("seed.insert({key_arg}, {value_arg});"));
            seed.line("seed");
            seed.after(",");
            native_new_block.push_block(seed);
        } else if rest.is_non_empty_array_tail() {
            let mut first_arg = format!("first_{}_element", rest.field_name);
            let reserved: Vec<String> = record
                .fields
                .iter()
                .map(|field| field.name.clone())
                .chain(record.dynamic_rows().map(|row| row.field_name.clone()))
                .collect();
            let mut suffix = 2;
            while reserved.iter().any(|name| name == &first_arg) {
                first_arg = format!("first_{}_element_{suffix}", rest.field_name);
                suffix += 1;
            }
            native_new.arg(&first_arg, rest.element().for_rust_move(types, cli));
            new_arg_count += 1;
            native_new_comments.push(format!(
                "* `{first_arg}` - the first trailing element (CDDL `+ t` / `1* t`: the tail holds at least one element)"
            ));
            native_new_block.line(format!(
                "{}: NonEmptyVec::new({first_arg}),",
                rest.field_name
            ));
            if let Some(line) = value_bounds_check_line(rest.element(), &first_arg, true) {
                native_new.line(&line);
            }
        } else {
            native_new_block.line(format!(
                "{}: {}::new(),",
                rest.field_name,
                rest_container_ctor(rest, cli)
            ));
        }
    }
    // The open table's hand-written JSON face, in place of the derives suppressed above.
    if hand_written_open_table_json {
        emit_open_table_json(gen_scope, types, name, record, cli);
    }
    if !native_new_comments.is_empty() {
        native_new.doc(native_new_comments.join("\n"));
    }
    let len_encoding_var = if cli.preserve_encodings {
        let encoding_name = RustIdent::new(CDDLIdent::new(format!("{name}Encoding")));
        native_struct.field(
            format!(
                "{}pub encodings",
                encoding_var_macros(types.key_demand(name), manual_json, cli)
            ),
            format!("Option<{encoding_name}>"),
        );
        native_new_block.line("encodings: None,");

        let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
        let mut encoding_aliases: Vec<(String, String)> = Vec::new();
        encoding_struct.field("pub len_encoding", "LenEncoding");
        if tag.is_some() {
            encoding_struct.field("pub tag_encoding", "Option<cbor_event::Sz>");
        }
        if record.rep == Representation::Map {
            encoding_struct.field("pub orig_deser_order", "Vec<usize>");
        }
        for field in &record.fields {
            // even fixed values still need to keep track of their encodings.
            // DECLARED type (see `EncodingField::type_name`): these `type_name`s become this encoding
            // struct's field types, which must spell the member as its data-struct field does.
            for field_enc in field_encoding_fields(
                types,
                &field.name,
                &field.rust_type,
                Some(&field.rule_metadata),
                true,
                cli,
            ) {
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    name,
                    &field_enc.field_name,
                    &field_enc.type_name,
                );
            }
            if record.rep == Representation::Map {
                let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    name,
                    &key_enc.field_name,
                    &key_enc.type_name,
                );
            }
        }
        // Open rest per-entry encoding sidecars (CONCRETE content only — `any`-typed content is
        // self-carried and contributes none). A map `* k => v` row: `{field}_{key,value}_encodings`,
        // keyed by the key VALUE (`BTreeMap`) for the loose (reject/default) container, or POSITIONAL
        // (`Vec`, indexed by entry position) for the `@duplicates preserve` twin whose keys repeat —
        // exactly the loose-table pair-map split. An array `* t` tail: one POSITIONAL
        // `{field}_elem_encodings: Vec<..>` (extras are positional by construction — no keys, so no
        // key map and no `orig_deser_order`). In both reps the rest's OWN header length lives in the
        // owner's `len_encoding` (no `_encoding` here). Capture-only (an `@ignore` row/tail stores
        // nothing, and it is rejected under preserve anyway).
        for rest in record.captured_dynamic_rows() {
            if rest.is_array_tail() {
                // Array `* t` tail: a single POSITIONAL `Vec` sidecar for the element encoding,
                // indexed by tail position (extras sit strictly after the declared prefix, so wire
                // order = `Vec` order — no key map, no `orig_deser_order`). A self-carried `any`
                // element contributes none.
                let elem_encs = rest_array_elem_encoding_fields(types, rest, cli);
                if !elem_encs.is_empty() {
                    push_encoding_struct_field(
                        &mut encoding_struct,
                        &mut encoding_aliases,
                        name,
                        &format!("{}_elem_encodings", rest.field_name),
                        &format!("Vec<{}>", tuple_type_name(&elem_encs)),
                    );
                }
            } else {
                let key_rust = rest.domain().for_rust_member(types, false, cli);
                let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
                let sidecar_type = |elem: String| {
                    if rest_is_pair_map(rest) {
                        format!("Vec<{elem}>")
                    } else {
                        format!("BTreeMap<{key_rust}, {elem}>")
                    }
                };
                if !key_encs.is_empty() {
                    push_encoding_struct_field(
                        &mut encoding_struct,
                        &mut encoding_aliases,
                        name,
                        &format!("{}_key_encodings", rest.field_name),
                        &sidecar_type(tuple_type_name(&key_encs)),
                    );
                }
                if !value_encs.is_empty() {
                    push_encoding_struct_field(
                        &mut encoding_struct,
                        &mut encoding_aliases,
                        name,
                        &format!("{}_value_encodings", rest.field_name),
                        &sidecar_type(tuple_type_name(&value_encs)),
                    );
                }
            }
        }

        let enc_scope = gen_scope.cbor_encodings(types, name);
        for (alias, target) in encoding_aliases {
            enc_scope.push_type_alias(TypeAlias::new(&alias, &target).vis("pub").clone());
        }
        enc_scope.push_struct(encoding_struct);

        Some("len_encoding")
    } else {
        None
    };
    native_new.push_block(native_new_block);
    native_impl.push_fn(native_new);

    // A whole-record custom pair owns the complete CBOR item. Generate only the shared-contract
    // trait shells: root references dispatch to the same free functions before their kind-specific
    // path, so direct and embedded APIs have one wire form. No embedded-group trait belongs here.
    if let (Some(custom_serialize), Some(custom_deserialize)) =
        (&config.custom_serialize, &config.custom_deserialize)
    {
        let mut ser_impl = make_serialization_impl(name.as_ref(), cli);
        let mut ser_func = make_serialization_function("serialize", cli);
        ser_func.line(format!(
            "{}(serializer, self{})",
            custom_serialize,
            canonical_param(cli)
        ));
        ser_impl.push_fn(ser_func);
        gen_scope.rust_serialize(types, name).push_impl(ser_impl);

        let mut deser_impl = codegen::Impl::new(name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize", cli);
        deser_func.line(format!("{}(raw)", custom_deserialize));
        deser_impl.push_fn(deser_func);
        gen_scope.rust_serialize(types, name).push_impl(deser_impl);
    } else {
        let (ser_func, mut ser_impl, mut ser_embedded_impl) = create_serialize_impls(
            name,
            Some(record.rep),
            tag,
            &record.definite_info("self", false, types, cli),
            len_encoding_var
                .map(|var| {
                    format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()")
                })
                .as_deref(),
            types.is_plain_group(name),
            cli,
        );
        let mut ser_func = match ser_embedded_impl {
            Some(_) => {
                ser_impl.push_fn(ser_func);
                make_serialization_function("serialize_as_embedded_group", cli)
            }
            None => ser_func,
        };
        // Deliberate-lossiness breadcrumb on the public serialize fn of an `@ignore` open struct-map
        // or open array (a rest row/tail honored here can only sit in a map/array record, never a plain
        // group, so this is the real `serialize` — never `serialize_as_embedded_group`). Array-worded
        // for an array tail.
        if let Some(rest) = record.ignored_rest() {
            ser_func.doc(ignore_lossiness_doc(rest));
        }
        let mut deser_code = DeserializationCode::default();
        let in_embedded = types.is_plain_group(name);
        let ctor_block = match record.rep {
            Representation::Array => {
                generate_array_struct_serialization(
                    gen_scope,
                    types,
                    record,
                    true,
                    &mut ser_func,
                    cli,
                );
                let code = generate_array_struct_deserialization(
                    gen_scope,
                    types,
                    record,
                    tag,
                    in_embedded,
                    true,
                    cli,
                );
                code.deser_code.add_to_code(&mut deser_code);
                let mut deser_ctor = Block::new(format!("Ok({name}"));
                for (var, expr) in code.deser_ctor_fields {
                    if var == expr {
                        deser_ctor.line(format!("{var},"));
                    } else {
                        deser_ctor.line(format!("{var}: {expr},"));
                    }
                }
                if !code.encoding_struct_ctor_fields.is_empty() {
                    let mut encoding_ctor_block =
                        Block::new(format!("encodings: Some({name}Encoding"));
                    encoding_ctor_block.after("),");
                    for (var, expr) in code.encoding_struct_ctor_fields {
                        if var == expr {
                            encoding_ctor_block.line(format!("{var},"));
                        } else {
                            encoding_ctor_block.line(format!("{var}: {expr},"));
                        }
                    }
                    deser_ctor.push_block(encoding_ctor_block);
                }
                deser_ctor.after(")");
                deser_ctor
            }
            Representation::Map => {
                let mut uint_field_deserializers = Vec::new();
                let mut text_field_deserializers = Vec::new();
                // (field_index, field, content) -- this is ordered by canonical order
                let mut ser_content: Vec<(usize, &RustField, BlocksOrLines)> = Vec::new();
                if cli.preserve_encodings {
                    deser_code
                        .content
                        .line("let mut orig_deser_order = Vec::new();");
                }
                // we default to canonical ordering here as the default ordering as that should be the most useful
                // keep in mind this is always overwritten if you have cli.preserve_encodings enabled AND there was
                // a deserialized encoding, otherwise we still use this by default.
                for (field_index, field) in record.canonical_ordering() {
                    // (a plain-group field refuses this map record's deserialize — recorded ahead
                    // of the emission walk by `map_record_deser_refusals`, which is where the
                    // reason text and the why live)
                    // declare variables for deser loop
                    if cli.preserve_encodings {
                        for field_enc in field_encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type,
                            Some(&field.rule_metadata),
                            true,
                            cli,
                        ) {
                            deser_code.content.line(&format!(
                                "let mut {} = {};",
                                field_enc.field_name, field_enc.default_expr
                            ));
                        }
                        let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                        deser_code.content.line(&format!(
                            "let mut {} = {};",
                            key_enc.field_name, key_enc.default_expr
                        ));
                    }
                    if field.rust_type.is_fixed_value() {
                        deser_code
                            .content
                            .line(&format!("let mut {}_present = false;", field.name));
                    } else {
                        deser_code
                            .content
                            .line(&format!("let mut {} = None;", field.name));
                    }
                    let (data_name, expr_is_ref) =
                        if field.optional && field.rust_type.config.default.is_none() {
                            (String::from("field"), true)
                        } else {
                            (format!("self.{}", field.name), false)
                        };

                    let key = field.key.clone().unwrap();
                    // deserialize key + value
                    let deser_block = build_map_field_deser_arm(
                        gen_scope,
                        types,
                        name,
                        field,
                        field_index,
                        &key,
                        in_embedded,
                        &mut deser_code,
                        cli,
                    );

                    // serialize key
                    let mut map_ser_content = BlocksOrLines::default();
                    // `for_field`, NOT `new(..)`: this one config serves both the key write (via
                    // `key_encoding_var` just below) and the VALUE serialize further down, and the
                    // value is a record field — so it must carry the field's `@custom_serialize`
                    // exactly like the array-rep sites do, or a map-rep field's custom writer is
                    // silently dropped while `DeserializeConfig::for_field` still honors the custom
                    // READER (a round-trip-breaking asymmetry). `for_field` is `new(..)` plus that
                    // carry and nothing else, so the key side is untouched: `encoding_var` reads only
                    // `var_name`/`encoding_var_in_option_struct`, and the key write never consults
                    // `custom_serialize`.
                    let serialize_config = SerializeConfig::for_field(&data_name, field)
                        .expr_is_ref(expr_is_ref)
                        .encoding_var_in_option_struct("self.encodings");
                    let key_encoding_var =
                        serialize_config.encoding_var(Some("key"), key.encoding_var_is_copy(types));
                    match &key {
                        FixedValue::Uint(x) => {
                            let expr = format!("{x}u64");
                            write_using_sz(
                                &mut map_ser_content,
                                "write_unsigned_integer",
                                "serializer",
                                &expr,
                                &expr,
                                "?;",
                                &key_encoding_var,
                                cli,
                            );
                            uint_field_deserializers.push(deser_block);
                        }
                        FixedValue::Text(s) => {
                            write_string_sz(
                                &mut map_ser_content,
                                "write_text",
                                "serializer",
                                &format!("\"{}\"", escape_rust_str(s)),
                                true,
                                "?;",
                                &key_encoding_var,
                                cli,
                            );
                            text_field_deserializers.push(deser_block);
                        }
                        _ => panic!(
                            "unsupported map key type for {}.{}: {:?}",
                            name, field.name, key
                        ),
                    };

                    // serialize value
                    gen_scope.generate_serialize(
                        types,
                        (&field.rust_type).into(),
                        &mut map_ser_content,
                        serialize_config,
                        cli,
                    );
                    ser_content.push((field_index, field, map_ser_content));
                }
                if cli.preserve_encodings {
                    let rest_index_base = record.fields.len();
                    if record.is_open_table() {
                        emit_open_table_preserve_order(
                            gen_scope,
                            types,
                            record,
                            &mut ser_func,
                            cli,
                        );
                    } else if let Some(rest) = &record.rest {
                        // OPEN struct: the wire-position index space is `0..N` (declared fields) then
                        // `N + i` (i-th rest entry). The self-heal replay uses `orig_deser_order` when
                        // its length matches the present-entry count (`definite_info`, which folds
                        // `rest.len()`), else falls back to declaration order + rest appended. Under
                        // --canonical an open struct's canonical order depends on RUNTIME rest keys, so
                        // it cannot be a baked `vec![..]`: build it at runtime by serializing every
                        // present entry's key canonically and sorting length-first via the shared
                        // comparator (declared keys' bytes are codegen-time constants).
                        // `definite_info` is `u64` (the map header wants `u64`, and it folds
                        // `self.rest.len() as u64`), so cast the `usize` order length to compare.
                        let orig_or_fallback = format!(
                            "self.encodings.as_ref().filter(|encs| encs.orig_deser_order.len() as u64 == {}).map(|encs| encs.orig_deser_order.clone()).unwrap_or_else(|| (0..{} + self.{}.len()).collect::<Vec<usize>>())",
                            record.definite_info("self", false, types, cli),
                            rest_index_base,
                            rest.field_name
                        );
                        if cli.canonical_form {
                            let mut merge = Block::new("let deser_order = if force_canonical");
                            merge.line("let mut key_order: Vec<(Vec<u8>, usize)> = Vec::new();");
                            for (decl_index, field) in record.fields.iter().enumerate() {
                                let key_bytes = field.key.as_ref().unwrap().to_bytes();
                                let push = format!(
                                    "key_order.push(({}, {}));",
                                    byte_vec_literal(&key_bytes),
                                    decl_index
                                );
                                match rest_merge_present_condition(field) {
                                    Some(cond) => {
                                        let mut b = Block::new(format!("if {cond}"));
                                        b.line(push);
                                        merge.push_block(b);
                                    }
                                    None => {
                                        merge.line(push);
                                    }
                                }
                            }
                            let mut rest_key_loop = Block::new(format!(
                                "for (i, (rest_key, _)) in self.{}.iter().enumerate()",
                                rest.field_name
                            ));
                            rest_key_loop
                                .line("let mut buf = cbor_event::se::Serializer::new_vec();");
                            // A concrete key's serialize references its encoding var; under
                            // force_canonical the write is minimal regardless, so bind the defaults
                            // (self-carried `any` keys need none).
                            let (merge_key_encs, _) = rest_encoding_fields(types, rest, cli);
                            for enc in &merge_key_encs {
                                rest_key_loop.line(format!(
                                    "let {} = {};",
                                    enc.field_name, enc.default_expr
                                ));
                            }
                            let merge_key_config = SerializeConfig::new(
                                "rest_key",
                                format!("{}_key", rest.field_name),
                            )
                            .expr_is_ref(true)
                            .is_end(false)
                            .serializer_name_overload(("buf", true))
                            .encoding_var_is_ref(false);
                            // No sidecar lookup here: under force_canonical the key is written minimal
                            // regardless, so the merge's sort key matches the bytes the rest arm writes.
                            gen_scope.generate_serialize(
                                types,
                                (rest.domain()).into(),
                                &mut rest_key_loop,
                                merge_key_config,
                                cli,
                            );
                            rest_key_loop.line(format!(
                                "key_order.push((buf.finalize(), {rest_index_base} + i));"
                            ));
                            merge.push_block(rest_key_loop);
                            merge.line(
                                "key_order.sort_by(|(lhs, _), (rhs, _)| cbor_canonical_key_cmp(lhs, rhs));",
                            );
                            merge.line(
                                "key_order.into_iter().map(|(_, idx)| idx).collect::<Vec<usize>>()",
                            );
                            merge.after(format!(" else {{ {orig_or_fallback} }};"));
                            ser_func.push_block(merge);
                        } else {
                            ser_func.line(format!("let deser_order = {orig_or_fallback};"));
                        }
                        // `OrderedHashMap`/`PairMap` deref to backing types with no positional `get`,
                        // so materialize the entries once for the `N + i` index lookup in the replay.
                        ser_func.line(format!(
                            "let rest_entries: Vec<_> = self.{}.iter().collect();",
                            rest.field_name
                        ));
                    } else {
                        let (check_canonical, serialization_order) = if cli.canonical_form {
                            let indices_str = record
                                .canonical_ordering()
                                .iter()
                                .map(|(i, _)| i.to_string())
                                .collect::<Vec<String>>()
                                .join(",");
                            ("!force_canonical && ", format!("vec![{indices_str}]"))
                        } else {
                            ("", format!("(0..{}).collect()", ser_content.len()))
                        };
                        ser_func.line(format!(
                        "let deser_order = self.encodings.as_ref().filter(|encs| {}encs.orig_deser_order.len() == {}).map(|encs| encs.orig_deser_order.clone()).unwrap_or_else(|| {});",
                        check_canonical,
                        record.definite_info("self", false, types, cli),
                        serialization_order));
                    }
                    let mut ser_loop = Block::new("for field_index in deser_order");
                    // An OPEN TABLE's slots are TAGGED by source sequence (low bit), not a flat
                    // index space, so its replay scrutinizes the tag rather than the raw index.
                    let mut ser_loop_match = if record.is_open_table() {
                        Block::new("match field_index % 2")
                    } else {
                        Block::new("match field_index")
                    };
                    for (field_index, field, content) in ser_content.into_iter() {
                        // TODO: while this would be nice we would need to either:
                        // 1) know this before we call gen_scope.generate_serialize() OR
                        // 2) strip that !is_end (?;) field from it which seems brittle
                        //if let Some(single_line) = content.as_single_line() {
                        //    ser_loop_match.line(format!("{} => {},"));
                        //} else {
                        //}
                        let mut field_ser_block = if field.optional
                            && field.rust_type.is_fixed_value()
                        {
                            // optional fixed value: the `bool` presence field guards the write
                            Block::new(format!("{} => if self.{}", field_index, field.name))
                        } else if field.optional && field.rust_type.config.default.is_none() {
                            Block::new(format!(
                                "{} => if let Some(field) = &self.{}",
                                field_index, field.name
                            ))
                        } else if field.optional {
                            // defaulted optional: the map HEADER (definite_info) counts this field
                            // only when it differs from its default (or was explicitly present on
                            // deserialize) — the write arm must apply the IDENTICAL condition or a
                            // freshly-constructed default-valued field serializes a body entry the
                            // header didn't count (corrupt CBOR: length mismatch / trailing data)
                            let default_value = field.rust_type.config.default.as_ref().unwrap();
                            Block::new(format!(
                                "{} => if self.{} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false)",
                                field_index,
                                field.name,
                                default_value.to_primitive_str_compare(),
                                field.name
                            ))
                        } else {
                            Block::new(format!("{field_index} =>"))
                        };
                        field_ser_block.push_all(content);
                        ser_loop_match.push_block(field_ser_block);
                    }
                    if record.is_open_table() {
                        emit_open_table_replay(gen_scope, types, record, &mut ser_loop_match, cli);
                    } else if let Some(rest) = &record.rest {
                        // OPEN struct rest arm: index `>= N` selects the (index - N)-th rest entry.
                        // `.get()` returns `Option`, so a stale `orig_deser_order` (a user mutated
                        // `rest` after deserialize, shifting the count) SKIPS rather than panics —
                        // serialize's never-panic philosophy. `rest_i` is that positional index — the
                        // `Vec` sidecar lookup key for the `@duplicates preserve` twin (whose keys
                        // repeat); the loose container keys its sidecar by the key VALUE (`"key"`).
                        let mut rest_arm = Block::new("_ =>");
                        rest_arm.line(format!("let rest_i = field_index - {rest_index_base};"));
                        let mut got =
                            Block::new("if let Some(&(key, value)) = rest_entries.get(rest_i)");
                        let enc_lookup_var = if rest_is_pair_map(rest) {
                            "rest_i"
                        } else {
                            "key"
                        };
                        emit_rest_entry_serialize(
                            gen_scope,
                            types,
                            rest,
                            enc_lookup_var,
                            &mut got,
                            cli,
                        );
                        rest_arm.push_block(got);
                        ser_loop_match.push_block(rest_arm);
                        ser_loop_match.after(";");
                    } else {
                        ser_loop_match.line("_ => unreachable!()").after(";");
                    }
                    ser_loop.push_block(ser_loop_match);
                    ser_func.push_block(ser_loop);
                } else {
                    for (_field_index, field, content) in ser_content.into_iter() {
                        if field.optional {
                            let optional_ser_field_check = if field.rust_type.is_fixed_value() {
                                // optional fixed value: the `bool` presence field guards the write
                                format!("if self.{}", field.name)
                            } else if let Some(default_value) = &field.rust_type.config.default {
                                format!(
                                    "if self.{} != {}",
                                    field.name,
                                    default_value.to_primitive_str_compare()
                                )
                            } else {
                                format!("if let Some(field) = &self.{}", field.name)
                            };
                            let mut optional_ser_field = Block::new(optional_ser_field_check);
                            optional_ser_field.push_all(content);
                            ser_func.push_block(optional_ser_field);
                        } else {
                            ser_func.push_all(content);
                        }
                    }
                }
                // Open struct-map rest row, NON-preserve: after the declared fields, write each
                // captured entry as a bare key/value pair into the owner's map (the map header
                // already counted them via `definite_info`'s `+ self.rest.len()`). `BTreeMap`
                // iteration order (by key) drives the order; no encoding sidecars. The PRESERVE
                // flavor interleaves the rest entries into the `orig_deser_order` replay above (wire
                // position fidelity + per-entry sidecars), so it does NOT take this appended-loop path.
                // Capture-only: an `@ignore` struct re-serializes ONLY its declared members (the whole
                // point of the tolerate-and-drop flavor), and it has no `rest` field to iterate.
                for rest in record
                    .captured_dynamic_rows()
                    .filter(|_| !cli.preserve_encodings)
                {
                    let mut rest_loop = Block::new(format!(
                        "for (key, value) in self.{}.iter()",
                        rest.field_name
                    ));
                    let key_config = SerializeConfig::new("key", "rest_key")
                        .expr_is_ref(true)
                        .is_end(false);
                    gen_scope.generate_serialize(
                        types,
                        (rest.domain()).into(),
                        &mut rest_loop,
                        key_config,
                        cli,
                    );
                    let value_config = SerializeConfig::new("value", "rest_value")
                        .expr_is_ref(true)
                        .is_end(false);
                    gen_scope.generate_serialize(
                        types,
                        (rest.range()).into(),
                        &mut rest_loop,
                        value_config,
                        cli,
                    );
                    ser_func.push_block(rest_loop);
                }
                // Open struct-map (loose CBOR): declare the rest capture container (+ preserve
                // encoding sidecars) and fold the unknown-key match arms into captures below.
                // `record.rest` is `None` for every closed struct (byte-identical output).
                // An OPEN TABLE's two dynamic rows tag their slots by source sequence; every other
                // shape has one dynamic sequence after the `0..N` declared fields.
                let slots = if record.is_open_table() {
                    OrderSlots::Tagged { odd: true }
                } else {
                    OrderSlots::AfterDeclared(record.fields.len())
                };
                let rest_domain = record.rest.as_ref().map(|r| RestKeyDomain::of(types, r));
                let rest_is_typed = rest_domain == Some(RestKeyDomain::Typed);
                let any_cbor = format!("{}::any_cbor::AnyCbor", cli.common_import_rust());
                // BOTH flavors run unknown-key arms that account each entry via `read_len.read_elems`
                // and use `?`, so the loop needs the real (not `_`-prefixed) `read_len` and a
                // Result-returning closure — mark them used regardless of capture/ignore.
                if record.rest.is_some() || record.typed_row.is_some() {
                    deser_code.read_len_used = true;
                    deser_code.throws = true;
                }
                // CAPTURE only: declare the capture container (+ preserve encoding sidecar locals) the
                // arms insert into. An `@ignore` row deserializes-and-DROPS each unknown entry in
                // place, so there is no container and nothing to declare here.
                for rest in record.captured_dynamic_rows() {
                    if cli.preserve_encodings {
                        // Annotate the container type under preserve: for an `any`-domain rest the
                        // value-`Eq` dup scan (`.iter().any(|(k, _)| k.value_eq(..))`) runs BEFORE the
                        // first `insert`, so inference has nothing to pin `K`/`V` from otherwise. The
                        // non-preserve path (below) infers from its `insert`-based dup check, so it
                        // keeps the un-annotated form (byte-identical to the non-preserve output).
                        deser_code.content.line(&format!(
                            "let mut {}: {} = {}::new();",
                            rest.field_name,
                            rest_member_type(rest).for_rust_member(types, false, cli),
                            rest_container_ctor(rest, cli)
                        ));
                    } else {
                        deser_code.content.line(&format!(
                            "let mut {} = {}::new();",
                            rest.field_name,
                            rest_container_ctor(rest, cli)
                        ));
                    }
                    if cli.preserve_encodings {
                        // Sidecar locals mirror the encoding-struct shape: `Vec` for the pair-map
                        // twin (positional), `BTreeMap` for the loose container (keyed by key value).
                        let sidecar_ctor = if rest_is_pair_map(rest) {
                            "Vec::new()"
                        } else {
                            "BTreeMap::new()"
                        };
                        let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
                        if !key_encs.is_empty() {
                            deser_code.content.line(&format!(
                                "let mut {}_key_encodings = {sidecar_ctor};",
                                rest.field_name
                            ));
                        }
                        if !value_encs.is_empty() {
                            deser_code.content.line(&format!(
                                "let mut {}_value_encodings = {sidecar_ctor};",
                                rest.field_name
                            ));
                        }
                    }
                }
                // needs to be in one line rather than a block because Block::after() only takes a string
                deser_code.content.line("let mut read = 0;");
                // the loop condition and the Special-key arm below both read the bare `len`,
                // which in a plain group is the embedded-group param
                deser_code.len_used = true;
                let mut deser_loop = make_deser_loop("len", "read", cli);
                // TYPED rest key, seek anchor. A declared-key match arm reads the key to dispatch on
                // it; when the value falls through to the catch-all the key belongs to the REST row
                // and must be re-read by `K::deserialize`, so the arm rewinds to here first. One
                // hoisted line serves every arm (the alternative — bracing each arm's scrutinee —
                // would restructure four match headers), and the `cbor_event` `Deserializer` is
                // `(data, offset)` with no buffered lookahead, so `set_position` restores it exactly.
                //
                // Emitted only when a declared arm could consume the key: with no declared uint/text
                // keys nothing is ever read before the capture, and an unused binding would warn in
                // the generated crate.
                let rest_needs_seek_anchor = rest_is_typed
                    && (!uint_field_deserializers.is_empty()
                        || !text_field_deserializers.is_empty());
                if rest_needs_seek_anchor {
                    deser_loop.line("let initial_position = raw.position();");
                }
                let mut type_match = Block::new("match raw.cbor_type()?");
                // An OPEN TABLE dispatches the two rows by wire major: the typed row claims exactly
                // its key's single statically-known major, the catch-all sees the complement. With
                // zero declared fields there is no declared-key arm to compose with, so the whole
                // match is purpose-built rather than threaded through the open-struct-map arms.
                if record.is_open_table() {
                    append_open_table_dispatch(gen_scope, types, record, &mut type_match, cli);
                } else {
                    // The uint key the record loop already read (`unknown_key`, a u64) is the capture key
                    // for a `uint`-domain rest (used directly, its `key_enc` going to the key sidecar) or
                    // reconstructed into `AnyCbor` for an `any`-domain rest — under preserve carrying the
                    // peeked wire width `Sz` so byte-exactness holds. A `text`-domain rest does not accept
                    // a uint key (stays an error).
                    let uint_rest_key: Option<String> = match rest_domain {
                        Some(RestKeyDomain::Uint) => Some("unknown_key".to_owned()),
                        Some(RestKeyDomain::Any) if cli.preserve_encodings => {
                            Some(format!("{any_cbor}::UInt(unknown_key, Some(key_enc))"))
                        }
                        Some(RestKeyDomain::Any) => {
                            Some(format!("{any_cbor}::new_uint(unknown_key)"))
                        }
                        _ => None,
                    };
                    // The concrete uint key's peeked encoding var, threaded to the key sidecar (None for a
                    // self-carried `any` key).
                    let uint_key_enc: Option<String> =
                        if cli.preserve_encodings && rest_domain == Some(RestKeyDomain::Uint) {
                            Some("key_enc".to_owned())
                        } else {
                            None
                        };
                    if uint_field_deserializers.is_empty() {
                        if let (Some(rest), true) = (&record.rest, rest_is_typed) {
                            // TYPED key, no declared uint keys: nothing has been read, so there is nothing
                            // to reconstruct AND nothing to rewind — `K::deserialize` starts on the key's
                            // first byte.
                            let mut arm = Block::new("cbor_event::Type::UnsignedInteger =>");
                            append_rest_capture(
                                gen_scope, types, rest, slots, None, None, &mut arm, cli,
                            );
                            arm.after(",");
                            type_match.push_block(arm);
                        } else if let (Some(rest), Some(key_expr)) =
                            (&record.rest, uint_rest_key.clone())
                        {
                            let mut arm = Block::new("cbor_event::Type::UnsignedInteger =>");
                            if cli.preserve_encodings {
                                arm.line(
                                    "let (unknown_key, key_enc) = raw.unsigned_integer_sz()?;",
                                );
                            } else {
                                arm.line("let unknown_key = raw.unsigned_integer()?;");
                            }
                            append_rest_capture(
                                gen_scope,
                                types,
                                rest,
                                slots,
                                Some(key_expr),
                                uint_key_enc.clone(),
                                &mut arm,
                                cli,
                            );
                            arm.after(",");
                            type_match.push_block(arm);
                        } else {
                            type_match.line("cbor_event::Type::UnsignedInteger => return Err(DeserializeFailure::UnknownKey(Key::Uint(raw.unsigned_integer()?)).into()),");
                        }
                    } else {
                        let mut uint_match = if cli.preserve_encodings {
                            Block::new(
                                "cbor_event::Type::UnsignedInteger => match raw.unsigned_integer_sz()?",
                            )
                        } else {
                            Block::new(
                                "cbor_event::Type::UnsignedInteger => match raw.unsigned_integer()?",
                            )
                        };
                        for case in uint_field_deserializers {
                            uint_match.push_block(case);
                        }
                        if let (Some(rest), true) = (&record.rest, rest_is_typed) {
                            // TYPED key past the declared arms: the scrutinee read consumed the key's
                            // bytes, so rewind to the loop-body anchor and let `K::deserialize` read them
                            // for itself. A wildcard binds nothing, so neither the u64 nor its `Sz` is
                            // left dead.
                            let mut arm = Block::new("_ =>");
                            arm.line("raw.set_position(initial_position).unwrap();");
                            append_rest_capture(
                                gen_scope, types, rest, slots, None, None, &mut arm, cli,
                            );
                            arm.after(",");
                            uint_match.push_block(arm);
                        } else if let (Some(rest), Some(key_expr)) =
                            (&record.rest, uint_rest_key.clone())
                        {
                            // Under preserve the scrutinee is `(u64, Sz)`, so bind both; else just the u64.
                            let mut arm = if cli.preserve_encodings {
                                Block::new("(unknown_key, key_enc) =>")
                            } else {
                                Block::new("unknown_key =>")
                            };
                            append_rest_capture(
                                gen_scope,
                                types,
                                rest,
                                slots,
                                Some(key_expr),
                                uint_key_enc.clone(),
                                &mut arm,
                                cli,
                            );
                            arm.after(",");
                            uint_match.push_block(arm);
                        } else {
                            let unknown_key_decl = if cli.preserve_encodings {
                                "(unknown_key, _enc)"
                            } else {
                                "unknown_key"
                            };
                            uint_match.line(format!("{unknown_key_decl} => return Err(DeserializeFailure::UnknownKey(Key::Uint(unknown_key)).into()),"));
                        }
                        uint_match.after(",");
                        type_match.push_block(uint_match);
                    }
                    // we can't map text_sz() with String::as_str() to match it since that would return a reference to a temporary
                    // so we need to store it in a local and have an extra block to declare it
                    // `text_rest_key_ref`: `unknown_key`/`text_key` is `&str` (the non-empty match-arm form);
                    // `text_rest_key_owned`: `unknown_key` is `String` (the empty-arm form).
                    let text_key_enc: Option<String> =
                        if cli.preserve_encodings && rest_domain == Some(RestKeyDomain::Text) {
                            Some("key_enc".to_owned())
                        } else {
                            None
                        };
                    // For the non-empty match arm the matched binding is `&str` — `.to_owned()` it.
                    let text_rest_key_ref: Option<String> = match rest_domain {
                        Some(RestKeyDomain::Text) => Some("unknown_key.to_owned()".to_owned()),
                        Some(RestKeyDomain::Any) if cli.preserve_encodings => Some(format!(
                            "{any_cbor}::Text(unknown_key.to_owned(), StringEncoding::from(key_enc))"
                        )),
                        Some(RestKeyDomain::Any) => {
                            Some(format!("{any_cbor}::new_text(unknown_key.to_owned())"))
                        }
                        _ => None,
                    };
                    // For the empty arm the binding `unknown_key` is an owned `String`.
                    let text_rest_key_owned: Option<String> = match rest_domain {
                        Some(RestKeyDomain::Text) => Some("unknown_key".to_owned()),
                        Some(RestKeyDomain::Any) if cli.preserve_encodings => Some(format!(
                            "{any_cbor}::Text(unknown_key, StringEncoding::from(key_enc))"
                        )),
                        Some(RestKeyDomain::Any) => {
                            Some(format!("{any_cbor}::new_text(unknown_key)"))
                        }
                        _ => None,
                    };
                    if text_field_deserializers.is_empty() {
                        if let (Some(rest), true) = (&record.rest, rest_is_typed) {
                            // TYPED key, no declared text keys — the uint-arm twin: nothing read, nothing
                            // to rewind.
                            let mut arm = Block::new("cbor_event::Type::Text =>");
                            append_rest_capture(
                                gen_scope, types, rest, slots, None, None, &mut arm, cli,
                            );
                            arm.after(",");
                            type_match.push_block(arm);
                        } else if let (Some(rest), Some(key_expr)) =
                            (&record.rest, text_rest_key_owned.clone())
                        {
                            let mut arm = Block::new("cbor_event::Type::Text =>");
                            if cli.preserve_encodings {
                                arm.line("let (unknown_key, key_enc) = raw.text_sz()?;");
                            } else {
                                arm.line("let unknown_key = raw.text()?;");
                            }
                            append_rest_capture(
                                gen_scope,
                                types,
                                rest,
                                slots,
                                Some(key_expr),
                                text_key_enc.clone(),
                                &mut arm,
                                cli,
                            );
                            arm.after(",");
                            type_match.push_block(arm);
                        } else {
                            type_match.line("cbor_event::Type::Text => return Err(DeserializeFailure::UnknownKey(Key::Str(raw.text()?)).into()),");
                        }
                    } else if cli.preserve_encodings {
                        let mut outer_match = Block::new("cbor_event::Type::Text =>");
                        outer_match.line("let (text_key, key_enc) = raw.text_sz()?;");
                        let mut text_match = Block::new("match text_key.as_str()");
                        for case in text_field_deserializers {
                            text_match.push_block(case);
                        }
                        if let (Some(rest), true) = (&record.rest, rest_is_typed) {
                            // TYPED key past the declared arms: rewind and re-read. `text_key`/`key_enc`
                            // stay live (the declared arms use them), so only this arm's own binding is
                            // dropped.
                            let mut arm = Block::new("_ =>");
                            arm.line("raw.set_position(initial_position).unwrap();");
                            append_rest_capture(
                                gen_scope, types, rest, slots, None, None, &mut arm, cli,
                            );
                            arm.after(",");
                            text_match.push_block(arm);
                        } else if let (Some(rest), Some(key_expr)) =
                            (&record.rest, text_rest_key_ref.clone())
                        {
                            // capture arm: `unknown_key` (`&str`) shadows via the match binding; `key_enc`
                            // and `text_key` (owned) are the outer `text_sz()` reads.
                            let mut arm = Block::new("unknown_key =>");
                            append_rest_capture(
                                gen_scope,
                                types,
                                rest,
                                slots,
                                Some(key_expr),
                                text_key_enc.clone(),
                                &mut arm,
                                cli,
                            );
                            arm.after(",");
                            text_match.push_block(arm);
                        } else {
                            text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
                        }
                        outer_match.after(",");
                        outer_match.push_block(text_match);
                        type_match.push_block(outer_match);
                    } else if let (Some(rest), true) = (&record.rest, rest_is_typed) {
                        // TYPED key, plain flavor, declared text keys present. This is the ONE arm that
                        // also restructures its header: `match raw.text()?.as_str()` extends the `String`
                        // temporary — and the `&mut raw` autoref that produced it — across the whole
                        // match, so a `raw.set_position(..)` inside an arm would contend with it. Lifting
                        // the read into a `let` (the shape the preserve flavor already emits) ends that
                        // borrow before the arms run. Gated on the typed path, so the plain form stays
                        // byte-identical everywhere else.
                        let mut outer_match = Block::new("cbor_event::Type::Text =>");
                        outer_match.line("let text_key = raw.text()?;");
                        let mut text_match = Block::new("match text_key.as_str()");
                        for case in text_field_deserializers {
                            text_match.push_block(case);
                        }
                        let mut arm = Block::new("_ =>");
                        arm.line("raw.set_position(initial_position).unwrap();");
                        append_rest_capture(
                            gen_scope, types, rest, slots, None, None, &mut arm, cli,
                        );
                        arm.after(",");
                        text_match.push_block(arm);
                        outer_match.after(",");
                        outer_match.push_block(text_match);
                        type_match.push_block(outer_match);
                    } else {
                        let mut text_match =
                            Block::new("cbor_event::Type::Text => match raw.text()?.as_str()");
                        for case in text_field_deserializers {
                            text_match.push_block(case);
                        }
                        if let (Some(rest), Some(key_expr)) =
                            (&record.rest, text_rest_key_ref.clone())
                        {
                            let mut arm = Block::new("unknown_key =>");
                            append_rest_capture(
                                gen_scope,
                                types,
                                rest,
                                slots,
                                Some(key_expr),
                                None,
                                &mut arm,
                                cli,
                            );
                            arm.after(",");
                            text_match.push_block(arm);
                        } else {
                            text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
                        }
                        text_match.after(",");
                        type_match.push_block(text_match);
                    }
                    if let (Some(rest), true) = (
                        &record.rest,
                        matches!(rest_domain, Some(RestKeyDomain::Any | RestKeyDomain::Typed)),
                    ) {
                        // any-domain or TYPED rest: a Special is either the break ending an indefinite map
                        // or a special-typed KEY (bool/null/undefined/float/unassigned) to capture.
                        // `special_break()` advances ONLY on a true break, so a non-break special is left
                        // intact for the key deserialize; both break match arms diverge (return/break), so
                        // control only falls through to the capture when it was NOT a break. A typed `K`
                        // that admits no special value simply errors out of its own deserialize — that is
                        // refinement, not a case to special-case (and a null-admitting `K`, the one shape
                        // where the two readings genuinely collide, is rejected at recognition).
                        let mut special_arm = Block::new("cbor_event::Type::Special =>");
                        let mut is_break = Block::new("if raw.special_break()?");
                        let mut break_len = Block::new("match len");
                        break_len.line(format!(
                            "{} => return Err(DeserializeFailure::BreakInDefiniteLen.into()),",
                            cbor_event_len_n("_", cli)
                        ));
                        break_len.line(format!("{} => break,", cbor_event_len_indef(cli)));
                        is_break.push_block(break_len);
                        special_arm.push_block(is_break);
                        append_rest_capture(
                            gen_scope,
                            types,
                            rest,
                            slots,
                            None,
                            None,
                            &mut special_arm,
                            cli,
                        );
                        special_arm.after(",");
                        type_match.push_block(special_arm);
                    } else {
                        let mut special_match =
                            Block::new("cbor_event::Type::Special => match len");
                        special_match.line(format!(
                            "{} => return Err(DeserializeFailure::BreakInDefiniteLen.into()),",
                            cbor_event_len_n("_", cli)
                        ));
                        // TODO: this will need to change if we support Special values as keys (e.g. true / false)
                        let mut break_check = Block::new(format!(
                            "{} => match raw.special()?",
                            cbor_event_len_indef(cli)
                        ));
                        break_check.line("cbor_event::Special::Break => break,");
                        break_check.line(
                            "_ => return Err(DeserializeFailure::EndingBreakMissing.into()),",
                        );
                        break_check.after(",");
                        special_match.push_block(break_check);
                        special_match.after(",");
                        type_match.push_block(special_match);
                    }
                    if let (Some(rest), true) = (
                        &record.rest,
                        matches!(rest_domain, Some(RestKeyDomain::Any | RestKeyDomain::Typed)),
                    ) {
                        // any-domain or TYPED rest: bytes/negative-int/array/map/tag keys land here;
                        // deserialize the key straight from `raw` (uint/text/special are handled by the
                        // arms above). Nothing was consumed to reach this arm, so no rewind is needed.
                        let mut arm = Block::new("_ =>");
                        append_rest_capture(
                            gen_scope, types, rest, slots, None, None, &mut arm, cli,
                        );
                        arm.after(",");
                        type_match.push_block(arm);
                    } else {
                        type_match.line("other_type => return Err(DeserializeFailure::UnexpectedKeyType(other_type).into()),");
                    }
                }
                deser_loop.push_block(type_match);
                deser_loop.line("read += 1;");
                deser_code.content.push_block(deser_loop);
                // The NonEmpty open table's min-1 bound (`{ + K_t => V_t, * K_r => V_r }`), enforced
                // where the mandatory-field checks below are: after the loop, on the capture LOCAL,
                // before it moves into the struct. It counts TYPED entries only — a map of purely
                // captured entries is not a non-empty table — and raises the very error
                // `NonEmptyMap`'s `TryFrom` door raises, so the wire door and every API door report
                // the bound identically.
                if let Some(typed) = record.typed_row().filter(|t| t.non_empty) {
                    let mut min_one = Block::new(format!("if {}.is_empty()", typed.field_name));
                    min_one.line(
                        "return Err(DeserializeFailure::RangeCheck { found: 0, min: Some(1), max: None }.into());",
                    );
                    deser_code.content.push_block(min_one);
                }
                let mut ctor_block = Block::new("Ok(Self");
                // make sure the field is present, and unwrap the Option<T>
                for field in &record.fields {
                    if !field.optional {
                        let key = match &field.key {
                            Some(FixedValue::Uint(x)) => format!("Key::Uint({x})"),
                            Some(FixedValue::Text(x)) => {
                                format!("Key::Str(String::from(\"{}\"))", escape_rust_str(x))
                            }
                            None => unreachable!(),
                            _ => unimplemented!(),
                        };
                        if field.rust_type.is_fixed_value() {
                            let mut mandatory_field_check =
                                Block::new(format!("if !{}_present", field.name));
                            mandatory_field_check.line(format!(
                            "return Err(DeserializeFailure::MandatoryFieldMissing({key}).into());"
                        ));
                            deser_code.content.push_block(mandatory_field_check);
                        } else {
                            let mut mandatory_field_check =
                                Block::new(format!("let {} = match {}", field.name, field.name));
                            mandatory_field_check.line("Some(x) => x,");

                            mandatory_field_check.line(format!("None => return Err(DeserializeFailure::MandatoryFieldMissing({key}).into()),"));
                            mandatory_field_check.after(";");
                            deser_code.content.push_block(mandatory_field_check);
                        }
                    } else if let Some(default_value) = &field.rust_type.config.default {
                        if cli.preserve_encodings {
                            let mut default_present_check = Block::new(format!(
                                "if {} == Some({})",
                                field.name,
                                default_value.to_primitive_str_assign()
                            ));
                            default_present_check
                                .line(format!("{}_default_present = true;", field.name));
                            deser_code.content.push_block(default_present_check);
                        }
                        match default_value {
                            FixedValue::Text(_) | FixedValue::Bytes(_) => {
                                // to avoid clippy::or_fun_call
                                deser_code.content.line(&format!(
                                    "let {} = {}.unwrap_or_else(|| {});",
                                    field.name,
                                    field.name,
                                    default_value.to_primitive_str_assign()
                                ));
                            }
                            FixedValue::Bool(_)
                            | FixedValue::Nint(_)
                            | FixedValue::Null
                            | FixedValue::Undefined
                            | FixedValue::Float(_)
                            | FixedValue::Uint(_) => {
                                deser_code.content.line(&format!(
                                    "let {} = {}.unwrap_or({});",
                                    field.name,
                                    field.name,
                                    default_value.to_primitive_str_assign()
                                ));
                            }
                        }
                    }
                    if !field.rust_type.is_fixed_value() {
                        ctor_block.line(format!("{},", field.name));
                    } else if field.optional && field.rust_type.is_fixed_value() {
                        // optional fixed value -> the struct's `bool` presence field is the
                        // `{field}_present` flag (true iff the key was seen during the map loop)
                        ctor_block.line(format!("{}: {}_present,", field.name, field.name));
                    }
                }
                // Open struct-map rest row (CAPTURE only): the capture container local (declared
                // before the loop, named by the rest field) moves into the constructed struct. An
                // `@ignore` row declared no container and adds no field, so nothing moves in.
                for rest in record.captured_dynamic_rows() {
                    ctor_block.line(format!("{},", rest.field_name));
                }
                if cli.preserve_encodings {
                    let mut encoding_ctor = Block::new(format!("encodings: Some({name}Encoding"));
                    if tag.is_some() {
                        encoding_ctor.line("tag_encoding: Some(tag_encoding),");
                    }
                    encoding_ctor
                        .line("len_encoding,")
                        .line("orig_deser_order,");
                    for field in record.fields.iter() {
                        let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                        encoding_ctor.line(format!("{},", key_enc.field_name));
                        for field_enc in field_encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type,
                            Some(&field.rule_metadata),
                            true,
                            cli,
                        ) {
                            encoding_ctor.line(format!("{},", field_enc.field_name));
                        }
                    }
                    // Open struct-map rest row: the per-entry encoding sidecar locals (declared before
                    // the loop and populated at each concrete-domain capture) move into the encoding
                    // struct. Absent for a fully self-carried `any` rest. Capture-only (preserve).
                    for rest in record.captured_dynamic_rows() {
                        let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
                        if !key_encs.is_empty() {
                            encoding_ctor.line(format!("{}_key_encodings,", rest.field_name));
                        }
                        if !value_encs.is_empty() {
                            encoding_ctor.line(format!("{}_value_encodings,", rest.field_name));
                        }
                    }
                    encoding_ctor.after("),");
                    ctor_block.push_block(encoding_ctor);
                }
                ctor_block.after(")");
                ctor_block
            }
        };
        let len_enc_var = len_encoding_var
            .map(|var| {
                format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()")
            })
            .unwrap_or_default();
        match &mut ser_embedded_impl {
            Some(ser_embedded_impl) => {
                // Embedded (plain-group) serialize writes only the group's contents; the entity
                // that wrote the array/map head owns the ending break. Writing `.end()` here too
                // double-writes the break when the framing owner is indefinite-length (the break
                // is written once by the container / standalone serialize). Just return.
                ser_func.line("Ok(serializer)");
                ser_embedded_impl.push_fn(ser_func)
            }
            None => {
                end_len(&mut ser_func, "serializer", &len_enc_var, true, cli);
                ser_impl.push_fn(ser_func)
            }
        };
        let mut deser_scaffolding = BlocksOrLines::default();
        // the scaffolding lands inside the annotate closure only for non-embedded records (the
        // embedded/plain-group scaffolding stays in deserialize(), outside any closure)
        let scaffolding_annotated = cli.annotate_fields && !types.is_plain_group(name);
        let (mut deser_impl, mut deser_embedded_impl) = create_deserialize_impls(
            name,
            Some(record.rep),
            tag,
            Some(record.cbor_len_info(types)),
            types.is_plain_group(name),
            len_encoding_var,
            &mut deser_scaffolding,
            scaffolding_annotated,
            cli,
        );
        if deser_embedded_impl.is_none() {
            // ending checks are included with embedded serialization setup
            // since we are populating deserialize_as_embedded_group() and deserialize()
            // is already complete
            // but these checks must be done manually here *after* we populate deserialize()
            add_deserialize_final_len_check(
                &mut deser_code.content,
                Some(record.rep),
                record.cbor_len_info(types),
                cli,
            );
        }
        deser_code.content.push_block(ctor_block);

        if deser_embedded_impl.is_none() {
            // Non-embedded records: the container header + length reads (tag / map / array +
            // read_elems / finish, built into `deser_scaffolding` by create_deserialize_impls)
            // must sit INSIDE the annotate closure so wrong-major-type and wrong-length errors
            // carry the type name exactly like field-level errors already do. Prepend the
            // scaffolding ahead of the field-read code so the whole body is annotated as one unit.
            // (The embedded/plain-group case annotates its scaffolding differently:
            // create_deserialize_impls wraps the pre-delegation header reads and the post-delegation
            // final-len check each in their own annotate closure, keeping the delegated
            // deserialize_as_embedded_group() call OUTSIDE any closure — its body is already
            // annotated per-field, so wrapping the delegation would double-annotate field errors
            // ("Type.Type.field").)
            let mut body = std::mem::take(&mut deser_scaffolding);
            body.push_all(std::mem::take(&mut deser_code.content));
            deser_code.content = body;
        }

        if cli.annotate_fields {
            deser_code = deser_code.annotate(name.as_ref(), "", "");
        }

        if let Some(deser_embedded_impl) = &mut deser_embedded_impl {
            let mut deser_f = make_deserialization_function("deserialize", cli);
            deser_f.push_all(deser_scaffolding);
            deser_impl.push_fn(deser_f);
            let mut deser_embed_f =
                make_deserialization_function("deserialize_as_embedded_group", cli);
            let read_len_arg = if deser_code.read_len_used {
                "read_len"
            } else {
                "_read_len"
            };
            deser_embed_f.arg(read_len_arg, "&mut CBORReadLen");
            if cli.preserve_encodings {
                // always consumed by the `len_encoding` binding below
                deser_embed_f.arg("len", "cbor_event::LenSz");
            } else {
                let len_arg = if deser_code.len_used { "len" } else { "_len" };
                deser_embed_f.arg(len_arg, "cbor_event::Len");
            }
            // this is expected when creating the final struct but wouldn't have been available
            // otherwise as it is in the non-embedded deserialiation function
            if cli.preserve_encodings {
                deser_embed_f.line("let len_encoding = len.into();");
            }
            deser_embed_f.push_all(deser_code.content);
            deser_embedded_impl.push_fn(deser_embed_f);
        } else {
            // Non-embedded: `deser_scaffolding` was merged into `deser_code.content` above (inside
            // the annotate closure), so the whole deserialize() body is just the annotated code.
            let mut deser_f = make_deserialization_function("deserialize", cli);
            deser_f.push_all(deser_code.content);
            deser_impl.push_fn(deser_f);
        }

        if config.custom_serialize.is_none() {
            gen_scope.rust_serialize(types, name).push_impl(ser_impl);
            if let Some(s) = ser_embedded_impl {
                gen_scope.rust_serialize(types, name).push_impl(s);
            }
        }

        // TODO: generic deserialize (might need backtracking)
        if gen_scope.deserialize_generated(name) {
            gen_scope.rust_serialize(types, name).push_impl(deser_impl);
            if let Some(deser_embedded_impl) = deser_embedded_impl {
                gen_scope
                    .rust_serialize(types, name)
                    .push_impl(deser_embedded_impl);
            }
        }
    }

    gen_scope
        .rust(types, name)
        .push_struct(native_struct)
        .push_impl(native_impl);

    // for clippy we generate a Default when new takes no args.
    // We keep new() for consistency with other types.
    if new_arg_count == 0 {
        let mut default_impl = codegen::Impl::new(name.to_string());
        default_impl
            .impl_trait("Default")
            .new_fn("default")
            .ret("Self")
            .line("Self::new()");
        gen_scope.rust(types, name).push_impl(default_impl);
    }
}
