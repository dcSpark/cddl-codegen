use super::*;

/// Rustdoc attached to a generated open struct-map type whose rest row is `@ignore` (tolerate-and-drop)
/// and to its `serialize` fn. The drop is invisible at the API surface (there is no `rest` field), so a
/// consumer has no other signal that the type is deliberately lossy — state the contract at the point of
/// use.
const IGNORE_LOSSINESS_DOC: &str =
    "Open struct-map with an ignored rest row: tolerates unknown map entries on deserialize and DROPS \
     them, and re-serializes only the declared fields. Byte round-trips do NOT hold for wire data that \
     carried unknown entries.";

/// Combine a type's optional CDDL-derived doc with the `@ignore` lossiness breadcrumb (a blank line
/// between them when both are present), yielding the doc string to attach to the type / its serialize fn.
fn ignore_aware_doc(base: Option<&str>, is_ignore: bool) -> Option<String> {
    match (base, is_ignore) {
        (Some(d), true) => Some(format!("{d}\n\n{IGNORE_LOSSINESS_DOC}")),
        (Some(d), false) => Some(d.to_owned()),
        (None, true) => Some(IGNORE_LOSSINESS_DOC.to_owned()),
        (None, false) => None,
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
                            format!(
                                "if {} != {} || self.encodings.map(|encs| encs.{}_default_present).unwrap_or(false)",
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
pub(super) fn generate_array_struct_deserialization(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
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
            .then(|| encoding_var_names_str(types, &field.name, &field.rust_type, cli))
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
                // preserve bool/null: the deserialize emits a trailing `()` value expr; without a
                // terminating `;` the following statement fails to parse. (Non-preserve fixed
                // deserialize asserts an empty `after` — it emits no value — so keep it empty
                // there.)
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
            // we can support optional fields, but only when they're immediately non-ambiguous
            // i.e. when the next type (possibly skipping subsequent optional fields)
            // is different from the current type.
            // Supporting the general case 100% is extremely complicated without a combinatorial
            // backtrack but for most sane real-world cases this wouldn't be necessary.
            // Think purposefully written edge-cases with multiple optional fields, possibly nested
            // in other structs, and with many of the same types.
            // e.g. [ ? uint, uint, ? (uint, text), ? text]
            let field_cbor_types = field.rust_type.cbor_types(types);
            let mut possibly_last_field = true;
            for i in (field_index + 1)..record.fields.len() {
                if record.fields[i]
                    .rust_type
                    .cbor_types(types)
                    .iter()
                    .any(|ct| field_cbor_types.contains(ct))
                {
                    gen_scope.dont_generate_deserialize(
                        name,
                        format!(
                            "Array struct with potentially-ambiguous optional field {}: {:?}",
                            field.name, field.rust_type,
                        ),
                    );
                }
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
                    encoding_fields(
                        types,
                        &field.name,
                        &field.rust_type.clone().resolve_aliases(),
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
                        // encoding-less (bool/null): the deserialize emits its verify plus a
                        // trailing `()` value expr. Terminate it (`;`) so the appended `true`
                        // becomes the block's tail expression rather than a parse error.
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
                        encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
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
                    let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
                    gen_scope
                        .generate_deserialize(
                            types,
                            (&field.rust_type).into(),
                            DeserializeBeforeAfter::new("Some(", ")", false),
                            deser_config,
                            cli,
                        )
                        .wrap_in_block(type_check_block)
                        .add_to_code(&mut deser_code);
                    type_check_else.line("None");
                }
                type_check_else.after(after);
                deser_code.content.push_block(type_check_else);
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
            for field_enc in encoding_fields(
                types,
                &field.name,
                &field.rust_type.clone().resolve_aliases(),
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
/// Key domain of an open struct-map rest row, for plain-mode (non-preserve) capture. Recognition
/// (`recognize_rest_row`) restricts the domain to `uint` (u64) / `text` / `any`, so these three
/// exhaust it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RestKeyDomain {
    Uint,
    Text,
    Any,
}

/// `@duplicates preserve` on the rest row → the vec-of-pairs twin (`PairMap`), which accepts AND
/// re-emits duplicate keys in wire order (matching `@duplicates preserve` TABLES). Otherwise the
/// loose container (`OrderedHashMap`/`BTreeMap`) with the value-duplicate rejection (accept/reject
/// keyed on CBOR VALUE equality, not the domain's spelling).
fn rest_is_pair_map(rest: &RestRow) -> bool {
    rest.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
}

/// The rest container CONSTRUCTOR token (`PairMap` / `OrderedHashMap` / `BTreeMap`).
fn rest_container_ctor(rest: &RestRow, cli: &Cli) -> &'static str {
    if rest_is_pair_map(rest) {
        "PairMap"
    } else {
        table_type(cli)
    }
}

/// The rest field's member RUST TYPE (`PairMap<K, V>` / `OrderedHashMap<K, V>` / `BTreeMap<K, V>`).
/// A `@duplicates preserve` policy on the `Map` `RustType` routes `for_rust_member` to the pair-map
/// twin, reusing the table machinery.
fn rest_member_type(rest: &RestRow) -> crate::intermediate::RustType {
    let ty: crate::intermediate::RustType =
        ConceptualRustType::Map(Box::new(rest.domain.clone()), Box::new(rest.range.clone())).into();
    ty.with_duplicates_policy(rest.duplicates)
}

impl RestKeyDomain {
    fn of(rest: &RestRow) -> Self {
        match rest.domain.conceptual_type.resolve_alias_shallow() {
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
    let key_encs = encoding_fields(
        types,
        &format!("{}_key", rest.field_name),
        &rest.domain.clone().resolve_aliases(),
        false,
        cli,
    );
    let value_encs = encoding_fields(
        types,
        &format!("{}_value", rest.field_name),
        &rest.range.clone().resolve_aliases(),
        false,
        cli,
    );
    (key_encs, value_encs)
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
    // The `any`-domain key/value views live in `any_cbor`; the domain-agnostic flatten helpers live
    // in the `any`-free `open_struct_rest_json` module (so a fully-typed rest row needs no AnyCbor).
    let base = format!("{}::any_cbor", cli.common_import_rust());
    let flatten = format!("{}::open_struct_rest_json", cli.common_import_rust());
    let container_ty = rest_member_type(rest).for_rust_member(types, false, cli);
    let range_is_any = matches!(
        rest.range.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Any
    );
    // snake_case the owner name so the free fns are snake (no `non_snake_case` warning) and unique
    // (struct idents are unique; `convert_to_snake_case` is injective enough here as the field name
    // and fixed suffixes disambiguate).
    let owner_snake = crate::utils::convert_to_snake_case(name.as_ref());
    let ser_fn = format!("{}_{}_flatten_serialize", owner_snake, rest.field_name);
    let deser_fn = format!("{}_{}_flatten_deserialize", owner_snake, rest.field_name);

    let mut annotations = Vec::new();
    if cli.json_serde_derives {
        annotations.push("#[serde(flatten)]".to_owned());
        annotations.push(format!(
            "#[serde(serialize_with = \"{ser_fn}\", deserialize_with = \"{deser_fn}\")]"
        ));
    }
    if cli.json_schema_export {
        // The rest field's schema is the honest open-map `additionalProperties`: for an
        // `any` range it is the permissive "any JSON value" (json2ts → a `{ [k: string]: unknown }`
        // index signature intersected with the declared `properties`); for a TYPED range schemars'
        // native flatten-map handling already yields `additionalProperties: <range schema>`, so no
        // `schema_with` override is emitted there.
        if range_is_any {
            annotations.push(format!(
                "#[schemars(schema_with = \"{base}::natural_any_cbor_map_schema\")]"
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
    let (key_closure, key_coerce) = match RestKeyDomain::of(rest) {
        // A typed-domain key stringify never fails, so the error type is unconstrained by the body —
        // pin it (`Infallible: Display`) so the generic helper's `E: Display` bound resolves.
        RestKeyDomain::Uint => (
            "|k: &u64| Ok::<String, std::convert::Infallible>(k.to_string())".to_owned(),
            "let k = ks.parse::<u64>().map_err(|_| serde::de::Error::custom(\
             format!(\"open struct-map rest key {ks:?} is not a valid uint\")))?;"
                .to_owned(),
        ),
        RestKeyDomain::Text => (
            "|k: &String| Ok::<String, std::convert::Infallible>(k.clone())".to_owned(),
            "let k = ks;".to_owned(),
        ),
        RestKeyDomain::Any => (
            format!("{base}::any_cbor_natural_key_string"),
            format!("let k = {base}::any_cbor_natural_key_from_string(&ks);"),
        ),
    };
    // Per-range value view: an `any` range renders NATURALLY (the natural walk); a typed range uses its own
    // serde (which, if it transitively contains `any`, is itself steered by the natural adapters).
    let (value_wrap, value_de_ty, value_unwrap) = if range_is_any {
        (
            format!("{base}::NaturalAnyCborSer(v)"),
            format!("{base}::NaturalAnyCborDe"),
            "v.0".to_owned(),
        )
    } else {
        (
            "v".to_owned(),
            rest.range.for_rust_member(types, false, cli),
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
         \x20       {field}.iter().map(|(k, v)| (k, {value_wrap})),\n\
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
fn append_rest_capture(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    rest: &RestRow,
    rest_index_base: usize,
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
                        (&rest.domain).into(),
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
                (&rest.range).into(),
                DeserializeBeforeAfter::new("let _rest_value = ", ";", false),
                DeserializeConfig::new("_rest_value"),
                cli,
            )
            .add_to(block);
        return;
    }
    let domain = RestKeyDomain::of(rest);
    let (key_encs, value_encs) = rest_encoding_fields(types, rest, cli);
    // --- key ---
    match key_val_expr {
        Some(expr) => {
            block.line(format!("let rest_key = {expr};"));
        }
        None => {
            gen_scope
                .generate_deserialize(
                    types,
                    (&rest.domain).into(),
                    DeserializeBeforeAfter::new("let rest_key = ", ";", false),
                    DeserializeConfig::new("rest_key"),
                    cli,
                )
                .add_to(block);
        }
    }
    // --- value (with encoding capture under preserve for a concrete range) ---
    if cli.preserve_encodings && !value_encs.is_empty() {
        let var_names_str = encoding_var_names_str(types, "rest_value", &rest.range, cli);
        gen_scope
            .generate_deserialize(
                types,
                (&rest.range).into(),
                DeserializeBeforeAfter::new(&format!("let {var_names_str} = "), ";", false),
                DeserializeConfig::new("rest_value"),
                cli,
            )
            .add_to(block);
    } else {
        gen_scope
            .generate_deserialize(
                types,
                (&rest.range).into(),
                DeserializeBeforeAfter::new("let rest_value = ", ";", false),
                DeserializeConfig::new("rest_value"),
                cli,
            )
            .add_to(block);
    }
    let key_is_copy = rest.domain.is_copy(types);
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
            let val_tuple = tuple_str(value_encs.iter().map(|e| e.field_name.clone()).collect());
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
            // Concrete uint/text key: the peeked raw encoding (`key_enc_expr`) converted via the
            // key type's single encoding field. (Composite concrete keys are out of the WP-supported
            // uint/text/any domain set, so a single key-encoding field is the only shape here.)
            let raw = key_enc_expr
                .clone()
                .expect("concrete-domain rest key peeks its encoding under preserve");
            let key_enc_val = key_encs[0].enc_conversion(&raw);
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
            "orig_deser_order.push({} + {}.len());",
            rest_index_base, rest.field_name
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
    let dup_key_error = match domain {
        RestKeyDomain::Uint => "Key::Uint(rest_key)".to_owned(),
        RestKeyDomain::Text => "Key::Str(rest_key)".to_owned(),
        // AnyCbor keys have no simple `Key` spelling — mirror the table dup path's placeholder.
        RestKeyDomain::Any => "Key::Str(String::from(\"<open-map rest key>\"))".to_owned(),
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
    gen_scope.generate_serialize(types, (&rest.domain).into(), block, key_config, cli);
    if !value_encs.is_empty() {
        block.line(&outer.container_encoding_lookup("value", &value_encs, enc_lookup_var));
    }
    let value_config = SerializeConfig::new("value", format!("{}_value", rest.field_name))
        .expr_is_ref(true)
        .is_end(false)
        .encoding_var_no_option_struct()
        .encoding_var_is_ref(false)
        .tag_depth(0);
    gen_scope.generate_serialize(types, (&rest.range).into(), block, value_config, cli);
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
        let var_names_str = encoding_var_names_str(types, &temp_var_prefix, &field.rust_type, cli);
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
            let mut deser_config = DeserializeConfig::for_field(field, in_embedded, field.optional);
            if field.rust_type.is_fixed_value() {
                // A fixed value's deserialize binds `{var}_value` / `{var}_encoding` INLINE — with
                // annotate=false no closure isolates them, so the un-prefixed `{field}_encoding`
                // would shadow this match arm's outer accumulator and the trailing reassignment
                // below would assign the shadow (E0308: `Sz` vs `Option<Sz>`). Bind the temporaries
                // under the same `tmp_` prefix the non-fixed path uses.
                deser_config = deser_config.overload_var_name(&temp_var_prefix);
            }
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
        for enc_field in encoding_fields(
            types,
            &field.name,
            &field.rust_type.clone().resolve_aliases(),
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
        .any(|f| !f.optional && f.rust_type.has_value_bounds());
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
                    } else if matches!(
                        field.rust_type.conceptual_type.resolve_alias_shallow(),
                        ConceptualRustType::Optional(_)
                    ) {
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
                            eprintln!(
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
        // Open struct-map rest row (CAPTURE only): a getter returning the captured entries as the
        // wasm map wrapper (`MapKToV` / the `@duplicates preserve` PairMap-backed twin). Deliberately
        // no `new()` arg and no setter — the rest defaults empty and rides the map wrapper's own
        // mutation surface (matching the rust side, where `new()` excludes it). The wrapper class is
        // minted in the wasm pass (`mint_wasm_wrapper_for_visited_type` for the rest map). An
        // `@ignore` row stores nothing, so it has no getter (its wasm class is a closed struct's).
        if let Some(rest) = record.captured_rest() {
            let rest_ty = rest_member_type(rest);
            let mut getter = codegen::Function::new(&rest.field_name);
            getter
                .arg_ref_self()
                .ret(rest_ty.for_wasm_return(types))
                .vis("pub")
                .doc(
                    "The captured open-map entries whose keys are not declared fields (CDDL \
                     `* k => v` rest row), as the wasm map wrapper.",
                )
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
        if let Some(doc) =
            ignore_aware_doc(config.doc.as_deref(), record.ignored_rest().is_some())
        {
            wrapper.s.doc(&doc);
        }
        wrapper.s_impl.push_fn(wasm_new);
        wrapper.push(gen_scope, types);
    }

    // Rust-only for the rest of this function

    // Struct (fields) + constructor
    let (mut native_struct, mut native_impl) =
        create_base_rust_struct(types, name, config.custom_json, None, cli);
    native_struct.vis("pub");
    if let Some(doc) = ignore_aware_doc(config.doc.as_deref(), record.ignored_rest().is_some()) {
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
        if !gen_scope.deserialize_generated_for_type(types, &field.rust_type.conceptual_type) {
            gen_scope.dont_generate_deserialize(
                name,
                format!(
                    "field {}: {} couldn't generate deserialize",
                    field.name,
                    field.rust_type.for_rust_member(types, false, cli)
                ),
            );
        }
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
                        Some(if opt { P::OptSeq } else { P::Seq })
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
                ConceptualRustType::Fixed(FixedValue::Text(s)) => format!("\"{s}\""),
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
    // Open struct-map rest row (CAPTURE only): a `pub` map field holding the captured unknown
    // entries. Deliberately NOT a constructor argument — `new()` defaults it empty, so adding a rest
    // row to a spec is source-compatible for existing `new()` callers. The container matches the
    // table switch (non-preserve `BTreeMap`; `OrderedHashMap` under `--preserve-encodings`). An
    // `@ignore` row emits NO field (it drops unknown entries), so the struct is a closed struct's.
    if let Some(rest) = record.captured_rest() {
        let mut rest_field = codegen::Field::new(
            format!("pub {}", rest.field_name),
            rest_member_type(rest).for_rust_member(types, false, cli),
        );
        rest_field.doc(
            "Captured open-map entries whose keys are not declared fields (CDDL `* k => v` rest row). \
             Serialized after the declared fields; defaults empty. `@duplicates preserve` makes this \
             a `PairMap` (duplicate keys kept, in wire order); otherwise the loose table container.",
        );
        // The rest field's FLATTENED JSON surface. Skipped when the
        // struct owns a custom json impl (no derive to steer) — matches the declared-field handling.
        if !config.custom_json {
            for annotation in emit_rest_flatten_json(gen_scope, types, name, record, rest, cli) {
                rest_field.annotation(annotation);
            }
        }
        native_struct.push_field(rest_field);
        native_new_block.line(format!(
            "{}: {}::new(),",
            rest.field_name,
            rest_container_ctor(rest, cli)
        ));
    }
    if !native_new_comments.is_empty() {
        native_new.doc(native_new_comments.join("\n"));
    }
    let len_encoding_var = if cli.preserve_encodings {
        let encoding_name = RustIdent::new(CDDLIdent::new(format!("{name}Encoding")));
        native_struct.field(
            format!(
                "{}pub encodings",
                encoding_var_macros(types.key_demand(name), config.custom_json, cli)
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
            // even fixed values still need to keep track of their encodings
            for field_enc in encoding_fields(
                types,
                &field.name,
                &field.rust_type.clone().resolve_aliases(),
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
        // Open struct-map rest row: per-entry encoding sidecars for CONCRETE key/value domains
        // (`any`-typed content is self-carried and contributes none — see `rest_encoding_fields`).
        // The loose (reject/default) container keys the sidecars by the key VALUE (`BTreeMap`); the
        // `@duplicates preserve` twin's keys repeat, so its sidecars are POSITIONAL (`Vec`, parallel
        // to the pair-list, indexed by entry position) — exactly the loose-table pair-map split. The
        // rest row's OWN map-header length lives in the owner's `len_encoding` (no `_encoding` here).
        // Capture-only (an `@ignore` row stores nothing, and it is rejected under preserve anyway).
        if let Some(rest) = record.captured_rest() {
            let key_rust = rest.domain.for_rust_member(types, false, cli);
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

    // Serialization (via rust traits) - includes Deserialization too
    if config.custom_serialize.is_none() || config.custom_deserialize.is_none() {
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
        // (a rest row honored here can only sit in a map record, never a plain group, so this is the
        // real `serialize` — never `serialize_as_embedded_group`).
        if record.ignored_rest().is_some() {
            ser_func.doc(IGNORE_LOSSINESS_DOC);
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
                    name,
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
                    // to support maps with plain groups inside is very difficult as we cannot guarantee
                    // the order of fields so foo = {a, b, bar}, bar = (c, d) could have the order be
                    // {a, d, c, b}, {c, a, b, d}, etc which doesn't fit with the nature of deserialize_as_embedded_group
                    // A possible solution would be to take all fields into one big map, either in generation to begin with,
                    // or just for deserialization then constructing at the end with locals like a, b, bar_c, bar_d.
                    if let ConceptualRustType::Rust(ident) = &field.rust_type.conceptual_type
                        && types.is_plain_group(ident)
                    {
                        gen_scope.dont_generate_deserialize(
                            name,
                            format!(
                                "Map with plain group field {}: {}",
                                field.name,
                                field.rust_type.for_rust_member(types, false, cli)
                            ),
                        );
                    }
                    // declare variables for deser loop
                    if cli.preserve_encodings {
                        for field_enc in encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
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
                    let serialize_config = SerializeConfig::new(&data_name, &field.name)
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
                    if let Some(rest) = &record.rest {
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
                                (&rest.domain).into(),
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
                    let mut ser_loop_match = Block::new("match field_index");
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
                    if let Some(rest) = &record.rest {
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
                if let Some(rest) = record.captured_rest().filter(|_| !cli.preserve_encodings) {
                    let mut rest_loop = Block::new(format!(
                        "for (key, value) in self.{}.iter()",
                        rest.field_name
                    ));
                    let key_config = SerializeConfig::new("key", "rest_key")
                        .expr_is_ref(true)
                        .is_end(false);
                    gen_scope.generate_serialize(
                        types,
                        (&rest.domain).into(),
                        &mut rest_loop,
                        key_config,
                        cli,
                    );
                    let value_config = SerializeConfig::new("value", "rest_value")
                        .expr_is_ref(true)
                        .is_end(false);
                    gen_scope.generate_serialize(
                        types,
                        (&rest.range).into(),
                        &mut rest_loop,
                        value_config,
                        cli,
                    );
                    ser_func.push_block(rest_loop);
                }
                // Open struct-map (loose CBOR): declare the rest capture container (+ preserve
                // encoding sidecars) and fold the unknown-key match arms into captures below.
                // `record.rest` is `None` for every closed struct (byte-identical output).
                let rest_index_base = record.fields.len();
                let rest_domain = record.rest.as_ref().map(|r| RestKeyDomain::of(r));
                let any_cbor = format!("{}::any_cbor::AnyCbor", cli.common_import_rust());
                // BOTH flavors run unknown-key arms that account each entry via `read_len.read_elems`
                // and use `?`, so the loop needs the real (not `_`-prefixed) `read_len` and a
                // Result-returning closure — mark them used regardless of capture/ignore.
                if record.rest.is_some() {
                    deser_code.read_len_used = true;
                    deser_code.throws = true;
                }
                // CAPTURE only: declare the capture container (+ preserve encoding sidecar locals) the
                // arms insert into. An `@ignore` row deserializes-and-DROPS each unknown entry in
                // place, so there is no container and nothing to declare here.
                if let Some(rest) = record.captured_rest() {
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
                let mut type_match = Block::new("match raw.cbor_type()?");
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
                    Some(RestKeyDomain::Any) => Some(format!("{any_cbor}::new_uint(unknown_key)")),
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
                    if let (Some(rest), Some(key_expr)) = (&record.rest, uint_rest_key.clone()) {
                        let mut arm = Block::new("cbor_event::Type::UnsignedInteger =>");
                        if cli.preserve_encodings {
                            arm.line("let (unknown_key, key_enc) = raw.unsigned_integer_sz()?;");
                        } else {
                            arm.line("let unknown_key = raw.unsigned_integer()?;");
                        }
                        append_rest_capture(
                            gen_scope,
                            types,
                            rest,
                            rest_index_base,
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
                    if let (Some(rest), Some(key_expr)) = (&record.rest, uint_rest_key.clone()) {
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
                            rest_index_base,
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
                    Some(RestKeyDomain::Any) => Some(format!("{any_cbor}::new_text(unknown_key)")),
                    _ => None,
                };
                if text_field_deserializers.is_empty() {
                    if let (Some(rest), Some(key_expr)) =
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
                            rest_index_base,
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
                    if let (Some(rest), Some(key_expr)) = (&record.rest, text_rest_key_ref.clone())
                    {
                        // capture arm: `unknown_key` (`&str`) shadows via the match binding; `key_enc`
                        // and `text_key` (owned) are the outer `text_sz()` reads.
                        let mut arm = Block::new("unknown_key =>");
                        append_rest_capture(
                            gen_scope,
                            types,
                            rest,
                            rest_index_base,
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
                } else {
                    let mut text_match =
                        Block::new("cbor_event::Type::Text => match raw.text()?.as_str()");
                    for case in text_field_deserializers {
                        text_match.push_block(case);
                    }
                    if let (Some(rest), Some(key_expr)) = (&record.rest, text_rest_key_ref.clone())
                    {
                        let mut arm = Block::new("unknown_key =>");
                        append_rest_capture(
                            gen_scope,
                            types,
                            rest,
                            rest_index_base,
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
                if let (Some(rest), Some(RestKeyDomain::Any)) = (&record.rest, rest_domain) {
                    // any-domain rest: a Special is either the break ending an indefinite map or a
                    // special-typed KEY (bool/null/undefined/float/unassigned) to capture.
                    // `special_break()` advances ONLY on a true break, so a non-break special is left
                    // intact for the key deserialize; both break match arms diverge (return/break), so
                    // control only falls through to the capture when it was NOT a break.
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
                        rest_index_base,
                        None,
                        None,
                        &mut special_arm,
                        cli,
                    );
                    special_arm.after(",");
                    type_match.push_block(special_arm);
                } else {
                    let mut special_match = Block::new("cbor_event::Type::Special => match len");
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
                    break_check
                        .line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
                    break_check.after(",");
                    special_match.push_block(break_check);
                    special_match.after(",");
                    type_match.push_block(special_match);
                }
                if let (Some(rest), Some(RestKeyDomain::Any)) = (&record.rest, rest_domain) {
                    // any-domain rest: bytes/negative-int/array/map/tag keys land here; deserialize
                    // the key straight from `raw` (uint/text/special are handled by the arms above).
                    let mut arm = Block::new("_ =>");
                    append_rest_capture(
                        gen_scope,
                        types,
                        rest,
                        rest_index_base,
                        None,
                        None,
                        &mut arm,
                        cli,
                    );
                    arm.after(",");
                    type_match.push_block(arm);
                } else {
                    type_match.line("other_type => return Err(DeserializeFailure::UnexpectedKeyType(other_type).into()),");
                }
                deser_loop.push_block(type_match);
                deser_loop.line("read += 1;");
                deser_code.content.push_block(deser_loop);
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
                            FixedValue::Text(_) => {
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
                if let Some(rest) = record.captured_rest() {
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
                        for field_enc in encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
                            true,
                            cli,
                        ) {
                            encoding_ctor.line(format!("{},", field_enc.field_name));
                        }
                    }
                    // Open struct-map rest row: the per-entry encoding sidecar locals (declared before
                    // the loop and populated at each concrete-domain capture) move into the encoding
                    // struct. Absent for a fully self-carried `any` rest. Capture-only (preserve).
                    if let Some(rest) = record.captured_rest() {
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
