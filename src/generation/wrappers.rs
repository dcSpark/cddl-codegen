use super::*;

// `annotated` - true iff deser_func is the body of an `.annotate(ident)` error closure: emit
// locationless errors and let the closure supply the name (the per-error annotate/named forms
// would get the name prepended AGAIN by the closure, reading "Name.Name"). When false, each error
// carries the name itself, as no closure will add it.
pub(crate) fn generate_tag_check(
    deser_func: &mut dyn CodeBlock,
    ident: &RustIdent,
    tag: Option<usize>,
    annotated: bool,
) {
    if let Some(tag) = tag {
        if annotated {
            deser_func.line("let tag = raw.tag()?;");
        } else {
            deser_func.line(&format!(
                "let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{ident}\"))?;"
            ));
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        if annotated {
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
        } else {
            tag_check.line(format!("return Err(DeserializeError::new(\"{ident}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        }
        deser_func.push_block(tag_check);
    }
}

// This is used mostly for when thing are tagged have specific ranges.
#[allow(clippy::too_many_arguments)]
pub(super) fn generate_wrapper_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    type_name: &RustIdent,
    field_type: &RustType,
    min_max: Option<(Option<i128>, Option<i128>)>,
    float_min_max: Option<crate::intermediate::FloatWindow>,
    struct_config: &RustStructConfig,
    cli: &Cli,
) {
    if min_max.is_some() || float_min_max.is_some() {
        assert!(types.can_new_fail(type_name));
    }
    // The inner-value getter name: an explicit `@newtype <name>` renames it, otherwise every
    // wrapper (bare tag, plain `@newtype`, bounded/range) exposes the inner value under `get`.
    let getter_name = match struct_config.newtype_getter.as_ref() {
        Some(Some(name)) => name.as_str(),
        _ => "get",
    };
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, type_name, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .arg("inner", field_type.for_wasm_param(types))
            .vis("pub");

        // Delegate to the rust wrapper's `new`, mirroring the enum-variant wasm ctor: convert the
        // wasm inner to the rust inner (fallibility, if any, lives in the rust `new`, so pass
        // can_fail = false here), then let the rust ctor produce the native wrapper. Building
        // `Self(inner.into())` directly would need two chained `.into()`s for a Rust-typed inner
        // (wasm→native inner, then native inner→native wrapper) with an uninferable middle type.
        let from_wasm_expr = field_type.from_wasm_boundary_clone(types, "inner", false);
        let ctor = format!(
            "{}::new({})",
            rust_crate_struct_from_wasm(types, type_name, cli),
            ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
        );
        if types.can_new_fail(type_name) {
            // you can't use Self in a parameter in wasm_bindgen for some reason
            wasm_new
                .ret(format!("Result<{type_name}, JsError>"))
                .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
        } else {
            wasm_new.ret("Self").line(format!("Self({ctor})"));
        }
        wrapper.s_impl.push_fn(wasm_new);
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub")
            .arg_ref_self()
            .ret(field_type.for_wasm_return(types))
            .line(field_type.to_wasm_boundary(types, &format!("self.0.{getter_name}()"), false));
        wrapper.s_impl.push_fn(get);
        wrapper.push(gen_scope, types);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(types, type_name, true, cli);
    let (inner_var, self_var) = if cli.preserve_encodings {
        ("inner", "self.inner")
    } else {
        ("0", "self.0")
    };

    // manual JSON impls
    let mut serde_ser_impl = codegen::Impl::new(type_name);
    let mut serde_deser_impl = codegen::Impl::new(type_name);
    let mut json_schema_impl = codegen::Impl::new(type_name);
    let json_hex_bytes = matches!(
        field_type.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::Bytes)
    );
    let json_schema_type = if json_hex_bytes {
        Cow::Borrowed("String")
    } else {
        Cow::Owned(field_type.for_rust_member(types, false, cli))
    };

    if !struct_config.custom_json {
        // serde Serialize / Deserialize
        if cli.json_serde_derives {
            let mut serde_ser_fn = codegen::Function::new("serialize");
            serde_ser_fn
                .generic("S")
                .bound("S", "serde::Serializer")
                .arg_ref_self()
                .arg("serializer", "S")
                .ret("Result<S::Ok, S::Error>");
            let mut serde_deser_fn = codegen::Function::new("deserialize");
            serde_deser_fn
                .generic("D")
                .bound("D", "serde::de::Deserializer<'de>")
                .arg("deserializer", "D")
                .ret("Result<Self, D::Error>");
            if json_hex_bytes {
                serde_ser_fn.line(format!(
                    "serializer.serialize_str(&hex::encode({self_var}.clone()))"
                ));
                let err_body = "{ serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid hex bytes\") }";
                serde_deser_fn
                    .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
                    .line("hex::decode(&s)");
                if types.can_new_fail(type_name) {
                    serde_deser_fn
                        .line(format!(
                            ".ok().and_then(|bytes| {type_name}::new(bytes).ok())"
                        ))
                        .line(format!(".ok_or_else(|| {err_body})"));
                } else {
                    serde_deser_fn
                        .line(format!(".map({type_name}::new)"))
                        .line(format!(".map_err(|_e| {err_body})"));
                }
            } else {
                serde_ser_fn.line(format!("{self_var}.serialize(serializer)"));
                serde_deser_fn
                    .line(format!("let inner = <{json_schema_type} as serde::de::Deserialize>::deserialize(deserializer)?;"));
                if types.can_new_fail(type_name) {
                    let unexpected = match field_type.resolve_alias_shallow() {
                        ConceptualRustType::Alias(_, _) => unreachable!(),
                        ConceptualRustType::Array(_) => "Seq",
                        ConceptualRustType::Fixed(fixed) => match fixed {
                            FixedValue::Bool(_) => "Bool(inner)",
                            FixedValue::Float(_) => "Float(inner)",
                            FixedValue::Nint(_) => "Signed(inner as i64)",
                            FixedValue::Null => "Option",
                            FixedValue::Text(_) => "Str(&inner)",
                            FixedValue::Uint(_) => "Unsigned(inner)",
                        },
                        ConceptualRustType::Map(_, _) => "Map",
                        ConceptualRustType::Optional(_) => "Option",
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bool => "Bool(inner)",
                            Primitive::Bytes => "Bytes(&inner)",
                            Primitive::F32 => "Float(inner as f64)",
                            Primitive::F64 => "Float(inner)",
                            Primitive::I8
                            | Primitive::I16
                            | Primitive::I32
                            | Primitive::I64
                            | Primitive::N64 => "Signed(inner as i64)",
                            Primitive::Str => "Str(&inner)",
                            Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                                "Unsigned(inner as u64)"
                            }
                            Primitive::U64 => "Unsigned(inner)",
                        },
                        ConceptualRustType::Rust(_) => "StructVariant",
                    };
                    // Unexpected::Str(&inner)/Bytes(&inner) borrow `inner` in the error closure,
                    // but `Self::new(inner)` moves it first (String/Vec aren't Copy) → E0382. Clone
                    // into the constructor in that case so the original survives for the error. The
                    // other (Copy) variants need no clone.
                    let new_arg = if unexpected.contains("&inner") {
                        "inner.clone()"
                    } else {
                        "inner"
                    };
                    serde_deser_fn
                        .line(format!("Self::new({new_arg})"))
                        .line(format!(".map_err(|_e| {{ serde::de::Error::invalid_value(serde::de::Unexpected::{unexpected}, &\"invalid {type_name}\") }})"));
                } else {
                    serde_deser_fn.line("Ok(Self::new(inner))");
                }
            }
            serde_ser_impl
                .impl_trait("serde::Serialize")
                .push_fn(serde_ser_fn);
            serde_deser_impl
                .impl_trait("serde::de::Deserialize<'de>")
                .generic("'de")
                .push_fn(serde_deser_fn);
        }

        // JsonSchema
        if cli.json_schema_export {
            let mut schema_name_fn = codegen::Function::new("schema_name");
            schema_name_fn
                .ret("::std::borrow::Cow<'static, str>")
                .line(format!("::std::borrow::Cow::Borrowed(\"{type_name}\")"));
            let mut json_schema_fn = codegen::Function::new("json_schema");
            json_schema_fn
                .arg("generator", "&mut schemars::SchemaGenerator")
                .ret("schemars::Schema")
                // qualified-path form: `json_schema_type` is a type-position spelling, so a generic
                // backing type (map/array @newtype) needs `<T as Trait>::method`, not `T::method`
                // (which parses `<` as a comparison in expression position). Matches the
                // `<{json_schema_type} as serde::de::Deserialize>::deserialize` precedent above.
                .line(format!(
                    "<{json_schema_type} as schemars::JsonSchema>::json_schema(generator)"
                ));
            let mut inline_schema = codegen::Function::new("inline_schema");
            inline_schema.ret("bool").line(format!(
                "<{json_schema_type} as schemars::JsonSchema>::inline_schema()"
            ));
            json_schema_impl
                .impl_trait("schemars::JsonSchema")
                .push_fn(schema_name_fn)
                .push_fn(json_schema_fn)
                .push_fn(inline_schema);
        }
    }
    s.vis("pub");
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{type_name}Encoding")));
    let enc_fields = if cli.preserve_encodings {
        // `pub(crate)`, matching the default profile's tuple field: the bound-check boundary that
        // matters is the CRATE boundary — external crates still cannot literal-construct or mutate
        // the wrapper (bypassing the `new()` bound check), so the invariant holds where it's
        // observable. Within the crate, hand-written modules — which under the thin-root layout live
        // OUTSIDE the always-clobbered generated subtree — legitimately need field access (e.g. a
        // `RawBytesEncoding` impl on a bounded newtype). In-crate privacy was already bypassable by
        // dropping a hand file inside the scope subtree, so it protected nothing real.
        s.field(
            "pub(crate) inner",
            field_type.for_rust_member(types, false, cli),
        );
        let enc_fields = encoding_fields(
            types,
            "inner",
            &field_type.clone().resolve_aliases(),
            true,
            cli,
        );

        if !enc_fields.is_empty() {
            s.field(
                format!(
                    "{}pub encodings",
                    encoding_var_macros(types.key_demand(type_name), true, cli)
                ),
                format!("Option<{encoding_name}>"),
            );
            let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
            let mut encoding_aliases: Vec<(String, String)> = Vec::new();
            for field_enc in &enc_fields {
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    type_name,
                    &field_enc.field_name,
                    &field_enc.type_name,
                );
            }
            let enc_scope = gen_scope.cbor_encodings(types, type_name);
            for (alias, target) in encoding_aliases {
                enc_scope.push_type_alias(TypeAlias::new(&alias, &target).vis("pub").clone());
            }
            enc_scope.push_struct(encoding_struct);
        }
        Some(enc_fields)
    } else {
        s.tuple_field(
            Some("pub(crate)".to_string()),
            field_type.for_rust_member(types, false, cli),
        );
        None
    };
    // TODO: is there a way to know if the encoding object is also copyable?
    if field_type.is_copy(types) && !cli.preserve_encodings {
        s.derive("Copy");
    }
    {
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub").arg_ref_self();
        if field_type.is_copy(types) {
            get.ret(field_type.for_rust_member(types, false, cli))
                .line(field_type.clone_if_not_copy(types, self_var));
        } else {
            get.ret(format!(
                "&{}",
                field_type.for_rust_member(types, false, cli)
            ))
            .line(format!("&{self_var}"));
        }
        s_impl.push_fn(get);
    }
    let mut ser_func = make_serialization_function("serialize", cli);
    let mut ser_impl = make_serialization_impl(type_name.as_ref(), cli);
    gen_scope.generate_serialize(
        types,
        field_type.into(),
        &mut ser_func,
        SerializeConfig::new(self_var, "inner")
            .is_end(true)
            .encoding_var_in_option_struct("self.encodings"),
        cli,
    );
    ser_impl.push_fn(ser_func);
    let mut deser_func = make_deserialization_function("deserialize", cli);
    let mut deser_impl = codegen::Impl::new(type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let ConceptualRustType::Rust(id) = &field_type.conceptual_type
        && types.is_plain_group(id)
    {
        unimplemented!(
            "TODO: make len/read_len variables of appropriate sizes so the generated code compiles"
        );
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move(types, cli))
        .vis("pub");
    let var_names_str = if cli.preserve_encodings {
        encoding_var_names_str(types, "inner", field_type, cli)
    } else {
        "inner".to_owned()
    };
    // nint is stored as its u64 magnitude, and magnitude is a *decreasing* function of the value, so
    // a value bound maps to a SWAPPED magnitude bound (`nint_bounds_to_u64`) — the same transform the
    // struct-field / setter paths apply. Without it the wrapper's `new()`/deserialize check compares
    // the u64 `inner` against a negative literal (does not compile: E0600) with inverted semantics.
    let min_max = if matches!(
        &field_type.conceptual_type,
        ConceptualRustType::Primitive(Primitive::N64)
    ) && !field_type
        .encodings
        .contains(&CBOREncodingOperation::CBORBytes)
    {
        min_max.map(|mm| nint_bounds_to_u64(&mm))
    } else {
        min_max
    };
    // The whole deserialize() body is accumulated here so it can be wrapped in one
    // `.annotate(type_name)` error closure when `cli.annotate_fields` (giving the container/
    // primitive reads a `failed in <T>` location exactly as field-level errors already get). When
    // annotate_fields is off no closure is emitted and the content is pushed verbatim, byte-identical
    // to before. `new()` and the `TryFrom`/`From` paths NEVER go through this closure, so any error
    // they emit must keep the name-carrying form (see `build_check`'s `annotated=false` arm).
    let mut deser_body = BlocksOrLines::default();
    let from_impl = if min_max.is_some() || float_min_max.is_some() {
        let (before, after) = if var_names_str.is_empty() {
            ("".to_owned(), "")
        } else {
            (format!("let {var_names_str} = "), ";")
        };
        gen_scope
            .generate_deserialize(
                types,
                field_type.into(),
                DeserializeBeforeAfter::new(&before, after, false),
                DeserializeConfig::new("inner"),
                cli,
            )
            .add_to(&mut deser_body);

        // Materialize the range check per-consumer via the shared bounds-check owner, so the wrapper
        // spells its condition exactly as the member/deserialize sites do. Both copies keep the
        // ORIGINAL failure payload (min/max unchanged) — only the condition unifies. The deserialize()
        // copy is locationless (`.into()`) when it lands inside the annotate closure, while the
        // `new()` copy always carries the name (`DeserializeError::new`) since no closure ever wraps
        // it, so `annotated` maps directly onto `location` (annotated → None, else Some(type_name)).
        let render_check = |annotated: bool| -> String {
            let location = if annotated {
                None
            } else {
                Some(type_name.as_ref())
            };
            if let Some(window) = &float_min_max {
                // NaN-safe float window: accept-form negation, value compared as f64 so the authored
                // decimal literal is exact. Reports the ORIGINAL window with its per-side exclusivity.
                let cast_f64 = matches!(
                    &field_type.conceptual_type,
                    ConceptualRustType::Primitive(Primitive::F32)
                );
                bounds_check_if_block_float(window, cast_f64, "inner", true, location)
            } else {
                let (min, max) = min_max.unwrap();
                let against = if field_type
                    .encodings
                    .contains(&CBOREncodingOperation::CBORBytes)
                {
                    "inner.len()"
                } else {
                    match &field_type.conceptual_type {
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bytes | Primitive::Str => "inner.len()",
                            Primitive::Bool
                            | Primitive::F32
                            | Primitive::F64
                            | Primitive::U8
                            | Primitive::U16
                            | Primitive::U32
                            | Primitive::U64
                            | Primitive::I8
                            | Primitive::I16
                            | Primitive::I32
                            | Primitive::I64
                            | Primitive::N64 => "inner",
                        },
                        _ => unimplemented!(),
                    }
                };
                bounds_check_if_block(
                    &(min, max),
                    against,
                    true,
                    bounds_check_expr_non_negative(field_type),
                    location,
                )
            }
        };
        deser_body.line(&render_check(cli.annotate_fields));
        new_func
            .ret("Result<Self, DeserializeError>")
            .line(render_check(false));
        if let Some(enc_fields) = &enc_fields {
            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_body.push_block(deser_ctor);

            let mut ctor_block = Block::new("Ok(Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            ctor_block.after(")");
            new_func.push_block(ctor_block);
        } else {
            deser_body.line("Ok(Self(inner))");
            new_func.line("Ok(Self(inner))");
        }
        let mut try_from = codegen::Impl::new(type_name.to_string());
        try_from
            .associate_type("Error", "DeserializeError")
            .impl_trait(format!(
                "TryFrom<{}>",
                field_type.for_rust_member(types, false, cli)
            ))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_member(types, false, cli))
            .ret("Result<Self, Self::Error>")
            .line(format!(
                "{}::new({})",
                type_name,
                ToWasmBoundaryOperations::format(
                    field_type
                        .from_wasm_boundary_clone(types, "inner", false)
                        .into_iter()
                )
            ));
        try_from
    } else {
        // let field_type_tagged = if let Some(t) = tag {
        //     ConceptualRustType::Tagged(t, Box::new(field_type.clone()))
        // } else {
        //     field_type.clone()
        // };
        // gen_scope.generate_deserialize(types, &field_type_tagged, "inner", "Ok(Self(", "))", false, false, true, &mut deser_func);
        new_func.ret("Self");
        if let Some(enc_fields) = &enc_fields {
            let (before, after) = if var_names_str.is_empty() {
                ("".to_owned(), "")
            } else {
                (format!("let {var_names_str} = "), ";")
            };
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new(&before, after, false),
                    DeserializeConfig::new("inner"),
                    cli,
                )
                .add_to(&mut deser_body);

            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_body.push_block(deser_ctor);

            let mut ctor_block = Block::new("Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            new_func.push_block(ctor_block);
        } else {
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new("Ok(Self(", "))", false),
                    DeserializeConfig::new("inner"),
                    cli,
                )
                .add_to(&mut deser_body);
            new_func.line("Self(inner)");
        }

        let mut from = codegen::Impl::new(type_name.to_string());
        from.impl_trait(format!(
            "From<{}>",
            field_type.for_rust_member(types, false, cli)
        ))
        .new_fn("from")
        .arg("inner", field_type.for_rust_member(types, false, cli))
        .ret("Self")
        .line(format!(
            "{}::new({})",
            type_name,
            ToWasmBoundaryOperations::format(
                field_type
                    .from_wasm_boundary_clone(types, "inner", false)
                    .into_iter()
            )
        ));
        from
    };
    // Flush the accumulated deserialize() body: wrap it in a single `.annotate(type_name)` error
    // closure when annotate_fields is on (giving container/primitive reads a `failed in <T>`
    // location; the in-body range check is already the locationless form so the closure names it
    // exactly once), else push it verbatim (byte-identical to the pre-annotation output).
    if cli.annotate_fields {
        let mut error_annotator = make_err_annotate_block(type_name.as_ref(), "", "");
        error_annotator.push_all(deser_body);
        deser_func.push_block(error_annotator);
    } else {
        deser_func.push_all(deser_body);
    }
    deser_impl.push_fn(deser_func);
    s_impl.push_fn(new_func);
    let mut from_inner_impl = codegen::Impl::new(field_type.for_rust_member(types, false, cli));
    from_inner_impl
        .impl_trait(format!("From<{type_name}>"))
        .new_fn("from")
        .arg("wrapper", type_name.to_string())
        .ret("Self")
        .line(format!("wrapper.{inner_var}"));
    gen_scope
        .rust(types, type_name)
        .push_struct(s)
        .push_impl(s_impl)
        .push_impl(from_impl)
        .push_impl(from_inner_impl);
    if !struct_config.custom_json {
        if cli.json_serde_derives {
            gen_scope
                .rust(types, type_name)
                .push_impl(serde_ser_impl)
                .push_impl(serde_deser_impl);
        }
        if cli.json_schema_export {
            gen_scope.rust(types, type_name).push_impl(json_schema_impl);
        }
    }
    gen_scope
        .rust_serialize(types, type_name)
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

pub(super) fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes, cli: &Cli) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, &ident, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        let mut new_if = Block::new("if x >= 0");
        let mut new_else = Block::new("else");
        new_if.line(format!(
            "Self({}::Int::new_uint(x as u64))",
            cli.lib_name_code()
        ));
        new_else.line(format!(
            "Self({}::Int::new_nint((x + 1).unsigned_abs()))",
            cli.lib_name_code()
        ));
        wasm_new
            .ret("Self")
            .vis("pub")
            .arg("x", "i64")
            .push_block(new_if)
            .push_block(new_else);

        let mut wasm_new_uint = codegen::Function::new("new_uint");
        wasm_new_uint
            .ret("Self")
            .vis("pub")
            .arg("value", "u64")
            .line(format!(
                "Self({}::Int::new_uint(value))",
                cli.lib_name_code()
            ));

        let mut wasm_new_nint = codegen::Function::new("new_nint");
        wasm_new_nint
            .ret("Self")
            .vis("pub")
            .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
            .arg("value", "u64")
            .line(format!("Self({}::Int::new_nint(value))", cli.lib_name_code()));

        let mut to_str = codegen::Function::new("to_str");
        to_str
            .vis("pub")
            .arg_ref_self()
            .ret("String")
            .line("self.0.to_string()");

        let mut from_str = codegen::Function::new("from_str");
        from_str
            .attr("allow(clippy::should_implement_trait)")
            .vis("pub")
            .arg("string", "&str")
            .ret("Result<Int, JsError>")
            .line("// have to redefine so it's visible in WASM")
            .line("std::str::FromStr::from_str(string).map(Self).map_err(|e| JsError::new(&format!(\"Int.from_str({}): {:?}\", string, e)))");

        wrapper
            .s_impl
            .push_fn(wasm_new)
            .push_fn(wasm_new_uint)
            .push_fn(wasm_new_nint)
            .push_fn(to_str)
            .push_fn(from_str);
        wrapper.push(gen_scope, types);

        // Rust exposes `IntError` as the `FromStr` associated error. The wasm constructor maps that
        // to `JsError`, so keep source-level parity without claiming wasm exports a data-bearing enum.
        gen_scope
            .wasm(types, &ident)
            .push_type_alias(TypeAlias::new("IntError", "JsError").vis("pub").clone());
    }

    let mut native_struct = codegen::Enum::new("Int");
    native_struct.vis("pub");
    let mut uint = codegen::Variant::new("Uint");
    let mut nint = codegen::Variant::new("Nint");
    if cli.preserve_encodings {
        uint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
        nint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
    } else {
        uint.tuple("u64");
        nint.tuple("u64");
    }
    native_struct.push_variant(uint);
    native_struct.push_variant(nint);
    add_struct_derives(
        &mut native_struct,
        types.key_demand(&ident),
        /* is_enum */ true,
        /* cstyle_baseline */ false,
        /* custom_json */ true,
        cli,
    );

    // JSON: Int's serde/schemars impls are written here by hand (the `custom_json` arg above) rather
    // than derived — the derived enum form would leak the CBOR encoding quirk (`{"Nint":4}` actually
    // meaning -5). Serialize as the signed decimal string (via Display/FromStr): it matches Int's own
    // to_str/from_str API and safely covers the full [-2^64, 2^64) range a JSON number can't hold.
    if cli.json_serde_derives {
        let mut serde_ser_impl = codegen::Impl::new("Int");
        let mut serde_ser_fn = codegen::Function::new("serialize");
        serde_ser_fn
            .generic("S")
            .bound("S", "serde::Serializer")
            .arg_ref_self()
            .arg("serializer", "S")
            .ret("Result<S::Ok, S::Error>")
            .line("serializer.serialize_str(&self.to_string())");
        serde_ser_impl
            .impl_trait("serde::Serialize")
            .push_fn(serde_ser_fn);
        gen_scope.rust_lib().push_impl(serde_ser_impl);

        let mut serde_deser_impl = codegen::Impl::new("Int");
        let mut serde_deser_fn = codegen::Function::new("deserialize");
        serde_deser_fn
            .generic("D")
            .bound("D", "serde::de::Deserializer<'de>")
            .arg("deserializer", "D")
            .ret("Result<Self, D::Error>")
            .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
            .line("std::str::FromStr::from_str(&s).map_err(|_e| serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid Int\"))");
        serde_deser_impl
            .impl_trait("serde::de::Deserialize<'de>")
            .generic("'de")
            .push_fn(serde_deser_fn);
        gen_scope.rust_lib().push_impl(serde_deser_impl);
    }
    if cli.json_schema_export {
        let mut json_schema_impl = codegen::Impl::new("Int");
        let mut schema_name_fn = codegen::Function::new("schema_name");
        schema_name_fn
            .ret("::std::borrow::Cow<'static, str>")
            .line("::std::borrow::Cow::Borrowed(\"Int\")");
        let mut json_schema_fn = codegen::Function::new("json_schema");
        json_schema_fn
            .arg("generator", "&mut schemars::SchemaGenerator")
            .ret("schemars::Schema")
            .line("String::json_schema(generator)");
        let mut inline_schema_fn = codegen::Function::new("inline_schema");
        inline_schema_fn.ret("bool").line("String::inline_schema()");
        json_schema_impl
            .impl_trait("schemars::JsonSchema")
            .push_fn(schema_name_fn)
            .push_fn(json_schema_fn)
            .push_fn(inline_schema_fn);
        gen_scope.rust_lib().push_impl(json_schema_impl);
    }

    // impl Int
    let mut native_impl = codegen::Impl::new("Int");
    let mut new_uint = codegen::Function::new("new_uint");
    new_uint.vis("pub").arg("value", "u64").ret("Self");
    if cli.preserve_encodings {
        let mut new_uint_ctor = Block::new("Self::Uint");
        new_uint_ctor.line("value,").line("encoding: None,");
        new_uint.push_block(new_uint_ctor);
    } else {
        new_uint.line("Self::Uint(value)");
    }
    native_impl.push_fn(new_uint);

    let mut new_nint = codegen::Function::new("new_nint");
    new_nint
        .vis("pub")
        .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
        .arg("value", "u64")
        .ret("Self");
    if cli.preserve_encodings {
        let mut new_nint_ctor = Block::new("Self::Nint");
        new_nint_ctor.line("value,").line("encoding: None,");
        new_nint.push_block(new_nint_ctor);
    } else {
        new_nint.line("Self::Nint(value)");
    }
    native_impl.push_fn(new_nint);

    // serialization
    let mut ser_impl = make_serialization_impl("Int", cli);
    let mut ser_func = make_serialization_function("serialize", cli);
    let mut ser_block = Block::new("match self");
    if cli.preserve_encodings {
        ser_block
            .line(format!("Self::Uint{{ value, encoding }} => serializer.write_unsigned_integer_sz(*value, fit_sz(*value, *encoding{})),", canonical_param(cli)))
            .line(format!("Self::Nint{{ value, encoding }} => serializer.write_negative_integer_sz(-((*value as i128) + 1), fit_sz(*value, *encoding{})),", canonical_param(cli)));
    } else {
        ser_block
            .line("Self::Uint(x) => serializer.write_unsigned_integer(*x),")
            .line("Self::Nint(x) => serializer.write_negative_integer_sz(-((*x as i128) + 1), cbor_event::Sz::canonical(*x)),");
    }
    ser_func.push_block(ser_block);
    ser_impl.push_fn(ser_func);

    // deserialization
    let mut deser_impl = codegen::Impl::new("Int");
    deser_impl.impl_trait("Deserialize");
    let mut deser_func = make_deserialization_function("deserialize", cli);
    let mut annotate = make_err_annotate_block("Int", "", "");
    let mut deser_match = Block::new("match raw.cbor_type()?");
    if cli.preserve_encodings {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => raw.unsigned_integer_sz().map(|(x, enc)| Self::Uint{ value: x, encoding: Some(enc) }).map_err(std::convert::Into::into),")
            .line("cbor_event::Type::NegativeInteger => raw.negative_integer_sz().map(|(x, enc)| Self::Nint{ value: (-1 - x) as u64, encoding: Some(enc) }).map_err(std::convert::Into::into),");
    } else {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => Ok(Self::Uint(raw.unsigned_integer()?)),")
            .line("cbor_event::Type::NegativeInteger => Ok(Self::Nint((-1 - raw.negative_integer_sz().map(|(x, _enc)| x)?) as u64)),");
    }
    deser_match.line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
    annotate.push_block(deser_match);
    deser_func.push_block(annotate);
    deser_impl.push_fn(deser_func);

    // traits
    let mut int_err = codegen::Enum::new("IntError");
    int_err.vis("pub").derive("Clone").derive("Debug");
    int_err
        .new_variant("Bounds")
        .tuple("std::num::TryFromIntError");
    int_err
        .new_variant("Parsing")
        .tuple("std::num::ParseIntError");

    let mut display = codegen::Impl::new("Int");
    let mut display_match = Block::new("match self");
    // Nint: RFC 8949 §3.1 says a major-type-1 value is `-1 - argument`, range -2^64..=-1. The
    // argument is held as u64, so the most-negative Int (argument u64::MAX -> -2^64) overflows
    // i64/u64; compute the signed value in i128 (same idiom as the serialize path below). Both
    // branches below do this; they differ only in field syntax (named under preserve, else tuple).
    if cli.preserve_encodings {
        display_match
            .line("Self::Uint{ value, .. } => write!(f, \"{}\", value),")
            .line("Self::Nint{ value, .. } => write!(f, \"{}\", -((*value as i128) + 1)),");
    } else {
        display_match
            .line("Self::Uint(x) => write!(f, \"{}\", x),")
            .line("Self::Nint(x) => write!(f, \"{}\", -((*x as i128) + 1)),");
    }
    display
        .impl_trait("std::fmt::Display")
        .new_fn("fmt")
        .arg_ref_self()
        .arg("f", "&mut std::fmt::Formatter<'_>")
        .ret("std::fmt::Result")
        .push_block(display_match);

    let mut from_str = codegen::Impl::new("Int");
    from_str
        .impl_trait("std::str::FromStr")
        .associate_type("Err", "IntError")
        .new_fn("from_str")
        .arg("s", "&str")
        .ret("Result<Self, Self::Err>")
        .line("let x = i128::from_str(s).map_err(IntError::Parsing)?;")
        .line("Self::try_from(x).map_err(IntError::Bounds)");

    let mut try_from_i128 = codegen::Impl::new("Int");
    let mut try_from_if = Block::new("if x >= 0");
    let mut try_from_else = Block::new("else");
    if cli.preserve_encodings {
        try_from_if.line("u64::try_from(x).map(|x| Self::Uint{ value: x, encoding: None })");
        try_from_else.line(
            "u64::try_from((x + 1).unsigned_abs()).map(|x| Self::Nint{ value: x, encoding: None })",
        );
    } else {
        try_from_if.line("u64::try_from(x).map(Self::Uint)");
        try_from_else.line("u64::try_from((x + 1).unsigned_abs()).map(Self::Nint)");
    }
    try_from_i128
        .impl_trait("TryFrom<i128>")
        .associate_type("Error", "std::num::TryFromIntError")
        .new_fn("try_from")
        .arg("x", "i128")
        .ret("Result<Self, Self::Error>")
        .push_block(try_from_if)
        .push_block(try_from_else);

    gen_scope
        .rust_lib()
        .push_enum(native_struct)
        .push_enum(int_err)
        .push_impl(native_impl)
        .push_impl(display)
        .push_impl(from_str)
        .push_impl(try_from_i128);
    gen_scope
        .rust_serialize_lib()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}
