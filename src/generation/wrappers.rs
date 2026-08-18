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
    // A SET NOMINAL (Phase 2.2) does NOT emit the inherent `get()`: a 0-arg `get(&self)` shadows the
    // inner `OrderedSet::get(index)` reached through `Deref`, turning every indexed read into a
    // compile error (E0061 — method probing stops at the inherent name). `Deref` covers inner access.
    // An explicit `@newtype <name>` custom getter is still emitted (a custom name doesn't shadow
    // `get(index)`); a bare set nominal emits none.
    let set_nominal = struct_config.set_nominal;
    let emit_getter =
        !set_nominal || matches!(struct_config.newtype_getter.as_ref(), Some(Some(_)));
    let set_demand = crate::comment_ast::DemandSet {
        bare: true,
        hash: true,
        ord: true,
    };
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, type_name, true, cli);
        if let Some(doc) = struct_config.doc.as_ref() {
            wrapper.s.doc(doc);
        }
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .arg(
                "inner",
                gen_scope.wasm_param_type(
                    types,
                    field_type,
                    type_name,
                    "wrapper constructor parameter",
                ),
            )
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
        // Only the `@duplicates reject` set nominal wraps a uniqueness twin
        // (`OrderedSet`/`NonEmptyOrderedSet`/`BoundedOrderedSet`), whose checked `push` / `contains`
        // doors the flat delegation below relies on. Loose/min-one twins additionally expose
        // `insert -> bool` / `try_opt_from`; bounded twins intentionally do not. A `@duplicates preserve` set
        // nominal wraps a plain `Vec`/`NonEmptyVec` (different method surface, no `try_opt_from`), so
        // it keeps the original 0-arg `get()` returning its companion list wrapper.
        let reject_twin = matches!(
            struct_config.duplicates,
            Some(crate::comment_ast::DuplicatesPolicy::Reject)
        );
        let bounded_reject = field_type.is_bounded_reject_ordered_set();
        if set_nominal && reject_twin {
            // FLATTENED set-nominal surface. The wasm class has no `Deref`, so before this the only
            // read door was a 0-arg `get()` returning the companion collection class — forcing every
            // JS read into a two-layer `set.get().get(i)` unwrap. Instead, DELEGATE the companion's
            // collection surface directly onto the nominal (`self.0` is the rust nominal, which
            // `Deref`s to its ordered-set inner — `len`/`get(index)`/`push`/`contains`/`Index` all
            // resolve through it; only loose/min-one flavors add `insert`/`try_opt_from`. JS reads
            // `set.get(i)`, and the wasm surface tells the same story as the rust set API. `try_from`
            // delegates to the rust nominal's `TryFrom<Vec<_>>` door so a flat list constructs the
            // nominal without threading through the companion class.
            let element_type = match &field_type.conceptual_type {
                ConceptualRustType::Array(elem) => (**elem).clone(),
                // set_nominal is set only for Array-wrapped rules (parsing.rs); no other shape reaches here.
                other => unreachable!("set nominal wrapped a non-array type: {other:?}"),
            };
            let native_wrapper = rust_crate_struct_from_wasm(types, type_name, cli);
            let from_elem = |name: &str| {
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, name, false)
                        .into_iter(),
                )
            };
            wrapper
                .s_impl
                .new_fn("len")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .line("self.0.len()");
            wrapper
                .s_impl
                .new_fn("get")
                .vis("pub")
                .ret(gen_scope.wasm_return_type(
                    types,
                    &element_type,
                    type_name,
                    "set-nominal get return",
                ))
                .arg_ref_self()
                .arg("index", "usize")
                .line(element_type.to_wasm_boundary(types, "self.0[index]", false));
            // This is the sole mutable wasm door for a bounded set: both duplicate and overflow
            // are errors, so it must not normalize an attempted insertion into a no-op.
            let elem_handover = |name: &str| {
                super::collections::wasm_exact_byte_handover(&element_type, name, cli)
                    .unwrap_or_else(|| from_elem(name))
            };
            wrapper
                .s_impl
                .new_fn("add")
                .vis("pub")
                .ret("Result<(), JsError>")
                .arg_mut_self()
                .arg(
                    "elem",
                    gen_scope.wasm_param_type(
                        types,
                        &element_type,
                        type_name,
                        "set-nominal add parameter",
                    ),
                )
                .line(format!(
                    "self.0.push({}).map_err(|e| JsError::new(&e.to_string()))",
                    elem_handover("elem")
                ));
            if !bounded_reject {
                let mut insert = codegen::Function::new("insert");
                insert.vis("pub").arg_mut_self().arg(
                    "elem",
                    gen_scope.wasm_param_type(
                        types,
                        &element_type,
                        type_name,
                        "set-nominal insert parameter",
                    ),
                );
                if let Some(handover) =
                    super::collections::wasm_exact_byte_handover(&element_type, "elem", cli)
                {
                    insert
                        .ret("Result<bool, JsError>")
                        .line(format!("Ok(self.0.insert({handover}))"));
                } else {
                    insert
                        .ret("bool")
                        .line(format!("self.0.insert({})", from_elem("elem")));
                }
                wrapper.s_impl.push_fn(insert);
            }
            let mut contains = codegen::Function::new("contains");
            contains.vis("pub").arg_ref_self().arg(
                "elem",
                gen_scope.wasm_param_type(
                    types,
                    &element_type,
                    type_name,
                    "set-nominal contains parameter",
                ),
            );
            if let Some(handover) =
                super::collections::wasm_exact_byte_handover(&element_type, "elem", cli)
            {
                contains
                    .ret("Result<bool, JsError>")
                    .line(format!("Ok(self.0.contains(&{handover}))"));
            } else {
                contains
                    .ret("bool")
                    .line(format!("self.0.contains(&{})", from_elem("elem")));
            }
            wrapper.s_impl.push_fn(contains);
            // A list-taking construction door + the empty-means-absent `try_opt_from` (the wasm
            // mirror of the rust nominal's inherent constructor — its landing removes the matching
            // `PARITY_EXEMPT` entries). Both delegate to the rust nominal's `TryFrom<Vec<Elem>>` /
            // `try_opt_from` through a SHARED list door: an element a BARE `Vec` can carry across the
            // ABI crosses as `Vec<Elem>` (passed straight through); otherwise the minted `<Elem>List`
            // wrapper (always emitted alongside this nominal's companion) is cloned into the native
            // `Vec`. The bare-`Vec` test is `vec_of_self_directly_wasm_exposable` — the ELEMENT's own
            // exposability is the wrong question, and asking it put `Vec<Vec<u8>>` in this signature
            // for a bytes-element set (generation exit 0, wasm crate E0271).
            // A nested non-empty-array element has no clean loose source, so no list door is emitted
            // for it — the sole residual, uncovered by any fixture; a future one re-reds parity on
            // `<Nominal>::try_opt_from` (loud, local) rather than silently miscompiling here.
            let elem_wasm = gen_scope.wasm_member_type(
                types,
                &element_type,
                type_name,
                "set-nominal try_from element type",
            );
            let list_door: Option<(&str, String, Option<String>)> =
                if element_type.vec_of_self_directly_wasm_exposable(types) {
                    Some(("elements", format!("Vec<{elem_wasm}>"), None))
                } else if !element_type.is_non_empty_array() {
                    let loose_type =
                        RustType::new(ConceptualRustType::Array(Box::new(element_type.clone())));
                    let loose = gen_scope.wasm_member_type(
                        types,
                        &loose_type,
                        type_name,
                        "set-nominal try_from loose-list source",
                    );
                    let inner_vec = element_type.name_as_rust_array(types, true, cli);
                    Some((
                        "list",
                        format!("&{loose}"),
                        Some(format!("let list: {inner_vec} = list.clone().into();")),
                    ))
                } else {
                    None
                };
            if let Some((arg, arg_ty, prep)) = &list_door {
                let try_from_fn = wrapper
                    .s_impl
                    .new_fn("try_from")
                    .vis("pub")
                    .ret(format!("Result<{type_name}, JsError>"))
                    .arg(arg, arg_ty);
                if let Some(prep) = prep {
                    try_from_fn.line(prep);
                }
                try_from_fn.line(format!(
                    "{native_wrapper}::try_from({arg}).map(Self).map_err(|e| JsError::new(&e.to_string()))"
                ));
                if !bounded_reject {
                    let try_opt_fn = wrapper
                        .s_impl
                        .new_fn("try_opt_from")
                        .vis("pub")
                        .ret(format!("Result<Option<{type_name}>, JsError>"))
                        .arg(arg, arg_ty);
                    if let Some(prep) = prep {
                        try_opt_fn.line(prep);
                    }
                    try_opt_fn.line(format!(
                    "{native_wrapper}::try_opt_from({arg}).map(|opt| opt.map(Self)).map_err(|e| JsError::new(&e.to_string()))"
                ));
                }
            }
            // A custom `@newtype <name>` getter (rare on a set nominal) still returns the companion —
            // it does not collide with the flat `get(index)` above.
            if let Some(Some(_)) = struct_config.newtype_getter.as_ref() {
                let getter_body = format!("self.0.{getter_name}()");
                wrapper
                    .s_impl
                    .new_fn(getter_name)
                    .vis("pub")
                    .arg_ref_self()
                    .ret(gen_scope.wasm_return_type(
                        types,
                        field_type,
                        type_name,
                        "wrapper custom getter return",
                    ))
                    .line(field_type.to_wasm_boundary(types, &getter_body, false));
            }
        } else if set_nominal {
            // PRESERVE set nominal (`@duplicates preserve`): wraps a plain `Vec`/`NonEmptyVec`, so the
            // uniqueness-twin flat surface above does not apply. It keeps the original 0-arg `get()`
            // returning its companion list wrapper — reconstructed through the emitted
            // `From<Wrapper> for <inner>` impl (a bare set nominal has no rust inherent `get()` to
            // delegate to), then the usual inner→wasm boundary conversion. A custom `@newtype <name>`
            // getter instead delegates to the rust getter, byte-identical.
            let getter_body = if matches!(struct_config.newtype_getter.as_ref(), Some(Some(_))) {
                format!("self.0.{getter_name}()")
            } else {
                // qualified-path form `<T>::from` — a generic inner (`Vec<u64>`) parses `<` as a
                // comparison in the bare `T::from` spelling; `from_wasm=true` crate-qualifies the
                // element so the inner spelling matches the structural wasm wrapper's native field.
                format!(
                    "<{}>::from(self.0.clone())",
                    field_type.for_rust_member(types, true, cli)
                )
            };
            let mut get = codegen::Function::new(getter_name);
            get.vis("pub")
                .arg_ref_self()
                .ret(gen_scope.wasm_return_type(
                    types,
                    field_type,
                    type_name,
                    "wrapper set getter return",
                ))
                .line(field_type.to_wasm_boundary(types, &getter_body, false));
            wrapper.s_impl.push_fn(get);
        } else if emit_getter {
            // Non-set wrappers keep delegating to the rust getter, byte-identical.
            let getter_body = format!("self.0.{getter_name}()");
            let mut get = codegen::Function::new(getter_name);
            get.vis("pub")
                .arg_ref_self()
                .ret(gen_scope.wasm_return_type(
                    types,
                    field_type,
                    type_name,
                    "wrapper getter return",
                ))
                .line(field_type.to_wasm_boundary(types, &getter_body, false));
            wrapper.s_impl.push_fn(get);
        }
        wrapper.push(gen_scope, types);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(
        types,
        type_name,
        true,
        // Set nominals mandate always-on encodings-ignored comparison derives (rethink fact 5).
        if set_nominal { Some(set_demand) } else { None },
        cli,
    );
    if let Some(doc) = struct_config.doc.as_ref() {
        s.doc(doc);
    }
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
    // A newtype wrapping an `any` (e.g. `t = #6.11(any)` → `Tagged(AnyCbor)`)
    // renders its JSON NATURALLY, not through `AnyCbor`'s tagged codec. The wrapper's manual serde /
    // schemars route through the `any_cbor` runtime module's natural adapter (the CBOR-only tag is
    // absent from JSON, so the natural walk of the inner value is the whole JSON surface).
    let json_natural_any = matches!(field_type.resolve_alias_shallow(), ConceptualRustType::Any);
    let any_cbor_mod = format!("{}::any_cbor", cli.common_import_rust());
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
                // One rejection wording for one class: every way the string can be refused — a
                // non-canonical hex character, an odd digit count, a failing `new` — builds the
                // SAME serde error, so a consumer reading it cannot tell which check refused it.
                let err_expr = "serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid hex bytes\")";
                let err_body = format!("{{ {err_expr} }}");
                serde_deser_fn
                    .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
                    // The accepted JSON grammar for a bytes newtype is CANONICAL hex — exactly what
                    // the serialize half above emits — and it is the runtime's
                    // `decode_canonical_hex` that owns it, not this call site. Routing through the
                    // shared door rather than calling the backing decoder here is what keeps this
                    // surface's grammar identical to `RawBytesEncoding::from_raw_hex`'s: the
                    // decoder itself is lenient (it strips a `0x`/`0X` prefix and takes either
                    // case), so a direct call would silently widen the read side past what the
                    // write side emits.
                    .line("decode_canonical_hex(&s)");
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
            } else if json_natural_any {
                serde_ser_fn.line(format!(
                    "{any_cbor_mod}::natural_any_cbor::serialize(&{self_var}, serializer)"
                ));
                serde_deser_fn
                    .line(format!(
                        "let inner = {any_cbor_mod}::natural_any_cbor::deserialize(deserializer)?;"
                    ))
                    // `any` is never a can_new_fail wrapper, so construction is infallible.
                    .line("Ok(Self::new(inner))");
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
                            FixedValue::Undefined => unreachable!(
                                "fixed undefined is a nominal unit value, never a JSON constructor argument"
                            ),
                            FixedValue::Text(_) => "Str(&inner)",
                            FixedValue::Bytes(_) => "Bytes(&inner)",
                            FixedValue::Uint(_) => "Unsigned(inner)",
                        },
                        ConceptualRustType::Map(_, _) => "Map",
                        ConceptualRustType::Optional(_) => "Option",
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bool => "Bool(inner)",
                            Primitive::Bytes => "Bytes(&inner)",
                            Primitive::F16 | Primitive::F32 | Primitive::F16To32 => {
                                "Float(inner as f64)"
                            }
                            Primitive::F64 | Primitive::F32To64 | Primitive::Float => {
                                "Float(inner)"
                            }
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
                        // `any`+json rejects at generation, and `any` is never a can_new_fail
                        // wrapper, so this JSON serde-derive path is unreachable for it.
                        ConceptualRustType::Any => unreachable!(),
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
                .ret("alloc::borrow::Cow<'static, str>")
                .line(format!("alloc::borrow::Cow::Borrowed(\"{type_name}\")"));
            let mut json_schema_fn = codegen::Function::new("json_schema");
            json_schema_fn
                .arg("generator", "&mut schemars::SchemaGenerator")
                .ret("schemars::Schema");
            let mut inline_schema = codegen::Function::new("inline_schema");
            inline_schema.ret("bool");
            if json_natural_any {
                // Permissive natural-rendering schema, distinct from `AnyCbor`'s own tagged codec schema.
                json_schema_fn.line(format!(
                    "{any_cbor_mod}::natural_any_cbor_schema(generator)"
                ));
                inline_schema.line("false");
            } else if let Some(len) = field_type.exact_byte_array_len_checked() {
                // Byte wrappers serialize as canonical hexadecimal strings.  The native carrier
                // makes the byte count exact, so the JSON string has exactly twice that many
                // characters; delegating to `String` alone would silently advertise every length.
                let hex_len = len * 2;
                json_schema_fn
                    // `out` is already in GENERATED_LOCAL_PROBED_SAFE (including the JSON
                    // profiles). Reuse that swept local instead of minting an unverdicted
                    // `schema` binding at this emitter seam.
                    .line("let mut out = String::json_schema(generator);")
                    .line(format!(
                        "out.insert(\"minLength\".to_owned(), {hex_len}u64.into());"
                    ))
                    .line(format!(
                        "out.insert(\"maxLength\".to_owned(), {hex_len}u64.into());"
                    ))
                    .line("out");
                inline_schema.line("String::inline_schema()");
            } else {
                // qualified-path form: `json_schema_type` is a type-position spelling, so a generic
                // backing type (map/array @newtype) needs `<T as Trait>::method`, not `T::method`
                // (which parses `<` as a comparison in expression position). Matches the
                // `<{json_schema_type} as serde::de::Deserialize>::deserialize` precedent above.
                json_schema_fn.line(format!(
                    "<{json_schema_type} as schemars::JsonSchema>::json_schema(generator)"
                ));
                inline_schema.line(format!(
                    "<{json_schema_type} as schemars::JsonSchema>::inline_schema()"
                ));
            }
            json_schema_impl
                .impl_trait("schemars::JsonSchema")
                .push_fn(schema_name_fn)
                .push_fn(json_schema_fn)
                .push_fn(inline_schema);
        }
    }
    s.vis("pub");
    // A complete pair makes this wrapper's wire self-carrying. In particular, preserve mode must
    // not infer the wrapped collection's length/key/value sidecars: they describe the DEFAULT map
    // wire the pair replaced and would both leak a false hand-code contract and make a decoded
    // value appear to retain bytes the pair never saw.
    let custom_pair = match (
        struct_config.custom_serialize.as_ref(),
        struct_config.custom_deserialize.as_ref(),
    ) {
        (Some(custom_serialize), Some(custom_deserialize)) => {
            Some((custom_serialize, custom_deserialize))
        }
        _ => None,
    };
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{type_name}Encoding")));
    let enc_fields = if cli.preserve_encodings {
        // `pub(crate)`, matching the default profile's tuple field: the bound-check boundary that
        // matters is the CRATE boundary — external crates still cannot literal-construct or mutate
        // the wrapper (bypassing the `new()` bound check), so the invariant holds where it's
        // observable. Within the crate, hand-written modules — which under the thin-root layout live
        // OUTSIDE the always-clobbered generated subtree — legitimately need field access (e.g. a
        // `RawBytesEncoding` impl on a bounded newtype). In-crate privacy was already bypassable by
        // dropping a hand file inside the scope subtree, so it protected nothing real.
        // (Named-field shape, so it is NOT routed through `push_overwidth_guarded_tuple_field`: the
        // rustfmt#5703 hazard that helper guards needs a visibility token on a TUPLE field, and a
        // named field of any width is unaffected. The default profile's tuple shape below does go
        // through it, as do the wasm-crate wrappers.)
        s.field(
            "pub(crate) inner",
            field_type.for_rust_member(types, false, cli),
        );
        // DECLARED type (see `EncodingField::type_name`): these become the wrapper's encoding-struct
        // field types, beside the `inner` field spelled from the same `field_type` just above.
        let enc_fields = if custom_pair.is_some() {
            Vec::new()
        } else {
            encoding_fields(types, "inner", field_type, true, cli)
        };

        if !enc_fields.is_empty() {
            // A set nominal derives always-on comparisons (see `create_base_rust_struct` above), so its
            // encodings field must be derivative-IGNORED exactly like a key-demanded struct's — union
            // the full set demand into whatever key demand the rule already carries.
            let enc_demand = match (types.key_demand(type_name), set_nominal) {
                (Some(d), true) => Some(d.union(set_demand)),
                (d, false) => d,
                (None, true) => Some(set_demand),
            };
            s.field(
                format!(
                    "{}pub encodings",
                    encoding_var_macros(enc_demand, true, cli)
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
        // Same `pub(crate)` reasoning as the preserve-encodings named field above, but emitted as a
        // single-field tuple struct — the shape that trips rust-lang/rustfmt#5703 once the field
        // line passes rustfmt's max_width. `push_overwidth_guarded_tuple_field` (mod.rs) owns both
        // the visibility literal and that workaround for this site and for the wasm wrappers.
        push_overwidth_guarded_tuple_field(
            &mut s,
            codegen::Type::new(field_type.for_rust_member(types, false, cli)),
        );
        None
    };
    // TODO: is there a way to know if the encoding object is also copyable?
    if field_type.is_copy(types) && !cli.preserve_encodings {
        s.derive("Copy");
    }
    if emit_getter {
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
    // A complete pair on a self-nominalized table owns the COMPLETE item. The wrapper is still a
    // normal map-wrapper API (`new`/`From`/`get` below), but neither direct bytes APIs nor embedded
    // references may walk that map structurally: both trait shells call the same free functions.
    // Like the record precedent, the nominal value itself crosses the custom boundary under
    // --preserve-encodings; no inferred key/value/length tuple leaks out.
    let mut ser_func = make_serialization_function("serialize", cli);
    let mut ser_impl = make_serialization_impl(type_name.as_ref(), cli);
    if let Some((custom_serialize, _)) = custom_pair {
        ser_func.line(format!(
            "{}(serializer, self{})",
            custom_serialize,
            canonical_param(cli)
        ));
    } else {
        gen_scope.generate_serialize(
            types,
            field_type.into(),
            &mut ser_func,
            SerializeConfig::new(self_var, "inner")
                .is_end(true)
                .encoding_var_in_option_struct("self.encodings"),
            cli,
        );
    }
    ser_impl.push_fn(ser_func);
    let mut deser_func = make_deserialization_function("deserialize", cli);
    let mut deser_impl = codegen::Impl::new(type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    // A wrapper over a genuinely EMBEDDED plain group has no length context to hand the group's
    // `deserialize_as_embedded_group` — that is what this guard is for. It must ask `is_basic`
    // rather than `is_plain_group` alone: a plain group reached through its own array framing
    // (`[coords]`, `bytes .cbor [coords]`) carries `basic_override`, so the emitter calls the
    // group's STANDALONE `deserialize` (which reads the array header itself) and there is nothing
    // to be short of. Before the `.cbor` rule body force-wrapped, the coarse spelling was unreachable
    // for that shape only because the rule registered as a transparent alias instead.
    if field_type.is_basic(types) {
        unimplemented!(
            "TODO: make len/read_len variables of appropriate sizes so the generated code compiles"
        );
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move(types, cli))
        .vis("pub");
    let exact_byte_array_len = field_type.exact_byte_array_len_checked();
    let optional_exact_byte_array_len = match field_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Optional(inner) => inner.exact_byte_array_len_checked(),
        _ => None,
    };
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
    let from_impl = if min_max.is_some()
        || float_min_max.is_some()
        || exact_byte_array_len.is_some()
        || optional_exact_byte_array_len.is_some()
    {
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
                    ConceptualRustType::Primitive(p) if p.float_carrier_is_f32()
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
                            | Primitive::Float
                            | Primitive::F16
                            | Primitive::F32
                            | Primitive::F64
                            | Primitive::F16To32
                            | Primitive::F32To64
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
                    // the wrapper checks its stored `inner` (a member type: i8..i64/u64, or a
                    // `.len()` usize) — never already i128, so the widening cast is real.
                    false,
                )
            }
        };
        if let Some(len) = exact_byte_array_len {
            new_func.line(format!(
                "let inner: [u8; {len}] = inner.try_into().map_err(|bytes: Vec<u8>| DeserializeError::new(\"{type_name}\", DeserializeFailure::RangeCheck{{ found: bytes.len() as i128, min: Some({len}), max: Some({len}) }} ))?;"
            ));
        } else if let Some(len) = optional_exact_byte_array_len {
            new_func.line(format!(
                "let inner: Option<[u8; {len}]> = inner.map(|bytes| bytes.try_into().map_err(|bytes: Vec<u8>| DeserializeError::new(\"{type_name}\", DeserializeFailure::RangeCheck{{ found: bytes.len() as i128, min: Some({len}), max: Some({len}) }} ))).transpose()?;"
            ));
        } else {
            deser_body.line(&render_check(cli.annotate_fields));
        }
        new_func.ret("Result<Self, DeserializeError>");
        if exact_byte_array_len.is_none() && optional_exact_byte_array_len.is_none() {
            new_func.line(render_check(false));
        }
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
            .impl_trait(format!("TryFrom<{}>", field_type.for_rust_move(types, cli)))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_move(types, cli))
            .ret("Result<Self, Self::Error>")
            // `inner` is by-value and already `new()`'s loose input type. For exact bytes it is
            // intentionally `Vec<u8>` while `new()` materializes `[u8; N]`; for every other
            // wrapper it is the same type as `for_rust_member(false)`. Pass it straight through —
            // routing it via `from_wasm_boundary_clone` would emit an identity `.clone().into()`
            // (clippy::useless_conversion + a redundant clone of the owned last-use param). `new()`
            // returns the `Result` here (the bounded/float/exact-byte branch), so TryFrom keeps its
            // semantics.
            .line(format!("{type_name}::new(inner)"));
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
        // `inner` is by-value and already exactly `new()`'s param type, so pass it straight
        // through — see the `try_from` twin above (this unbounded branch's `new()` returns `Self`).
        .line(format!("{type_name}::new(inner)"));
        from
    };
    // Flush the accumulated deserialize() body: wrap it in a single `.annotate(type_name)` error
    // closure when annotate_fields is on (giving container/primitive reads a `failed in <T>`
    // location; the in-body range check is already the locationless form so the closure names it
    // exactly once), else push it verbatim (byte-identical to the pre-annotation output).
    if let Some((_, custom_deserialize)) = custom_pair {
        deser_func.line(format!("{}(raw)", custom_deserialize));
    } else if cli.annotate_fields {
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
    // Set-nominal ergonomics (Phase 2.2), for parity with what a transparent `OrderedSet`/`Vec` alias
    // offered directly: `Deref`/`DerefMut` to the inner collection (`OrderedSet` mutation stays
    // checked, so `DerefMut` cannot break uniqueness), borrowed + owned `IntoIterator`, and Vec
    // conversions. The wrapper path already emits `From<inner>`/`From<Self> for inner` and `new()`.
    // For a plain `Vec` inner (preserve, non-`[+]`) those already ARE the Vec conversions; the
    // fallible-door inners (`NonEmptyVec`/`OrderedSet`/`NonEmptyOrderedSet`) add `From<Self> for
    // Vec<T>` and a duplicate/emptiness-checking `TryFrom<Vec<T>>`.
    if set_nominal {
        let inner_ty = field_type.for_rust_member(types, false, cli);
        let elem_ty = if let ConceptualRustType::Array(elem) = &field_type.conceptual_type {
            elem.for_rust_member(types, false, cli)
        } else {
            unreachable!("a set nominal always wraps a homogeneous occurrence array")
        };
        let inner_is_plain_vec = inner_ty.starts_with("Vec<");
        let owned_iter_body = if inner_is_plain_vec {
            format!("{self_var}.into_iter()")
        } else {
            format!("Vec::<{elem_ty}>::from({self_var}).into_iter()")
        };
        let mut ergo = format!(
            "impl core::ops::Deref for {type_name} {{\n    type Target = {inner_ty};\n\n    fn deref(&self) -> &Self::Target {{\n        &{self_var}\n    }}\n}}\n\nimpl core::ops::DerefMut for {type_name} {{\n    fn deref_mut(&mut self) -> &mut Self::Target {{\n        &mut {self_var}\n    }}\n}}\n\nimpl<'a> IntoIterator for &'a {type_name} {{\n    type Item = &'a {elem_ty};\n    type IntoIter = core::slice::Iter<'a, {elem_ty}>;\n\n    fn into_iter(self) -> Self::IntoIter {{\n        {self_var}.iter()\n    }}\n}}\n\nimpl IntoIterator for {type_name} {{\n    type Item = {elem_ty};\n    type IntoIter = alloc::vec::IntoIter<{elem_ty}>;\n\n    fn into_iter(self) -> Self::IntoIter {{\n        {owned_iter_body}\n    }}\n}}\n"
        );
        if !inner_is_plain_vec {
            ergo.push_str(&format!(
                "\nimpl From<{type_name}> for Vec<{elem_ty}> {{\n    fn from(wrapper: {type_name}) -> Self {{\n        Vec::from(wrapper.{inner_var})\n    }}\n}}\n\nimpl TryFrom<Vec<{elem_ty}>> for {type_name} {{\n    type Error = DeserializeError;\n\n    fn try_from(vec: Vec<{elem_ty}>) -> Result<Self, Self::Error> {{\n        Ok({type_name}::new(<{inner_ty}>::try_from(vec)?))\n    }}\n}}\n"
            ));
        }
        // `try_opt_from` — the empty-means-absent constructor for an optional set field — is a NAMED
        // door (not the blanket-conflicting `TryFrom`), so it is emitted inherently on the nominal,
        // delegating to the inner uniqueness twin's runtime door and re-wrapping each accepted set via
        // `new`. Only the `OrderedSet`/`NonEmptyOrderedSet` inners have this door (the `Vec`/`NonEmptyVec`
        // preserve inners do not), so gate on the twin inner, not merely on `!inner_is_plain_vec`.
        let inner_is_ordered_set =
            inner_ty.starts_with("OrderedSet<") || inner_ty.starts_with("NonEmptyOrderedSet<");
        if inner_is_ordered_set {
            ergo.push_str(&format!(
                "\nimpl {type_name} {{\n    /// Empty input is `Ok(None)` (the optional set field is absent); a non-empty input goes through\n    /// the inner uniqueness door wrapped in `Some`, so ONLY a duplicate surfaces as `Err`.\n    pub fn try_opt_from(vec: Vec<{elem_ty}>) -> Result<Option<Self>, DeserializeError> {{\n        Ok(<{inner_ty}>::try_opt_from(vec)?.map({type_name}::new))\n    }}\n}}\n"
            ));
        }
        gen_scope.rust(types, type_name).raw(&ergo);
    }
    if !struct_config.custom_json {
        if cli.json_serde_derives {
            // The bytes-newtype deserializer emitted above calls `decode_canonical_hex`
            // unqualified, so this scope needs the import. Pushed dumbly (the central
            // `import_prune` pass drops it again if nothing in the module family names it) and
            // routed through `common_import_rust()`, which is what makes it resolve in all three
            // layouts: in-crate (`crate::generated::serialization`), under
            // `--common-import-override` (the shared runtime crate), and in the `--config` split
            // (same, via the override the config writes).
            if json_hex_bytes {
                gen_scope.rust(types, type_name).push_import(
                    format!("{}::serialization", cli.common_import_rust()),
                    "decode_canonical_hex",
                    None,
                );
            }
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
        .push_impl(ser_impl);
    // Same rule as an enum over a refused arm: the wrapper's deserialize reads its INNER type, so
    // a wrapped type with no `Deserialize` leaves the wrapper with none either (verdict seeded
    // before emission — see `seed_no_deserialize_verdicts`).
    if gen_scope.deserialize_generated(type_name) {
        gen_scope
            .rust_serialize(types, type_name)
            .push_impl(deser_impl);
    }
}

pub(super) fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes, cli: &Cli) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    // `--common-import-override` set (`!export_static_files()`): `Int` is common scaffolding
    // parameterized only by CLI flags, the same class as error/serialization/ordered_hash_map. RE-EXPORT
    // the common crate's `Int`/`IntError` rather than minting a crate-local copy, so this crate's `Int`
    // shares the common crate's type identity — a forked rust `Int` fails E0308 at every boundary and a
    // forked wasm `#[wasm_bindgen] Int` cannot link into one cdylib. Emit nothing else (the enum, its
    // impls, and the ser/deser/serde/schemars impls all live in the common crate).
    if !cli.export_static_files() {
        // Place the re-export in Int's ROOT module scope (via `rust`/`wasm`), NOT the `*_lib` scope:
        // a `*_lib` `raw` is emitted before the crate inner attributes (`#![allow(...)]`) that
        // finalization pushes there, which is a compile error. The ROOT module scope is merged AFTER
        // the lib scope, so its `pub use` lands below the attrs — the same placement the in-crate
        // extern re-export glue uses.
        gen_scope.rust(types, &ident).raw(format!(
            "pub use {}::{{Int, IntError}};",
            cli.common_import_rust()
        ));
        if cli.wasm {
            // Wasm FACE of `Int`: the common crate's WASM crate, NOT its rust crate.
            // `common_import_wasm()` returns the override string verbatim — but that names the rust
            // runtime crate (its existing uses import the rust `serialization`/`ordered_hash_map` from
            // wasm code), so a bare `pub use <override>::Int;` re-exports the rust enum where a
            // `#[wasm_bindgen]` class is needed and any exported method returning `Int` fails to
            // compile. Route the override key through `--extern-wasm-crate` (the documented pairing:
            // `--common-import-override=cml_core --extern-wasm-crate=cml_core=cml_core_wasm`), falling
            // back to `common_import_wasm()` when unmapped — the same rule the deferred collection
            // wrappers follow, and the unmapped fallback is what a single-crate target (rust+wasm faces
            // in one crate) relies on. The lookup key matches the map's convention (the extern-dep
            // module name, `extern_dep_names()`) exactly as the `mod.rs`/`requests.rs` lookups do — no
            // extra normalization.
            let wasm_common = cli
                .extern_wasm_crate_map()
                .get(cli.common_import_rust())
                .cloned()
                .unwrap_or_else(|| cli.common_import_wasm());
            gen_scope
                .wasm(types, &ident)
                .raw(format!("pub use {wasm_common}::Int;"));
            // The common wasm crate has no `IntError` (it's not a shared type); keep the local
            // `JsError` parity alias the non-override wasm path emits below — the wasm `from_str`
            // maps rust's `FromStr`-associated `IntError` to `JsError`.
            gen_scope
                .wasm(types, &ident)
                .push_type_alias(TypeAlias::new("IntError", "JsError").vis("pub").clone());
        }
        return;
    }
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

        // An inherent `from_str` beside the rust crate's `FromStr` impl: a trait impl is invisible
        // across the wasm boundary, so the method has to be redefined here to be exported.
        //
        // That rationale is a comment HERE and not a `.line("// …")` in the emitted body, and the
        // distinction is load-bearing rather than stylistic: an own-line comment inside a per-type
        // generated item is stranded when a spec change deletes the type, and the
        // comment-preservation overlay then re-injects it as a `cddl-codegen:unpreserved-comment` +
        // `compile_error!` sentinel that every further regen carries forward. This body carried such
        // a comment until `regen_over_prior_output_corpus` reproduced exactly that trap on three
        // corpus fixtures. The standing emitter rule: generated comments live in fixed banners or in
        // `///` docs (which the overlay owns and drops cleanly), never as a bare `//` on a row a spec
        // change can delete.
        let mut from_str = codegen::Function::new("from_str");
        from_str
            .attr("allow(clippy::should_implement_trait)")
            .vis("pub")
            .arg("string", "&str")
            .ret("Result<Int, JsError>")
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
            .line("core::str::FromStr::from_str(&s).map_err(|_e| serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid Int\"))");
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
            .ret("alloc::borrow::Cow<'static, str>")
            .line("alloc::borrow::Cow::Borrowed(\"Int\")");
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
            .line("cbor_event::Type::UnsignedInteger => raw.unsigned_integer_sz().map(|(x, enc)| Self::Uint{ value: x, encoding: Some(enc) }).map_err(core::convert::Into::into),")
            .line("cbor_event::Type::NegativeInteger => raw.negative_integer_sz().map(|(x, enc)| Self::Nint{ value: (-1 - x) as u64, encoding: Some(enc) }).map_err(core::convert::Into::into),");
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
        .tuple("core::num::TryFromIntError");
    int_err
        .new_variant("Parsing")
        .tuple("core::num::ParseIntError");

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
        .impl_trait("core::fmt::Display")
        .new_fn("fmt")
        .arg_ref_self()
        .arg("f", "&mut core::fmt::Formatter<'_>")
        .ret("core::fmt::Result")
        .push_block(display_match);

    let mut from_str = codegen::Impl::new("Int");
    from_str
        .impl_trait("core::str::FromStr")
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
        .associate_type("Error", "core::num::TryFromIntError")
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

/// wasm face of the `AnyCbor` runtime type (CDDL `any`). Sibling of `generate_int`; called from the
/// generate loop's wasm prelude when `types.uses_any_cbor() && cli.wasm` (keyed on usage, not on an
/// ident reference — `AnyCbor` is a static-runtime type, never a registered `RustStruct`).
///
/// Two paths, exactly as `generate_int`:
/// - `--common-import-override` (`!export_static_files()`): re-export the common WASM crate's
///   `AnyCbor`/`AnyCborKind` classes (a forked `#[wasm_bindgen]` class cannot link into one cdylib),
///   routing the crate key through `--extern-wasm-crate` with the `common_import_wasm()` fallback.
/// - own-static path: mint a base wrapper around the import-glued rust `AnyCbor`. The base struct
///   already provides `from_cbor_bytes`/`to_cbor_bytes` (+ `to_json`/`from_json` under the json
///   flag) against `self.0` — `AnyCbor` impls the local `Serialize`/`Deserialize` traits and, under
///   the json flag, serde's — so v1's CBOR/JSON surface needs no hand-written method bodies. We add
///   only `kind()` and a wasm-side `AnyCborKind` c-style enum.
pub(super) fn generate_any_cbor_wasm(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    cli: &Cli,
) {
    assert!(cli.wasm);
    let ident = RustIdent::new(CDDLIdent::new("AnyCbor"));
    let kind_ident = RustIdent::new(CDDLIdent::new("AnyCborKind"));

    if !cli.export_static_files() {
        // Wasm FACE of the common crate's runtime types: route the `--common-import-override` key
        // through `--extern-wasm-crate` (same rule as `Int` and the deferred collection wrappers),
        // falling back to `common_import_wasm()` when unmapped (single-crate rust+wasm target).
        let wasm_common = cli
            .extern_wasm_crate_map()
            .get(cli.common_import_rust())
            .cloned()
            .unwrap_or_else(|| cli.common_import_wasm());
        gen_scope
            .wasm(types, &ident)
            .raw(format!("pub use {wasm_common}::{{AnyCbor, AnyCborKind}};"));
        return;
    }

    // Import-glued inner rust path. The synthetic `AnyCbor` ident has no registered scope
    // (`types.scope` -> root), so `create_base_wasm_wrapper`'s `default_structure` path would
    // mis-render it as `<lib>::AnyCbor`, missing the `any_cbor` submodule. Build the base with no
    // inner field, then push the correct native name by hand. `common_import_wasm()` is the rust
    // crate as seen from wasm code (its `any_cbor` module is re-exported at the crate root via
    // `pub use generated::*`).
    let native_name = format!("{}::any_cbor::AnyCbor", cli.common_import_wasm());
    let mut wrapper = create_base_wasm_wrapper(gen_scope, types, &ident, false, cli);
    wrapper.push_inner_field(&native_name);
    wrapper.add_conversion_methods(&native_name, cli);

    let mut kind_fn = codegen::Function::new("kind");
    kind_fn
        .vis("pub")
        .arg_ref_self()
        .ret("AnyCborKind")
        .line("self.0.kind().into()");
    wrapper.s_impl.push_fn(kind_fn);
    wrapper.push(gen_scope, types);

    // wasm-exposable `AnyCborKind`: the rust `AnyCborKind` lives in the rust crate without a
    // `#[wasm_bindgen]` attr, so the wasm face mints its own c-style enum plus a `From` conversion
    // (the pattern for kind accessors whose enum is not itself feature-gated in the rust crate). The
    // variant set mirrors `static/any_cbor_*.rs`'s `AnyCborKind` verbatim — a fixed runtime-type
    // contract; keep in lockstep if that enum ever gains a variant.
    const ANY_CBOR_KIND_VARIANTS: [&str; 12] = [
        "UInt",
        "NInt",
        "Bytes",
        "Text",
        "Array",
        "Map",
        "Tag",
        "Bool",
        "Null",
        "Undefined",
        "Unassigned",
        "Float",
    ];
    let native_kind = format!("{}::any_cbor::AnyCborKind", cli.common_import_wasm());
    let mut kind_enum = codegen::Enum::new("AnyCborKind");
    kind_enum
        .vis("pub")
        .derive("Copy")
        .derive("Clone")
        .attr("wasm_bindgen");
    for v in ANY_CBOR_KIND_VARIANTS {
        kind_enum.new_variant(v);
    }
    gen_scope.wasm(types, &kind_ident).push_enum(kind_enum);

    let mut kind_from = codegen::Impl::new("AnyCborKind");
    let mut kind_from_fn = codegen::Function::new("from");
    kind_from_fn.arg("native", &native_kind).ret("Self");
    let mut match_block = Block::new("match native");
    for v in ANY_CBOR_KIND_VARIANTS {
        match_block.line(format!("{native_kind}::{v} => Self::{v},"));
    }
    kind_from_fn.push_block(match_block);
    kind_from
        .impl_trait(format!("From<{native_kind}>"))
        .push_fn(kind_from_fn);
    gen_scope.wasm(types, &kind_ident).push_impl(kind_from);
}
