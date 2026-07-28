use super::*;

impl GenerationScope {
    // TODO: repurpose this for type choices (not group choices)
    // TODO: make this its own function - there's no reason for this to be a method
    pub(super) fn generate_type_choices_from_variants(
        &mut self,
        types: &IntermediateTypes,
        name: &RustIdent,
        variants: &[EnumVariant],
        tag: Option<usize>,
        config: &RustStructConfig,
        cli: &Cli,
    ) {
        // I don't believe this is even possible (wouldn't be a single CBOR value + nowhere to embed)
        // Just sanity checking since it's not handled in the wrapper code here
        assert!(
            variants
                .iter()
                .all(|v| !matches!(v.data, EnumVariantData::Inlined(_)))
        );
        // Rust only
        generate_enum(self, types, name, variants, None, true, tag, config, cli);
        if cli.wasm {
            // Generate a wrapper object that we will expose to wasm around this
            let mut wrapper = create_base_wasm_wrapper(self, types, name, true, cli);
            // new
            for variant in variants.iter() {
                let variant_arg = variant.name_as_var();
                let mut new_func = codegen::Function::new(format!("new_{variant_arg}"));
                new_func.vis("pub");
                if let Some(doc) = &variant.doc {
                    new_func.doc(doc);
                }
                // This wasm ctor must mirror the fallibility of the rust-side type-choice ctor it
                // calls (generate_enum's rep=None path), which is fallible iff the variant type
                // carries an inline value bound (`has_value_bounds()`) — that path emits the inline
                // bounds check and returns `Result`. A *named* type's own fallible `new` is
                // irrelevant here: both ctors receive an already-constructed value, so the inner
                // type's construction (and its own bounds) already happened upstream. Using
                // `needs_bounds_check_if_inlined()` (which also trips on any named `can_new_fail`
                // type, i.e. every bounded Wrapper) would wrongly make this wasm ctor fallible over
                // an infallible rust ctor.
                let can_fail = variant.rust_type().has_value_bounds();
                if !variant.rust_type().is_fixed_value() {
                    new_func.arg(&variant_arg, variant.rust_type().for_wasm_param(types));
                }
                let ctor = if variant.rust_type().is_fixed_value() {
                    format!(
                        "{}::new_{}()",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name_as_var()
                    )
                } else {
                    // Never `try_into` at the wasm boundary: the rust ctor takes an already-built
                    // value, so any inner-type bound was enforced when that value was constructed.
                    let try_into = false;
                    let from_wasm_expr =
                        variant
                            .rust_type()
                            .from_wasm_boundary_clone(types, &variant_arg, try_into);
                    format!(
                        "{}::new_{}({})",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name_as_var(),
                        ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
                    )
                };
                if can_fail {
                    new_func
                        .ret(format!("Result<{name}, JsError>"))
                        .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                } else {
                    new_func.ret("Self").line(format!("Self({ctor})"));
                }
                wrapper.s_impl.push_fn(new_func);
            }
            add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, None, cli);
            wrapper.push(self, types);
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn codegen_group_choices(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Representation,
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) {
    // rust inner enum
    generate_enum(
        gen_scope,
        types,
        name,
        variants,
        Some(rep),
        false,
        tag,
        config,
        cli,
    );

    // wasm wrapper
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true, cli);
        // new (1 per variant)
        for variant in variants.iter() {
            // TODO: verify if variant.serialize_as_embedded_group impacts ctor generation
            let mut new_func = codegen::Function::new(format!("new_{}", variant.name_as_var()));
            new_func.vis("pub");
            if let Some(doc) = &variant.doc {
                new_func.doc(doc);
            }
            let mut output_comma = false;
            // We only want to generate Variant::new() calls when we created a special struct
            // for the variant, which happens in the general case for multi-field group choices
            let ctor_fields: Option<Vec<&RustField>> = match &variant.data {
                // named type resolving to a Record — the shared helper (also what the
                // `scope_references` import walk marks, so params and imports can't drift)
                EnumVariantData::RustType(_) => variant.group_ctor_record_fields(types, name),
                // an inlined record has no named struct to construct separately, so even its
                // OPTIONAL fields stay direct ctor params (hence no `!f.optional` filter here)
                EnumVariantData::Inlined(record) => Some(
                    record
                        .fields
                        .iter()
                        .filter(|f| !f.rust_type.is_fixed_value())
                        .collect(),
                ),
            };
            match ctor_fields {
                Some(ctor_fields) => {
                    let can_fail = ctor_fields.iter().any(|f| f.rust_type.has_value_bounds());
                    match ctor_fields.len() {
                        0 => {
                            new_func
                                .line(format!(
                                    "Self({}::new_{}())",
                                    rust_crate_struct_from_wasm(types, name, cli),
                                    variant.name_as_var()
                                ))
                                .ret("Self");
                        }
                        // TODO: verify. I think this was here so that 1-field things would be directly stored
                        // 1 => {
                        //     let field = ctor_fields.first().unwrap();
                        //     println!("in {} there's {:?}", enum_name, field);
                        //     new_func
                        //         .arg(&field.name, field.rust_type.for_wasm_param())
                        //         .line(format!("Self({}::{}({}))", enum_name, variant.name, variant.rust_type.from_wasm_boundary_clone(&field.name)));
                        // },
                        // multi-field struct, so for convenience we let you pass the parameters directly here
                        // instead of having to separately construct the variant to pass in
                        _ => {
                            let mut ctor = format!(
                                "{}::new_{}(",
                                rust_crate_struct_from_wasm(types, name, cli),
                                variant.name_as_var()
                            );
                            for field in ctor_fields {
                                if output_comma {
                                    ctor.push_str(", ");
                                } else {
                                    output_comma = true;
                                }
                                // optional only reaches here on the Inlined arm (the named-Record
                                // arm filters `!f.optional`), where it wraps as Option via
                                // `to_embedded_rust_type`
                                let wasm_param_type = field.to_embedded_rust_type();
                                new_func.arg(&field.name, wasm_param_type.for_wasm_param(types));
                                ctor.push_str(&ToWasmBoundaryOperations::format(
                                    wasm_param_type
                                        .from_wasm_boundary_clone(types, &field.name, false)
                                        .into_iter(),
                                ));
                            }
                            ctor.push(')');
                            if can_fail {
                                new_func
                                    .ret(format!("Result<{name}, JsError>"))
                                    .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                            } else {
                                new_func.ret("Self").line(format!("Self({ctor})"));
                            }
                        }
                    }
                }
                None => {
                    // just directly pass in the variant's type
                    if variant.rust_type().is_fixed_value() {
                        new_func.ret("Self").line(format!(
                            "Self({}::new_{}())",
                            rust_crate_struct_from_wasm(types, name, cli),
                            variant.name_as_var()
                        ));
                    } else {
                        let field_name = convert_to_snake_case(&variant.name.to_string());
                        let ctor = format!(
                            "{}::new_{}({})",
                            rust_crate_struct_from_wasm(types, name, cli),
                            variant.name_as_var(),
                            ToWasmBoundaryOperations::format(
                                variant
                                    .rust_type()
                                    .from_wasm_boundary_clone(types, &field_name, false)
                                    .into_iter()
                            )
                        );
                        new_func.arg(&field_name, variant.rust_type().for_wasm_param(types));
                        if variant.rust_type().has_value_bounds() {
                            new_func
                                .ret(format!("Result<{name}, JsError>"))
                                .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                        } else {
                            new_func.ret("Self").line(format!("Self({ctor})"));
                        };
                    }
                }
            };
            wrapper.s_impl.push_fn(new_func);
        }
        // enum-getters
        add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, Some(rep), cli);
        wrapper.push(gen_scope, types);
    }
}

fn add_wasm_enum_getters(
    s_impl: &mut codegen::Impl,
    types: &IntermediateTypes<'_>,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    cli: &Cli,
) {
    assert!(cli.wasm);
    // kind() getter
    let kind_name = format!("{name}Kind");
    let mut get_kind = codegen::Function::new("kind");
    get_kind.arg_ref_self().vis("pub").ret(&kind_name);
    let mut get_kind_match = Block::new("match &self.0");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
        get_kind_match.line(format!(
            "{}::{}{} => {}::{},",
            rust_crate_struct_from_wasm(types, name, cli),
            variant.name,
            enum_gen_info.capture_ignore_all(),
            kind_name,
            variant.name
        ));
    }
    get_kind.push_block(get_kind_match);
    s_impl.push_fn(get_kind);

    // as_{variant} conversions (returns None -> undefined when not the type)
    for variant in variants.iter() {
        let mut add_variant_functions = |ty: &RustType| {
            let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
            let mut as_variant = codegen::Function::new(format!("as_{}", variant.name_as_var()));
            as_variant.arg_ref_self().vis("pub");
            let mut variant_match = Block::new("match &self.0");
            // unfortunately wasm_bindgen doesn't support nested options so we must flatten
            // this is a bit ambiguous but it's better than nothing
            let supported = if let ConceptualRustType::Optional(inner) = ty.resolve_alias_shallow()
            {
                if let ConceptualRustType::Optional(_) = inner.resolve_alias_shallow() {
                    // An enum variant whose payload resolves to Option<Option<T>> (a
                    // nullable-of-nullable, e.g. `text / ((uint / null) / null)`, or via an alias
                    // chain to a nullable) is UNREACHABLE at this getter arm: the wasm enum
                    // CONSTRUCTOR for such a variant panics earlier, in
                    // `from_wasm_boundary_clone_optional` ("unsupported or unexpected"), before getter
                    // emission ever runs. No supported CDDL reaches here, so the former silent
                    // `println!` skip only advertised a behavior (dropping the getter) that can never
                    // occur. Fail loudly instead: if a future constructor change lets the shape emit,
                    // this points at the real work — double-flatten the getter plus an
                    // `as_<variant>_present()` presence accessor (see docs/docs/wasm_differences.mdx)
                    // — rather than silently dropping the getter.
                    unreachable!(
                        "enum variant {}::{} resolves to Option<Option<T>>, which the wasm enum \
                         constructor rejects (from_wasm_boundary_clone_optional) before getters are \
                         emitted — no supported CDDL reaches this arm",
                        name,
                        variant.name_as_var()
                    );
                } else {
                    as_variant
                        .ret(ty.for_wasm_return(types))
                        .doc(format!("Returns None if not {} variant OR it is but it's set to None\nThis is to get around wasm_bindgen not supporting Option<Option<T>>", variant.name));
                    variant_match.line(format!(
                        "{}::{}{} => {},",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name,
                        enum_gen_info.capture_ignore_encodings(),
                        ty.to_wasm_boundary(types, &enum_gen_info.names[0], true)
                    ));
                    true
                }
            } else {
                as_variant.ret(format!("Option<{}>", ty.for_wasm_return(types)));
                variant_match.line(format!(
                    "{}::{}{} => Some({}),",
                    rust_crate_struct_from_wasm(types, name, cli),
                    variant.name,
                    enum_gen_info.capture_ignore_encodings(),
                    ty.to_wasm_boundary(types, &enum_gen_info.names[0], true)
                ));
                true
            };
            if supported {
                variant_match.line("_ => None,");
                as_variant.push_block(variant_match);
                s_impl.push_fn(as_variant);
            }
        };
        match &variant.data {
            EnumVariantData::RustType(ty) => {
                if !ty.is_fixed_value() {
                    add_variant_functions(ty);
                }
            }
            EnumVariantData::Inlined(record) => {
                let non_fixed_types = record
                    .fields
                    .iter()
                    .filter(|field| !field.rust_type.is_fixed_value())
                    .collect::<Vec<_>>();
                // we don't even embed in this case and instead crate a new variant but this is here in case someone
                // tries to add that in the future so they hit this assert.
                assert!(
                    non_fixed_types.len() <= 1,
                    "multiple non-fixed not allowed right now for embedding into enums"
                );
                if let Some(&field) = non_fixed_types.first() {
                    add_variant_functions(field.to_embedded_rust_type().as_ref());
                }
            }
        }
    }
}

/// All the details about how a given EnumVariant will be structured in rust
/// e.g. will it be a tuple or a named variant, what will the fields be called
/// plus helpers to deal with how to pattern match/construct these without
/// caring about the actual representation.
#[derive(Debug)]
struct EnumVariantInRust {
    name: VariantIdent,
    enc_fields: Vec<EncodingField>,
    names: Vec<String>,
    types: Vec<String>,
    outer_vars: usize,
}

impl EnumVariantInRust {
    fn new(
        types: &IntermediateTypes,
        variant: &EnumVariant,
        rep: Option<Representation>,
        cli: &Cli,
    ) -> Self {
        let name = variant.name_as_var();
        match &variant.data {
            EnumVariantData::RustType(ty) => {
                let mut enc_fields = if cli.preserve_encodings {
                    encoding_fields(types, &name, &ty.clone().resolve_aliases(), true, cli)
                } else {
                    vec![]
                };
                // A collapsed map-rep arm writes+verifies a fixed member key on the wire; under
                // preserve-encodings its layout is remembered in a `{var}_key_encoding` field, kept
                // right after the value's encodings and before the outer `len_encoding`.
                if cli.preserve_encodings
                    && rep == Some(Representation::Map)
                    && let Some(key) = &variant.key
                {
                    enc_fields.push(key_encoding_field(&name, key));
                }
                let (mut enum_types, mut names) = if ty.is_fixed_value() {
                    (vec![], vec![])
                } else {
                    (vec![ty.for_rust_member(types, false, cli)], vec![name])
                };
                let mut outer_vars = 0;
                // TOOD: for tags too?
                if cli.preserve_encodings && rep.is_some() && !variant.serialize_as_embedded_group {
                    enc_fields.push(EncodingField {
                        field_name: "len_encoding".to_owned(),
                        type_name: "LenEncoding".to_owned(),
                        default_expr: "LenEncoding::default()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: true,
                    });
                    outer_vars += 1;
                }
                for enc_field in &enc_fields {
                    enum_types.push(enc_field.type_name.clone());
                    names.push(enc_field.field_name.clone());
                }
                assert_eq!(enum_types.len(), names.len());
                Self {
                    name: variant.name.clone(),
                    enc_fields,
                    names,
                    types: enum_types,
                    outer_vars,
                }
            }
            EnumVariantData::Inlined(record) => {
                let mut enc_fields = vec![];
                let mut enum_types = vec![];
                let mut names = vec![];
                if cli.preserve_encodings {
                    enc_fields.push(EncodingField {
                        field_name: "len_encoding".to_owned(),
                        type_name: "LenEncoding".to_owned(),
                        default_expr: "LenEncoding::default()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: true,
                    });
                    for field in record.fields.iter() {
                        enc_fields.extend(encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
                            true,
                            cli,
                        ));
                    }
                }
                for field in record.fields.iter() {
                    if !field.rust_type.is_fixed_value() {
                        names.push(field.name.clone());
                        enum_types.push(
                            field
                                .to_embedded_rust_type()
                                .for_rust_member(types, false, cli),
                        );
                    }
                }
                for enc_field in &enc_fields {
                    enum_types.push(enc_field.type_name.clone());
                    names.push(enc_field.field_name.clone());
                }
                assert_eq!(enum_types.len(), names.len());
                Self {
                    name: variant.name.clone(),
                    enc_fields,
                    names,
                    types: enum_types,
                    outer_vars: 0,
                }
            }
        }
    }

    fn names_without_outer(&self) -> &[String] {
        &self.names[..self.names.len() - self.outer_vars]
    }

    fn names_with_macros(
        &self,
        key_demand: Option<DemandSet>,
        custom_json: bool,
        cli: &Cli,
    ) -> Vec<String> {
        self.names
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if i < self.names.len() - self.enc_fields.len() {
                    // not an encoding variable:
                    name.clone()
                } else {
                    // encoding variable:
                    // the codeen crate doesn't support proc macros on fields but we just inline
                    // these with a newline in the field names for declaring as workaround.
                    // Indentation is never an issue as we're always 2 levels deep for field declarations
                    format!(
                        "{}{}",
                        encoding_var_macros(key_demand, custom_json, cli),
                        name
                    )
                }
            })
            .collect()
    }

    fn capture_all(&self) -> String {
        match self.names.len() {
            0 => "".to_owned(),
            1 if self.enc_fields.is_empty() => format!("({})", self.names[0]),
            _ => format!("{{ {} }}", self.names.join(", ")),
        }
    }

    fn capture_ignore_all(&self) -> &'static str {
        match self.names.len() {
            0 => "",
            1 if self.enc_fields.is_empty() => "(_)",
            _ => "{ .. }",
        }
    }

    fn capture_ignore_encodings(&self) -> String {
        match self.names.len() {
            0 => "".to_owned(),
            1 if self.enc_fields.is_empty() => format!("({})", self.names[0]),
            _ => {
                if self.enc_fields.len() == self.names.len() {
                    "{ .. }".to_owned()
                } else {
                    format!("{{ {}, .. }}", self.names[0])
                }
            }
        }
    }

    /// if init_fields exists, use these for values, otherwise assumes variables exist with same names
    fn generate_constructor(
        &self,
        body: &mut dyn CodeBlock,
        before: &str,
        after: &str,
        init_fields: Option<&Vec<String>>,
    ) {
        let init_fields = init_fields.unwrap_or_else(|| self.names.as_ref());
        match init_fields.len() {
            0 => {
                body.line(&format!("{}Self::{}{}", before, self.name, after));
            }
            1 if self.enc_fields.is_empty() => {
                body.line(&format!(
                    "{}Self::{}({}){}",
                    before,
                    self.name,
                    init_fields.join(", "),
                    after
                ));
            }
            _ => {
                let mut ctor = Block::new(format!("{}Self::{}", before, self.name));
                for (name, expr) in self.names.iter().zip(init_fields.iter()) {
                    if name == expr {
                        ctor.line(format!("{name},"));
                    } else {
                        ctor.line(format!("{name}: {expr},"));
                    }
                }
                ctor.after(after);
                body.push_block(ctor);
            }
        };
    }
}

// Generates an enum where all variants are fixed values (i.e. C-style enum)
// and return true, or return false and do nothing (i.e. enum too complex)
pub(super) fn generate_c_style_enum(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) -> bool {
    if tag.is_some() && cli.preserve_encodings {
        // cannot store it in a C-style enum
        return false;
    }
    if variants.iter().any(|ev: &EnumVariant| {
        ev.serialize_as_embedded_group
            || (cli.preserve_encodings && !ev.rust_type().encodings.is_empty())
            || !matches!(
                ev.rust_type().conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Fixed(_)
            )
    }) {
        return false;
    }
    // rust enum containing the data
    let mut e = codegen::Enum::new(name.to_string());
    e.vis("pub");
    e.derive("Copy");
    // Eq/PartialEq/Ord/PartialOrd are needed for a c-style enum used as a map/set key. When it *is* a
    // key, `add_struct_derives` (below) adds them — and handles `--preserve-encodings` via `derivative`
    // — so deriving them here too would double-derive (`E0119` conflicting impls). Only add them here
    // for the non-key case (unchanged output there).
    if !types.used_as_key(name) {
        e.derive("Eq")
            .derive("PartialEq")
            .derive("Ord")
            .derive("PartialOrd");
    }
    if cli.wasm {
        // Gate the attribute behind a cargo feature (default `wasm`) so the rust crate compiles
        // standalone without the optional wasm-bindgen dep; the wasm crate enables the feature via
        // its path dep. This is the only rust-crate-side `#[wasm_bindgen]` emission.
        e.attr(format!(
            "cfg_attr(feature = \"{}\", wasm_bindgen::prelude::wasm_bindgen)",
            cli.rust_wasm_feature
        ));
        gen_scope
            .wasm(types, name)
            .new_import(
                rust_crate_struct_scope_from_wasm(types, name, cli),
                name,
                None,
            )
            .vis("pub");
    }
    add_struct_derives(
        &mut e,
        types.key_demand(name),
        true,
        /* cstyle_baseline */ true,
        config.custom_json,
        cli,
    );
    for variant in variants.iter() {
        e.new_variant(variant.name.to_string());
    }
    // Only the enum definition is emitted — no serialize/deserialize impl. A c-style enum's
    // fixed-value encoding is generated inline wherever it's used (see the field/variant serializers)
    // rather than via an `impl` on the enum, so a c-style enum that nothing references produces no
    // serialization code at all (its `serialization.rs` ends up empty).
    gen_scope.rust(types, name).push_enum(e);
    true
}

#[allow(clippy::too_many_arguments)]
pub(super) fn make_enum_variant_return_if_deserialized(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    variant: &EnumVariant,
    no_enum_types: bool,
    len_check: Option<(RustStructCBORLen, Representation)>,
    deser_body: &mut dyn CodeBlock,
    // The deserializer this variant probe reads from. `"raw"` for an enum's own `deserialize` impl
    // (where `raw` is the function's parameter); the overload (`inner_de`) when the enum's decode is
    // INLINED under a `bytes .cbor` payload — a C-style enum has no `Deserialize` impl of its own,
    // so its try-each-variant sequence is emitted at the use site and must read the payload's cursor.
    // Both emission branches below must agree on the name: the single-line branch splices the
    // variant code straight into `deser_body` with nothing shadowing, so its body has to spell the
    // overload, which in turn forces the closure branch to BIND that same name (a closure parameter
    // called `raw` around a body spelling `inner_de` would not compile).
    deserializer_name: &str,
    cli: &Cli,
) -> Block {
    let (before, after) = if len_check.is_some() && !no_enum_types {
        ("let ret = ", ";")
    } else {
        ("", "")
    };
    let variant_deser_code = if no_enum_types {
        let mut code = gen_scope.generate_deserialize(
            types,
            (variant.rust_type()).into(),
            DeserializeBeforeAfter::new(before, after, false),
            DeserializeConfig::new(&variant.name_as_var()).overload_deserializer(deserializer_name),
            cli,
        );
        if let Some((len_info, rep)) = len_check {
            code = surround_in_len_checks(code, len_info, rep, cli);
        }
        code.content.line("Ok(())");
        code
    } else {
        let mut code = gen_scope.generate_deserialize(
            types,
            (variant.rust_type()).into(),
            DeserializeBeforeAfter::new(before, after, true),
            DeserializeConfig::new(&variant.name_as_var()).overload_deserializer(deserializer_name),
            cli,
        );
        if let Some((len_info, rep)) = len_check {
            code = surround_in_len_checks(code, len_info, rep, cli);
            code.content.line("ret");
        }
        code
    };
    match variant_deser_code.content.as_single_line() {
        Some(single_line) if !variant_deser_code.throws => {
            // to get around type annotations being needed for error types (e.g. auto conversions with ?) we make a variable
            // to do better than this we'd need to make DeserializationCode keep track of error types too.
            deser_body.line(&format!(
                "let deser_variant: Result<_, DeserializeError> = {single_line};"
            ));
        }
        _ => {
            let mut variant_deser = Block::new(format!(
                "let deser_variant = (|{deserializer_name}: &mut Deserializer| -> Result<_, DeserializeError>"
            ));
            variant_deser.after(format!(")({deserializer_name});"));
            variant_deser.push_all(variant_deser_code.content);
            deser_body.push_block(variant_deser);
        }
    }
    Block::new("match deser_variant")
}

fn surround_in_len_checks(
    mut main_deser_code: DeserializationCode,
    len_info: RustStructCBORLen,
    rep: Representation,
    cli: &Cli,
) -> DeserializationCode {
    let mut len_check_before = DeserializationCode::default();
    add_deserialize_initial_len_check(&mut len_check_before.content, len_info, cli);
    main_deser_code.add_to_code(&mut len_check_before);
    main_deser_code = len_check_before;
    add_deserialize_final_len_check(&mut main_deser_code.content, Some(rep), len_info, cli);
    main_deser_code
}

fn make_inline_deser_code(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    tag: Option<usize>,
    record: &RustRecord,
    enum_gen_info: &EnumVariantInRust,
    cli: &Cli,
) -> DeserializationCode {
    let mut variant_deser_code = generate_array_struct_deserialization(
        gen_scope, types, name, record, tag, false, false, cli,
    );
    // generate_constructor zips the expressions with the names in the enum_gen_info
    // so just make sure we're in the same order as returned above
    assert_eq!(
        enum_gen_info.names.len(),
        variant_deser_code.deser_ctor_fields.len()
            + variant_deser_code.encoding_struct_ctor_fields.len()
    );
    let ctor_exprs = variant_deser_code
        .deser_ctor_fields
        .into_iter()
        .chain(variant_deser_code.encoding_struct_ctor_fields)
        .zip(enum_gen_info.names.iter())
        .map(|((var, expr), name)| {
            assert_eq!(var, *name);
            expr
        })
        .collect();
    variant_deser_code.deser_code = surround_in_len_checks(
        variant_deser_code.deser_code,
        record.cbor_len_info(types),
        record.rep,
        cli,
    );
    enum_gen_info.generate_constructor(
        &mut variant_deser_code.deser_code.content,
        "Ok(",
        ")",
        Some(&ctor_exprs),
    );
    variant_deser_code.deser_code
}

/// Writes the fixed member key of a collapsed map-rep group-choice arm, between the map header and
/// the value. Under `--preserve-encodings` it uses the variant's `{var}_key_encoding` field
/// (captured directly from the match arm), mirroring the record map-key write path.
fn push_map_choice_key_ser(
    body: &mut dyn CodeBlock,
    variant_var: &str,
    key: &FixedValue,
    cli: &Cli,
) {
    match key {
        FixedValue::Uint(x) => {
            let expr = format!("{x}u64");
            // the key encoding var is a `Copy` `Option<Sz>` captured by ref → deref like the value
            // path does via `encoding_var_is_ref`.
            write_using_sz(
                body,
                "write_unsigned_integer",
                "serializer",
                &expr,
                &expr,
                "?;",
                &format!("*{variant_var}_key_encoding"),
                cli,
            );
        }
        FixedValue::Text(s) => {
            write_string_sz(
                body,
                "write_text",
                "serializer",
                &format!("\"{}\"", escape_rust_str(s)),
                true,
                "?;",
                &format!("{variant_var}_key_encoding"),
                cli,
            );
        }
        _ => panic!("unsupported map choice key type (only uint/text are supported): {key:?}"),
    }
}

/// Reads and verifies the fixed member key of a collapsed map-rep group-choice arm. A mismatch
/// returns `Err` (in the brute-force path this becomes try-the-next-variant). Under
/// `--preserve-encodings` it produces the `{var}_key_encoding` local consumed by the constructor.
fn push_map_choice_key_deser(
    body: &mut dyn CodeBlock,
    variant_var: &str,
    key: &FixedValue,
    cli: &Cli,
) {
    match key {
        FixedValue::Uint(x) => {
            if cli.preserve_encodings {
                body.line(&format!(
                    "let ({variant_var}_key, {variant_var}_key_encoding) = raw.unsigned_integer_sz()?;"
                ));
            } else {
                body.line(&format!("let {variant_var}_key = raw.unsigned_integer()?;"));
            }
            let mut cmp = Block::new(format!("if {variant_var}_key != {x}"));
            cmp.line(format!(
                "return Err(DeserializeFailure::FixedValueMismatch {{ found: Key::Uint({variant_var}_key), expected: Key::Uint({x}) }}.into());"
            ));
            body.push_block(cmp);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let {variant_var}_key_encoding = Some({variant_var}_key_encoding);"
                ));
            }
        }
        FixedValue::Text(s) => {
            let escaped = escape_rust_str(s);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let ({variant_var}_key, {variant_var}_key_encoding) = raw.text_sz()?;"
                ));
            } else {
                body.line(&format!("let {variant_var}_key = raw.text()?;"));
            }
            let mut cmp = Block::new(format!("if {variant_var}_key != \"{escaped}\""));
            cmp.line(format!(
                "return Err(DeserializeFailure::FixedValueMismatch {{ found: Key::Str({variant_var}_key), expected: Key::Str(String::from(\"{escaped}\")) }}.into());"
            ));
            body.push_block(cmp);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let {variant_var}_key_encoding = StringEncoding::from({variant_var}_key_encoding);"
                ));
            }
        }
        _ => panic!("unsupported map choice key type (only uint/text are supported): {key:?}"),
    }
}

/// Full deserialization body for a collapsed map-rep group-choice arm that carries a fixed key:
/// len-check, key read+verify, value read, final len-check, and the variant constructor. The map
/// holds exactly one pair (key + value), so the length check is `Fixed(1)`. Used by both enum
/// dispatch paths (type-match arm body / brute-force closure body).
fn make_keyed_map_variant_deser_code(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variant: &EnumVariant,
    key: &FixedValue,
    enum_gen_info: &EnumVariantInRust,
    cli: &Cli,
) -> DeserializationCode {
    let variant_var = variant.name_as_var();
    let ty = variant.rust_type();
    let var_names_str = if cli.preserve_encodings {
        encoding_var_names_str(types, &variant_var, ty, cli)
    } else {
        variant_var.clone()
    };
    // read + verify the fixed key
    let mut inner = DeserializationCode::default();
    push_map_choice_key_deser(&mut inner.content, &variant_var, key, cli);
    inner.throws = true;
    // Read the value. A fixed-value arm (`t = { a: 0 // b: tstr }`) has nothing to bind under the
    // default profile: `generate_deserialize`'s `Fixed` branch only READS AND VERIFIES the constant
    // and evaluates to no value, which is exactly what its `assert_eq!(before_after.before/after,
    // "")` pair states. Under `--preserve-encodings` the arm still owns encoding fields (the value's
    // own, plus the `{var}_key_encoding` pushed above), so there the binding is real and stays —
    // which is why that profile has always generated this shape.
    let (before, after) = if !cli.preserve_encodings && ty.is_fixed_value() {
        (Cow::from(""), "")
    } else {
        (Cow::from(format!("let {var_names_str} = ")), ";")
    };
    let value_code = gen_scope.generate_deserialize(
        types,
        ty.into(),
        DeserializeBeforeAfter::new(&before, after, false),
        DeserializeConfig::new(&variant_var),
        cli,
    );
    value_code.add_to_code(&mut inner);
    // Map holds a single pair — count of PAIRS is 1. We deliberately request the ARRAY-style
    // final len check here: for maps `add_deserialize_final_len_check` skips the ending-Break
    // consumption because record map deserializers are loops that consume the Break themselves —
    // but this keyed arm is straight-line code, so an indefinite map (`bf .. ff`) would otherwise
    // leave the trailing Break unread (spec-valid input then dies on "trailing data"). The Array
    // branch emits exactly the needed `match len { Len => (), Indefinite => expect Break }`, and
    // `Len`/`LenSz` are shared between array and map reads so the emitted code is rep-agnostic.
    let mut deser_code = surround_in_len_checks(
        inner,
        RustStructCBORLen::Fixed(1),
        Representation::Array,
        cli,
    );
    if enum_gen_info.names.is_empty() {
        // A field-less variant is constructed by naming it, not by calling it. This is the same
        // spelling the type-choice path uses for its own field-less arms; reached here only by a
        // fixed-value arm under the default profile (any encoding field would put a name in the
        // list), which is precisely the case whose value was not bound above.
        deser_code
            .content
            .line(&format!("Ok({}::{})", name, variant.name));
    } else if enum_gen_info.outer_vars == 0 {
        deser_code.content.line(&format!(
            "Ok({}::{}({}))",
            name, variant.name, var_names_str
        ));
    } else {
        enum_gen_info.generate_constructor(&mut deser_code.content, "Ok(", ")", None);
    }
    deser_code
}

// Generates a general enum e.g. Foo { A(A), B(B), C(C) } for types A, B, C
// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choicecs which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
#[allow(clippy::too_many_arguments)]
fn generate_enum(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    generate_deserialize_directly: bool,
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) {
    if cli.wasm {
        // also create a wasm-exposed enum just to distinguish the type
        let mut kind = codegen::Enum::new(format!("{name}Kind"));
        kind.vis("pub");
        for variant in variants.iter() {
            kind.new_variant(variant.name.to_string());
        }
        kind.attr("wasm_bindgen");
        gen_scope.wasm(types, name).push_enum(kind);
    }

    // rust enum containing the data
    let mut e = codegen::Enum::new(name.to_string());
    e.vis("pub");
    if let Some(doc) = config.doc.as_ref() {
        e.doc(doc);
    }
    let mut e_impl = codegen::Impl::new(name.to_string());
    // instead of using create_serialize_impls() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(
        &mut e,
        types.key_demand(name),
        true,
        /* cstyle_baseline */ false,
        config.custom_json,
        cli,
    );
    let mut ser_impl = make_serialization_impl(name.as_ref(), cli);
    let mut ser_func = make_serialization_function("serialize", cli);
    if let Some(tag) = tag {
        // TODO: how to even store these? (maybe it could be a new field in every enum variant)
        assert!(!cli.preserve_encodings);
        ser_func.line(format!("serializer.write_tag({tag}u64)?;"));
    }
    let mut ser_array_match_block = Block::new("match self");
    let mut deser_func = make_deserialization_function("deserialize", cli);
    let mut error_annotator = make_err_annotate_block(name.as_ref(), "", "");
    let deser_body: &mut dyn CodeBlock = if cli.annotate_fields {
        &mut error_annotator
    } else {
        &mut deser_func
    };
    let mut deser_impl = if generate_deserialize_directly {
        // this is handled in create_deseriaize_impls in the other case, and it MUST be handled there to ensure that
        // the tag check is done BEFORE reading the array/map CBOR
        generate_tag_check(deser_body, name, tag, cli.annotate_fields);
        let mut deser_impl = codegen::Impl::new(name.to_string());
        deser_impl.impl_trait("Deserialize");
        deser_impl
    } else {
        // this handles the tag check too
        let outer_encoding_var = if cli.preserve_encodings
            && variants
                .iter()
                .any(|variant| !variant.serialize_as_embedded_group)
        {
            Some("len_encoding")
        } else {
            None
        };
        let (deser_impl, _deser_embedded_impl) = create_deserialize_impls(
            name,
            rep,
            tag,
            None,
            false,
            outer_encoding_var,
            deser_body,
            cli.annotate_fields,
            cli,
        );
        deser_impl
    };
    // We avoid checking ALL variants if we can figure it out by instead checking the type.
    // This only works when the variants don't have first types in common.
    let mut non_overlapping_types_match = {
        let mut all_first_types = BTreeSet::new();
        let mut duplicates_or_unknown = false;
        for variant in variants.iter() {
            match variant.cbor_types_inner(types, rep) {
                Some(first_types) => {
                    for first_type in first_types.iter() {
                        // to_byte(0) is used since cbor_event::Type doesn't implement
                        // Ord or Hash so we can't put it in a set. Since we fix the lenth
                        // to always 0 this still remains a 1-to-1 mapping to Type.
                        if !all_first_types.insert(first_type.to_byte(0)) {
                            duplicates_or_unknown = true;
                        }
                    }
                }
                None => {
                    duplicates_or_unknown = true;
                    break;
                }
            }
        }
        if duplicates_or_unknown {
            None
        } else {
            let deser_covers_all_types = all_first_types.len() == 8;
            Some((Block::new("match raw.cbor_type()?"), deser_covers_all_types))
        }
    };
    // A bare `any` type-choice arm (conceptual `Any`, no encoding ops) accepts every CBOR item, so
    // its `cbor_types()` spans all 8 major types — it overlaps every other arm, forcing
    // `non_overlapping_types_match = None` above and thus the backtracking emitter. That forcing is
    // REQUIRED for correctness: the `cbor_type()`-dispatch form routes by wire type first, so a typed
    // arm that matches on type but fails on *content* (e.g. `uint .le 5` vs wire `6`) would error
    // inside that arm and never reach the catch-all `any`, violating CDDL's ordered type-choice
    // semantics (a later arm must be reachable when an earlier arm rejects on content). Assert it
    // loudly rather than silently miscompile if `Any::cbor_types` ever narrows.
    debug_assert!(
        non_overlapping_types_match.is_none()
            || !variants.iter().any(|v| matches!(
                &v.data,
                EnumVariantData::RustType(ty)
                    if ty.encodings.is_empty()
                        && matches!(
                            ty.conceptual_type.resolve_alias_shallow(),
                            ConceptualRustType::Any
                        )
            )),
        "an `any` choice arm must force the backtracking deserializer (never the \
         cbor_type()-dispatch form); `Any::cbor_types` must span all major types"
    );
    if non_overlapping_types_match.is_none() {
        deser_body
            .line("let initial_position = raw.position();")
            .line("let mut errs = Vec::new();");
    }
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
        let variant_var_name = variant.name_as_var();
        let mut v = codegen::Variant::new(variant.name.to_string());
        match enum_gen_info.names.len() {
            0 => {}
            1 if enum_gen_info.enc_fields.is_empty() => {
                v.tuple(&enum_gen_info.types[0]);
            }
            _ => {
                for (name_with_macros, type_str) in enum_gen_info
                    .names_with_macros(types.key_demand(name), config.custom_json, cli)
                    .into_iter()
                    .zip(enum_gen_info.types.iter())
                {
                    v.named(&name_with_macros, type_str);
                }
            }
        }
        if let Some(doc) = &variant.doc {
            // we must repurpose annotations since there is no doc support on enum variants
            v.annotation(format!("/// {doc}"));
        }
        // An `any`-typed choice arm renders its JSON NATURALLY (as the value it naturally is, not
        // through `AnyCbor`'s tagged codec; see `output_format.mdx` § "Natural rendering …"). A
        // newtype variant accepts variant-level `#[serde(with = …)]` / `#[schemars(schema_with = …)]`,
        // which serde/schemars apply to the variant's single field. The CBOR-only tag on a
        // tagged-`any` arm never appears in JSON, so the same natural routing is correct there too.
        if !config.custom_json
            && matches!(
                &variant.data,
                EnumVariantData::RustType(ty)
                    if matches!(
                        ty.conceptual_type.resolve_alias_shallow(),
                        ConceptualRustType::Any
                    )
            )
        {
            for annotation in
                super::natural_any_serde_annotations(cli, super::NaturalAnyPosition::Direct)
            {
                v.annotation(annotation);
            }
        }
        e.push_variant(v);
        // new (particularly useful if we have encoding variables)
        let mut new_func = codegen::Function::new(format!("new_{variant_var_name}"));
        new_func.vis("pub");
        if let Some(doc) = &variant.doc {
            new_func.doc(doc);
        }
        let mut output_comma = false;
        let (mut init_fields, can_fail) = match &variant.data {
            EnumVariantData::RustType(ty) => {
                // We only want to generate Variant::new() calls when we created a special struct
                // for the variant, which happens in the general case for multi-field group choices
                let fields = variant.group_ctor_record_fields(types, name);
                match rep.and(fields) {
                    Some(ctor_fields) => {
                        let can_fail = ctor_fields
                            .iter()
                            .any(|field| field.rust_type.has_value_bounds());
                        // bounds checking should be handled by the called constructor here
                        let mut ctor = format!("{}::new(", ty.conceptual_type.for_variant());
                        for field in ctor_fields {
                            if output_comma {
                                ctor.push_str(", ");
                            } else {
                                output_comma = true;
                            }
                            new_func.arg(&field.name, field.rust_type.for_rust_move(types, cli));
                            ctor.push_str(&field.name);
                        }
                        ctor.push(')');
                        if can_fail {
                            ctor.push('?');
                        }
                        (vec![ctor], can_fail)
                    }
                    None => {
                        if ty.is_fixed_value() {
                            (vec![], false)
                        } else {
                            // just directly pass in the variant's type
                            let field_name = variant.name_as_var();
                            new_func
                                .arg(&field_name, variant.rust_type().for_rust_move(types, cli));
                            if let Some(line) = value_bounds_check_line(ty, &field_name, true) {
                                new_func.line(&line);
                            }
                            (vec![field_name], ty.has_value_bounds())
                        }
                    }
                }
            }
            EnumVariantData::Inlined(record) => {
                let init_fields = record
                    .fields
                    .iter()
                    .filter(|field| !field.rust_type.is_fixed_value())
                    .map(|field| {
                        new_func.arg(
                            &field.name,
                            field.to_embedded_rust_type().for_rust_move(types, cli),
                        );
                        field.name.clone()
                    })
                    .collect();
                let can_fail = record.fields.iter().any(|field| {
                    let can_fail = field.rust_type.needs_bounds_check_if_inlined(types);
                    // a bounded named Rust wrapper checks at its own ctor (no inline check line, but
                    // still fallible via `?`); a primitive int/float field emits its check here.
                    if can_fail
                        && let Some(line) =
                            value_bounds_check_line(&field.rust_type, &field.name, true)
                    {
                        new_func.line(&line);
                    }
                    can_fail
                });
                (init_fields, can_fail)
            }
        };
        for enc_field in enum_gen_info.enc_fields.iter() {
            init_fields.push(enc_field.default_expr.to_owned());
        }
        let (ret_type, ctor_before, ctor_after) = if can_fail {
            ("Result<Self, DeserializeError>", "Ok(", ")")
        } else {
            ("Self", "", "")
        };
        new_func.ret(ret_type);
        enum_gen_info.generate_constructor(
            &mut new_func,
            ctor_before,
            ctor_after,
            Some(&init_fields),
        );
        e_impl.push_fn(new_func);

        // serialize
        if variant.serialize_as_embedded_group {
            assert_eq!(enum_gen_info.names.len(), 1);
            // we use serialize() instead of serialize_as_embedded_group() to count as the outer array tag here
            // to simplify things (the size logic is there already)
            ser_array_match_block.line(format!(
                "{}::{}({}) => {}.serialize(serializer{}),",
                name,
                variant.name,
                variant_var_name,
                variant_var_name,
                canonical_param(cli)
            ));
        } else {
            let mut case_block = Block::new(format!(
                "{}::{}{} =>",
                name,
                variant.name,
                enum_gen_info.capture_all()
            ));
            match &variant.data {
                EnumVariantData::RustType(ty) => {
                    if cli.preserve_encodings {
                        if let Some(r) = rep {
                            // group choice
                            let n = ty.expanded_field_count(types).expect(
                                "preserve-encodings=true not supported with varying-size group choice",
                            );
                            start_len(
                                &mut case_block,
                                r,
                                "serializer",
                                "len_encoding",
                                &n.to_string(),
                                cli,
                            );
                            // map-rep collapsed arm: write the fixed member key before the value
                            if r == Representation::Map
                                && let Some(key) = &variant.key
                            {
                                push_map_choice_key_ser(
                                    &mut case_block,
                                    &variant_var_name,
                                    key,
                                    cli,
                                );
                            }
                            gen_scope.generate_serialize(
                                types,
                                ty.into(),
                                &mut case_block,
                                SerializeConfig::new(&variant_var_name, &variant_var_name)
                                    .expr_is_ref(true)
                                    .encoding_var_is_ref(true),
                                cli,
                            );
                            end_len(&mut case_block, "serializer", "len_encoding", false, cli);
                            case_block.line("Ok(serializer)");
                        } else {
                            // type choice
                            gen_scope.generate_serialize(
                                types,
                                ty.into(),
                                &mut case_block,
                                SerializeConfig::new(&variant_var_name, &variant_var_name)
                                    .expr_is_ref(true)
                                    .is_end(true)
                                    .encoding_var_is_ref(true),
                                cli,
                            );
                        }
                    } else {
                        let write_break = match rep {
                            // group choice
                            Some(r) => {
                                let (len_str, indefinite) = match ty.expanded_field_count(types) {
                                    Some(n) => (cbor_event_len_n(&n.to_string(), cli), false),
                                    None => (String::from(cbor_event_len_indef(cli)), true),
                                };
                                let func_str = match r {
                                    Representation::Array => "write_array",
                                    Representation::Map => "write_map",
                                };
                                case_block.line(format!("serializer.{func_str}({len_str})?;"));
                                // map-rep collapsed arm: write the fixed member key before the value
                                if r == Representation::Map
                                    && let Some(key) = &variant.key
                                {
                                    push_map_choice_key_ser(
                                        &mut case_block,
                                        &variant_var_name,
                                        key,
                                        cli,
                                    );
                                }
                                indefinite
                            }
                            // type choice
                            None => false,
                        };
                        // TODO: only generate a block if the serialize is more than 1 line
                        // Problem: generate_serialize() works in terms of line() and push_block()
                        //          but we'd just want to inline the single one inside of a line...
                        gen_scope.generate_serialize(
                            types,
                            ty.into(),
                            &mut case_block,
                            SerializeConfig::new(&variant_var_name, &variant_var_name)
                                .expr_is_ref(true)
                                .is_end(!write_break),
                            cli,
                        );
                        if write_break {
                            case_block.line("serializer.write_special(cbor_event::Special::Break)");
                        }
                    }
                }
                EnumVariantData::Inlined(record) => {
                    start_len(
                        &mut case_block,
                        rep.expect("can't inline in type choices"),
                        "serializer",
                        "len_encoding",
                        &record.definite_info("", true, types, cli),
                        cli,
                    );
                    generate_array_struct_serialization(
                        gen_scope,
                        types,
                        record,
                        false,
                        &mut case_block,
                        cli,
                    );
                    end_len(&mut case_block, "serializer", "len_encoding", false, cli);
                    case_block.line("Ok(serializer)");
                }
            }
            case_block.after(",");
            ser_array_match_block.push_block(case_block);
        }
        // deserialize
        // TODO: how to detect when a greedy match won't work? (ie choice with choices in a choice possibly)
        match non_overlapping_types_match.as_mut() {
            Some((deser_type_match, _deser_covers_all_types)) => {
                let variant_deser_code = match &variant.data {
                    // map-rep collapsed arm with a fixed key: read+verify the key before the value
                    EnumVariantData::RustType(_)
                        if rep == Some(Representation::Map) && variant.key.is_some() =>
                    {
                        make_keyed_map_variant_deser_code(
                            gen_scope,
                            types,
                            name,
                            variant,
                            variant.key.as_ref().unwrap(),
                            &enum_gen_info,
                            cli,
                        )
                    }
                    EnumVariantData::RustType(ty) => {
                        let var_names_str = if cli.preserve_encodings {
                            encoding_var_names_str(types, &variant.name_as_var(), ty, cli)
                        } else {
                            variant.name_as_var()
                        };
                        // A fixed-value arm evaluates to no value in either representation — the
                        // `Fixed` deserialize branch reads and verifies the constant and binds
                        // nothing — so the exemption is a property of the arm's TYPE alone. It used
                        // to carry `|| rep.is_some()`, which forced a GROUP-choice arm to bind a
                        // value it has none of and tripped that branch's own empty-before/after
                        // assertion; the type-choice case took the exemption, which is why
                        // `t = 0 / tstr` generated while `t = [ 0 // tstr ]` aborted.
                        let (before, after) =
                            if cli.preserve_encodings || !variant.rust_type().is_fixed_value() {
                                (Cow::from(format!("let {var_names_str} = ")), ";")
                            } else {
                                (Cow::from(""), "")
                            };
                        let mut variant_deser_code = gen_scope.generate_deserialize(
                            types,
                            (variant.rust_type()).into(),
                            DeserializeBeforeAfter::new(&before, after, false),
                            DeserializeConfig::new(&variant.name_as_var()),
                            cli,
                        );
                        if let Some(r) = rep {
                            let len_info = match ty.conceptual_type.resolve_alias_shallow() {
                                ConceptualRustType::Rust(ident) if types.is_plain_group(ident) => {
                                    types.rust_struct(ident).unwrap().cbor_len_info(types)
                                }
                                _ => RustStructCBORLen::Fixed(1),
                            };
                            // this will never be 1 line so don't bother with the below cases
                            variant_deser_code =
                                surround_in_len_checks(variant_deser_code, len_info, r, cli);
                            if enum_gen_info.names.is_empty() {
                                // Field-less variant: name it, don't call it. Same spelling the
                                // `rep.is_none()` branch below uses for its own empty-names arm,
                                // reached here only by a fixed-value arm under the default profile.
                                variant_deser_code
                                    .content
                                    .line(&format!("Ok({}::{})", name, variant.name));
                            } else if enum_gen_info.outer_vars == 0 {
                                variant_deser_code.content.line(&format!(
                                    "Ok({}::{}({}))",
                                    name, variant.name, var_names_str
                                ));
                            } else {
                                enum_gen_info.generate_constructor(
                                    &mut variant_deser_code.content,
                                    "Ok(",
                                    ")",
                                    None,
                                );
                            }
                        } else {
                            // we can avoid this ugly block and directly do it as a line possibly
                            if variant_deser_code.content.as_single_line().is_some()
                                && enum_gen_info.names.len() == 1
                            {
                                variant_deser_code = gen_scope.generate_deserialize(
                                    types,
                                    (variant.rust_type()).into(),
                                    DeserializeBeforeAfter::new(
                                        &format!("Ok({}::{}(", name, variant.name),
                                        "))",
                                        false,
                                    ),
                                    DeserializeConfig::new(&variant.name_as_var()),
                                    cli,
                                );
                            } else if enum_gen_info.names.is_empty() {
                                variant_deser_code
                                    .content
                                    .line(&format!("Ok({}::{})", name, variant.name));
                            } else {
                                enum_gen_info.generate_constructor(
                                    &mut variant_deser_code.content,
                                    "Ok(",
                                    ")",
                                    None,
                                );
                            }
                        }
                        variant_deser_code
                    }
                    EnumVariantData::Inlined(record) => make_inline_deser_code(
                        gen_scope,
                        types,
                        name,
                        tag,
                        record,
                        &enum_gen_info,
                        cli,
                    ),
                };
                let cbor_types_str = variant
                    .cbor_types_inner(types, rep)
                    .expect("Already checked above")
                    .into_iter()
                    .map(cbor_type_code_str)
                    .collect::<Vec<_>>()
                    .join("|");
                match variant_deser_code.content.as_single_line() {
                    Some(single_line) => {
                        deser_type_match.line(format!("{cbor_types_str} => {single_line},"));
                    }
                    None => {
                        let mut match_arm = Block::new(format!("{cbor_types_str} =>"));
                        variant_deser_code.add_to(&mut match_arm);
                        deser_type_match.push_block(match_arm);
                    }
                }
            }
            None => {
                let mut return_if_deserialized = match &variant.data {
                    // map-rep collapsed arm with a fixed key: the closure reads+verifies the key
                    // then the value and returns the fully-constructed variant (like the Inlined
                    // path), so a key mismatch cleanly falls through to the next variant.
                    EnumVariantData::RustType(_)
                        if rep == Some(Representation::Map) && variant.key.is_some() =>
                    {
                        let variant_deser_code = make_keyed_map_variant_deser_code(
                            gen_scope,
                            types,
                            name,
                            variant,
                            variant.key.as_ref().unwrap(),
                            &enum_gen_info,
                            cli,
                        );
                        let mut variant_deser = Block::new(
                            "let variant_deser = (|raw: &mut Deserializer| -> Result<_, DeserializeError>",
                        );
                        variant_deser.after(")(raw);");
                        variant_deser.push_all(variant_deser_code.content);
                        deser_body.push_block(variant_deser);
                        let mut return_if_deserialized = Block::new("match variant_deser");
                        return_if_deserialized.line("Ok(variant) => return Ok(variant),");
                        return_if_deserialized
                    }
                    EnumVariantData::RustType(ty) => {
                        let mut return_if_deserialized = make_enum_variant_return_if_deserialized(
                            gen_scope,
                            types,
                            variant,
                            enum_gen_info.types.is_empty(),
                            rep.map(|r| {
                                let len_info = match ty.conceptual_type.resolve_alias_shallow() {
                                    ConceptualRustType::Rust(ident)
                                        if types.is_plain_group(ident) =>
                                    {
                                        types.rust_struct(ident).unwrap().cbor_len_info(types)
                                    }
                                    _ => RustStructCBORLen::Fixed(1),
                                };
                                (len_info, r)
                            }),
                            deser_body,
                            // this is the enum's OWN `deserialize` impl: `raw` is the fn parameter
                            "raw",
                            cli,
                        );
                        let names_without_outer = enum_gen_info.names_without_outer();
                        if names_without_outer.is_empty() {
                            return_if_deserialized
                                .line(format!("Ok(()) => return Ok({}::{}),", name, variant.name));
                        } else {
                            enum_gen_info.generate_constructor(
                                &mut return_if_deserialized,
                                &if names_without_outer.len() > 1 {
                                    format!(
                                        "Ok(({})) => return Ok(",
                                        names_without_outer.join(", ")
                                    )
                                } else {
                                    format!("Ok({}) => return Ok(", names_without_outer.join(", "))
                                },
                                "),",
                                None,
                            );
                        }
                        return_if_deserialized
                    }
                    EnumVariantData::Inlined(record) => {
                        let variant_deser_code = make_inline_deser_code(
                            gen_scope,
                            types,
                            name,
                            tag,
                            record,
                            &enum_gen_info,
                            cli,
                        );
                        let mut variant_deser = Block::new(
                            "let variant_deser = (|raw: &mut Deserializer| -> Result<_, DeserializeError>",
                        );
                        variant_deser.after(")(raw);");
                        variant_deser.push_all(variant_deser_code.content);
                        deser_body.push_block(variant_deser);
                        // can't chain blocks so we just put them one after the other
                        let mut return_if_deserialized = Block::new("match variant_deser");
                        return_if_deserialized.line("Ok(variant) => return Ok(variant),");
                        return_if_deserialized
                    }
                };
                let mut variant_deser_failed_block = Block::new("Err(e) =>");
                variant_deser_failed_block
                    .line(format!("errs.push(e.annotate(\"{}\"));", variant.name))
                    .line("raw.set_position(initial_position).unwrap();");
                return_if_deserialized.push_block(variant_deser_failed_block);
                return_if_deserialized.after(";");
                deser_body.push_block(return_if_deserialized);
            }
        }
    }
    ser_func.push_block(ser_array_match_block);
    ser_impl.push_fn(ser_func);
    match non_overlapping_types_match {
        Some((mut deser_type_match, deser_covers_all_types)) => {
            if !deser_covers_all_types {
                // Mirror the `annotated` switch (see `generate_tag_check`): when annotate_fields is
                // set, `deser_body` is the body of the `.annotate(name)` closure, so emit the
                // locationless form and let the closure supply the name (the name-carrying form
                // would get the name prepended AGAIN, reading "Name.Name").
                if cli.annotate_fields {
                    deser_type_match.line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
                } else {
                    deser_type_match.line(format!(
                        "_ => Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatched)),"
                    ));
                }
            }
            deser_body.push_block(deser_type_match);
        }
        None => {
            if cli.annotate_fields {
                deser_body.line("Err(DeserializeFailure::NoVariantMatchedWithCauses(errs).into())");
            } else {
                deser_body.line(&format!(
                    "Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatchedWithCauses(errs)))"
                ));
            }
        }
    }
    if cli.annotate_fields {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    gen_scope.rust(types, name).push_enum(e).push_impl(e_impl);
    gen_scope
        .rust_serialize(types, name)
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}
