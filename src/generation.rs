use codegen::{Block};
use cbor_event::Type as CBORType;
use std::collections::{BTreeMap, BTreeSet};

use crate::cli::CLI_ARGS;

use crate::intermediate::{
    AliasIdent,
    CDDLIdent,
    EnumVariant,
    FixedValue,
    IntermediateTypes,
    Representation,
    RustIdent,
    RustType,
    RustField,
    RustRecord,
    RustStructCBORLen,
    RustStructType,
    Primitive,
    ToWasmBoundaryOperations,
    VariantIdent,
};
use crate::utils::{
    convert_to_snake_case,
};

#[derive(Debug, Clone)]
struct SerializeConfig<'a> {
    expr: String,
    expr_is_ref: bool,
    var_name: String,
    is_end: bool,
    encoding_var_is_ref: bool,
    encoding_var_in_option_struct: Option<String>,
    serializer_name_overload: Option<(&'a str, bool)>,
}

impl<'a> SerializeConfig<'a> {
    fn new<S: Into<String>, T: Into<String>>(expr: S, var_name: T) -> Self {
        Self {
            expr: expr.into(),
            expr_is_ref: false,
            var_name: var_name.into(),
            is_end: false,
            encoding_var_is_ref: false,
            encoding_var_in_option_struct: None,
            serializer_name_overload: None,
        }
    }

    fn expr<S: Into<String>>(mut self, expr: S) -> Self {
        self.expr = expr.into();
        self
    }

    fn var_name<S: Into<String>>(mut self, var_name: S) -> Self {
        self.var_name = var_name.into();
        self
    }

    fn expr_is_ref(mut self, is_ref: bool) -> Self {
        self.expr_is_ref = is_ref;
        self
    }

    fn is_end(mut self, is_end: bool) -> Self {
        self.is_end = is_end;
        self
    }

    fn encoding_var_is_ref(mut self, is_ref: bool) -> Self {
        self.encoding_var_is_ref = is_ref;
        self
    }

    fn encoding_var_in_option_struct<S: Into<String>>(mut self, option_struct: S) -> Self {
        self.encoding_var_in_option_struct = Some(option_struct.into());
        self
    }

    fn encoding_var_no_option_struct(mut self) -> Self {
        self.encoding_var_in_option_struct = None;
        self
    }

    fn serializer_name_overload(mut self, overload: (&'a str, bool)) -> Self {
        self.serializer_name_overload = Some(overload);
        self
    }

    fn encoding_var(&self, child: Option<&str>) -> String {
        let child_suffix = match child {
            Some(c) => format!("_{}", c),
            None => "".to_owned(),
        };
        match &self.encoding_var_in_option_struct {
            Some(namespace) => format!(
                "{}.as_ref().map(|encs| encs.{}{}_encoding.clone()).unwrap_or_default()",
                namespace,
                self.var_name,
                child_suffix),
            None => format!("{}{}_encoding", self.var_name, child_suffix),
        }
    }

    /// for looking up encoding vars stored within a Vec<T> / Map<K, V> and declaring them as local variables
    fn container_encoding_lookup(&self, prefix: &str, encoding_fields: &Vec<EncodingField>, var: &str) -> String {
        let encoding_lookup = match &self.encoding_var_in_option_struct {
            Some(namespace) => format!(
                "{}.as_ref().and_then(|encs| encs.{}_{}_encodings.get({})).map(|e| e.clone())",
                namespace,
                self.var_name,
                prefix,
                var),
            None => format!(
                "{}_{}_encodings.get({}).map(|e| e.clone())",
                self.var_name,
                prefix,
                var),
        };
        format!(
            "let {} = {}.unwrap_or_else(|| {});",
            tuple_str(encoding_fields.iter().map(|enc| enc.field_name.clone()).collect()),
            encoding_lookup,
            tuple_str(encoding_fields.iter().map(|enc| enc.default_expr.to_owned()).collect()))
    }
}

pub struct GenerationScope {
    global_scope: codegen::Scope,
    serialize_scope: codegen::Scope,
    wasm_scope: codegen::Scope,
    cbor_encodings_scope: codegen::Scope,
    json_scope: codegen::Scope,
    already_generated: BTreeSet<RustIdent>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            global_scope: codegen::Scope::new(),
            serialize_scope: codegen::Scope::new(),
            wasm_scope: codegen::Scope::new(),
            cbor_encodings_scope: codegen::Scope::new(),
            json_scope: codegen::Scope::new(),
            already_generated: BTreeSet::new(),
            no_deser_reasons: BTreeMap::new(),
        }
    }

    pub fn generate(&mut self, types: &IntermediateTypes) {
        for (alias, (base_type, gen_rust_alias, gen_wasm_alias)) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if *gen_rust_alias {
                    self
                        .rust()
                        .raw(&format!("pub type {} = {};", ident, base_type.for_rust_member(false)));
                }
                if *gen_wasm_alias {
                    if let RustType::Fixed(constant) = base_type {
                        // wasm-bindgen doesn't support const or static vars so we must do a function
                        let (ty, val) = match constant {
                            FixedValue::Null => panic!("null constants not supported"),
                            FixedValue::Bool(b) => ("bool", b.to_string()),
                            FixedValue::Int(i) => ("i32", i.to_string()),
                            FixedValue::Uint(u) => ("u32", u.to_string()),
                            FixedValue::Text(s) => ("String", format!("\"{}\".to_owned()", s)),
                        };
                        self
                            .wasm()
                            .raw("#[wasm_bindgen]")
                            .new_fn(&convert_to_snake_case(&ident.to_string()))
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        self
                            .wasm()
                            .raw(&format!("pub type {} = {};", ident, base_type.for_wasm_member()));
                    }
                }
            }
        }
        let mut wasm_wrappers_generated = BTreeSet::new();
        for (rust_ident, rust_struct) in types.rust_structs() {
            assert_eq!(rust_ident, rust_struct.ident());
            if CLI_ARGS.wasm {
                rust_struct.visit_types(types, &mut |ty| {
                    match ty {
                        RustType::Array(elem) => {
                            if !ty.directly_wasm_exposable() {
                                let array_ident = elem.name_as_wasm_array();
                                if wasm_wrappers_generated.insert(array_ident.clone()) {
                                    self.generate_array_type(types, *elem.clone(), &RustIdent::new(CDDLIdent::new(array_ident)));
                                }
                            }
                        },
                        RustType::Map(k, v) => {
                            let map_ident = RustType::name_for_wasm_map(&k, &v);
                            if wasm_wrappers_generated.insert(map_ident.to_string()) {
                                codegen_table_type(self, types, &map_ident, *k.clone(), *v.clone(), None);
                            }
                            if !RustType::Array(Box::new(*k.clone())).directly_wasm_exposable() {
                                let keys_ident = k.name_as_wasm_array();
                                if wasm_wrappers_generated.insert(keys_ident.clone()) {
                                    self.generate_array_type(types, *k.clone(), &RustIdent::new(CDDLIdent::new(keys_ident)));
                                }
                            }
                        },
                        _ => (),
                    }
                });
            }
            match rust_struct.variant() {
                RustStructType::Record(record) => {
                    codegen_struct(self, types, rust_ident, rust_struct.tag(), record);
                },
                RustStructType::Table { domain, range } => {
                    if CLI_ARGS.wasm {
                        let map_ident = RustType::name_for_wasm_map(domain, range);
                        if wasm_wrappers_generated.insert(map_ident.to_string()) {
                            codegen_table_type(self, types, rust_ident, domain.clone(), range.clone(), rust_struct.tag());
                        }
                    }
                    //self
                    //    .rust()
                    //    .raw(&format!("type {} = {};", rust_struct.ident(), RustType::name_for_rust_map(domain, range, false)));
                },
                RustStructType::Array { element_type } => {
                    if CLI_ARGS.wasm {
                        self.generate_array_type(types, element_type.clone(), rust_ident);
                    }
                    //self
                    //    .rust()
                    //    .raw(&format!("type {} = {};", rust_struct.ident(), element_type.name_as_rust_array(false)));
                },
                RustStructType::TypeChoice { variants } => {
                    self.generate_type_choices_from_variants(types, rust_ident, variants, rust_struct.tag());
                },
                RustStructType::GroupChoice { variants, rep } => {
                    codegen_group_choices(self, types, rust_ident, variants, *rep, rust_struct.tag())
                },
                RustStructType::Wrapper{ wrapped, min_max } => {
                    match rust_struct.tag() {
                        Some(tag) => generate_wrapper_struct(self, types, rust_ident, &RustType::Tagged(tag, Box::new(wrapped.clone())), min_max.clone()),
                        None => generate_wrapper_struct(self, types, rust_ident, wrapped, min_max.clone()),
                    }
                },
                RustStructType::Prelude => {
                    match rust_ident.to_string().as_ref() {
                        "Int" => generate_int(self, types),
                        other => panic!("prelude not defined: {}", other),
                    }
                },
            }
        }
        if CLI_ARGS.json_schema_export {
            self.json_scope.raw("use cddl_lib_core::*;");
            let mut gen_json_schema = Block::new("macro_rules! gen_json_schema");
            let mut macro_match = Block::new("($name:ident) => ");
            macro_match
                .line("let dest_path = std::path::Path::new(&\"schemas\").join(&format!(\"{}.json\", stringify!($name)));")
                .line("std::fs::write(&dest_path, serde_json::to_string_pretty(&schemars::schema_for!($name)).unwrap()).unwrap();");
            gen_json_schema.push_block(macro_match);
            let mut main = codegen::Function::new("main");
            main.push_block(gen_json_schema);
            main.line("let schema_path = std::path::Path::new(&\"schemas\");");
            let mut path_exists = codegen::Block::new("if !schema_path.exists()");
            path_exists.line("std::fs::create_dir(schema_path).unwrap();");
            main.push_block(path_exists);
            for (rust_ident, rust_struct) in types.rust_structs() {
                let is_typedef = match rust_struct.variant() {
                    RustStructType::Array{ .. } => true,
                    RustStructType::Table{ .. } => true,
                    _ => false,
                };
                if !is_typedef {
                    main.line(format!("gen_json_schema!({});", rust_ident));
                }
            }
            self.json_scope.push_fn(main);
        }
    }

    pub fn rust(&mut self) -> &mut codegen::Scope {
        &mut self.global_scope
    }

    pub fn rust_serialize(&mut self) -> &mut codegen::Scope {
        &mut self.serialize_scope
    }

    pub fn wasm(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_scope
    }

    pub fn cbor_encodings(&mut self) -> &mut codegen::Scope {
        &mut self.cbor_encodings_scope
    }

    pub fn json(&mut self) -> &mut codegen::Scope {
        &mut self.json_scope
    }

    // is_end means the final line should evaluate to Ok(serializer), or equivalent ie dropping last ?; from line
    // expr - the name of the variable where this is accessed, e.g. "self.foo" or "field" (e.g. for if let Some(field) = self.foo)
    // var_name - For use in generating *unique* identifiers from this. Must be unique within a type, e.g. field name: for the above it would be "foo" for both
    // serializer_name_overload will provide an overload instead of using "serializer". (name, is_local) - if is_local then &mut will be appended when needed.
    fn generate_serialize(&mut self, types: &IntermediateTypes, rust_type: &RustType, body: &mut dyn CodeBlock, config: SerializeConfig) {
        //body.line(&format!("// DEBUG - generated from: {:?}", rust_type));
        let line_ender = if config.is_end {
            ""
        } else {
            "?;"
        };
        let expr_deref = if config.expr_is_ref {
            format!("*{}", config.expr)
        } else {
            config.expr.to_owned()
        };
        let expr_ref = if config.expr_is_ref {
            config.expr.to_owned()
        } else {
            format!("&{}", config.expr)
        };
        let (serializer_use, serializer_pass) = config.serializer_name_overload
            .map(|(name, is_local)| if is_local { (name, format!("&mut {}", name)) } else { (name, name.to_owned()) })
            .unwrap_or(("serializer", "serializer".to_owned()));
        let encoding_deref = if config.encoding_var_is_ref {
            "*"
        } else {
            ""
        };
        let encoding_var = config.encoding_var(None);
        let encoding_var_deref = format!("{}{}", encoding_deref, encoding_var);
        match rust_type {
            RustType::Fixed(value) => match value {
                FixedValue::Null => {
                    body.line(&format!("{}.write_special(CBORSpecial::Null){}", serializer_use, line_ender));
                },
                FixedValue::Bool(b) => {
                    body.line(&format!("{}.write_special(CBORSpecial::Bool({})){}", serializer_use, b, line_ender));
                },
                FixedValue::Uint(u) => {
                    let expr = format!("{}u64", u);
                    write_using_sz(body, "write_unsigned_integer", serializer_use, &expr, &expr, line_ender, &encoding_var_deref);
                },
                // TODO: should this be Nint instead of Int? CDDL spec is Nint but cddl lib is Int
                FixedValue::Int(i) => {
                    write_using_sz(body, "write_negative_integer", serializer_use, &i.to_string(), &format!("({} + 1).abs() as u64", i), line_ender, &encoding_var_deref);
                },
                FixedValue::Text(s) => {
                    write_string_sz(body, "write_text", serializer_use, &format!("\"{}\"", s), line_ender, &encoding_var);
                },
            },
            RustType::Primitive(primitive) => match primitive {
                Primitive::Bool => {
                    body.line(&format!("{}.write_special(cbor_event::Special::Bool({})){}", serializer_use, expr_deref, line_ender));
                },
                Primitive::Bytes => {
                    write_string_sz(body, "write_bytes", serializer_use, &config.expr, line_ender, &encoding_var);
                },
                Primitive::Str => {
                    write_string_sz(body, "write_text", serializer_use, &config.expr, line_ender, &encoding_var);
                },
                Primitive::I8 |
                Primitive::I16 |
                Primitive::I32 |
                Primitive::I64 => {
                    let mut pos = Block::new(&format!("if {} >= 0", expr_deref));
                    let expr_pos = format!("{} as u64", expr_deref);
                    write_using_sz(&mut pos, "write_unsigned_integer", serializer_use, &expr_pos, &expr_pos, line_ender, &encoding_var_deref);
                    body.push_block(pos);
                    let mut neg = Block::new("else");
                    // only the _sz variants support i128, the other endpoint is i64
                    let expr = if CLI_ARGS.preserve_encodings {
                        format!("{} as i128", expr_deref)
                    } else {
                        format!("{} as i64", expr_deref)
                    };
                    write_using_sz(&mut neg, "write_negative_integer", serializer_use, &expr, &format!("({} + 1).abs() as u64", expr_deref), line_ender, &encoding_var_deref);
                    body.push_block(neg);
                },
                Primitive::U8 |
                Primitive::U16 |
                Primitive::U32 => {
                    let expr = format!("{} as u64", expr_deref);
                    write_using_sz(body, "write_unsigned_integer", serializer_use, &expr, &expr, line_ender, &encoding_var_deref);
                },
                Primitive::U64 => {
                    write_using_sz(body, "write_unsigned_integer", serializer_use, &expr_deref, &expr_deref, line_ender, &encoding_var_deref);
                },
                Primitive::N64 => {
                    // only the _sz variants support i128, the other endpoint is i64
                    let expr = if CLI_ARGS.preserve_encodings {
                        format!("{} as i128", expr_deref)
                    } else {
                        format!("{} as i64", expr_deref)
                    };
                    write_using_sz(body, "write_negative_integer", serializer_use, &expr, &format!("({} + 1).abs() as u64", expr_deref), line_ender, &encoding_var_deref);
                },
            },
            RustType::Rust(t) => {
                if types.is_plain_group(t) {
                    body.line(&format!("{}.serialize_as_embedded_group({}{}){}", config.expr, serializer_pass, canonical_param(), line_ender));
                } else {
                    body.line(&format!("{}.serialize({}{}){}", config.expr, serializer_pass, canonical_param(), line_ender));
                }
            },
            RustType::Array(ty) => {
                start_len(body, Representation::Array, serializer_use, &encoding_var, &format!("{}.len() as u64", config.expr));
                let elem_var_name = format!("{}_elem", config.var_name);
                let elem_encs = if CLI_ARGS.preserve_encodings {
                    encoding_fields(&elem_var_name, &ty.clone().resolve_aliases())
                } else {
                    vec![]
                };
                let mut loop_block = if !elem_encs.is_empty() {
                    let mut block = Block::new(&format!("for (i, element) in {}.iter().enumerate()", config.expr));
                    block.line(config.container_encoding_lookup("elem", &elem_encs, "i"));
                    block
                } else {
                    Block::new(&format!("for element in {}.iter()", config.expr))
                };
                let elem_config = config
                    .clone()
                    .expr("element")
                    .expr_is_ref(true)
                    .var_name(elem_var_name.clone())
                    .is_end(false)
                    .encoding_var_no_option_struct()
                    .encoding_var_is_ref(false);
                self.generate_serialize(types, ty, &mut loop_block, elem_config);
                body.push_block(loop_block);
                end_len(body, serializer_use, &encoding_var, config.is_end);
            },
            RustType::Map(key, value) => {
                start_len(body, Representation::Map, serializer_use, &encoding_var, &format!("{}.len() as u64", config.expr));
                let ser_loop = if CLI_ARGS.preserve_encodings {
                    let key_enc_fields = encoding_fields(&format!("{}_key", config.var_name), &key.clone().resolve_aliases());
                    let value_enc_fields = encoding_fields(&format!("{}_value", config.var_name), &value.clone().resolve_aliases());
                    let mut ser_loop = if CLI_ARGS.canonical_form {
                        let mut key_order = codegen::Block::new(&format!("let mut key_order = {}.iter().map(|(k, v)|", config.expr));
                        key_order
                            .line("let mut buf = cbor_event::se::Serializer::new_vec();");
                        if !key_enc_fields.is_empty() {
                            key_order.line(config.container_encoding_lookup("key", &key_enc_fields, "k"));
                        }
                        let key_config = SerializeConfig::new("k", format!("{}_key", config.var_name))
                            .expr_is_ref(true)
                            .is_end(false)
                            .serializer_name_overload(("buf", true))
                            .encoding_var_is_ref(false);
                        self.generate_serialize(types, key, &mut key_order, key_config);
                        key_order
                            .line("Ok((buf.finalize(), k, v))")
                            .after(").collect::<Result<Vec<(Vec<u8>, &_, &_)>, cbor_event::Error>>()?;");
                        body.push_block(key_order);
                        let mut key_order_if = codegen::Block::new("if force_canonical");
                        let mut key_order_sort = codegen::Block::new("key_order.sort_by(|(lhs_bytes, _, _), (rhs_bytes, _, _)|");
                        let mut key_order_sort_match = codegen::Block::new("match lhs_bytes.len().cmp(&rhs_bytes.len())");
                        key_order_sort_match
                            .line("std::cmp::Ordering::Equal => lhs_bytes.cmp(&rhs_bytes),")
                            .line("diff_ord => diff_ord,");
                        key_order_sort
                            .push_block(key_order_sort_match)
                            .after(");");
                        key_order_if.push_block(key_order_sort);
                        body.push_block(key_order_if);
                
                        let mut ser_loop = Block::new("for (key_bytes, key, value) in key_order");
                        ser_loop.line(format!("{}.write_raw_bytes(&key_bytes)?;", serializer_use));
                        ser_loop
                    } else {
                        let mut ser_loop = Block::new(&format!("for (key, value) in {}.iter()", config.expr));
                        if !key_enc_fields.is_empty() {
                            ser_loop.line(config.container_encoding_lookup("key", &key_enc_fields, "key"));
                        }
                        let key_config = config
                            .clone()
                            .expr("key")
                            .expr_is_ref(true)
                            .var_name(format!("{}_key", config.var_name))
                            .is_end(false)
                            .encoding_var_no_option_struct()
                            .encoding_var_is_ref(false);
                        self.generate_serialize(types, key, &mut ser_loop, key_config);
                        ser_loop
                    };
                    if !value_enc_fields.is_empty() {
                        ser_loop.line(config.container_encoding_lookup("value", &value_enc_fields, "key"));
                    }
                    let value_config = config
                            .clone()
                            .expr("value")
                            .expr_is_ref(true)
                            .var_name(format!("{}_value", config.var_name))
                            .is_end(false)
                            .encoding_var_no_option_struct()
                            .encoding_var_is_ref(false);
                    self.generate_serialize(types, value, &mut ser_loop, value_config);
                    ser_loop
                } else {
                    let mut ser_loop = Block::new(&format!("for (key, value) in {}.iter()", config.expr));
                    let key_config = config
                            .clone()
                            .expr("key")
                            .expr_is_ref(true)
                            .var_name(format!("{}_key", config.var_name))
                            .is_end(false)
                            .encoding_var_no_option_struct()
                            .encoding_var_is_ref(false);
                    let value_config = key_config
                        .clone()
                        .expr("value")
                        .var_name(format!("{}_value", config.var_name));
                    self.generate_serialize(types, key, &mut ser_loop, key_config);
                    self.generate_serialize(types, value, &mut ser_loop, value_config);
                    ser_loop
                };
                body.push_block(ser_loop);
                end_len(body, serializer_use, &encoding_var, config.is_end);
            },
            RustType::Tagged(tag, ty) => {
                let expr = format!("{}u64", tag);
                write_using_sz(body, "write_tag", serializer_use, &expr, &expr, "?;", &format!("{}{}", encoding_deref, config.encoding_var(Some("tag"))));
                self.generate_serialize(types, ty, body, config);
            },
            RustType::Optional(ty) => {
                let mut opt_block = Block::new(&format!("match {}", expr_ref));
                // TODO: do this in one line without a block if possible somehow.
                //       see other comment in generate_enum()
                let mut some_block = Block::new("Some(x) =>");
                let opt_config = config
                    .clone()
                    .expr("x")
                    .expr_is_ref(true)
                    .is_end(true);
                self.generate_serialize(types, ty, &mut some_block, opt_config);
                some_block.after(",");
                opt_block.push_block(some_block);
                opt_block.line(&format!("None => {}.write_special(CBORSpecial::Null),", serializer_use));
                if !config.is_end {
                    opt_block.after("?;");
                }
                body.push_block(opt_block);
            },
            RustType::Alias(_ident, ty) => self.generate_serialize(types, ty, body, config),
            RustType::CBORBytes(ty) => {
                let inner_se = format!("{}_inner_se", config.var_name);
                body.line(&format!("let mut {} = Serializer::new_vec();", inner_se));
                let inner_config = config
                    .clone()
                    .is_end(false)
                    .serializer_name_overload((&inner_se, true));
                self.generate_serialize(types, ty, body, inner_config);
                body.line(&format!("let {}_bytes = {}.finalize();", config.var_name, inner_se));
                write_string_sz(body, "write_bytes", serializer_use, &format!("{}_bytes", config.var_name), line_ender, &config.encoding_var(Some("bytes")));
            },
        };
    }

    // formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
    // * {let x = }{<value>}{;} - creation of variables
    // * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
    // * {}{<value>}{} - for last-expression eval in blocks
    // * etc
    // var_name is passed in for use in creating unique identifiers for temporaries
    // if force_non_embedded always deserialize as the outer wrapper, not as the embedded plain group when the Rust ident is for a plain group
    // final_exprs contains what other expressions to return as part of the final tuple e.g. (x, x_encoding, x_key_encodings)
    fn generate_deserialize(&mut self, types: &IntermediateTypes, rust_type: &RustType, var_name: &str, before: &str, after: &str, in_embedded: bool, optional_field: bool, mut final_exprs: Vec<String>, body: &mut dyn CodeBlock, deserializer_name_overload: Option<&str>) {
        let deserializer_name = deserializer_name_overload.unwrap_or("raw");
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        if !CLI_ARGS.preserve_encodings {
            assert!(final_exprs.is_empty());
        }
        let final_expr = |mut encoding_exprs: Vec<String>, actual_value: Option<String>| {
            if let Some(e) = actual_value {
                // possibly less efficient but more concise
                encoding_exprs.insert(0, e);
            }
            if encoding_exprs.len() > 1 {
                format!("({})", encoding_exprs.join(", "))
            } else {
                encoding_exprs.join(", ")
            }
        };
        match rust_type {
            RustType::Fixed(f) => {
                if !CLI_ARGS.preserve_encodings {
                    // we don't evaluate to any values here, just verify
                    // before/after are ignored and we need to handle fixed value deserialization in a different way
                    // than normal ones.
                    assert_eq!(after, "");
                    assert_eq!(before, "");
                }
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                match f {
                    FixedValue::Null => {
                        let mut special_block = Block::new(&format!("if {}.special()? != CBORSpecial::Null", deserializer_name));
                        special_block.line("return Err(DeserializeFailure::ExpectedNull.into());");
                        body.push_block(special_block);
                        if CLI_ARGS.preserve_encodings {
                            body.line(&format!("{}{}{}", before, final_expr(final_exprs, None), after));
                        }
                    },
                    FixedValue::Uint(x) => {
                        if CLI_ARGS.preserve_encodings {
                            body.line(&format!("let ({}_value, {}_encoding) = {}.unsigned_integer_sz()?;", var_name, var_name, deserializer_name));
                        } else {
                            body.line(&format!("let {}_value = {}.unsigned_integer()?;", var_name, deserializer_name));
                        }
                        let mut compare_block = Block::new(&format!("if {}_value != {}", var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint({}_value), expected: Key::Uint({}) }}.into());", var_name, x));
                        body.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            final_exprs.push(format!("Some({}_encoding)", var_name));
                            body.line(&format!("{}{}{}", before, final_expr(final_exprs, None), after));
                            //body.line(&format!("{}{}{}_encoding{}{}", before, sp, var_name, ep, after));
                        }
                    },
                    FixedValue::Text(x) => {
                        if CLI_ARGS.preserve_encodings {
                            body.line(&format!("let ({}_value, {}_encoding) = {}.text_sz()?;", var_name, var_name, deserializer_name));
                        } else {
                            body.line(&format!("let {}_value = {}.text()?;", var_name, deserializer_name));
                        }
                        let mut compare_block = Block::new(&format!("if {}_value != \"{}\"", var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Str({}_value), expected: Key::Str(String::from(\"{}\")) }}.into());", var_name, x));
                        body.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            final_exprs.push(format!("StringEncoding::from({}_encoding)", var_name));
                            body.line(&format!("{}{}{}", before, final_expr(final_exprs, None), after));
                        }
                    },
                    _ => unimplemented!(),
                };
            },
            RustType::Primitive(p) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                let sz_str = if CLI_ARGS.preserve_encodings {
                    "_sz"
                } else {
                    ""
                };
                let mut deser_primitive = |mut final_exprs: Vec<String>, func: &str, x: &str, x_expr: &str| if CLI_ARGS.preserve_encodings {
                    let enc_expr = match func {
                        "text" | "bytes" => "StringEncoding::from(enc)",
                        _ => "Some(enc)",
                    };
                    final_exprs.push(enc_expr.to_owned());
                    body.line(
                        &format!(
                            "{}{}.{}_sz().map(|({}, enc)| {})?{}",
                            before,
                            deserializer_name,
                            func,
                            x,
                            final_expr(final_exprs, Some(x_expr.to_owned())),
                            after));
                } else {
                    body.line(&format!("{}{}.{}()? as {}{}", before, deserializer_name, func, p.to_string(), after));
                };
                match p {
                    Primitive::Bytes => deser_primitive(final_exprs, "bytes", "bytes", "bytes"),
                    Primitive::U8  | Primitive::U16 | Primitive::U32 => deser_primitive(final_exprs, "unsigned_integer", "x", &format!("x as {}", p.to_string())),
                    Primitive::U64 => deser_primitive(final_exprs, "unsigned_integer", "x", "x"),
                    Primitive::I8 |
                    Primitive::I16 |
                    Primitive::I32 |
                    Primitive::I64 => {
                        let mut type_check = Block::new(&format!("{}match {}.cbor_type()?", before, deserializer_name));
                        if CLI_ARGS.preserve_encodings {
                            let mut pos = Block::new("cbor_event::Type::UnsignedInteger =>");
                            pos
                                .line(&format!("let (x, enc) = {}.unsigned_integer_sz()?;", deserializer_name))
                                .line(format!("(x as {}, Some(enc))", p.to_string()))
                                .after(",");
                            type_check.push_block(pos);
                            // let this cover both the negative int case + error case
                            let mut neg = Block::new("_ =>");
                            neg
                                .line(&format!("let (x, enc) = {}.negative_integer_sz()?;", deserializer_name))
                                .line(format!("(x as {}, Some(enc))", p.to_string()))
                                .after(",");    
                            type_check.push_block(neg);
                        } else {
                            type_check
                                .line(format!("cbor_event::Type::UnsignedInteger => {}.unsigned_integer()? as {},", deserializer_name, p.to_string()))
                                .line(format!("_ => {}.negative_integer()? as {},", deserializer_name, p.to_string()));
                        }
                        type_check.after(after);
                        body.push_block(type_check);
                    },
                    Primitive::N64 => deser_primitive(final_exprs, "negative_integer", "x", "x as i128"),
                    Primitive::Str => deser_primitive(final_exprs, "text", "s", "s"),
                    Primitive::Bool => {
                        // no encoding differences for bool
                        body.line(&format!("{}{}{}", before, final_expr(final_exprs, Some("bool::deserialize(raw)?".to_owned())), after));
                    },
                };
            },
            RustType::Rust(ident) => if types.is_plain_group(ident) {
                // This would mess up with length checks otherwise and is probably not a likely situation if this is even valid in CDDL.
                // To have this work (if it's valid) you'd either need to generate 2 embedded deserialize methods or pass
                // a parameter whether it was an optional field, and if so, read_len.read_elems(embedded mandatory fields)?;
                // since otherwise it'd only length check the optional fields within the type.
                assert!(!optional_field);
                let pass_read_len = if in_embedded {
                    "read_len"
                } else {
                    "&mut read_len"
                };
                body.line(&format!("{}{}{}", before, final_expr(final_exprs, Some(format!("{}::deserialize_as_embedded_group({}, {}, len)?", ident, deserializer_name, pass_read_len))), after));
            } else {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                body.line(&format!("{}{}{}", before, final_expr(final_exprs, Some(format!("{}::deserialize({})?", ident, deserializer_name))), after));
            },
            RustType::Tagged(tag, ty) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                let mut tag_check = if CLI_ARGS.preserve_encodings {
                    let mut tag_check = Block::new(&format!("{}match {}.tag_sz()?", before, deserializer_name));
                    let mut deser_block = Block::new(&format!("({}, tag_enc) =>", tag));
                    final_exprs.push("Some(tag_enc)".to_owned());
                    self.generate_deserialize(types, ty, var_name, "", "", in_embedded, false, final_exprs, &mut deser_block, deserializer_name_overload);
                    deser_block.after(",");
                    tag_check.push_block(deser_block);
                    tag_check
                } else {
                    let mut tag_check = Block::new(&format!("{}match {}.tag()?", before, deserializer_name));
                    let mut deser_block = Block::new(&format!("{} =>", tag));
                    self.generate_deserialize(types, ty, var_name, "", "", in_embedded, false, final_exprs, &mut deser_block, deserializer_name_overload);
                    deser_block.after(",");
                    tag_check.push_block(deser_block);
                    tag_check
                };
                tag_check.line(&format!("{} => return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),", if CLI_ARGS.preserve_encodings { "(tag, _enc)" } else { "tag" }, tag));
                tag_check.after(after);
                body.push_block(tag_check);
            },
            RustType::Optional(ty) => {
                let read_len_check = optional_field || (ty.expanded_field_count(types) != Some(1));
                // codegen crate doesn't support if/else or appending a block after a block, only strings
                // so we need to create a local bool var and use a match instead
                let if_label = if ty.cbor_types().contains(&CBORType::Special) {
                    let is_some_check_var = format!("{}_is_some", var_name);
                    let mut is_some_check = Block::new(&format!("let {} = match cbor_type()?", is_some_check_var));
                    let mut special_block = Block::new("CBORType::Special =>");
                    special_block.line(&format!("let special = {}.special()?;", deserializer_name));
                    special_block.line(&format!("{}.as_mut_ref().seek(SeekFrom::Current(-1)).unwrap();", deserializer_name));
                    let mut special_match = Block::new("match special");
                    // TODO: we need to check that we don't have null / null somewhere
                    special_match.line("CBORSpecial::Null => false,");
                    // no need to error check - would happen in generated deserialize code
                    special_match.line("_ => true,");
                    special_block.push_block(special_match);
                    special_block.after(",");
                    is_some_check.push_block(special_block);
                    // it's possible the Some case only has Special as its starting tag(s),
                    // but we don't care since it'll fail in either either case anyway,
                    // and would give a good enough error (ie expected Special::X but found non-Special)
                    is_some_check.line("_ => true,");
                    is_some_check.after(";");
                    body.push_block(is_some_check);
                    is_some_check_var
                } else {
                    String::from(&format!("{}.cbor_type()? != CBORType::Special", deserializer_name))
                };
                let mut deser_block = Block::new(&format!("{}match {}", before, if_label));
                let mut some_block = Block::new("true =>");
                if read_len_check {
                    let mandatory_fields = ty.expanded_mandatory_field_count(types);
                    if mandatory_fields != 0 {
                        some_block.line(format!("read_len.read_elems({})?;", mandatory_fields));
                    }
                }
                let ty_enc_fields = if CLI_ARGS.preserve_encodings {
                    encoding_fields(var_name, &ty.clone().resolve_aliases())
                } else {
                    vec![]
                };
                if ty_enc_fields.is_empty() {
                    self.generate_deserialize(types, ty, var_name, "Some(", ")", in_embedded, false, final_exprs, &mut some_block, deserializer_name_overload);
                } else {
                    let (map_some_before, map_some_after) = if ty.is_fixed_value() {
                        // case 1: no actual return, only encoding values for tags/fixed values, no need to wrap in Some()
                        ("", "".to_owned())
                    } else {
                        // case 2: need to map FIRST element in Some(x)
                        let enc_vars_str = ty_enc_fields
                            .iter()
                            .map(|enc_field| enc_field.field_name.clone())
                            .collect::<Vec<String>>()
                            .join(", ");
                        // we need to annotate the Ok's error type since the compiler gets confused otherwise
                        ("Result::<_, DeserializeError>::Ok(", format!(").map(|(x, {})| (Some(x), {}))?", enc_vars_str, enc_vars_str))
                    };
                    self.generate_deserialize(types, ty, var_name, map_some_before, &map_some_after, in_embedded, false, final_exprs, &mut some_block, deserializer_name_overload);
                }
                some_block.after(",");
                deser_block.push_block(some_block);
                let mut none_block = Block::new("false =>");
                if read_len_check {
                    none_block.line("read_len.read_elems(1)?;");
                }
                // we don't use this to avoid the new (true) if CLI_ARGS.preserve_encodings is set
                //self.generate_deserialize(types, &RustType::Fixed(FixedValue::Null), var_name, "", "", in_embedded, false, add_parens, &mut none_block);
                let mut check_null = Block::new(&format!("if {}.special()? != CBORSpecial::Null", deserializer_name));
                check_null.line("return Err(DeserializeFailure::ExpectedNull.into());");
                none_block.push_block(check_null);
                if CLI_ARGS.preserve_encodings {
                    let mut none_elems = if ty.is_fixed_value() {
                        vec![]
                    } else {
                        vec!["None".to_owned()]
                    };
                    none_elems.extend(ty_enc_fields.iter().map(|enc_field| enc_field.default_expr.to_owned()));
                    match none_elems.len() {
                        // this probably isn't properly supported by other parts of code and is so unlikely to be encountered
                        // that we really don't care right now. if you run into this open an issue and it can be investigated
                        0 => unimplemented!("please open a github issue"),
                        1 => none_block.line(none_elems.first().unwrap()),
                        _ => none_block.line(format!("({})", none_elems.join(", "))),
                    };
                } else {
                    none_block.line("None");
                }
                deser_block.after(after);
                deser_block.push_block(none_block);
                body.push_block(deser_block);
            },
            RustType::Array(ty) => {
                //if !self.deserialize_generated_for_type(&element_type) {
                    // TODO: check this elsehwere???
                    //self.dont_generate_deserialize(&array_type_ident, format!("inner type {} doesn't support deserialize", element_type.for_rust_member()));
                //}
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                let arr_var_name = format!("{}_arr", var_name);
                body.line(&format!("let mut {} = Vec::new();", arr_var_name));
                let elem_var_name = format!("{}_elem", var_name);
                let elem_encs = if CLI_ARGS.preserve_encodings {
                    encoding_fields(&elem_var_name, &ty.clone().resolve_aliases())
                } else {
                    vec![]
                };
                if CLI_ARGS.preserve_encodings {
                    body
                        .line(&format!("let len = {}.array_sz()?;", deserializer_name))
                        .line(&format!("let {}_encoding = len.into();", var_name));
                    if !elem_encs.is_empty() {
                        body.line(&format!("let mut {}_elem_encodings = Vec::new();", var_name));
                    }
                } else {
                    body.line(&format!("let len = {}.array()?;", deserializer_name));
                }
                let mut deser_loop = make_deser_loop("len", &format!("{}.len()", arr_var_name));
                deser_loop.push_block(make_deser_loop_break_check());
                if let RustType::Rust(ty_ident) = &**ty {
                    // TODO: properly handle which read_len would be checked here.
                    assert!(!types.is_plain_group(&*ty_ident));
                }
                if !elem_encs.is_empty() {
                    let elem_var_names_str = encoding_var_names_str(&elem_var_name, ty);
                    self.generate_deserialize(types, ty, &elem_var_name, &format!("let {} = ", elem_var_names_str), ";", in_embedded, false, vec![], &mut deser_loop, deserializer_name_overload);
                    deser_loop
                        .line(format!("{}.push({});", arr_var_name, elem_var_name))
                        .line(format!(
                            "{}_elem_encodings.push({});",
                            var_name,
                            tuple_str(elem_encs.iter().map(|enc| enc.field_name.clone()).collect())));
                } else {
                    self.generate_deserialize(types, ty, &elem_var_name, &format!("{}.push(", arr_var_name), ");", in_embedded, false, vec![], &mut deser_loop, deserializer_name_overload);
                }
                body.push_block(deser_loop);
                if CLI_ARGS.preserve_encodings {
                    final_exprs.push(format!("{}_encoding", var_name));
                    if !elem_encs.is_empty() {
                        final_exprs.push(format!("{}_elem_encodings", var_name));
                    }
                    body.line(&format!("{}{}{}", before, final_expr(final_exprs, Some(arr_var_name)), after));
                } else {
                    body.line(&format!("{}{}{}", before, arr_var_name, after));
                }
            },
            RustType::Map(key_type, value_type) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                if !self.deserialize_generated_for_type(key_type) {
                    todo!();
                    // TODO: where is the best place to check for this? should we pass in a RustIdent to say where we're generating?!
                    //self.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_rust_member()));
                } else if !self.deserialize_generated_for_type(&value_type) {
                    todo!();
                    //self.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_rust_member()));
                } else {
                    let table_var = format!("{}_table", var_name);
                    body.line(&format!("let mut {} = {}::new();", table_var, table_type()));
                    let key_var_name = format!("{}_key", var_name);
                    let value_var_name = format!("{}_value", var_name);
                    let key_encs = if CLI_ARGS.preserve_encodings {
                        encoding_fields(&key_var_name, &key_type.clone().resolve_aliases())
                    } else {
                        vec![]
                    };
                    let value_encs = if CLI_ARGS.preserve_encodings {
                        encoding_fields(&value_var_name, &value_type.clone().resolve_aliases())
                    } else {
                        vec![]
                    };
                    let len_var = format!("{}_len", var_name);
                    if CLI_ARGS.preserve_encodings {
                        body
                            .line(&format!("let {} = {}.map_sz()?;", len_var, deserializer_name))
                            .line(&format!("let {}_encoding = {}.into();", var_name, len_var));
                        if !key_encs.is_empty() {
                            body.line(&format!("let mut {}_key_encodings = BTreeMap::new();", var_name));
                        }
                        if !value_encs.is_empty() {
                            body.line(&format!("let mut {}_value_encodings = BTreeMap::new();", var_name));
                        }
                    } else {
                        body.line(&format!("let {} = {}.map()?;", len_var, deserializer_name));
                    }
                    let mut deser_loop = make_deser_loop(&len_var, &format!("{}.len()", table_var));
                    deser_loop.push_block(make_deser_loop_break_check());
                    if CLI_ARGS.preserve_encodings {
                        let key_var_names_str = encoding_var_names_str(&key_var_name, key_type);
                        let value_var_names_str = encoding_var_names_str(&value_var_name, value_type);
                        self.generate_deserialize(types, key_type, &key_var_name, &format!("let {} = ", key_var_names_str), ";", false, false, vec![], &mut deser_loop, deserializer_name_overload);
                        self.generate_deserialize(types, value_type, &value_var_name, &format!("let {} = ", value_var_names_str), ";", false, false, vec![], &mut deser_loop, deserializer_name_overload);
                    } else {
                        self.generate_deserialize(types, key_type, &key_var_name, &format!("let {} = ", key_var_name), ";", false, false, vec![], &mut deser_loop, deserializer_name_overload);
                        self.generate_deserialize(types, value_type, &value_var_name, &format!("let {} = ", value_var_name), ";", false, false, vec![], &mut deser_loop, deserializer_name_overload);
                    }
                    let mut dup_check = Block::new(&format!("if {}.insert({}.clone(), {}).is_some()", table_var, key_var_name, value_var_name));
                    let dup_key_error_key = match &**key_type {
                        RustType::Primitive(Primitive::U8) |
                        RustType::Primitive(Primitive::U16) |
                        RustType::Primitive(Primitive::U32) |
                        RustType::Primitive(Primitive::U64) => format!("Key::Uint({}.into())", key_var_name),
                        RustType::Primitive(Primitive::Str) => format!("Key::Str({})", key_var_name),
                        // TODO: make a generic one then store serialized CBOR?
                        _ => "Key::Str(String::from(\"some complicated/unsupported type\"))".to_owned(),
                    };
                    dup_check.line(format!("return Err(DeserializeFailure::DuplicateKey({}).into());", dup_key_error_key));
                    deser_loop.push_block(dup_check);
                    if CLI_ARGS.preserve_encodings {
                        if !key_encs.is_empty() {
                            deser_loop.line(format!(
                                "{}_key_encodings.insert({}.clone(), {});",
                                var_name,
                                key_var_name,
                                tuple_str(key_encs.iter().map(|enc| enc.field_name.clone()).collect())));
                        }
                        if !value_encs.is_empty() {
                            deser_loop.line(format!(
                                "{}_value_encodings.insert({}.clone(), {});",
                                var_name,
                                key_var_name,
                                tuple_str(value_encs.iter().map(|enc| enc.field_name.clone()).collect())));
                        }
                    }
                    body.push_block(deser_loop);
                    if CLI_ARGS.preserve_encodings {
                        final_exprs.push(format!("{}_encoding", var_name));
                        if !key_encs.is_empty() {
                            final_exprs.push(format!("{}_key_encodings", var_name));
                        }
                        if !value_encs.is_empty() {
                            final_exprs.push(format!("{}_value_encodings", var_name));
                        }
                        body.line(&format!("{}{}{}", before, final_expr(final_exprs, Some(table_var)), after));
                    } else {
                        body.line(&format!("{}{}{}", before, table_var, after));
                    }
                }
            },
            RustType::Alias(_ident, ty) => self.generate_deserialize(types, ty, var_name, before, after, in_embedded, optional_field, final_exprs, body, deserializer_name_overload),
            RustType::CBORBytes(ty) => {
                if CLI_ARGS.preserve_encodings {
                    final_exprs.push(format!("StringEncoding::from({}_bytes_encoding)", var_name));
                    body.line(&format!("let ({}_bytes, {}_bytes_encoding) = raw.bytes_sz()?;", var_name, var_name));
                } else {
                    body.line(&format!("let {}_bytes = raw.bytes()?;", var_name));
                };
                let name_overload = "inner_de";
                body.line(&format!("let mut {} = &mut Deserializer::from(std::io::Cursor::new({}_bytes));", name_overload, var_name));
                self.generate_deserialize(types, ty, var_name, before, after, in_embedded, optional_field, final_exprs, body, Some(name_overload));
            },
        }
    }

    fn deserialize_generated(&self, name: &RustIdent) -> bool {
        !self.no_deser_reasons.contains_key(name)
    }

    fn deserialize_generated_for_type(&self, field_type: &RustType) -> bool {
        match field_type {
            RustType::Fixed(_) => true,
            RustType::Primitive(_) => true,
            RustType::Rust(ident) => self.deserialize_generated(ident),
            RustType::Array(ty) => self.deserialize_generated_for_type(ty),
            RustType::Map(k, v) => self.deserialize_generated_for_type(k) && self.deserialize_generated_for_type(v),
            RustType::Tagged(_tag, ty) => self.deserialize_generated_for_type(ty),
            RustType::Optional(ty) => self.deserialize_generated_for_type(ty),
            RustType::Alias(_ident, ty) => self.deserialize_generated_for_type(ty),
            RustType::CBORBytes(ty) => self.deserialize_generated_for_type(ty),
        }
    }

    fn dont_generate_deserialize(&mut self, name: &RustIdent, reason: String) {
        self.no_deser_reasons.entry(name.clone()).or_default().push(reason);
    }

    pub fn print_structs_without_deserialize(&self) {
        for (name, reasons) in &self.no_deser_reasons {
            println!("Not generating {}::deserialize() - reasons:", name);
            for reason in reasons {
                println!("\t{}", reason);
            }
        }
    }

    pub fn reset_except_not_deserialized(&mut self, last_time: &mut Option<usize>) -> bool {
        let repeat = match last_time {
            Some(n) => *n != self.no_deser_reasons.len(),
            None => true,
        };
        if repeat {
            self.global_scope = codegen::Scope::new();
            self.serialize_scope = codegen::Scope::new();
            self.wasm_scope = codegen::Scope::new();
            self.cbor_encodings_scope = codegen::Scope::new();
            self.json_scope = codegen::Scope::new();
            self.already_generated.clear();
            // don't delete these to let out-of-order type aliases work
            //self.type_aliases = Self::aliases();
            // keep empty ones so we can know it's not generated
            for (_ident, reasons) in self.no_deser_reasons.iter_mut() {
                reasons.clear();
            }
        }
        *last_time = Some(self.no_deser_reasons.len());
        repeat
    }

    // TODO: repurpose this for type choices (not group choices)
    // TODO: make this its own function - there's no reason for this to be a method
    fn generate_type_choices_from_variants(&mut self, types: &IntermediateTypes, name: &RustIdent, variants: &Vec<EnumVariant>, tag: Option<usize>) {
        // Rust only
        generate_enum(self, types, &name, &variants, None, true, tag);
        if CLI_ARGS.wasm {
            // Generate a wrapper object that we will expose to wasm around this
            let mut wrapper = create_base_wasm_wrapper(self, name, true);
            // new
            for variant in variants.iter() {
                let variant_arg = variant.name_as_var();
                let enc_fields = if CLI_ARGS.preserve_encodings {
                    encoding_fields(&variant_arg, &variant.rust_type.clone().resolve_aliases())
                } else {
                    vec![]
                };
                let mut new_func = codegen::Function::new(&format!("new_{}", variant_arg));
                new_func.vis("pub");
                let can_fail = match &variant.name {
                    VariantIdent::Custom(_) => false,
                    VariantIdent::RustStruct(rust_ident) => types.can_new_fail(rust_ident),
                };
                if can_fail {
                    new_func.ret(format!("Result<{}, JsValue>", name));
                } else {
                    new_func.ret("Self");
                }
                if !variant.rust_type.is_fixed_value() {
                    new_func.arg(&variant_arg, &variant.rust_type.for_wasm_param());
                }
                if variant.rust_type.is_fixed_value() {
                    new_func.line(format!("Self(core::{}::new_{}())", name, variant.name_as_var()));
                } else {
                    let from_wasm_expr = variant.rust_type.from_wasm_boundary_clone(&variant_arg, can_fail);
                    new_func.line(format!(
                        "Self(core::{}::new_{}({}))",
                        name,
                        variant.name_as_var(),
                        ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())));
                }
                wrapper.s_impl.push_fn(new_func);
            }
            add_wasm_enum_getters(&mut wrapper.s_impl, name, &variants, None);
            wrapper.push(self);
            //push_wasm_wrapper(self, name, s, s_impl);
        }
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(&mut self, types: &IntermediateTypes, element_type: RustType, array_type_ident: &RustIdent) {
        if self.already_generated.insert(array_type_ident.clone()) {
            let inner_type = element_type.name_as_rust_array(true);
            let mut s = codegen::Struct::new(&array_type_ident.to_string());
            s
                .vis("pub")
                .tuple_field(&inner_type)
                .derive("Clone")
                .derive("Debug");
            // other functions
            let mut array_impl = codegen::Impl::new(&array_type_ident.to_string());
            let mut new_func = codegen::Function::new("new");
            new_func
                .vis("pub")
                .ret("Self");
            new_func.line("Self(Vec::new())");
            array_impl.push_fn(new_func);
            array_impl
                .new_fn("len")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .line("self.0.len()");
            array_impl
                .new_fn("get")
                .vis("pub")
                .ret(&element_type.for_wasm_return())
                .arg_ref_self()
                .arg("index", "usize")
                .line(element_type.to_wasm_boundary("self.0[index]", false));
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            array_impl
                .new_fn("add")
                .vis("pub")
                .arg_mut_self()
                .arg("elem", element_type.for_wasm_param())
                .line(format!("self.0.push({});", ToWasmBoundaryOperations::format(element_type.from_wasm_boundary_clone("elem", false).into_iter())));
            
            let mut from_wasm = codegen::Impl::new(&array_type_ident.to_string());
            from_wasm
                .impl_trait(format!("From<{}>", inner_type))
                .new_fn("from")
                .arg("native", &inner_type)
                .ret("Self")
                .line("Self(native)");
            let mut to_wasm = codegen::Impl::new(&array_type_ident.to_string());
            to_wasm
                .impl_trait(format!("std::convert::Into<{}>", inner_type))
                .new_fn("into")
                .arg_self()
                .ret(inner_type)
                .line("self.0");
            self
                .wasm()
                .raw("#[wasm_bindgen]")
                .push_struct(s)
                .raw("#[wasm_bindgen]")
                .push_impl(array_impl)
                .push_impl(from_wasm)
                .push_impl(to_wasm);
        }
    }
}

fn canonical_param() -> &'static str {
    if CLI_ARGS.canonical_form {
        ", force_canonical"
    } else {
        ""
    }
}

/// the codegen crate doesn't support proc macros for fields so we need to
/// do this with newlines. codegen takes care of indentation somehow.
fn encoding_var_macros(used_in_key: bool) -> String {
    let mut ret = if used_in_key {
        format!(
            "#[derivative({})]\n",
            key_derives(true)
                .iter()
                .map(|derive| format!("{}=\"ignore\"", derive))
                .collect::<Vec<String>>()
                .join(", "))
    } else {
        String::new()
    };
    if CLI_ARGS.json_serde_derives {
        ret.push_str("#[serde(skip)]\n");
    }
    ret
}

fn start_len(body: &mut dyn CodeBlock, rep: Representation, serializer_use: &str, encoding_var: &str, len_expr: &str) {
    let rep_str = match rep {
        Representation::Array => "array",
        Representation::Map => "map",
    };
    if CLI_ARGS.preserve_encodings {
        body.line(&format!("{}.write_{}_sz({}.to_len_sz({}{}))?;", serializer_use, rep_str, encoding_var, len_expr, canonical_param()));
    } else {
        body.line(&format!("{}.write_{}(cbor_event::Len::Len({}))?;", serializer_use, rep_str, len_expr));
    }
}

fn end_len(body: &mut dyn CodeBlock, serializer_use: &str, encoding_var: &str, is_end: bool) {
    if CLI_ARGS.preserve_encodings {
        body.line(&format!("{}.end({}{}){}", encoding_var, serializer_use, canonical_param(), if is_end { "" } else { "?;" }));
    } else {
        if is_end {
            body.line("Ok(serializer)");
        }
    }
}

fn write_using_sz(body: &mut dyn CodeBlock, func: &str, serializer_use: &str, expr: &str, fit_sz_expr: &str, line_ender: &str, encoding_var: &str) {
    if CLI_ARGS.preserve_encodings {
        body.line(
            &format!(
                "{}.{}_sz({}, fit_sz({}, {}{})){}",
                serializer_use, func, expr, fit_sz_expr, encoding_var, canonical_param(), line_ender));
    } else {
        body.line(&format!("{}.{}({}){}", serializer_use, func, expr, line_ender));
    }
}

fn write_string_sz(body: &mut dyn CodeBlock, func: &str, serializer_use: &str, expr: &str, line_ender: &str, encoding_var: &str) {
    if CLI_ARGS.preserve_encodings {
        body.line(
            &format!(
                "{}.{}_sz(&{}, {}.to_str_len_sz({}.len() as u64{})){}",
                serializer_use,
                func,
                expr,
                encoding_var,
                expr,
                canonical_param(),
                line_ender));
    } else {
        body.line(&format!("{}.{}(&{}){}", serializer_use, func, expr, line_ender));
    }
}

enum BlockOrLine {
    Line(String),
    Block(codegen::Block),
}

#[derive(Default)]
struct BlocksOrLines(Vec<BlockOrLine>);

impl BlocksOrLines {
    fn _as_single_line(&self) -> Option<&str> {
        match self.0.len() {
            1 => match &self.0[0] {
                BlockOrLine::Line(line) => Some(&line),
                BlockOrLine::Block(_) => None,
            },
            _ => None,
        }
    }
}

trait CodeBlock {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock;

    fn push_block(&mut self, block: codegen::Block) -> &mut dyn CodeBlock;

    fn push_all(&mut self, contents: BlocksOrLines) -> &mut dyn CodeBlock where Self: Sized {
        for content in contents.0 {
            match content {
                BlockOrLine::Line(line) => self.line(&line),
                BlockOrLine::Block(block) => self.push_block(block),
            };
        }
        self as &mut dyn CodeBlock
    }
}

impl CodeBlock for codegen::Function {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: codegen::Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for codegen::Block {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }
    
    fn push_block(&mut self, block: codegen::Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for BlocksOrLines {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Line(line.to_owned()));
        self
    }
    
    fn push_block(&mut self, block: codegen::Block) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Block(block));
        self
    }
}

trait DataType {
    fn derive(&mut self, derive: &str) -> &mut Self;
}

impl DataType for codegen::Struct {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

impl DataType for codegen::Enum {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

fn create_base_rust_struct<'a>(types: &IntermediateTypes<'a>, ident: &RustIdent) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(&mut s, types.used_as_key(ident), false);
    let mut group_impl = codegen::Impl::new(name);
    // TODO: anything here?
    (s, group_impl)
}

struct WasmWrapper {
    s: codegen::Struct,
    s_impl: codegen::Impl,
    // rust -> wasm
    from_wasm: Option<codegen::Impl>,
    // wasm -> rust
    from_native: Option<codegen::Impl>,
}

impl WasmWrapper {
    fn push(self, gen_scope: &mut GenerationScope) {
        gen_scope
            .wasm()
            .raw("#[wasm_bindgen]")
            .push_struct(self.s)
            .raw("#[wasm_bindgen]")
            .push_impl(self.s_impl);
        if let Some(from_wasm) = self.from_wasm {
            gen_scope.wasm().push_impl(from_wasm);
        }
        if let Some(from_native) = self.from_native {
            gen_scope.wasm().push_impl(from_native);
        }
    }
}

fn create_base_wasm_struct(gen_scope: &GenerationScope, ident: &RustIdent, exists_in_rust: bool) -> WasmWrapper {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    s
        .vis("pub")
        .derive("Clone")
        .derive("Debug");
    let mut s_impl = codegen::Impl::new(name);
    // There are auto-implementing ToBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if exists_in_rust {
        if CLI_ARGS.to_from_bytes_methods {
            let mut to_bytes = codegen::Function::new("to_bytes");
            to_bytes
                .ret("Vec<u8>")
                .arg_ref_self()
                .vis("pub");
            if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
                to_bytes
                    .arg("force_canonical", "bool")
                    .line("use core::serialization::ToBytes;")
                    .line("ToBytes::to_bytes(&self.0, force_canonical)");
            } else {
                to_bytes
                    .line("use core::serialization::ToBytes;")
                    .line("ToBytes::to_bytes(&self.0)");
            }
            s_impl.push_fn(to_bytes);
            if gen_scope.deserialize_generated(ident) {
                s_impl
                    .new_fn("from_bytes")
                    .ret(format!("Result<{}, JsValue>", name))
                    .arg("data", "Vec<u8>")
                    .vis("pub")
                    .line("use core::prelude::FromBytes;")
                    .line("FromBytes::from_bytes(data).map(Self).map_err(|e| JsValue::from_str(&format!(\"from_bytes: {}\", e)))");
            }
        }
        if CLI_ARGS.json_serde_derives {
            let mut to_json = codegen::Function::new("to_json");
            to_json
                .ret("Result<String, JsValue>")
                .arg_ref_self()
                .vis("pub")
                .line("serde_json::to_string_pretty(&self.0).map_err(|e| JsValue::from_str(&format!(\"to_json: {}\", e)))");
            s_impl.push_fn(to_json);
            let mut to_json_value = codegen::Function::new("to_json_value");
            to_json_value
                .ret("Result<JsValue, JsValue>")
                .arg_ref_self()
                .vis("pub")
                .line("JsValue::from_serde(&self.0).map_err(|e| JsValue::from_str(&format!(\"to_js_value: {}\", e)))");
            s_impl.push_fn(to_json_value);
            s_impl
                .new_fn("from_json")
                .ret(format!("Result<{}, JsValue>", name))
                .arg("json", "&str")
                .vis("pub")
                .line("serde_json::from_str(json).map(Self).map_err(|e| JsValue::from_str(&format!(\"from_json: {}\", e)))");
            
        }
    }
    WasmWrapper {
        s,
        s_impl,
        from_wasm: None,
        from_native: None,
    }
}

/// default_structure will have it be a DIRECT wrapper with a tuple field of core::{ident}
/// this will include generating to/from traits automatically
fn create_base_wasm_wrapper(gen_scope: &GenerationScope, ident: &RustIdent, default_structure: bool) -> WasmWrapper {
    assert!(CLI_ARGS.wasm);
    let mut base = create_base_wasm_struct(gen_scope, ident, true);
    let name = &ident.to_string();
    if default_structure {
        let native_name = &format!("core::{}", name);
        base.s.tuple_field(native_name);
        let mut from_wasm = codegen::Impl::new(name);
        from_wasm
            .impl_trait(format!("From<{}>", native_name))
            .new_fn("from")
            .arg("native", native_name)
            .ret("Self")
            .line("Self(native)");
            //.line(format!("Self({}(native))", native_name));
        let mut from_native = codegen::Impl::new(native_name);
        from_native
            .impl_trait(format!("From<{}>", name))
            .new_fn("from")
            .arg("wasm", name)
            .ret("Self")
            .line("wasm.0");
        base.from_wasm = Some(from_wasm);
        base.from_native = Some(from_native);
    }
    base
}

// Alway creates directly just Serialize impl. Shortcut for create_serialize_impls when
// we know we won't need the SerializeEmbeddedGroup impl.
// See comments for create_serialize_impls for usage.
fn create_serialize_impl(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, definite_len: Option<String>, use_this_encoding: Option<&str>) -> (codegen::Function, codegen::Impl) {
    match create_serialize_impls(ident, rep, tag, definite_len, use_this_encoding, false) {
        (ser_func, ser_impl, None) => (ser_func, ser_impl),
        (_ser_func, _ser_impl, Some(_embedded_impl)) => unreachable!(),
    }
}

// If definite_len is provided, it will use that expression as the definite length.
// Otherwise indefinite will be used and the user should remember to write a Special::Break at the end.
// Returns (serialize, Serialize, Some(SerializeEmbeddedGroup)) impls for structs that require embedded, in which case
// the serialize calls the embedded serialize and you implement the embedded serialize
// Otherwise returns (serialize Serialize, None) impls and you implement the serialize.
// In both cases the serialize function should be pushed to the Serialize impl.
// In the first case (need embedded ie plain group) the caller should create and push their
// own serialize_as_embedded_group to the SerializeEmbeddedGroup impl which will be called
// from within serialize(), and serialize() should not be expanded upon, just pushed.
// In the second case (no embedded), only the array/map tag + length are written and the user will
// want to write the rest of serialize() after that.
// * `use_this_encoding` - If present, references a variable (must be bool and in this scope) to toggle definite vs indefinite (e.g. for PRESERVE_ENCODING)
fn create_serialize_impls(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, definite_len: Option<String>, use_this_encoding: Option<&str>, generate_serialize_embedded: bool) -> (codegen::Function, codegen::Impl, Option<codegen::Impl>) {
    if generate_serialize_embedded {
        // This is not necessarily a problem but we should investigate this case to ensure we're not calling
        // (de)serialize_as_embedded without (de)serializing the tag
        assert_eq!(tag, None);
    }
    if use_this_encoding.is_some() && definite_len.is_none() {
        panic!("definite_len is required for use_this_encoding or else we'd only be able to serialize indefinite no matter what");
    }
    let name = &ident.to_string();
    let ser_impl = make_serialization_impl(name);
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        let expr = format!("{}u64", tag);
        write_using_sz(&mut ser_func, "write_tag", "serializer", &expr, &expr, "?;", "self.encodings.as_ref().map(|encs| encs.tag_encoding).unwrap_or_default()");
    }
    // TODO: do definite length encoding for optional fields too
    if let Some (rep) = rep {
        if let Some(definite) = use_this_encoding {
            start_len(&mut ser_func, rep, "serializer", definite, definite_len.as_ref().unwrap());
        } else {
            let len = match &definite_len {
                Some(fixed_field_count) => cbor_event_len_n(fixed_field_count),
                None => {
                    assert!(!CLI_ARGS.canonical_form);
                    cbor_event_len_indef().to_owned()
                },
            };
            match rep {
                Representation::Array => ser_func.line(format!("serializer.write_array({})?;", len)),
                Representation::Map => ser_func.line(format!("serializer.write_map({})?;", len)),
            };
        }
        if generate_serialize_embedded {
            match definite_len {
                Some(_) => ser_func.line(format!("self.serialize_as_embedded_group(serializer{})", canonical_param())),
                None => {
                    ser_func.line(format!("self.serialize_as_embedded_group(serializer{})?;", canonical_param()));
                    ser_func.line("serializer.write_special(CBORSpecial::Break)")
                },
            };
        }
    } else {
        // not array or map, generate serialize directly
        if generate_serialize_embedded {
            ser_func.line(format!("self.serialize_as_embedded_group(serializer{})", canonical_param()));
        }
    }
    if generate_serialize_embedded {
        let mut ser_embedded_impl = codegen::Impl::new(name);
        ser_embedded_impl.impl_trait("SerializeEmbeddedGroup");
        (ser_func, ser_impl, Some(ser_embedded_impl))
    } else {
        (ser_func, ser_impl, None)
    }
}

// Adds a fixed length check if length is fixed, reads the mandatory amount if there are optional fields, or nothing for dynamic lengths
fn add_deserialize_initial_len_check(deser_body: &mut dyn CodeBlock, len_info: RustStructCBORLen) {
    deser_body.line("let mut read_len = CBORReadLen::new(len);");
    match len_info {
        RustStructCBORLen::Dynamic => /*nothing*/(),
        // TODO: direct check here instead of involving read_len
        RustStructCBORLen::OptionalFields(mandatory) => {
            if mandatory != 0 {
                deser_body.line(&format!("read_len.read_elems({})?;", mandatory));
            }
        },
        RustStructCBORLen::Fixed(fixed) => {
            if fixed != 0 {
                deser_body.line(&format!("read_len.read_elems({})?;", fixed));
            }
        },
    }
}

// Adds final Len check if not fixed + reads for the ending Special::Break for Indefinite arrays 
fn add_deserialize_final_len_check(deser_body: &mut dyn CodeBlock, rep: Option<Representation>, len_info: RustStructCBORLen) {
    // We only check for Break for arrays since the implementation for maps uses len to decide
    // when to stop reading values, since otherwise with optional parameters it doesn't know.
    // We also can't do it from within deserialize_as_embedded_group() as that interferes with
    // plain groups nested inside other array groups
    let ending_check = match len_info {
        RustStructCBORLen::Fixed(_) => "()", // no need to check - checked at the start
        RustStructCBORLen::OptionalFields(_) |
        RustStructCBORLen::Dynamic => "read_len.finish()?",
    };
    match rep {
        Some(Representation::Array) => {
            let mut end_len_check = Block::new("match len");
            end_len_check.line(format!("{} => {},", cbor_event_len_n("_"), ending_check));
            let mut indefinite_check = Block::new(&format!("{} => match raw.special()?", cbor_event_len_indef()));
            indefinite_check.line(format!("CBORSpecial::Break => {},", ending_check));
            indefinite_check.line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
            indefinite_check.after(",");
            end_len_check.push_block(indefinite_check);
            deser_body.push_block(end_len_check);
        },
        Some(Representation::Map) => {
            deser_body.line(&format!("{};", ending_check));
        },
        None => /* this should just be for type choices */(),
    }
}

// CASE 1 - generate_deserialize_embedded = true:
//     Returns (Deserialize impl, Some(DeserializeEmbeddedGroup impl))
//     The caller should create and push their own deserialize_as_embedded_group to the
//     DeserializeEmbeddedGroup impl which will be called
//     from within deserialize(), and deserialize() should not be expanded upon, just pushed.
// CASE 2 - generate_deserialize_embedded = false:
//     Returns (Deserialize impl, None) and you implement the rest of the deserialize.
//     Only the array/map tag + length are read (including length checks) so far
//     and the user will want to write the rest of deserialize() after that.
//     It would be wise to use add_deserialize_final_len_check() as well since that does a final length check AND
//     reads the ending break closing tag for indefinite arrays (indefinite maps are read as a by-product of implementation)),
//     but this is done automatically for the embedded case.
// In both cases the deserialize function should be created and pushed to the Deserialize impl.
// deser_body shall be the body of deserialize()
// Also, a length check will be done if len_info is passed in, it will be checked at the start
// of deserialize(). An ending check is also done if we are generating the embedded deserialize,
// and should be added manually via CBORReadLen::finish() at the end of deserialize() if not using add_deserialize_final_len_check().
// This (in both options) relies on the use of CBORReadLen at every non-mandatory (if using len_info) element read, or all elements otherwise.
// * `store_encoding` - If present, creates a variable of the provided name in the deserialization impl as a bool to store if definite was used (true) or indefinite (false)
// Only generated if generate_deserialize_embedded is false as otherwise we wouldn't have access to it from within the embedded code block as it is declared in the regular Deserialize
fn create_deserialize_impls(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, len_info: RustStructCBORLen, generate_deserialize_embedded: bool, store_encoding: Option<&str>, deser_body: &mut dyn CodeBlock) -> (codegen::Impl, Option<codegen::Impl>) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Custom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    if let Some(tag) = tag {
        if CLI_ARGS.preserve_encodings {
            deser_body.line("let (tag, tag_encoding) = raw.tag_sz()?;");
        } else {
            deser_body.line("let tag = raw.tag()?;");
        }
        let mut tag_check = Block::new(&format!("if tag != {}", tag));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}));", name, tag));
        deser_body.push_block(tag_check);
    }
    if let Some (rep) = rep {
        match rep {
            Representation::Array => {
                if CLI_ARGS.preserve_encodings {
                    deser_body.line("let len = raw.array_sz()?;");
                } else {
                    deser_body.line("let len = raw.array()?;");
                }
                if !generate_deserialize_embedded {
                    if let Some(encoding_var_name) = store_encoding {
                        deser_body.line(&format!("let {}: LenEncoding = len.into();", encoding_var_name));
                    }
                }
                add_deserialize_initial_len_check(deser_body, len_info);
                if generate_deserialize_embedded {
                    deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);");
                }
            },
            Representation::Map => {
                if CLI_ARGS.preserve_encodings {
                    deser_body.line("let len = raw.map_sz()?;");
                } else {
                    deser_body.line("let len = raw.map()?;");
                }
                if !generate_deserialize_embedded {
                    if let Some(encoding_var_name) = store_encoding {
                        deser_body.line(&format!("let {}: LenEncoding = len.into();", encoding_var_name));
                    }
                }
                add_deserialize_initial_len_check(deser_body, len_info);
                if generate_deserialize_embedded {
                    deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);");
                }
            },
        };
    } else {
        panic!("TODO: how should we handle this considering we are dealing with Len?");
        //deser_body.line("Self::deserialize_as_embedded_group(serializer)");
    }
    let deser_embedded_impl = if generate_deserialize_embedded {
        add_deserialize_final_len_check(deser_body, rep, len_info);
        deser_body.line("ret");
        let mut embedded_impl = codegen::Impl::new(name);
        embedded_impl.impl_trait("DeserializeEmbeddedGroup");
        Some(embedded_impl)
    } else {
        None
    };
    (deser_impl, deser_embedded_impl)
}

fn push_rust_struct(
    gen_scope: &mut GenerationScope,
    name: &RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    ser_impl: codegen::Impl,
    ser_embedded_impl: Option<codegen::Impl>,
) {
    gen_scope.rust().push_struct(s);
    gen_scope.rust().push_impl(s_impl);
    gen_scope.rust_serialize().push_impl(ser_impl);
    if let Some(s) = ser_embedded_impl {
        gen_scope.rust_serialize().push_impl(s);
    }
}

// We need to execute field deserialization inside a closure in order to capture and annotate with the field name
// without having to put error annotation inside of every single cbor_event call.
fn make_err_annotate_block(annotation: &str, before: &str, after: &str) -> Block {
    let mut if_block = Block::new(&format!("{}(|| -> Result<_, DeserializeError>", before));
    if_block.after(&format!(")().map_err(|e| e.annotate(\"{}\")){}", annotation, after));
    if_block
}

fn make_deser_loop(len_var: &str, len_expr: &str) -> Block {
    Block::new(
        &format!(
            "while match {} {{ {} => {} < n as usize, {} => true, }}",
            len_var,
            cbor_event_len_n("n"),
            len_expr,
            cbor_event_len_indef()))
}

fn make_deser_loop_break_check() -> Block {
    let mut break_check = Block::new("if raw.cbor_type()? == CBORType::Special");
    // TODO: read special and go back 1 character
    break_check.line("assert_eq!(raw.special()?, CBORSpecial::Break);");
    break_check.line("break;");
    break_check
}

pub fn table_type() -> &'static str {
    if CLI_ARGS.preserve_encodings {
        "OrderedHashMap"
    } else {
        "BTreeMap"
    }
}

fn codegen_table_type(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, key_type: RustType, value_type: RustType, tag: Option<usize>) {
    assert!(CLI_ARGS.wasm);
    // this would interfere with loop code generation unless we
    // specially handle this case since you wouldn't know whether you hit a break
    // or are reading a key here, unless we check, but then you'd need to store the
    // non-break special value once read
    assert!(!key_type.cbor_types().contains(&CBORType::Special));
    let mut wrapper = create_base_wasm_struct(gen_scope, name, false);
    let inner_type = RustType::name_for_rust_map(&key_type, &value_type, true);
    wrapper.s.tuple_field(&inner_type);
    // new
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self")
        .line(format!("Self({}::new())", table_type()));
    wrapper.s_impl.push_fn(new_func);
    // len
    wrapper.s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    // insert
    let ret_modifier = if value_type.is_copy() {
        ""
    } else if value_type.directly_wasm_exposable() {
        ".map(|v| v.clone())"
    } else {
        ".map(|v| v.clone().into())"
    };
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm_param())
        .arg("value", value_type.for_wasm_param())
        .ret(format!("Option<{}>", value_type.for_wasm_return()))
        .line(
            format!(
                "self.0.insert({}, {}){}",
                ToWasmBoundaryOperations::format(key_type.from_wasm_boundary_clone("key", false).into_iter()),
                ToWasmBoundaryOperations::format(value_type.from_wasm_boundary_clone("value", false).into_iter()),
                ret_modifier));
    // ^ TODO: support failable types everywhere or just force it to be only a detail in the wrapper?
    wrapper.s_impl.push_fn(insert_func);
    // get
    let mut getter = codegen::Function::new("get");
    getter
        .arg_ref_self()
        .arg("key", key_type.for_wasm_param())
        .ret(format!("Option<{}>", value_type.for_wasm_return()))
        .vis("pub");
    if key_type.directly_wasm_exposable() {
        getter.line(format!(
            "self.0.get({}){}",
            key_type.from_wasm_boundary_ref("key"),
            if value_type.is_copy() { ".copied()" } else { ret_modifier }));
    } else {
        getter.line(format!(
            "self.0.get(&{}.0){}",
            key_type.from_wasm_boundary_ref("key"),
            if value_type.is_copy() { ".copied()" } else { ret_modifier }));
    }
    wrapper.s_impl.push_fn(getter);
    // keys
    let keys_type = RustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys
        .arg_ref_self()
        .ret(keys_type.for_wasm_return())
        .vis("pub");
    if keys_type.directly_wasm_exposable() {
        keys.line("self.0.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>()");
    } else {
        keys.line(format!("{}(self.0.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>())", keys_type.for_wasm_return()));
    }
    wrapper.s_impl.push_fn(keys);
    // to/from
    let mut from_wasm = codegen::Impl::new(&name.to_string());
    from_wasm
        .impl_trait(format!("From<{}>", inner_type))
        .new_fn("from")
        .arg("native", &inner_type)
        .ret("Self")
        .line("Self(native)");
    let mut to_wasm = codegen::Impl::new(&name.to_string());
    to_wasm
        .impl_trait(format!("std::convert::Into<{}>", inner_type))
        .new_fn("into")
        .arg_self()
        .ret(inner_type)
        .line("self.0");
    wrapper.push(gen_scope);
    gen_scope
        .wasm()
        .push_impl(from_wasm)
        .push_impl(to_wasm);
}

#[derive(Debug)]
struct EncodingField {
    field_name: String,
    type_name: String,
    default_expr: &'static str,
    // inner encodings - used for map/vec types
    inner: Vec<EncodingField>,
}

fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    match key {
        FixedValue::Text(_) => EncodingField {
            field_name: format!("{}_key_encoding", name),
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            inner: Vec::new(),
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{}_key_encoding", name),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            inner: Vec::new(),
        },
        _ => unimplemented!(),
    }
}

fn encoding_fields(name: &str, ty: &RustType) -> Vec<EncodingField> {
    assert!(CLI_ARGS.preserve_encodings);
    let x = match ty {
        RustType::Array(elem_ty) => {
            let base = EncodingField {
                field_name: format!("{}_encoding", name),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                inner: Vec::new(),
            };
            let inner_encs = encoding_fields(&format!("{}_elem", name), elem_ty);
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = if inner_encs.len() == 1 {
                    inner_encs.first().unwrap().type_name.clone()
                } else {
                    format!("({})", inner_encs.iter().map(|key_enc| key_enc.type_name.clone()).collect::<Vec<_>>().join(", "))
                };
                vec![
                    base,
                    EncodingField {
                        field_name: format!("{}_elem_encodings", name),
                        type_name: format!("Vec<{}>", type_name_elem),
                        default_expr: "Vec::new()",
                        inner: inner_encs,
                    },
                ]
            }
        },
        RustType::Map(k, v) => {
            let mut encs = vec![
                EncodingField {
                    field_name: format!("{}_encoding", name),
                    type_name: "LenEncoding".to_owned(),
                    default_expr: "LenEncoding::default()",
                    inner: Vec::new(),
                }
            ];
            let key_encs = encoding_fields(&format!("{}_key", name), k);
            let val_encs = encoding_fields(&format!("{}_value", name), v);

            if !key_encs.is_empty() {
                let type_name_value = if key_encs.len() == 1 {
                    key_encs.first().unwrap().type_name.clone()
                } else {
                    format!("({})", key_encs.iter().map(|key_enc| key_enc.type_name.clone()).collect::<Vec<_>>().join(", "))
                };
                encs.push(EncodingField {
                    field_name: format!("{}_key_encodings", name),
                    type_name: format!("BTreeMap<{}, {}>", k.for_rust_member(false), type_name_value),
                    default_expr: "BTreeMap::new()",
                    inner: key_encs,
                });
            }

            if !val_encs.is_empty() {
                let type_name_value = if val_encs.len() == 1 {
                    val_encs.first().unwrap().type_name.clone()
                } else {
                    format!("({})", val_encs.iter().map(|val_enc| val_enc.type_name.clone()).collect::<Vec<_>>().join(", "))
                };
                encs.push(EncodingField {
                    field_name: format!("{}_value_encodings", name),
                    type_name: format!("BTreeMap<{}, {}>", k.for_rust_member(false), type_name_value),
                    default_expr: "BTreeMap::new()",
                    inner: val_encs,
                });
            }
            encs
        },
        RustType::Primitive(p) => match p {
            Primitive::Bytes |
            Primitive::Str => vec![
                EncodingField {
                    field_name: format!("{}_encoding", name),
                    type_name: "StringEncoding".to_owned(),
                    default_expr: "StringEncoding::default()",
                    inner: Vec::new(),
                }
            ],
            Primitive::I8 |
            Primitive::I16 |
            Primitive::I32 |
            Primitive::I64 |
            Primitive::N64 |
            Primitive::U8 |
            Primitive::U16 |
            Primitive::U32 |
            Primitive::U64 => vec![
                EncodingField {
                    field_name: format!("{}_encoding", name),
                    type_name: "Option<cbor_event::Sz>".to_owned(),
                    default_expr: "None",
                    inner: Vec::new(),
                }
            ],
            Primitive::Bool => /* bool only has 1 encoding */vec![],
        },
        RustType::Fixed(f) => match f {
            FixedValue::Bool(_) |
            FixedValue::Null => vec![],
            FixedValue::Int(_) => encoding_fields(name, &RustType::Primitive(Primitive::I64)),
            FixedValue::Uint(_) => encoding_fields(name, &RustType::Primitive(Primitive::U64)),
            FixedValue::Text(_) => encoding_fields(name, &RustType::Primitive(Primitive::Str)),
        },
        RustType::Alias(_, _) => panic!("resolve types before calling this"),
        RustType::Tagged(tag, inner_ty) => {
            let mut encs = encoding_fields(&format!("{}_tag", name), &RustType::Fixed(FixedValue::Uint(*tag)));
            encs.append(&mut encoding_fields(name, &inner_ty));
            encs
        }
        RustType::Optional(ty) => encoding_fields(name, ty),
        RustType::Rust(_) => vec![],
        RustType::CBORBytes(inner_ty) => {
            let mut encs = encoding_fields(&format!("{}_bytes", name), &RustType::Primitive(Primitive::Bytes));
            encs.append(&mut encoding_fields(name, &inner_ty));
            encs
        }
    };
    x
}

fn encoding_var_names_str(field_name: &str, rust_type: &RustType) -> String {
    assert!(CLI_ARGS.preserve_encodings);
    let resolved_rust_type = rust_type.clone().resolve_aliases();
    let mut var_names = if resolved_rust_type.is_fixed_value() {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in encoding_fields(field_name, &resolved_rust_type).into_iter() {
        var_names.push(enc.field_name);
    }
    let var_names_str = if var_names.len() > 1 {
        format!("({})", var_names.join(", "))
    } else {
        var_names.join(", ")
    };
    var_names_str
}

fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

fn codegen_struct(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, tag: Option<usize>, record: &RustRecord) {
    // wasm wrapper
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, name, true);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .ret("Self")
            .vis("pub");
        let mut wasm_new_args = Vec::new();
        for field in &record.fields {
            // Fixed values don't need constructors or getters or fields in the rust code
            if !field.rust_type.is_fixed_value() {
                if field.optional {
                    // setter
                    let mut setter = codegen::Function::new(&format!("set_{}", field.name));
                    setter
                        .arg_mut_self()
                        .arg(&field.name, &field.rust_type.for_wasm_param())
                        .vis("pub")
                        .line(format!(
                            "self.0.{} = Some({})",
                            field.name,
                            ToWasmBoundaryOperations::format(field.rust_type.from_wasm_boundary_clone(&field.name, false).into_iter())));
                    // ^ TODO: check types.can_new_fail(&field.name)
                    wrapper.s_impl.push_fn(setter);
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter
                        .arg_ref_self()
                        .ret(format!("Option<{}>", field.rust_type.for_wasm_return()))
                        .vis("pub")
                        .line(field.rust_type.to_wasm_boundary_optional(&format!("self.0.{}", field.name), false));
                    wrapper.s_impl.push_fn(getter);
                } else {
                    // new
                    wasm_new.arg(&field.name, field.rust_type.for_wasm_param());
                    wasm_new_args.push(ToWasmBoundaryOperations::format(field.rust_type.from_wasm_boundary_clone(&field.name, false).into_iter()));
                    // ^ TODO: check types.can_new_fail(&field.name)
                    // do we want setters here later for mandatory types covered by new?
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter
                        .arg_ref_self()
                        .ret(field.rust_type.for_wasm_return())
                        .vis("pub")
                        .line(field.rust_type.to_wasm_boundary(&format!("self.0.{}", field.name), false));
                    wrapper.s_impl.push_fn(getter);
                }
            }
        }
        wasm_new.line(format!("Self(core::{}::new({}))", name, wasm_new_args.join(", ")));
        wrapper.s_impl.push_fn(wasm_new);
        wrapper.push(gen_scope);
    }
    
    // Rust-only for the rest of this function

    // Struct (fields) + constructor
    let (mut native_struct, mut native_impl) = create_base_rust_struct(types, name);
    native_struct.vis("pub");
    let mut native_new = codegen::Function::new("new");
    native_new
        .ret("Self")
        .vis("pub");
    let mut native_new_block = Block::new("Self");
    for field in &record.fields {
        if !gen_scope.deserialize_generated_for_type(&field.rust_type) {
            gen_scope.dont_generate_deserialize(name, format!("field {}: {} couldn't generate serialize", field.name, field.rust_type.for_rust_member(false)));
        }
        // Fixed values only exist in (de)serialization code (outside of preserve-encodings=true)
        if !field.rust_type.is_fixed_value() {
            if field.optional {
                // field
                native_struct.field(&format!("pub {}", field.name), format!("Option<{}>", field.rust_type.for_rust_member(false)));
                // new
                native_new_block.line(format!("{}: None,", field.name));
            } else {
                // field
                native_struct.field(&format!("pub {}", field.name), field.rust_type.for_rust_member(false));
                // new
                native_new.arg(&field.name, field.rust_type.for_rust_move());
                native_new_block.line(format!("{},", field.name));
            }
        }
    }
    let len_encoding_var = if CLI_ARGS.preserve_encodings {
        let encoding_name = RustIdent::new(CDDLIdent::new(format!("{}Encoding", name)));
        native_struct.field(&format!("{}encodings", encoding_var_macros(types.used_as_key(name))), format!("Option<{}>", encoding_name));
        native_new_block.line("encodings: None,");

        let mut encoding_struct = make_encoding_struct(&encoding_name.to_string());
        encoding_struct.field("pub len_encoding", "LenEncoding");
        if tag.is_some() {
            encoding_struct.field("pub tag_encoding", "Option<Sz>");
        }
        if record.rep == Representation::Map {
            encoding_struct.field("pub orig_deser_order", "Vec<usize>");
        }
        for field in &record.fields {
            // even fixed values still need to keep track of their encodings
            for field_enc in encoding_fields(&field.name, &field.rust_type.clone().resolve_aliases()) {
                encoding_struct.field(&format!("pub {}", field_enc.field_name), field_enc.type_name);
            }
            if record.rep == Representation::Map {
                let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                encoding_struct.field(&format!("pub {}", key_enc.field_name), key_enc.type_name);
            }
        }

        gen_scope.cbor_encodings().push_struct(encoding_struct);

        Some("len_encoding")
    } else {
        None
    };
    native_new.push_block(native_new_block);
    native_impl.push_fn(native_new);

    // Serialization (via rust traits) - includes Deserialization too
    let (ser_func, mut ser_impl, mut ser_embedded_impl) =
        create_serialize_impls(
            name,
            Some(record.rep),
            tag,
            record.definite_info(types),
            len_encoding_var.map(|var| format!("self.encodings.as_ref().map(|encs| encs.{}).unwrap_or_default()", var)).as_deref(),
            types.is_plain_group(name));
    let mut deser_f = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(&name.to_string(), "", "");
    let deser_body: &mut dyn CodeBlock = if CLI_ARGS.annotate_fields {
        &mut error_annotator
    } else {
        &mut deser_f
    };
    let (mut deser_impl, mut deser_embedded_impl) =
        create_deserialize_impls(
            name,
            Some(record.rep),
            tag,
            record.cbor_len_info(types),
            types.is_plain_group(name),
            len_encoding_var,
            deser_body);
    let mut deser_f = match deser_embedded_impl {
        Some(_) => {
            if CLI_ARGS.annotate_fields {
                // rustc complains about it being moved here and used elsewhere, but
                // that can never happen so let's just clone it here.
                // We need these to be in 2 separate blocks so we can borrow the correct
                // f here below.
                deser_f.push_block(error_annotator.clone());
            }
            deser_impl.push_fn(deser_f);
            let mut f = make_deserialization_function("deserialize_as_embedded_group");
            f.arg("read_len", "&mut CBORReadLen");
            if CLI_ARGS.preserve_encodings {
                f.arg("len", "cbor_event::LenSz");
            } else {
                f.arg("len", "cbor_event::Len");
            }
            // this is expected when creating the final struct but wouldn't have been available
            // otherwise as it is in the non-embedded deserialiation function
            if CLI_ARGS.preserve_encodings {
                f.line("let len_encoding = len.into();");
            }
            f
        },
        None => deser_f,
    };
    let deser_body: &mut dyn CodeBlock = match deser_embedded_impl {
        Some(_) => &mut deser_f,
        None => if CLI_ARGS.annotate_fields {
            &mut error_annotator
        } else {
            &mut deser_f
        },
    };
    let mut ser_func = match ser_embedded_impl {
        Some(_) => {
            ser_impl.push_fn(ser_func);
            make_serialization_function("serialize_as_embedded_group")
        },
        None => ser_func,
    };
    let in_embedded = deser_embedded_impl.is_some();
    let ctor_block = match record.rep {
        Representation::Array => {
            let mut deser_ret = Block::new(&format!("Ok({}", name));
            for field in record.fields.iter() {
                if field.optional {
                    let mut optional_array_ser_block = Block::new(&format!("if let Some(field) = &self.{}", field.name));
                    gen_scope.generate_serialize(
                        types,
                        &field.rust_type,
                        &mut optional_array_ser_block,
                        SerializeConfig::new("field", &field.name).expr_is_ref(true).encoding_var_in_option_struct("self.encodings"));
                    ser_func.push_block(optional_array_ser_block);
                    gen_scope.dont_generate_deserialize(name, format!("Array with optional field {}: {}", field.name, field.rust_type.for_rust_member(false)));
                } else {
                    gen_scope.generate_serialize(
                        types,
                        &field.rust_type,
                        &mut ser_func,
                        SerializeConfig::new(format!("self.{}", field.name), &field.name).encoding_var_in_option_struct("self.encodings"));
                    if CLI_ARGS.preserve_encodings {
                        let var_names_str = encoding_var_names_str(&field.name, &field.rust_type);
                        if CLI_ARGS.annotate_fields {
                            let mut err_deser = make_err_annotate_block(&field.name, &format!("let {} = ", var_names_str), "?;");
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, false, vec![], &mut err_deser, None);
                            deser_body.push_block(err_deser);
                        } else {
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &format!("let {} = ", var_names_str), ";", in_embedded, false, vec![], deser_body, None);
                        }
                    } else {
                        if field.rust_type.is_fixed_value() {
                            // don't set anything, only verify data
                            if CLI_ARGS.annotate_fields {
                                let mut err_deser = make_err_annotate_block(&field.name, "", "?;");
                                gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, false, vec![], &mut err_deser, None);
                                // this block needs to evaluate to a Result even though it has no value
                                err_deser.line("Ok(())");
                                deser_body.push_block(err_deser);
                            } else {
                                gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, false, vec![], deser_body, None);
                            }
                        } else {
                            if CLI_ARGS.annotate_fields {
                                let mut err_deser = make_err_annotate_block(&field.name, &format!("let {} = ", field.name), "?;");
                                gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, false, vec![], &mut err_deser, None);
                                deser_body.push_block(err_deser);
                            } else {
                                gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &format!("let {} = ", field.name), ";", in_embedded, false, vec![], deser_body, None);
                            }
                        }
                    }
                    if !field.rust_type.is_fixed_value() {
                        deser_ret.line(&format!("{},", field.name));
                    }
                }
            }
            if CLI_ARGS.preserve_encodings {
                let mut encoding_ctor = codegen::Block::new(&format!("encodings: Some({}Encoding", name));
                encoding_ctor.line("len_encoding,");
                if tag.is_some() {
                    encoding_ctor.line("tag_encoding: Some(tag_encoding),");
                }
                for field in record.fields.iter() {
                    // we don't support deserialization for optional fields so don't even bother
                    if !field.optional {
                        for field_enc in encoding_fields(&field.name, &field.rust_type.clone().resolve_aliases()) {
                            encoding_ctor.line(format!("{},", field_enc.field_name));
                        }
                    }
                }
                encoding_ctor.after("),");
                deser_ret.push_block(encoding_ctor);
            }
            // length checked inside of deserialize() - it causes problems for plain groups nested
            // in other groups otherwise
            deser_ret.after(")");
            deser_ret
        },
        Representation::Map => {
            let mut uint_field_deserializers = Vec::new();
            let mut text_field_deserializers = Vec::new();
            // (field_index, field, content) -- this is ordered by canonical order
            let mut ser_content: Vec<(usize, &RustField, BlocksOrLines)> = Vec::new();
            if CLI_ARGS.preserve_encodings {
                deser_body.line("let mut orig_deser_order = Vec::new();");
            }
            // we default to canonical ordering here as the default ordering as that should be the most useful
            // keep in mind this is always overwritten if you have CLI_ARGS.preserve_encodings enabled AND there was
            // a deserialized encoding, otherwise we still use this by default.
            for (field_index, field) in record.canonical_ordering() {
                // to support maps with plain groups inside is very difficult as we cannot guarantee
                // the order of fields so foo = {a, b, bar}, bar = (c, d) could have the order be
                // {a, d, c, b}, {c, a, b, d}, etc which doesn't fit with the nature of deserialize_as_embedded_group
                // A possible solution would be to take all fields into one big map, either in generation to begin with,
                // or just for deserialization then constructing at the end with locals like a, b, bar_c, bar_d.
                if let RustType::Rust(ident) = &field.rust_type {
                    if types.is_plain_group(&ident) {
                        gen_scope.dont_generate_deserialize(name, format!("Map with plain group field {}: {}", field.name, field.rust_type.for_rust_member(false)));
                    }
                }
                // declare variables for deser loop
                if CLI_ARGS.preserve_encodings {
                    for field_enc in encoding_fields(&field.name, &field.rust_type.clone().resolve_aliases()) {
                        deser_body.line(&format!("let mut {} = {};", field_enc.field_name, field_enc.default_expr));
                    }
                    let key_enc = key_encoding_field(&field.name, &field.key.as_ref().unwrap());
                    deser_body.line(&format!("let mut {} = {};", key_enc.field_name, key_enc.default_expr));
                }
                if field.rust_type.is_fixed_value() {
                    deser_body.line(&format!("let mut {}_present = false;", field.name));
                } else {
                    deser_body.line(&format!("let mut {} = None;", field.name));
                }
                let (data_name, expr_is_ref) = if field.optional {
                    (String::from("field"), true)
                } else {
                    (format!("self.{}", field.name), false)
                };

                let key = field.key.clone().unwrap();
                // deserialize key + value
                let mut deser_block = match &key {
                    FixedValue::Uint(x) => if CLI_ARGS.preserve_encodings {
                        Block::new(&format!("({}, key_enc) => ", x))
                    } else {
                        Block::new(&format!("{} => ", x))
                    },
                    FixedValue::Text(x) => Block::new(&format!("\"{}\" => ", x)),
                    _ => panic!("unsupported map key type for {}.{}: {:?}", name, field.name, key),
                };
                deser_block.after(",");
                let key_in_rust = match &key {
                    FixedValue::Uint(x) => format!("Key::Uint({})", x),
                    FixedValue::Text(x) => format!("Key::Str(\"{}\".into())", x),
                    _ => unimplemented!(),
                };
                if CLI_ARGS.preserve_encodings {
                    let mut dup_check = if field.rust_type.is_fixed_value() {
                        Block::new(&format!("if {}_present", field.name))
                    } else {
                        Block::new(&format!("if {}.is_some()", field.name))
                    };
                    dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                    deser_block.push_block(dup_check);

                    let temp_var_prefix = format!("tmp_{}", field.name);
                    let var_names_str = encoding_var_names_str(&temp_var_prefix, &field.rust_type);
                    let needs_vars = !var_names_str.is_empty();
                    let (before, after) = if var_names_str.is_empty() {
                        ("".to_owned(), "?")
                    } else {
                        (format!("let {} = ", var_names_str), "?;")
                    };
                    if CLI_ARGS.annotate_fields {
                        let mut err_deser = make_err_annotate_block(&field.name, &before, after);
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, field.optional, vec![], &mut err_deser, None);
                        deser_block.push_block(err_deser);
                    } else {
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &before, after, in_embedded, field.optional, vec![], deser_body, None);
                    }
                    // Due to destructuring assignemnt (RFC 372 / 71156) being unstable we're forced to use temporaries then reassign after
                    // which is not ideal but doing the assignment inside the lambda or otherwise has issues where it's putting lots of
                    // context-sensitive logic into generate_deserialize and you would need to declare temporaries in most cases anyway
                    // as cbor_event encoding-aware functions return tuples which just pushes the problem there instead.
                    // We might be able to write a nice way around this in the annotate_fields=false, preserve_encodings=true case
                    // but I don't think anyone (or many) would care about this as it's incredibly niche
                    // (annotate_fields=false would be for minimizing code size but then preserve_encodings=true generates way more code)
                    if field.rust_type.is_fixed_value() {
                        deser_block.line(format!("{}_present = true;", field.name));
                    } else {
                        deser_block.line(format!("{} = Some(tmp_{});", field.name, field.name));
                    }
                    for enc_field in encoding_fields(&field.name, &field.rust_type.clone().resolve_aliases()) {
                        deser_block.line(format!("{} = tmp_{};", enc_field.field_name, enc_field.field_name));
                    }
                } else {
                    if field.rust_type.is_fixed_value() {
                        let mut dup_check = Block::new(&format!("if {}_present", field.name));
                        dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                        deser_block.push_block(dup_check);
                        // only does verification and sets the field_present bool to do error checking later
                        if CLI_ARGS.annotate_fields {
                            let mut err_deser = make_err_annotate_block(&field.name, &format!("{}_present = ", field.name), "?;");
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, field.optional, vec![], &mut err_deser, None);
                            err_deser.line("Ok(true)");
                            deser_block.push_block(err_deser);
                        } else {
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, field.optional, vec![], &mut deser_block, None);
                            deser_block.line(&format!("{}_present = true;", field.name));
                        }
                    } else {
                        let mut dup_check = Block::new(&format!("if {}.is_some()", field.name));
                        dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                        deser_block.push_block(dup_check);
                        if CLI_ARGS.annotate_fields {
                            let mut err_deser = make_err_annotate_block(&field.name, &format!("{} = Some(", field.name), "?);");
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, field.optional, vec![], &mut err_deser, None);
                            deser_block.push_block(err_deser);
                        } else {
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &format!("{} = Some(", field.name), ");", in_embedded, field.optional, vec![], &mut deser_block, None);
                        }
                    }
                }
                if CLI_ARGS.preserve_encodings {
                    let key_encoding_var = key_encoding_field(&field.name, &key).field_name;
                    let enc_conversion = match &key {
                        FixedValue::Uint(_) => "Some(key_enc)",
                        FixedValue::Text(_) => "StringEncoding::from(key_enc)",
                        _ => unimplemented!(),
                    };
                    deser_block
                        .line(format!("{} = {};", key_encoding_var, enc_conversion))
                        .line(format!("orig_deser_order.push({});", field_index));
                }

                // serialize key
                let mut map_ser_content = BlocksOrLines::default();
                let serialize_config = SerializeConfig::new(&data_name, &field.name)
                    .expr_is_ref(expr_is_ref)
                    .encoding_var_in_option_struct("self.encodings");
                let key_encoding_var = serialize_config.encoding_var(Some("key"));
                
                match &key {
                    FixedValue::Uint(x) => {
                        let expr = format!("{}u64", x);
                        write_using_sz(&mut map_ser_content, "write_unsigned_integer", "serializer", &expr, &expr, "?;", &key_encoding_var);
                        uint_field_deserializers.push(deser_block);
                    },
                    FixedValue::Text(s) => {
                        write_string_sz(&mut map_ser_content, "write_text", "serializer", &format!("\"{}\"", s), "?;", &key_encoding_var);
                        text_field_deserializers.push(deser_block);
                    },
                    _ => panic!("unsupported map key type for {}.{}: {:?}", name, field.name, key),
                };

                // serialize value
                gen_scope.generate_serialize(
                    types,
                    &field.rust_type,
                    &mut map_ser_content,
                    serialize_config);
                ser_content.push((field_index, field, map_ser_content));
            }
            if CLI_ARGS.preserve_encodings {
                let (check_canonical, serialization_order) = if CLI_ARGS.canonical_form {
                    let indices_str = record
                        .canonical_ordering()
                        .iter()
                        .map(|(i, _)| i.to_string())
                        .collect::<Vec<String>>()
                        .join(",");
                    ("!force_canonical && ", format!("vec![{}]", indices_str))
                } else {
                    ("", format!("(0..{}).collect()", ser_content.len()))
                };
                ser_func.line(format!(
                    "let deser_order = self.encodings.as_ref().filter(|encs| {}encs.orig_deser_order.len() == {}).map(|encs| encs.orig_deser_order.clone()).unwrap_or_else(|| {});",
                    check_canonical,
                    record.definite_info(types).expect("cannot fail for maps"),
                    serialization_order));
                let mut ser_loop = codegen::Block::new("for field_index in deser_order");
                let mut ser_loop_match = codegen::Block::new("match field_index");
                for (field_index, field, content) in ser_content.into_iter() {
                    // TODO: while this would be nice we would need to either:
                    // 1) know this before we call gen_scope.generate_serialize() OR
                    // 2) strip that !is_end (?;) field from it which seems brittle
                    //if let Some(single_line) = content.as_single_line() {
                    //    ser_loop_match.line(format!("{} => {},"));
                    //} else {
                    //}
                    let mut field_ser_block = if field.optional {
                        Block::new(&format!("{} => if let Some(field) = &self.{}", field_index, field.name))
                    } else {
                        Block::new(&format!("{} =>", field_index))
                    };
                    field_ser_block.push_all(content);
                    ser_loop_match.push_block(field_ser_block);
                }
                ser_loop_match
                    .line("_ => unreachable!()")
                    .after(";");
                ser_loop.push_block(ser_loop_match);
                ser_func.push_block(ser_loop);
            } else {
                for (_field_index, field, content) in ser_content.into_iter() {
                    if field.optional {
                        let mut optional_ser_field = Block::new(&format!("if let Some(field) = &self.{}", field.name));
                        optional_ser_field.push_all(content);
                        ser_func.push_block(optional_ser_field);
                    } else {
                        ser_func.push_all(content);
                    }
                }
            }
            // needs to be in one line rather than a block because Block::after() only takes a string
            deser_body.line("let mut read = 0;");
            let mut deser_loop = make_deser_loop("len", "read");
            let mut type_match = Block::new("match raw.cbor_type()?");
            let mut uint_match =  if CLI_ARGS.preserve_encodings {
                Block::new("CBORType::UnsignedInteger => match raw.unsigned_integer_sz()?")
            } else {
                Block::new("CBORType::UnsignedInteger => match raw.unsigned_integer()?")
            };
            for case in uint_field_deserializers {
                uint_match.push_block(case);
            }
            let unknown_key_decl = if CLI_ARGS.preserve_encodings {
                "(unknown_key, _enc)"
            } else {
                "unknown_key"
            };
            uint_match.line(format!("{} => return Err(DeserializeFailure::UnknownKey(Key::Uint(unknown_key)).into()),", unknown_key_decl));
            uint_match.after(",");
            type_match.push_block(uint_match);
            // we can't map text_sz() with String::as_str() to match it since that would return a reference to a temporary
            // so we need to store it in a local and have an extra block to declare it
            if CLI_ARGS.preserve_encodings {
                let mut outer_match = Block::new("CBORType::Text =>");
                outer_match.line("let (text_key, key_enc) = raw.text_sz()?;");
                let mut text_match = Block::new("match text_key.as_str()");
                for case in text_field_deserializers {
                    text_match.push_block(case);
                }
                text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
                outer_match.after(",");
                outer_match.push_block(text_match);
                type_match.push_block(outer_match);
            } else {
                let mut text_match = Block::new("CBORType::Text => match raw.text()?.as_str()");
                for case in text_field_deserializers {
                    text_match.push_block(case);
                }
                text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
                text_match.after(",");
                type_match.push_block(text_match);
            };
            let mut special_match = Block::new("CBORType::Special => match len");
            special_match.line(format!("{} => return Err(DeserializeFailure::BreakInDefiniteLen.into()),", cbor_event_len_n("_")));
            // TODO: this will need to change if we support Special values as keys (e.g. true / false)
            let mut break_check = Block::new(&format!("{} => match raw.special()?", cbor_event_len_indef()));
            break_check.line("CBORSpecial::Break => break,");
            break_check.line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
            break_check.after(",");
            special_match.push_block(break_check);
            special_match.after(",");
            type_match.push_block(special_match);
            type_match.line("other_type => return Err(DeserializeFailure::UnexpectedKeyType(other_type).into()),");
            deser_loop.push_block(type_match);
            deser_loop.line("read += 1;");
            deser_body.push_block(deser_loop);
            let mut ctor_block = Block::new("Ok(Self");
            // make sure the field is present, and unwrap the Option<T>
            for field in &record.fields {
                if !field.optional {
                    let key = match &field.key {
                        Some(FixedValue::Uint(x)) => format!("Key::Uint({})", x),
                        Some(FixedValue::Text(x)) => format!("Key::Str(String::from(\"{}\"))", x),
                        None => unreachable!(),
                        _ => unimplemented!(),
                    };
                    if field.rust_type.is_fixed_value() {
                        let mut mandatory_field_check = Block::new(&format!("if !{}_present", field.name));
                        mandatory_field_check.line(format!("return Err(DeserializeFailure::MandatoryFieldMissing({}).into());", key));
                        deser_body.push_block(mandatory_field_check);
                    } else {
                        let mut mandatory_field_check = Block::new(&format!("let {} = match {}", field.name, field.name));
                        mandatory_field_check.line("Some(x) => x,");
                        
                        mandatory_field_check.line(format!("None => return Err(DeserializeFailure::MandatoryFieldMissing({}).into()),", key));
                        mandatory_field_check.after(";");
                        deser_body.push_block(mandatory_field_check);
                    }
                }
                if !field.rust_type.is_fixed_value() {
                    ctor_block.line(format!("{},", field.name));
                }
            }
            if CLI_ARGS.preserve_encodings {
                let mut encoding_ctor = codegen::Block::new(&format!("encodings: Some({}Encoding", name));
                if tag.is_some() {
                    encoding_ctor.line("tag_encoding: Some(tag_encoding),");
                }
                encoding_ctor
                    .line("len_encoding,")
                    .line("orig_deser_order,");
                for field in record.fields.iter() {
                    let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                    encoding_ctor.line(format!("{},", key_enc.field_name));
                    for field_enc in encoding_fields(&field.name, &field.rust_type.clone().resolve_aliases()) {
                        encoding_ctor.line(format!("{},", field_enc.field_name));
                    }
                }
                encoding_ctor.after("),");
                ctor_block.push_block(encoding_ctor);
            }
            ctor_block.after(")");
            ctor_block
        },
    };
    let len_enc_var = len_encoding_var.map(|var| format!("self.encodings.as_ref().map(|encs| encs.{}).unwrap_or_default()", var)).unwrap_or_default();
    end_len(&mut ser_func, "serializer", &len_enc_var, true);
    match &mut ser_embedded_impl {
        Some(ser_embedded_impl) => ser_embedded_impl.push_fn(ser_func),
        None => ser_impl.push_fn(ser_func),
    };
    if deser_embedded_impl.is_none() {
        // ending checks are included with embedded serialization setup
        // since we are populating deserialize_as_embedded_group() and deserialize()
        // is already complete
        // but these checks must be done manually here *after* we populate deserialize()
        add_deserialize_final_len_check(deser_body, Some(record.rep), record.cbor_len_info(types));
    }
    deser_body.push_block(ctor_block);

    match &mut deser_embedded_impl {
        Some(deser_embedded_impl) => {
            deser_embedded_impl.push_fn(deser_f);
        },
        None => {
            if CLI_ARGS.annotate_fields {
                deser_f.push_block(error_annotator);
            }
            deser_impl.push_fn(deser_f);
        },
    };
    push_rust_struct(gen_scope, name, native_struct, native_impl, ser_impl, ser_embedded_impl);
    // TODO: generic deserialize (might need backtracking)
    if gen_scope.deserialize_generated(name) {
        gen_scope.rust_serialize().push_impl(deser_impl);
        if let Some(deser_embedded_impl) = deser_embedded_impl {
            gen_scope.rust_serialize().push_impl(deser_embedded_impl);
        }
    }
}

fn codegen_group_choices(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, variants: &Vec<EnumVariant>, rep: Representation, tag: Option<usize>) {
    // rust inner enum
    generate_enum(gen_scope, types, name, &variants, Some(rep), false, tag);

    // wasm wrapper
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, name, true);
        // new (1 per variant)
        for variant in variants.iter() {
            // TODO: verify if variant.serialize_as_embedded_group impacts ctor generation
            let mut new_func = codegen::Function::new(&format!("new_{}", variant.name_as_var()));
            new_func
                .ret("Self")
                .vis("pub");
            let mut output_comma = false;
            // We only want to generate Variant::new() calls when we created a special struct
            // for the variant, which happens in the general case for multi-field group choices
            let fields = match &variant.rust_type {
                // we need to check for sanity here, as if we're referring to the ident
                // it should at this stage be registered
                RustType::Rust(ident) => match types.rust_struct(ident).unwrap().variant() {
                    RustStructType::Record(record) => Some(&record.fields),
                    _ => None,
                },
                RustType::Alias(_, _) => unimplemented!("TODO: do we need to handle aliases here?"),
                _ => None,
            };
            match fields {
                Some(fields) => {
                    let ctor_fields: Vec<&RustField> = fields.iter().filter(|f| !f.optional && !f.rust_type.is_fixed_value()).collect();
                    match ctor_fields.len() {
                        0 => {
                            new_func.line(format!("Self(core::{}::new_{}())", name, variant.name_as_var()));
                        },
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
                            let mut ctor = format!("Self(core::{}::new_{}(", name, variant.name_as_var());
                            for field in ctor_fields {
                                if output_comma {
                                    ctor.push_str(", ");
                                } else {
                                    output_comma = true;
                                }
                                new_func.arg(&field.name, field.rust_type.for_wasm_param());
                                ctor.push_str(&ToWasmBoundaryOperations::format(field.rust_type.from_wasm_boundary_clone(&field.name, false).into_iter()));
                                // ^ TODO: check types.can_new_fail(&field.name)
                            }
                            ctor.push_str("))");
                            new_func.line(ctor);
                        },
                    }
                },
                None => {
                    // just directly pass in the variant's type
                    if variant.rust_type.is_fixed_value() {
                        new_func.line(format!("Self(core::{}::new_{}())", name, variant.name_as_var()));
                    } else {
                        let field_name = variant.name.to_string();
                        new_func
                            .arg(&field_name, variant.rust_type.for_wasm_param())
                            .line(format!("Self(core::{}::new_{}({}))", name, variant.name_as_var(), ToWasmBoundaryOperations::format(variant.rust_type.from_wasm_boundary_clone(&field_name, false).into_iter())));
                        // ^ TODO: check types.can_new_fail(&field.name)
                    }
                },
            }
            wrapper.s_impl.push_fn(new_func);
        }
        // enum-getters
        add_wasm_enum_getters(&mut wrapper.s_impl, name, &variants, Some(rep));
        //push_wasm_wrapper(gen_scope, name, s, s_impl);
        wrapper.push(gen_scope);
    }
}

fn add_wasm_enum_getters(s_impl: &mut codegen::Impl, name: &RustIdent, variants: &Vec<EnumVariant>, rep: Option<Representation>) {
    assert!(CLI_ARGS.wasm);
    // kind() getter
    let kind_name = format!("{}Kind", name);
    let mut get_kind = codegen::Function::new("kind");
    get_kind
        .arg_ref_self()
        .vis("pub")
        .ret(&kind_name);
    let mut get_kind_match = codegen::Block::new("match &self.0");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(&variant, rep);
        get_kind_match.line(format!("core::{}::{}{} => {}::{},", name, variant.name, enum_gen_info.capture_ignore_all(), kind_name, variant.name));
    }
    get_kind.push_block(get_kind_match);
    s_impl.push_fn(get_kind);

    // as_{variant} conversions (returns None -> undefined when not the type)
    for variant in variants.iter() {
        if !variant.rust_type.is_fixed_value() {
            let enum_gen_info = EnumVariantInRust::new(&variant, rep);
            let mut as_variant = codegen::Function::new(&format!("as_{}", variant.name_as_var()));
            as_variant
                .arg_ref_self()
                .vis("pub")
                .ret(&format!("Option<{}>", variant.rust_type.for_wasm_return()));
            let mut variant_match = codegen::Block::new("match &self.0");
            variant_match.line(format!(
                "core::{}::{}{} => Some({}),",
                name,
                variant.name,
                enum_gen_info.capture_ignore_encodings(),
                variant.rust_type.to_wasm_boundary(&enum_gen_info.names[0], true)));
            variant_match.line("_ => None,");
            as_variant.push_block(variant_match);
            s_impl.push_fn(as_variant);
        }
    }
}

fn cbor_event_len_n(n: &str) -> String {
    if CLI_ARGS.preserve_encodings {
        format!("cbor_event::LenSz::Len({}, _)", n)
    } else {
        format!("cbor_event::Len::Len({})", n)
    }
}

fn cbor_event_len_indef() -> &'static str {
    if CLI_ARGS.preserve_encodings {
        "cbor_event::LenSz::Indefinite"
    } else {
        "cbor_event::Len::Indefinite"
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
    fn new(variant: &EnumVariant, rep: Option<Representation>) -> Self {
        let name = variant.name_as_var();
        let mut enc_fields = if CLI_ARGS.preserve_encodings {
            encoding_fields(&name, &variant.rust_type.clone().resolve_aliases())
        } else {
            vec![]
        };
        let (mut types, mut names) = if variant.rust_type.is_fixed_value() {
            (vec![], vec![])
        } else {
            (vec![variant.rust_type.for_rust_member(false)], vec![name])
        };
        let mut outer_vars = 0;
        // TOOD: for tags too?
        if CLI_ARGS.preserve_encodings && rep.is_some() && !variant.serialize_as_embedded_group {
            enc_fields.push(EncodingField {
                field_name: "outer_len_encoding".to_owned(),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                inner: Vec::new(),
            });
            outer_vars += 1;
        }
        for enc_field in &enc_fields {
            types.push(enc_field.type_name.clone());
            names.push(enc_field.field_name.clone());
        }
        assert_eq!(types.len(), names.len());
        Self {
            name: variant.name.clone(),
            enc_fields,
            names,
            types,
            outer_vars,
        }
    }

    fn names_without_outer(&self) -> &[String] {
        &self.names[..self.names.len() - self.outer_vars]
    }

    fn names_with_macros(&self, used_in_key: bool) -> Vec<String> {
        self.names.iter().enumerate().map(|(i, name)| {
            if i < self.names.len() - self.enc_fields.len() {
                // not an encoding variable:
                name.clone()
            } else {
                // encoding variable:
                // the codeen crate doesn't support proc macros on fields but we just inline
                // these with a newline in the field names for declaring as workaround.
                // Indentation is never an issue as we're always 2 levels deep for field declarations
                format!("{}{}", encoding_var_macros(used_in_key), name)
            }
        }).collect()
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
            _ => if self.enc_fields.len() == self.names.len() {
                "{ .. }".to_owned()
            } else {
                format!("{{ {}, .. }}", self.names[0])
            },
        }
    }

    /// if init_fields exists, use these for values, otherwise assumes variables exist with same names
    fn generate_constructor(&self, body: &mut dyn CodeBlock, before: &str, after: &str, init_fields: Option<&Vec<String>>) {
        let init_fields = init_fields.unwrap_or_else(|| self.names.as_ref());
        match init_fields.len() {
            0 => {
                body.line(&format!("{}Self::{}{}", before, self.name, after));
            },
            1 if self.enc_fields.is_empty() => {
                body.line(&format!("{}Self::{}({}){}", before, self.name, init_fields.join(", "), after));
            },
            _ => {
                let mut ctor = Block::new(&format!("{}Self::{}", before, self.name));
                for (name, expr) in self.names.iter().zip(init_fields.iter()) {
                    if name == expr {
                        ctor.line(format!("{},", name));
                    } else {
                        ctor.line(format!("{}: {},", name, expr));
                    }
                }
                ctor.after(after);
                body.push_block(ctor);
            },
        };
    }
}

// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choicecs which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
fn generate_enum(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, variants: &Vec<EnumVariant>, rep: Option<Representation>, generate_deserialize_directly: bool, tag: Option<usize>) {
    if CLI_ARGS.wasm {
        // also create a wasm-exposed enum just to distinguish the type
        let mut kind = codegen::Enum::new(&format!("{}Kind", name));
        kind.vis("pub");
        for variant in variants.iter() {
            kind.new_variant(&variant.name.to_string());
        }
        gen_scope
            .wasm()
            .raw("#[wasm_bindgen]")
            .push_enum(kind);
    }

    // rust enum containing the data
    let mut e = codegen::Enum::new(&name.to_string());
    e.vis("pub");
    let mut e_impl = codegen::Impl::new(name.to_string());
    // instead of using create_serialize_impl() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(&mut e, types.used_as_key(name), true);
    let mut ser_impl = make_serialization_impl(&name.to_string());
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        // TODO: how to even store these? (maybe it could be a new field in every enum variant)
        assert!(!CLI_ARGS.preserve_encodings);
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    let mut ser_array_match_block = Block::new("match self");
    // we use Dynamic to avoid having any length checks here since we don't know what they are yet without testing the variants
    // and it's not worth looking into and complicating things on the off chance that all variants are the same length.
    let len_info = RustStructCBORLen::Dynamic;
    let mut deser_func = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(&name.to_string(), "", "");
    let deser_body: &mut dyn CodeBlock = if CLI_ARGS.annotate_fields {
        &mut error_annotator
    } else {
        &mut deser_func
    };
    let mut deser_impl = if generate_deserialize_directly {
        // this is handled in create_deseriaize_impls in the other case, and it MUST be handled there to ensure that
        // the tag check is done BEFORE reading the array/map CBOR
        generate_tag_check(deser_body, name, tag);
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        deser_impl
    } else {
        // this handles the tag check too
        let outer_encoding_var = if CLI_ARGS.preserve_encodings {
            Some("outer_len_encoding")
        } else {
            None
        };
        let (deser_impl, _deser_embedded_impl) = create_deserialize_impls(name, rep, tag, len_info.clone(), false, outer_encoding_var, deser_body);
        deser_impl
    };
    deser_body.line("let initial_position = raw.as_mut_ref().seek(SeekFrom::Current(0)).unwrap();");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(&variant, rep);
        let variant_var_name = variant.name_as_var();
        let mut v = codegen::Variant::new(&variant.name.to_string());
        match enum_gen_info.names.len() {
            0 => {
            },
            1 if enum_gen_info.enc_fields.is_empty() => {
                v.tuple(&enum_gen_info.types[0]);
            },
            _ => {
                for (name_with_macros, type_str) in enum_gen_info.names_with_macros(types.used_as_key(name)).into_iter().zip(enum_gen_info.types.iter()) {
                    v.named(&name_with_macros, type_str);
                }
            },
        }
        e.push_variant(v);
        // new (particularly useful if we have encoding variables)
        let mut new_func = codegen::Function::new(&format!("new_{}", variant_var_name));
        new_func
            .ret("Self")
            .vis("pub");
        let mut output_comma = false;
        // We only want to generate Variant::new() calls when we created a special struct
        // for the variant, which happens in the general case for multi-field group choices
        let fields = match variant.rust_type.strip_to_semantical_type() {
            // we need to check for sanity here, as if we're referring to the ident
            // it should at this stage be registered
            RustType::Rust(ident) => {
                
                match types.rust_struct(ident).expect(&format!("{} refers to undefined ident: {}", name, ident)).variant() {
                    RustStructType::Record(record) => Some(&record.fields),
                    _ => None,
                }
            },
            _ => None,
        };
        let mut init_fields = match rep.and(fields) {
            Some(fields) => {
                let ctor_fields: Vec<&RustField> = fields.iter().filter(|f| !f.optional && !f.rust_type.is_fixed_value()).collect();
                let mut ctor = format!("{}::new(", variant.name);
                for field in ctor_fields {
                    if output_comma {
                        ctor.push_str(", ");
                    } else {
                        output_comma = true;
                    }
                    new_func.arg(&field.name, field.rust_type.for_rust_move());
                    ctor.push_str(&field.name);
                    // ^ TODO: check types.can_new_fail(&field.name)?
                }
                ctor.push_str(")");
                vec![ctor]
            },
            None => {
                if variant.rust_type.is_fixed_value() {
                    vec![]
                } else {
                    // just directly pass in the variant's type
                    let field_name = variant.name_as_var();
                    new_func.arg(&field_name, variant.rust_type.for_rust_move());
                    vec![field_name]
                    // ^ TODO: check types.can_new_fail(&field.name)?
                }
            },
        };
        for enc_field in enum_gen_info.enc_fields.iter() {
            init_fields.push(enc_field.default_expr.to_owned());
        }
        enum_gen_info.generate_constructor(&mut new_func, "", "", Some(&init_fields));
        e_impl.push_fn(new_func);

        // serialize
        if variant.serialize_as_embedded_group {
            assert_eq!(enum_gen_info.names.len(), 1);
            // we use serialize() instead of serialize_as_embedded_group() to count as the outer array tag here
            // to simplify things (the size logic is there already)
            ser_array_match_block.line(&format!(
                "{}::{}({}) => {}.serialize(serializer{}),",
                name,
                variant.name,
                variant_var_name,
                variant_var_name,
                canonical_param()));
        } else {
            let mut case_block = Block::new(&format!("{}::{}{} =>", name, variant.name, enum_gen_info.capture_all()));
            if CLI_ARGS.preserve_encodings {
                if let Some(r) = rep {
                    // group choice
                    let n = variant.rust_type.expanded_field_count(types)
                        .expect("preserve-encodings=true not supported with varying-size group choice");
                    start_len(&mut case_block, r, "serializer", "outer_len_encoding", &n.to_string());
                    gen_scope.generate_serialize(
                        types,
                        &variant.rust_type,
                        &mut case_block,
                        SerializeConfig::new(&variant_var_name, &variant_var_name)
                            .expr_is_ref(true)
                            .encoding_var_is_ref(true));
                    end_len(&mut case_block, "serializer", "outer_len_encoding", false);
                    case_block.line("Ok(serializer)");
                } else {
                    // type choice
                    gen_scope.generate_serialize(
                        types,
                        &variant.rust_type,
                        &mut case_block,
                        SerializeConfig::new(&variant_var_name, &variant_var_name)
                            .expr_is_ref(true)
                            .is_end(true)
                            .encoding_var_is_ref(true));
                }
            } else {
                let write_break = match rep {
                    Some(r) => {
                        let (len_str, indefinite) = match variant.rust_type.expanded_field_count(types) {
                            Some(n) => (cbor_event_len_n(&n.to_string()), false),
                            None => (String::from(cbor_event_len_indef()), true),
                        };
                        let func_str = match r {
                            Representation::Array => "write_array",
                            Representation::Map => "write_map",
                        };
                        case_block.line(format!("serializer.{}({})?;", func_str, len_str));
                        indefinite
                    },
                    None => false,
                };
                // TODO: only generate a block if the serialize is more than 1 line
                // Problem: generate_serialize() works in terms of line() and push_block()
                //          but we'd just want to inline the single one inside of a line...
                gen_scope.generate_serialize(
                    types,
                    &variant.rust_type,
                    &mut case_block,
                    SerializeConfig::new(&variant_var_name, &variant_var_name)
                        .expr_is_ref(true)
                        .is_end(!write_break));
                if write_break {
                    case_block.line("serializer.write_special(CBORSpecial::Break)");
                }
            }
            case_block.after(",");
            ser_array_match_block.push_block(case_block);
        }
        // deserialize
        // TODO: don't backtrack if variants begin with non-overlapping cbor types
        // TODO: how to detect when a greedy match won't work? (ie choice with choices in a choice possibly)
        let mut variant_deser = Block::new("match (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>");
        if enum_gen_info.types.is_empty() {
            gen_scope.generate_deserialize(types, &variant.rust_type, &variant.name_as_var(), "", "", false, false, vec![], &mut variant_deser, None);
            variant_deser.line("Ok(())");
        } else {
            gen_scope.generate_deserialize(types, &variant.rust_type, &variant.name_as_var(), "Ok(", ")", false, false, vec![], &mut variant_deser, None);
        }
        variant_deser.after(")(raw)");
        deser_body.push_block(variant_deser);
        // can't chain blocks so we just put them one after the other
        let mut return_if_deserialized = Block::new("");
        let names_without_outer = enum_gen_info.names_without_outer();
        if names_without_outer.is_empty() {
            return_if_deserialized.line(format!("Ok(()) => return Ok({}::{}),", name, variant.name));
        } else {
            enum_gen_info.generate_constructor(
                &mut return_if_deserialized,
                &if names_without_outer.len() > 1 {
                    format!("Ok(({})) => return Ok(", names_without_outer.join(", "))
                } else {
                    format!("Ok({}) => return Ok(", names_without_outer.join(", "))
                },
                "),",
                None);
        }
        return_if_deserialized.line("Err(_) => raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap(),");
        return_if_deserialized.after(";");
        deser_body.push_block(return_if_deserialized);
    }
    ser_func.push_block(ser_array_match_block);
    ser_impl.push_fn(ser_func);
    // TODO: we pass in a dummy Fixed to avoid the check since we don't support optional fields for plain groups
    // which causes an issue with group choices of plain groups where if we generate_deserialize() with
    // optional_field = true then we hit asserts (not supported) and
    // optional_field = false causes this final check to fail since no elements were read
    // A possible workaround for this could be to read it beforehand if possible but
    // that gets complicated for optional fields inside those plain groups so we'll
    // just avoid this check instead for this one case.
    add_deserialize_final_len_check(deser_body, rep, RustStructCBORLen::Fixed(0));
    deser_body.line(&format!("Err(DeserializeError::new(\"{}\", DeserializeFailure::NoVariantMatched.into()))", name));
    if CLI_ARGS.annotate_fields {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    gen_scope
        .rust()
        .push_enum(e)
        .push_impl(e_impl);
    gen_scope
        .rust_serialize()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

fn make_serialization_function(name: &str) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f
        .generic("'se, W: Write")
        .ret("cbor_event::Result<&'se mut Serializer<W>>")
        .arg_ref_self()
        .arg("serializer", "&'se mut Serializer<W>");
    if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
        f.arg("force_canonical", "bool");
    }
    f
}

fn make_serialization_impl(name: &str) -> codegen::Impl {
    let mut ser_impl = codegen::Impl::new(name);
    if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
        ser_impl.impl_trait("Serialize");
    } else {
        ser_impl.impl_trait("cbor_event::se::Serialize");
    }
    ser_impl
}

fn make_deserialization_function(name: &str) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f
        .generic("R: BufRead + Seek")
        .ret("Result<Self, DeserializeError>")
        .arg("raw", "&mut Deserializer<R>");
    f
}

fn make_encoding_struct(encoding_name: &str) -> codegen::Struct {
    let mut encoding_struct = codegen::Struct::new(&encoding_name.to_string());
    encoding_struct
        .vis("pub")
        .derive("Clone")
        .derive("Debug")
        .derive("Default");
    encoding_struct
}

fn generate_tag_check(deser_func: &mut dyn CodeBlock, ident: &RustIdent, tag: Option<usize>) {
    if let Some(tag) = tag {
        deser_func.line(&format!("let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{}\"))?;", ident));
        let mut tag_check = Block::new(&format!("if tag != {}", tag));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}));", ident, tag));
        deser_func.push_block(tag_check);
    }
}

// This is used mostly for when thing are tagged have specific ranges.
fn generate_wrapper_struct(gen_scope: &mut GenerationScope, types: &IntermediateTypes, type_name: &RustIdent, field_type: &RustType, min_max: Option<(Option<i128>, Option<i128>)>) {
    if min_max.is_some() {
        assert!(types.can_new_fail(type_name));
    }
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, type_name, true);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .arg("inner", field_type.for_wasm_param())
            .vis("pub");
        
        if types.can_new_fail(type_name) {
            // you can't use Self in a parameter in wasm_bindgen for some reason
            wasm_new
                .ret("Result<{}, JsValue>")
                // TODO: test
                .line("inner.try_into().map(Self).map_err(|e| JsValue::from_str(&e.to_string()))");
        } else {
            let mut ops = field_type.from_wasm_boundary_clone("inner", false);
            ops.push(ToWasmBoundaryOperations::Into);
            wasm_new
                .ret("Self")
                .line(format!("Self({})", ToWasmBoundaryOperations::format(ops.into_iter())));
        }
        let mut get = codegen::Function::new("get");
        get
            .vis("pub")
            .arg_ref_self()
            .ret(field_type.for_wasm_return());
        if field_type.directly_wasm_exposable() && !field_type.is_copy() {
            get.line(field_type.to_wasm_boundary("self.0.get().clone()", false));
        } else {
            get.line(field_type.to_wasm_boundary("self.0.get()", false));
        }
        wrapper.s_impl.push_fn(get);
        wrapper.push(gen_scope);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(types, type_name);
    s.vis("pub");
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{}Encoding", type_name)));
    let enc_fields = if CLI_ARGS.preserve_encodings {
        s.field("inner", field_type.for_rust_member(false));
        let enc_fields = encoding_fields("inner", &field_type.clone().resolve_aliases());
        
        if !enc_fields.is_empty() {
            s.field(&format!("{}encodings", encoding_var_macros(types.used_as_key(type_name))), format!("Option<{}>", encoding_name));
            let mut encoding_struct = make_encoding_struct(&encoding_name.to_string());
            for field_enc in &enc_fields {
                encoding_struct.field(&format!("pub {}", field_enc.field_name), &field_enc.type_name);
            }
            gen_scope.cbor_encodings().push_struct(encoding_struct);
        }
        Some(enc_fields)
    } else {
        s.tuple_field(field_type.for_rust_member(false));
        None
    };
    // TODO: is there a way to know if the encoding object is also copyable?
    if field_type.is_copy() && !CLI_ARGS.preserve_encodings {
        s.derive("Copy");
    }
    let (inner_var, self_var) = if CLI_ARGS.preserve_encodings {
        ("inner", "self.inner")
    } else {
        ("0", "self.0")
    };
    let mut get = codegen::Function::new("get");
    get
        .vis("pub")
        .arg_ref_self();
    if field_type.is_copy() {
        get
            .ret(field_type.for_rust_member(false))
            .line(field_type.clone_if_not_copy(self_var));
    } else {
        get
            .ret(format!("&{}", field_type.for_rust_member(false)))
            .line(format!("&{}", self_var));
    }
    s_impl.push_fn(get);
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_impl = make_serialization_impl(&type_name.to_string());
    gen_scope.generate_serialize(
        types,
        &field_type,
        &mut ser_func,
        SerializeConfig::new(self_var, "inner").is_end(true).encoding_var_in_option_struct("self.encodings"));
    ser_impl.push_fn(ser_func);
    let mut deser_func = make_deserialization_function("deserialize");
    let mut deser_impl = codegen::Impl::new(&type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let RustType::Rust(id) = field_type {
        if types.is_plain_group(id) {
            unimplemented!("TODO: make len/read_len variables of appropriate sizes so the generated code compiles");
        }
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move())
        .vis("pub");
    let var_names_str = if CLI_ARGS.preserve_encodings {
        encoding_var_names_str("inner", &field_type)
    } else {
        "inner".to_owned()
    };
    let from_impl = if let Some((min, max)) = min_max {
        let (before, after) = if var_names_str.is_empty() {
            ("".to_owned(), "")
        } else {
            (format!("let {} = ", var_names_str), ";")
        };
        gen_scope.generate_deserialize(types, field_type, "inner", &before, after, false, false, vec![], &mut deser_func, None);
        
        let against = match field_type.strip_to_serialization_type().strip_tag() {
            RustType::Primitive(p) => match p {
                Primitive::Bytes |
                Primitive::Str => "inner.len()",
                Primitive::Bool |
                Primitive::U8 |
                Primitive::U16 |
                Primitive::U32 |
                Primitive::U64 |
                Primitive::I8 |
                Primitive::I16 |
                Primitive::I32 |
                Primitive::I64 |
                Primitive::N64 => "inner",
            },
            _ => unimplemented!(),
        };
        let mut check = match (min, max) {
            (Some(min), Some(max)) => if min == max {
                Block::new(&format!("if {} != {}", against, min))
            } else {
                let non_negative = match field_type.strip_to_serialization_type().strip_tag() {
                    RustType::Primitive(p) => match p {
                        Primitive::Bytes |
                        Primitive::Str => true,
                        Primitive::Bool |
                        Primitive::U8 |
                        Primitive::U16 |
                        Primitive::U32 |
                        Primitive::U64 => true,
                        Primitive::I8 |
                        Primitive::I16 |
                        Primitive::I32 |
                        Primitive::I64 |
                        Primitive::N64 => false,
                    },
                    _ => unimplemented!(),
                };
                if min == 0 && non_negative {
                    Block::new(&format!("if {} > {}", against, max))
                } else {
                    Block::new(&format!("if {} < {} || {} > {}", against, min, against, max))
                }
            },
            (Some(min), None) => Block::new(&format!("if {} < {}", against, min)),
            (None, Some(max)) => Block::new(&format!("if {} > {}", against, max)),
            (None, None) => panic!("How did we end up with a range requirement of (None, None)? Entire thing should've been None then"),
        };
        check.line(format!(
            "return Err(DeserializeError::new(\"{}\", DeserializeFailure::RangeCheck{{ found: {}, min: {}, max: {} }}));",
            type_name,
            against,
            match min {
                Some(min) => format!("Some({})", min),
                None => String::from("None")
            },
            match max {
                Some(max) => format!("Some({})", max),
                None => String::from("None")
            }));
        deser_func.push_block(check.clone());
        new_func
            .ret("Result<Self, DeserializeError>")
            .push_block(check);
        if let Some(enc_fields) = &enc_fields {
            let mut deser_ctor = codegen::Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = codegen::Block::new(&format!("encodings: Some({}", encoding_name));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_func.push_block(deser_ctor);

            let mut ctor_block = codegen::Block::new("Ok(Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            ctor_block.after(")");
            new_func.push_block(ctor_block);
        } else {
            deser_func.line("Ok(Self(inner))");
            new_func.line("Ok(Self(inner))");
        }
        let mut try_from = codegen::Impl::new(&type_name.to_string());
        try_from
            .associate_type("Error", "DeserializeError")
            .impl_trait(format!("TryFrom<{}>", field_type.for_rust_member(false)))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_member(false))
            .ret("Result<Self, Self::Error>")
            .line(format!("{}::new({})", type_name, ToWasmBoundaryOperations::format(field_type.from_wasm_boundary_clone("inner", false).into_iter())));
        try_from
    } else {
        // let field_type_tagged = if let Some(t) = tag {
        //     RustType::Tagged(t, Box::new(field_type.clone()))
        // } else {
        //     field_type.clone()
        // };
        // gen_scope.generate_deserialize(types, &field_type_tagged, "inner", "Ok(Self(", "))", false, false, true, &mut deser_func);
        new_func.ret("Self");
        if let Some(enc_fields) = &enc_fields {
            let (before, after) = if var_names_str.is_empty() {
                ("".to_owned(), "")
            } else {
                (format!("let {} = ", var_names_str), ";")
            };
            gen_scope.generate_deserialize(types, &field_type, "inner", &before, after, false, false, vec![], &mut deser_func, None);

            let mut deser_ctor = codegen::Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = codegen::Block::new(&format!("encodings: Some({}", encoding_name));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_func.push_block(deser_ctor);

            let mut ctor_block = codegen::Block::new("Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            new_func.push_block(ctor_block);
        } else {
            gen_scope.generate_deserialize(types, &field_type, "inner", "Ok(Self(", "))", false, false, vec![], &mut deser_func, None);
            new_func.line("Self(inner)");
        }
	    
        let mut from = codegen::Impl::new(&type_name.to_string());
        from
            .impl_trait(format!("From<{}>", field_type.for_rust_member(false)))
            .new_fn("from")
            .arg("inner", field_type.for_rust_member(false))
            .ret("Self")
            .line(format!("{}::new({})", type_name, ToWasmBoundaryOperations::format(field_type.from_wasm_boundary_clone("inner", false).into_iter())));
        from
    };
    deser_impl.push_fn(deser_func);
    s_impl.push_fn(new_func);
    let mut from_inner_impl = codegen::Impl::new(field_type.for_rust_member(false));
    from_inner_impl
        .impl_trait(format!("From<{}>", type_name))
        .new_fn("from")
        .arg("wrapper", type_name.to_string())
        .ret("Self")
        .line(format!("wrapper.{}", inner_var));
    gen_scope
        .rust()
        .push_struct(s)
        .push_impl(s_impl)
        .push_impl(from_impl)
        .push_impl(from_inner_impl);
    gen_scope
        .rust_serialize()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// the derivative crate doesn't accept Eq="ignore" but omitting it
/// seems to behave correctly
fn key_derives(for_ignore: bool) -> &'static [&'static str] {
    if for_ignore {
        if CLI_ARGS.preserve_encodings {
            &["PartialEq", "Ord", "PartialOrd", "Hash"]
        } else {
            &["PartialEq", "Ord", "PartialOrd"]
        }
    } else {
        if CLI_ARGS.preserve_encodings {
            &["Eq", "PartialEq", "Ord", "PartialOrd", "Hash"]
        } else {
            &["Eq", "PartialEq", "Ord", "PartialOrd"]
        }
    }
}

fn add_struct_derives<T: DataType>(data_type: &mut T, used_in_key: bool, is_enum: bool) {
    data_type
            .derive("Clone")
            .derive("Debug");
    if CLI_ARGS.json_serde_derives {
        data_type
            .derive("serde::Deserialize")
            .derive("serde::Serialize");
    }
    if CLI_ARGS.json_schema_export {
        data_type.derive("schemars::JsonSchema");
    }
    if used_in_key {
        if CLI_ARGS.preserve_encodings {
            // there's no way to do non-derive() proc macros in the codegen
            // cate so we must sadly use a newline like this. codegen manages indentation
            data_type.derive(&format!("Derivative)]\n#[derivative({}", key_derives(false).iter().map(|tr| match *tr {
                // the derivative crate doesn't support enums tagged with ord/partialord yet without this
                "Ord" | "PartialOrd" if is_enum => format!("{}=\"feature_allow_slow_enum\"", tr),
                _ => String::from(*tr),
            }).collect::<Vec<String>>().join(", ")));
        } else {
            for key_derive in key_derives(false) {
                data_type.derive(key_derive);
            }
        }
    }
}

fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    // todo: wasm bindings + to/from string traits
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, &ident, true);
        let mut wasm_new = codegen::Function::new("new");
        let mut new_if = codegen::Block::new("if x >= 0");
        new_if.line("Self(core::Int::Uint(x as u64))");
        let mut new_else = codegen::Block::new("else");
        new_else.line("Self(core::Int::Nint((x + 1).abs() as u64))");
        wasm_new
            .ret("Self")
            .vis("pub")
            .arg("x", "i64")
            .push_block(new_if)
            .push_block(new_else);

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
            .ret("Result<Int, JsValue>")
            .line("// have to redefine so it's visible in WASM")
            .line("std::str::FromStr::from_str(string).map(Self).map_err(|e| JsValue::from_str(&format!(\"Int.from_str({}): {:?}\", string, e)))");

        wrapper
            .s_impl
            .push_fn(wasm_new)
            .push_fn(to_str)
            .push_fn(from_str);
        wrapper.push(gen_scope);
    }

    let mut native_struct = codegen::Enum::new("Int");
    native_struct.vis("pub");
    let mut uint = codegen::Variant::new("Uint");
    uint.tuple("u64");
    native_struct.push_variant(uint);
    let mut nint = codegen::Variant::new("Nint");
    nint.tuple("u64");
    native_struct.push_variant(nint);
    add_struct_derives(&mut native_struct, types.used_as_key(&ident), true);

    // serialization
    let mut ser_impl = make_serialization_impl("Int");
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_block = Block::new("match self");
    ser_block
        .line("Self::Uint(x) => serializer.write_unsigned_integer(*x),")
        .line("Self::Nint(x) => serializer.write_negative_integer(-((*x + 1) as i128) as i64),");
    ser_func.push_block(ser_block);
    ser_impl.push_fn(ser_func);

    // deserialization
    let mut deser_impl = codegen::Impl::new("Int");
    deser_impl.impl_trait("Deserialize");
    let mut deser_func = make_deserialization_function("deserialize");
    let mut annotate = make_err_annotate_block("Int", "", "");
    let mut deser_match = codegen::Block::new("match raw.cbor_type()?");
    deser_match
        .line("cbor_event::Type::UnsignedInteger => Ok(Self::Uint(raw.unsigned_integer()?)),")
        .line("cbor_event::Type::NegativeInteger => Ok(Self::Nint((-1 - raw.negative_integer()?) as u64)),")
        .line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
    annotate.push_block(deser_match);
    deser_func.push_block(annotate);
    deser_impl.push_fn(deser_func);

    // traits
    let mut int_err = codegen::Enum::new("IntError");
    int_err
        .vis("pub")
        .derive("Clone")
        .derive("Debug");
    int_err
        .new_variant("Bounds")
        .tuple("std::num::TryFromIntError");
    int_err
        .new_variant("Parsing")
        .tuple("std::num::ParseIntError");

    let mut display = codegen::Impl::new("Int");
    let mut display_match = codegen::Block::new("match self");
    display_match
        .line("Self::Uint(x) => write!(f, \"{}\", x),")
        .line("Self::Nint(x) => write!(f, \"-{}\", x + 1),");
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
    let mut try_from_if = codegen::Block::new("if x >= 0");
    try_from_if.line("u64::try_from(x).map(Self::Uint)");
    let mut try_from_else = codegen::Block::new("else");
    try_from_else.line("u64::try_from((x + 1).abs()).map(Self::Nint)");
    try_from_i128
        .impl_trait("TryFrom<i128>")
        .associate_type("Error", "std::num::TryFromIntError")
        .new_fn("try_from")
        .arg("x", "i128")
        .ret("Result<Self, Self::Error>")
        .push_block(try_from_if)
        .push_block(try_from_else);

    gen_scope
        .rust()
        .push_enum(native_struct)
        .push_enum(int_err)
        .push_impl(display)
        .push_impl(from_str)
        .push_impl(try_from_i128);
    gen_scope
        .rust_serialize()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}