use codegen::{Block};
use cbor_event::Type as CBORType;
use std::collections::{BTreeMap, BTreeSet};

use crate::cmd::{
    ANNOTATE_FIELDS,
    GENERATE_TO_FROM_BYTES,
    PRESERVE_ENCODINGS,
};
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
};
use crate::utils::{
    convert_to_snake_case,
};


pub struct GenerationScope {
    global_scope: codegen::Scope,
    serialize_scope: codegen::Scope,
    already_generated: BTreeSet<RustIdent>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            global_scope: codegen::Scope::new(),
            serialize_scope: codegen::Scope::new(),
            already_generated: BTreeSet::new(),
            no_deser_reasons: BTreeMap::new(),
        }
    }

    pub fn generate(&mut self, types: &IntermediateTypes) {
        for (alias, (base_type, gen_type_alias)) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if *gen_type_alias {
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
                            .scope()
                            .raw("#[wasm_bindgen]")
                            .new_fn(&convert_to_snake_case(&ident.to_string()))
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        self
                            .scope()
                            .raw(&format!("type {} = {};", ident, base_type.for_member()));
                    }
                }
            }
        }
        for (rust_ident, rust_struct) in types.rust_structs() {
            assert_eq!(rust_ident, rust_struct.ident());
            match rust_struct.variant() {
                RustStructType::Record(record) => {
                    codegen_struct(self, types, rust_ident, rust_struct.tag(), record);
                },
                RustStructType::Table { domain, range } => {
                    codegen_table_type(self, types, rust_ident, domain.clone(), range.clone(), rust_struct.tag());
                },
                RustStructType::Array { element_type } => {
                    self.generate_array_type(types, element_type.clone(), rust_ident);
                },
                RustStructType::TypeChoice { variants } => {
                    self.generate_type_choices_from_variants(types, rust_ident, variants, rust_struct.tag());
                },
                RustStructType::GroupChoice { variants, rep } => {
                    codegen_group_choices(self, types, rust_ident, variants, *rep, rust_struct.tag())
                },
                RustStructType::Wrapper{ wrapped, min_max } => {
                    generate_wrapper_struct(self, types, rust_ident, wrapped, rust_struct.tag(), *min_max);
                },
            }
        }
    }

    pub fn scope(&mut self) -> &mut codegen::Scope {
        &mut self.global_scope
    }

    pub fn serialize_scope(&mut self) -> &mut codegen::Scope {
        &mut self.serialize_scope
    }

    // is_end means the final line should evaluate to Ok(serializer), or equivalent ie dropping last ?; from line
    // expr - the name of the variable where this is accessed, e.g. "self.foo" or "field" (e.g. for if let Some(field) = self.foo)
    // var_name - For use in generating *unique* identifiers from this. Must be unique within a type, e.g. field name: for the above it would be "foo" for both
    fn generate_serialize(&mut self, types: &IntermediateTypes, rust_type: &RustType, expr: &str, var_name: &str, body: &mut dyn CodeBlock, is_end: bool) {
        //body.line(&format!("// DEBUG - generated from: {:?}", rust_type));
        let line_ender = if is_end {
            ""
        } else {
            "?;"
        };
        match rust_type {
            RustType::Fixed(value) => match value {
                FixedValue::Null => {
                    body.line(&format!("serializer.write_special(CBORSpecial::Null){}", line_ender));
                },
                FixedValue::Bool(b) => {
                    body.line(&format!("serializer.write_special(CBORSpecial::Bool({})){}", b, line_ender));
                },
                FixedValue::Uint(u) => {
                    body.line(&format!("serializer.write_unsigned_integer({}u64){}", u, line_ender));
                },
                // TODO: should this be Nint instead of Int? CDDL spec is Nint but cddl lib is Int
                FixedValue::Int(i) => {
                    body.line(&format!("serializer.write_negative_integer({}i64){}", i, line_ender));
                },
                FixedValue::Text(s) => {
                    body.line(&format!("serializer.write_text(\"{}\"){}", s, line_ender));
                },
            },
            RustType::Primitive(primitive) => match primitive {
                Primitive::Bytes => {
                    body.line(&format!("serializer.write_bytes(&{}){}", expr, line_ender));
                },
                Primitive::Str => {
                    body.line(&format!("serializer.write_text(&{}){}", expr, line_ender));
                },
                Primitive::I32 |
                Primitive::I64 => {
                    let mut pos = Block::new(&format!("if *{} >= 0", expr));
                    pos.line(format!("serializer.write_unsigned_integer(*{} as u64){}", expr, line_ender));
                    body.push_block(pos);
                    let mut neg = Block::new("else");
                    neg.line(format!("serializer.write_negative_integer(*{} as i64){}", expr, line_ender));
                    body.push_block(neg);
                },
                Primitive::N64 => {
                    body.line(&format!("serializer.write_negative_integer(*{}){}", expr, line_ender));
                },
                _ => {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                },
            },
            RustType::Rust(t) => {
                if types.is_plain_group(t) {
                    body.line(&format!("{}.serialize_as_embedded_group(serializer){}", expr, line_ender));
                } else {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                }
            },
            RustType::Array(ty) => {
                if rust_type.directly_wasm_exposable() {
                    let cbor_len = if PRESERVE_ENCODINGS {
                        format!("if self.{}_definite_encoding {{ cbor_event::Len::Len({}.len() as u64) }} else {{ cbor_event::Len::Indefinite }}", var_name, expr)
                    } else {
                        format!("cbor_event::Len::Len({}.len() as u64)", expr)
                    };
                    body.line(&format!("serializer.write_array({})?;", cbor_len));
                    let mut loop_block = Block::new(&format!("for element in {}.iter()", expr));
                    self.generate_serialize(types, ty, "element", &format!("{}_elem", var_name), &mut loop_block, false);
                    body.push_block(loop_block);
                    if PRESERVE_ENCODINGS {
                        let mut append_break = codegen::Block::new(&format!("if !self.{}_definite_encoding", var_name));
                        append_break.line("serializer.write_special(cbor_event::Special::Break)?;");
                        body.push_block(append_break);
                    }
                    if is_end {
                        body.line("Ok(serializer)");
                    }
                } else {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                }
            },
            RustType::Tagged(tag, ty) => {
                body.line(&format!("serializer.write_tag({}u64)?;", tag));
                self.generate_serialize(types, ty, expr, var_name, body, is_end);
            },
            RustType::Optional(ty) => {
                let mut opt_block = Block::new(&format!("match &{}", expr));
                // TODO: do this in one line without a block if possible somehow.
                //       see other comment in generate_enum()
                let mut some_block = Block::new("Some(x) =>");
                self.generate_serialize(types, ty, "x", var_name, &mut some_block, true);
                some_block.after(",");
                opt_block.push_block(some_block);
                opt_block.line(&format!("None => serializer.write_special(CBORSpecial::Null),"));
                if !is_end {
                    opt_block.after("?;");
                }
                body.push_block(opt_block);
            },
            RustType::Map(_key, _value) => {
                // body.line("serializer.write_map(cbor_event::Len::Indefinite)?;");
                // let mut table_loop = Block::new(&format!("for (key, value) in {}.iter()", expr));
                // self.generate_serialize(&key_type, "key", &mut table_loop, false);
                // self.generate_serialize(&value_type, "value", &mut table_loop, false);
                // body.push_block(table_loop);
                // body.line(&format!("serializer.write_special(CBORSpecial::Break){}", line_ender));
                body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
            },
            RustType::Alias(_ident, ty) => self.generate_serialize(types, ty, expr, var_name, body, is_end),
        };
    }

    // formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
    // * {let x = }{<value>}{;} - creation of variables
    // * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
    // * {}{<value>}{} - for last-expression eval in blocks
    // * etc
    // var_name is passed in for use in creating unique identifiers for temporaries
    // if force_non_embedded always deserialize as the outer wrapper, not as the embedded plain group when the Rust ident is for a plain group
    fn generate_deserialize(&mut self, types: &IntermediateTypes, rust_type: &RustType, var_name: &str, before: &str, after: &str, in_embedded: bool, optional_field: bool, body: &mut dyn CodeBlock) {
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        match rust_type {
            RustType::Fixed(f) => {
                // we don't evaluate to any values here, just verify
                // before/after are ignored and we need to handle fixed value deserialization in a different way
                // than normal ones.
                assert_eq!(after, "");
                assert_eq!(before, "");
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                match f {
                    FixedValue::Null => {
                        let mut special_block = Block::new("if raw.special()? != CBORSpecial::Null");
                        special_block.line("return Err(DeserializeFailure::ExpectedNull.into());");
                        body.push_block(special_block);
                    },
                    FixedValue::Uint(x) => {
                        body.line(&format!("let {}_value = raw.unsigned_integer()?;", var_name));
                        let mut compare_block = Block::new(&format!("if {}_value != {}", var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint({}_value), expected: Key::Uint({}) }}.into());", var_name, x));
                        body.push_block(compare_block);
                    },
                    FixedValue::Text(x) => {
                        body.line(&format!("let {}_value = raw.text()?;", var_name));
                        let mut compare_block = Block::new(&format!("if {}_value != \"{}\"", var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Str({}_value), expected: Key::Str(String::from(\"{}\")) }}.into());", var_name, x));
                        body.push_block(compare_block);
                    },
                    _ => unimplemented!(),
                }
            },
            RustType::Primitive(p) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                match p {
                    Primitive::Bytes => {
                        body.line(&format!("{}raw.bytes()?{}", before, after));
                    },
                    Primitive::I32 |
                    Primitive::I64 => {
                        let mut sign = Block::new(&format!("{}match raw.unsigned_integer()", before));
                        sign.line(format!("Ok(x) => x as {},", p.to_string()));
                        sign.line(format!("Err(_) => raw.negative_integer()? as {},", p.to_string()));
                        sign.after(after);
                        body.push_block(sign);
                    },
                    Primitive::N64 => {
                        body.line(&format!("{}raw.negative_integer(){}", before, after));
                    },
                    _ => {
                        body.line(&format!("{}{}::deserialize(raw)?{}", before, p.to_string(), after));
                    },
                }
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
                body.line(&format!("{}{}::deserialize_as_embedded_group(raw, {}, len)?{}", before, ident, pass_read_len, after));
            } else {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                body.line(&format!("{}{}::deserialize(raw)?{}", before, ident, after));
            },
            RustType::Tagged(tag, ty) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                let mut tag_check = Block::new(&format!("{}match raw.tag()?", before));
                let mut deser_block = Block::new(&format!("{} =>", tag));
                self.generate_deserialize(types, ty, var_name, "", "", in_embedded, false, &mut deser_block);
                deser_block.after(",");
                tag_check.push_block(deser_block);
                tag_check.line(&format!("tag => return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),", tag));
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
                    special_block.line("let special = raw.special()?;");
                    special_block.line("raw.as_mut_ref().seek(SeekFrom::Current(-1)).unwrap();");
                    let mut special_match = Block::new("match special()");
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
                    String::from("raw.cbor_type()? != CBORType::Special")
                };
                let mut deser_block = Block::new(&format!("{}match {}", before, if_label));
                let mut some_block = Block::new("true =>");
                if read_len_check {
                    let mandatory_fields = ty.expanded_mandatory_field_count(types);
                    if mandatory_fields != 0 {
                        some_block.line(format!("read_len.read_elems({})?;", mandatory_fields));
                    }
                }
                self.generate_deserialize(types, ty, var_name, "Some(", ")", in_embedded, false, &mut some_block);
                some_block.after(",");
                deser_block.push_block(some_block);
                let mut none_block = Block::new("false =>");
                if read_len_check {
                    none_block.line("read_len.read_elems(1)?;");
                }
                self.generate_deserialize(types, &RustType::Fixed(FixedValue::Null), var_name, "", "", in_embedded, false, &mut none_block);
                none_block.line("None");
                deser_block.after(after);
                deser_block.push_block(none_block);
                body.push_block(deser_block);
            },
            RustType::Array(ty) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                if rust_type.directly_wasm_exposable() {
                    body.line("let mut arr = Vec::new();");
                    body.line("let len = raw.array()?;");
                    if PRESERVE_ENCODINGS {
                        body.line(&format!("{}_definite_encoding = len != cbor_event::Len::Indefinite;", var_name));
                    }
                    let mut deser_loop = make_deser_loop("len", "arr.len()");
                    deser_loop.push_block(make_deser_loop_break_check());
                    if let RustType::Rust(ty_ident) = &**ty {
                        // TODO: properly handle which read_len would be checked here.
                        assert!(!types.is_plain_group(&*ty_ident));
                    }
                    self.generate_deserialize(types, ty, &format!("{}_elem", var_name), "arr.push(", ");", in_embedded, false, &mut deser_loop);
                    body.push_block(deser_loop);
                    body.line(&format!("{}arr{}", before, after));
                } else {
                    // a wrapper type was already generated - so just use that
                    body.line(&format!("{}{}::deserialize(raw)?{}", before, rust_type.for_member(), after));
                }
            },
            RustType::Map(_key_type, _value_type) => {
                if optional_field {
                    body.line("read_len.read_elems(1)?;");
                }
                // I don't think this will ever be used since we always generate a wrapper type for this
                // so that we can expose it to wasm. It could be later if someone needs that later (for a non-wasm feature maybe)
                //     body.line(&format!("let mut table = {}::new();", rust_type.for_member()));
                //     body.line("let len = raw.map()?;");
                //     let deser_loop = make_table_deser_loop(self, types, key_type, value_type, "len", "table");
                //     body.push_block(deser_loop);
                //     body.line(&format!("{}table{}", before, after));
                // so instead we just generate this:
                body.line(&format!("{}{}::deserialize(raw)?{}", before, rust_type.for_member(), after));
            },
            RustType::Alias(_ident, ty) => self.generate_deserialize(types, ty, var_name, before, after, in_embedded, optional_field, body),
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
        // Handle group with choices by generating an enum then generating a group for every choice
        let enum_name = RustIdent::new(CDDLIdent::new(format!("{}Enum", name)));
        let kind_name = RustIdent::new(CDDLIdent::new(format!("{}Kind", name)));
        generate_enum(self, types, &enum_name, &kind_name, &variants, None, true);

        // Now generate a wrapper object that we will expose to wasm around this
        let (mut s, mut s_impl) = create_empty_exposed_struct(self, name);
        let (mut ser_func, mut ser_impl) = create_serialize_impl(name, None, tag, None, None);
        s
            .vis("pub")
            .tuple_field(&enum_name.to_string());
        // new
        for variant in variants.iter() {
            let variant_arg = convert_to_snake_case(&variant.name.to_string());
            let mut new_func = codegen::Function::new(&format!("new_{}", variant_arg));
            new_func
                .ret("Self")
                .vis("pub");
            if variant.rust_type.is_fixed_value() {
                new_func.line(format!("Self({}::{})", enum_name, variant.name));
            } else {
                // can't use (rust) reserved keywords as param: eg new_u32(u32: u32)
                // TODO: do we need to cover any other (rust) reserved keywords?
                let arg_name = match variant_arg.as_str() {
                    "u8" | "u16" | "u32" | "u64" => "uint",
                    "i8" | "i16" | "i32" | "i64" => "int",
                    x => x,
                };
                new_func
                    .arg(&arg_name, &variant.rust_type.for_wasm_param())
                    .line(format!("Self({}::{}({}))", enum_name, variant.name, variant.rust_type.from_wasm_boundary_clone(arg_name)));
            }
            s_impl.push_fn(new_func);
        }
        // serialize
        ser_func.line("self.0.serialize(serializer)");
        ser_impl.push_fn(ser_func);
        // enum-getters
        add_enum_getters(&mut s_impl, &enum_name, &kind_name, &variants);
        push_exposed_struct(self, &enum_name, s, s_impl, ser_impl, None);
        // deserialize
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize");
        deser_func.line(format!("Ok(Self({}::deserialize(raw)?))", enum_name));
        deser_impl.push_fn(deser_func);
        self.serialize_scope().push_impl(deser_impl);
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(&mut self, types: &IntermediateTypes, element_type: RustType, array_type_ident: &RustIdent) {
        let element_type_rust = element_type.for_member();
        if self.already_generated.insert(array_type_ident.clone()) {
            let mut s = codegen::Struct::new(&array_type_ident.to_string());
            s.vis("pub");
            if PRESERVE_ENCODINGS {
                s
                    .field("elems", format!("Vec<{}>", element_type_rust))
                    .field("definite_encoding", "bool");
            } else {
                s.tuple_field(format!("Vec<{}>", element_type_rust));
            }
            let elems_field = if PRESERVE_ENCODINGS { "self.elems" } else { "self.0" };
            add_struct_derives(&mut s);
            self.global_scope.raw("#[wasm_bindgen]");
            self.global_scope.push_struct(s);
            // serialize
            let mut ser_impl = codegen::Impl::new(&array_type_ident.to_string());
            ser_impl.impl_trait("cbor_event::se::Serialize");
            let mut ser_func = make_serialization_function("serialize");
            if PRESERVE_ENCODINGS {
                let mut encoding_if = codegen::Block::new("if self.definite_encoding");
                encoding_if.line("serializer.write_array(cbor_event::Len::Len(self.elems.len() as u64))?;");
                let mut encoding_else = codegen::Block::new("else");
                encoding_else.line("serializer.write_array(cbor_event::Len::Indefinite)?;");
                ser_func
                    .push_block(encoding_if)
                    .push_block(encoding_else);

            } else {
                ser_func.line("serializer.write_array(cbor_event::Len::Len(self.0.len() as u64))?;");
            }
            let mut loop_block = Block::new(&format!("for element in &{}", elems_field));
            self.generate_serialize(types, &element_type, "element", &array_type_ident.to_string(), &mut loop_block, false);
            ser_func.push_block(loop_block);
            if PRESERVE_ENCODINGS {
                let mut final_break = codegen::Block::new("if !self.definite_encoding");
                final_break.line("serializer.write_special(cbor_event::Special::Break)?;");
                ser_func.push_block(final_break);
            }
            ser_func.line("Ok(serializer)");
            ser_impl.push_fn(ser_func);
            self.serialize_scope.push_impl(ser_impl);
            // deserialize
            if self.deserialize_generated_for_type(&element_type) {
                let mut deser_impl = codegen::Impl::new(&array_type_ident.to_string());
                deser_impl.impl_trait("Deserialize");
                let mut deser_func = make_deserialization_function("deserialize");
                deser_func.line("let mut arr = Vec::new();");
                if PRESERVE_ENCODINGS {
                    deser_func.line("let mut definite_encoding = true;");
                }
                let mut error_annotator = make_err_annotate_block(&array_type_ident.to_string(), "", "?;");
                let deser_body: &mut dyn CodeBlock = if ANNOTATE_FIELDS {
                    &mut error_annotator
                } else {
                    &mut deser_func
                };
                deser_body.line("let len = raw.array()?;");
                if PRESERVE_ENCODINGS {
                    deser_body.line("definite_encoding = len != cbor_event::Len::Indefinite;");
                }
                let mut deser_loop = make_deser_loop("len", "arr.len()");
                deser_loop.push_block(make_deser_loop_break_check());
                self.generate_deserialize(types, &element_type, &array_type_ident.to_string(), "arr.push(", ");", false, false, &mut deser_loop);
                deser_body.push_block(deser_loop);
                if ANNOTATE_FIELDS {
                    error_annotator.line("Ok(())");
                    deser_func.push_block(error_annotator);
                }
                if PRESERVE_ENCODINGS {
                    let mut deser_self_create = codegen::Block::new("Ok(Self");
                    deser_self_create
                        .line("elems: arr,")
                        .line("definite_encoding")
                        .after(")");
                    deser_func.push_block(deser_self_create);
                } else {
                    deser_func.line("Ok(Self(arr))");
                }
                deser_impl.push_fn(deser_func);
                self.serialize_scope().push_impl(deser_impl);
            } else {
                self.dont_generate_deserialize(&array_type_ident, format!("inner type {} doesn't support deserialize", element_type.for_member()));
            }
            // other functions
            let mut array_impl = codegen::Impl::new(&array_type_ident.to_string());
            let mut new_func = codegen::Function::new("new");
            new_func
                .vis("pub")
                .ret("Self");
            if PRESERVE_ENCODINGS {
                let mut new_self_create = codegen::Block::new("Self");
                new_self_create
                    .line("elems: Vec::new(),")
                    .line("definite_encoding: true");
                new_func.push_block(new_self_create);
            } else {
                new_func.line("Self(Vec::new())");
            }
            array_impl
                .new_fn("len")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .line(format!("{}.len()", elems_field));
            array_impl
                .new_fn("get")
                .vis("pub")
                .ret(&element_type.for_wasm_return())
                .arg_ref_self()
                .arg("index", "usize")
                .line(format!("{}[index].clone()", elems_field));
            array_impl
                .new_fn("add")
                .vis("pub")
                .arg_mut_self()
                .arg("elem", element_type.for_wasm_param())
                .line(format!("{}.push({});", elems_field, element_type.from_wasm_boundary_clone("elem")));
            self.global_scope.raw("#[wasm_bindgen]");
            self.global_scope.push_impl(array_impl);
        }
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

fn create_empty_exposed_struct(gen_scope: &GenerationScope, ident: &RustIdent) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(&mut s);
    let mut group_impl = codegen::Impl::new(name);
    // There are auto-implementing ToBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if GENERATE_TO_FROM_BYTES {
        group_impl
            .new_fn("to_bytes")
            .ret("Vec<u8>")
            .arg_ref_self()
            .vis("pub")
            .line("ToBytes::to_bytes(self)");
        if gen_scope.deserialize_generated(ident) {
            group_impl
                .new_fn("from_bytes")
                .ret(format!("Result<{}, JsValue>", name))
                .arg("data", "Vec<u8>")
                .vis("pub")
                .line("FromBytes::from_bytes(data)");
        }
    }
    (s, group_impl)
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
    let mut ser_impl = codegen::Impl::new(name);
    ser_impl.impl_trait("cbor_event::se::Serialize");
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    // TODO: do definite length encoding for optional fields too
    if let Some (rep) = rep {
        let cbor_len_str = if let Some(definite) = use_this_encoding {
            let mut len_code = codegen::Block::new(&format!("let cbor_len = match {}", definite));
            len_code
                .line(format!("true => cbor_event::Len::Len({}),", definite_len.as_ref().unwrap()))
                .line("false => cbor_event::Len::Indefinite,")
                .after(";");
            ser_func.push_block(len_code);
            String::from("cbor_len")
        } else {
            match &definite_len {
                Some(fixed_field_count) => format!("cbor_event::Len::Len({})", fixed_field_count),
                None => String::from("cbor_event::Len::Indefinite"),
            }
        };
        match rep {
            Representation::Array => ser_func.line(format!("serializer.write_array({})?;", cbor_len_str)),
            Representation::Map => ser_func.line(format!("serializer.write_map({})?;", cbor_len_str)),
        };
        if generate_serialize_embedded {
            match definite_len {
                Some(_) => ser_func.line("self.serialize_as_embedded_group(serializer)"),
                None => {
                    ser_func.line("self.serialize_as_embedded_group(serializer)?;");
                    ser_func.line("serializer.write_special(CBORSpecial::Break)")
                },
            };
        }
    } else {
        // not array or map, generate serialize directly
        if generate_serialize_embedded {
            ser_func.line("self.serialize_as_embedded_group(serializer)");
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
            end_len_check.line(format!("cbor_event::Len::Len(_) => {},", ending_check));
            let mut indefinite_check = Block::new("cbor_event::Len::Indefinite => match raw.special()?");
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
fn create_deserialize_impls(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, len_info: RustStructCBORLen, generate_deserialize_embedded: bool, store_encoding: Option<&str>, deser_body: &mut dyn CodeBlock) -> (codegen::Impl, Option<codegen::Impl>) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Cusom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    if let Some(tag) = tag {
        deser_body.line("let tag = raw.tag()?;");
        let mut tag_check = Block::new(&format!("if tag != {}", tag));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}));", name, tag));
        deser_body.push_block(tag_check);
    }
    if let Some (rep) = rep {
        match rep {
            Representation::Array => {
                deser_body.line("let len = raw.array()?;");
                if let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!("let {} = len != cbor_event::Len::Indefinite;", encoding_var_name));
                }
                add_deserialize_initial_len_check(deser_body, len_info);
                if generate_deserialize_embedded {
                    deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);");
                }
            },
            Representation::Map => {
                deser_body.line("let len = raw.map()?;");
                if let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!("let {} = len != cbor_event::Len::Indefinite;", encoding_var_name));
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

fn push_exposed_struct(
    gen_scope: &mut GenerationScope,
    name: &RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    ser_impl: codegen::Impl,
    ser_embedded_impl: Option<codegen::Impl>,
) {
    gen_scope
        .scope()
        .raw("#[wasm_bindgen]")
        .push_struct(s);

    //gen_scope.scope().raw(&format!("to_from_bytes!({});", name))
    gen_scope
        .scope()
        .raw("#[wasm_bindgen]")
        .push_impl(s_impl);
    gen_scope.serialize_scope()
        .push_impl(ser_impl);
    if let Some(s) = ser_embedded_impl {
        gen_scope.serialize_scope().push_impl(s);
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
    Block::new(&format!("while match {} {{ cbor_event::Len::Len(n) => {} < n as usize, cbor_event::Len::Indefinite => true, }}", len_var, len_expr))
}

fn make_deser_loop_break_check() -> Block {
    let mut break_check = Block::new("if raw.cbor_type()? == CBORType::Special");
    // TODO: read special and go back 1 character
    break_check.line("assert_eq!(raw.special()?, CBORSpecial::Break);");
    break_check.line("break;");
    break_check
}

fn make_table_deser_loop(gen_scope: &mut GenerationScope, types: &IntermediateTypes, key_type: &RustType, value_type: &RustType, len_var: &str, table_var: &str) -> Block {
    let mut deser_loop = make_deser_loop(len_var, &format!("{}.len()", table_var));
    deser_loop.push_block(make_deser_loop_break_check());
    gen_scope.generate_deserialize(types, key_type, "key", "let key = ", ";", false, false, &mut deser_loop);
    gen_scope.generate_deserialize(types, value_type, "value", "let value = ", ";", false, false, &mut deser_loop);
    let mut dup_check = Block::new(&format!("if {}.insert(key.clone(), value).is_some()", table_var));
    let dup_key_error_key = match key_type {
        RustType::Primitive(Primitive::U32) |
        RustType::Primitive(Primitive::U64) => "Key::Uint(key.into())",
        RustType::Primitive(Primitive::Str) => "Key::Str(key)",
        // TODO: make a generic one then store serialized CBOR?
        _ => "Key::Str(String::from(\"some complicated/unsupported type\"))",
    };
    dup_check.line(format!("return Err(DeserializeFailure::DuplicateKey({}).into());", dup_key_error_key));
    deser_loop.push_block(dup_check);
    deser_loop
}

fn codegen_table_type(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, key_type: RustType, value_type: RustType, tag: Option<usize>) {
    // this would interfere with loop code generation unless we
    // specially handle this case since you wouldn't know whether you hit a break
    // or are reading a key here, unless we check, but then you'd need to store the
    // non-break special value once read
    assert!(!key_type.cbor_types().contains(&CBORType::Special));
    let (mut s, mut s_impl) = create_empty_exposed_struct(gen_scope, name);
    let (elems_field, use_this_encoding) = if PRESERVE_ENCODINGS {
        ("self.elems", Some("self.definite_encoding"))
    } else {
        ("self.0", None)
    };
    let (mut ser_func, mut ser_impl) = create_serialize_impl(name, Some(Representation::Map), tag, Some(format!("{}.len() as u64", elems_field)), use_this_encoding);
    s.vis("pub");
    if PRESERVE_ENCODINGS {
        s
            .field("elems", format!("linked_hash_map::LinkedHashMap<{}, {}>", key_type.for_member(), value_type.for_member()))
            .field("definite_encoding", "bool");
    } else {
        s.tuple_field(format!("std::collections::BTreeMap<{}, {}>", key_type.for_member(), value_type.for_member()));
    }
    // new
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self");
    let map_type = if PRESERVE_ENCODINGS { "linked_hash_map::LinkedHashMap" } else { "std::collections::BTreeMap" };
    if PRESERVE_ENCODINGS {
        let mut new_self_create = codegen::Block::new("Self");
        new_self_create
            .line(format!("elems: {}::new(),", map_type))
            .line("definite_encoding: true,");
        new_func.push_block(new_self_create);
    } else {
        new_func.line(format!("Self({}::new())", map_type));
    }
    s_impl.push_fn(new_func);
    // len
    s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line(format!("{}.len()", elems_field));
    // insert
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm_param())
        .arg("value", value_type.for_wasm_param())
        .ret(format!("Option<{}>", value_type.for_wasm_return()))
        .line(
            format!(
                "{}.insert({}, {})",
                elems_field,
                key_type.from_wasm_boundary_clone("key"),
                value_type.from_wasm_boundary_clone("value")));
    s_impl.push_fn(insert_func);
    // get
    let mut getter = codegen::Function::new("get");
    getter
        .arg_ref_self()
        .arg("key", key_type.for_wasm_param())
        .ret(format!("Option<{}>", value_type.for_wasm_return()))
        .vis("pub")
        .line(format!("{}.get({}).map(|v| v.clone())", elems_field, key_type.from_wasm_boundary_ref("key")));
    s_impl.push_fn(getter);
    // keys
    let keys_type = RustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys
        .arg_ref_self()
        .ret(keys_type.for_wasm_return())
        .vis("pub");
    if keys_type.directly_wasm_exposable() {
        keys.line(format!("{}.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>()", elems_field));
    } else {
        if PRESERVE_ENCODINGS {
            let mut keys_create = codegen::Block::new(&keys_type.for_wasm_return());
            keys_create
                .line(format!("elems: {}.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>(),", elems_field))
                .line("definite_encoding: true,");
            keys.push_block(keys_create);
        } else {
            keys.line(format!("{}({}.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>())", keys_type.for_wasm_return(), elems_field));
        }
    }
    s_impl.push_fn(keys);
    // serialize
    let mut ser_loop = Block::new(&format!("for (key, value) in &{}", elems_field));
    gen_scope.generate_serialize(types, &key_type, "key", &format!("{}_key", name), &mut ser_loop, false);
    gen_scope.generate_serialize(types, &value_type, "value", &format!("{}_value", name), &mut ser_loop, false);
    ser_func.push_block(ser_loop);
    ser_func.line("Ok(serializer)");
    ser_impl.push_fn(ser_func);
    push_exposed_struct(gen_scope, name, s, s_impl, ser_impl, None);
    // deserialize
    if !gen_scope.deserialize_generated_for_type(&key_type) {
        gen_scope.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_member()));
    } else if !gen_scope.deserialize_generated_for_type(&value_type) {
        gen_scope.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_member()));
    } else {
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize");
        deser_func.line(format!("let mut table = {}::new();", map_type));
        if PRESERVE_ENCODINGS {
            deser_func.line("let mut definite_encoding = true;");
        }
        let mut error_annotator = make_err_annotate_block(&name.to_string(), "", "?;");
        let deser_body: &mut dyn CodeBlock = if ANNOTATE_FIELDS {
            &mut error_annotator
        } else {
            &mut deser_func
        };
        if let Some(tag) = tag {
            deser_body.line("let tag = raw.tag()?;");
            let mut tag_check = Block::new(&format!("if tag != {}", tag));
            tag_check.line(&format!("return Err(DeserializeError::new(\"{}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}));", name, tag));
            deser_body.push_block(tag_check);
        }
        deser_body.line("let len = raw.map()?;");
        let deser_loop = make_table_deser_loop(gen_scope, types, &key_type, &value_type, "len", "table");
        deser_body.push_block(deser_loop);
        if ANNOTATE_FIELDS {
            error_annotator.line("Ok(())");
            deser_func.push_block(error_annotator);
        }
        if PRESERVE_ENCODINGS {
            let mut deser_self_create = codegen::Block::new("Ok(Self");
            deser_self_create
                .line("elems: table,")
                .line("definite_encoding,")
                .after(")");
            deser_func.push_block(deser_self_create);
        } else {
            deser_func.line("Ok(Self(table))");
        }
        deser_impl.push_fn(deser_func);
        gen_scope.serialize_scope().push_impl(deser_impl);
    }
}

fn codegen_struct(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, tag: Option<usize>, rust_struct: &RustRecord) {
    let (mut s, mut s_impl) = create_empty_exposed_struct(gen_scope, name);
    s.vis("pub");

    // Generate struct + fields + constructor
    let mut new_func = codegen::Function::new("new");
    new_func
        .ret("Self")
        .vis("pub");
    let mut new_func_block = Block::new("Self");
    for field in &rust_struct.fields {
        if !gen_scope.deserialize_generated_for_type(&field.rust_type) {
            gen_scope.dont_generate_deserialize(name, format!("field {}: {} couldn't generate serialize", field.name, field.rust_type.for_member()));
        }
        // Fixed values don't need constructors or getters or fields in the rust code
        if !field.rust_type.is_fixed_value() {
            if field.optional {
                // field
                s.field(&field.name, format!("Option<{}>", field.rust_type.for_member()));
                // new
                new_func_block.line(format!("{}: None,", field.name));
                // setter
                let mut setter = codegen::Function::new(&format!("set_{}", field.name));
                setter
                    .arg_mut_self()
                    .arg(&field.name, &field.rust_type.for_wasm_param())
                    .vis("pub")
                    .line(format!("self.{} = Some({})", field.name, field.rust_type.from_wasm_boundary_clone(&field.name)));
                s_impl.push_fn(setter);
                // getter
                let mut getter = codegen::Function::new(&field.name);
                getter
                    .arg_ref_self()
                    .ret(format!("Option<{}>", field.rust_type.for_wasm_return()))
                    .vis("pub")
                    .line(format!("self.{}.clone()", field.name));
                s_impl.push_fn(getter);
            } else {
                // field
                s.field(&field.name, field.rust_type.for_member());
                // new
                new_func.arg(&field.name, field.rust_type.for_wasm_param());
                new_func_block.line(format!("{}: {},", field.name, field.rust_type.from_wasm_boundary_clone(&field.name)));
                // do we want setters here later for mandatory types covered by new?
                // getter
                let mut getter = codegen::Function::new(&field.name);
                getter
                    .arg_ref_self()
                    .ret(field.rust_type.for_wasm_return())
                    .vis("pub")
                    .line(format!("self.{}.clone()", field.name));
                s_impl.push_fn(getter);
            }
            if let RustType::Array(_) = &field.rust_type {
                if PRESERVE_ENCODINGS && field.rust_type.directly_wasm_exposable() {
                    s.field(&format!("{}_definite_encoding", field.name), "bool");
                    new_func_block.line(format!("{}_definite_encoding: true,", field.name));
                }
            }
        }
    }
    let encoding_var = if PRESERVE_ENCODINGS {
        s.field("definite_encoding", "bool");
        new_func_block.line("definite_encoding: true,");
        if rust_struct.rep == Representation::Map {
            s.field("orig_deser_order", "Option<Vec<usize>>");
            new_func_block.line("orig_deser_order: None,");
        }
        Some("definite_encoding")
    } else {
        None
    };
    new_func.push_block(new_func_block);
    s_impl.push_fn(new_func);

    // Generate serialization
    let (ser_func, mut ser_impl, mut ser_embedded_impl) =
        create_serialize_impls(
            name,
            Some(rust_struct.rep),
            tag,
            rust_struct.definite_info(types),
            encoding_var.map(|var| format!("self.{}", var)).as_deref(),
            types.is_plain_group(name));
    let mut deser_f = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(&name.to_string(), "", "");
    let deser_body: &mut dyn CodeBlock = if ANNOTATE_FIELDS {
        &mut error_annotator
    } else {
        &mut deser_f
    };
    let (mut deser_impl, mut deser_embedded_impl) =
        create_deserialize_impls(
            name,
            Some(rust_struct.rep),
            tag,
            rust_struct.cbor_len_info(types),
            types.is_plain_group(name),
            encoding_var,
            deser_body);
    let mut deser_f = match deser_embedded_impl {
        Some(_) => {
            if ANNOTATE_FIELDS {
                // rustc complains about it being moved here and used elsewhere, but
                // that can never happen so let's just clone it here.
                // We need these to be in 2 separate blocks so we can borrow the correct
                // f here below.
                deser_f.push_block(error_annotator.clone());
            }
            deser_impl.push_fn(deser_f);
            let mut f = make_deserialization_function("deserialize_as_embedded_group");
            f
                .arg("read_len", "&mut CBORReadLen")
                .arg("len", "cbor_event::Len");
            f
        },
        None => deser_f,
    };
    let deser_body: &mut dyn CodeBlock = match deser_embedded_impl {
        Some(_) => &mut deser_f,
        None => if ANNOTATE_FIELDS {
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
    let ctor_block = match rust_struct.rep {
        Representation::Array => {
            let mut deser_ret = Block::new(&format!("Ok({}", name));
            for field in rust_struct.fields.iter() {
                if field.optional {
                    let mut optional_array_ser_block = Block::new(&format!("if let Some(field) = &self.{}", field.name));
                    gen_scope.generate_serialize(types, &field.rust_type, "field", &field.name, &mut optional_array_ser_block, false);
                    ser_func.push_block(optional_array_ser_block);
                    gen_scope.dont_generate_deserialize(name, format!("Array with optional field {}: {}", field.name, field.rust_type.for_member()));
                } else {
                    gen_scope.generate_serialize(types, &field.rust_type, &format!("self.{}", field.name), &field.name, &mut ser_func, false);
                    if field.rust_type.is_fixed_value() {
                        // don't set anything, only verify data
                        if ANNOTATE_FIELDS {
                            let mut err_deser = make_err_annotate_block(&field.name, "", "?;");
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, false, &mut err_deser);
                            // this block needs to evaluate to a Result even though it has no value
                            err_deser.line("Ok(())");
                            deser_body.push_block(err_deser);
                        } else {
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, false, deser_body);
                        }
                    } else {
                        if ANNOTATE_FIELDS {
                            let mut err_deser = make_err_annotate_block(&field.name, &format!("let {} = ", field.name), "?;");
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, false, &mut err_deser);
                            deser_body.push_block(err_deser);
                        } else {
                            gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &format!("let {} = ", field.name), ";", in_embedded, false, deser_body);
                        }
                    }
                    if !field.rust_type.is_fixed_value() {
                        deser_ret.line(&format!("{},", field.name));
                    }
                }
            }
            if let Some(var_name) = encoding_var {
                deser_ret.line(format!("{},", var_name));
            }
            // length checked inside of deserialize() - it causes problems for plain groups nested
            // in other groups otherwise
            deser_ret.after(")");
            deser_ret
        },
        Representation::Map => {
            let mut uint_field_deserializers = Vec::new();
            let mut text_field_deserializers = Vec::new();
            // indexed by the order in {fields}
            let mut ser_content: Vec<BlocksOrLines> = Vec::new();
            deser_body.line("let mut orig_deser_order = Vec::new();");
            for (field_index, field) in rust_struct.fields.iter().enumerate() {
                // to support maps with plain groups inside is very difficult as we cannot guarantee
                // the order of fields so foo = {a, b, bar}, bar = (c, d) could have the order ben
                // {a, d, c, b}, {c, a, b, d}, etc which doesn't fit with the nature of deserialize_as_embedded_group
                // A possible solution would be to take all fields into one big map, either in generation to begin with,
                // or just for deserialization then constructing at the end with locals like a, b, bar_c, bar_d.
                if let RustType::Rust(ident) = &field.rust_type {
                    if types.is_plain_group(&ident) {
                        gen_scope.dont_generate_deserialize(name, format!("Map with plain group field {}: {}", field.name, field.rust_type.for_member()));
                    }
                }
                if field.rust_type.is_fixed_value() {
                    deser_body.line(&format!("let mut {}_present = false;", field.name));
                } else {
                    deser_body.line(&format!("let mut {} = None;", field.name));
                }
                if let RustType::Array(_) = &field.rust_type {
                    if PRESERVE_ENCODINGS && field.rust_type.directly_wasm_exposable() {
                        deser_body.line(&format!("let mut {}_definite_encoding = true;", field.name));
                    }
                }
                let data_name = if field.optional {
                    String::from("field")
                } else {
                    format!("self.{}", field.name)
                };

                let key = field.key.clone().unwrap();
                // deserialize key + value
                let mut deser_block = match &key {
                    FixedValue::Uint(x) => Block::new(&format!("{} => ", x)),
                    FixedValue::Text(x) => Block::new(&format!("\"{}\" => ", x)),
                    _ => panic!("unsupported map key type for {}.{}: {:?}", name, field.name, key),
                };
                deser_block.after(",");
                let key_in_rust = match &key {
                    FixedValue::Uint(x) => format!("Key::Uint({})", x),
                    FixedValue::Text(x) => format!("Key::Str(\"{}\".into())", x),
                    _ => unimplemented!(),
                };
                if field.rust_type.is_fixed_value() {
                    let mut dup_check = Block::new(&format!("if {}_present", field.name));
                    dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                    deser_block.push_block(dup_check);
                    // only does verification and sets the field_present bool to do error checking later
                    if ANNOTATE_FIELDS {
                        let mut err_deser = make_err_annotate_block(&field.name, &format!("{}_present = ", field.name), "?;");
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, field.optional, &mut err_deser);
                        err_deser.line("Ok(true)");
                        deser_block.push_block(err_deser);
                    } else {
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "", "", in_embedded, field.optional, &mut deser_block);
                        deser_block.line(&format!("{}_present = true;", field.name));
                    }
                } else {
                    let mut dup_check = Block::new(&format!("if {}.is_some()", field.name));
                    dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                    deser_block.push_block(dup_check);
                    if ANNOTATE_FIELDS {
                        let mut err_deser = make_err_annotate_block(&field.name, &format!("{} = Some(", field.name), "?);");
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, "Ok(", ")", in_embedded, field.optional, &mut err_deser);
                        deser_block.push_block(err_deser);
                    } else {
                        gen_scope.generate_deserialize(types, &field.rust_type, &field.name, &format!("{} = Some(", field.name), ");", in_embedded, field.optional, &mut deser_block);
                    }
                }
                deser_block.line(format!("orig_deser_order.push({})", field_index));

                // serialize key
                let mut map_ser_content = BlocksOrLines::default();
                match &key {
                    FixedValue::Uint(x) => {
                        map_ser_content.line(&format!("serializer.write_unsigned_integer({})?;", x));
                        uint_field_deserializers.push(deser_block);
                    },
                    FixedValue::Text(x) => {
                        map_ser_content.line(&format!("serializer.write_text(\"{}\")?;", x));
                        text_field_deserializers.push(deser_block);
                    },
                    _ => panic!("unsupported map key type for {}.{}: {:?}", name, field.name, key),
                };

                // serialize value
                gen_scope.generate_serialize(types, &field.rust_type, &data_name, &field.name, &mut map_ser_content, false);
                ser_content.push(map_ser_content);
            }
            if PRESERVE_ENCODINGS {
                let mut deser_order_decl = codegen::Block::new(&format!("let deser_order = if self.orig_deser_order.as_ref().map(|v| v.len() == {}).unwrap_or(false)", rust_struct.definite_info(types).expect("cannot fail for maps")));
                deser_order_decl
                    .line("self.orig_deser_order.clone().unwrap()");
                ser_func.push_block(deser_order_decl);
                let mut deser_order_decl_else = codegen::Block::new("else");
                deser_order_decl_else
                    .line(format!("(0..{}).collect()", ser_content.len()))
                    .after(";");
                ser_func.push_block(deser_order_decl_else);
                let mut ser_loop = codegen::Block::new("for field_index in deser_order");
                let mut ser_loop_match = codegen::Block::new("match field_index");
                for (field_index, (content, field)) in ser_content.into_iter().zip(rust_struct.fields.iter()).enumerate() {
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
                for (content, field) in ser_content.into_iter().zip(rust_struct.fields.iter()) {
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
            let mut uint_match = Block::new("CBORType::UnsignedInteger => match raw.unsigned_integer()?");
            for case in uint_field_deserializers {
                uint_match.push_block(case);
            }
            uint_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Uint(unknown_key)).into()),");
            uint_match.after(",");
            type_match.push_block(uint_match);
            let mut text_match = Block::new("CBORType::Text => match raw.text()?.as_str()");
            for case in text_field_deserializers {
                text_match.push_block(case);
            }
            text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
            text_match.after(",");
            type_match.push_block(text_match);
            let mut special_match = Block::new("CBORType::Special => match len");
            special_match.line("cbor_event::Len::Len(_) => return Err(DeserializeFailure::BreakInDefiniteLen.into()),");
            // TODO: this will need to change if we support Special values as keys (e.g. true / false)
            let mut break_check = Block::new("cbor_event::Len::Indefinite => match raw.special()?");
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
            for field in &rust_struct.fields {
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
                if let RustType::Array(_) = &field.rust_type {
                    if PRESERVE_ENCODINGS && field.rust_type.directly_wasm_exposable() {
                        ctor_block.line(format!("{}_definite_encoding,", field.name));
                    }
                }
            }
            ctor_block
                .line("definite_encoding,")
                .line("orig_deser_order: Some(orig_deser_order),")
                .after(")");
            ctor_block
        },
    };
    ser_func.line("Ok(serializer)");
    match &mut ser_embedded_impl {
        Some(ser_embedded_impl) => ser_embedded_impl.push_fn(ser_func),
        None => ser_impl.push_fn(ser_func),
    };
    if deser_embedded_impl.is_none() {
        // ending checks are included with embedded serialization setup
        // since we are populating deserialize_as_embedded_group() and deserialize()
        // is already complete
        // but these checks must be done manually here *after* we populate deserialize()
        add_deserialize_final_len_check(deser_body, Some(rust_struct.rep), rust_struct.cbor_len_info(types));
    }
    deser_body.push_block(ctor_block);

    match &mut deser_embedded_impl {
        Some(deser_embedded_impl) => {
            deser_embedded_impl.push_fn(deser_f);
        },
        None => {
            if ANNOTATE_FIELDS {
                deser_f.push_block(error_annotator);
            }
            deser_impl.push_fn(deser_f);
        },
    };
    push_exposed_struct(gen_scope, name, s, s_impl, ser_impl, ser_embedded_impl);
    // TODO: generic deserialize (might need backtracking)
    if gen_scope.deserialize_generated(name) {
        gen_scope.serialize_scope().push_impl(deser_impl);
        if let Some(deser_embedded_impl) = deser_embedded_impl {
            gen_scope.serialize_scope().push_impl(deser_embedded_impl);
        }
    }
}

fn codegen_group_choices(gen_scope: &mut GenerationScope, types: &IntermediateTypes, name: &RustIdent, variants: &Vec<EnumVariant>, rep: Representation, tag: Option<usize>) {
    let enum_name = RustIdent::new(CDDLIdent::new(format!("{}Enum", name)));
    let kind_name = RustIdent::new(CDDLIdent::new(format!("{}Kind", name)));
    generate_enum(gen_scope, types, &enum_name, &kind_name, &variants, Some(rep), false);

    // Now generate a wrapper object that we will expose to wasm around this
    let (mut s, mut s_impl) = create_empty_exposed_struct(gen_scope, name);
    s
        .vis("pub")
        .tuple_field(&enum_name.to_string());
    // new (1 per variant)
    for variant in variants.iter() {
        // TODO: verify if variant.serialize_as_embedded_group impacts ctor generation
        let mut new_func = codegen::Function::new(&format!("new_{}", convert_to_snake_case(&variant.name.to_string())));
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
                        new_func.line(format!("Self({}::{})", enum_name, variant.name));
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
                        let mut ctor = format!("Self({}::{}({}::new(", enum_name, variant.name, variant.name);
                        for field in ctor_fields {
                            if output_comma {
                                ctor.push_str(", ");
                            } else {
                                output_comma = true;
                            }
                            new_func.arg(&field.name, field.rust_type.for_wasm_param());
                            // We don't use rust_type.from_wasm_boundary_*() here as we're delegating to the
                            // wasm-exposed variant.name::new() function
                            ctor.push_str(&field.name);
                        }
                        ctor.push_str(")))");
                        new_func.line(ctor);
                    },
                }
            },
            None => {
                // just directly pass in the variant's type
                let field_name = variant.name.to_string();
                new_func
                    .arg(&field_name, variant.rust_type.for_wasm_param())
                    .line(format!("Self({}::{}({}))", enum_name, variant.name, variant.rust_type.from_wasm_boundary_clone(&field_name)));
            },
        }
        s_impl.push_fn(new_func);
    }
    // serialize
    let mut ser_impl = codegen::Impl::new(&name.to_string());
    ser_impl.impl_trait("cbor_event::se::Serialize");
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    ser_func.line("self.0.serialize(serializer)");
    ser_impl.push_fn(ser_func);
    // enum-getters
    add_enum_getters(&mut s_impl, &enum_name, &kind_name, &variants);
    push_exposed_struct(gen_scope, name, s, s_impl, ser_impl, None);
    // deserialize
    let mut deser_impl = codegen::Impl::new(&name.to_string());
    deser_impl.impl_trait("Deserialize");
    let mut deser_func = make_deserialization_function("deserialize");
    deser_func.line(format!("Ok(Self({}::deserialize(raw)?))", enum_name));
    deser_impl.push_fn(deser_func);
    gen_scope.serialize_scope().push_impl(deser_impl);
}

fn add_enum_getters(s_impl: &mut codegen::Impl, enum_name: &RustIdent, kind_name: &RustIdent, variants: &Vec<EnumVariant>) {
    // kind() getter
    let mut get_kind = codegen::Function::new("kind");
    get_kind
        .arg_ref_self()
        .vis("pub")
        .ret(&kind_name.to_string());
    let mut get_kind_match = codegen::Block::new("match &self.0");
    for variant in variants.iter() {
        if variant.rust_type.is_fixed_value() {
            get_kind_match.line(format!("{}::{} => {}::{},", enum_name, variant.name, kind_name, variant.name));
        } else {
            get_kind_match.line(format!("{}::{}(_) => {}::{},", enum_name, variant.name, kind_name, variant.name));
        }
    }
    get_kind.push_block(get_kind_match);
    s_impl.push_fn(get_kind);
    // as_{variant} conversions (returns None -> undefined when not the type)
    for variant in variants.iter() {
        if !variant.rust_type.is_fixed_value() {
            let mut as_variant = codegen::Function::new(&format!("as_{}", &convert_to_snake_case(&variant.name.to_string())));
            as_variant
                .arg_ref_self()
                .vis("pub")
                .ret(&format!("Option<{}>", variant.rust_type.for_wasm_return()));
            let mut variant_match = codegen::Block::new("match &self.0");
            variant_match.line(format!("{}::{}(x) => Some(x.clone()),", enum_name, variant.name));
            variant_match.line("_ => None,");
            as_variant.push_block(variant_match);
            s_impl.push_fn(as_variant);
        }
    }
}

// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choicecs which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
// enum_name is for the rust enum containing all the data that is not exposed to wasm
// kind_name is for a int-only enum that just represents which type enum_name is
fn generate_enum(gen_scope: &mut GenerationScope, types: &IntermediateTypes, enum_name: &RustIdent, kind_name: &RustIdent, variants: &Vec<EnumVariant>, rep: Option<Representation>, generate_deserialize_directly: bool) {
    // also create a wasm-exposed enum just to distinguish the type
    let mut kind = codegen::Enum::new(&kind_name.to_string());
    kind.vis("pub");
    add_struct_derives(&mut kind);
    for variant in variants.iter() {
        kind.new_variant(&variant.name.to_string());
    }
    gen_scope
        .scope()
        .raw("#[wasm_bindgen]")
        .push_enum(kind);

    // rust enum containing the data
    let mut e = codegen::Enum::new(&enum_name.to_string());
    // instead of using create_serialize_impl() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(&mut e);
    let mut ser_impl = codegen::Impl::new(&enum_name.to_string());
    ser_impl.impl_trait("cbor_event::se::Serialize");
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_array_match_block = Block::new("match self");
    // we use Dynamic to avoid having any length checks here since we don't know what they are yet without testing the variants
    // and it's not worth looking into and complicating things on the off chance that all variants are the same length.
    let len_info = RustStructCBORLen::Dynamic;
    let mut deser_func = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(&enum_name.to_string(), "", "");
    let deser_body: &mut dyn CodeBlock = if ANNOTATE_FIELDS {
        &mut error_annotator
    } else {
        &mut deser_func
    };
    let mut deser_impl = if generate_deserialize_directly {
        let mut deser_impl = codegen::Impl::new(&enum_name.to_string());
        deser_impl.impl_trait("Deserialize");
        deser_impl
    } else {
        let (deser_impl, _deser_embedded_impl) = create_deserialize_impls(enum_name, rep, None, len_info.clone(), false, None, deser_body);
        deser_impl
    };
    deser_body.line("let initial_position = raw.as_mut_ref().seek(SeekFrom::Current(0)).unwrap();");
    for variant in variants.iter() {
        if variant.rust_type.is_fixed_value() {
            e.push_variant(codegen::Variant::new(&format!("{}", variant.name)));
        } else {
            e.push_variant(codegen::Variant::new(&format!("{}({})", variant.name, variant.rust_type.for_member())));
        }
        // serialize
        if variant.serialize_as_embedded_group {
            ser_array_match_block.line(&format!("{}::{}(x) => x.serialize(serializer),", enum_name, variant.name));
        } else {
            let capture = if variant.rust_type.is_fixed_value() {
                ""
            } else {
                "(x)"
            };
            //if variant.rust_type.is_serialize_multiline() {
                let mut case_block = Block::new(&format!("{}::{}{} =>", enum_name, variant.name, capture));
                let write_break = match rep {
                    Some(r) => {
                        let (len_str, indefinite) = match variant.rust_type.expanded_field_count(types) {
                            Some(n) => (format!("cbor_event::Len::Len({})", n), false),
                            None => (String::from("cbor_event::Len::Indefinite"), true),
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
                gen_scope.generate_serialize(types, &variant.rust_type, "x", &variant.name.to_string(), &mut case_block, !write_break);
                if write_break {
                    case_block.line("serializer.write_special(CBORSpecial::Break)");
                }
                case_block.after(",");
                ser_array_match_block.push_block(case_block);
            //}
        }
        // deserialize
        // TODO: don't backtrack if variants begin with non-overlapping cbor types
        // TODO: how to detect when a greedy match won't work? (ie choice with choices in a choice possibly)
        let mut variant_deser = Block::new("match (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>");
        if variant.rust_type.is_fixed_value() {
            gen_scope.generate_deserialize(types, &variant.rust_type, &convert_to_snake_case(&variant.name.to_string()), "", "", false, false, &mut variant_deser);
            variant_deser.line("Ok(())");
        } else {
            gen_scope.generate_deserialize(types, &variant.rust_type, &convert_to_snake_case(&variant.name.to_string()), "Ok(", ")", false, false, &mut variant_deser);
        }
        variant_deser.after(")(raw)");
        deser_body.push_block(variant_deser);
        // can't chain blocks so we just put them one after the other
        let mut return_if_deserialized = Block::new("");
        if variant.rust_type.is_fixed_value() {
            return_if_deserialized.line(format!("Ok(()) => return Ok({}::{}),", enum_name, variant.name));
        } else {
            return_if_deserialized.line(format!("Ok(variant) => return Ok({}::{}(variant)),", enum_name, variant.name));
        }
        return_if_deserialized.line("Err(_) => raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap(),");
        return_if_deserialized.after(";");
        deser_body.push_block(return_if_deserialized);
    }
    ser_func.push_block(ser_array_match_block);
    ser_impl.push_fn(ser_func);
    add_deserialize_final_len_check(deser_body, rep, len_info);
    deser_body.line(&format!("Err(DeserializeError::new(\"{}\", DeserializeFailure::NoVariantMatched.into()))", enum_name));
    if ANNOTATE_FIELDS {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    gen_scope
        .scope()
        .push_enum(e);
    gen_scope
        .serialize_scope()
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
    f
}

fn make_deserialization_function(name: &str) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f
        .generic("R: BufRead + Seek")
        .ret("Result<Self, DeserializeError>")
        .arg("raw", "&mut Deserializer<R>");
    f
}

fn generate_wrapper_struct(gen_scope: &mut GenerationScope, types: &IntermediateTypes, type_name: &RustIdent, field_type: &RustType, tag: Option<usize>, min_max: Option<(Option<isize>, Option<isize>)>) {
    let (mut s, mut s_impl) = create_empty_exposed_struct(gen_scope, type_name);
    s
        .vis("pub")
        .tuple_field(field_type.for_member());
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_impl = codegen::Impl::new(&type_name.to_string());
    ser_impl.impl_trait("cbor_event::se::Serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    gen_scope.generate_serialize(types, &field_type, "self.0", &type_name.to_string(), &mut ser_func, true);
    ser_impl.push_fn(ser_func);
    let mut deser_func = make_deserialization_function("deserialize");
    let mut deser_impl = codegen::Impl::new(&type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let Some(tag) = tag {
        deser_func.line(format!("let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{}\"))?;", type_name));
        let mut tag_check = Block::new(&format!("if tag != {}", tag));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}));", type_name, tag));
        deser_func.push_block(tag_check);
    }
    if let RustType::Rust(id) = field_type {
        if types.is_plain_group(id) {
            unimplemented!("TODO: make len/read_len variables of appropriate sizes so the generated code compiles");
        }
    }
    if let Some((min, max)) = min_max {
        gen_scope.generate_deserialize(types, field_type, "", "let wrapped = ", ";", false, false, &mut deser_func);
        let against = match field_type {
            RustType::Primitive(p) => match p {
                Primitive::Bytes |
                Primitive::Str => "wrapped.len()",
                Primitive::Bool |
                Primitive::U32 |
                Primitive::U64 |
                Primitive::I32 |
                Primitive::I64 |
                Primitive::N64 => "wrapped",
            },
            _ => unimplemented!(),
        };
        let mut check = match (min, max) {
            (Some(min), Some(max)) => if min == max {
                Block::new(&format!("if {} != {}", against, min))
            } else {
                Block::new(&format!("if {} < {} || {} > {}", against, min, against, max))
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
        deser_func.push_block(check);
        deser_func.line("Ok(Self(wrapped))");
    } else {
        gen_scope.generate_deserialize(types, field_type, "", "Ok(Self(", "))", false, false, &mut deser_func);
    }
    deser_impl.push_fn(deser_func);
    let mut new_func = codegen::Function::new("new");
    new_func
        .ret("Self")
        .arg("data", field_type.for_wasm_param())
        .vis("pub");
    new_func.line(format!("Self({})", field_type.from_wasm_boundary_clone("data")));
    s_impl.push_fn(new_func);
    gen_scope
        .scope()
        .raw("#[wasm_bindgen]")
        .push_struct(s)
        .raw("#[wasm_bindgen]")
        .push_impl(s_impl);
    gen_scope
        .serialize_scope()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

fn add_struct_derives<T: DataType>(data_type: &mut T) {
    data_type
        .derive("Clone")
        .derive("Debug")
        .derive("Eq")
        .derive("Ord")
        .derive("PartialEq")
        .derive("PartialOrd");
}