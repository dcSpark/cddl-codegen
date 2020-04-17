
// We need to figure how out we handle integers as they can only be serialized as
// Unsigned or Negative. Do we do an enum for the int type?
// It's only used in transaction_metadata as one choice.
// How it has to be serialized (assuming number: i32):
// if number >= 0 {
//     serializer.write_unsigned_integer(number as u64)
// } else {
//     serializer.write_negative_integer(number as i64)
// }

mod codegen_helpers;

use cddl::ast::*;
use codegen_helpers::{CodeBlock, DataType};
use std::collections::{BTreeMap, BTreeSet};
use either::{Either};

#[derive(Copy, Clone)]
enum Representation {
    Array,
    Map,
}

#[derive(Clone, Debug)]
enum RustType {
    // Primitive type that can be passed to/from wasm
    Primitive(String),
    // Rust-defined type that cannot be put in arrays/etc
    Rust(String),
    // Array-wrapped type. Passed as Vec<T> if T is Primitive
    Array(Box<RustType>),
    // Tagged type. Behavior depends entirely on wrapped type.
    Tagged(usize, Box<RustType>),

    // TODO: table type to support inlined defined table-type groups as fields

    // TODO: for non-table-type ones we could define a RustField(Ident, RustType) and then
    // a variant here Struct(Vec<RustField>) and delegate field/argument generation to
    // RustField so that we could basically expand them and not care about having to generate
    // and intermediate fields - although this could pose an issue for optional types... so maybe
    // another approach would be necessary.
}

impl RustType {
    fn directly_wasm_exposable(&self) -> bool {
        match self {
            RustType::Primitive(_) => true,
            RustType::Rust(_) => false,
            RustType::Array(ty) => ty.directly_wasm_exposable(),
            RustType::Tagged(_tag, ty) => ty.directly_wasm_exposable(),
        }
    }

    // TODO: should we unify this and for_member() in to_string()? (they were different previously) will we need the distinction for other types?
    fn for_wasm(&self) -> String {
        match self {
            RustType::Primitive(s) => s.clone(),
            RustType::Rust(s) => s.clone(),
            RustType::Array(ty) => if ty.directly_wasm_exposable() {
                format!("Vec<{}>", ty.for_wasm())
            } else {
                format!("{}s", ty.for_wasm())
            },
            RustType::Tagged(_tag, ty) => ty.for_wasm(),
        }
    }

    fn for_member(&self) -> String {
        match self {
            RustType::Primitive(s) => s.clone(),
            RustType::Rust(s) => s.clone(),
            RustType::Array(ty) => if ty.directly_wasm_exposable() {
                format!("Vec<{}>", ty.for_wasm())
            } else {
                format!("{}s", ty.for_wasm())
            },
            RustType::Tagged(_tag, ty) => ty.for_member(),
        }
    }

    fn for_variant(&self) -> String {
        match self {
            RustType::Primitive(p) => match p.as_str() {
                // to get around issues with String (CamelCase) or str (snake_case)
                "String" => String::from("Text"),
                p => convert_to_camel_case(p),
            },
            RustType::Array(inner) => {
                if let RustType::Primitive(p) = &**inner {
                    if p == "u8" {
                        return String::from("Bytes");
                    }
                }
                format!("Arr{}", inner.for_variant())
            },
            _ => self.for_member(),
        }
    }

    // TODO: should we get rid of from_wasm_boundary() entirely, or will it be useful for things that aren't tagged types?        
    fn from_wasm_boundary(&self, expr: &str) -> String {
        match self {
            //RustType::Tagged(tag, ty) => format!("TaggedData::<{}>::new({}, {})", ty.for_member(), expr, tag),
            _ => expr.to_owned(),
        }
    }
}

struct GlobalScope {
    global_scope: codegen::Scope,
    serialize_scope: codegen::Scope,
    already_generated: BTreeSet<String>,
    plain_groups: BTreeMap<String, Group>,
    type_aliases: BTreeMap::<String, RustType>,
}

impl GlobalScope {
    fn new() -> Self {
        let mut aliases = BTreeMap::<String, RustType>::new();
        // TODO: use u64/i64 later when you figure out the BigInt issues from wasm
        aliases.insert(String::from("uint"), RustType::Primitive(String::from("u32")));
        // Not sure on this one, I think they can be bigger than i64 can fit
        // but the cbor_event serialization takes the argument as an i64
        aliases.insert(String::from("nint"), RustType::Primitive(String::from("i32")));
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type = RustType::Primitive(String::from("String"));
        aliases.insert(String::from("tstr"), string_type.clone());
        aliases.insert(String::from("text"), string_type);
        // TODO: Is this right to have it be Vec<u8>?
        // the serialization library for bytes takes type [u8]
        // so we'll have to put some logic in there I guess?
        // it might be necessary to put a wrapper type..
        let byte_type = RustType::Array(Box::new(RustType::Primitive(String::from("u8"))));
        aliases.insert(String::from("bstr"), byte_type.clone());
        aliases.insert(String::from("bytes"), byte_type);
        // What about bingint/other stuff in the standard prelude?
        Self {
            global_scope: codegen::Scope::new(),
            serialize_scope: codegen::Scope::new(),
            already_generated: BTreeSet::new(),
            plain_groups: BTreeMap::new(),
            type_aliases: aliases,
        }
    }

    fn new_raw_type(&self, raw: &str) -> RustType {
        let resolved = self.apply_type_aliases(raw);
        if !is_identifier_reserved(raw) {
            if let RustType::Array(inner) = &resolved {
                if let RustType::Primitive(inner) = &**inner {
                    if inner == "u8" {
                        return RustType::Rust(raw.to_owned());
                    }
                }
            }
        }
        resolved
    }

    fn apply_type_aliases(&self, raw: &str) -> RustType {
        // Assumes we are not trying to pass in any kind of compound type (arrays, etc)
        match self.type_aliases.get(raw) {
            Some(alias) => match alias {
                RustType::Rust(id) => self.apply_type_aliases(id),
                x => x.clone(),
            },
            None => match raw {
                x => RustType::Rust(convert_to_camel_case(x)),
            },
        }
    }

    fn generate_type_alias(&mut self, alias: String, value: &str) {
        let base_type = self.new_raw_type(value);
        self.global_scope.raw(format!("type {} = {};", alias, base_type.for_member()).as_ref());
        self.apply_type_alias_without_codegen(alias, base_type);
    }

    fn apply_type_alias_without_codegen(&mut self, alias: String, base_type: RustType) {
        self.type_aliases.insert(alias.to_string(), base_type);
    }

    fn scope(&mut self) -> &mut codegen::Scope {
        &mut self.global_scope
    }

    fn serialize_scope(&mut self) -> &mut codegen::Scope {
        &mut self.serialize_scope
    }

    fn mark_plain_group(&mut self, name: String, group: Group) {
        self.plain_groups.insert(name, group);
    }

    // If it is a plain group, enerates a wrapper group if one wasn't before
    fn generate_if_plain_group(&mut self, name: String, rep: Representation) {
        // to get around borrow checker borrowing self mutably + immutably
        if let Some(group) = self.plain_groups.get(&name).map(|g| (*g).clone()) {
            if self.already_generated.insert(name.clone()) {
                // TODO: implement ability to have both an array and a map representation
                //       if someone ever needs that
                codegen_group(self, &group, &name, rep, None);
            }
        }
    }

    // TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos
    fn create_variants_from_type_choices(&mut self, type_choices: &Vec<Type1>) -> Vec<EnumVariant> {
        let mut variant_names_used = BTreeMap::<String, u32>::new();
        type_choices.iter().map(|type1| {
            let rust_type = match rust_type_from_type2(self, &type1.type2) {
                Some(t) => t,
                None => panic!("Could not generate type from {:?} in {:?}", type1.type2, type_choices),
            };
            let variant_name = rust_type.for_variant();
            let entry = variant_names_used.entry(rust_type.for_member()).or_default();
            *entry += 1;
            let variant_name = if *entry > 1 {
                format!("{}{}", variant_name, *entry)
            } else {
                variant_name
            };
            EnumVariant {
                name: variant_name,
                rust_type: rust_type,
            }
        }).collect()
    }

    fn generate_type_choices(&mut self, name: &str, variants: &Vec<EnumVariant>, tag: Option<usize>) {
        // Handle group with choices by generating an enum then generating a group for every choice
        let enum_name = format!("{}Enum", name);
        generate_enum(self, &enum_name, &variants, None, false);

        // Now generate a wrapper object that we will expose to wasm around this
        let (mut s, mut s_impl) = create_exposed_group(name);
        let (ser_impl, mut ser_embedded_impl) = create_serialize_impls(name, None, tag);
        s
            .vis("pub")
            .tuple_field(&enum_name);
        // new
        for variant in variants.iter() {
            let variant_arg = convert_to_snake_case(&variant.name);
            let mut new_func = codegen::Function::new(&format!("new_{}", variant_arg));
            new_func
                .ret("Self")
                .vis("pub")
                .arg(&variant_arg, &variant.rust_type.for_wasm())
                .line(format!("Self({}::{}({}))", enum_name, variant.name, variant_arg));
            s_impl.push_fn(new_func);
        }
        // serialize
        let mut ser_embedded_func = make_serialization_function("serialize_as_embedded_group");
        ser_embedded_func.line("self.0.serialize_as_embedded_group(serializer)");
        ser_embedded_impl.push_fn(ser_embedded_func);
        push_exposed_struct(self, s, s_impl, ser_impl, ser_embedded_impl);
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(&mut self, element_type: RustType) -> RustType {
        if element_type.directly_wasm_exposable() {
            return RustType::Array(Box::new(element_type));
        }
        if let RustType::Rust(name) = &element_type {
            self.generate_if_plain_group(name.clone(), Representation::Array);
        }
        let element_type_wasm = element_type.for_wasm();
        let element_type_rust = element_type.for_member();
        let array_type = format!("{}s", element_type_rust);
        if self.already_generated.insert(array_type.clone()) {
            let mut s = codegen::Struct::new(&array_type);
            s
                .tuple_field(format!("Vec<{}>", element_type_rust))
                .vis("pub");
            add_struct_derives(&mut s);
            // TODO: accessors (mostly only necessary if we support deserialization)
            self.global_scope.raw("#[wasm_bindgen]");
            self.global_scope.push_struct(s);
            let mut ser_impl = codegen::Impl::new(&array_type);
            ser_impl.impl_trait("cbor_event::se::Serialize");
            let mut ser_func = make_serialization_function("serialize");
            ser_func.line("serializer.write_array(cbor_event::Len::Len(self.0.len() as u64))?;");
            let mut loop_block = codegen::Block::new("for element in &self.0");
            loop_block.line("element.serialize(serializer)?;");
            ser_func.push_block(loop_block);
            ser_func.line("Ok(serializer)");
            ser_impl.push_fn(ser_func);
            self.serialize_scope.push_impl(ser_impl);
            let mut array_impl = codegen::Impl::new(&array_type);
            array_impl
                .new_fn("new")
                .vis("pub")
                .ret("Self")
                .line("Self(Vec::new())");
            array_impl
                .new_fn("size")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .line("self.0.len()");
            array_impl
                .new_fn("get")
                .vis("pub")
                .ret(&element_type_wasm)
                .arg_ref_self()
                .arg("index", "usize")
                .line("self.0[index].clone()");
            array_impl
                .new_fn("add")
                .vis("pub")
                .arg_mut_self()
                .arg("elem", element_type_wasm)
                .line("self.0.push(elem);");
            self.global_scope.raw("#[wasm_bindgen]");
            self.global_scope.push_impl(array_impl);
        }
        RustType::Array(Box::new(element_type))
    }

    fn generate_serialize(&mut self, rust_type: &RustType, mut expr: String, body: &mut dyn CodeBlock) {
        //body.line(&format!("// DEBUG - generated from: {:?}", rust_type));
        match rust_type {
            RustType::Primitive(_) => {
                // clone() is to handle String, might not be necessary
                body.line(&format!("{}.clone().serialize(serializer)?;", expr));
            },
            RustType::Rust(t) => {
                if self.plain_groups.contains_key(t) {
                    body.line(&format!("{}.serialize_as_embedded_group(serializer)?;", expr));
                } else {
                    body.line(&format!("{}.serialize(serializer)?;", expr));
                }
            },
            RustType::Array(ty) => {
                // TODO: I think this should be removed due to the TaggedData<T> removal - please test
                //       to see if we need to replace this with something else, or if arrays of tagged things
                //       work fine with the refactor.
                // not iterating tagged data but instead its contents
                if let RustType::Tagged(_, _) = &**ty {
                    expr.push_str(".data");
                };
                // non-literal types are contained in vec wrapper types
                if !ty.directly_wasm_exposable() {
                    expr.push_str(".0");
                }
                body.line(&format!("serializer.write_array(cbor_event::Len::Len({}.len() as u64))?;", expr));
                let mut loop_block = codegen::Block::new(&format!("for element in {}.iter()", expr));
                loop_block.line("element.serialize(serializer)?;");
                body.push_block(loop_block);
            },
            RustType::Tagged(tag, ty) => {
                body.line(&format!("serializer.write_tag({}u64)?;", tag));
                self.generate_serialize(ty, expr, body);
            },
        }
    }
}

fn convert_to_snake_case(ident: &str) -> String {
    let mut snake_case = String::new();
    for c in ident.chars() {
        match c {
            '-' => {
                snake_case.push('_');
            },
            '$' | '@' => {
                // ignored
            },
            c => {
                if c.is_ascii_uppercase() && !snake_case.is_empty() {
                    snake_case.push('_')
                }
                snake_case.push(c.to_ascii_lowercase());
            }
        }
    }
    snake_case
}

fn convert_to_camel_case(ident: &str) -> String {
    let mut camel_case = String::new();
    let mut uppercase = true;
    for c in ident.chars() {
        match c {
            '_' | '-' => {
                uppercase = true;
            },
            '$' | '@' => {
                // ignored
            },
            c => {
                if uppercase {
                    camel_case.push(c.to_ascii_uppercase());
                    uppercase = false;
                } else {
                    camel_case.push(c);
                }
            },
        }
    }
    camel_case
}

fn add_struct_derives<T: DataType>(data_type: &mut T) {
    data_type
        .derive("Clone")
        .derive("Eq")
        .derive("Ord")
        .derive("PartialEq")
        .derive("PartialOrd");
}

fn is_identifier_reserved(name: &str) -> bool {
    match name {
        // Do we need more? We don't support any other ones.
        "uint"  |
        "int"   |
        "nint"  |
        "text"  |
        "tstr"  |
        "bytes" |
        "bstr"  => true,
        _ => false,
    }
}

fn type_to_field_name(t: &Type) -> Option<String> {
    match t.type_choices.len() {
        1 => match &t.type_choices.first().unwrap().type2 {
            Type2::Typename{ ident, .. } => Some(ident.to_string()),
            Type2::TextValue { value, .. } => Some(value.clone()),
            Type2::Array { group, .. } => match group.group_choices.len() {
                1 => {
                    let entries = &group.group_choices.first().unwrap().group_entries;
                    match entries.len() {
                        1 => {
                            match &entries.first().unwrap().0 {
                                // should we do this? here it possibly allows [[foo]] -> fooss
                                GroupEntry::ValueMemberKey{ ge, .. } => Some(format!("{}s", type_to_field_name(&ge.entry_type)?)),
                                GroupEntry::TypeGroupname{ ge, .. } => Some(format!("{}s", ge.name.to_string())),
                                GroupEntry::InlineGroup { .. } => None,
                            }
                        },
                        // only supports homogenous arrays for now
                        _ => None,
                    }
                },
                // no group choice support here
                _ => None
            }
            // non array/text/identifier types not supported here - value keys are caught earlier anyway
            _ => None
        },
        // no type choice support here
        _ => None,
    }
}

// Attempts to use the style-converted type name as a field name, and if we have already
// generated one, then we simply add numerals starting at 2, 3, 4...
fn group_entry_to_field_name(entry: &GroupEntry, index: usize, already_generated: &mut BTreeMap<String, u32>) -> String {
    let field_name = convert_to_snake_case(&match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => match ge.member_key.as_ref() {
            Some(member_key) => match member_key {
                MemberKey::Value{ value, .. } => format!("key_{}", value),
                MemberKey::Bareword{ ident, .. } => ident.to_string(),
                MemberKey::Type1{ t1, .. } => match t1.type2 {
                    Type2::UintValue{ value, .. } => format!("key_{}", value),
                    _ => panic!("Encountered Type1 member key in multi-field map - not supported: {:?}", entry),
                },
            },
            None => {
                type_to_field_name(&ge.entry_type).unwrap_or_else(|| format!("index_{}", index))
            }
        },
        GroupEntry::TypeGroupname{ ge: TypeGroupnameEntry { name, .. }, .. } => match is_identifier_reserved(&name.to_string()) {
            true => format!("index_{}", index),
            false => name.to_string(),
        },
        GroupEntry::InlineGroup{ group, .. } => panic!("not implemented (define a new struct for this!) = {}\n\n {:?}", group, group),
    });
    let entry = already_generated.entry(field_name.clone()).or_default();
    *entry += 1;
    if *entry > 1 {
        format!("{}{}", field_name, *entry)
    } else {
        field_name
    }
}

// Returns None if this is a fixed value that we should not be storing
fn rust_type_from_type2(global: &mut GlobalScope, type2: &Type2) -> Option<RustType> {
    match type2 {
        // ignoring IntValue/FloatValue/other primitives since they're not in the shelley spec
        // ie Type2::UintValue(value) => format!("uint<{}>", value),
        // generic args not in shelley.cddl
        // TODO: socket plugs (used in hash type)
        Type2::Typename{ ident, .. } => Some(global.new_raw_type(&ident.ident)),
        // Map(group) not implemented as it's not in shelley.cddl
        Type2::Array{ group, .. } => {
            let mut arr_type = None;
            for choice in &group.group_choices {
                // special case for homogenous arrays
                if choice.group_entries.len() == 1{
                    let (entry, _has_comma) = choice.group_entries.first().unwrap();
                    let element_type = match entry {
                        GroupEntry::ValueMemberKey{ ge, .. } => rust_type(global, &ge.entry_type),
                        GroupEntry::TypeGroupname{ ge, .. } => Some(global.new_raw_type(&ge.name.to_string())),//Some(RustType::new_raw(&tgn.name.to_string())),
                        _ => panic!("UNSUPPORTED_ARRAY_ELEMENT<{:?}>", entry),
                    }.unwrap();
                    arr_type = Some(global.generate_array_type(element_type));
                } else {
                    panic!("TODO: how do we handle this? tuples? or creating a struct definition and referring to it by name?")
                }
                // TODO: handle group choices (enums?)
                break;
            }
            arr_type
        },
        // unsure if we need to handle the None case - when does this happen?
        Type2::TaggedData{ tag, t, .. } => {
            Some(RustType::Tagged(tag.expect("tagged data without tag not supported"), Box::new(rust_type(global, t).unwrap())))
        },
        _ => {
            println!("Ignoring Type2: {:?}", type2);
            None
        },
    }
}

fn rust_type(global: &mut GlobalScope, t: &Type) -> Option<RustType> {
    if t.type_choices.len() == 1 {
        rust_type_from_type2(global, &t.type_choices.first().unwrap().type2)
    } else {
        let variants = global.create_variants_from_type_choices(&t.type_choices);
        let mut combined_name = String::new();
        // one caveat: nested types can leave ambiguous names and cause problems like
        // (a / b) / c and a / (b / c) would both be AOrBOrC
        for variant in &variants {
        if !combined_name.is_empty() {
                combined_name.push_str("Or");
            }
            // due to undercase primitive names, we need to convert here
            combined_name.push_str(&variant.rust_type.for_variant());
        }
        global.generate_type_choices(&combined_name, &variants, None);
        Some(global.new_raw_type(&combined_name))
    }
}

fn group_entry_optional(entry: &GroupEntry) -> bool {
    match match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => &ge.occur,
        GroupEntry::TypeGroupname{ ge, .. } => &ge.occur,
        GroupEntry::InlineGroup{ .. } => panic!("inline group entries are not implemented"),
    } {
        Some(Occur::Optional(_)) => true,
        _ => false,
    }
}

fn group_entry_to_type(global: &mut GlobalScope, entry: &GroupEntry) -> Option<RustType> {
    let ret = match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => rust_type(global, &ge.entry_type),
        GroupEntry::TypeGroupname{ ge, .. } => Some(global.new_raw_type(&ge.name.to_string())),
        GroupEntry::InlineGroup{ .. } => panic!("inline group entries are not implemented"),
    };
    //println!("group_entry_to_typename({:?}) = {:?}\n", entry, ret);
    ret
}

fn create_exposed_group(name: &str) -> (codegen::Struct, codegen::Impl) {
    let mut s = codegen::Struct::new(name);
    add_struct_derives(&mut s);
    let mut group_impl = codegen::Impl::new(name);
    group_impl.new_fn("to_bytes")
        .ret("Vec<u8>")
        .arg_ref_self()
        .vis("pub")
        .line("let mut buf = Serializer::new_vec();")
        .line("self.serialize(&mut buf).unwrap();")
        .line("buf.finalize()");
    (s, group_impl)
}

// The serialize impls calls the embedded serialize impl, but the embedded one is
// empty and must be implemented yourself.
fn create_serialize_impls(name: &str, rep: Option<Representation>, tag: Option<usize>) -> (codegen::Impl, codegen::Impl) {
    let mut ser_impl = codegen::Impl::new(name);
    ser_impl.impl_trait("cbor_event::se::Serialize");
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    // TODO: indefinite or definite encoding?
    if let Some (rep) = rep {
        match rep {
            Representation::Array => ser_func.line(format!("serializer.write_array(cbor_event::Len::Indefinite)?;")),
            Representation::Map => ser_func.line(format!("serializer.write_map(cbor_event::Len::Indefinite)?;")),
        };
        ser_func.line("self.serialize_as_embedded_group(serializer)?;");
        ser_func.line("serializer.write_special(cbor_event::Special::Break)");
    } else {
        ser_func.line("self.serialize_as_embedded_group(serializer)");
    }
    ser_impl.push_fn(ser_func);
    let mut ser_embedded_impl = codegen::Impl::new(name);
    ser_embedded_impl.impl_trait("SerializeEmbeddedGroup");
    (ser_impl, ser_embedded_impl)
}

fn push_exposed_struct(
    global: &mut GlobalScope,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    ser_impl: codegen::Impl,
    ser_embedded_impl: codegen::Impl,
) {
    global.scope()
        .raw("#[wasm_bindgen]")
        .push_struct(s)
        .raw("#[wasm_bindgen]")
        .push_impl(s_impl);
    global.serialize_scope()
        .push_impl(ser_impl)
        .push_impl(ser_embedded_impl);
}

fn codegen_group_choice(global: &mut GlobalScope, group_choice: &GroupChoice, name: &str, rep: Representation, tag: Option<usize>) {
    let (mut s, mut s_impl) = create_exposed_group(name);
    let (ser_impl, mut ser_embedded_impl) = create_serialize_impls(name, Some(rep), tag);
    s.vis("pub");
    let table_types = table_domain_range(group_choice, rep);
    match table_types {
        // Table map - homogenous key/value types
        Some((domain, range)) => {
            let key_type = rust_type_from_type2(global, domain).unwrap();
            let value_type = rust_type(global, range).unwrap();
            s.field("table", format!("std::collections::BTreeMap<{}, {}>", key_type.for_member(), value_type.for_member()));
            // new
            let mut new_block = codegen::Block::new("Self");
            new_block.line("table: std::collections::BTreeMap::new(),");
            s_impl
                .new_fn("new")
                .vis("pub")
                .ret("Self")
                .push_block(new_block);
            // insert
            let mut insert_func = codegen::Function::new("insert");
            insert_func
                .vis("pub")
                .arg_mut_self()
                .arg("key", key_type.for_wasm())
                .arg("value", value_type.for_wasm())
                .line(
                    format!(
                        "self.table.insert({}, {});",
                        key_type.from_wasm_boundary("key"),
                        value_type.from_wasm_boundary("value")));
            s_impl.push_fn(insert_func);
            // serialize
            let mut ser_map = make_serialization_function("serialize_as_embedded_group");
            let mut table_loop = codegen::Block::new("for (key, value) in &self.table");
            global.generate_serialize(&key_type, String::from("key"), &mut table_loop);
            global.generate_serialize(&value_type, String::from("value"), &mut table_loop);
            ser_map.push_block(table_loop);
            ser_map.line("Ok(serializer)");
            ser_embedded_impl.push_fn(ser_map);
        },
        // Heterogenous map (or array!) with defined key/value pairs in the cddl like a struct
        None => {
            // Construct all field data here since we iterate over it multiple times and having
            // it in one huge loop was extremely unreadable
            let mut generated_fields = BTreeMap::<String, u32>::new();
            let fields: Vec<(String, Option<RustType>, bool, &GroupEntry)> = group_choice.group_entries.iter().enumerate().map(
                |(index, (group_entry, _has_comma))| {
                    let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields);
                    // does not exist for fixed values importantly
                    let field_type = group_entry_to_type(global, group_entry);
                    if let Some(RustType::Rust(ident)) = &field_type {
                        global.generate_if_plain_group(ident.clone(), rep);
                    }
                    let optional_field = group_entry_optional(group_entry);
                    (field_name, field_type, optional_field, group_entry)
                }
            ).collect();

            // Generate struct + fields + constructor
            let mut new_func = codegen::Function::new("new");
            new_func
                .ret("Self")
                .vis("pub");
            let mut new_func_block = codegen::Block::new("Self");
            for (field_name, field_type, optional_field, _group_entry) in &fields {
                // Unsupported types so far are fixed values, only have fields for these.
                if let Some(rust_type) = field_type {
                    if *optional_field {
                        // field
                        s.field(field_name, format!("Option<{}>", rust_type.for_member()));
                        // new
                        new_func_block.line(format!("{}: None,", field_name));
                        // setter
                        let mut setter = codegen::Function::new(&format!("set_{}", field_name));
                        setter
                            .arg_mut_self()
                            .arg(&field_name, &rust_type.for_wasm())
                            .vis("pub")
                            .line(format!("self.{} = Some({})", field_name, field_name));
                        s_impl.push_fn(setter);
                    } else {
                        // field
                        s.field(field_name, rust_type.for_member());
                        // new
                        new_func.arg(&field_name, rust_type.for_wasm());
                        new_func_block.line(format!("{}: {},", field_name, rust_type.from_wasm_boundary(field_name)));
                        // do we want setters here later for mandatory types covered by new?
                    }
                }
            }
            new_func.push_block(new_func_block);
            s_impl.push_fn(new_func);

            // Generate serialization
            let mut ser_func = make_serialization_function("serialize_as_embedded_group");
            match rep {
                Representation::Array => {
                    for (field_name, field_type, optional_field, group_entry) in &fields {
                        // Unsupported types so far are fixed values, only have fields for these.
                        if let Some(rust_type) = field_type {
                            if *optional_field {
                                let mut optional_array_ser_block = codegen::Block::new(&format!("if let Some(field) = &self.{}", field_name));
                                global.generate_serialize(&rust_type, String::from("field"), &mut optional_array_ser_block);
                                ser_func.push_block(optional_array_ser_block);
                            } else {
                                global.generate_serialize(&rust_type, format!("self.{}", field_name), &mut ser_func);
                            }
                        } else {
                            // But even without a field, we still need to serialize them.
                            // This is for when the entry was a literal value
                            match group_entry {
                                GroupEntry::ValueMemberKey{ ge, .. } => {
                                    assert_eq!(ge.entry_type.type_choices.len(), 1, "Type choices not supported as map keys");
                                    match ge.entry_type.type_choices.first() {
                                        Some(x) => match &x.type2 {
                                            Type2::UintValue{ value, .. } => {
                                                ser_func.line(format!("serializer.write_unsigned_integer({})?;", value));
                                            },
                                            x => panic!("unsupported fixed type: {}", x),
                                        },
                                        None => unreachable!(),
                                    }
                                },
                                _ => panic!("unsupported fixed type: {:?}", group_entry),
                            }
                        }
                    }
                },
                Representation::Map => {
                    // If we have a group with entries that have no names, that's fine for arrays
                    // but not for maps, so if we encounter one assume we should not generate
                    // map-related functions.
                    // In the future we could change this tool to only emit the array or map
                    // functions when they are strictly necessary (wrapped in array or map elsewhere)
                    // This would also reduce error checking here since we wouldn't hit certain cases
                    let mut contains_entries_without_names = false;
                    for (field_name, field_type, optional_field, group_entry) in &fields {
                        // Unsupported types so far are fixed values, only have fields for these.
                        if let Some(rust_type) = field_type {
                            let mut optional_map_ser_block = codegen::Block::new(&format!("if let Some(field) = &self.{}", field_name));
                            let (data_name, map_ser_block): (String, &mut dyn CodeBlock) = if *optional_field {
                                (String::from("field"), &mut optional_map_ser_block)
                            } else {
                                (format!("self.{}", field_name), &mut ser_func)
                            };
                            // This match is for serializing KEYS for maps
                            match group_entry {
                                GroupEntry::ValueMemberKey{ ge, .. } => {
                                    match ge.member_key.as_ref() {
                                        Some(member_key) => match member_key {
                                            MemberKey::Value{ value, .. } => match value {
                                                cddl::token::Value::UINT(x) => {
                                                    map_ser_block.line(&format!("serializer.write_unsigned_integer({})?;", x));
                                                },
                                                _ => panic!("unsupported map identifier(1): {:?}", value),
                                            },
                                            MemberKey::Bareword{ ident, .. } => {
                                                map_ser_block.line(&format!("serializer.write_text(\"{}\")?;", ident.to_string()));
                                            },
                                            MemberKey::Type1{ t1, .. } => match t1.type2 {
                                                Type2::UintValue{ value, .. } => {
                                                    map_ser_block.line(&format!("serializer.write_unsigned_integer({})?;", value));
                                                },
                                                _ => panic!("unsupported map identifier(2): {:?}", member_key),
                                            },
                                        },
                                        None => {
                                            contains_entries_without_names = true;
                                        },
                                    }
                                },
                                // TODO: why are we hitting this?
                                // GroupEntry::TypeGroupname(tgn) => match tgn.name.to_string().as_ref() {
                                //     "uint" => format!("serializer.write_unsigned_integer({})?;", x),
                                //     x => panic!("TODO: serialize '{}'", x),
                                // },
                                _ => {
                                    //panic!("unsupported map identifier(3): {:?}", x),
                                    // TODO: only generate map vs array stuff when needed to avoid this hack
                                    contains_entries_without_names = true;
                                },
                            };
                            // and serialize value
                            global.generate_serialize(&rust_type, data_name, map_ser_block);
                            if *optional_field {
                                ser_func.push_block(optional_map_ser_block);
                            }
                        } else {
                            panic!("could not generate map serialization for group with non-ValueMemberKey field: {:?}", group_choice);
                        }
                    }
                    assert!(!contains_entries_without_names, "could not generate as map without key names");
                },
            };
            ser_func.line("Ok(serializer)");
            ser_embedded_impl.push_fn(ser_func);
        }
    }
    push_exposed_struct(global, s, s_impl, ser_impl, ser_embedded_impl);
}

fn codegen_group(global: &mut GlobalScope, group: &Group, name: &str, rep: Representation, tag: Option<usize>) {
    if group.group_choices.len() == 1 {
        // Handle simple (no choices) group.
        codegen_group_choice(global, group.group_choices.first().unwrap(), name, rep, tag);
    } else {
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // Handle group with choices by generating an enum then generating a group for every choice
        let enum_name = format!("{}Enum", name);
        let variants: Vec<EnumVariant> = group.group_choices.iter().enumerate().map(|(i, group_choice)| {
            let variant_name = format!("{}{}", name, i);
            // TODO: Should we generate these within their own namespace?
            codegen_group_choice(global, group_choice, &variant_name, rep, None);
            EnumVariant {
                name: variant_name.clone(),
                rust_type: RustType::Rust(variant_name),
            }
        }).collect();
        generate_enum(global, &enum_name, &variants, Some(rep), true);

        // Now generate a wrapper object that we will expose to wasm around this
        let (mut s, mut s_impl) = create_exposed_group(name);
        let (ser_impl, mut ser_embedded_impl) = create_serialize_impls(name, Some(rep), tag);
        s
            .vis("pub")
            .tuple_field(&enum_name);
        // new
        for (variant, group_choice) in variants.iter().zip(group.group_choices.iter()) {
            let mut new_func = codegen::Function::new(&format!("new_{}", convert_to_snake_case(&variant.name)));
            new_func
                .ret("Self")
                .vis("pub");
            let mut output_comma = false;
            let mut ctor = format!("Self({}::{}({}::new(", enum_name, variant.name, variant.name);
            let mut generated_fields = BTreeMap::<String, u32>::new();
            for (index, (group_entry, _has_comma)) in group_choice.group_entries.iter().enumerate() {
                if !group_entry_optional(group_entry) {
                    let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields);
                    // Unsupported types so far are fixed values, only have fields for these.
                    if let Some(rust_type) = group_entry_to_type(global, group_entry) {
                        if output_comma {
                            ctor.push_str(", ");
                        } else {
                            output_comma = true;
                        }
                        new_func.arg(&field_name, rust_type.for_wasm());
                        ctor.push_str(&field_name);
                    }
                }
            }
            ctor.push_str(")))");
            new_func.line(ctor);
            s_impl.push_fn(new_func);
        }
        // serialize
        let mut ser_embedded_func = make_serialization_function("serialize_as_embedded_group");
        ser_embedded_func.line("self.0.serialize_as_embedded_group(serializer)");
        ser_embedded_impl.push_fn(ser_embedded_func);
        push_exposed_struct(global, s, s_impl, ser_impl, ser_embedded_impl);
    }
}

// rep is Optional - None means we just serialize raw, ie for tpye choices
struct EnumVariant {
    name: String,
    rust_type: RustType,
}

fn generate_enum(global: &mut GlobalScope, enum_name: &str, variants: &Vec<EnumVariant>, rep: Option<Representation>, serialize_as_embedded_group: bool) {
    let mut e = codegen::Enum::new(enum_name);
    add_struct_derives(&mut e);
    let (ser_impl, mut ser_embedded_impl) = create_serialize_impls(enum_name, rep, None);
    let mut ser_func_embedded = make_serialization_function("serialize_as_embedded_group");
    let mut ser_array_embedded_match_block = codegen::Block::new("match self");
    for variant in variants.iter() {
        e.push_variant(codegen::Variant::new(&format!("{}({})", variant.name, variant.rust_type.for_member())));
        if serialize_as_embedded_group {
            ser_array_embedded_match_block.line(&format!("{}::{}(x) => x.serialize_as_embedded_group(serializer),", enum_name, variant.name));
        } else {
            let mut case_block = codegen::Block::new(&format!("{}::{}(x) =>", enum_name, variant.name));
            global.generate_serialize(&variant.rust_type, String::from("x"), &mut case_block);
            case_block.after(",");
            ser_array_embedded_match_block.push_block(case_block);
        }
    }
    ser_func_embedded.push_block(ser_array_embedded_match_block);
    if !serialize_as_embedded_group {
        ser_func_embedded.line("Ok(serializer)");
    }
    ser_embedded_impl.push_fn(ser_func_embedded);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    global.scope()
        .push_enum(e);
    global.serialize_scope()
        .push_impl(ser_impl)
        .push_impl(ser_embedded_impl);
}

fn table_domain_range(group_choice: &GroupChoice, rep: Representation) -> Option<(&Type2, &Type)> {
    // Here we test if this is a struct vs a table.
    // struct: { x: int, y: int }, etc
    // table: { * int => tstr }, etc
    // this assumes that all maps representing tables are homogenous
    // and contain no other fields. I am not sure if this is a guarantee in
    // cbor but I would hope that the cddl specs we are using follow this.
    if let Representation::Map = rep {
        if group_choice.group_entries.len() == 1 {
            match group_choice.group_entries.first() {
                Some((GroupEntry::ValueMemberKey{ ge, .. }, _)) => {
                    match &ge.member_key {
                        Some(MemberKey::Type1{ t1, .. }) => {
                            // TODO: Do we need to handle cuts for what we're doing?
                            // Does the range control operator matter?
                            return Some((&t1.type2, &ge.entry_type));
                        },
                        _ => panic!("unsupported table map key (1): {:?}", ge),
                    }
                },
                _ => panic!("unsupported table map key (2): {:?}", group_choice.group_entries.first().unwrap()),
            }
        }
    }
    // Could not get a table-type domain/range - must be a heterogenous struct
    None
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

fn generate_wrapper_struct(global: &mut GlobalScope, type_name: &str, field_type: &RustType, tag: Option<usize>) {
    let (mut s, mut s_impl) = create_exposed_group(type_name);
    s
        .vis("pub")
        .tuple_field(field_type.for_member());
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_impl = codegen::Impl::new(type_name);
    ser_impl.impl_trait("cbor_event::se::Serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    global.generate_serialize(&field_type, String::from("self.0"), &mut ser_func);
    ser_func.line("Ok(serializer)");
    ser_impl.push_fn(ser_func);
    let mut new_func = codegen::Function::new("new");
    new_func
        .ret("Self")
        .arg("data", field_type.for_wasm())
        .vis("pub");
    new_func.line(format!("Self({})", field_type.from_wasm_boundary("data")));
    s_impl.push_fn(new_func);
    global.scope().raw("#[wasm_bindgen]");
    global.scope().push_struct(s);
    global.scope().raw("#[wasm_bindgen]");
    global.scope().push_impl(s_impl);
    global.serialize_scope().push_impl(ser_impl);
}

fn generate_type(global: &mut GlobalScope, type_name: &str, type2: &Type2, outer_tag: Option<usize>) {
    match type2 {
        Type2::Typename{ ident, .. } => {
            // This should be controlled in a better way - maybe we can annotate the cddl
            // to specify whether or not we want to simply to a typedef to Vec<u8> for bytes
            // or whether we want to do what we are doing here and creating a custom type.
            // This is more specific to our own use-case since the binary types via type alises
            // in the shelley.cddl spec are things that should have their own type and we would
            // want to expand upon the code later on.
            // Perhaps we could change the cddl and have some specific tag like "BINARY_FORMAT"
            // to generate this?
            let generate_binary_wrapper = match ident.to_string().as_ref() {
                "bytes" | "bstr" => true,
                ident => if let RustType::Array(inner) = global.apply_type_aliases(&convert_to_camel_case(ident)) {
                    if let RustType::Primitive(x) = *inner {
                        x == "u8"
                    } else {
                        false
                    }
                } else {
                    false
                },
            };
            if generate_binary_wrapper {
                let field_type = RustType::Array(Box::new(RustType::Primitive(String::from("u8"))));
                generate_wrapper_struct(global, type_name, &field_type, None);
                global.apply_type_alias_without_codegen(type_name.to_owned(), field_type);
            } else {
                // Using RustType here just to get a string out of it that applies
                // common conversions like uint -> u64. Since we're only using it
                // to get a String, we should be fine.
                global.generate_type_alias(type_name.to_owned(), &ident.to_string());
            }
        },
        Type2::Map{ group, .. } => {
            codegen_group(global, group, type_name, Representation::Map, outer_tag);
        },
        Type2::Array{ group, .. } => {
            codegen_group(global,group, type_name, Representation::Array, outer_tag);
        },
        Type2::TaggedData{ tag, t, .. } => {
            if let Some(_) = outer_tag {
                panic!("doubly nested tags are not supported");
            }
            tag.expect("not sure what empty tag here would mean - unsupported");
            assert_eq!(t.type_choices.len(), 1, "root level tagged type choices not supported");
            let inner_type = &t.type_choices.first().unwrap().type2;
            match match inner_type {
                Type2::Typename{ ident, .. } => Either::Right(ident),
                Type2::Map{ group, .. } => Either::Left(group),
                Type2::Array{ group, .. } => Either::Left(group),
                x => panic!("only supports tagged arrays/maps/typenames - found: {:?} in rule {}", x, type_name),
            } {
                Either::Left(_group) => generate_type(global, type_name, inner_type, *tag),
                Either::Right(ident) => generate_wrapper_struct(global, type_name, &global.new_raw_type(&ident.to_string()), *tag),
            };
        },
        x => {
            println!("\nignored typename {} -> {:?}\n", type_name, x);
        },
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cddl_in = std::fs::read_to_string("type_choice_test.cddl").unwrap();
    let cddl = cddl::parser::cddl_from_str(&cddl_in)?;
    //println!("CDDL file: {}", cddl);
    let mut global = GlobalScope::new();
    // Can't generate groups of imports with codegen::Import so we just output this as raw text
    // since we don't need it to be dynamic so it's fine. codegen::Impl::new("a", "{z::b, z::c}")
    // does not work.
    global.scope().raw("// This library was code-generated using an experimental CDDL to rust tool:\n// https://github.com/Emurgo/cddl-codegen");
    global.scope().raw("use cbor_event::{self, de::{Deserialize, Deserializer}, se::{Serialize, Serializer}};");
    global.scope().import("std::io", "Write");
    global.scope().import("wasm_bindgen::prelude", "*");
    global.scope().import("prelude", "*");
    global.scope().raw("mod prelude;");
    global.scope().raw("mod serialization;");
    global.serialize_scope().import("super", "*");
    // Need to know beforehand which are plain groups so we can serialize them properly
    // ie x = (3, 4), y = [1, x, 2] would be [1, 3, 4, 2] instead of [1, [3, 4], 2]
    for cddl_rule in &cddl.rules {
        if let Rule::Group{ rule, .. } = cddl_rule {
            // Freely defined group - no need to generate anything outside of group module
            match &rule.entry {
                GroupEntry::InlineGroup{ group, .. } => {
                    global.mark_plain_group(convert_to_camel_case(&rule.name.to_string()), group.clone());
                },
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        }
    }
    for cddl_rule in &cddl.rules {
        println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
        match cddl_rule {
            Rule::Type{ rule, .. } => {
                // (1) does not handle optional generic parameters
                // (2) is_type_choice_alternate ignored since shelley.cddl doesn't need it
                //     It's used, but used for no reason as it is the initial definition
                //     (which is also valid cddl), but it would be fine as = instead of /=
                // (3) ignores control operators - only used in shelley spec to limit string length for application metadata
                let rule_name = convert_to_camel_case(&rule.name.to_string());
                if rule.value.type_choices.len() == 1 {
                    let choice = &rule.value.type_choices.first().unwrap();
                    generate_type(&mut global, &rule_name, &choice.type2, None);
                } else {
                    let variants = global.create_variants_from_type_choices(&rule.value.type_choices);
                    global.generate_type_choices(&rule_name, &variants, None);
                }
            },
            Rule::Group{ rule, .. } => {
                // Freely defined group - no need to generate anything outside of group module
                match &rule.entry {
                    GroupEntry::InlineGroup{ .. } => (),// already handled above
                    x => panic!("Group rule with non-inline group? {:?}", x),
                }
            },
        }
    }
    match std::fs::remove_dir_all("export/src") {
        Ok(()) => (),
        Err(_) => (),
    };
    std::fs::create_dir_all("export/src").unwrap();
    std::fs::write("export/src/lib.rs", global.scope().to_string()).unwrap();
    std::fs::write("export/src/serialization.rs", global.serialize_scope().to_string()).unwrap();
    std::fs::copy("static/Cargo.toml", "export/Cargo.toml").unwrap();
    std::fs::copy("static/prelude.rs", "export/src/prelude.rs").unwrap();

    println!("\n\nPlain groups:");
    for plain_group in global.plain_groups.iter() {
        println!("{}", plain_group.0);
    }

    Ok(())
}
