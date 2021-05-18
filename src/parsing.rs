use cddl::ast::*;
use either::{Either};
use std::collections::{BTreeMap};

use crate::cmd::{
    BINARY_WRAPPERS,
};
use crate::intermediate::{
    AliasIdent,
    CDDLIdent,
    EnumVariant,
    FixedValue,
    IntermediateTypes,
    Primitive,
    Representation,
    RustIdent,
    RustRecord,
    RustStruct,
    RustType,
    RustField,
    VariantIdent,
};
use crate::utils::{
    append_number_if_duplicate,
    convert_to_camel_case,
    convert_to_snake_case,
    is_identifier_user_defined,
};

pub fn parse_rule(types: &mut IntermediateTypes, cddl_rule: &cddl::ast::Rule) {
    match cddl_rule {
        cddl::ast::Rule::Type{ rule, .. } => {
            // (1) does not handle optional generic parameters
            // (2) is_type_choice_alternate ignored since shelley.cddl doesn't need it
            //     It's used, but used for no reason as it is the initial definition
            //     (which is also valid cddl), but it would be fine as = instead of /=
            // (3) ignores control operators - only used in shelley spec to limit string length for application metadata
            let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
            if rule.value.type_choices.len() == 1 {
                let choice = &rule.value.type_choices.first().unwrap();
                parse_type(types, &rust_ident, &choice.type2, None);
            } else {
                parse_type_choices(types, &rust_ident, &rule.value.type_choices, None);
            }
        },
        cddl::ast::Rule::Group{ rule, .. } => {
            // Freely defined group - no need to generate anything outside of group module
            match &rule.entry {
                cddl::ast::GroupEntry::InlineGroup{ .. } => (),// already handled in main.rs
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        },
    }
}

fn parse_type_choices(types: &mut IntermediateTypes, name: &RustIdent, type_choices: &Vec<Type1>, tag: Option<usize>) {
    let optional_inner_type = if type_choices.len() == 2 {
        let a = &type_choices[0].type2;
        let b = &type_choices[1].type2;
        if type2_is_null(a) {
            Some(b)
        } else if type2_is_null(b) {
            Some(a)
        } else {
            None
        }
    } else {
        None
    };
    if let Some(inner_type2) = optional_inner_type {
        let inner_rust_type = rust_type_from_type2(types, inner_type2);
        match tag {
            // only want to create a wrapper if we NEED to - so that we can keep the tag information
            Some(_) => {
                types.register_rust_struct(RustStruct::new_wrapper(name.clone(), tag, RustType::Optional(Box::new(inner_rust_type))));
            },
            // otherwise a simple typedef of type $name = Option<$inner_rust_type>; works better
            None => {
                types.register_type_alias(name.clone(), RustType::Optional(Box::new(inner_rust_type)), true);
            },
        };
    } else {
        let variants = create_variants_from_type_choices(types, type_choices);
        types.register_rust_struct(RustStruct::new_type_choice(name.clone(), tag, variants));
    }
}

fn parse_type(types: &mut IntermediateTypes, type_name: &RustIdent, type2: &Type2, outer_tag: Option<usize>) {
    match type2 {
        Type2::Typename{ ident, .. } => {
            // Note: this handles bool constants too, since we apply the type aliases and they resolve
            // and there's no Type2::BooleanValue
            let cddl_ident = CDDLIdent::new(ident.to_string());
            // This should be controlled in a better way - maybe we can annotate the cddl
            // to specify whether or not we want to simply to a typedef to Vec<u8> for bytes
            // or whether we want to do what we are doing here and creating a custom type.
            // This is more specific to our own use-case since the binary types via type alises
            // in the shelley.cddl spec are things that should have their own type and we would
            // want to expand upon the code later on.
            // Perhaps we could change the cddl and have some specific tag like "BINARY_FORMAT"
            // to generate this?
            let generate_binary_wrapper = BINARY_WRAPPERS && match ident.to_string().as_ref() {
                "bytes" | "bstr" => true,
                _ident => if let Some(RustType::Primitive(Primitive::Bytes)) = types.apply_type_aliases(&AliasIdent::new(cddl_ident.clone())) {
                    true
                } else {
                    false
                },
            };
            if generate_binary_wrapper {
                let field_type = RustType::Primitive(Primitive::Bytes);
                types.register_rust_struct(RustStruct::new_wrapper(type_name.clone(), outer_tag, field_type.clone()));
                types.register_type_alias(type_name.clone(), field_type, false);
            } else {
                let concrete_type = match types.new_type(&cddl_ident) {
                    RustType::Alias(_ident, ty) => *ty,
                    ty => ty,
                };
                types.register_type_alias(type_name.clone(), concrete_type, true);
            }
        },
        Type2::Map{ group, .. } => {
            parse_group(types, group, type_name, Representation::Map, outer_tag);
        },
        Type2::Array{ group, .. } => {
            // TODO: We could potentially generate an array-wrapper type around this
            // possibly based on the occurency specifier.
            parse_group(types, group, type_name, Representation::Array, outer_tag);
        },
        Type2::TaggedData{ tag, t, .. } => {
            if let Some(_) = outer_tag {
                panic!("doubly nested tags are not supported");
            }
            tag.expect("not sure what empty tag here would mean - unsupported");
            match t.type_choices.len() {
                1 => {
                    let inner_type = &t.type_choices.first().unwrap().type2;
                    match match inner_type {
                        Type2::Typename{ ident, .. } => Either::Right(ident),
                        Type2::Map{ group, .. } => Either::Left(group),
                        Type2::Array{ group, .. } => Either::Left(group),
                        x => panic!("only supports tagged arrays/maps/typenames - found: {:?} in rule {}", x, type_name),
                    } {
                        Either::Left(_group) => parse_type(types, type_name, inner_type, *tag),
                        Either::Right(ident) => {
                            let new_type = types.new_type(&CDDLIdent::new(ident.to_string()));
                            types.register_rust_struct(RustStruct::new_wrapper(type_name.clone(), *tag, new_type))
                        },
                    };
                },
                _ => {
                    parse_type_choices(types, type_name, &t.type_choices, *tag);
                }
            };
        },
        // Note: bool constants are handled via Type2::Typename
        Type2::IntValue{ value, .. } => {
            types.register_type_alias(type_name.clone(), RustType::Fixed(FixedValue::Int(*value)), true);
        },
        Type2::UintValue{ value, .. } => {
            types.register_type_alias(type_name.clone(), RustType::Fixed(FixedValue::Uint(*value)), true);
        },
        Type2::TextValue{ value, .. } => {
            types.register_type_alias(type_name.clone(), RustType::Fixed(FixedValue::Text(value.clone())), true);
        },
        x => {
            panic!("\nignored typename {} -> {:?}\n", type_name, x);
        },
    }
}

// TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos
pub fn create_variants_from_type_choices(types: &mut IntermediateTypes, type_choices: &Vec<Type1>) -> Vec<EnumVariant> {
    let mut variant_names_used = BTreeMap::<String, u32>::new();
    type_choices.iter().map(|type1| {
        let rust_type = rust_type_from_type2(types, &type1.type2);
        let variant_name = append_number_if_duplicate(&mut variant_names_used, rust_type.for_variant().to_string());
        EnumVariant::new(VariantIdent::new_custom(variant_name), rust_type, false)
    }).collect()
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

// would use rust_type_from_type2 but that requires IntermediateTypes which we shouldn't
fn type2_is_null(t2: &Type2) -> bool {
    match t2 {
        Type2::Typename{ ident, .. } => ident.ident == "null" || ident.ident == "nil",
        _ => false,
    }
}

fn type_to_field_name(t: &Type) -> Option<String> {
    let type2_to_field_name = |t2: &Type2| match t2 {
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
    };
    match t.type_choices.len() {
        1 => type2_to_field_name(&t.type_choices.first().unwrap().type2),
        2 => {
            // special case for T / null -> maps to Option<T> so field name should be same as just T
            let a = &t.type_choices[0].type2;
            let b = &t.type_choices[1].type2;
            if type2_is_null(a) {
                type2_to_field_name(b)
            } else if type2_is_null(b) {
                type2_to_field_name(a)
            } else {
                // neither are null - we do not support type choices here
                None
            }
        },
        // no type choice support here
        _ => None,
    }
}

// Attempts to use the style-converted type name as a field name, and if we have already
// generated one, then we simply add numerals starting at 2, 3, 4...
// If you wish to only check if there is an explicitly stated field name,
// then use group_entry_to_raw_field_name()
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
        GroupEntry::TypeGroupname{ ge: TypeGroupnameEntry { name, .. }, .. } => match !is_identifier_user_defined(&name.to_string()) {
            true => format!("index_{}", index),
            false => name.to_string(),
        },
        GroupEntry::InlineGroup{ group, .. } => panic!("not implemented (define a new struct for this!) = {}\n\n {:?}", group, group),
    });
    append_number_if_duplicate(already_generated, field_name.clone())
}

// Only returns Some(String) if there was an explicit field name provided, otherwise None.
// If you need to try and make one using the type/etc, then try group_entry_to_field_name()
// Also does not do any CamelCase or snake_case formatting.
fn group_entry_to_raw_field_name(entry: &GroupEntry) -> Option<String> {
    match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => match ge.member_key.as_ref() {
            Some(MemberKey::Bareword{ ident, .. } ) => Some(ident.to_string()),
            _ => None,
        },
        GroupEntry::TypeGroupname{ ge: TypeGroupnameEntry { name, .. }, .. } => match !is_identifier_user_defined(&name.to_string()) {
            true => None,
            false => Some(name.to_string()),
        },
        GroupEntry::InlineGroup{ group, .. } => panic!("not implemented (define a new struct for this!) = {}\n\n {:?}", group, group),
    }
}

fn rust_type_from_type2(types: &mut IntermediateTypes, type2: &Type2) -> RustType {
    // TODO: generics
    // TODO: socket plugs (used in hash type)
    match type2 {
        Type2::UintValue{ value, .. } => RustType::Fixed(FixedValue::Uint(*value)),
        Type2::IntValue{ value, .. } => RustType::Fixed(FixedValue::Int(*value)),
        //Type2::FloatValue{ value, .. } => RustType::Fixed(FixedValue::Float(*value)),
        Type2::TextValue{ value, .. } => RustType::Fixed(FixedValue::Text(value.clone())),
        Type2::Typename{ ident, .. } => types.new_type(&CDDLIdent::new(&ident.ident)),
        Type2::Array{ group, .. } => {
            // TODO: support for group choices in arrays?
            let element_type = match group.group_choices.len() {
                1 => {
                    let choice = &group.group_choices.first().unwrap();
                    // special case for homogenous arrays
                    if choice.group_entries.len() == 1 {
                        let (entry, _has_comma) = choice.group_entries.first().unwrap();
                        match entry {
                            GroupEntry::ValueMemberKey{ ge, .. } => rust_type(types, &ge.entry_type),
                            GroupEntry::TypeGroupname{ ge, .. } => types.new_type(&CDDLIdent::new(&ge.name.to_string())),
                            _ => panic!("UNSUPPORTED_ARRAY_ELEMENT<{:?}>", entry),
                        }
                    } else {
                        // array of non-choice element that has multiple fields: tuples? create seperately?
                        panic!("TODO: how do we handle this? tuples? or creating a struct definition and referring to it by name?")
                    }
                },
                // array of elements with choices: enums?
                _ => unimplemented!("group choices in array type not supported"),
            };
            let array_wrapper_name = element_type.name_as_array();
            types.create_and_register_array_type(element_type, &array_wrapper_name)
        },
        Type2::Map { group, .. } => {
            match group.group_choices.len() {
                1 => {
                    let group_choice = group.group_choices.first().unwrap();
                    let table_types = table_domain_range(group_choice, Representation::Map);
                    match table_types {
                        // Table map - homogenous key/value types
                        Some((domain, range)) => {
                            let key_type = rust_type_from_type2(types, domain);
                            let value_type = rust_type(types, range);
                            // Generate a MapTToV for a { t => v } table-type map as we are an anonymous type
                            // defined as part of another type if we're in this level of parsing.
                            // We also can't have plain groups unlike arrays, so don't try and generate those
                            // for general map types we can though but not for tables
                            let table_type_ident = RustIdent::new(CDDLIdent::new(format!("Map{}To{}", key_type.for_member(), value_type.for_member())));
                            types.register_rust_struct(RustStruct::new_table(table_type_ident, None, key_type.clone(), value_type.clone()));
                            RustType::Map(Box::new(key_type), Box::new(value_type))
                        },
                        None => unimplemented!("TODO: non-table types as types: {:?}", group),
                    }
                },
                _ => unimplemented!("group choices in inlined map types not allowed: {:?}", group),
            }
        },
        // unsure if we need to handle the None case - when does this happen?
        Type2::TaggedData{ tag, t, .. } => {
            RustType::Tagged(tag.expect("tagged data without tag not supported"), Box::new(rust_type(types, t)))
        },
        _ => {
            panic!("Ignoring Type2: {:?}", type2);
        },
    }
}

fn rust_type(types: &mut IntermediateTypes, t: &Type) -> RustType {
    if t.type_choices.len() == 1 {
        rust_type_from_type2(types, &t.type_choices.first().unwrap().type2)
    } else {
        if t.type_choices.len() == 2 {
            // T / null   or   null / T   should map to Option<T>
            let a = &t.type_choices[0].type2;
            let b = &t.type_choices[1].type2;
            if type2_is_null(a) {
                return RustType::Optional(Box::new(rust_type_from_type2(types, b)));
            }
            if type2_is_null(b) {
                return RustType::Optional(Box::new(rust_type_from_type2(types, a)));
            }
        }
        let variants = create_variants_from_type_choices(types, &t.type_choices);
        let mut combined_name = String::new();
        // one caveat: nested types can leave ambiguous names and cause problems like
        // (a / b) / c and a / (b / c) would both be AOrBOrC
        for variant in &variants {
            if !combined_name.is_empty() {
                combined_name.push_str("Or");
            }
            // due to undercase primitive names, we need to convert here
            combined_name.push_str(&variant.rust_type.for_variant().to_string());
        }
        let combined_ident = RustIdent::new(CDDLIdent::new(&combined_name));
        types.register_rust_struct(RustStruct::new_type_choice(combined_ident, None, variants));
        types.new_type(&CDDLIdent::new(combined_name))
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

fn group_entry_to_type(types: &mut IntermediateTypes, entry: &GroupEntry) -> RustType {
    let ret = match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => rust_type(types, &ge.entry_type),
        GroupEntry::TypeGroupname{ ge, .. } => types.new_type(&CDDLIdent::new(ge.name.to_string())),
        GroupEntry::InlineGroup{ .. } => panic!("inline group entries are not implemented"),
    };
    //println!("group_entry_to_typename({:?}) = {:?}\n", entry, ret);
    ret
}

fn group_entry_to_key(entry: &GroupEntry) -> Option<FixedValue> {
    match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => {
            match ge.member_key.as_ref()? {
                MemberKey::Value{ value, .. } => match value {
                    cddl::token::Value::UINT(x) => Some(FixedValue::Uint(*x)),
                    cddl::token::Value::INT(x) => Some(FixedValue::Int(*x)),
                    cddl::token::Value::TEXT(x) => Some(FixedValue::Text(x.clone())),
                    _ => panic!("unsupported map identifier(1): {:?}", value),
                },
                MemberKey::Bareword{ ident, .. } => Some(FixedValue::Text(ident.to_string())),
                MemberKey::Type1{ t1, .. } => match &t1.type2 {
                    Type2::UintValue{ value, .. } => Some(FixedValue::Uint(*value)),
                    Type2::IntValue{ value, .. } => Some(FixedValue::Int(*value)),
                    Type2::TextValue{ value, .. } => Some(FixedValue::Text(value.clone())),
                    _ => panic!("unsupported map identifier(2): {:?}", entry),
                },
            }
        },
        _ => None,
    }
}

fn parse_record_from_group_choice(types: &mut IntermediateTypes, rep: Representation, group_choice: &GroupChoice) -> RustRecord {
    let mut generated_fields = BTreeMap::<String, u32>::new();
    let fields = group_choice.group_entries.iter().enumerate().map(
        |(index, (group_entry, _has_comma))| {
            let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields);
            // does not exist for fixed values importantly
            let field_type = group_entry_to_type(types, group_entry);
            if let RustType::Rust(ident) = &field_type {
                types.set_rep_if_plain_group(ident, rep);
            }
            let optional_field = group_entry_optional(group_entry);
            let key = match rep {
                Representation::Map => Some(group_entry_to_key(group_entry).expect("map fields need keys")),
                Representation::Array => None,
            };
            RustField::new(field_name, field_type, optional_field, key)
        }
    ).collect();
    RustRecord {
        rep,
        fields,
    }
}

fn parse_group_choice(types: &mut IntermediateTypes, group_choice: &GroupChoice, name: &RustIdent, rep: Representation, tag: Option<usize>) {
    let table_types = table_domain_range(group_choice, rep);
    match table_types {
        // Table map - homogenous key/value types
        Some((domain, range)) => {
            let key_type = rust_type_from_type2(types, domain);
            let value_type = rust_type(types, range);
            types.register_rust_struct(RustStruct::new_table(name.clone(), tag, key_type, value_type));
        },
        // Heterogenous map (or array!) with defined key/value pairs in the cddl like a struct
        None => {
            let record = parse_record_from_group_choice(types, rep, group_choice);
            // We need to store this in IntermediateTypes so we can refer from one struct to another.
            types.register_rust_struct(RustStruct::new_record(name.clone(), tag, record));
        }
    }
}

pub fn parse_group(types: &mut IntermediateTypes, group: &Group, name: &RustIdent, rep: Representation, tag: Option<usize>) {
    if group.group_choices.len() == 1 {
        // Handle simple (no choices) group.
        parse_group_choice(types, group.group_choices.first().unwrap(), name, rep, tag);
    } else {
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // TODO: We don't support generating SerializeEmbeddedGroup for group choices which is necessary for plain groups
        // It would not be as trivial to add as we do the outer group's array/map tag writing inside the variant match
        // to avoid having to always generate SerializeEmbeddedGroup when not necessary.
        assert!(!types.is_plain_group(name));
        
        // Handle group with choices by generating an enum then generating a group for every choice
        let mut variants_names_used = BTreeMap::<String, u32>::new();
        let variants: Vec<EnumVariant> = group.group_choices.iter().enumerate().map(|(i, group_choice)| {
            // If we're a 1-element we should just wrap that type in the variant rather than
            // define a new struct just for each variant.
            // TODO: handle map-based enums? It would require being able to extract the key logic
            // We might end up doing this anyway to support table-maps in choices though.
            if group_choice.group_entries.len() == 1 {
                let group_entry = &group_choice.group_entries.first().unwrap().0;
                let ty = group_entry_to_type(types, group_entry);
                let serialize_as_embedded = if let RustType::Rust(ident) = &ty {
                    // we might need to generate it if not used elsewhere
                    types.set_rep_if_plain_group(ident, rep);
                    types.is_plain_group(ident)
                } else {
                    false
                };
                let variant_ident = convert_to_camel_case(&match group_entry_to_raw_field_name(group_entry) {
                    Some(name) => name,
                    None => append_number_if_duplicate(&mut variants_names_used, ty.for_variant().to_string()),
                });
                let variant_ident = VariantIdent::new_custom(variant_ident);
                EnumVariant::new(variant_ident, ty, serialize_as_embedded)
                // None => {
                //     // TODO: Weird case, group choice with only one fixed-value field.
                //     // What should we do here? In the future we could make this a
                //     // non-value-taking enum then handle this in the serialization code.
                //     // However, for now we just default to default behavior:
                //     let variant_name = format!("{}{}", name, i);
                //     // TODO: Should we generate these within their own namespace?
                //     codegen_group_choice(global, group_choice, &variant_name, rep, None);
                //     EnumVariant::new(variant_name.clone(), RustType::Rust(variant_name), true)
                // },
            } else {
                // General case, GroupN type identifiers and generate group choice since it's inlined here
                let variant_name = RustIdent::new(CDDLIdent::new(format!("{}{}", name, i)));
                types.mark_plain_group(variant_name.clone(), None);
                // TODO: Should we generate these within their own namespace?
                parse_group_choice(types, group_choice, &variant_name, rep, None);
                EnumVariant::new(VariantIdent::new_rust(variant_name.clone()), RustType::Rust(variant_name), true)
            }
        }).collect();
        types.register_rust_struct(RustStruct::new_group_choice(name.clone(), tag, variants, rep));
    }
}