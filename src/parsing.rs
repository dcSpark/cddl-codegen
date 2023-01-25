use cddl::ast::parent::ParentVisitor;
use cddl::{ast::*, token};
use either::{Either};
use std::collections::{BTreeMap};

use crate::comment_ast::{RuleMetadata, metadata_from_comments};
use crate::intermediate::{
    AliasIdent,
    CDDLIdent,
    ConceptualRustType,
    EnumVariant,
    FixedValue,
    GenericDef,
    GenericInstance,
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

#[derive(Clone, Debug)]
enum ControlOperator {
    Range((Option<i128>, Option<i128>)),
    CBOR(RustType),
    Default(FixedValue),
}

pub const SCOPE_MARKER: &'static str = "_CDDL_CODEGEN_SCOPE_MARKER_";

/// Some means it is a scope marker, containing the scope
pub fn rule_is_scope_marker(cddl_rule: &cddl::ast::Rule) -> Option<String> {
    match cddl_rule {
        Rule::Type{ rule: TypeRule{ name: Identifier{ ident , .. }, value, .. }, .. } => {
            if value.type_choices.len() == 1 && ident.starts_with(SCOPE_MARKER) {
                match &value.type_choices[0].type1.type2 {
                    Type2::TextValue{ value, .. } => Some(value.to_string()),
                    _ => None,
                }
            } else {
                None
            }
        },
        _ => None,
    }
}

pub fn parse_rule(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, cddl_rule: &cddl::ast::Rule) {
    match cddl_rule {
        cddl::ast::Rule::Type{ rule, .. } => {
            // (1) is_type_choice_alternate ignored since shelley.cddl doesn't need it
            //     It's used, but used for no reason as it is the initial definition
            //     (which is also valid cddl), but it would be fine as = instead of /=
            // (2) ignores control operators - only used in shelley spec to limit string length for application metadata
            let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
            let generic_params = rule
                .generic_params
                .as_ref()
                .map(|gp| gp.params.iter().map(|id| RustIdent::new(CDDLIdent::new(id.param.to_string()))).collect::<Vec<_>>());
            if rule.value.type_choices.len() == 1 {
                let choice = &rule.value.type_choices.first().unwrap();
                parse_type(types, parent_visitor, &rust_ident, choice, None, generic_params);
            } else {
                parse_type_choices(types, parent_visitor, &rust_ident, &rule.value.type_choices, None, generic_params);
            }
        },
        cddl::ast::Rule::Group{ rule, .. } => {
            assert_eq!(rule.generic_params, None, "{}: Generics not supported on plain groups", rule.name);
            // Freely defined group - no need to generate anything outside of group module
            // already handled in main.rs
            match &rule.entry {
                cddl::ast::GroupEntry::InlineGroup{ .. } => (),
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        },
    }
}

pub fn rule_ident(cddl_rule: &cddl::ast::Rule) -> RustIdent {
    match cddl_rule {
        cddl::ast::Rule::Type{ rule, .. } => RustIdent::new(CDDLIdent::new(rule.name.to_string())),
        cddl::ast::Rule::Group{ rule, .. } => match &rule.entry {
            cddl::ast::GroupEntry::InlineGroup{ .. } => RustIdent::new(CDDLIdent::new(rule.name.to_string())),
            x => panic!("Group rule with non-inline group? {:?}", x),
        },
    }
}

fn parse_type_choices(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, name: &RustIdent, type_choices: &Vec<TypeChoice>, tag: Option<usize>, generic_params: Option<Vec<RustIdent>>) {
    let optional_inner_type = if type_choices.len() == 2 {
        let a = &type_choices[0].type1;
        let b = &type_choices[1].type1;
        if type2_is_null(&a.type2) {
            Some(b)
        } else if type2_is_null(&b.type2) {
            Some(a)
        } else {
            None
        }
    } else {
        None
    };
    if let Some(inner_type2) = optional_inner_type {
        if generic_params.is_some() {
            // the current generic support relies on having a RustStruct to swap out the types with
            // but that won't happen with T / null types since we generate an alias instead
            todo!("support foo<T> = T / null");
        }
        let inner_rust_type = rust_type_from_type1(types, parent_visitor, inner_type2);
        let final_type = match tag {
            Some(tag) => RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))).tag(tag),
            None => RustType::new(ConceptualRustType::Optional(Box::new(inner_rust_type))),
        };
        types.register_type_alias(name.clone(), final_type, true, true);
    } else {
        let variants = create_variants_from_type_choices(types, parent_visitor, type_choices);
        let rust_struct = RustStruct::new_type_choice(name.clone(), tag, variants);
        match generic_params {
            Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
            None => types.register_rust_struct(parent_visitor, rust_struct),
        };
    }
}

fn type2_to_number_literal(type2: &Type2) -> isize {
    match type2 {
        Type2::UintValue{ value, .. } => *value as isize,
        Type2::IntValue{ value, .. } => *value,
        Type2::FloatValue{ value, .. } => *value as isize,
        _ => panic!("Value specified: {:?} must be a number literal to be used here", type2),
    }
}

fn type2_to_fixed_value(type2: &Type2) -> FixedValue {
    match type2 {
        Type2::UintValue{ value, .. } => FixedValue::Uint(*value),
        Type2::IntValue{ value, .. } => FixedValue::Nint(*value),
        Type2::FloatValue{ value, .. } => FixedValue::Float(*value),
        Type2::TextValue{ value, .. } => FixedValue::Text(value.to_string()),
        _ => panic!("Type2: {:?} does not correspond to a supported FixedValue", type2),
    }
}

fn parse_control_operator(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, type2: &Type2, operator: &Operator) -> ControlOperator {
    let lower_bound = match type2 {
        Type2::Typename{ ident, .. } if ident.to_string() == "uint" => Some(0),
        _ => None,
    };
    //todo: read up on other range control operators in CDDL RFC
    // (rangeop / ctlop) S type2
    match operator.operator {
        RangeCtlOp::RangeOp{ is_inclusive, .. } => {
            let range_start = match type2 {
                Type2::UintValue{ value, .. } => *value as isize,
                Type2::IntValue{ value, .. } => *value,
                Type2::FloatValue{ value, .. } => *value as isize,
                _ => panic!("Number expected as range start. Found {:?}", type2)
            };
            let range_end = match operator.type2 {
                Type2::UintValue{ value, .. } => value as isize,
                Type2::IntValue{ value, ..} => value,
                Type2::FloatValue{ value, .. } => value as isize,
                _ => unimplemented!("unsupported type in range control operator: {:?}", operator),
            };
            ControlOperator::Range((Some(range_start as i128), Some(if is_inclusive { range_end  as i128 } else { (range_end + 1)  as i128 })))
        },
        RangeCtlOp::CtlOp{ ctrl, .. } => match ctrl {
            token::ControlOperator::CBORSEQ |
            token::ControlOperator::WITHIN |
            token::ControlOperator::AND => todo!("control operator {} not supported", ctrl),
            token::ControlOperator::DEFAULT => ControlOperator::Default(type2_to_fixed_value(&operator.type2)),
            token::ControlOperator::CBOR => ControlOperator::CBOR(rust_type_from_type2(types, parent_visitor, &operator.type2)),
            token::ControlOperator::EQ => ControlOperator::Range((Some(type2_to_number_literal(&operator.type2)  as i128), Some(type2_to_number_literal(&operator.type2)  as i128))),
            // TODO: this would be MUCH nicer (for error displaying, etc) to handle this in its own dedicated way
            //       which might be necessary once we support other control operators anyway
            token::ControlOperator::NE => ControlOperator::Range((Some((type2_to_number_literal(&operator.type2) + 1) as i128), Some((type2_to_number_literal(&operator.type2) - 1) as i128))),
            token::ControlOperator::LE => ControlOperator::Range((lower_bound, Some(type2_to_number_literal(&operator.type2) as i128))),
            token::ControlOperator::LT => ControlOperator::Range((lower_bound, Some((type2_to_number_literal(&operator.type2) - 1) as i128))),
            token::ControlOperator::GE => ControlOperator::Range((Some(type2_to_number_literal(&operator.type2) as i128), None)),
            token::ControlOperator::GT => ControlOperator::Range((Some((type2_to_number_literal(&operator.type2) + 1) as i128), None)),
            token::ControlOperator::SIZE => {
                let base_range = match &operator.type2 {
                    Type2::UintValue{ value, .. } => ControlOperator::Range((None, Some(*value as i128))),
                    Type2::IntValue{ value, .. } => ControlOperator::Range((None, Some(*value as i128))),
                    Type2::FloatValue{ value, .. } => ControlOperator::Range((None, Some(*value as i128))),
                    Type2::ParenthesizedType{ pt, .. } => {
                        assert_eq!(pt.type_choices.len(), 1);
                        let inner_type = &pt.type_choices.first().unwrap().type1;
                        let min = match inner_type.type2 {
                            Type2::UintValue{ value, .. } => Some(value as i128),
                            Type2::IntValue{ value, .. } => Some(value as i128),
                            Type2::FloatValue{ value, .. } => Some(value as i128),
                            _ => unimplemented!("unsupported type in range control operator: {:?}", operator),
                        };
                        match &inner_type.operator {
                            // if there was only one value instead of a range, we take that value to be the max
                            // ex: uint .size (1)
                            None => ControlOperator::Range((None, min)),
                            Some(op) => match op.operator {
                                RangeCtlOp::RangeOp{ is_inclusive, ..} => {
                                    let value = match op.type2 {
                                        Type2::UintValue{ value, .. } => value as i128,
                                        Type2::IntValue{ value, ..} => value as i128,
                                        Type2::FloatValue{ value, .. } => value as i128,
                                        _ => unimplemented!("unsupported type in range control operator: {:?}", operator),
                                    };
                                    let max = Some(if is_inclusive { value } else { value + 1 });
                                    ControlOperator::Range((min, max))
                                },
                                RangeCtlOp::CtlOp{ .. } => panic!(""),
                            },
                        }
                    },
                    _ => unimplemented!("unsupported type in range control operator: {:?}", operator),
                };
                match type2 {
                    Type2::Typename{ ident, .. } if ident.to_string() == "uint" => {
                        // .size 3 means 24 bits
                        match &base_range {
                            ControlOperator::Range((Some(l), Some(h))) => ControlOperator::Range((Some(i128::pow(2, 8 * *l as u32)), Some(i128::pow(2, 8 * *h as u32) - 1))),
                            ControlOperator::Range((None, Some(h))) => ControlOperator::Range((Some(0), Some(i128::pow(2, 8 * *h as u32) - 1))),
                            _ => panic!("unexpected partial range in size control operator: {:?}", operator)
                        }
                    },
                    Type2::Typename{ ident, .. } if ident.to_string() == "int" => {
                        match &base_range {
                            // this is complex to support since it requires two disjoint ranges of possible values
                            ControlOperator::Range((Some(_), Some(_))) => panic!(".size range unsupported for signed int type: {:?}", operator),
                            ControlOperator::Range((None, Some(h))) => ControlOperator::Range((Some(-i128::pow(2, ((8 * *h) - 1) as u32)), Some(i128::pow(2, ((8 * *h) - 1) as u32) - 1))),
                            _ => panic!("unexpected partial range in size control operator: {:?}", operator)
                        }
                    }
                    _ => {
                        match base_range {
                            // for strings & byte arrays, specifying an upper value means an exact value (.size 3 means a 3 char string)
                            ControlOperator::Range((None, Some(h))) => ControlOperator::Range((Some(h), Some(h))),
                            range => range,
                        }
                    }
                }
            },
            _ => panic!("Unknown (not seen in RFC-8610) range control operator: {}", ctrl),
        }
    }
}

fn range_to_primitive(low: Option<i128>, high: Option<i128>) -> Option<ConceptualRustType> {
    match (low, high) {
        (Some(l), Some(h)) if l == u8::MIN as i128 && h == u8::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::U8)),
        (Some(l), Some(h)) if l == i8::MIN as i128 && h == i8::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::I8)),
        (Some(l), Some(h)) if l == u16::MIN as i128 && h == u16::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::U16)),
        (Some(l), Some(h)) if l == i16::MIN as i128 && h == i16::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::I16)),
        (Some(l), Some(h)) if l == u32::MIN as i128 && h == u32::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::U32)),
        (Some(l), Some(h)) if l == i32::MIN as i128 && h == i32::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::I32)),
        (Some(l), Some(h)) if l == u64::MIN as i128 && h == u64::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::U64)),
        (Some(l), Some(h)) if l == i64::MIN as i128 && h == i64::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::I64)),
        (Some(l), Some(h)) if l == f32::MIN as i128 && h == f32::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::F32)),
        (Some(l), Some(h)) if l == f64::MIN as i128 && h == f64::MAX as i128 => Some(ConceptualRustType::Primitive(Primitive::F64)),
        _ => None
    }
}

fn parse_type(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, type_name: &RustIdent, type_choice: &TypeChoice, outer_tag: Option<usize>, generic_params: Option<Vec<RustIdent>>) {
    let type1 = &type_choice.type1;
    match &type1.type2 {
        Type2::Typename{ ident, generic_args, .. } => {
            // Note: this handles bool constants too, since we apply the type aliases and they resolve
            // and there's no Type2::BooleanValue
            let cddl_ident = CDDLIdent::new(ident.to_string());
            let control = type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op));
            match control {
                Some(control) => {
                    assert!(generic_params.is_none(), "Generics combined with range specifiers not supported");
                    // TODO: what about aliases that resolve to these? is it even possible to know this at this stage?
                    let field_type = || match cddl_ident.to_string().as_str() {
                        "tstr" | "text" => ConceptualRustType::Primitive(Primitive::Str),
                        "bstr" | "bytes" => ConceptualRustType::Primitive(Primitive::Bytes),
                        other => panic!("range control specifiers not supported for type: {}", other),
                    };
                    match control {
                        ControlOperator::Range(min_max) => {
                            match cddl_ident.to_string().as_str() {
                                "int" | "uint" | "float64" | "float32" => match range_to_primitive(min_max.0, min_max.1) {
                                    Some(t) => types.register_type_alias(type_name.clone(), t.into(), true, true),
                                    None => panic!("unsupported range for {:?}: {:?}", cddl_ident.to_string().as_str(), control)
                                },
                                _ => types.register_rust_struct(parent_visitor, RustStruct::new_wrapper(type_name.clone(), outer_tag, field_type().clone().into(), Some(min_max)))
                            }
                        },
                        ControlOperator::CBOR(ty) => match field_type() {
                            ConceptualRustType::Primitive(Primitive::Bytes) => {
                                types.register_type_alias(type_name.clone(), ty.as_bytes(), true, true);
                            },
                            _ => panic!(".cbor is only allowed on bytes as per CDDL spec"),
                        },
                        ControlOperator::Default(default_value) => {
                            let default_type = rust_type_from_type2(types, parent_visitor, &type1.type2)
                                .default(default_value);
                            types.register_type_alias(type_name.clone(), default_type, true, true);
                        },
                    }
                },
                None => {
                    let mut concrete_type = types.new_type(&cddl_ident);
                    if let ConceptualRustType::Alias(_ident, ty) = concrete_type.conceptual_type {
                        concrete_type.conceptual_type = *ty;
                    };
                    match &generic_params {
                        Some(_params) => {
                            // this should be the only situation where you need this as otherwise the params would be unbound
                            todo!("generics on defined types e.g. foo<T, U> = [T, U], bar<V> = foo<V, uint>");
                            // TODO: maybe you could do this by resolving it here then storing the resolved one as GenericDef
                        },
                        None => {
                            match generic_args {
                                Some(arg) => {
                                    // This is for named generic instances such as:
                                    // foo = bar<text>
                                    let generic_args = arg.args.iter().map(|a| rust_type_from_type1(types, parent_visitor, &a.arg)).collect();
                                    types.register_generic_instance(GenericInstance::new(type_name.clone(), RustIdent::new(cddl_ident.clone()), generic_args))
                                },
                                None => {
                                    let rule_metadata = RuleMetadata::from(type1.comments_after_type.as_ref());
                                    if rule_metadata.is_newtype {
                                        types.register_rust_struct(parent_visitor, RustStruct::new_wrapper(type_name.clone(), None, concrete_type, None)); 
                                    } else {
                                        types.register_type_alias(type_name.clone(), concrete_type, true, true);
                                    }
                                }
                            }
                        }
                    }
                },
            }
        },
        Type2::Map{ group, .. } => {
            parse_group(types, parent_visitor, group, type_name, Representation::Map, outer_tag, generic_params);
        },
        Type2::Array{ group, .. } => {
            // TODO: We could potentially generate an array-wrapper type around this
            // possibly based on the occurency specifier.
            parse_group(types, parent_visitor, group, type_name, Representation::Array, outer_tag, generic_params);
        },
        Type2::TaggedData{ tag, t, .. } => {
            if let Some(_) = outer_tag {
                panic!("doubly nested tags are not supported");
            }
            let tag_unwrap = tag.expect("not sure what empty tag here would mean - unsupported");
            match t.type_choices.len() {
                1 => {
                    let inner_type = &t.type_choices.first().unwrap();
                    match match &inner_type.type1.type2 {
                        Type2::Typename{ ident, .. } => Either::Right(ident),
                        Type2::Map{ group, .. } => Either::Left(group),
                        Type2::Array{ group, .. } => Either::Left(group),
                        x => panic!("only supports tagged arrays/maps/typenames - found: {:?} in rule {}", x, type_name),
                    } {
                        Either::Left(_group) => parse_type(types, parent_visitor, type_name, inner_type, *tag, generic_params),
                        Either::Right(ident) => {
                            let new_type = types.new_type(&CDDLIdent::new(ident.to_string()));
                            let control = inner_type.type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &inner_type.type1.type2, op));
                            match control {
                                Some(ControlOperator::CBOR(ty)) => {
                                    let base_type = types
                                        .apply_type_aliases(&AliasIdent::new(CDDLIdent::new(ident.to_string())))
                                        .expect("should not fail since ordered by dep graph");
                                    assert_eq!(base_type.conceptual_type, ConceptualRustType::Primitive(Primitive::Bytes));
                                    assert_eq!(base_type.default, None);
                                    assert!(base_type.encodings.is_empty());
                                    types.register_type_alias(type_name.clone(), ty.as_bytes().tag(tag_unwrap), true, true);
                                },
                                Some(ControlOperator::Range(min_max)) => {
                                    match ident.to_string().as_str() {
                                        "int" | "uint" | "float64" | "float32" => match range_to_primitive(min_max.0, min_max.1) {
                                            Some(t) => types.register_type_alias(type_name.clone(), t.into(), true, true),
                                            None => panic!("unsupported range for {:?}: {:?}", ident.to_string().as_str(), control)
                                        },
                                        _ => types.register_rust_struct(parent_visitor, RustStruct::new_wrapper(type_name.clone(), *tag, new_type, Some(min_max)))
                                    }
                                },
                                Some(ControlOperator::Default(default_value)) => {
                                    let default_tagged_type = rust_type_from_type2(types, parent_visitor, &inner_type.type1.type2)
                                        .default(default_value)
                                        .tag(tag_unwrap);
                                    types.register_type_alias(type_name.clone(), default_tagged_type, true, true);
                                },
                                None => {
                                    // TODO: this would be fixed if we ordered definitions via a dependency graph to begin with
                                    // which would also allow us to do a single pass instead of many like we do now
                                    let base_type = types
                                        .apply_type_aliases(&AliasIdent::new(CDDLIdent::new(ident.to_string())))
                                        .expect("should not fail since ordered by dep graph");
                                    types.register_type_alias(type_name.clone(), base_type.tag(tag_unwrap), true, true);
                                },
                            }
                        },
                    };
                },
                _ => {
                    parse_type_choices(types, parent_visitor, type_name, &t.type_choices, *tag, generic_params);
                }
            };
        },
        // Note: bool constants are handled via Type2::Typename
        Type2::IntValue{ value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Nint(*value));

            let control = type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op));
            let base_type = match control {
                Some(ControlOperator::Range(min_max)) => {
                    match range_to_primitive(min_max.0, min_max.1) {
                        Some(t) => t,
                        _ => fallback_type
                    }
                },
                _ => fallback_type
            };
            types.register_type_alias(type_name.clone(), base_type.into(), true, true);
        },
        Type2::UintValue{ value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Uint(*value));

            let control = type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op));
            let base_type = match control {
                Some(ControlOperator::Range(min_max)) => {
                    match range_to_primitive(min_max.0, min_max.1) {
                        Some(t) => t,
                        _ => fallback_type
                    }
                },
                _ => fallback_type
            };
            types.register_type_alias(type_name.clone(), base_type.into(), true, true);
        },
        Type2::TextValue{ value, .. } => {
            types.register_type_alias(type_name.clone(), ConceptualRustType::Fixed(FixedValue::Text(value.to_string())).into(), true, true);
        },
        Type2::FloatValue { value, .. } => {
            let fallback_type = ConceptualRustType::Fixed(FixedValue::Float(*value));

            let control = type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op));
            let base_type = match control {
                Some(ControlOperator::Range(min_max)) => {
                    match range_to_primitive(min_max.0, min_max.1) {
                        Some(t) => t,
                        _ => fallback_type
                    }
                },
                _ => fallback_type
            };
            types.register_type_alias(type_name.clone(), base_type.into(), true, true);
        },
        x => {
            panic!("\nignored typename {} -> {:?}\n", type_name, x);
        },
    }
}

// TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos
pub fn create_variants_from_type_choices(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, type_choices: &Vec<TypeChoice>) -> Vec<EnumVariant> {
    let mut variant_names_used = BTreeMap::<String, u32>::new();
    type_choices.iter().map(|choice| {
        let rust_type = rust_type_from_type1(types, parent_visitor, &choice.type1);
        let base_name = match RuleMetadata::from(choice.type1.comments_after_type.as_ref()) {
            RuleMetadata { name: Some(name), .. } => convert_to_camel_case(&name),
            _ => rust_type.for_variant().to_string(),
        };
        let variant_name = append_number_if_duplicate(&mut variant_names_used, base_name);
        EnumVariant::new(VariantIdent::new_custom(variant_name), rust_type, false)
    }).collect()
}

fn table_domain_range<'a>(group_choice: &'a GroupChoice<'a>, rep: Representation) -> Option<(&'a Type1<'a>, &'a Type<'a>)> {
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
                            return Some((&t1, &ge.entry_type));
                        },
                        // has a fixed value - this is just a 1-element struct
                        Some(MemberKey::Value{ .. }) => return None,
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

// would use rust_type_from_type1 but that requires IntermediateTypes which we shouldn't
fn type2_is_null(t2: &Type2) -> bool {
    match t2 {
        Type2::Typename{ ident, .. } => ident.ident == "null" || ident.ident == "nil",
        _ => false,
    }
}

fn type_to_field_name(t: &Type) -> Option<String> {
    let type2_to_field_name = |t2: &Type2| match t2 {
        Type2::Typename{ ident, .. } => Some(ident.to_string()),
            Type2::TextValue { value, .. } => Some(value.to_string()),
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
        1 => type2_to_field_name(&t.type_choices.first().unwrap().type1.type2),
        2 => {
            // special case for T / null -> maps to Option<T> so field name should be same as just T
            let a = &t.type_choices[0].type1.type2;
            let b = &t.type_choices[1].type1.type2;
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

fn _field_name_from_comments<'a>(comments: &Option<Comments<'a>>) -> Option<String> {
    comments
        .as_ref()?
        .0
        .iter()
        .find(|c| c.trim().starts_with("field:"))
        .map(|c| c.trim()[6..].trim().to_owned())
}

fn combine_comments<'a>(a: &'a Option<Comments>, b: &'a Option<Comments>) -> Option<Vec<&'a str>> {
    match (a.as_ref().map(|comment| comment.0.clone()), b.as_ref().map(|comment| comment.0.clone())) {
        (Some(a), Some(b)) => Some([a, b].concat()),
        (opt_a, opt_b) => opt_a.or(opt_b)
    }
}

// Attempts to use the style-converted type name as a field name, and if we have already
// generated one, then we simply add numerals starting at 2, 3, 4...
// If you wish to only check if there is an explicitly stated field name,
// then use group_entry_to_raw_field_name()
fn group_entry_to_field_name(entry: &GroupEntry, index: usize, already_generated: &mut BTreeMap<String, u32>, optional_comma: &OptionalComma) -> String {
    //println!("group_entry_to_field_name() = {:#?}", entry);
    let field_name = convert_to_snake_case(&match entry {
        GroupEntry::ValueMemberKey{ trailing_comments, ge, .. } => match ge.member_key.as_ref() {
            Some(member_key) => match member_key {
                MemberKey::Value{ value, .. } => {
                    let combined_comments = combine_comments(trailing_comments, &optional_comma.trailing_comments);
                    match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                        RuleMetadata { name: Some(name), .. } => name,
                        _ => format!("key_{}", value)
                    }
                },
                MemberKey::Bareword{ ident, .. } => ident.to_string(),
                MemberKey::Type1{ t1, .. } => match t1.type2 {
                    Type2::UintValue{ value, .. } => format!("key_{}", value),
                    _ => panic!("Encountered Type1 member key in multi-field map - not supported: {:?}", entry),
                },
                MemberKey::NonMemberKey{ .. } => panic!("Please open a github issue with repro steps"),
            },
            None => {
                type_to_field_name(&ge.entry_type).unwrap_or_else(|| {
                    let combined_comments = combine_comments(trailing_comments, &optional_comma.trailing_comments);
                    match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                        RuleMetadata { name: Some(name), .. } => name,
                        _ => format!("index_{}", index),
                    }
                })
            }
        },
        GroupEntry::TypeGroupname{ trailing_comments, ge: TypeGroupnameEntry { name, .. }, .. } => match !is_identifier_user_defined(&name.to_string()) {
            true => {
                let combined_comments = combine_comments(trailing_comments, &optional_comma.trailing_comments);
                match metadata_from_comments(&combined_comments.unwrap_or_default()) {
                    RuleMetadata { name: Some(name), .. } => name,
                    _ => format!("index_{}", index),
                }
            },
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

fn rust_type_from_type1(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, type1: &Type1) -> RustType {
    let control = type1.operator.as_ref().map(|op| parse_control_operator(types, parent_visitor, &type1.type2, op));
    let base_type = rust_type_from_type2(types, parent_visitor, &type1.type2);
    // println!("type1: {:#?}", type1);
    match control {
        Some(ControlOperator::CBOR(ty)) => {
            assert!(matches!(base_type.conceptual_type.resolve_aliases(), ConceptualRustType::Primitive(Primitive::Bytes)));
            ty.as_bytes()
        },
        Some(ControlOperator::Range(min_max)) => {
            match &type1.type2 {
                Type2::Typename{ ident, .. } if ident.to_string() == "uint" || ident.to_string() == "int" || ident.to_string() == "float32" || ident.to_string() == "float64" => match range_to_primitive(min_max.0, min_max.1) {
                    Some(t) => t.into(),
                    None => panic!("unsupported range for {:?}: {:?}", ident.to_string().as_str(), control)
                },
                _ => base_type
            }
        },
        Some(ControlOperator::Default(default_value)) => base_type.default(default_value),
        None => base_type,
    }
}

fn rust_type_from_type2(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, type2: &Type2) -> RustType {
    // TODO: socket plugs (used in hash type)
    match &type2 {
        Type2::UintValue{ value, .. } => ConceptualRustType::Fixed(FixedValue::Uint(*value)).into(),
        Type2::IntValue{ value, .. } => ConceptualRustType::Fixed(FixedValue::Nint(*value)).into(),
        Type2::FloatValue{ value, .. } => ConceptualRustType::Fixed(FixedValue::Float(*value)).into(),
        Type2::TextValue{ value, .. } => ConceptualRustType::Fixed(FixedValue::Text(value.to_string())).into(),
        Type2::Typename{ ident, generic_args, .. } => {
            let cddl_ident = CDDLIdent::new(ident.ident);
            match generic_args {
                Some(args) => {
                    // This is for anonymous instances (i.e. members) such as:
                    // foo = [a: bar<text, bool>]
                    // so to be able to expose it to wasm, we create a new generic instance
                    // under the name bar_string_bool in this case.
                    let generic_args = args.args.iter().map(|a| rust_type_from_type1(types, parent_visitor, &a.arg)).collect::<Vec<_>>();
                    let args_name = generic_args.iter().map(|t| t.for_variant().to_string()).collect::<Vec<String>>().join("_");
                    let instance_cddl_ident = CDDLIdent::new(format!("{}_{}", cddl_ident, args_name));
                    let instance_ident = RustIdent::new(instance_cddl_ident.clone());
                    let generic_ident = RustIdent::new(cddl_ident);
                    types.register_generic_instance(GenericInstance::new(instance_ident, generic_ident, generic_args));
                    types.new_type(&instance_cddl_ident)
                },
                None => types.new_type(&cddl_ident),
            }
        },
        Type2::Array{ group, .. } => {
            // TODO: support for group choices in arrays?
            let element_type = match group.group_choices.len() {
                1 => {
                    let choice = &group.group_choices.first().unwrap();
                    // special case for homogenous arrays
                    if choice.group_entries.len() == 1 {
                        let (entry, _has_comma) = choice.group_entries.first().unwrap();
                        match entry {
                            GroupEntry::ValueMemberKey{ ge, .. } => rust_type(types, parent_visitor, &ge.entry_type),
                            GroupEntry::TypeGroupname{ ge, .. } => types.new_type(&CDDLIdent::new(&ge.name.to_string())),
                            _ => panic!("UNSUPPORTED_ARRAY_ELEMENT<{:?}>", entry),
                        }
                    } else {
                        let rule_metadata = RuleMetadata::from(get_comment_after(parent_visitor, &CDDLType::from(type2), None).as_ref());;
                        let name = match rule_metadata.name.as_ref() {
                            Some(name) => name,
                            None => panic!("Anonymous groups not allowed. Either create an explicit rule (foo = [0, bytes]) or give it a name using the @name notation. Group: {:#?}", group)
                        };
                        let cddl_ident = CDDLIdent::new(name);
                        let rust_ident = RustIdent::new(cddl_ident.clone());
                        parse_group(types, parent_visitor, group, &rust_ident, Representation::Array, None, None);
                        // we aren't returning an array, but rather a struct where the fields are ordered
                        return types.new_type(&cddl_ident)
                    }
                },
                // array of elements with choices: enums?
                _ => unimplemented!("group choices in array type not supported"),
            };

            //let array_wrapper_name = element_type.name_as_wasm_array();
            //types.create_and_register_array_type(element_type, &array_wrapper_name)
            if let ConceptualRustType::Rust(element_ident) = &element_type.conceptual_type {
                types.set_rep_if_plain_group(parent_visitor, element_ident, Representation::Array);
            }
            ConceptualRustType::Array(Box::new(element_type)).into()
        },
        Type2::Map { group, .. } => {
            match group.group_choices.len() {
                1 => {
                    let group_choice = group.group_choices.first().unwrap();
                    let table_types = table_domain_range(group_choice, Representation::Map);
                    match table_types {
                        // Table map - homogenous key/value types
                        Some((domain, range)) => {
                            let key_type = rust_type_from_type1(types, parent_visitor, domain);
                            let value_type = rust_type(types, parent_visitor, range);
                            // Generate a MapTToV for a { t => v } table-type map as we are an anonymous type
                            // defined as part of another type if we're in this level of parsing.
                            // We also can't have plain groups unlike arrays, so don't try and generate those
                            // for general map types we can though but not for tables
                            //let table_type_ident = RustIdent::new(CDDLIdent::new(format!("Map{}To{}", key_type.for_wasm_member(), value_type.for_wasm_member())));
                            //types.register_rust_struct(RustStruct::new_table(table_type_ident, None, key_type.clone(), value_type.clone()));
                            ConceptualRustType::Map(Box::new(key_type), Box::new(value_type)).into()
                        },
                        None => unimplemented!("TODO: non-table types as types: {:?}", group),
                    }
                },
                _ => unimplemented!("group choices in inlined map types not allowed: {:?}", group),
            }
        },
        // unsure if we need to handle the None case - when does this happen?
        Type2::TaggedData{ tag, t, .. } => {
            let tag_unwrap = tag.expect("tagged data without tag not supported");
            rust_type(types, parent_visitor, t).tag(tag_unwrap)
        },
        Type2::ParenthesizedType { pt, .. } => {
            rust_type(types, parent_visitor, pt)
        },
        _ => {
            panic!("Ignoring Type2: {:?}", type2);
        },
    }
}

fn rust_type(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, t: &Type) -> RustType {
    if t.type_choices.len() == 1 {
        rust_type_from_type1(types, parent_visitor, &t.type_choices.first().unwrap().type1)
    } else {
        if t.type_choices.len() == 2 {
            // T / null   or   null / T   should map to Option<T>
            let a = &t.type_choices[0].type1;
            let b = &t.type_choices[1].type1;
            if type2_is_null(&a.type2) {
                return ConceptualRustType::Optional(Box::new(rust_type_from_type1(types, parent_visitor, b))).into();
            }
            if type2_is_null(&b.type2) {
                return ConceptualRustType::Optional(Box::new(rust_type_from_type1(types, parent_visitor, a))).into();
            }
        }
        let variants = create_variants_from_type_choices(types, parent_visitor, &t.type_choices);
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
        types.register_rust_struct(parent_visitor, RustStruct::new_type_choice(combined_ident, None, variants));
        types.new_type(&CDDLIdent::new(combined_name))
    }
}

fn group_entry_optional(entry: &GroupEntry) -> bool {
    let occur = match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => &ge.occur,
        GroupEntry::TypeGroupname{ ge, .. } => &ge.occur,
        GroupEntry::InlineGroup{ .. } => panic!("inline group entries are not implemented"),
    };
    occur
        .as_ref()
        .map(|o| match o.occur {
            Occur::Optional { .. } => true,
            _ => false,
        })
        .unwrap_or(false)
}

fn group_entry_to_type(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, entry: &GroupEntry) -> RustType {
    let ret = match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => rust_type(types, parent_visitor, &ge.entry_type),
        GroupEntry::TypeGroupname{ ge, .. } => {
            if ge.generic_args.is_some() {
                // I am not sure how we end up with this kind of generic args since definitional ones
                // and member ones are created elsewhere. I thought that if you had a field like
                // foo: bar<uint> it would be here but it turns out it's in the ValueMemberKey
                // variant instead.
                todo!("If you run into this please create a github issue and include the .cddl that caused it");
            }
            let cddl_ident = CDDLIdent::new(ge.name.to_string());
            types.new_type(&cddl_ident)
        },
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
                    cddl::token::Value::INT(x) => Some(FixedValue::Nint(*x)),
                    cddl::token::Value::TEXT(x) => Some(FixedValue::Text(x.to_string())),
                    cddl::token::Value::FLOAT(x) => Some(FixedValue::Float(*x)),
                    _ => panic!("unsupported map identifier(1): {:?}", value),
                },
                MemberKey::Bareword{ ident, .. } => Some(FixedValue::Text(ident.to_string())),
                MemberKey::Type1{ t1, .. } => match &t1.type2 {
                    Type2::UintValue{ value, .. } => Some(FixedValue::Uint(*value)),
                    Type2::IntValue{ value, .. } => Some(FixedValue::Nint(*value)),
                    Type2::TextValue{ value, .. } => Some(FixedValue::Text(value.to_string())),
                    Type2::FloatValue { value, .. } => Some(FixedValue::Float(*value)),
                    _ => panic!("unsupported map identifier(2): {:?}", entry),
                },
                MemberKey::NonMemberKey{ .. } => panic!("Please open a github issue with repro steps"),
            }
        },
        _ => None,
    }
}

fn parse_record_from_group_choice(types: &mut IntermediateTypes, rep: Representation, parent_visitor: &ParentVisitor, group_choice: &GroupChoice) -> RustRecord {
    let mut generated_fields = BTreeMap::<String, u32>::new();
    let fields = group_choice.group_entries.iter().enumerate().map(
        |(index, (group_entry, optional_comma))| {
            let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields, optional_comma);
            // does not exist for fixed values importantly
            let field_type = group_entry_to_type(types, parent_visitor, group_entry);
            if let ConceptualRustType::Rust(ident) = &field_type.conceptual_type {
                types.set_rep_if_plain_group(parent_visitor, ident, rep);
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

fn parse_group_choice<'a>(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, group_choice: &'a GroupChoice, name: &RustIdent, rep: Representation, tag: Option<usize>, generic_params: Option<Vec<RustIdent>>) {
    let table_types = table_domain_range(group_choice, rep);
    let rust_struct = match table_types {
        // Table map - homogenous key/value types
        Some((domain, range)) => {
            let key_type = rust_type_from_type1(types, parent_visitor, domain);
            let value_type = rust_type(types, parent_visitor, range);
            RustStruct::new_table(name.clone(), tag, key_type, value_type)
        },
        // Heterogenous map (or array!) with defined key/value pairs in the cddl like a struct
        None => {
            let record = parse_record_from_group_choice(types, rep, parent_visitor, group_choice);
            // We need to store this in IntermediateTypes so we can refer from one struct to another.
            RustStruct::new_record(name.clone(), tag, record)
        }
    };
    match generic_params {
        Some(params) => types.register_generic_def(GenericDef::new(params, rust_struct)),
        None => types.register_rust_struct(parent_visitor, rust_struct),
    };
}

pub fn parse_group(types: &mut IntermediateTypes, parent_visitor: &ParentVisitor, group: &Group, name: &RustIdent, rep: Representation, tag: Option<usize>, generic_params: Option<Vec<RustIdent>>) {
    if group.group_choices.len() == 1 {
        // Handle simple (no choices) group.
        parse_group_choice(types, parent_visitor, group.group_choices.first().unwrap(), name, rep, tag, generic_params);
    } else {
        if generic_params.is_some() {
            todo!("{}: generic group choices not supported", name);
        }
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
                let ty = group_entry_to_type(types, parent_visitor, group_entry);
                let serialize_as_embedded = if let ConceptualRustType::Rust(ident) = &ty.conceptual_type {
                    // we might need to generate it if not used elsewhere
                    types.set_rep_if_plain_group(parent_visitor, ident, rep);
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
                let rule_metadata = RuleMetadata::from(group_choice.comments_before_grpchoice.as_ref());
                let ident_name = rule_metadata.name.unwrap_or_else(|| format!("{}{}", name, i));
                // General case, GroupN type identifiers and generate group choice since it's inlined here
                let variant_name = RustIdent::new(CDDLIdent::new(ident_name));
                types.mark_plain_group(variant_name.clone(), None);
                parse_group_choice(types, parent_visitor, group_choice, &variant_name, rep, None, generic_params.clone());
                EnumVariant::new(VariantIdent::new_rust(variant_name.clone()), ConceptualRustType::Rust(variant_name).into(), true)
            }
        }).collect();
        types.register_rust_struct(parent_visitor, RustStruct::new_group_choice(name.clone(), tag, variants, rep));
    }
}

fn get_comments_if_group_parent<'a>(parent_visitor: &'a ParentVisitor<'a, 'a>, cddl_type: &CDDLType<'a, 'a>, child: Option<&CDDLType<'a, 'a>>, comments_after_group: &Option<Comments<'a>>) -> Option<Comments<'a>> {
    if let Some(CDDLType::Group(_)) = child {
        return comments_after_group.clone()
    }
    get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type))
}
fn get_comments_if_type_parent<'a>(parent_visitor: &'a ParentVisitor<'a, 'a>, cddl_type: &CDDLType<'a, 'a>, child: Option<&CDDLType<'a, 'a>>, comments_after_type: &Option<Comments<'a>>) -> Option<Comments<'a>> {
    if let Some(CDDLType::Type(_)) = child {
        return comments_after_type.clone()
    }
    get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type))
}


/// Gets the comment(s) that come after a type by parsing the CDDL AST
/// 
/// (implementation detail) sometimes getting the comment after a type requires walking up the AST
/// This happens when whether or not the type has a comment after it depends in which structure it is embedded in
///    For example, CDDLType::Group has no "comment_after_group" type
///    However, when part of a Type2::Array, it does have a "comment_after_group" embedded inside the Type2
/// 
/// Note: we do NOT merge comments when the type is coincidentally the last node inside its parent structure
///    For example, the last CDDLType::GroupChoice inside a CDDLType::Group will not return its parent's comment
fn get_comment_after<'a>(parent_visitor: &'a ParentVisitor<'a, 'a>, cddl_type: &CDDLType<'a, 'a>, child: Option<&CDDLType<'a, 'a>>) -> Option<Comments<'a>> {
    match cddl_type {
        CDDLType::CDDL(_) => None,
        CDDLType::Rule(t) =>
            match t {
                Rule::Type { comments_after_rule, .. } => comments_after_rule.clone(),
                Rule::Group { comments_after_rule, .. } => comments_after_rule.clone(),
            },
        CDDLType::TypeRule(_) => get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type)),
        CDDLType::GroupRule(_) => get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type)),
        CDDLType::Group(_) => {
            match cddl_type.parent(parent_visitor).unwrap() {
                parent@CDDLType::GroupEntry(_) => get_comment_after(parent_visitor, parent, Some(cddl_type)),
                parent@CDDLType::Type2(_) => get_comment_after(parent_visitor, parent, Some(cddl_type)),
                parent@CDDLType::MemberKey(_) => get_comment_after(parent_visitor, parent, Some(cddl_type)),
                _ => None,
            }
        },
        // TODO: handle child by looking up the group entry in group_entries
        // the expected behavior of this may instead be to combine_comments based off its parents
        // which is a slippery slope in complexity
        CDDLType::GroupChoice(_) => None,
        CDDLType::GenericParams(_) => None,
        CDDLType::GenericParam(t) => {
            if let Some(CDDLType::Identifier(_)) = child {
                return t.comments_after_ident.clone()
            }
            None
        },
        CDDLType::GenericArgs(_) => None,
        CDDLType::GenericArg(t) => {
            if let Some(CDDLType::Type1(_)) = child {
                return t.comments_after_type.clone()
            }
            None
        },
        CDDLType::GroupEntry(t) => match t {
            GroupEntry::ValueMemberKey { trailing_comments, .. } => trailing_comments.clone(),
            GroupEntry::TypeGroupname { trailing_comments, .. } => trailing_comments.clone(),
            GroupEntry::InlineGroup { comments_after_group, .. } => {
                if let Some(CDDLType::Group(_)) = child {
                    return comments_after_group.clone()
                }
                None
            },
        },
        CDDLType::Identifier(_) => None, // TODO: recurse up for GenericParam
        CDDLType::Type(_) => None,
        CDDLType::TypeChoice(t) => t.comments_after_type.clone(),
        CDDLType::Type1(t) => {
            if let Some(CDDLType::Type2(_)) = child {
                if let Some(op) = &t.operator {
                    return op.comments_before_operator.clone()
                } else {
                    return t.comments_after_type.clone()
                }
            };
            if t.operator.is_none() {
                get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type))
            } else {
                None
            }
        },
        CDDLType::Type2(t) => match t {
            Type2::ParenthesizedType { comments_after_type, .. } => get_comments_if_type_parent(parent_visitor, cddl_type, child, comments_after_type),
            Type2::Map { comments_after_group, .. } => get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group),
            Type2::Array { comments_after_group, .. } => get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group),
            Type2::Unwrap { .. } => get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type)),
            Type2::ChoiceFromInlineGroup { comments_after_group, .. } => get_comments_if_group_parent(parent_visitor, cddl_type, child, comments_after_group),
            Type2::ChoiceFromGroup { .. } => get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type)),
            Type2::TaggedData { comments_after_type, .. } => get_comments_if_type_parent(parent_visitor, cddl_type, child, comments_after_type),
            _ => None,
        },
        CDDLType::Operator(t) => {
            if let Some(CDDLType::RangeCtlOp(_)) = child {
                return t.comments_after_operator.clone()
            }
            if let Some(CDDLType::Type2(t2)) = child {
                // "comments_before_operator" is associated with the 1st type2 and not the second (t.type2)
                if std::ptr::eq(*t2, &t.type2) {
                    return None
                } else {
                    return t.comments_before_operator.clone()
                }
            }
            None
        },
        CDDLType::Occurrence(t) => t.comments.clone(),
        CDDLType::Occur(_) => None,
        CDDLType::Value(_) => None,
        CDDLType::ValueMemberKeyEntry(_) => None,
        CDDLType::TypeGroupnameEntry(_) => None,
        CDDLType::MemberKey(t) => match t {
            MemberKey::NonMemberKey { comments_after_type_or_group, .. } => comments_after_type_or_group.clone(),
            _ => None
        },
        CDDLType::NonMemberKey(_) => get_comment_after(parent_visitor, cddl_type.parent(parent_visitor).unwrap(), Some(cddl_type)),
        _ => None
    }
}

fn get_rule_name<'a, 'b>(parent_visitor: &'a ParentVisitor, cddl_type: &CDDLType<'a, 'b>) -> Identifier<'a> {
    match cddl_type {
        CDDLType::CDDL(_) => panic!("Cannot get the rule name of a top-level CDDL node"),
        CDDLType::Rule(t) =>
            match t {
                Rule::Type { rule, .. } => get_rule_name(parent_visitor, &CDDLType::from(rule)),
                Rule::Group { rule, .. } => get_rule_name(parent_visitor, &CDDLType::from(rule.as_ref())),
            },
        CDDLType::TypeRule(t) => t.name.clone(),
        CDDLType::GroupRule(t) => t.name.clone(),
        other => get_rule_name(parent_visitor, other.parent(parent_visitor).unwrap()),
    }
}
