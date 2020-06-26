mod codegen_helpers;

use cbor_event::Type as CBORType;
use cbor_event::Special as CBORSpecial;
use cddl::ast::*;
use codegen::{Block};
use codegen_helpers::{CodeBlock, DataType};
use std::collections::{BTreeMap, BTreeSet};
use either::{Either};

// TODO: add these as cmd args
// TODO: make non-annotation generate different DeserializeError that is simpler
//       and works with From<cbor_event:Error> only
const ANNOTATE_FIELDS: bool = true;

#[derive(Copy, Clone, Debug, PartialEq)]
enum Representation {
    Array,
    Map,
}

fn _cbor_type_code_str(cbor_type: CBORType) -> &'static str {
    match cbor_type {
        CBORType::UnsignedInteger => "CBORType::UnsignedInteger",
        CBORType::NegativeInteger => "CBORType::NegativeInteger",
        CBORType::Bytes => "CBORType::Bytes",
        CBORType::Text => "CBORType::Text",
        CBORType::Array => "CBORType::Array",
        CBORType::Map => "CBORType::Map",
        CBORType::Tag => "CBORType::Tag",
        CBORType::Special => "CBORType::Special",
    }
}

#[derive(Clone, Debug)]
enum FixedValue {
    Null,
    Bool(bool),
    Int(isize),
    Uint(usize),
    // float not supported right now - doesn't appear to be in cbor_event
    //Float(f64),
    Text(String),
    // UTF byte types not supported
}

fn convert_to_alphanumeric(input: &str) -> String {
    input.chars().filter(|c| c.is_ascii_alphanumeric()).collect()
}

impl FixedValue {
    fn for_variant(&self) -> VariantIdent {
        match self {
            FixedValue::Null => VariantIdent::new_custom("Null"),
            FixedValue::Bool(b) => VariantIdent::new_custom(match b {
                true => "True",
                false => "False",
            }),
            FixedValue::Int(i) => VariantIdent::new_custom(format!("U{}", i)),
            FixedValue::Uint(u) => VariantIdent::new_custom(format!("I{}", u)),
            //FixedValue::Float(f) => format!("F{}", f),
            FixedValue::Text(s) => VariantIdent::new_custom(convert_to_alphanumeric(&convert_to_camel_case(&s))),
        }
    }
}

#[derive(Clone, Debug)]
enum Primitive {
    Bool,
    U32,
    I32,
    Str,
    Bytes,
}

// TODO: impl display or fmt or whatever rust uses
impl Primitive {
    fn to_string(&self) -> String {
        String::from(match self {
            Primitive::Bool => "bool",
            Primitive::U32 => "u32",
            Primitive::I32 => "i32",
            Primitive::Str => "String",
            Primitive::Bytes => "Vec<u8>",
        })
    }

    fn to_variant(&self) -> VariantIdent {
        VariantIdent::new_custom(match self {
            Primitive::Bool => "Bool",
            Primitive::U32 => "U32",
            Primitive::I32 => "I32",
            Primitive::Str => "Text",
            Primitive::Bytes => "Bytes",
        })
    }

    fn cbor_type(&self) -> CBORType {
        match self {
            Primitive::Bool => CBORType::Special,
            Primitive::U32 => CBORType::UnsignedInteger,
            Primitive::I32 => CBORType::NegativeInteger,
            Primitive::Str => CBORType::Text,
            Primitive::Bytes => CBORType::Bytes,
        }
    }
}

mod idents {
    // to resolve ambiguities between raw (from CDDL) and already-formatted
    // for things like type aliases, etc, we use this wrapper struct for both
    #[derive(Clone, Debug)]
    pub struct CDDLIdent(String);

    impl CDDLIdent {
        pub fn new<T: Into<String>>(raw: T) -> Self {
            Self(raw.into())
        }
    }

    // impl<'a> From<&'a CDDLIdent> for &'a str {
    //     fn from(ident: &'a CDDLIdent) -> &'a str {
    //         &ident.0
    //     }
    // }
    // why does this not compile?
    // impl From<&CDDLIdent> for &str {
    //     fn from(ident: &CDDLIdent) -> &str {
    //         &ident.0
    //     }
    // }
    // since it doesn't compile, using this for now:
    impl std::fmt::Display for CDDLIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub struct RustIdent(String);

    impl RustIdent {
        // this should not be created directly, but instead via GlobalScope::new_type()
        // except for defining new cddl rules, since those should not be reserved identifiers
        pub fn new(cddl_ident: CDDLIdent) -> Self {
            assert!(!super::is_identifier_reserved(&cddl_ident.0));
            Self(super::convert_to_camel_case(&cddl_ident.0))
        }
    }

    impl std::fmt::Display for RustIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    pub enum VariantIdent {
        Custom(String),
        RustStruct(RustIdent),
    }

    impl VariantIdent {
        pub fn new_custom<T: Into<String>>(name: T) -> Self {
            VariantIdent::Custom(name.into())
        }

        pub fn new_rust(ident: RustIdent) -> Self {
            VariantIdent::RustStruct(ident)
        }
    }

    impl std::fmt::Display for VariantIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                VariantIdent::Custom(name) => write!(f, "{}", name),
                VariantIdent::RustStruct(ident) => ident.fmt(f),
            }
        }
    }

    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum AliasIdent {
        Reserved(String),
        Rust(RustIdent),
    }

    impl AliasIdent {
        pub fn new(ident: CDDLIdent) -> Self {
            if super::is_identifier_reserved(&ident.0) {
                AliasIdent::Reserved(ident.0)
            } else {
                AliasIdent::Rust(RustIdent::new(ident))
            }
        }
    }

    impl From<RustIdent> for AliasIdent {
        fn from(ident: RustIdent) -> AliasIdent {
            AliasIdent::Rust(ident)
        }
    }
}
use idents::*;

#[derive(Clone, Debug)]
enum RustType {
    Fixed(FixedValue),
    // Primitive type that can be passed to/from wasm - TODO: make this an enum so we can get type guarantees on it
    Primitive(Primitive),
    // Rust-defined type that cannot be put in arrays/etc
    Rust(RustIdent),
    // Array-wrapped type. Passed as Vec<T> if T is Primitive
    Array(Box<RustType>),
    // Tagged type. Behavior depends entirely on wrapped type.
    Tagged(usize, Box<RustType>),
    // T / null in CBOR - auto-converts to Option<T> in rust for ease of use.
    Optional(Box<RustType>),
    // TODO: table type to support inlined defined table-type groups as fields
    Map(Box<RustType>, Box<RustType>),

    // TODO: for non-table-type ones we could define a RustField(Ident, RustType) and then
    // a variant here Struct(Vec<RustField>) and delegate field/argument generation to
    // RustField so that we could basically expand them and not care about having to generate
    // and intermediate fields - although this could pose an issue for optional types... so maybe
    // another approach would be necessary.
}

impl RustType {
    fn directly_wasm_exposable(&self) -> bool {
        match self {
            RustType::Fixed(_) => false,
            RustType::Primitive(_) => true,
            RustType::Rust(_) => false,
            RustType::Array(ty) => ty.directly_wasm_exposable(),
            RustType::Tagged(_tag, ty) => ty.directly_wasm_exposable(),
            RustType::Optional(ty) => ty.directly_wasm_exposable(),
            RustType::Map(_, _) => false,
        }
    }

    fn is_fixed_value(&self) -> bool {
        match self {
            RustType::Fixed(_) => true,
            RustType::Tagged(_tag, ty) => ty.is_fixed_value(),
            _ => false,
        }
    }

    fn name_as_array(&self) -> String {
        if self.directly_wasm_exposable() {
            format!("Vec<{}>", self.for_wasm())
        } else {
            format!("{}s", self.for_wasm())
        }
    }

    // TODO: should we unify this and for_member() in to_string()? (they were different previously) will we need the distinction for other types?
    fn for_wasm(&self) -> String {
        match self {
            RustType::Fixed(_) => panic!("should not expose Fixed type to wasm, only here for serialization: {:?}", self),
            RustType::Primitive(s) => s.to_string(),
            RustType::Rust(s) => s.to_string(),
            RustType::Array(ty) => ty.name_as_array(),
            RustType::Tagged(_tag, ty) => ty.for_wasm(),
            RustType::Optional(ty) => format!("Option<{}>", ty.for_wasm()),
            RustType::Map(k, v) => format!("Map{}To{}", k.for_wasm(), v.for_wasm()),
        }
    }

    fn for_member(&self) -> String {
        match self {
            RustType::Fixed(_) => panic!("should not expose Fixed type in member, only needed for serializaiton: {:?}", self),
            RustType::Primitive(s) => s.to_string(),
            RustType::Rust(s) => s.to_string(),
            RustType::Array(ty) => ty.name_as_array(),
            RustType::Tagged(_tag, ty) => ty.for_member(),
            RustType::Optional(ty) => format!("Option<{}>", ty.for_member()),
            RustType::Map(k, v) => format!("Map{}To{}", k.for_member(), v.for_member()),
        }
    }

    fn for_variant(&self) -> VariantIdent {
        match self {
            RustType::Fixed(f) => f.for_variant(),
            RustType::Primitive(p) => p.to_variant(),
            RustType::Rust(ident) => VariantIdent::new_rust(ident.clone()),
            RustType::Array(inner) => VariantIdent::new_custom(format!("Arr{}", inner.for_variant())),
            RustType::Tagged(_tag, ty) => ty.for_variant(),
            // TODO: should we not end up in this situation and just insert a Null fixed value instead?
            RustType::Optional(ty) => VariantIdent::new_custom(format!("Opt{}", ty.for_variant())),
            RustType::Map(k, v) => VariantIdent::new_custom(format!("Map{}To{}", k.for_variant(), v.for_variant())),
        }
    }

    // TODO: should we get rid of from_wasm_boundary() entirely, or will it be useful for things that aren't tagged types?        
    fn from_wasm_boundary(&self, expr: &str) -> String {
        match self {
            //RustType::Tagged(tag, ty) => format!("TaggedData::<{}>::new({}, {})", ty.for_member(), expr, tag),
            _ => expr.to_owned(),
        }
    }

    // Ok case is single first cbor type in bytes, Err is multiple possibilities
    fn cbor_types(&self) -> Vec<CBORType> {
        match self {
            RustType::Fixed(f) => vec![match f {
                FixedValue::Uint(_) => CBORType::UnsignedInteger,
                FixedValue::Int(_) => CBORType::NegativeInteger,
                FixedValue::Text(_) => CBORType::Text,
                FixedValue::Null => CBORType::Special,
                FixedValue::Bool(_) => CBORType::Special,
            }],
            RustType::Primitive(p) => vec![p.cbor_type()],
            RustType::Rust(_ident) => {
                //panic!("TODO: store first cbor tag somewhere")
                vec![CBORType::Array, CBORType::Map]
            },
            RustType::Tagged(_tag, _ty) => vec![CBORType::Tag],
            RustType::Array(_) => vec![CBORType::Array],
            RustType::Map(_k, _v) => vec![CBORType::Map],
            RustType::Optional(ty) => {
                let mut inner_types = ty.cbor_types();
                if !inner_types.contains(&CBORType::Special) {
                    inner_types.push(CBORType::Special);
                }
                inner_types
            }
        }
    }

    fn _cbor_special_type(&self) -> Option<CBORSpecial> {
        unimplemented!()
    }

    fn _is_serialize_multiline(&self) -> bool {
        match self {
            RustType::Fixed(_) => false,
            RustType::Primitive(_) => false,
            RustType::Rust(_) => false,
            RustType::Array(_) => true,
            RustType::Tagged(_, _) => true,
            RustType::Optional(_) => false,
            RustType::Map(_, _) => false,
        }
    }

    fn expanded_field_count(&self, global: &GlobalScope) -> Option<usize> {
        match self {
            RustType::Optional(ty) => match ty.expanded_field_count(global) {
                Some(1) => Some(1),
                // differing sizes when Null vs Some
                _ => None,
            },
            // TODO: instead of returning None when ident doesn't exist, throw error.
            // Once we split up parsing and codegen this shouldn't happen but with our current multi-pass
            // approach we might have out of order struct references which would break here without it
            // but on the final pass (the one we export) this should't be an issue
            RustType::Rust(ident) => if global.is_plain_group(ident) {
                global.rust_structs.get(&ident)?.fixed_field_count(global)
            } else {
                Some(1)
            },
            _ => Some(1),
        }
    }

    // See comment in RustStruct::definite_info(), this is the same, returns a string expression
    // which evaluates to the length when possible, or None if not.
    // self_expr is an expresison that evaluates to this RustType (e.g. member, etc) at the point where
    // the return of this function will be used.
    fn definite_info(&self, self_expr: &str, global: &GlobalScope) -> Option<String> {
        match self.expanded_field_count(global) {
            Some(count) => Some(count.to_string()),
            None => match self {
                RustType::Optional(ty) => Some(format!("match {} {{ Some(x) => {}, None => 1 }}", self_expr, ty.definite_info("x", global)?)),
                RustType::Rust(ident) => if global.is_plain_group(ident) {
                    match global.rust_structs.get(&ident) {
                        Some(rs) => rs.definite_info(global),
                        // when we split up parsing from codegen instead of multi-passing this should be an error
                        None => None,
                    }
                } else {
                    Some(String::from("1"))
                },
                _ => Some(String::from("1")),
            }
        }
    }
}

#[derive(Clone, Debug)]
struct RustField {
    name: String,
    rust_type: RustType,
    optional: bool,
    // None for array fields, Some for map fields. FixedValue for (de)serialization for map keys
    key: Option<FixedValue>,
}

impl RustField {
    fn new(name: String, rust_type: RustType, optional: bool, key: Option<FixedValue>) -> Self {
        Self {
            name,
            rust_type,
            optional,
            key,
        }
    }
}

// TODO: It would be nice to separate parsing the CDDL lib structs and code generation entirely.
// We would just need to construct these structs (+ maybe the array/table wrappre types) separately and pass these into codegen.
// This would also give us more access to this info without reparsing which could simplify code in some places.
// It would also remove the need for multiple passes over the CDDL to sort out dependencies between structs,
// which could also pave the way for multi-file CDDL supprt.
#[derive(Clone, Debug)]
enum RustStruct {
    Record(RustRecord),
    Table {
        ident: RustIdent,
        domain: RustType,
        range: RustType,
    },
    Array {
        ident: RustIdent,
        element_type: RustType,
    },
}

impl RustStruct {
    fn new_table(ident: RustIdent, domain: RustType, range: RustType) -> Self {
        Self::Table {
            ident,
            domain,
            range,
        }
    }

    fn new_array(ident: RustIdent, element_type: RustType) -> Self {
        Self::Array {
            ident,
            element_type,
        }
    }

    fn ident(&self) -> &RustIdent {
        match self {
            Self::Record(record) => &record.ident,
            Self::Table{ ident, .. } => ident,
            Self::Array{ ident, .. } => ident,
        }
    }
    
    // Some(count) if it always has the same number of fields (ie no optional fields), None otherwise
    fn fixed_field_count(&self, global: &GlobalScope) -> Option<usize> {
        match self {
            Self::Record(record) => record.fixed_field_count(global),
            Self::Table{ .. } => None,
            Self::Array{ .. } => None,
        }
    }

    // Even if fixed_field_count() == None, this will try and return an expression for
    // a definite length, e.g. with optional field checks in the expression
    fn definite_info(&self, global: &GlobalScope) -> Option<String> {
        match self {
            Self::Record(record) => record.definite_info(global),
            Self::Table{ .. } => Some(String::from("self.0.len() as u64")),
            Self::Array{ .. } => Some(String::from("self.0.len() as u64")),
        }
    }
}

// Regular struct with fields and such
#[derive(Clone, Debug)]
struct RustRecord {
    ident: RustIdent,
    rep: Representation,
    fields: Vec<RustField>
}

impl RustRecord {
    fn new_record_from_group_choice(global: &mut GlobalScope, ident: RustIdent, rep: Representation, group_choice: &GroupChoice) -> Self {
        let mut generated_fields = BTreeMap::<String, u32>::new();
        let fields = group_choice.group_entries.iter().enumerate().map(
            |(index, (group_entry, _has_comma))| {
                let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields);
                // does not exist for fixed values importantly
                let field_type = group_entry_to_type(global, group_entry);
                if let RustType::Rust(ident) = &field_type {
                    global.generate_if_plain_group(ident.clone(), rep);
                }
                let optional_field = group_entry_optional(group_entry);
                let key = match rep {
                    Representation::Map => Some(group_entry_to_key(group_entry).expect("map fields need keys")),
                    Representation::Array => None,
                };
                RustField::new(field_name, field_type, optional_field, key)
            }
        ).collect();
        Self {
            ident,
            rep,
            fields,
        }
    }

    fn fixed_field_count(&self, global: &GlobalScope) -> Option<usize> {
        let mut count = 0;
        for field in &self.fields {
            if field.optional {
                return None;
            }
            count += field.rust_type.expanded_field_count(global)?;
        }
        Some(count)
    }

    fn definite_info(&self, global: &GlobalScope) -> Option<String> {
        match self.fixed_field_count(global) {
            Some(count) => Some(count.to_string()),
            None => {
                let mut fixed_field_count = 0;
                let mut conditional_field_expr = String::new();
                for field in &self.fields {
                    if field.optional {
                        if !conditional_field_expr.is_empty() {
                            conditional_field_expr.push_str(" + ");
                        }
                        conditional_field_expr.push_str(&format!("match &self.{} {{ Some(x) => {}, None => 0 }}", field.name, field.rust_type.definite_info("x", global)?));
                    } else {
                        match field.rust_type.expanded_field_count(global) {
                            Some(field_expanded_count) => fixed_field_count += field_expanded_count,
                            None => {
                                if !conditional_field_expr.is_empty() {
                                    conditional_field_expr.push_str(" + ");
                                }
                                let field_len_expr = field.rust_type.definite_info(&format!("self.{}", field.name), global)?;
                                conditional_field_expr.push_str(&field_len_expr);
                            },
                        };
                    }
                }
                if conditional_field_expr.is_empty() || fixed_field_count != 0 {
                    Some(format!("{} + {}", fixed_field_count.to_string(), conditional_field_expr))
                } else {
                    Some(conditional_field_expr)
                }
            }
        }
    }
}

struct GlobalScope {
    global_scope: codegen::Scope,
    serialize_scope: codegen::Scope,
    already_generated: BTreeSet<RustIdent>,
    // Some(group) = directly defined in .cddl
    // None = indirectly generated due to a group choice
    plain_groups: BTreeMap<RustIdent, Option<Group>>,
    type_aliases: BTreeMap::<AliasIdent, RustType>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
    rust_structs: BTreeMap<RustIdent, RustStruct>,
}

impl GlobalScope {
    fn new() -> Self {
        Self {
            global_scope: codegen::Scope::new(),
            serialize_scope: codegen::Scope::new(),
            already_generated: BTreeSet::new(),
            plain_groups: BTreeMap::new(),
            type_aliases: Self::aliases(),
            no_deser_reasons: BTreeMap::new(),
            rust_structs: BTreeMap::new(),
        }
    }

    fn aliases() -> BTreeMap<idents::AliasIdent, RustType> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, RustType>::new();
        // TODO: use u64/i64 later when you figure out the BigInt issues from wasm
        aliases.insert(AliasIdent::new(CDDLIdent::new("uint")), RustType::Primitive(Primitive::U32));
        // Not sure on this one, I think they can be bigger than i64 can fit
        // but the cbor_event serialization takes the argument as an i64
        aliases.insert(AliasIdent::new(CDDLIdent::new("nint")), RustType::Primitive(Primitive::I32));
        aliases.insert(AliasIdent::new(CDDLIdent::new("bool")), RustType::Primitive(Primitive::Bool));
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type = RustType::Primitive(Primitive::Str);
        aliases.insert(AliasIdent::new(CDDLIdent::new("tstr")), string_type.clone());
        aliases.insert(AliasIdent::new(CDDLIdent::new("text")), string_type);
        // TODO: Is this right to have it be Vec<u8>?
        // the serialization library for bytes takes type [u8]
        // so we'll have to put some logic in there I guess?
        // it might be necessary to put a wrapper type..
        aliases.insert(AliasIdent::new(CDDLIdent::new("bstr")), RustType::Primitive(Primitive::Bytes));
        aliases.insert(AliasIdent::new(CDDLIdent::new("bytes")), RustType::Primitive(Primitive::Bytes));
        let null_type = RustType::Fixed(FixedValue::Null);
        aliases.insert(AliasIdent::new(CDDLIdent::new("null")), null_type.clone());
        aliases.insert(AliasIdent::new(CDDLIdent::new("nil")), null_type);
        aliases.insert(AliasIdent::new(CDDLIdent::new("true")), RustType::Fixed(FixedValue::Bool(true)));
        aliases.insert(AliasIdent::new(CDDLIdent::new("false")), RustType::Fixed(FixedValue::Bool(false)));
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    fn new_type(&self, raw: &CDDLIdent) -> RustType {
        let alias_ident = AliasIdent::new(raw.clone());
        let resolved = self.apply_type_aliases(&alias_ident);
        // if we're not literally bytes/bstr, and instead an alias for it
        // we would have generated a named wrapper object so we should
        // refer to that instead
        if !is_identifier_reserved(&raw.to_string()) {
            if let RustType::Primitive(Primitive::Bytes) = &resolved {
                return RustType::Rust(RustIdent::new(raw.clone()));
            }
        }
        // this would interfere with map/array loop code generation unless we
        // specially handle this case since you wouldn't know whether you hit a break
        // or are reading a key here, unless we check, but then you'd need to store the
        // non-break special value once read
        if let RustType::Array(ty) = &resolved {
            assert!(!ty.cbor_types().contains(&CBORType::Special));
        }
        if let RustType::Map(key_type, _val_type) = &resolved {
            assert!(!key_type.cbor_types().contains(&CBORType::Special));
        }
        resolved
    }

    fn apply_type_aliases(&self, alias_ident: &AliasIdent) -> RustType {
        // Assumes we are not trying to pass in any kind of compound type (arrays, etc)
        match self.type_aliases.get(alias_ident) {
            Some(alias) => match alias {
                RustType::Rust(id) => self.apply_type_aliases(&AliasIdent::from(id.clone())),
                x => x.clone(),
            },
            None => match alias_ident {
                AliasIdent::Rust(rust_ident) => RustType::Rust(rust_ident.clone()),
                AliasIdent::Reserved(reserved) => panic!("Reserved ident {} didn't define type alias", reserved),
            },
        }
    }

    fn generate_type_alias(&mut self, alias: RustIdent, base_type: RustType) {
        self.global_scope.raw(format!("type {} = {};", alias, base_type.for_member()).as_ref());
        self.apply_type_alias_without_codegen(alias, base_type);
    }

    fn apply_type_alias_without_codegen(&mut self, alias: RustIdent, base_type: RustType) {
        self.type_aliases.insert(alias.into(), base_type);
    }

    fn scope(&mut self) -> &mut codegen::Scope {
        &mut self.global_scope
    }

    fn serialize_scope(&mut self) -> &mut codegen::Scope {
        &mut self.serialize_scope
    }

    fn register_rust_struct(&mut self, rust_struct: RustStruct) {
        self.rust_structs.insert(rust_struct.ident().clone(), rust_struct);
    }

    fn mark_plain_group(&mut self, name: RustIdent, group: Option<Group>) {
        self.plain_groups.insert(name, group);
    }


    fn is_plain_group(&self, name: &RustIdent) -> bool{
        self.plain_groups.contains_key(name)
    }

    fn plain_group_fields(&self, name: &RustIdent) -> Option<Vec<GroupEntry>> {
        match self.plain_groups.get(name) {
            Some(plain_group) => match plain_group {
                // Defined in .cddl directly so we have a cddl::Group associated to it
                Some(cddl_group) => {
                    assert_eq!(cddl_group.group_choices.len(), 1, "can only get fields of plain group without choices");
                    Some(cddl_group.group_choices.first().unwrap().group_entries.iter().map(|(e, _)| e.clone()).collect())
                },
                // A plain group indirectly generated from a group choice.
                // This None is not a problem right now as we only call this function in order to get the fields
                // in the case of when a group choice has a single plain group entry, in order to avoid
                // generating an unneccessary wrapper indirect plain group.
                // If in the future we need to find out the fields of this too, we would have to store
                // that information about indirectly generated groups as well.
                None => None,
            },
            None => None,
        }
    }

    // If it is a plain group, enerates a wrapper group if one wasn't before
    fn generate_if_plain_group(&mut self, name: RustIdent, rep: Representation) {
        // to get around borrow checker borrowing self mutably + immutably
        if let Some(plain_group) = self.plain_groups.get(&name).map(|g| (*g).clone()) {
            if let Some(cddl_group) = plain_group {
                if self.already_generated.insert(name.clone()) {
                    // TODO: implement ability to have both an array and a map representation
                    //       if someone ever needs that
                    codegen_group(self, &cddl_group, &name, rep, None);
                }
            }
        }
    }

    // TODO: Also generates individual choices if required, ie for a / [foo] / c would generate Foos
    fn create_variants_from_type_choices(&mut self, type_choices: &Vec<Type1>) -> Vec<EnumVariant> {
        let mut variant_names_used = BTreeMap::<String, u32>::new();
        type_choices.iter().map(|type1| {
            let rust_type = rust_type_from_type2(self, &type1.type2);
            let variant_name = append_number_if_duplicate(&mut variant_names_used, rust_type.for_variant().to_string());
            EnumVariant::new(VariantIdent::new_custom(variant_name), rust_type, false)
        }).collect()
    }

    fn generate_type_choices_from_variants(&mut self, name: &RustIdent, variants: &Vec<EnumVariant>, tag: Option<usize>) {
        // Handle group with choices by generating an enum then generating a group for every choice
        let enum_name = RustIdent::new(CDDLIdent::new(format!("{}Enum", name)));
        generate_enum(self, &enum_name, &variants, None, true);

        // Now generate a wrapper object that we will expose to wasm around this
        let (mut s, mut s_impl) = create_exposed_group(self, name);
        let (mut ser_func, mut ser_impl) = create_serialize_impl(name, None, tag, None);
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
                // can't use reserved keywords as param: eg new_u32(u32: u32)
                let arg_name = match variant_arg.as_str() {
                    "u8" | "u16" | "u32" | "u64" => "uint",
                    "i8" | "i16" | "i32" | "i64" => "int",
                    x => x,
                };
                new_func
                    .arg(&arg_name, &variant.rust_type.for_wasm())
                    .line(format!("Self({}::{}({}))", enum_name, variant.name, arg_name));
            }
            s_impl.push_fn(new_func);
        }
        // serialize
        ser_func.line("self.0.serialize(serializer)");
        ser_impl.push_fn(ser_func);
        push_exposed_struct(self, s, s_impl, ser_impl, None);
        // deserialize
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize");
        deser_func.line(format!("Ok(Self({}::deserialize(raw)?))", enum_name));
        deser_impl.push_fn(deser_func);
        self.serialize_scope().push_impl(deser_impl);
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(&mut self, element_type: RustType, array_type_name: &str) -> RustType {
        if element_type.directly_wasm_exposable() {
            return RustType::Array(Box::new(element_type));
        }
        if let RustType::Rust(name) = &element_type {
            self.generate_if_plain_group(name.clone(), Representation::Array);
        }
        let element_type_wasm = element_type.for_wasm();
        let element_type_rust = element_type.for_member();
        let array_type = RustIdent::new(CDDLIdent::new(array_type_name));
        if self.already_generated.insert(array_type.clone()) {
            let mut s = codegen::Struct::new(&array_type.to_string());
            s
                .tuple_field(format!("Vec<{}>", element_type_rust))
                .vis("pub");
            add_struct_derives(&mut s);
            // TODO: accessors (mostly only necessary if we support deserialization)
            self.global_scope.raw("#[wasm_bindgen]");
            self.global_scope.push_struct(s);
            // serialize
            let mut ser_impl = codegen::Impl::new(&array_type.to_string());
            ser_impl.impl_trait("cbor_event::se::Serialize");
            let mut ser_func = make_serialization_function("serialize");
            ser_func.line("serializer.write_array(cbor_event::Len::Len(self.0.len() as u64))?;");
            let mut loop_block = Block::new("for element in &self.0");
            loop_block.line("element.serialize(serializer)?;");
            ser_func.push_block(loop_block);
            ser_func.line("Ok(serializer)");
            ser_impl.push_fn(ser_func);
            self.serialize_scope.push_impl(ser_impl);
            // deserialize
            if self.deserialize_generated_for_type(&element_type) {
                let mut deser_impl = codegen::Impl::new(&array_type.to_string());
                deser_impl.impl_trait("Deserialize");
                let mut deser_func = make_deserialization_function("deserialize");
                deser_func.line("let mut arr = Vec::new();");
                let mut error_annotator = make_err_annotate_block(&array_type.to_string(), "", "?;");
                let deser_body: &mut dyn CodeBlock = if ANNOTATE_FIELDS {
                    &mut error_annotator
                } else {
                    &mut deser_func
                };
                deser_body.line("let len = raw.array()?;");
                let mut deser_loop = make_deser_loop("len", "arr.len()");
                deser_loop.push_block(make_deser_loop_break_check());
                self.generate_deserialize(&element_type, array_type_name, "arr.push(", ");", false, &mut deser_loop);
                deser_body.push_block(deser_loop);
                if ANNOTATE_FIELDS {
                    error_annotator.line("Ok(())");
                    deser_func.push_block(error_annotator);
                }
                deser_func.line("Ok(Self(arr))");
                deser_impl.push_fn(deser_func);
                self.serialize_scope().push_impl(deser_impl);
            } else {
                self.dont_generate_deserialize(&array_type, format!("inner type {} doesn't support deserialize", element_type.for_member()));
            }
            // other functions
            let mut array_impl = codegen::Impl::new(&array_type.to_string());
            array_impl
                .new_fn("new")
                .vis("pub")
                .ret("Self")
                .line("Self(Vec::new())");
            array_impl
                .new_fn("len")
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
            self.register_rust_struct(RustStruct::new_array(array_type, element_type.clone()));
        }
        RustType::Array(Box::new(element_type))
    }

    // Generates MapTToV for a { t => v } table-type map
    fn generate_table_type(&mut self, key_type: RustType, value_type: RustType) -> RustType {
        // can't have plain groups unlike arrays, so don't try and generate those
        // for general map types we can though but not for tables
        let table_type = RustIdent::new(CDDLIdent::new(format!("Map{}To{}", key_type.for_member(), value_type.for_member())));
        if self.already_generated.insert(table_type.clone()) {
            codegen_table_type(self, &table_type, key_type.clone(), value_type.clone(), None);
        }
        RustType::Map(Box::new(key_type), Box::new(value_type))
    }

    // is_end means the final line should evaluate to Ok(serializer), or equivalent ie dropping last ?; from line
    fn generate_serialize(&mut self, rust_type: &RustType, expr: &str, body: &mut dyn CodeBlock, is_end: bool) {
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
                _ => {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                },
            },
            RustType::Rust(t) => {
                if self.plain_groups.contains_key(t) {
                    body.line(&format!("{}.serialize_as_embedded_group(serializer){}", expr, line_ender));
                } else {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                }
            },
            RustType::Array(ty) => {
                if ty.directly_wasm_exposable() {
                    body.line(&format!("serializer.write_array(cbor_event::Len::Len({}.len() as u64))?;", expr));
                    let mut loop_block = Block::new(&format!("for element in {}.iter()", expr));
                    loop_block.line("element.serialize(serializer)?;");
                    body.push_block(loop_block);
                    if is_end {
                        body.line("Ok(serializer)");
                    }
                } else {
                    body.line(&format!("{}.serialize(serializer){}", expr, line_ender));
                }
            },
            RustType::Tagged(tag, ty) => {
                body.line(&format!("serializer.write_tag({}u64)?;", tag));
                self.generate_serialize(ty, expr, body, is_end);
            },
            RustType::Optional(ty) => {
                let mut opt_block = Block::new(&format!("match &{}", expr));
                // TODO: do this in one line without a block if possible somehow.
                //       see other comment in generate_enum()
                let mut some_block = Block::new("Some(x) =>");
                self.generate_serialize(ty, "x", &mut some_block, true);
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
            }
        };
    }

    // formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
    // * {let x = }{<value>}{;} - creation of variables
    // * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
    // * {}{<value>}{} - for last-expression eval in blocks
    // * etc
    // var_name is passed in for use in creating unique identifiers for temporaries
    // if force_non_embedded always deserialize as the outer wrapper, not as the embedded plain group when the Rust ident is for a plain group
    fn generate_deserialize(&mut self, rust_type: &RustType, var_name: &str, before: &str, after: &str, force_non_embedded: bool, body: &mut dyn CodeBlock) {
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        match rust_type {
            RustType::Fixed(f) => {
                // we don't evaluate to any values here, just verify
                // before/after are ignored and we need to handle fixed value deserialization in a different way
                // than normal ones.
                assert_eq!(after, "");
                assert_eq!(before, "");
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
            RustType::Primitive(p) => match p {
                Primitive::Bytes => {
                    body.line(&format!("{}raw.bytes()?{}", before, after));
                },
                _ => {
                    body.line(&format!("{}{}::deserialize(raw)?{}", before, p.to_string(), after));
                },
            },
            RustType::Rust(ident) => if self.is_plain_group(ident) {
                body.line(&format!("{}{}::deserialize_as_embedded_group(raw, len)?{}", before, ident, after));
            } else {
                body.line(&format!("{}{}::deserialize(raw)?{}", before, ident, after));
            },
            RustType::Tagged(tag, ty) => {
                let mut tag_check = Block::new(&format!("{}match raw.tag()?", before));
                let mut deser_block = Block::new(&format!("{} =>", tag));
                self.generate_deserialize(ty, var_name, "", "", force_non_embedded, &mut deser_block);
                deser_block.after(",");
                tag_check.push_block(deser_block);
                tag_check.line(&format!("tag => return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),", tag));
                tag_check.after(after);
                body.push_block(tag_check);
            },
            RustType::Optional(ty) => {
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
                self.generate_deserialize(ty, var_name, "Some(", ")", force_non_embedded, &mut some_block);
                some_block.after(",");
                deser_block.push_block(some_block);
                let mut none_block = Block::new("false =>");
                self.generate_deserialize(&RustType::Fixed(FixedValue::Null), var_name, "", "", force_non_embedded, &mut none_block);
                none_block.line("None");
                deser_block.after(after);
                deser_block.push_block(none_block);
                body.push_block(deser_block);
            },
            RustType::Array(ty) => {
                if ty.directly_wasm_exposable() {
                    body.line("let mut arr = Vec::new();");
                    body.line("let len = raw.array()?;");
                    let mut deser_loop = make_deser_loop("len", "arr.len()");
                    deser_loop.push_block(make_deser_loop_break_check());
                    self.generate_deserialize(ty, var_name, "arr.push(", ");", force_non_embedded, &mut deser_loop);
                    body.push_block(deser_loop);
                    body.line(&format!("{}arr{}", before, after));
                } else {
                    body.line(&format!("{}{}::deserialize(raw)?{}", before, rust_type.for_member(), after));
                }
            },
            RustType::Map(_key_type, _value_type) => {
                // I don't think this will ever be used since we always generate a wrappre type for this
                // so that we can expose it to wasm. It could be later if someone needs that later (for a non-wasm feature maybe)
                //     body.line(&format!("let mut table = {}::new();", rust_type.for_member()));
                //     body.line("let len = raw.map()?;");
                //     let deser_loop = make_table_deser_loop(self, key_type, value_type, "len", "table");
                //     body.push_block(deser_loop);
                //     body.line(&format!("{}table{}", before, after));
                // so instead we just generate this:
                body.line(&format!("{}{}::deserialize(raw)?{}", before, rust_type.for_member(), after));
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
        }
    }

    fn dont_generate_deserialize(&mut self, name: &RustIdent, reason: String) {
        self.no_deser_reasons.entry(name.clone()).or_default().push(reason);
    }

    fn print_structs_without_deserialize(&self) {
        for (name, reasons) in &self.no_deser_reasons {
            println!("Not generating {}::deserialize() - reasons:", name);
            for reason in reasons {
                println!("\t{}", reason);
            }
        }
    }

    fn reset_except_not_deserialized_and_plain_groups(&mut self, last_time: &mut Option<usize>) -> bool {
        let repeat = match last_time {
            Some(n) => *n != self.no_deser_reasons.len(),
            None => true,
        };
        if repeat {
            self.global_scope = codegen::Scope::new();
            self.serialize_scope = codegen::Scope::new();
            self.already_generated.clear();
            self.type_aliases = Self::aliases();
            // keep empty ones so we can know it's not generated
            for (_ident, reasons) in self.no_deser_reasons.iter_mut() {
                reasons.clear();
            }
        }
        *last_time = Some(self.no_deser_reasons.len());
        repeat
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
        .derive("Debug")
        .derive("Eq")
        .derive("Ord")
        .derive("PartialEq")
        .derive("PartialOrd");
}

fn is_identifier_reserved(name: &str) -> bool {
    match name {
        // These are all possible reserved identifiers, even if we don't support them
        "uint"       |
        "int"        |
        "nint"       |
        "text"       |
        "tstr"       |
        "bytes"      |
        "bstr"       |
        "bool"       |
        "float"      |
        "float16"    |
        "float32"    |
        "float64"    |
        "float16-32" |
        "float32-64" |
        "tdate"      |
        "time"       |
        "number"     |
        "biguint"    |
        "bignint"    |
        "bigint"     | 
        "integer"    |
        "unsigned"   |
        "decfrac"    |
        "bigfloat"   |
        "eb64url"    |
        "eb64legacy" |
        "eb16"       |
        "encoded-cbor" |
        "uri"        |
        "b64url"     |
        "b64legacy"  |
        "regexp"     |
        "mime-messag e" |
        "cbor-any"   |
        "null"       |
        "nil"        |
        "undefined"  |
        "true"       |
        "false" => true,
        _ => false,
    }
}

fn append_number_if_duplicate(used_names: &mut BTreeMap<String, u32>, name: String) -> String {
    let entry = used_names.entry(name.clone()).or_default();
    *entry += 1;
    if *entry > 1 {
        format!("{}{}", name, *entry)
    } else {
        name
    }
}

// would use rust_type_from_type2 but that requires GlobalScope which we shouldn't
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
        GroupEntry::TypeGroupname{ ge: TypeGroupnameEntry { name, .. }, .. } => match is_identifier_reserved(&name.to_string()) {
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
        GroupEntry::TypeGroupname{ ge: TypeGroupnameEntry { name, .. }, .. } => match is_identifier_reserved(&name.to_string()) {
            true => None,
            false => Some(name.to_string()),
        },
        GroupEntry::InlineGroup{ group, .. } => panic!("not implemented (define a new struct for this!) = {}\n\n {:?}", group, group),
    }
}

// Returns None if this is a fixed value that we should not be storing
fn rust_type_from_type2(global: &mut GlobalScope, type2: &Type2) -> RustType {
    // generic args not in shelley.cddl
    // TODO: socket plugs (used in hash type)
    match type2 {
        Type2::UintValue{ value, .. } => RustType::Fixed(FixedValue::Uint(*value)),
        Type2::IntValue{ value, .. } => RustType::Fixed(FixedValue::Int(*value)),
        //Type2::FloatValue{ value, .. } => RustType::Fixed(FixedValue::Float(*value)),
        Type2::TextValue{ value, .. } => RustType::Fixed(FixedValue::Text(value.clone())),
        Type2::Typename{ ident, .. } => global.new_type(&CDDLIdent::new(&ident.ident)),
        Type2::Array{ group, .. } => {
            // TODO: support for group choices in arrays?
            let element_type = match group.group_choices.len() {
                1 => {
                    let choice = &group.group_choices.first().unwrap();
                    // special case for homogenous arrays
                    if choice.group_entries.len() == 1 {
                        let (entry, _has_comma) = choice.group_entries.first().unwrap();
                        match entry {
                            GroupEntry::ValueMemberKey{ ge, .. } => rust_type(global, &ge.entry_type),
                            GroupEntry::TypeGroupname{ ge, .. } => global.new_type(&CDDLIdent::new(&ge.name.to_string())),
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
            global.generate_array_type(element_type, &array_wrapper_name)
        },
        Type2::Map { group, .. } => {
            match group.group_choices.len() {
                1 => {
                    let group_choice = group.group_choices.first().unwrap();
                    let table_types = table_domain_range(group_choice, Representation::Map);
                    match table_types {
                        // Table map - homogenous key/value types
                        Some((domain, range)) => {
                            let key_type = rust_type_from_type2(global, domain);
                            let value_type = rust_type(global, range);
                            global.generate_table_type(key_type, value_type)
                        },
                        None => unimplemented!("TODO: non-table types as types: {:?}", group),
                    }
                },
                _ => unimplemented!("group choices in inlined map types not allowed: {:?}", group),
            }
        },
        // unsure if we need to handle the None case - when does this happen?
        Type2::TaggedData{ tag, t, .. } => {
            RustType::Tagged(tag.expect("tagged data without tag not supported"), Box::new(rust_type(global, t)))
        },
        _ => {
            panic!("Ignoring Type2: {:?}", type2);
        },
    }
}

fn rust_type(global: &mut GlobalScope, t: &Type) -> RustType {
    if t.type_choices.len() == 1 {
        rust_type_from_type2(global, &t.type_choices.first().unwrap().type2)
    } else {
        if t.type_choices.len() == 2 {
            // T / null   or   null / T   should map to Option<T>
            let a = &t.type_choices[0].type2;
            let b = &t.type_choices[1].type2;
            if type2_is_null(a) {
                return RustType::Optional(Box::new(rust_type_from_type2(global, b)));
            }
            if type2_is_null(b) {
                return RustType::Optional(Box::new(rust_type_from_type2(global, a)));
            }
        }
        let variants = global.create_variants_from_type_choices(&t.type_choices);
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
        global.generate_type_choices_from_variants(&combined_ident, &variants, None);
        global.new_type(&CDDLIdent::new(combined_name))
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

fn group_entry_to_type(global: &mut GlobalScope, entry: &GroupEntry) -> RustType {
    let ret = match entry {
        GroupEntry::ValueMemberKey{ ge, .. } => rust_type(global, &ge.entry_type),
        GroupEntry::TypeGroupname{ ge, .. } => global.new_type(&CDDLIdent::new(ge.name.to_string())),
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

fn create_exposed_group(global: &GlobalScope, ident: &RustIdent) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(&mut s);
    let mut group_impl = codegen::Impl::new(name);
    // There are auto-implementing ToBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    group_impl
        .new_fn("to_bytes")
        .ret("Vec<u8>")
        .arg_ref_self()
        .vis("pub")
        .line("ToBytes::to_bytes(self)");
    if global.deserialize_generated(ident) {
        group_impl
            .new_fn("from_bytes")
            .ret(format!("Result<{}, JsValue>", name))
            .arg("data", "Vec<u8>")
            .vis("pub")
            .line("FromBytes::from_bytes(data)");
    }
    (s, group_impl)
}

// Alway creates directly just Serialize impl. Shortcut for create_serialize_impls when
// we know we won't need the SerializeEmbeddedGroup impl.
// See comments for create_serialize_impls for usage.
fn create_serialize_impl(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, definite_len: Option<String>) -> (codegen::Function, codegen::Impl) {
    match create_serialize_impls(ident, rep, tag, definite_len, false) {
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
fn create_serialize_impls(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>, definite_len: Option<String>, generate_serialize_embedded: bool) -> (codegen::Function, codegen::Impl, Option<codegen::Impl>) {
    if generate_serialize_embedded {
        // This is not necessarily a problem but we should investigate this case to ensure we're not calling
        // (de)serialize_as_embedded without (de)serializing the tag
        assert_eq!(tag, None);
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
        let cbor_len_str = match &definite_len {
            Some(fixed_field_count) => format!("cbor_event::Len::Len({})", fixed_field_count),
            None => String::from("cbor_event::Len::Indefinite"),
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

// The Deserialize impl is complete and calls the DeserializeEmbeddedGroup impl,
// but the DeserializeEmbeddedGroup one is empty and must be implemented yourself
// by pushing a deserialize_as_embedded_group function to it.
// TODO: maybe refactor this like create_alL_serialize_impls() so that it only generates
//       DeserializeEmbeddedGroup when needed? (e.g. for plain groups)
//       It works fine as-is but produces bloated code
fn create_deserialize_impls(ident: &RustIdent, rep: Option<Representation>, tag: Option<usize>) -> (codegen::Impl, codegen::Impl) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Cusom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    let mut deser_func = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(name, "", "");
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
    // TODO: enforce finite element lengths
    if let Some (rep) = rep {
        match rep {
            Representation::Array => {
                deser_body.line("let len = raw.array()?;");
                deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, len);");
            },
            Representation::Map => {
                deser_body.line("let len = raw.map()?;");
                deser_body.line("Self::deserialize_as_embedded_group(raw, len)");
            },
        };
    } else {
        panic!("TODO: how should we handle this considering we are dealing with Len?");
        //deser_body.line("Self::deserialize_as_embedded_group(serializer)");
    }
    // We only check it for arrays since the implementation for maps uses len to decide
    // when to stop reading values, since otherwise with optional parameters it doesn't know.
    // We also can't do it from within deserialize_as_embedded_group() as that interferes with
    // plain groups nested inside other array groups
    if let Some(Representation::Array) = rep {
        // TODO: check finite len
        let mut end_len_check = Block::new("match len");
        end_len_check.line("cbor_event::Len::Len(_) => /* TODO: check finite len somewhere */(),");
        let mut indefinite_check = Block::new("cbor_event::Len::Indefinite => match raw.special()?");
        indefinite_check.line("CBORSpecial::Break => /* it's ok */(),");
        indefinite_check.line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
        indefinite_check.after(",");
        end_len_check.push_block(indefinite_check);
        deser_body.push_block(end_len_check);
        deser_body.line("ret");
    }
    if ANNOTATE_FIELDS {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    let mut deser_embedded_impl = codegen::Impl::new(name);
    deser_embedded_impl.impl_trait("DeserializeEmbeddedGroup");
    (deser_impl, deser_embedded_impl)
}

fn push_exposed_struct(
    global: &mut GlobalScope,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    ser_impl: codegen::Impl,
    ser_embedded_impl: Option<codegen::Impl>,
) {
    global.scope()
        .raw("#[wasm_bindgen]")
        .push_struct(s)
        .raw("#[wasm_bindgen]")
        .push_impl(s_impl);
    global.serialize_scope()
        .push_impl(ser_impl);
    if let Some(s) = ser_embedded_impl {
        global.serialize_scope().push_impl(s);
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

fn make_table_deser_loop(global: &mut GlobalScope, key_type: &RustType, value_type: &RustType, len_var: &str, table_var: &str) -> Block {
    let mut deser_loop = make_deser_loop(len_var, &format!("{}.len()", table_var));
    deser_loop.push_block(make_deser_loop_break_check());
    global.generate_deserialize(key_type, "key", "let key = ", ";", false, &mut deser_loop);
    global.generate_deserialize(value_type, "value", "let value = ", ";", false, &mut deser_loop);
    let mut dup_check = Block::new(&format!("if {}.insert(key.clone(), value).is_some()", table_var));
    let dup_key_error_key = match key_type {
        RustType::Primitive(Primitive::U32) => "Key::Uint(key.into())",
        RustType::Primitive(Primitive::Str) => "Key::Str(key)",
        // TODO: make a generic one then store serialized CBOR?
        _ => "Key::Str(String::from(\"some complicated/unsupported type\"))",
    };
    dup_check.line(format!("return Err(DeserializeFailure::DuplicateKey({}).into());", dup_key_error_key));
    deser_loop.push_block(dup_check);
    deser_loop
}

fn codegen_table_type(global: &mut GlobalScope, name: &RustIdent, key_type: RustType, value_type: RustType, tag: Option<usize>) {
    // this would interfere with loop code generation unless we
    // specially handle this case since you wouldn't know whether you hit a break
    // or are reading a key here, unless we check, but then you'd need to store the
    // non-break special value once read
    assert!(!key_type.cbor_types().contains(&CBORType::Special));
    let (mut s, mut s_impl) = create_exposed_group(global, name);
    let (mut ser_func, mut ser_impl) = create_serialize_impl(name, Some(Representation::Map), tag, Some(String::from("self.0.len() as u64")));
    s.vis("pub");
    s.tuple_field(format!("std::collections::BTreeMap<{}, {}>", key_type.for_member(), value_type.for_member()));
    // new
    s_impl
        .new_fn("new")
        .vis("pub")
        .ret("Self")
        .line("Self(std::collections::BTreeMap::new())");
    // len
    s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    // insert
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm())
        .arg("value", value_type.for_wasm())
        .ret(format!("Option<{}>", value_type.for_wasm()))
        .line(
            format!(
                "self.0.insert({}, {})",
                key_type.from_wasm_boundary("key"),
                value_type.from_wasm_boundary("value")));
    s_impl.push_fn(insert_func);
    // serialize
    let mut ser_loop = Block::new("for (key, value) in &self.0");
    global.generate_serialize(&key_type, "key", &mut ser_loop, false);
    global.generate_serialize(&value_type, "value", &mut ser_loop, false);
    ser_func.push_block(ser_loop);
    ser_func.line("Ok(serializer)");
    ser_impl.push_fn(ser_func);
    push_exposed_struct(global, s, s_impl, ser_impl, None);
    // deserialize
    if !global.deserialize_generated_for_type(&key_type) {
        global.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_member()));
    } else if !global.deserialize_generated_for_type(&value_type) {
        global.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_member()));
    } else {
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize");
        deser_func.line("let mut table = std::collections::BTreeMap::new();");
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
        let deser_loop = make_table_deser_loop(global, &key_type, &value_type, "len", "table");
        deser_body.push_block(deser_loop);
        if ANNOTATE_FIELDS {
            error_annotator.line("Ok(())");
            deser_func.push_block(error_annotator);
        }
        deser_func.line("Ok(Self(table))");
        deser_impl.push_fn(deser_func);
        global.serialize_scope().push_impl(deser_impl);
    }
}

fn codegen_struct(global: &mut GlobalScope, name: &RustIdent, tag: Option<usize>, rust_struct: &RustRecord) {
    let (mut s, mut s_impl) = create_exposed_group(global, name);
    let (mut ser_func, mut ser_impl, mut ser_embedded_impl) =
        create_serialize_impls(
            name,
            Some(rust_struct.rep),
            tag,
            rust_struct.definite_info(global),
            global.is_plain_group(name));
    let (deser_impl, mut deser_embedded_impl) = create_deserialize_impls(name, Some(rust_struct.rep), tag);
    s.vis("pub");

    // Generate struct + fields + constructor
    let mut new_func = codegen::Function::new("new");
    new_func
        .ret("Self")
        .vis("pub");
    let mut new_func_block = Block::new("Self");
    for field in &rust_struct.fields {
        if !global.deserialize_generated_for_type(&field.rust_type) {
            global.dont_generate_deserialize(name, format!("field {}: {} couldn't generate serialize", field.name, field.rust_type.for_member()));
        }
        // Unsupported types so far are fixed values, only have fields for these.
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
                    .arg(&field.name, &field.rust_type.for_wasm())
                    .vis("pub")
                    .line(format!("self.{} = Some({})", field.name, field.name));
                s_impl.push_fn(setter);
            } else {
                // field
                s.field(&field.name, field.rust_type.for_member());
                // new
                new_func.arg(&field.name, field.rust_type.for_wasm());
                new_func_block.line(format!("{}: {},", field.name, field.rust_type.from_wasm_boundary(&field.name)));
                // do we want setters here later for mandatory types covered by new?
            }
        }
    }
    new_func.push_block(new_func_block);
    s_impl.push_fn(new_func);

    // Generate serialization
    let mut ser_func = match ser_embedded_impl {
        Some(_) => {
            ser_impl.push_fn(ser_func);
            make_serialization_function("serialize_as_embedded_group")
        },
        None => ser_func,
    };
    let mut deser_func = make_deserialization_function("deserialize_as_embedded_group");
    deser_func.arg("len", "cbor_event::Len");
    match rust_struct.rep {
        Representation::Array => {
            let mut deser_ret = format!("Ok({}::new(", name);
            let mut first_param = true;
            for field in &rust_struct.fields {
                if field.optional {
                    let mut optional_array_ser_block = Block::new(&format!("if let Some(field) = &self.{}", field.name));
                    global.generate_serialize(&field.rust_type, "field", &mut optional_array_ser_block, false);
                    ser_func.push_block(optional_array_ser_block);
                    global.dont_generate_deserialize(name, format!("Array with optional field {}: {}", field.name, field.rust_type.for_member()));
                } else {
                    global.generate_serialize(&field.rust_type, &format!("self.{}", field.name), &mut ser_func, false);
                    if field.rust_type.is_fixed_value() {
                        // don't set anything, only verify data
                        if ANNOTATE_FIELDS {
                            let mut err_deser = make_err_annotate_block(&field.name, "", "?;");
                            global.generate_deserialize(&field.rust_type, &field.name, "", "", false, &mut err_deser);
                            // this block needs to evaluate to a Result even though it has no value
                            err_deser.line("Ok(())");
                            deser_func.push_block(err_deser);
                        } else {
                            global.generate_deserialize(&field.rust_type, &field.name, "", "", false, &mut deser_func);
                        }
                    } else {
                        if ANNOTATE_FIELDS {
                            let mut err_deser = make_err_annotate_block(&field.name, &format!("let {} = ", field.name), "?;");
                            global.generate_deserialize(&field.rust_type, &field.name, "Ok(", ")", false, &mut err_deser);
                            deser_func.push_block(err_deser);
                        } else {
                            global.generate_deserialize(&field.rust_type, &field.name, &format!("let {} = ", field.name), ";", false, &mut deser_func);
                        }
                    }
                    if !field.rust_type.is_fixed_value() {
                        if first_param {
                            first_param = false;
                        } else {
                            deser_ret.push_str(", ");
                        }
                        deser_ret.push_str(&field.name);
                    }
                }
            }
            // length checked inside of deserialize() - it causes problems for plain groups nested
            // in other groups otherwise
            deser_ret.push_str("))");
            deser_func.line(deser_ret);
        },
        Representation::Map => {
            let mut uint_field_deserializers = Vec::new();
            let mut text_field_deserializers = Vec::new();
            for field in &rust_struct.fields {
                // to support maps with plain groups inside is very difficult as we cannot guarantee
                // the order of fields so foo = {a, b, bar}, bar = (c, d) could have the order ben
                // {a, d, c, b}, {c, a, b, d}, etc which doesn't fit with the nature of deserialize_as_embedded_group
                // A possible solution would be to take all fields into one big map, either in generation to begin with,
                // or just for deserialization then constructing at the end with locals like a, b, bar_c, bar_d.
                if let RustType::Rust(ident) = &field.rust_type {
                    if global.is_plain_group(&ident) {
                        global.dont_generate_deserialize(name, format!("Map with plain group field {}: {}", field.name, field.rust_type.for_member()));
                    }
                }
                if field.rust_type.is_fixed_value() {
                    deser_func.line(&format!("let mut {}_present = false;", field.name));
                } else {
                    deser_func.line(format!("let mut {} = None;", field.name));
                }
                let mut optional_map_ser_block = Block::new(&format!("if let Some(field) = &self.{}", field.name));
                let (data_name, map_ser_block): (String, &mut dyn CodeBlock) = if field.optional {
                    (String::from("field"), &mut optional_map_ser_block)
                } else {
                    (format!("self.{}", field.name), &mut ser_func)
                };

                // serialize key
                let key = field.key.clone().unwrap();
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
                        global.generate_deserialize(&field.rust_type, &field.name, "", "", false, &mut err_deser);
                        err_deser.line("Ok(true)");
                        deser_block.push_block(err_deser);
                    } else {
                        global.generate_deserialize(&field.rust_type, &field.name, "", "", false, &mut deser_block);
                        deser_block.line(&format!("{}_present = true;", field.name));
                    }
                } else {
                    let mut dup_check = Block::new(&format!("if {}.is_some()", field.name));
                    dup_check.line(&format!("return Err(DeserializeFailure::DuplicateKey({}).into());", key_in_rust));
                    deser_block.push_block(dup_check);
                    if ANNOTATE_FIELDS {
                        let mut err_deser = make_err_annotate_block(&field.name, &format!("{} = Some(", field.name), "?);");
                        global.generate_deserialize(&field.rust_type, &field.name, "Ok(", ")", false, &mut err_deser);
                        deser_block.push_block(err_deser);
                    } else {
                        global.generate_deserialize(&field.rust_type, &field.name, &format!("{} = Some(", field.name), ");", false, &mut deser_block);
                    }
                }
                match &key {
                    FixedValue::Uint(x) => {
                        map_ser_block.line(&format!("serializer.write_unsigned_integer({})?;", x));
                        uint_field_deserializers.push(deser_block);
                    },
                    FixedValue::Text(x) => {
                        map_ser_block.line(&format!("serializer.write_text(\"{}\")?;", x));
                        text_field_deserializers.push(deser_block);
                    },
                    _ => panic!("unsupported map key type for {}.{}: {:?}", name, field.name, key),
                };
                // and serialize value
                global.generate_serialize(&field.rust_type, &data_name, map_ser_block, false);
                if field.optional {
                    ser_func.push_block(optional_map_ser_block);
                }
            }
            // needs to be in one line rather than a block because Block::after() only takes a string
            deser_func.line("let mut read = 0;");
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
            // this will need to change if we support Special values as keys (e.g. true / false)
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
            deser_func.push_block(deser_loop);
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
                        deser_func.push_block(mandatory_field_check);
                    } else {
                        let mut mandatory_field_check = Block::new(&format!("let {} = match {}", field.name, field.name));
                        mandatory_field_check.line("Some(x) => x,");
                        
                        mandatory_field_check.line(format!("None => return Err(DeserializeFailure::MandatoryFieldMissing({}).into()),", key));
                        mandatory_field_check.after(";");
                        deser_func.push_block(mandatory_field_check);
                    }
                }
                if !field.rust_type.is_fixed_value() {
                    ctor_block.line(format!("{},", field.name));
                }
            }
            ctor_block.after(")");
            deser_func.push_block(ctor_block);
        },
    };
    ser_func.line("Ok(serializer)");
    match &mut ser_embedded_impl {
        Some(ser_embedded_impl) => ser_embedded_impl.push_fn(ser_func),
        None => ser_impl.push_fn(ser_func),
    };
    deser_embedded_impl.push_fn(deser_func);
    push_exposed_struct(global, s, s_impl, ser_impl, ser_embedded_impl);
    // TODO: generic deserialize (might need backtracking)
    if global.deserialize_generated(name) {
        global.serialize_scope()
            .push_impl(deser_impl)
            .push_impl(deser_embedded_impl);
    }
}

fn codegen_group_choice(global: &mut GlobalScope, group_choice: &GroupChoice, name: &RustIdent, rep: Representation, tag: Option<usize>) {
    let table_types = table_domain_range(group_choice, rep);
    match table_types {
        // Table map - homogenous key/value types
        Some((domain, range)) => {
            let key_type = rust_type_from_type2(global, domain);
            let value_type = rust_type(global, range);
            global.register_rust_struct(RustStruct::new_table(name.clone(), key_type.clone(), value_type.clone()));
            codegen_table_type(global, name, key_type, value_type, tag);
        },
        // Heterogenous map (or array!) with defined key/value pairs in the cddl like a struct
        None => {
            let rust_struct = RustRecord::new_record_from_group_choice(global, name.clone(), rep, group_choice);
            // We need to store this in GlobalScope so we can refer from one struct to another.
            // TODO: split up cddl lib's parsing from codegen so we don't need multiple passes among other reasons
            global.register_rust_struct(RustStruct::Record(rust_struct.clone()));
            codegen_struct(global, name, tag, &rust_struct);
        }
    }
}

fn codegen_group(global: &mut GlobalScope, group: &Group, name: &RustIdent, rep: Representation, tag: Option<usize>) {
    if group.group_choices.len() == 1 {
        // Handle simple (no choices) group.
        codegen_group_choice(global, group.group_choices.first().unwrap(), name, rep, tag);
    } else {
        // Generate Enum object that is not exposed to wasm, since wasm can't expose
        // fully featured rust enums via wasm_bindgen

        // TODO: We don't support generating SerializeEmbeddedGroup for group choices which is necessary for plain groups
        // It would not be as trivial to add as we do the outer group's array/map tag writing inside the variant match
        // to avoid having to always generate SerializeEmbeddedGroup when not necessary.
        assert!(!global.is_plain_group(name));
        
        // Handle group with choices by generating an enum then generating a group for every choice
        let enum_name = RustIdent::new(CDDLIdent::new(format!("{}Enum", name)));
        let mut variants_names_used = BTreeMap::<String, u32>::new();
        let variants: Vec<EnumVariant> = group.group_choices.iter().enumerate().map(|(i, group_choice)| {
            // If we're a 1-element we should just wrap that type in the variant rather than
            // define a new struct just for each variant.
            // TODO: handle map-based enums? It would require being able to extract the key logic
            // We might end up doing this anyway to support table-maps in choices though.
            if group_choice.group_entries.len() == 1 {
                let group_entry = &group_choice.group_entries.first().unwrap().0;
                let ty = group_entry_to_type(global, group_entry);
                let serialize_as_embedded = if let RustType::Rust(ident) = &ty {
                    // we might need to generate it if not used elsewhere
                    global.generate_if_plain_group(ident.clone(), rep);
                    global.is_plain_group(ident)
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
                global.mark_plain_group(variant_name.clone(), None);
                // TODO: Should we generate these within their own namespace?
                codegen_group_choice(global, group_choice, &variant_name, rep, None);
                EnumVariant::new(VariantIdent::new_rust(variant_name.clone()), RustType::Rust(variant_name), true)
            }
        }).collect();
        generate_enum(global, &enum_name, &variants, Some(rep), false);

        // Now generate a wrapper object that we will expose to wasm around this
        let (mut s, mut s_impl) = create_exposed_group(global, name);
        s
            .vis("pub")
            .tuple_field(&enum_name.to_string());
        // new
        for (variant, group_choice) in variants.iter().zip(group.group_choices.iter()) {
            let mut new_func = codegen::Function::new(&format!("new_{}", convert_to_snake_case(&variant.name.to_string())));
            new_func
                .ret("Self")
                .vis("pub");
            let mut output_comma = false;
            let mut generated_fields = BTreeMap::<String, u32>::new();
            let group_entries = match match &variant.name {
                VariantIdent::RustStruct(rust_ident) => global.plain_group_fields(rust_ident),
                VariantIdent::Custom(_) => None,
            } {
                Some(entries) => entries,
                None => group_choice.group_entries.iter().map(|(e, _)| e.clone()).collect(),
            };
            // We only want to generate Variant::new() calls when we created a special struct
            // for the variant, which happens in the general case for multi-field group choices
            if group_choice.group_entries.len() > 1 {
                let mut ctor = format!("Self({}::{}({}::new(", enum_name, variant.name, variant.name);
                for (index, group_entry) in group_entries.iter().enumerate() {
                    if !group_entry_optional(group_entry) {
                        let field_name = group_entry_to_field_name(group_entry, index, &mut generated_fields);
                        // Unsupported types so far are fixed values, only have fields for these.
                        let rust_type = group_entry_to_type(global, group_entry);
                        if !rust_type.is_fixed_value() {
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
            } else {
                let group_entry = &group_choice.group_entries.first().unwrap().0;
                let field_name = group_entry_to_field_name(group_entry, 0, &mut generated_fields);
                let rust_type = group_entry_to_type(global, group_entry);
                if !group_entry_optional(group_entry) && !rust_type.is_fixed_value() {
                    new_func
                        .arg(&field_name, rust_type.for_wasm())
                        .line(format!("Self({}::{}({}))", enum_name, variant.name, field_name));
                } else {
                    new_func.line(format!("Self({}::{})", enum_name, variant.name));
                }
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
        push_exposed_struct(global, s, s_impl, ser_impl, None);
        // deserialize
        let mut deser_impl = codegen::Impl::new(&name.to_string());
        deser_impl.impl_trait("Deserialize");
        let mut deser_func = make_deserialization_function("deserialize");
        deser_func.line(format!("Ok(Self({}::deserialize(raw)?))", enum_name));
        deser_impl.push_fn(deser_func);
        global.serialize_scope().push_impl(deser_impl);
    }
}

// rep is Optional - None means we just serialize raw, ie for type choices
struct EnumVariant {
    name: VariantIdent,
    rust_type: RustType,
    serialize_as_embedded_group: bool,
}

impl EnumVariant {
    pub fn new(name: VariantIdent, rust_type: RustType, serialize_as_embedded_group: bool) -> Self {
        Self {
            name,
            rust_type,
            serialize_as_embedded_group,
        }
    }
}

// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choices which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
fn generate_enum(global: &mut GlobalScope, enum_name: &RustIdent, variants: &Vec<EnumVariant>, rep: Option<Representation>, generate_deserialize_directly: bool) {
    let mut e = codegen::Enum::new(&enum_name.to_string());
    // instead of using create_serialize_impl() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(&mut e);
    let mut ser_impl = codegen::Impl::new(&enum_name.to_string());
    ser_impl.impl_trait("cbor_event::se::Serialize");
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_array_match_block = Block::new("match self");
    let mut deser_func = if generate_deserialize_directly {
        make_deserialization_function("deserialize")
    } else {
        let mut embedded_func = make_deserialization_function("deserialize_as_embedded_group");
        embedded_func.arg("len", "cbor_event::Len");
        embedded_func
    };
    deser_func.line("let initial_position = raw.as_mut_ref().seek(SeekFrom::Current(0)).unwrap();");
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
                        let (len_str, indefinite) = match variant.rust_type.expanded_field_count(global) {
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
                global.generate_serialize(&variant.rust_type, "x", &mut case_block, !write_break);
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
            global.generate_deserialize(&variant.rust_type, &convert_to_snake_case(&variant.name.to_string()), "", "", generate_deserialize_directly, &mut variant_deser);
            variant_deser.line("Ok(())");
        } else {
            global.generate_deserialize(&variant.rust_type, &convert_to_snake_case(&variant.name.to_string()), "Ok(", ")", generate_deserialize_directly, &mut variant_deser);
        }
        variant_deser.after(")(raw)");
        deser_func.push_block(variant_deser);
        // can't chain blocks so we just put them one after the other
        let mut return_if_deserialized = Block::new("");
        if variant.rust_type.is_fixed_value() {
            return_if_deserialized.line(format!("Ok(()) => return Ok({}::{}),", enum_name, variant.name));
        } else {
            return_if_deserialized.line(format!("Ok(variant) => return Ok({}::{}(variant)),", enum_name, variant.name));
        }
        return_if_deserialized.line("Err(_) => raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap(),");
        return_if_deserialized.after(";");
        deser_func.push_block(return_if_deserialized);
    }
    ser_func.push_block(ser_array_match_block);
    ser_impl.push_fn(ser_func);
    deser_func.line(format!("Err(DeserializeError::new(\"{}\", DeserializeFailure::NoVariantMatched.into()))", enum_name));
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    global
        .scope()
        .push_enum(e);
    global
        .serialize_scope()
        .push_impl(ser_impl);
    if generate_deserialize_directly {
        let mut deser_impl = codegen::Impl::new(&enum_name.to_string());
        deser_impl.impl_trait("Deserialize");
        deser_impl.push_fn(deser_func);
        global.serialize_scope().push_impl(deser_impl);
    } else {
        let (deser_impl, mut deser_embedded_impl) = create_deserialize_impls(enum_name, rep, None);
        deser_embedded_impl.push_fn(deser_func);
        global
            .serialize_scope()
            .push_impl(deser_impl)
            .push_impl(deser_embedded_impl);
    }
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

fn make_deserialization_function(name: &str) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f
        .generic("R: BufRead + Seek")
        .ret("Result<Self, DeserializeError>")
        .arg("raw", "&mut Deserializer<R>");
    f
}

fn generate_wrapper_struct(global: &mut GlobalScope, type_name: &RustIdent, field_type: &RustType, tag: Option<usize>) {
    let (mut s, mut s_impl) = create_exposed_group(global, type_name);
    s
        .vis("pub")
        .tuple_field(field_type.for_member());
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_impl = codegen::Impl::new(&type_name.to_string());
    ser_impl.impl_trait("cbor_event::se::Serialize");
    if let Some(tag) = tag {
        ser_func.line(format!("serializer.write_tag({}u64)?;", tag));
    }
    global.generate_serialize(&field_type, "self.0", &mut ser_func, true);
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
    global.generate_deserialize(field_type, "", "Ok(Self(", "))", false, &mut deser_func);
    deser_impl.push_fn(deser_func);
    let mut new_func = codegen::Function::new("new");
    new_func
        .ret("Self")
        .arg("data", field_type.for_wasm())
        .vis("pub");
    new_func.line(format!("Self({})", field_type.from_wasm_boundary("data")));
    s_impl.push_fn(new_func);
    global
        .scope()
        .raw("#[wasm_bindgen]")
        .push_struct(s)
        .raw("#[wasm_bindgen]")
        .push_impl(s_impl);
    global
        .serialize_scope()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
    
}

fn generate_type_choices(global: &mut GlobalScope, name: &RustIdent, type_choices: &Vec<Type1>, tag: Option<usize>) {
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
        let inner_rust_type = rust_type_from_type2(global, inner_type2);
        match tag {
            Some(_) => {
                generate_wrapper_struct(global, name, &RustType::Optional(Box::new(inner_rust_type)), tag);
            },
            None => {
                global.generate_type_alias(name.to_owned(), RustType::Optional(Box::new(inner_rust_type)));
            },
        };
    } else {
        let variants = global.create_variants_from_type_choices(type_choices);
        global.generate_type_choices_from_variants(&name, &variants, tag);
    }
}

fn generate_type(global: &mut GlobalScope, type_name: &RustIdent, type2: &Type2, outer_tag: Option<usize>) {
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
            let cddl_ident = CDDLIdent::new(ident.to_string());
            let generate_binary_wrapper = match ident.to_string().as_ref() {
                "bytes" | "bstr" => true,
                _ident => if let RustType::Primitive(Primitive::Bytes) = global.apply_type_aliases(&AliasIdent::new(cddl_ident.clone())) {
                    true
                } else {
                    false
                },
            };
            if generate_binary_wrapper {
                let field_type = RustType::Primitive(Primitive::Bytes);
                generate_wrapper_struct(global, type_name, &field_type, None);
                global.apply_type_alias_without_codegen(type_name.to_owned(), field_type);
            } else {
                global.generate_type_alias(type_name.to_owned(), global.new_type(&cddl_ident));
            }
        },
        Type2::Map{ group, .. } => {
            codegen_group(global, group, type_name, Representation::Map, outer_tag);
        },
        Type2::Array{ group, .. } => {
            // TODO: We could potentially generate an array-wrapper type around this
            // possibly based on the occurency specifier.
            codegen_group(global, group, type_name, Representation::Array, outer_tag);
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
                        Either::Left(_group) => generate_type(global, type_name, inner_type, *tag),
                        Either::Right(ident) => generate_wrapper_struct(global, type_name, &global.new_type(&CDDLIdent::new(ident.to_string())), *tag),
                    };
                },
                _ => {
                    generate_type_choices(global, type_name, &t.type_choices, *tag);
                }
            };
        },
        x => {
            println!("\nignored typename {} -> {:?}\n", type_name, x);
        },
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cddl_in = std::fs::read_to_string("input.cddl").expect("input.cddl file not present or could not be opened");
    let cddl = cddl::parser::cddl_from_str(&cddl_in)?;
    //println!("CDDL file: {}", cddl);
    let mut global = GlobalScope::new();
    // TODO: this is a quick hack to get around out-of-order declarations in the cddl file
    // e.g. foo = [bar], bar = ... where bar can't deserialize would still generate foo's deserialize
    // if we just do everything independently and it's our of order, so as a super quick fix just do multiple
    // passes until the number of unsupported structs doesn't change.
    // We should probably construct some dependency graph instead of doing this.
    let mut not_deserialized = None;
    let mut pass_count = 0;
    // Need to know beforehand which are plain groups so we can serialize them properly
    // ie x = (3, 4), y = [1, x, 2] would be [1, 3, 4, 2] instead of [1, [3, 4], 2]
    for cddl_rule in &cddl.rules {
        if let Rule::Group{ rule, .. } = cddl_rule {
            // Freely defined group - no need to generate anything outside of group module
            match &rule.entry {
                GroupEntry::InlineGroup{ group, .. } => {
                    global.mark_plain_group(RustIdent::new(CDDLIdent::new(rule.name.to_string())), Some(group.clone()));
                },
                x => panic!("Group rule with non-inline group? {:?}", x),
            }
        }
    }
    while global.reset_except_not_deserialized_and_plain_groups(&mut not_deserialized) {
        println!("Pass #{}", pass_count);
        pass_count += 1;
        // Can't generate groups of imports with codegen::Import so we just output this as raw text
        // since we don't need it to be dynamic so it's fine. codegen::Impl::new("a", "{z::b, z::c}")
        // does not work.
        global.scope().raw("// This library was code-generated using an experimental CDDL to rust tool:\n// https://github.com/Emurgo/cddl-codegen");
        global.scope().raw("use cbor_event::{self, de::Deserializer, se::{Serialize, Serializer}};");
        global.scope().import("std::io", "{BufRead, Seek, Write}");
        global.scope().import("wasm_bindgen::prelude", "*");
        global.scope().import("prelude", "*");
        global.scope().raw("use cbor_event::Type as CBORType;");
        global.scope().raw("use cbor_event::Special as CBORSpecial;");
        global.scope().raw("mod prelude;");
        global.scope().raw("mod serialization;");
        global.serialize_scope().import("super", "*");
        global.serialize_scope().import("std::io", "{Seek, SeekFrom}");
        for cddl_rule in &cddl.rules {
            println!("\n\n------------------------------------------\n- Handling rule: {}\n------------------------------------", cddl_rule.name());
            match cddl_rule {
                Rule::Type{ rule, .. } => {
                    // (1) does not handle optional generic parameters
                    // (2) is_type_choice_alternate ignored since shelley.cddl doesn't need it
                    //     It's used, but used for no reason as it is the initial definition
                    //     (which is also valid cddl), but it would be fine as = instead of /=
                    // (3) ignores control operators - only used in shelley spec to limit string length for application metadata
                    let rust_ident = RustIdent::new(CDDLIdent::new(rule.name.to_string()));
                    if rule.value.type_choices.len() == 1 {
                        let choice = &rule.value.type_choices.first().unwrap();
                        generate_type(&mut global, &rust_ident, &choice.type2, None);
                    } else {
                        generate_type_choices(&mut global, &rust_ident, &rule.value.type_choices, None);
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

    if !global.plain_groups.is_empty() {
        println!("\n\nPlain groups:");
        for plain_group in global.plain_groups.iter() {
            println!("{}", plain_group.0);
        }
    }

    if !global.type_aliases.is_empty() {
        println!("\n\nAliases:");
        for (alias_name, alias_type) in global.type_aliases.iter() {
            println!("{:?} -> {:?}", alias_name, alias_type);
        }
    }

    if !global.rust_structs.is_empty() {
        println!("\n\nRustStructs:");
        for (ident, rust_struct) in global.rust_structs.iter() {
            println!("{} -> {:?}\n", ident, rust_struct);
        }
    }

    global.print_structs_without_deserialize();

    Ok(())
}
