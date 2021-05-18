use std::collections::{BTreeMap};
use cbor_event::Type as CBORType;
use cbor_event::Special as CBORSpecial;

use crate::cmd::{
    BINARY_WRAPPERS,
    USE_EXTENDED_PRELUDE,
};
use crate::utils::{
    convert_to_camel_case,
    is_identifier_reserved,
    is_identifier_user_defined,
};

pub struct IntermediateTypes {
    // Storing the cddl::Group is the easiest way to go here even after the parse/codegen split.
    // This is since in order to generate plain groups we must have a representation, which isn't
    // known at group definition. It is later fixed when the plain group is referenced somewhere
    // and we can't parse the group without knowing the representation so instead this parsing is
    // delayed until the point where it is referenced via self.set_rep_if_plain_group(rep)
    // Some(group) = directly defined in .cddl (must call set_plain_group_representatio() later)
    // None = indirectly generated due to a group choice (no reason to call set_rep_if_plain_group() later but it won't crash)
    plain_groups: BTreeMap<RustIdent, Option<cddl::ast::Group>>,
    // (Type, whether to generate a rust type alias statement)
    type_aliases: BTreeMap<AliasIdent, (RustType, bool)>,
    rust_structs: BTreeMap<RustIdent, RustStruct>,
}

impl IntermediateTypes {
    pub fn new() -> Self {
        Self {
            plain_groups: BTreeMap::new(),
            type_aliases: Self::aliases(),
            rust_structs: BTreeMap::new(),
        }
    }

    pub fn type_aliases(&self) -> &BTreeMap<AliasIdent, (RustType, bool)> {
        &self.type_aliases
    }

    pub fn rust_structs(&self) -> &BTreeMap<RustIdent, RustStruct> {
        &self.rust_structs
    }

    fn aliases() -> BTreeMap<idents::AliasIdent, (RustType, bool)> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, (RustType, bool)>::new();
        let mut insert_alias = |name: &str, rust_type: RustType| {
            let ident = AliasIdent::new(CDDLIdent::new(name));
            aliases.insert(ident.clone(), (rust_type, false));
        };
        insert_alias("uint", RustType::Primitive(Primitive::U64));
        insert_alias("nint", RustType::Primitive(Primitive::N64));
        if USE_EXTENDED_PRELUDE {
            // We also provide non-standard 32-bit variants for ease of use from wasm
            insert_alias("u32", RustType::Primitive(Primitive::U32));
            insert_alias("i32", RustType::Primitive(Primitive::I32));
            insert_alias("u64", RustType::Primitive(Primitive::U64));
            insert_alias("i64", RustType::Primitive(Primitive::I64));
        }
        insert_alias("bool", RustType::Primitive(Primitive::Bool));
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type = RustType::Primitive(Primitive::Str);
        insert_alias("tstr", string_type.clone());
        insert_alias("text", string_type);
        insert_alias("bstr", RustType::Primitive(Primitive::Bytes));
        insert_alias("bytes", RustType::Primitive(Primitive::Bytes));
        let null_type = RustType::Fixed(FixedValue::Null);
        insert_alias("null", null_type.clone());
        insert_alias("nil", null_type);
        insert_alias("true", RustType::Fixed(FixedValue::Bool(true)));
        insert_alias("false", RustType::Fixed(FixedValue::Bool(false)));
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    pub fn new_type(&self, raw: &CDDLIdent) -> RustType {
        let alias_ident = AliasIdent::new(raw.clone());
        let resolved = match self.apply_type_aliases(&alias_ident) {
            Some(ty) => match alias_ident {
                AliasIdent::Reserved(_) => ty,
                AliasIdent::Rust(_) => RustType::Alias(alias_ident.clone(), Box::new(ty))
            },
            None => RustType::Rust(RustIdent::new(raw.clone())),
        };
        let resolved_inner = match &resolved {
            RustType::Alias(_, ty) => ty,
            ty => ty,
        };
        if BINARY_WRAPPERS {
            // if we're not literally bytes/bstr, and instead an alias for it
            // we would have generated a named wrapper object so we should
            // refer to that instead
            if !is_identifier_reserved(&raw.to_string()) {
                if let RustType::Primitive(Primitive::Bytes) = resolved_inner {
                    return RustType::Rust(RustIdent::new(raw.clone()));
                }
            }
        }
        // this would interfere with map/array loop code generation unless we
        // specifically handle this case since you wouldn't know whether you hit a break
        // or are reading a key here, unless we check, but then you'd need to store the
        // non-break special value once read
        if let RustType::Array(ty) = resolved_inner {
            assert!(!ty.cbor_types().contains(&CBORType::Special));
        }
        if let RustType::Map(key_type, _val_type) = resolved_inner {
            assert!(!key_type.cbor_types().contains(&CBORType::Special));
        }
        resolved
    }

    pub fn apply_type_aliases(&self, alias_ident: &AliasIdent) -> Option<RustType> {
        // Assumes we are not trying to pass in any kind of compound type (arrays, etc)
        match self.type_aliases.get(alias_ident) {
            Some((alias, _)) => Some(alias.clone()),
            None => match alias_ident {
                AliasIdent::Rust(_rust_ident) => None,
                AliasIdent::Reserved(reserved) => if reserved == "int" {
                    // We define an Int rust struct in prelude.rs
                    None
                } else {
                    panic!("Reserved ident {} didn't define type alias", reserved)
                },
            },
        }
    }

    pub fn register_type_alias(&mut self, alias: RustIdent, base_type: RustType, generate_rust_alias: bool) {
        if let RustType::Alias(_ident, _ty) = &base_type {
            panic!("register_type_alias*({}, {:?}) wrap automatically in Alias, no need to provide it.", alias, base_type);
        }
        self.type_aliases.insert(alias.into(), (base_type, generate_rust_alias));
    }

    pub fn rust_struct(&self, ident: &RustIdent) -> Option<&RustStruct> {
        self.rust_structs.get(ident)
    }

    // this is called by register_table_type / register_array_type automatically
    pub fn register_rust_struct(&mut self, rust_struct: RustStruct) {
        // we must provide the keys type to return
        if let RustStructType::Table { domain, .. } = &rust_struct.variant {
            self.create_and_register_array_type(domain.clone(), &domain.name_as_array());
        }
        self.rust_structs.insert(rust_struct.ident().clone(), rust_struct);
    }

    // creates a RustType for the array type - and if needed, registers a type to generate
    pub fn create_and_register_array_type(&mut self, element_type: RustType, array_type_name: &str) -> RustType {
        let raw_arr_type = RustType::Array(Box::new(element_type.clone()));
        // only generate an array wrapper if we can't wasm-expose it raw
        if raw_arr_type.directly_wasm_exposable() {
            return raw_arr_type;
        }
        let array_type_ident = RustIdent::new(CDDLIdent::new(array_type_name));
        // If we are the only thing referring to our element and it's a plain group
        // we must mark it as being serialized as an array
        if let RustType::Rust(_) = &element_type {
            self.set_rep_if_plain_group(&array_type_ident, Representation::Array);
        }
        // we don't pass in tags here. If a tag-wrapped array is done I think it generates
        // 2 separate types (array wrapper -> tag wrapper struct)
        self.register_rust_struct(RustStruct::new_array(array_type_ident, None, element_type.clone()));
        RustType::Array(Box::new(element_type))
    }

    // see self.plain_groups comments
    pub fn mark_plain_group(&mut self, ident: RustIdent, group: Option<cddl::ast::Group>) {
        self.plain_groups.insert(ident, group);
    }

    // see self.plain_groups comments
    pub fn set_rep_if_plain_group(&mut self, ident: &RustIdent, rep: Representation) {
        if let Some(plain_group) = self.plain_groups.get(ident) {
            // the clone is to get around the borrow checker
            if let Some(group) = plain_group.as_ref().map(|g| g.clone()) {
                // we are defined via .cddl and thus need to register a concrete
                // representation of the plain group
                if let Some(rust_struct) = self.rust_structs.get(ident) {
                    // it's already defined, let's check that we're not giving it multiple representations
                    let found_rep = match &rust_struct.variant {
                        RustStructType::Record(record) => Some(record.rep),
                        RustStructType::GroupChoice{ rep, .. } => Some(*rep),
                        _ => None,
                    };
                    assert_eq!(found_rep, Some(rep));
                } else {
                    // you can't tag plain groups hence the None
                    crate::parsing::parse_group(self, &group, ident, rep, None);
                }
            } else {
                // If plain_group is None, then this wasn't defined in .cddl but instead
                // created by us i.e. in a group choice with inlined fields.
                // In this case we already should have registered the struct with a defined
                // representation and we don't need to parse it here.
                assert!(self.rust_structs.contains_key(ident));
            }
        }
    }

    pub fn is_plain_group(&self, name: &RustIdent) -> bool {
        self.plain_groups.contains_key(name)
    }

    pub fn print_info(&self) {
        if !self.plain_groups.is_empty() {
            println!("\n\nPlain groups:");
            for plain_group in self.plain_groups.iter() {
                println!("{}", plain_group.0);
            }
        }
    
        if !self.type_aliases.is_empty() {
            println!("\n\nAliases:");
            for (alias_name, alias_type) in self.type_aliases.iter() {
                println!("{:?} -> {:?}", alias_name, alias_type);
            }
        }
    
        if !self.rust_structs.is_empty() {
            println!("\n\nRustStructs:");
            for (ident, rust_struct) in self.rust_structs.iter() {
                println!("{} -> {:?}\n", ident, rust_struct);
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Representation {
    Array,
    Map,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FixedValue {
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Primitive {
    Bool,
    // u32 in our cddl
    U32,
    // i32 in our cddl
    I32,
    // uint - also u64 in our cddl
    U64,
    // i64 in our cddl
    I64,
    // nint
    N64,
    Str,
    Bytes,
}

// TODO: impl display or fmt or whatever rust uses
impl Primitive {
    pub fn to_string(&self) -> String {
        String::from(match self {
            Primitive::Bool => "bool",
            Primitive::U32 => "u32",
            Primitive::I32 => "i32",
            Primitive::U64 => "u64",
            Primitive::I64 => "i64",
            // TODO: this should ideally by a u64 but the cbor_event lib takes i64 anyway so we don't get that extra precision
            Primitive::N64 => "i64",
            Primitive::Str => "String",
            Primitive::Bytes => "Vec<u8>",
        })
    }

    pub fn to_variant(&self) -> VariantIdent {
        VariantIdent::new_custom(match self {
            Primitive::Bool => "Bool",
            Primitive::U32 => "U32",
            Primitive::I32 => "I32",
            Primitive::U64 => "U64",
            Primitive::I64 => "I64",
            Primitive::N64 => "N64",
            Primitive::Str => "Text",
            Primitive::Bytes => "Bytes",
        })
    }

    pub fn cbor_types(&self) -> Vec<CBORType> {
        match self {
            Primitive::Bool => vec![CBORType::Special],
            Primitive::U32 => vec![CBORType::UnsignedInteger],
            Primitive::I32 => vec![CBORType::UnsignedInteger, CBORType::NegativeInteger],
            Primitive::U64 => vec![CBORType::UnsignedInteger],
            Primitive::I64 => vec![CBORType::UnsignedInteger, CBORType::NegativeInteger],
            Primitive::N64 => vec![CBORType::NegativeInteger],
            Primitive::Str => vec![CBORType::Text],
            Primitive::Bytes => vec![CBORType::Bytes],
        }
    }
}

mod idents {
    // to resolve ambiguities between raw (from CDDL) and already-formatted
    // for things like type aliases, etc, we use these wrapper structs

    // raw unchanged cddl identifier
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

    // formatted code-generation identifier exactly as how it would be in the rust code
    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub struct RustIdent(String);

    impl RustIdent {
        // this should not be created directly, but instead via IntermediateTypes::new_type()
        // except for defining new cddl rules, since those should not be reserved identifiers
        pub fn new(cddl_ident: CDDLIdent) -> Self {
            // int is special here since it refers to our own rust struct, not a primitive
            println!("{}", cddl_ident.0);
            assert!(cddl_ident.0 == "int" || super::is_identifier_user_defined(&cddl_ident.0));
            Self(super::convert_to_camel_case(&cddl_ident.0))
        }
    }

    impl std::fmt::Display for RustIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    // identifier for enum (group/type choice) variants
    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum VariantIdent {
        // Custom is used for cases like primitives, 0-ary fields, etc that need custom names
        Custom(String),
        // whereas with RustStruct we can directly use a rust-defined type as a name
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

    // identifier referring to a type alias
    #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum AliasIdent {
        // type definition defined in the cddl standard prelude
        Reserved(String),
        // user-made type alias
        Rust(RustIdent),
    }

    impl AliasIdent {
        pub fn new(ident: CDDLIdent) -> Self {
            if ident.0 == "int" || super::is_identifier_user_defined(&ident.0) {
                AliasIdent::Rust(RustIdent::new(ident))
            } else {
                AliasIdent::Reserved(ident.0)
            }
        }
    }

    impl From<RustIdent> for AliasIdent {
        fn from(ident: RustIdent) -> AliasIdent {
            AliasIdent::Rust(ident)
        }
    }

    impl std::fmt::Display for AliasIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                AliasIdent::Reserved(name) => write!(f, "{}", name),
                AliasIdent::Rust(ident) => ident.fmt(f),
            }
        }
    }
}
pub use idents::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RustType {
    Fixed(FixedValue),
    // Primitive type that can be passed to/from wasm
    Primitive(Primitive),
    // Rust-defined type that cannot be put in arrays/etc
    Rust(RustIdent),
    // Array-wrapped type. Passed as Vec<T> if T is Primitive
    Array(Box<RustType>),
    // Tagged type. Behavior depends entirely on wrapped type.
    Tagged(usize, Box<RustType>),
    // T / null in CDDL - auto-converts to Option<T> in rust for ease of use.
    Optional(Box<RustType>),
    // TODO: table type to support inlined defined table-type groups as fields
    Map(Box<RustType>, Box<RustType>),
    // Alias for another type
    Alias(AliasIdent, Box<RustType>),

    // TODO: for non-table-type ones we could define a RustField(Ident, RustType) and then
    // a variant here Struct(Vec<RustField>) and delegate field/argument generation to
    // RustField so that we could basically expand them and not care about having to generate
    // and intermediate fields - although this could pose an issue for optional types... so maybe
    // another approach would be necessary.
}

impl RustType {
    pub fn directly_wasm_exposable(&self) -> bool {
        match self {
            RustType::Fixed(_) => false,
            RustType::Primitive(_) => true,
            RustType::Rust(_) => false,
            // wasm_bindgen doesn't support nested vecs, even if the inner vec would be supported
            RustType::Array(ty) => {
                let inner = match &**ty {
                    RustType::Alias(_ident, ty) => ty,
                    RustType::Optional(ty) => ty,
                    RustType::Tagged(_tag, ty) => ty,
                    ty => ty,
                };
                match inner {
                    RustType::Primitive(p) => match p {
                        // converts to js number which is supported as Vec<T>
                        Primitive::Bool |
                        Primitive::I32 |
                        Primitive::U32 => true,
                        // since we generate these as BigNum/Int wrappers we can't nest them
                        Primitive::I64 |
                        Primitive::N64 |
                        Primitive::U64 => false,
                        // Bytes is already implemented as Vec<u8> so we can't nest it
                        Primitive::Bytes => false,
                        // Vec<String> is not supported by wasm-bindgen
                        Primitive::Str => false,
                    },
                    RustType::Array(_) => false,
                    _ => ty.directly_wasm_exposable(),
                }
            },
            RustType::Tagged(_tag, ty) => ty.directly_wasm_exposable(),
            RustType::Optional(ty) => ty.directly_wasm_exposable(),
            RustType::Map(_, _) => false,
            RustType::Alias(_ident, ty) => ty.directly_wasm_exposable(),
        }
    }

    pub fn is_fixed_value(&self) -> bool {
        match self {
            RustType::Fixed(_) => true,
            RustType::Tagged(_tag, ty) => ty.is_fixed_value(),
            RustType::Alias(_ident, ty) => ty.is_fixed_value(),
            _ => false,
        }
    }

    pub fn name_as_array(&self) -> String {
        if RustType::Array(Box::new(self.clone())).directly_wasm_exposable() {
            format!("Vec<{}>", self.for_member())
        } else {
            format!("{}s", self.for_member())
        }
    }

    pub fn for_wasm_param(&self) -> String {
        let x = match self {
            RustType::Fixed(_) => panic!("should not expose Fixed type to wasm, only here for serialization: {:?}", self),
            RustType::Primitive(p) => p.to_string(),
            RustType::Rust(ident) => format!("&{}", ident),
            RustType::Array(ty) => if self.directly_wasm_exposable() {
                ty.name_as_array()
            } else {
                format!("&{}", ty.name_as_array())
            },
            RustType::Tagged(_tag, ty) => ty.for_wasm_param(),
            RustType::Optional(ty) => format!("Option<{}>", ty.for_member()),
            RustType::Map(_k, _v) => format!("&{}", self.for_member()),
            RustType::Alias(ident, ty) => match &**ty {
                RustType::Rust(_) => format!("&{}", ident),
                _ => ident.to_string(),
            }
        };
        println!("for_wasm_param({:?}) = {}", self, x);
        x
    }

    pub fn for_wasm_return(&self) -> String {
        self.for_member()
    }

    pub fn for_member(&self) -> String {
        match self {
            RustType::Fixed(_) => panic!("should not expose Fixed type in member, only needed for serializaiton: {:?}", self),
            RustType::Primitive(p) => p.to_string(),
            RustType::Rust(ident) => ident.to_string(),
            RustType::Array(ty) => ty.name_as_array(),
            RustType::Tagged(_tag, ty) => ty.for_member(),
            RustType::Optional(ty) => format!("Option<{}>", ty.for_member()),
            RustType::Map(k, v) => format!("Map{}To{}", k.for_member(), v.for_member()),
            RustType::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_member(),
                // but other aliases are generated and should be used.
                AliasIdent::Rust(_) => ident.to_string(),
            },
        }
    }

    pub fn for_variant(&self) -> VariantIdent {
        match self {
            RustType::Fixed(f) => f.for_variant(),
            RustType::Primitive(p) => p.to_variant(),
            RustType::Rust(ident) => VariantIdent::new_rust(ident.clone()),
            RustType::Array(inner) => VariantIdent::new_custom(format!("Arr{}", inner.for_variant())),
            RustType::Tagged(_tag, ty) => ty.for_variant(),
            // TODO: should we not end up in this situation and just insert a Null fixed value instead?
            RustType::Optional(ty) => VariantIdent::new_custom(format!("Opt{}", ty.for_variant())),
            RustType::Map(k, v) => VariantIdent::new_custom(format!("Map{}To{}", k.for_variant(), v.for_variant())),
            RustType::Alias(ident, _ty) => match ident {
                AliasIdent::Rust(rust_ident) => VariantIdent::new_rust(rust_ident.clone()),
                AliasIdent::Reserved(reserved) => VariantIdent::new_custom(reserved),
            },
        }
    }

    // for parameters from wasm that take ownership (via cloning here)
    pub fn from_wasm_boundary_clone(&self, expr: &str) -> String {
        match self {
            RustType::Tagged(_tag, ty) => ty.from_wasm_boundary_clone(expr),
            RustType::Rust(_ident) => format!("{}.clone()", expr),
            RustType::Alias(_ident, ty) => ty.from_wasm_boundary_clone(expr),
            RustType::Optional(ty) => ty.from_wasm_boundary_clone(expr),
            RustType::Array(ty) => if self.directly_wasm_exposable() {
                ty.from_wasm_boundary_clone(expr)
            } else {
                format!("{}.clone()", expr)
            },
            RustType::Map(_k, _v) => format!("{}.clone()", expr),
            _ => expr.to_owned(),
        }
    }

    // for non-owning parameters from wasm
    pub fn from_wasm_boundary_ref(&self, expr: &str) -> String {
        match self {
            RustType::Tagged(_tag, ty) => ty.from_wasm_boundary_ref(expr),
            RustType::Rust(_ident) => expr.to_owned(),
            RustType::Alias(_ident, ty) => ty.from_wasm_boundary_ref(expr),
            RustType::Optional(ty) => ty.from_wasm_boundary_ref(expr),
            RustType::Array(ty) => if self.directly_wasm_exposable() {
                ty.from_wasm_boundary_ref(expr)
            } else {
                expr.to_owned()
            },
            RustType::Map(_k, _v) => expr.to_owned(),
            _ => format!("&{}", expr),
        }
    }

    // Ok case is single first cbor type in bytes, Err is multiple possibilities
    pub fn cbor_types(&self) -> Vec<CBORType> {
        match self {
            RustType::Fixed(f) => vec![match f {
                FixedValue::Uint(_) => CBORType::UnsignedInteger,
                FixedValue::Int(_) => CBORType::NegativeInteger,
                FixedValue::Text(_) => CBORType::Text,
                FixedValue::Null => CBORType::Special,
                FixedValue::Bool(_) => CBORType::Special,
            }],
            RustType::Primitive(p) => p.cbor_types(),
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
            },
            RustType::Alias(_ident, ty) => ty.cbor_types(),
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
            RustType::Alias(_ident, ty) => ty._is_serialize_multiline(),
        }
    }

    // CBOR len count for the entire type if it were embedded as a member in a cbor collection (array/map)
    pub fn expanded_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        match self {
            RustType::Optional(ty) => match ty.expanded_field_count(types) {
                Some(1) => Some(1),
                // differing sizes when Null vs Some
                _ => None,
            },
            // TODO: instead of returning None when ident doesn't exist, throw error.
            // Once we split up parsing and codegen this shouldn't happen but with our current multi-pass
            // approach we might have out of order struct references which would break here without it
            // but on the final pass (the one we export) this should't be an issue
            RustType::Rust(ident) => if types.is_plain_group(ident) {
                types.rust_structs.get(&ident)?.fixed_field_count(types)
            } else {
                Some(1)
            },
            RustType::Alias(_ident, ty) => ty.expanded_field_count(types),
            _ => Some(1),
        }
    }

    // See comment in RustStruct::definite_info(), this is the same, returns a string expression
    // which evaluates to the length when possible, or None if not.
    // self_expr is an expresison that evaluates to this RustType (e.g. member, etc) at the point where
    // the return of this function will be used.
    pub fn definite_info(&self, self_expr: &str, types: &IntermediateTypes) -> Option<String> {
        match self.expanded_field_count(types) {
            Some(count) => Some(count.to_string()),
            None => match self {
                RustType::Optional(ty) => Some(format!("match {} {{ Some(x) => {}, None => 1 }}", self_expr, ty.definite_info("x", types)?)),
                RustType::Rust(ident) => if types.is_plain_group(ident) {
                    match types.rust_structs.get(&ident) {
                        Some(rs) => rs.definite_info(types),
                        // when we split up parsing from codegen instead of multi-passing this should be an error
                        None => None,
                    }
                } else {
                    Some(String::from("1"))
                },
                RustType::Alias(_ident, ty) => ty.definite_info(self_expr, types),
                _ => Some(String::from("1")),
            }
        }
    }

    // the minimum cbor length of this struct - can be useful for deserialization length checks
    // does not count ANY type choice like types including Optional UNLESS the option Some type
    // has cbor len 1 too - to be consistent with expanded_field_count
    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        match self {
            RustType::Optional(ty) => match ty.expanded_field_count(types) {
                Some(1) => 1,
                _ => 0,
            },
            RustType::Rust(ident) => if types.is_plain_group(ident) {
                println!("ident: {}", ident);
                types.rust_structs.get(&ident).unwrap().expanded_mandatory_field_count(types)
            } else {
                1
            },
            RustType::Alias(_ident, ty) => ty.expanded_mandatory_field_count(types),
            _ => 1,
        }
    }
}

// rep is Optional - None means we just serialize raw, ie for type choices
#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: VariantIdent,
    pub rust_type: RustType,
    pub serialize_as_embedded_group: bool,
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

#[derive(Clone, Debug)]
pub struct RustField {
    pub name: String,
    pub rust_type: RustType,
    pub optional: bool,
    // None for array fields, Some for map fields. FixedValue for (de)serialization for map keys
    pub key: Option<FixedValue>,
}

impl RustField {
    pub fn new(name: String, rust_type: RustType, optional: bool, key: Option<FixedValue>) -> Self {
        Self {
            name,
            rust_type,
            optional,
            key,
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum RustStructCBORLen {
    // always a fixed number of CBOR length
    Fixed(usize),
    // can vary with no min/max
    Dynamic,
    // has optional fields. (mandatory fields) - skips over type choices (including T / nil -> Option<T>)
    OptionalFields(usize),
}

// TODO: It would be nice to separate parsing the CDDL lib structs and code generation entirely.
// We would just need to construct these structs (+ maybe the array/table wrapper types) separately and pass these into codegen.
// This would also give us more access to this info without reparsing which could simplify code in some places.
// It would also remove the need for multiple passes over the CDDL to sort out dependencies between structs,
// which could also pave the way for multi-file CDDL supprt.
#[derive(Clone, Debug)]
pub struct RustStruct {
    ident: RustIdent,
    tag: Option<usize>,
    variant: RustStructType,
}

#[derive(Clone, Debug)]
pub enum RustStructType {
    Record(RustRecord),
    Table {
        domain: RustType,
        range: RustType,
    },
    Array {
        element_type: RustType,
    },
    TypeChoice {
        variants: Vec<EnumVariant>,
    },
    GroupChoice {
        variants: Vec<EnumVariant>,
        rep: Representation,
    },
    Wrapper(RustType),
}

impl RustStruct {
    pub fn new_record(ident: RustIdent, tag: Option<usize>, record: RustRecord) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Record(record),
        }
    }

    pub fn new_table(ident: RustIdent, tag: Option<usize>, domain: RustType, range: RustType) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Table {
                domain,
                range,
            },
        }
    }

    pub fn new_array(ident: RustIdent, tag: Option<usize>, element_type: RustType) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Array {
                element_type,
            },
        }
    }

    pub fn new_type_choice(ident: RustIdent, tag: Option<usize>, variants: Vec<EnumVariant>) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::TypeChoice {
                variants
            }
        }
    }

    pub fn new_group_choice(ident: RustIdent, tag: Option<usize>, variants: Vec<EnumVariant>, rep: Representation) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::GroupChoice {
                variants,
                rep
            }
        }
    }

    pub fn new_wrapper(ident: RustIdent, tag: Option<usize>, wrapped_type: RustType) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Wrapper(wrapped_type),
        }
    }

    pub fn ident(&self) -> &RustIdent {
        &self.ident
    }

    pub fn tag(&self) -> Option<usize> {
        self.tag
    }

    pub fn variant(&self) -> &RustStructType {
        &self.variant
    }

    // The following methods are used internally to generate serialize/deserialize code
    // INSIDE of the serialize/deserialize implementations for this specific type.
    // You probably aren't interested in this from outside of that use-case.
    
    // Some(count) if it always has the same number of fields (ie no optional fields), None otherwise
    pub fn fixed_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        match &self.variant {
            RustStructType::Record(record) => record.fixed_field_count(types),
            RustStructType::Table{ .. } => None,
            RustStructType::Array{ .. } => None,
            // TODO: investigate if we should be supporting this for TypeChoice (also wrapper?)
            //RustStructType::TypeChoice { .. } => None,
            RustStructType::TypeChoice{ .. } => unreachable!("I don't think type choices should be using length?"),
            RustStructType::GroupChoice{ .. } => unreachable!("I don't think group choices should be using length?"),
            RustStructType::Wrapper(_wrapped) => unreachable!("wrapper types don't use length"),
        }
    }

    // Even if fixed_field_count() == None, this will try and return an expression for
    // a definite length, e.g. with optional field checks in the expression
    // This is useful for definite-length serialization
    pub fn definite_info(&self, types: &IntermediateTypes) -> Option<String> {
        match &self.variant {
            RustStructType::Record(record) => record.definite_info(types),
            RustStructType::Table{ .. } => Some(String::from("self.0.len() as u64")),
            RustStructType::Array{ .. } => Some(String::from("self.0.len() as u64")),
            //RustStructType::TypeChoice{ .. } => None,
            RustStructType::TypeChoice{ .. } => unreachable!("I don't think type choices should be using length?"),
            RustStructType::GroupChoice{ .. } => unreachable!("I don't think group choices should be using length?"),
            RustStructType::Wrapper(_wrapped) => unreachable!("wrapper types don't use length"),
        }
    }

    // the minimum cbor length of this struct - can be useful for deserialization length checks
    // does not count ANY type choice like types including Optional UNLESS the option Some type
    // has cbor len 1 too - to be consistent with expanded_field_count
    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        match &self.variant {
            RustStructType::Record(record) => record.expanded_mandatory_field_count(types),
            RustStructType::Table{ .. } => 0,
            RustStructType::Array{ .. } => 0,
            //RustStructType::TypeChoice{ .. } => 0,
            RustStructType::TypeChoice{ .. } => unreachable!("I don't think type choices should be using length?"),
            RustStructType::GroupChoice{ .. } => unreachable!("I don't think group choices should be using length?"),
            RustStructType::Wrapper(_wrapped) => unreachable!("wrapper types don't use length"),
        }
    }

    fn _cbor_len_info(&self, types: &IntermediateTypes) -> RustStructCBORLen {
        match &self.variant {
            RustStructType::Record(record) => record.cbor_len_info(types),
            RustStructType::Table{ .. } => RustStructCBORLen::Dynamic,
            RustStructType::Array{ .. } => RustStructCBORLen::Dynamic,
            //RustStructType::TypeChoice{ .. } => RustStructCBORLen::Dynamic,
            RustStructType::TypeChoice{ .. } => unreachable!("I don't think type choices should be using length?"),
            RustStructType::GroupChoice{ .. } => unreachable!("I don't think group choices should be using length?"),
            RustStructType::Wrapper(_wrapped) => unreachable!("wrapper types don't use length"),
        }
    }
}

// Regular struct with fields and such
#[derive(Clone, Debug)]
pub struct RustRecord {
    pub rep: Representation,
    pub fields: Vec<RustField>,
}

impl RustRecord {
    pub fn fixed_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        let mut count = 0;
        for field in &self.fields {
            if field.optional {
                return None;
            }
            count += field.rust_type.expanded_field_count(types)?;
        }
        Some(count)
    }

    pub fn definite_info(&self, types: &IntermediateTypes) -> Option<String> {
        match self.fixed_field_count(types) {
            Some(count) => Some(count.to_string()),
            None => {
                let mut fixed_field_count = 0;
                let mut conditional_field_expr = String::new();
                for field in &self.fields {
                    if field.optional {
                        if !conditional_field_expr.is_empty() {
                            conditional_field_expr.push_str(" + ");
                        }
                        conditional_field_expr.push_str(&format!("match &self.{} {{ Some(x) => {}, None => 0 }}", field.name, field.rust_type.definite_info("x", types)?));
                    } else {
                        match field.rust_type.expanded_field_count(types) {
                            Some(field_expanded_count) => fixed_field_count += field_expanded_count,
                            None => {
                                if !conditional_field_expr.is_empty() {
                                    conditional_field_expr.push_str(" + ");
                                }
                                let field_len_expr = field.rust_type.definite_info(&format!("self.{}", field.name), types)?;
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

    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        self.fields.iter().filter(|field| !field.optional).map(|field| field.rust_type.expanded_mandatory_field_count(types)).sum()
    }

    pub fn cbor_len_info(&self, types: &IntermediateTypes) -> RustStructCBORLen {
        match self.fixed_field_count(types) {
            Some(fixed_count) => RustStructCBORLen::Fixed(fixed_count),
            None => RustStructCBORLen::OptionalFields(self.expanded_mandatory_field_count(types)),
        }
    }
}