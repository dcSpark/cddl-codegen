use std::collections::{BTreeMap, BTreeSet};
use cbor_event::Type as CBORType;
use cbor_event::Special as CBORSpecial;
use cddl::ast::parent::ParentVisitor;

use crate::cli::CLI_ARGS;
// TODO: move all of these generation specifics into generation.rs
use crate::generation::table_type;
use crate::utils::{
    cddl_prelude,
    convert_to_camel_case,
    convert_to_snake_case,
    is_identifier_reserved,
    is_identifier_user_defined,
};

#[derive(Debug)]
pub struct IntermediateTypes<'a> {
    // Storing the cddl::Group is the easiest way to go here even after the parse/codegen split.
    // This is since in order to generate plain groups we must have a representation, which isn't
    // known at group definition. It is later fixed when the plain group is referenced somewhere
    // and we can't parse the group without knowing the representation so instead this parsing is
    // delayed until the point where it is referenced via self.set_rep_if_plain_group(rep)
    // Some(group) = directly defined in .cddl (must call set_plain_group_representatio() later)
    // None = indirectly generated due to a group choice (no reason to call set_rep_if_plain_group() later but it won't crash)
    plain_groups: BTreeMap<RustIdent, Option<cddl::ast::Group<'a>>>,
    // (Type, whether to generate a rust type alias statement, whether to generate it for wasm)
    type_aliases: BTreeMap<AliasIdent, (RustType, bool, bool)>,
    rust_structs: BTreeMap<RustIdent, RustStruct>,
    prelude_to_emit: BTreeSet<String>,
    generic_defs: BTreeMap<RustIdent, GenericDef>,
    generic_instances: BTreeMap<RustIdent, GenericInstance>,
    news_can_fail: BTreeSet<RustIdent>,
    used_as_key: BTreeSet<RustIdent>,
    scopes: BTreeMap<RustIdent, String>,
}

impl<'a> IntermediateTypes<'a> {
    pub fn new() -> Self {
        let mut rust_structs = BTreeMap::new();
        rust_structs.insert(RustIdent::new(CDDLIdent::new("int")), RustStruct::new_prelude(RustIdent::new(CDDLIdent::new("int"))));
        Self {
            plain_groups: BTreeMap::new(),
            type_aliases: Self::aliases(),
            rust_structs,
            prelude_to_emit: BTreeSet::new(),
            generic_defs: BTreeMap::new(),
            generic_instances: BTreeMap::new(),
            news_can_fail: BTreeSet::new(),
            used_as_key: BTreeSet::new(),
            scopes: BTreeMap::new(),
        }
    }

    pub fn has_ident(&self, ident: &RustIdent) -> bool {
        let foo: Vec<RustIdent> = self.type_aliases.keys().fold(vec![], |mut acc, alias| {
            match alias {
                AliasIdent::Reserved(_) => {},
                AliasIdent::Rust(ident) => { acc.push(ident.clone()) }
            };
            acc
        });
        println!("{:?}", self.plain_groups.keys().chain(foo.iter()).chain(self.rust_structs.keys()).chain(self.generic_defs.keys()).chain(self.generic_instances.keys()));
        self.plain_groups.contains_key(ident)
         || self.type_aliases.contains_key(&AliasIdent::Rust(ident.clone()))
         || self.rust_structs.contains_key(ident)
         || self.generic_defs.contains_key(ident)
         || self.generic_instances.contains_key(ident)
    }

    pub fn type_aliases(&self) -> &BTreeMap<AliasIdent, (RustType, bool, bool)> {
        &self.type_aliases
    }

    pub fn rust_structs(&self) -> &BTreeMap<RustIdent, RustStruct> {
        &self.rust_structs
    }

    fn aliases() -> BTreeMap<idents::AliasIdent, (RustType, bool, bool)> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, (RustType, bool, bool)>::new();
        let mut insert_alias = |name: &str, rust_type: RustType| {
            let ident = AliasIdent::new(CDDLIdent::new(name));
            aliases.insert(ident.clone(), (rust_type, false, false));
        };
        insert_alias("uint", ConceptualRustType::Primitive(Primitive::U64).into());
        insert_alias("nint", ConceptualRustType::Primitive(Primitive::N64).into());
        insert_alias("bool", ConceptualRustType::Primitive(Primitive::Bool).into());
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type: RustType = ConceptualRustType::Primitive(Primitive::Str).into();
        insert_alias("tstr", string_type.clone());
        insert_alias("text", string_type);
        insert_alias("bstr", ConceptualRustType::Primitive(Primitive::Bytes).into());
        insert_alias("bytes", ConceptualRustType::Primitive(Primitive::Bytes).into());
        let null_type: RustType = ConceptualRustType::Fixed(FixedValue::Null).into();
        insert_alias("null", null_type.clone());
        insert_alias("nil", null_type);
        insert_alias("true", ConceptualRustType::Fixed(FixedValue::Bool(true)).into());
        insert_alias("false", ConceptualRustType::Fixed(FixedValue::Bool(false)).into());
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    // note: this is mut so that apply_type_aliases() can mark which reserved idents
    // are in the CDDL prelude so we don't generate code for all of them, potentially
    // bloating generated code a bit
    pub fn new_type(&mut self, raw: &CDDLIdent) -> RustType {
        let alias_ident = AliasIdent::new(raw.clone());
        let resolved = match self.apply_type_aliases(&alias_ident) {
            Some(ty) => match alias_ident {
                AliasIdent::Reserved(_) => ty,
                AliasIdent::Rust(_) => ty.as_alias(alias_ident.clone())
            },
            None => ConceptualRustType::Rust(RustIdent::new(raw.clone())).into(),
        };
        let resolved_inner = match &resolved.conceptual_type {
            ConceptualRustType::Alias(_, ty) => ty,
            ty => ty,
        };
        if CLI_ARGS.binary_wrappers {
            // if we're not literally bytes/bstr, and instead an alias for it
            // we would have generated a named wrapper object so we should
            // refer to that instead
            if !is_identifier_reserved(&raw.to_string()) {
                if let ConceptualRustType::Primitive(Primitive::Bytes) = resolved_inner {
                    return ConceptualRustType::Rust(RustIdent::new(raw.clone())).into();
                }
            }
        }
        // this would interfere with map/array loop code generation unless we
        // specifically handle this case since you wouldn't know whether you hit a break
        // or are reading a key here, unless we check, but then you'd need to store the
        // non-break special value once read
        if let ConceptualRustType::Array(ty) = resolved_inner {
            assert!(!ty.cbor_types().contains(&CBORType::Special));
        }
        if let ConceptualRustType::Map(key_type, _val_type) = resolved_inner {
            assert!(!key_type.cbor_types().contains(&CBORType::Special));
        }
        resolved
    }

    // see new_type() for why this is mut
    pub fn apply_type_aliases(&mut self, alias_ident: &AliasIdent) -> Option<RustType> {
        // Assumes we are not trying to pass in any kind of compound type (arrays, etc)
        match self.type_aliases.get(alias_ident) {
            Some((alias, _, _)) => Some(alias.clone()),
            None => match alias_ident {
                AliasIdent::Rust(_rust_ident) => None,
                AliasIdent::Reserved(reserved) => if reserved == "int" {
                    // We define an Int rust struct in prelude.rs
                    None
                } else {
                    // we auto-include only the parts of the cddl prelude necessary (and supported)
                    cddl_prelude(reserved).expect(&format!("Reserved ident {} not a part of cddl_prelude?", reserved));
                    self.emit_prelude(reserved.clone());
                    Some(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(format!("prelude_{}", reserved)))).into())
                },
            },
        }
    }

    pub fn register_type_alias(&mut self, alias: RustIdent, base_type: RustType, generate_rust_alias: bool, generate_wasm_alias: bool) {
        if let ConceptualRustType::Alias(_ident, _ty) = &base_type.conceptual_type {
            panic!("register_type_alias*({}, {:?}) wrap automatically in Alias, no need to provide it.", alias, base_type);
        }
        self.type_aliases.insert(alias.into(), (base_type, generate_rust_alias, generate_wasm_alias));
    }

    pub fn rust_struct(&self, ident: &RustIdent) -> Option<&RustStruct> {
        self.rust_structs.get(ident)
    }

    // this is called by register_table_type / register_array_type automatically
    pub fn register_rust_struct(&mut self, parent_visitor: &ParentVisitor, rust_struct: RustStruct) {
        match &rust_struct.variant {
            RustStructType::Table { domain, range } => {
                // we must provide the keys type to return
                self.create_and_register_array_type(parent_visitor, domain.clone(), &domain.conceptual_type.name_as_wasm_array());
                let mut map_type: RustType = ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone())).into();
                if let Some(tag) = rust_struct.tag {
                    map_type = map_type.tag(tag);
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    map_type,
                    true,
                    false)
            },
            RustStructType::Array { element_type } => {
                let mut array_type: RustType = ConceptualRustType::Array(Box::new(element_type.clone())).into();
                if let Some(tag) = rust_struct.tag {
                    array_type = array_type.tag(tag);
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    array_type,
                    true,
                    false)
            },
            RustStructType::Wrapper { min_max: Some(_) , ..} => {
                self.mark_new_can_fail(rust_struct.ident.clone());
            },
            _ => (),
        }
        self.rust_structs.insert(rust_struct.ident().clone(), rust_struct);
    }

    // creates a RustType for the array type - and if needed, registers a type to generate
    // TODO: After the split we should be able to only register it directly
    // and then examine those at generation-time and handle things ALWAYS as RustType::Array
    pub fn create_and_register_array_type(&mut self, parent_visitor: &ParentVisitor, element_type: RustType, array_type_name: &str) -> RustType {
        let raw_arr_type = ConceptualRustType::Array(Box::new(element_type.clone()));
        // only generate an array wrapper if we can't wasm-expose it raw
        if raw_arr_type.directly_wasm_exposable() {
            return raw_arr_type.into();
        }
        let array_type_ident = RustIdent::new(CDDLIdent::new(array_type_name));
        // If we are the only thing referring to our element and it's a plain group
        // we must mark it as being serialized as an array
        if let ConceptualRustType::Rust(_) = &element_type.conceptual_type {
            self.set_rep_if_plain_group(parent_visitor, &array_type_ident, Representation::Array);
        }
        if CLI_ARGS.wasm {
            // we don't pass in tags here. If a tag-wrapped array is done I think it generates
            // 2 separate types (array wrapper -> tag wrapper struct)
            self.register_rust_struct(parent_visitor, RustStruct::new_array(array_type_ident, None, element_type.clone()));
        }
        ConceptualRustType::Array(Box::new(element_type)).into()
    }

    pub fn register_generic_def(&mut self, def: GenericDef) {
        let ident = def.orig.ident().clone();
        self.generic_defs.insert(ident, def);
    }

    pub fn register_generic_instance(&mut self, instance: GenericInstance) {
        let ident = instance.instance_ident.clone();
        self.generic_instances.insert(ident, instance);
    }

    // call this after all types have been registered
    pub fn finalize(&mut self, parent_visitor: &ParentVisitor) {
        // resolve generics
        // resolve then register in 2 phases to get around borrow checker
        let resolved_generics = self.generic_instances.values().map(|instance| instance.resolve(self)).collect::<Vec<_>>();
        for resolved_instance in resolved_generics {
            self.register_rust_struct(parent_visitor, resolved_instance);
        }
        // recursively check all types used as keys or contained within a type used as a key
        // this is so we only derive comparison or hash traits for those types
        let mut used_as_key = BTreeSet::new();
        fn mark_used_as_key(ty: &ConceptualRustType, used_as_key: &mut BTreeSet<RustIdent>) {
            if let ConceptualRustType::Rust(ident) = ty {
                used_as_key.insert(ident.clone());
            }
        }
        fn check_used_as_key<'a>(ty: &ConceptualRustType, types: &IntermediateTypes<'a>, used_as_key: &mut BTreeSet<RustIdent>) {
            if let ConceptualRustType::Map(k, _v) = ty {
                k.visit_types(types, &mut |ty| mark_used_as_key(ty, used_as_key));
            }
        }
        for rust_struct in self.rust_structs().values() {
            rust_struct.visit_types(self, &mut |ty| check_used_as_key(ty, self, &mut used_as_key));
            if let RustStructType::Table{ domain, .. } = rust_struct.variant() {
                domain.visit_types(self, &mut |ty| mark_used_as_key(ty, &mut used_as_key));
            }
        }
        self.used_as_key = used_as_key;
    }

    pub fn visit_types<F: FnMut(&ConceptualRustType)>(&self, f: &mut F) {
        for rust_struct in self.rust_structs().values() {
            rust_struct.visit_types(self, f);
        }
    }

    pub fn is_referenced(&self, ident: &RustIdent) -> bool {
        let mut found = false;
        self.visit_types(&mut |ty| match ty {
            ConceptualRustType::Rust(id) => if id == ident {
                found = true
            },
            _ => (),
        });
        found
    }

    // see self.plain_groups comments
    pub fn mark_plain_group(&mut self, ident: RustIdent, group: Option<cddl::ast::Group<'a>>) {
        self.plain_groups.insert(ident, group);
    }

    // see self.plain_groups comments
    pub fn set_rep_if_plain_group(&mut self, parent_visitor: &ParentVisitor, ident: &RustIdent, rep: Representation) {
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
                    // we also don't support generics in plain groups hence the other None
                    crate::parsing::parse_group(self, parent_visitor, &group, ident, rep, None, None);
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

    fn mark_new_can_fail(&mut self, name: RustIdent) {
        self.news_can_fail.insert(name);
    }

    pub fn can_new_fail(&self, name: &RustIdent) -> bool {
        self.news_can_fail.contains(name)
    }

    pub fn mark_scope(&mut self, ident: RustIdent, scope: String) {
        if let Some(old_scope) = self.scopes.insert(ident.clone(), scope.clone()) {
            if old_scope != scope {
                panic!("{} defined multiple times, first referenced in scope '{}' then in '{}'", ident, old_scope, scope);
            }
        }
    }

    pub fn scope(&self, ident: &RustIdent) -> &str {
        self.scopes.get(ident).map(|s| s.as_str()).unwrap_or("lib")
    }

    pub fn used_as_key(&self, name: &RustIdent) -> bool {
        self.used_as_key.contains(name)
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

        if !self.generic_defs.is_empty() {
            println!("\n\nGeneric Definitions:");
            for (ident, def) in self.generic_defs.iter() {
                println!("{} -> {:?}", ident, def);
            }
        }

        if !self.generic_instances.is_empty() {
            println!("\n\nGeneric Instances:");
            for (ident, def) in self.generic_instances.iter() {
                println!("{} -> {:?}", ident, def);
            }
        }
    
        if !self.rust_structs.is_empty() {
            println!("\n\nRustStructs:");
            for (ident, rust_struct) in self.rust_structs.iter() {
                println!("{} -> {:?}\n", ident, rust_struct);
            }
        }
    }

    fn emit_prelude(&mut self, cddl_name: String) {
        // we just emit this directly into this scope.
        // due to some referencing others this is the quickest way
        // to support it.
        // TODO: we might want to custom-write some of these to make them
        // easier to use instead of directly parsing
        if self.prelude_to_emit.insert(cddl_name.clone()) {
            let def = format!("prelude_{} = {}\n", cddl_name, cddl_prelude(&cddl_name).unwrap());
            let cddl = cddl::parser::cddl_from_str(&def, true).unwrap();
            assert_eq!(cddl.rules.len(), 1);
            let pv = ParentVisitor::new(&cddl).unwrap();
            crate::parsing::parse_rule(self, &pv, cddl.rules.first().unwrap());
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
    Nint(isize),
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
            FixedValue::Nint(i) => VariantIdent::new_custom(format!("U{}", i)),
            FixedValue::Uint(u) => VariantIdent::new_custom(format!("I{}", u)),
            //FixedValue::Float(f) => format!("F{}", f),
            FixedValue::Text(s) => VariantIdent::new_custom(convert_to_alphanumeric(&convert_to_camel_case(&s))),
        }
    }

    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        match self {
            FixedValue::Null => buf.write_special(cbor_event::Special::Null),
            FixedValue::Bool(b) => buf.write_special(cbor_event::Special::Bool(*b)),
            FixedValue::Nint(i) => buf.write_negative_integer(*i as i64),
            FixedValue::Uint(u) => buf.write_unsigned_integer(*u as u64),
            FixedValue::Text(s) => buf.write_text(s),
        }.expect("Unable to serialize key for canonical ordering");
        buf.finalize()
    }

    /// Converts a literal to a valid rust expression capable of initializing a Primitive
    /// e.g. Text is an actual String, etc
    pub fn to_primitive_str_assign(&self) -> String {
        match self {
            FixedValue::Null => "None".to_owned(),
            FixedValue::Bool(b) => b.to_string(),
            FixedValue::Nint(i) => i.to_string(),
            FixedValue::Uint(u) => u.to_string(),
            FixedValue::Text(s) => format!("\"{}\".to_owned()", s),
        }
    }

    /// Converts a literal to a valid rust comparison valid for comparisons
    /// e.g. Text can be &str to avoid creating a String
    pub fn to_primitive_str_compare(&self) -> String {
        match self {
            FixedValue::Text(s) => format!("\"{}\"", s),
            _=> self.to_primitive_str_assign(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Primitive {
    Bool,
    // u8 in our cddl
    U8,
    // i8 in our cddl
    I8,
    // u16 in our cddl
    U16,
    // i16 in our cddl
    I16,
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
            Primitive::U8 => "u8",
            Primitive::I8 => "i8",
            Primitive::U16 => "u16",
            Primitive::I16 => "i16",
            Primitive::U32 => "u32",
            Primitive::I32 => "i32",
            Primitive::U64 => "u64",
            Primitive::I64 => "i64",
            Primitive::N64 => "u64",
            Primitive::Str => "String",
            Primitive::Bytes => "Vec<u8>",
        })
    }

    pub fn to_variant(&self) -> VariantIdent {
        VariantIdent::new_custom(match self {
            Primitive::Bool => "Bool",
            Primitive::U8 => "U8",
            Primitive::I8 => "I8",
            Primitive::U16 => "U16",
            Primitive::I16 => "I16",
            Primitive::U32 => "U32",
            Primitive::I32 => "I32",
            Primitive::U64 => "U64",
            Primitive::I64 => "I64",
            Primitive::N64 => "N64",
            Primitive::Str => "Text",
            Primitive::Bytes => "Bytes",
        })
    }

    /// All POSSIBLE outermost CBOR types this can encode to
    pub fn cbor_types(&self) -> Vec<CBORType> {
        match self {
            Primitive::Bool => vec![CBORType::Special],
            Primitive::U8 => vec![CBORType::UnsignedInteger],
            Primitive::I8 => vec![CBORType::UnsignedInteger, CBORType::NegativeInteger],
            Primitive::U16 => vec![CBORType::UnsignedInteger],
            Primitive::I16 => vec![CBORType::UnsignedInteger, CBORType::NegativeInteger],
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
    use crate::{rust_reserved::STD_TYPES, utils::{is_identifier_reserved, is_identifier_in_our_prelude}, cli::CLI_ARGS};

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

            assert!(!STD_TYPES.contains(&&super::convert_to_camel_case(&cddl_ident.0)[..]), "Cannot use reserved Rust type name: \"{}\"", cddl_ident.0);
            if cddl_ident.0 != "int" {
                assert!(!is_identifier_reserved(&cddl_ident.0), "Cannot use reserved CDDL keyword: \"{}\"", cddl_ident.0);
            }

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

/// Details on how to encode a rust type in CBOR. Order is important
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CBOREncodingOperation {
    /// CBOR tagged type
    Tagged(usize),
    /// bytes .cbor T in cddl, outside of serialization is semantically like T
    CBORBytes,
}

/// A complete rust type, including serialization options that don't impact other areas
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RustType {
    /// Conceptual type i.e. how it's used in non-serialization contexts
    pub conceptual_type: ConceptualRustType,
    /// How to encode the conceptual type. Order is important. Applied in iteration order.
    pub encodings: Vec<CBOREncodingOperation>,
    // default value when missing in deserialization
    pub default: Option<FixedValue>,
}

impl std::ops::Deref for RustType {
    type Target = ConceptualRustType;

    fn deref(&self) -> &Self::Target {
        &self.conceptual_type
    }
}

impl RustType {
    pub fn new(conceptual_type: ConceptualRustType) -> Self {
        Self {
            conceptual_type,
            encodings: Vec::new(),
            default: None,
        }
    }

    pub fn as_alias(mut self, alias_ident: AliasIdent) -> Self {
        self.conceptual_type = ConceptualRustType::Alias(alias_ident, Box::new(self.conceptual_type));
        self
    }

    pub fn tag(mut self, tag: usize) -> Self {
        self.encodings.push(CBOREncodingOperation::Tagged(tag));
        self
    }

    pub fn default(mut self, default_value: FixedValue) -> Self {
        assert!(self.default.is_none());
        let matches = if let ConceptualRustType::Primitive(p) = self.conceptual_type.clone().resolve_aliases() {
            match &default_value {
                FixedValue::Bool(_) => p == Primitive::Bool,
                FixedValue::Nint(_) => p.cbor_types().contains(&CBORType::NegativeInteger),
                FixedValue::Uint(_) => p.cbor_types().contains(&CBORType::UnsignedInteger),
                FixedValue::Null => false,
                FixedValue::Text(_) => p == Primitive::Str,
            }
        } else {
            false
        };
        if !matches {
            panic!(".default {:?} invalid for type {:?}", default_value, self.conceptual_type);
        }
        self.default = Some(default_value);
        self
    }

    pub fn as_bytes(mut self) -> Self {
        self.encodings.push(CBOREncodingOperation::CBORBytes);
        self
    }

    pub fn resolve_aliases(self) -> Self {
        Self {
            conceptual_type: self.conceptual_type.resolve_aliases(),
            encodings: self.encodings,
            default: self.default,
        }
    }

    /// All POSSIBLE outermost CBOR types this can encode to
    pub fn cbor_types(&self) -> Vec<CBORType> {
        match self.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_)) => vec![CBORType::Tag],
            Some(CBOREncodingOperation::CBORBytes) => vec![CBORType::Bytes],
            None => match &self.conceptual_type {
                ConceptualRustType::Fixed(f) => vec![match f {
                    FixedValue::Uint(_) => CBORType::UnsignedInteger,
                    FixedValue::Nint(_) => CBORType::NegativeInteger,
                    FixedValue::Text(_) => CBORType::Text,
                    FixedValue::Null => CBORType::Special,
                    FixedValue::Bool(_) => CBORType::Special,
                }],
                ConceptualRustType::Primitive(p) => p.cbor_types(),
                ConceptualRustType::Rust(_ident) => {
                    //panic!("TODO: store first cbor tag somewhere")
                    vec![CBORType::Array, CBORType::Map]
                },
                ConceptualRustType::Array(_) => vec![CBORType::Array],
                ConceptualRustType::Map(_k, _v) => vec![CBORType::Map],
                ConceptualRustType::Optional(ty) => {
                    let mut inner_types = ty.cbor_types();
                    if !inner_types.contains(&CBORType::Special) {
                        inner_types.push(CBORType::Special);
                    }
                    inner_types
                },
                ConceptualRustType::Alias(_ident, ty) => Self::new((**ty).clone()).cbor_types(),
            }
        }
    }

    fn _cbor_special_type(&self) -> Option<CBORSpecial> {
        unimplemented!()
    }

    fn _is_serialize_multiline(&self) -> bool {
        if self.encodings.is_empty() {
            match &self.conceptual_type {
                ConceptualRustType::Fixed(_) => false,
                ConceptualRustType::Primitive(_) => false,
                ConceptualRustType::Rust(_) => false,
                ConceptualRustType::Array(_) => true,
                ConceptualRustType::Optional(_) => false,
                ConceptualRustType::Map(_, _) => false,
                ConceptualRustType::Alias(_ident, ty) => Self::new((**ty).clone())._is_serialize_multiline(),
            }
        } else {
            true
        }
    }
}

impl std::convert::From<ConceptualRustType> for RustType {
    fn from(conceptual_type: ConceptualRustType) -> Self {
        Self::new(conceptual_type)
    }
}

/// How a type will be represented in rust outside of a serialization context
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ConceptualRustType {
    Fixed(FixedValue),
    // Primitive type that can be passed to/from wasm
    Primitive(Primitive),
    // Rust-defined type that cannot be put in arrays/etc
    Rust(RustIdent),
    // Array-wrapped type. Passed as Vec<T> if T is Primitive
    Array(Box<RustType>),
    // T / null in CDDL - auto-converts to Option<T> in rust for ease of use.
    Optional(Box<RustType>),
    // TODO: table type to support inlined defined table-type groups as fields
    Map(Box<RustType>, Box<RustType>),
    // Alias for another type
    Alias(AliasIdent, Box<ConceptualRustType>),

    // TODO: for non-table-type ones we could define a RustField(Ident, RustType) and then
    // a variant here Struct(Vec<RustField>) and delegate field/argument generation to
    // RustField so that we could basically expand them and not care about having to generate
    // and intermediate fields - although this could pose an issue for optional types... so maybe
    // another approach would be necessary.
}

impl ConceptualRustType {
    pub fn resolve_aliases(self) -> Self {
        match self {
            Self::Array(ty) => Self::Array(Box::new(ty.resolve_aliases())),
            Self::Alias(_, ty) => ty.resolve_aliases(),
            Self::Map(key, value) => Self::Map(Box::new(key.resolve_aliases()), Box::new(value.resolve_aliases())),
            Self::Optional(ty) => Self::Optional(Box::new(ty.resolve_aliases())),
            _ => self,
        }
    }

    pub fn directly_wasm_exposable(&self) -> bool {
        match self {
            Self::Fixed(_) => false,
            Self::Primitive(_) => true,
            Self::Rust(_) => false,
            // wasm_bindgen doesn't support nested vecs, even if the inner vec would be supported
            Self::Array(ty) => {
                let inner = match &ty.conceptual_type {
                    Self::Alias(_ident, ty) => &*ty,
                    Self::Optional(ty) => &ty.conceptual_type,
                    ty => ty,
                };
                match inner {
                    Self::Primitive(p) => match p {
                        // converts to js number which is supported as Vec<T>
                        Primitive::Bool |
                        Primitive::I8 |
                        Primitive::U8 |
                        Primitive::I16 |
                        Primitive::U16 |
                        Primitive::I32 |
                        Primitive::U32 |
                        Primitive::I64 |
                        Primitive::N64 |
                        Primitive::U64 => true,
                        // Bytes is already implemented as Vec<u8> so we can't nest it
                        Primitive::Bytes => false,
                        // Vec<String> is not supported by wasm-bindgen
                        Primitive::Str => false,
                    },
                    Self::Array(_) => false,
                    _ => ty.conceptual_type.directly_wasm_exposable(),
                }
            },
            Self::Optional(ty) => ty.conceptual_type.directly_wasm_exposable(),
            Self::Map(_, _) => false,
            Self::Alias(_ident, ty) => ty.directly_wasm_exposable(),
        }
    }

    pub fn is_fixed_value(&self) -> bool {
        match self {
            Self::Fixed(_) => true,
            Self::Alias(_ident, ty) => ty.is_fixed_value(),
            _ => false,
        }
    }

    pub fn name_as_wasm_array(&self) -> String {
        if Self::Array(Box::new(self.clone().into())).directly_wasm_exposable() {
            format!("Vec<{}>", self.for_wasm_member())
        } else {
            format!("{}s", self.for_variant())
        }
    }

    pub fn name_as_rust_array(&self, from_wasm: bool) -> String {
        format!("Vec<{}>", self.for_rust_member(from_wasm))
    }

    /// Function parameter TYPE that will be moved in
    pub fn for_rust_move(&self) -> String {
        self.for_rust_member(false)
    }

    /// Function parameter TYPE by-non-mut-reference for read-only
    pub fn _for_rust_read(&self) -> String {
        match self {
            Self::Fixed(_) => panic!("should not expose Fixed type, only here for serialization: {:?}", self),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => format!("&{}", ident),
            Self::Array(ty) => format!("&{}", ty.conceptual_type.name_as_rust_array(false)),
            Self::Optional(ty) => format!("Option<{}>", ty.conceptual_type._for_rust_read()),
            Self::Map(_k, _v) => format!("&{}", self.for_rust_member(false)),
            Self::Alias(ident, ty) => match &**ty {
                // TODO: ???
                Self::Rust(_) => format!("&{}", ident),
                _ => ident.to_string(),
            },
        }
    }

    /// Function parameter TYPE from wasm (i.e. ref for non-primitives, value for supported primitives)
    pub fn for_wasm_param(&self) -> String {
        self.for_wasm_param_impl(false)
    }

    fn for_wasm_param_impl(&self, force_not_ref: bool) -> String {
        let opt_ref = if force_not_ref {
            ""
        } else {
            "&"
        };
        match self {
            Self::Fixed(_) => panic!("should not expose Fixed type to wasm, only here for serialization: {:?}", self),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => format!("{}{}", opt_ref, ident),
            Self::Array(ty) => if self.directly_wasm_exposable() {
                ty.conceptual_type.name_as_wasm_array()
            } else {
                format!("{}{}", opt_ref, ty.conceptual_type.name_as_wasm_array())
            },
            Self::Optional(ty) => format!("Option<{}>", ty.conceptual_type.for_wasm_param_impl(true)),
            Self::Map(_k, _v) => format!("{}{}", opt_ref, self.for_wasm_member()),
            // it might not be worth generating this as aliases are ignored by wasm-pack build, but
            // that could change in the future so as long as it doens't cause issues we'll leave it
            Self::Alias(ident, ty) => match &**ty {
                Self::Rust(_) | 
                Self::Array(_) |
                Self::Map(_, _) if !self.directly_wasm_exposable() => format!("{}{}", opt_ref, ident),
                Self::Optional(_) |
                // no special handling if for some reason nested aliases, just strip all to avoid hassle
                Self::Alias(_, _) => ty.for_wasm_param_impl(force_not_ref),
                _ => ident.to_string(),
            }
        }
    }

    /// Return TYPE for wasm
    pub fn for_wasm_return(&self) -> String {
        self.for_wasm_member()
    }

    pub fn name_for_wasm_map(k: &RustType, v: &RustType) -> RustIdent {
        RustIdent::new(CDDLIdent::new(format!("Map{}To{}", k.conceptual_type.for_variant(), v.conceptual_type.for_variant())))
    }

    pub fn name_for_rust_map(k: &RustType, v: &RustType, from_wasm: bool) -> String {
        format!("{}<{}, {}>", table_type(), k.conceptual_type.for_rust_member(from_wasm), v.conceptual_type.for_rust_member(from_wasm))
    }

    /// If we were to store a value directly in a wasm-wrapper, this would be used.
    pub fn for_wasm_member(&self) -> String {
        match self {
            Self::Fixed(_) => panic!("should not expose Fixed type in member, only needed for serializaiton: {:?}", self),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => ident.to_string(),
            Self::Array(ty) => ty.conceptual_type.name_as_wasm_array(),
            Self::Optional(ty) => format!("Option<{}>", ty.conceptual_type.for_wasm_member()),
            Self::Map(k, v) => Self::name_for_wasm_map(k, v).to_string(),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_wasm_member(),
                // but other aliases are generated and should be used.
                AliasIdent::Rust(_) => ident.to_string(),
            },
        }
    }

    /// Type when storing a value inside of a rust struct. This is the underlying raw representation.
    pub fn for_rust_member(&self, from_wasm: bool) -> String {
        let core = if from_wasm {
            "core::"
        } else {
            ""
        };
        match self {
            Self::Fixed(_) => panic!("should not expose Fixed type in member, only needed for serializaiton: {:?}", self),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => format!("{}{}", core, ident),
            Self::Array(ty) => ty.conceptual_type.name_as_rust_array(from_wasm),
            Self::Optional(ty) => format!("Option<{}>", ty.conceptual_type.for_rust_member(from_wasm)),
            Self::Map(k, v) => Self::name_for_rust_map(k, v, from_wasm),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_rust_member(from_wasm),
                // but other aliases are generated and should be used.
                AliasIdent::Rust(_) => format!("{}{}", core, ident),
            },
        }
    }

    /// IDENTIFIER for an enum variant. (Use for_rust_member() for the enum value)
    pub fn for_variant(&self) -> VariantIdent {
        match self {
            Self::Fixed(f) => f.for_variant(),
            Self::Primitive(p) => p.to_variant(),
            Self::Rust(ident) => VariantIdent::new_rust(ident.clone()),
            Self::Array(inner) => VariantIdent::new_custom(format!("Arr{}", inner.conceptual_type.for_variant())),
            // TODO: should we not end up in this situation and just insert a Null fixed value instead?
            Self::Optional(ty) => VariantIdent::new_custom(format!("Opt{}", ty.conceptual_type.for_variant())),
            Self::Map(k, v) => VariantIdent::new_custom(Self::name_for_wasm_map(k, v).to_string()),
            Self::Alias(ident, _ty) => match ident {
                AliasIdent::Rust(rust_ident) => VariantIdent::new_rust(rust_ident.clone()),
                AliasIdent::Reserved(reserved) => VariantIdent::new_custom(reserved),
            },
        }
    }

    /// for parameter TYPES from wasm that take ownership (via cloning here)
    /// can_fail is for cases where checks (e.g. range checks) are done if there
    /// is a type transformation (i.e. wrapper types) like text (wasm) -> #6.14(text) (rust)
    pub fn from_wasm_boundary_clone(&self, expr: &str, can_fail: bool) -> Vec<ToWasmBoundaryOperations> {
        let expr_cloned = if self.is_copy() {
            expr.to_owned()
        } else {
            format!("{}.clone()", expr)
        };
        let mut ops = match self {
            Self::Rust(_ident) => vec![
                ToWasmBoundaryOperations::Code(expr_cloned),
                ToWasmBoundaryOperations::Into,
            ],
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_clone(expr, can_fail),
            Self::Optional(ty) => ty.conceptual_type.from_wasm_boundary_clone_optional(expr, can_fail),
            Self::Array(ty) => if self.directly_wasm_exposable() {
                ty.conceptual_type.from_wasm_boundary_clone(expr, can_fail)
            } else {
                vec![
                    ToWasmBoundaryOperations::Code(expr_cloned),
                    ToWasmBoundaryOperations::Into,
                ]
            },
            Self::Map(_k, _v) => vec![
                ToWasmBoundaryOperations::Code(expr_cloned),
                ToWasmBoundaryOperations::Into,
            ],
            _ => vec![ToWasmBoundaryOperations::Code(expr.to_owned())],
        };
        if can_fail {
            ops.push(ToWasmBoundaryOperations::TryInto);
        }
        ops
    }

    fn from_wasm_boundary_clone_optional(&self, expr: &str, can_fail: bool) -> Vec<ToWasmBoundaryOperations> {
        let mut ops = match self {
            Self::Primitive(_p) => vec![ToWasmBoundaryOperations::Code(expr.to_owned())],
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_clone_optional(expr, can_fail),
            Self::Array(..) |
            Self::Rust(..) |
            Self::Map(..) => vec![
                ToWasmBoundaryOperations::Code(expr.to_owned()),
                if can_fail {
                    ToWasmBoundaryOperations::MapTryInto
                } else {
                    ToWasmBoundaryOperations::MapInto
                },
            ],
            _ => panic!("unsupported or unexpected"),
        };
        if can_fail {
            ops.push(ToWasmBoundaryOperations::TryInto);
        }
        ops
    }

    /// for non-owning parameter TYPES from wasm
    pub fn from_wasm_boundary_ref(&self, expr: &str) -> String {
        match self {
            Self::Rust(_ident) => expr.to_owned(),
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_ref(expr),
            Self::Optional(ty) => ty.conceptual_type.from_wasm_boundary_ref(expr),
            Self::Array(ty) => if self.directly_wasm_exposable() {
                ty.conceptual_type.from_wasm_boundary_ref(expr)
            } else {
                expr.to_owned()
            },
            Self::Map(_k, _v) => expr.to_owned(),
            _ => format!("&{}", expr),
        }
    }

    /// FROM rust TO wasm (with cloning/wrapping) (for arguments)
    pub fn to_wasm_boundary(&self, expr: &str, is_ref: bool) -> String {
        match self {
            Self::Fixed(_) => panic!("fixed types are a serialization detail"),
            Self::Primitive(_p) => if self.is_copy() {
                if is_ref {
                    format!("*{}", expr)
                } else {
                    expr.to_owned()
                }
            } else {
                format!("{}.clone()", expr)
            },
            Self::Rust(_ident) => format!("{}.clone().into()", expr),
            //Self::Array(ty) => format!("{}({}.clone())", ty.name_as_wasm_array(), expr),
            //Self::Map(k, v) => format!("{}({}.clone())", Self::name_for_wasm_map(k, v), expr),
            Self::Array(_ty) => if self.directly_wasm_exposable() {
                format!("{}.clone()", expr)
            } else {
                format!("{}.clone().into()", expr)
            },
            Self::Map(_k, _v) => format!("{}.clone().into()", expr),
            Self::Optional(ty) => ty.conceptual_type.to_wasm_boundary_optional(expr, is_ref),
            Self::Alias(_ident, ty) => ty.to_wasm_boundary(expr, is_ref),
        }
    }

    /// FROM rust TO wasm as Option<T>. This is separate as we can have optional fields
    /// that act identical to Self::Optional(ty)
    pub fn to_wasm_boundary_optional(&self, expr: &str, is_ref: bool) -> String {
        if self.directly_wasm_exposable() {
            self.to_wasm_boundary(expr, is_ref)
        } else {
            format!("{}.clone().map(std::convert::Into::into)", expr)
        }
    }

    // if it impements the Copy trait in rust
    pub fn is_copy(&self) -> bool {
        match self {
            Self::Fixed(_f) => unreachable!(),
            Self::Primitive(p) => match p {
                Primitive::Bool |
                Primitive::I8 |
                Primitive::I16 |
                Primitive::I32 |
                Primitive::I64 |
                Primitive::N64 |
                Primitive::U8 |
                Primitive::U16 |
                Primitive::U32 |
                Primitive::U64 => true,
                Primitive::Str |
                Primitive::Bytes => false,
            },
            Self::Rust(_ident) => false,
            Self::Array(_) => false,
            Self::Map(_k, _v) => false,
            Self::Optional(ty) => ty.conceptual_type.is_copy(),
            Self::Alias(_ident, ty) => ty.is_copy(),
        }
    }

    pub fn clone_if_not_copy(&self, expr: &str) -> String {
        if self.is_copy() {
            expr.to_owned()
        } else {
            format!("{}.clone()", expr)
        }
    }

    // CBOR len count for the entire type if it were embedded as a member in a cbor collection (array/map)
    pub fn expanded_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        match self {
            Self::Optional(ty) => match ty.conceptual_type.expanded_field_count(types) {
                Some(1) => Some(1),
                // differing sizes when Null vs Some
                _ => None,
            },
            // TODO: instead of returning None when ident doesn't exist, throw error.
            // Once we split up parsing and codegen this shouldn't happen but with our current multi-pass
            // approach we might have out of order struct references which would break here without it
            // but on the final pass (the one we export) this should't be an issue
            Self::Rust(ident) => if types.is_plain_group(ident) {
                types.rust_structs.get(&ident)?.fixed_field_count(types)
            } else {
                Some(1)
            },
            Self::Alias(_ident, ty) => ty.expanded_field_count(types),
            _ => Some(1),
        }
    }

    // See comment in RustStruct::definite_info(), this is the same, returns a string expression
    // which evaluates to the length.
    // self_expr is an expression that evaluates to this RustType (e.g. member, etc) at the point where
    // the return of this function will be used.
    pub fn definite_info(&self, self_expr: &str, types: &IntermediateTypes) -> String {
        match self.expanded_field_count(types) {
            Some(count) => count.to_string(),
            None => match self {
                Self::Optional(ty) => format!("match {} {{ Some(x) => {}, None => 1 }}", self_expr, ty.conceptual_type.definite_info("x", types)),
                Self::Rust(ident) => if types.is_plain_group(ident) {
                    match types.rust_structs.get(&ident) {
                        Some(rs) => rs.definite_info(types),
                        None => panic!("rust struct {} not found but referenced by {:?}", ident, self),
                    }
                } else {
                    String::from("1")
                },
                Self::Alias(_ident, ty) => ty.definite_info(self_expr, types),
                _ => String::from("1"),
            }
        }
    }

    // the minimum cbor length of this struct - can be useful for deserialization length checks
    // does not count ANY type choice like types including Optional UNLESS the option Some type
    // has cbor len 1 too - to be consistent with expanded_field_count
    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        match self {
            Self::Optional(ty) => match ty.conceptual_type.expanded_field_count(types) {
                Some(1) => 1,
                _ => 0,
            },
            Self::Rust(ident) => if types.is_plain_group(ident) {
                println!("ident: {}", ident);
                match types.rust_structs.get(&ident) {
                    Some(x) => x.expanded_mandatory_field_count(types),
                    None => panic!("could not find ident: {}", ident),
                }
                //types.rust_structs.get(&ident).unwrap().expanded_mandatory_field_count(types)
            } else {
                1
            },
            Self::Alias(_ident, ty) => ty.expanded_mandatory_field_count(types),
            _ => 1,
        }
    }

    pub fn visit_types<F: FnMut(&Self)>(&self, types: &IntermediateTypes, f: &mut F) {
        self.visit_types_excluding(types, f, &mut BTreeSet::new())
    }

    pub fn visit_types_excluding<F: FnMut(&Self)>(&self, types: &IntermediateTypes, f: &mut F, already_visited: &mut BTreeSet<RustIdent>) {
        f(self);
        match self {
            Self::Alias(ident, ty) => {
                match ident {
                    AliasIdent::Rust(rust_ident) => {
                        if already_visited.insert(rust_ident.clone()) {
                            ty.visit_types_excluding(types, f, already_visited)
                        }
                    },
                    _ => ty.visit_types_excluding(types, f, already_visited)
                };
            },
            Self::Array(ty) => ty.conceptual_type.visit_types_excluding(types, f, already_visited),
            Self::Fixed(_) => (),
            Self::Map(k, v) => {
                k.conceptual_type.visit_types_excluding(types, f, already_visited);
                v.conceptual_type.visit_types_excluding(types, f, already_visited);
            },
            Self::Optional(ty) => ty.conceptual_type.visit_types_excluding(types, f, already_visited),
            Self::Primitive(_) => (),
            Self::Rust(ident) => {
                if already_visited.insert(ident.clone()) {
                    types.rust_struct(ident).map(|t| t.visit_types_excluding(types, f, already_visited));
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum ToWasmBoundaryOperations {
    Code(String),
    Into,
    TryInto,
    MapInto,
    MapTryInto,
}

impl ToWasmBoundaryOperations {
    /// Returns Some(NewOp) if self + next can be merged into a single step, otherwise None
    fn merge(&self, next: &Self) -> Option<Self> {
        match self {
            Self::Code(_) => None,
            Self::Into => match next {
                Self::Code(_) => None,
                next => Some(next.clone()),
            },
            Self::TryInto => match next {
                Self::Code(_) => None,
                Self::Into |
                Self::TryInto => Some(Self::TryInto),
                Self::MapInto |
                Self::MapTryInto => Some(Self::MapTryInto),
            },
            Self::MapInto => match next {
                Self::Code(_) => None,
                Self::Into |
                Self::MapInto => Some(Self::MapInto),
                Self::TryInto |
                Self::MapTryInto => Some(Self::MapTryInto),
            },
            Self::MapTryInto => match next {
                Self::Code(_) => None,
                _ => Some(Self::MapTryInto),
            },
        }
    }

    pub fn format(mut operations: impl Iterator<Item = Self>) -> String {
        use std::fmt::Write;
        let mut buf = String::new();
        let mut current: Option<Self> = None;
        while let Some(to_apply) = operations.next() {
            match current {
                Some(c) => {
                    match c.merge(&to_apply) {
                        Some(merged) => {
                            current = Some(merged);
                        },
                        None => {
                            write!(buf, "{}", c).unwrap();
                            current = Some(to_apply);
                        },
                    }
                },
                None => {
                    current = Some(to_apply);
                },
            }
        }
        if let Some(c) = current {
            write!(buf, "{}", c).unwrap();
        }
        buf
    }
}

impl std::fmt::Display for ToWasmBoundaryOperations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Code(code) => write!(f, "{}", code),
            Self::Into => write!(f, ".into()"),
            Self::TryInto => write!(f, ".try_into()"),
            Self::MapInto => write!(f, ".map(Into::into)"),
            Self::MapTryInto => write!(f, ".map(TryInto::try_into)"),
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

    pub fn name_as_var(&self) -> String {
        let snake = convert_to_snake_case(&self.name.to_string());
        // we can't use (rust) reserved keywords as param: eg new_u32(u32: u32)
        // TODO: do we need to cover any other (rust) reserved keywords?
        String::from(match snake.as_str() {
            "u8" | "u16" | "u32" | "u64" => "uint",
            "i8" | "i16" | "i32" | "i64" => "int",
            x => x,
        })
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
    Wrapper{
        wrapped: RustType,
        min_max: Option<(Option<i128>, Option<i128>)>,
    },
    /// This is a no-op in generation but to prevent lookups of things in the prelude
    /// e.g. `int` from not being resolved while still being able to detect it when
    /// referring to a struct that doesn't exist even after generation.
    Prelude,
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

    pub fn new_wrapper(ident: RustIdent, tag: Option<usize>, wrapped_type: RustType, min_max: Option<(Option<i128>, Option<i128>)>) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Wrapper {
                wrapped: wrapped_type,
                min_max,
            },
        }
    }

    pub fn new_prelude(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
            variant: RustStructType::Prelude,
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
            RustStructType::Wrapper{ .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Prelude{ .. } => panic!("do we need to look this up ever? will the prelude have structs with fields?"),
        }
    }

    // Even if fixed_field_count() == None, this will return an expression for
    // a definite length, e.g. with optional field checks in the expression
    // This is useful for definite-length serialization
    pub fn definite_info(&self, types: &IntermediateTypes) -> String {
        match &self.variant {
            RustStructType::Record(record) => record.definite_info(types),
            RustStructType::Table{ .. } => String::from("self.0.len() as u64"),
            RustStructType::Array{ .. } => String::from("self.0.len() as u64"),
            RustStructType::TypeChoice{ .. } => unreachable!("I don't think type choices should be using length?"),
            RustStructType::GroupChoice{ .. } => unreachable!("I don't think group choices should be using length?"),
            RustStructType::Wrapper{ .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Prelude{ .. } => panic!("do we need to look this up ever? will the prelude have structs with fields?"),
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
            RustStructType::Wrapper{ .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Prelude{ .. } => panic!("do we need to look this up ever? will the prelude have structs with fields?"),
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
            RustStructType::Wrapper{ .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Prelude{ .. } => panic!("do we need to look this up ever? will the prelude have structs with fields?"),
        }
    }

    pub fn visit_types<F: FnMut(&ConceptualRustType)>(&self, types: &IntermediateTypes, f: &mut F) {
        self.visit_types_excluding(types, f, &mut BTreeSet::new())
    }
    pub fn visit_types_excluding<F: FnMut(&ConceptualRustType)>(&self, types: &IntermediateTypes, f: &mut F, already_visited: &mut BTreeSet<RustIdent>) {
        match &self.variant {
            RustStructType::Array{ element_type } => element_type.conceptual_type.visit_types_excluding(types, f, already_visited),
            RustStructType::GroupChoice{ variants, .. } |
            RustStructType::TypeChoice{ variants, .. } => variants.iter().for_each(|v| v.rust_type.conceptual_type.visit_types_excluding(types, f, already_visited)),
            RustStructType::Record(record) => record.fields.iter().for_each(|field| field.rust_type.conceptual_type.visit_types_excluding(types, f, already_visited)),
            RustStructType::Table{domain, range} => {
                domain.conceptual_type.visit_types_excluding(types, f, already_visited);
                range.conceptual_type.visit_types_excluding(types, f, already_visited);
            },
            RustStructType::Wrapper{ wrapped, .. } => wrapped.conceptual_type.visit_types_excluding(types, f, already_visited),
            RustStructType::Prelude => (),
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
            count += match self.rep {
                Representation::Array => field.rust_type.conceptual_type.expanded_field_count(types)?,
                Representation::Map => 1,
            };
        }
        Some(count)
    }

    // This is guaranteed 
    pub fn definite_info(&self, types: &IntermediateTypes) -> String {
        match self.fixed_field_count(types) {
            Some(count) => count.to_string(),
            None => {
                let mut fixed_field_count = 0;
                let mut conditional_field_expr = String::new();
                for field in &self.fields {
                    if field.optional {
                        if !conditional_field_expr.is_empty() {
                            conditional_field_expr.push_str(" + ");
                        }
                        let (field_expr, field_contribution) = match self.rep {
                            Representation::Array => ("x", field.rust_type.conceptual_type.definite_info("x", types)),
                            // maps are defined by their keys instead (although they shouldn't have multi-length values either...)
                            Representation::Map => ("_", String::from("1")),
                        };
                        if let Some(default_value) = &field.rust_type.default {
                            if CLI_ARGS.preserve_encodings {
                                conditional_field_expr.push_str(&format!(
                                    "if self.{} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false) {{ {} }} else {{ 0 }}",
                                    field.name,
                                    default_value.to_primitive_str_compare(),
                                    field.name,
                                    field_contribution));
                            } else {
                                conditional_field_expr.push_str(&format!(
                                    "if self.{} != {} {{ {} }} else {{ 0 }}",
                                    field.name,
                                    default_value.to_primitive_str_compare(),
                                    field_contribution));
                            }
                        } else {
                            conditional_field_expr.push_str(&format!("match &self.{} {{ Some({}) => {}, None => 0 }}", field.name, field_expr, field_contribution));
                        }
                    } else {
                        match self.rep {
                            Representation::Array => match field.rust_type.conceptual_type.expanded_field_count(types) {
                                Some(field_expanded_count) => fixed_field_count += field_expanded_count,
                                None => {
                                    if !conditional_field_expr.is_empty() {
                                        conditional_field_expr.push_str(" + ");
                                    }
                                    let field_len_expr = field.rust_type.conceptual_type.definite_info(&format!("self.{}", field.name), types);
                                    conditional_field_expr.push_str(&field_len_expr);
                                },
                            },
                            Representation::Map => {
                                fixed_field_count += 1;
                            },
                        };
                    }
                }
                if conditional_field_expr.is_empty() || fixed_field_count != 0 {
                    format!("{} + {}", fixed_field_count.to_string(), conditional_field_expr)
                } else {
                    conditional_field_expr
                }
            }
        }
    }

    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        self.fields.iter().filter(|field| !field.optional).map(|field| field.rust_type.conceptual_type.expanded_mandatory_field_count(types)).sum()
    }

    pub fn cbor_len_info(&self, types: &IntermediateTypes) -> RustStructCBORLen {
        match self.fixed_field_count(types) {
            Some(fixed_count) => RustStructCBORLen::Fixed(fixed_count),
            None => RustStructCBORLen::OptionalFields(self.expanded_mandatory_field_count(types)),
        }
    }

    pub fn canonical_ordering<'a>(&'a self) -> Vec<(usize, &'a RustField)> {
        let mut fields: Vec<(usize, &'a RustField)> = self.fields.iter().enumerate().collect();
        if self.rep == Representation::Map {
            fields.sort_by(|lhs, rhs| {
                let lhs_bytes = lhs.1.key.as_ref().unwrap().to_bytes();
                let rhs_bytes = rhs.1.key.as_ref().unwrap().to_bytes();
                match lhs_bytes.len().cmp(&rhs_bytes.len()) {
                    std::cmp::Ordering::Equal => lhs_bytes.cmp(&rhs_bytes),
                    diff_ord => diff_ord,
                }
            });
        }
        fields
    }
}

// definition of a generic type e.g. foo<T, U> = [x: T, y: U]
#[derive(Debug)]
pub struct GenericDef {
    generic_params: Vec<RustIdent>,
    orig: RustStruct,
}

impl GenericDef {
    pub fn new(generic_params: Vec<RustIdent>, orig: RustStruct) -> Self {
        Self {
            generic_params,
            orig,
        }
    }
}

// invocation of a generic definition e.g. foo = bar<text>
#[derive(Debug)]
pub struct GenericInstance {
    instance_ident: RustIdent,
    generic_ident: RustIdent,
    generic_args: Vec<RustType>,
}

impl GenericInstance {
    pub fn new(instance_ident: RustIdent, generic_ident: RustIdent, generic_args: Vec<RustType>) -> Self {
        Self {
            instance_ident,
            generic_ident,
            generic_args,
        }
    }

    // TODO: should we rename fields / variant names after-the-fact?
    // (for the cases where the name came from the original generic param)
    pub fn resolve(&self, types: &IntermediateTypes) -> RustStruct {
        let def = match types.generic_defs.get(&self.generic_ident) {
            Some(def) => def,
            None => panic!("Generic instance used on {} without definition", self.generic_ident),
        };
        assert_eq!(def.generic_params.len(), self.generic_args.len());
        let resolved_args = def
            .generic_params
            .iter()
            .zip(self.generic_args.iter())
            .collect::<BTreeMap::<&RustIdent, &RustType>>();
        let mut instance = def.orig.clone();
        instance.ident = self.instance_ident.clone();
        
        match &mut instance.variant {
            RustStructType::Record(record) => {
                for field in record.fields.iter_mut() {
                    field.rust_type = Self::resolve_type(&resolved_args, &field.rust_type);
                }
            },
            RustStructType::Table{ domain, range } => {
                *domain = Self::resolve_type(&resolved_args, domain);
                *range = Self::resolve_type(&resolved_args, range);
            },
            RustStructType::Array{ element_type } => {
                *element_type = Self::resolve_type(&resolved_args, element_type);
            },
            RustStructType::TypeChoice{ variants } => {
                for variant in variants.iter_mut() {
                    variant.rust_type = Self::resolve_type(&resolved_args, &variant.rust_type);
                }
            },
            RustStructType::GroupChoice{ .. } => {
                // for variant in variants.mut_iter() {
                //     variant.rust_type = Self::resolve_type(&resolved_args, &variant.rust_type);
                // }
                todo!("we might need to recursively resolve on these");
            },
            RustStructType::Wrapper{ .. } => {
                todo!("should we look this up in types to resolve?");
            },
            RustStructType::Prelude => panic!("generics should not be used on types in the prelude (e.g. int)"),
        };
        instance
    }

    fn resolve_type(args: &BTreeMap<&RustIdent, &RustType>, orig: &RustType) -> RustType {
        if let ConceptualRustType::Rust(ident) = &orig.conceptual_type {
            if let Some(resolved_type) = args.get(ident) {
                return (*resolved_type).clone();
            }
        }
        orig.clone()
    }
}

fn try_ident_with_id(intermediate_types: &IntermediateTypes, name: &CDDLIdent, value: u32) -> CDDLIdent {
    let new_ident = CDDLIdent::new(format!("{}{}", name, value));
    let rust_ident = RustIdent::new(new_ident.clone());
    match intermediate_types.has_ident(&rust_ident) {
        false => new_ident,
        true => try_ident_with_id(intermediate_types, name, value + 1)
    }
}
