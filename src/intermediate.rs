use cbor_event::Special as CBORSpecial;
use cbor_event::{Special, Type as CBORType};
use cddl::ast::parent::ParentVisitor;
use std::collections::{BTreeMap, BTreeSet};

// TODO: move all of these generation specifics into generation.rs
use crate::generation::table_type;
use crate::utils::{
    cddl_prelude, convert_to_camel_case, convert_to_snake_case, is_identifier_reserved,
    is_identifier_user_defined,
};

use once_cell::sync::Lazy;
pub static ROOT_SCOPE: Lazy<ModuleScope> = Lazy::new(|| vec![String::from("lib")].into());

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ModuleScope(Vec<String>);

impl ModuleScope {
    pub fn new(scope: Vec<String>) -> Self {
        Self::from(scope)
    }

    pub fn components(&self) -> &Vec<String> {
        &self.0
    }
}

impl From<Vec<String>> for ModuleScope {
    fn from(scope: Vec<String>) -> Self {
        Self(scope)
    }
}

impl std::fmt::Display for ModuleScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

#[derive(Debug)]
pub struct AliasInfo {
    pub base_type: RustType,
    pub gen_rust_alias: bool,
    pub gen_wasm_alias: bool,
}

impl AliasInfo {
    pub fn new(base_type: RustType, gen_rust_alias: bool, gen_wasm_alias: bool) -> Self {
        Self {
            base_type,
            gen_rust_alias,
            gen_wasm_alias,
        }
    }
}

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
    type_aliases: BTreeMap<AliasIdent, AliasInfo>,
    rust_structs: BTreeMap<RustIdent, RustStruct>,
    prelude_to_emit: BTreeSet<String>,
    generic_defs: BTreeMap<RustIdent, GenericDef>,
    generic_instances: BTreeMap<RustIdent, GenericInstance>,
    news_can_fail: BTreeSet<RustIdent>,
    used_as_key: BTreeSet<RustIdent>,
    scopes: BTreeMap<RustIdent, ModuleScope>,
    // for scope() to work we keep this here.
    // Returning a reference to the const ROOT_SCOPE complains of returning a temporary
    root_scope: ModuleScope,
}

impl Default for IntermediateTypes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> IntermediateTypes<'a> {
    pub fn new() -> Self {
        let mut rust_structs = BTreeMap::new();
        rust_structs.insert(
            RustIdent::new(CDDLIdent::new("int")),
            RustStruct::new_extern(RustIdent::new(CDDLIdent::new("int"))),
        );
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
            root_scope: ROOT_SCOPE.clone(),
        }
    }

    #[allow(unused)]
    pub fn has_ident(&self, ident: &RustIdent) -> bool {
        let idents: Vec<RustIdent> = self.type_aliases.keys().fold(vec![], |mut acc, alias| {
            match alias {
                AliasIdent::Reserved(_) => {}
                AliasIdent::Rust(ident) => acc.push(ident.clone()),
            };
            acc
        });
        println!(
            "{:?}",
            self.plain_groups
                .keys()
                .chain(idents.iter())
                .chain(self.rust_structs.keys())
                .chain(self.generic_defs.keys())
                .chain(self.generic_instances.keys())
        );
        self.plain_groups.contains_key(ident)
            || self
                .type_aliases
                .contains_key(&AliasIdent::Rust(ident.clone()))
            || self.rust_structs.contains_key(ident)
            || self.generic_defs.contains_key(ident)
            || self.generic_instances.contains_key(ident)
    }

    pub fn type_aliases(&self) -> &BTreeMap<AliasIdent, AliasInfo> {
        &self.type_aliases
    }

    pub fn rust_structs(&self) -> &BTreeMap<RustIdent, RustStruct> {
        &self.rust_structs
    }

    /// For each scope, which other scopes are referenced, and which structs are referenced
    pub fn scope_references(
        &self,
        wasm: bool,
    ) -> BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>> {
        // we only want to mark TOP-LEVEL references without recursing into those types
        // which is why we don't use visit_types() here
        let mut refs = BTreeMap::new();
        fn set_ref(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            current_scope: &ModuleScope,
            rust_ident: &RustIdent,
        ) {
            let ref_scope = types.scope(rust_ident);
            if current_scope != ref_scope {
                refs.entry(current_scope.clone())
                    .or_default()
                    .entry(ref_scope.clone())
                    .or_default()
                    .insert(rust_ident.clone());
            }
        }
        fn mark_refs(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            wasm: bool,
            current_scope: &ModuleScope,
            ty: &RustType,
        ) {
            match &ty.conceptual_type {
                ConceptualRustType::Alias(alias_ident, _alias_ty) => {
                    if let AliasIdent::Rust(rust_ident) = alias_ident {
                        set_ref(refs, types, current_scope, rust_ident);
                    }
                }
                ConceptualRustType::Rust(rust_ident) => {
                    set_ref(refs, types, current_scope, rust_ident)
                }
                ConceptualRustType::Array(elem_ty) => {
                    if wasm
                        && !elem_ty.directly_wasm_exposable(types)
                        && *current_scope != *ROOT_SCOPE
                    {
                        // TODO: we should be doing array wrappers where they are declared or used,
                        // but for the latter, what to do if multiple places use it? default to lib?
                        // issue: https://github.com/dcSpark/cddl-codegen/issues/138
                        let arr_wrapper_ident =
                            RustIdent::new(CDDLIdent::new(elem_ty.name_as_wasm_array(types)));
                        refs.entry(current_scope.to_owned())
                            .or_default()
                            .entry(ROOT_SCOPE.clone())
                            .or_default()
                            .insert(arr_wrapper_ident);
                    } else {
                        mark_refs(refs, types, wasm, current_scope, elem_ty);
                    }
                }
                ConceptualRustType::Fixed(_) | ConceptualRustType::Primitive(_) => {
                    // nothing to import
                }
                ConceptualRustType::Map(key, value) => {
                    if wasm && *current_scope != *ROOT_SCOPE {
                        // TODO: we should be doing map wrappers where they are declared or used,
                        // but for the former what if the key/value types are in different modules,
                        // and for the latter, what to do if multiple places use it? default to lib?
                        // issue: https://github.com/dcSpark/cddl-codegen/issues/138
                        let map_wrapper_ident = ConceptualRustType::name_for_wasm_map(key, value);
                        refs.entry(current_scope.to_owned())
                            .or_default()
                            .entry(ROOT_SCOPE.clone())
                            .or_default()
                            .insert(map_wrapper_ident);
                    } else {
                        mark_refs(refs, types, wasm, current_scope, key);
                        mark_refs(refs, types, wasm, current_scope, value);
                    }
                }
                ConceptualRustType::Optional(inner_ty) => {
                    mark_refs(refs, types, wasm, current_scope, inner_ty)
                }
            }
        }
        for rust_struct in self.rust_structs().values() {
            let current_scope = self.scope(&rust_struct.ident);
            match rust_struct.variant() {
                RustStructType::Array { element_type } => {
                    mark_refs(&mut refs, self, wasm, current_scope, element_type)
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants, .. } => {
                    variants.iter().for_each(|ev| match &ev.data {
                        EnumVariantData::RustType(ty) => {
                            mark_refs(&mut refs, self, wasm, current_scope, ty)
                        }
                        EnumVariantData::Inlined(record) => {
                            record.fields.iter().for_each(|field| {
                                mark_refs(&mut refs, self, wasm, current_scope, &field.rust_type)
                            })
                        }
                    })
                }
                RustStructType::Record(record) => record.fields.iter().for_each(|field| {
                    mark_refs(&mut refs, self, wasm, current_scope, &field.rust_type)
                }),
                RustStructType::Table { domain, range } => {
                    mark_refs(&mut refs, self, wasm, current_scope, domain);
                    mark_refs(&mut refs, self, wasm, current_scope, range);
                }
                RustStructType::Wrapper { wrapped, .. } => {
                    mark_refs(&mut refs, self, wasm, current_scope, wrapped)
                }
                RustStructType::Extern | RustStructType::RawBytesType => {
                    // impossible to know what this refers to - will have to be done afterwards by user
                }
                RustStructType::CStyleEnum { .. } => {
                    // should only refer to constants
                }
            }
        }
        refs
    }

    fn aliases() -> BTreeMap<idents::AliasIdent, AliasInfo> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, AliasInfo>::new();
        let mut insert_alias = |name: &str, rust_type: RustType| {
            let ident = AliasIdent::new(CDDLIdent::new(name));
            aliases.insert(ident, AliasInfo::new(rust_type, false, false));
        };
        insert_alias("uint", ConceptualRustType::Primitive(Primitive::U64).into());
        insert_alias("nint", ConceptualRustType::Primitive(Primitive::N64).into());
        insert_alias(
            "bool",
            ConceptualRustType::Primitive(Primitive::Bool).into(),
        );
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type: RustType = ConceptualRustType::Primitive(Primitive::Str).into();
        insert_alias("tstr", string_type.clone());
        insert_alias("text", string_type);
        insert_alias(
            "bstr",
            ConceptualRustType::Primitive(Primitive::Bytes).into(),
        );
        insert_alias(
            "bytes",
            ConceptualRustType::Primitive(Primitive::Bytes).into(),
        );
        let null_type: RustType = ConceptualRustType::Fixed(FixedValue::Null).into();
        insert_alias("null", null_type.clone());
        insert_alias("nil", null_type);
        insert_alias(
            "true",
            ConceptualRustType::Fixed(FixedValue::Bool(true)).into(),
        );
        insert_alias(
            "false",
            ConceptualRustType::Fixed(FixedValue::Bool(false)).into(),
        );
        // Note: defaulting to float64 for "float" (so without precision).
        insert_alias(
            "float",
            ConceptualRustType::Primitive(Primitive::F64).into(),
        );
        insert_alias(
            "float64",
            ConceptualRustType::Primitive(Primitive::F64).into(),
        );
        insert_alias(
            "float32",
            ConceptualRustType::Primitive(Primitive::F32).into(),
        );
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    // note: this is mut so that apply_type_aliases() can mark which reserved idents
    // are in the CDDL prelude so we don't generate code for all of them, potentially
    // bloating generated code a bit
    pub fn new_type(&mut self, raw: &CDDLIdent, cli: &Cli) -> RustType {
        let alias_ident = AliasIdent::new(raw.clone());
        let resolved = match self.apply_type_aliases(&alias_ident, cli) {
            Some((ty, true)) => ty,
            Some((ty, false)) => ty.as_alias(alias_ident.clone()),
            None => ConceptualRustType::Rust(RustIdent::new(raw.clone())).into(),
        };
        let resolved_inner = match &resolved.conceptual_type {
            ConceptualRustType::Alias(_, ty) => ty,
            ty => ty,
        };
        if cli.binary_wrappers {
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
            assert!(!ty.cbor_types(self).contains(&CBORType::Special));
        }
        if let ConceptualRustType::Map(key_type, _val_type) = resolved_inner {
            assert!(!key_type.cbor_types(self).contains(&CBORType::Special));
        }
        resolved
    }

    // see new_type() for why this is mut
    /// returns: (base type, if the alias should be substituted)
    pub fn apply_type_aliases(
        &mut self,
        alias_ident: &AliasIdent,
        cli: &Cli,
    ) -> Option<(RustType, bool)> {
        // Assumes we are not trying to pass in any kind of compound type (arrays, etc)
        match self.type_aliases.get(alias_ident) {
            Some(alias) => Some((alias.base_type.clone(), !alias.gen_rust_alias)),
            None => match alias_ident {
                AliasIdent::Rust(_rust_ident) => None,
                AliasIdent::Reserved(reserved) => {
                    if reserved == "int" {
                        // We define an Int rust struct in prelude.rs
                        None
                    } else {
                        // we auto-include only the parts of the cddl prelude necessary (and supported)
                        cddl_prelude(reserved).unwrap_or_else(|| {
                            panic!(
                                "{}",
                                "Reserved ident {reserved} not a part of cddl_prelude?"
                            )
                        });
                        self.emit_prelude(reserved.clone(), cli);
                        Some((
                            ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(format!(
                                "prelude_{reserved}"
                            ))))
                            .into(),
                            true,
                        ))
                    }
                }
            },
        }
    }

    pub fn register_type_alias(&mut self, alias: RustIdent, info: AliasInfo) {
        if let ConceptualRustType::Alias(_ident, _ty) = &info.base_type.conceptual_type {
            panic!("register_type_alias*({}, {:?}) wraps automatically in Alias, no need to provide it.", alias, info.base_type);
        }
        self.type_aliases.insert(alias.into(), info);
    }

    pub fn rust_struct(&self, ident: &RustIdent) -> Option<&RustStruct> {
        self.rust_structs.get(ident)
    }

    /// mostly for convenience since this is checked in so many places
    pub fn is_enum(&self, ident: &RustIdent) -> bool {
        matches!(
            self.rust_struct(ident).unwrap().variant(),
            RustStructType::CStyleEnum { .. }
        )
    }

    // this is called by register_table_type / register_array_type automatically
    pub fn register_rust_struct(
        &mut self,
        parent_visitor: &ParentVisitor,
        rust_struct: RustStruct,
        cli: &Cli,
    ) {
        match &rust_struct.variant {
            RustStructType::Table { domain, range } => {
                // we must provide the keys type to return
                self.create_and_register_array_type(
                    parent_visitor,
                    domain.clone(),
                    &domain.conceptual_type.name_as_wasm_array(self),
                    cli,
                );
                let mut map_type: RustType =
                    ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone()))
                        .into();
                if let Some(tag) = rust_struct.tag {
                    map_type = map_type.tag(tag);
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    AliasInfo::new(map_type, true, false),
                )
            }
            RustStructType::Array { element_type } => {
                let mut array_type: RustType =
                    ConceptualRustType::Array(Box::new(element_type.clone())).into();
                if let Some(tag) = rust_struct.tag {
                    array_type = array_type.tag(tag);
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    AliasInfo::new(array_type, true, false),
                )
            }
            RustStructType::Wrapper {
                min_max: Some(_), ..
            } => {
                self.mark_new_can_fail(rust_struct.ident.clone());
            }
            _ => (),
        }
        self.rust_structs
            .insert(rust_struct.ident().clone(), rust_struct);
    }

    // creates a RustType for the array type - and if needed, registers a type to generate
    // TODO: After the split we should be able to only register it directly
    // and then examine those at generation-time and handle things ALWAYS as RustType::Array
    pub fn create_and_register_array_type(
        &mut self,
        parent_visitor: &ParentVisitor,
        element_type: RustType,
        array_type_name: &str,
        cli: &Cli,
    ) -> RustType {
        let raw_arr_type = ConceptualRustType::Array(Box::new(element_type.clone()));
        // only generate an array wrapper if we can't wasm-expose it raw
        if raw_arr_type.directly_wasm_exposable(self) {
            return raw_arr_type.into();
        }
        let array_type_ident = RustIdent::new(CDDLIdent::new(array_type_name));
        // If we are the only thing referring to our element and it's a plain group
        // we must mark it as being serialized as an array
        if let ConceptualRustType::Rust(_) = &element_type.conceptual_type {
            self.set_rep_if_plain_group(
                parent_visitor,
                &array_type_ident,
                Representation::Array,
                cli,
            );
        }
        if cli.wasm {
            // we don't pass in tags here. If a tag-wrapped array is done I think it generates
            // 2 separate types (array wrapper -> tag wrapper struct)
            self.register_rust_struct(
                parent_visitor,
                RustStruct::new_array(array_type_ident, None, element_type.clone()),
                cli,
            );
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
    pub fn finalize(&mut self, parent_visitor: &ParentVisitor, cli: &Cli) {
        // resolve generics
        // resolve then register in 2 phases to get around borrow checker
        let resolved_generics = self
            .generic_instances
            .values()
            .map(|instance| instance.resolve(self))
            .collect::<Vec<_>>();
        for resolved_instance in resolved_generics {
            self.register_rust_struct(parent_visitor, resolved_instance, cli);
        }
        // recursively check all types used as keys or contained within a type used as a key
        // this is so we only derive comparison or hash traits for those types
        let mut used_as_key = BTreeSet::new();
        fn mark_used_as_key(ty: &ConceptualRustType, used_as_key: &mut BTreeSet<RustIdent>) {
            if let ConceptualRustType::Rust(ident) = ty {
                used_as_key.insert(ident.clone());
            }
        }
        fn check_used_as_key(
            ty: &ConceptualRustType,
            types: &IntermediateTypes<'_>,
            used_as_key: &mut BTreeSet<RustIdent>,
        ) {
            if let ConceptualRustType::Map(k, _v) = ty {
                k.visit_types(types, &mut |ty| mark_used_as_key(ty, used_as_key));
            }
        }
        for rust_struct in self.rust_structs().values() {
            rust_struct.visit_types(self, &mut |ty| {
                check_used_as_key(ty, self, &mut used_as_key)
            });
            if let RustStructType::Table { domain, .. } = rust_struct.variant() {
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
        self.visit_types(&mut |ty| {
            if let ConceptualRustType::Rust(id) = ty {
                if id == ident {
                    found = true
                }
            }
        });
        found
    }

    // see self.plain_groups comments
    pub fn mark_plain_group(&mut self, ident: RustIdent, group: Option<cddl::ast::Group<'a>>) {
        self.plain_groups.insert(ident, group);
    }

    // see self.plain_groups comments
    pub fn set_rep_if_plain_group(
        &mut self,
        parent_visitor: &ParentVisitor,
        ident: &RustIdent,
        rep: Representation,
        cli: &Cli,
    ) {
        if let Some(plain_group) = self.plain_groups.get(ident) {
            // the clone is to get around the borrow checker
            if let Some(group) = plain_group.as_ref().cloned() {
                // we are defined via .cddl and thus need to register a concrete
                // representation of the plain group
                if let Some(rust_struct) = self.rust_structs.get(ident) {
                    // it's already defined, let's check that we're not giving it multiple representations
                    let found_rep = match &rust_struct.variant {
                        RustStructType::Record(record) => Some(record.rep),
                        RustStructType::GroupChoice { rep, .. } => Some(*rep),
                        _ => None,
                    };
                    assert_eq!(found_rep, Some(rep));
                } else {
                    // you can't tag plain groups hence the None
                    // we also don't support generics in plain groups hence the other None
                    crate::parsing::parse_group(
                        self,
                        parent_visitor,
                        &group,
                        ident,
                        rep,
                        None,
                        None,
                        cli,
                    );
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

    pub fn mark_scope(&mut self, ident: RustIdent, scope: ModuleScope) {
        if let Some(old_scope) = self.scopes.insert(ident.clone(), scope.clone()) {
            if old_scope != scope {
                panic!(
                    "{} defined multiple times, first referenced in scope '{}' then in '{}'",
                    ident, old_scope, scope
                );
            }
        }
    }

    pub fn scope(&self, ident: &RustIdent) -> &ModuleScope {
        self.scopes.get(ident).unwrap_or(&self.root_scope)
    }

    // we need to do this for some generated intermediate structures as the parsing code
    // doesn't allow to just generate a rust struct but instead inserts everything needed
    pub fn remove_rust_struct(&mut self, ident: &RustIdent) -> Option<RustStruct> {
        self.plain_groups.remove(ident);
        self.scopes.remove(ident);
        self.rust_structs.remove(ident)
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
            for (alias_name, alias_info) in self.type_aliases.iter() {
                println!("{alias_name:?} -> {alias_info:?}");
            }
        }

        if !self.generic_defs.is_empty() {
            println!("\n\nGeneric Definitions:");
            for (ident, def) in self.generic_defs.iter() {
                println!("{ident} -> {def:?}");
            }
        }

        if !self.generic_instances.is_empty() {
            println!("\n\nGeneric Instances:");
            for (ident, def) in self.generic_instances.iter() {
                println!("{ident} -> {def:?}");
            }
        }

        if !self.rust_structs.is_empty() {
            println!("\n\nRustStructs:");
            for (ident, rust_struct) in self.rust_structs.iter() {
                println!("{ident} -> {rust_struct:?}\n");
            }
        }
    }

    fn emit_prelude(&mut self, cddl_name: String, cli: &Cli) {
        // we just emit this directly into this scope.
        // due to some referencing others this is the quickest way
        // to support it.
        // TODO: we might want to custom-write some of these to make them
        // easier to use instead of directly parsing
        if self.prelude_to_emit.insert(cddl_name.clone()) {
            let def = format!(
                "prelude_{} = {}\n",
                cddl_name,
                cddl_prelude(&cddl_name).unwrap()
            );
            let cddl = cddl::parser::cddl_from_str(&def, true).unwrap();
            assert_eq!(cddl.rules.len(), 1);
            let pv = ParentVisitor::new(&cddl).unwrap();
            crate::parsing::parse_rule(self, &pv, cddl.rules.first().unwrap(), cli);
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Representation {
    Array,
    Map,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixedValue {
    Null,
    Bool(bool),
    Nint(isize),
    Uint(usize),
    Float(f64),
    Text(String),
    // UTF byte types not supported
}

fn convert_to_alphanumeric(input: &str) -> String {
    input
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .collect()
}

impl FixedValue {
    fn for_variant(&self) -> VariantIdent {
        match self {
            FixedValue::Null => VariantIdent::new_custom("Null"),
            FixedValue::Bool(b) => VariantIdent::new_custom(match b {
                true => "True",
                false => "False",
            }),
            FixedValue::Nint(i) => VariantIdent::new_custom(format!("U{i}")),
            FixedValue::Uint(u) => VariantIdent::new_custom(format!("I{u}")),
            FixedValue::Float(f) => VariantIdent::new_custom(format!("F{f}")),
            FixedValue::Text(s) => {
                VariantIdent::new_custom(convert_to_alphanumeric(&convert_to_camel_case(s)))
            }
        }
    }

    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        match self {
            FixedValue::Null => buf.write_special(cbor_event::Special::Null),
            FixedValue::Bool(b) => buf.write_special(cbor_event::Special::Bool(*b)),
            FixedValue::Nint(i) => buf.write_negative_integer(*i as i64),
            FixedValue::Uint(u) => buf.write_unsigned_integer(*u as u64),
            FixedValue::Float(f) => buf.write_special(Special::Float(*f)),
            FixedValue::Text(s) => buf.write_text(s),
        }
        .expect("Unable to serialize key for canonical ordering");
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
            FixedValue::Float(f) => f.to_string(),
            FixedValue::Text(s) => format!("\"{s}\".to_owned()"),
        }
    }

    /// Converts a literal to a valid rust comparison valid for comparisons
    /// e.g. Text can be &str to avoid creating a String
    pub fn to_primitive_str_compare(&self) -> String {
        match self {
            FixedValue::Text(s) => format!("\"{s}\""),
            _ => self.to_primitive_str_assign(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Primitive {
    Bool,
    F64,
    F32,
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

impl ToString for Primitive {
    fn to_string(&self) -> String {
        String::from(match self {
            Primitive::Bool => "bool",
            Primitive::F32 => "f32",
            Primitive::F64 => "f64",
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
}
// TODO: impl display or fmt or whatever rust uses
impl Primitive {
    pub fn to_variant(&self) -> VariantIdent {
        VariantIdent::new_custom(match self {
            Primitive::Bool => "Bool",
            Primitive::F32 => "F32",
            Primitive::F64 => "F64",
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
            Primitive::F32 => vec![CBORType::Special],
            Primitive::F64 => vec![CBORType::Special],
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
    use crate::{rust_reserved::STD_TYPES, utils::is_identifier_reserved};

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

            assert!(
                !STD_TYPES.contains(&&super::convert_to_camel_case(&cddl_ident.0)[..]),
                "Cannot use reserved Rust type name: \"{}\"",
                cddl_ident.0
            );
            if cddl_ident.0 != "int" {
                assert!(
                    !is_identifier_reserved(&cddl_ident.0),
                    "Cannot use reserved CDDL keyword: \"{}\"",
                    cddl_ident.0
                );
            }

            Self(super::convert_to_camel_case(&cddl_ident.0))
        }
    }

    impl std::fmt::Display for RustIdent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl AsRef<str> for RustIdent {
        fn as_ref(&self) -> &str {
            self.0.as_str()
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
                VariantIdent::Custom(name) => write!(f, "{name}"),
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
                AliasIdent::Reserved(name) => write!(f, "{name}"),
                AliasIdent::Rust(ident) => ident.fmt(f),
            }
        }
    }
}
use crate::cli::Cli;
pub use idents::*;

/// Details on how to encode a rust type in CBOR. Order is important
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CBOREncodingOperation {
    /// CBOR tagged type
    Tagged(usize),
    /// bytes .cbor T in cddl, outside of serialization is semantically like T
    CBORBytes,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct RustTypeSerializeConfig {
    /// default value when missing in deserialization
    pub default: Option<FixedValue>,
    /// Bounds to check. Relevant to primitives + arrays + maps
    pub bounds: Option<(Option<i128>, Option<i128>)>,
    /// Basic group encoding override. If true basic encoding will not be used in (de)serialization
    pub basic_override: bool,
}

/// A complete rust type, including serialization options that don't impact other areas
#[derive(Clone, Debug, PartialEq)]
pub struct RustType {
    /// Conceptual type i.e. how it's used in non-serialization contexts
    pub conceptual_type: ConceptualRustType,
    /// How to encode the conceptual type. Order is important. Applied in iteration order.
    pub encodings: Vec<CBOREncodingOperation>,
    /// Further type configuration that aren't encoding operation
    pub config: RustTypeSerializeConfig,
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
            config: RustTypeSerializeConfig::default(),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn as_alias(mut self, alias_ident: AliasIdent) -> Self {
        self.conceptual_type =
            ConceptualRustType::Alias(alias_ident, Box::new(self.conceptual_type));
        self
    }

    pub fn tag(mut self, tag: usize) -> Self {
        self.encodings.push(CBOREncodingOperation::Tagged(tag));
        self
    }

    pub fn tag_if(self, tag: Option<usize>) -> Self {
        if let Some(t) = tag {
            self.tag(t)
        } else {
            self
        }
    }

    pub fn default(mut self, default_value: FixedValue) -> Self {
        assert!(self.config.default.is_none());
        let matches = if let ConceptualRustType::Primitive(p) =
            self.conceptual_type.resolve_alias_shallow()
        {
            match &default_value {
                FixedValue::Bool(_) => *p == Primitive::Bool,
                FixedValue::Nint(_) => p.cbor_types().contains(&CBORType::NegativeInteger),
                FixedValue::Uint(_) => p.cbor_types().contains(&CBORType::UnsignedInteger),
                FixedValue::Float(_) => *p == Primitive::F64 || *p == Primitive::F32,
                FixedValue::Null => false,
                FixedValue::Text(_) => *p == Primitive::Str,
            }
        } else {
            false
        };
        if !matches {
            panic!(
                ".default {:?} invalid for type {:?}",
                default_value, self.conceptual_type
            );
        }
        self.config.default = Some(default_value);
        self
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn as_bytes(mut self) -> Self {
        self.encodings.push(CBOREncodingOperation::CBORBytes);
        self
    }

    // deep resolve aliases
    pub fn resolve_aliases(self) -> Self {
        Self {
            conceptual_type: self.conceptual_type.resolve_aliases(),
            encodings: self.encodings,
            config: self.config,
        }
    }

    pub fn with_bounds(self, mut bounds: (Option<i128>, Option<i128>)) -> Self {
        assert!(self.config.bounds.is_none());
        // remove redundant 0 for unsigned types
        if bounds.0 == Some(0)
            && matches!(
                self.conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Primitive(Primitive::Bytes)
                    | ConceptualRustType::Primitive(Primitive::Str)
                    | ConceptualRustType::Primitive(Primitive::U8)
                    | ConceptualRustType::Primitive(Primitive::U16)
                    | ConceptualRustType::Primitive(Primitive::U32)
                    | ConceptualRustType::Primitive(Primitive::U64)
            )
        {
            bounds.0 = None;
        }
        Self {
            conceptual_type: self.conceptual_type,
            encodings: self.encodings,
            config: RustTypeSerializeConfig {
                default: self.config.default,
                bounds: if bounds.0.is_some() || bounds.1.is_some() {
                    Some(bounds)
                } else {
                    None
                },
                basic_override: self.config.basic_override,
            },
        }
    }

    pub fn not_basic(self) -> Self {
        Self {
            conceptual_type: self.conceptual_type,
            encodings: self.encodings,
            config: RustTypeSerializeConfig {
                default: self.config.default,
                bounds: self.config.bounds,
                basic_override: true,
            },
        }
    }

    /// Checks whether FROM THIS CONTEXT the type is a basic group.
    /// Only relevant to rust structs.
    pub fn is_basic(&self, types: &IntermediateTypes) -> bool {
        if let ConceptualRustType::Rust(ident) = self.conceptual_type.resolve_alias_shallow() {
            !self.config.basic_override && types.is_plain_group(ident)
        } else {
            false
        }
    }

    // CBOR len count for the entire type if it were embedded as a member in a cbor collection (array/map)
    pub fn expanded_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        match self.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Optional(ty) => match ty.expanded_field_count(types) {
                Some(1) => Some(1),
                // differing sizes when Null vs Some
                _ => None,
            },
            ConceptualRustType::Rust(ident) => {
                if self.is_basic(types) {
                    match types.rust_structs.get(ident) {
                        Some(rs) => rs.fixed_field_count(types),
                        None => panic!(
                            "rust struct {} not found but referenced by {:?}",
                            ident, self
                        ),
                    }
                } else {
                    // C-style enums + extern + raw bytes should all be 1 too so don't bother checking
                    Some(1)
                }
            }
            _ => Some(1),
        }
    }

    // See comment in RustStruct::definite_info(), this is the same, returns a string expression
    // which evaluates to the length.
    // self_expr is an expression that evaluates to this RustType (e.g. member, etc) at the point where
    // the return of this function will be used.
    pub fn definite_info(&self, self_expr: &str, types: &IntermediateTypes, cli: &Cli) -> String {
        match self.expanded_field_count(types) {
            Some(count) => count.to_string(),
            None => match self.conceptual_type.resolve_alias_shallow() {
                ConceptualRustType::Optional(ty) => format!(
                    "match {} {{ Some(x) => {}, None => 1 }}",
                    self_expr,
                    ty.definite_info("x", types, cli)
                ),
                ConceptualRustType::Rust(ident) => {
                    if types.is_plain_group(ident) {
                        match types.rust_structs.get(ident) {
                            Some(rs) => rs.definite_info(types, cli),
                            None => panic!(
                                "rust struct {} not found but referenced by {:?}",
                                ident, self
                            ),
                        }
                    } else {
                        // C-style enums + extern + raw bytes should all be 1 too so don't bother checking
                        String::from("1")
                    }
                }
                _ => String::from("1"),
            },
        }
    }

    // the minimum cbor length of this struct - can be useful for deserialization length checks
    // does not count ANY type choice like types including Optional UNLESS the option Some type
    // has cbor len 1 too - to be consistent with expanded_field_count
    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        match self.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Optional(ty) => match ty.expanded_field_count(types) {
                Some(1) => 1,
                _ => 0,
            },
            ConceptualRustType::Rust(ident) => {
                if types.is_plain_group(ident) {
                    match types.rust_structs.get(ident) {
                        Some(x) => x.expanded_mandatory_field_count(types),
                        None => panic!(
                            "rust struct {} not found but referenced by {:?}",
                            ident, self
                        ),
                    }
                } else {
                    // C-style enums + extern + raw bytes should all be 1 too so don't bother checking
                    1
                }
            }
            _ => 1,
        }
    }

    /// All POSSIBLE outermost CBOR types this can encode to
    pub fn cbor_types(&self, types: &IntermediateTypes) -> Vec<CBORType> {
        match self.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_)) => vec![CBORType::Tag],
            Some(CBOREncodingOperation::CBORBytes) => vec![CBORType::Bytes],
            None => match &self.conceptual_type {
                ConceptualRustType::Fixed(f) => vec![match f {
                    FixedValue::Uint(_) => CBORType::UnsignedInteger,
                    FixedValue::Nint(_) => CBORType::NegativeInteger,
                    FixedValue::Float(_) => CBORType::Special,
                    FixedValue::Text(_) => CBORType::Text,
                    FixedValue::Null => CBORType::Special,
                    FixedValue::Bool(_) => CBORType::Special,
                }],
                ConceptualRustType::Primitive(p) => p.cbor_types(),
                ConceptualRustType::Rust(ident) => {
                    let rust_struct = types.rust_struct(ident).unwrap();
                    if rust_struct.tag.is_some() {
                        vec![CBORType::Tag]
                    } else {
                        match rust_struct.variant() {
                            RustStructType::Wrapper { wrapped, .. } => wrapped.cbor_types(types),
                            // we can't know this unless there's a way to provide this info
                            RustStructType::Extern => vec![CBORType::Array, CBORType::Map],
                            RustStructType::Record(record) => match record.rep {
                                Representation::Array => vec![CBORType::Array],
                                Representation::Map => vec![CBORType::Map],
                            },
                            RustStructType::CStyleEnum { variants }
                            | RustStructType::TypeChoice { variants } => {
                                let mut variant_cbor_types = variants
                                    .iter()
                                    .flat_map(|ev| ev.rust_type().cbor_types(types))
                                    .collect::<Vec<CBORType>>();
                                variant_cbor_types.dedup();
                                variant_cbor_types
                            }
                            RustStructType::GroupChoice { rep, .. } => match rep {
                                Representation::Array => vec![CBORType::Array],
                                Representation::Map => vec![CBORType::Map],
                            },
                            RustStructType::RawBytesType => vec![CBORType::Bytes],
                            _ => panic!(),
                        }
                    }
                }
                ConceptualRustType::Array(_) => vec![CBORType::Array],
                ConceptualRustType::Map(_k, _v) => vec![CBORType::Map],
                ConceptualRustType::Optional(ty) => {
                    let mut inner_types = ty.cbor_types(types);
                    if !inner_types.contains(&CBORType::Special) {
                        inner_types.push(CBORType::Special);
                    }
                    inner_types
                }
                ConceptualRustType::Alias(_ident, ty) => {
                    Self::new((**ty).clone()).cbor_types(types)
                }
            },
        }
    }

    fn _cbor_special_type(&self) -> Option<CBORSpecial> {
        unimplemented!()
    }

    fn _is_serialize_multiline(&self, types: &IntermediateTypes) -> bool {
        if self.encodings.is_empty() {
            match &self.conceptual_type {
                ConceptualRustType::Fixed(_) => false,
                ConceptualRustType::Primitive(_) => false,
                ConceptualRustType::Rust(ident) => types.is_enum(ident),
                ConceptualRustType::Array(_) => true,
                ConceptualRustType::Optional(_) => false,
                ConceptualRustType::Map(_, _) => false,
                ConceptualRustType::Alias(_ident, ty) => {
                    Self::new((**ty).clone())._is_serialize_multiline(types)
                }
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
#[derive(Clone, Debug, PartialEq)]
pub enum ConceptualRustType {
    Fixed(FixedValue),
    // Primitive type that can be passed to/from wasm
    Primitive(Primitive),
    // Rust-defined type that can be put in arrays/etc. Can be an enum, etc too.
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
    // deep resolve aliases - does it inside of options, maps, arrays, etc
    pub fn resolve_aliases(self) -> Self {
        match self {
            Self::Array(ty) => Self::Array(Box::new(ty.resolve_aliases())),
            Self::Alias(_, ty) => ty.resolve_aliases(),
            Self::Map(key, value) => Self::Map(
                Box::new(key.resolve_aliases()),
                Box::new(value.resolve_aliases()),
            ),
            Self::Optional(ty) => Self::Optional(Box::new(ty.resolve_aliases())),
            _ => self,
        }
    }

    // shallow resolve aliases. use this when you only need to strip direct aliases
    // to check the type more easily e.g. to figure out if a ConceptualRustType
    // is a Rust, a Primitive, etc
    // This avoids the clone in this case
    pub fn resolve_alias_shallow(&self) -> &Self {
        match self {
            Self::Alias(_, ty) => ty.resolve_alias_shallow(),
            _ => self,
        }
    }

    pub fn directly_wasm_exposable(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::Fixed(_) => false,
            Self::Primitive(_) => true,
            Self::Rust(ident) => types.is_enum(ident),
            // wasm_bindgen doesn't support nested vecs, even if the inner vec would be supported
            Self::Array(ty) => {
                let inner = match &ty.conceptual_type {
                    Self::Alias(_ident, ty) => ty,
                    Self::Optional(ty) => &ty.conceptual_type,
                    ty => ty,
                };
                match inner {
                    Self::Primitive(p) => match p {
                        // converts to js number which is supported as Vec<T>
                        Primitive::Bool
                        | Primitive::F32
                        | Primitive::F64
                        | Primitive::I8
                        | Primitive::U8
                        | Primitive::I16
                        | Primitive::U16
                        | Primitive::I32
                        | Primitive::U32
                        | Primitive::I64
                        | Primitive::N64
                        | Primitive::U64 => true,
                        // Bytes is already implemented as Vec<u8> so we can't nest it
                        Primitive::Bytes => false,
                        // Vec<String> is not supported by wasm-bindgen
                        Primitive::Str => false,
                    },
                    Self::Array(_) => false,
                    _ => ty.conceptual_type.directly_wasm_exposable(types),
                }
            }
            Self::Optional(ty) => ty.conceptual_type.directly_wasm_exposable(types),
            Self::Map(_, _) => false,
            Self::Alias(_ident, ty) => ty.directly_wasm_exposable(types),
        }
    }

    pub fn is_fixed_value(&self) -> bool {
        match self {
            Self::Fixed(_) => true,
            Self::Alias(_ident, ty) => ty.is_fixed_value(),
            _ => false,
        }
    }

    pub fn name_as_wasm_array(&self, types: &IntermediateTypes) -> String {
        if Self::Array(Box::new(self.clone().into())).directly_wasm_exposable(types) {
            format!("Vec<{}>", self.for_wasm_member(types))
        } else {
            format!("{}List", self.for_variant())
        }
    }

    pub fn name_as_rust_array(
        &self,
        types: &IntermediateTypes,
        from_wasm: bool,
        cli: &Cli,
    ) -> String {
        format!("Vec<{}>", self.for_rust_member(types, from_wasm, cli))
    }

    /// Function parameter TYPE that will be moved in
    pub fn for_rust_move(&self, types: &IntermediateTypes, cli: &Cli) -> String {
        self.for_rust_member(types, false, cli)
    }

    /// Function parameter TYPE by-non-mut-reference for read-only
    pub fn _for_rust_read(&self, types: &IntermediateTypes, cli: &Cli) -> String {
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type, only here for serialization: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => {
                if types.is_enum(ident) {
                    ident.to_string()
                } else {
                    format!("&{ident}")
                }
            }
            Self::Array(ty) => format!(
                "&{}",
                ty.conceptual_type.name_as_rust_array(types, false, cli)
            ),
            Self::Optional(ty) => {
                format!("Option<{}>", ty.conceptual_type._for_rust_read(types, cli))
            }
            Self::Map(_k, _v) => format!("&{}", self.for_rust_member(types, false, cli)),
            Self::Alias(ident, ty) => match &**ty {
                // TODO: ???
                Self::Rust(_) => format!("&{ident}"),
                _ => ident.to_string(),
            },
        }
    }

    /// Function parameter TYPE from wasm (i.e. ref for non-primitives, value for supported primitives)
    pub fn for_wasm_param(&self, types: &IntermediateTypes) -> String {
        self.for_wasm_param_impl(types, false)
    }

    fn for_wasm_param_impl(&self, types: &IntermediateTypes, force_not_ref: bool) -> String {
        let opt_ref = if force_not_ref { "" } else { "&" };
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type to wasm, only here for serialization: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => {
                if types.is_enum(ident) {
                    ident.to_string()
                } else {
                    format!("{opt_ref}{ident}")
                }
            }
            Self::Array(ty) => {
                if self.directly_wasm_exposable(types) {
                    ty.conceptual_type.name_as_wasm_array(types)
                } else {
                    format!(
                        "{}{}",
                        opt_ref,
                        ty.conceptual_type.name_as_wasm_array(types)
                    )
                }
            }
            Self::Optional(ty) => {
                format!(
                    "Option<{}>",
                    ty.conceptual_type.for_wasm_param_impl(types, true)
                )
            }
            Self::Map(_k, _v) => format!("{}{}", opt_ref, self.for_wasm_member(types)),
            // it might not be worth generating this as aliases are ignored by wasm-pack build, but
            // that could change in the future so as long as it doens't cause issues we'll leave it
            Self::Alias(ident, ty) => match &**ty {
                Self::Rust(_) |
                Self::Array(_) |
                Self::Map(_, _) if !self.directly_wasm_exposable(types) => format!("{opt_ref}{ident}"),
                Self::Optional(_) |
                // no special handling if for some reason nested aliases, just strip all to avoid hassle
                Self::Alias(_, _) => ty.for_wasm_param_impl(types, force_not_ref),
                _ => ident.to_string(),
            },
        }
    }

    /// Return TYPE for wasm
    pub fn for_wasm_return(&self, types: &IntermediateTypes) -> String {
        self.for_wasm_member(types)
    }

    pub fn name_for_wasm_map(k: &RustType, v: &RustType) -> RustIdent {
        RustIdent::new(CDDLIdent::new(format!(
            "Map{}To{}",
            k.conceptual_type.for_variant(),
            v.conceptual_type.for_variant()
        )))
    }

    pub fn name_for_rust_map(
        types: &IntermediateTypes,
        k: &RustType,
        v: &RustType,
        from_wasm: bool,
        cli: &Cli,
    ) -> String {
        format!(
            "{}<{}, {}>",
            table_type(cli),
            k.conceptual_type.for_rust_member(types, from_wasm, cli),
            v.conceptual_type.for_rust_member(types, from_wasm, cli)
        )
    }

    /// If we were to store a value directly in a wasm-wrapper, this would be used.
    pub fn for_wasm_member(&self, types: &IntermediateTypes) -> String {
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type in member, only needed for serializaiton: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => ident.to_string(),
            Self::Array(ty) => ty.conceptual_type.name_as_wasm_array(types),
            Self::Optional(ty) => format!("Option<{}>", ty.conceptual_type.for_wasm_member(types)),
            Self::Map(k, v) => Self::name_for_wasm_map(k, v).to_string(),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_wasm_member(types),
                // but other aliases are generated and should be used.
                AliasIdent::Rust(_) => ident.to_string(),
            },
        }
    }

    /// Type when storing a value inside of a rust struct. This is the underlying raw representation.
    pub fn for_rust_member(&self, types: &IntermediateTypes, from_wasm: bool, cli: &Cli) -> String {
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type in member, only needed for serializaiton: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            Self::Rust(ident) => {
                if from_wasm && !types.is_enum(ident) {
                    crate::generation::rust_crate_struct_from_wasm(types, ident, cli)
                } else {
                    ident.to_string()
                }
            }
            Self::Array(ty) => ty.conceptual_type.name_as_rust_array(types, from_wasm, cli),
            Self::Optional(ty) => {
                format!(
                    "Option<{}>",
                    ty.conceptual_type.for_rust_member(types, from_wasm, cli)
                )
            }
            Self::Map(k, v) => Self::name_for_rust_map(types, k, v, from_wasm, cli),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_rust_member(types, from_wasm, cli),
                // but other aliases are generated and should be used.
                AliasIdent::Rust(rust_ident) => {
                    if from_wasm {
                        crate::generation::rust_crate_struct_from_wasm(types, rust_ident, cli)
                    } else {
                        ident.to_string()
                    }
                }
            },
        }
    }

    /// IDENTIFIER for an enum variant. (Use for_rust_member() for the enum value)
    pub fn for_variant(&self) -> VariantIdent {
        match self {
            Self::Fixed(f) => f.for_variant(),
            Self::Primitive(p) => p.to_variant(),
            Self::Rust(ident) => VariantIdent::new_rust(ident.clone()),
            Self::Array(inner) => {
                VariantIdent::new_custom(format!("Arr{}", inner.conceptual_type.for_variant()))
            }
            // TODO: should we not end up in this situation and just insert a Null fixed value instead?
            Self::Optional(ty) => {
                VariantIdent::new_custom(format!("Opt{}", ty.conceptual_type.for_variant()))
            }
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
    #[allow(clippy::wrong_self_convention)]
    pub fn from_wasm_boundary_clone(
        &self,
        types: &IntermediateTypes,
        expr: &str,
        can_fail: bool,
    ) -> Vec<ToWasmBoundaryOperations> {
        let expr_cloned = if self.is_copy(types) {
            expr.to_owned()
        } else {
            format!("{expr}.clone()")
        };
        let mut ops = match self {
            Self::Rust(_ident) => vec![
                ToWasmBoundaryOperations::Code(expr_cloned),
                ToWasmBoundaryOperations::Into,
            ],
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_clone(types, expr, can_fail),
            Self::Optional(ty) => ty
                .conceptual_type
                .from_wasm_boundary_clone_optional(types, expr, can_fail),
            Self::Array(ty) => {
                if self.directly_wasm_exposable(types) {
                    ty.conceptual_type
                        .from_wasm_boundary_clone(types, expr, can_fail)
                } else {
                    vec![
                        ToWasmBoundaryOperations::Code(expr_cloned),
                        ToWasmBoundaryOperations::Into,
                    ]
                }
            }
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

    #[allow(clippy::wrong_self_convention)]
    fn from_wasm_boundary_clone_optional(
        &self,
        types: &IntermediateTypes,
        expr: &str,
        can_fail: bool,
    ) -> Vec<ToWasmBoundaryOperations> {
        let mut ops = match self {
            Self::Primitive(_) => vec![ToWasmBoundaryOperations::Code(expr.to_owned())],
            Self::Rust(ident) if types.is_enum(ident) => {
                vec![ToWasmBoundaryOperations::Code(expr.to_owned())]
            }
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_clone_optional(types, expr, can_fail),
            Self::Array(..) | Self::Rust(..) | Self::Map(..) => vec![
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
    #[allow(clippy::wrong_self_convention)]
    pub fn from_wasm_boundary_ref(&self, types: &IntermediateTypes, expr: &str) -> String {
        match self {
            Self::Rust(_ident) => expr.to_owned(),
            Self::Alias(_ident, ty) => ty.from_wasm_boundary_ref(types, expr),
            Self::Optional(ty) => ty.conceptual_type.from_wasm_boundary_ref(types, expr),
            Self::Array(ty) => {
                if self.directly_wasm_exposable(types) {
                    ty.conceptual_type.from_wasm_boundary_ref(types, expr)
                } else {
                    expr.to_owned()
                }
            }
            Self::Map(_k, _v) => expr.to_owned(),
            _ => format!("&{expr}"),
        }
    }

    /// FROM rust TO wasm (with cloning/wrapping) (for arguments)
    pub fn to_wasm_boundary(&self, types: &IntermediateTypes, expr: &str, is_ref: bool) -> String {
        let primitive_impl = || {
            if self.is_copy(types) {
                if is_ref {
                    format!("*{expr}")
                } else {
                    expr.to_owned()
                }
            } else {
                format!("{expr}.clone()")
            }
        };
        match self {
            Self::Fixed(_) => panic!("fixed types are a serialization detail"),
            Self::Primitive(_) => primitive_impl(),
            Self::Rust(ident) => {
                if types.is_enum(ident) {
                    primitive_impl()
                } else {
                    format!("{expr}.clone().into()")
                }
            }
            //Self::Array(ty) => format!("{}({}.clone())", ty.name_as_wasm_array(types), expr),
            //Self::Map(k, v) => format!("{}({}.clone())", Self::name_for_wasm_map(k, v), expr),
            Self::Array(_ty) => {
                if self.directly_wasm_exposable(types) {
                    format!("{expr}.clone()")
                } else {
                    format!("{expr}.clone().into()")
                }
            }
            Self::Map(_k, _v) => format!("{expr}.clone().into()"),
            Self::Optional(ty) => ty
                .conceptual_type
                .to_wasm_boundary_optional(types, expr, is_ref),
            Self::Alias(_ident, ty) => ty.to_wasm_boundary(types, expr, is_ref),
        }
    }

    /// FROM rust TO wasm as Option<T>. This is separate as we can have optional fields
    /// that act identical to Self::Optional(ty)
    pub fn to_wasm_boundary_optional(
        &self,
        types: &IntermediateTypes,
        expr: &str,
        is_ref: bool,
    ) -> String {
        if self.directly_wasm_exposable(types) {
            self.to_wasm_boundary(types, expr, is_ref)
        } else {
            format!("{expr}.clone().map(std::convert::Into::into)")
        }
    }

    // if it impements the Copy trait in rust
    pub fn is_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::Fixed(_f) => unreachable!(),
            Self::Primitive(p) => match p {
                Primitive::Bool
                | Primitive::F32
                | Primitive::F64
                | Primitive::I8
                | Primitive::I16
                | Primitive::I32
                | Primitive::I64
                | Primitive::N64
                | Primitive::U8
                | Primitive::U16
                | Primitive::U32
                | Primitive::U64 => true,
                Primitive::Str | Primitive::Bytes => false,
            },
            Self::Rust(ident) => types.is_enum(ident),
            Self::Array(_) => false,
            Self::Map(_k, _v) => false,
            Self::Optional(ty) => ty.conceptual_type.is_copy(types),
            Self::Alias(_ident, ty) => ty.is_copy(types),
        }
    }

    pub fn clone_if_not_copy(&self, types: &IntermediateTypes, expr: &str) -> String {
        if self.is_copy(types) {
            expr.to_owned()
        } else {
            format!("{expr}.clone()")
        }
    }

    pub fn visit_types<F: FnMut(&Self)>(&self, types: &IntermediateTypes, f: &mut F) {
        self.visit_types_excluding(types, f, &mut BTreeSet::new())
    }

    pub fn visit_types_excluding<F: FnMut(&Self)>(
        &self,
        types: &IntermediateTypes,
        f: &mut F,
        already_visited: &mut BTreeSet<RustIdent>,
    ) {
        f(self);
        match self {
            Self::Alias(ident, ty) => {
                match ident {
                    AliasIdent::Rust(rust_ident) => {
                        if already_visited.insert(rust_ident.clone()) {
                            ty.visit_types_excluding(types, f, already_visited)
                        }
                    }
                    _ => ty.visit_types_excluding(types, f, already_visited),
                };
            }
            Self::Array(ty) => ty
                .conceptual_type
                .visit_types_excluding(types, f, already_visited),
            Self::Fixed(_) => (),
            Self::Map(k, v) => {
                k.conceptual_type
                    .visit_types_excluding(types, f, already_visited);
                v.conceptual_type
                    .visit_types_excluding(types, f, already_visited);
            }
            Self::Optional(ty) => {
                ty.conceptual_type
                    .visit_types_excluding(types, f, already_visited)
            }
            Self::Primitive(_) => (),
            Self::Rust(ident) => {
                if already_visited.insert(ident.clone()) {
                    if let Some(t) = types.rust_struct(ident) {
                        t.visit_types_excluding(types, f, already_visited)
                    }
                }
            }
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
                Self::Into | Self::TryInto => Some(Self::TryInto),
                Self::MapInto | Self::MapTryInto => Some(Self::MapTryInto),
            },
            Self::MapInto => match next {
                Self::Code(_) => None,
                Self::Into | Self::MapInto => Some(Self::MapInto),
                Self::TryInto | Self::MapTryInto => Some(Self::MapTryInto),
            },
            Self::MapTryInto => match next {
                Self::Code(_) => None,
                _ => Some(Self::MapTryInto),
            },
        }
    }

    pub fn format(operations: impl Iterator<Item = Self>) -> String {
        use std::fmt::Write;
        let mut buf = String::new();
        let mut current: Option<Self> = None;
        for to_apply in operations {
            match current {
                Some(c) => match c.merge(&to_apply) {
                    Some(merged) => {
                        current = Some(merged);
                    }
                    None => {
                        write!(buf, "{c}").unwrap();
                        current = Some(to_apply);
                    }
                },
                None => {
                    current = Some(to_apply);
                }
            }
        }
        if let Some(c) = current {
            write!(buf, "{c}").unwrap();
        }
        buf
    }
}

impl std::fmt::Display for ToWasmBoundaryOperations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Code(code) => write!(f, "{code}"),
            Self::Into => write!(f, ".into()"),
            Self::TryInto => write!(f, ".try_into()"),
            Self::MapInto => write!(f, ".map(Into::into)"),
            Self::MapTryInto => write!(f, ".map(TryInto::try_into)"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum EnumVariantData {
    Inlined(RustRecord),
    RustType(RustType),
}

// rep is Optional - None means we just serialize raw, ie for type choices
#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: VariantIdent,
    pub data: EnumVariantData,
    pub serialize_as_embedded_group: bool,
}

impl EnumVariant {
    pub fn new(name: VariantIdent, rust_type: RustType, serialize_as_embedded_group: bool) -> Self {
        Self {
            name,
            data: EnumVariantData::RustType(rust_type),
            serialize_as_embedded_group,
        }
    }

    pub fn new_embedded(name: VariantIdent, embedded_record: RustRecord) -> Self {
        Self {
            name,
            data: EnumVariantData::Inlined(embedded_record),
            serialize_as_embedded_group: false,
        }
    }

    pub fn cbor_types(&self, types: &IntermediateTypes) -> Vec<CBORType> {
        match &self.data {
            EnumVariantData::RustType(ty) => ty.cbor_types(types),
            EnumVariantData::Inlined(record) => match record.rep {
                Representation::Array => vec![CBORType::Array],
                Representation::Map => vec![CBORType::Map],
            },
        }
    }

    // Can only be used on RustType variants, panics otherwise.
    // So don't call this when we're embedding the variant types
    pub fn rust_type(&self) -> &RustType {
        match &self.data {
            EnumVariantData::RustType(ty) => ty,
            EnumVariantData::Inlined(_) => {
                panic!("only call rust_type() when you know it can't be inlined")
            }
        }
    }

    pub fn name_as_var(&self) -> String {
        let snake = convert_to_snake_case(&self.name.to_string());
        // we can't use (rust) reserved keywords as param: eg new_u32(u32: u32)
        // TODO: do we need to cover any other (rust) reserved keywords?
        String::from(match snake.as_str() {
            "u8" | "u16" | "u32" | "u64" => "uint",
            "i8" | "i16" | "i32" | "i64" => "int",
            "f32" => "float32",
            "f64" => "float64",
            x => x,
        })
    }

    pub fn can_embed_fields(types: &IntermediateTypes, ty: &ConceptualRustType) -> bool {
        match ty {
            ConceptualRustType::Rust(ident) => {
                if let RustStructType::Record(record) = types.rust_struct(ident).unwrap().variant()
                {
                    return record
                        .fields
                        .iter()
                        .filter(|field| !field.rust_type.is_fixed_value())
                        .count()
                        <= 1;
                }
                false
            }
            ConceptualRustType::Alias(_, ty) => Self::can_embed_fields(types, ty),
            _ => false,
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
    pub(crate) variant: RustStructType,
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
    Wrapper {
        wrapped: RustType,
        min_max: Option<(Option<i128>, Option<i128>)>,
    },
    /// This is a no-op in generation but to prevent lookups of things in the prelude
    /// e.g. `int` from not being resolved while still being able to detect it when
    /// referring to a struct that doesn't exist even after generation.
    Extern,
    CStyleEnum {
        variants: Vec<EnumVariant>,
    },
    RawBytesType,
}

impl RustStruct {
    pub fn new_record(ident: RustIdent, tag: Option<usize>, record: RustRecord) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Record(record),
        }
    }

    pub fn new_table(
        ident: RustIdent,
        tag: Option<usize>,
        domain: RustType,
        range: RustType,
    ) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Table { domain, range },
        }
    }

    pub fn new_array(ident: RustIdent, tag: Option<usize>, element_type: RustType) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Array { element_type },
        }
    }

    /// This will automatically check if it's a c-stlye enum and use that instead if possible
    pub fn new_type_choice(
        ident: RustIdent,
        tag: Option<usize>,
        variants: Vec<EnumVariant>,
        cli: &Cli,
    ) -> Self {
        // we could potentially push these encoding vars out too but this is extremely low priority
        // unless people want to have tagged c-style enums encoded in different ways
        let cant_store_tag = tag.is_some() && cli.preserve_encodings;
        let not_fixed_or_cant_store_enc_vars_or_outer_len =
            variants.iter().any(|ev: &EnumVariant| {
                ev.serialize_as_embedded_group
                    || (cli.preserve_encodings && !ev.rust_type().encodings.is_empty())
                    || !matches!(
                        ev.rust_type().conceptual_type.resolve_alias_shallow(),
                        ConceptualRustType::Fixed(_)
                    )
            });
        if cant_store_tag
            || not_fixed_or_cant_store_enc_vars_or_outer_len
            || (cli.preserve_encodings && !enum_variants_have_same_encoding_var(&variants))
        {
            Self {
                ident,
                tag,
                variant: RustStructType::TypeChoice { variants },
            }
        } else {
            Self {
                ident,
                tag,
                variant: RustStructType::CStyleEnum { variants },
            }
        }
    }

    pub fn new_group_choice(
        ident: RustIdent,
        tag: Option<usize>,
        variants: Vec<EnumVariant>,
        rep: Representation,
    ) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::GroupChoice { variants, rep },
        }
    }

    pub fn new_wrapper(
        ident: RustIdent,
        tag: Option<usize>,
        wrapped_type: RustType,
        min_max: Option<(Option<i128>, Option<i128>)>,
    ) -> Self {
        Self {
            ident,
            tag,
            variant: RustStructType::Wrapper {
                wrapped: wrapped_type,
                min_max,
            },
        }
    }

    pub fn new_extern(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
            variant: RustStructType::Extern,
        }
    }

    pub fn new_raw_bytes(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
            variant: RustStructType::RawBytesType,
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
            RustStructType::Table { .. } => None,
            RustStructType::Array { .. } => None,
            // TODO: investigate if we should be supporting this for TypeChoice (also wrapper?)
            //RustStructType::TypeChoice { .. } => None,
            RustStructType::TypeChoice { .. } => {
                unreachable!("I don't think type choices should be using length?")
            }
            RustStructType::GroupChoice { .. } => {
                unreachable!("I don't think group choices should be using length?")
            }
            RustStructType::Wrapper { .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Extern => panic!(
                "do we need to look this up ever? will the prelude have structs with fields?"
            ),
            RustStructType::CStyleEnum { .. } => Some(1),
            RustStructType::RawBytesType => Some(1),
        }
    }

    // Even if fixed_field_count() == None, this will return an expression for
    // a definite length, e.g. with optional field checks in the expression
    // This is useful for definite-length serialization
    pub fn definite_info(&self, types: &IntermediateTypes, cli: &Cli) -> String {
        match &self.variant {
            RustStructType::Record(record) => record.definite_info(types, cli),
            RustStructType::Table { .. } => String::from("self.0.len() as u64"),
            RustStructType::Array { .. } => String::from("self.0.len() as u64"),
            RustStructType::TypeChoice { .. } => {
                unreachable!("I don't think type choices should be using length?")
            }
            RustStructType::GroupChoice { .. } => {
                unreachable!("I don't think group choices should be using length?")
            }
            RustStructType::Wrapper { .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Extern { .. } => panic!(
                "do we need to look this up ever? will the prelude have structs with fields?"
            ),
            RustStructType::CStyleEnum { .. } => "1".into(),
            RustStructType::RawBytesType { .. } => "1".into(),
        }
    }

    // the minimum cbor length of this struct - can be useful for deserialization length checks
    // does not count ANY type choice like types including Optional UNLESS the option Some type
    // has cbor len 1 too - to be consistent with expanded_field_count
    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        match &self.variant {
            RustStructType::Record(record) => record.expanded_mandatory_field_count(types),
            RustStructType::Table { .. } => 0,
            RustStructType::Array { .. } => 0,
            //RustStructType::TypeChoice{ .. } => 0,
            RustStructType::TypeChoice { .. } => {
                unreachable!("I don't think type choices should be using length?")
            }
            RustStructType::GroupChoice { .. } => {
                unreachable!("I don't think group choices should be using length?")
            }
            RustStructType::Wrapper { .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Extern { .. } => panic!(
                "do we need to look this up ever? will the prelude have structs with fields?"
            ),
            RustStructType::CStyleEnum { .. } => 1,
            RustStructType::RawBytesType => 1,
        }
    }

    fn _cbor_len_info(&self, types: &IntermediateTypes) -> RustStructCBORLen {
        match &self.variant {
            RustStructType::Record(record) => record.cbor_len_info(types),
            RustStructType::Table { .. } => RustStructCBORLen::Dynamic,
            RustStructType::Array { .. } => RustStructCBORLen::Dynamic,
            //RustStructType::TypeChoice{ .. } => RustStructCBORLen::Dynamic,
            RustStructType::TypeChoice { .. } => {
                unreachable!("I don't think type choices should be using length?")
            }
            RustStructType::GroupChoice { .. } => {
                unreachable!("I don't think group choices should be using length?")
            }
            RustStructType::Wrapper { .. } => unreachable!("wrapper types don't use length"),
            RustStructType::Extern { .. } => panic!(
                "do we need to look this up ever? will the prelude have structs with fields?"
            ),
            RustStructType::CStyleEnum { .. } => RustStructCBORLen::Fixed(1),
            RustStructType::RawBytesType => RustStructCBORLen::Fixed(1),
        }
    }

    pub fn visit_types<F: FnMut(&ConceptualRustType)>(&self, types: &IntermediateTypes, f: &mut F) {
        self.visit_types_excluding(types, f, &mut BTreeSet::new())
    }
    pub fn visit_types_excluding<F: FnMut(&ConceptualRustType)>(
        &self,
        types: &IntermediateTypes,
        f: &mut F,
        already_visited: &mut BTreeSet<RustIdent>,
    ) {
        match &self.variant {
            RustStructType::Array { element_type } => element_type
                .conceptual_type
                .visit_types_excluding(types, f, already_visited),
            RustStructType::GroupChoice { variants, .. }
            | RustStructType::TypeChoice { variants, .. }
            | RustStructType::CStyleEnum { variants } => {
                variants.iter().for_each(|v| match &v.data {
                    EnumVariantData::RustType(ty) => {
                        ty.conceptual_type
                            .visit_types_excluding(types, f, already_visited)
                    }
                    EnumVariantData::Inlined(record) => record.fields.iter().for_each(|field| {
                        field
                            .rust_type
                            .visit_types_excluding(types, f, already_visited)
                    }),
                })
            }
            RustStructType::Record(record) => record.fields.iter().for_each(|field| {
                field
                    .rust_type
                    .conceptual_type
                    .visit_types_excluding(types, f, already_visited)
            }),
            RustStructType::Table { domain, range } => {
                domain
                    .conceptual_type
                    .visit_types_excluding(types, f, already_visited);
                range
                    .conceptual_type
                    .visit_types_excluding(types, f, already_visited);
            }
            RustStructType::Wrapper { wrapped, .. } => wrapped
                .conceptual_type
                .visit_types_excluding(types, f, already_visited),
            RustStructType::Extern => (),
            RustStructType::RawBytesType => (),
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
                Representation::Array => field.rust_type.expanded_field_count(types)?,
                Representation::Map => 1,
            };
        }
        Some(count)
    }

    // This is guaranteed
    pub fn definite_info(&self, types: &IntermediateTypes, cli: &Cli) -> String {
        match self.fixed_field_count(types) {
            Some(count) => count.to_string(),
            None => {
                let mut fixed_field_count = 0;
                let mut conditional_field_expr = String::new();
                for field in &self.fields {
                    if field.optional {
                        if !cli.preserve_encodings && field.rust_type.is_fixed_value() {
                            // we don't create fields for fixed values when preserve-encodings=false
                            continue;
                        }
                        if !conditional_field_expr.is_empty() {
                            conditional_field_expr.push_str(" + ");
                        }
                        let (field_expr, field_contribution) = match self.rep {
                            Representation::Array => {
                                ("x", field.rust_type.definite_info("x", types, cli))
                            }
                            // maps are defined by their keys instead (although they shouldn't have multi-length values either...)
                            Representation::Map => ("_", String::from("1")),
                        };
                        if let Some(default_value) = &field.rust_type.config.default {
                            if cli.preserve_encodings {
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
                                    field_contribution
                                ));
                            }
                        } else {
                            conditional_field_expr.push_str(&format!(
                                "match &self.{} {{ Some({}) => {}, None => 0 }}",
                                field.name, field_expr, field_contribution
                            ));
                        }
                    } else {
                        match self.rep {
                            Representation::Array => {
                                match field.rust_type.expanded_field_count(types) {
                                    Some(field_expanded_count) => {
                                        fixed_field_count += field_expanded_count
                                    }
                                    None => {
                                        if !conditional_field_expr.is_empty() {
                                            conditional_field_expr.push_str(" + ");
                                        }
                                        let field_len_expr = field.rust_type.definite_info(
                                            &format!("self.{}", field.name),
                                            types,
                                            cli,
                                        );
                                        conditional_field_expr.push_str(&field_len_expr);
                                    }
                                }
                            }
                            Representation::Map => {
                                fixed_field_count += 1;
                            }
                        };
                    }
                }
                if conditional_field_expr.is_empty() || fixed_field_count != 0 {
                    format!("{fixed_field_count} + {conditional_field_expr}")
                } else {
                    conditional_field_expr
                }
            }
        }
    }

    pub fn expanded_mandatory_field_count(&self, types: &IntermediateTypes) -> usize {
        self.fields
            .iter()
            .filter(|field| !field.optional)
            .map(|field| field.rust_type.expanded_mandatory_field_count(types))
            .sum()
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
    pub fn new(
        instance_ident: RustIdent,
        generic_ident: RustIdent,
        generic_args: Vec<RustType>,
    ) -> Self {
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
            None => panic!(
                "Generic instance used on {} without definition",
                self.generic_ident
            ),
        };
        assert_eq!(def.generic_params.len(), self.generic_args.len());
        let resolved_args = def
            .generic_params
            .iter()
            .zip(self.generic_args.iter())
            .collect::<BTreeMap<&RustIdent, &RustType>>();
        let mut instance = def.orig.clone();
        instance.ident = self.instance_ident.clone();

        match &mut instance.variant {
            RustStructType::Record(record) => {
                for field in record.fields.iter_mut() {
                    field.rust_type = Self::resolve_type(&resolved_args, &field.rust_type);
                }
            }
            RustStructType::Table { domain, range } => {
                *domain = Self::resolve_type(&resolved_args, domain);
                *range = Self::resolve_type(&resolved_args, range);
            }
            RustStructType::Array { element_type } => {
                *element_type = Self::resolve_type(&resolved_args, element_type);
            }
            RustStructType::TypeChoice { variants } | RustStructType::CStyleEnum { variants } => {
                for variant in variants.iter_mut() {
                    match &mut variant.data {
                        EnumVariantData::RustType(ty) => {
                            *ty = Self::resolve_type(&resolved_args, ty);
                        }
                        EnumVariantData::Inlined(_) => unreachable!(),
                    }
                }
            }
            RustStructType::GroupChoice { .. } => {
                // for variant in variants.mut_iter() {
                //     variant.rust_type = Self::resolve_type(&resolved_args, &variant.rust_type);
                // }
                todo!("we might need to recursively resolve on these");
            }
            RustStructType::Wrapper { .. } => {
                todo!("should we look this up in types to resolve?");
            }
            RustStructType::Extern => {
                panic!("generics should not be used on types in the prelude (e.g. int)")
            }
            RustStructType::RawBytesType => {
                panic!("generics not supported on raw bytes types")
            }
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

fn enum_variant_constant(variant: &EnumVariant) -> Option<FixedValue> {
    if let EnumVariantData::RustType(ty) = &variant.data {
        if let ConceptualRustType::Fixed(constant) = ty.conceptual_type.resolve_alias_shallow() {
            return Some(constant.clone());
        }
    }
    None
}

pub fn enum_variants_have_same_encoding_var(variants: &[EnumVariant]) -> bool {
    variants
        .iter()
        .fold(
            variants.first().and_then(enum_variant_constant),
            |acc: Option<FixedValue>, ev: &EnumVariant| -> Option<FixedValue> {
                match (&acc, enum_variant_constant(ev)) {
                    // all these share the same encoding var type (Option<Sz>)
                    (
                        Some(FixedValue::Uint(_) | FixedValue::Nint(_) | FixedValue::Float(_)),
                        Some(FixedValue::Uint(_) | FixedValue::Nint(_) | FixedValue::Float(_)),
                    ) => acc,
                    // bytes would go here once it's supported
                    (Some(FixedValue::Text(_)), Some(FixedValue::Text(_))) => acc,
                    // these don't have any encoding vars
                    (
                        Some(FixedValue::Bool(_) | FixedValue::Null),
                        Some(FixedValue::Bool(_) | FixedValue::Null),
                    ) => acc,
                    _ => None,
                }
            },
        )
        .is_some()
}

/// Some when all variants have the same FixedValue variant (e.g. all Uint, all Nint, all Text, etc)
/// or None if they differ in any way (including encoding details)
pub fn _enum_variants_common_constant_type(variants: &[EnumVariant]) -> Option<FixedValue> {
    variants.iter().fold(
        variants.first().and_then(enum_variant_constant),
        |acc: Option<FixedValue>, ev: &EnumVariant| -> Option<FixedValue> {
            match &ev.data {
                EnumVariantData::Inlined(_) => return None,
                EnumVariantData::RustType(ty) => {
                    if !ty.encodings.is_empty() {
                        return None;
                    }
                }
            }
            match (&acc, enum_variant_constant(ev)) {
                (Some(FixedValue::Uint(_)), Some(FixedValue::Uint(_))) => acc,
                (Some(FixedValue::Nint(_)), Some(FixedValue::Nint(_))) => acc,
                (Some(FixedValue::Bool(_)), Some(FixedValue::Bool(_))) => acc,
                (Some(FixedValue::Float(_)), Some(FixedValue::Float(_))) => acc,
                (Some(FixedValue::Null), Some(FixedValue::Null)) => acc,
                (Some(FixedValue::Text(_)), Some(FixedValue::Text(_))) => acc,
                _ => None,
            }
        },
    )
}

#[allow(unused)]
fn try_ident_with_id(
    intermediate_types: &IntermediateTypes,
    name: &CDDLIdent,
    value: u32,
) -> CDDLIdent {
    let new_ident = CDDLIdent::new(format!("{name}{value}"));
    let rust_ident = RustIdent::new(new_ident.clone());
    match intermediate_types.has_ident(&rust_ident) {
        false => new_ident,
        true => try_ident_with_id(intermediate_types, name, value + 1),
    }
}
