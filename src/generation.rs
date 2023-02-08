use codegen::{Block, TypeAlias};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::process::{Command, Stdio};
use std::str::FromStr;

use crate::cli::CLI_ARGS;

use crate::intermediate::{
    AliasIdent, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, FixedValue,
    IntermediateTypes, Primitive, Representation, RustField, RustIdent, RustRecord,
    RustStructCBORLen, RustStructType, RustType, ToWasmBoundaryOperations, VariantIdent,
};
use crate::utils::convert_to_snake_case;

#[derive(Debug, Clone)]
struct SerializeConfig<'a> {
    /// the name of the variable where this is accessed, e.g. "self.foo" or "field" (e.g. for if let Some(field) = self.foo)
    expr: String,
    expr_is_ref: bool,
    /// used in generating *unique* identifiers from this. Must be unique within a type, e.g. field name: for the above it would be "foo" for both
    var_name: String,
    /// if true the final line should evaluate to Ok(serializer), or equivalent ie dropping last ?; from line
    is_end: bool,
    encoding_var_is_ref: bool,
    /// If the encoding var is contained within another sturct in an option e.g. encodings: Option<FooEncodings> within struct Foo
    encoding_var_in_option_struct: Option<String>,
    /// an overload instead of using "serializer". (name, is_local) - if is_local then &mut will be appended when needed.
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

    #[allow(clippy::wrong_self_convention)]
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

    fn encoding_var(&self, child: Option<&str>, is_copy: bool) -> String {
        let child_suffix = match child {
            Some(c) => format!("_{c}"),
            None => "".to_owned(),
        };
        let clone_call = if is_copy { "" } else { ".clone()" };
        match &self.encoding_var_in_option_struct {
            Some(namespace) => format!(
                "{}.as_ref().map(|encs| encs.{}{}_encoding{}).unwrap_or_default()",
                namespace, self.var_name, child_suffix, clone_call
            ),
            None => format!("{}{}_encoding", self.var_name, child_suffix),
        }
    }

    /// for looking up encoding vars stored within a Vec<T> / Map<K, V> and declaring them as local variables
    fn container_encoding_lookup(
        &self,
        prefix: &str,
        encoding_fields: &Vec<EncodingField>,
        var: &str,
    ) -> String {
        let encoding_lookup = match &self.encoding_var_in_option_struct {
            Some(namespace) => format!(
                "{}.as_ref().and_then(|encs| encs.{}_{}_encodings.get({})).cloned()",
                namespace, self.var_name, prefix, var
            ),
            None => format!(
                "{}_{}_encodings.get({}).cloned()",
                self.var_name, prefix, var
            ),
        };
        // for clippy::redundant_closure
        if encoding_fields.len() > 1 {
            format!(
                "let {} = {}.unwrap_or_else(|| {});",
                tuple_str(
                    encoding_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect()
                ),
                encoding_lookup,
                tuple_str(
                    encoding_fields
                        .iter()
                        .map(|enc| enc.default_expr.to_owned())
                        .collect()
                )
            )
        } else {
            format!(
                "let {} = {}.unwrap_or_default();",
                tuple_str(
                    encoding_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect()
                ),
                encoding_lookup
            )
        }
    }
}

#[derive(Debug, Clone)]
struct DeserializeConfig<'a> {
    /// for creating unique identifiers for temporaries
    var_name: &'a str,
    /// whether we're generating inside of a DeserializeEmbeddedGroup impl
    in_embedded: bool,
    /// whether this is an optional field
    optional_field: bool,
    /// Extra values associated with the deserialization to return as part of the final tuple e.g. (x, x_encoding, x_key_encodings)/
    /// This will be empty for root calls, but recursive ones
    /// might need to add in extra ones for encoding vars e.g. a tagged other value will have the tagged encoding
    /// var tacked on to the inner values.
    final_exprs: Vec<String>,
    /// Overload for the deserializer's name. Defaults to "raw"
    deserializer_name_overload: Option<&'a str>,
    /// Overload for read_len. This would be a local e.g. for arrays
    read_len_overload: Option<String>,
}

impl<'a> DeserializeConfig<'a> {
    fn new(var_name: &'a str) -> Self {
        Self {
            var_name,
            in_embedded: false,
            optional_field: false,
            final_exprs: Vec::new(),
            deserializer_name_overload: None,
            read_len_overload: None,
        }
    }

    fn in_embedded(mut self, in_embedded: bool) -> Self {
        self.in_embedded = in_embedded;
        self
    }

    fn optional_field(mut self, is_optional: bool) -> Self {
        self.optional_field = is_optional;
        self
    }

    fn overload_deserializer(mut self, overload: &'a str) -> Self {
        self.deserializer_name_overload = Some(overload);
        self
    }

    fn deserializer_name(&self) -> &'a str {
        self.deserializer_name_overload.unwrap_or("raw")
    }

    fn overload_read_len(mut self, overload: String) -> Self {
        self.read_len_overload = Some(overload);
        self
    }

    fn pass_read_len(&self) -> String {
        if let Some(overload) = &self.read_len_overload {
            // the ONLY way to have a name overload is if we have a local variable (e.g. arrays)
            format!("&mut {overload}")
        } else if self.in_embedded {
            "read_len".to_owned()
        } else {
            "&mut read_len".to_owned()
        }
    }
}

fn concat_files(paths: Vec<&str>) -> std::io::Result<String> {
    let mut buf = String::new();
    for path in paths {
        buf.push_str(
            &std::fs::read_to_string(path)
                .map_err(|_| panic!("can't read: {}", path))
                .unwrap(),
        );
    }
    Ok(buf)
}

#[derive(Debug)]
enum SerializingRustType<'a> {
    EncodingOperation(&'a CBOREncodingOperation, Box<SerializingRustType<'a>>),
    Root(&'a ConceptualRustType),
}

trait EncodingVarIsCopy {
    fn encoding_var_is_copy(&self) -> bool;
}

impl<'a> EncodingVarIsCopy for SerializingRustType<'a> {
    fn encoding_var_is_copy(&self) -> bool {
        match self {
            Self::EncodingOperation(CBOREncodingOperation::CBORBytes, _) => false,
            Self::EncodingOperation(CBOREncodingOperation::Tagged(_), _) => true,
            Self::Root(ty) => ty.encoding_var_is_copy(),
        }
    }
}

impl EncodingVarIsCopy for FixedValue {
    fn encoding_var_is_copy(&self) -> bool {
        match self {
            // bool / null have no encoding var
            Self::Bool(_) | Self::Nint(_) | Self::Null | Self::Float(_) | Self::Uint(_) => true,
            Self::Text(_) => false,
        }
    }
}

impl EncodingVarIsCopy for ConceptualRustType {
    fn encoding_var_is_copy(&self) -> bool {
        match self {
            // these are true (refers to the length encoding! not key/value/elem encodings as those are separate)
            Self::Array(_) => true,
            Self::Map(_, _) => true,
            Self::Fixed(fv) => fv.encoding_var_is_copy(),
            Self::Optional(ty) => SerializingRustType::from(&**ty).encoding_var_is_copy(),
            Self::Primitive(p) => match p {
                // bool has no encoding var
                Primitive::Bool
                | Primitive::F64
                | Primitive::F32
                | Primitive::I8
                | Primitive::I16
                | Primitive::I32
                | Primitive::I64
                | Primitive::U8
                | Primitive::U16
                | Primitive::U32
                | Primitive::U64
                | Primitive::N64 => true,
                Primitive::Bytes | Primitive::Str => false,
            },
            // technically no encoding var
            Self::Rust(_) => true,
            Self::Alias(_, ty) => ty.encoding_var_is_copy(),
        }
    }
}

impl<'a> std::convert::From<&'a RustType> for SerializingRustType<'a> {
    fn from(rust_type: &'a RustType) -> Self {
        let mut root = Self::Root(&rust_type.conceptual_type);
        for cbor_encoding_op in rust_type.encodings.iter() {
            root = Self::EncodingOperation(cbor_encoding_op, Box::new(root));
        }
        root
    }
}

impl<'a> std::convert::From<&'a ConceptualRustType> for SerializingRustType<'a> {
    fn from(conceptual_rust_type: &'a ConceptualRustType) -> Self {
        Self::Root(conceptual_rust_type)
    }
}

/// Output code for deserialization. Includes meta information for better usage to prevent warnings.
#[derive(Default)]
struct DeserializationCode {
    content: BlocksOrLines,
    read_len_used: bool,
    // whether ? is used in content
    throws: bool,
}

impl DeserializationCode {
    fn add_to<T>(self, body: &mut T)
    where
        T: CodeBlock + Sized,
    {
        body.push_all(self.content);
    }

    fn add_to_code(self, target: &mut Self) {
        if self.read_len_used {
            target.read_len_used = true;
        }
        if self.throws {
            target.throws = true;
        }
        target.content.push_all(self.content);
    }

    /// dumps self.content into {block} then uses {block} as our new content
    fn wrap_in_block(mut self, mut block: codegen::Block) -> Self {
        block.push_all(self.content);
        self.content = block.into();
        self
    }

    /// This MUST have self.content be a Result, as if you were going to wrap it in
    /// an error annotation lambda block. If possible this will avoid the need for
    /// the block to avoid clippy warnings.
    fn annotate(mut self, annotation: &str, before: &str, after: &str) -> Self {
        if let Some(single_line) = self.content.as_single_line() {
            self.content = BlocksOrLines(vec![BlockOrLine::Line(format!(
                "{before}{single_line}.map_err(|e: DeserializeError| e.annotate(\"{annotation}\")){after}"
            ))]);
            self
        } else {
            self.throws = false;
            self.wrap_in_block(make_err_annotate_block(annotation, before, after))
        }
    }

    fn mark_and_extract_content(self, target: &mut Self) -> BlocksOrLines {
        if self.read_len_used {
            target.read_len_used = true;
        }
        if self.throws {
            target.throws = true;
        }
        self.content
    }
}
/// Context as to how to generate deserialization code.
/// formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
/// * {let x = }{<value>}{;} - creation of variables
/// * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
/// * {}{<value>}{} - for last-expression eval in blocks
/// * etc
/// We also keep track of if it expects a result and can adjust the generated code based on that
/// to avoid warnings (e.g. avoid Ok(foo?) and directly do foo instead)
struct DeserializeBeforeAfter<'a> {
    before: &'a str,
    after: &'a str,
    expects_result: bool,
}

impl<'a> DeserializeBeforeAfter<'a> {
    fn new(before: &'a str, after: &'a str, expects_result: bool) -> Self {
        Self {
            before,
            after,
            expects_result,
        }
    }

    fn before_str(&self, is_result: bool) -> String {
        match (self.expects_result, is_result) {
            // T -> Result<T, _>
            (true, false) => format!("{}Ok(", self.before),
            // Result<T, _> => T (nothing to be done in before case)
            // (false, true) => self.before.to_owned(),
            // expected == found, nothing to be done
            (_, _) => self.before.to_owned(),
        }
    }

    fn after_str(&self, is_result: bool) -> String {
        match (self.expects_result, is_result) {
            // Result<T, _> -> T
            (false, true) => format!("?{}", self.after),
            // T ->
            (true, false) => format!("){}", self.before),
            // expected == found, nothing to be done
            (false, false) | (true, true) => self.after.to_owned(),
        }
    }
}

pub struct GenerationScope {
    rust_lib_scope: codegen::Scope,
    rust_scopes: BTreeMap<String, codegen::Scope>,
    serialize_scope: codegen::Scope,
    wasm_lib_scope: codegen::Scope,
    wasm_scopes: BTreeMap<String, codegen::Scope>,
    cbor_encodings_scope: codegen::Scope,
    json_scope: codegen::Scope,
    already_generated: BTreeSet<RustIdent>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            rust_lib_scope: codegen::Scope::new(),
            rust_scopes: BTreeMap::new(),
            serialize_scope: codegen::Scope::new(),
            wasm_lib_scope: codegen::Scope::new(),
            wasm_scopes: BTreeMap::new(),
            cbor_encodings_scope: codegen::Scope::new(),
            json_scope: codegen::Scope::new(),
            already_generated: BTreeSet::new(),
            no_deser_reasons: BTreeMap::new(),
        }
    }

    /// Generates, i.e. populates the state, based on `types`.
    /// this does not create any files, call export() after.
    pub fn generate(&mut self, types: &IntermediateTypes) {
        // Rust (populate rust_lib() - used for top of file before structs)
        self
            .rust_lib()
            .raw("#![allow(clippy::too_many_arguments)]\n")
            .raw("// This library was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n");
        // We can't generate groups of imports with codegen::Import so we just output this as raw text
        // since we don't need it to be dynamic so it's fine. codegen::Impl::new("a", "{z::b, z::c}")
        // does not work.
        if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
            self.rust_lib()
                .push_import("cbor_event", "self", None)
                .push_import("cbor_event::de", "Deserializer", None)
                .push_import("cbor_event::se", "Serializer", None);
        } else {
            self.rust_lib()
                .push_import("cbor_event", "self", None)
                .push_import("cbor_event::de", "Deserializer", None)
                .push_import("cbor_event::se", "Serialize", None)
                .push_import("cbor_event::se", "Serializer", None);
        }
        self.rust_lib()
            .push_import("std::io", "BufRead", None)
            .push_import("std::io", "Write", None)
            .push_import("std::io", "Write", None)
            .push_import("prelude", "*", None)
            .push_import("std::convert", "From", None)
            .push_import("std::convert", "TryFrom", None)
            .raw("pub mod prelude;")
            .raw("pub mod serialization;");
        if CLI_ARGS.preserve_encodings {
            self.rust_lib()
                .raw("pub mod ordered_hash_map;")
                .raw("pub mod cbor_encodings;")
                .raw("extern crate derivative;");
            // Issue (general - not just here): https://github.com/dcSpark/cddl-codegen/issues/139
            self.cbor_encodings().push_import("super", "*", None);
            self.rust_serialize()
                .push_import("crate::cbor_encodings", "*", None);
        }
        self.rust_serialize()
            .push_import("super", "*", None)
            .push_import("std::io", "Seek", None)
            .push_import("std::io", "SeekFrom", None);

        // Wasm (populate wasm_lib() - used for top of file before structs)
        if CLI_ARGS.wasm {
            self
                .wasm_lib()
                .raw("#![allow(clippy::len_without_is_empty, clippy::too_many_arguments, clippy::new_without_default)]");
        }

        // Type aliases
        for (alias_ident, alias_info) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias_ident {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if alias_info.gen_rust_alias {
                    self.rust(types, ident).push_type_alias(
                        TypeAlias::new(ident, alias_info.base_type.for_rust_member(types, false))
                            .vis("pub")
                            .clone(),
                    );
                }
                if alias_info.gen_wasm_alias {
                    // WASM crate
                    if let ConceptualRustType::Fixed(constant) =
                        &alias_info.base_type.conceptual_type
                    {
                        // wasm-bindgen doesn't support const or static vars so we must do a function
                        let (ty, val) = match constant {
                            FixedValue::Null => panic!("null constants not supported"),
                            FixedValue::Bool(b) => ("bool", b.to_string()),
                            FixedValue::Nint(i) => ("i32", i.to_string()),
                            FixedValue::Uint(u) => ("u32", u.to_string()),
                            FixedValue::Float(f) => ("f64", f.to_string()),
                            FixedValue::Text(s) => ("String", format!("\"{s}\".to_owned()")),
                        };
                        self.wasm(types, ident)
                            .new_fn(&convert_to_snake_case(ident.as_ref()))
                            .attr("wasm_bindgen")
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        self.wasm(types, ident).push_type_alias(
                            TypeAlias::new(ident, alias_info.base_type.for_wasm_member())
                                .vis("pub")
                                .clone(),
                        );
                    }
                }
            }
        }

        // Structs
        {
            // we can ignore types already handled by the alias
            // otherwise wasm_wrappers_generated may cause us to pointlessly create aliases to aliases
            let mut existing_aliases = types.type_aliases().iter().fold(
                BTreeSet::<RustIdent>::new(),
                |mut acc, (alias, _)| {
                    match alias {
                        AliasIdent::Reserved(_) => {}
                        AliasIdent::Rust(ident) => {
                            acc.insert(ident.clone());
                        }
                    };
                    acc
                },
            );

            let mut wasm_wrappers_generated = BTreeSet::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                assert_eq!(rust_ident, rust_struct.ident());
                if CLI_ARGS.wasm {
                    rust_struct.visit_types_excluding(
                        types,
                        &mut |ty| match ty {
                            ConceptualRustType::Array(elem) => {
                                if !ty.directly_wasm_exposable() {
                                    let array_ident = elem.name_as_wasm_array();
                                    if wasm_wrappers_generated.insert(array_ident.clone()) {
                                        self.generate_array_type(
                                            types,
                                            *elem.clone(),
                                            &RustIdent::new(CDDLIdent::new(array_ident)),
                                        );
                                    }
                                }
                            }
                            ConceptualRustType::Map(k, v) => {
                                let map_ident = ConceptualRustType::name_for_wasm_map(k, v);
                                if wasm_wrappers_generated.insert(map_ident.to_string()) {
                                    codegen_table_type(
                                        self,
                                        types,
                                        &map_ident,
                                        *k.clone(),
                                        *v.clone(),
                                        None,
                                        false,
                                    );
                                }
                                if !ConceptualRustType::Array(Box::new(*k.clone()))
                                    .directly_wasm_exposable()
                                {
                                    let keys_ident = k.name_as_wasm_array();
                                    if wasm_wrappers_generated.insert(keys_ident.clone()) {
                                        self.generate_array_type(
                                            types,
                                            *k.clone(),
                                            &RustIdent::new(CDDLIdent::new(keys_ident)),
                                        );
                                    }
                                }
                            }
                            _ => (),
                        },
                        &mut existing_aliases,
                    );
                }
                match rust_struct.variant() {
                    RustStructType::Record(record) => {
                        codegen_struct(self, types, rust_ident, rust_struct.tag(), record);
                    }
                    RustStructType::Table { domain, range } => {
                        if CLI_ARGS.wasm {
                            let map_ident = ConceptualRustType::name_for_wasm_map(domain, range);
                            // want to use `contains` instead of insert
                            // since although map_ident may not be required for this struct
                            // we may still have to generate it later if a table of the same shape is embedded inside different struct
                            if !wasm_wrappers_generated.contains(&map_ident.to_string()) {
                                codegen_table_type(
                                    self,
                                    types,
                                    rust_ident,
                                    domain.clone(),
                                    range.clone(),
                                    rust_struct.tag(),
                                    true,
                                );
                            } else {
                                self.wasm(types, rust_ident)
                                    .push_type_alias(TypeAlias::new(rust_ident, map_ident));
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), ConceptualRustType::name_for_rust_map(domain, range, false)));
                    }
                    RustStructType::Array { element_type } => {
                        if CLI_ARGS.wasm {
                            self.generate_array_type(types, element_type.clone(), rust_ident);
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), element_type.name_as_rust_array(false)));
                    }
                    RustStructType::TypeChoice { variants } => {
                        self.generate_type_choices_from_variants(
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                        );
                    }
                    RustStructType::GroupChoice { variants, rep } => codegen_group_choices(
                        self,
                        types,
                        rust_ident,
                        variants,
                        *rep,
                        rust_struct.tag(),
                    ),
                    RustStructType::Wrapper { wrapped, min_max } => match rust_struct.tag() {
                        Some(tag) => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            &wrapped.clone().tag(tag),
                            *min_max,
                        ),
                        None => generate_wrapper_struct(self, types, rust_ident, wrapped, *min_max),
                    },
                    RustStructType::Extern => {
                        #[allow(clippy::single_match)]
                        match rust_ident.to_string().as_ref() {
                            "Int" => {
                                if types.is_referenced(rust_ident) {
                                    generate_int(self, types)
                                }
                            }
                            _ => (), /* user-specified external types */
                        }
                    }
                }
            }
        }

        // JSON export
        if CLI_ARGS.json_schema_export {
            let mut gen_json_schema = Block::new("macro_rules! gen_json_schema");
            let mut macro_match = Block::new("($name:ty) => ");
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
            let mut main_lines_by_file: BTreeMap<String, Vec<String>> = BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                let is_typedef = matches!(
                    rust_struct.variant(),
                    RustStructType::Array { .. } | RustStructType::Table { .. }
                );
                // the is_referenced check is for things like Int which are included by default
                // in order for the CDDL to parse but might not be used.
                if !is_typedef && types.is_referenced(rust_ident) {
                    main_lines_by_file
                        .entry(types.scope(rust_ident).to_owned())
                        .or_default()
                        .push(format!(
                            "gen_json_schema!({});",
                            rust_crate_struct_from_wasm(types, rust_ident)
                        ));
                }
            }
            let multiple_files = main_lines_by_file.len() > 1;
            for (scope_name, lines) in main_lines_by_file {
                if multiple_files {
                    main.line(format!("// {scope_name}.rs"));
                }
                for line in lines {
                    main.line(line);
                }
            }
            self.json_scope.push_fn(main);
        }
    }

    /// Exports all already-generated state to the provided directory.
    /// Call generate() first to populate the generation state.
    pub fn export(&mut self, types: &IntermediateTypes) -> std::io::Result<()> {
        // package.json / scripts
        let rust_dir = if CLI_ARGS.package_json {
            if CLI_ARGS.json_schema_export {
                std::fs::create_dir_all(CLI_ARGS.output.join("scripts"))?;
                std::fs::copy(
                    "static/run-json2ts.js",
                    CLI_ARGS.output.join("scripts/run-json2ts.js"),
                )?;
                std::fs::copy(
                    "static/json-ts-types.js",
                    CLI_ARGS.output.join("scripts/json-ts-types.js"),
                )?;
                std::fs::copy(
                    "static/package_json_schemas.json",
                    CLI_ARGS.output.join("package.json"),
                )?;
            } else {
                std::fs::copy("static/package.json", CLI_ARGS.output.join("package.json"))?;
            }
            CLI_ARGS.output.join("rust")
        } else {
            CLI_ARGS.output.clone()
        };

        // lib.rs / other files (if input is a directory)
        std::fs::create_dir_all(rust_dir.join("rust/src"))?;
        let scope_names = self
            .rust_scopes
            .keys()
            .filter(|scope| *scope != "lib")
            .cloned()
            .collect::<Vec<_>>();
        let rust_imports = types.scope_references(false);
        for scope in scope_names {
            self.rust_lib().raw(&format!("pub mod {scope};"));
        }
        let mut merged_rust_scope = codegen::Scope::new();
        merged_rust_scope.append(self.rust_lib());
        // import encoding structs
        if CLI_ARGS.preserve_encodings {
            for (rust_ident, rust_struct) in types.rust_structs() {
                if matches!(
                    rust_struct.variant(),
                    RustStructType::Record(_) | RustStructType::Wrapper { .. }
                ) {
                    // ALL records have an encoding struct since at minimum they contian
                    // the array or map encoding details so no need to check fields
                    let cbor_encodings_scope = match types.scope(rust_ident) {
                        "lib" => "cbor_encodings",
                        _other_scope => "crate::cbor_encodings",
                    };
                    self.rust(types, rust_ident).push_import(
                        cbor_encodings_scope,
                        format!("{rust_ident}Encoding"),
                        None,
                    );
                }
            }
        }
        fn add_imports_from_scope_refs(
            scope: &String,
            content: &mut codegen::Scope,
            imports: &BTreeMap<String, BTreeMap<String, BTreeSet<RustIdent>>>,
        ) {
            // might not exist if we don't use stuff from other scopes
            if let Some(scope_imports) = imports.get(scope) {
                for (import_scope, idents) in scope_imports.iter() {
                    let import_scope = if import_scope == "lib" {
                        Cow::from("super")
                    } else if scope == "lib" {
                        Cow::from(import_scope)
                    } else {
                        Cow::from(format!("crate::{import_scope}"))
                    };
                    #[allow(clippy::comparison_chain)]
                    if idents.len() > 1 {
                        content.push_import(
                            import_scope,
                            format!(
                                "{{{}}}",
                                idents
                                    .iter()
                                    .map(|i| i.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            None,
                        );
                    } else if idents.len() == 1 {
                        content.push_import(
                            import_scope,
                            idents.first().unwrap().to_string(),
                            None,
                        );
                    }
                }
            }
        }
        // imports and write to file
        for (scope, content) in self.rust_scopes.iter_mut() {
            add_imports_from_scope_refs(scope, content, &rust_imports);
            // TODO: we blindly add these two map imports. Ideally we would only do it when needed
            // but the code to figure that out would be potentially complex.
            // Issue (general - not just here): https://github.com/dcSpark/cddl-codegen/issues/139
            content.push_import("std::collections", "BTreeMap", None);
            if CLI_ARGS.preserve_encodings {
                if scope == "lib" {
                    content.push_import("ordered_hash_map", "OrderedHashMap", None);
                } else {
                    content.push_import("crate::ordered_hash_map", "OrderedHashMap", None);
                }
            }
            if scope == "lib" {
                merged_rust_scope.append(&content.clone());
            } else {
                std::fs::write(
                    rust_dir.join(format!("rust/src/{scope}.rs")),
                    rustfmt_generated_string(&content.to_string())?.as_ref(),
                )?;
            }
        }
        std::fs::write(
            rust_dir.join("rust/src/lib.rs"),
            rustfmt_generated_string(&merged_rust_scope.to_string())?.as_ref(),
        )?;

        // serialiation.rs
        let mut serialize_paths = vec!["static/serialization.rs"];
        if CLI_ARGS.preserve_encodings {
            serialize_paths.push("static/serialization_preserve.rs");
            if CLI_ARGS.canonical_form {
                serialize_paths.push("static/serialization_preserve_force_canonical.rs");
            } else {
                serialize_paths.push("static/serialization_preserve_non_force_canonical.rs");
                serialize_paths.push("static/serialization_non_force_canonical.rs");
            }
        } else {
            serialize_paths.push("static/serialization_non_preserve.rs");
            serialize_paths.push("static/serialization_non_force_canonical.rs");
        }
        let mut serialize_contents = concat_files(serialize_paths)?;
        serialize_contents.push_str(&self.rust_serialize().to_string());
        std::fs::write(
            rust_dir.join("rust/src/serialization.rs"),
            rustfmt_generated_string(&serialize_contents)?.as_ref(),
        )?;

        // Cargo.toml
        let mut rust_cargo_toml = std::fs::read_to_string("static/Cargo_rust.toml")?;
        if CLI_ARGS.preserve_encodings {
            rust_cargo_toml.push_str("linked-hash-map = \"0.5.3\"\n");
            rust_cargo_toml.push_str("derivative = \"2.2.0\"\n");
        }
        if CLI_ARGS.json_serde_derives {
            rust_cargo_toml.push_str("serde = { version = \"1.0\", features = [\"derive\"] }\n");
            rust_cargo_toml.push_str("serde_json = \"1.0.57\"\n");
        }
        if CLI_ARGS.json_schema_export {
            rust_cargo_toml.push_str("schemars = \"0.8.8\"\n");
        }
        std::fs::write(
            rust_dir.join("rust/Cargo.toml"),
            rust_cargo_toml.replace("cddl-lib", &CLI_ARGS.lib_name),
        )?;

        // prelude.rs
        std::fs::copy("static/prelude.rs", rust_dir.join("rust/src/prelude.rs"))?;

        // cbor_encodings.rs + ordered_hash_map.rs
        if CLI_ARGS.preserve_encodings {
            std::fs::write(
                rust_dir.join("rust/src/cbor_encodings.rs"),
                rustfmt_generated_string(&self.cbor_encodings().to_string())?.as_ref(),
            )?;
            let mut ordered_hash_map_rs = std::fs::read_to_string("static/ordered_hash_map.rs")?;
            if CLI_ARGS.json_serde_derives {
                ordered_hash_map_rs
                    .push_str(&std::fs::read_to_string("static/ordered_hash_map_json.rs")?);
            }
            if CLI_ARGS.json_schema_export {
                ordered_hash_map_rs.push_str(&std::fs::read_to_string(
                    "static/ordered_hash_map_schemars.rs",
                )?);
            }
            std::fs::write(
                rust_dir.join("rust/src/ordered_hash_map.rs"),
                rustfmt_generated_string(&ordered_hash_map_rs)?.as_ref(),
            )?;
        }

        // wasm crate
        if CLI_ARGS.wasm {
            std::fs::create_dir_all(rust_dir.join("wasm/src"))?;
            let scope_names = self
                .wasm_scopes
                .keys()
                .filter(|scope| *scope != "lib")
                .cloned()
                .collect::<Vec<_>>();
            let wasm_imports = types.scope_references(true);
            for scope in scope_names {
                self.wasm_lib().raw(&format!("pub mod {scope};"));
            }
            let mut merged_wasm_scope = codegen::Scope::new();
            merged_wasm_scope.append(self.wasm_lib());
            for (scope, content) in self.wasm_scopes.iter_mut() {
                add_imports_from_scope_refs(scope, content, &wasm_imports);
                content
                    .push_import("wasm_bindgen::prelude", "wasm_bindgen", None)
                    .push_import("wasm_bindgen::prelude", "JsValue", None);
                if CLI_ARGS.preserve_encodings {
                    content.push_import(
                        format!("{}::ordered_hash_map", CLI_ARGS.lib_name_code()),
                        "OrderedHashMap",
                        None,
                    );
                } else {
                    content.push_import("std::collections", "BTreeMap", None);
                }
                if scope == "lib" {
                    merged_wasm_scope.append(&content.clone());
                } else {
                    std::fs::write(
                        rust_dir.join(format!("wasm/src/{scope}.rs")),
                        rustfmt_generated_string(&content.to_string())?.as_ref(),
                    )?;
                }
            }
            std::fs::write(
                rust_dir.join("wasm/src/lib.rs"),
                rustfmt_generated_string(&merged_wasm_scope.to_string())?.as_ref(),
            )?;
            let mut wasm_toml = std::fs::read_to_string("static/Cargo_wasm.toml")?;
            if CLI_ARGS.json_serde_derives {
                wasm_toml.push_str("serde_json = \"1.0.57\"\n");
                wasm_toml.push_str("serde-wasm-bindgen = \"0.4.5\"\n");
            }
            std::fs::write(
                rust_dir.join("wasm/Cargo.toml"),
                wasm_toml.replace("cddl-lib", &CLI_ARGS.lib_name),
            )?;
        }

        // json-gen crate for exporting JSON schemas
        if CLI_ARGS.json_schema_export {
            std::fs::create_dir_all(rust_dir.join("wasm/json-gen/src"))?;
            let json_gen_toml = std::fs::read_to_string(
                std::path::PathBuf::from_str("static/Cargo_json_gen.toml").unwrap(),
            )
            .unwrap();
            std::fs::write(
                rust_dir.join("wasm/json-gen/Cargo.toml"),
                json_gen_toml.replace("cddl-lib", &CLI_ARGS.lib_name),
            )?;
            std::fs::write(
                rust_dir.join("wasm/json-gen/src/main.rs"),
                rustfmt_generated_string(&self.json().to_string())?.as_ref(),
            )?;
        }

        Ok(())
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated structs and associated traits (besides serialization ones)
    pub fn rust(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.rust_scopes
            .entry(scope_name)
            .or_insert(codegen::Scope::new())
    }

    /// Scope header above the rest of the "lib" rust scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn rust_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_lib_scope
    }

    /// We dump all serialization into a "serialization" scope as these details
    /// aren't very helpful to users as long as they work so we don't clutter other files.
    pub fn rust_serialize(&mut self) -> &mut codegen::Scope {
        &mut self.serialize_scope
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated WASM wrapper structs and associated traits
    pub fn wasm(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.wasm_scopes
            .entry(scope_name)
            .or_insert(codegen::Scope::new())
    }

    /// Scope header above the rest of the "lib" WASM scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn wasm_lib(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_lib_scope
    }

    /// If used, all associated *Encoding structs.
    pub fn cbor_encodings(&mut self) -> &mut codegen::Scope {
        &mut self.cbor_encodings_scope
    }

    /// If used, scope for json-gen crate's main.rs
    pub fn json(&mut self) -> &mut codegen::Scope {
        &mut self.json_scope
    }

    /// Write code for serializing {serializing_rust_type} directly into {body}
    #[allow(clippy::only_used_in_recursion)]
    fn generate_serialize(
        &mut self,
        types: &IntermediateTypes,
        serializing_rust_type: SerializingRustType<'_>,
        body: &mut dyn CodeBlock,
        config: SerializeConfig,
    ) {
        //body.line(&format!("// DEBUG - generated from: {:?}", rust_type));
        let line_ender = if config.is_end { "" } else { "?;" };
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
        let (serializer_use, serializer_pass) = config
            .serializer_name_overload
            .map(|(name, is_local)| {
                if is_local {
                    (name, format!("&mut {name}"))
                } else {
                    (name, name.to_owned())
                }
            })
            .unwrap_or(("serializer", "serializer".to_owned()));
        let encoding_deref = if config.encoding_var_is_ref { "*" } else { "" };
        let encoding_var_is_copy = serializing_rust_type.encoding_var_is_copy();
        let encoding_var = config.encoding_var(None, encoding_var_is_copy);
        let encoding_var_deref = format!("{encoding_deref}{encoding_var}");
        match serializing_rust_type {
            SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
                let expr = format!("{tag}u64");
                write_using_sz(
                    body,
                    "write_tag",
                    serializer_use,
                    &expr,
                    &expr,
                    "?;",
                    &format!(
                        "{}{}",
                        encoding_deref,
                        config.encoding_var(Some("tag"), encoding_var_is_copy)
                    ),
                );
                self.generate_serialize(types, *child, body, config);
            }
            SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
                let inner_se = format!("{}_inner_se", config.var_name);
                body.line(&format!("let mut {inner_se} = Serializer::new_vec();"));
                let inner_config = config
                    .clone()
                    .is_end(false)
                    .serializer_name_overload((&inner_se, true));
                self.generate_serialize(types, *child, body, inner_config);
                body.line(&format!(
                    "let {}_bytes = {}.finalize();",
                    config.var_name, inner_se
                ));
                write_string_sz(
                    body,
                    "write_bytes",
                    serializer_use,
                    &format!("{}_bytes", config.var_name),
                    line_ender,
                    &config.encoding_var(Some("bytes"), encoding_var_is_copy),
                );
            }
            SerializingRustType::Root(ConceptualRustType::Fixed(value)) => match value {
                FixedValue::Null => {
                    body.line(&format!(
                        "{serializer_use}.write_special(cbor_event::Special::Null){line_ender}"
                    ));
                }
                FixedValue::Bool(b) => {
                    body.line(&format!(
                        "{serializer_use}.write_special(cbor_event::Special::Bool({b})){line_ender}"
                    ));
                }
                FixedValue::Uint(u) => {
                    let expr = format!("{u}u64");
                    write_using_sz(
                        body,
                        "write_unsigned_integer",
                        serializer_use,
                        &expr,
                        &expr,
                        line_ender,
                        &encoding_var_deref,
                    );
                }
                FixedValue::Nint(i) => {
                    assert!(*i < 0);
                    if !CLI_ARGS.preserve_encodings
                        && isize::BITS >= i64::BITS
                        && *i <= i64::MIN as isize
                    {
                        // cbor_event's write_negative_integer doesn't support serializing i64::MIN (https://github.com/primetype/cbor_event/issues/9)
                        // we need to use the write_negative_integer_sz endpoint which does support it.
                        // the bits check is since the constant parsed by cddl might not even be able to
                        // be that small e.g. on 32-bit platforms in which case we're already working with garbage
                        let sz_str = if *i >= -24 {
                            "cbor_event::Sz::Inline"
                        } else if *i >= -0x1_00 {
                            "cbor_event::Sz::One"
                        } else if *i >= -0x1_00_00 {
                            "cbor_event::Sz::Two"
                        } else if *i >= -0x1_00_00_00_00 {
                            "cbor_event::Sz::Four"
                        } else {
                            "cbor_event::Sz::Eight"
                        };
                        body.line(&format!(
                            "{serializer_use}.write_negative_integer_sz({i}i128, {sz_str}){line_ender}"
                        ));
                    } else {
                        write_using_sz(
                            body,
                            "write_negative_integer",
                            serializer_use,
                            &i.to_string(),
                            &format!("({i}i128 + 1).abs() as u64"),
                            line_ender,
                            &encoding_var_deref,
                        );
                    }
                }
                FixedValue::Float(f) => {
                    body.line(&format!(
                        "{serializer_use}.write_special(cbor_event::Special::Float({f})){line_ender}"
                    ));
                }
                FixedValue::Text(s) => {
                    write_string_sz(
                        body,
                        "write_text",
                        serializer_use,
                        &format!("\"{s}\""),
                        line_ender,
                        &encoding_var,
                    );
                }
            },
            SerializingRustType::Root(ConceptualRustType::Primitive(primitive)) => {
                match primitive {
                    Primitive::Bool => {
                        body.line(&format!(
                            "{serializer_use}.write_special(cbor_event::Special::Bool({expr_deref})){line_ender}"
                        ));
                    }
                    Primitive::F32 => {
                        body.line(&format!(
                            "{serializer_use}.write_special(cbor_event::Special::Float({expr_deref} as f64)){line_ender}"
                        ));
                    }
                    Primitive::F64 => {
                        body.line(&format!(
                            "{serializer_use}.write_special(cbor_event::Special::Float({expr_deref})){line_ender}"
                        ));
                    }
                    Primitive::Bytes => {
                        write_string_sz(
                            body,
                            "write_bytes",
                            serializer_use,
                            &config.expr,
                            line_ender,
                            &encoding_var,
                        );
                    }
                    Primitive::Str => {
                        write_string_sz(
                            body,
                            "write_text",
                            serializer_use,
                            &config.expr,
                            line_ender,
                            &encoding_var,
                        );
                    }
                    Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64 => {
                        let mut pos = Block::new(format!("if {expr_deref} >= 0"));
                        let expr_pos = format!("{expr_deref} as u64");
                        write_using_sz(
                            &mut pos,
                            "write_unsigned_integer",
                            serializer_use,
                            &expr_pos,
                            &expr_pos,
                            line_ender,
                            &encoding_var_deref,
                        );
                        body.push_block(pos);
                        let mut neg = Block::new("else");
                        // only the _sz variants support i128, the other endpoint is i64
                        let expr = if CLI_ARGS.preserve_encodings {
                            format!("{expr_deref} as i128")
                        } else {
                            format!("{expr_deref} as i64")
                        };
                        if !CLI_ARGS.preserve_encodings && *primitive == Primitive::I64 {
                            // https://github.com/primetype/cbor_event/issues/9
                            // cbor_event doesn't support i64::MIN on write_negative_integer() so we use write_negative_integer_sz() for i64s
                            // even when not preserving encodings
                            neg.line(&format!("{serializer_use}.write_negative_integer_sz({expr_deref} as i128, cbor_event::Sz::canonical(({expr_deref} + 1).abs() as u64)){line_ender}"));
                        } else {
                            write_using_sz(
                                &mut neg,
                                "write_negative_integer",
                                serializer_use,
                                &expr,
                                &format!("({expr_deref} + 1).abs() as u64"),
                                line_ender,
                                &encoding_var_deref,
                            );
                        }
                        body.push_block(neg);
                    }
                    Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                        let expr = format!("{expr_deref} as u64");
                        write_using_sz(
                            body,
                            "write_unsigned_integer",
                            serializer_use,
                            &expr,
                            &expr,
                            line_ender,
                            &encoding_var_deref,
                        );
                    }
                    Primitive::U64 => {
                        write_using_sz(
                            body,
                            "write_unsigned_integer",
                            serializer_use,
                            &expr_deref,
                            &expr_deref,
                            line_ender,
                            &encoding_var_deref,
                        );
                    }
                    Primitive::N64 => {
                        if CLI_ARGS.preserve_encodings {
                            write_using_sz(
                                body,
                                "write_negative_integer",
                                serializer_use,
                                &format!("-({expr_deref} as i128 + 1)"),
                                &expr_deref,
                                line_ender,
                                &encoding_var_deref,
                            );
                        } else {
                            // https://github.com/primetype/cbor_event/issues/9
                            // cbor_event doesn't support i64::MIN on write_negative_integer() so we use write_negative_integer_sz()
                            // even when not preserving encodings
                            body.line(&format!("{serializer_use}.write_negative_integer_sz(-({expr_deref} as i128 + 1), cbor_event::Sz::canonical({expr_deref})){line_ender}"));
                        }
                    }
                }
            }
            SerializingRustType::Root(ConceptualRustType::Rust(t)) => {
                if types.is_plain_group(t) {
                    body.line(&format!(
                        "{}.serialize_as_embedded_group({}{}){}",
                        config.expr,
                        serializer_pass,
                        canonical_param(),
                        line_ender
                    ));
                } else {
                    body.line(&format!(
                        "{}.serialize({}{}){}",
                        config.expr,
                        serializer_pass,
                        canonical_param(),
                        line_ender
                    ));
                }
            }
            SerializingRustType::Root(ConceptualRustType::Array(ty)) => {
                let len_expr = match &ty.conceptual_type {
                    ConceptualRustType::Rust(elem_ident) if types.is_plain_group(elem_ident) => {
                        // you should not be able to indiscriminately encode a plain group like this as it
                        // could be multiple elements. This would require special handling if it's even permitted in CDDL.
                        assert!(ty.encodings.is_empty());
                        if let Some(fixed_elem_size) =
                            ty.conceptual_type.expanded_field_count(types)
                        {
                            format!("{} * {}.len() as u64", fixed_elem_size, config.expr)
                        } else {
                            format!(
                                "{}.iter().map(|e| {}).sum()",
                                config.expr,
                                ty.conceptual_type.definite_info("e", types)
                            )
                        }
                    }
                    _ => format!("{}.len() as u64", config.expr),
                };
                start_len(
                    body,
                    Representation::Array,
                    serializer_use,
                    &encoding_var,
                    &len_expr,
                );
                let elem_var_name = format!("{}_elem", config.var_name);
                let elem_encs = if CLI_ARGS.preserve_encodings {
                    encoding_fields(types, &elem_var_name, &ty.clone().resolve_aliases(), false)
                } else {
                    vec![]
                };
                let mut loop_block = if !elem_encs.is_empty() {
                    let mut block = Block::new(format!(
                        "for (i, element) in {}.iter().enumerate()",
                        config.expr
                    ));
                    block.line(config.container_encoding_lookup("elem", &elem_encs, "i"));
                    block
                } else {
                    Block::new(format!("for element in {}.iter()", config.expr))
                };
                let elem_config = config
                    .clone()
                    .expr("element")
                    .expr_is_ref(true)
                    .var_name(elem_var_name)
                    .is_end(false)
                    .encoding_var_no_option_struct()
                    .encoding_var_is_ref(false);
                self.generate_serialize(types, (&**ty).into(), &mut loop_block, elem_config);
                body.push_block(loop_block);
                end_len(body, serializer_use, &encoding_var, config.is_end);
            }
            SerializingRustType::Root(ConceptualRustType::Map(key, value)) => {
                start_len(
                    body,
                    Representation::Map,
                    serializer_use,
                    &encoding_var,
                    &format!("{}.len() as u64", config.expr),
                );
                let ser_loop = if CLI_ARGS.preserve_encodings {
                    let key_enc_fields = encoding_fields(
                        types,
                        &format!("{}_key", config.var_name),
                        &key.clone().resolve_aliases(),
                        false,
                    );
                    let value_enc_fields = encoding_fields(
                        types,
                        &format!("{}_value", config.var_name),
                        &value.clone().resolve_aliases(),
                        false,
                    );
                    let mut ser_loop = if CLI_ARGS.canonical_form {
                        let mut key_order = codegen::Block::new(format!(
                            "let mut key_order = {}.iter().map(|(k, v)|",
                            config.expr
                        ));
                        key_order.line("let mut buf = cbor_event::se::Serializer::new_vec();");
                        if !key_enc_fields.is_empty() {
                            key_order.line(config.container_encoding_lookup(
                                "key",
                                &key_enc_fields,
                                "k",
                            ));
                        }
                        let key_config =
                            SerializeConfig::new("k", format!("{}_key", config.var_name))
                                .expr_is_ref(true)
                                .is_end(false)
                                .serializer_name_overload(("buf", true))
                                .encoding_var_is_ref(false);
                        self.generate_serialize(types, (&**key).into(), &mut key_order, key_config);
                        key_order.line("Ok((buf.finalize(), k, v))").after(
                            ").collect::<Result<Vec<(Vec<u8>, &_, &_)>, cbor_event::Error>>()?;",
                        );
                        body.push_block(key_order);
                        let mut key_order_if = codegen::Block::new("if force_canonical");
                        let mut key_order_sort = codegen::Block::new(
                            "key_order.sort_by(|(lhs_bytes, _, _), (rhs_bytes, _, _)|",
                        );
                        let mut key_order_sort_match =
                            codegen::Block::new("match lhs_bytes.len().cmp(&rhs_bytes.len())");
                        key_order_sort_match
                            .line("std::cmp::Ordering::Equal => lhs_bytes.cmp(rhs_bytes),")
                            .line("diff_ord => diff_ord,");
                        key_order_sort.push_block(key_order_sort_match).after(");");
                        key_order_if.push_block(key_order_sort);
                        body.push_block(key_order_if);
                        let key_loop_var = if value_enc_fields.is_empty() {
                            "_key"
                        } else {
                            "key"
                        };
                        let mut ser_loop = Block::new(format!(
                            "for (key_bytes, {key_loop_var}, value) in key_order"
                        ));
                        ser_loop.line(format!("{serializer_use}.write_raw_bytes(&key_bytes)?;"));
                        ser_loop
                    } else {
                        let mut ser_loop =
                            Block::new(format!("for (key, value) in {}.iter()", config.expr));
                        if !key_enc_fields.is_empty() {
                            ser_loop.line(config.container_encoding_lookup(
                                "key",
                                &key_enc_fields,
                                "key",
                            ));
                        }
                        let key_config = config
                            .clone()
                            .expr("key")
                            .expr_is_ref(true)
                            .var_name(format!("{}_key", config.var_name))
                            .is_end(false)
                            .encoding_var_no_option_struct()
                            .encoding_var_is_ref(false);
                        self.generate_serialize(types, (&**key).into(), &mut ser_loop, key_config);
                        ser_loop
                    };
                    if !value_enc_fields.is_empty() {
                        ser_loop.line(config.container_encoding_lookup(
                            "value",
                            &value_enc_fields,
                            "key",
                        ));
                    }
                    let value_config = config
                        .clone()
                        .expr("value")
                        .expr_is_ref(true)
                        .var_name(format!("{}_value", config.var_name))
                        .is_end(false)
                        .encoding_var_no_option_struct()
                        .encoding_var_is_ref(false);
                    self.generate_serialize(types, (&**value).into(), &mut ser_loop, value_config);
                    ser_loop
                } else {
                    let mut ser_loop =
                        Block::new(format!("for (key, value) in {}.iter()", config.expr));
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
                    self.generate_serialize(types, (&**key).into(), &mut ser_loop, key_config);
                    self.generate_serialize(types, (&**value).into(), &mut ser_loop, value_config);
                    ser_loop
                };
                body.push_block(ser_loop);
                end_len(body, serializer_use, &encoding_var, config.is_end);
            }
            SerializingRustType::Root(ConceptualRustType::Optional(ty)) => {
                let mut opt_block = Block::new(format!("match {expr_ref}"));
                // TODO: do this in one line without a block if possible somehow.
                //       see other comment in generate_enum()
                let mut some_block = Block::new("Some(x) =>");
                let opt_config = config.clone().expr("x").expr_is_ref(true).is_end(true);
                self.generate_serialize(types, (&**ty).into(), &mut some_block, opt_config);
                some_block.after(",");
                opt_block.push_block(some_block);
                opt_block.line(&format!(
                    "None => {serializer_use}.write_special(cbor_event::Special::Null),"
                ));
                if !config.is_end {
                    opt_block.after("?;");
                }
                body.push_block(opt_block);
            }
            SerializingRustType::Root(ConceptualRustType::Alias(_ident, ty)) => {
                self.generate_serialize(types, (&**ty).into(), body, config)
            }
        };
    }

    /// Generates a DeserializationCode to serialize {serializing_rust_type} using the context in {before_after}
    /// This returned value must be in turn pushed into deserialization code to be used.
    #[must_use]
    fn generate_deserialize(
        &mut self,
        types: &IntermediateTypes,
        serializing_rust_type: SerializingRustType,
        before_after: DeserializeBeforeAfter,
        mut config: DeserializeConfig,
    ) -> DeserializationCode {
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        if !CLI_ARGS.preserve_encodings {
            assert!(config.final_exprs.is_empty());
        }
        let mut deser_code = DeserializationCode::default();
        // joins all config.final_expr together (possibly) with the actual value into a tuple type (if multiple)
        // or otherwise the value just goes through on its own
        let final_expr =
            |mut encoding_exprs: Vec<String>, actual_value: Option<String>| -> String {
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
        // Gives a total final expression including the before_after context
        // as well as dealing with avoiding clippy warning which is why we can
        // be conditionally a direct value (if there are encoding vars thus a tuple)
        // or we can be a result that goes straight through (subject to before_after)
        // This helps avoid clippy::needless_question_mark here.
        let final_result_expr_complete =
            |throws: &mut bool, final_exprs: Vec<String>, result_expr: &str| -> String {
                if final_exprs.is_empty() {
                    format!(
                        "{}{}{}",
                        before_after.before_str(true),
                        result_expr,
                        before_after.after_str(true)
                    )
                } else {
                    *throws = true;
                    format!(
                        "{}{}{}",
                        before_after.before_str(false),
                        final_expr(final_exprs, Some(format!("{result_expr}?"))),
                        before_after.after_str(false)
                    )
                }
            };
        let deserializer_name = config.deserializer_name();
        match serializing_rust_type {
            SerializingRustType::Root(ConceptualRustType::Fixed(f)) => {
                if !CLI_ARGS.preserve_encodings {
                    // we don't evaluate to any values here, just verify
                    // before/after are ignored and we need to handle fixed value deserialization in a different way
                    // than normal ones.
                    assert_eq!(before_after.after, "");
                    assert_eq!(before_after.before, "");
                }
                if config.optional_field {
                    deser_code.content.line("read_len.read_elems(1)?;");
                    deser_code.throws = true;
                    deser_code.read_len_used = true;
                }
                match f {
                    FixedValue::Null => {
                        let mut special_block = Block::new(format!(
                            "if {deserializer_name}.special()? != cbor_event::Special::Null"
                        ));
                        special_block.line("return Err(DeserializeFailure::ExpectedNull.into());");
                        deser_code.content.push_block(special_block);
                        if CLI_ARGS.preserve_encodings {
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, None),
                                before_after.after_str(false)
                            ));
                        }
                    }
                    FixedValue::Uint(x) => {
                        if CLI_ARGS.preserve_encodings {
                            deser_code.content.line(&format!(
                                "let ({}_value, {}_encoding) = {}.unsigned_integer_sz()?;",
                                config.var_name, config.var_name, deserializer_name
                            ));
                        } else {
                            deser_code.content.line(&format!(
                                "let {}_value = {}.unsigned_integer()?;",
                                config.var_name, deserializer_name
                            ));
                        }
                        let mut compare_block =
                            Block::new(format!("if {}_value != {}", config.var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint({}_value), expected: Key::Uint({}) }}.into());", config.var_name, x));
                        deser_code.content.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            config
                                .final_exprs
                                .push(format!("Some({}_encoding)", config.var_name));
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, None),
                                before_after.after_str(false)
                            ));
                            //body.line(&format!("{}{}{}_encoding{}{}", before, sp, var_name, ep, after));
                        }
                    }
                    FixedValue::Nint(x) => {
                        if CLI_ARGS.preserve_encodings {
                            deser_code.content.line(&format!(
                                "let ({}_value, {}_encoding) = {}.negative_integer_sz()?;",
                                config.var_name, config.var_name, deserializer_name
                            ));
                        } else {
                            // we use the _sz variant here too to get around imcomplete nint support in the regular negative_integer()
                            deser_code.content.line(&format!(
                                "let ({}_value, _) = {}.negative_integer_sz()?;",
                                config.var_name, deserializer_name
                            ));
                        }
                        let x_abs = (x + 1).abs();
                        let mut compare_block =
                            Block::new(format!("if {}_value != {}", config.var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint(({}_value + 1).abs() as u64), expected: Key::Uint({}) }}.into());", config.var_name, x_abs));
                        deser_code.content.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            config
                                .final_exprs
                                .push(format!("Some({}_encoding)", config.var_name));
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, None),
                                before_after.after_str(false)
                            ));
                            //body.line(&format!("{}{}{}_encoding{}{}", before, sp, var_name, ep, after));
                        }
                    }
                    FixedValue::Text(x) => {
                        if CLI_ARGS.preserve_encodings {
                            deser_code.content.line(&format!(
                                "let ({}_value, {}_encoding) = {}.text_sz()?;",
                                config.var_name, config.var_name, deserializer_name
                            ));
                        } else {
                            deser_code.content.line(&format!(
                                "let {}_value = {}.text()?;",
                                config.var_name, deserializer_name
                            ));
                        }
                        let mut compare_block =
                            Block::new(format!("if {}_value != \"{}\"", config.var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Str({}_value), expected: Key::Str(String::from(\"{}\")) }}.into());", config.var_name, x));
                        deser_code.content.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            config.final_exprs.push(format!(
                                "StringEncoding::from({}_encoding)",
                                config.var_name
                            ));
                            deser_code.content.line(&format!(
                                "{}{}{}",
                                before_after.before_str(false),
                                final_expr(config.final_exprs, None),
                                before_after.after_str(false)
                            ));
                        }
                    }
                    FixedValue::Float(x) => {
                        deser_code.content.line(&format!(
                            "let {}_value = {}.float()?;",
                            config.var_name, deserializer_name
                        ));
                        let mut compare_block =
                            Block::new(format!("if {}_value != {}", config.var_name, x));
                        compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Float({}_value), expected: Key::Float({}) }}.into());", config.var_name, x));
                        deser_code.content.push_block(compare_block);
                        if CLI_ARGS.preserve_encodings {
                            unimplemented!("preserve_encodings is not implemented for float")
                        }
                    }
                    _ => unimplemented!(),
                };
                deser_code.throws = true;
            }
            SerializingRustType::Root(ConceptualRustType::Primitive(p)) => {
                if config.optional_field {
                    deser_code.content.line("read_len.read_elems(1)?;");
                    deser_code.read_len_used = true;
                    deser_code.throws = true;
                }
                let error_convert = if before_after.expects_result {
                    ".map_err(Into::<DeserializeError>::into)"
                } else {
                    ""
                };
                let mut deser_primitive =
                    |mut final_exprs: Vec<String>, func: &str, x: &str, x_expr: &str| {
                        if CLI_ARGS.preserve_encodings {
                            let enc_expr = match func {
                                "text" | "bytes" => "StringEncoding::from(enc)",
                                _ => "Some(enc)",
                            };
                            final_exprs.push(enc_expr.to_owned());
                            deser_code.content.line(&format!(
                                "{}{}.{}_sz().map(|({}, enc)| {}){}{}",
                                before_after.before_str(true),
                                deserializer_name,
                                func,
                                x,
                                final_expr(final_exprs, Some(x_expr.to_owned())),
                                error_convert,
                                before_after.after_str(true)
                            ));
                        } else {
                            deser_code.content.line(&format!(
                                "{}{}.{}()? as {}{}",
                                before_after.before_str(false),
                                deserializer_name,
                                func,
                                p.to_string(),
                                before_after.after_str(false)
                            ));
                            deser_code.throws = true;
                        }
                    };
                match p {
                    Primitive::Bytes => {
                        deser_primitive(config.final_exprs, "bytes", "bytes", "bytes")
                    }
                    Primitive::U8 | Primitive::U16 | Primitive::U32 => deser_primitive(
                        config.final_exprs,
                        "unsigned_integer",
                        "x",
                        &format!("x as {}", p.to_string()),
                    ),
                    Primitive::U64 => {
                        deser_primitive(config.final_exprs, "unsigned_integer", "x", "x")
                    }
                    Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64 => {
                        let mut type_check = Block::new(format!(
                            "{}match {}.cbor_type()?",
                            before_after.before_str(false),
                            deserializer_name
                        ));
                        if CLI_ARGS.preserve_encodings {
                            let mut pos = Block::new("cbor_event::Type::UnsignedInteger =>");
                            pos.line(&format!(
                                "let (x, enc) = {deserializer_name}.unsigned_integer_sz()?;"
                            ))
                            .line(format!("(x as {}, Some(enc))", p.to_string()))
                            .after(",");
                            type_check.push_block(pos);
                            // let this cover both the negative int case + error case
                            let mut neg = Block::new("_ =>");
                            neg.line(&format!(
                                "let (x, enc) = {deserializer_name}.negative_integer_sz()?;"
                            ))
                            .line(format!("(x as {}, Some(enc))", p.to_string()))
                            .after(",");
                            type_check.push_block(neg);
                        } else {
                            type_check
                                .line(format!("cbor_event::Type::UnsignedInteger => {}.unsigned_integer()? as {},", deserializer_name, p.to_string()));
                            // https://github.com/primetype/cbor_event/issues/9
                            // cbor_event's negative_integer() doesn't support i64::MIN so we use the _sz function here instead as that one supports all nints
                            if *p == Primitive::I64 {
                                type_check.line(format!(
                                    "_ => {}.negative_integer_sz().map(|(x, _enc)| x)? as {},",
                                    deserializer_name,
                                    p.to_string()
                                ));
                            } else {
                                type_check.line(format!(
                                    "_ => {}.negative_integer()? as {},",
                                    deserializer_name,
                                    p.to_string()
                                ));
                            }
                        }
                        type_check.after(&before_after.after_str(false));
                        deser_code.content.push_block(type_check);
                        deser_code.throws = true;
                    }
                    Primitive::N64 => {
                        if CLI_ARGS.preserve_encodings {
                            deser_primitive(
                                config.final_exprs,
                                "negative_integer",
                                "x",
                                "(x + 1).abs() as u64",
                            )
                        } else {
                            // https://github.com/primetype/cbor_event/issues/9
                            // cbor_event's negative_integer() doesn't support full nint range so we use the _sz function here instead as that one supports all nints
                            deser_code.content.line(&format!("{}{}.negative_integer_sz().map(|(x, _enc)| (x + 1).abs() as u64){}{}", before_after.before_str(true), deserializer_name, error_convert, before_after.after_str(true)));
                        }
                    }
                    Primitive::Str => deser_primitive(config.final_exprs, "text", "s", "s"),
                    Primitive::Bool => {
                        // no encoding differences for bool
                        deser_code.content.line(&final_result_expr_complete(
                            &mut deser_code.throws,
                            config.final_exprs,
                            "raw.bool()",
                        ));
                    }
                    Primitive::F32 => {
                        deser_code.content.line(&final_result_expr_complete(
                            &mut deser_code.throws,
                            config.final_exprs,
                            "f32::deserialize(raw)",
                        ));
                        if CLI_ARGS.preserve_encodings {
                            unimplemented!("preserve_encodings is not implemented for float")
                        }
                    }
                    Primitive::F64 => {
                        deser_code.content.line(&final_result_expr_complete(
                            &mut deser_code.throws,
                            config.final_exprs,
                            "f64::deserialize(raw)",
                        ));
                        if CLI_ARGS.preserve_encodings {
                            unimplemented!("preserve_encodings is not implemented for float")
                        }
                    }
                };
            }
            SerializingRustType::Root(ConceptualRustType::Rust(ident)) => {
                if types.is_plain_group(ident) {
                    // This would mess up with length checks otherwise and is probably not a likely situation if this is even valid in CDDL.
                    // To have this work (if it's valid) you'd either need to generate 2 embedded deserialize methods or pass
                    // a parameter whether it was an optional field, and if so, read_len.read_elems(embedded mandatory fields)?;
                    // since otherwise it'd only length check the optional fields within the type.
                    assert!(!config.optional_field);
                    deser_code.read_len_used = true;
                    let final_expr_value = format!(
                        "{}::deserialize_as_embedded_group({}, {}, len)",
                        ident,
                        deserializer_name,
                        config.pass_read_len()
                    );

                    deser_code.content.line(&final_result_expr_complete(
                        &mut deser_code.throws,
                        config.final_exprs,
                        &final_expr_value,
                    ));
                } else {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                        deser_code.throws = true;
                    }
                    let final_expr_value = format!("{ident}::deserialize({deserializer_name})");
                    deser_code.content.line(&final_result_expr_complete(
                        &mut deser_code.throws,
                        config.final_exprs,
                        &final_expr_value,
                    ));
                }
            }
            SerializingRustType::Root(ConceptualRustType::Optional(ty)) => {
                let read_len_check =
                    config.optional_field || (ty.expanded_field_count(types) != Some(1));
                // codegen crate doesn't support if/else or appending a block after a block, only strings
                // so we need to create a local bool var and use a match instead
                let if_label = if ty.cbor_types().contains(&cbor_event::Type::Special) {
                    let is_some_check_var = format!("{}_is_some", config.var_name);
                    let mut is_some_check =
                        Block::new(format!("let {is_some_check_var} = match cbor_type()?"));
                    let mut special_block = Block::new("cbor_event::Type::Special =>");
                    special_block.line(&format!("let special = {deserializer_name}.special()?;"));
                    special_block.line(&format!(
                        "{deserializer_name}.as_mut_ref().seek(SeekFrom::Current(-1)).unwrap();"
                    ));
                    let mut special_match = Block::new("match special");
                    // TODO: we need to check that we don't have null / null somewhere
                    special_match.line("cbor_event::Special::Null => false,");
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
                    deser_code.content.push_block(is_some_check);
                    is_some_check_var
                } else {
                    String::from(&format!(
                        "{deserializer_name}.cbor_type()? != cbor_event::Type::Special"
                    ))
                };
                let mut deser_block = Block::new(format!(
                    "{}match {}",
                    before_after.before_str(false),
                    if_label
                ));
                let mut some_block = Block::new("true =>");
                if read_len_check {
                    let mandatory_fields = ty.expanded_mandatory_field_count(types);
                    if mandatory_fields != 0 {
                        some_block.line(format!("read_len.read_elems({mandatory_fields})?;"));
                        deser_code.read_len_used = true;
                    }
                }
                let ty_enc_fields = if CLI_ARGS.preserve_encodings {
                    encoding_fields(types, config.var_name, &ty.clone().resolve_aliases(), false)
                } else {
                    vec![]
                };
                if ty_enc_fields.is_empty() {
                    self.generate_deserialize(
                        types,
                        (&**ty).into(),
                        DeserializeBeforeAfter::new("Some(", ")", false),
                        config.optional_field(false),
                    )
                    .add_to(&mut some_block);
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
                        (
                            "Result::<_, DeserializeError>::Ok(",
                            format!(").map(|(x, {enc_vars_str})| (Some(x), {enc_vars_str}))?"),
                        )
                    };
                    self.generate_deserialize(
                        types,
                        (&**ty).into(),
                        DeserializeBeforeAfter::new(map_some_before, &map_some_after, false),
                        config.optional_field(false),
                    )
                    .add_to(&mut some_block);
                }
                some_block.after(",");
                deser_block.push_block(some_block);
                let mut none_block = Block::new("false =>");
                if read_len_check {
                    none_block.line("read_len.read_elems(1)?;");
                    deser_code.read_len_used = true;
                }
                // we don't use this to avoid the new (true) if CLI_ARGS.preserve_encodings is set
                //self.generate_deserialize(types, &ConceptualRustType::Fixed(FixedValue::Null), var_name, "", "", in_embedded, false, add_parens, &mut none_block);
                let mut check_null = Block::new(format!(
                    "if {deserializer_name}.special()? != cbor_event::Special::Null"
                ));
                check_null.line("return Err(DeserializeFailure::ExpectedNull.into());");
                none_block.push_block(check_null);
                if CLI_ARGS.preserve_encodings {
                    let mut none_elems = if ty.is_fixed_value() {
                        vec![]
                    } else {
                        vec!["None".to_owned()]
                    };
                    none_elems.extend(
                        ty_enc_fields
                            .iter()
                            .map(|enc_field| enc_field.default_expr.to_owned()),
                    );
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
                deser_block.after(&before_after.after_str(false));
                deser_block.push_block(none_block);
                deser_code.content.push_block(deser_block);
                deser_code.throws = true;
            }
            SerializingRustType::Root(ConceptualRustType::Array(ty)) => {
                //if !self.deserialize_generated_for_type(&element_type) {
                // TODO: check this elsehwere???
                //self.dont_generate_deserialize(&array_type_ident, format!("inner type {} doesn't support deserialize", element_type.for_rust_member()));
                //}
                if config.optional_field {
                    deser_code.content.line("read_len.read_elems(1)?;");
                    deser_code.read_len_used = true;
                }
                let arr_var_name = format!("{}_arr", config.var_name);
                deser_code
                    .content
                    .line(&format!("let mut {arr_var_name} = Vec::new();"));
                let elem_var_name = format!("{}_elem", config.var_name);
                let elem_encs = if CLI_ARGS.preserve_encodings {
                    encoding_fields(types, &elem_var_name, &ty.clone().resolve_aliases(), false)
                } else {
                    vec![]
                };
                if CLI_ARGS.preserve_encodings {
                    deser_code
                        .content
                        .line(&format!("let len = {deserializer_name}.array_sz()?;"))
                        .line(&format!("let {}_encoding = len.into();", config.var_name));
                    if !elem_encs.is_empty() {
                        deser_code.content.line(&format!(
                            "let mut {}_elem_encodings = Vec::new();",
                            config.var_name
                        ));
                    }
                } else {
                    deser_code
                        .content
                        .line(&format!("let len = {deserializer_name}.array()?;"));
                }
                let mut elem_config = DeserializeConfig::new(&elem_var_name);
                let (mut deser_loop, plain_len_check) = match &ty.conceptual_type {
                    ConceptualRustType::Rust(ty_ident) if types.is_plain_group(ty_ident) => {
                        // two things that must be done differently for embedded plain groups:
                        // 1) We can't directly read the CBOR len's number of items since it could be >1
                        // 2) We need a different cbor read len var to pass into embedded deserialize
                        let read_len_overload = format!("{}_read_len", config.var_name);
                        deser_code.content.line(&format!(
                            "let mut {read_len_overload} = CBORReadLen::new(len);"
                        ));
                        // inside of deserialize_as_embedded_group we only modify read_len for things we couldn't
                        // statically know beforehand. This was done for other areas that use plain groups in order
                        // to be able to do static length checks for statically sized groups that contain plain groups
                        // at the start of deserialization instead of many checks for every single field.
                        let plain_len_check =
                            match ty.conceptual_type.expanded_mandatory_field_count(types) {
                                0 => None,
                                n => Some(format!("{read_len_overload}.read_elems({n})?;")),
                            };
                        elem_config = elem_config.overload_read_len(read_len_overload);
                        let deser_loop =
                            make_deser_loop("len", &format!("{}_read_len.read()", config.var_name));
                        (deser_loop, plain_len_check)
                    }
                    _ => (
                        make_deser_loop("len", &format!("({arr_var_name}.len() as u64)")),
                        None,
                    ),
                };
                deser_loop.push_block(make_deser_loop_break_check());
                if let Some(plain_len_check) = plain_len_check {
                    deser_loop.line(plain_len_check);
                }
                elem_config.deserializer_name_overload = config.deserializer_name_overload;
                if !elem_encs.is_empty() {
                    let elem_var_names_str = encoding_var_names_str(types, &elem_var_name, ty);
                    self.generate_deserialize(
                        types,
                        (&**ty).into(),
                        DeserializeBeforeAfter::new(
                            &format!("let {elem_var_names_str} = "),
                            ";",
                            false,
                        ),
                        elem_config,
                    )
                    .add_to(&mut deser_loop);
                    deser_loop
                        .line(format!("{arr_var_name}.push({elem_var_name});"))
                        .line(format!(
                            "{}_elem_encodings.push({});",
                            config.var_name,
                            tuple_str(elem_encs.iter().map(|enc| enc.field_name.clone()).collect())
                        ));
                } else {
                    self.generate_deserialize(
                        types,
                        (&**ty).into(),
                        DeserializeBeforeAfter::new(&format!("{arr_var_name}.push("), ");", false),
                        elem_config,
                    )
                    .add_to(&mut deser_loop);
                }
                deser_code.content.push_block(deser_loop);
                if CLI_ARGS.preserve_encodings {
                    config
                        .final_exprs
                        .push(format!("{}_encoding", config.var_name));
                    if !elem_encs.is_empty() {
                        config
                            .final_exprs
                            .push(format!("{}_elem_encodings", config.var_name));
                    }
                    deser_code.content.line(&format!(
                        "{}{}{}",
                        before_after.before_str(false),
                        final_expr(config.final_exprs, Some(arr_var_name)),
                        before_after.after_str(false)
                    ));
                } else {
                    deser_code.content.line(&format!(
                        "{}{}{}",
                        before_after.before_str(false),
                        arr_var_name,
                        before_after.after_str(false)
                    ));
                }
                deser_code.throws = true;
            }
            SerializingRustType::Root(ConceptualRustType::Map(key_type, value_type)) => {
                if config.optional_field {
                    deser_code.content.line("read_len.read_elems(1)?;");
                    deser_code.read_len_used = true;
                }
                if !self.deserialize_generated_for_type(&key_type.conceptual_type) {
                    todo!();
                    // TODO: where is the best place to check for this? should we pass in a RustIdent to say where we're generating?!
                    //self.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_rust_member()));
                } else if !self.deserialize_generated_for_type(&value_type.conceptual_type) {
                    todo!();
                    //self.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_rust_member()));
                } else {
                    let table_var = format!("{}_table", config.var_name);
                    deser_code.content.line(&format!(
                        "let mut {} = {}::new();",
                        table_var,
                        table_type()
                    ));
                    let key_var_name = format!("{}_key", config.var_name);
                    let value_var_name = format!("{}_value", config.var_name);
                    let key_encs = if CLI_ARGS.preserve_encodings {
                        encoding_fields(
                            types,
                            &key_var_name,
                            &key_type.clone().resolve_aliases(),
                            false,
                        )
                    } else {
                        vec![]
                    };
                    let value_encs = if CLI_ARGS.preserve_encodings {
                        encoding_fields(
                            types,
                            &value_var_name,
                            &value_type.clone().resolve_aliases(),
                            false,
                        )
                    } else {
                        vec![]
                    };
                    let len_var = format!("{}_len", config.var_name);
                    if CLI_ARGS.preserve_encodings {
                        deser_code
                            .content
                            .line(&format!("let {len_var} = {deserializer_name}.map_sz()?;"))
                            .line(&format!(
                                "let {}_encoding = {}.into();",
                                config.var_name, len_var
                            ));
                        if !key_encs.is_empty() {
                            deser_code.content.line(&format!(
                                "let mut {}_key_encodings = BTreeMap::new();",
                                config.var_name
                            ));
                        }
                        if !value_encs.is_empty() {
                            deser_code.content.line(&format!(
                                "let mut {}_value_encodings = BTreeMap::new();",
                                config.var_name
                            ));
                        }
                    } else {
                        deser_code
                            .content
                            .line(&format!("let {len_var} = {deserializer_name}.map()?;"));
                    }
                    let mut deser_loop =
                        make_deser_loop(&len_var, &format!("({table_var}.len() as u64)"));
                    deser_loop.push_block(make_deser_loop_break_check());
                    let mut key_config = DeserializeConfig::new(&key_var_name);
                    key_config.deserializer_name_overload = config.deserializer_name_overload;
                    let mut value_config = DeserializeConfig::new(&value_var_name);
                    value_config.deserializer_name_overload = config.deserializer_name_overload;
                    let (key_var_names_str, value_var_names_str) = if CLI_ARGS.preserve_encodings {
                        (
                            encoding_var_names_str(types, &key_var_name, key_type),
                            encoding_var_names_str(types, &value_var_name, value_type),
                        )
                    } else {
                        (key_var_name.clone(), value_var_name.clone())
                    };
                    self.generate_deserialize(
                        types,
                        (&**key_type).into(),
                        DeserializeBeforeAfter::new(
                            &format!("let {key_var_names_str} = "),
                            ";",
                            false,
                        ),
                        key_config,
                    )
                    .add_to(&mut deser_loop);
                    self.generate_deserialize(
                        types,
                        (&**value_type).into(),
                        DeserializeBeforeAfter::new(
                            &format!("let {value_var_names_str} = "),
                            ";",
                            false,
                        ),
                        value_config,
                    )
                    .add_to(&mut deser_loop);
                    let mut dup_check = Block::new(format!(
                        "if {}.insert({}{}, {}).is_some()",
                        table_var,
                        key_var_name,
                        if key_type.is_copy() { "" } else { ".clone()" },
                        value_var_name
                    ));
                    let dup_key_error_key = match &key_type.conceptual_type {
                        ConceptualRustType::Primitive(Primitive::U8)
                        | ConceptualRustType::Primitive(Primitive::U16)
                        | ConceptualRustType::Primitive(Primitive::U32)
                        | ConceptualRustType::Primitive(Primitive::U64) => {
                            format!("Key::Uint({key_var_name}.into())")
                        }
                        ConceptualRustType::Primitive(Primitive::Str) => {
                            format!("Key::Str({key_var_name})")
                        }
                        // TODO: make a generic one then store serialized CBOR?
                        _ => "Key::Str(String::from(\"some complicated/unsupported type\"))"
                            .to_owned(),
                    };
                    dup_check.line(format!(
                        "return Err(DeserializeFailure::DuplicateKey({dup_key_error_key}).into());"
                    ));
                    deser_loop.push_block(dup_check);
                    if CLI_ARGS.preserve_encodings {
                        if !key_encs.is_empty() {
                            deser_loop.line(format!(
                                "{}_key_encodings.insert({}{}, {});",
                                config.var_name,
                                key_var_name,
                                if key_type.encoding_var_is_copy() {
                                    ""
                                } else {
                                    ".clone()"
                                },
                                tuple_str(
                                    key_encs.iter().map(|enc| enc.field_name.clone()).collect()
                                )
                            ));
                        }
                        if !value_encs.is_empty() {
                            deser_loop.line(format!(
                                "{}_value_encodings.insert({}{}, {});",
                                config.var_name,
                                key_var_name,
                                if key_type.encoding_var_is_copy() {
                                    ""
                                } else {
                                    ".clone()"
                                },
                                tuple_str(
                                    value_encs
                                        .iter()
                                        .map(|enc| enc.field_name.clone())
                                        .collect()
                                )
                            ));
                        }
                    }
                    deser_code.content.push_block(deser_loop);
                    if CLI_ARGS.preserve_encodings {
                        config
                            .final_exprs
                            .push(format!("{}_encoding", config.var_name));
                        if !key_encs.is_empty() {
                            config
                                .final_exprs
                                .push(format!("{}_key_encodings", config.var_name));
                        }
                        if !value_encs.is_empty() {
                            config
                                .final_exprs
                                .push(format!("{}_value_encodings", config.var_name));
                        }
                        deser_code.content.line(&format!(
                            "{}{}{}",
                            before_after.before_str(false),
                            final_expr(config.final_exprs, Some(table_var)),
                            before_after.after_str(false)
                        ));
                    } else {
                        deser_code.content.line(&format!(
                            "{}{}{}",
                            before_after.before_str(false),
                            table_var,
                            before_after.after_str(false)
                        ));
                    }
                }
                deser_code.throws = true;
            }
            SerializingRustType::Root(ConceptualRustType::Alias(_ident, ty)) => {
                self.generate_deserialize(types, (&**ty).into(), before_after, config)
                    .add_to_code(&mut deser_code);
            }
            SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
                if CLI_ARGS.preserve_encodings {
                    config.final_exprs.push(format!(
                        "StringEncoding::from({}_bytes_encoding)",
                        config.var_name
                    ));
                    deser_code.content.line(&format!(
                        "let ({}_bytes, {}_bytes_encoding) = raw.bytes_sz()?;",
                        config.var_name, config.var_name
                    ));
                } else {
                    deser_code
                        .content
                        .line(&format!("let {}_bytes = raw.bytes()?;", config.var_name));
                };
                let name_overload = "inner_de";
                deser_code.content.line(&format!(
                    "let {} = &mut Deserializer::from(std::io::Cursor::new({}_bytes));",
                    name_overload, config.var_name
                ));
                self.generate_deserialize(
                    types,
                    *child,
                    before_after,
                    config.overload_deserializer(name_overload),
                )
                .add_to_code(&mut deser_code);
                deser_code.throws = true;
            }
            SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
                if config.optional_field {
                    deser_code.content.line("read_len.read_elems(1)?;");
                    deser_code.read_len_used = true;
                }
                let mut tag_check = if CLI_ARGS.preserve_encodings {
                    let mut tag_check = Block::new(format!(
                        "{}match {}.tag_sz()?",
                        before_after.before, deserializer_name
                    ));
                    config.final_exprs.push("Some(tag_enc)".to_owned());
                    let some_deser_code = self
                        .generate_deserialize(
                            types,
                            *child,
                            DeserializeBeforeAfter::new("", "", before_after.expects_result),
                            config.optional_field(false),
                        )
                        .mark_and_extract_content(&mut deser_code);
                    if let Some(single_line) = some_deser_code.as_single_line() {
                        tag_check.line(format!("({tag}, tag_enc) => {single_line},"));
                    } else {
                        let mut deser_block = Block::new(format!("({tag}, tag_enc) =>"));
                        deser_block.push_all(some_deser_code);
                        deser_block.after(",");
                        tag_check.push_block(deser_block);
                    }
                    tag_check
                } else {
                    let mut tag_check = Block::new(format!(
                        "{}match {}.tag()?",
                        before_after.before, deserializer_name
                    ));

                    let some_deser_code = self
                        .generate_deserialize(
                            types,
                            *child,
                            DeserializeBeforeAfter::new("", "", before_after.expects_result),
                            config.optional_field(false),
                        )
                        .mark_and_extract_content(&mut deser_code);
                    if let Some(single_line) = some_deser_code.as_single_line() {
                        tag_check.line(format!("{tag} => {single_line},"));
                    } else {
                        let mut deser_block = Block::new(format!("{tag} =>"));
                        deser_block.push_all(some_deser_code);
                        deser_block.after(",");
                        tag_check.push_block(deser_block);
                    }
                    tag_check
                };
                tag_check.line(&format!(
                    "{} => {}Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),",
                    if CLI_ARGS.preserve_encodings { "(tag, _enc)" } else { "tag" },
                    if before_after.expects_result { "" } else { "return " },
                    tag));
                tag_check.after(before_after.after);
                deser_code.content.push_block(tag_check);
                deser_code.throws = true;
            }
        }
        deser_code
    }

    fn deserialize_generated(&self, name: &RustIdent) -> bool {
        !self.no_deser_reasons.contains_key(name)
    }

    fn deserialize_generated_for_type(&self, field_type: &ConceptualRustType) -> bool {
        match field_type {
            ConceptualRustType::Fixed(_) => true,
            ConceptualRustType::Primitive(_) => true,
            ConceptualRustType::Rust(ident) => self.deserialize_generated(ident),
            ConceptualRustType::Array(ty) => {
                self.deserialize_generated_for_type(&ty.conceptual_type)
            }
            ConceptualRustType::Map(k, v) => {
                self.deserialize_generated_for_type(&k.conceptual_type)
                    && self.deserialize_generated_for_type(&v.conceptual_type)
            }
            ConceptualRustType::Optional(ty) => {
                self.deserialize_generated_for_type(&ty.conceptual_type)
            }
            ConceptualRustType::Alias(_ident, ty) => self.deserialize_generated_for_type(ty),
        }
    }

    fn dont_generate_deserialize(&mut self, name: &RustIdent, reason: String) {
        self.no_deser_reasons
            .entry(name.clone())
            .or_default()
            .push(reason);
    }

    pub fn print_structs_without_deserialize(&self) {
        for (name, reasons) in &self.no_deser_reasons {
            eprintln!("Not generating {name}::deserialize() - reasons:");
            for reason in reasons {
                println!("\t{reason}");
            }
        }
    }

    // TODO: repurpose this for type choices (not group choices)
    // TODO: make this its own function - there's no reason for this to be a method
    fn generate_type_choices_from_variants(
        &mut self,
        types: &IntermediateTypes,
        name: &RustIdent,
        variants: &[EnumVariant],
        tag: Option<usize>,
    ) {
        // Rust only
        generate_enum(self, types, name, variants, None, true, tag);
        if CLI_ARGS.wasm {
            // Generate a wrapper object that we will expose to wasm around this
            let mut wrapper = create_base_wasm_wrapper(self, types, name, true);
            // new
            for variant in variants.iter() {
                let variant_arg = variant.name_as_var();
                let mut new_func = codegen::Function::new(&format!("new_{variant_arg}"));
                new_func.vis("pub");
                let can_fail = match &variant.name {
                    VariantIdent::Custom(_) => false,
                    VariantIdent::RustStruct(rust_ident) => types.can_new_fail(rust_ident),
                };
                if can_fail {
                    new_func.ret(format!("Result<{name}, JsValue>"));
                } else {
                    new_func.ret("Self");
                }
                if !variant.rust_type.is_fixed_value() {
                    new_func.arg(&variant_arg, &variant.rust_type.for_wasm_param());
                }
                if variant.rust_type.is_fixed_value() {
                    new_func.line(format!(
                        "Self({}::new_{}())",
                        rust_crate_struct_from_wasm(types, name),
                        variant.name_as_var()
                    ));
                } else {
                    let from_wasm_expr = variant
                        .rust_type
                        .from_wasm_boundary_clone(&variant_arg, can_fail);
                    new_func.line(format!(
                        "Self({}::new_{}({}))",
                        rust_crate_struct_from_wasm(types, name),
                        variant.name_as_var(),
                        ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
                    ));
                }
                wrapper.s_impl.push_fn(new_func);
            }
            add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, None);
            wrapper.push(self, types);
        }
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        array_type_ident: &RustIdent,
    ) {
        if self.already_generated.insert(array_type_ident.clone()) {
            let inner_type = element_type.name_as_rust_array(types, true);
            let mut wrapper = create_base_wasm_struct(self, array_type_ident, false);
            wrapper.s.tuple_field(None, &inner_type);
            // other functions
            let mut new_func = codegen::Function::new("new");
            new_func.vis("pub").ret("Self");
            new_func.line("Self(Vec::new())");
            wrapper.s_impl.push_fn(new_func);
            wrapper
                .s_impl
                .new_fn("len")
                .vis("pub")
                .ret("usize")
                .arg_ref_self()
                .line("self.0.len()");
            wrapper
                .s_impl
                .new_fn("get")
                .vis("pub")
                .ret(&element_type.for_wasm_return())
                .arg_ref_self()
                .arg("index", "usize")
                .line(element_type.to_wasm_boundary("self.0[index]", false));
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            wrapper
                .s_impl
                .new_fn("add")
                .vis("pub")
                .arg_mut_self()
                .arg("elem", element_type.for_wasm_param())
                .line(format!(
                    "self.0.push({});",
                    ToWasmBoundaryOperations::format(
                        element_type
                            .from_wasm_boundary_clone("elem", false)
                            .into_iter()
                    )
                ));
            wrapper.add_conversion_methods(&inner_type);
            wrapper.push(self, types);
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
                .map(|derive| format!("{derive}=\"ignore\""))
                .collect::<Vec<String>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    if CLI_ARGS.json_serde_derives {
        ret.push_str("#[serde(skip)]\n");
    }
    ret
}

fn start_len(
    body: &mut dyn CodeBlock,
    rep: Representation,
    serializer_use: &str,
    encoding_var: &str,
    len_expr: &str,
) {
    let rep_str = match rep {
        Representation::Array => "array",
        Representation::Map => "map",
    };
    if CLI_ARGS.preserve_encodings {
        body.line(&format!(
            "{}.write_{}_sz({}.to_len_sz({}{}))?;",
            serializer_use,
            rep_str,
            encoding_var,
            len_expr,
            canonical_param()
        ));
    } else {
        body.line(&format!(
            "{serializer_use}.write_{rep_str}(cbor_event::Len::Len({len_expr}))?;"
        ));
    }
}

fn end_len(body: &mut dyn CodeBlock, serializer_use: &str, encoding_var: &str, is_end: bool) {
    if CLI_ARGS.preserve_encodings {
        body.line(&format!(
            "{}.end({}{}){}",
            encoding_var,
            serializer_use,
            canonical_param(),
            if is_end { "" } else { "?;" }
        ));
    } else if is_end {
        body.line("Ok(serializer)");
    }
}

fn write_using_sz(
    body: &mut dyn CodeBlock,
    func: &str,
    serializer_use: &str,
    expr: &str,
    fit_sz_expr: &str,
    line_ender: &str,
    encoding_var: &str,
) {
    if CLI_ARGS.preserve_encodings {
        body.line(&format!(
            "{}.{}_sz({}, fit_sz({}, {}{})){}",
            serializer_use,
            func,
            expr,
            fit_sz_expr,
            encoding_var,
            canonical_param(),
            line_ender
        ));
    } else {
        body.line(&format!("{serializer_use}.{func}({expr}){line_ender}"));
    }
}

fn write_string_sz(
    body: &mut dyn CodeBlock,
    func: &str,
    serializer_use: &str,
    expr: &str,
    line_ender: &str,
    encoding_var: &str,
) {
    if CLI_ARGS.preserve_encodings {
        body.line(&format!(
            "{}.{}_sz(&{}, {}.to_str_len_sz({}.len() as u64{})){}",
            serializer_use,
            func,
            expr,
            encoding_var,
            expr,
            canonical_param(),
            line_ender
        ));
    } else {
        body.line(&format!("{serializer_use}.{func}(&{expr}){line_ender}"));
    }
}

enum BlockOrLine {
    Line(String),
    Block(codegen::Block),
}

#[derive(Default)]
struct BlocksOrLines(Vec<BlockOrLine>);

impl BlocksOrLines {
    fn as_single_line(&self) -> Option<&str> {
        match self.0.len() {
            1 => match &self.0[0] {
                BlockOrLine::Line(line) => Some(line),
                BlockOrLine::Block(_) => None,
            },
            _ => None,
        }
    }
}

impl From<codegen::Block> for BlocksOrLines {
    fn from(block: codegen::Block) -> Self {
        Self(vec![BlockOrLine::Block(block)])
    }
}

trait CodeBlock {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock;

    fn push_block(&mut self, block: codegen::Block) -> &mut dyn CodeBlock;

    fn push_all(&mut self, contents: BlocksOrLines) -> &mut dyn CodeBlock
    where
        Self: Sized,
    {
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

fn create_base_rust_struct(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(&mut s, types.used_as_key(ident), false);
    let group_impl = codegen::Impl::new(name);
    // TODO: anything here?
    (s, group_impl)
}

/// Formatted string for fully scoped rust crate struct for use from wasm crate
pub fn rust_crate_struct_from_wasm(types: &IntermediateTypes<'_>, ident: &RustIdent) -> String {
    println!("scope for [{}]: {}", ident.as_ref(), types.scope(ident));
    match types.scope(ident) {
        "lib" => format!("{}::{}", CLI_ARGS.lib_name_code(), ident),
        other_scope => format!("{}::{}::{}", CLI_ARGS.lib_name_code(), other_scope, ident),
    }
}

#[derive(Debug)]
struct WasmWrapper<'a> {
    ident: &'a RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    // rust -> wasm
    from_wasm: Option<codegen::Impl>,
    // wasm -> rust
    from_native: Option<codegen::Impl>,
    // AsRef
    as_ref: Option<codegen::Impl>,
}

impl<'a> WasmWrapper<'a> {
    fn push(self, gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
        gen_scope
            .wasm(types, self.ident)
            .push_struct(self.s)
            .push_impl(self.s_impl);
        if let Some(from_wasm) = self.from_wasm {
            gen_scope.wasm(types, self.ident).push_impl(from_wasm);
        }
        if let Some(from_native) = self.from_native {
            gen_scope.wasm(types, self.ident).push_impl(from_native);
        }
        if let Some(as_ref) = self.as_ref {
            gen_scope.wasm(types, self.ident).push_impl(as_ref);
        }
    }

    /// native_name is &str since we need to possibly prepend namespacing
    /// and where we're calling it we'd have to construct a RustType where we
    /// didn't have to before, but we already had the string.
    fn add_conversion_methods(&mut self, native_name: &str) {
        let mut from_wasm = codegen::Impl::new(self.ident.to_string());
        from_wasm
            .impl_trait(format!("From<{native_name}>"))
            .new_fn("from")
            .arg("native", native_name)
            .ret("Self")
            .line("Self(native)");
        self.from_wasm = Some(from_wasm);
        let mut from_native = codegen::Impl::new(native_name);
        from_native
            .impl_trait(format!("From<{}>", self.ident))
            .new_fn("from")
            .arg("wasm", self.ident.to_string())
            .ret("Self")
            .line("wasm.0");
        self.from_native = Some(from_native);
        let mut as_ref = codegen::Impl::new(self.ident.to_string());
        as_ref
            .impl_trait(format!("AsRef<{native_name}>"))
            .new_fn("as_ref")
            .arg_ref_self()
            .ret(&format!("&{native_name}"))
            .line("&self.0");
        self.as_ref = Some(as_ref);
    }
}

fn create_base_wasm_struct<'a>(
    gen_scope: &GenerationScope,
    ident: &'a RustIdent,
    exists_in_rust: bool,
) -> WasmWrapper<'a> {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    s.vis("pub")
        .derive("Clone")
        .derive("Debug")
        .attr("wasm_bindgen");
    let mut s_impl = codegen::Impl::new(name);
    s_impl.r#macro("#[wasm_bindgen]");
    // There are auto-implementing ToCBORBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if exists_in_rust {
        if CLI_ARGS.to_from_bytes_methods {
            let mut to_bytes = codegen::Function::new("to_cbor_bytes");
            to_bytes.ret("Vec<u8>").arg_ref_self().vis("pub");
            if CLI_ARGS.preserve_encodings && CLI_ARGS.canonical_form {
                to_bytes.line(format!(
                    "{}::serialization::Serialize::to_cbor_bytes(&self.0)",
                    CLI_ARGS.lib_name_code()
                ));
                let mut to_canonical_bytes = codegen::Function::new("to_canonical_cbor_bytes");
                to_canonical_bytes
                    .ret("Vec<u8>")
                    .arg_ref_self()
                    .vis("pub")
                    .line("Serialize::to_canonical_cbor_bytes(&self.0)");
            } else {
                to_bytes.line(format!(
                    "{}::serialization::ToCBORBytes::to_cbor_bytes(&self.0)",
                    CLI_ARGS.lib_name_code()
                ));
            }
            s_impl.push_fn(to_bytes);
            if gen_scope.deserialize_generated(ident) {
                s_impl
                    .new_fn("from_cbor_bytes")
                    .ret(format!("Result<{name}, JsValue>"))
                    .arg("cbor_bytes", "&[u8]")
                    .vis("pub")
                    .line(format!(
                        "{}::serialization::Deserialize::from_cbor_bytes(cbor_bytes).map(Self).map_err(|e| JsValue::from_str(&format!(\"from_bytes: {{}}\", e)))",
                        CLI_ARGS.lib_name_code()));
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
                .line("serde_wasm_bindgen::to_value(&self.0).map_err(|e| JsValue::from_str(&format!(\"to_js_value: {}\", e)))");
            s_impl.push_fn(to_json_value);
            s_impl
                .new_fn("from_json")
                .ret(format!("Result<{name}, JsValue>"))
                .arg("json", "&str")
                .vis("pub")
                .line("serde_json::from_str(json).map(Self).map_err(|e| JsValue::from_str(&format!(\"from_json: {}\", e)))");
        }
    }
    WasmWrapper {
        ident,
        s,
        s_impl,
        from_wasm: None,
        from_native: None,
        as_ref: None,
    }
}

/// default_structure will have it be a DIRECT wrapper with a tuple field of rust_lib::{ident}
/// this will include generating to/from traits automatically
fn create_base_wasm_wrapper<'a>(
    gen_scope: &GenerationScope,
    types: &IntermediateTypes<'_>,
    ident: &'a RustIdent,
    default_structure: bool,
) -> WasmWrapper<'a> {
    assert!(CLI_ARGS.wasm);
    let mut base = create_base_wasm_struct(gen_scope, ident, true);
    if default_structure {
        let native_name = rust_crate_struct_from_wasm(types, ident);
        base.s.tuple_field(None, &native_name);
        base.add_conversion_methods(&native_name);
    }
    base
}

// Always creates directly just Serialize impl. Shortcut for create_serialize_impls when
// we know we won't need the SerializeEmbeddedGroup impl.
// See comments for create_serialize_impls for usage.
#[allow(unused)]
fn create_serialize_impl(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    definite_len: &str,
    use_this_encoding: Option<&str>,
) -> (codegen::Function, codegen::Impl) {
    match create_serialize_impls(ident, rep, tag, definite_len, use_this_encoding, false) {
        (ser_func, ser_impl, None) => (ser_func, ser_impl),
        (_ser_func, _ser_impl, Some(_embedded_impl)) => unreachable!(),
    }
}

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
fn create_serialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    definite_len: &str,
    use_this_encoding: Option<&str>,
    generate_serialize_embedded: bool,
) -> (codegen::Function, codegen::Impl, Option<codegen::Impl>) {
    if generate_serialize_embedded {
        // This is not necessarily a problem but we should investigate this case to ensure we're not calling
        // (de)serialize_as_embedded without (de)serializing the tag
        assert_eq!(tag, None);
    }
    let name = &ident.to_string();
    let ser_impl = make_serialization_impl(name);
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        let expr = format!("{tag}u64");
        write_using_sz(
            &mut ser_func,
            "write_tag",
            "serializer",
            &expr,
            &expr,
            "?;",
            "self.encodings.as_ref().map(|encs| encs.tag_encoding).unwrap_or_default()",
        );
    }
    // TODO: do definite length encoding for optional fields too
    if let Some(rep) = rep {
        if let Some(definite) = use_this_encoding {
            start_len(&mut ser_func, rep, "serializer", definite, definite_len);
        } else {
            let len = cbor_event_len_n(definite_len);
            match rep {
                Representation::Array => ser_func.line(format!("serializer.write_array({len})?;")),
                Representation::Map => ser_func.line(format!("serializer.write_map({len})?;")),
            };
        }
        if generate_serialize_embedded {
            ser_func.line(format!(
                "self.serialize_as_embedded_group(serializer{})",
                canonical_param()
            ));
        }
    } else {
        // not array or map, generate serialize directly
        if generate_serialize_embedded {
            ser_func.line(format!(
                "self.serialize_as_embedded_group(serializer{})",
                canonical_param()
            ));
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
        RustStructCBORLen::Dynamic =>
            /*nothing*/
            {}
        // TODO: direct check here instead of involving read_len
        RustStructCBORLen::OptionalFields(mandatory) => {
            if mandatory != 0 {
                deser_body.line(&format!("read_len.read_elems({mandatory})?;"));
            }
        }
        RustStructCBORLen::Fixed(fixed) => {
            if fixed != 0 {
                deser_body.line(&format!("read_len.read_elems({fixed})?;"));
            }
        }
    }
}

// Adds final Len check if not fixed + reads for the ending Special::Break for Indefinite arrays
fn add_deserialize_final_len_check(
    deser_body: &mut dyn CodeBlock,
    rep: Option<Representation>,
    len_info: RustStructCBORLen,
) {
    // We only check for Break for arrays since the implementation for maps uses len to decide
    // when to stop reading values, since otherwise with optional parameters it doesn't know.
    // We also can't do it from within deserialize_as_embedded_group() as that interferes with
    // plain groups nested inside other array groups
    let ending_check = match len_info {
        RustStructCBORLen::Fixed(_) => "()", // no need to check - checked at the start
        RustStructCBORLen::OptionalFields(_) | RustStructCBORLen::Dynamic => "read_len.finish()?",
    };
    match rep {
        Some(Representation::Array) => {
            let mut end_len_check = Block::new("match len");
            end_len_check.line(format!("{} => {},", cbor_event_len_n("_"), ending_check));
            let mut indefinite_check = Block::new(format!(
                "{} => match raw.special()?",
                cbor_event_len_indef()
            ));
            indefinite_check.line(format!("cbor_event::Special::Break => {ending_check},"));
            indefinite_check
                .line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
            indefinite_check.after(",");
            end_len_check.push_block(indefinite_check);
            deser_body.push_block(end_len_check);
        }
        Some(Representation::Map) => {
            deser_body.line(&format!("{ending_check};"));
        }
        None =>
            /* this should just be for type choices */
            {}
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
fn create_deserialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    len_info: RustStructCBORLen,
    generate_deserialize_embedded: bool,
    store_encoding: Option<&str>,
    deser_body: &mut dyn CodeBlock,
) -> (codegen::Impl, Option<codegen::Impl>) {
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
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{name}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        deser_body.push_block(tag_check);
    }
    if let Some(rep) = rep {
        match rep {
            Representation::Array => {
                if CLI_ARGS.preserve_encodings {
                    deser_body.line("let len = raw.array_sz()?;");
                } else {
                    deser_body.line("let len = raw.array()?;");
                }
                if !generate_deserialize_embedded {
                    if let Some(encoding_var_name) = store_encoding {
                        deser_body.line(&format!(
                            "let {encoding_var_name}: LenEncoding = len.into();"
                        ));
                    }
                }
                add_deserialize_initial_len_check(deser_body, len_info);
                if generate_deserialize_embedded {
                    deser_body.line(
                        "let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);",
                    );
                }
            }
            Representation::Map => {
                if CLI_ARGS.preserve_encodings {
                    deser_body.line("let len = raw.map_sz()?;");
                } else {
                    deser_body.line("let len = raw.map()?;");
                }
                if !generate_deserialize_embedded {
                    if let Some(encoding_var_name) = store_encoding {
                        deser_body.line(&format!(
                            "let {encoding_var_name}: LenEncoding = len.into();"
                        ));
                    }
                }
                add_deserialize_initial_len_check(deser_body, len_info);
                if generate_deserialize_embedded {
                    deser_body.line(
                        "let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);",
                    );
                }
            }
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
    types: &IntermediateTypes,
    name: &RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    ser_impl: codegen::Impl,
    ser_embedded_impl: Option<codegen::Impl>,
) {
    gen_scope.rust(types, name).push_struct(s).push_impl(s_impl);
    gen_scope.rust_serialize().push_impl(ser_impl);
    if let Some(s) = ser_embedded_impl {
        gen_scope.rust_serialize().push_impl(s);
    }
}

// We need to execute field deserialization inside a closure in order to capture and annotate with the field name
// without having to put error annotation inside of every single cbor_event call.
fn make_err_annotate_block(annotation: &str, before: &str, after: &str) -> Block {
    let mut if_block = Block::new(format!("{before}(|| -> Result<_, DeserializeError>"));
    if_block.after(&format!(
        ")().map_err(|e| e.annotate(\"{annotation}\")){after}"
    ));
    if_block
}

fn make_deser_loop(len_var: &str, len_expr: &str) -> Block {
    Block::new(format!(
        "while match {} {{ {} => {} < n, {} => true, }}",
        len_var,
        cbor_event_len_n("n"),
        len_expr,
        cbor_event_len_indef()
    ))
}

fn make_deser_loop_break_check() -> Block {
    let mut break_check = Block::new("if raw.cbor_type()? == cbor_event::Type::Special");
    // TODO: read special and go back 1 character
    break_check.line("assert_eq!(raw.special()?, cbor_event::Special::Break);");
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

fn codegen_table_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    key_type: RustType,
    value_type: RustType,
    tag: Option<usize>,
    exists_in_rust: bool,
) {
    assert!(CLI_ARGS.wasm);
    assert!(tag.is_none(), "TODO: why is this not used anymore? is it since it's only on the wasm side now so it shouldn't happen now?");
    // this would interfere with loop code generation unless we
    // specially handle this case since you wouldn't know whether you hit a break
    // or are reading a key here, unless we check, but then you'd need to store the
    // non-break special value once read
    assert!(!key_type.cbor_types().contains(&cbor_event::Type::Special));
    let mut wrapper = create_base_wasm_struct(gen_scope, name, false);

    let inner_type = if exists_in_rust {
        rust_crate_struct_from_wasm(types, name)
    } else {
        ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true)
    };
    wrapper.s.tuple_field(None, &inner_type);
    // new
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self")
        .line(format!("Self({}::new())", table_type()));
    wrapper.s_impl.push_fn(new_func);
    // len
    wrapper
        .s_impl
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
        .arg("key", key_type.for_wasm_param())
        .arg("value", value_type.for_wasm_param())
        .ret(format!("Option<{}>", value_type.for_wasm_return()))
        .line(format!(
            "self.0.insert({}, {}){}",
            ToWasmBoundaryOperations::format(
                key_type.from_wasm_boundary_clone("key", false).into_iter()
            ),
            ToWasmBoundaryOperations::format(
                value_type
                    .from_wasm_boundary_clone("value", false)
                    .into_iter()
            ),
            if value_type.directly_wasm_exposable() {
                ""
            } else {
                ".map(Into::into)"
            }
        ));
    // ^ TODO: support failable types everywhere or just force it to be only a detail in the wrapper?
    wrapper.s_impl.push_fn(insert_func);
    // get
    let get_ret_modifier = if value_type.is_copy() {
        ""
    } else if value_type.directly_wasm_exposable() {
        ".map(|v| v.clone())"
    } else {
        ".map(|v| v.clone().into())"
    };
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
            if value_type.is_copy() {
                ".copied()"
            } else {
                get_ret_modifier
            }
        ));
    } else {
        getter.line(format!(
            "self.0.get({}.as_ref()){}",
            key_type.from_wasm_boundary_ref("key"),
            if value_type.is_copy() {
                ".copied()"
            } else {
                get_ret_modifier
            }
        ));
    }
    wrapper.s_impl.push_fn(getter);
    // keys
    let keys_type = ConceptualRustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys.arg_ref_self()
        .ret(keys_type.for_wasm_return())
        .vis("pub");
    if keys_type.directly_wasm_exposable() {
        let key_clone = if key_type.is_copy() {
            ".keys().copied()"
        } else {
            ".keys().cloned()"
        };
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>()"));
    } else {
        keys.line(format!(
            "{}(self.0.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>())",
            keys_type.for_wasm_return()
        ));
    }
    wrapper.s_impl.push_fn(keys);
    wrapper.add_conversion_methods(&inner_type);
    wrapper.push(gen_scope, types);
}

#[derive(Debug)]
struct EncodingField {
    field_name: String,
    type_name: String,
    /// this MUST be equivalent to the Default trait of the encoding field.
    /// This can be more concise though e.g. None for Option<T>::default()
    default_expr: &'static str,
    /// inner encodings - used for map/vec types
    #[allow(unused)]
    inner: Vec<EncodingField>,
}

fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    match key {
        FixedValue::Text(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            inner: Vec::new(),
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            inner: Vec::new(),
        },
        _ => unimplemented!(),
    }
}

fn encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
) -> Vec<EncodingField> {
    assert!(CLI_ARGS.preserve_encodings);
    // TODO: how do we handle defaults for nested things? e.g. inside of a ConceptualRustType::Map
    let mut encs = encoding_fields_impl(types, name, ty.into());
    if include_default && ty.default.is_some() {
        encs.push(EncodingField {
            field_name: format!("{name}_default_present"),
            type_name: "bool".to_owned(),
            default_expr: "false",
            inner: Vec::new(),
        });
    }
    encs
}

fn encoding_fields_impl(
    types: &IntermediateTypes,
    name: &str,
    ty: SerializingRustType,
) -> Vec<EncodingField> {
    assert!(CLI_ARGS.preserve_encodings);
    match ty {
        SerializingRustType::Root(ConceptualRustType::Array(elem_ty)) => {
            let base = EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                inner: Vec::new(),
            };
            let inner_encs =
                encoding_fields_impl(types, &format!("{name}_elem"), (&**elem_ty).into());
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = if inner_encs.len() == 1 {
                    inner_encs.first().unwrap().type_name.clone()
                } else {
                    format!(
                        "({})",
                        inner_encs
                            .iter()
                            .map(|key_enc| key_enc.type_name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };
                vec![
                    base,
                    EncodingField {
                        field_name: format!("{name}_elem_encodings"),
                        type_name: format!("Vec<{type_name_elem}>"),
                        default_expr: "Vec::new()",
                        inner: inner_encs,
                    },
                ]
            }
        }
        SerializingRustType::Root(ConceptualRustType::Map(k, v)) => {
            let mut encs = vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                inner: Vec::new(),
            }];
            let key_encs = encoding_fields_impl(types, &format!("{name}_key"), (&**k).into());
            let val_encs = encoding_fields_impl(types, &format!("{name}_value"), (&**v).into());

            if !key_encs.is_empty() {
                let type_name_value = if key_encs.len() == 1 {
                    key_encs.first().unwrap().type_name.clone()
                } else {
                    format!(
                        "({})",
                        key_encs
                            .iter()
                            .map(|key_enc| key_enc.type_name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };
                encs.push(EncodingField {
                    field_name: format!("{name}_key_encodings"),
                    type_name: format!(
                        "BTreeMap<{}, {}>",
                        k.for_rust_member(types, false),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    inner: key_encs,
                });
            }

            if !val_encs.is_empty() {
                let type_name_value = if val_encs.len() == 1 {
                    val_encs.first().unwrap().type_name.clone()
                } else {
                    format!(
                        "({})",
                        val_encs
                            .iter()
                            .map(|val_enc| val_enc.type_name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };
                encs.push(EncodingField {
                    field_name: format!("{name}_value_encodings"),
                    type_name: format!(
                        "BTreeMap<{}, {}>",
                        k.for_rust_member(types, false),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    inner: val_encs,
                });
            }
            encs
        }
        SerializingRustType::Root(ConceptualRustType::Primitive(p)) => match p {
            Primitive::Bytes | Primitive::Str => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "StringEncoding".to_owned(),
                default_expr: "StringEncoding::default()",
                inner: Vec::new(),
            }],
            Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64
            | Primitive::N64
            | Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::F32
            | Primitive::F64 => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "Option<cbor_event::Sz>".to_owned(),
                default_expr: "None",
                inner: Vec::new(),
            }],
            Primitive::Bool =>
            /* bool only has 1 encoding */
            {
                vec![]
            }
        },
        SerializingRustType::Root(ConceptualRustType::Fixed(f)) => match f {
            FixedValue::Bool(_) | FixedValue::Null => vec![],
            FixedValue::Nint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::I64)).into(),
            ),
            FixedValue::Uint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::U64)).into(),
            ),
            FixedValue::Float(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::F64)).into(),
            ),
            FixedValue::Text(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Str)).into(),
            ),
        },
        SerializingRustType::Root(ConceptualRustType::Alias(_, _)) => {
            panic!("resolve types before calling this")
        }
        SerializingRustType::Root(ConceptualRustType::Optional(ty)) => {
            encoding_fields(types, name, ty, false)
        }
        SerializingRustType::Root(ConceptualRustType::Rust(_)) => vec![],
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_tag"),
                (&ConceptualRustType::Fixed(FixedValue::Uint(*tag))).into(),
            );
            encs.append(&mut encoding_fields_impl(types, name, *child));
            encs
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_bytes"),
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
            );
            encs.append(&mut encoding_fields_impl(types, name, *child));
            encs
        }
    }
}

fn encoding_var_names_str(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
) -> String {
    assert!(CLI_ARGS.preserve_encodings);
    let resolved_rust_type = rust_type.clone().resolve_aliases();
    let mut var_names = if resolved_rust_type.is_fixed_value() {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in encoding_fields(types, field_name, &resolved_rust_type, false).into_iter() {
        var_names.push(enc.field_name);
    }

    if var_names.len() > 1 {
        format!("({})", var_names.join(", "))
    } else {
        var_names.join(", ")
    }
}

fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

fn codegen_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    tag: Option<usize>,
    record: &RustRecord,
) {
    // wasm wrapper
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new.ret("Self").vis("pub");
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
                        .vis("pub");
                    if field.rust_type.default.is_some() {
                        setter.line(format!(
                            "self.0.{} = {}",
                            field.name,
                            ToWasmBoundaryOperations::format(
                                field
                                    .rust_type
                                    .from_wasm_boundary_clone(&field.name, false)
                                    .into_iter()
                            )
                        ));
                    } else {
                        setter.line(format!(
                            "self.0.{} = Some({})",
                            field.name,
                            ToWasmBoundaryOperations::format(
                                field
                                    .rust_type
                                    .from_wasm_boundary_clone(&field.name, false)
                                    .into_iter()
                            )
                        ));
                    }

                    // ^ TODO: check types.can_new_fail(&field.name)
                    wrapper.s_impl.push_fn(setter);
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter.arg_ref_self().vis("pub");
                    if field.rust_type.default.is_some() {
                        getter.ret(field.rust_type.for_wasm_return()).line(
                            field
                                .rust_type
                                .to_wasm_boundary(&format!("self.0.{}", field.name), false),
                        );
                    } else {
                        getter
                            .ret(format!("Option<{}>", field.rust_type.for_wasm_return()))
                            .line(field.rust_type.to_wasm_boundary_optional(
                                &format!("self.0.{}", field.name),
                                false,
                            ));
                    }
                    wrapper.s_impl.push_fn(getter);
                } else {
                    // new
                    wasm_new.arg(&field.name, field.rust_type.for_wasm_param());
                    wasm_new_args.push(ToWasmBoundaryOperations::format(
                        field
                            .rust_type
                            .from_wasm_boundary_clone(&field.name, false)
                            .into_iter(),
                    ));
                    // ^ TODO: check types.can_new_fail(&field.name)
                    // do we want setters here later for mandatory types covered by new?
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter
                        .arg_ref_self()
                        .ret(field.rust_type.for_wasm_return())
                        .vis("pub")
                        .line(
                            field
                                .rust_type
                                .to_wasm_boundary(&format!("self.0.{}", field.name), false),
                        );
                    wrapper.s_impl.push_fn(getter);
                }
            }
        }
        wasm_new.line(format!(
            "Self({}::new({}))",
            rust_crate_struct_from_wasm(types, name),
            wasm_new_args.join(", ")
        ));
        wrapper.s_impl.push_fn(wasm_new);
        wrapper.push(gen_scope, types);
    }

    // Rust-only for the rest of this function

    // Struct (fields) + constructor
    let (mut native_struct, mut native_impl) = create_base_rust_struct(types, name);
    native_struct.vis("pub");
    let mut native_new = codegen::Function::new("new");
    native_new.ret("Self").vis("pub");
    let mut native_new_block = Block::new("Self");
    // for clippy we generate a Default impl if new has no args
    let mut new_arg_count = 0;
    for field in &record.fields {
        if !gen_scope.deserialize_generated_for_type(&field.rust_type.conceptual_type) {
            gen_scope.dont_generate_deserialize(
                name,
                format!(
                    "field {}: {} couldn't generate serialize",
                    field.name,
                    field.rust_type.for_rust_member(types, false)
                ),
            );
        }
        // Fixed values only exist in (de)serialization code (outside of preserve-encodings=true)
        if !field.rust_type.is_fixed_value() {
            if let Some(default_value) = &field.rust_type.default {
                // field
                native_struct.field(
                    &format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false),
                );
                // new
                native_new_block.line(format!(
                    "{}: {},",
                    field.name,
                    default_value.to_primitive_str_assign()
                ));
            } else if field.optional {
                // field
                native_struct.field(
                    &format!("pub {}", field.name),
                    format!("Option<{}>", field.rust_type.for_rust_member(types, false)),
                );
                // new
                native_new_block.line(format!("{}: None,", field.name));
            } else {
                // field
                native_struct.field(
                    &format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false),
                );
                // new
                native_new.arg(&field.name, field.rust_type.for_rust_move(types));
                new_arg_count += 1;
                native_new_block.line(format!("{},", field.name));
            }
        }
    }
    let len_encoding_var = if CLI_ARGS.preserve_encodings {
        let encoding_name = RustIdent::new(CDDLIdent::new(format!("{name}Encoding")));
        native_struct.field(
            &format!(
                "{}pub encodings",
                encoding_var_macros(types.used_as_key(name))
            ),
            format!("Option<{encoding_name}>"),
        );
        native_new_block.line("encodings: None,");

        let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
        encoding_struct.field("pub len_encoding", "LenEncoding");
        if tag.is_some() {
            encoding_struct.field("pub tag_encoding", "Option<cbor_event::Sz>");
        }
        if record.rep == Representation::Map {
            encoding_struct.field("pub orig_deser_order", "Vec<usize>");
        }
        for field in &record.fields {
            // even fixed values still need to keep track of their encodings
            for field_enc in encoding_fields(
                types,
                &field.name,
                &field.rust_type.clone().resolve_aliases(),
                true,
            ) {
                encoding_struct.field(
                    &format!("pub {}", field_enc.field_name),
                    field_enc.type_name,
                );
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
    let (ser_func, mut ser_impl, mut ser_embedded_impl) = create_serialize_impls(
        name,
        Some(record.rep),
        tag,
        &record.definite_info(types),
        len_encoding_var
            .map(|var| {
                format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()")
            })
            .as_deref(),
        types.is_plain_group(name),
    );
    let mut ser_func = match ser_embedded_impl {
        Some(_) => {
            ser_impl.push_fn(ser_func);
            make_serialization_function("serialize_as_embedded_group")
        }
        None => ser_func,
    };
    let mut deser_code = DeserializationCode::default();
    let in_embedded = types.is_plain_group(name);
    let ctor_block = match record.rep {
        Representation::Array => {
            let mut deser_ret = Block::new(&format!("Ok({name}"));
            for field in record.fields.iter() {
                if field.optional {
                    let (optional_field_check, field_expr, expr_is_ref) = if let Some(
                        default_value,
                    ) =
                        &field.rust_type.default
                    {
                        (
                            if CLI_ARGS.preserve_encodings {
                                format!("if self.{} != {} || self.encodings.map(|encs| encs.{}_default_present).unwrap_or(false)", field.name, default_value.to_primitive_str_compare(), field.name)
                            } else {
                                format!(
                                    "if self.{} != {}",
                                    field.name,
                                    default_value.to_primitive_str_compare()
                                )
                            },
                            format!("self.{}", field.name),
                            false,
                        )
                    } else {
                        (
                            format!("if let Some(field) = &self.{}", field.name),
                            "field".to_owned(),
                            true,
                        )
                    };
                    let mut optional_array_ser_block = Block::new(&optional_field_check);
                    gen_scope.generate_serialize(
                        types,
                        (&field.rust_type).into(),
                        &mut optional_array_ser_block,
                        SerializeConfig::new(&field_expr, &field.name)
                            .expr_is_ref(expr_is_ref)
                            .encoding_var_in_option_struct("self.encodings"),
                    );
                    ser_func.push_block(optional_array_ser_block);
                    gen_scope.dont_generate_deserialize(
                        name,
                        format!(
                            "Array with optional field {}: {}",
                            field.name,
                            field.rust_type.for_rust_member(types, false)
                        ),
                    );
                } else {
                    gen_scope.generate_serialize(
                        types,
                        (&field.rust_type).into(),
                        &mut ser_func,
                        SerializeConfig::new(format!("self.{}", field.name), &field.name)
                            .encoding_var_in_option_struct("self.encodings"),
                    );
                    if CLI_ARGS.preserve_encodings {
                        let var_names_str =
                            encoding_var_names_str(types, &field.name, &field.rust_type);
                        if CLI_ARGS.annotate_fields {
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new("", "", true),
                                    DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                                )
                                .annotate(&field.name, &format!("let {var_names_str} = "), "?;")
                                .add_to_code(&mut deser_code);
                        } else {
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new(
                                        &format!("let {var_names_str} = "),
                                        ";",
                                        false,
                                    ),
                                    DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                                )
                                .add_to_code(&mut deser_code);
                        }
                    } else if field.rust_type.is_fixed_value() {
                        // don't set anything, only verify data
                        if CLI_ARGS.annotate_fields {
                            let mut err_deser = gen_scope.generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", true),
                                DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                            );
                            // this block needs to evaluate to a Result even though it has no value
                            err_deser.content.line("Ok(())");
                            err_deser
                                .annotate(&field.name, "", "?;")
                                .add_to_code(&mut deser_code);
                        } else {
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new("", "", false),
                                    DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                                )
                                .add_to_code(&mut deser_code);
                        }
                    } else if CLI_ARGS.annotate_fields {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", true),
                                DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                            )
                            .annotate(&field.name, &format!("let {} = ", field.name), "?;")
                            .add_to_code(&mut deser_code);
                    } else {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new(
                                    &format!("let {} = ", field.name),
                                    ";",
                                    false,
                                ),
                                DeserializeConfig::new(&field.name).in_embedded(in_embedded),
                            )
                            .add_to_code(&mut deser_code);
                    }
                    if !field.rust_type.is_fixed_value() {
                        deser_ret.line(&format!("{},", field.name));
                    }
                }
            }
            if CLI_ARGS.preserve_encodings {
                let mut encoding_ctor =
                    codegen::Block::new(&format!("encodings: Some({name}Encoding"));
                encoding_ctor.line("len_encoding,");
                if tag.is_some() {
                    encoding_ctor.line("tag_encoding: Some(tag_encoding),");
                }
                for field in record.fields.iter() {
                    // we don't support deserialization for optional fields so don't even bother
                    if !field.optional {
                        for field_enc in encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
                            true,
                        ) {
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
        }
        Representation::Map => {
            let mut uint_field_deserializers = Vec::new();
            let mut text_field_deserializers = Vec::new();
            // (field_index, field, content) -- this is ordered by canonical order
            let mut ser_content: Vec<(usize, &RustField, BlocksOrLines)> = Vec::new();
            if CLI_ARGS.preserve_encodings {
                deser_code
                    .content
                    .line("let mut orig_deser_order = Vec::new();");
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
                if let ConceptualRustType::Rust(ident) = &field.rust_type.conceptual_type {
                    if types.is_plain_group(ident) {
                        gen_scope.dont_generate_deserialize(
                            name,
                            format!(
                                "Map with plain group field {}: {}",
                                field.name,
                                field.rust_type.for_rust_member(types, false)
                            ),
                        );
                    }
                }
                // declare variables for deser loop
                if CLI_ARGS.preserve_encodings {
                    for field_enc in encoding_fields(
                        types,
                        &field.name,
                        &field.rust_type.clone().resolve_aliases(),
                        true,
                    ) {
                        deser_code.content.line(&format!(
                            "let mut {} = {};",
                            field_enc.field_name, field_enc.default_expr
                        ));
                    }
                    let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                    deser_code.content.line(&format!(
                        "let mut {} = {};",
                        key_enc.field_name, key_enc.default_expr
                    ));
                }
                if field.rust_type.is_fixed_value() {
                    deser_code
                        .content
                        .line(&format!("let mut {}_present = false;", field.name));
                } else {
                    deser_code
                        .content
                        .line(&format!("let mut {} = None;", field.name));
                }
                let (data_name, expr_is_ref) =
                    if field.optional && field.rust_type.default.is_none() {
                        (String::from("field"), true)
                    } else {
                        (format!("self.{}", field.name), false)
                    };

                let key = field.key.clone().unwrap();
                // deserialize key + value
                let mut deser_block = match &key {
                    FixedValue::Uint(x) => {
                        if CLI_ARGS.preserve_encodings {
                            Block::new(&format!("({x}, key_enc) => "))
                        } else {
                            Block::new(&format!("{x} => "))
                        }
                    }
                    FixedValue::Text(x) => Block::new(&format!("\"{x}\" => ")),
                    _ => panic!(
                        "unsupported map key type for {}.{}: {:?}",
                        name, field.name, key
                    ),
                };
                deser_block.after(",");
                let mut deser_block_code = DeserializationCode::default();
                let key_in_rust = match &key {
                    FixedValue::Uint(x) => format!("Key::Uint({x})"),
                    FixedValue::Text(x) => format!("Key::Str(\"{x}\".into())"),
                    _ => unimplemented!(),
                };
                if CLI_ARGS.preserve_encodings {
                    let mut dup_check = if field.rust_type.is_fixed_value() {
                        Block::new(&format!("if {}_present", field.name))
                    } else {
                        Block::new(&format!("if {}.is_some()", field.name))
                    };
                    dup_check.line(&format!(
                        "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                    ));
                    deser_block_code.content.push_block(dup_check);

                    let temp_var_prefix = format!("tmp_{}", field.name);
                    let var_names_str =
                        encoding_var_names_str(types, &temp_var_prefix, &field.rust_type);
                    if CLI_ARGS.annotate_fields {
                        let (before, after) = if var_names_str.is_empty() {
                            ("".to_owned(), "?")
                        } else {
                            (format!("let {var_names_str} = "), "?;")
                        };
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", true),
                                DeserializeConfig::new(&field.name)
                                    .in_embedded(in_embedded)
                                    .optional_field(field.optional),
                            )
                            .annotate(&field.name, &before, after)
                            .add_to_code(&mut deser_block_code);
                    } else {
                        let (before, after) = if var_names_str.is_empty() {
                            ("".to_owned(), "")
                        } else {
                            (format!("let {var_names_str} = "), ";")
                        };
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new(&before, after, false),
                                DeserializeConfig::new(&field.name)
                                    .in_embedded(in_embedded)
                                    .optional_field(field.optional),
                            )
                            .add_to_code(&mut deser_block_code);
                    }
                    // Due to destructuring assignemnt (RFC 372 / 71156) being unstable we're forced to use temporaries then reassign after
                    // which is not ideal but doing the assignment inside the lambda or otherwise has issues where it's putting lots of
                    // context-sensitive logic into generate_deserialize and you would need to declare temporaries in most cases anyway
                    // as cbor_event encoding-aware functions return tuples which just pushes the problem there instead.
                    // We might be able to write a nice way around this in the annotate_fields=false, preserve_encodings=true case
                    // but I don't think anyone (or many) would care about this as it's incredibly niche
                    // (annotate_fields=false would be for minimizing code size but then preserve_encodings=true generates way more code)
                    if field.rust_type.is_fixed_value() {
                        deser_block_code
                            .content
                            .line(&format!("{}_present = true;", field.name));
                    } else {
                        deser_block_code
                            .content
                            .line(&format!("{} = Some(tmp_{});", field.name, field.name));
                    }
                    for enc_field in encoding_fields(
                        types,
                        &field.name,
                        &field.rust_type.clone().resolve_aliases(),
                        false,
                    ) {
                        deser_block_code.content.line(&format!(
                            "{} = tmp_{};",
                            enc_field.field_name, enc_field.field_name
                        ));
                    }
                } else if field.rust_type.is_fixed_value() {
                    let mut dup_check = Block::new(&format!("if {}_present", field.name));
                    dup_check.line(&format!(
                        "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                    ));
                    deser_block_code.content.push_block(dup_check);
                    // only does verification and sets the field_present bool to do error checking later
                    if CLI_ARGS.annotate_fields {
                        let mut err_deser = gen_scope.generate_deserialize(
                            types,
                            (&field.rust_type).into(),
                            DeserializeBeforeAfter::new("", "", false),
                            DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional),
                        );
                        err_deser.content.line("Ok(true)");
                        err_deser
                            .annotate(&field.name, &format!("{}_present = ", field.name), "?;")
                            .add_to_code(&mut deser_block_code);
                    } else {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", false),
                                DeserializeConfig::new(&field.name)
                                    .in_embedded(in_embedded)
                                    .optional_field(field.optional),
                            )
                            .add_to_code(&mut deser_block_code);
                        deser_block_code
                            .content
                            .line(&format!("{}_present = true;", field.name));
                    }
                } else {
                    let mut dup_check = Block::new(&format!("if {}.is_some()", field.name));
                    dup_check.line(&format!(
                        "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                    ));
                    deser_block_code.content.push_block(dup_check);
                    if CLI_ARGS.annotate_fields {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", true),
                                DeserializeConfig::new(&field.name)
                                    .in_embedded(in_embedded)
                                    .optional_field(field.optional),
                            )
                            .annotate(&field.name, &format!("{} = Some(", field.name), "?);")
                            .add_to_code(&mut deser_block_code);
                    } else {
                        gen_scope
                            .generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new(
                                    &format!("{} = Some(", field.name),
                                    ");",
                                    false,
                                ),
                                DeserializeConfig::new(&field.name)
                                    .in_embedded(in_embedded)
                                    .optional_field(field.optional),
                            )
                            .add_to_code(&mut deser_block_code);
                    }
                }
                if CLI_ARGS.preserve_encodings {
                    let key_encoding_var = key_encoding_field(&field.name, &key).field_name;
                    let enc_conversion = match &key {
                        FixedValue::Uint(_) => "Some(key_enc)",
                        FixedValue::Text(_) => "StringEncoding::from(key_enc)",
                        _ => unimplemented!(),
                    };
                    deser_block_code
                        .content
                        .line(&format!("{key_encoding_var} = {enc_conversion};"))
                        .line(&format!("orig_deser_order.push({field_index});"));
                }

                // serialize key
                let mut map_ser_content = BlocksOrLines::default();
                let serialize_config = SerializeConfig::new(&data_name, &field.name)
                    .expr_is_ref(expr_is_ref)
                    .encoding_var_in_option_struct("self.encodings");
                let key_encoding_var =
                    serialize_config.encoding_var(Some("key"), key.encoding_var_is_copy());

                deser_block.push_all(deser_block_code.mark_and_extract_content(&mut deser_code));
                match &key {
                    FixedValue::Uint(x) => {
                        let expr = format!("{x}u64");
                        write_using_sz(
                            &mut map_ser_content,
                            "write_unsigned_integer",
                            "serializer",
                            &expr,
                            &expr,
                            "?;",
                            &key_encoding_var,
                        );
                        uint_field_deserializers.push(deser_block);
                    }
                    FixedValue::Text(s) => {
                        write_string_sz(
                            &mut map_ser_content,
                            "write_text",
                            "serializer",
                            &format!("\"{s}\""),
                            "?;",
                            &key_encoding_var,
                        );
                        text_field_deserializers.push(deser_block);
                    }
                    _ => panic!(
                        "unsupported map key type for {}.{}: {:?}",
                        name, field.name, key
                    ),
                };

                // serialize value
                gen_scope.generate_serialize(
                    types,
                    (&field.rust_type).into(),
                    &mut map_ser_content,
                    serialize_config,
                );
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
                    ("!force_canonical && ", format!("vec![{indices_str}]"))
                } else {
                    ("", format!("(0..{}).collect()", ser_content.len()))
                };
                ser_func.line(format!(
                    "let deser_order = self.encodings.as_ref().filter(|encs| {}encs.orig_deser_order.len() == {}).map(|encs| encs.orig_deser_order.clone()).unwrap_or_else(|| {});",
                    check_canonical,
                    record.definite_info(types),
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
                    let mut field_ser_block = if field.optional && field.rust_type.default.is_none()
                    {
                        Block::new(&format!(
                            "{} => if let Some(field) = &self.{}",
                            field_index, field.name
                        ))
                    } else {
                        Block::new(&format!("{field_index} =>"))
                    };
                    field_ser_block.push_all(content);
                    ser_loop_match.push_block(field_ser_block);
                }
                ser_loop_match.line("_ => unreachable!()").after(";");
                ser_loop.push_block(ser_loop_match);
                ser_func.push_block(ser_loop);
            } else {
                for (_field_index, field, content) in ser_content.into_iter() {
                    if field.optional {
                        let optional_ser_field_check =
                            if let Some(default_value) = &field.rust_type.default {
                                format!(
                                    "if self.{} != {}",
                                    field.name,
                                    default_value.to_primitive_str_compare()
                                )
                            } else {
                                format!("if let Some(field) = &self.{}", field.name)
                            };
                        let mut optional_ser_field = Block::new(&optional_ser_field_check);
                        optional_ser_field.push_all(content);
                        ser_func.push_block(optional_ser_field);
                    } else {
                        ser_func.push_all(content);
                    }
                }
            }
            // needs to be in one line rather than a block because Block::after() only takes a string
            deser_code.content.line("let mut read = 0;");
            let mut deser_loop = make_deser_loop("len", "read");
            let mut type_match = Block::new("match raw.cbor_type()?");
            if uint_field_deserializers.is_empty() {
                type_match.line("cbor_event::Type::UnsignedInteger => return Err(DeserializeFailure::UnknownKey(Key::Uint(raw.unsigned_integer()?)).into()),");
            } else {
                let mut uint_match = if CLI_ARGS.preserve_encodings {
                    Block::new(
                        "cbor_event::Type::UnsignedInteger => match raw.unsigned_integer_sz()?",
                    )
                } else {
                    Block::new("cbor_event::Type::UnsignedInteger => match raw.unsigned_integer()?")
                };
                for case in uint_field_deserializers {
                    uint_match.push_block(case);
                }
                let unknown_key_decl = if CLI_ARGS.preserve_encodings {
                    "(unknown_key, _enc)"
                } else {
                    "unknown_key"
                };
                uint_match.line(format!("{unknown_key_decl} => return Err(DeserializeFailure::UnknownKey(Key::Uint(unknown_key)).into()),"));
                uint_match.after(",");
                type_match.push_block(uint_match);
            }
            // we can't map text_sz() with String::as_str() to match it since that would return a reference to a temporary
            // so we need to store it in a local and have an extra block to declare it
            if text_field_deserializers.is_empty() {
                type_match.line("cbor_event::Type::Text => return Err(DeserializeFailure::UnknownKey(Key::Str(raw.text()?)).into()),");
            } else if CLI_ARGS.preserve_encodings {
                let mut outer_match = Block::new("cbor_event::Type::Text =>");
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
                let mut text_match =
                    Block::new("cbor_event::Type::Text => match raw.text()?.as_str()");
                for case in text_field_deserializers {
                    text_match.push_block(case);
                }
                text_match.line("unknown_key => return Err(DeserializeFailure::UnknownKey(Key::Str(unknown_key.to_owned())).into()),");
                text_match.after(",");
                type_match.push_block(text_match);
            }
            let mut special_match = Block::new("cbor_event::Type::Special => match len");
            special_match.line(format!(
                "{} => return Err(DeserializeFailure::BreakInDefiniteLen.into()),",
                cbor_event_len_n("_")
            ));
            // TODO: this will need to change if we support Special values as keys (e.g. true / false)
            let mut break_check = Block::new(&format!(
                "{} => match raw.special()?",
                cbor_event_len_indef()
            ));
            break_check.line("cbor_event::Special::Break => break,");
            break_check.line("_ => return Err(DeserializeFailure::EndingBreakMissing.into()),");
            break_check.after(",");
            special_match.push_block(break_check);
            special_match.after(",");
            type_match.push_block(special_match);
            type_match.line("other_type => return Err(DeserializeFailure::UnexpectedKeyType(other_type).into()),");
            deser_loop.push_block(type_match);
            deser_loop.line("read += 1;");
            deser_code.content.push_block(deser_loop);
            let mut ctor_block = Block::new("Ok(Self");
            // make sure the field is present, and unwrap the Option<T>
            for field in &record.fields {
                if !field.optional {
                    let key = match &field.key {
                        Some(FixedValue::Uint(x)) => format!("Key::Uint({x})"),
                        Some(FixedValue::Text(x)) => format!("Key::Str(String::from(\"{x}\"))"),
                        None => unreachable!(),
                        _ => unimplemented!(),
                    };
                    if field.rust_type.is_fixed_value() {
                        let mut mandatory_field_check =
                            Block::new(&format!("if !{}_present", field.name));
                        mandatory_field_check.line(format!(
                            "return Err(DeserializeFailure::MandatoryFieldMissing({key}).into());"
                        ));
                        deser_code.content.push_block(mandatory_field_check);
                    } else {
                        let mut mandatory_field_check =
                            Block::new(&format!("let {} = match {}", field.name, field.name));
                        mandatory_field_check.line("Some(x) => x,");

                        mandatory_field_check.line(format!("None => return Err(DeserializeFailure::MandatoryFieldMissing({key}).into()),"));
                        mandatory_field_check.after(";");
                        deser_code.content.push_block(mandatory_field_check);
                    }
                } else if let Some(default_value) = &field.rust_type.default {
                    if CLI_ARGS.preserve_encodings {
                        let mut default_present_check = Block::new(&format!(
                            "if {} == Some({})",
                            field.name,
                            default_value.to_primitive_str_assign()
                        ));
                        default_present_check
                            .line(format!("{}_default_present = true;", field.name));
                        deser_code.content.push_block(default_present_check);
                    }
                    match default_value {
                        FixedValue::Text(_) => {
                            // to avoid clippy::or_fun_call
                            deser_code.content.line(&format!(
                                "let {} = {}.unwrap_or_else(|| {});",
                                field.name,
                                field.name,
                                default_value.to_primitive_str_assign()
                            ));
                        }
                        FixedValue::Bool(_)
                        | FixedValue::Nint(_)
                        | FixedValue::Null
                        | FixedValue::Float(_)
                        | FixedValue::Uint(_) => {
                            deser_code.content.line(&format!(
                                "let {} = {}.unwrap_or({});",
                                field.name,
                                field.name,
                                default_value.to_primitive_str_assign()
                            ));
                        }
                    }
                }
                if !field.rust_type.is_fixed_value() {
                    ctor_block.line(format!("{},", field.name));
                }
            }
            if CLI_ARGS.preserve_encodings {
                let mut encoding_ctor =
                    codegen::Block::new(&format!("encodings: Some({name}Encoding"));
                if tag.is_some() {
                    encoding_ctor.line("tag_encoding: Some(tag_encoding),");
                }
                encoding_ctor
                    .line("len_encoding,")
                    .line("orig_deser_order,");
                for field in record.fields.iter() {
                    let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                    encoding_ctor.line(format!("{},", key_enc.field_name));
                    for field_enc in encoding_fields(
                        types,
                        &field.name,
                        &field.rust_type.clone().resolve_aliases(),
                        true,
                    ) {
                        encoding_ctor.line(format!("{},", field_enc.field_name));
                    }
                }
                encoding_ctor.after("),");
                ctor_block.push_block(encoding_ctor);
            }
            ctor_block.after(")");
            ctor_block
        }
    };
    let len_enc_var = len_encoding_var
        .map(|var| format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()"))
        .unwrap_or_default();
    end_len(&mut ser_func, "serializer", &len_enc_var, true);
    match &mut ser_embedded_impl {
        Some(ser_embedded_impl) => ser_embedded_impl.push_fn(ser_func),
        None => ser_impl.push_fn(ser_func),
    };
    let mut deser_scaffolding = BlocksOrLines::default();
    let (mut deser_impl, mut deser_embedded_impl) = create_deserialize_impls(
        name,
        Some(record.rep),
        tag,
        record.cbor_len_info(types),
        types.is_plain_group(name),
        len_encoding_var,
        &mut deser_scaffolding,
    );
    if deser_embedded_impl.is_none() {
        // ending checks are included with embedded serialization setup
        // since we are populating deserialize_as_embedded_group() and deserialize()
        // is already complete
        // but these checks must be done manually here *after* we populate deserialize()
        add_deserialize_final_len_check(
            &mut deser_code.content,
            Some(record.rep),
            record.cbor_len_info(types),
        );
    }
    deser_code.content.push_block(ctor_block);

    if CLI_ARGS.annotate_fields {
        deser_code = deser_code.annotate(name.as_ref(), "", "");
    }

    if let Some(deser_embedded_impl) = &mut deser_embedded_impl {
        let mut deser_f = make_deserialization_function("deserialize");
        deser_f.push_all(deser_scaffolding);
        deser_impl.push_fn(deser_f);
        let mut deser_embed_f = make_deserialization_function("deserialize_as_embedded_group");
        let read_len_arg = if deser_code.read_len_used {
            "read_len"
        } else {
            "_read_len"
        };
        deser_embed_f.arg(read_len_arg, "&mut CBORReadLen");
        if CLI_ARGS.preserve_encodings {
            deser_embed_f.arg("len", "cbor_event::LenSz");
        } else {
            deser_embed_f.arg("len", "cbor_event::Len");
        }
        // this is expected when creating the final struct but wouldn't have been available
        // otherwise as it is in the non-embedded deserialiation function
        if CLI_ARGS.preserve_encodings {
            deser_embed_f.line("let len_encoding = len.into();");
        }
        deser_embed_f.push_all(deser_code.content);
        deser_embedded_impl.push_fn(deser_embed_f);
    } else {
        let mut deser_f = make_deserialization_function("deserialize");
        deser_f.push_all(deser_scaffolding);
        deser_f.push_all(deser_code.content);
        deser_impl.push_fn(deser_f);
    }
    push_rust_struct(
        gen_scope,
        types,
        name,
        native_struct,
        native_impl,
        ser_impl,
        ser_embedded_impl,
    );
    // for clippy we generate a Default when new takes no args.
    // We keep new() for consistency with other types.
    if new_arg_count == 0 {
        let mut default_impl = codegen::Impl::new(name.to_string());
        default_impl
            .impl_trait("Default")
            .new_fn("default")
            .ret("Self")
            .line("Self::new()");
        gen_scope.rust(types, name).push_impl(default_impl);
    }
    // TODO: generic deserialize (might need backtracking)
    if gen_scope.deserialize_generated(name) {
        gen_scope.rust_serialize().push_impl(deser_impl);
        if let Some(deser_embedded_impl) = deser_embedded_impl {
            gen_scope.rust_serialize().push_impl(deser_embedded_impl);
        }
    }
}

fn codegen_group_choices(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Representation,
    tag: Option<usize>,
) {
    // rust inner enum
    generate_enum(gen_scope, types, name, variants, Some(rep), false, tag);

    // wasm wrapper
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true);
        // new (1 per variant)
        for variant in variants.iter() {
            // TODO: verify if variant.serialize_as_embedded_group impacts ctor generation
            let mut new_func = codegen::Function::new(&format!("new_{}", variant.name_as_var()));
            new_func.ret("Self").vis("pub");
            let mut output_comma = false;
            // We only want to generate Variant::new() calls when we created a special struct
            // for the variant, which happens in the general case for multi-field group choices
            let fields = match &variant.rust_type.conceptual_type {
                // we need to check for sanity here, as if we're referring to the ident
                // it should at this stage be registered
                ConceptualRustType::Rust(ident) => {
                    match types.rust_struct(ident).unwrap().variant() {
                        RustStructType::Record(record) => Some(&record.fields),
                        _ => None,
                    }
                }
                ConceptualRustType::Alias(_, _) => {
                    unimplemented!("TODO: do we need to handle aliases here?")
                }
                _ => None,
            };
            match fields {
                Some(fields) => {
                    let ctor_fields: Vec<&RustField> = fields
                        .iter()
                        .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                        .collect();
                    match ctor_fields.len() {
                        0 => {
                            new_func.line(format!(
                                "Self({}::new_{}())",
                                rust_crate_struct_from_wasm(types, name),
                                variant.name_as_var()
                            ));
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
                                "Self({}::new_{}(",
                                rust_crate_struct_from_wasm(types, name),
                                variant.name_as_var()
                            );
                            for field in ctor_fields {
                                if output_comma {
                                    ctor.push_str(", ");
                                } else {
                                    output_comma = true;
                                }
                                new_func.arg(&field.name, field.rust_type.for_wasm_param());
                                ctor.push_str(&ToWasmBoundaryOperations::format(
                                    field
                                        .rust_type
                                        .from_wasm_boundary_clone(&field.name, false)
                                        .into_iter(),
                                ));
                                // ^ TODO: check types.can_new_fail(&field.name)
                            }
                            ctor.push_str("))");
                            new_func.line(ctor);
                        }
                    }
                }
                None => {
                    // just directly pass in the variant's type
                    if variant.rust_type.is_fixed_value() {
                        new_func.line(format!(
                            "Self({}::new_{}())",
                            rust_crate_struct_from_wasm(types, name),
                            variant.name_as_var()
                        ));
                    } else {
                        let field_name = variant.name.to_string();
                        new_func
                            .arg(&field_name, variant.rust_type.for_wasm_param())
                            .line(format!(
                                "Self({}::new_{}({}))",
                                rust_crate_struct_from_wasm(types, name),
                                variant.name_as_var(),
                                ToWasmBoundaryOperations::format(
                                    variant
                                        .rust_type
                                        .from_wasm_boundary_clone(&field_name, false)
                                        .into_iter()
                                )
                            ));
                        // ^ TODO: check types.can_new_fail(&field.name)
                    }
                }
            }
            wrapper.s_impl.push_fn(new_func);
        }
        // enum-getters
        add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, Some(rep));
        wrapper.push(gen_scope, types);
    }
}

fn add_wasm_enum_getters(
    s_impl: &mut codegen::Impl,
    types: &IntermediateTypes<'_>,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
) {
    assert!(CLI_ARGS.wasm);
    // kind() getter
    let kind_name = format!("{name}Kind");
    let mut get_kind = codegen::Function::new("kind");
    get_kind.arg_ref_self().vis("pub").ret(&kind_name);
    let mut get_kind_match = codegen::Block::new("match &self.0");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep);
        get_kind_match.line(format!(
            "{}::{}{} => {}::{},",
            rust_crate_struct_from_wasm(types, name),
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
        if !variant.rust_type.is_fixed_value() {
            let enum_gen_info = EnumVariantInRust::new(types, variant, rep);
            let mut as_variant = codegen::Function::new(&format!("as_{}", variant.name_as_var()));
            as_variant
                .arg_ref_self()
                .vis("pub")
                .ret(&format!("Option<{}>", variant.rust_type.for_wasm_return()));
            let mut variant_match = codegen::Block::new("match &self.0");
            variant_match.line(format!(
                "{}::{}{} => Some({}),",
                rust_crate_struct_from_wasm(types, name),
                variant.name,
                enum_gen_info.capture_ignore_encodings(),
                variant
                    .rust_type
                    .to_wasm_boundary(&enum_gen_info.names[0], true)
            ));
            variant_match.line("_ => None,");
            as_variant.push_block(variant_match);
            s_impl.push_fn(as_variant);
        }
    }
}

fn cbor_event_len_n(n: &str) -> String {
    if CLI_ARGS.preserve_encodings {
        format!("cbor_event::LenSz::Len({n}, _)")
    } else {
        format!("cbor_event::Len::Len({n})")
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
    fn new(types: &IntermediateTypes, variant: &EnumVariant, rep: Option<Representation>) -> Self {
        let name = variant.name_as_var();
        let mut enc_fields = if CLI_ARGS.preserve_encodings {
            encoding_fields(
                types,
                &name,
                &variant.rust_type.clone().resolve_aliases(),
                true,
            )
        } else {
            vec![]
        };
        let (mut types, mut names) = if variant.rust_type.is_fixed_value() {
            (vec![], vec![])
        } else {
            (
                vec![variant.rust_type.for_rust_member(types, false)],
                vec![name],
            )
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
                    format!("{}{}", encoding_var_macros(used_in_key), name)
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
                let mut ctor = Block::new(&format!("{}Self::{}", before, self.name));
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

// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choicecs which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
fn generate_enum(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    generate_deserialize_directly: bool,
    tag: Option<usize>,
) {
    if CLI_ARGS.wasm {
        // also create a wasm-exposed enum just to distinguish the type
        let mut kind = codegen::Enum::new(&format!("{name}Kind"));
        kind.vis("pub");
        for variant in variants.iter() {
            kind.new_variant(&variant.name.to_string());
        }
        kind.attr("wasm_bindgen");
        gen_scope.wasm(types, name).push_enum(kind);
    }

    // rust enum containing the data
    let mut e = codegen::Enum::new(&name.to_string());
    e.vis("pub");
    let mut e_impl = codegen::Impl::new(name.to_string());
    // instead of using create_serialize_impl() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(&mut e, types.used_as_key(name), true);
    let mut ser_impl = make_serialization_impl(name.as_ref());
    let mut ser_func = make_serialization_function("serialize");
    if let Some(tag) = tag {
        // TODO: how to even store these? (maybe it could be a new field in every enum variant)
        assert!(!CLI_ARGS.preserve_encodings);
        ser_func.line(format!("serializer.write_tag({tag}u64)?;"));
    }
    let mut ser_array_match_block = Block::new("match self");
    // we use Dynamic to avoid having any length checks here since we don't know what they are yet without testing the variants
    // and it's not worth looking into and complicating things on the off chance that all variants are the same length.
    let len_info = RustStructCBORLen::Dynamic;
    let mut deser_func = make_deserialization_function("deserialize");
    let mut error_annotator = make_err_annotate_block(name.as_ref(), "", "");
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
        let outer_encoding_var = if CLI_ARGS.preserve_encodings
            && variants
                .iter()
                .any(|variant| !variant.serialize_as_embedded_group)
        {
            Some("outer_len_encoding")
        } else {
            None
        };
        let (deser_impl, _deser_embedded_impl) = create_deserialize_impls(
            name,
            rep,
            tag,
            len_info,
            false,
            outer_encoding_var,
            deser_body,
        );
        deser_impl
    };
    deser_body.line("let initial_position = raw.as_mut_ref().seek(SeekFrom::Current(0)).unwrap();");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep);
        let variant_var_name = variant.name_as_var();
        let mut v = codegen::Variant::new(&variant.name.to_string());
        match enum_gen_info.names.len() {
            0 => {}
            1 if enum_gen_info.enc_fields.is_empty() => {
                v.tuple(&enum_gen_info.types[0]);
            }
            _ => {
                for (name_with_macros, type_str) in enum_gen_info
                    .names_with_macros(types.used_as_key(name))
                    .into_iter()
                    .zip(enum_gen_info.types.iter())
                {
                    v.named(&name_with_macros, type_str);
                }
            }
        }
        e.push_variant(v);
        // new (particularly useful if we have encoding variables)
        let mut new_func = codegen::Function::new(&format!("new_{variant_var_name}"));
        new_func.ret("Self").vis("pub");
        let mut output_comma = false;
        // We only want to generate Variant::new() calls when we created a special struct
        // for the variant, which happens in the general case for multi-field group choices
        let fields = match &variant.rust_type.conceptual_type {
            // we need to check for sanity here, as if we're referring to the ident
            // it should at this stage be registered
            ConceptualRustType::Rust(ident) => {
                match types
                    .rust_struct(ident)
                    .unwrap_or_else(|| panic!("{}", "{name} refers to undefined ident: {ident}"))
                    .variant()
                {
                    RustStructType::Record(record) => Some(&record.fields),
                    _ => None,
                }
            }
            _ => None,
        };
        let mut init_fields = match rep.and(fields) {
            Some(fields) => {
                let ctor_fields: Vec<&RustField> = fields
                    .iter()
                    .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                    .collect();
                let mut ctor = format!("{}::new(", variant.name);
                for field in ctor_fields {
                    if output_comma {
                        ctor.push_str(", ");
                    } else {
                        output_comma = true;
                    }
                    new_func.arg(&field.name, field.rust_type.for_rust_move(types));
                    ctor.push_str(&field.name);
                    // ^ TODO: check types.can_new_fail(&field.name)?
                }
                ctor.push(')');
                vec![ctor]
            }
            None => {
                if variant.rust_type.is_fixed_value() {
                    vec![]
                } else {
                    // just directly pass in the variant's type
                    let field_name = variant.name_as_var();
                    new_func.arg(&field_name, variant.rust_type.for_rust_move(types));
                    vec![field_name]
                    // ^ TODO: check types.can_new_fail(&field.name)?
                }
            }
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
                canonical_param()
            ));
        } else {
            let mut case_block = Block::new(&format!(
                "{}::{}{} =>",
                name,
                variant.name,
                enum_gen_info.capture_all()
            ));
            if CLI_ARGS.preserve_encodings {
                if let Some(r) = rep {
                    // group choice
                    let n = variant.rust_type.expanded_field_count(types).expect(
                        "preserve-encodings=true not supported with varying-size group choice",
                    );
                    start_len(
                        &mut case_block,
                        r,
                        "serializer",
                        "outer_len_encoding",
                        &n.to_string(),
                    );
                    gen_scope.generate_serialize(
                        types,
                        (&variant.rust_type).into(),
                        &mut case_block,
                        SerializeConfig::new(&variant_var_name, &variant_var_name)
                            .expr_is_ref(true)
                            .encoding_var_is_ref(true),
                    );
                    end_len(&mut case_block, "serializer", "outer_len_encoding", false);
                    case_block.line("Ok(serializer)");
                } else {
                    // type choice
                    gen_scope.generate_serialize(
                        types,
                        (&variant.rust_type).into(),
                        &mut case_block,
                        SerializeConfig::new(&variant_var_name, &variant_var_name)
                            .expr_is_ref(true)
                            .is_end(true)
                            .encoding_var_is_ref(true),
                    );
                }
            } else {
                let write_break = match rep {
                    Some(r) => {
                        let (len_str, indefinite) =
                            match variant.rust_type.expanded_field_count(types) {
                                Some(n) => (cbor_event_len_n(&n.to_string()), false),
                                None => (String::from(cbor_event_len_indef()), true),
                            };
                        let func_str = match r {
                            Representation::Array => "write_array",
                            Representation::Map => "write_map",
                        };
                        case_block.line(format!("serializer.{func_str}({len_str})?;"));
                        indefinite
                    }
                    None => false,
                };
                // TODO: only generate a block if the serialize is more than 1 line
                // Problem: generate_serialize() works in terms of line() and push_block()
                //          but we'd just want to inline the single one inside of a line...
                gen_scope.generate_serialize(
                    types,
                    (&variant.rust_type).into(),
                    &mut case_block,
                    SerializeConfig::new(&variant_var_name, &variant_var_name)
                        .expr_is_ref(true)
                        .is_end(!write_break),
                );
                if write_break {
                    case_block.line("serializer.write_special(cbor_event::Special::Break)");
                }
            }
            case_block.after(",");
            ser_array_match_block.push_block(case_block);
        }
        // deserialize
        // TODO: don't backtrack if variants begin with non-overlapping cbor types
        // TODO: how to detect when a greedy match won't work? (ie choice with choices in a choice possibly)
        let variant_deser_code = if enum_gen_info.types.is_empty() {
            let mut code = gen_scope.generate_deserialize(
                types,
                (&variant.rust_type).into(),
                DeserializeBeforeAfter::new("", "", false),
                DeserializeConfig::new(&variant.name_as_var()),
            );
            code.content.line("Ok(())");
            code
        } else {
            gen_scope.generate_deserialize(
                types,
                (&variant.rust_type).into(),
                DeserializeBeforeAfter::new("", "", true),
                DeserializeConfig::new(&variant.name_as_var()),
            )
        };
        let mut return_if_deserialized = match variant_deser_code.content.as_single_line() {
            Some(single_line) if !variant_deser_code.throws => {
                // to get around type annotations being needed for error types (e.g. auto conversions with ?) we make a variable
                // to do better than this we'd need to make DeserializationCode keep track of error types too.
                deser_body.line(&format!(
                    "let deser_variant: Result<_, DeserializeError> = {single_line};"
                ));
                Block::new("match deser_variant")
            }
            _ => {
                let mut variant_deser =
                    Block::new("match (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>");
                variant_deser.after(")(raw)");
                variant_deser.push_all(variant_deser_code.content);
                deser_body.push_block(variant_deser);
                // can't chain blocks so we just put them one after the other
                Block::new("")
            }
        };
        let names_without_outer = enum_gen_info.names_without_outer();
        if names_without_outer.is_empty() {
            return_if_deserialized
                .line(format!("Ok(()) => return Ok({}::{}),", name, variant.name));
        } else {
            enum_gen_info.generate_constructor(
                &mut return_if_deserialized,
                &if names_without_outer.len() > 1 {
                    format!("Ok(({})) => return Ok(", names_without_outer.join(", "))
                } else {
                    format!("Ok({}) => return Ok(", names_without_outer.join(", "))
                },
                "),",
                None,
            );
        }
        return_if_deserialized
            .line("Err(_) => raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap(),");
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
    deser_body.line(&format!(
        "Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatched))"
    ));
    if CLI_ARGS.annotate_fields {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    gen_scope.rust(types, name).push_enum(e).push_impl(e_impl);
    gen_scope
        .rust_serialize()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

fn make_serialization_function(name: &str) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f.generic("'se, W: Write")
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
    f.generic("R: BufRead + Seek")
        .ret("Result<Self, DeserializeError>")
        .arg("raw", "&mut Deserializer<R>");
    f
}

fn make_encoding_struct(encoding_name: &str) -> codegen::Struct {
    let mut encoding_struct = codegen::Struct::new(encoding_name.to_string());
    encoding_struct
        .vis("pub")
        .derive("Clone")
        .derive("Debug")
        .derive("Default");
    encoding_struct
}

fn generate_tag_check(deser_func: &mut dyn CodeBlock, ident: &RustIdent, tag: Option<usize>) {
    if let Some(tag) = tag {
        deser_func.line(&format!(
            "let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{ident}\"))?;"
        ));
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        tag_check.line(&format!("return Err(DeserializeError::new(\"{ident}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        deser_func.push_block(tag_check);
    }
}

// This is used mostly for when thing are tagged have specific ranges.
fn generate_wrapper_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    type_name: &RustIdent,
    field_type: &RustType,
    min_max: Option<(Option<i128>, Option<i128>)>,
) {
    if min_max.is_some() {
        assert!(types.can_new_fail(type_name));
    }
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, type_name, true);
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
            wasm_new.ret("Self").line(format!(
                "Self({})",
                ToWasmBoundaryOperations::format(ops.into_iter())
            ));
        }
        let mut get = codegen::Function::new("get");
        get.vis("pub")
            .arg_ref_self()
            .ret(field_type.for_wasm_return())
            .line(field_type.to_wasm_boundary("self.0.get()", false));
        wrapper.s_impl.push_fn(get);
        wrapper.push(gen_scope, types);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(types, type_name);
    s.vis("pub");
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{type_name}Encoding")));
    let enc_fields = if CLI_ARGS.preserve_encodings {
        s.field("pub inner", field_type.for_rust_member(types, false));
        let enc_fields =
            encoding_fields(types, "inner", &field_type.clone().resolve_aliases(), true);

        if !enc_fields.is_empty() {
            s.field(
                &format!(
                    "{}pub encodings",
                    encoding_var_macros(types.used_as_key(type_name))
                ),
                format!("Option<{encoding_name}>"),
            );
            let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
            for field_enc in &enc_fields {
                encoding_struct.field(
                    &format!("pub {}", field_enc.field_name),
                    &field_enc.type_name,
                );
            }
            gen_scope.cbor_encodings().push_struct(encoding_struct);
        }
        Some(enc_fields)
    } else {
        s.tuple_field(
            Some("pub".to_string()),
            field_type.for_rust_member(types, false),
        );
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
    get.vis("pub").arg_ref_self();
    if field_type.is_copy() {
        get.ret(field_type.for_rust_member(types, false))
            .line(field_type.clone_if_not_copy(self_var));
    } else {
        get.ret(format!("&{}", field_type.for_rust_member(types, false)))
            .line(format!("&{self_var}"));
    }
    s_impl.push_fn(get);
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_impl = make_serialization_impl(type_name.as_ref());
    gen_scope.generate_serialize(
        types,
        field_type.into(),
        &mut ser_func,
        SerializeConfig::new(self_var, "inner")
            .is_end(true)
            .encoding_var_in_option_struct("self.encodings"),
    );
    ser_impl.push_fn(ser_func);
    let mut deser_func = make_deserialization_function("deserialize");
    let mut deser_impl = codegen::Impl::new(&type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let ConceptualRustType::Rust(id) = &field_type.conceptual_type {
        if types.is_plain_group(id) {
            unimplemented!("TODO: make len/read_len variables of appropriate sizes so the generated code compiles");
        }
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move(types))
        .vis("pub");
    let var_names_str = if CLI_ARGS.preserve_encodings {
        encoding_var_names_str(types, "inner", field_type)
    } else {
        "inner".to_owned()
    };
    let from_impl = if let Some((min, max)) = min_max {
        let (before, after) = if var_names_str.is_empty() {
            ("".to_owned(), "")
        } else {
            (format!("let {var_names_str} = "), ";")
        };
        gen_scope
            .generate_deserialize(
                types,
                field_type.into(),
                DeserializeBeforeAfter::new(&before, after, false),
                DeserializeConfig::new("inner"),
            )
            .add_to(&mut deser_func);

        let against = if field_type
            .encodings
            .contains(&CBOREncodingOperation::CBORBytes)
        {
            "inner.len()"
        } else {
            match &field_type.conceptual_type {
                ConceptualRustType::Primitive(p) => match p {
                    Primitive::Bytes | Primitive::Str => "inner.len()",
                    Primitive::Bool
                    | Primitive::F32
                    | Primitive::F64
                    | Primitive::U8
                    | Primitive::U16
                    | Primitive::U32
                    | Primitive::U64
                    | Primitive::I8
                    | Primitive::I16
                    | Primitive::I32
                    | Primitive::I64
                    | Primitive::N64 => "inner",
                },
                _ => unimplemented!(),
            }
        };
        let mut check = match (min, max) {
            (Some(min), Some(max)) => if min == max {
                Block::new(&format!("if {against} != {min}"))
            } else {
                let non_negative = field_type.encodings.is_empty() && match &field_type.conceptual_type {
                    ConceptualRustType::Primitive(p) => match p {
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
                        Primitive::N64 |
                        Primitive::F32 |
                        Primitive::F64 => false,
                    },
                    _ => unimplemented!(),
                };
                if min == 0 && non_negative {
                    Block::new(&format!("if {against} > {max}"))
                } else {
                    Block::new(&format!("if {against} < {min} || {against} > {max}"))
                }
            },
            (Some(min), None) => Block::new(&format!("if {against} < {min}")),
            (None, Some(max)) => Block::new(&format!("if {against} > {max}")),
            (None, None) => panic!("How did we end up with a range requirement of (None, None)? Entire thing should've been None then"),
        };
        check.line(format!(
            "return Err(DeserializeError::new(\"{}\", DeserializeFailure::RangeCheck{{ found: {}, min: {}, max: {} }}));",
            type_name,
            against,
            match min {
                Some(min) => format!("Some({min})"),
                None => String::from("None")
            },
            match max {
                Some(max) => format!("Some({max})"),
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
                let mut encoding_ctor =
                    codegen::Block::new(&format!("encodings: Some({encoding_name}"));
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
        let mut try_from = codegen::Impl::new(type_name.to_string());
        try_from
            .associate_type("Error", "DeserializeError")
            .impl_trait(format!(
                "TryFrom<{}>",
                field_type.for_rust_member(types, false)
            ))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_member(types, false))
            .ret("Result<Self, Self::Error>")
            .line(format!(
                "{}::new({})",
                type_name,
                ToWasmBoundaryOperations::format(
                    field_type
                        .from_wasm_boundary_clone("inner", false)
                        .into_iter()
                )
            ));
        try_from
    } else {
        // let field_type_tagged = if let Some(t) = tag {
        //     ConceptualRustType::Tagged(t, Box::new(field_type.clone()))
        // } else {
        //     field_type.clone()
        // };
        // gen_scope.generate_deserialize(types, &field_type_tagged, "inner", "Ok(Self(", "))", false, false, true, &mut deser_func);
        new_func.ret("Self");
        if let Some(enc_fields) = &enc_fields {
            let (before, after) = if var_names_str.is_empty() {
                ("".to_owned(), "")
            } else {
                (format!("let {var_names_str} = "), ";")
            };
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new(&before, after, false),
                    DeserializeConfig::new("inner"),
                )
                .add_to(&mut deser_func);

            let mut deser_ctor = codegen::Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor =
                    codegen::Block::new(&format!("encodings: Some({encoding_name}"));
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
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new("Ok(Self(", "))", false),
                    DeserializeConfig::new("inner"),
                )
                .add_to(&mut deser_func);
            new_func.line("Self(inner)");
        }

        let mut from = codegen::Impl::new(type_name.to_string());
        from.impl_trait(format!(
            "From<{}>",
            field_type.for_rust_member(types, false)
        ))
        .new_fn("from")
        .arg("inner", field_type.for_rust_member(types, false))
        .ret("Self")
        .line(format!(
            "{}::new({})",
            type_name,
            ToWasmBoundaryOperations::format(
                field_type
                    .from_wasm_boundary_clone("inner", false)
                    .into_iter()
            )
        ));
        from
    };
    deser_impl.push_fn(deser_func);
    s_impl.push_fn(new_func);
    let mut from_inner_impl = codegen::Impl::new(field_type.for_rust_member(types, false));
    from_inner_impl
        .impl_trait(format!("From<{type_name}>"))
        .new_fn("from")
        .arg("wrapper", type_name.to_string())
        .ret("Self")
        .line(format!("wrapper.{inner_var}"));
    gen_scope
        .rust(types, type_name)
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
    } else if CLI_ARGS.preserve_encodings {
        &["Eq", "PartialEq", "Ord", "PartialOrd", "Hash"]
    } else {
        &["Eq", "PartialEq", "Ord", "PartialOrd"]
    }
}

fn add_struct_derives<T: DataType>(data_type: &mut T, used_in_key: bool, is_enum: bool) {
    data_type.derive("Clone").derive("Debug");
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
            data_type.derive(&format!(
                "derivative::Derivative)]\n#[derivative({}",
                key_derives(false)
                    .iter()
                    .map(|tr| match *tr {
                        // the derivative crate doesn't support enums tagged with ord/partialord yet without this
                        "Ord" | "PartialOrd" if is_enum =>
                            format!("{tr}=\"feature_allow_slow_enum\""),
                        _ => String::from(*tr),
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ));
        } else {
            for key_derive in key_derives(false) {
                data_type.derive(key_derive);
            }
        }
    }
}

fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    if CLI_ARGS.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, &ident, true);
        let mut wasm_new = codegen::Function::new("new");
        let mut new_if = codegen::Block::new("if x >= 0");
        let mut new_else = codegen::Block::new("else");
        new_if.line(format!(
            "Self({}::Int::new_uint(x as u64))",
            CLI_ARGS.lib_name_code()
        ));
        new_else.line(format!(
            "Self({}::Int::new_nint((x + 1).abs() as u64))",
            CLI_ARGS.lib_name_code()
        ));
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
        wrapper.push(gen_scope, types);
    }

    let mut native_struct = codegen::Enum::new("Int");
    native_struct.vis("pub");
    let mut uint = codegen::Variant::new("Uint");
    let mut nint = codegen::Variant::new("Nint");
    if CLI_ARGS.preserve_encodings {
        uint.named("value", "u64").named(
            &format!("{}encoding", encoding_var_macros(types.used_as_key(&ident))),
            "Option<cbor_event::Sz>",
        );
        nint.named("value", "u64").named(
            &format!("{}encoding", encoding_var_macros(types.used_as_key(&ident))),
            "Option<cbor_event::Sz>",
        );
    } else {
        uint.tuple("u64");
        nint.tuple("u64");
    }
    native_struct.push_variant(uint);
    native_struct.push_variant(nint);
    add_struct_derives(&mut native_struct, types.used_as_key(&ident), true);

    // impl Int
    let mut native_impl = codegen::Impl::new("Int");
    let mut new_uint = codegen::Function::new("new_uint");
    new_uint.vis("pub").arg("value", "u64").ret("Self");
    if CLI_ARGS.preserve_encodings {
        let mut new_uint_ctor = codegen::Block::new("Self::Uint");
        new_uint_ctor.line("value,").line("encoding: None,");
        new_uint.push_block(new_uint_ctor);
    } else {
        new_uint.line("Self::Uint(value)");
    }
    native_impl.push_fn(new_uint);

    let mut new_nint = codegen::Function::new("new_nint");
    new_nint
        .vis("pub")
        .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
        .arg("value", "u64")
        .ret("Self");
    if CLI_ARGS.preserve_encodings {
        let mut new_nint_ctor = codegen::Block::new("Self::Nint");
        new_nint_ctor.line("value,").line("encoding: None,");
        new_nint.push_block(new_nint_ctor);
    } else {
        new_nint.line("Self::Nint(value)");
    }
    native_impl.push_fn(new_nint);

    // serialization
    let mut ser_impl = make_serialization_impl("Int");
    let mut ser_func = make_serialization_function("serialize");
    let mut ser_block = Block::new("match self");
    if CLI_ARGS.preserve_encodings {
        ser_block
            .line(format!("Self::Uint{{ value, encoding }} => serializer.write_unsigned_integer_sz(*value, fit_sz(*value, *encoding{})),", canonical_param()))
            .line(format!("Self::Nint{{ value, encoding }} => serializer.write_negative_integer_sz(-((*value as i128) + 1), fit_sz(*value, *encoding{})),", canonical_param()));
    } else {
        ser_block
            .line("Self::Uint(x) => serializer.write_unsigned_integer(*x),")
            .line("Self::Nint(x) => serializer.write_negative_integer_sz(-((*x as i128) + 1), cbor_event::Sz::canonical(*x)),");
    }
    ser_func.push_block(ser_block);
    ser_impl.push_fn(ser_func);

    // deserialization
    let mut deser_impl = codegen::Impl::new("Int");
    deser_impl.impl_trait("Deserialize");
    let mut deser_func = make_deserialization_function("deserialize");
    let mut annotate = make_err_annotate_block("Int", "", "");
    let mut deser_match = codegen::Block::new("match raw.cbor_type()?");
    if CLI_ARGS.preserve_encodings {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => raw.unsigned_integer_sz().map(|(x, enc)| Self::Uint{ value: x, encoding: Some(enc) }).map_err(std::convert::Into::into),")
            .line("cbor_event::Type::NegativeInteger => raw.negative_integer_sz().map(|(x, enc)| Self::Nint{ value: (-1 - x) as u64, encoding: Some(enc) }).map_err(std::convert::Into::into),");
    } else {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => Ok(Self::Uint(raw.unsigned_integer()?)),")
            .line("cbor_event::Type::NegativeInteger => Ok(Self::Nint((-1 - raw.negative_integer_sz().map(|(x, _enc)| x)?) as u64)),");
    }
    deser_match.line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
    annotate.push_block(deser_match);
    deser_func.push_block(annotate);
    deser_impl.push_fn(deser_func);

    // traits
    let mut int_err = codegen::Enum::new("IntError");
    int_err.vis("pub").derive("Clone").derive("Debug");
    int_err
        .new_variant("Bounds")
        .tuple("std::num::TryFromIntError");
    int_err
        .new_variant("Parsing")
        .tuple("std::num::ParseIntError");

    let mut display = codegen::Impl::new("Int");
    let mut display_match = codegen::Block::new("match self");
    if CLI_ARGS.preserve_encodings {
        display_match
            .line("Self::Uint{ value, .. } => write!(f, \"{}\", value),")
            .line("Self::Nint{ value, .. } => write!(f, \"-{}\", value + 1),");
    } else {
        display_match
            .line("Self::Uint(x) => write!(f, \"{}\", x),")
            .line("Self::Nint(x) => write!(f, \"-{}\", x + 1),");
    }
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
    let mut try_from_else = codegen::Block::new("else");
    if CLI_ARGS.preserve_encodings {
        try_from_if.line("u64::try_from(x).map(|x| Self::Uint{ value: x, encoding: None })");
        try_from_else
            .line("u64::try_from((x + 1).abs()).map(|x| Self::Nint{ value: x, encoding: None })");
    } else {
        try_from_if.line("u64::try_from(x).map(Self::Uint)");
        try_from_else.line("u64::try_from((x + 1).abs()).map(Self::Nint)");
    }
    try_from_i128
        .impl_trait("TryFrom<i128>")
        .associate_type("Error", "std::num::TryFromIntError")
        .new_fn("try_from")
        .arg("x", "i128")
        .ret("Result<Self, Self::Error>")
        .push_block(try_from_if)
        .push_block(try_from_else);

    gen_scope
        .rust_lib()
        .push_enum(native_struct)
        .push_enum(int_err)
        .push_impl(native_impl)
        .push_impl(display)
        .push_impl(from_str)
        .push_impl(try_from_i128);
    gen_scope
        .rust_serialize()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// Gets the rustfmt path to rustfmt the generated bindings.
fn rustfmt_path<'a>() -> std::io::Result<Cow<'a, std::path::PathBuf>> {
    if let Ok(rustfmt) = std::env::var("RUSTFMT") {
        return Ok(Cow::Owned(rustfmt.into()));
    }
    #[cfg(feature = "which-rustfmt")]
    match which::which("rustfmt") {
        Ok(p) => Ok(Cow::Owned(p)),
        Err(e) => Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("{e}"),
        )),
    }
    #[cfg(not(feature = "which-rustfmt"))]
    Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        "which wasn't enabled, and no rustfmt binary specified",
    ))
}

/// Runs rustfmt on the string
pub fn rustfmt_generated_string(source: &str) -> std::io::Result<Cow<str>> {
    let mut cmd = Command::new(rustfmt_path().unwrap().as_ref());
    cmd.stdin(Stdio::piped()).stdout(Stdio::piped());

    // cmd.args(&["--config-path", path]);

    let mut child = cmd.spawn()?;
    let mut child_stdin = child.stdin.take().unwrap();
    let mut child_stdout = child.stdout.take().unwrap();

    let source = source.to_owned();

    // Write to stdin in a new thread, so that we can read from stdout on this
    // thread. This keeps the child from blocking on writing to its stdout which
    // might block us from writing to its stdin.
    let stdin_handle = ::std::thread::spawn(move || {
        let _ = child_stdin.write_all(source.as_bytes());
        source
    });

    let mut output = vec![];
    std::io::copy(&mut child_stdout, &mut output)?;

    let status = child.wait()?;
    let source = stdin_handle.join().expect(
        "The thread writing to rustfmt's stdin doesn't do \
         anything that could panic",
    );

    match String::from_utf8(output) {
        Ok(bindings) => match status.code() {
            Some(0) => Ok(Cow::Owned(bindings)),
            Some(2) => {
                println!("Rustfmt parsing errors.");
                Ok(Cow::Owned(source))
            }
            Some(3) => {
                println!("Rustfmt could not format some lines.");
                Ok(Cow::Owned(bindings))
            }
            _ => {
                println!("Rustfmt internal error.");
                Ok(Cow::Owned(source))
            }
        },
        _ => Ok(Cow::Owned(source)),
    }
}
