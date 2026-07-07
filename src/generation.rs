use crate::cli::Cli;
use codegen::{Block, TypeAlias};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::intermediate::{
    AliasIdent, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, EnumVariantData,
    FixedValue, IntermediateTypes, ModuleScope, Primitive, ROOT_SCOPE, Representation, RustField,
    RustIdent, RustRecord, RustStructCBORLen, RustStructConfig, RustStructType, RustType,
    RustTypeSerializeConfig, ToWasmBoundaryOperations, VariantIdent, escape_rust_str,
};
use crate::utils::{cbor_type_code_str, convert_to_snake_case};

/// The seed-once thin root written to each generated crate's `src/lib.rs` on the first export only
/// (rust, wasm, and json-gen all share this same content). All regenerated code lives under
/// `src/generated/**` (a subtree the tool always clobbers); this root is user-owned after its first
/// write and never overwritten (existence-only, mirroring `ManifestOp::SeedOnce`), so hand-added
/// modules/re-exports/attrs survive every regeneration.
const SEEDED_CRATE_ROOT: &str = "\
// Seeded by cddl-codegen on first export; never overwritten after that.
// All regenerated code lives in the `generated` module. Add your own
// modules/re-exports/attrs here freely (e.g. `pub mod utils;`).
mod generated;
pub use generated::*;
";

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
    /// Override regular serialization lgoic with a call to this function
    custom_serialize: Option<String>,
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
            custom_serialize: None,
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

    fn custom_serialize(mut self, func: String) -> Self {
        self.custom_serialize = Some(func);
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
        encoding_fields: &[EncodingField],
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
    /// Override regular deserialization lgoic with a call to this function
    custom_deserialize: Option<String>,
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
            custom_deserialize: None,
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

    fn custom_deserialize(mut self, func: String) -> Self {
        self.custom_deserialize = Some(func);
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

fn concat_files<P: AsRef<Path>>(paths: &Vec<P>) -> std::io::Result<String> {
    let mut buf = String::new();
    for path in paths {
        buf.push_str(
            &std::fs::read_to_string(path)
                .map_err(|e| {
                    panic!(
                        "can't read: {}. Err: {:?} | {:?}",
                        path.as_ref().to_str().unwrap_or("Path is not in unicode"),
                        e,
                        paths
                            .iter()
                            .map(|p| p.as_ref().to_str().unwrap())
                            .collect::<Vec<_>>(),
                    )
                })
                .unwrap(),
        );
    }
    Ok(buf)
}

#[derive(Debug)]
enum SerializingRustType<'a> {
    EncodingOperation(&'a CBOREncodingOperation, Box<SerializingRustType<'a>>),
    Root(&'a ConceptualRustType, Cow<'a, RustTypeSerializeConfig>),
}

trait EncodingVarIsCopy {
    fn encoding_var_is_copy(&self, types: &IntermediateTypes) -> bool;
}

impl<'a> EncodingVarIsCopy for SerializingRustType<'a> {
    fn encoding_var_is_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::EncodingOperation(CBOREncodingOperation::CBORBytes, _) => false,
            Self::EncodingOperation(CBOREncodingOperation::Tagged(_), _) => true,
            Self::Root(ty, _cfg) => ty.encoding_var_is_copy(types),
        }
    }
}

impl EncodingVarIsCopy for FixedValue {
    fn encoding_var_is_copy(&self, _types: &IntermediateTypes) -> bool {
        match self {
            // bool / null have no encoding var
            Self::Bool(_) | Self::Nint(_) | Self::Null | Self::Float(_) | Self::Uint(_) => true,
            Self::Text(_) => false,
        }
    }
}

impl EncodingVarIsCopy for ConceptualRustType {
    fn encoding_var_is_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            // these are true (refers to the length encoding! not key/value/elem encodings as those are separate)
            Self::Array(_) => true,
            Self::Map(_, _) => true,
            Self::Fixed(fv) => fv.encoding_var_is_copy(types),
            Self::Optional(ty) => SerializingRustType::from(&**ty).encoding_var_is_copy(types),
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
            Self::Rust(ident) => match types.rust_struct(ident).unwrap().variant() {
                RustStructType::CStyleEnum { variants } => {
                    variants.iter().all(|ev| match &ev.data {
                        EnumVariantData::RustType(ty) => ty.encoding_var_is_copy(types),
                        EnumVariantData::Inlined(record) => record
                            .fields
                            .iter()
                            .all(|f| f.rust_type.encoding_var_is_copy(types)),
                    })
                }
                RustStructType::RawBytesType => false,
                _ => {
                    // technically no encoding var
                    true
                }
            },
            Self::Alias(_, ty) => ty.encoding_var_is_copy(types),
        }
    }
}

impl<'a> From<&'a RustType> for SerializingRustType<'a> {
    fn from(rust_type: &'a RustType) -> Self {
        let mut root = Self::Root(&rust_type.conceptual_type, Cow::Borrowed(&rust_type.config));
        for cbor_encoding_op in rust_type.encodings.iter() {
            root = Self::EncodingOperation(cbor_encoding_op, Box::new(root));
        }
        root
    }
}

impl<'a> From<&'a ConceptualRustType> for SerializingRustType<'a> {
    fn from(conceptual_rust_type: &'a ConceptualRustType) -> Self {
        Self::Root(
            conceptual_rust_type,
            Cow::Owned(RustTypeSerializeConfig::default()),
        )
    }
}

/// Output code for deserialization. Includes meta information for better usage to prevent warnings.
#[derive(Default, Debug)]
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
    fn wrap_in_block(mut self, mut block: Block) -> Self {
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

impl From<BlocksOrLines> for DeserializationCode {
    fn from(content: BlocksOrLines) -> Self {
        Self {
            content,
            read_len_used: false,
            throws: false,
        }
    }
}

/// Context as to how to generate deserialization code.
/// formats as {before}{<deserialized value>}{after} in a line within the body param, allowing freedom e.g.:
/// * {let x = }{<value>}{;} - creation of variables
/// * {x = Some(}{<value>}{);} - variable assignment (could be nested in function call, etc, too)
/// * {}{<value>}{} - for last-expression eval in blocks
/// * etc
///
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
            (true, false) => format!("){}", self.after),
            // expected == found, nothing to be done
            (false, false) | (true, true) => self.after.to_owned(),
        }
    }
}

pub struct GenerationScope {
    rust_lib_scope: codegen::Scope,
    rust_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    rust_serialize_lib_scope: codegen::Scope,
    serialize_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    wasm_lib_scope: codegen::Scope,
    wasm_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    cbor_encodings_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    json_lines: BlocksOrLines,
    already_generated: BTreeSet<RustIdent>,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
    /// Type-parameter names for the emitted `serialize` / `deserialize` fns. Normally `"W"` / `"R"`,
    /// but if a rule camel-cases to a type named `W`/`R` (which would shadow the generic and break
    /// compilation) these fall back to the first non-colliding candidate. Computed once in
    /// `generate()` from the ident set; see `pick_generic_name`.
    serialize_generic: String,
    deserialize_generic: String,
}

impl Default for GenerationScope {
    fn default() -> Self {
        Self::new()
    }
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            rust_lib_scope: codegen::Scope::new(),
            rust_scopes: BTreeMap::new(),
            rust_serialize_lib_scope: codegen::Scope::new(),
            serialize_scopes: BTreeMap::new(),
            wasm_lib_scope: codegen::Scope::new(),
            wasm_scopes: BTreeMap::new(),
            cbor_encodings_scopes: BTreeMap::new(),
            json_lines: BlocksOrLines::default(),
            already_generated: BTreeSet::new(),
            no_deser_reasons: BTreeMap::new(),
            serialize_generic: "W".to_string(),
            deserialize_generic: "R".to_string(),
        }
    }

    /// Generates, i.e. populates the state, based on `types`.
    /// this does not create any files, call export() after.
    pub fn generate(&mut self, types: &IntermediateTypes, cli: &Cli) {
        // Pick collision-proof generic-parameter names for the emitted serialize/deserialize fns
        // BEFORE emitting anything: a rule named `w`/`r` camel-cases to a type `W`/`R` that would
        // shadow the hardcoded `fn serialize<'se, W: Write>` / `fn deserialize<R: BufRead + Seek>`
        // parameters, so we thread the chosen names through `make_{serialization,deserialization}_
        // function`. Depends only on the (deterministic) ident set, so output stays byte-identical:
        // with no collision these resolve to the defaults `"W"` / `"R"` and nothing churns.
        let defined_idents = types.defined_rust_idents();
        self.serialize_generic = pick_generic_name(&defined_idents, "W", "Ser");
        self.deserialize_generic = pick_generic_name(&defined_idents, "R", "De");

        // Type aliases
        for (alias_ident, alias_info) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias_ident {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if alias_info.gen_rust_alias {
                    self.rust(types, ident).push_type_alias(
                        TypeAlias::new(
                            ident,
                            alias_info.base_type.for_rust_member(types, false, cli),
                        )
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
                            // float_literal, not Display: a whole-valued f64 would render as an
                            // integer literal in the f64-returning wasm constant fn (E0308).
                            FixedValue::Float(f) => ("f64", float_fixed_literal(*f)),
                            FixedValue::Text(s) => {
                                ("String", format!("\"{}\".to_owned()", escape_rust_str(s)))
                            }
                        };
                        self.wasm(types, ident)
                            .new_fn(convert_to_snake_case(ident.as_ref()))
                            .attr("wasm_bindgen")
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        // A passthrough alias to a named collection (`ptm = mp`) is a transparent
                        // `pub type` in rust but a wrapper struct in wasm; point the wasm alias at that
                        // wrapper rather than `for_wasm_member`'s inline-only `MapU64To…` name. Only when
                        // the target is *not* directly-wasm-exposable, though: an exposable named array
                        // (`[* uint]`) also has a `Nums` wrapper struct, but the boundary code treats it
                        // transparently as `Vec<u64>`, so aliasing to the wrapper would desync (E0308).
                        // Maps are never directly exposable, so this covers `passthrumap` while leaving
                        // `passthru` (exposable arrays) on the transparent `for_wasm_member` path.
                        let wasm_target = alias_info
                            .wasm_alias_target
                            .as_ref()
                            .filter(|target| {
                                types.has_wasm_wrapper(target)
                                    && !alias_info
                                        .base_type
                                        .conceptual_type
                                        .directly_wasm_exposable(types)
                            })
                            .map(|target| target.to_string())
                            .unwrap_or_else(|| alias_info.base_type.for_wasm_member(types));
                        self.wasm(types, ident)
                            .push_type_alias(TypeAlias::new(ident, wasm_target).vis("pub").clone());
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

            // Which named table rule(s) declare each structural wasm-map shape. Keyed by the
            // structural name string (`name_for_wasm_map`) — that string IS the shape identity the
            // JS-class-name collision is about. Built up front over ALL table rules so the result is
            // iteration-order-independent: it depends only on the SET of table rules, never on which
            // struct the emit loop visits first.
            let mut table_shape_owners: BTreeMap<String, Vec<RustIdent>> = BTreeMap::new();
            for (owner_ident, owner_struct) in types.rust_structs() {
                if let RustStructType::Table { domain, range } = owner_struct.variant() {
                    let structural =
                        ConceptualRustType::name_for_wasm_map(domain, range).to_string();
                    table_shape_owners
                        .entry(structural)
                        .or_default()
                        .push(owner_ident.clone());
                }
            }
            // Shapes owned by EXACTLY ONE named rule: their embedded/resolved uses share the
            // rule-named class (a real `#[wasm_bindgen]` class under the CDDL identifier), and the
            // structural `MapKToV` name becomes a `pub type` alias to it. Same-shape rule PAIRS (2+
            // owners) are absent here — they keep the structural fallback for embedded uses while
            // each named rule still gets its own class.
            let table_shape_sole_owner: BTreeMap<String, RustIdent> = table_shape_owners
                .into_iter()
                .filter_map(|(structural, mut owners)| {
                    (owners.len() == 1).then(|| (structural, owners.pop().unwrap()))
                })
                .collect();

            let mut wasm_wrappers_generated = BTreeSet::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                assert_eq!(rust_ident, rust_struct.ident());
                if cli.wasm {
                    rust_struct.visit_types_excluding(
                        types,
                        &mut |ty| match ty {
                            ConceptualRustType::Array(elem) => {
                                if !ty.directly_wasm_exposable(types) {
                                    let array_ident = elem.name_as_wasm_array(types);
                                    if wasm_wrappers_generated.insert(array_ident.clone()) {
                                        self.generate_array_type(
                                            types,
                                            *elem.clone(),
                                            &RustIdent::new(CDDLIdent::new(array_ident)),
                                            cli,
                                        );
                                    }
                                }
                            }
                            ConceptualRustType::Map(k, v) => {
                                let map_ident = ConceptualRustType::name_for_wasm_map(k, v);
                                match table_shape_sole_owner.get(&map_ident.to_string()) {
                                    // A single named rule owns this shape: this embedded/resolved use
                                    // shares that rule-named class (JS-visible under the CDDL
                                    // identifier) rather than minting an anonymous structural class.
                                    Some(owner) => mint_sole_owner_table(
                                        self,
                                        types,
                                        owner,
                                        &map_ident,
                                        &mut wasm_wrappers_generated,
                                        cli,
                                    ),
                                    // Anonymous-only shape (or a same-shape rule pair): mint the
                                    // structural class, whose inner is the raw map (not a rust rule).
                                    None => {
                                        if wasm_wrappers_generated.insert(map_ident.to_string()) {
                                            codegen_table_type(
                                                self,
                                                types,
                                                &map_ident,
                                                *k.clone(),
                                                *v.clone(),
                                                None,
                                                false,
                                                cli,
                                            );
                                        }
                                    }
                                }
                                if !ConceptualRustType::Array(Box::new(*k.clone()))
                                    .directly_wasm_exposable(types)
                                {
                                    let keys_ident = k.name_as_wasm_array(types);
                                    if wasm_wrappers_generated.insert(keys_ident.clone()) {
                                        self.generate_array_type(
                                            types,
                                            *k.clone(),
                                            &RustIdent::new(CDDLIdent::new(keys_ident)),
                                            cli,
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
                        codegen_struct(
                            self,
                            types,
                            rust_ident,
                            rust_struct.tag(),
                            record,
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::Table { domain, range } => {
                        if cli.wasm {
                            let map_ident = ConceptualRustType::name_for_wasm_map(domain, range);
                            if table_shape_sole_owner.get(&map_ident.to_string())
                                == Some(rust_ident)
                            {
                                // Sole owner of this shape: emit the real JS class under the rule name
                                // plus the structural alias. Idempotent — the visit arm may have
                                // minted it already for an embedded/resolved use; either order
                                // converges to identical output.
                                mint_sole_owner_table(
                                    self,
                                    types,
                                    rust_ident,
                                    &map_ident,
                                    &mut wasm_wrappers_generated,
                                    cli,
                                );
                            } else if wasm_wrappers_generated.insert(rust_ident.to_string()) {
                                // Shared shape: a same-shape rule PAIR, or a shape also reached by
                                // anonymous/embedded uses. Every named rule STILL surfaces as its own
                                // real JS class under its identifier (unconditionally, independent of
                                // whether a structural twin was minted first); the structural `MapKToV`
                                // class, where referenced, is minted by the visit arm above.
                                codegen_table_type(
                                    self,
                                    types,
                                    rust_ident,
                                    domain.clone(),
                                    range.clone(),
                                    rust_struct.tag(),
                                    true,
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), ConceptualRustType::name_for_rust_map(domain, range, false)));
                    }
                    RustStructType::Array { element_type, .. } => {
                        if cli.wasm {
                            self.generate_array_type(types, element_type.clone(), rust_ident, cli);
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
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::GroupChoice { variants, rep } => codegen_group_choices(
                        self,
                        types,
                        rust_ident,
                        variants,
                        *rep,
                        rust_struct.tag(),
                        rust_struct.config(),
                        cli,
                    ),
                    RustStructType::Wrapper {
                        wrapped,
                        min_max,
                        float_min_max,
                    } => match rust_struct.tag() {
                        Some(tag) => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            &wrapped.clone().tag(tag),
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                        None => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            wrapped,
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                    },
                    RustStructType::Extern => {
                        #[allow(clippy::single_match)]
                        match rust_ident.to_string().as_ref() {
                            "Int" if types.is_referenced(rust_ident) => {
                                generate_int(self, types, cli)
                            }
                            _ => (), /* user-specified external types */
                        }
                    }
                    RustStructType::CStyleEnum { variants } => {
                        generate_c_style_enum(
                            self,
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::RawBytesType => {
                        // nothing to do, user specified
                    }
                }
            }
        }

        // JSON export crate
        if cli.json_schema_export {
            self.json_lines
                .line("let schema_path = std::path::Path::new(&\"schemas\");");
            let mut path_exists = Block::new("if !schema_path.exists()");
            path_exists.line("std::fs::create_dir(schema_path).unwrap();");
            self.json_lines.push_block(path_exists);
            let mut main_lines_by_file: BTreeMap<ModuleScope, Vec<String>> = BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                let is_typedef = matches!(
                    rust_struct.variant(),
                    RustStructType::Array { .. } | RustStructType::Table { .. }
                );
                // The is_referenced check is for things like Int which are included by default
                // in order for the CDDL to parse but might not be used.
                // However, we need to export other root types from the user's spec
                if !is_typedef && (rust_ident.as_ref() != "Int" || types.is_referenced(rust_ident))
                {
                    main_lines_by_file
                        .entry(types.scope(rust_ident).clone())
                        .or_default()
                        .push(format!(
                            "gen_json_schema!({});",
                            rust_crate_struct_from_wasm(types, rust_ident, cli)
                        ));
                }
            }
            let multiple_files = main_lines_by_file.len() > 1;
            for (scope_name, lines) in main_lines_by_file {
                if multiple_files {
                    self.json_lines.line(&format!("// {scope_name}"));
                }
                for line in lines {
                    self.json_lines.line(&line);
                }
            }
        }

        // imports / module declarations
        // this is done at the end so we already know all information about output code

        // rust
        self.rust_lib()
            .raw("#![allow(clippy::too_many_arguments)]\n");
        let codegen_comment = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n";
        for content in self.rust_scopes.values_mut() {
            content.raw(codegen_comment);
        }
        for content in self.cbor_encodings_scopes.values_mut() {
            content.raw(codegen_comment);
        }
        for content in self.serialize_scopes.values_mut() {
            content.raw(codegen_comment);
        }
        for content in self.wasm_scopes.values_mut() {
            content.raw(codegen_comment);
        }

        // declare modules (root lib specific)
        if cli.export_static_files() {
            self.rust_lib().raw("pub mod error;");
            if cli.preserve_encodings {
                self.rust_lib().raw("pub mod ordered_hash_map;");
            }
        }
        if cli.preserve_encodings {
            self.rust_lib().raw("extern crate derivative;");
        }
        let scope_names = self
            .rust_scopes
            .keys()
            .filter(|scope| **scope != *ROOT_SCOPE)
            .cloned()
            .collect::<Vec<_>>();
        for scope in scope_names
            .iter()
            .filter_map(|s| {
                if s.export() {
                    s.components().first()
                } else {
                    None
                }
            })
            .collect::<BTreeSet<_>>()
        {
            self.rust_lib().raw(format!("pub mod {scope};"));
        }

        // declare common modules in each module (struct files). cbor_encodings is declared only
        // where a cbor_encodings.rs is actually emitted (mirror the condition in generated_files):
        // a scope with no encoding structs (e.g. a root of only c-style enums) emits no such file,
        // and declaring the module anyway yields a `pub mod cbor_encodings;` with no backing file
        // (E0583, uncompilable).
        for (scope, content) in self.rust_scopes.iter_mut() {
            content.raw("pub mod serialization;");
            if cli.preserve_encodings
                && scope.export()
                && self.cbor_encodings_scopes.contains_key(scope)
            {
                content.raw("pub mod cbor_encodings;");
            }
        }

        // Extern-type re-export glue. Generated code refers to each in-crate extern type by its bare
        // ident within the scope that declared it (and cross-scope as `crate::generated::<scope>::Name`;
        // the serializer sees it via `use super::*;`). Under the thin-root split the user cannot inject
        // that definition into `generated/**` (clobbered every run), so the contract is to DEFINE the
        // extern in a hand-written module and RE-EXPORT it at the crate root (`pub use utils::Name;` in
        // the thin `lib.rs`). Re-export it from crate root INTO the declaring scope's generated module so
        // every such bare/`use super::*` reference resolves; the explicit `pub use crate::Name;` binds to
        // the user's definition and beats the `pub use generated::*;` glob cycle. Emitted unconditionally
        // — under `--common-import-override` the extern is still crate-local. Skipped:
        //   - the built-in `Int` extern (the tool generates its definition when referenced),
        //   - generic-extern instances that already emit a `pub type` alias in this module (the base
        //     generic extern carries the glue instead — re-exporting the aliased name would collide),
        //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes; those resolve through their dep
        //     crate already — `ModuleScope::export()` is the discriminator).
        let rust_aliased: BTreeSet<&RustIdent> = types
            .type_aliases()
            .iter()
            .filter_map(|(alias_ident, info)| match alias_ident {
                AliasIdent::Rust(ident) if info.gen_rust_alias => Some(ident),
                _ => None,
            })
            .collect();
        let mut externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> = BTreeMap::new();
        for (rust_ident, rust_struct) in types.rust_structs() {
            if matches!(rust_struct.variant(), RustStructType::Extern)
                && rust_ident.as_ref() != "Int"
                && !rust_aliased.contains(rust_ident)
            {
                let scope = types.scope(rust_ident);
                if scope.export() {
                    externs_by_scope
                        .entry(scope.clone())
                        .or_default()
                        .insert(rust_ident.clone());
                }
            }
        }
        for (scope, idents) in &externs_by_scope {
            let content = self.rust_scopes.entry(scope.clone()).or_default();
            for ident in idents {
                content.raw(format!("pub use crate::{ident};"));
            }
        }

        // general common imports (struct files)
        for content in self.rust_scopes.values_mut() {
            // needed if there's any params that can fail
            content
                .push_import("std::convert", "TryFrom", None)
                .push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            // in case we store these in enums we're just going to dump them in everywhere
            if cli.preserve_encodings {
                content
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    );
            }
        }

        // cbor_encodings imports
        if cli.preserve_encodings {
            // Issue (general - not just here): https://github.com/dcSpark/cddl-codegen/issues/139
            for content in self.cbor_encodings_scopes.values_mut() {
                content
                    // encoding structs can reference GENERATED types (a table keyed by a
                    // type-choice enum stores `BTreeMap<KeyEnum, StringEncoding>`), so like
                    // serialization.rs this needs the scope module's items — `super::*` also
                    // covers cross-scope keys, since a child glob re-imports the parent struct
                    // file's `use` bindings (the scope_references imports pushed above)
                    .push_import("super", "*", None)
                    .push_import("std::collections", "BTreeMap", None)
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    );
            }
        }

        // import encoding structs (struct files)
        if cli.preserve_encodings {
            for (rust_ident, rust_struct) in types.rust_structs() {
                if match rust_struct.variant() {
                    RustStructType::Record(_) => true,
                    RustStructType::Wrapper { wrapped, .. } => {
                        !encoding_fields(types, rust_ident.as_ref(), wrapped, true, cli).is_empty()
                    }
                    _ => false,
                } {
                    // ALL records have an encoding struct since at minimum they contian
                    // the array or map encoding details so no need to check fields
                    self.rust(types, rust_ident).push_import(
                        "cbor_encodings",
                        format!("{rust_ident}Encoding"),
                        None,
                    );
                }
            }
        }

        fn add_imports_from_scope_refs(
            scope: &ModuleScope,
            content: &mut codegen::Scope,
            imports: &BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            // The crate-root prefix for cross-scope references within the SAME crate: both the rust
            // and wasm crates nest their generated tree one level (`crate::generated`). Root-scope
            // items and non-exported scopes are still reached relatively.
            crate_prefix: &str,
        ) {
            // might not exist if we don't use stuff from other scopes
            if let Some(scope_imports) = imports.get(scope) {
                for (import_scope, idents) in scope_imports.iter() {
                    let import_scope = if *import_scope == *ROOT_SCOPE {
                        Cow::from(crate_prefix.to_owned())
                    } else if *scope == *ROOT_SCOPE || !import_scope.export() {
                        Cow::from(import_scope.to_string())
                    } else {
                        Cow::from(format!("{crate_prefix}::{import_scope}"))
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
        // imports for generated structs from other files (struct files)
        let rust_imports = types.scope_references(false);
        for (scope, content) in self.rust_scopes.iter_mut() {
            add_imports_from_scope_refs(scope, content, &rust_imports, "crate::generated");
            // TODO: we blindly add these two map imports. Ideally we would only do it when needed
            // but the code to figure that out would be potentially complex.
            // Issue (general - not just here): https://github.com/dcSpark/cddl-codegen/issues/139
            content.push_import("std::collections", "BTreeMap", None);
            if cli.preserve_encodings {
                content.push_import(
                    format!("{}::ordered_hash_map", cli.common_import_rust()),
                    "OrderedHashMap",
                    None,
                );
            }
        }

        // serialization
        // The imports every generated serialization.rs needs regardless of scope — the static
        // prelude and all generated impls reference these. Shared by the per-scope loop and the
        // lib-scope fallback below so the set can't drift between the two.
        let push_base_serialize_imports = |scope: &mut codegen::Scope| {
            scope
                .push_import("super", "*", None)
                .push_import("std::io", "BufRead", None)
                .push_import("std::io", "Seek", None)
                .push_import("std::io", "SeekFrom", None)
                .push_import("std::io", "Write", None)
                .push_import("cbor_event::de", "Deserializer", None)
                .push_import("cbor_event::se", "Serializer", None)
                .push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            if !(cli.preserve_encodings && cli.canonical_form) {
                scope.push_import("cbor_event::se", "Serialize", None);
            }
        };
        for (scope, content) in self.serialize_scopes.iter_mut() {
            push_base_serialize_imports(content);
            if let Some(common_import) = cli.common_import_override.as_ref() {
                content.push_import(format!("{}::serialization", common_import), "*", None);
            }
            // Only import cbor_encodings where a cbor_encodings.rs is actually emitted for this
            // scope (same condition as its `pub mod` declaration / generated_files): a scope with
            // serialization but no encoding structs (e.g. a group/type choice) emits no such file,
            // so importing it would be an unresolved import (E0432).
            if cli.preserve_encodings && self.cbor_encodings_scopes.contains_key(scope) {
                content.push_import("super::cbor_encodings", "*", None);
            }
            if *scope != *ROOT_SCOPE {
                content.push_import(
                    format!("{}::serialization", cli.common_import_rust()),
                    "*",
                    None,
                );
            }
        }

        // The static serialization prelude prepended to the root serialization.rs (when we own the
        // static files) references Serializer/Deserializer/BufRead/DeserializeError/etc. Those
        // imports are added to the ROOT_SCOPE serialize scope by the loop above — but a spec whose
        // root has no per-type serialization (e.g. only c-style enums) produces no ROOT_SCOPE entry,
        // leaving the prelude (and any rust_serialize_lib impls) without imports and the crate
        // uncompilable. Add the base imports to the lib serialize scope in that case. (No
        // cbor_encodings/non-root imports: no ROOT_SCOPE entry means no root struct, so no root
        // encoding struct and nothing cross-module to reach.)
        if cli.export_static_files() && !self.serialize_scopes.contains_key(&*ROOT_SCOPE) {
            push_base_serialize_imports(self.rust_serialize_lib());
        }

        // declare submodules
        // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
        // for these modules when they only exist to support modules nested deeper
        declare_modules(&mut self.rust_scopes, &scope_names);

        // wasm
        if cli.wasm {
            self
            .wasm_lib()
            .raw("#![allow(clippy::len_without_is_empty, clippy::too_many_arguments, clippy::new_without_default)]");
            // wasm module declarations
            let wasm_scope_names = self
                .wasm_scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE)
                .cloned()
                .collect::<Vec<_>>();
            for scope in wasm_scope_names
                .iter()
                .filter_map(|s| {
                    if s.export() {
                        s.components().first()
                    } else {
                        None
                    }
                })
                .collect::<BTreeSet<_>>()
            {
                self.wasm_lib().raw(format!("pub mod {scope};"));
            }
            // wasm imports
            let wasm_imports = types.scope_references(true);
            for (scope, content) in self.wasm_scopes.iter_mut() {
                // imports from other struct modules; the wasm generated tree nests one level under
                // `crate::generated` (same as the rust crate)
                add_imports_from_scope_refs(scope, content, &wasm_imports, "crate::generated");
                // common imports
                content
                    .push_import("wasm_bindgen::prelude", "wasm_bindgen", None)
                    .push_import("wasm_bindgen::prelude", "JsError", None);
                if cli.json_serde_derives && cli.wasm_cbor_json_api_macro.is_none() {
                    content.push_import("wasm_bindgen::prelude", "JsValue", None);
                }
                if cli.preserve_encodings {
                    content.push_import(
                        format!("{}::ordered_hash_map", cli.common_import_wasm()),
                        "OrderedHashMap",
                        None,
                    );
                } else {
                    content.push_import("std::collections", "BTreeMap", None);
                }
                // external macros
                if let Some(cbor_json_macro) = &cli.wasm_cbor_json_api_macro
                    && let Some((path, m)) = cbor_json_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(conversion_macro) = &cli.wasm_conversions_macro
                    && let Some((path, m)) = conversion_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(list_macro) = &cli.wasm_list_macro
                    && let Some((path, m)) = list_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
            }
            // Extern-type re-export glue (wasm crate). The wasm generated code names each in-crate
            // extern by its bare WRAPPER ident within the declaring scope (`req: ExternalFoo`, and via
            // `use super::*;` in nested modules), exactly as the rust crate names the native type — same
            // E0433 shape under the thin-root split, since a crate-root name isn't visible inside
            // `mod generated`. The contract mirrors rust: DEFINE the wasm wrapper in a hand-written
            // wasm-crate module and RE-EXPORT it at the wasm crate root (`pub use utils::Name;`); the tool
            // re-exports it from crate root INTO the declaring scope's generated module so every such
            // reference resolves against the user's wrapper. Skipped:
            //   - the built-in `Int` extern (the tool generates its own wasm wrapper when referenced, so
            //     `pub use crate::Int;` would collide),
            //   - generic-extern instances that already emit a wasm `pub type` alias here (`gen_wasm_alias`
            //     — the wrapper the alias points at carries the glue instead),
            //   - generic-extern BASES (`Foo` of `Foo<Bar>`): a plain `Extern` rust struct, but wasm never
            //     names it (wasm-bindgen has no generics; the instance collapses to the argument wrapper),
            //     so there is no wasm-crate-root definition to re-export — emitting glue would be an
            //     unresolved import. The rust side keeps the base because its `pub type` alias names it.
            //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes) resolve through their dep crate via
            //     `common_import_wasm()` already — `ModuleScope::export()` is the discriminator.
            let wasm_aliased: BTreeSet<&RustIdent> = types
                .type_aliases()
                .iter()
                .filter_map(|(alias_ident, info)| match alias_ident {
                    AliasIdent::Rust(ident) if info.gen_wasm_alias => Some(ident),
                    _ => None,
                })
                .collect();
            let generic_bases = types.generic_instance_bases();
            let mut wasm_externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> =
                BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                if matches!(rust_struct.variant(), RustStructType::Extern)
                    && rust_ident.as_ref() != "Int"
                    && !wasm_aliased.contains(rust_ident)
                    && !generic_bases.contains(rust_ident)
                {
                    let scope = types.scope(rust_ident);
                    if scope.export() {
                        wasm_externs_by_scope
                            .entry(scope.clone())
                            .or_default()
                            .insert(rust_ident.clone());
                    }
                }
            }
            for (scope, idents) in &wasm_externs_by_scope {
                let content = self.wasm_scopes.entry(scope.clone()).or_default();
                for ident in idents {
                    content.raw(format!("pub use crate::{ident};"));
                }
            }
            // declare submodules
            // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
            // for these modules when they only exist to support modules nested deeper
            declare_modules(&mut self.wasm_scopes, &wasm_scope_names);
        }

        // optional generated-test module (reject + round-trip halves; off by default, so it
        // doesn't touch the snapshot suite)
        if cli.emit_tests
            && let Some(test_mod) = crate::emit_tests::emit_generated_tests(types, cli)
        {
            self.rust_lib().raw(&test_mod);
        }
        // the wasm-crate counterpart: same MintValue derivation, rendered through the wrapper API +
        // the cddl_lib rust twin (cross-crate byte differential). `#[cfg(test)]` so it's inert for
        // build/check/wasm-pack — only a `cargo test` of the wasm crate compiles and runs it.
        if cli.wasm
            && cli.emit_tests
            && let Some(test_mod) = crate::emit_tests_wasm::emit_generated_wasm_tests(types, cli)
        {
            self.wasm_lib().raw(&test_mod);
        }
    }

    /// Exports all already-generated state to the provided directory.
    /// Call generate() first to populate the generation state.
    pub fn export(
        &self,
        types: &IntermediateTypes,
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<()> {
        // check it exists here to get clearer error message
        assert!(std::path::Path::exists(&cli.static_dir));

        // package.json / scripts
        let rust_dir = if cli.package_json {
            if cli.json_schema_export {
                std::fs::create_dir_all(cli.output.join("scripts"))?;
                std::fs::copy(
                    cli.static_dir.join("run-json2ts.js"),
                    cli.output.join("scripts/run-json2ts.js"),
                )?;
                std::fs::copy(
                    cli.static_dir.join("json-ts-types.js"),
                    cli.output.join("scripts/json-ts-types.js"),
                )?;
                std::fs::copy(
                    cli.static_dir.join("package_json_schemas.json"),
                    cli.output.join("package.json"),
                )?;
            } else {
                std::fs::copy(
                    cli.static_dir.join("package.json"),
                    cli.output.join("package.json"),
                )?;
            }
            cli.output.join("rust")
        } else {
            cli.output.clone()
        };

        // All generated files come from the single producer the snapshot tests also use, so the
        // shipped output and the tested output can't drift.
        let mut files = self.generated_files(types, export_raw_bytes_encoding_trait, cli)?;

        // `generated_files` produces serialization.rs generated-only; the shipped root one has the
        // static serialization prelude prepended and is rustfmt'd together (exactly as before).
        if cli.export_static_files() {
            let mut merged = codegen::Scope::new();
            merged.raw(Self::serialization_prelude(
                export_raw_bytes_encoding_trait,
                cli,
            )?);
            merged.append(&self.rust_serialize_lib_scope);
            for (scope, content) in &self.serialize_scopes {
                if *scope == *ROOT_SCOPE {
                    merged.append(&content.clone());
                }
            }
            files.insert(
                "rust/src/generated/serialization.rs".to_owned(),
                rustfmt_generated_string(&merged.to_string())?.into_owned(),
            );
        }

        // Manifests merge into whatever is already on disk (the declarative changeset) rather than
        // clobbering, so user edits to keys the tool doesn't own survive regeneration. This is the
        // ONLY place output depends on prior directory contents, and only as the changeset contract
        // allows: keys no op mentions pass through, `SeedOnce` checks existence. An unparseable
        // existing manifest is a hard error naming the file (see `cargo_manifest::apply`) — never a
        // silent clobber. `generated_files` above produced these same manifests against an empty
        // document; here we re-derive them against the on-disk file before the common write loop.
        let mut manifest_ops = vec![(
            "rust/Cargo.toml",
            crate::cargo_manifest::ops_for_rust(types, export_raw_bytes_encoding_trait, cli)?,
        )];
        if cli.wasm {
            manifest_ops.push(("wasm/Cargo.toml", crate::cargo_manifest::ops_for_wasm(cli)?));
        }
        if cli.json_schema_export {
            manifest_ops.push((
                "wasm/json-gen/Cargo.toml",
                crate::cargo_manifest::ops_for_json_gen(cli)?,
            ));
        }
        for (rel_path, ops) in &manifest_ops {
            if files.contains_key(*rel_path) {
                let existing = std::fs::read_to_string(rust_dir.join(rel_path)).ok();
                let merged = crate::cargo_manifest::apply(ops, existing.as_deref(), rel_path)
                    .map_err(std::io::Error::other)?;
                files.insert((*rel_path).to_owned(), merged);
            }
        }

        for (rel_path, content) in &files {
            let path = rust_dir.join(rel_path);
            // Seed-once thin roots: each generated crate's root `lib.rs` (rust, wasm, json-gen) is
            // written only if absent (existence check only — the same bounded exception the manifest
            // changeset carves out of the no-prior-output invariant). Everything else under
            // `generated/**` clobbers as always.
            if matches!(
                rel_path.as_str(),
                "rust/src/lib.rs" | "wasm/src/lib.rs" | "wasm/json-gen/src/lib.rs"
            ) && path.exists()
            {
                // A root that predates the thin-root split still carries generated type definitions
                // interleaved with hand wiring; under seed-once the tool leaves it untouched, so the
                // now-under-`generated/**` types it duplicates produce loud compile errors. Detect
                // that shape (no `mod generated;`) and name the one-time migration on stderr — a
                // diagnostic only, so the written bytes (and the no-prior-output invariant) are
                // unchanged. Reading the file here is the same bounded existence-adjacent peek the
                // seed-once check already makes; it never feeds back into what is generated.
                if let Ok(existing) = std::fs::read_to_string(&path)
                    && !existing.contains("mod generated")
                {
                    eprintln!(
                        "warning: {rel_path} predates the thin-root layout (no `mod generated;`). \
                         Generated code now lives under `src/generated/**` and this root is \
                         seed-once (never overwritten), so any generated items still in it will \
                         collide with the regenerated subtree. One-time migration: delete the \
                         generated items from {rel_path}, keep your hand wiring, and add \
                         `mod generated;` and `pub use generated::*;`. See the \"Migrating from \
                         pre-split layouts\" section of docs/output_format."
                    );
                }
                continue;
            }
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(path, content)?;
        }

        // static files copied/assembled verbatim (only when we own the common types)
        if cli.export_static_files() {
            // error.rs
            std::fs::copy(
                cli.static_dir.join("error.rs"),
                rust_dir.join("rust/src/generated/error.rs"),
            )?;

            // ordered_hash_map.rs
            if cli.preserve_encodings {
                let mut ordered_hash_map_rs =
                    std::fs::read_to_string(cli.static_dir.join("ordered_hash_map.rs"))?;
                if cli.json_serde_derives {
                    ordered_hash_map_rs.push_str(&std::fs::read_to_string(
                        cli.static_dir.join("ordered_hash_map_json.rs"),
                    )?);
                }
                if cli.json_schema_export {
                    ordered_hash_map_rs.push_str(&std::fs::read_to_string(
                        cli.static_dir.join("ordered_hash_map_schemars.rs"),
                    )?);
                }
                std::fs::write(
                    rust_dir.join("rust/src/generated/ordered_hash_map.rs"),
                    rustfmt_generated_string(&ordered_hash_map_rs)?.as_ref(),
                )?;
            }
        }

        Ok(())
    }

    /// Shared scope-merge producer used by both [`Self::export`] and [`Self::generated_files`]:
    /// merges the root scope into `merged_scope`, emits each non-root module scope as its own
    /// file, and inserts the (rustfmt'd) results into `out` keyed by `<dir>/.../<name>`.
    fn merge_scopes_to_strings(
        out: &mut BTreeMap<String, String>,
        dir: &str,
        mut merged_scope: codegen::Scope,
        other_scopes: &BTreeMap<ModuleScope, codegen::Scope>,
        root_name: &str,
        inner_name: &str,
    ) -> std::io::Result<()> {
        for (scope, content) in other_scopes {
            if *scope == *ROOT_SCOPE {
                merged_scope.append(&content.clone());
            } else if scope.export() {
                let path = format!("{dir}/{}/{inner_name}", scope.components().join("/"));
                out.insert(
                    path,
                    rustfmt_generated_string(&content.to_string())?.into_owned(),
                );
            }
        }
        out.insert(
            format!("{dir}/{root_name}"),
            rustfmt_generated_string(&merged_scope.to_string())?.into_owned(),
        );
        Ok(())
    }

    /// The static serialization runtime prelude (concatenated from `static/serialization*.rs`)
    /// that `export` prepends to the root serialization.rs. Exposed so it can be snapshotted on
    /// its own (it ships verbatim but varies by `--preserve-encodings`/`--canonical-form`).
    pub(crate) fn serialization_prelude(
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<String> {
        let mut serialize_paths = vec![cli.static_dir.join("serialization.rs")];
        if cli.preserve_encodings {
            serialize_paths.push(cli.static_dir.join("serialization_preserve.rs"));
            if cli.canonical_form {
                serialize_paths.push(
                    cli.static_dir
                        .join("serialization_preserve_force_canonical.rs"),
                );
            } else {
                serialize_paths.push(
                    cli.static_dir
                        .join("serialization_preserve_non_force_canonical.rs"),
                );
                serialize_paths.push(cli.static_dir.join("serialization_non_force_canonical.rs"));
            }
        } else {
            serialize_paths.push(cli.static_dir.join("serialization_non_preserve.rs"));
            serialize_paths.push(cli.static_dir.join("serialization_non_force_canonical.rs"));
        }
        if export_raw_bytes_encoding_trait {
            serialize_paths.push(cli.static_dir.join("raw_bytes_encoding.rs"));
        }
        // Opt-in recursion depth guard runtime (the `DepthGuard` RAII type + thread-local counter).
        // Conditioned like the preserve-encodings runtime so crates generated without the flag carry
        // no dead runtime code. The `DepthLimitExceeded` failure variant it constructs lives in the
        // verbatim-copied error.rs (a monolithic pub enum a file-concat can't conditionally extend;
        // a pub variant is not dead code), so only this function/thread-local piece is gated here.
        if cli.deserialize_depth_limit.is_some() {
            serialize_paths.push(cli.static_dir.join("serialization_depth_guard.rs"));
        }
        concat_files(&serialize_paths)
    }

    /// Single producer for every generated source file (post-rustfmt), keyed by path relative to
    /// the crate root (e.g. "rust/src/lib.rs"). Used by BOTH [`Self::export`] (which writes these
    /// to disk, after prepending the static serialization prelude to the root serialization.rs)
    /// and the snapshot tests — so the shipped path and the tested path can't drift. The
    /// serialization.rs here is generated-only; the static prelude and verbatim-copied files
    /// (error.rs, ordered_hash_map.rs, package.json, scripts) are handled directly by `export`.
    pub(crate) fn generated_files(
        &self,
        types: &IntermediateTypes,
        export_raw_bytes_encoding_trait: bool,
        cli: &Cli,
    ) -> std::io::Result<BTreeMap<String, String>> {
        let mut out = BTreeMap::new();

        // rust generated/mod.rs (merged ROOT_SCOPE content + module decls + inner crate attrs) /
        // generated/{module}/mod.rs. The tool-owned generated tree lives under `generated/`; the
        // crate root `lib.rs` is a seed-once thin root (added below) that the tool never clobbers.
        Self::merge_scopes_to_strings(
            &mut out,
            "rust/src/generated",
            self.rust_lib_scope.clone(),
            &self.rust_scopes,
            "mod.rs",
            "mod.rs",
        )?;

        // The seed-once thin root: written to `rust/src/lib.rs` only if absent (existence-only,
        // mirroring `ManifestOp::SeedOnce`). Included in the producer so clean runs / snapshots carry
        // it, but `export`'s write loop skips it when the file already exists so user edits survive.
        out.insert(
            "rust/src/lib.rs".to_owned(),
            rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
        );

        // serialization.rs (generated impls only; export prepends the static prelude to the root)
        let mut serialize_scope = codegen::Scope::new();
        serialize_scope.append(&self.rust_serialize_lib_scope);
        Self::merge_scopes_to_strings(
            &mut out,
            "rust/src/generated",
            serialize_scope,
            &self.serialize_scopes,
            "serialization.rs",
            "serialization.rs",
        )?;

        // cbor_encodings.rs / {module}/cbor_encodings.rs
        if cli.preserve_encodings {
            for (scope, contents) in self.cbor_encodings_scopes.iter() {
                if scope.export() {
                    let path = if *scope == *ROOT_SCOPE {
                        "rust/src/generated/cbor_encodings.rs".to_owned()
                    } else {
                        format!(
                            "rust/src/generated/{}/cbor_encodings.rs",
                            scope.components().join("/")
                        )
                    };
                    out.insert(
                        path,
                        rustfmt_generated_string(&contents.to_string())?.into_owned(),
                    );
                }
            }
        }

        // rust Cargo.toml — declarative changeset applied to an empty document (pure, so the
        // snapshot tests keep consuming the same producer). `export` re-applies the same ops onto
        // any on-disk manifest so user edits survive.
        out.insert(
            "rust/Cargo.toml".to_owned(),
            crate::cargo_manifest::apply(
                &crate::cargo_manifest::ops_for_rust(types, export_raw_bytes_encoding_trait, cli)?,
                None,
                "rust/Cargo.toml",
            )
            .map_err(std::io::Error::other)?,
        );

        // wasm crate
        if cli.wasm {
            // Same split as the rust crate: the tool-owned generated tree lives under
            // `wasm/src/generated/` (root scope + inner crate attrs in `mod.rs`), and the crate root
            // `wasm/src/lib.rs` is a seed-once thin root (added below) the tool never clobbers.
            Self::merge_scopes_to_strings(
                &mut out,
                "wasm/src/generated",
                self.wasm_lib_scope.clone(),
                &self.wasm_scopes,
                "mod.rs",
                "mod.rs",
            )?;
            out.insert(
                "wasm/src/lib.rs".to_owned(),
                rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
            );
            out.insert(
                "wasm/Cargo.toml".to_owned(),
                crate::cargo_manifest::apply(
                    &crate::cargo_manifest::ops_for_wasm(cli)?,
                    None,
                    "wasm/Cargo.toml",
                )
                .map_err(std::io::Error::other)?,
            );
        }

        // json-gen crate for exporting JSON schemas
        if cli.json_schema_export {
            out.insert(
                "wasm/json-gen/Cargo.toml".to_owned(),
                crate::cargo_manifest::apply(
                    &crate::cargo_manifest::ops_for_json_gen(cli)?,
                    None,
                    "wasm/json-gen/Cargo.toml",
                )
                .map_err(std::io::Error::other)?,
            );

            let mut gen_json_schema = Block::new("macro_rules! gen_json_schema");
            let mut macro_match = Block::new("($name:ty) => ");
            macro_match
                .line("let dest_path = std::path::Path::new(&\"schemas\").join(&format!(\"{}.json\", stringify!($name)));")
                .line("std::fs::write(&dest_path, serde_json::to_string_pretty(&schemars::schema_for!($name)).unwrap()).unwrap();");
            gen_json_schema.push_block(macro_match);
            let mut lib_str = String::new();
            gen_json_schema
                .fmt(&mut codegen::Formatter::new(&mut lib_str))
                .unwrap();
            lib_str.push('\n');
            let mut lib_scope = codegen::Scope::new();
            let mut lib_export_fn = codegen::Function::new("export_schemas");
            lib_export_fn.vis("pub").push_all(self.json_lines.clone());
            lib_scope.push_fn(lib_export_fn);
            lib_str.push_str(&lib_scope.to_string());
            // Same split as the other crate roots: the generated `macro_rules!` + `export_schemas`
            // live under `wasm/json-gen/src/generated/mod.rs`, exposed through the seed-once thin
            // root's glob re-export (so `<lib>_json_schema_gen::export_schemas()` in main.rs still
            // resolves). `main.rs` stays fully tool-owned and unchanged.
            out.insert(
                "wasm/json-gen/src/generated/mod.rs".to_owned(),
                rustfmt_generated_string(&lib_str)?.into_owned(),
            );
            out.insert(
                "wasm/json-gen/src/lib.rs".to_owned(),
                rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
            );

            let mut main_scope = codegen::Scope::new();
            main_scope.new_fn("main").line(format!(
                "{}_json_schema_gen::export_schemas();",
                cli.lib_name_code()
            ));
            out.insert(
                "wasm/json-gen/src/main.rs".to_owned(),
                rustfmt_generated_string(&main_scope.to_string())?.into_owned(),
            );
        }

        Ok(out)
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated structs and associated traits (besides serialization ones)
    pub fn rust(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.rust_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" rust scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn rust_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_lib_scope
    }

    /// Serialization scope for `ident`
    pub fn rust_serialize(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.serialize_scopes.entry(scope_name).or_default()
    }

    /// Serialization scope for lib.cddl
    /// e.g. for core stuff, or things without an explicit scope like WASM arrays
    pub fn rust_serialize_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_serialize_lib_scope
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated WASM wrapper structs and associated traits
    pub fn wasm(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.wasm_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" WASM scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn wasm_lib(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_lib_scope
    }

    /// CBOR encoding scope for `ident` (i.e. *Encoding structs)
    pub fn cbor_encodings(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope = types.scope(ident).clone();
        self.cbor_encodings_scopes.entry(scope).or_default()
    }

    /// Write code for serializing {serializing_rust_type} directly into {body}
    #[allow(clippy::only_used_in_recursion)]
    fn generate_serialize(
        &mut self,
        types: &IntermediateTypes,
        serializing_rust_type: SerializingRustType<'_>,
        body: &mut dyn CodeBlock,
        config: SerializeConfig,
        cli: &Cli,
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
        let encoding_var_is_copy = serializing_rust_type.encoding_var_is_copy(types);
        let encoding_var = config.encoding_var(None, encoding_var_is_copy);
        let encoding_var_deref = format!("{encoding_deref}{encoding_var}");
        // field-level @custom_serialize overrides everything
        if let Some(custom_serialize) = &config.custom_serialize {
            let pass_encoding_args = if cli.preserve_encodings {
                Cow::Owned(
                    encoding_fields_impl(types, &config.var_name, serializing_rust_type, cli)
                        .into_iter()
                        .map(|enc| {
                            format!(
                                ", {}",
                                match &config.encoding_var_in_option_struct {
                                    Some(namespace) => format!(
                                        "{}{}.as_ref().map(|encs| encs.{}{}).unwrap_or_default()",
                                        if enc.is_copy { "" } else { "&" },
                                        namespace,
                                        enc.field_name,
                                        if enc.is_copy { "" } else { ".clone()" },
                                    ),
                                    None => enc.field_name.clone(),
                                }
                            )
                        })
                        .collect::<Vec<String>>()
                        .join(""),
                )
            } else {
                Cow::Borrowed("")
            };
            body.line(&format!(
                "{}({}, {}{}{}){}",
                custom_serialize,
                serializer_use,
                expr_ref,
                pass_encoding_args,
                canonical_param(cli),
                line_ender
            ));
        } else {
            match serializing_rust_type {
                SerializingRustType::EncodingOperation(
                    CBOREncodingOperation::Tagged(tag),
                    child,
                ) => {
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
                        cli,
                    );
                    self.generate_serialize(types, *child, body, config, cli);
                }
                SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
                    let inner_se = format!("{}_inner_se", config.var_name);
                    body.line(&format!("let mut {inner_se} = Serializer::new_vec();"));
                    let inner_config = config
                        .clone()
                        .is_end(false)
                        .serializer_name_overload((&inner_se, true));
                    self.generate_serialize(types, *child, body, inner_config, cli);
                    body.line(&format!(
                        "let {}_bytes = {}.finalize();",
                        config.var_name, inner_se
                    ));
                    write_string_sz(
                        body,
                        "write_bytes",
                        serializer_use,
                        &format!("{}_bytes", config.var_name),
                        true,
                        line_ender,
                        &config.encoding_var(Some("bytes"), encoding_var_is_copy),
                        cli,
                    );
                }
                SerializingRustType::Root(ConceptualRustType::Fixed(value), _cfg) => match value {
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
                            cli,
                        );
                    }
                    FixedValue::Nint(i) => {
                        assert!(*i < 0);
                        if !cli.preserve_encodings
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
                                cli,
                            );
                        }
                    }
                    FixedValue::Float(f) => {
                        // float_literal, not Display: `{}` on a whole-valued f64 drops the decimal
                        // point (3.0 -> "3"), emitting an integer literal in an f64 position (E0308).
                        body.line(&format!(
                            "{serializer_use}.write_special(cbor_event::Special::Float({})){line_ender}",
                            float_fixed_literal(*f)
                        ));
                    }
                    FixedValue::Text(s) => {
                        write_string_sz(
                            body,
                            "write_text",
                            serializer_use,
                            &format!("\"{}\"", escape_rust_str(s)),
                            false,
                            line_ender,
                            &encoding_var,
                            cli,
                        );
                    }
                },
                SerializingRustType::Root(ConceptualRustType::Primitive(primitive), _cfg) => {
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
                                true,
                                line_ender,
                                &encoding_var,
                                cli,
                            );
                        }
                        Primitive::Str => {
                            write_string_sz(
                                body,
                                "write_text",
                                serializer_use,
                                &config.expr,
                                true,
                                line_ender,
                                &encoding_var,
                                cli,
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
                                cli,
                            );
                            body.push_block(pos);
                            let mut neg = Block::new("else");
                            // only the _sz variants support i128, the other endpoint is i64
                            let expr = if cli.preserve_encodings {
                                format!("{expr_deref} as i128")
                            } else {
                                format!("{expr_deref} as i64")
                            };
                            if !cli.preserve_encodings && *primitive == Primitive::I64 {
                                // https://github.com/primetype/cbor_event/issues/9
                                // cbor_event doesn't support i64::MIN on write_negative_integer() so we use write_negative_integer_sz() for i64s
                                // even when not preserving encodings
                                neg.line(format!("{serializer_use}.write_negative_integer_sz({expr_deref} as i128, cbor_event::Sz::canonical(({expr_deref} + 1).abs() as u64)){line_ender}"));
                            } else {
                                write_using_sz(
                                    &mut neg,
                                    "write_negative_integer",
                                    serializer_use,
                                    &expr,
                                    &format!("({expr_deref} + 1).abs() as u64"),
                                    line_ender,
                                    &encoding_var_deref,
                                    cli,
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
                                cli,
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
                                cli,
                            );
                        }
                        Primitive::N64 => {
                            if cli.preserve_encodings {
                                write_using_sz(
                                    body,
                                    "write_negative_integer",
                                    serializer_use,
                                    &format!("-({expr_deref} as i128 + 1)"),
                                    &expr_deref,
                                    line_ender,
                                    &encoding_var_deref,
                                    cli,
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
                SerializingRustType::Root(ConceptualRustType::Rust(t), type_cfg) => {
                    match &types.rust_struct(t).unwrap().variant() {
                        RustStructType::CStyleEnum { variants } => {
                            let mut enum_body = Block::new(format!("match {expr_ref}"));
                            for variant in variants {
                                let mut variant_match =
                                    Block::new(format!("{}::{} =>", t, variant.name));
                                self.generate_serialize(
                                    types,
                                    (variant.rust_type()).into(),
                                    &mut variant_match,
                                    config.clone().is_end(true),
                                    cli,
                                );
                                enum_body.push_block(variant_match);
                            }
                            if !config.is_end {
                                enum_body.after("?;");
                            }
                            body.push_block(enum_body);
                        }
                        RustStructType::RawBytesType => {
                            write_string_sz(
                                body,
                                "write_bytes",
                                serializer_use,
                                &format!("{}.to_raw_bytes()", config.expr),
                                false,
                                line_ender,
                                &config.encoding_var(None, false),
                                cli,
                            );
                        }
                        _ => {
                            if types.is_plain_group(t) && !type_cfg.basic_override {
                                body.line(&format!(
                                    "{}.serialize_as_embedded_group({}{}){}",
                                    config.expr,
                                    serializer_pass,
                                    canonical_param(cli),
                                    line_ender
                                ));
                            } else {
                                body.line(&format!(
                                    "{}.serialize({}{}){}",
                                    config.expr,
                                    serializer_pass,
                                    canonical_param(cli),
                                    line_ender
                                ));
                            }
                        }
                    }
                }
                SerializingRustType::Root(ConceptualRustType::Array(ty), _cfg) => {
                    let len_expr = match &ty.conceptual_type {
                        ConceptualRustType::Rust(elem_ident)
                            if types.is_plain_group(elem_ident) =>
                        {
                            // you should not be able to indiscriminately encode a plain group like this as it
                            // could be multiple elements. This would require special handling if it's even permitted in CDDL.
                            assert!(ty.encodings.is_empty());
                            if let Some(fixed_elem_size) = ty.expanded_field_count(types) {
                                format!("{} * {}.len() as u64", fixed_elem_size, config.expr)
                            } else {
                                format!(
                                    "{}.iter().map(|e| {}).sum()",
                                    config.expr,
                                    ty.definite_info("e", true, types, cli)
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
                        cli,
                    );
                    let elem_var_name = format!("{}_elem", config.var_name);
                    let elem_encs = if cli.preserve_encodings {
                        encoding_fields(
                            types,
                            &elem_var_name,
                            &ty.clone().resolve_aliases(),
                            false,
                            cli,
                        )
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
                    self.generate_serialize(
                        types,
                        (&**ty).into(),
                        &mut loop_block,
                        elem_config,
                        cli,
                    );
                    body.push_block(loop_block);
                    end_len(body, serializer_use, &encoding_var, config.is_end, cli);
                }
                SerializingRustType::Root(ConceptualRustType::Map(key, value), _cfg) => {
                    start_len(
                        body,
                        Representation::Map,
                        serializer_use,
                        &encoding_var,
                        &format!("{}.len() as u64", config.expr),
                        cli,
                    );
                    let ser_loop = if cli.preserve_encodings {
                        let key_enc_fields = encoding_fields(
                            types,
                            &format!("{}_key", config.var_name),
                            &key.clone().resolve_aliases(),
                            false,
                            cli,
                        );
                        let value_enc_fields = encoding_fields(
                            types,
                            &format!("{}_value", config.var_name),
                            &value.clone().resolve_aliases(),
                            false,
                            cli,
                        );
                        let mut ser_loop = if cli.canonical_form {
                            let mut key_order = Block::new(format!(
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
                            self.generate_serialize(
                                types,
                                (&**key).into(),
                                &mut key_order,
                                key_config,
                                cli,
                            );
                            key_order.line("Ok((buf.finalize(), k, v))").after(
                                ").collect::<Result<Vec<(Vec<u8>, &_, &_)>, cbor_event::Error>>()?;",
                            );
                            body.push_block(key_order);
                            let mut key_order_if = Block::new("if force_canonical");
                            let mut key_order_sort = Block::new(
                                "key_order.sort_by(|(lhs_bytes, _, _), (rhs_bytes, _, _)|",
                            );
                            let mut key_order_sort_match =
                                Block::new("match lhs_bytes.len().cmp(&rhs_bytes.len())");
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
                            ser_loop
                                .line(format!("{serializer_use}.write_raw_bytes(&key_bytes)?;"));
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
                            self.generate_serialize(
                                types,
                                (&**key).into(),
                                &mut ser_loop,
                                key_config,
                                cli,
                            );
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
                        self.generate_serialize(
                            types,
                            (&**value).into(),
                            &mut ser_loop,
                            value_config,
                            cli,
                        );
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
                        self.generate_serialize(
                            types,
                            (&**key).into(),
                            &mut ser_loop,
                            key_config,
                            cli,
                        );
                        self.generate_serialize(
                            types,
                            (&**value).into(),
                            &mut ser_loop,
                            value_config,
                            cli,
                        );
                        ser_loop
                    };
                    body.push_block(ser_loop);
                    end_len(body, serializer_use, &encoding_var, config.is_end, cli);
                }
                SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
                    let mut opt_block = Block::new(format!("match {expr_ref}"));
                    // TODO: do this in one line without a block if possible somehow.
                    //       see other comment in generate_enum()
                    let mut some_block = Block::new("Some(x) =>");
                    // The inner serialize must terminate the same way the whole Optional does. When
                    // the Optional is the tail expression (`is_end`), each arm RETURNS the
                    // serializer. When it is one statement among others (a struct field), the arms
                    // must be *statements* ending in `?;`: an inner whose body is inlined (a
                    // collection loop) emits an owning `Ok(serializer)` tail under `is_end=true`,
                    // which moves `serializer` and then conflicts with the caller's trailing
                    // `Ok(serializer)` (E0382). Mirroring `config.is_end` keeps both cases valid.
                    let opt_config = config
                        .clone()
                        .expr("x")
                        .expr_is_ref(true)
                        .is_end(config.is_end);
                    self.generate_serialize(
                        types,
                        (&**ty).into(),
                        &mut some_block,
                        opt_config,
                        cli,
                    );
                    some_block.after(",");
                    opt_block.push_block(some_block);
                    if config.is_end {
                        opt_block.line(format!(
                            "None => {serializer_use}.write_special(cbor_event::Special::Null),"
                        ));
                    } else {
                        let mut none_block = Block::new("None =>");
                        none_block.line(format!(
                            "{serializer_use}.write_special(cbor_event::Special::Null)?;"
                        ));
                        none_block.after(",");
                        opt_block.push_block(none_block);
                        opt_block.after(";");
                    }
                    body.push_block(opt_block);
                }
                SerializingRustType::Root(ConceptualRustType::Alias(ident, ty), _cfg) => {
                    let config_for_alias = if let Some(custom_serialize) = types
                        .type_aliases()
                        .get(ident)
                        .unwrap()
                        .rule_metadata
                        .as_ref()
                        .and_then(|rmd| rmd.custom_serialize.clone())
                    {
                        config.custom_serialize(custom_serialize)
                    } else {
                        config
                    };
                    self.generate_serialize(types, (&**ty).into(), body, config_for_alias, cli)
                }
            };
        }
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
        cli: &Cli,
    ) -> DeserializationCode {
        //body.line(&format!("println!(\"deserializing {}\");", var_name));
        if !cli.preserve_encodings {
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
        let convert_err_to_ours = ".map_err(Into::<DeserializeError>::into)";
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
        // field-level @custom_deserialize overrides everything
        if let Some(custom_deserialize) = &config.custom_deserialize {
            let deser_err_map = if !config.final_exprs.is_empty() {
                let enc_fields =
                    encoding_fields_impl(types, config.var_name, serializing_rust_type, cli);
                let (closure_args, tuple_fields) = if enc_fields.is_empty() {
                    (config.var_name.to_owned(), "".to_owned())
                } else {
                    let enc_fields_names = enc_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect::<Vec<String>>()
                        .join(", ");
                    (
                        format!("({}, {})", config.var_name, enc_fields_names),
                        enc_fields_names,
                    )
                };
                Cow::Owned(format!(
                    ".map(|{}| ({}, {}, {}))",
                    closure_args,
                    config.var_name,
                    config.final_exprs.join(", "),
                    tuple_fields
                ))
            } else {
                Cow::Borrowed("")
            };
            deser_code.content.line(&format!(
                "{}{}({}){}{}",
                before_after.before_str(true),
                custom_deserialize,
                deserializer_name,
                deser_err_map,
                before_after.after_str(true),
            ));
        } else {
            match serializing_rust_type {
                SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => {
                    if !cli.preserve_encodings {
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
                            special_block
                                .line("return Err(DeserializeFailure::ExpectedNull.into());");
                            deser_code.content.push_block(special_block);
                            if cli.preserve_encodings {
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, None),
                                    before_after.after_str(false)
                                ));
                            }
                        }
                        FixedValue::Uint(x) => {
                            if cli.preserve_encodings {
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
                            if cli.preserve_encodings {
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
                            if cli.preserve_encodings {
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
                            if cli.preserve_encodings {
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
                            if cli.preserve_encodings {
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
                            let mut compare_block = Block::new(format!(
                                "if {}_value != \"{}\"",
                                config.var_name,
                                escape_rust_str(x)
                            ));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Str({}_value), expected: Key::Str(String::from(\"{}\")) }}.into());", config.var_name, escape_rust_str(x)));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
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
                            // float_literal, not Display: `{}` on a whole-valued f64 drops the
                            // decimal point (3.0 -> "3"), emitting integer literals in the f64
                            // compare and Key::Float positions (E0308).
                            let mut compare_block = Block::new(format!(
                                "if {}_value != {}",
                                config.var_name,
                                float_fixed_literal(*x)
                            ));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Float({}_value), expected: Key::Float({}) }}.into());", config.var_name, float_fixed_literal(*x)));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                unimplemented!("preserve_encodings is not implemented for float")
                            }
                        }
                        _ => unimplemented!(),
                    };
                    deser_code.throws = true;
                    // this block needs to evaluate to a Result even though it has no value
                    if !cli.preserve_encodings && before_after.expects_result {
                        deser_code.content.line("Ok(())");
                    }
                }
                SerializingRustType::Root(ConceptualRustType::Primitive(p), type_cfg) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                        deser_code.throws = true;
                    }
                    let error_convert = if before_after.expects_result {
                        convert_err_to_ours
                    } else {
                        ""
                    };
                    let non_preserve_bounds_fn =
                        |x: &str, bounds: &Option<(Option<i128>, Option<i128>)>| match bounds {
                            // always convert error to have consistent E for the and_then
                            Some(bounds) => Cow::Owned(format!(
                                "{}.and_then(|{}| {} else {{ Ok({}) }})",
                                convert_err_to_ours,
                                x,
                                bounds_check_if_block(bounds, &bounds_check_expr(*p, x), false),
                                x,
                            )),
                            None => Cow::Borrowed(""),
                        };
                    // --- width guards for the narrowing casts below ---------------------------
                    // Every integer read on this path comes back WIDER than the target type (u64
                    // from the unsigned readers, i64/i128 from the nint readers), so each `as`
                    // cast must be preceded by a check that makes it lossless: a bare cast
                    // silently truncated out-of-width values (`uint .size 2` -> u16 decoded 65536
                    // "successfully" as 0), and the exact-width collapses (`i8 = -128..127`)
                    // carry NO residual `bounds`, so nothing else rejected. The guard reuses the
                    // authored-bounds `.and_then(..)` shape and the existing RangeCheck failure
                    // (reporting the full type window), and is SKIPPED when the authored/
                    // classified check already caps the failing side (subsuming it), so bounded
                    // emissions stay byte-identical.
                    let prim_window = |p: Primitive| match p {
                        Primitive::U8 => (0i128, u8::MAX as i128),
                        Primitive::U16 => (0i128, u16::MAX as i128),
                        Primitive::U32 => (0i128, u32::MAX as i128),
                        Primitive::I8 => (i8::MIN as i128, i8::MAX as i128),
                        Primitive::I16 => (i16::MIN as i128, i16::MAX as i128),
                        Primitive::I32 => (i32::MIN as i128, i32::MAX as i128),
                        Primitive::I64 => (i64::MIN as i128, i64::MAX as i128),
                        _ => unreachable!("width guard only applies to narrowing-cast primitives"),
                    };
                    // `.and_then(..)` rejecting `cond` with the full type window via RangeCheck.
                    // `pat`/`ok` carry the value-only vs (value, encoding)-tuple shapes.
                    // `converted`: whether an earlier chain stage (an authored-bounds fn or the
                    // site's error_convert) already mapped the error to DeserializeError — when
                    // nothing did, the guard prepends the conversion itself (same "consistent E
                    // for the and_then" rule as the bounds fns).
                    let width_reject = |cond: &str,
                                        wmin: i128,
                                        wmax: i128,
                                        pat: &str,
                                        ok: &str,
                                        converted: bool| {
                        format!(
                            "{}.and_then(|{pat}| if {cond} {{ Err(DeserializeFailure::RangeCheck{{ found: x as isize, min: Some({wmin}), max: Some({wmax}) }}.into()) }} else {{ Ok({ok}) }})",
                            if converted { "" } else { convert_err_to_ours },
                        )
                    };
                    // A guard is superfluous when the emitted check already caps the arm's failing
                    // side: an authored/classified upper bound <= the type max (uint side) or lower
                    // bound >= the type min (nint side). A min>max pair is the `.ne` EXCLUSION
                    // encoding — it caps nothing.
                    let upper_caps = |bounds: &Option<(Option<i128>, Option<i128>)>, wmax: i128| matches!(bounds, Some((mn, Some(mx))) if mn.is_none_or(|mn| mn <= *mx) && *mx <= wmax);
                    let lower_caps = |bounds: &Option<(Option<i128>, Option<i128>)>, wmin: i128| matches!(bounds, Some((Some(mn), mx)) if mx.is_none_or(|mx| *mn <= mx) && *mn >= wmin);
                    let uint_arm_needs_width = |arm: &SignArmBounds, wmax: i128| match arm {
                        // the whole arm rejects unconditionally — no value ever reaches the cast
                        SignArmBounds::Empty(_) => false,
                        SignArmBounds::Check(bounds) => !upper_caps(&Some(*bounds), wmax),
                        SignArmBounds::Unconstrained => true,
                    };
                    let nint_arm_needs_width = |arm: &SignArmBounds, wmin: i128| match arm {
                        SignArmBounds::Empty(_) => false,
                        SignArmBounds::Check(bounds) => !lower_caps(&Some(*bounds), wmin),
                        SignArmBounds::Unconstrained => true,
                    };
                    // `width`: the optional (wmin, wmax) window for a width guard on the value
                    // read — Some only for the narrowing-cast unsigned primitives (u8/u16/u32),
                    // None for every width-safe caller (bytes/text/u64/n64).
                    let mut deser_primitive =
                        |mut final_exprs: Vec<String>,
                         func: &str,
                         x: &str,
                         x_expr: &str,
                         width: Option<(i128, i128)>| {
                            if cli.preserve_encodings {
                                let enc_expr = match func {
                                    "text" | "bytes" => "StringEncoding::from(enc)",
                                    _ => "Some(enc)",
                                };
                                final_exprs.push(enc_expr.to_owned());
                                let width_fn = width
                                    .map(|(wmin, wmax)| {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !error_convert.is_empty(),
                                        )
                                    })
                                    .unwrap_or_default();
                                let enc_map_fn = match &type_cfg.bounds {
                                    // always convert error to have consistent E for the and_then
                                    Some(bounds) => format!(
                                        "{}.and_then(|({}, enc)| {} else {{ Ok({}) }})",
                                        convert_err_to_ours,
                                        x,
                                        bounds_check_if_block(
                                            bounds,
                                            &bounds_check_expr(*p, x),
                                            false
                                        ),
                                        final_expr(final_exprs, Some(x_expr.to_owned())),
                                    ),
                                    None => format!(
                                        ".map(|({}, enc)| {})",
                                        x,
                                        final_expr(final_exprs, Some(x_expr.to_owned()))
                                    ),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.{}_sz(){}{}{}{}",
                                    before_after.before_str(true),
                                    deserializer_name,
                                    func,
                                    error_convert,
                                    width_fn,
                                    enc_map_fn,
                                    before_after.after_str(true)
                                ));
                            } else {
                                let bounds_fn = non_preserve_bounds_fn(x, &type_cfg.bounds);
                                let width_fn = width
                                    .map(|(wmin, wmax)| {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "x",
                                            "x",
                                            !bounds_fn.is_empty(),
                                        )
                                    })
                                    .unwrap_or_default();
                                deser_code.content.line(&format!(
                                    "{}{}.{}(){}{}? as {}{}",
                                    before_after.before_str(false),
                                    deserializer_name,
                                    func,
                                    bounds_fn,
                                    width_fn,
                                    p,
                                    before_after.after_str(false)
                                ));
                                deser_code.throws = true;
                            }
                        };
                    match p {
                        Primitive::Bytes => {
                            deser_primitive(config.final_exprs, "bytes", "bytes", "bytes", None)
                        }
                        Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                            // The u64 read is wider than the target: width-guard the cast unless
                            // an authored upper bound already caps it.
                            let (wmin, wmax) = prim_window(*p);
                            let width =
                                (!upper_caps(&type_cfg.bounds, wmax)).then_some((wmin, wmax));
                            deser_primitive(
                                config.final_exprs,
                                "unsigned_integer",
                                "x",
                                &format!("x as {}", p),
                                width,
                            )
                        }
                        Primitive::U64 => {
                            deser_primitive(config.final_exprs, "unsigned_integer", "x", "x", None)
                        }
                        Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64 => {
                            // A signed int splits across two CBOR major types (uint arm / nint arm),
                            // so we classify the value window per arm: a bound may be vacuous here
                            // (drop it), constraining (keep it), or exclude the arm's whole sign
                            // domain (reject unconditionally). The uint arm reads a `u64` and so can
                            // never compare against a negative bound — hence the classification
                            // rather than a raw full-window check.
                            let uint_arm = classify_sign_arm(&type_cfg.bounds, SignArm::Uint);
                            let nint_arm = classify_sign_arm(&type_cfg.bounds, SignArm::Nint);
                            // Width guards for the per-arm narrowing casts: the uint arm reads a
                            // u64 (can exceed the type max — 2^63 would wrap i64 negative) and the
                            // nint readers return i64/i128 (can fall below the type min). Skipped
                            // when the arm's classified check already caps that side.
                            let (wmin, wmax) = prim_window(*p);
                            let uint_width = uint_arm_needs_width(&uint_arm, wmax);
                            let nint_width = nint_arm_needs_width(&nint_arm, wmin);
                            let mut type_check = Block::new(format!(
                                "{}match {}.cbor_type()?",
                                before_after.before_str(false),
                                deserializer_name
                            ));
                            if cli.preserve_encodings {
                                let bounds_fn = |arm: &SignArmBounds| match sign_arm_if_block(
                                    arm, "x", false,
                                ) {
                                    // always convert error to have consistent E for the and_then
                                    Some(if_block) => Cow::Owned(format!(
                                        "{}.and_then(|(x, enc)| {} else {{ Ok((x, enc)) }})",
                                        convert_err_to_ours, if_block,
                                    )),
                                    None => Cow::Borrowed(""),
                                };
                                let uint_bounds_fn = bounds_fn(&uint_arm);
                                let mut pos = Block::new("cbor_event::Type::UnsignedInteger =>");
                                pos.line(format!(
                                    "let (x, enc) = {}.unsigned_integer_sz(){}{}?;",
                                    deserializer_name,
                                    uint_bounds_fn,
                                    if uint_width {
                                        width_reject(
                                            &format!("x > {wmax}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !uint_bounds_fn.is_empty(),
                                        )
                                    } else {
                                        String::new()
                                    }
                                ))
                                .line(format!("(x as {}, Some(enc))", p))
                                .after(",");
                                type_check.push_block(pos);
                                // let this cover both the negative int case + error case
                                let nint_bounds_fn = bounds_fn(&nint_arm);
                                let mut neg = Block::new("_ =>");
                                neg.line(format!(
                                    "let (x, enc) = {}.negative_integer_sz(){}{}?;",
                                    deserializer_name,
                                    nint_bounds_fn,
                                    if nint_width {
                                        width_reject(
                                            &format!("x < {wmin}"),
                                            wmin,
                                            wmax,
                                            "(x, enc)",
                                            "(x, enc)",
                                            !nint_bounds_fn.is_empty(),
                                        )
                                    } else {
                                        String::new()
                                    }
                                ))
                                .line(format!("(x as {}, Some(enc))", p))
                                .after(",");
                                type_check.push_block(neg);
                            } else {
                                let non_preserve_arm_fn = |arm: &SignArmBounds, x: &str| {
                                    match sign_arm_if_block(arm, x, false) {
                                        // always convert error to have consistent E for the and_then
                                        Some(if_block) => Cow::Owned(format!(
                                            "{}.and_then(|{}| {} else {{ Ok({}) }})",
                                            convert_err_to_ours, x, if_block, x,
                                        )),
                                        None => Cow::Borrowed(""),
                                    }
                                };
                                let uint_arm_fn = non_preserve_arm_fn(&uint_arm, "x");
                                type_check
                                .line(format!(
                                    "cbor_event::Type::UnsignedInteger => {}.unsigned_integer(){}{}? as {},",
                                    deserializer_name,
                                    uint_arm_fn,
                                    if uint_width {
                                        width_reject(&format!("x > {wmax}"), wmin, wmax, "x", "x", !uint_arm_fn.is_empty())
                                    } else {
                                        String::new()
                                    },
                                    p));
                                // https://github.com/primetype/cbor_event/issues/9
                                // cbor_event's negative_integer() doesn't support i64::MIN so we use the _sz function here instead as that one supports all nints.
                                // The _sz reader yields the real signed value, so the nint arm checks the full window directly (no sign partition needed).
                                if *p == Primitive::I64 {
                                    let bounds_fn = match &type_cfg.bounds {
                                        Some(bounds) => Cow::Owned(format!(
                                            "{}.and_then(|(x, _enc)| {} else {{ Ok((x, _enc)) }})",
                                            convert_err_to_ours,
                                            bounds_check_if_block(
                                                bounds,
                                                &bounds_check_expr(*p, "x"),
                                                false
                                            ),
                                        )),
                                        None => Cow::Borrowed(""),
                                    };
                                    type_check.line(format!(
                                    "_ => {}.negative_integer_sz(){}{}.map(|(x, _enc)| x)? as {},",
                                    deserializer_name, bounds_fn,
                                    if nint_width {
                                        width_reject(&format!("x < {wmin}"), wmin, wmax, "(x, _enc)", "(x, _enc)", !bounds_fn.is_empty())
                                    } else {
                                        String::new()
                                    },
                                    p
                                ));
                                } else {
                                    let nint_arm_fn = non_preserve_arm_fn(&nint_arm, "x");
                                    type_check.line(format!(
                                        "_ => {}.negative_integer(){}{}? as {},",
                                        deserializer_name,
                                        nint_arm_fn,
                                        if nint_width {
                                            width_reject(
                                                &format!("x < {wmin}"),
                                                wmin,
                                                wmax,
                                                "x",
                                                "x",
                                                !nint_arm_fn.is_empty(),
                                            )
                                        } else {
                                            String::new()
                                        },
                                        p
                                    ));
                                }
                            }
                            type_check.after(before_after.after_str(false));
                            deser_code.content.push_block(type_check);
                            deser_code.throws = true;
                        }
                        Primitive::N64 => {
                            if cli.preserve_encodings {
                                deser_primitive(
                                    config.final_exprs,
                                    "negative_integer",
                                    "x",
                                    // width-safe: the nint domain (-2^64..-1) maps onto the u64
                                    // magnitude exactly, so no guard is needed
                                    "(x + 1).abs() as u64",
                                    None,
                                )
                            } else {
                                // https://github.com/primetype/cbor_event/issues/9
                                // cbor_event's negative_integer() doesn't support full nint range so we use the _sz function here instead as that one supports all nints
                                let bounds_fn = match &type_cfg.bounds {
                                    Some(bounds) => Cow::Owned(format!(
                                        ".and_then(|(x, _enc)| {} else {{ Ok((x + 1).abs() as u64) }})",
                                        bounds_check_if_block(
                                            bounds,
                                            &bounds_check_expr(*p, "x"),
                                            false
                                        ),
                                    )),
                                    None => Cow::Borrowed(".map(|(x, _enc)| (x + 1).abs() as u64)"),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.negative_integer_sz(){}{}{}",
                                    before_after.before_str(true),
                                    deserializer_name,
                                    error_convert,
                                    bounds_fn,
                                    before_after.after_str(true)
                                ));
                            }
                        }
                        Primitive::Str => {
                            deser_primitive(config.final_exprs, "text", "s", "s", None)
                        }
                        Primitive::Bool => {
                            // no encoding differences for bool. Use `bool::deserialize` (like the
                            // float arms below) rather than `raw.bool().map_err(Into::into)`: the
                            // latter's intermediate error type is unconstrained in element/push
                            // position (`arr.push(<expr>?)`), so with multiple `From<_> for
                            // DeserializeError` impls it fails inference (E0282/E0283) — e.g.
                            // `[* bool]` emitted non-compiling code.
                            deser_code.content.line(&final_result_expr_complete(
                                &mut deser_code.throws,
                                config.final_exprs,
                                "bool::deserialize(raw)",
                            ));
                        }
                        Primitive::F32 => {
                            if cli.preserve_encodings {
                                unimplemented!("preserve_encodings is not implemented for float")
                            }
                            // NaN-safe window enforced inline via `and_then` (the value is compared
                            // as f64 so the authored decimal literal is exact). Integer `bounds`
                            // never attach to a float (parsing routes those to float_bounds/reject);
                            // assert it so a routing regression fails loudly instead of silently
                            // skipping enforcement.
                            assert!(
                                type_cfg.bounds.is_none(),
                                "integer bounds on an f32 — parsing must route float constraints to float_bounds"
                            );
                            let result_expr = match &type_cfg.float_bounds {
                                Some(window) => format!(
                                    "f32::deserialize(raw).and_then(|x| {} else {{ Ok(x) }})",
                                    bounds_check_if_block_float(window, true, "x", false, None)
                                ),
                                None => "f32::deserialize(raw)".to_owned(),
                            };
                            deser_code.content.line(&final_result_expr_complete(
                                &mut deser_code.throws,
                                config.final_exprs,
                                &result_expr,
                            ));
                        }
                        Primitive::F64 => {
                            if cli.preserve_encodings {
                                unimplemented!("preserve_encodings is not implemented for float")
                            }
                            assert!(
                                type_cfg.bounds.is_none(),
                                "integer bounds on an f64 — parsing must route float constraints to float_bounds"
                            );
                            let result_expr = match &type_cfg.float_bounds {
                                Some(window) => format!(
                                    "f64::deserialize(raw).and_then(|x| {} else {{ Ok(x) }})",
                                    bounds_check_if_block_float(window, false, "x", false, None)
                                ),
                                None => "f64::deserialize(raw)".to_owned(),
                            };
                            deser_code.content.line(&final_result_expr_complete(
                                &mut deser_code.throws,
                                config.final_exprs,
                                &result_expr,
                            ));
                        }
                    };
                }
                SerializingRustType::Root(ConceptualRustType::Rust(ident), type_cfg) => {
                    // check for type-level @custom_deserialize
                    if let Some(custom_deserialize) = &types
                        .rust_struct(ident)
                        .unwrap()
                        .config()
                        .custom_deserialize
                    {
                        // because this is type-level we must handle final_exprs as it could be wrapped in a tag, etc
                        deser_code.content.line(&final_result_expr_complete(
                            &mut deser_code.throws,
                            config.final_exprs,
                            &format!("{}({})", custom_deserialize, deserializer_name),
                        ));
                    } else {
                        match &types.rust_struct(ident).unwrap().variant() {
                            RustStructType::CStyleEnum { variants } => {
                                if config.optional_field {
                                    deser_code.content.line("read_len.read_elems(1)?;");
                                    deser_code.throws = true;
                                    deser_code.read_len_used = true;
                                }
                                // iflet Some(common) = enum_variants_common_constant_type(variants) {
                                //     // TODO: potentially simplified deserialization some day
                                //     // issue: https://github.com/dcSpark/cddl-codegen/issues/145
                                // } else {
                                // A c-style enum has no Deserialize impl of its own: its decode is a
                                // try-each-variant sequence with early `return Ok(Enum::Variant)` + a
                                // trailing NoVariantMatched Err, which only type-checks as the body of a
                                // fn/closure returning `Result<Enum, _>`. When the caller places our
                                // result directly (empty before/after — e.g. a struct field that wraps us
                                // in its own annotate closure, or a type-choice variant's closure) that
                                // body composes as-is. When the caller instead splices our value into a
                                // larger expression (non-empty before/after — the newtype wrapper's
                                // `Ok(Self(<here>))`) the statement form can't be spliced (the early
                                // returns would leak out, dropping the wrapper -> E0308), so we first wrap
                                // it in an immediately-invoked closure to yield a composable
                                // `Result<Enum, _>` expression and let before_after wrap that.
                                let mut enum_body = (!before_after.before.is_empty()
                                    || !before_after.after.is_empty())
                                .then(|| {
                                    let mut b = Block::new(format!(
                                        "{}(|| -> Result<_, DeserializeError>",
                                        before_after.before_str(true)
                                    ));
                                    b.after(format!(")(){}", before_after.after_str(true)));
                                    b
                                });
                                {
                                    let target: &mut dyn CodeBlock = match enum_body.as_mut() {
                                        Some(b) => b,
                                        None => &mut deser_code.content,
                                    };
                                    target.line(
                                        "let initial_position = raw.as_mut_ref().stream_position().unwrap();",
                                    );
                                    let mut variant_final_exprs = config.final_exprs.clone();
                                    if cli.preserve_encodings {
                                        for enc_var in encoding_fields(
                                            types,
                                            config.var_name,
                                            variants[0].rust_type(),
                                            false,
                                            cli,
                                        ) {
                                            variant_final_exprs.push(enc_var.field_name);
                                        }
                                    }
                                    for variant in variants {
                                        let mut return_if_deserialized =
                                            make_enum_variant_return_if_deserialized(
                                                self,
                                                types,
                                                variant,
                                                variant_final_exprs.is_empty(),
                                                None,
                                                target,
                                                cli,
                                            );
                                        return_if_deserialized
                            .line(format!("Ok(({})) => return Ok({}),",
                            variant_final_exprs.join(", "),
                            final_expr(variant_final_exprs.clone(), Some(format!("{}::{}", ident, variant.name)))))
                            .line("Err(_) => raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap(),")
                            .after(";");
                                        target.push_block(return_if_deserialized);
                                    }
                                    target.line(&format!(
                        "Err(DeserializeError::new(\"{ident}\", DeserializeFailure::NoVariantMatched))"
                    ));
                                }
                                if let Some(enum_body) = enum_body {
                                    deser_code.content.push_block(enum_body);
                                }
                            }
                            RustStructType::RawBytesType => {
                                if config.optional_field {
                                    deser_code.content.line("read_len.read_elems(1)?;");
                                    deser_code.throws = true;
                                    deser_code.read_len_used = true;
                                }
                                if cli.preserve_encodings {
                                    config
                                        .final_exprs
                                        .push("StringEncoding::from(enc)".to_owned());
                                    let from_raw_bytes_with_conversions = format!(
                                        "{}::from_raw_bytes(&bytes).map(|bytes| {}).map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)).into())",
                                        ident,
                                        final_expr(config.final_exprs, Some("bytes".to_owned()))
                                    );
                                    deser_code.content.line(&format!(
                                        "{}{}.bytes_sz(){}.and_then(|(bytes, enc)| {}){}",
                                        before_after.before_str(true),
                                        deserializer_name,
                                        convert_err_to_ours,
                                        from_raw_bytes_with_conversions,
                                        before_after.after_str(true)
                                    ));
                                } else {
                                    let from_raw_bytes_with_conversions = format!(
                                        "{ident}::from_raw_bytes(&bytes).map_err(|e| DeserializeFailure::InvalidStructure(Box::new(e)).into())"
                                    );
                                    deser_code.content.line(&format!(
                                        "{}{}.bytes(){}.and_then(|bytes| {}){}",
                                        before_after.before_str(true),
                                        deserializer_name,
                                        convert_err_to_ours,
                                        from_raw_bytes_with_conversions,
                                        before_after.after_str(true)
                                    ));
                                }
                            }
                            _ => {
                                if types.is_plain_group(ident) && !type_cfg.basic_override {
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
                                    let final_expr_value =
                                        format!("{ident}::deserialize({deserializer_name})");
                                    deser_code.content.line(&final_result_expr_complete(
                                        &mut deser_code.throws,
                                        config.final_exprs,
                                        &final_expr_value,
                                    ));
                                }
                            }
                        }
                    }
                }
                SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
                    let read_len_check =
                        config.optional_field || (ty.expanded_field_count(types) != Some(1));
                    // codegen crate doesn't support if/else or appending a block after a block, only strings
                    // so we need to create a local bool var and use a match instead
                    let if_label = if ty.cbor_types(types).contains(&cbor_event::Type::Special) {
                        let is_some_check_var = format!("{}_is_some", config.var_name);
                        let mut is_some_check =
                            Block::new(format!("let {is_some_check_var} = match cbor_type()?"));
                        let mut special_block = Block::new("cbor_event::Type::Special =>");
                        special_block
                            .line(format!("let special = {deserializer_name}.special()?;"));
                        special_block.line(format!(
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
                    let ty_enc_fields = if cli.preserve_encodings {
                        encoding_fields(
                            types,
                            config.var_name,
                            &ty.clone().resolve_aliases(),
                            false,
                            cli,
                        )
                    } else {
                        vec![]
                    };
                    if ty_enc_fields.is_empty() {
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new("Some(", ")", false),
                            config.optional_field(false),
                            cli,
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
                            cli,
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
                    // we don't use this to avoid the new (true) if cli.preserve_encodings is set
                    //self.generate_deserialize(types, &ConceptualRustType::Fixed(FixedValue::Null), var_name, "", "", in_embedded, false, add_parens, &mut none_block);
                    let mut check_null = Block::new(format!(
                        "if {deserializer_name}.special()? != cbor_event::Special::Null"
                    ));
                    check_null.line("return Err(DeserializeFailure::ExpectedNull.into());");
                    none_block.push_block(check_null);
                    if cli.preserve_encodings {
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
                    deser_block.after(before_after.after_str(false));
                    deser_block.push_block(none_block);
                    deser_code.content.push_block(deser_block);
                    deser_code.throws = true;
                }
                SerializingRustType::Root(ConceptualRustType::Array(ty), type_cfg) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    let arr_var_name = format!("{}_arr", config.var_name);
                    deser_code
                        .content
                        .line(&format!("let mut {arr_var_name} = Vec::new();"));
                    let elem_var_name = format!("{}_elem", config.var_name);
                    let elem_encs = if cli.preserve_encodings {
                        encoding_fields(
                            types,
                            &elem_var_name,
                            &ty.clone().resolve_aliases(),
                            false,
                            cli,
                        )
                    } else {
                        vec![]
                    };
                    if cli.preserve_encodings {
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
                                "let mut {read_len_overload} = {}(len);",
                                cbor_read_len_ctor(cli)
                            ));
                            // inside of deserialize_as_embedded_group we only modify read_len for things we couldn't
                            // statically know beforehand. This was done for other areas that use plain groups in order
                            // to be able to do static length checks for statically sized groups that contain plain groups
                            // at the start of deserialization instead of many checks for every single field.
                            let plain_len_check = match ty.expanded_mandatory_field_count(types) {
                                0 => None,
                                n => Some(format!("{read_len_overload}.read_elems({n})?;")),
                            };
                            elem_config = elem_config.overload_read_len(read_len_overload);
                            let deser_loop = make_deser_loop(
                                "len",
                                &format!("{}_read_len.read()", config.var_name),
                                cli,
                            );
                            (deser_loop, plain_len_check)
                        }
                        _ => (
                            make_deser_loop("len", &format!("({arr_var_name}.len() as u64)"), cli),
                            None,
                        ),
                    };
                    deser_loop.push_block(make_deser_loop_break_check("len", cli));
                    if let Some(plain_len_check) = plain_len_check {
                        deser_loop.line(plain_len_check);
                    }
                    elem_config.deserializer_name_overload = config.deserializer_name_overload;
                    if !elem_encs.is_empty() {
                        let elem_var_names_str =
                            encoding_var_names_str(types, &elem_var_name, ty, cli);
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new(
                                &format!("let {elem_var_names_str} = "),
                                ";",
                                false,
                            ),
                            elem_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                        deser_loop
                            .line(format!("{arr_var_name}.push({elem_var_name});"))
                            .line(format!(
                                "{}_elem_encodings.push({});",
                                config.var_name,
                                tuple_str(
                                    elem_encs.iter().map(|enc| enc.field_name.clone()).collect()
                                )
                            ));
                    } else {
                        self.generate_deserialize(
                            types,
                            (&**ty).into(),
                            DeserializeBeforeAfter::new(
                                &format!("{arr_var_name}.push("),
                                ");",
                                false,
                            ),
                            elem_config,
                            cli,
                        )
                        .add_to(&mut deser_loop);
                    }
                    deser_code.content.push_block(deser_loop);
                    if let Some(bounds) = &type_cfg.bounds {
                        // we use cargo fmt after so it's okay if we just use .line() here
                        deser_code.content.line(&bounds_check_if_block(
                            bounds,
                            &format!("{arr_var_name}.len()"),
                            true,
                        ));
                    }
                    if cli.preserve_encodings {
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
                SerializingRustType::Root(
                    ConceptualRustType::Map(key_type, value_type),
                    type_cfg,
                ) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    if !self.deserialize_generated_for_type(types, &key_type.conceptual_type) {
                        todo!();
                        // TODO: where is the best place to check for this? should we pass in a RustIdent to say where we're generating?!
                        //self.dont_generate_deserialize(name, format!("key type {} doesn't support deserialize", key_type.for_rust_member()));
                    } else if !self
                        .deserialize_generated_for_type(types, &value_type.conceptual_type)
                    {
                        todo!();
                        //self.dont_generate_deserialize(name, format!("value type {} doesn't support deserialize", value_type.for_rust_member()));
                    } else {
                        let table_var = format!("{}_table", config.var_name);
                        deser_code.content.line(&format!(
                            "let mut {} = {}::new();",
                            table_var,
                            table_type(cli)
                        ));
                        let key_var_name = format!("{}_key", config.var_name);
                        let value_var_name = format!("{}_value", config.var_name);
                        let key_encs = if cli.preserve_encodings {
                            encoding_fields(
                                types,
                                &key_var_name,
                                &key_type.clone().resolve_aliases(),
                                false,
                                cli,
                            )
                        } else {
                            vec![]
                        };
                        let value_encs = if cli.preserve_encodings {
                            encoding_fields(
                                types,
                                &value_var_name,
                                &value_type.clone().resolve_aliases(),
                                false,
                                cli,
                            )
                        } else {
                            vec![]
                        };
                        let len_var = format!("{}_len", config.var_name);
                        if cli.preserve_encodings {
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
                            make_deser_loop(&len_var, &format!("({table_var}.len() as u64)"), cli);
                        deser_loop.push_block(make_deser_loop_break_check(&len_var, cli));
                        let mut key_config = DeserializeConfig::new(&key_var_name);
                        key_config.deserializer_name_overload = config.deserializer_name_overload;
                        let mut value_config = DeserializeConfig::new(&value_var_name);
                        value_config.deserializer_name_overload = config.deserializer_name_overload;
                        let (key_var_names_str, value_var_names_str) = if cli.preserve_encodings {
                            (
                                encoding_var_names_str(types, &key_var_name, key_type, cli),
                                encoding_var_names_str(types, &value_var_name, value_type, cli),
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
                            cli,
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
                            cli,
                        )
                        .add_to(&mut deser_loop);
                        let mut dup_check = Block::new(format!(
                            "if {}.insert({}{}, {}).is_some()",
                            table_var,
                            key_var_name,
                            if key_type.is_copy(types) {
                                ""
                            } else {
                                ".clone()"
                            },
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
                        if cli.preserve_encodings {
                            if !key_encs.is_empty() {
                                deser_loop.line(format!(
                                    "{}_key_encodings.insert({}{}, {});",
                                    config.var_name,
                                    key_var_name,
                                    if key_type.encoding_var_is_copy(types) {
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
                                    if key_type.encoding_var_is_copy(types) {
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
                        if let Some(bounds) = &type_cfg.bounds {
                            // we use cargo fmt after so it's okay if we just use .line() here
                            deser_code.content.line(&bounds_check_if_block(
                                bounds,
                                &format!("{table_var}.len()"),
                                true,
                            ));
                        }
                        if cli.preserve_encodings {
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
                SerializingRustType::Root(ConceptualRustType::Alias(ident, ty), cfg) => {
                    let config_for_alias = if let Some(custom_deserialize) = types
                        .type_aliases()
                        .get(ident)
                        .unwrap()
                        .rule_metadata
                        .as_ref()
                        .and_then(|rmd| rmd.custom_deserialize.clone())
                    {
                        config.custom_deserialize(custom_deserialize)
                    } else {
                        config
                    };
                    // keep the OUTER config: an Alias's inner is a bare ConceptualRustType (no
                    // config of its own — see `as_alias`), so recursing with `(&**ty).into()`
                    // would default the config and drop e.g. the occurrence-count bounds a named
                    // array alias carries (its length check would silently vanish here while the
                    // constructor check, emitted from the field's RustType, kept working)
                    self.generate_deserialize(
                        types,
                        SerializingRustType::Root(ty, cfg),
                        before_after,
                        config_for_alias,
                        cli,
                    )
                    .add_to_code(&mut deser_code);
                }
                SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
                    if cli.preserve_encodings {
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
                        cli,
                    )
                    .add_to_code(&mut deser_code);
                    deser_code.throws = true;
                }
                SerializingRustType::EncodingOperation(
                    CBOREncodingOperation::Tagged(tag),
                    child,
                ) => {
                    if config.optional_field {
                        deser_code.content.line("read_len.read_elems(1)?;");
                        deser_code.read_len_used = true;
                    }
                    let mut tag_check = if cli.preserve_encodings {
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
                                cli,
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
                                cli,
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
                    tag_check.line(format!(
                    "{} => {}Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {} }}.into()),",
                    if cli.preserve_encodings { "(tag, _enc)" } else { "tag" },
                    if before_after.expects_result { "" } else { "return " },
                    tag));
                    tag_check.after(before_after.after);
                    deser_code.content.push_block(tag_check);
                    deser_code.throws = true;
                }
            }
        }
        deser_code
    }

    fn deserialize_generated(&self, name: &RustIdent) -> bool {
        !self.no_deser_reasons.contains_key(name)
    }

    fn deserialize_generated_for_type(
        &self,
        types: &IntermediateTypes,
        field_type: &ConceptualRustType,
    ) -> bool {
        match field_type {
            ConceptualRustType::Fixed(_) => true,
            ConceptualRustType::Primitive(_) => true,
            ConceptualRustType::Rust(ident) => {
                types.is_enum(ident) || self.deserialize_generated(ident)
            }
            ConceptualRustType::Array(ty) => {
                self.deserialize_generated_for_type(types, &ty.conceptual_type)
            }
            ConceptualRustType::Map(k, v) => {
                self.deserialize_generated_for_type(types, &k.conceptual_type)
                    && self.deserialize_generated_for_type(types, &v.conceptual_type)
            }
            ConceptualRustType::Optional(ty) => {
                self.deserialize_generated_for_type(types, &ty.conceptual_type)
            }
            ConceptualRustType::Alias(_ident, ty) => self.deserialize_generated_for_type(types, ty),
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
        config: &RustStructConfig,
        cli: &Cli,
    ) {
        // I don't believe this is even possible (wouldn't be a single CBOR value + nowhere to embed)
        // Just sanity checking since it's not handled in the wrapper code here
        assert!(
            variants
                .iter()
                .all(|v| !matches!(v.data, EnumVariantData::Inlined(_)))
        );
        // Rust only
        generate_enum(self, types, name, variants, None, true, tag, config, cli);
        if cli.wasm {
            // Generate a wrapper object that we will expose to wasm around this
            let mut wrapper = create_base_wasm_wrapper(self, types, name, true, cli);
            // new
            for variant in variants.iter() {
                let variant_arg = variant.name_as_var();
                let mut new_func = codegen::Function::new(format!("new_{variant_arg}"));
                new_func.vis("pub");
                if let Some(doc) = &variant.doc {
                    new_func.doc(doc);
                }
                let can_fail = variant.rust_type().needs_bounds_check_if_inlined(types);
                if !variant.rust_type().is_fixed_value() {
                    new_func.arg(&variant_arg, variant.rust_type().for_wasm_param(types));
                }
                let ctor = if variant.rust_type().is_fixed_value() {
                    format!(
                        "{}::new_{}()",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name_as_var()
                    )
                } else {
                    // TODO: see if this is ever needed. we don't pass non-false values in anywher else
                    // and these checks should only be done in the rust side not wasm but there must have
                    // been a reson this was here before (checking only types.can_new_fail())
                    let try_into = false;
                    let from_wasm_expr =
                        variant
                            .rust_type()
                            .from_wasm_boundary_clone(types, &variant_arg, try_into);
                    format!(
                        "{}::new_{}({})",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name_as_var(),
                        ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
                    )
                };
                if can_fail {
                    new_func
                        .ret(format!("Result<{name}, JsError>"))
                        .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                } else {
                    new_func.ret("Self").line(format!("Self({ctor})"));
                }
                wrapper.s_impl.push_fn(new_func);
            }
            add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, None, cli);
            wrapper.push(self, types);
        }
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        array_type_ident: &RustIdent,
        cli: &Cli,
    ) {
        if self.already_generated.insert(array_type_ident.clone()) {
            // --wasm-list-macro: emit a single macro invocation in place of the inline struct +
            // accessor block + conversion impls. The macro also emits the conversions, so we skip
            // building the WasmWrapper entirely (returning early) to avoid double-defining them.
            // Element types whose wasm boundary doesn't reduce to (needs_into, is_copy) - e.g.
            // Optional - fall through to the inline path below.
            if let Some(list_macro) = &cli.wasm_list_macro
                && let Some(needs_into) = element_type.wasm_list_macro_needs_into(types)
            {
                let macro_name = list_macro.split("::").last().unwrap();
                let args = [
                    element_type.for_rust_member(types, true, cli),
                    element_type.for_wasm_return(types),
                    array_type_ident.to_string(),
                    needs_into.to_string(),
                    element_type.is_copy(types).to_string(),
                ];
                self.wasm(types, array_type_ident).raw(format!(
                    "{}!({});",
                    macro_name,
                    args.join(", ")
                ));
                return;
            }
            let inner_type = element_type.name_as_rust_array(types, true, cli);
            let mut wrapper = create_base_wasm_struct(self, array_type_ident, false, cli);
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
                .ret(element_type.for_wasm_return(types))
                .arg_ref_self()
                .arg("index", "usize")
                .line(element_type.to_wasm_boundary(types, "self.0[index]", false));
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            wrapper
                .s_impl
                .new_fn("add")
                .vis("pub")
                .arg_mut_self()
                .arg("elem", element_type.for_wasm_param(types))
                .line(format!(
                    "self.0.push({});",
                    ToWasmBoundaryOperations::format(
                        element_type
                            .from_wasm_boundary_clone(types, "elem", false)
                            .into_iter()
                    )
                ));
            wrapper.add_conversion_methods(&inner_type, cli);
            wrapper.push(self, types);
        }
    }
}

fn canonical_param(cli: &Cli) -> &'static str {
    if cli.canonical_form {
        ", force_canonical"
    } else {
        ""
    }
}

/// the codegen crate doesn't support proc macros for fields so we need to
/// do this with newlines. codegen takes care of indentation somehow.
fn encoding_var_macros(used_in_key: bool, custom_json: bool, cli: &Cli) -> String {
    let mut ret = if used_in_key {
        format!(
            "#[derivative({})]\n",
            key_derives(true, cli)
                .iter()
                .map(|derive| format!("{derive}=\"ignore\""))
                .collect::<Vec<String>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    if cli.json_serde_derives && !custom_json {
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
    cli: &Cli,
) {
    let rep_str = match rep {
        Representation::Array => "array",
        Representation::Map => "map",
    };
    if cli.preserve_encodings {
        body.line(&format!(
            "{}.write_{}_sz({}.to_len_sz({}{}))?;",
            serializer_use,
            rep_str,
            encoding_var,
            len_expr,
            canonical_param(cli)
        ));
    } else {
        body.line(&format!(
            "{serializer_use}.write_{rep_str}(cbor_event::Len::Len({len_expr}))?;"
        ));
    }
}

fn end_len(
    body: &mut dyn CodeBlock,
    serializer_use: &str,
    encoding_var: &str,
    is_end: bool,
    cli: &Cli,
) {
    if cli.preserve_encodings {
        body.line(&format!(
            "{}.end({}{}){}",
            encoding_var,
            serializer_use,
            canonical_param(cli),
            if is_end { "" } else { "?;" }
        ));
    } else if is_end {
        body.line("Ok(serializer)");
    }
}

#[allow(clippy::too_many_arguments)]
fn write_using_sz(
    body: &mut dyn CodeBlock,
    func: &str,
    serializer_use: &str,
    expr: &str,
    fit_sz_expr: &str,
    line_ender: &str,
    encoding_var: &str,
    cli: &Cli,
) {
    if cli.preserve_encodings {
        body.line(&format!(
            "{}.{}_sz({}, fit_sz({}, {}{})){}",
            serializer_use,
            func,
            expr,
            fit_sz_expr,
            encoding_var,
            canonical_param(cli),
            line_ender
        ));
    } else {
        body.line(&format!("{serializer_use}.{func}({expr}){line_ender}"));
    }
}

#[allow(clippy::too_many_arguments)]
fn write_string_sz(
    body: &mut dyn CodeBlock,
    func: &str,
    serializer_use: &str,
    expr: &str,
    expr_is_ref: bool,
    line_ender: &str,
    encoding_var: &str,
    cli: &Cli,
) {
    let expr_ref = if expr_is_ref {
        Cow::from(format!("&{expr}"))
    } else {
        Cow::from(expr)
    };
    if cli.preserve_encodings {
        body.line(&format!(
            "{}.{}_sz({}, {}.to_str_len_sz({}.len() as u64{})){}",
            serializer_use,
            func,
            expr_ref,
            encoding_var,
            expr,
            canonical_param(cli),
            line_ender
        ));
    } else {
        body.line(&format!("{serializer_use}.{func}({expr_ref}){line_ender}"));
    }
}

fn bounds_check_expr(p: Primitive, e: &str) -> String {
    match p {
        Primitive::Bool
        | Primitive::F32
        | Primitive::F64
        | Primitive::I8
        | Primitive::I16
        | Primitive::I32
        | Primitive::I64
        | Primitive::U8
        | Primitive::U16
        | Primitive::U32
        | Primitive::U64
        | Primitive::N64 => e.to_owned(),
        Primitive::Str | Primitive::Bytes => format!("{e}.len()"),
    }
}

// pub(crate): emit_tests mirrors ctor fallibility as `needs_bounds_check_if_inlined && this is Some`
pub(crate) fn bounds_check_expr_rust_type(ty: &RustType, e: &str) -> Option<String> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) => Some(bounds_check_expr(*p, e)),
        ConceptualRustType::Array(_) |
        ConceptualRustType::Map(_, _) => Some(format!("{e}.len()")),
        // Alias should never be hit due to above alias resolving
        ConceptualRustType::Alias(_, _) => unreachable!(),
        // RustType is covered by passed in ctor
        ConceptualRustType::Rust(_) |
        // Optional is not passed into ctor, but instead set later
        ConceptualRustType::Optional(_) |
        // FixedValue has no field associated with it
        ConceptualRustType::Fixed(_) => None,
    }
}

// we store nint as its u64 magnitude `m = |v + 1| = -v - 1`, which is *decreasing* in the signed
// value `v`. So a value bound `vmin <= v <= vmax` maps to a magnitude bound with the endpoints
// SWAPPED: the value-min becomes the magnitude-max and the value-max becomes the magnitude-min
// (e.g. `nint .ge -5` → `v >= -5` → `m <= 4`). Not swapping inverts the check in the constructor
// (the deserializer, which checks the signed value directly, stays correct — so the two disagree).
pub(crate) fn nint_bounds_to_u64(
    bounds: &(Option<i128>, Option<i128>),
) -> (Option<i128>, Option<i128>) {
    (
        bounds.1.map(|x| (x + 1).abs()),
        bounds.0.map(|x| (x + 1).abs()),
    )
}

fn range_check_err(e: &str, min: Option<i128>, max: Option<i128>, return_err: bool) -> String {
    let possible_return = if return_err { "return " } else { "" };
    let opt = |b: Option<i128>| b.map_or_else(|| "None".to_owned(), |b| format!("Some({b})"));
    format!(
        "{{ {}Err(DeserializeFailure::RangeCheck{{ found: {} as isize, min: {}, max: {}}}.into()) }}",
        possible_return,
        e,
        opt(min),
        opt(max),
    )
}

/// Renders a f64 literal that round-trips exactly (Rust's `{:?}` guarantees this for f64), with the
/// `f64` suffix so it types as f64 even when compared against an f32-derived value.
fn float_literal(v: f64) -> String {
    format!("{v:?}f64")
}

/// The unsuffixed twin of `float_literal` for the FIXED-VALUE emission sites (serialize write,
/// deserialize compare, mismatch-error construction, wasm constant): `{:?}` never drops the
/// decimal point (3.0 -> "3.0", where Display renders "3" — an integer literal in an f64
/// position, E0308), while non-whole values render byte-identically to Display (3.5 -> "3.5").
/// Every such site is already f64-typed, so no suffix is needed. NaN/inf cannot reach these
/// sites: a CDDL fixed float value comes from the grammar's decimal/hexfloat lexemes, which
/// denote finite values.
fn float_fixed_literal(v: f64) -> String {
    debug_assert!(v.is_finite(), "fixed-value float literal must be finite");
    format!("{v:?}")
}

/// The ACCEPT-form condition for a float window over `val` (a NaN-safe conjunction of the present
/// sides). Never reject-form (`x < min || x > max`) — under that shape a NaN slips through because
/// both comparisons are false. The caller negates this (`if !(<cond>) {{ Err }}`), so NaN — for
/// which every comparison is false, making the conjunction false — is always rejected.
fn float_accept_cond(
    window: &crate::intermediate::FloatWindow,
    val: &str,
    cast_f64: bool,
) -> String {
    let v = if cast_f64 {
        format!("({val} as f64)")
    } else {
        val.to_owned()
    };
    let mut parts = Vec::new();
    if let Some((min, exclusive)) = window.0 {
        parts.push(format!(
            "{v} {} {}",
            if exclusive { ">" } else { ">=" },
            float_literal(min)
        ));
    }
    if let Some((max, exclusive)) = window.1 {
        parts.push(format!(
            "{v} {} {}",
            if exclusive { "<" } else { "<=" },
            float_literal(max)
        ));
    }
    // a real window always has at least one side; guard the impossible empty case
    if parts.is_empty() {
        unreachable!("float_accept_cond called with an empty window");
    }
    parts.join(" && ")
}

/// The `Err(..)` expression for a failed float window check. `location` `Some(name)` produces a
/// `DeserializeError::new(name, ..)` (wrapper deserialize/new, which annotate the type), `None`
/// produces a bare `DeserializeFailure::RangeCheckFloat{..}.into()` (primitive deserialize and_then).
fn range_check_err_float(
    found_f64: &str,
    window: &crate::intermediate::FloatWindow,
    return_err: bool,
    location: Option<&str>,
) -> String {
    let opt = |side: Option<(f64, bool)>| match side {
        Some((v, _)) => format!("Some({})", float_literal(v)),
        None => "None".to_owned(),
    };
    // stored inclusivity is the negation of the parsed exclusivity flag
    let incl = |side: Option<(f64, bool)>| match side {
        Some((_, exclusive)) => (!exclusive).to_string(),
        None => "false".to_owned(),
    };
    let failure = format!(
        "DeserializeFailure::RangeCheckFloat{{ found: {} as f64, min: {}, max: {}, min_inclusive: {}, max_inclusive: {} }}",
        found_f64,
        opt(window.0),
        opt(window.1),
        incl(window.0),
        incl(window.1),
    );
    let err = match location {
        Some(loc) => format!("DeserializeError::new(\"{loc}\", {failure})"),
        None => format!("{failure}.into()"),
    };
    let possible_return = if return_err { "return " } else { "" };
    format!("{{ {possible_return}Err({err}) }}")
}

/// The NaN-safe `if !(<accept>) {{ Err(..) }}` float bounds check. `cast_f64` casts an f32 value to
/// f64 first so the authored decimal literal is compared exactly.
fn bounds_check_if_block_float(
    window: &crate::intermediate::FloatWindow,
    cast_f64: bool,
    e: &str,
    return_err: bool,
    location: Option<&str>,
) -> String {
    // `range_check_err_float` already appends `as f64` to the found value, so pass the raw expr
    // (avoids a redundant `(x as f64) as f64` for an f32 value).
    format!(
        "if !({}) {}",
        float_accept_cond(window, e, cast_f64),
        range_check_err_float(e, window, return_err, location)
    )
}

/// The value bounds check line for a field/setter/variant-ctor site, dispatching to the integer or
/// float path (or `None` if the type carries no value window / no check expression exists — e.g. a
/// bounded named Rust wrapper checks at its own construction). Reproduces the integer path
/// byte-for-byte (same `nint_bounds_to_u64` swap) so existing snapshots are unchanged.
fn value_bounds_check_line(ty: &RustType, e: &str, return_err: bool) -> Option<String> {
    if let Some(window) = &ty.config.float_bounds {
        let cast_f64 = matches!(
            ty.resolve_alias_shallow(),
            ConceptualRustType::Primitive(Primitive::F32)
        );
        return Some(bounds_check_if_block_float(
            window, cast_f64, e, return_err, None,
        ));
    }
    let bounds = ty.config.bounds.as_ref()?;
    let check_expr = bounds_check_expr_rust_type(ty, e)?;
    if matches!(
        ty.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::N64)
    ) {
        Some(bounds_check_if_block(
            &nint_bounds_to_u64(bounds),
            &check_expr,
            return_err,
        ))
    } else {
        Some(bounds_check_if_block(bounds, &check_expr, return_err))
    }
}

fn bounds_check_if_block(
    bounds: &(Option<i128>, Option<i128>),
    e: &str,
    return_err: bool,
) -> String {
    let cond = match bounds {
        // `.ne N` is encoded as Range(N+1, N-1) (see parsing.rs NE): min > max means an
        // EXCLUSION of the single value between them, not an (unsatisfiable) window
        (Some(min), Some(max)) if min > max => format!("{e} == {}", min - 1),
        (Some(min), Some(max)) => format!("{e} < {min} || {e} > {max}"),
        (None, Some(max)) => format!("{e} > {max}"),
        (Some(min), None) => format!("{e} < {min}"),
        // `classify_sign_arm` never emits a `Check` with both bounds absent (it returns
        // `Unconstrained` instead), and every other caller passes a real range, so this is
        // unreachable by construction rather than a silent panic on empty windows.
        (None, None) => unreachable!("bounds_check_if_block called with no bounds"),
    };
    format!(
        "if {} {}",
        cond,
        range_check_err(e, bounds.0, bounds.1, return_err)
    )
}

/// The two CBOR sign arms a signed int can decode from (unsigned-integer major type vs
/// negative-integer major type). A value window is classified independently per arm.
#[derive(Clone, Copy)]
enum SignArm {
    /// values >= 0, read as a `u64` — a check here can never compare against a negative literal
    Uint,
    /// values <= -1, read as the real signed value via `negative_integer_sz`
    Nint,
}

/// How a value window projects onto one CBOR sign arm.
enum SignArmBounds {
    /// The window imposes no constraint on this arm (bounds vacuous here) — emit no check.
    Unconstrained,
    /// The window narrows to these (possibly one-sided) bounds on this arm — emit the check.
    Check((Option<i128>, Option<i128>)),
    /// The window excludes this arm's entire sign domain — every value it decodes is out of
    /// range. Reject unconditionally, reporting the ORIGINAL window (not the empty projection).
    Empty((Option<i128>, Option<i128>)),
}

/// Project a value window onto one CBOR sign arm. Distinguishes "vacuous in this arm" (drop the
/// bound) from "this arm's whole sign domain is excluded" (unconditional reject) — conflating the
/// two is what made the old per-arm filter panic on all-negative / zero-upper windows.
fn classify_sign_arm(bounds: &Option<(Option<i128>, Option<i128>)>, arm: SignArm) -> SignArmBounds {
    let bounds = match bounds {
        Some(b) => *b,
        None => return SignArmBounds::Unconstrained,
    };
    // `.ne N` exclusion encoding: min > max excludes the single value min-1. Route the exclusion
    // check to whichever arm the excluded value lives in; the other arm has nothing to check.
    if let (Some(min), Some(max)) = bounds
        && min > max
    {
        let excluded_here = match arm {
            SignArm::Uint => (min - 1) >= 0,
            SignArm::Nint => (min - 1) < 0,
        };
        return if excluded_here {
            SignArmBounds::Check((Some(min), Some(max)))
        } else {
            SignArmBounds::Unconstrained
        };
    }
    let (lower, upper) = bounds;
    match arm {
        SignArm::Uint => {
            // uint arm covers values >= 0
            if matches!(upper, Some(u) if u < 0) {
                // upper < 0 → no non-negative value is in range
                return SignArmBounds::Empty((lower, upper));
            }
            // lower <= 0 is vacuous for a u64; upper >= 0 is kept (u == 0 emits `x > 0`)
            let narrowed_lower = lower.filter(|l| *l > 0);
            if narrowed_lower.is_none() && upper.is_none() {
                SignArmBounds::Unconstrained
            } else {
                SignArmBounds::Check((narrowed_lower, upper))
            }
        }
        SignArm::Nint => {
            // nint arm covers values <= -1
            if matches!(lower, Some(l) if l >= 0) {
                // lower >= 0 → no negative value is in range
                return SignArmBounds::Empty((lower, upper));
            }
            // upper >= -1 is vacuous for a nint; lower <= -1 is kept
            let narrowed_upper = upper.filter(|u| *u < -1);
            if lower.is_none() && narrowed_upper.is_none() {
                SignArmBounds::Unconstrained
            } else {
                SignArmBounds::Check((lower, narrowed_upper))
            }
        }
    }
}

/// The `if <cond> { Err(RangeCheck..) }` for one classified sign arm, or `None` when the arm
/// needs no check. The `Empty` case rejects unconditionally (`if true`) rather than emitting the
/// real comparison, since the uint arm can't compare a `u64` against a negative bound.
fn sign_arm_if_block(arm: &SignArmBounds, e: &str, return_err: bool) -> Option<String> {
    match arm {
        SignArmBounds::Unconstrained => None,
        SignArmBounds::Check(bounds) => Some(bounds_check_if_block(bounds, e, return_err)),
        SignArmBounds::Empty(orig) => Some(format!(
            "if true {}",
            range_check_err(e, orig.0, orig.1, return_err)
        )),
    }
}

fn declare_modules(
    gen_scopes: &mut BTreeMap<ModuleScope, codegen::Scope>,
    module_scopes: &[ModuleScope],
) {
    for module_scope in module_scopes.iter() {
        if module_scope.export() {
            let components = module_scope.components();
            for (i, component) in components.iter().enumerate().skip(1) {
                gen_scopes
                    .entry(module_scope.parents(i))
                    .or_default()
                    .raw(format!("pub mod {};", component));
            }
        }
    }
}

#[derive(Debug, Clone)]
enum BlockOrLine {
    Line(String),
    Block(Block),
}

#[derive(Default, Debug, Clone)]
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

impl From<Block> for BlocksOrLines {
    fn from(block: Block) -> Self {
        Self(vec![BlockOrLine::Block(block)])
    }
}

trait CodeBlock {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock;

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock;

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

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for Block {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for BlocksOrLines {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Line(line.to_owned()));
        self
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
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
    manual_json_impl: bool,
    cli: &Cli,
) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(
        &mut s,
        types.used_as_key(ident),
        false,
        manual_json_impl,
        cli,
    );
    let group_impl = codegen::Impl::new(name);
    // TODO: anything here?
    (s, group_impl)
}

/// Formatted string for fully scoped rust crate struct for use from wasm crate
pub fn rust_crate_struct_from_wasm(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    format!(
        "{}::{}",
        rust_crate_struct_scope_from_wasm(types, ident, cli),
        ident
    )
}

pub fn rust_crate_struct_scope_from_wasm(
    types: &IntermediateTypes,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    let scope = types.scope(ident);
    if *scope == *ROOT_SCOPE {
        cli.lib_name_code()
    } else {
        format!("{}::{}", cli.lib_name_code(), scope)
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
    // (macro name, macro params)
    macros: Vec<(String, Vec<String>)>,
}

impl<'a> WasmWrapper<'a> {
    fn push(mut self, gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
        // using Scope::raw() for the macro calls would result in them all being include at the top of the file
        // so we instead use the impl's macro spot to put them before the impl where we want them
        for (full_name, params) in self.macros {
            let macro_name = full_name.split("::").last().unwrap();
            self.s_impl
                .r#macro(format!("{}!({});\n", macro_name, params.join(", ")));
        }
        self.s_impl.r#macro("#[wasm_bindgen]");
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
    fn add_conversion_methods(&mut self, native_name: &str, cli: &Cli) {
        match &cli.wasm_conversions_macro {
            Some(conversion_macro) => {
                self.macros.push((
                    conversion_macro.clone(),
                    vec![native_name.to_owned(), self.ident.to_string()],
                ));
            }
            None => {
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
                    .ret(format!("&{native_name}"))
                    .line("&self.0");
                self.as_ref = Some(as_ref);
            }
        }
    }
}

fn create_base_wasm_struct<'a>(
    gen_scope: &GenerationScope,
    ident: &'a RustIdent,
    exists_in_rust: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    s.vis("pub")
        .derive("Clone")
        .derive("Debug")
        .attr("wasm_bindgen");
    let mut s_impl = codegen::Impl::new(name);
    let mut macros = Vec::new();
    // There are auto-implementing ToCBORBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if exists_in_rust {
        match &cli.wasm_cbor_json_api_macro {
            Some(cbor_json_macro) => {
                macros.push((cbor_json_macro.clone(), vec![name.to_owned()]));
            }
            None => {
                if cli.to_from_bytes_methods {
                    let mut to_bytes = codegen::Function::new("to_cbor_bytes");
                    to_bytes.ret("Vec<u8>").arg_ref_self().vis("pub");
                    if cli.preserve_encodings && cli.canonical_form {
                        to_bytes.line(format!(
                            "{}::serialization::Serialize::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                        let mut to_canonical_bytes =
                            codegen::Function::new("to_canonical_cbor_bytes");
                        to_canonical_bytes
                            .ret("Vec<u8>")
                            .arg_ref_self()
                            .vis("pub")
                            .line("Serialize::to_canonical_cbor_bytes(&self.0)");
                    } else {
                        to_bytes.line(format!(
                            "{}::serialization::ToCBORBytes::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                    }
                    s_impl.push_fn(to_bytes);
                    if gen_scope.deserialize_generated(ident) {
                        s_impl
                            .new_fn("from_cbor_bytes")
                            .ret(format!("Result<{name}, JsError>"))
                            .arg("cbor_bytes", "&[u8]")
                            .vis("pub")
                            .line(format!(
                                "{}::serialization::Deserialize::from_cbor_bytes(cbor_bytes).map(Self).map_err(|e| JsError::new(&format!(\"from_bytes: {{}}\", e)))",
                                cli.common_import_wasm()));
                    }
                }
                if cli.json_serde_derives {
                    let mut to_json = codegen::Function::new("to_json");
                    to_json
                        .ret("Result<String, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde_json::to_string_pretty(&self.0).map_err(|e| JsError::new(&format!(\"to_json: {}\", e)))");
                    s_impl.push_fn(to_json);
                    let mut to_json_value = codegen::Function::new("to_json_value");
                    to_json_value
                        .ret("Result<JsValue, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde::Serialize::serialize(&self.0, &serde_wasm_bindgen::Serializer::json_compatible()).map_err(|e| JsError::new(&format!(\"to_js_value: {}\", e)))");
                    s_impl.push_fn(to_json_value);
                    s_impl
                        .new_fn("from_json")
                        .ret(format!("Result<{name}, JsError>"))
                        .arg("json", "&str")
                        .vis("pub")
                        .line("serde_json::from_str(json).map(Self).map_err(|e| JsError::new(&format!(\"from_json: {}\", e)))");
                }
            }
        }
    }
    WasmWrapper {
        ident,
        s,
        s_impl,
        from_wasm: None,
        from_native: None,
        as_ref: None,
        macros,
    }
}

/// default_structure will have it be a DIRECT wrapper with a tuple field of rust_lib::{ident}
/// this will include generating to/from traits automatically
fn create_base_wasm_wrapper<'a>(
    gen_scope: &GenerationScope,
    types: &IntermediateTypes<'_>,
    ident: &'a RustIdent,
    default_structure: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    assert!(cli.wasm);
    let mut base = create_base_wasm_struct(gen_scope, ident, true, cli);
    if default_structure {
        let native_name = rust_crate_struct_from_wasm(types, ident, cli);
        base.s.tuple_field(None, &native_name);
        base.add_conversion_methods(&native_name, cli);
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
    writer: &str,
    cli: &Cli,
) -> (codegen::Function, codegen::Impl) {
    match create_serialize_impls(
        ident,
        rep,
        tag,
        definite_len,
        use_this_encoding,
        false,
        writer,
        cli,
    ) {
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
#[allow(clippy::too_many_arguments)]
fn create_serialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    definite_len: &str,
    use_this_encoding: Option<&str>,
    generate_serialize_embedded: bool,
    writer: &str,
    cli: &Cli,
) -> (codegen::Function, codegen::Impl, Option<codegen::Impl>) {
    if generate_serialize_embedded {
        // This is not necessarily a problem but we should investigate this case to ensure we're not calling
        // (de)serialize_as_embedded without (de)serializing the tag
        assert_eq!(tag, None);
    }
    let name = &ident.to_string();
    let ser_impl = make_serialization_impl(name, cli);
    let mut ser_func = make_serialization_function("serialize", writer, cli);
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
            cli,
        );
    }
    // TODO: do definite length encoding for optional fields too
    if let Some(rep) = rep {
        if let Some(definite) = use_this_encoding {
            start_len(
                &mut ser_func,
                rep,
                "serializer",
                definite,
                definite_len,
                cli,
            );
        } else {
            let len = cbor_event_len_n(definite_len, cli);
            match rep {
                Representation::Array => ser_func.line(format!("serializer.write_array({len})?;")),
                Representation::Map => ser_func.line(format!("serializer.write_map({len})?;")),
            };
        }
        if generate_serialize_embedded {
            if cli.preserve_encodings {
                // The embedded serialize writes only the group's contents — the entity that wrote
                // the array/map head owns the ending break (see the embedded-impl comment in
                // codegen). So the standalone serialize writes the break itself, after the contents,
                // rather than delegating it to the embedded call. Without this split an
                // indefinite-length container would double-write the break.
                ser_func.line(format!(
                    "self.serialize_as_embedded_group(serializer{})?;",
                    canonical_param(cli)
                ));
                end_len(
                    &mut ser_func,
                    "serializer",
                    use_this_encoding.expect(
                        "preserve-encodings embedded serialize: the array/map head was written from \
                         `use_this_encoding` (always Some under preserve — see the len_encoding_var \
                         caller), so its ending break must reference the same length-encoding \
                         variable; a None here would emit a free-floating `.end(serializer, ..)` on \
                         no receiver — an uncompilable generated crate",
                    ),
                    true,
                    cli,
                );
            } else {
                ser_func.line(format!(
                    "self.serialize_as_embedded_group(serializer{})",
                    canonical_param(cli)
                ));
            }
        }
    } else {
        // not array or map, generate serialize directly
        if generate_serialize_embedded {
            ser_func.line(format!(
                "self.serialize_as_embedded_group(serializer{})",
                canonical_param(cli)
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
fn add_deserialize_initial_len_check(
    deser_body: &mut dyn CodeBlock,
    len_info: RustStructCBORLen,
    cli: &Cli,
) {
    deser_body.line(&format!(
        "let mut read_len = {}(len);",
        cbor_read_len_ctor(cli)
    ));
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
            // We MUST check even in the fixed case, as you might be parsing something that
            // is a CBOR prefix field-wise to your data e.g.:
            //   foo = [uint, bytes]
            //   bar = [uint, bytes, str]
            // would have any bar be parsable as foo (problematic when we have foo / bar in a choice)
            // so we must ensure we end up with precisely 0 left over at the end even in fixed cases.
            // We do the check right away instead of waiting. We don't do this inside of
            // add_deserialize_final_len_check for all variants as some enum use-cases
            // break as they rely on being able to do the final check without read_len
            deser_body.line("read_len.finish()?;");
        }
    }
}

// Adds final Len check if not fixed + reads for the ending Special::Break for Indefinite arrays
fn add_deserialize_final_len_check(
    deser_body: &mut dyn CodeBlock,
    rep: Option<Representation>,
    len_info: RustStructCBORLen,
    cli: &Cli,
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
            end_len_check.line(format!(
                "{} => {},",
                cbor_event_len_n("_", cli),
                ending_check
            ));
            let mut indefinite_check = Block::new(format!(
                "{} => match raw.special()?",
                cbor_event_len_indef(cli)
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
#[allow(clippy::too_many_arguments)]
fn create_deserialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    len_info: Option<RustStructCBORLen>,
    generate_deserialize_embedded: bool,
    store_encoding: Option<&str>,
    deser_body: &mut dyn CodeBlock,
    cli: &Cli,
) -> (codegen::Impl, Option<codegen::Impl>) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Custom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    if let Some(tag) = tag {
        if cli.preserve_encodings {
            deser_body.line("let (tag, tag_encoding) = raw.tag_sz()?;");
        } else {
            deser_body.line("let tag = raw.tag()?;");
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        tag_check.line(format!("return Err(DeserializeError::new(\"{name}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        deser_body.push_block(tag_check);
    }
    if let Some(rep) = rep {
        match rep {
            Representation::Array => {
                if cli.preserve_encodings {
                    deser_body.line("let len = raw.array_sz()?;");
                } else {
                    deser_body.line("let len = raw.array()?;");
                }
                if !generate_deserialize_embedded && let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!(
                        "let {encoding_var_name}: LenEncoding = len.into();"
                    ));
                }
                if let Some(len_info) = len_info {
                    add_deserialize_initial_len_check(deser_body, len_info, cli);
                }
                if generate_deserialize_embedded {
                    deser_body.line(
                        "let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);",
                    );
                }
            }
            Representation::Map => {
                if cli.preserve_encodings {
                    deser_body.line("let len = raw.map_sz()?;");
                } else {
                    deser_body.line("let len = raw.map()?;");
                }
                if !generate_deserialize_embedded && let Some(encoding_var_name) = store_encoding {
                    deser_body.line(&format!(
                        "let {encoding_var_name}: LenEncoding = len.into();"
                    ));
                }
                if let Some(len_info) = len_info {
                    add_deserialize_initial_len_check(deser_body, len_info, cli);
                }
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
        if let Some(len_info) = len_info {
            add_deserialize_final_len_check(deser_body, rep, len_info, cli);
        }
        deser_body.line("ret");
        let mut embedded_impl = codegen::Impl::new(name);
        embedded_impl.impl_trait("DeserializeEmbeddedGroup");
        Some(embedded_impl)
    } else {
        None
    };
    (deser_impl, deser_embedded_impl)
}

// We need to execute field deserialization inside a closure in order to capture and annotate with the field name
// without having to put error annotation inside of every single cbor_event call.
fn make_err_annotate_block(annotation: &str, before: &str, after: &str) -> Block {
    let mut if_block = Block::new(format!("{before}(|| -> Result<_, DeserializeError>"));
    if_block.after(format!(
        ")().map_err(|e| e.annotate(\"{annotation}\")){after}"
    ));
    if_block
}

fn make_deser_loop(len_var: &str, len_expr: &str, cli: &Cli) -> Block {
    Block::new(format!(
        "while match {} {{ {} => {} < n, {} => true, }}",
        len_var,
        cbor_event_len_n("n", cli),
        len_expr,
        cbor_event_len_indef(cli)
    ))
}

fn make_deser_loop_break_check(len_var: &str, cli: &Cli) -> Block {
    // Only INDEFINITE-length collections carry a break byte (`0xff`). For a definite length the loop
    // reads exactly `n` items, so there is nothing to detect here — and we must NOT peek: the break
    // byte shares major type 7 (Special) with bool / null / float16-32-64 / simple, so an ungated
    // Special check would eat a definite-length special element/key.
    //
    // In the indefinite arm we detect the break with `Deserializer::special_break()`: a
    // NON-consuming probe that advances past the `0xff` break iff that's the next byte, and returns
    // `false` WITHOUT advancing on any other Special (a bool/null/float element or key), which then
    // falls through to the element/key deserializer and reads normally. This is why the whole prior
    // "indefinite container of value-specials" limitation is gone — a non-break special is no longer
    // consumed-and-rejected. `special_break` sits in the same `impl<R: BufRead>` block as
    // `cbor_type()`, which this check already calls inside the reader-type-erased type-choice
    // deserializer closures (`|raw: &mut Deserializer<_>|`), so it carries no new bound and no E0282
    // risk. The `cbor_type` guard stays load-bearing: `special_break` errors on non-Special input.
    let mut indef = Block::new(format!("if let {} = {len_var}", cbor_event_len_indef(cli)));
    let mut brk =
        Block::new("if raw.cbor_type()? == cbor_event::Type::Special && raw.special_break()?");
    brk.line("break;");
    indef.push_block(brk);
    indef
}

pub fn table_type(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "OrderedHashMap"
    } else {
        "BTreeMap"
    }
}

/// Mint the JS-visible class for a table shape whose SOLE owner is the named rule `owner`, plus a
/// `pub type <structural> = <owner>;` alias so structural-name reference sites (an anonymous `Map`'s
/// `for_wasm_member`, `@newtype` inner getters, cross-module `mark_refs` imports) still resolve —
/// wasm_bindgen exports no type aliases, so it folds the alias onto the `owner` class in the JS ABI.
/// Idempotent via `generated` (which records BOTH the rule name and the structural name), so the
/// visit arm and the Table arm converge to identical output regardless of which reaches the shape
/// first. The class body always derives from the OWNER's declared `(domain, range)`, keeping the
/// output iteration-order-independent.
fn mint_sole_owner_table(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    owner: &RustIdent,
    structural_ident: &RustIdent,
    generated: &mut BTreeSet<String>,
    cli: &Cli,
) {
    if generated.insert(owner.to_string()) {
        let (domain, range, tag) = {
            let owner_struct = types
                .rust_structs()
                .get(owner)
                .expect("sole owner of a table shape must be a rust struct");
            match owner_struct.variant() {
                RustStructType::Table { domain, range } => {
                    (domain.clone(), range.clone(), owner_struct.tag())
                }
                _ => unreachable!("sole owner of a table shape must be a Table rust struct"),
            }
        };
        // `exists_in_rust = true`: the inner is the rust crate's `pub type <owner>` alias (exactly the
        // struct-field role's inner), not the raw inline map.
        codegen_table_type(gen_scope, types, owner, domain, range, tag, true, cli);
    }
    // Structural alias in the SAME module as the class (`owner`'s scope). Skip a self-alias when the
    // rule ident already equals the structural name.
    if *structural_ident != *owner && generated.insert(structural_ident.to_string()) {
        gen_scope
            .wasm(types, owner)
            .push_type_alias(TypeAlias::new(structural_ident, owner).vis("pub").clone());
    }
}

#[allow(clippy::too_many_arguments)]
fn codegen_table_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    key_type: RustType,
    value_type: RustType,
    tag: Option<usize>,
    exists_in_rust: bool,
    cli: &Cli,
) {
    assert!(cli.wasm);
    assert!(
        tag.is_none(),
        "TODO: why is this not used anymore? is it since it's only on the wasm side now so it shouldn't happen now?"
    );
    // Special-class (major type 7) keys used to be asserted away here, but the break-byte
    // ambiguity they alluded to lives in the rust-side deserialize loop, which
    // `make_deser_loop_break_check` now handles (definite lengths read exactly `n` entries; the
    // indefinite case errors gracefully). This wasm wrapper emits only accessors — nothing here
    // depends on the key's CBOR class.
    let mut wrapper = create_base_wasm_struct(gen_scope, name, false, cli);

    let inner_type = if exists_in_rust {
        rust_crate_struct_from_wasm(types, name, cli)
    } else {
        ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli)
    };
    wrapper.s.tuple_field(None, &inner_type);
    // new
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self")
        .line(format!("Self({}::new())", table_type(cli)));
    wrapper.s_impl.push_fn(new_func);
    // len
    wrapper
        .s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    // A nullable value (`* uint => (T / null)` -> `Option<T>`) would make get/insert return
    // `Option<Option<T>>` — which wasm-bindgen can't represent (`Option<T>: OptionIntoWasmAbi` is not
    // satisfied). So when the value is itself an `Option`, we flatten the presence-`Option` these
    // accessors add into it and return a single `Option<T>`. This is the same convention the c-style
    // enum-getter (`add_wasm_enum_getters`) uses; native storage still holds all three states
    // (key-absent / present-null / present-value), so CBOR round-trips are unaffected — only the wasm
    // read conflates absent with present-null.
    let value_nullable = matches!(
        value_type.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    );
    let map_value_ret = || {
        if value_nullable {
            value_type.for_wasm_return(types)
        } else {
            format!("Option<{}>", value_type.for_wasm_return(types))
        }
    };
    let value_flatten = if value_nullable { ".flatten()" } else { "" };
    // When the value is nullable, the stored inner is `Option<InnerRust>`. If that inner is not
    // directly wasm-exposable (a named collection / data-enum), the boundary must convert it —
    // `.map(Into::into)` through the Option — not a blanket `.into()`, which has no
    // `From<Option<Inner>>` impl (wasm E0277/E0308).
    let value_nullable_inner_exposable = match value_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Optional(inner) => inner.conceptual_type.directly_wasm_exposable(types),
        _ => false,
    };
    // insert
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm_param(types))
        .arg("value", value_type.for_wasm_param(types))
        .ret(map_value_ret());
    if value_nullable {
        insert_func.doc("Returns the displaced value, or None if the key was absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    insert_func.line(format!(
        "self.0.insert({}, {}){}",
        ToWasmBoundaryOperations::format(
            key_type
                .from_wasm_boundary_clone(types, "key", false)
                .into_iter()
        ),
        ToWasmBoundaryOperations::format(
            value_type
                .from_wasm_boundary_clone(types, "value", false)
                .into_iter()
        ),
        if value_nullable {
            if value_nullable_inner_exposable {
                value_flatten.to_owned()
            } else {
                // displaced value is `Option<InnerRust>` after flatten; convert its inner to wasm.
                format!("{value_flatten}.map(Into::into)")
            }
        } else if value_type.directly_wasm_exposable(types) {
            String::new()
        } else {
            ".map(Into::into)".to_owned()
        }
    ));
    // ^ TODO: support failable types everywhere or just force it to be only a detail in the wrapper?
    wrapper.s_impl.push_fn(insert_func);
    // get
    let get_ret_modifier = if value_type.is_copy(types) {
        ""
    } else if value_nullable {
        // stored value is `Option<InnerRust>`; convert the inner across the boundary (when it is
        // not directly exposable) THROUGH the Option, yielding `Option<Option<Wrapper>>` which the
        // trailing `value_flatten` collapses to `Option<Wrapper>`.
        if value_nullable_inner_exposable {
            ".map(|v| v.clone())"
        } else {
            ".map(|v| v.clone().map(Into::into))"
        }
    } else if value_type.directly_wasm_exposable(types) {
        ".map(|v| v.clone())"
    } else {
        ".map(|v| v.clone().into())"
    };
    let mut getter = codegen::Function::new("get");
    getter
        .arg_ref_self()
        .arg("key", key_type.for_wasm_param(types))
        .ret(map_value_ret())
        .vis("pub");
    if value_nullable {
        getter.doc("Returns None if the key is absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    if key_type.directly_wasm_exposable(types) {
        getter.line(format!(
            "self.0.get({}){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            if value_type.is_copy(types) {
                ".copied()"
            } else {
                get_ret_modifier
            },
            value_flatten
        ));
    } else {
        getter.line(format!(
            "self.0.get({}.as_ref()){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            if value_type.is_copy(types) {
                ".copied()"
            } else {
                get_ret_modifier
            },
            value_flatten
        ));
    }
    wrapper.s_impl.push_fn(getter);
    // has(key): key-presence accessor, emitted from exactly the `value_nullable` flatten condition
    // above (single source of truth) so it can never drift from `get`. When the value is nullable,
    // `get` collapses Option<Option<T>> -> Option<T>, so a `None` return conflates an absent key with
    // a present-but-null one; `has` exposes the key's presence directly (a direct key lookup, not the
    // `keys()` scan that was the only recovery before). Mirrors `get`'s key-boundary handling.
    //
    // No collision check is needed here (unlike the record `has_<field>` accessor): a table wrapper's
    // method surface is entirely generator-fixed (`len`/`insert`/`get`/`has`/`keys`) with no
    // user-named methods — a map has no named fields, only key/value TYPES — so `has` cannot clash
    // with anything the spec author controls.
    if value_nullable {
        let mut has_func = codegen::Function::new("has");
        has_func
            .arg_ref_self()
            .arg("key", key_type.for_wasm_param(types))
            .ret("bool")
            .vis("pub")
            .doc("Returns whether the key is present, distinguishing an absent key from a present-but-null value (both of which `get` reports as None).");
        if key_type.directly_wasm_exposable(types) {
            has_func.line(format!(
                "self.0.get({}).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        } else {
            has_func.line(format!(
                "self.0.get({}.as_ref()).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        }
        wrapper.s_impl.push_fn(has_func);
    }
    // keys
    let keys_type = ConceptualRustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys.arg_ref_self()
        .ret(keys_type.for_wasm_return(types))
        .vis("pub");
    if keys_type.directly_wasm_exposable(types) {
        let key_clone = if key_type.is_copy(types) {
            ".keys().copied()"
        } else {
            ".keys().cloned()"
        };
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>()"));
    } else {
        keys.line(format!(
            "{}(self.0.iter().map(|(k, _v)| k.clone()).collect::<Vec<_>>())",
            keys_type.for_wasm_return(types)
        ));
    }
    wrapper.s_impl.push_fn(keys);
    wrapper.add_conversion_methods(&inner_type, cli);
    wrapper.push(gen_scope, types);
}

#[derive(Debug)]
struct EncodingField {
    field_name: String,
    type_name: String,
    /// this MUST be equivalent to the Default trait of the encoding field.
    /// This can be more concise though e.g. None for Option<T>::default()
    default_expr: &'static str,
    enc_conversion_before: &'static str,
    enc_conversion_after: &'static str,
    is_copy: bool,
    /// inner encodings - used for map/vec types
    #[allow(unused)]
    inner: Vec<EncodingField>,
}

impl EncodingField {
    pub fn enc_conversion(&self, expr: &str) -> String {
        format!(
            "{}{}{}",
            self.enc_conversion_before, expr, self.enc_conversion_after
        )
    }
}

fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    match key {
        FixedValue::Text(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            enc_conversion_before: "StringEncoding::from(",
            enc_conversion_after: ")",
            is_copy: false,
            inner: Vec::new(),
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            enc_conversion_before: "Some(",
            enc_conversion_after: ")",
            is_copy: true,
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
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    // TODO: how do we handle defaults for nested things? e.g. inside of a ConceptualRustType::Map
    let mut encs = encoding_fields_impl(types, name, ty.into(), cli);
    if include_default && ty.config.default.is_some() {
        encs.push(EncodingField {
            field_name: format!("{name}_default_present"),
            type_name: "bool".to_owned(),
            default_expr: "false",
            enc_conversion_before: "",
            enc_conversion_after: "",
            is_copy: true,
            inner: Vec::new(),
        });
    }
    encs
}

fn encoding_fields_impl(
    types: &IntermediateTypes,
    name: &str,
    ty: SerializingRustType,
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    match ty {
        SerializingRustType::Root(ConceptualRustType::Array(elem_ty), _cfg) => {
            let base = EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
                inner: Vec::new(),
            };
            let inner_encs =
                encoding_fields_impl(types, &format!("{name}_elem"), (&**elem_ty).into(), cli);
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
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: false,
                        inner: inner_encs,
                    },
                ]
            }
        }
        SerializingRustType::Root(ConceptualRustType::Map(k, v), _cfg) => {
            let mut encs = vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
                inner: Vec::new(),
            }];
            let key_encs = encoding_fields_impl(types, &format!("{name}_key"), (&**k).into(), cli);
            let val_encs =
                encoding_fields_impl(types, &format!("{name}_value"), (&**v).into(), cli);

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
                        k.for_rust_member(types, false, cli),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
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
                        k.for_rust_member(types, false, cli),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
                    inner: val_encs,
                });
            }
            encs
        }
        SerializingRustType::Root(ConceptualRustType::Primitive(p), _cfg) => match p {
            Primitive::Bytes | Primitive::Str => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "StringEncoding".to_owned(),
                default_expr: "StringEncoding::default()",
                enc_conversion_before: "StringEncoding::from(",
                enc_conversion_after: ")",
                is_copy: false,
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
                enc_conversion_before: "Some(",
                enc_conversion_after: ")",
                is_copy: true,
                inner: Vec::new(),
            }],
            Primitive::Bool =>
            /* bool only has 1 encoding */
            {
                vec![]
            }
        },
        SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => match f {
            FixedValue::Bool(_) | FixedValue::Null => vec![],
            FixedValue::Nint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::I64)).into(),
                cli,
            ),
            FixedValue::Uint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::U64)).into(),
                cli,
            ),
            FixedValue::Float(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::F64)).into(),
                cli,
            ),
            FixedValue::Text(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Str)).into(),
                cli,
            ),
        },
        SerializingRustType::Root(ConceptualRustType::Alias(_, ty), _cfg) => {
            encoding_fields_impl(types, name, (&**ty).into(), cli)
        }
        SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
            encoding_fields(types, name, ty, false, cli)
        }
        SerializingRustType::Root(ConceptualRustType::Rust(rust_ident), _cfg) => {
            match &types.rust_struct(rust_ident).unwrap().variant() {
                // for c-style enums we push those up to where they are used instead of self-containing
                RustStructType::CStyleEnum { variants } => {
                    // earlier we are guaranteed that all variants will have the same encoding types
                    // or else it wouldn't end up as a c-style enum in the first place in IntermediateTypes
                    encoding_fields(types, name, variants[0].rust_type(), false, cli)
                }
                // also push them out for RawBytesType as they're not stored there, as if we had `bytes` directly here
                RustStructType::RawBytesType => encoding_fields_impl(
                    types,
                    name,
                    (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                    cli,
                ),
                // no encodings here. they're contained inside the struct
                _ => vec![],
            }
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_tag"),
                (&ConceptualRustType::Fixed(FixedValue::Uint(*tag))).into(),
                cli,
            );
            encs.append(&mut encoding_fields_impl(types, name, *child, cli));
            encs
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_bytes"),
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                cli,
            );
            encs.append(&mut encoding_fields_impl(types, name, *child, cli));
            encs
        }
    }
}

fn encoding_var_names_str(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    cli: &Cli,
) -> String {
    assert!(cli.preserve_encodings);
    let resolved_rust_type = rust_type.clone().resolve_aliases();
    let mut var_names = if resolved_rust_type.is_fixed_value() {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in encoding_fields(types, field_name, &resolved_rust_type, false, cli).into_iter() {
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

// generates serialization code for an array-encoded record into ser_func EXCEPT FOR array length
fn generate_array_struct_serialization(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    record: &RustRecord,
    vars_in_self: bool,
    ser_func: &mut dyn CodeBlock,
    cli: &Cli,
) {
    assert_eq!(record.rep, Representation::Array);
    let opt_self = if vars_in_self { "self." } else { "" };
    for field in record.fields.iter() {
        let field_expr = format!("{}{}", opt_self, field.name);
        if field.optional {
            if field.rust_type.is_fixed_value() && !cli.preserve_encodings {
                // we just want to skip this entirely if we aren't remembering enecodings
                continue;
            }
            let (optional_field_check, field_expr, expr_is_ref) = if let Some(default_value) =
                &field.rust_type.config.default
            {
                (
                    if cli.preserve_encodings {
                        if vars_in_self {
                            format!(
                                "if {} != {} || self.encodings.map(|encs| encs.{}_default_present).unwrap_or(false)",
                                field_expr,
                                default_value.to_primitive_str_compare(),
                                field.name
                            )
                        } else {
                            format!(
                                "if {} != {} || {}_default_present",
                                field_expr,
                                default_value.to_primitive_str_compare(),
                                field.name
                            )
                        }
                    } else {
                        format!(
                            "if {}{} != {}",
                            opt_self,
                            field.name,
                            default_value.to_primitive_str_compare()
                        )
                    },
                    field_expr.as_str(),
                    !vars_in_self,
                )
            } else {
                (
                    if vars_in_self {
                        format!("if let Some(field) = &self.{}", field.name)
                    } else {
                        format!("if let Some(field) = {}", field.name)
                    },
                    "field",
                    true,
                )
            };
            let mut optional_array_ser_block = Block::new(optional_field_check);
            let mut config = SerializeConfig::new(field_expr, &field.name).expr_is_ref(expr_is_ref);
            if let Some(custom_serialize) = &field.rule_metadata.custom_serialize {
                config = config.custom_serialize(custom_serialize.clone());
            }
            if vars_in_self {
                config = config.encoding_var_in_option_struct("self.encodings")
            } else {
                config = config.expr_is_ref(true).encoding_var_is_ref(true)
            }
            gen_scope.generate_serialize(
                types,
                (&field.rust_type).into(),
                &mut optional_array_ser_block,
                config,
                cli,
            );
            ser_func.push_block(optional_array_ser_block);
        } else {
            let mut config = SerializeConfig::new(&field_expr, &field.name);
            if let Some(custom_serialize) = &field.rule_metadata.custom_serialize {
                config = config.custom_serialize(custom_serialize.clone());
            }
            if vars_in_self {
                config = config.encoding_var_in_option_struct("self.encodings")
            } else {
                config = config.expr_is_ref(true).encoding_var_is_ref(true)
            }
            gen_scope.generate_serialize(types, (&field.rust_type).into(), ser_func, config, cli);
        }
    }
}

#[derive(Default, Debug)]
struct ArrayStructDeserializeCode {
    deser_code: DeserializationCode,
    // (var, expr)
    deser_ctor_fields: Vec<(String, String)>,
    // (var, expr)
    encoding_struct_ctor_fields: Vec<(String, String)>,
}

// generates deserialization code for an array-encoded record into deser_code EXCEPT FOR:
// 1) any final length check (so it can be used for generating embedded deserialization impls)
// 2) the final constructor block is not added to deser_code but has the vars/exprs returned in two vectors:
//    i) all root-level vars/exprs
//    ii) if Some, all vars/exprs that need to be put inside of an *Encodings struct's constructor
// so you will need to construct the constructor expression from these
#[allow(clippy::too_many_arguments)]
fn generate_array_struct_deserialization(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    record: &RustRecord,
    tag: Option<usize>,
    in_embedded: bool,
    vars_in_self: bool,
    cli: &Cli,
) -> ArrayStructDeserializeCode {
    assert_eq!(record.rep, Representation::Array);
    let mut deser_code = DeserializationCode::default();
    let mut deser_ctor_fields = vec![];
    let mut encoding_struct_ctor_fields = vec![];
    for (field_index, field) in record.fields.iter().enumerate() {
        let (before, after) = if cli.preserve_encodings {
            let var_names_str = encoding_var_names_str(types, &field.name, &field.rust_type, cli);
            if cli.annotate_fields {
                (
                    Cow::from(format!("let {var_names_str} = ")),
                    Cow::from("?;"),
                )
            } else {
                (Cow::from(format!("let {var_names_str} = ")), Cow::from(";"))
            }
        } else if field.rust_type.is_fixed_value() {
            // don't set anything, only verify data
            if cli.annotate_fields {
                (Cow::from(""), Cow::from("?;"))
            } else {
                (Cow::from(""), Cow::from(""))
            }
        } else if cli.annotate_fields {
            (Cow::from(format!("let {} = ", field.name)), Cow::from("?;"))
        } else {
            (Cow::from(format!("let {} = ", field.name)), Cow::from(";"))
        };
        if field.optional {
            // we can support optional fields, but only when they're immediately non-ambiguous
            // i.e. when the next type (possibly skipping subsequent optional fields)
            // is different from the current type.
            // Supporting the general case 100% is extremely complicated without a combinatorial
            // backtrack but for most sane real-world cases this wouldn't be necessary.
            // Think purposefully written edge-cases with multiple optional fields, possibly nested
            // in other structs, and with many of the same types.
            // e.g. [ ? uint, uint, ? (uint, text), ? text]
            let field_cbor_types = field.rust_type.cbor_types(types);
            let mut possibly_last_field = true;
            for i in (field_index + 1)..record.fields.len() {
                if record.fields[i]
                    .rust_type
                    .cbor_types(types)
                    .iter()
                    .any(|ct| field_cbor_types.contains(ct))
                {
                    gen_scope.dont_generate_deserialize(
                        name,
                        format!(
                            "Array struct with potentially-ambiguous optional field {}: {:?}",
                            field.name, field.rust_type,
                        ),
                    );
                }
                if !record.fields[i].optional {
                    if i < record.fields.len() - 1 {
                        possibly_last_field = false;
                    }
                    break;
                }
            }
            // we also need to be careful if we're possibly the last field in the CBOR
            // buffer to avoid raw.cbor_type()? throwing an error for CBOR(NotEnough(0, 0))
            let type_check_cond = if field_cbor_types.len() == 1 {
                let type_str = cbor_type_code_str(field_cbor_types[0]);
                if possibly_last_field {
                    // We also need to be careful if the last one is a non-Break special
                    // and the array is encoded using indefinite encoding.
                    // There's no nice way to access this as Deserializer::special_break() consumes
                    // the byte so we'll just inline this ugly code instead
                    if field_cbor_types.contains(&cbor_event::Type::Special) {
                        "if raw.as_mut_ref().fill_buf().ok().and_then(|buf| buf.get(0)).map(|byte: &u8| cbor_event::Type::from(*byte) == cbor_event::Type::Special && (*byte & 0b0001_1111) != 0x1f).unwrap_or(false)".to_owned()
                    } else {
                        format!("if raw.cbor_type().map(|ty| ty == {type_str}).unwrap_or(false)")
                    }
                } else {
                    format!("if raw.cbor_type()? == {type_str}")
                }
            } else {
                let types_str = field_cbor_types
                    .iter()
                    .map(|ty| cbor_type_code_str(*ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                if possibly_last_field {
                    // We also need to be careful if the last one is a non-Break special
                    // and the array is encoded using indefinite encoding.
                    // There's no nice way to access this as Deserializer::special_break() consumes
                    // the byte so we'll just inline this ugly code instead
                    if field_cbor_types.contains(&cbor_event::Type::Special) {
                        format!(
                            "if raw.as_mut_ref().fill_buf().ok().and_then(|buf| buf.get(0)).map(|byte: &u8| vec![{types_str}].contains(&cbor_event::Type::from(*byte)) && (*byte & 0b0001_1111) != 0x1f).unwrap_or(false)",
                        )
                    } else {
                        format!(
                            "if raw.cbor_type().map(|ty| vec![{types_str}].contains(&ty)).unwrap_or(false)"
                        )
                    }
                } else {
                    format!("if vec![{types_str}].contains(&raw.cbor_type()?)")
                }
            };
            let type_check_block = Block::new(format!("{before}{type_check_cond}"));
            let mut type_check_else = Block::new("else");
            if cli.annotate_fields {
                let enc_fields = if cli.preserve_encodings {
                    let resolved_rust_type = field.rust_type.clone().resolve_aliases();
                    assert!(
                        !resolved_rust_type.is_fixed_value(),
                        "https://github.com/dcSpark/cddl-codegen/issues/205"
                    );
                    encoding_fields(types, &field.name, &resolved_rust_type, false, cli)
                } else {
                    vec![]
                };
                let (some_map, defaults) = if !enc_fields.is_empty() {
                    let enc_names_str = enc_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect::<Vec<String>>()
                        .join(", ");
                    (
                        Cow::from(format!(
                            "|({}, {})| (Some({}), {})",
                            field.name, enc_names_str, field.name, enc_names_str
                        )),
                        Cow::from(format!(
                            "(None, {})",
                            enc_fields
                                .iter()
                                .map(|enc| enc.default_expr.to_owned())
                                .collect::<Vec<String>>()
                                .join(", ")
                        )),
                    )
                } else {
                    (Cow::from("Some"), Cow::from("None"))
                };
                let mut deser_config = DeserializeConfig::new(&field.name)
                    .in_embedded(in_embedded)
                    .optional_field(true);
                if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
                    deser_config = deser_config.custom_deserialize(custom_deserialize.clone());
                }
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new("", "", true),
                        deser_config,
                        cli,
                    )
                    .annotate(&field.name, "", &format!(".map({some_map})"))
                    .wrap_in_block(type_check_block)
                    .add_to_code(&mut deser_code);
                type_check_else.line(format!("Ok({defaults})"));
            } else {
                let mut deser_config = DeserializeConfig::new(&field.name)
                    .in_embedded(in_embedded)
                    .optional_field(true);
                if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
                    deser_config = deser_config.custom_deserialize(custom_deserialize.clone());
                }
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new("Some(", ")", false),
                        deser_config,
                        cli,
                    )
                    .wrap_in_block(type_check_block)
                    .add_to_code(&mut deser_code);
                type_check_else.line("None");
            }
            type_check_else.after(after);
            deser_code.content.push_block(type_check_else);
        } else {
            // mandatory fields
            if cli.annotate_fields {
                let mut deser_config = DeserializeConfig::new(&field.name).in_embedded(in_embedded);
                if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
                    deser_config = deser_config.custom_deserialize(custom_deserialize.clone());
                }
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new("", "", true),
                        deser_config,
                        cli,
                    )
                    .annotate(&field.name, before.as_ref(), after.as_ref())
                    .add_to_code(&mut deser_code);
            } else {
                let mut deser_config = DeserializeConfig::new(&field.name).in_embedded(in_embedded);
                if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
                    deser_config = deser_config.custom_deserialize(custom_deserialize.clone());
                }
                gen_scope
                    .generate_deserialize(
                        types,
                        (&field.rust_type).into(),
                        DeserializeBeforeAfter::new(before.as_ref(), after.as_ref(), false),
                        deser_config,
                        cli,
                    )
                    .add_to_code(&mut deser_code);
            }
        }
        if !field.rust_type.is_fixed_value() {
            deser_ctor_fields.push((field.name.clone(), field.name.clone()));
        }
    }
    if cli.preserve_encodings {
        let encoding_vars_output = if vars_in_self {
            &mut encoding_struct_ctor_fields
        } else {
            // no explicit encoding struct - dump in with other regular fields
            &mut deser_ctor_fields
        };
        encoding_vars_output.push(("len_encoding".to_owned(), "len_encoding".to_owned()));
        if tag.is_some() {
            encoding_vars_output.push(("tag_encoding".to_owned(), "Some(tag_encoding)".to_owned()));
        }
        for field in record.fields.iter() {
            for field_enc in encoding_fields(
                types,
                &field.name,
                &field.rust_type.clone().resolve_aliases(),
                true,
                cli,
            ) {
                encoding_vars_output
                    .push((field_enc.field_name.clone(), field_enc.field_name.clone()));
            }
        }
    }
    // length checked inside of deserialize() - it causes problems for plain groups nested
    // in other groups otherwise
    ArrayStructDeserializeCode {
        deser_code,
        deser_ctor_fields,
        encoding_struct_ctor_fields,
    }
}

fn codegen_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    tag: Option<usize>,
    record: &RustRecord,
    config: &RustStructConfig,
    cli: &Cli,
) {
    // NOTE: mirrored by emit_tests::record_ctor_can_fail — keep the two in sync
    let new_can_fail = record
        .fields
        .iter()
        .any(|f| !f.optional && f.rust_type.has_value_bounds());
    // wasm wrapper
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        if new_can_fail {
            wasm_new.ret(format!("Result<{name}, JsError>"));
        } else {
            wasm_new.ret("Self");
        }
        wasm_new.vis("pub");
        let mut wasm_new_args = Vec::new();
        let mut wasm_new_comments = Vec::new();
        for field in &record.fields {
            // Fixed values don't need constructors or getters or fields in the rust code
            if !field.rust_type.is_fixed_value() {
                if field.optional {
                    // setter
                    let mut setter = codegen::Function::new(format!("set_{}", field.name));
                    setter
                        .arg_mut_self()
                        .arg(&field.name, field.rust_type.for_wasm_param(types))
                        .vis("pub");
                    // don't call needs_bounds_check_if_inlined() since if it's a RustType it's checked during that ctor
                    if field.rust_type.has_value_bounds() {
                        setter.ret("Result<(), JsError>");
                        if let Some(line) =
                            value_bounds_check_line(&field.rust_type, &field.name, true)
                        {
                            setter.line(&line);
                        }
                    }
                    if field.rust_type.config.default.is_some() {
                        setter.line(format!(
                            "self.0.{} = {}",
                            field.name,
                            ToWasmBoundaryOperations::format(
                                field
                                    .rust_type
                                    .from_wasm_boundary_clone(types, &field.name, false)
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
                                    .from_wasm_boundary_clone(types, &field.name, false)
                                    .into_iter()
                            )
                        ));
                    }

                    wrapper.s_impl.push_fn(setter);
                    // getter
                    // Set true iff the getter takes the flatten path below (nullable optional field
                    // stored as Option<Option<T>>). This is the single source of truth for "this
                    // position is lossy", so the `has_<field>` presence accessor emitted after the
                    // getter can never drift from the flatten emission.
                    let mut field_getter_flattens = false;
                    let mut getter = codegen::Function::new(&field.name);
                    getter.arg_ref_self().vis("pub");
                    if field.rust_type.config.default.is_some() {
                        getter.ret(field.rust_type.for_wasm_return(types)).line(
                            field.rust_type.to_wasm_boundary(
                                types,
                                &format!("self.0.{}", field.name),
                                false,
                            ),
                        );
                    } else if matches!(
                        field.rust_type.conceptual_type.resolve_alias_shallow(),
                        ConceptualRustType::Optional(_)
                    ) {
                        // A nullable optional field is stored as `Option<Option<T>>`, which
                        // wasm-bindgen can't return. Flatten the presence-`Option` into the value's
                        // `Option` and return a single `Option<T>` (same convention as the map
                        // accessors / c-style enum getters). Native storage keeps all three states
                        // (absent / present-null / present-value), so CBOR round-trips are unaffected —
                        // only the wasm read conflates absent with present-null.
                        field_getter_flattens = true;
                        getter
                            .doc("Returns None if the field is absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).")
                            .ret(field.rust_type.for_wasm_return(types))
                            .line(format!(
                                "self.0.{}{}.flatten()",
                                field.name,
                                if field.rust_type.is_copy(types) {
                                    ""
                                } else {
                                    ".clone()"
                                }
                            ));
                    } else {
                        getter
                            .ret(format!(
                                "Option<{}>",
                                field.rust_type.for_wasm_return(types)
                            ))
                            .line(field.rust_type.to_wasm_boundary_optional(
                                types,
                                &format!("self.0.{}", field.name),
                                false,
                            ));
                    }
                    wrapper.s_impl.push_fn(getter);
                    // Presence accessor for the flattened optional-nullable field. The getter above
                    // collapses Option<Option<T>> -> Option<T> (absent and present-null both read
                    // None); `has_<field>()` exposes the outer presence so a JS consumer can tell the
                    // three states apart. Gated on `field_getter_flattens` — the exact flatten
                    // condition — so the accessor and the flatten can never diverge.
                    //
                    // Collision guard: the accessor name `has_<field>` is synthesized, so a sibling
                    // field literally named `has_<field>` (whose own wasm getter is `pub fn
                    // has_<field>`) would make two identically-named methods in one impl —
                    // non-compiling (E0592/E0201) for an otherwise-valid spec. On a clash we SKIP the
                    // disambiguator loudly rather than invent a rename: the flattening getter still
                    // works, only the three-state distinguisher is lost. The wasm getter surface of a
                    // record is exactly one method per non-fixed-value field, named `field.name`, so a
                    // clash is exactly `has_<field>` appearing as a sibling field name.
                    if field_getter_flattens {
                        let has_name = format!("has_{}", field.name);
                        let collides = record
                            .fields
                            .iter()
                            .filter(|f| !f.rust_type.is_fixed_value())
                            .any(|f| f.name == has_name);
                        if collides {
                            eprintln!(
                                "cddl-codegen --wasm: {name}: presence accessor `{has_name}()` for \
                                 optional-nullable field `{}` collides with a sibling field of the \
                                 same name — skipping the accessor (the flattening getter still \
                                 works; the absent-vs-present-null distinction is lost for this field)",
                                field.name
                            );
                        } else {
                            let mut has_field = codegen::Function::new(&has_name);
                            has_field
                                .arg_ref_self()
                                .vis("pub")
                                .ret("bool")
                                .doc("Returns whether the optional field is present (outer Some), distinguishing an absent field from a present-but-null one (both of which the getter reports as None).")
                                .line(format!("self.0.{}.is_some()", field.name));
                            wrapper.s_impl.push_fn(has_field);
                        }
                    }
                } else {
                    // new
                    wasm_new.arg(&field.name, field.rust_type.for_wasm_param(types));
                    wasm_new_args.push(ToWasmBoundaryOperations::format(
                        field
                            .rust_type
                            .from_wasm_boundary_clone(types, &field.name, false)
                            .into_iter(),
                    ));
                    if let Some(comment) = &field.rule_metadata.comment {
                        wasm_new_comments.push(format!("* `{}` - {}", field.name, comment));
                    }
                    // do we want setters here later for mandatory types covered by new?
                    // getter
                    let mut getter = codegen::Function::new(&field.name);
                    getter
                        .arg_ref_self()
                        .ret(field.rust_type.for_wasm_return(types))
                        .vis("pub")
                        .line(field.rust_type.to_wasm_boundary(
                            types,
                            &format!("self.0.{}", field.name),
                            false,
                        ));
                    wrapper.s_impl.push_fn(getter);
                }
            }
        }
        if new_can_fail {
            wasm_new.line(format!(
                "{}::new({}).map(Into::into).map_err(Into::into)",
                rust_crate_struct_from_wasm(types, name, cli),
                wasm_new_args.join(", ")
            ));
        } else {
            wasm_new.line(format!(
                "Self({}::new({}))",
                rust_crate_struct_from_wasm(types, name, cli),
                wasm_new_args.join(", ")
            ));
        }
        if !wasm_new_comments.is_empty() {
            wasm_new.doc(wasm_new_comments.join("\n"));
        }
        if let Some(doc) = config.doc.as_ref() {
            wrapper.s.doc(doc);
        }
        wrapper.s_impl.push_fn(wasm_new);
        wrapper.push(gen_scope, types);
    }

    // Rust-only for the rest of this function

    // Struct (fields) + constructor
    let (mut native_struct, mut native_impl) = create_base_rust_struct(types, name, false, cli);
    native_struct.vis("pub");
    if let Some(doc) = config.doc.as_ref() {
        native_struct.doc(doc);
    }
    let mut native_new = codegen::Function::new("new");
    let (ctor_ret, ctor_before) = if new_can_fail {
        ("Result<Self, DeserializeError>", "Ok(Self")
    } else {
        ("Self", "Self")
    };
    native_new.ret(ctor_ret).vis("pub");
    let mut native_new_block = Block::new(ctor_before);
    if new_can_fail {
        native_new_block.after(")");
    }
    let mut native_new_comments = Vec::new();
    // for clippy we generate a Default impl if new has no args
    let mut new_arg_count = 0;
    for field in &record.fields {
        if !gen_scope.deserialize_generated_for_type(types, &field.rust_type.conceptual_type) {
            gen_scope.dont_generate_deserialize(
                name,
                format!(
                    "field {}: {} couldn't generate deserialize",
                    field.name,
                    field.rust_type.for_rust_member(types, false, cli)
                ),
            );
        }
        // Fixed values only exist in (de)serialization code (outside of preserve-encodings=true)
        if !field.rust_type.is_fixed_value() {
            let mut codegen_field = if let Some(default_value) = &field.rust_type.config.default {
                // new
                native_new_block.line(format!(
                    "{}: {},",
                    field.name,
                    default_value.to_primitive_str_assign()
                ));
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false, cli),
                )
            } else if field.optional {
                // new
                native_new_block.line(format!("{}: None,", field.name));
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    format!(
                        "Option<{}>",
                        field.rust_type.for_rust_member(types, false, cli)
                    ),
                )
            } else {
                // new
                native_new.arg(&field.name, field.rust_type.for_rust_move(types, cli));
                if let Some(comment) = &field.rule_metadata.comment {
                    native_new_comments.push(format!("* `{}` - {}", field.name, comment));
                }
                new_arg_count += 1;
                native_new_block.line(format!("{},", field.name));
                if let Some(line) = value_bounds_check_line(&field.rust_type, &field.name, true) {
                    native_new.line(&line);
                }
                // field
                codegen::Field::new(
                    format!("pub {}", field.name),
                    field.rust_type.for_rust_member(types, false, cli),
                )
            };
            if let Some(comment) = &field.rule_metadata.comment {
                codegen_field.doc(comment);
            }
            native_struct.push_field(codegen_field);
        }
    }
    if !native_new_comments.is_empty() {
        native_new.doc(native_new_comments.join("\n"));
    }
    let len_encoding_var = if cli.preserve_encodings {
        let encoding_name = RustIdent::new(CDDLIdent::new(format!("{name}Encoding")));
        native_struct.field(
            format!(
                "{}pub encodings",
                encoding_var_macros(types.used_as_key(name), false, cli)
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
                cli,
            ) {
                encoding_struct.field(format!("pub {}", field_enc.field_name), field_enc.type_name);
            }
            if record.rep == Representation::Map {
                let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                encoding_struct.field(format!("pub {}", key_enc.field_name), key_enc.type_name);
            }
        }

        gen_scope
            .cbor_encodings(types, name)
            .push_struct(encoding_struct);

        Some("len_encoding")
    } else {
        None
    };
    native_new.push_block(native_new_block);
    native_impl.push_fn(native_new);

    // Serialization (via rust traits) - includes Deserialization too
    if config.custom_serialize.is_none() || config.custom_deserialize.is_none() {
        let (ser_func, mut ser_impl, mut ser_embedded_impl) = create_serialize_impls(
            name,
            Some(record.rep),
            tag,
            &record.definite_info("self", false, types, cli),
            len_encoding_var
                .map(|var| {
                    format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()")
                })
                .as_deref(),
            types.is_plain_group(name),
            &gen_scope.serialize_generic,
            cli,
        );
        let mut ser_func = match ser_embedded_impl {
            Some(_) => {
                ser_impl.push_fn(ser_func);
                make_serialization_function(
                    "serialize_as_embedded_group",
                    &gen_scope.serialize_generic,
                    cli,
                )
            }
            None => ser_func,
        };
        let mut deser_code = DeserializationCode::default();
        let in_embedded = types.is_plain_group(name);
        let ctor_block = match record.rep {
            Representation::Array => {
                generate_array_struct_serialization(
                    gen_scope,
                    types,
                    record,
                    true,
                    &mut ser_func,
                    cli,
                );
                let code = generate_array_struct_deserialization(
                    gen_scope,
                    types,
                    name,
                    record,
                    tag,
                    in_embedded,
                    true,
                    cli,
                );
                code.deser_code.add_to_code(&mut deser_code);
                let mut deser_ctor = Block::new(format!("Ok({name}"));
                for (var, expr) in code.deser_ctor_fields {
                    if var == expr {
                        deser_ctor.line(format!("{var},"));
                    } else {
                        deser_ctor.line(format!("{var}: {expr},"));
                    }
                }
                if !code.encoding_struct_ctor_fields.is_empty() {
                    let mut encoding_ctor_block =
                        Block::new(format!("encodings: Some({name}Encoding"));
                    encoding_ctor_block.after("),");
                    for (var, expr) in code.encoding_struct_ctor_fields {
                        if var == expr {
                            encoding_ctor_block.line(format!("{var},"));
                        } else {
                            encoding_ctor_block.line(format!("{var}: {expr},"));
                        }
                    }
                    deser_ctor.push_block(encoding_ctor_block);
                }
                deser_ctor.after(")");
                deser_ctor
            }
            Representation::Map => {
                let mut uint_field_deserializers = Vec::new();
                let mut text_field_deserializers = Vec::new();
                // (field_index, field, content) -- this is ordered by canonical order
                let mut ser_content: Vec<(usize, &RustField, BlocksOrLines)> = Vec::new();
                if cli.preserve_encodings {
                    deser_code
                        .content
                        .line("let mut orig_deser_order = Vec::new();");
                }
                // we default to canonical ordering here as the default ordering as that should be the most useful
                // keep in mind this is always overwritten if you have cli.preserve_encodings enabled AND there was
                // a deserialized encoding, otherwise we still use this by default.
                for (field_index, field) in record.canonical_ordering() {
                    // to support maps with plain groups inside is very difficult as we cannot guarantee
                    // the order of fields so foo = {a, b, bar}, bar = (c, d) could have the order be
                    // {a, d, c, b}, {c, a, b, d}, etc which doesn't fit with the nature of deserialize_as_embedded_group
                    // A possible solution would be to take all fields into one big map, either in generation to begin with,
                    // or just for deserialization then constructing at the end with locals like a, b, bar_c, bar_d.
                    if let ConceptualRustType::Rust(ident) = &field.rust_type.conceptual_type
                        && types.is_plain_group(ident)
                    {
                        gen_scope.dont_generate_deserialize(
                            name,
                            format!(
                                "Map with plain group field {}: {}",
                                field.name,
                                field.rust_type.for_rust_member(types, false, cli)
                            ),
                        );
                    }
                    // declare variables for deser loop
                    if cli.preserve_encodings {
                        for field_enc in encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
                            true,
                            cli,
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
                        if field.optional && field.rust_type.config.default.is_none() {
                            (String::from("field"), true)
                        } else {
                            (format!("self.{}", field.name), false)
                        };

                    let key = field.key.clone().unwrap();
                    // deserialize key + value
                    let mut deser_block = match &key {
                        FixedValue::Uint(x) => {
                            if cli.preserve_encodings {
                                Block::new(format!("({x}, key_enc) => "))
                            } else {
                                Block::new(format!("{x} => "))
                            }
                        }
                        FixedValue::Text(x) => {
                            Block::new(format!("\"{}\" => ", escape_rust_str(x)))
                        }
                        _ => panic!(
                            "unsupported map key type for {}.{}: {:?}",
                            name, field.name, key
                        ),
                    };
                    deser_block.after(",");
                    let mut deser_block_code = DeserializationCode::default();
                    let key_in_rust = match &key {
                        FixedValue::Uint(x) => format!("Key::Uint({x})"),
                        FixedValue::Text(x) => {
                            format!("Key::Str(\"{}\".into())", escape_rust_str(x))
                        }
                        _ => unimplemented!(),
                    };
                    if cli.preserve_encodings {
                        let mut dup_check = if field.rust_type.is_fixed_value() {
                            Block::new(format!("if {}_present", field.name))
                        } else {
                            Block::new(format!("if {}.is_some()", field.name))
                        };
                        dup_check.line(format!(
                            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                        ));
                        deser_block_code.content.push_block(dup_check);

                        let temp_var_prefix = format!("tmp_{}", field.name);
                        let var_names_str =
                            encoding_var_names_str(types, &temp_var_prefix, &field.rust_type, cli);
                        if cli.annotate_fields {
                            let (before, after) = if var_names_str.is_empty() {
                                ("".to_owned(), "?")
                            } else {
                                (format!("let {var_names_str} = "), "?;")
                            };
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new("", "", true),
                                    deser_config,
                                    cli,
                                )
                                .annotate(&field.name, &before, after)
                                .add_to_code(&mut deser_block_code);
                        } else {
                            let (before, after) = if var_names_str.is_empty() {
                                ("".to_owned(), "")
                            } else {
                                (format!("let {var_names_str} = "), ";")
                            };
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new(&before, after, false),
                                    deser_config,
                                    cli,
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
                            cli,
                        ) {
                            deser_block_code.content.line(&format!(
                                "{} = tmp_{};",
                                enc_field.field_name, enc_field.field_name
                            ));
                        }
                    } else if field.rust_type.is_fixed_value() {
                        let mut dup_check = Block::new(format!("if {}_present", field.name));
                        dup_check.line(format!(
                            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                        ));
                        deser_block_code.content.push_block(dup_check);
                        // only does verification and sets the field_present bool to do error checking later
                        if cli.annotate_fields {
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            let mut err_deser = gen_scope.generate_deserialize(
                                types,
                                (&field.rust_type).into(),
                                DeserializeBeforeAfter::new("", "", false),
                                deser_config,
                                cli,
                            );
                            err_deser.content.line("Ok(true)");
                            err_deser
                                .annotate(&field.name, &format!("{}_present = ", field.name), "?;")
                                .add_to_code(&mut deser_block_code);
                        } else {
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new("", "", false),
                                    deser_config,
                                    cli,
                                )
                                .add_to_code(&mut deser_block_code);
                            deser_block_code
                                .content
                                .line(&format!("{}_present = true;", field.name));
                        }
                    } else {
                        let mut dup_check = Block::new(format!("if {}.is_some()", field.name));
                        dup_check.line(format!(
                            "return Err(DeserializeFailure::DuplicateKey({key_in_rust}).into());"
                        ));
                        deser_block_code.content.push_block(dup_check);
                        if cli.annotate_fields {
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new("", "", true),
                                    deser_config,
                                    cli,
                                )
                                .annotate(&field.name, &format!("{} = Some(", field.name), "?);")
                                .add_to_code(&mut deser_block_code);
                        } else {
                            let mut deser_config = DeserializeConfig::new(&field.name)
                                .in_embedded(in_embedded)
                                .optional_field(field.optional);
                            if let Some(custom_deserialize) =
                                &field.rule_metadata.custom_deserialize
                            {
                                deser_config =
                                    deser_config.custom_deserialize(custom_deserialize.clone());
                            }
                            gen_scope
                                .generate_deserialize(
                                    types,
                                    (&field.rust_type).into(),
                                    DeserializeBeforeAfter::new(
                                        &format!("{} = Some(", field.name),
                                        ");",
                                        false,
                                    ),
                                    deser_config,
                                    cli,
                                )
                                .add_to_code(&mut deser_block_code);
                        }
                    }
                    if cli.preserve_encodings {
                        let key_encoding = key_encoding_field(&field.name, &key);
                        deser_block_code
                            .content
                            .line(&format!(
                                "{} = {};",
                                key_encoding.field_name,
                                key_encoding.enc_conversion("key_enc")
                            ))
                            .line(&format!("orig_deser_order.push({field_index});"));
                    }

                    // serialize key
                    let mut map_ser_content = BlocksOrLines::default();
                    let serialize_config = SerializeConfig::new(&data_name, &field.name)
                        .expr_is_ref(expr_is_ref)
                        .encoding_var_in_option_struct("self.encodings");
                    let key_encoding_var =
                        serialize_config.encoding_var(Some("key"), key.encoding_var_is_copy(types));

                    deser_block
                        .push_all(deser_block_code.mark_and_extract_content(&mut deser_code));
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
                                cli,
                            );
                            uint_field_deserializers.push(deser_block);
                        }
                        FixedValue::Text(s) => {
                            write_string_sz(
                                &mut map_ser_content,
                                "write_text",
                                "serializer",
                                &format!("\"{}\"", escape_rust_str(s)),
                                false,
                                "?;",
                                &key_encoding_var,
                                cli,
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
                        cli,
                    );
                    ser_content.push((field_index, field, map_ser_content));
                }
                if cli.preserve_encodings {
                    let (check_canonical, serialization_order) = if cli.canonical_form {
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
                    record.definite_info("self", false, types, cli),
                    serialization_order));
                    let mut ser_loop = Block::new("for field_index in deser_order");
                    let mut ser_loop_match = Block::new("match field_index");
                    for (field_index, field, content) in ser_content.into_iter() {
                        // TODO: while this would be nice we would need to either:
                        // 1) know this before we call gen_scope.generate_serialize() OR
                        // 2) strip that !is_end (?;) field from it which seems brittle
                        //if let Some(single_line) = content.as_single_line() {
                        //    ser_loop_match.line(format!("{} => {},"));
                        //} else {
                        //}
                        let mut field_ser_block = if field.optional
                            && field.rust_type.config.default.is_none()
                        {
                            Block::new(format!(
                                "{} => if let Some(field) = &self.{}",
                                field_index, field.name
                            ))
                        } else if field.optional {
                            // defaulted optional: the map HEADER (definite_info) counts this field
                            // only when it differs from its default (or was explicitly present on
                            // deserialize) — the write arm must apply the IDENTICAL condition or a
                            // freshly-constructed default-valued field serializes a body entry the
                            // header didn't count (corrupt CBOR: length mismatch / trailing data)
                            let default_value = field.rust_type.config.default.as_ref().unwrap();
                            Block::new(format!(
                                "{} => if self.{} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false)",
                                field_index,
                                field.name,
                                default_value.to_primitive_str_compare(),
                                field.name
                            ))
                        } else {
                            Block::new(format!("{field_index} =>"))
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
                                if let Some(default_value) = &field.rust_type.config.default {
                                    format!(
                                        "if self.{} != {}",
                                        field.name,
                                        default_value.to_primitive_str_compare()
                                    )
                                } else {
                                    format!("if let Some(field) = &self.{}", field.name)
                                };
                            let mut optional_ser_field = Block::new(optional_ser_field_check);
                            optional_ser_field.push_all(content);
                            ser_func.push_block(optional_ser_field);
                        } else {
                            ser_func.push_all(content);
                        }
                    }
                }
                // needs to be in one line rather than a block because Block::after() only takes a string
                deser_code.content.line("let mut read = 0;");
                let mut deser_loop = make_deser_loop("len", "read", cli);
                let mut type_match = Block::new("match raw.cbor_type()?");
                if uint_field_deserializers.is_empty() {
                    type_match.line("cbor_event::Type::UnsignedInteger => return Err(DeserializeFailure::UnknownKey(Key::Uint(raw.unsigned_integer()?)).into()),");
                } else {
                    let mut uint_match = if cli.preserve_encodings {
                        Block::new(
                            "cbor_event::Type::UnsignedInteger => match raw.unsigned_integer_sz()?",
                        )
                    } else {
                        Block::new(
                            "cbor_event::Type::UnsignedInteger => match raw.unsigned_integer()?",
                        )
                    };
                    for case in uint_field_deserializers {
                        uint_match.push_block(case);
                    }
                    let unknown_key_decl = if cli.preserve_encodings {
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
                } else if cli.preserve_encodings {
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
                    cbor_event_len_n("_", cli)
                ));
                // TODO: this will need to change if we support Special values as keys (e.g. true / false)
                let mut break_check = Block::new(format!(
                    "{} => match raw.special()?",
                    cbor_event_len_indef(cli)
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
                            Some(FixedValue::Text(x)) => {
                                format!("Key::Str(String::from(\"{}\"))", escape_rust_str(x))
                            }
                            None => unreachable!(),
                            _ => unimplemented!(),
                        };
                        if field.rust_type.is_fixed_value() {
                            let mut mandatory_field_check =
                                Block::new(format!("if !{}_present", field.name));
                            mandatory_field_check.line(format!(
                            "return Err(DeserializeFailure::MandatoryFieldMissing({key}).into());"
                        ));
                            deser_code.content.push_block(mandatory_field_check);
                        } else {
                            let mut mandatory_field_check =
                                Block::new(format!("let {} = match {}", field.name, field.name));
                            mandatory_field_check.line("Some(x) => x,");

                            mandatory_field_check.line(format!("None => return Err(DeserializeFailure::MandatoryFieldMissing({key}).into()),"));
                            mandatory_field_check.after(";");
                            deser_code.content.push_block(mandatory_field_check);
                        }
                    } else if let Some(default_value) = &field.rust_type.config.default {
                        if cli.preserve_encodings {
                            let mut default_present_check = Block::new(format!(
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
                if cli.preserve_encodings {
                    let mut encoding_ctor = Block::new(format!("encodings: Some({name}Encoding"));
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
                            cli,
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
            .map(|var| {
                format!("self.encodings.as_ref().map(|encs| encs.{var}).unwrap_or_default()")
            })
            .unwrap_or_default();
        match &mut ser_embedded_impl {
            Some(ser_embedded_impl) => {
                // Embedded (plain-group) serialize writes only the group's contents; the entity
                // that wrote the array/map head owns the ending break. Writing `.end()` here too
                // double-writes the break when the framing owner is indefinite-length (the break
                // is written once by the container / standalone serialize). Just return.
                ser_func.line("Ok(serializer)");
                ser_embedded_impl.push_fn(ser_func)
            }
            None => {
                end_len(&mut ser_func, "serializer", &len_enc_var, true, cli);
                ser_impl.push_fn(ser_func)
            }
        };
        let mut deser_scaffolding = BlocksOrLines::default();
        let (mut deser_impl, mut deser_embedded_impl) = create_deserialize_impls(
            name,
            Some(record.rep),
            tag,
            Some(record.cbor_len_info(types)),
            types.is_plain_group(name),
            len_encoding_var,
            &mut deser_scaffolding,
            cli,
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
                cli,
            );
        }
        deser_code.content.push_block(ctor_block);

        if cli.annotate_fields {
            deser_code = deser_code.annotate(name.as_ref(), "", "");
        }

        if let Some(deser_embedded_impl) = &mut deser_embedded_impl {
            let mut deser_f =
                make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
            deser_f.push_all(deser_scaffolding);
            deser_impl.push_fn(deser_f);
            let mut deser_embed_f = make_deserialization_function(
                "deserialize_as_embedded_group",
                &gen_scope.deserialize_generic,
                cli,
            );
            let read_len_arg = if deser_code.read_len_used {
                "read_len"
            } else {
                "_read_len"
            };
            deser_embed_f.arg(read_len_arg, "&mut CBORReadLen");
            if cli.preserve_encodings {
                deser_embed_f.arg("len", "cbor_event::LenSz");
            } else {
                deser_embed_f.arg("len", "cbor_event::Len");
            }
            // this is expected when creating the final struct but wouldn't have been available
            // otherwise as it is in the non-embedded deserialiation function
            if cli.preserve_encodings {
                deser_embed_f.line("let len_encoding = len.into();");
            }
            deser_embed_f.push_all(deser_code.content);
            deser_embedded_impl.push_fn(deser_embed_f);
        } else {
            let mut deser_f =
                make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
            deser_f.push_all(deser_scaffolding);
            deser_f.push_all(deser_code.content);
            deser_impl.push_fn(deser_f);
        }

        if config.custom_serialize.is_none() {
            gen_scope.rust_serialize(types, name).push_impl(ser_impl);
            if let Some(s) = ser_embedded_impl {
                gen_scope.rust_serialize(types, name).push_impl(s);
            }
        }

        // TODO: generic deserialize (might need backtracking)
        if gen_scope.deserialize_generated(name) {
            gen_scope.rust_serialize(types, name).push_impl(deser_impl);
            if let Some(deser_embedded_impl) = deser_embedded_impl {
                gen_scope
                    .rust_serialize(types, name)
                    .push_impl(deser_embedded_impl);
            }
        }
    }

    gen_scope
        .rust(types, name)
        .push_struct(native_struct)
        .push_impl(native_impl);

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
}

#[allow(clippy::too_many_arguments)]
fn codegen_group_choices(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Representation,
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) {
    // rust inner enum
    generate_enum(
        gen_scope,
        types,
        name,
        variants,
        Some(rep),
        false,
        tag,
        config,
        cli,
    );

    // wasm wrapper
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, name, true, cli);
        // new (1 per variant)
        for variant in variants.iter() {
            // TODO: verify if variant.serialize_as_embedded_group impacts ctor generation
            let mut new_func = codegen::Function::new(format!("new_{}", variant.name_as_var()));
            new_func.vis("pub");
            if let Some(doc) = &variant.doc {
                new_func.doc(doc);
            }
            let mut output_comma = false;
            // We only want to generate Variant::new() calls when we created a special struct
            // for the variant, which happens in the general case for multi-field group choices
            let fields = match &variant.data {
                EnumVariantData::RustType(ty) => {
                    match ty.conceptual_type.resolve_alias_shallow() {
                        // we need to check for sanity here, as if we're referring to the ident
                        // it should at this stage be registered
                        ConceptualRustType::Rust(ident) => {
                            match types.rust_struct(ident).unwrap().variant() {
                                RustStructType::Record(record) => Some(&record.fields),
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                }
                EnumVariantData::Inlined(record) => Some(&record.fields),
            };
            match fields {
                Some(fields) => {
                    let inlined = matches!(&variant.data, EnumVariantData::Inlined(_));
                    let ctor_fields: Vec<&RustField> = fields
                        .iter()
                        .filter(|f| (!f.optional || inlined) && !f.rust_type.is_fixed_value())
                        .collect();
                    let can_fail = ctor_fields.iter().any(|f| f.rust_type.has_value_bounds());
                    match ctor_fields.len() {
                        0 => {
                            new_func
                                .line(format!(
                                    "Self({}::new_{}())",
                                    rust_crate_struct_from_wasm(types, name, cli),
                                    variant.name_as_var()
                                ))
                                .ret("Self");
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
                                "{}::new_{}(",
                                rust_crate_struct_from_wasm(types, name, cli),
                                variant.name_as_var()
                            );
                            for field in ctor_fields {
                                if output_comma {
                                    ctor.push_str(", ");
                                } else {
                                    output_comma = true;
                                }
                                // always okay - if not inlined this field would be skipped earlier
                                assert!(!field.optional || inlined);
                                let wasm_param_type = field.to_embedded_rust_type();
                                new_func.arg(&field.name, wasm_param_type.for_wasm_param(types));
                                ctor.push_str(&ToWasmBoundaryOperations::format(
                                    wasm_param_type
                                        .from_wasm_boundary_clone(types, &field.name, false)
                                        .into_iter(),
                                ));
                            }
                            ctor.push(')');
                            if can_fail {
                                new_func
                                    .ret(format!("Result<{name}, JsError>"))
                                    .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                            } else {
                                new_func.ret("Self").line(format!("Self({ctor})"));
                            }
                        }
                    }
                }
                None => {
                    // just directly pass in the variant's type
                    if variant.rust_type().is_fixed_value() {
                        new_func.ret("Self").line(format!(
                            "Self({}::new_{}())",
                            rust_crate_struct_from_wasm(types, name, cli),
                            variant.name_as_var()
                        ));
                    } else {
                        let field_name = convert_to_snake_case(&variant.name.to_string());
                        let ctor = format!(
                            "{}::new_{}({})",
                            rust_crate_struct_from_wasm(types, name, cli),
                            variant.name_as_var(),
                            ToWasmBoundaryOperations::format(
                                variant
                                    .rust_type()
                                    .from_wasm_boundary_clone(types, &field_name, false)
                                    .into_iter()
                            )
                        );
                        new_func.arg(&field_name, variant.rust_type().for_wasm_param(types));
                        if variant.rust_type().has_value_bounds() {
                            new_func
                                .ret(format!("Result<{name}, JsError>"))
                                .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
                        } else {
                            new_func.ret("Self").line(format!("Self({ctor})"));
                        };
                    }
                }
            };
            wrapper.s_impl.push_fn(new_func);
        }
        // enum-getters
        add_wasm_enum_getters(&mut wrapper.s_impl, types, name, variants, Some(rep), cli);
        wrapper.push(gen_scope, types);
    }
}

fn add_wasm_enum_getters(
    s_impl: &mut codegen::Impl,
    types: &IntermediateTypes<'_>,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    cli: &Cli,
) {
    assert!(cli.wasm);
    // kind() getter
    let kind_name = format!("{name}Kind");
    let mut get_kind = codegen::Function::new("kind");
    get_kind.arg_ref_self().vis("pub").ret(&kind_name);
    let mut get_kind_match = Block::new("match &self.0");
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
        get_kind_match.line(format!(
            "{}::{}{} => {}::{},",
            rust_crate_struct_from_wasm(types, name, cli),
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
        let mut add_variant_functions = |ty: &RustType| {
            let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
            let mut as_variant = codegen::Function::new(format!("as_{}", variant.name_as_var()));
            as_variant.arg_ref_self().vis("pub");
            let mut variant_match = Block::new("match &self.0");
            // unfortunately wasm_bindgen doesn't support nested options so we must flatten
            // this is a bit ambiguous but it's better than nothing
            let supported = if let ConceptualRustType::Optional(inner) = ty.resolve_alias_shallow()
            {
                if let ConceptualRustType::Optional(_) = inner.resolve_alias_shallow() {
                    // An enum variant whose payload resolves to Option<Option<T>> (a
                    // nullable-of-nullable, e.g. `text / ((uint / null) / null)`, or via an alias
                    // chain to a nullable) is UNREACHABLE at this getter arm: the wasm enum
                    // CONSTRUCTOR for such a variant panics earlier, in
                    // `from_wasm_boundary_clone_optional` ("unsupported or unexpected"), before getter
                    // emission ever runs. No supported CDDL reaches here, so the former silent
                    // `println!` skip only advertised a behavior (dropping the getter) that can never
                    // occur. Fail loudly instead: if a future constructor change lets the shape emit,
                    // this points at the real work — double-flatten the getter plus an
                    // `as_<variant>_present()` presence accessor (see docs/docs/wasm_differences.mdx)
                    // — rather than silently dropping the getter.
                    unreachable!(
                        "enum variant {}::{} resolves to Option<Option<T>>, which the wasm enum \
                         constructor rejects (from_wasm_boundary_clone_optional) before getters are \
                         emitted — no supported CDDL reaches this arm",
                        name,
                        variant.name_as_var()
                    );
                } else {
                    as_variant
                        .ret(ty.for_wasm_return(types))
                        .doc(format!("Returns None if not {} variant OR it is but it's set to None\nThis is to get around wasm_bindgen not supporting Option<Option<T>>", variant.name));
                    variant_match.line(format!(
                        "{}::{}{} => {},",
                        rust_crate_struct_from_wasm(types, name, cli),
                        variant.name,
                        enum_gen_info.capture_ignore_encodings(),
                        ty.to_wasm_boundary(types, &enum_gen_info.names[0], true)
                    ));
                    true
                }
            } else {
                as_variant.ret(format!("Option<{}>", ty.for_wasm_return(types)));
                variant_match.line(format!(
                    "{}::{}{} => Some({}),",
                    rust_crate_struct_from_wasm(types, name, cli),
                    variant.name,
                    enum_gen_info.capture_ignore_encodings(),
                    ty.to_wasm_boundary(types, &enum_gen_info.names[0], true)
                ));
                true
            };
            if supported {
                variant_match.line("_ => None,");
                as_variant.push_block(variant_match);
                s_impl.push_fn(as_variant);
            }
        };
        match &variant.data {
            EnumVariantData::RustType(ty) => {
                if !ty.is_fixed_value() {
                    add_variant_functions(ty);
                }
            }
            EnumVariantData::Inlined(record) => {
                let non_fixed_types = record
                    .fields
                    .iter()
                    .filter(|field| !field.rust_type.is_fixed_value())
                    .collect::<Vec<_>>();
                // we don't even embed in this case and instead crate a new variant but this is here in case someone
                // tries to add that in the future so they hit this assert.
                assert!(
                    non_fixed_types.len() <= 1,
                    "multiple non-fixed not allowed right now for embedding into enums"
                );
                if let Some(&field) = non_fixed_types.first() {
                    add_variant_functions(field.to_embedded_rust_type().as_ref());
                }
            }
        }
    }
}

fn cbor_event_len_n(n: &str, cli: &Cli) -> String {
    if cli.preserve_encodings {
        format!("cbor_event::LenSz::Len({n}, _)")
    } else {
        format!("cbor_event::Len::Len({n})")
    }
}

fn cbor_event_len_indef(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "cbor_event::LenSz::Indefinite"
    } else {
        "cbor_event::Len::Indefinite"
    }
}

/// How to construct a `CBORReadLen` from the freshly-read `len`. In preserve mode `len` is a
/// `cbor_event::LenSz`, matching `CBORReadLen::new`. In non-preserve mode `len` is a
/// `cbor_event::Len`; going through `From<cbor_event::Len>` (instead of `new`) lets the same
/// emission compile against either runtime flavor — crucially a preserve-flavored
/// `--common-import-override` target (e.g. cml_core), whose `new` takes `LenSz`. Preserve stays on
/// `new` because such external cores expose no `From<LenSz>`.
fn cbor_read_len_ctor(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "CBORReadLen::new"
    } else {
        "CBORReadLen::from"
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
    fn new(
        types: &IntermediateTypes,
        variant: &EnumVariant,
        rep: Option<Representation>,
        cli: &Cli,
    ) -> Self {
        let name = variant.name_as_var();
        match &variant.data {
            EnumVariantData::RustType(ty) => {
                let mut enc_fields = if cli.preserve_encodings {
                    encoding_fields(types, &name, &ty.clone().resolve_aliases(), true, cli)
                } else {
                    vec![]
                };
                // A collapsed map-rep arm writes+verifies a fixed member key on the wire; under
                // preserve-encodings its layout is remembered in a `{var}_key_encoding` field, kept
                // right after the value's encodings and before the outer `len_encoding`.
                if cli.preserve_encodings
                    && rep == Some(Representation::Map)
                    && let Some(key) = &variant.key
                {
                    enc_fields.push(key_encoding_field(&name, key));
                }
                let (mut enum_types, mut names) = if ty.is_fixed_value() {
                    (vec![], vec![])
                } else {
                    (vec![ty.for_rust_member(types, false, cli)], vec![name])
                };
                let mut outer_vars = 0;
                // TOOD: for tags too?
                if cli.preserve_encodings && rep.is_some() && !variant.serialize_as_embedded_group {
                    enc_fields.push(EncodingField {
                        field_name: "len_encoding".to_owned(),
                        type_name: "LenEncoding".to_owned(),
                        default_expr: "LenEncoding::default()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: true,
                        inner: Vec::new(),
                    });
                    outer_vars += 1;
                }
                for enc_field in &enc_fields {
                    enum_types.push(enc_field.type_name.clone());
                    names.push(enc_field.field_name.clone());
                }
                assert_eq!(enum_types.len(), names.len());
                Self {
                    name: variant.name.clone(),
                    enc_fields,
                    names,
                    types: enum_types,
                    outer_vars,
                }
            }
            EnumVariantData::Inlined(record) => {
                let mut enc_fields = vec![];
                let mut enum_types = vec![];
                let mut names = vec![];
                if cli.preserve_encodings {
                    enc_fields.push(EncodingField {
                        field_name: "len_encoding".to_owned(),
                        type_name: "LenEncoding".to_owned(),
                        default_expr: "LenEncoding::default()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: true,
                        inner: Vec::new(),
                    });
                    for field in record.fields.iter() {
                        enc_fields.extend(encoding_fields(
                            types,
                            &field.name,
                            &field.rust_type.clone().resolve_aliases(),
                            true,
                            cli,
                        ));
                    }
                }
                for field in record.fields.iter() {
                    if !field.rust_type.is_fixed_value() {
                        names.push(field.name.clone());
                        enum_types.push(
                            field
                                .to_embedded_rust_type()
                                .for_rust_member(types, false, cli),
                        );
                    }
                }
                for enc_field in &enc_fields {
                    enum_types.push(enc_field.type_name.clone());
                    names.push(enc_field.field_name.clone());
                }
                assert_eq!(enum_types.len(), names.len());
                Self {
                    name: variant.name.clone(),
                    enc_fields,
                    names,
                    types: enum_types,
                    outer_vars: 0,
                }
            }
        }
    }

    fn names_without_outer(&self) -> &[String] {
        &self.names[..self.names.len() - self.outer_vars]
    }

    fn names_with_macros(&self, used_in_key: bool, cli: &Cli) -> Vec<String> {
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
                    format!("{}{}", encoding_var_macros(used_in_key, false, cli), name)
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
                let mut ctor = Block::new(format!("{}Self::{}", before, self.name));
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

// Generates an enum where all variants are fixed values (i.e. C-style enum)
// and return true, or return false and do nothing (i.e. enum too complex)
fn generate_c_style_enum(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) -> bool {
    if tag.is_some() && cli.preserve_encodings {
        // cannot store it in a C-style enum
        return false;
    }
    if variants.iter().any(|ev: &EnumVariant| {
        ev.serialize_as_embedded_group
            || (cli.preserve_encodings && !ev.rust_type().encodings.is_empty())
            || !matches!(
                ev.rust_type().conceptual_type.resolve_alias_shallow(),
                ConceptualRustType::Fixed(_)
            )
    }) {
        return false;
    }
    // rust enum containing the data
    let mut e = codegen::Enum::new(name.to_string());
    e.vis("pub");
    e.derive("Copy");
    // Eq/PartialEq/Ord/PartialOrd are needed for a c-style enum used as a map/set key. When it *is* a
    // key, `add_struct_derives` (below) adds them — and handles `--preserve-encodings` via `derivative`
    // — so deriving them here too would double-derive (`E0119` conflicting impls). Only add them here
    // for the non-key case (unchanged output there).
    if !types.used_as_key(name) {
        e.derive("Eq")
            .derive("PartialEq")
            .derive("Ord")
            .derive("PartialOrd");
    }
    if cli.wasm {
        e.attr("wasm_bindgen::prelude::wasm_bindgen");
        gen_scope
            .wasm(types, name)
            .new_import(
                rust_crate_struct_scope_from_wasm(types, name, cli),
                name,
                None,
            )
            .vis("pub");
    }
    add_struct_derives(
        &mut e,
        types.used_as_key(name),
        true,
        config.custom_json,
        cli,
    );
    for variant in variants.iter() {
        e.new_variant(variant.name.to_string());
    }
    // Only the enum definition is emitted — no serialize/deserialize impl. A c-style enum's
    // fixed-value encoding is generated inline wherever it's used (see the field/variant serializers)
    // rather than via an `impl` on the enum, so a c-style enum that nothing references produces no
    // serialization code at all (its `serialization.rs` ends up empty).
    gen_scope.rust(types, name).push_enum(e);
    true
}

fn make_enum_variant_return_if_deserialized(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    variant: &EnumVariant,
    no_enum_types: bool,
    len_check: Option<(RustStructCBORLen, Representation)>,
    deser_body: &mut dyn CodeBlock,
    cli: &Cli,
) -> Block {
    let (before, after) = if len_check.is_some() && !no_enum_types {
        ("let ret = ", ";")
    } else {
        ("", "")
    };
    let variant_deser_code = if no_enum_types {
        let mut code = gen_scope.generate_deserialize(
            types,
            (variant.rust_type()).into(),
            DeserializeBeforeAfter::new(before, after, false),
            DeserializeConfig::new(&variant.name_as_var()),
            cli,
        );
        if let Some((len_info, rep)) = len_check {
            code = surround_in_len_checks(code, len_info, rep, cli);
        }
        code.content.line("Ok(())");
        code
    } else {
        let mut code = gen_scope.generate_deserialize(
            types,
            (variant.rust_type()).into(),
            DeserializeBeforeAfter::new(before, after, true),
            DeserializeConfig::new(&variant.name_as_var()),
            cli,
        );
        if let Some((len_info, rep)) = len_check {
            code = surround_in_len_checks(code, len_info, rep, cli);
            code.content.line("ret");
        }
        code
    };
    match variant_deser_code.content.as_single_line() {
        Some(single_line) if !variant_deser_code.throws => {
            // to get around type annotations being needed for error types (e.g. auto conversions with ?) we make a variable
            // to do better than this we'd need to make DeserializationCode keep track of error types too.
            deser_body.line(&format!(
                "let deser_variant: Result<_, DeserializeError> = {single_line};"
            ));
        }
        _ => {
            let mut variant_deser = Block::new(
                "let deser_variant = (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>",
            );
            variant_deser.after(")(raw);");
            variant_deser.push_all(variant_deser_code.content);
            deser_body.push_block(variant_deser);
        }
    }
    Block::new("match deser_variant")
}

fn surround_in_len_checks(
    mut main_deser_code: DeserializationCode,
    len_info: RustStructCBORLen,
    rep: Representation,
    cli: &Cli,
) -> DeserializationCode {
    let mut len_check_before = DeserializationCode::default();
    add_deserialize_initial_len_check(&mut len_check_before.content, len_info, cli);
    main_deser_code.add_to_code(&mut len_check_before);
    main_deser_code = len_check_before;
    add_deserialize_final_len_check(&mut main_deser_code.content, Some(rep), len_info, cli);
    main_deser_code
}

fn make_inline_deser_code(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    tag: Option<usize>,
    record: &RustRecord,
    enum_gen_info: &EnumVariantInRust,
    cli: &Cli,
) -> DeserializationCode {
    let mut variant_deser_code = generate_array_struct_deserialization(
        gen_scope, types, name, record, tag, false, false, cli,
    );
    // generate_constructor zips the expressions with the names in the enum_gen_info
    // so just make sure we're in the same order as returned above
    assert_eq!(
        enum_gen_info.names.len(),
        variant_deser_code.deser_ctor_fields.len()
            + variant_deser_code.encoding_struct_ctor_fields.len()
    );
    let ctor_exprs = variant_deser_code
        .deser_ctor_fields
        .into_iter()
        .chain(variant_deser_code.encoding_struct_ctor_fields)
        .zip(enum_gen_info.names.iter())
        .map(|((var, expr), name)| {
            assert_eq!(var, *name);
            expr
        })
        .collect();
    variant_deser_code.deser_code = surround_in_len_checks(
        variant_deser_code.deser_code,
        record.cbor_len_info(types),
        record.rep,
        cli,
    );
    enum_gen_info.generate_constructor(
        &mut variant_deser_code.deser_code.content,
        "Ok(",
        ")",
        Some(&ctor_exprs),
    );
    variant_deser_code.deser_code
}

/// Writes the fixed member key of a collapsed map-rep group-choice arm, between the map header and
/// the value. Under `--preserve-encodings` it uses the variant's `{var}_key_encoding` field
/// (captured directly from the match arm), mirroring the record map-key write path.
fn push_map_choice_key_ser(
    body: &mut dyn CodeBlock,
    variant_var: &str,
    key: &FixedValue,
    cli: &Cli,
) {
    match key {
        FixedValue::Uint(x) => {
            let expr = format!("{x}u64");
            // the key encoding var is a `Copy` `Option<Sz>` captured by ref → deref like the value
            // path does via `encoding_var_is_ref`.
            write_using_sz(
                body,
                "write_unsigned_integer",
                "serializer",
                &expr,
                &expr,
                "?;",
                &format!("*{variant_var}_key_encoding"),
                cli,
            );
        }
        FixedValue::Text(s) => {
            write_string_sz(
                body,
                "write_text",
                "serializer",
                &format!("\"{}\"", escape_rust_str(s)),
                false,
                "?;",
                &format!("{variant_var}_key_encoding"),
                cli,
            );
        }
        _ => panic!("unsupported map choice key type (only uint/text are supported): {key:?}"),
    }
}

/// Reads and verifies the fixed member key of a collapsed map-rep group-choice arm. A mismatch
/// returns `Err` (in the brute-force path this becomes try-the-next-variant). Under
/// `--preserve-encodings` it produces the `{var}_key_encoding` local consumed by the constructor.
fn push_map_choice_key_deser(
    body: &mut dyn CodeBlock,
    variant_var: &str,
    key: &FixedValue,
    cli: &Cli,
) {
    match key {
        FixedValue::Uint(x) => {
            if cli.preserve_encodings {
                body.line(&format!(
                    "let ({variant_var}_key, {variant_var}_key_encoding) = raw.unsigned_integer_sz()?;"
                ));
            } else {
                body.line(&format!("let {variant_var}_key = raw.unsigned_integer()?;"));
            }
            let mut cmp = Block::new(format!("if {variant_var}_key != {x}"));
            cmp.line(format!(
                "return Err(DeserializeFailure::FixedValueMismatch {{ found: Key::Uint({variant_var}_key), expected: Key::Uint({x}) }}.into());"
            ));
            body.push_block(cmp);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let {variant_var}_key_encoding = Some({variant_var}_key_encoding);"
                ));
            }
        }
        FixedValue::Text(s) => {
            let escaped = escape_rust_str(s);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let ({variant_var}_key, {variant_var}_key_encoding) = raw.text_sz()?;"
                ));
            } else {
                body.line(&format!("let {variant_var}_key = raw.text()?;"));
            }
            let mut cmp = Block::new(format!("if {variant_var}_key != \"{escaped}\""));
            cmp.line(format!(
                "return Err(DeserializeFailure::FixedValueMismatch {{ found: Key::Str({variant_var}_key), expected: Key::Str(String::from(\"{escaped}\")) }}.into());"
            ));
            body.push_block(cmp);
            if cli.preserve_encodings {
                body.line(&format!(
                    "let {variant_var}_key_encoding = StringEncoding::from({variant_var}_key_encoding);"
                ));
            }
        }
        _ => panic!("unsupported map choice key type (only uint/text are supported): {key:?}"),
    }
}

/// Full deserialization body for a collapsed map-rep group-choice arm that carries a fixed key:
/// len-check, key read+verify, value read, final len-check, and the variant constructor. The map
/// holds exactly one pair (key + value), so the length check is `Fixed(1)`. Used by both enum
/// dispatch paths (type-match arm body / brute-force closure body).
fn make_keyed_map_variant_deser_code(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variant: &EnumVariant,
    key: &FixedValue,
    enum_gen_info: &EnumVariantInRust,
    cli: &Cli,
) -> DeserializationCode {
    let variant_var = variant.name_as_var();
    let ty = variant.rust_type();
    let var_names_str = if cli.preserve_encodings {
        encoding_var_names_str(types, &variant_var, ty, cli)
    } else {
        variant_var.clone()
    };
    // read + verify the fixed key
    let mut inner = DeserializationCode::default();
    push_map_choice_key_deser(&mut inner.content, &variant_var, key, cli);
    inner.throws = true;
    // read the value
    let value_code = gen_scope.generate_deserialize(
        types,
        ty.into(),
        DeserializeBeforeAfter::new(&format!("let {var_names_str} = "), ";", false),
        DeserializeConfig::new(&variant_var),
        cli,
    );
    value_code.add_to_code(&mut inner);
    // Map holds a single pair — count of PAIRS is 1. We deliberately request the ARRAY-style
    // final len check here: for maps `add_deserialize_final_len_check` skips the ending-Break
    // consumption because record map deserializers are loops that consume the Break themselves —
    // but this keyed arm is straight-line code, so an indefinite map (`bf .. ff`) would otherwise
    // leave the trailing Break unread (spec-valid input then dies on "trailing data"). The Array
    // branch emits exactly the needed `match len { Len => (), Indefinite => expect Break }`, and
    // `Len`/`LenSz` are shared between array and map reads so the emitted code is rep-agnostic.
    let mut deser_code = surround_in_len_checks(
        inner,
        RustStructCBORLen::Fixed(1),
        Representation::Array,
        cli,
    );
    if enum_gen_info.outer_vars == 0 {
        deser_code.content.line(&format!(
            "Ok({}::{}({}))",
            name, variant.name, var_names_str
        ));
    } else {
        enum_gen_info.generate_constructor(&mut deser_code.content, "Ok(", ")", None);
    }
    deser_code
}

// Generates a general enum e.g. Foo { A(A), B(B), C(C) } for types A, B, C
// if generate_deserialize_directly, don't generate deserialize_as_embedded_group() and just inline it within deserialize()
// This is useful for type choicecs which don't have any enclosing array/map tags, and thus don't benefit from exposing a
// deserialize_as_embedded_group as the behavior would be identical.
#[allow(clippy::too_many_arguments)]
fn generate_enum(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    variants: &[EnumVariant],
    rep: Option<Representation>,
    generate_deserialize_directly: bool,
    tag: Option<usize>,
    config: &RustStructConfig,
    cli: &Cli,
) {
    if cli.wasm {
        // also create a wasm-exposed enum just to distinguish the type
        let mut kind = codegen::Enum::new(format!("{name}Kind"));
        kind.vis("pub");
        for variant in variants.iter() {
            kind.new_variant(variant.name.to_string());
        }
        kind.attr("wasm_bindgen");
        gen_scope.wasm(types, name).push_enum(kind);
    }

    // rust enum containing the data
    let mut e = codegen::Enum::new(name.to_string());
    e.vis("pub");
    if let Some(doc) = config.doc.as_ref() {
        e.doc(doc);
    }
    let mut e_impl = codegen::Impl::new(name.to_string());
    // instead of using create_serialize_impl() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(
        &mut e,
        types.used_as_key(name),
        true,
        config.custom_json,
        cli,
    );
    let mut ser_impl = make_serialization_impl(name.as_ref(), cli);
    let mut ser_func = make_serialization_function("serialize", &gen_scope.serialize_generic, cli);
    if let Some(tag) = tag {
        // TODO: how to even store these? (maybe it could be a new field in every enum variant)
        assert!(!cli.preserve_encodings);
        ser_func.line(format!("serializer.write_tag({tag}u64)?;"));
    }
    let mut ser_array_match_block = Block::new("match self");
    let mut deser_func =
        make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
    let mut error_annotator = make_err_annotate_block(name.as_ref(), "", "");
    let deser_body: &mut dyn CodeBlock = if cli.annotate_fields {
        &mut error_annotator
    } else {
        &mut deser_func
    };
    let mut deser_impl = if generate_deserialize_directly {
        // this is handled in create_deseriaize_impls in the other case, and it MUST be handled there to ensure that
        // the tag check is done BEFORE reading the array/map CBOR
        generate_tag_check(deser_body, name, tag);
        let mut deser_impl = codegen::Impl::new(name.to_string());
        deser_impl.impl_trait("Deserialize");
        deser_impl
    } else {
        // this handles the tag check too
        let outer_encoding_var = if cli.preserve_encodings
            && variants
                .iter()
                .any(|variant| !variant.serialize_as_embedded_group)
        {
            Some("len_encoding")
        } else {
            None
        };
        let (deser_impl, _deser_embedded_impl) = create_deserialize_impls(
            name,
            rep,
            tag,
            None,
            false,
            outer_encoding_var,
            deser_body,
            cli,
        );
        deser_impl
    };
    // We avoid checking ALL variants if we can figure it out by instead checking the type.
    // This only works when the variants don't have first types in common.
    let mut non_overlapping_types_match = {
        let mut all_first_types = BTreeSet::new();
        let mut duplicates_or_unknown = false;
        for variant in variants.iter() {
            match variant.cbor_types_inner(types, rep) {
                Some(first_types) => {
                    for first_type in first_types.iter() {
                        // to_byte(0) is used since cbor_event::Type doesn't implement
                        // Ord or Hash so we can't put it in a set. Since we fix the lenth
                        // to always 0 this still remains a 1-to-1 mapping to Type.
                        if !all_first_types.insert(first_type.to_byte(0)) {
                            duplicates_or_unknown = true;
                        }
                    }
                }
                None => {
                    duplicates_or_unknown = true;
                    break;
                }
            }
        }
        if duplicates_or_unknown {
            None
        } else {
            let deser_covers_all_types = all_first_types.len() == 8;
            Some((Block::new("match raw.cbor_type()?"), deser_covers_all_types))
        }
    };
    if non_overlapping_types_match.is_none() {
        deser_body
            .line("let initial_position = raw.as_mut_ref().stream_position().unwrap();")
            .line("let mut errs = Vec::new();");
    }
    for variant in variants.iter() {
        let enum_gen_info = EnumVariantInRust::new(types, variant, rep, cli);
        let variant_var_name = variant.name_as_var();
        let mut v = codegen::Variant::new(variant.name.to_string());
        match enum_gen_info.names.len() {
            0 => {}
            1 if enum_gen_info.enc_fields.is_empty() => {
                v.tuple(&enum_gen_info.types[0]);
            }
            _ => {
                for (name_with_macros, type_str) in enum_gen_info
                    .names_with_macros(types.used_as_key(name), cli)
                    .into_iter()
                    .zip(enum_gen_info.types.iter())
                {
                    v.named(&name_with_macros, type_str);
                }
            }
        }
        if let Some(doc) = &variant.doc {
            // we must repurpose annotations since there is no doc support on enum variants
            v.annotation(format!("/// {doc}"));
        }
        e.push_variant(v);
        // new (particularly useful if we have encoding variables)
        let mut new_func = codegen::Function::new(format!("new_{variant_var_name}"));
        new_func.vis("pub");
        if let Some(doc) = &variant.doc {
            new_func.doc(doc);
        }
        let mut output_comma = false;
        let (mut init_fields, can_fail) = match &variant.data {
            EnumVariantData::RustType(ty) => {
                // We only want to generate Variant::new() calls when we created a special struct
                // for the variant, which happens in the general case for multi-field group choices
                let fields = match &ty.conceptual_type {
                    // we need to check for sanity here, as if we're referring to the ident
                    // it should at this stage be registered
                    ConceptualRustType::Rust(ident) => {
                        match types
                            .rust_struct(ident)
                            .unwrap_or_else(|| {
                                panic!("{} refers to undefined ident: {}", name, ident)
                            })
                            .variant()
                        {
                            RustStructType::Record(record) => Some(&record.fields),
                            _ => None,
                        }
                    }
                    _ => None,
                };
                match rep.and(fields) {
                    Some(fields) => {
                        let ctor_fields: Vec<&RustField> = fields
                            .iter()
                            .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                            .collect();
                        let can_fail = ctor_fields
                            .iter()
                            .any(|field| field.rust_type.has_value_bounds());
                        // bounds checking should be handled by the called constructor here
                        let mut ctor = format!("{}::new(", ty.conceptual_type.for_variant());
                        for field in ctor_fields {
                            if output_comma {
                                ctor.push_str(", ");
                            } else {
                                output_comma = true;
                            }
                            new_func.arg(&field.name, field.rust_type.for_rust_move(types, cli));
                            ctor.push_str(&field.name);
                        }
                        ctor.push(')');
                        if can_fail {
                            ctor.push('?');
                        }
                        (vec![ctor], can_fail)
                    }
                    None => {
                        if ty.is_fixed_value() {
                            (vec![], false)
                        } else {
                            // just directly pass in the variant's type
                            let field_name = variant.name_as_var();
                            new_func
                                .arg(&field_name, variant.rust_type().for_rust_move(types, cli));
                            if let Some(line) = value_bounds_check_line(ty, &field_name, true) {
                                new_func.line(&line);
                            }
                            (vec![field_name], ty.has_value_bounds())
                        }
                    }
                }
            }
            EnumVariantData::Inlined(record) => {
                let init_fields = record
                    .fields
                    .iter()
                    .filter(|field| !field.rust_type.is_fixed_value())
                    .map(|field| {
                        new_func.arg(
                            &field.name,
                            field.to_embedded_rust_type().for_rust_move(types, cli),
                        );
                        field.name.clone()
                    })
                    .collect();
                let can_fail = record.fields.iter().any(|field| {
                    let can_fail = field.rust_type.needs_bounds_check_if_inlined(types);
                    // a bounded named Rust wrapper checks at its own ctor (no inline check line, but
                    // still fallible via `?`); a primitive int/float field emits its check here.
                    if can_fail
                        && let Some(line) =
                            value_bounds_check_line(&field.rust_type, &field.name, true)
                    {
                        new_func.line(&line);
                    }
                    can_fail
                });
                (init_fields, can_fail)
            }
        };
        for enc_field in enum_gen_info.enc_fields.iter() {
            init_fields.push(enc_field.default_expr.to_owned());
        }
        let (ret_type, ctor_before, ctor_after) = if can_fail {
            ("Result<Self, DeserializeError>", "Ok(", ")")
        } else {
            ("Self", "", "")
        };
        new_func.ret(ret_type);
        enum_gen_info.generate_constructor(
            &mut new_func,
            ctor_before,
            ctor_after,
            Some(&init_fields),
        );
        e_impl.push_fn(new_func);

        // serialize
        if variant.serialize_as_embedded_group {
            assert_eq!(enum_gen_info.names.len(), 1);
            // we use serialize() instead of serialize_as_embedded_group() to count as the outer array tag here
            // to simplify things (the size logic is there already)
            ser_array_match_block.line(format!(
                "{}::{}({}) => {}.serialize(serializer{}),",
                name,
                variant.name,
                variant_var_name,
                variant_var_name,
                canonical_param(cli)
            ));
        } else {
            let mut case_block = Block::new(format!(
                "{}::{}{} =>",
                name,
                variant.name,
                enum_gen_info.capture_all()
            ));
            match &variant.data {
                EnumVariantData::RustType(ty) => {
                    if cli.preserve_encodings {
                        if let Some(r) = rep {
                            // group choice
                            let n = ty.expanded_field_count(types).expect(
                                "preserve-encodings=true not supported with varying-size group choice",
                            );
                            start_len(
                                &mut case_block,
                                r,
                                "serializer",
                                "len_encoding",
                                &n.to_string(),
                                cli,
                            );
                            // map-rep collapsed arm: write the fixed member key before the value
                            if r == Representation::Map
                                && let Some(key) = &variant.key
                            {
                                push_map_choice_key_ser(
                                    &mut case_block,
                                    &variant_var_name,
                                    key,
                                    cli,
                                );
                            }
                            gen_scope.generate_serialize(
                                types,
                                ty.into(),
                                &mut case_block,
                                SerializeConfig::new(&variant_var_name, &variant_var_name)
                                    .expr_is_ref(true)
                                    .encoding_var_is_ref(true),
                                cli,
                            );
                            end_len(&mut case_block, "serializer", "len_encoding", false, cli);
                            case_block.line("Ok(serializer)");
                        } else {
                            // type choice
                            gen_scope.generate_serialize(
                                types,
                                ty.into(),
                                &mut case_block,
                                SerializeConfig::new(&variant_var_name, &variant_var_name)
                                    .expr_is_ref(true)
                                    .is_end(true)
                                    .encoding_var_is_ref(true),
                                cli,
                            );
                        }
                    } else {
                        let write_break = match rep {
                            // group choice
                            Some(r) => {
                                let (len_str, indefinite) = match ty.expanded_field_count(types) {
                                    Some(n) => (cbor_event_len_n(&n.to_string(), cli), false),
                                    None => (String::from(cbor_event_len_indef(cli)), true),
                                };
                                let func_str = match r {
                                    Representation::Array => "write_array",
                                    Representation::Map => "write_map",
                                };
                                case_block.line(format!("serializer.{func_str}({len_str})?;"));
                                // map-rep collapsed arm: write the fixed member key before the value
                                if r == Representation::Map
                                    && let Some(key) = &variant.key
                                {
                                    push_map_choice_key_ser(
                                        &mut case_block,
                                        &variant_var_name,
                                        key,
                                        cli,
                                    );
                                }
                                indefinite
                            }
                            // type choice
                            None => false,
                        };
                        // TODO: only generate a block if the serialize is more than 1 line
                        // Problem: generate_serialize() works in terms of line() and push_block()
                        //          but we'd just want to inline the single one inside of a line...
                        gen_scope.generate_serialize(
                            types,
                            ty.into(),
                            &mut case_block,
                            SerializeConfig::new(&variant_var_name, &variant_var_name)
                                .expr_is_ref(true)
                                .is_end(!write_break),
                            cli,
                        );
                        if write_break {
                            case_block.line("serializer.write_special(cbor_event::Special::Break)");
                        }
                    }
                }
                EnumVariantData::Inlined(record) => {
                    start_len(
                        &mut case_block,
                        rep.expect("can't inline in type choices"),
                        "serializer",
                        "len_encoding",
                        &record.definite_info("", true, types, cli),
                        cli,
                    );
                    generate_array_struct_serialization(
                        gen_scope,
                        types,
                        record,
                        false,
                        &mut case_block,
                        cli,
                    );
                    end_len(&mut case_block, "serializer", "len_encoding", false, cli);
                    case_block.line("Ok(serializer)");
                }
            }
            case_block.after(",");
            ser_array_match_block.push_block(case_block);
        }
        // deserialize
        // TODO: how to detect when a greedy match won't work? (ie choice with choices in a choice possibly)
        match non_overlapping_types_match.as_mut() {
            Some((deser_type_match, _deser_covers_all_types)) => {
                let variant_deser_code = match &variant.data {
                    // map-rep collapsed arm with a fixed key: read+verify the key before the value
                    EnumVariantData::RustType(_)
                        if rep == Some(Representation::Map) && variant.key.is_some() =>
                    {
                        make_keyed_map_variant_deser_code(
                            gen_scope,
                            types,
                            name,
                            variant,
                            variant.key.as_ref().unwrap(),
                            &enum_gen_info,
                            cli,
                        )
                    }
                    EnumVariantData::RustType(ty) => {
                        let var_names_str = if cli.preserve_encodings {
                            encoding_var_names_str(types, &variant.name_as_var(), ty, cli)
                        } else {
                            variant.name_as_var()
                        };
                        let (before, after) = if cli.preserve_encodings
                            || !variant.rust_type().is_fixed_value()
                            || rep.is_some()
                        {
                            (Cow::from(format!("let {var_names_str} = ")), ";")
                        } else {
                            (Cow::from(""), "")
                        };
                        let mut variant_deser_code = gen_scope.generate_deserialize(
                            types,
                            (variant.rust_type()).into(),
                            DeserializeBeforeAfter::new(&before, after, false),
                            DeserializeConfig::new(&variant.name_as_var()),
                            cli,
                        );
                        if let Some(r) = rep {
                            let len_info = match ty.conceptual_type.resolve_alias_shallow() {
                                ConceptualRustType::Rust(ident) if types.is_plain_group(ident) => {
                                    types.rust_struct(ident).unwrap().cbor_len_info(types)
                                }
                                _ => RustStructCBORLen::Fixed(1),
                            };
                            // this will never be 1 line so don't bother with the below cases
                            variant_deser_code =
                                surround_in_len_checks(variant_deser_code, len_info, r, cli);
                            if enum_gen_info.outer_vars == 0 {
                                variant_deser_code.content.line(&format!(
                                    "Ok({}::{}({}))",
                                    name, variant.name, var_names_str
                                ));
                            } else {
                                enum_gen_info.generate_constructor(
                                    &mut variant_deser_code.content,
                                    "Ok(",
                                    ")",
                                    None,
                                );
                            }
                        } else {
                            // we can avoid this ugly block and directly do it as a line possibly
                            if variant_deser_code.content.as_single_line().is_some()
                                && enum_gen_info.names.len() == 1
                            {
                                variant_deser_code = gen_scope.generate_deserialize(
                                    types,
                                    (variant.rust_type()).into(),
                                    DeserializeBeforeAfter::new(
                                        &format!("Ok({}::{}(", name, variant.name),
                                        "))",
                                        false,
                                    ),
                                    DeserializeConfig::new(&variant.name_as_var()),
                                    cli,
                                );
                            } else if enum_gen_info.names.is_empty() {
                                variant_deser_code
                                    .content
                                    .line(&format!("Ok({}::{})", name, variant.name));
                            } else {
                                enum_gen_info.generate_constructor(
                                    &mut variant_deser_code.content,
                                    "Ok(",
                                    ")",
                                    None,
                                );
                            }
                        }
                        variant_deser_code
                    }
                    EnumVariantData::Inlined(record) => make_inline_deser_code(
                        gen_scope,
                        types,
                        name,
                        tag,
                        record,
                        &enum_gen_info,
                        cli,
                    ),
                };
                let cbor_types_str = variant
                    .cbor_types_inner(types, rep)
                    .expect("Already checked above")
                    .into_iter()
                    .map(cbor_type_code_str)
                    .collect::<Vec<_>>()
                    .join("|");
                match variant_deser_code.content.as_single_line() {
                    Some(single_line) => {
                        deser_type_match.line(format!("{cbor_types_str} => {single_line},"));
                    }
                    None => {
                        let mut match_arm = Block::new(format!("{cbor_types_str} =>"));
                        variant_deser_code.add_to(&mut match_arm);
                        deser_type_match.push_block(match_arm);
                    }
                }
            }
            None => {
                let mut return_if_deserialized = match &variant.data {
                    // map-rep collapsed arm with a fixed key: the closure reads+verifies the key
                    // then the value and returns the fully-constructed variant (like the Inlined
                    // path), so a key mismatch cleanly falls through to the next variant.
                    EnumVariantData::RustType(_)
                        if rep == Some(Representation::Map) && variant.key.is_some() =>
                    {
                        let variant_deser_code = make_keyed_map_variant_deser_code(
                            gen_scope,
                            types,
                            name,
                            variant,
                            variant.key.as_ref().unwrap(),
                            &enum_gen_info,
                            cli,
                        );
                        let mut variant_deser = Block::new(
                            "let variant_deser = (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>",
                        );
                        variant_deser.after(")(raw);");
                        variant_deser.push_all(variant_deser_code.content);
                        deser_body.push_block(variant_deser);
                        let mut return_if_deserialized = Block::new("match variant_deser");
                        return_if_deserialized.line("Ok(variant) => return Ok(variant),");
                        return_if_deserialized
                    }
                    EnumVariantData::RustType(ty) => {
                        let mut return_if_deserialized = make_enum_variant_return_if_deserialized(
                            gen_scope,
                            types,
                            variant,
                            enum_gen_info.types.is_empty(),
                            rep.map(|r| {
                                let len_info = match ty.conceptual_type.resolve_alias_shallow() {
                                    ConceptualRustType::Rust(ident)
                                        if types.is_plain_group(ident) =>
                                    {
                                        types.rust_struct(ident).unwrap().cbor_len_info(types)
                                    }
                                    _ => RustStructCBORLen::Fixed(1),
                                };
                                (len_info, r)
                            }),
                            deser_body,
                            cli,
                        );
                        let names_without_outer = enum_gen_info.names_without_outer();
                        if names_without_outer.is_empty() {
                            return_if_deserialized
                                .line(format!("Ok(()) => return Ok({}::{}),", name, variant.name));
                        } else {
                            enum_gen_info.generate_constructor(
                                &mut return_if_deserialized,
                                &if names_without_outer.len() > 1 {
                                    format!(
                                        "Ok(({})) => return Ok(",
                                        names_without_outer.join(", ")
                                    )
                                } else {
                                    format!("Ok({}) => return Ok(", names_without_outer.join(", "))
                                },
                                "),",
                                None,
                            );
                        }
                        return_if_deserialized
                    }
                    EnumVariantData::Inlined(record) => {
                        let variant_deser_code = make_inline_deser_code(
                            gen_scope,
                            types,
                            name,
                            tag,
                            record,
                            &enum_gen_info,
                            cli,
                        );
                        let mut variant_deser = Block::new(
                            "let variant_deser = (|raw: &mut Deserializer<_>| -> Result<_, DeserializeError>",
                        );
                        variant_deser.after(")(raw);");
                        variant_deser.push_all(variant_deser_code.content);
                        deser_body.push_block(variant_deser);
                        // can't chain blocks so we just put them one after the other
                        let mut return_if_deserialized = Block::new("match variant_deser");
                        return_if_deserialized.line("Ok(variant) => return Ok(variant),");
                        return_if_deserialized
                    }
                };
                let mut variant_deser_failed_block = Block::new("Err(e) =>");
                variant_deser_failed_block
                    .line(format!("errs.push(e.annotate(\"{}\"));", variant.name))
                    .line("raw.as_mut_ref().seek(SeekFrom::Start(initial_position)).unwrap();");
                return_if_deserialized.push_block(variant_deser_failed_block);
                return_if_deserialized.after(";");
                deser_body.push_block(return_if_deserialized);
            }
        }
    }
    ser_func.push_block(ser_array_match_block);
    ser_impl.push_fn(ser_func);
    match non_overlapping_types_match {
        Some((mut deser_type_match, deser_covers_all_types)) => {
            if !deser_covers_all_types {
                deser_type_match.line(format!(
                    "_ => Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatched)),"
                ));
            }
            deser_body.push_block(deser_type_match);
        }
        None => {
            deser_body.line(&format!(
                "Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatchedWithCauses(errs)))"
            ));
        }
    }
    if cli.annotate_fields {
        deser_func.push_block(error_annotator);
    }
    deser_impl.push_fn(deser_func);
    // TODO: should we stick this in another scope somewhere or not? it's not exposed to wasm
    // however, clients expanding upon the generated lib might find it of use to change.
    gen_scope.rust(types, name).push_enum(e).push_impl(e_impl);
    gen_scope
        .rust_serialize(types, name)
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// First name in a deterministic candidate sequence that does NOT collide with a defined type
/// ident: `base` (`"W"`/`"R"`), then `base+suffix` (`"WSer"`/`"RDe"`), then `base+suffix+index`
/// (`"WSer0"`, `"WSer1"`, …). The bare `base` wins whenever nothing is named it, so a spec with no
/// `w`/`r` collision keeps the historical `"W"`/`"R"` names and the snapshot corpus does not churn.
fn pick_generic_name(
    taken: &std::collections::BTreeSet<String>,
    base: &str,
    suffix: &str,
) -> String {
    if !taken.contains(base) {
        return base.to_string();
    }
    let combined = format!("{base}{suffix}");
    if !taken.contains(&combined) {
        return combined;
    }
    (0..)
        .map(|i| format!("{base}{suffix}{i}"))
        .find(|candidate| !taken.contains(candidate))
        .expect("infinite candidate sequence always yields a free name")
}

fn make_serialization_function(name: &str, writer: &str, cli: &Cli) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f.generic(format!("'se, {writer}: Write"))
        .ret(format!("cbor_event::Result<&'se mut Serializer<{writer}>>"))
        .arg_ref_self()
        .arg("serializer", format!("&'se mut Serializer<{writer}>"));
    if cli.preserve_encodings && cli.canonical_form {
        f.arg("force_canonical", "bool");
    }
    f
}

fn make_serialization_impl(name: &str, cli: &Cli) -> codegen::Impl {
    let mut ser_impl = codegen::Impl::new(name);
    if cli.preserve_encodings && cli.canonical_form {
        ser_impl.impl_trait("Serialize");
    } else {
        ser_impl.impl_trait("cbor_event::se::Serialize");
    }
    ser_impl
}

fn make_deserialization_function(name: &str, reader: &str, cli: &Cli) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f.generic(format!("{reader}: BufRead + Seek"))
        .ret("Result<Self, DeserializeError>")
        .arg("raw", format!("&mut Deserializer<{reader}>"));
    // Opt-in recursion depth guard: the first statement of every composite `deserialize` acquires
    // an RAII guard whose Drop restores the thread-local depth on any return path (including `?`).
    // Bound in the outer function scope so it stays alive across the annotator closure the body may
    // be wrapped in. Only the top-level `deserialize` is guarded (not `deserialize_as_embedded_group`,
    // which is part of the same logical type and reached with the guard already held). The limit is
    // baked at generation time from the flag.
    if name == "deserialize"
        && let Some(limit) = cli.deserialize_depth_limit
    {
        f.line(format!(
            "let _depth_guard = DepthGuard::acquire({limit}usize)?;"
        ));
    }
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
        tag_check.line(format!("return Err(DeserializeError::new(\"{ident}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        deser_func.push_block(tag_check);
    }
}

// This is used mostly for when thing are tagged have specific ranges.
#[allow(clippy::too_many_arguments)]
fn generate_wrapper_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    type_name: &RustIdent,
    field_type: &RustType,
    min_max: Option<(Option<i128>, Option<i128>)>,
    float_min_max: Option<crate::intermediate::FloatWindow>,
    struct_config: &RustStructConfig,
    cli: &Cli,
) {
    if min_max.is_some() || float_min_max.is_some() {
        assert!(types.can_new_fail(type_name));
    }
    // The inner-value getter name: an explicit `@newtype <name>` renames it, otherwise every
    // wrapper (bare tag, plain `@newtype`, bounded/range) exposes the inner value under `get`.
    let getter_name = match struct_config.newtype_getter.as_ref() {
        Some(Some(name)) => name.as_str(),
        _ => "get",
    };
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, type_name, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .arg("inner", field_type.for_wasm_param(types))
            .vis("pub");

        // Delegate to the rust wrapper's `new`, mirroring the enum-variant wasm ctor: convert the
        // wasm inner to the rust inner (fallibility, if any, lives in the rust `new`, so pass
        // can_fail = false here), then let the rust ctor produce the native wrapper. Building
        // `Self(inner.into())` directly would need two chained `.into()`s for a Rust-typed inner
        // (wasm→native inner, then native inner→native wrapper) with an uninferable middle type.
        let from_wasm_expr = field_type.from_wasm_boundary_clone(types, "inner", false);
        let ctor = format!(
            "{}::new({})",
            rust_crate_struct_from_wasm(types, type_name, cli),
            ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
        );
        if types.can_new_fail(type_name) {
            // you can't use Self in a parameter in wasm_bindgen for some reason
            wasm_new
                .ret(format!("Result<{type_name}, JsError>"))
                .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
        } else {
            wasm_new.ret("Self").line(format!("Self({ctor})"));
        }
        wrapper.s_impl.push_fn(wasm_new);
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub")
            .arg_ref_self()
            .ret(field_type.for_wasm_return(types))
            .line(field_type.to_wasm_boundary(types, &format!("self.0.{getter_name}()"), false));
        wrapper.s_impl.push_fn(get);
        wrapper.push(gen_scope, types);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(types, type_name, true, cli);
    let (inner_var, self_var) = if cli.preserve_encodings {
        ("inner", "self.inner")
    } else {
        ("0", "self.0")
    };

    // manual JSON impls
    let mut serde_ser_impl = codegen::Impl::new(type_name);
    let mut serde_deser_impl = codegen::Impl::new(type_name);
    let mut json_schema_impl = codegen::Impl::new(type_name);
    let json_hex_bytes = matches!(
        field_type.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::Bytes)
    );
    let json_schema_type = if json_hex_bytes {
        Cow::Borrowed("String")
    } else {
        Cow::Owned(field_type.for_rust_member(types, false, cli))
    };

    if !struct_config.custom_json {
        // serde Serialize / Deserialize
        if cli.json_serde_derives {
            let mut serde_ser_fn = codegen::Function::new("serialize");
            serde_ser_fn
                .generic("S")
                .bound("S", "serde::Serializer")
                .arg_ref_self()
                .arg("serializer", "S")
                .ret("Result<S::Ok, S::Error>");
            let mut serde_deser_fn = codegen::Function::new("deserialize");
            serde_deser_fn
                .generic("D")
                .bound("D", "serde::de::Deserializer<'de>")
                .arg("deserializer", "D")
                .ret("Result<Self, D::Error>");
            if json_hex_bytes {
                serde_ser_fn.line(format!(
                    "serializer.serialize_str(&hex::encode({self_var}.clone()))"
                ));
                let err_body = "{ serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid hex bytes\") }";
                serde_deser_fn
                    .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
                    .line("hex::decode(&s)");
                if types.can_new_fail(type_name) {
                    serde_deser_fn
                        .line(format!(
                            ".ok().and_then(|bytes| {type_name}::new(bytes).ok())"
                        ))
                        .line(format!(".ok_or_else(|| {err_body})"));
                } else {
                    serde_deser_fn
                        .line(format!(".map({type_name}::new)"))
                        .line(format!(".map_err(|_e| {err_body})"));
                }
            } else {
                serde_ser_fn.line(format!("{self_var}.serialize(serializer)"));
                serde_deser_fn
                    .line(format!("let inner = <{json_schema_type} as serde::de::Deserialize>::deserialize(deserializer)?;"));
                if types.can_new_fail(type_name) {
                    let unexpected = match field_type.resolve_alias_shallow() {
                        ConceptualRustType::Alias(_, _) => unreachable!(),
                        ConceptualRustType::Array(_) => "Seq",
                        ConceptualRustType::Fixed(fixed) => match fixed {
                            FixedValue::Bool(_) => "Bool(inner)",
                            FixedValue::Float(_) => "Float(inner)",
                            FixedValue::Nint(_) => "Signed(inner as i64)",
                            FixedValue::Null => "Option",
                            FixedValue::Text(_) => "Str(&inner)",
                            FixedValue::Uint(_) => "Unsigned(inner)",
                        },
                        ConceptualRustType::Map(_, _) => "Map",
                        ConceptualRustType::Optional(_) => "Option",
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bool => "Bool(inner)",
                            Primitive::Bytes => "Bytes(&inner)",
                            Primitive::F32 => "Float(inner as f64)",
                            Primitive::F64 => "Float(inner)",
                            Primitive::I8
                            | Primitive::I16
                            | Primitive::I32
                            | Primitive::I64
                            | Primitive::N64 => "Signed(inner as i64)",
                            Primitive::Str => "Str(&inner)",
                            Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                                "Unsigned(inner as u64)"
                            }
                            Primitive::U64 => "Unsigned(inner)",
                        },
                        ConceptualRustType::Rust(_) => "StructVariant",
                    };
                    // Unexpected::Str(&inner)/Bytes(&inner) borrow `inner` in the error closure,
                    // but `Self::new(inner)` moves it first (String/Vec aren't Copy) → E0382. Clone
                    // into the constructor in that case so the original survives for the error. The
                    // other (Copy) variants need no clone.
                    let new_arg = if unexpected.contains("&inner") {
                        "inner.clone()"
                    } else {
                        "inner"
                    };
                    serde_deser_fn
                        .line(format!("Self::new({new_arg})"))
                        .line(format!(".map_err(|_e| {{ serde::de::Error::invalid_value(serde::de::Unexpected::{unexpected}, &\"invalid {type_name}\") }})"));
                } else {
                    serde_deser_fn.line("Ok(Self::new(inner))");
                }
            }
            serde_ser_impl
                .impl_trait("serde::Serialize")
                .push_fn(serde_ser_fn);
            serde_deser_impl
                .impl_trait("serde::de::Deserialize<'de>")
                .generic("'de")
                .push_fn(serde_deser_fn);
        }

        // JsonSchema
        if cli.json_schema_export {
            let mut schema_name_fn = codegen::Function::new("schema_name");
            schema_name_fn
                .ret("::std::borrow::Cow<'static, str>")
                .line(format!("::std::borrow::Cow::Borrowed(\"{type_name}\")"));
            let mut json_schema_fn = codegen::Function::new("json_schema");
            json_schema_fn
                .arg("generator", "&mut schemars::SchemaGenerator")
                .ret("schemars::Schema")
                // qualified-path form: `json_schema_type` is a type-position spelling, so a generic
                // backing type (map/array @newtype) needs `<T as Trait>::method`, not `T::method`
                // (which parses `<` as a comparison in expression position). Matches the
                // `<{json_schema_type} as serde::de::Deserialize>::deserialize` precedent above.
                .line(format!(
                    "<{json_schema_type} as schemars::JsonSchema>::json_schema(generator)"
                ));
            let mut inline_schema = codegen::Function::new("inline_schema");
            inline_schema.ret("bool").line(format!(
                "<{json_schema_type} as schemars::JsonSchema>::inline_schema()"
            ));
            json_schema_impl
                .impl_trait("schemars::JsonSchema")
                .push_fn(schema_name_fn)
                .push_fn(json_schema_fn)
                .push_fn(inline_schema);
        }
    }
    s.vis("pub");
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{type_name}Encoding")));
    let enc_fields = if cli.preserve_encodings {
        // PRIVATE, matching the default profile's private tuple field: a pub `inner` would let
        // downstream code literal-construct or mutate the wrapper, bypassing the bound check
        // `new()` enforces. Access goes through the getter (same as default); `serialization.rs`
        // is a child module so it still reads/constructs the field directly.
        s.field("inner", field_type.for_rust_member(types, false, cli));
        let enc_fields = encoding_fields(
            types,
            "inner",
            &field_type.clone().resolve_aliases(),
            true,
            cli,
        );

        if !enc_fields.is_empty() {
            s.field(
                format!(
                    "{}pub encodings",
                    encoding_var_macros(types.used_as_key(type_name), true, cli)
                ),
                format!("Option<{encoding_name}>"),
            );
            let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
            for field_enc in &enc_fields {
                encoding_struct.field(
                    format!("pub {}", field_enc.field_name),
                    &field_enc.type_name,
                );
            }
            gen_scope
                .cbor_encodings(types, type_name)
                .push_struct(encoding_struct);
        }
        Some(enc_fields)
    } else {
        s.tuple_field(None, field_type.for_rust_member(types, false, cli));
        None
    };
    // TODO: is there a way to know if the encoding object is also copyable?
    if field_type.is_copy(types) && !cli.preserve_encodings {
        s.derive("Copy");
    }
    {
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub").arg_ref_self();
        if field_type.is_copy(types) {
            get.ret(field_type.for_rust_member(types, false, cli))
                .line(field_type.clone_if_not_copy(types, self_var));
        } else {
            get.ret(format!(
                "&{}",
                field_type.for_rust_member(types, false, cli)
            ))
            .line(format!("&{self_var}"));
        }
        s_impl.push_fn(get);
    }
    let mut ser_func = make_serialization_function("serialize", &gen_scope.serialize_generic, cli);
    let mut ser_impl = make_serialization_impl(type_name.as_ref(), cli);
    gen_scope.generate_serialize(
        types,
        field_type.into(),
        &mut ser_func,
        SerializeConfig::new(self_var, "inner")
            .is_end(true)
            .encoding_var_in_option_struct("self.encodings"),
        cli,
    );
    ser_impl.push_fn(ser_func);
    let mut deser_func =
        make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
    let mut deser_impl = codegen::Impl::new(type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let ConceptualRustType::Rust(id) = &field_type.conceptual_type
        && types.is_plain_group(id)
    {
        unimplemented!(
            "TODO: make len/read_len variables of appropriate sizes so the generated code compiles"
        );
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move(types, cli))
        .vis("pub");
    let var_names_str = if cli.preserve_encodings {
        encoding_var_names_str(types, "inner", field_type, cli)
    } else {
        "inner".to_owned()
    };
    // nint is stored as its u64 magnitude, and magnitude is a *decreasing* function of the value, so
    // a value bound maps to a SWAPPED magnitude bound (`nint_bounds_to_u64`) — the same transform the
    // struct-field / setter paths apply. Without it the wrapper's `new()`/deserialize check compares
    // the u64 `inner` against a negative literal (does not compile: E0600) with inverted semantics.
    let min_max = if matches!(
        &field_type.conceptual_type,
        ConceptualRustType::Primitive(Primitive::N64)
    ) && !field_type
        .encodings
        .contains(&CBOREncodingOperation::CBORBytes)
    {
        min_max.map(|mm| nint_bounds_to_u64(&mm))
    } else {
        min_max
    };
    let from_impl = if min_max.is_some() || float_min_max.is_some() {
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
                cli,
            )
            .add_to(&mut deser_func);

        let check = if let Some(window) = float_min_max {
            // NaN-safe float window: accept-form negation, value compared as f64 so the authored
            // decimal literal is exact. Reports the ORIGINAL window with its per-side exclusivity.
            let cast_f64 = matches!(
                &field_type.conceptual_type,
                ConceptualRustType::Primitive(Primitive::F32)
            );
            let mut check = Block::new(format!(
                "if !({})",
                float_accept_cond(&window, "inner", cast_f64)
            ));
            let opt = |side: Option<(f64, bool)>| match side {
                Some((v, _)) => format!("Some({})", float_literal(v)),
                None => "None".to_owned(),
            };
            let incl = |side: Option<(f64, bool)>| match side {
                Some((_, exclusive)) => (!exclusive).to_string(),
                None => "false".to_owned(),
            };
            check.line(format!(
                "return Err(DeserializeError::new(\"{}\", DeserializeFailure::RangeCheckFloat{{ found: inner as f64, min: {}, max: {}, min_inclusive: {}, max_inclusive: {} }}));",
                type_name,
                opt(window.0),
                opt(window.1),
                incl(window.0),
                incl(window.1)
            ));
            check
        } else {
            let (min, max) = min_max.unwrap();
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
                (Some(min), Some(max)) => {
                    if min == max {
                        Block::new(format!("if {against} != {min}"))
                    } else if min > max {
                        // `.ne N` is encoded as Range(N+1, N-1): an exclusion, not a window
                        Block::new(format!("if {against} == {}", min - 1))
                    } else {
                        let non_negative = field_type.encodings.is_empty()
                            && match &field_type.conceptual_type {
                                ConceptualRustType::Primitive(p) => match p {
                                    Primitive::Bytes | Primitive::Str => true,
                                    Primitive::Bool
                                    | Primitive::U8
                                    | Primitive::U16
                                    | Primitive::U32
                                    | Primitive::U64 => true,
                                    Primitive::I8
                                    | Primitive::I16
                                    | Primitive::I32
                                    | Primitive::I64
                                    | Primitive::N64
                                    | Primitive::F32
                                    | Primitive::F64 => false,
                                },
                                _ => unimplemented!(),
                            };
                        if min == 0 && non_negative {
                            Block::new(format!("if {against} > {max}"))
                        } else {
                            Block::new(format!("if {against} < {min} || {against} > {max}"))
                        }
                    }
                }
                (Some(min), None) => Block::new(format!("if {against} < {min}")),
                (None, Some(max)) => Block::new(format!("if {against} > {max}")),
                (None, None) => panic!(
                    "How did we end up with a range requirement of (None, None)? Entire thing should've been None then"
                ),
            };
            check.line(format!(
                "return Err(DeserializeError::new(\"{}\", DeserializeFailure::RangeCheck{{ found: {} as isize, min: {}, max: {} }}));",
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
            check
        };
        deser_func.push_block(check.clone());
        new_func
            .ret("Result<Self, DeserializeError>")
            .push_block(check);
        if let Some(enc_fields) = &enc_fields {
            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_func.push_block(deser_ctor);

            let mut ctor_block = Block::new("Ok(Self");
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
                field_type.for_rust_member(types, false, cli)
            ))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_member(types, false, cli))
            .ret("Result<Self, Self::Error>")
            .line(format!(
                "{}::new({})",
                type_name,
                ToWasmBoundaryOperations::format(
                    field_type
                        .from_wasm_boundary_clone(types, "inner", false)
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
                    cli,
                )
                .add_to(&mut deser_func);

            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_func.push_block(deser_ctor);

            let mut ctor_block = Block::new("Self");
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
                    cli,
                )
                .add_to(&mut deser_func);
            new_func.line("Self(inner)");
        }

        let mut from = codegen::Impl::new(type_name.to_string());
        from.impl_trait(format!(
            "From<{}>",
            field_type.for_rust_member(types, false, cli)
        ))
        .new_fn("from")
        .arg("inner", field_type.for_rust_member(types, false, cli))
        .ret("Self")
        .line(format!(
            "{}::new({})",
            type_name,
            ToWasmBoundaryOperations::format(
                field_type
                    .from_wasm_boundary_clone(types, "inner", false)
                    .into_iter()
            )
        ));
        from
    };
    deser_impl.push_fn(deser_func);
    s_impl.push_fn(new_func);
    let mut from_inner_impl = codegen::Impl::new(field_type.for_rust_member(types, false, cli));
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
    if !struct_config.custom_json {
        if cli.json_serde_derives {
            gen_scope
                .rust(types, type_name)
                .push_impl(serde_ser_impl)
                .push_impl(serde_deser_impl);
        }
        if cli.json_schema_export {
            gen_scope.rust(types, type_name).push_impl(json_schema_impl);
        }
    }
    gen_scope
        .rust_serialize(types, type_name)
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// the derivative crate doesn't accept Eq="ignore" but omitting it
/// seems to behave correctly
fn key_derives(for_ignore: bool, cli: &Cli) -> &'static [&'static str] {
    if for_ignore {
        if cli.preserve_encodings {
            &["PartialEq", "Ord", "PartialOrd", "Hash"]
        } else {
            &["PartialEq", "Ord", "PartialOrd"]
        }
    } else if cli.preserve_encodings {
        &["Eq", "PartialEq", "Ord", "PartialOrd", "Hash"]
    } else {
        &["Eq", "PartialEq", "Ord", "PartialOrd"]
    }
}

fn add_struct_derives<T: DataType>(
    data_type: &mut T,
    used_in_key: bool,
    is_enum: bool,
    custom_json: bool,
    cli: &Cli,
) {
    data_type.derive("Clone").derive("Debug");
    if !custom_json {
        if cli.json_serde_derives {
            data_type
                .derive("serde::Deserialize")
                .derive("serde::Serialize");
        }
        if cli.json_schema_export {
            data_type.derive("schemars::JsonSchema");
        }
    }
    if used_in_key {
        if cli.preserve_encodings {
            // there's no way to do non-derive() proc macros in the codegen
            // cate so we must sadly use a newline like this. codegen manages indentation
            data_type.derive(&format!(
                "derivative::Derivative)]\n#[derivative({}",
                key_derives(false, cli)
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
            for key_derive in key_derives(false, cli) {
                data_type.derive(key_derive);
            }
        }
    }
}

fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes, cli: &Cli) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, &ident, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        let mut new_if = Block::new("if x >= 0");
        let mut new_else = Block::new("else");
        new_if.line(format!(
            "Self({}::Int::new_uint(x as u64))",
            cli.lib_name_code()
        ));
        new_else.line(format!(
            "Self({}::Int::new_nint((x + 1).unsigned_abs()))",
            cli.lib_name_code()
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
            .ret("Result<Int, JsError>")
            .line("// have to redefine so it's visible in WASM")
            .line("std::str::FromStr::from_str(string).map(Self).map_err(|e| JsError::new(&format!(\"Int.from_str({}): {:?}\", string, e)))");

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
    if cli.preserve_encodings {
        uint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.used_as_key(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
        nint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.used_as_key(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
    } else {
        uint.tuple("u64");
        nint.tuple("u64");
    }
    native_struct.push_variant(uint);
    native_struct.push_variant(nint);
    add_struct_derives(
        &mut native_struct,
        types.used_as_key(&ident),
        /* is_enum */ true,
        /* custom_json */ true,
        cli,
    );

    // JSON: Int's serde/schemars impls are written here by hand (the `custom_json` arg above) rather
    // than derived — the derived enum form would leak the CBOR encoding quirk (`{"Nint":4}` actually
    // meaning -5). Serialize as the signed decimal string (via Display/FromStr): it matches Int's own
    // to_str/from_str API and safely covers the full [-2^64, 2^64) range a JSON number can't hold.
    if cli.json_serde_derives {
        let mut serde_ser_impl = codegen::Impl::new("Int");
        let mut serde_ser_fn = codegen::Function::new("serialize");
        serde_ser_fn
            .generic("S")
            .bound("S", "serde::Serializer")
            .arg_ref_self()
            .arg("serializer", "S")
            .ret("Result<S::Ok, S::Error>")
            .line("serializer.serialize_str(&self.to_string())");
        serde_ser_impl
            .impl_trait("serde::Serialize")
            .push_fn(serde_ser_fn);
        gen_scope.rust_lib().push_impl(serde_ser_impl);

        let mut serde_deser_impl = codegen::Impl::new("Int");
        let mut serde_deser_fn = codegen::Function::new("deserialize");
        serde_deser_fn
            .generic("D")
            .bound("D", "serde::de::Deserializer<'de>")
            .arg("deserializer", "D")
            .ret("Result<Self, D::Error>")
            .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
            .line("std::str::FromStr::from_str(&s).map_err(|_e| serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid Int\"))");
        serde_deser_impl
            .impl_trait("serde::de::Deserialize<'de>")
            .generic("'de")
            .push_fn(serde_deser_fn);
        gen_scope.rust_lib().push_impl(serde_deser_impl);
    }
    if cli.json_schema_export {
        let mut json_schema_impl = codegen::Impl::new("Int");
        let mut schema_name_fn = codegen::Function::new("schema_name");
        schema_name_fn
            .ret("::std::borrow::Cow<'static, str>")
            .line("::std::borrow::Cow::Borrowed(\"Int\")");
        let mut json_schema_fn = codegen::Function::new("json_schema");
        json_schema_fn
            .arg("generator", "&mut schemars::SchemaGenerator")
            .ret("schemars::Schema")
            .line("String::json_schema(generator)");
        let mut inline_schema_fn = codegen::Function::new("inline_schema");
        inline_schema_fn.ret("bool").line("String::inline_schema()");
        json_schema_impl
            .impl_trait("schemars::JsonSchema")
            .push_fn(schema_name_fn)
            .push_fn(json_schema_fn)
            .push_fn(inline_schema_fn);
        gen_scope.rust_lib().push_impl(json_schema_impl);
    }

    // impl Int
    let mut native_impl = codegen::Impl::new("Int");
    let mut new_uint = codegen::Function::new("new_uint");
    new_uint.vis("pub").arg("value", "u64").ret("Self");
    if cli.preserve_encodings {
        let mut new_uint_ctor = Block::new("Self::Uint");
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
    if cli.preserve_encodings {
        let mut new_nint_ctor = Block::new("Self::Nint");
        new_nint_ctor.line("value,").line("encoding: None,");
        new_nint.push_block(new_nint_ctor);
    } else {
        new_nint.line("Self::Nint(value)");
    }
    native_impl.push_fn(new_nint);

    // serialization
    let mut ser_impl = make_serialization_impl("Int", cli);
    let mut ser_func = make_serialization_function("serialize", &gen_scope.serialize_generic, cli);
    let mut ser_block = Block::new("match self");
    if cli.preserve_encodings {
        ser_block
            .line(format!("Self::Uint{{ value, encoding }} => serializer.write_unsigned_integer_sz(*value, fit_sz(*value, *encoding{})),", canonical_param(cli)))
            .line(format!("Self::Nint{{ value, encoding }} => serializer.write_negative_integer_sz(-((*value as i128) + 1), fit_sz(*value, *encoding{})),", canonical_param(cli)));
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
    let mut deser_func =
        make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
    let mut annotate = make_err_annotate_block("Int", "", "");
    let mut deser_match = Block::new("match raw.cbor_type()?");
    if cli.preserve_encodings {
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
    let mut display_match = Block::new("match self");
    // Nint: RFC 8949 §3.1 says a major-type-1 value is `-1 - argument`, range -2^64..=-1. The
    // argument is held as u64, so the most-negative Int (argument u64::MAX -> -2^64) overflows
    // i64/u64; compute the signed value in i128 (same idiom as the serialize path below). Both
    // branches below do this; they differ only in field syntax (named under preserve, else tuple).
    if cli.preserve_encodings {
        display_match
            .line("Self::Uint{ value, .. } => write!(f, \"{}\", value),")
            .line("Self::Nint{ value, .. } => write!(f, \"{}\", -((*value as i128) + 1)),");
    } else {
        display_match
            .line("Self::Uint(x) => write!(f, \"{}\", x),")
            .line("Self::Nint(x) => write!(f, \"{}\", -((*x as i128) + 1)),");
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
    let mut try_from_if = Block::new("if x >= 0");
    let mut try_from_else = Block::new("else");
    if cli.preserve_encodings {
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
        .rust_serialize_lib()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// Gets the rustfmt path to rustfmt the generated bindings.
fn rustfmt_path() -> std::io::Result<std::path::PathBuf> {
    if let Ok(rustfmt) = std::env::var("RUSTFMT") {
        return Ok(rustfmt.into());
    }
    #[cfg(feature = "which-rustfmt")]
    match which::which("rustfmt") {
        Ok(p) => Ok(p),
        Err(e) => Err(std::io::Error::other(format!("{e}"))),
    }
    #[cfg(not(feature = "which-rustfmt"))]
    Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        "which wasn't enabled, and no rustfmt binary specified",
    ))
}

/// Runs rustfmt on the string
pub fn rustfmt_generated_string(source: &str) -> std::io::Result<Cow<'_, str>> {
    let mut cmd = Command::new(rustfmt_path().unwrap());
    cmd.stdin(Stdio::piped()).stdout(Stdio::piped());

    // cmd.args(&["--config-path", path]);

    let mut child = cmd.spawn()?;
    let mut child_stdin = child.stdin.take().unwrap();
    let mut child_stdout = child.stdout.take().unwrap();

    let source = source.to_owned();

    // Write to stdin in a new thread, so that we can read from stdout on this
    // thread. This keeps the child from blocking on writing to its stdout which
    // might block us from writing to its stdin.
    let stdin_handle = std::thread::spawn(move || {
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
            // exit 2 = rustfmt could not PARSE the input: the generator emitted invalid Rust. This
            // used to be swallowed (return the unformatted source, exit 0), which is exactly how the
            // JSON-schema turbofish bug shipped green. Fail loud instead — the rustfmt errors are on
            // stderr (inherited) above; a parse failure is always a generator bug, never benign.
            Some(2) => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "rustfmt rejected the generated source as unparseable — this is a generator bug \
                 (see the rustfmt errors above)",
            )),
            // exit 3 = formatted fine but gave up on SOME lines: the output is still valid Rust, so
            // keep it (not a correctness problem, just cosmetic).
            Some(3) => {
                println!("Rustfmt could not format some lines.");
                Ok(Cow::Owned(bindings))
            }
            // any other exit (rustfmt internal error) — the turbofish bug actually hit this arm, not
            // exit 2 — also indicates the generator fed rustfmt something it couldn't handle. Fatal.
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "rustfmt failed on the generated source (internal error) — likely invalid Rust \
                 emitted; this is a generator bug (see the rustfmt output above)",
            )),
        },
        _ => Ok(Cow::Owned(source)),
    }
}
