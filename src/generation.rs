use crate::cli::Cli;
use crate::comment_ast::DemandSet;
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
use crate::utils::{cbor_type_code_str, convert_to_camel_case, convert_to_snake_case};

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

/// The code-generation provenance banner stamped at the top of every generated `.rs` file in the
/// tool-owned generated trees. Ends with a newline so it prepends cleanly onto rustfmt'd content.
/// `pub(crate)` so the `generated_files_start_with_header` gate asserts against the same banner
/// and path family the stamper uses (a private copy in the test would drift silently).
pub(crate) const CODEGEN_HEADER: &str = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n\n";

/// True for the header-stamped scope families: the tool-owned generated trees under
/// `rust/src/generated/` and `wasm/src/generated/`. The seed-once crate roots (`*/src/lib.rs`),
/// the json-gen crate, and every `Cargo.toml` are deliberately left unstamped.
pub(crate) fn is_header_stamped_path(path: &str) -> bool {
    path.ends_with(".rs")
        && (path.starts_with("rust/src/generated/") || path.starts_with("wasm/src/generated/"))
}

/// True for a `.rs` file the comment-preservation overlay runs on: the tool-owned generated trees
/// (rust, wasm, json-gen) plus the json-gen `main.rs`, which is regenerated wholesale every run
/// (it is NOT seed-once, unlike the three `lib.rs` roots — those and every `Cargo.toml` are the
/// files deliberately outside the overlay).
pub(crate) fn is_preservable_generated_path(path: &str) -> bool {
    path == "wasm/json-gen/src/main.rs"
        || (path.ends_with(".rs")
            && (path.starts_with("rust/src/generated/")
                || path.starts_with("wasm/src/generated/")
                || path.starts_with("wasm/json-gen/src/generated/")))
}

/// The preserve-or-clobber write every overlay-covered `.rs` goes through — the common write loop
/// and the four static runtime files (`error.rs`, `ordered_hash_map.rs`, `non_empty.rs`,
/// `non_empty_map.rs`) alike, so the "all generated trees uniformly" promise holds. An existing
/// file that cannot be read (not UTF-8) or lexed is a hard error naming the file, never a silent
/// clobber. Only content that actually received an insertion pays the extra rustfmt pass.
fn write_rs_with_preserve(
    path: &std::path::Path,
    rel_path: &str,
    content: &str,
    preserve: bool,
) -> std::io::Result<()> {
    if preserve && path.exists() {
        let existing = std::fs::read_to_string(path).map_err(|e| {
            std::io::Error::other(format!(
                "{rel_path}: cannot read the existing generated file for comment preservation: \
                 {e}. Fix or delete the file, or pass --no-preserve-comments."
            ))
        })?;
        let preserved = crate::comment_preserve::preserve(&existing, content)
            .map_err(|e| std::io::Error::other(e.render(rel_path)))?;
        if preserved.changed {
            std::fs::write(path, rustfmt_generated_string(&preserved.content)?.as_ref())?;
        } else {
            std::fs::write(path, content)?;
        }
        return Ok(());
    }
    std::fs::write(path, content)
}

/// The composed rust runtime static files (`error.rs`, `ordered_hash_map.rs`, `non_empty.rs`,
/// `non_empty_map.rs`) shared by the in-crate static export and the `--export-static-dir` path so
/// the two can't drift. Each returned entry is (bare filename, rustfmt'd content). The content
/// COMPOSITION (file concatenation, json/schemars companions, the preserve-encodings
/// BTreeMap→OrderedHashMap substitution for non_empty_map) is identical between the two callers —
/// only WHICH files appear differs: `include_non_empty_vec`/`include_non_empty_map` gate the two
/// NonEmpty runtimes on spec usage in-crate but are forced true for the exported dir (a pure
/// function of the flag set, not of the spec that happened to be run). `ordered_hash_map.rs` is
/// gated on `--preserve-encodings` for both. `serialization.rs` is deliberately NOT here: the
/// in-crate path appends the generated per-type impls to the prelude, while the export-dir path
/// writes the prelude only — each composes that file itself.
///
/// The content is rustfmt'd here (not at the write site) so both callers hand the
/// comment-preservation overlay identical, rustfmt-stable bytes: a preserve-rewrite is written
/// rustfmt'd, so raw content whose rustfmt form differs by a token (e.g. a static's block-arm
/// trailing comma) would make a later run's fresh tokens mismatch the written tokens and trap an
/// already-placed comment with no input change (pinned by
/// `comment_preservation_static_files_rustfmt_stable`).
fn composed_runtime_static_files(
    cli: &Cli,
    include_non_empty_vec: bool,
    include_non_empty_map: bool,
) -> std::io::Result<Vec<(String, String)>> {
    let mut out = Vec::new();

    // error.rs — always, verbatim static/error.rs + rustfmt.
    let error_rs = std::fs::read_to_string(cli.static_dir.join("error.rs"))?;
    out.push((
        "error.rs".to_owned(),
        rustfmt_generated_string(&error_rs)?.into_owned(),
    ));

    // ordered_hash_map.rs — iff --preserve-encodings, with the json/schemars companions appended
    // per the json flags.
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
        out.push((
            "ordered_hash_map.rs".to_owned(),
            rustfmt_generated_string(&ordered_hash_map_rs)?.into_owned(),
        ));
    }

    // non_empty.rs (the NonEmptyVec runtime). Its json/schemars companions append under the same
    // flags as the ordered_hash_map ones.
    if include_non_empty_vec {
        let mut non_empty_rs = std::fs::read_to_string(cli.static_dir.join("non_empty.rs"))?;
        if cli.json_serde_derives {
            non_empty_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            non_empty_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_schemars.rs"),
            )?);
        }
        out.push((
            "non_empty.rs".to_owned(),
            rustfmt_generated_string(&non_empty_rs)?.into_owned(),
        ));
    }

    // non_empty_map.rs (the NonEmptyMap runtime). Its inner map is the table type: BTreeMap by
    // default, and under --preserve-encodings a targeted substitution swaps it for OrderedHashMap
    // (import + type token + the extra `Hash + Eq` key bound the hash-map flavor requires),
    // following the ordered_hash_map flavoring precedent. Iteration stays deterministic either way.
    if include_non_empty_map {
        let mut non_empty_map_rs =
            std::fs::read_to_string(cli.static_dir.join("non_empty_map.rs"))?;
        if cli.json_serde_derives {
            non_empty_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_map_json.rs"),
            )?);
        }
        if cli.json_schema_export {
            non_empty_map_rs.push_str(&std::fs::read_to_string(
                cli.static_dir.join("non_empty_map_schemars.rs"),
            )?);
        }
        if cli.preserve_encodings {
            non_empty_map_rs = non_empty_map_rs
                .replace(
                    "use std::collections::BTreeMap;",
                    "use super::ordered_hash_map::OrderedHashMap;",
                )
                .replace("K: Ord", "K: Ord + core::hash::Hash + Eq")
                .replace("BTreeMap", "OrderedHashMap");
        }
        out.push((
            "non_empty_map.rs".to_owned(),
            rustfmt_generated_string(&non_empty_map_rs)?.into_owned(),
        ));
    }

    Ok(out)
}

/// Recursively collect every `.rs` file under `dir` (absent dir = no files). Drives the stale-file
/// scan at the end of [`GenerationScope::export`].
fn collect_rs_files(
    dir: &std::path::Path,
    out: &mut Vec<std::path::PathBuf>,
) -> std::io::Result<()> {
    if !dir.is_dir() {
        return Ok(());
    }
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_rs_files(&path, out)?;
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
    Ok(())
}

/// Prepend the codegen header onto a (already rustfmt'd) generated file's content. The header is
/// pure `//` comments, so it leads the file verbatim regardless of whether the body opens with an
/// inner `#![…]` attribute (both orderings are valid Rust; a comment may precede an inner attr).
fn stamp_codegen_header(content: &str) -> String {
    format!("{CODEGEN_HEADER}{content}")
}

/// If `line` is a line-leading top-level type-namespace definition — `pub struct`/`pub enum`/`pub
/// type` at column 0, exactly how `codegen` emits items at the file root — return the defined
/// ident. Drives the `generated_files` duplicate-ident backstop. The leading-anchor (no
/// `strip_prefix` for indented forms) excludes nested items inside `mod {}` blocks (indented) and
/// the anchor keywords exclude other namespaces (`impl`/`fn`/`use`), which never collide in the
/// type namespace. Returns `None` for anything else.
fn top_level_type_ident(line: &str) -> Option<&str> {
    let rest = line
        .strip_prefix("pub struct ")
        .or_else(|| line.strip_prefix("pub enum "))
        .or_else(|| line.strip_prefix("pub type "))?;
    let ident = rest
        .split(|c: char| !(c.is_alphanumeric() || c == '_'))
        .next()?;
    (!ident.is_empty()).then_some(ident)
}

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

    /// THE constructor for serializing a record field. Use this at every record-field serialize site
    /// rather than `new(..)` + hand-chained setter: it carries the field's `@custom_serialize`
    /// directive automatically. Forgetting to re-carry a custom directive when adding a new call site
    /// is a recurring bug class here, so this owns that carry in one place.
    fn for_field<S: Into<String>>(expr: S, field: &RustField) -> Self {
        let mut config = Self::new(expr, &field.name);
        if let Some(custom_serialize) = &field.rule_metadata.custom_serialize {
            config = config.custom_serialize(custom_serialize.clone());
        }
        config
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
            let default_tuple = tuple_str(
                encoding_fields
                    .iter()
                    .map(|enc| enc.default_expr.to_owned())
                    .collect(),
            );
            // An all-trivial-literal default tuple can use `unwrap_or(..)`; keep the lazy
            // `unwrap_or_else(|| ..)` when any element is a function call, or clippy::or_fun_call
            // fires instead (the same tension recorded at the `default_value` deserialize site).
            let unwrap_call = if encoding_defaults_all_trivial(encoding_fields) {
                format!("unwrap_or({default_tuple})")
            } else {
                format!("unwrap_or_else(|| {default_tuple})")
            };
            format!(
                "let {} = {}.{};",
                tuple_str(
                    encoding_fields
                        .iter()
                        .map(|enc| enc.field_name.clone())
                        .collect()
                ),
                encoding_lookup,
                unwrap_call
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

    /// THE constructor for deserializing a record field. Use this at every record-field deserialize
    /// site rather than `new(..)` + hand-chained setters: it carries the field's
    /// `@custom_deserialize` directive automatically. Forgetting to re-carry a custom directive when
    /// adding a new call site is a recurring bug class here, so this owns that carry in one place.
    fn for_field(field: &'a RustField, in_embedded: bool, optional: bool) -> Self {
        let mut config = Self::new(&field.name)
            .in_embedded(in_embedded)
            .optional_field(optional);
        if let Some(custom_deserialize) = &field.rule_metadata.custom_deserialize {
            config = config.custom_deserialize(custom_deserialize.clone());
        }
        config
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

pub(crate) fn concat_files<P: AsRef<Path>>(paths: &Vec<P>) -> std::io::Result<String> {
    let mut buf = String::new();
    for path in paths {
        buf.push_str(&std::fs::read_to_string(path).map_err(|e| {
            std::io::Error::new(
                e.kind(),
                format!("can't read {}: {e}", path.as_ref().display()),
            )
        })?);
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
    /// Every collection-wrapper CLASS the wasm crate actually minted this run, mapped to the
    /// `ModuleScope` it was emitted into. Recorded at the point of actual emission (inside each of
    /// the four wrapper emitters' `already_generated` success paths), so it equals EXACTLY the set
    /// of wrapper classes the crate owns — no more, no less. Materialized into
    /// `wasm/src/generated/collections.rs` (a `pub use` re-export index) by `generated_files`. A
    /// `BTreeMap` keeps the index deterministic (sorted by class name). Only populated under
    /// `--wasm`; unused otherwise.
    wasm_collection_wrappers: BTreeMap<RustIdent, ModuleScope>,
    /// Parsed `--extern-wrapper-index` inventories: extern-deps dependency name -> the set of
    /// collection-wrapper class names that dependency's own wasm crate already emits (read from its
    /// committed `generated/collections.rs`). Consulted when deciding whether a wrapper the consumer
    /// would mint should instead be deferred to the dependency. Empty unless the flag is passed.
    extern_wrapper_index: BTreeMap<String, BTreeSet<String>>,
    /// Collection wrappers the consumer is NOT minting this run because a mapped dependency already
    /// owns them (`--extern-wrapper-index`), keyed by the structural wrapper ident and mapped to the
    /// dependency's `collections` module scope (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/collections`,
    /// non-exported) the reference is imported from. Populated at each emitter's mint point during the
    /// wasm struct walk (before imports are computed), so `scope_references` can route a plain
    /// `use <dep_wasm>::collections::<Name>;` into every referencing module and the two keys()
    /// accessors know to construct via `.into()` cross-crate (R3d). Never records a wrapper into
    /// `wasm_collection_wrappers`, so a deferred wrapper stays out of the consumer's own index (R3e).
    deferred_wrappers: BTreeMap<RustIdent, ModuleScope>,
    /// Wrapper idents already named in a `--extern-wrapper-index` "candidate not in the dep's index"
    /// stderr warning, so the diagnostic fires at most once per wrapper across the walk.
    deferred_warned: BTreeSet<RustIdent>,
    /// Parsed `--workspace-dep` set (extern-deps directory names marked co-generated workspace
    /// members). A wrapper whose element types are ALL owned by one of these deps DEFERS
    /// UNCONDITIONALLY (no index consult) and is recorded in `borrowed_wrappers`. Empty unless the
    /// flag is passed; populated (and validated) in `generate()` under `--wasm` only.
    workspace_deps: BTreeSet<String>,
    /// Collection wrappers deferred to a workspace dep this run (`--workspace-dep`), keyed by the
    /// structural wrapper ident and mapped to `(dep rust-crate name, canonical CDDL shape)`. The
    /// mirror image of `wasm_collection_wrappers` ("what I provide" ↔ "what I borrow, from whom"),
    /// materialized into `wasm/src/generated/borrowed_collections.rs` for the dep's own generation to
    /// read. Recording is idempotent (the same wrapper is probed from several sites); two DISTINCT
    /// shapes deriving the SAME structural name is a hard error (the `MapAToBToC` reverse-ambiguity).
    borrowed_wrappers: BTreeMap<RustIdent, (String, String)>,
    /// W2 dep side (`--wrapper-requests`): the canonical CDDL shape (`render_wrapper_shape` output) of
    /// every collection wrapper this crate produces from its OWN spec, mapped to that wrapper's ident.
    /// Recorded at each emitter's actual mint point during the main walk (guarded off during requested
    /// emission). Answers "does the dep already produce this requested shape, and under what name?": a
    /// requested shape whose canonical form is a key here is own-spec-produced — satisfied when the
    /// ident is the structural name, a hard error when it is a different (rule-declared) name.
    own_wrapper_shapes: BTreeMap<String, RustIdent>,
    /// W2 dep side: while `Some`, `wasm()` / `record_collection_wrapper` route the wrapper being
    /// emitted into this scope (the `requested_collections` module) instead of `types.scope(ident)` —
    /// the requested wrappers are not in the dep's IR, so they have no natural scope. Set only around
    /// the requested-wrapper emission in `emit_requested_collections`; `None` everywhere else.
    requested_scope_override: Option<ModuleScope>,
    /// W2 dep side: attribution doc text (`Generated at the request of: …`) keyed by requested-wrapper
    /// ident. Consulted by `create_base_wasm_struct` (and prepended by the NonEmpty emitters, which set
    /// their own struct doc). Empty except during requested emission, so own-spec wrappers are
    /// unaffected (flag-off byte-identity).
    requested_attribution: BTreeMap<RustIdent, String>,
    /// W2 dep side: `true` when requested-wrapper emission produced a `[+ …]` / `{+ … => …}` wrapper
    /// whose NonEmpty runtime the dep's OWN spec does not otherwise pull in. The runtime-provisioning
    /// gates (`pub mod non_empty`/`non_empty_map` decl + static file copy) OR these in so the dep
    /// hosts a requested NonEmpty wrapper's `NonEmptyVec`/`NonEmptyMap` type. Never set off the flag.
    requested_non_empty_vec: bool,
    requested_non_empty_map: bool,
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
            wasm_collection_wrappers: BTreeMap::new(),
            extern_wrapper_index: BTreeMap::new(),
            deferred_wrappers: BTreeMap::new(),
            deferred_warned: BTreeSet::new(),
            workspace_deps: BTreeSet::new(),
            borrowed_wrappers: BTreeMap::new(),
            own_wrapper_shapes: BTreeMap::new(),
            requested_scope_override: None,
            requested_attribution: BTreeMap::new(),
            requested_non_empty_vec: false,
            requested_non_empty_map: false,
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

        // `--extern-wrapper-index`: read each mapped dependency's committed collection-wrapper index
        // (`generated/collections.rs`) so the wasm struct walk below can DEFER any wrapper the dep
        // already owns instead of re-minting it (a wasm duplicate-symbol link error otherwise).
        // Parsed once, up front, so it is available at every emitter's mint point. Only meaningful
        // under `--wasm`; a mapping naming a non-extern dependency is a hard error, mirroring
        // `--extern-wasm-crate` (a typo would otherwise silently disable deferral and reintroduce the
        // link error).
        if cli.wasm {
            self.extern_wrapper_index = load_extern_wrapper_indices(types, cli);
            self.workspace_deps = load_workspace_deps(types, cli);
        }

        // Type aliases
        for (alias_ident, alias_info) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias_ident {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if alias_info.gen_rust_alias
                    && !(cli.no_synthesized_rust_collection_aliases
                        && alias_info.synthesized_collection)
                {
                    let mut type_alias = TypeAlias::new(
                        ident,
                        alias_info.base_type.for_rust_member(types, false, cli),
                    );
                    type_alias.vis("pub");
                    // Decision 11 (two-type design doc): a named `[+ T]` rule's alias quotes the
                    // originating occurrence — the type name, doc comment, and TryFrom signature
                    // are three redundant discovery signals for the constraint.
                    if alias_info.base_type.is_non_empty_array()
                        && let ConceptualRustType::Array(elem) =
                            &alias_info.base_type.conceptual_type
                    {
                        type_alias.doc(format!(
                            "`[+ {}]`: at least one element, enforced at the `NonEmptyVec` \
                             `TryFrom<Vec<_>>` door (the CBOR decoder routes through the same \
                             door, so wire-side and API-side rejection are identical).",
                            elem.for_rust_member(types, false, cli)
                        ));
                    }
                    // map-side twin: a named `{+ k => v}` rule's alias quotes the occurrence too.
                    if alias_info.base_type.is_non_empty_map()
                        && let ConceptualRustType::Map(k, v) = &alias_info.base_type.conceptual_type
                    {
                        type_alias.doc(format!(
                            "`{{+ {} => {}}}`: at least one entry, enforced at the `NonEmptyMap` \
                             `TryFrom` door (the CBOR decoder routes through the same door, so \
                             wire-side and API-side rejection are identical).",
                            k.for_rust_member(types, false, cli),
                            v.for_rust_member(types, false, cli)
                        ));
                    }
                    self.rust(types, ident).push_type_alias(type_alias);
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
                                    && !alias_info.base_type.directly_wasm_exposable(types)
                            })
                            .map(|target| target.to_string())
                            .unwrap_or_else(|| alias_info.base_type.for_wasm_member(types));
                        self.wasm(types, ident)
                            .push_type_alias(TypeAlias::new(ident, wasm_target).vis("pub").clone());
                    }
                    // A type-alias BASE can carry an inline `[+ T]` / `{+ k => v}` shape that only
                    // this alias reaches — e.g. `x = bytes .cbor [+ uint]` classifies as a plain
                    // alias (not a `RustStructType::Array`), so the rust_structs minting walk below
                    // never visits it, while the wasm alias line above names the restricted wrapper
                    // (`pub type X = NonEmptyU64List;`). Mint the wrappers the base needs here; the
                    // dedup-to-named and `already_generated` guards inside apply as everywhere else,
                    // so a base whose shape a named rule owns dedups instead of double-minting.
                    // (Found by the recombination wasm sweep: rc1205's `NonEmptyU64List` was
                    // referenced but never emitted — E0425 with generation exit 0.)
                    if cli.wasm {
                        self.ensure_non_empty_wrappers(types, &alias_info.base_type, cli);
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

            // Shapes owned by EXACTLY ONE named table rule: their embedded/resolved uses share the
            // rule-named class (a real `#[wasm_bindgen]` class under the CDDL identifier), and the
            // structural `MapKToV` name becomes a `pub type` alias to it. Same-shape rule PAIRS (2+
            // owners) and anonymous-only shapes are absent — they keep the structural fallback class
            // at the crate root. Shared with `scope_references`'s Map arm (import placement) via the
            // one helper so emission and import placement CANNOT disagree.
            let table_shape_sole_owner = types.table_shape_sole_owners();

            let mut wasm_wrappers_generated = BTreeSet::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                assert_eq!(rust_ident, rust_struct.ident());
                if cli.wasm {
                    rust_struct.visit_types_excluding(
                        types,
                        &mut |ty| {
                            mint_wasm_wrapper_for_visited_type(
                                self,
                                types,
                                ty,
                                &mut wasm_wrappers_generated,
                                &table_shape_sole_owner,
                                cli,
                            )
                        },
                        &mut existing_aliases,
                    );
                    // The conceptual visitor above can't see array LENGTH bounds (they live on the
                    // RustType, stripped before it recurses), so mint the restricted `NonEmpty*List`
                    // wrappers for inline `[+ T]` shapes from a RustType-level walk that does.
                    match rust_struct.variant() {
                        RustStructType::Record(record) => {
                            for field in &record.fields {
                                self.ensure_non_empty_wrappers(types, &field.rust_type, cli);
                            }
                        }
                        RustStructType::Table { domain, range, .. } => {
                            // the named table's OWN restricted wrapper (`{+ k => v}`) is minted in
                            // the variant match below (under the rule ident); here just mint wrappers
                            // its domain/range need (nested `{+ …}` in a key or value position)
                            self.ensure_non_empty_wrappers(types, domain, cli);
                            self.ensure_non_empty_wrappers(types, range, cli);
                        }
                        RustStructType::Wrapper { wrapped, .. } => {
                            self.ensure_non_empty_wrappers(types, wrapped, cli);
                        }
                        RustStructType::GroupChoice { variants, .. }
                        | RustStructType::TypeChoice { variants } => {
                            for v in variants {
                                match &v.data {
                                    EnumVariantData::RustType(t) => {
                                        self.ensure_non_empty_wrappers(types, t, cli)
                                    }
                                    EnumVariantData::Inlined(rec) => {
                                        for f in &rec.fields {
                                            self.ensure_non_empty_wrappers(
                                                types,
                                                &f.rust_type,
                                                cli,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        RustStructType::Array { element_type, .. } => {
                            // the named rule's own wrapper is minted in the variant match below;
                            // here just mint wrappers its element needs (nested `[+ [+ int]]`)
                            self.ensure_non_empty_wrappers(types, element_type, cli);
                        }
                        _ => (),
                    }
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
                    RustStructType::Table {
                        domain,
                        range,
                        bounds,
                    } => {
                        if cli.wasm && *bounds == Some((Some(1), None)) {
                            // named `{+ k => v}` rule: its JS class is the RESTRICTED wrapper
                            // (wrapping core::NonEmptyMap) under the rule ident, not the loose table
                            // wrapper — the map-side twin of the named `[+ T]` array arm.
                            self.generate_non_empty_map_type(
                                types,
                                domain.clone(),
                                range.clone(),
                                rust_ident,
                                true,
                                cli,
                            );
                        } else if cli.wasm {
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
                                    true,
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), ConceptualRustType::name_for_rust_map(domain, range, false)));
                    }
                    RustStructType::Array {
                        element_type,
                        bounds,
                    } => {
                        if cli.wasm {
                            if *bounds == Some((Some(1), None)) {
                                // named `[+ T]` rule: its JS class is the RESTRICTED wrapper (wrapping
                                // core::NonEmptyVec) under the rule ident, not the loose list wrapper.
                                self.generate_non_empty_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    true,
                                    cli,
                                );
                            } else {
                                self.generate_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    true,
                                    cli,
                                );
                            }
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

            // Structural wrappers reachable ONLY through a wasm-emitted plain `pub type` alias, never
            // through any rust struct — e.g. `x = bytes .cbor { bignint => uint }`, where `x` is a type
            // alias (not a struct). Its `Map` target is embedded elsewhere only as `Alias(Rust(x), Map)`,
            // and `x` sits in `existing_aliases`, so the rust-struct walk above never descends into that
            // Map — leaving the emitted `pub type X = MapKToV` alias naming a class no one minted. Walk
            // each wasm-alias base type through the same minting path (shared `wasm_wrappers_generated` /
            // `existing_aliases`, so it stays idempotent with the walk above and self-referential/other
            // named aliases are not re-descended).
            if cli.wasm {
                for (alias_ident, alias_info) in types.type_aliases() {
                    if matches!(alias_ident, AliasIdent::Rust(_)) && alias_info.gen_wasm_alias {
                        alias_info.base_type.conceptual_type.visit_types_excluding(
                            types,
                            &mut |ty| {
                                mint_wasm_wrapper_for_visited_type(
                                    self,
                                    types,
                                    ty,
                                    &mut wasm_wrappers_generated,
                                    &table_shape_sole_owner,
                                    cli,
                                )
                            },
                            &mut existing_aliases,
                        );
                    }
                }
            }
        }

        // `@used_as_elem`: mint the loose-list wasm wrapper (`<Elem>List`, the `[* elem]` equivalent)
        // for each tagged element, exactly as an inline `[* elem]` usage would. Runs AFTER the
        // own-spec wasm walk (so a real inline usage that already minted the wrapper dedups via the
        // shared `already_generated`) and BEFORE `emit_requested_collections` (so the wrapper is
        // recorded in `own_wrapper_shapes`, letting a consumer's request for the same shape be
        // satisfied by this crate's own spec instead of re-emitted into requested_collections). The
        // mark set is a `BTreeSet`, so this walks idents in sorted order — deterministic output. A
        // directly-wasm-exposable element has no wrapper and is rejected in `finalize`, so nothing
        // exposable reaches here. `try_defer_wrapper` inside applies normally: if a workspace dep
        // owns the element, deferring to the dep is the correct canonical-host semantics.
        if cli.wasm {
            for ident in types.used_as_elem() {
                let element_type = types.used_as_elem_element_type(ident);
                let structural =
                    RustIdent::new(CDDLIdent::new(element_type.name_as_wasm_array(types)));
                self.generate_array_type(types, element_type, &structural, false, cli);
            }
        }

        // W2 dep side (`--wrapper-requests`): now that the OWN-spec wasm wrapper walk is complete
        // (`wasm_collection_wrappers` / `own_wrapper_shapes` fully populated), read the consumer
        // sidecars, union the requested shapes, and emit each requested wrapper the dep does not
        // already produce into the `requested_collections` module. Wasm-only, and a no-op (byte
        // identical) with no `--wrapper-requests` flag.
        if cli.wasm {
            self.emit_requested_collections(types, cli);
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

        // rust. The codegen provenance header is stamped once per emitted FILE (see
        // `generated_files` / `export`), not per scope — a scope-level raw would hoist above the
        // module-linking raws that `merge_scopes_to_strings` prepends into a merged root file.
        self.rust_lib()
            .raw("#![allow(clippy::too_many_arguments)]\n");

        // declare modules (root lib specific)
        if cli.export_static_files() {
            self.rust_lib().raw("pub mod error;");
            if cli.preserve_encodings {
                self.rust_lib().raw("pub mod ordered_hash_map;");
            }
            // only crates that actually use `[+ T]` pull in the NonEmptyVec runtime — keeps every
            // non-`+` crate's output byte-identical. `--wrapper-requests`: a dep hosting a requested
            // NonEmpty wrapper needs the runtime module even when its own spec has no `[+ …]`.
            if types.uses_non_empty_vec() || self.requested_non_empty_vec {
                self.rust_lib().raw("pub mod non_empty;");
            }
            // only crates that actually use `{+ k => v}` pull in the NonEmptyMap runtime
            if types.uses_non_empty_map() || self.requested_non_empty_map {
                self.rust_lib().raw("pub mod non_empty_map;");
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

        // The borrowed-key-types sidecar module (materialized as `generated/borrowed_key_types.rs` in
        // `generated_files`). RUST crate, not wasm — key derives are a rust-crate concern (the
        // consumer's rust crate is what fails to build without them). PRIVATE (`mod`): its
        // `BORROWED_KEY_TYPES` const is `pub(crate)`-machine-read output and the compiled self-check
        // fails THIS crate's build if a dep drops a derive; nothing is re-exported. Declared whenever
        // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is borrowed.
        if !self.workspace_deps.is_empty() {
            self.rust_lib().raw("mod borrowed_key_types;");
        }

        // The key-demand assertions module (materialized as `generated/key_demand_assertions.rs` in
        // `generated_files`), declared only when some `@used_as_key` root — flavored or bare — exists,
        // so a key-free crate emits neither the decl nor the file. PRIVATE (`mod`): its `_demand_*`
        // fns are compile-time-only self-checks.
        if !assertion_roots(types).is_empty() {
            self.rust_lib().raw("mod key_demand_assertions;");
        }

        // declare common modules in each module (struct files). serialization / cbor_encodings are
        // each declared only where the corresponding .rs is actually emitted (mirror the conditions
        // in generated_files / merge_scopes_to_strings): declaring a `pub mod` with no backing file
        // is E0583, uncompilable.
        //   - serialization.rs: the root always materializes one (the static prelude is prepended
        //     unconditionally — merge_scopes_to_strings always writes the root file), and a non-root
        //     scope only when it has generated serialize impls (`serialize_scopes`). An alias/enum-only
        //     non-root module (scalar/collection/table alias, or a c-style enum whose serialization is
        //     emitted elsewhere) produces no serialization.rs, so an unconditional decl was E0583.
        //   - cbor_encodings.rs: a scope with no encoding structs (e.g. a root of only c-style enums)
        //     emits no such file, so the decl is conditioned on `cbor_encodings_scopes` the same way.
        for (scope, content) in self.rust_scopes.iter_mut() {
            if *scope == *ROOT_SCOPE || self.serialize_scopes.contains_key(scope) {
                content.raw("pub mod serialization;");
            }
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
            // `BTreeMap` is pushed into every cbor_encodings file unconditionally; the prune pass
            // (`import_prune::prune_generated_files`, run in `generated_files`) drops it from files
            // whose module family doesn't name it. Dumb-push + central prune — see the struct loop
            // below.
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
            // Wasm pass only: `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` -> wasm crate name. When a
            // non-exported (cross-crate extern-dep) import scope's leading component is mapped, the
            // wasm import is qualified through the dep's wasm crate instead of its rust crate (the
            // rust type has no wasm-bindgen bindings under the split `<dep>`/`<dep>-wasm` layout).
            // `None` for the rust pass and for unmapped deps => import path stays verbatim.
            extern_wasm_crate_map: Option<&BTreeMap<String, String>>,
        ) {
            // might not exist if we don't use stuff from other scopes
            if let Some(scope_imports) = imports.get(scope) {
                for (import_scope, idents) in scope_imports.iter() {
                    let import_scope = if *import_scope == *ROOT_SCOPE {
                        Cow::from(crate_prefix.to_owned())
                    } else if *scope == *ROOT_SCOPE || !import_scope.export() {
                        // Cross-crate extern-dep scopes are non-exported: their leading component is
                        // the dependency crate name. In the wasm pass, remap that component to the
                        // dep's wasm crate when a mapping is present.
                        let components = import_scope.components();
                        match (extern_wasm_crate_map, components.split_first()) {
                            (Some(map), Some((first, rest)))
                                if !import_scope.export() && map.contains_key(first) =>
                            {
                                let wasm_crate = &map[first];
                                if rest.is_empty() {
                                    Cow::from(wasm_crate.clone())
                                } else {
                                    Cow::from(format!("{}::{}", wasm_crate, rest.join("::")))
                                }
                            }
                            _ => Cow::from(import_scope.to_string()),
                        }
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
        // The rust pass registers no collection-wrapper class imports (those are wasm-only), so
        // deferral never applies here — pass an empty map so rust output is untouched by the flag.
        let rust_imports = types.scope_references(false, &BTreeMap::new());
        for (scope, content) in self.rust_scopes.iter_mut() {
            add_imports_from_scope_refs(scope, content, &rust_imports, "crate::generated", None);
            // These collection-type imports are pushed unconditionally (or on spec-global gates)
            // even into files that never reference them: dumb-push here, and the usage-derived
            // prune pass (`import_prune::prune_generated_files`, run once over the whole file map in
            // `generated_files`) removes any that the file's module family doesn't actually name.
            // Deriving the import set from the emitted tokens is sound by construction and lives in
            // one place; predicting per-file need at each of these ~30 sites would have to mirror
            // every local emission decision and drift.
            content.push_import("std::collections", "BTreeMap", None);
            if cli.preserve_encodings {
                content.push_import(
                    format!("{}::ordered_hash_map", cli.common_import_rust()),
                    "OrderedHashMap",
                    None,
                );
            }
            if types.uses_non_empty_vec() {
                content.push_import(
                    format!("{}::non_empty", cli.common_import_rust()),
                    "NonEmptyVec",
                    None,
                );
            }
            if types.uses_non_empty_map() {
                content.push_import(
                    format!("{}::non_empty_map", cli.common_import_rust()),
                    "NonEmptyMap",
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
            let extern_wasm_crate_map = cli.extern_wasm_crate_map();
            // Validate mapping keys BEFORE emitting: a key that names no extern dependency is almost
            // certainly a typo, and a silent no-op would leave the generated wasm crate pointing at
            // the (non-wasm) rust crate and failing to compile with no hint why.
            if !extern_wasm_crate_map.is_empty() {
                let extern_dep_names = types.extern_dep_names();
                for dep in extern_wasm_crate_map.keys() {
                    if !extern_dep_names.contains(dep) {
                        panic!(
                            "--extern-wasm-crate names dependency {dep:?}, which is not an \
                             extern dependency in this spec. Known extern dependencies: {:?}",
                            extern_dep_names
                        );
                    }
                }
            }
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
            // The collection-wrapper index module (materialized as `generated/collections.rs` in
            // `generated_files`). Declared unconditionally for every wasm run — even one that mints
            // zero wrappers — from the always-regenerated generated root, never the seed-once
            // crate-root lib.rs.
            self.wasm_lib().raw("pub mod collections;");
            // The borrowed-collections sidecar module (materialized as `generated/borrowed_collections.rs`
            // in `generated_files`). PRIVATE (`mod`, never `pub mod`) — its `use` lines only
            // existence-check the borrowed wrapper names; borrowed wrappers are never re-exported (the
            // consumer's own `collections.rs` lists only wrappers it defines). Declared whenever
            // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is
            // borrowed.
            if !self.workspace_deps.is_empty() {
                self.wasm_lib().raw("mod borrowed_collections;");
            }
            // wasm imports
            // `deferred_wrappers` was fully populated during the wasm struct walk above (every
            // deferred wrapper's mint point recorded it), so referencing modules now get a plain
            // `use <dep_wasm>::collections::<Name>;` for each instead of a local class.
            let wasm_imports = types.scope_references(true, &self.deferred_wrappers);
            for (scope, content) in self.wasm_scopes.iter_mut() {
                // imports from other struct modules; the wasm generated tree nests one level under
                // `crate::generated` (same as the rust crate)
                add_imports_from_scope_refs(
                    scope,
                    content,
                    &wasm_imports,
                    "crate::generated",
                    Some(&extern_wasm_crate_map),
                );
                // common imports. The collection-type imports below (`BTreeMap`/`OrderedHashMap`
                // and the two NonEmpty types) are pushed on spec-global gates even into wasm files
                // that never reference them; the prune pass (`import_prune::prune_generated_files`,
                // in `generated_files`) removes the ones the file's module family doesn't name.
                // Dumb-push + central prune.
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
                if types.uses_non_empty_vec() {
                    content.push_import(
                        format!("{}::non_empty", cli.common_import_wasm()),
                        "NonEmptyVec",
                        None,
                    );
                }
                if types.uses_non_empty_map() {
                    content.push_import(
                        format!("{}::non_empty_map", cli.common_import_wasm()),
                        "NonEmptyMap",
                        None,
                    );
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
        //
        // Multifile output: each test module lands at the generated ROOT (the `raw` below) while
        // the minted values name submodule types bare (`St`, `Bholder`) — `use super::*;` only
        // reaches root-scope items, so the emitters glob-import each declared non-root module
        // (`use super::a::*;`). The lists are derived from the SAME per-crate scope maps the
        // module-declaration loops above consume (`rust_scopes`/`wasm_scopes`, minus root and
        // non-exported extern-dep scopes), so a glob can never name an undeclared module; BTreeMap
        // keys keep the order deterministic. Caveat: glob imports can collide (E0659) if two
        // submodules export the same type name — no matrix cell or corpus fixture does; if a
        // consumer ever hits it, the robust long-term shape is fully-qualified rendering (thread
        // `types.scope(ident)` into `render_rust`/`render_wasm`) instead of globs.
        let submodule_glob_paths = |scopes: &BTreeMap<ModuleScope, codegen::Scope>| -> Vec<String> {
            scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE && scope.export())
                .map(|scope| scope.components().join("::"))
                .collect()
        };
        if cli.emit_tests {
            let rust_submodules = submodule_glob_paths(&self.rust_scopes);
            if let Some(test_mod) =
                crate::emit_tests::emit_generated_tests(types, cli, &rust_submodules)
            {
                self.rust_lib().raw(&test_mod);
            }
        }
        // the wasm-crate counterpart: same MintValue derivation, rendered through the wrapper API +
        // the cddl_lib rust twin (cross-crate byte differential). `#[cfg(test)]` so it's inert for
        // build/check/wasm-pack — only a `cargo test` of the wasm crate compiles and runs it.
        if cli.wasm && cli.emit_tests {
            let wasm_submodules = submodule_glob_paths(&self.wasm_scopes);
            if let Some(test_mod) =
                crate::emit_tests_wasm::emit_generated_wasm_tests(types, cli, &wasm_submodules)
            {
                self.wasm_lib().raw(&test_mod);
            }
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
            // Restamp: `generated_files` already stamped its generated-only serialization.rs, but
            // this rebuilt version (static prelude + merged ROOT serialize scope) replaces it, so it
            // needs the header applied here too (this is a header-stamped path).
            files.insert(
                "rust/src/generated/serialization.rs".to_owned(),
                stamp_codegen_header(&rustfmt_generated_string(&merged.to_string())?),
            );
        }

        // Manifests merge into whatever is already on disk (the declarative changeset) rather than
        // clobbering, so user edits to keys the tool doesn't own survive regeneration. This is one of
        // the bounded exceptions where output depends on prior directory contents (the others: the
        // seed-once crate roots below, and the comment-preservation overlay in the write loop), and
        // only as the changeset contract allows: keys no op mentions pass through, `SeedOnce` checks
        // existence. An unparseable
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

        // Every generated-tree `.rs` written this run, so the stale-file scan below can tell an
        // orphan (a file a prior run generated but this one no longer does — e.g. a removed/renamed
        // scope) from live output.
        let mut written_generated_rs: BTreeSet<std::path::PathBuf> = BTreeSet::new();
        for (rel_path, content) in &files {
            let path = rust_dir.join(rel_path);
            if is_preservable_generated_path(rel_path) {
                written_generated_rs.insert(path.clone());
            }
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
            // Comment-preservation overlay: for a generated `.rs` that already exists on disk, carry
            // the user's own-line comments from the prior output onto the fresh content (unplaceable
            // ones become tagged `compile_error!` blocks — loud, never a silent drop). This is the
            // third bounded exception to the no-prior-output invariant: prior output contributes ONLY
            // comment bytes and `cddl-codegen:unpreserved-comment` compile_error blocks — never a
            // code token OUTSIDE those tagged blocks — and run-twice-equals-run-once still holds
            // (see `comment_preserve`).
            if is_preservable_generated_path(rel_path) {
                write_rs_with_preserve(&path, rel_path, content, cli.preserve_comments)?;
            } else {
                std::fs::write(path, content)?;
            }
        }

        // static files copied/assembled verbatim (only when we own the common types). The runtime
        // composition (error.rs / ordered_hash_map.rs / non_empty.rs / non_empty_map.rs) is shared
        // with the `--export-static-dir` path via `composed_runtime_static_files` so the two can't
        // drift; the returned content is already rustfmt'd (load-bearing for the overlay — see that
        // helper). In-crate the NonEmpty runtimes are gated on spec usage: only for crates that use
        // `[+ T]` / `{+ k => v}`. `--wrapper-requests`: a dep hosting a requested NonEmpty wrapper
        // needs the runtime file even when its own spec has none.
        if cli.export_static_files() {
            let runtime_files = composed_runtime_static_files(
                cli,
                types.uses_non_empty_vec() || self.requested_non_empty_vec,
                types.uses_non_empty_map() || self.requested_non_empty_map,
            )?;
            for (filename, content) in &runtime_files {
                let rel_path = format!("rust/src/generated/{filename}");
                let path = rust_dir.join(&rel_path);
                write_rs_with_preserve(&path, &rel_path, content, cli.preserve_comments)?;
                written_generated_rs.insert(path);
            }
        }

        // `--export-static-dir`: ADDITIONALLY write the composed rust runtime into the named dir,
        // independent of the in-crate export above (the upgrade path for --common-import-override
        // users). The exported set is a PURE FUNCTION OF THE FLAG SET, never of the spec: the two
        // NonEmpty runtimes are ALWAYS included (unlike the spec-usage gating in-crate) and
        // serialization.rs always includes raw_bytes_encoding — a shared runtime crate serves many
        // specs, so which spec was run must not change the output. serialization.rs here is the
        // composed static PRELUDE ONLY (no generated per-type impls appended). No mod.rs/lib.rs is
        // written — the target crate owns its module declarations; static files reference siblings
        // via `super::…`, so a flat module dir works. This dir is OUTSIDE the output crate, so its
        // paths are deliberately not added to `written_generated_rs` / the stale-file scan.
        if let Some(export_dir) = &cli.export_static_dir {
            std::fs::create_dir_all(export_dir)?;
            let runtime_files = composed_runtime_static_files(cli, true, true)?;
            for (filename, content) in &runtime_files {
                let path = export_dir.join(filename);
                write_rs_with_preserve(&path, filename, content, cli.preserve_comments)?;
            }
            // serialization.rs — the static prelude only. `export_raw_bytes_encoding_trait` is
            // forced true (always include raw_bytes_encoding, per the pure-function-of-flags rule).
            // rustfmt'd before the preserve write, exactly like the composed runtime files.
            //
            // The prelude carries no `use` statements of its own: in-crate it is prepended to the
            // generated root serialization.rs, whose emitted import block serves the whole module
            // (`use` is scope-wide regardless of position). Standalone, the exported file must
            // bring its own imports or it does not compile. Every prelude flavor references all of
            // these (Deserialize/Serialize traits are in the base file).
            let prelude = format!(
                "use super::error::{{DeserializeError, DeserializeFailure}};\n\
                 use cbor_event::de::Deserializer;\n\
                 use cbor_event::se::Serializer;\n\
                 use std::io::{{BufRead, Seek, Write}};\n\n{}",
                Self::serialization_prelude(true, cli)?
            );
            let serialization_path = export_dir.join("serialization.rs");
            write_rs_with_preserve(
                &serialization_path,
                "serialization.rs",
                rustfmt_generated_string(&prelude)?.as_ref(),
                cli.preserve_comments,
            )?;
        }

        // Stale-file scan: a `.rs` under a tool-owned generated tree that this run did not produce
        // was generated by a PRIOR run (removed/renamed type or scope). Its `mod` declaration is
        // gone from the regenerated tree, so it (and any user comments in it) silently drops out of
        // the build — the one comment-loss path the per-file overlay cannot see. Diagnostic-only
        // stderr (same bounded read as the legacy-root warning): no output byte depends on it.
        for tree in [
            "rust/src/generated",
            "wasm/src/generated",
            "wasm/json-gen/src/generated",
        ] {
            let mut orphans = Vec::new();
            collect_rs_files(&rust_dir.join(tree), &mut orphans)?;
            orphans.retain(|p| !written_generated_rs.contains(p));
            orphans.sort();
            for orphan in orphans {
                eprintln!(
                    "warning: {} was generated by a previous run but is no longer generated; it is \
                     orphaned (nothing declares it as a module). Delete it — any comments you \
                     added there are NOT carried anywhere.",
                    orphan.display()
                );
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

        // Borrowed-key-types sidecar (`--workspace-dep`): the rust-crate analog of
        // `borrowed_collections.rs` for the map-key-derive concern. A consumer map keyed on a dep type
        // (`{* dep_key => …}`) marks `dep_key` used-as-key in finalize, but the derive lives in the
        // DEP's crate; when the value is consumer-owned (`{* dep_key => my_local}`) the wrapper is not
        // all-one-dep and never enters `borrowed_collections.rs`, yet the dep must still derive the key
        // traits on `dep_key` or the consumer's rust crate fails to build. This file records every such
        // borrowed key type so the dep can re-read it via `--key-requests`. Emitted whenever the flag
        // is present — INCLUDING when nothing is borrowed (stable presence/diffs) — and never
        // otherwise, mirroring `borrowed_collections.rs`. Fixed format: the four-line banner, a
        // `_assert_key_traits` bound-carrier + a `_borrowed_key_types_self_check` fn (the compiled half
        // — a dep dropping a derive fails THIS crate's build naming the type), and the
        // `#[allow(dead_code)] pub(crate) const BORROWED_KEY_TYPES` machine table (rows sorted by
        // (dep, ident); the first column is the dep's RUST crate name — the extern-deps dir name).
        if !self.workspace_deps.is_empty() {
            let mut rows: Vec<(String, String, DemandSet)> = Vec::new();
            for ident in types.used_as_key_idents() {
                let scope = types.scope(ident);
                if scope.export() {
                    continue;
                }
                let Some(dep) = scope.components().first() else {
                    continue;
                };
                if !self.workspace_deps.contains(dep) {
                    continue;
                }
                let demand = types.key_demand(ident).unwrap_or_default();
                rows.push((dep.clone(), convert_to_snake_case(ident.as_ref()), demand));
            }
            rows.sort();
            rows.dedup();
            // A borrowed key whose demand carries a `hash`/`ord` FLAVOR (a consumer keyed the dep type
            // through a `@used_as_key hash`/`ord` root) needs the flavored 3-column format + per-flavor
            // self-check bound. When every borrowed key is `bare` (the universal pre-flavor case), the
            // legacy 2-column form is emitted BYTE-IDENTICALLY — no banner/type/self-check churn.
            let any_flavored = rows.iter().any(|(_, _, d)| d.hash || d.ord);
            let sidecar = if any_flavored {
                let mut s = String::from(
                    "// This file records every map-key type this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
                     // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
                     // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
                     // Rows are (dep rust-crate name, cddl ident, demand flavor) of each borrowed map-key type.\n",
                );
                // One bound-carrier per distinct demand (the flavor decides the bound), then a
                // per-row self-check call routed to its flavor's carrier.
                let mut demands: Vec<DemandSet> = rows.iter().map(|(_, _, d)| *d).collect();
                demands.sort();
                demands.dedup();
                let assert_fn = |d: DemandSet| {
                    format!(
                        "_assert_key_traits_{}",
                        key_flavor_token(d).replace(' ', "_")
                    )
                };
                for d in &demands {
                    s.push_str(&format!(
                        "#[allow(dead_code)]\nfn {}<K: {}>() {{}}\n",
                        assert_fn(*d),
                        key_bound(*d, cli)
                    ));
                }
                s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
                for (dep, ident, d) in &rows {
                    let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
                    s.push_str(&format!("    {}::<{dep}::{ty}>();\n", assert_fn(*d)));
                }
                s.push_str("}\n");
                s.push_str(
                    "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[\n",
                );
                for (dep, ident, d) in &rows {
                    let flavor = key_flavor_token(*d);
                    s.push_str(&format!("    ({dep:?}, {ident:?}, {flavor:?}),\n"));
                }
                s.push_str("];\n");
                s
            } else {
                let bound = if cli.preserve_encodings {
                    "Eq + Ord + PartialOrd + core::hash::Hash"
                } else {
                    "Eq + Ord + PartialOrd"
                };
                let mut s = String::from(
                    "// This file records every map-key type this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--key-requests) so they derive the key\n\
                     // traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the\n\
                     // compiled self-check below fails THIS crate's build if a dep drops such a derive.\n\
                     // Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.\n",
                );
                s.push_str(&format!(
                    "#[allow(dead_code)]\nfn _assert_key_traits<K: {bound}>() {{}}\n"
                ));
                if !rows.is_empty() {
                    s.push_str("#[allow(dead_code)]\nfn _borrowed_key_types_self_check() {\n");
                    for (dep, ident, _) in &rows {
                        let ty = RustIdent::new(CDDLIdent::new(ident.clone()));
                        s.push_str(&format!("    _assert_key_traits::<{dep}::{ty}>();\n"));
                    }
                    s.push_str("}\n");
                }
                s.push_str(
                    "#[allow(dead_code)]\npub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[\n",
                );
                for (dep, ident, _) in &rows {
                    s.push_str(&format!("    ({dep:?}, {ident:?}),\n"));
                }
                s.push_str("];\n");
                s
            };
            out.insert(
                "rust/src/generated/borrowed_key_types.rs".to_owned(),
                rustfmt_generated_string(&sidecar)?.into_owned(),
            );
        }

        // Key-demand assertions: for each `@used_as_key` root — flavored or bare — emit a named
        // `_demand_<rule>` fn that instantiates a bound-carrier over the tagged type. The Rust
        // compiler — the one component never wrong about trait supply — then converts a distant
        // downstream trait error (e.g. a tx-out struct's extern field lacking `Ord`) into a NEAR, named
        // error at THIS assertion, citing the tag; for demand that fails at a contained struct's own
        // derive, the file is the in-crate breadcrumb from the failing trait back to the causing tag.
        // A bare root asserts the mode-dependent internal bundle it demands (ord family; + hash under
        // --preserve-encodings), mirroring `key_trait_list`. Internal auto-detected keys emit nothing
        // (their containers' own bounds enforce them in-crate).
        let assertion_roots = assertion_roots(types);
        if !assertion_roots.is_empty() {
            // The families each root's demand resolves to in THIS mode (bare is mode-dependent).
            let hash_family = |d: &DemandSet| d.hash || (d.bare && cli.preserve_encodings);
            let ord_family = |d: &DemandSet| d.ord || d.bare;
            let mut file = String::from(
                "// Compile-time key-demand assertions for `@used_as_key` tags. Each\n\
                 // `_demand_<rule>` fn makes the Rust compiler prove the tagged type implements the\n\
                 // traits its tag demands, turning a distant downstream trait error into a near,\n\
                 // named one at the tagged type's definition site.\n",
            );
            if assertion_roots.iter().any(|(_, d)| hash_family(d)) {
                file.push_str(
                    "#[allow(dead_code)]\nfn _key_demand_hash<T: core::hash::Hash + Eq>() {}\n",
                );
            }
            if assertion_roots.iter().any(|(_, d)| ord_family(d)) {
                file.push_str("#[allow(dead_code)]\nfn _key_demand_ord<T: Ord>() {}\n");
            }
            for (ident, demand) in &assertion_roots {
                let scope = types.scope(ident);
                let path = if *scope == *ROOT_SCOPE {
                    format!("crate::generated::{ident}")
                } else {
                    format!(
                        "crate::generated::{}::{ident}",
                        scope.components().join("::")
                    )
                };
                let src = types
                    .source_rule_name(ident)
                    .map(str::to_owned)
                    .unwrap_or_else(|| ident.to_string());
                let mut words = Vec::new();
                if demand.hash {
                    words.push(" hash");
                }
                if demand.ord {
                    words.push(" ord");
                }
                file.push_str(&format!(
                    "#[allow(dead_code)]\nfn _demand_{}() {{\n    // required by `@used_as_key{}` on {}\n",
                    convert_to_snake_case(ident.as_ref()),
                    words.concat(),
                    src
                ));
                if hash_family(demand) {
                    file.push_str(&format!("    _key_demand_hash::<{path}>();\n"));
                }
                if ord_family(demand) {
                    file.push_str(&format!("    _key_demand_ord::<{path}>();\n"));
                }
                file.push_str("}\n");
            }
            out.insert(
                "rust/src/generated/key_demand_assertions.rs".to_owned(),
                rustfmt_generated_string(&file)?.into_owned(),
            );
        }

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
            // W2 (`--wrapper-requests`): the synthetic `requested_collections` scope has no
            // submodules, so materialize it as the flat `generated/requested_collections.rs` the
            // cross-crate contract names (its `pub mod requested_collections;` decl and the index's
            // `crate::generated::requested_collections::…` re-exports resolve to either layout). Every
            // other exported scope keeps its `<name>/mod.rs` form (it may nest submodules).
            if let Some(content) = out.remove("wasm/src/generated/requested_collections/mod.rs") {
                out.insert(
                    "wasm/src/generated/requested_collections.rs".to_owned(),
                    content,
                );
            }
            out.insert(
                "wasm/src/lib.rs".to_owned(),
                rustfmt_generated_string(SEEDED_CRATE_ROOT)?.into_owned(),
            );

            // Collection-wrapper index: one `pub use crate::…::<Wrapper>;` per collection wrapper
            // CLASS this crate minted this run (recorded at each emitter's actual-mint point in
            // `wasm_collection_wrappers`). Because these are `pub use` lines compiled as part of
            // THIS crate, the index cannot drift: a line naming a removed wrapper fails this crate's
            // own build. A downstream crate points `--extern-wrapper-index <dep>=<this file>` at it
            // to skip re-minting the same wrappers (a wasm duplicate-symbol link error otherwise).
            // Emitted even when zero wrappers were minted (header comment only). The paths mirror
            // exactly how `merge_scopes_to_strings` lays the wasm generated tree out: ROOT_SCOPE
            // wrappers live in `generated/mod.rs` (`crate::generated::<Name>`); an exported
            // sub-scope's wrappers live in `generated/<scope>/mod.rs`
            // (`crate::generated::<scope>::<Name>`).
            let mut collections = String::from(
                "// Collection-wrapper index for this crate: one `pub use` re-export per collection\n\
                 // wrapper class defined here (list/map wrappers minted from `[* T]` / `{* K => V}`\n\
                 // shapes, including their NonEmpty variants). Compiled as part of this crate, so a\n\
                 // line naming a removed wrapper fails this crate's own build — the index cannot\n\
                 // drift. Downstream crates point `--extern-wrapper-index <dep>=<this file>` here to\n\
                 // avoid re-minting these wrappers (a wasm duplicate-symbol link error otherwise).\n",
            );
            for (ident, scope) in &self.wasm_collection_wrappers {
                let path = if *scope == *ROOT_SCOPE {
                    format!("crate::generated::{ident}")
                } else if scope.export() {
                    format!(
                        "crate::generated::{}::{ident}",
                        scope.components().join("::")
                    )
                } else {
                    // Non-exported (extern-dep) scopes are never written to a file by
                    // `merge_scopes_to_strings`, so a wrapper there is not part of THIS crate's
                    // output and must not appear in its index. Defensive — post-W1 no wrapper the
                    // crate mints lands in a non-exported scope.
                    continue;
                };
                collections.push_str(&format!("pub use {path};\n"));
            }
            out.insert(
                "wasm/src/generated/collections.rs".to_owned(),
                rustfmt_generated_string(&collections)?.into_owned(),
            );

            // Borrowed-collections sidecar (`--workspace-dep`): the mirror image of `collections.rs`
            // ("what I provide" ↔ "what I borrow, from whom"). Emitted whenever the flag is present —
            // INCLUDING when nothing is borrowed (stable presence, stable diffs) — and never
            // otherwise. Fixed format, ALL payload in code (no load-bearing comments the preservation
            // overlay could trap on): a private `#[allow(unused_imports)] mod borrowed` of plain `use`
            // lines (the compile-checked half — a wrapper a dep stops providing fails THIS crate's
            // build naming the type) and a `#[allow(dead_code)] pub(crate) const BORROWED_SHAPES`
            // table (the machine half the dep re-parses). Entries sorted by (dep, name); the `use`
            // paths go through the `--extern-wasm-crate` remap; the const's first column is the dep's
            // RUST crate name (the extern-deps directory name / `--extern-wasm-crate` left side), not
            // the wasm crate name.
            if !self.workspace_deps.is_empty() {
                let extern_wasm_crate_map = cli.extern_wasm_crate_map();
                let mut entries: Vec<(&str, &str, &str)> = self
                    .borrowed_wrappers
                    .iter()
                    .map(|(name, (dep, shape))| (dep.as_str(), name.as_ref(), shape.as_str()))
                    .collect();
                entries.sort_unstable();
                // The column legend lives in the banner (anchored to the file, which always exists),
                // NEVER inside the const body: an in-const comment is anchored to a row by the
                // preservation overlay, so deleting that row on an in-place regen (a consumer
                // dropping its last borrow of a shape) trapped the legend in a `compile_error!`
                // block — which the dep-side strict parser then (correctly) refused to consume.
                let mut sidecar = String::from(
                    "// This file records every collection wrapper this crate borrows from workspace deps.\n\
                     // It is machine-read by those deps' generation runs (--wrapper-requests) and compiled\n\
                     // here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.\n\
                     // Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).\n\
                     #[allow(unused_imports)]\n\
                     mod borrowed {\n",
                );
                for (dep, name, _) in &entries {
                    let dep_wasm = extern_wasm_crate_map
                        .get(*dep)
                        .map(String::as_str)
                        .unwrap_or(dep);
                    sidecar.push_str(&format!("    use {dep_wasm}::collections::{name};\n"));
                }
                sidecar.push_str(
                    "}\n\
                     #[allow(dead_code)]\n\
                     pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[\n",
                );
                for (dep, name, shape) in &entries {
                    sidecar.push_str(&format!("    ({dep:?}, {name:?}, {shape:?}),\n"));
                }
                sidecar.push_str("];\n");
                out.insert(
                    "wasm/src/generated/borrowed_collections.rs".to_owned(),
                    rustfmt_generated_string(&sidecar)?.into_owned(),
                );
            }

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

        // Stamp the codegen header once per emitted file, for the tool-owned generated trees only.
        // File-level (not scope-level) stamping guarantees the banner leads even in merged root
        // files, where the module-linking declarations from the lib scope would otherwise precede a
        // scope-level header raw. `export` restamps the one file it rebuilds after us (the root
        // serialization.rs, which it re-merges with the static prelude).
        for (path, content) in out.iter_mut() {
            if is_header_stamped_path(path) {
                *content = stamp_codegen_header(content);
            }
        }

        // Duplicate-ident backstop: no top-level type-namespace ident (struct/enum/type) may be
        // defined twice within a single generated file. Silent redefinitions arise when a user rule
        // name collides with a generator-synthesized structural ident (list/map wrapper families) —
        // exit-0 today, E0428 in the output crate. Observing the ACTUAL emitted source (not an IR
        // prediction) makes this the backstop for every mint path, present and future. Scoped to the
        // tool-owned `src/generated/**` trees (all three crates); static/template-sourced files are
        // excluded. `out` is a BTreeMap (sorted keys) and per-file idents are collected into a
        // BTreeMap, so the first offending file and its listed idents are deterministic. On a hit
        // this returns an `Err` at the seam (surfaced as `error (graceful)` by the catalogs), never
        // a panic.
        for (path, content) in out.iter() {
            if !path.contains("src/generated/") {
                continue;
            }
            let mut seen: BTreeMap<&str, usize> = BTreeMap::new();
            for line in content.lines() {
                if let Some(ident) = top_level_type_ident(line) {
                    *seen.entry(ident).or_insert(0) += 1;
                }
            }
            let dups: Vec<&str> = seen
                .iter()
                .filter(|&(_, &count)| count > 1)
                .map(|(ident, _)| *ident)
                .collect();
            if !dups.is_empty() {
                let names = dups
                    .iter()
                    .map(|d| format!("'{d}'"))
                    .collect::<Vec<_>>()
                    .join(", ");
                return Err(std::io::Error::other(format!(
                    "duplicate top-level ident{} {names} in {path}: a rule name collides with a \
                     generator-synthesized ident (list/map wrapper families) — rename the rule; if \
                     no user rule is involved this is a cddl-codegen bug",
                    if dups.len() == 1 { "" } else { "s" },
                )));
            }
        }

        // Usage-derived import prune: drop the blindly-pushed collection-type imports
        // (`BTreeMap`/`OrderedHashMap`/`NonEmptyVec`/`NonEmptyMap`) that a file's module family
        // references nowhere. Runs here, over the WHOLE file map, rather than per-file in
        // `rustfmt_generated_string`, because soundness needs each file's descendant modules in
        // view: a child's `use super::*;` chain can consume the parent's private imports, so a
        // file's import is genuinely unused only when neither the file nor any descendant module
        // names the ident (see `import_prune.rs`). The pass returns the changed files' pruned
        // (not-yet-rustfmt'd) content; rustfmt normalizes the splice here. This is still BEFORE the
        // comment-preservation overlay (which runs at `export` write time), so fresh content stays
        // a rustfmt-stable fixed point run-over-run.
        for (path, pruned) in crate::import_prune::prune_generated_files(&out) {
            let formatted = rustfmt_generated_string(&pruned)?.into_owned();
            out.insert(path, formatted);
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
        // W2 (`--wrapper-requests`): a requested wrapper is not in this dep's IR, so `types.scope`
        // would fall back to the crate root. While the override is set (only around requested-wrapper
        // emission), route it into the dedicated `requested_collections` module instead.
        let scope_name = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).to_owned(),
        };
        self.wasm_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" WASM scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn wasm_lib(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_lib_scope
    }

    /// Record that a collection-wrapper class `ident` was just emitted, for the
    /// `wasm/src/generated/collections.rs` re-export index. Called from each of the four wrapper
    /// emitters right after their shared `already_generated` guard admits the mint, so the index
    /// captures every wrapper class exactly once and never a suppressed one. The recorded
    /// `ModuleScope` is `types.scope(ident)` — the SAME scope `wasm(types, ident)` places the class
    /// in — so the index path derives from the class's real emission location.
    fn record_collection_wrapper(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
        shape: &str,
    ) {
        // The recorded scope is where the class is actually emitted: the requested-collections
        // override when active (so the index re-exports it from that module), else `types.scope`.
        let scope = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).clone(),
        };
        self.wasm_collection_wrappers.insert(ident.clone(), scope);
        // W2 (`--wrapper-requests`): index this crate's OWN collection-wrapper shapes (main walk only,
        // never the requested wrappers being minted under the override) so a dep can tell whether it
        // already produces a requested shape, and under what name.
        if self.requested_scope_override.is_none() {
            self.own_wrapper_shapes
                .insert(shape.to_owned(), ident.clone());
        }
    }

    /// W2 (`--wrapper-requests`): the attribution doc for `ident` as a paragraph PREFIX (trailing
    /// blank line) to prepend to an emitter-set struct doc, or `""` when the wrapper is not requested.
    /// Used by the NonEmpty emitters, whose `.doc()` call would otherwise clobber the attribution
    /// `create_base_wasm_struct` injects.
    fn requested_attribution_prefix(&self, ident: &RustIdent) -> String {
        self.requested_attribution
            .get(ident)
            .map(|d| format!("{d}\n\n"))
            .unwrap_or_default()
    }

    /// Record that structural wrapper `ident` was deferred to workspace dependency `dep` this run
    /// (`--workspace-dep`), for the `wasm/src/generated/borrowed_collections.rs` sidecar. Idempotent:
    /// the same wrapper is probed from several sites (the loose emitter, a keys-list, a NonEmpty
    /// try_from source), each recording the same `(dep, shape)`. Two DISTINCT shapes deriving the
    /// SAME structural name — the `MapAToBToC` reverse-ambiguity (`{* a => b_to_c}` vs
    /// `{* a_to_b => c}`) — is a hard error naming both shapes: today that pair already fails rustc
    /// (two same-named local mints), so this upgrades a compile failure into an actionable diagnostic.
    pub(crate) fn record_borrowed_wrapper(&mut self, ident: &RustIdent, dep: &str, shape: &str) {
        if let Some((_, existing_shape)) = self.borrowed_wrappers.get(ident)
            && existing_shape != shape
        {
            panic!(
                "two distinct shapes in this crate's spec derive the same borrowed collection wrapper \
                 name {ident}: {existing_shape:?} and {shape:?}. These would define one JS class for \
                 two concepts — rename or @name one of them."
            );
        }
        self.borrowed_wrappers
            .insert(ident.clone(), (dep.to_owned(), shape.to_owned()));
    }

    /// W2 dep side (`--wrapper-requests`): read each consumer's committed `borrowed_collections.rs`,
    /// take the entries addressed to THIS dep (dep column == the normalized `--lib-name`), union the
    /// requested collection-wrapper shapes across consumers, and emit every requested wrapper the dep
    /// does not already produce into `wasm/src/generated/requested_collections.rs` (indexed via
    /// `record_collection_wrapper`, each carrying a sorted-requester attribution doc). Called once,
    /// after the own-spec wasm walk, under `--wasm`. A no-op — output byte-identical to today — when
    /// no `--wrapper-requests` flag is set (the module is not even created).
    ///
    /// Determinism: everything is keyed/sorted (`BTreeMap`/`BTreeSet`), so the union and the emission
    /// order depend on neither the flag order nor the consumers' regen order.
    fn emit_requested_collections(&mut self, types: &IntermediateTypes, cli: &Cli) {
        let request_files = cli.wrapper_requests();
        if request_files.is_empty() {
            // No flag => no file, byte-identical to today (acceptance criterion 10 analog).
            return;
        }
        let my_lib = cli.lib_name_code();

        // One entry per requested shape after unioning across consumers.
        struct Unioned {
            rt: RustType,
            structural: String,
            requesters: BTreeSet<String>,
        }
        // Keyed by the canonically RE-RENDERED shape (so `stake-credential` ≡ `stake_credential`
        // unify): two consumers requesting the same shape with hyphen/underscore skew collapse here.
        let mut union: BTreeMap<String, Unioned> = BTreeMap::new();

        for (consumer, path) in &request_files {
            let contents = std::fs::read_to_string(path).unwrap_or_else(|e| {
                panic!("--wrapper-requests {consumer}={path}: cannot read the sidecar: {e}")
            });
            let entries = crate::wrapper_requests::parse_sidecar(&contents, path);
            for entry in entries {
                // Entries addressed to OTHER deps (dep column != this crate's normalized lib name)
                // are silently skipped — a shared sidecar can name several deps.
                if entry.dep.replace('-', "_") != my_lib {
                    continue;
                }
                let rt = parse_requested_shape(types, &entry.shape, consumer, path, &entry.name);
                // A requested shape that is DIRECTLY WASM-EXPOSABLE has no wrapper class at all —
                // it lowers to a bare `Vec<…>` at the wasm boundary — so no borrowed wrapper exists
                // or is needed. Such a request is the symptom of an unfaithful consumer stub: the
                // consumer declared its element(s) opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this
                // dep resolves them transparently to a directly-exposable type. Diagnose it here,
                // before deriving the structural name — otherwise a loose list over a transparent
                // primitive alias (`[* coin]` with `coin = uint`) misdiagnoses as a name↔shape
                // disagreement, and a member-form listing (`Vec<u64>` for `[* uint]`) slips past the
                // cross-check and dies later in rustfmt labeled a generator bug.
                if let Some(member) = requested_exposable_member(types, &rt) {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        "its element is a wasm-primitive".to_owned()
                    } else {
                        format!("its element(s) resolve here as {}", leaves.join(", "))
                    };
                    panic!(
                        "--wrapper-requests {consumer} ({path}): the requested wrapper {:?} with \
                         shape {:?} is directly wasm-exposable — it lowers to `{member}` with no \
                         wrapper class, so no borrowed wrapper exists or is needed ({leaf_note}). \
                         This request is the symptom of an unfaithful consumer stub: the consumer \
                         declared the element opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this dep \
                         resolves it transparently. Remedy: fix the consumer's \
                         `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` stub for this dep to declare the element \
                         truthfully (e.g. `coin = uint`) and regenerate the consumer, which will \
                         then stop borrowing this shape.",
                        entry.name, entry.shape
                    );
                }
                let canonical = render_wrapper_shape(&rt);
                let structural = requested_structural_name(types, &rt, consumer, path);
                // Cross-check the derived structural name against the listed name (criterion 8 #2).
                if structural != entry.name {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        String::new()
                    } else {
                        format!(" Element resolution in this dep: {}.", leaves.join(", "))
                    };
                    panic!(
                        "--wrapper-requests {consumer} ({path}): the borrowed wrapper listed as \
                         {:?} with shape {:?} derives the structural name {:?}, not {:?} — the \
                         sidecar's name and shape columns disagree (a name↔shape mismatch).{leaf_note}",
                        entry.name, entry.shape, structural, entry.name
                    );
                }
                let u = union.entry(canonical).or_insert_with(|| Unioned {
                    rt: rt.clone(),
                    structural: structural.clone(),
                    requesters: BTreeSet::new(),
                });
                u.requesters.insert(consumer.clone());
            }
        }

        // Criterion 8 #4: two DISTINCT requested shapes deriving the SAME structural name (from any
        // combination of consumers) — one JS class for two concepts. Name both shapes and their
        // requesters.
        let mut by_structural: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for shape in union.keys() {
            by_structural
                .entry(union[shape].structural.clone())
                .or_default()
                .push(shape.clone());
        }
        for (structural, shapes) in &by_structural {
            if shapes.len() > 1 {
                let requesters: BTreeSet<&String> = shapes
                    .iter()
                    .flat_map(|s| union[s].requesters.iter())
                    .collect();
                panic!(
                    "--wrapper-requests: two distinct requested shapes derive the same structural \
                     wrapper name {structural:?}: {shapes:?} (requested by {requesters:?}). These \
                     would define one JS class for two concepts — rename or @name one of the shapes \
                     in the requesting consumers."
                );
            }
        }

        // Decide, per unioned shape, whether the dep already produces it (skip), produces it under a
        // different rule name (criterion 8 #3, hard error), or must emit it.
        let mut to_emit: Vec<(String, RustType, String, Vec<String>)> = Vec::new();
        for (canonical, u) in &union {
            match self.own_wrapper_shapes.get(canonical) {
                // Own spec already produces this shape under the STRUCTURAL name => request satisfied
                // by the existing indexed wrapper; emit nothing.
                Some(existing) if existing.as_ref() == u.structural => {}
                // Own spec produces this shape under a DIFFERENT (rule-declared) name => hard error.
                Some(existing) => {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} (requested by {:?}) is \
                         already produced by this dep's own spec under the non-structural rule name \
                         {existing}, not the structural name {:?} the consumers import. Emitting \
                         both would create two JS classes for one concept. Remedy: rename the rule \
                         {existing} to {}, give it `@name {}`, or drop it.",
                        u.requesters, u.structural, u.structural, u.structural
                    );
                }
                None => {
                    let mut requesters: Vec<String> = u.requesters.iter().cloned().collect();
                    requesters.sort();
                    to_emit.push((
                        canonical.clone(),
                        u.rt.clone(),
                        u.structural.clone(),
                        requesters,
                    ));
                }
            }
        }

        // Criterion 8 #5: a requested NESTED shape whose inner collection wrapper is neither requested
        // nor own-spec-produced — an integrity check against a hand-edited / truncated sidecar (a real
        // consumer closes over its nested shapes automatically, so the inner should always be present).
        for (canonical, rt, _, _) in &to_emit {
            for inner in inner_collection_shapes(rt) {
                let requested = union.contains_key(&inner);
                let own = self.own_wrapper_shapes.contains_key(&inner);
                if !requested && !own {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} nests the collection \
                         wrapper {inner:?}, which is neither requested by any consumer nor produced \
                         by this dep's own spec. The inner collection of an all-one-dep shape is \
                         itself all-one-dep and must be requested too — this sidecar looks truncated \
                         or hand-edited."
                    );
                }
            }
        }

        // Emit. `to_emit` is in canonical-shape (BTreeMap) order, so loose `[* …]` precedes its
        // NonEmpty `[+ …]` twin (`*` < `+`): a separately-requested loose source is emitted (and gets
        // its attribution) BEFORE the NonEmpty emitter's recursive mint no-ops on it. A NonEmpty
        // support source that is NOT itself requested is minted by the emitter into this same module
        // (indexed, no attribution — a benign transitive superset). Byte-identical under any flag /
        // regen order because the input set is fully sorted.
        let requested_scope = ModuleScope::from(vec!["requested_collections".to_owned()]);
        for (_, _, structural, requesters) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
            self.requested_attribution.insert(
                ident,
                format!("Generated at the request of: {}.", requesters.join(", ")),
            );
        }
        self.requested_scope_override = Some(requested_scope.clone());
        for (_, rt, structural, _) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
            match &rt.conceptual_type {
                ConceptualRustType::Array(inner) => {
                    if rt.is_non_empty_array() {
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    } else {
                        self.generate_array_type(types, (**inner).clone(), &ident, false, cli);
                    }
                }
                ConceptualRustType::Map(k, v) => {
                    if rt.is_non_empty_map() {
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    } else {
                        codegen_table_type(
                            self,
                            types,
                            &ident,
                            (**k).clone(),
                            (**v).clone(),
                            false,
                            cli,
                        );
                    }
                }
                other => unreachable!("requested shape is not a collection: {other:?}"),
            }
        }
        self.requested_scope_override = None;

        // A requested NonEmpty wrapper pulls in the NonEmpty runtime the dep's OWN spec may not use;
        // record it so the runtime-provisioning gates (mod decl + static file copy) fire, and import
        // the type into this scope explicitly (the per-scope loop's import gate is keyed off the dep's
        // own IR, which doesn't see the requested wrappers).
        self.requested_non_empty_vec = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_array());
        self.requested_non_empty_map = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_map());
        let non_empty_import = self
            .requested_non_empty_vec
            .then(|| format!("{}::non_empty", cli.common_import_wasm()));
        let non_empty_map_import = self
            .requested_non_empty_map
            .then(|| format!("{}::non_empty_map", cli.common_import_wasm()));

        // Ensure the module exists even when nothing is emitted (all requests satisfied by own spec /
        // addressed elsewhere) — stable presence, stable diffs (plan decision 1). When non-empty, the
        // wrappers reference the dep's own element WASM wrappers (which live at the generated root or a
        // sibling module); `use super::*;` reaches them, mirroring the emit-tests glob. The per-scope
        // import loop later adds the common wasm imports (wasm_bindgen/JsError/OrderedHashMap/…).
        let scope_content = self.wasm_scopes.entry(requested_scope).or_default();
        if !to_emit.is_empty() {
            scope_content.raw("use super::*;");
        }
        // These NonEmpty imports are pushed whenever the requested wrappers use them; if the file's
        // module family ends up not naming one, the prune pass
        // (`import_prune::prune_generated_files`, in `generated_files`) drops it. Dumb-push +
        // central prune, same as the struct sites.
        if let Some(path) = non_empty_import {
            scope_content.push_import(path, "NonEmptyVec", None);
        }
        if let Some(path) = non_empty_map_import {
            scope_content.push_import(path, "NonEmptyMap", None);
        }
    }

    /// Decide whether a structural collection wrapper the consumer is about to mint should instead be
    /// DEFERRED to a dependency that already owns it (`--extern-wrapper-index`). `structural_name` is
    /// the wrapper's structurally-derived name (`name_as_wasm_array` / `name_for_wasm_map`) and
    /// `constituents` its element (list) or key+value (map) conceptual types.
    ///
    /// Returns `true` when the wrapper is deferred — the caller must emit NO local class and skip
    /// `record_collection_wrapper`, so the deferred wrapper leaves the crate's own `collections.rs`
    /// index (R3e). The ident is recorded in `deferred_wrappers` mapped to the dependency's
    /// `collections` module scope, so `scope_references` routes a plain
    /// `use <dep_wasm>::collections::<Name>;` into every referencing module (R3b) and the keys()
    /// accessors construct via `.into()` cross-crate (R3d). Returns `false` (mint locally) when: the
    /// flag is unused; the ident is not the structural name of these constituents (a rule-declared
    /// wrapper — never suppressed); the constituents are mixed / not all one dependency (R3c, silent);
    /// or an all-extern-of-one-dep candidate is absent from that dep's index (local + one stderr
    /// warning naming the wrapper).
    #[allow(clippy::too_many_arguments)]
    fn try_defer_wrapper(
        &mut self,
        types: &IntermediateTypes,
        wrapper_ident: &RustIdent,
        structural_name: &str,
        constituents: &[&ConceptualRustType],
        // The wrapper's CDDL shape fragment (canonical renderer output), used to build the paste-able
        // "add this rule" hint on the not-in-index warning AND recorded in the workspace sidecar.
        shape: &str,
        // `true` when this mint request comes from an explicit RULE declaration (`foo_list = [* foo]`
        // reached via the `RustStruct::{Array,Table}` variant arms) rather than a synthesized/inline
        // wrapper. Only meaningful when the rule's ident coincides with the structural name (the
        // common `name != structural` case is already screened below); in workspace mode a
        // rule-declared wrapper is the consumer's OWN class and must NEVER defer — instead it triggers
        // the shadowing warning (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) -> bool {
        // Fast out only when NEITHER deferral mechanism is active. (Flag-off byte-identity: with both
        // sets empty this is the same early `false` as before — the workspace branch below is dead
        // code, criterion 10.)
        if self.extern_wrapper_index.is_empty() && self.workspace_deps.is_empty() {
            return false;
        }
        // Only structural-named wrappers are defer candidates: a rule-declared wrapper
        // (`foo_list = [* extern_foo]`) whose ident DIFFERS from the structural name is the consumer's
        // OWN class and is never suppressed. (A rule whose ident COINCIDES with the structural name
        // passes this guard; workspace mode distinguishes it via `rule_declared` just below.)
        if wrapper_ident.as_ref() != structural_name {
            return false;
        }
        // Workspace mode (`--workspace-dep`): an all-one-workspace-dep wrapper DEFERS UNCONDITIONALLY,
        // before any index consult. The placement decision is factored as one function over the
        // transitive element-owner set (plan decision 4: today "exactly one owner ∈ workspace deps →
        // Borrow"; "latest of the element owners" can replace this body later without touching call
        // sites). Ownerless / mixed-dep wrappers fall through to the shipped index/local logic below
        // (criterion 2). A rule-declared wrapper that would otherwise borrow is the consumer's own
        // class: warn (criterion 9) and fall through, never suppress it.
        if !self.workspace_deps.is_empty()
            && let WrapperPlacement::Borrow(dep) = wrapper_placement(
                &transitive_owner_set(types, constituents),
                &self.workspace_deps,
            )
        {
            if rule_declared {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    eprintln!(
                        "warning: rule-declared type {structural_name} shadows the collection wrapper \
                         this crate would otherwise borrow from workspace dependency {dep:?}; the \
                         authored class will duplicate-symbol against the dep's requested class at \
                         link. Remedy: rename the rule, or give it a distinct @name."
                    );
                }
                // fall through to the shipped behavior (never a workspace defer)
            } else {
                // Deferred to the workspace dep: record the borrow (idempotent; a same-name/different
                // -shape collision hard-errors inside) and route the import exactly like the index
                // branch does, so `scope_references` emits `use <dep_wasm>::collections::<Name>;`.
                self.record_borrowed_wrapper(wrapper_ident, &dep, shape);
                let dep_scope = ModuleScope::from(vec![
                    crate::parsing::EXTERN_DEPS_DIR.to_owned(),
                    dep,
                    "collections".to_owned(),
                ]);
                self.deferred_wrappers
                    .insert(wrapper_ident.clone(), dep_scope);
                return true;
            }
        }
        // Beyond this point is the shipped `--extern-wrapper-index` path (unchanged). It requires the
        // index; with only `--workspace-dep` set (no index) there is nothing more to do.
        if self.extern_wrapper_index.is_empty() {
            return false;
        }
        // Each named constituent (element / key / value that resolves to a named rule) maps to the
        // dependency owning it (leading component of its non-exported scope), or `None` when it's a
        // consumer-owned (exported) type. Primitives contribute no constituent.
        let mut constituent_deps: Vec<Option<String>> = Vec::new();
        for c in constituents {
            for id in named_constituent_idents(c) {
                let scope = types.scope(&id);
                constituent_deps.push(if scope.export() {
                    None
                } else {
                    scope.components().first().cloned()
                });
            }
        }
        let dep = if constituent_deps.is_empty() {
            // Zero named constituents (e.g. `MapU64ToText`): a defer candidate only if some configured
            // index lists the name. Several listing it would each be a duplicate-symbol link error, so
            // defer to the lexicographically-first dep (BTreeMap iteration order) and warn.
            let matching: Vec<&String> = self
                .extern_wrapper_index
                .iter()
                .filter(|(_, names)| names.contains(structural_name))
                .map(|(dep, _)| dep)
                .collect();
            match matching.as_slice() {
                [] => return false, // owned by no dependency -> local, silent
                [only] => (*only).clone(),
                many => {
                    if self.deferred_warned.insert(wrapper_ident.clone()) {
                        eprintln!(
                            "warning: collection wrapper {structural_name} is listed in several \
                             --extern-wrapper-index files ({many:?}); deferring to the first ({})",
                            many[0]
                        );
                    }
                    many[0].clone()
                }
            }
        } else {
            // Has named constituents: a defer candidate only if they ALL resolve to extern types of
            // the SAME dependency (R3c: any consumer-owned or cross-dependency constituent -> local,
            // silent).
            let mut single: Option<String> = None;
            for d in &constituent_deps {
                match d {
                    None => return false,
                    Some(name) => match &single {
                        None => single = Some(name.clone()),
                        Some(s) if s == name => {}
                        Some(_) => return false,
                    },
                }
            }
            let dep = single.unwrap();
            // All-extern-of-one-dep candidate: defer iff that dep's index lists it; otherwise mint
            // locally and warn once (a dep-side inventory change that silently shifted ownership back
            // to the consumer is then loud in the regen log, not only in the diff).
            if !self
                .extern_wrapper_index
                .get(&dep)
                .is_some_and(|names| names.contains(structural_name))
            {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    // Append the exact rule line to paste into the owning dep's spec: declaring it
                    // there lands the wrapper in the dep's collections.rs index (by construction), so
                    // every consumer's index-deferral then picks it up — the shipped manual override
                    // for wrappers no request sidecar covers (hand-written consumer code, mixed-dep
                    // shapes). Rule name = snake_case of the structural name; shape from the canonical
                    // renderer; requester = this consumer's normalized --lib-name.
                    let rule_name = convert_to_snake_case(structural_name);
                    let requester = cli.lib_name_code();
                    eprintln!(
                        "warning: collection wrapper {structural_name} has only extern elements of \
                         dependency {dep:?} but is absent from its --extern-wrapper-index; minting \
                         it locally (a dep that later adds it would duplicate-symbol at link time)\n\
                         hint: add to {dep}'s spec: {rule_name} = {shape} ; requested by {requester}"
                    );
                }
                return false;
            }
            dep
        };
        // Deferred: import from the dep's `collections` module. The non-exported scope
        // `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/collections` is remapped by
        // `add_imports_from_scope_refs` to `<dep_wasm>::collections` when `--extern-wasm-crate` maps
        // the dep, or left as `<dep>::collections` (the dep's rust crate name — the same fallback
        // unmapped extern types get) otherwise.
        let dep_scope = ModuleScope::from(vec![
            crate::parsing::EXTERN_DEPS_DIR.to_owned(),
            dep,
            "collections".to_owned(),
        ]);
        self.deferred_wrappers
            .insert(wrapper_ident.clone(), dep_scope);
        true
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
                        false,
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
                                &format!("({i}i128 + 1).unsigned_abs() as u64"),
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
                            true,
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
                                config.expr_is_ref,
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
                                config.expr_is_ref,
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
                                neg.line(format!("{serializer_use}.write_negative_integer_sz({expr_deref} as i128, cbor_event::Sz::canonical(({expr_deref} + 1).unsigned_abs())){line_ender}"));
                            } else {
                                // unsigned_abs() on i8/i16/i32 yields the same-width unsigned type;
                                // widen to u64 for Sz::canonical (a bare `as u64` on the i64 case
                                // would be a no-op cast)
                                let sz_expr = if *primitive == Primitive::I64 {
                                    format!("({expr_deref} + 1).unsigned_abs()")
                                } else {
                                    format!("({expr_deref} + 1).unsigned_abs() as u64")
                                };
                                write_using_sz(
                                    &mut neg,
                                    "write_negative_integer",
                                    serializer_use,
                                    &expr,
                                    &sz_expr,
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
                                true,
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
                    // `.end()` takes the serializer as an ARGUMENT, so it needs the pass form
                    // (`&mut <name>` for a `.cbor`-payload local `Serializer::new_vec()`), not the
                    // method-receiver form `serializer_use`. For the top-level `serializer` the two
                    // are identical; they diverge only for the `is_local` inner-buffer overload.
                    end_len(body, &serializer_pass, &encoding_var, config.is_end, cli);
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
                    // Argument to `.end()`: use the pass form (`&mut <name>` for a `.cbor`-payload
                    // local serializer) — see the Array arm above for the rationale.
                    end_len(body, &serializer_pass, &encoding_var, config.is_end, cli);
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
                                // A fixed null/bool contributes no encoding var of its own, but a
                                // WRAPPING path may already have pushed exprs into final_exprs (a
                                // CBOR tag pushes its tag-encoding expr before recursing). Split:
                                // - final_exprs EMPTY: the block's value is the unit `()` — pass it
                                //   explicitly, else the final expr collapses to empty and, under
                                //   `expects_result`, emits `Ok()` (E0061) instead of `Ok(())`.
                                //   (Non-preserve appends `Ok(())` below; preserve produces it here.)
                                // - final_exprs NON-empty: pass None — the value is the encoding
                                //   expr(s) alone (e.g. `Some(tag_enc)` bound to a single
                                //   `let v_tag_encoding = ...`); inserting `()` would mis-shape it
                                //   into `((), Some(tag_enc))` (E0308, seen with
                                //   `[v: #6.1(null), x: uint]`).
                                let unit_if_no_encs =
                                    config.final_exprs.is_empty().then(|| "()".to_owned());
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, unit_if_no_encs),
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
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Uint(({}_value + 1).unsigned_abs() as u64), expected: Key::Uint({}) }}.into());", config.var_name, x_abs));
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
                        FixedValue::Bool(b) => {
                            // A bool special has no encoding variation (unlike int/text `_sz`
                            // widths), so — like the Null arm — there is no encoding var to
                            // thread; just verify. `.bool()?` is unambiguous here: statement
                            // position binds the Ok type (bool) and the `?` converts the CBOR
                            // error, the same shape the Uint arm's `.unsigned_integer()?` uses
                            // (the inference hazard the `Primitive::Bool` arm documents only bites
                            // in element/push position).
                            deser_code.content.line(&format!(
                                "let {}_value = {}.bool()?;",
                                config.var_name, deserializer_name
                            ));
                            let mut compare_block =
                                Block::new(format!("if {}_value != {}", config.var_name, b));
                            compare_block.line(format!("return Err(DeserializeFailure::FixedValueMismatch{{ found: Key::Bool({}_value), expected: Key::Bool({}) }}.into());", config.var_name, b));
                            deser_code.content.push_block(compare_block);
                            if cli.preserve_encodings {
                                // No encoding var for a bool special, but a wrapping tag may have
                                // pushed into final_exprs — same empty/non-empty split as the
                                // FixedValue::Null arm: unit `()` only when final_exprs is empty
                                // (else `Ok()` E0061); None when non-empty (else `((), tag_enc)`
                                // E0308).
                                let unit_if_no_encs =
                                    config.final_exprs.is_empty().then(|| "()".to_owned());
                                deser_code.content.line(&format!(
                                    "{}{}{}",
                                    before_after.before_str(false),
                                    final_expr(config.final_exprs, unit_if_no_encs),
                                    before_after.after_str(false)
                                ));
                            }
                        }
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
                            "{}.and_then(|{pat}| if {cond} {{ Err(DeserializeFailure::RangeCheck{{ found: x as i128, min: Some({wmin}), max: Some({wmax}) }}.into()) }} else {{ Ok({ok}) }})",
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
                                    // Convert the error to DeserializeError so the `.and_then`
                                    // closure's `Err(DeserializeFailure::…into())` sees a consistent
                                    // E — but ONLY when no earlier stage of this chain already did.
                                    // The site's `error_convert` and any `width_fn` both leave the
                                    // error type as DeserializeError, so re-converting is a redundant
                                    // identity `From<T> for T`. Same `converted`-flag rule as
                                    // `width_reject`.
                                    Some(bounds) => format!(
                                        "{}.and_then(|({}, enc)| {} else {{ Ok({}) }})",
                                        if error_convert.is_empty() && width_fn.is_empty() {
                                            convert_err_to_ours
                                        } else {
                                            ""
                                        },
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
                                let cast = match p {
                                    Primitive::U64 | Primitive::Str | Primitive::Bytes => {
                                        Cow::Borrowed("")
                                    }
                                    _ => Cow::Owned(format!(" as {p}")),
                                };
                                deser_code.content.line(&format!(
                                    "{}{}.{}(){}{}?{}{}",
                                    before_after.before_str(false),
                                    deserializer_name,
                                    func,
                                    bounds_fn,
                                    width_fn,
                                    cast,
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
                                // Fold the accumulated outer-wrapper encoding exprs (e.g. a Tagged
                                // wrapper's `Some(tag_enc)`, a CBORBytes wrapper's StringEncoding)
                                // into the value tuple — as every other primitive path does via
                                // `final_expr`. Both arms MUST emit the same tuple shape. With an
                                // empty `config.final_exprs` this is the byte-identical
                                // `(x as {p}, Some(enc))`; with wrapper exprs it grows to the
                                // 3-tuple the member-level destructure expects (else a preserve-only
                                // E0308).
                                let mut arm_final_exprs = config.final_exprs.clone();
                                arm_final_exprs.push("Some(enc)".to_owned());
                                let arm_tuple =
                                    final_expr(arm_final_exprs, Some(format!("x as {p}")));
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
                                .line(&arm_tuple)
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
                                .line(&arm_tuple)
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
                                    "(x + 1).unsigned_abs() as u64",
                                    None,
                                )
                            } else {
                                // https://github.com/primetype/cbor_event/issues/9
                                // cbor_event's negative_integer() doesn't support full nint range so we use the _sz function here instead as that one supports all nints
                                let bounds_fn = match &type_cfg.bounds {
                                    Some(bounds) => Cow::Owned(format!(
                                        ".and_then(|(x, _enc)| {} else {{ Ok((x + 1).unsigned_abs() as u64) }})",
                                        bounds_check_if_block(
                                            bounds,
                                            &bounds_check_expr(*p, "x"),
                                            false
                                        ),
                                    )),
                                    None => Cow::Borrowed(
                                        ".map(|(x, _enc)| (x + 1).unsigned_abs() as u64)",
                                    ),
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
                                        // pattern parens only for a real tuple (>1), mirroring the
                                        // expression side's final_expr and the non-value enum
                                        // dispatch's names_without_outer.len() > 1 check
                                        let ok_pattern = if variant_final_exprs.len() == 1 {
                                            variant_final_exprs[0].clone()
                                        } else {
                                            format!("({})", variant_final_exprs.join(", "))
                                        };
                                        return_if_deserialized
                            .line(format!("Ok({}) => return Ok({}),",
                            ok_pattern,
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
                    if type_cfg.bounds == Some((Some(1), None)) {
                        // `[+ T]`: route the collected Vec through the SAME `TryFrom` door the API
                        // uses, so the wire side and API side report the identical RangeCheck error
                        // ("0 not at least 1") and can never drift. The encoding vars stay keyed off
                        // the field (untouched below) — only the value var is rebound.
                        deser_code.content.line(&format!(
                            "let {arr_var_name} = NonEmptyVec::try_from({arr_var_name})?;"
                        ));
                    } else if let Some(bounds) = &type_cfg.bounds {
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
                            | ConceptualRustType::Primitive(Primitive::U32) => {
                                format!("Key::Uint({key_var_name}.into())")
                            }
                            ConceptualRustType::Primitive(Primitive::U64) => {
                                format!("Key::Uint({key_var_name})")
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
                                    // The inserted expr is the key VALUE, so gate the clone on the
                                    // key value's copy-ness (matching the adjacent dup-check block),
                                    // NOT its encoding var's — a composite (e.g. array) key value is
                                    // a non-Copy Vec even though its length-encoding var is Copy, so
                                    // moving it here then reusing it below is a preserve-only E0382.
                                    if key_type.is_copy(types) {
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
                                    // Same as the key-encoding insert: the map is keyed by the key
                                    // VALUE, so gate its clone on the value's copy-ness, not the
                                    // encoding var's.
                                    if key_type.is_copy(types) {
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
                        if type_cfg.bounds == Some((Some(1), None)) {
                            // `{+ k => v}`: route the collected map through the SAME `TryFrom` door the
                            // API uses, so the wire side and API side report the identical RangeCheck
                            // error ("0 not at least 1") and can never drift. The encoding vars stay
                            // keyed off the field (untouched below) — only the value var is rebound.
                            deser_code.content.line(&format!(
                                "let {table_var} = NonEmptyMap::try_from({table_var})?;"
                            ));
                        } else if let Some(bounds) = &type_cfg.bounds {
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
                // This wasm ctor must mirror the fallibility of the rust-side type-choice ctor it
                // calls (generate_enum's rep=None path), which is fallible iff the variant type
                // carries an inline value bound (`has_value_bounds()`) — that path emits the inline
                // bounds check and returns `Result`. A *named* type's own fallible `new` is
                // irrelevant here: both ctors receive an already-constructed value, so the inner
                // type's construction (and its own bounds) already happened upstream. Using
                // `needs_bounds_check_if_inlined()` (which also trips on any named `can_new_fail`
                // type, i.e. every bounded Wrapper) would wrongly make this wasm ctor fallible over
                // an infallible rust ctor.
                let can_fail = variant.rust_type().has_value_bounds();
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
                    // Never `try_into` at the wasm boundary: the rust ctor takes an already-built
                    // value, so any inner-type bound was enforced when that value was constructed.
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
        // `true` when `array_type_ident` is an explicit RULE ident (`foo_list = [* foo]`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index` / `--workspace-dep`: if a dependency already owns (index) or a
        // workspace dep owns (unconditional) this exact list wrapper, defer to it (import from the
        // dep's `collections` module) instead of re-minting a duplicate class.
        let shape = format!("[* {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            array_type_ident,
            &element_type.name_as_wasm_array(types),
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        if self.already_generated.insert(array_type_ident.clone()) {
            // Record for the collections.rs index BEFORE the `--wasm-list-macro` early return: the
            // macro still DEFINES the wrapper class, so it belongs in the index exactly like the
            // inline struct below.
            self.record_collection_wrapper(types, array_type_ident, &shape);
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
                // Emit the invocation as a sort-participating item keyed under the wrapper type it
                // defines, so it lands where the equivalent inline struct would (not hoisted to the
                // top above the file header) — see `Scope::raw_sorted`.
                self.wasm(types, array_type_ident).raw_sorted(
                    array_type_ident.as_ref(),
                    &format!("{}!({});", macro_name, args.join(", ")),
                );
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
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            push_list_accessors(&mut wrapper, types, &element_type);
            wrapper.add_conversion_methods(&inner_type, cli);
            wrapper.push(self, types);
        }
    }

    /// Emit the RESTRICTED list wrapper for a `[+ elem]` array — the wasm twin of the loose list
    /// wrapper, but wrapping `core::NonEmptyVec<elem>` instead of `Vec<elem>`. Created via
    /// `try_from` (borrow + clone, so the source loose list/Vec stays valid) or `new(first)`; `add`
    /// stays infallible (a push can't break a `>= 1` bound). `wrapper_ident` is the JS class name —
    /// the synthesized `NonEmpty*List` for inline arrays, or the rule ident for a named `[+ …]`.
    fn generate_non_empty_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`foo = [+ foo]`), so a structural-name
        // coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmpty*List` over a mapped dependency's extern
        // element is a defer candidate exactly like the loose list — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (`try_defer_wrapper`'s rule-declared guard: a named `[+ …]` rule keeps its ident,
        // which differs from the structural `NonEmpty*List`, and is never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_wrapper_name`, which cannot be called
        // here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the map twin below).
        let structural_name = format!("NonEmpty{}List", element_type.conceptual_type.for_variant());
        let shape = format!("[+ {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the element itself needs (nested `[+ [+ int]]`) first
        self.ensure_non_empty_wrappers(types, &element_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let elem_rust = element_type.for_rust_member(types, true, cli);
        let inner_type = format!("NonEmptyVec<{elem_rust}>");
        // the element's structural loose-builder name; when it coincides with THIS wrapper's ident
        // (a self-named rule like `bar_list = [+ bar]`), the loose builder cannot exist — the rule
        // legitimately owns the ident for its restricted class (collision-checked in finalize), so
        // the wrapper emits WITHOUT `try_from` and is built incrementally (`new(first)` + `add`).
        let elem_wasm = element_type.for_wasm_member(types);
        let loose_list = (!element_type.directly_wasm_exposable(types)
            && !element_type.is_non_empty_array())
        .then(|| element_type.name_as_wasm_array(types));
        let self_named = loose_list.as_deref() == Some(wrapper_ident.as_ref());
        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        // Decision 11 (two-type design doc): quote the originating CDDL occurrence so the type
        // name, the doc comment, and the try_from signature are three redundant discovery signals.
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first element (`new(first)` + `add`)."
        } else {
            "Enter via `try_from` or `new(first)`."
        };
        // W2 (`--wrapper-requests`): a requested NonEmpty wrapper sets its own struct doc (above /
        // below), which would clobber the attribution doc `create_base_wasm_struct` injects, so
        // prepend the attribution here. Empty prefix (the common case) leaves output byte-identical.
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`[+ {elem_wasm}]`: at least one element, enforced by the `NonEmptyVec` \
             representation.\n{entry_doc}\n`add` can never violate the bound; removal is checked \
             in the core type."
        ));
        wrapper.s.tuple_field(None, &inner_type);
        // new(first) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first", element_type.for_wasm_param(types))
            .line(format!(
                "Self(NonEmptyVec::new({}))",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "first", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // add stays infallible: a push can never violate the >= 1 lower bound
        push_list_accessors(&mut wrapper, types, &element_type);
        // try_from: the single checked door from the loose form to the restricted wrapper. It
        // BORROWS (and clones) so the source loose list/Vec remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if element_type.directly_wasm_exposable(types) {
            // exposable element: no loose wrapper exists, so take the bare Vec by value (boundary copy)
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("elements", format!("Vec<{elem_wasm}>"))
                .line(
                    "NonEmptyVec::try_from(elements).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        } else if let Some(loose_list) = loose_list.filter(|_| !self_named) {
            // non-exposable, non-nested element: borrow the loose list wrapper and clone it out.
            // Make sure the loose builder exists (inline arrays already mint it; a named `[+ bar]`
            // rule may not have — minting is idempotent via `already_generated`, and a user rule
            // of incompatible shape claiming this ident was rejected at finalize). This mint runs
            // through `try_defer_wrapper` like any other, so a dep-indexed loose source DEFERS —
            // the `try_from` below then borrows the dep's class, whose import is routed at THIS
            // wrapper's emission scope by `scope_references` (the try_from reference is invisible
            // to the field walk — see `register_deferred_non_empty_list_source`).
            self.generate_array_type(
                types,
                element_type.clone(),
                &RustIdent::new(CDDLIdent::new(loose_list.clone())),
                false,
                cli,
            );
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("list", format!("&{loose_list}"))
                .line(format!(
                    "let inner: {} = list.clone().into();",
                    element_type.name_as_rust_array(types, true, cli)
                ))
                .line(
                    "NonEmptyVec::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        }
        // else: self-named rule (loose ident unavailable — see the doc comment) or a nested
        // nonempty element (no clean loose source): built incrementally via new(first)+add only.
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Emit the RESTRICTED table wrapper for a `{+ k => v}` map — the wasm twin of the loose table
    /// wrapper (`codegen_table_type`), but wrapping `core::NonEmptyMap<K, V>` instead of the raw map.
    /// Created via `try_from(&MapKToV)` (borrow + clone, so the source loose wrapper stays valid) or
    /// `new(first_key, first_value)`; `insert` stays infallible (an insert can't break a `>= 1`
    /// bound); removal is checked in the core type. `wrapper_ident` is the JS class name — the
    /// synthesized `NonEmptyMapKToV` for inline maps, or the rule ident for a named `{+ …}`. The
    /// `insert`/`get`/`has`/`keys` accessors are minted by the shared `push_table_accessors` (also
    /// used by `codegen_table_type`), delegating to `self.0`, whose `NonEmptyMap` method surface
    /// matches the raw map's `len`/`insert`/`get`/`keys`.
    #[allow(clippy::too_many_lines)]
    fn generate_non_empty_map_type(
        &mut self,
        types: &IntermediateTypes,
        key_type: RustType,
        value_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`m = {+ k => v}`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmptyMap*` over a mapped dependency's extern
        // key+value is a defer candidate exactly like the loose table — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (rule-declared `{+ …}` rules keep their ident and are never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_map_wrapper_name`, which cannot be
        // called here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the list twin above).
        let structural_name = format!(
            "NonEmpty{}",
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type)
        );
        let shape = format!(
            "{{+ {} => {}}}",
            render_wrapper_shape(&key_type),
            render_wrapper_shape(&value_type)
        );
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the key/value themselves need (nested `{+ …}`) first
        self.ensure_non_empty_wrappers(types, &key_type, cli);
        self.ensure_non_empty_wrappers(types, &value_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let inner_map =
            ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli);
        let inner_type = format!("NonEmptyMap<{}>", {
            // strip the leading table-type token (`BTreeMap<K, V>` / `OrderedHashMap<K, V>`) to reuse
            // the same `K, V` spelling, keeping the wrapper's inner in lockstep with the rust field.
            let open = inner_map.find('<').expect("map type has generics");
            let close = inner_map.rfind('>').expect("map type has generics");
            inner_map[open + 1..close].to_owned()
        });
        // the loose structural table wrapper (`MapKToV`) is the `try_from` source; when its ident
        // coincides with THIS wrapper's ident (a self-named rule like `map_text_to_uint = {+ …}`),
        // the loose builder cannot exist — the rule legitimately owns the ident for its restricted
        // class (collision-checked in finalize), so the wrapper emits WITHOUT `try_from` and is built
        // incrementally (`new(first_key, first_value)` + `insert`).
        let loose_ident = ConceptualRustType::name_for_wasm_map(&key_type, &value_type);
        let self_named = loose_ident.to_string() == wrapper_ident.to_string();

        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        let map_wasm = ConceptualRustType::name_for_wasm_map(&key_type, &value_type);
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first entry (`new(first_key, first_value)` + \
             `insert`)."
        } else {
            "Enter via `try_from` or `new(first_key, first_value)`."
        };
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`{{+ k => v}}` (`{map_wasm}`): at least one entry, enforced by the \
             `NonEmptyMap` representation.\n{entry_doc}\n`insert` can never violate the bound; \
             removal is checked in the core type."
        ));
        wrapper.s.tuple_field(None, &inner_type);
        // new(first_key, first_value) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first_key", key_type.for_wasm_param(types))
            .arg("first_value", value_type.for_wasm_param(types))
            .line(format!(
                "Self(NonEmptyMap::new({}, {}))",
                ToWasmBoundaryOperations::format(
                    key_type
                        .from_wasm_boundary_clone(types, "first_key", false)
                        .into_iter()
                ),
                ToWasmBoundaryOperations::format(
                    value_type
                        .from_wasm_boundary_clone(types, "first_value", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // len
        wrapper
            .s_impl
            .new_fn("len")
            .vis("pub")
            .ret("usize")
            .arg_ref_self()
            .line("self.0.len()");
        // insert / get / has / keys are minted by the shared `push_table_accessors` — the single
        // source of the nullable-value flattening convention, called by both this restricted twin and
        // the loose `codegen_table_type`. See that helper for the rationale comments.
        push_table_accessors(self, &mut wrapper, types, &key_type, &value_type, cli);
        // try_from: the single checked door from the loose table wrapper to the restricted wrapper.
        // It BORROWS (and clones) so the source loose `MapKToV` remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if !self_named {
            // ensure the loose builder exists as the `try_from` source. Inline maps already mint the
            // structural `MapKToV` via the visitor (idempotent with our mint through
            // `already_generated`), and a named `{+ …}` rule may not have — so mint it here. EXCEPT
            // when a PLAIN table rule of the same shape is the SOLE OWNER of `MapKToV`: then the loose
            // builder is that owner's class exposed as a `pub type MapKToV = <Owner>;` alias (emitted
            // by `mint_sole_owner_table`), and minting a second `pub struct MapKToV` here would clash
            // with that alias (E0428). The alias resolves to the owner, whose conversion methods make
            // `map.clone().into()` work, so sharing it is both correct and necessary.
            let shape_has_sole_owner = types
                .table_shape_sole_owners()
                .contains_key(&loose_ident.to_string());
            if !shape_has_sole_owner {
                // This mint runs through `try_defer_wrapper` like any other, so a dep-indexed loose
                // `MapKToV` source DEFERS — the `try_from` below then borrows the dep's class, whose
                // import is routed at THIS wrapper's emission scope by `scope_references` (the
                // try_from reference is invisible to the field walk — see
                // `register_deferred_non_empty_map_source`).
                codegen_table_type(
                    self,
                    types,
                    &loose_ident,
                    key_type.clone(),
                    value_type.clone(),
                    false,
                    cli,
                );
            }
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("map", format!("&{loose_ident}"))
                .line(format!("let inner: {inner_map} = map.clone().into();"))
                .line(
                    "NonEmptyMap::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        }
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Recursively mint the restricted `NonEmpty*List` wrappers a type (at any nesting level) needs.
    /// Named `[+ …]` rules mint their own wrapper under the rule ident elsewhere, so this only fires
    /// on INLINE array shapes (conceptual `Array` carrying the `(Some(1), None)` bounds) that do NOT
    /// dedup to a named rule.
    fn ensure_non_empty_wrappers(&mut self, types: &IntermediateTypes, rt: &RustType, cli: &Cli) {
        match &rt.conceptual_type {
            ConceptualRustType::Array(inner) => {
                if rt.is_non_empty_array() {
                    // dedup-to-named: an inline `[+ elem]` whose element has a NAMED `[+ …]` rule
                    // uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here
                    if types.non_empty_named_owner(inner).is_none() {
                        let ident =
                            RustIdent::new(CDDLIdent::new(rt.non_empty_wasm_wrapper_name(types)));
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, inner, cli);
                }
            }
            ConceptualRustType::Optional(inner) => {
                self.ensure_non_empty_wrappers(types, inner, cli)
            }
            ConceptualRustType::Map(k, v) => {
                if rt.is_non_empty_map() {
                    // dedup-to-named: an inline `{+ k => v}` whose shape has a NAMED `{+ …}` table
                    // rule uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here. Its key/value still get their own nested wrappers.
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                    if types.non_empty_map_named_owner(k, v).is_none() {
                        let ident = RustIdent::new(CDDLIdent::new(
                            rt.non_empty_wasm_map_wrapper_name(types),
                        ));
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                }
            }
            _ => (),
        }
    }
}

/// Emit the shared wasm list-wrapper accessor triple — `len`, `get`, `add` — onto `wrapper`'s impl.
/// The loose `Vec` wrapper (`generate_array_type`) and its restricted `NonEmptyVec` twin
/// (`generate_non_empty_array_type`) deliberately expose the SAME method surface, each accessor
/// delegating to `self.0` identically, so both mint these three through here — the conventions live
/// once. Only `new` differs between the twins (loose: `Self(Vec::new())`; NonEmpty: `new(first)`),
/// so it stays at each call site (along with any site-specific rationale) and is emitted before this.
fn push_list_accessors(
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    element_type: &RustType,
) {
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
}

/// Emit the shared wasm table-wrapper accessor surface — `insert`, `get`, the conditional `has`, and
/// `keys` — onto `wrapper`'s impl, together with the value-nullable machinery all four depend on. The
/// loose map wrapper (`codegen_table_type`) and its restricted `NonEmptyMap` twin
/// (`generate_non_empty_map_type`) deliberately expose the SAME method surface, each accessor
/// delegating to `self.0` identically, so both mint these through here — the nullable-value
/// flattening convention lives once. `new` differs between the twins and `len` is trivial, so both
/// stay at each call site (emitted before this); the `try_from` / conversion tails stay too.
fn push_table_accessors(
    gen_scope: &mut GenerationScope,
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    key_type: &RustType,
    value_type: &RustType,
    cli: &Cli,
) {
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
        ConceptualRustType::Optional(inner) => {
            inner.conceptual_type.directly_wasm_exposable_ct(types)
        }
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
            ".cloned()"
        } else {
            ".map(|v| v.clone().map(Into::into))"
        }
    } else if value_type.directly_wasm_exposable(types) {
        ".cloned()"
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
    // The is_copy value returns `.copied()`, else the boundary modifier computed above applies. The
    // two twins spelled this differently in source — codegen_table_type inlined the `if` in each key
    // branch, generate_non_empty_map_type used this closure — but produced the same bytes; the closure
    // is the single spelling here.
    let copied_or = |modifier: &str| {
        if value_type.is_copy(types) {
            ".copied()".to_owned()
        } else {
            modifier.to_owned()
        }
    };
    if key_type.directly_wasm_exposable(types) {
        getter.line(format!(
            "self.0.get({}){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
            value_flatten
        ));
    } else {
        getter.line(format!(
            "self.0.get({}.as_ref()){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
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
        .ret(keys_type.for_wasm_return_ct(types))
        .vis("pub");
    let key_clone = if key_type.is_copy(types) {
        ".keys().copied()"
    } else {
        ".keys().cloned()"
    };
    // R3d: decide the keys-list wrapper's deferral BEFORE emitting keys() — the keys-list emitter
    // (`generate_array_type`) may run AFTER this map class, so consulting `deferred_wrappers` alone
    // would miss it. `try_defer_wrapper` is idempotent, so this both records the decision (the later
    // emitter re-runs it, suppresses, and the import is routed) and drives the `.into()` here.
    let keys_deferred = !keys_type.directly_wasm_exposable_ct(types)
        && gen_scope.try_defer_wrapper(
            types,
            &RustIdent::new(CDDLIdent::new(key_type.name_as_wasm_array(types))),
            &key_type.name_as_wasm_array(types),
            &[&key_type.conceptual_type],
            &format!("[* {}]", render_wrapper_shape(key_type)),
            false,
            cli,
        );
    if keys_type.directly_wasm_exposable_ct(types) {
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>()"));
    } else if keys_deferred {
        // R3d: the keys-list wrapper is deferred to a dependency (`--extern-wrapper-index`); its tuple
        // field is private cross-crate, so build it through `From<Vec<_>>` (`.into()`) instead of
        // tuple-struct syntax.
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>().into()"));
    } else {
        keys.line(format!(
            "{}(self.0{key_clone}.collect::<Vec<_>>())",
            keys_type.for_wasm_return_ct(types)
        ));
    }
    wrapper.s_impl.push_fn(keys);
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
fn encoding_var_macros(key_demand: Option<DemandSet>, custom_json: bool, cli: &Cli) -> String {
    let mut ret = if let Some(demand) = key_demand {
        format!(
            "#[derivative({})]\n",
            key_trait_list(demand, true, cli)
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
        Cow::from(expr)
    } else {
        Cow::from(format!("&{expr}"))
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
        "{{ {}Err(DeserializeFailure::RangeCheck{{ found: {} as i128, min: {}, max: {}}}.into()) }}",
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
    // The `[+ T]` shape enforces its `>= 1` bound at the type level (NonEmptyVec's single TryFrom
    // door), so no inline length check is emitted at ctor/setter/deser sites — the invalid state is
    // unrepresentable. Every OTHER array bound (2*5, *3, …) keeps this runtime-check path. Alias-
    // resolving so a field referencing a named `[+ …]` rule skips the check too.
    if ty.is_type_enforced_non_empty() {
        return None;
    }
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
pub(crate) struct BlocksOrLines(Vec<BlockOrLine>);

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

pub(crate) trait CodeBlock {
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
        types.key_demand(ident),
        false,
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
    } else if !scope.export() {
        // A non-exported (cross-crate extern-dep) scope already stores the dependency's crate as its
        // leading component (the `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` prefix is stripped by
        // `ModuleScope::from`), so `dep_crate::sub` is the dep's own rust path. Prefixing the
        // generated crate's own lib name would mint `cddl_lib::dep_crate::sub`, a path that exists in
        // no crate. The rust type lives in the dep's rust crate regardless of the wasm-crate mapping.
        scope.to_string()
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
        // using Scope::raw() for the macro calls would result in them all being included at the top of the
        // file, so we instead use the impl's macro spot to put them before the impl where we want them.
        // (For a standalone invocation with no impl to attach to — the --wasm-list-macro case — the
        // equivalent is Scope::raw_sorted, which sorts the text where a struct of that name would.)
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
    // W2 (`--wrapper-requests`): a requested wrapper carries a `/// Generated at the request of: …`
    // attribution doc. Set here so the loose list / map emitters (which set no struct doc of their
    // own) carry it; the NonEmpty emitters set their own struct doc and PREPEND this text via
    // `requested_attribution_prefix` (a `.doc()` call replaces, not appends). Empty map off the flag,
    // so own-spec wrappers are byte-identical.
    if let Some(doc) = gen_scope.requested_attribution.get(ident) {
        s.doc(doc);
    }
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
        // For Fixed, ending_check is the "()" placeholder (length already fully checked at the
        // start); emitting it as a statement would produce a standalone `();` (clippy::no_effect).
        Some(Representation::Map) if !matches!(len_info, RustStructCBORLen::Fixed(_)) => {
            deser_body.line(&format!("{ending_check};"));
        }
        Some(Representation::Map) => {}
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
// * `annotated` - true iff deser_body will end up inside an `.annotate(name)` error closure. The
//   tag-mismatch error must then be the locationless form (`DeserializeFailure::..into()`): the
//   closure's map_err supplies the type name, and the location-carrying form
//   (`DeserializeError::new(name, ..)`) would get the name PREPENDED again ("Name.Name"). When no
//   closure exists (annotate_fields=false) the named form is required or the name would be lost
//   entirely. This param governs the NON-embedded emission below; the embedded (plain-group)
//   deserialize() case builds its own scaffolding closures when `cli.annotate_fields` (handled in a
//   dedicated branch at the top of the fn) and ignores `annotated` — the codegen_struct caller
//   therefore passes it `false` for plain groups.
#[allow(clippy::too_many_arguments)]
fn create_deserialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    len_info: Option<RustStructCBORLen>,
    generate_deserialize_embedded: bool,
    store_encoding: Option<&str>,
    deser_body: &mut dyn CodeBlock,
    annotated: bool,
    cli: &Cli,
) -> (codegen::Impl, Option<codegen::Impl>) {
    let name = &ident.to_string();
    let mut deser_impl = codegen::Impl::new(name);
    // TODO: add config param to decide if we want to use our deserialize
    //       or theirs using Error::Custom(String) + DeserializeError::to_string()
    //deser_impl.impl_trait("cbor_event::de::Deserialize");
    deser_impl.impl_trait("Deserialize");
    // Plain-group (embedded) deserialize() with annotation on: the pre-delegation scaffolding (tag
    // read/check + container-len read + read_len construction/initial checks) and the post-delegation
    // final-len check each get their OWN `.annotate(name)` error closure so a wrong-major /
    // wrong-length / missing-break rejection carries the type name — exactly like the non-embedded
    // record path already annotates its scaffolding. The `deserialize_as_embedded_group` delegation
    // stays OUTSIDE any closure: its body is already annotated per-field, so wrapping it would
    // double-annotate ("Type.Type.field"). This branch only fires for embedded groups (never the enum
    // path, which passes generate_deserialize_embedded=false); every other case keeps the original
    // sequential emission below unchanged, so non-embedded records and enums stay byte-identical.
    if generate_deserialize_embedded && cli.annotate_fields {
        let rep = rep.expect("embedded groups always have an array/map representation");
        let len_info =
            len_info.expect("embedded plain-group deserialize() is always given its len_info");
        // Pre-delegation scaffolding, built into a closure returning the bindings later code needs.
        let mut pre = BlocksOrLines::default();
        if let Some(tag) = tag {
            if cli.preserve_encodings {
                pre.line("let (tag, tag_encoding) = raw.tag_sz()?;");
            } else {
                pre.line("let tag = raw.tag()?;");
            }
            // Inside the annotate closure, so the locationless form (the closure supplies the name).
            let mut tag_check = Block::new(format!("if tag != {tag}"));
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
            pre.push_block(tag_check);
        }
        match rep {
            Representation::Array => {
                pre.line(if cli.preserve_encodings {
                    "let len = raw.array_sz()?;"
                } else {
                    "let len = raw.array()?;"
                });
            }
            Representation::Map => {
                pre.line(if cli.preserve_encodings {
                    "let len = raw.map_sz()?;"
                } else {
                    "let len = raw.map()?;"
                });
            }
        }
        // Inline the read_len construction + initial checks instead of calling
        // add_deserialize_initial_len_check: here the delegation's `&mut read_len` use lives OUTSIDE
        // the closure, so `read_len` is only mutated inside the closure when a `read_elems` is
        // emitted (Fixed>0 / OptionalFields>0). Binding it `mut` unconditionally (as the shared
        // helper does, correct there because the delegation follows in-scope) would emit `unused_mut`
        // for the Dynamic / Fixed(0) / OptionalFields(0) cases. Everything else matches the helper.
        let read_len_mutated = matches!(len_info, RustStructCBORLen::Fixed(f) if f != 0)
            || matches!(len_info, RustStructCBORLen::OptionalFields(m) if m != 0);
        pre.line(&format!(
            "let {}read_len = {}(len);",
            if read_len_mutated { "mut " } else { "" },
            cbor_read_len_ctor(cli)
        ));
        match len_info {
            RustStructCBORLen::Dynamic => {}
            RustStructCBORLen::OptionalFields(mandatory) => {
                if mandatory != 0 {
                    pre.line(&format!("read_len.read_elems({mandatory})?;"));
                }
            }
            RustStructCBORLen::Fixed(fixed) => {
                if fixed != 0 {
                    pre.line(&format!("read_len.read_elems({fixed})?;"));
                }
                pre.line("read_len.finish()?;");
            }
        }
        pre.line("Ok((len, read_len))");
        let mut pre_closure = make_err_annotate_block(name, "let (len, mut read_len) = ", "?;");
        pre_closure.push_all(pre);
        deser_body.push_block(pre_closure);
        // Delegation OUTSIDE any closure (its per-field errors are already annotated).
        deser_body.line("let ret = Self::deserialize_as_embedded_group(raw, &mut read_len, len);");
        // Post-delegation final-len check (ending break / trailing-length), wrapped in its own
        // annotate closure so a missing-break / definite-len-mismatch rejection carries the name.
        let mut post = BlocksOrLines::default();
        add_deserialize_final_len_check(&mut post, Some(rep), len_info, cli);
        if !post.0.is_empty() {
            let mut post_closure = make_err_annotate_block(name, "", "?;");
            post_closure.push_all(post);
            post_closure.line("Ok(())");
            deser_body.push_block(post_closure);
        }
        deser_body.line("ret");
        let mut embedded_impl = codegen::Impl::new(name);
        embedded_impl.impl_trait("DeserializeEmbeddedGroup");
        return (deser_impl, Some(embedded_impl));
    }
    if let Some(tag) = tag {
        if cli.preserve_encodings {
            deser_body.line("let (tag, tag_encoding) = raw.tag_sz()?;");
        } else {
            deser_body.line("let tag = raw.tag()?;");
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        if annotated {
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
        } else {
            tag_check.line(format!("return Err(DeserializeError::new(\"{name}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        }
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
    let mut brk = Block::new(format!(
        "if matches!({len_var}, {}) && raw.cbor_type()? == cbor_event::Type::Special && raw.special_break()?",
        cbor_event_len_indef(cli)
    ));
    brk.line("break;");
    brk
}

pub fn table_type(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "OrderedHashMap"
    } else {
        "BTreeMap"
    }
}

/// The CDDL prelude spelling of a primitive, for the canonical shape renderer. Kept in lockstep with
/// the wasm-map/list structural naming: the dep re-parses a rendered shape and must derive the SAME
/// structural name, so each primitive renders to a CDDL name whose `for_variant` round-trips (e.g.
/// `uint` -> `U64` -> `MapU64To…`). `u8`/`i8`/… are cddl-codegen's own sized-int spellings.
fn primitive_cddl_name(p: &Primitive) -> &'static str {
    match p {
        Primitive::Bool => "bool",
        Primitive::F64 => "float64",
        Primitive::F32 => "float32",
        Primitive::U8 => "u8",
        Primitive::I8 => "i8",
        Primitive::U16 => "u16",
        Primitive::I16 => "i16",
        Primitive::U32 => "u32",
        Primitive::I32 => "i32",
        Primitive::U64 => "uint",
        Primitive::I64 => "i64",
        Primitive::N64 => "nint",
        Primitive::Str => "text",
        Primitive::Bytes => "bytes",
    }
}

/// Render a collection wrapper's CDDL shape fragment in the canonical W1 shape-column grammar —
/// `[* foo]` / `[+ foo]` for loose / non-empty lists, `{* k => v}` / `{+ k => v}` for maps, nesting
/// recursively (`[* [* foo]]`, `[* [+ foo]]`). Element idents are the dependency's own spec spelling
/// (snake_case of the rust ident, matching the extern-stub naming a dep re-parses after
/// normalization); primitives render as their CDDL prelude name. The occurrence marker is taken from
/// the `RustType`'s own bounds so nested non-empty shapes are honored at every level. This is the
/// single shape renderer shared by the not-in-index warning hint and (later) the request-sidecar
/// machinery, so its output is EXACTLY the format a dep parses back.
pub(crate) fn render_wrapper_shape(rt: &RustType) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            let occ = if rt.is_non_empty_array() { "+" } else { "*" };
            format!("[{occ} {}]", render_wrapper_shape(inner))
        }
        ConceptualRustType::Map(key, value) => {
            let occ = if rt.is_non_empty_map() { "+" } else { "*" };
            format!(
                "{{{occ} {} => {}}}",
                render_wrapper_shape(key),
                render_wrapper_shape(value)
            )
        }
        // An optional isn't itself a wrapper occurrence — render its inner shape (only reachable via
        // nesting; the top-level constituents the callers pass are Array/Map/named-leaf).
        ConceptualRustType::Optional(inner) => render_wrapper_shape(inner),
        ConceptualRustType::Rust(ident) => convert_to_snake_case(ident.as_ref()),
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => {
            convert_to_snake_case(ident.as_ref())
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(name), _) => name.clone(),
        ConceptualRustType::Primitive(p) => primitive_cddl_name(p).to_owned(),
        // Fixed values carry no CDDL ident and never appear as a real wrapper element; render a
        // placeholder rather than panicking so the advisory hint text stays best-effort.
        ConceptualRustType::Fixed(_) => "_".to_owned(),
    }
}

/// The top-level NAMED rust idents of a wrapper constituent (element / key / value) — what the defer
/// decision resolves to a dependency scope. Primitives / fixed values contribute none; an alias
/// contributes its aliased ident; an optional passes through to its inner type.
fn named_constituent_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => named_constituent_idents(&inner.conceptual_type),
        _ => vec![],
    }
}

/// The TRANSITIVE named leaf idents of a wrapper constituent — `named_constituent_idents` extended to
/// descend through nested `Array`/`Map` conceptual types to the named types at the leaves. A
/// `[* [* foo]]` has leaf `foo` (its inner wrapper is classified independently); `{* a => [* b]}` has
/// leaves `a` and `b`. Primitives / fixed values contribute none; alias / optional unwrap to their
/// inner. This is what workspace placement resolves to dependency owners.
fn transitive_named_leaf_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Array(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Map(key, value) => {
            let mut out = transitive_named_leaf_idents(&key.conceptual_type);
            out.extend(transitive_named_leaf_idents(&value.conceptual_type));
            out
        }
        _ => vec![],
    }
}

/// The set of element OWNERS of a wrapper's constituents, computed transitively to the named leaves.
/// Each leaf resolves to `Some(dep)` when it is an extern type (leading component of its non-exported
/// scope) or `None` when it is a consumer-owned (exported) type. An empty set means "ownerless" (no
/// named leaves — a primitives-only wrapper like `{* uint => text}`). This is the input to
/// `wrapper_placement`.
fn transitive_owner_set(
    types: &IntermediateTypes,
    constituents: &[&ConceptualRustType],
) -> BTreeSet<Option<String>> {
    let mut owners = BTreeSet::new();
    for c in constituents {
        for id in transitive_named_leaf_idents(c) {
            let scope = types.scope(&id);
            owners.insert(if scope.export() {
                None
            } else {
                scope.components().first().cloned()
            });
        }
    }
    owners
}

/// Where a collection wrapper is hosted, given its transitive element owners. Factored as one
/// function so the placement rule can generalize (plan decision 4): today `Borrow(dep)` iff the
/// wrapper has EXACTLY ONE owner, that owner is a named dependency, and that dependency is a
/// `--workspace-dep`; every other case (ownerless, mixed-dep, a lone non-workspace owner, any
/// consumer-owned leaf) is `Local`. The future rule ("latest of the element owners" / least upper
/// bound in a DAG) replaces this body without touching call sites.
enum WrapperPlacement {
    Local,
    Borrow(String),
}

fn wrapper_placement(
    owners: &BTreeSet<Option<String>>,
    workspace_deps: &BTreeSet<String>,
) -> WrapperPlacement {
    if owners.len() == 1
        && let Some(Some(dep)) = owners.iter().next()
        && workspace_deps.contains(dep)
    {
        return WrapperPlacement::Borrow(dep.clone());
    }
    WrapperPlacement::Local
}

/// Validate `--workspace-dep` values (plan decision 6) and return the set. Each named dep must be a
/// configured extern dependency (`extern_dep_names()`) AND have an `--extern-wasm-crate` mapping —
/// the deferral imports and the sidecar's `use` lines both need the wasm crate name, so a missing
/// mapping is a hard error rather than a silent fallback. Mirrors `load_extern_wrapper_indices`'
/// startup hardening. The accessor already rejected empty / `=`-bearing values.
fn load_workspace_deps(types: &IntermediateTypes, cli: &Cli) -> BTreeSet<String> {
    let deps = cli.workspace_deps();
    if deps.is_empty() {
        return BTreeSet::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let wasm_crate_map = cli.extern_wasm_crate_map();
    for dep in &deps {
        if !extern_dep_names.contains(dep) {
            panic!(
                "--workspace-dep names dependency {dep:?}, which is not an extern dependency in this \
                 spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        if !wasm_crate_map.contains_key(dep) {
            panic!(
                "--workspace-dep {dep:?} has no --extern-wasm-crate mapping; workspace deferral needs \
                 the dep's wasm crate name for its imports and the borrowed-collections sidecar. Add \
                 --extern-wasm-crate {dep}=<wasm_crate>."
            );
        }
    }
    deps
}

// ===== W2 dep side (`--wrapper-requests`): shape reconstruction + structural naming ===============

/// Reverse of `primitive_cddl_name`: the `Primitive` a shape-column leaf denotes, or `None` for a
/// named-type leaf. Only the exact spellings `render_wrapper_shape` emits for primitive leaves are
/// recognized, so a dep type whose snake-case happens NOT to be a prelude name is correctly treated
/// as a named element.
fn primitive_from_cddl_name(name: &str) -> Option<Primitive> {
    Some(match name {
        "bool" => Primitive::Bool,
        "float64" => Primitive::F64,
        "float32" => Primitive::F32,
        "u8" => Primitive::U8,
        "i8" => Primitive::I8,
        "u16" => Primitive::U16,
        "i16" => Primitive::I16,
        "u32" => Primitive::U32,
        "i32" => Primitive::I32,
        "uint" => Primitive::U64,
        "i64" => Primitive::I64,
        "nint" => Primitive::N64,
        "text" => Primitive::Str,
        "bytes" => Primitive::Bytes,
        _ => return None,
    })
}

/// Whether this dep's OWN spec defines `ident` (a generated struct/enum or a user type alias) as an
/// exported, in-crate type. A non-exported (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/…`) scope means the type
/// belongs to one of the DEP's own deps, not the dep itself, so it is NOT owned.
fn dep_owns_element(types: &IntermediateTypes, ident: &RustIdent) -> bool {
    let known = types.rust_struct(ident).is_some()
        || types
            .type_aliases()
            .contains_key(&AliasIdent::Rust(ident.clone()));
    known && types.scope(ident).export()
}

/// Reconstruct a requested wrapper's `RustType` from its canonical shape column, resolving each
/// named leaf against the DEP's own IR after the same normalization (`RustIdent::new`, which
/// camel-cases and folds `-`/`_`) type-name derivation uses. A leaf the dep does not own is a hard
/// error (criterion 8 #1). `consumer`/`path`/`listed_name` are threaded only for actionable errors.
fn parse_requested_shape(
    types: &IntermediateTypes,
    shape: &str,
    consumer: &str,
    path: &str,
    listed_name: &str,
) -> RustType {
    let chars: Vec<char> = shape.chars().collect();
    let mut pos = 0;
    let rt = parse_shape_fragment(
        types,
        &chars,
        &mut pos,
        consumer,
        path,
        shape,
        listed_name,
        0,
    );
    while pos < chars.len() && chars[pos].is_whitespace() {
        pos += 1;
    }
    if pos != chars.len() {
        panic!(
            "--wrapper-requests {consumer} ({path}): trailing content after the shape {shape:?} \
             (wrapper {listed_name:?})."
        );
    }
    rt
}

/// Depth cap for `parse_shape_fragment`'s recursion. Real wrapper shapes nest 2–3 deep; 32 is a
/// generous ceiling that turns a pathological hand-edited sidecar (thousands of `[* [* …]]` levels)
/// into an actionable hard error instead of a stack-overflow abort.
const MAX_SHAPE_DEPTH: usize = 32;

#[allow(clippy::too_many_arguments)]
fn parse_shape_fragment(
    types: &IntermediateTypes,
    chars: &[char],
    pos: &mut usize,
    consumer: &str,
    path: &str,
    shape: &str,
    listed_name: &str,
    depth: usize,
) -> RustType {
    let skip_ws = |pos: &mut usize| {
        while *pos < chars.len() && chars[*pos].is_whitespace() {
            *pos += 1;
        }
    };
    let bad = |what: &str| -> ! {
        panic!(
            "--wrapper-requests {consumer} ({path}): malformed shape {shape:?} (wrapper \
             {listed_name:?}): {what}."
        );
    };
    if depth > MAX_SHAPE_DEPTH {
        panic!(
            "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
             (shape {shape:?}) nests collections deeper than the supported limit of \
             {MAX_SHAPE_DEPTH}. Real wrapper shapes nest only a few levels; this is almost \
             certainly a malformed hand-edited sidecar."
        );
    }
    skip_ws(pos);
    if *pos >= chars.len() {
        bad("unexpected end of shape");
    }
    match chars[*pos] {
        '[' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let inner = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != ']' {
                bad("expected `]`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Array(Box::new(inner)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        '{' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let key = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if !(chars.get(*pos) == Some(&'=') && chars.get(*pos + 1) == Some(&'>')) {
                bad("expected `=>`");
            }
            *pos += 2;
            skip_ws(pos);
            let value = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != '}' {
                bad("expected `}`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Map(Box::new(key), Box::new(value)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        _ => {
            // A named or primitive leaf: read the ident token.
            let start = *pos;
            while *pos < chars.len()
                && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
            {
                *pos += 1;
            }
            if *pos == start {
                bad("expected an element type name");
            }
            let token: String = chars[start..*pos].iter().collect();
            if let Some(p) = primitive_from_cddl_name(&token) {
                return RustType::new(ConceptualRustType::Primitive(p));
            }
            // A reserved CDDL keyword (`biguint`, `bigint`, …) or reserved Rust type name
            // (`option` → `Option`) as a leaf token would trip `RustIdent::new`'s internal asserts
            // — an internal panic reachable only from a hand-edited sidecar (a real consumer never
            // emits these). Pre-check through the reservation rule's one owner
            // (`RustIdent::reserved_reason`, the same predicate `new` asserts on) so external
            // input surfaces the feature's own hard error instead of the assert.
            if RustIdent::reserved_reason(&token).is_some() {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) uses the reserved identifier {token:?} as a wrapper element; \
                     reserved CDDL keywords and reserved Rust type names cannot be wrapper elements."
                );
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            if !dep_owns_element(types, &ident) {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) references the element type {token:?}, which this dep does not \
                     own. The consumer's extern stub for this dep and the dep's own spec disagree — \
                     the request cannot be satisfied."
                );
            }
            // Resolve through the pipeline's one alias-substitution rule (`resolve_alias`, shared
            // with `new_type` so this path cannot drift from pipeline resolution): a leaf left as
            // a bare `Rust(ident)` naming an alias (`stake_credential = credential`, `policy_id =
            // script_hash`) panics downstream lookups (`is_enum`, exposability, member naming)
            // that assume `Rust(ident)` names a registered struct. The `Alias` wrapper the rule
            // keeps for rust-alias-generating rules preserves the requested ident for structural
            // naming (the consumer derived `StakeCredentialList` from the alias name) while
            // resolving storage/exposability through the target, matching what the dep's own
            // generation of the same CDDL shape would produce. `dep_owns_element` already required
            // a spec-registered ident, so `new_type`'s unregistered-reserved prelude fallback (the
            // one mutable part) cannot be needed here.
            types
                .resolve_alias(&AliasIdent::Rust(ident.clone()))
                .unwrap_or_else(|| RustType::new(ConceptualRustType::Rust(ident)))
        }
    }
}

/// Read a `*`/`+` occurrence marker at `chars[*pos]`, advancing past it.
fn read_occurrence(chars: &[char], pos: &mut usize) -> Option<char> {
    match chars.get(*pos) {
        Some('*') => {
            *pos += 1;
            Some('*')
        }
        Some('+') => {
            *pos += 1;
            Some('+')
        }
        _ => None,
    }
}

/// The owner-INDEPENDENT structural wrapper name for a reconstructed requested shape — the exact
/// spelling the consumer's emitter passed to `try_defer_wrapper` and recorded in its sidecar. Uses
/// the raw `NonEmpty*List` / `NonEmpty<MapKToV>` forms (NOT `non_empty_wasm_wrapper_name`, which
/// consults named owners) so a dep that authored a `[+ …]` rule surfaces as a name↔shape/own-spec
/// disagreement rather than silently matching. Panics for a non-collection top level (a hand-edited
/// sidecar row).
fn requested_structural_name(
    types: &IntermediateTypes,
    rt: &RustType,
    consumer: &str,
    path: &str,
) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if rt.is_non_empty_array() {
                format!("NonEmpty{}List", inner.conceptual_type.for_variant())
            } else {
                inner.conceptual_type.name_as_wasm_array_ct(types)
            }
        }
        ConceptualRustType::Map(k, v) => {
            if rt.is_non_empty_map() {
                format!("NonEmpty{}", ConceptualRustType::name_for_wasm_map(k, v))
            } else {
                ConceptualRustType::name_for_wasm_map(k, v).to_string()
            }
        }
        other => panic!(
            "--wrapper-requests {consumer} ({path}): a requested shape must be a collection wrapper \
             (list or map), got {other:?}."
        ),
    }
}

/// If a reconstructed requested shape is DIRECTLY WASM-EXPOSABLE (it lowers to a bare `Vec<…>` with
/// no wrapper class), return that member spelling; otherwise `None`. Mirrors `name_as_wasm_array_ct`'s
/// own exposability test exactly (rebuild `Array(inner)` and ask `directly_wasm_exposable_ct`) rather
/// than sniffing a rendered string. A `Map` top level is never directly exposable; a `[+ …]` NonEmpty
/// array always gets a wrapper class, so only the loose-array (`[* …]`) case can be exposable.
fn requested_exposable_member(types: &IntermediateTypes, rt: &RustType) -> Option<String> {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) if !rt.is_non_empty_array() => {
            if ConceptualRustType::Array(Box::new(inner.conceptual_type.clone().into()))
                .directly_wasm_exposable_ct(types)
            {
                Some(inner.conceptual_type.name_as_wasm_array_ct(types))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Describe how this dep resolves each NAMED leaf element written in a requested shape's shape column,
/// for the actionable exposable-shape / name↔shape diagnostics. Walks the ORIGINAL shape tokens (not
/// the reconstructed `RustType`, which has already substituted `@no_alias` idents away) so the message
/// names the ident the operator wrote and its resolution target. Primitive leaves contribute nothing.
/// Only reached after a successful `parse_requested_shape`, so every named token is an owned,
/// non-reserved ident — `RustIdent::new` cannot trip.
fn requested_shape_leaf_resolutions(types: &IntermediateTypes, shape: &str) -> Vec<String> {
    let chars: Vec<char> = shape.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-' {
            let start = i;
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-')
            {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            if primitive_from_cddl_name(&token).is_some() {
                continue;
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            out.push(describe_leaf_resolution(types, &token, &ident));
        } else {
            i += 1;
        }
    }
    out
}

/// One leaf's resolution phrase: a registered struct, a kept alias (rust alias preserving the ident),
/// or a transparent (`@no_alias` / passthrough) substitution to its base. Consults `type_aliases()`,
/// the same table `parse_shape_fragment`'s leaf arm resolves through.
fn describe_leaf_resolution(types: &IntermediateTypes, token: &str, ident: &RustIdent) -> String {
    match types.type_aliases().get(&AliasIdent::Rust(ident.clone())) {
        Some(info) => {
            let target = render_wrapper_shape(&info.base_type);
            if info.gen_rust_alias {
                format!("`{token}` (a kept alias resolving to `{target}`)")
            } else {
                format!("`{token}` (transparently substituted to `{target}`)")
            }
        }
        None => format!("`{token}` (a registered struct)"),
    }
}

/// The immediate nested collection shapes of a requested wrapper (canonical form), used for the
/// inner-closure integrity check (criterion 8 #5). Only ONE level: deeper nesting is covered
/// transitively because each level is a separately-requested (and separately-checked) entry.
fn inner_collection_shapes(rt: &RustType) -> Vec<String> {
    let is_collection = |rt: &RustType| {
        matches!(
            rt.conceptual_type,
            ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
        )
    };
    let mut out = Vec::new();
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if is_collection(inner) {
                out.push(render_wrapper_shape(inner));
            }
        }
        ConceptualRustType::Map(k, v) => {
            if is_collection(k) {
                out.push(render_wrapper_shape(k));
            }
            if is_collection(v) {
                out.push(render_wrapper_shape(v));
            }
        }
        _ => {}
    }
    out
}

/// Parse every `--extern-wrapper-index <dep>=<path>` file into `dep -> {wrapper class names}`. Each
/// file is a dependency's committed `generated/collections.rs`: `pub use <path>::<Name>;` lines (plus
/// blank / `//` comment lines). Any other non-blank line is a hard error — the format is ours, and a
/// silently-tolerated stray line would let a malformed index disable deferral and reintroduce the
/// duplicate-symbol link error. Mapping keys are validated against `extern_dep_names()` first (a typo
/// there has the same silent-disable failure mode), mirroring `--extern-wasm-crate`.
fn load_extern_wrapper_indices(
    types: &IntermediateTypes,
    cli: &Cli,
) -> BTreeMap<String, BTreeSet<String>> {
    let files = cli.extern_wrapper_index_files();
    if files.is_empty() {
        return BTreeMap::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let mut out = BTreeMap::new();
    for (dep, path) in files {
        if !extern_dep_names.contains(&dep) {
            panic!(
                "--extern-wrapper-index names dependency {dep:?}, which is not an extern dependency \
                 in this spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        let contents = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("--extern-wrapper-index {dep}={path}: cannot read the index file: {e}")
        });
        let mut names = BTreeSet::new();
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            // Fixed shape: `pub use <path>::<Name>;` — take the segment after the last `::`.
            let name = line
                .strip_prefix("pub use ")
                .and_then(|rest| rest.strip_suffix(';'))
                .and_then(|path| path.rsplit("::").next())
                .filter(|name| {
                    !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_')
                });
            match name {
                Some(name) => {
                    names.insert(name.to_owned());
                }
                None => panic!(
                    "--extern-wrapper-index {dep}={path}: unexpected line {line:?}; the index is a \
                     generated `collections.rs` of `pub use <path>::<Name>;` re-export lines"
                ),
            }
        }
        out.insert(dep, names);
    }
    out
}

/// Mint the wasm structural wrapper class for a single visited `ConceptualRustType` (the per-type body
/// of the wasm-wrapper visit). Shared by the rust-struct walk and the wasm-alias-target walk so both
/// reach identical minting decisions (sole-owner routing, map-key array wrappers). Idempotent via
/// `wasm_wrappers_generated`; every class body is derived purely from the shape, so the result is
/// iteration-order-independent.
fn mint_wasm_wrapper_for_visited_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    ty: &ConceptualRustType,
    wasm_wrappers_generated: &mut BTreeSet<String>,
    table_shape_sole_owner: &BTreeMap<String, RustIdent>,
    cli: &Cli,
) {
    match ty {
        ConceptualRustType::Array(elem) => {
            if !ty.directly_wasm_exposable_ct(types) {
                let array_ident = elem.name_as_wasm_array(types);
                if wasm_wrappers_generated.insert(array_ident.clone()) {
                    gen_scope.generate_array_type(
                        types,
                        *elem.clone(),
                        &RustIdent::new(CDDLIdent::new(array_ident)),
                        false,
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
                    gen_scope,
                    types,
                    owner,
                    &map_ident,
                    wasm_wrappers_generated,
                    cli,
                ),
                // Anonymous-only shape (or a same-shape rule pair): mint the
                // structural class, whose inner is the raw map (not a rust rule).
                None => {
                    if wasm_wrappers_generated.insert(map_ident.to_string()) {
                        codegen_table_type(
                            gen_scope,
                            types,
                            &map_ident,
                            *k.clone(),
                            *v.clone(),
                            false,
                            cli,
                        );
                    }
                }
            }
            if !ConceptualRustType::Array(Box::new(*k.clone())).directly_wasm_exposable_ct(types) {
                let keys_ident = k.name_as_wasm_array(types);
                if wasm_wrappers_generated.insert(keys_ident.clone()) {
                    gen_scope.generate_array_type(
                        types,
                        *k.clone(),
                        &RustIdent::new(CDDLIdent::new(keys_ident)),
                        false,
                        cli,
                    );
                }
            }
        }
        _ => (),
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
        let (domain, range) = {
            let owner_struct = types
                .rust_structs()
                .get(owner)
                .expect("sole owner of a table shape must be a rust struct");
            match owner_struct.variant() {
                RustStructType::Table { domain, range, .. } => (domain.clone(), range.clone()),
                _ => unreachable!("sole owner of a table shape must be a Table rust struct"),
            }
        };
        // `exists_in_rust = true`: the inner is the rust crate's `pub type <owner>` alias (exactly the
        // struct-field role's inner), not the raw inline map. Any CBOR tag on the owner is honored by
        // that rust type's serialization, so it is not threaded into this wasm wrapper.
        codegen_table_type(gen_scope, types, owner, domain, range, true, cli);
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
    exists_in_rust: bool,
    cli: &Cli,
) {
    assert!(cli.wasm);
    // `--extern-wrapper-index`: only the anonymous STRUCTURAL map wrapper (`!exists_in_rust`, name ==
    // `name_for_wasm_map`) is a defer candidate — a rule-owned class (`exists_in_rust`) is the
    // consumer's own type. If a mapped dependency owns this exact structural map wrapper, defer to it
    // (import from the dep's `collections` module) instead of re-minting a duplicate class.
    let shape = format!(
        "{{* {} => {}}}",
        render_wrapper_shape(&key_type),
        render_wrapper_shape(&value_type)
    );
    if !exists_in_rust
        && gen_scope.try_defer_wrapper(
            types,
            name,
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type).as_ref(),
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            // Only the anonymous STRUCTURAL map wrapper reaches here (`!exists_in_rust`); a
            // rule-declared table is screened out above and never a defer candidate.
            false,
            cli,
        )
    {
        return;
    }
    // Idempotency guard, unified with the array wrappers' `already_generated`: the loose structural
    // `MapKToV` builder can be requested BOTH by the wasm-wrapper visitor (a plain `{* k => v}` use)
    // AND directly by `generate_non_empty_map_type` (as a `{+ k => v}` wrapper's `try_from` source);
    // without a shared guard those two paths would double-define the class (E0428). The callers' own
    // dedup sets (`wasm_wrappers_generated` / `generated`) remain — this only ADDS protection, so
    // every existing single-mint path stays byte-identical (the guard passes on first request).
    if !gen_scope.already_generated.insert(name.clone()) {
        return;
    }
    gen_scope.record_collection_wrapper(types, name, &shape);
    // No `tag` parameter: this emits ONLY the wasm wrapper class (accessors + delegation). When the
    // shape has a CBOR tag (`#6.n({ ... })`), the tag is owned entirely by the rust crate's type,
    // which this wrapper's single tuple field holds (via `rust_crate_struct_from_wasm` when
    // `exists_in_rust`); that type's serialize/deserialize writes/checks the tag. The wrapper adds no
    // serialization of its own, so it has nothing to do with the tag — hence the caller's tag is not
    // threaded here.
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
    // insert / get / has / keys (and the nullable-value flattening convention they share) are minted
    // by `push_table_accessors`, also called by the restricted `NonEmptyMap` twin
    // (`generate_non_empty_map_type`).
    push_table_accessors(gen_scope, &mut wrapper, types, &key_type, &value_type, cli);
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
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            enc_conversion_before: "Some(",
            enc_conversion_after: ")",
            is_copy: true,
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
            };
            let inner_encs =
                encoding_fields_impl(types, &format!("{name}_elem"), (&**elem_ty).into(), cli);
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = tuple_type_name(&inner_encs);
                vec![
                    base,
                    EncodingField {
                        field_name: format!("{name}_elem_encodings"),
                        type_name: format!("Vec<{type_name_elem}>"),
                        default_expr: "Vec::new()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: false,
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
            }];
            let key_encs = encoding_fields_impl(types, &format!("{name}_key"), (&**k).into(), cli);
            let val_encs =
                encoding_fields_impl(types, &format!("{name}_value"), (&**v).into(), cli);

            if !key_encs.is_empty() {
                let type_name_value = tuple_type_name(&key_encs);
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
                });
            }

            if !val_encs.is_empty() {
                let type_name_value = tuple_type_name(&val_encs);
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

// Value-level twin of `tuple_type_name`: joins encoding VAR names into a parenthesized tuple.
fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

// Type-level twin of `tuple_str`: joins encoding fields' `type_name`s into a parenthesized tuple
// type unless there is exactly one (then the lone type_name stands alone, unparenthesized).
fn tuple_type_name(encs: &[EncodingField]) -> String {
    if encs.len() == 1 {
        encs[0].type_name.clone()
    } else {
        format!(
            "({})",
            encs.iter()
                .map(|enc| enc.type_name.clone())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// True iff every encoding field's `default_expr` is a trivial literal (`None`/`false`) rather than
/// a function call (`LenEncoding::default()`, `Vec::new()`, `BTreeMap::new()`,
/// `StringEncoding::default()`). Trivial-literal tuple defaults may be emitted with `unwrap_or(..)`;
/// a call-bearing default must stay behind `unwrap_or_else(|| ..)` or clippy::or_fun_call fires.
/// Centralized so every tuple-default emission site agrees on the same decision.
fn encoding_defaults_all_trivial(encoding_fields: &[EncodingField]) -> bool {
    encoding_fields
        .iter()
        .all(|enc| matches!(enc.default_expr, "None" | "false"))
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
            let mut config = SerializeConfig::for_field(field_expr, field).expr_is_ref(expr_is_ref);
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
            let mut config = SerializeConfig::for_field(&field_expr, field);
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
        // Under preserve-encodings a fixed value with no encoding variation (bool / null) still has
        // NO binding target — `encoding_var_names_str` is empty — so a `let {} = ` LHS would be
        // invalid Rust (`let  = ...`). Gate the preserve branch on a non-empty binding and let those
        // fixed values fall through to the verify-only branch (same as non-preserve fixed values).
        let preserve_binding = cli
            .preserve_encodings
            .then(|| encoding_var_names_str(types, &field.name, &field.rust_type, cli))
            .filter(|s| !s.is_empty());
        let (before, after) = if let Some(var_names_str) = preserve_binding {
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
                let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
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
                let deser_config = DeserializeConfig::for_field(field, in_embedded, true);
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
                let deser_config = DeserializeConfig::for_field(field, in_embedded, false);
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
                let deser_config = DeserializeConfig::for_field(field, in_embedded, false);
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
    let (mut native_struct, mut native_impl) =
        create_base_rust_struct(types, name, config.custom_json, cli);
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
                encoding_var_macros(types.key_demand(name), config.custom_json, cli)
            ),
            format!("Option<{encoding_name}>"),
        );
        native_new_block.line("encodings: None,");

        let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
        let mut encoding_aliases: Vec<(String, String)> = Vec::new();
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
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    name,
                    &field_enc.field_name,
                    &field_enc.type_name,
                );
            }
            if record.rep == Representation::Map {
                let key_enc = key_encoding_field(&field.name, field.key.as_ref().unwrap());
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    name,
                    &key_enc.field_name,
                    &key_enc.type_name,
                );
            }
        }

        let enc_scope = gen_scope.cbor_encodings(types, name);
        for (alias, target) in encoding_aliases {
            enc_scope.push_type_alias(TypeAlias::new(&alias, &target).vis("pub").clone());
        }
        enc_scope.push_struct(encoding_struct);

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
                                // empty binding == a fixed value with no encoding var (bool / null):
                                // there is no `let X =` LHS, so the annotated deserialize is a bare
                                // statement and needs its own terminating `;` (the non-empty branch
                                // gets it from `?;`). Emitting just `?` drops the semicolon and the
                                // next line (`{field}_present = true;`) fails to parse.
                                ("".to_owned(), "?;")
                            } else {
                                (format!("let {var_names_str} = "), "?;")
                            };
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                            let deser_config =
                                DeserializeConfig::for_field(field, in_embedded, field.optional);
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
                                true,
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
        // the scaffolding lands inside the annotate closure only for non-embedded records (the
        // embedded/plain-group scaffolding stays in deserialize(), outside any closure)
        let scaffolding_annotated = cli.annotate_fields && !types.is_plain_group(name);
        let (mut deser_impl, mut deser_embedded_impl) = create_deserialize_impls(
            name,
            Some(record.rep),
            tag,
            Some(record.cbor_len_info(types)),
            types.is_plain_group(name),
            len_encoding_var,
            &mut deser_scaffolding,
            scaffolding_annotated,
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

        if deser_embedded_impl.is_none() {
            // Non-embedded records: the container header + length reads (tag / map / array +
            // read_elems / finish, built into `deser_scaffolding` by create_deserialize_impls)
            // must sit INSIDE the annotate closure so wrong-major-type and wrong-length errors
            // carry the type name exactly like field-level errors already do. Prepend the
            // scaffolding ahead of the field-read code so the whole body is annotated as one unit.
            // (The embedded/plain-group case annotates its scaffolding differently:
            // create_deserialize_impls wraps the pre-delegation header reads and the post-delegation
            // final-len check each in their own annotate closure, keeping the delegated
            // deserialize_as_embedded_group() call OUTSIDE any closure — its body is already
            // annotated per-field, so wrapping the delegation would double-annotate field errors
            // ("Type.Type.field").)
            let mut body = std::mem::take(&mut deser_scaffolding);
            body.push_all(std::mem::take(&mut deser_code.content));
            deser_code.content = body;
        }

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
            // Non-embedded: `deser_scaffolding` was merged into `deser_code.content` above (inside
            // the annotate closure), so the whole deserialize() body is just the annotated code.
            let mut deser_f =
                make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
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
                    match &ty.conceptual_type {
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

    fn names_with_macros(
        &self,
        key_demand: Option<DemandSet>,
        custom_json: bool,
        cli: &Cli,
    ) -> Vec<String> {
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
                    format!(
                        "{}{}",
                        encoding_var_macros(key_demand, custom_json, cli),
                        name
                    )
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
        types.key_demand(name),
        true,
        /* cstyle_baseline */ true,
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
                true,
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
    // instead of using create_serialize_impls() and having the length encoded there, we want to make it easier
    // to offer definite length encoding even if we're mixing plain group members and non-plain group members (or mixed length plain ones)
    // by potentially wrapping the choices with the array/map tag in the variant branch when applicable
    add_struct_derives(
        &mut e,
        types.key_demand(name),
        true,
        /* cstyle_baseline */ false,
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
        generate_tag_check(deser_body, name, tag, cli.annotate_fields);
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
            cli.annotate_fields,
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
                    .names_with_macros(types.key_demand(name), config.custom_json, cli)
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
                // Mirror the `annotated` switch (see `generate_tag_check`): when annotate_fields is
                // set, `deser_body` is the body of the `.annotate(name)` closure, so emit the
                // locationless form and let the closure supply the name (the name-carrying form
                // would get the name prepended AGAIN, reading "Name.Name").
                if cli.annotate_fields {
                    deser_type_match.line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
                } else {
                    deser_type_match.line(format!(
                        "_ => Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatched)),"
                    ));
                }
            }
            deser_body.push_block(deser_type_match);
        }
        None => {
            if cli.annotate_fields {
                deser_body.line("Err(DeserializeFailure::NoVariantMatchedWithCauses(errs).into())");
            } else {
                deser_body.line(&format!(
                    "Err(DeserializeError::new(\"{name}\", DeserializeFailure::NoVariantMatchedWithCauses(errs)))"
                ));
            }
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

/// clippy's default `type-complexity-threshold`. A type in a lint-scored position (struct field, fn
/// signature, ...) whose structural score exceeds this trips `clippy::type_complexity`. Type
/// *aliases* are not scored by the lint, so hoisting an over-threshold encoding-struct field type
/// into a `pub type` alias silences it without an `#[allow]` and without changing any emitted bytes
/// or round-trip semantics.
const TYPE_COMPLEXITY_THRESHOLD: u64 = 250;

/// Reproduce clippy's `type_complexity` scoring closely enough to decide, deterministically,
/// whether an emitted encoding field type would trip the lint. clippy walks the type and adds
/// `10 * nest` for every path / tuple / array / slice / reference node, incrementing `nest` by one
/// when descending into that node's children. The emitted encoding types use only paths (`Foo`,
/// `Foo<..>`, `a::b`) and tuples (no refs/slices), so scoring those node kinds suffices.
/// Over-estimating here is harmless (it only mints an extra alias); the clippy gate is the backstop
/// if the real boundary ever shifts.
fn type_complexity_score(ty: &str) -> u64 {
    /// Split `s` on top-level `delim` (bracket depth 0 over `<>` and `()`), trimming each piece.
    fn split_top_level(s: &str, delim: char) -> Vec<&str> {
        let mut depth = 0i32;
        let mut parts = Vec::new();
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => depth -= 1,
                c if c == delim && depth == 0 => {
                    parts.push(s[start..i].trim());
                    start = i + c.len_utf8();
                }
                _ => {}
            }
        }
        parts.push(s[start..].trim());
        parts
    }
    /// True iff every prefix of `s` has non-negative `<>`/`()` depth and the whole is balanced —
    /// i.e. an outermost `(...)` pair actually wraps the entire string.
    fn is_balanced(s: &str) -> bool {
        let mut depth = 0i32;
        for c in s.chars() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => {
                    depth -= 1;
                    if depth < 0 {
                        return false;
                    }
                }
                _ => {}
            }
        }
        depth == 0
    }
    fn score(ty: &str, nest: u64) -> u64 {
        let ty = ty.trim();
        // Parenthesized: a tuple (>=2 top-level elements) is one node whose elements are children;
        // a single `(T)` grouping is just `T` (no HIR node); `()` is a unit.
        if let Some(inner) = ty
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .filter(|inner| is_balanced(inner))
        {
            let parts = split_top_level(inner, ',');
            return if inner.trim().is_empty() {
                1 // unit ()
            } else if parts.len() >= 2 {
                10 * nest + parts.iter().map(|p| score(p, nest + 1)).sum::<u64>()
            } else {
                score(inner, nest) // grouping, not a tuple
            };
        }
        // Path with generics `Ident<..>` / `a::b::Ident<..>`: one node, generic args are children.
        if let (Some(open), Some(close)) = (ty.find('<'), ty.rfind('>')) {
            let args = &ty[open + 1..close];
            return 10 * nest
                + split_top_level(args, ',')
                    .iter()
                    .map(|a| score(a, nest + 1))
                    .sum::<u64>();
        }
        // Plain path node (`u64`, `LenEncoding`, `cbor_event::Sz`, ...).
        10 * nest
    }
    score(ty, 1)
}

/// Add one field to an encoding struct, hoisting an over-`type_complexity` field type into a
/// deterministic `pub type <Owner><FieldCamel> = ..;` alias in the same `cbor_encodings` scope so
/// `clippy::type_complexity` stays quiet without an `#[allow]`. Alias names can't collide with each
/// other: `owner` (the owning encoding struct's base type name) is distinct per struct and
/// `field_name` is distinct within a struct, so identical anonymous shapes in different rules never
/// collide. An alias CAN in principle collide with another rule's encoding-struct name:
/// owner `Foo` + field `bar_encoding` aliases to `FooBarEncoding`, which a rule named `foo-bar`
/// also claims. That needs an over-threshold field AND the exact sibling rule name, and it fails
/// LOUD (E0428 in the generated crate, caught by every compile gate), so it is not disambiguated
/// preemptively.
/// Aliases are collected (not pushed) so the caller can push them into the scope alongside the
/// struct.
fn push_encoding_struct_field(
    encoding_struct: &mut codegen::Struct,
    aliases: &mut Vec<(String, String)>,
    owner: &RustIdent,
    field_name: &str,
    type_name: &str,
) {
    let field_type = if type_complexity_score(type_name) > TYPE_COMPLEXITY_THRESHOLD {
        let alias = format!("{}{}", owner, convert_to_camel_case(field_name));
        aliases.push((alias.clone(), type_name.to_owned()));
        alias
    } else {
        type_name.to_owned()
    };
    encoding_struct.field(format!("pub {field_name}"), field_type);
}

// `annotated` - true iff deser_func is the body of an `.annotate(ident)` error closure: emit
// locationless errors and let the closure supply the name (the per-error annotate/named forms
// would get the name prepended AGAIN by the closure, reading "Name.Name"). When false, each error
// carries the name itself, as no closure will add it.
pub(crate) fn generate_tag_check(
    deser_func: &mut dyn CodeBlock,
    ident: &RustIdent,
    tag: Option<usize>,
    annotated: bool,
) {
    if let Some(tag) = tag {
        if annotated {
            deser_func.line("let tag = raw.tag()?;");
        } else {
            deser_func.line(&format!(
                "let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{ident}\"))?;"
            ));
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        if annotated {
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
        } else {
            tag_check.line(format!("return Err(DeserializeError::new(\"{ident}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        }
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
                    encoding_var_macros(types.key_demand(type_name), true, cli)
                ),
                format!("Option<{encoding_name}>"),
            );
            let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
            let mut encoding_aliases: Vec<(String, String)> = Vec::new();
            for field_enc in &enc_fields {
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    type_name,
                    &field_enc.field_name,
                    &field_enc.type_name,
                );
            }
            let enc_scope = gen_scope.cbor_encodings(types, type_name);
            for (alias, target) in encoding_aliases {
                enc_scope.push_type_alias(TypeAlias::new(&alias, &target).vis("pub").clone());
            }
            enc_scope.push_struct(encoding_struct);
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
    // The whole deserialize() body is accumulated here so it can be wrapped in one
    // `.annotate(type_name)` error closure when `cli.annotate_fields` (giving the container/
    // primitive reads a `failed in <T>` location exactly as field-level errors already get). When
    // annotate_fields is off no closure is emitted and the content is pushed verbatim, byte-identical
    // to before. `new()` and the `TryFrom`/`From` paths NEVER go through this closure, so any error
    // they emit must keep the name-carrying form (see `build_check`'s `annotated=false` arm).
    let mut deser_body = BlocksOrLines::default();
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
            .add_to(&mut deser_body);

        // Build the range-check `if` condition and its `DeserializeFailure::..` payload once, then
        // materialize the check per-consumer (see `build_check`): the deserialize() copy is
        // locationless (`.into()`) when it lands inside the annotate closure, while the `new()` copy
        // always carries the name (`DeserializeError::new`) since no closure ever wraps it.
        let (cond, failure_expr) = if let Some(window) = float_min_max {
            // NaN-safe float window: accept-form negation, value compared as f64 so the authored
            // decimal literal is exact. Reports the ORIGINAL window with its per-side exclusivity.
            let cast_f64 = matches!(
                &field_type.conceptual_type,
                ConceptualRustType::Primitive(Primitive::F32)
            );
            let cond = format!("if !({})", float_accept_cond(&window, "inner", cast_f64));
            let opt = |side: Option<(f64, bool)>| match side {
                Some((v, _)) => format!("Some({})", float_literal(v)),
                None => "None".to_owned(),
            };
            let incl = |side: Option<(f64, bool)>| match side {
                Some((_, exclusive)) => (!exclusive).to_string(),
                None => "false".to_owned(),
            };
            let failure_expr = format!(
                "DeserializeFailure::RangeCheckFloat{{ found: inner as f64, min: {}, max: {}, min_inclusive: {}, max_inclusive: {} }}",
                opt(window.0),
                opt(window.1),
                incl(window.0),
                incl(window.1)
            );
            (cond, failure_expr)
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
            let cond = match (min, max) {
                (Some(min), Some(max)) => {
                    if min == max {
                        format!("if {against} != {min}")
                    } else if min > max {
                        // `.ne N` is encoded as Range(N+1, N-1): an exclusion, not a window
                        format!("if {against} == {}", min - 1)
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
                            format!("if {against} > {max}")
                        } else {
                            format!("if {against} < {min} || {against} > {max}")
                        }
                    }
                }
                (Some(min), None) => format!("if {against} < {min}"),
                (None, Some(max)) => format!("if {against} > {max}"),
                (None, None) => panic!(
                    "How did we end up with a range requirement of (None, None)? Entire thing should've been None then"
                ),
            };
            let failure_expr = format!(
                "DeserializeFailure::RangeCheck{{ found: {} as i128, min: {}, max: {} }}",
                against,
                match min {
                    Some(min) => format!("Some({min})"),
                    None => String::from("None"),
                },
                match max {
                    Some(max) => format!("Some({max})"),
                    None => String::from("None"),
                }
            );
            (cond, failure_expr)
        };
        let build_check = |annotated: bool| {
            let mut check = Block::new(cond.clone());
            if annotated {
                check.line(format!("return Err({failure_expr}.into());"));
            } else {
                check.line(format!(
                    "return Err(DeserializeError::new(\"{type_name}\", {failure_expr}));"
                ));
            }
            check
        };
        deser_body.push_block(build_check(cli.annotate_fields));
        new_func
            .ret("Result<Self, DeserializeError>")
            .push_block(build_check(false));
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
            deser_body.push_block(deser_ctor);

            let mut ctor_block = Block::new("Ok(Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            ctor_block.after(")");
            new_func.push_block(ctor_block);
        } else {
            deser_body.line("Ok(Self(inner))");
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
                .add_to(&mut deser_body);

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
            deser_body.push_block(deser_ctor);

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
                .add_to(&mut deser_body);
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
    // Flush the accumulated deserialize() body: wrap it in a single `.annotate(type_name)` error
    // closure when annotate_fields is on (giving container/primitive reads a `failed in <T>`
    // location; the in-body range check is already the locationless form so the closure names it
    // exactly once), else push it verbatim (byte-identical to the pre-annotation output).
    if cli.annotate_fields {
        let mut error_annotator = make_err_annotate_block(type_name.as_ref(), "", "");
        error_annotator.push_all(deser_body);
        deser_func.push_block(error_annotator);
    } else {
        deser_func.push_all(deser_body);
    }
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
/// The SINGLE demand→traits mapping (pinned semantics 6), used by every derive/ignore emission site so
/// the bare path stays byte-identical. Resolves a `DemandSet` to the comparison/hash traits it demands,
/// in the canonical emission order `Eq, PartialEq, Ord, PartialOrd, Hash`:
/// - `bare` → today's mode-dependent internal bundle (`Eq/PartialEq/Ord/PartialOrd`, plus `Hash` under
///   `--preserve-encodings`);
/// - `hash` → `Hash, Eq, PartialEq` (mode-independent);
/// - `ord` → `Ord, PartialOrd, Eq, PartialEq` (mode-independent).
///
/// `for_ignore` drops `Eq` (the `derivative` field ignore-list has no `Eq` attribute — `Eq` is a
/// fieldless marker), reproducing the old `key_derives(for_ignore=true)` set exactly.
fn key_trait_list(demand: DemandSet, for_ignore: bool, cli: &Cli) -> Vec<&'static str> {
    let mut eq = false;
    let mut ord = false;
    let mut hash = false;
    if demand.bare {
        eq = true;
        ord = true;
        if cli.preserve_encodings {
            hash = true;
        }
    }
    if demand.hash {
        hash = true;
        eq = true;
    }
    if demand.ord {
        ord = true;
        eq = true;
    }
    let mut out = Vec::new();
    if eq && !for_ignore {
        out.push("Eq");
    }
    if eq {
        out.push("PartialEq");
    }
    if ord {
        out.push("Ord");
        out.push("PartialOrd");
    }
    if hash {
        out.push("Hash");
    }
    out
}

/// The `where`-clause trait bound a key demand needs, as used by the `borrowed_key_types.rs`
/// `_assert_key_traits*` self-check carriers. Drops `PartialEq` (a supertrait of `Eq`, redundant as a
/// bound) and maps `Hash` to its full path, so the `bare` bound reproduces the historical
/// `Eq + Ord + PartialOrd + core::hash::Hash` (byte-identical) form.
fn key_bound(demand: DemandSet, cli: &Cli) -> String {
    key_trait_list(demand, false, cli)
        .iter()
        .filter(|t| **t != "PartialEq")
        .map(|t| if *t == "Hash" { "core::hash::Hash" } else { *t })
        .collect::<Vec<_>>()
        .join(" + ")
}

/// The sidecar flavor token for a demand (`bare`/`hash`/`ord`, space-joined when several bits are set).
/// This is the optional 3rd `BORROWED_KEY_TYPES` column; `parse_key_flavor` is its inverse.
fn key_flavor_token(demand: DemandSet) -> String {
    let mut parts = Vec::new();
    if demand.bare {
        parts.push("bare");
    }
    if demand.hash {
        parts.push("hash");
    }
    if demand.ord {
        parts.push("ord");
    }
    parts.join(" ")
}

/// The directly-tagged demand roots that warrant an emitted compile-time assertion: every
/// `@used_as_key` root — flavored or bare — whose type is a generated (non-extern), export-scope
/// struct in THIS crate, so it can be named `crate::generated::…` and its supply proven by the
/// compiler. Bare roots are included as a diagnosis breadcrumb: their derive demand propagates
/// transitively, so a missing-trait failure surfaces at a contained struct with nothing connecting
/// it to the tag — this file is the in-crate record of which tag caused which demand. (Internal
/// auto-detected map keys still emit nothing: their containers' own bounds enforce them in-crate.)
/// Sorted by ident (`BTreeMap` iteration) for deterministic placement.
fn assertion_roots(types: &IntermediateTypes) -> Vec<(RustIdent, DemandSet)> {
    types
        .key_demand_roots()
        .iter()
        .filter(|(ident, _)| {
            types.scope(ident).export()
                && types.rust_struct(ident).is_some_and(|rs| {
                    !matches!(
                        rs.variant(),
                        RustStructType::Extern | RustStructType::RawBytesType
                    )
                })
        })
        .map(|(ident, d)| (ident.clone(), *d))
        .collect()
}

fn add_struct_derives<T: DataType>(
    data_type: &mut T,
    key_demand: Option<DemandSet>,
    is_enum: bool,
    cstyle_baseline: bool,
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
    if let Some(mut demand) = key_demand {
        // A c-style enum's always-on baseline is `Eq/PartialEq/Ord/PartialOrd` (emitted directly when
        // it is NOT a key). When it IS a key, that baseline must be UNIONED with the tag's flavor so a
        // tagged enum never derives LESS than an untagged one (pinned semantics 5). `ord` supplies the
        // whole `Ord/PartialOrd/Eq/PartialEq` family, so forcing it reconstitutes the baseline.
        if cstyle_baseline {
            demand.ord = true;
        }
        let traits = key_trait_list(demand, false, cli);
        if cli.preserve_encodings {
            // there's no way to do non-derive() proc macros in the codegen
            // cate so we must sadly use a newline like this. codegen manages indentation
            data_type.derive(&format!(
                "derivative::Derivative)]\n#[derivative({}",
                traits
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
            for key_derive in traits {
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

        let mut wasm_new_uint = codegen::Function::new("new_uint");
        wasm_new_uint
            .ret("Self")
            .vis("pub")
            .arg("value", "u64")
            .line(format!(
                "Self({}::Int::new_uint(value))",
                cli.lib_name_code()
            ));

        let mut wasm_new_nint = codegen::Function::new("new_nint");
        wasm_new_nint
            .ret("Self")
            .vis("pub")
            .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
            .arg("value", "u64")
            .line(format!("Self({}::Int::new_nint(value))", cli.lib_name_code()));

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
            .push_fn(wasm_new_uint)
            .push_fn(wasm_new_nint)
            .push_fn(to_str)
            .push_fn(from_str);
        wrapper.push(gen_scope, types);

        // Rust exposes `IntError` as the `FromStr` associated error. The wasm constructor maps that
        // to `JsError`, so keep source-level parity without claiming wasm exports a data-bearing enum.
        gen_scope
            .wasm(types, &ident)
            .push_type_alias(TypeAlias::new("IntError", "JsError").vis("pub").clone());
    }

    let mut native_struct = codegen::Enum::new("Int");
    native_struct.vis("pub");
    let mut uint = codegen::Variant::new("Uint");
    let mut nint = codegen::Variant::new("Nint");
    if cli.preserve_encodings {
        uint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
        nint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
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
        types.key_demand(&ident),
        /* is_enum */ true,
        /* cstyle_baseline */ false,
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
        try_from_else.line(
            "u64::try_from((x + 1).unsigned_abs()).map(|x| Self::Nint{ value: x, encoding: None })",
        );
    } else {
        try_from_if.line("u64::try_from(x).map(Self::Uint)");
        try_from_else.line("u64::try_from((x + 1).unsigned_abs()).map(Self::Nint)");
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

/// Runs rustfmt on the string.
///
/// Import pruning is NOT done here: the usage-derived prune (`import_prune.rs`) needs to see a
/// file's descendant modules (a parent module's import can be consumed by a child via
/// `use super::*;`, so per-file "ident absent from this file" does NOT imply unused), so it runs
/// once over the full file map in `generated_files` — see `import_prune::prune_generated_files`.
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
