use super::*;

#[derive(Debug, Clone)]
pub(super) struct SerializeConfig<'a> {
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
    /// The `@custom_encodings` declaration written beside the pair in `custom_serialize`, when it has
    /// one: the codec-visible encoding variables of ITS wire, which then decide the trailing argument
    /// list instead of the replaced type's inferred demand. Lifted at exactly the two places the pair
    /// itself is lifted (`for_field`, and the `Alias` arm), so it can never travel without its pair.
    /// `None` = no declaration; inference, unchanged.
    custom_encodings: Option<Vec<EncodingKind>>,
    /// number of tag levels already crossed on this member name (0 at the field root). Drives the
    /// `tag`/`tag2`/… encoding-var infix so stacked tags read their own level's var. See
    /// `tag_encoding_infix`.
    ///
    /// INVARIANT: this must stay in lockstep with `encoding_fields_impl`'s own `tag_depth`, which
    /// MINTS the member names this reads. So a child config must reset it to 0 at EXACTLY the
    /// recursion boundaries where `encoding_fields_impl` resets to 0 — the array element, the map
    /// key, the map value, and the CStyleEnum variant hand-off (which routes through the
    /// `encoding_fields` wrapper, i.e. reset) — and must THREAD it (unchanged, or +1 at a tag) where
    /// `encoding_fields_impl` threads: the Fixed conversions, Alias, Optional, the CBORBytes child,
    /// and the Tagged/OptionallyTagged child (at level+1). A child built by cloning the parent config
    /// across a name boundary WITHOUT the reset leaks the parent's depth, so serialize reads
    /// `{elem}_tag2_encoding` while the struct only minted `{elem}_tag_encoding` (E0425).
    tag_depth: usize,
}

impl<'a> SerializeConfig<'a> {
    pub(super) fn new<S: Into<String>, T: Into<String>>(expr: S, var_name: T) -> Self {
        Self {
            expr: expr.into(),
            expr_is_ref: false,
            var_name: var_name.into(),
            is_end: false,
            encoding_var_is_ref: false,
            encoding_var_in_option_struct: None,
            serializer_name_overload: None,
            custom_serialize: None,
            custom_encodings: None,
            tag_depth: 0,
        }
    }

    /// THE constructor for serializing a record field. Use this at every record-field serialize site
    /// rather than `new(..)` + hand-chained setter: it carries the field's `@custom_serialize`
    /// directive automatically. Forgetting to re-carry a custom directive when adding a new call site
    /// is a recurring bug class here, so this owns that carry in one place.
    pub(super) fn for_field<S: Into<String>>(expr: S, field: &RustField) -> Self {
        let mut config = Self::new(expr, &field.name);
        if let Some(custom_serialize) = &field.rule_metadata.custom_serialize {
            config = config.custom_serialize(custom_serialize.clone());
            // The field's own `@custom_encodings`, and only its own: a declaration describes the wire
            // of the codec written beside it, so a field-level pair never inherits the declaration of
            // an alias it shadows (that one describes a different codec's wire).
            config.custom_encodings = field.rule_metadata.custom_encodings.clone();
        }
        config
    }

    pub(super) fn expr<S: Into<String>>(mut self, expr: S) -> Self {
        self.expr = expr.into();
        self
    }

    pub(super) fn var_name<S: Into<String>>(mut self, var_name: S) -> Self {
        self.var_name = var_name.into();
        self
    }

    pub(super) fn expr_is_ref(mut self, is_ref: bool) -> Self {
        self.expr_is_ref = is_ref;
        self
    }

    #[allow(clippy::wrong_self_convention)]
    pub(super) fn is_end(mut self, is_end: bool) -> Self {
        self.is_end = is_end;
        self
    }

    pub(super) fn encoding_var_is_ref(mut self, is_ref: bool) -> Self {
        self.encoding_var_is_ref = is_ref;
        self
    }

    pub(super) fn encoding_var_in_option_struct<S: Into<String>>(
        mut self,
        option_struct: S,
    ) -> Self {
        self.encoding_var_in_option_struct = Some(option_struct.into());
        self
    }

    pub(super) fn encoding_var_no_option_struct(mut self) -> Self {
        self.encoding_var_in_option_struct = None;
        self
    }

    pub(super) fn serializer_name_overload(mut self, overload: (&'a str, bool)) -> Self {
        self.serializer_name_overload = Some(overload);
        self
    }

    pub(super) fn custom_serialize(mut self, func: String) -> Self {
        self.custom_serialize = Some(func);
        self
    }

    /// Lift a pair's `@custom_encodings` declaration. Always chained onto the same `custom_serialize`
    /// lift so the two cannot separate.
    pub(super) fn custom_encodings(mut self, kinds: Option<Vec<EncodingKind>>) -> Self {
        self.custom_encodings = kinds;
        self
    }

    pub(super) fn tag_depth(mut self, tag_depth: usize) -> Self {
        self.tag_depth = tag_depth;
        self
    }

    pub(super) fn encoding_var(&self, child: Option<&str>, is_copy: bool) -> String {
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
    pub(super) fn container_encoding_lookup(
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

#[derive(Debug)]
pub(super) enum SerializingRustType<'a> {
    EncodingOperation(&'a CBOREncodingOperation, Box<SerializingRustType<'a>>),
    Root(&'a ConceptualRustType, Cow<'a, RustTypeSerializeConfig>),
}

/// The serialize config to recurse a NOMINAL reference to a collection-typedef rule
/// (`RustStructType::Table` / `Array`) into its structural conceptual type with.
///
/// A resolved-alias reference carries the rule's per-rule policy on its own `RustType` config —
/// `@duplicates` (which selects the positional pair-map path) and the occurrence bounds (which
/// select the `NonEmpty*` twin). A nominal `Rust(ident)` reference carries neither, because the
/// policy lives on the referenced STRUCT. Read it back off the struct so both reference paths emit
/// the same code for the same rule. An explicit config on the reference wins (nothing sets one
/// today; this only keeps the merge from silently outranking a caller).
pub(super) fn nominal_collection_cfg<'a>(
    types: &IntermediateTypes,
    ident: &RustIdent,
    incoming: &Cow<'a, RustTypeSerializeConfig>,
) -> Cow<'a, RustTypeSerializeConfig> {
    let rust_struct = types.rust_struct(ident).unwrap();
    let bounds = match rust_struct.variant() {
        RustStructType::Table { bounds, .. } | RustStructType::Array { bounds, .. } => *bounds,
        _ => None,
    };
    let duplicates = rust_struct.config().duplicates;
    if incoming.duplicates == duplicates && incoming.bounds == bounds {
        return incoming.clone();
    }
    let mut cfg = incoming.as_ref().clone();
    if cfg.duplicates.is_none() {
        cfg.duplicates = duplicates;
    }
    if cfg.bounds.is_none() {
        cfg.bounds = bounds;
    }
    Cow::Owned(cfg)
}

pub(super) trait EncodingVarIsCopy {
    fn encoding_var_is_copy(&self, types: &IntermediateTypes) -> bool;
}

impl<'a> EncodingVarIsCopy for SerializingRustType<'a> {
    fn encoding_var_is_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::EncodingOperation(CBOREncodingOperation::CBORBytes, _) => false,
            Self::EncodingOperation(CBOREncodingOperation::Tagged(_), _) => true,
            // TagPresenceEncoding is Copy
            Self::EncodingOperation(CBOREncodingOperation::OptionallyTagged(_), _) => true,
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
                | Primitive::Float
                | Primitive::F16
                | Primitive::F32
                | Primitive::F64
                | Primitive::F16To32
                | Primitive::F32To64
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
                    // technically no encoding var.
                    //
                    // EXCEPT `Table`/`Array`, which reach this arm from a NOMINAL reference and DO
                    // contribute one: `encoding_fields_impl` pushes the collection's encodings OUT
                    // to the referring member, since a collection typedef has no struct to hold
                    // them. `true` is still the right answer for them, and not by luck — that var
                    // is the collection's LENGTH encoding, which the structural `Map`/`Array` arms
                    // above also answer `true` for (key/value/element encodings are separate vars).
                    // Both reference paths therefore agree, which is the property that matters.
                    // Spelled out because the blanket "no encoding var" reasoning stopped being
                    // true for these two variants, and a reader auditing this dispatch class would
                    // otherwise have to re-derive why the catch-all is still correct.
                    true
                }
            },
            // `AnyCbor` is self-carried: it contributes NO owner encoding var (its encodings live
            // inside the value), so like the Rust `_ => true` "technically no encoding var" case.
            Self::Any => true,
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

pub(super) fn start_len(
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

pub(super) fn end_len(
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
        // Spelled through the parameter, never as a bare `serializer`: this helper is called with
        // the caller's `serializer_pass`, which is the `.cbor`-payload inner buffer under a
        // `serializer_name_overload`. Every overload site sets `is_end(false)` today, so this branch
        // is reachable only with the default name — but a leaf that hardcodes the default is the
        // exact failure class `snapshot_tests::emitter_overload_no_bare_default_tokens` lints for,
        // and it is that lint's arming run that found this one.
        body.line(&format!("Ok({serializer_use})"));
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn write_using_sz(
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

/// The float twin of `write_using_sz`. A float head's WIDTH is data under preserve exactly as an
/// integer argument's is, but the runtime side is a free function (`write_float`) rather than
/// `write_using_sz`'s `{serializer}.{func}_sz(expr, fit_sz(..))` chain, because for a float the width
/// and the written VALUE are coupled: a canonical write drops a NaN payload (RFC 8949 §4.2.2) and the
/// width must then come from the NORMALIZED value. Splitting them into a `fit_*`-style width helper
/// plus an inline value expression is exactly how the two silently disagree — the first
/// implementation did, writing the canonical NaN at `Sz::Eight`.
///
/// Takes `serializer_pass`, NOT `serializer_use`: a free function needs the serializer as an
/// ARGUMENT, and inside a `bytes .cbor T` wrapper the serializer is a local `Serializer` value whose
/// pass-form is `&mut <name>` — a method receiver auto-refs, a function argument does not (E0308).
pub(super) fn write_float(
    body: &mut dyn CodeBlock,
    serializer_pass: &str,
    value_expr: &str,
    line_ender: &str,
    encoding_var: &str,
    cli: &Cli,
) {
    body.line(&format!(
        "write_float({}, {}, {}{}){}",
        serializer_pass,
        value_expr,
        encoding_var,
        canonical_param(cli),
        line_ender
    ));
}

#[allow(clippy::too_many_arguments)]
pub(super) fn write_string_sz(
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
pub(super) fn create_serialize_impls(
    ident: &RustIdent,
    rep: Option<Representation>,
    tag: Option<usize>,
    definite_len: &str,
    use_this_encoding: Option<&str>,
    generate_serialize_embedded: bool,
    cli: &Cli,
) -> (codegen::Function, codegen::Impl, Option<codegen::Impl>) {
    if generate_serialize_embedded {
        // This is not necessarily a problem but we should investigate this case to ensure we're not calling
        // (de)serialize_as_embedded without (de)serializing the tag
        assert_eq!(tag, None);
    }
    let name = &ident.to_string();
    let ser_impl = make_serialization_impl(name, cli);
    let mut ser_func = make_serialization_function("serialize", cli);
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

pub(super) fn make_serialization_function(name: &str, cli: &Cli) -> codegen::Function {
    let mut f = codegen::Function::new(name);
    f.generic("'se")
        .ret("cbor_event::Result<&'se mut Serializer>")
        .arg_ref_self()
        .arg("serializer", "&'se mut Serializer");
    if cli.preserve_encodings && cli.canonical_form {
        f.arg("force_canonical", "bool");
    }
    f
}

pub(super) fn make_serialization_impl(name: &str, cli: &Cli) -> codegen::Impl {
    let mut ser_impl = codegen::Impl::new(name);
    if cli.preserve_encodings && cli.canonical_form {
        ser_impl.impl_trait("Serialize");
    } else {
        ser_impl.impl_trait("cbor_event::se::Serialize");
    }
    ser_impl
}

impl GenerationScope {
    /// Write code for serializing {serializing_rust_type} directly into {body}
    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn generate_serialize(
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
                // The pair's OWN declaration wins over the replaced type's inferred demand — that is
                // the whole point of `@custom_encodings` (a self-carrying replaced type infers
                // nothing, so the custom framing had nowhere to go). Undeclared: today's inference,
                // blind to declarations below since THIS codec now owns the wire from here down.
                let codec_encodings = match &config.custom_encodings {
                    Some(kinds) => declared_encoding_fields(&config.var_name, kinds),
                    None => encoding_fields_impl(
                        types,
                        &config.var_name,
                        serializing_rust_type,
                        cli,
                        0,
                        AliasDeclarations::Blind,
                    ),
                };
                Cow::Owned(
                    codec_encodings
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
            // `serializer_pass`, NOT `serializer_use`: a custom serialize target is a FREE FUNCTION
            // taking the serializer as an ARGUMENT, so where the serializer in scope is a local
            // `Serializer` value (the canonical key-sort scratch `buf`, the open struct-map canonical
            // merge's `buf`, a `bytes .cbor T` wrapper's inner) the pass-form is `&mut <name>` — a
            // method receiver auto-refs, a function argument does not (E0308). Same class as
            // `write_float`'s doc comment above; for the DEFAULT `serializer` (already `&mut`) the two
            // forms coincide, so this changes emitted bytes only at local-serializer sites.
            body.line(&format!(
                "{}({}, {}{}{}){}",
                custom_serialize,
                serializer_pass,
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
                    // level (tag_depth + 1) counted outside-in; the infix keeps the member name in
                    // lockstep with `encoding_fields_impl`, and the child recurses one level deeper.
                    let tag_level = config.tag_depth + 1;
                    let tag_infix = tag_encoding_infix(tag_level);
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
                            config.encoding_var(Some(&tag_infix), encoding_var_is_copy)
                        ),
                        cli,
                    );
                    self.generate_serialize(types, *child, body, config.tag_depth(tag_level), cli);
                }
                SerializingRustType::EncodingOperation(
                    CBOREncodingOperation::OptionallyTagged(tag),
                    child,
                ) => {
                    // level (tag_depth + 1) counted outside-in; the infix keeps the member name in
                    // lockstep with `encoding_fields_impl`, and the child recurses one level deeper.
                    let tag_level = config.tag_depth + 1;
                    let tag_infix = tag_encoding_infix(tag_level);
                    let expr = format!("{tag}u64");
                    if cli.preserve_encodings {
                        // CANONICAL POLICY (decided): force_canonical normalizes the tag's SIZE
                        // (via `fit_sz` below) but NEVER its PRESENCE. Which arm was written is part
                        // of what the spec author encoded and other implementations validate
                        // structurally; canonicality governs encoding minimality only. So a value
                        // read untagged re-serializes untagged even under --canonical-form.
                        let enc_expr = format!(
                            "{}{}",
                            encoding_deref,
                            config.encoding_var(Some(&tag_infix), encoding_var_is_copy)
                        );
                        let mut tag_block = Block::new(format!(
                            "if let TagPresenceEncoding::Tagged(tag_sz) = {enc_expr}"
                        ));
                        write_using_sz(
                            &mut tag_block,
                            "write_tag",
                            serializer_use,
                            &expr,
                            &expr,
                            "?;",
                            "tag_sz",
                            cli,
                        );
                        body.push_block(tag_block);
                    } else {
                        // No encoding var to consult: default new values to tagged (matches the
                        // first/tagged arm and current-era ledger emission).
                        write_using_sz(
                            body,
                            "write_tag",
                            serializer_use,
                            &expr,
                            &expr,
                            "?;",
                            "",
                            cli,
                        );
                    }
                    self.generate_serialize(types, *child, body, config.tag_depth(tag_level), cli);
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
                        if !cli.preserve_encodings && *i <= i64::MIN as i128 {
                            // Nint literals are i128: below i64::MIN they don't fit the plain
                            // write_negative_integer endpoint's i64 argument (upstream keeps the
                            // narrow argument by design — the i128-taking _sz endpoint is the
                            // documented full-range form), and the i64::MIN literal itself stays on
                            // the explicit-Sz spelling pinned by
                            // `i64_min_fixed_value_emits_width_correct_nint`.
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
                        let lit = float_fixed_literal(*f);
                        if cli.preserve_encodings {
                            write_float(
                                body,
                                &serializer_pass,
                                &lit,
                                line_ender,
                                &encoding_var_deref,
                                cli,
                            );
                        } else {
                            // Smallest value-preserving head (RFC 8949 §4.1), like every other
                            // float write. A fixed literal is read back by VALUE comparison at any
                            // head, so the width is free to be the preferred one.
                            body.line(&format!(
                                "write_float({serializer_pass}, {lit}){line_ender}"
                            ));
                        }
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
                        p @ (Primitive::Float
                        | Primitive::F16
                        | Primitive::F32
                        | Primitive::F64
                        | Primitive::F16To32
                        | Primitive::F32To64) => {
                            // The CBOR float domain is f64, so an f32-CARRIED class (`float16`,
                            // `float32`, `float16-32`) widens here. Through the crate's exact
                            // widening, never `as`/`From`: those may quiet a signaling NaN or drop
                            // its payload, and LLVM const-folds the conversion to a canonical quiet
                            // NaN, so `as` can differ between the const-evaluated and runtime paths
                            // of one binary. A float round-trips byte-exactly, payload included.
                            let value = if p.float_carrier_is_f32() {
                                Cow::Owned(format!(
                                    "cbor_event::se::f32_to_f64_exact({expr_deref})"
                                ))
                            } else {
                                Cow::Borrowed(expr_deref.as_str())
                            };
                            // Every class writes the smallest head that preserves the value (RFC
                            // 8949 §4.1), uniformly in both profiles — the same rule the integer
                            // writes follow. For a MEMBER of a constrained class that head IS its
                            // declared width, because membership means the value's shortest lossless
                            // form lands in the class's window; a non-member fails loudly inside the
                            // helper rather than being written at a head the class admits.
                            //
                            // Width-unconstrained `float` admits every value, so it needs no window
                            // and no membership check — only the smallest-head rule.
                            let class_window = (*p != Primitive::Float).then(|| {
                                let (min, max) = p.float_class_window().unwrap();
                                format!("cbor_event::Sz::{min}, cbor_event::Sz::{max}")
                            });
                            match (cli.preserve_encodings, class_window) {
                                (true, None) => write_float(
                                    body,
                                    &serializer_pass,
                                    &value,
                                    line_ender,
                                    &encoding_var_deref,
                                    cli,
                                ),
                                (true, Some(class_window)) => {
                                    body.line(&format!(
                                        "write_float_width({serializer_pass}, {value}, {encoding_var_deref}, {class_window}{}){line_ender}",
                                        canonical_param(cli)
                                    ));
                                }
                                (false, None) => {
                                    body.line(&format!(
                                        "write_float({serializer_pass}, {value}){line_ender}"
                                    ));
                                }
                                (false, Some(class_window)) => {
                                    body.line(&format!(
                                        "write_float_width({serializer_pass}, {value}, {class_window}){line_ender}"
                                    ));
                                }
                            }
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
                            // only the _sz variants support i128, the plain endpoint takes i64
                            // (and negates internally in i128, so i64::MIN needs no special-casing)
                            let expr = if cli.preserve_encodings {
                                format!("{expr_deref} as i128")
                            } else {
                                format!("{expr_deref} as i64")
                            };
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
                                // N64 covers the full CBOR nint range down to -2^64, whose bottom
                                // half doesn't fit the plain write_negative_integer endpoint's i64
                                // argument — only the i128 _sz endpoint reaches it. Sz::canonical
                                // keeps the bytes identical to the plain endpoint's derived width.
                                body.line(&format!("{serializer_use}.write_negative_integer_sz(-({expr_deref} as i128 + 1), cbor_event::Sz::canonical({expr_deref})){line_ender}"));
                            }
                        }
                    }
                }
                // `any` serializes via `AnyCbor`'s own `Serialize` impl (self-carried encodings), the
                // same shape as a plain Rust struct reference — mirror the `Rust(_)` fallthrough
                // (`.serialize(serializer[, force_canonical])`) minus owner-encoding threading.
                SerializingRustType::Root(ConceptualRustType::Any, _cfg) => {
                    body.line(&format!(
                        "{}.serialize({}{}){}",
                        config.expr,
                        serializer_pass,
                        canonical_param(cli),
                        line_ender
                    ));
                }
                SerializingRustType::Root(ConceptualRustType::Rust(t), type_cfg) => {
                    // A named record with a whole-record custom pair owns its complete CBOR item.
                    // Dispatch before the kind walk so an embed site calls the same free writer as
                    // the record's thin Serialize impl; in particular, do not route a plain group
                    // through SerializeEmbeddedGroup or fall back to the ordinary record fields.
                    if matches!(
                        types.rust_struct(t).unwrap().variant(),
                        RustStructType::Record(_)
                    ) && let Some(custom_serialize) =
                        &types.rust_struct(t).unwrap().config().custom_serialize
                    {
                        body.line(&format!(
                            "{}({}, {}{}){}",
                            custom_serialize,
                            serializer_pass,
                            expr_ref,
                            canonical_param(cli),
                            line_ender
                        ));
                        return;
                    }
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
                                    // the CStyleEnum variant hand-off resets tag depth to 0 to match
                                    // `encoding_fields_impl` (which recurses the variant through the
                                    // `encoding_fields` wrapper, i.e. reset).
                                    config.clone().is_end(true).tag_depth(0),
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
                        // A named table/array rule emits NO impls of its own — it is a bare rust
                        // typedef onto a collection (`pub type Mdmap = BTreeMap<..>`), so the
                        // `.serialize()` the fallback below emits names a method the target type
                        // does not have. Recurse into the collection's STRUCTURAL conceptual type
                        // instead: that is the same code the resolved-alias reference path emits,
                        // and it is the only code that exists for these shapes. Reached only from a
                        // NOMINAL reference to such a rule, which parse-order makes possible when a
                        // rule cycle is entered at the collection rule (its referrer is handled
                        // first, so the reference never resolves through the alias table).
                        // The struct's OWN per-rule config carries the policy the reference cannot
                        // (`@duplicates`) — thread it in exactly as the Alias arm keeps its outer
                        // config for the same reason, so a `preserve` table still picks the
                        // positional pair-map path.
                        RustStructType::Table { domain, range, .. } => {
                            let structural = ConceptualRustType::Map(
                                Box::new(domain.clone()),
                                Box::new(range.clone()),
                            );
                            let cfg = nominal_collection_cfg(types, t, &type_cfg);
                            self.generate_serialize(
                                types,
                                SerializingRustType::Root(&structural, cfg),
                                body,
                                config,
                                cli,
                            );
                        }
                        RustStructType::Array { element_type, .. } => {
                            let structural =
                                ConceptualRustType::Array(Box::new(element_type.clone()));
                            let cfg = nominal_collection_cfg(types, t, &type_cfg);
                            self.generate_serialize(
                                types,
                                SerializingRustType::Root(&structural, cfg),
                                body,
                                config,
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
                    // Resolve the element's aliases before classifying it: an alias is transparent,
                    // so `[* kv_alias]` splices exactly as many items per element as `[* kv]` does.
                    // Matching the bare `Rust` ident wrote a header counting ELEMENTS while the loop
                    // below wrote each element's members FLAT — an array whose header disagrees with
                    // its own contents, at exit 0 in a crate that compiles. Pinned by
                    // `alias_to_plain_group_in_array_positions_matches_the_direct_reference`; its
                    // deserialize counterpart is the element-read arm in `generate_deserialize`.
                    let len_expr = match ty.conceptual_type.resolve_alias_shallow() {
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
                        encoding_fields(types, &elem_var_name, ty, false, cli)
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
                        .encoding_var_is_ref(false)
                        // fresh `{name}_elem` name namespace: reset tag depth to 0 to match
                        // `encoding_fields_impl`'s array-element reset (else the element's own tag
                        // reads a depth-inflated var the struct never minted).
                        .tag_depth(0);
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
                SerializingRustType::Root(ConceptualRustType::Map(key, value), cfg) => {
                    // `@duplicates preserve` (the pair-map twin): the encoding sidecar is POSITIONAL
                    // (a `Vec` parallel to the entries), so the serialize loop reads encodings by
                    // INDEX (`.get(i)`) via `.enumerate()`, exactly like the array `_elem_encodings`
                    // path — a keyed lookup would be structurally wrong (two same-key entries share
                    // one map slot). The non-preserve-encodings loop and the value serialize are
                    // shared; only the encoding-lookup key (`i` vs `key`) differs.
                    let preserve_pair_map =
                        cfg.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
                    let enc_lookup_var = if preserve_pair_map { "i" } else { "key" };
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
                            key,
                            false,
                            cli,
                        );
                        let value_enc_fields = encoding_fields(
                            types,
                            &format!("{}_value", config.var_name),
                            value,
                            false,
                            cli,
                        );
                        let mut ser_loop = if cli.canonical_form {
                            // `@duplicates preserve` under canonical: RFC 8949 deterministic encoding
                            // requires unique keys, so duplicate-carrying data has NO canonical form.
                            // The flag is crate-wide, so we do the deterministic best-effort — a STABLE
                            // sort by encoded key bytes (duplicates stay adjacent in first-appearance
                            // order) — rather than a generation-time refusal or a runtime error (which
                            // would make `to_canonical_cbor_bytes` partial over every enclosing type).
                            // Canonicalizing metadata is moot anyway: its consensus hash is over the
                            // original bytes, which non-canonical round-trip preserves. The positional
                            // encoding sidecar means the index `i` must ride through the sorted tuple so
                            // the value lookup stays aligned after the sort.
                            let map_head = if preserve_pair_map {
                                format!(
                                    "let mut key_order = {}.iter().enumerate().map(|(i, (k, v))|",
                                    config.expr
                                )
                            } else {
                                format!("let mut key_order = {}.iter().map(|(k, v)|", config.expr)
                            };
                            let mut key_order = Block::new(map_head);
                            key_order.line("let mut buf = cbor_event::se::Serializer::new_vec();");
                            if !key_enc_fields.is_empty() {
                                key_order.line(config.container_encoding_lookup(
                                    "key",
                                    &key_enc_fields,
                                    if preserve_pair_map { "i" } else { "k" },
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
                            if preserve_pair_map {
                                key_order.line("Ok((buf.finalize(), i, k, v))").after(
                                    ").collect::<Result<Vec<(Vec<u8>, usize, &_, &_)>, cbor_event::Error>>()?;",
                                );
                            } else {
                                key_order.line("Ok((buf.finalize(), k, v))").after(
                                    ").collect::<Result<Vec<(Vec<u8>, &_, &_)>, cbor_event::Error>>()?;",
                                );
                            }
                            body.push_block(key_order);
                            let mut key_order_if = Block::new("if force_canonical");
                            // `sort_by` is a STABLE sort, so equal-keyed (duplicate) entries keep their
                            // first-appearance order — the property the preserve tuple carries `i` for.
                            // The length-first-then-bytewise comparison is the ONE shared runtime helper
                            // (`cbor_canonical_key_cmp`, static preserve runtime), so this sort agrees
                            // by construction with `AnyCbor`'s own canonical map sort and generated open
                            // struct-maps' runtime key merge.
                            let sort_call = if preserve_pair_map {
                                "key_order.sort_by(|(lhs_bytes, _, _, _), (rhs_bytes, _, _, _)| cbor_canonical_key_cmp(lhs_bytes, rhs_bytes));"
                            } else {
                                "key_order.sort_by(|(lhs_bytes, _, _), (rhs_bytes, _, _)| cbor_canonical_key_cmp(lhs_bytes, rhs_bytes));"
                            };
                            key_order_if.line(sort_call);
                            body.push_block(key_order_if);
                            let key_loop_var = if value_enc_fields.is_empty() {
                                "_key"
                            } else {
                                "key"
                            };
                            let mut ser_loop = if preserve_pair_map {
                                // `i` is the positional index into the value encoding sidecar; the key
                                // value is not re-serialized (its bytes were written above).
                                let idx_var = if value_enc_fields.is_empty() {
                                    "_i"
                                } else {
                                    "i"
                                };
                                Block::new(format!(
                                    "for (key_bytes, {idx_var}, _key, value) in key_order"
                                ))
                            } else {
                                Block::new(format!(
                                    "for (key_bytes, {key_loop_var}, value) in key_order"
                                ))
                            };
                            ser_loop
                                .line(format!("{serializer_use}.write_raw_bytes(&key_bytes)?;"));
                            ser_loop
                        } else {
                            let mut ser_loop = if preserve_pair_map {
                                // positional: enumerate so the encoding sidecar is read by index.
                                Block::new(format!(
                                    "for (i, (key, value)) in {}.iter().enumerate()",
                                    config.expr
                                ))
                            } else {
                                Block::new(format!("for (key, value) in {}.iter()", config.expr))
                            };
                            if !key_enc_fields.is_empty() {
                                ser_loop.line(config.container_encoding_lookup(
                                    "key",
                                    &key_enc_fields,
                                    enc_lookup_var,
                                ));
                            }
                            let key_config = config
                                .clone()
                                .expr("key")
                                .expr_is_ref(true)
                                .var_name(format!("{}_key", config.var_name))
                                .is_end(false)
                                .encoding_var_no_option_struct()
                                .encoding_var_is_ref(false)
                                // fresh `{name}_key` namespace: reset tag depth to match
                                // `encoding_fields_impl`'s map-key reset.
                                .tag_depth(0);
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
                                enc_lookup_var,
                            ));
                        }
                        let value_config = config
                            .clone()
                            .expr("value")
                            .expr_is_ref(true)
                            .var_name(format!("{}_value", config.var_name))
                            .is_end(false)
                            .encoding_var_no_option_struct()
                            .encoding_var_is_ref(false)
                            // fresh `{name}_value` namespace: reset tag depth to match
                            // `encoding_fields_impl`'s map-value reset.
                            .tag_depth(0);
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
                            .encoding_var_is_ref(false)
                            // fresh `{name}_key` namespace: reset tag depth (as above).
                            .tag_depth(0);
                        let value_config = key_config
                            .clone()
                            .expr("value")
                            // `{name}_value` namespace; key_config already reset, kept explicit.
                            .var_name(format!("{}_value", config.var_name))
                            .tag_depth(0);
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
                SerializingRustType::Root(ConceptualRustType::Alias(ident, ty), cfg) => {
                    let alias_metadata = types
                        .type_aliases()
                        .get(ident)
                        .unwrap()
                        .rule_metadata
                        .as_ref();
                    let config_for_alias = if let Some(custom_serialize) =
                        alias_metadata.and_then(|rmd| rmd.custom_serialize.clone())
                    {
                        // The rule's `@custom_encodings` rides with the pair it is written beside —
                        // the second of the two carrier channels the emission sites see a
                        // declaration through (the other being the derivation from the type, which
                        // `encoding_fields_impl`'s own `Alias` arm owns).
                        config.custom_serialize(custom_serialize).custom_encodings(
                            alias_metadata.and_then(|rmd| rmd.custom_encodings.clone()),
                        )
                    } else {
                        config
                    };
                    // Keep the OUTER RustTypeSerializeConfig (`cfg`): an Alias's inner is a bare
                    // ConceptualRustType with no config of its own, so recursing with `(&**ty).into()`
                    // would DEFAULT the config and drop the per-rule policy the alias carries —
                    // notably `@duplicates preserve`, which the Map arm reads to pick the POSITIONAL
                    // encoding sidecar. (Deserialize's Alias arm keeps the config for the same reason;
                    // serialize previously discarded it because no serialize path had needed it —
                    // NonEmptyVec/NonEmptyMap serialize identically to their loose forms.)
                    self.generate_serialize(
                        types,
                        SerializingRustType::Root(ty, cfg),
                        body,
                        config_for_alias,
                        cli,
                    )
                }
            };
        }
    }
}
