use super::*;

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
    pub doc: Option<String>,
    /// For a map-representation group choice arm that collapsed a single keyed entry
    /// (`{ a: uint // ... }` → `A(u64)`), this holds the fixed member key (`a`). The value
    /// still lives in `data`; the key is written before it on serialization and read+verified
    /// before it on deserialization. `None` for array reps, type choices, and keyless entries.
    pub key: Option<FixedValue>,
}

impl EnumVariant {
    pub fn new(
        name: VariantIdent,
        rust_type: RustType,
        serialize_as_embedded_group: bool,
        doc: Option<String>,
    ) -> Self {
        Self {
            name,
            data: EnumVariantData::RustType(rust_type),
            serialize_as_embedded_group,
            doc,
            key: None,
        }
    }

    pub fn new_embedded(
        name: VariantIdent,
        embedded_record: RustRecord,
        doc: Option<String>,
    ) -> Self {
        Self {
            name,
            data: EnumVariantData::Inlined(embedded_record),
            serialize_as_embedded_group: false,
            doc,
            key: None,
        }
    }

    /// Builder for the collapse site: attach the fixed member key of a collapsed map-rep arm.
    pub fn with_key(mut self, key: Option<FixedValue>) -> Self {
        self.key = key;
        self
    }

    /// Gets the next CBOR type after the passed in rep (array/map) tag
    /// Returns None if this is not possible and brute-force deserialization
    /// trying every variant should be used instead
    pub fn cbor_types_inner(
        &self,
        types: &IntermediateTypes,
        outer_rep: Option<Representation>,
    ) -> Option<Vec<CBORType>> {
        // A collapsed map-rep arm dispatches on the type of its fixed member KEY, not its value:
        // spec-valid bytes for `{ a: uint // ... }` start with the key `"a"`, so the first CBOR
        // type after the map tag is the key's. (Array reps / type choices leave `key == None`.)
        if outer_rep == Some(Representation::Map)
            && let Some(key) = &self.key
        {
            return Some(vec![fixed_value_cbor_type(key)]);
        }
        match &self.data {
            EnumVariantData::RustType(ty) => {
                if ty.encodings.is_empty() && outer_rep.is_some() {
                    if let ConceptualRustType::Rust(ident) =
                        ty.conceptual_type.resolve_alias_shallow()
                    {
                        match types.rust_struct(ident).unwrap().variant() {
                            // we can't know this unless there's a way to provide this info
                            RustStructType::Extern => None,
                            RustStructType::Record(record) => {
                                // Reconcile with the serializer's `serialize_as_embedded_group`
                                // split (`generate_enum`): an embedded plain-group arm flattens
                                // the record's fields directly after the outer header, so the arm
                                // discriminant peeks the record's FIRST FIELD. A non-embedded
                                // named/aliased record arm instead serializes NESTED — the outer
                                // header then the record's OWN Array/Map header (or struct-level
                                // tag) — so the discriminant must peek that own wire type, which
                                // `RustType::cbor_types` already derives from `record.rep` and any
                                // struct tag. Hand-mapping `record.rep` here would drop the tag.
                                if self.serialize_as_embedded_group {
                                    Self::record_first_cbor_types(types, record)
                                } else {
                                    Some(ty.cbor_types(types))
                                }
                            }
                            // Conservative, not a gap: `None` = brute-force try-each-variant,
                            // which is always correct. By the same wire-form analysis as the
                            // Record arm above, a NON-embedded group-choice arm could dispatch on
                            // its own rep header (its serialize writes outer header then its own
                            // per-arm header), i.e. `Some(ty.cbor_types(types))` — but no row/cell
                            // pins that shape's dispatch today, so keep brute force until one does
                            // (enumerate the row first, per cddl-matrix/ROADMAP.md's
                            // "Intra-alternative variation rows" rule).
                            RustStructType::GroupChoice { .. } => None,
                            _ => Some(ty.cbor_types(types)),
                        }
                    } else {
                        Some(ty.cbor_types(types))
                    }
                } else {
                    Some(ty.cbor_types(types))
                }
            }
            EnumVariantData::Inlined(record) => {
                if outer_rep.is_some() {
                    Self::record_first_cbor_types(types, record)
                } else {
                    Some(match record.rep {
                        Representation::Array => vec![CBORType::Array],
                        Representation::Map => vec![CBORType::Map],
                    })
                }
            }
        }
    }

    /// The set of CBOR types the first data item of a record-representation arm can take, used for
    /// enum dispatch. Arrays are positional: only the first (non-optional-prefix) field matters. A
    /// map presents its entries in any order, so the first item is any KEY — dispatch must union
    /// every field's key type. A non-fixed key (`k => v`) is unknowable → `None` (brute force).
    fn record_first_cbor_types(
        types: &IntermediateTypes,
        record: &RustRecord,
    ) -> Option<Vec<CBORType>> {
        match record.rep {
            Representation::Map => {
                let mut ret = vec![];
                for field in record.fields.iter() {
                    match &field.key {
                        Some(key) => ret.push(fixed_value_cbor_type(key)),
                        None => return None,
                    }
                }
                Some(ret)
            }
            Representation::Array => {
                let mut ret = vec![];
                for field in record.fields.iter() {
                    ret.extend(field.rust_type.cbor_types(types));
                    if !field.optional {
                        break;
                    }
                }
                Some(ret)
            }
        }
    }

    /// The Record fields a GROUP choice expands into this variant's `new_<variant>` ctor as
    /// direct parameters (both the rust and the wasm pass), when the variant is a NAMED type
    /// resolving to a Record struct — `None` for every other variant shape (incl. aliases to
    /// records: the emitters match the bare `Rust` ident only, deliberately not
    /// `resolve_alias_shallow`, and an alias arm gets the single-arg ctor instead).
    /// `scope_references` marks exactly these field types so a ctor expanded from a record in
    /// ANOTHER module still imports what its parameters name; the emitters and that import walk
    /// all go through this one helper so the parameter list and the import set can't drift.
    /// Panics on an unresolvable ident (`enum_ident` names the enum for the message) — the
    /// resolution the emitters previously did inline.
    pub fn group_ctor_record_fields<'a>(
        &self,
        types: &'a IntermediateTypes,
        enum_ident: &RustIdent,
    ) -> Option<Vec<&'a RustField>> {
        let EnumVariantData::RustType(ty) = &self.data else {
            return None;
        };
        let ConceptualRustType::Rust(ident) = &ty.conceptual_type else {
            return None;
        };
        let resolved = types.rust_struct(ident).unwrap_or_else(|| {
            // Constant text LEADS so the recombination sweep's panic-class ledger can key on the
            // message (its key-shape floor rejects site-only keys; the idents vary per spec).
            panic!("variant ctor refers to undefined ident: {enum_ident} -> {ident}")
        });
        let RustStructType::Record(record) = resolved.variant() else {
            return None;
        };
        Some(
            record
                .fields
                .iter()
                .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                .collect(),
        )
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
                    // Only ARRAY records can be inlined into an enum variant: the inlined
                    // serializer (`generate_array_struct_serialization`) asserts array rep, and a
                    // map record's keys must still be written/read via the record path. A map arm
                    // with <=1 non-fixed fields therefore stays a named `GroupN` reference handled
                    // by the (key-aware) record path instead of being embedded (which panicked).
                    return record.rep == Representation::Array
                        && record
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

/// The CBOR major type a fixed value serializes to — used to dispatch enum variants on their fixed
/// map key. Mirrors `RustType::cbor_types` for `ConceptualRustType::Fixed`.
fn fixed_value_cbor_type(value: &FixedValue) -> CBORType {
    match value {
        FixedValue::Uint(_) => CBORType::UnsignedInteger,
        FixedValue::Nint(_) => CBORType::NegativeInteger,
        FixedValue::Float(_) => CBORType::Special,
        FixedValue::Text(_) => CBORType::Text,
        FixedValue::Null => CBORType::Special,
        FixedValue::Bool(_) => CBORType::Special,
    }
}

#[derive(Clone, Debug)]
pub struct RustField {
    pub name: String,
    pub rust_type: RustType,
    pub optional: bool,
    // None for array fields, Some for map fields. FixedValue for (de)serialization for map keys
    pub key: Option<FixedValue>,
    // comment DSL metadata applied to this field
    pub rule_metadata: RuleMetadata,
}

impl RustField {
    pub fn new(
        name: String,
        rust_type: RustType,
        optional: bool,
        key: Option<FixedValue>,
        rule_metadata: RuleMetadata,
    ) -> Self {
        Self {
            name,
            rust_type,
            optional,
            key,
            rule_metadata,
        }
    }

    pub fn to_embedded_rust_type(&self) -> Cow<'_, RustType> {
        if self.optional {
            Cow::Owned(RustType::new(ConceptualRustType::Optional(Box::new(
                self.rust_type.clone(),
            ))))
        } else {
            Cow::Borrowed(&self.rust_type)
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

#[derive(Clone, Debug, Default)]
pub struct RustStructConfig {
    pub custom_json: bool,
    pub custom_serialize: Option<String>,
    pub custom_deserialize: Option<String>,
    pub doc: Option<String>,
    pub newtype_getter: Option<Option<String>>,
    /// `@duplicates` policy for a collection rule (`[* a]` / `[+ a]` / the tag-258 set idiom / a
    /// table). Carried onto the registered transparent alias's `RustType` at `register_rust_struct`
    /// so every embed site (and generic use-site re-resolution) sees the policy.
    pub duplicates: Option<crate::comment_ast::DuplicatesPolicy>,
    /// A named non-generic SET rule (the tag-258 idiom or single-arm mandatory-258 form) nominalized
    /// into a `Wrapper` struct that OWNS its `{tag, len, elem}` encodings (Phase 2.2). Distinct from
    /// a plain `@newtype` wrapper: the set nominal suppresses the inherent `get()` (it shadows
    /// `OrderedSet::get(index)` through `Deref` — E0061), emits the set ergonomics
    /// (`Deref`/`DerefMut`/`IntoIterator`/`From`/`TryFrom`), and mandates always-on
    /// encodings-ignored comparison derives for parity with `OrderedSet`'s unconditional derives.
    pub set_nominal: bool,
}

impl From<Option<&RuleMetadata>> for RustStructConfig {
    fn from(rule_metadata: Option<&RuleMetadata>) -> Self {
        match rule_metadata {
            Some(rule_metadata) => Self {
                custom_json: rule_metadata.custom_json,
                custom_serialize: rule_metadata.custom_serialize.clone(),
                custom_deserialize: rule_metadata.custom_deserialize.clone(),
                doc: rule_metadata.comment.clone(),
                newtype_getter: rule_metadata.newtype.clone(),
                duplicates: rule_metadata.duplicates,
                set_nominal: false,
            },
            None => Self::default(),
        }
    }
}

// TODO: It would be nice to separate parsing the CDDL lib structs and code generation entirely.
// We would just need to construct these structs (+ maybe the array/table wrapper types) separately and pass these into codegen.
// This would also give us more access to this info without reparsing which could simplify code in some places.
// It would also remove the need for multiple passes over the CDDL to sort out dependencies between structs,
// which could also pave the way for multi-file CDDL supprt.
#[derive(Clone, Debug)]
pub struct RustStruct {
    pub(super) ident: RustIdent,
    pub(super) tag: Option<usize>,
    /// When `tag` is set, whether that tag is OPTIONALLY present on the wire (the transparent
    /// tag-set idiom, `x = #6.N([* a]) / [* a]`) rather than mandatory. Only ever true on an
    /// `Array`/`Table` variant produced by the `parse_type_choices` collapse; drives
    /// `register_rust_struct` to attach `OptionallyTagged` instead of `Tagged` to the registered
    /// alias. `false` everywhere else, so every pre-existing tagged rule stays byte-identical.
    pub(super) tag_optional: bool,
    config: RustStructConfig,
    pub(crate) variant: RustStructType,
}

#[derive(Clone, Debug)]
pub enum RustStructType {
    Record(RustRecord),
    Table {
        domain: RustType,
        range: RustType,
        /// occurrence-count bounds (`+` / `n*m`) — a min-cardinality constraint on the table itself.
        /// Only the `+` / `1*` shape `(Some(1), None)` is honored (→ `NonEmptyMap`); every other
        /// count-permitting marker is rejected at parse time (see `parse_group_type`), so in practice
        /// this is `None` (unbounded `*` table) or `Some((Some(1), None))` (non-empty table). Rides
        /// the registered alias's `RustType` so embed sites enforce it, exactly like `Array` bounds.
        bounds: Option<(Option<i128>, Option<i128>)>,
    },
    Array {
        element_type: RustType,
        /// occurrence-count bounds (`+` / `n*m`) — a LENGTH constraint on the array itself.
        /// Applied to the registered alias RustType's config so embed sites enforce it; kept off
        /// the element_type so it can't be misread as an element VALUE bound.
        bounds: Option<(Option<i128>, Option<i128>)>,
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
        /// NaN-safe float window for a float-typed wrapper (`c = 0.5..10.5`, `#6.5(0.5..10.5)`).
        /// Mutually exclusive with `min_max` (a wrapper never carries both). Its presence — like a
        /// `Some` `min_max` — makes the wrapper's `new()`/deserialize fallible.
        float_min_max: Option<FloatWindow>,
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
    pub fn new_record(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        record: RustRecord,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::Record(record),
        }
    }

    pub fn new_table(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        domain: RustType,
        range: RustType,
        bounds: Option<(Option<i128>, Option<i128>)>,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::Table {
                domain,
                range,
                bounds,
            },
        }
    }

    pub fn new_array(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        element_type: RustType,
        bounds: Option<(Option<i128>, Option<i128>)>,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::Array {
                element_type,
                bounds,
            },
        }
    }

    /// This will automatically check if it's a c-stlye enum and use that instead if possible
    pub fn new_type_choice(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
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
                tag_optional: false,
                config: RustStructConfig::from(rule_metadata),
                variant: RustStructType::TypeChoice { variants },
            }
        } else {
            Self {
                ident,
                tag,
                tag_optional: false,
                config: RustStructConfig::from(rule_metadata),
                variant: RustStructType::CStyleEnum { variants },
            }
        }
    }

    pub fn new_group_choice(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        variants: Vec<EnumVariant>,
        rep: Representation,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::GroupChoice { variants, rep },
        }
    }

    pub fn new_wrapper(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        wrapped_type: RustType,
        min_max: Option<(Option<i128>, Option<i128>)>,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::Wrapper {
                wrapped: wrapped_type,
                min_max,
                float_min_max: None,
            },
        }
    }

    /// A float-windowed wrapper (`c = 0.5..10.5`, `#6.5(0.5..10.5)`). Parallel to `new_wrapper` but
    /// carries a NaN-safe `FloatWindow` instead of an integer `min_max`.
    pub fn new_wrapper_float(
        ident: RustIdent,
        tag: Option<usize>,
        rule_metadata: Option<&RuleMetadata>,
        wrapped_type: RustType,
        float_min_max: Option<FloatWindow>,
    ) -> Self {
        Self {
            ident,
            tag,
            tag_optional: false,
            config: RustStructConfig::from(rule_metadata),
            variant: RustStructType::Wrapper {
                wrapped: wrapped_type,
                min_max: None,
                float_min_max,
            },
        }
    }

    pub fn new_extern(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
            tag_optional: false,
            config: RustStructConfig::default(),
            variant: RustStructType::Extern,
        }
    }

    pub fn new_raw_bytes(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
            tag_optional: false,
            config: RustStructConfig::default(),
            variant: RustStructType::RawBytesType,
        }
    }

    pub fn ident(&self) -> &RustIdent {
        &self.ident
    }

    pub fn tag(&self) -> Option<usize> {
        self.tag
    }

    /// Mark this struct's tag as OPTIONALLY present on the wire (the transparent tag-set collapse).
    /// See the `tag_optional` field. Consuming builder, like `RustType::as_alias`/`as_bytes`.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_optionally_tagged(mut self) -> Self {
        self.tag_optional = true;
        self
    }

    /// Mark this `Wrapper` struct as a nominalized SET (Phase 2.2). See `RustStructConfig::set_nominal`.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_set_nominal(mut self) -> Self {
        self.config.set_nominal = true;
        self
    }

    pub fn tag_optional(&self) -> bool {
        self.tag_optional
    }

    pub fn config(&self) -> &RustStructConfig {
        &self.config
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

    /// Even if fixed_field_count() == None, this will return an expression for
    /// a definite length, e.g. with optional field checks in the expression
    /// This is useful for definite-length serialization
    /// self_expr is an expression that evaluates to this struct (e.g. "self") at the point where
    /// the return of this function will be used.
    /// self_is_ref whether the above expression is by-ref
    pub fn definite_info(
        &self,
        self_expr: &str,
        self_is_ref: bool,
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> String {
        match &self.variant {
            RustStructType::Record(record) => {
                record.definite_info(self_expr, self_is_ref, types, cli)
            }
            RustStructType::Table { .. } => format!("{self_expr}.0.len() as u64"),
            RustStructType::Array { .. } => format!("{self_expr}.0.len() as u64"),
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
            RustStructType::CStyleEnum { .. } => "1".into(),
            RustStructType::RawBytesType => "1".into(),
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
            RustStructType::Extern => panic!(
                "do we need to look this up ever? will the prelude have structs with fields?"
            ),
            RustStructType::CStyleEnum { .. } => 1,
            RustStructType::RawBytesType => 1,
        }
    }

    pub fn cbor_len_info(&self, types: &IntermediateTypes) -> RustStructCBORLen {
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
            RustStructType::Extern => panic!(
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
            RustStructType::Array { element_type, .. } => element_type
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
            RustStructType::Record(record) => {
                record.fields.iter().for_each(|field| {
                    field
                        .rust_type
                        .conceptual_type
                        .visit_types_excluding(types, f, already_visited)
                });
                // Open struct-map rest row: its key/value types are real occurrences (they can
                // reference named rules), so a reference reachable ONLY through the rest domain/range
                // stays visible to `is_referenced` and friends.
                if let Some(rest) = &record.rest {
                    rest.domain
                        .conceptual_type
                        .visit_types_excluding(types, f, already_visited);
                    rest.range
                        .conceptual_type
                        .visit_types_excluding(types, f, already_visited);
                }
            }
            RustStructType::Table { domain, range, .. } => {
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
    /// The open ("rest") part of an open struct-map (`{ 1: a, ..., * K => V }`) — the trailing
    /// `* K => V` row captured as a map member alongside the fixed fields, rather than a fake
    /// `RustField` (fields drive `new()`/JSON/wasm/`orig_deser_order` indices; the rest row does
    /// not participate in those). `None` for every closed struct — the snapshot corpus enforces
    /// byte-identical output for those. Only ever `Some` on a `Map`-rep record. `Box`ed to keep
    /// `RustRecord` (and the `EnumVariantData::Inlined` embedding it) small — `RestRow` holds two
    /// `RustType`s.
    pub rest: Option<Box<RestRow>>,
}

/// Which of the two open struct-map flavors a rest row selects.
///
/// - `Capture` (the default): unknown entries are retained in a `pub` map field and round-tripped.
/// - `Ignore` (`@ignore` on the row): unknown entries are typed-deserialized and then DROPPED — no
///   struct field, serialize writes only the declared members. Deliberately lossy (documented), so
///   byte round-trips do not hold for wire data carrying unknown entries.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RestSemantics {
    Capture,
    Ignore,
}

/// The trailing `* K => V` row of an open struct-map. Under the CAPTURE flavor the content lands in a
/// `pub` map field (`rest` by default, `@name`-overridable) whose container matches the table switch
/// (`BTreeMap`/`OrderedHashMap`); under the IGNORE flavor nothing is stored. Not a `RustField`: it is
/// excluded from `new()` (defaults empty, so adding a rest row to a spec is source-compatible) and
/// carries the open-map semantics explicitly for the emitters.
#[derive(Clone, Debug)]
pub struct RestRow {
    /// The key type (`K`). Supported key domains: `uint`, `text`, and `any`.
    pub domain: RustType,
    /// The value type (`V`). Any supported type, including `any`.
    pub range: RustType,
    /// Capture (retain + round-trip) vs Ignore (tolerate-and-drop). Selects, at every emitter branch
    /// point, whether a `pub` field / constructor line / getter / encoding sidecar / serialize-back /
    /// JSON-flatten surface is emitted at all (capture-only) — while the deserialize arms and the IR
    /// visitors walk the row for BOTH flavors (both must consume the wire entries and both may pull in
    /// a runtime type through `domain`/`range`).
    pub semantics: RestSemantics,
    /// The captured map field's Rust name (default `rest`, overridable with `@name` on the row).
    /// Only meaningful under `Capture`; `Ignore` emits no field.
    pub field_name: String,
    /// `@duplicates` policy on the row. In the non-preserve capture flavor the container's `Eq` is
    /// value equality, so the default (reject) is enforced structurally by `insert().is_some()`;
    /// `Preserve` (the positional pair-list twin) is rejected at parse until the preserve work
    /// package reads this to select the container. Carried on the IR now so that lands cleanly.
    #[allow(dead_code)]
    pub duplicates: Option<crate::comment_ast::DuplicatesPolicy>,
}

impl RustRecord {
    /// The rest row IFF it CAPTURES (a `pub` map field is emitted and re-serialized). `None` for a
    /// closed struct AND for an `@ignore` (tolerate-and-drop) rest row, which stores nothing. Every
    /// capture-only emission (struct field, `new()` line, wasm getter, encoding sidecars, serialize
    /// of rest entries, flattened JSON, `definite_info`'s rest-count fold) keys on THIS, not on
    /// `rest.is_some()` — an ignore struct has no field to reference. The deserialize arms and the IR
    /// visitors keep using `rest` directly (both flavors consume the wire and may need a runtime type
    /// reachable only through `domain`/`range`).
    pub fn captured_rest(&self) -> Option<&RestRow> {
        self.rest
            .as_deref()
            .filter(|r| r.semantics == RestSemantics::Capture)
    }

    pub fn fixed_field_count(&self, types: &IntermediateTypes) -> Option<usize> {
        // An OPEN struct (rest row present) has a variable number of wire entries, so it is never a
        // fixed-length map: forcing `None` here routes `cbor_len_info` to the dynamic class (so the
        // deserialize length check accounts each rest entry via `read_len.read_elems(1)` in the loop
        // + `read_len.finish()` after, rather than asserting a fixed count up front) and steers
        // `definite_info` to the additive branch that folds in `rest.len()`.
        if self.rest.is_some() {
            return None;
        }
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

    /// This is guaranteed
    /// If inlined_enum is set, assumes the field is accessible via a local reference e.g. match branch
    /// Otherwise assumes it's a field e.g. self.name
    /// self_expr is an expression that evaluates to this struct (e.g. "self") at the point where
    /// the return of this function will be used.
    /// self_is_ref whether the above expression is by-ref
    pub fn definite_info(
        &self,
        self_expr: &str,
        self_is_ref: bool,
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> String {
        match self.fixed_field_count(types) {
            Some(count) => count.to_string(),
            None => {
                let mut fixed_field_count = 0;
                let mut conditional_field_expr = String::new();
                for field in &self.fields {
                    if field.optional {
                        if field.rust_type.is_fixed_value() {
                            // Optional fixed value (any kind, including float): modeled by a `bool`
                            // presence field (present => exactly one encoded item — an array element
                            // or a map key/value — absent => none). This replaces the former
                            // unconditional skip, which left `conditional_field_expr` empty and
                            // emitted the malformed `Len(1 + )` when it was the only dynamic-length
                            // field.
                            if !conditional_field_expr.is_empty() {
                                conditional_field_expr.push_str(" + ");
                            }
                            let self_field_expr = if self_expr.is_empty() {
                                Cow::Borrowed(&field.name)
                            } else {
                                Cow::Owned(format!("{}.{}", self_expr, field.name))
                            };
                            conditional_field_expr
                                .push_str(&format!("if {self_field_expr} {{ 1 }} else {{ 0 }}"));
                            continue;
                        }
                        if !conditional_field_expr.is_empty() {
                            conditional_field_expr.push_str(" + ");
                        }
                        let self_field_expr = if self_expr.is_empty() {
                            Cow::Borrowed(&field.name)
                        } else {
                            Cow::Owned(format!("{}.{}", self_expr, field.name))
                        };
                        if let Some(default_value) = &field.rust_type.config.default {
                            let field_contribution = match self.rep {
                                Representation::Array => Cow::Owned(field.rust_type.definite_info(
                                    &self_field_expr,
                                    true,
                                    types,
                                    cli,
                                )),
                                // maps are defined by their keys instead (although they shouldn't have multi-length values either...)
                                Representation::Map => Cow::Borrowed("1"),
                            };
                            if cli.preserve_encodings {
                                conditional_field_expr.push_str(&format!(
                                    "if {}.{} != {} || self.encodings.as_ref().map(|encs| encs.{}_default_present).unwrap_or(false) {{ {} }} else {{ 0 }}",
                                    self_expr,
                                    field.name,
                                    default_value.to_primitive_str_compare(),
                                    field.name,
                                    field_contribution));
                            } else {
                                conditional_field_expr.push_str(&format!(
                                    "if {}.{} != {} {{ {} }} else {{ 0 }}",
                                    self_expr,
                                    field.name,
                                    default_value.to_primitive_str_compare(),
                                    field_contribution
                                ));
                            }
                        } else {
                            let (field_expr, field_contribution) = match self.rep {
                                // when expanded_field_count is Some, definite_info returns the
                                // constant count.to_string() and never uses the binding, so bind
                                // `_` to avoid an unused-variable warning in the generated match.
                                Representation::Array => {
                                    match field.rust_type.expanded_field_count(types) {
                                        Some(count) => ("_", count.to_string()),
                                        None => (
                                            "x",
                                            field.rust_type.definite_info("x", true, types, cli),
                                        ),
                                    }
                                }
                                // maps are defined by their keys instead (although they shouldn't have multi-length values either...)
                                Representation::Map => ("_", String::from("1")),
                            };
                            conditional_field_expr.push_str(&format!(
                                "match {}{} {{ Some({}) => {}, None => 0 }}",
                                if self_is_ref { "" } else { "&" },
                                self_field_expr,
                                field_expr,
                                field_contribution
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
                                            false,
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
                // Open struct (CAPTURE flavor only): the map header must count the captured rest
                // entries too. `rest` is a `pub` map field, so its live entry count is
                // `{self_expr}.rest.len()`. Folding it in here keeps the serialize header and the
                // deserialize length accounting in agreement (both go through `definite_info`/the
                // dynamic length class). Empty rest → `+ 0`, recovering the closed-struct byte count
                // (empty-rest ≡ closed-struct invariant). The IGNORE flavor stores and re-serializes
                // NO rest entries and has no field to reference, so its header is the closed-struct
                // count — no fold (and `fixed_field_count` still returns `None` for it, so the deser
                // loop stays dynamic and tolerates the extra wire entries it drops).
                if let Some(rest) = self.captured_rest() {
                    // `.len()` is `usize`; the map header (`cbor_event::Len::Len(u64)`) and the
                    // additive expression it joins are `u64`, so cast (`as` binds tighter than `+`).
                    let rest_len_expr = if self_expr.is_empty() {
                        format!("{}.len() as u64", rest.field_name)
                    } else {
                        format!("{}.{}.len() as u64", self_expr, rest.field_name)
                    };
                    if !conditional_field_expr.is_empty() {
                        conditional_field_expr.push_str(" + ");
                    }
                    conditional_field_expr.push_str(&rest_len_expr);
                }
                if conditional_field_expr.is_empty() {
                    // No optional field and no captured-rest fold (an `@ignore` open struct with only
                    // mandatory map fields reaches here via `fixed_field_count == None`): the definite
                    // length is exactly the mandatory count. Emitting `"{n} + "` would be malformed.
                    // Existing specs never hit this branch — a closed struct returns `Some(count)`
                    // early, and a capture open struct always folds a non-empty `rest.len()` term — so
                    // output for everything that exists today is byte-identical.
                    fixed_field_count.to_string()
                } else if fixed_field_count != 0 {
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
    pub(super) orig: RustStruct,
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
    pub(super) instance_ident: RustIdent,
    pub(super) generic_ident: RustIdent,
    generic_args: Vec<RustType>,
    // `true` when the instance ident was SYNTHESIZED for an anonymous use site (`[a: bar<text>]` →
    // `BarText`), rather than declared by a rule the author wrote (`foo = bar<text>`, ident `Foo`).
    // A synthesized name carries no user intent worth surfacing as its own wasm class: an anonymous
    // instance resolving to a TRANSPARENT COLLECTION lowers wasm-side to the STRUCTURAL wrapper
    // (`BarTextList`), exactly like the equivalent inline `[* text]`, rather than minting a
    // rule-named `#[wasm_bindgen]` class. See `converge_anonymous_collection_instance_wasm`.
    pub(super) anonymous: bool,
    /// The INSTANTIATION-derived nominal ident this instance mints, computed identically for every
    /// spelling of the same instantiation (`<def>_<args' for_variant()>` → `SetKeyHash`; Phase 2.3).
    /// For an anonymous use site this equals `instance_ident`; for a named binding
    /// (`named_set = set<key_hash>`, ident `NamedSet`) it is the instantiation nominal the binding
    /// aliases TO. Only consulted when the resolved def is a SET NOMINAL wrapper — every other
    /// instance keeps minting under `instance_ident` unchanged.
    pub(super) canonical_ident: RustIdent,
}

#[derive(Debug, Clone)]
// internal, short-lived during generic resolution and never stored in bulk,
// so the size gap doesn't matter. Box the Resolved variant only if it ever lands in a hot collection.
#[allow(clippy::large_enum_variant)]
pub enum GenericResolved {
    // resolved with types swapped to concrete instance
    Resolved(RustStruct),
    /// A generic SET-NOMINAL instance (`set<key_hash>` → `SetKeyHash`; Phase 2.3): the instantiation
    /// mints ONE nominal wrapper struct under `canonical_ident`, DEDUPED across every spelling of the
    /// same instantiation. `finalize` registers the struct once per distinct `canonical_ident`, and
    /// registers a transparent alias `instance_ident = canonical_ident` whenever the two differ (a
    /// named binding) — an anonymous instance's `instance_ident` already equals its canonical, so it
    /// needs no alias. The author's spelling is the identity (rev 7): `set<[* uint]>` and
    /// `set<xs_int>` mint DIFFERENT nominals openly.
    SetNominal {
        instance_ident: RustIdent,
        canonical_ident: RustIdent,
        resolved: RustStruct,
    },
    // could not resolve (def is extern)
    Extern {
        // internal generic ident e.g. FooBar for Foo<Bar>
        instance_ident: RustIdent,
        // actual data type e.g. Foo<Bar>
        real_ident: RustIdent,
        // Some(base) when `@raw_bytes_flavor` selected the `<base>RawBytes` wrapper for this
        // instance (a raw-bytes argument was supplied); `finalize` records the base so the extern
        // re-export glue emits `pub use crate::<base>RawBytes;`. None for the plain path.
        flavored_base: Option<RustIdent>,
    },
}

impl GenericInstance {
    pub fn new(
        instance_ident: RustIdent,
        generic_ident: RustIdent,
        generic_args: Vec<RustType>,
        anonymous: bool,
        canonical_ident: RustIdent,
    ) -> Self {
        Self {
            instance_ident,
            generic_ident,
            generic_args,
            anonymous,
            canonical_ident,
        }
    }

    // TODO: should we rename fields / variant names after-the-fact?
    // (for the cases where the name came from the original generic param)
    // returns None when it can't be resolved i.e. extern defs
    // this will be left to the user instead to handle.
    pub fn resolve(
        &self,
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> Result<GenericResolved, Box<dyn std::error::Error>> {
        let def = match types.generic_defs.get(&self.generic_ident) {
            Some(def) => def,
            None => {
                if types
                    .rust_struct(&self.generic_ident)
                    .map(|rs| matches!(rs.variant(), RustStructType::Extern))
                    .unwrap_or(false)
                {
                    // The instance's `real_ident` is the full `Base<Args>` type expression: the
                    // `@raw_bytes_flavor` base name (`extern_base_ident`) plus the concrete args.
                    // The base-name/flavor decision is owned by `extern_base_ident` /
                    // `uses_raw_bytes_flavor` so this mint and the alias-import walk in
                    // `scope_references` reference the same base and cannot drift.
                    let real_ident = RustIdent::new_generic_with_base(
                        self.extern_base_ident(types).as_ref(),
                        &self.generic_args,
                        types,
                        cli,
                    );
                    let flavored_base = self
                        .uses_raw_bytes_flavor(types)
                        .then(|| self.generic_ident.clone());
                    return Ok(GenericResolved::Extern {
                        instance_ident: self.instance_ident.clone(),
                        real_ident,
                        flavored_base,
                    });
                }
                return Err(format!(
                    "generic instance `{}` references undefined generic type `{}`",
                    self.instance_ident, self.generic_ident
                )
                .into());
            }
        };
        if def.generic_params.len() != self.generic_args.len() {
            return Err(format!(
                "generic `{}` expects {} argument(s) but `{}` supplies {}",
                self.generic_ident,
                def.generic_params.len(),
                self.instance_ident,
                self.generic_args.len()
            )
            .into());
        }
        let resolved_args = def
            .generic_params
            .iter()
            .zip(self.generic_args.iter())
            .collect::<BTreeMap<&RustIdent, &RustType>>();
        let mut instance = def.orig.clone();
        instance.ident = self.instance_ident.clone();

        // A generic SET NOMINAL def (`set<a0> = #6.258([* a0]) / [* a0]`) resolves to ONE nominal
        // wrapper per instantiation (Phase 2.3). Capture the flag before the `&mut instance.variant`
        // borrow so both the element resolution below and the post-match rename can consult it.
        let set_nominal = instance.config.set_nominal;

        match &mut instance.variant {
            RustStructType::Record(record) => {
                for field in record.fields.iter_mut() {
                    field.rust_type = Self::resolve_type(&resolved_args, &field.rust_type);
                }
            }
            RustStructType::Table { domain, range, .. } => {
                *domain = Self::resolve_type(&resolved_args, domain);
                *range = Self::resolve_type(&resolved_args, range);
            }
            RustStructType::Array { element_type, .. } => {
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
            RustStructType::Wrapper { wrapped, .. } if set_nominal => {
                // A set nominal always wraps a homogeneous occurrence array (`Array(elem)`); resolve
                // the ELEMENT (the generic param) to the concrete instance type, exactly like the
                // `Array` arm above. The optional-tag encoding op and occurrence bounds ride on the
                // wrapped `RustType` unchanged.
                if let ConceptualRustType::Array(elem) = &mut wrapped.conceptual_type {
                    **elem = Self::resolve_type(&resolved_args, elem);
                } else {
                    unreachable!(
                        "a generic set nominal always wraps a homogeneous occurrence array"
                    );
                }
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
        if set_nominal {
            // Mint under the INSTANTIATION-derived canonical ident (`SetKeyHash`), deduped by
            // `finalize` across every spelling of this instantiation; the binding's own ident aliases
            // to it when it differs.
            instance.ident = self.canonical_ident.clone();
            return Ok(GenericResolved::SetNominal {
                instance_ident: self.instance_ident.clone(),
                canonical_ident: self.canonical_ident.clone(),
                resolved: instance,
            });
        }
        Ok(GenericResolved::Resolved(instance))
    }

    /// The concrete type arguments this instance was invoked with (`ext_set<pub_key>` → `[pub_key]`).
    /// Exposed so `scope_references`'s alias walk can import each argument type the resolved alias
    /// line names bare (`…<PubKey>`).
    pub(super) fn generic_args(&self) -> &[RustType] {
        &self.generic_args
    }

    /// `@raw_bytes_flavor`: the base extern is tagged AND at least one argument resolves to a
    /// `_CDDL_CODEGEN_RAW_BYTES_TYPE_`, so the instance references the convention-named
    /// `<Base>RawBytes` wrapper flavor instead of the plain `<Base>`. Opt-in only — a plain-name
    /// instance keeps compiling for wrappers bound solely on `RawBytesEncoding`, so this never fires
    /// without the tag.
    fn uses_raw_bytes_flavor(&self, types: &IntermediateTypes) -> bool {
        types.raw_bytes_flavor().contains(&self.generic_ident)
            && self
                .generic_args
                .iter()
                .any(|arg| Self::arg_is_raw_bytes(types, arg))
    }

    /// The base name the resolved extern alias references: the `<Base>RawBytes` wrapper flavor when
    /// `@raw_bytes_flavor` selected it (see [`Self::uses_raw_bytes_flavor`]), else the plain
    /// `<Base>`. The ONE owner of the flavor→base-name decision, called from both `resolve` (which
    /// mints the full `Base<Args>` real ident) and `scope_references`'s type-alias walk (which
    /// imports this base at the base extern's declaring scope, where the re-export glue places
    /// `pub use crate::<Base>[RawBytes];`). Single owner so the emitted alias target and its import
    /// cannot drift. The flavored name is built the same way the glue does — `{generic_ident}RawBytes`
    /// (`convert_to_camel_case` is idempotent on an already-camel base, so no name drift).
    pub(super) fn extern_base_ident(&self, types: &IntermediateTypes) -> RustIdent {
        if self.uses_raw_bytes_flavor(types) {
            RustIdent::new(CDDLIdent::new(format!("{}RawBytes", self.generic_ident)))
        } else {
            self.generic_ident.clone()
        }
    }

    fn resolve_type(args: &BTreeMap<&RustIdent, &RustType>, orig: &RustType) -> RustType {
        if let ConceptualRustType::Rust(ident) = &orig.conceptual_type
            && let Some(resolved_type) = args.get(ident)
        {
            return (*resolved_type).clone();
        }
        orig.clone()
    }

    /// Whether a generic argument ultimately names a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` struct. Follows
    /// inline conceptual aliases (`resolve_alias_shallow`) and named type aliases so an argument that
    /// reaches a raw-bytes struct through an intermediate alias still selects the flavor. The chain is
    /// bounded to guard against a pathological self-referential alias.
    fn arg_is_raw_bytes(types: &IntermediateTypes, arg: &RustType) -> bool {
        let mut ct = arg.conceptual_type.resolve_alias_shallow().clone();
        for _ in 0..16 {
            let ident = match ct {
                ConceptualRustType::Rust(ident) => ident,
                _ => return false,
            };
            if types
                .rust_struct(&ident)
                .map(|rs| matches!(rs.variant(), RustStructType::RawBytesType))
                .unwrap_or(false)
            {
                return true;
            }
            // Not a struct we recognize as raw-bytes — follow a named type alias if there is one.
            match types.resolve_alias(&AliasIdent::Rust(ident)) {
                Some(next) => ct = next.conceptual_type.resolve_alias_shallow().clone(),
                None => return false,
            }
        }
        false
    }
}

fn enum_variant_constant(variant: &EnumVariant) -> Option<FixedValue> {
    if let EnumVariantData::RustType(ty) = &variant.data
        && let ConceptualRustType::Fixed(constant) = ty.conceptual_type.resolve_alias_shallow()
    {
        return Some(constant.clone());
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
