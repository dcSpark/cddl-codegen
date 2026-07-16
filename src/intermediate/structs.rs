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
                config: RustStructConfig::from(rule_metadata),
                variant: RustStructType::TypeChoice { variants },
            }
        } else {
            Self {
                ident,
                tag,
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
            config: RustStructConfig::default(),
            variant: RustStructType::Extern,
        }
    }

    pub fn new_raw_bytes(ident: RustIdent) -> Self {
        Self {
            ident,
            tag: None,
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
            RustStructType::Record(record) => record.fields.iter().for_each(|field| {
                field
                    .rust_type
                    .conceptual_type
                    .visit_types_excluding(types, f, already_visited)
            }),
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
                        if !cli.preserve_encodings && field.rust_type.is_fixed_value() {
                            // we don't create fields for fixed values when preserve-encodings=false
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
                                Representation::Array => {
                                    ("x", field.rust_type.definite_info("x", true, types, cli))
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
}

#[derive(Debug, Clone)]
// internal, short-lived during generic resolution and never stored in bulk,
// so the size gap doesn't matter. Box the Resolved variant only if it ever lands in a hot collection.
#[allow(clippy::large_enum_variant)]
pub enum GenericResolved {
    // resolved with types swapped to concrete instance
    Resolved(RustStruct),
    // could not resolve (def is extern)
    Extern {
        // internal generic ident e.g. FooBar for Foo<Bar>
        instance_ident: RustIdent,
        // actual data type e.g. Foo<Bar>
        real_ident: RustIdent,
    },
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
                    return Ok(GenericResolved::Extern {
                        instance_ident: self.instance_ident.clone(),
                        real_ident: RustIdent::new_generic(
                            &self.generic_ident,
                            &self.generic_args,
                            types,
                            cli,
                        ),
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
        Ok(GenericResolved::Resolved(instance))
    }

    fn resolve_type(args: &BTreeMap<&RustIdent, &RustType>, orig: &RustType) -> RustType {
        if let ConceptualRustType::Rust(ident) = &orig.conceptual_type
            && let Some(resolved_type) = args.get(ident)
        {
            return (*resolved_type).clone();
        }
        orig.clone()
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
