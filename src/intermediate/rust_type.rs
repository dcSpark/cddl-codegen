//! The rust type vocabulary (`RustType`/`ConceptualRustType` and friends).
//!
//! Known exceptions to the intermediate->generation module boundary: the type-naming methods here
//! reach back into `generation` — the `table_type` import below and the
//! `crate::generation::rust_crate_struct_from_wasm` calls in `for_rust_member_ct`/`name_for_rust_map`.
//! Full inversion is a bigger job (the `from_wasm` flag threads through type naming), so these stay
//! as the boundary's documented leaks rather than being "fixed" here.
use super::*;
use crate::generation::table_type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Representation {
    Array,
    Map,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FixedValue {
    Null,
    Undefined,
    Bool(bool),
    Nint(i128),
    Uint(u64),
    Float(f64),
    Text(String),
    /// A decoded CDDL byte-string literal.  Keep bytes, rather than a source spelling, so h'…'
    /// and '…' share the wire identity and non-text values never pass through UTF-8.
    Bytes(Vec<u8>),
}

fn convert_to_alphanumeric(input: &str) -> String {
    input
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .collect()
}

impl FixedValue {
    /// A typed, owned Rust byte-vector literal.  The explicit carrier is needed for `h''`, whose
    /// bare `vec![]` has no element type in generated serialization code.
    pub fn bytes_rust_expr(bytes: &[u8]) -> String {
        format!("Vec::<u8>::from({bytes:?})")
    }
    /// A total, Rust-identifier-safe spelling for a nominal fixed-value singleton.  This is an
    /// identity, not a display label: text uses UTF-8 bytes and floats use their IEEE bits so that
    /// values which happen to print alike (notably `-0.0`) cannot share an owner.
    pub fn singleton_name_fragment(&self) -> String {
        match self {
            Self::Null => "Null".to_owned(),
            Self::Undefined => "Undefined".to_owned(),
            Self::Bool(true) => "BoolTrue".to_owned(),
            Self::Bool(false) => "BoolFalse".to_owned(),
            Self::Uint(value) => format!("Uint{value}"),
            Self::Nint(value) => format!("Nint{}", value.unsigned_abs()),
            Self::Float(value) => format!("Float{:016X}", value.to_bits()),
            Self::Text(value) if value.is_empty() => "TextEmpty".to_owned(),
            Self::Text(value) => format!(
                "Text{}",
                value
                    .as_bytes()
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<String>()
            ),
            Self::Bytes(value) if value.is_empty() => "BytesEmpty".to_owned(),
            Self::Bytes(value) => format!(
                "Bytes{}",
                value
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<String>()
            ),
        }
    }

    fn for_variant(&self) -> VariantIdent {
        match self {
            FixedValue::Null => VariantIdent::new_custom("Null"),
            FixedValue::Undefined => VariantIdent::new_custom("Undefined"),
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
            FixedValue::Bytes(bytes) if bytes.is_empty() => VariantIdent::new_custom("BytesEmpty"),
            FixedValue::Bytes(bytes) => VariantIdent::new_custom(format!(
                "Bytes{}",
                bytes
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<String>()
            )),
        }
    }

    // `pub(crate)`: the codegen-time canonical key merge for open struct-maps
    // (`generation/records.rs`) renders a declared field's canonical key bytes as a `vec![..]`
    // literal, fed to the same length-first comparator the runtime uses — so an open struct and its
    // closed equivalent canonicalize identically.
    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        let mut buf = cbor_event::se::Serializer::new_vec();
        match self {
            FixedValue::Null => buf.write_special(cbor_event::Special::Null),
            FixedValue::Undefined => buf.write_special(cbor_event::Special::Undefined),
            FixedValue::Bool(b) => buf.write_special(cbor_event::Special::Bool(*b)),
            // Nint holds i128, whose values below i64::MIN don't fit the plain
            // write_negative_integer endpoint's i64 argument; the _sz form
            // takes i128 and encodes the full CBOR nint range. Passing Sz::canonical(magnitude)
            // reproduces the default endpoint's byte-for-byte canonical layout (write_type_definite
            // derives the same Sz from the magnitude when no Sz is supplied).
            FixedValue::Nint(i) => {
                let magnitude = (-(*i + 1)) as u64;
                buf.write_negative_integer_sz(*i, cbor_event::Sz::canonical(magnitude))
            }
            FixedValue::Uint(u) => buf.write_unsigned_integer(*u),
            FixedValue::Float(f) => buf.write_special(Special::Float(*f)),
            FixedValue::Text(s) => buf.write_text(s),
            FixedValue::Bytes(bytes) => buf.write_bytes(bytes),
        }
        .expect("Unable to serialize key for canonical ordering");
        buf.finalize()
    }

    /// Converts a literal to a valid rust expression capable of initializing a Primitive
    /// e.g. Text is an actual String, etc
    pub fn to_primitive_str_assign(&self) -> String {
        match self {
            FixedValue::Null => "None".to_owned(),
            // `undefined` has no Rust primitive representation.  This helper only serves the
            // primitive-default/comparison paths, which reject it before rendering an expression.
            FixedValue::Undefined => unreachable!("undefined is not a Rust primitive"),
            FixedValue::Bool(b) => b.to_string(),
            FixedValue::Nint(i) => i.to_string(),
            FixedValue::Uint(u) => u.to_string(),
            // `{:?}`, not Display: Display on a whole-valued f64 drops the decimal point
            // (3.0 -> "3"), rendering an integer literal in a float position (E0308). Debug
            // round-trips (3.0 -> "3.0"); unsuffixed so the literal also types as f32 where
            // the target is f32.
            FixedValue::Float(f) => format!("{f:?}"),
            FixedValue::Text(s) => format!("\"{}\".to_owned()", escape_rust_str(s)),
            FixedValue::Bytes(bytes) => Self::bytes_rust_expr(bytes),
        }
    }

    /// How the value is spelled in CDDL SOURCE, for rejection messages the user reads against
    /// their own spec (`5`, `true`, `null`, `"v1"`) — not the rust-literal renderings above, whose
    /// escaping / `to_owned()` framing belongs to emitted code. Every message naming a bare fixed
    /// value routes through this one helper so the alias-position and member-position rejections
    /// can't drift in how they quote the value back.
    pub fn cddl_source_desc(&self) -> String {
        match self {
            FixedValue::Null => "null".to_owned(),
            FixedValue::Undefined => "undefined".to_owned(),
            FixedValue::Bool(b) => b.to_string(),
            FixedValue::Nint(i) => i.to_string(),
            FixedValue::Uint(u) => u.to_string(),
            // `{:?}`, not Display: Display on a whole-valued f64 drops the decimal point, which
            // would quote `3.0` back to the user as `3`.
            FixedValue::Float(f) => format!("{f:?}"),
            FixedValue::Text(s) => format!("\"{s}\""),
            FixedValue::Bytes(bytes) => format!(
                "h'{}'",
                bytes
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<String>()
            ),
        }
    }

    /// Converts a literal to a valid rust comparison valid for comparisons
    /// e.g. Text can be &str to avoid creating a String
    pub fn to_primitive_str_compare(&self) -> String {
        match self {
            FixedValue::Text(s) => format!("\"{}\"", escape_rust_str(s)),
            FixedValue::Bytes(_) => self.to_primitive_str_assign(),
            _ => self.to_primitive_str_assign(),
        }
    }
}

impl RustType {
    /// A total identifier fragment for a synthesized nominal fixed-value owner.  It includes the
    /// entire wire shape, not merely the CBOR value: `true` and `#6.7(true)` are distinct Rust
    /// values with distinct codecs, and must never deduplicate to one owner based on parse order.
    ///
    /// This deliberately spells the small, closed encoding-operation vocabulary rather than using
    /// a hash.  The spelling is injective (and therefore cannot turn a hash collision into a
    /// silent wire change), Rust-identifier-safe, and preserves the historic bare-value names.
    pub fn fixed_singleton_name_fragment(&self) -> String {
        let fixed = match self.conceptual_type.resolve_alias_shallow() {
            ConceptualRustType::Fixed(fixed) => fixed,
            other => panic!("fixed singleton name requested for {other:?}"),
        };
        let mut out = fixed.singleton_name_fragment();
        for operation in &self.encodings {
            match operation {
                CBOREncodingOperation::Tagged(tag) => out.push_str(&format!("__Tag{tag}")),
                CBOREncodingOperation::OptionallyTagged(tag) => {
                    out.push_str(&format!("__OptionalTag{tag}"));
                }
                CBOREncodingOperation::CBORBytes => out.push_str("__CborBytes"),
            }
        }
        // Fixed values normally have default config, but retaining every field makes this identity
        // correct if a future fixed-value control operator attaches one.  The delimiters and
        // fixed-width float bits keep this spelling structurally injective.
        if let Some(default) = &self.config.default {
            out.push_str("__Default");
            out.push_str(&default.singleton_name_fragment());
        }
        if let Some((min, max)) = self.config.bounds {
            out.push_str(&format!(
                "__Bounds{}_{}",
                min.map_or_else(|| "None".to_owned(), |value| format!("Some{value}")),
                max.map_or_else(|| "None".to_owned(), |value| format!("Some{value}")),
            ));
        }
        if let Some((min, max)) = self.config.float_bounds {
            let bound = |value: Option<(f64, bool)>| match value {
                Some((value, exclusive)) => format!(
                    "Some{:016X}{}",
                    value.to_bits(),
                    if exclusive { "Exclusive" } else { "Inclusive" }
                ),
                None => "None".to_owned(),
            };
            out.push_str(&format!("__FloatBounds{}_{}", bound(min), bound(max)));
        }
        if let Some(policy) = self.config.duplicates {
            out.push_str(&format!("__Duplicates{policy:?}"));
        }
        if self.config.basic_override {
            out.push_str("__BasicOverride");
        }
        out
    }
}

/// Escape a CDDL fixed text value for safe interpolation into an emitted Rust string literal.
/// CDDL text literals may legally contain `"` or `\`; without escaping, those emit invalid Rust
/// (rustfmt then fails on the generated source). Plain values are unchanged.
pub fn escape_rust_str(s: &str) -> String {
    s.escape_default().to_string()
}

/// The CDDL float prelude names are SIX distinct VALUE classes, not two carrier widths, and not
/// wire-encoding constraints. RFC 8610 §2.2.3 is explicit that the `#7.x` notation "is about a set
/// of values at the data model level … it does not mandate that these values also do have to be
/// serialized as half-precision floats: CDDL does not provide any language means to restrict the
/// choice of serialization variants". The six names PARTITION the float values by their shortest
/// lossless form: `float16` is the values whose shortest form is `#7.25`, `float32` `#7.26`,
/// `float64` `#7.27`, with `float16-32`/`float32-64` spanning two and `float` all three. The classes
/// are disjoint — `1.5` is a `float16` and not a `float32` — which is the only reading under which
/// `float16-32`/`float32-64` are not redundant spellings.
///
/// The Rust CARRIER is a function of the class window (`f32` when every value in the class fits it),
/// so two classes can share a carrier and still be distinct types — which is why `float` and
/// `float64` cannot share one identity: both carry `f64`, but `float` is every float value and
/// `float64` only those needing all eight bytes.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Primitive {
    Bool,
    /// CDDL `float` (`float = float16-32 / float64`) — UNCONSTRAINED: every float value.
    Float,
    /// CDDL `float64` — the values whose shortest lossless form is `#7.27` (`fb`).
    F64,
    /// CDDL `float32` — the values whose shortest lossless form is `#7.26` (`fa`).
    F32,
    /// CDDL `float16` — the values whose shortest lossless form is `#7.25` (`f9`). Carried as
    /// `f32`: every such value widens into it exactly.
    F16,
    /// CDDL `float16-32` — shortest lossless form `#7.25` or `#7.26`.
    F16To32,
    /// CDDL `float32-64` — shortest lossless form `#7.26` or `#7.27`.
    F32To64,
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

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Primitive::Bool => "bool",
                Primitive::F16 | Primitive::F32 | Primitive::F16To32 => "f32",
                Primitive::F64 | Primitive::F32To64 | Primitive::Float => "f64",
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
            }
        )
    }
}
// TODO: impl display or fmt or whatever rust uses
impl Primitive {
    /// The window of SHORTEST-LOSSLESS-FORM widths this primitive's CDDL float class spans, as the
    /// `cbor_event::Sz` spellings of its narrowest and widest — or `None` when it is not a float.
    /// A value belongs to the class exactly when `smallest_float_sz(value)` lands inside the window,
    /// which is what lets both directions express the class as a pair of bounds: decode accepts any
    /// head and rejects a VALUE whose shortest form falls outside, and a write emits that same
    /// shortest form (RFC 8949 §4.1 preferred serialization), which for a member is its declared
    /// width by construction. The window is always contiguous in width order (`Two` < `Four` <
    /// `Eight`).
    pub fn float_class_window(self) -> Option<(&'static str, &'static str)> {
        Some(match self {
            Primitive::F16 => ("Two", "Two"),
            Primitive::F32 => ("Four", "Four"),
            Primitive::F64 => ("Eight", "Eight"),
            Primitive::F16To32 => ("Two", "Four"),
            Primitive::F32To64 => ("Four", "Eight"),
            Primitive::Float => ("Two", "Eight"),
            _ => return None,
        })
    }

    /// Whether this is one of the six float classes.
    pub fn is_float(self) -> bool {
        self.float_class_window().is_some()
    }

    /// Whether a float class's Rust carrier is `f32` — true exactly when every VALUE it admits fits
    /// an `f32` exactly (`float16`, `float32`, `float16-32`). The wire domain is `f64` either way,
    /// so an `f32`-carried class narrows after a read and widens before a write; both conversions
    /// are exact by construction and go through the runtime's `narrow_f32` /
    /// `cbor_event::se::f32_to_f64_exact` (never an `as` cast, whose NaN-payload behavior is both
    /// platform-dependent and const-fold-dependent within one binary).
    pub fn float_carrier_is_f32(self) -> bool {
        matches!(self, Primitive::F16 | Primitive::F32 | Primitive::F16To32)
    }

    pub fn to_variant(self) -> VariantIdent {
        VariantIdent::new_custom(match self {
            Primitive::Bool => "Bool",
            Primitive::Float => "Float",
            Primitive::F16 => "F16",
            Primitive::F32 => "F32",
            Primitive::F64 => "F64",
            Primitive::F16To32 => "F16To32",
            Primitive::F32To64 => "F32To64",
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
            // Every float class is major type 7 regardless of which VALUES it admits — class
            // membership is checked after the type dispatch, not by it.
            Primitive::Float
            | Primitive::F16
            | Primitive::F32
            | Primitive::F64
            | Primitive::F16To32
            | Primitive::F32To64 => vec![CBORType::Special],
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

/// Details on how to encode a rust type in CBOR. Order is important
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CBOREncodingOperation {
    /// CBOR tagged type
    Tagged(usize),
    /// An OPTIONALLY-present CBOR tag: the value may appear tagged (`#6.N(inner)`) or bare (`inner`)
    /// on the wire and both denote the same logical value. Produced only by the transparent
    /// tag-set collapse (`x = #6.N([* a]) / [* a]`, see `parse_type_choices`); kept as its own
    /// variant — never a flag on `Tagged` — so every existing `Tagged` site stays byte-identical.
    /// Under `--preserve-encodings` the wire arm is stored in a `TagPresenceEncoding` var; without
    /// it, serialize defaults to tagged and deserialize accepts either.
    OptionallyTagged(usize),
    /// bytes .cbor T in cddl, outside of serialization is semantically like T
    CBORBytes,
}

/// A per-side float bound window: `(min, max)` where each present side is `Some((value, exclusive))`.
/// `exclusive == true` means the endpoint is EXCLUDED (`.gt`/`.lt`/the `...` exclusive rangeop);
/// `false` means included (`.ge`/`.le`/`.eq`/the `..` inclusive rangeop). Unlike the integer window
/// there is no ±1 collapse (float space is dense), so exclusivity is carried explicitly per side.
/// Stored PARALLEL to — and mutually exclusive with — the integer `bounds` (a type never carries
/// both). The value is kept as f64 even for an f32-typed member so the authored decimal literal is
/// used exactly; emitted comparisons cast the f32 value to f64.
pub type FloatWindow = (Option<(f64, bool)>, Option<(f64, bool)>);

#[derive(Clone, Debug, Default, PartialEq)]
pub struct RustTypeSerializeConfig {
    /// default value when missing in deserialization
    pub default: Option<FixedValue>,
    /// Bounds to check. Relevant to primitives + arrays + maps
    pub bounds: Option<(Option<i128>, Option<i128>)>,
    /// Per-rule `@duplicates` policy for an array or table collection member. On arrays (`[* a]` /
    /// `[+ a]`, including the tag-258 set idiom), `Some(Reject)` swaps to the uniqueness twin
    /// (`OrderedSet`/`NonEmptyOrderedSet`) whose single `TryFrom` door refuses duplicates. On loose
    /// tables it is the explicit, accepted default: the `BTreeMap` representation is already
    /// key-unique. This rides the transparent alias `RustType` the same way `bounds` does — attached
    /// POST-arm at the `register_rust_struct` collection arms, never on the raw arm types the
    /// tag-set collapse recognizer compares for structural equality (see
    /// `parsing::recognize_optional_tag_set`).
    pub duplicates: Option<crate::comment_ast::DuplicatesPolicy>,
    /// Float value window to check (NaN-safe). Mutually exclusive with `bounds`; only ever set on
    /// a float primitive member (`float64 .le 10.5`, `[f: 0.5..10.5]`).
    pub float_bounds: Option<FloatWindow>,
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
        if let Some(t) = tag { self.tag(t) } else { self }
    }

    /// Push an OPTIONALLY-present tag (the transparent tag-set idiom). See
    /// [`CBOREncodingOperation::OptionallyTagged`].
    pub fn optionally_tag(mut self, tag: usize) -> Self {
        self.encodings
            .push(CBOREncodingOperation::OptionallyTagged(tag));
        self
    }

    /// Apply a `.default` value, or hand the type BACK undefaulted when the value cannot be lowered
    /// onto it.
    ///
    /// A default substitutes for an absent value at deserialization, so it is written into the
    /// primitive that backs the constrained type — a head that is not such a primitive (a named type
    /// with no rust primitive behind it, or the inert placeholder a name refusal already left behind)
    /// has nothing to write it into. Fallible rather than asserting because BOTH `.default`
    /// application sites are reached from ordinary user CDDL, where an unmappable head is an input to
    /// refuse gracefully and not a tool bug; the `Err` payload is the untouched type, which the
    /// caller keeps walking over while `finalize` drains the recorded rejection.
    // The `Err` payload is the whole `RustType` on purpose — it is the caller's inert placeholder,
    // and handing it back is what makes "you cannot apply a default without handling the refusal" a
    // type-level property rather than a convention. `clippy::result_large_err` is about hot returns;
    // this one runs once per written `.default` at parse time.
    #[allow(clippy::result_large_err)]
    pub fn try_default(mut self, default_value: FixedValue) -> Result<Self, Self> {
        assert!(self.config.default.is_none());
        let matches = if let ConceptualRustType::Primitive(p) =
            self.conceptual_type.resolve_alias_shallow()
        {
            match &default_value {
                FixedValue::Bool(_) => *p == Primitive::Bool,
                FixedValue::Nint(_) => p.cbor_types().contains(&CBORType::NegativeInteger),
                FixedValue::Uint(_) => p.cbor_types().contains(&CBORType::UnsignedInteger),
                FixedValue::Float(_) => p.is_float(),
                FixedValue::Null => false,
                FixedValue::Undefined => false,
                FixedValue::Text(_) => *p == Primitive::Str,
                FixedValue::Bytes(_) => *p == Primitive::Bytes,
            }
        } else {
            false
        };
        if !matches {
            return Err(self);
        }
        self.config.default = Some(default_value);
        Ok(self)
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
                float_bounds: self.config.float_bounds,
                basic_override: self.config.basic_override,
                duplicates: self.config.duplicates,
            },
        }
    }

    /// Attach a per-rule `@duplicates` policy to a collection member `RustType`. Applied at the
    /// `register_rust_struct` collection arms so it rides the transparent alias to every embed site,
    /// exactly like `with_bounds`. `None`/`Preserve` are no-ops on representation (`Vec` stays
    /// `Vec`); only `Reject` swaps in the uniqueness twin at `for_rust_member`.
    pub fn with_duplicates_policy(
        mut self,
        policy: Option<crate::comment_ast::DuplicatesPolicy>,
    ) -> Self {
        self.config.duplicates = policy;
        self
    }

    /// Attach a NaN-safe float value window (`float64 .le 10.5`, `[f: 0.5..10.5]`). Parallel to
    /// `with_bounds`: a type never carries both integer and float bounds (asserted here). A window
    /// with both sides absent collapses to no bound (returns self unchanged).
    pub fn with_float_bounds(mut self, window: FloatWindow) -> Self {
        assert!(self.config.bounds.is_none());
        assert!(self.config.float_bounds.is_none());
        if window.0.is_some() || window.1.is_some() {
            self.config.float_bounds = Some(window);
        }
        self
    }

    pub fn not_basic(self) -> Self {
        Self {
            conceptual_type: self.conceptual_type,
            encodings: self.encodings,
            config: RustTypeSerializeConfig {
                default: self.config.default,
                bounds: self.config.bounds,
                float_bounds: self.config.float_bounds,
                basic_override: true,
                duplicates: self.config.duplicates,
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

    /// See comment in RustStruct::definite_info(), this is the same, returns a string expression
    /// which evaluates to the length.
    /// self_expr is an expression that evaluates to this RustType (e.g. member, etc) at the point where
    /// the return of this function will be used.
    /// self_is_ref whether the above expression is by-ref
    pub fn definite_info(
        &self,
        self_expr: &str,
        self_is_ref: bool,
        types: &IntermediateTypes,
        cli: &Cli,
    ) -> String {
        match self.expanded_field_count(types) {
            Some(count) => count.to_string(),
            None => match self.conceptual_type.resolve_alias_shallow() {
                ConceptualRustType::Optional(ty) => {
                    // when ty.expanded_field_count is Some, ty.definite_info returns the constant
                    // count.to_string() and never uses the binding, so bind `_` to avoid an
                    // unused-variable warning in the generated match.
                    let (binding, contribution) = match ty.expanded_field_count(types) {
                        Some(count) => ("_", count.to_string()),
                        None => ("x", ty.definite_info("x", true, types, cli)),
                    };
                    format!(
                        "match {}{} {{ Some({}) => {}, None => 1 }}",
                        if self_is_ref { "" } else { "&" },
                        self_expr,
                        binding,
                        contribution
                    )
                }
                ConceptualRustType::Rust(ident) => {
                    if types.is_plain_group(ident) {
                        match types.rust_structs.get(ident) {
                            Some(rs) => rs.definite_info(self_expr, self_is_ref, types, cli),
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
            // C-style enums + extern + raw bytes should all be 1 too, so anything that
            // isn't a plain group falls through to the `_ => 1` arm below.
            ConceptualRustType::Rust(ident) if types.is_plain_group(ident) => {
                match types.rust_structs.get(ident) {
                    Some(x) => x.expanded_mandatory_field_count(types),
                    None => panic!(
                        "rust struct {} not found but referenced by {:?}",
                        ident, self
                    ),
                }
            }
            _ => 1,
        }
    }

    /// All POSSIBLE outermost CBOR types this can encode to
    pub fn cbor_types(&self, types: &IntermediateTypes) -> Vec<CBORType> {
        match self.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_)) => vec![CBORType::Tag],
            // An optionally-tagged value can start as the tag OR as the inner value's own starting
            // types — a two-entry answer type-choice discrimination must tolerate (both arms of the
            // collapse remain distinguishable on the wire).
            Some(CBOREncodingOperation::OptionallyTagged(_)) => {
                let mut inner = self.clone();
                inner.encodings.pop();
                let mut types_out = vec![CBORType::Tag];
                types_out.extend(inner.cbor_types(types));
                types_out
            }
            Some(CBOREncodingOperation::CBORBytes) => vec![CBORType::Bytes],
            None => match &self.conceptual_type {
                ConceptualRustType::Fixed(f) => vec![match f {
                    FixedValue::Uint(_) => CBORType::UnsignedInteger,
                    FixedValue::Nint(_) => CBORType::NegativeInteger,
                    FixedValue::Float(_) => CBORType::Special,
                    FixedValue::Text(_) => CBORType::Text,
                    FixedValue::Bytes(_) => CBORType::Bytes,
                    FixedValue::Null => CBORType::Special,
                    FixedValue::Undefined => CBORType::Special,
                    FixedValue::Bool(_) => CBORType::Special,
                }],
                ConceptualRustType::Primitive(p) => p.cbor_types(),
                // `any` accepts every well-formed CBOR item, so it starts as any major type. A
                // type-choice discriminator must treat it as overlapping everything (which is why
                // `any` is only ever a LAST/catch-all arm — A3; A2 rejects it in choice position).
                ConceptualRustType::Any => vec![
                    CBORType::UnsignedInteger,
                    CBORType::NegativeInteger,
                    CBORType::Bytes,
                    CBORType::Text,
                    CBORType::Array,
                    CBORType::Map,
                    CBORType::Tag,
                    CBORType::Special,
                ],
                ConceptualRustType::Rust(ident) => {
                    let rust_struct = types.rust_struct(ident).unwrap();
                    if rust_struct.tag.is_some() && rust_struct.tag_optional() {
                        // an optionally-tagged collection struct referenced bare (an unresolved
                        // alias): it starts as the tag OR its inner collection type. Mirrors the
                        // `OptionallyTagged` encoding-op arm above so both reference paths agree.
                        let mut types_out = vec![CBORType::Tag];
                        types_out.extend(match rust_struct.variant() {
                            RustStructType::Table { .. } => vec![CBORType::Map],
                            _ => vec![CBORType::Array],
                        });
                        types_out
                    } else if rust_struct.tag.is_some() {
                        vec![CBORType::Tag]
                    } else {
                        match rust_struct.variant() {
                            RustStructType::Wrapper { wrapped, .. } => wrapped.cbor_types(types),
                            // the reserved `int` prelude extern (static prelude `Int`) encodes as a
                            // CBOR uint OR nint — so a type-choice variant dispatch (e.g. `int /
                            // bigint`) must branch on those major types, not the generic-extern
                            // fallback below. Without this a bare `int` variant is dispatched on
                            // Array|Map and never matches its own wire bytes.
                            RustStructType::Extern if ident.to_string() == "Int" => {
                                vec![CBORType::UnsignedInteger, CBORType::NegativeInteger]
                            }
                            // any other extern references user code whose wire shape we can't know
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
                            // a named table/array rule emits no struct of its own — it is a rust
                            // type ALIAS onto a map/vec collection (`pub type Mdmap =
                            // BTreeMap<String, Int>`), so its wire shape is exactly that
                            // collection's: the `Map`/`Array` conceptual arms below. Reached — in
                            // preference to those resolved arms — when a RECURSIVE rule cycle keeps
                            // the reference nominal: the referring rule is handled before the
                            // collection rule registers its alias, so the variant stays a
                            // `Rust(ident)` forward reference naming the registered struct.
                            RustStructType::Table { .. } => vec![CBORType::Map],
                            RustStructType::Array { .. } => vec![CBORType::Array],
                            // NO catch-all arm: every `RustStructType` is spelled out, so a new
                            // variant is a compile error at this site rather than an abort on valid
                            // user input at generation time.
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

    /// The `[+ T]` occurrence shape — lower bound exactly 1, no upper bound — on a homogeneous
    /// ARRAY. This is the original restricted sibling: it becomes `NonEmptyVec<T>`, whose single
    /// `TryFrom<Vec<T>>` door enforces non-emptiness at the type level. Other ordinary/preserve
    /// bounded windows (`2*5`, `*3`, …) use a type-enforced carrier. `@duplicates reject` selects
    /// the compound `BoundedOrderedSet` sibling rather than weakening either invariant at call sites.
    /// Matches the RAW conceptual type (not alias-resolved): a field that *references* a
    /// named `[+ int]` rule carries this bounds shape but is an `Alias`, and its member type must
    /// stay the alias name (whose target is already `NonEmptyVec`), not re-inline the container.
    pub fn is_non_empty_array(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Array(_))
            && self.config.bounds == Some((Some(1), None))
    }

    /// A finite or non-zero-minimum homogeneous ARRAY occurrence whose window is represented by
    /// `BoundedVec` (or `BoundedOrderedSet` for reject sets), rather than a loose `Vec` plus checks
    /// at each construction site. `[+ T]` deliberately remains the older NonEmpty sibling.
    pub fn is_bounded_array(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Array(_))
            && matches!(self.config.bounds, Some(bounds) if bounds != (None, None) && bounds != (Some(1), None))
    }

    /// The const arguments used by `BoundedVec`. Occurrence endpoints are non-negative parse
    /// quantities; retain the checked conversion here so no code generator can silently truncate a
    /// future wider parser carrier.
    pub fn bounded_array_u64_bounds(&self) -> Option<(u64, u64)> {
        if !self.is_bounded_array() {
            return None;
        }
        let (min, max) = self.config.bounds?;
        let min = min.unwrap_or(0).try_into().ok()?;
        let max = max
            .map(|value| value.try_into().ok())
            .unwrap_or(Some(u64::MAX))?;
        (min <= max).then_some((min, max))
    }

    /// Alias-aware counterpart used only for invariant decisions and minting. Naming retains the
    /// raw predicate above so a field referencing a named rule stays that rule's alias.
    pub fn type_enforced_bounded_array_u64_bounds(&self) -> Option<(u64, u64)> {
        matches!(
            self.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Array(_)
        )
        .then_some(())?;
        let (min, max) = self.config.bounds?;
        if (min, max) == (None, None) || (min, max) == (Some(1), None) {
            return None;
        }
        let min = min.unwrap_or(0).try_into().ok()?;
        let max = max
            .map(|value| value.try_into().ok())
            .unwrap_or(Some(u64::MAX))?;
        (min <= max).then_some((min, max))
    }

    /// True when this array-shaped member carries `@duplicates reject` — its representation swaps to
    /// the uniqueness twin (`OrderedSet<T>`, or `NonEmptyOrderedSet<T>` when also `[+]`). Matches the
    /// RAW conceptual type (not alias-resolved), the same convention as `is_non_empty_array`: a field
    /// REFERENCING a named reject rule is an `Alias` whose target already resolves to the twin, so its
    /// member type must stay the alias name rather than re-inline the container.
    pub fn is_reject_ordered_set(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Array(_))
            && self.config.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject)
    }

    /// The compound bounded-unique shape. This intentionally excludes loose `*` and the existing
    /// min-one `+`/`1*` OrderedSet twins, which retain their established carriers.
    pub fn is_bounded_reject_ordered_set(&self) -> bool {
        self.is_reject_ordered_set() && self.is_bounded_array()
    }

    /// Whether this member carries the `@duplicates reject` policy, INDEPENDENT of whether the
    /// conceptual type is a raw `Array` or an alias wrapping one. Unlike `is_reject_ordered_set`
    /// (which requires a raw `Array` — the naming/representation decision), this reads only the config
    /// flag, so it stays true after `resolve_alias` re-wraps the resolved base in an `Alias(...)`
    /// (the config survives that wrap). Used at the generic-instance convergence seams, which work on
    /// alias-resolved `RustType`s.
    pub fn duplicates_reject(&self) -> bool {
        self.config.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject)
    }

    /// Whether this type, at ANY nesting level, contains the `@duplicates reject` `OrderedSet` shape
    /// (so the crate needs the `ordered_set` runtime module + imports). Recurses into container inners
    /// like `contains_non_empty_array`.
    pub fn contains_ordered_set(&self) -> bool {
        if self.is_reject_ordered_set() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_ordered_set()
            }
            ConceptualRustType::Map(k, v) => k.contains_ordered_set() || v.contains_ordered_set(),
            _ => false,
        }
    }

    /// True when this table-shaped member carries `@duplicates preserve` — its representation swaps
    /// from the loose table (`BTreeMap`/`OrderedHashMap`) to the vec-of-pairs twin (`PairMap<K, V>`,
    /// or `NonEmptyPairMap<K, V>` when also `{+}`), the only shape that can hold duplicate keys. A
    /// loose table keyed by key VALUE is structurally incapable of that, so preserve on a table
    /// forces the pair-map member AND the positional preserve-encodings sidecar. Matches the RAW
    /// conceptual type (not alias-resolved), the same convention as `is_reject_ordered_set`: a field
    /// REFERENCING a named preserve table is an `Alias` whose target already resolves to the twin.
    pub fn is_preserve_pair_map(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Map(_, _))
            && self.config.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
    }

    /// Whether this type, at ANY nesting level, contains the `@duplicates preserve` `PairMap` shape
    /// (so the crate needs the `pair_map` runtime module + imports). Recurses into container inners
    /// like `contains_ordered_set`.
    pub fn contains_pair_map(&self) -> bool {
        if self.is_preserve_pair_map() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_pair_map()
            }
            ConceptualRustType::Map(k, v) => k.contains_pair_map() || v.contains_pair_map(),
            _ => false,
        }
    }

    /// The `{+ k => v}` occurrence shape — lower bound exactly 1, no upper bound — on a homogeneous
    /// MAP (table). The map-side twin of `is_non_empty_array`: this becomes `NonEmptyMap<K, V>`, whose
    /// single `TryFrom<{table_type}>` door enforces non-emptiness at the type level. Matches the RAW
    /// conceptual type (not alias-resolved) so a field referencing a named `{+ …}` rule keeps the
    /// alias name (whose target is already `NonEmptyMap`) rather than re-inlining the container.
    pub fn is_non_empty_map(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Map(_, _))
            && self.config.bounds == Some((Some(1), None))
    }

    /// A finite, optional, exact-once, or lower-bounded table represented by a type-enforced bounded
    /// carrier. `@duplicates preserve` selects `BoundedPairMap`; unique-key tables select
    /// `BoundedMap`. `+` retains the compatibility NonEmpty* representations.
    pub fn is_bounded_map(&self) -> bool {
        matches!(self.conceptual_type, ConceptualRustType::Map(_, _))
            && matches!(self.config.bounds, Some(bounds) if bounds != (None, None) && bounds != (Some(1), None))
    }

    pub fn is_bounded_pair_map(&self) -> bool {
        self.is_bounded_map() && self.is_preserve_pair_map()
    }

    pub fn bounded_map_u64_bounds(&self) -> Option<(u64, u64)> {
        if !self.is_bounded_map() {
            return None;
        }
        let (min, max) = self.config.bounds?;
        let min = min.unwrap_or(0).try_into().ok()?;
        let max = max.map(|v| v.try_into().ok()).unwrap_or(Some(u64::MAX))?;
        (min <= max).then_some((min, max))
    }

    /// Alias-aware counterpart used for invariant decisions. Naming deliberately retains the raw
    /// predicate above, so a field referring to a named bounded table keeps its rule-derived name
    /// while the BoundedMap target remains the single occurrence-window enforcement door.
    pub fn type_enforced_bounded_map_u64_bounds(&self) -> Option<(u64, u64)> {
        matches!(
            self.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Map(_, _)
        )
        .then_some(())?;
        let (min, max) = self.config.bounds?;
        if (min, max) == (None, None) || (min, max) == (Some(1), None) {
            return None;
        }
        let min = min.unwrap_or(0).try_into().ok()?;
        let max = max
            .map(|value| value.try_into().ok())
            .unwrap_or(Some(u64::MAX))?;
        (min <= max).then_some((min, max))
    }

    pub fn is_type_enforced_bounded_map(&self) -> bool {
        self.type_enforced_bounded_map_u64_bounds().is_some()
    }

    /// Like `is_non_empty_array`/`is_non_empty_map` but alias-resolving and covering BOTH containers:
    /// true for an inline `[+ T]` / `{+ k => v}` and for a field that *references* a named `[+ …]` /
    /// `{+ …}` rule (an `Alias` whose target is the non-empty container). Used only for the
    /// ENFORCEMENT decision (skip the ctor/setter length check, don't make new() fallible) — the
    /// enforcement lives in the member TYPE (`NonEmptyVec`/`NonEmptyMap`, whether inline or the alias
    /// target). Naming stays on the RAW `is_non_empty_*` so an aliased field keeps its rule-derived
    /// wrapper name rather than synthesizing a structural one.
    pub fn is_type_enforced_non_empty(&self) -> bool {
        matches!(
            self.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
        ) && self.config.bounds == Some((Some(1), None))
    }

    /// Like `is_type_enforced_non_empty`, but for every bounded ARRAY occurrence represented by a
    /// `BoundedVec` or, for `@duplicates reject`, a `BoundedOrderedSet`.
    pub fn is_type_enforced_bounded_array(&self) -> bool {
        matches!(
            self.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Array(_)
        ) && self.type_enforced_bounded_array_u64_bounds().is_some()
    }

    /// Whether this type, at ANY nesting level, contains the `[+ T]` NonEmptyVec shape (so the
    /// crate needs the `non_empty` runtime module + import). Recurses into container inners.
    pub fn contains_non_empty_array(&self) -> bool {
        if self.is_non_empty_array() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_non_empty_array()
            }
            ConceptualRustType::Map(k, v) => {
                k.contains_non_empty_array() || v.contains_non_empty_array()
            }
            _ => false,
        }
    }

    /// Whether any nested position needs the `BoundedVec` runtime module.
    pub fn contains_bounded_array(&self) -> bool {
        if self.is_bounded_array() && !self.is_bounded_reject_ordered_set() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_bounded_array()
            }
            ConceptualRustType::Map(k, v) => {
                k.contains_bounded_array() || v.contains_bounded_array()
            }
            _ => false,
        }
    }

    /// Whether this type, at ANY nesting level, contains the `{+ k => v}` NonEmptyMap shape (so the
    /// crate needs the `non_empty_map` runtime module + import). Recurses into container inners.
    pub fn contains_non_empty_map(&self) -> bool {
        if self.is_non_empty_map() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_non_empty_map()
            }
            ConceptualRustType::Map(k, v) => {
                k.contains_non_empty_map() || v.contains_non_empty_map()
            }
            _ => false,
        }
    }

    /// Whether any nested position needs the unique-key BoundedMap runtime module.
    pub fn contains_bounded_map(&self) -> bool {
        if self.is_bounded_map() && !self.is_bounded_pair_map() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_bounded_map()
            }
            ConceptualRustType::Map(k, v) => k.contains_bounded_map() || v.contains_bounded_map(),
            _ => false,
        }
    }

    /// Whether any nested position needs the bounded duplicate-preserving pair-map carrier.
    pub fn contains_bounded_pair_map(&self) -> bool {
        if self.is_bounded_pair_map() {
            return true;
        }
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                inner.contains_bounded_pair_map()
            }
            ConceptualRustType::Map(k, v) => {
                k.contains_bounded_pair_map() || v.contains_bounded_pair_map()
            }
            _ => false,
        }
    }

    /// Whether this type, at ANY nesting level, contains CDDL `any` (the `AnyCbor` runtime type), so
    /// `export`/import wiring pulls in the `any_cbor` runtime module + `AnyCbor` import only for
    /// crates that need it (keeping every non-`any` crate's output byte-identical). Recurses into
    /// container inners AND the `Alias` base (a top-level `x = any` registers a transparent alias
    /// whose base is `Any`).
    pub fn contains_any_cbor(&self) -> bool {
        self.conceptual_type.contains_any_cbor()
    }

    /// Whether this type carries a value window (integer OR float) that a constructor/setter must
    /// enforce. Used to decide constructor fallibility at inline (field/variant/ctor) sites.
    pub fn has_value_bounds(&self) -> bool {
        // The NonEmptyVec (`[+ T]`) shape enforces its `>= 1` bound at the type level via a single
        // TryFrom door, so it emits NO constructor/setter length check and does NOT make new()
        // fallible — the invalid (empty) state is unrepresentable, not runtime-rejected. Covers both
        // inline `[+ T]` and a field referencing a named `[+ …]` rule (alias-resolving).
        if self.is_type_enforced_non_empty()
            || self.is_type_enforced_bounded_array()
            || self.is_type_enforced_bounded_map()
        {
            return false;
        }
        self.config.bounds.is_some() || self.config.float_bounds.is_some()
    }

    pub fn needs_bounds_check_if_inlined(&self, types: &IntermediateTypes) -> bool {
        self.has_value_bounds()
            || match self.resolve_alias_shallow() {
                ConceptualRustType::Rust(ident) => types.can_new_fail(ident),
                _ => false,
            }
    }

    /// The wasm-boundary name of the restricted list wrapper for a `[+ elem]` array. When a NAMED
    /// `[+ elem]` rule of the same element exists, the inline use DEDUPS to that rule's class (the
    /// spec author's name wins — see `IntermediateTypes::non_empty_named_owner`); otherwise a
    /// `NonEmpty<Elem>List` class is synthesized (`NonEmptyBarList` for `[+ bar]`). Used both to
    /// REFERENCE the wrapper (parent ctor/setter param, getter return) and to decide MINTING, so
    /// the two sites can never disagree. Named `[+ …]` rules themselves don't route through here —
    /// they keep their rule ident as the wrapper name.
    pub fn non_empty_wasm_wrapper_name(&self, types: &IntermediateTypes) -> String {
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) => match types.non_empty_named_owner(inner) {
                Some(owner) => owner.to_string(),
                // LOCKSTEP: `generate_non_empty_array_type`'s defer-candidate structural name
                // duplicates THIS spelling on purpose (it must stay owner-independent — an
                // owner-named wrapper must never look deferrable). Change both together.
                None => format!("NonEmpty{}List", inner.wasm_structural_variant(types)),
            },
            _ => unreachable!("non_empty_wasm_wrapper_name on a non-array: {:?}", self),
        }
    }

    /// Mechanical restricted-list wasm name for a bounded homogeneous array. A named rule wins;
    /// anonymous occurrences carry both sides of their inclusive window in the class name.
    pub fn bounded_wasm_wrapper_name(&self, types: &IntermediateTypes) -> String {
        let ConceptualRustType::Array(inner) = &self.conceptual_type else {
            unreachable!("bounded_wasm_wrapper_name on a non-array: {:?}", self);
        };
        let bounds = self
            .config
            .bounds
            .expect("bounded array has occurrence bounds");
        if let Some(owner) = types.bounded_array_named_owner(inner, bounds) {
            return owner.to_string();
        }
        let (min, max) = self
            .bounded_array_u64_bounds()
            .expect("bounded wasm wrapper has representable bounds");
        let base = inner.wasm_structural_variant(types);
        match (min, max == u64::MAX) {
            (0, false) => format!("{base}ListMax{max}"),
            (_, true) => format!("{base}ListMin{min}"),
            _ => format!("{base}ListMin{min}Max{max}"),
        }
    }

    /// The wasm-boundary name of the restricted uniqueness-twin wrapper for a `@duplicates reject`
    /// set. The reject analog of `non_empty_wasm_wrapper_name`: an inline (anonymous generic-instance)
    /// reject set has no author rule name, so it synthesizes a structural `<Elem>OrderedSet` class
    /// (`U64OrderedSet` for `[* uint]`), or `NonEmpty<Elem>OrderedSet` when the rule is also `[+]` (its
    /// door composes uniqueness with the min-1 bound). Used both to REFERENCE the wrapper (parent
    /// getter/param) and to decide MINTING (`ensure_non_empty_wrappers`'s reject arm), so the two sites
    /// can never disagree. NAMED reject rules keep their rule ident as the wrapper name and never route
    /// through here (like the NonEmpty twin, the raw-`Array` `is_reject_ordered_set` gate leaves an
    /// aliased field on its rule-derived name).
    pub fn reject_ordered_set_wasm_wrapper_name(&self, types: &IntermediateTypes) -> String {
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) => {
                let variant = inner.wasm_structural_variant(types);
                if self.is_non_empty_array() {
                    format!("NonEmpty{variant}OrderedSet")
                } else {
                    format!("{variant}OrderedSet")
                }
            }
            _ => unreachable!(
                "reject_ordered_set_wasm_wrapper_name on a non-array: {:?}",
                self
            ),
        }
    }

    /// Structural wasm name for a bounded `@duplicates reject` array. Bounds and the uniqueness
    /// flavor are both part of the name, so it cannot collide with either a bounded loose list or
    /// a loose/non-empty ordered-set wrapper of the same element type.
    pub fn bounded_reject_ordered_set_wasm_wrapper_name(
        &self,
        types: &IntermediateTypes,
    ) -> String {
        let (min, max) = self
            .bounded_array_u64_bounds()
            .expect("bounded reject set has representable occurrence bounds");
        let ConceptualRustType::Array(inner) = &self.conceptual_type else {
            unreachable!("bounded_reject_ordered_set_wasm_wrapper_name on a non-array");
        };
        let base = inner.wasm_structural_variant(types);
        match (min, max == u64::MAX) {
            (0, false) => format!("{base}BoundedOrderedSetMax{max}"),
            (_, true) => format!("{base}BoundedOrderedSetMin{min}"),
            _ => format!("{base}BoundedOrderedSetMin{min}Max{max}"),
        }
    }

    /// The wasm-boundary name of the restricted map wrapper for a `{+ k => v}` table. When a NAMED
    /// `{+ k => v}` rule of the same domain/range exists, the inline use DEDUPS to that rule's class
    /// (the spec author's name wins — see `IntermediateTypes::non_empty_map_named_owner`); otherwise a
    /// `NonEmpty<MapKToV>` class is synthesized (`NonEmptyMapTextToUint` for `{+ text => uint}`, and
    /// `NonEmptyPairMapTextToUint` for its `@duplicates preserve` twin — the container flavor composes
    /// with the min-1 prefix). The map-side twin of `non_empty_wasm_wrapper_name`. Named `{+ …}` rules
    /// keep their rule ident.
    pub fn non_empty_wasm_map_wrapper_name(&self, types: &IntermediateTypes) -> String {
        match &self.conceptual_type {
            ConceptualRustType::Map(k, v) => match types.non_empty_map_named_owner(k, v) {
                Some(owner) => owner.to_string(),
                // LOCKSTEP: `generate_non_empty_map_type`'s defer-candidate structural name
                // duplicates THIS spelling on purpose (it must stay owner-independent — an
                // owner-named wrapper must never look deferrable). Change both together.
                None => format!(
                    "NonEmpty{}",
                    Self::name_for_wasm_map(types, k, v, self.is_preserve_pair_map())
                ),
            },
            _ => unreachable!("non_empty_wasm_map_wrapper_name on a non-map: {:?}", self),
        }
    }

    /// Mechanical wasm name for a finite/exact table. Both bounds and duplicate policy are part of
    /// the structural identity, so a loose source can never be mistaken for this wrapper.
    pub fn bounded_wasm_map_structural_name(&self, types: &IntermediateTypes) -> String {
        let (min, max) = self
            .bounded_map_u64_bounds()
            .expect("bounded map wasm wrapper has representable bounds");
        let base = match &self.conceptual_type {
            ConceptualRustType::Map(k, v) => {
                Self::name_for_wasm_map(types, k, v, self.is_preserve_pair_map())
            }
            _ => unreachable!("bounded_wasm_map_wrapper_name on a non-map"),
        };
        match (min, max == u64::MAX) {
            (0, false) => format!("{base}Max{max}"),
            (_, true) => format!("{base}Min{min}"),
            _ => format!("{base}Min{min}Max{max}"),
        }
    }

    /// Bounded maps follow bounded arrays' owner rule: a matching authored table owns an inline
    /// occurrence's wasm surface, otherwise the structural `MapKToVMinN/MaxN` name is minted.
    pub fn bounded_wasm_map_wrapper_name(&self, types: &IntermediateTypes) -> String {
        let ConceptualRustType::Map(key, value) = &self.conceptual_type else {
            unreachable!("bounded_wasm_map_wrapper_name on a non-map");
        };
        let bounds = self
            .config
            .bounds
            .expect("bounded map has occurrence bounds");
        types
            .bounded_map_named_owner(key, value, bounds, self.is_preserve_pair_map())
            .map(ToString::to_string)
            .unwrap_or_else(|| self.bounded_wasm_map_structural_name(types))
    }

    /// The wasm-boundary name of the LOOSE `@duplicates preserve` map wrapper (`PairMapKToV`) — the
    /// pair-map twin of the default `MapKToV` structural class. Unlike the NonEmpty/reject twins there
    /// is no dedup-to-named lookup: a named preserve `{* …}` table that SOLELY owns the shape has its
    /// class minted under the rule ident with a `pub type PairMapKToV = <Owner>;` alias beside it
    /// (`mint_sole_owner_table`), so every reference site names the structural spelling and
    /// wasm-bindgen folds it onto the owner class.
    pub fn preserve_pair_map_wasm_wrapper_name(&self, types: &IntermediateTypes) -> String {
        match &self.conceptual_type {
            ConceptualRustType::Map(k, v) => Self::name_for_wasm_map(types, k, v, true).to_string(),
            _ => unreachable!(
                "preserve_pair_map_wasm_wrapper_name on a non-map: {:?}",
                self
            ),
        }
    }

    // --- Bounds-aware type-naming/boundary wrappers (RustType level) -------------------------------
    // `RustType` Derefs to `ConceptualRustType`, but `config.bounds` lives on `RustType`, so the
    // conceptual `*_ct` methods below can't see the `[+ T]` shape. These inherent methods consult
    // `config.bounds` and pick `NonEmptyVec`/`NonEmpty*List` for that one shape, recursing at the
    // RustType level so nested `[+ [+ int]]` bounds are each honored; everything else delegates to
    // the raw conceptual `*_ct` method (element iteration / encoding-var plumbing stay raw).

    /// Type when stored inside a rust struct (member/alias/param). Bounds-aware over `for_rust_member_ct`.
    pub fn for_rust_member(&self, types: &IntermediateTypes, from_wasm: bool, cli: &Cli) -> String {
        match &self.conceptual_type {
            ConceptualRustType::Array(inner) => {
                let element = inner.for_rust_member(types, from_wasm, cli);
                if let Some((min, max)) = self.bounded_array_u64_bounds() {
                    let max = if max == u64::MAX {
                        "{ u64::MAX }".to_owned()
                    } else {
                        max.to_string()
                    };
                    let carrier = if self.is_bounded_reject_ordered_set() {
                        "BoundedOrderedSet"
                    } else {
                        "BoundedVec"
                    };
                    return format!("{carrier}<{element}, {min}, {max}>");
                }
                format!(
                    "{}<{element}>",
                    match (self.is_reject_ordered_set(), self.is_non_empty_array()) {
                        // `@duplicates reject`: the uniqueness twin (order-preserving), non-empty flavor
                        // when the rule is also `[+]` (its door composes uniqueness + the min-1 check).
                        (true, true) => "NonEmptyOrderedSet",
                        (true, false) => "OrderedSet",
                        (false, true) => "NonEmptyVec",
                        (false, false) => "Vec",
                    }
                )
            }
            ConceptualRustType::Optional(inner) => {
                format!("Option<{}>", inner.for_rust_member(types, from_wasm, cli))
            }
            ConceptualRustType::Map(k, v)
                if self.is_preserve_pair_map()
                    || self.is_non_empty_map()
                    || self.is_bounded_map() =>
            {
                if let Some((min, max)) = self.bounded_map_u64_bounds() {
                    let max = if max == u64::MAX {
                        "{ u64::MAX }".to_owned()
                    } else {
                        max.to_string()
                    };
                    let carrier = if self.is_bounded_pair_map() {
                        "BoundedPairMap"
                    } else {
                        "BoundedMap"
                    };
                    return format!(
                        "{carrier}<{}, {}, {min}, {max}>",
                        k.for_rust_member(types, from_wasm, cli),
                        v.for_rust_member(types, from_wasm, cli)
                    );
                }
                let table = match (self.is_preserve_pair_map(), self.is_non_empty_map()) {
                    // `@duplicates preserve`: the vec-of-pairs twin (duplicate-permitting), non-empty
                    // flavor when the rule is also `{+}` (its door composes the min-1 check).
                    (true, true) => "NonEmptyPairMap",
                    (true, false) => "PairMap",
                    (false, true) => "NonEmptyMap",
                    // unreachable given the arm guard, but keeps the match total.
                    (false, false) => unreachable!(),
                };
                format!(
                    "{table}<{}, {}>",
                    k.for_rust_member(types, from_wasm, cli),
                    v.for_rust_member(types, from_wasm, cli)
                )
            }
            _ => self
                .conceptual_type
                .for_rust_member_ct(types, from_wasm, cli),
        }
    }

    /// Function parameter TYPE that will be moved in. Bounds-aware over `for_rust_move_ct`.
    pub fn for_rust_move(&self, types: &IntermediateTypes, cli: &Cli) -> String {
        self.for_rust_member(types, false, cli)
    }

    /// If we were to store a value directly in a wasm-wrapper, this would be used. Bounds-aware.
    pub fn for_wasm_member(&self, types: &IntermediateTypes) -> String {
        if self.is_bounded_reject_ordered_set() {
            return self.bounded_reject_ordered_set_wasm_wrapper_name(types);
        }
        if self.is_reject_ordered_set() {
            return self.reject_ordered_set_wasm_wrapper_name(types);
        }
        if self.is_non_empty_array() {
            return self.non_empty_wasm_wrapper_name(types);
        }
        if self.is_bounded_array() {
            return self.bounded_wasm_wrapper_name(types);
        }
        if self.is_non_empty_map() {
            return self.non_empty_wasm_map_wrapper_name(types);
        }
        if self.is_bounded_map() {
            return self.bounded_wasm_map_wrapper_name(types);
        }
        // `@duplicates preserve` loose map: the flavored structural class (`PairMapKToV`). The
        // conceptual `for_wasm_member_ct` below cannot see the policy, so the flavor branch lives here
        // — the same seam `is_non_empty_map` uses for the occurrence bound.
        if self.is_preserve_pair_map() {
            return self.preserve_pair_map_wasm_wrapper_name(types);
        }
        if let ConceptualRustType::Map(k, v) = &self.conceptual_type {
            return Self::name_for_wasm_map(types, k, v, false).to_string();
        }
        match &self.conceptual_type {
            ConceptualRustType::Optional(inner) => {
                format!("Option<{}>", inner.for_wasm_member(types))
            }
            _ => self.conceptual_type.for_wasm_member_ct(types),
        }
    }

    /// Return TYPE for wasm. Bounds-aware over `for_wasm_return_ct`.
    pub fn for_wasm_return(&self, types: &IntermediateTypes) -> String {
        self.for_wasm_member(types)
    }

    /// Function parameter TYPE from wasm (ref for non-primitives). Bounds-aware over `for_wasm_param_ct`.
    pub fn for_wasm_param(&self, types: &IntermediateTypes) -> String {
        if self.is_bounded_reject_ordered_set() {
            return format!(
                "&{}",
                self.bounded_reject_ordered_set_wasm_wrapper_name(types)
            );
        }
        if self.is_reject_ordered_set() {
            return format!("&{}", self.reject_ordered_set_wasm_wrapper_name(types));
        }
        if self.is_non_empty_array() {
            return format!("&{}", self.non_empty_wasm_wrapper_name(types));
        }
        if self.is_bounded_array() {
            return format!("&{}", self.bounded_wasm_wrapper_name(types));
        }
        if self.is_non_empty_map() {
            return format!("&{}", self.non_empty_wasm_map_wrapper_name(types));
        }
        if self.is_bounded_map() {
            return format!("&{}", self.bounded_wasm_map_wrapper_name(types));
        }
        if self.is_preserve_pair_map() {
            return format!("&{}", self.preserve_pair_map_wasm_wrapper_name(types));
        }
        if let ConceptualRustType::Map(k, v) = &self.conceptual_type {
            return format!("&{}", Self::name_for_wasm_map(types, k, v, false));
        }
        match &self.conceptual_type {
            ConceptualRustType::Optional(inner) => {
                format!("Option<{}>", inner.for_wasm_param_impl_rt(types))
            }
            _ => self.conceptual_type.for_wasm_param_ct(types),
        }
    }

    /// Optional-inner variant of `for_wasm_param` (no leading `&`), bounds-aware.
    fn for_wasm_param_impl_rt(&self, types: &IntermediateTypes) -> String {
        if self.is_bounded_reject_ordered_set() {
            return self.bounded_reject_ordered_set_wasm_wrapper_name(types);
        }
        if self.is_reject_ordered_set() {
            return self.reject_ordered_set_wasm_wrapper_name(types);
        }
        if self.is_non_empty_array() {
            return self.non_empty_wasm_wrapper_name(types);
        }
        if self.is_bounded_array() {
            return self.bounded_wasm_wrapper_name(types);
        }
        if self.is_non_empty_map() {
            return self.non_empty_wasm_map_wrapper_name(types);
        }
        if self.is_bounded_map() {
            return self.bounded_wasm_map_wrapper_name(types);
        }
        if self.is_preserve_pair_map() {
            return self.preserve_pair_map_wasm_wrapper_name(types);
        }
        if let ConceptualRustType::Map(k, v) = &self.conceptual_type {
            return Self::name_for_wasm_map(types, k, v, false).to_string();
        }
        self.conceptual_type.for_wasm_param_impl(types, true)
    }

    /// Whether the type crosses the wasm boundary as a bare value (not via a wrapper). A NonEmpty
    /// array is ALWAYS wrapped (restricted `NonEmpty*List`), so it is never directly exposable.
    pub fn directly_wasm_exposable(&self, types: &IntermediateTypes) -> bool {
        // A `[+ T]` (NonEmptyVec) or a `@duplicates reject` set (OrderedSet/NonEmptyOrderedSet) always
        // crosses the wasm boundary through its restricted wrapper class, never as a bare `Vec`: the
        // bare form would drop the invariant AND mismatch the rust core type. Same reason as `[+ T]`.
        if self.is_non_empty_array()
            || self.is_bounded_array()
            || self.is_reject_ordered_set()
            || self.is_bounded_map()
        {
            return false;
        }
        self.conceptual_type.directly_wasm_exposable_ct(types)
    }

    /// `self` is the ELEMENT type; whether a BARE `Vec<self>` is legal in a `#[wasm_bindgen]`
    /// signature. This — not `directly_wasm_exposable` — is the question every list-taking DOOR
    /// (`try_from(elements: Vec<Elem>)`) must ask, because wasm-bindgen exposing a scalar does NOT
    /// imply it exposes a vector of it: `bytes` is already `Vec<u8>` (nesting is unrepresentable) and
    /// `bool` has no `VectorFromWasmAbi`. Asking the element's own question there emitted
    /// `try_from(elements: Vec<Vec<u8>>)`, which generates at exit 0 and then fails the generated
    /// wasm crate's own compile with `E0271 … <Vec<u8> as ErasableGeneric>::Repr == JsValue`.
    /// Spelled as the ARRAY-level probe so it stays in lockstep with `name_as_wasm_array`, which
    /// names the loose `<Elem>List` wrapper under exactly the negation of this test — so a door that
    /// falls out of the bare-`Vec` arm always has a loose class to borrow instead. The bounds-aware
    /// element cases are subsumed: `[+ …]` / `@duplicates reject` both require an `Array` conceptual
    /// type, which the array-level probe already rejects as a nested vec.
    pub fn vec_of_self_directly_wasm_exposable(&self, types: &IntermediateTypes) -> bool {
        ConceptualRustType::Array(Box::new(self.clone())).directly_wasm_exposable_ct(types)
    }

    /// `self` is the ELEMENT type; this names the LOOSE `Vec`-of-`self` wrapper (`BarList`,
    /// `ArrIntList`). It is NOT the outer container's nonempty/bounded name. A restricted ELEMENT,
    /// however, must contribute its own boundary carrier to the name: `[* [2*5 uint]]` stores
    /// `BoundedVec<u64, 2, 5>` and therefore uses `U64ListMin2Max5List`, not the `ArrU64List` a
    /// loose `Vec<Vec<u64>>` owns. This keeps table `keys()` wrappers and structural map builders
    /// one-name/one-native-type while still unifying constraints that share a carrier.
    pub fn name_as_wasm_array(&self, types: &IntermediateTypes) -> String {
        let carrier = self.wasm_structural_variant(types);
        if carrier == self.conceptual_type.for_variant().to_string() {
            self.conceptual_type.name_as_wasm_array_ct(types)
        } else {
            format!("{carrier}List")
        }
    }

    /// The wasm wrapper carrier this Rust type contributes when nested inside another structural
    /// wrapper name. Unlike `ConceptualRustType::for_variant`, this preserves occurrence bounds and
    /// duplicate policy whenever they change the native stored type.
    pub fn wasm_structural_variant(&self, types: &IntermediateTypes) -> String {
        if self.is_bounded_reject_ordered_set() {
            self.bounded_reject_ordered_set_wasm_wrapper_name(types)
        } else if self.is_reject_ordered_set() {
            self.reject_ordered_set_wasm_wrapper_name(types)
        } else if self.is_non_empty_array() {
            self.non_empty_wasm_wrapper_name(types)
        } else if self.is_bounded_array() {
            self.bounded_wasm_wrapper_name(types)
        } else if self.is_non_empty_map() {
            self.non_empty_wasm_map_wrapper_name(types)
        } else if self.is_bounded_map() {
            self.bounded_wasm_map_wrapper_name(types)
        } else if self.is_preserve_pair_map() {
            self.preserve_pair_map_wasm_wrapper_name(types)
        } else {
            match &self.conceptual_type {
                ConceptualRustType::Array(inner) => {
                    let inner_variant = inner.wasm_structural_variant(types);
                    if inner_variant == inner.conceptual_type.for_variant().to_string() {
                        self.conceptual_type.for_variant().to_string()
                    } else {
                        format!("Arr{inner_variant}")
                    }
                }
                ConceptualRustType::Optional(inner) => {
                    let inner_variant = inner.wasm_structural_variant(types);
                    if inner_variant == inner.conceptual_type.for_variant().to_string() {
                        self.conceptual_type.for_variant().to_string()
                    } else {
                        format!("Opt{inner_variant}")
                    }
                }
                ConceptualRustType::Map(k, v) => {
                    Self::name_for_wasm_map(types, k, v, false).to_string()
                }
                _ => self.conceptual_type.for_variant().to_string(),
            }
        }
    }

    /// Bounds/policy-aware structural map name. The conceptual predecessor remains for contexts
    /// that truly have only conceptual types; wasm wrapper ownership and emission use this form so
    /// two equal names always imply equal native key/value carriers.
    pub fn name_for_wasm_map(
        types: &IntermediateTypes,
        key: &RustType,
        value: &RustType,
        preserve: bool,
    ) -> RustIdent {
        RustIdent::new(CDDLIdent::new(format!(
            "{}Map{}To{}",
            if preserve { "Pair" } else { "" },
            key.wasm_structural_variant(types),
            value.wasm_structural_variant(types)
        )))
    }

    /// `self` is the ELEMENT type; this is the `Vec<element>` rust type. Bounds-aware over the
    /// ELEMENT (a nonempty element becomes `Vec<NonEmptyVec<..>>`), but never wraps `self` itself in
    /// `NonEmptyVec` — the container's nonemptiness is decided by `for_rust_member` on the array.
    pub fn name_as_rust_array(
        &self,
        types: &IntermediateTypes,
        from_wasm: bool,
        cli: &Cli,
    ) -> String {
        format!("Vec<{}>", self.for_rust_member(types, from_wasm, cli))
    }

    /// FROM rust TO wasm (getter/return). A NonEmpty array OR a `@duplicates reject` set crosses as
    /// its restricted wrapper class (`NonEmpty*List` / `*OrderedSet`), never as a bare `Vec` — so the
    /// core value is cloned and `.into()`'d into that wrapper. A plain `[*]` reject set is a reject
    /// set but NOT a non-empty array, so it needs its own arm here (the sibling type-name methods
    /// `for_wasm_member`/`for_wasm_param`/`directly_wasm_exposable` already treat both the same way).
    pub fn to_wasm_boundary(&self, types: &IntermediateTypes, expr: &str, is_ref: bool) -> String {
        if self.is_non_empty_array()
            || self.is_bounded_array()
            || self.is_reject_ordered_set()
            || self.is_bounded_map()
        {
            return format!("{expr}.clone().into()");
        }
        self.conceptual_type.to_wasm_boundary(types, expr, is_ref)
    }

    pub fn to_wasm_boundary_optional(
        &self,
        types: &IntermediateTypes,
        expr: &str,
        is_ref: bool,
    ) -> String {
        if self.is_non_empty_array()
            || self.is_bounded_array()
            || self.is_reject_ordered_set()
            || self.is_bounded_map()
        {
            return format!("{expr}.clone().map(std::convert::Into::into)");
        }
        self.conceptual_type
            .to_wasm_boundary_optional(types, expr, is_ref)
    }

    /// FROM wasm TO rust (owning). A NonEmpty array OR a `@duplicates reject` set is handed over as
    /// `&NonEmpty*List` / `&*OrderedSet` and cloned + `.into()`'d into the core `NonEmptyVec` /
    /// `OrderedSet` (the wrapper's `From`/`AsRef` conversion methods). A plain `[*]` reject set is a
    /// reject set but NOT a non-empty array, so it needs its own arm here.
    #[allow(clippy::wrong_self_convention)]
    pub fn from_wasm_boundary_clone(
        &self,
        types: &IntermediateTypes,
        expr: &str,
        can_fail: bool,
    ) -> Vec<ToWasmBoundaryOperations> {
        if self.is_non_empty_array()
            || self.is_bounded_array()
            || self.is_reject_ordered_set()
            || self.is_bounded_map()
        {
            let mut ops = vec![
                ToWasmBoundaryOperations::Code(format!("{expr}.clone()")),
                ToWasmBoundaryOperations::Into,
            ];
            if can_fail {
                ops.push(ToWasmBoundaryOperations::TryInto);
            }
            return ops;
        }
        self.conceptual_type
            .from_wasm_boundary_clone(types, expr, can_fail)
    }

    /// FROM wasm as non-owning ref. A NonEmpty-array OR `@duplicates reject`-set wrapper is passed
    /// by-ref unchanged (both cross as `&Wrapper`).
    #[allow(clippy::wrong_self_convention)]
    pub fn from_wasm_boundary_ref(&self, types: &IntermediateTypes, expr: &str) -> String {
        if self.is_non_empty_array() || self.is_bounded_array() || self.is_reject_ordered_set() {
            return expr.to_owned();
        }
        self.conceptual_type.from_wasm_boundary_ref(types, expr)
    }
}

impl std::convert::From<ConceptualRustType> for RustType {
    fn from(conceptual_type: ConceptualRustType) -> Self {
        Self::new(conceptual_type)
    }
}

/// How a type will be represented in rust outside of a serialization context
///
/// Adding a variant: there is no compilable intermediate state — the exhaustive match sites
/// (~24 across the emitters/IR) error loudly, but this file alone has ~30 wildcard `_ =>` arms
/// that will silently swallow a new variant with whatever default they encode. Hand-audit every
/// wildcard method ("is this default right for the new variant?") before trusting a green build,
/// and plan the first commit at variant + all arms + emission granularity — reachable emitter
/// arms can't be stubbed without breaking snapshots. (Learned on the A2 `Any` delivery, `7a08a0f`.)
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
    // CDDL `any` — a structured, self-describing CBOR value lowered to the static-runtime
    // `AnyCbor` type. Self-carried encodings (contributes no owner encoding fields). The rust
    // token is import-glued (`<common>::any_cbor::AnyCbor`) via `for_rust_member`.
    Any,
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

    pub fn directly_wasm_exposable_ct(&self, types: &IntermediateTypes) -> bool {
        self.directly_wasm_exposable_ct_with_aliases(types, &mut BTreeSet::new())
    }

    /// Whether a named transparent alias resolves to a directly wasm-exposable structural type.
    ///
    /// `finalize` can register an alias after an earlier use-site retained its nominal `Rust(ident)`
    /// spelling. Follow that late alias here, at the shared naming seam, rather than making each
    /// wrapper/collision caller guess how to resolve it. The per-query set is intentionally a path
    /// guard, not a memo: revisiting an alias means a recursive collection would cross the wasm
    /// boundary as a bare vector, which wasm-bindgen cannot expose, so `false` is the safe answer.
    ///
    /// An ident in neither the struct nor alias registries is NOT this case. It must still reach
    /// `is_enum` below so the registered-or-generic assertion catches genuinely dangling names.
    fn directly_wasm_exposable_ct_with_aliases(
        &self,
        types: &IntermediateTypes,
        aliases_being_followed: &mut BTreeSet<RustIdent>,
    ) -> bool {
        match self {
            Self::Fixed(_) => false,
            Self::Primitive(_) => true,
            // `AnyCbor` is a static-runtime type exposed through a wasm wrapper class, not directly
            // wasm-bindgen-exposable — like a Rust wrapper struct.
            Self::Any => false,
            Self::Rust(ident) => match types.rust_struct(ident).map(|rs| rs.variant()) {
                Some(RustStructType::CStyleEnum { .. }) => true,
                Some(_) => false,
                None => {
                    if let Some(alias) = types.type_aliases().get(&AliasIdent::Rust(ident.clone()))
                    {
                        Self::follow_alias_for_wasm_exposability(
                            ident,
                            &alias.base_type,
                            types,
                            aliases_being_followed,
                        )
                    } else {
                        // Keep `is_enum`'s generic-instance assertion live for a genuinely unknown
                        // `Rust(ident)`; only a registered alias is legal to resolve structurally.
                        types.is_enum(ident)
                    }
                }
            },
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
                        Primitive::Float
                        | Primitive::F16
                        | Primitive::F32
                        | Primitive::F64
                        | Primitive::F16To32
                        | Primitive::F32To64
                        | Primitive::I8
                        | Primitive::U8
                        | Primitive::I16
                        | Primitive::U16
                        | Primitive::I32
                        | Primitive::U32
                        | Primitive::I64
                        | Primitive::N64
                        | Primitive::U64 => true,
                        // NOT a js number: wasm-bindgen has no VectorIntoWasmAbi for bool, so a
                        // bare Vec<bool> return/param fails E0271 — it needs a BoolList wrapper
                        // (ex: hit by a bool-keyed table's keys() accessor; see special_map_key)
                        Primitive::Bool => false,
                        // Bytes is already implemented as Vec<u8> so we can't nest it
                        Primitive::Bytes => false,
                        // directly exposable: wasm-bindgen supports Vec<String> in parameter and
                        // return position (a JS string array), and strings are copied at the
                        // boundary, so the ownership hazard justifying struct *List wrappers
                        // does not apply
                        Primitive::Str => true,
                    },
                    Self::Array(_) => false,
                    _ => ty
                        .conceptual_type
                        .directly_wasm_exposable_ct_with_aliases(types, aliases_being_followed),
                }
            }
            Self::Optional(ty) => ty
                .conceptual_type
                .directly_wasm_exposable_ct_with_aliases(types, aliases_being_followed),
            Self::Map(_, _) => false,
            Self::Alias(ident, ty) => match ident {
                // reserved aliases (uint→u64, …) generate no wrapper — they ARE the raw type, unwrap
                AliasIdent::Reserved(_) => {
                    ty.directly_wasm_exposable_ct_with_aliases(types, aliases_being_followed)
                }
                // Whether a named alias is directly exposable turns on whether `ident` is emitted as a
                // `#[wasm_bindgen]` WRAPPER struct (a generated `RustStruct`, e.g. `nums = [* uint]`)
                // or a transparent `pub type` alias (no generated struct — a passthrough `arr2 = arr`,
                // or a `foo_bytes = bytes .cbor foo` transparent to the wrapper `Foo`). A wrapper is
                // NOT directly exposable. The bug we avoid is recursing into the inlined inner unconditionally:
                // for `nums` the inner `Vec<u64>` is exposable, so the *wrapper* `Nums` was wrongly
                // called exposable and boundary conversions were dropped. For a transparent alias (or a
                // re-exported c-style enum) we DO follow what it aliases — so `arr2 = arr` (→ exposable
                // `Vec<u64>`) and `foo_bytes` (→ the wrapper `Foo`, not exposable) each resolve right.
                AliasIdent::Rust(rust_ident) => {
                    match types.rust_struct(rust_ident).map(|rs| rs.variant()) {
                        Some(RustStructType::CStyleEnum { .. }) => ty
                            .directly_wasm_exposable_ct_with_aliases(types, aliases_being_followed),
                        Some(_) => false,
                        None => types
                            .type_aliases()
                            .get(&AliasIdent::Rust(rust_ident.clone()))
                            .map(|alias| {
                                Self::follow_alias_for_wasm_exposability(
                                    rust_ident,
                                    &alias.base_type,
                                    types,
                                    aliases_being_followed,
                                )
                            })
                            // An `Alias` node normally originates from the alias registry. Retain
                            // the old inlined-base behavior if an internal caller supplied one that
                            // has no registered owner; only a bare `Rust(ident)` may reach `is_enum`.
                            .unwrap_or_else(|| {
                                ty.directly_wasm_exposable_ct_with_aliases(
                                    types,
                                    aliases_being_followed,
                                )
                            }),
                    }
                }
            },
        }
    }

    fn follow_alias_for_wasm_exposability(
        ident: &RustIdent,
        base_type: &RustType,
        types: &IntermediateTypes,
        aliases_being_followed: &mut BTreeSet<RustIdent>,
    ) -> bool {
        if !aliases_being_followed.insert(ident.clone()) {
            return false;
        }
        let result = base_type
            .conceptual_type
            .directly_wasm_exposable_ct_with_aliases(types, aliases_being_followed);
        aliases_being_followed.remove(ident);
        result
    }

    pub fn is_fixed_value(&self) -> bool {
        match self {
            Self::Fixed(_) => true,
            Self::Alias(_ident, ty) => ty.is_fixed_value(),
            _ => false,
        }
    }

    /// See [`RustType::contains_any_cbor`]. Recurses container inners and the `Alias` base.
    pub fn contains_any_cbor(&self) -> bool {
        match self {
            Self::Any => true,
            Self::Array(inner) | Self::Optional(inner) => inner.conceptual_type.contains_any_cbor(),
            Self::Map(k, v) => {
                k.conceptual_type.contains_any_cbor() || v.conceptual_type.contains_any_cbor()
            }
            Self::Alias(_, inner) => inner.contains_any_cbor(),
            _ => false,
        }
    }

    pub fn name_as_wasm_array_ct(&self, types: &IntermediateTypes) -> String {
        if Self::Array(Box::new(self.clone().into())).directly_wasm_exposable_ct(types) {
            format!("Vec<{}>", self.for_wasm_member_ct(types))
        } else {
            format!("{}List", self.for_variant())
        }
    }

    pub fn name_as_rust_array_ct(
        &self,
        types: &IntermediateTypes,
        from_wasm: bool,
        cli: &Cli,
    ) -> String {
        format!("Vec<{}>", self.for_rust_member_ct(types, from_wasm, cli))
    }

    /// Function parameter TYPE from wasm (i.e. ref for non-primitives, value for supported primitives)
    pub fn for_wasm_param_ct(&self, types: &IntermediateTypes) -> String {
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
            // Honest-but-unreachable: A2 rejects `any` under --wasm before any wasm param renders.
            Self::Any => format!("{opt_ref}AnyCbor"),
            Self::Rust(ident) => {
                if types.is_enum(ident) {
                    ident.to_string()
                } else {
                    format!("{opt_ref}{ident}")
                }
            }
            Self::Array(ty) => {
                if self.directly_wasm_exposable_ct(types) {
                    ty.conceptual_type.name_as_wasm_array_ct(types)
                } else {
                    format!(
                        "{}{}",
                        opt_ref,
                        ty.conceptual_type.name_as_wasm_array_ct(types)
                    )
                }
            }
            Self::Optional(ty) => {
                format!(
                    "Option<{}>",
                    ty.conceptual_type.for_wasm_param_impl(types, true)
                )
            }
            Self::Map(_k, _v) => format!("{}{}", opt_ref, self.for_wasm_member_ct(types)),
            // it might not be worth generating this as aliases are ignored by wasm-pack build, but
            // that could change in the future so as long as it doens't cause issues we'll leave it
            // A pair-carrying alias emits no `pub type` on the wasm face either, so its name is not
            // a type a parameter can be declared as — spell what it resolves to. Checked before the
            // shape arms below because the suppression is a property of the NAME, not of the shape.
            Self::Alias(AliasIdent::Rust(rust_ident), ty)
                if types.alias_projection_suppressed(rust_ident) =>
            {
                ty.for_wasm_param_impl(types, force_not_ref)
            }
            Self::Alias(ident, ty) => match &**ty {
                Self::Rust(_) |
                Self::Array(_) |
                Self::Map(_, _) if !self.directly_wasm_exposable_ct(types) => format!("{opt_ref}{ident}"),
                Self::Optional(_) |
                // no special handling if for some reason nested aliases, just strip all to avoid hassle
                Self::Alias(_, _) => ty.for_wasm_param_impl(types, force_not_ref),
                _ => ident.to_string(),
            },
        }
    }

    /// Return TYPE for wasm
    pub fn for_wasm_return_ct(&self, types: &IntermediateTypes) -> String {
        self.for_wasm_member_ct(types)
    }

    /// The structural wasm class name for a table shape. The name derives from the STRUCTURE, and the
    /// structure includes the backing container: a `@duplicates preserve` map is a `PairMap<K, V>` (a
    /// duplicate-permitting vec of pairs) while the default flavor is a key-VALUE-keyed
    /// `OrderedHashMap`/`BTreeMap`, and two structurally different types must not derive one name —
    /// one class can only have one inner type, so sharing the name emits a wasm crate that does not
    /// compile. `preserve` therefore prefixes `PairMap` exactly as `NonEmpty` prefixes for the min-1
    /// occurrence (`NonEmptyPairMapKToV` composes both).
    ///
    /// `preserve` is LOCAL information at every call site — a table rule's `config().duplicates`, a
    /// `RestRow::duplicates()`, or the `RustType`'s own carried policy (`is_preserve_pair_map`). It is
    /// never recovered from a crate-wide shape lookup: the whole point of encoding the flavor in the
    /// name is that a shape no longer determines a flavor.
    pub fn name_for_wasm_map(k: &RustType, v: &RustType, preserve: bool) -> RustIdent {
        RustIdent::new(CDDLIdent::new(format!(
            "{}Map{}To{}",
            if preserve { "Pair" } else { "" },
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
            // RustType-level so a `[+ T]` map value picks up NonEmptyVec (bounds live on RustType)
            k.for_rust_member(types, from_wasm, cli),
            v.for_rust_member(types, from_wasm, cli)
        )
    }

    /// If we were to store a value directly in a wasm-wrapper, this would be used.
    pub fn for_wasm_member_ct(&self, types: &IntermediateTypes) -> String {
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type in member, only needed for serializaiton: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            // Honest-but-unreachable: A2 rejects `any` under --wasm before a wasm member renders.
            Self::Any => "AnyCbor".to_owned(),
            Self::Rust(ident) => ident.to_string(),
            Self::Array(ty) => ty.conceptual_type.name_as_wasm_array_ct(types),
            Self::Optional(ty) => {
                format!("Option<{}>", ty.conceptual_type.for_wasm_member_ct(types))
            }
            // Flavor-blind by construction: a `ConceptualRustType` carries no `@duplicates` policy
            // (it lives on the enclosing `RustType`), so this names the DEFAULT-flavored class. The
            // preserve twin is named one level up, by `RustType::for_wasm_member`, which can see the
            // policy — exactly how the `{+ …}` bound is handled.
            Self::Map(k, v) => Self::name_for_wasm_map(k, v, false).to_string(),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_wasm_member_ct(types),
                // A pair-carrying alias emits no wasm `pub type`, so the name does not exist to
                // store a member as — spell the resolved type. The stored VALUE is unaffected: the
                // suppressed alias was transparent, so the name and the base named one type.
                AliasIdent::Rust(rust_ident) if types.alias_projection_suppressed(rust_ident) => {
                    ty.for_wasm_member_ct(types)
                }
                // but other aliases are generated and should be used.
                AliasIdent::Rust(_) => ident.to_string(),
            },
        }
    }

    /// Type when storing a value inside of a rust struct. This is the underlying raw representation.
    pub fn for_rust_member_ct(
        &self,
        types: &IntermediateTypes,
        from_wasm: bool,
        cli: &Cli,
    ) -> String {
        match self {
            Self::Fixed(_) => panic!(
                "should not expose Fixed type in member, only needed for serializaiton: {:?}",
                self
            ),
            Self::Primitive(p) => p.to_string(),
            // The static-runtime `AnyCbor`, reached through the same common-import glue as the other
            // own-module runtime types (`ordered_hash_map::OrderedHashMap`). `from_wasm` renders the
            // rust type as seen FROM the wasm crate — the wasm-crate-visible prefix
            // (`common_import_wasm()`: `<lib>` non-override, the override crate otherwise), never
            // `common_import_rust()` (`crate::generated`), which in the wasm crate would name the
            // wasm crate's own — absent — `any_cbor` module. The rust-side path
            // (`crate::generated::any_cbor::AnyCbor`) is unchanged for in-crate members.
            Self::Any => {
                let common = if from_wasm {
                    cli.common_import_wasm()
                } else {
                    cli.common_import_rust().to_owned()
                };
                format!("{common}::any_cbor::AnyCbor")
            }
            Self::Rust(ident) => {
                if from_wasm && !types.is_enum(ident) {
                    crate::generation::rust_crate_struct_from_wasm(types, ident, cli)
                } else {
                    ident.to_string()
                }
            }
            Self::Array(ty) => ty
                .conceptual_type
                .name_as_rust_array_ct(types, from_wasm, cli),
            Self::Optional(ty) => {
                format!(
                    "Option<{}>",
                    ty.conceptual_type.for_rust_member_ct(types, from_wasm, cli)
                )
            }
            Self::Map(k, v) => Self::name_for_rust_map(types, k, v, from_wasm, cli),
            Self::Alias(ident, ty) => match ident {
                // we don't generate type aliases for reserved types, just transform
                // them into rust equivalents, so we can't and shouldn't use their alias here.
                AliasIdent::Reserved(_) => ty.for_rust_member_ct(types, from_wasm, cli),
                // A pair-carrying alias emits no `pub type`, so its name is not a type a member can
                // be declared as — spell what it resolves to. Recursing (rather than spelling the
                // base ident here) is what keeps the marker-alias flavor right: the base is a
                // `Rust(PolicyId)`, whose own arm applies the `from_wasm` crate-path form the
                // suppressed name would otherwise have carried.
                AliasIdent::Rust(rust_ident) if types.alias_projection_suppressed(rust_ident) => {
                    ty.for_rust_member_ct(types, from_wasm, cli)
                }
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
            // Choice-arm `any` is A3 (A2 rejects it), but `for_variant` is also reached by
            // structural names (`MapAnyToAny`, `ArrAny`) for tables/arrays of `any`, so it needs a
            // stable custom spelling here.
            Self::Any => VariantIdent::new_custom("Any"),
            Self::Rust(ident) => VariantIdent::new_rust(ident.clone()),
            Self::Array(inner) => {
                VariantIdent::new_custom(format!("Arr{}", inner.conceptual_type.for_variant()))
            }
            // TODO: should we not end up in this situation and just insert a Null fixed value instead?
            Self::Optional(ty) => {
                VariantIdent::new_custom(format!("Opt{}", ty.conceptual_type.for_variant()))
            }
            // Default-flavored, like `for_wasm_member_ct`: this arm is only reached by a RAW inline
            // `Map` occurrence (a named preserve table is referenced as an `Alias`, taking the alias
            // arm below), and an inline occurrence carries no `@duplicates` directive — the policy is
            // per-RULE, so a preserve map must be given its own named rule.
            Self::Map(k, v) => {
                VariantIdent::new_custom(Self::name_for_wasm_map(k, v, false).to_string())
            }
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
        // WASM face: `expr` here holds the value on the WASM side (crossing INTO rust), so the clone
        // decision is `is_wasm_copy`, NOT `is_copy`. A `@copy` extern is rust-Copy but its wasm face
        // is a non-Copy wrapper, so its `.clone()` MUST stay (dropping it here would move out of a
        // borrowed wasm value — E0507).
        let expr_cloned = if self.is_wasm_copy(types) {
            expr.to_owned()
        } else {
            format!("{expr}.clone()")
        };
        let mut ops = match self {
            // A c-style enum is `pub use`-re-exported into the wasm crate (its wasm face IS the rust
            // type), so the wasm->rust `.into()` is identity — skip it (clippy::useless_conversion).
            // `is_copy` already dropped the clone, so an enum reduces to the bare `expr`. Every other
            // `Rust` ident is a distinct wasm wrapper that needs the real `.into()`.
            Self::Rust(ident) if types.is_enum(ident) => {
                vec![ToWasmBoundaryOperations::Code(expr_cloned)]
            }
            Self::Rust(_ident) => vec![
                ToWasmBoundaryOperations::Code(expr_cloned),
                ToWasmBoundaryOperations::Into,
            ],
            // named alias: exposed as its wrapper type, so convert FROM the wrapper like
            // `Rust(ident)` (only reserved aliases unwrap to the inner rust type).
            Self::Alias(ident, ty) => match ident {
                AliasIdent::Reserved(_) => ty.from_wasm_boundary_clone(types, expr, can_fail),
                // An alias of an optional is exposed transparently as `Option<Wrapper>`, so the
                // reverse boundary must map through the Option (mirroring the `Self::Optional`
                // arm below) rather than a blanket `Into` — else wasm E0277 on the composite inner.
                AliasIdent::Rust(_) if matches!(&**ty, Self::Optional(_)) => {
                    ty.from_wasm_boundary_clone(types, expr, can_fail)
                }
                AliasIdent::Rust(_) => {
                    if self.directly_wasm_exposable_ct(types)
                        && matches!(ty.resolve_alias_shallow(), Self::Primitive(_))
                    {
                        vec![ToWasmBoundaryOperations::Code(expr_cloned)]
                    } else {
                        vec![
                            ToWasmBoundaryOperations::Code(expr_cloned),
                            ToWasmBoundaryOperations::Into,
                        ]
                    }
                }
            },
            Self::Optional(ty) => ty
                .conceptual_type
                .from_wasm_boundary_clone_optional(types, expr, can_fail),
            Self::Array(ty) => {
                if self.directly_wasm_exposable_ct(types) {
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
            // `AnyCbor` is a distinct wasm wrapper (`is_wasm_copy` false, so the clone stays); the
            // wasm->rust `.into()` reaches the `From<wasm AnyCbor> for rust AnyCbor` conversion
            // `add_conversion_methods` emits. Same shape as `Rust(_ident)`/`Map`.
            Self::Any => vec![
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
            Self::Array(..) | Self::Rust(..) | Self::Map(..) | Self::Any => vec![
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
            // A wrapper struct (`Rust` ident, NOT directly exposable) is exposed by-ref and the map-key
            // `get` caller appends `.as_ref()` to reach `&native`, so return `expr` unchanged. A
            // *directly-exposable* `Rust` ident — a Copy c-style enum re-exported by value — is passed
            // BY VALUE and reaches the no-`.as_ref()` get branch, so it needs an explicit `&` for
            // `BTreeMap::get` (like a primitive key), else `self.0.get(key)` mismatches `&Q` (E0308).
            Self::Rust(_ident) => {
                if self.directly_wasm_exposable_ct(types) {
                    format!("&{expr}")
                } else {
                    expr.to_owned()
                }
            }
            // A named alias is exposed to wasm AS its wrapper (for_wasm_member keeps the alias name),
            // and whether it's a wrapper or a transparent `pub type` is a struct-table fact, not a
            // shape fact (see directly_wasm_exposable / to_wasm_boundary). A wrapper alias
            // (nums = [* uint]) is passed by-ref exactly like Rust(ident) — its own AsRef yields the
            // &native the caller wants (the map-key `get` appends `.as_ref()`), so return `expr`
            // unchanged. Transparently unwrapping it instead re-derived the inline type and, for an
            // array wrapper, prepended a stray `&` -> `&key.as_ref()` (E0277). Only a transparent
            // passthrough (arr2 = arr) or a reserved alias (u64, …) unwraps into what it aliases.
            Self::Alias(ident, ty) => match ident {
                AliasIdent::Reserved(_) => ty.from_wasm_boundary_ref(types, expr),
                AliasIdent::Rust(_) => {
                    if self.directly_wasm_exposable_ct(types) {
                        ty.from_wasm_boundary_ref(types, expr)
                    } else {
                        expr.to_owned()
                    }
                }
            },
            Self::Optional(ty) => ty.conceptual_type.from_wasm_boundary_ref(types, expr),
            Self::Array(ty) => {
                if self.directly_wasm_exposable_ct(types) {
                    ty.conceptual_type.from_wasm_boundary_ref(types, expr)
                } else {
                    expr.to_owned()
                }
            }
            Self::Map(_k, _v) => expr.to_owned(),
            // `AnyCbor` is a by-ref wasm wrapper (not directly exposable): the map-key `get`/`has`
            // caller appends `.as_ref()`, and the wasm `AnyCbor`'s `AsRef<rust AnyCbor>` (from
            // `add_conversion_methods`) yields the `&native` the lookup wants — so return `expr`
            // unchanged, exactly like the non-exposable `Rust(_ident)` arm. The `&{expr}` catch-all
            // would produce `&key.as_ref()` (`&&rust AnyCbor`, E0308).
            Self::Any => expr.to_owned(),
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
            // Honest-but-unreachable: A2 rejects `any` under --wasm before a boundary crossing.
            Self::Any => format!("{expr}.clone().into()"),
            Self::Rust(ident) => {
                if types.is_enum(ident) {
                    primitive_impl()
                } else if self.is_copy(types) {
                    // A `@copy` extern: rust-Copy but wasm-wrapped. Drop the defensive `.clone()`
                    // (the value copies), keep the `.into()` to the wasm wrapper. `(*expr).into()`
                    // when the binding is a reference (enum-variant match accessors — report site 3),
                    // else `expr.into()` (record/wrapper getters, list indexed getter).
                    if is_ref {
                        format!("(*{expr}).into()")
                    } else {
                        format!("{expr}.into()")
                    }
                } else {
                    format!("{expr}.clone().into()")
                }
            }
            //Self::Array(ty) => format!("{}({}.clone())", ty.name_as_wasm_array(types), expr),
            //Self::Map(k, v) => format!("{}({}.clone())", Self::name_for_wasm_map(k, v), expr),
            Self::Array(_ty) => {
                if self.directly_wasm_exposable_ct(types) {
                    format!("{expr}.clone()")
                } else {
                    format!("{expr}.clone().into()")
                }
            }
            Self::Map(_k, _v) => format!("{expr}.clone().into()"),
            Self::Optional(ty) => ty
                .conceptual_type
                .to_wasm_boundary_optional(types, expr, is_ref),
            // a named alias (`AliasIdent::Rust`) is exposed to wasm AS its wrapper type (see
            // `for_wasm_member`), so the boundary must convert INTO that wrapper — mirror the
            // `Rust(ident)` arm rather than transparently unwrapping (which yields the inlined inner
            // type and mismatches the wrapper return type). Only reserved aliases (u64, …) unwrap.
            Self::Alias(ident, ty) => match ident {
                AliasIdent::Reserved(_) => ty.to_wasm_boundary(types, expr, is_ref),
                // a `Copy` named alias (e.g. a c-style-enum or primitive alias) is exposed by value
                // with no wrapper conversion; a non-copy named alias (array/map/struct wrapper) needs
                // `.into()` into its wrapper. `is_copy` handles the alias without the `is_enum`
                // precondition (which panics on pure type-aliases that are neither struct nor generic).
                AliasIdent::Rust(_) => {
                    // `is_wasm_copy`, not `is_copy`: `primitive_impl` emits the value with NO
                    // `.into()`, correct only when the wasm face IS the rust type (a c-style-enum or
                    // primitive alias). A `@copy` extern alias is rust-Copy but wasm-wrapped, so it
                    // needs the `.into()` — it falls through to the `.clone().into()` arm below.
                    if self.is_wasm_copy(types) {
                        primitive_impl()
                    } else if matches!(&**ty, Self::Optional(_)) {
                        // An alias of an optional (`x = inner / null`) is exposed transparently as
                        // `Option<Wrapper>` (a `pub type`), NOT a newtype wrapper, so it must
                        // convert THROUGH the Option (`.map(Into::into)`). A blanket `.into()` here
                        // has no `From<Option<Inner>>` impl when the inner needs a wrapper
                        // conversion (named collection / data-enum), producing wasm E0277/E0308.
                        ty.to_wasm_boundary(types, expr, is_ref)
                    } else {
                        format!("{expr}.clone().into()")
                    }
                }
            },
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
        if self.directly_wasm_exposable_ct(types) {
            self.to_wasm_boundary(types, expr, is_ref)
        } else if self.is_copy(types) {
            // A `@copy` extern in an `Option<T>` field: `Option<T>` is itself Copy (T: Copy), so the
            // `.map` consumes it by copy — drop the `.clone()` (clippy::clone_on_copy — report
            // site 2). The `.into()` to the wasm wrapper still runs per element.
            format!("{expr}.map(std::convert::Into::into)")
        } else {
            format!("{expr}.clone().map(std::convert::Into::into)")
        }
    }

    // if it impements the Copy trait in rust
    //
    // This is the RUST-face question. A `Rust(ident)` is Copy when it is a c-style enum (a plain
    // fieldless rust enum) OR an extern / raw-bytes type declared `@copy` (its hand-written rust type
    // derives Copy). Note the wasm face of a `@copy` extern is a distinct `#[wasm_bindgen]` wrapper
    // that is NOT Copy — wasm-side clone decisions must use `is_wasm_copy`, not this.
    pub fn is_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::Fixed(_f) => unreachable!(),
            Self::Primitive(p) => match p {
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
                | Primitive::N64
                | Primitive::U8
                | Primitive::U16
                | Primitive::U32
                | Primitive::U64 => true,
                Primitive::Str | Primitive::Bytes => false,
            },
            Self::Rust(ident) => types.is_enum(ident) || types.is_copy_extern(ident),
            Self::Array(_) => false,
            Self::Map(_k, _v) => false,
            // `AnyCbor` holds Vec/String/Box — not Copy.
            Self::Any => false,
            Self::Optional(ty) => ty.conceptual_type.is_copy(types),
            Self::Alias(_ident, ty) => ty.is_copy(types),
        }
    }

    /// Whether the WASM face of this type is Copy. This differs from [`is_copy`] (the rust face) at
    /// exactly one shape: a `@copy` extern / raw-bytes type, whose rust type derives Copy but whose
    /// wasm face is a distinct `#[wasm_bindgen]` wrapper struct (Clone, never Copy). Every OTHER Copy
    /// shape — primitives, c-style enums (`pub use`-re-exported so wasm face IS the rust type) — is
    /// Copy on both faces, so `is_wasm_copy == is_copy` there. Wasm-side clone/`.copied()` decisions
    /// on a value whose type is the wasm wrapper (e.g. `from_wasm_boundary_clone`) use THIS, so a
    /// `@copy` extern still clones its wasm wrapper across the wasm→rust boundary.
    pub fn is_wasm_copy(&self, types: &IntermediateTypes) -> bool {
        match self {
            Self::Rust(ident) => types.is_enum(ident),
            Self::Optional(ty) => ty.conceptual_type.is_wasm_copy(types),
            Self::Alias(_ident, ty) => ty.is_wasm_copy(types),
            // Every non-`Rust` shape is Copy on both faces iff it is Copy at all.
            _ => self.is_copy(types),
        }
    }

    /// For `--wasm-list-macro`: whether the element's wasm-boundary reduces to the
    /// `(needs_into, is_copy)` form the list macro takes. Returns `Some(needs_into)` when it
    /// does (mirroring the `.into()` decision in `to_wasm_boundary`), or `None` when the element
    /// must stay inline because its boundary can't be expressed by those two bits.
    ///
    /// Note on enums: `get` takes no `.into()` (enums are `Copy`, returned by value) so
    /// `needs_into = false` — and the inline `add` matches: `from_wasm_boundary_clone`'s `Rust` arm
    /// skips the `.into()` for a c-style enum (which is re-exported into the wasm crate via
    /// `pub use ...::Color`, so `rust_elem` and `wasm_elem` are the *same* type — no conversion
    /// needed either way, and the macro's `push(elem)` is exactly what both sides emit).
    /// `Optional`/`Fixed` return `None`: an `Optional` element's
    /// wasm return type is `Option<T>`, which the two-bit form can't express, so it falls back to
    /// the inline wrapper (reachable, e.g. `m = text / null; [* m]`).
    pub fn wasm_list_macro_needs_into(&self, types: &IntermediateTypes) -> Option<bool> {
        match self {
            Self::Primitive(_) => Some(false),
            Self::Rust(ident) => Some(!types.is_enum(ident)),
            Self::Array(_) => Some(!self.directly_wasm_exposable_ct(types)),
            Self::Map(_k, _v) => Some(true),
            // A named alias is exposed AS its wrapper struct when one exists (see the Alias arm of
            // `directly_wasm_exposable`), so conversions go through From/Into regardless of the
            // aliased SHAPE's own exposability — recursing shape-transparently mislabels an alias
            // to an exposable array (`nested = [* texts]` stores `Vec<rust::Texts>` but exposes the
            // `Texts` wrapper class, so it still needs `.into()`). Only a transparent alias (no
            // generated struct, or a re-exported c-style enum) follows what it aliases.
            Self::Alias(ident, ty) => match ident {
                AliasIdent::Reserved(_) => ty.wasm_list_macro_needs_into(types),
                AliasIdent::Rust(rust_ident) => {
                    if types.has_wasm_wrapper(rust_ident) {
                        Some(true)
                    } else {
                        ty.wasm_list_macro_needs_into(types)
                    }
                }
            },
            // serialization-only / can't reduce to the two-bit form (`any` has no wasm surface in
            // A2 — falls back to the inline path, honest-but-unreachable under the --wasm reject).
            Self::Optional(_) | Self::Fixed(_) | Self::Any => None,
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
            // Leaf: `any` is opaque (`AnyCbor` carries its own inner CBOR, not IR types), so we
            // visit self (via `f(self)` above) and do NOT recurse. This is what keeps
            // `key_contains_float` false for `Any` — floats are reachable INSIDE an `AnyCbor` value
            // at runtime, but they are not IR float types the key-float pass can see.
            Self::Any => (),
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
                if already_visited.insert(ident.clone())
                    && let Some(t) = types.rust_struct(ident)
                {
                    t.visit_types_excluding(types, f, already_visited)
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

#[cfg(test)]
mod tests {
    use super::FixedValue;
    use cbor_event::Sz;

    /// `FixedValue::to_bytes` for negative literals must produce canonical CBOR nint bytes across
    /// the full magnitude ladder, and — critically — for `i64::MIN`, the boundary where a
    /// same-width negation overflows (pre-3.x cbor_event rejected it on the plain endpoint; the
    /// `_sz` form takes i128 and covers the full nint range).
    ///
    /// Expected bytes are hard-coded literals (NOT computed via the old code path) so a width or
    /// endpoint regression that silently changed the bytes would be caught here rather than
    /// masked by re-deriving both sides from the same buggy source.
    #[test]
    fn nint_to_bytes_canonical_across_boundaries() {
        // (value, canonical CBOR nint encoding)
        let cases: &[(i128, &[u8])] = &[
            (-1, &[0x20]),
            (-24, &[0x37]),
            (-25, &[0x38, 0x18]),
            (-0x100, &[0x38, 0xff]),
            (-0x101, &[0x39, 0x01, 0x00]),
            (-0x1_0000, &[0x39, 0xff, 0xff]),
            (-0x1_0001, &[0x3a, 0x00, 0x01, 0x00, 0x00]),
            (-0x1_0000_0000, &[0x3a, 0xff, 0xff, 0xff, 0xff]),
            (
                -0x1_0000_0001,
                &[0x3b, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00],
            ),
            (
                (i64::MIN as i128) + 1,
                &[0x3b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe],
            ),
            // i64::MIN: 0x3b + 8 bytes of 0x7fffffffffffffff. The old i64 path overflowed here.
            (
                i64::MIN as i128,
                &[0x3b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
            ),
        ];
        for (value, expected) in cases {
            assert_eq!(
                FixedValue::Nint(*value).to_bytes(),
                *expected,
                "Nint({value}) encoded incorrectly"
            );
        }
    }

    /// Sanity-check the magnitude→Sz derivation matches cbor_event's canonical rule at the
    /// class boundaries (guards against an off-by-one if the magnitude formula is ever touched).
    #[test]
    fn nint_magnitude_sz_is_canonical() {
        assert_eq!(Sz::canonical(0), Sz::Inline);
        assert_eq!(Sz::canonical(23), Sz::Inline);
        assert_eq!(Sz::canonical(24), Sz::One);
        assert_eq!(Sz::canonical(0xff), Sz::One);
        assert_eq!(Sz::canonical(0x100), Sz::Two);
        assert_eq!(Sz::canonical(0xffff), Sz::Two);
        assert_eq!(Sz::canonical(0x1_0000), Sz::Four);
        assert_eq!(Sz::canonical(0xffff_ffff), Sz::Four);
        assert_eq!(Sz::canonical(0x1_0000_0000), Sz::Eight);
    }
}
