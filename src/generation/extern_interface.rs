//! IR → CDDL renderer for the dep-side extern-interface export (commit 3 of the export series).
//!
//! The export emitter (commit 4) spells one CDDL rule per exported name. **Class-backed** rows
//! (records, wrappers, type/group choices, externs, raw-bytes) export as opaque markers and need no
//! renderer. **Transparent** rows — transparent aliases, c-style enums, and named collections (whose
//! rust surface is a `pub type`) — must be spelled truthfully, as the real CDDL shape a consumer's
//! generator will re-derive identically. That truthful spelling is this module's job: turn a
//! finalized `RustType`/`ConceptualRustType` back into CDDL text.
//!
//! ## Domain inventory (what a transparent row can carry, all handled below)
//!
//! - **Primitives** — `bool`, `uint`, `nint`, `int`, `tstr`, `bytes`, `float32`, `float64`. The
//!   fixed-width integer identities (`u8`/`u16`/`u32`, `i8`/`i16`/`i32`) have no bare CDDL prelude
//!   name — they arise only from a bound-collapse (`uint .size 2` → `u16`), so they render back to a
//!   provably-equivalent size/range form (`uint .size 2`, `-128..127`, …) that round-trips.
//! - **Value-range / `.size` bounds** — a primitive carrying `config.bounds` (integer window,
//!   endpoints normalized inclusive) or `config.float_bounds` (per-side NaN-safe float window). Text
//!   / bytes `.size` (exact `.size n`, ranged `.size (n..m)`) live here too.
//! - **`#6.n` tags** — a `CBOREncodingOperation::Tagged(n)` wraps the inner spelling as `#6.n(inner)`.
//! - **`.cbor`-wrapped types** — a `CBOREncodingOperation::CBORBytes` renders `bytes .cbor inner`.
//! - **`Optional`** (`T / null`).
//! - **Fixed values** — `null`, `true`/`false`, integers, floats, text literals (c-style enum arms
//!   are exactly a list of these: `0 / 1 / 2`).
//! - **Nested arrays / maps with occurrence bounds** — `[* elem]`, `[+ elem]`, `[n*m elem]`,
//!   `{* k => v}`, occurrence taken from the container `RustType`'s `config.bounds`.
//! - **References to named rules** — a `Rust(ident)` / `Alias(Rust(ident), _)` renders as the
//!   referenced rule's ORIGINAL CDDL ident (from the source-spelling registry,
//!   `IntermediateTypes::source_rule_name`), never a re-derived spelling — the consumer resolves it
//!   by that ident.
//!
//! ## Discipline
//!
//! - **No lossy or guessed spelling.** Any shape that cannot be spelled faithfully is a hard `Err`
//!   naming the rule and the shape — a lossy export is worse than none (the hand-stub escape hatch
//!   covers the gap until the renderer grows the case).
//! - **Exhaustive matches** over `ConceptualRustType`, `Primitive`, and `FixedValue` — no `_ =>`
//!   arm — so a future variant forces an explicit render decision at compile time.
//! - **Custom-serialize is a projection hard error.** A transparent rule whose `RuleMetadata`
//!   carries `@custom_serialize`/`@custom_deserialize` diverges from default wire logic; exporting
//!   its plain definition would make the consumer emit default (de)serialization — a silent
//!   wire-format divergence, the worst failure class here. The entry points TAKE the metadata so
//!   this check cannot be bypassed.
//!
//! Every entry point is a `Result` — no panics.

use std::collections::{BTreeMap, BTreeSet};

use crate::cli::Cli;
use crate::comment_ast::RuleMetadata;
use crate::intermediate::{
    AliasIdent, CBOREncodingOperation, ConceptualRustType, EnumVariant, EnumVariantData,
    FixedValue, FloatWindow, IntermediateTypes, Primitive, ROOT_SCOPE, Representation, RustField,
    RustIdent, RustRecord, RustStruct, RustStructConfig, RustStructType, RustType,
    RustTypeSerializeConfig,
};

/// A rendering failure. Both variants name the rule; `Unrenderable` also names the offending shape.
/// Result-based (never a panic) so the export emitter can attribute the failure and fall back to the
/// hand-stub escape hatch.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ExternInterfaceError {
    /// A shape the renderer cannot spell faithfully back to CDDL (naming rule + shape).
    Unrenderable { rule: String, shape: String },
    /// A transparent rule carrying `@custom_serialize` / `@custom_deserialize`: exporting its plain
    /// CDDL would make the consumer emit DEFAULT wire logic, silently diverging from the dep's real
    /// wire format. Projection-level hard error (hand-stub escape hatch).
    CustomSerializeTransparent {
        rule: String,
        annotation: &'static str,
    },
}

impl std::fmt::Display for ExternInterfaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExternInterfaceError::Unrenderable { rule, shape } => write!(
                f,
                "rule `{rule}`: cannot render {shape} back to CDDL for the extern-interface export \
                 (a lossy export is worse than none — hand-stub this rule instead)"
            ),
            ExternInterfaceError::CustomSerializeTransparent { rule, annotation } => write!(
                f,
                "rule `{rule}`: a transparent rule carrying `{annotation}` cannot be exported — its \
                 plain CDDL would make the consumer emit default wire logic, silently diverging from \
                 the dependency's real wire format (hand-stub this rule instead)"
            ),
        }
    }
}

type RenderResult = Result<String, ExternInterfaceError>;

fn unrenderable(rule: &str, shape: impl Into<String>) -> ExternInterfaceError {
    ExternInterfaceError::Unrenderable {
        rule: rule.to_string(),
        shape: shape.into(),
    }
}

// --- Entry points ------------------------------------------------------------------------------

/// Render the right-hand side (body) of a TRANSPARENT rule (a transparent alias, or a named
/// collection whose alias `RustType` carries the `Array`/`Map` shape + occurrence bounds + any tag).
/// The emitter prepends `<ident> = ` and appends the `@rust_name` pin / header.
///
/// `rule` is the rule's ORIGINAL CDDL ident (for diagnostics). `metadata` is the rule's
/// `RuleMetadata` (or `None`) — passing it here makes the custom-serialize projection error
/// impossible to bypass.
pub(crate) fn render_transparent_rule_body(
    rule: &str,
    ty: &RustType,
    metadata: Option<&RuleMetadata>,
    types: &IntermediateTypes,
) -> RenderResult {
    reject_custom_serialize(rule, metadata)?;
    render_rust_type(rule, ty, types)
}

/// Render the body of a c-style enum rule as its value choices (`0 / 1 / 2`), reading each variant's
/// fixed value from the IR. A c-style enum is transparent (a real Rust enum lives in the dep, but the
/// consumer sees only the value choices), so it is gated on the same custom-serialize projection
/// error.
pub(crate) fn render_c_style_enum_body(
    rule: &str,
    variants: &[EnumVariant],
    metadata: Option<&RuleMetadata>,
    types: &IntermediateTypes,
) -> RenderResult {
    reject_custom_serialize(rule, metadata)?;
    let _ = types; // symmetry with the transparent-rule entry; not needed for pure value choices
    if variants.is_empty() {
        return Err(unrenderable(rule, "a c-style enum with no variants"));
    }
    let mut choices = Vec::with_capacity(variants.len());
    for variant in variants {
        match &variant.data {
            EnumVariantData::RustType(ty) => match ty.conceptual_type.resolve_alias_shallow() {
                ConceptualRustType::Fixed(fixed) => choices.push(render_fixed_value(fixed)),
                other => {
                    return Err(unrenderable(
                        rule,
                        format!("a c-style enum variant that is not a fixed value ({other:?})"),
                    ));
                }
            },
            EnumVariantData::Inlined(_) => {
                return Err(unrenderable(
                    rule,
                    "a c-style enum variant with an inlined record",
                ));
            }
        }
    }
    Ok(choices.join(" / "))
}

/// Render the body of a materialized plain group (a `Record`) as a CDDL group body: `( m, m, … )`,
/// the truthful post-DSL shape a consumer's generator re-derives identically. A plain group is
/// TRANSPARENT — it has no cross-crate whole-value class the consumer holds opaquely; instead the
/// consumer regenerates the shape and delegates the wire code to the dep's own class (whole-value
/// for a group-choice arm, embedded-group for a spliced record member). So it is gated on the same
/// custom-serialize projection error as the other transparent rows.
///
/// Member rules (all round-trip: the consumer's parse of the rendered member yields the identical
/// `RustField`):
/// - **member key** — an ARRAY-rep field carries a bareword label = its post-DSL rust field name (a
///   snake_case CDDL id that re-derives to itself, baking in any `@name` rename with no annotation);
///   a MAP-rep field carries its fixed member key (`Uint(n)` → `n:`, `Text(s)` → bareword `s:` when
///   `s` is a valid CDDL id else the quoted `"s":`).
/// - **optionality** — a `? ` prefix from the field's occurrence flag ONLY. A `T / null` field is
///   NOT occurrence-optional (it is a present-or-null `Optional` conceptual type), so it renders its
///   `… / null` type with no `?` — the two carry different wire formats and must not be conflated.
/// - **`.default`** — legal only in member position, so it is stripped before the inner render
///   (`render_conceptual` hard-errors on `config.default`) and re-appended as ` .default <value>`.
/// - a member type that cannot be spelled hard-`Err`s (propagates to an exclusion record).
pub(crate) fn render_group_body(
    rule: &str,
    record: &RustRecord,
    metadata: Option<&RuleMetadata>,
    types: &IntermediateTypes,
) -> RenderResult {
    reject_custom_serialize(rule, metadata)?;
    let mut members = Vec::with_capacity(record.fields.len());
    for field in &record.fields {
        members.push(render_group_member(rule, record.rep, field, types)?);
    }
    Ok(format!("({})", members.join(", ")))
}

/// Render one group member: `[? ]<key: >type[ .default v]`. See [`render_group_body`] for the rules.
fn render_group_member(
    rule: &str,
    rep: Representation,
    field: &RustField,
    types: &IntermediateTypes,
) -> RenderResult {
    // A `.default` on the field's type would make `render_conceptual` hard-error (it is a member-only
    // construct); strip it for the inner render and re-append it here, where it is legal.
    let (type_source, default_suffix) = match &field.rust_type.config.default {
        Some(value) => {
            let mut stripped = field.rust_type.clone();
            stripped.config.default = None;
            (stripped, format!(" .default {}", render_fixed_value(value)))
        }
        None => (field.rust_type.clone(), String::new()),
    };
    let type_s = render_rust_type(rule, &type_source, types)?;
    let key_s = render_member_key(rule, rep, field)?;
    let occurrence = if field.optional { "? " } else { "" };
    Ok(format!("{occurrence}{key_s}{type_s}{default_suffix}"))
}

/// The `<key>: ` prefix of a group member. An ARRAY-rep field labels itself with its post-DSL rust
/// field name; a MAP-rep field spells its fixed member key. A map member with no key, or a non-uint/
/// non-text fixed key, has no faithful member spelling — hard `Err`.
fn render_member_key(
    rule: &str,
    rep: Representation,
    field: &RustField,
) -> Result<String, ExternInterfaceError> {
    match rep {
        Representation::Array => Ok(format!("{}: ", field.name)),
        Representation::Map => match &field.key {
            Some(FixedValue::Uint(n)) => Ok(format!("{n}: ")),
            Some(FixedValue::Text(s)) if is_cddl_bareword(s) => Ok(format!("{s}: ")),
            Some(FixedValue::Text(s)) => Ok(format!("{s:?}: ")),
            Some(other) => Err(unrenderable(
                rule,
                format!("a map-rep group member with an unsupported fixed key kind ({other:?})"),
            )),
            None => Err(unrenderable(
                rule,
                "a map-rep group member with no member key",
            )),
        },
    }
}

/// Whether `s` is a valid bare CDDL identifier (so a text map key can spell as `s:` rather than the
/// quoted `"s":`). CDDL idents start with a letter (or `@`/`_`/`$`) and continue with letters,
/// digits, `-`, `.`, `@`, `_`, `$`. Conservatively require an ASCII-alpha lead here — the quoted
/// form is always available as the faithful fallback, so this only chooses the prettier spelling.
fn is_cddl_bareword(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.' | '@' | '$'))
}

fn reject_custom_serialize(
    rule: &str,
    metadata: Option<&RuleMetadata>,
) -> Result<(), ExternInterfaceError> {
    if let Some(md) = metadata {
        if md.custom_serialize.is_some() {
            return Err(ExternInterfaceError::CustomSerializeTransparent {
                rule: rule.to_string(),
                annotation: "@custom_serialize",
            });
        }
        if md.custom_deserialize.is_some() {
            return Err(ExternInterfaceError::CustomSerializeTransparent {
                rule: rule.to_string(),
                annotation: "@custom_deserialize",
            });
        }
    }
    Ok(())
}

// --- Core recursion ----------------------------------------------------------------------------

/// Render a full `RustType`: peel encoding operations (outermost = last in the vec) as CDDL
/// wrappers, then render the conceptual type with its value config at the base.
fn render_rust_type(rule: &str, ty: &RustType, types: &IntermediateTypes) -> RenderResult {
    if let Some((last, rest)) = ty.encodings.split_last() {
        // Peel one encoding, keeping the value config with the (still-inner) conceptual type so the
        // base render applies bounds exactly once.
        let inner = RustType {
            conceptual_type: ty.conceptual_type.clone(),
            encodings: rest.to_vec(),
            config: ty.config.clone(),
        };
        let inner_s = render_rust_type(rule, &inner, types)?;
        return Ok(match last {
            CBOREncodingOperation::Tagged(tag) => format!("#6.{tag}({inner_s})"),
            // reverse-projection of the transparent tag-set collapse: the CDDL was a
            // tagged-or-untagged choice of the same collection.
            CBOREncodingOperation::OptionallyTagged(tag) => {
                format!("#6.{tag}({inner_s}) / {inner_s}")
            }
            CBOREncodingOperation::CBORBytes => format!("bytes .cbor {inner_s}"),
        });
    }
    render_conceptual(rule, &ty.conceptual_type, &ty.config, types)
}

fn render_conceptual(
    rule: &str,
    ct: &ConceptualRustType,
    config: &RustTypeSerializeConfig,
    types: &IntermediateTypes,
) -> RenderResult {
    // A `.default` is a member-position construct (`? field: uint .default 0`), never a standalone
    // transparent-rule RHS — its presence here is an unexpected shape, not something to silently drop.
    if config.default.is_some() {
        return Err(unrenderable(rule, "a type carrying a `.default` value"));
    }
    match ct {
        // Value config (bounds/float_bounds) is consumed only by Primitive / Array / Map; on every
        // other shape it is unexpected and must not be silently dropped.
        ConceptualRustType::Fixed(fixed) => {
            reject_value_config(rule, config, "a fixed value")?;
            Ok(render_fixed_value(fixed))
        }
        ConceptualRustType::Primitive(p) => render_primitive(rule, *p, config),
        ConceptualRustType::Rust(ident) => {
            reject_value_config(rule, config, "a named-rule reference")?;
            render_rust_ref(rule, ident, types)
        }
        ConceptualRustType::Optional(inner) => {
            reject_value_config(rule, config, "an optional")?;
            // The inner RustType carries its own encodings/config (e.g. `#6.n(inner) / null`).
            let inner_s = render_rust_type(rule, inner, types)?;
            Ok(format!("{inner_s} / null"))
        }
        ConceptualRustType::Array(inner) => {
            let occ = occurrence_marker(rule, config.float_bounds, config.bounds)?;
            let inner_s = render_rust_type(rule, inner, types)?;
            Ok(format!("[{occ} {inner_s}]"))
        }
        ConceptualRustType::Map(key, value) => {
            let occ = occurrence_marker(rule, config.float_bounds, config.bounds)?;
            let key_s = render_rust_type(rule, key, types)?;
            let value_s = render_rust_type(rule, value, types)?;
            Ok(format!("{{{occ} {key_s} => {value_s}}}"))
        }
        ConceptualRustType::Alias(alias_ident, _inner) => {
            reject_value_config(rule, config, "an alias reference")?;
            render_alias_ref(rule, alias_ident, types)
        }
    }
}

/// Guard for arms that do not consume `config.bounds`/`float_bounds`: a value window on such a shape
/// is unexpected, so hard-error rather than drop it.
fn reject_value_config(
    rule: &str,
    config: &RustTypeSerializeConfig,
    shape: &str,
) -> Result<(), ExternInterfaceError> {
    if config.bounds.is_some() || config.float_bounds.is_some() {
        return Err(unrenderable(
            rule,
            format!("{shape} carrying an unexpected value/size bound"),
        ));
    }
    Ok(())
}

// --- References --------------------------------------------------------------------------------

/// A reference to a named rule renders as its ORIGINAL CDDL ident (from the source-spelling
/// registry) — the consumer resolves it by that ident. Two references carry no user source name and
/// render by their CDDL prelude name instead (the consumer re-expands the prelude identically): the
/// reserved `int` extern (the `Int` struct) → `int`, and any synthesized `prelude_<name>` rule
/// (`PreludeBignint` → `bignint`, …). Anything else with no source name (e.g. an anonymous generic
/// instance) has no faithful CDDL spelling — a hard `Err` the emitter walk converts to an exclusion.
fn render_rust_ref(rule: &str, ident: &RustIdent, types: &IntermediateTypes) -> RenderResult {
    if let Some(source) = types.source_rule_name(ident) {
        return Ok(source.to_string());
    }
    if ident.to_string() == "Int" {
        return Ok("int".to_string());
    }
    if let Some(prelude) = types.prelude_cddl_name(ident) {
        return Ok(prelude);
    }
    Err(unrenderable(
        rule,
        format!("a reference to `{ident}` with no recorded source CDDL rule name"),
    ))
}

fn render_alias_ref(rule: &str, alias: &AliasIdent, types: &IntermediateTypes) -> RenderResult {
    match alias {
        // A reserved-prelude alias is the CDDL prelude name itself.
        AliasIdent::Reserved(name) => Ok(name.clone()),
        // A user alias resolves through the source-spelling registry to its original CDDL ident.
        AliasIdent::Rust(rust_ident) => render_rust_ref(rule, rust_ident, types),
    }
}

// --- Primitives & bounds -----------------------------------------------------------------------

fn render_primitive(rule: &str, p: Primitive, config: &RustTypeSerializeConfig) -> RenderResult {
    if let Some(window) = config.float_bounds {
        return render_float_primitive(rule, p, window);
    }
    let bounds = config.bounds;
    match p {
        Primitive::Bool => plain_primitive(rule, "bool", bounds),
        // float windows (the only bound a float carries) are handled above; a float with an INTEGER
        // window is an unexpected shape.
        Primitive::F32 => plain_primitive(rule, "float32", bounds),
        Primitive::F64 => plain_primitive(rule, "float64", bounds),
        Primitive::Str => render_text_or_bytes_size(rule, "tstr", bounds),
        Primitive::Bytes => render_text_or_bytes_size(rule, "bytes", bounds),
        // Fixed-width integer identities: no bare CDDL name — spelled as a provably-equivalent
        // size/range form (`uint .size 2` re-collapses to `u16`, etc.). Never carries extra bounds.
        Primitive::U8 => fixed_width_int(rule, "uint .size 1", bounds),
        Primitive::U16 => fixed_width_int(rule, "uint .size 2", bounds),
        Primitive::U32 => fixed_width_int(rule, "uint .size 4", bounds),
        Primitive::I8 => fixed_width_int(rule, "-128..127", bounds),
        Primitive::I16 => fixed_width_int(rule, "-32768..32767", bounds),
        Primitive::I32 => fixed_width_int(rule, "-2147483648..2147483647", bounds),
        // The wide/named integer types carry an explicit (inclusive-normalized) window.
        Primitive::U64 => render_int_bounds(rule, "uint", bounds),
        Primitive::I64 => render_int_bounds(rule, "int", bounds),
        Primitive::N64 => render_int_bounds(rule, "nint", bounds),
    }
}

/// A primitive whose only faithful spelling is its bare name; any window is an unexpected shape.
fn plain_primitive(
    rule: &str,
    name: &str,
    bounds: Option<(Option<i128>, Option<i128>)>,
) -> RenderResult {
    match bounds {
        None => Ok(name.to_string()),
        Some(_) => Err(unrenderable(
            rule,
            format!("`{name}` carrying an unexpected value bound"),
        )),
    }
}

/// A fixed-width integer identity: the collapsed type IS the constraint, so it carries no further
/// bounds. Its identity spelling (`text`) round-trips.
fn fixed_width_int(
    rule: &str,
    text: &str,
    bounds: Option<(Option<i128>, Option<i128>)>,
) -> RenderResult {
    match bounds {
        None => Ok(text.to_string()),
        Some(_) => Err(unrenderable(
            rule,
            format!("a fixed-width integer (`{text}`) carrying an unexpected extra bound"),
        )),
    }
}

/// `.size` on `tstr`/`bytes`: exact (`.size n`) or ranged (`.size (n..m)`). The parser normalizes a
/// bare `.size n` to the exact window `(Some(n), Some(n))` and a `.size (0..m)` to `(None, Some(m))`
/// (unsigned min-0 stripped). Anything else (a lone lower bound) has no faithful `.size` spelling.
fn render_text_or_bytes_size(
    rule: &str,
    base: &str,
    bounds: Option<(Option<i128>, Option<i128>)>,
) -> RenderResult {
    match bounds {
        None => Ok(base.to_string()),
        Some((Some(n), Some(m))) if n == m => Ok(format!("{base} .size {n}")),
        Some((Some(n), Some(m))) => Ok(format!("{base} .size ({n}..{m})")),
        Some((None, Some(m))) => Ok(format!("{base} .size (0..{m})")),
        Some((Some(n), None)) => Err(unrenderable(
            rule,
            format!(
                "`{base}` with a lower-only size bound (>= {n}) — no faithful `.size` spelling"
            ),
        )),
        Some((None, None)) => Ok(base.to_string()),
    }
}

/// Integer window on a wide/named type. Endpoints are inclusive (parser-normalized). One-sided →
/// `.ge`/`.le` (preserves the base typename). Two-sided → a literal range `a..b`, which round-trips
/// for `uint`/`int` (the literal sign re-derives the same base). Two-sided on `nint` has no faithful
/// literal-range form (a negative literal range parses as `int`), so it hard-errors.
fn render_int_bounds(
    rule: &str,
    base: &str,
    bounds: Option<(Option<i128>, Option<i128>)>,
) -> RenderResult {
    match bounds {
        None | Some((None, None)) => Ok(base.to_string()),
        Some((Some(a), None)) => Ok(format!("{base} .ge {a}")),
        Some((None, Some(b))) => Ok(format!("{base} .le {b}")),
        Some((Some(a), Some(b))) => {
            if base == "nint" {
                return Err(unrenderable(
                    rule,
                    format!(
                        "`nint` with a two-sided window ({a}..{b}) — no faithful CDDL spelling"
                    ),
                ));
            }
            Ok(format!("{a}..{b}"))
        }
    }
}

/// A NaN-safe float window `(min, max)`, each side `Some((value, exclusive))`. One-sided renders
/// `.ge`/`.gt`/`.le`/`.lt` (preserving the base typename). Two-sided renders a literal range (`..`
/// inclusive both sides, `...` exclusive both sides) — but a literal float range parses back as
/// `float64`, so a two-sided window on `float32` would change the wire precision and a
/// mixed-exclusivity window has no single-op form; both hard-error.
fn render_float_primitive(rule: &str, p: Primitive, window: FloatWindow) -> RenderResult {
    let base = match p {
        Primitive::F32 => "float32",
        Primitive::F64 => "float64",
        _ => {
            return Err(unrenderable(
                rule,
                "a float value window on a non-float primitive",
            ));
        }
    };
    match window {
        (Some((lo, lo_excl)), None) => {
            let op = if lo_excl { ".gt" } else { ".ge" };
            Ok(format!("{base} {op} {}", render_f64(lo)))
        }
        (None, Some((hi, hi_excl))) => {
            let op = if hi_excl { ".lt" } else { ".le" };
            Ok(format!("{base} {op} {}", render_f64(hi)))
        }
        (Some((lo, lo_excl)), Some((hi, hi_excl))) => {
            if p == Primitive::F32 {
                return Err(unrenderable(
                    rule,
                    "a two-sided window on `float32` — a literal float range parses as `float64`, \
                     changing the wire precision",
                ));
            }
            match (lo_excl, hi_excl) {
                (false, false) => Ok(format!("{}..{}", render_f64(lo), render_f64(hi))),
                (true, true) => Ok(format!("{}...{}", render_f64(lo), render_f64(hi))),
                _ => Err(unrenderable(
                    rule,
                    "a two-sided float window with mixed inclusive/exclusive endpoints — no \
                     single-operator CDDL form",
                )),
            }
        }
        (None, None) => Ok(base.to_string()),
    }
}

// --- Occurrence & fixed values -----------------------------------------------------------------

/// The CDDL occurrence marker for a container's count bounds: `*` (unbounded), `+` (`1*`, one+),
/// `n*m`, `n*`, `*m`. A float window is never valid on a container.
fn occurrence_marker(
    rule: &str,
    float_bounds: Option<FloatWindow>,
    bounds: Option<(Option<i128>, Option<i128>)>,
) -> RenderResult {
    if float_bounds.is_some() {
        return Err(unrenderable(
            rule,
            "a container carrying a float value window",
        ));
    }
    Ok(match bounds {
        None | Some((None, None)) => "*".to_string(),
        Some((Some(1), None)) => "+".to_string(),
        Some((Some(n), None)) => format!("{n}*"),
        Some((None, Some(m))) => format!("*{m}"),
        Some((Some(n), Some(m))) => format!("{n}*{m}"),
    })
}

/// A CDDL literal for a fixed value. Infallible: every `FixedValue` has a literal form.
fn render_fixed_value(fixed: &FixedValue) -> String {
    match fixed {
        FixedValue::Null => "null".to_string(),
        FixedValue::Bool(true) => "true".to_string(),
        FixedValue::Bool(false) => "false".to_string(),
        FixedValue::Uint(u) => u.to_string(),
        FixedValue::Nint(i) => i.to_string(),
        FixedValue::Float(f) => render_f64(*f),
        // `{:?}` yields a JSON-ish quoted/escaped literal, matching CDDL text-literal escaping for
        // the common cases (quotes, backslashes).
        FixedValue::Text(s) => format!("{s:?}"),
    }
}

/// Render an f64 as a CDDL float literal. `{:?}` keeps the decimal point on whole values (`3.0`, not
/// `3`) so the literal stays a float rather than degrading to an integer literal.
fn render_f64(f: f64) -> String {
    format!("{f:?}")
}

// --- The projection walk (the export emitter) --------------------------------------------------

/// The strict per-file header opting a machine-generated export into strict parsing (§2). Every
/// emitted file begins with this exact line; a physically-copied single file therefore still
/// carries its seam.
pub(crate) const EXTERN_INTERFACE_HEADER: &str = "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1";

/// The version-agnostic prefix of the seam header (everything before the ` v1` version token). A
/// flag-fed file whose first line starts with this prefix but is not exactly [`EXTERN_INTERFACE_HEADER`]
/// carries an UNSUPPORTED version (distinct diagnostic from a MISSING header); see the consumer-side
/// strict scan in `api::scan_extern_import_seam`.
pub(crate) const EXTERN_INTERFACE_HEADER_PREFIX: &str = "; _CDDL_CODEGEN_EXTERN_INTERFACE_";

/// The dep-side compiled self-check's assertion for an included row (commit 5). Derived from the
/// SAME projection the export emits, so the export and its self-check cannot drift.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ExternCheckKind {
    /// Opaque class-backed row (record / type-or-group choice / wrapper / hand-written extern):
    /// assert the type implements `Serialize` (and `Deserialize` where the dep generates one). Sound
    /// for hand-written externs too — the generated code emits `<Extern>::serialize`/`deserialize`
    /// calls, so the advertised bound is exactly the contract the hand-written type must satisfy.
    Serialize,
    /// A raw-bytes row: assert the type implements `RawBytesEncoding`.
    RawBytes,
    /// A transparent GROUP-BODY row (a materialized plain group exported as its `( … )` body). The
    /// dep generates a class for it, which the consumer's own generated code reaches through BOTH
    /// surfaces: whole-value `Serialize`/`Deserialize` (a group-choice arm splicing it calls
    /// `.serialize()`) AND the embedded-group surface (`SerializeEmbeddedGroup` /
    /// `DeserializeEmbeddedGroup` — a record member splicing it delegates inline). The self-check
    /// asserts all four bounds (each `Deserialize`-side gated on the dep actually generating one).
    EmbeddedGroup,
    /// A transparent row that materializes a named rust surface (a `pub type` alias, a c-style enum,
    /// a named collection): a `use crate::…::<Name> as _;` existence check.
    Use,
    /// No assertable self-check surface. Two cases: a transparent row that emits NO named rust type
    /// (`@no_alias` — the underlying definition is inlined, there is no `<Name>` on either side), and
    /// an exported generic-extern BASE (`ext_set<T> = _CDDL_CODEGEN_EXTERN_TYPE_` → the rust type is
    /// generic `ExtSet<T>`; bare `ExtSet` names no concrete type, so no bound is assertable on it —
    /// its instances are asserted individually). The export still carries the row; the self-check
    /// asserts nothing for it.
    None,
}

/// A successfully-projected rule staged for emission, plus the set of EXPORTED-rule idents its
/// exported CDDL text references (empty for opaque rows — their marker body is self-contained, and
/// for prelude/fixed refs — those render self-contained by prelude name). The reference set drives
/// reference-closure: if any referenced rule ends up excluded (or was never exported), this rule
/// dangles for every consumer and is excluded too. `check` is the dep-side self-check assertion this
/// row projects to (commit 5) — carried alongside the CDDL text so both consumers share the walk.
struct IncludedRule {
    components: Vec<String>,
    source: String,
    line: String,
    rule_refs: BTreeSet<RustIdent>,
    check: ExternCheckKind,
}

/// A rule kept OUT of the export, recorded as a sorted `; unexported: <ident> — <reason>` comment
/// after the header. `root` is the CDDL ident of the primary failure at the head of the reference
/// chain (the rule itself for a direct projection failure), so a transitively-excluded rule names
/// the original cause, not just its immediate neighbour.
struct ExcludedRule {
    components: Vec<String>,
    source: String,
    reason: String,
    root: String,
}

/// Project the finalized IR into the dep-side extern-interface export, keyed by path RELATIVE to
/// `<output>` (`extern-interface/<dep_key>/<scope-path>/mod.cddl`, sibling of `rust/`). One rule per
/// exported name; the dep's own extern-deps scopes are skipped (depth-1 rule). INFALLIBLE by design:
/// a rule whose projection fails (custom-serialize transparent alias, unrenderable shape, or — via
/// reference-closure — a reference to an unexportable name) is EXCLUDED-WITH-RECORD and generation
/// still succeeds, so a leaf/test spec that will never be a dependency still regenerates cleanly.
/// The failure surfaces later, only at a consumer that actually references an excluded ident.
///
/// The projection `match` over `RustStructType` is EXHAUSTIVE (no `_ =>` arm) so a future variant
/// forces an explicit export-spelling decision at compile time.
pub(crate) fn extern_interface_files(
    types: &IntermediateTypes,
    cli: &Cli,
) -> BTreeMap<String, String> {
    let (dep_key, included, excluded) = project_extern_interface(types, cli);
    render_export_files(&dep_key, &included, &excluded)
}

/// One entry of the dep-side compiled self-check (commit 5): the exported name, its scope-path
/// components, and the assertion kind — sorted deterministically by `RustIdent` (`BTreeMap`
/// iteration). Produced from the SAME projection walk `extern_interface_files` uses, so the export
/// and its self-check share one membership computation and cannot drift. The row's source CDDL
/// ident is deliberately NOT carried: the self-check emits no per-row comments (a comment on a
/// deletable row is a preservation-overlay trap — see `export.rs`), and each row's type path is
/// its own traceability.
pub(crate) struct ExternCheckEntry {
    pub components: Vec<String>,
    pub ident: RustIdent,
    pub kind: ExternCheckKind,
}

/// The self-check entries for every INCLUDED export row (excluded rows are asserted nothing — the
/// export never advertises them). Same projection as [`extern_interface_files`].
pub(crate) fn extern_interface_check_entries(
    types: &IntermediateTypes,
    cli: &Cli,
) -> Vec<ExternCheckEntry> {
    let (_dep_key, included, _excluded) = project_extern_interface(types, cli);
    included
        .into_iter()
        .map(|(ident, inc)| ExternCheckEntry {
            components: inc.components,
            kind: inc.check,
            ident,
        })
        .collect()
}

/// The shared projection walk: finalized IR → the included / excluded rule maps (plus the dep key).
/// Both the CDDL export ([`extern_interface_files`]) and the compiled self-check
/// ([`extern_interface_check_entries`]) consume this, so membership is computed once.
fn project_extern_interface(
    types: &IntermediateTypes,
    cli: &Cli,
) -> (
    String,
    BTreeMap<RustIdent, IncludedRule>,
    BTreeMap<RustIdent, ExcludedRule>,
) {
    let dep_key = cli.lib_name_code();
    let mut included: BTreeMap<RustIdent, IncludedRule> = BTreeMap::new();
    let mut excluded: BTreeMap<RustIdent, ExcludedRule> = BTreeMap::new();
    // The base idents of every generic EXTERN rule (`ExtSet` of `ExtSet<T>`). An exported
    // generic-extern base spells `ExtSet<T>` in rust; bare `ExtSet` names no concrete type, so the
    // self-check can assert no bound on it (its instances are asserted individually). Keyed on
    // `generic_extern_base_idents()` (the union of the parse-time record and the usage-site
    // instances) so a base with ZERO instances (`ext_unused<T>`) AND a base declared plain-but-used-
    // generic (`extern_generic<..>`, tests/core) both flip to `ExternCheckKind::None` instead of
    // emitting an E0107 `_assert_serialize::<ExtSet>()`.
    let generic_bases = types.generic_extern_base_idents();
    // Dedup across the two passes: a named collection / named generic-extern instance registers BOTH
    // a `rust_structs` entry AND a `type_aliases` entry; project each ident exactly once (pass 1
    // wins), so pass 2 skips anything pass 1 already staged.
    let mut seen: BTreeSet<RustIdent> = BTreeSet::new();

    // Pass 1 — `rust_structs`. The variant decides the spelling (exhaustive match).
    for (ident, rust_struct) in types.rust_structs() {
        let scope = types.scope(ident);
        // Exported scopes only: a dep's own deps never appear in its export.
        if !scope.export() {
            continue;
        }
        // Only top-level CDDL rules project; a struct synthesized during IR build (embedded record,
        // collection-keys wrapper, …) and the reserved prelude `int` extern carry no source rule
        // name and are not candidates.
        let Some(source) = types.source_rule_name(ident) else {
            continue;
        };
        seen.insert(ident.clone());
        let components = scope_path(scope);
        // `RustStructConfig` retains the custom-serialize annotations; rebuild the minimal
        // `RuleMetadata` the transparent renderer consults so the projection exclusion cannot be
        // bypassed on the class-backed transparent rows (Array/Table/CStyleEnum) or the plain-group
        // group-body rows.
        let md = rule_metadata_from_config(rust_struct.config());
        // Plain groups are inlined at use sites; a referenced one materializes here (a Record for a
        // heterogeneous body, an Array/Table/Wrapper for a homogeneous/newtype one). They are NOT an
        // opaque cross-crate class surface, so they never take the class-backed variant match below —
        // a materialized Record exports transparently as a group-body row, every other materialized
        // shape leaves a `; unexported:` record (Ask 0).
        if types.is_plain_group(ident) {
            let (projected, kind) = project_plain_group(source, rust_struct, &md, types);
            stage_rule(
                &mut included,
                &mut excluded,
                ident,
                source,
                components,
                projected,
                kind,
            );
            continue;
        }
        let (projected, kind): (RuleProjection, ExternCheckKind) = match rust_struct.variant() {
            // Genuinely class-backed types: opaque. `@newtype`/custom-(de)serialize/custom-json do
            // NOT travel — they shape the dep's internals; the consumer sees only "class exists,
            // named X". A generic-extern base carrying `@raw_bytes_flavor` re-exports the tag verbatim
            // (the flavor lives in `types.raw_bytes_flavor()`, keyed by the base ident). An opaque
            // marker body is self-contained, so it references nothing (never closure-excluded). The
            // self-check asserts `Serialize`(+`Deserialize`) on the concrete type — except an exported
            // generic-extern base, whose bare ident names no concrete type (`None`).
            RustStructType::Record(_)
            | RustStructType::TypeChoice { .. }
            | RustStructType::GroupChoice { .. }
            | RustStructType::Wrapper { .. }
            | RustStructType::Extern => {
                let mut annotations = Vec::new();
                if types.raw_bytes_flavor().contains(ident) {
                    annotations.push("@raw_bytes_flavor".to_string());
                }
                let check = if generic_bases.contains(ident) {
                    ExternCheckKind::None
                } else {
                    ExternCheckKind::Serialize
                };
                (
                    Ok((
                        crate::parsing::EXTERN_MARKER.to_string(),
                        annotations,
                        BTreeSet::new(),
                    )),
                    check,
                )
            }
            // A raw-bytes type is opaque behind its own marker.
            RustStructType::RawBytesType => (
                Ok((
                    crate::parsing::RAW_BYTES_MARKER.to_string(),
                    Vec::new(),
                    BTreeSet::new(),
                )),
                ExternCheckKind::RawBytes,
            ),
            // Named collections: transparent. The rust surface is a `pub type`, so spelling it opaque
            // would violate the fidelity contract — render the registered transparent alias body and
            // collect the rule idents it references for the closure. The self-check is a `use`
            // existence check on the `pub type`.
            RustStructType::Array { .. } | RustStructType::Table { .. } => {
                // Both representation-changing `@duplicates` placements travel verbatim: `reject` on a
                // set swaps the rust surface to a uniqueness twin (`OrderedSet`/`NonEmptyOrderedSet`),
                // and `preserve` on a table swaps it to a vec-of-pairs twin
                // (`PairMap`/`NonEmptyPairMap`). A consumer that rebuilds this rule without the
                // directive would embed the DEFAULT representation and skew across the seam: a
                // preserve-mode `Vec` that ACCEPTS what a reject dep rejects, or a reject-default
                // `BTreeMap` that REJECTS what a preserve dep preserves. The transparent alias's
                // `base_type` carries the policy (`with_duplicates_policy` at registration), and the
                // shape-aware predicates filter the two no-op defaults (set `preserve`, table
                // `reject`) so only the representation-changing halves project.
                let projected = match types.type_aliases().get(&AliasIdent::Rust(ident.clone())) {
                    Some(alias) => {
                        let dup_annotations = duplicates_annotation(&alias.base_type);
                        render_transparent_rule_body(source, &alias.base_type, Some(&md), types)
                            .map(|body| {
                                (
                                    body,
                                    dup_annotations,
                                    collect_rule_refs(&alias.base_type, types),
                                )
                            })
                    }
                    None => Err(unrenderable(
                        source,
                        "a named collection with no registered transparent alias",
                    )),
                };
                (projected, ExternCheckKind::Use)
            }
            // A c-style enum is transparent — its value choices (`0 / 1 / 2`) — but a real Rust enum
            // lives in the dep, so it still needs the `@rust_name` pin. Value choices reference no
            // rules. The self-check is a `use` existence check on the enum.
            RustStructType::CStyleEnum { variants } => (
                render_c_style_enum_body(source, variants, Some(&md), types)
                    .map(|body| (body, Vec::new(), BTreeSet::new())),
                ExternCheckKind::Use,
            ),
        };
        stage_rule(
            &mut included,
            &mut excluded,
            ident,
            source,
            components,
            projected,
            kind,
        );
    }

    // Pass 2 — `type_aliases`: the transparent aliases (`coin = uint`, alias chains, `@no_alias`
    // rules) with no `rust_structs` entry. Reserved prelude aliases (`uint`, `tstr`, …) are
    // `AliasIdent::Reserved` and carry no source name, so they never project.
    for (alias_ident, alias_info) in types.type_aliases() {
        let AliasIdent::Rust(ident) = alias_ident else {
            continue;
        };
        if seen.contains(ident) {
            continue;
        }
        let scope = types.scope(ident);
        if !scope.export() {
            continue;
        }
        let Some(source) = types.source_rule_name(ident) else {
            continue;
        };
        seen.insert(ident.clone());
        let components = scope_path(scope);
        // `@no_alias` travels verbatim: a truthful export makes the consumer's generator treat the
        // rule exactly as the dep's did.
        let mut extra_annotations = Vec::new();
        if alias_info
            .rule_metadata
            .as_ref()
            .is_some_and(|m| m.no_alias)
        {
            extra_annotations.push("@no_alias".to_string());
        }
        // A collection reaching pass 2 (rather than the pass-1 Array/Table arm) still carries its
        // policy on the alias base type; project the representation-changing halves (set `reject`,
        // table `preserve`) for the same anti-skew reason.
        extra_annotations.extend(duplicates_annotation(&alias_info.base_type));
        let projected: RuleProjection = if let Some(target) = &alias_info.wasm_alias_target {
            // A `ptm = mp` rule whose `Alias(mp, …)` wrapper was stripped to inline the type keeps a
            // `wasm_alias_target`; spell it truthfully as a reference to that target's original ident
            // rather than re-inlining the whole collection shape.
            render_rust_ref(source, target, types).map(|body| {
                let mut refs = BTreeSet::new();
                if types.source_rule_name(target).is_some() {
                    refs.insert(target.clone());
                }
                (body, extra_annotations, refs)
            })
        } else {
            render_transparent_rule_body(
                source,
                &alias_info.base_type,
                alias_info.rule_metadata.as_ref(),
                types,
            )
            .map(|body| {
                (
                    body,
                    extra_annotations,
                    collect_rule_refs(&alias_info.base_type, types),
                )
            })
        };
        // A transparent alias materializes a named rust surface (a `pub type`) only when
        // `gen_rust_alias` is set. A `@no_alias` rule (and a `wasm_alias_target` inline) generates no
        // rust type — nothing for the self-check to `use`, so it asserts nothing (`None`).
        let kind = if alias_info.gen_rust_alias {
            ExternCheckKind::Use
        } else {
            ExternCheckKind::None
        };
        stage_rule(
            &mut included,
            &mut excluded,
            ident,
            source,
            components,
            projected,
            kind,
        );
    }

    // Pass 3 — never-materialized plain groups. A plain group that is never referenced in the dep's
    // own spec materializes no `rust_structs` entry, so neither pass above saw it — but it IS a rule
    // in the dep's spec, and the excluded-with-record contract demands a trace (a consumer hitting the
    // undefined-reference path must not get the "regenerate the dependency" hint for a rule no regen
    // will ever produce). Record it. Guards mirror the passes above: exported scope only, a recorded
    // source rule name only, and skip anything already staged (a materialized group `seen` in pass 1).
    for ident in types.directly_defined_plain_group_idents() {
        if seen.contains(ident) {
            continue;
        }
        let scope = types.scope(ident);
        if !scope.export() {
            continue;
        }
        let Some(source) = types.source_rule_name(ident) else {
            continue;
        };
        seen.insert(ident.clone());
        excluded.insert(
            ident.clone(),
            ExcludedRule {
                components: scope_path(scope),
                source: source.to_string(),
                reason: "plain group never referenced in the dependency's own spec — no \
                         materialized shape to project"
                    .to_string(),
                root: source.to_string(),
            },
        );
    }

    // Reference-closure to fixpoint: consumers run the checked parse over the whole export, so a rule
    // whose exported body references an ident that is NOT itself exported (excluded, or never a
    // candidate — e.g. an extern-dep-scope rule) would dangle for EVERY consumer. Exclude it too,
    // naming the chain root. Monotone (only moves rules out of `included`), so it terminates;
    // deterministic (`BTreeMap` iteration, first offending ref in `BTreeSet` order).
    loop {
        let next = included.iter().find_map(|(ident, inc)| {
            inc.rule_refs
                .iter()
                .find(|r| !included.contains_key(*r))
                .map(|r| {
                    let root = excluded
                        .get(r)
                        .map(|e| e.root.clone())
                        .or_else(|| types.source_rule_name(r).map(str::to_owned))
                        .unwrap_or_else(|| r.to_string());
                    (ident.clone(), root)
                })
        });
        let Some((ident, root)) = next else {
            break;
        };
        let inc = included.remove(&ident).unwrap();
        excluded.insert(
            ident,
            ExcludedRule {
                components: inc.components,
                source: inc.source,
                reason: format!("references excluded {root}"),
                root,
            },
        );
    }

    (dep_key, included, excluded)
}

/// A per-rule projection: `Ok((body, extra annotations, referenced rule idents))` or an `Err` the
/// walk converts to an exclusion. The `@rust_name` pin is appended by `stage_rule`, so `extra
/// annotations` holds only the row-specific ones (`@no_alias`, `@raw_bytes_flavor`).
type RuleProjection = Result<(String, Vec<String>, BTreeSet<RustIdent>), ExternInterfaceError>;

/// Project a MATERIALIZED plain group (one referenced somewhere in the dep's own spec, so it has a
/// `rust_structs` entry). A plain group is inlined at its use sites — it is not an opaque cross-crate
/// class — so it never takes the class-backed variant match. A heterogeneous body materializes as a
/// `Record` and exports TRANSPARENTLY as a group-body row (the consumer re-derives the shape and
/// delegates the wire code to the dep's class through the whole-value + embedded-group surfaces);
/// every OTHER materialized shape (a homogeneous `Array`/`Table`, a `@newtype` `Wrapper`, and the
/// shapes a plain group can never actually take — probe-verified) has no embedded-group surface and
/// leaves a `; unexported:` record (Ask 0). The match over `RustStructType` is EXHAUSTIVE (module
/// discipline).
fn project_plain_group(
    source: &str,
    rust_struct: &RustStruct,
    md: &RuleMetadata,
    types: &IntermediateTypes,
) -> (RuleProjection, ExternCheckKind) {
    match rust_struct.variant() {
        RustStructType::Record(record) => (
            render_group_body(source, record, Some(md), types).map(|body| {
                // The group body references every field type; union their rule refs for the
                // reference-closure (nested plain-group refs included).
                let mut refs = BTreeSet::new();
                for field in &record.fields {
                    refs.extend(collect_rule_refs(&field.rust_type, types));
                }
                (body, Vec::new(), refs)
            }),
            ExternCheckKind::EmbeddedGroup,
        ),
        other => {
            let shape = match other {
                RustStructType::Record(_) => unreachable!("handled above"),
                RustStructType::Array { .. } => "a homogeneous array",
                RustStructType::Table { .. } => "a homogeneous table",
                RustStructType::Wrapper { .. } => "a @newtype wrapper",
                RustStructType::GroupChoice { .. } => "a group choice",
                RustStructType::TypeChoice { .. } => "a type choice",
                RustStructType::CStyleEnum { .. } => "a c-style enum",
                RustStructType::Extern => "an extern",
                RustStructType::RawBytesType => "a raw-bytes type",
            };
            (
                Err(unrenderable(
                    source,
                    format!(
                        "a plain group materialized as {shape} — no embedded-group surface to project"
                    ),
                )),
                ExternCheckKind::None,
            )
        }
    }
}

/// Stage one projected candidate: an `Ok` becomes an included rule (with the `@rust_name` pin
/// appended, carrying its self-check `kind`); an `Err` becomes a primary exclusion whose `root` is
/// the rule itself (an excluded row is never in the export, so its `kind` is irrelevant).
fn stage_rule(
    included: &mut BTreeMap<RustIdent, IncludedRule>,
    excluded: &mut BTreeMap<RustIdent, ExcludedRule>,
    ident: &RustIdent,
    source: &str,
    components: Vec<String>,
    projected: RuleProjection,
    kind: ExternCheckKind,
) {
    match projected {
        Ok((body, mut annotations, rule_refs)) => {
            annotations.push(format!("@rust_name {ident}"));
            included.insert(
                ident.clone(),
                IncludedRule {
                    components,
                    source: source.to_string(),
                    line: format_rule_line(source, &body, &annotations),
                    rule_refs,
                    check: kind,
                },
            );
        }
        Err(e) => {
            excluded.insert(
                ident.clone(),
                ExcludedRule {
                    components,
                    source: source.to_string(),
                    reason: exclusion_reason(&e),
                    root: source.to_string(),
                },
            );
        }
    }
}

/// The human-facing (never parsed) reason recorded for a primary exclusion.
fn exclusion_reason(err: &ExternInterfaceError) -> String {
    match err {
        ExternInterfaceError::CustomSerializeTransparent { annotation, .. } => format!(
            "{annotation} — plain export would make the consumer emit default wire logic, diverging \
             from the dependency's real format"
        ),
        ExternInterfaceError::Unrenderable { shape, .. } => format!("unrenderable shape: {shape}"),
    }
}

/// The EXPORTED-rule idents a transparent body references (for the reference-closure). Only idents
/// with a recorded source rule name count: a prelude ref (`bignint`) or the `int` extern renders
/// self-contained by its prelude name and needs no exported rule to resolve against, and a fixed
/// value references nothing. An `Alias(Rust(ident), _)` renders as the reference `ident` itself, so
/// its inner is deliberately NOT recursed into.
fn collect_rule_refs(ty: &RustType, types: &IntermediateTypes) -> BTreeSet<RustIdent> {
    fn walk(ty: &RustType, types: &IntermediateTypes, out: &mut BTreeSet<RustIdent>) {
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident)
            | ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => {
                if types.source_rule_name(ident).is_some() {
                    out.insert(ident.clone());
                }
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), _) => {}
            ConceptualRustType::Optional(inner) | ConceptualRustType::Array(inner) => {
                walk(inner, types, out)
            }
            ConceptualRustType::Map(key, value) => {
                walk(key, types, out);
                walk(value, types, out);
            }
            ConceptualRustType::Primitive(_) | ConceptualRustType::Fixed(_) => {}
        }
    }
    let mut out = BTreeSet::new();
    walk(ty, types, &mut out);
    out
}

/// Render the included rules and exclusion records into per-scope files. Each file is the header, a
/// sorted `; unexported:` block, then the sorted rule lines. The root file (`<dep_key>/mod.cddl`)
/// always emits — an empty surface still leaves a stable, header-only presence.
fn render_export_files(
    dep_key: &str,
    included: &BTreeMap<RustIdent, IncludedRule>,
    excluded: &BTreeMap<RustIdent, ExcludedRule>,
) -> BTreeMap<String, String> {
    // scope components -> sorted (source ident -> line / reason).
    let mut rules_by_scope: BTreeMap<Vec<String>, BTreeMap<String, String>> = BTreeMap::new();
    let mut excluded_by_scope: BTreeMap<Vec<String>, BTreeMap<String, String>> = BTreeMap::new();
    rules_by_scope.entry(Vec::new()).or_default();
    for inc in included.values() {
        rules_by_scope
            .entry(inc.components.clone())
            .or_default()
            .insert(inc.source.clone(), inc.line.clone());
    }
    for exc in excluded.values() {
        excluded_by_scope
            .entry(exc.components.clone())
            .or_default()
            .insert(exc.source.clone(), exc.reason.clone());
        // Ensure a scope carrying only exclusions still emits its file.
        rules_by_scope.entry(exc.components.clone()).or_default();
    }

    let mut all_scopes: BTreeSet<&Vec<String>> = BTreeSet::new();
    all_scopes.extend(rules_by_scope.keys());
    all_scopes.extend(excluded_by_scope.keys());

    let mut files = BTreeMap::new();
    for components in all_scopes {
        let subpath = if components.is_empty() {
            "mod.cddl".to_string()
        } else {
            format!("{}/mod.cddl", components.join("/"))
        };
        let mut content = String::from(EXTERN_INTERFACE_HEADER);
        content.push('\n');
        if let Some(records) = excluded_by_scope.get(components) {
            for (source, reason) in records {
                content.push_str(&format!("; unexported: {source} — {reason}\n"));
            }
        }
        if let Some(rules) = rules_by_scope.get(components) {
            for line in rules.values() {
                content.push_str(line);
                content.push('\n');
            }
        }
        files.insert(format!("extern-interface/{dep_key}/{subpath}"), content);
    }
    files
}

/// The scope-path components a scope's file lives under: empty for `ROOT_SCOPE` (the dep-root
/// `mod.cddl`), else the scope's own components (`a::c::foo` → `a/c/foo/mod.cddl`), mirroring the
/// generated rust tree so the "drop `mod` stems" consumer derivation recovers the scope.
fn scope_path(scope: &crate::intermediate::ModuleScope) -> Vec<String> {
    if *scope == *ROOT_SCOPE {
        Vec::new()
    } else {
        scope.components().clone()
    }
}

/// A `RuleMetadata` carrying only the custom-(de)serialize annotations `RustStructConfig` retains —
/// enough for the transparent renderer's projection exclusion check on the class-backed transparent
/// rows (named collections, c-style enums).
fn rule_metadata_from_config(config: &RustStructConfig) -> RuleMetadata {
    RuleMetadata {
        custom_serialize: config.custom_serialize.clone(),
        custom_deserialize: config.custom_deserialize.clone(),
        ..Default::default()
    }
}

/// The `@duplicates` annotation to project for a collection rule, or empty. Only the two
/// REPRESENTATION-CHANGING placements travel: `@duplicates reject` on an array/set (swaps the surface
/// to the `OrderedSet`/`NonEmptyOrderedSet` uniqueness twin) and `@duplicates preserve` on a table
/// (swaps it to the `PairMap`/`NonEmptyPairMap` vec-of-pairs twin). Omitting either would make a
/// consumer rebuild the DEFAULT representation and skew across the crate seam in one of two mirrored
/// directions: a dropped `reject` rebuilds a preserve-mode `Vec` that ACCEPTS the duplicates the dep
/// rejects; a dropped table `preserve` rebuilds a reject-default `BTreeMap` that REJECTS the
/// duplicates the dep preserves. Array `preserve` (the set default) and table `reject` (the map
/// default) are genuine no-ops on both sides — projecting them would only add export noise — so they
/// are shape-filtered out by the `is_reject_ordered_set`/`is_preserve_pair_map` predicates rather
/// than read off the bare policy flag.
fn duplicates_annotation(base_type: &RustType) -> Vec<String> {
    if base_type.is_reject_ordered_set() {
        vec!["@duplicates reject".to_string()]
    } else if base_type.is_preserve_pair_map() {
        vec!["@duplicates preserve".to_string()]
    } else {
        Vec::new()
    }
}

/// Assemble one export rule line: `<source> = <body> ; <annotations...>`. Every rule carries at
/// least the `@rust_name` pin (appended by `stage_rule`), so `annotations` is never empty.
fn format_rule_line(source: &str, body: &str, annotations: &[String]) -> String {
    format!("{source} = {body} ; {}", annotations.join(" "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intermediate::{CDDLIdent, VariantIdent};

    fn types_with_sources(sources: &[(&str, &str)]) -> IntermediateTypes<'static> {
        let mut types = IntermediateTypes::new();
        for (rust, cddl) in sources {
            types.mark_source_rule_name(RustIdent::new(CDDLIdent::new(*rust)), (*cddl).to_string());
        }
        types
    }

    fn prim(p: Primitive) -> RustType {
        RustType::new(ConceptualRustType::Primitive(p))
    }

    fn render(ty: &RustType, types: &IntermediateTypes) -> RenderResult {
        render_transparent_rule_body("test_rule", ty, None, types)
    }

    fn ok(ty: &RustType, types: &IntermediateTypes) -> String {
        render(ty, types).expect("expected a renderable shape")
    }

    // --- Primitives ---------------------------------------------------------------------------

    #[test]
    fn primitives_named() {
        let t = IntermediateTypes::new();
        assert_eq!(ok(&prim(Primitive::Bool), &t), "bool");
        assert_eq!(ok(&prim(Primitive::U64), &t), "uint");
        assert_eq!(ok(&prim(Primitive::N64), &t), "nint");
        assert_eq!(ok(&prim(Primitive::I64), &t), "int");
        assert_eq!(ok(&prim(Primitive::Str), &t), "tstr");
        assert_eq!(ok(&prim(Primitive::Bytes), &t), "bytes");
        assert_eq!(ok(&prim(Primitive::F32), &t), "float32");
        assert_eq!(ok(&prim(Primitive::F64), &t), "float64");
    }

    #[test]
    fn fixed_width_integers_render_equivalent_size_or_range() {
        let t = IntermediateTypes::new();
        // `uint .size K` for byte-aligned unsigned widths (round-trips: `.size 2` re-collapses to u16)
        assert_eq!(ok(&prim(Primitive::U8), &t), "uint .size 1");
        assert_eq!(ok(&prim(Primitive::U16), &t), "uint .size 2");
        assert_eq!(ok(&prim(Primitive::U32), &t), "uint .size 4");
        // explicit ranges for signed widths (no `int .size` form exists)
        assert_eq!(ok(&prim(Primitive::I8), &t), "-128..127");
        assert_eq!(ok(&prim(Primitive::I16), &t), "-32768..32767");
        assert_eq!(ok(&prim(Primitive::I32), &t), "-2147483648..2147483647");
    }

    // --- Integer value bounds ------------------------------------------------------------------

    #[test]
    fn integer_bounds_one_and_two_sided() {
        let t = IntermediateTypes::new();
        // one-sided preserves the base typename
        assert_eq!(
            ok(&prim(Primitive::U64).with_bounds((None, Some(1000))), &t),
            "uint .le 1000"
        );
        assert_eq!(
            ok(&prim(Primitive::U64).with_bounds((Some(5), None)), &t),
            "uint .ge 5"
        );
        assert_eq!(
            ok(&prim(Primitive::I64).with_bounds((Some(-10), None)), &t),
            "int .ge -10"
        );
        // two-sided uint/int → literal range (round-trips via literal sign)
        assert_eq!(
            ok(&prim(Primitive::U64).with_bounds((Some(5), Some(100))), &t),
            "5..100"
        );
        assert_eq!(
            ok(&prim(Primitive::I64).with_bounds((Some(-5), Some(100))), &t),
            "-5..100"
        );
    }

    // --- Text / bytes sizes --------------------------------------------------------------------

    #[test]
    fn text_and_bytes_sizes() {
        let t = IntermediateTypes::new();
        assert_eq!(
            ok(
                &prim(Primitive::Bytes).with_bounds((Some(32), Some(32))),
                &t
            ),
            "bytes .size 32"
        );
        assert_eq!(
            ok(&prim(Primitive::Str).with_bounds((Some(1), Some(64))), &t),
            "tstr .size (1..64)"
        );
        // `.size (0..m)` form (parser strips the unsigned min-0, storing (None, Some(m)))
        assert_eq!(
            ok(&prim(Primitive::Bytes).with_bounds((None, Some(8))), &t),
            "bytes .size (0..8)"
        );
    }

    // --- Fixed values --------------------------------------------------------------------------

    #[test]
    fn fixed_values() {
        assert_eq!(render_fixed_value(&FixedValue::Null), "null");
        assert_eq!(render_fixed_value(&FixedValue::Bool(true)), "true");
        assert_eq!(render_fixed_value(&FixedValue::Bool(false)), "false");
        assert_eq!(render_fixed_value(&FixedValue::Uint(7)), "7");
        assert_eq!(render_fixed_value(&FixedValue::Nint(-3)), "-3");
        assert_eq!(render_fixed_value(&FixedValue::Float(3.0)), "3.0");
        assert_eq!(render_fixed_value(&FixedValue::Float(1.5)), "1.5");
        assert_eq!(
            render_fixed_value(&FixedValue::Text("abc".to_string())),
            "\"abc\""
        );
    }

    #[test]
    fn standalone_fixed_value_type() {
        let t = IntermediateTypes::new();
        assert_eq!(
            ok(
                &RustType::new(ConceptualRustType::Fixed(FixedValue::Uint(0))),
                &t
            ),
            "0"
        );
    }

    // --- Tags & .cbor --------------------------------------------------------------------------

    #[test]
    fn tag_wraps_inner() {
        let t = types_with_sources(&[("Foo", "foo")]);
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Foo",
        ))))
        .tag(24);
        assert_eq!(ok(&ty, &t), "#6.24(foo)");
    }

    #[test]
    fn tag_over_primitive() {
        let t = IntermediateTypes::new();
        assert_eq!(ok(&prim(Primitive::U64).tag(2), &t), "#6.2(uint)");
    }

    #[test]
    fn cbor_bytes_wraps_inner() {
        let t = types_with_sources(&[("Foo", "foo")]);
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Foo",
        ))))
        .as_bytes();
        assert_eq!(ok(&ty, &t), "bytes .cbor foo");
    }

    #[test]
    fn tag_over_cbor_bytes() {
        // `#6.24(bytes .cbor foo)` — encodings [CBORBytes, Tagged(24)], last (tag) outermost.
        let t = types_with_sources(&[("Foo", "foo")]);
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Foo",
        ))))
        .as_bytes()
        .tag(24);
        assert_eq!(ok(&ty, &t), "#6.24(bytes .cbor foo)");
    }

    // --- Optional ------------------------------------------------------------------------------

    #[test]
    fn optional_renders_or_null() {
        let t = IntermediateTypes::new();
        let ty = RustType::new(ConceptualRustType::Optional(Box::new(prim(Primitive::U64))));
        assert_eq!(ok(&ty, &t), "uint / null");
    }

    #[test]
    fn optional_of_named_ref() {
        let t = types_with_sources(&[("Foo", "foo")]);
        let inner = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Foo",
        ))));
        let ty = RustType::new(ConceptualRustType::Optional(Box::new(inner)));
        assert_eq!(ok(&ty, &t), "foo / null");
    }

    // --- Named references ----------------------------------------------------------------------

    #[test]
    fn rust_ref_uses_source_name() {
        let t = types_with_sources(&[("MyRule", "my-rule")]);
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "MyRule",
        ))));
        // original CDDL spelling (with the hyphen), not the camel-cased ident
        assert_eq!(ok(&ty, &t), "my-rule");
    }

    #[test]
    fn alias_ref_uses_source_name() {
        let t = types_with_sources(&[("Coin", "coin")]);
        let ty = RustType::new(ConceptualRustType::Alias(
            AliasIdent::Rust(RustIdent::new(CDDLIdent::new("Coin"))),
            Box::new(ConceptualRustType::Primitive(Primitive::U64)),
        ));
        assert_eq!(ok(&ty, &t), "coin");
    }

    #[test]
    fn reserved_int_ref_renders_int() {
        let t = IntermediateTypes::new();
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Int",
        ))));
        assert_eq!(ok(&ty, &t), "int");
    }

    #[test]
    fn unknown_rust_ref_hard_errors() {
        let t = IntermediateTypes::new();
        let ty = RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            "Unknown",
        ))));
        let err = render(&ty, &t).unwrap_err();
        match err {
            ExternInterfaceError::Unrenderable { rule, shape } => {
                assert_eq!(rule, "test_rule");
                assert!(shape.contains("Unknown"), "shape names the ref: {shape}");
            }
            other => panic!("expected Unrenderable, got {other:?}"),
        }
    }

    // --- Arrays / maps with occurrence bounds --------------------------------------------------

    fn array_of(inner: RustType, bounds: Option<(Option<i128>, Option<i128>)>) -> RustType {
        let mut ty = RustType::new(ConceptualRustType::Array(Box::new(inner)));
        if let Some(b) = bounds {
            ty = ty.with_bounds(b);
        }
        ty
    }

    #[test]
    fn array_unbounded_and_occurrence() {
        let t = types_with_sources(&[("Foo", "foo")]);
        let foo = || {
            RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
                "Foo",
            ))))
        };
        assert_eq!(ok(&array_of(foo(), None), &t), "[* foo]");
        assert_eq!(ok(&array_of(foo(), Some((Some(1), None))), &t), "[+ foo]");
        assert_eq!(
            ok(&array_of(foo(), Some((Some(2), Some(5)))), &t),
            "[2*5 foo]"
        );
        assert_eq!(ok(&array_of(foo(), Some((Some(3), None))), &t), "[3* foo]");
        assert_eq!(ok(&array_of(foo(), Some((None, Some(4)))), &t), "[*4 foo]");
    }

    #[test]
    fn array_of_primitive() {
        let t = IntermediateTypes::new();
        assert_eq!(ok(&array_of(prim(Primitive::U64), None), &t), "[* uint]");
    }

    #[test]
    fn nested_array() {
        let t = IntermediateTypes::new();
        let inner = array_of(prim(Primitive::U64), None);
        assert_eq!(
            ok(&array_of(inner, Some((Some(1), None))), &t),
            "[+ [* uint]]"
        );
    }

    #[test]
    fn map_with_occurrence() {
        let t = IntermediateTypes::new();
        let mut ty = RustType::new(ConceptualRustType::Map(
            Box::new(prim(Primitive::Str)),
            Box::new(prim(Primitive::U64)),
        ));
        assert_eq!(ok(&ty, &t), "{* tstr => uint}");
        ty = RustType::new(ConceptualRustType::Map(
            Box::new(prim(Primitive::Str)),
            Box::new(prim(Primitive::U64)),
        ))
        .with_bounds((Some(1), None));
        assert_eq!(ok(&ty, &t), "{+ tstr => uint}");
    }

    #[test]
    fn tagged_named_collection() {
        // A named collection alias carrying a tag: `#6.30([* uint])`.
        let t = IntermediateTypes::new();
        let ty = array_of(prim(Primitive::U64), None).tag(30);
        assert_eq!(ok(&ty, &t), "#6.30([* uint])");
    }

    // --- C-style enum --------------------------------------------------------------------------

    fn fixed_variant(name: &str, value: FixedValue) -> EnumVariant {
        EnumVariant::new(
            VariantIdent::new_custom(name),
            RustType::new(ConceptualRustType::Fixed(value)),
            false,
            None,
        )
    }

    #[test]
    fn c_style_enum_value_choices() {
        let t = IntermediateTypes::new();
        let variants = vec![
            fixed_variant("I0", FixedValue::Uint(0)),
            fixed_variant("I1", FixedValue::Uint(1)),
            fixed_variant("I2", FixedValue::Uint(2)),
        ];
        assert_eq!(
            render_c_style_enum_body("fe", &variants, None, &t).unwrap(),
            "0 / 1 / 2"
        );
    }

    #[test]
    fn c_style_enum_empty_hard_errors() {
        let t = IntermediateTypes::new();
        render_c_style_enum_body("fe", &[], None, &t).unwrap_err();
    }

    #[test]
    fn c_style_enum_non_fixed_variant_hard_errors() {
        let t = IntermediateTypes::new();
        let variants = vec![EnumVariant::new(
            VariantIdent::new_custom("X"),
            RustType::new(ConceptualRustType::Primitive(Primitive::U64)),
            false,
            None,
        )];
        render_c_style_enum_body("fe", &variants, None, &t).unwrap_err();
    }

    // --- Custom-serialize projection hard error ------------------------------------------------

    fn metadata_with_custom_serialize() -> RuleMetadata {
        RuleMetadata {
            custom_serialize: Some("my_ser".to_string()),
            ..Default::default()
        }
    }

    fn metadata_with_custom_deserialize() -> RuleMetadata {
        RuleMetadata {
            custom_deserialize: Some("my_deser".to_string()),
            ..Default::default()
        }
    }

    #[test]
    fn custom_serialize_on_transparent_alias_hard_errors() {
        let t = IntermediateTypes::new();
        let md = metadata_with_custom_serialize();
        let err =
            render_transparent_rule_body("coin", &prim(Primitive::U64), Some(&md), &t).unwrap_err();
        match err {
            ExternInterfaceError::CustomSerializeTransparent { rule, annotation } => {
                assert_eq!(rule, "coin");
                assert_eq!(annotation, "@custom_serialize");
            }
            other => panic!("expected CustomSerializeTransparent, got {other:?}"),
        }
    }

    #[test]
    fn custom_deserialize_on_transparent_alias_hard_errors() {
        let t = IntermediateTypes::new();
        let md = metadata_with_custom_deserialize();
        let err =
            render_transparent_rule_body("coin", &prim(Primitive::U64), Some(&md), &t).unwrap_err();
        assert!(matches!(
            err,
            ExternInterfaceError::CustomSerializeTransparent { .. }
        ));
    }

    #[test]
    fn custom_serialize_on_c_style_enum_hard_errors() {
        let t = IntermediateTypes::new();
        let md = metadata_with_custom_serialize();
        let variants = vec![fixed_variant("I0", FixedValue::Uint(0))];
        render_c_style_enum_body("fe", &variants, Some(&md), &t).unwrap_err();
    }

    // --- Float value windows -------------------------------------------------------------------

    fn float_windowed(p: Primitive, window: FloatWindow) -> RustType {
        RustType::new(ConceptualRustType::Primitive(p)).with_float_bounds(window)
    }

    #[test]
    fn float_one_sided_windows() {
        let t = IntermediateTypes::new();
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (None, Some((10.5, false)))),
                &t
            ),
            "float64 .le 10.5"
        );
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (None, Some((10.5, true)))),
                &t
            ),
            "float64 .lt 10.5"
        );
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (Some((0.5, false)), None)),
                &t
            ),
            "float64 .ge 0.5"
        );
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (Some((0.5, true)), None)),
                &t
            ),
            "float64 .gt 0.5"
        );
        // base typename preserved on the F32 one-sided case
        assert_eq!(
            ok(
                &float_windowed(Primitive::F32, (None, Some((10.5, false)))),
                &t
            ),
            "float32 .le 10.5"
        );
    }

    #[test]
    fn float_two_sided_windows() {
        let t = IntermediateTypes::new();
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (Some((0.5, false)), Some((10.5, false)))),
                &t
            ),
            "0.5..10.5"
        );
        assert_eq!(
            ok(
                &float_windowed(Primitive::F64, (Some((0.5, true)), Some((10.5, true)))),
                &t
            ),
            "0.5...10.5"
        );
    }

    #[test]
    fn float_two_sided_f32_is_lossy_hard_error() {
        // A literal float range parses back as float64, so a two-sided float32 window cannot be
        // spelled without changing wire precision — hard error rather than a lossy spelling.
        let t = IntermediateTypes::new();
        render(
            &float_windowed(Primitive::F32, (Some((0.5, false)), Some((10.5, false)))),
            &t,
        )
        .unwrap_err();
    }

    #[test]
    fn float_two_sided_mixed_exclusivity_hard_error() {
        let t = IntermediateTypes::new();
        render(
            &float_windowed(Primitive::F64, (Some((0.5, false)), Some((10.5, true)))),
            &t,
        )
        .unwrap_err();
    }

    // --- Plain-group group bodies --------------------------------------------------------------

    fn field(name: &str, ty: RustType, optional: bool, key: Option<FixedValue>) -> RustField {
        RustField::new(name.to_string(), ty, optional, key, RuleMetadata::default())
    }

    fn record(rep: Representation, fields: Vec<RustField>) -> RustRecord {
        RustRecord { rep, fields }
    }

    fn rust_ref(rust: &str) -> RustType {
        RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(
            rust,
        ))))
    }

    /// Array-rep body: a fixed tag renders its literal, a named ref renders its source ident, an
    /// optional field takes `? `, and a `.default` re-appends in member position.
    #[test]
    fn group_body_array_rep_tag_ref_optional_default() {
        let t = types_with_sources(&[("Credential", "credential")]);
        let rec = record(
            Representation::Array,
            vec![
                field(
                    "tag",
                    RustType::new(ConceptualRustType::Fixed(FixedValue::Uint(0))),
                    false,
                    None,
                ),
                field("credential", rust_ref("Credential"), false, None),
                field(
                    "count",
                    prim(Primitive::U64).default(FixedValue::Uint(0)),
                    true,
                    None,
                ),
            ],
        );
        assert_eq!(
            render_group_body("cert", &rec, None, &t).unwrap(),
            "(tag: 0, credential: credential, ? count: uint .default 0)"
        );
    }

    /// Map-rep body: a `Uint` key renders `n:`, a bareword-valid `Text` key renders `s:`, and a
    /// non-bareword `Text` key falls back to the quoted `"s":` form.
    #[test]
    fn group_body_map_rep_uint_and_text_keys() {
        let t = IntermediateTypes::new();
        let rec = record(
            Representation::Map,
            vec![
                field("a", prim(Primitive::U64), false, Some(FixedValue::Uint(1))),
                field(
                    "b",
                    prim(Primitive::Str),
                    false,
                    Some(FixedValue::Text("foo".to_string())),
                ),
                field(
                    "c",
                    prim(Primitive::Bool),
                    false,
                    Some(FixedValue::Text("has space".to_string())),
                ),
            ],
        );
        assert_eq!(
            render_group_body("m", &rec, None, &t).unwrap(),
            "(1: uint, foo: tstr, \"has space\": bool)"
        );
    }

    /// A map-rep member with no key has no faithful member spelling — hard `Err`.
    #[test]
    fn group_body_map_rep_missing_key_hard_errors() {
        let t = IntermediateTypes::new();
        let rec = record(
            Representation::Map,
            vec![field("a", prim(Primitive::U64), false, None)],
        );
        render_group_body("m", &rec, None, &t).unwrap_err();
    }

    /// A nested plain-group reference renders by its source ident (the closure keeps it alive).
    #[test]
    fn group_body_nested_ref_renders_source_ident() {
        let t = types_with_sources(&[("Inner", "inner")]);
        let rec = record(
            Representation::Array,
            vec![field("inner", rust_ref("Inner"), false, None)],
        );
        assert_eq!(
            render_group_body("outer", &rec, None, &t).unwrap(),
            "(inner: inner)"
        );
    }

    /// A member type the renderer cannot spell (an unknown ref) propagates as a hard `Err`.
    #[test]
    fn group_body_unrenderable_member_hard_errors() {
        let t = IntermediateTypes::new();
        let rec = record(
            Representation::Array,
            vec![field("x", rust_ref("Unknown"), false, None)],
        );
        render_group_body("g", &rec, None, &t).unwrap_err();
    }

    /// A `@custom_serialize` plain group is a projection hard `Err` (its plain body would make the
    /// consumer emit default wire logic).
    #[test]
    fn group_body_custom_serialize_hard_errors() {
        let t = IntermediateTypes::new();
        let md = metadata_with_custom_serialize();
        let rec = record(
            Representation::Array,
            vec![field("a", prim(Primitive::U64), false, None)],
        );
        render_group_body("g", &rec, Some(&md), &t).unwrap_err();
    }
}
