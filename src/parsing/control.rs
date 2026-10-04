use super::{
    length_window, literal_head_spelling, mixed_int_float_range_rejection,
    non_literal_control_operand_rejection, non_literal_range_bound_rejection,
    non_literal_size_operand_rejection, rust_type_from_type2,
};
use crate::cli::Cli;
use crate::comment_ast::RuleMetadata;
use crate::intermediate::{
    AliasIdent, AliasInfo, CDDLIdent, ConceptualRustType, FixedValue, FloatWindow, IntBounds,
    IntWindow, IntermediateTypes, Primitive, RustIdent, RustStruct, RustType,
};
use cddl::ast::parent::ParentVisitor;
use cddl::ast::{Operator, RangeCtlOp, Type2};
use cddl::token;

#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub(super) enum ControlOperator {
    Range(IntWindow),
    /// A NaN-safe float value window (`float64 .le 10.5`, `0.5..10.5`, `float .le 10`). Carries
    /// per-side exclusivity because float space is dense (no ±1 collapse like the integer window).
    RangeFloat(FloatWindow),
    CBOR(RustType),
    Default(FixedValue),
}

pub(super) fn ident_to_primitive(ident: &CDDLIdent) -> Option<Primitive> {
    // TODO: what about aliases that resolve to these? is it even possible to know this at this stage?
    match ident.to_string().as_str() {
        "tstr" | "text" => Some(Primitive::Str),
        "bstr" | "bytes" => Some(Primitive::Bytes),
        "int" => Some(Primitive::I64),
        "uint" => Some(Primitive::U64),
        "nint" => Some(Primitive::N64),
        // One primitive per float prelude name: each names a different set of float VALUES.
        "float16" => Some(Primitive::F16),
        "float32" => Some(Primitive::F32),
        "float64" => Some(Primitive::F64),
        "float16-32" => Some(Primitive::F16To32),
        "float32-64" => Some(Primitive::F32To64),
        "float" => Some(Primitive::Float),
        _other => None,
    }
}

/// The integer a value-comparison control operand denotes, or the graceful-rejection message for
/// one the integer window cannot hold. Decimal floats never arrive: `try_float_or_reject`
/// intercepts them first. Integral floats outside the CDDL int/uint literal range are refused.
pub(super) fn control_operand_integer(
    rule_name: Option<&RustIdent>,
    ctrl: token::ControlOperator,
    operand: &Type2,
) -> Result<i128, String> {
    match operand {
        Type2::UintValue { value, .. } => Ok(*value as i128),
        Type2::IntValue { value, .. } => Ok(*value as i128),
        Type2::FloatValue { value, .. } => {
            // Outside the CDDL int/uint literal range, `as i128` saturates into a bogus window.
            if (-9_223_372_036_854_775_808.0..18_446_744_073_709_551_616.0).contains(value) {
                Ok(*value as i128)
            } else {
                Err(format!(
                    "{}float bound `{value:?}` on an integer-typed head is outside the integer range — an \
                     integral float bound must lie within -9223372036854775808..18446744073709551615; use an \
                     integer literal bound or a float head (float64)",
                    reject_rule_prefix(rule_name)
                ))
            }
        }
        _ => Err(non_literal_control_operand_rejection(
            rule_name, ctrl, operand,
        )),
    }
}

/// The value of an INTEGER literal `Type2` (uint/int) as i128, or `None` for anything else — float
/// literals included, which the `.size` arm refuses (`size_operand_float_literal`) before it
/// reads any bound, so no float is ever truncated into an integer window here.
fn int_literal_to_i128(type2: &Type2) -> Option<i128> {
    match type2 {
        Type2::UintValue { value, .. } => Some(*value as i128),
        Type2::IntValue { value, .. } => Some(*value as i128),
        _ => None,
    }
}

/// The first float literal a `.size` operand spells: the bare operand (`.size 2.5`) or either bound
/// of its parenthesized form (`.size (1.5..2)`, `.size (1.5)`).
fn size_operand_float_literal(operand: &Type2) -> Option<f64> {
    let float = |t: &Type2| match t {
        Type2::FloatValue { value, .. } => Some(*value),
        _ => None,
    };
    match operand {
        Type2::ParenthesizedType { pt, .. } => pt.type_choices.iter().find_map(|choice| {
            float(&choice.type1.type2).or_else(|| {
                choice
                    .type1
                    .operator
                    .as_ref()
                    .and_then(|op| float(&op.type2))
            })
        }),
        other => float(other),
    }
}

/// The numeric value of a literal `Type2` (uint/int/float) as f64, or `None` if it isn't a number
/// literal. Used to build float windows without truncation (ints promote to f64 losslessly here).
fn type2_to_f64(type2: &Type2) -> Option<f64> {
    match type2 {
        Type2::UintValue { value, .. } => Some(*value as f64),
        Type2::IntValue { value, .. } => Some(*value as f64),
        Type2::FloatValue { value, .. } => Some(*value),
        _ => None,
    }
}

/// Whether a literal `Type2` is a DECIMAL float (non-integer-valued), e.g. `10.5`. A whole float
/// (`10.0`) or an int literal is not decimal — those keep the integer window path.
pub(super) fn type2_is_decimal_float(type2: &Type2) -> bool {
    matches!(type2, Type2::FloatValue { value, .. } if value.fract() != 0.0)
}

/// The rust primitive a control HEAD denotes once aliases resolve: a prelude name (`uint`,
/// `float64`) or a rule that is a transparent alias of one (`f = float64`, forward references and
/// chains included — rules parse in dependency order), looking through bare single-type parentheses
/// (`(f)`). `None` for a literal, a generic parameter, a generic instance, and any name that resolves
/// to something other than a bare primitive (a record, an enum, or a wrapper that a tag, a window or
/// `@newtype` made nominal).
pub(super) fn resolved_head_primitive(
    types: &IntermediateTypes,
    head: &Type2,
) -> Option<Primitive> {
    let mut head = head;
    while let Type2::ParenthesizedType { pt, .. } = head
        && let [only] = pt.type_choices.as_slice()
        && only.type1.operator.is_none()
    {
        head = &only.type1.type2;
    }
    let Type2::Typename {
        ident,
        generic_args: None,
        ..
    } = head
    else {
        return None;
    };
    let cddl_ident = CDDLIdent::new(ident.to_string());
    if let Some(primitive) = ident_to_primitive(&cddl_ident) {
        return Some(primitive);
    }
    if types.active_generic_param_binding(ident.ident).is_some() {
        return None;
    }
    let resolved = types.resolve_alias(&AliasIdent::new(cddl_ident))?;
    if resolved.config.bounds.is_some() || resolved.config.float_bounds.is_some() {
        return None;
    }
    match resolved.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Primitive(primitive) => Some(*primitive),
        _ => None,
    }
}

/// Numeric classification of a range/control HEAD (the `type2` left of the operator).
#[derive(Clone, Copy, PartialEq)]
enum HeadNumeric {
    /// a float primitive typename (`float16`/`float32`/`float64`) or a float literal (`0.5`)
    Float,
    /// an integer primitive typename (`uint`/`int`/`nint`)
    NamedInt,
    /// an integer literal (`0`, `-3`) — the head of a top-level literal range rule
    IntLiteral,
    Other,
}

fn head_numeric(types: &IntermediateTypes, type2: &Type2) -> HeadNumeric {
    match type2 {
        Type2::Typename { .. } | Type2::ParenthesizedType { .. } => {
            match resolved_head_primitive(types, type2) {
                Some(p) if p.is_float() => HeadNumeric::Float,
                Some(Primitive::U64) | Some(Primitive::N64) | Some(Primitive::I64) => {
                    HeadNumeric::NamedInt
                }
                _ => HeadNumeric::Other,
            }
        }
        Type2::FloatValue { .. } => HeadNumeric::Float,
        Type2::UintValue { .. } | Type2::IntValue { .. } => HeadNumeric::IntLiteral,
        _ => HeadNumeric::Other,
    }
}

/// Message naming the offending rule for a graceful rejection on the rule-position control-operator
/// path — float windows, `.ne`, nested `.cbor`. `None` (member position) omits the rule name; the op
/// + remedy still make the message actionable.
pub(super) fn reject_rule_prefix(rule_name: Option<&RustIdent>) -> String {
    match rule_name {
        Some(r) => format!("rule `{r}`: "),
        None => String::new(),
    }
}

/// Intercepts the float-window and graceful-rejection cases of a numeric range/control operator,
/// BEFORE the integer arms of `parse_control_operator` run (so an integer arm never casts a
/// decimal operand). Returns:
/// - `Some(RangeFloat(window))` when the constraint is a float window over a float-typed head;
/// - `Some(Range((None, None)))` (a harmless placeholder) after RECORDING a graceful rejection for
///   an unsupported shape (`.ne` over a float, a decimal float bound on an integer-typed head, or
///   a range mixing integer and float literal endpoints);
/// - `None` when this is a genuine integer constraint (or a non-value op like `.size`/`.cbor`),
///   which the caller then handles on the existing integer path.
fn try_float_or_reject(
    types: &mut IntermediateTypes,
    type2: &Type2,
    operator: &Operator,
    rule_name: Option<&RustIdent>,
) -> Option<ControlOperator> {
    let head = head_numeric(types, type2);
    match operator.operator {
        RangeCtlOp::RangeOp { is_inclusive, .. } => {
            let is_int = |t: &Type2| matches!(t, Type2::UintValue { .. } | Type2::IntValue { .. });
            let is_float = |t: &Type2| matches!(t, Type2::FloatValue { .. });
            if (is_int(type2) && is_float(&operator.type2))
                || (is_float(type2) && is_int(&operator.type2))
            {
                types.record_rejection(mixed_int_float_range_rejection(
                    rule_name,
                    type2,
                    &operator.type2,
                    is_inclusive,
                ));
                return Some(ControlOperator::Range((None, None)));
            }
            let decimal_endpoint =
                type2_is_decimal_float(type2) || type2_is_decimal_float(&operator.type2);
            let is_float = head == HeadNumeric::Float || decimal_endpoint;
            if !is_float {
                return None;
            }
            match (type2_to_f64(type2), type2_to_f64(&operator.type2)) {
                (Some(start), Some(end)) => Some(ControlOperator::RangeFloat((
                    // range lower endpoint is always included; upper is excluded only for `a...b`
                    Some((start, false)),
                    Some((end, !is_inclusive)),
                ))),
                _ => {
                    // a decimal float endpoint against a non-literal (e.g. named-int) head has no
                    // representable numeric partner — reject gracefully instead of panicking.
                    types.record_rejection(format!(
                        "{}decimal float bound in a range against a non-numeric-literal head is unsupported — use a float head (float64) or integer bounds",
                        reject_rule_prefix(rule_name)
                    ));
                    Some(ControlOperator::Range((None, None)))
                }
            }
        }
        RangeCtlOp::CtlOp { ctrl, .. } => {
            use token::ControlOperator as Ctl;
            // only the value-comparison control ops map onto a float window
            if !matches!(
                ctrl,
                Ctl::EQ | Ctl::NE | Ctl::LE | Ctl::LT | Ctl::GE | Ctl::GT
            ) {
                return None;
            }
            let operand = &operator.type2;
            if head == HeadNumeric::Float {
                if matches!(ctrl, Ctl::NE) {
                    // single-value exclusion has no principled float window (the integer min>max
                    // ±1 hack is meaningless in dense float space) — reject gracefully.
                    types.record_rejection(format!(
                        "{}`.ne` on a float value is unsupported — single-value float exclusion has no representable window; use a range or remove the constraint",
                        reject_rule_prefix(rule_name)
                    ));
                    return Some(ControlOperator::Range((None, None)));
                }
                let Some(v) = type2_to_f64(operand) else {
                    types.record_rejection(non_literal_control_operand_rejection(
                        rule_name, ctrl, operand,
                    ));
                    return Some(ControlOperator::Range((None, None)));
                };
                let window = match ctrl {
                    Ctl::EQ => (Some((v, false)), Some((v, false))),
                    Ctl::LE => (None, Some((v, false))),
                    Ctl::LT => (None, Some((v, true))),
                    Ctl::GE => (Some((v, false)), None),
                    Ctl::GT => (Some((v, true)), None),
                    _ => unreachable!(),
                };
                return Some(ControlOperator::RangeFloat(window));
            }
            // integer-typed head with a DECIMAL float bound: do not silently floor — reject.
            if type2_is_decimal_float(operand) {
                types.record_rejection(decimal_integer_control_operand_rejection(
                    rule_name, operand,
                ));
                return Some(ControlOperator::Range((None, None)));
            }
            None
        }
    }
}

pub(super) fn decimal_integer_control_operand_rejection(
    rule_name: Option<&RustIdent>,
    operand: &Type2,
) -> String {
    format!(
        "{}decimal float bound `{}` on an integer-typed head is unsupported — use an integer bound or a float head (float64)",
        reject_rule_prefix(rule_name),
        match operand {
            Type2::FloatValue { value, .. } => *value,
            _ => 0.0,
        }
    )
}

/// The literal a control operand denotes, or `None` when the operand is not a literal at all.
///
/// `None` is an ordinary user input (`? f: uint .default some_rule`), not a tool bug, so the single
/// caller records a graceful rejection over it. `true`/`false`/`null`/`nil`/`undefined` are CDDL
/// prelude CONSTANTS spelled as typenames rather than as their own `Type2` kinds — the same
/// classification the fixed map-key path makes — so they are lowered here instead of falling into
/// the `None` arm and reading as "not a value".
pub(super) fn type2_to_fixed_value(type2: &Type2) -> Option<FixedValue> {
    match type2 {
        Type2::UintValue { value, .. } => Some(FixedValue::Uint(*value as u64)),
        Type2::IntValue { value, .. } => Some(FixedValue::Nint(*value as i128)),
        Type2::FloatValue { value, .. } => Some(FixedValue::Float(*value)),
        Type2::TextValue { value, .. } => Some(FixedValue::Text(value.to_string())),
        Type2::B16ByteString { value, .. }
        | Type2::B64ByteString { value, .. }
        | Type2::UTF8ByteString { value, .. } => Some(FixedValue::Bytes(value.to_vec())),
        Type2::Typename { ident, .. } if ident.ident == "true" => Some(FixedValue::Bool(true)),
        Type2::Typename { ident, .. } if ident.ident == "false" => Some(FixedValue::Bool(false)),
        Type2::Typename { ident, .. } if ident.ident == "undefined" => Some(FixedValue::Undefined),
        // Lowered so the head check owns the verdict: `FixedValue::Null` never matches a primitive,
        // so `try_default` refuses it with the message that names the head and the value — which is
        // the accurate account of `? f: uint .default null`, where the value IS a literal and the
        // head simply cannot hold it.
        Type2::Typename { ident, .. } if ident.ident == "null" || ident.ident == "nil" => {
            Some(FixedValue::Null)
        }
        _ => None,
    }
}

/// A `.default` whose OPERAND is not a literal value at all (a type name, a group reference, a
/// nested expression). Distinct from [`super::unmappable_default_head_rejection`], which is about a literal
/// the HEAD cannot hold: here there is no value to lower in the first place.
fn non_literal_default_operand_rejection(rule_name: Option<&RustIdent>, operand: &Type2) -> String {
    format!(
        "{}`.default {operand}` is not a default VALUE — a default substitutes for an absent value \
         at deserialization, so it must be a literal the head can hold: an integer (`0`, `-2`), a \
         float (`1.5`), a text string (`\"hi\"`), a byte string (`h'CAFE'`), or `true`/`false`. Spell the value \
         literally, or remove the control.",
        reject_rule_prefix(rule_name)
    )
}

pub(super) fn parse_control_operator(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type2: &Type2,
    operator: &Operator,
    // The enclosing rule name, when available (top-level rule position), for graceful-rejection
    // messages naming the offending rule. `None` in member position (`rust_type_from_type1`).
    rule_name: Option<&RustIdent>,
    cli: &Cli,
) -> ControlOperator {
    // A control on a literal VALUE (`5 .lt 3`, `"a" .regexp "b"`, `h'00' .cbor uint`) is refused
    // before any arm reads it: a literal denotes exactly one value, so the control either always
    // holds or never does, and every arm below would lower it onto a type the literal is not (the
    // value comparisons built a window that ignored the literal). A RANGE between literals
    // (`0..255`) is the one operator a literal head takes. `.size` on a literal never arrives: the
    // pre-scan refuses it (`unsizable_size_head_rejections`).
    if let RangeCtlOp::CtlOp { ctrl, .. } = operator.operator
        && let Some(literal) = literal_head_spelling(type2)
    {
        types.record_rejection(format!(
            "{}the `{ctrl}` control operator on the literal value `{literal}` is unsupported — a \
             literal already denotes exactly one value, so the control either always holds or \
             never does. Remove the control, or apply it to a type (`uint .lt 3`, `bytes .cbor \
             uint`).",
            reject_rule_prefix(rule_name)
        ));
        return ControlOperator::Range((None, None));
    }
    // Float windows and graceful rejections (`.ne` over float, decimal bound on an int head) are
    // decided first, so the integer arms below only ever see genuine integer operands.
    if let Some(result) = try_float_or_reject(types, type2, operator, rule_name) {
        return result;
    }
    let lower_bound = match type2 {
        Type2::Typename { ident, .. } if ident.to_string() == "uint" => Some(0),
        _ => None,
    };
    //todo: read up on other range control operators in CDDL RFC
    // (rangeop / ctlop) S type2
    match operator.operator {
        RangeCtlOp::RangeOp { is_inclusive, .. } => {
            // Both bounds are read as VALUES here, before any name in them resolves, so a
            // non-literal bound is refused on the SHAPE axis: `record_rejection` + the inert
            // full-range placeholder every other graceful arm of this function returns, drained
            // into a graceful `Err` by finalize before generation runs.
            let range_start = match type2 {
                Type2::UintValue { value, .. } => *value as i128,
                Type2::IntValue { value, .. } => *value as i128,
                _ => {
                    types.record_rejection(non_literal_range_bound_rejection(
                        rule_name, "start", type2,
                    ));
                    return ControlOperator::Range((None, None));
                }
            };
            let range_end = match operator.type2 {
                Type2::UintValue { value, .. } => value as i128,
                Type2::IntValue { value, .. } => value as i128,
                _ => {
                    types.record_rejection(non_literal_range_bound_rejection(
                        rule_name,
                        "end",
                        &operator.type2,
                    ));
                    return ControlOperator::Range((None, None));
                }
            };
            ControlOperator::Range((
                Some(range_start),
                Some(if is_inclusive {
                    range_end
                } else {
                    // `a...b` is the exclusive range: it EXCLUDES b, so the max valid value
                    // is b-1 (RFC 8610 §3.2). The inclusive `a..b` path keeps range_end as-is.
                    range_end - 1
                }),
            ))
        }
        RangeCtlOp::CtlOp { ctrl, .. } => match ctrl {
            token::ControlOperator::DEFAULT => match type2_to_fixed_value(&operator.type2) {
                Some(value) => ControlOperator::Default(value),
                // Same graceful shape as the catch-all arm below: record the rejection and
                // hand back the inert full-range placeholder, which `finalize` drains into an
                // `Err` before generation runs. One message — the operand never reaches the head
                // check, which has nothing to say about a non-value.
                None => {
                    types.record_rejection(non_literal_default_operand_rejection(
                        rule_name,
                        &operator.type2,
                    ));
                    ControlOperator::Range((None, None))
                }
            },
            // The `.cbor` TARGET is handed on WHOLE — an `Alias` node included. This seam serves
            // every position the operator can be written at, and the two of them want opposite
            // things from a strip, so neither is done here:
            //
            // * a MEMBER or type-choice arm keeps the node as the field/variant type. The node is
            //   the ONLY thing `generate_serialize`/`generate_deserialize`'s `Alias` arms lift an
            //   aliased rule's `@custom_serialize`/`@custom_deserialize` pair (and its
            //   `@custom_encodings`) from, so stripping it silently re-derives the built-in wire
            //   while a plain member of the same alias routes through the pair — one CDDL type, two
            //   wire forms in one crate. It also decides the field's SPELLING: the `.cbor` here is
            //   the MEMBER expression's, so the alias still denotes the value read inside the byte
            //   string and `output_format.mdx` § "Type spelling at member positions" says the field
            //   is typed by the alias.
            // * a rule BODY registering a transparent alias must strip, because
            //   `register_type_alias` refuses an already-`Alias`-wrapped base — and it does so at
            //   its own seam, through `strip_alias_for_registration`, which carries the wire facts
            //   across the strip so the payload's codec survives the flattening. The rule-body
            //   WRAPPER spelling (`@newtype`, or a tag head) keeps the node, exactly as the
            //   member/arm positions do.
            //
            // Aliases are the only shape this distinction is about; a target naming a real
            // struct/collection is already a `Rust` ident and was never touched.
            //
            // Chain DEPTH is not a variable at either seam (`a = b`, `b = c`, `c = uint`): the alias
            // table never stores a nested alias — `register_type_alias` refuses one — so
            // `resolve_alias`/`new_type` can never hand back more than a single level, and each link
            // flattens (facts and all) as it registers. A four-link chain and a one-link chain reach
            // this seam as the identical `RustType`. Pinned by
            // tests/robustness/cbor_ref_alias_chain.cddl.
            token::ControlOperator::CBOR => ControlOperator::CBOR(rust_type_from_type2(
                types,
                parent_visitor,
                &operator.type2,
                cli,
            )),
            // TODO: this would be MUCH nicer (for error displaying, etc) to handle this in its own dedicated way
            //       which might be necessary once we support other control operators anyway
            token::ControlOperator::EQ
            | token::ControlOperator::NE
            | token::ControlOperator::LE
            | token::ControlOperator::LT
            | token::ControlOperator::GE
            | token::ControlOperator::GT => {
                let value = match control_operand_integer(rule_name, ctrl, &operator.type2) {
                    Ok(value) => value,
                    Err(msg) => {
                        types.record_rejection(msg);
                        return ControlOperator::Range((None, None));
                    }
                };
                match ctrl {
                    token::ControlOperator::EQ => {
                        ControlOperator::Range((Some(value), Some(value)))
                    }
                    // A value the head cannot hold (`uint .ne -1`, `nint .ne 0`) is never
                    // present, so excluding it constrains nothing.
                    token::ControlOperator::NE
                        if resolved_head_primitive(types, type2)
                            .and_then(integer_primitive_domain)
                            .is_some_and(|(min, max)| value < min || value > max) =>
                    {
                        ControlOperator::Range((None, None))
                    }
                    token::ControlOperator::NE => {
                        ControlOperator::Range(IntBounds::exclusion(value))
                    }
                    token::ControlOperator::LE => {
                        ControlOperator::Range((lower_bound, Some(value)))
                    }
                    token::ControlOperator::LT => {
                        ControlOperator::Range((lower_bound, Some(value - 1)))
                    }
                    token::ControlOperator::GE => ControlOperator::Range((Some(value), None)),
                    token::ControlOperator::GT => ControlOperator::Range((Some(value + 1), None)),
                    _ => unreachable!("guarded by the enclosing arm"),
                }
            }
            token::ControlOperator::SIZE => {
                // A size counts whole bytes. A float operand is refused rather than cast through a
                // saturating `as i128`, which truncated `2.5` to 2 and turned `(-1.0e300..1.0e300)`
                // into the full-i128 window `range_to_primitive` mapped onto `f32`. The operand
                // class decides, not its value: an integral `2.0` refuses too.
                if let Some(float) = size_operand_float_literal(&operator.type2) {
                    // `{:?}` keeps the float spelling (`2.0`, `-1e300`); `Type2`'s Display prints
                    // `2` and expands `1.0e300` to 301 digits.
                    types.record_rejection(format!(
                        "{}float `.size` operand `{float:?}` is unsupported — a size counts whole bytes, so a float has no exact meaning; spell the size as an integer literal (`.size 4`, `.size (1..63)`)",
                        reject_rule_prefix(rule_name),
                    ));
                    return ControlOperator::Range((None, None));
                }
                // `spelled` keeps the authored bounds: the exclusive form derives its max as
                // `b - 1`, so `(0...0)` is empty (max -1 < min 0) without spelling a negative.
                let (base_range, spelled) = match &operator.type2 {
                    Type2::ParenthesizedType { pt, .. } => {
                        if pt.type_choices.len() != 1 {
                            types.record_rejection(non_literal_size_operand_rejection(
                                rule_name,
                                &operator.type2,
                            ));
                            return ControlOperator::Range((None, None));
                        }
                        let inner_type = &pt.type_choices.first().unwrap().type1;
                        let min = match int_literal_to_i128(&inner_type.type2) {
                            Some(value) => Some(value),
                            None => {
                                types.record_rejection(non_literal_size_operand_rejection(
                                    rule_name,
                                    &operator.type2,
                                ));
                                return ControlOperator::Range((None, None));
                            }
                        };
                        match &inner_type.operator {
                            // if there was only one value instead of a range, we take that value to be the max
                            // ex: uint .size (1)
                            None => (ControlOperator::Range((None, min)), [min, None]),
                            Some(op) => match op.operator {
                                RangeCtlOp::RangeOp { is_inclusive, .. } => {
                                    let value = match int_literal_to_i128(&op.type2) {
                                        Some(value) => value,
                                        None => {
                                            types.record_rejection(
                                                non_literal_size_operand_rejection(
                                                    rule_name,
                                                    &operator.type2,
                                                ),
                                            );
                                            return ControlOperator::Range((None, None));
                                        }
                                    };
                                    // `(a...b)` EXCLUDES b (RFC 8610 §3.2), as in the value
                                    // range arm above: the largest admitted size is b-1.
                                    let max = Some(if is_inclusive { value } else { value - 1 });
                                    (ControlOperator::Range((min, max)), [min, Some(value)])
                                }
                                RangeCtlOp::CtlOp { .. } => {
                                    types.record_rejection(non_literal_size_operand_rejection(
                                        rule_name,
                                        &operator.type2,
                                    ));
                                    return ControlOperator::Range((None, None));
                                }
                            },
                        }
                    }
                    operand => match int_literal_to_i128(operand) {
                        Some(value) => (
                            ControlOperator::Range((None, Some(value))),
                            [Some(value), None],
                        ),
                        None => {
                            types.record_rejection(non_literal_size_operand_rejection(
                                rule_name,
                                &operator.type2,
                            ));
                            return ControlOperator::Range((None, None));
                        }
                    },
                };
                // A size counts bytes (or characters), so a negative authored bound has no
                // meaning, and a window with no admitted size (`(0...0)`, `(3..1)`) describes no
                // value. Both refuse here, for every head, before any head arm scales the window:
                // the `uint` arm's `2**(8*h)` overflowed on the `-1` max of `(0...0)`.
                if let Some(negative) = spelled.into_iter().flatten().find(|v| *v < 0) {
                    types.record_rejection(format!(
                        "{}negative `.size` operand `{negative}` is unsupported — a size counts bytes or characters, so it is never negative",
                        reject_rule_prefix(rule_name),
                    ));
                    return ControlOperator::Range((None, None));
                }
                if let ControlOperator::Range((Some(min), Some(max))) = base_range
                    && max < min
                {
                    types.record_rejection(format!(
                        "{}empty `.size` window `{}` is unsupported — it admits no size (the largest admitted size {max} is below the smallest {min}), so no value matches",
                        reject_rule_prefix(rule_name),
                        operator.type2,
                    ));
                    return ControlOperator::Range((None, None));
                }
                match type2 {
                    // `uint`, `(uint)`, or a transparent alias of it (`u = uint`): the same window
                    // `uint .size N` gets, so `[a: u .size 2]` means `0..=65535` rather than a
                    // length. `resolved_head_primitive` is the reading the member route attaches.
                    _ if resolved_head_primitive(types, type2).is_some_and(is_uint_primitive) => {
                        // .size 3 means 24 bits
                        match &base_range {
                            // RFC 8610 §3.8.1: the controller is a type of admitted sizes, and on
                            // `uint` each size N is a MAXIMUM (`uint .size N` is `0...256**N`). A
                            // ranged controller admits a value that fits in SOME N of `l..h`, so
                            // the union is `uint .size h` and the lower size never raises the
                            // lower bound. Reading `l` as "needs at least l bytes" (`256**(l-1)`)
                            // would contradict N being a maximum. The empty and negative windows
                            // were refused above, so `0 <= l <= h` here.
                            ControlOperator::Range((Some(_), Some(h))) => {
                                ControlOperator::Range((Some(0), Some(uint_size_max(*h))))
                            }
                            ControlOperator::Range((None, Some(h))) => {
                                ControlOperator::Range((Some(0), Some(uint_size_max(*h))))
                            }
                            _ => unreachable!(
                                "every `.size` operand lowers to a window with a maximum: {operator:?}"
                            ),
                        }
                    }
                    Type2::Typename { ident, .. } if ident.to_string() == "int" => {
                        // Rejected, not mapped: per the RFC author (cbor-wg/cddl#32) a control
                        // distributes over `int = uint / nint` and is undefined on `nint`
                        // (per-value non-match), so `int .size N` matches exactly the
                        // `uint .size N` window — the historical signed i{8N} mapping
                        // mis-enforced it in both directions. The rust cddl oracle also
                        // hard-errors on the construct, so an aligned window would be
                        // uncertifiable; revisit when upstream ships the per-value semantics
                        // (ledgered in cddl-matrix/roadmap.toml).
                        types.record_rejection(format!(
                            "{}`.size` on a signed `int` is unsupported — its spec meaning is the `uint .size` window (cbor-wg/cddl#32), which the signed reading mis-enforces; use `uint .size N`, or an explicit range for an N-byte signed int",
                            reject_rule_prefix(rule_name)
                        ));
                        ControlOperator::Range((None, None))
                    }
                    _ => {
                        match base_range {
                            // for strings & byte arrays, specifying an upper value means an exact value (.size 3 means a 3 char string)
                            ControlOperator::Range((None, Some(h))) => {
                                ControlOperator::Range((Some(h), Some(h)))
                            }
                            range => range,
                        }
                    }
                }
            }
            // Every other control operator is unsupported: `.within` / `.and` (LIVE — `uint
            // .within int`, `uint .and (0..9)`), `.bits`, `.regexp`, `.pcre`, the RFC 9165
            // additional controls (`.cat`, `.det`, `.plus`, `.abnf`, `.b64u`, `.hex`, `.join`,
            // `.json`, `.printf`, …), and `.cbor-seq` (unreachable: the cddl parser rejects it at
            // parse/lex, matrix `ctl.cborseq` evidence). A wildcard rather than a list because the
            // cddl crate gates most of these variants behind cargo features. Follows the
            // `.size`-on-`int` sibling above: `record_rejection` + an inert full-range placeholder,
            // drained into a graceful `Err` by finalize before generation ever runs.
            _ => {
                types.record_rejection(format!(
                    "{}the `{ctrl}` control operator is unsupported",
                    reject_rule_prefix(rule_name)
                ));
                ControlOperator::Range((None, None))
            }
        },
    }
}

/// An unsigned integer primitive: `uint` itself, or the narrower carrier a `uint` window collapses
/// onto (`u8 = uint .size 1`). `.size` on one of these is the `uint` reading, a maximum byte count.
pub(super) fn is_uint_primitive(primitive: Primitive) -> bool {
    matches!(
        primitive,
        Primitive::U8 | Primitive::U16 | Primitive::U32 | Primitive::U64
    )
}

/// The inclusive value domain of an integer primitive, or `None` for a non-integer one.
pub(super) fn integer_primitive_domain(primitive: Primitive) -> Option<(i128, i128)> {
    match primitive {
        Primitive::U8 => Some((0, u8::MAX as i128)),
        Primitive::U16 => Some((0, u16::MAX as i128)),
        Primitive::U32 => Some((0, u32::MAX as i128)),
        Primitive::U64 => Some((0, u64::MAX as i128)),
        Primitive::I8 => Some((i8::MIN as i128, i8::MAX as i128)),
        Primitive::I16 => Some((i16::MIN as i128, i16::MAX as i128)),
        Primitive::I32 => Some((i32::MIN as i128, i32::MAX as i128)),
        Primitive::I64 => Some((i64::MIN as i128, i64::MAX as i128)),
        Primitive::N64 => Some((-(u64::MAX as i128) - 1, -1)),
        _ => None,
    }
}

/// The largest uint of at most `bytes` bytes (`256**bytes - 1`). A CBOR uint is at most 8 bytes,
/// so any `bytes >= 8` admits every uint: `u64::MAX`, the window `uint .size 8` collapses onto.
/// Scaling `2**(8*bytes)` directly overflowed i128 from 16 bytes on and, from 9, emitted a bound
/// above `u64::MAX`. `bytes` is non-negative: the `.size` arm refuses negative operands first.
fn uint_size_max(bytes: i128) -> i128 {
    if bytes >= 8 {
        u64::MAX as i128
    } else {
        (1i128 << (8 * bytes)) - 1
    }
}

/// `ty` with occurrence `bounds` attached when there are any.
pub(super) fn with_optional_bounds(ty: RustType, bounds: Option<IntWindow>) -> RustType {
    match bounds {
        Some(bounds) => ty.with_occurrence_bounds(bounds),
        None => ty,
    }
}

/// Builds the ranged `RustType` for an integer window on `primitive`. Only an INTEGER head whose
/// window is exactly a rust integer's range collapses onto that integer primitive; any other head
/// (a `bytes`/`tstr` `.size` length window) keeps its own carrier with the window as its bounds,
/// or none when the window is the unconstrained one (`0..u64::MAX`: a CBOR length never exceeds
/// it). Collapsing by bounds alone turned `bytes .size (0..255)` into `u8`.
pub(super) fn range_to_primitive(
    low: Option<i128>,
    high: Option<i128>,
    primitive: Primitive,
) -> RustType {
    let integer_head = matches!(
        primitive,
        Primitive::U8
            | Primitive::I8
            | Primitive::U16
            | Primitive::I16
            | Primitive::U32
            | Primitive::I32
            | Primitive::U64
            | Primitive::I64
            | Primitive::N64
    );
    if !integer_head {
        return RustType::from(ConceptualRustType::Primitive(primitive))
            .with_value_bounds(length_window(primitive, (low, high)));
    }
    match (low, high) {
        (Some(l), Some(h)) if l == u8::MIN as i128 && h == u8::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U8).into()
        }
        (Some(l), Some(h)) if l == i8::MIN as i128 && h == i8::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I8).into()
        }
        (Some(l), Some(h)) if l == u16::MIN as i128 && h == u16::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U16).into()
        }
        (Some(l), Some(h)) if l == i16::MIN as i128 && h == i16::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I16).into()
        }
        (Some(l), Some(h)) if l == u32::MIN as i128 && h == u32::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U32).into()
        }
        (Some(l), Some(h)) if l == i32::MIN as i128 && h == i32::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I32).into()
        }
        (Some(l), Some(h)) if l == u64::MIN as i128 && h == u64::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::U64).into()
        }
        (Some(l), Some(h)) if l == i64::MIN as i128 && h == i64::MAX as i128 => {
            ConceptualRustType::Primitive(Primitive::I64).into()
        }
        // TODO: use minimal primitive or check here? e.g. uint .le 8 -> U8 instead of U64
        bounds => {
            RustType::from(ConceptualRustType::Primitive(primitive)).with_value_bounds(bounds)
        }
    }
}

/// Builds the ranged `RustType` for a FLOAT window: the given float primitive with the window
/// attached as `float_bounds`. Unlike `range_to_primitive` there is no "collapses exactly onto a
/// rust primitive" case — a float window is always a genuine sub-domain constraint (a both-`None`
/// window, which never arises from a real op, drops to no bound via `with_float_bounds`).
pub(super) fn float_range_to_primitive(window: FloatWindow, primitive: Primitive) -> RustType {
    RustType::from(ConceptualRustType::Primitive(primitive)).with_float_bounds(window)
}

/// Registers a top-level FLOAT range/control rule (`c = 0.5..10.5`, `#6.5(0.5..10.5)`,
/// `float64 .le 10.5`). Mirrors `register_ranged_type`'s three-way split for float windows:
/// narrow window (or `@newtype`) → bounds-enforcing float wrapper; tag-only → wrapper that writes
/// the tag; otherwise a transparent alias.
#[allow(clippy::too_many_arguments)]
pub(super) fn register_float_range(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    mut ranged_type: RustType,
    window: FloatWindow,
    outer_tag: Option<usize>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    if ranged_type.config.float_bounds.is_some() || rule_metadata.newtype.is_some() {
        // window carried in the wrapper's dedicated slot, not on the inner type
        ranged_type.config.float_bounds = None;
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper_float(
                type_name.clone(),
                outer_tag,
                Some(&rule_metadata),
                ranged_type,
                Some(window),
            ),
            cli,
        );
    } else if outer_tag.is_some() {
        // full-domain float (no residual window) but tagged: wrap so the tag is written/checked
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper_float(
                type_name.clone(),
                None,
                Some(&rule_metadata),
                ranged_type.tag_if(outer_tag),
                None,
            ),
            cli,
        );
    } else {
        types.register_type_alias(
            type_name.clone(),
            AliasInfo::new_from_metadata(ranged_type.tag_if(outer_tag), rule_metadata),
        );
    }
}

/// Registers a top-level ranged rule — a typename head (`u = uint .le 5`, `b = bytes .size 4`,
/// `#6.n(uint .le 255)`) or a literal-headed range (`c = -10..-3`, `e = #6.5(3..10)`) — through the
/// one three-way split, so both spellings wrap identically. `ranged_type` is the collapsed
/// primitive (with any residual `config.bounds`); `min_max` is the original window carried into
/// the wrapper's full-window check.
#[allow(clippy::too_many_arguments)]
pub(super) fn register_ranged_type(
    types: &mut IntermediateTypes,
    parent_visitor: &ParentVisitor,
    type_name: &RustIdent,
    mut ranged_type: RustType,
    min_max: IntWindow,
    outer_tag: Option<usize>,
    rule_metadata: RuleMetadata,
    cli: &Cli,
) {
    let exact_byte_array = ranged_type.exact_byte_array_len_checked().is_some();
    if ranged_type.config.bounds.is_some() || rule_metadata.newtype.is_some() {
        // Most nominal ranges carry their full window on the wrapper. Exact bytes are different:
        // their member spelling needs the window to select `[u8; N]`, and the wrapper constructor
        // owns the single Vec -> array handover instead of a second len guard.
        if !exact_byte_array {
            ranged_type.config.bounds = None;
        }
        // has non-rust-primitive matching bounds
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper(
                type_name.clone(),
                outer_tag,
                Some(&rule_metadata),
                ranged_type,
                (!exact_byte_array && min_max != (None, None)).then_some(min_max),
            ),
            cli,
        );
    } else if outer_tag.is_some() {
        // The range collapses exactly onto a rust primitive (no residual bound to check), but a
        // top-level `#6.n(0..255)` tag rule must still wrap so its standalone `to/from_cbor_bytes`
        // writes/checks the tag — a transparent `pub type` alias would drop it from the wire. The tag
        // rides on `ranged_type` (`.tag_if(outer_tag)`) and there's no `min_max` since the primitive
        // already covers the whole domain.
        types.register_rust_struct(
            parent_visitor,
            RustStruct::new_wrapper(
                type_name.clone(),
                None,
                Some(&rule_metadata),
                ranged_type.tag_if(outer_tag),
                None,
            ),
            cli,
        );
    } else {
        // matches to known rust type e.g. u32, i16, etc so just make an alias
        types.register_type_alias(
            type_name.clone(),
            AliasInfo::new_from_metadata(ranged_type.tag_if(outer_tag), rule_metadata),
        );
    }
}
