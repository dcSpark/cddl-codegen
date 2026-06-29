//! `--emit-tests` reject-half generator.
//!
//! For every type that carries a bounded (`RangeCheck`) field, emit a `#[test]` that pushes a
//! field out of bounds and asserts the generated code rejects it. Two shapes, mirroring the
//! hand-written `tests/core/tests.rs::bounds()` precedent:
//!
//! * **deser-reject** (structs / `Record`): mint a valid baseline via `new(..)`, mutate one `pub`
//!   field out of bounds, serialize, and assert `from_cbor_bytes` rejects the wire bytes as
//!   `DeserializeFailure::RangeCheck`. This exercises the *wire* path — the roadmap's target —
//!   because serialize does not re-check bounds (raw `write_*`) but deserialize does.
//! * **construct-reject** (type / group choices, bounded `@newtype` wrappers): assert the
//!   constructor itself rejects an out-of-bounds value as `RangeCheck` (and accepts the boundary).
//!   Type and group choices share the same deserialization code, so we only check the constructor
//!   API here.
//!
//! Deliberately scoped to the cheap cases (the first slice — see `tests/TESTING_ROADMAP.md` c6):
//! the valid baseline is minted from compile-time literals, so any field that can't be cheaply
//! minted (nested rust structs/tags, bounded `nint`s — whose stored/wire direction is inverted)
//! causes that one type to be skipped with an `eprintln!`, never a silently-weakened test.

use crate::cli::Cli;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, IntermediateTypes, Primitive, RustField,
    RustRecord, RustStructType, RustType,
};
use crate::utils::convert_to_snake_case;

type Bounds = (Option<i128>, Option<i128>);

/// Emit the `#[cfg(test)]` reject-test module, or `None` if there's nothing bounded to test.
pub fn emit_reject_tests(types: &IntermediateTypes, cli: &Cli) -> Option<String> {
    if !cli.to_from_bytes_methods {
        // deser-reject needs to_cbor_bytes/from_cbor_bytes
        eprintln!(
            "cddl-codegen --emit-tests: skipped (requires --to-from-bytes-methods, which is off)"
        );
        return None;
    }

    let mut fns: Vec<String> = Vec::new();
    for (ident, rust_struct) in types.rust_structs() {
        let name = ident.to_string();
        let body = match rust_struct.variant() {
            RustStructType::Record(record) => record_deser_reject(types, &name, record),
            RustStructType::TypeChoice { variants }
            | RustStructType::GroupChoice { variants, .. } => {
                choice_construct_reject(types, &name, variants)
            }
            RustStructType::Wrapper { wrapped, min_max } => {
                min_max.and_then(|mm| wrapper_construct_reject(types, &name, wrapped, mm))
            }
            _ => None,
        };
        if let Some(lines) = body
            && !lines.is_empty()
        {
            fns.push(format!(
                "#[test]\nfn reject_{}() {{\n{}\n}}\n",
                convert_to_snake_case(&name),
                lines
            ));
        }
    }

    if fns.is_empty() {
        return None;
    }
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\nmod cddl_generated_tests {{\n    use super::*;\n{}\n}}\n",
        fns.join("\n")
    ))
}

/// deser-reject for a struct: for each cheaply-mutatable bounded field, mint a valid baseline,
/// mutate that one field out of bounds, and assert the wire path rejects it as `RangeCheck`.
fn record_deser_reject(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
) -> Option<String> {
    // constructor arg list: mandatory, non-fixed, non-default fields (mirrors codegen_struct)
    let ctor_fields: Vec<&RustField> = record
        .fields
        .iter()
        .filter(|f| {
            !f.optional && !f.rust_type.is_fixed_value() && f.rust_type.config.default.is_none()
        })
        .collect();

    // surface any bounded field we can't push out of bounds (e.g. nint) so the gap isn't silent
    for f in &record.fields {
        if f.rust_type.config.bounds.is_some() && measure_kind(&f.rust_type).is_none() {
            eprintln!(
                "cddl-codegen --emit-tests: {name}.{} bounded but not cheaply testable (e.g. nint) — no reject test",
                f.name
            );
        }
    }

    // the fields we can actually push out of bounds (bounded, non-nint, cheaply measurable)
    let targets: Vec<&RustField> = ctor_fields
        .iter()
        .copied()
        .filter(|f| f.rust_type.config.bounds.is_some() && measure_kind(&f.rust_type).is_some())
        .collect();
    if targets.is_empty() {
        return None;
    }

    // valid baseline arg for every constructor field; bail the whole type if any isn't mintable
    let mut valid_args: Vec<String> = Vec::new();
    for f in &ctor_fields {
        match valid_value(types, &f.rust_type) {
            Some(v) => valid_args.push(v),
            None => {
                eprintln!(
                    "cddl-codegen --emit-tests: skipped {name} (field {} not cheaply mintable)",
                    f.name
                );
                return None;
            }
        }
    }
    let baseline = format!("{name}::new({}).unwrap()", valid_args.join(", "));

    let mut blocks = Vec::new();
    for target in targets {
        let field = &target.name;
        let is_len = measure_kind(&target.rust_type) == Some(MeasureKind::Len);
        let cases = bound_cases(
            types,
            &target.rust_type,
            target.rust_type.config.bounds.unwrap(),
            is_len,
        );
        // skip fields whose bound coincides with the rust type's domain: no representable
        // out-of-bounds value exists, so the only cases are accepts and the test would be vacuous.
        if !cases.iter().any(|(_, accept, _)| !accept) {
            continue;
        }
        for (expr, accept, label) in cases {
            if accept {
                blocks.push(format!(
                    "    {{
        let mut v = mk();
        v.{field} = {expr};
        let bytes = v.to_cbor_bytes();
        let back = {name}::from_cbor_bytes(&bytes).expect(\"{name}.{field} {label} must deserialize\");
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name}.{field} {label} must round-trip\");
    }}"
                ));
            } else {
                blocks.push(format!(
                    "    {{
        let mut v = mk();
        v.{field} = {expr};
        let err = {name}::from_cbor_bytes(&v.to_cbor_bytes()).unwrap_err();
        assert!(matches!(err.failure(), DeserializeFailure::RangeCheck {{ .. }}), \"{name}.{field} {label} must be rejected as RangeCheck, got {{:?}}\", err.failure());
    }}"
                ));
            }
        }
    }
    if blocks.is_empty() {
        return None;
    }
    Some(format!(
        "    let mk = || {baseline};\n{}",
        blocks.join("\n")
    ))
}

/// construct-reject for type/group choice variants whose constructor checks bounds.
fn choice_construct_reject(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
) -> Option<String> {
    let mut lines = Vec::new();
    for variant in variants {
        let ctor = format!("new_{}", variant.name_as_var());
        // figure out the constructor arg list (mirrors generate_enum's new_<variant>)
        let arg_fields: Vec<(&RustType, String)> = match &variant.data {
            EnumVariantData::RustType(ty) => {
                if let Some(record) = ty_as_record(types, ty) {
                    // group-choice variant backed by a multi-field record: ctor flattens its fields
                    record
                        .fields
                        .iter()
                        .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                        .map(|f| (&f.rust_type, f.name.clone()))
                        .collect()
                } else if ty.is_fixed_value() {
                    vec![]
                } else {
                    // single value passed straight in
                    vec![(ty, variant.name_as_var())]
                }
            }
            EnumVariantData::Inlined(record) => {
                if record.fields.iter().any(|f| f.optional) {
                    // optional args complicate the baseline — defer
                    continue;
                }
                record
                    .fields
                    .iter()
                    .filter(|f| !f.rust_type.is_fixed_value())
                    .map(|f| (&f.rust_type, f.name.clone()))
                    .collect()
            }
        };

        // which arg (if any) carries a cheaply-testable bound?
        for (i, (arg_ty, _)) in arg_fields.iter().enumerate() {
            let Some(kind) = measure_kind(arg_ty) else {
                continue;
            };
            let Some(bounds) = arg_ty.config.bounds else {
                continue;
            };
            let cases = bound_cases(types, arg_ty, bounds, kind == MeasureKind::Len);
            if !cases.iter().any(|(_, accept, _)| !accept) {
                continue; // bound == type domain: no constructible out-of-bounds value
            }
            for (expr, accept, label) in cases {
                // build the call: this arg = boundary/beyond value, valid for the rest
                let mut call_args: Vec<String> = Vec::new();
                let mut ok = true;
                for (j, (ty, _)) in arg_fields.iter().enumerate() {
                    let v = if j == i {
                        Some(expr.clone())
                    } else {
                        valid_value(types, ty)
                    };
                    match v {
                        Some(s) => call_args.push(s),
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if ok {
                    let args = call_args.join(", ");
                    lines.push(if accept {
                        format!("    assert!({name}::{ctor}({args}).is_ok(), \"{name}::{ctor} {label} arg must be accepted\");")
                    } else {
                        format!("    assert!(matches!({name}::{ctor}({args}).unwrap_err().failure(), DeserializeFailure::RangeCheck {{ .. }}), \"{name}::{ctor} {label} arg must be rejected as RangeCheck\");")
                    });
                }
            }
        }
    }
    if lines.is_empty() {
        None
    } else {
        Some(lines.join("\n"))
    }
}

/// construct-reject for a bounded `@newtype` wrapper. The wrapper checks the *raw* `min_max`
/// against the inner value (or its `.len()`), with no nint transform, so we synthesize directly
/// from `min_max`.
fn wrapper_construct_reject(
    types: &IntermediateTypes,
    name: &str,
    wrapped: &RustType,
    min_max: Bounds,
) -> Option<String> {
    let kind = measure_kind(wrapped)?;
    let cases = bound_cases(types, wrapped, min_max, kind == MeasureKind::Len);
    if !cases.iter().any(|(_, accept, _)| !accept) {
        return None; // bound == type domain: no constructible out-of-bounds value
    }
    let lines: Vec<String> = cases
        .into_iter()
        .map(|(expr, accept, label)| {
            if accept {
                format!("    assert!({name}::new({expr}).is_ok(), \"{name}::new {label} value must be accepted\");")
            } else {
                format!("    assert!(matches!({name}::new({expr}).unwrap_err().failure(), DeserializeFailure::RangeCheck {{ .. }}), \"{name}::new {label} value must be rejected as RangeCheck\");")
            }
        })
        .collect();
    Some(lines.join("\n"))
}

#[derive(PartialEq, Clone, Copy)]
enum MeasureKind {
    /// the value itself is bounded (integer primitives)
    Value,
    /// the length is bounded (text / bytes / array / map)
    Len,
}

/// How `ty`'s bound is measured, or `None` if it isn't a cheaply-testable bounded shape
/// (`nint`/bool/float values are excluded — see module docs).
fn measure_kind(ty: &RustType) -> Option<MeasureKind> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) => match p {
            Primitive::Str | Primitive::Bytes => Some(MeasureKind::Len),
            Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64 => Some(MeasureKind::Value),
            // nint: stored/wire direction is inverted; bool/float: bounds don't apply meaningfully
            Primitive::N64 | Primitive::Bool | Primitive::F32 | Primitive::F64 => None,
        },
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _) => Some(MeasureKind::Len),
        _ => None,
    }
}

/// In-range measure for a valid baseline: the inclusive min (or max, or 0).
fn valid_measure(b: Bounds) -> i128 {
    b.0.or(b.1).unwrap_or(0)
}

/// nint bounds are stored as u64 magnitudes; mirror the generator's transform so a valid baseline
/// value passes the same check `new()` emits (see `generation.rs::nint_bounds_to_u64`).
fn nint_bounds_to_u64(b: Bounds) -> Bounds {
    (b.0.map(|x| (x + 1).abs()), b.1.map(|x| (x + 1).abs()))
}

/// Inclusive representable range of an integer primitive's backing Rust type.
fn prim_range(p: &Primitive) -> (i128, i128) {
    match p {
        Primitive::U8 => (0, u8::MAX as i128),
        Primitive::U16 => (0, u16::MAX as i128),
        Primitive::U32 => (0, u32::MAX as i128),
        Primitive::U64 => (0, u64::MAX as i128),
        Primitive::I8 => (i8::MIN as i128, i8::MAX as i128),
        Primitive::I16 => (i16::MIN as i128, i16::MAX as i128),
        Primitive::I32 => (i32::MIN as i128, i32::MAX as i128),
        Primitive::I64 => (i64::MIN as i128, i64::MAX as i128),
        // not an integer primitive with a fixed signed/unsigned width
        Primitive::N64
        | Primitive::Bool
        | Primitive::F32
        | Primitive::F64
        | Primitive::Str
        | Primitive::Bytes => (i128::MIN, i128::MAX),
    }
}

/// Boundary and just-beyond cases for a bounded type: `(value_expr, accept, label)`. Accept cases
/// are the inclusive min/max boundaries (must round-trip / construct); reject cases are one step
/// beyond (must be rejected as `RangeCheck`). Two-sided when both endpoints are present; each case
/// is dropped if its value isn't representable in the backing rust type.
fn bound_cases(
    types: &IntermediateTypes,
    ty: &RustType,
    bounds: Bounds,
    is_len: bool,
) -> Vec<(String, bool, &'static str)> {
    let mut out = Vec::new();
    if let Some(min) = bounds.0 {
        if let Some(e) = materialize(types, ty, min) {
            out.push((e, true, "min boundary"));
        }
        // going below the minimum is only representable when the floor isn't already 0 (lengths)
        if (!is_len || min >= 1)
            && let Some(e) = materialize(types, ty, min - 1)
        {
            out.push((e, false, "below min"));
        }
    }
    if let Some(max) = bounds.1 {
        if let Some(e) = materialize(types, ty, max) {
            out.push((e, true, "max boundary"));
        }
        if let Some(e) = materialize(types, ty, max + 1) {
            out.push((e, false, "above max"));
        }
    }
    out
}

/// A valid in-range Rust value expression for `ty`, or `None` if it can't be cheaply minted.
fn valid_value(types: &IntermediateTypes, ty: &RustType) -> Option<String> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Optional(_) => Some("None".to_owned()),
        ConceptualRustType::Primitive(Primitive::Bool) => Some("false".to_owned()),
        ConceptualRustType::Primitive(Primitive::F32 | Primitive::F64) => Some("0.0".to_owned()),
        // nint can't be an OOB *target* (stored/wire direction is inverted), but a valid baseline
        // value is mintable: new()'s check uses the nint-transformed bounds, so the transformed
        // min (or 0 when unbounded) is in range.
        ConceptualRustType::Primitive(Primitive::N64) => {
            let b = ty
                .config
                .bounds
                .map(nint_bounds_to_u64)
                .unwrap_or((None, None));
            Some(format!("{}", valid_measure(b)))
        }
        _ => materialize(
            types,
            ty,
            valid_measure(ty.config.bounds.unwrap_or((None, None))),
        ),
    }
}

/// Build a Rust value expression for `ty` whose bound-relevant measure equals `measure`
/// (the value itself for integers, the length for text/bytes/array/map).
fn materialize(types: &IntermediateTypes, ty: &RustType, measure: i128) -> Option<String> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) => match p {
            Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64 => {
                // the literal must fit the inner rust type, else it won't compile. When the bound
                // coincides with the type's domain (e.g. `uint .lt 256` backed by `u8`) there's no
                // representable out-of-bounds value — skip rather than emit uncompilable code.
                let (lo, hi) = prim_range(p);
                (measure >= lo && measure <= hi).then(|| format!("{measure}"))
            }
            // nint stored values are non-negative u64 magnitudes; valid_measure keeps them in range
            Primitive::N64 => Some(format!("{measure}")),
            Primitive::Str => Some(format!("\"a\".repeat({measure})")),
            Primitive::Bytes => Some(format!("vec![0u8; {measure}]")),
            Primitive::Bool => Some("false".to_owned()),
            Primitive::F32 | Primitive::F64 => Some("0.0".to_owned()),
        },
        ConceptualRustType::Array(elem) => {
            let e = valid_value(types, elem)?;
            Some(format!("vec![{e}; {measure}]"))
        }
        ConceptualRustType::Map(k, v) => {
            let key = match k.resolve_alias_shallow() {
                ConceptualRustType::Primitive(
                    p @ (Primitive::U8
                    | Primitive::U16
                    | Primitive::U32
                    | Primitive::U64
                    | Primitive::I8
                    | Primitive::I16
                    | Primitive::I32
                    | Primitive::I64
                    | Primitive::N64),
                ) => format!("__i as {p}"),
                ConceptualRustType::Primitive(Primitive::Str) => "__i.to_string()".to_owned(),
                _ => return None, // non-trivial keys aren't cheaply mintable
            };
            let val = valid_value(types, v)?;
            // distinct keys 0..measure; collect() infers the map type from the target position
            Some(format!(
                "(0u64..{measure}).map(|__i| ({key}, {val})).collect()"
            ))
        }
        _ => None,
    }
}

/// If `ty` directly names a generated `Record` struct, return it (so we can flatten its
/// constructor args for a group-choice variant).
fn ty_as_record<'a>(types: &'a IntermediateTypes, ty: &RustType) -> Option<&'a RustRecord> {
    if let ConceptualRustType::Rust(ident) = ty.resolve_alias_shallow()
        && let RustStructType::Record(record) = types.rust_struct(ident)?.variant()
    {
        return Some(record);
    }
    None
}
