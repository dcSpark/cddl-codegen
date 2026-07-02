//! `--emit-tests` generated-test emitter: the REJECT half and the ROUND-TRIP half.
//!
//! Everything here is derived from each type's IR at generation time — there are no hand-authored
//! value lists. The per-IR-shape derivation rules below are the single maintained surface, and any
//! type/field/variant they can't mint is skipped with an `eprintln!`, never a silently-weakened
//! test. The one deliberate weakening — an unbounded collection whose element can't be minted is
//! minted EMPTY (its element wire path goes unexercised) so recursion can terminate — is loud too.
//!
//! **Reject half** — for every type carrying a bounded (`RangeCheck`) field, a `#[test]` that
//! pushes a field out of bounds and asserts the generated code rejects it. Two shapes, mirroring
//! the hand-written `tests/core/tests.rs::bounds()` precedent:
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
//! **Round-trip half** — for every type we can mint, a `#[test]` that constructs IR-derived value
//! cases and asserts the full wire cycle is byte-identical (`value → to_cbor_bytes →
//! from_cbor_bytes → to_cbor_bytes == bytes`). Cases per shape: a valid baseline; each optional
//! field additionally present (records); one mandatory nullable (`T / null`) field additionally
//! set to `Some(inner)` (records — so the present-value wire path runs, not just the `None`
//! baseline); one case per choice/c-enum variant. This is the
//! "output is right, not just unchanged" oracle (TESTING_ROADMAP item 1): a serialize/deserialize
//! disagreement fails here even when snapshots and compile gates stay green. It deliberately
//! shares the generator's IR, so on its own it cannot catch IR-level bugs (wrong bounds computed at
//! parse time) — that is the spec-anchored oracles' job (golden hex / conformance validation).
//!
//! **Conformance oracle add-on (`--emit-tests-conformance`).** When enabled, each round-trip case
//! gets one extra line right after its `bytes` are computed: `cddl_conformance::validate(&bytes,
//! "<rule>")`, validating the minted bytes against the SOURCE `.cddl` rule via the `cddl` crate's
//! independent decode+constraint path. This closes the IR-bug residual: when an IR miscompile mints
//! a spec-violating value (e.g. the `0...10` exclusive-range bug minting `11`), the round-trip
//! asserts it green but the conformance validator rejects it. It shares the fork's PARSER with the
//! generator, so it catches wrong VALUES, not fork-level misparses (same caveat as
//! `tests/deser_test_conformance.rs`, whose helpers it reuses).
//!
//! Deliberately scoped to the cheap cases (the first slice — see `tests/TESTING_ROADMAP.md` c6):
//! valid values are minted from compile-time literals, so any field that can't be cheaply
//! minted (nested rust structs/tags, bounded `nint`s — whose stored/wire direction is inverted)
//! causes that one type/case to be skipped with an `eprintln!`.

use crate::cli::Cli;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, IntermediateTypes, Primitive, RustField,
    RustRecord, RustStructType, RustType,
};
use crate::utils::{convert_to_camel_case, convert_to_snake_case};

type Bounds = (Option<i128>, Option<i128>);

/// Emit the `#[cfg(test)]` generated-test module (reject + round-trip halves), or `None` if
/// nothing at all could be minted.
pub fn emit_generated_tests(types: &IntermediateTypes, cli: &Cli) -> Option<String> {
    if !cli.to_from_bytes_methods {
        // both halves need to_cbor_bytes/from_cbor_bytes
        eprintln!(
            "cddl-codegen --emit-tests: skipped (requires --to-from-bytes-methods, which is off)"
        );
        return None;
    }

    let mut fns: Vec<String> = Vec::new();
    for (ident, rust_struct) in types.rust_structs() {
        let name = ident.to_string();
        let reject = match rust_struct.variant() {
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
        if let Some(lines) = reject
            && !lines.is_empty()
        {
            fns.push(format!(
                "#[test]\nfn reject_{}() {{\n{}\n}}\n",
                convert_to_snake_case(&name),
                lines
            ));
        }

        // `--emit-tests-conformance`: the source rule name to validate this type's minted bytes
        // against (None when off, when the type isn't a top-level rule, or when its name can't be
        // faithfully reversed — see `conformance_rule_name`).
        let conf = cli
            .emit_tests_conformance
            .then(|| conformance_rule_name(types, ident))
            .flatten();
        let conf = conf.as_deref();

        let roundtrip = match rust_struct.variant() {
            RustStructType::Record(record) => record_roundtrip(types, &name, record, conf),
            RustStructType::TypeChoice { variants }
            | RustStructType::GroupChoice { variants, .. } => {
                choice_roundtrip(types, &name, variants, conf)
            }
            RustStructType::Wrapper { wrapped, min_max } => {
                wrapper_roundtrip(types, &name, wrapped, *min_max, conf)
            }
            // c-style enums have no standalone Serialize/Deserialize impls (they serialize inline
            // in their containing types) — they're exercised wherever a record embeds them
            RustStructType::CStyleEnum { .. } => None,
            // Rust-side tables/arrays are transparent `pub type` aliases — a PERMANENT skip, not a
            // TODO: `pub type X = Vec<T>` has no standalone `Serialize` (cbor_event implements it
            // for `String` but not `Vec`/`BTreeMap`, and the orphan rule forbids adding it here),
            // and `from_cbor_bytes` on the alias routes through cbor_event's generic impls rather
            // than the generated element loop — so a standalone round-trip could not exercise
            // generated code even if it compiled. The generator-emitted wire path exists only at
            // EMBED sites, which mint via their containing record (e.g. `bool_holder`).
            RustStructType::Table { .. } | RustStructType::Array { .. } => {
                eprintln!(
                    "cddl-codegen --emit-tests: {name} is a transparent table/array alias — no standalone round-trip exists (embed-site coverage only)"
                );
                None
            }
            // reference user-supplied code; the generated crate can't exercise them standalone
            RustStructType::Extern | RustStructType::RawBytesType => None,
        };
        if let Some(lines) = roundtrip
            && !lines.is_empty()
        {
            fns.push(format!(
                "#[test]\nfn roundtrip_{}() {{\n{}\n}}\n",
                convert_to_snake_case(&name),
                lines
            ));
        }
    }

    if fns.is_empty() {
        return None;
    }
    // `--emit-tests-conformance`: the sub-module the emitted `cddl_conformance::validate(..)` calls
    // resolve to. It reuses the shared oracle helpers appended at crate root from
    // `tests/deser_test_conformance.rs` (do NOT duplicate the validator logic) and reads the source
    // spec the IR-conformance gate copies next to the crate's Cargo.toml. Emitted whenever the flag
    // is on (harmless + `dead_code`-allowed if a fixture minted no round-trip cases). See that file
    // for the oracle's strength (independent decode+constraint path) and caveats (shares the fork's
    // PARSER; validator has known gaps like unenforced `uint .size`).
    let conformance_mod = if cli.emit_tests_conformance {
        "    #[allow(dead_code)]\n    mod cddl_conformance {\n        pub fn validate(bytes: &[u8], root_rule: &str) {\n            let spec = crate::cddl_oracle_load_spec(\"cddl_conformance_source.cddl\");\n            crate::assert_cddl_conforms(&spec, root_rule, bytes);\n        }\n    }\n"
    } else {
        ""
    };
    // `to_cbor_bytes`/`from_cbor_bytes` are trait methods (serialization::{ToCBORBytes,
    // Deserialize}); `use super::*` alone doesn't bring traits into scope in a standalone crate
    // (the integration harness happens to append `use serialization::*;` to lib.rs, which masked
    // this), so import the serialization surface explicitly.
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\nmod cddl_generated_tests {{\n    use super::*;\n    use super::serialization::*;\n{conformance_mod}{}\n}}\n",
        fns.join("\n")
    ))
}

/// The source CDDL rule name to validate `ident`'s minted bytes against under
/// `--emit-tests-conformance`, or `None` if this type can't be soundly conformance-checked. Only a
/// top-level rule qualifies (a synthesized struct — embedded record, inline group — has no spec rule
/// to root the validator against). The rule name is recovered as `convert_to_snake_case(ident)` and
/// only accepted when `convert_to_camel_case` of it round-trips back to the exact ident, so a lossy
/// reversal (dashed/acronym names the corpus doesn't use) is skipped loudly rather than pointed at a
/// rule that doesn't exist.
fn conformance_rule_name(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
) -> Option<String> {
    if !types.is_toplevel_rule(ident) {
        return None;
    }
    let name = ident.to_string();
    let snake = convert_to_snake_case(&name);
    if convert_to_camel_case(&snake) != name {
        eprintln!(
            "cddl-codegen --emit-tests-conformance: cannot recover a source rule name for {name} \
             (snake_case reversal is not faithful) — no conformance oracle for this type"
        );
        return None;
    }
    Some(snake)
}

// ============================================================================================
// ROUND-TRIP half. Each fn returns the body of one `roundtrip_<type>` test: a set of IR-derived
// value cases, each pushed through the full wire cycle and asserted byte-identical.
// ============================================================================================

/// The shared wire-cycle emission for a list of `(value_expr, label)` cases. `conf` is the source
/// CDDL rule name for the `--emit-tests-conformance` oracle (`None` when off): when set, each case
/// validates its minted `bytes` against the spec right after computing them.
fn roundtrip_body(name: &str, cases: Vec<(String, String)>, conf: Option<&str>) -> Option<String> {
    if cases.is_empty() {
        return None;
    }
    let conf_line = conf
        .map(|rule| format!("        cddl_conformance::validate(&bytes, \"{rule}\");\n"))
        .unwrap_or_default();
    let blocks: Vec<String> = cases
        .into_iter()
        .map(|(expr, label)| {
            format!(
                "    {{
        let v = {expr};
        let bytes = v.to_cbor_bytes();
{conf_line}        let back = {name}::from_cbor_bytes(&bytes).expect(\"{name} ({label}): serialized bytes must deserialize\");
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name} ({label}): wire round-trip must be byte-identical\");
    }}"
            )
        })
        .collect();
    Some(blocks.join("\n"))
}

/// Mirrors the record constructor's fallibility rule (`generation.rs` `new_can_fail`): `new()`
/// returns `Result` iff any non-optional field is bounded.
fn record_ctor_can_fail(record: &RustRecord) -> bool {
    record
        .fields
        .iter()
        .any(|f| !f.optional && f.rust_type.config.bounds.is_some())
}

/// Record round-trip: a valid baseline, plus one case per optional field with that field present.
fn record_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
    conf: Option<&str>,
) -> Option<String> {
    let ctor_fields: Vec<&RustField> = record
        .fields
        .iter()
        .filter(|f| {
            !f.optional && !f.rust_type.is_fixed_value() && f.rust_type.config.default.is_none()
        })
        .collect();
    let mut valid_args: Vec<String> = Vec::new();
    for f in &ctor_fields {
        match valid_value(types, &f.rust_type) {
            Some(v) => valid_args.push(v),
            None => {
                eprintln!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (field {} not cheaply mintable)",
                    f.name
                );
                return None;
            }
        }
    }
    let unwrap = if record_ctor_can_fail(record) {
        ".unwrap()"
    } else {
        ""
    };
    let base = format!("{name}::new({}){unwrap}", valid_args.join(", "));
    let mut cases = vec![(base.clone(), "baseline".to_owned())];
    for f in record.fields.iter().filter(|f| f.optional) {
        match valid_value(types, &f.rust_type) {
            Some(x) => {
                // a defaulted optional is stored as a PLAIN field (absent on the wire = default);
                // only non-defaulted optionals are Option<T> in the struct
                let assign = if f.rust_type.config.default.is_some() {
                    x
                } else {
                    format!("Some({x})")
                };
                cases.push((
                    format!("{{ let mut v = {base}; v.{} = {assign}; v }}", f.name),
                    format!("optional `{}` present", f.name),
                ));
            }
            None => eprintln!(
                "cddl-codegen --emit-tests: {name}.{} optional-present case not cheaply mintable — skipped",
                f.name
            ),
        }
    }
    // A MANDATORY nullable (`T / null` -> `Option<T>`) field mints its degenerate `None` in the
    // baseline (`valid_value` yields `None` for any `Optional`), so the `Some(inner)` serialize /
    // deserialize direction stays compile-locked but never executed. Add ONE case exercising it —
    // first mintable nullable field, no combinatorics — so the composite-inner wire path runs.
    for f in record.fields.iter().filter(|f| !f.optional) {
        if let ConceptualRustType::Optional(inner) = f.rust_type.resolve_alias_shallow()
            && let Some(x) = valid_value(types, inner)
        {
            cases.push((
                format!("{{ let mut v = {base}; v.{} = Some({x}); v }}", f.name),
                format!("nullable `{}` present", f.name),
            ));
            break;
        }
    }
    roundtrip_body(name, cases, conf)
}

/// Choice round-trip: one wire cycle per constructible variant (the construct-reject half never
/// serializes, so this is the variants' first trip through the actual encode/decode path).
fn choice_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    conf: Option<&str>,
) -> Option<String> {
    let mut cases = Vec::new();
    for variant in variants {
        let ctor = format!("new_{}", variant.name_as_var());
        let Some(arg_fields) = variant_arg_fields(types, variant) else {
            eprintln!(
                "cddl-codegen --emit-tests: {name}::{ctor} not cheaply constructible — no round-trip case"
            );
            continue;
        };
        let mut args: Vec<String> = Vec::new();
        let mut ok = true;
        for (ty, field) in &arg_fields {
            match valid_value(types, ty) {
                Some(v) => args.push(v),
                None => {
                    eprintln!(
                        "cddl-codegen --emit-tests: {name}::{ctor} arg {field} not cheaply mintable — no round-trip case"
                    );
                    ok = false;
                    break;
                }
            }
        }
        if !ok {
            continue;
        }
        let unwrap = if arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)) {
            ".unwrap()"
        } else {
            ""
        };
        cases.push((
            format!("{name}::{ctor}({}){unwrap}", args.join(", ")),
            format!("variant {}", variant.name),
        ));
    }
    roundtrip_body(name, cases, conf)
}

/// Wrapper round-trip: one wire cycle with a valid inner value (bounds-respecting when `min_max`
/// is present — the wrapper checks the raw measure, no nint transform).
fn wrapper_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    wrapped: &RustType,
    min_max: Option<Bounds>,
    conf: Option<&str>,
) -> Option<String> {
    let inner = match min_max {
        Some(mm) => materialize(types, wrapped, valid_measure(mm)),
        None => valid_value(types, wrapped),
    };
    let Some(inner) = inner else {
        eprintln!(
            "cddl-codegen --emit-tests: no round-trip for {name} (inner value not cheaply mintable)"
        );
        return None;
    };
    // mirrors the wrapper ctor's fallibility rule: `new()` returns Result iff a min_max check exists
    let unwrap = if min_max.is_some() { ".unwrap()" } else { "" };
    roundtrip_body(
        name,
        vec![(
            format!("{name}::new({inner}){unwrap}"),
            "baseline".to_owned(),
        )],
        conf,
    )
}

/// Mirrors a choice-variant ctor's per-arg fallibility (`generation.rs` per-variant `can_fail`):
/// an arg makes `new_<variant>` return `Result` only when it needs an inlined bounds check AND a
/// check expression exists for its shape (a bounded named wrapper like `Hash` checks at ITS OWN
/// construction, so passing one in stays infallible).
fn arg_can_fail(types: &IntermediateTypes, ty: &RustType) -> bool {
    ty.needs_bounds_check_if_inlined(types)
        && crate::generation::bounds_check_expr_rust_type(ty, "x").is_some()
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
        let Some(arg_fields) = variant_arg_fields(types, variant) else {
            continue;
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

/// nint bounds are stored as u64 magnitudes; the minted baseline must pass the exact check `new()`
/// emits, so delegate to the generator's transform rather than reimplementing it. (A hand-rolled
/// copy previously omitted the min/max endpoint SWAP the generator applies — harmless for the
/// current single-endpoint `valid_measure`, but a latent divergence trap for the planned
/// construct-reject work. Sharing the one implementation removes that class outright.)
fn nint_bounds_to_u64(b: Bounds) -> Bounds {
    crate::generation::nint_bounds_to_u64(&b)
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

/// Named-struct minting recursion cap: deep enough for realistic nesting (e.g. a record holding a
/// tagged wrapper holding a record), finite for self-recursive types (whose unbounded tail is
/// minted empty at the cap, with a loud notice; any other capped mint gets the caller's loud skip).
const MAX_MINT_DEPTH: u8 = 4;

/// A valid in-range Rust value expression for `ty`, or `None` if it can't be cheaply minted.
fn valid_value(types: &IntermediateTypes, ty: &RustType) -> Option<String> {
    valid_value_at(types, ty, 0)
}

fn valid_value_at(types: &IntermediateTypes, ty: &RustType, depth: u8) -> Option<String> {
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
        // a field nesting a NAMED generated type: mint an instance of that type recursively
        ConceptualRustType::Rust(ident) => mint_struct(types, ident, depth),
        _ => {
            let bounds = ty.config.bounds.unwrap_or((None, None));
            // A length-measured type (array/map/text/bytes) minted at length 0 never serializes or
            // deserializes its elements, so a type whose every non-empty value is broken passes the
            // round-trip gate vacuously. Mint a single element when unbounded; bounded types already
            // carry a >=1 min (or a real max) via `valid_measure`.
            let unbounded_len =
                matches!(measure_kind(ty), Some(MeasureKind::Len)) && bounds == (None, None);
            let measure = if unbounded_len {
                1
            } else {
                valid_measure(bounds)
            };
            materialize_at(types, ty, measure, depth).or_else(|| {
                // An unbounded (`*`-occurrence, lower bound 0) collection whose element can't be
                // minted — e.g. a self-recursive element that hits the depth cap — is still valid
                // EMPTY, so terminate the mint with an empty collection rather than failing the
                // whole enclosing type. This ONLY applies to the unbounded case: a bounded
                // collection needs its exact length materialized, so it stays a loud `None`, and a
                // broken (non-`None`) element still surfaces normally above. Announce the
                // degradation (module invariant: never a SILENTLY-weakened test) — this
                // collection's element wire path goes unexercised.
                let fallback = unbounded_len.then(|| empty_collection(ty)).flatten();
                if fallback.is_some() {
                    eprintln!(
                        "cddl-codegen --emit-tests: unbounded collection element not cheaply mintable (recursion cap or unsupported element shape) — minted empty; its element wire path is unexercised"
                    );
                }
                fallback
            })
        }
    }
}

/// Mint a valid instance of a NAMED generated struct, recursing into its fields (depth-capped so
/// recursion terminates: at the cap this returns `None`, which an enclosing unbounded collection
/// absorbs by minting empty — loudly — while any other enclosing mint gets the caller's loud skip).
fn mint_struct(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
    depth: u8,
) -> Option<String> {
    if depth >= MAX_MINT_DEPTH {
        return None;
    }
    let rust_struct = types.rust_struct(ident)?;
    let name = ident.to_string();
    match rust_struct.variant() {
        RustStructType::Record(record) => {
            let ctor_fields: Vec<&RustField> = record
                .fields
                .iter()
                .filter(|f| {
                    !f.optional
                        && !f.rust_type.is_fixed_value()
                        && f.rust_type.config.default.is_none()
                })
                .collect();
            let args: Option<Vec<String>> = ctor_fields
                .iter()
                .map(|f| valid_value_at(types, &f.rust_type, depth + 1))
                .collect();
            let unwrap = if record_ctor_can_fail(record) {
                ".unwrap()"
            } else {
                ""
            };
            Some(format!("{name}::new({}){unwrap}", args?.join(", ")))
        }
        RustStructType::Wrapper { wrapped, min_max } => {
            let inner = match min_max {
                Some(mm) => materialize_at(types, wrapped, valid_measure(*mm), depth + 1)?,
                None => valid_value_at(types, wrapped, depth + 1)?,
            };
            let unwrap = if min_max.is_some() { ".unwrap()" } else { "" };
            Some(format!("{name}::new({inner}){unwrap}"))
        }
        RustStructType::CStyleEnum { variants } => {
            variants.first().map(|v| format!("{name}::{}", v.name))
        }
        RustStructType::TypeChoice { variants } | RustStructType::GroupChoice { variants, .. } => {
            // first constructible variant wins (deterministic: variant order is IR order)
            for variant in variants {
                let Some(arg_fields) = variant_arg_fields(types, variant) else {
                    continue;
                };
                let args: Option<Vec<String>> = arg_fields
                    .iter()
                    .map(|(ty, _)| valid_value_at(types, ty, depth + 1))
                    .collect();
                let Some(args) = args else { continue };
                let unwrap = if arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)) {
                    ".unwrap()"
                } else {
                    ""
                };
                return Some(format!(
                    "{name}::new_{}({}){unwrap}",
                    variant.name_as_var(),
                    args.join(", ")
                ));
            }
            None
        }
        // transparent aliases: an empty map/vec is valid for `*`-occurrence tables/arrays, and the
        // alias's associated `new()` resolves to the underlying map type's constructor
        // ponytail: named tables mint empty (one-entry minting needs the struct's insert API);
        // inline `{ * k => v }` map *fields* already mint one entry via materialize_at, so the map
        // element wire path is still exercised there. Named-table standalone element coverage is a
        // known residual (see TESTING_ROADMAP).
        RustStructType::Table { .. } => Some(format!("{name}::new()")),
        RustStructType::Array { element_type, .. } => {
            // mint one element so the element serialize/deserialize path runs; fall back to empty
            // (valid for `*`) when the element isn't cheaply mintable.
            Some(match valid_value_at(types, element_type, depth + 1) {
                Some(e) => format!("vec![{e}; 1]"),
                None => "vec![]".to_owned(),
            })
        }
        RustStructType::Extern | RustStructType::RawBytesType => None,
    }
}

/// An empty value expression for a collection `ty` (valid for a 0-lower-bound `*`-occurrence).
/// `Default::default()` covers every collection representation an inline map field can take
/// (`BTreeMap`, or the preserve-encodings `OrderedHashMap`, both `Default`), inferred from the
/// constructor-argument position; `vec![]` is the clearer form for arrays.
fn empty_collection(ty: &RustType) -> Option<String> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Array(_) => Some("vec![]".to_owned()),
        ConceptualRustType::Map(_, _) => Some("Default::default()".to_owned()),
        _ => None,
    }
}

/// Build a Rust value expression for `ty` whose bound-relevant measure equals `measure`
/// (the value itself for integers, the length for text/bytes/array/map).
fn materialize(types: &IntermediateTypes, ty: &RustType, measure: i128) -> Option<String> {
    materialize_at(types, ty, measure, 0)
}

fn materialize_at(
    types: &IntermediateTypes,
    ty: &RustType,
    measure: i128,
    depth: u8,
) -> Option<String> {
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
            let e = valid_value_at(types, elem, depth)?;
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
                // bool has exactly 2 distinct keys, so only lengths <= 2 are mintable — beyond
                // that the collect() would dedupe and the map would miss its target measure
                ConceptualRustType::Primitive(Primitive::Bool) if measure <= 2 => {
                    "__i == 1".to_owned()
                }
                _ => return None, // non-trivial keys aren't cheaply mintable
            };
            let val = valid_value_at(types, v, depth)?;
            // distinct keys 0..measure; collect() infers the map type from the target position
            Some(format!(
                "(0u64..{measure}).map(|__i| ({key}, {val})).collect()"
            ))
        }
        _ => None,
    }
}

/// The constructor arg list of a choice variant's `new_<variant>` (mirrors `generate_enum`), or
/// `None` when it isn't cheaply constructible (inlined records with optional fields — deferred).
fn variant_arg_fields<'a>(
    types: &'a IntermediateTypes,
    variant: &'a EnumVariant,
) -> Option<Vec<(&'a RustType, String)>> {
    match &variant.data {
        EnumVariantData::RustType(ty) => {
            if let Some(record) = ty_as_record(types, ty) {
                // group-choice variant backed by a multi-field record: ctor flattens its fields
                Some(
                    record
                        .fields
                        .iter()
                        .filter(|f| !f.optional && !f.rust_type.is_fixed_value())
                        .map(|f| (&f.rust_type, f.name.clone()))
                        .collect(),
                )
            } else if ty.is_fixed_value() {
                Some(vec![])
            } else {
                // single value passed straight in
                Some(vec![(ty, variant.name_as_var())])
            }
        }
        EnumVariantData::Inlined(record) => {
            if record.fields.iter().any(|f| f.optional) {
                // optional args complicate the baseline — defer
                return None;
            }
            Some(
                record
                    .fields
                    .iter()
                    .filter(|f| !f.rust_type.is_fixed_value())
                    .map(|f| (&f.rust_type, f.name.clone()))
                    .collect(),
            )
        }
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
