//! `--emit-tests` generated WASM-test emitter (the wasm-crate half of the emitted test surface).
//!
//! This is the SECOND renderer over the shared `emit_tests::MintValue` derivation surface: the rust
//! half (`emit_tests.rs`) renders each minted value as a rust-crate API string; this half renders
//! the SAME minted tree two ways at once — through the generated wasm WRAPPER API and through the
//! `cddl_lib::` rust API it path-depends on — and asserts they agree. The teeth (see the plan in
//! `draft/emit-wasm-tests/PLAN.md` §1) are, per mintable type:
//!
//! 1. **Cross-crate byte differential** — build the value through the wasm wrapper ctor/`new_*` AND,
//!    independently, through the `cddl_lib::` rust ctor; assert `to_cbor_bytes()` is byte-equal. A
//!    wrong conversion in a wasm `new`/`new_<variant>` can't cancel here (the rust build is
//!    independent), so this catches the identity-`.into()`-where-a-transform-was-needed class.
//! 2. **Wire round-trip** — `from_cbor_bytes(bytes)` then `to_cbor_bytes()` byte-identical.
//! 3. **Accessor read-back against emit-time literals** — primitive getters compared to the exact
//!    minted literal (NOT original-vs-back, which lets a wrong getter conversion cancel); enum
//!    `kind()`/`as_<variant>()` pinned to the minted variant.
//! 4. **Boundary acceptance** — bounded ctor surfaces: the accepted boundary value constructs
//!    (`.ok().is_some()`). The beyond-boundary REJECT direction is NOT host-executable — a wasm
//!    ctor's error path builds a `JsError` through a wasm-bindgen import, which panics under host
//!    `cargo test` ("cannot call wasm-bindgen imported functions on non-wasm targets"). Rejection is
//!    already pinned as `RangeCheck` on the wire by the rust `--emit-tests` module, so this half only
//!    confirms the acceptance plumbing (a `wasm_bounds_<type>` test).
//!
//! **wasm-API facts baked in here** (all verified against generated core output): `JsError: !Debug`,
//! so a wasm `Result` is unwrapped as `.ok().expect(..)`, never `.unwrap()`/`.expect()`; composite
//! ctor params cross as `&Wrapper` (hence the `&` before composite args); c-style enums cross by
//! value as the re-exported rust enum (no wrapper); fixed-value fields are omitted from `new`; and
//! `@newtype`/tag wrappers expose NO wasm `new` in the generated crate, so a wrapper entry type is
//! built by decoding the rust twin's bytes (`from_cbor_bytes`) rather than a wasm ctor.
//!
//! **Loud skips (never silent):** every shape this renderer can't faithfully express emits an
//! `eprintln!("cddl-codegen --emit-tests: ...")` and is dropped — wrapper-typed composite ctor args
//! (no wasm ctor to build them), non-primitive-element collection fields (wrapper lists / maps, whose
//! block-expr build is deferred), optional-nullable present-null flatten points, and the macro-API
//! flag configurations (whole module). The hand-written `tests/<dir>/tests_wasm.rs` covers the
//! deferred collection/wrapper shapes as a plausibility cross-check.
//!
//! **Mutation-verified (red-first, per repo idiom — the same discipline as `emit_tests.rs`'s
//! constant-writing-serializer check).** Three hand-applied `generation.rs` mutations — each an
//! integer `.wrapping_add(1)` injected at one wasm-boundary site — turned this module RED on exactly
//! the intended assertion class, and only that class (verified, then reverted):
//!   (a) integer record GETTER conversion (`codegen_struct` getter) → §3 accessor read-back fires
//!       (18 record read-backs red; no differential/wire failures);
//!   (b) integer record CTOR arg (`codegen_struct` `new`)           → §1 byte differential fires
//!       (22 ctor differentials red);
//!   (c) integer type-choice `new_<variant>` inner conversion       → §1 byte differential fires
//!       (3 `new_uint` differentials red).

use crate::cli::Cli;
use crate::emit_tests::{
    self, MapKey, MintValue, arg_can_fail, bound_cases, measure_kind, mint_struct,
    record_ctor_can_fail, valid_value, variant_arg_fields,
};
use crate::generation::rust_crate_struct_from_wasm;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, IntermediateTypes, RustField, RustRecord,
    RustStructType, RustType,
};
use std::collections::BTreeMap;

/// Map from a generated type's name to its fully-scoped `cddl_lib::` path (for the rust twin).
type ScopeMap = BTreeMap<String, String>;

/// Emit the `#[cfg(test)]` generated wasm-test module, or `None` if nothing could be minted / the
/// configuration replaces the method surface this renderer targets.
pub fn emit_generated_wasm_tests(types: &IntermediateTypes, cli: &Cli) -> Option<String> {
    if !cli.to_from_bytes_methods {
        eprintln!(
            "cddl-codegen --emit-tests: wasm module skipped (requires --to-from-bytes-methods, which is off)"
        );
        return None;
    }
    // The macro-API flags REPLACE the per-type method surface (new/getters/to_from_bytes) this
    // renderer targets, so the whole module can't be soundly emitted under them.
    if cli.wasm_cbor_json_api_macro.is_some()
        || cli.wasm_conversions_macro.is_some()
        || cli.wasm_list_macro.is_some()
    {
        eprintln!(
            "cddl-codegen --emit-tests: wasm module skipped (a --wasm-*-macro flag replaces the wrapper method surface)"
        );
        return None;
    }

    let scoped: ScopeMap = types
        .rust_structs()
        .keys()
        .map(|id| (id.to_string(), rust_crate_struct_from_wasm(types, id, cli)))
        .collect();

    let mut fns: Vec<String> = Vec::new();
    for (ident, rust_struct) in types.rust_structs() {
        let name = ident.to_string();
        let roundtrip = match rust_struct.variant() {
            RustStructType::Record(record) => {
                wasm_record_roundtrip(types, ident, &name, record, &scoped, cli)
            }
            RustStructType::TypeChoice { variants } => {
                wasm_choice_roundtrip(types, &name, variants, false, &scoped, cli)
            }
            RustStructType::GroupChoice { variants, .. } => {
                wasm_choice_roundtrip(types, &name, variants, true, &scoped, cli)
            }
            RustStructType::Wrapper { .. } => wasm_wrapper_roundtrip(types, ident, &name, &scoped),
            // c-style enums serialize inline (no standalone wasm CBOR surface); tables/arrays are
            // wrapper types with NO CBOR methods (exercised only inside composite mints); extern/raw
            // reference user code.
            _ => None,
        };
        if let Some(body) = roundtrip
            && !body.is_empty()
        {
            fns.push(format!(
                "#[test]\nfn wasm_roundtrip_{}() {{\n{body}\n}}\n",
                crate::utils::convert_to_snake_case(&name),
            ));
        }

        let bounds = match rust_struct.variant() {
            RustStructType::Record(record) => {
                wasm_record_bounds(types, &name, record, &scoped, cli)
            }
            RustStructType::TypeChoice { variants } => {
                wasm_choice_bounds(types, &name, variants, false, &scoped, cli)
            }
            RustStructType::GroupChoice { variants, .. } => {
                wasm_choice_bounds(types, &name, variants, true, &scoped, cli)
            }
            _ => None,
        };
        if let Some(body) = bounds
            && !body.is_empty()
        {
            fns.push(format!(
                "#[test]\nfn wasm_bounds_{}() {{\n{body}\n}}\n",
                crate::utils::convert_to_snake_case(&name),
            ));
        }
    }

    if fns.is_empty() {
        return None;
    }
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\nmod cddl_generated_wasm_tests {{\n    use super::*;\n{}\n}}\n",
        fns.join("\n")
    ))
}

// ============================================================================================
// Rendering: the SAME MintValue tree → the rust-twin (`cddl_lib::`) form and the wasm-wrapper form.
// ============================================================================================

/// `cddl_lib::`-scoped rust value expression (the independent twin for the byte differential).
/// Mirrors `emit_tests::render_rust` but scope-qualifies every named type.
fn rust_scoped(mv: &MintValue, scoped: &ScopeMap) -> String {
    let sc = |ident: &str| {
        scoped
            .get(ident)
            .cloned()
            .unwrap_or_else(|| ident.to_string())
    };
    let unwrap = |can_fail: bool| if can_fail { ".unwrap()" } else { "" };
    match mv {
        MintValue::None => "None".to_owned(),
        MintValue::Bool => "false".to_owned(),
        MintValue::Float => "0.0".to_owned(),
        MintValue::Int { value, .. } => format!("{value}"),
        MintValue::Str { len } => format!("\"a\".repeat({len})"),
        MintValue::Bytes { len } => format!("vec![0u8; {len}]"),
        MintValue::Array {
            elem: Some(e),
            count,
        } => format!("vec![{}; {count}]", rust_scoped(e, scoped)),
        MintValue::Array { elem: None, .. } => "vec![]".to_owned(),
        MintValue::Map { key, val, count } => {
            let k = map_key_expr(key);
            format!(
                "(0u64..{count}).map(|__i| ({k}, {})).collect()",
                rust_scoped(val, scoped)
            )
        }
        MintValue::DefaultMap => "Default::default()".to_owned(),
        MintValue::Record {
            ident,
            args,
            can_fail,
        } => {
            let a: Vec<String> = args.iter().map(|m| rust_scoped(m, scoped)).collect();
            format!("{}::new({}){}", sc(ident), a.join(", "), unwrap(*can_fail))
        }
        MintValue::Wrapper {
            ident,
            inner,
            can_fail,
        } => format!(
            "{}::new({}){}",
            sc(ident),
            rust_scoped(inner, scoped),
            unwrap(*can_fail)
        ),
        MintValue::CEnum { ident, variant } => format!("{}::{variant}", sc(ident)),
        MintValue::Choice {
            ident,
            variant,
            args,
            can_fail,
        } => {
            let a: Vec<String> = args.iter().map(|m| rust_scoped(m, scoped)).collect();
            format!(
                "{}::new_{variant}({}){}",
                sc(ident),
                a.join(", "),
                unwrap(*can_fail)
            )
        }
        MintValue::TableEmpty { ident } => format!("{}::new()", sc(ident)),
        MintValue::IntExtern { ident, value } => format!("{}::new_uint({value})", sc(ident)),
    }
}

fn map_key_expr(key: &MapKey) -> String {
    match key {
        MapKey::Int(p) => format!("__i as {p}"),
        MapKey::Str => "__i.to_string()".to_owned(),
        MapKey::Bool => "__i == 1".to_owned(),
    }
}

/// The wasm wrapper-API value expression for `mv` of resolved type `ty`, or `None` (skip the whole
/// enclosing type, loudly at the caller) when the shape has no faithful wasm-ctor build.
fn wasm_value(
    types: &IntermediateTypes,
    mv: &MintValue,
    ty: &ConceptualRustType,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    match ty {
        ConceptualRustType::Primitive(_) => Some(emit_tests::render_rust(mv)),
        ConceptualRustType::Optional(inner) => match mv {
            // a mandatory-nullable ctor arg mints its degenerate `None` baseline
            MintValue::None => Some("None".to_owned()),
            other => Some(format!(
                "Some({})",
                wasm_value(types, other, inner.resolve_alias_shallow(), scoped, cli)?
            )),
        },
        ConceptualRustType::Array(_) => {
            if ty.directly_wasm_exposable(types) {
                // wasm exposes this as a plain Vec<prim>, identical literal to the rust side
                Some(emit_tests::render_rust(mv))
            } else {
                // a wrapper List (FooList, …): block-expr build via new/add is deferred
                None
            }
        }
        ConceptualRustType::Map(_, _) => None, // wrapper map block-expr build deferred
        ConceptualRustType::Rust(ident) => wasm_named(types, ident, mv, scoped, cli),
        ConceptualRustType::Fixed(_) | ConceptualRustType::Alias(_, _) => None,
    }
}

/// Build a named generated type through its wasm wrapper API from `mv`.
fn wasm_named(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
    mv: &MintValue,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let name = ident.to_string();
    match types.rust_struct(ident)?.variant() {
        // c-style enums cross by value as the re-exported rust enum
        RustStructType::CStyleEnum { .. } => Some(emit_tests::render_rust(mv)),
        RustStructType::Record(record) => {
            let MintValue::Record { args, .. } = mv else {
                return None;
            };
            let ctor_fields = record_ctor_fields(record);
            if ctor_fields.len() != args.len() {
                return None;
            }
            let mut wasm_args = Vec::new();
            for (f, amv) in ctor_fields.iter().zip(args) {
                wasm_args.push(wasm_arg(types, amv, &f.rust_type, scoped, cli)?);
            }
            let call = format!("{name}::new({})", wasm_args.join(", "));
            Some(finish_fallible(call, record_ctor_can_fail(record), &name))
        }
        RustStructType::TypeChoice { variants } => {
            wasm_choice_value(types, &name, variants, false, mv, scoped, cli)
        }
        RustStructType::GroupChoice { variants, .. } => {
            wasm_choice_value(types, &name, variants, true, mv, scoped, cli)
        }
        // the reserved `Int` extern crosses the wasm boundary as a wrapper with a single
        // `Int::new(x: i64)` ctor (dispatches to `new_uint`/`new_nint` on sign); mint the
        // non-negative baseline. Every other extern references user code with no wasm ctor.
        RustStructType::Extern if name == "Int" => {
            let MintValue::IntExtern { value, .. } = mv else {
                return None;
            };
            Some(format!("Int::new({value})"))
        }
        // @newtype/tag wrappers have no wasm `new`; a wrapper as a ctor ARG can't be built here
        RustStructType::Wrapper { .. }
        | RustStructType::Table { .. }
        | RustStructType::Array { .. }
        | RustStructType::Extern
        | RustStructType::RawBytesType => None,
    }
}

/// Build a choice variant through `new_<variant>` from a `Choice` mint value.
fn wasm_choice_value(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    group_choice: bool,
    mv: &MintValue,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let MintValue::Choice {
        variant: var, args, ..
    } = mv
    else {
        return None;
    };
    let variant = variants.iter().find(|v| &v.name_as_var() == var)?;
    let arg_fields = variant_arg_fields(types, variant, group_choice)?;
    if arg_fields.len() != args.len() {
        return None;
    }
    let mut wasm_args = Vec::new();
    for ((ty, _), amv) in arg_fields.iter().zip(args) {
        wasm_args.push(wasm_arg(types, amv, ty, scoped, cli)?);
    }
    let can_fail = arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty));
    let call = format!("{name}::new_{var}({})", wasm_args.join(", "));
    Some(finish_fallible(
        call,
        can_fail,
        &format!("{name}::new_{var}"),
    ))
}

/// A single wasm ctor argument: the value expression prefixed with `&` when the wasm param is a ref
/// (composite wrappers / wrapper collections), matching `for_wasm_param`.
fn wasm_arg(
    types: &IntermediateTypes,
    mv: &MintValue,
    field_ty: &RustType,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let resolved = field_ty.resolve_alias_shallow();
    // A wrapper collection reached through a rust alias (`nums = [* uint]` used as a field mints
    // `Alias(Rust(Nums), Array(U64))`) crosses the wasm boundary as `&Nums`, NOT a transparent
    // `Vec` — but shallow-resolving the alias would peel the `Nums` wrapper away and let the Array
    // branch mint a bare `vec![..]`, which no longer type-checks against the `&Nums` ctor param.
    // `directly_wasm_exposable` on the UNRESOLVED type sees the wrapper (returns false), so use it to
    // loud-skip here — the block-expr wrapper-collection build is the same deferred class as an
    // inline non-exposable list/map field (which `wasm_value` already returns None for).
    if matches!(
        resolved,
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
    ) && !field_ty.conceptual_type.directly_wasm_exposable(types)
    {
        return None;
    }
    let val = wasm_value(types, mv, resolved, scoped, cli)?;
    if field_ty.for_wasm_param(types).starts_with('&') {
        Some(format!("&{val}"))
    } else {
        Some(val)
    }
}

/// A fallible wasm ctor returns `Result<_, JsError>`; `JsError: !Debug`, so embed via `.ok().expect`.
fn finish_fallible(call: String, can_fail: bool, what: &str) -> String {
    if can_fail {
        format!("{call}.ok().expect(\"{what}\")")
    } else {
        call
    }
}

// ============================================================================================
// Round-trip emitters (one `wasm_roundtrip_<type>` body per mintable entry type).
// ============================================================================================

fn wasm_record_roundtrip(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
    name: &str,
    record: &RustRecord,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let entry_mv = mint_struct(types, ident, 0)?;
    let MintValue::Record { args, .. } = &entry_mv else {
        return None;
    };
    let Some(wasm_build) = wasm_named(types, ident, &entry_mv, scoped, cli) else {
        eprintln!(
            "cddl-codegen --emit-tests: no wasm round-trip for {name} (a ctor arg has no wasm build — wrapper/collection field)"
        );
        return None;
    };
    let rust_build = rust_scoped(&entry_mv, scoped);

    // §3 accessor read-back: primitive/c-enum ctor getters against the emit-time literal.
    let ctor_fields = record_ctor_fields(record);
    let mut readbacks = Vec::new();
    for (f, amv) in ctor_fields.iter().zip(args) {
        if let Some(expected) = scalar_readback(&f.rust_type, amv) {
            // read back on the freshly-BUILT value (not the post-wire `back`): a getter reads
            // `self.0` through its `to_wasm_boundary` conversion, so a broken conversion still
            // fails here, while reading pre-wire sidesteps deser variant-ambiguity (a wire-ambiguous
            // choice can decode to a different-but-byte-equal variant).
            readbacks.push(format!(
                "        assert_eq!(wasm_v.{}(), {expected}, \"{name}.{} accessor must read back the minted value\");",
                f.name, f.name
            ));
        }
    }
    Some(roundtrip_body(name, &wasm_build, &rust_build, &readbacks))
}

fn wasm_choice_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    group_choice: bool,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let mut blocks = Vec::new();
    for variant in variants {
        let var = variant.name_as_var();
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            continue;
        };
        // mint each arg
        let mut mvs = Vec::new();
        let mut ok = true;
        for (ty, _) in &arg_fields {
            match valid_value(types, ty) {
                Some(m) => mvs.push(m),
                None => {
                    ok = false;
                    break;
                }
            }
        }
        if !ok {
            continue;
        }
        let choice_mv = MintValue::Choice {
            ident: name.to_owned(),
            variant: var.clone(),
            args: mvs,
            can_fail: arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)),
        };
        let Some(wasm_build) =
            wasm_choice_value(types, name, variants, group_choice, &choice_mv, scoped, cli)
        else {
            eprintln!(
                "cddl-codegen --emit-tests: no wasm round-trip for {name}::new_{var} (a variant arg has no wasm build)"
            );
            continue;
        };
        let rust_build = rust_scoped(&choice_mv, scoped);

        // §3: kind() pinned to the minted variant; as_<variant>() Some (== literal for a single
        // primitive payload) and a sibling variant's as_() None.
        // read back on the freshly-BUILT `wasm_v` (not post-wire `back`): a wire-ambiguous choice
        // (e.g. uint `0` vs a fixed `i0` variant) can decode to a different byte-equal variant.
        let mut readbacks = vec![format!(
            "        assert!(matches!(wasm_v.kind(), {name}Kind::{}), \"{name} kind() must be {}\");",
            variant.name, variant.name
        )];
        if !arg_fields.is_empty() {
            // as_<variant>() must answer for the minted variant. A direct `== Some(literal)` compare
            // is only sound when the variant's PAYLOAD is itself a primitive returned as-is by the
            // getter (`RustType(primitive)` → `as_<var>()` returns the primitive). A record-backed
            // variant — even a group-choice one flattened to a single ctor field — returns the
            // embedded WRAPPER (`Option<Ed>`), and an inlined variant likewise, so fall back to
            // `is_some()` there.
            let primitive_payload = matches!(
                &variant.data,
                EnumVariantData::RustType(vty)
                    if matches!(vty.resolve_alias_shallow(), ConceptualRustType::Primitive(_))
            );
            if primitive_payload
                && let [(ty, _)] = arg_fields.as_slice()
                && let Some(expected) = scalar_readback(ty, &choice_variant_first_arg(&choice_mv)?)
            {
                readbacks.push(format!(
                    "        assert_eq!(wasm_v.as_{var}(), Some({expected}), \"{name}.as_{var}() must read back the minted payload\");"
                ));
            } else {
                readbacks.push(format!(
                    "        assert!(wasm_v.as_{var}().is_some(), \"{name}.as_{var}() must be Some for the {} variant\");",
                    variant.name
                ));
            }
            if let Some(other) = variants
                .iter()
                .find(|v| v.name_as_var() != var && !variant_is_fixed(types, v, group_choice))
            {
                readbacks.push(format!(
                    "        assert!(wasm_v.as_{}().is_none(), \"{name}.as_{}() must be None on the {} variant\");",
                    other.name_as_var(),
                    other.name_as_var(),
                    variant.name
                ));
            }
        }
        blocks.push(roundtrip_case(
            name,
            &var,
            &wasm_build,
            &rust_build,
            &readbacks,
        ));
    }
    if blocks.is_empty() {
        None
    } else {
        Some(blocks.join("\n"))
    }
}

fn wasm_wrapper_roundtrip(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
    name: &str,
    scoped: &ScopeMap,
) -> Option<String> {
    // No wasm `new` for wrappers in the generated crate: build the wasm value by decoding the rust
    // twin's bytes, then assert the wasm wire round-trip is byte-identical.
    let entry_mv = mint_struct(types, ident, 0)?;
    if !matches!(entry_mv, MintValue::Wrapper { .. }) {
        return None;
    }
    let rust_build = rust_scoped(&entry_mv, scoped);
    Some(format!(
        "    {{
        let rust_v = {rust_build};
        let bytes = cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&rust_v);
        let wasm_v = {name}::from_cbor_bytes(&bytes).ok().expect(\"{name}::from_cbor_bytes\");
        assert_eq!(wasm_v.to_cbor_bytes(), bytes, \"{name}: wasm wire round-trip must be byte-identical\");
    }}"
    ))
}

/// The shared body for a single-value (record/wrapper) round-trip test: §1 differential, §2 wire, §3.
fn roundtrip_body(name: &str, wasm_build: &str, rust_build: &str, readbacks: &[String]) -> String {
    let rb = if readbacks.is_empty() {
        String::new()
    } else {
        format!("\n{}", readbacks.join("\n"))
    };
    format!(
        "    {{
        let wasm_v = {wasm_build};
        let rust_v = {rust_build};
        let bytes = wasm_v.to_cbor_bytes();
        assert_eq!(bytes, cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&rust_v), \"{name}: wasm-built and rust-built bytes must match (ctor conversion)\");
        let back = {name}::from_cbor_bytes(&bytes).ok().expect(\"{name}::from_cbor_bytes\");
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name}: wasm wire round-trip must be byte-identical\");{rb}
    }}"
    )
}

/// The per-variant round-trip case for a choice.
fn roundtrip_case(
    name: &str,
    var: &str,
    wasm_build: &str,
    rust_build: &str,
    readbacks: &[String],
) -> String {
    let rb = if readbacks.is_empty() {
        String::new()
    } else {
        format!("\n{}", readbacks.join("\n"))
    };
    format!(
        "    {{
        let wasm_v = {wasm_build};
        let rust_v = {rust_build};
        let bytes = wasm_v.to_cbor_bytes();
        assert_eq!(bytes, cddl_lib::serialization::ToCBORBytes::to_cbor_bytes(&rust_v), \"{name}::{var}: wasm-built and rust-built bytes must match (new_{var} conversion)\");
        let back = {name}::from_cbor_bytes(&bytes).ok().expect(\"{name}::from_cbor_bytes ({var})\");
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name}::{var}: wasm wire round-trip must be byte-identical\");{rb}
    }}"
    )
}

// ============================================================================================
// Boundary emitters (bounded ctor ACCEPTANCE plumbing).
//
// Only the ACCEPTED-boundary direction (`.ok().is_some()`) is host-executable: a wasm ctor's error
// path builds a `JsError` via a wasm-bindgen import, which panics ("cannot call wasm-bindgen imported
// functions on non-wasm targets") under host `cargo test`. So the beyond-boundary REJECT direction
// can't run here — the rust `--emit-tests` module already pins rejection as `RangeCheck` on the wire;
// this half only confirms the bounded wasm ctor accepts its exact boundary value.
// ============================================================================================

fn wasm_record_bounds(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let ctor_fields = record_ctor_fields(record);
    // baseline args for every ctor field (all must be wasm-mintable)
    let mut baseline: Vec<String> = Vec::new();
    for f in &ctor_fields {
        let m = valid_value(types, &f.rust_type)?;
        baseline.push(wasm_arg(types, &m, &f.rust_type, scoped, cli)?);
    }
    let mut lines = Vec::new();
    for (i, f) in ctor_fields.iter().enumerate() {
        if !bounded_scalar(&f.rust_type) {
            continue;
        }
        let Some(bounds) = f.rust_type.config.bounds else {
            continue;
        };
        let is_len = measure_kind(&f.rust_type) == Some(emit_tests::MeasureKind::Len);
        for (mv, label) in accept_cases(types, &f.rust_type, bounds, is_len) {
            let mut args = baseline.clone();
            args[i] = emit_tests::render_rust(&mv); // bounded scalars render identically wasm/rust
            let call = format!("{name}::new({})", args.join(", "));
            lines.push(accept_assert(&call, name, &f.name, label));
        }
    }
    if lines.is_empty() {
        None
    } else {
        Some(lines.join("\n"))
    }
}

fn wasm_choice_bounds(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    group_choice: bool,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let mut lines = Vec::new();
    for variant in variants {
        let var = variant.name_as_var();
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            continue;
        };
        for (i, (arg_ty, _)) in arg_fields.iter().enumerate() {
            if !bounded_scalar(arg_ty) {
                continue;
            }
            let Some(bounds) = arg_ty.config.bounds else {
                continue;
            };
            let is_len = measure_kind(arg_ty) == Some(emit_tests::MeasureKind::Len);
            let accepts = accept_cases(types, arg_ty, bounds, is_len);
            if accepts.is_empty() {
                continue;
            }
            // valid baseline args for the whole variant (all wasm-mintable)
            let mut baseline: Vec<String> = Vec::new();
            let mut ok = true;
            for (ty, _) in &arg_fields {
                match valid_value(types, ty).and_then(|m| wasm_arg(types, &m, ty, scoped, cli)) {
                    Some(a) => baseline.push(a),
                    None => {
                        ok = false;
                        break;
                    }
                }
            }
            if !ok {
                continue;
            }
            for (mv, label) in accepts {
                let mut args = baseline.clone();
                args[i] = emit_tests::render_rust(&mv);
                let call = format!("{name}::new_{var}({})", args.join(", "));
                lines.push(accept_assert(&call, name, &format!("new_{var}"), label));
            }
        }
    }
    if lines.is_empty() {
        None
    } else {
        Some(lines.join("\n"))
    }
}

/// The ACCEPTED-boundary cases only (the host-safe half of `bound_cases`).
fn accept_cases(
    types: &IntermediateTypes,
    ty: &RustType,
    bounds: (Option<i128>, Option<i128>),
    is_len: bool,
) -> Vec<(MintValue, &'static str)> {
    bound_cases(types, ty, bounds, is_len)
        .into_iter()
        .filter_map(|(mv, accept, label)| accept.then_some((mv, label)))
        .collect()
}

/// Boundary-acceptance assertion: the bounded wasm ctor accepts its exact boundary value.
fn accept_assert(call: &str, name: &str, field: &str, label: &str) -> String {
    format!(
        "    assert!({call}.ok().is_some(), \"{name}.{field} {label} must be accepted at the boundary\");"
    )
}

// ============================================================================================
// Small shared helpers.
// ============================================================================================

/// The constructor field list (mirrors `codegen_struct`): mandatory, non-fixed, non-default.
fn record_ctor_fields(record: &RustRecord) -> Vec<&RustField> {
    record
        .fields
        .iter()
        .filter(|f| {
            !f.optional && !f.rust_type.is_fixed_value() && f.rust_type.config.default.is_none()
        })
        .collect()
}

/// If the getter for `mv` is directly comparable to the emit-time literal, the expected expression;
/// else `None` (composite getters are covered by the byte differential + wire round-trip only). A
/// primitive getter returns the value; a c-style enum getter returns the re-exported enum by value
/// (a `CEnum` mint value is only ever produced for a c-style enum, so it's a sound signal).
fn scalar_readback(ty: &RustType, mv: &MintValue) -> Option<String> {
    match (ty.resolve_alias_shallow(), mv) {
        (ConceptualRustType::Primitive(_), _) => Some(emit_tests::render_rust(mv)),
        (_, MintValue::CEnum { .. }) => Some(emit_tests::render_rust(mv)),
        _ => None,
    }
}

/// Is `ty` a bounded SCALAR (integer / text / bytes) we can push past its boundary with a literal?
/// Array/map length bounds need a wasm collection build (deferred), so they're excluded here.
fn bounded_scalar(ty: &RustType) -> bool {
    ty.config.bounds.is_some()
        && measure_kind(ty).is_some()
        && matches!(ty.resolve_alias_shallow(), ConceptualRustType::Primitive(_))
}

/// Does this variant carry no payload (fixed value → no `as_<variant>()` getter)?
fn variant_is_fixed(types: &IntermediateTypes, variant: &EnumVariant, group_choice: bool) -> bool {
    variant_arg_fields(types, variant, group_choice)
        .map(|a| a.is_empty())
        .unwrap_or(true)
}

/// The first arg of a `Choice` mint value (for a single-payload variant read-back).
fn choice_variant_first_arg(mv: &MintValue) -> Option<MintValue> {
    if let MintValue::Choice { args, .. } = mv {
        args.first().cloned()
    } else {
        None
    }
}
