//! `--emit-tests` generated WASM-test emitter (the wasm-crate half of the emitted test surface).
//!
//! This is the SECOND renderer over the shared `emit_tests::MintValue` derivation surface: the rust
//! half (`emit_tests.rs`) renders each minted value as a rust-crate API string; this half renders
//! the SAME minted tree two ways at once — through the generated wasm WRAPPER API and through the
//! `cddl_lib::` rust API it path-depends on — and asserts they agree. The teeth are, per mintable
//! type:
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
//! value as the re-exported rust enum (no wrapper); fixed-value fields are omitted from `new`; every
//! `@newtype`/tag/bounded wrapper exposes a wasm `new(inner)` ctor (`Result`-returning when the bound
//! makes it fallible) plus an inner-value getter (`get`, or the `@newtype <name>` rename), so a
//! wrapper ENTRY type is built through that public `new` (`wasm_wrapper_roundtrip`) — the minted inner
//! is rendered by the same ctor-arg machinery (`wasm_arg`) and the getter is read back against the
//! minted literal for a primitive inner. A wrapper CTOR ARG (a wrapper appearing as another type's
//! ctor field) is instead built via the `From<cddl_lib::Native>` impl every wasm wrapper carries
//! (`wasm_named`): a convenience choice, since the native expr is already at hand there and every
//! named wrapper is a top-level rule that gets its own entry test exercising `new`. A wrapper
//! COLLECTION ctor arg (`FooList`/`FooMap`, or an aliased `nums = [* uint]` -> `&Nums`) is built as a
//! block expression through the wrapper's `new`/`add`/`insert` API (`wasm_collection_build`).
//!
//! **Loud skips (never silent):** every shape this renderer can't faithfully express emits a
//! `crate::warn!("cddl-codegen --emit-tests: ...")` — stderr, visible at the default verbosity — and
//! is dropped: a ctor arg with no wasm build (a wrapper collection past a name-erasing point, a
//! `Fixed`/`Alias`/`any` inner) and the macro-API flag configurations (whole module). An exact,
//! bounded, or bounded-map wrapper collection with such an element instead builds through its
//! `From<core>` bridge.
//!
//! **Where the extern / raw-bytes skip actually lives: the RUST half, not here.** The SHARED minter
//! (`emit_tests::mint_struct`) returns `None` for `RustStructType::Extern` (other than the reserved
//! `Int`) and `RustStructType::RawBytesType`, and `materialize_at` mints no `Rust(ident)` at all, so
//! NO `MintValue` for those classes exists anywhere. A type with an extern/raw-bytes ctor arg — or a
//! wrapper around one — therefore fails to mint upstream and is dropped with the rust half's own
//! loud warn ("… not cheaply mintable") before this renderer is reached. Nothing in this module
//! observes an extern value today. Of the two arms that name the class: `wasm_named`'s is a
//! variant-specific backstop nothing can reach (kept as the site a future extern-minting change
//! must teach), while `wasm_wrapper_roundtrip`'s from_cbor_bytes fallback IS live for a DIFFERENT
//! cause — an inner the rust minter can mint but this renderer can't express, verified on
//! `#6.42(any)`, or a wrapper collection past a name-erasing point — which is why its message names
//! the condition rather than the class.
//!
//! Optional-nullable flatten points need no skip: optional fields are not ctor args, so no
//! mint ever constructs a present-null state (the three-state write/read surface is covered by the
//! hand-written `tests/nullable-wasm/` fixture instead). The hand-written `tests/<dir>/tests_wasm.rs`
//! covers the collection/wrapper shapes as a plausibility cross-check.
//!
//! **Mutation-verified (red-first, per repo idiom — the same discipline as `emit_tests.rs`'s
//! constant-writing-serializer check).** Three hand-applied `generation/` mutations — each an
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
    self, MintValue, TypePaths, arg_can_fail, bound_cases, map_key_literal, measure_kind,
    mint_struct, multi_array_occurrence_ctor_arg_slots, record_ctor_arg_types,
    record_wasm_ctor_can_fail, valid_value, variant_arg_fields,
};
use crate::generation::rust_crate_struct_from_wasm;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, IntWindow, IntermediateTypes, RustField,
    RustIdent, RustRecord, RustStructType, RustType,
};
use std::collections::{BTreeMap, BTreeSet};

/// Map from a generated type's name to its fully-scoped `cddl_lib::` path (for the rust twin).
type ScopeMap = BTreeMap<String, String>;

/// Emit the `#[cfg(test)]` generated wasm-test module, or `None` if nothing could be minted / the
/// configuration replaces the method surface this renderer targets.
///
/// `submodules` — the WASM crate's declared non-root module paths (multifile output; see the call
/// site in `generation/mod.rs` for the derivation). The module this emits lands at the generated root
/// while its minted wrapper values name submodule types bare, so each entry contributes a
/// `use super::<m>::*;` glob; empty (single-file output) emits nothing extra, keeping that output
/// byte-identical. (The rust-twin side of each assertion is already fully qualified via
/// `rust_crate_struct_from_wasm` — only the wrapper names need the globs.)
pub fn emit_generated_wasm_tests(
    types: &IntermediateTypes,
    cli: &Cli,
    submodules: &[String],
    no_deserialize: &BTreeSet<RustIdent>,
) -> Option<String> {
    if !cli.to_from_bytes_methods {
        crate::warn!(
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
        crate::warn!(
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
        // The wrapper's `from_cbor_bytes` is emitted only when the rust face gave the inner type a
        // `Deserialize`, so the round-trip half has nothing to call for a refused type. The BOUNDS
        // half is a pure constructor test and still mints — the skip is the decode half only.
        let deserializable = !no_deserialize.contains(ident);
        if !deserializable {
            crate::warn!(
                "cddl-codegen --emit-tests: {name} wasm round-trip skipped (no Deserialize impl was generated for it)"
            );
        }
        let roundtrip = match rust_struct.variant() {
            _ if !deserializable => None,
            RustStructType::Record(record) => {
                wasm_record_roundtrip(types, ident, &name, record, &scoped, cli)
            }
            RustStructType::TypeChoice { variants } => {
                wasm_choice_roundtrip(types, ident, &name, variants, false, &scoped, cli)
            }
            RustStructType::GroupChoice { variants, .. } => {
                wasm_choice_roundtrip(types, ident, &name, variants, true, &scoped, cli)
            }
            RustStructType::Wrapper { .. } => {
                wasm_wrapper_roundtrip(types, ident, &name, &scoped, cli)
            }
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
    // Bring the rust twin's serialization trait into scope so `rust_v.to_cbor_bytes()` resolves as
    // a method regardless of which trait provides it — `ToCBORBytes` under default flags, `Serialize`
    // under `--preserve-encodings`/`--canonical-form` (a fully-qualified `ToCBORBytes::` path fails
    // to compile under preserve, where that trait doesn't exist).
    //
    // Multifile output: glob-import each declared non-root module — the minted wrapper values name
    // submodule types bare, and `use super::*;` only reaches root-scope items (E0433 otherwise).
    // Empty for single-file output (byte-identical). E0659 glob-collision caveat + the
    // fully-qualified long-term alternative: see the call site.
    let scope_globs: String = submodules
        .iter()
        .map(|path| format!("    use super::{path}::*;\n"))
        .collect();
    // A tagged wrapper around `any` has no value-destructuring wasm ctor. Its test therefore
    // decodes the rust twin's bytes, and that twin's shared `MintValue::Any` renderer uses the
    // same `__AnyCborMint` alias as the rust generated-test module. Keep this conditional so an
    // any-free wasm test module remains byte-identical.
    // Both imports reach the rust crate through the same prefix every other wasm-side runtime path
    // uses (`--common-import-override`, else the `--lib-name` code form), not a literal `cddl_lib`
    // (E0433 under any non-default lib name).
    let common = cli.common_import_wasm();
    let any_import = if types.uses_any_cbor() {
        format!("    use {common}::any_cbor::AnyCbor as __AnyCborMint;\n")
    } else {
        String::new()
    };
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\n#[allow(unused_imports)]\nmod cddl_generated_wasm_tests {{\n    use super::*;\n{scope_globs}    use {common}::serialization::*;\n{any_import}{}\n}}\n",
        fns.join("\n")
    ))
}

// ============================================================================================
// Rendering: the SAME MintValue tree → the wasm-wrapper form. The independent rust twin (the
// `cddl_lib::` form) is `emit_tests::render_rust_for_named` / `render_rust_for_direct_storage`
// under `TypePaths::Scoped`.
// ============================================================================================

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
            if ty.directly_wasm_exposable_ct(types) {
                // wasm exposes this as a plain Vec<prim>, identical literal to the rust side
                Some(emit_tests::render_rust(mv))
            } else {
                // a wrapper List (FooList, …): the new/add block-expr build lives in `wasm_arg`,
                // which still holds the UNRESOLVED type carrying the wrapper NAME (a resolved
                // `Array(_)` here has already lost it — the coll__struct-field trap). Reaching this
                // arm means the wrapper collection sat past a name-erasing point (e.g. nested in an
                // `Optional`), which stays a deferred loud skip at the caller.
                None
            }
        }
        // wrapper Map (FooMap, …): same as the array arm — the new/insert block-expr build lives in
        // `wasm_arg` where the wrapper name survives; a resolved `Map(_,_)` here has lost it.
        ConceptualRustType::Map(_, _) => None,
        ConceptualRustType::Rust(ident) => wasm_named(types, ident, mv, scoped, cli),
        // `any` has no wasm ctor mint path (`None`, skipped loudly by the caller, like
        // Fixed/Alias): the wasm `AnyCbor` wrapper is byte-oriented with no value-destructuring
        // ctor, so a minted `any` value has no wasm-side round-trip differential — the rust-leg
        // mint covers it. Lifting follows demand.
        ConceptualRustType::Fixed(_)
        | ConceptualRustType::Alias(_, _)
        | ConceptualRustType::Any => None,
    }
}

/// Build a named generated type through its wasm wrapper API from `mv`.
fn wasm_named(
    types: &IntermediateTypes,
    ident: &RustIdent,
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
            let Some(ctor_args) = record_wasm_ctor_args(types, record, args) else {
                crate::warn!(
                    "cddl-codegen --emit-tests: no wasm build for {name} (minted constructor arguments drift from the record API)"
                );
                return None;
            };
            let mut wasm_args = Vec::new();
            for (ty, amv) in ctor_args {
                wasm_args.push(wasm_arg(types, amv, &ty, scoped, cli)?);
            }
            let call = format!("{name}::new({})", wasm_args.join(", "));
            Some(finish_fallible(
                call,
                record_wasm_ctor_can_fail(record, types),
                &name,
            ))
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
        // `@newtype`/tag wrappers now expose a wasm `new(inner)`, but as a CTOR ARG we build them via
        // the `From<cddl_lib::Native>` impl every wasm wrapper carries (see `add_conversion_methods`)
        // — a convenience: the fully-scoped rust twin is already at hand here, and every named wrapper
        // is a top-level rule whose own entry test (`wasm_wrapper_roundtrip`) exercises its `new`.
        // Named table/array wrappers have no scalar `new`, so `From` is their only build. Either way
        // the arg's boundary conversion + serialization stay covered by the enclosing byte differential.
        RustStructType::Wrapper { .. }
        | RustStructType::Table { .. }
        | RustStructType::Array { .. } => Some(format!(
            "{name}::from({})",
            emit_tests::render_rust_for_named(types, mv, ident, TypePaths::Scoped(scoped))
        )),
        // extern / raw-bytes: unreachable backstop (see the module header for why no mint arrives).
        RustStructType::Extern | RustStructType::RawBytesType => {
            crate::warn!(
                "cddl-codegen --emit-tests: no wasm build for {name} ctor arg (extern/raw-bytes — user-supplied type)"
            );
            None
        }
    }
}

/// Project a native-record mint onto the public WASM constructor ABI.  The native constructor
/// accepts every checked dynamic map carrier. An open table's bounded typed row crosses WASM as its
/// loose staging builder, then the wasm constructor re-enters the native checked carrier. Keep the
/// projection beside `wasm_named`, rather than pretending the native argument list is a WASM
/// signature, so the emitted differential follows the same constructor callers receive.
fn record_wasm_ctor_args<'a>(
    types: &IntermediateTypes,
    record: &RustRecord,
    native_args: &'a [MintValue],
) -> Option<Vec<(RustType, &'a MintValue)>> {
    let native_types = record_ctor_arg_types(record, types);
    if native_types.len() != native_args.len() {
        return None;
    }
    if let Some(slots) = multi_array_occurrence_ctor_arg_slots(record) {
        let native_by_source: BTreeMap<usize, (&RustType, &'a MintValue)> = slots
            .iter()
            .zip(native_args)
            .map(|((source_index, ty), value)| (*source_index, (ty, value)))
            .collect();
        let mut wasm = Vec::new();
        // The public wasm constructor keeps its shipped fixed-field-then-wrapper ABI. Look up
        // each value by its native source-order slot rather than treating the two ABIs as aligned.
        for field in record_ctor_fields(record) {
            let (_, value) = native_by_source.get(&field.source_index)?;
            wasm.push((field.rust_type.clone(), *value));
        }
        for row in record
            .captured_dynamic_rows()
            .filter(|row| row.is_array_tail())
        {
            let source_index = row.array_source_index()?;
            let (_, value) = native_by_source.get(&source_index)?;
            wasm.push((row.container_type(), *value));
        }
        return Some(wasm);
    }
    let mut native = native_types.iter().zip(native_args);
    let mut wasm = Vec::new();
    for field in record.fields.iter().filter(|field| {
        !field.optional
            && !field.rust_type.is_fixed_value()
            && field.rust_type.config.default.is_none()
    }) {
        let (_, value) = native.next()?;
        wasm.push((field.rust_type.clone(), value));
    }
    if let Some(typed) = record
        .typed_row()
        .filter(|_| record.is_non_empty_open_table())
    {
        let (_, key) = native.next()?;
        let (_, value) = native.next()?;
        wasm.push((typed.domain().clone(), key));
        wasm.push((typed.range().clone(), value));
    }
    for row in record.captured_dynamic_rows().filter(|row| {
        record.ctor_takes_complete_map_row(row, types)
            && !(record.is_typed_row(row) && record.is_non_empty_open_table())
    }) {
        let (_, value) = native.next()?;
        let ty = if record.is_typed_row(row)
            && row.container_type().bounded_map_u64_bounds().is_some()
        {
            // WASM receives the loose builder and re-enters Bounded{,Pair}Map::try_from itself.
            row.staging_container_type()
        } else {
            row.container_type()
        };
        wasm.push((ty, value));
    }
    for row in record
        .captured_dynamic_rows()
        .filter(|row| row.is_non_empty_array_tail())
    {
        let (_, value) = native.next()?;
        wasm.push((row.element().clone(), value));
    }
    for row in record
        .captured_dynamic_rows()
        .filter(|row| row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail())
    {
        let (_, value) = native.next()?;
        wasm.push((row.container_type(), value));
    }
    native.next().is_none().then_some(wasm)
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
    // A wrapper collection crosses the wasm boundary as `&Wrapper` (a `FooList`/`FooMap`, a named
    // list/map like `nums = [* uint]` -> `&Nums`, or a restricted `&NonEmpty<Elem>List`), so it's
    // built through the wrapper's `new`/`add` (list) or `new`/`insert` (map) API — see
    // `wasm_collection_build`. The RustType-level `directly_wasm_exposable` (bounds-aware) is what
    // distinguishes it from a plain `Vec<prim>`: a `[+ uint]` element is NOT bare-exposable (it
    // crosses as its `NonEmpty<Elem>List` wrapper) even though the conceptual array would be.
    if matches!(
        resolved,
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
    ) && !field_ty.directly_wasm_exposable(types)
    {
        let build = wasm_collection_build(types, field_ty, resolved, mv, scoped, cli)?;
        // the ctor param is `&Wrapper` for a wrapper collection (`for_wasm_param` prefixes `&`)
        return Some(if field_ty.for_wasm_param(types).starts_with('&') {
            format!("&{build}")
        } else {
            build
        });
    }
    let val = wasm_value(types, mv, resolved, scoped, cli)?;
    if field_ty.for_wasm_param(types).starts_with('&') {
        Some(format!("&{val}"))
    } else {
        Some(val)
    }
}

/// Build a wrapper collection (`FooList`/`FooMap`, or a named list/map wrapper) through its wasm
/// `new`/`add` (list) or `new`/`insert` (map) API as a block expression usable in ctor-arg position.
///
/// CRITICAL: the wrapper type NAME is taken from `unresolved` (the field's own `Alias(Rust(Nums), ..)`
/// / inline `Array(..)` conceptual type), NEVER from `resolved` — shallow-resolving past the alias
/// drops the `Nums` wrapper and would name the build `<Elem>List`, which doesn't type-check against
/// the `&Nums` ctor param (the coll__struct-field trap). `for_wasm_member` reads that name off the
/// unresolved type (the alias ident for a named list/map, `<Elem>List`/`Map<K>To<V>` for an inline
/// one) — exactly the wrapper the generator emits. `resolved` supplies the element / key+value types,
/// which are the same whichever way the field named its collection.
/// A restricted (exact, bounded, or bounded-map) wrapper builds through `From<core>` whenever its
/// element has no wasm-native vector door, so an element with no wasm build of its own (e.g. `any`)
/// does not drop the enclosing type.
fn wasm_collection_build(
    types: &IntermediateTypes,
    field_ty: &RustType,
    resolved: &ConceptualRustType,
    mv: &MintValue,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    // Bounds-aware wrapper name: `NonEmptyBarList` for `[+ bar]`, the alias/loose name otherwise.
    let wrapper = field_ty.for_wasm_member(types);
    match (resolved, mv) {
        (
            ConceptualRustType::Array(elem_ty),
            MintValue::Array {
                elem,
                count,
                reject,
                ..
            },
        ) => {
            // `@duplicates reject`: the wrapper's `add` is CHECKED (returns `Result`) and N identical
            // copies would be refused as duplicates, so the loose `new()` + `add()` build below is
            // wrong for it. Build through the `From<core>` impl every wasm wrapper carries, over the
            // reject-aware scoped native twin (`emit_tests::render_rust_for_direct_storage`): a
            // single unique element or empty set, mirroring the named-Array ctor-arg path in
            // `wasm_named`. Element exposability is irrelevant: the
            // core value already carries the right twin, and `From` is infallible.
            if *reject {
                return Some(format!(
                    "{wrapper}::from({})",
                    emit_tests::render_rust_for_direct_storage(
                        types,
                        mv,
                        field_ty,
                        TypePaths::Scoped(scoped)
                    )
                ));
            }
            // `add(elem)` takes the element via `for_wasm_param`, so reuse `wasm_arg` for the same
            // by-ref/by-value boundary the wrapper's ctor param uses.
            if field_ty.is_type_enforced_non_empty() {
                // restricted wrapper: `new(first)` seeds the first element (no empty state), `add`
                // the rest. `count` is >= 1 for a `[+ T]` shape.
                let e = elem.as_ref()?;
                let elem_expr = wasm_arg(types, e, elem_ty, scoped, cli)?;
                let binding_mut = if *count > 1 { "mut " } else { "" };
                let mut body = format!("let {binding_mut}l = {wrapper}::new({elem_expr});");
                for _ in 1..*count {
                    body.push_str(&format!(" l.add({elem_expr});"));
                }
                return Some(format!("{{ {body} l }}"));
            }
            if field_ty.is_type_enforced_exact_homogeneous_array() {
                let e = elem.as_ref()?;
                if elem_ty.vec_of_self_directly_wasm_exposable(types) {
                    let elem_expr = wasm_arg(types, e, elem_ty, scoped, cli)?;
                    return Some(format!(
                        "{wrapper}::try_from(vec![{elem_expr}; {count}]).ok().expect(\"static-array emitted-test mint\")"
                    ));
                }
                return Some(format!(
                    "{wrapper}::from({})",
                    emit_tests::render_rust_for_direct_storage(
                        types,
                        mv,
                        field_ty,
                        TypePaths::Scoped(scoped)
                    )
                ));
            }
            if field_ty.is_type_enforced_bounded_array() {
                // A bounded wrapper has no invalid empty seed when MIN > 0. For wasm-native
                // elements its `try_from(Vec<_>)` door is the direct, checked construction path.
                let e = elem.as_ref()?;
                if elem_ty.vec_of_self_directly_wasm_exposable(types) {
                    let elem_expr = wasm_arg(types, e, elem_ty, scoped, cli)?;
                    return Some(format!(
                        "{wrapper}::try_from(vec![{elem_expr}; {count}]).ok().expect(\"bounded emitted-test mint\")"
                    ));
                }
                return Some(format!(
                    "{wrapper}::from({})",
                    emit_tests::render_rust_for_direct_storage(
                        types,
                        mv,
                        field_ty,
                        TypePaths::Scoped(scoped)
                    )
                ));
            }
            let binding_mut = if elem.is_some() && *count > 0 {
                "mut "
            } else {
                ""
            };
            let mut body = format!("let {binding_mut}l = {wrapper}::new();");
            if let Some(e) = elem {
                let elem_expr = wasm_arg(types, e, elem_ty, scoped, cli)?;
                for _ in 0..*count {
                    body.push_str(&format!(" l.add({elem_expr});"));
                }
            }
            Some(format!("{{ {body} l }}"))
        }
        (
            ConceptualRustType::Map(_k, v),
            MintValue::Map {
                key,
                key_base,
                val,
                count,
                preserve,
                ..
            },
        ) => {
            if field_ty.is_type_enforced_bounded_map() {
                // Positive-minimum bounded maps intentionally have no empty wasm constructor.
                // The shared mint already entered the core through BoundedMap::TryFrom<Vec<_>>;
                // every wrapper has an infallible From<core> bridge, so this preserves that door.
                return Some(format!(
                    "{wrapper}::from({})",
                    emit_tests::render_rust_for_direct_storage(
                        types,
                        mv,
                        field_ty,
                        TypePaths::Scoped(scoped)
                    )
                ));
            }
            // cheaply-minted map keys are always primitives crossing by value (see `materialize`),
            // so synthesize each of the `count` distinct keys as a literal; `insert` takes the value
            // via `for_wasm_param`, so `wasm_arg` gives it the same boundary treatment.
            let val_expr = wasm_arg(types, val, v, scoped, cli)?;
            if field_ty.is_type_enforced_non_empty() {
                // restricted wrapper: `new(first_key, first_value)` seeds the first entry (no empty
                // state), `insert` the rest. `count` is >= 1 for a `{+ k => v}` shape.
                let binding_mut = if *count > 1 { "mut " } else { "" };
                let mut body = format!(
                    "let {binding_mut}m = {wrapper}::new({}, {val_expr});",
                    map_key_literal(key, *key_base, 0)
                );
                for i in 1..*count {
                    body.push_str(&format!(
                        " m.insert({}, {val_expr});",
                        map_key_literal(key, *key_base, i)
                    ));
                }
                return Some(format!("{{ {body} m }}"));
            }
            let binding_mut = if *count > 0 { "mut " } else { "" };
            let mut body = format!("let {binding_mut}m = {wrapper}::new();");
            for i in 0..*count {
                body.push_str(&format!(
                    " m.insert({}, {val_expr});",
                    map_key_literal(key, *key_base, if *preserve { 0 } else { i })
                ));
            }
            Some(format!("{{ {body} m }}"))
        }
        // an inline map minted empty for an unmintable value (loud-skip fallback): build it empty
        (ConceptualRustType::Map(_, _), MintValue::DefaultMap) => {
            Some(format!("{{ {wrapper}::new() }}"))
        }
        _ => None,
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
    ident: &RustIdent,
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
        crate::warn!(
            "cddl-codegen --emit-tests: no wasm round-trip for {name} (a ctor arg has no wasm build — wrapper/collection field)"
        );
        return None;
    };
    let rust_build =
        emit_tests::render_rust_for_named(types, &entry_mv, ident, TypePaths::Scoped(scoped));

    // §3 accessor read-back: primitive/c-enum ctor getters against the emit-time literal.
    // `record_wasm_ctor_args` (which `wasm_named` just accepted) yields the constructor fields
    // first, in `record_ctor_fields` order, so its leading entries pair each field with its mint.
    let ctor_args = record_wasm_ctor_args(types, record, args)?;
    let mut readbacks = Vec::new();
    for (f, (_, amv)) in record_ctor_fields(record).iter().zip(&ctor_args) {
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
    Some(wasm_roundtrip_block(
        name,
        None,
        &wasm_build,
        &rust_build,
        &readbacks,
    ))
}

fn wasm_choice_roundtrip(
    types: &IntermediateTypes,
    ident: &RustIdent,
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
        let Some(mvs) = arg_fields
            .iter()
            .map(|(ty, _)| valid_value(types, ty))
            .collect::<Option<Vec<_>>>()
        else {
            continue;
        };
        let choice_mv = MintValue::Choice {
            ident: name.to_owned(),
            variant: var.clone(),
            args: mvs,
            can_fail: arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)),
        };
        let Some(wasm_build) =
            wasm_choice_value(types, name, variants, group_choice, &choice_mv, scoped, cli)
        else {
            crate::warn!(
                "cddl-codegen --emit-tests: no wasm round-trip for {name}::new_{var} (a variant arg has no wasm build)"
            );
            continue;
        };
        let rust_build =
            emit_tests::render_rust_for_named(types, &choice_mv, ident, TypePaths::Scoped(scoped));

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
            // A nullable payload (`opt = uint / null` used as an arm) exposes a *lossy* getter:
            // wasm_bindgen can't return `Option<Option<T>>`, so `as_<var>()` returns `None` both
            // when the arm isn't selected AND when it holds `null` (the getter's own doc states this
            // `Option<Option<T>>` conflation). The minter carries the `null` inhabitant, so an
            // `is_some()` readback here is provably unsatisfiable — it asserts nothing, so emit NO
            // self-readback for a nullable arm. `kind()` + the byte round-trip still prove the right
            // variant was selected and survived the boundary; the sibling arm's `is_none()` readback
            // below stays valid (a lossy getter returns `None` when unselected regardless).
            let nullable_payload = matches!(
                &variant.data,
                EnumVariantData::RustType(vty)
                    if matches!(vty.resolve_alias_shallow(), ConceptualRustType::Optional(_))
            );
            if nullable_payload {
                // no self-readback: the getter is lossy for this arm (see above).
            } else if primitive_payload
                && let [(ty, _)] = arg_fields.as_slice()
                && let MintValue::Choice { args, .. } = &choice_mv
                && let Some(expected) = scalar_readback(ty, args.first()?)
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
        blocks.push(wasm_roundtrip_block(
            name,
            Some(&var),
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
    ident: &RustIdent,
    name: &str,
    scoped: &ScopeMap,
    cli: &Cli,
) -> Option<String> {
    let entry_mv = mint_struct(types, ident, 0)?;
    let MintValue::Wrapper { inner, .. } = &entry_mv else {
        return None;
    };
    let rust_build =
        emit_tests::render_rust_for_named(types, &entry_mv, ident, TypePaths::Scoped(scoped));
    // The wrapped inner type — drives the inner wasm expression through the wrapper's public `new`.
    let RustStructType::Wrapper { wrapped, .. } = types.rust_struct(ident)?.variant() else {
        return None;
    };

    // Build the inner value through the SAME ctor-arg machinery the wrapper's `new(inner)` param uses
    // (`wasm_arg` applies the by-ref/`&` boundary of `for_wasm_param`). When the inner has no faithful
    // wasm build, fall back to decoding the rust twin's bytes with a loud skip of the ctor
    // differential — the wire round-trip still runs. Reachability: see the module header.
    let Some(inner_expr) = wasm_arg(types, inner, wrapped, scoped, cli) else {
        crate::warn!(
            "cddl-codegen --emit-tests: no wasm ctor build for {name} (inner has no wasm ctor expression); building via from_cbor_bytes, ctor differential skipped"
        );
        return Some(format!(
            "    {{
        let rust_v = {rust_build};
        let bytes = rust_v.to_cbor_bytes();
        let wasm_v = {name}::from_cbor_bytes(&bytes).ok().expect(\"{name}::from_cbor_bytes\");
        assert_eq!(wasm_v.to_cbor_bytes(), bytes, \"{name}: wasm wire round-trip must be byte-identical\");
    }}"
        ));
    };

    // Build through the public wasm `new`. A bounded/range wrapper's `new` returns `Result<_, JsError>`;
    // the minted inner is in-window by construction, so `.ok().expect(..)` it (JsError: !Debug — never
    // `.unwrap()`). The REJECT direction stays rust-side (its JsError error path panics under host tests).
    let ctor = format!("{name}::new({inner_expr})");
    let wasm_build = finish_fallible(ctor, types.can_new_fail(ident), name);

    // §3 getter read-back: a primitive inner is compared against its emit-time literal on the freshly
    // BUILT value (a broken getter conversion still fails here); non-primitive inners skip the literal
    // compare (same policy as struct accessor read-back — the byte differential + wire cover them).
    let getter = wrapper_getter_name(types, ident);
    let mut readbacks = Vec::new();
    if let Some(expected) = scalar_readback(wrapped, inner) {
        readbacks.push(format!(
            "        assert_eq!(wasm_v.{getter}(), {expected}, \"{name}.{getter}() must read back the minted inner value\");"
        ));
    }
    Some(wasm_roundtrip_block(
        name,
        None,
        &wasm_build,
        &rust_build,
        &readbacks,
    ))
}

/// The effective inner-value getter name for a wrapper: an explicit `@newtype <name>` renames it,
/// otherwise every wrapper (bare tag, plain `@newtype`, bounded/range) exposes the inner under `get`
/// — the same resolution `generate_wrapper_struct` uses to emit the getter.
fn wrapper_getter_name(types: &IntermediateTypes, ident: &RustIdent) -> String {
    match types
        .rust_struct(ident)
        .and_then(|s| s.config().newtype_getter.as_ref())
    {
        Some(Some(name)) => name.clone(),
        _ => "get".to_owned(),
    }
}

/// One wasm round-trip block: §1 differential, §2 wire, §3 read-backs. `variant` is `None` for a
/// single-value (record/wrapper) test and the variant's var-name for a choice's per-variant case,
/// which qualifies the assertion labels.
fn wasm_roundtrip_block(
    name: &str,
    variant: Option<&str>,
    wasm_build: &str,
    rust_build: &str,
    readbacks: &[String],
) -> String {
    let rb = if readbacks.is_empty() {
        String::new()
    } else {
        format!("\n{}", readbacks.join("\n"))
    };
    let (subject, conversion, decode_suffix) = match variant {
        None => (name.to_owned(), "ctor".to_owned(), String::new()),
        Some(var) => (
            format!("{name}::{var}"),
            format!("new_{var}"),
            format!(" ({var})"),
        ),
    };
    format!(
        "    {{
        let wasm_v = {wasm_build};
        let rust_v = {rust_build};
        let bytes = wasm_v.to_cbor_bytes();
        assert_eq!(bytes, rust_v.to_cbor_bytes(), \"{subject}: wasm-built and rust-built bytes must match ({conversion} conversion)\");
        let back = {name}::from_cbor_bytes(&bytes).ok().expect(\"{name}::from_cbor_bytes{decode_suffix}\");
        assert_eq!(back.to_cbor_bytes(), bytes, \"{subject}: wasm wire round-trip must be byte-identical\");{rb}
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
    let native_types = record_ctor_arg_types(record, types);
    let native_values: Vec<MintValue> = native_types
        .iter()
        .map(|ty| valid_value(types, ty))
        .collect::<Option<_>>()?;
    // The public wasm ABI can differ from native source order for multi-exact records. Reuse the
    // identity-preserving projection used by round-trip mints before choosing an index to mutate.
    let wasm_ctor_args = record_wasm_ctor_args(types, record, &native_values)?;
    let baseline: Vec<String> = wasm_ctor_args
        .iter()
        .map(|(ty, value)| wasm_arg(types, value, ty, scoped, cli))
        .collect::<Option<_>>()?;
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
            let Some(baseline) = arg_fields
                .iter()
                .map(|(ty, _)| {
                    valid_value(types, ty).and_then(|m| wasm_arg(types, &m, ty, scoped, cli))
                })
                .collect::<Option<Vec<_>>>()
            else {
                continue;
            };
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
    bounds: IntWindow,
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
