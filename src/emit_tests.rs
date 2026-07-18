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
//! from_cbor_bytes → to_cbor_bytes == bytes`) AND — outside preserve-encodings — that the
//! deserialized value `Debug`-equals the minted original (byte-identity alone is blind to
//! projection miscompiles: a serializer that loses information idempotently is a fixed point of
//! the wire cycle; see `roundtrip_body`). Cases per shape: a valid baseline; each optional
//! field additionally present (records); one mandatory nullable (`T / null`) field additionally
//! set to `Some(inner)` (records — so the present-value wire path runs, not just the `None`
//! baseline); one case per choice/c-enum variant. This is the
//! "output is right, not just unchanged" oracle (tests/README.md § "Generated-test harness"): a
//! serialize/deserialize
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
//! Deliberately scoped to the cheap cases:
//! valid values are minted from compile-time literals, so any field that can't be cheaply
//! minted (nested rust structs/tags, bounded `nint`s — whose stored/wire direction is inverted)
//! causes that one type/case to be skipped with an `eprintln!`.

use crate::cli::Cli;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, IntermediateTypes, Primitive, RustField,
    RustIdent, RustRecord, RustStruct, RustStructType, RustType,
};
use crate::utils::convert_to_snake_case;

type Bounds = (Option<i128>, Option<i128>);

// ============================================================================================
// The MINT-VALUE data layer. Value derivation (`valid_value`/`materialize`/`mint_struct` and the
// `bound_cases` boundary triples) produces this abstract tree; a renderer turns it into source.
// `render_rust` reproduces the rust-crate API strings byte-for-byte (the ONLY renderer today).
// A second renderer (`emit_tests_wasm::render_wasm`, off in this module) targets the wasm wrapper
// API from the SAME tree, so a single derivation surface feeds both crates' emitted tests. Kept
// only as abstract as those two renderers need — deliberately NOT a general codegen IR.
// ============================================================================================

/// The synthesized-key kind for a minted map (distinct keys `0..count`).
#[derive(Clone)]
pub(crate) enum MapKey {
    /// integer key cast to the map's key primitive: `__i as <prim>`
    Int(Primitive),
    /// text key: `__i.to_string()`
    Str,
    /// byte-string key: `vec![__i as u8]` (one distinct byte per index)
    Bytes,
    /// bool key (only for count <= 2): `__i == 1`
    Bool,
}

/// An abstract minted value. Each variant maps to exactly one rust-source shape (`render_rust`) and,
/// under the wasm renderer, to the corresponding wrapper-API construction.
#[derive(Clone)]
pub(crate) enum MintValue {
    /// `None` (an `Optional` field's degenerate baseline)
    None,
    /// `false` (bool baseline / measured bool)
    Bool,
    /// `0.0` (unbounded-float baseline)
    Float,
    /// an in-window (or boundary/NaN) float literal for a bounded float type. `is_f32` selects the
    /// typed constant needed for NaN / suffix (an f32 ctor param can't take `f64::NAN`).
    FloatLit { value: f64, is_f32: bool },
    /// an integer literal; `prim` is the backing rust primitive (load-bearing for the wasm renderer,
    /// unused by `render_rust`). Covers unsigned/signed ints and `N64` (nint magnitude).
    Int {
        value: i128,
        // read by the wasm renderer (emit_tests_wasm); `render_rust` needs only `value`
        #[allow(dead_code)]
        prim: Primitive,
    },
    /// a text string of the given length: `"a".repeat(len)`
    Str { len: i128 },
    /// a fixed text literal: `"content".to_owned()`. Used to mint a semantically-VALID inner value
    /// for prelude tag wrappers whose tag number carries RFC 8949 content requirements the reference
    /// validator enforces (e.g. tag 0 = tdate must be an RFC 3339 date-time). The generic
    /// `Str { len }` baseline (`"a"`) is spec-violating for those tags, so it would round-trip
    /// byte-identically here yet be rejected by the conformance oracle. Renders identically in both
    /// renderers (a `String` on the wire, so it still round-trips).
    StrLit { content: String },
    /// a byte string of the given length: `vec![0u8; len]`
    Bytes { len: i128 },
    /// a vec of `count` copies of `elem`, or the empty vec when `elem` is `None`. When `non_empty`
    /// the target type is `NonEmptyVec<T>` (`[+ T]`), so it is built through the single TryFrom door
    /// (`NonEmptyVec::try_from(vec![..]).unwrap()`).
    Array {
        elem: Option<Box<MintValue>>,
        count: i128,
        non_empty: bool,
    },
    /// a map of `count` entries with synthesized keys. When `non_empty` the target type is
    /// `NonEmptyMap<K, V>` (`{+ k => v}`), so it is built through the single TryFrom door
    /// (`NonEmptyMap::try_from(map).unwrap()`, the collect target inferred from the sole impl).
    Map {
        key: MapKey,
        val: Box<MintValue>,
        count: i128,
        non_empty: bool,
    },
    /// `Default::default()` — an empty inline-map field minted for an unmintable element (loud skip)
    DefaultMap,
    /// a named record: `Ident::new(args)` (+ `.unwrap()` when `can_fail`)
    Record {
        ident: String,
        args: Vec<MintValue>,
        can_fail: bool,
    },
    /// a named `@newtype`/tag wrapper: `Ident::new(inner)` (+ `.unwrap()`)
    Wrapper {
        ident: String,
        inner: Box<MintValue>,
        can_fail: bool,
    },
    /// a c-style enum variant: `Ident::Variant`
    CEnum { ident: String, variant: String },
    /// a type/group choice: `Ident::new_<variant>(args)` (+ `.unwrap()`)
    Choice {
        ident: String,
        variant: String,
        args: Vec<MintValue>,
        can_fail: bool,
    },
    /// a named table minted empty: `Ident::new()`
    TableEmpty { ident: String },
    /// the reserved `Int` prelude extern (a bare CDDL `int`): rust `Ident::new_uint(value)` for the
    /// non-negative baseline. Its wasm twin exposes a single `Ident::new(value as i64)` ctor, so the
    /// wasm renderer keys off this variant rather than the generic wrapper/record shapes.
    IntExtern { ident: String, value: i128 },
}

/// Render a `MintValue` as the rust-crate API expression string. This reproduces, byte-for-byte,
/// the output the fused derive-and-format code produced before the derivation/render split.
pub(crate) fn render_rust(mv: &MintValue) -> String {
    let unwrap = |can_fail: bool| if can_fail { ".unwrap()" } else { "" };
    match mv {
        MintValue::None => "None".to_owned(),
        MintValue::Bool => "false".to_owned(),
        MintValue::Float => "0.0".to_owned(),
        MintValue::FloatLit { value, is_f32 } => render_float_lit(*value, *is_f32),
        MintValue::Int { value, .. } => format!("{value}"),
        MintValue::Str { len } => format!("\"a\".repeat({len})"),
        MintValue::StrLit { content } => format!("\"{content}\".to_owned()"),
        MintValue::Bytes { len } => format!("vec![0u8; {len}]"),
        MintValue::Array {
            elem: Some(e),
            count,
            non_empty,
        } => {
            let vec = format!("vec![{}; {count}]", render_rust(e));
            if *non_empty {
                // route through the single TryFrom door (same as every other construction path)
                format!("NonEmptyVec::try_from({vec}).unwrap()")
            } else {
                vec
            }
        }
        MintValue::Array { elem: None, .. } => "vec![]".to_owned(),
        MintValue::Map {
            key,
            val,
            count,
            non_empty,
        } => {
            let k = match key {
                MapKey::Int(p) => format!("__i as {p}"),
                MapKey::Str => "__i.to_string()".to_owned(),
                MapKey::Bytes => "vec![__i as u8]".to_owned(),
                MapKey::Bool => "__i == 1".to_owned(),
            };
            let v = render_rust(val);
            if *non_empty {
                // build via `new(first_key, first_value)` + `insert` (flavor-agnostic and
                // unambiguous). A bare `try_from((..).collect())` can't infer the collect target here:
                // the reflexive `TryFrom<Self>` blanket competes with `TryFrom<{table_type}>`, so the
                // `{table_type}` (BTreeMap / OrderedHashMap) is not uniquely determined. `new` never
                // names the inner map type, so it compiles under every profile.
                format!(
                    "{{ let mut __m = {{ let __i = 0u64; NonEmptyMap::new({k}, {v}) }}; for __i in 1u64..{count} {{ __m.insert({k}, {v}); }} __m }}"
                )
            } else {
                format!("(0u64..{count}).map(|__i| ({k}, {v})).collect()")
            }
        }
        MintValue::DefaultMap => "Default::default()".to_owned(),
        MintValue::Record {
            ident,
            args,
            can_fail,
        } => {
            let a: Vec<String> = args.iter().map(render_rust).collect();
            format!("{ident}::new({}){}", a.join(", "), unwrap(*can_fail))
        }
        MintValue::Wrapper {
            ident,
            inner,
            can_fail,
        } => format!("{ident}::new({}){}", render_rust(inner), unwrap(*can_fail)),
        MintValue::CEnum { ident, variant } => format!("{ident}::{variant}"),
        MintValue::Choice {
            ident,
            variant,
            args,
            can_fail,
        } => {
            let a: Vec<String> = args.iter().map(render_rust).collect();
            format!(
                "{ident}::new_{variant}({}){}",
                a.join(", "),
                unwrap(*can_fail)
            )
        }
        MintValue::TableEmpty { ident } => format!("{ident}::new()"),
        MintValue::IntExtern { ident, value } => format!("{ident}::new_uint({value})"),
    }
}

/// Emit the `#[cfg(test)]` generated-test module (reject + round-trip halves), or `None` if
/// nothing at all could be minted.
///
/// `submodules` — the crate's declared non-root module paths (multifile output; see the call site
/// in `generation/mod.rs` for the derivation). The module this emits lands at the generated root while
/// its minted values name submodule types bare, so each entry contributes a `use super::<m>::*;`
/// glob; empty (single-file output) emits nothing extra, keeping that output byte-identical.
pub fn emit_generated_tests(
    types: &IntermediateTypes,
    cli: &Cli,
    submodules: &[String],
) -> Option<String> {
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
            RustStructType::Record(record) => {
                record_deser_reject(types, &name, record, !cli.preserve_encodings)
            }
            RustStructType::TypeChoice { variants } => {
                choice_construct_reject(types, &name, variants, false)
            }
            RustStructType::GroupChoice { variants, .. } => {
                choice_construct_reject(types, &name, variants, true)
            }
            RustStructType::Wrapper {
                wrapped,
                min_max,
                float_min_max,
            } => match float_min_max {
                Some(window) => wrapper_construct_reject_float(&name, wrapped, window),
                None => min_max.and_then(|mm| wrapper_construct_reject(types, &name, wrapped, mm)),
            },
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

        // The source rule name for this type (None when it isn't a top-level rule or its name can't
        // be faithfully snake↔camel reversed — see `conformance_rule_name`, which is loud about the
        // latter). Computed unconditionally: it names both the flag-gated conformance validate line
        // AND the env-gated minted-bytes dump (which needs no CLI flag — see `roundtrip_body`), so it
        // must be available even without `--emit-tests-conformance`.
        let rule_name = conformance_rule_name(types, ident);
        let rule_name = rule_name.as_deref();
        // `--emit-tests-conformance`: only under the flag does the emitted round-trip call the cddl
        // validator on the minted bytes.
        let conf = cli.emit_tests_conformance.then_some(rule_name).flatten();

        // value-equality is meaningless under preserve-encodings: `back` carries encoding structs
        // populated from the wire while the minted value has ctor defaults (see roundtrip_body).
        // The encoding-fidelity oracle asserts the GENERATED preserve contract, so it must not run
        // on a type whose wire format is (partly) user-supplied via `@custom_serialize` /
        // `@custom_deserialize`: a hand-written custom deserializer that rejects a valid irregular
        // encoding is the user's choice, not a generator bug. Round-trip/value-eq assertions still
        // run (the custom baseline round-trips); only the mutated-variant assertions are gated off.
        let uses_custom = struct_uses_custom_ser(types, rust_struct);
        let rt = RtEmit {
            value_eq: !cli.preserve_encodings,
            preserve: cli.preserve_encodings && !uses_custom,
            canonical: cli.canonical_form && !uses_custom,
        };
        let roundtrip = match rust_struct.variant() {
            RustStructType::Record(record) => {
                record_roundtrip(types, &name, record, conf, rule_name, rt)
            }
            RustStructType::TypeChoice { variants } => {
                choice_roundtrip(types, &name, variants, false, conf, rule_name, rt)
            }
            RustStructType::GroupChoice { variants, .. } => {
                choice_roundtrip(types, &name, variants, true, conf, rule_name, rt)
            }
            RustStructType::Wrapper {
                wrapped,
                min_max,
                float_min_max,
            } => wrapper_roundtrip(
                types,
                &name,
                wrapped,
                *min_max,
                *float_min_max,
                conf,
                rule_name,
                rt,
            ),
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
    // `--preserve-encodings`: the self-contained CBOR mutator whose `variants()` the round-trip loop
    // calls (spliced verbatim, no external append needed, so it works at corpus breadth). Only
    // meaningful under preserve — the encoding-fidelity assertions the loop emits are keyed on the
    // same flag. See the file header for the mutation classes and self-check.
    let fidelity_mod = if cli.preserve_encodings {
        include_str!("../static/emit_tests_encoding_fidelity.rs")
    } else {
        ""
    };
    // `to_cbor_bytes`/`from_cbor_bytes` are trait methods (serialization::{ToCBORBytes,
    // Deserialize}); `use super::*` alone doesn't bring traits into scope in a standalone crate
    // (the integration harness happens to append `use serialization::*;` to lib.rs, which masked
    // this), so import the serialization surface explicitly.
    //
    // Multifile output: glob-import each declared non-root module — the minted values name
    // submodule types bare, and `use super::*;` only reaches root-scope items (E0433 otherwise).
    // Guarded on non-empty so single-file output stays byte-identical; the extra
    // `#[allow(unused_imports)]` (a submodule can legitimately contribute no referenced name —
    // e.g. its only type minted no standalone test) is injected under the same guard. E0659
    // glob-collision caveat + the fully-qualified long-term alternative: see the call site.
    let scope_globs: String = submodules
        .iter()
        .map(|path| format!("    use super::{path}::*;\n"))
        .collect();
    let unused_imports_allow = if scope_globs.is_empty() {
        ""
    } else {
        "#[allow(unused_imports)]\n"
    };
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\n{unused_imports_allow}mod cddl_generated_tests {{\n    use super::*;\n    use super::serialization::*;\n{scope_globs}{conformance_mod}{fidelity_mod}{}\n}}\n",
        fns.join("\n")
    ))
}

/// The source CDDL rule name naming `ident`'s minted bytes for the conformance oracles, or `None` if
/// this type can't be soundly rooted. Only a top-level rule qualifies (a synthesized struct —
/// embedded record, inline group — has no spec rule). The name is the EXACT source spelling recorded
/// at rule registration (`IntermediateTypes::source_rule_name`), not a reversal of the camel-cased
/// `RustIdent`: CDDL treats `-` and `_` as distinct rule characters (`my-rule` ≠ `my_rule`) but
/// `RustIdent` camel-cases both to `MyRule`, so any snake↔camel guess would silently point the
/// validator/dump at a NONEXISTENT rule. A rule whose source name can't be recovered is excluded
/// loudly rather than mis-rooted.
///
/// Two consumers depend on this: the flag-gated `--emit-tests-conformance` validate call, and the
/// always-on env-gated minted-bytes dump (`CDDL_CODEGEN_DUMP_MINTED`) the decorrelated ruby sweep
/// reads — so a `None` here drops the type from BOTH oracles for this fixture.
fn conformance_rule_name(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
) -> Option<String> {
    if !types.is_toplevel_rule(ident) {
        return None;
    }
    if types.is_plain_group(ident) {
        // A top-level GROUP rule (`g = (a: uint, b: uint)`) is a reusable group fragment, not a
        // rootable instance type, even if embed-site resolution has registered it as an
        // array/map-serialized Rust struct. This cannot exclude ordinary array/map TYPE rules:
        // `types.is_plain_group` is set only by `Rule::Group` registration, while `t = [...]` /
        // `t = {...}` rules are registered as normal Rust structs and still reach the oracles.
        return None;
    }
    match types.source_rule_name(ident) {
        Some(name) => Some(name.to_owned()),
        None => {
            // A top-level rule with no recorded source name shouldn't happen (registration records
            // every rule), but if it ever does, exclude loudly rather than mis-root the oracles.
            eprintln!(
                "cddl-codegen --emit-tests: cannot recover the source CDDL rule name for {ident} \
                 — excluding it from the conformance validate (--emit-tests-conformance) AND the \
                 decorrelated minted-bytes dump/ruby sweep for this fixture"
            );
            None
        }
    }
}

/// True if any part of this type's wire format is user-supplied (`@custom_serialize` /
/// `@custom_deserialize`) — at the type level, or on any field / choice variant / inner type
/// reachable through the type's fields (including through named-struct references). Used to gate
/// OFF the encoding-fidelity oracle, whose contract is the *generated* serializer's (a picky
/// hand-written custom deserializer legitimately rejects valid irregular encodings, so a container
/// embedding such a struct must be excluded too). Bounded and cycle-safe: a `visited` set of the
/// named structs already entered stops recursion at the first repeat, so a recursive type
/// terminates.
fn struct_uses_custom_ser(types: &IntermediateTypes, rust_struct: &RustStruct) -> bool {
    let mut visited = std::collections::BTreeSet::new();
    struct_uses_custom_ser_inner(types, rust_struct, &mut visited)
}

fn struct_uses_custom_ser_inner(
    types: &IntermediateTypes,
    rust_struct: &RustStruct,
    visited: &mut std::collections::BTreeSet<RustIdent>,
) -> bool {
    let cfg = rust_struct.config();
    if cfg.custom_serialize.is_some() || cfg.custom_deserialize.is_some() {
        return true;
    }
    match rust_struct.variant() {
        RustStructType::Record(record) => record
            .fields
            .iter()
            .any(|f| field_uses_custom_ser(types, f, visited)),
        RustStructType::Wrapper { wrapped, .. } => type_uses_custom_ser(types, wrapped, visited),
        RustStructType::Table { domain, range, .. } => {
            type_uses_custom_ser(types, domain, visited)
                || type_uses_custom_ser(types, range, visited)
        }
        RustStructType::Array { element_type, .. } => {
            type_uses_custom_ser(types, element_type, visited)
        }
        RustStructType::TypeChoice { variants } | RustStructType::GroupChoice { variants, .. } => {
            variants.iter().any(|v| match &v.data {
                EnumVariantData::RustType(ty) => type_uses_custom_ser(types, ty, visited),
                EnumVariantData::Inlined(record) => record
                    .fields
                    .iter()
                    .any(|f| field_uses_custom_ser(types, f, visited)),
            })
        }
        _ => false,
    }
}

fn field_uses_custom_ser(
    types: &IntermediateTypes,
    field: &RustField,
    visited: &mut std::collections::BTreeSet<RustIdent>,
) -> bool {
    field.rule_metadata.custom_serialize.is_some()
        || field.rule_metadata.custom_deserialize.is_some()
        || type_uses_custom_ser(types, &field.rust_type, visited)
}

fn type_uses_custom_ser(
    types: &IntermediateTypes,
    ty: &RustType,
    visited: &mut std::collections::BTreeSet<RustIdent>,
) -> bool {
    fn walk(
        types: &IntermediateTypes,
        ct: &ConceptualRustType,
        visited: &mut std::collections::BTreeSet<RustIdent>,
    ) -> bool {
        match ct {
            ConceptualRustType::Alias(ident, inner) => {
                types
                    .type_aliases()
                    .get(ident)
                    .and_then(|a| a.rule_metadata.as_ref())
                    .map(|m| m.custom_serialize.is_some() || m.custom_deserialize.is_some())
                    .unwrap_or(false)
                    || walk(types, inner, visited)
            }
            ConceptualRustType::Optional(t) | ConceptualRustType::Array(t) => {
                walk(types, &t.conceptual_type, visited)
            }
            ConceptualRustType::Map(k, v) => {
                walk(types, &k.conceptual_type, visited) || walk(types, &v.conceptual_type, visited)
            }
            // a referenced named struct: recurse into its fields too (a field-level
            // `@custom_serialize` on an embedded struct's field is still on THIS type's wire), guard
            // against cycles with `visited`.
            ConceptualRustType::Rust(ident) => {
                if !visited.insert(ident.clone()) {
                    return false;
                }
                types
                    .rust_struct(ident)
                    .map(|s| struct_uses_custom_ser_inner(types, s, visited))
                    .unwrap_or(false)
            }
            ConceptualRustType::Fixed(_) | ConceptualRustType::Primitive(_) => false,
        }
    }
    walk(types, &ty.conceptual_type, visited)
}

// ============================================================================================
// ROUND-TRIP half. Each fn returns the body of one `roundtrip_<type>` test: a set of IR-derived
// value cases, each pushed through the full wire cycle and asserted byte-identical.
// ============================================================================================

/// Per-case emission flags, bundled. See `roundtrip_body` for what each one emits.
#[derive(Clone, Copy)]
struct RtEmit {
    value_eq: bool,
    preserve: bool,
    canonical: bool,
}

/// The shared wire-cycle emission for a list of `(value_expr, label)` cases. `conf` is the source
/// CDDL rule name for the `--emit-tests-conformance` oracle (`None` when off): when set, each case
/// validates its minted `bytes` against the spec right after computing them.
///
/// `value_eq` additionally asserts the deserialized VALUE equals the minted original (via derived
/// `Debug`, since generated types don't derive `PartialEq`). Byte-identity alone can't see an
/// information-losing serializer that is a projection (e.g. one that writes a constant: the wrong
/// bytes deserialize to the wrong value, which re-serializes to the same wrong bytes — a fixed
/// point; mutation-verified against exactly that). Off under `--preserve-encodings`: `back`'s
/// encoding fields are populated from the wire while the minted value carries ctor defaults, so
/// their `Debug`s legitimately differ even when the wire cycle is perfect.
///
/// `preserve` appends the encoding-fidelity block: each of the mutator's irregular re-encodings of
/// the canonical `bytes` must decode and (preserve) re-encode byte-identically. `canonical`
/// additionally hoists a per-case canonical baseline, asserts it's a fixed point, and asserts every
/// variant canonicalizes to it (the encoding-invariance differential). See
/// `static/emit_tests_encoding_fidelity.rs`.
fn roundtrip_body(
    name: &str,
    cases: Vec<(String, String)>,
    conf: Option<&str>,
    dump_rule: Option<&str>,
    rt: RtEmit,
) -> Option<String> {
    if cases.is_empty() {
        return None;
    }
    let RtEmit {
        value_eq,
        preserve,
        canonical,
    } = rt;
    let conf_line = conf
        .map(|rule| format!("        cddl_conformance::validate(&bytes, \"{rule}\");\n"))
        .unwrap_or_default();
    let blocks: Vec<String> = cases
        .into_iter()
        .enumerate()
        .map(|(case_idx, (expr, label))| {
            // Minted-bytes dump hook (decorrelated conformance oracle): when the env var
            // CDDL_CODEGEN_DUMP_MINTED points at a directory, write each freshly minted case's bytes
            // to `<dir>/<source_rule>__case<i>.cbor` before the assertions, so a harness-side
            // reference validator (the ruby `cddl` gem, in the `ir_conformance_corpus` gate) can
            // re-check them through a lineage-decorrelated parser. Pure std, inert when the var is
            // unset, needs no CLI flag. Only emitted when the type has a faithful source-rule name
            // (`dump_rule`); a lossy name is skipped loudly by `conformance_rule_name`.
            //
            // The hook is deliberately NON-FATAL: this same code ships in EVERY `--emit-tests` crate,
            // so a leaked/misdirected env var must NOT turn a user's green suite into a wall of
            // panics. It best-effort creates the dir and, on any write failure, logs to stderr and
            // continues — the test never fails on the dump. The harness keeps its teeth elsewhere
            // (the sweep's per-fixture case floor + negative control detect a dump that silently
            // stopped firing), so a swallowed write can't quietly weaken the oracle.
            let dump_line = dump_rule
                .map(|rule| format!(
                    "        if let Ok(__dump_dir) = std::env::var(\"CDDL_CODEGEN_DUMP_MINTED\") {{\n            let _ = std::fs::create_dir_all(&__dump_dir);\n            let __dump_path = format!(\"{{__dump_dir}}/{rule}__case{case_idx}.cbor\");\n            if let Err(__e) = std::fs::write(&__dump_path, &bytes) {{\n                eprintln!(\"cddl-codegen: could not dump minted bytes to {{__dump_path}}: {{__e}}\");\n            }}\n        }}\n"
                ))
                .unwrap_or_default();
            let value_eq_line = if value_eq {
                format!("\n        assert_eq!(format!(\"{{:?}}\", back), format!(\"{{:?}}\", v), \"{name} ({label}): deserialized value must equal the minted original\");")
            } else {
                String::new()
            };
            // encoding-fidelity: only under --preserve-encodings (the assertion is meaningless
            // otherwise — non-preserve serializers normalize on re-encode).
            let fidelity = if preserve {
                // canonical: the differential (all encodings canonicalize identically) + a
                // per-case fixed point. Under plain preserve, only the byte-identity assertion.
                let (canon_hoist, canon_assert) = if canonical {
                    (
                        format!(
                            "\n        let canonical_baseline = v.to_canonical_cbor_bytes();\n        assert_eq!({name}::from_cbor_bytes(&canonical_baseline).unwrap().to_canonical_cbor_bytes(), canonical_baseline, \"{name} ({label}): canonical bytes must be a fixed point\");"
                        ),
                        format!(
                            "\n            assert_eq!(back.to_canonical_cbor_bytes(), canonical_baseline, \"{name} ({label})/{{mut_label}}: canonicalization must be encoding-invariant\");"
                        ),
                    )
                } else {
                    (String::new(), String::new())
                };
                format!(
                    "{canon_hoist}
        for (mut_label, mutated) in cddl_encoding_fidelity::variants(&bytes) {{
            let back = {name}::from_cbor_bytes(&mutated).unwrap_or_else(|e| panic!(\"{name} ({label})/{{mut_label}}: irregular encoding must deserialize: {{e:?}}\"));
            assert_eq!(back.to_cbor_bytes(), mutated, \"{name} ({label})/{{mut_label}}: preserve-encodings must re-encode irregular input byte-identically\");{canon_assert}
        }}"
                )
            } else {
                String::new()
            };
            format!(
                "    {{
        let v = {expr};
        let bytes = v.to_cbor_bytes();
{dump_line}{conf_line}        let back = {name}::from_cbor_bytes(&bytes).expect(\"{name} ({label}): serialized bytes must deserialize\");{value_eq_line}
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name} ({label}): wire round-trip must be byte-identical\");{fidelity}
    }}"
            )
        })
        .collect();
    Some(blocks.join("\n"))
}

/// Mirrors the record constructor's fallibility rule (`generation/records.rs` `new_can_fail`): `new()`
/// returns `Result` iff any non-optional field is bounded.
pub(crate) fn record_ctor_can_fail(record: &RustRecord) -> bool {
    record
        .fields
        .iter()
        .any(|f| !f.optional && f.rust_type.has_value_bounds())
}

/// Record round-trip: a valid baseline, plus one case per optional field with that field present.
fn record_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
    conf: Option<&str>,
    dump_rule: Option<&str>,
    rt: RtEmit,
) -> Option<String> {
    let ctor_fields: Vec<&RustField> = record
        .fields
        .iter()
        .filter(|f| {
            !f.optional && !f.rust_type.is_fixed_value() && f.rust_type.config.default.is_none()
        })
        .collect();
    let mut valid_args: Vec<MintValue> = Vec::new();
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
    let base = render_rust(&MintValue::Record {
        ident: name.to_owned(),
        args: valid_args,
        can_fail: record_ctor_can_fail(record),
    });
    let mut cases = vec![(base.clone(), "baseline".to_owned())];
    for f in record.fields.iter().filter(|f| f.optional) {
        // An optional fixed value (any kind, including float) is stored as a `bool` presence field,
        // not `Option<T>`: the present case just flips it true so the round-trip exercises writing
        // (and verifying) the constant on the wire.
        if f.rust_type.is_fixed_value() {
            cases.push((
                format!("{{ let mut v = {base}; v.{} = true; v }}", f.name),
                format!("optional `{}` present", f.name),
            ));
            continue;
        }
        match valid_value(types, &f.rust_type) {
            Some(x) => {
                let x = render_rust(&x);
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
                format!(
                    "{{ let mut v = {base}; v.{} = Some({}); v }}",
                    f.name,
                    render_rust(&x)
                ),
                format!("nullable `{}` present", f.name),
            ));
            break;
        }
    }
    roundtrip_body(name, cases, conf, dump_rule, rt)
}

/// Choice round-trip: one wire cycle per constructible variant (the construct-reject half never
/// serializes, so this is the variants' first trip through the actual encode/decode path).
fn choice_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    group_choice: bool,
    conf: Option<&str>,
    dump_rule: Option<&str>,
    rt: RtEmit,
) -> Option<String> {
    let mut cases = Vec::new();
    for variant in variants {
        let ctor = format!("new_{}", variant.name_as_var());
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            eprintln!(
                "cddl-codegen --emit-tests: {name}::{ctor} not cheaply constructible — no round-trip case"
            );
            continue;
        };
        let mut args: Vec<MintValue> = Vec::new();
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
        cases.push((
            render_rust(&MintValue::Choice {
                ident: name.to_owned(),
                variant: variant.name_as_var(),
                args,
                can_fail: arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)),
            }),
            format!("variant {}", variant.name),
        ));
    }
    roundtrip_body(name, cases, conf, dump_rule, rt)
}

/// Wrapper round-trip: one wire cycle with a valid inner value (bounds-respecting when `min_max`
/// is present — the wrapper checks the raw measure, no nint transform).
#[allow(clippy::too_many_arguments)]
fn wrapper_roundtrip(
    types: &IntermediateTypes,
    name: &str,
    wrapped: &RustType,
    min_max: Option<Bounds>,
    float_min_max: Option<crate::intermediate::FloatWindow>,
    conf: Option<&str>,
    dump_rule: Option<&str>,
    rt: RtEmit,
) -> Option<String> {
    // Tag-aware, same as mint_struct's Wrapper arm: a semantically-enforced tag (e.g. tdate) gets a
    // valid literal so this standalone round-trip stays consistent with the aggregate-record mint.
    let inner = match semantic_tag_content(wrapped) {
        Some(content) => Some(MintValue::StrLit {
            content: content.to_owned(),
        }),
        None => match float_min_max {
            Some(window) => Some(MintValue::FloatLit {
                value: valid_float_in_window(&window),
                is_f32: float_is_f32(wrapped),
            }),
            None => match min_max {
                Some(mm) => materialize(types, wrapped, wrapper_measure(wrapped, mm)),
                None => valid_value(types, wrapped),
            },
        },
    };
    let Some(inner) = inner else {
        eprintln!(
            "cddl-codegen --emit-tests: no round-trip for {name} (inner value not cheaply mintable)"
        );
        return None;
    };
    // mirrors the wrapper ctor's fallibility rule: `new()` returns Result iff a window check exists
    let base = render_rust(&MintValue::Wrapper {
        ident: name.to_owned(),
        inner: Box::new(inner),
        can_fail: min_max.is_some() || float_min_max.is_some(),
    });
    roundtrip_body(
        name,
        vec![(base, "baseline".to_owned())],
        conf,
        dump_rule,
        rt,
    )
}

/// Mirrors a choice-variant ctor's per-arg fallibility (`generation/enums.rs` per-variant `can_fail`):
/// an arg makes `new_<variant>` return `Result` only when it needs an inlined bounds check AND a
/// check expression exists for its shape (a bounded named wrapper like `Hash` checks at ITS OWN
/// construction, so passing one in stays infallible).
pub(crate) fn arg_can_fail(types: &IntermediateTypes, ty: &RustType) -> bool {
    ty.needs_bounds_check_if_inlined(types)
        && crate::generation::bounds_check_expr_rust_type(ty, "x").is_some()
}

/// deser-reject for a struct: for each cheaply-mutatable bounded field, mint a valid baseline,
/// mutate that one field out of bounds, and assert the wire path rejects it as `RangeCheck`.
fn record_deser_reject(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
    value_eq: bool,
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

    // the fields we can actually push out of bounds: integer bounds cheaply measurable, or a float
    // window (always testable — below/above/excluded-endpoint/NaN).
    let targets: Vec<&RustField> = ctor_fields
        .iter()
        .copied()
        .filter(|f| {
            // the `[+ T]` shape enforces its bound in the type (`NonEmptyVec`), so its invalid
            // (empty) state is UNREPRESENTABLE — there is nothing to mutate-and-serialize for a
            // deser-reject. Its wire-side rejection is covered by the hand-written fixture tests
            // (an empty wire array must be rejected through the same TryFrom door).
            (f.rust_type.config.bounds.is_some()
                && measure_kind(&f.rust_type).is_some()
                && !f.rust_type.is_type_enforced_non_empty())
                || f.rust_type.config.float_bounds.is_some()
        })
        .collect();
    if targets.is_empty() {
        return None;
    }

    // valid baseline arg for every constructor field; bail the whole type if any isn't mintable
    let mut valid_args: Vec<String> = Vec::new();
    for f in &ctor_fields {
        match valid_value(types, &f.rust_type) {
            Some(v) => valid_args.push(render_rust(&v)),
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
        // float window vs integer window: different case generator + reject failure variant. A NaN
        // reject exercises the accept-form (NaN-safe) check that a reject-form check would let slip.
        let (cases, failure) = if let Some(window) = &target.rust_type.config.float_bounds {
            (
                float_bound_cases(window, float_is_f32(&target.rust_type)),
                "RangeCheckFloat",
            )
        } else {
            let is_len = measure_kind(&target.rust_type) == Some(MeasureKind::Len);
            (
                bound_cases(
                    types,
                    &target.rust_type,
                    target.rust_type.config.bounds.unwrap(),
                    is_len,
                ),
                "RangeCheck",
            )
        };
        // skip fields whose bound coincides with the rust type's domain: no representable
        // out-of-bounds value exists, so the only cases are accepts and the test would be vacuous.
        if !cases.iter().any(|(_, accept, _)| !accept) {
            continue;
        }
        for (expr, accept, label) in cases {
            let expr = render_rust(&expr);
            if accept {
                let value_eq_line = if value_eq {
                    format!(
                        "\n        assert_eq!(format!(\"{{:?}}\", back), format!(\"{{:?}}\", v), \"{name}.{field} {label}: deserialized value must equal the minted original\");"
                    )
                } else {
                    String::new()
                };
                blocks.push(format!(
                    "    {{
        let mut v = mk();
        v.{field} = {expr};
        let bytes = v.to_cbor_bytes();
        let back = {name}::from_cbor_bytes(&bytes).expect(\"{name}.{field} {label} must deserialize\");{value_eq_line}
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name}.{field} {label} must round-trip\");
    }}"
                ));
            } else {
                blocks.push(format!(
                    "    {{
        let mut v = mk();
        v.{field} = {expr};
        let err = {name}::from_cbor_bytes(&v.to_cbor_bytes()).unwrap_err();
        assert!(matches!(err.failure(), DeserializeFailure::{failure} {{ .. }}), \"{name}.{field} {label} must be rejected as {failure}, got {{:?}}\", err.failure());
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
    group_choice: bool,
) -> Option<String> {
    let mut lines = Vec::new();
    for variant in variants {
        let ctor = format!("new_{}", variant.name_as_var());
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            continue;
        };

        // which arg (if any) carries a cheaply-testable bound?
        for (i, (arg_ty, _)) in arg_fields.iter().enumerate() {
            let (cases, failure) = if let Some(window) = &arg_ty.config.float_bounds {
                (
                    float_bound_cases(window, float_is_f32(arg_ty)),
                    "RangeCheckFloat",
                )
            } else {
                let Some(kind) = measure_kind(arg_ty) else {
                    continue;
                };
                let Some(bounds) = arg_ty.config.bounds else {
                    continue;
                };
                // the `[+ T]` / `{+ k => v}` shapes enforce their bound in the TYPE
                // (`NonEmptyVec`/`NonEmptyMap`): the ctor takes the restricted type, so an
                // out-of-bounds arg is UNREPRESENTABLE — minting one would panic at the arg's
                // own `try_from(..).unwrap()`, not exercise the ctor (the same skip
                // `record_deser_reject` applies). Rejection via the TryFrom door is covered by
                // the hand-written fixture tests.
                if arg_ty.is_type_enforced_non_empty() {
                    continue;
                }
                (
                    bound_cases(types, arg_ty, bounds, kind == MeasureKind::Len),
                    "RangeCheck",
                )
            };
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
                        Some(s) => call_args.push(render_rust(&s)),
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
                        format!("    assert!(matches!({name}::{ctor}({args}).unwrap_err().failure(), DeserializeFailure::{failure} {{ .. }}), \"{name}::{ctor} {label} arg must be rejected as {failure}\");")
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
    // A bounded nint wrapper stores the inner as a u64 MAGNITUDE (`m = |v + 1|`) and its `new()`
    // checks the nint-transformed bounds (`generation/wrappers.rs` applies `nint_bounds_to_u64`). The
    // out-of-bounds direction is inverted in value space, so synthesize the boundary cases directly
    // in magnitude space: the transformed bounds, measured like a length (magnitude has a floor of 0,
    // so a "below min" case below 0 is dropped by `materialize`). `measure_kind` deliberately excludes
    // N64 (the standalone nint field/target direction is genuinely inverted), so handle it here.
    let (eff_bounds, is_len) = if matches!(
        wrapped.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::N64)
    ) {
        (nint_bounds_to_u64(min_max), true)
    } else {
        let kind = measure_kind(wrapped)?;
        (min_max, kind == MeasureKind::Len)
    };
    let cases = bound_cases(types, wrapped, eff_bounds, is_len);
    if !cases.iter().any(|(_, accept, _)| !accept) {
        return None; // bound == type domain: no constructible out-of-bounds value
    }
    let lines: Vec<String> = cases
        .into_iter()
        .map(|(expr, accept, label)| {
            let expr = render_rust(&expr);
            if accept {
                format!("    assert!({name}::new({expr}).is_ok(), \"{name}::new {label} value must be accepted\");")
            } else {
                format!("    assert!(matches!({name}::new({expr}).unwrap_err().failure(), DeserializeFailure::RangeCheck {{ .. }}), \"{name}::new {label} value must be rejected as RangeCheck\");")
            }
        })
        .collect();
    Some(lines.join("\n"))
}

/// construct-reject for a bounded FLOAT wrapper (`c = 0.5..10.5`, `#6.5(0.5..10.5)`). `new()` checks
/// the NaN-safe window over the inner value (compared as f64), so each boundary/margin/NaN case is
/// synthesized directly from the window and asserted to reject as `RangeCheckFloat`.
fn wrapper_construct_reject_float(
    name: &str,
    wrapped: &RustType,
    window: &crate::intermediate::FloatWindow,
) -> Option<String> {
    let cases = float_bound_cases(window, float_is_f32(wrapped));
    let lines: Vec<String> = cases
        .into_iter()
        .map(|(expr, accept, label)| {
            let expr = render_rust(&expr);
            if accept {
                format!("    assert!({name}::new({expr}).is_ok(), \"{name}::new {label} value must be accepted\");")
            } else {
                format!("    assert!(matches!({name}::new({expr}).unwrap_err().failure(), DeserializeFailure::RangeCheckFloat {{ .. }}), \"{name}::new {label} value must be rejected as RangeCheckFloat\");")
            }
        })
        .collect();
    Some(lines.join("\n"))
}

#[derive(PartialEq, Clone, Copy)]
pub(crate) enum MeasureKind {
    /// the value itself is bounded (integer primitives)
    Value,
    /// the length is bounded (text / bytes / array / map)
    Len,
}

/// How `ty`'s bound is measured, or `None` if it isn't a cheaply-testable bounded shape
/// (`nint`/bool/float values are excluded — see module docs).
pub(crate) fn measure_kind(ty: &RustType) -> Option<MeasureKind> {
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
pub(crate) fn valid_measure(b: Bounds) -> i128 {
    b.0.or(b.1).unwrap_or(0)
}

/// Render a float literal for a `MintValue`. NaN needs a typed constant (an f32 ctor param rejects
/// `f64::NAN`); finite values render via `{:?}` (round-trippable) with the `f32` suffix when needed.
pub(crate) fn render_float_lit(value: f64, is_f32: bool) -> String {
    if value.is_nan() {
        if is_f32 { "f32::NAN" } else { "f64::NAN" }.to_owned()
    } else if is_f32 {
        format!("{value:?}f32")
    } else {
        format!("{value:?}")
    }
}

/// A value safely INSIDE a float window (never a boundary), so a bounded float's round-trip / accessor
/// baseline constructs successfully: the two-sided midpoint, or an interior point a unit past a
/// one-sided endpoint (`.eq` collapses to its single value).
fn valid_float_in_window(window: &crate::intermediate::FloatWindow) -> f64 {
    match (window.0, window.1) {
        (Some((lo, _)), Some((hi, _))) => {
            if lo == hi {
                lo
            } else {
                (lo + hi) / 2.0
            }
        }
        (Some((lo, exclusive)), None) => {
            if exclusive {
                lo + 1.0
            } else {
                lo
            }
        }
        (None, Some((hi, exclusive))) => {
            if exclusive {
                hi - 1.0
            } else {
                hi
            }
        }
        (None, None) => 0.0,
    }
}

/// Accept/reject boundary cases for a float window: `(value, accept, label)`. Always includes the
/// out-of-window rejects (below min / above max with a unit of margin) and a NaN reject, plus an
/// interior accept. For an f64 window (exact representation) it also pins each endpoint — included
/// endpoints accept, excluded endpoints reject. f32 windows skip the exact-endpoint cases (an f32
/// value cast back to f64 need not equal the authored decimal), keeping only the margin/NaN cases.
fn float_bound_cases(
    window: &crate::intermediate::FloatWindow,
    is_f32: bool,
) -> Vec<(MintValue, bool, &'static str)> {
    let lit = |v: f64| MintValue::FloatLit { value: v, is_f32 };
    let mut out = Vec::new();
    // interior accept
    out.push((lit(valid_float_in_window(window)), true, "in-window"));
    if let Some((lo, exclusive)) = window.0 {
        out.push((lit(lo - 1.0), false, "below min"));
        if !is_f32 {
            out.push((
                lit(lo),
                !exclusive,
                if exclusive {
                    "excluded min"
                } else {
                    "min boundary"
                },
            ));
        }
    }
    if let Some((hi, exclusive)) = window.1 {
        out.push((lit(hi + 1.0), false, "above max"));
        if !is_f32 {
            out.push((
                lit(hi),
                !exclusive,
                if exclusive {
                    "excluded max"
                } else {
                    "max boundary"
                },
            ));
        }
    }
    // NaN must always be rejected by the NaN-safe accept-form check
    out.push((lit(f64::NAN), false, "NaN"));
    out
}

/// Whether a float primitive is f32 (its window value is stored as f64 but compared/minted as f32).
fn float_is_f32(ty: &RustType) -> bool {
    matches!(
        ty.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::F32)
    )
}

/// The in-range inner measure for a bounded wrapper's `new(inner)`. A bounded `nint` wrapper stores
/// the inner as a `u64` MAGNITUDE and its `new` checks the nint-transformed bounds (`generation/wrappers.rs`
/// applies `nint_bounds_to_u64` there), so the baseline must be minted from the transformed bounds —
/// otherwise a raw negative literal (e.g. `-5`) is passed to a `u64` ctor and the emitted code won't
/// compile. Non-nint wrappers check the raw measure directly.
fn wrapper_measure(wrapped: &RustType, mm: Bounds) -> i128 {
    if matches!(
        wrapped.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::N64)
    ) {
        valid_measure(nint_bounds_to_u64(mm))
    } else {
        valid_measure(mm)
    }
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
pub(crate) fn bound_cases(
    types: &IntermediateTypes,
    ty: &RustType,
    bounds: Bounds,
    is_len: bool,
) -> Vec<(MintValue, bool, &'static str)> {
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

/// A valid in-range minted value for `ty`, or `None` if it can't be cheaply minted.
pub(crate) fn valid_value(types: &IntermediateTypes, ty: &RustType) -> Option<MintValue> {
    valid_value_at(types, ty, 0)
}

fn valid_value_at(types: &IntermediateTypes, ty: &RustType, depth: u8) -> Option<MintValue> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Optional(_) => Some(MintValue::None),
        ConceptualRustType::Primitive(Primitive::Bool) => Some(MintValue::Bool),
        // a bounded float must mint IN-WINDOW (a default 0.0 may sit outside the window and fail the
        // ctor / round-trip); an unbounded float keeps the 0.0 baseline.
        ConceptualRustType::Primitive(Primitive::F32 | Primitive::F64) => {
            Some(match &ty.config.float_bounds {
                Some(window) => MintValue::FloatLit {
                    value: valid_float_in_window(window),
                    is_f32: float_is_f32(ty),
                },
                None => MintValue::Float,
            })
        }
        // nint can't be an OOB *target* (stored/wire direction is inverted), but a valid baseline
        // value is mintable: new()'s check uses the nint-transformed bounds, so the transformed
        // min (or 0 when unbounded) is in range.
        ConceptualRustType::Primitive(Primitive::N64) => {
            let b = ty
                .config
                .bounds
                .map(nint_bounds_to_u64)
                .unwrap_or((None, None));
            Some(MintValue::Int {
                value: valid_measure(b),
                prim: Primitive::N64,
            })
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

/// A semantically-VALID inner literal for a CBOR tag whose RFC 8949 content requirements the
/// reference `cddl` validator enforces on decode. Returns `None` for tags the validator accepts with
/// any well-typed content (only the enforced ones need a constant). Reads the tag off the wrapped
/// type's encoding operations — the tag lives on the inner `RustType` for a `#6.N(...)` prelude
/// wrapper, not on the wrapper struct.
fn semantic_tag_content(wrapped: &RustType) -> Option<&'static str> {
    use crate::intermediate::CBOREncodingOperation;
    let tag = wrapped.encodings.iter().find_map(|op| match op {
        CBOREncodingOperation::Tagged(t) => Some(*t),
        _ => None,
    })?;
    match tag {
        // tag 0 (tdate): RFC 8949 §3.4.1 — a standard date/time string per RFC 3339.
        0 => Some("1970-01-01T00:00:00Z"),
        _ => None,
    }
}

/// Mint a valid instance of a NAMED generated struct, recursing into its fields (depth-capped so
/// recursion terminates: at the cap this returns `None`, which an enclosing unbounded collection
/// absorbs by minting empty — loudly — while any other enclosing mint gets the caller's loud skip).
pub(crate) fn mint_struct(
    types: &IntermediateTypes,
    ident: &crate::intermediate::RustIdent,
    depth: u8,
) -> Option<MintValue> {
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
            let args: Option<Vec<MintValue>> = ctor_fields
                .iter()
                .map(|f| valid_value_at(types, &f.rust_type, depth + 1))
                .collect();
            Some(MintValue::Record {
                ident: name,
                args: args?,
                can_fail: record_ctor_can_fail(record),
            })
        }
        RustStructType::Wrapper {
            wrapped,
            min_max,
            float_min_max,
        } => {
            // Tag-aware minting: when the wrapped type carries a CBOR tag whose RFC 8949 content the
            // reference `cddl` validator SEMANTICALLY enforces, the generic `"a"` baseline is
            // spec-violating (it round-trips byte-identically but the conformance oracle rejects it).
            // Mint a fixed valid literal for exactly those tags instead. `None` for every other tag —
            // no speculative coverage beyond what the oracle demands.
            let inner = match semantic_tag_content(wrapped) {
                Some(content) => MintValue::StrLit {
                    content: content.to_owned(),
                },
                None => match float_min_max {
                    // a bounded float wrapper mints an in-window inner (0.0 may be outside the window)
                    Some(window) => MintValue::FloatLit {
                        value: valid_float_in_window(window),
                        is_f32: float_is_f32(wrapped),
                    },
                    None => match min_max {
                        Some(mm) => materialize_at(
                            types,
                            wrapped,
                            wrapper_measure(wrapped, *mm),
                            depth + 1,
                        )?,
                        None => valid_value_at(types, wrapped, depth + 1)?,
                    },
                },
            };
            Some(MintValue::Wrapper {
                ident: name,
                inner: Box::new(inner),
                can_fail: min_max.is_some() || float_min_max.is_some(),
            })
        }
        RustStructType::CStyleEnum { variants } => variants.first().map(|v| MintValue::CEnum {
            ident: name,
            variant: v.name.to_string(),
        }),
        RustStructType::TypeChoice { variants } => {
            mint_choice(types, &name, variants, false, depth)
        }
        RustStructType::GroupChoice { variants, .. } => {
            mint_choice(types, &name, variants, true, depth)
        }
        // transparent aliases: an empty map/vec is valid for `*`-occurrence tables/arrays, and the
        // alias's associated `new()` resolves to the underlying map type's constructor
        // ponytail: named tables mint empty (one-entry minting needs the struct's insert API);
        // inline `{ * k => v }` map *fields* already mint one entry via materialize_at, so the map
        // element wire path is still exercised there. Named-table standalone element coverage is
        // owned at the embed site by verify.ts's synthetic-holder probe (cddl-matrix/README.md).
        RustStructType::Table {
            domain,
            range,
            bounds,
        } => {
            if *bounds == Some((Some(1), None)) {
                // a non-empty table: `TableEmpty` (`Foo::new()`) is invalid — the `NonEmptyMap` alias
                // has no zero-arg ctor. Mint one entry through the Map path (routed through the same
                // `NonEmptyMap::try_from` door). Named tables are skipped from STANDALONE round-trips
                // (transparent alias), so this only feeds an embed site that references the rule.
                let map_ty: RustType =
                    ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone()))
                        .into();
                materialize_at(types, &map_ty.with_bounds((Some(1), None)), 1, depth + 1)
            } else {
                Some(MintValue::TableEmpty { ident: name })
            }
        }
        RustStructType::Array {
            element_type,
            bounds,
        } => {
            // mint one element so the element serialize/deserialize path runs; fall back to empty
            // (valid for `*`) when the element isn't cheaply mintable.
            Some(MintValue::Array {
                elem: valid_value_at(types, element_type, depth + 1).map(Box::new),
                count: 1,
                non_empty: *bounds == Some((Some(1), None)),
            })
        }
        // the reserved `int` prelude resolves to the hand-written `Int` extern (static prelude):
        // mint its non-negative baseline through `Int::new_uint`. A bare CDDL `int` carries no
        // bounds, so `0` is always in range. Every OTHER extern references user-supplied code the
        // generated crate can't construct — a loud skip at the caller.
        RustStructType::Extern if name == "Int" => Some(MintValue::IntExtern {
            ident: name,
            value: 0,
        }),
        RustStructType::Extern | RustStructType::RawBytesType => None,
    }
}

/// Mint the first constructible variant of a choice (deterministic: variant order is IR order).
fn mint_choice(
    types: &IntermediateTypes,
    name: &str,
    variants: &[EnumVariant],
    group_choice: bool,
    depth: u8,
) -> Option<MintValue> {
    for variant in variants {
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            continue;
        };
        let args: Option<Vec<MintValue>> = arg_fields
            .iter()
            .map(|(ty, _)| valid_value_at(types, ty, depth + 1))
            .collect();
        let Some(args) = args else { continue };
        return Some(MintValue::Choice {
            ident: name.to_owned(),
            variant: variant.name_as_var(),
            args,
            can_fail: arg_fields.iter().any(|(ty, _)| arg_can_fail(types, ty)),
        });
    }
    None
}

/// An empty value expression for a collection `ty` (valid for a 0-lower-bound `*`-occurrence).
/// `Default::default()` covers every collection representation an inline map field can take
/// (`BTreeMap`, or the preserve-encodings `OrderedHashMap`, both `Default`), inferred from the
/// constructor-argument position; `vec![]` is the clearer form for arrays.
fn empty_collection(ty: &RustType) -> Option<MintValue> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Array(_) => Some(MintValue::Array {
            elem: None,
            count: 0,
            non_empty: false,
        }),
        ConceptualRustType::Map(_, _) => Some(MintValue::DefaultMap),
        _ => None,
    }
}

/// Build a minted value for `ty` whose bound-relevant measure equals `measure`
/// (the value itself for integers, the length for text/bytes/array/map).
pub(crate) fn materialize(
    types: &IntermediateTypes,
    ty: &RustType,
    measure: i128,
) -> Option<MintValue> {
    materialize_at(types, ty, measure, 0)
}

fn materialize_at(
    types: &IntermediateTypes,
    ty: &RustType,
    measure: i128,
    depth: u8,
) -> Option<MintValue> {
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
                (measure >= lo && measure <= hi).then_some(MintValue::Int {
                    value: measure,
                    prim: *p,
                })
            }
            // nint stored values are non-negative u64 magnitudes. A "below min" boundary case can
            // ask for magnitude -1 (e.g. a `.le -1` wrapper whose magnitude floor is 0); that isn't
            // representable in the u64 backing type, so drop it rather than render `new(-1)`.
            Primitive::N64 => (measure >= 0).then_some(MintValue::Int {
                value: measure,
                prim: Primitive::N64,
            }),
            Primitive::Str => Some(MintValue::Str { len: measure }),
            Primitive::Bytes => Some(MintValue::Bytes { len: measure }),
            Primitive::Bool => Some(MintValue::Bool),
            Primitive::F32 | Primitive::F64 => Some(match &ty.config.float_bounds {
                Some(window) => MintValue::FloatLit {
                    value: valid_float_in_window(window),
                    is_f32: matches!(p, Primitive::F32),
                },
                None => MintValue::Float,
            }),
        },
        ConceptualRustType::Array(elem) => {
            let e = valid_value_at(types, elem, depth)?;
            Some(MintValue::Array {
                elem: Some(Box::new(e)),
                count: measure,
                non_empty: ty.is_type_enforced_non_empty(),
            })
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
                ) => MapKey::Int(*p),
                ConceptualRustType::Primitive(Primitive::Str) => MapKey::Str,
                // one distinct byte per index (`vec![__i as u8]`); measure <= 256 keeps the keys
                // distinct (beyond that `__i as u8` wraps and collect() would dedupe). Minted
                // measures are tiny (0/1), so the cap is never a real constraint here.
                ConceptualRustType::Primitive(Primitive::Bytes) if measure <= 256 => MapKey::Bytes,
                // bool has exactly 2 distinct keys, so only lengths <= 2 are mintable — beyond
                // that the collect() would dedupe and the map would miss its target measure
                ConceptualRustType::Primitive(Primitive::Bool) if measure <= 2 => MapKey::Bool,
                other => {
                    // A key shape we can't synthesize distinct instances for (named struct / tag
                    // key, or a bytes/bool key past its distinct-value cap). The enclosing collection
                    // still mints EMPTY via the caller's unbounded fallback, but that generic notice
                    // masks WHY — surface the specific key here so the gap isn't silently a wrong
                    // reason.
                    eprintln!(
                        "cddl-codegen --emit-tests: map key {other:?} not cheaply mintable — the map's key wire path is unexercised"
                    );
                    return None;
                }
            };
            let val = valid_value_at(types, v, depth)?;
            // distinct keys 0..measure; collect() infers the map type from the target position
            Some(MintValue::Map {
                key,
                val: Box::new(val),
                count: measure,
                non_empty: ty.is_type_enforced_non_empty(),
            })
        }
        _ => None,
    }
}

/// The constructor arg list of a choice variant's `new_<variant>` (mirrors `generate_enum`), or
/// `None` when it isn't cheaply constructible (inlined records with optional fields — deferred).
///
/// `group_choice` mirrors the generator's `rep.and(fields)` flatten rule (`generation/enums.rs`): a
/// GROUP-choice variant that names a record flattens its fields into `new_<variant>(field, ..)`,
/// but a TYPE-choice variant (no representation) passes the whole named value as one arg
/// (`new_<variant>(WholeType)`). Flattening a type-choice variant emits an uncompilable ctor call.
pub(crate) fn variant_arg_fields<'a>(
    types: &'a IntermediateTypes,
    variant: &'a EnumVariant,
    group_choice: bool,
) -> Option<Vec<(&'a RustType, String)>> {
    match &variant.data {
        EnumVariantData::RustType(ty) => {
            if let Some(record) = group_choice.then(|| ty_as_record(types, ty)).flatten() {
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
    if let ConceptualRustType::Rust(ident) = &ty.conceptual_type
        && let RustStructType::Record(record) = types.rust_struct(ident)?.variant()
    {
        return Some(record);
    }
    None
}
