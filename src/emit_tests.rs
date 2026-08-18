//! `--emit-tests` generated-test emitter: the REJECT half and the ROUND-TRIP half.
//!
//! Everything here is derived from each type's IR at generation time — there are no hand-authored
//! value lists. The per-IR-shape derivation rules below are the single maintained surface, and any
//! type/field/variant they can't mint is skipped with a `warn!` (stderr, visible at the default
//! verbosity), never a silently-weakened test. The one deliberate weakening — an unbounded collection whose element can't be minted is
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
//! * **construct-reject** (record/choice bounded-array arguments, other bounded type/group choices,
//!   and bounded `@newtype` wrappers): assert the fallible construction door rejects an
//!   out-of-bounds value as `RangeCheck` (and accepts the boundary). For a bounded-array argument
//!   that door is `BoundedVec::try_from`; the checked value then passes into its infallible outer
//!   record/choice constructor. Type and group choices share the same deserialization code, so we
//!   only check the construction API here.
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
//! causes that one type/case to be skipped with a `warn!`.

use crate::cli::Cli;
use crate::intermediate::{
    ConceptualRustType, EnumVariant, EnumVariantData, FixedValue, IntermediateTypes, Primitive,
    Representation, RestRow, RustField, RustIdent, RustRecord, RustStruct, RustStructType,
    RustType,
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

/// The synthesized-key kind for a minted map (distinct keys `key_base..key_base+count`).
#[derive(Clone, Debug)]
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
    /// (`NonEmptyVec::try_from(vec![..]).unwrap()`). When `reject` the target is the `@duplicates
    /// reject` uniqueness twin. `unique_elems`, when present, supplies distinct values for each
    /// requested member so a finite/minimum bounded set is minted at its actual valid cardinality.
    Array {
        elem: Option<Box<MintValue>>,
        count: i128,
        non_empty: bool,
        /// A type-enforced finite/zero-minimum occurrence. The renderer uses the same checked door
        /// as generated decode rather than assigning a loose vector to a restricted field.
        bounded: Option<(u64, u64)>,
        reject: bool,
        unique_elems: Option<Vec<MintValue>>,
    },
    /// a map of `count` entries with synthesized keys. When `non_empty` the target type is
    /// `NonEmptyMap<K, V>` (`{+ k => v}`), so it is built through the single TryFrom door
    /// (`NonEmptyMap::try_from(map).unwrap()`, the collect target inferred from the sole impl).
    Map {
        key: MapKey,
        /// the first synthesized key; the `count` keys are `key_base .. key_base + count`. Non-zero
        /// only when the key domain carries a value window that excludes `0` (see `map_key_base`);
        /// at `0` every renderer emits exactly the spelling it emitted before bases existed.
        key_base: i128,
        val: Box<MintValue>,
        count: i128,
        non_empty: bool,
        bounded: Option<(u64, u64)>,
        /// `@duplicates preserve`: the target is `PairMap<K, V>` (or `NonEmptyPairMap` when also
        /// non-empty). Duplicates are permitted, so N distinct-key entries synthesize fine; the
        /// non-empty flavor still routes through its `new`/`insert` door.
        preserve: bool,
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
    /// a CDDL `any` (`AnyCbor`): a fixed composite `[uint 5, float 1.5]` built through the mode-paired
    /// `new_*` constructors. The float head is deliberate — it is what makes the `--preserve-encodings`
    /// encoding-fidelity `widen_float` mutation class reachable (a uint-only mint would leave that
    /// class dead). Renders via the `__AnyCborMint` alias `emit_generated_tests` injects for the
    /// import-glued `AnyCbor` path, so `render_rust` stays free of the CLI (the alias resolves in both
    /// the default and `--common-import-override` arrangements).
    Any,
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
            bounded,
            reject,
            unique_elems,
        } => {
            if *reject {
                let vec = unique_elems.as_ref().map_or_else(
                    || format!("vec![{}; {count}]", render_rust(e)),
                    |elems| format!("vec![{}]", elems.iter().map(render_rust).collect::<Vec<_>>().join(", ")),
                );
                let twin = if let Some((min, max)) = bounded {
                    return format!("BoundedOrderedSet::<_, {min}, {max}>::try_from({vec}).unwrap()");
                } else if *non_empty {
                    "NonEmptyOrderedSet"
                } else {
                    "OrderedSet"
                };
                format!("{twin}::try_from({vec}).unwrap()")
            } else {
                let vec = format!("vec![{}; {count}]", render_rust(e));
                if let Some((min, max)) = bounded {
                    format!("BoundedVec::<_, {min}, {max}>::try_from({vec}).unwrap()")
                } else if *non_empty {
                    // route through the single TryFrom door (same as every other construction path)
                    format!("NonEmptyVec::try_from({vec}).unwrap()")
                } else {
                    vec
                }
            }
        }
        MintValue::Array {
            elem: None,
            reject: true,
            bounded: Some((min, max)),
            ..
        } => format!("BoundedOrderedSet::<_, {min}, {max}>::try_from(vec![]).unwrap()"),
        MintValue::Array { elem: None, reject: true, .. } => "OrderedSet::try_from(vec![]).unwrap()".to_owned(),
        MintValue::Array { elem: None, .. } => "vec![]".to_owned(),
        MintValue::Map {
            key,
            key_base,
            val,
            count,
            non_empty,
            bounded,
            preserve,
        } => {
            let k = map_key_expr(key, *key_base);
            let v = render_rust(val);
            if let Some((min, max)) = bounded {
                let carrier = if *preserve { "BoundedPairMap" } else { "BoundedMap" };
                // A preserve table's generated valid fixture deliberately reuses its key: this is
                // the emitted-test coverage that proves the bounded door counts duplicate entries
                // rather than silently collecting them into a unique-key map.
                let k = if *preserve { k.replace("__i", "0") } else { k };
                let index = if *preserve { "_i" } else { "__i" };
                format!("{carrier}::<_, _, {min}, {max}>::try_from((0u64..{count}).map(|{index}| ({k}, {v})).collect::<Vec<_>>()).unwrap()")
            } else if *non_empty {
                // build via `new(first_key, first_value)` + `insert` (flavor-agnostic and
                // unambiguous). A bare `try_from((..).collect())` can't infer the collect target here:
                // the reflexive `TryFrom<Self>` blanket competes with `TryFrom<{table_type}>`, so the
                // `{table_type}` (BTreeMap / OrderedHashMap) is not uniquely determined. `new` never
                // names the inner map type, so it compiles under every profile. The preserve flavor
                // routes through `NonEmptyPairMap` (whose `new`/`insert` mirror `NonEmptyMap`'s).
                let ctor = if *preserve {
                    "NonEmptyPairMap"
                } else {
                    "NonEmptyMap"
                };
                format!(
                    "{{ let mut __m = {{ let __i = 0u64; {ctor}::new({k}, {v}) }}; for __i in 1u64..{count} {{ __m.insert({k}, {v}); }} __m }}"
                )
            } else {
                // `.collect()` infers the target from the field type (PairMap has FromIterator too).
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
        // `[uint 5, float 1.5]` through the mode-paired ctors. The float head is what the preserve
        // `widen_float` fidelity class widens; `__AnyCborMint` is the import-glued `AnyCbor` alias
        // `emit_generated_tests` injects at the test module root.
        MintValue::Any => "__AnyCborMint::new_array(vec![__AnyCborMint::new_uint(5), __AnyCborMint::new_float(1.5)])".to_owned(),
    }
}

/// The synthesized map key at loop index `__i`, offset by `key_base` (so the `count` keys are
/// `key_base .. key_base + count`). Owned here rather than duplicated per renderer: the wasm
/// renderer calls this same function, so the rust and wasm key spellings cannot drift apart.
///
/// At `key_base == 0` every arm is the pre-base spelling verbatim, so no already-blessed emitted
/// test text moves when bases are introduced.
pub(crate) fn map_key_expr(key: &MapKey, key_base: i128) -> String {
    match key {
        MapKey::Int(p) if key_base == 0 => format!("__i as {p}"),
        // `__i` is a `u64` loop index; widen before offsetting so a negative base is expressible,
        // then narrow to the key's own primitive (the acceptance check in `map_key_base` has
        // already proven every key in the run fits that primitive's range).
        MapKey::Int(p) => format!("({key_base} + __i as i128) as {p}"),
        MapKey::Str if key_base == 0 => "__i.to_string()".to_owned(),
        MapKey::Str => format!("({key_base} + __i as i128).to_string()"),
        MapKey::Bytes if key_base == 0 => "vec![__i as u8]".to_owned(),
        MapKey::Bytes => format!("vec![({key_base} + __i as i128) as u8]"),
        MapKey::Bool if key_base == 0 => "__i == 1".to_owned(),
        MapKey::Bool => format!("(__i + {key_base}u64) % 2 == 1"),
    }
}

/// A single synthesized map key at index `i` — the literal form of `map_key_expr`, for the wasm
/// renderer's explicit-`insert` build where `__i` (a closure param) isn't in scope. Shared for the
/// same anti-drift reason.
pub(crate) fn map_key_literal(key: &MapKey, key_base: i128, i: i128) -> String {
    let i = key_base + i;
    match key {
        MapKey::Int(p) => format!("{i} as {p}"),
        MapKey::Str => format!("{i}.to_string()"),
        MapKey::Bytes => format!("vec![{i} as u8]"),
        MapKey::Bool => format!("{i} % 2 == 1"),
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
    no_deserialize: &std::collections::BTreeSet<RustIdent>,
) -> Option<String> {
    if !cli.to_from_bytes_methods {
        // both halves need to_cbor_bytes/from_cbor_bytes
        crate::warn!(
            "cddl-codegen --emit-tests: skipped (requires --to-from-bytes-methods, which is off)"
        );
        return None;
    }

    let mut fns: Vec<String> = Vec::new();
    for (ident, rust_struct) in types.rust_structs() {
        let name = ident.to_string();
        // BOTH halves decode: the round-trip asserts `from_cbor_bytes(to_cbor_bytes(v))` and the
        // reject half asserts `from_cbor_bytes` REJECTS out-of-bounds wire bytes. Neither exists
        // for a type the generator declined to give a `Deserialize` (the loud
        // `Not generating {name}::deserialize()` warning says why), so minting either would emit a
        // test crate that does not build.
        if no_deserialize.contains(ident) {
            crate::warn!(
                "cddl-codegen --emit-tests: {name} skipped (no Deserialize impl was generated for it)"
            );
            continue;
        }
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
                crate::warn!(
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
    // CDDL `any` (`AnyCbor`) mint alias: generated members reference `AnyCbor` by the import-glued
    // FULL path (`for_rust_member_ct`: `crate::generated::any_cbor::AnyCbor`, or the override crate),
    // never a bare re-export, so the minted `MintValue::Any` constructor calls need that path in
    // scope. Bind it to a `__`-prefixed alias (never collides with a camel-cased user type) so
    // `render_rust` can emit CLI-free constructor calls that resolve in BOTH the default and
    // `--common-import-override` arrangements. `#[allow(unused_imports)]`: `uses_any_cbor()` can be
    // true while every `any` sits in a position the minter skips (e.g. an `AnyCbor`-keyed table),
    // leaving the alias unused. Emitted only when the spec uses `any`, so any-free output is
    // byte-identical.
    let any_import = if types.uses_any_cbor() {
        format!(
            "    #[allow(unused_imports)]\n    use {}::any_cbor::AnyCbor as __AnyCborMint;\n",
            cli.common_import_rust()
        )
    } else {
        String::new()
    };
    // Keep error-name ownership local to the generated-test module. If a type's production code
    // needs no error export but its emitted reject probe does, resolving `DeserializeFailure`
    // through the parent's `error::*` makes that parent glob look needed to the source-level import
    // pruner while rustc still diagnoses it as unused (a descendant's `use super::*` does not count
    // as a use of every parent glob). An explicit child import lets both analyses agree.
    let bounded_failure_import = if fns
        .iter()
        .any(|body| body.contains("__CddlTestDeserializeFailure"))
    {
        format!(
            "    use {}::error::DeserializeFailure as __CddlTestDeserializeFailure;\n",
            cli.common_import_rust()
        )
    } else {
        String::new()
    };
    Some(format!(
        "#[cfg(test)]\n#[allow(clippy::all)]\n{unused_imports_allow}mod cddl_generated_tests {{\n{STD_RESTORE}    use super::*;\n    use super::serialization::*;\n{bounded_failure_import}{any_import}{scope_globs}{conformance_mod}{fidelity_mod}{}\n}}\n",
        fns.join("\n")
    ))
}

/// The emitted test module's own `std` restore, so `cargo test --no-default-features --lib` works on
/// a generated crate.
///
/// **Why it is needed.** The seeded crate root carries `#![cfg_attr(not(feature = "std"), no_std)]`,
/// so under `--no-default-features` the language prelude is `core`'s — and this module's bodies use
/// `std::env`/`std::fs` (the minted-bytes dump hook), `format!`, `vec!`, `eprintln!`, `String` and
/// `.to_string()`, none of which resolve there. Tests always run on a host, where `std` exists to be
/// linked, so the module restores it for itself instead of the combination being unsupported.
///
/// **Why it is module-local.** A non-crate-root `extern crate` does not reach a nested inline `mod`
/// body, and this module is exactly that — nested inside `generated/mod.rs`. Delivering it from the
/// crate root is not available either: that root is seed-once, so a new line there would never reach
/// an already-generated consumer tree. `static/emit_tests_encoding_fidelity.rs`'s own nested
/// `mod cddl_encoding_fidelity` carries its own copy of this pair for the same reason, and says so.
///
/// **Why it is unconditional.** Single-variant emission is what the snapshot corpus, the
/// comment-preservation overlay and the determinism invariants all want. Under default features
/// `std` IS the prelude, so the glob re-imports the same items and neither line warns (`use` globs
/// never warn; `unused_extern_crates` is allow-by-default).
///
/// **Why `use std::panic;` is here.** `std::panic!` and `core::panic!` are DIFFERENT macros, so
/// under `not(std)` the prelude glob alone makes every emitted `panic!` (the preserve-encodings
/// fidelity loop's `unwrap_or_else`) ambiguous with the core prelude's —
/// `ambiguous_panic_imports`, a future-incompatibility warning slated to become a hard error. An
/// explicit import is the disambiguation rustc itself suggests; the `#[allow(unused_imports)]`
/// covers the emissions that mint no `panic!` at all (any non-preserve crate).
const STD_RESTORE: &str = "    extern crate std;\n    #[allow(unused_imports)]\n    use std::panic;\n    use std::prelude::rust_2024::*;\n";

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
            crate::warn!(
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
            // `any` uses no custom serialization.
            ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_)
            | ConceptualRustType::Any => false,
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

/// What an enum's round-trip needs to assert the property the WIRE has, rather than the one the
/// Rust API suggests.
///
/// A choice's decoder tries its arms in declaration order and returns the FIRST that accepts the
/// bytes. So when two arms overlap on the wire — `[ ga: -10..10 / tstr ]` vs a plain `tstr` arm, a
/// `bytes .cbor uint` arm vs a plain `bytes` arm, or an explicitly `@name`d duplicate the IR keeps —
/// a value minted from the LATER arm legitimately decodes back as the EARLIER variant. Asserting
/// variant identity there is asserting something the wire cannot carry, and produces a false red on
/// a correct decoder.
///
/// So the emitted test asserts the first-match property instead: the decoded variant index `j` must
/// satisfy `j <= i` for the minted index `i` (`j > i` means an earlier matching arm was skipped —
/// always a decoder bug), value identity is asserted only when `j == i`, and byte-identity of the
/// re-encode is asserted in BOTH cases (the fidelity a choice genuinely owes).
struct FirstMatch {
    /// The `match &back { … }` arms mapping every variant of the enum to its declaration index.
    arms: String,
    /// Minted variant index per emitted case — parallel to `roundtrip_body`'s `cases` (a variant
    /// that minted no case leaves no entry, so this is not the variant index itself).
    minted: Vec<usize>,
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
///
/// `first_match` is the choice flavor's honesty correction — see `FirstMatch`.
fn roundtrip_body(
    name: &str,
    cases: Vec<(String, String)>,
    conf: Option<&str>,
    dump_rule: Option<&str>,
    rt: RtEmit,
    first_match: Option<&FirstMatch>,
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
            // The choice flavor's first-match block: compute which variant came BACK, refuse a
            // decode that skipped an earlier matching arm, and demote value-identity to the cases
            // where the decoder did land on the minted arm (see `FirstMatch`).
            let first_match_line = first_match
                .map(|fm| {
                    let minted = fm.minted[case_idx];
                    format!(
                        "
        // The decoder tries the arms in declaration order, so a value minted from a later arm that
        // an EARLIER arm also accepts comes back as the earlier variant. That is the wire's answer,
        // not a bug: what a choice can carry is the encoding, not which arm produced it.
        let minted_variant = {minted}usize;
        let decoded_variant = match &back {{{arms}
        }};
        assert!(decoded_variant <= minted_variant, \"{name} ({label}): decoded as variant #{{decoded_variant}}, later than the minted #{{minted_variant}} — the decoder skipped an earlier arm that matches these bytes\");",
                        arms = fm.arms
                    )
                })
                .unwrap_or_default();
            let value_eq_line = if value_eq {
                let assertion = format!("assert_eq!(format!(\"{{:?}}\", back), format!(\"{{:?}}\", v), \"{name} ({label}): deserialized value must equal the minted original\");");
                if first_match.is_some() {
                    // `decoded_variant < minted_variant` means an earlier arm accepts the same
                    // encoding, so the minted variant is unreachable by decode and value identity is
                    // not a property the wire has. Byte-identity below still holds, and is asserted
                    // unconditionally — that is the fidelity the wire DOES carry.
                    format!("\n        if decoded_variant == minted_variant {{\n            {assertion}\n        }}")
                } else {
                    format!("\n        {assertion}")
                }
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
{dump_line}{conf_line}        let back = {name}::from_cbor_bytes(&bytes).expect(\"{name} ({label}): serialized bytes must deserialize\");{first_match_line}{value_eq_line}
        assert_eq!(back.to_cbor_bytes(), bytes, \"{name} ({label}): wire round-trip must be byte-identical\");{fidelity}
    }}"
            )
        })
        .collect();
    Some(blocks.join("\n"))
}

/// Mirrors the record constructor's fallibility rule (`generation/records.rs` `new_can_fail`):
/// bounds and protected complete-rest validation are the two reasons `new()` returns `Result`.
pub(crate) fn record_ctor_can_fail(record: &RustRecord, types: &IntermediateTypes) -> bool {
    record
        .fields
        .iter()
        .any(|f| !f.optional && f.rust_type.has_value_bounds())
        || record
            .captured_dynamic_rows()
            .any(|row| row.is_non_empty_array_tail() && row.element().has_value_bounds())
        || (record.has_forbidden_fields() && record.has_protected_rest_keys(types))
        || (record.has_protected_rest_keys(types)
            && record
                .captured_rest()
                .is_some_and(|row| !row.is_array_tail() && row.is_restricted()))
}

/// The wasm record constructor is normally as fallible as the native one.  One open-table shape
/// adds a wasm-only checked door: a bounded typed row remains flattened on the owner class, so wasm
/// receives a loose builder and turns it into the native checked carrier.
/// Keep this beside `record_ctor_can_fail`, which `generation::records::codegen_struct` explicitly
/// mirrors for the native constructor.
pub(crate) fn record_wasm_ctor_can_fail(record: &RustRecord, types: &IntermediateTypes) -> bool {
    record_ctor_can_fail(record, types)
        || record.typed_row().is_some_and(|row| {
            !row.is_array_tail() && row.container_type().bounded_map_u64_bounds().is_some()
        })
}

/// The source-indexed native constructor slots for a multiple-exact ARRAY record. Its new API is
/// intentionally source-ordered; consumers whose own public ABI is field-then-wrapper (the wasm
/// face) must project by this identity rather than zip positions.
pub(crate) fn multi_exact_array_ctor_arg_slots(
    record: &RustRecord,
) -> Option<Vec<(usize, RustType)>> {
    if record.rep != Representation::Array || record.array_segments.is_empty() {
        return None;
    }
    let mut args: Vec<(usize, RustType)> = record
        .fields
        .iter()
        .filter(|field| {
            !field.optional
                && !field.rust_type.is_fixed_value()
                && field.rust_type.config.default.is_none()
        })
        .map(|field| (field.source_index, field.rust_type.clone()))
        .chain(
            record
                .captured_dynamic_rows()
                .filter(|row| row.is_array_tail())
                .map(|row| {
                    (
                        row.array_source_index()
                            .expect("array segment has a source index"),
                        row.container_type(),
                    )
                }),
        )
        .collect();
    args.sort_by_key(|(source_index, _)| *source_index);
    Some(args)
}

/// The generated record constructor's argument types, in its exact native ABI order. This is
/// source order for multiple-exact ARRAY records and the historic field-then-dynamic-row order
/// otherwise.
pub(crate) fn record_ctor_arg_types(
    record: &RustRecord,
    types: &IntermediateTypes,
) -> Vec<RustType> {
    if let Some(slots) = multi_exact_array_ctor_arg_slots(record) {
        return slots.into_iter().map(|(_, ty)| ty).collect();
    }
    let mut args: Vec<RustType> = record
        .fields
        .iter()
        .filter(|f| {
            !f.optional && !f.rust_type.is_fixed_value() && f.rust_type.config.default.is_none()
        })
        .map(|f| f.rust_type.clone())
        .collect();
    if let Some(typed) = record
        .typed_row()
        .filter(|_| record.is_non_empty_open_table())
    {
        args.push(typed.domain().clone());
        args.push(typed.range().clone());
    }
    // All remaining restricted map rows reach `new` as their complete checked carrier. The typed
    // min-one row above deliberately keeps its first-key/first-value compatibility ABI instead.
    args.extend(
        record
            .captured_dynamic_rows()
            .filter(|row| {
                !row.is_array_tail()
                    && (row.is_restricted()
                        || (record.has_forbidden_fields() && record.has_protected_rest_keys(types)))
                    && !(record.is_typed_row(row) && record.is_non_empty_open_table())
            })
            .map(|row| row.container_type()),
    );
    args.extend(
        record
            .captured_dynamic_rows()
            .filter(|row| row.is_non_empty_array_tail())
            .map(|row| row.element().clone()),
    );
    args.extend(
        record
            .captured_dynamic_rows()
            .filter(|row| {
                row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail()
            })
            .map(|row| row.container_type()),
    );
    args
}

/// A map rest row in an open struct shares its key space with declared fixed members.  The generic
/// map minter starts at the first in-window key, which can make a valid checked carrier collide with
/// a declared key only after it reaches record serialization (`{ 1: uint, 2*3 uint => text }` was
/// the concrete failure). Shift the complete run to the first cheap collision-free window instead;
/// a row with no such window is skipped loudly, never minted as an invalid record.
fn mint_dynamic_map_row(
    types: &IntermediateTypes,
    record: &RustRecord,
    row: &RestRow,
    depth: u8,
) -> Option<MintValue> {
    let carrier = row.container_type();
    let mut value = valid_value_at(types, &carrier, depth)?;
    let MintValue::Map {
        key,
        key_base,
        count,
        ..
    } = &mut value
    else {
        return Some(value);
    };
    let fixed_keys: Vec<&FixedValue> = record
        .fields
        .iter()
        .filter_map(|field| field.key.as_ref())
        .chain(record.forbidden_fields.iter().map(|field| &field.key))
        .collect();
    if fixed_keys.is_empty() || *count == 0 {
        return Some(value);
    }
    let max_offset = match key {
        // One fixed key can rule out up to `count` consecutive candidate starts (a two-entry run
        // at bases 0 and 1 both contains fixed key 1).  Searching `fixed_keys * count`, plus the
        // initial candidate, therefore reaches the first inexpensive free run without claiming
        // that a finite key domain has room.
        MapKey::Int(_) | MapKey::Str => (fixed_keys.len() as i128).checked_mul(*count)?,
        // Bytes have 256 one-byte distinct candidates; a larger map was already rejected by the
        // generic minter before it reached this helper.
        MapKey::Bytes => 256i128.checked_sub(*count)?,
        // A boolean map can have exactly two distinct key runs (false/true or true/false).
        MapKey::Bool => 1,
    };
    let initial = *key_base;
    for offset in 0..=max_offset {
        let Some(candidate) = initial.checked_add(offset) else {
            break;
        };
        if !map_key_run_is_accepted(key, row.domain(), *count, candidate)
            || (0..*count).any(|index| {
                fixed_keys
                    .iter()
                    .any(|fixed| map_minted_key_equals_fixed(key, candidate, index, fixed))
            })
        {
            continue;
        }
        *key_base = candidate;
        return Some(value);
    }
    crate::warn!(
        "cddl-codegen --emit-tests: dynamic map row {} has no cheaply minted key run that avoids this record's fixed keys — its row wire path is unexercised",
        row.field_name
    );
    None
}

/// The canonical CDDL value denoted by one storage-space coordinate of a minted map key.
///
/// `MintValue::Map::{key_base,count}` remain storage coordinates because both renderers emit their
/// carriers directly (notably N64's u64 magnitude). Every semantic question about a coordinate
/// comes through this view instead: bounds see its numeric CDDL value and fixed-key collision sees
/// `FixedValue` equality. `None` means the coordinate cannot be rendered without a narrowing or
/// wrapping conversion, so callers reject it rather than silently changing the minted key.
fn map_minted_key_fixed_value(key: &MapKey, coordinate: i128) -> Option<FixedValue> {
    match key {
        MapKey::Int(Primitive::N64) => {
            let magnitude = u64::try_from(coordinate).ok()?;
            Some(FixedValue::Nint(-1 - magnitude as i128))
        }
        MapKey::Int(p @ (Primitive::U8 | Primitive::U16 | Primitive::U32 | Primitive::U64)) => {
            let (min, max) = prim_range(p);
            (min..=max)
                .contains(&coordinate)
                .then_some(FixedValue::Uint(coordinate as u64))
        }
        MapKey::Int(p @ (Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64)) => {
            let (min, max) = prim_range(p);
            if !(min..=max).contains(&coordinate) {
                None
            } else if coordinate >= 0 {
                Some(FixedValue::Uint(coordinate as u64))
            } else {
                Some(FixedValue::Nint(coordinate))
            }
        }
        MapKey::Int(_) => None,
        MapKey::Str => Some(FixedValue::Text(coordinate.to_string())),
        MapKey::Bytes => u8::try_from(coordinate)
            .ok()
            .map(|byte| FixedValue::Bytes(vec![byte])),
        MapKey::Bool => u64::try_from(coordinate)
            .ok()
            .map(|value| FixedValue::Bool(value % 2 == 1)),
    }
}

/// Whether one entry from a `MintValue::Map` run has the same CDDL value as a declared map key.
/// All key mints are primitive, so matching is exact and does not need to re-encode CBOR.
fn map_minted_key_equals_fixed(key: &MapKey, base: i128, index: i128, fixed: &FixedValue) -> bool {
    base.checked_add(index)
        .and_then(|coordinate| map_minted_key_fixed_value(key, coordinate))
        .is_some_and(|minted| minted == *fixed)
}

/// The shifted numeric run must continue to obey the domain bounds and primitive carrier.  This is
/// map-key-base's acceptance check factored for the open-struct collision search above.
fn map_key_run_is_accepted(key: &MapKey, key_ty: &RustType, count: i128, base: i128) -> bool {
    if count <= 0 {
        return true;
    }
    let MapKey::Int(_) = key else {
        return true;
    };
    (0..count).all(|offset| {
        let Some(coordinate) = base.checked_add(offset) else {
            return false;
        };
        let Some(value) = map_minted_key_fixed_value(key, coordinate) else {
            return false;
        };
        let numeric_value = match value {
            FixedValue::Uint(value) => value as i128,
            FixedValue::Nint(value) => value,
            _ => return false,
        };
        key_ty
            .config
            .bounds
            .is_none_or(|bounds| !crate::generation::bounds_reject_value(&bounds, numeric_value))
    })
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
                crate::warn!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (field {} not cheaply mintable)",
                    f.name
                );
                return None;
            }
        }
    }
    // The NonEmpty open table (`t = { + K_t => V_t, * K_r => V_r }`): its `new` takes the first typed
    // entry, so the BASELINE already carries one — the min-1 bound is unbreakable by construction and
    // needs no separate case. A `@custom_wire_major` key whose codec owns the wire is skipped for the
    // whole record rather than for the row: unlike the unbounded flavor there is no
    // empty-typed-region baseline left to round-trip, and a minted value could land on the wrong side
    // of the dispatch (the mint artifact the row-level exclusion below describes).
    if let Some(typed) = record
        .typed_row()
        .filter(|_| record.is_non_empty_open_table())
    {
        if type_uses_custom_ser(
            types,
            typed.domain(),
            &mut std::collections::BTreeSet::new(),
        ) {
            crate::warn!(
                "cddl-codegen --emit-tests: no round-trip for {name} (NonEmpty typed key is written by a custom codec)"
            );
            return None;
        }
        match (
            valid_value(types, typed.domain()),
            valid_value(types, typed.range()),
        ) {
            (Some(k), Some(v)) => {
                valid_args.push(k);
                valid_args.push(v);
            }
            _ => {
                crate::warn!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (NonEmpty typed row not cheaply mintable)"
                );
                return None;
            }
        }
    }
    // Every other restricted dynamic map row is a complete checked constructor argument.  Its
    // `MintValue::Map` crosses Bounded*/NonEmpty*'s one TryFrom door, so the baseline is valid by
    // construction and actually exercises the row rather than emitting an arity-invalid `new()`.
    for rest in record.captured_dynamic_rows().filter(|row| {
        !row.is_array_tail()
            && (row.is_restricted()
                || (record.has_forbidden_fields() && record.has_protected_rest_keys(types)))
            && !(record.is_typed_row(row) && record.is_non_empty_open_table())
    }) {
        match mint_dynamic_map_row(types, record, rest, 0) {
            Some(value) => valid_args.push(value),
            None => {
                crate::warn!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (checked dynamic map row {} not cheaply mintable)",
                    rest.field_name
                );
                return None;
            }
        }
    }
    // A one-or-more array tail has the same valid-by-construction constructor door as its public
    // record API: the first captured element is a constructor argument, not a later `Vec::push`.
    // Keep it after the fixed fields (and the open-table door, if present) in exact emitter order.
    for rest in record
        .captured_dynamic_rows()
        .filter(|row| row.is_non_empty_array_tail())
    {
        match valid_value(types, rest.element()) {
            Some(first) => valid_args.push(first),
            None => {
                crate::warn!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (NonEmpty rest tail not cheaply mintable)"
                );
                return None;
            }
        }
    }
    // Bounded array tails enter `new` as a full checked carrier, so emitted mints must construct
    // that carrier rather than omit the argument or mutate a default Vec after construction.
    for rest in record
        .captured_dynamic_rows()
        .filter(|row| row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail())
    {
        match valid_value(types, &rest.container_type()) {
            Some(carrier) => valid_args.push(carrier),
            None => {
                crate::warn!(
                    "cddl-codegen --emit-tests: no round-trip for {name} (bounded rest tail not cheaply mintable)"
                );
                return None;
            }
        }
    }
    // Multiple exact array segments have a source-ordered native constructor.  The established
    // one-segment emitter builds fixed arguments before the tail, so rebuild only this new shape's
    // mint list with source indices instead of pairing a correct type list with stale values.
    if record.rep == Representation::Array && !record.array_segments.is_empty() {
        let mut ordered: Vec<(usize, MintValue)> = record
            .fields
            .iter()
            .filter(|field| {
                !field.optional
                    && !field.rust_type.is_fixed_value()
                    && field.rust_type.config.default.is_none()
            })
            .map(|field| {
                (
                    field.source_index,
                    valid_value(types, &field.rust_type)
                        .expect("already validated mandatory field mint"),
                )
            })
            .chain(
                record
                    .captured_dynamic_rows()
                    .filter(|row| row.is_array_tail() && row.is_restricted())
                    .map(|row| {
                        (
                            row.array_source_index()
                                .expect("array segment has a source index"),
                            valid_value(types, &row.container_type())
                                .expect("already validated exact segment mint"),
                        )
                    }),
            )
            .collect();
        ordered.sort_by_key(|(source_index, _)| *source_index);
        valid_args = ordered.into_iter().map(|(_, value)| value).collect();
    }
    let base = render_rust(&MintValue::Record {
        ident: name.to_owned(),
        args: valid_args,
        can_fail: record_ctor_can_fail(record, types),
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
            None => crate::warn!(
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
    // Open struct-map (rest row): mint ONE captured entry through the generated `.rest` map API
    // (`insert`, uniform across the BTreeMap / OrderedHashMap / PairMap flavors) so the round-trip
    // loop actually serializes and re-reads rest content — the rest row is excluded from `new()`
    // (defaults empty), so this is the only path that populates it. Under `--preserve-encodings` the
    // encoding-fidelity mutator then exercises the header/width classes over the captured entry's
    // bytes, and — when the rest RANGE is `any` — the `widen_float` class over the float head the
    // `MintValue::Any` composite carries (the rest-position twin of `emit_tests_any_float_execute`'s
    // member-position float mint). Both key (domain) and value (range) are minted via `valid_value`;
    // a domain/range not cheaply mintable skips the rest case, leaving the baseline empty-rest case
    // (which still round-trips: empty rest ≡ closed-struct bytes). CAPTURE only: an `@ignore`
    // (tolerate-and-drop) row exposes no `.rest` field to mint into — the minted value carries only
    // declared fields, and round-trips trivially (no unknown entries exist in generated-API mint).
    //
    // BOTH dynamic rows, so an open table's TYPED row is minted the same way its catch-all is —
    // otherwise every round-trip of the shape the feature exists for would carry an empty typed
    // region, i.e. would never execute the wire-major dispatch at all. Each row also contributes to
    // a COMBINED case below, which is the one that exercises what two dynamic sequences add: the
    // tagged order encoding and (under canonical) the key merge spanning both regions.
    //
    // One exclusion, for the typed row only: a `@custom_wire_major` key whose codec owns the wire
    // writes bytes of the DECLARED major, which need not be the major the key's rust type would
    // naturally write. A value minted from the type could therefore land on the wrong side of the
    // dispatch and read back into the catch-all — a mint artifact reported as a round-trip failure.
    // Those specs have the acceptance corpus as their oracle; here the row is skipped loudly.
    let mut per_row_entry_mints: Vec<String> = Vec::new();
    for rest in record.captured_dynamic_rows() {
        let typed = record.is_typed_row(rest);
        match &rest.kind {
            // Map `* k => v` rest row: mint one entry via the row's map `insert` API.
            crate::intermediate::RestKind::MapEntries { domain, range, .. } => {
                // An exact occurrence row already enters the baseline as its complete checked
                // carrier. Adding one more member through the generic per-row mutation is
                // necessarily invalid (and can reintroduce a fixed-key collision); the baseline
                // is the present-row execution case for this shape.
                if rest.has_exact_occurrence_window() {
                    continue;
                }
                if typed
                    && type_uses_custom_ser(types, domain, &mut std::collections::BTreeSet::new())
                {
                    crate::warn!(
                        "cddl-codegen --emit-tests: {name} typed row's key is written by a custom codec — round-trip covers an empty typed row only"
                    );
                    continue;
                }
                match (valid_value(types, domain), valid_value(types, range)) {
                    (Some(k), Some(v)) => {
                        let mint = if record.has_protected_rest_keys(types) && !rest.is_array_tail()
                        {
                            // A possible fixed/rest collision makes the carrier private. Re-enter
                            // the record's checked insertion door, which composes declared/forbidden
                            // validation with the carrier's own cardinality/duplicate semantics.
                            format!(
                                "v.insert_{}({}, {}).unwrap();",
                                rest.field_name,
                                render_rust(&k),
                                render_rust(&v)
                            )
                        } else {
                            format!(
                                "v.{}.insert({}, {});",
                                rest.field_name,
                                render_rust(&k),
                                render_rust(&v)
                            )
                        };
                        cases.push((
                            format!("{{ let mut v = {base}; {mint} v }}"),
                            if typed {
                                "typed row entry present".to_owned()
                            } else {
                                "rest entry present".to_owned()
                            },
                        ));
                        per_row_entry_mints.push(mint);
                    }
                    _ => {
                        if typed {
                            crate::warn!(
                                "cddl-codegen --emit-tests: {name} typed row not cheaply mintable — round-trip covers an empty typed row only"
                            )
                        } else {
                            crate::warn!(
                                "cddl-codegen --emit-tests: {name} rest row not cheaply mintable — round-trip covers empty rest only"
                            )
                        }
                    }
                }
            }
            // Array rest tail: a loose `* t` starts empty, while a `+ t` baseline already holds its
            // first element through `new`; both gain one more element here through the non-shrinking
            // `.push` API so serialization/deserialization executes the tail loop.
            crate::intermediate::RestKind::ArrayTail { element, .. } => {
                match valid_value(types, element) {
                    Some(e) => cases.push((
                        format!(
                            "{{ let mut v = {base}; v.{}.push({}); v }}",
                            rest.field_name,
                            render_rust(&e)
                        ),
                        "rest tail element present".to_owned(),
                    )),
                    None => crate::warn!(
                        "cddl-codegen --emit-tests: {name} rest tail not cheaply mintable — round-trip covers empty tail only"
                    ),
                }
            }
        }
    }
    // The open table's discriminating case: BOTH regions populated at once. Only this one puts two
    // dynamic sequences on the wire in one value, which is what the tagged order encoding exists for
    // — and, under `--canonical-form`, what the key merge spanning both regions has to sort. Emitted
    // only when both rows minted (a skipped row leaves its own single-region case standing).
    if record.is_open_table() && per_row_entry_mints.len() == 2 {
        cases.push((
            format!(
                "{{ let mut v = {base}; {} v }}",
                per_row_entry_mints.join(" ")
            ),
            "both rows populated".to_owned(),
        ));
    }
    roundtrip_body(name, cases, conf, dump_rule, rt, None)
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
    let mut minted = Vec::new();
    for (variant_idx, variant) in variants.iter().enumerate() {
        let ctor = format!("new_{}", variant.name_as_var());
        let Some(arg_fields) = variant_arg_fields(types, variant, group_choice) else {
            crate::warn!(
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
                    crate::warn!(
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
        minted.push(variant_idx);
    }
    // Every variant needs an arm (the match must be exhaustive), including ones that minted no case.
    // `Variant { .. }` is the one pattern form that covers all three shapes an emitted variant takes
    // — unit, newtype-tuple, and named-field (the multi-field group-choice arm).
    let arms = variants
        .iter()
        .enumerate()
        .map(|(i, v)| format!("\n            {name}::{} {{ .. }} => {i}usize,", v.name))
        .collect::<String>();
    roundtrip_body(
        name,
        cases,
        conf,
        dump_rule,
        rt,
        Some(&FirstMatch { arms, minted }),
    )
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
                value: valid_float_in_window_of_class(&window, float_class_of(wrapped)),
                is_f32: float_is_f32(wrapped),
            }),
            None => match min_max {
                Some(mm) => materialize(types, wrapped, wrapper_measure(wrapped, mm)),
                None => valid_value(types, wrapped),
            },
        },
    };
    let Some(inner) = inner else {
        crate::warn!(
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
        None,
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

/// Render the loose-to-tight door for a bounded-array mint, leaving its `Result` observable.
/// `render_rust` normally unwraps this door because it produces a valid constructor argument; the
/// boundary probes need to assert on the door itself instead.
fn render_bounded_array_try_from(mv: &MintValue) -> Option<String> {
    let MintValue::Array {
        elem: Some(elem),
        count,
        bounded: Some((min, max)),
        reject,
        ..
    } = mv
    else {
        return None;
    };
    let elems = if *reject {
        match mv {
            MintValue::Array {
                unique_elems: Some(elems),
                ..
            } => elems,
            _ => return None,
        }
    } else {
        return Some(format!(
            "BoundedVec::<_, {min}, {max}>::try_from(vec![{}; {count}])",
            render_rust(elem)
        ));
    };
    Some(format!(
        "BoundedOrderedSet::<_, {min}, {max}>::try_from(vec![{}])",
        elems.iter().map(render_rust).collect::<Vec<_>>().join(", ")
    ))
}

/// Boundary probes for constructor arguments whose cardinality is already enforced by their
/// `BoundedVec` type. Accepted values cross that type's `TryFrom<Vec<_>>` door before reaching the
/// outer constructor; rejected values never can reach the outer constructor at all.
fn type_enforced_bounded_array_ctor_probes(
    types: &IntermediateTypes,
    ctor: &str,
    arg_types: &[&RustType],
    ctor_can_fail: bool,
) -> Vec<String> {
    let mut blocks = Vec::new();
    for (target, arg_ty) in arg_types.iter().enumerate() {
        if !arg_ty.is_type_enforced_bounded_array() {
            continue;
        }
        let Some(bounds) = arg_ty.config.bounds else {
            continue;
        };
        for (mv, accept, label) in bound_cases(types, arg_ty, bounds, true) {
            let Some(door) = render_bounded_array_try_from(&mv) else {
                continue;
            };
            if accept {
                let mut args = Vec::new();
                let mut mintable = true;
                for (i, ty) in arg_types.iter().enumerate() {
                    if i == target {
                        args.push("__bounded_arg".to_owned());
                    } else if let Some(value) = valid_value(types, ty) {
                        args.push(render_rust(&value));
                    } else {
                        mintable = false;
                        break;
                    }
                }
                if mintable {
                    let call = format!("{ctor}({})", args.join(", "));
                    let call = if ctor_can_fail {
                        format!("{call}.expect(\"{ctor} {label} must be accepted\")")
                    } else {
                        call
                    };
                    blocks.push(format!(
                        "    {{
        let __bounded_arg = {door}.expect(\"{ctor} argument {label} must be accepted\");
        let _ = {call};
    }}"
                    ));
                }
            } else {
                blocks.push(format!(
                    "    assert!(matches!({door}.unwrap_err().failure(), __CddlTestDeserializeFailure::RangeCheck {{ .. }}), \"{ctor} argument {label} must be rejected as RangeCheck\");"
                ));
            }
        }
    }
    blocks
}

/// deser-reject for a struct: for each cheaply-mutatable bounded field, mint a valid baseline,
/// mutate that one field out of bounds, and assert the wire path rejects it as `RangeCheck`.
fn record_deser_reject(
    types: &IntermediateTypes,
    name: &str,
    record: &RustRecord,
    value_eq: bool,
) -> Option<String> {
    let ctor_arg_types = record_ctor_arg_types(record, types);
    let ctor_arg_type_refs: Vec<&RustType> = ctor_arg_types.iter().collect();
    let mut bounded_array_probes = type_enforced_bounded_array_ctor_probes(
        types,
        &format!("{name}::new"),
        &ctor_arg_type_refs,
        record_ctor_can_fail(record, types),
    );

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
            crate::warn!(
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
                && !f.rust_type.is_type_enforced_non_empty()
                && !f.rust_type.is_type_enforced_bounded_array()
                && !f.rust_type.is_type_enforced_bounded_map())
                || f.rust_type.config.float_bounds.is_some()
        })
        .collect();
    if targets.is_empty() {
        return (!bounded_array_probes.is_empty()).then(|| bounded_array_probes.join("\n"));
    }

    // Valid baseline args for every constructor input (fixed fields plus any checked dynamic map
    // row); bail the whole type if any isn't mintable.
    let mut valid_args: Vec<String> = Vec::new();
    for ty in &ctor_arg_types {
        match valid_value(types, ty) {
            Some(v) => valid_args.push(render_rust(&v)),
            None => {
                crate::warn!(
                    "cddl-codegen --emit-tests: skipped {name} (constructor argument not cheaply mintable)"
                );
                return (!bounded_array_probes.is_empty()).then(|| bounded_array_probes.join("\n"));
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
                float_bound_cases(
                    window,
                    float_is_f32(&target.rust_type),
                    float_class_of(&target.rust_type)?,
                ),
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
        return (!bounded_array_probes.is_empty()).then(|| bounded_array_probes.join("\n"));
    }
    bounded_array_probes.push(format!(
        "    let mk = || {baseline};\n{}",
        blocks.join("\n")
    ));
    Some(bounded_array_probes.join("\n"))
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

        let arg_types: Vec<&RustType> = arg_fields.iter().map(|(ty, _)| *ty).collect();
        lines.extend(type_enforced_bounded_array_ctor_probes(
            types,
            &format!("{name}::{ctor}"),
            &arg_types,
            arg_types.iter().any(|ty| arg_can_fail(types, ty)),
        ));

        // which arg (if any) carries a cheaply-testable bound?
        for (i, (arg_ty, _)) in arg_fields.iter().enumerate() {
            if !arg_can_fail(types, arg_ty) {
                continue;
            }
            let (cases, failure) = if let Some(window) = &arg_ty.config.float_bounds {
                (
                    float_bound_cases(window, float_is_f32(arg_ty), float_class_of(arg_ty)?),
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
                if arg_ty.is_type_enforced_non_empty() || arg_ty.is_type_enforced_bounded_map() {
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
    let cases = float_bound_cases(window, float_is_f32(wrapped), float_class_of(wrapped)?);
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
            Primitive::N64
            | Primitive::Bool
            | Primitive::Float
            | Primitive::F16
            | Primitive::F32
            | Primitive::F64
            | Primitive::F16To32
            | Primitive::F32To64 => None,
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

/// Rank of a float class-window endpoint (`float_class_window`'s `cbor_event::Sz` spelling) in width
/// order. Derived from that one table rather than restating it, so the six classes have a single
/// definition here as they do in the runtime.
fn float_width_rank(name: &str) -> u8 {
    match name {
        "Two" => 0,
        "Four" => 1,
        _ => 2,
    }
}

/// Whether `value` is a MEMBER of the CDDL float class `p` names. The six names partition the float
/// values by shortest lossless form, so membership is `smallest_float_sz(value)` landing inside the
/// class's window — the same predicate the emitted code enforces on both sides
/// (`static/serialization.rs`, `float_class_width`). A mint MUST satisfy it: serializing a
/// non-member fails loudly by design, so a non-member baseline would make every generated
/// round-trip for that type fail.
fn float_class_admits(p: Primitive, value: f64) -> bool {
    let Some((min, max)) = p.float_class_window() else {
        return false;
    };
    let smallest = match cbor_event::se::smallest_float_sz(value) {
        cbor_event::Sz::Two => 0,
        cbor_event::Sz::Four => 1,
        _ => 2,
    };
    smallest >= float_width_rank(min) && smallest <= float_width_rank(max)
}

/// A member of `p`'s float class near `base`, in preference order: `base` itself, then the nearest
/// values above and below it that a wider class needs. A class whose narrowest admitted width is
/// `fa`/`fb` excludes every value a narrower head represents, so an arbitrary decimal (a window
/// midpoint, `0.0`) is typically NOT a member of it; perturbing the lowest mantissa bit of the f32
/// (resp. f64) image of `base` lands on a value that needs exactly that width and sits as close to
/// `base` as the format allows — close enough to stay inside any non-degenerate bounds window.
///
/// Both directions are offered because the bounds window may be one-sided: a `.le`-bounded class
/// needs the candidate below `base`, a `.ge`-bounded one above it.
fn float_class_candidates(p: Primitive, base: f64) -> Vec<f64> {
    let mut out = vec![base];
    // f32-exact neighbours: an f32 whose lowest mantissa bit differs from `base`'s image needs the
    // full binary32 mantissa, so its shortest form is `fa` (never `f9`).
    let b32 = (base as f32).to_bits();
    for bits in [b32.wrapping_sub(1), b32.wrapping_add(1)] {
        out.push(cbor_event::se::f32_to_f64_exact(f32::from_bits(bits)));
    }
    // f64-exact neighbours: same argument one width up — the lowest binary64 mantissa bit set means
    // no narrower head represents the value, so the shortest form is `fb`.
    let b64 = base.to_bits();
    for bits in [b64.wrapping_sub(1), b64.wrapping_add(1)] {
        out.push(f64::from_bits(bits));
    }
    // Last resort when `base` is degenerate (0.0, an infinity): one known member per width.
    out.extend_from_slice(&[0.0, 1e10, 1.1]);
    out.retain(|v| v.is_finite() && float_class_admits(p, *v));
    out
}

/// The unbounded baseline for a float of class `p`: one readable literal per width, the narrowest
/// the class admits. `float16`/`float16-32`/`float` all admit `0.0` (its shortest form is `f9`);
/// `float32`/`float32-64` take `1e10` (binary32-exact, far outside binary16's range, so `fa`); and
/// `float64` takes `1.1` (it needs the full binary64 mantissa, so `fb`).
fn float_class_baseline(p: Primitive) -> Option<f64> {
    [0.0, 1e10, 1.1]
        .into_iter()
        .find(|v| float_class_admits(p, *v))
}

/// [`valid_float_in_window`] narrowed to a MEMBER of `p`'s float class — the two constraints a
/// bounded declared-width float must satisfy at once. Falls back to the plain interior point when no
/// candidate satisfies both, which means the CDDL type is uninhabited (`float64 .eq 3.5`: the only
/// value the bound admits has shortest form `f9`, so no `float64` value satisfies it) and the
/// emitted round-trip is expected to say so loudly rather than silently mint something else.
fn valid_float_in_window_of_class(
    window: &crate::intermediate::FloatWindow,
    class: Option<Primitive>,
) -> f64 {
    let base = valid_float_in_window(window);
    let Some(p) = class else { return base };
    let in_window = |v: f64| {
        window
            .0
            .is_none_or(|(lo, excl)| if excl { v > lo } else { v >= lo })
            && window
                .1
                .is_none_or(|(hi, excl)| if excl { v < hi } else { v <= hi })
    };
    float_class_candidates(p, base)
        .into_iter()
        .find(|v| in_window(*v))
        .unwrap_or(base)
}

/// The float class a type names, or `None` when it is not a float.
fn float_class_of(ty: &RustType) -> Option<Primitive> {
    match ty.resolve_alias_shallow() {
        ConceptualRustType::Primitive(p) if p.is_float() => Some(*p),
        _ => None,
    }
}

/// Accept/reject boundary cases for a float window: `(value, accept, label)`. Always includes the
/// out-of-window rejects (below min / above max with a unit of margin) and a NaN reject, plus an
/// interior accept. For an f64 window (exact representation) it also pins each endpoint — included
/// endpoints accept, excluded endpoints reject. f32 windows skip the exact-endpoint cases (an f32
/// value cast back to f64 need not equal the authored decimal), keeping only the margin/NaN cases.
///
/// Every case is also a MEMBER of the field's float CLASS, because the reject cases are asserted
/// through the WIRE (mint the out-of-bounds value, serialize, expect the decode to reject it) and a
/// non-member cannot be serialized at all. The margin cases are therefore taken near the margin
/// rather than exactly a unit past it, and a case whose value the class excludes outright — an
/// endpoint the class cannot represent, or NaN for any class narrower than `float16` — is DROPPED:
/// a `float64` field has no NaN, since the canonical quiet NaN's shortest lossless form is `f9`.
fn float_bound_cases(
    window: &crate::intermediate::FloatWindow,
    is_f32: bool,
    class: Primitive,
) -> Vec<(MintValue, bool, &'static str)> {
    let lit = |v: f64| MintValue::FloatLit { value: v, is_f32 };
    // a member of `class` near `base` that still satisfies what the case is testing
    let member = |base: f64, pred: &dyn Fn(f64) -> bool| {
        float_class_candidates(class, base)
            .into_iter()
            .find(|v| pred(*v))
    };
    let mut out = Vec::new();
    // interior accept
    out.push((
        lit(valid_float_in_window_of_class(window, Some(class))),
        true,
        "in-window",
    ));
    if let Some((lo, exclusive)) = window.0 {
        if let Some(v) = member(lo - 1.0, &|v| v < lo) {
            out.push((lit(v), false, "below min"));
        }
        if !is_f32 && float_class_admits(class, lo) {
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
        if let Some(v) = member(hi + 1.0, &|v| v > hi) {
            out.push((lit(v), false, "above max"));
        }
        if !is_f32 && float_class_admits(class, hi) {
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
    // NaN must always be rejected by the NaN-safe accept-form check — where the class has one
    if float_class_admits(class, f64::NAN) {
        out.push((lit(f64::NAN), false, "NaN"));
    }
    out
}

/// Whether a float primitive is f32 (its window value is stored as f64 but compared/minted as f32).
fn float_is_f32(ty: &RustType) -> bool {
    matches!(
        ty.resolve_alias_shallow(),
        ConceptualRustType::Primitive(p) if p.float_carrier_is_f32()
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
        | Primitive::Float
        | Primitive::F16
        | Primitive::F32
        | Primitive::F64
        | Primitive::F16To32
        | Primitive::F32To64
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
        // A minted float must be a MEMBER of its CDDL class (serializing a non-member fails loudly
        // by design) and, when bounded, must also sit IN-WINDOW — a default `0.0` satisfies neither
        // in general. `float` alone admits every value, so it keeps the `0.0` baseline.
        ConceptualRustType::Primitive(p) if p.is_float() => match &ty.config.float_bounds {
            Some(window) => Some(MintValue::FloatLit {
                value: valid_float_in_window_of_class(window, Some(*p)),
                is_f32: float_is_f32(ty),
            }),
            None if *p == Primitive::Float => Some(MintValue::Float),
            None => Some(MintValue::FloatLit {
                value: float_class_baseline(*p)?,
                is_f32: float_is_f32(ty),
            }),
        },
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
        // CDDL `any` (`AnyCbor`): a fixed composite carrying a float head (see `MintValue::Any`). The
        // rendered constructor calls go through the `__AnyCborMint` import-glued alias, so this is
        // mode-agnostic (both preserve/non-preserve build the same via mode-paired `new_*` ctors).
        ConceptualRustType::Any => Some(MintValue::Any),
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
                    crate::warn!(
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
            if let Some(slots) = multi_exact_array_ctor_arg_slots(record) {
                let args = slots
                    .iter()
                    .map(|(_, ty)| valid_value_at(types, ty, depth + 1))
                    .collect::<Option<_>>()?;
                return Some(MintValue::Record {
                    ident: name,
                    args,
                    can_fail: record_ctor_can_fail(record, types),
                });
            }
            let ctor_fields: Vec<&RustField> = record
                .fields
                .iter()
                .filter(|f| {
                    !f.optional
                        && !f.rust_type.is_fixed_value()
                        && f.rust_type.config.default.is_none()
                })
                .collect();
            let mut args: Vec<MintValue> = ctor_fields
                .iter()
                .map(|f| valid_value_at(types, &f.rust_type, depth + 1))
                .collect::<Option<_>>()?;
            // The NonEmpty open table's door takes the first typed entry (see `record_roundtrip`);
            // a nested mint of one owes those two arguments exactly as the top-level baseline does.
            if let Some(typed) = record
                .typed_row()
                .filter(|_| record.is_non_empty_open_table())
            {
                args.push(valid_value_at(types, typed.domain(), depth + 1)?);
                args.push(valid_value_at(types, typed.range(), depth + 1)?);
            }
            // The other restricted map rows are full checked-carrier constructor arguments, just
            // as at the top-level round-trip mint. This is needed for nested records too: leaving
            // one out produces an arity-invalid `new()` only after the enclosing test is emitted.
            for rest in record.captured_dynamic_rows().filter(|row| {
                !row.is_array_tail()
                    && (row.is_restricted()
                        || (record.has_forbidden_fields() && record.has_protected_rest_keys(types)))
                    && !(record.is_typed_row(row) && record.is_non_empty_open_table())
            }) {
                args.push(mint_dynamic_map_row(types, record, rest, depth + 1)?);
            }
            // The one-or-more array tail's first element is a real constructor argument, just as it
            // is at the top-level `record_roundtrip` call. Without it nested record mints call
            // `new` with a missing argument and fail to compile.
            for rest in record
                .captured_dynamic_rows()
                .filter(|row| row.is_non_empty_array_tail())
            {
                args.push(valid_value_at(types, rest.element(), depth + 1)?);
            }
            for rest in record.captured_dynamic_rows().filter(|row| {
                row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail()
            }) {
                args.push(valid_value_at(types, &rest.container_type(), depth + 1)?);
            }
            Some(MintValue::Record {
                ident: name,
                args,
                can_fail: record_ctor_can_fail(record, types),
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
                    // a bounded float wrapper mints an inner that is both in-window and a member of
                    // its float class (`0.0` is generally neither)
                    Some(window) => MintValue::FloatLit {
                        value: valid_float_in_window_of_class(window, float_class_of(wrapped)),
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
        // named tables mint empty (one-entry minting needs the struct's insert API);
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
            let count = valid_measure(bounds.unwrap_or((None, None)));
            let reject = rust_struct.config().duplicates
                == Some(crate::comment_ast::DuplicatesPolicy::Reject);
            let unique_elems = if reject {
                unique_array_elems(types, element_type, count, depth + 1)?
            } else {
                None
            };
            Some(MintValue::Array {
                elem: unique_elems
                    .as_ref()
                    .and_then(|elems| elems.first().cloned())
                    .map(Box::new)
                    .or_else(|| valid_value_at(types, element_type, depth + 1).map(Box::new)),
                count,
                non_empty: *bounds == Some((Some(1), None)),
                bounded: bounds.and_then(|(min, max)| {
                    ((min, max) != (None, None) && (min, max) != (Some(1), None)).then_some((
                        u64::try_from(min.unwrap_or(0)).ok()?,
                        max.map(|v| u64::try_from(v).ok())
                            .unwrap_or(Some(u64::MAX))?,
                    ))
                }),
                reject,
                unique_elems,
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
            bounded: None,
            reject: ty.config.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject),
            unique_elems: None,
        }),
        ConceptualRustType::Map(_, _) => Some(MintValue::DefaultMap),
        _ => None,
    }
}

/// Build distinct, cheaply-renderable members for a reject-set mint.  This is deliberately bounded:
/// emitted tests skip a collection whose element has no finite distinct mint surface rather than
/// fabricate an invalid set with repeated copies.  The normal corpus windows are tiny; a larger
/// window is still covered by its runtime/decoder doors and receives the existing loud mint skip.
fn unique_array_elems(
    types: &IntermediateTypes,
    elem: &RustType,
    count: i128,
    depth: u8,
) -> Option<Option<Vec<MintValue>>> {
    if !(0..=16).contains(&count) {
        return None;
    }
    // The empty array is already a distinct, valid reject-set mint.  Do not enter the candidate
    // loop: it can only return after adding an element, so it could never satisfy `len() == 0`.
    if count == 0 {
        return Some(Some(Vec::new()));
    }
    let mut elems = Vec::new();
    let mut rendered = Vec::new();
    for candidate in 0..(count.saturating_mul(8).saturating_add(16)) {
        let value = materialize_at(types, elem, candidate, depth + 1)?;
        let spelling = render_rust(&value);
        if rendered.iter().any(|prior| prior == &spelling) {
            continue;
        }
        rendered.push(spelling);
        elems.push(value);
        if elems.len() == count as usize {
            return Some(Some(elems));
        }
    }
    None
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
            Primitive::Float
            | Primitive::F16
            | Primitive::F32
            | Primitive::F64
            | Primitive::F16To32
            | Primitive::F32To64 => match &ty.config.float_bounds {
                Some(window) => Some(MintValue::FloatLit {
                    value: valid_float_in_window_of_class(window, Some(*p)),
                    is_f32: p.float_carrier_is_f32(),
                }),
                None if *p == Primitive::Float => Some(MintValue::Float),
                None => Some(MintValue::FloatLit {
                    value: float_class_baseline(*p)?,
                    is_f32: p.float_carrier_is_f32(),
                }),
            },
        },
        ConceptualRustType::Array(elem) => {
            let reject = ty.config.duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject);
            let unique_elems = if reject {
                unique_array_elems(types, elem, measure, depth)?
            } else {
                None
            };
            let e = unique_elems
                .as_ref()
                .and_then(|elems| elems.first().cloned())
                .or_else(|| valid_value_at(types, elem, depth))?;
            Some(MintValue::Array {
                elem: Some(Box::new(e)),
                count: measure,
                non_empty: ty.is_type_enforced_non_empty(),
                bounded: ty.type_enforced_bounded_array_u64_bounds(),
                reject,
                unique_elems,
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
                    crate::warn!(
                        "cddl-codegen --emit-tests: map key {other:?} not cheaply mintable — the map's key wire path is unexercised"
                    );
                    return None;
                }
            };
            let key_base = map_key_base(&key, k, measure)?;
            let val = valid_value_at(types, v, depth)?;
            // distinct keys key_base..key_base+measure; collect() infers the map type from the
            // target position
            Some(MintValue::Map {
                key,
                key_base,
                val: Box::new(val),
                count: measure,
                non_empty: ty.is_type_enforced_non_empty(),
                bounded: ty.type_enforced_bounded_map_u64_bounds(),
                preserve: ty.config.duplicates
                    == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
            })
        }
        _ => None,
    }
}

/// The first synthesized map key, chosen so that every key in `key_base .. key_base + count` is a
/// value the emitted decoder ACCEPTS. A key DOMAIN can carry its own value window
/// (`{ * int .ne 0 => uint }`) and the renderers lay keys down consecutively from a base, so a
/// hardcoded base of `0` mints a key the (correct) generated bounds check rejects with
/// `RangeCheck` — a red round-trip vector blaming code that is behaving exactly as specified.
///
/// Acceptance is decided by `bounds_reject_value`, which shares `reject_cond` with the emitted
/// check, so the minter and the decoder can never disagree about what the window means (the `.ne N`
/// inverted-range encoding in particular is easy to re-derive backwards).
///
/// The base is chosen in the KEY'S OWN storage space, which for `nint` is not value space: an
/// `N64` key is stored as the u64 magnitude `m = |v + 1|`. `nint_bounds_to_u64` (which swaps
/// endpoints because magnitude decreases as the value increases) supplies cheap candidate starts,
/// while every candidate is separately projected back to its CDDL value before the original bounds
/// decide acceptance. This keeps a transformed search heuristic from becoming a second semantic
/// oracle alongside the generated decoder.
///
/// `None` means "skip this map loudly" — this module never silently weakens a vector.
fn map_key_base(key: &MapKey, key_ty: &RustType, count: i128) -> Option<i128> {
    let MapKey::Int(p) = key else {
        let Some(bounds) = key_ty.config.bounds else {
            return Some(0);
        };
        // A `.size`-bounded tstr/bytes key (or a bounded bool): the window constrains the key's
        // LENGTH (or nothing at all), which the `__i.to_string()` / `vec![__i as u8]` /
        // `__i == 1` spellings have no way to steer. Minting anyway emits a vector the generated
        // decoder rejects and reads as a decoder bug.
        crate::warn!(
            "cddl-codegen --emit-tests: map key carries bounds {bounds:?} that the {key:?} key spelling cannot honour — the map's key wire path is unexercised"
        );
        return None;
    };
    if count <= 0 {
        return Some(0);
    }
    // `N64` is the one key primitive whose stored coordinate is not the CDDL value. Transform
    // bounds only to choose cheap magnitude candidates; `map_key_run_is_accepted` independently
    // validates each rendered coordinate through its canonical value-space `FixedValue`.
    let selection_bounds = key_ty.config.bounds.map(|bounds| {
        if matches!(p, Primitive::N64) {
            crate::generation::nint_bounds_to_u64(&bounds)
        } else {
            bounds
        }
    });
    // Candidate bases in preference order: the window's lower endpoint (which for the INVERTED
    // `.ne N` encoding is `N + 1` — the first value above the exclusion), `0` (a window open on
    // the low side), and the highest run that still fits under the upper endpoint. Each is only a
    // heuristic; the acceptance loop below is what makes it safe.
    let candidates = [
        selection_bounds.and_then(|bounds| bounds.0),
        Some(0),
        selection_bounds.and_then(|bounds| bounds.1.and_then(|max| max.checked_sub(count - 1))),
    ];
    for base in candidates.into_iter().flatten() {
        if map_key_run_is_accepted(key, key_ty, count, base) {
            return Some(base);
        }
    }
    crate::warn!(
        "cddl-codegen --emit-tests: map key window {:?} has no run of {count} consecutive accepted {p:?} values — the map's key wire path is unexercised",
        key_ty.config.bounds
    );
    None
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minted_map_key_fixed_value_covers_storage_domains() {
        let cases = [
            (Primitive::U8, 0, FixedValue::Uint(0)),
            (
                Primitive::U8,
                u8::MAX as i128,
                FixedValue::Uint(u8::MAX as u64),
            ),
            (
                Primitive::U16,
                u16::MAX as i128,
                FixedValue::Uint(u16::MAX as u64),
            ),
            (
                Primitive::U32,
                u32::MAX as i128,
                FixedValue::Uint(u32::MAX as u64),
            ),
            (Primitive::U64, u64::MAX as i128, FixedValue::Uint(u64::MAX)),
            (
                Primitive::I8,
                i8::MIN as i128,
                FixedValue::Nint(i8::MIN as i128),
            ),
            (
                Primitive::I8,
                i8::MAX as i128,
                FixedValue::Uint(i8::MAX as u64),
            ),
            (
                Primitive::I16,
                i16::MIN as i128,
                FixedValue::Nint(i16::MIN as i128),
            ),
            (
                Primitive::I16,
                i16::MAX as i128,
                FixedValue::Uint(i16::MAX as u64),
            ),
            (
                Primitive::I32,
                i32::MIN as i128,
                FixedValue::Nint(i32::MIN as i128),
            ),
            (
                Primitive::I32,
                i32::MAX as i128,
                FixedValue::Uint(i32::MAX as u64),
            ),
            (
                Primitive::I64,
                i64::MIN as i128,
                FixedValue::Nint(i64::MIN as i128),
            ),
            (
                Primitive::I64,
                i64::MAX as i128,
                FixedValue::Uint(i64::MAX as u64),
            ),
            (Primitive::N64, 0, FixedValue::Nint(-1)),
            (
                Primitive::N64,
                u64::MAX as i128,
                FixedValue::Nint(-1 - u64::MAX as i128),
            ),
        ];
        for (primitive, coordinate, expected) in cases {
            assert_eq!(
                map_minted_key_fixed_value(&MapKey::Int(primitive), coordinate),
                Some(expected),
                "{primitive:?} coordinate {coordinate}",
            );
        }
        assert_eq!(
            map_minted_key_fixed_value(&MapKey::Str, -7),
            Some(FixedValue::Text("-7".to_owned()))
        );
        assert_eq!(
            map_minted_key_fixed_value(&MapKey::Bytes, u8::MAX as i128),
            Some(FixedValue::Bytes(vec![u8::MAX]))
        );
        assert_eq!(
            map_minted_key_fixed_value(&MapKey::Bool, 1),
            Some(FixedValue::Bool(true))
        );
    }

    #[test]
    fn minted_map_key_fixed_value_rejects_unrepresentable_coordinates() {
        for (key, coordinate) in [
            (MapKey::Int(Primitive::U8), -1),
            (MapKey::Int(Primitive::U8), u8::MAX as i128 + 1),
            (MapKey::Int(Primitive::I8), i8::MIN as i128 - 1),
            (MapKey::Int(Primitive::I8), i8::MAX as i128 + 1),
            (MapKey::Int(Primitive::N64), -1),
            (MapKey::Int(Primitive::N64), u64::MAX as i128 + 1),
            (MapKey::Bytes, -1),
            (MapKey::Bytes, u8::MAX as i128 + 1),
            (MapKey::Bool, -1),
        ] {
            assert_eq!(map_minted_key_fixed_value(&key, coordinate), None);
        }
    }

    #[test]
    fn nint_minted_magnitudes_have_canonical_fixed_value_equality() {
        assert!(!map_minted_key_equals_fixed(
            &MapKey::Int(Primitive::N64),
            0,
            0,
            &FixedValue::Uint(0),
        ));
        assert!(map_minted_key_equals_fixed(
            &MapKey::Int(Primitive::N64),
            0,
            0,
            &FixedValue::Nint(-1),
        ));
        assert!(map_minted_key_equals_fixed(
            &MapKey::Int(Primitive::U64),
            0,
            0,
            &FixedValue::Uint(0),
        ));
    }
}
