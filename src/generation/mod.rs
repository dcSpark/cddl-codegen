use crate::cli::Cli;
use crate::comment_ast::DemandSet;
use codegen::{Block, TypeAlias};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use crate::intermediate::{
    AliasIdent, CBOREncodingOperation, CDDLIdent, ConceptualRustType, EnumVariant, EnumVariantData,
    FixedValue, IntermediateTypes, ModuleScope, Primitive, ROOT_SCOPE, Representation, RustField,
    RustIdent, RustRecord, RustStructCBORLen, RustStructConfig, RustStructType, RustType,
    RustTypeSerializeConfig, ToWasmBoundaryOperations, VariantIdent, escape_rust_str,
};
use crate::utils::{cbor_type_code_str, convert_to_camel_case, convert_to_snake_case};

mod export;
use export::declare_modules;
// Re-exports keeping the pre-split paths (`generation::X`) resolving for callers outside this
// module: the public `rustfmt_generated_string` and the test-only helpers. None are used in the
// crate's non-test compilations, so the aliases read as unused there — allow the lint on them.
#[allow(unused_imports)]
pub use export::rustfmt_generated_string;
#[allow(unused_imports)]
pub(crate) use export::{
    CODEGEN_HEADER, concat_files, is_header_stamped_path, is_preservable_generated_path,
};

mod bounds;
use bounds::{
    CONVERT_ERR_TO_OURS, SignArm, SignArmBounds, bounds_check_expr, bounds_check_expr_non_negative,
    bounds_check_if_block, bounds_check_if_block_float, classify_sign_arm, float_fixed_literal,
    nint_arm_needs_width, non_preserve_bounds_fn, prim_window, primitive_non_negative,
    sign_arm_if_block, uint_arm_needs_width, upper_caps, value_bounds_check_line, width_reject,
};
pub(crate) use bounds::{bounds_check_expr_rust_type, nint_bounds_to_u64};

mod deserialize;
mod serialize;
use deserialize::{
    DeserializationCode, DeserializeBeforeAfter, DeserializeConfig,
    add_deserialize_final_len_check, add_deserialize_initial_len_check, create_deserialize_impls,
    make_deser_loop, make_deserialization_function, make_err_annotate_block,
};
use serialize::{
    EncodingVarIsCopy, SerializeConfig, SerializingRustType, create_serialize_impls, end_len,
    make_serialization_function, make_serialization_impl, start_len, write_string_sz,
    write_using_sz,
};

mod records;
use records::{
    codegen_struct, generate_array_struct_deserialization, generate_array_struct_serialization,
};

mod enums;
use enums::{
    codegen_group_choices, generate_c_style_enum, make_enum_variant_return_if_deserialized,
};

pub struct GenerationScope {
    rust_lib_scope: codegen::Scope,
    rust_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    rust_serialize_lib_scope: codegen::Scope,
    serialize_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    wasm_lib_scope: codegen::Scope,
    wasm_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    cbor_encodings_scopes: BTreeMap<ModuleScope, codegen::Scope>,
    json_lines: BlocksOrLines,
    already_generated: BTreeSet<RustIdent>,
    /// Every collection-wrapper CLASS the wasm crate actually minted this run, mapped to the
    /// `ModuleScope` it was emitted into. Recorded at the point of actual emission (inside each of
    /// the four wrapper emitters' `already_generated` success paths), so it equals EXACTLY the set
    /// of wrapper classes the crate owns — no more, no less. Materialized into
    /// `wasm/src/generated/collections.rs` (a `pub use` re-export index) by `generated_files`. A
    /// `BTreeMap` keeps the index deterministic (sorted by class name). Only populated under
    /// `--wasm`; unused otherwise.
    wasm_collection_wrappers: BTreeMap<RustIdent, ModuleScope>,
    /// Parsed `--extern-wrapper-index` inventories: extern-deps dependency name -> the set of
    /// collection-wrapper class names that dependency's own wasm crate already emits (read from its
    /// committed `generated/collections.rs`). Consulted when deciding whether a wrapper the consumer
    /// would mint should instead be deferred to the dependency. Empty unless the flag is passed.
    extern_wrapper_index: BTreeMap<String, BTreeSet<String>>,
    /// Collection wrappers the consumer is NOT minting this run because a mapped dependency already
    /// owns them (`--extern-wrapper-index`), keyed by the structural wrapper ident and mapped to the
    /// dependency's `collections` module scope (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/collections`,
    /// non-exported) the reference is imported from. Populated at each emitter's mint point during the
    /// wasm struct walk (before imports are computed), so `scope_references` can route a plain
    /// `use <dep_wasm>::collections::<Name>;` into every referencing module and the two keys()
    /// accessors know to construct via `.into()` cross-crate (R3d). Never records a wrapper into
    /// `wasm_collection_wrappers`, so a deferred wrapper stays out of the consumer's own index (R3e).
    deferred_wrappers: BTreeMap<RustIdent, ModuleScope>,
    /// Wrapper idents already named in a `--extern-wrapper-index` "candidate not in the dep's index"
    /// stderr warning, so the diagnostic fires at most once per wrapper across the walk.
    deferred_warned: BTreeSet<RustIdent>,
    /// Parsed `--workspace-dep` set (extern-deps directory names marked co-generated workspace
    /// members). A wrapper whose element types are ALL owned by one of these deps DEFERS
    /// UNCONDITIONALLY (no index consult) and is recorded in `borrowed_wrappers`. Empty unless the
    /// flag is passed; populated (and validated) in `generate()` under `--wasm` only.
    workspace_deps: BTreeSet<String>,
    /// Collection wrappers deferred to a workspace dep this run (`--workspace-dep`), keyed by the
    /// structural wrapper ident and mapped to `(dep rust-crate name, canonical CDDL shape)`. The
    /// mirror image of `wasm_collection_wrappers` ("what I provide" ↔ "what I borrow, from whom"),
    /// materialized into `wasm/src/generated/borrowed_collections.rs` for the dep's own generation to
    /// read. Recording is idempotent (the same wrapper is probed from several sites); two DISTINCT
    /// shapes deriving the SAME structural name is a hard error (the `MapAToBToC` reverse-ambiguity).
    borrowed_wrappers: BTreeMap<RustIdent, (String, String)>,
    /// W2 dep side (`--wrapper-requests`): the canonical CDDL shape (`render_wrapper_shape` output) of
    /// every collection wrapper this crate produces from its OWN spec, mapped to that wrapper's ident.
    /// Recorded at each emitter's actual mint point during the main walk (guarded off during requested
    /// emission). Answers "does the dep already produce this requested shape, and under what name?": a
    /// requested shape whose canonical form is a key here is own-spec-produced — satisfied when the
    /// ident is the structural name, a hard error when it is a different (rule-declared) name.
    own_wrapper_shapes: BTreeMap<String, RustIdent>,
    /// W2 dep side: while `Some`, `wasm()` / `record_collection_wrapper` route the wrapper being
    /// emitted into this scope (the `requested_collections` module) instead of `types.scope(ident)` —
    /// the requested wrappers are not in the dep's IR, so they have no natural scope. Set only around
    /// the requested-wrapper emission in `emit_requested_collections`; `None` everywhere else.
    requested_scope_override: Option<ModuleScope>,
    /// W2 dep side: attribution doc text (`Generated at the request of: …`) keyed by requested-wrapper
    /// ident. Consulted by `create_base_wasm_struct` (and prepended by the NonEmpty emitters, which set
    /// their own struct doc). Empty except during requested emission, so own-spec wrappers are
    /// unaffected (flag-off byte-identity).
    requested_attribution: BTreeMap<RustIdent, String>,
    /// W2 dep side: `true` when requested-wrapper emission produced a `[+ …]` / `{+ … => …}` wrapper
    /// whose NonEmpty runtime the dep's OWN spec does not otherwise pull in. The runtime-provisioning
    /// gates (`pub mod non_empty`/`non_empty_map` decl + static file copy) OR these in so the dep
    /// hosts a requested NonEmpty wrapper's `NonEmptyVec`/`NonEmptyMap` type. Never set off the flag.
    requested_non_empty_vec: bool,
    requested_non_empty_map: bool,
    no_deser_reasons: BTreeMap<RustIdent, Vec<String>>,
    /// Type-parameter names for the emitted `serialize` / `deserialize` fns. Normally `"W"` / `"R"`,
    /// but if a rule camel-cases to a type named `W`/`R` (which would shadow the generic and break
    /// compilation) these fall back to the first non-colliding candidate. Computed once in
    /// `generate()` from the ident set; see `pick_generic_name`.
    serialize_generic: String,
    deserialize_generic: String,
}

impl Default for GenerationScope {
    fn default() -> Self {
        Self::new()
    }
}

impl GenerationScope {
    pub fn new() -> Self {
        Self {
            rust_lib_scope: codegen::Scope::new(),
            rust_scopes: BTreeMap::new(),
            rust_serialize_lib_scope: codegen::Scope::new(),
            serialize_scopes: BTreeMap::new(),
            wasm_lib_scope: codegen::Scope::new(),
            wasm_scopes: BTreeMap::new(),
            cbor_encodings_scopes: BTreeMap::new(),
            json_lines: BlocksOrLines::default(),
            already_generated: BTreeSet::new(),
            wasm_collection_wrappers: BTreeMap::new(),
            extern_wrapper_index: BTreeMap::new(),
            deferred_wrappers: BTreeMap::new(),
            deferred_warned: BTreeSet::new(),
            workspace_deps: BTreeSet::new(),
            borrowed_wrappers: BTreeMap::new(),
            own_wrapper_shapes: BTreeMap::new(),
            requested_scope_override: None,
            requested_attribution: BTreeMap::new(),
            requested_non_empty_vec: false,
            requested_non_empty_map: false,
            no_deser_reasons: BTreeMap::new(),
            serialize_generic: "W".to_string(),
            deserialize_generic: "R".to_string(),
        }
    }

    /// Generates, i.e. populates the state, based on `types`.
    /// this does not create any files, call export() after.
    pub fn generate(&mut self, types: &IntermediateTypes, cli: &Cli) {
        // Pick collision-proof generic-parameter names for the emitted serialize/deserialize fns
        // BEFORE emitting anything: a rule named `w`/`r` camel-cases to a type `W`/`R` that would
        // shadow the hardcoded `fn serialize<'se, W: Write>` / `fn deserialize<R: BufRead + Seek>`
        // parameters, so we thread the chosen names through `make_{serialization,deserialization}_
        // function`. Depends only on the (deterministic) ident set, so output stays byte-identical:
        // with no collision these resolve to the defaults `"W"` / `"R"` and nothing churns.
        let defined_idents = types.defined_rust_idents();
        self.serialize_generic = pick_generic_name(&defined_idents, "W", "Ser");
        self.deserialize_generic = pick_generic_name(&defined_idents, "R", "De");

        // `--extern-wrapper-index`: read each mapped dependency's committed collection-wrapper index
        // (`generated/collections.rs`) so the wasm struct walk below can DEFER any wrapper the dep
        // already owns instead of re-minting it (a wasm duplicate-symbol link error otherwise).
        // Parsed once, up front, so it is available at every emitter's mint point. Only meaningful
        // under `--wasm`; a mapping naming a non-extern dependency is a hard error, mirroring
        // `--extern-wasm-crate` (a typo would otherwise silently disable deferral and reintroduce the
        // link error).
        if cli.wasm {
            self.extern_wrapper_index = load_extern_wrapper_indices(types, cli);
            self.workspace_deps = load_workspace_deps(types, cli);
        }

        // Type aliases
        for (alias_ident, alias_info) in types.type_aliases() {
            // only generate user-defined ones
            if let AliasIdent::Rust(ident) = alias_ident {
                // also make sure not to generate it if we instead generated a binary wrapper type
                if alias_info.gen_rust_alias
                    && !(cli.no_synthesized_rust_collection_aliases
                        && alias_info.synthesized_collection)
                {
                    let mut type_alias = TypeAlias::new(
                        ident,
                        alias_info.base_type.for_rust_member(types, false, cli),
                    );
                    type_alias.vis("pub");
                    // Decision 11 (two-type design doc): a named `[+ T]` rule's alias quotes the
                    // originating occurrence — the type name, doc comment, and TryFrom signature
                    // are three redundant discovery signals for the constraint.
                    if alias_info.base_type.is_non_empty_array()
                        && let ConceptualRustType::Array(elem) =
                            &alias_info.base_type.conceptual_type
                    {
                        type_alias.doc(format!(
                            "`[+ {}]`: at least one element, enforced at the `NonEmptyVec` \
                             `TryFrom<Vec<_>>` door (the CBOR decoder routes through the same \
                             door, so wire-side and API-side rejection are identical).",
                            elem.for_rust_member(types, false, cli)
                        ));
                    }
                    // map-side twin: a named `{+ k => v}` rule's alias quotes the occurrence too.
                    if alias_info.base_type.is_non_empty_map()
                        && let ConceptualRustType::Map(k, v) = &alias_info.base_type.conceptual_type
                    {
                        type_alias.doc(format!(
                            "`{{+ {} => {}}}`: at least one entry, enforced at the `NonEmptyMap` \
                             `TryFrom` door (the CBOR decoder routes through the same door, so \
                             wire-side and API-side rejection are identical).",
                            k.for_rust_member(types, false, cli),
                            v.for_rust_member(types, false, cli)
                        ));
                    }
                    self.rust(types, ident).push_type_alias(type_alias);
                }
                if alias_info.gen_wasm_alias {
                    // WASM crate
                    if let ConceptualRustType::Fixed(constant) =
                        &alias_info.base_type.conceptual_type
                    {
                        // wasm-bindgen doesn't support const or static vars so we must do a function
                        let (ty, val) = match constant {
                            FixedValue::Null => panic!("null constants not supported"),
                            FixedValue::Bool(b) => ("bool", b.to_string()),
                            FixedValue::Nint(i) => ("i32", i.to_string()),
                            FixedValue::Uint(u) => ("u32", u.to_string()),
                            // float_literal, not Display: a whole-valued f64 would render as an
                            // integer literal in the f64-returning wasm constant fn (E0308).
                            FixedValue::Float(f) => ("f64", float_fixed_literal(*f)),
                            FixedValue::Text(s) => {
                                ("String", format!("\"{}\".to_owned()", escape_rust_str(s)))
                            }
                        };
                        self.wasm(types, ident)
                            .new_fn(convert_to_snake_case(ident.as_ref()))
                            .attr("wasm_bindgen")
                            .vis("pub")
                            .ret(ty)
                            .line(val);
                    } else {
                        // A passthrough alias to a named collection (`ptm = mp`) is a transparent
                        // `pub type` in rust but a wrapper struct in wasm; point the wasm alias at that
                        // wrapper rather than `for_wasm_member`'s inline-only `MapU64To…` name. Only when
                        // the target is *not* directly-wasm-exposable, though: an exposable named array
                        // (`[* uint]`) also has a `Nums` wrapper struct, but the boundary code treats it
                        // transparently as `Vec<u64>`, so aliasing to the wrapper would desync (E0308).
                        // Maps are never directly exposable, so this covers `passthrumap` while leaving
                        // `passthru` (exposable arrays) on the transparent `for_wasm_member` path.
                        let wasm_target = alias_info
                            .wasm_alias_target
                            .as_ref()
                            .filter(|target| {
                                types.has_wasm_wrapper(target)
                                    && !alias_info.base_type.directly_wasm_exposable(types)
                            })
                            .map(|target| target.to_string())
                            .unwrap_or_else(|| alias_info.base_type.for_wasm_member(types));
                        self.wasm(types, ident)
                            .push_type_alias(TypeAlias::new(ident, wasm_target).vis("pub").clone());
                    }
                    // A type-alias BASE can carry an inline `[+ T]` / `{+ k => v}` shape that only
                    // this alias reaches — e.g. `x = bytes .cbor [+ uint]` classifies as a plain
                    // alias (not a `RustStructType::Array`), so the rust_structs minting walk below
                    // never visits it, while the wasm alias line above names the restricted wrapper
                    // (`pub type X = NonEmptyU64List;`). Mint the wrappers the base needs here; the
                    // dedup-to-named and `already_generated` guards inside apply as everywhere else,
                    // so a base whose shape a named rule owns dedups instead of double-minting.
                    // (Found by the recombination wasm sweep: rc1205's `NonEmptyU64List` was
                    // referenced but never emitted — E0425 with generation exit 0.)
                    if cli.wasm {
                        self.ensure_non_empty_wrappers(types, &alias_info.base_type, cli);
                    }
                }
            }
        }

        // Structs
        {
            // we can ignore types already handled by the alias
            // otherwise wasm_wrappers_generated may cause us to pointlessly create aliases to aliases
            let mut existing_aliases = types.type_aliases().iter().fold(
                BTreeSet::<RustIdent>::new(),
                |mut acc, (alias, _)| {
                    match alias {
                        AliasIdent::Reserved(_) => {}
                        AliasIdent::Rust(ident) => {
                            acc.insert(ident.clone());
                        }
                    };
                    acc
                },
            );

            // Shapes owned by EXACTLY ONE named table rule: their embedded/resolved uses share the
            // rule-named class (a real `#[wasm_bindgen]` class under the CDDL identifier), and the
            // structural `MapKToV` name becomes a `pub type` alias to it. Same-shape rule PAIRS (2+
            // owners) and anonymous-only shapes are absent — they keep the structural fallback class
            // at the crate root. Shared with `scope_references`'s Map arm (import placement) via the
            // one helper so emission and import placement CANNOT disagree.
            let table_shape_sole_owner = types.table_shape_sole_owners();

            let mut wasm_wrappers_generated = BTreeSet::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                assert_eq!(rust_ident, rust_struct.ident());
                if cli.wasm {
                    rust_struct.visit_types_excluding(
                        types,
                        &mut |ty| {
                            mint_wasm_wrapper_for_visited_type(
                                self,
                                types,
                                ty,
                                &mut wasm_wrappers_generated,
                                &table_shape_sole_owner,
                                cli,
                            )
                        },
                        &mut existing_aliases,
                    );
                    // The conceptual visitor above can't see array LENGTH bounds (they live on the
                    // RustType, stripped before it recurses), so mint the restricted `NonEmpty*List`
                    // wrappers for inline `[+ T]` shapes from a RustType-level walk that does.
                    match rust_struct.variant() {
                        RustStructType::Record(record) => {
                            for field in &record.fields {
                                self.ensure_non_empty_wrappers(types, &field.rust_type, cli);
                            }
                        }
                        RustStructType::Table { domain, range, .. } => {
                            // the named table's OWN restricted wrapper (`{+ k => v}`) is minted in
                            // the variant match below (under the rule ident); here just mint wrappers
                            // its domain/range need (nested `{+ …}` in a key or value position)
                            self.ensure_non_empty_wrappers(types, domain, cli);
                            self.ensure_non_empty_wrappers(types, range, cli);
                        }
                        RustStructType::Wrapper { wrapped, .. } => {
                            self.ensure_non_empty_wrappers(types, wrapped, cli);
                        }
                        RustStructType::GroupChoice { variants, .. }
                        | RustStructType::TypeChoice { variants } => {
                            for v in variants {
                                match &v.data {
                                    EnumVariantData::RustType(t) => {
                                        self.ensure_non_empty_wrappers(types, t, cli)
                                    }
                                    EnumVariantData::Inlined(rec) => {
                                        for f in &rec.fields {
                                            self.ensure_non_empty_wrappers(
                                                types,
                                                &f.rust_type,
                                                cli,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        RustStructType::Array { element_type, .. } => {
                            // the named rule's own wrapper is minted in the variant match below;
                            // here just mint wrappers its element needs (nested `[+ [+ int]]`)
                            self.ensure_non_empty_wrappers(types, element_type, cli);
                        }
                        _ => (),
                    }
                }
                match rust_struct.variant() {
                    RustStructType::Record(record) => {
                        codegen_struct(
                            self,
                            types,
                            rust_ident,
                            rust_struct.tag(),
                            record,
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::Table {
                        domain,
                        range,
                        bounds,
                    } => {
                        if cli.wasm && *bounds == Some((Some(1), None)) {
                            // named `{+ k => v}` rule: its JS class is the RESTRICTED wrapper
                            // (wrapping core::NonEmptyMap) under the rule ident, not the loose table
                            // wrapper — the map-side twin of the named `[+ T]` array arm.
                            self.generate_non_empty_map_type(
                                types,
                                domain.clone(),
                                range.clone(),
                                rust_ident,
                                true,
                                cli,
                            );
                        } else if cli.wasm {
                            let map_ident = ConceptualRustType::name_for_wasm_map(domain, range);
                            if table_shape_sole_owner.get(&map_ident.to_string())
                                == Some(rust_ident)
                            {
                                // Sole owner of this shape: emit the real JS class under the rule name
                                // plus the structural alias. Idempotent — the visit arm may have
                                // minted it already for an embedded/resolved use; either order
                                // converges to identical output.
                                mint_sole_owner_table(
                                    self,
                                    types,
                                    rust_ident,
                                    &map_ident,
                                    &mut wasm_wrappers_generated,
                                    cli,
                                );
                            } else if wasm_wrappers_generated.insert(rust_ident.to_string()) {
                                // Shared shape: a same-shape rule PAIR, or a shape also reached by
                                // anonymous/embedded uses. Every named rule STILL surfaces as its own
                                // real JS class under its identifier (unconditionally, independent of
                                // whether a structural twin was minted first); the structural `MapKToV`
                                // class, where referenced, is minted by the visit arm above.
                                codegen_table_type(
                                    self,
                                    types,
                                    rust_ident,
                                    domain.clone(),
                                    range.clone(),
                                    true,
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), ConceptualRustType::name_for_rust_map(domain, range, false)));
                    }
                    RustStructType::Array {
                        element_type,
                        bounds,
                    } => {
                        if cli.wasm {
                            if *bounds == Some((Some(1), None)) {
                                // named `[+ T]` rule: its JS class is the RESTRICTED wrapper (wrapping
                                // core::NonEmptyVec) under the rule ident, not the loose list wrapper.
                                self.generate_non_empty_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    true,
                                    cli,
                                );
                            } else {
                                self.generate_array_type(
                                    types,
                                    element_type.clone(),
                                    rust_ident,
                                    true,
                                    cli,
                                );
                            }
                        }
                        //self
                        //    .rust()
                        //    .push_type_alias(TypeAlias::new(rust_struct.ident(), element_type.name_as_rust_array(false)));
                    }
                    RustStructType::TypeChoice { variants } => {
                        self.generate_type_choices_from_variants(
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::GroupChoice { variants, rep } => codegen_group_choices(
                        self,
                        types,
                        rust_ident,
                        variants,
                        *rep,
                        rust_struct.tag(),
                        rust_struct.config(),
                        cli,
                    ),
                    RustStructType::Wrapper {
                        wrapped,
                        min_max,
                        float_min_max,
                    } => match rust_struct.tag() {
                        Some(tag) => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            &wrapped.clone().tag(tag),
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                        None => generate_wrapper_struct(
                            self,
                            types,
                            rust_ident,
                            wrapped,
                            *min_max,
                            *float_min_max,
                            rust_struct.config(),
                            cli,
                        ),
                    },
                    RustStructType::Extern => {
                        #[allow(clippy::single_match)]
                        match rust_ident.to_string().as_ref() {
                            "Int" if types.is_referenced(rust_ident) => {
                                generate_int(self, types, cli)
                            }
                            _ => (), /* user-specified external types */
                        }
                    }
                    RustStructType::CStyleEnum { variants } => {
                        generate_c_style_enum(
                            self,
                            types,
                            rust_ident,
                            variants,
                            rust_struct.tag(),
                            rust_struct.config(),
                            cli,
                        );
                    }
                    RustStructType::RawBytesType => {
                        // nothing to do, user specified
                    }
                }
            }

            // Structural wrappers reachable ONLY through a wasm-emitted plain `pub type` alias, never
            // through any rust struct — e.g. `x = bytes .cbor { bignint => uint }`, where `x` is a type
            // alias (not a struct). Its `Map` target is embedded elsewhere only as `Alias(Rust(x), Map)`,
            // and `x` sits in `existing_aliases`, so the rust-struct walk above never descends into that
            // Map — leaving the emitted `pub type X = MapKToV` alias naming a class no one minted. Walk
            // each wasm-alias base type through the same minting path (shared `wasm_wrappers_generated` /
            // `existing_aliases`, so it stays idempotent with the walk above and self-referential/other
            // named aliases are not re-descended).
            if cli.wasm {
                for (alias_ident, alias_info) in types.type_aliases() {
                    if matches!(alias_ident, AliasIdent::Rust(_)) && alias_info.gen_wasm_alias {
                        alias_info.base_type.conceptual_type.visit_types_excluding(
                            types,
                            &mut |ty| {
                                mint_wasm_wrapper_for_visited_type(
                                    self,
                                    types,
                                    ty,
                                    &mut wasm_wrappers_generated,
                                    &table_shape_sole_owner,
                                    cli,
                                )
                            },
                            &mut existing_aliases,
                        );
                    }
                }
            }
        }

        // `@used_as_elem`: mint the loose-list wasm wrapper (`<Elem>List`, the `[* elem]` equivalent)
        // for each tagged element, exactly as an inline `[* elem]` usage would. Runs AFTER the
        // own-spec wasm walk (so a real inline usage that already minted the wrapper dedups via the
        // shared `already_generated`) and BEFORE `emit_requested_collections` (so the wrapper is
        // recorded in `own_wrapper_shapes`, letting a consumer's request for the same shape be
        // satisfied by this crate's own spec instead of re-emitted into requested_collections). The
        // mark set is a `BTreeSet`, so this walks idents in sorted order — deterministic output. A
        // directly-wasm-exposable element has no wrapper and is rejected in `finalize`, so nothing
        // exposable reaches here. `try_defer_wrapper` inside applies normally: if a workspace dep
        // owns the element, deferring to the dep is the correct canonical-host semantics.
        if cli.wasm {
            for ident in types.used_as_elem() {
                let element_type = types.used_as_elem_element_type(ident);
                let structural =
                    RustIdent::new(CDDLIdent::new(element_type.name_as_wasm_array(types)));
                self.generate_array_type(types, element_type, &structural, false, cli);
            }
        }

        // W2 dep side (`--wrapper-requests`): now that the OWN-spec wasm wrapper walk is complete
        // (`wasm_collection_wrappers` / `own_wrapper_shapes` fully populated), read the consumer
        // sidecars, union the requested shapes, and emit each requested wrapper the dep does not
        // already produce into the `requested_collections` module. Wasm-only, and a no-op (byte
        // identical) with no `--wrapper-requests` flag.
        if cli.wasm {
            self.emit_requested_collections(types, cli);
        }

        // JSON export crate
        if cli.json_schema_export {
            self.json_lines
                .line("let schema_path = std::path::Path::new(&\"schemas\");");
            let mut path_exists = Block::new("if !schema_path.exists()");
            path_exists.line("std::fs::create_dir(schema_path).unwrap();");
            self.json_lines.push_block(path_exists);
            let mut main_lines_by_file: BTreeMap<ModuleScope, Vec<String>> = BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                let is_typedef = matches!(
                    rust_struct.variant(),
                    RustStructType::Array { .. } | RustStructType::Table { .. }
                );
                // The is_referenced check is for things like Int which are included by default
                // in order for the CDDL to parse but might not be used.
                // However, we need to export other root types from the user's spec
                if !is_typedef && (rust_ident.as_ref() != "Int" || types.is_referenced(rust_ident))
                {
                    main_lines_by_file
                        .entry(types.scope(rust_ident).clone())
                        .or_default()
                        .push(format!(
                            "gen_json_schema!({});",
                            rust_crate_struct_from_wasm(types, rust_ident, cli)
                        ));
                }
            }
            let multiple_files = main_lines_by_file.len() > 1;
            for (scope_name, lines) in main_lines_by_file {
                if multiple_files {
                    self.json_lines.line(&format!("// {scope_name}"));
                }
                for line in lines {
                    self.json_lines.line(&line);
                }
            }
        }

        // imports / module declarations
        // this is done at the end so we already know all information about output code

        // rust. The codegen provenance header is stamped once per emitted FILE (see
        // `generated_files` / `export`), not per scope — a scope-level raw would hoist above the
        // module-linking raws that `merge_scopes_to_strings` prepends into a merged root file.
        self.rust_lib()
            .raw("#![allow(clippy::too_many_arguments)]\n");

        // declare modules (root lib specific)
        if cli.export_static_files() {
            self.rust_lib().raw("pub mod error;");
            if cli.preserve_encodings {
                self.rust_lib().raw("pub mod ordered_hash_map;");
            }
            // only crates that actually use `[+ T]` pull in the NonEmptyVec runtime — keeps every
            // non-`+` crate's output byte-identical. `--wrapper-requests`: a dep hosting a requested
            // NonEmpty wrapper needs the runtime module even when its own spec has no `[+ …]`.
            if types.uses_non_empty_vec() || self.requested_non_empty_vec {
                self.rust_lib().raw("pub mod non_empty;");
            }
            // only crates that actually use `{+ k => v}` pull in the NonEmptyMap runtime
            if types.uses_non_empty_map() || self.requested_non_empty_map {
                self.rust_lib().raw("pub mod non_empty_map;");
            }
        }
        if cli.preserve_encodings {
            self.rust_lib().raw("extern crate derivative;");
        }
        let scope_names = self
            .rust_scopes
            .keys()
            .filter(|scope| **scope != *ROOT_SCOPE)
            .cloned()
            .collect::<Vec<_>>();
        for scope in scope_names
            .iter()
            .filter_map(|s| {
                if s.export() {
                    s.components().first()
                } else {
                    None
                }
            })
            .collect::<BTreeSet<_>>()
        {
            self.rust_lib().raw(format!("pub mod {scope};"));
        }

        // The borrowed-key-types sidecar module (materialized as `generated/borrowed_key_types.rs` in
        // `generated_files`). RUST crate, not wasm — key derives are a rust-crate concern (the
        // consumer's rust crate is what fails to build without them). PRIVATE (`mod`): its
        // `BORROWED_KEY_TYPES` const is `pub(crate)`-machine-read output and the compiled self-check
        // fails THIS crate's build if a dep drops a derive; nothing is re-exported. Declared whenever
        // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is borrowed.
        if !self.workspace_deps.is_empty() {
            self.rust_lib().raw("mod borrowed_key_types;");
        }

        // The key-demand assertions module (materialized as `generated/key_demand_assertions.rs` in
        // `generated_files`), declared only when some `@used_as_key` root — flavored or bare — exists,
        // so a key-free crate emits neither the decl nor the file. PRIVATE (`mod`): its `_demand_*`
        // fns are compile-time-only self-checks.
        if !assertion_roots(types).is_empty() {
            self.rust_lib().raw("mod key_demand_assertions;");
        }

        // declare common modules in each module (struct files). serialization / cbor_encodings are
        // each declared only where the corresponding .rs is actually emitted (mirror the conditions
        // in generated_files / merge_scopes_to_strings): declaring a `pub mod` with no backing file
        // is E0583, uncompilable.
        //   - serialization.rs: the root always materializes one (the static prelude is prepended
        //     unconditionally — merge_scopes_to_strings always writes the root file), and a non-root
        //     scope only when it has generated serialize impls (`serialize_scopes`). An alias/enum-only
        //     non-root module (scalar/collection/table alias, or a c-style enum whose serialization is
        //     emitted elsewhere) produces no serialization.rs, so an unconditional decl was E0583.
        //   - cbor_encodings.rs: a scope with no encoding structs (e.g. a root of only c-style enums)
        //     emits no such file, so the decl is conditioned on `cbor_encodings_scopes` the same way.
        for (scope, content) in self.rust_scopes.iter_mut() {
            if *scope == *ROOT_SCOPE || self.serialize_scopes.contains_key(scope) {
                content.raw("pub mod serialization;");
            }
            if cli.preserve_encodings
                && scope.export()
                && self.cbor_encodings_scopes.contains_key(scope)
            {
                content.raw("pub mod cbor_encodings;");
            }
        }

        // Extern-type re-export glue. Generated code refers to each in-crate extern type by its bare
        // ident within the scope that declared it (and cross-scope as `crate::generated::<scope>::Name`;
        // the serializer sees it via `use super::*;`). Under the thin-root split the user cannot inject
        // that definition into `generated/**` (clobbered every run), so the contract is to DEFINE the
        // extern in a hand-written module and RE-EXPORT it at the crate root (`pub use utils::Name;` in
        // the thin `lib.rs`). Re-export it from crate root INTO the declaring scope's generated module so
        // every such bare/`use super::*` reference resolves; the explicit `pub use crate::Name;` binds to
        // the user's definition and beats the `pub use generated::*;` glob cycle. Emitted unconditionally
        // — under `--common-import-override` the extern is still crate-local. Skipped:
        //   - the built-in `Int` extern (the tool generates its definition when referenced),
        //   - generic-extern instances that already emit a `pub type` alias in this module (the base
        //     generic extern carries the glue instead — re-exporting the aliased name would collide),
        //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes; those resolve through their dep
        //     crate already — `ModuleScope::export()` is the discriminator).
        let rust_aliased: BTreeSet<&RustIdent> = types
            .type_aliases()
            .iter()
            .filter_map(|(alias_ident, info)| match alias_ident {
                AliasIdent::Rust(ident) if info.gen_rust_alias => Some(ident),
                _ => None,
            })
            .collect();
        let mut externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> = BTreeMap::new();
        for (rust_ident, rust_struct) in types.rust_structs() {
            if matches!(rust_struct.variant(), RustStructType::Extern)
                && rust_ident.as_ref() != "Int"
                && !rust_aliased.contains(rust_ident)
            {
                let scope = types.scope(rust_ident);
                if scope.export() {
                    externs_by_scope
                        .entry(scope.clone())
                        .or_default()
                        .insert(rust_ident.clone());
                }
            }
        }
        for (scope, idents) in &externs_by_scope {
            let content = self.rust_scopes.entry(scope.clone()).or_default();
            for ident in idents {
                content.raw(format!("pub use crate::{ident};"));
            }
        }

        // general common imports (struct files)
        for content in self.rust_scopes.values_mut() {
            // needed if there's any params that can fail
            content
                .push_import("std::convert", "TryFrom", None)
                .push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            // in case we store these in enums we're just going to dump them in everywhere
            if cli.preserve_encodings {
                content
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    );
            }
        }

        // cbor_encodings imports
        if cli.preserve_encodings {
            // `BTreeMap` is pushed into every cbor_encodings file unconditionally; the prune pass
            // (`import_prune::prune_generated_files`, run in `generated_files`) drops it from files
            // whose module family doesn't name it. Dumb-push + central prune — see the struct loop
            // below.
            for content in self.cbor_encodings_scopes.values_mut() {
                content
                    // encoding structs can reference GENERATED types (a table keyed by a
                    // type-choice enum stores `BTreeMap<KeyEnum, StringEncoding>`), so like
                    // serialization.rs this needs the scope module's items — `super::*` also
                    // covers cross-scope keys, since a child glob re-imports the parent struct
                    // file's `use` bindings (the scope_references imports pushed above)
                    .push_import("super", "*", None)
                    .push_import("std::collections", "BTreeMap", None)
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "LenEncoding",
                        None,
                    )
                    .push_import(
                        format!("{}::serialization", cli.common_import_rust()),
                        "StringEncoding",
                        None,
                    );
            }
        }

        // import encoding structs (struct files)
        if cli.preserve_encodings {
            for (rust_ident, rust_struct) in types.rust_structs() {
                if match rust_struct.variant() {
                    RustStructType::Record(_) => true,
                    RustStructType::Wrapper { wrapped, .. } => {
                        !encoding_fields(types, rust_ident.as_ref(), wrapped, true, cli).is_empty()
                    }
                    _ => false,
                } {
                    // ALL records have an encoding struct since at minimum they contian
                    // the array or map encoding details so no need to check fields
                    self.rust(types, rust_ident).push_import(
                        "cbor_encodings",
                        format!("{rust_ident}Encoding"),
                        None,
                    );
                }
            }
        }

        fn add_imports_from_scope_refs(
            scope: &ModuleScope,
            content: &mut codegen::Scope,
            imports: &BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            // The crate-root prefix for cross-scope references within the SAME crate: both the rust
            // and wasm crates nest their generated tree one level (`crate::generated`). Root-scope
            // items and non-exported scopes are still reached relatively.
            crate_prefix: &str,
            // Wasm pass only: `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>` -> wasm crate name. When a
            // non-exported (cross-crate extern-dep) import scope's leading component is mapped, the
            // wasm import is qualified through the dep's wasm crate instead of its rust crate (the
            // rust type has no wasm-bindgen bindings under the split `<dep>`/`<dep>-wasm` layout).
            // `None` for the rust pass and for unmapped deps => import path stays verbatim.
            extern_wasm_crate_map: Option<&BTreeMap<String, String>>,
        ) {
            // might not exist if we don't use stuff from other scopes
            if let Some(scope_imports) = imports.get(scope) {
                for (import_scope, idents) in scope_imports.iter() {
                    let import_scope = if *import_scope == *ROOT_SCOPE {
                        Cow::from(crate_prefix.to_owned())
                    } else if *scope == *ROOT_SCOPE || !import_scope.export() {
                        // Cross-crate extern-dep scopes are non-exported: their leading component is
                        // the dependency crate name. In the wasm pass, remap that component to the
                        // dep's wasm crate when a mapping is present.
                        let components = import_scope.components();
                        match (extern_wasm_crate_map, components.split_first()) {
                            (Some(map), Some((first, rest)))
                                if !import_scope.export() && map.contains_key(first) =>
                            {
                                let wasm_crate = &map[first];
                                if rest.is_empty() {
                                    Cow::from(wasm_crate.clone())
                                } else {
                                    Cow::from(format!("{}::{}", wasm_crate, rest.join("::")))
                                }
                            }
                            _ => Cow::from(import_scope.to_string()),
                        }
                    } else {
                        Cow::from(format!("{crate_prefix}::{import_scope}"))
                    };
                    #[allow(clippy::comparison_chain)]
                    if idents.len() > 1 {
                        content.push_import(
                            import_scope,
                            format!(
                                "{{{}}}",
                                idents
                                    .iter()
                                    .map(|i| i.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            None,
                        );
                    } else if idents.len() == 1 {
                        content.push_import(
                            import_scope,
                            idents.first().unwrap().to_string(),
                            None,
                        );
                    }
                }
            }
        }
        // imports for generated structs from other files (struct files)
        // The rust pass registers no collection-wrapper class imports (those are wasm-only), so
        // deferral never applies here — pass an empty map so rust output is untouched by the flag.
        let rust_imports = types.scope_references(false, &BTreeMap::new());
        for (scope, content) in self.rust_scopes.iter_mut() {
            add_imports_from_scope_refs(scope, content, &rust_imports, "crate::generated", None);
            // These collection-type imports are pushed unconditionally (or on spec-global gates)
            // even into files that never reference them: dumb-push here, and the usage-derived
            // prune pass (`import_prune::prune_generated_files`, run once over the whole file map in
            // `generated_files`) removes any that the file's module family doesn't actually name.
            // Deriving the import set from the emitted tokens is sound by construction and lives in
            // one place; predicting per-file need at each of these ~30 sites would have to mirror
            // every local emission decision and drift.
            content.push_import("std::collections", "BTreeMap", None);
            if cli.preserve_encodings {
                content.push_import(
                    format!("{}::ordered_hash_map", cli.common_import_rust()),
                    "OrderedHashMap",
                    None,
                );
            }
            if types.uses_non_empty_vec() {
                content.push_import(
                    format!("{}::non_empty", cli.common_import_rust()),
                    "NonEmptyVec",
                    None,
                );
            }
            if types.uses_non_empty_map() {
                content.push_import(
                    format!("{}::non_empty_map", cli.common_import_rust()),
                    "NonEmptyMap",
                    None,
                );
            }
        }

        // serialization
        // The imports every generated serialization.rs needs regardless of scope — the static
        // prelude and all generated impls reference these. Shared by the per-scope loop and the
        // lib-scope fallback below so the set can't drift between the two.
        let push_base_serialize_imports = |scope: &mut codegen::Scope| {
            scope
                .push_import("super", "*", None)
                .push_import("std::io", "BufRead", None)
                .push_import("std::io", "Seek", None)
                .push_import("std::io", "SeekFrom", None)
                .push_import("std::io", "Write", None)
                .push_import("cbor_event::de", "Deserializer", None)
                .push_import("cbor_event::se", "Serializer", None)
                .push_import(format!("{}::error", cli.common_import_rust()), "*", None);
            if !(cli.preserve_encodings && cli.canonical_form) {
                scope.push_import("cbor_event::se", "Serialize", None);
            }
        };
        for (scope, content) in self.serialize_scopes.iter_mut() {
            push_base_serialize_imports(content);
            if let Some(common_import) = cli.common_import_override.as_ref() {
                content.push_import(format!("{}::serialization", common_import), "*", None);
            }
            // Only import cbor_encodings where a cbor_encodings.rs is actually emitted for this
            // scope (same condition as its `pub mod` declaration / generated_files): a scope with
            // serialization but no encoding structs (e.g. a group/type choice) emits no such file,
            // so importing it would be an unresolved import (E0432).
            if cli.preserve_encodings && self.cbor_encodings_scopes.contains_key(scope) {
                content.push_import("super::cbor_encodings", "*", None);
            }
            if *scope != *ROOT_SCOPE {
                content.push_import(
                    format!("{}::serialization", cli.common_import_rust()),
                    "*",
                    None,
                );
            }
        }

        // The static serialization prelude prepended to the root serialization.rs (when we own the
        // static files) references Serializer/Deserializer/BufRead/DeserializeError/etc. Those
        // imports are added to the ROOT_SCOPE serialize scope by the loop above — but a spec whose
        // root has no per-type serialization (e.g. only c-style enums) produces no ROOT_SCOPE entry,
        // leaving the prelude (and any rust_serialize_lib impls) without imports and the crate
        // uncompilable. Add the base imports to the lib serialize scope in that case. (No
        // cbor_encodings/non-root imports: no ROOT_SCOPE entry means no root struct, so no root
        // encoding struct and nothing cross-module to reach.)
        if cli.export_static_files() && !self.serialize_scopes.contains_key(&*ROOT_SCOPE) {
            push_base_serialize_imports(self.rust_serialize_lib());
        }

        // declare submodules
        // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
        // for these modules when they only exist to support modules nested deeper
        declare_modules(&mut self.rust_scopes, &scope_names);

        // wasm
        if cli.wasm {
            let extern_wasm_crate_map = cli.extern_wasm_crate_map();
            // Validate mapping keys BEFORE emitting: a key that names no extern dependency is almost
            // certainly a typo, and a silent no-op would leave the generated wasm crate pointing at
            // the (non-wasm) rust crate and failing to compile with no hint why.
            if !extern_wasm_crate_map.is_empty() {
                let extern_dep_names = types.extern_dep_names();
                for dep in extern_wasm_crate_map.keys() {
                    if !extern_dep_names.contains(dep) {
                        panic!(
                            "--extern-wasm-crate names dependency {dep:?}, which is not an \
                             extern dependency in this spec. Known extern dependencies: {:?}",
                            extern_dep_names
                        );
                    }
                }
            }
            self
            .wasm_lib()
            .raw("#![allow(clippy::len_without_is_empty, clippy::too_many_arguments, clippy::new_without_default)]");
            // wasm module declarations
            let wasm_scope_names = self
                .wasm_scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE)
                .cloned()
                .collect::<Vec<_>>();
            for scope in wasm_scope_names
                .iter()
                .filter_map(|s| {
                    if s.export() {
                        s.components().first()
                    } else {
                        None
                    }
                })
                .collect::<BTreeSet<_>>()
            {
                self.wasm_lib().raw(format!("pub mod {scope};"));
            }
            // The collection-wrapper index module (materialized as `generated/collections.rs` in
            // `generated_files`). Declared unconditionally for every wasm run — even one that mints
            // zero wrappers — from the always-regenerated generated root, never the seed-once
            // crate-root lib.rs.
            self.wasm_lib().raw("pub mod collections;");
            // The borrowed-collections sidecar module (materialized as `generated/borrowed_collections.rs`
            // in `generated_files`). PRIVATE (`mod`, never `pub mod`) — its `use` lines only
            // existence-check the borrowed wrapper names; borrowed wrappers are never re-exported (the
            // consumer's own `collections.rs` lists only wrappers it defines). Declared whenever
            // `--workspace-dep` is present (stable presence, stable diffs), even when nothing is
            // borrowed.
            if !self.workspace_deps.is_empty() {
                self.wasm_lib().raw("mod borrowed_collections;");
            }
            // wasm imports
            // `deferred_wrappers` was fully populated during the wasm struct walk above (every
            // deferred wrapper's mint point recorded it), so referencing modules now get a plain
            // `use <dep_wasm>::collections::<Name>;` for each instead of a local class.
            let wasm_imports = types.scope_references(true, &self.deferred_wrappers);
            for (scope, content) in self.wasm_scopes.iter_mut() {
                // imports from other struct modules; the wasm generated tree nests one level under
                // `crate::generated` (same as the rust crate)
                add_imports_from_scope_refs(
                    scope,
                    content,
                    &wasm_imports,
                    "crate::generated",
                    Some(&extern_wasm_crate_map),
                );
                // common imports. The collection-type imports below (`BTreeMap`/`OrderedHashMap`
                // and the two NonEmpty types) are pushed on spec-global gates even into wasm files
                // that never reference them; the prune pass (`import_prune::prune_generated_files`,
                // in `generated_files`) removes the ones the file's module family doesn't name.
                // Dumb-push + central prune.
                content
                    .push_import("wasm_bindgen::prelude", "wasm_bindgen", None)
                    .push_import("wasm_bindgen::prelude", "JsError", None);
                if cli.json_serde_derives && cli.wasm_cbor_json_api_macro.is_none() {
                    content.push_import("wasm_bindgen::prelude", "JsValue", None);
                }
                if cli.preserve_encodings {
                    content.push_import(
                        format!("{}::ordered_hash_map", cli.common_import_wasm()),
                        "OrderedHashMap",
                        None,
                    );
                } else {
                    content.push_import("std::collections", "BTreeMap", None);
                }
                if types.uses_non_empty_vec() {
                    content.push_import(
                        format!("{}::non_empty", cli.common_import_wasm()),
                        "NonEmptyVec",
                        None,
                    );
                }
                if types.uses_non_empty_map() {
                    content.push_import(
                        format!("{}::non_empty_map", cli.common_import_wasm()),
                        "NonEmptyMap",
                        None,
                    );
                }
                // external macros
                if let Some(cbor_json_macro) = &cli.wasm_cbor_json_api_macro
                    && let Some((path, m)) = cbor_json_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(conversion_macro) = &cli.wasm_conversions_macro
                    && let Some((path, m)) = conversion_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
                if let Some(list_macro) = &cli.wasm_list_macro
                    && let Some((path, m)) = list_macro.rsplit_once("::")
                {
                    content.push_import(path, m, None);
                }
            }
            // Extern-type re-export glue (wasm crate). The wasm generated code names each in-crate
            // extern by its bare WRAPPER ident within the declaring scope (`req: ExternalFoo`, and via
            // `use super::*;` in nested modules), exactly as the rust crate names the native type — same
            // E0433 shape under the thin-root split, since a crate-root name isn't visible inside
            // `mod generated`. The contract mirrors rust: DEFINE the wasm wrapper in a hand-written
            // wasm-crate module and RE-EXPORT it at the wasm crate root (`pub use utils::Name;`); the tool
            // re-exports it from crate root INTO the declaring scope's generated module so every such
            // reference resolves against the user's wrapper. Skipped:
            //   - the built-in `Int` extern (the tool generates its own wasm wrapper when referenced, so
            //     `pub use crate::Int;` would collide),
            //   - generic-extern instances that already emit a wasm `pub type` alias here (`gen_wasm_alias`
            //     — the wrapper the alias points at carries the glue instead),
            //   - generic-extern BASES (`Foo` of `Foo<Bar>`): a plain `Extern` rust struct, but wasm never
            //     names it (wasm-bindgen has no generics; the instance collapses to the argument wrapper),
            //     so there is no wasm-crate-root definition to re-export — emitting glue would be an
            //     unresolved import. The rust side keeps the base because its `pub type` alias names it.
            //   - externs under `EXTERN_DEPS_DIR` (non-exported scopes) resolve through their dep crate via
            //     `common_import_wasm()` already — `ModuleScope::export()` is the discriminator.
            let wasm_aliased: BTreeSet<&RustIdent> = types
                .type_aliases()
                .iter()
                .filter_map(|(alias_ident, info)| match alias_ident {
                    AliasIdent::Rust(ident) if info.gen_wasm_alias => Some(ident),
                    _ => None,
                })
                .collect();
            let generic_bases = types.generic_instance_bases();
            let mut wasm_externs_by_scope: BTreeMap<ModuleScope, BTreeSet<RustIdent>> =
                BTreeMap::new();
            for (rust_ident, rust_struct) in types.rust_structs() {
                if matches!(rust_struct.variant(), RustStructType::Extern)
                    && rust_ident.as_ref() != "Int"
                    && !wasm_aliased.contains(rust_ident)
                    && !generic_bases.contains(rust_ident)
                {
                    let scope = types.scope(rust_ident);
                    if scope.export() {
                        wasm_externs_by_scope
                            .entry(scope.clone())
                            .or_default()
                            .insert(rust_ident.clone());
                    }
                }
            }
            for (scope, idents) in &wasm_externs_by_scope {
                let content = self.wasm_scopes.entry(scope.clone()).or_default();
                for ident in idents {
                    content.raw(format!("pub use crate::{ident};"));
                }
            }
            // declare submodules
            // we do this after the rest to avoid declaring serialization mod/cbor encodings/etc
            // for these modules when they only exist to support modules nested deeper
            declare_modules(&mut self.wasm_scopes, &wasm_scope_names);
        }

        // optional generated-test module (reject + round-trip halves; off by default, so it
        // doesn't touch the snapshot suite)
        //
        // Multifile output: each test module lands at the generated ROOT (the `raw` below) while
        // the minted values name submodule types bare (`St`, `Bholder`) — `use super::*;` only
        // reaches root-scope items, so the emitters glob-import each declared non-root module
        // (`use super::a::*;`). The lists are derived from the SAME per-crate scope maps the
        // module-declaration loops above consume (`rust_scopes`/`wasm_scopes`, minus root and
        // non-exported extern-dep scopes), so a glob can never name an undeclared module; BTreeMap
        // keys keep the order deterministic. Caveat: glob imports can collide (E0659) if two
        // submodules export the same type name — no matrix cell or corpus fixture does; if a
        // consumer ever hits it, the robust long-term shape is fully-qualified rendering (thread
        // `types.scope(ident)` into `render_rust`/`render_wasm`) instead of globs.
        let submodule_glob_paths = |scopes: &BTreeMap<ModuleScope, codegen::Scope>| -> Vec<String> {
            scopes
                .keys()
                .filter(|scope| **scope != *ROOT_SCOPE && scope.export())
                .map(|scope| scope.components().join("::"))
                .collect()
        };
        if cli.emit_tests {
            let rust_submodules = submodule_glob_paths(&self.rust_scopes);
            if let Some(test_mod) =
                crate::emit_tests::emit_generated_tests(types, cli, &rust_submodules)
            {
                self.rust_lib().raw(&test_mod);
            }
        }
        // the wasm-crate counterpart: same MintValue derivation, rendered through the wrapper API +
        // the cddl_lib rust twin (cross-crate byte differential). `#[cfg(test)]` so it's inert for
        // build/check/wasm-pack — only a `cargo test` of the wasm crate compiles and runs it.
        if cli.wasm && cli.emit_tests {
            let wasm_submodules = submodule_glob_paths(&self.wasm_scopes);
            if let Some(test_mod) =
                crate::emit_tests_wasm::emit_generated_wasm_tests(types, cli, &wasm_submodules)
            {
                self.wasm_lib().raw(&test_mod);
            }
        }
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated structs and associated traits (besides serialization ones)
    pub fn rust(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.rust_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" rust scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn rust_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_lib_scope
    }

    /// Serialization scope for `ident`
    pub fn rust_serialize(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope_name = types.scope(ident).to_owned();
        self.serialize_scopes.entry(scope_name).or_default()
    }

    /// Serialization scope for lib.cddl
    /// e.g. for core stuff, or things without an explicit scope like WASM arrays
    pub fn rust_serialize_lib(&mut self) -> &mut codegen::Scope {
        &mut self.rust_serialize_lib_scope
    }

    /// Generates in the appropriate scope for `ident`
    /// Used for all the generated WASM wrapper structs and associated traits
    pub fn wasm(&mut self, types: &IntermediateTypes, ident: &RustIdent) -> &mut codegen::Scope {
        // W2 (`--wrapper-requests`): a requested wrapper is not in this dep's IR, so `types.scope`
        // would fall back to the crate root. While the override is set (only around requested-wrapper
        // emission), route it into the dedicated `requested_collections` module instead.
        let scope_name = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).to_owned(),
        };
        self.wasm_scopes.entry(scope_name).or_default()
    }

    /// Scope header above the rest of the "lib" WASM scope.
    /// This is useful for when there is no explicit scope
    /// e.g. implicit types like arrays/tables (for WASM)
    pub fn wasm_lib(&mut self) -> &mut codegen::Scope {
        &mut self.wasm_lib_scope
    }

    /// Record that a collection-wrapper class `ident` was just emitted, for the
    /// `wasm/src/generated/collections.rs` re-export index. Called from each of the four wrapper
    /// emitters right after their shared `already_generated` guard admits the mint, so the index
    /// captures every wrapper class exactly once and never a suppressed one. The recorded
    /// `ModuleScope` is `types.scope(ident)` — the SAME scope `wasm(types, ident)` places the class
    /// in — so the index path derives from the class's real emission location.
    fn record_collection_wrapper(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
        shape: &str,
    ) {
        // The recorded scope is where the class is actually emitted: the requested-collections
        // override when active (so the index re-exports it from that module), else `types.scope`.
        let scope = match &self.requested_scope_override {
            Some(scope) => scope.clone(),
            None => types.scope(ident).clone(),
        };
        self.wasm_collection_wrappers.insert(ident.clone(), scope);
        // W2 (`--wrapper-requests`): index this crate's OWN collection-wrapper shapes (main walk only,
        // never the requested wrappers being minted under the override) so a dep can tell whether it
        // already produces a requested shape, and under what name.
        if self.requested_scope_override.is_none() {
            self.own_wrapper_shapes
                .insert(shape.to_owned(), ident.clone());
        }
    }

    /// W2 (`--wrapper-requests`): the attribution doc for `ident` as a paragraph PREFIX (trailing
    /// blank line) to prepend to an emitter-set struct doc, or `""` when the wrapper is not requested.
    /// Used by the NonEmpty emitters, whose `.doc()` call would otherwise clobber the attribution
    /// `create_base_wasm_struct` injects.
    fn requested_attribution_prefix(&self, ident: &RustIdent) -> String {
        self.requested_attribution
            .get(ident)
            .map(|d| format!("{d}\n\n"))
            .unwrap_or_default()
    }

    /// Record that structural wrapper `ident` was deferred to workspace dependency `dep` this run
    /// (`--workspace-dep`), for the `wasm/src/generated/borrowed_collections.rs` sidecar. Idempotent:
    /// the same wrapper is probed from several sites (the loose emitter, a keys-list, a NonEmpty
    /// try_from source), each recording the same `(dep, shape)`. Two DISTINCT shapes deriving the
    /// SAME structural name — the `MapAToBToC` reverse-ambiguity (`{* a => b_to_c}` vs
    /// `{* a_to_b => c}`) — is a hard error naming both shapes: today that pair already fails rustc
    /// (two same-named local mints), so this upgrades a compile failure into an actionable diagnostic.
    pub(crate) fn record_borrowed_wrapper(&mut self, ident: &RustIdent, dep: &str, shape: &str) {
        if let Some((_, existing_shape)) = self.borrowed_wrappers.get(ident)
            && existing_shape != shape
        {
            panic!(
                "two distinct shapes in this crate's spec derive the same borrowed collection wrapper \
                 name {ident}: {existing_shape:?} and {shape:?}. These would define one JS class for \
                 two concepts — rename or @name one of them."
            );
        }
        self.borrowed_wrappers
            .insert(ident.clone(), (dep.to_owned(), shape.to_owned()));
    }

    /// W2 dep side (`--wrapper-requests`): read each consumer's committed `borrowed_collections.rs`,
    /// take the entries addressed to THIS dep (dep column == the normalized `--lib-name`), union the
    /// requested collection-wrapper shapes across consumers, and emit every requested wrapper the dep
    /// does not already produce into `wasm/src/generated/requested_collections.rs` (indexed via
    /// `record_collection_wrapper`, each carrying a sorted-requester attribution doc). Called once,
    /// after the own-spec wasm walk, under `--wasm`. A no-op — output byte-identical to today — when
    /// no `--wrapper-requests` flag is set (the module is not even created).
    ///
    /// Determinism: everything is keyed/sorted (`BTreeMap`/`BTreeSet`), so the union and the emission
    /// order depend on neither the flag order nor the consumers' regen order.
    fn emit_requested_collections(&mut self, types: &IntermediateTypes, cli: &Cli) {
        let request_files = cli.wrapper_requests();
        if request_files.is_empty() {
            // No flag => no file, byte-identical to today (acceptance criterion 10 analog).
            return;
        }
        let my_lib = cli.lib_name_code();

        // One entry per requested shape after unioning across consumers.
        struct Unioned {
            rt: RustType,
            structural: String,
            requesters: BTreeSet<String>,
        }
        // Keyed by the canonically RE-RENDERED shape (so `stake-credential` ≡ `stake_credential`
        // unify): two consumers requesting the same shape with hyphen/underscore skew collapse here.
        let mut union: BTreeMap<String, Unioned> = BTreeMap::new();

        for (consumer, path) in &request_files {
            let contents = std::fs::read_to_string(path).unwrap_or_else(|e| {
                panic!("--wrapper-requests {consumer}={path}: cannot read the sidecar: {e}")
            });
            let entries = crate::wrapper_requests::parse_sidecar(&contents, path);
            for entry in entries {
                // Entries addressed to OTHER deps (dep column != this crate's normalized lib name)
                // are silently skipped — a shared sidecar can name several deps.
                if entry.dep.replace('-', "_") != my_lib {
                    continue;
                }
                let rt = parse_requested_shape(types, &entry.shape, consumer, path, &entry.name);
                // A requested shape that is DIRECTLY WASM-EXPOSABLE has no wrapper class at all —
                // it lowers to a bare `Vec<…>` at the wasm boundary — so no borrowed wrapper exists
                // or is needed. Such a request is the symptom of an unfaithful consumer stub: the
                // consumer declared its element(s) opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this
                // dep resolves them transparently to a directly-exposable type. Diagnose it here,
                // before deriving the structural name — otherwise a loose list over a transparent
                // primitive alias (`[* coin]` with `coin = uint`) misdiagnoses as a name↔shape
                // disagreement, and a member-form listing (`Vec<u64>` for `[* uint]`) slips past the
                // cross-check and dies later in rustfmt labeled a generator bug.
                if let Some(member) = requested_exposable_member(types, &rt) {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        "its element is a wasm-primitive".to_owned()
                    } else {
                        format!("its element(s) resolve here as {}", leaves.join(", "))
                    };
                    panic!(
                        "--wrapper-requests {consumer} ({path}): the requested wrapper {:?} with \
                         shape {:?} is directly wasm-exposable — it lowers to `{member}` with no \
                         wrapper class, so no borrowed wrapper exists or is needed ({leaf_note}). \
                         This request is the symptom of an unfaithful consumer stub: the consumer \
                         declared the element opaque (`_CDDL_CODEGEN_EXTERN_TYPE_`) while this dep \
                         resolves it transparently. Remedy: fix the consumer's \
                         `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` stub for this dep to declare the element \
                         truthfully (e.g. `coin = uint`) and regenerate the consumer, which will \
                         then stop borrowing this shape.",
                        entry.name, entry.shape
                    );
                }
                let canonical = render_wrapper_shape(&rt);
                let structural = requested_structural_name(types, &rt, consumer, path);
                // Cross-check the derived structural name against the listed name (criterion 8 #2).
                if structural != entry.name {
                    let leaves = requested_shape_leaf_resolutions(types, &entry.shape);
                    let leaf_note = if leaves.is_empty() {
                        String::new()
                    } else {
                        format!(" Element resolution in this dep: {}.", leaves.join(", "))
                    };
                    panic!(
                        "--wrapper-requests {consumer} ({path}): the borrowed wrapper listed as \
                         {:?} with shape {:?} derives the structural name {:?}, not {:?} — the \
                         sidecar's name and shape columns disagree (a name↔shape mismatch).{leaf_note}",
                        entry.name, entry.shape, structural, entry.name
                    );
                }
                let u = union.entry(canonical).or_insert_with(|| Unioned {
                    rt: rt.clone(),
                    structural: structural.clone(),
                    requesters: BTreeSet::new(),
                });
                u.requesters.insert(consumer.clone());
            }
        }

        // Criterion 8 #4: two DISTINCT requested shapes deriving the SAME structural name (from any
        // combination of consumers) — one JS class for two concepts. Name both shapes and their
        // requesters.
        let mut by_structural: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for shape in union.keys() {
            by_structural
                .entry(union[shape].structural.clone())
                .or_default()
                .push(shape.clone());
        }
        for (structural, shapes) in &by_structural {
            if shapes.len() > 1 {
                let requesters: BTreeSet<&String> = shapes
                    .iter()
                    .flat_map(|s| union[s].requesters.iter())
                    .collect();
                panic!(
                    "--wrapper-requests: two distinct requested shapes derive the same structural \
                     wrapper name {structural:?}: {shapes:?} (requested by {requesters:?}). These \
                     would define one JS class for two concepts — rename or @name one of the shapes \
                     in the requesting consumers."
                );
            }
        }

        // Decide, per unioned shape, whether the dep already produces it (skip), produces it under a
        // different rule name (criterion 8 #3, hard error), or must emit it.
        let mut to_emit: Vec<(String, RustType, String, Vec<String>)> = Vec::new();
        for (canonical, u) in &union {
            match self.own_wrapper_shapes.get(canonical) {
                // Own spec already produces this shape under the STRUCTURAL name => request satisfied
                // by the existing indexed wrapper; emit nothing.
                Some(existing) if existing.as_ref() == u.structural => {}
                // Own spec produces this shape under a DIFFERENT (rule-declared) name => hard error.
                Some(existing) => {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} (requested by {:?}) is \
                         already produced by this dep's own spec under the non-structural rule name \
                         {existing}, not the structural name {:?} the consumers import. Emitting \
                         both would create two JS classes for one concept. Remedy: rename the rule \
                         {existing} to {}, give it `@name {}`, or drop it.",
                        u.requesters, u.structural, u.structural, u.structural
                    );
                }
                None => {
                    let mut requesters: Vec<String> = u.requesters.iter().cloned().collect();
                    requesters.sort();
                    to_emit.push((
                        canonical.clone(),
                        u.rt.clone(),
                        u.structural.clone(),
                        requesters,
                    ));
                }
            }
        }

        // Criterion 8 #5: a requested NESTED shape whose inner collection wrapper is neither requested
        // nor own-spec-produced — an integrity check against a hand-edited / truncated sidecar (a real
        // consumer closes over its nested shapes automatically, so the inner should always be present).
        for (canonical, rt, _, _) in &to_emit {
            for inner in inner_collection_shapes(rt) {
                let requested = union.contains_key(&inner);
                let own = self.own_wrapper_shapes.contains_key(&inner);
                if !requested && !own {
                    panic!(
                        "--wrapper-requests: requested shape {canonical:?} nests the collection \
                         wrapper {inner:?}, which is neither requested by any consumer nor produced \
                         by this dep's own spec. The inner collection of an all-one-dep shape is \
                         itself all-one-dep and must be requested too — this sidecar looks truncated \
                         or hand-edited."
                    );
                }
            }
        }

        // Emit. `to_emit` is in canonical-shape (BTreeMap) order, so loose `[* …]` precedes its
        // NonEmpty `[+ …]` twin (`*` < `+`): a separately-requested loose source is emitted (and gets
        // its attribution) BEFORE the NonEmpty emitter's recursive mint no-ops on it. A NonEmpty
        // support source that is NOT itself requested is minted by the emitter into this same module
        // (indexed, no attribution — a benign transitive superset). Byte-identical under any flag /
        // regen order because the input set is fully sorted.
        let requested_scope = ModuleScope::from(vec!["requested_collections".to_owned()]);
        for (_, _, structural, requesters) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
            self.requested_attribution.insert(
                ident,
                format!("Generated at the request of: {}.", requesters.join(", ")),
            );
        }
        self.requested_scope_override = Some(requested_scope.clone());
        for (_, rt, structural, _) in &to_emit {
            let ident = RustIdent::new(CDDLIdent::new(structural.clone()));
            match &rt.conceptual_type {
                ConceptualRustType::Array(inner) => {
                    if rt.is_non_empty_array() {
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    } else {
                        self.generate_array_type(types, (**inner).clone(), &ident, false, cli);
                    }
                }
                ConceptualRustType::Map(k, v) => {
                    if rt.is_non_empty_map() {
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    } else {
                        codegen_table_type(
                            self,
                            types,
                            &ident,
                            (**k).clone(),
                            (**v).clone(),
                            false,
                            cli,
                        );
                    }
                }
                other => unreachable!("requested shape is not a collection: {other:?}"),
            }
        }
        self.requested_scope_override = None;

        // A requested NonEmpty wrapper pulls in the NonEmpty runtime the dep's OWN spec may not use;
        // record it so the runtime-provisioning gates (mod decl + static file copy) fire, and import
        // the type into this scope explicitly (the per-scope loop's import gate is keyed off the dep's
        // own IR, which doesn't see the requested wrappers).
        self.requested_non_empty_vec = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_array());
        self.requested_non_empty_map = to_emit
            .iter()
            .any(|(_, rt, _, _)| rt.contains_non_empty_map());
        let non_empty_import = self
            .requested_non_empty_vec
            .then(|| format!("{}::non_empty", cli.common_import_wasm()));
        let non_empty_map_import = self
            .requested_non_empty_map
            .then(|| format!("{}::non_empty_map", cli.common_import_wasm()));

        // Ensure the module exists even when nothing is emitted (all requests satisfied by own spec /
        // addressed elsewhere) — stable presence, stable diffs (plan decision 1). When non-empty, the
        // wrappers reference the dep's own element WASM wrappers (which live at the generated root or a
        // sibling module); `use super::*;` reaches them, mirroring the emit-tests glob. The per-scope
        // import loop later adds the common wasm imports (wasm_bindgen/JsError/OrderedHashMap/…).
        let scope_content = self.wasm_scopes.entry(requested_scope).or_default();
        if !to_emit.is_empty() {
            scope_content.raw("use super::*;");
        }
        // These NonEmpty imports are pushed whenever the requested wrappers use them; if the file's
        // module family ends up not naming one, the prune pass
        // (`import_prune::prune_generated_files`, in `generated_files`) drops it. Dumb-push +
        // central prune, same as the struct sites.
        if let Some(path) = non_empty_import {
            scope_content.push_import(path, "NonEmptyVec", None);
        }
        if let Some(path) = non_empty_map_import {
            scope_content.push_import(path, "NonEmptyMap", None);
        }
    }

    /// Decide whether a structural collection wrapper the consumer is about to mint should instead be
    /// DEFERRED to a dependency that already owns it (`--extern-wrapper-index`). `structural_name` is
    /// the wrapper's structurally-derived name (`name_as_wasm_array` / `name_for_wasm_map`) and
    /// `constituents` its element (list) or key+value (map) conceptual types.
    ///
    /// Returns `true` when the wrapper is deferred — the caller must emit NO local class and skip
    /// `record_collection_wrapper`, so the deferred wrapper leaves the crate's own `collections.rs`
    /// index (R3e). The ident is recorded in `deferred_wrappers` mapped to the dependency's
    /// `collections` module scope, so `scope_references` routes a plain
    /// `use <dep_wasm>::collections::<Name>;` into every referencing module (R3b) and the keys()
    /// accessors construct via `.into()` cross-crate (R3d). Returns `false` (mint locally) when: the
    /// flag is unused; the ident is not the structural name of these constituents (a rule-declared
    /// wrapper — never suppressed); the constituents are mixed / not all one dependency (R3c, silent);
    /// or an all-extern-of-one-dep candidate is absent from that dep's index (local + one stderr
    /// warning naming the wrapper).
    #[allow(clippy::too_many_arguments)]
    fn try_defer_wrapper(
        &mut self,
        types: &IntermediateTypes,
        wrapper_ident: &RustIdent,
        structural_name: &str,
        constituents: &[&ConceptualRustType],
        // The wrapper's CDDL shape fragment (canonical renderer output), used to build the paste-able
        // "add this rule" hint on the not-in-index warning AND recorded in the workspace sidecar.
        shape: &str,
        // `true` when this mint request comes from an explicit RULE declaration (`foo_list = [* foo]`
        // reached via the `RustStruct::{Array,Table}` variant arms) rather than a synthesized/inline
        // wrapper. Only meaningful when the rule's ident coincides with the structural name (the
        // common `name != structural` case is already screened below); in workspace mode a
        // rule-declared wrapper is the consumer's OWN class and must NEVER defer — instead it triggers
        // the shadowing warning (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) -> bool {
        // Fast out only when NEITHER deferral mechanism is active. (Flag-off byte-identity: with both
        // sets empty this is the same early `false` as before — the workspace branch below is dead
        // code, criterion 10.)
        if self.extern_wrapper_index.is_empty() && self.workspace_deps.is_empty() {
            return false;
        }
        // Only structural-named wrappers are defer candidates: a rule-declared wrapper
        // (`foo_list = [* extern_foo]`) whose ident DIFFERS from the structural name is the consumer's
        // OWN class and is never suppressed. (A rule whose ident COINCIDES with the structural name
        // passes this guard; workspace mode distinguishes it via `rule_declared` just below.)
        if wrapper_ident.as_ref() != structural_name {
            return false;
        }
        // Workspace mode (`--workspace-dep`): an all-one-workspace-dep wrapper DEFERS UNCONDITIONALLY,
        // before any index consult. The placement decision is factored as one function over the
        // transitive element-owner set (plan decision 4: today "exactly one owner ∈ workspace deps →
        // Borrow"; "latest of the element owners" can replace this body later without touching call
        // sites). Ownerless / mixed-dep wrappers fall through to the shipped index/local logic below
        // (criterion 2). A rule-declared wrapper that would otherwise borrow is the consumer's own
        // class: warn (criterion 9) and fall through, never suppress it.
        if !self.workspace_deps.is_empty()
            && let WrapperPlacement::Borrow(dep) = wrapper_placement(
                &transitive_owner_set(types, constituents),
                &self.workspace_deps,
            )
        {
            if rule_declared {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    eprintln!(
                        "warning: rule-declared type {structural_name} shadows the collection wrapper \
                         this crate would otherwise borrow from workspace dependency {dep:?}; the \
                         authored class will duplicate-symbol against the dep's requested class at \
                         link. Remedy: rename the rule, or give it a distinct @name."
                    );
                }
                // fall through to the shipped behavior (never a workspace defer)
            } else {
                // Deferred to the workspace dep: record the borrow (idempotent; a same-name/different
                // -shape collision hard-errors inside) and route the import exactly like the index
                // branch does, so `scope_references` emits `use <dep_wasm>::collections::<Name>;`.
                self.record_borrowed_wrapper(wrapper_ident, &dep, shape);
                let dep_scope = ModuleScope::from(vec![
                    crate::parsing::EXTERN_DEPS_DIR.to_owned(),
                    dep,
                    "collections".to_owned(),
                ]);
                self.deferred_wrappers
                    .insert(wrapper_ident.clone(), dep_scope);
                return true;
            }
        }
        // Beyond this point is the shipped `--extern-wrapper-index` path (unchanged). It requires the
        // index; with only `--workspace-dep` set (no index) there is nothing more to do.
        if self.extern_wrapper_index.is_empty() {
            return false;
        }
        // Each named constituent (element / key / value that resolves to a named rule) maps to the
        // dependency owning it (leading component of its non-exported scope), or `None` when it's a
        // consumer-owned (exported) type. Primitives contribute no constituent.
        let mut constituent_deps: Vec<Option<String>> = Vec::new();
        for c in constituents {
            for id in named_constituent_idents(c) {
                let scope = types.scope(&id);
                constituent_deps.push(if scope.export() {
                    None
                } else {
                    scope.components().first().cloned()
                });
            }
        }
        let dep = if constituent_deps.is_empty() {
            // Zero named constituents (e.g. `MapU64ToText`): a defer candidate only if some configured
            // index lists the name. Several listing it would each be a duplicate-symbol link error, so
            // defer to the lexicographically-first dep (BTreeMap iteration order) and warn.
            let matching: Vec<&String> = self
                .extern_wrapper_index
                .iter()
                .filter(|(_, names)| names.contains(structural_name))
                .map(|(dep, _)| dep)
                .collect();
            match matching.as_slice() {
                [] => return false, // owned by no dependency -> local, silent
                [only] => (*only).clone(),
                many => {
                    if self.deferred_warned.insert(wrapper_ident.clone()) {
                        eprintln!(
                            "warning: collection wrapper {structural_name} is listed in several \
                             --extern-wrapper-index files ({many:?}); deferring to the first ({})",
                            many[0]
                        );
                    }
                    many[0].clone()
                }
            }
        } else {
            // Has named constituents: a defer candidate only if they ALL resolve to extern types of
            // the SAME dependency (R3c: any consumer-owned or cross-dependency constituent -> local,
            // silent).
            let mut single: Option<String> = None;
            for d in &constituent_deps {
                match d {
                    None => return false,
                    Some(name) => match &single {
                        None => single = Some(name.clone()),
                        Some(s) if s == name => {}
                        Some(_) => return false,
                    },
                }
            }
            let dep = single.unwrap();
            // All-extern-of-one-dep candidate: defer iff that dep's index lists it; otherwise mint
            // locally and warn once (a dep-side inventory change that silently shifted ownership back
            // to the consumer is then loud in the regen log, not only in the diff).
            if !self
                .extern_wrapper_index
                .get(&dep)
                .is_some_and(|names| names.contains(structural_name))
            {
                if self.deferred_warned.insert(wrapper_ident.clone()) {
                    // Append the exact rule line to paste into the owning dep's spec: declaring it
                    // there lands the wrapper in the dep's collections.rs index (by construction), so
                    // every consumer's index-deferral then picks it up — the shipped manual override
                    // for wrappers no request sidecar covers (hand-written consumer code, mixed-dep
                    // shapes). Rule name = snake_case of the structural name; shape from the canonical
                    // renderer; requester = this consumer's normalized --lib-name.
                    let rule_name = convert_to_snake_case(structural_name);
                    let requester = cli.lib_name_code();
                    eprintln!(
                        "warning: collection wrapper {structural_name} has only extern elements of \
                         dependency {dep:?} but is absent from its --extern-wrapper-index; minting \
                         it locally (a dep that later adds it would duplicate-symbol at link time)\n\
                         hint: add to {dep}'s spec: {rule_name} = {shape} ; requested by {requester}"
                    );
                }
                return false;
            }
            dep
        };
        // Deferred: import from the dep's `collections` module. The non-exported scope
        // `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>/collections` is remapped by
        // `add_imports_from_scope_refs` to `<dep_wasm>::collections` when `--extern-wasm-crate` maps
        // the dep, or left as `<dep>::collections` (the dep's rust crate name — the same fallback
        // unmapped extern types get) otherwise.
        let dep_scope = ModuleScope::from(vec![
            crate::parsing::EXTERN_DEPS_DIR.to_owned(),
            dep,
            "collections".to_owned(),
        ]);
        self.deferred_wrappers
            .insert(wrapper_ident.clone(), dep_scope);
        true
    }

    /// CBOR encoding scope for `ident` (i.e. *Encoding structs)
    pub fn cbor_encodings(
        &mut self,
        types: &IntermediateTypes,
        ident: &RustIdent,
    ) -> &mut codegen::Scope {
        let scope = types.scope(ident).clone();
        self.cbor_encodings_scopes.entry(scope).or_default()
    }

    // generate array type ie [Foo] generates Foos if not already created
    fn generate_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        array_type_ident: &RustIdent,
        // `true` when `array_type_ident` is an explicit RULE ident (`foo_list = [* foo]`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index` / `--workspace-dep`: if a dependency already owns (index) or a
        // workspace dep owns (unconditional) this exact list wrapper, defer to it (import from the
        // dep's `collections` module) instead of re-minting a duplicate class.
        let shape = format!("[* {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            array_type_ident,
            &element_type.name_as_wasm_array(types),
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        if self.already_generated.insert(array_type_ident.clone()) {
            // Record for the collections.rs index BEFORE the `--wasm-list-macro` early return: the
            // macro still DEFINES the wrapper class, so it belongs in the index exactly like the
            // inline struct below.
            self.record_collection_wrapper(types, array_type_ident, &shape);
            // --wasm-list-macro: emit a single macro invocation in place of the inline struct +
            // accessor block + conversion impls. The macro also emits the conversions, so we skip
            // building the WasmWrapper entirely (returning early) to avoid double-defining them.
            // Element types whose wasm boundary doesn't reduce to (needs_into, is_copy) - e.g.
            // Optional - fall through to the inline path below.
            if let Some(list_macro) = &cli.wasm_list_macro
                && let Some(needs_into) = element_type.wasm_list_macro_needs_into(types)
            {
                let macro_name = list_macro.split("::").last().unwrap();
                let args = [
                    element_type.for_rust_member(types, true, cli),
                    element_type.for_wasm_return(types),
                    array_type_ident.to_string(),
                    needs_into.to_string(),
                    element_type.is_copy(types).to_string(),
                ];
                // Emit the invocation as a sort-participating item keyed under the wrapper type it
                // defines, so it lands where the equivalent inline struct would (not hoisted to the
                // top above the file header) — see `Scope::raw_sorted`.
                self.wasm(types, array_type_ident).raw_sorted(
                    array_type_ident.as_ref(),
                    &format!("{}!({});", macro_name, args.join(", ")),
                );
                return;
            }
            let inner_type = element_type.name_as_rust_array(types, true, cli);
            let mut wrapper = create_base_wasm_struct(self, array_type_ident, false, cli);
            wrapper.s.tuple_field(None, &inner_type);
            // other functions
            let mut new_func = codegen::Function::new("new");
            new_func.vis("pub").ret("Self");
            new_func.line("Self(Vec::new())");
            wrapper.s_impl.push_fn(new_func);
            // TODO: range check stuff? where do we want to put this? or do we want to get rid of this like before?
            push_list_accessors(&mut wrapper, types, &element_type);
            wrapper.add_conversion_methods(&inner_type, cli);
            wrapper.push(self, types);
        }
    }

    /// Emit the RESTRICTED list wrapper for a `[+ elem]` array — the wasm twin of the loose list
    /// wrapper, but wrapping `core::NonEmptyVec<elem>` instead of `Vec<elem>`. Created via
    /// `try_from` (borrow + clone, so the source loose list/Vec stays valid) or `new(first)`; `add`
    /// stays infallible (a push can't break a `>= 1` bound). `wrapper_ident` is the JS class name —
    /// the synthesized `NonEmpty*List` for inline arrays, or the rule ident for a named `[+ …]`.
    fn generate_non_empty_array_type(
        &mut self,
        types: &IntermediateTypes,
        element_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`foo = [+ foo]`), so a structural-name
        // coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmpty*List` over a mapped dependency's extern
        // element is a defer candidate exactly like the loose list — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (`try_defer_wrapper`'s rule-declared guard: a named `[+ …]` rule keeps its ident,
        // which differs from the structural `NonEmpty*List`, and is never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_wrapper_name`, which cannot be called
        // here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the map twin below).
        let structural_name = format!("NonEmpty{}List", element_type.conceptual_type.for_variant());
        let shape = format!("[+ {}]", render_wrapper_shape(&element_type));
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&element_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the element itself needs (nested `[+ [+ int]]`) first
        self.ensure_non_empty_wrappers(types, &element_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let elem_rust = element_type.for_rust_member(types, true, cli);
        let inner_type = format!("NonEmptyVec<{elem_rust}>");
        // the element's structural loose-builder name; when it coincides with THIS wrapper's ident
        // (a self-named rule like `bar_list = [+ bar]`), the loose builder cannot exist — the rule
        // legitimately owns the ident for its restricted class (collision-checked in finalize), so
        // the wrapper emits WITHOUT `try_from` and is built incrementally (`new(first)` + `add`).
        let elem_wasm = element_type.for_wasm_member(types);
        let loose_list = (!element_type.directly_wasm_exposable(types)
            && !element_type.is_non_empty_array())
        .then(|| element_type.name_as_wasm_array(types));
        let self_named = loose_list.as_deref() == Some(wrapper_ident.as_ref());
        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        // Decision 11 (two-type design doc): quote the originating CDDL occurrence so the type
        // name, the doc comment, and the try_from signature are three redundant discovery signals.
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first element (`new(first)` + `add`)."
        } else {
            "Enter via `try_from` or `new(first)`."
        };
        // W2 (`--wrapper-requests`): a requested NonEmpty wrapper sets its own struct doc (above /
        // below), which would clobber the attribution doc `create_base_wasm_struct` injects, so
        // prepend the attribution here. Empty prefix (the common case) leaves output byte-identical.
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`[+ {elem_wasm}]`: at least one element, enforced by the `NonEmptyVec` \
             representation.\n{entry_doc}\n`add` can never violate the bound; removal is checked \
             in the core type."
        ));
        wrapper.s.tuple_field(None, &inner_type);
        // new(first) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first", element_type.for_wasm_param(types))
            .line(format!(
                "Self(NonEmptyVec::new({}))",
                ToWasmBoundaryOperations::format(
                    element_type
                        .from_wasm_boundary_clone(types, "first", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // add stays infallible: a push can never violate the >= 1 lower bound
        push_list_accessors(&mut wrapper, types, &element_type);
        // try_from: the single checked door from the loose form to the restricted wrapper. It
        // BORROWS (and clones) so the source loose list/Vec remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if element_type.directly_wasm_exposable(types) {
            // exposable element: no loose wrapper exists, so take the bare Vec by value (boundary copy)
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("elements", format!("Vec<{elem_wasm}>"))
                .line(
                    "NonEmptyVec::try_from(elements).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        } else if let Some(loose_list) = loose_list.filter(|_| !self_named) {
            // non-exposable, non-nested element: borrow the loose list wrapper and clone it out.
            // Make sure the loose builder exists (inline arrays already mint it; a named `[+ bar]`
            // rule may not have — minting is idempotent via `already_generated`, and a user rule
            // of incompatible shape claiming this ident was rejected at finalize). This mint runs
            // through `try_defer_wrapper` like any other, so a dep-indexed loose source DEFERS —
            // the `try_from` below then borrows the dep's class, whose import is routed at THIS
            // wrapper's emission scope by `scope_references` (the try_from reference is invisible
            // to the field walk — see `register_deferred_non_empty_list_source`).
            self.generate_array_type(
                types,
                element_type.clone(),
                &RustIdent::new(CDDLIdent::new(loose_list.clone())),
                false,
                cli,
            );
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("list", format!("&{loose_list}"))
                .line(format!(
                    "let inner: {} = list.clone().into();",
                    element_type.name_as_rust_array(types, true, cli)
                ))
                .line(
                    "NonEmptyVec::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        }
        // else: self-named rule (loose ident unavailable — see the doc comment) or a nested
        // nonempty element (no clean loose source): built incrementally via new(first)+add only.
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Emit the RESTRICTED table wrapper for a `{+ k => v}` map — the wasm twin of the loose table
    /// wrapper (`codegen_table_type`), but wrapping `core::NonEmptyMap<K, V>` instead of the raw map.
    /// Created via `try_from(&MapKToV)` (borrow + clone, so the source loose wrapper stays valid) or
    /// `new(first_key, first_value)`; `insert` stays infallible (an insert can't break a `>= 1`
    /// bound); removal is checked in the core type. `wrapper_ident` is the JS class name — the
    /// synthesized `NonEmptyMapKToV` for inline maps, or the rule ident for a named `{+ …}`. The
    /// `insert`/`get`/`has`/`keys` accessors are minted by the shared `push_table_accessors` (also
    /// used by `codegen_table_type`), delegating to `self.0`, whose `NonEmptyMap` method surface
    /// matches the raw map's `len`/`insert`/`get`/`keys`.
    fn generate_non_empty_map_type(
        &mut self,
        types: &IntermediateTypes,
        key_type: RustType,
        value_type: RustType,
        wrapper_ident: &RustIdent,
        // `true` when `wrapper_ident` is an explicit RULE ident (`m = {+ k => v}`), so a
        // structural-name coincidence never workspace-defers the consumer's own class (criterion 9).
        rule_declared: bool,
        cli: &Cli,
    ) {
        // `--extern-wrapper-index`: a synthesized `NonEmptyMap*` over a mapped dependency's extern
        // key+value is a defer candidate exactly like the loose table — if the dep owns it, import it
        // instead of re-minting a colliding `#[wasm_bindgen]` class. Only the STRUCTURAL name is a
        // candidate (rule-declared `{+ …}` rules keep their ident and are never suppressed).
        // LOCKSTEP: this spelling is deliberately the owner-INDEPENDENT structural name — the `None`
        // (no named owner) branch of `RustType::non_empty_wasm_map_wrapper_name`, which cannot be
        // called here because an owner-named wrapper must never look deferrable. If that helper's
        // synthesized spelling changes, change this format! too (and the list twin above).
        let structural_name = format!(
            "NonEmpty{}",
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type)
        );
        let shape = format!(
            "{{+ {} => {}}}",
            render_wrapper_shape(&key_type),
            render_wrapper_shape(&value_type)
        );
        if self.try_defer_wrapper(
            types,
            wrapper_ident,
            &structural_name,
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            rule_declared,
            cli,
        ) {
            return;
        }
        // mint any NonEmpty wrappers the key/value themselves need (nested `{+ …}`) first
        self.ensure_non_empty_wrappers(types, &key_type, cli);
        self.ensure_non_empty_wrappers(types, &value_type, cli);
        if !self.already_generated.insert(wrapper_ident.clone()) {
            return;
        }
        self.record_collection_wrapper(types, wrapper_ident, &shape);
        let inner_map =
            ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli);
        let inner_type = format!("NonEmptyMap<{}>", {
            // strip the leading table-type token (`BTreeMap<K, V>` / `OrderedHashMap<K, V>`) to reuse
            // the same `K, V` spelling, keeping the wrapper's inner in lockstep with the rust field.
            let open = inner_map.find('<').expect("map type has generics");
            let close = inner_map.rfind('>').expect("map type has generics");
            inner_map[open + 1..close].to_owned()
        });
        // the loose structural table wrapper (`MapKToV`) is the `try_from` source; when its ident
        // coincides with THIS wrapper's ident (a self-named rule like `map_text_to_uint = {+ …}`),
        // the loose builder cannot exist — the rule legitimately owns the ident for its restricted
        // class (collision-checked in finalize), so the wrapper emits WITHOUT `try_from` and is built
        // incrementally (`new(first_key, first_value)` + `insert`).
        let loose_ident = ConceptualRustType::name_for_wasm_map(&key_type, &value_type);
        let self_named = loose_ident.to_string() == wrapper_ident.to_string();

        let mut wrapper = create_base_wasm_struct(self, wrapper_ident, false, cli);
        let map_wasm = ConceptualRustType::name_for_wasm_map(&key_type, &value_type);
        let entry_doc = if self_named {
            "The rule name coincides with the loose builder name, so no `try_from` source class \
             exists — build incrementally from the first entry (`new(first_key, first_value)` + \
             `insert`)."
        } else {
            "Enter via `try_from` or `new(first_key, first_value)`."
        };
        let attr_prefix = self.requested_attribution_prefix(wrapper_ident);
        wrapper.s.doc(format!(
            "{attr_prefix}`{{+ k => v}}` (`{map_wasm}`): at least one entry, enforced by the \
             `NonEmptyMap` representation.\n{entry_doc}\n`insert` can never violate the bound; \
             removal is checked in the core type."
        ));
        wrapper.s.tuple_field(None, &inner_type);
        // new(first_key, first_value) — always valid (length 1)
        let mut new_func = codegen::Function::new("new");
        new_func
            .vis("pub")
            .ret("Self")
            .arg("first_key", key_type.for_wasm_param(types))
            .arg("first_value", value_type.for_wasm_param(types))
            .line(format!(
                "Self(NonEmptyMap::new({}, {}))",
                ToWasmBoundaryOperations::format(
                    key_type
                        .from_wasm_boundary_clone(types, "first_key", false)
                        .into_iter()
                ),
                ToWasmBoundaryOperations::format(
                    value_type
                        .from_wasm_boundary_clone(types, "first_value", false)
                        .into_iter()
                )
            ));
        wrapper.s_impl.push_fn(new_func);
        // len
        wrapper
            .s_impl
            .new_fn("len")
            .vis("pub")
            .ret("usize")
            .arg_ref_self()
            .line("self.0.len()");
        // insert / get / has / keys are minted by the shared `push_table_accessors` — the single
        // source of the nullable-value flattening convention, called by both this restricted twin and
        // the loose `codegen_table_type`. See that helper for the rationale comments.
        push_table_accessors(self, &mut wrapper, types, &key_type, &value_type, cli);
        // try_from: the single checked door from the loose table wrapper to the restricted wrapper.
        // It BORROWS (and clones) so the source loose `MapKToV` remains valid on the JS side, and the
        // throw happens here — right at the conversion, not inside a parent constructor.
        if !self_named {
            // ensure the loose builder exists as the `try_from` source. Inline maps already mint the
            // structural `MapKToV` via the visitor (idempotent with our mint through
            // `already_generated`), and a named `{+ …}` rule may not have — so mint it here. EXCEPT
            // when a PLAIN table rule of the same shape is the SOLE OWNER of `MapKToV`: then the loose
            // builder is that owner's class exposed as a `pub type MapKToV = <Owner>;` alias (emitted
            // by `mint_sole_owner_table`), and minting a second `pub struct MapKToV` here would clash
            // with that alias (E0428). The alias resolves to the owner, whose conversion methods make
            // `map.clone().into()` work, so sharing it is both correct and necessary.
            let shape_has_sole_owner = types
                .table_shape_sole_owners()
                .contains_key(&loose_ident.to_string());
            if !shape_has_sole_owner {
                // This mint runs through `try_defer_wrapper` like any other, so a dep-indexed loose
                // `MapKToV` source DEFERS — the `try_from` below then borrows the dep's class, whose
                // import is routed at THIS wrapper's emission scope by `scope_references` (the
                // try_from reference is invisible to the field walk — see
                // `register_deferred_non_empty_map_source`).
                codegen_table_type(
                    self,
                    types,
                    &loose_ident,
                    key_type.clone(),
                    value_type.clone(),
                    false,
                    cli,
                );
            }
            wrapper
                .s_impl
                .new_fn("try_from")
                .vis("pub")
                .ret(format!("Result<{wrapper_ident}, JsError>"))
                .arg("map", format!("&{loose_ident}"))
                .line(format!("let inner: {inner_map} = map.clone().into();"))
                .line(
                    "NonEmptyMap::try_from(inner).map(Self).map_err(|e| JsError::new(&e.to_string()))",
                );
        }
        wrapper.add_conversion_methods(&inner_type, cli);
        wrapper.push(self, types);
    }

    /// Recursively mint the restricted `NonEmpty*List` wrappers a type (at any nesting level) needs.
    /// Named `[+ …]` rules mint their own wrapper under the rule ident elsewhere, so this only fires
    /// on INLINE array shapes (conceptual `Array` carrying the `(Some(1), None)` bounds) that do NOT
    /// dedup to a named rule.
    fn ensure_non_empty_wrappers(&mut self, types: &IntermediateTypes, rt: &RustType, cli: &Cli) {
        match &rt.conceptual_type {
            ConceptualRustType::Array(inner) => {
                if rt.is_non_empty_array() {
                    // dedup-to-named: an inline `[+ elem]` whose element has a NAMED `[+ …]` rule
                    // uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here
                    if types.non_empty_named_owner(inner).is_none() {
                        let ident =
                            RustIdent::new(CDDLIdent::new(rt.non_empty_wasm_wrapper_name(types)));
                        self.generate_non_empty_array_type(
                            types,
                            (**inner).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, inner, cli);
                }
            }
            ConceptualRustType::Optional(inner) => {
                self.ensure_non_empty_wrappers(types, inner, cli)
            }
            ConceptualRustType::Map(k, v) => {
                if rt.is_non_empty_map() {
                    // dedup-to-named: an inline `{+ k => v}` whose shape has a NAMED `{+ …}` table
                    // rule uses that rule's class (minted by the rule's own variant-match) — nothing
                    // synthesized here. Its key/value still get their own nested wrappers.
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                    if types.non_empty_map_named_owner(k, v).is_none() {
                        let ident = RustIdent::new(CDDLIdent::new(
                            rt.non_empty_wasm_map_wrapper_name(types),
                        ));
                        self.generate_non_empty_map_type(
                            types,
                            (**k).clone(),
                            (**v).clone(),
                            &ident,
                            false,
                            cli,
                        );
                    }
                } else {
                    self.ensure_non_empty_wrappers(types, k, cli);
                    self.ensure_non_empty_wrappers(types, v, cli);
                }
            }
            _ => (),
        }
    }
}

/// Emit the shared wasm list-wrapper accessor triple — `len`, `get`, `add` — onto `wrapper`'s impl.
/// The loose `Vec` wrapper (`generate_array_type`) and its restricted `NonEmptyVec` twin
/// (`generate_non_empty_array_type`) deliberately expose the SAME method surface, each accessor
/// delegating to `self.0` identically, so both mint these three through here — the conventions live
/// once. Only `new` differs between the twins (loose: `Self(Vec::new())`; NonEmpty: `new(first)`),
/// so it stays at each call site (along with any site-specific rationale) and is emitted before this.
fn push_list_accessors(
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    element_type: &RustType,
) {
    wrapper
        .s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    wrapper
        .s_impl
        .new_fn("get")
        .vis("pub")
        .ret(element_type.for_wasm_return(types))
        .arg_ref_self()
        .arg("index", "usize")
        .line(element_type.to_wasm_boundary(types, "self.0[index]", false));
    wrapper
        .s_impl
        .new_fn("add")
        .vis("pub")
        .arg_mut_self()
        .arg("elem", element_type.for_wasm_param(types))
        .line(format!(
            "self.0.push({});",
            ToWasmBoundaryOperations::format(
                element_type
                    .from_wasm_boundary_clone(types, "elem", false)
                    .into_iter()
            )
        ));
}

/// Emit the shared wasm table-wrapper accessor surface — `insert`, `get`, the conditional `has`, and
/// `keys` — onto `wrapper`'s impl, together with the value-nullable machinery all four depend on. The
/// loose map wrapper (`codegen_table_type`) and its restricted `NonEmptyMap` twin
/// (`generate_non_empty_map_type`) deliberately expose the SAME method surface, each accessor
/// delegating to `self.0` identically, so both mint these through here — the nullable-value
/// flattening convention lives once. `new` differs between the twins and `len` is trivial, so both
/// stay at each call site (emitted before this); the `try_from` / conversion tails stay too.
fn push_table_accessors(
    gen_scope: &mut GenerationScope,
    wrapper: &mut WasmWrapper,
    types: &IntermediateTypes,
    key_type: &RustType,
    value_type: &RustType,
    cli: &Cli,
) {
    // A nullable value (`* uint => (T / null)` -> `Option<T>`) would make get/insert return
    // `Option<Option<T>>` — which wasm-bindgen can't represent (`Option<T>: OptionIntoWasmAbi` is not
    // satisfied). So when the value is itself an `Option`, we flatten the presence-`Option` these
    // accessors add into it and return a single `Option<T>`. This is the same convention the c-style
    // enum-getter (`add_wasm_enum_getters`) uses; native storage still holds all three states
    // (key-absent / present-null / present-value), so CBOR round-trips are unaffected — only the wasm
    // read conflates absent with present-null.
    let value_nullable = matches!(
        value_type.conceptual_type.resolve_alias_shallow(),
        ConceptualRustType::Optional(_)
    );
    let map_value_ret = || {
        if value_nullable {
            value_type.for_wasm_return(types)
        } else {
            format!("Option<{}>", value_type.for_wasm_return(types))
        }
    };
    let value_flatten = if value_nullable { ".flatten()" } else { "" };
    // When the value is nullable, the stored inner is `Option<InnerRust>`. If that inner is not
    // directly wasm-exposable (a named collection / data-enum), the boundary must convert it —
    // `.map(Into::into)` through the Option — not a blanket `.into()`, which has no
    // `From<Option<Inner>>` impl (wasm E0277/E0308).
    let value_nullable_inner_exposable = match value_type.conceptual_type.resolve_alias_shallow() {
        ConceptualRustType::Optional(inner) => {
            inner.conceptual_type.directly_wasm_exposable_ct(types)
        }
        _ => false,
    };
    // insert
    let mut insert_func = codegen::Function::new("insert");
    insert_func
        .vis("pub")
        .arg_mut_self()
        .arg("key", key_type.for_wasm_param(types))
        .arg("value", value_type.for_wasm_param(types))
        .ret(map_value_ret());
    if value_nullable {
        insert_func.doc("Returns the displaced value, or None if the key was absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    insert_func.line(format!(
        "self.0.insert({}, {}){}",
        ToWasmBoundaryOperations::format(
            key_type
                .from_wasm_boundary_clone(types, "key", false)
                .into_iter()
        ),
        ToWasmBoundaryOperations::format(
            value_type
                .from_wasm_boundary_clone(types, "value", false)
                .into_iter()
        ),
        if value_nullable {
            if value_nullable_inner_exposable {
                value_flatten.to_owned()
            } else {
                // displaced value is `Option<InnerRust>` after flatten; convert its inner to wasm.
                format!("{value_flatten}.map(Into::into)")
            }
        } else if value_type.directly_wasm_exposable(types) {
            String::new()
        } else {
            ".map(Into::into)".to_owned()
        }
    ));
    // ^ TODO: support failable types everywhere or just force it to be only a detail in the wrapper?
    wrapper.s_impl.push_fn(insert_func);
    // get
    let get_ret_modifier = if value_type.is_copy(types) {
        ""
    } else if value_nullable {
        // stored value is `Option<InnerRust>`; convert the inner across the boundary (when it is
        // not directly exposable) THROUGH the Option, yielding `Option<Option<Wrapper>>` which the
        // trailing `value_flatten` collapses to `Option<Wrapper>`.
        if value_nullable_inner_exposable {
            ".cloned()"
        } else {
            ".map(|v| v.clone().map(Into::into))"
        }
    } else if value_type.directly_wasm_exposable(types) {
        ".cloned()"
    } else {
        ".map(|v| v.clone().into())"
    };
    let mut getter = codegen::Function::new("get");
    getter
        .arg_ref_self()
        .arg("key", key_type.for_wasm_param(types))
        .ret(map_value_ret())
        .vis("pub");
    if value_nullable {
        getter.doc("Returns None if the key is absent OR present-but-null (wasm-bindgen can't represent Option<Option<T>>).");
    }
    // The is_copy value returns `.copied()`, else the boundary modifier computed above applies. The
    // two twins spelled this differently in source — codegen_table_type inlined the `if` in each key
    // branch, generate_non_empty_map_type used this closure — but produced the same bytes; the closure
    // is the single spelling here.
    let copied_or = |modifier: &str| {
        if value_type.is_copy(types) {
            ".copied()".to_owned()
        } else {
            modifier.to_owned()
        }
    };
    if key_type.directly_wasm_exposable(types) {
        getter.line(format!(
            "self.0.get({}){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
            value_flatten
        ));
    } else {
        getter.line(format!(
            "self.0.get({}.as_ref()){}{}",
            key_type.from_wasm_boundary_ref(types, "key"),
            copied_or(get_ret_modifier),
            value_flatten
        ));
    }
    wrapper.s_impl.push_fn(getter);
    // has(key): key-presence accessor, emitted from exactly the `value_nullable` flatten condition
    // above (single source of truth) so it can never drift from `get`. When the value is nullable,
    // `get` collapses Option<Option<T>> -> Option<T>, so a `None` return conflates an absent key with
    // a present-but-null one; `has` exposes the key's presence directly (a direct key lookup, not the
    // `keys()` scan that was the only recovery before). Mirrors `get`'s key-boundary handling.
    //
    // No collision check is needed here (unlike the record `has_<field>` accessor): a table wrapper's
    // method surface is entirely generator-fixed (`len`/`insert`/`get`/`has`/`keys`) with no
    // user-named methods — a map has no named fields, only key/value TYPES — so `has` cannot clash
    // with anything the spec author controls.
    if value_nullable {
        let mut has_func = codegen::Function::new("has");
        has_func
            .arg_ref_self()
            .arg("key", key_type.for_wasm_param(types))
            .ret("bool")
            .vis("pub")
            .doc("Returns whether the key is present, distinguishing an absent key from a present-but-null value (both of which `get` reports as None).");
        if key_type.directly_wasm_exposable(types) {
            has_func.line(format!(
                "self.0.get({}).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        } else {
            has_func.line(format!(
                "self.0.get({}.as_ref()).is_some()",
                key_type.from_wasm_boundary_ref(types, "key")
            ));
        }
        wrapper.s_impl.push_fn(has_func);
    }
    // keys
    let keys_type = ConceptualRustType::Array(Box::new(key_type.clone()));
    let mut keys = codegen::Function::new("keys");
    keys.arg_ref_self()
        .ret(keys_type.for_wasm_return_ct(types))
        .vis("pub");
    let key_clone = if key_type.is_copy(types) {
        ".keys().copied()"
    } else {
        ".keys().cloned()"
    };
    // R3d: decide the keys-list wrapper's deferral BEFORE emitting keys() — the keys-list emitter
    // (`generate_array_type`) may run AFTER this map class, so consulting `deferred_wrappers` alone
    // would miss it. `try_defer_wrapper` is idempotent, so this both records the decision (the later
    // emitter re-runs it, suppresses, and the import is routed) and drives the `.into()` here.
    let keys_deferred = !keys_type.directly_wasm_exposable_ct(types)
        && gen_scope.try_defer_wrapper(
            types,
            &RustIdent::new(CDDLIdent::new(key_type.name_as_wasm_array(types))),
            &key_type.name_as_wasm_array(types),
            &[&key_type.conceptual_type],
            &format!("[* {}]", render_wrapper_shape(key_type)),
            false,
            cli,
        );
    if keys_type.directly_wasm_exposable_ct(types) {
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>()"));
    } else if keys_deferred {
        // R3d: the keys-list wrapper is deferred to a dependency (`--extern-wrapper-index`); its tuple
        // field is private cross-crate, so build it through `From<Vec<_>>` (`.into()`) instead of
        // tuple-struct syntax.
        keys.line(format!("self.0{key_clone}.collect::<Vec<_>>().into()"));
    } else {
        keys.line(format!(
            "{}(self.0{key_clone}.collect::<Vec<_>>())",
            keys_type.for_wasm_return_ct(types)
        ));
    }
    wrapper.s_impl.push_fn(keys);
}

fn canonical_param(cli: &Cli) -> &'static str {
    if cli.canonical_form {
        ", force_canonical"
    } else {
        ""
    }
}

/// the codegen crate doesn't support proc macros for fields so we need to
/// do this with newlines. codegen takes care of indentation somehow.
fn encoding_var_macros(key_demand: Option<DemandSet>, custom_json: bool, cli: &Cli) -> String {
    let mut ret = if let Some(demand) = key_demand {
        format!(
            "#[derivative({})]\n",
            key_trait_list(demand, true, cli)
                .iter()
                .map(|derive| format!("{derive}=\"ignore\""))
                .collect::<Vec<String>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    if cli.json_serde_derives && !custom_json {
        ret.push_str("#[serde(skip)]\n");
    }
    ret
}

#[derive(Debug, Clone)]
enum BlockOrLine {
    Line(String),
    Block(Block),
}

#[derive(Default, Debug, Clone)]
pub(crate) struct BlocksOrLines(Vec<BlockOrLine>);

impl BlocksOrLines {
    fn as_single_line(&self) -> Option<&str> {
        match self.0.len() {
            1 => match &self.0[0] {
                BlockOrLine::Line(line) => Some(line),
                BlockOrLine::Block(_) => None,
            },
            _ => None,
        }
    }
}

impl From<Block> for BlocksOrLines {
    fn from(block: Block) -> Self {
        Self(vec![BlockOrLine::Block(block)])
    }
}

pub(crate) trait CodeBlock {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock;

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock;

    fn push_all(&mut self, contents: BlocksOrLines) -> &mut dyn CodeBlock
    where
        Self: Sized,
    {
        for content in contents.0 {
            match content {
                BlockOrLine::Line(line) => self.line(&line),
                BlockOrLine::Block(block) => self.push_block(block),
            };
        }
        self as &mut dyn CodeBlock
    }
}

impl CodeBlock for codegen::Function {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for Block {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.line(line)
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.push_block(block)
    }
}

impl CodeBlock for BlocksOrLines {
    fn line(&mut self, line: &str) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Line(line.to_owned()));
        self
    }

    fn push_block(&mut self, block: Block) -> &mut dyn CodeBlock {
        self.0.push(BlockOrLine::Block(block));
        self
    }
}

trait DataType {
    fn derive(&mut self, derive: &str) -> &mut Self;
}

impl DataType for codegen::Struct {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

impl DataType for codegen::Enum {
    fn derive(&mut self, derive: &str) -> &mut Self {
        self.derive(derive)
    }
}

fn create_base_rust_struct(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
    manual_json_impl: bool,
    cli: &Cli,
) -> (codegen::Struct, codegen::Impl) {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    add_struct_derives(
        &mut s,
        types.key_demand(ident),
        false,
        false,
        manual_json_impl,
        cli,
    );
    let group_impl = codegen::Impl::new(name);
    // TODO: anything here?
    (s, group_impl)
}

/// Formatted string for fully scoped rust crate struct for use from wasm crate
pub fn rust_crate_struct_from_wasm(
    types: &IntermediateTypes<'_>,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    format!(
        "{}::{}",
        rust_crate_struct_scope_from_wasm(types, ident, cli),
        ident
    )
}

pub fn rust_crate_struct_scope_from_wasm(
    types: &IntermediateTypes,
    ident: &RustIdent,
    cli: &Cli,
) -> String {
    let scope = types.scope(ident);
    if *scope == *ROOT_SCOPE {
        cli.lib_name_code()
    } else if !scope.export() {
        // A non-exported (cross-crate extern-dep) scope already stores the dependency's crate as its
        // leading component (the `_CDDL_CODEGEN_EXTERN_DEPS_DIR_` prefix is stripped by
        // `ModuleScope::from`), so `dep_crate::sub` is the dep's own rust path. Prefixing the
        // generated crate's own lib name would mint `cddl_lib::dep_crate::sub`, a path that exists in
        // no crate. The rust type lives in the dep's rust crate regardless of the wasm-crate mapping.
        scope.to_string()
    } else {
        format!("{}::{}", cli.lib_name_code(), scope)
    }
}

#[derive(Debug)]
struct WasmWrapper<'a> {
    ident: &'a RustIdent,
    s: codegen::Struct,
    s_impl: codegen::Impl,
    // rust -> wasm
    from_wasm: Option<codegen::Impl>,
    // wasm -> rust
    from_native: Option<codegen::Impl>,
    // AsRef
    as_ref: Option<codegen::Impl>,
    // (macro name, macro params)
    macros: Vec<(String, Vec<String>)>,
}

impl<'a> WasmWrapper<'a> {
    fn push(mut self, gen_scope: &mut GenerationScope, types: &IntermediateTypes) {
        // using Scope::raw() for the macro calls would result in them all being included at the top of the
        // file, so we instead use the impl's macro spot to put them before the impl where we want them.
        // (For a standalone invocation with no impl to attach to — the --wasm-list-macro case — the
        // equivalent is Scope::raw_sorted, which sorts the text where a struct of that name would.)
        for (full_name, params) in self.macros {
            let macro_name = full_name.split("::").last().unwrap();
            self.s_impl
                .r#macro(format!("{}!({});\n", macro_name, params.join(", ")));
        }
        self.s_impl.r#macro("#[wasm_bindgen]");
        gen_scope
            .wasm(types, self.ident)
            .push_struct(self.s)
            .push_impl(self.s_impl);
        if let Some(from_wasm) = self.from_wasm {
            gen_scope.wasm(types, self.ident).push_impl(from_wasm);
        }
        if let Some(from_native) = self.from_native {
            gen_scope.wasm(types, self.ident).push_impl(from_native);
        }
        if let Some(as_ref) = self.as_ref {
            gen_scope.wasm(types, self.ident).push_impl(as_ref);
        }
    }

    /// native_name is &str since we need to possibly prepend namespacing
    /// and where we're calling it we'd have to construct a RustType where we
    /// didn't have to before, but we already had the string.
    fn add_conversion_methods(&mut self, native_name: &str, cli: &Cli) {
        match &cli.wasm_conversions_macro {
            Some(conversion_macro) => {
                self.macros.push((
                    conversion_macro.clone(),
                    vec![native_name.to_owned(), self.ident.to_string()],
                ));
            }
            None => {
                let mut from_wasm = codegen::Impl::new(self.ident.to_string());
                from_wasm
                    .impl_trait(format!("From<{native_name}>"))
                    .new_fn("from")
                    .arg("native", native_name)
                    .ret("Self")
                    .line("Self(native)");
                self.from_wasm = Some(from_wasm);
                let mut from_native = codegen::Impl::new(native_name);
                from_native
                    .impl_trait(format!("From<{}>", self.ident))
                    .new_fn("from")
                    .arg("wasm", self.ident.to_string())
                    .ret("Self")
                    .line("wasm.0");
                self.from_native = Some(from_native);
                let mut as_ref = codegen::Impl::new(self.ident.to_string());
                as_ref
                    .impl_trait(format!("AsRef<{native_name}>"))
                    .new_fn("as_ref")
                    .arg_ref_self()
                    .ret(format!("&{native_name}"))
                    .line("&self.0");
                self.as_ref = Some(as_ref);
            }
        }
    }
}

fn create_base_wasm_struct<'a>(
    gen_scope: &GenerationScope,
    ident: &'a RustIdent,
    exists_in_rust: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    let name = &ident.to_string();
    let mut s = codegen::Struct::new(name);
    s.vis("pub")
        .derive("Clone")
        .derive("Debug")
        .attr("wasm_bindgen");
    // W2 (`--wrapper-requests`): a requested wrapper carries a `/// Generated at the request of: …`
    // attribution doc. Set here so the loose list / map emitters (which set no struct doc of their
    // own) carry it; the NonEmpty emitters set their own struct doc and PREPEND this text via
    // `requested_attribution_prefix` (a `.doc()` call replaces, not appends). Empty map off the flag,
    // so own-spec wrappers are byte-identical.
    if let Some(doc) = gen_scope.requested_attribution.get(ident) {
        s.doc(doc);
    }
    let mut s_impl = codegen::Impl::new(name);
    let mut macros = Vec::new();
    // There are auto-implementing ToCBORBytes and FromBytes traits, but unfortunately
    // wasm_bindgen right now can't export traits, so we export this functionality
    // as a non-trait function.
    if exists_in_rust {
        match &cli.wasm_cbor_json_api_macro {
            Some(cbor_json_macro) => {
                macros.push((cbor_json_macro.clone(), vec![name.to_owned()]));
            }
            None => {
                if cli.to_from_bytes_methods {
                    let mut to_bytes = codegen::Function::new("to_cbor_bytes");
                    to_bytes.ret("Vec<u8>").arg_ref_self().vis("pub");
                    if cli.preserve_encodings && cli.canonical_form {
                        to_bytes.line(format!(
                            "{}::serialization::Serialize::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                        let mut to_canonical_bytes =
                            codegen::Function::new("to_canonical_cbor_bytes");
                        to_canonical_bytes
                            .ret("Vec<u8>")
                            .arg_ref_self()
                            .vis("pub")
                            .line("Serialize::to_canonical_cbor_bytes(&self.0)");
                    } else {
                        to_bytes.line(format!(
                            "{}::serialization::ToCBORBytes::to_cbor_bytes(&self.0)",
                            cli.common_import_wasm()
                        ));
                    }
                    s_impl.push_fn(to_bytes);
                    if gen_scope.deserialize_generated(ident) {
                        s_impl
                            .new_fn("from_cbor_bytes")
                            .ret(format!("Result<{name}, JsError>"))
                            .arg("cbor_bytes", "&[u8]")
                            .vis("pub")
                            .line(format!(
                                "{}::serialization::Deserialize::from_cbor_bytes(cbor_bytes).map(Self).map_err(|e| JsError::new(&format!(\"from_bytes: {{}}\", e)))",
                                cli.common_import_wasm()));
                    }
                }
                if cli.json_serde_derives {
                    let mut to_json = codegen::Function::new("to_json");
                    to_json
                        .ret("Result<String, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde_json::to_string_pretty(&self.0).map_err(|e| JsError::new(&format!(\"to_json: {}\", e)))");
                    s_impl.push_fn(to_json);
                    let mut to_json_value = codegen::Function::new("to_json_value");
                    to_json_value
                        .ret("Result<JsValue, JsError>")
                        .arg_ref_self()
                        .vis("pub")
                        .line("serde::Serialize::serialize(&self.0, &serde_wasm_bindgen::Serializer::json_compatible()).map_err(|e| JsError::new(&format!(\"to_js_value: {}\", e)))");
                    s_impl.push_fn(to_json_value);
                    s_impl
                        .new_fn("from_json")
                        .ret(format!("Result<{name}, JsError>"))
                        .arg("json", "&str")
                        .vis("pub")
                        .line("serde_json::from_str(json).map(Self).map_err(|e| JsError::new(&format!(\"from_json: {}\", e)))");
                }
            }
        }
    }
    WasmWrapper {
        ident,
        s,
        s_impl,
        from_wasm: None,
        from_native: None,
        as_ref: None,
        macros,
    }
}

/// default_structure will have it be a DIRECT wrapper with a tuple field of rust_lib::{ident}
/// this will include generating to/from traits automatically
fn create_base_wasm_wrapper<'a>(
    gen_scope: &GenerationScope,
    types: &IntermediateTypes<'_>,
    ident: &'a RustIdent,
    default_structure: bool,
    cli: &Cli,
) -> WasmWrapper<'a> {
    assert!(cli.wasm);
    let mut base = create_base_wasm_struct(gen_scope, ident, true, cli);
    if default_structure {
        let native_name = rust_crate_struct_from_wasm(types, ident, cli);
        base.s.tuple_field(None, &native_name);
        base.add_conversion_methods(&native_name, cli);
    }
    base
}

pub fn table_type(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "OrderedHashMap"
    } else {
        "BTreeMap"
    }
}

/// The CDDL prelude spelling of a primitive, for the canonical shape renderer. Kept in lockstep with
/// the wasm-map/list structural naming: the dep re-parses a rendered shape and must derive the SAME
/// structural name, so each primitive renders to a CDDL name whose `for_variant` round-trips (e.g.
/// `uint` -> `U64` -> `MapU64To…`). `u8`/`i8`/… are cddl-codegen's own sized-int spellings.
fn primitive_cddl_name(p: &Primitive) -> &'static str {
    match p {
        Primitive::Bool => "bool",
        Primitive::F64 => "float64",
        Primitive::F32 => "float32",
        Primitive::U8 => "u8",
        Primitive::I8 => "i8",
        Primitive::U16 => "u16",
        Primitive::I16 => "i16",
        Primitive::U32 => "u32",
        Primitive::I32 => "i32",
        Primitive::U64 => "uint",
        Primitive::I64 => "i64",
        Primitive::N64 => "nint",
        Primitive::Str => "text",
        Primitive::Bytes => "bytes",
    }
}

/// Render a collection wrapper's CDDL shape fragment in the canonical W1 shape-column grammar —
/// `[* foo]` / `[+ foo]` for loose / non-empty lists, `{* k => v}` / `{+ k => v}` for maps, nesting
/// recursively (`[* [* foo]]`, `[* [+ foo]]`). Element idents are the dependency's own spec spelling
/// (snake_case of the rust ident, matching the extern-stub naming a dep re-parses after
/// normalization); primitives render as their CDDL prelude name. The occurrence marker is taken from
/// the `RustType`'s own bounds so nested non-empty shapes are honored at every level. This is the
/// single shape renderer shared by the not-in-index warning hint and (later) the request-sidecar
/// machinery, so its output is EXACTLY the format a dep parses back.
pub(crate) fn render_wrapper_shape(rt: &RustType) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            let occ = if rt.is_non_empty_array() { "+" } else { "*" };
            format!("[{occ} {}]", render_wrapper_shape(inner))
        }
        ConceptualRustType::Map(key, value) => {
            let occ = if rt.is_non_empty_map() { "+" } else { "*" };
            format!(
                "{{{occ} {} => {}}}",
                render_wrapper_shape(key),
                render_wrapper_shape(value)
            )
        }
        // An optional isn't itself a wrapper occurrence — render its inner shape (only reachable via
        // nesting; the top-level constituents the callers pass are Array/Map/named-leaf).
        ConceptualRustType::Optional(inner) => render_wrapper_shape(inner),
        ConceptualRustType::Rust(ident) => convert_to_snake_case(ident.as_ref()),
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => {
            convert_to_snake_case(ident.as_ref())
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(name), _) => name.clone(),
        ConceptualRustType::Primitive(p) => primitive_cddl_name(p).to_owned(),
        // Fixed values carry no CDDL ident and never appear as a real wrapper element; render a
        // placeholder rather than panicking so the advisory hint text stays best-effort.
        ConceptualRustType::Fixed(_) => "_".to_owned(),
    }
}

/// The top-level NAMED rust idents of a wrapper constituent (element / key / value) — what the defer
/// decision resolves to a dependency scope. Primitives / fixed values contribute none; an alias
/// contributes its aliased ident; an optional passes through to its inner type.
fn named_constituent_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => named_constituent_idents(&inner.conceptual_type),
        _ => vec![],
    }
}

/// The TRANSITIVE named leaf idents of a wrapper constituent — `named_constituent_idents` extended to
/// descend through nested `Array`/`Map` conceptual types to the named types at the leaves. A
/// `[* [* foo]]` has leaf `foo` (its inner wrapper is classified independently); `{* a => [* b]}` has
/// leaves `a` and `b`. Primitives / fixed values contribute none; alias / optional unwrap to their
/// inner. This is what workspace placement resolves to dependency owners.
fn transitive_named_leaf_idents(ty: &ConceptualRustType) -> Vec<RustIdent> {
    match ty {
        ConceptualRustType::Rust(ident) => vec![ident.clone()],
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => vec![ident.clone()],
        ConceptualRustType::Optional(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Array(inner) => transitive_named_leaf_idents(&inner.conceptual_type),
        ConceptualRustType::Map(key, value) => {
            let mut out = transitive_named_leaf_idents(&key.conceptual_type);
            out.extend(transitive_named_leaf_idents(&value.conceptual_type));
            out
        }
        _ => vec![],
    }
}

/// The set of element OWNERS of a wrapper's constituents, computed transitively to the named leaves.
/// Each leaf resolves to `Some(dep)` when it is an extern type (leading component of its non-exported
/// scope) or `None` when it is a consumer-owned (exported) type. An empty set means "ownerless" (no
/// named leaves — a primitives-only wrapper like `{* uint => text}`). This is the input to
/// `wrapper_placement`.
fn transitive_owner_set(
    types: &IntermediateTypes,
    constituents: &[&ConceptualRustType],
) -> BTreeSet<Option<String>> {
    let mut owners = BTreeSet::new();
    for c in constituents {
        for id in transitive_named_leaf_idents(c) {
            let scope = types.scope(&id);
            owners.insert(if scope.export() {
                None
            } else {
                scope.components().first().cloned()
            });
        }
    }
    owners
}

/// Where a collection wrapper is hosted, given its transitive element owners. Factored as one
/// function so the placement rule can generalize (plan decision 4): today `Borrow(dep)` iff the
/// wrapper has EXACTLY ONE owner, that owner is a named dependency, and that dependency is a
/// `--workspace-dep`; every other case (ownerless, mixed-dep, a lone non-workspace owner, any
/// consumer-owned leaf) is `Local`. The future rule ("latest of the element owners" / least upper
/// bound in a DAG) replaces this body without touching call sites.
enum WrapperPlacement {
    Local,
    Borrow(String),
}

fn wrapper_placement(
    owners: &BTreeSet<Option<String>>,
    workspace_deps: &BTreeSet<String>,
) -> WrapperPlacement {
    if owners.len() == 1
        && let Some(Some(dep)) = owners.iter().next()
        && workspace_deps.contains(dep)
    {
        return WrapperPlacement::Borrow(dep.clone());
    }
    WrapperPlacement::Local
}

/// Validate `--workspace-dep` values (plan decision 6) and return the set. Each named dep must be a
/// configured extern dependency (`extern_dep_names()`) AND have an `--extern-wasm-crate` mapping —
/// the deferral imports and the sidecar's `use` lines both need the wasm crate name, so a missing
/// mapping is a hard error rather than a silent fallback. Mirrors `load_extern_wrapper_indices`'
/// startup hardening. The accessor already rejected empty / `=`-bearing values.
fn load_workspace_deps(types: &IntermediateTypes, cli: &Cli) -> BTreeSet<String> {
    let deps = cli.workspace_deps();
    if deps.is_empty() {
        return BTreeSet::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let wasm_crate_map = cli.extern_wasm_crate_map();
    for dep in &deps {
        if !extern_dep_names.contains(dep) {
            panic!(
                "--workspace-dep names dependency {dep:?}, which is not an extern dependency in this \
                 spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        if !wasm_crate_map.contains_key(dep) {
            panic!(
                "--workspace-dep {dep:?} has no --extern-wasm-crate mapping; workspace deferral needs \
                 the dep's wasm crate name for its imports and the borrowed-collections sidecar. Add \
                 --extern-wasm-crate {dep}=<wasm_crate>."
            );
        }
    }
    deps
}

// ===== W2 dep side (`--wrapper-requests`): shape reconstruction + structural naming ===============

/// Reverse of `primitive_cddl_name`: the `Primitive` a shape-column leaf denotes, or `None` for a
/// named-type leaf. Only the exact spellings `render_wrapper_shape` emits for primitive leaves are
/// recognized, so a dep type whose snake-case happens NOT to be a prelude name is correctly treated
/// as a named element.
fn primitive_from_cddl_name(name: &str) -> Option<Primitive> {
    Some(match name {
        "bool" => Primitive::Bool,
        "float64" => Primitive::F64,
        "float32" => Primitive::F32,
        "u8" => Primitive::U8,
        "i8" => Primitive::I8,
        "u16" => Primitive::U16,
        "i16" => Primitive::I16,
        "u32" => Primitive::U32,
        "i32" => Primitive::I32,
        "uint" => Primitive::U64,
        "i64" => Primitive::I64,
        "nint" => Primitive::N64,
        "text" => Primitive::Str,
        "bytes" => Primitive::Bytes,
        _ => return None,
    })
}

/// Whether this dep's OWN spec defines `ident` (a generated struct/enum or a user type alias) as an
/// exported, in-crate type. A non-exported (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/…`) scope means the type
/// belongs to one of the DEP's own deps, not the dep itself, so it is NOT owned.
fn dep_owns_element(types: &IntermediateTypes, ident: &RustIdent) -> bool {
    let known = types.rust_struct(ident).is_some()
        || types
            .type_aliases()
            .contains_key(&AliasIdent::Rust(ident.clone()));
    known && types.scope(ident).export()
}

/// Reconstruct a requested wrapper's `RustType` from its canonical shape column, resolving each
/// named leaf against the DEP's own IR after the same normalization (`RustIdent::new`, which
/// camel-cases and folds `-`/`_`) type-name derivation uses. A leaf the dep does not own is a hard
/// error (criterion 8 #1). `consumer`/`path`/`listed_name` are threaded only for actionable errors.
fn parse_requested_shape(
    types: &IntermediateTypes,
    shape: &str,
    consumer: &str,
    path: &str,
    listed_name: &str,
) -> RustType {
    let chars: Vec<char> = shape.chars().collect();
    let mut pos = 0;
    let rt = parse_shape_fragment(
        types,
        &chars,
        &mut pos,
        consumer,
        path,
        shape,
        listed_name,
        0,
    );
    while pos < chars.len() && chars[pos].is_whitespace() {
        pos += 1;
    }
    if pos != chars.len() {
        panic!(
            "--wrapper-requests {consumer} ({path}): trailing content after the shape {shape:?} \
             (wrapper {listed_name:?})."
        );
    }
    rt
}

/// Depth cap for `parse_shape_fragment`'s recursion. Real wrapper shapes nest 2–3 deep; 32 is a
/// generous ceiling that turns a pathological hand-edited sidecar (thousands of `[* [* …]]` levels)
/// into an actionable hard error instead of a stack-overflow abort.
const MAX_SHAPE_DEPTH: usize = 32;

#[allow(clippy::too_many_arguments)]
fn parse_shape_fragment(
    types: &IntermediateTypes,
    chars: &[char],
    pos: &mut usize,
    consumer: &str,
    path: &str,
    shape: &str,
    listed_name: &str,
    depth: usize,
) -> RustType {
    let skip_ws = |pos: &mut usize| {
        while *pos < chars.len() && chars[*pos].is_whitespace() {
            *pos += 1;
        }
    };
    let bad = |what: &str| -> ! {
        panic!(
            "--wrapper-requests {consumer} ({path}): malformed shape {shape:?} (wrapper \
             {listed_name:?}): {what}."
        );
    };
    if depth > MAX_SHAPE_DEPTH {
        panic!(
            "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
             (shape {shape:?}) nests collections deeper than the supported limit of \
             {MAX_SHAPE_DEPTH}. Real wrapper shapes nest only a few levels; this is almost \
             certainly a malformed hand-edited sidecar."
        );
    }
    skip_ws(pos);
    if *pos >= chars.len() {
        bad("unexpected end of shape");
    }
    match chars[*pos] {
        '[' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let inner = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != ']' {
                bad("expected `]`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Array(Box::new(inner)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        '{' => {
            *pos += 1;
            skip_ws(pos);
            let occ = read_occurrence(chars, pos).unwrap_or_else(|| bad("expected `*` or `+`"));
            skip_ws(pos);
            let key = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if !(chars.get(*pos) == Some(&'=') && chars.get(*pos + 1) == Some(&'>')) {
                bad("expected `=>`");
            }
            *pos += 2;
            skip_ws(pos);
            let value = parse_shape_fragment(
                types,
                chars,
                pos,
                consumer,
                path,
                shape,
                listed_name,
                depth + 1,
            );
            skip_ws(pos);
            if *pos >= chars.len() || chars[*pos] != '}' {
                bad("expected `}`");
            }
            *pos += 1;
            let rt = RustType::new(ConceptualRustType::Map(Box::new(key), Box::new(value)));
            if occ == '+' {
                rt.with_bounds((Some(1), None))
            } else {
                rt
            }
        }
        _ => {
            // A named or primitive leaf: read the ident token.
            let start = *pos;
            while *pos < chars.len()
                && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
            {
                *pos += 1;
            }
            if *pos == start {
                bad("expected an element type name");
            }
            let token: String = chars[start..*pos].iter().collect();
            if let Some(p) = primitive_from_cddl_name(&token) {
                return RustType::new(ConceptualRustType::Primitive(p));
            }
            // A reserved CDDL keyword (`biguint`, `bigint`, …) or reserved Rust type name
            // (`option` → `Option`) as a leaf token would trip `RustIdent::new`'s internal asserts
            // — an internal panic reachable only from a hand-edited sidecar (a real consumer never
            // emits these). Pre-check through the reservation rule's one owner
            // (`RustIdent::reserved_reason`, the same predicate `new` asserts on) so external
            // input surfaces the feature's own hard error instead of the assert.
            if RustIdent::reserved_reason(&token).is_some() {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) uses the reserved identifier {token:?} as a wrapper element; \
                     reserved CDDL keywords and reserved Rust type names cannot be wrapper elements."
                );
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            if !dep_owns_element(types, &ident) {
                panic!(
                    "--wrapper-requests {consumer} ({path}): the requested wrapper {listed_name:?} \
                     (shape {shape:?}) references the element type {token:?}, which this dep does not \
                     own. The consumer's extern stub for this dep and the dep's own spec disagree — \
                     the request cannot be satisfied."
                );
            }
            // Resolve through the pipeline's one alias-substitution rule (`resolve_alias`, shared
            // with `new_type` so this path cannot drift from pipeline resolution): a leaf left as
            // a bare `Rust(ident)` naming an alias (`stake_credential = credential`, `policy_id =
            // script_hash`) panics downstream lookups (`is_enum`, exposability, member naming)
            // that assume `Rust(ident)` names a registered struct. The `Alias` wrapper the rule
            // keeps for rust-alias-generating rules preserves the requested ident for structural
            // naming (the consumer derived `StakeCredentialList` from the alias name) while
            // resolving storage/exposability through the target, matching what the dep's own
            // generation of the same CDDL shape would produce. `dep_owns_element` already required
            // a spec-registered ident, so `new_type`'s unregistered-reserved prelude fallback (the
            // one mutable part) cannot be needed here.
            types
                .resolve_alias(&AliasIdent::Rust(ident.clone()))
                .unwrap_or_else(|| RustType::new(ConceptualRustType::Rust(ident)))
        }
    }
}

/// Read a `*`/`+` occurrence marker at `chars[*pos]`, advancing past it.
fn read_occurrence(chars: &[char], pos: &mut usize) -> Option<char> {
    match chars.get(*pos) {
        Some('*') => {
            *pos += 1;
            Some('*')
        }
        Some('+') => {
            *pos += 1;
            Some('+')
        }
        _ => None,
    }
}

/// The owner-INDEPENDENT structural wrapper name for a reconstructed requested shape — the exact
/// spelling the consumer's emitter passed to `try_defer_wrapper` and recorded in its sidecar. Uses
/// the raw `NonEmpty*List` / `NonEmpty<MapKToV>` forms (NOT `non_empty_wasm_wrapper_name`, which
/// consults named owners) so a dep that authored a `[+ …]` rule surfaces as a name↔shape/own-spec
/// disagreement rather than silently matching. Panics for a non-collection top level (a hand-edited
/// sidecar row).
fn requested_structural_name(
    types: &IntermediateTypes,
    rt: &RustType,
    consumer: &str,
    path: &str,
) -> String {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if rt.is_non_empty_array() {
                format!("NonEmpty{}List", inner.conceptual_type.for_variant())
            } else {
                inner.conceptual_type.name_as_wasm_array_ct(types)
            }
        }
        ConceptualRustType::Map(k, v) => {
            if rt.is_non_empty_map() {
                format!("NonEmpty{}", ConceptualRustType::name_for_wasm_map(k, v))
            } else {
                ConceptualRustType::name_for_wasm_map(k, v).to_string()
            }
        }
        other => panic!(
            "--wrapper-requests {consumer} ({path}): a requested shape must be a collection wrapper \
             (list or map), got {other:?}."
        ),
    }
}

/// If a reconstructed requested shape is DIRECTLY WASM-EXPOSABLE (it lowers to a bare `Vec<…>` with
/// no wrapper class), return that member spelling; otherwise `None`. Mirrors `name_as_wasm_array_ct`'s
/// own exposability test exactly (rebuild `Array(inner)` and ask `directly_wasm_exposable_ct`) rather
/// than sniffing a rendered string. A `Map` top level is never directly exposable; a `[+ …]` NonEmpty
/// array always gets a wrapper class, so only the loose-array (`[* …]`) case can be exposable.
fn requested_exposable_member(types: &IntermediateTypes, rt: &RustType) -> Option<String> {
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) if !rt.is_non_empty_array() => {
            if ConceptualRustType::Array(Box::new(inner.conceptual_type.clone().into()))
                .directly_wasm_exposable_ct(types)
            {
                Some(inner.conceptual_type.name_as_wasm_array_ct(types))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Describe how this dep resolves each NAMED leaf element written in a requested shape's shape column,
/// for the actionable exposable-shape / name↔shape diagnostics. Walks the ORIGINAL shape tokens (not
/// the reconstructed `RustType`, which has already substituted `@no_alias` idents away) so the message
/// names the ident the operator wrote and its resolution target. Primitive leaves contribute nothing.
/// Only reached after a successful `parse_requested_shape`, so every named token is an owned,
/// non-reserved ident — `RustIdent::new` cannot trip.
fn requested_shape_leaf_resolutions(types: &IntermediateTypes, shape: &str) -> Vec<String> {
    let chars: Vec<char> = shape.chars().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-' {
            let start = i;
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-')
            {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            if primitive_from_cddl_name(&token).is_some() {
                continue;
            }
            let ident = RustIdent::new(CDDLIdent::new(token.clone()));
            out.push(describe_leaf_resolution(types, &token, &ident));
        } else {
            i += 1;
        }
    }
    out
}

/// One leaf's resolution phrase: a registered struct, a kept alias (rust alias preserving the ident),
/// or a transparent (`@no_alias` / passthrough) substitution to its base. Consults `type_aliases()`,
/// the same table `parse_shape_fragment`'s leaf arm resolves through.
fn describe_leaf_resolution(types: &IntermediateTypes, token: &str, ident: &RustIdent) -> String {
    match types.type_aliases().get(&AliasIdent::Rust(ident.clone())) {
        Some(info) => {
            let target = render_wrapper_shape(&info.base_type);
            if info.gen_rust_alias {
                format!("`{token}` (a kept alias resolving to `{target}`)")
            } else {
                format!("`{token}` (transparently substituted to `{target}`)")
            }
        }
        None => format!("`{token}` (a registered struct)"),
    }
}

/// The immediate nested collection shapes of a requested wrapper (canonical form), used for the
/// inner-closure integrity check (criterion 8 #5). Only ONE level: deeper nesting is covered
/// transitively because each level is a separately-requested (and separately-checked) entry.
fn inner_collection_shapes(rt: &RustType) -> Vec<String> {
    let is_collection = |rt: &RustType| {
        matches!(
            rt.conceptual_type,
            ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
        )
    };
    let mut out = Vec::new();
    match &rt.conceptual_type {
        ConceptualRustType::Array(inner) => {
            if is_collection(inner) {
                out.push(render_wrapper_shape(inner));
            }
        }
        ConceptualRustType::Map(k, v) => {
            if is_collection(k) {
                out.push(render_wrapper_shape(k));
            }
            if is_collection(v) {
                out.push(render_wrapper_shape(v));
            }
        }
        _ => {}
    }
    out
}

/// Parse every `--extern-wrapper-index <dep>=<path>` file into `dep -> {wrapper class names}`. Each
/// file is a dependency's committed `generated/collections.rs`: `pub use <path>::<Name>;` lines (plus
/// blank / `//` comment lines). Any other non-blank line is a hard error — the format is ours, and a
/// silently-tolerated stray line would let a malformed index disable deferral and reintroduce the
/// duplicate-symbol link error. Mapping keys are validated against `extern_dep_names()` first (a typo
/// there has the same silent-disable failure mode), mirroring `--extern-wasm-crate`.
fn load_extern_wrapper_indices(
    types: &IntermediateTypes,
    cli: &Cli,
) -> BTreeMap<String, BTreeSet<String>> {
    let files = cli.extern_wrapper_index_files();
    if files.is_empty() {
        return BTreeMap::new();
    }
    let extern_dep_names = types.extern_dep_names();
    let mut out = BTreeMap::new();
    for (dep, path) in files {
        if !extern_dep_names.contains(&dep) {
            panic!(
                "--extern-wrapper-index names dependency {dep:?}, which is not an extern dependency \
                 in this spec. Known extern dependencies: {extern_dep_names:?}"
            );
        }
        let contents = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("--extern-wrapper-index {dep}={path}: cannot read the index file: {e}")
        });
        let mut names = BTreeSet::new();
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            // Fixed shape: `pub use <path>::<Name>;` — take the segment after the last `::`.
            let name = line
                .strip_prefix("pub use ")
                .and_then(|rest| rest.strip_suffix(';'))
                .and_then(|path| path.rsplit("::").next())
                .filter(|name| {
                    !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_')
                });
            match name {
                Some(name) => {
                    names.insert(name.to_owned());
                }
                None => panic!(
                    "--extern-wrapper-index {dep}={path}: unexpected line {line:?}; the index is a \
                     generated `collections.rs` of `pub use <path>::<Name>;` re-export lines"
                ),
            }
        }
        out.insert(dep, names);
    }
    out
}

/// Mint the wasm structural wrapper class for a single visited `ConceptualRustType` (the per-type body
/// of the wasm-wrapper visit). Shared by the rust-struct walk and the wasm-alias-target walk so both
/// reach identical minting decisions (sole-owner routing, map-key array wrappers). Idempotent via
/// `wasm_wrappers_generated`; every class body is derived purely from the shape, so the result is
/// iteration-order-independent.
fn mint_wasm_wrapper_for_visited_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    ty: &ConceptualRustType,
    wasm_wrappers_generated: &mut BTreeSet<String>,
    table_shape_sole_owner: &BTreeMap<String, RustIdent>,
    cli: &Cli,
) {
    match ty {
        ConceptualRustType::Array(elem) => {
            if !ty.directly_wasm_exposable_ct(types) {
                let array_ident = elem.name_as_wasm_array(types);
                if wasm_wrappers_generated.insert(array_ident.clone()) {
                    gen_scope.generate_array_type(
                        types,
                        *elem.clone(),
                        &RustIdent::new(CDDLIdent::new(array_ident)),
                        false,
                        cli,
                    );
                }
            }
        }
        ConceptualRustType::Map(k, v) => {
            let map_ident = ConceptualRustType::name_for_wasm_map(k, v);
            match table_shape_sole_owner.get(&map_ident.to_string()) {
                // A single named rule owns this shape: this embedded/resolved use
                // shares that rule-named class (JS-visible under the CDDL
                // identifier) rather than minting an anonymous structural class.
                Some(owner) => mint_sole_owner_table(
                    gen_scope,
                    types,
                    owner,
                    &map_ident,
                    wasm_wrappers_generated,
                    cli,
                ),
                // Anonymous-only shape (or a same-shape rule pair): mint the
                // structural class, whose inner is the raw map (not a rust rule).
                None => {
                    if wasm_wrappers_generated.insert(map_ident.to_string()) {
                        codegen_table_type(
                            gen_scope,
                            types,
                            &map_ident,
                            *k.clone(),
                            *v.clone(),
                            false,
                            cli,
                        );
                    }
                }
            }
            if !ConceptualRustType::Array(Box::new(*k.clone())).directly_wasm_exposable_ct(types) {
                let keys_ident = k.name_as_wasm_array(types);
                if wasm_wrappers_generated.insert(keys_ident.clone()) {
                    gen_scope.generate_array_type(
                        types,
                        *k.clone(),
                        &RustIdent::new(CDDLIdent::new(keys_ident)),
                        false,
                        cli,
                    );
                }
            }
        }
        _ => (),
    }
}

/// Mint the JS-visible class for a table shape whose SOLE owner is the named rule `owner`, plus a
/// `pub type <structural> = <owner>;` alias so structural-name reference sites (an anonymous `Map`'s
/// `for_wasm_member`, `@newtype` inner getters, cross-module `mark_refs` imports) still resolve —
/// wasm_bindgen exports no type aliases, so it folds the alias onto the `owner` class in the JS ABI.
/// Idempotent via `generated` (which records BOTH the rule name and the structural name), so the
/// visit arm and the Table arm converge to identical output regardless of which reaches the shape
/// first. The class body always derives from the OWNER's declared `(domain, range)`, keeping the
/// output iteration-order-independent.
fn mint_sole_owner_table(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    owner: &RustIdent,
    structural_ident: &RustIdent,
    generated: &mut BTreeSet<String>,
    cli: &Cli,
) {
    if generated.insert(owner.to_string()) {
        let (domain, range) = {
            let owner_struct = types
                .rust_structs()
                .get(owner)
                .expect("sole owner of a table shape must be a rust struct");
            match owner_struct.variant() {
                RustStructType::Table { domain, range, .. } => (domain.clone(), range.clone()),
                _ => unreachable!("sole owner of a table shape must be a Table rust struct"),
            }
        };
        // `exists_in_rust = true`: the inner is the rust crate's `pub type <owner>` alias (exactly the
        // struct-field role's inner), not the raw inline map. Any CBOR tag on the owner is honored by
        // that rust type's serialization, so it is not threaded into this wasm wrapper.
        codegen_table_type(gen_scope, types, owner, domain, range, true, cli);
    }
    // Structural alias in the SAME module as the class (`owner`'s scope). Skip a self-alias when the
    // rule ident already equals the structural name.
    if *structural_ident != *owner && generated.insert(structural_ident.to_string()) {
        gen_scope
            .wasm(types, owner)
            .push_type_alias(TypeAlias::new(structural_ident, owner).vis("pub").clone());
    }
}

#[allow(clippy::too_many_arguments)]
fn codegen_table_type(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    name: &RustIdent,
    key_type: RustType,
    value_type: RustType,
    exists_in_rust: bool,
    cli: &Cli,
) {
    assert!(cli.wasm);
    // `--extern-wrapper-index`: only the anonymous STRUCTURAL map wrapper (`!exists_in_rust`, name ==
    // `name_for_wasm_map`) is a defer candidate — a rule-owned class (`exists_in_rust`) is the
    // consumer's own type. If a mapped dependency owns this exact structural map wrapper, defer to it
    // (import from the dep's `collections` module) instead of re-minting a duplicate class.
    let shape = format!(
        "{{* {} => {}}}",
        render_wrapper_shape(&key_type),
        render_wrapper_shape(&value_type)
    );
    if !exists_in_rust
        && gen_scope.try_defer_wrapper(
            types,
            name,
            ConceptualRustType::name_for_wasm_map(&key_type, &value_type).as_ref(),
            &[&key_type.conceptual_type, &value_type.conceptual_type],
            &shape,
            // Only the anonymous STRUCTURAL map wrapper reaches here (`!exists_in_rust`); a
            // rule-declared table is screened out above and never a defer candidate.
            false,
            cli,
        )
    {
        return;
    }
    // Idempotency guard, unified with the array wrappers' `already_generated`: the loose structural
    // `MapKToV` builder can be requested BOTH by the wasm-wrapper visitor (a plain `{* k => v}` use)
    // AND directly by `generate_non_empty_map_type` (as a `{+ k => v}` wrapper's `try_from` source);
    // without a shared guard those two paths would double-define the class (E0428). The callers' own
    // dedup sets (`wasm_wrappers_generated` / `generated`) remain — this only ADDS protection, so
    // every existing single-mint path stays byte-identical (the guard passes on first request).
    if !gen_scope.already_generated.insert(name.clone()) {
        return;
    }
    gen_scope.record_collection_wrapper(types, name, &shape);
    // No `tag` parameter: this emits ONLY the wasm wrapper class (accessors + delegation). When the
    // shape has a CBOR tag (`#6.n({ ... })`), the tag is owned entirely by the rust crate's type,
    // which this wrapper's single tuple field holds (via `rust_crate_struct_from_wasm` when
    // `exists_in_rust`); that type's serialize/deserialize writes/checks the tag. The wrapper adds no
    // serialization of its own, so it has nothing to do with the tag — hence the caller's tag is not
    // threaded here.
    // Special-class (major type 7) keys used to be asserted away here, but the break-byte
    // ambiguity they alluded to lives in the rust-side deserialize loop, which
    // `make_deser_loop_break_check` now handles (definite lengths read exactly `n` entries; the
    // indefinite case errors gracefully). This wasm wrapper emits only accessors — nothing here
    // depends on the key's CBOR class.
    let mut wrapper = create_base_wasm_struct(gen_scope, name, false, cli);

    let inner_type = if exists_in_rust {
        rust_crate_struct_from_wasm(types, name, cli)
    } else {
        ConceptualRustType::name_for_rust_map(types, &key_type, &value_type, true, cli)
    };
    wrapper.s.tuple_field(None, &inner_type);
    // new
    let mut new_func = codegen::Function::new("new");
    new_func
        .vis("pub")
        .ret("Self")
        .line(format!("Self({}::new())", table_type(cli)));
    wrapper.s_impl.push_fn(new_func);
    // len
    wrapper
        .s_impl
        .new_fn("len")
        .vis("pub")
        .ret("usize")
        .arg_ref_self()
        .line("self.0.len()");
    // insert / get / has / keys (and the nullable-value flattening convention they share) are minted
    // by `push_table_accessors`, also called by the restricted `NonEmptyMap` twin
    // (`generate_non_empty_map_type`).
    push_table_accessors(gen_scope, &mut wrapper, types, &key_type, &value_type, cli);
    wrapper.add_conversion_methods(&inner_type, cli);
    wrapper.push(gen_scope, types);
}

#[derive(Debug)]
struct EncodingField {
    field_name: String,
    type_name: String,
    /// this MUST be equivalent to the Default trait of the encoding field.
    /// This can be more concise though e.g. None for Option<T>::default()
    default_expr: &'static str,
    enc_conversion_before: &'static str,
    enc_conversion_after: &'static str,
    is_copy: bool,
}

impl EncodingField {
    pub fn enc_conversion(&self, expr: &str) -> String {
        format!(
            "{}{}{}",
            self.enc_conversion_before, expr, self.enc_conversion_after
        )
    }
}

fn key_encoding_field(name: &str, key: &FixedValue) -> EncodingField {
    match key {
        FixedValue::Text(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "StringEncoding".to_owned(),
            default_expr: "StringEncoding::default()",
            enc_conversion_before: "StringEncoding::from(",
            enc_conversion_after: ")",
            is_copy: false,
        },
        FixedValue::Uint(_) => EncodingField {
            field_name: format!("{name}_key_encoding"),
            type_name: "Option<cbor_event::Sz>".to_owned(),
            default_expr: "None",
            enc_conversion_before: "Some(",
            enc_conversion_after: ")",
            is_copy: true,
        },
        _ => unimplemented!(),
    }
}

fn encoding_fields(
    types: &IntermediateTypes,
    name: &str,
    ty: &RustType,
    include_default: bool,
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    // TODO: how do we handle defaults for nested things? e.g. inside of a ConceptualRustType::Map
    let mut encs = encoding_fields_impl(types, name, ty.into(), cli);
    if include_default && ty.config.default.is_some() {
        encs.push(EncodingField {
            field_name: format!("{name}_default_present"),
            type_name: "bool".to_owned(),
            default_expr: "false",
            enc_conversion_before: "",
            enc_conversion_after: "",
            is_copy: true,
        });
    }
    encs
}

fn encoding_fields_impl(
    types: &IntermediateTypes,
    name: &str,
    ty: SerializingRustType,
    cli: &Cli,
) -> Vec<EncodingField> {
    assert!(cli.preserve_encodings);
    match ty {
        SerializingRustType::Root(ConceptualRustType::Array(elem_ty), _cfg) => {
            let base = EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
            };
            let inner_encs =
                encoding_fields_impl(types, &format!("{name}_elem"), (&**elem_ty).into(), cli);
            if inner_encs.is_empty() {
                vec![base]
            } else {
                let type_name_elem = tuple_type_name(&inner_encs);
                vec![
                    base,
                    EncodingField {
                        field_name: format!("{name}_elem_encodings"),
                        type_name: format!("Vec<{type_name_elem}>"),
                        default_expr: "Vec::new()",
                        enc_conversion_before: "",
                        enc_conversion_after: "",
                        is_copy: false,
                    },
                ]
            }
        }
        SerializingRustType::Root(ConceptualRustType::Map(k, v), _cfg) => {
            let mut encs = vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "LenEncoding".to_owned(),
                default_expr: "LenEncoding::default()",
                enc_conversion_before: "",
                enc_conversion_after: "",
                is_copy: true,
            }];
            let key_encs = encoding_fields_impl(types, &format!("{name}_key"), (&**k).into(), cli);
            let val_encs =
                encoding_fields_impl(types, &format!("{name}_value"), (&**v).into(), cli);

            if !key_encs.is_empty() {
                let type_name_value = tuple_type_name(&key_encs);
                encs.push(EncodingField {
                    field_name: format!("{name}_key_encodings"),
                    type_name: format!(
                        "BTreeMap<{}, {}>",
                        k.for_rust_member(types, false, cli),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
                });
            }

            if !val_encs.is_empty() {
                let type_name_value = tuple_type_name(&val_encs);
                encs.push(EncodingField {
                    field_name: format!("{name}_value_encodings"),
                    type_name: format!(
                        "BTreeMap<{}, {}>",
                        k.for_rust_member(types, false, cli),
                        type_name_value
                    ),
                    default_expr: "BTreeMap::new()",
                    enc_conversion_before: "",
                    enc_conversion_after: "",
                    is_copy: false,
                });
            }
            encs
        }
        SerializingRustType::Root(ConceptualRustType::Primitive(p), _cfg) => match p {
            Primitive::Bytes | Primitive::Str => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "StringEncoding".to_owned(),
                default_expr: "StringEncoding::default()",
                enc_conversion_before: "StringEncoding::from(",
                enc_conversion_after: ")",
                is_copy: false,
            }],
            Primitive::I8
            | Primitive::I16
            | Primitive::I32
            | Primitive::I64
            | Primitive::N64
            | Primitive::U8
            | Primitive::U16
            | Primitive::U32
            | Primitive::U64
            | Primitive::F32
            | Primitive::F64 => vec![EncodingField {
                field_name: format!("{name}_encoding"),
                type_name: "Option<cbor_event::Sz>".to_owned(),
                default_expr: "None",
                enc_conversion_before: "Some(",
                enc_conversion_after: ")",
                is_copy: true,
            }],
            Primitive::Bool =>
            /* bool only has 1 encoding */
            {
                vec![]
            }
        },
        SerializingRustType::Root(ConceptualRustType::Fixed(f), _cfg) => match f {
            FixedValue::Bool(_) | FixedValue::Null => vec![],
            FixedValue::Nint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::I64)).into(),
                cli,
            ),
            FixedValue::Uint(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::U64)).into(),
                cli,
            ),
            FixedValue::Float(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::F64)).into(),
                cli,
            ),
            FixedValue::Text(_) => encoding_fields_impl(
                types,
                name,
                (&ConceptualRustType::Primitive(Primitive::Str)).into(),
                cli,
            ),
        },
        SerializingRustType::Root(ConceptualRustType::Alias(_, ty), _cfg) => {
            encoding_fields_impl(types, name, (&**ty).into(), cli)
        }
        SerializingRustType::Root(ConceptualRustType::Optional(ty), _cfg) => {
            encoding_fields(types, name, ty, false, cli)
        }
        SerializingRustType::Root(ConceptualRustType::Rust(rust_ident), _cfg) => {
            match &types.rust_struct(rust_ident).unwrap().variant() {
                // for c-style enums we push those up to where they are used instead of self-containing
                RustStructType::CStyleEnum { variants } => {
                    // earlier we are guaranteed that all variants will have the same encoding types
                    // or else it wouldn't end up as a c-style enum in the first place in IntermediateTypes
                    encoding_fields(types, name, variants[0].rust_type(), false, cli)
                }
                // also push them out for RawBytesType as they're not stored there, as if we had `bytes` directly here
                RustStructType::RawBytesType => encoding_fields_impl(
                    types,
                    name,
                    (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                    cli,
                ),
                // no encodings here. they're contained inside the struct
                _ => vec![],
            }
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::Tagged(tag), child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_tag"),
                (&ConceptualRustType::Fixed(FixedValue::Uint(*tag))).into(),
                cli,
            );
            encs.append(&mut encoding_fields_impl(types, name, *child, cli));
            encs
        }
        SerializingRustType::EncodingOperation(CBOREncodingOperation::CBORBytes, child) => {
            let mut encs = encoding_fields_impl(
                types,
                &format!("{name}_bytes"),
                (&ConceptualRustType::Primitive(Primitive::Bytes)).into(),
                cli,
            );
            encs.append(&mut encoding_fields_impl(types, name, *child, cli));
            encs
        }
    }
}

fn encoding_var_names_str(
    types: &IntermediateTypes,
    field_name: &str,
    rust_type: &RustType,
    cli: &Cli,
) -> String {
    assert!(cli.preserve_encodings);
    let resolved_rust_type = rust_type.clone().resolve_aliases();
    let mut var_names = if resolved_rust_type.is_fixed_value() {
        vec![]
    } else {
        vec![field_name.to_owned()]
    };
    for enc in encoding_fields(types, field_name, &resolved_rust_type, false, cli).into_iter() {
        var_names.push(enc.field_name);
    }

    if var_names.len() > 1 {
        format!("({})", var_names.join(", "))
    } else {
        var_names.join(", ")
    }
}

// Value-level twin of `tuple_type_name`: joins encoding VAR names into a parenthesized tuple.
fn tuple_str(strs: Vec<String>) -> String {
    if strs.len() > 1 {
        format!("({})", strs.join(", "))
    } else {
        strs.join(", ")
    }
}

// Type-level twin of `tuple_str`: joins encoding fields' `type_name`s into a parenthesized tuple
// type unless there is exactly one (then the lone type_name stands alone, unparenthesized).
fn tuple_type_name(encs: &[EncodingField]) -> String {
    if encs.len() == 1 {
        encs[0].type_name.clone()
    } else {
        format!(
            "({})",
            encs.iter()
                .map(|enc| enc.type_name.clone())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// True iff every encoding field's `default_expr` is a trivial literal (`None`/`false`) rather than
/// a function call (`LenEncoding::default()`, `Vec::new()`, `BTreeMap::new()`,
/// `StringEncoding::default()`). Trivial-literal tuple defaults may be emitted with `unwrap_or(..)`;
/// a call-bearing default must stay behind `unwrap_or_else(|| ..)` or clippy::or_fun_call fires.
/// Centralized so every tuple-default emission site agrees on the same decision.
fn encoding_defaults_all_trivial(encoding_fields: &[EncodingField]) -> bool {
    encoding_fields
        .iter()
        .all(|enc| matches!(enc.default_expr, "None" | "false"))
}

fn cbor_event_len_n(n: &str, cli: &Cli) -> String {
    if cli.preserve_encodings {
        format!("cbor_event::LenSz::Len({n}, _)")
    } else {
        format!("cbor_event::Len::Len({n})")
    }
}

fn cbor_event_len_indef(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "cbor_event::LenSz::Indefinite"
    } else {
        "cbor_event::Len::Indefinite"
    }
}

/// How to construct a `CBORReadLen` from the freshly-read `len`. In preserve mode `len` is a
/// `cbor_event::LenSz`, matching `CBORReadLen::new`. In non-preserve mode `len` is a
/// `cbor_event::Len`; going through `From<cbor_event::Len>` (instead of `new`) lets the same
/// emission compile against either runtime flavor — crucially a preserve-flavored
/// `--common-import-override` target (e.g. cml_core), whose `new` takes `LenSz`. Preserve stays on
/// `new` because such external cores expose no `From<LenSz>`.
fn cbor_read_len_ctor(cli: &Cli) -> &'static str {
    if cli.preserve_encodings {
        "CBORReadLen::new"
    } else {
        "CBORReadLen::from"
    }
}

/// First name in a deterministic candidate sequence that does NOT collide with a defined type
/// ident: `base` (`"W"`/`"R"`), then `base+suffix` (`"WSer"`/`"RDe"`), then `base+suffix+index`
/// (`"WSer0"`, `"WSer1"`, …). The bare `base` wins whenever nothing is named it, so a spec with no
/// `w`/`r` collision keeps the historical `"W"`/`"R"` names and the snapshot corpus does not churn.
fn pick_generic_name(
    taken: &std::collections::BTreeSet<String>,
    base: &str,
    suffix: &str,
) -> String {
    if !taken.contains(base) {
        return base.to_string();
    }
    let combined = format!("{base}{suffix}");
    if !taken.contains(&combined) {
        return combined;
    }
    (0..)
        .map(|i| format!("{base}{suffix}{i}"))
        .find(|candidate| !taken.contains(candidate))
        .expect("infinite candidate sequence always yields a free name")
}

fn make_encoding_struct(encoding_name: &str) -> codegen::Struct {
    let mut encoding_struct = codegen::Struct::new(encoding_name.to_string());
    encoding_struct
        .vis("pub")
        .derive("Clone")
        .derive("Debug")
        .derive("Default");
    encoding_struct
}

/// clippy's default `type-complexity-threshold`. A type in a lint-scored position (struct field, fn
/// signature, ...) whose structural score exceeds this trips `clippy::type_complexity`. Type
/// *aliases* are not scored by the lint, so hoisting an over-threshold encoding-struct field type
/// into a `pub type` alias silences it without an `#[allow]` and without changing any emitted bytes
/// or round-trip semantics.
const TYPE_COMPLEXITY_THRESHOLD: u64 = 250;

/// Reproduce clippy's `type_complexity` scoring closely enough to decide, deterministically,
/// whether an emitted encoding field type would trip the lint. clippy walks the type and adds
/// `10 * nest` for every path / tuple / array / slice / reference node, incrementing `nest` by one
/// when descending into that node's children. The emitted encoding types use only paths (`Foo`,
/// `Foo<..>`, `a::b`) and tuples (no refs/slices), so scoring those node kinds suffices.
/// Over-estimating here is harmless (it only mints an extra alias); the clippy gate is the backstop
/// if the real boundary ever shifts.
fn type_complexity_score(ty: &str) -> u64 {
    /// Split `s` on top-level `delim` (bracket depth 0 over `<>` and `()`), trimming each piece.
    fn split_top_level(s: &str, delim: char) -> Vec<&str> {
        let mut depth = 0i32;
        let mut parts = Vec::new();
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => depth -= 1,
                c if c == delim && depth == 0 => {
                    parts.push(s[start..i].trim());
                    start = i + c.len_utf8();
                }
                _ => {}
            }
        }
        parts.push(s[start..].trim());
        parts
    }
    /// True iff every prefix of `s` has non-negative `<>`/`()` depth and the whole is balanced —
    /// i.e. an outermost `(...)` pair actually wraps the entire string.
    fn is_balanced(s: &str) -> bool {
        let mut depth = 0i32;
        for c in s.chars() {
            match c {
                '<' | '(' => depth += 1,
                '>' | ')' => {
                    depth -= 1;
                    if depth < 0 {
                        return false;
                    }
                }
                _ => {}
            }
        }
        depth == 0
    }
    fn score(ty: &str, nest: u64) -> u64 {
        let ty = ty.trim();
        // Parenthesized: a tuple (>=2 top-level elements) is one node whose elements are children;
        // a single `(T)` grouping is just `T` (no HIR node); `()` is a unit.
        if let Some(inner) = ty
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .filter(|inner| is_balanced(inner))
        {
            let parts = split_top_level(inner, ',');
            return if inner.trim().is_empty() {
                1 // unit ()
            } else if parts.len() >= 2 {
                10 * nest + parts.iter().map(|p| score(p, nest + 1)).sum::<u64>()
            } else {
                score(inner, nest) // grouping, not a tuple
            };
        }
        // Path with generics `Ident<..>` / `a::b::Ident<..>`: one node, generic args are children.
        if let (Some(open), Some(close)) = (ty.find('<'), ty.rfind('>')) {
            let args = &ty[open + 1..close];
            return 10 * nest
                + split_top_level(args, ',')
                    .iter()
                    .map(|a| score(a, nest + 1))
                    .sum::<u64>();
        }
        // Plain path node (`u64`, `LenEncoding`, `cbor_event::Sz`, ...).
        10 * nest
    }
    score(ty, 1)
}

/// Add one field to an encoding struct, hoisting an over-`type_complexity` field type into a
/// deterministic `pub type <Owner><FieldCamel> = ..;` alias in the same `cbor_encodings` scope so
/// `clippy::type_complexity` stays quiet without an `#[allow]`. Alias names can't collide with each
/// other: `owner` (the owning encoding struct's base type name) is distinct per struct and
/// `field_name` is distinct within a struct, so identical anonymous shapes in different rules never
/// collide. An alias CAN in principle collide with another rule's encoding-struct name:
/// owner `Foo` + field `bar_encoding` aliases to `FooBarEncoding`, which a rule named `foo-bar`
/// also claims. That needs an over-threshold field AND the exact sibling rule name, and it fails
/// LOUD (E0428 in the generated crate, caught by every compile gate), so it is not disambiguated
/// preemptively.
/// Aliases are collected (not pushed) so the caller can push them into the scope alongside the
/// struct.
fn push_encoding_struct_field(
    encoding_struct: &mut codegen::Struct,
    aliases: &mut Vec<(String, String)>,
    owner: &RustIdent,
    field_name: &str,
    type_name: &str,
) {
    let field_type = if type_complexity_score(type_name) > TYPE_COMPLEXITY_THRESHOLD {
        let alias = format!("{}{}", owner, convert_to_camel_case(field_name));
        aliases.push((alias.clone(), type_name.to_owned()));
        alias
    } else {
        type_name.to_owned()
    };
    encoding_struct.field(format!("pub {field_name}"), field_type);
}

// `annotated` - true iff deser_func is the body of an `.annotate(ident)` error closure: emit
// locationless errors and let the closure supply the name (the per-error annotate/named forms
// would get the name prepended AGAIN by the closure, reading "Name.Name"). When false, each error
// carries the name itself, as no closure will add it.
pub(crate) fn generate_tag_check(
    deser_func: &mut dyn CodeBlock,
    ident: &RustIdent,
    tag: Option<usize>,
    annotated: bool,
) {
    if let Some(tag) = tag {
        if annotated {
            deser_func.line("let tag = raw.tag()?;");
        } else {
            deser_func.line(&format!(
                "let tag = raw.tag().map_err(|e| DeserializeError::from(e).annotate(\"{ident}\"))?;"
            ));
        }
        let mut tag_check = Block::new(format!("if tag != {tag}"));
        if annotated {
            tag_check.line(format!("return Err(DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}.into());"));
        } else {
            tag_check.line(format!("return Err(DeserializeError::new(\"{ident}\", DeserializeFailure::TagMismatch{{ found: tag, expected: {tag} }}));"));
        }
        deser_func.push_block(tag_check);
    }
}

// This is used mostly for when thing are tagged have specific ranges.
#[allow(clippy::too_many_arguments)]
fn generate_wrapper_struct(
    gen_scope: &mut GenerationScope,
    types: &IntermediateTypes,
    type_name: &RustIdent,
    field_type: &RustType,
    min_max: Option<(Option<i128>, Option<i128>)>,
    float_min_max: Option<crate::intermediate::FloatWindow>,
    struct_config: &RustStructConfig,
    cli: &Cli,
) {
    if min_max.is_some() || float_min_max.is_some() {
        assert!(types.can_new_fail(type_name));
    }
    // The inner-value getter name: an explicit `@newtype <name>` renames it, otherwise every
    // wrapper (bare tag, plain `@newtype`, bounded/range) exposes the inner value under `get`.
    let getter_name = match struct_config.newtype_getter.as_ref() {
        Some(Some(name)) => name.as_str(),
        _ => "get",
    };
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, type_name, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        wasm_new
            .arg("inner", field_type.for_wasm_param(types))
            .vis("pub");

        // Delegate to the rust wrapper's `new`, mirroring the enum-variant wasm ctor: convert the
        // wasm inner to the rust inner (fallibility, if any, lives in the rust `new`, so pass
        // can_fail = false here), then let the rust ctor produce the native wrapper. Building
        // `Self(inner.into())` directly would need two chained `.into()`s for a Rust-typed inner
        // (wasm→native inner, then native inner→native wrapper) with an uninferable middle type.
        let from_wasm_expr = field_type.from_wasm_boundary_clone(types, "inner", false);
        let ctor = format!(
            "{}::new({})",
            rust_crate_struct_from_wasm(types, type_name, cli),
            ToWasmBoundaryOperations::format(from_wasm_expr.into_iter())
        );
        if types.can_new_fail(type_name) {
            // you can't use Self in a parameter in wasm_bindgen for some reason
            wasm_new
                .ret(format!("Result<{type_name}, JsError>"))
                .line(format!("{ctor}.map(Into::into).map_err(Into::into)"));
        } else {
            wasm_new.ret("Self").line(format!("Self({ctor})"));
        }
        wrapper.s_impl.push_fn(wasm_new);
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub")
            .arg_ref_self()
            .ret(field_type.for_wasm_return(types))
            .line(field_type.to_wasm_boundary(types, &format!("self.0.{getter_name}()"), false));
        wrapper.s_impl.push_fn(get);
        wrapper.push(gen_scope, types);
    }

    // TODO: do we want to get rid of the rust struct and embed the tag / min/max size here?
    // The tag is easy but the min/max size would require error types in any place that sets/modifies these in other structs.
    let (mut s, mut s_impl) = create_base_rust_struct(types, type_name, true, cli);
    let (inner_var, self_var) = if cli.preserve_encodings {
        ("inner", "self.inner")
    } else {
        ("0", "self.0")
    };

    // manual JSON impls
    let mut serde_ser_impl = codegen::Impl::new(type_name);
    let mut serde_deser_impl = codegen::Impl::new(type_name);
    let mut json_schema_impl = codegen::Impl::new(type_name);
    let json_hex_bytes = matches!(
        field_type.resolve_alias_shallow(),
        ConceptualRustType::Primitive(Primitive::Bytes)
    );
    let json_schema_type = if json_hex_bytes {
        Cow::Borrowed("String")
    } else {
        Cow::Owned(field_type.for_rust_member(types, false, cli))
    };

    if !struct_config.custom_json {
        // serde Serialize / Deserialize
        if cli.json_serde_derives {
            let mut serde_ser_fn = codegen::Function::new("serialize");
            serde_ser_fn
                .generic("S")
                .bound("S", "serde::Serializer")
                .arg_ref_self()
                .arg("serializer", "S")
                .ret("Result<S::Ok, S::Error>");
            let mut serde_deser_fn = codegen::Function::new("deserialize");
            serde_deser_fn
                .generic("D")
                .bound("D", "serde::de::Deserializer<'de>")
                .arg("deserializer", "D")
                .ret("Result<Self, D::Error>");
            if json_hex_bytes {
                serde_ser_fn.line(format!(
                    "serializer.serialize_str(&hex::encode({self_var}.clone()))"
                ));
                let err_body = "{ serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid hex bytes\") }";
                serde_deser_fn
                    .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
                    .line("hex::decode(&s)");
                if types.can_new_fail(type_name) {
                    serde_deser_fn
                        .line(format!(
                            ".ok().and_then(|bytes| {type_name}::new(bytes).ok())"
                        ))
                        .line(format!(".ok_or_else(|| {err_body})"));
                } else {
                    serde_deser_fn
                        .line(format!(".map({type_name}::new)"))
                        .line(format!(".map_err(|_e| {err_body})"));
                }
            } else {
                serde_ser_fn.line(format!("{self_var}.serialize(serializer)"));
                serde_deser_fn
                    .line(format!("let inner = <{json_schema_type} as serde::de::Deserialize>::deserialize(deserializer)?;"));
                if types.can_new_fail(type_name) {
                    let unexpected = match field_type.resolve_alias_shallow() {
                        ConceptualRustType::Alias(_, _) => unreachable!(),
                        ConceptualRustType::Array(_) => "Seq",
                        ConceptualRustType::Fixed(fixed) => match fixed {
                            FixedValue::Bool(_) => "Bool(inner)",
                            FixedValue::Float(_) => "Float(inner)",
                            FixedValue::Nint(_) => "Signed(inner as i64)",
                            FixedValue::Null => "Option",
                            FixedValue::Text(_) => "Str(&inner)",
                            FixedValue::Uint(_) => "Unsigned(inner)",
                        },
                        ConceptualRustType::Map(_, _) => "Map",
                        ConceptualRustType::Optional(_) => "Option",
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bool => "Bool(inner)",
                            Primitive::Bytes => "Bytes(&inner)",
                            Primitive::F32 => "Float(inner as f64)",
                            Primitive::F64 => "Float(inner)",
                            Primitive::I8
                            | Primitive::I16
                            | Primitive::I32
                            | Primitive::I64
                            | Primitive::N64 => "Signed(inner as i64)",
                            Primitive::Str => "Str(&inner)",
                            Primitive::U8 | Primitive::U16 | Primitive::U32 => {
                                "Unsigned(inner as u64)"
                            }
                            Primitive::U64 => "Unsigned(inner)",
                        },
                        ConceptualRustType::Rust(_) => "StructVariant",
                    };
                    // Unexpected::Str(&inner)/Bytes(&inner) borrow `inner` in the error closure,
                    // but `Self::new(inner)` moves it first (String/Vec aren't Copy) → E0382. Clone
                    // into the constructor in that case so the original survives for the error. The
                    // other (Copy) variants need no clone.
                    let new_arg = if unexpected.contains("&inner") {
                        "inner.clone()"
                    } else {
                        "inner"
                    };
                    serde_deser_fn
                        .line(format!("Self::new({new_arg})"))
                        .line(format!(".map_err(|_e| {{ serde::de::Error::invalid_value(serde::de::Unexpected::{unexpected}, &\"invalid {type_name}\") }})"));
                } else {
                    serde_deser_fn.line("Ok(Self::new(inner))");
                }
            }
            serde_ser_impl
                .impl_trait("serde::Serialize")
                .push_fn(serde_ser_fn);
            serde_deser_impl
                .impl_trait("serde::de::Deserialize<'de>")
                .generic("'de")
                .push_fn(serde_deser_fn);
        }

        // JsonSchema
        if cli.json_schema_export {
            let mut schema_name_fn = codegen::Function::new("schema_name");
            schema_name_fn
                .ret("::std::borrow::Cow<'static, str>")
                .line(format!("::std::borrow::Cow::Borrowed(\"{type_name}\")"));
            let mut json_schema_fn = codegen::Function::new("json_schema");
            json_schema_fn
                .arg("generator", "&mut schemars::SchemaGenerator")
                .ret("schemars::Schema")
                // qualified-path form: `json_schema_type` is a type-position spelling, so a generic
                // backing type (map/array @newtype) needs `<T as Trait>::method`, not `T::method`
                // (which parses `<` as a comparison in expression position). Matches the
                // `<{json_schema_type} as serde::de::Deserialize>::deserialize` precedent above.
                .line(format!(
                    "<{json_schema_type} as schemars::JsonSchema>::json_schema(generator)"
                ));
            let mut inline_schema = codegen::Function::new("inline_schema");
            inline_schema.ret("bool").line(format!(
                "<{json_schema_type} as schemars::JsonSchema>::inline_schema()"
            ));
            json_schema_impl
                .impl_trait("schemars::JsonSchema")
                .push_fn(schema_name_fn)
                .push_fn(json_schema_fn)
                .push_fn(inline_schema);
        }
    }
    s.vis("pub");
    let encoding_name = RustIdent::new(CDDLIdent::new(format!("{type_name}Encoding")));
    let enc_fields = if cli.preserve_encodings {
        // PRIVATE, matching the default profile's private tuple field: a pub `inner` would let
        // downstream code literal-construct or mutate the wrapper, bypassing the bound check
        // `new()` enforces. Access goes through the getter (same as default); `serialization.rs`
        // is a child module so it still reads/constructs the field directly.
        s.field("inner", field_type.for_rust_member(types, false, cli));
        let enc_fields = encoding_fields(
            types,
            "inner",
            &field_type.clone().resolve_aliases(),
            true,
            cli,
        );

        if !enc_fields.is_empty() {
            s.field(
                format!(
                    "{}pub encodings",
                    encoding_var_macros(types.key_demand(type_name), true, cli)
                ),
                format!("Option<{encoding_name}>"),
            );
            let mut encoding_struct = make_encoding_struct(encoding_name.as_ref());
            let mut encoding_aliases: Vec<(String, String)> = Vec::new();
            for field_enc in &enc_fields {
                push_encoding_struct_field(
                    &mut encoding_struct,
                    &mut encoding_aliases,
                    type_name,
                    &field_enc.field_name,
                    &field_enc.type_name,
                );
            }
            let enc_scope = gen_scope.cbor_encodings(types, type_name);
            for (alias, target) in encoding_aliases {
                enc_scope.push_type_alias(TypeAlias::new(&alias, &target).vis("pub").clone());
            }
            enc_scope.push_struct(encoding_struct);
        }
        Some(enc_fields)
    } else {
        s.tuple_field(None, field_type.for_rust_member(types, false, cli));
        None
    };
    // TODO: is there a way to know if the encoding object is also copyable?
    if field_type.is_copy(types) && !cli.preserve_encodings {
        s.derive("Copy");
    }
    {
        let mut get = codegen::Function::new(getter_name);
        get.vis("pub").arg_ref_self();
        if field_type.is_copy(types) {
            get.ret(field_type.for_rust_member(types, false, cli))
                .line(field_type.clone_if_not_copy(types, self_var));
        } else {
            get.ret(format!(
                "&{}",
                field_type.for_rust_member(types, false, cli)
            ))
            .line(format!("&{self_var}"));
        }
        s_impl.push_fn(get);
    }
    let mut ser_func = make_serialization_function("serialize", &gen_scope.serialize_generic, cli);
    let mut ser_impl = make_serialization_impl(type_name.as_ref(), cli);
    gen_scope.generate_serialize(
        types,
        field_type.into(),
        &mut ser_func,
        SerializeConfig::new(self_var, "inner")
            .is_end(true)
            .encoding_var_in_option_struct("self.encodings"),
        cli,
    );
    ser_impl.push_fn(ser_func);
    let mut deser_func =
        make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
    let mut deser_impl = codegen::Impl::new(type_name.to_string());
    deser_impl.impl_trait("Deserialize");
    if let ConceptualRustType::Rust(id) = &field_type.conceptual_type
        && types.is_plain_group(id)
    {
        unimplemented!(
            "TODO: make len/read_len variables of appropriate sizes so the generated code compiles"
        );
    }
    let mut new_func = codegen::Function::new("new");
    new_func
        .arg("inner", field_type.for_rust_move(types, cli))
        .vis("pub");
    let var_names_str = if cli.preserve_encodings {
        encoding_var_names_str(types, "inner", field_type, cli)
    } else {
        "inner".to_owned()
    };
    // nint is stored as its u64 magnitude, and magnitude is a *decreasing* function of the value, so
    // a value bound maps to a SWAPPED magnitude bound (`nint_bounds_to_u64`) — the same transform the
    // struct-field / setter paths apply. Without it the wrapper's `new()`/deserialize check compares
    // the u64 `inner` against a negative literal (does not compile: E0600) with inverted semantics.
    let min_max = if matches!(
        &field_type.conceptual_type,
        ConceptualRustType::Primitive(Primitive::N64)
    ) && !field_type
        .encodings
        .contains(&CBOREncodingOperation::CBORBytes)
    {
        min_max.map(|mm| nint_bounds_to_u64(&mm))
    } else {
        min_max
    };
    // The whole deserialize() body is accumulated here so it can be wrapped in one
    // `.annotate(type_name)` error closure when `cli.annotate_fields` (giving the container/
    // primitive reads a `failed in <T>` location exactly as field-level errors already get). When
    // annotate_fields is off no closure is emitted and the content is pushed verbatim, byte-identical
    // to before. `new()` and the `TryFrom`/`From` paths NEVER go through this closure, so any error
    // they emit must keep the name-carrying form (see `build_check`'s `annotated=false` arm).
    let mut deser_body = BlocksOrLines::default();
    let from_impl = if min_max.is_some() || float_min_max.is_some() {
        let (before, after) = if var_names_str.is_empty() {
            ("".to_owned(), "")
        } else {
            (format!("let {var_names_str} = "), ";")
        };
        gen_scope
            .generate_deserialize(
                types,
                field_type.into(),
                DeserializeBeforeAfter::new(&before, after, false),
                DeserializeConfig::new("inner"),
                cli,
            )
            .add_to(&mut deser_body);

        // Materialize the range check per-consumer via the shared bounds-check owner, so the wrapper
        // spells its condition exactly as the member/deserialize sites do. Both copies keep the
        // ORIGINAL failure payload (min/max unchanged) — only the condition unifies. The deserialize()
        // copy is locationless (`.into()`) when it lands inside the annotate closure, while the
        // `new()` copy always carries the name (`DeserializeError::new`) since no closure ever wraps
        // it, so `annotated` maps directly onto `location` (annotated → None, else Some(type_name)).
        let render_check = |annotated: bool| -> String {
            let location = if annotated {
                None
            } else {
                Some(type_name.as_ref())
            };
            if let Some(window) = &float_min_max {
                // NaN-safe float window: accept-form negation, value compared as f64 so the authored
                // decimal literal is exact. Reports the ORIGINAL window with its per-side exclusivity.
                let cast_f64 = matches!(
                    &field_type.conceptual_type,
                    ConceptualRustType::Primitive(Primitive::F32)
                );
                bounds_check_if_block_float(window, cast_f64, "inner", true, location)
            } else {
                let (min, max) = min_max.unwrap();
                let against = if field_type
                    .encodings
                    .contains(&CBOREncodingOperation::CBORBytes)
                {
                    "inner.len()"
                } else {
                    match &field_type.conceptual_type {
                        ConceptualRustType::Primitive(p) => match p {
                            Primitive::Bytes | Primitive::Str => "inner.len()",
                            Primitive::Bool
                            | Primitive::F32
                            | Primitive::F64
                            | Primitive::U8
                            | Primitive::U16
                            | Primitive::U32
                            | Primitive::U64
                            | Primitive::I8
                            | Primitive::I16
                            | Primitive::I32
                            | Primitive::I64
                            | Primitive::N64 => "inner",
                        },
                        _ => unimplemented!(),
                    }
                };
                bounds_check_if_block(
                    &(min, max),
                    against,
                    true,
                    bounds_check_expr_non_negative(field_type),
                    location,
                )
            }
        };
        deser_body.line(&render_check(cli.annotate_fields));
        new_func
            .ret("Result<Self, DeserializeError>")
            .line(render_check(false));
        if let Some(enc_fields) = &enc_fields {
            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_body.push_block(deser_ctor);

            let mut ctor_block = Block::new("Ok(Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            ctor_block.after(")");
            new_func.push_block(ctor_block);
        } else {
            deser_body.line("Ok(Self(inner))");
            new_func.line("Ok(Self(inner))");
        }
        let mut try_from = codegen::Impl::new(type_name.to_string());
        try_from
            .associate_type("Error", "DeserializeError")
            .impl_trait(format!(
                "TryFrom<{}>",
                field_type.for_rust_member(types, false, cli)
            ))
            .new_fn("try_from")
            .arg("inner", field_type.for_rust_member(types, false, cli))
            .ret("Result<Self, Self::Error>")
            .line(format!(
                "{}::new({})",
                type_name,
                ToWasmBoundaryOperations::format(
                    field_type
                        .from_wasm_boundary_clone(types, "inner", false)
                        .into_iter()
                )
            ));
        try_from
    } else {
        // let field_type_tagged = if let Some(t) = tag {
        //     ConceptualRustType::Tagged(t, Box::new(field_type.clone()))
        // } else {
        //     field_type.clone()
        // };
        // gen_scope.generate_deserialize(types, &field_type_tagged, "inner", "Ok(Self(", "))", false, false, true, &mut deser_func);
        new_func.ret("Self");
        if let Some(enc_fields) = &enc_fields {
            let (before, after) = if var_names_str.is_empty() {
                ("".to_owned(), "")
            } else {
                (format!("let {var_names_str} = "), ";")
            };
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new(&before, after, false),
                    DeserializeConfig::new("inner"),
                    cli,
                )
                .add_to(&mut deser_body);

            let mut deser_ctor = Block::new("Ok(Self");
            deser_ctor.line("inner,");
            if !enc_fields.is_empty() {
                let mut encoding_ctor = Block::new(format!("encodings: Some({encoding_name}"));
                for field_enc in enc_fields {
                    encoding_ctor.line(format!("{},", field_enc.field_name));
                }
                encoding_ctor.after("),");
                deser_ctor.push_block(encoding_ctor);
            }
            deser_ctor.after(")");
            deser_body.push_block(deser_ctor);

            let mut ctor_block = Block::new("Self");
            ctor_block.line("inner,");
            if !enc_fields.is_empty() {
                ctor_block.line("encodings: None,");
            }
            new_func.push_block(ctor_block);
        } else {
            gen_scope
                .generate_deserialize(
                    types,
                    field_type.into(),
                    DeserializeBeforeAfter::new("Ok(Self(", "))", false),
                    DeserializeConfig::new("inner"),
                    cli,
                )
                .add_to(&mut deser_body);
            new_func.line("Self(inner)");
        }

        let mut from = codegen::Impl::new(type_name.to_string());
        from.impl_trait(format!(
            "From<{}>",
            field_type.for_rust_member(types, false, cli)
        ))
        .new_fn("from")
        .arg("inner", field_type.for_rust_member(types, false, cli))
        .ret("Self")
        .line(format!(
            "{}::new({})",
            type_name,
            ToWasmBoundaryOperations::format(
                field_type
                    .from_wasm_boundary_clone(types, "inner", false)
                    .into_iter()
            )
        ));
        from
    };
    // Flush the accumulated deserialize() body: wrap it in a single `.annotate(type_name)` error
    // closure when annotate_fields is on (giving container/primitive reads a `failed in <T>`
    // location; the in-body range check is already the locationless form so the closure names it
    // exactly once), else push it verbatim (byte-identical to the pre-annotation output).
    if cli.annotate_fields {
        let mut error_annotator = make_err_annotate_block(type_name.as_ref(), "", "");
        error_annotator.push_all(deser_body);
        deser_func.push_block(error_annotator);
    } else {
        deser_func.push_all(deser_body);
    }
    deser_impl.push_fn(deser_func);
    s_impl.push_fn(new_func);
    let mut from_inner_impl = codegen::Impl::new(field_type.for_rust_member(types, false, cli));
    from_inner_impl
        .impl_trait(format!("From<{type_name}>"))
        .new_fn("from")
        .arg("wrapper", type_name.to_string())
        .ret("Self")
        .line(format!("wrapper.{inner_var}"));
    gen_scope
        .rust(types, type_name)
        .push_struct(s)
        .push_impl(s_impl)
        .push_impl(from_impl)
        .push_impl(from_inner_impl);
    if !struct_config.custom_json {
        if cli.json_serde_derives {
            gen_scope
                .rust(types, type_name)
                .push_impl(serde_ser_impl)
                .push_impl(serde_deser_impl);
        }
        if cli.json_schema_export {
            gen_scope.rust(types, type_name).push_impl(json_schema_impl);
        }
    }
    gen_scope
        .rust_serialize(types, type_name)
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}

/// the derivative crate doesn't accept Eq="ignore" but omitting it
/// seems to behave correctly
/// The SINGLE demand→traits mapping (pinned semantics 6), used by every derive/ignore emission site so
/// the bare path stays byte-identical. Resolves a `DemandSet` to the comparison/hash traits it demands,
/// in the canonical emission order `Eq, PartialEq, Ord, PartialOrd, Hash`:
/// - `bare` → today's mode-dependent internal bundle (`Eq/PartialEq/Ord/PartialOrd`, plus `Hash` under
///   `--preserve-encodings`);
/// - `hash` → `Hash, Eq, PartialEq` (mode-independent);
/// - `ord` → `Ord, PartialOrd, Eq, PartialEq` (mode-independent).
///
/// `for_ignore` drops `Eq` (the `derivative` field ignore-list has no `Eq` attribute — `Eq` is a
/// fieldless marker), reproducing the old `key_derives(for_ignore=true)` set exactly.
fn key_trait_list(demand: DemandSet, for_ignore: bool, cli: &Cli) -> Vec<&'static str> {
    let mut eq = false;
    let mut ord = false;
    let mut hash = false;
    if demand.bare {
        eq = true;
        ord = true;
        if cli.preserve_encodings {
            hash = true;
        }
    }
    if demand.hash {
        hash = true;
        eq = true;
    }
    if demand.ord {
        ord = true;
        eq = true;
    }
    let mut out = Vec::new();
    if eq && !for_ignore {
        out.push("Eq");
    }
    if eq {
        out.push("PartialEq");
    }
    if ord {
        out.push("Ord");
        out.push("PartialOrd");
    }
    if hash {
        out.push("Hash");
    }
    out
}

/// The `where`-clause trait bound a key demand needs, as used by the `borrowed_key_types.rs`
/// `_assert_key_traits*` self-check carriers. Drops `PartialEq` (a supertrait of `Eq`, redundant as a
/// bound) and maps `Hash` to its full path, so the `bare` bound reproduces the historical
/// `Eq + Ord + PartialOrd + core::hash::Hash` (byte-identical) form.
fn key_bound(demand: DemandSet, cli: &Cli) -> String {
    key_trait_list(demand, false, cli)
        .iter()
        .filter(|t| **t != "PartialEq")
        .map(|t| if *t == "Hash" { "core::hash::Hash" } else { *t })
        .collect::<Vec<_>>()
        .join(" + ")
}

/// The sidecar flavor token for a demand (`bare`/`hash`/`ord`, space-joined when several bits are set).
/// This is the optional 3rd `BORROWED_KEY_TYPES` column; `parse_key_flavor` is its inverse.
fn key_flavor_token(demand: DemandSet) -> String {
    let mut parts = Vec::new();
    if demand.bare {
        parts.push("bare");
    }
    if demand.hash {
        parts.push("hash");
    }
    if demand.ord {
        parts.push("ord");
    }
    parts.join(" ")
}

/// The directly-tagged demand roots that warrant an emitted compile-time assertion: every
/// `@used_as_key` root — flavored or bare — whose type is a generated (non-extern), export-scope
/// struct in THIS crate, so it can be named `crate::generated::…` and its supply proven by the
/// compiler. Bare roots are included as a diagnosis breadcrumb: their derive demand propagates
/// transitively, so a missing-trait failure surfaces at a contained struct with nothing connecting
/// it to the tag — this file is the in-crate record of which tag caused which demand. (Internal
/// auto-detected map keys still emit nothing: their containers' own bounds enforce them in-crate.)
/// Sorted by ident (`BTreeMap` iteration) for deterministic placement.
fn assertion_roots(types: &IntermediateTypes) -> Vec<(RustIdent, DemandSet)> {
    types
        .key_demand_roots()
        .iter()
        .filter(|(ident, _)| {
            types.scope(ident).export()
                && types.rust_struct(ident).is_some_and(|rs| {
                    !matches!(
                        rs.variant(),
                        RustStructType::Extern | RustStructType::RawBytesType
                    )
                })
        })
        .map(|(ident, d)| (ident.clone(), *d))
        .collect()
}

fn add_struct_derives<T: DataType>(
    data_type: &mut T,
    key_demand: Option<DemandSet>,
    is_enum: bool,
    cstyle_baseline: bool,
    custom_json: bool,
    cli: &Cli,
) {
    data_type.derive("Clone").derive("Debug");
    if !custom_json {
        if cli.json_serde_derives {
            data_type
                .derive("serde::Deserialize")
                .derive("serde::Serialize");
        }
        if cli.json_schema_export {
            data_type.derive("schemars::JsonSchema");
        }
    }
    if let Some(mut demand) = key_demand {
        // A c-style enum's always-on baseline is `Eq/PartialEq/Ord/PartialOrd` (emitted directly when
        // it is NOT a key). When it IS a key, that baseline must be UNIONED with the tag's flavor so a
        // tagged enum never derives LESS than an untagged one (pinned semantics 5). `ord` supplies the
        // whole `Ord/PartialOrd/Eq/PartialEq` family, so forcing it reconstitutes the baseline.
        if cstyle_baseline {
            demand.ord = true;
        }
        let traits = key_trait_list(demand, false, cli);
        if cli.preserve_encodings {
            // there's no way to do non-derive() proc macros in the codegen
            // cate so we must sadly use a newline like this. codegen manages indentation
            data_type.derive(&format!(
                "derivative::Derivative)]\n#[derivative({}",
                traits
                    .iter()
                    .map(|tr| match *tr {
                        // the derivative crate doesn't support enums tagged with ord/partialord yet without this
                        "Ord" | "PartialOrd" if is_enum =>
                            format!("{tr}=\"feature_allow_slow_enum\""),
                        _ => String::from(*tr),
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ));
        } else {
            for key_derive in traits {
                data_type.derive(key_derive);
            }
        }
    }
}

fn generate_int(gen_scope: &mut GenerationScope, types: &IntermediateTypes, cli: &Cli) {
    let ident = RustIdent::new(CDDLIdent::new("int"));
    if cli.wasm {
        let mut wrapper = create_base_wasm_wrapper(gen_scope, types, &ident, true, cli);
        let mut wasm_new = codegen::Function::new("new");
        let mut new_if = Block::new("if x >= 0");
        let mut new_else = Block::new("else");
        new_if.line(format!(
            "Self({}::Int::new_uint(x as u64))",
            cli.lib_name_code()
        ));
        new_else.line(format!(
            "Self({}::Int::new_nint((x + 1).unsigned_abs()))",
            cli.lib_name_code()
        ));
        wasm_new
            .ret("Self")
            .vis("pub")
            .arg("x", "i64")
            .push_block(new_if)
            .push_block(new_else);

        let mut wasm_new_uint = codegen::Function::new("new_uint");
        wasm_new_uint
            .ret("Self")
            .vis("pub")
            .arg("value", "u64")
            .line(format!(
                "Self({}::Int::new_uint(value))",
                cli.lib_name_code()
            ));

        let mut wasm_new_nint = codegen::Function::new("new_nint");
        wasm_new_nint
            .ret("Self")
            .vis("pub")
            .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
            .arg("value", "u64")
            .line(format!("Self({}::Int::new_nint(value))", cli.lib_name_code()));

        let mut to_str = codegen::Function::new("to_str");
        to_str
            .vis("pub")
            .arg_ref_self()
            .ret("String")
            .line("self.0.to_string()");

        let mut from_str = codegen::Function::new("from_str");
        from_str
            .attr("allow(clippy::should_implement_trait)")
            .vis("pub")
            .arg("string", "&str")
            .ret("Result<Int, JsError>")
            .line("// have to redefine so it's visible in WASM")
            .line("std::str::FromStr::from_str(string).map(Self).map_err(|e| JsError::new(&format!(\"Int.from_str({}): {:?}\", string, e)))");

        wrapper
            .s_impl
            .push_fn(wasm_new)
            .push_fn(wasm_new_uint)
            .push_fn(wasm_new_nint)
            .push_fn(to_str)
            .push_fn(from_str);
        wrapper.push(gen_scope, types);

        // Rust exposes `IntError` as the `FromStr` associated error. The wasm constructor maps that
        // to `JsError`, so keep source-level parity without claiming wasm exports a data-bearing enum.
        gen_scope
            .wasm(types, &ident)
            .push_type_alias(TypeAlias::new("IntError", "JsError").vis("pub").clone());
    }

    let mut native_struct = codegen::Enum::new("Int");
    native_struct.vis("pub");
    let mut uint = codegen::Variant::new("Uint");
    let mut nint = codegen::Variant::new("Nint");
    if cli.preserve_encodings {
        uint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
        nint.named("value", "u64").named(
            format!(
                "{}encoding",
                encoding_var_macros(types.key_demand(&ident), true, cli)
            ),
            "Option<cbor_event::Sz>",
        );
    } else {
        uint.tuple("u64");
        nint.tuple("u64");
    }
    native_struct.push_variant(uint);
    native_struct.push_variant(nint);
    add_struct_derives(
        &mut native_struct,
        types.key_demand(&ident),
        /* is_enum */ true,
        /* cstyle_baseline */ false,
        /* custom_json */ true,
        cli,
    );

    // JSON: Int's serde/schemars impls are written here by hand (the `custom_json` arg above) rather
    // than derived — the derived enum form would leak the CBOR encoding quirk (`{"Nint":4}` actually
    // meaning -5). Serialize as the signed decimal string (via Display/FromStr): it matches Int's own
    // to_str/from_str API and safely covers the full [-2^64, 2^64) range a JSON number can't hold.
    if cli.json_serde_derives {
        let mut serde_ser_impl = codegen::Impl::new("Int");
        let mut serde_ser_fn = codegen::Function::new("serialize");
        serde_ser_fn
            .generic("S")
            .bound("S", "serde::Serializer")
            .arg_ref_self()
            .arg("serializer", "S")
            .ret("Result<S::Ok, S::Error>")
            .line("serializer.serialize_str(&self.to_string())");
        serde_ser_impl
            .impl_trait("serde::Serialize")
            .push_fn(serde_ser_fn);
        gen_scope.rust_lib().push_impl(serde_ser_impl);

        let mut serde_deser_impl = codegen::Impl::new("Int");
        let mut serde_deser_fn = codegen::Function::new("deserialize");
        serde_deser_fn
            .generic("D")
            .bound("D", "serde::de::Deserializer<'de>")
            .arg("deserializer", "D")
            .ret("Result<Self, D::Error>")
            .line("let s = <String as serde::de::Deserialize>::deserialize(deserializer)?;")
            .line("std::str::FromStr::from_str(&s).map_err(|_e| serde::de::Error::invalid_value(serde::de::Unexpected::Str(&s), &\"invalid Int\"))");
        serde_deser_impl
            .impl_trait("serde::de::Deserialize<'de>")
            .generic("'de")
            .push_fn(serde_deser_fn);
        gen_scope.rust_lib().push_impl(serde_deser_impl);
    }
    if cli.json_schema_export {
        let mut json_schema_impl = codegen::Impl::new("Int");
        let mut schema_name_fn = codegen::Function::new("schema_name");
        schema_name_fn
            .ret("::std::borrow::Cow<'static, str>")
            .line("::std::borrow::Cow::Borrowed(\"Int\")");
        let mut json_schema_fn = codegen::Function::new("json_schema");
        json_schema_fn
            .arg("generator", "&mut schemars::SchemaGenerator")
            .ret("schemars::Schema")
            .line("String::json_schema(generator)");
        let mut inline_schema_fn = codegen::Function::new("inline_schema");
        inline_schema_fn.ret("bool").line("String::inline_schema()");
        json_schema_impl
            .impl_trait("schemars::JsonSchema")
            .push_fn(schema_name_fn)
            .push_fn(json_schema_fn)
            .push_fn(inline_schema_fn);
        gen_scope.rust_lib().push_impl(json_schema_impl);
    }

    // impl Int
    let mut native_impl = codegen::Impl::new("Int");
    let mut new_uint = codegen::Function::new("new_uint");
    new_uint.vis("pub").arg("value", "u64").ret("Self");
    if cli.preserve_encodings {
        let mut new_uint_ctor = Block::new("Self::Uint");
        new_uint_ctor.line("value,").line("encoding: None,");
        new_uint.push_block(new_uint_ctor);
    } else {
        new_uint.line("Self::Uint(value)");
    }
    native_impl.push_fn(new_uint);

    let mut new_nint = codegen::Function::new("new_nint");
    new_nint
        .vis("pub")
        .doc("* `value` - Value as encoded in CBOR - note: a negative `x` here would be `|x + 1|` due to CBOR's `nint` encoding e.g. to represent -5, pass in 4.")
        .arg("value", "u64")
        .ret("Self");
    if cli.preserve_encodings {
        let mut new_nint_ctor = Block::new("Self::Nint");
        new_nint_ctor.line("value,").line("encoding: None,");
        new_nint.push_block(new_nint_ctor);
    } else {
        new_nint.line("Self::Nint(value)");
    }
    native_impl.push_fn(new_nint);

    // serialization
    let mut ser_impl = make_serialization_impl("Int", cli);
    let mut ser_func = make_serialization_function("serialize", &gen_scope.serialize_generic, cli);
    let mut ser_block = Block::new("match self");
    if cli.preserve_encodings {
        ser_block
            .line(format!("Self::Uint{{ value, encoding }} => serializer.write_unsigned_integer_sz(*value, fit_sz(*value, *encoding{})),", canonical_param(cli)))
            .line(format!("Self::Nint{{ value, encoding }} => serializer.write_negative_integer_sz(-((*value as i128) + 1), fit_sz(*value, *encoding{})),", canonical_param(cli)));
    } else {
        ser_block
            .line("Self::Uint(x) => serializer.write_unsigned_integer(*x),")
            .line("Self::Nint(x) => serializer.write_negative_integer_sz(-((*x as i128) + 1), cbor_event::Sz::canonical(*x)),");
    }
    ser_func.push_block(ser_block);
    ser_impl.push_fn(ser_func);

    // deserialization
    let mut deser_impl = codegen::Impl::new("Int");
    deser_impl.impl_trait("Deserialize");
    let mut deser_func =
        make_deserialization_function("deserialize", &gen_scope.deserialize_generic, cli);
    let mut annotate = make_err_annotate_block("Int", "", "");
    let mut deser_match = Block::new("match raw.cbor_type()?");
    if cli.preserve_encodings {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => raw.unsigned_integer_sz().map(|(x, enc)| Self::Uint{ value: x, encoding: Some(enc) }).map_err(std::convert::Into::into),")
            .line("cbor_event::Type::NegativeInteger => raw.negative_integer_sz().map(|(x, enc)| Self::Nint{ value: (-1 - x) as u64, encoding: Some(enc) }).map_err(std::convert::Into::into),");
    } else {
        deser_match
            .line("cbor_event::Type::UnsignedInteger => Ok(Self::Uint(raw.unsigned_integer()?)),")
            .line("cbor_event::Type::NegativeInteger => Ok(Self::Nint((-1 - raw.negative_integer_sz().map(|(x, _enc)| x)?) as u64)),");
    }
    deser_match.line("_ => Err(DeserializeFailure::NoVariantMatched.into()),");
    annotate.push_block(deser_match);
    deser_func.push_block(annotate);
    deser_impl.push_fn(deser_func);

    // traits
    let mut int_err = codegen::Enum::new("IntError");
    int_err.vis("pub").derive("Clone").derive("Debug");
    int_err
        .new_variant("Bounds")
        .tuple("std::num::TryFromIntError");
    int_err
        .new_variant("Parsing")
        .tuple("std::num::ParseIntError");

    let mut display = codegen::Impl::new("Int");
    let mut display_match = Block::new("match self");
    // Nint: RFC 8949 §3.1 says a major-type-1 value is `-1 - argument`, range -2^64..=-1. The
    // argument is held as u64, so the most-negative Int (argument u64::MAX -> -2^64) overflows
    // i64/u64; compute the signed value in i128 (same idiom as the serialize path below). Both
    // branches below do this; they differ only in field syntax (named under preserve, else tuple).
    if cli.preserve_encodings {
        display_match
            .line("Self::Uint{ value, .. } => write!(f, \"{}\", value),")
            .line("Self::Nint{ value, .. } => write!(f, \"{}\", -((*value as i128) + 1)),");
    } else {
        display_match
            .line("Self::Uint(x) => write!(f, \"{}\", x),")
            .line("Self::Nint(x) => write!(f, \"{}\", -((*x as i128) + 1)),");
    }
    display
        .impl_trait("std::fmt::Display")
        .new_fn("fmt")
        .arg_ref_self()
        .arg("f", "&mut std::fmt::Formatter<'_>")
        .ret("std::fmt::Result")
        .push_block(display_match);

    let mut from_str = codegen::Impl::new("Int");
    from_str
        .impl_trait("std::str::FromStr")
        .associate_type("Err", "IntError")
        .new_fn("from_str")
        .arg("s", "&str")
        .ret("Result<Self, Self::Err>")
        .line("let x = i128::from_str(s).map_err(IntError::Parsing)?;")
        .line("Self::try_from(x).map_err(IntError::Bounds)");

    let mut try_from_i128 = codegen::Impl::new("Int");
    let mut try_from_if = Block::new("if x >= 0");
    let mut try_from_else = Block::new("else");
    if cli.preserve_encodings {
        try_from_if.line("u64::try_from(x).map(|x| Self::Uint{ value: x, encoding: None })");
        try_from_else.line(
            "u64::try_from((x + 1).unsigned_abs()).map(|x| Self::Nint{ value: x, encoding: None })",
        );
    } else {
        try_from_if.line("u64::try_from(x).map(Self::Uint)");
        try_from_else.line("u64::try_from((x + 1).unsigned_abs()).map(Self::Nint)");
    }
    try_from_i128
        .impl_trait("TryFrom<i128>")
        .associate_type("Error", "std::num::TryFromIntError")
        .new_fn("try_from")
        .arg("x", "i128")
        .ret("Result<Self, Self::Error>")
        .push_block(try_from_if)
        .push_block(try_from_else);

    gen_scope
        .rust_lib()
        .push_enum(native_struct)
        .push_enum(int_err)
        .push_impl(native_impl)
        .push_impl(display)
        .push_impl(from_str)
        .push_impl(try_from_i128);
    gen_scope
        .rust_serialize_lib()
        .push_impl(ser_impl)
        .push_impl(deser_impl);
}
