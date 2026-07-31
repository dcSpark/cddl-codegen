use cbor_event::{Special, Type as CBORType};
use cddl::ast::parent::ParentVisitor;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::comment_ast::{DemandSet, RuleMetadata};
use crate::parsing::EXTERN_MARKER;
use crate::utils::{
    cddl_prelude, convert_to_camel_case, convert_to_snake_case, is_identifier_reserved,
    is_identifier_user_defined,
};

use std::sync::LazyLock;
pub static ROOT_SCOPE: LazyLock<ModuleScope> = LazyLock::new(|| vec![String::from("lib")].into());

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ModuleScope {
    export: bool,
    scope: Vec<String>,
}

impl ModuleScope {
    pub fn new(scope: Vec<String>) -> Self {
        Self::from(scope)
    }

    /// Make a new ModuleScope using only the first [depth] components
    pub fn parents(&self, depth: usize) -> Self {
        Self {
            export: self.export,
            scope: self.scope.as_slice()[0..depth].to_vec(),
        }
    }

    pub fn export(&self) -> bool {
        self.export
    }

    pub fn components(&self) -> &Vec<String> {
        &self.scope
    }
}

impl From<Vec<String>> for ModuleScope {
    fn from(mut scope: Vec<String>) -> Self {
        let export = match scope.first() {
            Some(first_scope) => first_scope != crate::parsing::EXTERN_DEPS_DIR,
            None => true,
        };
        let scope = if export { scope } else { scope.split_off(1) };
        Self { export, scope }
    }
}

impl std::fmt::Display for ModuleScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.scope.join("::"))
    }
}

#[derive(Debug)]
pub struct AliasInfo {
    pub base_type: RustType,
    pub gen_rust_alias: bool,
    pub gen_wasm_alias: bool,
    pub rule_metadata: Option<RuleMetadata>,
    /// The named ident this alias was resolved from, when a plain-typename rule (`ptm = mp`) had its
    /// `Alias(mp, …)` wrapper stripped to inline the type for serialization. The rust `base_type` is
    /// the correct transparent representation, but the WASM alias must instead point at the target's
    /// wrapper struct when it has one (`has_wasm_wrapper`) — a named collection is a transparent
    /// `pub type` in rust but a `#[wasm_bindgen]` wrapper in wasm, so `for_wasm_member` on the stripped
    /// (bare `Map`/`Vec`) `base_type` would mint the inline-only `MapU64To…`/`…List` name that only
    /// exists for anonymous members. `None` = emit `for_wasm_member(base_type)` as before.
    pub wasm_alias_target: Option<RustIdent>,
    /// `true` only for a generator-SYNTHESIZED collection wrapper's rust alias — currently the
    /// keys-list array a table rule mints (`create_and_register_array_type`). Distinguishes it from
    /// an authored `foo_list = [* foo]` / `tbl = { * a => b }`, which reach the same `new_manual`
    /// Array/Table registration arms and therefore CANNOT be told apart by `rule_metadata` (both are
    /// `None`). Gates `--no-synthesized-rust-collection-aliases`: rule-declared names always survive.
    pub synthesized_collection: bool,
}

impl AliasInfo {
    pub fn new_manual(base_type: RustType, gen_rust_alias: bool, gen_wasm_alias: bool) -> Self {
        Self {
            base_type,
            gen_rust_alias,
            gen_wasm_alias,
            rule_metadata: None,
            wasm_alias_target: None,
            synthesized_collection: false,
        }
    }

    pub fn new_from_metadata(base_type: RustType, rule_metadata: RuleMetadata) -> Self {
        let gen_rust_alias = !rule_metadata.no_alias;
        let gen_wasm_alias = !rule_metadata.no_alias;
        Self {
            base_type,
            gen_rust_alias,
            gen_wasm_alias,
            rule_metadata: Some(rule_metadata),
            wasm_alias_target: None,
            synthesized_collection: false,
        }
    }

    pub fn with_wasm_alias_target(mut self, target: Option<RustIdent>) -> Self {
        self.wasm_alias_target = target;
        self
    }

    /// The named wrapper the WASM `pub type` alias line points at, when it points at one — the
    /// single owner of that decision, consulted by BOTH the wasm alias emitter and
    /// `scope_references`' type-alias walk so the emitted target and its import cannot drift.
    /// `None` = the alias line renders `for_wasm_member(base_type)` instead (a transparent/
    /// structural spelling whose imports follow from walking `base_type`). The filter: a stripped
    /// plain-typename target only substitutes when it has a wasm wrapper class AND the base is not
    /// directly exposable — an exposable named array's wrapper is bypassed at the boundary
    /// (`Vec<T>`), so aliasing to the wrapper would desync (E0308).
    pub fn resolved_wasm_alias_target(&self, types: &IntermediateTypes) -> Option<&RustIdent> {
        self.wasm_alias_target.as_ref().filter(|target| {
            types.has_wasm_wrapper(target) && !self.base_type.directly_wasm_exposable(types)
        })
    }
}

#[derive(Debug, Clone)]
pub struct PlainGroupInfo<'a> {
    group: Option<cddl::ast::Group<'a>>,
    rule_metadata: RuleMetadata,
}

impl<'a> PlainGroupInfo<'a> {
    pub fn new(group: Option<cddl::ast::Group<'a>>, rule_metadata: RuleMetadata) -> Self {
        Self {
            group,
            rule_metadata,
        }
    }
}

#[derive(Debug)]
pub struct IntermediateTypes<'a> {
    // Storing the cddl::Group is the easiest way to go here even after the parse/codegen split.
    // This is since in order to generate plain groups we must have a representation, which isn't
    // known at group definition. It is later fixed when the plain group is referenced somewhere
    // and we can't parse the group without knowing the representation so instead this parsing is
    // delayed until the point where it is referenced via self.set_rep_if_plain_group(rep)
    // Some(group) = directly defined in .cddl (must call set_plain_group_representatio() later)
    // None = indirectly generated due to a group choice (no reason to call set_rep_if_plain_group() later but it won't crash)
    plain_groups: BTreeMap<RustIdent, PlainGroupInfo<'a>>,
    type_aliases: BTreeMap<AliasIdent, AliasInfo>,
    rust_structs: BTreeMap<RustIdent, RustStruct>,
    prelude_to_emit: BTreeSet<String>,
    generic_defs: BTreeMap<RustIdent, GenericDef>,
    generic_instances: BTreeMap<RustIdent, GenericInstance>,
    // Idents of SYNTHESIZED anonymous generic instances (`[a: set<key_hash>]` → `SetKeyHash`) that
    // resolve to a TRANSPARENT COLLECTION. `finalize` populates this (wasm mode only). Such an
    // instance must NOT mint a rule-named `#[wasm_bindgen]` collection class: its wasm wrapper lowers
    // to the STRUCTURAL name (`KeyHashList`, minted by the loose/NonEmpty wrapper machinery), and a
    // wasm `pub type SetKeyHash = KeyHashList;` passthrough alias (its `gen_wasm_alias` is flipped on)
    // points the field's reference at it — exactly the inline `[* key_hash]` shape. The rust side is
    // untouched (the transparent `pub type SetKeyHash = Vec<KeyHash>` alias stays). This is what makes
    // an anonymous collapsed-set instance and its inline equivalent ONE wasm concept, so a
    // `--wrapper-requests` consumer's request for the structural shape resolves via own-spec (the
    // synthesized name never reaches `own_wrapper_shapes`). Determinism: `BTreeSet`.
    anonymous_collection_instances: BTreeSet<RustIdent>,
    // Every base ident of a GENERIC extern rule (`foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`), recorded at
    // parse time from `generic_params.is_some()`. A generic extern is registered as a plain `Extern`
    // rust struct that drops its generic params on the floor, so the ONLY record of its
    // generic-ness is here. Unlike `generic_instance_bases()` (which derives bases FROM instances and
    // is therefore blind to a never-instantiated base), this sees every generic extern base whether
    // or not any `foo<uint>` instance exists — the two agree on any base that has at least one
    // instance, and this is a superset. Consumers that must reject the bare base as "names no
    // concrete type" (the json-gen schema-row emitter, the extern-interface self-check's
    // `ExternCheckKind::None`) key off THIS. Determinism: `BTreeSet`.
    generic_extern_bases: BTreeSet<RustIdent>,
    news_can_fail: BTreeSet<RustIdent>,
    // Every ident finalize resolves as used-as-key, mapped to the UNION of comparison/hash trait
    // demand on it (`@used_as_key` flavors + auto-detected internal map-key bundle). Presence in the
    // map == used-as-key; `DemandSet` records WHICH derive family. Propagated as demand SETS (not one
    // bit) through the transitive `visit_types` walk in `finalize`. Determinism: `BTreeMap`.
    key_demand: BTreeMap<RustIdent, DemandSet>,
    // The subset of `key_demand` that was DIRECTLY tagged (via `@used_as_key`/`--key-requests`), before
    // finalize's transitive expansion — the "demand roots". Only these get an emitted compile-time
    // demand assertion (auto-detected internal keys are enforced by the generated containers' own
    // bounds). Recorded at `mark_key_demand` time so the roots survive the finalize union.
    key_demand_roots: BTreeMap<RustIdent, DemandSet>,
    // Idents explicitly tagged `@used_as_elem`: the generator mints the loose-list wasm wrapper
    // (`FooList = [* foo]` equivalent) for each, exactly as an inline `[* foo]` usage would. Unlike
    // `used_as_key`, there is NO transitive expansion — the tag names the element directly, and the
    // wrapper's identity is fully determined by that one element type.
    used_as_elem: BTreeSet<RustIdent>,
    // Structural `<Elem>List` idents whose keys-list mint OVERWROTE an incompatible authored rule of
    // the same ident, mapped to the element the list wraps. `create_and_register_array_type`'s
    // `register_rust_struct` is last-wins, so once it has run the authored rule is GONE from
    // `rust_structs` and no later scan can tell an overwritten record from a rule that was never
    // there — the swallow is only observable at the instant it happens. Recorded here (wasm only,
    // the sole condition under which the mint runs at all) so
    // `non_empty_wrapper_name_collisions`' direct-claim leg can reject it in the family's voice.
    // A COMPATIBLE authored rule (`foo_list = [* foo]`, which IS the builder) is deliberate aliasing
    // and is not recorded: its re-mint is byte-identical. Determinism: `BTreeMap`.
    swallowed_structural_list_claims: BTreeMap<String, RustType>,
    // Idents of extern / raw-bytes rules tagged `@copy`: the externally-defined rust type derives
    // `Copy`, so `ConceptualRustType::is_copy` treats a `Rust(ident)` reference to one as Copy and
    // the generator drops the defensive boundary `.clone()`. The declaring crate emits a compile-time
    // `Copy` assertion for each (see `export.rs`), and the tag rides the extern-interface seam like
    // `@raw_bytes_flavor` so `--extern-import` consumers inherit it. See `RuleMetadata::copy`.
    copy_externs: BTreeSet<RustIdent>,
    // `@extern_companions` declarations, keyed by the LOCAL extern rule that carries one: the sibling
    // wasm crate path plus the exact structural companion class names that already exist there. The
    // wasm wrapper-deferral decision (`try_defer_wrapper`) consults this to REFERENCE those classes
    // instead of minting duplicate `#[wasm_bindgen]` ones. Deliberately NOT part of
    // `RustStructConfig` for the same reason as `no_json_schema_export`: `RustStruct::new_extern`
    // builds with `RustStructConfig::default()` and drops rule metadata, and an extern rule is this
    // directive's ONLY customer. Deliberately does NOT ride the extern-interface seam (unlike
    // `@copy`): the declaration is about where THIS crate's wasm face borrows from, which a consumer
    // of this crate answers for itself. Determinism: `BTreeMap`.
    extern_companions: BTreeMap<RustIdent, crate::comment_ast::ExternCompanions>,
    // Idents of rules tagged `@no_json_schema_export`: the json-gen crate emits no
    // schema-registration row for them (see the row loop in `generation/mod.rs`). Carried as a
    // per-ident marker set rather than a `RustStructConfig` field because `RustStruct::new_extern` /
    // `new_raw_bytes` build with `RustStructConfig::default()` — they drop rule metadata entirely, so
    // a config field would be silently dead on extern rules, which are the directive's primary
    // customer (an own-spec extern whose hand-written rust type has no `schemars::JsonSchema` impl).
    // The marker set covers extern and ordinary rules through ONE mechanism. Deliberately does NOT
    // ride the extern-interface seam (unlike `@copy`): a dep-owned type's row is already skipped by
    // the non-export-scope rule, so a consumer has nothing to inherit. Determinism: `BTreeSet`.
    // See `RuleMetadata::no_json_schema_export`.
    no_json_schema_export: BTreeSet<RustIdent>,
    // Base generic extern idents tagged `@raw_bytes_flavor`: an instance of one whose argument
    // resolves to a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` aliases the `<Base>RawBytes` wrapper flavor
    // instead of the plain `<Base>`. Opt-in only — see `RuleMetadata::raw_bytes_flavor`.
    raw_bytes_flavor: BTreeSet<RustIdent>,
    // Subset of `raw_bytes_flavor` for which an actual flavored instance was emitted during
    // `finalize` (a raw-bytes argument was supplied at least once). The extern re-export glue emits
    // `pub use crate::<Base>RawBytes;` only for these, so a tag with no raw-bytes instance never
    // forces the user to define an unused flavor type.
    raw_bytes_flavor_emitted: BTreeSet<RustIdent>,
    // `@rust_name` pins: a derived `RustIdent` (of a rule in a non-exported extern-deps scope) ->
    // the FINAL Rust type name the dependency's own codegen version spelled into its artifact. Every
    // INTERNAL spelling keeps the consumer-derived ident (so the ~66 `RustIdent::new` sites and all
    // references stay untouched); translation to the pin happens only at the crate boundary — the
    // `use <dep>::<Pinned> as <Derived>;` import alias (`add_imports_from_scope_refs`) and the
    // wasm→rust full-path sites (`rust_crate_struct_from_wasm`). Pin-less rules keep today's
    // derivation (hand-stub compatibility). Populated at parse time from extern-scope `RuleMetadata`
    // (`parsing::handle_rust_name_pin`). Determinism: `BTreeMap`.
    rust_name_pins: BTreeMap<RustIdent, String>,
    // which scope an ident is declared in
    scopes: BTreeMap<RustIdent, ModuleScope>,
    // The ORIGINAL CDDL source name for each top-level rule's `RustIdent`. `RustIdent::new`
    // camel-cases (and thus destroys the `-`/`_` distinction that CDDL treats as significant), so the
    // ident alone can't be reversed back to the spec rule. Recorded verbatim at rule-registration
    // time (`api::with_types`, alongside `mark_scope`) so the conformance oracle can root its
    // validator at the PROVABLE source rule rather than a lossy snake↔camel guess.
    rule_source_names: BTreeMap<RustIdent, String>,
    // Idents claimed by a multi-arm group-choice arm whose record SURVIVES parsing (the
    // non-embeddable arms — an embeddable arm's record is pulled straight back out by
    // `remove_rust_struct` and never occupies the name). Value = the source name of the rule that
    // owns the arm, so a later claimant's rejection can name both sides. Read together with
    // `is_toplevel_rule` this is the full set of claimants on a Rust struct ident, which is what
    // makes the arm-ident collision check in `parse_group_choice` order-INDEPENDENT: rule idents are
    // all scope-marked up front (`api::with_types`, before the parse loop), and two arms claiming one
    // name reject symmetrically whichever is parsed first.
    group_choice_arm_claims: BTreeMap<RustIdent, String>,
    // Deferred rejections: constructs the parse walk (which returns `()` and so can't surface an
    // `Err`) recognizes as unsupported-by-design but must reject GRACEFULLY rather than `panic!`.
    // Each entry is a human-actionable message; `finalize` drains them into a single `Err` before
    // any resolution runs, so no later code operates on the incomplete IR left behind by a skipped
    // field. A `Vec` keeps insertion order deterministic (rule order is already deterministic).
    rejections: Vec<String>,
    // for scope() to work we keep this here.
    // Returning a reference to the const ROOT_SCOPE complains of returning a temporary
    root_scope: ModuleScope,
}

impl Default for IntermediateTypes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> IntermediateTypes<'a> {
    pub fn new() -> Self {
        let mut rust_structs = BTreeMap::new();
        rust_structs.insert(
            RustIdent::new(CDDLIdent::new("int")),
            RustStruct::new_extern(RustIdent::new(CDDLIdent::new("int"))),
        );
        Self {
            plain_groups: BTreeMap::new(),
            type_aliases: Self::aliases(),
            rust_structs,
            prelude_to_emit: BTreeSet::new(),
            generic_defs: BTreeMap::new(),
            generic_instances: BTreeMap::new(),
            anonymous_collection_instances: BTreeSet::new(),
            generic_extern_bases: BTreeSet::new(),
            news_can_fail: BTreeSet::new(),
            key_demand: BTreeMap::new(),
            key_demand_roots: BTreeMap::new(),
            used_as_elem: BTreeSet::new(),
            swallowed_structural_list_claims: BTreeMap::new(),
            copy_externs: BTreeSet::new(),
            extern_companions: BTreeMap::new(),
            no_json_schema_export: BTreeSet::new(),
            raw_bytes_flavor: BTreeSet::new(),
            raw_bytes_flavor_emitted: BTreeSet::new(),
            rust_name_pins: BTreeMap::new(),
            scopes: BTreeMap::new(),
            rule_source_names: BTreeMap::new(),
            group_choice_arm_claims: BTreeMap::new(),
            rejections: Vec::new(),
            root_scope: ROOT_SCOPE.clone(),
        }
    }

    /// Record a construct the parse walk rejects by design (it can't return an `Err` itself).
    /// `finalize` turns any accumulated rejections into a single graceful `Err`.
    pub fn record_rejection(&mut self, msg: String) {
        self.rejections.push(msg);
    }

    /// Whether any parse-walk rejection has been recorded. Lets the reserved-name pre-scan in
    /// `api::with_types` abort BEFORE IR construction, since `RustIdent::new`'s reserved-ident
    /// `assert!`s (no `IntermediateTypes` handle, so they can't reject gracefully) would otherwise
    /// panic on the very name we just recorded before `finalize` ever runs.
    pub fn has_rejections(&self) -> bool {
        !self.rejections.is_empty()
    }

    /// Drain the accumulated rejections into a single graceful `Err`. Reused by `finalize` and by
    /// the early reserved-name abort so both surface the identical shape.
    pub fn rejections_error(&self) -> Box<dyn std::error::Error> {
        self.rejections.join("\n").into()
    }

    pub fn type_aliases(&self) -> &BTreeMap<AliasIdent, AliasInfo> {
        &self.type_aliases
    }

    /// Whether ANY generated type uses the `[+ T]` NonEmptyVec shape, so `export`/import wiring can
    /// pull in the `non_empty` runtime module + `NonEmptyVec` import only for crates that need it
    /// (keeping every non-`+` crate's output byte-identical).
    ///
    /// Detection folds `contains_non_empty_array` over `visit_all_rust_types`, which reaches EVERY
    /// `RustType` position in the IR — type-alias base types, record fields, table domain AND range,
    /// wrapper inners, named-array element types, and enum variants (incl. inlined records) —
    /// recursing into Array/Optional/Map inners at each node. This is a strict superset of a
    /// per-variant hand-walk: it can't miss a nested occurrence such as an inline `[+ x]` buried in a
    /// named array's element type (`[* [+ uint]]`) or in a table's domain.
    ///
    /// The named-rule bounds special case is kept deliberately. A named `[+ …]` rule registers as a
    /// `RustStructType::Array` whose `>= 1` lower bound lives on the STRUCT's `bounds`, not on the
    /// `element_type` `RustType` the visitor walks — so the visitor alone would not see it. Every
    /// such rule ALSO registers a transparent alias (`pub type Foo = NonEmptyVec<…>`), which the
    /// visitor's alias-base walk does cover today, so this check is redundant in every shape observed;
    /// but that redundancy is unproven across all IR shapes, and dropping a cheap belt-and-suspenders
    /// guard on an unverified premise is how a latent regression ships — so it stays.
    /// Whether ANY generated type uses CDDL `any` (the `AnyCbor` runtime type), so `export`/import
    /// wiring pulls in the `any_cbor` runtime module + `AnyCbor` import only for crates that need it
    /// (keeping every non-`any` crate's output byte-identical — the usage-gating invariant). Folds
    /// `contains_any_cbor` over `visit_all_rust_types`, the same superset walk `uses_non_empty_map`
    /// uses (reaches type-alias base types, record fields, table domain AND range, wrapper inners,
    /// array elements, tagged inners, and enum variants).
    pub fn uses_any_cbor(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_any_cbor());
        found
    }

    pub fn uses_non_empty_vec(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_non_empty_array());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Array { bounds, .. } if *bounds == Some((Some(1), None))
                )
            })
    }

    /// Whether ANY generated type uses the `{+ k => v}` NonEmptyMap shape, so `export`/import wiring
    /// can pull in the `non_empty_map` runtime module + `NonEmptyMap` import only for crates that need
    /// it (keeping every non-`+`-table crate's output byte-identical).
    ///
    /// Detection folds `contains_non_empty_map` over `visit_all_rust_types`, which reaches EVERY
    /// `RustType` position in the IR — type-alias base types, record fields, table domain AND range,
    /// wrapper inners, named-array element types, and enum variants (incl. inlined records) —
    /// recursing into Array/Optional/Map inners at each node. This is a strict superset of a
    /// per-variant hand-walk: it can't miss a nested occurrence such as an inline `{+ k => v}` buried
    /// in a table's DOMAIN (`{ * {+ uint => uint} => text }`) or in a named array's element type.
    ///
    /// The named-rule bounds special case is kept deliberately, for the same reason as
    /// `uses_non_empty_vec`: a named `{+ …}` table rule registers as a `RustStructType::Table` whose
    /// `>= 1` lower bound lives on the STRUCT's `bounds`, not on the `domain`/`range` `RustType`s the
    /// visitor walks. The transparent alias every such rule also registers covers it today, but that
    /// redundancy is unproven across all IR shapes, so the cheap guard stays.
    pub fn uses_non_empty_map(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_non_empty_map());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Table { bounds, .. } if *bounds == Some((Some(1), None))
                )
            })
    }

    /// Whether ANY generated type uses the `@duplicates reject` `OrderedSet`/`NonEmptyOrderedSet`
    /// shape, so `export`/import wiring pulls in the `ordered_set` runtime module + imports only for
    /// crates that need it (keeping every non-reject crate's output byte-identical). Detection folds
    /// `contains_ordered_set` over `visit_all_rust_types` (the same superset walk as
    /// `uses_non_empty_vec`), plus the deliberate belt-and-suspenders on the struct config: a
    /// reject-mode `Array` rule's policy lives on the STRUCT config (and its registered alias), so the
    /// alias-base walk covers it today, but the cheap guard stays for the same unproven-across-shapes
    /// reason as the non-empty twins.
    pub fn uses_ordered_set(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_ordered_set());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Array { .. })
                    && rs.config().duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject)
            })
    }

    /// Whether ANY generated type uses the `@duplicates preserve` `PairMap`/`NonEmptyPairMap` shape,
    /// so `export`/import wiring pulls in the `pair_map` runtime module + imports only for crates that
    /// need it. The pair-map analog of `uses_ordered_set`: folds `contains_pair_map` over
    /// `visit_all_rust_types` plus the belt-and-suspenders on the struct config (a preserve-mode
    /// `Table` rule's policy lives on the STRUCT config and its registered alias).
    pub fn uses_pair_map(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_pair_map());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Table { .. })
                    && rs.config().duplicates
                        == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
            })
            // An open struct-map rest row with `@duplicates preserve` lowers to the `PairMap` twin
            // (its `Map` type carries the policy only at emit time — `rest.domain`/`range` visited
            // above are the K/V, not the Map — so check the rest row's policy directly here).
            || self.rust_structs.values().any(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Record(record)
                        if record.rest.as_ref().is_some_and(|r| {
                            r.duplicates() == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                        })
                )
            })
    }

    /// Whether ANY generated record carries a CAPTURING open struct-map rest row (`* k => v` after
    /// fixed keys, default flavor). Gates the standalone `open_struct_rest_json` runtime module (the
    /// flatten helpers `serialize_flattened_rest` / `read_flattened_rest_pairs`) under
    /// `--json-serde-derives` — the helpers are `any`-free so they cannot live in `any_cbor.rs` (a
    /// fully-typed `* uint => text` rest row does not pull in the `AnyCbor` runtime). An `@ignore`
    /// (tolerate-and-drop) row emits no captured field, so its JSON is a closed struct's — it needs
    /// none of the flatten machinery and does not count here.
    pub fn uses_open_struct_rest(&self) -> bool {
        self.rust_structs.values().any(
            |rs| matches!(rs.variant(), RustStructType::Record(record) if record.captured_rest().is_some()),
        )
    }

    pub fn rust_structs(&self) -> &BTreeMap<RustIdent, RustStruct> {
        &self.rust_structs
    }

    /// The NAMED `{+ k => v}` table rule that owns the wasm surface for an inline `{+ k => v}` of the
    /// same domain/range, if any — the design doc's inline-dedup-to-named rule (the spec author's
    /// chosen name wins over a synthesized `NonEmptyMapKToV`). Domain/range equality is alias-resolved
    /// so spelling differences can't defeat the dedup. Deterministic (`rust_structs` is a `BTreeMap`).
    pub fn non_empty_map_named_owner(
        &self,
        key: &RustType,
        value: &RustType,
    ) -> Option<&RustIdent> {
        let key_resolved = key.clone().resolve_aliases();
        let value_resolved = value.clone().resolve_aliases();
        self.rust_structs
            .iter()
            .find_map(|(ident, rs)| match rs.variant() {
                // A SYNTHESIZED anonymous instance carries no author name worth surfacing — it lowers
                // to the structural `NonEmpty<MapKToV>` wrapper (see the anonymous-collapse
                // convergence), so it must NOT win the owner slot the way an authored `{+ …}` rule does.
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } if *bounds == Some((Some(1), None))
                    // A `@duplicates preserve` named `{+ …}` rule's wasm class wraps `NonEmptyPairMap`,
                    // but an inline `{+ …}` occurrence carries no directive (inline occurrences are
                    // directive-less), so its rust member is the loose `NonEmptyMap`. Capturing the
                    // inline surface onto the preserve rule would name it after a wrapper of the wrong
                    // core type — a loud-but-broken wasm crate (`From<NonEmptyMap>` for the preserve
                    // wrapper does not exist). The map-side of the reject-set guard in
                    // `non_empty_named_owner`: only a non-preserve named rule may own the loose inline.
                    && rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    && !self.is_anonymous_collection_instance(ident)
                    && domain.clone().resolve_aliases() == key_resolved
                    && range.clone().resolve_aliases() == value_resolved =>
                {
                    Some(ident)
                }
                _ => None,
            })
    }

    /// The NAMED `[+ elem]` rule that owns the wasm surface for an inline `[+ elem]` of the same
    /// element, if any — the design doc's inline-dedup-to-named rule: the spec author's chosen name
    /// wins over a synthesized `NonEmpty<Elem>List`. Element equality is alias-resolved so spelling
    /// differences can't defeat the dedup. Deterministic: `rust_structs` is a `BTreeMap`, so the
    /// lexicographically-first matching rule ident wins when several same-shape rules exist.
    pub fn non_empty_named_owner(&self, element: &RustType) -> Option<&RustIdent> {
        let resolved = element.clone().resolve_aliases();
        self.rust_structs
            .iter()
            .find_map(|(ident, rs)| match rs.variant() {
                // A SYNTHESIZED anonymous instance is excluded (see the map twin above): it lowers to
                // the structural `NonEmpty<Elem>List`, so a `nonempty_set<key_hash>` instance never
                // shadows the inline `[+ key_hash]`'s synthesized wrapper name with its own ident.
                RustStructType::Array {
                    element_type,
                    bounds,
                } if *bounds == Some((Some(1), None))
                    // A `@duplicates reject` named rule's wasm class wraps `NonEmptyOrderedSet`, but
                    // an inline `[+ elem]` occurrence carries no directive (inline occurrences are
                    // always preserve-policy) so its rust member is `NonEmptyVec`. Capturing the
                    // preserve inline surface onto the reject rule would name it after a wrapper of
                    // the wrong core type — a loud-but-broken wasm crate (`From<NonEmptyVec>` for the
                    // reject wrapper does not exist). Only a preserve-policy named rule may own it.
                    && rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Reject)
                    && !self.is_anonymous_collection_instance(ident)
                    && element_type.clone().resolve_aliases() == resolved =>
                {
                    Some(ident)
                }
                _ => None,
            })
    }

    /// Visit every `RustType` occurrence in the IR — record fields, table domain/range, wrapper
    /// inners, enum variants (incl. inlined records), named-array element types, and type-alias
    /// base types — recursing into Array/Optional/Map inners at the RustType level, so occurrence
    /// bounds (which live on `RustType`, not the conceptual type) stay visible to the visitor
    /// (the conceptual `visit_types` strips them at every step).
    pub fn visit_all_rust_types<F: FnMut(&RustType)>(&self, f: &mut F) {
        fn walk<F: FnMut(&RustType)>(rt: &RustType, f: &mut F) {
            f(rt);
            match &rt.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, f)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, f);
                    walk(v, f);
                }
                _ => {}
            }
        }
        for alias in self.type_aliases.values() {
            walk(&alias.base_type, f);
        }
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Record(record) => {
                    record.fields.iter().for_each(|fl| walk(&fl.rust_type, f));
                    // An open rest (map `* k => v` row or array `* t` tail) carries RustType(s)
                    // (loose CBOR) that are NOT a `RustField`, so walk them explicitly — else usage
                    // detectors (`uses_any_cbor`, the collection-twin detectors) miss an
                    // `any`/collection type that appears only in the rest inner type(s).
                    if let Some(rest) = &record.rest {
                        match &rest.kind {
                            RestKind::MapEntries { domain, range, .. } => {
                                walk(domain, f);
                                walk(range, f);
                            }
                            RestKind::ArrayTail { element } => walk(element, f),
                        }
                    }
                }
                RustStructType::Table { domain, range, .. } => {
                    walk(domain, f);
                    walk(range, f);
                }
                RustStructType::Wrapper { wrapped, .. } => walk(wrapped, f),
                RustStructType::Array { element_type, .. } => walk(element_type, f),
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    variants.iter().for_each(|v| match &v.data {
                        EnumVariantData::RustType(t) => walk(t, f),
                        EnumVariantData::Inlined(rec) => {
                            rec.fields.iter().for_each(|fl| walk(&fl.rust_type, f))
                        }
                    })
                }
                RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    }

    /// Whether `name` is claimed by a user-defined rule (a generated rust struct or a user type
    /// alias) — the namespace the synthesized wasm wrapper names must not silently shadow.
    fn wasm_ident_claimed_by_user_rule(&self, name: &str) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        self.rust_structs.contains_key(&ident)
            || self.type_aliases.contains_key(&AliasIdent::Rust(ident))
    }

    /// Whether rule `name` provides a COMPATIBLE loose list wrapper for `element_resolved` (an
    /// Array rust struct of the same element whose bounds are NOT the `[+]` shape — its wasm class
    /// is the loose `Vec` wrapper, byte-compatible with what the try_from source needs).
    fn provides_compatible_loose_list(&self, name: &str, element_resolved: &RustType) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        matches!(
            self.rust_structs.get(&ident).map(|rs| rs.variant()),
            Some(RustStructType::Array {
                element_type,
                bounds,
            }) if *bounds != Some((Some(1), None))
                && element_type.clone().resolve_aliases() == *element_resolved
        )
    }

    /// Whether rule `name` is the SELF-NAMED `[+ elem]` rule of the element whose loose list class is
    /// spelled `name` (`nev_q_list = [+ nev_q]` -> `NevQList`). Such a rule legitimately owns the
    /// ident for its RESTRICTED wrapper, and the self-named leg of
    /// `non_empty_wrapper_name_collisions` reports the resulting conflict with the `[+ …]` rule as
    /// the named claimant — so the direct-claim leg must not report the same conflict a second time
    /// in a voice that would tell the author to rename a rule the other message already names.
    fn claims_ident_as_self_named_non_empty_list(&self, name: &str) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        matches!(
            self.rust_structs.get(&ident).map(|rs| rs.variant()),
            Some(RustStructType::Array {
                element_type,
                bounds,
            }) if *bounds == Some((Some(1), None))
                && element_type.name_as_wasm_array(self) == name
        )
    }

    /// The map-side twin of `claims_ident_as_self_named_non_empty_list`: whether rule `name` is the
    /// SELF-NAMED `{+ k => v}` table whose own loose-builder name is `name`. Owned by the self-named
    /// leg of `non_empty_map_wrapper_name_collisions`, so the direct-claim leg skips it.
    fn claims_ident_as_self_named_non_empty_table(&self, name: &str) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        self.rust_structs.get(&ident).is_some_and(|rs| {
            let preserve =
                rs.config().duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
            matches!(
                rs.variant(),
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } if *bounds == Some((Some(1), None))
                    && ConceptualRustType::name_for_wasm_map(domain, range, preserve).to_string()
                        == name
            )
        })
    }

    /// Whether rule `name` provides a COMPATIBLE loose table wrapper for `(key, value)` (a plain
    /// `{* k => v}` Table rust struct of the same domain/range — its wasm class IS the loose `MapKToV`
    /// builder the `{+ …}` restricted wrapper's `try_from` source needs, so it is shared, not a
    /// collision). Non-empty tables are excluded (their class is the restricted wrapper, not the
    /// loose builder).
    fn provides_compatible_loose_table(
        &self,
        name: &str,
        key_resolved: &RustType,
        value_resolved: &RustType,
        // the flavor the caller needs: a `@duplicates preserve` builder is a `PairMap`-backed class,
        // structurally incompatible with the keyed default, so the rule must match on BOTH
        preserve: bool,
    ) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        let rule_preserve = self.rust_structs.get(&ident).is_some_and(|rs| {
            rs.config().duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
        });
        rule_preserve == preserve
            && matches!(
                self.rust_structs.get(&ident).map(|rs| rs.variant()),
                Some(RustStructType::Table {
                    domain,
                    range,
                    bounds,
                }) if *bounds != Some((Some(1), None))
                    && domain.clone().resolve_aliases() == *key_resolved
                    && range.clone().resolve_aliases() == *value_resolved
            )
    }

    /// The base idents (`generic_ident`) of every registered generic instance, e.g. `Foo` for the
    /// instance `Foo<Bar>`. For a generic EXTERN base this is the only place the bare base name lives:
    /// a generic extern (`Foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`) is registered as a plain `Extern`
    /// rust struct, but the wasm crate never names it — wasm-bindgen can't express generics, so the
    /// instance collapses to the argument's wasm wrapper via a `pub type FooBar = BarWrapper;` alias.
    /// The wasm extern re-export glue uses this to skip such bases (no wasm-crate-root definition
    /// exists to re-export), whereas the rust side keeps the base (`pub type FooBar = Foo<Bar>;`
    /// references it).
    pub fn generic_instance_bases(&self) -> BTreeSet<RustIdent> {
        self.generic_instances
            .values()
            .map(|inst| inst.generic_ident.clone())
            .collect()
    }

    /// Record a generic extern rule's base ident (`foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`), from the
    /// parse-time `generic_params.is_some()` signal. See the `generic_extern_bases` field comment.
    pub fn mark_generic_extern_base(&mut self, ident: RustIdent) {
        self.generic_extern_bases.insert(ident);
    }

    /// Every ident that is the base of a generic extern, from EITHER signal: recorded at parse time
    /// when the rule DECLARES params (`foo<T> = _CDDL_CODEGEN_EXTERN_TYPE_`), OR derived from a
    /// usage-site instance (`extern_generic = _CDDL_CODEGEN_EXTERN_TYPE_` declared plain but used as
    /// `extern_generic<external_foo>` — the `tests/core` style). Neither signal subsumes the other: a
    /// never-instantiated base shows only in the parse record, a plain-declared-but-used base only in
    /// the instances, so the union is required. Use this — not `generic_instance_bases` alone —
    /// anywhere a bare generic extern base must be skipped because it names no concrete type (the
    /// json-gen schema-row emitter, the extern-interface `ExternCheckKind::None` decision). Including
    /// the non-extern members of `generic_instance_bases` is harmless: a non-extern generic base
    /// never materializes as a `rust_structs` entry, so neither call site ever tests one.
    pub fn generic_extern_base_idents(&self) -> BTreeSet<RustIdent> {
        let mut set = self.generic_extern_bases.clone();
        set.extend(self.generic_instance_bases());
        set
    }

    /// Which named `Table` rule solely owns each structural wasm-map shape, keyed by the structural
    /// `name_for_wasm_map` string (that string IS the shape identity). A shape owned by EXACTLY ONE
    /// table rule has its wasm class plus the structural `pub type MapKToV = <Owner>;` alias minted in
    /// that owner's module (`mint_sole_owner_table` in generation/collections.rs); zero-owner (anonymous-only) and
    /// multi-owner (same-shape rule pair) shapes keep the structural fallback class at the crate root.
    /// Both the wasm emit path AND `scope_references`'s Map arm consult this single helper so import
    /// placement and emission placement CANNOT disagree. Iterates `rust_structs()` (a BTreeMap) so the
    /// result depends only on the SET of table rules, never on visit order.
    pub fn table_shape_sole_owners(&self) -> BTreeMap<String, RustIdent> {
        let mut owners: BTreeMap<String, Vec<RustIdent>> = BTreeMap::new();
        for (ident, rust_struct) in self.rust_structs() {
            // A table rule defined inside an extern-deps stub (non-exported scope) describes a
            // dep-owned type and must never be recorded as the owner of a structural map shape: the
            // sole-owner class is minted in the owner's scope, and a non-exported scope emits
            // nothing, so a consumer's OWN anonymous use of the same shape would silently lose its
            // wrapper. Only crate-owned table rules can own a shape.
            if !self.scope(ident).export() {
                continue;
            }
            // A SYNTHESIZED anonymous collection instance (a generic table instance like
            // `tbl<uint, tstr>` -> `TblU64Text`) must NOT own a structural map shape: it lowers to the
            // structural `MapKToV` wrapper through its `gen_wasm_alias` passthrough
            // (`pub type TblU64Text = MapU64ToText;`), exactly as an inline `{* k => v}` does. Recording
            // it as the sole owner would ALSO mint `mint_sole_owner_table`'s `pub struct TblU64Text` +
            // `pub type MapU64ToText = TblU64Text;`, colliding with the passthrough alias on BOTH idents
            // (the export.rs duplicate-ident backstop). This mirrors the anonymous-instance exclusion in
            // `non_empty_map_named_owner` / `non_empty_named_owner`.
            if self.is_anonymous_collection_instance(ident) {
                continue;
            }
            if let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rust_struct.variant()
            {
                // A non-empty `{+ k => v}` table does NOT own the loose structural `MapKToV` shape —
                // its JS class is the distinct restricted `NonEmptyMapKToV` (or rule-ident) wrapper.
                // Excluding it keeps anonymous plain `{* k => v}` uses of the same shape from being
                // (wrongly) folded onto the restricted class.
                if *bounds == Some((Some(1), None)) {
                    continue;
                }
                // The shape identity is the FLAVORED structural name, so a `@duplicates preserve`
                // rule owns `PairMapKToV` while a default rule of the identical key/value owns
                // `MapKToV` — two independent sole-owner entries that cannot interfere. The flavor is
                // read from this rule's own config: local information, never a crate-wide lookup.
                let structural = ConceptualRustType::name_for_wasm_map(
                    domain,
                    range,
                    rust_struct.config().duplicates
                        == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                )
                .to_string();
                owners.entry(structural).or_default().push(ident.clone());
            }
        }
        owners
            .into_iter()
            .filter_map(|(structural, mut owners)| {
                (owners.len() == 1).then(|| (structural, owners.pop().unwrap()))
            })
            .collect()
    }

    /// The wasm wrapper the code emitter (`RustType::for_wasm_member`) names for a collection
    /// occurrence `ty`, paired with the module its class is MINTED in — the import-tracker twin of
    /// the emitter's own name resolution, so a using scope imports EXACTLY the ident the emitter
    /// references, from EXACTLY the module the mint walk / `wasm()` places it. It branches
    /// identically to `for_wasm_member` (reject-set → non-empty-array → non-empty-map → loose
    /// `[* elem]` list), and resolves the home scope the SAME way emission does:
    /// - a LOOSE structural `MapKToV` with a sole named owner is minted in that owner's module
    ///   (via `table_shape_sole_owners`, shared with `mint_sole_owner_table`) — the one wrapper whose
    ///   home `types.scope` can't see, since the structural name is never a registered scope;
    /// - every other wrapper lives at `types.scope(wrapper_ident)`: the crate root for a synthesized
    ///   structural name (`NonEmpty<Elem>List` / `MapKToV` / `<Elem>OrderedSet`), or the owner's
    ///   module for a dedup-to-named (`Nums`/`Recs`/`Mp`) or rule-named wrapper.
    ///
    /// `None` for an occurrence that crosses the wasm boundary bare (an exposable `Vec<..>` array, or
    /// a non-collection type) — nothing to import. Callers must be in the wasm pass; the rust pass
    /// names no such wrappers.
    fn wasm_collection_wrapper(
        &self,
        ty: &RustType,
        sole_owners: &BTreeMap<String, RustIdent>,
    ) -> Option<(RustIdent, ModuleScope)> {
        // LOOSE structural map (not reject/non-empty): its emission scope is the sole owner's module
        // when one exists (matching `mint_sole_owner_table`), else `types.scope` (root). Resolve it
        // before the `for_wasm_member` name below because the sole-owner indirection is invisible to
        // `types.scope` — the structural `MapKToV` name is never a registered scope.
        if let ConceptualRustType::Map(k, v) = &ty.conceptual_type
            && !ty.is_non_empty_map()
        {
            // The occurrence's own carried policy selects the flavor (`MapKToV` / `PairMapKToV`) —
            // the same local signal `for_wasm_member` uses, so name resolution here and at the
            // emitter cannot disagree.
            let ident = ConceptualRustType::name_for_wasm_map(k, v, ty.is_preserve_pair_map());
            let scope = sole_owners
                .get(&ident.to_string())
                .map(|owner| self.scope(owner).clone())
                .unwrap_or_else(|| self.scope(&ident).clone());
            return Some((ident, scope));
        }
        // Every remaining wrapper name resolves the same way `for_wasm_member` names it, and its home
        // is `types.scope(ident)` (root for a synthesized name, the owner's module for a dedup/rule
        // ident — a registered rust struct).
        let name = if ty.is_reject_ordered_set() {
            ty.reject_ordered_set_wasm_wrapper_name(self)
        } else if ty.is_non_empty_array() {
            ty.non_empty_wasm_wrapper_name(self)
        } else if ty.is_non_empty_map() {
            ty.non_empty_wasm_map_wrapper_name(self)
        } else {
            match &ty.conceptual_type {
                ConceptualRustType::Array(elem) if !ty.directly_wasm_exposable(self) => {
                    elem.name_as_wasm_array(self)
                }
                // exposable `[* uint]` -> bare `Vec`, or a non-collection: no wrapper to import
                _ => return None,
            }
        };
        let ident = RustIdent::new(CDDLIdent::new(name));
        let scope = self.scope(&ident).clone();
        Some((ident, scope))
    }

    /// For each scope, which other scopes are referenced, and which structs are referenced
    ///
    /// `deferred` (wasm pass only) maps every collection-wrapper ident the consumer is NOT minting —
    /// because a mapped dependency's `--extern-wrapper-index` already owns it — to that dependency's
    /// `collections` module scope. A deferred wrapper's referencing sites import it from there (a
    /// plain `use <dep_wasm>::collections::<Name>;` after the `--extern-wasm-crate` remap) from EVERY
    /// using scope, root included, since the class no longer lives locally. Empty for the rust pass
    /// and whenever `--extern-wrapper-index` is unused, so output is byte-identical without the flag.
    ///
    /// `requested` (wasm pass, `--wrapper-requests` host side) is every collection wrapper hosted into
    /// `requested_scope` this run as `(structural class ident, requested RustType)`. Those wrappers are
    /// NOT IR structs, so the struct walk below never sees them; after it runs, each is walked as if it
    /// were a rule emitted at `requested_scope` (mirroring the Array/Table struct-walk arms) so its body
    /// imports EXACTLY the cross-scope element / scoped-extern wasm classes it names. Empty (and
    /// `requested_scope` `None`) for the rust pass and whenever `--wrapper-requests` is unused, so output
    /// is byte-identical without the flag.
    pub fn scope_references(
        &self,
        wasm: bool,
        deferred: &BTreeMap<RustIdent, ModuleScope>,
        requested: &[(RustIdent, RustType)],
        requested_scope: Option<&ModuleScope>,
    ) -> BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>> {
        // we only want to mark TOP-LEVEL references without recursing into those types
        // which is why we don't use visit_types() here
        let mut refs = BTreeMap::new();
        // Resolve wasm-map wrapper imports to the SAME module emission places them: a shape with a
        // sole owner is minted (class + structural alias) in that owner's module, everything else
        // falls back to the crate root. Computed once via the shared helper so the two sites can't drift.
        let table_shape_sole_owners = self.table_shape_sole_owners();
        fn set_ref(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            current_scope: &ModuleScope,
            rust_ident: &RustIdent,
        ) {
            let ref_scope = types.scope(rust_ident);
            if current_scope != ref_scope {
                refs.entry(current_scope.clone())
                    .or_default()
                    .entry(ref_scope.clone())
                    .or_default()
                    .insert(rust_ident.clone());
            }
        }
        // Register the import of a DEFERRED keys-list wrapper into `emit_scope` (the module a locally
        // minted map class is emitted in — root or the sole owner's). A map's `keys()` accessor names
        // the keys-list wrapper; when that wrapper is deferred to a dependency it must be imported
        // where the map class lives, from the dep's `collections` module. No-op when the keys-list is
        // not deferred (its class is local, same module). Independent of `current_scope`: it follows
        // the map class, not the using site.
        fn register_deferred_keys_list(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            key: &RustType,
        ) {
            let keys_ident = RustIdent::new(CDDLIdent::new(key.name_as_wasm_array(types)));
            if let Some(dep_scope) = deferred.get(&keys_ident) {
                refs.entry(emit_scope.to_owned())
                    .or_default()
                    .entry(dep_scope.clone())
                    .or_default()
                    .insert(keys_ident);
            }
        }
        // Register the import of a DEFERRED loose LIST wrapper that a locally-minted restricted
        // wrapper (`NonEmpty*List`, or a named `[+ …]` rule's class) borrows as its `try_from`
        // source. The `try_from(&<Elem>List)` reference is conversion-internal — invisible to the
        // field walk, the same class of problem as a map's `keys()`-list
        // (`register_deferred_keys_list`), solved the same way: follow the CLASS, not the using
        // site — import at the restricted wrapper's EMISSION scope, from the dep's `collections`
        // module. No-op when: the element is exposable (`try_from` takes a bare `Vec`, no loose
        // class is named) or itself non-empty (no loose source exists — built incrementally); the
        // loose name equals the wrapper ident (a self-named rule emits no `try_from`); or the
        // loose wrapper is not deferred (it is a local class in the same scope). Empty `deferred`
        // (rust pass / flag unused) makes this a no-op, so output is byte-identical without the flag.
        fn register_deferred_non_empty_list_source(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            wrapper_ident: &RustIdent,
            elem: &RustType,
        ) {
            if elem.directly_wasm_exposable(types) || elem.is_non_empty_array() {
                return;
            }
            let loose = elem.name_as_wasm_array(types);
            if loose == wrapper_ident.as_ref() {
                return;
            }
            let loose_ident = RustIdent::new(CDDLIdent::new(loose));
            if let Some(dep_scope) = deferred.get(&loose_ident) {
                let emit_scope = types.scope(wrapper_ident).clone();
                refs.entry(emit_scope)
                    .or_default()
                    .entry(dep_scope.clone())
                    .or_default()
                    .insert(loose_ident);
            }
        }
        // The map twin of `register_deferred_non_empty_list_source`: a locally-minted restricted
        // `NonEmptyMap*` (or named `{+ …}` rule) class enters via `try_from(&MapKToV)` — when that
        // loose structural table wrapper is deferred, import it at the restricted wrapper's
        // emission scope. Additional no-op case: the loose shape has a SOLE table-rule owner —
        // the `try_from` source is then the owner's local `pub type MapKToV = <Owner>;` alias
        // (see `generate_non_empty_map_type`), never a deferred class.
        #[allow(clippy::too_many_arguments)]
        fn register_deferred_non_empty_map_source(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            sole_owners: &BTreeMap<String, RustIdent>,
            wrapper_ident: &RustIdent,
            key: &RustType,
            value: &RustType,
            // the restricted wrapper's container flavor: its `try_from` source is the loose wrapper of
            // the SAME flavor (`PairMapKToV` for a `@duplicates preserve` `{+ …}`, `MapKToV` otherwise)
            preserve: bool,
        ) {
            let loose_ident = ConceptualRustType::name_for_wasm_map(key, value, preserve);
            if loose_ident.as_ref() == wrapper_ident.as_ref()
                || sole_owners.contains_key(&loose_ident.to_string())
            {
                return;
            }
            if let Some(dep_scope) = deferred.get(&loose_ident) {
                let emit_scope = types.scope(wrapper_ident).clone();
                refs.entry(emit_scope)
                    .or_default()
                    .entry(dep_scope.clone())
                    .or_default()
                    .insert(loose_ident);
            }
        }
        // Register the import of a locally ROOT-minted keys-list wrapper into `emit_scope` (the
        // module a table's wasm class is emitted in). A map's `keys()` accessor names the keys-list
        // wrapper BARE (`{Elem}List(...)`) exactly when the key is non-exposable AND the wrapper is
        // not deferred — mirroring `codegen_table_type`'s emission condition. That wrapper is
        // synthesized at ROOT_SCOPE (`create_and_register_array_type`, never `mark_scope`'d), so a
        // class emitted in a non-root module must import it. No-op (matching the emitter naming NO
        // wrapper, or naming one that lives in the same scope) when: not the wasm pass; the emit
        // scope IS root (wrapper minted there too); the key is exposable (bare `Vec` return, no
        // wrapper named); or the keys-list is deferred (`register_deferred_keys_list` imports it from
        // the dep's `collections` module instead). Independent of the using site: it follows the
        // table class, like `register_deferred_keys_list`.
        fn register_root_keys_list(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            wasm: bool,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            key: &RustType,
        ) {
            if !wasm || *emit_scope == *ROOT_SCOPE {
                return;
            }
            // exposable keys return a bare `Vec` — the emitter names no wrapper, so nothing to import
            if ConceptualRustType::Array(Box::new(key.clone())).directly_wasm_exposable_ct(types) {
                return;
            }
            let keys_ident = RustIdent::new(CDDLIdent::new(key.name_as_wasm_array(types)));
            // deferred keys-lists live in a dep's `collections` module — imported by the deferred
            // helper, not from root
            if deferred.contains_key(&keys_ident) {
                return;
            }
            refs.entry(emit_scope.to_owned())
                .or_default()
                .entry(ROOT_SCOPE.clone())
                .or_default()
                .insert(keys_ident);
        }
        // The non-deferred analogue of `register_deferred_non_empty_list_source`: a restricted list
        // wrapper (`NonEmpty*List` / a named `[+ …]` rule / a dedup owner) emitted at `emit_scope`
        // borrows a LOOSE `<Elem>List` as its `try_from` source, and that loose builder is a locally
        // minted class (typically ROOT-minted). Its `try_from(&<Elem>List)` names the loose builder
        // bare in `emit_scope`, so import it there — the list twin of `register_root_keys_list`. Also
        // register the loose builder's OWN element ref at the builder's scope (its `get`/`add`
        // accessors name the element bare where the builder lives). No-op when: the element is
        // exposable (`try_from` takes a bare `Vec`, no loose class) or itself non-empty (built
        // incrementally, no loose source); the loose name equals the wrapper ident (a self-named rule
        // emits no `try_from`); or the loose builder is deferred (the deferred helper imports it from
        // the dep's `collections` module instead).
        #[allow(clippy::too_many_arguments)]
        fn register_root_non_empty_list_source(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            wasm: bool,
            sole_owners: &BTreeMap<String, RustIdent>,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            wrapper_ident: &RustIdent,
            elem: &RustType,
        ) {
            if !wasm || elem.directly_wasm_exposable(types) || elem.is_non_empty_array() {
                return;
            }
            let loose = elem.name_as_wasm_array(types);
            if loose == wrapper_ident.as_ref() {
                return;
            }
            let loose_ident = RustIdent::new(CDDLIdent::new(loose));
            if deferred.contains_key(&loose_ident) {
                return;
            }
            let loose_scope = types.scope(&loose_ident).clone();
            if loose_scope != *emit_scope {
                refs.entry(emit_scope.to_owned())
                    .or_default()
                    .entry(loose_scope.clone())
                    .or_default()
                    .insert(loose_ident);
            }
            mark_refs(refs, types, wasm, sole_owners, deferred, &loose_scope, elem);
        }
        // The map twin of `register_root_non_empty_list_source`: a restricted `NonEmptyMap*` /
        // `NonEmptyPairMap*` wrapper (synthesized, named `{+ …}` rule, or dedup owner) emitted at
        // `emit_scope` enters via `try_from(&MapKToV)`, naming the LOOSE structural table wrapper bare
        // in `emit_scope`. Import it there, resolving the loose builder's own home the SAME way
        // emission places it (`table_shape_sole_owners`: the owner's `pub type MapKToV = <Owner>;`
        // module when a sole owner exists, else root). Also register the loose builder's key/value
        // refs at its scope (its accessors name them bare there). No-op when the loose name equals the
        // wrapper ident (self-named rule) or the loose builder is deferred.
        #[allow(clippy::too_many_arguments)]
        fn register_root_non_empty_map_source(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            wasm: bool,
            sole_owners: &BTreeMap<String, RustIdent>,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            wrapper_ident: &RustIdent,
            key: &RustType,
            value: &RustType,
            // the restricted wrapper's container flavor; see the deferred twin above
            preserve: bool,
        ) {
            if !wasm {
                return;
            }
            let loose_ident = ConceptualRustType::name_for_wasm_map(key, value, preserve);
            if loose_ident.as_ref() == wrapper_ident.as_ref() || deferred.contains_key(&loose_ident)
            {
                return;
            }
            let loose_scope = sole_owners
                .get(&loose_ident.to_string())
                .map(|owner| types.scope(owner).clone())
                .unwrap_or_else(|| types.scope(&loose_ident).clone());
            if loose_scope != *emit_scope {
                refs.entry(emit_scope.to_owned())
                    .or_default()
                    .entry(loose_scope.clone())
                    .or_default()
                    .insert(loose_ident.clone());
            }
            mark_refs(refs, types, wasm, sole_owners, deferred, &loose_scope, key);
            mark_refs(
                refs,
                types,
                wasm,
                sole_owners,
                deferred,
                &loose_scope,
                value,
            );
        }
        fn mark_refs(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            wasm: bool,
            sole_owners: &BTreeMap<String, RustIdent>,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            current_scope: &ModuleScope,
            ty: &RustType,
        ) {
            match &ty.conceptual_type {
                ConceptualRustType::Alias(alias_ident, alias_ty) => {
                    if let AliasIdent::Rust(rust_ident) = alias_ident {
                        set_ref(refs, types, current_scope, rust_ident);
                        // A named COLLECTION rule (`recs = [* foo]` / `withdrawals = {* k => v}`, or a
                        // generic instance like `gcn = gcoll<foo>`) registers a transparent alias, so
                        // a field referencing it is `Alias(Recs, Array(Foo))`. In the WASM pass the
                        // rule's OWN class (imported above via `set_ref`) IS the boundary surface —
                        // recursing the collection target would mint a structural-wrapper import
                        // (`FooList` / `MapKToV`) the rule subsumes and nothing else defines: E0432 for
                        // a locally-owned rule, or a dangling `crate::generated::MapKToV` for a
                        // DEP-owned rule (`table_shape_sole_owners` excludes non-exported scopes, so it
                        // falls back to a root structural name with no owner). Suppress the target
                        // recursion for such an alias; its element/key/value are the rule's concern,
                        // imported at the rule's own scope by its Table/Array struct-walk arm. Only the
                        // WASM pass names these structural wrappers, so the rust pass still recurses
                        // (byte-identical output).
                        if wasm
                            && matches!(
                                types.rust_struct(rust_ident).map(|rs| rs.variant()),
                                Some(RustStructType::Array { .. } | RustStructType::Table { .. })
                            )
                        {
                            return;
                        }
                    }
                    // Also import idents the serialization INLINED through this transparent alias
                    // will name. A cross-module NAMED `.cbor` ref (`fb = bytes .cbor foo` in module
                    // `a`, referenced by name from `b`) resolves the alias to its target
                    // (`pub type Fb = Foo;`), and `b`'s serialization emits `Foo::deserialize(..)`
                    // while only `Fb` was imported — E0433 `cannot find type Foo`. `set_ref` records
                    // only CROSS-scope idents, so single-module output is byte-identical; an
                    // occasionally-unneeded cross-module import is harmless (generated code
                    // legitimately over-imports; `unused_imports` is deliberately not denied).
                    // Only the conceptual type drives ref-marking, so wrap the alias target (a bare
                    // `ConceptualRustType`) in a throwaway `RustType`.
                    let alias_target = RustType::new((**alias_ty).clone());
                    mark_refs(
                        refs,
                        types,
                        wasm,
                        sole_owners,
                        deferred,
                        current_scope,
                        &alias_target,
                    );
                }
                ConceptualRustType::Rust(rust_ident) => {
                    set_ref(refs, types, current_scope, rust_ident)
                }
                ConceptualRustType::Array(elem_ty) => {
                    // Resolve the wasm wrapper this occurrence crosses the boundary as, and its
                    // emission scope, the SAME way the emitter (`for_wasm_member`) names it and the
                    // mint walk places it — so a using scope imports EXACTLY the ident the emitter
                    // references (`NonEmpty<Elem>List` / a dedup owner / a rule ident / the loose
                    // `<Elem>List`), never the pre-NonEmpty spelling, and from the wrapper's TRUE
                    // home rather than a hard-coded root.
                    if let Some((wrapper, emit_scope)) = wasm
                        .then(|| types.wasm_collection_wrapper(ty, sole_owners))
                        .flatten()
                    {
                        if let Some(dep_scope) = deferred.get(&wrapper) {
                            // Deferred to a dependency's `--extern-wrapper-index`: the wrapper class
                            // no longer lives locally — import it from the dep's `collections` module
                            // from EVERY using scope (root included) and do NOT recurse (wrapper and
                            // element are both the dependency's).
                            refs.entry(current_scope.to_owned())
                                .or_default()
                                .entry(dep_scope.clone())
                                .or_default()
                                .insert(wrapper);
                            return;
                        }
                        // Import the emitter-named wrapper into the using scope from its emission
                        // scope (a no-op when they coincide, e.g. an anonymous same-shape use inside
                        // the wrapper's own module).
                        if emit_scope != *current_scope {
                            refs.entry(current_scope.to_owned())
                                .or_default()
                                .entry(emit_scope.clone())
                                .or_default()
                                .insert(wrapper.clone());
                        }
                        // A RESTRICTED wrapper (`[+ …]` / `@duplicates reject`) borrows a LOOSE
                        // `<Elem>List` as its `try_from` source, named bare in its emission scope —
                        // import it there (deferred + non-deferred analogues).
                        if ty.is_non_empty_array() || ty.is_reject_ordered_set() {
                            register_deferred_non_empty_list_source(
                                refs, types, deferred, &wrapper, elem_ty,
                            );
                            register_root_non_empty_list_source(
                                refs,
                                types,
                                wasm,
                                sole_owners,
                                deferred,
                                &emit_scope,
                                &wrapper,
                                elem_ty,
                            );
                        }
                        // The wrapper's emitted code names its ELEMENT type bare in its EMISSION
                        // scope, which may not be this using scope — register the element ref from
                        // there. Recurse (not a single `set_ref`) so a nested anonymous wrapper
                        // resolves its own element too.
                        mark_refs(
                            refs,
                            types,
                            wasm,
                            sole_owners,
                            deferred,
                            &emit_scope,
                            elem_ty,
                        );
                        return;
                    }
                    // Exposable `[* uint]` (bare `Vec`) or the rust pass: recurse the element at the
                    // using scope, as before (rust-side output stays byte-identical).
                    mark_refs(
                        refs,
                        types,
                        wasm,
                        sole_owners,
                        deferred,
                        current_scope,
                        elem_ty,
                    );
                }
                ConceptualRustType::Fixed(_)
                | ConceptualRustType::Primitive(_)
                // `AnyCbor` is a static-runtime type imported globally (see the `uses_any_cbor`
                // dumb-push in generation/mod.rs), not a cross-scope generated ident, so ref-marking
                // has nothing to add here.
                | ConceptualRustType::Any => {
                    // nothing to import
                }
                ConceptualRustType::Map(key, value) => {
                    // Resolve the wasm map wrapper this occurrence crosses as, and its emission scope,
                    // the SAME way emission decides both — `for_wasm_member` for the NAME (the
                    // restricted `NonEmptyMap*` / a dedup owner / the loose `MapKToV`) and
                    // `table_shape_sole_owners` for the loose builder's HOME (the sole owner's module
                    // when one exists, else root). One helper, so import placement and emission
                    // placement cannot disagree.
                    if let Some((wrapper, emit_scope)) = wasm
                        .then(|| types.wasm_collection_wrapper(ty, sole_owners))
                        .flatten()
                    {
                        if let Some(dep_scope) = deferred.get(&wrapper) {
                            // The whole map wrapper is deferred to a dependency's
                            // `--extern-wrapper-index`: import it from the dep's `collections` module
                            // from every using scope (root included); wrapper, key, and value are all
                            // the dependency's, so don't recurse.
                            refs.entry(current_scope.to_owned())
                                .or_default()
                                .entry(dep_scope.clone())
                                .or_default()
                                .insert(wrapper);
                            return;
                        }
                        if emit_scope != *current_scope {
                            refs.entry(current_scope.to_owned())
                                .or_default()
                                .entry(emit_scope.clone())
                                .or_default()
                                .insert(wrapper.clone());
                        }
                        // A RESTRICTED `{+ …}` map wrapper enters via `try_from(&MapKToV)`, naming the
                        // loose structural table wrapper bare in its emission scope — import it there
                        // (deferred + non-deferred analogues).
                        if ty.is_non_empty_map() {
                            register_deferred_non_empty_map_source(
                                refs,
                                types,
                                deferred,
                                sole_owners,
                                &wrapper,
                                key,
                                value,
                                ty.is_preserve_pair_map(),
                            );
                            register_root_non_empty_map_source(
                                refs,
                                types,
                                wasm,
                                sole_owners,
                                deferred,
                                &emit_scope,
                                &wrapper,
                                key,
                                value,
                                ty.is_preserve_pair_map(),
                            );
                        }
                        // The map class's `keys()` accessor names the keys-list wrapper bare in its
                        // EMISSION scope — import it there (deferred from the dep's `collections`
                        // module, or a ROOT-minted `<Key>List` when non-exposable).
                        register_deferred_keys_list(refs, types, deferred, &emit_scope, key);
                        register_root_keys_list(refs, types, wasm, deferred, &emit_scope, key);
                        // The wrapper body names its KEY and VALUE types bare in its emission scope —
                        // register their refs from there.
                        mark_refs(refs, types, wasm, sole_owners, deferred, &emit_scope, key);
                        mark_refs(refs, types, wasm, sole_owners, deferred, &emit_scope, value);
                        return;
                    }
                    // The rust pass (maps always cross wasm through a wrapper, so this is rust-only):
                    // recurse key/value at the using scope, as before (byte-identical rust output).
                    mark_refs(refs, types, wasm, sole_owners, deferred, current_scope, key);
                    mark_refs(
                        refs,
                        types,
                        wasm,
                        sole_owners,
                        deferred,
                        current_scope,
                        value,
                    );
                }
                ConceptualRustType::Optional(inner_ty) => mark_refs(
                    refs,
                    types,
                    wasm,
                    sole_owners,
                    deferred,
                    current_scope,
                    inner_ty,
                ),
            }
        }
        for rust_struct in self.rust_structs().values() {
            let current_scope = self.scope(&rust_struct.ident);
            match rust_struct.variant() {
                RustStructType::Array {
                    element_type,
                    bounds,
                } => {
                    // A NAMED rule whose emitted class borrows the LOOSE `<Elem>List` as its
                    // `try_from(&<Elem>List)` source names that builder bare in THIS scope. Two rule
                    // families do so: a restricted `[+ …]` rule (`generate_non_empty_array_type`) and
                    // a `@duplicates reject` rule of ANY bounds (`generate_reject_ordered_set_type`) —
                    // a plain `[*] reject` set still enters through `try_from(&FooList)`, so gating on
                    // the non-empty bound alone left its loose-source import (at the rule's scope) and
                    // the loose builder's element ref (at ROOT, its emission scope) unregistered
                    // (E0425 on both `FooList` and its element). The two helpers below apply the
                    // element-exposable / non-empty-element / self-named / deferred guards that decide
                    // whether a loose source actually exists, so a plain non-reject `[* foo]` rule
                    // (whose class wraps `Vec<Foo>` directly, no `try_from` source) is correctly a
                    // no-op even when it reaches here.
                    // LOCKSTEP: this gate mirrors `generate_reject_ordered_set_type`'s /
                    // `generate_non_empty_array_type`'s `loose_list` decision — a reject rule emits
                    // `try_from(&Loose)` regardless of its `[*]`/`[+]` bound. Change them together.
                    if wasm
                        && (*bounds == Some((Some(1), None))
                            || rust_struct.config().duplicates
                                == Some(crate::comment_ast::DuplicatesPolicy::Reject))
                    {
                        // The deferred (`--extern-wrapper-index`) analogue: when the loose `<Elem>List`
                        // is owned by a mapped dependency, import it from the dep's `collections`
                        // module at this rule's scope. Routed for reject rules too, by the same
                        // same-condition principle (a reject rule over a dep-owned element defers its
                        // loose source exactly as a non-empty rule does); a no-op when `deferred` is
                        // empty, so output is byte-identical without the flag.
                        register_deferred_non_empty_list_source(
                            &mut refs,
                            self,
                            deferred,
                            &rust_struct.ident,
                            element_type,
                        );
                        // The non-deferred analogue: the loose `<Elem>List` is a locally (ROOT-)
                        // minted class the rule's `try_from(&<Elem>List)` names bare in THIS scope,
                        // so import it here (E0425 otherwise). Fixes the `necollrec` and `rsetrec`
                        // cells.
                        register_root_non_empty_list_source(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rust_struct.ident,
                            element_type,
                        );
                    }
                    mark_refs(
                        &mut refs,
                        self,
                        wasm,
                        &table_shape_sole_owners,
                        deferred,
                        current_scope,
                        element_type,
                    )
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants, .. } => {
                    let is_group_choice =
                        matches!(rust_struct.variant(), RustStructType::GroupChoice { .. });
                    variants.iter().for_each(|ev| match &ev.data {
                        EnumVariantData::RustType(ty) => {
                            mark_refs(
                                &mut refs,
                                self,
                                wasm,
                                &table_shape_sole_owners,
                                deferred,
                                current_scope,
                                ty,
                            );
                            // A GROUP choice's `new_<variant>` ctor (both passes) expands a
                            // named-Record variant's fields into direct parameters, so the
                            // emitted code names those FIELD types in THIS scope — a Record
                            // living in another module otherwise only registers them for its
                            // own scope (its Record arm below) and the expanded ctor fails
                            // E0412. Mark exactly the ctor-visible set via the same helper the
                            // emitters use (a TYPE choice never expands — `generate_enum`'s
                            // `rep.and(..)` gate — so marking it here would only add unused
                            // imports).
                            if is_group_choice {
                                for field in ev
                                    .group_ctor_record_fields(self, &rust_struct.ident)
                                    .unwrap_or_default()
                                {
                                    mark_refs(
                                        &mut refs,
                                        self,
                                        wasm,
                                        &table_shape_sole_owners,
                                        deferred,
                                        current_scope,
                                        &field.rust_type,
                                    )
                                }
                            }
                        }
                        EnumVariantData::Inlined(record) => {
                            record.fields.iter().for_each(|field| {
                                mark_refs(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    current_scope,
                                    &field.rust_type,
                                )
                            })
                        }
                    })
                }
                RustStructType::Record(record) => {
                    record.fields.iter().for_each(|field| {
                        mark_refs(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &field.rust_type,
                        )
                    });
                    // Open rest (map `* k => v` row or array `* t` tail): mark its CONTAINER
                    // (`RestRow::container_type` — the same `Map(k, v)`/`Array(t)` the rest field's
                    // member type and its wasm wrapper mint are built from), not the inner types
                    // flat. The container routes through `mark_refs`' Map/Array arms, so a rest row
                    // gets EXACTLY what a map/array FIELD of the same shape gets: the wrapper class
                    // the rest accessor returns imported into THIS scope from its emission scope,
                    // the `keys()` list wrapper registered at that emission scope, and the inner
                    // types marked THERE (the wrapper body names them, and it may live in another
                    // module). Marking the inners flat at the using scope instead left both sides
                    // dangling (E0425 on the wrapper here and on its key/value at root) for every
                    // rest row whose inner types are not root-scoped — the same-scope case only
                    // worked because `current_scope == emit_scope` made the flat marking
                    // coincidentally correct. Rust-side output is unchanged: with `wasm == false`
                    // both container arms fall through to marking the inners at the using scope,
                    // which is what this did.
                    if let Some(rest) = &record.rest {
                        mark_refs(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rest.container_type(),
                        );
                    }
                }
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    // The named table's own wasm class is emitted in `current_scope`; its `keys()`
                    // accessor names the keys-list wrapper bare. Register BOTH homes, exactly as the
                    // inline Map arm above does: a ROOT-minted `<Key>List` for a non-exposable key
                    // (`register_root_keys_list`), OR a `use <dep_wasm>::collections::<KeysList>;` when
                    // the keys-list is workspace/index deferred (`register_deferred_keys_list`). Before
                    // the alias-recursion suppression that removed the accidental import route, a field
                    // referencing this named table recursed into the Map arm, whose call registered the
                    // deferred import; the named-rule arm is the correct home for it (follow the CLASS,
                    // not the using site — same rationale as the existing helpers).
                    register_deferred_keys_list(&mut refs, self, deferred, current_scope, domain);
                    register_root_keys_list(&mut refs, self, wasm, deferred, current_scope, domain);
                    // A NAMED `{+ …}` rule's restricted class borrows the LOOSE structural
                    // `MapKToV` as its `try_from` source; when that source is deferred, import it
                    // at THIS rule's scope — the rule-named analogue of the inline Map arm's
                    // registration above.
                    if wasm && *bounds == Some((Some(1), None)) {
                        // the rule's own `@duplicates` config picks its container flavor, so the
                        // `try_from` source resolved below is the loose wrapper of the SAME flavor
                        let preserve = rust_struct.config().duplicates
                            == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
                        register_deferred_non_empty_map_source(
                            &mut refs,
                            self,
                            deferred,
                            &table_shape_sole_owners,
                            &rust_struct.ident,
                            domain,
                            range,
                            preserve,
                        );
                        // The non-deferred analogue: the loose `MapKToV` builder is a locally
                        // (ROOT- or sole-owner-) minted class the rule's `try_from(&MapKToV)` names
                        // bare in THIS scope, so import it here (E0425 otherwise). Fixes the
                        // `nemap`/`nepmap`/`nepmapa` cells.
                        register_root_non_empty_map_source(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rust_struct.ident,
                            domain,
                            range,
                            preserve,
                        );
                    }
                    mark_refs(
                        &mut refs,
                        self,
                        wasm,
                        &table_shape_sole_owners,
                        deferred,
                        current_scope,
                        domain,
                    );
                    mark_refs(
                        &mut refs,
                        self,
                        wasm,
                        &table_shape_sole_owners,
                        deferred,
                        current_scope,
                        range,
                    );
                }
                RustStructType::Wrapper { wrapped, .. } => mark_refs(
                    &mut refs,
                    self,
                    wasm,
                    &table_shape_sole_owners,
                    deferred,
                    current_scope,
                    wrapped,
                ),
                RustStructType::Extern | RustStructType::RawBytesType => {
                    // impossible to know what this refers to - will have to be done afterwards by user
                }
                RustStructType::CStyleEnum { .. } => {
                    // should only refer to constants
                }
            }
        }
        // Type aliases are their own emission surface: a plain alias rule (`bal = st`) emits a
        // `pub type Bal = St;` line naming its TARGET bare in the alias's scope, with no field
        // reference for the struct walk above to see — a cross-scope target under-imported (E0412,
        // hit in production by `policy_id = script_hash`-style domain aliasing; the matrix
        // `aliased` cells pin every shape). Walk each emitted alias's `base_type` through
        // `mark_refs` — the rust line renders `for_rust_member(base_type)`, and the wasm
        // alias-base MINT walk (`generation/mod.rs`, the `visit_types_excluding` loop over
        // `type_aliases`) mints wrappers for the base's structural shapes regardless of what the
        // alias line names, so the import walk covers the base symmetrically (imports follow
        // minting; for a named-collection alias this re-records what its struct twin already did,
        // a `BTreeSet` no-op). The wasm alias line itself substitutes the stripped plain-typename
        // target's wrapper class when `resolved_wasm_alias_target` says so (the emitter consults
        // the SAME helper, so the emitted target and its import cannot drift) — that ident is
        // invisible in the stripped `base_type`, so import it additionally (from the dep's
        // `collections` module when deferred, like every deferred wrapper reference).
        // `set_ref`/`mark_refs` record only CROSS-scope idents, so single-module output is
        // byte-identical.
        for (alias_ident, alias_info) in self.type_aliases() {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            let emitted_this_pass = if wasm {
                alias_info.gen_wasm_alias
            } else {
                alias_info.gen_rust_alias
            };
            if !emitted_this_pass {
                continue;
            }
            let current_scope = self.scope(ident);
            // A generic-EXTERN instance's alias base is a `Base<Args>` TYPE EXPRESSION minted by
            // `RustIdent::new_generic_with_base` (`ExtSet<Plain>`, `ExtSetRawBytes<PubKey>`), not an
            // importable path segment — `finalize`'s `GenericResolved::Extern` arm registers it as
            // `Rust(Base<Args>)` (a COLLECTION-bodied generic instance instead registers a transparent
            // structural alias, handled by the shape guard just below). Feeding that opaque ident
            // through `mark_refs`→`set_ref` would land the whole `<…>`-carrying text verbatim in the
            // scope's `use crate::generated::{…}` list (invalid Rust; the rustfmt post-pass aborts).
            // Decompose it instead: import the base at the base extern's DECLARING scope (where the
            // re-export glue in `generation/mod.rs` places `pub use crate::<Base>[RawBytes];` — NOT
            // `self.scope(&base_ident)`, since the flavored name is unregistered and would misroute
            // to root), and walk each argument so the bare arg names the alias line renders resolve
            // too. The wasm pass never reaches here (these aliases are `gen_wasm_alias=false`, gated
            // out above), so no wasm-side handling is needed.
            if let Some(gi) = self.generic_instances.get(ident) {
                let base_ident = gi.extern_base_ident(self);
                // Only a generic-EXTERN instance's alias base is the opaque `<Base>[RawBytes]<Args>`
                // type expression this block must decompose. A generated generic def with a
                // COLLECTION body (`xs<T> = [* T]` / `{* k => T}`, instanced as `xs<uint>`) resolves
                // to a TRANSPARENT structural alias (`Vec<u64>` / `BTreeMap<..>`) — its base has no
                // `<Args>` to strip and imports correctly through the normal `mark_refs` walk below,
                // so fall through rather than misroute it here. The shape guard IS the discriminator:
                // an instance alias whose base is `Rust(base)` prefixed by the extern base name can
                // only be the extern type expression (collection instances register `Array`/`Map`
                // bases, record instances register no alias at all).
                if matches!(
                    &alias_info.base_type.conceptual_type,
                    ConceptualRustType::Rust(base) if base.as_ref().starts_with(base_ident.as_ref())
                ) {
                    let base_scope = self.scope(&gi.generic_ident).clone();
                    if base_scope != *current_scope {
                        refs.entry(current_scope.clone())
                            .or_default()
                            .entry(base_scope)
                            .or_default()
                            .insert(base_ident);
                    }
                    for arg in gi.generic_args() {
                        mark_refs(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            arg,
                        );
                    }
                    continue;
                }
            }
            if wasm && let Some(target) = alias_info.resolved_wasm_alias_target(self) {
                if let Some(dep_scope) = deferred.get(target) {
                    refs.entry(current_scope.clone())
                        .or_default()
                        .entry(dep_scope.clone())
                        .or_default()
                        .insert(target.clone());
                } else {
                    set_ref(&mut refs, self, current_scope, target);
                }
            }
            mark_refs(
                &mut refs,
                self,
                wasm,
                &table_shape_sole_owners,
                deferred,
                current_scope,
                &alias_info.base_type,
            );
        }
        // W2 dep side (`--wrapper-requests`): the hosted requested wrappers are emitted into
        // `requested_scope` but are NOT in the IR, so the struct walk above never marked the wasm
        // classes their bodies name (element for a list, key/value/keys-list for a map, plus a
        // restricted wrapper's loose `try_from` source). Mirror the Array/Table struct-walk arms with
        // `requested_scope` as the emission scope — a hosted wrapper is exactly a rule emitted there —
        // so a cross-scope element (a struct in a non-root module) or a scoped extern (whose re-export
        // glue lands in its declaring scope, not the root) is imported from its true home instead of
        // being (un)reached by the ROOT-only `use super::*;`. Empty `requested` (rust pass / flag unused)
        // makes this a no-op, so output is byte-identical without the flag.
        if wasm && let Some(req_scope) = requested_scope {
            let requested_idents: BTreeSet<RustIdent> =
                requested.iter().map(|(id, _)| id.clone()).collect();
            // Mark the ref a hosted wrapper's body names for one member (element / key / value). A member
            // that is ITSELF a hosted requested collection lives in `requested_scope` too (same file):
            // its wasm class is named bare with nothing to import, and its own body is walked when the
            // loop reaches its entry — so skip it rather than let `wasm_collection_wrapper` misroute the
            // structural name to the crate root (`types.scope` doesn't know the requested wrappers). Every
            // other member routes through the shared `mark_refs`, resolving to the member's true home.
            let mark_requested_member =
                |refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
                 member: &RustType| {
                    if let Some((wrapper, _)) =
                        self.wasm_collection_wrapper(member, &table_shape_sole_owners)
                        && requested_idents.contains(&wrapper)
                    {
                        return;
                    }
                    mark_refs(
                        refs,
                        self,
                        wasm,
                        &table_shape_sole_owners,
                        deferred,
                        req_scope,
                        member,
                    );
                };
            for (wid, rt) in requested {
                match &rt.conceptual_type {
                    ConceptualRustType::Array(elem) => {
                        // A restricted list (`[+ …]` / `@duplicates reject`) borrows a LOOSE `<Elem>List`
                        // as its `try_from` source, named bare at the emission scope. Import it there —
                        // unless that loose source is itself a hosted requested wrapper (same scope, no
                        // import; `register_root_*` would misroute the structural name to root).
                        if rt.is_non_empty_array() || rt.is_reject_ordered_set() {
                            register_deferred_non_empty_list_source(
                                &mut refs, self, deferred, wid, elem,
                            );
                            let loose =
                                RustIdent::new(CDDLIdent::new(elem.name_as_wasm_array(self)));
                            if !requested_idents.contains(&loose) {
                                register_root_non_empty_list_source(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    req_scope,
                                    wid,
                                    elem,
                                );
                            }
                        }
                        mark_requested_member(&mut refs, elem);
                    }
                    ConceptualRustType::Map(key, value) => {
                        // The map class's `keys()` accessor names the keys-list wrapper bare at the
                        // emission scope (deferred from a dep's `collections`, or a ROOT-minted
                        // `<Key>List` for a non-exposable key). Skip the ROOT-import when the keys-list
                        // is ITSELF a co-hosted requested wrapper (same file): it is minted here, so
                        // `register_root_keys_list` would misroute the structural name to the crate
                        // root (E0432) — the keys-list twin of the loose-`try_from`-source guards on
                        // the list/map non-empty arms above. `register_deferred_keys_list` stays
                        // unguarded: it is a no-op for a locally-hosted keys-list and MUST still run
                        // for a mid-chain host whose keys-list is deferred to a deeper dep.
                        register_deferred_keys_list(&mut refs, self, deferred, req_scope, key);
                        let keys_ident =
                            RustIdent::new(CDDLIdent::new(key.name_as_wasm_array(self)));
                        if !requested_idents.contains(&keys_ident) {
                            register_root_keys_list(
                                &mut refs, self, wasm, deferred, req_scope, key,
                            );
                        }
                        // A restricted `{+ …}` map borrows a LOOSE `MapKToV` as its `try_from` source
                        // named bare at the emission scope — same requested-source guard as the list arm.
                        if rt.is_non_empty_map() {
                            register_deferred_non_empty_map_source(
                                &mut refs,
                                self,
                                deferred,
                                &table_shape_sole_owners,
                                wid,
                                key,
                                value,
                                rt.is_preserve_pair_map(),
                            );
                            let loose = ConceptualRustType::name_for_wasm_map(
                                key,
                                value,
                                rt.is_preserve_pair_map(),
                            );
                            if !requested_idents.contains(&loose) {
                                register_root_non_empty_map_source(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    req_scope,
                                    wid,
                                    key,
                                    value,
                                    rt.is_preserve_pair_map(),
                                );
                            }
                        }
                        mark_requested_member(&mut refs, key);
                        mark_requested_member(&mut refs, value);
                    }
                    // A requested shape is always a collection (guarded in `emit_requested_collections`).
                    _ => {}
                }
            }
        }
        refs
    }

    fn aliases() -> BTreeMap<idents::AliasIdent, AliasInfo> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, AliasInfo>::new();
        let mut insert_alias = |name: &str, rust_type: RustType| {
            let ident = AliasIdent::new(CDDLIdent::new(name));
            aliases.insert(ident, AliasInfo::new_manual(rust_type, false, false));
        };
        insert_alias("uint", ConceptualRustType::Primitive(Primitive::U64).into());
        insert_alias("nint", ConceptualRustType::Primitive(Primitive::N64).into());
        insert_alias(
            "bool",
            ConceptualRustType::Primitive(Primitive::Bool).into(),
        );
        // TODO: define enum or something as otherwise it can overflow i64
        // and also we can't define the serialization traits for types
        // that are defined outside of this crate (includes primitives)
        //"int" => "i64",
        let string_type: RustType = ConceptualRustType::Primitive(Primitive::Str).into();
        insert_alias("tstr", string_type.clone());
        insert_alias("text", string_type);
        insert_alias(
            "bstr",
            ConceptualRustType::Primitive(Primitive::Bytes).into(),
        );
        insert_alias(
            "bytes",
            ConceptualRustType::Primitive(Primitive::Bytes).into(),
        );
        let null_type: RustType = ConceptualRustType::Fixed(FixedValue::Null).into();
        insert_alias("null", null_type.clone());
        insert_alias("nil", null_type);
        insert_alias(
            "true",
            ConceptualRustType::Fixed(FixedValue::Bool(true)).into(),
        );
        insert_alias(
            "false",
            ConceptualRustType::Fixed(FixedValue::Bool(false)).into(),
        );
        // Note: defaulting to float64 for "float" (so without precision).
        insert_alias(
            "float",
            ConceptualRustType::Primitive(Primitive::F64).into(),
        );
        insert_alias(
            "float64",
            ConceptualRustType::Primitive(Primitive::F64).into(),
        );
        insert_alias(
            "float32",
            ConceptualRustType::Primitive(Primitive::F32).into(),
        );
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    /// The alias-substitution rule: a REGISTERED alias resolves to its base type, kept behind an
    /// `Alias` wrapper when the alias generates a rust type (`gen_rust_alias` — the wrapper is what
    /// preserves the alias's name for naming derivations) and substituted transparently when it
    /// doesn't; an unregistered ident is `None` (the caller decides the fallback). This is the ONE
    /// owner of that rule: `new_type` (the canonical pipeline constructor) and the
    /// `--wrapper-requests` shape parser (generation/requests.rs `parse_shape_fragment`) both call it, so a
    /// leaf built outside the pipeline cannot drift from pipeline resolution — the drift is exactly
    /// how alias-element requests once panicked `is_enum`'s registered-struct invariant (pinned by
    /// `workspace_requests_alias_elements_host`). Immutable on purpose: prelude emission for
    /// unregistered reserved idents is `new_type`'s fallback, not part of the rule.
    pub fn resolve_alias(&self, alias_ident: &AliasIdent) -> Option<RustType> {
        self.type_aliases.get(alias_ident).map(|info| {
            if info.gen_rust_alias {
                info.base_type.clone().as_alias(alias_ident.clone())
            } else {
                info.base_type.clone()
            }
        })
    }

    // note: this is mut so the unregistered-reserved fallback can mark which reserved idents
    // are in the CDDL prelude so we don't generate code for all of them, potentially
    // bloating generated code a bit
    pub fn new_type(&mut self, raw: &CDDLIdent, cli: &Cli) -> RustType {
        let alias_ident = AliasIdent::new(raw.clone());
        let resolved = match self.resolve_alias(&alias_ident) {
            Some(ty) => ty,
            None => match &alias_ident {
                // CDDL `any` — the prelude name for "some CBOR I don't model". Intercept it here at
                // the unresolved-Rust fallback so a USER rule literally named `any` (`any = uint`)
                // still shadows it: a registered user alias resolves via `resolve_alias` ABOVE and
                // never reaches this arm. `any` is not in `is_identifier_reserved`, so it classes
                // as `AliasIdent::Rust`; without this it would return a bare `Rust("Any")` naming a
                // struct that never exists (the historic panic/non-compile class).
                AliasIdent::Rust(_) if raw.to_string() == "any" => ConceptualRustType::Any.into(),
                AliasIdent::Rust(_) => ConceptualRustType::Rust(RustIdent::new(raw.clone())).into(),
                AliasIdent::Reserved(reserved) if reserved == "int" => {
                    // We define an Int rust struct in prelude.rs
                    ConceptualRustType::Rust(RustIdent::new(raw.clone())).into()
                }
                // The CDDL prelude constant `undefined` (major type 7, simple value 23). Unlike
                // `null`/`true`/`false` it has no `FixedValue` and no Rust value to store, so there
                // is nothing for a member, an element, or a rule body to hold — refuse instead of
                // aborting. Intercepted HERE, at the same unresolved-reserved fallback as the `any`
                // arm above, because that is the one seam every position funnels through: a user
                // rule literally named `undefined` still shadows it (a registered alias resolves in
                // `resolve_alias` above and never reaches this arm), and the refusal needs the
                // `IntermediateTypes` handle that `cddl_prelude` — a pure `&str -> Option<&str>` —
                // does not have. The consequence is that the message is ROLE-NEUTRAL by
                // construction: this seam knows the name, never the position it was written in.
                // The `Fixed(FixedValue::Null)` placeholder is the inert stand-in the sibling
                // rejections in `rust_type_from_type2` use, so the walk continues and `finalize`
                // reports this alongside anything else it finds.
                AliasIdent::Reserved(reserved) if reserved == "undefined" => {
                    self.record_rejection(
                        "the CDDL prelude type `undefined` (major type 7, simple value 23) is \
                         unsupported — it has no representation in generated code. A position that \
                         only needs to carry an arbitrary CBOR item (`undefined` included) can use \
                         the supported `any` type; constraining a position specifically to \
                         `undefined` is not supported."
                            .to_string(),
                    );
                    ConceptualRustType::Fixed(FixedValue::Null).into()
                }
                AliasIdent::Reserved(reserved) => {
                    // we auto-include only the parts of the cddl prelude necessary (and supported)
                    cddl_prelude(reserved).unwrap_or_else(|| {
                        panic!(
                            "{}",
                            "Reserved ident {reserved} not a part of cddl_prelude?"
                        )
                    });
                    self.emit_prelude(reserved.clone(), cli);
                    // Resolve to whatever the emitted `prelude_<x>` rule resolves to, exactly
                    // as a user-written reference to that rule would. This yields a proper
                    // Alias (for plain prelude types like biguint) or a Rust struct ref (for
                    // type-choice prelude types like bigint), instead of a bare Rust ident
                    // pointing at an unregistered type alias - which panics downstream lookups
                    // (is_enum, cbor_types, ...) that assume Rust(ident) names a real struct.
                    self.new_type(&CDDLIdent::new(format!("prelude_{reserved}")), cli)
                }
            },
        };
        let resolved_inner = match &resolved.conceptual_type {
            ConceptualRustType::Alias(_, ty) => ty,
            ty => ty,
        };
        if cli.binary_wrappers {
            // if we're not literally bytes/bstr, and instead an alias for it
            // we would have generated a named wrapper object so we should
            // refer to that instead
            if !is_identifier_reserved(&raw.to_string())
                && let ConceptualRustType::Primitive(Primitive::Bytes) = resolved_inner
            {
                return ConceptualRustType::Rust(RustIdent::new(raw.clone())).into();
            }
        }
        // Array element types and map KEY types in the Special CBOR class (bool / null /
        // float16-32-64 / simple, major type 7) share their major type with the
        // indefinite-length break byte (`0xff`), so a naive loop can't tell "read another item"
        // from "stop at the break". `make_deser_loop_break_check` (generation/deserialize.rs) handles this
        // correctly in both framings: a definite-length collection reads exactly `n` items and
        // never inspects for a break, and the indefinite case uses the non-consuming
        // `special_break()` probe so a bool/null/float element/key is left in place and read
        // normally while only the real `0xff` break stops the loop. So a named `[* float64]` /
        // `{ * bool => uint }`, definite OR indefinite, deserializes correctly (covered by the
        // homogeneous_array / special_map_key corpus round-trips and the golden_hex_preserve KATs)
        // — not something an assert here needs to guard.
        resolved
    }

    pub fn register_type_alias(&mut self, alias: RustIdent, info: AliasInfo) {
        if let ConceptualRustType::Alias(_ident, _ty) = &info.base_type.conceptual_type {
            panic!(
                "register_type_alias*({}, {:?}) wraps automatically in Alias, no need to provide it.",
                alias, info.base_type
            );
        }
        // A top-level rule whose entire body resolves to a bare fixed value — `foo = 5`, `foo = -5`,
        // `foo = "text"`, `foo = 5.0`, the reserved-alias constants `foo = true`/`false`/`null`/`nil`,
        // or any of these behind a tag head (`foo = #6.n(5)`) — arrives here as a standalone `Fixed`
        // conceptual type. `Fixed` has NO standalone/member Rust representation: it exists only
        // implicitly, as an unstored struct/array member whose value is fixed by the schema, so
        // exposing it as a top-level type would panic `for_rust_member`/`for_wasm_member` during
        // generation. Reject gracefully here — the single choke point every top-level alias passes
        // through — via the normal rejection channel: `finalize` short-circuits on `has_rejections`
        // BEFORE any resolution/generation runs, so the recorded rejection becomes an `Err` and the
        // `Fixed` alias below never reaches the panic site. Supporting it (a wrapper newtype carrying
        // the constant) is future work. This does NOT touch member-position fixed values
        // (`foo = [1, uint]`, `foo = { bar: 1 }`): those live on group entries and are never
        // registered as a top-level alias. It also leaves the auto-wrapping tag-inner variants alone
        // (`#6.n(uint .default 5)`, `#6.n(uint .le 255)`): those resolve to a Primitive wrapper
        // struct, not a bare `Fixed`.
        //
        // We still INSERT the alias (rather than returning early): a sibling rule can reference this
        // one (`foo = 5` + `m = { foo => uint }`), and its parse resolves the reference through the
        // alias table. Dropping the entry would leave that reference dangling and panic a downstream
        // lookup during parse — before `finalize` ever surfaces the graceful `Err`. The registered
        // `Fixed` alias is harmless because `finalize` never generates once a rejection is recorded.
        if let ConceptualRustType::Fixed(fixed) = &info.base_type.conceptual_type {
            let fixed = fixed.clone();
            self.record_bare_fixed_rule_rejection(&alias, &fixed);
        }
        self.type_aliases.insert(alias.into(), info);
    }

    /// The rejection for a top-level rule whose whole body is a bare fixed value. Shared by the two
    /// registration seams a rule body can land on — the transparent-alias seam
    /// (`register_type_alias` above, `foo = 5` / `foo = true` / `foo = #6.5(5)`) and the WRAPPER
    /// seam in the parse walk (`foo = #6.11(true)`, where a tag head or `@newtype` forces
    /// `RustStruct::new_wrapper` instead of an alias). They are genuinely different seams — one
    /// inserts into the alias table, the other registers a rust struct — so each keeps its own
    /// guard, and this helper keeps the ONE message they both emit from drifting apart. The text is
    /// a matrix `code_anchor` (`cddl-matrix/annotations/corpus/cddl_codegen.toml`, the
    /// `value.number` / `prelude.true` family): reword it and those annotations dangle.
    pub fn record_bare_fixed_rule_rejection(&mut self, rule: &RustIdent, fixed: &FixedValue) {
        let value_desc = fixed.cddl_source_desc();
        self.record_rejection(format!(
            "rule `{rule}`: a top-level rule whose entire body is a bare fixed value ({value_desc}) \
             is unsupported — a fixed value has no standalone type representation, only meaning as an \
             (unstored) struct or array member. Wrap it in a group (e.g. `{rule} = [{value_desc}]`) \
             or reference it from a member position."
        ));
    }

    pub fn rust_struct(&self, ident: &RustIdent) -> Option<&RustStruct> {
        self.rust_structs.get(ident)
    }

    /// Whether `ident` is exposed to wasm AS a `#[wasm_bindgen]` wrapper struct (vs. directly, like a
    /// `Copy` c-style enum, or as a transparent `pub type`). A named collection/struct/wrapper generates
    /// a wrapper; a c-style enum is exposed directly; anything not a rust-struct (a plain type-alias or
    /// primitive) has no wrapper. This is the single source of truth the wasm alias emission consults so
    /// a passthrough alias to a named map/array (`ptm = mp`) points at that wrapper instead of the
    /// inline-only `MapU64To…` name. Mirrors `ConceptualRustType::directly_wasm_exposable`'s alias arm.
    pub fn has_wasm_wrapper(&self, ident: &RustIdent) -> bool {
        match self.rust_struct(ident).map(|rs| rs.variant()) {
            Some(RustStructType::CStyleEnum { .. }) | None => false,
            Some(_) => true,
        }
    }

    /// mostly for convenience since this is checked in so many places
    pub fn is_enum(&self, ident: &RustIdent) -> bool {
        if let Some(rs) = self.rust_struct(ident) {
            matches!(rs.variant(), RustStructType::CStyleEnum { .. })
        } else {
            // could be a generic instead. (Message text is a recombination-sweep panic-class key —
            // known-class ledgers match on it, so don't reword casually.)
            assert!(self.generic_instances.contains_key(ident));
            false
        }
    }

    // this is called by register_table_type / register_array_type automatically
    pub fn register_rust_struct(
        &mut self,
        parent_visitor: &ParentVisitor,
        mut rust_struct: RustStruct,
        cli: &Cli,
    ) {
        match &rust_struct.variant {
            RustStructType::Table {
                domain,
                range,
                bounds,
            } => {
                // Synthesize the keys-list array wrapper only for a table rule the crate OWNS. A
                // table rule defined inside an extern-deps stub (non-exported scope) describes a
                // type the DEPENDENCY owns; the synthesized keys-list wrapper defaults to
                // ROOT_SCOPE (it is never `mark_scope`'d) and would therefore be minted in the
                // CONSUMER's own output — an alias/`#[wasm_bindgen]` class the consumer neither
                // owns nor references from its own spec, duplicating a wrapper the dep exports. The
                // `register_type_alias` below still runs unconditionally so the ident resolves as a
                // type for cross-crate references.
                // A domain typed as a not-yet-resolved GENERIC-COLLECTION instance is still a bare
                // `Rust(<instance>)` here (its transparent alias is registered only in `finalize`),
                // so naming the keys-list wrapper from it now bakes the INSTANCE-ident name
                // (`GcollU64List` for `gcoll<uint>`). But `finalize`'s
                // `resolve_generic_collection_instance_fields` then rewrites the domain to its resolved
                // collection (`Array(u64)` for an exposable element), and the wasm `keys()` accessor
                // names the wrapper from THAT — the structural `ArrU64List`, an E0425 against the
                // instance-named mint. Defer the mint to `finalize_generic_table_keys_lists` (run
                // right after the domain resolution) so the keys-list is named from the FINAL domain,
                // matching `keys()`. Non-generic table domains are already final here and mint as before.
                if self.scope(&rust_struct.ident).export()
                    && !matches!(&domain.conceptual_type, ConceptualRustType::Rust(id) if self.generic_instances.contains_key(id))
                {
                    // we must provide the keys type to return
                    self.create_and_register_array_type(
                        parent_visitor,
                        domain.clone(),
                        &domain.conceptual_type.name_as_wasm_array_ct(self),
                        cli,
                    );
                }
                let mut map_type: RustType =
                    ConceptualRustType::Map(Box::new(domain.clone()), Box::new(range.clone()))
                        .into();
                if let Some(bounds) = bounds {
                    // the occurrence-count bounds ride the alias so every embed site of the named
                    // table enforces them (deserialize routes through the NonEmptyMap TryFrom door),
                    // exactly like the `Array` arm below
                    map_type = map_type.with_bounds(*bounds);
                }
                // `@duplicates` rides the alias too. For tables `reject` is the default (a no-op
                // recorded for self-documentation) while `preserve` swaps the member to the
                // `PairMap`/`NonEmptyPairMap` vec-of-pairs twin, so this seam is the single place a
                // table-preserve embed site (and the extern-interface projection) reads the policy.
                map_type = map_type.with_duplicates_policy(rust_struct.config().duplicates);
                if let Some(tag) = rust_struct.tag {
                    map_type = if rust_struct.tag_optional {
                        map_type.optionally_tag(tag)
                    } else {
                        map_type.tag(tag)
                    };
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    AliasInfo::new_manual(map_type, true, false),
                )
            }
            RustStructType::Array {
                element_type,
                bounds,
            } => {
                let mut array_type: RustType =
                    ConceptualRustType::Array(Box::new(element_type.clone())).into();
                if let Some(bounds) = bounds {
                    // the occurrence-count length bounds ride the alias so every embed site of
                    // the named array enforces them (deserialize + fallible constructor)
                    array_type = array_type.with_bounds(*bounds);
                }
                // `@duplicates reject` rides the alias so every embed site (and generic use-site
                // re-resolution) sees the uniqueness twin. Applied POST-arm so the raw arm types the
                // tag-set collapse recognizer compared for equality stayed policy-free.
                array_type = array_type.with_duplicates_policy(rust_struct.config().duplicates);
                if let Some(tag) = rust_struct.tag {
                    array_type = if rust_struct.tag_optional {
                        array_type.optionally_tag(tag)
                    } else {
                        array_type.tag(tag)
                    };
                }
                self.register_type_alias(
                    rust_struct.ident.clone(),
                    AliasInfo::new_manual(array_type, true, false),
                )
            }
            RustStructType::Wrapper {
                min_max: Some(_), ..
            }
            | RustStructType::Wrapper {
                float_min_max: Some(_),
                ..
            } => {
                self.mark_new_can_fail(rust_struct.ident.clone());
            }
            _ => (),
        }
        // A `@newtype` wrapper over an INLINE ARRAY set (`#6.258([* a]) ; @newtype`,
        // `[* a] ; @newtype @duplicates reject`) selects its inner representation by the effective
        // `@duplicates` policy exactly as a transparent array alias does (the `Array` arm above
        // `with_duplicates_policy` its stored member type): reject ⇒ the `OrderedSet`/`NonEmptyOrderedSet`
        // uniqueness twin, preserve ⇒ today's plain `Vec`/`NonEmptyVec`. The policy is written on THIS
        // rule (an explicit `@duplicates` directive or the single-arm registry default) so it lives in
        // the wrapper's struct config, but the inline array type built at the parse site carries none —
        // thread it onto the STORED wrapped type here so `generate_wrapper_struct` (which reads the inner
        // via the wrapped type, not the struct config) and every IR walker see one consistent shape.
        // Only override when this rule actually carries a policy: a `@newtype` over a REFERENCED set
        // rule (`homogeneous = #6.24(homogeneous_inner)`) has no directive of its own, and its wrapped
        // type already carries the referenced rule's policy through the alias — overwriting with `None`
        // would clobber that inherited reject/preserve back to a plain `Vec`.
        // Scoped to ARRAY wrapped types deliberately: the table twin (`{* k => v} ; @newtype` +
        // `@duplicates preserve` ⇒ a `PairMap` inner) has NO wired wasm boundary — the synthesized
        // structural map wasm wrapper class wraps `BTreeMap`, not `PairMap` (a preserve-table ALIAS
        // works under wasm only because the named rule itself becomes the `PairMap` wasm class). Wiring
        // that PairMap-aware synthesized wasm class is Phase 2.2's per-kind wrapper work; until then the
        // parse site hard-rejects `@duplicates` on a `@newtype` table rather than emit a broken wasm
        // crate or silently drop the directive.
        if let Some(policy) = rust_struct.config().duplicates
            && let RustStructType::Wrapper { wrapped, .. } = &mut rust_struct.variant
            && matches!(wrapped.conceptual_type, ConceptualRustType::Array(_))
        {
            *wrapped = wrapped.clone().with_duplicates_policy(Some(policy));
        }
        self.rust_structs
            .insert(rust_struct.ident().clone(), rust_struct);
    }

    /// Re-resolve fields typed as a generic COLLECTION instance's transparent alias.
    ///
    /// A non-generic collection rule (`foo = #6.258([* uint])`) registers its transparent alias in
    /// `type_aliases` at PARSE time, so a use-site field referencing it resolves through `new_type`
    /// to the structural `Alias(Foo, Array(..))` conceptual type right where the field is built. A
    /// generic instance's alias (`xs_int = xs<uint>`) is instead registered only in `finalize` (the
    /// `GenericResolved::Resolved` → [`Self::register_rust_struct`] Array/Table arms), AFTER every
    /// use-site field was already built — so such a field keeps an unresolved `Rust(xs_int)`
    /// conceptual type, which generation turns into `self.field.serialize(..)` /
    /// `XsInt::deserialize(..)` method calls a bare `Vec`/`BTreeMap` alias has no impls for.
    ///
    /// Now that the late aliases exist, re-run alias substitution on the affected field leaves so the
    /// generic path converges on the SAME `Alias(ident, Array/Map)` field type the non-generic path
    /// gets — one collection code path (inline serialize, per-field len/elem/tag encoding vars), not
    /// a parallel one. Scoped to instances whose alias base is a structural `Array`/`Map`: generic
    /// EXTERN instances (alias base `Rust(real_ident)`, registered above with the same
    /// `gen_rust_alias=true`) resolve to a `Rust` type, are excluded here, and stay byte-identical.
    /// Converge each SYNTHESIZED anonymous generic-collection instance onto the anonymous INLINE
    /// collection path for the wasm boundary. Wasm only.
    ///
    /// An anonymous instance (`[a: set<key_hash>]` → `SetKeyHash`) that resolves to a transparent
    /// collection is registered — exactly like a bare `foo = #6.258([* key_hash])` rule — as an
    /// `Array`/`Table`-variant `RustStruct` PLUS a transparent alias whose `gen_wasm_alias` is `false`,
    /// so the wasm struct walk mints a `#[wasm_bindgen]` class under the RULE ident (`SetKeyHash`).
    /// That is wrong for a synthesized name: the equivalent inline `[* key_hash]` field mints its
    /// wrapper under the STRUCTURAL name (`KeyHashList`). Two spellings of one anonymous shape then
    /// define two wasm classes for one concept — and a `--wrapper-requests` consumer importing the
    /// structural name hard-errors (the synthesized name sits in `own_wrapper_shapes`).
    ///
    /// Fix: flip the instance alias's `gen_wasm_alias` to `true` and record the ident here. The alias
    /// loop then emits `pub type SetKeyHash = KeyHashList;` (`for_wasm_member` on the alias base is
    /// bounds-aware, so `[+ …]` yields the `NonEmpty…List` name and a directly-exposable element
    /// yields the bare `Vec<…>`), the base-type walk mints the STRUCTURAL wrapper (recording it in
    /// `own_wrapper_shapes` under the structural name), and the struct walk is told to SKIP the
    /// rule-named class mint (via `is_anonymous_collection_instance`). The rust side is untouched — the
    /// transparent `pub type SetKeyHash = Vec<KeyHash>;` alias and every rust reference to it stay
    /// byte-identical. NAMED instance rules (`named_set = set<key_hash>`, ident from the author's rule)
    /// are NOT anonymous, keep their own wasm class, and keep the criterion-8 `--wrapper-requests`
    /// contract. Runs alongside `resolve_generic_collection_instance_fields` (both after the late
    /// instance aliases exist); order between them does not matter — this only edits alias flags.
    fn converge_anonymous_collection_instance_wasm(&mut self, cli: &Cli) {
        if !cli.wasm {
            return;
        }
        // (ident, gets_wrapper) for every anonymous instance resolving to a collection.
        // `gets_wrapper` is true for a `[+ …]` (always wrapped), a `@duplicates reject` set (always
        // crosses through its `OrderedSet`/`NonEmptyOrderedSet` wrapper), or a non-exposable element
        // (`set<key_hash>`), and false for a directly-exposable loose collection (`set<uint>` → bare
        // `Vec<u64>`). Every wrapper-getting instance routes through the structural-wrapper alias
        // passthrough below — for a reject set that structural wrapper is the uniqueness twin.
        let anon_collection: Vec<(RustIdent, bool)> = self
            .generic_instances
            .values()
            .filter(|i| i.anonymous)
            .filter_map(|i| {
                let rt = self.resolve_alias(&AliasIdent::Rust(i.instance_ident.clone()))?;
                let shallow = rt.conceptual_type.resolve_alias_shallow();
                // Non-collection anonymous instances (a generic EXTERN, `extern_generic<foo>`, whose
                // alias base is a bare `Rust(...)`) are out of scope AND must not reach the
                // exposability probe below: `directly_wasm_exposable_ct` calls `is_enum`, whose
                // rust-struct/generic-instance assertion an extern's synthesized element ident would
                // trip. Screen them out first.
                if !matches!(
                    shallow,
                    ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
                ) {
                    return None;
                }
                // Exposability is tested on the RESOLVED collection (a `[+ …]` is never exposable;
                // otherwise ask the shallow-resolved shape) — NOT on the `Alias` wrapper, whose own
                // rust-struct entry would wrongly report "wrapped" for the very type we are converging.
                // A reject-mode set is never directly exposable (it crosses through its OrderedSet
                // wrapper, like `[+ …]`), so it always gets a wrapper — the conceptual exposability
                // probe below can't see the policy (it lives on the RustType config), so add it here.
                let gets_wrapper = rt.is_type_enforced_non_empty()
                    || rt.duplicates_reject()
                    || !shallow.directly_wasm_exposable_ct(self);
                Some((i.instance_ident.clone(), gets_wrapper))
            })
            .collect();
        for (ident, gets_wrapper) in anon_collection {
            // Every anonymous collection instance skips the rule-named class mint (recorded here). The
            // WRAPPER subset additionally routes through a `gen_wasm_alias` passthrough to the
            // STRUCTURAL wrapper (`pub type SetKeyHash = KeyHashList;` for the loose case, or
            // `pub type OsetU64 = U64OrderedSet;` for a `@duplicates reject` set — `for_wasm_member`
            // on the alias base picks the twin name). The exposable
            // subset needs no alias: `resolve_generic_collection_instance_fields` lowers its field to
            // the bare inline collection (`Vec<u64>`), so the wasm boundary crosses by value exactly
            // like an inline `[* uint]` — no wrapper class, no `&Vec` ctor param (no `RefFromWasmAbi`).
            if gets_wrapper
                && let Some(alias) = self.type_aliases.get_mut(&AliasIdent::Rust(ident.clone()))
            {
                alias.gen_wasm_alias = true;
            }
            self.anonymous_collection_instances.insert(ident);
        }
    }

    fn resolve_generic_collection_instance_fields(&mut self) {
        // Snapshot the resolved alias `RustType` for each generic-collection instance (base resolves
        // to Array/Map), cloned out first so the field walk can borrow `rust_structs` mutably. The
        // snapshot value is exactly what `new_type` would have returned at parse time: the alias's
        // `Alias(ident, Array/Map)` conceptual type carrying the optional-tag encoding op and the
        // occurrence bounds.
        let mut resolved: BTreeMap<RustIdent, RustType> = BTreeMap::new();
        let idents: Vec<RustIdent> = self.generic_instances.keys().cloned().collect();
        for ident in idents {
            let Some(rt) = self.resolve_alias(&AliasIdent::Rust(ident.clone())) else {
                continue;
            };
            let shallow = rt.conceptual_type.resolve_alias_shallow();
            // A NAMED set-nominal binding (`named_set = set<key_hash>`) resolves to
            // `Alias(NamedSet, Rust(SetKeyHash))` whose base is the instantiation NOMINAL struct — not
            // a transparent Array/Map. Its use-site fields were built as a bare `Rust(NamedSet)` at
            // parse time (before the late alias existed), which generation would look up as a struct
            // and panic. Re-resolve those leaves to the `Alias` so they name the nominal through the
            // `pub type NamedSet = SetKeyHash;` binding (Phase 2.3). Anonymous instances already carry
            // `Rust(SetKeyHash)` at their fields (the canonical IS a registered struct) and need no
            // re-resolution here.
            let is_set_nominal_alias = matches!(
                shallow,
                ConceptualRustType::Rust(id)
                    if self.rust_struct(id).map(|rs| rs.config().set_nominal).unwrap_or(false)
            );
            if is_set_nominal_alias {
                resolved.insert(ident, rt);
                continue;
            }
            if !matches!(
                shallow,
                ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
            ) {
                continue;
            }
            // A SYNTHESIZED anonymous instance that resolves to a DIRECTLY-exposable collection
            // (`set<uint>` → `Vec<u64>`, populated by `converge_anonymous_collection_instance_wasm`)
            // lowers its field to the BARE collection — the alias wrapper is dropped, so the field's
            // type is exactly the inline `[* uint]` shape (`Vec<u64>`, not `Alias(SetU64, …)`). That
            // makes it cross the wasm boundary by value with no wrapper class, byte-identical to the
            // inline equivalent, instead of a `&SetU64` ref param with no `RefFromWasmAbi`. The bare
            // base keeps the collapsed-set encoding op (the optional tag rides on `base_type`), so the
            // rust CBOR bytes are unchanged; only the rust field-type SPELLING becomes `Vec<u64>`
            // (the same type the `pub type SetU64 = Vec<u64>` alias still names). Wrapper-getting
            // anonymous instances (non-exposable / `[+ …]`) and NAMED instances keep the `Alias`.
            let exposable_anon = self.is_anonymous_collection_instance(&ident)
                && !rt.is_type_enforced_non_empty()
                && !rt.duplicates_reject()
                && shallow.directly_wasm_exposable_ct(self);
            let replacement = if exposable_anon {
                self.type_aliases
                    .get(&AliasIdent::Rust(ident.clone()))
                    .map(|info| info.base_type.clone())
            } else {
                Some(rt)
            };
            if let Some(replacement) = replacement {
                resolved.insert(ident, replacement);
            }
        }
        if resolved.is_empty() {
            return;
        }
        // Replace a `Rust(instance)` leaf with the resolved collection alias, keeping any
        // reference-site encodings (`#6.24(xs_int)`-style outer wraps) OUTSIDE the alias's own by
        // appending them. Recurses into structural children first so `[* xs_int]` / `? xs_int` reach
        // the leaf. Does not descend through the inserted `Alias` box — nested collection-of-generic
        // -instance is out of scope (no such shape reaches here today).
        fn walk(rt: &mut RustType, resolved: &BTreeMap<RustIdent, RustType>) {
            match &mut rt.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, resolved)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, resolved);
                    walk(v, resolved);
                }
                _ => {}
            }
            let replacement = match &rt.conceptual_type {
                ConceptualRustType::Rust(ident) => resolved.get(ident).cloned(),
                _ => None,
            };
            if let Some(mut new_rt) = replacement {
                new_rt.encodings.append(&mut rt.encodings);
                *rt = new_rt;
            }
        }
        for rust_struct in self.rust_structs.values_mut() {
            match &mut rust_struct.variant {
                RustStructType::Record(record) => {
                    for field in record.fields.iter_mut() {
                        walk(&mut field.rust_type, &resolved);
                    }
                }
                RustStructType::Table { domain, range, .. } => {
                    walk(domain, &resolved);
                    walk(range, &resolved);
                }
                RustStructType::Array { element_type, .. } => {
                    walk(element_type, &resolved);
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    for variant in variants.iter_mut() {
                        match &mut variant.data {
                            EnumVariantData::RustType(ty) => walk(ty, &resolved),
                            EnumVariantData::Inlined(rec) => {
                                for field in rec.fields.iter_mut() {
                                    walk(&mut field.rust_type, &resolved);
                                }
                            }
                        }
                    }
                }
                RustStructType::Wrapper { wrapped, .. } => walk(wrapped, &resolved),
                RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    }

    /// Mint the keys-list array wrapper for each exported table whose domain was a generic-collection
    /// instance (deferred from `register_rust_struct`; see the deferral comment there). Runs in
    /// `finalize` AFTER `resolve_generic_collection_instance_fields` has rewritten each such domain to
    /// its resolved collection, so the wrapper name derives from the FINAL domain and matches the wasm
    /// `keys()` accessor. Wasm-only (rust maps use native `.keys()`; the wrapper exists only to cross
    /// the wasm boundary). Guarded by "not already registered": a non-generic table minted its keys-list
    /// at parse (its domain was final there) and is a no-op here — so the byte output for any spec
    /// WITHOUT a generic-collection-instance-keyed table is unchanged. Deterministic (`BTreeMap` order).
    /// If two deferred tables resolve to the SAME domain, both pass the not-registered filter before
    /// either mints; the second `create_and_register_array_type` re-mints the identical keys-list
    /// struct, which `register_rust_struct` overwrites with a byte-identical entry — the same
    /// last-wins-on-shared-shape idiom the parse-time keys-list mint already relies on, so it is benign.
    fn finalize_generic_table_keys_lists(&mut self, parent_visitor: &ParentVisitor, cli: &Cli) {
        if !cli.wasm {
            return;
        }
        let deferred: Vec<RustType> = self
            .rust_structs
            .iter()
            .filter_map(|(ident, rs)| match rs.variant() {
                RustStructType::Table { domain, .. } if self.scope(ident).export() => {
                    let name = domain.conceptual_type.name_as_wasm_array_ct(self);
                    // A directly-exposable KEYS-LIST (`{ * uint => v }` -> bare `Vec<u64>` keys) mints
                    // no wrapper; `create_and_register_array_type` returns early on it, so only a real
                    // wrapper name that is not already registered identifies a deferred mint.
                    (!ConceptualRustType::Array(Box::new(domain.clone()))
                        .directly_wasm_exposable_ct(self)
                        && !self
                            .rust_structs
                            .contains_key(&RustIdent::new(CDDLIdent::new(name.clone()))))
                    .then(|| domain.clone())
                }
                _ => None,
            })
            .collect();
        for domain in deferred {
            let name = domain.conceptual_type.name_as_wasm_array_ct(self);
            self.create_and_register_array_type(parent_visitor, domain, &name, cli);
        }
    }

    // creates a RustType for the array type - and if needed, registers a type to generate
    // TODO: After the split we should be able to only register it directly
    // and then examine those at generation-time and handle things ALWAYS as RustType::Array
    pub fn create_and_register_array_type(
        &mut self,
        parent_visitor: &ParentVisitor,
        element_type: RustType,
        array_type_name: &str,
        cli: &Cli,
    ) -> RustType {
        let raw_arr_type = ConceptualRustType::Array(Box::new(element_type.clone()));
        // only generate an array wrapper if we can't wasm-expose it raw
        if raw_arr_type.directly_wasm_exposable_ct(self) {
            return raw_arr_type.into();
        }
        let array_type_ident = RustIdent::new(CDDLIdent::new(array_type_name));
        // If we are the only thing referring to our element and it's a plain group
        // we must mark it as being serialized as an array
        if let ConceptualRustType::Rust(_) = &element_type.conceptual_type {
            self.set_rep_if_plain_group(
                parent_visitor,
                &array_type_ident,
                Representation::Array,
                cli,
            );
        }
        if cli.wasm {
            // Whether an AUTHORED rule of this exact structural ident (`foo_list = [* foo]`) already
            // registered its own Array struct. When it did, THIS synthesis re-mints a byte-identical
            // entry (the last-wins idiom) but must NOT stamp the `synthesized_collection` marker over
            // the authored provenance: an authored keys-list is a rule-declared class that must still
            // survive `--no-synthesized-rust-collection-aliases` AND still trip the criterion-9 shadow
            // warning. Only a purely-synthesized keys-list (no authored rule) gets the marker.
            let already_registered = self.rust_structs.contains_key(&array_type_ident);
            // ...and whether that prior registration is INCOMPATIBLE — a rule of this ident that is
            // not the same-element loose builder. The `register_rust_struct` below is last-wins, so
            // it is about to delete that rule outright: record the claim while it is still visible,
            // for `non_empty_wrapper_name_collisions` to reject in `finalize`. Nothing else can see
            // it afterwards — the overwritten entry is byte-identical to a purely synthesized one.
            if already_registered
                && !self.provides_compatible_loose_list(
                    array_type_name,
                    &element_type.clone().resolve_aliases(),
                )
            {
                self.swallowed_structural_list_claims
                    .insert(array_type_name.to_owned(), element_type.clone());
            }
            // we don't pass in tags here. If a tag-wrapped array is done I think it generates
            // 2 separate types (array wrapper -> tag wrapper struct)
            self.register_rust_struct(
                parent_visitor,
                RustStruct::new_array(
                    array_type_ident.clone(),
                    None,
                    None,
                    element_type.clone(),
                    None,
                ),
                cli,
            );
            // register_rust_struct's Array arm just registered this keys-list's transparent rust
            // alias (`pub type XxxList = Vec<Elem>;`) via `new_manual` — indistinguishable from an
            // authored `foo_list = [* foo]` by provenance alone. Mark it here (the sole synthesis
            // site) so `--no-synthesized-rust-collection-aliases` can suppress only it, AND so the
            // wasm struct walk's Array arm does not pass `rule_declared: true` for it (a false
            // criterion-9 shadow warning over a keys-list no rule declares). Suppressed when an
            // authored rule registered first, preserving that rule's provenance.
            if !already_registered
                && let Some(alias) = self.type_aliases.get_mut(&array_type_ident.into())
            {
                alias.synthesized_collection = true;
            }
        }
        ConceptualRustType::Array(Box::new(element_type)).into()
    }

    pub fn register_generic_def(&mut self, def: GenericDef) {
        let ident = def.orig.ident().clone();
        self.generic_defs.insert(ident, def);
    }

    pub fn register_generic_instance(&mut self, instance: GenericInstance) {
        let ident = instance.instance_ident.clone();
        self.generic_instances.insert(ident, instance);
    }

    /// Phase 2.4 consolidation seam: rewrite every INLINE `#6.258([* T])` occurrence in the finalized
    /// construction products into a reference to a shape-derived nominal set wrapper (`SetU64`,
    /// `SetNonEmptyText`, `SetSetU64`, …), minting one wrapper per DEDUPED shape. This is the SINGLE
    /// place inline set nominalization + effective-policy (258 ⇒ reject) resolution happens; the
    /// `Type2::TaggedData` arm builds only the plain tagged occurrence, so a discarded transient
    /// type-choice arm can never mint a spurious nominal (the class the old `SUPPRESS_INLINE_TAG_DEFAULT`
    /// thread-local worked around, now structurally impossible).
    ///
    /// Walks both `rust_structs` (record fields, table domain/range, named-array elements, enum arms,
    /// wrapper inners) and `type_aliases` — the alias walk is the hook for the `T / null` corner (a
    /// `foo = #6.258([* uint]) / null` collapses to an `Optional(..)` ALIAS that bypasses
    /// `register_rust_struct`, so it would otherwise never be visited).
    fn nominalize_inline_sets(
        &mut self,
        parent_visitor: &ParentVisitor,
        cli: &Cli,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut minted: BTreeMap<RustIdent, RustStruct> = BTreeMap::new();
        // `mem::take` to sidestep the borrow: the rewrite is a pure function of each product's own
        // types, accumulating minted wrappers on the side.
        let mut structs = std::mem::take(&mut self.rust_structs);
        for rs in structs.values_mut() {
            rewrite_inline_sets_in_struct(rs, &mut minted);
        }
        self.rust_structs = structs;
        let mut aliases = std::mem::take(&mut self.type_aliases);
        for info in aliases.values_mut() {
            rewrite_inline_sets_in_type(&mut info.base_type, &mut minted);
        }
        self.type_aliases = aliases;
        // Register each minted nominal once (`minted` is already deduped by canonical ident, and it is
        // a `BTreeMap`, so registration order is deterministic). `register_rust_struct` threads the
        // reject policy onto the wrapped array (selecting the `OrderedSet` twin); the minted wrapper
        // contains no further inline set, so there is no re-entrancy.
        for (ident, nominal) in minted {
            // A shape-derived ident that already names a registered struct OR a user type alias is a
            // genuine collision — a user rule (`set_u64 = [x: uint]` → a `SetU64` struct, or
            // `set_u64 = text` → a `SetU64` type alias) or a generic instantiation (`set<uint>` →
            // `SetU64`, a DIFFERENT nominal: two-arm optional-tag record vs this inline mandatory-tag
            // record). Overwriting would silently re-point the rewritten reference; refuse loudly with a
            // SET-nominal-specific message (the per-kind sibling of the duplicate-top-level-ident
            // backstop, whose generic "list/map wrapper families" text is misleading here).
            if self.rust_structs.contains_key(&ident)
                || self
                    .type_aliases
                    .contains_key(&AliasIdent::Rust(ident.clone()))
            {
                return Err(format!(
                    "name collision: the shape-derived nominal `{ident}` synthesized for an inline \
                     `#6.258([* …])` set occurrence collides with an already-defined rule or generic \
                     set instantiation of the same name — rename the colliding rule, or hoist the \
                     inline occurrence to a distinctly-named rule"
                )
                .into());
            }
            // The nominal defaults to `@duplicates reject` (IANA set semantics) — a decode-behavior
            // change (loose historical bytes with duplicate elements now fail `DuplicateKey`). Notice
            // it once per minted nominal, naming the type and the hoist-to-named-rule opt-out (inline
            // positions have no comment slot for `@duplicates`).
            crate::warn!(
                "Inline #6.258 set occurrence nominalized to `{ident}` (defaults to @duplicates reject, IANA set semantics) — hoist it to a named rule with `; @duplicates preserve` to opt out"
            );
            self.register_rust_struct(parent_visitor, nominal, cli);
        }
        Ok(())
    }

    // call this after all types have been registered
    pub fn finalize(
        &mut self,
        parent_visitor: &ParentVisitor,
        cli: &Cli,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Surface any deferred rejections BEFORE any resolution runs, so nothing downstream
        // operates on the incomplete IR a skipped-field record leaves behind.
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        // resolve generics
        // resolve then register in 2 phases to get around borrow checker
        let resolved_generics = self
            .generic_instances
            .values()
            .map(|instance| instance.resolve(self, cli))
            .collect::<Result<Vec<_>, _>>()?;
        // Dedup guard for generic SET-NOMINAL instantiations: every spelling of `set<key_hash>`
        // resolves to the same `canonical_ident` (`SetKeyHash`), which must mint exactly ONE nominal
        // wrapper struct. A named binding whose own ident differs (`named_set` → `NamedSet`) then
        // aliases transparently to it.
        let mut minted_set_nominals: BTreeSet<RustIdent> = BTreeSet::new();
        for resolved_instance in resolved_generics {
            match resolved_instance {
                GenericResolved::Resolved(rs) => self.register_rust_struct(parent_visitor, rs, cli),
                GenericResolved::SetNominal {
                    instance_ident,
                    canonical_ident,
                    resolved,
                } => {
                    if minted_set_nominals.insert(canonical_ident.clone()) {
                        self.register_rust_struct(parent_visitor, resolved, cli);
                    }
                    // A named binding (`named_set = set<key_hash>`) becomes a transparent alias TO the
                    // instantiation nominal: `pub type NamedSet = SetKeyHash;` (rust AND wasm — wasm
                    // keeps ONE class + this passthrough alias). An anonymous instance's ident already
                    // IS the canonical, so it needs no alias.
                    if instance_ident != canonical_ident {
                        self.register_type_alias(
                            instance_ident,
                            AliasInfo::new_manual(
                                ConceptualRustType::Rust(canonical_ident).into(),
                                true,
                                true,
                            ),
                        );
                    }
                }
                GenericResolved::Extern {
                    instance_ident,
                    real_ident,
                    flavored_base,
                } => {
                    // `@raw_bytes_flavor` selected the `<Base>RawBytes` wrapper for this instance;
                    // record the base so the extern re-export glue emits `pub use crate::<Base>RawBytes;`
                    // (in addition to the plain `pub use crate::<Base>;` the base extern carries).
                    if let Some(base) = flavored_base {
                        self.mark_raw_bytes_flavor_emitted(base);
                    }
                    // must be generic extern - register it so other lookups don't fail
                    self.register_rust_struct(
                        parent_visitor,
                        RustStruct::new_extern(instance_ident.clone()),
                        cli,
                    );
                    // we do direct rust alias replacing (gen_rust_alias=false) since no problems with generics in rust
                    // but wasm_bindgen can't work with it directly we assume the user will supply the correct mappings
                    self.register_type_alias(
                        instance_ident,
                        AliasInfo::new_manual(
                            ConceptualRustType::Rust(real_ident).into(),
                            true,
                            false,
                        ),
                    );
                }
            }
        }
        // The `Resolved` arm above registered each generic COLLECTION instance's transparent alias
        // (`xs_int = xs<uint>` → `pub type XsInt = Vec<u64>;`) only just now — AFTER every use-site
        // field was built at parse time. Re-resolve those fields so the generic path converges on
        // the SAME structural collection field type the non-generic path already has (see the method
        // doc); must run BEFORE the key-demand / encoding analysis below so they see the collection.
        // Classify each SYNTHESIZED anonymous collection instance for the wasm boundary FIRST (this
        // populates `anonymous_collection_instances` and flips `gen_wasm_alias` for the wrapper
        // subset), so the field re-resolution below can see the classification and lower the
        // directly-exposable subset onto the bare inline collection. Wasm-only; non-wasm untouched.
        self.converge_anonymous_collection_instance_wasm(cli);
        self.resolve_generic_collection_instance_fields();
        // Mint the wasm keys-list wrappers whose owning table had a GENERIC-COLLECTION-instance
        // domain — deferred from `register_rust_struct` until now, so they name from the resolved
        // domain (see the deferral comment there). Idempotent by the not-yet-registered guard: a
        // non-generic table already minted its keys-list at parse and is skipped here.
        self.finalize_generic_table_keys_lists(parent_visitor, cli);
        // Phase 2.4: nominalize INLINE `#6.258([* T])` occurrences into shape-derived `Set<Elem>`
        // wrappers, at the ONE post-collapse seam (over the finalized construction PRODUCTS, never in
        // `rust_type_from_type2`). Must run BEFORE the key-demand analysis below so the minted
        // wrappers' elements get the full comparison bundle via the set-nominal block there, and after
        // the generic resolution/re-resolution above so every registered product (incl. resolved
        // generic instances) is seen in final shape.
        self.nominalize_inline_sets(parent_visitor, cli)?;
        // recursively check all types used as keys or contained within a type used as a key
        // this is so we only derive comparison or hash traits for those types. Demand is propagated as
        // SETS (`DemandSet`), union-merged: a tagged root spreads ITS flavor to every contained type;
        // an auto-detected internal map key spreads the mode-dependent `bare` internal bundle. Flavors
        // can only ADD to `bare`, never narrow it, so a type that is both is safe.
        let mut key_demand: BTreeMap<RustIdent, DemandSet> = BTreeMap::new();
        fn mark_key_demand(
            ty: &ConceptualRustType,
            key_demand: &mut BTreeMap<RustIdent, DemandSet>,
            demand: DemandSet,
        ) {
            if let ConceptualRustType::Rust(ident) = ty {
                let e = key_demand.entry(ident.clone()).or_default();
                *e = e.union(demand);
            }
        }
        // An auto-detected internal map key demands today's `bare` internal bundle (mode-dependent).
        let bare = DemandSet {
            bare: true,
            hash: false,
            ord: false,
        };
        // A `@duplicates reject` set's element type goes through the uniqueness twin's
        // `TryFrom<Vec<T>>` door, whose hybrid `scan_unique` (linear below a small-size threshold,
        // sorted-index above) is bounded `T: Ord` — so demand the `ord` flavor
        // (`Eq/PartialEq/Ord/PartialOrd`). (`mark_key_demand` marks `Rust(ident)` nodes only;
        // primitive/std elements carry `Ord` intrinsically EXCEPT floats, which are rejected
        // gracefully below like float map keys.)
        let ord = DemandSet {
            bare: false,
            hash: false,
            ord: true,
        };
        // A SET NOMINAL's element needs the FULL comparison bundle: the wrapper's always-on
        // `PartialEq/Eq/PartialOrd/Ord/Hash` derives flow through its `OrderedSet`/`Vec` inner onto
        // the element type. Matches the demand the wrapper forces on ITSELF (`wrappers.rs`).
        let full_set_demand = DemandSet {
            bare: true,
            hash: true,
            ord: true,
        };
        fn check_used_as_key(
            ty: &ConceptualRustType,
            types: &IntermediateTypes<'_>,
            key_demand: &mut BTreeMap<RustIdent, DemandSet>,
            bare: DemandSet,
        ) {
            if let ConceptualRustType::Map(k, _v) = ty {
                k.visit_types(types, &mut |ty| mark_key_demand(ty, key_demand, bare));
            }
        }
        // A map key that is (or recursively contains) a float compiles to a `BTreeMap<f64, _>` (or
        // an `OrderedHashMap` bounded `K: Hash + Eq + Ord` under --preserve-encodings); floats
        // implement none of Eq/Ord/Hash, so the emitted crate always fails to build (E0277). Such
        // rules are rejected gracefully at generation below. Collected into a local set (recorded
        // after the loop) to sidestep the borrow checker, like `used_as_key`, since the loop borrows
        // `self` immutably. `visit_types` guards recursion with a visited-ident set, so a
        // self-referential key type can't loop.
        let mut float_key_rejections = BTreeSet::new();
        fn key_contains_float(ty: &ConceptualRustType, types: &IntermediateTypes<'_>) -> bool {
            let mut found = false;
            ty.visit_types(types, &mut |t| {
                if matches!(
                    t,
                    ConceptualRustType::Primitive(Primitive::F32 | Primitive::F64)
                ) {
                    found = true;
                }
            });
            found
        }
        fn float_key_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: table key type contains a float (floats have no total order, so they cannot be map keys) — use an integer/text/bytes key domain instead"
            )
        }
        // The rest-row twin of `float_key_msg`: an open struct-map's captured entries live in the same
        // `BTreeMap`/`OrderedHashMap` a table's do, so a float key domain is the same E0277 — named
        // for the position so the remedy points at the rest row rather than at a table rule.
        fn float_rest_key_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: open struct-map rest-row key type contains a float (floats have no total order, so they cannot be map keys) — use an integer/text/bytes key domain instead"
            )
        }
        // The set-side twin of `float_key_msg`: a set's uniqueness door and always-on comparison
        // derives need `Ord` on the element, which floats don't have.
        fn float_set_elem_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: set element type contains a float (floats have no total order, so set elements cannot be compared for uniqueness) — use a non-float element type, or drop the uniqueness requirement (`@duplicates preserve` on a tag-258 set rule; no directive on a plain array rule)"
            )
        }
        // do a recursive check on the ones explicitly tagged as keys using @used_as_key: each tagged
        // root spreads its OWN flavor to every type it (transitively) contains. Iterating the roots map
        // (not the full `key_demand`, which finalize is about to expand) keeps the propagated flavor
        // exactly what the tag declared.
        for (ident, demand) in &self.key_demand_roots {
            if let Some(rust_struct) = self.rust_struct(ident) {
                let demand = *demand;
                rust_struct
                    .visit_types(self, &mut |ty| mark_key_demand(ty, &mut key_demand, demand));
            }
        }
        // check all other places used as keys
        for rust_struct in self.rust_structs().values() {
            let rule_ident = rust_struct.ident().clone();
            rust_struct.visit_types(self, &mut |ty| {
                check_used_as_key(ty, self, &mut key_demand, bare);
                // A nested/inline map (`{ number => uint }` as an array element or map value)
                // surfaces as a Map conceptual type rather than a Table struct, so its float key is
                // rejected here — the Table branch below only sees top-level `x = { k => v }` rules.
                if let ConceptualRustType::Map(k, _v) = ty
                    && key_contains_float(&k.conceptual_type, self)
                {
                    float_key_rejections.insert(float_key_msg(&rule_ident));
                }
            });
            // A reject-mode set's element type gets the `ord` demand so the twin's uniqueness scan
            // compiles. The policy lives on the struct config (and its alias). A float element can
            // never satisfy that `Ord` bound, so it is rejected gracefully (the set-side analog of
            // the float-key rejection above) instead of emitting a non-compiling crate.
            if let RustStructType::Array { element_type, .. } = rust_struct.variant()
                && rust_struct.config().duplicates
                    == Some(crate::comment_ast::DuplicatesPolicy::Reject)
            {
                element_type.visit_types(self, &mut |ty| mark_key_demand(ty, &mut key_demand, ord));
                if key_contains_float(&element_type.conceptual_type, self) {
                    float_key_rejections.insert(float_set_elem_msg(&rule_ident));
                }
            }
            // A SET NOMINAL wrapper (Phase 2.2/2.3) derives always-on encodings-ignored
            // `PartialEq/Eq/PartialOrd/Ord/Hash`, and its inner collection (`OrderedSet<Elem>` under
            // reject, `Vec<Elem>` under preserve) propagates every one of those bounds onto `Elem`.
            // So the element needs the FULL demand (`bare + hash + ord`), regardless of policy —
            // otherwise a Rust-struct element (`set<key_hash>`) fails to satisfy `Eq/Ord/Hash` and the
            // crate does not compile. Primitive/std elements carry the bounds intrinsically (no-op).
            if let RustStructType::Wrapper { wrapped, .. } = rust_struct.variant()
                && rust_struct.config().set_nominal
                && let ConceptualRustType::Array(element_type) = &wrapped.conceptual_type
            {
                element_type.visit_types(self, &mut |ty| {
                    mark_key_demand(ty, &mut key_demand, full_set_demand)
                });
                // The wrapper's always-on `Ord`/`Hash` derives (and, under reject, the uniqueness
                // door's `T: Ord`) flow onto the element regardless of policy, so a float element
                // can never compile — reject gracefully like the reject-array branch above.
                if key_contains_float(&element_type.conceptual_type, self) {
                    float_key_rejections.insert(float_set_elem_msg(&rule_ident));
                }
            }
            // An open struct-map's CAPTURED rest row (`{ 1: uint, * K => V }`) keys the very same
            // container a table rule does, but the IR stores its `K` FLAT (`RestKind::MapEntries`),
            // never as a `Map(k, v)` node — so neither `check_used_as_key` above nor the `Table`
            // branch below ever sees it, and without this branch a typed `K` reaches
            // `BTreeMap<K, V>`/`OrderedHashMap<K, V>` with no `Eq`/`Ord`/`Hash` derives (E0277 in the
            // generated crate, and — for a dep-owned `K` — no `borrowed_key_types.rs` row for the
            // dependency to satisfy either, since that file is built from this same map).
            //
            // Gated on the TYPED path: a bare `uint`/`text`/`any` domain keys nothing that could be
            // marked (`mark_key_demand` marks `Rust(ident)` nodes only), so gating keeps every
            // existing spec's derives byte-identical rather than relying on that coincidence.
            if let RustStructType::Record(record) = rust_struct.variant()
                && let Some(rest) = record.captured_rest()
                && !rest.is_array_tail()
                && !rest.map_key_uses_peeked_path(self)
            {
                // Same relaxation as the `Table` branch: a `@duplicates preserve` row's keys live in
                // a `PairMap`, compared by a linear `PartialEq` scan rather than hashed/ordered, so
                // the `ord` (Eq-containing) flavor suffices where the loose container needs `bare`.
                let key_flavor =
                    if rest.duplicates() == Some(crate::comment_ast::DuplicatesPolicy::Preserve) {
                        ord
                    } else {
                        bare
                    };
                rest.domain().visit_types(self, &mut |ty| {
                    mark_key_demand(ty, &mut key_demand, key_flavor)
                });
                // Walked directly (not as a `Map` node), so the float check is this branch's own —
                // and, running after generic resolution, it also catches a float behind a resolved
                // generic instance (`* gen<float64> => v`).
                if key_contains_float(&rest.domain().conceptual_type, self) {
                    float_key_rejections.insert(float_rest_key_msg(&rule_ident));
                }
            }
            if let RustStructType::Table { domain, .. } = rust_struct.variant() {
                // A `@duplicates preserve` table's key is compared with the pair-map's linear
                // `contains`/`find` scan (`K: PartialEq`), NOT hashed or ordered like a `BTreeMap`/
                // `OrderedHashMap` key — so it needs only the `ord` (Eq-containing) flavor, not the
                // full `bare` (`Hash + Eq + Ord`) bundle the loose table forces on its key. This is
                // the map-side of the reject-set `ord` relaxation above.
                let key_flavor = if rust_struct.config().duplicates
                    == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                {
                    ord
                } else {
                    bare
                };
                domain.visit_types(self, &mut |ty| {
                    mark_key_demand(ty, &mut key_demand, key_flavor)
                });
                // A top-level table rule's key is its `domain`, walked directly (not as a Map node),
                // so it needs its own check. This runs AFTER generic resolution, so it also catches
                // float keys hidden behind a resolved generic instance (`{ gen<float64> => uint }`),
                // the one seam that sees such instances. The marking above is left intact (harmless —
                // the crate never generates once we reject) so this is a pure add-on.
                if key_contains_float(&domain.conceptual_type, self) {
                    float_key_rejections.insert(float_key_msg(&rule_ident));
                }
            }
        }
        // we use a separate one here to get around the borrow checker in the above visit_types
        for (ident, demand) in key_demand {
            self.union_key_demand(ident, demand);
        }
        for msg in float_key_rejections {
            self.record_rejection(msg);
        }
        // NonEmptyVec wasm-wrapper name collisions: an inline `[+ elem]` mints a `NonEmpty<Elem>List`
        // wasm class; if a user rule already OWNS that identifier, silently sharing it would emit a
        // wrapper of the wrong shape (loose `Vec` vs restricted `NonEmptyVec`). Reject clearly rather
        // than shadow. Only relevant with wasm bindings (the collision is on the wasm class name).
        if cli.wasm {
            for msg in self.non_empty_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // NonEmptyMap wasm-wrapper name collisions — the map-side twin of the above.
            for msg in self.non_empty_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // `@duplicates reject` uniqueness-twin wasm-wrapper name collisions — the third container
            // kind's sibling of the two detectors above (the reject twin is the new container kind
            // AGENTS.md's twin-detector note reserved as the trigger for this expansion).
            for msg in self.reject_ordered_set_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // `@duplicates preserve` pair-map wrapper-name collisions — the fourth container kind's
            // siblings (loose `PairMapKToV`, restricted `NonEmptyPairMapKToV`). The flavored
            // structural names make the preserve-vs-default SHAPE collision unrepresentable, so what
            // is left is the same rule-ident-vs-wrapper-ident hazard the other kinds guard.
            for msg in self.preserve_pair_map_loose_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.preserve_pair_map_non_empty_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // `@extern_companions` names classes this crate must NOT define, so a same-crate RULE of
            // one of those names is a contradiction the deferral cannot resolve: the `use
            // <prefix>::<Class>;` and the rule's own class would claim one identifier (rustc E0255).
            // Sibling in spirit to the four wrapper-name detectors above — a rule ident contending
            // with a name the generator routes elsewhere — but its own function because the contested
            // name comes from the SPEC's declaration rather than a structural derivation, so it needs
            // neither shape reconstruction nor a per-container-kind twin.
            for msg in self.extern_companion_rule_name_collisions() {
                self.record_rejection(msg);
            }
            // `@used_as_elem` mints the loose-list wasm wrapper `<Elem>List` for each tagged
            // element. A directly-wasm-exposable element (e.g. a transparent `coin = uint` alias)
            // has NO such wrapper — the list lowers to a bare `Vec<..>` at the wasm boundary — so
            // the tag has nothing to mint. Reject gracefully here (mirroring the `--wrapper-requests`
            // exposable diagnostic) rather than silently no-op. Collected into a local set to
            // sidestep the borrow checker, like the float-key rejections above.
            let mut exposable_elem_rejections = BTreeSet::new();
            for ident in &self.used_as_elem {
                let element_type = self.used_as_elem_element_type(ident);
                if ConceptualRustType::Array(Box::new(element_type.conceptual_type.clone().into()))
                    .directly_wasm_exposable_ct(self)
                {
                    let member = element_type.name_as_wasm_array(self);
                    exposable_elem_rejections.insert(format!(
                        "@used_as_elem on `{ident}`: the loose list `[* {ident}]` is directly \
                         wasm-exposable — it lowers to `{member}` with no wrapper class, so there \
                         is no wrapper for this tag to mint. Remove `@used_as_elem` (the element \
                         already crosses the wasm boundary as a bare `{member}`)."
                    ));
                }
            }
            for msg in exposable_elem_rejections {
                self.record_rejection(msg);
            }
        }
        // The component face's own detector family, on exactly the terms the wasm block above
        // states: a name that is legal on the rust and wasm faces can be broken on the WIT one, so
        // the check is flag-gated on the face that has the restriction.
        //
        // Placed HERE — after every `register_rust_struct` in this fn (they all run in the generic
        // resolution at the top) — because the detector walks `rust_structs` and `scopes`, which are
        // complete from that point on.
        //
        // Its SIBLING — the strong-uniqueness name-collision detector — deliberately does NOT run
        // here: its verdict depends on which types the rust face gives a `Deserialize` impl (a
        // `from-cbor-bytes` static the tool never emits cannot collide with anything), which only
        // that face's own walk reaches. It runs in `GenerationScope::generate` instead and surfaces
        // through the graceful error channel `generated_files`/`export` already carry. A spec with
        // BOTH a cycle and a collision therefore reports the cycle first, which is correct: a cyclic
        // package has no resolvable WIT to have collisions in.
        if cli.component {
            // WIT requires interfaces linked with `use` to be acyclic, and each exported module
            // scope becomes one interface. Cyclic cross-scope references generate fine on the rust
            // face, so this restriction arrives with `--component` and nowhere else.
            for msg in crate::generation::wit::wit_scope_cycles(self, cli) {
                self.record_rejection(msg);
            }
        }
        // `@no_json_schema_export` suppresses a rule's schema-registration row. A rule that registers
        // NO `RustStruct` at all — a transparent alias (`x = uint`), a `@no_alias` alias, a named
        // binding to a set nominal, a generic DEFINITION (only its instantiations are types), a
        // plain group no rule splices — has no row for the directive to
        // suppress, so it would be silently dead: reject it in the house style of the other
        // directive-misplacement rejections. Deliberately NOT rejected on a rule that registers a
        // struct the row loop skips for other reasons (an `Array`/`Table` typedef, a generic-extern
        // base): those are redundant-but-honest annotations, and keeping the rule "valid wherever a
        // rust type is produced" keeps it simple and flag-independent. Deferred to here rather than
        // the parse walk because a generic INSTANCE (`my_foo = foo<uint>`) only registers its struct
        // during the generic resolution above. Flag-independent (outside the `cli.wasm` block above):
        // the directive means the same thing under every flag set. Determinism: `BTreeSet` iteration.
        let struct_less_no_json_schema_export = self
            .no_json_schema_export
            .iter()
            .filter(|ident| !self.rust_structs.contains_key(ident))
            .cloned()
            .collect::<Vec<_>>();
        for ident in struct_less_no_json_schema_export {
            self.record_rejection(format!(
                "@no_json_schema_export on `{ident}`: this rule registers no rust struct, so there \
                 is no schema-registration row to suppress and the directive would silently do \
                 nothing. Either it is a transparent alias (a plain type alias `{ident} = uint`, a \
                 `@no_alias` alias, or a named binding to a generic instantiation), or it is a \
                 generic DEFINITION whose instantiations own the types (annotate the instance — \
                 `inst = {ident}<uint> ; @no_json_schema_export` — not the definition), or it is a \
                 plain group no rule splices. Remove it from this rule, or move it to the rule that \
                 actually produces the type."
            ));
        }
        // The `@custom_serialize`/`@custom_deserialize` pair is a TYPE-level override: it replaces
        // the codec of the rust type a rule resolves to. The parse-walk rejections cover the
        // placements that DELETE or BYPASS the node it keys on (`@no_alias`, `@newtype`, an extern /
        // raw-bytes marker, a row-entry slot). The two below are the placements that MINT a struct
        // whose generated impls the pair does not displace, so it half-applies:
        //
        //   - an ENUM rule (type choice, group choice, or the fixed-value C-style enum): its
        //     serialize side is generated unconditionally while `generate_deserialize`'s
        //     `Root(Rust(ident))` arm rewrites every embed site to the named reader — the same
        //     read-one-format/write-another asymmetry `@newtype` is rejected for.
        //   - a RECORD rule carrying only ONE half. Serialize-only emits no `Serialize` impl and
        //     never calls the named function (an undiagnosed non-compiling crate);
        //     deserialize-only keeps the type's own generated `Deserialize` impl while rewriting
        //     every embed site, so one type decodes the same bytes two ways — and the rule projects
        //     OPAQUELY across the extern-interface seam, carrying the divergence to consumers.
        //     BOTH halves on a record rule is deliberately NOT rejected: it suppresses the generated
        //     impls for the author to hand-own, which is unspecified-and-at-risk rather than wrong
        //     (see `docs/docs/comment_dsl.mdx`).
        //
        // Deferred to here rather than the parse walk for the same reason `@no_json_schema_export`
        // above is: the struct KIND decides, and a generic instance only materializes its struct
        // during the resolution above. Collected into a `BTreeSet` (determinism + no duplicate line
        // if two registrations ever land on one ident), like the float-key rejections.
        let mut custom_codec_rejections = BTreeSet::new();
        for (ident, rust_struct) in &self.rust_structs {
            let config = rust_struct.config();
            let enum_shape = match rust_struct.variant() {
                RustStructType::TypeChoice { .. } => Some("a type-choice rule (`a / b`)"),
                RustStructType::GroupChoice { .. } => {
                    Some("a group-choice rule (`{ … } // { … }`)")
                }
                RustStructType::CStyleEnum { .. } => {
                    Some("a fixed-value type-choice rule (`0 / 1`, a C-style enum)")
                }
                _ => None,
            };
            if let Some(shape) = enum_shape {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: {shape} mints an enum whose serialize side is \
                             generated unconditionally, while the deserialize CALL SITES do route \
                             through the custom reader — so the pair would make the enum read one \
                             wire format and write another. Put the pair on the rule of the variant \
                             type that needs the custom format, or declare `{ident}` as a \
                             {EXTERN_MARKER} rule and hand-write the type in full."
                        ));
                    }
                }
            }
            if matches!(rust_struct.variant(), RustStructType::Record(_)) {
                if config.custom_serialize.is_some() && config.custom_deserialize.is_none() {
                    custom_codec_rejections.insert(format!(
                        "@custom_serialize alone on `{ident}`: a record rule with only the serialize \
                         half emits no `Serialize` impl for the type and never calls the named \
                         function, so the generated crate does not compile — every site holding a \
                         `{ident}` calls `.serialize(..)` on a type that has no impl. Move the pair \
                         to the field (or to the type rule of the member) that needs the custom \
                         format, or declare `{ident}` as a {EXTERN_MARKER} rule and hand-write the \
                         type in full."
                    ));
                }
                if config.custom_deserialize.is_some() && config.custom_serialize.is_none() {
                    custom_codec_rejections.insert(format!(
                        "@custom_deserialize alone on `{ident}`: a record rule with only the \
                         deserialize half still emits the type's own generated `Deserialize` impl, \
                         while every site holding a `{ident}` is rewritten to call the named function \
                         — so `{ident}::from_cbor_bytes` and a field of type `{ident}` decode the \
                         same bytes differently. The rule also projects OPAQUELY across the \
                         extern-interface seam, so a consumer decodes it the generated way. Move the \
                         pair to the field (or to the type rule of the member) that needs the custom \
                         format, or declare `{ident}` as a {EXTERN_MARKER} rule and hand-write the \
                         type in full."
                    ));
                }
            }
        }
        for msg in custom_codec_rejections {
            self.record_rejection(msg);
        }
        // Surface any rejection recorded DURING finalize (e.g. the float-key check above, which can
        // only run post-generic-resolution). Without this the entry-point check at the top of
        // finalize would silently swallow anything recorded here.
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        Ok(())
    }

    /// Detect wasm-class name conflicts the `[+ elem]` (NonEmptyVec) emission would otherwise turn
    /// into a non-compiling wasm crate — every leg rejects gracefully rather than silently shadow
    /// or emit malformed code. Scans EVERY RustType position (incl. named-array element types and
    /// alias base types) via `visit_all_rust_types`. Four conflict classes:
    ///
    /// 1. An inline `[+ elem]` with no named owner (see `non_empty_named_owner` — when an owner
    ///    exists the inline use dedups to the named rule and mints nothing) mints a synthesized
    ///    `NonEmpty<Elem>List` class: a user rule claiming that ident collides.
    /// 2. A restricted wrapper whose element is non-exposable needs the LOOSE `<Elem>List` builder
    ///    as its `try_from` source (synthesized mints and free-named `[+ elem]` rules alike): a
    ///    user rule claiming that ident with any shape OTHER than a same-element loose Array rule
    ///    (which IS the builder, shared) collides.
    /// 3. A self-named rule (`bar_list = [+ bar]` — the rule ident IS the element's loose-builder
    ///    name) legitimately claims the name for its RESTRICTED wrapper (it emits with no
    ///    `try_from`; construction is `new(first)` + `add`), but then no OTHER use may need the
    ///    loose `<Elem>List` builder: a plain non-exposable `[* elem]` mint, a map-key list
    ///    wrapper of the same element, or an open struct's rest row (a `* K => V` row's `keys()`
    ///    wrapper, a `* T` tail's own getter) would reference a class of the wrong shape.
    /// 4. A DIRECT claim: any of those plain uses MINTS the loose `<Elem>List` on its own, with no
    ///    `[+ …]` shape anywhere, and a user rule of the same ident and an incompatible shape
    ///    shadows it. Classes 2 and 3 both arrive through a `[+ …]` wrapper, so this is the leg a
    ///    spec containing no `[+ …]` at all reaches — and the table-keys member of it is the one
    ///    claim in this family that is otherwise SILENT rather than a compile error
    ///    (`create_and_register_array_type` overwrites the authored rule; see
    ///    `swallowed_structural_list_claims`, whose record is that overwrite's only evidence).
    fn non_empty_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty shape + every loose-builder need from PLAIN array shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose <Elem>List classes needed by plain (non-`+`) uses: name -> (a use description, the
        // ELEMENT the class wraps). The element rides along so the direct-claim leg below can ask
        // `provides_compatible_loose_list` whether a rule of that ident IS the builder.
        let mut plain_loose_needs: BTreeMap<String, (String, RustType)> = BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_array() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Array(elem) = &rt.conceptual_type
                && !rt.directly_wasm_exposable(self)
            {
                plain_loose_needs.insert(
                    elem.name_as_wasm_array(self),
                    (
                        "a plain (`*`-occurrence) array use".to_owned(),
                        (**elem).clone(),
                    ),
                );
            }
            if let ConceptualRustType::Map(k, _v) = &rt.conceptual_type {
                // table wrappers mint a keys() list wrapper over the KEY type
                if !ConceptualRustType::Array(Box::new((**k).clone()))
                    .directly_wasm_exposable_ct(self)
                {
                    plain_loose_needs.insert(
                        k.name_as_wasm_array(self),
                        ("a map keys() wrapper".to_owned(), (**k).clone()),
                    );
                }
            }
        });
        // named tables' keys() wrappers (Table structs aren't visited as Map RustTypes)
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Table { domain, .. } => {
                    if !ConceptualRustType::Array(Box::new(domain.clone()))
                        .directly_wasm_exposable_ct(self)
                    {
                        plain_loose_needs.insert(
                            domain.name_as_wasm_array(self),
                            ("a table keys() wrapper".to_owned(), domain.clone()),
                        );
                    }
                }
                // An open struct's rest row names a list wrapper the same way a field of the row's
                // CONTAINER type would, and the IR stores the row's inner types flat, so neither the
                // `visit_all_rust_types` walk above nor the Table arm sees the claim: a `* K => V`
                // row's wasm class needs the loose `<K>List` for its `keys()`, and a `* T` tail's
                // getter needs the loose `<T>List` itself. Only a CAPTURED row mints anything — an
                // `@ignore` row has no field and no getter.
                RustStructType::Record(record) => {
                    let Some(rest) = record.captured_rest() else {
                        continue;
                    };
                    if rest.is_array_tail() {
                        if !rest.container_type().directly_wasm_exposable(self) {
                            plain_loose_needs.insert(
                                rest.element().name_as_wasm_array(self),
                                (
                                    "an open array `* …` rest tail".to_owned(),
                                    rest.element().clone(),
                                ),
                            );
                        }
                    } else if !ConceptualRustType::Array(Box::new(rest.domain().clone()))
                        .directly_wasm_exposable_ct(self)
                    {
                        plain_loose_needs.insert(
                            rest.domain().name_as_wasm_array(self),
                            (
                                "an open struct-map rest row's keys() wrapper".to_owned(),
                                rest.domain().clone(),
                            ),
                        );
                    }
                }
                _ => {}
            }
        }

        // shared leg: the loose-builder need of a restricted wrapper (synthesized or free-named)
        let check_loose_need = |element: &RustType,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            if element.directly_wasm_exposable(self) || element.is_non_empty_array() {
                return; // exposable: try_from takes a bare Vec; nested: no loose source at all
            }
            let loose = element.name_as_wasm_array(self);
            if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_list(&loose, &element.clone().resolve_aliases())
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' list \
                     builder needs as the `try_from` source of {needed_by} — rename the rule (or \
                     make it `[* …]` of the same element, which IS that builder)"
                ));
            }
        };

        // (1) + (2) for inline `[+ elem]` shapes that actually mint a synthesized class
        for rt in &inline_non_empty {
            let ConceptualRustType::Array(elem) = &rt.conceptual_type else {
                unreachable!("is_non_empty_array implies an Array conceptual type");
            };
            if self.non_empty_named_owner(elem).is_some() {
                continue; // dedups to the named rule's class — nothing synthesized, no conflict
            }
            let restricted = rt.non_empty_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline `[+ …]` occurrence — rename the rule to \
                     avoid shadowing the restricted NonEmptyVec wrapper"
                ));
            }
            check_loose_need(
                elem,
                &format!("the inline `[+ …]` wrapper '{restricted}'"),
                &mut msgs,
            );
        }

        // (2) + (3) for named `[+ elem]` rules
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Array {
                element_type,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if *bounds != Some((Some(1), None)) {
                continue;
            }
            if element_type.directly_wasm_exposable(self) || element_type.is_non_empty_array() {
                continue;
            }
            let loose = element_type.name_as_wasm_array(self);
            if loose == ident.to_string() {
                // self-named rule: it owns the ident as its RESTRICTED class (no try_from); any
                // OTHER use needing the loose builder of this element now has no class to name
                if let Some((need, _elem)) = plain_loose_needs.get(&loose) {
                    msgs.insert(format!(
                        "name collision: rule '{ident}' (`[+ …]`) claims the ident that {need} of \
                         the same element needs for its loose '{loose}' list wrapper — rename the \
                         rule so the loose builder class can exist"
                    ));
                }
            } else {
                check_loose_need(
                    element_type,
                    &format!("the named `[+ …]` rule '{ident}'"),
                    &mut msgs,
                );
            }
        }

        // (4) DIRECT claims. Every leg above reaches `plain_loose_needs` through a `[+ …]` shape —
        // as a try_from source or as a self-named rule — so a spec with no `[+ …]` anywhere never
        // consults it, and the plain mints it collects go unchecked. That gap is not merely
        // undiagnosed: a named table's keys-list is minted into `rust_structs` by
        // `create_and_register_array_type`, whose last-wins `register_rust_struct` REPLACES a user
        // rule of the same ident outright — the rule vanishes from the crate, a field of its type
        // silently becomes an array of the key element, and generation exits 0. The other plain
        // mints (rest rows, rest tails, inline `[* …]` uses) reach the generic duplicate-ident
        // backstop instead, which is loud but reports the ident rather than the claim. One leg over
        // the collected needs covers all of them in this family's voice, exactly as the map side's
        // rest-row leg does for `MapKToV`.
        //
        // A rule that IS `[* elem]` of the same element is NOT a collision: it is that very builder,
        // and the re-mint is byte-identical (deliberate aliasing — the idiom
        // `create_and_register_array_type` anticipates). A SELF-NAMED `[+ elem]` rule is skipped
        // too: leg (3) above owns it, with a message that names the `[+ …]` rule as the claimant.
        //
        // A SWALLOWED claim (`swallowed_structural_list_claims`) is reported here too, and is the
        // only member of this leg that cannot be re-derived from the finalized IR: the mint already
        // replaced the authored rule, so `provides_compatible_loose_list` now answers about the
        // BUILDER and says "compatible". The recorded claim is the sole surviving evidence.
        for (loose, element) in &self.swallowed_structural_list_claims {
            plain_loose_needs
                .entry(loose.clone())
                .or_insert_with(|| ("a table keys() wrapper".to_owned(), element.clone()));
        }
        for (loose, (need, element)) in &plain_loose_needs {
            if self.wasm_ident_claimed_by_user_rule(loose)
                && (self.swallowed_structural_list_claims.contains_key(loose)
                    || !self
                        .provides_compatible_loose_list(loose, &element.clone().resolve_aliases()))
                && !self.claims_ident_as_self_named_non_empty_list(loose)
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' collides with the '{loose}' wasm wrapper \
                     generated for {need} of the same element — rename the rule to avoid shadowing \
                     the loose list wrapper (or make it a `[* …]` of the same element, which IS \
                     that wrapper)"
                ));
            }
        }

        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `{+ k => v}` (NonEmptyMap) emission would otherwise turn
    /// into a non-compiling wasm crate — the map-side twin of `non_empty_wrapper_name_collisions`.
    /// The loose table builder is always `MapKToV` (`name_for_wasm_map`); a map is never directly
    /// exposable, so (unlike arrays) the loose builder is ALWAYS the `try_from` source. Five classes:
    ///
    /// 1. An inline `{+ k => v}` with no named owner (see `non_empty_map_named_owner`) mints a
    ///    synthesized `NonEmptyMapKToV` class: a user rule claiming that ident collides.
    /// 2. A restricted wrapper (inline-synth or named non-self-named) needs the loose `MapKToV`
    ///    builder as its `try_from` source: a user rule claiming that ident with any shape OTHER than
    ///    a same-shape plain `{* k => v}` table rule (which IS the builder, shared) collides.
    /// 3. A self-named rule (`map_k_to_v = {+ k => v}` — the rule ident IS the loose-builder name)
    ///    legitimately claims the name for its RESTRICTED wrapper (it emits with no `try_from`;
    ///    construction is `new(first_key, first_value)` + `insert`), but then no OTHER use may need
    ///    the loose `MapKToV` builder: a plain `{* k => v}` use, an anonymous same-shape map, or an
    ///    open struct-map rest row of the same key/value would reference a class of the wrong shape.
    /// 4. A DEFAULT-flavored open struct-map REST ROW mints the loose `MapKToV` its wasm getter
    ///    returns: a user rule claiming that ident with any shape other than the shared plain
    ///    `{* k => v}` table collides. This is the default-flavor twin of the Record leg in
    ///    `preserve_pair_map_loose_wrapper_name_collisions`.
    /// 5. A DIRECT claim on the loose `MapKToV` a plain `{* k => v}` USE or TABLE RULE mints — the
    ///    symmetric sibling of the list side's class 4. Rest rows are class 4 and preserve-flavored
    ///    shapes are `preserve_pair_map_loose_wrapper_name_collisions`, so this leg's source set is
    ///    restricted to keep one collision reported once, in one kind's voice.
    fn non_empty_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty map shape + every loose-builder need from PLAIN map shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose MapKToV classes needed by plain (non-`+`) uses: name -> a use description
        let mut plain_loose_needs: BTreeMap<String, String> = BTreeMap::new();
        // The subset of those needs the DIRECT-claim leg owns: name -> (use description, key,
        // value). DEFAULT-flavored plain map uses and plain table rules only — a rest row is leg (4)
        // and a `@duplicates preserve` shape is
        // `preserve_pair_map_loose_wrapper_name_collisions`, so restricting the set here is what
        // keeps one collision to one message in one kind's voice.
        let mut direct_claim_needs: BTreeMap<String, (String, RustType, RustType)> =
            BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_map() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Map(k, v) = &rt.conceptual_type {
                let preserve = rt.is_preserve_pair_map();
                let name = ConceptualRustType::name_for_wasm_map(k, v, preserve).to_string();
                plain_loose_needs
                    .insert(name.clone(), "a plain (`*`-occurrence) map use".to_owned());
                if !preserve {
                    direct_claim_needs.insert(
                        name,
                        (
                            "a plain (`*`-occurrence) map use".to_owned(),
                            (**k).clone(),
                            (**v).clone(),
                        ),
                    );
                }
            }
        });
        // named plain tables mint their loose `MapKToV` class too (Table structs aren't visited as
        // Map RustTypes); exclude non-empty tables (their class is the restricted wrapper)
        for rs in self.rust_structs.values() {
            match rs.variant() {
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    if *bounds != Some((Some(1), None)) {
                        let preserve = rs.config().duplicates
                            == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
                        let name = ConceptualRustType::name_for_wasm_map(domain, range, preserve)
                            .to_string();
                        plain_loose_needs.insert(
                            name.clone(),
                            "a plain (`*`-occurrence) table rule".to_owned(),
                        );
                        if !preserve {
                            direct_claim_needs.insert(
                                name,
                                (
                                    "a plain (`*`-occurrence) table rule".to_owned(),
                                    domain.clone(),
                                    range.clone(),
                                ),
                            );
                        }
                    }
                }
                // An open struct-map rest row mints the loose builder its wasm getter returns, in
                // the flavor the row carries — invisible to the walk above because the IR stores the
                // row's key/value flat. Read the name off `RestRow::container_type`, the one
                // container spelling the emitter and the scope walk also use, so a row's claim
                // cannot drift from the class it actually mints.
                RustStructType::Record(record) => {
                    let Some(rest) = record.captured_rest().filter(|r| !r.is_array_tail()) else {
                        continue;
                    };
                    let container = rest.container_type();
                    let ConceptualRustType::Map(k, v) = &container.conceptual_type else {
                        unreachable!("a map rest row's container is a Map");
                    };
                    plain_loose_needs.insert(
                        ConceptualRustType::name_for_wasm_map(
                            k,
                            v,
                            container.is_preserve_pair_map(),
                        )
                        .to_string(),
                        "an open struct-map rest row".to_owned(),
                    );
                }
                _ => {}
            }
        }

        // shared leg: the loose-builder need of a restricted map wrapper (synthesized or named)
        let check_loose_need = |key: &RustType,
                                value: &RustType,
                                preserve: bool,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            let loose = ConceptualRustType::name_for_wasm_map(key, value, preserve).to_string();
            if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_table(
                    &loose,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    preserve,
                )
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' table \
                     builder needs as the `try_from` source of {needed_by} — rename the rule (or \
                     make it `{{* …}}` of the same key/value, which IS that builder)"
                ));
            }
        };

        // (1) + (2) for inline `{+ k => v}` shapes that actually mint a synthesized class
        for rt in &inline_non_empty {
            let ConceptualRustType::Map(k, v) = &rt.conceptual_type else {
                unreachable!("is_non_empty_map implies a Map conceptual type");
            };
            if self.non_empty_map_named_owner(k, v).is_some() {
                continue; // dedups to the named rule's class — nothing synthesized, no conflict
            }
            let restricted = rt.non_empty_wasm_map_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline `{{+ …}}` map occurrence — rename the rule to \
                     avoid shadowing the restricted NonEmptyMap wrapper"
                ));
            }
            check_loose_need(
                k,
                v,
                rt.is_preserve_pair_map(),
                &format!("the inline `{{+ …}}` wrapper '{restricted}'"),
                &mut msgs,
            );
        }

        // (2) + (3) for named `{+ k => v}` table rules
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if *bounds != Some((Some(1), None)) {
                continue;
            }
            let preserve =
                rs.config().duplicates == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
            let loose = ConceptualRustType::name_for_wasm_map(domain, range, preserve).to_string();
            if loose == ident.to_string() {
                // self-named rule: it owns the ident as its RESTRICTED class (no try_from); any
                // OTHER use needing the loose builder of this shape now has no class to name
                if let Some(need) = plain_loose_needs.get(&loose) {
                    msgs.insert(format!(
                        "name collision: rule '{ident}' (`{{+ …}}`) claims the ident that {need} of \
                         the same key/value needs for its loose '{loose}' table wrapper — rename \
                         the rule so the loose builder class can exist"
                    ));
                }
            } else {
                check_loose_need(
                    domain,
                    range,
                    preserve,
                    &format!("the named `{{+ …}}` rule '{ident}'"),
                    &mut msgs,
                );
            }
        }

        // (4) a DEFAULT-flavored open struct-map rest row MINTS the loose `MapKToV` class its wasm
        // getter returns, so a user rule spelling that ident shadows it. The `@duplicates preserve`
        // twin of this leg lives in `preserve_pair_map_loose_wrapper_name_collisions` (the flavor is
        // part of the structural name, so the two rows can never contend for one class) — per-kind
        // siblings with deliberately distinct texts, so a failing spec points at the right flavor.
        // A rule that IS a plain `{* k => v}` table of the same key/value is not a collision: it
        // solely owns the shape and the row's getter returns it through its `pub type` alias.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record.captured_rest().filter(|r| {
                !r.is_array_tail()
                    && r.duplicates() != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
            }) else {
                continue;
            };
            let structural =
                ConceptualRustType::name_for_wasm_map(rest.domain(), rest.range(), false)
                    .to_string();
            if self.wasm_ident_claimed_by_user_rule(&structural)
                && !self.provides_compatible_loose_table(
                    &structural,
                    &rest.domain().clone().resolve_aliases(),
                    &rest.range().clone().resolve_aliases(),
                    false,
                )
            {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for the open struct-map rest row of '{ident}' — rename the \
                     rule to avoid shadowing the loose map wrapper (or make it a `{{* …}}` table of \
                     the same key/value, which IS that wrapper)"
                ));
            }
        }

        // (5) DIRECT claims by a plain `{* k => v}` use or table rule — the symmetric sibling of the
        // list side's direct-claim leg. Without it these reach only the generic duplicate-ident
        // backstop, which is loud but reports the ident rather than the claim; the shape here is the
        // one leg (4) already uses for a rest row, with the plain use named instead of the row.
        for (loose, (need, key, value)) in &direct_claim_needs {
            if self.wasm_ident_claimed_by_user_rule(loose)
                && !self.provides_compatible_loose_table(
                    loose,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    false,
                )
                && !self.claims_ident_as_self_named_non_empty_table(loose)
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' collides with the '{loose}' wasm wrapper \
                     generated for {need} of the same key/value — rename the rule to avoid \
                     shadowing the loose map wrapper (or make it a `{{* …}}` table of the same \
                     key/value, which IS that wrapper)"
                ));
            }
        }

        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `@duplicates reject` uniqueness-twin emission would turn
    /// into a non-compiling wasm crate — the third container kind's sibling of
    /// `non_empty_wrapper_name_collisions` / `non_empty_map_wrapper_name_collisions`. An INLINE
    /// (anonymous generic-instance) reject set mints a synthesized `<Elem>OrderedSet` /
    /// `NonEmpty<Elem>OrderedSet` wasm class (`reject_ordered_set_wasm_wrapper_name`); a user rule
    /// claiming that ident would silently collide (a plain `pub struct`/`pub type` of the wrong shape).
    /// NAMED reject rules mint under their own rule ident (never a synthesized structural name), so
    /// they are not a source here — only anonymous instances are. The message text is deliberately
    /// distinct from the two NonEmpty siblings' (it names the reject twin, not the NonEmptyVec/Map
    /// wrapper) so a failing spec points at the right container kind.
    fn reject_ordered_set_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Array {
                element_type,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            // Only INLINE (anonymous instance) reject sets synthesize a structural class; a named
            // reject rule owns its rule ident and mints there.
            if rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Reject)
                || !self.is_anonymous_collection_instance(ident)
            {
                continue;
            }
            let variant = element_type.conceptual_type.for_variant();
            let structural = if *bounds == Some((Some(1), None)) {
                format!("NonEmpty{variant}OrderedSet")
            } else {
                format!("{variant}OrderedSet")
            };
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an inline `@duplicates reject` set occurrence — rename \
                     the rule to avoid shadowing the restricted OrderedSet wrapper"
                ));
            }
        }
        // A SET NOMINAL wrapper (Phase 2.2 named rule, Phase 2.3 generic instantiation) whose inner is
        // the reject uniqueness twin mints the SAME structural `<Elem>OrderedSet` /
        // `NonEmpty<Elem>OrderedSet` wasm class for its `new()`/`get()` boundary. A user rule claiming
        // that ident silently collides (a `pub struct`/`pub type` of the wrong shape) — the nominal
        // sibling of the inline occurrence above. Message names the reject twin (the "OrderedSet
        // wrapper" pinned substring), distinct from the NonEmpty siblings.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Wrapper { wrapped, .. } = rs.variant() else {
                continue;
            };
            if !rs.config().set_nominal || !wrapped.duplicates_reject() {
                continue;
            }
            let ConceptualRustType::Array(element_type) = &wrapped.conceptual_type else {
                continue;
            };
            let variant = element_type.conceptual_type.for_variant();
            let structural = if wrapped.is_non_empty_array() {
                format!("NonEmpty{variant}OrderedSet")
            } else {
                format!("{variant}OrderedSet")
            };
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for the nominal `@duplicates reject` set `{ident}`'s \
                     boundary — rename the rule to avoid shadowing the restricted OrderedSet wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the LOOSE `@duplicates preserve` pair-map wrapper
    /// (`PairMapKToV`) would otherwise turn into a non-compiling wasm crate — the pair-map sibling of
    /// `non_empty_wrapper_name_collisions` / `non_empty_map_wrapper_name_collisions` /
    /// `reject_ordered_set_wrapper_name_collisions`. The container flavor is part of the structural
    /// name, so a preserve map and a default map of the identical key/value derive DIFFERENT classes
    /// and can never be asked to be one shape; what remains is the same hazard every other kind has —
    /// a user rule whose ident happens to spell the synthesized class name.
    ///
    /// Sources that mint or alias the loose `PairMapKToV` class: a preserve open-map REST ROW (its
    /// capture field's wasm getter returns the structural class), an ANONYMOUS preserve table instance
    /// (`ptbl<uint, tstr>`, which routes through the structural wrapper via its passthrough alias), a
    /// NAMED preserve `{* …}` rule that solely owns the shape (its `pub type PairMapKToV = <Owner>;`
    /// alias claims the ident beside the class), and a named preserve `{+ …}` rule whose `try_from`
    /// source is the loose pair-map builder. A user rule that IS a plain preserve `{* k => v}` table of
    /// the same key/value is not a collision — that rule IS the builder (shared, exactly as the
    /// default-flavored sibling shares it). Message text is deliberately distinct from the other kinds'
    /// (it names the pair-map twin) so a failing spec points at the right container kind.
    fn preserve_pair_map_loose_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        let check = |structural: String,
                     key: &RustType,
                     value: &RustType,
                     minted_by: String,
                     msgs: &mut BTreeSet<String>| {
            if self.wasm_ident_claimed_by_user_rule(&structural)
                && !self.provides_compatible_loose_table(
                    &structural,
                    &key.clone().resolve_aliases(),
                    &value.clone().resolve_aliases(),
                    true,
                )
            {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for {minted_by} — rename the rule to avoid shadowing the \
                     loose `@duplicates preserve` PairMap wrapper (or make it a `{{* …}}` \
                     `@duplicates preserve` table of the same key/value, which IS that wrapper)"
                ));
            }
        };
        for (ident, rs) in self.rust_structs.iter() {
            match rs.variant() {
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    if rs.config().duplicates
                        != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    {
                        continue;
                    }
                    let structural =
                        ConceptualRustType::name_for_wasm_map(domain, range, true).to_string();
                    // A self-named rule legitimately owns the ident for its own class.
                    if structural == ident.to_string() {
                        continue;
                    }
                    let minted_by = if *bounds == Some((Some(1), None)) {
                        format!(
                            "the `@duplicates preserve` `{{+ …}}` rule '{ident}'s `try_from` source"
                        )
                    } else {
                        format!("the `@duplicates preserve` table rule '{ident}'")
                    };
                    check(structural, domain, range, minted_by, &mut msgs);
                }
                RustStructType::Record(record) => {
                    // CAPTURED rows only: an `@ignore` row has no field and no getter, so it mints
                    // no wrapper and can claim no ident (same gate as the mint itself).
                    let Some(rest) = record.captured_rest().filter(|r| {
                        !r.is_array_tail()
                            && r.duplicates()
                                == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    }) else {
                        continue;
                    };
                    let structural =
                        ConceptualRustType::name_for_wasm_map(rest.domain(), rest.range(), true)
                            .to_string();
                    check(
                        structural,
                        rest.domain(),
                        rest.range(),
                        format!("the `@duplicates preserve` rest row of '{ident}'"),
                        &mut msgs,
                    );
                }
                _ => {}
            }
        }
        msgs.into_iter().collect()
    }

    /// The min-1 sibling of `preserve_pair_map_loose_wrapper_name_collisions`: an ANONYMOUS
    /// `@duplicates preserve` `{+ k => v}` table instance (a generic instantiation like
    /// `pnetbl<uint, tstr>`) mints the synthesized `NonEmptyPairMapKToV` class and routes to it through
    /// its passthrough alias, so a user rule claiming that ident silently shadows it. Named preserve
    /// `{+ …}` rules mint under their own rule ident and are not a source here — the same
    /// anonymous-instance-only scope as `reject_ordered_set_wrapper_name_collisions`' first leg. INLINE
    /// `{+ …}` occurrences carry no directive, so the default-flavored twin
    /// (`non_empty_map_wrapper_name_collisions`) covers them; that detector's naming is flavor-aware
    /// too, so if an inline preserve occurrence ever becomes expressible it is already checked there.
    fn preserve_pair_map_non_empty_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rs.variant()
            else {
                continue;
            };
            if rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                || *bounds != Some((Some(1), None))
                || !self.is_anonymous_collection_instance(ident)
            {
                continue;
            }
            let structural = format!(
                "NonEmpty{}",
                ConceptualRustType::name_for_wasm_map(domain, range, true)
            );
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an anonymous `@duplicates preserve` `{{+ …}}` table \
                     instance — rename the rule to avoid shadowing the restricted NonEmptyPairMap \
                     wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// `@extern_companions` names classes that live in a SIBLING crate, so this crate must not also
    /// define one. A top-level rule of this crate's own spec whose ident equals a listed class is
    /// that contradiction: the deferral emits `use <prefix>::<Class>;` while the rule mints a class
    /// of the same name, which is rustc E0255 in the generated wasm crate — a failure whose message
    /// names neither the directive nor the rule. Reported here instead, in the spec's own terms.
    ///
    /// The claim test is "is a top-level rule of an EXPORTED scope" (`scopes`, populated once per
    /// parsed rule) rather than the `rust_structs`/`type_aliases` membership the structural detectors
    /// use: those registries also hold generator-SYNTHESIZED wrappers, and a synthesized wrapper of a
    /// listed name is precisely what the directive suppresses — reading it as a claim would reject
    /// every correct use. A same-named rule in a dependency scope is a different crate's business.
    fn extern_companion_rule_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        for (owner, companions) in &self.extern_companions {
            for class in &companions.classes {
                let ident = RustIdent::new(CDDLIdent::new(class.clone()));
                if !self.is_toplevel_rule(&ident) || !self.scope(&ident).export() {
                    continue;
                }
                let claimant = self.source_rule_name(&ident).unwrap_or(class);
                let prefix = &companions.path_prefix;
                msgs.insert(format!(
                    "@extern_companions on `{owner}` declares that `{class}` already exists in \
                     `{prefix}`, but this crate's own rule `{claimant}` also defines `{class}` — the \
                     generated wasm crate would both `use {prefix}::{class};` and define it (E0255). \
                     Either drop `{class}` from the directive's class list (this crate owns it, and \
                     the sibling's class is a DIFFERENT type across the package boundary), or rename \
                     the rule."
                ));
            }
        }
        msgs.into_iter().collect()
    }

    pub fn visit_types<F: FnMut(&ConceptualRustType)>(&self, f: &mut F) {
        for rust_struct in self.rust_structs().values() {
            rust_struct.visit_types(self, f);
        }
        // Emitted type aliases (`x = int`, `x = bytes .cbor int`, `x = bytes .cbor { * tstr => int }`)
        // are `pub type` definitions whose base type never surfaces through any rust struct, so the
        // rust-struct walk above cannot see the built-in `Int` extern they reference — leaving a
        // dangling `Int` name (its `generate_int` emission is gated on `is_referenced`, whose only
        // walk is this one). Walk each emitted alias base type through the same conceptual visitor the
        // rust structs use, so references reachable only from an alias base (bare, `.cbor`-wrapped, or
        // a Map value) still register. A `@no_alias` rule (neither `gen_rust_alias` nor `gen_wasm_alias`)
        // is substituted transparently at its use sites, so its base type surfaces where it is actually
        // used — walking it from the alias table too would be redundant, not wrong. Reserved built-in
        // aliases (`AliasIdent::Reserved`) are filtered out; determinism holds — `type_aliases` is a
        // `BTreeMap`.
        for (alias_ident, alias_info) in self.type_aliases() {
            if matches!(alias_ident, AliasIdent::Rust(_))
                && (alias_info.gen_rust_alias || alias_info.gen_wasm_alias)
            {
                alias_info.base_type.conceptual_type.visit_types(self, f);
            }
        }
    }

    pub fn is_referenced(&self, ident: &RustIdent) -> bool {
        let mut found = false;
        self.visit_types(&mut |ty| {
            if let ConceptualRustType::Rust(id) = ty
                && id == ident
            {
                found = true
            }
        });
        found
    }

    // see self.plain_groups comments
    pub fn mark_plain_group(&mut self, ident: RustIdent, group_info: PlainGroupInfo<'a>) {
        self.plain_groups.insert(ident, group_info);
    }

    // see self.plain_groups comments
    pub fn set_rep_if_plain_group(
        &mut self,
        parent_visitor: &ParentVisitor,
        ident: &RustIdent,
        rep: Representation,
        cli: &Cli,
    ) {
        if let Some(plain_group) = self.plain_groups.get(ident) {
            // the clone is to get around the borrow checker
            let plain_group = plain_group.clone();
            if let Some(group) = plain_group.group.as_ref() {
                // we are defined via .cddl and thus need to register a concrete
                // representation of the plain group
                // `Some(inner)` = already materialized (inner = its rep, if a Record/GroupChoice);
                // `None` = not yet materialized. Extracted up front so the `rust_structs` borrow ends
                // before any `&mut self` call below.
                let existing =
                    self.rust_structs
                        .get(ident)
                        .map(|rust_struct| match &rust_struct.variant {
                            RustStructType::Record(record) => Some(record.rep),
                            RustStructType::GroupChoice { rep, .. } => Some(*rep),
                            _ => None,
                        });
                match existing {
                    // A plain group materialized once cannot be re-materialized with a DIFFERENT
                    // representation — one Rust struct has exactly one wire shape. This is reached
                    // when an array-of-plain-group collapsed to the bare group ident (`[coords]` ->
                    // Array-rep `Coords`, via `parse_group_type`'s `WrappedBasicGroup`) is then used
                    // where a conflicting rep is demanded, notably as a MAP-record / map-group-choice
                    // field value (`{ k: [coords] }`, `{ f0: [coords] // ... }`): the record-field
                    // and group-choice-arm paths stamp the outer Map rep onto the already-Array group.
                    // That collapsed field also carries a `basic_override` the map-value
                    // (de)serializer emits no code for (E0425/E0599), so the shape is unsupported
                    // today — reject it gracefully (drained by `finalize`) rather than `panic!`, with
                    // the supported named-type remedy. A matching rep is a no-op.
                    Some(Some(found_rep)) if found_rep != rep => {
                        self.record_rejection(format!(
                            "`{ident}` is used with conflicting representations (both array and map) \
                             — a single generated struct has one wire shape. This arises when an \
                             array wrapping a plain group (`[{ident}]`) is used as a map-value / \
                             map-group-choice field, whose (de)serializer is unsupported today. Give \
                             the array its own named type rule (e.g. `t = [..]`) and reference `t`."
                        ));
                    }
                    // already materialized with the SAME rep — nothing to do
                    Some(Some(_)) => {}
                    // A plain group only ever materializes via `parse_group` below, i.e. as a
                    // Record or GroupChoice — any other variant here is an internal invariant
                    // break, kept as loud as the `assert_eq!` this match replaced (which failed
                    // on `None != Some(rep)`), not silently absorbed.
                    Some(None) => unreachable!(
                        "plain group `{ident}` already materialized as a non-Record/non-GroupChoice struct"
                    ),
                    None => {
                        // you can't tag plain groups hence the None
                        // we also don't support generics in plain groups hence the other None
                        crate::parsing::parse_group(
                            self,
                            parent_visitor,
                            group,
                            ident,
                            rep,
                            None,
                            None,
                            &plain_group.rule_metadata,
                            cli,
                        );
                    }
                }
            } else {
                // If plain_group is None, then this wasn't defined in .cddl but instead
                // created by us i.e. in a group choice with inlined fields.
                // In this case we already should have registered the struct with a defined
                // representation and we don't need to parse it here.
                assert!(self.rust_structs.contains_key(ident));
            }
        }
    }

    pub fn is_plain_group(&self, name: &RustIdent) -> bool {
        self.plain_groups.contains_key(name)
    }

    /// The idents of every plain group registered from a `.cddl` rule (directly-defined groups whose
    /// `PlainGroupInfo` carries a source `Group`), in deterministic (`BTreeMap`) order. The
    /// extern-interface projection walks these to leave a `; unexported:` record for a plain group
    /// that never materialized a `rust_structs` entry (never referenced in the dep's own spec) — a
    /// materialized group is reached through `rust_structs` instead. Anonymous group-choice-variant
    /// groups (`PlainGroupInfo` with no source `Group`) are excluded: they carry no source rule name
    /// and are not a projectable surface.
    pub fn directly_defined_plain_group_idents(&self) -> impl Iterator<Item = &RustIdent> {
        self.plain_groups
            .iter()
            .filter(|(_, info)| info.group.is_some())
            .map(|(ident, _)| ident)
    }

    fn mark_new_can_fail(&mut self, name: RustIdent) {
        self.news_can_fail.insert(name);
    }

    pub fn can_new_fail(&self, name: &RustIdent) -> bool {
        self.news_can_fail.contains(name)
    }

    pub fn mark_scope(&mut self, ident: RustIdent, scope: ModuleScope) {
        if let Some(old_scope) = self.scopes.insert(ident.clone(), scope.clone())
            && old_scope != scope
        {
            panic!(
                "{} defined multiple times, first referenced in scope '{}' then in '{}'",
                ident, old_scope, scope
            );
        }
    }

    pub fn scope(&self, ident: &RustIdent) -> &ModuleScope {
        self.scopes.get(ident).unwrap_or(&self.root_scope)
    }

    /// The set of cross-crate extern-dependency crate names in use — the leading component of every
    /// non-exported (`_CDDL_CODEGEN_EXTERN_DEPS_DIR_/<dep>`) scope. Used to validate
    /// `--extern-wasm-crate` mappings so a misspelled dep name errors loudly instead of silently
    /// no-op'ing.
    pub fn extern_dep_names(&self) -> BTreeSet<String> {
        self.scopes
            .values()
            .filter(|scope| !scope.export())
            .filter_map(|scope| scope.components().first().cloned())
            .collect()
    }

    /// Record the original CDDL source name for a top-level rule's `RustIdent`. Called once per
    /// parsed rule (`api::with_types`), before camel-casing has erased the source spelling.
    pub fn mark_source_rule_name(&mut self, ident: RustIdent, source_name: String) {
        self.rule_source_names.insert(ident, source_name);
    }

    /// Record a `@rust_name` pin: `derived` (the consumer-derived `RustIdent`) is spelled `pinned`
    /// in the dependency's own crate. See the `rust_name_pins` field doc. Validated in
    /// `parsing::handle_rust_name_pin` (extern-scope-only, reserved-ident-clean) before this call.
    pub fn mark_rust_name_pin(&mut self, derived: RustIdent, pinned: String) {
        self.rust_name_pins.insert(derived, pinned);
    }

    /// The full pin map (`derived RustIdent` -> `pinned dep name`), for the crate-boundary
    /// translation sites (`add_imports_from_scope_refs`).
    pub fn rust_name_pins(&self) -> &BTreeMap<RustIdent, String> {
        &self.rust_name_pins
    }

    /// The pinned dependency name for `derived`, if it carries a `@rust_name` pin. `None` = derive
    /// the name today's way (hand-stub compatibility).
    pub fn rust_name_pin(&self, derived: &RustIdent) -> Option<&str> {
        self.rust_name_pins.get(derived).map(|s| s.as_str())
    }

    /// The CDDL prelude name a synthesized `prelude_<name>` rule ident stands for (`PreludeBignint`
    /// → `bignint`), or `None` for any other ident. A CDDL-prelude type referenced from a transparent
    /// extern-interface row renders back to this bare prelude name (the consumer re-expands the
    /// prelude identically) rather than dangling on the synthesized, never-exported `prelude_<name>`
    /// rule. Covers whatever prelude subset the IR actually materialized (`prelude_to_emit`).
    pub fn prelude_cddl_name(&self, ident: &RustIdent) -> Option<String> {
        self.prelude_to_emit.iter().find_map(|name| {
            (RustIdent::new(CDDLIdent::new(format!("prelude_{name}"))) == *ident)
                .then(|| name.clone())
        })
    }

    /// The exact CDDL source rule name `ident` was registered under (e.g. `my-rule`, which
    /// `RustIdent` camel-cases to `MyRule`, indistinguishable from `my_rule`). `None` for a struct
    /// synthesized during IR build (no source rule). The conformance oracle roots its validator here
    /// so it targets a PROVABLE spec rule rather than a lossy reversal of the ident.
    pub fn source_rule_name(&self, ident: &RustIdent) -> Option<&str> {
        self.rule_source_names.get(ident).map(|s| s.as_str())
    }

    /// Whether `ident` names a top-level CDDL rule (as opposed to a struct synthesized during IR
    /// build — an embedded record, inline group, etc.). `scopes` is populated by `mark_scope`,
    /// which `api::with_types` calls once per parsed rule, so its key set is exactly the top-level
    /// rules. Used by the `--emit-tests-conformance` oracle: only a real rule name can be aliased as
    /// the validator's synthetic root, so synthesized structs get no conformance call.
    pub fn is_toplevel_rule(&self, ident: &RustIdent) -> bool {
        self.scopes.contains_key(ident)
    }

    /// Record that a non-embeddable multi-arm group-choice arm in rule `owner` (source name) has
    /// taken `ident` as the name of a struct that will be EMITTED. See the
    /// `group_choice_arm_claims` field doc.
    pub fn claim_group_choice_arm_ident(&mut self, ident: RustIdent, owner: String) {
        self.group_choice_arm_claims.insert(ident, owner);
    }

    /// Which already-parsed rule, if any, owns an emitted group-choice arm struct named `ident`.
    pub fn group_choice_arm_claimant(&self, ident: &RustIdent) -> Option<&str> {
        self.group_choice_arm_claims.get(ident).map(|s| s.as_str())
    }

    /// An ident guaranteed not to name anything the IR already knows, derived deterministically from
    /// `base`.
    ///
    /// A multi-arm group-choice arm's record must be built through the normal
    /// [`Self::register_rust_struct`] path, which means occupying a name in the global maps for the
    /// duration. When the arm's own name is already claimed, borrowing it would clobber the real
    /// owner (and, for an embeddable arm, `remove_rust_struct` would then DELETE it), so the arm
    /// borrows one of these instead. Nothing is ever emitted under a synthesized name: an embeddable
    /// arm removes it again immediately, and a non-embeddable arm that needed one has, by
    /// construction, also recorded a rejection that aborts before emission.
    pub fn fresh_synthesized_ident(&self, base: &str) -> RustIdent {
        let taken = |ident: &RustIdent| {
            self.rust_structs.contains_key(ident)
                || self.plain_groups.contains_key(ident)
                || self.scopes.contains_key(ident)
                || self.generic_instances.contains_key(ident)
                || self.group_choice_arm_claims.contains_key(ident)
                || self
                    .type_aliases
                    .contains_key(&AliasIdent::Rust(ident.clone()))
        };
        let mut candidate = RustIdent::new(CDDLIdent::new(base));
        let mut suffix = 0u32;
        while taken(&candidate) {
            suffix += 1;
            candidate = RustIdent::new(CDDLIdent::new(format!("{base}_{suffix}")));
        }
        candidate
    }

    // we need to do this for some generated intermediate structures as the parsing code
    // doesn't allow to just generate a rust struct but instead inserts everything needed
    pub fn remove_rust_struct(&mut self, ident: &RustIdent) -> Option<RustStruct> {
        self.plain_groups.remove(ident);
        self.scopes.remove(ident);
        self.rule_source_names.remove(ident);
        self.rust_structs.remove(ident)
    }

    pub fn used_as_key(&self, name: &RustIdent) -> bool {
        self.key_demand.contains_key(name)
    }

    /// The comparison/hash trait demand resolved onto `name` (the union of every tag + auto-detected
    /// internal-key contribution), or `None` if it is not used as a key.
    pub fn key_demand(&self, name: &RustIdent) -> Option<DemandSet> {
        self.key_demand.get(name).copied()
    }

    /// The full set of idents finalize resolved as used-as-key, in sorted (`BTreeMap`) order. The
    /// consumer-side `borrowed_key_types.rs` emitter partitions this for the extern idents owned by a
    /// `--workspace-dep` (those get marked here then otherwise evaporate — no in-crate type to derive).
    pub fn used_as_key_idents(&self) -> impl Iterator<Item = &RustIdent> {
        self.key_demand.keys()
    }

    /// The directly-tagged demand roots (pre-transitive-expansion), sorted. Drives the emitted
    /// compile-time demand assertions (`generation/mod.rs`).
    pub fn key_demand_roots(&self) -> &BTreeMap<RustIdent, DemandSet> {
        &self.key_demand_roots
    }

    /// Record a directly-tagged demand root (from `@used_as_key` or `--key-requests`). Unions into
    /// both the roots map and the full demand map (finalize then expands the full map transitively).
    pub fn mark_key_demand(&mut self, name: RustIdent, demand: DemandSet) {
        let root = self.key_demand_roots.entry(name.clone()).or_default();
        *root = root.union(demand);
        let full = self.key_demand.entry(name).or_default();
        *full = full.union(demand);
    }

    /// Union `demand` into the full demand map without touching the roots map — the transitive-expansion
    /// path used by `finalize`.
    fn union_key_demand(&mut self, name: RustIdent, demand: DemandSet) {
        let full = self.key_demand.entry(name).or_default();
        *full = full.union(demand);
    }

    /// The set of idents tagged `@used_as_elem`, in sorted (`BTreeSet`) order — the generator walks
    /// this to mint one loose-list wasm wrapper per marked element (see `mark_used_as_elem`).
    pub fn used_as_elem(&self) -> &BTreeSet<RustIdent> {
        &self.used_as_elem
    }

    /// Whether `ident` is a SYNTHESIZED anonymous generic instance resolving to a transparent
    /// collection (populated by `converge_anonymous_collection_instance_wasm`). When true, the wasm
    /// struct walk must NOT mint a rule-named collection class for it — its wrapper is the STRUCTURAL
    /// name, reached through the flipped-on `gen_wasm_alias` passthrough. See the field's doc.
    pub fn is_anonymous_collection_instance(&self, ident: &RustIdent) -> bool {
        self.anonymous_collection_instances.contains(ident)
    }

    /// Whether `ident`'s transparent rust alias was generator-SYNTHESIZED (a table rule's auto-named
    /// keys-list, `create_and_register_array_type`) rather than authored as a rule. The wasm struct
    /// walk's Array arm reads this to decide `rule_declared`: a synthesized keys-list must NOT trip
    /// the criterion-9 shadow warning (no rule declares it), whereas an authored `foo_list = [* foo]`
    /// of the same structural ident must (its class shadows the would-be-borrowed dep wrapper). False
    /// for an ident with no rust alias (`type_aliases` miss), and for an authored rule that registered
    /// its Array struct before any synthesis re-mint reached it.
    pub fn is_synthesized_collection(&self, ident: &RustIdent) -> bool {
        self.type_aliases
            .get(&AliasIdent::Rust(ident.clone()))
            .is_some_and(|alias| alias.synthesized_collection)
    }

    pub fn mark_used_as_elem(&mut self, name: RustIdent) {
        self.used_as_elem.insert(name);
    }

    /// The set of base generic extern idents tagged `@raw_bytes_flavor` (see `mark_raw_bytes_flavor`).
    /// `GenericInstance::resolve` consults this to decide whether an instance carrying a raw-bytes
    /// argument aliases the `<Base>RawBytes` flavor instead of the plain base name.
    pub fn raw_bytes_flavor(&self) -> &BTreeSet<RustIdent> {
        &self.raw_bytes_flavor
    }

    pub fn mark_raw_bytes_flavor(&mut self, name: RustIdent) {
        self.raw_bytes_flavor.insert(name);
    }

    /// Whether `ident` names an extern / raw-bytes rule declared `@copy`. `is_copy` ORs this into its
    /// `Rust(ident)` arm so the generator stops cloning a value whose rust type derives `Copy`.
    pub fn is_copy_extern(&self, ident: &RustIdent) -> bool {
        self.copy_externs.contains(ident)
    }

    pub fn mark_copy_extern(&mut self, name: RustIdent) {
        self.copy_externs.insert(name);
    }

    pub fn mark_extern_companions(
        &mut self,
        name: RustIdent,
        companions: crate::comment_ast::ExternCompanions,
    ) {
        self.extern_companions.insert(name, companions);
    }

    /// The whole `@extern_companions` registry, keyed by declaring extern rule. Empty unless some
    /// rule carries the directive, which is what keeps the deferral arm that reads it inert (and the
    /// output byte-identical) for every spec that does not.
    pub fn extern_companions(&self) -> &BTreeMap<RustIdent, crate::comment_ast::ExternCompanions> {
        &self.extern_companions
    }

    /// The `use`-path prefix under which `class` is declared to ALREADY exist, given that every named
    /// constituent of the wrapper resolves to `owner`. `None` when `owner` carries no declaration or
    /// its declaration does not list `class` — an unlisted structural companion mints locally, which
    /// is the whole point of the class list being a filter rather than a blanket opt-out.
    pub fn extern_companion_path(&self, owner: &RustIdent, class: &str) -> Option<&str> {
        self.extern_companions
            .get(owner)
            .filter(|c| c.classes.contains(class))
            .map(|c| c.path_prefix.as_str())
    }

    /// Whether the rule `ident` was declared `@no_json_schema_export` — the spec author's statement
    /// that this type is not part of the published JSON-schema surface. The json-gen row loop skips
    /// it; nothing else consults this.
    pub fn is_no_json_schema_export(&self, ident: &RustIdent) -> bool {
        self.no_json_schema_export.contains(ident)
    }

    pub fn mark_no_json_schema_export(&mut self, name: RustIdent) {
        self.no_json_schema_export.insert(name);
    }

    /// The base generic extern idents for which a flavored (`<Base>RawBytes`) instance was actually
    /// emitted during `finalize`. The extern re-export glue emits `pub use crate::<Base>RawBytes;`
    /// for exactly these (see `mark_raw_bytes_flavor_emitted`).
    pub fn raw_bytes_flavor_emitted(&self) -> &BTreeSet<RustIdent> {
        &self.raw_bytes_flavor_emitted
    }

    pub fn mark_raw_bytes_flavor_emitted(&mut self, base: RustIdent) {
        self.raw_bytes_flavor_emitted.insert(base);
    }

    /// Resolve a marked-`@used_as_elem` ident to the ELEMENT `RustType` of its loose-list wrapper,
    /// resolving through a type alias exactly as an inline `[* ident]` usage does (mirrors the
    /// alias-vs-struct split in `new_type`): a named alias resolves to its (aliased) base type, and a
    /// plain registered struct/reserved ident becomes `ConceptualRustType::Rust(ident)`.
    pub fn used_as_elem_element_type(&self, ident: &RustIdent) -> RustType {
        match self.resolve_alias(&AliasIdent::Rust(ident.clone())) {
            Some(ty) => ty,
            None => ConceptualRustType::Rust(ident.clone()).into(),
        }
    }

    /// The IR dump: `trace` only, and the caller in `api::generate_to_disk` ALSO guards the call.
    /// Both guards are wanted. These lines are `trace!` so the function stays correct if another
    /// caller appears; the call-site guard is what skips the `{:?}` formatting of every registered
    /// struct, which is the actual cost (215 KB on a 501-line spec) — `trace!` evaluates its
    /// arguments lazily per line, but the traversal and the per-line calls still happen.
    pub fn print_info(&self) {
        if !self.plain_groups.is_empty() {
            crate::trace!("\n\nPlain groups:");
            for plain_group in self.plain_groups.iter() {
                crate::trace!("{}", plain_group.0);
            }
        }

        if !self.type_aliases.is_empty() {
            crate::trace!("\n\nAliases:");
            for (alias_name, alias_info) in self.type_aliases.iter() {
                crate::trace!("{alias_name:?} -> {alias_info:?}");
            }
        }

        if !self.generic_defs.is_empty() {
            crate::trace!("\n\nGeneric Definitions:");
            for (ident, def) in self.generic_defs.iter() {
                crate::trace!("{ident} -> {def:?}");
            }
        }

        if !self.generic_instances.is_empty() {
            crate::trace!("\n\nGeneric Instances:");
            for (ident, def) in self.generic_instances.iter() {
                crate::trace!("{ident} -> {def:?}");
            }
        }

        if !self.rust_structs.is_empty() {
            crate::trace!("\n\nRustStructs:");
            for (ident, rust_struct) in self.rust_structs.iter() {
                crate::trace!("{ident} -> {rust_struct:?}\n");
            }
        }
    }

    fn emit_prelude(&mut self, cddl_name: String, cli: &Cli) {
        // we just emit this directly into this scope.
        // due to some referencing others this is the quickest way
        // to support it.
        // TODO: we might want to custom-write some of these to make them
        // easier to use instead of directly parsing
        if self.prelude_to_emit.insert(cddl_name.clone()) {
            let def = format!(
                "prelude_{} = {}\n",
                cddl_name,
                cddl_prelude(&cddl_name).unwrap()
            );
            let cddl = cddl::parser::cddl_from_str(&def, true).unwrap();
            assert_eq!(cddl.rules.len(), 1);
            let pv = ParentVisitor::new(&cddl).unwrap();
            crate::parsing::parse_rule(self, &pv, cddl.rules.first().unwrap(), cli);
        }
    }
}

/// The shape-derived ident an inline `#6.258([* Elem])` set occurrence nominalizes into (Phase 2.4):
/// `Set<Elem-variant>`, with a `NonEmpty` infix for the `[+]` bound. The prefix `Set` is the registry
/// entry for tag 258; the element spelling reuses `for_variant()` — the SAME element-spelling scheme
/// the wasm structural collection names (`<Elem>List` / `<Elem>OrderedSet`) and the generic
/// instantiation names use — so it is deterministic. `RustIdent::new` camel-cases the underscore form
/// (`set_u_64` never arises — `for_variant` yields `U64`, so `set_U64` → `SetU64`; `set_non_empty_Text`
/// → `SetNonEmptyText`). Distinct shapes yield distinct names; a name that nonetheless collides with a
/// user rule / generic instantiation is caught in `nominalize_inline_sets`.
fn inline_set_nominal_ident(element: &ConceptualRustType, non_empty: bool) -> RustIdent {
    let variant = element.for_variant();
    let raw = if non_empty {
        format!("set_non_empty_{variant}")
    } else {
        format!("set_{variant}")
    };
    RustIdent::new(CDDLIdent::new(raw))
}

/// Recurse into every `RustType` a `ConceptualRustType` transitively holds — including through an
/// `Alias`'s resolved base, which is where a `T / null` collapse hides its inline set: a use-site
/// field typed as such an alias carries `Alias(MaybeSet, Optional(#6.258([* uint])))` INLINE (its own
/// snapshot of the base, separate from the registered `type_aliases` entry), so both copies must be
/// rewritten to stay consistent (else the field's serialize inlines the old array shape while the
/// field TYPE names the nominal — an E0308).
fn rewrite_inline_sets_in_conceptual(
    ct: &mut ConceptualRustType,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    match ct {
        ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
            rewrite_inline_sets_in_type(inner, minted);
        }
        ConceptualRustType::Map(k, v) => {
            rewrite_inline_sets_in_type(k, minted);
            rewrite_inline_sets_in_type(v, minted);
        }
        ConceptualRustType::Alias(_, inner) => rewrite_inline_sets_in_conceptual(inner, minted),
        ConceptualRustType::Fixed(_)
        | ConceptualRustType::Primitive(_)
        // `any` is opaque — no inline IR sets to rewrite.
        | ConceptualRustType::Any
        | ConceptualRustType::Rust(_) => {}
    }
}

/// Recursively rewrite every INLINE `#6.258([* T])` occurrence WITHIN `rt` into a reference to its
/// shape-derived nominal set wrapper, minting the wrapper (once per deduped ident) into `minted`. See
/// `IntermediateTypes::nominalize_inline_sets`. Recurses into container inners FIRST so a nested inline
/// set inside the element is nominalized before the outer occurrence captures the (already-rewritten)
/// element.
fn rewrite_inline_sets_in_type(rt: &mut RustType, minted: &mut BTreeMap<RustIdent, RustStruct>) {
    rewrite_inline_sets_in_conceptual(&mut rt.conceptual_type, minted);
    // An inline set occurrence is a homogeneous `Array` whose INNERMOST encoding is a mandatory
    // `Tagged(258)`. (A named set nominal's wrapped array carries the tag on the STRUCT, not the
    // `RustType`, so it never matches here; a non-258 inline tag has no registry set default.)
    if !matches!(rt.conceptual_type, ConceptualRustType::Array(_))
        || rt.encodings.first() != Some(&CBOREncodingOperation::Tagged(258))
    {
        return;
    }
    let ConceptualRustType::Array(element) = &rt.conceptual_type else {
        unreachable!()
    };
    let non_empty = rt.is_non_empty_array();
    let ident = inline_set_nominal_ident(&element.conceptual_type, non_empty);
    // The wrapper owns a CLEAN array (the 258 becomes the struct's mandatory `Option<Sz>` tag record);
    // preserve the occurrence-count bound so `[+]` selects the `NonEmptyOrderedSet` twin.
    let mut array_type: RustType = ConceptualRustType::Array(element.clone()).into();
    if let Some(bounds) = rt.config.bounds {
        array_type = array_type.with_bounds(bounds);
    }
    // 258 ⇒ `@duplicates reject` (IANA set semantics); `register_rust_struct` reads this off the
    // wrapper config to swap the inner to the `OrderedSet`/`NonEmptyOrderedSet` twin.
    let effective_metadata = RuleMetadata {
        duplicates: Some(crate::comment_ast::DuplicatesPolicy::Reject),
        ..Default::default()
    };
    let nominal = RustStruct::new_wrapper(
        ident.clone(),
        Some(258),
        Some(&effective_metadata),
        array_type,
        None,
    )
    .as_set_nominal();
    minted.entry(ident.clone()).or_insert(nominal);
    // Reference the nominal; any OUTER encodings that sat ON TOP of the 258 (an `#6.24(#6.258(...))`
    // double tag) stay on the reference — the nominal owns only the innermost 258.
    let outer_encodings = rt.encodings[1..].to_vec();
    *rt = RustType {
        conceptual_type: ConceptualRustType::Rust(ident),
        encodings: outer_encodings,
        config: RustTypeSerializeConfig::default(),
    };
}

/// Rewrite inline set occurrences in every `RustType` a registered `RustStruct` holds. See
/// `rewrite_inline_sets_in_type`.
fn rewrite_inline_sets_in_struct(
    rs: &mut RustStruct,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    match &mut rs.variant {
        RustStructType::Record(record) => rewrite_inline_sets_in_record(record, minted),
        RustStructType::Table { domain, range, .. } => {
            rewrite_inline_sets_in_type(domain, minted);
            rewrite_inline_sets_in_type(range, minted);
        }
        RustStructType::Array { element_type, .. } => {
            rewrite_inline_sets_in_type(element_type, minted);
        }
        RustStructType::TypeChoice { variants }
        | RustStructType::GroupChoice { variants, .. }
        | RustStructType::CStyleEnum { variants } => {
            for variant in variants.iter_mut() {
                match &mut variant.data {
                    EnumVariantData::RustType(rt) => rewrite_inline_sets_in_type(rt, minted),
                    EnumVariantData::Inlined(record) => {
                        rewrite_inline_sets_in_record(record, minted)
                    }
                }
            }
        }
        RustStructType::Wrapper { wrapped, .. } => rewrite_inline_sets_in_type(wrapped, minted),
        RustStructType::Extern | RustStructType::RawBytesType => {}
    }
}

fn rewrite_inline_sets_in_record(
    record: &mut RustRecord,
    minted: &mut BTreeMap<RustIdent, RustStruct>,
) {
    for field in record.fields.iter_mut() {
        rewrite_inline_sets_in_type(&mut field.rust_type, minted);
    }
}

mod rust_type;
pub use rust_type::*;

/// A graceful-rejection message if `source_name` (a user-chosen rule / plain-group name, as spelled
/// in the CDDL) cannot be used as a Rust type name, else `None`. This mirrors the two `assert!`
/// guards in `RustIdent::new` exactly — a camel-cased form that collides with a reserved Rust
/// std/prelude type (`option` → `Option`, `box` → `Box`, `fn` → `Fn`, `self`/`Self` → `Self`), or a
/// CDDL keyword (`true` / `false` / prelude type names) — so the same names those asserts would
/// panic on are instead rejected gracefully when caught at the parse-walk seam (`api::with_types`).
/// `int` is excluded from the keyword branch identically to `RustIdent::new`: it names the project's
/// own pre-registered extern struct, not a colliding user type.
///
/// The asserts stay as a backstop for synthesized/internal idents (which never route through here);
/// this function is only for user-chosen names, where a panic on valid CDDL is the bug being fixed.
pub fn reserved_ident_rejection(source_name: &str) -> Option<String> {
    let camel = convert_to_camel_case(source_name);
    if crate::rust_reserved::STD_TYPES.contains(&camel.as_str()) {
        return Some(format!(
            "rule `{source_name}`: its name camel-cases to `{camel}`, a reserved Rust std/prelude \
             type the generated code depends on — emitting a type by that name would shadow it. A \
             rule/group name becomes the emitted Rust type name directly, so (unlike a struct field, \
             which a `; @name` comment renames) the CDDL identifier itself must be renamed to a \
             non-reserved name."
        ));
    }
    if source_name != "int" && is_identifier_reserved(source_name) {
        return Some(format!(
            "rule `{source_name}`: `{source_name}` is a reserved CDDL keyword and cannot be used as \
             a rule/group name. A rule/group name becomes the emitted Rust type name directly, so \
             (unlike a struct field, which a `; @name` comment renames) the CDDL identifier itself \
             must be renamed to a non-reserved name."
        ));
    }
    None
}

/// A graceful-rejection message if a `@rust_name` PIN cannot be used as a Rust type name, else
/// `None`. A pin becomes the emitted Rust name for the dependency's type verbatim (the consumer
/// imports `use dep::<pin> as <derived>;`), so it must clear the SAME reserved-ident bar a derived
/// name does — a `@rust_name Option` pin describes a type the dependency could never have emitted
/// (its own `reserved_ident_rejection` would have fired), so the pin can never be honored. Mirrors
/// `reserved_ident_rejection` but names the pin and the rule it sits on.
pub fn reserved_pin_rejection(pin: &str, rule: &str) -> Option<String> {
    let camel = convert_to_camel_case(pin);
    if crate::rust_reserved::STD_TYPES.contains(&camel.as_str()) || is_identifier_reserved(pin) {
        return Some(format!(
            "@rust_name `{pin}` on rule `{rule}`: the pinned Rust name is a reserved Rust \
             std/prelude type or CDDL keyword — a dependency could never have emitted a type by that \
             name, so this pin can never be honored. Choose a non-reserved name."
        ));
    }
    None
}

/// A rule/group name containing `.` is rejected at the reserved-name pre-scan seam. RFC 8610 allows
/// dots in identifiers, but `convert_to_camel_case` passes `.` straight through (`cose.label` →
/// `Cose.label`, invalid Rust) and `RustIdent::new` adds no dot check — so a dotted name would flow
/// silently into a crate that does not compile. Dotted idents chiefly arise from `cddlc`'s
/// `as`-namespacing expansion (`import … as cose` rewrites imported rules to `cose.<name>`), which
/// cddl-codegen does not yet support; rejecting them loudly is the interop-honest behavior until
/// scope-qualified idents land.
pub fn dotted_ident_rejection(source_name: &str) -> Option<String> {
    if source_name.contains('.') {
        return Some(format!(
            "rule `{source_name}`: its name contains a `.`, which cddl-codegen does not support in \
             a rule/group name — it camel-cases to invalid Rust (`{source_name}` → \
             `{}`). Dotted identifiers typically come from `cddlc`'s `as`-namespacing expansion \
             (`import … as <prefix>` rewrites rules to `<prefix>.<name>`); rename the rule to a \
             dot-free identifier.",
            convert_to_camel_case(source_name)
        ));
    }
    None
}

mod idents;
use crate::cli::Cli;
pub use idents::*;

mod structs;
pub use structs::*;
