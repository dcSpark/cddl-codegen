use cbor_event::{Special, Type as CBORType};
use cddl::ast::parent::ParentVisitor;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::comment_ast::{DemandSet, RuleMetadata};
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
    // Base generic extern idents tagged `@raw_bytes_flavor`: an instance of one whose argument
    // resolves to a `_CDDL_CODEGEN_RAW_BYTES_TYPE_` aliases the `<Base>RawBytes` wrapper flavor
    // instead of the plain `<Base>`. Opt-in only — see `RuleMetadata::raw_bytes_flavor`.
    raw_bytes_flavor: BTreeSet<RustIdent>,
    // Subset of `raw_bytes_flavor` for which an actual flavored instance was emitted during
    // `finalize` (a raw-bytes argument was supplied at least once). The extern re-export glue emits
    // `pub use crate::<Base>RawBytes;` only for these, so a tag with no raw-bytes instance never
    // forces the user to define an unused flavor type.
    raw_bytes_flavor_emitted: BTreeSet<RustIdent>,
    // which scope an ident is declared in
    scopes: BTreeMap<RustIdent, ModuleScope>,
    // The ORIGINAL CDDL source name for each top-level rule's `RustIdent`. `RustIdent::new`
    // camel-cases (and thus destroys the `-`/`_` distinction that CDDL treats as significant), so the
    // ident alone can't be reversed back to the spec rule. Recorded verbatim at rule-registration
    // time (`api::with_types`, alongside `mark_scope`) so the conformance oracle can root its
    // validator at the PROVABLE source rule rather than a lossy snake↔camel guess.
    rule_source_names: BTreeMap<RustIdent, String>,
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
            news_can_fail: BTreeSet::new(),
            key_demand: BTreeMap::new(),
            key_demand_roots: BTreeMap::new(),
            used_as_elem: BTreeSet::new(),
            raw_bytes_flavor: BTreeSet::new(),
            raw_bytes_flavor_emitted: BTreeSet::new(),
            scopes: BTreeMap::new(),
            rule_source_names: BTreeMap::new(),
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
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } if *bounds == Some((Some(1), None))
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
                RustStructType::Array {
                    element_type,
                    bounds,
                } if *bounds == Some((Some(1), None))
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
                    record.fields.iter().for_each(|fl| walk(&fl.rust_type, f))
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
    ) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        matches!(
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

    /// Every camel-cased Rust type ident this crate will DEFINE (structs, user aliases, plain
    /// groups, generic defs/instances). Deterministic (`BTreeSet`, no hashing). Consumed by
    /// generation to pick collision-proof names for the emitted `serialize`/`deserialize` fn
    /// generics: a rule named `r`/`w` camel-cases to a type `R`/`W` that would otherwise shadow the
    /// hardcoded `fn deserialize<R: BufRead + Seek>` / `fn serialize<'se, W: Write>` parameters,
    /// producing non-compiling crates (E0574 struct-shape / E0599 enum-variant-path).
    pub fn defined_rust_idents(&self) -> BTreeSet<String> {
        let mut out = BTreeSet::new();
        out.extend(self.rust_structs.keys().map(|k| k.to_string()));
        out.extend(self.plain_groups.keys().map(|k| k.to_string()));
        out.extend(self.generic_defs.keys().map(|k| k.to_string()));
        out.extend(self.generic_instances.keys().map(|k| k.to_string()));
        for alias in self.type_aliases.keys() {
            if let AliasIdent::Rust(ident) = alias {
                out.insert(ident.to_string());
            }
        }
        out
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
                let structural = ConceptualRustType::name_for_wasm_map(domain, range).to_string();
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

    /// For each scope, which other scopes are referenced, and which structs are referenced
    ///
    /// `deferred` (wasm pass only) maps every collection-wrapper ident the consumer is NOT minting —
    /// because a mapped dependency's `--extern-wrapper-index` already owns it — to that dependency's
    /// `collections` module scope. A deferred wrapper's referencing sites import it from there (a
    /// plain `use <dep_wasm>::collections::<Name>;` after the `--extern-wasm-crate` remap) from EVERY
    /// using scope, root included, since the class no longer lives locally. Empty for the rust pass
    /// and whenever `--extern-wrapper-index` is unused, so output is byte-identical without the flag.
    pub fn scope_references(
        &self,
        wasm: bool,
        deferred: &BTreeMap<RustIdent, ModuleScope>,
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
        fn register_deferred_non_empty_map_source(
            refs: &mut BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            sole_owners: &BTreeMap<String, RustIdent>,
            wrapper_ident: &RustIdent,
            key: &RustType,
            value: &RustType,
        ) {
            let loose_ident = ConceptualRustType::name_for_wasm_map(key, value);
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
                    // A `[+ elem]` field is wrapped as the restricted `NonEmpty*List`, not the loose
                    // `*List`; look that name up in `deferred` so a deferred NonEmpty wrapper is
                    // imported under its real (NonEmpty) name. The non-deferred fallback branches
                    // below stay keyed on the loose name (unchanged output without the flag).
                    let is_non_empty = wasm && ty.is_non_empty_array();
                    let deferred_wrapper = (wasm
                        && (is_non_empty || !elem_ty.directly_wasm_exposable(types)))
                    .then(|| {
                        let wrapper_name = if is_non_empty {
                            ty.non_empty_wasm_wrapper_name(types)
                        } else {
                            elem_ty.name_as_wasm_array(types)
                        };
                        let ident = RustIdent::new(CDDLIdent::new(wrapper_name));
                        deferred.get(&ident).map(|scope| (ident, scope))
                    })
                    .flatten();
                    if let Some((arr_wrapper_ident, dep_scope)) = deferred_wrapper {
                        // Deferred to a dependency's `--extern-wrapper-index`: the list wrapper class
                        // no longer lives locally, so import it from the dep's `collections` module
                        // from EVERY using scope (root included) and do NOT recurse into the element
                        // (both wrapper and element are the dependency's).
                        refs.entry(current_scope.to_owned())
                            .or_default()
                            .entry(dep_scope.clone())
                            .or_default()
                            .insert(arr_wrapper_ident);
                        return;
                    }
                    if is_non_empty {
                        // Locally-minted restricted wrapper whose loose `try_from` source may be
                        // deferred: route the source's import at the wrapper's emission scope.
                        let ne_ident =
                            RustIdent::new(CDDLIdent::new(ty.non_empty_wasm_wrapper_name(types)));
                        register_deferred_non_empty_list_source(
                            refs, types, deferred, &ne_ident, elem_ty,
                        );
                    }
                    if wasm
                        && !elem_ty.directly_wasm_exposable(types)
                        && *current_scope != *ROOT_SCOPE
                    {
                        // TODO: we should be doing array wrappers where they are declared or used,
                        // but for the latter, what to do if multiple places use it? default to lib?
                        // issue: https://github.com/dcSpark/cddl-codegen/issues/138
                        let arr_wrapper_ident =
                            RustIdent::new(CDDLIdent::new(elem_ty.name_as_wasm_array(types)));
                        refs.entry(current_scope.to_owned())
                            .or_default()
                            .entry(ROOT_SCOPE.clone())
                            .or_default()
                            .insert(arr_wrapper_ident);
                        // The wrapper's emitted code names the ELEMENT type bare in its EMISSION
                        // scope (root — synthesized array-wrapper idents default there via `scope`,
                        // and `GenerationScope::wasm` emits into it), which is NOT this using scope,
                        // so the element ref has to be registered FROM root. Recurse (not a single
                        // `set_ref`) so a nested anonymous wrapper — also root-emitted, its
                        // `current_scope != ROOT` guard failing here — resolves its own element too.
                        mark_refs(
                            refs,
                            types,
                            wasm,
                            sole_owners,
                            deferred,
                            &ROOT_SCOPE,
                            elem_ty,
                        );
                    } else {
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
                }
                ConceptualRustType::Fixed(_) | ConceptualRustType::Primitive(_) => {
                    // nothing to import
                }
                ConceptualRustType::Map(key, value) => {
                    let map_wrapper_ident = ConceptualRustType::name_for_wasm_map(key, value);
                    // A `{+ k => v}` field is wrapped as the restricted `NonEmptyMap*`, not the loose
                    // `Map*`; look that name up in `deferred` so a deferred NonEmpty map wrapper is
                    // imported under its real name. The non-deferred fallback branch below stays keyed
                    // on the loose `map_wrapper_ident` (unchanged output without the flag).
                    let deferred_lookup_ident = if wasm && ty.is_non_empty_map() {
                        RustIdent::new(CDDLIdent::new(ty.non_empty_wasm_map_wrapper_name(types)))
                    } else {
                        map_wrapper_ident.clone()
                    };
                    let map_deferred = if wasm {
                        deferred.get(&deferred_lookup_ident)
                    } else {
                        None
                    };
                    if wasm && ty.is_non_empty_map() && map_deferred.is_none() {
                        // Locally-minted restricted map wrapper whose loose `try_from` source may
                        // be deferred: route the source's import at the wrapper's emission scope.
                        register_deferred_non_empty_map_source(
                            refs,
                            types,
                            deferred,
                            sole_owners,
                            &deferred_lookup_ident,
                            key,
                            value,
                        );
                    }
                    if let Some(dep_scope) = map_deferred {
                        // The whole map wrapper is deferred to a dependency's
                        // `--extern-wrapper-index`: import it from the dep's `collections` module from
                        // every using scope (root included); wrapper, key, and value are all the
                        // dependency's, so don't recurse.
                        refs.entry(current_scope.to_owned())
                            .or_default()
                            .entry(dep_scope.clone())
                            .or_default()
                            .insert(deferred_lookup_ident);
                    } else if wasm && *current_scope != *ROOT_SCOPE {
                        // Resolve the map wrapper's import scope the SAME way emission decides
                        // placement (`table_shape_sole_owners` / `mint_sole_owner_table`): a shape
                        // with a sole named owner has its class + structural `pub type MapKToV`
                        // alias minted in the OWNER's module, so import it from there; zero-owner
                        // (anonymous-only) and multi-owner (same-shape rule pair) shapes keep the
                        // structural fallback class at the crate root. Fixes E0432 for an anonymous
                        // same-shape table used cross-module from a non-owner scope.
                        // (The Array arm's owner-resolution is still a TODO — no red cell pins it —
                        // issue: https://github.com/dcSpark/cddl-codegen/issues/138)
                        let import_scope = sole_owners
                            .get(&map_wrapper_ident.to_string())
                            .map_or_else(|| ROOT_SCOPE.clone(), |owner| types.scope(owner).clone());
                        if import_scope != *current_scope {
                            refs.entry(current_scope.to_owned())
                                .or_default()
                                .entry(import_scope.clone())
                                .or_default()
                                .insert(map_wrapper_ident);
                        }
                        // The locally minted map class's `keys()` accessor names the keys-list
                        // wrapper; when THAT is deferred (an extern key whose list the dep owns), it
                        // must be imported where the map class lives (`import_scope`), not the using
                        // site — the map class stays local while its keys-list is borrowed.
                        register_deferred_keys_list(refs, types, deferred, &import_scope, key);
                        // The non-deferred analogue: a non-exposable key whose keys-list wrapper is
                        // ROOT-minted (`FooList`) must be imported into `import_scope` (the non-root
                        // module the map class lives in) — otherwise `keys()` names it bare and
                        // dangles (E0425).
                        register_root_keys_list(refs, types, wasm, deferred, &import_scope, key);
                        // The wrapper's emitted code names its KEY and VALUE types bare in its
                        // EMISSION scope (`import_scope` — the sole owner's module or root, resolved
                        // the SAME way emission places the wrapper), which is not this using scope,
                        // so their refs must be registered from there. Recurse unconditionally: when
                        // `import_scope == current_scope` (a sole owner is the using module) `set_ref`
                        // drops the same-scope refs, and for a sole-owner NAMED table this just
                        // re-records what the `Table` struct-walk arm already did (refs are a
                        // `BTreeSet`, so it's a no-op).
                        mark_refs(refs, types, wasm, sole_owners, deferred, &import_scope, key);
                        mark_refs(
                            refs,
                            types,
                            wasm,
                            sole_owners,
                            deferred,
                            &import_scope,
                            value,
                        );
                    } else {
                        // ROOT_SCOPE (or the rust pass): the map class is emitted here, but its
                        // `keys()` accessor still borrows a deferred keys-list, which must be imported
                        // into THIS (the emission) scope.
                        if wasm {
                            register_deferred_keys_list(refs, types, deferred, current_scope, key);
                        }
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
                    // A NAMED `[+ …]` rule's restricted class (rule ident — never deferred) still
                    // borrows the LOOSE structural wrapper as its `try_from` source; when that
                    // source is deferred, import it at THIS rule's scope (its emission scope).
                    // The rule-named analogue of the inline Array arm's registration above.
                    if wasm && *bounds == Some((Some(1), None)) {
                        register_deferred_non_empty_list_source(
                            &mut refs,
                            self,
                            deferred,
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
                RustStructType::Record(record) => record.fields.iter().for_each(|field| {
                    mark_refs(
                        &mut refs,
                        self,
                        wasm,
                        &table_shape_sole_owners,
                        deferred,
                        current_scope,
                        &field.rust_type,
                    )
                }),
                RustStructType::Table {
                    domain,
                    range,
                    bounds,
                } => {
                    // The named table's own wasm class is emitted in `current_scope`; its `keys()`
                    // accessor names the ROOT-minted keys-list wrapper bare for a non-exposable key,
                    // so import it into this (non-root) module — the named/unref analogue of the Map
                    // arm's inline-use registration above.
                    register_root_keys_list(&mut refs, self, wasm, deferred, current_scope, domain);
                    // A NAMED `{+ …}` rule's restricted class borrows the LOOSE structural
                    // `MapKToV` as its `try_from` source; when that source is deferred, import it
                    // at THIS rule's scope — the rule-named analogue of the inline Map arm's
                    // registration above.
                    if wasm && *bounds == Some((Some(1), None)) {
                        register_deferred_non_empty_map_source(
                            &mut refs,
                            self,
                            deferred,
                            &table_shape_sole_owners,
                            &rust_struct.ident,
                            domain,
                            range,
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
                AliasIdent::Rust(_) => ConceptualRustType::Rust(RustIdent::new(raw.clone())).into(),
                AliasIdent::Reserved(reserved) if reserved == "int" => {
                    // We define an Int rust struct in prelude.rs
                    ConceptualRustType::Rust(RustIdent::new(raw.clone())).into()
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
            let value_desc = match fixed {
                FixedValue::Null => "null".to_owned(),
                FixedValue::Bool(b) => b.to_string(),
                FixedValue::Nint(i) => i.to_string(),
                FixedValue::Uint(u) => u.to_string(),
                FixedValue::Float(f) => format!("{f:?}"),
                FixedValue::Text(s) => format!("\"{s}\""),
            };
            self.record_rejection(format!(
                "rule `{alias}`: a top-level rule whose entire body is a bare fixed value ({value_desc}) \
                 is unsupported — a fixed value has no standalone type representation, only meaning as an \
                 (unstored) struct or array member. Wrap it in a group (e.g. `{alias} = [{value_desc}]`) \
                 or reference it from a member position."
            ));
        }
        self.type_aliases.insert(alias.into(), info);
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
        rust_struct: RustStruct,
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
                if self.scope(&rust_struct.ident).export() {
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
                if let Some(tag) = rust_struct.tag {
                    map_type = map_type.tag(tag);
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
                if let Some(tag) = rust_struct.tag {
                    array_type = array_type.tag(tag);
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
        self.rust_structs
            .insert(rust_struct.ident().clone(), rust_struct);
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
            // site) so `--no-synthesized-rust-collection-aliases` can suppress only it.
            if let Some(alias) = self.type_aliases.get_mut(&array_type_ident.into()) {
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
        for resolved_instance in resolved_generics {
            match resolved_instance {
                GenericResolved::Resolved(rs) => self.register_rust_struct(parent_visitor, rs, cli),
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
            if let RustStructType::Table { domain, .. } = rust_struct.variant() {
                domain.visit_types(self, &mut |ty| mark_key_demand(ty, &mut key_demand, bare));
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
    /// alias base types) via `visit_all_rust_types`. Three conflict classes:
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
    ///    loose `<Elem>List` builder: a plain non-exposable `[* elem]` mint or a map-key list
    ///    wrapper of the same element would reference a class of the wrong shape.
    fn non_empty_wrapper_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty shape + every loose-builder need from PLAIN array shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose <Elem>List classes needed by plain (non-`+`) uses: name -> a use description
        let mut plain_loose_needs: BTreeMap<String, String> = BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_array() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Array(elem) = &rt.conceptual_type
                && !rt.directly_wasm_exposable(self)
            {
                plain_loose_needs.insert(
                    elem.name_as_wasm_array(self),
                    "a plain (`*`-occurrence) array use".to_owned(),
                );
            }
            if let ConceptualRustType::Map(k, _v) = &rt.conceptual_type {
                // table wrappers mint a keys() list wrapper over the KEY type
                if !ConceptualRustType::Array(Box::new((**k).clone()))
                    .directly_wasm_exposable_ct(self)
                {
                    plain_loose_needs.insert(
                        k.name_as_wasm_array(self),
                        "a map keys() wrapper".to_owned(),
                    );
                }
            }
        });
        // named tables' keys() wrappers (Table structs aren't visited as Map RustTypes)
        for rs in self.rust_structs.values() {
            if let RustStructType::Table { domain, .. } = rs.variant()
                && !ConceptualRustType::Array(Box::new(domain.clone()))
                    .directly_wasm_exposable_ct(self)
            {
                plain_loose_needs.insert(
                    domain.name_as_wasm_array(self),
                    "a table keys() wrapper".to_owned(),
                );
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
                if let Some(need) = plain_loose_needs.get(&loose) {
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

        msgs.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the `{+ k => v}` (NonEmptyMap) emission would otherwise turn
    /// into a non-compiling wasm crate — the map-side twin of `non_empty_wrapper_name_collisions`.
    /// The loose table builder is always `MapKToV` (`name_for_wasm_map`); a map is never directly
    /// exposable, so (unlike arrays) the loose builder is ALWAYS the `try_from` source. Three classes:
    ///
    /// 1. An inline `{+ k => v}` with no named owner (see `non_empty_map_named_owner`) mints a
    ///    synthesized `NonEmptyMapKToV` class: a user rule claiming that ident collides.
    /// 2. A restricted wrapper (inline-synth or named non-self-named) needs the loose `MapKToV`
    ///    builder as its `try_from` source: a user rule claiming that ident with any shape OTHER than
    ///    a same-shape plain `{* k => v}` table rule (which IS the builder, shared) collides.
    /// 3. A self-named rule (`map_k_to_v = {+ k => v}` — the rule ident IS the loose-builder name)
    ///    legitimately claims the name for its RESTRICTED wrapper (it emits with no `try_from`;
    ///    construction is `new(first_key, first_value)` + `insert`), but then no OTHER use may need
    ///    the loose `MapKToV` builder: a plain `{* k => v}` use or an anonymous same-shape map would
    ///    reference a class of the wrong shape.
    fn non_empty_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();

        // collect every inline nonempty map shape + every loose-builder need from PLAIN map shapes
        let mut inline_non_empty: Vec<RustType> = Vec::new();
        // loose MapKToV classes needed by plain (non-`+`) uses: name -> a use description
        let mut plain_loose_needs: BTreeMap<String, String> = BTreeMap::new();
        self.visit_all_rust_types(&mut |rt| {
            if rt.is_non_empty_map() {
                inline_non_empty.push(rt.clone());
            } else if let ConceptualRustType::Map(k, v) = &rt.conceptual_type {
                plain_loose_needs.insert(
                    ConceptualRustType::name_for_wasm_map(k, v).to_string(),
                    "a plain (`*`-occurrence) map use".to_owned(),
                );
            }
        });
        // named plain tables mint their loose `MapKToV` class too (Table structs aren't visited as
        // Map RustTypes); exclude non-empty tables (their class is the restricted wrapper)
        for rs in self.rust_structs.values() {
            if let RustStructType::Table {
                domain,
                range,
                bounds,
            } = rs.variant()
                && *bounds != Some((Some(1), None))
            {
                plain_loose_needs.insert(
                    ConceptualRustType::name_for_wasm_map(domain, range).to_string(),
                    "a plain (`*`-occurrence) table rule".to_owned(),
                );
            }
        }

        // shared leg: the loose-builder need of a restricted map wrapper (synthesized or named)
        let check_loose_need =
            |key: &RustType, value: &RustType, needed_by: &str, msgs: &mut BTreeSet<String>| {
                let loose = ConceptualRustType::name_for_wasm_map(key, value).to_string();
                if self.wasm_ident_claimed_by_user_rule(&loose)
                    && !self.provides_compatible_loose_table(
                        &loose,
                        &key.clone().resolve_aliases(),
                        &value.clone().resolve_aliases(),
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
            let loose = ConceptualRustType::name_for_wasm_map(domain, range).to_string();
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
                    &format!("the named `{{+ …}}` rule '{ident}'"),
                    &mut msgs,
                );
            }
        }

        msgs.into_iter().collect()
    }

    pub fn visit_types<F: FnMut(&ConceptualRustType)>(&self, f: &mut F) {
        for rust_struct in self.rust_structs().values() {
            rust_struct.visit_types(self, f);
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

    pub fn print_info(&self) {
        if !self.plain_groups.is_empty() {
            println!("\n\nPlain groups:");
            for plain_group in self.plain_groups.iter() {
                println!("{}", plain_group.0);
            }
        }

        if !self.type_aliases.is_empty() {
            println!("\n\nAliases:");
            for (alias_name, alias_info) in self.type_aliases.iter() {
                println!("{alias_name:?} -> {alias_info:?}");
            }
        }

        if !self.generic_defs.is_empty() {
            println!("\n\nGeneric Definitions:");
            for (ident, def) in self.generic_defs.iter() {
                println!("{ident} -> {def:?}");
            }
        }

        if !self.generic_instances.is_empty() {
            println!("\n\nGeneric Instances:");
            for (ident, def) in self.generic_instances.iter() {
                println!("{ident} -> {def:?}");
            }
        }

        if !self.rust_structs.is_empty() {
            println!("\n\nRustStructs:");
            for (ident, rust_struct) in self.rust_structs.iter() {
                println!("{ident} -> {rust_struct:?}\n");
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

mod idents;
use crate::cli::Cli;
pub use idents::*;

mod structs;
pub use structs::*;
