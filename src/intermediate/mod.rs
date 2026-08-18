use cbor_event::{Special, Type as CBORType};
use cddl::ast::parent::ParentVisitor;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::comment_ast::{DemandSet, RuleMetadata};
use crate::parsing::EXTERN_MARKER;
use crate::utils::{
    cddl_prelude, convert_to_camel_case, convert_to_snake_case, is_identifier_reserved,
    is_identifier_user_defined, is_valid_rust_ident,
};

use std::sync::LazyLock;
pub static ROOT_SCOPE: LazyLock<ModuleScope> = LazyLock::new(|| vec![String::from("lib")].into());

fn rust_struct_kind(rust_struct: &RustStruct) -> &'static str {
    match rust_struct.variant() {
        RustStructType::Record(_) => "record",
        RustStructType::Table { .. } => "table",
        RustStructType::Array { .. } => "array",
        RustStructType::TypeChoice { .. } => "type-choice enum",
        RustStructType::GroupChoice { .. } => "group-choice enum",
        RustStructType::Wrapper { .. } => "wrapper",
        RustStructType::Extern => "extern marker",
        RustStructType::CStyleEnum { .. } => "C-style enum",
        RustStructType::RawBytesType => "raw-bytes marker",
    }
}

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
    /// the correct transparent representation. This preserves the original named source edge after
    /// the strip: the wasm alias can target the collection wrapper when it has one, and the
    /// recursive-type boundary can reconstruct the declared alias graph rather than mistaking a
    /// structural self-edge for the source edge. `None` = the rule body was not a stripped named
    /// alias.
    pub stripped_alias_target: Option<RustIdent>,
    /// `true` only for a generator-SYNTHESIZED collection wrapper's rust alias — currently the
    /// keys-list array a table rule mints (`create_and_register_array_type`). Distinguishes it from
    /// an authored `foo_list = [* foo]` / `tbl = { * a => b }`, which reach the same `new_manual`
    /// Array/Table registration arms and therefore CANNOT be told apart by `rule_metadata` (both are
    /// `None`). Gates `--no-synthesized-rust-collection-aliases`: rule-declared names always survive.
    pub synthesized_collection: bool,
    /// The rule this entry's WIRE-CODEC metadata (the `@custom_serialize`/`@custom_deserialize` pair
    /// and the `@custom_encodings`/`@custom_wire_major` declarations written beside it) was INHERITED
    /// from, when it was not written on this rule. `None` = the metadata is this rule's own.
    ///
    /// A registration seam cannot store the `Alias` node the emitter lifts a pair from (see
    /// `parsing::strip_alias_for_registration`), so the facts travel instead of the node. The
    /// provenance is what keeps the no-silent-directive checks honest across that travel: an
    /// inherited `@custom_wire_major` is not a directive anyone wrote HERE, so it is neither required
    /// to be consumed here nor counted as unconsumed, and consuming it through this entry counts as
    /// consuming the declaration at the rule that wrote it. Chains carry the ORIGIN rather than the
    /// previous link, so one hop always reaches the author.
    pub wire_metadata_inherited_from: Option<AliasIdent>,
}

impl AliasInfo {
    pub fn new_manual(base_type: RustType, gen_rust_alias: bool, gen_wasm_alias: bool) -> Self {
        Self {
            base_type,
            gen_rust_alias,
            gen_wasm_alias,
            rule_metadata: None,
            stripped_alias_target: None,
            synthesized_collection: false,
            wire_metadata_inherited_from: None,
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
            stripped_alias_target: None,
            synthesized_collection: false,
            wire_metadata_inherited_from: None,
        }
    }

    /// Whether this entry's wire is owned by a `@custom_serialize`/`@custom_deserialize` codec
    /// rather than by the aliased type's built-in one — the fact BOTH the routing and the
    /// projection key on, so they cannot disagree.
    ///
    /// Read off this entry's OWN `rule_metadata`, never off `wire_metadata_inherited_from`. That is
    /// not a shortcut: a rule that renames an annotated alias has the pair COPIED into its metadata
    /// at registration (`parsing::strip_alias_for_registration`) because the `Alias` node the
    /// emitters lift from is stripped there — so the metadata is where the facts actually live for
    /// own and inherited alike, and the provenance field records only WHO wrote them. Consulting
    /// the provenance instead would answer a different question (was this authored here?) than the
    /// one both consumers ask (does a codec own this wire?).
    ///
    /// Either half counts, though only complete pairs survive to generation — a lone half is a
    /// graceful rejection at the finalize seam. Keying on either is defense in depth: were a single
    /// half ever to reach here, it routes embed sites, and an entry that routes must not also
    /// project a standalone type whose codec contradicts it.
    pub fn carries_custom_pair(&self) -> bool {
        self.rule_metadata.as_ref().is_some_and(|metadata| {
            metadata.custom_serialize.is_some() || metadata.custom_deserialize.is_some()
        })
    }

    /// Whether the `Alias` wrapper NODE survives resolution. The node is not a projection — it is
    /// the routing key the serialize/deserialize emitters look the pair up by, and the source the
    /// enum-variant naming derives from — so a pair-carrying entry keeps it even though it emits no
    /// `pub type`. See [`Self::emits_rust_alias`] for the other half of that split.
    pub fn keeps_alias_node(&self) -> bool {
        self.gen_rust_alias || self.carries_custom_pair()
    }

    /// Whether a `pub type` line is emitted for this entry in the RUST crate, and therefore whether
    /// members/params/boundaries SPELL the alias name rather than the resolved base type.
    ///
    /// A pair-carrying alias emits none. The alias's standalone codec would be the aliased type's
    /// blanket impl — nothing per-alias is emitted for the pair to displace — while every embed
    /// site routes the pair, so keeping the name as a Rust type gives one CDDL name two wire forms,
    /// selected by whether a caller went through the standalone entry point or through a holder.
    /// Suppressing the projection removes the contradicting surface; the CDDL name still carries
    /// the wire facts, it just no longer names a Rust type.
    pub fn emits_rust_alias(&self) -> bool {
        self.gen_rust_alias && !self.carries_custom_pair()
    }

    /// The wasm-face twin of [`Self::emits_rust_alias`]. Separate stored flags, one shared
    /// suppression reason: the wasm `pub type` re-exposes the same contradicting standalone name.
    pub fn emits_wasm_alias(&self) -> bool {
        self.gen_wasm_alias && !self.carries_custom_pair()
    }

    /// Record that this entry's wire-codec metadata came from `origin` rather than from the rule's
    /// own comment. `None` leaves it as the rule's own (the constructors' default).
    pub fn with_inherited_wire_metadata(mut self, origin: Option<AliasIdent>) -> Self {
        self.wire_metadata_inherited_from = origin;
        self
    }

    pub fn with_stripped_alias_target(mut self, target: Option<RustIdent>) -> Self {
        self.stripped_alias_target = target;
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
        self.stripped_alias_target.as_ref().filter(|target| {
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
    // synthesized name never reaches the own-spec shape projection). Determinism: `BTreeSet`.
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
    /// The rules `crate::recursion_boundary` asked to be emitted as `@newtype` wrapper structs
    /// rather than transparent `pub type` aliases, because they are the collection-backed members of
    /// an alias-expansion cycle (rustc E0391). Seeded before parsing by `api::with_types`'s second
    /// build pass; empty on every spec with no such cycle, so byte-identical output is untouched.
    /// Determinism: `BTreeSet`, and the set itself is a canonical property of the cycle.
    auto_newtype_rules: BTreeSet<RustIdent>,
    // Idents of extern / raw-bytes rules tagged `@copy`: the externally-defined rust type derives
    // `Copy`, so `ConceptualRustType::is_copy` treats a `Rust(ident)` reference to one as Copy and
    // the generator drops the defensive boundary `.clone()`. The declaring crate emits a compile-time
    // `Copy` assertion for each (see `export.rs`), and the tag rides the extern-interface seam like
    // `@raw_bytes_flavor` so `--extern-import` consumers inherit it. See `RuleMetadata::copy`.
    copy_externs: BTreeSet<RustIdent>,
    // `@extern_companions` declarations, keyed by the LOCAL marker rule that carries one (either
    // user-supplied flavor — extern or raw-bytes): the sibling
    // wasm crate path plus the exact structural companion class names that already exist there. The
    // wasm wrapper-deferral decision (`try_defer_wrapper`) consults this to REFERENCE those classes
    // instead of minting duplicate `#[wasm_bindgen]` ones — keyed on IDENTS throughout, which is why
    // the raw-bytes flavor needed nothing here. Deliberately NOT part of
    // `RustStructConfig` for the same reason as `no_json_schema_export`: `RustStruct::new_extern` /
    // `new_raw_bytes` build with `RustStructConfig::default()` and drop rule metadata, and a marker
    // rule is this
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
    // Idents of rules tagged `@no_alias`. The directive's carrier is `AliasInfo::gen_rust_alias` /
    // `gen_wasm_alias`, which `AliasInfo::new_from_metadata` derives — but three rule kinds register
    // their own transparent alias through `AliasInfo::new_manual` instead, whose `rule_metadata` is
    // hardcoded `None`: a TABLE rule and an ARRAY typedef (registered from the `finalize` kind-walk,
    // where only the `RustStruct` is in scope) and a named binding to a generic SET NOMINAL
    // (registered from the generic-resolution arm, where only the resolved instance is in scope). On
    // all three the directive was silently dropped — the rule kept emitting the `pub type` it asks
    // to suppress. Recording the intent per-ident at the ONE parse seam that reads a rule's metadata,
    // and applying it in `register_type_alias`, makes every registration path honor it including
    // future ones, rather than adding a fourth place to remember. Rides the extern-interface seam:
    // a dep that suppresses its `pub type` must say so, or the consumer imports a name the dep no
    // longer materializes. Determinism: `BTreeSet`. See `RuleMetadata::no_alias`.
    no_alias_rules: BTreeSet<RustIdent>,
    // Rule-level `@doc` text, keyed by rule ident. The directive's ordinary carrier is
    // `RustStructConfig::doc` (for a rule that mints a struct) or `AliasInfo::rule_metadata` (for a
    // transparent alias), and two kinds reach NEITHER with their own metadata: a generic INSTANCE
    // binding (`foo = base<uint>`) mints a struct whose config is the generic DEFINITION's, and a
    // named binding to a generic SET NOMINAL registers its alias through `AliasInfo::new_manual`.
    // Both emitted a documentable construct while silently discarding the doc. Recorded at the same
    // parse seam as `no_alias_rules`, and applied where each construct is built. Determinism:
    // `BTreeMap`. See `RuleMetadata::comment`.
    rule_docs: BTreeMap<RustIdent, String>,
    // Idents whose rule carries `@custom_json`, keyed by rule ident. The directive's ordinary carrier
    // is `RustStructConfig::custom_json`, built from the rule's own metadata — and two struct-minting
    // kinds reach it with metadata that is not theirs: a generic INSTANCE binding
    // (`foo = base<uint>`) mints a struct whose config is the generic DEFINITION's, and a plain GROUP
    // rule's struct is built from `PlainGroupInfo`'s metadata, read off `comments_after_group` (empty
    // for the single-line spelling cddl actually binds to the last entry's trailing slot). Both
    // suppressed nothing while accepting the directive. Recorded at the same parse seams as
    // `no_alias_rules`/`rule_docs`, and applied in `register_rust_struct`. Determinism: `BTreeSet`.
    // See `RuleMetadata::custom_json`.
    custom_json_rules: BTreeSet<RustIdent>,
    // Every rule-position directive written on a plain GROUP rule, keyed by rule ident. A group is
    // only a type once some rule SPLICES it (`holder = [foo]`), and splicedness is a whole-spec
    // property — unknown at the parse seam that reads the directives, known by `finalize`, which
    // refuses the ones that landed on a group nothing splices. Determinism: `BTreeMap`, and the
    // per-ident directive list is already sorted by `RuleMetadata::all_directives`.
    plain_group_rule_directives: BTreeMap<RustIdent, Vec<&'static str>>,
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
    // Semantic nominal claims are retained before local minters can deduplicate or discard them.
    // The normal registration seam records the common case; exceptional pre-lookup minters call
    // the same ledger explicitly.
    nominal_mint_claims: BTreeMap<RustIdent, NominalMintClaim>,
    // Every choice's explicit reservations and settled derived names, keyed by its emitted-enum
    // context. This makes name allocation independent of arm order without a parser-local policy.
    variant_mint_claims: BTreeMap<String, Vec<VariantMintClaim>>,
    // Deferred rejections: constructs the parse walk (which returns `()` and so can't surface an
    // `Err`) recognizes as unsupported-by-design but must reject GRACEFULLY rather than `panic!`.
    // Each entry is a human-actionable message; `finalize` drains them into a single `Err` before
    // any resolution runs, so no later code operates on the incomplete IR left behind by a skipped
    // field. A `Vec` keeps insertion order deterministic (rule order is already deterministic).
    rejections: Vec<String>,
    // Every semantic rejection observation, including revisits of an AST node whose diagnostic was
    // already emitted. A parse branch uses this as a per-subwalk rejection signal: its inert
    // `Fixed(Null)` placeholder must remain rejected on every visit, even though the final error
    // reports that node only once. This is deliberately separate from `rejections`, whose length
    // is the number of user-visible diagnostic lines.
    rejection_observations: usize,
    // First-observation claims for diagnostics emitted by parse paths that can revisit one AST node
    // during classification and construction. The address is stable for the AST's in-run lifetime
    // and is never rendered or otherwise exposed; the kind keeps independent diagnostics at one
    // node distinct. `rejections` remains the ordered public result, so the ledger cannot affect
    // diagnostic order or emitted bytes beyond suppressing a repeated visit.
    diagnostic_node_claims: BTreeSet<(usize, &'static str)>,
    // for scope() to work we keep this here.
    // Returning a reference to the const ROOT_SCOPE complains of returning a temporary
    root_scope: ModuleScope,
}

impl Default for IntermediateTypes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// The imports needed by each scope, plus the named idents the wasm boundary actually uses.
///
/// `wasm_boundary_idents` deliberately records same-scope references too. Import placement needs
/// only cross-scope edges, but own-spec wasm extern/raw-bytes glue must know whether the emitted
/// wasm surface names its wrapper at all — a same-scope bare reference still needs that crate-root
/// re-export. Synthesized collection-wrapper idents may appear in the set; callers select only the
/// user-owned extern/raw-bytes candidates they can re-export.
#[derive(Debug, Default)]
pub struct ScopeReferences {
    pub imports: BTreeMap<ModuleScope, BTreeMap<ModuleScope, BTreeSet<RustIdent>>>,
    pub wasm_boundary_idents: BTreeSet<RustIdent>,
}

#[derive(Clone, Debug)]
struct NominalMintClaim {
    identity: String,
    site: String,
}

#[derive(Clone, Debug)]
pub(crate) struct VariantMintClaim {
    pub(crate) arm_ordinal: usize,
    pub(crate) source_name: String,
    emitted_name: String,
    explicit: bool,
    // A derived claim can settle as `Name2`, so `emitted_name` alone cannot distinguish an exact
    // AST revisit from a re-entry whose base derivation drifted.
    requested_base: Option<String>,
}

impl<'a> IntermediateTypes<'a> {
    // Inline choices need an address-qualified key so two independent anonymous namespaces do not
    // share reservations. An address is process-local, however, and must never reach a rejection:
    // diagnostics remain deterministic for identical CDDL input.
    fn variant_mint_context_for_diagnostic(context: &str) -> &str {
        context
            .strip_prefix("inline type choice at ")
            .map(|_| "an inline type choice")
            .unwrap_or(context)
    }

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
            auto_newtype_rules: BTreeSet::new(),
            copy_externs: BTreeSet::new(),
            extern_companions: BTreeMap::new(),
            no_json_schema_export: BTreeSet::new(),
            no_alias_rules: BTreeSet::new(),
            rule_docs: BTreeMap::new(),
            custom_json_rules: BTreeSet::new(),
            plain_group_rule_directives: BTreeMap::new(),
            raw_bytes_flavor: BTreeSet::new(),
            raw_bytes_flavor_emitted: BTreeSet::new(),
            rust_name_pins: BTreeMap::new(),
            scopes: BTreeMap::new(),
            rule_source_names: BTreeMap::new(),
            group_choice_arm_claims: BTreeMap::new(),
            nominal_mint_claims: BTreeMap::new(),
            variant_mint_claims: BTreeMap::new(),
            rejections: Vec::new(),
            rejection_observations: 0,
            diagnostic_node_claims: BTreeSet::new(),
            root_scope: ROOT_SCOPE.clone(),
        }
    }

    /// Release the one pre-registered `int` prelude marker when an authored rule uses the exact
    /// lowercase CDDL spelling `int`. This runs before any authored rule is parsed, so the rule may
    /// become the real `Int` owner; every other spelling that merely camel-cases to `Int` keeps the
    /// marker and reaches the ordinary incompatible-registration rejection.
    ///
    /// This is deliberately not part of `mark_source_rule_name`: source-name bookkeeping must not
    /// change ownership, and only `api::with_types` knows it is at the pre-parse lifecycle seam.
    pub fn release_pre_registered_int_marker_for_authored_lowercase_rule(&mut self) {
        let int = RustIdent::new(CDDLIdent::new("int"));
        let marker = self
            .rust_structs
            .remove(&int)
            .expect("IntermediateTypes::new must pre-register the built-in Int marker");
        assert!(
            matches!(marker.variant(), RustStructType::Extern),
            "only the pre-registered built-in Int marker may be released"
        );
    }

    /// Record a construct the parse walk rejects by design (it can't return an `Err` itself).
    /// `finalize` turns any accumulated rejections into a single graceful `Err`.
    pub fn record_rejection(&mut self, msg: String) {
        self.rejection_observations += 1;
        self.rejections.push(msg);
    }

    /// Record one semantic rejection observation at `node`, while emitting its diagnostic only on
    /// the first visit of that node/kind pair. Repeated classification/construction visits still
    /// count as rejected, so callers comparing [`Self::rejection_count`] around a sub-walk never
    /// mistake its inert placeholder for a real type.
    pub fn record_rejection_once_at<T>(&mut self, node: &T, kind: &'static str, msg: String) {
        self.rejection_observations += 1;
        if self.claim_diagnostic_node(node, kind) {
            self.rejections.push(msg);
        }
    }

    /// Claim the first observation of `kind` at one AST node. This is intentionally a claim-only
    /// seam: callers retain their own diagnostics, so two different messages at one node are not
    /// accidentally merged, and independently authored nodes with matching rendered text remain
    /// independently reportable.
    pub fn claim_diagnostic_node<T>(&mut self, node: &T, kind: &'static str) -> bool {
        self.diagnostic_node_claims
            .insert((node as *const T as usize, kind))
    }

    /// How many parse-walk rejections have been observed so far. Lets a caller tell whether a
    /// sub-walk it just ran rejected something even if a repeated AST visit suppresses a duplicate
    /// diagnostic: a rejected construct yields an INERT PLACEHOLDER type (`Fixed(Null)`) rather
    /// than the type it spelled, so any structural comparison against it compares placeholders —
    /// `[int] / [tstr]` would otherwise read as two identical arms.
    pub fn rejection_count(&self) -> usize {
        self.rejection_observations
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

    /// Whether `ident` names an alias whose TYPE PROJECTION is suppressed — it routes a custom
    /// (de)serializer pair, so it emits no `pub type` on either face and every position that would
    /// otherwise SPELL its name must spell the resolved base type instead.
    ///
    /// One predicate for both faces on purpose: the suppression reason is the pair, which is
    /// face-blind, so a rust member and a wasm member can never disagree about whether the name
    /// exists. The `Alias` NODE still survives resolution — this is only about the spelling; see
    /// [`AliasInfo::keeps_alias_node`] for the other half.
    pub fn alias_projection_suppressed(&self, ident: &RustIdent) -> bool {
        self.type_aliases
            .get(&AliasIdent::Rust(ident.clone()))
            .is_some_and(|info| info.carries_custom_pair())
    }

    /// Seed the rules `crate::recursion_boundary` decided to auto-`@newtype`, before any parsing.
    ///
    /// The repair is applied by RE-RUNNING the IR build with these marked rather than by rewriting
    /// a finalized `IntermediateTypes`: the whole point is that an auto-nominalized rule goes
    /// through the same machinery a spec-side `; @newtype` does, so its wasm wrapper, its
    /// preserve-encodings sidecars and its emit-tests minting exist without a second implementation
    /// — and so the emitted API is what the same spec with the directive written by hand produces.
    pub fn set_auto_newtype_rules(&mut self, rules: BTreeSet<RustIdent>) {
        self.auto_newtype_rules = rules;
    }

    /// Whether the recursive-type boundary asked for `ident` to be emitted as a wrapper struct
    /// rather than a transparent `pub type` alias. Read at the one seam a rule's directives are
    /// merged (`parsing::parse_type`).
    pub fn is_auto_newtype_rule(&self, ident: &RustIdent) -> bool {
        self.auto_newtype_rules.contains(ident)
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
            // A one-or-more open-array tail stores its inner type flat in `RestRow`; its composite
            // `NonEmptyVec<T>` is recovered by `RestRow::container_type`, so the generic type walk
            // deliberately does not see it. Keep runtime provisioning tied to that real container.
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Record(record)
                    if record.dynamic_rows().any(|row| row.is_non_empty_array_tail()))
            })
    }

    /// Whether any generated type uses a bounded homogeneous ARRAY occurrence. This mirrors the
    /// non-empty runtime gate and deliberately walks every IR position, including nested aliases.
    pub fn uses_bounded_vec(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_bounded_array());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(
                    rs.variant(),
                    RustStructType::Array { bounds: Some(bounds), .. }
                        if *bounds != (None, None)
                            && *bounds != (Some(1), None)
                            && rs.config().duplicates
                                != Some(crate::comment_ast::DuplicatesPolicy::Reject)
                )
            })
            // An open-array rest tail stores its element flat in RestRow, so the generic type walk
            // does not see the reconstructed BoundedVec container. Provision the runtime from the
            // same row-local occurrence that `RestRow::container_type` uses for emitted members.
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Record(record)
                    if record.dynamic_rows().any(|row| {
                        row.is_array_tail()
                            && row.is_restricted()
                            && !row.is_non_empty_array_tail()
                    }))
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
            // An open table's typed row stores its key/value flat in `RestRow`; its composite
            // `NonEmptyMap<K, V>` is recovered by `RestRow::container_type`, so the generic walk
            // deliberately cannot see it. Preserve rows are provisioned by `uses_pair_map` instead
            // (they use `NonEmptyPairMap`, never this runtime/import).
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Record(record)
                    if record.dynamic_rows().any(|row| {
                        row.is_non_empty()
                            && !row.is_array_tail()
                            && row.duplicates()
                                != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    }))
            })
    }

    /// Whether any owned type needs the finite/exact BoundedMap runtime.
    pub fn uses_bounded_map(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_bounded_map());
        found
            || self.rust_structs.values().any(|rs| matches!(
                rs.variant(),
                RustStructType::Table { bounds: Some(bounds), .. }
                    if *bounds != (None, None) && *bounds != (Some(1), None)
                        && rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
            ))
            // Dynamic map rows store their K/V types flat, so the generic walker intentionally
            // cannot see the BoundedMap composite. Recover it through the row's single carrier
            // source, mirroring the non-empty runtime gate above.
            || self.rust_structs.values().any(|rs| matches!(rs.variant(), RustStructType::Record(record)
                if record.dynamic_rows().any(|row| {
                    !row.is_array_tail()
                        && row.container_type().is_bounded_map()
                        && row.duplicates() != Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                })))
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
        self.visit_all_rust_types(&mut |rt| {
            found |= rt.contains_pair_map() || rt.contains_bounded_pair_map()
        });
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
                        if record.dynamic_rows().any(|r| {
                            r.duplicates() == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                        })
                )
            })
    }

    /// Whether a generated type needs the bounded preserve-table carrier. Kept distinct from
    /// `uses_pair_map` so loose preserve tables do not receive an unused `BoundedPairMap` import.
    pub fn uses_bounded_pair_map(&self) -> bool {
        let mut found = false;
        self.visit_all_rust_types(&mut |rt| found |= rt.contains_bounded_pair_map());
        found
            || self.rust_structs.values().any(|rs| {
                matches!(rs.variant(), RustStructType::Record(record)
                if record.dynamic_rows().any(|row| {
                    !row.is_array_tail() && row.container_type().is_bounded_pair_map()
                }))
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
            |rs| matches!(rs.variant(), RustStructType::Record(record) if record.captured_dynamic_rows().next().is_some()),
        )
    }

    /// Whether ANY generated record is an OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`). Gates
    /// the open-table fragments of the `open_struct_rest_json` runtime module — the hand-written
    /// serde pair's helpers and the two-range schema — so a crate with only ordinary rest rows keeps
    /// its output byte-identical. The module itself is already present for such a crate
    /// (`uses_open_struct_rest` counts an open table's two rows), which is why these are fragments
    /// of it rather than a module of their own: a new static module would oblige every
    /// `--export-static-crate` consumer to hand-add a `pub mod` line for a shape they may not use.
    pub fn uses_open_table(&self) -> bool {
        self.rust_structs.values().any(
            |rs| matches!(rs.variant(), RustStructType::Record(record) if record.is_open_table()),
        )
    }

    /// Whether ANY generated record carries a member that is BOTH optional and nullable
    /// (`? f: (T / null)`), whose rust member is therefore a nested `Option<Option<T>>`. Gates the
    /// standalone `double_option` runtime module (the `#[serde(with)]` adapter that keeps the JSON
    /// surface's absent / present-null / present-value distinction) under `--json-serde-derives`.
    ///
    /// The struct-field position is the WHOLE reachable universe for a nested `Option`: every other
    /// spelling collapses one of the two `Option`s before it reaches a serde surface — a table value
    /// / array element carries no presence-`Option` of its own (container membership is the presence
    /// bit), a wrapper body and a type-choice arm hold the nullable directly, and a group-choice arm
    /// either becomes a record of its own (this position) or is inlined into an enum variant, where
    /// the variant tag IS the presence bit (`can_embed_fields`, ≤1 non-fixed field). A member with a
    /// `.default` is not `Option`-wrapped at all, so it is excluded here exactly as it is at the
    /// emission site.
    pub fn uses_double_option(&self) -> bool {
        self.rust_structs.values().any(|rs| {
            matches!(rs.variant(), RustStructType::Record(record)
                if record.fields.iter().any(RustField::is_double_option))
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

    /// The authored bounded-array rule owning the wasm class for an inline occurrence with the
    /// identical element and inclusive window.  The bounds are part of the identity: unlike a
    /// loose list, `[2*5 T]` and `[2*6 T]` cannot share a class without losing the checked door.
    pub fn bounded_array_named_owner(
        &self,
        element: &RustType,
        bounds: (Option<i128>, Option<i128>),
    ) -> Option<&RustIdent> {
        let normalized = Self::normalized_bounded_array_window(bounds)?;
        let resolved = element.clone().resolve_aliases();
        self.rust_structs
            .iter()
            .find_map(|(ident, rs)| match rs.variant() {
                RustStructType::Array {
                    element_type,
                    bounds: Some(candidate),
                } if Self::normalized_bounded_array_window(*candidate) == Some(normalized)
                    // A reject-mode bounded rule owns an `OrderedSet` class, not the ordinary
                    // BoundedVec wasm class an inline preserve-policy occurrence needs. It cannot
                    // be a dedup owner without crossing incompatible core representations.
                    && rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Reject)
                    && !self.is_anonymous_collection_instance(ident)
                    && element_type.clone().resolve_aliases() == resolved =>
                {
                    Some(ident)
                }
                _ => None,
            })
    }

    /// The authored bounded-table rule owning the wasm class for an inline occurrence with the
    /// identical domain, range, and inclusive window. As with bounded arrays, the window is part
    /// of the identity: `{2*3 K => V}` cannot share a class with `{2*4 K => V}`.
    pub fn bounded_map_named_owner(
        &self,
        key: &RustType,
        value: &RustType,
        bounds: (Option<i128>, Option<i128>),
        preserve: bool,
    ) -> Option<&RustIdent> {
        let normalized = Self::normalized_bounded_map_window(bounds)?;
        let key_resolved = key.clone().resolve_aliases();
        let value_resolved = value.clone().resolve_aliases();
        self.rust_structs
            .iter()
            .find_map(|(ident, rs)| match rs.variant() {
                RustStructType::Table {
                    domain,
                    range,
                    bounds: Some(candidate),
                } if Self::normalized_bounded_map_window(*candidate) == Some(normalized)
                    && (rs.config().duplicates
                        == Some(crate::comment_ast::DuplicatesPolicy::Preserve))
                        == preserve
                    && !self.is_anonymous_collection_instance(ident)
                    && domain.clone().resolve_aliases() == key_resolved
                    && range.clone().resolve_aliases() == value_resolved =>
                {
                    Some(ident)
                }
                _ => None,
            })
    }

    /// Canonicalize an array occurrence window before comparing ownership or rendering a request:
    /// absent endpoints are the real `0` / unbounded values, and the two loose shapes are not
    /// bounded owners.  Keeping this here makes `[? T]`/`[0*1 T]` and `[*5 T]`/`[0*5 T]` one
    /// identity even though the parser preserves the source spelling.
    fn normalized_bounded_array_window(bounds: (Option<i128>, Option<i128>)) -> Option<(u64, u64)> {
        let min = u64::try_from(bounds.0.unwrap_or(0)).ok()?;
        let max = bounds
            .1
            .map(u64::try_from)
            .transpose()
            .ok()?
            .unwrap_or(u64::MAX);
        (min <= max && (min, max) != (0, u64::MAX) && (min, max) != (1, u64::MAX))
            .then_some((min, max))
    }

    /// Table-side occurrence-window canonicalization. `+` remains NonEmptyMap; all other non-loose
    /// unique-key table windows are BoundedMap owners.
    fn normalized_bounded_map_window(bounds: (Option<i128>, Option<i128>)) -> Option<(u64, u64)> {
        Self::normalized_bounded_array_window(bounds)
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
                    for rest in record.dynamic_rows() {
                        match &rest.kind {
                            RestKind::MapEntries { domain, range, .. } => {
                                walk(domain, f);
                                walk(range, f);
                            }
                            RestKind::ArrayTail { element, .. } => walk(element, f),
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

    /// Whether `name` is claimed by an exported source rule this crate owns — the namespace a
    /// synthesized wasm wrapper must not silently shadow. Read source-rule ownership from `scopes`,
    /// not the finalized struct/alias registries: those also contain generator-synthesized
    /// structural wrappers, and two compatible uses of one structural class must unify rather than
    /// mistake the first synthesis for a user claim when the second registers.
    fn wasm_ident_claimed_by_user_rule(&self, name: &str) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        self.is_toplevel_rule(&ident) && self.scope(&ident).export()
    }

    /// Whether rule `name` provides a COMPATIBLE loose list wrapper for `element_resolved`: only a
    /// genuinely unbounded default/preserve Array of that same element is the `Vec` builder a
    /// restricted wrapper may borrow. Bounded `BoundedVec` and reject `OrderedSet` rules share none
    /// of that representation, even if their bounds are not `[+]`.
    fn provides_compatible_loose_list(&self, name: &str, element_resolved: &RustType) -> bool {
        let ident = RustIdent::new(CDDLIdent::new(name));
        self.rust_structs.get(&ident).is_some_and(|rs| {
            matches!(
                rs.variant(),
                RustStructType::Array {
                    element_type,
                    bounds,
                } if matches!(bounds, None | Some((None, None)))
                    && element_type.clone().resolve_aliases() == *element_resolved
            ) && rs.config().duplicates != Some(crate::comment_ast::DuplicatesPolicy::Reject)
        })
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
                    && RustType::wasm_structural_map_name_for(domain, range, preserve, self).to_string()
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
    /// `wasm_structural_map_name_for` string (that string IS the shape identity). A shape owned by EXACTLY ONE
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
                let structural = RustType::wasm_structural_map_name_for(
                    domain,
                    range,
                    rust_struct.config().duplicates
                        == Some(crate::comment_ast::DuplicatesPolicy::Preserve),
                    self,
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
    pub(crate) fn wasm_collection_wrapper(
        &self,
        ty: &RustType,
        sole_owners: &BTreeMap<String, RustIdent>,
    ) -> Option<(RustIdent, ModuleScope)> {
        // LOOSE structural map (not reject/non-empty): its emission scope is the sole owner's module
        // when one exists (matching `mint_sole_owner_table`), else `types.scope` (root). Resolve it
        // before the `for_wasm_member` name below because the sole-owner indirection is invisible to
        // `types.scope` — the structural `MapKToV` name is never a registered scope.
        if let ConceptualRustType::Map(_, _) = &ty.conceptual_type
            && !ty.is_non_empty_map()
            && !ty.is_bounded_map()
        {
            // The occurrence's own carried policy selects the flavor (`MapKToV` / `PairMapKToV`) —
            // the same local signal `for_wasm_member` uses, so name resolution here and at the
            // emitter cannot disagree.
            let ident = ty.wasm_structural_map_name(self);
            let scope = sole_owners
                .get(&ident.to_string())
                .map(|owner| self.scope(owner).clone())
                .unwrap_or_else(|| self.scope(&ident).clone());
            return Some((ident, scope));
        }
        // Every remaining wrapper name resolves the same way `for_wasm_member` names it, and its home
        // is `types.scope(ident)` (root for a synthesized name, the owner's module for a dedup/rule
        // ident — a registered rust struct).
        let name = if ty.is_bounded_reject_ordered_set() {
            ty.bounded_reject_ordered_set_wasm_wrapper_name(self)
        } else if ty.is_reject_ordered_set() {
            ty.reject_ordered_set_wasm_wrapper_name(self)
        } else if ty.is_non_empty_array() {
            ty.non_empty_wasm_wrapper_name(self)
        } else if ty.is_bounded_array() {
            ty.bounded_wasm_wrapper_name(self)
        } else if ty.is_non_empty_map() {
            ty.non_empty_wasm_map_wrapper_name(self)
        } else if ty.is_bounded_map() {
            ty.bounded_wasm_map_wrapper_name(self)
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
    /// `requested` (wasm pass, `--wrapper-requests` host side) is every explicitly requested collection
    /// wrapper actually hosted into `requested_scope` this run as `(structural class ident, requested
    /// RustType)`. Those wrappers are
    /// NOT IR structs, so the struct walk below never sees them; after it runs, each is walked as if it
    /// were a rule emitted at `requested_scope` (mirroring the Array/Table struct-walk arms) so its body
    /// imports EXACTLY the cross-scope element / scoped-extern wasm classes it names. Empty (and
    /// `requested_scope` `None`) for the rust pass and whenever `--wrapper-requests` is unused, so output
    /// is byte-identical without the flag. `requested_hosted` is the full actual-mint ident set at that
    /// scope, including recursive support mints; all requested-scope same-file decisions use it.
    pub fn scope_references(
        &self,
        wasm: bool,
        deferred: &BTreeMap<RustIdent, ModuleScope>,
        requested: &[(RustIdent, RustType)],
        requested_hosted: &BTreeSet<RustIdent>,
        requested_scope: Option<&ModuleScope>,
    ) -> ScopeReferences {
        // we only want to mark TOP-LEVEL references without recursing into those types
        // which is why we don't use visit_types() here
        let mut refs = ScopeReferences::default();
        // Resolve wasm-map wrapper imports to the SAME module emission places them: a shape with a
        // sole owner is minted (class + structural alias) in that owner's module, everything else
        // falls back to the crate root. Computed once via the shared helper so the two sites can't drift.
        let table_shape_sole_owners = self.table_shape_sole_owners();
        fn set_ref(
            refs: &mut ScopeReferences,
            types: &IntermediateTypes,
            wasm: bool,
            current_scope: &ModuleScope,
            rust_ident: &RustIdent,
        ) {
            if wasm {
                refs.wasm_boundary_idents.insert(rust_ident.clone());
            }
            let ref_scope = types.scope(rust_ident);
            if current_scope != ref_scope {
                refs.imports
                    .entry(current_scope.clone())
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
            refs: &mut ScopeReferences,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            key: &RustType,
        ) {
            let keys_ident = RustIdent::new(CDDLIdent::new(
                key.loosened_for_wasm_table_boundary_key()
                    .name_as_wasm_array(types),
            ));
            if let Some(dep_scope) = deferred.get(&keys_ident) {
                refs.imports
                    .entry(emit_scope.to_owned())
                    .or_default()
                    .entry(dep_scope.clone())
                    .or_default()
                    .insert(keys_ident);
            }
        }
        // Register the import of a DEFERRED loose LIST wrapper that a locally-minted restricted
        // wrapper (`NonEmpty*List`, `BoundedVec`, or a named restricted rule's class) borrows as its `try_from`
        // source. The `try_from(&<Elem>List)` reference is conversion-internal — invisible to the
        // field walk, the same class of problem as a map's `keys()`-list
        // (`register_deferred_keys_list`), solved the same way: follow the CLASS, not the using
        // site — import at the restricted wrapper's EMISSION scope, from the dep's `collections`
        // module. No-op when: a bare `Vec` of the element crosses the ABI (`try_from` takes that
        // `Vec`, no loose class is named) or the element is itself non-empty (no loose source
        // exists — built incrementally); the
        // loose name equals the wrapper ident (a self-named rule emits no `try_from`); or the
        // loose wrapper is not deferred (it is a local class in the same scope). Empty `deferred`
        // (rust pass / flag unused) makes this a no-op, so output is byte-identical without the flag.
        fn register_deferred_restricted_list_source(
            refs: &mut ScopeReferences,
            types: &IntermediateTypes,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            wrapper_ident: &RustIdent,
            elem: &RustType,
            bounded_outer: bool,
        ) {
            if elem.vec_of_self_directly_wasm_exposable(types)
                || (!bounded_outer && (elem.is_non_empty_array() || elem.is_bounded_array()))
            {
                return;
            }
            let loose = elem.name_as_wasm_array(types);
            if loose == wrapper_ident.as_ref() {
                return;
            }
            let loose_ident = RustIdent::new(CDDLIdent::new(loose));
            if let Some(dep_scope) = deferred.get(&loose_ident) {
                let emit_scope = types.scope(wrapper_ident).clone();
                refs.imports
                    .entry(emit_scope)
                    .or_default()
                    .entry(dep_scope.clone())
                    .or_default()
                    .insert(loose_ident);
            }
        }
        // The map twin of `register_deferred_restricted_list_source`: a locally-minted restricted
        // map class enters via `try_from(&MapKToV)` — when that loose structural table wrapper is
        // deferred, import it at the restricted wrapper's emission scope. The caller passes the
        // exact SOURCE key: native for `{+ …}`, top-level-loosened for a bounded table. Additional
        // no-op case: the loose shape has a SOLE table-rule owner — the `try_from` source is then
        // the owner's local `pub type MapKToV = <Owner>;` alias, never a deferred class.
        #[allow(clippy::too_many_arguments)]
        fn register_deferred_restricted_map_source(
            refs: &mut ScopeReferences,
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
            let loose_ident = RustType::wasm_structural_map_name_for(key, value, preserve, types);
            if loose_ident.as_ref() == wrapper_ident.as_ref()
                || sole_owners.contains_key(&loose_ident.to_string())
            {
                return;
            }
            if let Some(dep_scope) = deferred.get(&loose_ident) {
                let emit_scope = types.scope(wrapper_ident).clone();
                refs.imports
                    .entry(emit_scope)
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
            refs: &mut ScopeReferences,
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
            let keys_ident = RustIdent::new(CDDLIdent::new(
                key.loosened_for_wasm_table_boundary_key()
                    .name_as_wasm_array(types),
            ));
            // deferred keys-lists live in a dep's `collections` module — imported by the deferred
            // helper, not from root
            if deferred.contains_key(&keys_ident) {
                return;
            }
            refs.imports
                .entry(emit_scope.to_owned())
                .or_default()
                .entry(ROOT_SCOPE.clone())
                .or_default()
                .insert(keys_ident);
        }
        // The non-deferred analogue of `register_deferred_restricted_list_source`: a restricted list
        // wrapper (`NonEmpty*List`, `BoundedVec`, a named restricted rule, or a dedup owner) emitted at `emit_scope`
        // borrows a LOOSE `<Elem>List` as its `try_from` source, and that loose builder is a locally
        // minted class (typically ROOT-minted). Its `try_from(&<Elem>List)` names the loose builder
        // bare in `emit_scope`, so import it there — the list twin of `register_root_keys_list`. Also
        // register the loose builder's OWN element ref at the builder's scope (its `get`/`add`
        // accessors name the element bare where the builder lives). No-op when: a bare `Vec` of the
        // element crosses the ABI (`try_from` takes that `Vec`, no loose class) or the element is
        // itself non-empty (built
        // incrementally, no loose source); the loose name equals the wrapper ident (a self-named rule
        // emits no `try_from`); or the loose builder is deferred (the deferred helper imports it from
        // the dep's `collections` module instead).
        #[allow(clippy::too_many_arguments)]
        fn register_root_restricted_list_source(
            refs: &mut ScopeReferences,
            types: &IntermediateTypes,
            wasm: bool,
            sole_owners: &BTreeMap<String, RustIdent>,
            deferred: &BTreeMap<RustIdent, ModuleScope>,
            emit_scope: &ModuleScope,
            wrapper_ident: &RustIdent,
            elem: &RustType,
            bounded_outer: bool,
        ) {
            if !wasm
                || elem.vec_of_self_directly_wasm_exposable(types)
                || (!bounded_outer && (elem.is_non_empty_array() || elem.is_bounded_array()))
            {
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
                refs.imports
                    .entry(emit_scope.to_owned())
                    .or_default()
                    .entry(loose_scope.clone())
                    .or_default()
                    .insert(loose_ident);
            }
            mark_refs(refs, types, wasm, sole_owners, deferred, &loose_scope, elem);
        }
        // The map twin of `register_root_restricted_list_source`: a restricted map wrapper emitted
        // at `emit_scope` enters via `try_from(&MapKToV)`, naming the LOOSE structural table wrapper
        // bare in `emit_scope`. The caller passes the exact SOURCE key: native for `{+ …}`,
        // top-level-loosened for a bounded table. Import it here, resolving the loose builder's own
        // home the SAME way emission places it (`table_shape_sole_owners`: the owner's
        // `pub type MapKToV = <Owner>;` module when a sole owner exists, else root). Also register
        // the loose builder's key/value refs at its scope. No-op when the loose name equals the
        // wrapper ident (self-named rule) or the loose builder is deferred.
        #[allow(clippy::too_many_arguments)]
        fn register_root_restricted_map_source(
            refs: &mut ScopeReferences,
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
            let loose_ident = RustType::wasm_structural_map_name_for(key, value, preserve, types);
            if loose_ident.as_ref() == wrapper_ident.as_ref() || deferred.contains_key(&loose_ident)
            {
                return;
            }
            let loose_scope = sole_owners
                .get(&loose_ident.to_string())
                .map(|owner| types.scope(owner).clone())
                .unwrap_or_else(|| types.scope(&loose_ident).clone());
            if loose_scope != *emit_scope {
                refs.imports
                    .entry(emit_scope.to_owned())
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
            refs: &mut ScopeReferences,
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
                        // A named COLLECTION rule whose ident COINCIDES with a structural wrapper
                        // name a mapped dependency's `--extern-wrapper-index` lists is DEFERRED at
                        // mint time: no class of that name exists locally, so `set_ref`'s
                        // same-scope no-op would leave every by-name reference naming an undefined
                        // type (E0425). Route the import from the dep's `collections` module into
                        // EVERY using scope, root included — the same rule the structural
                        // Array/Map arms below apply, which is the point: the named and inline
                        // reference positions must agree about where a deferred wrapper lives.
                        // Recursion stays suppressed (wrapper and element are both the
                        // dependency's), as it is for the local-rule case just below. `deferred` is
                        // empty for the rust pass and whenever the flag families are unused, so
                        // output is byte-identical without the flag.
                        if let Some(dep_scope) = deferred.get(rust_ident) {
                            refs.imports
                                .entry(current_scope.to_owned())
                                .or_default()
                                .entry(dep_scope.clone())
                                .or_default()
                                .insert(rust_ident.clone());
                            return;
                        }
                        set_ref(refs, types, wasm, current_scope, rust_ident);
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
                // No deferred consult here, unlike the Alias arm above: an ident can only enter
                // `deferred` from `try_defer_wrapper`, whose `wrapper_ident` is either a
                // synthesized structural wrapper name or the ident of a rust struct whose variant
                // is `Array` or `Table` — and `register_rust_struct` gives BOTH of those variants a
                // transparent type alias, so every by-name reference to a deferrable ident arrives
                // as `Alias(Rust(ident), …)` and is handled there. A bare `Rust(ident)` is a
                // Record / choice / Wrapper struct, none of which is ever a defer candidate.
                ConceptualRustType::Rust(rust_ident) => {
                    set_ref(refs, types, wasm, current_scope, rust_ident)
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
                            refs.imports
                                .entry(current_scope.to_owned())
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
                            refs.imports
                                .entry(current_scope.to_owned())
                                .or_default()
                                .entry(emit_scope.clone())
                                .or_default()
                                .insert(wrapper.clone());
                        }
                        // A RESTRICTED wrapper (`[+ …]` / `@duplicates reject`) borrows a LOOSE
                        // `<Elem>List` as its `try_from` source, named bare in its emission scope —
                        // import it there (deferred + non-deferred analogues).
                        if ty.is_non_empty_array()
                            || ty.is_bounded_array()
                            || ty.is_reject_ordered_set()
                        {
                            register_deferred_restricted_list_source(
                                refs,
                                types,
                                deferred,
                                &wrapper,
                                elem_ty,
                                ty.is_bounded_array(),
                            );
                            register_root_restricted_list_source(
                                refs,
                                types,
                                wasm,
                                sole_owners,
                                deferred,
                                &emit_scope,
                                &wrapper,
                                elem_ty,
                                ty.is_bounded_array(),
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
                            refs.imports
                                .entry(current_scope.to_owned())
                                .or_default()
                                .entry(dep_scope.clone())
                                .or_default()
                                .insert(wrapper);
                            return;
                        }
                        if emit_scope != *current_scope {
                            refs.imports
                                .entry(current_scope.to_owned())
                                .or_default()
                                .entry(emit_scope.clone())
                                .or_default()
                                .insert(wrapper.clone());
                        }
                        // A restricted map wrapper enters via `try_from(&MapKToV)`, naming the loose
                        // structural table wrapper bare in its emission scope. `{+ …}` uses its
                        // native key; a bounded table uses its deliberately loosened direct key.
                        if ty.is_non_empty_map() || ty.is_bounded_map() {
                            let source_key = if ty.is_bounded_map() {
                                key.loosened_for_wasm_table_boundary_key()
                            } else {
                                (**key).clone()
                            };
                            register_deferred_restricted_map_source(
                                refs,
                                types,
                                deferred,
                                sole_owners,
                                &wrapper,
                                &source_key,
                                value,
                                ty.is_preserve_pair_map(),
                            );
                            register_root_restricted_map_source(
                                refs,
                                types,
                                wasm,
                                sole_owners,
                                deferred,
                                &emit_scope,
                                &wrapper,
                                &source_key,
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
                    // `try_from(&<Elem>List)` source names that builder bare in THIS scope. Three rule
                    // families do so: a restricted `[+ …]` rule (`generate_non_empty_array_type`), an
                    // ordinary/preserve bounded rule (`generate_bounded_array_type`), and a
                    // `@duplicates reject` rule of ANY bounds (`generate_reject_ordered_set_type`) —
                    // a plain `[*] reject` set still enters through `try_from(&FooList)`, so gating on
                    // the non-empty bound alone left its loose-source import (at the rule's scope) and
                    // the loose builder's element ref (at ROOT, its emission scope) unregistered
                    // (E0425 on both `FooList` and its element). The two helpers below apply the
                    // element-exposable / non-empty-element / self-named / deferred guards that decide
                    // whether a loose source actually exists, so a plain non-reject `[* foo]` rule
                    // (whose class wraps `Vec<Foo>` directly, no `try_from` source) is correctly a
                    // no-op even when it reaches here.
                    // LOCKSTEP: this gate mirrors the three restricted emitters' `loose_list`
                    // decisions — a reject rule emits `try_from(&Loose)` regardless of its `[*]`/`[+]`
                    // bound, while a bounded outer also does so over a constrained element. Change
                    // them together.
                    // A rule whose own wrapper DEFERRED (index mode unifying a rule ident with an
                    // indexed structural name) emits no class and therefore no `try_from` — it still
                    // reaches this gate, and the two helpers' own guards plus the usage-derived
                    // import prune leave it importing nothing, so the gate stays keyed on the rule's
                    // SHAPE rather than on a placement decision made in the generator.
                    let bounded = rust_struct.config().duplicates
                        != Some(crate::comment_ast::DuplicatesPolicy::Reject)
                        && matches!(
                            bounds,
                            Some(window) if *window != (None, None) && *window != (Some(1), None)
                        );
                    if wasm
                        && (*bounds == Some((Some(1), None))
                            || bounded
                            || rust_struct.config().duplicates
                                == Some(crate::comment_ast::DuplicatesPolicy::Reject))
                    {
                        // The deferred (`--extern-wrapper-index`) analogue: when the loose `<Elem>List`
                        // is owned by a mapped dependency, import it from the dep's `collections`
                        // module at this rule's scope. Routed for reject rules too, by the same
                        // same-condition principle (a reject rule over a dep-owned element defers its
                        // loose source exactly as a non-empty rule does); a no-op when `deferred` is
                        // empty, so output is byte-identical without the flag.
                        register_deferred_restricted_list_source(
                            &mut refs,
                            self,
                            deferred,
                            &rust_struct.ident,
                            element_type,
                            bounded,
                        );
                        // The non-deferred analogue: the loose `<Elem>List` is a locally (ROOT-)
                        // minted class the rule's `try_from(&<Elem>List)` names bare in THIS scope,
                        // so import it here (E0425 otherwise). Fixes the `necollrec` and `rsetrec`
                        // cells.
                        register_root_restricted_list_source(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rust_struct.ident,
                            element_type,
                            bounded,
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
                    // which is what this did. The wasm record-level `insert_<row>` operation is the
                    // one extra consumer: unlike the snapshot getter it names K and V directly in
                    // the OWNER's signature, so map-row inners are also marked at the owner scope
                    // below. That direct mark is load-bearing for multifile and deferred extern
                    // rows whose wrapper class lives somewhere else.
                    //
                    // The one row that does NOT go through its container is an open table's TYPED
                    // row under wasm: it has no container CLASS to import, because its map surface
                    // is flattened onto this struct's own class. The `keys()` that names the
                    // keys-list wrapper bare is therefore a method of THIS class, in THIS scope —
                    // the named-Table arm's situation, so it takes that arm's registration verbatim
                    // (root-minted `<K_t>List`, or the dep import when deferred). Routing it
                    // through the container arm instead registered the list at the container's
                    // would-be emission scope and left the struct's own `keys()` naming an
                    // unimported class (E0425 in a non-root module).
                    for rest in record.dynamic_rows() {
                        if wasm && record.is_typed_row(rest) && !rest.is_array_tail() {
                            register_deferred_keys_list(
                                &mut refs,
                                self,
                                deferred,
                                current_scope,
                                rest.domain(),
                            );
                            register_root_keys_list(
                                &mut refs,
                                self,
                                wasm,
                                deferred,
                                current_scope,
                                rest.domain(),
                            );
                            // The struct's field type and its flattened accessors name `K_t`/`V_t`
                            // bare right here, so they are marked at THIS scope rather than at a
                            // container's.
                            for inner in [rest.domain(), rest.range()] {
                                mark_refs(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    current_scope,
                                    inner,
                                );
                            }
                            // The keys-list class ITSELF names `K_t` bare in its own body
                            // (`get`/`add`), and it is minted at root rather than registered as an
                            // IR struct — so no struct-walk arm ever reaches it. A named table gets
                            // this for free (its keys-list IS an IR Array struct, minted at parse);
                            // mark the element at the list's scope here on the same condition
                            // `register_root_keys_list` mints under.
                            let loose_domain = rest.domain().loosened_for_wasm_table_boundary_key();
                            let keys_ident = RustIdent::new(CDDLIdent::new(
                                loose_domain.name_as_wasm_array(self),
                            ));
                            if !ConceptualRustType::Array(Box::new(rest.domain().clone()))
                                .directly_wasm_exposable_ct(self)
                                && !deferred.contains_key(&keys_ident)
                            {
                                mark_refs(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    &ROOT_SCOPE,
                                    &loose_domain,
                                );
                            }
                            // Bounded typed rows keep their checked carrier flattened on this
                            // record, but their fallible wasm `new` takes a loose same-flavor
                            // structural builder. Mark only that auxiliary builder: it imports the
                            // class and its key/value references into THIS owner scope without
                            // pretending the forbidden restricted whole-row class exists.
                            if rest.container_type().bounded_map_u64_bounds().is_some() {
                                let builder = rest.staging_container_type();
                                mark_refs(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    current_scope,
                                    &builder,
                                );
                            }
                            continue;
                        }
                        mark_refs(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rest.container_type(),
                        );
                        if wasm && !rest.is_array_tail() {
                            for inner in [rest.domain(), rest.range()] {
                                mark_refs(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    current_scope,
                                    inner,
                                );
                            }
                        }
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
                    // A named restricted table's class borrows a LOOSE structural `MapKToV` as its
                    // `try_from` source; when that source is deferred, import it at THIS rule's
                    // scope. `{+ …}` uses its native direct key; a bounded table uses the same
                    // top-level-loosened key as `generate_bounded_map_type`.
                    let bounded_source = bounds.is_some_and(|candidate| {
                        Self::normalized_bounded_map_window(candidate).is_some()
                    });
                    if wasm && (*bounds == Some((Some(1), None)) || bounded_source) {
                        // the rule's own `@duplicates` config picks its container flavor, so the
                        // `try_from` source resolved below is the loose wrapper of the SAME flavor
                        let preserve = rust_struct.config().duplicates
                            == Some(crate::comment_ast::DuplicatesPolicy::Preserve);
                        let source_key = if bounded_source {
                            domain.loosened_for_wasm_table_boundary_key()
                        } else {
                            domain.clone()
                        };
                        register_deferred_restricted_map_source(
                            &mut refs,
                            self,
                            deferred,
                            &table_shape_sole_owners,
                            &rust_struct.ident,
                            &source_key,
                            range,
                            preserve,
                        );
                        // The non-deferred analogue: the loose `MapKToV` builder is a locally
                        // (ROOT- or sole-owner-) minted class the rule's `try_from(&MapKToV)` names
                        // bare in THIS scope, so import it here (E0425 otherwise). Fixes the
                        // `nemap`/`nepmap`/`nepmapa` cells.
                        register_root_restricted_map_source(
                            &mut refs,
                            self,
                            wasm,
                            &table_shape_sole_owners,
                            deferred,
                            current_scope,
                            &rust_struct.ident,
                            &source_key,
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
        // Their import map records only CROSS-scope idents, so single-module output is
        // byte-identical; in the wasm pass the separate boundary-use set also retains same-scope
        // names for own-spec extern glue.
        for (alias_ident, alias_info) in self.type_aliases() {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            let emitted_this_pass = if wasm {
                alias_info.emits_wasm_alias()
            } else {
                alias_info.emits_rust_alias()
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
                        refs.imports
                            .entry(current_scope.clone())
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
                    refs.imports
                        .entry(current_scope.clone())
                        .or_default()
                        .entry(dep_scope.clone())
                        .or_default()
                        .insert(target.clone());
                } else {
                    set_ref(&mut refs, self, wasm, current_scope, target);
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
            // Mark the ref a hosted wrapper's body names for one member (element / key / value). A member
            // that is ITSELF a hosted requested collection lives in `requested_scope` too (same file):
            // its wasm class is named bare with nothing to import, and its own body is walked when the
            // loop reaches its entry — so skip it rather than let `wasm_collection_wrapper` misroute the
            // structural name to the crate root (`types.scope` doesn't know the requested wrappers). Every
            // other member routes through the shared `mark_refs`, resolving to the member's true home.
            let mark_requested_member = |refs: &mut ScopeReferences, member: &RustType| {
                if let Some((wrapper, _)) =
                    self.wasm_collection_wrapper(member, &table_shape_sole_owners)
                    && requested_hosted.contains(&wrapper)
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
                        if rt.is_non_empty_array()
                            || rt.is_bounded_array()
                            || rt.is_reject_ordered_set()
                        {
                            register_deferred_restricted_list_source(
                                &mut refs,
                                self,
                                deferred,
                                wid,
                                elem,
                                rt.is_bounded_array(),
                            );
                            let loose =
                                RustIdent::new(CDDLIdent::new(elem.name_as_wasm_array(self)));
                            if !requested_hosted.contains(&loose) {
                                register_root_restricted_list_source(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    req_scope,
                                    wid,
                                    elem,
                                    rt.is_bounded_array(),
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
                        let keys_ident = RustIdent::new(CDDLIdent::new(
                            key.loosened_for_wasm_table_boundary_key()
                                .name_as_wasm_array(self),
                        ));
                        if !requested_hosted.contains(&keys_ident) {
                            register_root_keys_list(
                                &mut refs, self, wasm, deferred, req_scope, key,
                            );
                        }
                        // A restricted map borrows a LOOSE `MapKToV` as its `try_from` source named
                        // bare at the emission scope — same requested-source guard as the list arm.
                        // Bounded maps use the emitter's top-level-loosened direct key.
                        if rt.is_non_empty_map() || rt.is_bounded_map() {
                            let source_key = if rt.is_bounded_map() {
                                key.loosened_for_wasm_table_boundary_key()
                            } else {
                                (**key).clone()
                            };
                            register_deferred_restricted_map_source(
                                &mut refs,
                                self,
                                deferred,
                                &table_shape_sole_owners,
                                wid,
                                &source_key,
                                value,
                                rt.is_preserve_pair_map(),
                            );
                            let loose = RustType::wasm_structural_map_name_for(
                                &source_key,
                                value,
                                rt.is_preserve_pair_map(),
                                self,
                            );
                            if !requested_hosted.contains(&loose) {
                                register_root_restricted_map_source(
                                    &mut refs,
                                    self,
                                    wasm,
                                    &table_shape_sole_owners,
                                    deferred,
                                    req_scope,
                                    wid,
                                    &source_key,
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
            "undefined",
            ConceptualRustType::Fixed(FixedValue::Undefined).into(),
        );
        insert_alias(
            "true",
            ConceptualRustType::Fixed(FixedValue::Bool(true)).into(),
        );
        insert_alias(
            "false",
            ConceptualRustType::Fixed(FixedValue::Bool(false)).into(),
        );
        // `float` is UNCONSTRAINED (`float = float16-32 / float64`, RFC 8610 App. D): it is its
        // own value class — every float value — NOT an alias of `float64`, which holds only the
        // values needing all eight bytes. Sharing one identity is what made them indistinguishable.
        insert_alias(
            "float",
            ConceptualRustType::Primitive(Primitive::Float).into(),
        );
        insert_alias(
            "float64",
            ConceptualRustType::Primitive(Primitive::F64).into(),
        );
        insert_alias(
            "float32",
            ConceptualRustType::Primitive(Primitive::F32).into(),
        );
        // The head-CONSTRAINED names. `float16`'s carrier is `f32` (every `#7.25` value widens into
        // it exactly); the two union names carry the widest of their members'.
        insert_alias(
            "float16",
            ConceptualRustType::Primitive(Primitive::F16).into(),
        );
        insert_alias(
            "float16-32",
            ConceptualRustType::Primitive(Primitive::F16To32).into(),
        );
        insert_alias(
            "float32-64",
            ConceptualRustType::Primitive(Primitive::F32To64).into(),
        );
        // What about bingint/other stuff in the standard prelude?
        aliases
    }

    /// The alias-substitution rule: a REGISTERED alias resolves to its base type, kept behind an
    /// `Alias` wrapper when the alias [keeps its node](AliasInfo::keeps_alias_node) — because it
    /// emits a rust type, OR because it carries a custom pair the emitters route through that node
    /// (the wrapper is what preserves the alias's name for naming derivations and for the pair
    /// lookup) — and substituted transparently when it doesn't; an unregistered ident is `None`
    /// (the caller decides the fallback). Note the two are no longer the same question: a
    /// pair-carrying alias keeps the node while emitting no `pub type`. This is the ONE
    /// owner of that rule: `new_type` (the canonical pipeline constructor) and the
    /// `--wrapper-requests` shape parser (generation/requests.rs `parse_shape_fragment`) both call it, so a
    /// leaf built outside the pipeline cannot drift from pipeline resolution — the drift is exactly
    /// how alias-element requests once panicked `is_enum`'s registered-struct invariant (pinned by
    /// `workspace_requests_alias_elements_host`). Immutable on purpose: prelude emission for
    /// unregistered reserved idents is `new_type`'s fallback, not part of the rule.
    pub fn resolve_alias(&self, alias_ident: &AliasIdent) -> Option<RustType> {
        self.type_aliases.get(alias_ident).map(|info| {
            if info.keeps_alias_node() {
                info.base_type.clone().as_alias(alias_ident.clone())
            } else {
                info.base_type.clone()
            }
        })
    }

    /// **The refusal inventory**: every prelude name [`Self::new_type`]'s interception arms REFUSE
    /// (`record_rejection` + an inert placeholder), as opposed to resolving to a type. Sorted, so
    /// the list reads as a set.
    ///
    /// This is a constant rather than a shape the arms spell inline because a refusal recorded at
    /// ONE resolution seam does not bind the others: a name refused at this seam can still reach
    /// generation through a seam that never calls `new_type` (a control-operator head resolves its
    /// ident through `parsing::ident_to_primitive` — the narrower-float-name delivery needed a fix
    /// at each). The closure sweep `tests::refused_name_closure_tests` runs this list against its
    /// resolution-context registry, and keeps it honest in BOTH directions: it re-derives the
    /// inventory by probing the whole [`crate::utils::RESERVED_IDENTS`] universe, so a new refusal
    /// arm fails that derivation until the name is added here, and adding it here demands cells in
    /// every context. A name refused for its SHAPE (a recursion cycle, an inline composite) is not
    /// a member — this axis is name-keyed refusals only.
    ///
    /// `dead_code`-allowed because its only consumer is `#[cfg(test)]`-gated (the sweep), the same
    /// shape `wrapper_requests::BORROWED_SHAPES` uses: the constant belongs BESIDE the arms it
    /// describes, not in the test module that reads it — a list living in the test tree is exactly
    /// the mirror this design exists to avoid.
    #[allow(dead_code)]
    pub const REFUSED_PRELUDE_NAMES: &'static [&'static str] = &["cbor-any"];

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
                // `cbor-any` (#6.55799(any)) is a self-described STREAM marker, not an ordinary
                // value tag: it says the entire serialized item stream is CBOR. It therefore has
                // no value-wrapper representation. Intercept it HERE rather than in
                // `cddl_prelude` because this fallback is the seam every ordinary type position
                // funnels through, a registered user alias still resolves above it, and this
                // `IntermediateTypes` handle is where the role-neutral permanent-exclusion
                // diagnostic can be recorded. The `Fixed(FixedValue::Null)` placeholder is the
                // inert stand-in the sibling rejections use, so the walk continues and `finalize`
                // reports this alongside anything else it finds.
                AliasIdent::Reserved(reserved)
                    if Self::REFUSED_PRELUDE_NAMES.contains(&reserved.as_str()) =>
                {
                    self.record_rejection(format!(
                        "the CDDL prelude type `{reserved}` (#6.55799(any)) is unsupported — \
                         Support for `cbor-any` is permanently excluded: the self-describe tag \
                         marks a byte stream as CBOR, which is a property of the stream and not \
                         of any value a generated type could hold."
                    ));
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

    pub fn register_type_alias(&mut self, alias: RustIdent, mut info: AliasInfo) {
        // `@no_alias` is enforced HERE rather than at each constructor, so a registration path that
        // builds its `AliasInfo` without the rule's metadata (`new_manual`: the table/array
        // kind-walk, a named binding to a generic set nominal) honors the directive too. Idempotent
        // for `new_from_metadata`, which already derived both flags from the same bit.
        if self.no_alias_rules.contains(&alias) {
            info.gen_rust_alias = false;
            info.gen_wasm_alias = false;
        }
        if let ConceptualRustType::Alias(_ident, _ty) = &info.base_type.conceptual_type {
            panic!(
                "register_type_alias*({}, {:?}) wraps automatically in Alias, no need to provide it.",
                alias, info.base_type
            );
        }
        Self::assert_no_wire_facts_survive_a_transparent_alias(&alias, &info.base_type);
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
        // `@custom_json` is consumed EXCLUSIVELY through `RustStructConfig` — the derive/attribute
        // emitters for wrappers, records and enums, `encoding_var_macros`, `needs_hex`. A rule that
        // lands here mints no `RustStruct` at all: it emits `pub type Foo = u64;`, which has no
        // attribute site to suppress and no nominal type a hand-written serde/schemars impl could
        // legally target (the orphan rule owns that ceiling, not this generator). So the flag is
        // structurally unhonorable here rather than merely unimplemented, and it is refused for the
        // whole transparent-alias family at the one choke point they all pass through — the scalar
        // alias, the `T / null` Option collapse, and every tagged/ranged variant that falls back to
        // an alias. Refused regardless of the json flags, like every sibling placement rejection:
        // whether a directive may sit somewhere is a property of the spec, not of the build profile.
        //
        // The alias is still INSERTED, for the reason the bare-fixed guard above states: a sibling
        // rule's reference resolves through the alias table during parse, long before `finalize`
        // turns the recorded rejection into an `Err`.
        //
        // The TABLE flavor cannot be caught here — it registers through `AliasInfo::new_manual`,
        // whose `rule_metadata` is hardcoded `None` — so it is caught from the struct config in the
        // `finalize` kind-walk instead, beside the custom-codec rejections.
        if info
            .rule_metadata
            .as_ref()
            .is_some_and(|metadata| metadata.custom_json)
        {
            self.record_custom_json_on_transparent_alias_rejection(&alias);
        }
        self.type_aliases.insert(alias.into(), info);
    }

    /// The transparent-alias wire invariant: **no wire-affecting property of a `RustType` may
    /// survive on a root that emits a transparent alias.**
    ///
    /// A `pub type Foo = <target>;` line mints no type of its own, so `Foo`'s standalone
    /// `to_cbor_bytes`/`from_cbor_bytes` ARE the target's — while every embed site of `Foo` applies
    /// the encoding operations the alias entry carries (`write_bytes` around a `.cbor` payload, a
    /// `write_tag` before a tagged body). One CDDL type then has two incompatible wire forms
    /// depending on which use-site reached it, in a crate that compiles everywhere and says nothing.
    /// That was the shape T1-02 was (a `bytes .cbor T` rule body registering with a `CBORBytes`
    /// operation on its base) and T1-13 after it (a tagged collection or tagged `T / null` rule body
    /// registering with a `Tagged`/`OptionallyTagged` one). None of them can register any more —
    /// every such rule body force-wraps into a real wrapper struct — and this assert is what keeps
    /// the class UNREPRESENTABLE rather than re-found later: any FUTURE control operator that adds
    /// an encoding operation fails here at its first registration instead of shipping a second
    /// silent wire form.
    ///
    /// It is an internal invariant, unreachable from user input, so a panic is the honest posture —
    /// same class as the already-`Alias`-wrapped-base refusal above. ONE carve-out remains,
    /// enumerated by GENERATION (all committed specs plus the whole `--bin cddl-codegen` suite,
    /// with the carve-out removed) rather than by grep:
    ///
    /// 1. A base that is `Fixed`: the rule is refused, by `record_bare_fixed_rule_rejection` in this
    ///    same function (`foo = #6.5(5)`, `tests/robustness/tagged_literal.cddl`). The entry is
    ///    inserted only so a sibling's reference resolves during parse; `finalize` turns the
    ///    rejection into an `Err` before anything is emitted, so no alias — and no surviving tag —
    ///    ever reaches a wire. The invariant is about what EMITS.
    fn assert_no_wire_facts_survive_a_transparent_alias(alias: &RustIdent, base_type: &RustType) {
        if base_type.encodings.is_empty() {
            return;
        }
        if matches!(base_type.conceptual_type, ConceptualRustType::Fixed(_)) {
            return;
        }
        panic!(
            "register_type_alias({alias}): a transparent alias cannot carry wire-affecting \
             encodings, and this one carries {:?}. `pub type {alias} = …;` mints no type of its \
             own, so `{alias}`'s standalone to/from_cbor_bytes would be the target's (writing and \
             accepting the UNWRAPPED form) while every embed site of `{alias}` applies these \
             operations — one CDDL type with two wire forms. Register the rule as a wrapper struct \
             (`RustStruct::new_wrapper`) instead, the way the `.cbor` and tag-head rule bodies do.",
            base_type.encodings
        );
    }

    /// The rejection for `@custom_json` on a rule that resolves to a transparent alias rather than to
    /// a generated struct. Shared by the two seams such a rule can be recognized at — the alias table
    /// (`register_type_alias` above) and the `finalize` kind-walk, which is where the TABLE flavor is
    /// visible — so the one message they both emit cannot drift apart.
    pub fn record_custom_json_on_transparent_alias_rejection(&mut self, rule: &RustIdent) {
        self.record_rejection(format!(
            "@custom_json on `{rule}`: the rule resolves to a transparent alias (`pub type {rule} = \
             …;`), which is not a type of its own — there is no attribute site for the JSON derives \
             to be suppressed on, and no nominal type your hand-written `Serialize`/`JsonSchema` \
             impls could be written for. Add `@newtype` so the rule mints a real wrapper struct \
             (`{rule} = … ; @newtype @custom_json`), and hand-write the impls for that."
        ));
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

    /// Register a semantic nominal mint before its caller can lookup, deduplicate, or insert an
    /// owner.  It intentionally shares [`RustStruct::structural_fingerprint`] with the global
    /// registration guard, so equal claims retain the first owner and unequal ones are rejected
    /// rather than creating a competing ownership vocabulary.
    pub fn claim_nominal_mint(&mut self, rust_struct: &RustStruct, site: impl Into<String>) {
        self.claim_nominal_mint_inner(rust_struct, site.into(), true);
    }

    fn registration_mint_site(ident: &RustIdent) -> String {
        format!("RustStruct registration for `{ident}`")
    }

    fn claim_nominal_mint_inner(
        &mut self,
        rust_struct: &RustStruct,
        site: String,
        report_registration_duplicate: bool,
    ) {
        let ident = rust_struct.ident().clone();
        if ident.is_type_expression() {
            return;
        }
        let claim = NominalMintClaim {
            identity: rust_struct.structural_fingerprint(),
            site,
        };
        if let Some(first) = self.nominal_mint_claims.get(&ident) {
            if first.identity != claim.identity {
                // Ordinary registrations retain the legacy global guard's one diagnostic. The
                // mint ledger speaks only when a semantic pre-registration claimant is involved.
                if report_registration_duplicate
                    || !first.site.starts_with("RustStruct registration for `")
                {
                    self.record_rejection(format!(
                        "generated Rust type `{ident}` has incompatible mint claims: `{}` first claimed; \
                         `{}` later claimed a different structural/wire identity. Keep one wire shape \
                         per generated Rust name.",
                        first.site, claim.site,
                    ));
                }
            }
            return;
        }
        self.nominal_mint_claims.insert(ident, claim);
    }

    /// Reserve an explicit variant spelling before any derived sibling receives a suffix. Returns
    /// the first explicit claimant in this enum context, if any, so the parser can retain its
    /// established kind-specific diagnostic wording.
    pub(crate) fn reserve_explicit_variant_mint(
        &mut self,
        context: &str,
        arm_ordinal: usize,
        source_name: String,
        emitted_name: String,
    ) -> Option<VariantMintClaim> {
        if let Some(first) = self
            .variant_mint_claims
            .get(context)
            .and_then(|claims| claims.iter().find(|claim| claim.arm_ordinal == arm_ordinal))
            .cloned()
        {
            if first.explicit
                && first.source_name == source_name
                && first.emitted_name == emitted_name
            {
                // The AST walker can revisit a node during classification/construction. Replaying
                // the SAME semantic claim is not a second arm and must not turn into a false
                // explicit collision.
                return None;
            }
            let context = Self::variant_mint_context_for_diagnostic(context);
            self.record_rejection(format!(
                "variant mint claim drift in {context}: arm {arm_ordinal} first claimed source `{}` as `{}` ({}) but later claimed source `{source_name}` as `{emitted_name}` (explicit @name). One enum arm must retain one stable mint claim.",
                first.source_name,
                first.emitted_name,
                if first.explicit { "explicit @name" } else { "derived" },
            ));
            return None;
        }
        let claims = self
            .variant_mint_claims
            .entry(context.to_owned())
            .or_default();
        let first = claims
            .iter()
            .find(|claim| claim.explicit && claim.emitted_name == emitted_name)
            .cloned();
        claims.push(VariantMintClaim {
            arm_ordinal,
            source_name,
            emitted_name,
            explicit: true,
            requested_base: None,
        });
        first
    }

    /// Settle and retain a derived variant spelling against both explicit reservations and earlier
    /// derived settlements in this enum's actual namespace.
    pub(crate) fn settle_derived_variant_mint(
        &mut self,
        context: &str,
        arm_ordinal: usize,
        source_name: String,
        base: String,
    ) -> String {
        if let Some(first) = self
            .variant_mint_claims
            .get(context)
            .and_then(|claims| claims.iter().find(|claim| claim.arm_ordinal == arm_ordinal))
            .cloned()
        {
            if !first.explicit
                && first.source_name == source_name
                && first.requested_base.as_deref() == Some(base.as_str())
            {
                // Same revisit rule as the explicit path. Returning the settled spelling is
                // important: allocating again would manufacture a suffix merely because
                // construction re-entered.
                return first.emitted_name;
            }
            let context = Self::variant_mint_context_for_diagnostic(context);
            self.record_rejection(format!(
                "variant mint claim drift in {context}: arm {arm_ordinal} first claimed source `{}` as `{}` ({}) but later claimed source `{source_name}` from derived base `{base}`. One enum arm must retain one stable mint claim.",
                first.source_name,
                first.emitted_name,
                if first.explicit { "explicit @name" } else { "derived" },
            ));
            return first.emitted_name;
        }
        let claims = self
            .variant_mint_claims
            .entry(context.to_owned())
            .or_default();
        let used = |candidate: &str, claims: &[VariantMintClaim]| {
            claims.iter().any(|claim| claim.emitted_name == candidate)
        };
        let requested_base = base.clone();
        let emitted_name = if !used(&base, claims) {
            base
        } else {
            let mut n = 2u32;
            loop {
                let candidate = format!("{base}{n}");
                if !used(&candidate, claims) {
                    break candidate;
                }
                n += 1;
            }
        };
        claims.push(VariantMintClaim {
            arm_ordinal,
            source_name,
            emitted_name: emitted_name.clone(),
            explicit: false,
            requested_base: Some(requested_base),
        });
        emitted_name
    }

    // this is called by register_table_type / register_array_type automatically
    pub fn register_rust_struct(
        &mut self,
        parent_visitor: &ParentVisitor,
        mut rust_struct: RustStruct,
        cli: &Cli,
    ) {
        // A generic INSTANCE's config is the generic DEFINITION's, so the binding rule's own `@doc`
        // has no route into the struct it mints. Applied here, at the one registration seam every
        // struct passes through, rather than at the generic-resolution arm alone.
        if let Some(doc) = self.rule_docs.get(&rust_struct.ident).cloned() {
            rust_struct.set_doc_if_absent(&doc);
        }
        // `@custom_json` reaches its config the same two ways it can miss it — a generic INSTANCE's
        // config is the generic DEFINITION's, and a plain GROUP rule's is built from metadata read
        // off a slot cddl leaves empty — so the per-ident record is applied at this same seam. Only
        // ever sets the flag: a config that already carries it got it from the rule that owns the
        // struct, and the record is that rule's own statement, so the two can only agree.
        if self.custom_json_rules.contains(&rust_struct.ident) {
            rust_struct.set_custom_json();
        }
        // A `@newtype`- or TAG-forced wrapper over an INLINE COLLECTION (`#6.258([* a]) ; @newtype`,
        // `[* a] ; @newtype @duplicates reject`, `#6.24({* k => v}) ; @duplicates preserve`) selects
        // its inner representation by the effective `@duplicates` policy exactly as a transparent
        // alias does. This is local normalization of the incoming claim, so it belongs before the
        // structural comparison below; alias registration and every other IR-map mutation follow it.
        if let Some(policy) = rust_struct.config().duplicates
            && let RustStructType::Wrapper { wrapped, .. } = &mut rust_struct.variant
            && matches!(
                wrapped.conceptual_type,
                ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _)
            )
        {
            *wrapped = wrapped.clone().with_duplicates_policy(Some(policy));
        }
        self.claim_nominal_mint_inner(
            &rust_struct,
            Self::registration_mint_site(rust_struct.ident()),
            false,
        );
        // Every generated nominal shares this namespace. Decide ownership only after the incoming
        // claim's local configuration is complete, but before it can create an alias, synthesize a
        // table keys-list, or mutate any other IR map: last-registration-wins would silently
        // retarget every existing reference to a different wire shape. Equivalent structures are
        // deliberate shared ownership (not replacement).
        if let Some(existing) = self.rust_structs.get(rust_struct.ident()) {
            if existing.structurally_equivalent(&rust_struct) {
                return;
            }
            let ident = rust_struct.ident();
            self.record_rejection(format!(
                "generated Rust type `{ident}` has incompatible registrations: the first claimant \
                 is a {}, but the later claimant is a {}. Keep one wire shape per generated Rust \
                 name; rename one authored rule or the synthesized claimant that collides with it.",
                rust_struct_kind(existing),
                rust_struct_kind(&rust_struct),
            ));
            return;
        }
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
                // A domain that is not FINAL here defers its mint to
                // `finalize_deferred_table_keys_lists` (see `table_keys_list_mint_must_defer` for the
                // two classes); a final domain mints in place, as before.
                if self.scope(&rust_struct.ident).export()
                    && !self.table_keys_list_mint_must_defer(&domain.conceptual_type)
                {
                    let loose_domain = domain.loosened_for_wasm_table_boundary_key();
                    // we must provide the keys type to return
                    self.create_and_register_array_type(
                        parent_visitor,
                        domain.clone(),
                        &loose_domain.name_as_wasm_array(self),
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
        self.rust_structs
            .insert(rust_struct.ident().clone(), rust_struct);
    }

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
    /// structural name hard-errors (the synthesized name sits in the own-spec shape projection).
    ///
    /// Fix: flip the instance alias's `gen_wasm_alias` to `true` and record the ident here. The alias
    /// loop then emits `pub type SetKeyHash = KeyHashList;` (`for_wasm_member` on the alias base is
    /// bounds-aware, so `[+ …]` yields the `NonEmpty…List` name and a directly-exposable element
    /// yields the bare `Vec<…>`), the base-type walk mints the STRUCTURAL wrapper (recording it in
    /// the own-spec shape projection under the structural name), and the struct walk is told to SKIP the
    /// rule-named class mint (via `is_anonymous_collection_instance`). The rust side is untouched — the
    /// transparent `pub type SetKeyHash = Vec<KeyHash>;` alias and every rust reference to it stay
    /// byte-identical. NAMED instance rules (`named_set = set<key_hash>`, ident from the author's rule)
    /// are NOT anonymous, keep their own wasm class, and keep the criterion-8 `--wrapper-requests`
    /// contract. Runs alongside `resolve_late_alias_product_leaves` (both after the late
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
            // subset needs no alias: `resolve_late_alias_product_leaves` lowers its field to
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

    /// Re-resolve finalized product leaves whose transparent alias registered after the parse-time
    /// use-site was built.
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
    /// Now that the late aliases exist, re-run alias substitution on the affected product leaves so
    /// the generic path converges on the SAME `Alias(ident, Array/Map)` type the non-generic path
    /// gets — one collection code path (inline serialize, per-field len/elem/tag encoding vars), not
    /// a parallel one. Scoped to instances whose alias base is a structural `Array`/`Map`: generic
    /// EXTERN instances (alias base `Rust(real_ident)`, registered above with the same
    /// `gen_rust_alias=true`) resolve to a `Rust` type, are excluded here, and stay byte-identical.
    ///
    /// An ordinary forward alias needs the same one-step repair when auto-`@newtype` turns its target
    /// into a wrapper: the wrapper's parse-time `Rust(Alias)` leaf otherwise names no struct even
    /// though the finalized alias table can resolve it. Include every emitted rust alias with no
    /// `RustStruct`, using `resolve_alias` exactly as `new_type` would have if registration had
    /// preceded the use. The walk intentionally does not rewrite alias-table bases or recurse into a
    /// replacement in this pass: one `Alias` node is the terminating named boundary, and alias cycles
    /// remain the recursive-type boundary's responsibility.
    fn resolve_late_alias_product_leaves(&mut self) {
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
        // Rules are normally resolved when their use-site parses. The exception here is a forward
        // alias whose target becomes a wrapper only after the recursive-type boundary asks for an
        // auto-`@newtype` re-pass: its earlier leaf stayed `Rust(Alias)` because no alias existed at
        // that point. Add every finalized emitted alias with no struct owner through the SAME
        // `resolve_alias` path, while leaving the generic-specific convergence decisions above in
        // charge when they already supplied a replacement.
        for (alias_ident, alias_info) in &self.type_aliases {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            if !alias_info.keeps_alias_node() || self.rust_struct(ident).is_some() {
                continue;
            }
            if let Some(replacement) = self.resolve_alias(alias_ident) {
                resolved.entry(ident.clone()).or_insert(replacement);
            }
        }
        if resolved.is_empty() {
            return;
        }
        // The DANGLING, encoding-free subset of `resolved`: aliases that name no registered struct,
        // so a surviving `Rust(<alias>)` leaf refers to nothing generation can look up. This includes
        // a NAMED set-nominal binding (`xs_int = xs<uint>` over a tag-258 set idiom), whose resolution
        // mints the struct under the INSTANTIATION canonical (`XsU64`) and gives the binding ident
        // only a transparent `pub type XsInt = XsU64;` alias, plus the ordinary forward alias above.
        // A transparent-collection instance and a generic EXTERN instance both DO register a struct
        // under their own ident, so their leaves are well-formed and stay untouched — which is why
        // the `Alias`-box descent below is restricted to this subset rather than run over all of
        // `resolved`: repairing only what dangles keeps every already-working shape's emitted bytes
        // identical. The conceptual type alone is carried because an `Alias` box holds a
        // `ConceptualRustType` with nowhere to put encodings; the filter demands the resolved type
        // have none, so nothing can be dropped silently.
        let dangling: BTreeMap<RustIdent, ConceptualRustType> = resolved
            .iter()
            .filter(|(ident, rt)| self.rust_struct(ident).is_none() && rt.encodings.is_empty())
            .map(|(ident, rt)| (ident.clone(), rt.conceptual_type.clone()))
            .collect();
        // Replace a `Rust(alias)` leaf with the one-step resolved alias, keeping any
        // reference-site encodings (`#6.24(xs_int)`-style outer wraps) OUTSIDE the alias's own by
        // appending them. Recurses into structural children first so `[* xs_int]` / `? xs_int` reach
        // the leaf.
        //
        // An `Alias` box is descended too, for the `dangling` subset only. A SECOND alias hop
        // (`bar = xs_int`, then `[b: bar]` / `[* bar]`) registers `Bar`'s own base as the bare
        // instance leaf built at parse time, so the use site arrives here as
        // `Alias(Bar, Rust(XsInt))` — the leaf the one-hop case exposes at top level is one box
        // deeper, and leaving it there aborted generation on the set-idiom flavor. Chains of any
        // depth and leaves under a container inside the box (`Alias(Bar, Array(Rust(XsInt)))`)
        // recurse back through here.
        fn walk(
            rt: &mut RustType,
            resolved: &BTreeMap<RustIdent, RustType>,
            dangling: &BTreeMap<RustIdent, ConceptualRustType>,
        ) {
            match &mut rt.conceptual_type {
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, resolved, dangling)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, resolved, dangling);
                    walk(v, resolved, dangling);
                }
                ConceptualRustType::Alias(_, inner) => walk_ct(inner, resolved, dangling),
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
        /// The inside-an-`Alias`-box half of `walk`. Only `dangling` leaves are substituted here
        /// (see its definition); structural children are full `RustType`s and hand back to `walk`,
        /// which applies the encodings-preserving replacement they can hold.
        fn walk_ct(
            ct: &mut ConceptualRustType,
            resolved: &BTreeMap<RustIdent, RustType>,
            dangling: &BTreeMap<RustIdent, ConceptualRustType>,
        ) {
            match ct {
                ConceptualRustType::Alias(_, inner) => walk_ct(inner, resolved, dangling),
                ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                    walk(inner, resolved, dangling)
                }
                ConceptualRustType::Map(k, v) => {
                    walk(k, resolved, dangling);
                    walk(v, resolved, dangling);
                }
                _ => {}
            }
            if let ConceptualRustType::Rust(ident) = ct
                && let Some(replacement) = dangling.get(ident)
            {
                *ct = replacement.clone();
            }
        }
        for rust_struct in self.rust_structs.values_mut() {
            match &mut rust_struct.variant {
                RustStructType::Record(record) => {
                    for field in record.fields.iter_mut() {
                        walk(&mut field.rust_type, &resolved, &dangling);
                    }
                }
                RustStructType::Table { domain, range, .. } => {
                    walk(domain, &resolved, &dangling);
                    walk(range, &resolved, &dangling);
                }
                RustStructType::Array { element_type, .. } => {
                    walk(element_type, &resolved, &dangling);
                }
                RustStructType::GroupChoice { variants, .. }
                | RustStructType::TypeChoice { variants } => {
                    for variant in variants.iter_mut() {
                        match &mut variant.data {
                            EnumVariantData::RustType(ty) => walk(ty, &resolved, &dangling),
                            EnumVariantData::Inlined(rec) => {
                                for field in rec.fields.iter_mut() {
                                    walk(&mut field.rust_type, &resolved, &dangling);
                                }
                            }
                        }
                    }
                }
                RustStructType::Wrapper { wrapped, .. } => walk(wrapped, &resolved, &dangling),
                RustStructType::CStyleEnum { .. }
                | RustStructType::Extern
                | RustStructType::RawBytesType => {}
            }
        }
    }

    /// Whether the keys-list wasm wrapper for a table with this DOMAIN must be minted in `finalize`
    /// (by `finalize_deferred_table_keys_lists`) rather than at `register_rust_struct`. Two classes,
    /// both "the domain is not FINAL at registration time", and both answered by the same deferral:
    ///
    /// 1. A not-yet-resolved GENERIC-COLLECTION instance is still a bare `Rust(<instance>)` here (its
    ///    transparent alias is registered only in `finalize`), so naming the wrapper now bakes the
    ///    INSTANCE-ident name (`GcollU64List` for `gcoll<uint>`). `finalize`'s
    ///    `resolve_late_alias_product_leaves` then rewrites the domain to its resolved
    ///    collection (`Array(u64)` for an exposable element) and the wasm `keys()` accessor names the
    ///    wrapper from THAT — the structural `ArrU64List`, an E0425 against the instance-named mint.
    /// 2. A RECURSIVELY-registered named domain: rooting a `{ * u_val => u_val }` cycle at the UNION
    ///    (`u_holder = [u_val]`) orders the table's registration BEFORE `u_val` exists as a struct, so
    ///    the domain names an ident in neither `rust_structs` nor `generic_instances`. Naming the
    ///    wrapper routes through `name_as_wasm_array_ct` → `directly_wasm_exposable_ct` → `is_enum`,
    ///    whose registered-or-generic assertion then aborts generation. The assert is right to fire —
    ///    it guards genuinely-unregistered generic instances, and answering `false` there would
    ///    silently misclassify every such ident — so the mint moves instead of the guard.
    ///
    /// The recursion mirrors exactly the arms `Array(domain).directly_wasm_exposable_ct` can reach an
    /// `is_enum` call through, so a domain whose naming is already answerable keeps minting in place
    /// and its emitted bytes are unchanged: `Array`/`Map` stop that probe without consulting an ident,
    /// and a named alias is only followed when it names no wrapper struct.
    fn table_keys_list_mint_must_defer(&self, domain: &ConceptualRustType) -> bool {
        match domain {
            ConceptualRustType::Rust(ident) => {
                self.generic_instances.contains_key(ident) || self.rust_struct(ident).is_none()
            }
            ConceptualRustType::Optional(ty) => {
                self.table_keys_list_mint_must_defer(&ty.conceptual_type)
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), ty) => {
                self.table_keys_list_mint_must_defer(ty)
            }
            ConceptualRustType::Alias(AliasIdent::Rust(ident), ty) => {
                match self.rust_struct(ident).map(|rs| rs.variant()) {
                    // a wrapper struct answers the probe itself; anything else is followed through
                    Some(RustStructType::CStyleEnum { .. }) | None => {
                        self.table_keys_list_mint_must_defer(ty)
                    }
                    Some(_) => false,
                }
            }
            _ => false,
        }
    }

    /// Mint the keys-list array wrapper for each exported table whose domain was not final when the
    /// table registered (deferred from `register_rust_struct`; the two classes are
    /// `table_keys_list_mint_must_defer`'s). Runs in `finalize` AFTER
    /// `resolve_late_alias_product_leaves` has rewritten each generic-instance domain to its
    /// resolved collection, and after every rule in the spec has registered — so the wrapper name
    /// derives from the FINAL domain and matches the wasm `keys()` accessor. Wasm-only (rust maps use
    /// native `.keys()`; the wrapper exists only to cross the wasm boundary). Guarded by "not already
    /// registered": a table whose domain was final at parse minted its keys-list there and is a no-op
    /// here — so the byte output for any spec without a deferred domain is unchanged. Deterministic
    /// (`BTreeMap` order).
    /// If two deferred tables resolve to the SAME boundary name, both pass the not-registered filter
    /// before either mints; the table-keys registration mode retains the first synthesized loose
    /// boundary carrier. Bounds and encodings on the original key occurrence are deliberately not
    /// properties of the returned keys-list class.
    fn finalize_deferred_table_keys_lists(&mut self, parent_visitor: &ParentVisitor, cli: &Cli) {
        if !cli.wasm {
            return;
        }
        let deferred: Vec<RustType> = self
            .rust_structs
            .iter()
            .filter_map(|(ident, rs)| match rs.variant() {
                RustStructType::Table { domain, .. } if self.scope(ident).export() => {
                    let loose_domain = domain.loosened_for_wasm_table_boundary_key();
                    let name = loose_domain.name_as_wasm_array(self);
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
            let name = domain
                .loosened_for_wasm_table_boundary_key()
                .name_as_wasm_array(self);
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
        // This helper is the table-keys-list synthesis site. Structural list names intentionally
        // ignore an inline key collection's occurrence bounds, so store the corresponding loose
        // boundary carrier too; `push_table_accessors` performs the matching infallible conversion.
        let element_type = element_type.loosened_for_wasm_table_boundary_key();
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
            // Whether anything already occupies the structural ident, and independently whether an
            // EXPORTED SOURCE RULE claims it. Another table may have synthesized the same boundary
            // class first; that is compatible structural reuse, not an authored collision.
            let authored_claim = self.wasm_ident_claimed_by_user_rule(array_type_name);
            // An authored rule already claims this structural name (all rule idents are known
            // before parsing). Do not mint a temporary keys-list owner that would either replace
            // the authored rule or make the later authored registration collide with a synthesized
            // placeholder. `non_empty_wrapper_name_collisions` sees the table's final keys() need
            // and the authored owner, so it keeps its established family-specific diagnostic for
            // an incompatible shape in either source order; a same-element `[* …]` owner remains
            // the valid shared builder.
            if authored_claim {
                return raw_arr_type.into();
            }
            // we don't pass in tags here. If a tag-wrapped array is done I think it generates
            // 2 separate types (array wrapper -> tag wrapper struct)
            self.register_synthesized_table_keys_list(
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
            // criterion-9 shadow warning over a keys-list no rule declares). Re-apply the marker on
            // every synthesis-only mint. Non-byte-equivalent table re-mints retain the first alias
            // record through the narrow table-key registration mode, so its provenance stays intact.
            if !authored_claim
                && let Some(alias) = self.type_aliases.get_mut(&array_type_ident.into())
            {
                alias.synthesized_collection = true;
            }
        }
        ConceptualRustType::Array(Box::new(element_type)).into()
    }

    /// Table `keys()` exposes one loose wasm-boundary list per structural name. Several table key
    /// occurrences can project onto that one class while retaining distinct source bounds or
    /// encodings; the first synthesized class is therefore its canonical owner. This narrow mode is
    /// the only registration bypass: it never replaces an owner, and an authored claimant is kept
    /// out by `create_and_register_array_type` so `non_empty_wrapper_name_collisions` can issue its
    /// established family-specific rejection instead.
    /// `table_keys_list_syntheses_share_the_established_loose_boundary_carrier` covers the
    /// intentionally non-byte-identical source occurrences that share this class.
    fn register_synthesized_table_keys_list(
        &mut self,
        parent_visitor: &ParentVisitor,
        rust_struct: RustStruct,
        cli: &Cli,
    ) {
        if self.rust_struct(rust_struct.ident()).is_some()
            && self.is_synthesized_collection(rust_struct.ident())
        {
            return;
        }
        self.register_rust_struct(parent_visitor, rust_struct, cli);
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
    /// Derive the dispatch major of every open table's TYPED row (`RustRecord::typed_row`), and
    /// reject — gracefully, never silently — every shape whose major is not statically knowable.
    ///
    /// The two-stage staticness rule (the naive `cbor_types().len() == 1` test is WRONG on its own):
    ///
    /// 1. if the key's alias chain carries a `@custom_serialize`/`@custom_deserialize` pair, the
    ///    codec OWNS the wire and `cbor_types()` answers about the REPLACED type — a codec over a
    ///    raw-bytes marker reports `Bytes` while writing text. There the `@custom_wire_major`
    ///    DECLARATION is required, and it is the answer;
    /// 2. otherwise `cbor_types()` must yield exactly one major. Primitives, primitive-bodied
    ///    aliases, raw-bytes markers, aliases of markers, `.size`-constrained bytes and tagged types
    ///    qualify; plain externs (`Array`+`Map`), the reserved `Int` extern, multi-major unions,
    ///    `any` and optionally-tagged types report more than one and reject naturally.
    ///
    /// Plus the complement check: a catch-all whose admissible majors are EXHAUSTED by the typed row
    /// can never see an entry, so it is rejected rather than emitted as dead code.
    ///
    /// Plus, under the JSON flags only, the BARE-TEXT typed key check. A `K_t` that transparently
    /// resolves to `String` admits EVERY JSON object member name, so the typed-first partition binds
    /// every member to the typed row: the catch-all is provably unreachable through JSON, and
    /// `from_json` refuses what `to_json` wrote for any captured entry (the member rebinds typed and
    /// `V_t` refuses the value, or worse, silently accepts it into the wrong row). That is the T1
    /// fixed point failing on EVERY document with a captured entry, not on an edge case, so the shape
    /// is refused rather than documented. TRANSPARENT resolution only: an opaque `K_t` (an extern or
    /// a `@newtype` whose hand-written serde happens to read every string) is undecidable from here
    /// and stays the documented hazard it already is.
    fn derive_open_table_dispatch_majors(
        &mut self,
        cli: &Cli,
        consumed: &mut BTreeSet<AliasIdent>,
    ) {
        // Two passes, because `cbor_types()` on a `Rust(ident)` key reads `rust_structs` — so the
        // derivation cannot hold a mutable borrow of it. Pass 1 is a pure read of the whole IR
        // producing one verdict per open table; pass 2 applies the verdicts.
        let mut derived: BTreeMap<RustIdent, CBORType> = BTreeMap::new();
        let mut rejections: Vec<String> = Vec::new();
        // The shared no-silent-directive ledger may already contain successful variable-middle
        // array boundaries; typed rows add their own consumers before its final check below.
        for (rule_ident, rust_struct) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rust_struct.variant() else {
                continue;
            };
            if !record.is_open_table() {
                continue;
            }
            let typed_domain = record.typed_row().unwrap().domain();
            let catch_all_domain = record.rest.as_ref().unwrap().domain();
            if (cli.json_serde_derives || cli.json_schema_export)
                && matches!(
                    typed_domain.conceptual_type.resolve_alias_shallow(),
                    ConceptualRustType::Primitive(Primitive::Str)
                )
            {
                rejections.push(format!(
                    "rule `{rule_ident}`: an open table keyed on bare `text` is a CBOR-ONLY shape \
                     and cannot be generated with a JSON face. In JSON both rows share one object \
                     and a member name binds the TYPED row first, so a `String` key — which admits \
                     every member name there is — leaves the catch-all row unreachable and makes \
                     `from_json` refuse documents `to_json` itself wrote. Either generate this spec \
                     without `--json-serde-derives`/`--json-schema-export`, or key the typed row on \
                     a type whose admissible names are a proper subset (a raw-bytes marker or a \
                     `@newtype` whose serde writes a fixed-width hex/bech32 image, or a numeric \
                     key), or spell it as a plain table (`t = {{ * text => v }}`) if one row is all \
                     you need."
                ));
                continue;
            }
            let has_custom_codec = custom_codec_on_alias_chain(&typed_domain.conceptual_type, self);
            let declared = declared_wire_major_on_alias_chain(&typed_domain.conceptual_type, self);
            let major = if has_custom_codec {
                match declared {
                    Some(major) => {
                        mark_wire_major_consumed(&typed_domain.conceptual_type, self, consumed);
                        wire_major_to_cbor_type(major)
                    }
                    None => {
                        rejections.push(format!(
                            "rule `{rule_ident}`: the open table's typed-row key is written by a \
                             `@custom_serialize`/`@custom_deserialize` pair, so the generator cannot \
                             infer which CBOR major type the wire starts with — the codec owns that \
                             wire, and the type it replaces answers about a wire nobody writes. \
                             Declare it beside the pair with `@custom_wire_major <major>` (one of \
                             `uint` / `nint` / `bytes` / `text` / `array` / `map` / `tag` / \
                             `simple`)."
                        ));
                        continue;
                    }
                }
            } else {
                if declared.is_some() {
                    mark_wire_major_consumed(&typed_domain.conceptual_type, self, consumed);
                }
                let majors = typed_domain.cbor_types(self);
                match majors.as_slice() {
                    [only] => *only,
                    _ => {
                        rejections.push(format!(
                            "rule `{rule_ident}`: the open table's typed row must be keyed on a type \
                             whose CBOR major type is statically known — it claims exactly that one \
                             major and the catch-all row sees the complement — but this key admits \
                             {} majors ({}). Use a single-major key (a primitive, an alias of one, a \
                             raw-bytes marker or an alias of one, a `.size`-constrained bytes, or a \
                             tagged type); a key whose wire a `@custom_serialize` / \
                             `@custom_deserialize` pair owns declares its major with \
                             `@custom_wire_major <major>` instead.",
                            majors.len(),
                            majors
                                .iter()
                                .map(|m| format!("{m:?}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                        continue;
                    }
                }
            };
            // The catch-all sees the COMPLEMENT of the typed row's major. If its key admits nothing
            // else, it can never capture an entry — dead code standing in for a table.
            let catch_all_majors = catch_all_domain.cbor_types(self);
            if catch_all_majors.iter().all(|m| *m == major) {
                rejections.push(format!(
                    "rule `{rule_ident}`: the open table's catch-all row can never capture an entry \
                     — every CBOR major type its key admits ({major:?}) is already claimed by the \
                     typed row. Widen the catch-all's key type, or spell this as a plain table (`t = \
                     {{ * k => v }}`) if one row is all you need."
                ));
                continue;
            }
            derived.insert(rule_ident.clone(), major);
        }
        for (rule_ident, major) in derived {
            if let Some(RustStructType::Record(record)) = self
                .rust_structs
                .get_mut(&rule_ident)
                .map(|rs| &mut rs.variant)
                && let Some(typed) = record.typed_row.as_mut()
            {
                typed.dispatch_major = Some(major);
            }
        }
        // no-silent-directive: a `@custom_wire_major` nobody consumed declares a fact about a wire
        // no boundary reads. Consumed SOMEWHERE is enough — one alias may key an open table's typed
        // row or prove a variable middle array boundary and also appear at an ordinary field.
        //
        // Only an AUTHORED declaration is checked. An entry that inherited its wire facts across a
        // registration strip (`wire_metadata_inherited_from`) carries a copy nobody wrote there, so
        // "nobody consumes it" says nothing about anyone's spec — the copy exists precisely so a
        // member declared through the re-alias reaches the right codec, and demanding that every
        // re-alias of a table key also key a table would refuse specs that are correct today. The
        // author's own rule is still checked, and a dispatch reading through an inheritor marks it
        // consumed (`mark_wire_major_consumed`).
        for (alias_ident, info) in self.type_aliases.iter() {
            if info.wire_metadata_inherited_from.is_some() {
                continue;
            }
            if info
                .rule_metadata
                .as_ref()
                .and_then(|rmd| rmd.custom_wire_major)
                .is_some()
                && !consumed.contains(alias_ident)
            {
                rejections.push(format!(
                    "`@custom_wire_major` on rule `{alias_ident}`: nothing consumes the declared \
                     major. It is read when this transparent alias keys an OPEN TABLE's typed row \
                     (`t = {{ * {alias_ident} => v, * k2 => v2 }}`), or appears as either boundary \
                     item of a variable middle ARRAY occurrence with an immediate mandatory, \
                     major-disjoint suffix (`m = [prefix, * {alias_ident}, suffix]`). Remove the \
                     directive, or use this rule at one of those boundaries."
                ));
            }
        }
        for msg in rejections {
            self.record_rejection(msg);
        }
    }

    /// The statically-known CBOR majors a variable middle array occurrence may use as its greedy
    /// boundary discriminator. Mandatory generator-owned framing wins before a custom codec can
    /// own the inner value; otherwise a complete custom pair on a transparent alias needs its
    /// declared major, and every other custom/opaque head remains unproven. This is deliberately
    /// distinct from optional-field lookahead, whose established proof remains generator-owned.
    pub(crate) fn effective_wire_majors(&self, ty: &RustType) -> Option<Vec<CBORType>> {
        match ty.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_)) => return Some(vec![CBORType::Tag]),
            Some(CBOREncodingOperation::CBORBytes) => return Some(vec![CBORType::Bytes]),
            // An optional generator-owned tag can expose either the tag or its inner built-in
            // head.  Preserve the full set so a greedy middle loop recognizes both wire forms.
            // A custom inner value remains unproven: declared custom heads deliberately do not
            // extend through this optional framing, whose absent arm would expose user-owned wire.
            Some(CBOREncodingOperation::OptionallyTagged(_)) => {
                let mut inner = ty.clone();
                inner.encodings.pop();
                return (!self.type_has_unproven_wire_head(&inner)).then(|| ty.cbor_types(self));
            }
            None => {}
        }
        if custom_codec_on_alias_chain(&ty.conceptual_type, self) {
            return declared_wire_major_on_alias_chain(&ty.conceptual_type, self)
                .map(wire_major_to_cbor_type)
                .map(|major| vec![major]);
        }
        if self.type_has_unproven_wire_head(ty) {
            None
        } else {
            Some(ty.cbor_types(self))
        }
    }

    /// The complete CDDL-value domain of a conservative fixed-value boundary item.  This is
    /// intentionally narrower than `cbor_types()`: it proves every value the generated decoder can
    /// accept, rather than merely the item's outer CBOR major.  It admits only untagged,
    /// field-codec-free fixed literals and named all-fixed choices/singletons through transparent
    /// aliases.  Floats stay out even when their source spellings differ, because `f64::PartialEq`
    /// is not a CDDL-semantic proof for NaN values.
    pub(crate) fn fixed_value_domain(&self, ty: &RustType) -> Option<Vec<FixedValue>> {
        self.fixed_value_domain_inner(ty, &mut BTreeSet::new(), &mut BTreeSet::new())
    }

    /// Whether a variable middle boundary needs the finite-domain retry strategy rather than the
    /// established major peek.  Keep this derivation pure and recomputable from finalized IR so it
    /// does not become a presentation-only field in debug/snapshot IR.
    pub(crate) fn has_disjoint_fixed_domain_middle_boundary(
        &self,
        repeated: &RustType,
        suffix: &RustType,
    ) -> bool {
        let Some(repeated_majors) = self.effective_wire_majors(repeated) else {
            return false;
        };
        let Some(suffix_majors) = self.effective_wire_majors(suffix) else {
            return false;
        };
        if !repeated_majors
            .iter()
            .any(|major| suffix_majors.contains(major))
        {
            return false;
        }
        self.fixed_value_domain(repeated)
            .as_ref()
            .zip(self.fixed_value_domain(suffix).as_ref())
            .is_some_and(|(repeated, suffix)| {
                repeated
                    .iter()
                    .all(|value| !suffix.iter().any(|other| other == value))
            })
    }

    fn fixed_value_domain_inner(
        &self,
        ty: &RustType,
        seen_structs: &mut BTreeSet<RustIdent>,
        seen_aliases: &mut BTreeSet<AliasIdent>,
    ) -> Option<Vec<FixedValue>> {
        // Bounds, collection policy, defaulting, or framing change the construction/wire story
        // from the plain fixed literals this proof deliberately owns.
        if !ty.encodings.is_empty()
            || ty.config.default.is_some()
            || ty.config.bounds.is_some()
            || ty.config.float_bounds.is_some()
            || ty.config.duplicates.is_some()
            || ty.config.basic_override
        {
            return None;
        }
        match &ty.conceptual_type {
            ConceptualRustType::Fixed(value) => {
                (!matches!(value, FixedValue::Float(_))).then(|| vec![value.clone()])
            }
            ConceptualRustType::Alias(alias, _inner) => {
                let info = self.type_aliases.get(alias)?;
                if info.carries_custom_pair()
                    || info.rule_metadata.as_ref().is_some_and(|metadata| {
                        metadata.custom_encodings.is_some() || metadata.custom_wire_major.is_some()
                    })
                {
                    return None;
                }
                // `AliasInfo::base_type`, not `inner`, is the transparent alias source of truth:
                // registration can carry bounds/encodings/configuration there which the conceptual
                // inner deliberately does not store.  A classifier that reconstructed a fresh
                // `RustType` from `inner` could falsely prove a configured alias fixed-domain.
                if !seen_aliases.insert(alias.clone()) {
                    return None;
                }
                let result =
                    self.fixed_value_domain_inner(&info.base_type, seen_structs, seen_aliases);
                seen_aliases.remove(alias);
                result
            }
            ConceptualRustType::Rust(ident) => {
                if !seen_structs.insert(ident.clone()) {
                    return None;
                }
                let result = (|| {
                    let rust_struct = self.rust_struct(ident)?;
                    if rust_struct.tag.is_some()
                        || rust_struct.tag_optional()
                        || rust_struct.config().custom_serialize.is_some()
                        || rust_struct.config().custom_deserialize.is_some()
                        || rust_struct.config().custom_encodings.is_some()
                        || rust_struct.config().custom_wire_major.is_some()
                    {
                        return None;
                    }
                    let variants = match rust_struct.variant() {
                        RustStructType::TypeChoice { variants }
                        | RustStructType::CStyleEnum { variants } => variants,
                        _ => return None,
                    };
                    let mut values = Vec::new();
                    for variant in variants {
                        if variant.serialize_as_embedded_group || variant.key.is_some() {
                            return None;
                        }
                        values.extend(self.fixed_value_domain_inner(
                            variant.rust_type(),
                            seen_structs,
                            seen_aliases,
                        )?);
                    }
                    (!values.is_empty()).then_some(values)
                })();
                seen_structs.remove(ident);
                result
            }
            ConceptualRustType::Primitive(_)
            | ConceptualRustType::Any
            | ConceptualRustType::Optional(_)
            | ConceptualRustType::Array(_)
            | ConceptualRustType::Map(_, _) => None,
        }
    }

    /// Whether the effective-major derivation for this bare middle-boundary item actually READS a
    /// transparent alias's declaration. Mandatory generated outer framing wins without consulting
    /// it, so such a declaration stays subject to the no-silent-directive check.
    fn middle_boundary_consumes_wire_major_declaration(&self, ty: &RustType) -> bool {
        ty.encodings.is_empty()
            && custom_codec_on_alias_chain(&ty.conceptual_type, self)
            && declared_wire_major_on_alias_chain(&ty.conceptual_type, self).is_some()
    }

    /// Whether `ty` can write a head the generator cannot prove. Mandatory tags and `.cbor` own
    /// a stable outer head; optionally-tagged values may still expose their inner head. The visited
    /// set makes recursive type-choice/wrapper graphs conservative rather than recursive forever:
    /// a custom/unproven owner is found on its first visit.
    pub(crate) fn type_has_unproven_wire_head(&self, ty: &RustType) -> bool {
        self.type_has_unproven_wire_head_inner(ty, &mut BTreeSet::new())
    }

    fn type_has_unproven_wire_head_inner(
        &self,
        ty: &RustType,
        seen: &mut BTreeSet<RustIdent>,
    ) -> bool {
        match ty.encodings.last() {
            Some(CBOREncodingOperation::Tagged(_) | CBOREncodingOperation::CBORBytes) => {
                return false;
            }
            Some(CBOREncodingOperation::OptionallyTagged(_)) => {
                let mut inner = ty.clone();
                inner.encodings.pop();
                return self.type_has_unproven_wire_head_inner(&inner, seen);
            }
            None => {}
        }
        match &ty.conceptual_type {
            ConceptualRustType::Alias(alias_ident, inner) => {
                let codec_owned = self
                    .type_aliases()
                    .get(alias_ident)
                    .and_then(|info| info.rule_metadata.as_ref())
                    .is_some_and(|metadata| {
                        metadata.custom_serialize.is_some() || metadata.custom_deserialize.is_some()
                    });
                codec_owned
                    || self
                        .type_has_unproven_wire_head_inner(&RustType::new((**inner).clone()), seen)
            }
            ConceptualRustType::Optional(inner) => {
                self.type_has_unproven_wire_head_inner(inner, seen)
            }
            ConceptualRustType::Rust(ident) => {
                if !seen.insert(ident.clone()) {
                    return false;
                }
                let rust_struct = self.rust_struct(ident).unwrap();
                let config = rust_struct.config();
                if config.custom_serialize.is_some() || config.custom_deserialize.is_some() {
                    return true;
                }
                if rust_struct.tag.is_some() && !rust_struct.tag_optional() {
                    return false;
                }
                match rust_struct.variant() {
                    RustStructType::Wrapper { wrapped, .. } => {
                        self.type_has_unproven_wire_head_inner(wrapped, seen)
                    }
                    RustStructType::TypeChoice { variants }
                    | RustStructType::CStyleEnum { variants } => variants.iter().any(|variant| {
                        matches!(
                            &variant.data,
                            EnumVariantData::RustType(variant_ty)
                                if self.type_has_unproven_wire_head_inner(
                                    variant_ty, seen,
                                )
                        )
                    }),
                    RustStructType::Extern if ident.to_string() != "Int" => true,
                    // These variants emit a stable outer CBOR head; their nested codecs cannot
                    // affect membership at this array position.
                    RustStructType::Record(_)
                    | RustStructType::Table { .. }
                    | RustStructType::Array { .. }
                    | RustStructType::GroupChoice { .. }
                    | RustStructType::Extern
                    | RustStructType::RawBytesType => false,
                }
            }
            // Fixed, primitive, any, array, and map concepts own a built-in outer head.
            ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_)
            | ConceptualRustType::Any
            | ConceptualRustType::Array(_)
            | ConceptualRustType::Map(_, _) => false,
        }
    }

    /// Validate every non-final array occurrence segment after aliases and generic products have
    /// settled.  The wire is greedy (RFC 8610): a repeated element may stop before a fixed suffix
    /// only when the next CBOR head proves that it belongs to the suffix, so a same-major boundary
    /// cannot be guessed or recovered with backtracking.
    ///
    /// Parse records the segment's flattened source position; finalized fields retain theirs.  This
    /// pass is deliberately before every code-generation walk: `cbor_types()` and
    /// `expanded_field_count()` may inspect referenced structs, so the parser cannot soundly make
    /// this decision while forward references and generic instances are unresolved.
    fn validate_array_middle_occurrence_segments(&mut self) -> BTreeSet<AliasIdent> {
        let mut rejections = Vec::new();
        // A declaration becomes live only after its successful variable-middle boundary actually
        // reads the effective major. The open-table pass extends this same ledger before its one
        // no-silent-directive rejection runs.
        let mut consumed = BTreeSet::new();
        for (rule_ident, rust_struct) in &self.rust_structs {
            let RustStructType::Record(record) = rust_struct.variant() else {
                continue;
            };
            for segment in record.dynamic_rows().filter(|row| row.is_array_tail()) {
                let segment_index = segment
                    .array_source_index()
                    .expect("array occurrence segment has a source index");

                // An exact window owns its complete wire boundary by count.  This check must precede
                // the fixed-suffix lookup: adjacent exact segments are valid even though the next
                // authored member is another segment rather than a RustField.
                if segment.has_exact_occurrence_window() {
                    continue;
                }

                // The long-established final-tail form needs no discriminator: it consumes the owner
                // array through its boundary exactly as before.  Exact-zero metadata is included here
                // so an occurrence followed only by a forbidden member is not mistaken for final.
                let has_later_member = record
                    .fields
                    .iter()
                    .any(|field| field.source_index > segment_index)
                    || record
                        .forbidden_fields
                        .iter()
                        .any(|field| field.source_index > segment_index)
                    || record
                        .dynamic_rows()
                        .filter(|row| row.is_array_tail())
                        .any(|other| {
                            other
                                .array_source_index()
                                .is_some_and(|index| index > segment_index)
                        });
                if !has_later_member {
                    continue;
                }

                let source_rule = self
                    .source_rule_name(rule_ident)
                    .unwrap_or(rule_ident.as_ref());
                let Some(suffix) = record
                    .fields
                    .iter()
                    .find(|field| field.source_index == segment_index + 1)
                else {
                    rejections.push(format!(
                    "rule `{source_rule}`: the occurrence-bearing array member at position {} must \
                     be followed immediately by one mandatory, single-item fixed suffix so greedy \
                     decoding can stop without guessing. Frame the repeated part as its own array, \
                     move it final, or use a major-disjoint fixed suffix.",
                    segment_index + 1
                ));
                    continue;
                };
                if suffix.optional {
                    rejections.push(format!(
                    "rule `{source_rule}`: the immediate suffix `{}` after an occurrence-bearing \
                     array member must be mandatory — an optional suffix gives greedy decoding no \
                     certain boundary. Frame the repeated part as its own array, move it final, or \
                     make the suffix mandatory and major-disjoint.",
                    suffix.name
                ));
                    continue;
                }
                if suffix.rust_type.expanded_field_count(self) != Some(1) {
                    rejections.push(format!(
                    "rule `{source_rule}`: the immediate suffix `{}` after an occurrence-bearing \
                     array member must expand to exactly one CBOR item, but this suffix can splice \
                     multiple items. Frame the repeated part as its own array, move it final, or use \
                     a single-item major-disjoint suffix.",
                    suffix.name
                ));
                    continue;
                }
                let repeated_majors = self.effective_wire_majors(segment.element());
                // A field-local pair has no transparent alias metadata channel. Keep its existing
                // graceful refusal even if the field's replaced Rust type itself has one known major.
                let suffix_majors = if suffix.rule_metadata.custom_serialize.is_some()
                    || suffix.rule_metadata.custom_deserialize.is_some()
                {
                    None
                } else {
                    self.effective_wire_majors(&suffix.rust_type)
                };
                if repeated_majors.is_none() || suffix_majors.is_none() {
                    let positions = match (repeated_majors.is_none(), suffix_majors.is_none()) {
                        (true, true) => "the repeated element and immediate suffix",
                        (true, false) => "the repeated element",
                        (false, true) => "the immediate suffix",
                        (false, false) => unreachable!(),
                    };
                    rejections.push(format!(
                    "rule `{source_rule}`: {positions} around the occurrence-bearing array member \
                     have a custom- or extern-owned, otherwise-unproven wire head — greedy decoding \
                     must know both possible CBOR majors before it can prove the boundary. Frame the \
                     repeated part as its own array, move it final, or use a major-disjoint boundary \
                     with generator-proven wire heads."
                ));
                    continue;
                }

                let repeated_majors = repeated_majors.expect("checked above");
                let suffix_majors = suffix_majors.expect("checked above");
                let overlap = repeated_majors
                    .iter()
                    .filter(|major| suffix_majors.contains(major))
                    .map(|major| format!("{major:?}"))
                    .collect::<Vec<_>>();
                if !overlap.is_empty() {
                    if self.has_disjoint_fixed_domain_middle_boundary(
                        segment.element(),
                        &suffix.rust_type,
                    ) {
                        // Greedy still means no suffix speculation: the generated loop tries the
                        // repeated decoder once on the real cursor and restores it only when that
                        // decoder fails.  The finite domains prove a suffix byte can never be a
                        // successful repeated value.
                    } else {
                        rejections.push(format!(
                        "rule `{source_rule}`: the occurrence-bearing array member at position {} and \
                         its immediate suffix `{}` share CBOR major type(s) {}. RFC 8610 repetition is \
                         greedy and does not backtrack, so the generator will not guess where the \
                         repeated part ends. A same-major boundary is admitted only when BOTH sides \
                         have generator-owned, untagged finite fixed-value domains with no shared \
                         CDDL value; these boundaries do not prove that. Frame the repeated part as \
                         its own array, move it final, choose a major-disjoint suffix, or make both \
                         fixed-value domains disjoint.",
                        segment_index + 1,
                        suffix.name,
                        overlap.join(", ")
                    ));
                    }
                } else {
                    // Both boundaries reached their effective major sets and proved disjoint. Mark an
                    // alias chain only when its declaration supplied that effective set; a mandatory
                    // generated tag/`.cbor` frame wins without reading an inner declaration, which
                    // must remain inert. Marking keeps a re-alias's authored origin live too.
                    if self.middle_boundary_consumes_wire_major_declaration(segment.element()) {
                        mark_wire_major_consumed(
                            &segment.element().conceptual_type,
                            self,
                            &mut consumed,
                        );
                    }
                    if self.middle_boundary_consumes_wire_major_declaration(&suffix.rust_type) {
                        mark_wire_major_consumed(
                            &suffix.rust_type.conceptual_type,
                            self,
                            &mut consumed,
                        );
                    }
                }
            }
        }
        for rejection in rejections {
            self.record_rejection(rejection);
        }
        consumed
    }

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
                        // `@custom_json` on the BINDING has nothing to act on: the binding emits
                        // `pub type NamedSet = SetKeyHash;` and every derive it would suppress
                        // belongs to the nominal, whose config comes from the generic DEFINITION.
                        // The transparent-alias family's usual `@newtype` remedy does not apply —
                        // a set nominal already IS a wrapper and `@newtype` on the binding is an
                        // accepted no-op — so this shape carries its own message, naming the
                        // definition as the rule that owns the derives (probed: `@custom_json` on
                        // the generic set def drops the nominal's `Serialize`/`JsonSchema` impls).
                        if self.custom_json_rules.contains(&instance_ident) {
                            let source = self
                                .source_rule_name(&instance_ident)
                                .unwrap_or(instance_ident.as_ref())
                                .to_owned();
                            self.record_rejection(format!(
                                "@custom_json on `{source}`: this rule binds a generic set nominal, \
                                 so it emits a transparent `pub type {instance_ident} = \
                                 {canonical_ident};` and mints no type of its own — the \
                                 serde/schemars derives it would suppress are on `{canonical_ident}`, \
                                 whose config comes from the generic DEFINITION. Put `@custom_json` \
                                 on the definition instead (`<def><T> = #6.258([* T]) / [* T] ; \
                                 @custom_json`), and hand-write the impls for the nominal."
                            ));
                        }
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
        self.resolve_late_alias_product_leaves();
        // Array middle-occurrence safety depends on finalized aliases/generics and must reject
        // before any later IR walk or code emitter can build a non-round-tripping decoder.
        let mut wire_major_consumed = self.validate_array_middle_occurrence_segments();
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        // Mint the wasm keys-list wrappers whose owning table had a GENERIC-COLLECTION-instance
        // domain — deferred from `register_rust_struct` until now, so they name from the resolved
        // domain (see the deferral comment there). Idempotent by the not-yet-registered guard: a
        // non-generic table already minted its keys-list at parse and is skipped here.
        self.finalize_deferred_table_keys_lists(parent_visitor, cli);
        // Phase 2.4: nominalize INLINE `#6.258([* T])` occurrences into shape-derived `Set<Elem>`
        // wrappers, at the ONE post-collapse seam (over the finalized construction PRODUCTS, never in
        // `rust_type_from_type2`). Must run BEFORE the key-demand analysis below so the minted
        // wrappers' elements get the full comparison bundle via the set-nominal block there, and after
        // the generic resolution/re-resolution above so every registered product (incl. resolved
        // generic instances) is seen in final shape.
        self.nominalize_inline_sets(parent_visitor, cli)?;
        // Phase 2.5: derive each OPEN TABLE's typed-row dispatch major, and police the
        // `@custom_wire_major` declarations that feed it. Runs HERE — not in the parse walk — for the
        // same two reasons the float-key instruments below do: `cbor_types()` does
        // `rust_struct(ident).unwrap()` and would panic on a not-yet-registered forward reference,
        // and only a post-generic-resolution pass sees a key hidden behind a resolved generic
        // instance. Parse decides SHAPE, finalize decides STATICNESS.
        self.derive_open_table_dispatch_majors(cli, &mut wire_major_consumed);
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
                    ConceptualRustType::Primitive(p) if p.is_float()
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
        // The set-side twin of `float_key_msg`: a set's uniqueness door and (for a tag-258
        // nominal) always-on comparison derives need `Ord` on the element, which floats don't
        // have. A named tag-258 set stays a comparison-bearing wrapper even under `preserve`, so
        // that policy can never be offered as a float repair.
        fn float_set_elem_msg(rule: &RustIdent) -> String {
            format!(
                "rule `{rule}`: set element type contains a float (floats have no total order, so set elements cannot be compared for uniqueness) — use a non-float element type. A tag-258 set nominal always requires comparison derives, even with `@duplicates preserve`, so preserve cannot repair it; to keep float elements, rewrite that tag-258 set as a plain array (`foo = [* float64]`). A plain `@duplicates reject` array can instead drop that directive and use normal Vec semantics"
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
            // BOTH dynamic rows: an open table's TYPED row keys the same container kind, so a walk
            // that reads only the catch-all leaves `K_t` without its comparison derives (E0277 in the
            // generated crate, and no `borrowed_key_types.rs` row for a dep-owned `K_t`).
            for rest in match rust_struct.variant() {
                RustStructType::Record(record) => Some(record),
                _ => None,
            }
            .into_iter()
            .flat_map(|record| record.captured_dynamic_rows())
            .filter(|rest| !rest.is_array_tail() && !rest.map_key_uses_peeked_path(self))
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
        // `@used_as_key` / `@used_as_elem` ask for a wasm surface keyed on the rule's OWN type, and a
        // generic DEFINITION has none — only its instantiations name concrete types. `@used_as_key`
        // was dropped silently (the demand-propagation walk skips a root with no `rust_structs`
        // entry), and `@used_as_elem` was worse: the exposable-element check below resolves the
        // marked ident's element type, whose `directly_wasm_exposable` walk asserts that a
        // non-struct ident is a generic INSTANCE — so a marked generic DEF aborted the run at exit
        // 101 with an `assertion failed` and no diagnosis. Both refuse here, in the house style,
        // naming the instantiating rule as the placement that works.
        //
        // Placed before the `cli.wasm` block (which owns the abort site) and flag-independently,
        // like every sibling placement rejection: whether a directive may sit somewhere is a
        // property of the spec, not of the build profile. Keyed on `generic_defs` rather than on
        // "absent from `rust_structs`" so it covers every generic-def body spelling at once — the
        // record body the parse walk marks from, and the tag-set idiom the choice path marks from —
        // and refuses nothing else. Determinism: `BTreeSet`/`BTreeMap` iteration.
        // Named by their CDDL SOURCE spelling: a generic definition mints no rust type, and the
        // remedy is CDDL the author writes back into the spec.
        let generic_def_source = |ident: &RustIdent| {
            self.source_rule_name(ident)
                .unwrap_or(ident.as_ref())
                .to_owned()
        };
        let generic_def_elem = self
            .used_as_elem
            .iter()
            .filter(|ident| self.generic_defs.contains_key(*ident))
            .map(generic_def_source)
            .collect::<Vec<_>>();
        let generic_def_key = self
            .key_demand_roots
            .keys()
            .filter(|ident| self.generic_defs.contains_key(*ident))
            .map(generic_def_source)
            .collect::<Vec<_>>();
        for ident in generic_def_key {
            self.record_rejection(format!(
                "@used_as_key on `{ident}`: a generic DEFINITION names no concrete type — only its \
                 instantiations do — so there is no type for the map-key comparison derives to be \
                 demanded on, and the demand is dropped. Put the directive on the instantiating \
                 rule instead (`inst = {ident}<uint> ; @used_as_key`), which is where the concrete \
                 type is minted."
            ));
        }
        for ident in generic_def_elem {
            self.record_rejection(format!(
                "@used_as_elem on `{ident}`: a generic DEFINITION names no concrete type — only its \
                 instantiations do — so there is no element type for a loose-list wrapper to hold. \
                 Put the directive on the instantiating rule instead (`inst = {ident}<uint> ; \
                 @used_as_elem`), which is where the concrete type is minted."
            ));
        }
        // NonEmptyVec wasm-wrapper name collisions: an inline `[+ elem]` mints a `NonEmpty<Elem>List`
        // wasm class; if a user rule already OWNS that identifier, silently sharing it would emit a
        // wrapper of the wrong shape (loose `Vec` vs restricted `NonEmptyVec`). Reject clearly rather
        // than shadow. Only relevant with wasm bindings (the collision is on the wasm class name).
        if cli.wasm {
            for msg in self.non_empty_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_array_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_reject_ordered_set_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // NonEmptyMap wasm-wrapper name collisions — the map-side twin of the above.
            for msg in self.non_empty_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            // Finite/exact/lower-bounded unique-key table wrapper names are their own family:
            // `MapKToVMinN/MaxN` cannot share the NonEmptyMap detector because their checked door
            // and structural identity include both occurrence endpoints.
            for msg in self.bounded_map_wrapper_name_collisions() {
                self.record_rejection(msg);
            }
            for msg in self.bounded_pair_map_wrapper_name_collisions() {
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
            // The OPEN TABLE (`t = { * K_t => V_t, * K_r => V_r }`) is the fifth container kind, and
            // it is the one that gets NO sibling of its own — recorded here so the standing ruling
            // reads as satisfied rather than skipped. Two independent reasons, both structural:
            //   * its minted struct is named by the RULE IDENT, which is the author's own name by
            //     construction. The four siblings above each guard a name the generator DERIVES
            //     (`NonEmpty<Elem>List`, `MapKToV`, `<Elem>OrderedSet`, `PairMapKToV`) against a
            //     rule that shadows it; an open table synthesizes no such name because the shape is
            //     a NAMED-RULE concession (an inline anonymous open table is refused at
            //     recognition, naming the named-rule form). If that concession is ever lifted, the
            //     synthesized name arrives with it and so does the fifth sibling.
            //   * its TYPED row mints no container class at all — the map surface is flattened onto
            //     the struct's own class — so the `MapKToV`/`PairMapKToV` hazard is unrepresentable
            //     for it, the same move that retired the family's wrapper-vs-wrapper detector.
            // What the open table DOES claim is covered by legs on the detectors above: the
            // `<K_t>List` its flattened `keys()` returns, and the catch-all row's own map class in
            // whichever flavor the row carries.
            //
            // What flattening DOES create is a MEMBER-name hazard on one class rather than a class-
            // name one, which is why it is checked here and not in that family: the accessors the
            // typed row contributes and the getter the catch-all contributes land on the SAME wasm
            // impl, so a `@name`d catch-all spelling one of the five reserved accessor names would
            // emit two methods of one name (rustc E0592 in the wasm crate).
            for msg in self.open_table_flattened_accessor_name_collisions() {
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
                // A generic DEFINITION is refused earlier in this fn (it names no concrete type),
                // and the resolution below cannot survive one: its exposability walk asserts that a
                // non-struct ident is a generic INSTANCE, which a definition is not. Skipping keeps
                // that assert an unreachable re-earning guard instead of the abort it used to be.
                if self.generic_defs.contains_key(ident) {
                    continue;
                }
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
        // A plain GROUP rule becomes a rust type only by being SPLICED into a rule that materializes
        // it (`holder = [foo]`); a group nothing splices emits no struct and no fields, so every
        // rule-position directive written on it is inert — under the rule reading AND under the
        // field reading of the slot cddl binds it to. One uniform refusal covers the whole
        // vocabulary rather than thirteen per-directive sites, because the reason is the same for
        // all of them and does not depend on which directive it is.
        //
        // Deferred to here for the reason `@no_json_schema_export` above is: splicedness is a
        // whole-spec property, decided by rules the parse seam that reads the directives has not
        // reached yet. Two directives are excluded from the list — `@name`, which gets its own
        // long-standing message just below (one misplacement, one wording), and
        // `@no_json_schema_export`, whose refusal right above already names this exact shape.
        // `@rust_name` is excluded because a NON-exported (extern-deps) scope honors it there;
        // in an exported scope the parse walk has already refused it and finalize never runs.
        // Determinism: `BTreeMap` iteration, and each directive list is sorted at its source.
        // Named by its CDDL SOURCE spelling throughout, not its `RustIdent`: an unspliced group
        // materializes no rust type, so there is no rust name to report, and every remedy below is
        // CDDL the author writes back into the spec.
        let unspliced_annotated_groups = self
            .plain_group_rule_directives
            .iter()
            .filter(|(ident, _)| !self.rust_structs.contains_key(*ident))
            .map(|(ident, directives)| {
                (
                    self.source_rule_name(ident)
                        .unwrap_or(ident.as_ref())
                        .to_owned(),
                    directives.clone(),
                )
            })
            .collect::<Vec<_>>();
        for (ident, directives) in unspliced_annotated_groups {
            if directives.contains(&"@name") {
                self.record_rejection(crate::parsing::rule_position_name_message(&ident));
            }
            let remaining = directives
                .iter()
                .copied()
                .filter(|directive| {
                    !matches!(
                        *directive,
                        "@name" | "@no_json_schema_export" | "@rust_name"
                    )
                })
                .collect::<Vec<_>>();
            if !remaining.is_empty() {
                self.record_rejection(format!(
                    "{} on `{ident}`: the plain group `{ident}` is never spliced into any rule, so \
                     it materializes no rust type and no fields — a rule-position directive on it \
                     has nothing to act on and would be silently dropped. Splice the group into a \
                     rule that materializes it (`holder = [{ident}]` for an array shape, \
                     `holder = {{{ident}}}` for a map shape), which is where a rule-position \
                     directive on a group is read, or remove the directive.",
                    remaining.join(" / ")
                ));
            }
        }
        // The `@custom_serialize`/`@custom_deserialize` pair is a TYPE-level override: it replaces
        // the codec of the rust type a rule resolves to. The parse-walk rejections cover the
        // placements that DELETE or BYPASS the node it keys on (`@no_alias`, `@newtype`, an extern /
        // raw-bytes marker, a row-entry slot). The struct-kind checks below cover the remaining
        // placements that cannot honor the pair, while preserving the audited complete-pair owners:
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
        //   - a TABLE rule (`t = { * k => v }`) carrying a LONE half. A complete pair takes the
        //     separately audited implicit map-wrapper owner; only a table left as `Table` lowers
        //     through `AliasInfo::new_manual`, whose `rule_metadata` is hardcoded `None`, so its
        //     remaining half is unhonored and rejects.
        //
        // Deferred to here rather than the parse walk for the same reason `@no_json_schema_export`
        // above is: the struct KIND decides, and a generic instance only materializes its struct
        // during the resolution above. Collected into a `BTreeSet` (determinism + no duplicate line
        // if two registrations ever land on one ident), like the float-key rejections.
        let mut custom_codec_rejections = BTreeSet::new();
        // The COLLECTION-RULE flavors of the transparent-alias `@custom_json` refusal
        // (`register_type_alias` owns the rest of that family). A named table or array rule DOES
        // register a `RustStruct`, so its config carries the flag — but the struct only exists to
        // drive the wasm wrapper and the keys-list mint; the rust rule itself lowers to a
        // transparent `pub type` alias (registered through `AliasInfo::new_manual`, which drops the
        // metadata, so `register_type_alias` cannot see these two). No consumer of `custom_json`
        // reads either shape, on either the rust or the wasm side — same inert class, same message,
        // same `@newtype` remedy. Collected here and recorded once the `&self` borrow ends.
        let mut custom_json_alias_rejections: BTreeSet<RustIdent> = BTreeSet::new();
        for (ident, rust_struct) in &self.rust_structs {
            let config = rust_struct.config();
            if config.custom_json
                && matches!(
                    rust_struct.variant(),
                    RustStructType::Table { .. } | RustStructType::Array { .. }
                )
            {
                custom_json_alias_rejections.insert(ident.clone());
            }
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
            if matches!(rust_struct.variant(), RustStructType::Table { .. }) {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: this table has only one custom-codec half; a \
                             complete pair would self-nominalize as the supported whole-table owner, \
                             but a lone half remains a transparent map alias with no codec to \
                             override and is dropped rather than honored. Put it on the rule that defines the table's \
                             KEY or VALUE type (`k = bytes ; {directive} …`, then `{ident} = \
                             {{ * k => v }}`), or declare `{ident}` as a {EXTERN_MARKER} rule and \
                             hand-write the type in full."
                        ));
                    }
                }
            }
            // The ARRAY sibling of the table rule above, and unhonored for the same reason: a named
            // collection rule (`items = [* uint]`, `[+ uint]`, `[3*5 uint]`, and both `@duplicates`
            // flavors) lowers to a transparent collection TYPEDEF registered through
            // `AliasInfo::new_manual`, whose `rule_metadata` is hardcoded `None` — so the pair
            // reaches neither the collection's standalone codec nor a holder's field call sites, and
            // ANY presence rejects rather than only a lone half. Keyed on the `Array` struct variant,
            // which is exactly the family that lowers this way (a `[a: uint]` RECORD body mints
            // `Record` and is handled below); the flavors differ only in the container the typedef
            // names (`Vec` / `NonEmptyVec` / `OrderedSet`), never in the metadata drop.
            if matches!(rust_struct.variant(), RustStructType::Array { .. }) {
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: a named collection rule (`{ident} = [* t]`) \
                             lowers to a transparent collection typedef that owns no codec for the \
                             directive to override, so it is dropped rather than honored — in both \
                             directions, whichever half is written. Put it on the rule that defines \
                             the collection's ELEMENT type (`t = bytes ; {directive} …`, then \
                             `{ident} = [* t]`), or declare `{ident}` as a {EXTERN_MARKER} rule and \
                             hand-write the type in full to own the whole collection's wire."
                        ));
                    }
                }
            }
            // A TAGGED wrapper — a tag-head rule (`x = #6.42(uint)`), and the tag-258 set idiom,
            // which nominalizes into one — is outside the one wrapper contract B3-026 audited: an
            // implicit, untagged homogeneous-table map owner with a COMPLETE pair. Do not infer the
            // semantics of tag framing, set policy, encoding preservation, or cross-face projections
            // from that narrow owner; reject either half here. Rejected on tag presence rather than
            // on `Wrapper` at large so range-bounded wrappers remain an explicit unexpanded surface.
            // `@newtype` wrappers never reach here — their parse-walk rejection short-circuits
            // `finalize` — so one misplacement still reports once.
            if let RustStructType::Wrapper { wrapped, .. } = rust_struct.variant()
                && (rust_struct.tag().is_some()
                    || wrapped
                        .encodings
                        .iter()
                        .any(|op| matches!(op, CBOREncodingOperation::Tagged(_))))
            {
                let shape = if config.set_nominal {
                    "the tag-258 set idiom, which nominalizes into a set wrapper,"
                } else {
                    "a tag-head rule (`#6.n(…)`)"
                };
                for directive in ["@custom_serialize", "@custom_deserialize"] {
                    let present = match directive {
                        "@custom_serialize" => config.custom_serialize.is_some(),
                        _ => config.custom_deserialize.is_some(),
                    };
                    if present {
                        custom_codec_rejections.insert(format!(
                            "{directive} on `{ident}`: {shape} is a tagged wrapper, while this \
                             delivery supports and audits a complete pair only on the implicit \
                             homogeneous-table map owner. Its custom-codec contract (tag framing, \
                             encoding preservation, and cross-face behavior) is not defined here. \
                             Declare `{ident}` \
                             as a {EXTERN_MARKER} rule and hand-write the type in full, or give the \
                             rule a body that resolves to a transparent alias and write the wire \
                             framing in your own codec (`{ident} = <inner> ; @custom_serialize \
                             <fn> @custom_deserialize <fn>`)."
                        ));
                    }
                }
            }
            // BOTH halves on a record rule, and the complete pair's implicit whole-table map
            // wrapper, are the accepted rule-position pairs (each gets thin generated impls
            // delegating to the named functions). They are the only struct owners where a
            // `@custom_encodings` declaration would be read into rule metadata and then have
            // nowhere to go: a struct carries its encoding metadata INSIDE itself, so no
            // codec-visible tuple crosses the boundary. Other wrapper forms remain rejected by the
            // pair checks above; this fires once and only for an accepted owner that would otherwise
            // drop the declaration silently. (A declaration with one half or none is the parse
            // walk's `reject_custom_encodings_without_pair`, so it cannot double-report here.)
            // Only parsing's complete homogeneous-table path creates an untagged, non-`@newtype`
            // map wrapper. Explicit/newtype and tagged map wrappers are rejected elsewhere and must
            // not be treated as this accepted owner merely because they wrap a map.
            let is_complete_pair_map_wrapper = matches!(
                rust_struct.variant(),
                RustStructType::Wrapper {
                    wrapped,
                    ..
                } if matches!(wrapped.conceptual_type, ConceptualRustType::Map(_, _))
                    && rust_struct.tag().is_none()
                    && config.newtype_getter.is_none()
                    && !wrapped
                        .encodings
                        .iter()
                        .any(|op| {
                            matches!(
                                op,
                                CBOREncodingOperation::Tagged(_)
                                    | CBOREncodingOperation::OptionallyTagged(_)
                            )
                        })
            );
            if config.custom_encodings.is_some()
                && config.custom_serialize.is_some()
                && config.custom_deserialize.is_some()
                && (matches!(rust_struct.variant(), RustStructType::Record(_))
                    || is_complete_pair_map_wrapper)
            {
                custom_codec_rejections.insert(format!(
                    "@custom_encodings on `{ident}`: this rule mints a STRUCT, whose encoding \
                     metadata lives inside the struct itself (its `encodings` member) — the custom \
                     pair on a record rule delegates through generated thin impls, and \
                     hands no encoding tuple across the call, so there is nothing for a declaration \
                     to describe. Put the declaration where the pair takes encoding arguments: \
                     beside a FIELD's pair, or on a transparent alias rule's pair \
                     (`<rule> = <inner> ; @custom_serialize <fn> @custom_deserialize <fn> \
                     @custom_encodings <kinds>`)."
                ));
            }
            // The `@custom_wire_major` sibling of the check above, and for the same reason: the
            // declared major is read only through the ALIAS channel (`AliasInfo::rule_metadata`),
            // when the rule keys an open table's typed row or proves a variable middle array
            // boundary. A struct-minting rule has no such channel, so the declaration would be read
            // into the rule's metadata and dropped. (A declaration with one half of the pair or
            // none is the parse walk's
            // `reject_custom_encodings_without_pair`, so it cannot double-report here.)
            if config.custom_wire_major.is_some()
                && config.custom_serialize.is_some()
                && config.custom_deserialize.is_some()
            {
                custom_codec_rejections.insert(format!(
                    "@custom_wire_major on `{ident}`: this rule mints a STRUCT, and the declared \
                     major is read only where a transparent ALIAS keys an OPEN TABLE's typed row or \
                     proves a variable middle ARRAY boundary; a struct-minting rule has no such \
                     alias entry. Put the declaration on the alias rule whose codec writes that \
                     boundary item (`<wire> = <inner> ; @custom_serialize <fn> \
                     @custom_deserialize <fn> @custom_wire_major <major>`)."
                ));
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
        // A single half on a TRANSPARENT ALIAS rule — the alias twin of the record rule's
        // single-half rejection above, refused for that rejection's own stated reason: one type
        // decodes the same bytes two ways. An alias's lone half is the more insidious shape, because
        // unlike serialize-only on a record (no `Serialize` impl at all, so the crate does not
        // compile) it COMPILES and routes — `generate_serialize`/`generate_deserialize` lift each
        // half independently, so every embed site is rewritten in the declared direction while the
        // opposite direction keeps the aliased type's generated codec. The alias then writes one wire
        // format and reads another.
        //
        // Walked over the alias table rather than the struct loop above because the alias ENTRY is
        // what "lowers to a transparent alias" means — the collection and table rules register
        // through `AliasInfo::new_manual` (whose `rule_metadata` is `None`, so they are skipped here
        // and rejected by their own kind arms), and the struct-minting kinds have no entry at all.
        // Only a rule's OWN declaration reports: `strip_alias_for_registration` copies both halves
        // wholesale, so an INHERITED single half can only descend from an origin that is itself
        // reported here, and reporting each link would name rules nobody wrote the directive on.
        let mut single_half_alias_rejections = BTreeSet::new();
        for (alias_ident, info) in &self.type_aliases {
            let AliasIdent::Rust(ident) = alias_ident else {
                continue;
            };
            if info.wire_metadata_inherited_from.is_some() {
                continue;
            }
            let Some(metadata) = info.rule_metadata.as_ref() else {
                continue;
            };
            let (directive, half, rewritten, missing) =
                match (&metadata.custom_serialize, &metadata.custom_deserialize) {
                    (Some(_), None) => (
                        "@custom_serialize",
                        "serialize",
                        "WRITES",
                        "@custom_deserialize",
                    ),
                    (None, Some(_)) => (
                        "@custom_deserialize",
                        "deserialize",
                        "READS",
                        "@custom_serialize",
                    ),
                    _ => continue,
                };
            single_half_alias_rejections.insert(format!(
                "{directive} alone on `{ident}`: a transparent alias rule with only the {half} \
                 half rewrites every embed site that {rewritten} through the named function, while \
                 the opposite direction keeps the aliased type's own generated codec — so `{ident}` \
                 reads one wire format and writes another, at every position that reaches it. Write \
                 both halves (`{ident} = <body> ; @custom_serialize <fn> @custom_deserialize \
                 <fn>`), adding the missing {missing}, or drop the directive."
            ));
        }
        for msg in single_half_alias_rejections {
            self.record_rejection(msg);
        }
        for msg in custom_codec_rejections {
            self.record_rejection(msg);
        }
        for ident in custom_json_alias_rejections {
            self.record_custom_json_on_transparent_alias_rejection(&ident);
        }
        // A custom codec whose replaced type demands NO encoding variables, under
        // `--preserve-encodings`, and with no `@custom_encodings` declaration to say what its own wire
        // needs. The pair replaces the codec, so the CODEC owns the wire — but the signature and the
        // sidecar slots are inferred from the REPLACED type, and a self-carrying leaf (an extern, a
        // record, `bool`, `any`, a `null`-fixed) infers NOTHING. Every framing byte the custom wire
        // writes is then unrecorded, and the round trip silently NORMALIZES it — invisible to a
        // round-trip test (both directions agree), visible only as a re-encoded artifact whose bytes
        // no longer hash the same. The declaration makes that state representable, so refusing the
        // undeclared spelling makes the silent one unrepresentable.
        //
        // Asked of `generation::encoding_fields_decls` — the SAME function the emission sites use to
        // build the argument list — rather than a twin predicate, so "empty demand" cannot come to
        // mean two different things. `Blind` because a pair governs its whole subtree (a declaration
        // beneath describes a codec this one's wire has swallowed). Gated on `--preserve-encodings`:
        // without it no encoding variable exists anywhere and the directive family is inert (one
        // spec, many flag sets). Skipped when rejections already exist — the demand walk reads
        // registered structs, which a failed registration may have left absent.
        if cli.preserve_encodings && !self.has_rejections() {
            let mut zero_demand_rejections = BTreeSet::new();
            for (ident, alias_info) in &self.type_aliases {
                let Some(rmd) = alias_info.rule_metadata.as_ref() else {
                    continue;
                };
                if rmd.custom_serialize.is_none()
                    || rmd.custom_deserialize.is_none()
                    || rmd.custom_encodings.is_some()
                {
                    continue;
                }
                // The codec-visible type is the alias's INNER type: the pair is lifted AT the alias
                // node, so any encoding operation the rule itself owns (`x = bytes .cbor y`) has
                // already been written by the enclosing generated code and is not the codec's to
                // record. Same slice the emission site sees one recursion level down.
                let mut codec_visible = alias_info.base_type.clone();
                codec_visible.encodings.clear();
                if crate::generation::custom_codec_demand_is_empty(self, &codec_visible, cli) {
                    zero_demand_rejections.insert(custom_codec_zero_demand_rejection(
                        &format!("rule `{ident}`"),
                        matches!(
                            codec_visible.conceptual_type.clone().resolve_aliases(),
                            ConceptualRustType::Rust(_)
                        ),
                    ));
                }
            }
            for (struct_ident, rust_struct) in &self.rust_structs {
                let RustStructType::Record(record) = rust_struct.variant() else {
                    continue;
                };
                for field in &record.fields {
                    let rmd = &field.rule_metadata;
                    if rmd.custom_serialize.is_none()
                        || rmd.custom_deserialize.is_none()
                        || rmd.custom_encodings.is_some()
                    {
                        continue;
                    }
                    // A FIELD-level pair fires at the top of the member's recursion, so its
                    // codec-visible list is the member's WHOLE type — encoding operations included
                    // (a `#6.9(uint)` field hands its tag width to the custom writer).
                    if crate::generation::custom_codec_demand_is_empty(self, &field.rust_type, cli)
                    {
                        zero_demand_rejections.insert(custom_codec_zero_demand_rejection(
                            &format!("field `{}` of `{struct_ident}`", field.name),
                            matches!(
                                field.rust_type.clone().resolve_aliases().conceptual_type,
                                ConceptualRustType::Rust(_)
                            ),
                        ));
                    }
                }
            }
            for msg in zero_demand_rejections {
                self.record_rejection(msg);
            }
        }
        // The final post-construction floor is deliberately last: generic resolution, deferred
        // wrappers and inline-set nominalization all mutate the IR name surface. It validates the
        // names that actually survive those passes; `nominal_mint_claims` above separately retains
        // claims discarded before they could survive.
        for message in self.validate_emitted_name_surface() {
            self.record_rejection(message);
        }
        // Surface any rejection recorded DURING finalize (e.g. the float-key check above, which can
        // only run post-generic-resolution). Without this the entry-point check at the top of
        // finalize would silently swallow anything recorded here.
        if self.has_rejections() {
            return Err(self.rejections_error());
        }
        Ok(())
    }

    fn validate_emitted_name_surface(&self) -> Vec<String> {
        fn spellable(name: &str) -> bool {
            is_valid_rust_ident(name) && !crate::parsing::RUST_KEYWORDS.contains(&name)
        }
        fn check_name(messages: &mut BTreeSet<String>, name: &str, family: &str, provenance: &str) {
            if !spellable(name) {
                messages.insert(format!(
                    "emitted {family} `{name}` is not a spellable Rust identifier (mint site: {provenance}). Rename it to an ASCII Rust identifier; where this spelling comes from a supported directive, use `@name <new_name>`."
                ));
            }
        }
        fn check_record(messages: &mut BTreeSet<String>, record: &RustRecord, provenance: &str) {
            let mut names = BTreeSet::new();
            for field in &record.fields {
                check_name(messages, &field.name, "record field", provenance);
                if !names.insert(field.name.clone()) {
                    messages.insert(format!(
                        "emitted record field `{}` is duplicated in its record namespace (mint site: {provenance})",
                        field.name
                    ));
                }
            }
            for row in record.captured_dynamic_rows() {
                check_name(messages, &row.field_name, "dynamic-row field", provenance);
                if !names.insert(row.field_name.clone()) {
                    messages.insert(format!(
                        "emitted dynamic-row field `{}` duplicates a field in its record namespace (mint site: {provenance})",
                        row.field_name
                    ));
                }
            }
        }

        let mut messages = BTreeSet::new();
        for (ident, rust_struct) in &self.rust_structs {
            if !ident.is_type_expression() {
                let provenance = self
                    .nominal_mint_claims
                    .get(ident)
                    .map(|claim| claim.site.as_str())
                    .or_else(|| self.source_rule_name(ident))
                    .unwrap_or("IR nominal registration");
                check_name(
                    &mut messages,
                    ident.as_ref(),
                    "nominal Rust type",
                    provenance,
                );
            }
            let enum_provenance = format!("enum `{ident}`");
            let variants = match rust_struct.variant() {
                RustStructType::TypeChoice { variants }
                | RustStructType::GroupChoice { variants, .. }
                | RustStructType::CStyleEnum { variants } => Some(variants),
                RustStructType::Record(record) => {
                    check_record(&mut messages, record, &format!("record `{ident}`"));
                    None
                }
                _ => None,
            };
            if let Some(variants) = variants {
                let mut names = BTreeSet::new();
                for variant in variants {
                    let name = variant.name.to_string();
                    let variant_provenance = [
                        format!("type choice for rule {ident}"),
                        format!("group choice for rule {ident}"),
                    ]
                    .into_iter()
                    .find_map(|context| {
                        self.variant_mint_claims.get(&context).and_then(|claims| {
                            claims
                                .iter()
                                .find(|claim| claim.emitted_name == name)
                                .map(|claim| {
                                    format!(
                                        "{context}, arm {} (`{}`; {})",
                                        claim.arm_ordinal,
                                        claim.source_name,
                                        if claim.explicit {
                                            "explicit @name"
                                        } else {
                                            "derived"
                                        },
                                    )
                                })
                        })
                    })
                    .unwrap_or_else(|| enum_provenance.clone());
                    check_name(&mut messages, &name, "enum variant", &variant_provenance);
                    if !names.insert(name.clone()) {
                        messages.insert(format!(
                            "emitted enum variant `{ident}::{name}` is duplicated in its enum namespace (mint site: {variant_provenance})"
                        ));
                    }
                    if let EnumVariantData::Inlined(record) = &variant.data {
                        check_record(
                            &mut messages,
                            record,
                            &format!("inlined variant `{ident}::{name}`"),
                        );
                    }
                }
            }
        }
        for (alias, info) in &self.type_aliases {
            if let AliasIdent::Rust(ident) = alias
                && (info.emits_rust_alias() || info.emits_wasm_alias())
                && !ident.is_type_expression()
            {
                check_name(
                    &mut messages,
                    ident.as_ref(),
                    "type alias",
                    self.source_rule_name(ident).unwrap_or("alias registration"),
                );
            }
        }
        messages.into_iter().collect()
    }

    /// Detect wasm-class name conflicts the finite/zero-minimum `BoundedVec` emission would
    /// otherwise turn into a non-compiling wasm crate. This is deliberately a per-kind sibling of
    /// the NonEmpty detector: its name embeds MIN/MAX and its pinned diagnostic tells an author
    /// which restricted representation they shadowed. In addition to the restricted class's own
    /// name, a non-exposable bounded wrapper needs the loose `<Elem>List` builder as its checked
    /// `try_from` source. A positive-minimum self-named rule cannot use `new()` and cannot borrow
    /// that same ident as a loose source, so it is rejected rather than exposing an unconstructible
    /// JS class. Zero-minimum self-named rules remain constructible by `new()` + `add`.
    fn bounded_array_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        let check_loose_source = |wrapper_ident: &str,
                                  element: &RustType,
                                  min: u64,
                                  context: &str,
                                  msgs: &mut BTreeSet<String>| {
            // Unlike the NonEmpty/reject twins, a bounded OUTER can and must take a loose source
            // even when its element is another restricted collection: `generate_array_type` gives
            // that source wrapper its own checked element boundary.
            if element.vec_of_self_directly_wasm_exposable(self) {
                return;
            }
            let loose = element.name_as_wasm_array(self);
            if loose == wrapper_ident {
                if min != 0 {
                    msgs.insert(format!(
                        "name collision: positive-minimum bounded rule '{wrapper_ident}' owns the \
                         loose '{loose}' list-builder ident it needs as its `try_from` source — \
                         rename the rule so the loose builder class can exist"
                    ));
                }
            } else if self.wasm_ident_claimed_by_user_rule(&loose)
                && !self.provides_compatible_loose_list(&loose, &element.clone().resolve_aliases())
            {
                msgs.insert(format!(
                    "name collision: rule '{loose}' claims the ident the loose '{loose}' list \
                     builder needs as the `try_from` source of {context} — rename the rule (or \
                     make it `[* …]` of the same element, which IS that builder)"
                ));
            }
        };
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Array(elem) = &rt.conceptual_type else {
                return;
            };
            if !rt.is_bounded_array() || rt.is_bounded_reject_ordered_set() {
                return;
            }
            let (min, _) = rt
                .bounded_array_u64_bounds()
                .expect("bounded occurrence bounds were validated during parsing");
            if self
                .bounded_array_named_owner(elem, rt.config.bounds.unwrap())
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded array occurrence — rename the rule to \
                     avoid shadowing the restricted BoundedVec wrapper"
                ));
            }
            check_loose_source(
                &restricted,
                elem,
                min,
                &format!("the inline bounded wrapper '{restricted}'"),
                &mut msgs,
            );
        });
        // A named bounded rule uses its own ident for the restricted class and is not visited as a
        // raw Array occurrence above. It has the same loose-source need, including the positive-min
        // self-named dead end, so audit it explicitly.
        for (ident, rs) in &self.rust_structs {
            let RustStructType::Array {
                element_type,
                bounds: Some(bounds),
            } = rs.variant()
            else {
                continue;
            };
            if rs.config().duplicates == Some(crate::comment_ast::DuplicatesPolicy::Reject) {
                continue;
            }
            let Some((min, _)) = Self::normalized_bounded_array_window(*bounds) else {
                continue;
            };
            check_loose_source(
                ident.as_ref(),
                element_type,
                min,
                &format!("the named bounded rule '{ident}'"),
                &mut msgs,
            );
        }
        // A bounded final open-array tail stores only its element in RestRow, so it is absent from
        // the generic RustType walk above. Its wasm getter nevertheless mints exactly the same
        // structural BoundedVec wrapper as an inline `[2*3 T]`; audit its restricted name and loose
        // builder source here rather than letting a user rule shadow either class during emission.
        for rs in self.rust_structs.values() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            for rest in record.dynamic_rows().filter(|row| {
                row.is_array_tail() && row.is_restricted() && !row.is_non_empty_array_tail()
            }) {
                let container = rest.container_type();
                let ConceptualRustType::Array(element) = &container.conceptual_type else {
                    unreachable!("an array rest tail's container is an Array");
                };
                let (min, _) = container
                    .bounded_array_u64_bounds()
                    .expect("bounded array-tail occurrence bounds were validated during parsing");
                if self
                    .bounded_array_named_owner(element, container.config.bounds.unwrap())
                    .is_some()
                {
                    continue;
                }
                let restricted = container.bounded_wasm_wrapper_name(self);
                if self.wasm_ident_claimed_by_user_rule(&restricted) {
                    msgs.insert(format!(
                        "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                         wrapper generated for a bounded open-array rest tail — rename the rule to \
                         avoid shadowing the restricted BoundedVec wrapper"
                    ));
                }
                check_loose_source(
                    &restricted,
                    element,
                    min,
                    &format!("the bounded open-array wrapper '{restricted}'"),
                    &mut msgs,
                );
            }
        }
        msgs.into_iter().collect()
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
    ///    wrapper, a loose `* T` tail's own getter, or the `try_from` source of a non-empty `+ T`
    ///    tail) would reference a class of the wrong shape.
    /// 4. A DIRECT claim: any of those plain uses MINTS the loose `<Elem>List` on its own, with no
    ///    `[+ …]` shape anywhere, and a user rule of the same ident and an incompatible shape
    ///    shadows it. Classes 2 and 3 both arrive through a `[+ …]` wrapper, so this is the leg a
    ///    spec containing no `[+ …]` at all reaches. The table-keys member must stay here even
    ///    though synthesis now leaves an authored claimant in place: this detector owns the
    ///    family-specific explanation of why that claimant cannot serve the keys() wrapper.
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
                    let loose_key = k.loosened_for_wasm_table_boundary_key();
                    plain_loose_needs.insert(
                        loose_key.name_as_wasm_array(self),
                        ("a map keys() wrapper".to_owned(), loose_key),
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
                        let loose_domain = domain.loosened_for_wasm_table_boundary_key();
                        plain_loose_needs.insert(
                            loose_domain.name_as_wasm_array(self),
                            ("a table keys() wrapper".to_owned(), loose_domain),
                        );
                    }
                }
                // An open struct's rest row names a list wrapper the same way a field of the row's
                // CONTAINER type would, and the IR stores the row's inner types flat, so neither the
                // `visit_all_rust_types` walk above nor the Table arm sees the claim: a `* K => V`
                // row's wasm class needs the loose `<K>List` for its `keys()`, and an array tail's
                // getter needs its loose builder or checked list class. Only a CAPTURED row mints anything — an
                // `@ignore` row has no field and no getter.
                //
                // BOTH dynamic rows, because an open table's TYPED row claims a `<K_t>List` too: its
                // map surface is FLATTENED onto the minted struct's class, so the `keys()` that
                // returns that list is the STRUCT's own method — a walk reading only the catch-all
                // would let a user rule of that ident shadow it silently.
                RustStructType::Record(record) => {
                    for rest in record.captured_dynamic_rows() {
                        if rest.is_array_tail() {
                            // A final `+ T` tail is an inline restricted Array RustType even though
                            // the generic type walk sees only its element. Its getter mints the same
                            // NonEmpty<Elem>List class as an inline `[+ T]`, so it must enter the
                            // existing restricted-wrapper collision leg before generation gets a
                            // chance to silently shadow a user rule of that ident. Bounded tails
                            // enter the sibling bounded-wrapper audit above.
                            if rest.is_non_empty_array_tail() {
                                inline_non_empty.push(rest.container_type());
                            }
                            if !rest.container_type().directly_wasm_exposable(self) {
                                plain_loose_needs.insert(
                                    rest.element().name_as_wasm_array(self),
                                    (
                                        if rest.is_non_empty_array_tail() {
                                            "a one-or-more open array `+ …` rest tail".to_owned()
                                        } else if rest.is_restricted() {
                                            "a bounded open array rest tail".to_owned()
                                        } else {
                                            "an open array `* …` rest tail".to_owned()
                                        },
                                        rest.element().clone(),
                                    ),
                                );
                            }
                        } else if !ConceptualRustType::Array(Box::new(rest.domain().clone()))
                            .directly_wasm_exposable_ct(self)
                        {
                            let loose_domain = rest.domain().loosened_for_wasm_table_boundary_key();
                            plain_loose_needs.insert(
                                loose_domain.name_as_wasm_array(self),
                                (
                                    if record.is_typed_row(rest) {
                                        "an open table's keys() wrapper".to_owned()
                                    } else {
                                        "an open struct-map rest row's keys() wrapper".to_owned()
                                    },
                                    loose_domain,
                                ),
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        // shared leg: the loose-builder need of a restricted wrapper (synthesized or free-named)
        let check_loose_need = |element: &RustType,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            if element.vec_of_self_directly_wasm_exposable(self) || element.is_non_empty_array() {
                return; // bare-Vec door: try_from takes it directly; nested: no loose source at all
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
            if element_type.vec_of_self_directly_wasm_exposable(self)
                || element_type.is_non_empty_array()
            {
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
        // consults it, and the plain mints it collects go unchecked. A named table's keys-list
        // participates here even when its structural name is already author-claimed: the synthesis
        // leaves that owner intact and this leg gives the table-specific remedy. The other plain
        // mints (rest rows, rest tails, inline `[* …]` uses) arrive through the same collected needs.
        // One leg covers all of them in this family's voice, exactly as the map side's rest-row leg
        // does for `MapKToV`.
        //
        // A rule that IS `[* elem]` of the same element is NOT a collision: it is that very builder,
        // shared by the table keys() accessor. A SELF-NAMED `[+ elem]` rule is skipped
        // too: leg (3) above owns it, with a message that names the `[+ …]` rule as the claimant.
        for (loose, (need, element)) in &plain_loose_needs {
            if self.wasm_ident_claimed_by_user_rule(loose)
                && !self.provides_compatible_loose_list(loose, &element.clone().resolve_aliases())
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
    /// Ordinary loose-table paths use `MapKToV` (`wasm_structural_map_name_for`); a bounded table
    /// uses its separately named loose direct-key source (`wasm_loose_table_builder_name_for`). A
    /// map is never directly exposable, so (unlike arrays) each such builder is a `try_from` source.
    /// Six classes:
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
    /// 6. A bounded OPEN-TABLE TYPED row stays flattened on its owner but its fallible constructor
    ///    needs one loose builder. It mints that builder only; no restricted whole-row wrapper
    ///    exists for a user rule to shadow.
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
                let name = rt.wasm_structural_map_name(self).to_string();
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
        // Named table structs are not visited as Map RustTypes. An unbounded table mints its native
        // structural map class, while a bounded table mints the explicitly loose source its
        // `try_from` door names. Non-empty tables keep their existing restricted-wrapper path.
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
                        let bounded_source = bounds.is_some_and(|candidate| {
                            Self::normalized_bounded_map_window(candidate).is_some()
                        });
                        let (name, builder_key, need) = if bounded_source {
                            (
                                RustType::wasm_loose_table_builder_name_for(
                                    domain, range, preserve, self,
                                )
                                .to_string(),
                                domain.loosened_for_wasm_table_boundary_key(),
                                "a bounded table rule's loose `try_from` source",
                            )
                        } else {
                            (
                                RustType::wasm_structural_map_name_for(
                                    domain, range, preserve, self,
                                )
                                .to_string(),
                                domain.clone(),
                                "a plain (`*`-occurrence) table rule",
                            )
                        };
                        plain_loose_needs.insert(name.clone(), need.to_owned());
                        if !preserve {
                            direct_claim_needs
                                .insert(name, (need.to_owned(), builder_key, range.clone()));
                        }
                    }
                }
                // An open struct-map rest row mints the loose builder its wasm getter returns, in
                // the flavor the row carries — invisible to the walk above because the IR stores the
                // row's key/value flat. Read the name off `RestRow::container_type`, the one
                // container spelling the emitter and the scope walk also use, so a row's claim
                // cannot drift from the class it actually mints.
                //
                // `captured_rest()`, deliberately, and NOT `captured_dynamic_rows()`: for an open
                // table that IS the catch-all row, the only one of its two rows that mints a map
                // class. The TYPED row's map surface is FLATTENED onto the minted struct's own wasm
                // class, so no `MapKToV`/`PairMapKToV` is minted for it and there is nothing for a
                // user rule to shadow — the collision is unrepresentable rather than rejected, the
                // same move that retired this family's one wrapper-vs-wrapper detector.
                RustStructType::Record(record) => {
                    let Some(rest) = record.captured_rest().filter(|r| !r.is_array_tail()) else {
                        continue;
                    };
                    let container = rest.container_type();
                    let ConceptualRustType::Map(_, _) = &container.conceptual_type else {
                        unreachable!("a map rest row's container is a Map");
                    };
                    // A bounded row's checked wrapper takes its *loose direct-key builder*, not
                    // the raw structural spelling.  The rest row stores K/V flat, so recover this
                    // source here exactly as `generate_bounded_map_type` does.  In particular a
                    // bounded collection KEY must not accidentally name its restricted key class
                    // as the loose source.
                    let preserve = container.is_preserve_pair_map();
                    let structural = if container.is_bounded_map() {
                        RustType::wasm_loose_table_builder_name_for(
                            rest.domain(),
                            rest.range(),
                            preserve,
                            self,
                        )
                        .to_string()
                    } else {
                        container.wasm_structural_map_name(self).to_string()
                    };
                    plain_loose_needs.insert(
                        structural,
                        if container.is_bounded_map() {
                            "a bounded open struct-map rest row's loose `try_from` source"
                                .to_owned()
                        } else {
                            "an open struct-map rest row".to_owned()
                        },
                    );
                    // The `+` map-row carrier has its own RESTRICTED structural class as well as
                    // the loose source above.  A captured open-struct row or open-table catch-all
                    // crosses wasm through that class; the typed row is intentionally absent here
                    // because its `+` surface is flattened on the record class and mints no map
                    // wrapper.  Reuse the inline restricted leg below so default and preserve
                    // rows receive the same flavor-correct collision diagnostic.
                    if rest.is_non_empty() {
                        inline_non_empty.push(container);
                    }
                }
                _ => {}
            }
        }

        // A bounded TYPED row has a deliberately narrow exception to the flattened-table rule:
        // wasm `new(entries_builder)` accepts one LOOSE structural builder and immediately enters
        // the native BoundedMap/Pairs `TryFrom` door. It mints neither a `Map…Min…` class nor a
        // whole-row getter, so only the builder's structural ident belongs in this detector. The
        // complete checked carrier is instead represented on the native, JSON, and component faces.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(typed) = record
                .typed_row()
                .filter(|row| row.container_type().bounded_map_u64_bounds().is_some())
            else {
                continue;
            };
            let builder = typed.staging_container_type();
            let ConceptualRustType::Map(key, value) = &builder.conceptual_type else {
                unreachable!("an open table typed row's staging carrier is a Map");
            };
            let structural = builder.wasm_structural_map_name(self).to_string();
            let need = format!("the bounded typed row of open table '{ident}'");
            plain_loose_needs.insert(structural.clone(), need.clone());
            if !builder.is_preserve_pair_map() {
                direct_claim_needs.insert(structural, (need, (**key).clone(), (**value).clone()));
            }
        }

        // shared leg: the loose-builder need of a restricted map wrapper (synthesized or named)
        let check_loose_need = |key: &RustType,
                                value: &RustType,
                                preserve: bool,
                                needed_by: &str,
                                msgs: &mut BTreeSet<String>| {
            let loose =
                RustType::wasm_structural_map_name_for(key, value, preserve, self).to_string();
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
            let loose =
                RustType::wasm_structural_map_name_for(domain, range, preserve, self).to_string();
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
        //
        // For an OPEN TABLE this leg covers the CATCH-ALL row and only it — see the note on the
        // `plain_loose_needs` Record arm above for why the typed row mints no class of its own. The
        // message names the open table's catch-all rather than a struct-map rest row so the remedy
        // reads against the shape the author actually wrote.
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
            let container = rest.container_type();
            let bounded = container.is_bounded_map();
            let (structural, key, row_kind) = if bounded {
                (
                    RustType::wasm_loose_table_builder_name_for(
                        rest.domain(),
                        rest.range(),
                        false,
                        self,
                    )
                    .to_string(),
                    rest.domain().loosened_for_wasm_table_boundary_key(),
                    "bounded",
                )
            } else {
                (
                    RustType::wasm_structural_map_name_for(
                        rest.domain(),
                        rest.range(),
                        false,
                        self,
                    )
                    .to_string(),
                    rest.domain().clone(),
                    "loose",
                )
            };
            if self.wasm_ident_claimed_by_user_rule(&structural)
                && !self.provides_compatible_loose_table(
                    &structural,
                    &key.resolve_aliases(),
                    &rest.range().clone().resolve_aliases(),
                    false,
                )
            {
                let row = if record.is_open_table() {
                    format!("the open table catch-all row of '{ident}'")
                } else {
                    format!("the open struct-map rest row of '{ident}'")
                };
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for {row} — rename the rule to avoid shadowing the {row_kind} \
                     map wrapper (or make it a `{{* …}}` table of the same key/value, which IS that \
                     wrapper)"
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

    /// Detect an authored rule shadowing the synthesized wasm class for an inline bounded table.
    /// This is deliberately the bounded-table sibling of the bounded-array and NonEmptyMap
    /// detectors: a `MapKToVMinN/MaxN` wrapper owns a BoundedMap and its one checked `try_from`
    /// door, so silently resolving that name to an unrelated rule would expose the wrong class.
    ///
    /// A same-shape named bounded table is not a collision. It is the explicit owner of the inline
    /// occurrence's surface, including through nested/generic occurrences, and therefore no
    /// structural class is synthesized for it to shadow.
    fn bounded_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Map(key, value) = &rt.conceptual_type else {
                return;
            };
            let Some(bounds) = rt.bounded_map_u64_bounds() else {
                return;
            };
            if rt.is_bounded_pair_map() {
                return;
            }
            let source_bounds = rt
                .config
                .bounds
                .expect("bounded map occurrence carries bounds");
            if self
                .bounded_map_named_owner(key, value, source_bounds, rt.is_preserve_pair_map())
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = bounds;
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded `{{{window} …}}` table occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedMap wrapper"
                ));
            }
        });
        // Rest rows keep K/V flat in the Record IR, so the generic RustType walk above cannot see
        // the restricted BoundedMap class their wasm getter/constructor actually names.  Only a
        // CAPTURED open-struct rest or open-table catch-all reaches that class; a typed row remains
        // flattened on its owner and is deliberately covered only by its loose-builder audit.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record.captured_rest().filter(|row| {
                !row.is_array_tail()
                    && row.container_type().is_bounded_map()
                    && !row.container_type().is_bounded_pair_map()
            }) else {
                continue;
            };
            let container = rest.container_type();
            let source_bounds = container
                .config
                .bounds
                .expect("bounded rest map carries occurrence bounds");
            if self
                .bounded_map_named_owner(rest.domain(), rest.range(), source_bounds, false)
                .is_some()
            {
                continue;
            }
            let restricted = container.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = container
                    .bounded_map_u64_bounds()
                    .expect("bounded rest map has representable occurrence bounds");
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                let row = if record.is_open_table() {
                    format!("the bounded catch-all row of open table '{ident}'")
                } else {
                    format!("the bounded open struct-map rest row of '{ident}'")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for {row} (`{window}`) — rename the rule to avoid \
                     shadowing the restricted BoundedMap wrapper"
                ));
            }
        }
        msgs.into_iter().collect()
    }

    /// Bounded preserve tables mint a distinct `PairMap…MinN/MaxN` class. This stays a parallel
    /// detector so its remedy names the duplicate-preserving carrier rather than the keyed-map twin.
    fn bounded_pair_map_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            let ConceptualRustType::Map(key, value) = &rt.conceptual_type else {
                return;
            };
            let Some(bounds) = rt.bounded_map_u64_bounds() else {
                return;
            };
            if !rt.is_bounded_pair_map() {
                return;
            }
            let source_bounds = rt
                .config
                .bounds
                .expect("bounded pair-map occurrence carries bounds");
            // A same-shape preserve table is the authored owner of this class, just as a bounded
            // unique-key table is for `BoundedMap`; only an unrelated rule is a collision.
            if self
                .bounded_map_named_owner(key, value, source_bounds, true)
                .is_some()
            {
                return;
            }
            let restricted = rt.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = bounds;
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for an inline bounded `@duplicates preserve` `{{{window} …}}` \
                     table occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedPairMap wrapper"
                ));
            }
        });
        // The duplicate-preserving rest-row twin is separate on purpose: its structural name and
        // remediation name the BoundedPairMap carrier, not the unique-key BoundedMap above.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(rest) = record
                .captured_rest()
                .filter(|row| !row.is_array_tail() && row.container_type().is_bounded_pair_map())
            else {
                continue;
            };
            let container = rest.container_type();
            let source_bounds = container
                .config
                .bounds
                .expect("bounded preserve rest map carries occurrence bounds");
            if self
                .bounded_map_named_owner(rest.domain(), rest.range(), source_bounds, true)
                .is_some()
            {
                continue;
            }
            let restricted = container.bounded_wasm_map_structural_name(self);
            if self.wasm_ident_claimed_by_user_rule(&restricted) {
                let (min, max) = container
                    .bounded_map_u64_bounds()
                    .expect("bounded preserve rest map has representable occurrence bounds");
                let window = if max == u64::MAX {
                    format!("{min}*")
                } else {
                    format!("{min}*{max}")
                };
                let row = if record.is_open_table() {
                    format!(
                        "the bounded `@duplicates preserve` catch-all row of open table '{ident}'"
                    )
                } else {
                    format!(
                        "the bounded `@duplicates preserve` open struct-map rest row of '{ident}'"
                    )
                };
                msgs.insert(format!(
                    "name collision: rule '{restricted}' collides with the '{restricted}' wasm \
                     wrapper generated for {row} (`{window}`) — rename the rule to avoid \
                     shadowing the restricted BoundedPairMap wrapper"
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
            let mut reject_array =
                RustType::new(ConceptualRustType::Array(Box::new(element_type.clone())))
                    .with_duplicates_policy(Some(crate::comment_ast::DuplicatesPolicy::Reject));
            if let Some(bounds) = bounds {
                reject_array = reject_array.with_bounds(*bounds);
            }
            let structural = if reject_array.is_bounded_reject_ordered_set() {
                reject_array.bounded_reject_ordered_set_wasm_wrapper_name(self)
            } else {
                reject_array.reject_ordered_set_wasm_wrapper_name(self)
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
            let ConceptualRustType::Array(_) = &wrapped.conceptual_type else {
                continue;
            };
            let structural = if wrapped.is_bounded_reject_ordered_set() {
                wrapped.bounded_reject_ordered_set_wasm_wrapper_name(self)
            } else {
                wrapped.reject_ordered_set_wasm_wrapper_name(self)
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

    /// Bounded reject sets have their own structural wasm class: both the uniqueness flavor and
    /// occurrence endpoints are encoded, so the class cannot be confused with `BoundedVec` or an
    /// unbounded `OrderedSet`. Keep this a per-kind sibling of the other collision detectors: the
    /// pinned remedy must name the carrier an author is actually shadowing.
    fn bounded_reject_ordered_set_wrapper_name_collisions(&self) -> Vec<String> {
        let mut msgs = BTreeSet::new();
        self.visit_all_rust_types(&mut |rt| {
            if !rt.is_bounded_reject_ordered_set() {
                return;
            }
            let structural = rt.bounded_reject_ordered_set_wasm_wrapper_name(self);
            if self.wasm_ident_claimed_by_user_rule(&structural) {
                msgs.insert(format!(
                    "name collision: rule '{structural}' collides with the '{structural}' wasm \
                     wrapper generated for an inline bounded `@duplicates reject` set occurrence — \
                     rename the rule to avoid shadowing the restricted BoundedOrderedSet wrapper"
                ));
            }
        });
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
    /// alias claims the ident beside the class), a named preserve restricted table whose `try_from`
    /// source is the loose pair-map builder (including its bounded direct-key source), and a
    /// `@newtype`/TAG-forced WRAPPER over an inline
    /// `{* k => v} ; @duplicates preserve` (its `new`/getter boundary names the structural class,
    /// which the wasm struct walk mints for exactly that inner). A user rule that IS a plain preserve
    /// `{* k => v}` table of
    /// the same key/value is not a collision — that rule IS the builder (shared, exactly as the
    /// default-flavored sibling shares it); a rule that is the WRAPPER is, because a wrapper is a
    /// nominal type of its own and cannot double as the builder class. Message text is deliberately
    /// distinct from the other kinds'
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
                    let bounded_source = bounds.is_some_and(|candidate| {
                        Self::normalized_bounded_map_window(candidate).is_some()
                    });
                    let (structural, builder_key, source) = if bounded_source {
                        (
                            RustType::wasm_loose_table_builder_name_for(domain, range, true, self)
                                .to_string(),
                            domain.loosened_for_wasm_table_boundary_key(),
                            true,
                        )
                    } else {
                        (
                            RustType::wasm_structural_map_name_for(domain, range, true, self)
                                .to_string(),
                            domain.clone(),
                            *bounds == Some((Some(1), None)),
                        )
                    };
                    // A self-named loose or `{+}` rule legitimately owns the ident for its own
                    // class. A bounded rule does not: its restricted wrapper and loose checked
                    // source would otherwise claim the same ident with incompatible carriers.
                    if !bounded_source && structural == ident.to_string() {
                        continue;
                    }
                    let minted_by = if *bounds == Some((Some(1), None)) {
                        format!(
                            "the `@duplicates preserve` `{{+ …}}` rule '{ident}'s `try_from` source"
                        )
                    } else if source {
                        format!(
                            "the bounded `@duplicates preserve` rule '{ident}'s loose `try_from` source"
                        )
                    } else {
                        format!("the `@duplicates preserve` table rule '{ident}'")
                    };
                    check(structural, &builder_key, range, minted_by, &mut msgs);
                }
                RustStructType::Record(record) => {
                    // CAPTURED rows only: an `@ignore` row has no field and no getter, so it mints
                    // no wrapper and can claim no ident (same gate as the mint itself).
                    //
                    // `captured_rest()` and not `captured_dynamic_rows()`, for the reason its
                    // default-flavored twin in `non_empty_map_wrapper_name_collisions` records: an
                    // open table's TYPED row is flattened onto the minted struct's own wasm class
                    // and mints no PairMap container, so it has no ident to be shadowed.
                    let Some(rest) = record.captured_rest().filter(|r| {
                        !r.is_array_tail()
                            && r.duplicates()
                                == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
                    }) else {
                        continue;
                    };
                    let container = rest.container_type();
                    let bounded = container.is_bounded_map();
                    let (structural, builder_key) = if bounded {
                        (
                            RustType::wasm_loose_table_builder_name_for(
                                rest.domain(),
                                rest.range(),
                                true,
                                self,
                            )
                            .to_string(),
                            rest.domain().loosened_for_wasm_table_boundary_key(),
                        )
                    } else {
                        (
                            RustType::wasm_structural_map_name_for(
                                rest.domain(),
                                rest.range(),
                                true,
                                self,
                            )
                            .to_string(),
                            rest.domain().clone(),
                        )
                    };
                    check(
                        structural,
                        &builder_key,
                        rest.range(),
                        if record.is_open_table() {
                            if bounded {
                                format!(
                                    "the bounded `@duplicates preserve` catch-all row of '{ident}' \
                                     as its loose `try_from` source"
                                )
                            } else {
                                format!("the `@duplicates preserve` catch-all row of '{ident}'")
                            }
                        } else {
                            if bounded {
                                format!(
                                    "the bounded `@duplicates preserve` rest row of '{ident}' as \
                                     its loose `try_from` source"
                                )
                            } else {
                                format!("the `@duplicates preserve` rest row of '{ident}'")
                            }
                        },
                        &mut msgs,
                    );
                }
                // A `@newtype`- or TAG-forced wrapper over an inline `{* k => v} ; @duplicates
                // preserve`: the wasm struct walk mints the structural `PairMapKToV` class its
                // `new`/getter boundary names. The `{+ …}` flavor is NOT here — its restricted
                // `NonEmptyPairMapKToV` class and its loose `try_from` source are both claimed by
                // `non_empty_map_wrapper_name_collisions`' inline leg, whose naming is flavor-aware,
                // so routing it here too would emit two messages for one collision.
                RustStructType::Wrapper { wrapped, .. } => {
                    if !wrapped.is_preserve_pair_map() || wrapped.is_non_empty_map() {
                        continue;
                    }
                    let ConceptualRustType::Map(domain, range) = &wrapped.conceptual_type else {
                        unreachable!("is_preserve_pair_map implies a Map conceptual type");
                    };
                    let structural = wrapped.wasm_structural_map_name(self).to_string();
                    check(
                        structural,
                        domain,
                        range,
                        format!("the `@duplicates preserve` table wrapped by rule '{ident}'"),
                        &mut msgs,
                    );
                }
                _ => {}
            }
        }
        // A bounded TYPED row is flattened on its record class, but its fallible wasm constructor
        // receives this one loose PairMap builder. It mints no restricted whole-row class, so audit
        // precisely that builder ident and no fictional `PairMap…Min…` wrapper. The default-flavored
        // sibling is the final Record loop in
        // `non_empty_map_wrapper_name_collisions`.
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            let Some(typed) = record.typed_row().filter(|row| {
                row.container_type().bounded_map_u64_bounds().is_some()
                    && row.duplicates() == Some(crate::comment_ast::DuplicatesPolicy::Preserve)
            }) else {
                continue;
            };
            let builder = typed.staging_container_type();
            let ConceptualRustType::Map(key, value) = &builder.conceptual_type else {
                unreachable!("an open table typed row's staging carrier is a Map");
            };
            check(
                builder.wasm_structural_map_name(self).to_string(),
                key,
                value,
                format!(
                    "the bounded `@duplicates preserve` typed row of open table \
                     '{ident}'"
                ),
                &mut msgs,
            );
        }
        msgs.into_iter().collect()
    }

    /// The open table's own member-name check, and the only wasm-surface hazard its minted class
    /// creates. Flattening the TYPED row's map surface onto the class puts `len`/`insert`/`get`/
    /// `has`/`keys` on the same `#[wasm_bindgen]` impl the CATCH-ALL row's getter lands on, and that
    /// getter is named by the row (`rest` by default, anything under `@name`). A row named for one
    /// of the five would emit two methods of one name — E0592 in the generated wasm crate, at the
    /// exact remove where the spec author cannot see it.
    ///
    /// All five are reserved unconditionally, `has` included even though it is emitted only for a
    /// nullable typed value: making the reservation depend on the VALUE's nullability would mean a
    /// row name that generates today stops generating when an unrelated `/ null` is added to the
    /// typed row, which is a worse surprise than the flat rule.
    fn open_table_flattened_accessor_name_collisions(&self) -> Vec<String> {
        // BTreeSet: deterministic message order (repo determinism invariant)
        let mut msgs = BTreeSet::new();
        const FLATTENED_ACCESSORS: &[&str] = &["get", "has", "insert", "keys", "len"];
        for (ident, rs) in self.rust_structs.iter() {
            let RustStructType::Record(record) = rs.variant() else {
                continue;
            };
            if !record.is_open_table() {
                continue;
            }
            let Some(rest) = record.captured_rest() else {
                continue;
            };
            if FLATTENED_ACCESSORS.contains(&rest.field_name.as_str()) {
                let reserved = FLATTENED_ACCESSORS
                    .iter()
                    .map(|a| format!("`{a}`"))
                    .collect::<Vec<_>>()
                    .join(", ");
                msgs.insert(format!(
                    "name collision: the open table '{ident}' names its catch-all row '{}', which \
                     is one of the accessors ({reserved}) its wasm class flattens onto itself from \
                     the TYPED row — rename the row with `@name` so the catch-all getter and the \
                     flattened accessor do not claim one method",
                    rest.field_name,
                ));
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
    /// `{+ …}` occurrences are covered by the default-flavored twin
    /// (`non_empty_map_wrapper_name_collisions`), whose inline leg reads the flavor off the
    /// occurrence — which is what covers the one preserve-flavored inline shape that IS expressible:
    /// the inner of a `@newtype`/tag-forced wrapper over `{+ k => v} ; @duplicates preserve`, whose
    /// restricted class and loose `try_from` source that leg claims together (one collision, one
    /// message).
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
                RustType::wasm_structural_map_name_for(domain, range, true, self)
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
                    // A plain group normally materializes via `parse_group` below as a Record or
                    // GroupChoice. A pre-existing non-group owner is instead a generated-name
                    // collision; reject it in the global registration's voice rather than panicking
                    // before that seam can receive the group's would-be claim. This is reachable
                    // for an authored `Int` plain group, whose source spelling does not release the
                    // pre-registered lowercase-`int` prelude marker.
                    Some(None) => {
                        let existing_kind = self
                            .rust_structs
                            .get(ident)
                            .map(rust_struct_kind)
                            .expect("plain-group materialization observed an existing owner");
                        self.record_rejection(format!(
                            "generated Rust type `{ident}` has incompatible registrations: the first claimant \
                             is a {existing_kind}, but the later claimant is a plain group. Keep one wire shape \
                             per generated Rust name; rename one authored rule or the synthesized claimant that \
                             collides with it."
                        ));
                    }
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

    /// Whether `name` is a plain group declared by an authored CDDL rule, rather than an internal
    /// group-choice arm registered with `PlainGroupInfo::group == None`. The distinction matters at
    /// resolved source-reference legality seams: synthesized arm names can temporarily coincide
    /// with a generic parameter (`A`) while a generic type-choice body is being built, but that
    /// parameter still denotes a TYPE and must not inherit plain-group-only restrictions.
    pub fn is_directly_defined_plain_group(&self, name: &RustIdent) -> bool {
        self.plain_groups
            .get(name)
            .is_some_and(|info| info.group.is_some())
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
        let removed = self.rust_structs.remove(ident);
        // Group-choice parsing uses this only for temporary arm records that are inlined or
        // structurally shared. Their registrations never become nominal declarations, so retract
        // only the matching ordinary-registration claim; a semantic pre-registration claim (for
        // example a fixed singleton) remains evidence even if another owner is removed.
        if removed.is_some()
            && self
                .nominal_mint_claims
                .get(ident)
                .is_some_and(|claim| claim.site == Self::registration_mint_site(ident))
        {
            self.nominal_mint_claims.remove(ident);
        }
        removed
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

    /// The whole `@extern_companions` registry, keyed by declaring marker rule. Empty unless some
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

    /// Record that `name`'s rule carries `@no_alias`. Called from the parse seam that reads a rule's
    /// metadata, unconditionally — whether the rule ends up registering an alias at all is decided
    /// later, and by several different paths (see the `no_alias_rules` field comment).
    pub fn mark_no_alias_rule(&mut self, name: RustIdent) {
        self.no_alias_rules.insert(name);
    }

    /// Whether `name`'s rule asked for its transparent `pub type` to be suppressed. Read by
    /// `register_type_alias` (which enforces it) and by the extern-interface projection (which must
    /// tell a consumer, since the suppressed name is one the dep no longer materializes).
    pub fn is_no_alias_rule(&self, name: &RustIdent) -> bool {
        self.no_alias_rules.contains(name)
    }

    /// Record `name`'s rule-level `@doc` text. Called from the same parse seam as
    /// `mark_no_alias_rule`, unconditionally — which construct (if any) ends up carrying it is
    /// decided later (see the `rule_docs` field comment).
    pub fn mark_rule_doc(&mut self, name: RustIdent, doc: String) {
        self.rule_docs.insert(name, doc);
    }

    /// The rule-level `@doc` written on `name`'s rule, for the construct builders whose own config
    /// cannot carry it.
    pub fn rule_doc(&self, name: &RustIdent) -> Option<&str> {
        self.rule_docs.get(name).map(String::as_str)
    }

    /// Record that `name`'s rule carries `@custom_json`. Called from the same parse seams as
    /// `mark_no_alias_rule`/`mark_rule_doc`, unconditionally — which construct (if any) ends up
    /// carrying it is decided later (see the `custom_json_rules` field comment).
    pub fn mark_custom_json_rule(&mut self, name: RustIdent) {
        self.custom_json_rules.insert(name);
    }

    /// Record the rule-position directives written on the plain GROUP rule `name`, for the
    /// never-spliced refusal in `finalize` (see the `plain_group_rule_directives` field comment). A
    /// group with none is not recorded, so the refusal walk only ever visits annotated groups.
    pub fn mark_plain_group_rule_directives(
        &mut self,
        name: RustIdent,
        directives: Vec<&'static str>,
    ) {
        if !directives.is_empty() {
            self.plain_group_rule_directives.insert(name, directives);
        }
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
/// Exact lowercase `int` is excluded from the keyword branch identically to `RustIdent::new`: at
/// `api::with_types`' pre-parse lifecycle seam it releases the project's built-in `Int` marker, so
/// an authored `int` rule may become that type's owner. A different source spelling that camel-cases
/// to `Int` does not release the marker and is rejected by global struct registration instead.
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

/// The `--preserve-encodings` refusal for a custom (de)serializer pair whose replaced type demands no
/// encoding variables and which declares none of its own. `position` names where the pair is written
/// (a rule, or a field of a struct); `replaced_is_named_type` selects the extra remedy that only
/// applies when the replaced type is one this crate does not codegen — its own impls already own the
/// wire, so the pair has nothing to add.
///
/// Message text is pinned by `dsl_position_tests` and by the fixture suite; keep the three remedies
/// (declare the wire, declare `none`, drop the pair) present in any rewording.
fn custom_codec_zero_demand_rejection(position: &str, replaced_is_named_type: bool) -> String {
    let extern_remedy = if replaced_is_named_type {
        " If the replaced type has ONE wire — a hand-written extern, say — it does not need the pair \
         at all: its own `Serialize`/`Deserialize` impls own the wire, encodings included, and \
         dropping the pair records them."
    } else {
        ""
    };
    format!(
        "@custom_serialize/@custom_deserialize on {position}: under `--preserve-encodings` this \
         pair replaces the codec of a type that demands NO encoding variables, so the custom wire's \
         framing (int/tag widths, string headers, container lengths) is recorded nowhere and the \
         round trip silently normalizes it — both directions agree, so no round-trip test can see \
         it. Declare what the custom wire needs beside the pair (`@custom_encodings <kinds>`, a \
         comma-separated list of `sz` / `str` / `len`), or state that it needs nothing \
         (`@custom_encodings none`) if the wire genuinely has no framing.{extern_remedy}"
    )
}

#[cfg(test)]
mod registration_tests {
    use super::*;
    use clap::Parser;

    fn cli() -> Cli {
        Cli::parse_from([
            "cddl-codegen",
            "--input",
            "registration_test_input",
            "--output",
            "registration_test_output",
            "--wasm=false",
        ])
    }

    #[test]
    fn register_rust_struct_keeps_first_incompatible_owner_in_both_orders() {
        for first_is_extern in [true, false] {
            let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
            let parent_visitor = ParentVisitor::new(&cddl).unwrap();
            let cli = Cli::parse_from([
                "cddl-codegen",
                "--input",
                "registration_test_input",
                "--output",
                "registration_test_output",
                "--wasm=false",
            ]);
            let ident = RustIdent::new(CDDLIdent::new("contested"));
            let mut types = IntermediateTypes::new();
            let first = if first_is_extern {
                RustStruct::new_extern(ident.clone())
            } else {
                RustStruct::new_raw_bytes(ident.clone())
            };
            let second = if first_is_extern {
                RustStruct::new_raw_bytes(ident.clone())
            } else {
                RustStruct::new_extern(ident.clone())
            };

            types.register_rust_struct(&parent_visitor, first, &cli);
            types.register_rust_struct(&parent_visitor, second, &cli);

            assert!(
                matches!(
                    types.rust_struct(&ident).unwrap().variant(),
                    RustStructType::Extern
                ) == first_is_extern,
                "the first incompatible owner must stay registered"
            );
            let err = types
                .finalize(&parent_visitor, &cli)
                .expect_err("an incompatible duplicate registration must reject gracefully")
                .to_string();
            assert!(
                err.contains("generated Rust type `Contested` has incompatible registrations")
                    && err.contains("extern marker")
                    && err.contains("raw-bytes marker"),
                "the rejection must name the contested ident and both structural kinds: {err}"
            );
        }
    }

    #[test]
    fn register_rust_struct_accepts_structurally_equivalent_reuse() {
        let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
        let parent_visitor = ParentVisitor::new(&cddl).unwrap();
        let cli = Cli::parse_from([
            "cddl-codegen",
            "--input",
            "registration_test_input",
            "--output",
            "registration_test_output",
            "--wasm=false",
        ]);
        let ident = RustIdent::new(CDDLIdent::new("shared"));
        let mut types = IntermediateTypes::new();

        types.register_rust_struct(&parent_visitor, RustStruct::new_extern(ident.clone()), &cli);
        types.register_rust_struct(&parent_visitor, RustStruct::new_extern(ident.clone()), &cli);

        types
            .finalize(&parent_visitor, &cli)
            .expect("byte-identical registrations must reuse the first owner");
        assert!(matches!(
            types.rust_struct(&ident).unwrap().variant(),
            RustStructType::Extern
        ));
    }

    #[test]
    fn nominal_mint_claims_reject_pre_registration_loss_in_both_orders() {
        for bare_first in [true, false] {
            let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
            let parent_visitor = ParentVisitor::new(&cddl).unwrap();
            let ident = RustIdent::new(CDDLIdent::new("fixed_bool_true"));
            let bare = RustStruct::new_fixed_singleton(
                ident.clone(),
                None,
                None,
                RustType::new(ConceptualRustType::Fixed(FixedValue::Bool(true))),
            );
            let tagged = RustStruct::new_fixed_singleton(
                ident,
                Some(7),
                None,
                RustType::new(ConceptualRustType::Fixed(FixedValue::Bool(true))),
            );
            let mut types = IntermediateTypes::new();
            let (first, first_site, second, second_site) = if bare_first {
                (
                    &bare,
                    "bare fixed singleton",
                    &tagged,
                    "tagged fixed singleton",
                )
            } else {
                (
                    &tagged,
                    "tagged fixed singleton",
                    &bare,
                    "bare fixed singleton",
                )
            };
            types.claim_nominal_mint(first, first_site);
            // Model the fixed-singleton minter's early `rust_struct` return: no registration of
            // the second semantic claimant is needed for the ledger to retain and reject it.
            types.claim_nominal_mint(second, second_site);
            let err = types
                .finalize(&parent_visitor, &cli())
                .expect_err("different wire identities sharing a pre-registration name must reject")
                .to_string();
            assert!(
                err.contains("incompatible mint claims")
                    && err.contains("bare fixed singleton")
                    && err.contains("tagged fixed singleton"),
                "both mint sites must survive either claimant order: {err}"
            );
        }
    }

    #[test]
    fn finalized_emitted_name_floor_reports_bad_names_and_duplicate_namespaces() {
        let mut types = IntermediateTypes::new();
        let bad_nominal = RustIdent::new_unchecked_for_emitted_name_test("Bad.Name");
        types
            .rust_structs
            .insert(bad_nominal.clone(), RustStruct::new_extern(bad_nominal));

        let enum_ident = RustIdent::new(CDDLIdent::new("choices"));
        let primitive = RustType::new(ConceptualRustType::Primitive(Primitive::U64));
        types.reserve_explicit_variant_mint(
            "type choice for rule Choices",
            1,
            "self".to_owned(),
            "Self".to_owned(),
        );
        types.rust_structs.insert(
            enum_ident.clone(),
            RustStruct::new_type_choice(
                enum_ident,
                None,
                None,
                vec![
                    EnumVariant::new(
                        VariantIdent::new_custom("Self"),
                        primitive.clone(),
                        false,
                        None,
                    ),
                    EnumVariant::new(
                        VariantIdent::new_custom("Self"),
                        primitive.clone(),
                        false,
                        None,
                    ),
                ],
                &cli(),
            ),
        );

        let record_ident = RustIdent::new(CDDLIdent::new("record"));
        let record = RustRecord {
            rep: Representation::Array,
            fields: vec![
                RustField::new(
                    "bad.name".to_owned(),
                    primitive.clone(),
                    false,
                    None,
                    RuleMetadata::default(),
                ),
                RustField::new(
                    "bad.name".to_owned(),
                    primitive.clone(),
                    false,
                    None,
                    RuleMetadata::default(),
                ),
            ],
            forbidden_fields: vec![],
            rest: Some(Box::new(RestRow {
                kind: RestKind::ArrayTail {
                    element: primitive,
                    source_index: 2,
                },
                semantics: RestSemantics::Capture,
                field_name: "bad.name".to_owned(),
                dispatch_major: None,
                occurrence: None,
            })),
            array_segments: vec![],
            typed_row: None,
        };
        types.rust_structs.insert(
            record_ident.clone(),
            RustStruct::new_record(record_ident, None, None, record),
        );

        let messages = types.validate_emitted_name_surface().join("\n");
        for needle in [
            "nominal Rust type `Bad.Name`",
            "mint site:",
            "enum variant `Choices::Self`",
            "arm 1 (`self`; explicit @name)",
            "duplicated in its enum namespace",
            "record field `bad.name`",
            "dynamic-row field `bad.name`",
            "duplicates a field in its record namespace",
        ] {
            assert!(
                messages.contains(needle),
                "missing `{needle}` in:\n{messages}"
            );
        }
    }

    #[test]
    fn variant_mint_claims_are_idempotent_per_arm_and_retain_provenance() {
        let mut types = IntermediateTypes::new();
        let context = "type choice for rule Choice";
        assert!(types
            .reserve_explicit_variant_mint(
                context,
                2,
                "chosen".to_owned(),
                "Chosen".to_owned(),
            )
            .is_none());
        assert!(types
            .reserve_explicit_variant_mint(
                context,
                2,
                "chosen".to_owned(),
                "Chosen".to_owned(),
            )
            .is_none(), "a revisited explicit arm is not a second claimant");
        assert_eq!(
            types.settle_derived_variant_mint(context, 1, "tstr".to_owned(), "Text".to_owned()),
            "Text"
        );
        assert_eq!(
            types.settle_derived_variant_mint(context, 1, "tstr".to_owned(), "Text".to_owned()),
            "Text",
            "a revisited derived arm must retain its original spelling rather than suffix"
        );
        let claims = types.variant_mint_claims.get(context).unwrap();
        assert_eq!(claims.len(), 2);
        assert!(claims.iter().any(|claim| {
            claim.arm_ordinal == 2
                && claim.source_name == "chosen"
                && claim.emitted_name == "Chosen"
                && claim.explicit
        }));
    }

    #[test]
    fn variant_mint_claim_drift_for_one_arm_is_rejected() {
        let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
        let parent_visitor = ParentVisitor::new(&cddl).unwrap();
        let mut types = IntermediateTypes::new();
        let context = "type choice for rule Choice";
        assert_eq!(
            types.settle_derived_variant_mint(context, 1, "uint".to_owned(), "Uint".to_owned()),
            "Uint"
        );
        // Same ordinal is one semantic enum arm. A different source/base is not an AST revisit:
        // retaining the first claim and rejecting the second makes the drift deterministic.
        assert_eq!(
            types.settle_derived_variant_mint(context, 1, "tstr".to_owned(), "Text".to_owned()),
            "Uint"
        );
        let error = types
            .finalize(&parent_visitor, &cli())
            .expect_err("a changed claim for one arm must reject")
            .to_string();
        assert!(
            error.contains("variant mint claim drift in type choice for rule Choice")
                && error.contains("arm 1")
                && error.contains("source `uint` as `Uint`")
                && error.contains("source `tstr`")
                && error.contains("derived base `Text`"),
            "the drift error must retain both claims: {error}"
        );
    }

    #[test]
    fn explicit_variant_claim_drift_rejects_without_leaking_inline_key_identity() {
        let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
        let parent_visitor = ParentVisitor::new(&cddl).unwrap();
        let mut types = IntermediateTypes::new();
        // The pointer suffix is a private registry discriminator only. A drift error must not
        // make an otherwise deterministic rejection vary across processes.
        let context = "inline type choice at 0xDEADBEEF";
        assert!(
            types
                .reserve_explicit_variant_mint(context, 1, "first".to_owned(), "First".to_owned(),)
                .is_none()
        );
        assert!(types
            .reserve_explicit_variant_mint(
                context,
                1,
                "second".to_owned(),
                "Second".to_owned(),
            )
            .is_none());
        let error = types
            .finalize(&parent_visitor, &cli())
            .expect_err("an explicit re-entry with changed source/name must reject")
            .to_string();
        assert!(
            error.contains("variant mint claim drift in an inline type choice")
                && error.contains("source `first` as `First`")
                && error.contains("source `second` as `Second`")
                && !error.contains("0xDEADBEEF"),
            "the explicit drift must retain both claims but hide process-local key state: {error}"
        );
    }
}

mod idents;
use crate::cli::Cli;
pub use idents::*;

mod structs;
pub use structs::*;
