use cbor_event::{Special, Type as CBORType};
use cddl::ast::parent::ParentVisitor;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::cli::Cli;
use crate::comment_ast::{DemandSet, DuplicatesPolicy, RuleMetadata};
use crate::parsing::EXTERN_MARKER;
use crate::utils::{
    cddl_prelude, convert_to_camel_case, convert_to_snake_case, is_identifier_reserved,
    is_valid_rust_ident,
};

mod finalize;
mod idents;
mod inline_sets;
mod name_rejections;
mod runtime_usage;
mod rust_type;
mod scope_refs;
mod structs;
mod wrapper_collisions;
pub use idents::*;
pub use name_rejections::{
    dotted_ident_rejection, reserved_ident_rejection, reserved_pin_rejection,
};
pub use rust_type::*;
pub use structs::*;

use std::sync::LazyLock;
pub static ROOT_SCOPE: LazyLock<ModuleScope> = LazyLock::new(|| vec![String::from("lib")].into());

/// The ident of the reserved prelude extern carrying the full CBOR integer range (`int`).
pub(crate) const RESERVED_INT_IDENT: &str = "Int";

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

    /// Make a new ModuleScope using only the first `depth` components
    pub fn parents(&self, depth: usize) -> Self {
        Self {
            export: self.export,
            scope: self.scope.as_slice()[0..depth].to_vec(),
        }
    }

    pub fn export(&self) -> bool {
        self.export
    }

    pub fn components(&self) -> &[String] {
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
    gen_rust_alias: bool,
    gen_wasm_alias: bool,
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
    fn new_manual(base_type: RustType, gen_rust_alias: bool, gen_wasm_alias: bool) -> Self {
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

    /// A metadata-less alias that declares no `pub type` on either face (the CDDL prelude names).
    pub fn transparent(base_type: RustType) -> Self {
        Self::new_manual(base_type, false, false)
    }

    /// A metadata-less alias that declares a Rust `pub type` only.
    /// The wasm face resolves through the base type (collection-rule and generic-extern aliases).
    pub fn rust_only(base_type: RustType) -> Self {
        Self::new_manual(base_type, true, false)
    }

    /// A metadata-less alias declared on both faces (a named generic-set binding).
    pub fn rust_and_wasm(base_type: RustType) -> Self {
        Self::new_manual(base_type, true, true)
    }

    /// `@no_alias` suppresses both declared aliases, whatever the constructor chose.
    pub fn suppress_declared_aliases(&mut self) {
        self.gen_rust_alias = false;
        self.gen_wasm_alias = false;
    }

    /// Route an anonymous collection instance through its wasm wrapper passthrough.
    /// See `converge_anonymous_collection_instance_wasm` for the structural wrapper route.
    pub fn enable_wasm_passthrough(&mut self) {
        self.gen_wasm_alias = true;
    }

    /// The stored Rust declaration before custom-pair suppression by `emits_rust_alias`.
    /// Read this only where a pair-carrying alias must still count as declared.
    pub fn declared_rust_alias(&self) -> bool {
        self.gen_rust_alias
    }

    /// The stored wasm declaration before custom-pair suppression by `emits_wasm_alias`.
    pub fn declared_wasm_alias(&self) -> bool {
        self.gen_wasm_alias
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

/// Authored per-rule intent, kept separate from derived demand, emission and ownership facts.
#[derive(Default)]
struct RuleDirectiveTables {
    // Explicit element tags mint one loose-list WASM wrapper per element; no transitive expansion.
    used_as_elem: BTreeSet<RustIdent>,
    // Authored extern/raw-bytes Copy declarations drive clone suppression and compile-time assertions.
    // Extern-interface consumers inherit this declaration; it is not a derived Copy fact.
    copy_externs: BTreeSet<RustIdent>,
    // Local marker declarations name the sibling WASM path and exact classes available there.
    // Store intent separately because extern/raw-bytes configs discard rule metadata.
    // Unlisted classes mint locally; this borrowing choice is not projected to consumers.
    extern_companions: BTreeMap<RustIdent, crate::comment_ast::ExternCompanions>,
    // Authored schema-row suppression also supports finalize validation.
    // Store markers independently of extern/raw-bytes default configs; consumers do not inherit them.
    no_json_schema_export: BTreeSet<RustIdent>,
    // Apply authored suppression at alias registration, including manually registered collections/set bindings.
    // Internal alias resolution survives; extern-interface consumers inherit the suppressed declaration.
    no_alias_rules: BTreeSet<RustIdent>,
    // Rule-owned docs survive generic-definition configs and manual set-binding alias registration.
    // Apply at construct creation without replacing its own construct doc; the latest authored value wins.
    rule_docs: BTreeMap<RustIdent, String>,
    // Rule-owned custom JSON intent survives generic-definition and plain-group metadata.
    // Apply before struct ownership comparison; retain separate generic set-binding refusal policy.
    custom_json_rules: BTreeSet<RustIdent>,
    // Nonempty rule-position directive vectors retain producer-sorted static tags.
    // Finalize checks whole-spec splicedness; the storage owner does not validate it.
    plain_group_rule_directives: BTreeMap<RustIdent, Vec<&'static str>>,
    // Authored generic extern opt-in selects a RawBytes flavor only for a resolved raw-bytes argument.
    // Actual flavored emission remains a separate finalize-produced set.
    raw_bytes_flavor: BTreeSet<RustIdent>,
    // Validated extern-scope pins translate names only at import/WASM/component boundaries.
    // Internal RustIdent identity stays derived; rules without pins retain derived spelling.
    rust_name_pins: BTreeMap<RustIdent, String>,
}

pub struct IntermediateTypes<'a> {
    // Storing the cddl::Group is the easiest way to go here even after the parse/codegen split.
    // This is since in order to generate plain groups we must have a representation, which isn't
    // known at group definition. It is later fixed when the plain group is referenced somewhere
    // and we can't parse the group without knowing the representation so instead this parsing is
    // delayed until the point where it is referenced via self.set_rep_if_plain_group(rep)
    // Some(group) = directly defined in .cddl (must call set_rep_if_plain_group() later)
    // None = indirectly generated due to a group choice (no reason to call set_rep_if_plain_group() later but it won't crash)
    plain_groups: BTreeMap<RustIdent, PlainGroupInfo<'a>>,
    /// Lexical generic scopes active while a generic definition body is parsed.  The resulting
    /// `RustType` keeps the selected binding, so this is only resolution context, never the sole
    /// provenance record.
    generic_param_scopes: Vec<Vec<(String, GenericParamBinding)>>,
    /// Parser-only sidecars for anonymous type choices encountered while generic definitions are
    /// parsed.  A choice cannot be registered yet because its arm types still carry lexical
    /// bindings; `register_generic_def` transfers exactly the templates its constructed root
    /// references, discarding classifier-only visits that never reach that root.
    generic_inline_choice_scopes: Vec<GenericInlineChoiceScope>,
    generic_inline_choice_templates: BTreeMap<RustIdent, GenericInlineTypeChoiceTemplate>,
    generic_child_instance_templates: BTreeMap<RustIdent, GenericChildInstanceTemplate>,
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
    // Accumulated authored and derived used-as-key demand, mapped to the UNION of comparison/hash
    // trait demand on each ident (`@used_as_key` flavors + auto-detected internal map-key bundle). Presence in the
    // map == used-as-key; `DemandSet` records WHICH derive family. Propagated as demand SETS (not one
    // bit) through the transitive `visit_types` walk in `finalize`. Determinism: `BTreeMap`.
    key_demand: BTreeMap<RustIdent, DemandSet>,
    // The subset of `key_demand` that was DIRECTLY tagged (via `@used_as_key`/`--key-requests`), before
    // finalize's transitive expansion — the "demand roots". Only these get an emitted compile-time
    // demand assertion (auto-detected internal keys are enforced by the generated containers' own
    // bounds). Recorded at `mark_key_demand` time so the roots survive the finalize union.
    key_demand_roots: BTreeMap<RustIdent, DemandSet>,
    /// The rules `crate::recursion_boundary` asked to be emitted as `@newtype` wrapper structs
    /// rather than transparent `pub type` aliases, because they are the collection-backed members of
    /// an alias-expansion cycle (rustc E0391). Seeded before parsing by `api::with_types`'s second
    /// build pass; empty on every spec with no such cycle, so byte-identical output is untouched.
    /// Determinism: `BTreeSet`, and the set itself is a canonical property of the cycle.
    auto_newtype_rules: BTreeSet<RustIdent>,
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
    // Only `get` and `entry` access this map; it is never iterated, so address-derived `Ord` cannot order observable output.
    variant_mint_claims: BTreeMap<VariantMintContext, Vec<VariantMintClaim>>,
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
    // Authored rule intent; validation and derived facts remain on the store.
    rule_directives: RuleDirectiveTables,
}

impl std::fmt::Debug for IntermediateTypes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IntermediateTypes")
            .field("plain_groups", &self.plain_groups)
            .field("generic_param_scopes", &self.generic_param_scopes)
            .field(
                "generic_inline_choice_scopes",
                &self.generic_inline_choice_scopes,
            )
            .field(
                "generic_inline_choice_templates",
                &self.generic_inline_choice_templates,
            )
            .field(
                "generic_child_instance_templates",
                &self.generic_child_instance_templates,
            )
            .field("type_aliases", &self.type_aliases)
            .field("rust_structs", &self.rust_structs)
            .field("prelude_to_emit", &self.prelude_to_emit)
            .field("generic_defs", &self.generic_defs)
            .field("generic_instances", &self.generic_instances)
            .field(
                "anonymous_collection_instances",
                &self.anonymous_collection_instances,
            )
            .field("generic_extern_bases", &self.generic_extern_bases)
            .field("news_can_fail", &self.news_can_fail)
            .field("key_demand", &self.key_demand)
            .field("key_demand_roots", &self.key_demand_roots)
            .field("used_as_elem", &self.rule_directives.used_as_elem)
            .field("auto_newtype_rules", &self.auto_newtype_rules)
            .field("copy_externs", &self.rule_directives.copy_externs)
            .field("extern_companions", &self.rule_directives.extern_companions)
            .field(
                "no_json_schema_export",
                &self.rule_directives.no_json_schema_export,
            )
            .field("no_alias_rules", &self.rule_directives.no_alias_rules)
            .field("rule_docs", &self.rule_directives.rule_docs)
            .field("custom_json_rules", &self.rule_directives.custom_json_rules)
            .field(
                "plain_group_rule_directives",
                &self.rule_directives.plain_group_rule_directives,
            )
            .field("raw_bytes_flavor", &self.rule_directives.raw_bytes_flavor)
            .field("raw_bytes_flavor_emitted", &self.raw_bytes_flavor_emitted)
            .field("rust_name_pins", &self.rule_directives.rust_name_pins)
            .field("scopes", &self.scopes)
            .field("rule_source_names", &self.rule_source_names)
            .field("group_choice_arm_claims", &self.group_choice_arm_claims)
            .field("nominal_mint_claims", &self.nominal_mint_claims)
            .field("variant_mint_claims", &self.variant_mint_claims)
            .field("rejections", &self.rejections)
            .field("rejection_observations", &self.rejection_observations)
            .field("diagnostic_node_claims", &self.diagnostic_node_claims)
            .finish()
    }
}

impl Default for IntermediateTypes<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// A position in [`IntermediateTypes`]' recorded diagnostics; see
/// [`IntermediateTypes::drop_rewalk_repeats`].
#[derive(Clone, Copy, Debug)]
pub struct RejectionMark(usize);

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

impl ScopeReferences {
    /// Record that module `from` imports `ident` from module `to`.
    fn add_import(&mut self, from: ModuleScope, to: ModuleScope, ident: RustIdent) {
        self.imports
            .entry(from)
            .or_default()
            .entry(to)
            .or_default()
            .insert(ident);
    }
}

#[derive(Clone, Debug)]
struct NominalMintClaim {
    identity: StructuralFingerprint,
    site: MintSite,
}

/// Who claimed a nominal mint. Display preserves diagnostic and floor provenance.
#[derive(Clone, Debug, PartialEq, Eq)]
enum MintSite {
    /// The ordinary `register_rust_struct` seam, retracted by `remove_rust_struct`.
    Registration(RustIdent),
    /// A pre-registration minter via `claim_nominal_mint`, such as a fixed singleton.
    Semantic(String),
}

impl std::fmt::Display for MintSite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Registration(ident) => write!(f, "RustStruct registration for `{ident}`"),
            Self::Semantic(site) => f.write_str(site),
        }
    }
}

#[derive(Debug)]
struct GenericInlineChoiceScope {
    owner: RustIdent,
    next_ordinal: usize,
    next_child_ordinal: usize,
    /// A generic body can visit the same AST choice while it classifies then constructs a record.
    /// Keep that choice's placeholder stable across the visits, just as the variant-mint ledger
    /// keeps its derived variant names stable.
    placeholders_by_choice: BTreeMap<usize, RustIdent>,
    child_placeholders_by_application: BTreeMap<usize, RustIdent>,
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

/// The namespace where one enum's variant names are reserved and settled.
#[allow(clippy::enum_variant_names)] // Each variant names its source choice kind.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum VariantMintContext {
    TypeChoice(RustIdent),
    GroupChoice(RustIdent),
    /// The AST address qualifies the key so independent inline choices never share reservations.
    /// It is process-local and must never reach a rejection, keeping diagnostics deterministic for identical CDDL.
    InlineTypeChoice(usize),
}

impl std::fmt::Display for VariantMintContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeChoice(rule) => write!(f, "type choice for rule {rule}"),
            Self::GroupChoice(rule) => write!(f, "group choice for rule {rule}"),
            Self::InlineTypeChoice(_) => f.write_str("an inline type choice"),
        }
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
            generic_param_scopes: Vec::new(),
            generic_inline_choice_scopes: Vec::new(),
            generic_inline_choice_templates: BTreeMap::new(),
            generic_child_instance_templates: BTreeMap::new(),
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
            rule_directives: RuleDirectiveTables::default(),
            auto_newtype_rules: BTreeSet::new(),
            raw_bytes_flavor_emitted: BTreeSet::new(),
            scopes: BTreeMap::new(),
            rule_source_names: BTreeMap::new(),
            group_choice_arm_claims: BTreeMap::new(),
            nominal_mint_claims: BTreeMap::new(),
            variant_mint_claims: BTreeMap::new(),
            rejections: Vec::new(),
            rejection_observations: 0,
            diagnostic_node_claims: BTreeSet::new(),
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

    /// A position in the recorded diagnostics, for [`Self::drop_rewalk_repeats`].
    pub fn rejection_mark(&self) -> RejectionMark {
        RejectionMark(self.rejections.len())
    }

    /// For a caller that walks the same AST nodes twice (a classification walk from `walk` to
    /// `rewalk`, then a construction walk from `rewalk` to now): drop each diagnostic of the second
    /// walk whose text matches one the first walk recorded, one removal per earlier match, so a
    /// node reports once however many times it is visited. A diagnostic only one walk records is
    /// kept, and so is a repeat of equal text beyond the first walk's count (distinct nodes).
    pub fn drop_rewalk_repeats(&mut self, walk: RejectionMark, rewalk: RejectionMark) {
        let mut first_walk: BTreeMap<String, usize> = BTreeMap::new();
        for msg in &self.rejections[walk.0..rewalk.0] {
            *first_walk.entry(msg.clone()).or_insert(0) += 1;
        }
        let second_walk = self.rejections.split_off(rewalk.0);
        for msg in second_walk {
            match first_walk.get_mut(&msg) {
                Some(remaining) if *remaining > 0 => *remaining -= 1,
                _ => self.rejections.push(msg),
            }
        }
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

    pub fn rust_structs(&self) -> &BTreeMap<RustIdent, RustStruct> {
        &self.rust_structs
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn generic_def(&self, ident: &RustIdent) -> Option<&GenericDef> {
        self.generic_defs.get(ident)
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

    fn aliases() -> BTreeMap<idents::AliasIdent, AliasInfo> {
        // TODO: write the rest of the reserved keywords here from the CDDL RFC
        let mut aliases = BTreeMap::<AliasIdent, AliasInfo>::new();
        let mut insert_alias = |name: &str, rust_type: RustType| {
            let ident = AliasIdent::new(CDDLIdent::new(name));
            aliases.insert(ident, AliasInfo::transparent(rust_type));
        };
        insert_alias("uint", ConceptualRustType::Primitive(Primitive::U64).into());
        insert_alias("nint", ConceptualRustType::Primitive(Primitive::N64).into());
        insert_alias(
            "bool",
            ConceptualRustType::Primitive(Primitive::Bool).into(),
        );
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
    pub const REFUSED_PRELUDE_NAMES: &'static [&'static str] = &["cbor-any"];

    // note: this is mut so the unregistered-reserved fallback can mark which reserved idents
    // are in the CDDL prelude so we don't generate code for all of them, potentially
    // bloating generated code a bit
    pub fn new_type(&mut self, raw: &CDDLIdent, cli: &Cli) -> RustType {
        // Parameters shadow every registered claimant, but only when the BODY token has the exact
        // source spelling declared by the parameter.  `RustIdent` normalization deliberately
        // happens after this lookup: an outer `A` beside parameter `a` must remain the outer rule.
        if let Some(binding) = self
            .generic_param_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter())
            .find(|(source, _)| source == &raw.to_string())
            .map(|(_, binding)| *binding)
        {
            return RustType::new(ConceptualRustType::Rust(RustIdent::new(raw.clone())))
                .with_generic_param_binding(binding);
        }
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
                        panic!("Reserved ident {reserved} not a part of cddl_prelude?")
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

    /// Parse one generic body with an exact-source lexical parameter scope.  The scope is stack
    /// shaped because parsing a prelude expansion can re-enter normal type parsing; on return the
    /// surrounding definition's bindings are again the only active ones.
    pub fn with_generic_param_scope<R>(
        &mut self,
        bindings: Vec<(String, GenericParamBinding)>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.generic_param_scopes.push(bindings);
        let result = f(self);
        self.generic_param_scopes
            .pop()
            .expect("generic parameter scope must balance its parser entry");
        result
    }

    /// Own parser-only anonymous type-choice templates while a single generic definition body is
    /// built.  The stack matches the lexical generic scope: prelude expansion can re-enter parsing,
    /// and an inner definition must never donate a template to its caller.
    pub fn with_generic_inline_choice_scope<R>(
        &mut self,
        owner: RustIdent,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.generic_inline_choice_scopes
            .push(GenericInlineChoiceScope {
                owner,
                next_ordinal: 0,
                next_child_ordinal: 0,
                placeholders_by_choice: BTreeMap::new(),
                child_placeholders_by_application: BTreeMap::new(),
            });
        let result = f(self);
        let scope = self
            .generic_inline_choice_scopes
            .pop()
            .expect("generic inline choice scope must balance its parser entry");
        // `register_generic_def` already transferred every placeholder reachable from the finished
        // definition. Any remaining entry came from a classifier-only AST visit, so discard it
        // with this lexical scope rather than leaving parser state to influence a later definition.
        for placeholder in scope.placeholders_by_choice.into_values() {
            self.generic_inline_choice_templates.remove(&placeholder);
        }
        for placeholder in scope.child_placeholders_by_application.into_values() {
            self.generic_child_instance_templates.remove(&placeholder);
        }
        result
    }

    /// Stage an inline type-choice template under the active generic definition and return its
    /// private placeholder ident.  The placeholder never enters a generated file: generic
    /// finalization replaces it with the concrete anonymous-union owner before registration.
    pub fn register_generic_inline_type_choice_template(
        &mut self,
        choice_context: usize,
        template: RustStruct,
    ) -> RustIdent {
        let (owner, ordinal) = {
            let scope = self
                .generic_inline_choice_scopes
                .last_mut()
                .expect("generic inline type choice requires an active generic-definition scope");
            if let Some(placeholder) = scope.placeholders_by_choice.get(&choice_context) {
                return placeholder.clone();
            }
            scope.next_ordinal += 1;
            (scope.owner.clone(), scope.next_ordinal)
        };
        // This is a parser-private ident, but it participates in ordinary `RustType::Rust` graph
        // walks until generic finalization. Allocate it through the same collision-free path as
        // other synthesized idents so an authored rule (all pre-scanned in `scopes`) cannot be
        // rewritten as a deferred choice. `fresh_synthesized_ident` also sees existing staged
        // templates and active scopes, preserving stable reuse for a second visit to this AST node.
        let placeholder =
            self.fresh_synthesized_ident(&format!("{owner}GenericInlineChoice{ordinal}"));
        let mut template = template;
        template.ident = placeholder.clone();
        self.generic_inline_choice_templates.insert(
            placeholder.clone(),
            GenericInlineTypeChoiceTemplate::new(placeholder.clone(), template),
        );
        self.generic_inline_choice_scopes
            .last_mut()
            .expect("generic inline type choice scope must remain active while staging template")
            .placeholders_by_choice
            .insert(choice_context, placeholder.clone());
        placeholder
    }

    /// Whether a generic-application argument must stay owned by the active generic definition.
    /// The exact binding marker, not an emitted Rust identifier, is the authority; a nested child
    /// placeholder has the same deferred ownership requirement.
    pub fn generic_child_instance_is_deferred(&self, generic_args: &[RustType]) -> bool {
        fn deferred(
            ty: &RustType,
            placeholders: &BTreeMap<RustIdent, GenericChildInstanceTemplate>,
        ) -> bool {
            if ty.generic_param_binding.is_some() {
                return true;
            }
            match &ty.conceptual_type {
                ConceptualRustType::Rust(ident) => placeholders.contains_key(ident),
                ConceptualRustType::Array(element) | ConceptualRustType::Optional(element) => {
                    deferred(element, placeholders)
                }
                ConceptualRustType::Map(domain, range) => {
                    deferred(domain, placeholders) || deferred(range, placeholders)
                }
                ConceptualRustType::Fixed(_)
                | ConceptualRustType::Primitive(_)
                | ConceptualRustType::Alias(_, _)
                | ConceptualRustType::Any => false,
            }
        }
        generic_args
            .iter()
            .any(|arg| deferred(arg, &self.generic_child_instance_templates))
    }

    /// An inline type-choice used as a generic argument is a separate template whose concrete
    /// anonymous-owner identity is not available while the child application is staged. This is
    /// the deliberately narrow remaining cross-template boundary; callers reject it rather than
    /// leaking an `OuterGenericInlineChoice…` pseudo-instance into the global registry.
    pub fn generic_child_instance_has_inline_choice_argument(
        &self,
        generic_args: &[RustType],
    ) -> bool {
        fn contains(
            ty: &RustType,
            placeholders: &BTreeMap<RustIdent, GenericInlineTypeChoiceTemplate>,
        ) -> bool {
            match &ty.conceptual_type {
                ConceptualRustType::Rust(ident) => placeholders.contains_key(ident),
                ConceptualRustType::Array(element) | ConceptualRustType::Optional(element) => {
                    contains(element, placeholders)
                }
                ConceptualRustType::Map(domain, range) => {
                    contains(domain, placeholders) || contains(range, placeholders)
                }
                ConceptualRustType::Fixed(_)
                | ConceptualRustType::Primitive(_)
                | ConceptualRustType::Alias(_, _)
                | ConceptualRustType::Any => false,
            }
        }
        generic_args
            .iter()
            .any(|arg| contains(arg, &self.generic_inline_choice_templates))
    }

    /// Stage a generic application below the active definition and return its private placeholder.
    /// The AST address only deduplicates repeat parser visits of one occurrence; it never reaches a
    /// generated name or diagnostic.
    pub fn register_generic_child_instance_template(
        &mut self,
        application_context: usize,
        generic_ident: RustIdent,
        generic_args: Vec<RustType>,
    ) -> RustIdent {
        let (owner, ordinal) = {
            let scope = self
                .generic_inline_choice_scopes
                .last_mut()
                .expect("deferred generic child requires an active generic-definition scope");
            if let Some(placeholder) = scope
                .child_placeholders_by_application
                .get(&application_context)
            {
                return placeholder.clone();
            }
            scope.next_child_ordinal += 1;
            (scope.owner.clone(), scope.next_child_ordinal)
        };
        let placeholder =
            self.fresh_synthesized_ident(&format!("{owner}GenericChildInstance{ordinal}"));
        self.generic_child_instance_templates.insert(
            placeholder.clone(),
            GenericChildInstanceTemplate::new(placeholder.clone(), generic_ident, generic_args),
        );
        self.generic_inline_choice_scopes
            .last_mut()
            .expect("generic child scope must remain active while staging template")
            .child_placeholders_by_application
            .insert(application_context, placeholder.clone());
        placeholder
    }

    /// The innermost lexical binding for this exact source token, if generic parsing is active.
    /// This is intentionally not a `RustIdent` query: case/separator normalization is an emitted
    /// spelling concern and must not alter CDDL lexical resolution.
    pub fn active_generic_param_binding(&self, raw: &str) -> Option<GenericParamBinding> {
        self.generic_param_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter())
            .find(|(source, _)| source == raw)
            .map(|(_, binding)| *binding)
    }

    pub fn register_type_alias(&mut self, alias: RustIdent, mut info: AliasInfo) {
        // `@no_alias` is enforced HERE rather than at each constructor, so a registration path that
        // builds its `AliasInfo` without the rule's metadata (`new_manual`: the table/array
        // kind-walk, a named binding to a generic set nominal) honors the directive too. Idempotent
        // for `new_from_metadata`, which already derived both flags from the same bit.
        if self.rule_directives.no_alias_rules.contains(&alias) {
            info.suppress_declared_aliases();
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
        let identity = rust_struct.structural_fingerprint();
        self.claim_nominal_mint_inner(
            rust_struct.ident(),
            &identity,
            MintSite::Semantic(site.into()),
            true,
        );
    }

    fn claim_nominal_mint_inner(
        &mut self,
        ident: &RustIdent,
        identity: &StructuralFingerprint,
        site: MintSite,
        report_registration_duplicate: bool,
    ) {
        if ident.is_type_expression() {
            return;
        }
        if let Some(first) = self.nominal_mint_claims.get(ident) {
            if first.identity != *identity {
                // Ordinary registrations retain the legacy global guard's one diagnostic. The
                // mint ledger speaks only when a semantic pre-registration claimant is involved.
                if report_registration_duplicate || !matches!(first.site, MintSite::Registration(_))
                {
                    self.record_rejection(format!(
                        "generated Rust type `{ident}` has incompatible mint claims: `{}` first claimed; \
                         `{}` later claimed a different structural/wire identity. Keep one wire shape \
                         per generated Rust name.",
                        first.site, site,
                    ));
                }
            }
            return;
        }
        self.nominal_mint_claims.insert(
            ident.clone(),
            NominalMintClaim {
                identity: identity.clone(),
                site,
            },
        );
    }

    /// Reserve an explicit variant spelling before any derived sibling receives a suffix. Returns
    /// the first explicit claimant in this enum context, if any, so the parser can retain its
    /// established kind-specific diagnostic wording.
    pub(crate) fn reserve_explicit_variant_mint(
        &mut self,
        context: &VariantMintContext,
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
            self.record_rejection(format!(
                "variant mint claim drift in {context}: arm {arm_ordinal} first claimed source `{}` as `{}` ({}) but later claimed source `{source_name}` as `{emitted_name}` (explicit @name). One enum arm must retain one stable mint claim.",
                first.source_name,
                first.emitted_name,
                if first.explicit { "explicit @name" } else { "derived" },
            ));
            return None;
        }
        let claims = self.variant_mint_claims.entry(context.clone()).or_default();
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
        context: &VariantMintContext,
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
            self.record_rejection(format!(
                "variant mint claim drift in {context}: arm {arm_ordinal} first claimed source `{}` as `{}` ({}) but later claimed source `{source_name}` from derived base `{base}`. One enum arm must retain one stable mint claim.",
                first.source_name,
                first.emitted_name,
                if first.explicit { "explicit @name" } else { "derived" },
            ));
            return first.emitted_name;
        }
        let claims = self.variant_mint_claims.entry(context.clone()).or_default();
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
        if let Some(doc) = self
            .rule_directives
            .rule_docs
            .get(&rust_struct.ident)
            .cloned()
        {
            rust_struct.set_doc_if_absent(&doc);
        }
        // `@custom_json` reaches its config the same two ways it can miss it — a generic INSTANCE's
        // config is the generic DEFINITION's, and a plain GROUP rule's is built from metadata read
        // off a slot cddl leaves empty — so the per-ident record is applied at this same seam. Only
        // ever sets the flag: a config that already carries it got it from the rule that owns the
        // struct, and the record is that rule's own statement, so the two can only agree.
        if self
            .rule_directives
            .custom_json_rules
            .contains(&rust_struct.ident)
        {
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
        let fingerprint = rust_struct.structural_fingerprint();
        self.claim_nominal_mint_inner(
            rust_struct.ident(),
            &fingerprint,
            MintSite::Registration(rust_struct.ident().clone()),
            false,
        );
        // Every generated nominal shares this namespace. Decide ownership only after the incoming
        // claim's local configuration is complete, but before it can create an alias, synthesize a
        // table keys-list, or mutate any other IR map: last-registration-wins would silently
        // retarget every existing reference to a different wire shape. Equivalent structures are
        // deliberate shared ownership (not replacement).
        if let Some(existing) = self.rust_structs.get(rust_struct.ident()) {
            if existing.structural_fingerprint() == fingerprint {
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
                    map_type = map_type.with_occurrence_bounds(*bounds);
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
                self.register_type_alias(rust_struct.ident.clone(), AliasInfo::rust_only(map_type))
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
                    array_type = array_type.with_occurrence_bounds(*bounds);
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
                    AliasInfo::rust_only(array_type),
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
            RustStructType::Wrapper { wrapped, .. }
                if wrapped.exact_byte_array_len_checked().is_some()
                    || matches!(wrapped.conceptual_type.resolve_alias_shallow(), ConceptualRustType::Optional(inner) if inner.exact_byte_array_len_checked().is_some()) =>
            {
                self.mark_new_can_fail(rust_struct.ident.clone());
            }
            _ => (),
        }
        self.rust_structs
            .insert(rust_struct.ident().clone(), rust_struct);
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
        let array_type_ident = RustIdent::from_formatted(array_type_name);
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

    pub fn register_generic_def(&mut self, mut def: GenericDef) {
        let ident = def.orig.ident().clone();
        let inline_template_idents = self
            .generic_inline_choice_templates
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>();
        let child_template_idents = self
            .generic_child_instance_templates
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>();
        let mut inline_placeholders = Vec::new();
        let mut child_placeholders = Vec::new();
        Self::collect_generic_placeholders(
            &def.orig,
            &inline_template_idents,
            &mut inline_placeholders,
        );
        Self::collect_generic_placeholders(
            &def.orig,
            &child_template_idents,
            &mut child_placeholders,
        );
        // Inline choices and child applications can depend on one another. Transfer exactly their
        // reachable closure into this definition; classifier-only visits remain parser-local.
        let mut seen_inline = BTreeSet::new();
        let mut seen_child = BTreeSet::new();
        let mut inline_templates = Vec::new();
        let mut child_templates = Vec::new();
        let mut next_inline = 0;
        let mut next_child = 0;
        while next_inline < inline_placeholders.len() || next_child < child_placeholders.len() {
            if let Some(placeholder) = inline_placeholders.get(next_inline).cloned() {
                next_inline += 1;
                if !seen_inline.insert(placeholder.clone()) {
                    continue;
                }
                if let Some(template) = self.generic_inline_choice_templates.remove(&placeholder) {
                    Self::collect_generic_placeholders(
                        &template.template,
                        &inline_template_idents,
                        &mut inline_placeholders,
                    );
                    Self::collect_generic_placeholders(
                        &template.template,
                        &child_template_idents,
                        &mut child_placeholders,
                    );
                    inline_templates.push(template);
                }
                continue;
            }
            let placeholder = child_placeholders[next_child].clone();
            next_child += 1;
            if !seen_child.insert(placeholder.clone()) {
                continue;
            }
            if let Some(template) = self.generic_child_instance_templates.remove(&placeholder) {
                for arg in &template.generic_args {
                    Self::collect_generic_placeholders_in_type(
                        arg,
                        &child_template_idents,
                        &mut child_placeholders,
                    );
                }
                child_templates.push(template);
            }
        }
        if !inline_templates.is_empty() {
            def.set_inline_type_choices(inline_templates);
        }
        if !child_templates.is_empty() {
            def.set_child_instances(child_templates);
        }
        self.generic_defs.insert(ident, def);
    }

    /// Choose the ordinary anonymous-type-choice owner for a concrete candidate.  This is shared
    /// by parser-time anonymous choices and generic-finalization templates so compatible choices
    /// reuse their first owner while incompatible anonymous siblings mint deterministically; an
    /// authored rule still keeps the established loud global-registration collision.
    pub fn anonymous_type_choice_ident(
        &self,
        base_ident: &RustIdent,
        candidate: &RustStruct,
    ) -> RustIdent {
        let sibling_base = format!("{}_inline_choice", base_ident);
        let sibling_prefix = RustIdent::new(CDDLIdent::new(&sibling_base)).to_string();
        if self.is_toplevel_rule(base_ident) {
            return base_ident.clone();
        }
        self.rust_structs()
            .iter()
            .find(|(ident, existing)| {
                !self.is_toplevel_rule(ident)
                    && (ident.as_ref() == base_ident.as_ref()
                        || ident.as_ref().starts_with(&sibling_prefix))
                    && existing.structurally_equivalent(candidate)
            })
            .map(|(ident, _)| ident.clone())
            .unwrap_or_else(|| {
                if self.rust_struct(base_ident).is_some() {
                    self.fresh_synthesized_ident(&sibling_base)
                } else {
                    base_ident.clone()
                }
            })
    }

    /// Register an ordinary concrete generic instance, preserving the first compatible claimant.
    /// A definition-owned child can reach this seam after an independently authored instance with
    /// the same synthesized ident has completed; replacing that completed entry would make already
    /// registered parents silently point at the later wire shape. Reject the incompatible claim
    /// through the established generated-name collision contract instead.
    pub fn register_generic_instance(&mut self, instance: GenericInstance) -> bool {
        let ident = instance.instance_ident.clone();
        if let Some(existing) = self.generic_instances.get(&ident) {
            if existing.registration_compatible_with(&instance) {
                return false;
            }
            self.record_rejection(format!(
                "generated Rust type `{ident}` has incompatible registrations: the first claimant \
                 is a generic instance of `{}`, but the later claimant is a generic instance of \
                 `{}`. Keep one wire shape per generated Rust name; rename one authored rule or \
                 the synthesized claimant that collides with it.",
                existing.generic_ident, instance.generic_ident,
            ));
            return false;
        }
        self.generic_instances.insert(ident, instance);
        true
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
                && (alias_info.declared_rust_alias() || alias_info.declared_wasm_alias())
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

    /// B5-404's nominal scalar carriers: unlike an occurrence-bounded member (whose containing
    /// record owns the check), a named integer/bytes/text window owns its public invariant.  Keep
    /// this IR fact next to `can_new_fail`: the rust, wasm, component, and emitted-test faces must
    /// agree that construction crosses the wrapper's `TryFrom` impl, rather than each growing a
    /// local notion of a "bounded newtype".
    pub fn requires_checked_try_from(&self, name: &RustIdent) -> bool {
        let Some(RustStructType::Wrapper {
            wrapped, min_max, ..
        }) = self.rust_struct(name).map(RustStruct::variant)
        else {
            return false;
        };
        if min_max.is_none() && wrapped.exact_byte_array_len_checked().is_none() {
            return false;
        }
        matches!(
            wrapped.conceptual_type.resolve_alias_shallow(),
            ConceptualRustType::Primitive(
                Primitive::Bytes
                    | Primitive::Str
                    | Primitive::U8
                    | Primitive::U16
                    | Primitive::U32
                    | Primitive::U64
                    | Primitive::I8
                    | Primitive::I16
                    | Primitive::I32
                    | Primitive::I64
                    | Primitive::N64
            )
        )
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
        self.scopes.get(ident).unwrap_or(&*ROOT_SCOPE)
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
        self.rule_directives.rust_name_pins.insert(derived, pinned);
    }

    /// The full pin map (`derived RustIdent` -> `pinned dep name`), for the crate-boundary
    /// translation sites (`add_imports_from_scope_refs`).
    pub fn rust_name_pins(&self) -> &BTreeMap<RustIdent, String> {
        &self.rule_directives.rust_name_pins
    }

    /// The pinned dependency name for `derived`, if it carries a `@rust_name` pin. `None` = derive
    /// the name today's way (hand-stub compatibility).
    pub fn rust_name_pin(&self, derived: &RustIdent) -> Option<&str> {
        self.rule_directives
            .rust_name_pins
            .get(derived)
            .map(|s| s.as_str())
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
    /// build — an embedded record, inline group, etc.). A synthesized type may inherit its owner's
    /// module scope so it emits beside that owner, so scope membership alone is deliberately not
    /// evidence of source-rule ownership. `rule_source_names` is populated only for real rules by
    /// `api::with_types`. Used by the `--emit-tests-conformance` oracle: only a real rule name can
    /// be aliased as the validator's synthetic root, so synthesized structs get no conformance call.
    pub fn is_toplevel_rule(&self, ident: &RustIdent) -> bool {
        self.rule_source_names.contains_key(ident)
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

    /// Whether `ident` is already spoken for by any type-like parser product, including a
    /// synthesized product that has no authored rule scope. Callers that mint a stable public type
    /// name must reject rather than borrow or suffix a claimant: a suffix would make the generated
    /// API depend on source ordering.
    pub fn generated_type_ident_is_claimed(&self, ident: &RustIdent) -> bool {
        self.rust_structs.contains_key(ident)
            || self.plain_groups.contains_key(ident)
            || self.scopes.contains_key(ident)
            || self.generic_defs.contains_key(ident)
            || self.generic_instances.contains_key(ident)
            || self.group_choice_arm_claims.contains_key(ident)
            || self.generic_inline_choice_templates.contains_key(ident)
            || self.generic_inline_choice_scopes.iter().any(|scope| {
                scope
                    .placeholders_by_choice
                    .values()
                    .any(|placeholder| placeholder == ident)
            })
            || self
                .type_aliases
                .contains_key(&AliasIdent::Rust(ident.clone()))
            || self.nominal_mint_claims.contains_key(ident)
            || self.prelude_cddl_name(ident).is_some()
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
        let mut candidate = RustIdent::new(CDDLIdent::new(base));
        let mut suffix = 0u32;
        while self.generated_type_ident_is_claimed(&candidate) {
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
            && self.nominal_mint_claims.get(ident).is_some_and(
                |claim| matches!(&claim.site, MintSite::Registration(owner) if owner == ident),
            )
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
        &self.rule_directives.used_as_elem
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
        self.rule_directives.used_as_elem.insert(name);
    }

    /// The set of base generic extern idents tagged `@raw_bytes_flavor` (see `mark_raw_bytes_flavor`).
    /// `GenericInstance::resolve` consults this to decide whether an instance carrying a raw-bytes
    /// argument aliases the `<Base>RawBytes` flavor instead of the plain base name.
    pub fn raw_bytes_flavor(&self) -> &BTreeSet<RustIdent> {
        &self.rule_directives.raw_bytes_flavor
    }

    pub fn mark_raw_bytes_flavor(&mut self, name: RustIdent) {
        self.rule_directives.raw_bytes_flavor.insert(name);
    }

    /// Whether `ident` names an extern / raw-bytes rule declared `@copy`. `is_copy` ORs this into its
    /// `Rust(ident)` arm so the generator stops cloning a value whose rust type derives `Copy`.
    pub fn is_copy_extern(&self, ident: &RustIdent) -> bool {
        self.rule_directives.copy_externs.contains(ident)
    }

    pub fn mark_copy_extern(&mut self, name: RustIdent) {
        self.rule_directives.copy_externs.insert(name);
    }

    pub fn mark_extern_companions(
        &mut self,
        name: RustIdent,
        companions: crate::comment_ast::ExternCompanions,
    ) {
        self.rule_directives
            .extern_companions
            .insert(name, companions);
    }

    /// The whole `@extern_companions` registry, keyed by declaring marker rule. Empty unless some
    /// rule carries the directive, which is what keeps the deferral arm that reads it inert (and the
    /// output byte-identical) for every spec that does not.
    pub fn extern_companions(&self) -> &BTreeMap<RustIdent, crate::comment_ast::ExternCompanions> {
        &self.rule_directives.extern_companions
    }

    /// The `use`-path prefix under which `class` is declared to ALREADY exist, given that every named
    /// constituent of the wrapper resolves to `owner`. `None` when `owner` carries no declaration or
    /// its declaration does not list `class` — an unlisted structural companion mints locally, which
    /// is the whole point of the class list being a filter rather than a blanket opt-out.
    pub fn extern_companion_path(&self, owner: &RustIdent, class: &str) -> Option<&str> {
        self.rule_directives
            .extern_companions
            .get(owner)
            .filter(|c| c.classes.contains(class))
            .map(|c| c.path_prefix.as_str())
    }

    /// Whether the rule `ident` was declared `@no_json_schema_export` — the spec author's statement
    /// that this type is not part of the published JSON-schema surface. The json-gen row loop skips
    /// it, and finalize rejects the directive on rules without a registered struct.
    pub fn is_no_json_schema_export(&self, ident: &RustIdent) -> bool {
        self.rule_directives.no_json_schema_export.contains(ident)
    }

    pub fn mark_no_json_schema_export(&mut self, name: RustIdent) {
        self.rule_directives.no_json_schema_export.insert(name);
    }

    /// Record that `name`'s rule carries `@no_alias`. Called from the parse seam that reads a rule's
    /// metadata, unconditionally — whether the rule ends up registering an alias at all is decided
    /// later, and by several different paths (see the `no_alias_rules` field comment).
    pub fn mark_no_alias_rule(&mut self, name: RustIdent) {
        self.rule_directives.no_alias_rules.insert(name);
    }

    /// Whether `name`'s rule asked for its transparent `pub type` to be suppressed. Read by
    /// `register_type_alias` (which enforces it) and by the extern-interface projection (which must
    /// tell a consumer, since the suppressed name is one the dep no longer materializes).
    pub fn is_no_alias_rule(&self, name: &RustIdent) -> bool {
        self.rule_directives.no_alias_rules.contains(name)
    }

    /// Record `name`'s rule-level `@doc` text. Called from the same parse seam as
    /// `mark_no_alias_rule`, unconditionally — which construct (if any) ends up carrying it is
    /// decided later (see the `rule_docs` field comment).
    pub fn mark_rule_doc(&mut self, name: RustIdent, doc: String) {
        self.rule_directives.rule_docs.insert(name, doc);
    }

    /// The rule-level `@doc` written on `name`'s rule, for the construct builders whose own config
    /// cannot carry it.
    pub fn rule_doc(&self, name: &RustIdent) -> Option<&str> {
        self.rule_directives.rule_docs.get(name).map(String::as_str)
    }

    /// Record that `name`'s rule carries `@custom_json`. Called from the same parse seams as
    /// `mark_no_alias_rule`/`mark_rule_doc`, unconditionally — which construct (if any) ends up
    /// carrying it is decided later (see the `custom_json_rules` field comment).
    pub fn mark_custom_json_rule(&mut self, name: RustIdent) {
        self.rule_directives.custom_json_rules.insert(name);
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
            self.rule_directives
                .plain_group_rule_directives
                .insert(name, directives);
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
    fn structural_identity_ignores_only_debug_omitted_provenance() {
        let ident = RustIdent::new(CDDLIdent::new("choice"));
        let u64_ty = RustType::new(ConceptualRustType::Primitive(Primitive::U64));
        let make = |ty: RustType, derived: bool, tag: Option<usize>| {
            let variant = EnumVariant::new(VariantIdent::new_custom("U64"), ty, false, None);
            RustStruct::new_type_choice(
                ident.clone(),
                tag,
                None,
                vec![if derived {
                    variant.with_derived_name()
                } else {
                    variant
                }],
                &cli(),
            )
        };
        let base = make(u64_ty.clone(), false, None);
        assert!(
            base.structurally_equivalent(&make(
                u64_ty
                    .clone()
                    .with_generic_param_binding(GenericParamBinding::new(0)),
                false,
                None,
            ))
        );
        assert!(base.structurally_equivalent(&make(u64_ty, true, None)));
        assert!(!base.structurally_equivalent(&make(
            RustType::new(ConceptualRustType::Primitive(Primitive::Str)),
            false,
            None,
        )));
        assert!(!base.structurally_equivalent(&make(
            RustType::new(ConceptualRustType::Primitive(Primitive::U64)),
            false,
            Some(7),
        )));
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
    fn nominal_mint_sites_render_registration_and_semantic_provenance() {
        let cddl = cddl::parser::cddl_from_str("anchor = uint\n", true).unwrap();
        let parent_visitor = ParentVisitor::new(&cddl).unwrap();
        let ident = RustIdent::new(CDDLIdent::new("contested"));
        let mut types = IntermediateTypes::new();
        types.register_rust_struct(
            &parent_visitor,
            RustStruct::new_extern(ident.clone()),
            &cli(),
        );
        types.claim_nominal_mint(&RustStruct::new_raw_bytes(ident.clone()), "semantic probe");
        let err = types
            .finalize(&parent_visitor, &cli())
            .expect_err("different semantic and registration claims must reject")
            .to_string();
        assert!(
            err.contains("generated Rust type `Contested` has incompatible mint claims: `RustStruct registration for `Contested`` first claimed; `semantic probe` later claimed"),
            "{err}"
        );

        let mut types = IntermediateTypes::new();
        types.register_rust_struct(
            &parent_visitor,
            RustStruct::new_extern(ident.clone()),
            &cli(),
        );
        types.register_rust_struct(
            &parent_visitor,
            RustStruct::new_raw_bytes(ident.clone()),
            &cli(),
        );
        let err = types
            .finalize(&parent_visitor, &cli())
            .expect_err("duplicate registrations must use the legacy guard")
            .to_string();
        assert!(err.contains("has incompatible registrations"), "{err}");
        assert!(!err.contains("incompatible mint claims"), "{err}");

        let mut types = IntermediateTypes::new();
        types.register_rust_struct(
            &parent_visitor,
            RustStruct::new_extern(ident.clone()),
            &cli(),
        );
        types.remove_rust_struct(&ident);
        assert!(!types.nominal_mint_claims.contains_key(&ident));
        types.claim_nominal_mint(&RustStruct::new_extern(ident.clone()), "semantic probe");
        types.register_rust_struct(
            &parent_visitor,
            RustStruct::new_extern(ident.clone()),
            &cli(),
        );
        types.remove_rust_struct(&ident);
        assert!(types.nominal_mint_claims.contains_key(&ident));
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
            &VariantMintContext::TypeChoice(RustIdent::new(CDDLIdent::new("choices"))),
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
        let context = VariantMintContext::TypeChoice(RustIdent::new(CDDLIdent::new("choice")));
        assert!(
            types
                .reserve_explicit_variant_mint(
                    &context,
                    2,
                    "chosen".to_owned(),
                    "Chosen".to_owned(),
                )
                .is_none()
        );
        assert!(
            types
                .reserve_explicit_variant_mint(
                    &context,
                    2,
                    "chosen".to_owned(),
                    "Chosen".to_owned(),
                )
                .is_none(),
            "a revisited explicit arm is not a second claimant"
        );
        assert_eq!(
            types.settle_derived_variant_mint(&context, 1, "tstr".to_owned(), "Text".to_owned()),
            "Text"
        );
        assert_eq!(
            types.settle_derived_variant_mint(&context, 1, "tstr".to_owned(), "Text".to_owned()),
            "Text",
            "a revisited derived arm must retain its original spelling rather than suffix"
        );
        let claims = types.variant_mint_claims.get(&context).unwrap();
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
        let context = VariantMintContext::TypeChoice(RustIdent::new(CDDLIdent::new("choice")));
        assert_eq!(
            types.settle_derived_variant_mint(&context, 1, "uint".to_owned(), "Uint".to_owned()),
            "Uint"
        );
        // Same ordinal is one semantic enum arm. A different source/base is not an AST revisit:
        // retaining the first claim and rejecting the second makes the drift deterministic.
        assert_eq!(
            types.settle_derived_variant_mint(&context, 1, "tstr".to_owned(), "Text".to_owned()),
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
        let context = VariantMintContext::InlineTypeChoice(0xDEAD_BEEF);
        assert!(
            types
                .reserve_explicit_variant_mint(&context, 1, "first".to_owned(), "First".to_owned(),)
                .is_none()
        );
        assert!(
            types
                .reserve_explicit_variant_mint(
                    &context,
                    1,
                    "second".to_owned(),
                    "Second".to_owned(),
                )
                .is_none()
        );
        let error = types
            .finalize(&parent_visitor, &cli())
            .expect_err("an explicit re-entry with changed source/name must reject")
            .to_string();
        assert!(
            error.contains("variant mint claim drift in an inline type choice")
                && error.contains("source `first` as `First`")
                && error.contains("source `second` as `Second`")
                && !error.contains("0xDEADBEEF")
                && !error.contains("deadbeef")
                && !error.contains("3735928559"),
            "the explicit drift must retain both claims but hide process-local key state: {error}"
        );
    }
}
