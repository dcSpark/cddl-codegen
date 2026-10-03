//! Reachable JSON-schema claims for finalized own-crate roots.

use super::*;

/// Emit side-effect-free registrar claims for the names schemars can reach while registering the
/// spec's ordinary roots. This intentionally models emitted JSON-schema calls rather than using the
/// general conceptual visitor: hand-written (`@custom_json` / extern) bodies are opaque, map keys
/// enter a schema BODY while values enter through `subschema_for`, wrappers delegate to their inner
/// body's `json_schema`, and natural `any` never reaches `AnyCbor`'s tagged schema.
///
/// The walker is conservative about ownership: it names only crate-exported `RustStruct`s, because
/// json-gen depends only on the current rust crate. A type with no generated struct is followed only
/// through its transparent alias, never guessed from a user-supplied path.
pub(super) fn json_schema_reachable_claims(
    types: &IntermediateTypes<'_>,
    roots: &BTreeSet<RustIdent>,
    cli: &Cli,
) -> BTreeSet<String> {
    let mut walker = ClaimWalker::new(types, cli);
    for root in roots {
        // A root itself is claimed by `reg.add`; start at its generated body so a root is not
        // claimed merely for being a root, while a recursive path may still legitimately reach and
        // preclaim it before the rows. Every transitive claim precedes every registration row.
        // `claim_named` with `claim = false` walks every generated schema-body shape without
        // placing the root in the ledger a second time (and stops itself at custom/extern roots).
        walker.claim_named(root, false);
    }
    walker.claims
}

/// Own one schema-claim traversal's state across all roots.
/// Visited names are global to the traversal, never popped on recursive return.
struct ClaimWalker<'types, 'ast, 'cli> {
    types: &'types IntermediateTypes<'ast>,
    cli: &'cli Cli,
    claims: BTreeSet<String>,
    visited: BTreeSet<RustIdent>,
    generic_bases: BTreeSet<RustIdent>,
}

impl<'types, 'ast, 'cli> ClaimWalker<'types, 'ast, 'cli> {
    fn new(types: &'types IntermediateTypes<'ast>, cli: &'cli Cli) -> Self {
        Self {
            types,
            cli,
            claims: BTreeSet::new(),
            visited: BTreeSet::new(),
            generic_bases: types.generic_extern_base_idents(),
        }
    }

    fn claim_named(&mut self, ident: &RustIdent, claim: bool) {
        let types = self.types;
        let cli = self.cli;
        if !types.scope(ident).export() || self.generic_bases.contains(ident) {
            return;
        }
        let Some(rust_struct) = types.rust_struct(ident) else {
            if let Some(alias) = types.resolve_alias(&AliasIdent::Rust(ident.clone())) {
                self.walk_subschema(&alias);
            }
            return;
        };
        if claim {
            self.claims.insert(format!(
                "reg.claim_reachable::<{}>();",
                rust_crate_struct_from_wasm(types, ident, cli)
            ));
        }
        if !self.visited.insert(ident.clone())
            || rust_struct.config().custom_json
            || matches!(rust_struct.variant(), RustStructType::Extern)
        {
            return;
        }
        match rust_struct.variant() {
            RustStructType::Record(record) => {
                for field in &record.fields {
                    // A record's natural-any adapter writes the complete field schema itself; it
                    // never invokes the field type's JsonSchema impl. Reuse the emitter's exact
                    // classifier so a later adapter shape cannot make this inventory drift.
                    if !rust_struct.config().custom_json
                        && natural_any_position(&field.rust_type, field.optional, cli).is_none()
                    {
                        self.walk_subschema(&field.rust_type);
                    }
                }
                for row in record.dynamic_rows() {
                    match &row.kind {
                        // Open-table and general-key rows call only their value helper; the
                        // primitive/any peeked-key path delegates to BTreeMap's key schema body.
                        RestKind::MapEntries { domain, range, .. } => {
                            if !record.is_open_table() && row.map_key_uses_peeked_path(types) {
                                self.walk_schema_body(domain);
                            }
                            // Both the flattened open-map adapter and the hand-written open-table
                            // schema emit natural JSON directly for an any range, without calling
                            // that alias's tagged JsonSchema implementation.
                            if !matches!(
                                range.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Any
                            ) {
                                self.walk_subschema(range);
                            }
                        }
                        RestKind::ArrayTail { element, .. } => {
                            // The rest-tail field's Seq adapter likewise owns an any element's
                            // schema, including an alias-hidden any.
                            if !matches!(
                                element.conceptual_type.resolve_alias_shallow(),
                                ConceptualRustType::Any
                            ) {
                                self.walk_subschema(element)
                            }
                        }
                    }
                }
            }
            RustStructType::Array { element_type, .. } => self.walk_subschema(element_type),
            RustStructType::Table {
                domain,
                range,
                bounds,
            } => {
                // A named table emits as its registered alias's collection type. Reconstruct that
                // type with the rule-owned bounds/flavor, not merely its raw conceptual Map: the
                // preserve flavor is PairMap and therefore reaches its key through a subschema.
                let mut table = RustType::new(ConceptualRustType::Map(
                    Box::new(domain.clone()),
                    Box::new(range.clone()),
                ));
                if let Some(bounds) = bounds {
                    table = table.with_occurrence_bounds(*bounds);
                }
                table = table.with_duplicates_policy(rust_struct.config().duplicates);
                self.walk_schema_body(&table);
            }
            RustStructType::TypeChoice { variants, .. }
            | RustStructType::GroupChoice { variants, .. }
            | RustStructType::CStyleEnum { variants } => {
                for variant in variants {
                    match &variant.data {
                        EnumVariantData::RustType(ty) => {
                            // Enum newtype arms have only the direct natural-any adapter (unlike
                            // record fields' broader container classifier). It owns that arm's
                            // complete schema, so the tagged AnyCbor alias is not reachable.
                            if !rust_struct.config().custom_json
                                && matches!(
                                    ty.conceptual_type.resolve_alias_shallow(),
                                    ConceptualRustType::Any
                                )
                            {
                                continue;
                            }
                            self.walk_subschema(ty)
                        }
                        EnumVariantData::Inlined(record) => {
                            for field in &record.fields {
                                self.walk_subschema(&field.rust_type);
                            }
                        }
                    }
                }
            }
            // Generated wrappers call the wrapped type's `json_schema` body and mirror its
            // `inline_schema`; this follows deeper subschema edges without claiming the wrapped
            // nominal merely because it is stored in the wrapper.
            RustStructType::Wrapper { wrapped, .. } => self.walk_schema_body(wrapped),
            RustStructType::Extern | RustStructType::RawBytesType => {}
        }
    }

    fn walk_subschema(&mut self, ty: &RustType) {
        let types = self.types;
        let cli = self.cli;
        // A recursive static-array descriptor supplies this whole schema body itself. In
        // particular, a nullable alias can lower to `Option<[T; N]>`, which has no standalone
        // JsonSchema implementation on the pinned array-trait versions; following it here would
        // register an impossible `claim_reachable` despite the enclosing callback never asking for
        // that trait.
        if recursive_exact_array_descriptor(types, ty, false, false, cli).is_some() {
            self.walk_descriptor_leaf(ty);
            return;
        }
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident) => self.claim_named(ident, true),
            ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) => {
                // A generated, export-scope alias is itself a nameable type token at the derived
                // field's `subschema_for` boundary. Claim it, then follow the target as a BODY:
                // aliases share their target's one JsonSchema impl and add no subschema edge.
                // An alias owned by an extern dependency is equally opaque: json-gen does not own
                // that crate's inventory and must not infer claims from the dep-exported target.
                if !types.scope(ident).export() {
                    return;
                }
                // An exact homogeneous array's field annotation supplies its complete schema via
                // the static-array adapter. Its alias has no `JsonSchema` implementation at wide
                // lengths, so claiming it would emit an unsatisfied `claim_reachable::<[T; N]>`.
                if !types.alias_projection_suppressed(ident)
                    && !ty.is_type_enforced_exact_homogeneous_array()
                {
                    self.claims.insert(format!(
                        "reg.claim_reachable::<{}>();",
                        rust_crate_struct_from_wasm(types, ident, cli)
                    ));
                }
                let target = clone_with_conceptual_type(ty, inner);
                self.walk_schema_body(&target);
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), inner) => {
                let target = clone_with_conceptual_type(ty, inner);
                self.walk_schema_body(&target)
            }
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                self.walk_subschema(inner)
            }
            ConceptualRustType::Map(key, value) => {
                // Schemars' ordinary BTreeMap form invokes the key's schema body and takes a
                // subschema for the value. PairMap is tuple-based and takes subschemas for both.
                if ty.is_preserve_pair_map() {
                    self.walk_subschema(key);
                } else {
                    self.walk_schema_body(key);
                }
                self.walk_subschema(value);
            }
            ConceptualRustType::Any
            | ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_) => {}
        }
    }

    fn walk_descriptor_leaf(&mut self, ty: &RustType) {
        let types = self.types;
        match &ty.conceptual_type {
            // A named transparent alias is only a carrier spelling here. Unfold it so a
            // `Vec<Alias<Option<[T; N]>>>` reaches a real terminal leaf without claiming
            // the alias's impossible pinned-array JsonSchema implementation.
            ConceptualRustType::Rust(ident) => {
                if let Some(alias) = types.resolve_alias(&AliasIdent::Rust(ident.clone())) {
                    if types.scope(ident).export() {
                        self.walk_descriptor_leaf(&alias);
                    }
                } else {
                    self.claim_named(ident, true);
                }
            }
            ConceptualRustType::Alias(_, inner) => {
                let target = clone_with_conceptual_type(ty, inner);
                self.walk_descriptor_leaf(&target);
            }
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                self.walk_descriptor_leaf(inner)
            }
            // NaturalAny owns a permissive schema directly, so it has no JsonSchema edge.
            ConceptualRustType::Any
            | ConceptualRustType::Map(_, _)
            | ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_) => {}
        }
    }

    fn walk_schema_body(&mut self, ty: &RustType) {
        let types = self.types;
        match &ty.conceptual_type {
            ConceptualRustType::Rust(ident) => self.claim_named(ident, false),
            ConceptualRustType::Alias(AliasIdent::Rust(ident), inner) => {
                // A body call on an alias from an extern dependency is still a call into code this
                // crate does not own. Do not turn its transparent target into own-crate claims.
                if !types.scope(ident).export() {
                    return;
                }
                // This is the BODY counterpart of the subschema alias descent above: retain the
                // outer occurrence configuration (pair-map flavor and bounds in particular),
                // replacing only the conceptual node as emitted aliases do.
                let target = clone_with_conceptual_type(ty, inner);
                self.walk_schema_body(&target)
            }
            ConceptualRustType::Alias(AliasIdent::Reserved(_), inner) => {
                let target = clone_with_conceptual_type(ty, inner);
                self.walk_schema_body(&target)
            }
            // Vec/Option schema bodies reference their element through schemars' subschema path.
            ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
                self.walk_subschema(inner)
            }
            ConceptualRustType::Map(key, value) => {
                if ty.is_preserve_pair_map() {
                    self.walk_subschema(key);
                } else {
                    self.walk_schema_body(key);
                }
                self.walk_subschema(value);
            }
            // Natural any schemas are emitted directly and intentionally never call AnyCbor's
            // tagged schema; there is no named definition to claim.
            ConceptualRustType::Any
            | ConceptualRustType::Fixed(_)
            | ConceptualRustType::Primitive(_) => {}
        }
    }
}
