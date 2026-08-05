//! The recursive-type boundary: which cycles in a spec generate Rust that compiles, which ones the
//! generator repairs for the user, and which ones it refuses.
//!
//! Recursion is supported — `tree = [value: uint, children: [* tree]]` generates and compiles — so
//! "this spec has a cycle" is not the defect. The predicate the emitted Rust actually obeys is
//! narrower, and it is a property of the EMITTED TYPES rather than of the CDDL:
//!
//! * A transparent `pub type` alias cannot take part in a type cycle at all. rustc expands aliases
//!   structurally, so heap indirection INSIDE the alias body does not break the cycle:
//!   `pub type X = Vec<X>;` is rustc **E0391**, "cycle detected when expanding type alias".
//! * A cycle between NOMINAL types (structs/enums) compiles only if some step of it crosses heap
//!   indirection. A direct field — `Option<T>` included, since an `Option` stores its payload
//!   inline — leaves the type infinitely sized: rustc **E0072**.
//!
//! Terminability of the CDDL is neither necessary nor sufficient: `x = [* x]` terminates (an empty
//! array satisfies it) and still fails E0391.
//!
//! `dep_graph`'s `Recursive type: …` notice cannot be promoted into this boundary, which is why
//! this module exists separately. That notice fires on SUPPORTED cycles too, it is rule-granular
//! (member attribution is lost), and the back edge it names is traversal-order dependent — while
//! generated output is promised invariant under rule ordering. Everything below is computed from
//! the FINALIZED IR, where struct kinds and member shapes are known, and every set it reports is a
//! canonical property of the cycle (a sorted `BTreeSet` of its members), never of a DFS back edge.
//!
//! ## The two graphs
//!
//! The two rustc failures are two different graphs over the same idents, and conflating them is
//! what makes a classifier reject the supported class:
//!
//! * **Alias expansion.** Nodes: idents emitted as `pub type`. Edge `A → B` when `A`'s body names
//!   `B` ANYWHERE, inside a `Vec`/`BTreeMap` included. A cycle here is E0391.
//! * **Nominal containment.** Nodes: idents emitted as a struct/enum. Edge `N → M` when `N` holds
//!   an `M` with no heap indirection between them — following alias hops (an alias to a nominal IS
//!   that nominal) but stopping at the first `Vec`/`BTreeMap`. A cycle here is E0072.
//!
//! `md = mdmap / int` with `mdmap = { * text => md }` is exactly why: it has a cycle, and it
//! belongs to NEITHER graph — `mdmap`'s alias body names the nominal `md` (so no alias-expansion
//! edge), and `md`'s variant reaches `mdmap` whose body is a `BTreeMap` (so no containment edge).
//! It compiles, and the boundary leaves it alone.
//!
//! ## The verdicts
//!
//! * **Supported** — in neither graph's cycles. Unchanged: generates, compiles.
//! * **Alias cycle with a named collection in it** — auto-`@newtype`: every collection-backed rule
//!   of the cycle is emitted as a wrapper struct instead of a `pub type`, which makes it nominal
//!   and ends the expansion. "Every collection-backed member of the cycle" is chosen because it is
//!   a canonical property of the cycle; picking one member would key the emitted API on rule
//!   ordering. The caller re-runs the IR build with those rules marked, so the wrapper is minted by
//!   the very machinery a hand-written `; @newtype` uses and every downstream surface (wasm,
//!   preserve-encodings, emit-tests) exists for it for free.
//! * **Alias cycle with nothing to nominalize**, and **every nominal-containment cycle** — refused
//!   gracefully, naming the cycle and the members that close it. No `Box` is emitted in any type
//!   position by this generator and no directive asks for one, so there is no auto-fix to offer for
//!   the E0072 class; the honest remedy is to restructure the spec.

use std::collections::{BTreeMap, BTreeSet};

use crate::intermediate::{
    AliasIdent, ConceptualRustType, IntermediateTypes, RustIdent, RustStructType, RustType,
};

/// What the boundary decided about one finalized IR.
#[derive(Default)]
pub struct Verdict {
    /// Rules to re-run the IR build with `@newtype` forced on. Empty for a spec whose cycles are
    /// all supported or all unfixable.
    pub auto_newtype: BTreeSet<RustIdent>,
    /// The stderr announcements for `auto_newtype`, one per cycle repaired.
    pub announcements: Vec<String>,
    /// Graceful-rejection messages, in a deterministic order.
    pub refusals: Vec<String>,
}

/// The user-facing name of a rule: its CDDL source spelling when the ident came from a rule, and
/// the Rust ident otherwise (a struct the IR synthesized has no source spelling to quote).
fn label(types: &IntermediateTypes, ident: &RustIdent) -> String {
    types
        .source_rule_name(ident)
        .unwrap_or(ident.as_ref())
        .to_owned()
}

/// `` `a`, `b`, `c` `` — the members of a cycle, sorted, for a message.
fn cycle_label(types: &IntermediateTypes, cycle: &BTreeSet<RustIdent>) -> String {
    cycle
        .iter()
        .map(|ident| format!("`{}`", label(types, ident)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Every ident NAMED by `ty` as the emitted Rust would spell it: recursing through the containers
/// that render their parameters (`Vec`, `BTreeMap`, `Option`) and stopping at a named type, whose
/// own definition is a separate node rather than part of this one's body.
fn named_idents(ty: &ConceptualRustType, out: &mut BTreeSet<RustIdent>) {
    match ty {
        ConceptualRustType::Rust(ident) => {
            out.insert(ident.clone());
        }
        // An `Alias` renders as the alias NAME, so the reference is to that name — the base type
        // behind it is the alias's own body and belongs to the alias's node, not to this one. A
        // RESERVED alias names a prelude type instead, which has no node of its own, so its base is
        // what the reference reaches.
        ConceptualRustType::Alias(AliasIdent::Rust(ident), _) => {
            out.insert(ident.clone());
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(_), base) => named_idents(base, out),
        ConceptualRustType::Array(inner) | ConceptualRustType::Optional(inner) => {
            named_idents(&inner.conceptual_type, out)
        }
        ConceptualRustType::Map(key, value) => {
            named_idents(&key.conceptual_type, out);
            named_idents(&value.conceptual_type, out);
        }
        ConceptualRustType::Fixed(_)
        | ConceptualRustType::Primitive(_)
        | ConceptualRustType::Any => (),
    }
}

/// Every ident reachable from `ty` WITHOUT crossing heap indirection, following alias hops.
///
/// `Vec`/`BTreeMap` end the walk: their contents live behind a pointer, which is exactly what makes
/// a nominal cycle finite. `Option` does not — it stores its payload inline — which is why the
/// docs' old "close the cycle through an optional member" remedy was wrong.
fn inline_idents(
    ty: &ConceptualRustType,
    aliases: &BTreeMap<RustIdent, RustType>,
    seen_aliases: &mut BTreeSet<RustIdent>,
    out: &mut BTreeSet<RustIdent>,
) {
    match ty {
        ConceptualRustType::Rust(ident) => {
            if let Some(body) = aliases.get(ident) {
                // An alias in a member position is transparent: the member's real type is the
                // alias's body. The guard is for an alias cycle, which the OTHER graph reports.
                if seen_aliases.insert(ident.clone()) {
                    inline_idents(&body.conceptual_type, aliases, seen_aliases, out);
                }
            } else {
                out.insert(ident.clone());
            }
        }
        ConceptualRustType::Alias(AliasIdent::Rust(ident), base) => {
            if aliases.contains_key(ident) {
                if seen_aliases.insert(ident.clone()) {
                    inline_idents(base, aliases, seen_aliases, out);
                }
            } else {
                out.insert(ident.clone());
            }
        }
        ConceptualRustType::Alias(AliasIdent::Reserved(_), base) => {
            inline_idents(base, aliases, seen_aliases, out)
        }
        ConceptualRustType::Optional(inner) => {
            inline_idents(&inner.conceptual_type, aliases, seen_aliases, out)
        }
        // heap indirection — the recursion terminates here, in the emitted Rust and in this walk
        ConceptualRustType::Array(_) | ConceptualRustType::Map(_, _) => (),
        ConceptualRustType::Fixed(_)
        | ConceptualRustType::Primitive(_)
        | ConceptualRustType::Any => (),
    }
}

/// A record's fields as `(member label, type)` pairs. The label is what a refusal quotes back, so
/// it carries the position (`field` / `variant … field`) the reader needs to find the member.
fn record_members<'a>(
    record: &'a crate::intermediate::RustRecord,
    prefix: &str,
) -> Vec<(String, &'a RustType)> {
    record
        .fields
        .iter()
        .map(|field| (format!("{prefix}`{}`", field.name), &field.rust_type))
        .collect()
}

/// Reachability closure, computed per node so an SCC can be read off it. Sorted containers
/// throughout, so every set this module reports is invariant under rule ordering.
fn reachability(edges: &BTreeMap<RustIdent, BTreeSet<RustIdent>>) -> ReachMap {
    let mut reach: ReachMap = BTreeMap::new();
    for start in edges.keys() {
        let mut seen = BTreeSet::new();
        let mut stack = vec![start.clone()];
        while let Some(node) = stack.pop() {
            for next in edges.get(&node).into_iter().flatten() {
                if seen.insert(next.clone()) {
                    stack.push(next.clone());
                }
            }
        }
        reach.insert(start.clone(), seen);
    }
    reach
}

type ReachMap = BTreeMap<RustIdent, BTreeSet<RustIdent>>;

/// The strongly-connected components of `edges` that actually contain a cycle (a self-loop counts),
/// each as a sorted member set. Deduplicated, and ordered by their smallest member.
fn cyclic_components(edges: &BTreeMap<RustIdent, BTreeSet<RustIdent>>) -> Vec<BTreeSet<RustIdent>> {
    let reach = reachability(edges);
    let mut components: BTreeSet<BTreeSet<RustIdent>> = BTreeSet::new();
    for (node, reachable) in &reach {
        if !reachable.contains(node) {
            continue;
        }
        let component: BTreeSet<RustIdent> = reachable
            .iter()
            .filter(|other| {
                reach
                    .get(*other)
                    .is_some_and(|back| back.contains(node) && back.contains(*other))
            })
            .cloned()
            .collect();
        components.insert(component);
    }
    components.into_iter().collect()
}

/// Classify every cycle in a finalized IR.
///
/// `already_forced` names the rules a previous pass already auto-`@newtype`d, so a cycle that
/// somehow survived its own repair is refused rather than driving the caller's re-run loop forever.
pub fn classify(types: &IntermediateTypes, already_forced: &BTreeSet<RustIdent>) -> Verdict {
    let mut verdict = Verdict::default();

    // Nodes of the alias-expansion graph: every ident this run emits as a `pub type`.
    let aliases: BTreeMap<RustIdent, RustType> = types
        .type_aliases()
        .iter()
        .filter_map(|(alias_ident, info)| match alias_ident {
            AliasIdent::Rust(ident) if info.gen_rust_alias => {
                Some((ident.clone(), info.base_type.clone()))
            }
            _ => None,
        })
        .collect();

    // --- E0391: cycles among the emitted `pub type` aliases -------------------------------------
    let alias_edges: BTreeMap<RustIdent, BTreeSet<RustIdent>> = aliases
        .iter()
        .map(|(ident, body)| {
            let mut named = BTreeSet::new();
            named_idents(&body.conceptual_type, &mut named);
            // The transparent-alias registration seam strips `Alias(Target, …)` to store the
            // structural body. That representation is right for emission, but an alias-expansion
            // graph must also preserve the source rule edge: otherwise `hop_alias = hop_arr` can
            // look like a self-edge after `hop_arr`'s collection base is inlined and the SCC loses
            // the collection the existing auto-`@newtype` repair needs.
            if let Some(target) = types
                .type_aliases()
                .get(&AliasIdent::Rust(ident.clone()))
                .and_then(|info| info.stripped_alias_target.as_ref())
            {
                named.insert(target.clone());
            }
            named.retain(|target| aliases.contains_key(target));
            (ident.clone(), named)
        })
        .collect();
    for cycle in cyclic_components(&alias_edges) {
        // A member that can carry the repair: a rule whose body IS a named collection, so
        // `@newtype` has something to wrap. Restricted to rules this crate owns and emits — a rule
        // that arrived from a dependency's extern-interface export is the dependency's to shape,
        // and a struct the IR synthesized has no rule for a directive to sit on.
        let repairable: BTreeSet<RustIdent> = cycle
            .iter()
            .filter(|ident| {
                matches!(
                    types.rust_struct(ident).map(|rs| rs.variant()),
                    Some(RustStructType::Array { .. } | RustStructType::Table { .. })
                ) && types.scope(ident).export()
                    && types.source_rule_name(ident).is_some()
            })
            .cloned()
            .collect();
        if repairable.is_empty()
            || repairable
                .iter()
                .all(|ident| already_forced.contains(ident))
        {
            verdict.refusals.push(format!(
                "recursive rule cycle over {members}: every rule in the cycle is emitted as a \
                 transparent `pub type` alias, and a `pub type` cannot take part in a type cycle — \
                 rustc expands aliases structurally, so heap indirection inside the alias body does \
                 not break it and the generated crate would fail to build (E0391, \"cycle detected \
                 when expanding type alias\"). No rule in this cycle names a collection, so there \
                 is nothing in it to emit as a wrapper struct instead. Give at least one rule in \
                 the cycle a body of its own — a record, or a collection (`[* ...]` / `{{* ... => \
                 ...}}`), which is emitted as a wrapper struct once it is the thing the cycle passes \
                 through — or break the cycle.",
                members = cycle_label(types, &cycle),
            ));
            continue;
        }
        verdict.announcements.push(format!(
            "recursive rule cycle over {members}: auto-`@newtype` applied to {repaired}. A \
             transparent `pub type` alias cannot take part in a type cycle (rustc E0391), so every \
             named collection in the cycle is emitted as a wrapper struct instead of a `pub type` \
             — which changes those types' Rust and wasm API shape. Write `; @newtype` on those \
             rules to make the wrapper explicit and silence this notice.",
            members = cycle_label(types, &cycle),
            repaired = cycle_label(types, &repairable),
        ));
        verdict.auto_newtype.extend(repairable);
    }

    // --- E0072: cycles among the emitted structs/enums, with no heap indirection -----------------
    // Nodes are the idents that emit a nominal type. An `Extern` is somebody else's hand-written
    // type and a `RawBytesType` is the runtime's, so neither has a member shape to walk; an
    // `Array`/`Table` is an alias and belongs to the graph above.
    let nominals: BTreeSet<RustIdent> = types
        .rust_structs()
        .iter()
        .filter(|(ident, rust_struct)| {
            !aliases.contains_key(*ident)
                && matches!(
                    rust_struct.variant(),
                    RustStructType::Record(_)
                        | RustStructType::TypeChoice { .. }
                        | RustStructType::GroupChoice { .. }
                        | RustStructType::Wrapper { .. }
                        | RustStructType::CStyleEnum { .. }
                )
        })
        .map(|(ident, _)| ident.clone())
        .collect();
    // Every inline-containment edge, labelled with the member that carries it. Sorted by label, so
    // the "members that close the cycle" list a refusal prints is canonical.
    let mut nominal_edges: BTreeMap<RustIdent, BTreeSet<RustIdent>> = BTreeMap::new();
    let mut edge_labels: BTreeMap<(RustIdent, RustIdent), BTreeSet<String>> = BTreeMap::new();
    for ident in &nominals {
        let rust_struct = types
            .rust_struct(ident)
            .expect("collected from rust_structs");
        let mut members: Vec<(String, &RustType)> = Vec::new();
        match rust_struct.variant() {
            RustStructType::Record(record) => members.extend(record_members(record, "field ")),
            RustStructType::TypeChoice { variants }
            | RustStructType::GroupChoice { variants, .. }
            | RustStructType::CStyleEnum { variants } => {
                for variant in variants {
                    match &variant.data {
                        crate::intermediate::EnumVariantData::RustType(ty) => {
                            members.push((format!("variant `{}`", variant.name), ty))
                        }
                        crate::intermediate::EnumVariantData::Inlined(record) => members.extend(
                            record_members(record, &format!("variant `{}` field ", variant.name)),
                        ),
                    }
                }
            }
            RustStructType::Wrapper { wrapped, .. } => {
                members.push(("the wrapped type".to_owned(), wrapped))
            }
            _ => (),
        }
        for (member, ty) in members {
            let mut targets = BTreeSet::new();
            inline_idents(
                &ty.conceptual_type,
                &aliases,
                &mut BTreeSet::new(),
                &mut targets,
            );
            for target in targets {
                if !nominals.contains(&target) {
                    continue;
                }
                edge_labels
                    .entry((ident.clone(), target.clone()))
                    .or_default()
                    .insert(format!(
                        "`{owner}` {member} (type `{target_label}`)",
                        owner = label(types, ident),
                        target_label = label(types, &target),
                    ));
                nominal_edges
                    .entry(ident.clone())
                    .or_default()
                    .insert(target);
            }
        }
    }
    for cycle in cyclic_components(&nominal_edges) {
        let closing: BTreeSet<&String> = edge_labels
            .iter()
            .filter(|((from, to), _)| cycle.contains(from) && cycle.contains(to))
            .flat_map(|(_, labels)| labels.iter())
            .collect();
        verdict.refusals.push(format!(
            "recursive rule cycle over {members}: the generated Rust types contain one another \
             directly, with no heap indirection anywhere on the cycle, so the generated crate would \
             fail to build (E0072, \"recursive type has infinite size\"). The members that close \
             the cycle: {closing}. Route at least one step of the cycle through a collection (`[* \
             ...]` / `{{* ... => ...}}`), which puts the recursive occurrence behind a \
             `Vec`/`BTreeMap`; an OPTIONAL member does not help, because an `Option` stores its \
             payload inline, and no directive boxes a member (this generator emits no `Box` in any \
             type position).",
            members = cycle_label(types, &cycle),
            closing = closing
                .into_iter()
                .cloned()
                .collect::<Vec<_>>()
                .join(", "),
        ));
    }

    verdict.refusals.sort();
    verdict
}
