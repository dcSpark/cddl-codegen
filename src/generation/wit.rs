//! The WIT face of the generated workspace: the naming rules and the pre-generation detectors that
//! decide whether a spec CAN be projected to a WIT package at all.
//!
//! # Contract
//!
//! This module is a pure IR→WIT projection, structurally a sibling of [`super::extern_interface`]:
//! it walks the FINALIZED IR, renders deterministically out of `BTreeMap`/`BTreeSet`s, and anything
//! it cannot render is EXCLUDED AND RECORDED rather than panicked on. It must not depend on the
//! guest-crate emitters — the dependency runs one way, `component.rs` → `wit.rs` — because the
//! rust↔WIT parity gate and the cross-crate export both consume the projection, and a
//! re-derivation at the second consumer is a silent-drift source.
//!
//! # What is here now
//!
//! The naming layer (keyword escaping over [`crate::utils::convert_to_kebab_case`]) and the two
//! detectors that run at `finalize` under `--component`. The projection value itself, and the
//! renderers over it, land with the WIT emitter.

use crate::cli::Cli;
use crate::intermediate::{
    AliasIdent, ConceptualRustType, EnumVariantData, IntermediateTypes, ModuleScope, RestKind,
    RustIdent, RustStructType, RustType,
};
use crate::utils::convert_to_kebab_case;
use std::collections::{BTreeMap, BTreeSet};

/// Every word the WIT lexer treats as a keyword across the toolchain versions this project supports
/// — deliberately a **superset** of any single version's set, because the set has moved in BOTH
/// directions and escaping a non-keyword is accepted everywhere.
///
/// Provenance, read out of the lexers rather than from memory (the `match` on the lexed word in
/// `wit-parser`'s `src/ast/lex.rs`):
///
/// - **wit-parser 0.247 / 0.252** (the pinned floor and its successor) — the whole list below except
///   `float32`/`float64`.
/// - **wit-parser 0.240** (standing in for the wasm-tools ≤ 1.231-era consumer floor) — the same
///   list except `map`, PLUS `float32`/`float64`, which it lexes as the `f32`/`f64` keywords under
///   its `require_f32_f64` compatibility switch and 0.247 has since dropped.
///
/// So `map` is a keyword only at the newer end and `float32`/`float64` only at the older end. A name
/// escaped as `%map` parses at both; escaping the union is the only choice compatible in both
/// directions. Bump this list when the pinned `wit-parser` moves — by UNION, never by replacement.
///
/// `_` is a keyword at every version and is deliberately ABSENT: `convert_to_kebab_case` maps
/// underscores to word separators, so a bare `_` cannot be produced, and `%_` is not a valid escaped
/// identifier anyway (an id must have at least one word).
///
/// `#[allow(dead_code)]`: its only reader is the WIT projection, the component face's next piece.
/// The naming layer lands first so the detectors and the flag surface are built against a settled
/// spelling rather than one invented twice.
#[allow(dead_code)]
pub(crate) const WIT_KEYWORDS: &[&str] = &[
    "as",
    "async",
    "bool",
    "borrow",
    "char",
    "constructor",
    "enum",
    "error-context",
    "export",
    "f32",
    "f64",
    "flags",
    "float32",
    "float64",
    "from",
    "func",
    "future",
    "import",
    "include",
    "interface",
    "list",
    "map",
    "option",
    "own",
    "package",
    "record",
    "resource",
    "result",
    "s16",
    "s32",
    "s64",
    "s8",
    "static",
    "stream",
    "string",
    "tuple",
    "type",
    "u16",
    "u32",
    "u64",
    "u8",
    "use",
    "variant",
    "with",
    "world",
];

/// A WIT identifier as it is WRITTEN: `%`-prefixed iff it is a keyword.
///
/// Applied at RENDER time only. The `%` is WIT syntax, not part of the name — it does not reach the
/// generated Rust bindings, and both the name-collision detector and the rust↔WIT parity gate
/// compare the UNESCAPED spelling — so escaping must never be baked into
/// [`convert_to_kebab_case`].
#[allow(dead_code)] // see WIT_KEYWORDS above
pub(crate) fn wit_escape(name: &str) -> String {
    if WIT_KEYWORDS.contains(&name) {
        format!("%{name}")
    } else {
        name.to_owned()
    }
}

/// A WIT package identifier: `<namespace>:<name>@<version>`.
///
/// Parsed from `--wit-package`, or derived from `--lib-name` when the flag is absent. The default
/// cannot be a clap `default_value` because it depends on another flag's value, which is why the
/// flag's field is an `Option<String>` and this type is minted by [`Cli::wit_package`].
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct WitPackageId {
    pub namespace: String,
    pub name: String,
    pub version: String,
}

impl WitPackageId {
    /// The default for a `--lib-name`: `cddl:<kebab lib-name>@0.1.0`.
    #[allow(dead_code)] // reached through `Cli::wit_package`, whose caller is the projection
    pub(crate) fn default_for_lib_name(lib_name: &str) -> Self {
        Self {
            namespace: "cddl".to_owned(),
            name: convert_to_kebab_case(lib_name),
            version: "0.1.0".to_owned(),
        }
    }

    /// Parse a `--wit-package` value: `<ns>:<name>` with an optional `@<version>`.
    ///
    /// Shape-checked here rather than at first use so a typo is a clap error naming the flag, not a
    /// WIT parse failure three stages later against a file the user did not write. The version is
    /// checked as dot-separated non-empty numeric-led parts rather than against full semver: WIT
    /// accepts pre-release/build metadata, and a stricter check here would reject values the
    /// resolver takes.
    pub(crate) fn parse(s: &str) -> Result<Self, String> {
        let (id, version) = match s.split_once('@') {
            Some((id, version)) => (id, version),
            None => (s, "0.1.0"),
        };
        let (namespace, name) = id.split_once(':').ok_or_else(|| {
            format!(
                "--wit-package value must be <namespace>:<name>[@<version>] (e.g. \
                 `cddl:my-lib@0.1.0`), got: {s:?}"
            )
        })?;
        for (label, part) in [("namespace", namespace), ("name", name)] {
            if part.is_empty() {
                return Err(format!(
                    "--wit-package {label} is empty in {s:?}: both sides of the `:` are required, \
                     as in `cddl:my-lib@0.1.0`"
                ));
            }
            if let Some(c) = part
                .chars()
                .find(|c| !matches!(c, 'a'..='z' | '0'..='9' | '-'))
            {
                return Err(format!(
                    "invalid character {c:?} in the --wit-package {label} {part:?}: a WIT package \
                     identifier is kebab-case ASCII ([a-z0-9-]), as in `cddl:my-lib@0.1.0`"
                ));
            }
            if part.starts_with('-') || part.ends_with('-') || part.contains("--") {
                return Err(format!(
                    "malformed --wit-package {label} {part:?}: a WIT identifier is `-`-separated \
                     words, so it can neither begin nor end with `-` nor carry an empty word"
                ));
            }
            if part.starts_with(|c: char| c.is_ascii_digit()) {
                return Err(format!(
                    "--wit-package {label} {part:?} begins with a digit: a WIT identifier's first \
                     word must start with a letter"
                ));
            }
        }
        if version.is_empty() {
            return Err(format!(
                "--wit-package version is empty in {s:?}: drop the `@` to take the default \
                 `@0.1.0`, or spell a version as in `cddl:my-lib@0.1.0`"
            ));
        }
        // The `x.y.z` core, checked before any pre-release (`-`) or build (`+`) metadata, which WIT
        // accepts and which nothing here needs to interpret.
        let core = version
            .split(['-', '+'])
            .next()
            .expect("split always yields at least one part");
        let parts: Vec<&str> = core.split('.').collect();
        if parts.len() != 3
            || parts
                .iter()
                .any(|p| p.is_empty() || !p.chars().all(|c| c.is_ascii_digit()))
        {
            return Err(format!(
                "malformed --wit-package version {version:?}: WIT versions are semver, so the \
                 leading part must be `<major>.<minor>.<patch>` (e.g. `0.1.0`), optionally \
                 followed by `-<pre-release>` or `+<build>`"
            ));
        }
        Ok(Self {
            namespace: namespace.to_owned(),
            name: name.to_owned(),
            version: version.to_owned(),
        })
    }
}

impl std::fmt::Display for WitPackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}@{}", self.namespace, self.name, self.version)
    }
}

/// Strong-uniqueness collisions in the WIT surface, one message per collision.
///
/// WIT compares names after stripping the `[method]`/`[static]`/`[constructor]` prefixes, and an
/// interface is one flat namespace, so three levels can collide: interface names against the world
/// name at PACKAGE level, type names within one INTERFACE, and member names (including a member
/// equal to the resource's own name) within one RESOURCE. All three fall out of one walk of the
/// projection, which is why they are one function with three message shapes rather than three
/// sibling detectors — the AGENTS.md parallel-sibling ruling is about the wasm WRAPPER-name family,
/// whose members have genuinely different inputs.
///
/// Wired at `finalize` now and returning nothing, because every one of the three checks reads the
/// projection value, which lands with the WIT emitter. The invocation site exists first so the
/// detector's arrival is a body change rather than a plumbing change — and so the flag surface
/// shipped in this state cannot be mistaken for one that has already been checked.
pub(crate) fn wit_name_collisions(_types: &IntermediateTypes, _cli: &Cli) -> Vec<String> {
    Vec::new()
}

/// Cross-scope reference CYCLES, one message per non-trivial strongly-connected component.
///
/// Each exported `ModuleScope` becomes one WIT `interface`, and a type in one interface reaching a
/// type in another is a `use` — which WIT requires to be ACYCLIC. The rule is interface-level and
/// therefore strictly stronger than a type-level one: `A.x → B.y` together with `B.z → A.w` is no
/// type cycle at all, but it is an interface cycle. cddl-codegen accepts such a spec today on the
/// rust face (rust modules may reference each other freely), so this is a `--component`-only
/// restriction and it must be reported as one.
///
/// Deliberately NOT built on `dep_graph::find_references`: that walks the AST rather than the IR,
/// and its cycle handling is a warn-and-continue over RULES, which is the wrong granularity twice
/// over.
pub(crate) fn wit_scope_cycles(types: &IntermediateTypes, _cli: &Cli) -> Vec<String> {
    // scope -> scope -> one witness edge (`referencing type`, `referenced type`), kept for the
    // message. `BTreeMap` throughout: the message text is generated output and must be reproducible.
    let mut edges: BTreeMap<ModuleScope, BTreeMap<ModuleScope, (RustIdent, RustIdent)>> =
        BTreeMap::new();
    for (ident, rust_struct) in types.rust_structs() {
        if types.source_rule_name(ident).is_none() {
            continue;
        }
        let from = types.scope(ident);
        if !from.export() {
            continue;
        }
        for referenced in struct_rule_refs(rust_struct.variant(), types) {
            let to = types.scope(&referenced);
            if !to.export() || to == from {
                continue;
            }
            edges
                .entry(from.clone())
                .or_default()
                .entry(to.clone())
                .or_insert_with(|| (ident.clone(), referenced.clone()));
        }
    }

    let mut msgs = Vec::new();
    for component in strongly_connected_components(&edges) {
        if component.len() < 2 {
            continue;
        }
        let scopes = component
            .iter()
            .map(|scope| format!("`{scope}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let mut witnesses = Vec::new();
        for from in &component {
            for to in &component {
                if let Some((referencing, referenced)) =
                    edges.get(from).and_then(|targets| targets.get(to))
                {
                    witnesses.push(format!(
                        "`{referencing}` in `{from}` references `{referenced}` in `{to}`"
                    ));
                }
            }
        }
        msgs.push(format!(
            "WIT interface cycle under --component: the scopes {scopes} reference each other \
             ({witnesses}). Each scope becomes one WIT interface, and WIT requires interfaces \
             linked with `use` to be acyclic, so the emitted package would not resolve. Note the \
             rule is INTERFACE-level, not type-level — no single type has to be recursive for this \
             to fire. Move a type so the scopes are acyclic (a leaf scope both sides reference is \
             the usual fix), or put the spec in a single file so every type lands in one interface.",
            witnesses = witnesses.join("; ")
        ));
    }
    msgs
}

/// The rules a struct references DIRECTLY — its own member types only, never through another
/// struct. The scope graph needs direct edges; a walk that recursed into referenced structs would
/// build the transitive closure, whose witness edges no longer name a reference the user wrote.
///
/// The match is EXHAUSTIVE with no `_ =>` arm (module discipline): a new `RustStructType` variant
/// must be a compile-time decision here, because silently contributing no edges would turn a real
/// cycle into a spec that generates unresolvable WIT.
fn struct_rule_refs(variant: &RustStructType, types: &IntermediateTypes) -> BTreeSet<RustIdent> {
    let mut out = BTreeSet::new();
    let mut walk = |ty: &RustType| collect_projected_refs(&ty.conceptual_type, types, &mut out);
    match variant {
        RustStructType::Record(record) => {
            for field in &record.fields {
                walk(&field.rust_type);
            }
            if let Some(rest) = &record.rest {
                match &rest.kind {
                    RestKind::MapEntries { domain, range, .. } => {
                        walk(domain);
                        walk(range);
                    }
                    RestKind::ArrayTail { element } => walk(element),
                }
            }
        }
        RustStructType::Table { domain, range, .. } => {
            walk(domain);
            walk(range);
        }
        RustStructType::Array { element_type, .. } => walk(element_type),
        RustStructType::TypeChoice { variants }
        | RustStructType::GroupChoice { variants, .. }
        | RustStructType::CStyleEnum { variants } => {
            for variant in variants {
                match &variant.data {
                    EnumVariantData::RustType(ty) => walk(ty),
                    EnumVariantData::Inlined(record) => {
                        for field in &record.fields {
                            walk(&field.rust_type);
                        }
                    }
                }
            }
        }
        RustStructType::Wrapper { wrapped, .. } => walk(wrapped),
        // A hand-written extern and a raw-bytes type have no IR-visible members to reference.
        RustStructType::Extern | RustStructType::RawBytesType => {}
    }
    out
}

/// The rules one occurrence references, AS THE PROJECTION SEES THEM.
///
/// Modeled on `extern_interface::collect_rule_refs`, with the one deliberate difference the WIT face
/// forces: a TRANSPARENT alias is resolved THROUGH rather than recorded. The projection never
/// surfaces CDDL type aliases as WIT types (a surfaced alias breaks the wasm-posture purity
/// invariant), so an alias occurrence's real target is its base type — recording the alias ident
/// instead would attribute the edge to a scope that owns nothing the WIT names. An alias that DOES
/// back a projected type (an `Array`/`Table` typedef registers both a struct and an alias) is
/// recorded as itself.
fn collect_projected_refs(
    ty: &ConceptualRustType,
    types: &IntermediateTypes,
    out: &mut BTreeSet<RustIdent>,
) {
    match ty {
        ConceptualRustType::Rust(ident) => {
            if types.source_rule_name(ident).is_some() {
                out.insert(ident.clone());
            }
        }
        ConceptualRustType::Alias(AliasIdent::Rust(ident), base) => {
            if types.rust_structs().contains_key(ident) && types.source_rule_name(ident).is_some() {
                out.insert(ident.clone());
            } else {
                collect_projected_refs(base, types, out);
            }
        }
        // A reserved alias (`uint`, `text`, …) is a WIT primitive: it names no rule.
        ConceptualRustType::Alias(AliasIdent::Reserved(_), _) => {}
        ConceptualRustType::Optional(inner) | ConceptualRustType::Array(inner) => {
            collect_projected_refs(&inner.conceptual_type, types, out)
        }
        ConceptualRustType::Map(key, value) => {
            collect_projected_refs(&key.conceptual_type, types, out);
            collect_projected_refs(&value.conceptual_type, types, out);
        }
        // `any` projects to the self-contained `any-cbor` alias; primitives and fixed values to WIT
        // primitives. None of them names a rule.
        ConceptualRustType::Primitive(_)
        | ConceptualRustType::Fixed(_)
        | ConceptualRustType::Any => {}
    }
}

/// The strongly-connected components of a directed graph, as sorted node lists in a deterministic
/// order.
///
/// Computed by mutual reachability over the transitive closure rather than by Tarjan: the graph has
/// one node per module scope, i.e. one per input file, so the cubic closure is free — and the
/// closure form is short enough to be read for correctness at a glance, which matters more here
/// than an asymptote nothing will reach.
fn strongly_connected_components<N: Clone + Ord, E>(
    edges: &BTreeMap<N, BTreeMap<N, E>>,
) -> Vec<Vec<N>> {
    let nodes: BTreeSet<N> = edges
        .iter()
        .flat_map(|(from, targets)| std::iter::once(from).chain(targets.keys()))
        .cloned()
        .collect();
    // reach[a] = every node a can get to in one or more steps.
    let mut reach: BTreeMap<N, BTreeSet<N>> = nodes
        .iter()
        .map(|node| {
            let direct = edges
                .get(node)
                .map(|targets| targets.keys().cloned().collect())
                .unwrap_or_default();
            (node.clone(), direct)
        })
        .collect();
    loop {
        let mut grew = false;
        for node in &nodes {
            let expanded: BTreeSet<N> = reach[node]
                .iter()
                .flat_map(|next| reach[next].iter().cloned())
                .collect();
            let entry = reach.get_mut(node).expect("node came from the same map");
            for target in expanded {
                grew |= entry.insert(target);
            }
        }
        if !grew {
            break;
        }
    }
    let mut components: Vec<Vec<N>> = Vec::new();
    let mut assigned: BTreeSet<N> = BTreeSet::new();
    for node in &nodes {
        if assigned.contains(node) {
            continue;
        }
        let component: Vec<N> = nodes
            .iter()
            .filter(|other| {
                *other == node || (reach[node].contains(other) && reach[other].contains(node))
            })
            .cloned()
            .collect();
        assigned.extend(component.iter().cloned());
        components.push(component);
    }
    components
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wit_escape_escapes_the_union_of_the_keyword_sets() {
        // a keyword at every probed version
        assert_eq!(wit_escape("bool"), "%bool");
        // a keyword only at the NEWER end (wit-parser 0.247+), tolerated escaped at the older one
        assert_eq!(wit_escape("map"), "%map");
        // a keyword only at the OLDER end (wit-parser 0.240's require_f32_f64 compatibility arm)
        assert_eq!(wit_escape("float32"), "%float32");
        assert_eq!(wit_escape("float64"), "%float64");
        // ordinary names are untouched
        assert_eq!(wit_escape("transaction"), "transaction");
        assert_eq!(wit_escape("to-cbor-bytes"), "to-cbor-bytes");
        // `record` is a keyword and is exactly the kind of name a CDDL rule carries
        assert_eq!(wit_escape("record"), "%record");
    }

    #[test]
    fn wit_keywords_is_sorted_and_deduplicated() {
        // The list is maintained by UNION across toolchain versions, so it is read and edited by
        // hand; sortedness is what keeps a merge from silently duplicating an entry.
        let sorted: Vec<&str> = {
            let mut v = WIT_KEYWORDS.to_vec();
            v.sort_unstable();
            v.dedup();
            v
        };
        assert_eq!(sorted, WIT_KEYWORDS.to_vec());
        // `_` is deliberately absent — see the const's doc.
        assert!(!WIT_KEYWORDS.contains(&"_"));
    }

    #[test]
    fn wit_package_id_default_and_parse() {
        assert_eq!(
            WitPackageId::default_for_lib_name("cml-chain").to_string(),
            "cddl:cml-chain@0.1.0"
        );
        // --lib-name is a cargo package name, so it can carry casing the WIT identifier cannot
        assert_eq!(
            WitPackageId::default_for_lib_name("CMLChain").to_string(),
            "cddl:cml-chain@0.1.0"
        );
        assert_eq!(
            WitPackageId::parse("acme:widgets@1.2.3")
                .unwrap()
                .to_string(),
            "acme:widgets@1.2.3"
        );
        // the version is optional and defaults to the same 0.1.0 the derived default carries
        assert_eq!(
            WitPackageId::parse("acme:widgets").unwrap().to_string(),
            "acme:widgets@0.1.0"
        );
        // pre-release / build metadata rides through: WIT accepts it, we interpret none of it
        assert_eq!(
            WitPackageId::parse("acme:widgets@0.1.0-rc.1")
                .unwrap()
                .to_string(),
            "acme:widgets@0.1.0-rc.1"
        );
    }

    #[test]
    fn wit_package_id_rejects_malformed_values() {
        for bad in [
            "widgets",          // no namespace
            ":widgets",         // empty namespace
            "acme:",            // empty name
            "Acme:widgets",     // uppercase is not a WIT identifier
            "acme:wid_gets",    // underscores are not WIT word separators
            "acme:-widgets",    // leading separator
            "acme:widgets-",    // trailing separator
            "acme:wid--gets",   // empty word
            "acme:0widgets",    // digit-led identifier
            "acme:widgets@",    // empty version
            "acme:widgets@1",   // not <major>.<minor>.<patch>
            "acme:widgets@1.2", // ditto
            "acme:widgets@a.b.c",
        ] {
            assert!(
                WitPackageId::parse(bad).is_err(),
                "--wit-package {bad:?} should have been rejected"
            );
        }
    }

    /// The edge payload the real graph carries is the witness pair; the SCC computation ignores it,
    /// so these graphs carry `()` and stay readable.
    fn graph(
        edges: &[(&'static str, &'static str)],
    ) -> BTreeMap<&'static str, BTreeMap<&'static str, ()>> {
        let mut out: BTreeMap<&str, BTreeMap<&str, ()>> = BTreeMap::new();
        for (from, to) in edges {
            out.entry(from).or_default().insert(to, ());
        }
        out
    }

    #[test]
    fn strongly_connected_components_finds_mutual_reachability_not_just_two_cycles() {
        // a -> b -> c -> a, plus an acyclic tail a -> d. The three-node cycle must come back as ONE
        // component: a detector that only looked for two-node back-edges would miss it, and that is
        // exactly the shape a three-file spec produces.
        let mut components = strongly_connected_components(&graph(&[
            ("a", "b"),
            ("a", "d"),
            ("b", "c"),
            ("c", "a"),
        ]));
        components.sort();
        assert_eq!(components, vec![vec!["a", "b", "c"], vec!["d"]]);
    }

    #[test]
    fn strongly_connected_components_is_trivial_on_an_acyclic_graph() {
        let components = strongly_connected_components(&graph(&[("a", "b"), ("b", "c")]));
        assert!(
            components.iter().all(|c| c.len() == 1),
            "an acyclic graph has no non-trivial SCC, got {components:?}"
        );
    }

    #[test]
    fn strongly_connected_components_separates_two_independent_cycles() {
        // Two disjoint cycles must be two components, not one merged blob — the message names the
        // scopes in ONE cycle, and merging them would send the user looking for a reference that
        // does not exist.
        let mut components = strongly_connected_components(&graph(&[
            ("a", "b"),
            ("b", "a"),
            ("c", "d"),
            ("d", "c"),
        ]));
        components.sort();
        assert_eq!(components, vec![vec!["a", "b"], vec!["c", "d"]]);
    }
}
