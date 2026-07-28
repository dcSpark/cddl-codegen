//! Need-based narrowing for `--extern-import`: which rules of a dependency's export a consumer
//! actually needs.
//!
//! A dependency's `extern-interface/<dep>/**` export is COMPLETE — it states the dependency's whole
//! surface, once, independent of how many consumers read it. A consumer needs a SLICE of that
//! surface, and the slice is fully derivable: the consumer's own spec references are the selection.
//! Concatenating the whole export instead makes every unused export rule a name in the consumer's
//! flat CDDL namespace, so a dependency rule the consumer never mentions can still collide with a
//! rule the consumer defines itself (the shared-document namespace rejects duplicate idents before
//! any scoping applies). Computing the slice removes that class: an unused dependency rule is not
//! imported, so the consumer's own definition of that name stands.
//!
//! ## The closure (the definition a test can pin)
//!
//! For one consumer and one dependency `<dep>` whose export defines the rule set `R`:
//!
//! - `defined` = rule names defined in the consumer's own input files (its main input, including
//!   any physical `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/` stub it carries — everything already assembled
//!   when the flag-fed imports are appended).
//! - `referenced` = identifiers referenced anywhere in those same files.
//! - `needed₀ = (referenced ∖ defined) ∩ names(R)`.
//! - `closure` = the least fixpoint of `needed₀` plus, for every rule already in it, the
//!   identifiers its EXPORT BODY references that are in `names(R)`.
//!
//! Opaque rows (`_CDDL_CODEGEN_EXTERN_TYPE_` / `_CDDL_CODEGEN_RAW_BYTES_TYPE_`) reference nothing,
//! so only transparent aliases, value enums, named collections and plain-group bodies contribute
//! closure edges — which is why the closure is shallow in practice.
//!
//! ## The two directions are asymmetric
//!
//! Computing too LITTLE fails loudly and immediately: the pipeline's checked parse validates that
//! every referenced name is defined, so a missing rule aborts generation with the
//! staleness-augmented "undefined reference" error. Computing too MUCH is silent — extra rules are
//! output-neutral right up until one collides, which reintroduces exactly the failure class the
//! narrowing exists to remove. That asymmetry is why [`needed_closure`] is a pure function over
//! plain sets rather than a filter inside the concatenation loop: only a pure function can be
//! pinned with EXACT-SET assertions, which is the only test shape that catches over-inclusion.
//!
//! ## Two hard errors
//!
//! Both are cheap set operations performed BEFORE the concatenated parse, so they replace the bare
//! `rule "X" is already defined` abort for their cases with a message that names the chain:
//!
//! - **Shadowing** — a closure member the consumer also defines. It is necessarily at depth ≥ 1
//!   (depth 0 subtracts `defined` by construction), so the message names the imported rule that
//!   pulled it in.
//! - **Ambiguity** — a name needed from two dependencies' exports at once.

use std::collections::{BTreeMap, BTreeSet};

/// One rule of a dependency's export file: its CDDL name, the byte span of its text within that
/// file, and the identifiers its body references.
///
/// The span starts where the AST says the rule starts and runs to where the NEXT rule starts (the
/// end of the file for the last one) — deliberately not to the AST's own end offset, which excludes
/// a rule's trailing comment for a GROUP rule while including it for a type rule. That comment is
/// load-bearing: every export row carries its `@rust_name` pin there, so slicing a group row at its
/// AST end drops the pin AND the newline separating it from the next row. Bounding each rule by its
/// successor's start makes the partition total — prefix plus every rule's slice is the whole file,
/// byte for byte — so a rule's annotations travel with it however the writer lays them out, and no
/// byte can be silently lost between two rows.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportRule {
    pub name: String,
    pub span: (usize, usize),
    pub refs: BTreeSet<String>,
}

/// One parsed export file: the byte offset at which its first rule starts, and its rules in source
/// order.
///
/// Everything before `prefix_end` is comment text — the versioned seam header and the sorted
/// `; unexported:` records — and is emitted verbatim ahead of whichever rules are selected, so a
/// file whose every rule is selected reproduces its original bytes exactly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportFile {
    pub prefix_end: usize,
    pub rules: Vec<ExportRule>,
}

impl ExportFile {
    /// Every rule name this file defines.
    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.rules.iter().map(|r| r.name.as_str())
    }
}

/// The consumer's own rule surface: what its input files define, and what they reference.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ConsumerSurface {
    pub defined: BTreeSet<String>,
    pub referenced: BTreeSet<String>,
}

/// Parse ONE export file standalone.
///
/// Export files are reference-closed by construction (the writer's own reference-closure excludes
/// any rule whose body names something the export does not carry), but they are not
/// reference-COMPLETE on their own: an opaque row's body is the `_CDDL_CODEGEN_EXTERN_TYPE_` marker,
/// whose placeholder definition is appended only to the fully assembled document. So this uses the
/// fork's UNCHECKED parse entry — syntax only, no "every referenced name is defined" validation —
/// which is exactly the tolerance an export file needs.
pub fn parse_export_file(content: &str) -> Result<ExportFile, String> {
    let cddl = cddl::parser::cddl_from_str(content, false)?;
    let mut rules = cddl
        .rules
        .iter()
        .map(|rule| {
            let start = match rule {
                cddl::ast::Rule::Type { span, .. } => span.0,
                cddl::ast::Rule::Group { span, .. } => span.0,
            };
            let (_ident, refs) = crate::dep_graph::find_references(rule);
            ExportRule {
                name: rule.name(),
                // Closed below, once the successor's start is known.
                span: (start, content.len()),
                refs: refs.into_iter().map(|i| i.ident.to_owned()).collect(),
            }
        })
        .collect::<Vec<_>>();
    rules.sort_by_key(|r| r.span.0);
    for i in 0..rules.len().saturating_sub(1) {
        rules[i].span.1 = rules[i + 1].span.0;
    }
    let prefix_end = rules.first().map_or(content.len(), |r| r.span.0);
    Ok(ExportFile { prefix_end, rules })
}

/// Scan the consumer's own (already assembled) input content for what it defines and what it
/// references, or `None` when that content does not parse.
///
/// Uses the same unchecked entry as [`parse_export_file`], for the opposite reason: consumer content
/// is FULL of dangling references by design — every name it means to take from a dependency is
/// undefined until the imports are appended.
///
/// `None` is not a failure path: broken consumer content is going to fail the pipeline's own checked
/// parse a moment later with the canonical message, and the caller answers `None` by importing
/// every rule — today's behaviour — so the diagnostic the user sees is unchanged.
pub fn scan_consumer(content: &str) -> Option<ConsumerSurface> {
    let cddl = cddl::parser::cddl_from_str(content, false).ok()?;
    let mut surface = ConsumerSurface::default();
    for rule in cddl.rules.iter() {
        let (ident, refs) = crate::dep_graph::find_references(rule);
        surface.defined.insert(ident.ident.to_owned());
        surface
            .referenced
            .extend(refs.into_iter().map(|i| i.ident.to_owned()));
    }
    Some(surface)
}

/// The needed closure, per dependency — the pure heart of the feature.
///
/// `exports` maps each dependency key to that dependency's whole export as `rule name → the export
/// rule names its body references` (references to anything outside the export — prelude names, the
/// extern markers — are simply absent from the value sets, or are filtered here).
///
/// Returns each dependency's selected rule names, or the message of the first hard error. Both
/// errors and the traversal are deterministic: dependencies are visited in `BTreeMap` order and each
/// closure expands its lowest unprocessed member first.
pub fn needed_closure(
    surface: &ConsumerSurface,
    exports: &BTreeMap<String, BTreeMap<String, BTreeSet<String>>>,
) -> Result<BTreeMap<String, BTreeSet<String>>, String> {
    let mut selected_by_dep: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (dep, rules) in exports {
        // Depth 0: what this spec references, does not define itself, and this export carries.
        let mut selected: BTreeSet<String> = surface
            .referenced
            .difference(&surface.defined)
            .filter(|name| rules.contains_key(*name))
            .cloned()
            .collect();
        let mut processed: BTreeSet<String> = BTreeSet::new();
        while let Some(current) = selected.difference(&processed).next().cloned() {
            processed.insert(current.clone());
            let Some(refs) = rules.get(&current) else {
                continue;
            };
            for reference in refs {
                // A reference to something the export does not define is a prelude name or an
                // extern marker — it resolves without an imported rule.
                if !rules.contains_key(reference) || selected.contains(reference) {
                    continue;
                }
                if surface.defined.contains(reference) {
                    return Err(shadowing_error(dep, &current, reference));
                }
                selected.insert(reference.clone());
            }
        }
        selected_by_dep.insert(dep.clone(), selected);
    }
    // Ambiguity is only meaningful across dependencies, so it is checked once the closures are
    // complete rather than while any one of them is being built.
    let mut owners: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (dep, selected) in &selected_by_dep {
        for name in selected {
            owners.entry(name.as_str()).or_default().push(dep.as_str());
        }
    }
    for (name, deps) in &owners {
        if deps.len() > 1 {
            return Err(ambiguity_error(name, deps));
        }
    }
    Ok(selected_by_dep)
}

/// The shadowing hard error: an imported rule needs `shadowed`, and the consumer defines that name
/// too. `puller` is the rule that reached it, so the message names the chain rather than just the
/// collision.
fn shadowing_error(dep: &str, puller: &str, shadowed: &str) -> String {
    format!(
        "--extern-import {dep}: this spec needs rule `{puller}` from the dependency's export, whose \
         body references `{shadowed}` — and this spec defines `{shadowed}` itself. A rule name is \
         defined exactly once in the assembled document, so the dependency's `{shadowed}` cannot be \
         imported alongside yours. Either rename this spec's `{shadowed}`, or hand-own the \
         dependency's type here: declare `{puller}` in this spec as \
         `_CDDL_CODEGEN_EXTERN_TYPE_` and re-export the dependency's Rust type by hand, so nothing \
         is taken from the export for it."
    )
}

/// The ambiguity hard error: one name is needed from two dependencies' exports at once.
fn ambiguity_error(name: &str, deps: &[&str]) -> String {
    format!(
        "--extern-import: rule `{name}` is needed from more than one dependency's export ({}). A \
         rule name is defined exactly once in the assembled document, so two exports cannot both \
         contribute it, and nothing here can decide which one this spec means. Rename the rule in \
         one dependency's spec, or hand-own the type in this spec as `_CDDL_CODEGEN_EXTERN_TYPE_` \
         so neither export is consulted for it.",
        deps.join(", ")
    )
}

/// The diagnostic-only note for a declared dependency nothing in the spec reaches. Not an error: a
/// dependency edge that is unused today is a normal state of a workspace being wired up, and the
/// same run's other flags for that dependency (`--extern-wasm-crate`, `--workspace-dep`) stay
/// harmlessly inert.
pub fn unused_dependency_note(dep: &str, import_path: &str) -> String {
    format!(
        "warning: --extern-import {dep}={import_path} is declared, but this spec references nothing \
         in that export — no rule is imported from it."
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn set(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| (*s).to_owned()).collect()
    }

    fn surface(defined: &[&str], referenced: &[&str]) -> ConsumerSurface {
        ConsumerSurface {
            defined: set(defined),
            referenced: set(referenced),
        }
    }

    fn export(rules: &[(&str, &[&str])]) -> BTreeMap<String, BTreeSet<String>> {
        rules
            .iter()
            .map(|(name, refs)| ((*name).to_owned(), set(refs)))
            .collect()
    }

    fn exports(
        deps: &[(&str, BTreeMap<String, BTreeSet<String>>)],
    ) -> BTreeMap<String, BTreeMap<String, BTreeSet<String>>> {
        deps.iter()
            .map(|(dep, rules)| ((*dep).to_owned(), rules.clone()))
            .collect()
    }

    fn selected(
        surface: &ConsumerSurface,
        exports: &BTreeMap<String, BTreeMap<String, BTreeSet<String>>>,
        dep: &str,
    ) -> BTreeSet<String> {
        needed_closure(surface, exports).expect("closure must succeed")[dep].clone()
    }

    /// Depth 0 only: an opaque row references nothing, so a consumer that names two of a
    /// three-rule export takes EXACTLY those two. The exact-set assertion is the point — an
    /// over-computed closure is the silent failure direction, and only an equality pin sees it.
    #[test]
    fn closure_depth_zero_takes_exactly_what_is_referenced() {
        let e = exports(&[(
            "dep",
            export(&[("foo", &[]), ("coin", &[]), ("unused", &[])]),
        )]);
        let s = surface(&["thing"], &["foo", "coin", "uint"]);
        assert_eq!(selected(&s, &e, "dep"), set(&["coin", "foo"]));
    }

    /// A transparent alias chain contributes closure edges: referencing `a` where the export says
    /// `a = b` and `b = c` takes all three, and still nothing else.
    #[test]
    fn closure_follows_transparent_alias_chain() {
        let e = exports(&[(
            "dep",
            export(&[
                ("a", &["b"]),
                ("b", &["c"]),
                ("c", &[]),
                ("d", &["e"]),
                ("e", &[]),
            ]),
        )]);
        let s = surface(&["own"], &["a"]);
        assert_eq!(selected(&s, &e, "dep"), set(&["a", "b", "c"]));
    }

    /// A plain-group body references every member type, so splicing one group pulls in the rules
    /// its fields name — the shape that grows CML's depth-0 set into its closure.
    #[test]
    fn closure_follows_group_body_edges() {
        let e = exports(&[(
            "dep",
            export(&[
                ("header_body", &["hash", "slot"]),
                ("hash", &[]),
                ("slot", &[]),
                ("unrelated", &["hash"]),
            ]),
        )]);
        let s = surface(&["block"], &["header_body"]);
        assert_eq!(
            selected(&s, &e, "dep"),
            set(&["hash", "header_body", "slot"])
        );
    }

    /// A generic reference contributes BOTH the generic base and its argument (the AST walk pushes
    /// the argument idents alongside the base), and a reference to something the export does not
    /// define — a prelude name, an extern marker — contributes nothing.
    #[test]
    fn closure_covers_generic_args_and_ignores_non_export_names() {
        let e = exports(&[(
            "dep",
            export(&[
                (
                    "holder",
                    &["ext_set", "coin", "_CDDL_CODEGEN_EXTERN_TYPE_", "uint"],
                ),
                ("ext_set", &[]),
                ("coin", &[]),
            ]),
        )]);
        let s = surface(&["own"], &["holder"]);
        assert_eq!(selected(&s, &e, "dep"), set(&["coin", "ext_set", "holder"]));
    }

    /// Two dependencies close independently: each subtracts the SAME `defined` set and intersects
    /// only its OWN names, so one dependency's rules never leak into the other's selection.
    #[test]
    fn closure_is_independent_per_dependency() {
        let e = exports(&[
            ("alpha", export(&[("a", &["shared_a"]), ("shared_a", &[])])),
            ("beta", export(&[("b", &[]), ("a", &[])])),
        ]);
        let s = surface(&["own"], &["a", "b"]);
        // `a` exists in BOTH exports but is needed at depth 0 from both — that is the ambiguity
        // case, so probe independence with a spec that needs only one of them.
        let s_one = surface(&["own"], &["b"]);
        assert_eq!(selected(&s_one, &e, "alpha"), set(&[]));
        assert_eq!(selected(&s_one, &e, "beta"), set(&["b"]));
        assert!(needed_closure(&s, &e).is_err(), "`a` is needed from both");
    }

    /// A name the consumer defines itself is subtracted at depth 0 — it is never imported, and it
    /// is never an error. This is the whole point of the feature: the consumer's own definition
    /// shadows the dependency's unused rule of the same name.
    #[test]
    fn own_definition_shadows_an_unneeded_export_rule() {
        let e = exports(&[("dep", export(&[("block", &[]), ("thing", &[])]))]);
        let s = surface(&["block"], &["thing", "block"]);
        assert_eq!(selected(&s, &e, "dep"), set(&["thing"]));
    }

    /// A DEEP collision is the honest remainder: the consumer needs `a`, whose export body needs
    /// `b`, and the consumer defines `b`. Nothing can silently resolve that, so it is a hard error
    /// naming both the puller and the shadowed rule.
    #[test]
    fn deep_collision_is_a_named_hard_error() {
        let e = exports(&[("dep", export(&[("a", &["b"]), ("b", &[])]))]);
        let s = surface(&["b"], &["a"]);
        let err = needed_closure(&s, &e).expect_err("a deep collision must be refused");
        assert!(err.contains("`a`") && err.contains("`b`"), "{err}");
        assert!(err.contains("dep"), "must name the dependency: {err}");
    }

    /// One name needed from two exports names both dependencies.
    #[test]
    fn ambiguous_name_names_both_dependencies() {
        let e = exports(&[
            ("alpha", export(&[("shared", &[])])),
            ("beta", export(&[("shared", &[])])),
        ]);
        let s = surface(&["own"], &["shared"]);
        let err = needed_closure(&s, &e).expect_err("an ambiguous name must be refused");
        assert!(err.contains("alpha") && err.contains("beta"), "{err}");
        assert!(err.contains("`shared`"), "{err}");
    }

    /// A rule of the export whose name the consumer never reaches at any depth stays out even when
    /// another export rule names it — reachability is from the consumer, not from the export.
    #[test]
    fn unreachable_export_rules_stay_out() {
        let e = exports(&[(
            "dep",
            export(&[
                ("wanted", &[]),
                ("other", &["wanted", "deep"]),
                ("deep", &[]),
            ]),
        )]);
        let s = surface(&["own"], &["wanted"]);
        assert_eq!(selected(&s, &e, "dep"), set(&["wanted"]));
    }

    /// The file-level reader: each rule's slice carries its own trailing `@rust_name` pin and its
    /// newline, the header and the `; unexported:` records land in the prefix, and re-emitting every
    /// rule after the prefix reproduces the file byte for byte.
    ///
    /// The GROUP row (`operational_cert = ( … )`, the CML plain-group shape) is the load-bearing case
    /// and is deliberately not last: a group rule's AST end offset stops before its trailing comment,
    /// so bounding a rule by that offset would silently drop the pin and weld the next rule onto it.
    /// The partition is by successor start for exactly this reason, and the byte-identity assertion
    /// below is what proves no row loses its annotations.
    #[test]
    fn export_file_spans_partition_the_file() {
        let content = "; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\n\
                       ; unexported: pool_params — a reason\n\
                       coin = uint ; @rust_name Coin\n\
                       operational_cert = (kes_period: uint, sigma: coin) ; @rust_name OperationalCert\n\
                       foo = [c: coin] ; @rust_name Foo\n";
        let parsed = parse_export_file(content).expect("an export file must parse standalone");
        assert_eq!(
            parsed
                .rules
                .iter()
                .map(|r| r.name.as_str())
                .collect::<Vec<_>>(),
            vec!["coin", "operational_cert", "foo"]
        );
        // Prelude names ride in `refs` verbatim — the closure filters them by intersecting with the
        // export's own rule names, so the reader stays a faithful record of what the body says.
        assert_eq!(parsed.rules[1].refs, set(&["coin", "uint"]));
        assert_eq!(parsed.rules[2].refs, set(&["coin"]));
        assert_eq!(
            &content[parsed.rules[0].span.0..parsed.rules[0].span.1],
            "coin = uint ; @rust_name Coin\n"
        );
        assert_eq!(
            &content[parsed.rules[1].span.0..parsed.rules[1].span.1],
            "operational_cert = (kes_period: uint, sigma: coin) ; @rust_name OperationalCert\n",
            "a group row must carry its own trailing pin, not stop at the AST's end offset"
        );
        let mut rebuilt = content[..parsed.prefix_end].to_string();
        for rule in &parsed.rules {
            rebuilt.push_str(&content[rule.span.0..rule.span.1]);
        }
        assert_eq!(rebuilt, content, "full selection must reproduce the file");
    }

    /// A header-only export file (a scope carrying no exported rule) parses to zero rules rather
    /// than failing — the root `mod.cddl` of a dependency with an empty surface is exactly this.
    #[test]
    fn header_only_export_file_has_no_rules() {
        let parsed = parse_export_file("; _CDDL_CODEGEN_EXTERN_INTERFACE_ v1\n").unwrap();
        assert!(parsed.rules.is_empty());
    }

    /// The consumer scan tolerates the dangling references that are the whole reason the consumer
    /// has an import flag, and records both halves of the surface.
    #[test]
    fn consumer_scan_tolerates_dangling_references() {
        let s = scan_consumer("thing = [f: foo, c: coin]\nblock = _CDDL_CODEGEN_EXTERN_TYPE_\n")
            .expect("consumer content must scan under the unchecked entry");
        assert_eq!(s.defined, set(&["block", "thing"]));
        assert_eq!(
            s.referenced,
            set(&["_CDDL_CODEGEN_EXTERN_TYPE_", "coin", "foo"])
        );
    }

    /// Unparseable consumer content yields `None` (the caller then imports everything and lets the
    /// pipeline's own checked parse report the real error).
    #[test]
    fn unparseable_consumer_content_scans_to_none() {
        assert!(scan_consumer("thing = [[[").is_none());
    }
}
