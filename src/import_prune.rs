//! Usage-derived pruning of blindly-emitted type imports.
//!
//! Several emission sites in `generation/` push a fixed set of collection-type imports into every
//! generated struct file unconditionally (or gated only on spec-global facts), so files that never
//! reference the type still import it — the `warning: unused import` walls consumers see. Rather
//! than teach each of ~30 emission sites to predict whether it will need `BTreeMap` (an IR-side
//! prediction that must mirror every local decision of a ~13k-line generator and silently drifts),
//! the emission sites stay dumb (they may over-import) and this single post-pass derives the final
//! import set from the rendered token streams: an import is justified iff the generated code
//! references the imported name. [`prune_generated_files`] is the entry point — the driver in
//! `generated_files` calls it once over the whole file map.
//!
//! **Module family via the super-glob edge graph.** The unit of analysis for a file F is F plus the
//! descendants that can actually CONSUME its private imports. A child re-exports the parent's
//! *private* imports only through `use super::*;` (`serialization.rs` reaches the `BTreeMap` it uses
//! through `use super::*;` from `mod.rs`, even though `mod.rs`'s own body never names `BTreeMap`), so
//! pruning F on its own idents alone breaks the crate (E0433 at the child). Privacy bounds who can do
//! this: a private `use` binding in module M is nameable only from M and M's descendants — a glob
//! from a NON-descendant imports only `pub` items, never M's private imports — and a descendant
//! reaches it EXCLUSIVELY through an unbroken chain of `use super::*;` edges (explicit `super::X`
//! paths are not emitted by the generator; verified). So F's protectors are exactly the descendants
//! D linked to F by such a chain ([`reachable_via_super`]); a sub-scope `serialization.rs` whose own
//! scope `mod.rs` does NOT re-glob the root can never consume the root's imports even though it names
//! the same idents. F is protected by its own used idents plus each super-reachable D's used idents,
//! MINUS the idents D resolves through a nearer binding of its own. Rust resolves to the NEAREST
//! binding, so three disqualifiers drop D as a protector of ident X:
//!   1. **Direct import** — D carries `use …::X;` of its own; D's uses of `X` resolve to that, never
//!      to F's copy reached through `use super::*;` (the `cbor_encodings.rs` shape).
//!   2. **Target file** — D IS the file of the module F imports X FROM (`use crate::<segs>::X;` →
//!      module `crate::<segs>`, whose file is `<segs>.rs`/`<segs>/mod.rs`). That file DEFINES X (the
//!      `serialization.rs` static prelude, concatenated LATER — invisible to this generated-only
//!      pass, so the target is read from the import PATH), so D resolves X locally. Restricted to the
//!      target FILE, not everything under it: a deeper descendant reaches the target's items only via
//!      its own `super::*` chain (the un-modeled intermediate case, kept conservative), and a
//!      crate-ROOT target (`use crate::generated::X;`) must not disqualify every file merely nested
//!      under `generated/`.
//!   3. **Source glob** — D carries a `use M::*;` of the SAME module F imports X from, so D resolves X
//!      through its own glob (the `--common-import-override` shape where the sub-scope
//!      `serialization.rs` globs `<dep>::serialization::*` and the definition is external, so
//!      disqualifier 2 can't see it).
//!
//! The `/src/` crate split (`rust` / `wasm` / `wasm/json-gen` are separate crates) stays the outer
//! boundary; a leaf file (no super-reachable descendants) is pruned purely on its own idents.
//!
//! **Glob pruning.** Two blindly-pushed private globs are themselves prunable when provably unused,
//! each against a fully-enumerable universe (conservative-keep when it can't be enumerated):
//!   - `use super::*;` — universe = the PARENT module's bound names ([`bound_names`]: its item defs,
//!     its `use`-bound leaves, and its own globs' exports, recursively). Removed from a file with no
//!     super-reachable descendant whose body names no parent-bound name it doesn't itself bind/define
//!     ([`super_glob_needed`]). Drops the `cbor_encodings.rs` `super::*` that only needs its own
//!     direct encoding imports; KEPT where an encoding struct is keyed by a parent-scope generated
//!     type (`BTreeMap<Ikey, StringEncoding>`).
//!   - `use <path>::error::*;` — universe = the fixed [`ERROR_MODULE_EXPORTS`]. Removed when neither F
//!     nor a super-reachable descendant demands an error export it doesn't resolve locally
//!     ([`error_glob_needed`]); the sub-scope `serialization.rs` carries its OWN `error::*`, so it
//!     resolves error names through that (source-glob), not the parent copy.
//!
//! **Documented-conservative residue.** The disqualifiers can still leave a protector standing and
//! KEEP a would-be-prunable import — never remove one a consumer needs, so any imprecision is
//! warning-severity, never a compile error: an INTERMEDIATE module M between F and a deeper
//! descendant D that consumes F's copy for everything at/below M, yet the per-descendant rule still
//! counts D — which reaches X through M, not F — as a protector. Watched by the
//! generated-code unused-import scan in the `feature_corpus_compiles` gate (src/tests), which fails
//! on ANY `unused import` rustc warning in the generated crates (minus a documented trait residue).
//!
//! **Soundness boundary — the name-scan candidate set.** Ident-scanning can prove a *concrete type*
//! unused: a type can only be used by naming it, so "ident absent from the module family" ⇒ unused.
//! That implication does NOT hold for traits (`use std::io::Write;` is exercised by `w.write_all(..)`
//! — the ident `Write` never appears). So name-scan pruning is restricted to concrete-type / macro
//! candidates: the built-in [`ALLOWLIST`] (collection helpers + `--preserve-encodings` encoding
//! enums) plus the per-run [`PruneConfig::extra_candidates`] — the wasm prelude names (`JsError` /
//! `JsValue` concrete types, the `wasm_bindgen` attribute macro exercised only via `#[wasm_bindgen]`,
//! whose ident the scan sees), the `--wasm-*-macro` leaf names (each exercised only via `name!(…)`),
//! and the cross-scope generator-minted type idents `scope_references` over-imports. Everything
//! else — notably the `cbor_event::se::Serialize` TRAIT the serialization prelude imports — is kept
//! untouched (a documented residue). Globs are handled by the separate glob-prune above, never by
//! name-scan.
//!
//! **Soundness boundary — path-tail idents.** The used-ident scan counts a bare ident as a use but
//! must NOT count an ident that is a PATH TAIL — the segment after a `::` (a module path, an
//! associated item, an enum variant, a macro-path segment). Such an ident is always resolved
//! relative to the preceding path segment, never through the local module namespace, so it can never
//! consume a `use` binding — counting it would wrongly PROTECT an import or glob that the code does
//! not actually reference (the residual behind the sidecar `use super::*;` that survived because the
//! body's `<dep>::sub::module::X` path made the scan count `sub`, which the parent module binds via
//! `pub mod sub;`). [`collect_idents_in_tokens`] therefore skips an ident immediately preceded by
//! `::` (two adjacent joint `Punct(':')`), keeping the direction of the failure asymmetry: a LONE
//! `:` (a struct field type, `let x: BTreeMap<…>`) is NOT a path separator and its following ident
//! still counts — over-skipping there would un-protect a load-bearing import (over-prune = consumer
//! compile error; under-prune = warning; when in doubt, count).
//!
//! **Second rule — re-export-only files (file-shape scoped).** An extern-only CDDL scope generates
//! a `mod.rs` containing nothing but extern re-export glue (`pub use crate::Address;`). The
//! unconditional common-import push still adds `error::*` and (under `--preserve-encodings`) the
//! encoding enums into it, none of which the allowlist rule above can touch (`error::*` is a glob;
//! before Deliverable-2's edition bump `TryFrom` was a trait). For a file whose *shape* proves
//! nothing local can consume any import, [`prune_generated_files`] applies a stronger rule that
//! removes ALL private `use` items (traits, globs, macros included — allowlist irrelevant). A file
//! F **qualifies** (see [`is_reexport_only_file`] plus the driver's descendant check) when: (a) it
//! is a prunable generated `.rs`; (b) it parses and every top-level `syn::Item` is `Item::Use`
//! (comments are trivia, not items — `Item::Mod` even bodyless, `Item::Macro`, verbatim, any code
//! item disqualifies); (c) the file map holds no other `.rs` under F's module dir in the same crate
//! (no descendant modules — regardless of whether that descendant parses); and (d) every NON-private
//! `use` is a plain `crate::`-anchored path chain ending in `Name`/`Rename` (a glob or group
//! anywhere in a non-private use disqualifies — conservatively).
//!
//! Soundness of the second rule: a private `use` binding is consumable only by (1) code in the same
//! file — none exists (condition b: only `use` items), or (2) descendant modules via `use super::*;`
//! chains — none exist (condition c). A glob from a NON-descendant (`use crate::generated::x::*;`
//! from a sibling) imports only `pub` items, never private bindings. So nothing can consume the
//! private imports; wholesale removal is total and sound. Condition (d) guards the one escape hatch:
//! 2018+ uniform paths let a relative `pub use self::Foo;` resolve *through* a private glob we would
//! be deleting; a `crate::`-anchored `pub use` cannot (these files are never the crate root — a
//! generated root carries `mod` declarations, excluded by (b)). Removal is all-or-nothing, so import
//! chaining (`use a::B; use B::c;`) is moot. This preserves the pass's failure asymmetry
//! (conservative-keep: parse failure, any descendant, a non-anchored `pub use`, or any code item all
//! leave the file untouched) and future-proofs the wasm side — if the wasm glue/blanket-import
//! ordering ever flips, the otherwise-unprunable `wasm_bindgen` macro import would be removed here.
//!
//! **Only private imports are candidates.** A `pub use` is API surface: a downstream crate may
//! import the re-exported name, so no amount of in-crate usage analysis can justify removing it.
//! Any `use` item whose visibility is not inherited (private) is skipped entirely. This also
//! closes the privacy argument above: only private uses are pruned, and only descendants can
//! consume private uses.
//!
//! **Failure asymmetry.** A wrongly-removed import breaks the generated crate (compile error at the
//! consumer); a wrongly-kept one is a warning. So the pass is conservative-keep everywhere: a file
//! that fails to parse is returned unchanged (and poisons every file it could be protecting — its
//! ancestors — so none of them are pruned either), and any ident found anywhere outside a private
//! `use` item — field types, fn signatures, expressions, the token streams of macro invocations
//! and attributes, and `pub use` re-export paths — keeps its import family-wide.

use std::borrow::Cow;
use std::collections::{BTreeMap, HashSet};

use proc_macro2::{Spacing, TokenStream, TokenTree};
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{Item, ItemUse, UseTree};

/// The built-in concrete-type import names this pass may remove by name-scan, always available (the
/// per-run [`PruneConfig::extra_candidates`] add to these). These are exactly the blindly-pushed
/// types the emission sites in `generation/` add unconditionally (or gated only on spec-global
/// facts): the six collection helpers plus the three `--preserve-encodings` encoding enums. Every
/// entry must be a concrete type (never a trait/macro/glob), or the "ident absent from the module
/// family ⇒ unused" implication that makes name-scanning sound breaks — the three encoding enums are
/// concrete enums (`static/serialization_preserve.rs`) only ever consumed by being named.
pub(crate) const ALLOWLIST: &[&str] = &[
    "BTreeMap",
    "OrderedHashMap",
    "NonEmptyVec",
    "NonEmptyMap",
    "OrderedSet",
    "NonEmptyOrderedSet",
    "PairMap",
    "NonEmptyPairMap",
    "LenEncoding",
    "StringEncoding",
    "TagPresenceEncoding",
];

/// The concrete-type names the static `error` module (`static/error.rs`) binds into a module's
/// namespace. A `use <common>::error::*;` glob supplies EXACTLY these, so this is the complete
/// universe for deciding whether such a glob is unused (see [`error_glob_needed`]). Held here as the
/// single owner and drift-guarded against `static/error.rs` by `error_exports_match_static_source`
/// (a stale entry there is a compile-time-caught test failure, not a silent under-prune). In
/// `--common-import-override` mode the glob targets the dependency crate's `error` module, which is
/// this same runtime copied into the dependency — the contract that keeps the universe complete.
pub(crate) const ERROR_MODULE_EXPORTS: &[&str] = &["Key", "DeserializeFailure", "DeserializeError"];

/// Per-run configuration for [`prune_generated_files`]: the name-scan-prunable idents BEYOND the
/// built-in [`ALLOWLIST`]. Everything here is a CONCRETE type, an attribute macro, or a
/// generator-minted type ident — never a trait — so the "ident absent from the module family ⇒
/// unused" implication that makes name-scanning sound holds for each (a trait like `cbor_event`'s
/// `Serialize` is exercised by a method call whose ident never appears, so it is NEVER added here;
/// it stays a documented residue). A pure function of CLI config + the finalized IR, so the prune
/// stays deterministic.
#[derive(Default)]
pub(crate) struct PruneConfig {
    /// Extra idents the prune may remove by name-scan, unioned onto [`ALLOWLIST`]:
    ///   - the wasm prelude names `JsError` / `JsValue` (concrete types) and the `wasm_bindgen`
    ///     attribute macro (exercised only via `#[wasm_bindgen]`, whose ident the scan sees);
    ///   - the `--wasm-*-macro` leaf names (each exercised only via `name!(…)`, whose ident the scan
    ///     sees) — the configured macros MUST be written fully-qualified (they must not assume
    ///     `JsError`/`JsValue`/`wasm_bindgen` in scope), the contract documented next to those flags;
    ///   - cross-scope generator-minted type idents pushed by `add_imports_from_scope_refs`, which
    ///     a referencing module over-imports (the `scope_references` set is an over-approximation).
    pub extra_candidates: std::collections::BTreeSet<String>,
}

fn is_candidate(ident: &str, config: &PruneConfig) -> bool {
    ALLOWLIST.contains(&ident) || config.extra_candidates.contains(ident)
}

/// Test-only: the driver uses [`is_candidate`] (allowlist ∪ per-run extras). Kept for the
/// self-contained unit-test path that exercises the built-in allowlist alone.
#[cfg(test)]
fn is_allowlisted(ident: &str) -> bool {
    ALLOWLIST.contains(&ident)
}

/// Prune allowlisted imports from `source` using an EXTERNALLY-supplied `used` ident set. `used`
/// must be the PROTECTED set for this file: the union of [`collect_used_idents_from_source`] over
/// the file itself and every strict path-descendant module in the same crate (see the module docs —
/// a descendant's `use super::*;` chain can consume this file's private imports, so passing only
/// this file's own idents is unsound for any file that has descendant modules). The driver
/// [`prune_generated_files`] computes that set; leaf files get exactly their own idents.
///
/// Only PRIVATE `use` items (inherited visibility) are candidates: a `pub use` is API surface a
/// downstream crate may import, which no in-crate analysis can rule out. Conservative-keep on
/// everything else: a parse failure returns `source` byte-identical; globs, renames, and
/// non-allowlisted names are never touched.
///
/// Re-emission is targeted span-based splicing (never whole-file token re-printing, which would
/// drop comments and break the comment-preservation overlay): only the byte range of each modified
/// `use` item is replaced; every comment and every other byte survives untouched. A whole-item
/// deletion also consumes the line's leading indentation and trailing newline when the item was
/// alone on its line(s), so no blank-line scar is left behind (rustfmt does NOT collapse those).
/// rustfmt runs after this pass and normalizes the splice's spacing.
#[cfg(test)]
pub(crate) fn prune_unused_type_imports_with_used<'a>(
    source: &'a str,
    used: &HashSet<String>,
) -> Cow<'a, str> {
    // Allowlist-only behaviour (unit-test path): drop allowlisted leaves absent from `used`; no glob
    // is ever removed.
    splice_private_uses(
        source,
        false,
        &|ident| is_allowlisted(ident) && !used.contains(ident),
        &HashSet::new(),
    )
}

/// Shared span-splicing edit loop for every prune rule. Iterates top-level PRIVATE `use` items;
/// non-`use` items and `pub use` re-exports are never edited. Re-emission is targeted byte-range
/// splicing (never whole-file token re-printing), so comments survive byte-for-byte.
///
/// `remove_all` drops every private `use` item wholesale (the re-export-only file shape). Otherwise
/// each item is filtered by [`filter_use_tree`]: a `Name` leaf is dropped when `remove_named`
/// returns true for it, and a `Glob` leaf is dropped when its full `::`-joined module path is in
/// `remove_globs` (renames are always kept — the alias target can't be name-scanned).
fn splice_private_uses<'a>(
    source: &'a str,
    remove_all: bool,
    remove_named: &dyn Fn(&str) -> bool,
    remove_globs: &HashSet<String>,
) -> Cow<'a, str> {
    let file = match syn::parse_file(source) {
        Ok(file) => file,
        // Conservative-keep: unparseable input (a generator bug rustfmt will also reject loudly)
        // must not be silently mangled here.
        Err(_) => return Cow::Borrowed(source),
    };

    let line_starts = line_start_offsets(source);
    let mut edits: Vec<(usize, usize, Option<String>)> = Vec::new();
    for item in &file.items {
        let Item::Use(use_item) = item else {
            continue;
        };
        if !matches!(use_item.vis, syn::Visibility::Inherited) {
            // `pub use` (any non-private visibility) is exported API surface — a downstream crate
            // may import the name, so usage analysis inside this crate can never justify removal.
            continue;
        }
        let drop_whole = |edits: &mut Vec<(usize, usize, Option<String>)>| {
            // Drop the whole item, including the line it occupied — plain span deletion would leave a
            // blank-line scar rustfmt does not collapse.
            let (start, end) = item_byte_range(use_item, &line_starts, source);
            let (start, end) = expand_to_whole_line(source, start, end);
            edits.push((start, end, None));
        };
        if remove_all {
            drop_whole(&mut edits);
            continue;
        }
        match filter_use_tree(&use_item.tree, &mut Vec::new(), remove_named, remove_globs) {
            Some(pruned_tree) => {
                if !trees_equal(&use_item.tree, &pruned_tree) {
                    let mut new_item = use_item.clone();
                    new_item.tree = pruned_tree;
                    let (start, end) = item_byte_range(use_item, &line_starts, source);
                    edits.push((start, end, Some(new_item.to_token_stream().to_string())));
                }
            }
            None => drop_whole(&mut edits),
        }
    }

    if edits.is_empty() {
        return Cow::Borrowed(source);
    }

    // Apply from the end so earlier byte offsets stay valid. Edits are non-overlapping top-level
    // items, so a simple descending splice is correct.
    edits.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));
    let mut out = source.to_owned();
    for (start, end, replacement) in edits {
        match replacement {
            Some(text) => out.replace_range(start..end, &text),
            None => out.replace_range(start..end, ""),
        }
    }
    Cow::Owned(out)
}

/// Single-file convenience: prune using only this file's own idents as the used set — the leaf-file
/// case (no descendant modules). The driver [`prune_generated_files`] never calls this; it always
/// computes the descendant-protected set. Kept for unit tests that exercise a self-contained
/// snippet.
#[cfg(test)]
pub(crate) fn prune_unused_type_imports(source: &str) -> Cow<'_, str> {
    match collect_used_idents_from_source(source) {
        Some(used) => prune_unused_type_imports_with_used(source, &used),
        None => Cow::Borrowed(source),
    }
}

/// Import prune over the full generated-file map, at module-family precision. Two removals per file
/// F (a `.rs` under a `/generated/` dir): named-candidate leaves ([`PruneConfig`] ∪ [`ALLOWLIST`])
/// and unused private globs (`use super::*;` / `use <common>::error::*;`). Both are gated on whether
/// F's private binding can still be CONSUMED by any file — F's own body, or a descendant.
///
/// **Who can consume F's private import: the super-glob edge graph.** A private `use` binding in
/// module M is nameable only inside M and M's descendants, and a descendant reaches it exclusively
/// through a `use super::*;` chain (a glob from a non-descendant imports only `pub` items; explicit
/// `super::X` paths are not emitted by the generator — verified). So the ONLY files that can consume
/// F's privates are the descendants D linked to F by an UNBROKEN chain of `use super::*;` edges
/// (`reachable_via_super`). This is what makes a scope `mod.rs`'s blindly-pushed imports prunable:
/// the sub-scope `serialization.rs`/`cbor_encodings.rs` files that name the same idents do NOT
/// `use super::*;` the ROOT (their own scope `mod.rs` doesn't re-glob upward), so they never chained
/// to the root's copy in the first place — they resolve their idents inside their own scope.
///
/// **Named-candidate protection.** X (a candidate F imports privately) is protected iff F's own body
/// names X, or a super-reachable descendant D names X and D does NOT resolve X through a nearer
/// binding of its own. Three disqualifiers, all from nearest-binding resolution:
///   1. **Direct import** — D carries `use …::X;` (`direct_by_path`).
///   2. **Target module** — D is at/under the crate-anchored module F imports X FROM
///      (`collect_candidate_import_targets`) — the self-contained `serialization.rs`-DEFINES-it shape.
///   3. **Source glob** — D carries a `use M::*;` of the SAME module F imports X from
///      (`collect_candidate_import_sources` ∩ D's private glob paths) — the
///      `--common-import-override` `serialization::*` shape, where the definition is external and
///      target-module cannot see it. Over-removal stays loud (E0412/E0432/E0433) in the compile gates.
///
/// **Glob pruning.** `use super::*;` is removed from a file with no super-reachable descendant whose
/// own body names no PARENT-bound name it doesn't itself bind (`super_glob_needed`, universe =
/// `bound_names(parent)`). A `use <path>::error::*;` is removed when neither F nor a super-reachable
/// descendant demands an [`ERROR_MODULE_EXPORTS`] name it doesn't resolve locally (`error_glob_needed`).
///
/// A super-reachable descendant that fails to parse might consume ANY private import, so it poisons F
/// (F is skipped). Returns `(path, pruned_content)` for each CHANGED file; the content is NOT
/// rustfmt'd (the splice can leave loose spacing), so the caller must rustfmt each returned entry.
pub(crate) fn prune_generated_files(
    files: &BTreeMap<String, String>,
    config: &PruneConfig,
) -> Vec<(String, String)> {
    // Per `.rs` file, computed once. `None` in `used_by_path` marks an unparseable file. `direct` is
    // the DIRECTLY-imported leaf idents; `glob` is the module paths of the file's PRIVATE globs
    // (`super`, `cml_core::error`, …); `defs` is the top-level item names it defines.
    let mut used_by_path: BTreeMap<&str, Option<HashSet<String>>> = BTreeMap::new();
    let mut direct_by_path: BTreeMap<&str, HashSet<String>> = BTreeMap::new();
    let mut glob_by_path: BTreeMap<&str, HashSet<String>> = BTreeMap::new();
    let mut defs_by_path: BTreeMap<&str, HashSet<String>> = BTreeMap::new();
    for (path, content) in files {
        if path.ends_with(".rs") {
            used_by_path.insert(path, collect_used_idents_from_source(content));
            direct_by_path.insert(path, collect_directly_imported_idents(content));
            glob_by_path.insert(path, collect_private_glob_paths(content));
            defs_by_path.insert(path, collect_module_item_defs(content));
        }
    }
    // A file re-imports its PARENT's namespace via `use super::*;`.
    let has_super = |p: &str| glob_by_path.get(p).is_some_and(|g| g.contains("super"));

    let mut changed = Vec::new();
    for (path, content) in files {
        if !is_prunable_generated_rs(path) {
            continue;
        }
        let Some(Some(own_used)) = used_by_path.get(path.as_str()) else {
            continue; // this file doesn't parse — the splicer would refuse anyway
        };
        let dir = module_dir(path);
        let key = crate_key(path);
        let import_targets = collect_candidate_import_targets(content, key, config);
        let import_sources = collect_candidate_import_sources(content, config);

        // The super-reachable descendants of F (the complete set of files that can consume F's private
        // imports) plus whether F has any STRUCTURAL descendant module (the re-export-only rule needs
        // the structural check: a descendant that doesn't `use super::*;` still can't consume F's
        // privates, so the re-export-only wholesale removal is even safer, but we keep its guard as-is).
        let mut reach: Vec<&str> = Vec::new();
        let mut poisoned = false;
        let mut has_descendant = false;
        for (desc_path, desc_used) in &used_by_path {
            if *desc_path == path.as_str()
                || !desc_path.starts_with(&dir)
                || crate_key(desc_path) != key
            {
                continue;
            }
            has_descendant = true;
            if desc_used.is_none() {
                // An unparseable descendant might carry a `use super::*;` we can't see and consume ANY
                // private import of F — conservatively poison F (skip it entirely).
                poisoned = true;
                break;
            }
            if reachable_via_super(desc_path, path, files, &has_super) {
                reach.push(desc_path);
            }
        }

        // Re-export-only file shape: no descendant module can consume a private import, and the file
        // is all `use` items with only `crate::`-anchored `pub use`s — every private `use` is dead and
        // removed wholesale, regardless of the candidate set.
        if !has_descendant && is_reexport_only_file(content) {
            let pruned = splice_private_uses(content, true, &|_| false, &HashSet::new());
            if let Cow::Owned(new_content) = pruned {
                changed.push((path.clone(), new_content));
            }
            continue;
        }
        if poisoned {
            continue;
        }

        // Named-candidate protected set.
        let mut protected = own_used.clone();
        for desc_path in &reach {
            let idents = used_by_path[desc_path].as_ref().unwrap();
            let direct = &direct_by_path[desc_path];
            let desc_globs = &glob_by_path[desc_path];
            for id in idents {
                if direct.contains(id) {
                    continue; // disqualifier 1: D's own direct import
                }
                if let Some(bases) = import_targets.get(id)
                    && bases
                        .iter()
                        .any(|base| module_is_target_file(desc_path, base))
                {
                    continue; // disqualifier 2: D IS the file of X's crate-anchored target module
                }
                if let Some(srcs) = import_sources.get(id)
                    && srcs.iter().any(|s| desc_globs.contains(s))
                {
                    continue; // disqualifier 3: D globs the module F imports X from
                }
                protected.insert(id.clone());
            }
        }

        // Glob pruning.
        let mut remove_globs: HashSet<String> = HashSet::new();
        let own_globs = &glob_by_path[path.as_str()];
        if own_globs.contains("super")
            && reach.is_empty()
            && !super_glob_needed(
                path,
                files,
                &used_by_path,
                &glob_by_path,
                &defs_by_path,
                &direct_by_path,
            )
        {
            remove_globs.insert("super".to_owned());
        }
        let error_universe: HashSet<String> =
            ERROR_MODULE_EXPORTS.iter().map(|s| s.to_string()).collect();
        for gp in own_globs {
            if gp == "super" || gp.rsplit("::").next() != Some("error") {
                continue;
            }
            if !error_glob_needed(
                gp,
                &error_universe,
                path,
                &reach,
                &used_by_path,
                &direct_by_path,
                &glob_by_path,
            ) {
                remove_globs.insert(gp.clone());
            }
        }

        let pruned = splice_private_uses(
            content,
            false,
            &|ident| is_candidate(ident, config) && !protected.contains(ident),
            &remove_globs,
        );
        if let Cow::Owned(new_content) = pruned {
            changed.push((path.clone(), new_content));
        }
    }
    changed
}

/// Walk from descendant `d` up the module tree to `f`, requiring an unbroken chain of `use super::*;`
/// edges: `d` re-imports its parent's namespace only if `d` carries `use super::*;`, and each
/// intermediate must too for the chain to keep reaching upward. True iff `f` is reached.
fn reachable_via_super(
    d: &str,
    f: &str,
    files: &BTreeMap<String, String>,
    has_super: &impl Fn(&str) -> bool,
) -> bool {
    let mut cur = d.to_owned();
    // Bounded by module-tree depth; the generated tree is shallow, but cap defensively.
    for _ in 0..64 {
        if !has_super(&cur) {
            return false;
        }
        match parent_mod_file(&cur, files) {
            None => return false,
            Some(parent) => {
                if parent == f {
                    return true;
                }
                cur = parent;
            }
        }
    }
    false
}

/// The FILE of the parent module of `path`, or `None` for the generated-tree root (or a path outside
/// a `/generated/` tree, or a parent not present in the map). The parent module lives in some
/// directory `pd`; its file is `pd/mod.rs` OR the 2018 plain-file layout `pd.rs`, whichever the map
/// holds. `gen/a/mod.rs`'s parent module dir is `gen`; `gen/a/leaf.rs`'s parent module dir is
/// `gen/a`; `.../generated/mod.rs` (the root) has no parent.
fn parent_mod_file(path: &str, files: &BTreeMap<String, String>) -> Option<String> {
    let dir = &path[..path.rfind('/')?];
    let parent_module_dir = if path.ends_with("/mod.rs") {
        if dir.ends_with("/generated") {
            return None; // the generated-tree root has no parent module
        }
        &dir[..dir.rfind('/')?]
    } else {
        // `path` is `dir/leaf.rs`; its module is `dir`, whose parent module dir is `dir` itself
        // (the parent file is `dir/mod.rs` or the plain-file `dir.rs`).
        dir
    };
    let as_mod = format!("{parent_module_dir}/mod.rs");
    if files.contains_key(&as_mod) {
        return Some(as_mod);
    }
    let as_file = format!("{parent_module_dir}.rs");
    files.contains_key(&as_file).then_some(as_file)
}

/// Whether a file's `use super::*;` is NEEDED: F names some name bound in its PARENT module's
/// namespace (`bound_names`) that F does not itself directly bind. Conservative-keep (`true`) when
/// the parent's namespace can't be fully enumerated (an unknown glob) — never remove a load-bearing
/// re-glob. Only consulted for files with NO super-reachable descendant (see the driver).
fn super_glob_needed(
    f: &str,
    files: &BTreeMap<String, String>,
    used_by_path: &BTreeMap<&str, Option<HashSet<String>>>,
    glob_by_path: &BTreeMap<&str, HashSet<String>>,
    defs_by_path: &BTreeMap<&str, HashSet<String>>,
    direct_by_path: &BTreeMap<&str, HashSet<String>>,
) -> bool {
    let Some(parent) = parent_mod_file(f, files) else {
        return true;
    };
    let empty = HashSet::new();
    let own_used = match used_by_path.get(f) {
        Some(Some(u)) => u,
        _ => return true, // unparseable F — keep conservatively (the splicer refuses it anyway)
    };
    // Names F resolves WITHOUT `super::*`: its own direct imports, its own item defs, and the exports
    // of its own enumerable non-super globs (`error::*` → ERROR_MODULE_EXPORTS). The last matters when
    // F and its parent BOTH carry `error::*`: F resolves `DeserializeError` through its own error
    // glob, so a `super::*` supplying only such names is dead (the leaf `serialization.rs` of a
    // c-style-enum-only scope names error types from the prelude but nothing from the parent module).
    let mut own_resolvable = direct_by_path.get(f).unwrap_or(&empty).clone();
    own_resolvable.extend(defs_by_path.get(f).unwrap_or(&empty).iter().cloned());
    if let Some(globs) = glob_by_path.get(f)
        && globs.iter().any(|g| g.rsplit("::").next() == Some("error"))
    {
        own_resolvable.extend(ERROR_MODULE_EXPORTS.iter().map(|s| s.to_string()));
    }
    match bound_names(
        &parent,
        files,
        defs_by_path,
        direct_by_path,
        glob_by_path,
        0,
    ) {
        // F names a parent-bound name it does NOT itself resolve through a nearer binding.
        Some(universe) => own_used
            .iter()
            .any(|y| universe.contains(y) && !own_resolvable.contains(y)),
        None => true,
    }
}

/// The complete set of names module `m` binds into its namespace — item definitions, `use`-bound
/// leaf idents, and the exports of each of its private globs — or `None` when a glob's exports can't
/// be enumerated (so the caller must conservatively keep any dependent re-glob). `error::*` → the
/// fixed [`ERROR_MODULE_EXPORTS`]; `super::*` → recurse into the parent; any other glob → `None`.
fn bound_names(
    m: &str,
    files: &BTreeMap<String, String>,
    defs_by_path: &BTreeMap<&str, HashSet<String>>,
    direct_by_path: &BTreeMap<&str, HashSet<String>>,
    glob_by_path: &BTreeMap<&str, HashSet<String>>,
    depth: usize,
) -> Option<HashSet<String>> {
    if depth > 32 {
        return None;
    }
    let mut result = defs_by_path.get(m)?.clone();
    if let Some(direct) = direct_by_path.get(m) {
        result.extend(direct.iter().cloned());
    }
    if let Some(globs) = glob_by_path.get(m) {
        for gp in globs {
            if gp.rsplit("::").next() == Some("error") {
                result.extend(ERROR_MODULE_EXPORTS.iter().map(|s| s.to_string()));
            } else if gp == "super" {
                let parent = parent_mod_file(m, files)?;
                let inner = bound_names(
                    &parent,
                    files,
                    defs_by_path,
                    direct_by_path,
                    glob_by_path,
                    depth + 1,
                )?;
                result.extend(inner);
            } else {
                return None; // unenumerable glob → universe incomplete
            }
        }
    }
    Some(result)
}

/// Whether a `use <path>::error::*;` glob (`gp`, universe = [`ERROR_MODULE_EXPORTS`]) is NEEDED:
/// F's own body, or a super-reachable descendant, demands an error export it doesn't resolve locally.
/// A descendant resolves an error name locally when it directly imports it OR carries its OWN glob of
/// the same error module (`gp`).
fn error_glob_needed(
    gp: &str,
    universe: &HashSet<String>,
    f: &str,
    reach: &[&str],
    used_by_path: &BTreeMap<&str, Option<HashSet<String>>>,
    direct_by_path: &BTreeMap<&str, HashSet<String>>,
    glob_by_path: &BTreeMap<&str, HashSet<String>>,
) -> bool {
    let demands = |file: &str| -> bool {
        let Some(Some(used)) = used_by_path.get(file) else {
            return false;
        };
        let direct = direct_by_path.get(file).cloned().unwrap_or_default();
        used.iter()
            .any(|y| universe.contains(y) && !direct.contains(y))
    };
    if demands(f) {
        return true;
    }
    for d in reach {
        if glob_by_path.get(d).is_some_and(|g| g.contains(gp)) {
            continue; // D resolves error names through its own glob of the same module
        }
        if demands(d) {
            return true;
        }
    }
    false
}

/// The crate a generated file belongs to: everything before `/src/` (`rust/src/generated/mod.rs`
/// -> `rust`; `wasm/json-gen/src/generated/mod.rs` -> `wasm/json-gen`). Files in different crates
/// are never glob-connected, so descendant protection never crosses this boundary.
fn crate_key(path: &str) -> &str {
    path.split_once("/src/")
        .map(|(head, _)| head)
        .unwrap_or(path)
}

/// The directory that holds a file's child modules: `d/mod.rs` owns `d/` (its children live beside
/// it); `d/foo.rs` owns `d/foo/` (2018-edition layout — children of `foo` live under `d/foo/`).
/// A path P is a strict descendant module of F iff P starts with F's module dir (and P ≠ F —
/// relevant only for `mod.rs`, which itself sits inside its own module dir).
fn module_dir(path: &str) -> String {
    if path == "mod.rs" {
        String::new()
    } else if let Some(dir) = path.strip_suffix("/mod.rs") {
        format!("{dir}/")
    } else if let Some(stem) = path.strip_suffix(".rs") {
        format!("{stem}/")
    } else {
        // Not a `.rs` path — never a prune candidate; return something that matches nothing.
        format!("{path}/")
    }
}

/// A generated-tree `.rs` file whose imports this pass may rewrite: any `.rs` under a `/generated/`
/// directory (the rust, wasm, and json-gen generated trees). Excludes the seed-once `lib.rs` roots
/// and manifests, which carry no allowlisted imports anyway.
fn is_prunable_generated_rs(path: &str) -> bool {
    path.ends_with(".rs") && path.contains("/generated/")
}

/// Local half of the re-export-only file-shape qualifier (conditions (b) and (d) — see the module
/// docs). True iff `source` parses, every top-level item is a `use`, and every NON-private `use` is
/// a plain `crate::`-anchored path chain ending in `Name`/`Rename`. The driver adds the
/// no-descendant check (condition (c)); a qualifying file has ALL its private `use` items removed.
fn is_reexport_only_file(source: &str) -> bool {
    let Ok(file) = syn::parse_file(source) else {
        return false;
    };
    for item in &file.items {
        let Item::Use(use_item) = item else {
            // Any code item — `fn`, `struct`, `Item::Mod` (even bodyless), `Item::Macro`,
            // `Item::ExternCrate`, verbatim — means a private import could be consumed locally.
            return false;
        };
        // A non-private (`pub`, `pub(crate)`, …) `use` is API surface we keep; it must be
        // `crate::`-anchored so it cannot resolve through a private glob we would be deleting.
        if !matches!(use_item.vis, syn::Visibility::Inherited)
            && !is_crate_anchored_use_tree(&use_item.tree)
        {
            return false;
        }
    }
    true
}

/// A non-private `use` tree we can safely leave standing in a re-export-only file: a plain path
/// chain whose first segment is `crate`, ending in a `Name` or `Rename` leaf. Any `Glob` or `Group`
/// anywhere in the chain → false (conservative: a group/glob could pull in relative bindings).
fn is_crate_anchored_use_tree(tree: &UseTree) -> bool {
    match tree {
        UseTree::Path(path) if path.ident == "crate" => is_plain_path_tail(&path.tree),
        _ => false,
    }
}

/// The tail of a `crate::`-anchored path: only nested `Path` segments ending in `Name`/`Rename`.
fn is_plain_path_tail(tree: &UseTree) -> bool {
    match tree {
        UseTree::Path(path) => is_plain_path_tail(&path.tree),
        UseTree::Name(_) | UseTree::Rename(_) => true,
        UseTree::Glob(_) | UseTree::Group(_) => false,
    }
}

/// Collect every `Ident` that appears in `source` outside a top-level PRIVATE `use` item, recursing
/// into the token streams of macro bodies and attributes (which `syn::visit` does NOT descend
/// into). We render each collected item back to a `TokenStream` and walk it, so field types, fn
/// signatures, expressions, attribute tokens, and macro-invocation tokens are ALL covered uniformly
/// — a `TokenTree::Ident` anywhere in that stream counts as a use, EXCEPT a path-tail segment
/// immediately following `::`, which resolves relative to its preceding segment and can never consume
/// a `use` binding (see [`collect_idents_in_tokens`]). `pub use` items are collected
/// too (only private ones are excluded): a re-export path like `pub use BTreeMap;` consumes a
/// private binding in scope, so its idents must protect the corresponding import. Returns `None` if
/// `source` doesn't parse (the caller must then treat this file as possibly-using-anything and skip
/// pruning any file it might be protecting).
pub(crate) fn collect_used_idents_from_source(source: &str) -> Option<HashSet<String>> {
    let file = syn::parse_file(source).ok()?;
    let mut used = HashSet::new();
    for item in &file.items {
        if let Item::Use(use_item) = item
            && matches!(use_item.vis, syn::Visibility::Inherited)
        {
            continue;
        }
        collect_idents_in_tokens(item.to_token_stream(), &mut used);
    }
    Some(used)
}

/// Ident collector with the path-tail exclusion (see the module docs' "Soundness boundary —
/// path-tail idents"): an `Ident` immediately preceded by the path separator `::` is a path-tail
/// segment (module path, associated item, enum variant) that resolves relative to the preceding
/// segment, NEVER through the local module namespace, so it can never consume a `use` binding and
/// must not protect an import or glob. `::` is two adjacent `Punct(':')` tokens — the first with
/// `Spacing::Joint`, the second with `Spacing::Alone` — so the precise test is: the previous token
/// is `Punct(':')` AND the one before it is `Punct(':')` with `Spacing::Joint`. A LONE `:` (struct
/// field type, `let x: BTreeMap<…>`) fails the two-token joint check, so the ident after it still
/// counts (over-skipping there would un-protect a load-bearing import — failure asymmetry: an
/// over-prune is a consumer compile error, an under-prune only a warning). The last-two-token
/// context is per token stream: a `Group`'s inner stream starts unpreceded, so recursion begins with
/// a fresh context.
fn collect_idents_in_tokens(tokens: TokenStream, used: &mut HashSet<String>) {
    // The two tokens immediately preceding the current one (in THIS stream). `prev1` is the token
    // right before; `prev2` is the one before that.
    let mut prev1: Option<TokenTree> = None;
    let mut prev2: Option<TokenTree> = None;
    for tree in tokens {
        match &tree {
            TokenTree::Ident(ident) => {
                let after_path_sep = matches!(&prev1, Some(TokenTree::Punct(p)) if p.as_char() == ':')
                    && matches!(&prev2, Some(TokenTree::Punct(p)) if p.as_char() == ':' && p.spacing() == Spacing::Joint);
                if !after_path_sep {
                    used.insert(ident.to_string());
                }
            }
            TokenTree::Group(group) => collect_idents_in_tokens(group.stream(), used),
            TokenTree::Punct(_) | TokenTree::Literal(_) => {}
        }
        prev2 = prev1.take();
        prev1 = Some(tree);
    }
}

/// Collect the leaf idents every `use` item in `source` DIRECTLY binds into this module's namespace:
/// a `Name` leaf (`use a::b::X;` binds `X`) and a `Rename` target (`use a::b::Y as X;` binds `X`),
/// recursing through paths and groups (`use a::{X, Y as Z};` binds `X` and `Z`). Both private and
/// non-private uses count — either creates a local binding that this module's uses of the name
/// resolve to in preference to anything reaching it through a `use super::*;` chain. A `Glob` leaf
/// binds no specific ident (it IS the indirect chain the driver distinguishes from), so it
/// contributes nothing. Returns an empty set if `source` doesn't parse — such a file is already
/// `None` in `used_by_path` and poisons its ancestors, so it never reaches the per-descendant
/// filter that consults this set.
fn collect_directly_imported_idents(source: &str) -> HashSet<String> {
    let mut direct = HashSet::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            if let Item::Use(use_item) = item {
                collect_use_tree_leaf_idents(&use_item.tree, &mut direct);
            }
        }
    }
    direct
}

fn collect_use_tree_leaf_idents(tree: &UseTree, direct: &mut HashSet<String>) {
    match tree {
        UseTree::Name(name) => {
            direct.insert(name.ident.to_string());
        }
        UseTree::Rename(rename) => {
            direct.insert(rename.rename.to_string());
        }
        UseTree::Path(path) => collect_use_tree_leaf_idents(&path.tree, direct),
        UseTree::Group(group) => {
            for item in &group.items {
                collect_use_tree_leaf_idents(item, direct);
            }
        }
        UseTree::Glob(_) => {}
    }
}

/// For F's own PRIVATE candidate imports (the only named ones this pass can remove), map each
/// candidate leaf ident X to the crate-relative FILE BASE(s) of the module F imports X FROM — but
/// only when the import path is `crate::`-anchored. `use crate::a::b::X;` in crate `<crate_root>`
/// yields base `<crate_root>/src/a/b`, whose module is the file `…/a/b.rs` (or `…/a/b/mod.rs`) plus
/// everything under `…/a/b/`. An `std::`/external/`self::`/`super::` path names no in-crate module,
/// so it contributes nothing and the target-module disqualifier never fires for that leaf (unchanged
/// for the collection helpers, which come from `std`/the runtime crate). Combined `use a::{X, Y};`
/// applies the same prefix to each leaf. Returns empty if `source` doesn't parse.
fn collect_candidate_import_targets(
    source: &str,
    crate_root: &str,
    config: &PruneConfig,
) -> BTreeMap<String, Vec<String>> {
    let mut targets: BTreeMap<String, Vec<String>> = BTreeMap::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            if let Item::Use(use_item) = item
                && matches!(use_item.vis, syn::Visibility::Inherited)
            {
                walk_use_targets(
                    &use_item.tree,
                    &mut Vec::new(),
                    crate_root,
                    config,
                    &mut targets,
                );
            }
        }
    }
    targets
}

fn walk_use_targets(
    tree: &UseTree,
    prefix: &mut Vec<String>,
    crate_root: &str,
    config: &PruneConfig,
    targets: &mut BTreeMap<String, Vec<String>>,
) {
    match tree {
        UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            walk_use_targets(&path.tree, prefix, crate_root, config, targets);
            prefix.pop();
        }
        UseTree::Group(group) => {
            for item in &group.items {
                walk_use_targets(item, prefix, crate_root, config, targets);
            }
        }
        UseTree::Name(name) => {
            let ident = name.ident.to_string();
            if is_candidate(&ident, config)
                && let Some(base) = target_base_from_prefix(prefix, crate_root)
            {
                targets.entry(ident).or_default().push(base);
            }
        }
        // A rename binds a different local name (its `as` target, not X), and a glob binds no
        // specific ident — neither is a candidate-leaf import path to derive a target from.
        UseTree::Rename(_) | UseTree::Glob(_) => {}
    }
}

/// For F's own PRIVATE candidate imports, map each candidate leaf ident X to the `::`-joined module
/// path(s) F imports X FROM (`use cml_core::serialization::LenEncoding;` → `cml_core::serialization`).
/// This is the source-glob disqualifier's key: a descendant that carries a `use <same path>::*;`
/// resolves X through its own glob, not through F's re-import. Verbatim path (external or relative) —
/// no crate-anchoring needed, since it's compared against another file's glob path spelled the same
/// way by the same emitter. Returns empty if `source` doesn't parse.
fn collect_candidate_import_sources(
    source: &str,
    config: &PruneConfig,
) -> BTreeMap<String, Vec<String>> {
    let mut sources: BTreeMap<String, Vec<String>> = BTreeMap::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            if let Item::Use(use_item) = item
                && matches!(use_item.vis, syn::Visibility::Inherited)
            {
                walk_use_sources(&use_item.tree, &mut Vec::new(), config, &mut sources);
            }
        }
    }
    sources
}

fn walk_use_sources(
    tree: &UseTree,
    prefix: &mut Vec<String>,
    config: &PruneConfig,
    sources: &mut BTreeMap<String, Vec<String>>,
) {
    match tree {
        UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            walk_use_sources(&path.tree, prefix, config, sources);
            prefix.pop();
        }
        UseTree::Group(group) => {
            for item in &group.items {
                walk_use_sources(item, prefix, config, sources);
            }
        }
        UseTree::Name(name) => {
            let ident = name.ident.to_string();
            if is_candidate(&ident, config) && !prefix.is_empty() {
                sources.entry(ident).or_default().push(prefix.join("::"));
            }
        }
        UseTree::Rename(_) | UseTree::Glob(_) => {}
    }
}

/// The `::`-joined module path of every PRIVATE glob (`use PATH::*;`) in `source` — `super`,
/// `cml_core::error`, `super::cbor_encodings`, … Used both as the source-glob disqualifier's
/// membership set and to detect a file's own prunable `super`/`error` globs. Empty if `source`
/// doesn't parse.
fn collect_private_glob_paths(source: &str) -> HashSet<String> {
    let mut globs = HashSet::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            if let Item::Use(use_item) = item
                && matches!(use_item.vis, syn::Visibility::Inherited)
            {
                walk_glob_paths(&use_item.tree, &mut Vec::new(), &mut globs);
            }
        }
    }
    globs
}

fn walk_glob_paths(tree: &UseTree, prefix: &mut Vec<String>, globs: &mut HashSet<String>) {
    match tree {
        UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            walk_glob_paths(&path.tree, prefix, globs);
            prefix.pop();
        }
        UseTree::Group(group) => {
            for item in &group.items {
                walk_glob_paths(item, prefix, globs);
            }
        }
        UseTree::Glob(_) => {
            if !prefix.is_empty() {
                globs.insert(prefix.join("::"));
            }
        }
        UseTree::Name(_) | UseTree::Rename(_) => {}
    }
}

/// The top-level item NAMES a module defines in its own namespace (`struct`/`enum`/`fn`/`type`/
/// `const`/`static`/`trait`/`union`/`mod`/`macro_rules`/`extern crate`). Combined with the module's
/// `use`-bound leaves and its globs' exports, this is what `use super::*;` re-exports downward — the
/// universe for deciding whether a child's `super::*` is load-bearing. Empty if `source` doesn't parse.
fn collect_module_item_defs(source: &str) -> HashSet<String> {
    let mut defs = HashSet::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            let name = match item {
                Item::Struct(i) => Some(i.ident.to_string()),
                Item::Enum(i) => Some(i.ident.to_string()),
                Item::Fn(i) => Some(i.sig.ident.to_string()),
                Item::Type(i) => Some(i.ident.to_string()),
                Item::Const(i) => Some(i.ident.to_string()),
                Item::Static(i) => Some(i.ident.to_string()),
                Item::Trait(i) => Some(i.ident.to_string()),
                Item::TraitAlias(i) => Some(i.ident.to_string()),
                Item::Union(i) => Some(i.ident.to_string()),
                Item::Mod(i) => Some(i.ident.to_string()),
                Item::ExternCrate(i) => Some(i.ident.to_string()),
                Item::Macro(i) => i.ident.as_ref().map(|id| id.to_string()),
                _ => None,
            };
            if let Some(name) = name {
                defs.insert(name);
            }
        }
    }
    defs
}

/// The crate-relative file base of the module a `crate::`-anchored path prefix names: `["crate",
/// "a", "b"]` in crate `<crate_root>` → `<crate_root>/src/a/b`. Returns `None` for a non-`crate::`
/// prefix (external/relative — no in-crate module). `use crate::X;` (no middle segments) targets the
/// crate root; base `<crate_root>/src` makes every in-crate file at/under it, which is exactly right
/// for a crate-root item every module reaches via `crate::X` (does not arise for the allowlist).
fn target_base_from_prefix(prefix: &[String], crate_root: &str) -> Option<String> {
    let (first, middle) = prefix.split_first()?;
    if first != "crate" {
        return None;
    }
    if middle.is_empty() {
        Some(format!("{crate_root}/src"))
    } else {
        Some(format!("{crate_root}/src/{}", middle.join("/")))
    }
}

/// True iff `file_path` IS the file of the module at `target_base` — `{base}.rs` or `{base}/mod.rs`.
/// The target-module disqualifier only holds for the file that DEFINES X (the `serialization.rs`
/// that owns `LenEncoding` via its static prelude), NOT for a file merely nested under it: a deeper
/// descendant reaches the target's items only through a `super::*` chain, and if that chain is
/// unbroken to the target it is a NEARER binding than F only when the target lies between D and F —
/// the un-modeled intermediate case, kept conservative (never remove X a deeper file might need). A
/// crate-ROOT target (`use crate::generated::X;` → base `.../generated`) makes only the root
/// `mod.rs` the definer, so a `plutus/serialization.rs` naming a root-defined `X` still protects the
/// `plutus/mod.rs` re-import it actually consumes.
fn module_is_target_file(file_path: &str, target_base: &str) -> bool {
    file_path == format!("{target_base}.rs") || file_path == format!("{target_base}/mod.rs")
}

/// Filter leaves from a `UseTree`. A `Name` leaf is dropped when `remove_named` returns true; a
/// `Glob` leaf is dropped when its full `::`-joined module path (built from the `prefix` of `Path`
/// segments walked to reach it) is in `remove_globs`. Returns `None` if the whole tree becomes empty
/// (the caller drops the item), otherwise the filtered tree. `Rename` leaves are always kept (the
/// alias target can't be connected to the local name by an ident scan); a single-element group
/// collapses back to its inner tree (so `use x::{Kept};` renders as `use x::Kept;`).
fn filter_use_tree(
    tree: &UseTree,
    prefix: &mut Vec<String>,
    remove_named: &dyn Fn(&str) -> bool,
    remove_globs: &HashSet<String>,
) -> Option<UseTree> {
    match tree {
        UseTree::Name(name) => {
            if remove_named(&name.ident.to_string()) {
                None
            } else {
                Some(tree.clone())
            }
        }
        // A rename's binding (`use x::A as B;`) is exercised by the local name `B`, which our ident
        // scan cannot connect back to `A`, so it is always kept.
        UseTree::Rename(_) => Some(tree.clone()),
        UseTree::Glob(_) => {
            if remove_globs.contains(&prefix.join("::")) {
                None
            } else {
                Some(tree.clone())
            }
        }
        UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            let inner = filter_use_tree(&path.tree, prefix, remove_named, remove_globs);
            prefix.pop();
            inner.map(|inner| {
                let mut new_path = path.clone();
                new_path.tree = Box::new(inner);
                UseTree::Path(new_path)
            })
        }
        UseTree::Group(group) => {
            // A group shares the prefix accumulated so far; filter each item under that same prefix.
            let kept: Vec<UseTree> = group
                .items
                .iter()
                .filter_map(|item| filter_use_tree(item, prefix, remove_named, remove_globs))
                .collect();
            match kept.len() {
                0 => None,
                1 => Some(kept.into_iter().next().unwrap()),
                _ => {
                    let mut new_group = group.clone();
                    new_group.items = kept.into_iter().collect();
                    Some(UseTree::Group(new_group))
                }
            }
        }
    }
}

fn trees_equal(a: &UseTree, b: &UseTree) -> bool {
    a.to_token_stream().to_string() == b.to_token_stream().to_string()
}

/// Byte range `[start, end)` covering the whole `use ...;` item (including any leading attributes,
/// which are part of the item's span). Regular `//` comments are trivia, not tokens, so they fall
/// OUTSIDE this range and survive the splice.
fn item_byte_range(item: &ItemUse, line_starts: &[usize], source: &str) -> (usize, usize) {
    let span = item.span();
    let start = line_col_to_byte(line_starts, source, span.start().line, span.start().column);
    let end = line_col_to_byte(line_starts, source, span.end().line, span.end().column);
    (start, end)
}

/// For a whole-item DELETION: when the item is alone on its line(s) — only indentation before it,
/// only trailing whitespace after it — expand `[start, end)` to swallow that indentation and the
/// trailing newline, so the line vanishes instead of leaving a blank-line scar (rustfmt does not
/// collapse a lone blank line between items). If anything else shares a line with the item (e.g. a
/// trailing `// comment`, which must survive), the range is returned unexpanded and only the item's
/// exact span is deleted.
fn expand_to_whole_line(source: &str, start: usize, end: usize) -> (usize, usize) {
    let bytes = source.as_bytes();
    // Walk back over the line's leading indentation.
    let mut line_start = start;
    while line_start > 0 && matches!(bytes[line_start - 1], b' ' | b'\t') {
        line_start -= 1;
    }
    let at_line_start = line_start == 0 || bytes[line_start - 1] == b'\n';
    // Walk forward over trailing whitespace to the newline (or EOF).
    let mut line_end = end;
    while matches!(bytes.get(line_end), Some(b' ' | b'\t')) {
        line_end += 1;
    }
    let newline_len = match bytes.get(line_end) {
        Some(b'\n') => 1,
        Some(b'\r') if bytes.get(line_end + 1) == Some(&b'\n') => 2,
        None => 0,                // EOF ends the line too
        _ => return (start, end), // something else on the line — delete only the item span
    };
    if at_line_start {
        (line_start, line_end + newline_len)
    } else {
        (start, end)
    }
}

/// Byte offset of the start of each 1-based line.
fn line_start_offsets(source: &str) -> Vec<usize> {
    let mut starts = vec![0usize];
    for (idx, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            starts.push(idx + 1);
        }
    }
    starts
}

/// Convert a proc-macro2 `LineColumn` (1-based line, 0-based column counted in characters) to a
/// byte offset. Column is advanced character-by-character so the mapping is correct even if a line
/// contains multi-byte UTF-8 before the column (generated `use` lines are ASCII, but this stays
/// correct regardless).
fn line_col_to_byte(line_starts: &[usize], source: &str, line: usize, column: usize) -> usize {
    // `line` is 1-based; clamp defensively rather than panic on an unexpected span.
    let line_start = line_starts
        .get(line.saturating_sub(1))
        .copied()
        .unwrap_or(source.len());
    let mut offset = line_start;
    let mut remaining = column;
    for ch in source[line_start..].chars() {
        if remaining == 0 {
            break;
        }
        offset += ch.len_utf8();
        remaining -= 1;
    }
    offset
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prune(s: &str) -> String {
        prune_unused_type_imports(s).into_owned()
    }

    #[test]
    fn removes_unused_single_import() {
        let src = "use std::collections::BTreeMap;\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            !out.contains("BTreeMap"),
            "unused BTreeMap import should be removed: {out}"
        );
        assert!(out.contains("struct Foo"));
    }

    #[test]
    fn reshapes_group_dropping_unused_leaf() {
        // `Kept` is a non-allowlisted name, so it stays; `BTreeMap` is allowlisted and unused.
        let src = "use x::{BTreeMap, Kept};\nfn f() -> Kept { todo!() }\n";
        let out = prune(src);
        assert!(
            !out.contains("BTreeMap"),
            "unused BTreeMap should be dropped from group: {out}"
        );
        assert!(out.contains("Kept"), "Kept must survive: {out}");
        // Single-element group collapses to a plain path (the `use` line carries no braces).
        let use_line = out
            .lines()
            .find(|l| l.contains("use x"))
            .expect("use line present");
        assert!(
            !use_line.contains('{'),
            "single-element group should collapse: {use_line}"
        );
    }

    #[test]
    fn drops_whole_item_when_group_empties() {
        let src = "use x::{BTreeMap, NonEmptyVec};\nstruct Foo;\n";
        let out = prune(src);
        assert!(!out.contains("BTreeMap"));
        assert!(!out.contains("NonEmptyVec"));
        assert!(
            !out.contains("use x"),
            "emptied use item should be dropped entirely: {out}"
        );
        assert!(out.contains("struct Foo"));
    }

    #[test]
    fn keeps_when_used_in_field_type() {
        let src = "use std::collections::BTreeMap;\nstruct Foo { m: BTreeMap<u8, u8> }\n";
        let out = prune(src);
        assert!(
            out.contains("use std::collections::BTreeMap"),
            "used in field type: {out}"
        );
    }

    #[test]
    fn keeps_when_used_in_fn_signature() {
        let src = "use std::collections::BTreeMap;\nfn f(m: BTreeMap<u8, u8>) {}\n";
        let out = prune(src);
        assert!(out.contains("BTreeMap"), "used in fn signature: {out}");
    }

    #[test]
    fn keeps_when_used_in_expression() {
        let src =
            "use std::collections::BTreeMap;\nfn f() { let _ = BTreeMap::<u8, u8>::new(); }\n";
        let out = prune(src);
        assert!(
            out.contains("use std::collections::BTreeMap"),
            "used in expression: {out}"
        );
    }

    #[test]
    fn keeps_when_used_in_macro_tokens() {
        // The ident only appears inside a macro invocation's token stream — syn::visit would miss
        // it, but our manual TokenStream recursion catches it.
        let src = "use std::collections::BTreeMap;\nfn f() { do_thing!(BTreeMap); }\n";
        let out = prune(src);
        assert!(
            out.contains("use std::collections::BTreeMap"),
            "used in macro tokens: {out}"
        );
    }

    #[test]
    fn keeps_when_used_in_attribute_tokens() {
        let src = "use std::collections::BTreeMap;\n#[some_attr(BTreeMap)]\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            out.contains("use std::collections::BTreeMap"),
            "used in attribute tokens: {out}"
        );
    }

    #[test]
    fn leaves_non_allowlisted_unused_import_untouched() {
        let src = "use std::convert::TryFrom;\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            out.contains("use std::convert::TryFrom"),
            "non-allowlisted import kept: {out}"
        );
    }

    #[test]
    fn leaves_glob_and_rename_untouched() {
        let src = "use crate::generated::error::*;\nuse x::BTreeMap as MyMap;\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            out.contains("use crate::generated::error::*"),
            "glob kept: {out}"
        );
        assert!(
            out.contains("BTreeMap as MyMap"),
            "rename kept even for allowlisted name: {out}"
        );
    }

    #[test]
    fn unparseable_input_returned_byte_identical() {
        let src = "this is not valid rust @@@ {{{";
        let out = prune_unused_type_imports(src);
        assert!(
            matches!(out, Cow::Borrowed(_)),
            "unparseable input must be borrowed unchanged"
        );
        assert_eq!(out, src);
    }

    #[test]
    fn idempotent() {
        let src = "use std::collections::BTreeMap;\nuse x::{NonEmptyVec, Kept};\nfn f() -> Kept { todo!() }\n";
        let once = prune(src);
        let twice = prune(&once);
        assert_eq!(once, twice, "prune must be a fixed point");
    }

    #[test]
    fn preserves_header_and_adjacent_comments() {
        let src = "// This file was code-generated using an experimental CDDL to rust tool:\n\
                   // https://github.com/dcSpark/cddl-codegen\n\
                   \n\
                   // a comment right before the doomed import\n\
                   use std::collections::BTreeMap;\n\
                   struct Foo;\n";
        let out = prune(src);
        assert!(
            out.contains("This file was code-generated"),
            "header comment must survive: {out}"
        );
        assert!(
            out.contains("a comment right before the doomed import"),
            "adjacent comment must survive the splice: {out}"
        );
        assert!(!out.contains("BTreeMap"), "import still removed: {out}");
    }

    #[test]
    fn keeps_used_allowlisted_while_removing_unused_one() {
        let src = "use std::collections::BTreeMap;\n\
                   use lib::non_empty::NonEmptyVec;\n\
                   struct Foo { m: BTreeMap<u8, u8> }\n";
        let out = prune(src);
        assert!(
            out.contains("use std::collections::BTreeMap"),
            "used BTreeMap kept: {out}"
        );
        assert!(
            !out.contains("NonEmptyVec"),
            "unused NonEmptyVec removed: {out}"
        );
    }

    /// The `@duplicates reject` uniqueness twins (`OrderedSet`/`NonEmptyOrderedSet`) are blindly
    /// pushed on the spec-global `uses_ordered_set()` gate into every scope's `mod.rs`, exactly like
    /// `NonEmptyVec`, so a scope that references neither must have both dropped. This is the live
    /// multi-scope gap the ALLOWLIST addition closes: one scope carries a `@duplicates reject` set,
    /// another does not, yet the spec-global gate pushes the twin import into the latter too.
    #[test]
    fn prunes_unused_ordered_set_twins() {
        let src =
            "use crate::generated::ordered_set::{NonEmptyOrderedSet, OrderedSet};\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            !out.contains("OrderedSet"),
            "both unused ordered-set twins removed: {out}"
        );
        assert!(
            !out.contains("use crate::generated::ordered_set"),
            "emptied use item dropped entirely: {out}"
        );
        assert!(out.contains("struct Foo"));
    }

    /// The used twin is kept while its unused sibling is pruned from the same group — the shape a
    /// reject-set scope's `mod.rs` grows (`pub type FooSet = OrderedSet<u64>;` names `OrderedSet`
    /// but nothing names `NonEmptyOrderedSet`).
    #[test]
    fn keeps_used_ordered_set_twin_dropping_unused_sibling() {
        let src = "use crate::generated::ordered_set::{NonEmptyOrderedSet, OrderedSet};\npub type FooSet = OrderedSet<u64>;\n";
        let out = prune(src);
        assert!(
            out.contains("OrderedSet<u64>"),
            "used OrderedSet kept: {out}"
        );
        assert!(
            !out.contains("NonEmptyOrderedSet"),
            "unused NonEmptyOrderedSet sibling dropped: {out}"
        );
    }

    /// A `pub use` is API surface a downstream crate may import — usage analysis inside this crate
    /// can never justify removing it, so the pass must skip any use item with non-inherited
    /// visibility even when the name is allowlisted and unused.
    #[test]
    fn pub_use_is_never_pruned() {
        let src = "pub use std::collections::BTreeMap;\nstruct Foo;\n";
        let out = prune(src);
        assert!(
            out.contains("pub use std::collections::BTreeMap"),
            "pub use is exported API and must be kept: {out}"
        );
    }

    /// Whole-item deletion must consume the line's trailing newline (and leading indentation) so no
    /// blank-line scar is left — rustfmt does NOT collapse a lone blank line between items. A
    /// trailing same-line comment forces the conservative span-only deletion (the comment survives).
    #[test]
    fn whole_item_deletion_leaves_no_blank_line() {
        let src = "struct A;\nuse std::collections::BTreeMap;\nstruct Foo;\n";
        assert_eq!(prune(src), "struct A;\nstruct Foo;\n");
        let with_comment = "use std::collections::BTreeMap; // trailing note\nstruct Foo;\n";
        let out = prune(with_comment);
        assert!(
            out.contains("// trailing note"),
            "same-line trailing comment must survive: {out}"
        );
        assert!(!out.contains("BTreeMap"), "import still removed: {out}");
    }

    // ----- path-tail ident exclusion (`collect_idents_in_tokens`) -----

    fn idents_in(src: &str) -> HashSet<String> {
        let mut used = HashSet::new();
        collect_idents_in_tokens(src.parse().expect("tokenizes"), &mut used);
        used
    }

    /// A `::`-qualified path counts only its LEADING segment: the tail segments resolve relative to
    /// the preceding path, never through the local `use` namespace, so they must not be counted.
    #[test]
    fn path_tail_segments_not_counted() {
        let used = idents_in("crate::generated::assets::Coin::new()");
        assert!(
            !used.contains("assets"),
            "path-tail `assets` must not count: {used:?}"
        );
        assert!(
            !used.contains("Coin"),
            "path-tail `Coin` must not count: {used:?}"
        );
        assert!(
            used.contains("crate"),
            "leading path segment still counts: {used:?}"
        );
    }

    /// A LONE `:` (a struct field type / `let x: T`) is not a path separator: the ident after it
    /// resolves through the local namespace and MUST count (over-skipping would un-protect the import).
    #[test]
    fn lone_colon_does_not_skip_field_type() {
        let used = idents_in("struct S { inner: BTreeMap<u8, u8> }");
        assert!(
            used.contains("BTreeMap"),
            "field type after a lone `:` must count: {used:?}"
        );
    }

    /// A turbofish type arg (`foo::<T>()`) is preceded by `<`, not `::`, and DOES resolve from local
    /// scope, so it must count.
    #[test]
    fn turbofish_type_arg_counted() {
        let used = idents_in("foo::<BTreeMap>()");
        assert!(
            used.contains("BTreeMap"),
            "turbofish type arg must count: {used:?}"
        );
    }

    /// Inside a macro invocation's token stream the same rule holds: `::`-path segments don't count,
    /// but the bare LEADING crate segment and bare argument idents do.
    #[test]
    fn macro_tokens_skip_path_tails_keep_bare_args() {
        let used = idents_in("impl_wasm_list_needs_into!(wr_dep::sub::module::X, X, XList, Foo)");
        assert!(
            !used.contains("sub"),
            "macro path-segment `sub` must not count: {used:?}"
        );
        assert!(
            !used.contains("module"),
            "macro path-segment `module` must not count: {used:?}"
        );
        assert!(
            used.contains("wr_dep"),
            "bare leading `wr_dep` counts: {used:?}"
        );
        assert!(used.contains("X"), "bare `X` argument counts: {used:?}");
        assert!(
            used.contains("XList"),
            "bare `XList` argument counts: {used:?}"
        );
    }

    // ----- module-family driver (`prune_generated_files`) -----

    fn files(pairs: &[(&str, &str)]) -> BTreeMap<String, String> {
        pairs
            .iter()
            .map(|(p, c)| (p.to_string(), c.to_string()))
            .collect()
    }

    /// The regression guard for the cross-file soundness bug: `mod.rs` imports `BTreeMap` but its
    /// own body never names it — a child `serialization.rs` uses it via `use super::*;`. A per-file
    /// prune would wrongly strip `mod.rs`'s import (E0433 at the child). The driver must KEEP it
    /// because the child is a path-descendant of `mod.rs`'s module, hence in its protected set.
    #[test]
    fn keeps_parent_import_consumed_by_child_via_super_glob() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub mod serialization;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn f() { let _ = BTreeMap::<u8, u8>::new(); }\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "BTreeMap is used by the child via super::*; nothing should be pruned: {changed:?}"
        );
    }

    /// The precision half of the descendant model: a SIBLING is not a descendant, and a glob from a
    /// non-descendant imports only pub items — it can never consume a sibling's private import. So
    /// `foo.rs` using `BTreeMap` must NOT protect `bar.rs`'s unused import (the whole-crate union
    /// would have wrongly kept it, leaving the warning walls in multi-scope crates).
    #[test]
    fn sibling_usage_does_not_protect_sibling_import() {
        let map = files(&[
            (
                "rust/src/generated/foo.rs",
                "fn f() -> std::vec::Vec<u8> { let _ = BTreeMap::<u8, u8>::new(); vec![] }\n",
            ),
            (
                "rust/src/generated/bar.rs",
                "use std::collections::BTreeMap;\npub struct Bar;\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "exactly bar.rs must change: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/bar.rs");
        assert!(
            !changed[0].1.contains("BTreeMap"),
            "sibling usage must not protect bar.rs's unused import: {}",
            changed[0].1
        );
    }

    /// Descendant protection is transitive over nesting: a grand-child two module levels down
    /// protects both intermediate `mod.rs` files (each level's `use super::*;` chain can carry the
    /// binding down).
    #[test]
    fn nested_two_level_descendant_protects_ancestors() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub mod a;\npub struct Root;\n",
            ),
            (
                "rust/src/generated/a/mod.rs",
                "use std::collections::BTreeMap;\nuse super::*;\npub mod b;\n",
            ),
            (
                "rust/src/generated/a/b.rs",
                "use super::*;\nfn f() { let _ = BTreeMap::<u8, u8>::new(); }\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "the grand-child's use must protect both ancestors' imports: {changed:?}"
        );
    }

    /// Module→dir mapping covers both layouts: `d/mod.rs` owns `d/` (children beside it), and a
    /// PLAIN file `d/foo.rs` owns `d/foo/` (2018-edition layout — children under `d/foo/`). A child
    /// at `d/foo/child.rs` must protect `d/foo.rs`'s import; the sibling `d/other.rs` must not be
    /// protected by it.
    #[test]
    fn plain_file_module_dir_maps_to_its_own_subdir() {
        let map = files(&[
            (
                "rust/src/generated/foo.rs",
                "use std::collections::BTreeMap;\npub mod child;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/foo/child.rs",
                "use super::*;\nfn f() { let _ = BTreeMap::<u8, u8>::new(); }\n",
            ),
            (
                "rust/src/generated/other.rs",
                "use std::collections::BTreeMap;\npub struct Other;\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(
            changed.len(),
            1,
            "foo.rs is protected by foo/child.rs; only other.rs changes: {changed:?}"
        );
        assert_eq!(changed[0].0, "rust/src/generated/other.rs");
        assert!(!changed[0].1.contains("BTreeMap"));
    }

    /// When the allowlisted ident is absent from a file's whole module family, the driver prunes it.
    /// The child `serialization.rs` names nothing from the parent, so its own `use super::*;` is a
    /// dead glob and is pruned too (glob-prune) — both files change.
    #[test]
    fn prunes_when_ident_absent_across_module_family() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub mod serialization;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn f() {}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let mod_rs = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/mod.rs")
            .expect("mod.rs changes");
        assert!(
            !mod_rs.1.contains("BTreeMap"),
            "family-unused BTreeMap pruned: {}",
            mod_rs.1
        );
        let ser = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/serialization.rs")
            .expect("serialization.rs changes (dead super glob)");
        assert!(
            !ser.1.contains("use super::*"),
            "dead super::* glob pruned: {}",
            ser.1
        );
    }

    /// Different crates (split at `/src/`) never share protection: `BTreeMap` used in the `wasm`
    /// crate must not protect the unused import in the `rust` crate.
    #[test]
    fn crates_are_independent() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub struct Foo;\n",
            ),
            (
                "wasm/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub struct Bar { m: BTreeMap<u8, u8> }\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "only the rust file changes: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/mod.rs");
        assert!(!changed[0].1.contains("BTreeMap"));
    }

    /// An unparseable file might consume ANY private import of its ancestors, so it poisons exactly
    /// them: `mod.rs` (whose module dir contains the broken file) is skipped, but a SIBLING of the
    /// broken file — which the broken file cannot reach privately — still prunes.
    #[test]
    fn unparseable_descendant_poisons_ancestors_only() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/sibling.rs",
                "use std::collections::BTreeMap;\npub struct S;\n",
            ),
            ("rust/src/generated/broken.rs", "this is not @@@ rust {{{"),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(
            changed.len(),
            1,
            "mod.rs is poisoned by its broken descendant; only sibling.rs changes: {changed:?}"
        );
        assert_eq!(changed[0].0, "rust/src/generated/sibling.rs");
        assert!(!changed[0].1.contains("BTreeMap"));
    }

    /// Only `.rs` files under a `/generated/` dir are rewritten; the seed-once `lib.rs` root is
    /// never touched.
    #[test]
    fn does_not_rewrite_non_generated_files() {
        let map = files(&[
            (
                "rust/src/lib.rs",
                "use std::collections::BTreeMap;\nmod generated;\n",
            ),
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub struct Foo;\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.iter().all(|(p, _)| p != "rust/src/lib.rs"),
            "lib.rs must not be rewritten: {changed:?}"
        );
    }

    // ----- re-export-only file-shape rule (second prune rule) -----

    /// The CML extern-only-scope shape: a `mod.rs` that is nothing but a header comment, three
    /// blindly-pushed private imports (a trait, a glob, a group), and two `crate::`-anchored
    /// `pub use` re-exports. All three private uses are dropped WHOLESALE (the allowlist can touch
    /// none of them), whole lines with no blank-line scars, and the comments + `pub use`s survive
    /// byte-for-byte.
    #[test]
    fn reexport_only_file_drops_all_private_uses_byte_exactly() {
        let content = concat!(
            "// This file was code-generated using an experimental CDDL to rust tool:\n",
            "// https://github.com/dcSpark/cddl-codegen\n",
            "use std::convert::TryFrom;\n",
            "use cml_core::error::*;\n",
            "use std::collections::{BTreeMap, BTreeSet};\n",
            "pub use crate::Address;\n",
            "pub use crate::RewardAddress;\n",
        );
        let map = files(&[("rust/src/generated/address/mod.rs", content)]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(
            changed.len(),
            1,
            "the re-export-only file changes: {changed:?}"
        );
        assert_eq!(changed[0].0, "rust/src/generated/address/mod.rs");
        let expected = concat!(
            "// This file was code-generated using an experimental CDDL to rust tool:\n",
            "// https://github.com/dcSpark/cddl-codegen\n",
            "pub use crate::Address;\n",
            "pub use crate::RewardAddress;\n",
        );
        assert_eq!(
            changed[0].1, expected,
            "all private uses removed; comments + pub uses byte-identical"
        );
    }

    /// A descendant module means `use super::*;` could consume a private import, so the WHOLESALE
    /// re-export rule must NOT fire: the `TryFrom` trait import (which only the wholesale rule could
    /// remove) survives, and `BTreeMap` survives because the child names it through `super::*`. The
    /// `error::*` glob is still dropped by the ordinary glob-prune (the child names no error export).
    #[test]
    fn reexport_rule_skips_when_descendant_present() {
        let content = concat!(
            "use std::convert::TryFrom;\n",
            "use cml_core::error::*;\n",
            "use std::collections::{BTreeMap, BTreeSet};\n",
            "pub use crate::Address;\n",
        );
        let map = files(&[
            ("rust/src/generated/address/mod.rs", content),
            (
                "rust/src/generated/address/child.rs",
                "use super::*;\nfn f() { let _ = BTreeMap::<u8, u8>::new(); }\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let out = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/address/mod.rs")
            .map(|(_, c)| c.clone())
            .unwrap_or_else(|| content.to_owned());
        assert!(
            out.contains("use std::convert::TryFrom"),
            "wholesale rule must not fire — trait import kept: {out}"
        );
        assert!(
            out.contains("BTreeMap"),
            "child names BTreeMap via super::*, so it is kept: {out}"
        );
    }

    /// A single code item (here a `struct`) disqualifies the wholesale rule; the ordinary prune still
    /// runs, so the `TryFrom` trait import survives (traits are outside the name-scan model) while the
    /// unused `BTreeMap` leaf and the unused `error::*` glob are dropped.
    #[test]
    fn reexport_rule_skips_when_a_code_item_present() {
        let content = concat!(
            "use std::convert::TryFrom;\n",
            "use cml_core::error::*;\n",
            "use std::collections::{BTreeMap, BTreeSet};\n",
            "pub use crate::Address;\n",
            "pub struct Foo;\n",
        );
        let map = files(&[("rust/src/generated/mod.rs", content)]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "{changed:?}");
        let out = &changed[0].1;
        assert!(
            out.contains("use std::convert::TryFrom"),
            "trait import NOT removed wholesale (rule did not fire): {out}"
        );
        assert!(
            !out.contains("use cml_core::error::*"),
            "unused error::* glob pruned by the glob-prune: {out}"
        );
        assert!(
            out.contains("pub use crate::Address"),
            "pub use kept: {out}"
        );
        assert!(out.contains("pub struct Foo"), "code item kept: {out}");
        assert!(
            !out.contains("BTreeMap"),
            "unused allowlisted leaf pruned: {out}"
        );
    }

    /// The local qualifier `is_reexport_only_file`: every top-level item must be a `use`. Any code
    /// item — `fn`, a bodyless `mod`, a top-level macro invocation, a `struct` — disqualifies.
    #[test]
    fn reexport_shape_rejects_non_use_items() {
        assert!(is_reexport_only_file("pub use crate::Address;\n"));
        assert!(is_reexport_only_file(
            "use std::convert::TryFrom;\npub use crate::Address;\n"
        ));
        assert!(!is_reexport_only_file(
            "pub use crate::Address;\nfn f() {}\n"
        ));
        assert!(!is_reexport_only_file(
            "pub use crate::Address;\npub mod x;\n"
        ));
        assert!(!is_reexport_only_file(
            "pub use crate::Address;\nmy_macro!();\n"
        ));
        assert!(!is_reexport_only_file(
            "pub use crate::Address;\npub struct S;\n"
        ));
    }

    /// The `crate::`-anchoring guard (condition d): a non-private `use` that is relative
    /// (`self::`/`super::`) or carries a glob/group anywhere disqualifies the whole file — such a
    /// `pub use` could resolve through a private glob we would be deleting. A crate-anchored rename
    /// is fine.
    #[test]
    fn reexport_shape_rejects_non_crate_anchored_pub_use() {
        assert!(!is_reexport_only_file(
            "pub use self::Foo;\npub use crate::Address;\n"
        ));
        assert!(!is_reexport_only_file(
            "pub use super::x::Foo;\npub use crate::Address;\n"
        ));
        assert!(!is_reexport_only_file("pub use crate::generated::x::*;\n"));
        assert!(!is_reexport_only_file("pub use crate::{A, B};\n"));
        assert!(is_reexport_only_file("pub use crate::inner::Foo as Bar;\n"));
    }

    /// A `pub(crate) use crate::X;` is non-private (kept as API surface) but still `crate::`-anchored,
    /// so the file qualifies: the wholesale rule fires on the PRIVATE imports while the `pub(crate)`
    /// and `pub` re-exports and the header comment survive.
    #[test]
    fn reexport_rule_fires_with_pub_crate_use() {
        let content = concat!(
            "// header\n",
            "use std::convert::TryFrom;\n",
            "pub(crate) use crate::Address;\n",
            "pub use crate::RewardAddress;\n",
        );
        let map = files(&[("rust/src/generated/a/mod.rs", content)]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "{changed:?}");
        let out = &changed[0].1;
        assert!(
            !out.contains("use std::convert::TryFrom"),
            "private import removed: {out}"
        );
        assert!(
            out.contains("pub(crate) use crate::Address;"),
            "pub(crate) re-export kept: {out}"
        );
        assert!(
            out.contains("pub use crate::RewardAddress;"),
            "pub re-export kept: {out}"
        );
        assert!(out.contains("// header"), "header comment kept: {out}");
    }

    // ----- allowlist extension: encoding enums -----

    /// `LenEncoding`/`StringEncoding` (the `--preserve-encodings` push) are allowlisted, so a
    /// code-bearing file whose module family never names them has them pruned.
    #[test]
    fn encoding_enums_pruned_when_family_never_names_them() {
        let map = files(&[(
            "rust/src/generated/mod.rs",
            "use cml_core::serialization::LenEncoding;\n\
             use cml_core::serialization::StringEncoding;\n\
             pub struct Foo;\n",
        )]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "{changed:?}");
        let out = &changed[0].1;
        assert!(
            !out.contains("LenEncoding"),
            "unused LenEncoding pruned: {out}"
        );
        assert!(
            !out.contains("StringEncoding"),
            "unused StringEncoding pruned: {out}"
        );
        assert!(out.contains("pub struct Foo"));
    }

    /// The encoding-enum imports are KEPT when a descendant module names them via `use super::*;`
    /// (same descendant-protection contract as the collection types).
    #[test]
    fn encoding_enums_kept_when_descendant_names_them() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use cml_core::serialization::LenEncoding;\n\
                 use cml_core::serialization::StringEncoding;\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn f(a: LenEncoding, b: StringEncoding) {}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "descendant names both encodings; nothing pruned: {changed:?}"
        );
    }

    // ----- disqualifier composition: direct-import + target-module skip (A2) -----

    /// Direct-import disqualifier (the `cbor_encodings.rs` shape): a descendant that names
    /// `LenEncoding` AND carries its own `use crate::…::LenEncoding;` resolves to its own copy, so the
    /// ancestor `mod.rs`'s blindly-pushed copy is dead and pruned. (`cbor_encodings.rs` is NOT the
    /// `serialization` target file, so this isolates the direct-import path.) The child's `use
    /// super::*;` is itself dead here (it names `LenEncoding` through its own direct import and defines
    /// `E` locally), so the glob-prune removes it too — both files change.
    #[test]
    fn descendant_direct_import_does_not_protect_ancestor_copy() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 pub mod cbor_encodings;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/cbor_encodings.rs",
                "use super::*;\n\
                 use crate::generated::serialization::LenEncoding;\n\
                 pub struct E {\n    pub e: LenEncoding,\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let mod_rs = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/mod.rs")
            .expect("mod.rs changes");
        assert!(
            !mod_rs.1.contains("LenEncoding"),
            "direct-import consumer does not protect; ancestor copy pruned: {}",
            mod_rs.1
        );
    }

    /// Target-module skip (A2) — the real `serialization.rs` shape this fix exists for: `mod.rs`
    /// imports `LenEncoding` from `crate::generated::serialization`, and the only descendant naming it
    /// is `serialization.rs` itself, which in this generated-only pass names it WITHOUT a direct import
    /// (the defining prelude concats later). Because `serialization.rs` is the module the import
    /// targets, it does not protect `mod.rs`'s copy, which is pruned.
    #[test]
    fn target_module_descendant_does_not_protect_ancestor_reimport() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn de() {\n    let _x: LenEncoding = todo!();\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "only mod.rs changes: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/mod.rs");
        assert!(
            !changed[0].1.contains("LenEncoding"),
            "target-module descendant does not protect; ancestor copy pruned: {}",
            changed[0].1
        );
    }

    /// A descendant that names X via `use super::*;`, is NOT under X's import-target module, and does
    /// not directly import it, STILL protects the ancestor's copy (conservative keep) — its glob chain
    /// can genuinely reach `mod.rs`'s re-import.
    #[test]
    fn non_target_glob_descendant_still_protects() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 pub mod other;\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/other.rs",
                "use super::*;\nfn f() {\n    let _x: LenEncoding = todo!();\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "other.rs (not under serialization, no own import) protects mod.rs's copy: {changed:?}"
        );
    }

    /// External import paths (`std::…`, the runtime crate) name no in-crate module, so the
    /// target-module skip never fires: a glob-naming descendant still protects a
    /// `std::collections::BTreeMap` copy (contrast `target_module_descendant_does_not_protect_…`,
    /// where the SAME `serialization.rs` descendant DOES disqualify the crate-anchored encoding import).
    #[test]
    fn external_target_import_unaffected_by_target_skip() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn f() {\n    let _ = BTreeMap::<u8, u8>::new();\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "std target: serialization.rs still protects via super-glob; nothing pruned: {changed:?}"
        );
    }

    /// A combined `use crate::…::serialization::{LenEncoding, StringEncoding};` applies the same target
    /// module to each leaf, so both are pruned when the only namer is the `serialization.rs` descendant.
    #[test]
    fn combined_group_import_each_leaf_target_skipped() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::{LenEncoding, StringEncoding};\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn de(a: LenEncoding, b: StringEncoding) {}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert_eq!(changed.len(), 1, "only mod.rs changes: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/mod.rs");
        assert!(
            !changed[0].1.contains("LenEncoding"),
            "LenEncoding pruned: {}",
            changed[0].1
        );
        assert!(
            !changed[0].1.contains("StringEncoding"),
            "StringEncoding pruned: {}",
            changed[0].1
        );
    }

    /// The ancestor's OWN body naming X always protects its import, even when the only descendant is
    /// disqualified (here the target-module `serialization.rs`).
    #[test]
    fn ancestor_own_use_keeps_import_despite_disqualified_descendant() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 pub mod serialization;\n\
                 pub struct Foo {\n    pub e: LenEncoding,\n}\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nfn de(a: LenEncoding) {}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "mod.rs's own body uses LenEncoding; import kept: {changed:?}"
        );
    }

    /// Deliberately-conservative keep (documented residue (a)): an INTERMEDIATE module `a/mod.rs` that
    /// directly imports X consumes the root's copy for everything at/under `a`, so the root re-import is
    /// actually dead — but the per-descendant rule still counts the deep `a/b.rs` (which reaches X
    /// through `a/mod.rs`, not the root) as a protector, so the root copy is KEPT. Sound (warning-only);
    /// pinned as intentional — exact resolution modelling is the escalation, gated on a real warning
    /// report from the `feature_corpus_compiles` arm.
    #[test]
    fn conservative_intermediate_module_keep() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 pub mod a;\n\
                 pub mod serialization;\n\
                 pub struct Foo;\n",
            ),
            (
                "rust/src/generated/a/mod.rs",
                "use crate::generated::serialization::LenEncoding;\n\
                 use super::*;\n\
                 pub mod b;\n\
                 pub struct A {\n    pub e: LenEncoding,\n}\n",
            ),
            (
                "rust/src/generated/a/b.rs",
                "use super::*;\nfn f(x: LenEncoding) {}\n",
            ),
            ("rust/src/generated/serialization.rs", "use super::*;\n"),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/mod.rs"),
            "root copy conservatively kept (a/b.rs protects it under the descendant rule): {changed:?}"
        );
    }

    // ----- glob pruning: `use super::*;` -----

    fn cfg(extra: &[&str]) -> PruneConfig {
        PruneConfig {
            extra_candidates: extra.iter().map(|s| (*s).to_owned()).collect(),
        }
    }

    /// A `cbor_encodings.rs`-shaped file whose body names nothing from its parent module has its
    /// `use super::*;` removed (class (a)); its own direct imports and definitions do not count.
    #[test]
    fn super_glob_pruned_when_parent_names_unused() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::error::*;\npub mod cbor_encodings;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/cbor_encodings.rs",
                "use super::*;\nuse crate::generated::serialization::LenEncoding;\n\
                 pub struct E {\n    pub e: LenEncoding,\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let enc = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/cbor_encodings.rs")
            .expect("cbor_encodings changes");
        assert!(
            !enc.1.contains("use super::*"),
            "unused super::* pruned: {}",
            enc.1
        );
        assert!(
            enc.1.contains("LenEncoding"),
            "direct import kept: {}",
            enc.1
        );
    }

    /// The transaction-scope shape: a `cbor_encodings.rs` whose encoding struct is keyed by a
    /// PARENT-scope generated type reaches that type through `use super::*;`, so the glob is
    /// load-bearing and MUST stay.
    #[test]
    fn super_glob_kept_when_parent_type_named() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "pub mod cbor_encodings;\npub struct TxInput;\n",
            ),
            (
                "rust/src/generated/cbor_encodings.rs",
                "use super::*;\npub struct E {\n    pub keys: Vec<TxInput>,\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/cbor_encodings.rs"),
            "super::* is load-bearing (names parent-scope TxInput); must stay: {changed:?}"
        );
    }

    /// The c-style-enum-only-scope leaf `serialization.rs`: it names error types (`DeserializeError`
    /// from the static prelude) but nothing from the parent MODULE, and it carries its OWN `error::*`
    /// glob. The parent also globs `error::*`, so `bound_names(parent)` includes the error exports —
    /// but F resolves them through its own glob, not `super::*`, so `super::*` is dead and pruned
    /// while `error::*` stays.
    #[test]
    fn super_glob_pruned_when_only_names_resolve_via_own_error_glob() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use crate::generated::error::*;\npub enum FixedEnum {\n    A,\n    B,\n}\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nuse crate::generated::error::*;\n\
                 pub trait De {\n    fn de() -> Result<(), DeserializeError>;\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let ser = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/serialization.rs")
            .expect("serialization changes");
        assert!(
            !ser.1.contains("use super::*"),
            "dead super::* pruned (error names resolve via own glob): {}",
            ser.1
        );
        assert!(
            ser.1.contains("use crate::generated::error::*"),
            "own error::* kept (it resolves DeserializeError): {}",
            ser.1
        );
    }

    /// A file whose parent module carries an UNENUMERABLE glob (a non-`error` foreign glob) can't have
    /// its parent namespace fully computed, so its `use super::*;` is conservatively KEPT.
    #[test]
    fn super_glob_kept_when_parent_universe_unknown() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use some_dep::prelude::*;\npub mod cbor_encodings;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/cbor_encodings.rs",
                "use super::*;\npub struct E {\n    pub x: u8,\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/cbor_encodings.rs"),
            "parent has an unenumerable glob → super::* conservatively kept: {changed:?}"
        );
    }

    /// The sidecar-shape residual (`--wrapper-requests` `requested_collections.rs`): a flat file whose
    /// only references to the parent-bound module `sub` are `::`-QUALIFIED path tails
    /// (`<dep>::sub::module::X`). Before the path-tail exclusion the token scan counted `sub`, which the
    /// root binds via `pub mod sub;`, so `super::*` was conservatively (and wrongly) kept. With
    /// path-tail idents excluded the family names nothing parent-bound, so the dead `super::*` is pruned.
    #[test]
    fn sidecar_super_glob_pruned_when_sub_only_path_qualified() {
        let map = files(&[
            (
                "wasm/src/generated/mod.rs",
                "pub mod sub;\npub struct Root;\n",
            ),
            (
                "wasm/src/generated/requested_collections.rs",
                "use super::*;\n\
                 use crate::generated::assets::Coin;\n\
                 pub struct L(Vec<Coin>);\n\
                 impl L {\n    pub fn ext(&self) -> wr_dep::sub::module::ScopedExt {\n        todo!()\n    }\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let sidecar = changed
            .iter()
            .find(|(p, _)| p == "wasm/src/generated/requested_collections.rs")
            .expect("sidecar changes (dead super glob)");
        assert!(
            !sidecar.1.contains("use super::*"),
            "dead super::* pruned (sub only appears as a `::` path tail): {}",
            sidecar.1
        );
        assert!(
            sidecar.1.contains("use crate::generated::assets::Coin"),
            "the explicit Coin import (used in Vec<Coin>) is kept: {}",
            sidecar.1
        );
    }

    /// The near-miss twin: same sidecar, but one BARE (non-`::`-preceded) reference to the parent-bound
    /// `sub` module. That leading segment DOES count, so `super::*` is load-bearing and must stay —
    /// pinning the guard against over-pruning in the direction that breaks consumer builds.
    #[test]
    fn sidecar_super_glob_kept_when_bare_parent_ident_present() {
        let map = files(&[
            (
                "wasm/src/generated/mod.rs",
                "pub mod sub;\npub struct Root;\n",
            ),
            (
                "wasm/src/generated/requested_collections.rs",
                "use super::*;\n\
                 use crate::generated::assets::Coin;\n\
                 pub struct L(Vec<Coin>);\n\
                 impl L {\n    pub fn ext(&self) {\n        let _ = sub::module::helper();\n    }\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let sidecar = changed
            .iter()
            .find(|(p, _)| p == "wasm/src/generated/requested_collections.rs");
        assert!(
            sidecar.map_or(true, |(_, c)| c.contains("use super::*")),
            "bare leading `sub` is parent-bound; super::* must be kept: {changed:?}"
        );
    }

    // ----- glob pruning: `use <path>::error::*;` -----

    /// A `mod.rs` whose family names no error export has its `error::*` glob pruned (class (b)); the
    /// descendant `serialization.rs`, which uses error names through its OWN `error::*`, does not
    /// protect the parent copy (source-glob disqualifier for the glob universe).
    #[test]
    fn error_glob_pruned_when_family_uses_no_error_name() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use cml_core::error::*;\npub mod serialization;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/serialization.rs",
                "use super::*;\nuse cml_core::error::*;\n\
                 fn de() -> Result<(), DeserializeError> {\n    Ok(())\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let mod_rs = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/mod.rs")
            .expect("mod.rs changes");
        assert!(
            !mod_rs.1.contains("use cml_core::error::*"),
            "mod.rs error::* pruned (only serialization uses error, via its own glob): {}",
            mod_rs.1
        );
    }

    /// A bounds-carrying wrapper whose `mod.rs` BODY does a fallible conversion naming `DeserializeError`
    /// keeps its `error::*` glob.
    #[test]
    fn error_glob_kept_when_mod_body_uses_error_name() {
        let map = files(&[(
            "rust/src/generated/mod.rs",
            "use cml_core::error::*;\n\
             pub struct W(u8);\n\
             impl TryFrom<u8> for W {\n    type Error = DeserializeError;\n    \
             fn try_from(x: u8) -> Result<Self, DeserializeError> {\n        Ok(W(x))\n    }\n}\n",
        )]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "mod.rs body names DeserializeError; error::* kept: {changed:?}"
        );
    }

    // ----- source-glob disqualifier (class (c), --common-import-override shape) -----

    /// `mod.rs` imports `LenEncoding` from `cml_core::serialization` (external — no crate-anchored
    /// target). Its only super-reachable namer is `serialization.rs`, which resolves `LenEncoding`
    /// through its OWN `use cml_core::serialization::*;` glob — the source-glob disqualifier drops it,
    /// so the parent copy is pruned. `cbor_encodings.rs` directly imports it (disqualifier 1).
    #[test]
    fn source_glob_descendant_does_not_protect() {
        let map = files(&[
            (
                "rust/src/generated/assets/mod.rs",
                "use cml_core::serialization::LenEncoding;\n\
                 pub mod cbor_encodings;\npub mod serialization;\npub struct Foo;\n",
            ),
            (
                "rust/src/generated/assets/cbor_encodings.rs",
                "use super::*;\nuse cml_core::serialization::LenEncoding;\n\
                 pub struct E {\n    pub e: LenEncoding,\n}\n",
            ),
            (
                "rust/src/generated/assets/serialization.rs",
                "use super::*;\nuse cml_core::serialization::*;\n\
                 fn ser(x: LenEncoding) {}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let mod_rs = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/assets/mod.rs")
            .expect("mod.rs changes");
        assert!(
            !mod_rs.1.contains("LenEncoding"),
            "serialization resolves LenEncoding via its own glob; parent copy pruned: {}",
            mod_rs.1
        );
    }

    // ----- super-glob-edge reachability (class (c)/(d) root over-breadth) -----

    /// A SUB-SCOPE `serialization.rs` does not `use super::*;` the ROOT (its own scope `mod.rs` does
    /// not re-glob upward), so it can NOT consume the root's private imports — even though it NAMES the
    /// same ident. The root's unused import is pruned; a same-scope sibling that DOES chain to its own
    /// scope's `mod.rs` still protects that scope's copy.
    #[test]
    fn sub_scope_serialization_does_not_protect_root_import() {
        let map = files(&[
            (
                "rust/src/generated/mod.rs",
                "use std::collections::BTreeMap;\npub mod tx;\npub struct Root;\n",
            ),
            (
                "rust/src/generated/tx/mod.rs",
                // a sub-scope mod.rs: no `use super::*;` back to root
                "use std::collections::BTreeMap;\npub mod serialization;\n\
                 pub struct Tx {\n    pub m: BTreeMap<u8, u8>,\n}\n",
            ),
            (
                "rust/src/generated/tx/serialization.rs",
                "use super::*;\nfn de() {\n    let _ = BTreeMap::<u8, u8>::new();\n}\n",
            ),
        ]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        let root = changed
            .iter()
            .find(|(p, _)| p == "rust/src/generated/mod.rs")
            .expect("root mod.rs changes");
        assert!(
            !root.1.contains("BTreeMap"),
            "root's BTreeMap is not consumable by tx/serialization (no super chain to root): {}",
            root.1
        );
        // tx/mod.rs's own BTreeMap is used by its own body AND its serialization descendant — kept.
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/tx/mod.rs"),
            "tx/mod.rs BTreeMap kept (own body + descendant use it): {changed:?}"
        );
    }

    // ----- extra name-scan candidates (classes (d)/(e)) -----

    /// A cross-scope generated type ident supplied via [`PruneConfig::extra_candidates`] is pruned
    /// exactly like an allowlisted name when the module family never names it.
    #[test]
    fn extra_candidate_pruned_when_family_unused() {
        let map = files(&[(
            "rust/src/generated/mod.rs",
            "use governance::Voter;\nuse assets::Coin;\n\
             pub struct Foo {\n    pub c: Coin,\n}\n",
        )]);
        let changed = prune_generated_files(&map, &cfg(&["Voter", "Coin"]));
        assert_eq!(changed.len(), 1, "{changed:?}");
        assert!(
            !changed[0].1.contains("Voter"),
            "unused cross-scope Voter pruned: {}",
            changed[0].1
        );
        assert!(
            changed[0].1.contains("Coin"),
            "used Coin kept: {}",
            changed[0].1
        );
    }

    /// A non-candidate name (not allowlisted, not in the extra set) is never touched even when unused —
    /// the soundness floor for names outside the name-scan model (e.g. the `Serialize` trait residue).
    #[test]
    fn non_candidate_unused_import_untouched() {
        let map = files(&[(
            "rust/src/generated/mod.rs",
            "use cbor_event::se::Serialize;\npub struct Foo;\n",
        )]);
        let changed = prune_generated_files(&map, &PruneConfig::default());
        assert!(
            changed.is_empty(),
            "unused non-candidate trait import kept: {changed:?}"
        );
    }

    // ----- drift guard: ERROR_MODULE_EXPORTS vs static/error.rs -----

    /// The error-glob universe must match the actual `static/error.rs` public items exactly. A drift
    /// (a new pub type added to the static error module) would make the universe incomplete and could
    /// under-prune-then-break a consumer, so pin it here against the on-disk source.
    #[test]
    fn error_exports_match_static_source() {
        let src = std::fs::read_to_string("static/error.rs")
            .expect("static/error.rs readable from repo root");
        let file = syn::parse_file(&src).expect("static/error.rs parses");
        let mut pub_items: Vec<String> = Vec::new();
        for item in &file.items {
            let (vis, name) = match item {
                Item::Struct(i) => (&i.vis, Some(i.ident.to_string())),
                Item::Enum(i) => (&i.vis, Some(i.ident.to_string())),
                Item::Type(i) => (&i.vis, Some(i.ident.to_string())),
                Item::Trait(i) => (&i.vis, Some(i.ident.to_string())),
                Item::Fn(i) => (&i.vis, Some(i.sig.ident.to_string())),
                Item::Const(i) => (&i.vis, Some(i.ident.to_string())),
                _ => (&syn::Visibility::Inherited, None),
            };
            if matches!(vis, syn::Visibility::Public(_))
                && let Some(name) = name
            {
                pub_items.push(name);
            }
        }
        pub_items.sort();
        let mut expected: Vec<String> =
            ERROR_MODULE_EXPORTS.iter().map(|s| s.to_string()).collect();
        expected.sort();
        assert_eq!(
            pub_items, expected,
            "ERROR_MODULE_EXPORTS drifted from static/error.rs public items — update the const \
             (the error-glob universe must stay complete or the glob-prune could under-prune)"
        );
    }
}
