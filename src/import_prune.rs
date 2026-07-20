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
//! **Module family, not single file.** The unit of analysis for a file F is F plus its *descendant
//! modules*, not F alone: a child module's `use super::*;` re-exports the parent module's *private*
//! imports (`serialization.rs` reaches the `BTreeMap` it uses through `use super::*;` from
//! `mod.rs`, even though `mod.rs`'s own body never names `BTreeMap`), so pruning F on its own
//! idents alone breaks the crate (E0433 at the child). The privacy rule bounds who can do this: a
//! private `use` binding in module M is nameable only from M and M's descendants — a glob from a
//! NON-descendant (`use crate::generated::foo::*;` from a sibling) imports only `pub` items, never
//! M's private imports. So descendants are the complete set of possible consumers, and F is
//! protected by the union of its own used idents and each descendant's used idents — but a
//! descendant D contributes an ident it names only when D's resolution of that ident can actually
//! reach F's re-import. Rust resolves to the NEAREST binding, so two disqualifiers drop D as a
//! protector of ident X:
//!   1. **Direct import** — D carries `use …::X;` of its own; D's uses of `X` resolve to that, never
//!      to F's copy reached through `use super::*;`. This sheds a `mod.rs` re-import whose only
//!      descendant consumer holds its own direct copy (the `cbor_encodings.rs` shape).
//!   2. **Target module (A2)** — D's module is AT or UNDER the module F imports X FROM
//!      (`use crate::<segs>::X;` → module `crate::<segs>`). F's import compiling at all requires X in
//!      that module's namespace, and D-at-or-under-it resolves X there (a local item/binding beats
//!      anything glob-chained from an ancestor), so D's chain never reaches F's re-import. This is
//!      what sheds `mod.rs`'s blindly-pushed `use crate::…::serialization::LenEncoding;` /
//!      `StringEncoding` whose consumer is the `serialization.rs` descendant that DEFINES them.
//!
//! Disqualifier 2 is derived from the import PATH rather than by scanning D for a local definition
//! ON PURPOSE: this pass runs over the GENERATED-ONLY file map (`generation/export.rs`'s
//! `generated_files`), and `serialization.rs`'s static prelude — which is where `LenEncoding` /
//! `StringEncoding` are actually defined — is concatenated onto the root file LATER, so the
//! definition is invisible here; the import path is the only in-view evidence of where X lives. The
//! `/src/` crate split (`rust` / `wasm` / `wasm/json-gen` are separate crates) stays the outer
//! boundary; leaf files (a scope's `serialization.rs` has no descendants) are pruned purely on their
//! own idents.
//!
//! **Deliberately-conservative residue (documented, not implemented).** The two disqualifiers can
//! still leave a protector standing and KEEP a would-be-prunable import — never remove one a consumer
//! needs, so any imprecision is warning-severity, never a compile error: (a) an INTERMEDIATE module M
//! between F and a deeper descendant D consumes F's copy for everything at or below M (M directly
//! imports X, or is itself at/under X's target module), yet the rule still counts D — which reaches X
//! through M, not F — as a protector of F; (b) descendants that never actually glob-chain back to F
//! (exact glob-EDGE tracking would drop these). Both are watched by the unused-allowlisted-import
//! scan in the `feature_corpus_compiles` gate (src/tests), which fails on any `unused import` rustc
//! warning naming an [`ALLOWLIST`] ident across the corpus; escalation to exact resolution modelling
//! is gated on a real warning report from that arm.
//!
//! **Soundness boundary — why an allowlist.** Ident-scanning can prove a *concrete type* unused: a
//! type can only be used by naming it, so "ident absent from the module family" ⇒ unused. That
//! implication does NOT hold for traits (`use std::io::Write;` is exercised by `w.write_all(..)` —
//! the ident `Write` never appears), macros, or globs (no ident to check). So this pass prunes
//! ONLY the explicit [`ALLOWLIST`] of concrete-type imports — exactly the blindly-pushed types
//! (the six collection helpers plus the three `--preserve-encodings` encoding enums). Everything
//! else is kept untouched.
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

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{Item, ItemUse, UseTree};

/// The concrete-type import names this pass is allowed to remove. These are exactly the
/// blindly-pushed types that the emission sites in `generation/` add unconditionally (or gated only
/// on spec-global facts): the six collection helpers plus the three `--preserve-encodings` encoding
/// enums. Extending this list later (e.g. `JsValue`) is a one-line change plus a snapshot re-bless —
/// but every addition must be a concrete type (never a trait/macro/glob), or the soundness argument
/// above breaks. All three encoding enums are concrete enums (`static/serialization_preserve.rs`)
/// only ever consumed by being named, so the "ident absent from the module family ⇒ unused"
/// implication holds for them exactly as for the collection types.
///
/// `pub(crate)` so the `feature_corpus_compiles` gate (src/tests) can scan generated-crate rustc
/// stderr for `unused import` warnings naming one of these idents — the warning-severity residue
/// this prune is meant to eliminate — against the single owner rather than a mirrored copy.
pub(crate) const ALLOWLIST: &[&str] = &[
    "BTreeMap",
    "OrderedHashMap",
    "NonEmptyVec",
    "NonEmptyMap",
    "OrderedSet",
    "NonEmptyOrderedSet",
    "LenEncoding",
    "StringEncoding",
    "TagPresenceEncoding",
];

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
pub(crate) fn prune_unused_type_imports_with_used<'a>(
    source: &'a str,
    used: &HashSet<String>,
) -> Cow<'a, str> {
    prune_private_use_items(source, &PruneMode::Allowlist(used))
}

/// Which private `use` items a splice pass targets — the two rules documented on this module.
enum PruneMode<'u> {
    /// Allowlist-scoped: within each private `use`, drop only allowlisted leaves absent from the
    /// supplied protected ident set; every other leaf (traits, globs, renames, non-allowlisted
    /// names) is kept. Non-qualifying files always take this path — byte-identical to the pass's
    /// long-standing behaviour.
    Allowlist(&'u HashSet<String>),
    /// Re-export-only file shape: drop EVERY private `use` item wholesale (allowlist irrelevant).
    /// Only reached for files that qualify via [`is_reexport_only_file`] + the driver's
    /// no-descendant check, where the file-shape soundness argument (module docs) proves nothing
    /// can consume a private import.
    ReexportOnly,
}

/// Shared span-splicing edit loop for both prune rules. Iterates top-level PRIVATE `use` items;
/// non-`use` items and `pub use` re-exports are never edited. Re-emission is targeted byte-range
/// splicing (never whole-file token re-printing), so comments survive byte-for-byte.
fn prune_private_use_items<'a>(source: &'a str, mode: &PruneMode) -> Cow<'a, str> {
    let file = match syn::parse_file(source) {
        Ok(file) => file,
        // Conservative-keep: unparseable input (a generator bug rustfmt will also reject loudly)
        // must not be silently mangled here.
        Err(_) => return Cow::Borrowed(source),
    };

    // Compute the byte-range edits for every top-level PRIVATE `use` item whose tree changes.
    // Non-`use` items and `pub use` re-exports are never edited.
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
        match mode {
            PruneMode::Allowlist(used) => {
                if let Some(pruned_tree) = prune_tree(&use_item.tree, used) {
                    // Tree changed but is non-empty: replace with the filtered use item.
                    if !trees_equal(&use_item.tree, &pruned_tree) {
                        let mut new_item = use_item.clone();
                        new_item.tree = pruned_tree;
                        let (start, end) = item_byte_range(use_item, &line_starts, source);
                        edits.push((start, end, Some(new_item.to_token_stream().to_string())));
                    }
                } else {
                    // Tree emptied entirely: drop the whole item, including the line it occupied —
                    // plain span deletion would leave a blank-line scar rustfmt does not collapse.
                    let (start, end) = item_byte_range(use_item, &line_starts, source);
                    let (start, end) = expand_to_whole_line(source, start, end);
                    edits.push((start, end, None));
                }
            }
            PruneMode::ReexportOnly => {
                // Drop the whole private `use` item (same whole-line deletion as an emptied tree).
                let (start, end) = item_byte_range(use_item, &line_starts, source);
                let (start, end) = expand_to_whole_line(source, start, end);
                edits.push((start, end, None));
            }
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

/// Import prune over the full generated-file map, at module-family precision. For each
/// generated-tree file F (`.rs` under a `/generated/` dir), the PROTECTED ident set is
///
/// > protected(F) = used_idents(F) ∪ ⋃ { X ∈ used_idents(D) : D does not RESOLVE X locally } for
/// > every D in the map whose module is a strict path-descendant of F's module (same crate — the
/// > `/src/` split is the outer boundary)
///
/// and an allowlisted private import of F is removed iff its ident is absent from protected(F). A
/// descendant D "resolves X locally" — and so does NOT protect F's copy — when either D directly
/// imports X (`direct_imports(D)`, see [`collect_directly_imported_idents`]) OR D's module is at/under
/// the module F imports X from (`use crate::<segs>::X;` → module `crate::<segs>`, see
/// [`collect_allowlisted_import_targets`]). Both follow from nearest-binding resolution: a directly-
/// importing D consumes its own copy, and a D at/under X's defining module reaches X there, so neither
/// chains back to F's re-import through `use super::*;`. Together they remove the `--preserve-encodings`
/// `LenEncoding`/`StringEncoding` walls a blindly-imported ancestor `mod.rs` grows: the direct-import
/// disqualifier drops the `cbor_encodings.rs` consumer, the target-module one drops the
/// `serialization.rs` consumer that DEFINES them (the definition is invisible to this generated-only
/// pass — see the module docs). Descendants are the complete set of files that can consume F's private
/// imports, so this is sound; leaf files (a scope's `serialization.rs` — no descendants) are pruned
/// purely on their own idents, which is what removes the per-file unused-import walls.
///
/// Module→dir mapping: `d/mod.rs`'s module dir is `d/`; `d/foo.rs`'s module dir is `d/foo/`.
/// Descendants of F = every `.rs` in the map strictly under F's module dir.
///
/// A file that fails to parse might use anything, so it poisons every file it could be protecting:
/// any F with an unparseable descendant is skipped (F itself is skipped inside the prune fn).
/// Files elsewhere in the crate are unaffected — a non-descendant cannot consume F's privates.
///
/// Returns `(path, pruned_content)` for each file that CHANGED; the content is NOT rustfmt'd (the
/// splice can leave loose spacing), so the caller must rustfmt each returned entry before writing.
pub(crate) fn prune_generated_files(files: &BTreeMap<String, String>) -> Vec<(String, String)> {
    // Used idents per `.rs` file, computed once; `None` marks an unparseable file. `direct_by_path`
    // is each file's DIRECTLY-imported leaf idents (see [`collect_directly_imported_idents`]) — used
    // to decide, per descendant, whether it resolves an ident through its own `use` (consuming its
    // own copy) or through the `use super::*;` chain (consuming the ancestor's).
    let mut used_by_path: BTreeMap<&str, Option<HashSet<String>>> = BTreeMap::new();
    let mut direct_by_path: BTreeMap<&str, HashSet<String>> = BTreeMap::new();
    for (path, content) in files {
        if path.ends_with(".rs") {
            used_by_path.insert(path, collect_used_idents_from_source(content));
            direct_by_path.insert(path, collect_directly_imported_idents(content));
        }
    }

    let mut changed = Vec::new();
    for (path, content) in files {
        if !is_prunable_generated_rs(path) {
            continue;
        }
        let Some(Some(own_used)) = used_by_path.get(path.as_str()) else {
            continue; // this file doesn't parse — the prune fn would refuse anyway
        };
        let dir = module_dir(path);
        let key = crate_key(path);
        // Per-import target module of F's own allowlisted imports (see A2 below). Maps an allowlisted
        // leaf ident X to the crate-relative file base(s) of the module F imports it FROM, when that
        // path is `crate::`-anchored (external/relative imports contribute nothing — no in-crate
        // module to be at/under).
        let import_targets = collect_allowlisted_import_targets(content, key);
        let mut protected = own_used.clone();
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
            match desc_used {
                // A descendant D protects F's import of ident X only when D's resolution of X can
                // actually reach F's re-import — i.e. D neither has its OWN binding of X nor a shorter
                // path to X's definition. Two disqualifiers, both from nearest-binding resolution:
                //   • DIRECT IMPORT: D carries `use …::X;` of its own, so D's uses of X resolve to
                //     that, never to F's copy reached through the `use super::*;` chain.
                //   • TARGET-MODULE (A2): D's module is AT or UNDER the module F imports X FROM
                //     (`use crate::<segs>::X;` → module `crate::<segs>`). F's import compiling at all
                //     requires X to be in that module's namespace, and a descendant at/under it
                //     resolves X there (a local item/binding beats anything glob-chained from above),
                //     so D's chain can never reach F's re-import. This is what the pruner CANNOT see
                //     via local definitions: its view is generated-only (the static prelude that
                //     defines `LenEncoding`/`StringEncoding` in `serialization.rs` concats AFTER this
                //     pass), so the disqualifier is derived from the import PATH, not a scanned def.
                // A disqualified descendant does not contribute X; every other descendant naming X
                // still protects (conservative keep). Over-removal stays loud (E0412/E0433) in the
                // compile gates.
                Some(idents) => {
                    let direct = direct_by_path.get(desc_path).cloned().unwrap_or_default();
                    for id in idents {
                        if direct.contains(id) {
                            continue;
                        }
                        if let Some(bases) = import_targets.get(id)
                            && bases.iter().any(|base| module_at_or_under(desc_path, base))
                        {
                            continue;
                        }
                        protected.insert(id.clone());
                    }
                }
                // An unparseable descendant might consume ANY of this file's private imports.
                None => {
                    poisoned = true;
                    break;
                }
            }
        }
        // Re-export-only file shape (module docs, second rule): no descendant module can consume a
        // private import (`has_descendant` is false, so `poisoned` is too), and the file is all
        // `use` items with only `crate::`-anchored `pub use`s — so every private `use` is provably
        // dead and removed wholesale, regardless of the allowlist. Non-qualifying files fall through
        // to the byte-identical allowlist rule below.
        if !has_descendant && is_reexport_only_file(content) {
            let pruned = prune_private_use_items(content, &PruneMode::ReexportOnly);
            if let Cow::Owned(new_content) = pruned {
                changed.push((path.clone(), new_content));
            }
            continue;
        }
        if poisoned {
            continue;
        }
        let pruned = prune_unused_type_imports_with_used(content, &protected);
        if let Cow::Owned(new_content) = pruned {
            changed.push((path.clone(), new_content));
        }
    }
    changed
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
/// — a `TokenTree::Ident` anywhere in that stream counts as a use. `pub use` items are collected
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

fn collect_idents_in_tokens(tokens: TokenStream, used: &mut HashSet<String>) {
    for tree in tokens {
        match tree {
            TokenTree::Ident(ident) => {
                used.insert(ident.to_string());
            }
            TokenTree::Group(group) => collect_idents_in_tokens(group.stream(), used),
            TokenTree::Punct(_) | TokenTree::Literal(_) => {}
        }
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

/// For F's own PRIVATE allowlisted imports (the only ones this pass can remove), map each allowlisted
/// leaf ident X to the crate-relative FILE BASE(s) of the module F imports X FROM — but only when the
/// import path is `crate::`-anchored. `use crate::a::b::X;` in crate `<crate_root>` yields base
/// `<crate_root>/src/a/b`, whose module is the file `…/a/b.rs` (or `…/a/b/mod.rs`) plus everything
/// under `…/a/b/`. An `std::`/external/`self::`/`super::` path names no in-crate module, so it
/// contributes nothing and the A2 target-module skip never fires for that leaf (behavior unchanged
/// for the collection helpers, which come from `std`/the runtime crate). Combined `use a::{X, Y};`
/// applies the same prefix to each leaf. Returns empty if `source` doesn't parse.
fn collect_allowlisted_import_targets(
    source: &str,
    crate_root: &str,
) -> BTreeMap<String, Vec<String>> {
    let mut targets: BTreeMap<String, Vec<String>> = BTreeMap::new();
    if let Ok(file) = syn::parse_file(source) {
        for item in &file.items {
            if let Item::Use(use_item) = item
                && matches!(use_item.vis, syn::Visibility::Inherited)
            {
                walk_use_targets(&use_item.tree, &mut Vec::new(), crate_root, &mut targets);
            }
        }
    }
    targets
}

fn walk_use_targets(
    tree: &UseTree,
    prefix: &mut Vec<String>,
    crate_root: &str,
    targets: &mut BTreeMap<String, Vec<String>>,
) {
    match tree {
        UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            walk_use_targets(&path.tree, prefix, crate_root, targets);
            prefix.pop();
        }
        UseTree::Group(group) => {
            for item in &group.items {
                walk_use_targets(item, prefix, crate_root, targets);
            }
        }
        UseTree::Name(name) => {
            let ident = name.ident.to_string();
            if is_allowlisted(&ident)
                && let Some(base) = target_base_from_prefix(prefix, crate_root)
            {
                targets.entry(ident).or_default().push(base);
            }
        }
        // A rename binds a different local name (its `as` target, not X), and a glob binds no
        // specific ident — neither is an allowlisted-leaf import path to derive a target from.
        UseTree::Rename(_) | UseTree::Glob(_) => {}
    }
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

/// True iff `file_path` is the module at `target_base` (`{base}.rs`) or a file strictly under its
/// module dir (`{base}/…`, which covers `{base}/mod.rs` and any deeper descendant).
fn module_at_or_under(file_path: &str, target_base: &str) -> bool {
    file_path == format!("{target_base}.rs") || file_path.starts_with(&format!("{target_base}/"))
}

/// Prune allowlisted-and-unused leaves from a `UseTree`. Returns `None` if the whole tree becomes
/// empty (the caller drops the item), otherwise the filtered tree. `Rename` and `Glob` leaves are
/// always kept; a single-element group collapses back to its inner tree (so `use x::{Kept};`
/// renders as `use x::Kept;`).
fn prune_tree(tree: &UseTree, used: &HashSet<String>) -> Option<UseTree> {
    match tree {
        UseTree::Name(name) => {
            let ident = name.ident.to_string();
            if is_allowlisted(&ident) && !used.contains(&ident) {
                None
            } else {
                Some(tree.clone())
            }
        }
        // A rename's binding (`use x::A as B;`) is exercised by the local name `B`, which our
        // ident scan cannot connect back to `A`; a glob has no ident to check. Both kept.
        UseTree::Rename(_) | UseTree::Glob(_) => Some(tree.clone()),
        UseTree::Path(path) => prune_tree(&path.tree, used).map(|inner| {
            let mut new_path = path.clone();
            new_path.tree = Box::new(inner);
            UseTree::Path(new_path)
        }),
        UseTree::Group(group) => {
            let kept: Vec<UseTree> = group
                .items
                .iter()
                .filter_map(|item| prune_tree(item, used))
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
        assert_eq!(
            changed.len(),
            1,
            "foo.rs is protected by foo/child.rs; only other.rs changes: {changed:?}"
        );
        assert_eq!(changed[0].0, "rust/src/generated/other.rs");
        assert!(!changed[0].1.contains("BTreeMap"));
    }

    /// When the allowlisted ident is absent from a file's whole module family, the driver prunes it.
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
        let changed = prune_generated_files(&map);
        assert_eq!(changed.len(), 1, "only mod.rs changes: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/mod.rs");
        assert!(
            !changed[0].1.contains("BTreeMap"),
            "family-unused BTreeMap pruned: {}",
            changed[0].1
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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

    /// A descendant module in the map means `use super::*;` could consume a private import, so the
    /// wholesale rule must NOT fire — only the ordinary allowlist prune applies. With the descendant
    /// naming `BTreeMap`, that prune keeps everything, so the re-export-only file is left unchanged
    /// (its trait/glob imports are NOT removed).
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
        let changed = prune_generated_files(&map);
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/address/mod.rs"),
            "descendant present: wholesale rule must not fire, allowlist keeps everything: {changed:?}"
        );
    }

    /// A single code item (here a `struct`) disqualifies the wholesale rule; the ordinary allowlist
    /// prune still runs, so the trait import and `error::*` glob survive while the unused `BTreeMap`
    /// leaf is dropped from the group.
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
        let changed = prune_generated_files(&map);
        assert_eq!(
            changed.len(),
            1,
            "allowlist prune drops BTreeMap: {changed:?}"
        );
        let out = &changed[0].1;
        assert!(
            out.contains("use std::convert::TryFrom"),
            "trait import NOT removed wholesale (rule did not fire): {out}"
        );
        assert!(out.contains("use cml_core::error::*"), "glob kept: {out}");
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
        assert!(
            changed.is_empty(),
            "descendant names both encodings; nothing pruned: {changed:?}"
        );
    }

    // ----- disqualifier composition: direct-import + target-module skip (A2) -----

    /// Direct-import disqualifier (the `cbor_encodings.rs` shape): a descendant that names
    /// `LenEncoding` AND carries its own `use crate::…::LenEncoding;` resolves to its own copy, so the
    /// ancestor `mod.rs`'s blindly-pushed copy is dead and pruned. (`cbor_encodings.rs` is NOT under
    /// the `serialization` target module, so this isolates the direct-import path.)
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
        let changed = prune_generated_files(&map);
        assert_eq!(changed.len(), 1, "only mod.rs changes: {changed:?}");
        assert_eq!(changed[0].0, "rust/src/generated/mod.rs");
        assert!(
            !changed[0].1.contains("LenEncoding"),
            "direct-import consumer does not protect; ancestor copy pruned: {}",
            changed[0].1
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
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
        let changed = prune_generated_files(&map);
        assert!(
            !changed
                .iter()
                .any(|(p, _)| p == "rust/src/generated/mod.rs"),
            "root copy conservatively kept (a/b.rs protects it under the descendant rule): {changed:?}"
        );
    }
}
