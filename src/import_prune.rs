//! Usage-derived pruning of blindly-emitted type imports.
//!
//! Several emission sites in `generation.rs` push a fixed set of collection-type imports into every
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
//! M's private imports. So descendants are the complete set of possible consumers, and protecting F
//! with the union of its own and all descendants' used idents (whether or not each level actually
//! globs `super`) is a conservative superset of the true glob-edge closure. It can only KEEP more
//! imports than exact glob-EDGE tracking would — never remove one a descendant needs — while leaf
//! files (a scope's `serialization.rs` has no descendants) are pruned purely on their own idents.
//! The `/src/` crate split (`rust` / `wasm` / `wasm/json-gen` are separate crates) stays the outer
//! boundary. Residual imprecision (protecting via a descendant that never actually glob-chains to
//! F) is recorded in `tests/TESTING_ROADMAP.md`.
//!
//! **Soundness boundary — why an allowlist.** Ident-scanning can prove a *concrete type* unused: a
//! type can only be used by naming it, so "ident absent from the module family" ⇒ unused. That
//! implication does NOT hold for traits (`use std::io::Write;` is exercised by `w.write_all(..)` —
//! the ident `Write` never appears), macros, or globs (no ident to check). So this pass prunes
//! ONLY the explicit [`ALLOWLIST`] of concrete-type imports — exactly the four blindly-pushed
//! types. Everything else is kept untouched.
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

/// The concrete-type import names this pass is allowed to remove. These are exactly the four types
/// that the emission sites in `generation.rs` push blindly. Extending this list later (e.g.
/// `JsValue`) is a one-line change plus a snapshot re-bless — but every addition must be a concrete
/// type (never a trait/macro/glob), or the soundness argument above breaks.
const ALLOWLIST: &[&str] = &["BTreeMap", "OrderedHashMap", "NonEmptyVec", "NonEmptyMap"];

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
        if let Some(pruned_tree) = prune_tree(&use_item.tree, used) {
            // Tree changed but is non-empty: replace with the filtered use item.
            if !trees_equal(&use_item.tree, &pruned_tree) {
                let mut new_item = use_item.clone();
                new_item.tree = pruned_tree;
                let (start, end) = item_byte_range(use_item, &line_starts, source);
                edits.push((start, end, Some(new_item.to_token_stream().to_string())));
            }
        } else {
            // Tree emptied entirely: drop the whole item, including the line it occupied — plain
            // span deletion would leave a blank-line scar that rustfmt does not collapse.
            let (start, end) = item_byte_range(use_item, &line_starts, source);
            let (start, end) = expand_to_whole_line(source, start, end);
            edits.push((start, end, None));
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
/// > protected(F) = used_idents(F) ∪ ⋃ used_idents(D) for every D in the map whose module is a
/// > strict path-descendant of F's module (same crate — the `/src/` split is the outer boundary)
///
/// and an allowlisted private import of F is removed iff its ident is absent from protected(F).
/// Descendants are the complete set of files that can consume F's private imports (see the module
/// docs), so this is sound; leaf files (a scope's `serialization.rs` — no descendants) are pruned
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
    // Used idents per `.rs` file, computed once; `None` marks an unparseable file.
    let mut used_by_path: BTreeMap<&str, Option<HashSet<String>>> = BTreeMap::new();
    for (path, content) in files {
        if path.ends_with(".rs") {
            used_by_path.insert(path, collect_used_idents_from_source(content));
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
        let mut protected = own_used.clone();
        let mut poisoned = false;
        for (desc_path, desc_used) in &used_by_path {
            if *desc_path == path.as_str()
                || !desc_path.starts_with(&dir)
                || crate_key(desc_path) != key
            {
                continue;
            }
            match desc_used {
                Some(idents) => protected.extend(idents.iter().cloned()),
                // An unparseable descendant might consume ANY of this file's private imports.
                None => {
                    poisoned = true;
                    break;
                }
            }
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
}
