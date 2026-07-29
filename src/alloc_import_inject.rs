//! Alloc-import injection for rust-crate-destined generated files.
//!
//! # Why this exists
//!
//! Emitted rust-crate code leans on the std prelude for `String`, `Vec`, `Box`, `Cow`, `ToString`,
//! `ToOwned`, `format!` and `vec!` — names the **core** prelude does not carry. Once the emitted
//! paths moved to `core::`/`alloc::`, each generated module file needs its own `use alloc::…;`
//! lines and its own `extern crate alloc;`, because `use alloc::…` does not resolve without an
//! `extern crate alloc;` **in scope** and the seed-once crate root cannot deliver one to a crate
//! that already exists.
//!
//! # The mechanism: a usage scan, not a prediction
//!
//! This pass is a **pure function of the final file content** — the same premise
//! [`crate::import_prune`] already holds. It scans each rust-crate-destined file's own tokens
//! against a fixed name→import table and injects exactly the lines the content needs. The
//! alternatives were rejected for concrete reasons, recorded so they are not re-litigated:
//!
//! * *Fully-qualified call forms* (`alloc::string::ToString::to_string(&x)`) change body bytes, so
//!   the bless diff stops being classifiable as paths-and-imports-only.
//! * *Emit-when-used flags threaded from the ~40 emission sites* duplicate knowledge the final
//!   content already carries, and miss future sites silently.
//! * *Unconditional imports plus a prune* cannot cover TRAITS — a trait's use is a method call
//!   whose ident is never the trait name, so it is outside the pruner's name-scan model — and an
//!   unconditional trait import warns as unused in every file that never calls `.to_string()`.
//!   Unused-import warnings fail the `feature_corpus_compiles` gate.
//!
//! # Soundness, and why it cannot fight the pruner
//!
//! Injection and pruning have **disjoint** conditions: this pass adds a line only when the name is
//! used in a form that resolves through the module namespace AND no existing `use` already binds
//! it; the prune removes a line only when the name is unused across the module family. A line can
//! never satisfy both, so recomputing them in either order reaches the same fixed point and no
//! oscillation is possible. That is what lets the injector run after the prune and be recomputed
//! after the comment-preservation overlay.
//!
//! # Scope rules (each one earns its place)
//!
//! * **Path tails never trigger.** An ident preceded by `::` resolves relative to the preceding
//!   segment, so a fully-qualified `alloc::collections::BTreeSet::new()` needs no import; injecting
//!   one would be an unused-import warning. Delegated to [`crate::import_prune::walk_ident_uses`],
//!   which owns that rule for both passes.
//! * **Nested inline `mod X { … }` bodies never trigger.** A file-top `use` does not reach a nested
//!   inline module, so a nested usage must not pull a file-top import in: the import would be
//!   unused at file scope and the nested module would still not resolve. Nested modules that need
//!   alloc names carry their own `use super::alloc::…;` lines by hand (the four `natural_any_cbor_*`
//!   serde adapters in `static/any_cbor_json*.rs` are the only such sites, and their content is
//!   static and unconditional within their fragment, so they cannot hit the duplicate-import hazard
//!   that hand imports elsewhere in `static/` would).
//! * **`use` items never trigger.** An import is not a use of the name it binds.
//! * **The `extern crate alloc;` line is broader on purpose**: it is emitted when the file
//!   references the crate `alloc` at file scope OR through `super::alloc`/`self::alloc` ANYWHERE in
//!   the file, nested modules included — precisely so the hand-written nested imports above have a
//!   file-scope binding to resolve through.
//!
//! # Known limits (documented, not solved)
//!
//! * A name used ONLY inside a consumer's `cddl-codegen:insert` block is the consumer's
//!   responsibility — their block, their imports.
//! * A name used ONLY inside a nested inline module that does not carry its own import will not
//!   resolve. In tool output the only nested inline modules are the hand-carried adapters above and
//!   the `--emit-tests` `#[cfg(test)] mod cddl_generated_tests`, which opens with `use super::*;`
//!   and so inherits the file's own (private) injected bindings.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::{Spacing, TokenStream, TokenTree};
use quote::ToTokens;
use syn::Item;

use crate::import_prune::{IdentForm, walk_ident_uses};

/// One row of the name→import table: what triggers the line, and what the line binds.
struct Row {
    /// A bare ident that triggers this row (`String`, `Vec`, …). `None` for macro-only rows.
    bare: Option<&'static str>,
    /// A macro ident — used as `name!` — that triggers this row (`vec!`, `format!`).
    macro_ident: Option<&'static str>,
    /// A method ident — used as `.name()` — that triggers this row (`.to_string()`).
    method_ident: Option<&'static str>,
    /// The leaf name the injected line binds, for the already-imported / already-defined skips.
    binds: &'static str,
    /// The exact line injected. Owned by this pass: it is removed and recomputed on every run.
    line: &'static str,
}

/// The table. Sorted by `line` so the injected block is deterministic without a sort at use time.
///
/// `ToString`/`ToOwned` keep their bare-ident triggers even though the emitted corpus reaches them
/// ONLY through the method idents (zero bare occurrences corpus-wide): the bare form costs one
/// comparison and covers a future emission site that names the trait directly.
const TABLE: &[Row] = &[
    Row {
        bare: Some("Cow"),
        macro_ident: None,
        method_ident: None,
        binds: "Cow",
        line: "use alloc::borrow::Cow;",
    },
    Row {
        bare: Some("ToOwned"),
        macro_ident: None,
        method_ident: Some("to_owned"),
        binds: "ToOwned",
        line: "use alloc::borrow::ToOwned;",
    },
    Row {
        bare: Some("Box"),
        macro_ident: None,
        method_ident: None,
        binds: "Box",
        line: "use alloc::boxed::Box;",
    },
    Row {
        bare: Some("BTreeMap"),
        macro_ident: None,
        method_ident: None,
        binds: "BTreeMap",
        line: "use alloc::collections::BTreeMap;",
    },
    Row {
        bare: Some("BTreeSet"),
        macro_ident: None,
        method_ident: None,
        binds: "BTreeSet",
        line: "use alloc::collections::BTreeSet;",
    },
    Row {
        bare: None,
        macro_ident: Some("format"),
        method_ident: None,
        binds: "format",
        line: "use alloc::format;",
    },
    Row {
        bare: Some("String"),
        macro_ident: None,
        method_ident: None,
        binds: "String",
        line: "use alloc::string::String;",
    },
    Row {
        bare: Some("ToString"),
        macro_ident: None,
        method_ident: Some("to_string"),
        binds: "ToString",
        line: "use alloc::string::ToString;",
    },
    Row {
        bare: None,
        macro_ident: Some("vec"),
        method_ident: None,
        binds: "vec",
        line: "use alloc::vec;",
    },
    Row {
        bare: Some("Vec"),
        macro_ident: None,
        method_ident: None,
        binds: "Vec",
        line: "use alloc::vec::Vec;",
    },
];

/// The `extern crate` item every injected `use alloc::…;` resolves through.
const EXTERN_CRATE_ALLOC: &str = "extern crate alloc;";

/// True for a `files`-map key whose content is compiled into the generated RUST crate and is
/// tool-owned. Mirrors [`crate::generation::export::is_header_stamped_path`]'s rust half.
///
/// The seed-once crate root `rust/src/lib.rs` is deliberately excluded: it is user-owned after its
/// first write, so injecting into it would either be clobbered or fight a consumer's edits. The
/// wasm and json-gen crates are excluded because they stay `std` — rewriting them would be churn
/// with no consumer.
pub(crate) fn is_rust_crate_destined(path: &str) -> bool {
    path.ends_with(".rs") && path.starts_with("rust/src/generated/")
}

/// Inject into every rust-crate-destined entry of a `files` map, in place, and return the paths
/// this pass actually changed.
///
/// **The caller MUST rustfmt every returned path.** This pass places its block in source order, not
/// in rustfmt's `use`-sort order, so injected content is not rustfmt-stable on its own — and every
/// surface the tool writes has to be rustfmt-stable or the comment-preservation overlay traps
/// comments on the next unchanged regeneration (that invariant is what
/// `comment_preservation_static_files_rustfmt_stable` pins). Returning the paths rather than
/// formatting here keeps this module free of the rustfmt plumbing, which lives in `generation`.
///
/// Idempotent, and safe to re-run after the comment-preservation overlay: [`inject`] removes this
/// pass's own lines before recomputing, so a `cddl-codegen:replace` block that deleted the last
/// user of a name drops its import too, and one that introduced a new user gains it.
pub(crate) fn inject_generated_files(files: &mut BTreeMap<String, String>) -> Vec<String> {
    let mut changed = Vec::new();
    for (path, content) in files.iter_mut() {
        if !is_rust_crate_destined(path) {
            continue;
        }
        if let Cow::Owned(injected) = inject(content) {
            *content = injected;
            changed.push(path.clone());
        }
    }
    changed
}

/// Inject into one rust-crate-destined source. Returns `Borrowed` when nothing changed.
///
/// Callers are responsible for the destination check — the composed runtime statics and the
/// `--export-static-crate` outputs reach this directly rather than through the `files` map, because
/// they are written outside it.
pub(crate) fn inject(source: &str) -> Cow<'_, str> {
    if syn::parse_file(source).is_err() {
        // Unparseable content is never tool output in practice; leaving it untouched keeps this
        // pass from being the thing that turns a generator bug into a confusing edit.
        return Cow::Borrowed(source);
    }

    // STRIP FIRST, then recompute from the stripped content. Doing it in this order is what makes
    // the pass a pure function of the code (rather than of the code plus whatever a previous run
    // left behind) and therefore idempotent AND correct as a post-overlay recompute. Deciding the
    // skip rules against the UNstripped source would be a bug in both directions: a line this pass
    // owns would read as an "already imported" foreign binding and suppress its own re-addition,
    // and an orphaned line would keep justifying its own `extern crate`.
    //
    // Only column-0 exact matches are ours. A hand-written `use super::alloc::…;` inside a nested
    // inline module is indented in rustfmt'd output and is never a candidate.
    let owned: BTreeSet<&'static str> = TABLE
        .iter()
        .map(|row| row.line)
        .chain(std::iter::once(EXTERN_CRATE_ALLOC))
        .collect();
    let kept: Vec<&str> = source
        .lines()
        .filter(|line| !owned.contains(*line))
        .collect();
    let stripped = kept.join("\n");

    let Ok(file) = syn::parse_file(&stripped) else {
        return Cow::Borrowed(source);
    };
    let triggered = triggered_rows(&file);
    let already_imported = crate::import_prune::collect_directly_imported_idents(&stripped);
    let defined_here = crate::import_prune::collect_module_item_defs(&stripped);

    // Skip rule (a): a surviving `use` already binds the name — never double-import (E0252).
    // Skip rule (b): the file DEFINES an item with that name — the file's own item wins.
    let wanted: BTreeSet<&'static str> = TABLE
        .iter()
        .filter(|row| triggered.contains(row.binds))
        .filter(|row| !already_imported.contains(row.binds))
        .filter(|row| !defined_here.contains(row.binds))
        .map(|row| row.line)
        .collect();

    // The `extern crate` line is needed when anything reaches the crate `alloc` — the lines we are
    // about to add included, plus any `super::alloc`/`self::alloc` in a nested module.
    let mut block: Vec<&str> = Vec::new();
    if !wanted.is_empty() || references_alloc_crate(&stripped) {
        block.push(EXTERN_CRATE_ALLOC);
    }
    // `wanted` is a `BTreeSet<&'static str>`, so the block is in a stable sorted order by
    // construction — no sort at use time, and the output is deterministic.
    block.extend(wanted.iter().copied());

    let insert_at = insertion_line(&kept);
    let mut out: Vec<&str> = Vec::with_capacity(kept.len() + block.len());
    out.extend_from_slice(&kept[..insert_at]);
    out.extend_from_slice(&block);
    out.extend_from_slice(&kept[insert_at..]);
    let mut joined = out.join("\n");
    if source.ends_with('\n') && !joined.ends_with('\n') {
        joined.push('\n');
    }
    if joined == source {
        return Cow::Borrowed(source);
    }
    Cow::Owned(joined)
}

/// The names, by `Row::binds`, whose trigger appears in `file` at a scope a file-top import serves.
///
/// Skips `use` items (an import is not a use of the name) and the bodies of nested inline modules
/// (a file-top import does not reach them — see the module docs).
fn triggered_rows(file: &syn::File) -> BTreeSet<&'static str> {
    let mut hit = BTreeSet::new();
    let mut sink = |ident: &str, form: IdentForm| {
        for row in TABLE {
            let matched = match form {
                IdentForm::Bare => row.bare == Some(ident),
                IdentForm::Macro => row.macro_ident == Some(ident),
                IdentForm::Method => row.method_ident == Some(ident),
            };
            if matched {
                hit.insert(row.binds);
            }
        }
    };
    for item in &file.items {
        match item {
            Item::Use(_) => continue,
            Item::Mod(m) if m.content.is_some() => continue,
            _ => walk_ident_uses(item.to_token_stream(), &mut sink),
        }
    }
    hit
}

/// True when anything in `source` names the crate `alloc` in a way that needs a file-scope
/// `extern crate alloc;`: a root-path `alloc::…` (an ident `alloc` NOT preceded by `::`), or a
/// `super::alloc`/`self::alloc` path — the latter being how a nested inline module reaches the
/// file's binding.
///
/// A dedicated walk rather than [`walk_ident_uses`] because the question is about a path's SHAPE,
/// not about which names a module's namespace binds: `super::alloc` is exactly the case
/// `walk_ident_uses` must drop (a path tail) and this must keep. Scans the WHOLE file, nested
/// module bodies and `use` items included, since both can hold such a reference.
fn references_alloc_crate(source: &str) -> bool {
    fn scan(tokens: TokenStream) -> bool {
        let trees: Vec<TokenTree> = tokens.into_iter().collect();
        for (i, tree) in trees.iter().enumerate() {
            match tree {
                TokenTree::Ident(ident) if ident == "alloc" => {
                    let prev1 = i.checked_sub(1).map(|j| &trees[j]);
                    let prev2 = i.checked_sub(2).map(|j| &trees[j]);
                    let after_path_sep = matches!(prev1, Some(TokenTree::Punct(p)) if p.as_char() == ':')
                        && matches!(prev2, Some(TokenTree::Punct(p)) if p.as_char() == ':' && p.spacing() == Spacing::Joint);
                    if !after_path_sep {
                        // Root-path `alloc::…`, or a bare `alloc` — either way the crate is named.
                        return true;
                    }
                    // `<something>::alloc` — only `super`/`self` reach this file's binding.
                    if matches!(i.checked_sub(3).map(|j| &trees[j]),
                                Some(TokenTree::Ident(head)) if head == "super" || head == "self")
                    {
                        return true;
                    }
                }
                TokenTree::Group(group) if scan(group.stream()) => return true,
                _ => {}
            }
        }
        false
    }
    match syn::parse_file(source) {
        Ok(file) => file.items.iter().any(|i| scan(i.to_token_stream())),
        Err(_) => false,
    }
}

/// The line index the injected block goes at: after the leading comment/blank run and after any
/// inner attributes, before the first item.
///
/// Inner attributes (`#![allow(…)]`, which the generated root `mod.rs` opens with) MUST precede
/// every item in their module, so an item injected above one is a hard error. Comments may precede
/// them, which is why the codegen banner can stay at the very top.
///
/// **A reserved `// cddl-codegen:` comment is a HARD STOP.** Those markers are structural: the
/// comment-preservation reader recognizes a fail-loudly block only while its
/// `cddl-codegen:unpreserved-comment` line sits IMMEDIATELY above its `compile_error!(…)`, and a
/// `cddl-codegen:keep` marker likewise claims the run directly below it. Skipping past one would
/// land this pass's block between a marker and the thing it owns, dissolving the structure — the
/// reader then meets a marker it cannot classify and hard-errors on its own emitted output. Placing
/// the block ABOVE such a marker is always safe: it is still after the banner and the inner
/// attributes, which is all the language requires.
fn insertion_line(lines: &[&str]) -> usize {
    let mut i = 0;
    let mut depth = 0i32;
    while i < lines.len() {
        let trimmed = lines[i].trim();
        if depth > 0 {
            depth += bracket_delta(trimmed);
            i += 1;
            continue;
        }
        if trimmed.starts_with("//")
            && trimmed
                .trim_start_matches('/')
                .trim_start()
                .starts_with("cddl-codegen:")
        {
            break;
        }
        if trimmed.is_empty() || trimmed.starts_with("//") {
            i += 1;
            continue;
        }
        if trimmed.starts_with("#!") {
            depth += bracket_delta(trimmed);
            i += 1;
            continue;
        }
        break;
    }
    i
}

/// Net bracket depth change of a line, counting only `[`/`]` (an inner attribute's own delimiters).
/// Brackets inside string literals do not occur in the attribute forms the generator emits.
fn bracket_delta(line: &str) -> i32 {
    line.chars().fold(0i32, |acc, c| match c {
        '[' => acc + 1,
        ']' => acc - 1,
        _ => acc,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Run the pass and return the result as an owned String.
    fn inj(src: &str) -> String {
        inject(src).into_owned()
    }

    /// The lines this pass owns that are present at file scope, in file order.
    fn owned_lines(src: &str) -> Vec<String> {
        let owned: BTreeSet<&str> = TABLE
            .iter()
            .map(|r| r.line)
            .chain(std::iter::once(EXTERN_CRATE_ALLOC))
            .collect();
        src.lines()
            .filter(|l| owned.contains(l))
            .map(|l| l.to_owned())
            .collect()
    }

    #[test]
    fn each_table_row_injects_on_its_own_trigger() {
        let cases = [
            ("pub struct S { f: String }", "use alloc::string::String;"),
            ("pub struct S { f: Vec<u8> }", "use alloc::vec::Vec;"),
            ("pub struct S { f: Box<u8> }", "use alloc::boxed::Box;"),
            (
                "pub fn f() -> Cow<'static, str> { todo!() }",
                "use alloc::borrow::Cow;",
            ),
            (
                "pub struct S { f: BTreeMap<u8, u8> }",
                "use alloc::collections::BTreeMap;",
            ),
            (
                "pub struct S { f: BTreeSet<u8> }",
                "use alloc::collections::BTreeSet;",
            ),
            ("pub fn f() { let _ = vec![1u8]; }", "use alloc::vec;"),
            (
                "pub fn f() { let _ = format!(\"{}\", 1); }",
                "use alloc::format;",
            ),
        ];
        for (src, expected) in cases {
            let out = inj(src);
            assert!(
                out.contains(expected),
                "{src:?} must inject {expected:?}, got:\n{out}"
            );
            assert!(
                out.contains(EXTERN_CRATE_ALLOC),
                "{src:?} must also gain the extern crate line, got:\n{out}"
            );
        }
    }

    #[test]
    fn trait_rows_trigger_on_the_method_ident() {
        // The corpus reaches ToString/ToOwned ONLY this way — zero bare-ident occurrences.
        let out = inj("pub fn f(x: u8) { let _ = x.to_string(); }");
        assert!(
            out.contains("use alloc::string::ToString;"),
            "a `.to_string()` call must pull the trait in:\n{out}"
        );
        let out = inj("pub fn f(x: &u8) { let _ = x.to_owned(); }");
        assert!(
            out.contains("use alloc::borrow::ToOwned;"),
            "a `.to_owned()` call must pull the trait in:\n{out}"
        );
    }

    #[test]
    fn string_literals_and_comments_do_not_trigger() {
        let src = "// String Vec Box vec! format!\n/* BTreeMap BTreeSet Cow */\npub fn f() { let _ = \"String Vec BTreeMap\"; }\n";
        let out = inj(src);
        assert_eq!(
            owned_lines(&out),
            Vec::<String>::new(),
            "comment and string-literal text must not trigger injection:\n{out}"
        );
    }

    #[test]
    fn a_qualified_path_tail_does_not_trigger() {
        // Ruling R1: the fully-qualified form needs no import, and injecting one would be an
        // unused-import warning — which fails the feature_corpus_compiles gate.
        let src = "pub fn f() { let _ = alloc::collections::BTreeSet::<u8>::new(); }\n";
        let out = inj(src);
        assert!(
            !out.contains("use alloc::collections::BTreeSet;"),
            "a path-tail `BTreeSet` must not trigger an import:\n{out}"
        );
        // ...but the file still names the crate `alloc`, so it needs the extern crate line.
        assert!(
            out.contains(EXTERN_CRATE_ALLOC),
            "a root-path alloc reference must still pull the extern crate line:\n{out}"
        );
    }

    #[test]
    fn skips_a_name_an_existing_use_already_binds() {
        let src = "use alloc::string::String;\npub struct S { f: String }\n";
        let out = inj(src);
        assert_eq!(
            out.matches("use alloc::string::String;").count(),
            1,
            "must never double-import (E0252):\n{out}"
        );
        // A DIFFERENT path binding the same leaf also counts — the binding is what matters.
        let src = "use crate::mine::Cow;\npub fn f() -> Cow<'static, str> { todo!() }\n";
        let out = inj(src);
        assert!(
            !out.contains("use alloc::borrow::Cow;"),
            "an existing binding of the leaf must suppress injection:\n{out}"
        );
    }

    #[test]
    fn skips_a_name_the_file_itself_defines() {
        let src = "pub struct Cow;\npub fn f() -> Cow { Cow }\n";
        let out = inj(src);
        assert!(
            !out.contains("use alloc::borrow::Cow;"),
            "the file's own item wins over the table:\n{out}"
        );
    }

    #[test]
    fn dedupes_the_extern_crate_line() {
        // The composed static files can already carry one; a second in the same module is E0259.
        let src = "extern crate alloc;\npub struct S { f: String }\n";
        let out = inj(src);
        assert_eq!(
            out.matches(EXTERN_CRATE_ALLOC).count(),
            1,
            "must not emit a second extern crate alloc (E0259):\n{out}"
        );
    }

    #[test]
    fn a_nested_inline_mod_body_does_not_trigger_a_file_top_import() {
        // Finding-A ruling: a file-top `use` does not reach a nested inline module, so a nested
        // usage must not pull one in — it would be unused at file scope AND still not resolve
        // inside the module.
        let src = "pub mod inner {\n    pub fn f() -> String { todo!() }\n}\n";
        let out = inj(src);
        assert!(
            !out.contains("use alloc::string::String;"),
            "nested-mod usage must not trigger a file-top import:\n{out}"
        );
        assert!(
            !out.contains(EXTERN_CRATE_ALLOC),
            "nothing here names the crate alloc:\n{out}"
        );
    }

    #[test]
    fn a_nested_super_alloc_import_pulls_the_file_top_extern_crate() {
        // The other half of the same ruling: the hand-carried nested imports need a file-scope
        // binding to resolve THROUGH, so `super::alloc` anywhere counts for the extern crate line.
        let src = "pub mod inner {\n    use super::alloc::collections::BTreeMap;\n    pub fn f() -> BTreeMap<u8, u8> { todo!() }\n}\n";
        let out = inj(src);
        assert!(
            out.contains(EXTERN_CRATE_ALLOC),
            "a nested `super::alloc` import must pull the file-top extern crate line:\n{out}"
        );
        assert!(
            !out.contains("use alloc::collections::BTreeMap;"),
            "...but not a file-top import the nested module cannot use:\n{out}"
        );
    }

    #[test]
    fn injects_after_inner_attributes_and_the_header() {
        // An item above an inner attribute is a hard error; the banner is comments, so it stays on
        // top.
        let src = "// This file was code-generated\n// https://example\n\n#![allow(\n    clippy::all,\n    clippy::pedantic\n)]\n\npub struct S { f: String }\n";
        let out = inj(src);
        let lines: Vec<&str> = out.lines().collect();
        let attr_close = lines.iter().position(|l| l.trim() == ")]").unwrap();
        let injected = lines
            .iter()
            .position(|l| *l == "use alloc::string::String;")
            .expect("import injected");
        assert!(
            injected > attr_close,
            "injected items must follow the inner attribute block:\n{out}"
        );
        assert!(lines[0].starts_with("//"), "banner must still lead:\n{out}");
    }

    #[test]
    fn is_idempotent() {
        let src = "pub struct S { f: String, g: Vec<u8> }\n";
        let once = inj(src);
        let twice = inj(&once);
        assert_eq!(once, twice, "a second pass must be a no-op");
    }

    #[test]
    fn recompute_removes_a_line_whose_last_user_went_away() {
        // The post-overlay recompute: a `cddl-codegen:replace` block that deleted the last
        // `.to_string()` must take the trait import with it, or the file warns as unused.
        let injected = inj("pub fn f(x: u8) { let _ = x.to_string(); }");
        assert!(injected.contains("use alloc::string::ToString;"));
        let edited = injected.replace("let _ = x.to_string();", "let _ = x;");
        let recomputed = inj(&edited);
        assert!(
            !recomputed.contains("use alloc::string::ToString;"),
            "the orphaned trait import must be removed on recompute:\n{recomputed}"
        );
        assert!(
            !recomputed.contains(EXTERN_CRATE_ALLOC),
            "and the extern crate line with it, once nothing needs alloc:\n{recomputed}"
        );
    }

    #[test]
    fn recompute_adds_a_line_a_replace_block_introduced() {
        let base = inj("pub fn f() {}\n");
        assert!(!base.contains(EXTERN_CRATE_ALLOC));
        let edited = base.replace(
            "pub fn f() {}",
            "pub fn f() { let _ = format!(\"{}\", 1); }",
        );
        let recomputed = inj(&edited);
        assert!(
            recomputed.contains("use alloc::format;") && recomputed.contains(EXTERN_CRATE_ALLOC),
            "a newly-introduced user must gain its import:\n{recomputed}"
        );
    }

    #[test]
    fn only_rust_crate_generated_keys_are_destined() {
        for yes in [
            "rust/src/generated/mod.rs",
            "rust/src/generated/serialization.rs",
            "rust/src/generated/a/bar/mod.rs",
        ] {
            assert!(is_rust_crate_destined(yes), "{yes} must be injected into");
        }
        for no in [
            "rust/src/lib.rs",                    // seed-once root: user-owned
            "wasm/src/generated/mod.rs",          // wasm stays std
            "wasm/json-gen/src/generated/mod.rs", // json-gen stays std
            "wasm/json-gen/src/main.rs",
            "rust/Cargo.toml",
            "rust/src/generated/notrust.txt",
        ] {
            assert!(!is_rust_crate_destined(no), "{no} must be left alone");
        }
    }

    #[test]
    fn map_injection_touches_only_rust_crate_keys() {
        let body = "pub struct S { f: String }\n".to_owned();
        let mut files = BTreeMap::from([
            ("rust/src/generated/mod.rs".to_owned(), body.clone()),
            ("rust/src/lib.rs".to_owned(), body.clone()),
            ("wasm/src/generated/mod.rs".to_owned(), body.clone()),
            (
                "wasm/json-gen/src/generated/mod.rs".to_owned(),
                body.clone(),
            ),
        ]);
        inject_generated_files(&mut files);
        assert!(files["rust/src/generated/mod.rs"].contains("use alloc::string::String;"));
        for untouched in [
            "rust/src/lib.rs",
            "wasm/src/generated/mod.rs",
            "wasm/json-gen/src/generated/mod.rs",
        ] {
            assert_eq!(files[untouched], body, "{untouched} must be byte-identical");
        }
    }

    #[test]
    fn injected_block_order_is_deterministic() {
        let src = "pub struct S { a: Vec<u8>, b: String, c: BTreeMap<u8, u8>, d: Box<u8> }\n";
        let out = inj(src);
        let got = owned_lines(&out);
        assert_eq!(
            got,
            vec![
                EXTERN_CRATE_ALLOC.to_owned(),
                "use alloc::boxed::Box;".to_owned(),
                "use alloc::collections::BTreeMap;".to_owned(),
                "use alloc::string::String;".to_owned(),
                "use alloc::vec::Vec;".to_owned(),
            ],
            "extern crate first, then the use lines in sorted order:\n{out}"
        );
    }

    /// A reserved `// cddl-codegen:` marker is a HARD STOP for placement: the injected block goes
    /// ABOVE it, never between the marker and the thing it owns.
    ///
    /// This is structural, not cosmetic. `comment_preserve`'s reader recognizes a fail-loudly block
    /// only while its `unpreserved-comment` line sits IMMEDIATELY above its `compile_error!(…)`;
    /// splitting them dissolves the structure, and the reader then meets a marker it cannot classify
    /// and hard-errors on the tool's OWN emitted output (the live failure this pins). A `keep` marker
    /// claims the comment run directly below it, and `insert-start`/`replace-start` open user blocks
    /// — all the same class.
    #[test]
    fn a_reserved_marker_is_a_hard_stop_for_placement() {
        let cases = [
            (
                "unpreserved-comment",
                "// cddl-codegen:unpreserved-comment (delete this block after review)\ncompile_error!(\"stale\");\npub struct S { f: String }\n",
            ),
            (
                "keep",
                "// cddl-codegen:keep\n/// user doc\npub struct S { f: String }\n",
            ),
            (
                "insert-start",
                "// cddl-codegen:insert-start\npub fn mine() {}\n// cddl-codegen:insert-end\npub struct S { f: String }\n",
            ),
            (
                "replace-start",
                "// cddl-codegen:replace-start\npub struct S { f: String }\n// cddl-codegen:replaces\n// pub struct S;\n// cddl-codegen:replace-end\n",
            ),
        ];
        for (label, src) in cases {
            let out = inj(src);
            let lines: Vec<&str> = out.lines().collect();
            let marker = lines
                .iter()
                .position(|l| l.trim_start().starts_with("// cddl-codegen:"))
                .unwrap_or_else(|| panic!("{label}: marker vanished:\n{out}"));
            let injected = lines
                .iter()
                .position(|l| *l == "extern crate alloc;")
                .unwrap_or_else(|| panic!("{label}: nothing injected:\n{out}"));
            assert!(
                injected < marker,
                "{label}: the injected block must sit ABOVE the reserved marker, not inside the \
                 structure it owns:\n{out}"
            );
            // The user/tool block itself travels untouched: same lines, same order, nothing dropped.
            for original in src.lines() {
                assert!(
                    out.lines().any(|l| l == original),
                    "{label}: line {original:?} was disturbed:\n{out}"
                );
            }
        }
    }

    /// The other half of the hard-stop rule: an ORDINARY leading comment (the codegen banner) is
    /// still skipped, so the banner keeps leading the file and the block lands below it.
    #[test]
    fn an_ordinary_leading_comment_is_not_a_hard_stop() {
        let src = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n\npub struct S { f: String }\n";
        let out = inj(src);
        let lines: Vec<&str> = out.lines().collect();
        assert!(lines[0].starts_with("// This file was code-generated"));
        let injected = lines
            .iter()
            .position(|l| *l == "extern crate alloc;")
            .unwrap();
        assert!(injected > 1, "block must land below the banner:\n{out}");
    }

    #[test]
    fn unparseable_source_is_left_alone() {
        let src = "pub struct { this is not rust\n";
        assert_eq!(inj(src), src);
    }
}
