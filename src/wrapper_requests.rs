//! Strict parser for a consumer's committed `wasm/src/generated/borrowed_collections.rs` sidecar —
//! the machine half a workspace **dependency** re-reads via `--wrapper-requests <consumer>=<path>`
//! (W2 of the workspace wrapper-placement feature). The consumer emits this file (W1,
//! `generation.rs`); the dep parses it here, unions the requested shapes across consumers, and hosts
//! every requested wrapper in its own `requested_collections.rs`.
//!
//! ## Why strict
//!
//! A request channel must never silently tolerate stray content: a hand-edited or drifted sidecar
//! that the dep quietly ignored would drop a borrow, and the consumer would then fail to link with
//! no actionable pointer. So the ONLY content this parser accepts is exactly what the frozen W1
//! emitter produces (`generation.rs`, the `borrowed_collections.rs` block):
//!
//! - the tool's two-line codegen header stamp and the four fixed sidecar banner comment lines
//!   (the fourth is the column legend; any OTHER `//` comment — including one inside the const
//!   body, where the legend used to live and where an anchored comment traps on row deletion — is
//!   a hard error);
//! - `#[allow(unused_imports)]` / `#[allow(dead_code)]`;
//! - `mod borrowed { <use lines> }` (or the empty `mod borrowed {}`);
//! - `pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[ <rows> ];`, each row a
//!   `("<dep>", "<name>", "<shape>")` triple;
//! - the edit-preservation overlay's user blocks (`// cddl-codegen:insert-start/end` and
//!   `replace-start`/`replaces`/`replace-end`, `comment_preserve.rs`) whose payload rows conform to
//!   the row grammar — the recorded-original `//` lines under a `replaces` section are skipped as
//!   part of the block structure.
//!
//! A `// cddl-codegen:unpreserved-comment` sentinel or any `compile_error!` is a HARD ERROR: those
//! mark a trapped/drifted sidecar, which must never be silently consumed. Any other content —
//! unknown comment, unknown item, stray token, mangled tuple — is likewise a hard error naming the
//! file and the offending content.
//!
//! ## Line-wrapping tolerance
//!
//! The const table's rows are tokenized (not matched line-by-line), so `rustfmt` wrapping a long
//! row across several lines is accepted; only the GRAMMAR is strict (a tuple is exactly three string
//! literals). Entries addressed to OTHER deps (dep column ≠ the reading dep's normalized `--lib-name`)
//! are filtered by the caller, not here — this parser returns every well-formed entry.

use crate::cli::Cli;
use crate::comment_ast::DemandSet;
use crate::intermediate::{AliasIdent, CDDLIdent, IntermediateTypes, RustIdent};

/// One row of a consumer's `BORROWED_SHAPES` table: a collection wrapper the consumer borrows from a
/// workspace dep. `dep` is the dep's rust-crate name as the consumer knows it (the extern-deps
/// directory name); `name` is the structural wrapper class name; `shape` is the canonical CDDL
/// shape fragment (`[* idx_foo]`, `{* uint => idx_foo}`, `[+ idx_foo]`, nested `[* [* idx_foo]]`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrapperRequestEntry {
    pub dep: String,
    pub name: String,
    pub shape: String,
}

/// The reserved own-line-comment namespace shared with `comment_preserve.rs`. Every
/// `// cddl-codegen:<tag>` is either a well-formed known overlay structure or a hard error.
const CDDL_NAMESPACE: &str = "cddl-codegen:";

/// The exact comment lines the W1 emitter writes (header stamp, four-line sidecar banner — the
/// fourth banner line is the column legend, kept OUT of the const body so the preservation overlay
/// anchors it to the file rather than to a deletable row). Anything else on a `//` own-line comment
/// (outside the `cddl-codegen:` overlay namespace) is a hard error — a drifted/hand-edited banner
/// must be loud, never silently consumed.
const KNOWN_COMMENTS: &[&str] = &[
    "// This file was code-generated using an experimental CDDL to rust tool:",
    "// https://github.com/dcSpark/cddl-codegen",
    "// This file records every collection wrapper this crate borrows from workspace deps.",
    "// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled",
    "// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.",
    "// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).",
];

/// Parse a committed `borrowed_collections.rs` sidecar into its `BORROWED_SHAPES` entries. `file` is
/// the on-disk path, used only to make the hard-error messages actionable. Panics (hard error) on
/// any content outside the frozen W1 grammar; returns every well-formed entry (the caller filters by
/// dep).
pub fn parse_sidecar(contents: &str, file: &str) -> Vec<WrapperRequestEntry> {
    // A `compile_error!` anywhere is the surest sign of a trapped sidecar (the preservation overlay
    // emits one inside its `unpreserved-comment` blocks). Reject before any structural parse.
    if contents.contains("compile_error!") {
        panic!(
            "--wrapper-requests {file}: the sidecar contains a `compile_error!` — it is a trapped or \
             drifted generated file (an edit-preservation `unpreserved-comment` block), which must \
             never be silently consumed. Regenerate the consumer crate to clear it."
        );
    }

    let logical = flatten_overlay_blocks(contents, file, "--wrapper-requests");

    let mut entries = Vec::new();
    let mut in_mod = false;
    let mut in_const = false;
    let mut const_item = String::new();

    for line in &logical {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if in_const {
            // Accumulate the raw const item until the line carrying its closing `];`, then parse the
            // whole item (so any rustfmt layout — wrapped rows, a wrapped initializer — is handled).
            // A `//` comment inside the const body is a hard error (overlay scaffolding was already
            // stripped by `flatten_overlay_blocks`): the emitter writes none — the column legend
            // lives in the banner precisely because an in-const comment anchors to a deletable row
            // and traps on an in-place regen — so any comment here is either a stale old-format
            // sidecar or a stray hand edit.
            if trimmed.starts_with("//") {
                hard_error(file, "unexpected comment inside `BORROWED_SHAPES`", trimmed);
            }
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(parse_const_item(&const_item, file));
                in_const = false;
                const_item.clear();
            }
            continue;
        }
        if in_mod {
            if trimmed == "}" {
                in_mod = false;
            } else if trimmed.starts_with("use ") && trimmed.ends_with(';') {
                // The compile-checked existence half; the dep validates via shapes, so the `use`
                // lines are only checked for well-formedness, never cross-referenced here.
            } else {
                hard_error(file, "unexpected line inside `mod borrowed`", trimmed);
            }
            continue;
        }
        // Top level.
        if KNOWN_COMMENTS.contains(&trimmed) {
            continue;
        }
        if trimmed.starts_with("//") {
            hard_error(file, "unexpected comment", trimmed);
        }
        if trimmed == "#[allow(unused_imports)]" || trimmed == "#[allow(dead_code)]" {
            continue;
        }
        // The `mod borrowed` block — either the empty single-line `mod borrowed {}` or the opening
        // `mod borrowed {` of a multi-line block.
        if trimmed == "mod borrowed {}" {
            continue;
        }
        if trimmed == "mod borrowed {" {
            in_mod = true;
            continue;
        }
        // The const table, accumulated as a whole item then parsed. `rustfmt` lays it out by size:
        // an empty table collapses whole onto one line (`… = &[];`), a single short row may collapse
        // onto a wrapped initializer (`… =\n    &[(…)];`), and a longer table keeps the `= &[`
        // opener with one row per line — parse_const_item handles all of them uniformly.
        if trimmed.starts_with("pub(crate) const BORROWED_SHAPES") {
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(parse_const_item(&const_item, file));
                const_item.clear();
            } else {
                in_const = true;
            }
            continue;
        }
        hard_error(file, "unexpected item", trimmed);
    }

    if in_mod {
        hard_error(file, "unterminated `mod borrowed` block", "");
    }
    if in_const {
        hard_error(
            file,
            "unterminated `BORROWED_SHAPES` table (missing `];`)",
            "",
        );
    }

    entries
}

/// Parse the complete `BORROWED_SHAPES` const item (header, `=`, `&[ … ];`) into entries. The
/// header up to the initializer `=` must be exactly the frozen declaration (whitespace-normalized —
/// rustfmt may wrap the initializer onto the next line); the initializer must be a `&[ … ]` array
/// expression. The first `=` in the item IS the initializer's: the type annotation contains none,
/// and shape strings (which can contain `=>`) only occur after it.
fn parse_const_item(item: &str, file: &str) -> Vec<WrapperRequestEntry> {
    let Some(eq) = item.find('=') else {
        hard_error(file, "malformed `BORROWED_SHAPES` item (missing `=`)", item);
    };
    let header: String = item[..eq].split_whitespace().collect::<Vec<_>>().join(" ");
    if header != "pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)]" {
        hard_error(
            file,
            "unexpected `BORROWED_SHAPES` declaration (the type must be exactly `&[(&str, &str, &str)]`)",
            &header,
        );
    }
    let init = item[eq + 1..].trim();
    let Some(body) = init
        .strip_prefix("&[")
        .and_then(|rest| rest.trim_end().strip_suffix("];"))
    else {
        hard_error(
            file,
            "malformed `BORROWED_SHAPES` initializer (expected `&[ … ];`)",
            init,
        );
    };
    parse_const_body(body, file)
}

/// Strip the edit-preservation overlay scaffolding (`comment_preserve.rs` marker structures) to a
/// flat list of logical lines the grammar scanner consumes. Insert blocks contribute their inner
/// lines verbatim (real payload rows); replace blocks contribute the USER section (before
/// `:replaces`) and drop the `//`-commented recorded original (the `:replaces` section). An
/// `unpreserved-comment` sentinel, or any unrecognized `cddl-codegen:` tag, is a hard error.
fn flatten_overlay_blocks(contents: &str, file: &str, flag: &str) -> Vec<String> {
    // Overlay state: whether we are inside a `replaces` section (recorded originals to drop).
    let mut in_replaces_original = false;
    let mut out = Vec::new();
    for raw in contents.lines() {
        if let Some(tag) = reserved_tag(raw) {
            match tag {
                "insert-start" | "insert-end" | "replace-start" | "replace-end" => {
                    // Scaffolding lines: dropped. `replace-end` also closes any originals section.
                    if tag == "replace-end" {
                        in_replaces_original = false;
                    }
                    continue;
                }
                "replaces" => {
                    in_replaces_original = true;
                    continue;
                }
                "unpreserved-comment" => {
                    panic!(
                        "{flag} {file}: the sidecar contains a \
                         `// cddl-codegen:unpreserved-comment` sentinel — it is a trapped or drifted \
                         generated file, which must never be silently consumed. Regenerate the \
                         consumer crate to clear it."
                    );
                }
                other => {
                    panic!(
                        "{flag} {file}: unexpected reserved comment \
                         `// cddl-codegen:{other}` in the sidecar."
                    );
                }
            }
        }
        if in_replaces_original {
            // Recorded-original lines under a `:replaces` marker are `//`-commented and skipped as
            // part of the block structure (they are not user payload).
            continue;
        }
        out.push(raw.to_string());
    }
    out
}

/// The reserved `cddl-codegen:` tag on an own-line comment, if any — the text after
/// `// cddl-codegen:` (whitespace-trimmed). Mirrors `comment_preserve.rs`'s recognizer so the two
/// stay in lockstep on what counts as an overlay marker.
fn reserved_tag(line: &str) -> Option<&str> {
    let t = line.trim();
    t.strip_prefix("//")
        .map(str::trim_start)
        .and_then(|rest| rest.strip_prefix(CDDL_NAMESPACE))
        .map(str::trim)
}

/// Tokenize the raw `BORROWED_SHAPES` array body (everything between `= &[` and `];`) into entries.
/// Strict tuple grammar: `( "<dep>" , "<name>" , "<shape>" )` with an optional trailing comma,
/// tuples separated by commas. Comments never reach here (own-line ones hard-error in the caller;
/// a trailing `// …` after a row surfaces as an unexpected token below). Any deviation — a
/// non-triple tuple, an unterminated literal, a stray token — is a hard error (a mangled sidecar
/// must be loud).
fn parse_const_body(body: &str, file: &str) -> Vec<WrapperRequestEntry> {
    let chars: Vec<char> = body.chars().collect();
    let mut i = 0;
    let mut entries = Vec::new();
    loop {
        i = skip_trivia(&chars, i);
        if i >= chars.len() {
            break;
        }
        if chars[i] != '(' {
            hard_error(
                file,
                "unexpected token in BORROWED_SHAPES (expected a `(...)` row)",
                &tail_snippet(&chars, i),
            );
        }
        i += 1; // consume '('
        let mut fields = Vec::new();
        loop {
            i = skip_trivia(&chars, i);
            if i < chars.len() && chars[i] == ')' {
                i += 1; // consume ')'
                break;
            }
            if i >= chars.len() || chars[i] != '"' {
                hard_error(
                    file,
                    "malformed BORROWED_SHAPES row (expected a string literal)",
                    &tail_snippet(&chars, i),
                );
            }
            let (s, next) = parse_string_literal(&chars, i, file);
            fields.push(s);
            i = next;
            i = skip_trivia(&chars, i);
            if i < chars.len() && chars[i] == ',' {
                i += 1; // field separator (or trailing comma before ')')
            } else if i < chars.len() && chars[i] == ')' {
                i += 1; // consume ')'
                break;
            } else {
                hard_error(
                    file,
                    "malformed BORROWED_SHAPES row (expected `,` or `)`)",
                    &tail_snippet(&chars, i),
                );
            }
        }
        if fields.len() != 3 {
            hard_error(
                file,
                "malformed BORROWED_SHAPES row (a row must be exactly three string literals: dep, name, shape)",
                &format!("{fields:?}"),
            );
        }
        entries.push(WrapperRequestEntry {
            dep: fields[0].clone(),
            name: fields[1].clone(),
            shape: fields[2].clone(),
        });
        // Optional comma between rows.
        i = skip_trivia(&chars, i);
        if i < chars.len() && chars[i] == ',' {
            i += 1;
        }
    }
    entries
}

/// Advance past whitespace only. Deliberately does NOT skip `//` comments: the emitter writes none
/// inside the const body (the column legend lives in the banner), so a comment reaching the
/// tokenizer is stray content that must surface as an unexpected-token hard error.
fn skip_trivia(chars: &[char], mut i: usize) -> usize {
    while i < chars.len() && chars[i].is_whitespace() {
        i += 1;
    }
    i
}

/// Parse a Rust string literal starting at `chars[start]` (which must be `"`), returning the
/// unescaped contents and the index just past the closing quote. Handles the escapes the emitter's
/// `{:?}` formatting can produce (`\"`, `\\`, `\n`, `\t`, `\r`, `\0`); an unterminated literal is a
/// hard error.
fn parse_string_literal(chars: &[char], start: usize, file: &str) -> (String, usize) {
    debug_assert_eq!(chars[start], '"');
    let mut i = start + 1;
    let mut s = String::new();
    while i < chars.len() {
        let c = chars[i];
        if c == '"' {
            return (s, i + 1);
        }
        if c == '\\' {
            i += 1;
            if i >= chars.len() {
                break;
            }
            match chars[i] {
                '"' => s.push('"'),
                '\\' => s.push('\\'),
                'n' => s.push('\n'),
                't' => s.push('\t'),
                'r' => s.push('\r'),
                '0' => s.push('\0'),
                other => s.push(other),
            }
            i += 1;
            continue;
        }
        s.push(c);
        i += 1;
    }
    hard_error(file, "unterminated string literal in BORROWED_SHAPES", "")
}

/// A short forward snippet of the remaining input, for error messages.
fn tail_snippet(chars: &[char], i: usize) -> String {
    let end = (i + 40).min(chars.len());
    chars[i..end].iter().collect::<String>()
}

fn hard_error(file: &str, what: &str, offending: &str) -> ! {
    if offending.is_empty() {
        panic!(
            "--wrapper-requests {file}: {what}. The sidecar must be an unmodified, tool-generated `borrowed_collections.rs`."
        );
    }
    panic!(
        "--wrapper-requests {file}: {what}: {offending:?}. The sidecar must be an unmodified, \
         tool-generated `borrowed_collections.rs`."
    );
}

// ===== pre-finalize seeding of `used_as_key` from `--wrapper-requests` map shapes ============
//
// A requested map wrapper `{* dep_key => v}` keyed on a dep struct the dep never keys itself compiles
// to `OrderedHashMap<DepKey, V>` (or `BTreeMap` without --preserve-encodings), whose bounds require
// `DepKey: Ord`/`Hash`. Unless the dep DERIVES those, the requested wrapper (and the consumer struct
// holding it) fail to build with E0277. So BEFORE `finalize` computes the key-derive set, the dep
// marks the key idents of every requested map shape as `used_as_key`; finalize then expands them
// transitively through the structs' private fields (the consumer cannot see inside extern types, so
// this dep-side expansion is the whole point).
//
// This pass is LENIENT: a sidecar it cannot scan seeds nothing rather than hard-erroring, so
// `emit_requested_collections` (post-finalize, wasm-only) stays the single owner of the strict W2
// diagnostics — no error fires twice or inconsistently, and a `--wasm=false` run over a trapped
// sidecar is not newly broken by this pass.

/// Seed `used_as_key` from the map KEYS of every requested shape addressed to THIS dep. No-op (and
/// byte-identical to today) when there are no `--wrapper-requests` flags. Lenient throughout — an
/// unreadable path, a structurally odd sidecar, or an unparseable shape simply contributes no seed.
pub fn seed_used_as_key_from_wrapper_requests(types: &mut IntermediateTypes, cli: &Cli) {
    let request_files = cli.wrapper_requests();
    if request_files.is_empty() {
        return;
    }
    let my_lib = cli.lib_name_code();
    let mut to_mark: std::collections::BTreeSet<RustIdent> = std::collections::BTreeSet::new();
    for path in request_files.values() {
        let Ok(contents) = std::fs::read_to_string(path) else {
            continue;
        };
        for (dep, shape) in scan_shape_rows_lenient(&contents) {
            if dep.replace('-', "_") != my_lib {
                continue;
            }
            for ident in map_key_cddl_idents(&shape) {
                // A primitive / reserved key leaf (`{* uint => …}`) is never a dep struct and would
                // panic `RustIdent::new` (reserved-keyword assert), so skip it — only named dep types
                // can carry key derives anyway.
                if crate::intermediate::reserved_ident_rejection(&ident).is_some() {
                    continue;
                }
                to_mark.insert(RustIdent::new(CDDLIdent::new(ident)));
            }
        }
    }
    for ident in to_mark {
        // A wrapper-requested map shape is an internal CBOR map key: it demands today's `bare` internal
        // bundle, exactly as an in-spec `{* k => v}` key would.
        types.mark_key_demand(
            ident,
            crate::comment_ast::DemandSet {
                bare: true,
                hash: false,
                ord: false,
            },
        );
    }
}

/// Tolerant scan of a `borrowed_collections.rs` sidecar for its `BORROWED_SHAPES` rows, returning
/// `(dep, shape)` for each 3-literal `("dep", "name", "shape")` tuple. Deliberately NOT the strict
/// grammar (`parse_sidecar`): `//` comment lines are dropped first (so an edit-preservation overlay's
/// `:replaces` recorded-original rows never seed), then the remaining text after the `BORROWED_SHAPES`
/// marker is tokenized into parenthesized string-literal groups. Never panics — malformed content
/// yields fewer rows, leaving the strict diagnosis to `emit_requested_collections`.
fn scan_shape_rows_lenient(contents: &str) -> Vec<(String, String)> {
    let mut body = String::new();
    let mut seen_marker = false;
    for line in contents.lines() {
        let t = line.trim();
        if t.starts_with("//") {
            continue;
        }
        if !seen_marker {
            if t.contains("BORROWED_SHAPES") {
                seen_marker = true;
            } else {
                continue;
            }
        }
        body.push_str(line);
        body.push('\n');
    }
    if !seen_marker {
        return Vec::new();
    }
    let chars: Vec<char> = body.chars().collect();
    let mut i = 0;
    let mut rows = Vec::new();
    while i < chars.len() {
        if chars[i] != '(' {
            i += 1;
            continue;
        }
        i += 1;
        let mut literals = Vec::new();
        loop {
            while i < chars.len() && chars[i].is_whitespace() {
                i += 1;
            }
            match chars.get(i) {
                Some('"') => {
                    let Some((s, next)) = read_str_lenient(&chars, i) else {
                        return rows;
                    };
                    literals.push(s);
                    i = next;
                }
                Some(',') => i += 1,
                Some(')') | None => {
                    i += 1;
                    break;
                }
                _ => i += 1,
            }
        }
        if literals.len() == 3 {
            rows.push((literals[0].clone(), literals[2].clone()));
        }
    }
    rows
}

/// Read a `"…"` literal at `chars[start]` leniently (`\"`, `\\`, `\n`, `\t`, `\r`, `\0` unescaped),
/// returning the contents and the index past the closing quote, or `None` if unterminated.
fn read_str_lenient(chars: &[char], start: usize) -> Option<(String, usize)> {
    let mut i = start + 1;
    let mut s = String::new();
    while i < chars.len() {
        match chars[i] {
            '"' => return Some((s, i + 1)),
            '\\' => {
                i += 1;
                match chars.get(i)? {
                    '"' => s.push('"'),
                    '\\' => s.push('\\'),
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    'r' => s.push('\r'),
                    '0' => s.push('\0'),
                    other => s.push(*other),
                }
                i += 1;
            }
            c => {
                s.push(c);
                i += 1;
            }
        }
    }
    None
}

/// The CDDL idents sitting in a MAP-KEY position anywhere in a wrapper shape string (canonical
/// renderer output, e.g. `{* idx_foo => uint}`, `[* {* a => b}]`). Every named leaf of a map's KEY
/// subtree is a key ident; the parse descends through nested collections in both key and value. A
/// shape it cannot parse yields no idents (lenient). Primitive leaves (`uint`, `text`, …) are kept as
/// idents but harmlessly resolve to no struct when marked, so no primitive filter is needed here.
pub fn map_key_cddl_idents(shape: &str) -> Vec<String> {
    let chars: Vec<char> = shape.chars().collect();
    let mut pos = 0;
    let mut out = Vec::new();
    if let Some(node) = parse_shape_node(&chars, &mut pos) {
        collect_map_key_idents(&node, &mut out);
    }
    out
}

/// A `types`-free view of a wrapper shape, just enough to locate map-key idents pre-finalize.
enum ShapeNode {
    List(Box<ShapeNode>),
    Map(Box<ShapeNode>, Box<ShapeNode>),
    Named(String),
}

fn parse_shape_node(chars: &[char], pos: &mut usize) -> Option<ShapeNode> {
    let skip_ws = |pos: &mut usize| {
        while *pos < chars.len() && chars[*pos].is_whitespace() {
            *pos += 1;
        }
    };
    skip_ws(pos);
    match chars.get(*pos)? {
        '[' => {
            *pos += 1;
            skip_ws(pos);
            // occurrence marker `*` / `+`
            if !matches!(chars.get(*pos), Some('*') | Some('+')) {
                return None;
            }
            *pos += 1;
            let inner = parse_shape_node(chars, pos)?;
            skip_ws(pos);
            if chars.get(*pos) != Some(&']') {
                return None;
            }
            *pos += 1;
            Some(ShapeNode::List(Box::new(inner)))
        }
        '{' => {
            *pos += 1;
            skip_ws(pos);
            if !matches!(chars.get(*pos), Some('*') | Some('+')) {
                return None;
            }
            *pos += 1;
            let key = parse_shape_node(chars, pos)?;
            skip_ws(pos);
            if chars.get(*pos) != Some(&'=') || chars.get(*pos + 1) != Some(&'>') {
                return None;
            }
            *pos += 2;
            let value = parse_shape_node(chars, pos)?;
            skip_ws(pos);
            if chars.get(*pos) != Some(&'}') {
                return None;
            }
            *pos += 1;
            Some(ShapeNode::Map(Box::new(key), Box::new(value)))
        }
        _ => {
            // A named / primitive leaf: identifier chars.
            let start = *pos;
            while let Some(c) = chars.get(*pos) {
                if c.is_alphanumeric() || *c == '_' || *c == '-' {
                    *pos += 1;
                } else {
                    break;
                }
            }
            if *pos == start {
                return None;
            }
            Some(ShapeNode::Named(chars[start..*pos].iter().collect()))
        }
    }
}

fn collect_map_key_idents(node: &ShapeNode, out: &mut Vec<String>) {
    match node {
        ShapeNode::List(inner) => collect_map_key_idents(inner, out),
        ShapeNode::Map(key, value) => {
            collect_all_named(key, out);
            // A nested map inside the KEY (rare) still contributes its own key idents; the value may
            // nest further maps whose keys must also seed.
            collect_map_key_idents(key, out);
            collect_map_key_idents(value, out);
        }
        ShapeNode::Named(_) => {}
    }
}

fn collect_all_named(node: &ShapeNode, out: &mut Vec<String>) {
    match node {
        ShapeNode::List(inner) => collect_all_named(inner, out),
        ShapeNode::Map(key, value) => {
            collect_all_named(key, out);
            collect_all_named(value, out);
        }
        ShapeNode::Named(name) => out.push(name.clone()),
    }
}

// ===== `--key-requests` — the in-workspace map-key-derive channel ==============================
//
// The analog of `--wrapper-requests` for the derive concern that the wrapper-requests map-key seeding (above) structurally
// can't cover: a consumer map mixing a dep KEY with a consumer-owned VALUE (`{* dep_key => my_local}`)
// is not all-one-dep, so it never enters `borrowed_collections.rs`, yet the dep must still derive key
// traits on `dep_key`. The consumer emits `rust/src/generated/borrowed_key_types.rs` recording every
// borrowed map-key type; the dep re-reads it via `--key-requests <consumer>=<path>` and seeds
// `used_as_key` pre-finalize (the same api.rs hook as the wrapper-requests seeding). STRICT, like the W1 sidecar: only the frozen emitter
// grammar is accepted, and a consumer keying on a type the dep no longer defines is a hard error.

/// One row of a consumer's `BORROWED_KEY_TYPES` table: a map-key type the consumer borrows from a
/// workspace dep. `dep` is the dep's rust-crate name as the consumer knows it (extern-deps dir name);
/// `ident` is the borrowed type's CDDL ident (snake-case, as `RustIdent::new` folds it back); `demand`
/// is the comparison/hash flavor the consumer needs on it (the optional 3rd column — absent = `bare`,
/// so old two-column sidecars parse unchanged).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyTypeEntry {
    pub dep: String,
    pub ident: String,
    pub demand: DemandSet,
}

/// Parse a sidecar flavor token (`bare`/`hash`/`ord`/`hash ord`) into a `DemandSet`. The emitter writes
/// a single space-joined token per row; anything else is a hard error (a mangled sidecar must be loud).
fn parse_key_flavor(token: &str, file: &str) -> DemandSet {
    let mut demand = DemandSet::default();
    for word in token.split_whitespace() {
        match word {
            "bare" => demand.bare = true,
            "hash" => demand.hash = true,
            "ord" => demand.ord = true,
            _ => key_hard_error(
                file,
                "unknown key-demand flavor in BORROWED_KEY_TYPES row",
                token,
            ),
        }
    }
    if demand == DemandSet::default() {
        key_hard_error(
            file,
            "empty key-demand flavor in BORROWED_KEY_TYPES row",
            token,
        );
    }
    demand
}

/// The exact comment lines the consumer's `borrowed_key_types.rs` emitter writes (header stamp +
/// four-line banner). Anything else on a top-level `//` comment (outside the `cddl-codegen:` overlay
/// namespace) is a hard error — a drifted/hand-edited banner must be loud.
const KNOWN_KEY_COMMENTS: &[&str] = &[
    "// This file was code-generated using an experimental CDDL to rust tool:",
    "// https://github.com/dcSpark/cddl-codegen",
    "// This file records every map-key type this crate borrows from workspace deps.",
    "// It is machine-read by those deps' generation runs (--key-requests) so they derive the key",
    "// traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the",
    "// compiled self-check below fails THIS crate's build if a dep drops such a derive.",
    "// Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.",
    // The flavored (three-column) banner variant — emitted only when a borrowed key carries a
    // `@used_as_key hash`/`ord` flavor. Both spellings are accepted so a bare sidecar and a flavored
    // one both parse; an OLD tool (with only the two-column banner) hard-errors "unexpected comment"
    // on a new flavored sidecar — the declared cross-crate breaking seam.
    "// Rows are (dep rust-crate name, cddl ident, demand flavor) of each borrowed map-key type.",
];

/// Parse a committed `borrowed_key_types.rs` sidecar into its `BORROWED_KEY_TYPES` entries. Strict:
/// only the frozen emitter grammar is accepted — the header/banner comments, `#[allow(dead_code)]`,
/// the `_assert_key_traits` fn def, the `_borrowed_key_types_self_check` fn block (skipped wholesale
/// by brace depth), and the `pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)]` table. A
/// `compile_error!` / `unpreserved-comment` sentinel, an unknown comment/item, or a mangled tuple is a
/// hard error naming the file; overlay user blocks (`comment_preserve.rs`) are tolerated like the W1
/// sidecar. Returns every well-formed row (the caller filters by dep).
pub fn parse_key_types_sidecar(contents: &str, file: &str) -> Vec<KeyTypeEntry> {
    if contents.contains("compile_error!") {
        panic!(
            "--key-requests {file}: the sidecar contains a `compile_error!` — it is a trapped or \
             drifted generated file, which must never be silently consumed. Regenerate the consumer \
             crate to clear it."
        );
    }
    let logical = flatten_overlay_blocks(contents, file, "--key-requests");
    let mut entries = Vec::new();
    let mut in_const = false;
    let mut const_item = String::new();
    // Brace depth inside a skipped `fn` block (the self-check); its body is not part of the grammar.
    let mut fn_depth: usize = 0;
    for line in &logical {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if fn_depth > 0 {
            fn_depth += trimmed.matches('{').count();
            fn_depth -= trimmed.matches('}').count();
            continue;
        }
        if in_const {
            if trimmed.starts_with("//") {
                key_hard_error(
                    file,
                    "unexpected comment inside `BORROWED_KEY_TYPES`",
                    trimmed,
                );
            }
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(parse_key_const_item(&const_item, file));
                in_const = false;
                const_item.clear();
            }
            continue;
        }
        // Top level.
        if KNOWN_KEY_COMMENTS.contains(&trimmed) {
            continue;
        }
        if trimmed.starts_with("//") {
            key_hard_error(file, "unexpected comment", trimmed);
        }
        if trimmed == "#[allow(dead_code)]" || trimmed == "#[allow(unused_imports)]" {
            continue;
        }
        // The self-check scaffolding: the `_assert_key_traits` bound-carrier and the
        // `_borrowed_key_types_self_check` block. Both are skipped wholesale (brace depth), since only
        // the const table is machine-read — the fns exist for the CONSUMER's compiled derive check.
        if trimmed.starts_with("fn _assert_key_traits")
            || trimmed.starts_with("fn _borrowed_key_types_self_check")
        {
            let opens = trimmed.matches('{').count();
            let closes = trimmed.matches('}').count();
            if opens > closes {
                fn_depth = opens - closes;
            }
            continue;
        }
        if trimmed.starts_with("pub(crate) const BORROWED_KEY_TYPES") {
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(parse_key_const_item(&const_item, file));
                const_item.clear();
            } else {
                in_const = true;
            }
            continue;
        }
        key_hard_error(file, "unexpected item", trimmed);
    }
    if in_const {
        key_hard_error(
            file,
            "unterminated `BORROWED_KEY_TYPES` table (missing `];`)",
            "",
        );
    }
    entries
}

/// Parse the complete `BORROWED_KEY_TYPES` const item into entries. The header up to the initializer
/// `=` must be exactly the frozen declaration (whitespace-normalized — rustfmt may wrap the
/// initializer); the initializer must be a `&[ … ]` array of `("<dep>", "<ident>")` pairs.
fn parse_key_const_item(item: &str, file: &str) -> Vec<KeyTypeEntry> {
    let Some(eq) = item.find('=') else {
        key_hard_error(
            file,
            "malformed `BORROWED_KEY_TYPES` item (missing `=`)",
            item,
        );
    };
    let header: String = item[..eq].split_whitespace().collect::<Vec<_>>().join(" ");
    // Two forms are accepted: the frozen two-column `&[(&str, &str)]` (all rows bare — byte-identical to
    // pre-flavor sidecars) and the three-column `&[(&str, &str, &str)]` (rows carry a flavor token). The
    // body parser tolerates 2- or 3-tuple rows regardless, so an old two-column-typed table with only
    // bare rows and a new three-column-typed table both round-trip.
    if header != "pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)]"
        && header != "pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)]"
    {
        key_hard_error(
            file,
            "unexpected `BORROWED_KEY_TYPES` declaration (the type must be `&[(&str, &str)]` or `&[(&str, &str, &str)]`)",
            &header,
        );
    }
    let init = item[eq + 1..].trim();
    let Some(body) = init
        .strip_prefix("&[")
        .and_then(|rest| rest.trim_end().strip_suffix("];"))
    else {
        key_hard_error(
            file,
            "malformed `BORROWED_KEY_TYPES` initializer (expected `&[ … ];`)",
            init,
        );
    };
    parse_key_const_body(body, file)
}

/// Tokenize the `BORROWED_KEY_TYPES` array body into `("<dep>", "<ident>")` pairs. Strict tuple
/// grammar: exactly two string literals per `(...)` row, optional trailing commas. Any deviation is a
/// hard error (a mangled sidecar must be loud).
fn parse_key_const_body(body: &str, file: &str) -> Vec<KeyTypeEntry> {
    let chars: Vec<char> = body.chars().collect();
    let mut i = 0;
    let mut entries = Vec::new();
    loop {
        i = skip_trivia(&chars, i);
        if i >= chars.len() {
            break;
        }
        if chars[i] != '(' {
            key_hard_error(
                file,
                "unexpected token in BORROWED_KEY_TYPES (expected a `(...)` row)",
                &tail_snippet(&chars, i),
            );
        }
        i += 1;
        let mut fields = Vec::new();
        loop {
            i = skip_trivia(&chars, i);
            if i < chars.len() && chars[i] == ')' {
                i += 1;
                break;
            }
            if i >= chars.len() || chars[i] != '"' {
                key_hard_error(
                    file,
                    "malformed BORROWED_KEY_TYPES row (expected a string literal)",
                    &tail_snippet(&chars, i),
                );
            }
            let (s, next) = parse_string_literal(&chars, i, file);
            fields.push(s);
            i = next;
            i = skip_trivia(&chars, i);
            if i < chars.len() && chars[i] == ',' {
                i += 1;
            } else if i < chars.len() && chars[i] == ')' {
                i += 1;
                break;
            } else {
                key_hard_error(
                    file,
                    "malformed BORROWED_KEY_TYPES row (expected `,` or `)`)",
                    &tail_snippet(&chars, i),
                );
            }
        }
        if fields.len() != 2 && fields.len() != 3 {
            key_hard_error(
                file,
                "malformed BORROWED_KEY_TYPES row (a row must be two string literals — dep, ident — or three, adding a flavor)",
                &format!("{fields:?}"),
            );
        }
        // The optional 3rd column is the comparison/hash flavor; a two-column (old) row is `bare`.
        let demand = if fields.len() == 3 {
            parse_key_flavor(&fields[2], file)
        } else {
            DemandSet {
                bare: true,
                hash: false,
                ord: false,
            }
        };
        entries.push(KeyTypeEntry {
            dep: fields[0].clone(),
            ident: fields[1].clone(),
            demand,
        });
        i = skip_trivia(&chars, i);
        if i < chars.len() && chars[i] == ',' {
            i += 1;
        }
    }
    entries
}

fn key_hard_error(file: &str, what: &str, offending: &str) -> ! {
    if offending.is_empty() {
        panic!(
            "--key-requests {file}: {what}. The sidecar must be an unmodified, tool-generated `borrowed_key_types.rs`."
        );
    }
    panic!(
        "--key-requests {file}: {what}: {offending:?}. The sidecar must be an unmodified, \
         tool-generated `borrowed_key_types.rs`."
    );
}

/// Seed `used_as_key` from every `--key-requests` sidecar's rows addressed to THIS dep, resolving
/// each CDDL ident to a `RustIdent` and marking it (finalize then expands transitively). No-op (and
/// byte-identical to today) when there are no `--key-requests` flags. STRICT: an unreadable path is a
/// hard error; a row naming a type this dep does not define is a hard error naming the consumer and
/// file (a consumer keying on a type the dep deleted must be loud, mirroring the W1 compiled-`use`).
pub fn seed_used_as_key_from_key_requests(types: &mut IntermediateTypes, cli: &Cli) {
    let request_files = cli.key_requests();
    if request_files.is_empty() {
        return;
    }
    let my_lib = cli.lib_name_code();
    let mut to_mark: std::collections::BTreeMap<RustIdent, DemandSet> =
        std::collections::BTreeMap::new();
    for (consumer, path) in &request_files {
        let contents = std::fs::read_to_string(path).unwrap_or_else(|e| {
            panic!("--key-requests {consumer}={path}: cannot read the sidecar: {e}")
        });
        for entry in parse_key_types_sidecar(&contents, path) {
            if entry.dep.replace('-', "_") != my_lib {
                continue;
            }
            // A reserved/primitive ident can never be a dep-defined type and would panic
            // `RustIdent::new`; treat it as unknown so it takes the actionable hard-error path below.
            let known = crate::intermediate::reserved_ident_rejection(&entry.ident).is_none() && {
                let ident = RustIdent::new(CDDLIdent::new(entry.ident.clone()));
                types.rust_struct(&ident).is_some()
                    || types
                        .type_aliases()
                        .contains_key(&AliasIdent::Rust(ident.clone()))
            };
            if !known {
                panic!(
                    "--key-requests {consumer} ({path}): the borrowed key type {:?} (row \
                     ({:?}, {:?})) is not a type this dep defines — a consumer is keying a map on a \
                     type the dep no longer provides. Remedy: restore the type in the dep spec, or \
                     regenerate the consumer so it stops borrowing this key type.",
                    entry.ident, entry.dep, entry.ident
                );
            }
            let ident = RustIdent::new(CDDLIdent::new(entry.ident));
            let e = to_mark.entry(ident).or_default();
            *e = e.union(entry.demand);
        }
    }
    for (ident, demand) in to_mark {
        types.mark_key_demand(ident, demand);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CANONICAL: &str = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {
    use index_dep_crate_wasm::collections::ArrIdxFooList;
    use index_dep_crate_wasm::collections::IdxFooList;
    use index_dep_crate_wasm::collections::MapU64ToIdxFoo;
    use index_dep_crate_wasm::collections::NonEmptyIdxFooList;
}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("index_dep_crate", "ArrIdxFooList", "[* [* idx_foo]]"),
    ("index_dep_crate", "IdxFooList", "[* idx_foo]"),
    ("index_dep_crate", "MapU64ToIdxFoo", "{* uint => idx_foo}"),
    ("index_dep_crate", "NonEmptyIdxFooList", "[+ idx_foo]"),
];
"#;

    const EMPTY: &str = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[];
"#;

    #[test]
    fn accepts_canonical_file() {
        let entries = parse_sidecar(CANONICAL, "borrowed_collections.rs");
        assert_eq!(
            entries,
            vec![
                WrapperRequestEntry {
                    dep: "index_dep_crate".into(),
                    name: "ArrIdxFooList".into(),
                    shape: "[* [* idx_foo]]".into(),
                },
                WrapperRequestEntry {
                    dep: "index_dep_crate".into(),
                    name: "IdxFooList".into(),
                    shape: "[* idx_foo]".into(),
                },
                WrapperRequestEntry {
                    dep: "index_dep_crate".into(),
                    name: "MapU64ToIdxFoo".into(),
                    shape: "{* uint => idx_foo}".into(),
                },
                WrapperRequestEntry {
                    dep: "index_dep_crate".into(),
                    name: "NonEmptyIdxFooList".into(),
                    shape: "[+ idx_foo]".into(),
                },
            ]
        );
    }

    #[test]
    fn accepts_empty_file() {
        let entries = parse_sidecar(EMPTY, "borrowed_collections.rs");
        assert!(entries.is_empty());
    }

    #[test]
    fn accepts_rustfmt_wrapped_rows() {
        // A row wrapped across several lines (what rustfmt does to a long row) parses identically —
        // the const body is tokenized, not matched line-by-line.
        let wrapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {
    use index_dep_crate_wasm::collections::IdxFooList;
}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    (
        "index_dep_crate",
        "IdxFooList",
        "[* idx_foo]",
    ),
];
"#;
        let entries = parse_sidecar(wrapped, "borrowed_collections.rs");
        assert_eq!(
            entries,
            vec![WrapperRequestEntry {
                dep: "index_dep_crate".into(),
                name: "IdxFooList".into(),
                shape: "[* idx_foo]".into(),
            }]
        );
    }

    #[test]
    fn accepts_single_row_collapsed_initializer() {
        // With one short row, rustfmt collapses the table onto a wrapped initializer
        // (`… =\n    &[(…)];` — no `= &[` opener line, no trailing comma). The const is parsed as a
        // whole item, so this lays out identically to the one-row-per-line form.
        let collapsed = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {
    use index_dep_crate_wasm::collections::IdxFooList;
}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] =
    &[("index_dep_crate", "IdxFooList", "[* idx_foo]")];
"#;
        let entries = parse_sidecar(collapsed, "borrowed_collections.rs");
        assert_eq!(
            entries,
            vec![WrapperRequestEntry {
                dep: "index_dep_crate".into(),
                name: "IdxFooList".into(),
                shape: "[* idx_foo]".into(),
            }]
        );
    }

    #[test]
    fn accepts_insert_block_with_conforming_row() {
        // A user `insert` block adds a row via the edit-preservation overlay; its payload row conforms
        // to the grammar and is accepted like any generated row.
        let with_insert = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {
    use index_dep_crate_wasm::collections::IdxFooList;
}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("index_dep_crate", "IdxFooList", "[* idx_foo]"),
    // cddl-codegen:insert-start
    ("index_dep_crate", "IdxBarList", "[* idx_bar]"),
    // cddl-codegen:insert-end
];
"#;
        let entries = parse_sidecar(with_insert, "borrowed_collections.rs");
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[1].name, "IdxBarList");
        assert_eq!(entries[1].shape, "[* idx_bar]");
    }

    #[test]
    fn accepts_replace_block_skipping_recorded_original() {
        // A `replace` block swaps the user's row in for the recorded original; the `:replaces` section
        // (`//`-commented original) is skipped as block structure.
        let with_replace = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// Rows are (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents).
#[allow(unused_imports)]
mod borrowed {}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    // cddl-codegen:replace-start
    ("index_dep_crate", "IdxFooList", "[* idx_foo]"),
    // cddl-codegen:replaces
    // ("index_dep_crate", "IdxBarList", "[* idx_bar]"),
    // cddl-codegen:replace-end
];
"#;
        let entries = parse_sidecar(with_replace, "borrowed_collections.rs");
        assert_eq!(
            entries,
            vec![WrapperRequestEntry {
                dep: "index_dep_crate".into(),
                name: "IdxFooList".into(),
                shape: "[* idx_foo]".into(),
            }]
        );
    }

    #[test]
    #[should_panic(expected = "unpreserved-comment")]
    fn rejects_unpreserved_comment_trap() {
        let trapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every collection wrapper this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--wrapper-requests) and compiled
// here, so a wrapper a dep stops providing fails THIS crate's build, naming the type.
// cddl-codegen:unpreserved-comment
#[allow(unused_imports)]
mod borrowed {}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        parse_sidecar(trapped, "borrowed_collections.rs");
    }

    #[test]
    #[should_panic(expected = "compile_error")]
    fn rejects_compile_error() {
        let trapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

compile_error!("this file drifted");
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        parse_sidecar(trapped, "borrowed_collections.rs");
    }

    #[test]
    #[should_panic(expected = "unexpected item")]
    fn rejects_stray_line() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

fn sneaky() {}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        parse_sidecar(stray, "borrowed_collections.rs");
    }

    #[test]
    #[should_panic(expected = "unexpected comment")]
    fn rejects_unknown_comment() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// a hand-written note that is not part of the frozen banner
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        parse_sidecar(stray, "borrowed_collections.rs");
    }

    #[test]
    #[should_panic(expected = "unexpected comment inside `BORROWED_SHAPES`")]
    fn rejects_in_const_comment() {
        // The old sidecar format kept the column legend INSIDE the const body, where the
        // preservation overlay anchored it to a deletable row (trapping on an in-place regen that
        // dropped a borrow). The legend now lives in the banner; any in-const comment is either a
        // stale old-format sidecar or a stray hand edit — both must be loud.
        let old_format = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    // (dep rust-crate name, wrapper name, shape in CDDL syntax with the dep's idents)
    ("index_dep_crate", "IdxFooList", "[* idx_foo]"),
];
"#;
        parse_sidecar(old_format, "borrowed_collections.rs");
    }

    #[test]
    #[should_panic(expected = "malformed BORROWED_SHAPES row")]
    fn rejects_mangled_tuple() {
        // A two-element tuple (dep + name, no shape) is a mangled row.
        let mangled = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("index_dep_crate", "IdxFooList"),
];
"#;
        parse_sidecar(mangled, "borrowed_collections.rs");
    }

    // ===== lenient shape-key extraction (wrapper-requests seeding) =====

    #[test]
    fn map_key_idents_extracts_key_positions() {
        // A struct-keyed map yields its key; a list yields nothing (lists have no key derives).
        assert_eq!(map_key_cddl_idents("{* idx_foo => uint}"), vec!["idx_foo"]);
        assert!(map_key_cddl_idents("[* idx_foo]").is_empty());
        assert!(map_key_cddl_idents("[+ idx_foo]").is_empty());
        // Nested: the OUTER map's key is `a`; a list value with an inner map contributes its key `k`.
        assert_eq!(map_key_cddl_idents("{* a => [* b]}"), vec!["a"]);
        assert_eq!(map_key_cddl_idents("[* {* k => v}]"), vec!["k"]);
        // A map keyed on a value-primitive contributes the primitive spelling (harmless — the seeding
        // loop filters reserved/primitive names before constructing a RustIdent).
        assert_eq!(map_key_cddl_idents("{* uint => idx_foo}"), vec!["uint"]);
        // Malformed shapes are lenient (no idents, no panic).
        assert!(map_key_cddl_idents("{* idx_foo =>").is_empty());
        assert!(map_key_cddl_idents("garbage((").is_empty());
    }

    #[test]
    fn scan_shape_rows_skips_commented_originals() {
        // The lenient scan drops `//`-commented lines (an overlay `:replaces` recorded original must
        // not seed) and returns (dep, shape) for each real 3-literal row.
        let sidecar = r#"// header
mod borrowed {}
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "MapIdxFooToU64", "{* idx_foo => uint}"),
    // ("wr_dep", "GhostList", "[* ghost]"),
];
"#;
        let rows = scan_shape_rows_lenient(sidecar);
        assert_eq!(
            rows,
            vec![("wr_dep".to_owned(), "{* idx_foo => uint}".to_owned())]
        );
    }

    // ===== strict borrowed_key_types.rs parser (--key-requests) =====

    const CANONICAL_KEYS: &str = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every map-key type this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--key-requests) so they derive the key
// traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the
// compiled self-check below fails THIS crate's build if a dep drops such a derive.
// Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd + core::hash::Hash>() {}
#[allow(dead_code)]
fn _borrowed_key_types_self_check() {
    _assert_key_traits::<wr_dep::IdxBar>();
    _assert_key_traits::<wr_dep::IdxFoo>();
}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] =
    &[("wr_dep", "idx_bar"), ("wr_dep", "idx_foo")];
"#;

    const EMPTY_KEYS: &str = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every map-key type this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--key-requests) so they derive the key
// traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the
// compiled self-check below fails THIS crate's build if a dep drops such a derive.
// Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd + core::hash::Hash>() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;

    #[test]
    fn key_types_accepts_canonical_file() {
        let entries = parse_key_types_sidecar(CANONICAL_KEYS, "borrowed_key_types.rs");
        assert_eq!(
            entries,
            vec![
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_bar".into(),
                    demand: DemandSet {
                        bare: true,
                        hash: false,
                        ord: false
                    },
                },
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_foo".into(),
                    demand: DemandSet {
                        bare: true,
                        hash: false,
                        ord: false
                    },
                },
            ]
        );
    }

    // A three-column table row carries a flavor token; a two-column row is `bare`. Mixed tables parse.
    #[test]
    fn key_types_accepts_flavor_column() {
        let src = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every map-key type this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--key-requests) so they derive the key
// traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the
// compiled self-check below fails THIS crate's build if a dep drops such a derive.
// Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd + core::hash::Hash>() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "idx_hash", "hash"),
    ("wr_dep", "idx_ho", "hash ord"),
];
"#;
        assert_eq!(
            parse_key_types_sidecar(src, "borrowed_key_types.rs"),
            vec![
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_hash".into(),
                    demand: DemandSet {
                        bare: false,
                        hash: true,
                        ord: false
                    },
                },
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_ho".into(),
                    demand: DemandSet {
                        bare: false,
                        hash: true,
                        ord: true
                    },
                },
            ]
        );
    }

    #[test]
    fn key_types_accepts_empty_file() {
        assert!(parse_key_types_sidecar(EMPTY_KEYS, "borrowed_key_types.rs").is_empty());
    }

    #[test]
    #[should_panic(expected = "unexpected comment")]
    fn key_types_rejects_unknown_comment() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// a hand-written note that is not part of the frozen banner
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd + core::hash::Hash>() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        parse_key_types_sidecar(stray, "borrowed_key_types.rs");
    }

    #[test]
    #[should_panic(expected = "compile_error")]
    fn key_types_rejects_compile_error() {
        let trapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

compile_error!("this file drifted");
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        parse_key_types_sidecar(trapped, "borrowed_key_types.rs");
    }

    #[test]
    #[should_panic(expected = "malformed BORROWED_KEY_TYPES row")]
    fn key_types_rejects_mangled_tuple() {
        // A four-element tuple is a mangled key row (a row is two literals — dep, ident — or three,
        // adding a flavor). Three is now legal (the optional flavor column), so the mangled case is 4+.
        let mangled = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[
    ("wr_dep", "idx_foo", "hash", "extra"),
];
"#;
        parse_key_types_sidecar(mangled, "borrowed_key_types.rs");
    }

    // A three-column row whose flavor token is not a known word is a hard error (mangled sidecar).
    #[test]
    #[should_panic(expected = "unknown key-demand flavor")]
    fn key_types_rejects_unknown_flavor() {
        let bad = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "idx_foo", "nonsense"),
];
"#;
        parse_key_types_sidecar(bad, "borrowed_key_types.rs");
    }

    #[test]
    #[should_panic(expected = "unexpected item")]
    fn key_types_rejects_stray_item() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

fn sneaky() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        parse_key_types_sidecar(stray, "borrowed_key_types.rs");
    }
}
