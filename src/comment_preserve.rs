//! Carry user-added comments across a regeneration of the tool-owned `src/generated/**` trees.
//!
//! At export time, for each generated `.rs` file that already exists on disk, [`preserve`] overlays
//! the comments a user added to the previous output onto the freshly generated string. It NEVER
//! uses a textual/positional diff: generated Rust has strongly named top-level structure, so a
//! comment is re-anchored by symbol identity (which named item it sits in/above) and token equality
//! (the safety test). A comment that cannot be safely re-placed is not dropped silently — it is
//! turned into a tagged `compile_error!` block so the generated crate fails to build and the user
//! reviews it.
//!
//! Anchoring escalates through tiers, each stricter about what "safe" means:
//! * identity — the file's code tokens are unchanged, so every user comment transfers at the same
//!   token index (the dominant case);
//! * per-item — the file changed elsewhere, but the named item holding the comment is unchanged, so
//!   the comment transfers within it, or the comment sits above a still-present item;
//! * unique-statement — the item's body changed, but the exact statement the comment annotates still
//!   appears exactly once, so the comment re-attaches above it;
//! * otherwise the comment fails loudly.
//!
//! v1 scope is own-line comments (only whitespace before them on their line). A user-added trailing
//! (end-of-line) comment is detected but not re-placed — it fails loudly with a hint to move it to
//! its own line — so the never-silent property holds without a trailing-anchor flavor.
//!
//! The lexer is string-aware by necessity, not thoroughness: Rust string/raw-string literals span
//! lines, so a line can begin with `//` while inside a literal; a comment cannot be classified
//! without tracking literal state first. The input is our own generated output plus user comments (a
//! constrained Rust subset), but a user edit that breaks the splitter's assumptions must land in the
//! fail-loudly path, never a silent misplacement — so imperfect splitting degrades to item-match
//! failure, which is loud.

use std::collections::{BTreeMap, BTreeSet};

/// Own-line comment line that marks a fail-loudly block. Matched at the START of a comment's text so
/// the block is recognized on the NEXT regeneration and carried forward verbatim rather than counted
/// as a user code edit.
const SENTINEL_MARKER: &str = "// cddl-codegen:unpreserved-comment";

/// The merged content plus whether any comment was inserted. `changed == false` means `content`
/// equals the pristine input byte-for-byte, so the caller can skip the extra rustfmt pass.
pub struct Preserved {
    pub content: String,
    /// True iff a comment/sentinel was inserted — the caller reformats only then (the insertion is
    /// raw text that rustfmt must normalize; a no-op pass would still be idempotent but wasteful).
    pub changed: bool,
}

/// A failure to lex the existing on-disk file. Carries a message; the caller attaches the file name.
#[derive(Debug)]
pub struct PreserveError {
    pub message: String,
}

impl std::fmt::Display for PreserveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for PreserveError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokKind {
    Ident,
    Literal,
    Punct,
    Lifetime,
}

#[derive(Debug, Clone, Copy)]
struct CodeTok<'a> {
    kind: TokKind,
    text: &'a str,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Copy)]
struct Comment<'a> {
    text: &'a str,
    own_line: bool,
    /// Index into the code-token stream of the following code token (== token count at EOF).
    anchor: usize,
    start: usize,
    end: usize,
}

struct Lexed<'a> {
    src: &'a str,
    code: Vec<CodeTok<'a>>,
    comments: Vec<Comment<'a>>,
}

fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_' || c >= 0x80
}

fn is_ident_cont(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c >= 0x80
}

fn err<T>(message: &str) -> Result<T, PreserveError> {
    Err(PreserveError {
        message: message.to_owned(),
    })
}

/// Scan a `"…"` string literal starting at the opening quote; return the byte index past the close.
fn scan_string(b: &[u8], i: usize) -> Result<usize, PreserveError> {
    let n = b.len();
    let mut j = i + 1;
    while j < n {
        match b[j] {
            b'\\' => j += 2, // skip the escaped byte (a trailing '\' overshoots → unterminated)
            b'"' => return Ok(j + 1),
            _ => j += 1,
        }
    }
    err("unterminated string literal")
}

/// Scan a char literal `'…'` starting at the opening quote; return the byte index past the close.
/// Escapes (`'\n'`, `'\u{1F600}'`, `'\''`) are skipped whole via the backslash rule.
fn scan_char(b: &[u8], i: usize) -> Result<usize, PreserveError> {
    let n = b.len();
    let mut j = i + 1;
    while j < n {
        match b[j] {
            b'\\' => j += 2,
            b'\'' => return Ok(j + 1),
            _ => j += 1,
        }
    }
    err("unterminated char/byte literal")
}

/// Scan a raw string starting at the `r` (already past any `b`/`c` prefix): `r#*"…"#*`.
fn scan_raw_string(b: &[u8], i: usize) -> Result<usize, PreserveError> {
    let n = b.len();
    let mut j = i + 1;
    let mut hashes = 0;
    while j < n && b[j] == b'#' {
        hashes += 1;
        j += 1;
    }
    if j >= n || b[j] != b'"' {
        return err("malformed raw string literal");
    }
    j += 1;
    while j < n {
        if b[j] == b'"' {
            let mut k = 0;
            let mut p = j + 1;
            while k < hashes && p < n && b[p] == b'#' {
                k += 1;
                p += 1;
            }
            if k == hashes {
                return Ok(p);
            }
        }
        j += 1;
    }
    err("unterminated raw string literal")
}

/// If a string-like literal with a letter prefix starts at `i` (`b"…"`, `c"…"`, `b'…'`, `r"…"`,
/// `r#…"#`, `br…`, `cr…`), return the byte index past its end. `None` means the leading letter is an
/// ordinary identifier.
fn try_prefixed_string(b: &[u8], i: usize) -> Result<Option<usize>, PreserveError> {
    let n = b.len();
    match b[i] {
        b'b' | b'c' => {
            if i + 1 < n && b[i + 1] == b'"' {
                return Ok(Some(scan_string(b, i + 1)?));
            }
            if b[i] == b'b' && i + 1 < n && b[i + 1] == b'\'' {
                return Ok(Some(scan_char(b, i + 1)?));
            }
            if i + 1 < n && b[i + 1] == b'r' && i + 2 < n && (b[i + 2] == b'"' || b[i + 2] == b'#')
            {
                return Ok(Some(scan_raw_string(b, i + 1)?));
            }
            Ok(None)
        }
        b'r' if i + 1 < n && (b[i + 1] == b'"' || b[i + 1] == b'#') => {
            Ok(Some(scan_raw_string(b, i)?))
        }
        _ => Ok(None),
    }
}

/// True for the byte after the multi-byte-safe advance in [`lex`]'s number branch etc. Adjusts the
/// running delimiter depth by one token's contribution.
fn delim_delta(text: &str) -> i32 {
    match text {
        "{" | "(" | "[" => 1,
        "}" | ")" | "]" => -1,
        _ => 0,
    }
}

fn is_open(text: &str) -> bool {
    matches!(text, "{" | "(" | "[")
}

fn is_close(text: &str) -> bool {
    matches!(text, "}" | ")" | "]")
}

/// Lex `src` into code tokens (comments stripped out, kept separately with a following-token
/// anchor). String/char/raw-literal state is tracked first so a `//` inside a literal is never
/// mistaken for a comment. An unterminated literal or block comment is a hard error (the caller's
/// existing file is unsafe to clobber silently).
fn lex(src: &str) -> Result<Lexed<'_>, PreserveError> {
    let b = src.as_bytes();
    let n = b.len();
    let mut i = 0;
    let mut code: Vec<CodeTok> = Vec::new();
    let mut comments: Vec<Comment> = Vec::new();

    while i < n {
        let c = b[i];
        if c.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        // line comment
        if c == b'/' && i + 1 < n && b[i + 1] == b'/' {
            let start = i;
            i += 2;
            while i < n && b[i] != b'\n' {
                i += 1;
            }
            comments.push(Comment {
                text: &src[start..i],
                own_line: false,
                anchor: code.len(),
                start,
                end: i,
            });
            continue;
        }
        // block comment (nested)
        if c == b'/' && i + 1 < n && b[i + 1] == b'*' {
            let start = i;
            i += 2;
            let mut depth = 1;
            while i < n && depth > 0 {
                if b[i] == b'/' && i + 1 < n && b[i + 1] == b'*' {
                    depth += 1;
                    i += 2;
                } else if b[i] == b'*' && i + 1 < n && b[i + 1] == b'/' {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth != 0 {
                return err("unterminated block comment");
            }
            comments.push(Comment {
                text: &src[start..i],
                own_line: false,
                anchor: code.len(),
                start,
                end: i,
            });
            continue;
        }
        // identifier (or a letter-prefixed string literal)
        if is_ident_start(c) {
            if let Some(end) = try_prefixed_string(b, i)? {
                code.push(CodeTok {
                    kind: TokKind::Literal,
                    text: &src[i..end],
                    start: i,
                    end,
                });
                i = end;
                continue;
            }
            let start = i;
            i += 1;
            while i < n && is_ident_cont(b[i]) {
                i += 1;
            }
            code.push(CodeTok {
                kind: TokKind::Ident,
                text: &src[start..i],
                start,
                end: i,
            });
            continue;
        }
        // plain string
        if c == b'"' {
            let end = scan_string(b, i)?;
            code.push(CodeTok {
                kind: TokKind::Literal,
                text: &src[i..end],
                start: i,
                end,
            });
            i = end;
            continue;
        }
        // char literal vs lifetime: `'a'` is a char, `'a` / `'static` is a lifetime. A `'` followed
        // by an ident-start whose run is NOT immediately closed by `'` is a lifetime.
        if c == b'\'' {
            if i + 1 < n && is_ident_start(b[i + 1]) && !(i + 2 < n && b[i + 2] == b'\'') {
                let start = i;
                let mut j = i + 1;
                while j < n && is_ident_cont(b[j]) {
                    j += 1;
                }
                code.push(CodeTok {
                    kind: TokKind::Lifetime,
                    text: &src[start..j],
                    start,
                    end: j,
                });
                i = j;
                continue;
            }
            let end = scan_char(b, i)?;
            code.push(CodeTok {
                kind: TokKind::Literal,
                text: &src[i..end],
                start: i,
                end,
            });
            i = end;
            continue;
        }
        // number
        if c.is_ascii_digit() {
            let start = i;
            i += 1;
            while i < n && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                i += 1;
            }
            if i < n && b[i] == b'.' && i + 1 < n && b[i + 1].is_ascii_digit() {
                i += 1;
                while i < n && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                    i += 1;
                }
            }
            code.push(CodeTok {
                kind: TokKind::Literal,
                text: &src[start..i],
                start,
                end: i,
            });
            continue;
        }
        // punctuation: merge `::` (needed to tell a `use` group brace from a body brace); every other
        // operator is a single-char token — consistent on both sides, which is all token equality needs.
        if c == b':' && i + 1 < n && b[i + 1] == b':' {
            code.push(CodeTok {
                kind: TokKind::Punct,
                text: &src[i..i + 2],
                start: i,
                end: i + 2,
            });
            i += 2;
            continue;
        }
        code.push(CodeTok {
            kind: TokKind::Punct,
            text: &src[i..i + 1],
            start: i,
            end: i + 1,
        });
        i += 1;
    }

    // Own-line classification: only whitespace before the comment on its line AND only whitespace
    // after it up to the next newline (the latter matters for a block comment sharing a code line).
    for cm in &mut comments {
        let line_start = src[..cm.start].rfind('\n').map(|p| p + 1).unwrap_or(0);
        let before_ws = src[line_start..cm.start].trim().is_empty();
        let after_end = src[cm.end..]
            .find('\n')
            .map(|p| cm.end + p)
            .unwrap_or(src.len());
        let after_ws = src[cm.end..after_end].trim().is_empty();
        cm.own_line = before_ws && after_ws;
    }

    Ok(Lexed {
        src,
        code,
        comments,
    })
}

/// A top-level item: a contiguous span of the code-token stream, keyed by kind + name so the same
/// item can be found in the regenerated stream even when the file changed elsewhere.
struct Item {
    kind: String,
    name: String,
    start: usize,
    end: usize,
}

/// Advance past a balanced `open`/`close` run starting at `idx` (which is on the open token); return
/// the index just past the matching close.
fn skip_balanced(toks: &[CodeTok], mut idx: usize, open: &str, close: &str) -> usize {
    let mut depth = 0;
    while idx < toks.len() {
        if toks[idx].text == open {
            depth += 1;
        } else if toks[idx].text == close {
            depth -= 1;
            if depth == 0 {
                return idx + 1;
            }
        }
        idx += 1;
    }
    idx
}

fn join_texts(toks: &[CodeTok]) -> String {
    toks.iter().map(|t| t.text).collect::<Vec<_>>().join(" ")
}

/// Determine an item's (kind, name) from its token span. Attributes / visibility / modifiers are
/// skipped to reach the item keyword; an item shape we don't recognize becomes `unknown` keyed on
/// its full text (so it only matches an identical one — anything else routes to fail-loudly).
fn classify(toks: &[CodeTok]) -> (String, String) {
    let len = toks.len();
    let mut idx = 0;
    loop {
        if idx >= len {
            return ("unknown".to_owned(), join_texts(toks));
        }
        match toks[idx].text {
            "#" => {
                idx += 1;
                if idx < len && toks[idx].text == "!" {
                    idx += 1;
                }
                if idx < len && toks[idx].text == "[" {
                    idx = skip_balanced(toks, idx, "[", "]");
                }
            }
            "pub" => {
                idx += 1;
                if idx < len && toks[idx].text == "(" {
                    idx = skip_balanced(toks, idx, "(", ")");
                }
            }
            "unsafe" | "async" | "default" => idx += 1,
            "extern" => {
                idx += 1;
                if idx < len && toks[idx].kind == TokKind::Literal {
                    idx += 1;
                }
            }
            // `const fn` — `const` is a modifier here, not the item keyword.
            "const" if idx + 1 < len && toks[idx + 1].text == "fn" => idx += 1,
            _ => break,
        }
    }
    let kw = toks[idx].text;
    let name = match kw {
        "struct" | "enum" | "union" | "trait" | "fn" | "mod" | "type" | "const" | "static" => toks
            .get(idx + 1)
            .map(|t| t.text.to_owned())
            .unwrap_or_default(),
        "macro_rules" => toks
            .get(idx + 2)
            .map(|t| t.text.to_owned())
            .unwrap_or_default(),
        "impl" => {
            let brace = toks[idx..]
                .iter()
                .position(|t| t.text == "{")
                .map(|p| idx + p)
                .unwrap_or(len);
            join_texts(&toks[idx..brace])
        }
        "use" => {
            let end = if len > idx + 1 && toks[len - 1].text == ";" {
                len - 1
            } else {
                len
            };
            join_texts(&toks[idx + 1..end])
        }
        _ => return ("unknown".to_owned(), join_texts(toks)),
    };
    (kw.to_owned(), name)
}

/// Partition a code-token stream into a contiguous list of top-level items. Between items there are
/// no code tokens (only whitespace/comments), so every code token belongs to exactly one item.
fn split_items(code: &[CodeTok]) -> Vec<Item> {
    let n = code.len();
    let mut items = Vec::new();
    let mut i = 0;
    while i < n {
        let start = i;
        // Inner attribute `#![ … ]` is a standalone item with no `;` or body brace.
        if code[i].text == "#" && i + 1 < n && code[i + 1].text == "!" {
            let mut j = i + 2;
            if j < n && code[j].text == "[" {
                j = skip_balanced(code, j, "[", "]");
            }
            let (kind, name) = classify(&code[start..j]);
            items.push(Item {
                kind,
                name,
                start,
                end: j,
            });
            i = j;
            continue;
        }
        let mut cd = 0i32; // curly depth
        let mut pd = 0i32; // paren/bracket depth
        let mut use_group = false;
        loop {
            if i >= n {
                break;
            }
            let t = code[i].text;
            match t {
                "{" => {
                    // A `::{` opens a `use` path group, not an item body; don't end at its close.
                    if cd == 0 && pd == 0 && i > start && code[i - 1].text == "::" {
                        use_group = true;
                    }
                    cd += 1;
                    i += 1;
                }
                "}" => {
                    cd -= 1;
                    i += 1;
                    if cd == 0 && pd == 0 && !use_group {
                        break;
                    }
                }
                "(" | "[" => {
                    pd += 1;
                    i += 1;
                }
                ")" | "]" => {
                    pd -= 1;
                    i += 1;
                }
                ";" => {
                    i += 1;
                    if cd == 0 && pd == 0 {
                        break;
                    }
                }
                _ => i += 1,
            }
        }
        let (kind, name) = classify(&code[start..i]);
        items.push(Item {
            kind,
            name,
            start,
            end: i,
        });
    }
    items
}

fn code_eq(a: &[CodeTok], b: &[CodeTok]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(x, y)| x.kind == y.kind && x.text == y.text)
}

/// All start offsets (into `hay`) where `needle` occurs as a contiguous token subsequence.
fn find_subsequence(hay: &[CodeTok], needle: &[CodeTok]) -> Vec<usize> {
    let mut res = Vec::new();
    if needle.is_empty() || needle.len() > hay.len() {
        return res;
    }
    for i in 0..=(hay.len() - needle.len()) {
        if (0..needle.len())
            .all(|k| hay[i + k].kind == needle[k].kind && hay[i + k].text == needle[k].text)
        {
            res.push(i);
        }
    }
    res
}

/// The token run from `rel` to the end of its statement within `toks`: through the `;` at the same
/// delimiter nesting, or up to the `}`/`)`/`]` that closes the enclosing block (exclusive). This is
/// the anchor the unique-statement tier searches for in the regenerated item.
fn statement_run<'a>(toks: &'a [CodeTok<'a>], rel: usize) -> &'a [CodeTok<'a>] {
    let base: i32 = toks[..rel].iter().map(|t| delim_delta(t.text)).sum();
    let mut d = base;
    let mut j = rel;
    while j < toks.len() {
        let tx = toks[j].text;
        if is_open(tx) {
            d += 1;
            j += 1;
        } else if is_close(tx) {
            if d == base {
                return &toks[rel..j];
            }
            d -= 1;
            j += 1;
        } else if tx == ";" && d == base {
            return &toks[rel..=j];
        } else {
            j += 1;
        }
    }
    &toks[rel..]
}

fn escape_for_rust_string(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => o.push_str("\\\\"),
            '"' => o.push_str("\\\""),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            _ => o.push(ch),
        }
    }
    o
}

/// Build a fail-loudly block: a recognizable sentinel comment line plus a `compile_error!` carrying
/// the full original comment, so the crate fails to build with the comment in the message.
fn sentinel_block(reason: &str, original_comment: &str) -> String {
    let message = format!(
        "cddl-codegen could not preserve a user comment across regeneration.\n{reason}\nOriginal comment:\n{original_comment}"
    );
    format!(
        "{SENTINEL_MARKER} (delete this block after review)\ncompile_error!(\"{}\");",
        escape_for_rust_string(&message)
    )
}

fn line_start(src: &str, pos: usize) -> usize {
    src[..pos].rfind('\n').map(|p| p + 1).unwrap_or(0)
}

fn line_indent(src: &str, pos: usize) -> &str {
    let ls = line_start(src, pos);
    let line = &src[ls..];
    let end = line
        .find(|c: char| !c.is_whitespace())
        .unwrap_or(line.len());
    &line[..end]
}

/// A pending text insertion into the pristine `new` source at a byte offset (a line-start position),
/// ordered so ties at one offset keep their push order.
struct Insertion {
    offset: usize,
    order: usize,
    text: String,
}

/// Overlay the user comments from `old` onto the freshly generated `new`. See the module docs for
/// the tiered anchoring. Pure: no I/O; output is a function of `(old, new)`.
pub fn preserve(old: &str, new: &str) -> Result<Preserved, PreserveError> {
    let old_lex = lex(old)?;
    let new_lex = lex(new)?;

    // 1. Recognize and strip fail-loudly blocks emitted by a prior run so they don't count as a user
    //    code edit (which would break the identity tier for the whole file). Their payload is carried
    //    forward verbatim — dropping it would silently destroy the trapped comment on the next regen.
    let mut removed_code: BTreeSet<usize> = BTreeSet::new();
    let mut sentinel_comment: BTreeSet<usize> = BTreeSet::new();
    let mut carried_blocks: Vec<String> = Vec::new();
    for (ci, cm) in old_lex.comments.iter().enumerate() {
        if !(cm.own_line && cm.text.starts_with(SENTINEL_MARKER)) {
            continue;
        }
        let a = cm.anchor;
        if a + 5 < old_lex.code.len()
            && old_lex.code[a].kind == TokKind::Ident
            && old_lex.code[a].text == "compile_error"
            && old_lex.code[a + 1].text == "!"
            && old_lex.code[a + 2].text == "("
            && old_lex.code[a + 3].kind == TokKind::Literal
            && old_lex.code[a + 4].text == ")"
            && old_lex.code[a + 5].text == ";"
        {
            carried_blocks.push(old_lex.src[cm.start..old_lex.code[a + 5].end].to_owned());
            sentinel_comment.insert(ci);
            for k in a..=a + 5 {
                removed_code.insert(k);
            }
        }
    }

    // Filtered old code stream (sentinel `compile_error!` tokens removed) + an anchor remap: for an
    // anchor `a` into the original stream, `kept_before[a]` is its index into the filtered stream.
    let mut kept_before = vec![0usize; old_lex.code.len() + 1];
    let mut kept = 0;
    for (idx, slot) in kept_before.iter_mut().enumerate().take(old_lex.code.len()) {
        *slot = kept;
        if !removed_code.contains(&idx) {
            kept += 1;
        }
    }
    kept_before[old_lex.code.len()] = kept;
    let old_code: Vec<CodeTok> = old_lex
        .code
        .iter()
        .enumerate()
        .filter(|(idx, _)| !removed_code.contains(idx))
        .map(|(_, t)| *t)
        .collect();

    // The generator's own comments (the CODEGEN_HEADER banner) appear identically in `new` at the
    // same anchor, so they self-cancel: exclude any old comment `new` already carries at that anchor.
    let new_comment_keys: BTreeSet<(usize, &str)> = new_lex
        .comments
        .iter()
        .filter(|c| c.own_line)
        .map(|c| (c.anchor, c.text))
        .collect();

    // Split old comments into: trailing (fail loudly) and own-line user comments (candidates).
    let mut trailing: Vec<&str> = Vec::new();
    let mut user_comments: Vec<Comment> = Vec::new();
    for (ci, cm) in old_lex.comments.iter().enumerate() {
        if sentinel_comment.contains(&ci) {
            continue;
        }
        if !cm.own_line {
            trailing.push(cm.text);
            continue;
        }
        let anchor = kept_before[cm.anchor];
        if new_comment_keys.contains(&(anchor, cm.text)) {
            continue; // generator comment (header) — already present in new
        }
        user_comments.push(Comment { anchor, ..*cm });
    }

    // 2. Place each user comment. Insertions target byte offsets in `new`; unplaceable comments and
    //    the verbatim carried blocks become fail-loudly blocks at the top (after the header).
    let mut insertions: Vec<Insertion> = Vec::new();
    let mut unplaceable: Vec<(String, String)> = Vec::new(); // (reason, original comment text)
    let mut order = 0usize;

    let identity = code_eq(&old_code, &new_lex.code);
    let old_items = if identity {
        Vec::new()
    } else {
        split_items(&old_code)
    };
    let new_items = if identity {
        Vec::new()
    } else {
        split_items(&new_lex.code)
    };
    // (kind, name) -> new item indices, in order (occurrence index disambiguates duplicates).
    let mut new_by_key: BTreeMap<(&str, &str), Vec<usize>> = BTreeMap::new();
    for (i, it) in new_items.iter().enumerate() {
        new_by_key
            .entry((it.kind.as_str(), it.name.as_str()))
            .or_default()
            .push(i);
    }
    // occurrence index of each old item among same-keyed old items.
    let mut old_occ = vec![0usize; old_items.len()];
    {
        let mut counts: BTreeMap<(&str, &str), usize> = BTreeMap::new();
        for (i, it) in old_items.iter().enumerate() {
            let c = counts
                .entry((it.kind.as_str(), it.name.as_str()))
                .or_default();
            old_occ[i] = *c;
            *c += 1;
        }
    }

    for cm in &user_comments {
        let a = cm.anchor;
        // Compute the target code-token index in `new` (or None → insert at EOF, or Err → unplaceable).
        let target: Result<Option<usize>, String> = if a >= old_code.len() {
            Ok(None) // trailing dangling comment at end of file
        } else if identity {
            Ok(Some(a)) // identity tier: same index in new
        } else {
            place_tier2(
                a,
                &old_code,
                &new_lex.code,
                &old_items,
                &new_items,
                &new_by_key,
                &old_occ,
            )
        };
        match target {
            Ok(Some(t)) => {
                let (offset, indent) = if t >= new_lex.code.len() {
                    (new.len(), "")
                } else {
                    let start = new_lex.code[t].start;
                    (line_start(new, start), line_indent(new, start))
                };
                insertions.push(Insertion {
                    offset,
                    order,
                    text: format!("{indent}{}\n", cm.text),
                });
            }
            Ok(None) => {
                insertions.push(Insertion {
                    offset: new.len(),
                    order,
                    text: format!("{}\n", cm.text),
                });
            }
            Err(reason) => unplaceable.push((reason, cm.text.to_owned())),
        }
        order += 1;
    }
    for t in trailing {
        unplaceable.push((
            "It is a trailing (end-of-line) comment; move it to its own line above the code to \
             preserve it."
                .to_owned(),
            t.to_owned(),
        ));
    }

    // Nothing to overlay → the pristine content is byte-identical to today.
    if insertions.is_empty() && unplaceable.is_empty() && carried_blocks.is_empty() {
        return Ok(Preserved {
            content: new.to_owned(),
            changed: false,
        });
    }

    // Fail-loudly blocks go at the top, after the header AND any leading inner attributes (a
    // `compile_error!` item placed before `#![…]` would make the inner attribute illegal). Carried
    // blocks (verbatim, for byte-stable carry-forward) precede freshly-minted ones.
    let top_offset = {
        let mut idx = 0;
        while idx + 1 < new_lex.code.len()
            && new_lex.code[idx].text == "#"
            && new_lex.code[idx + 1].text == "!"
        {
            let mut j = idx + 2;
            if j < new_lex.code.len() && new_lex.code[j].text == "[" {
                j = skip_balanced(&new_lex.code, j, "[", "]");
            }
            idx = j;
        }
        new_lex
            .code
            .get(idx)
            .map(|t| line_start(new, t.start))
            .unwrap_or(new.len())
    };
    let mut top_order = 0usize;
    let mut all: Vec<(usize, usize, usize, String)> = Vec::new(); // (offset, group, order, text)
    for block in &carried_blocks {
        all.push((top_offset, 0, top_order, format!("{block}\n")));
        top_order += 1;
    }
    for (reason, original) in &unplaceable {
        all.push((
            top_offset,
            0,
            top_order,
            format!("{}\n", sentinel_block(reason, original)),
        ));
        top_order += 1;
    }
    for ins in insertions {
        all.push((ins.offset, 1, ins.order, ins.text));
    }
    // Sort by offset, then top blocks before placed comments at the same offset, then push order.
    all.sort_by(|x, y| x.0.cmp(&y.0).then(x.1.cmp(&y.1)).then(x.2.cmp(&y.2)));

    let mut content = String::with_capacity(new.len() + 64);
    let mut prev = 0;
    for (offset, _, _, text) in &all {
        content.push_str(&new[prev..*offset]);
        content.push_str(text);
        prev = *offset;
    }
    content.push_str(&new[prev..]);

    Ok(Preserved {
        content,
        changed: true,
    })
}

/// The per-item / unique-statement tiers, for a comment at code index `a` when the file's tokens
/// differ. Returns the target index in `new` (`Some`), an EOF sentinel (`None` is not produced
/// here), or a fail-loudly reason (`Err`).
#[allow(clippy::too_many_arguments)]
fn place_tier2(
    a: usize,
    old_code: &[CodeTok],
    new_code: &[CodeTok],
    old_items: &[Item],
    new_items: &[Item],
    new_by_key: &BTreeMap<(&str, &str), Vec<usize>>,
    old_occ: &[usize],
) -> Result<Option<usize>, String> {
    let oi = match old_items.iter().position(|it| it.start <= a && a < it.end) {
        Some(oi) => oi,
        None => return Err("It could not be attached to any generated item.".to_owned()),
    };
    let item = &old_items[oi];
    let ni = new_by_key
        .get(&(item.kind.as_str(), item.name.as_str()))
        .and_then(|v| v.get(old_occ[oi]).copied());
    let ni = match ni {
        Some(ni) => ni,
        None => {
            return Err(format!(
                "It was attached to `{} {}`, which no longer exists in the regenerated code.",
                item.kind, item.name
            ));
        }
    };
    let nitem = &new_items[ni];

    // Comment sitting above the item (its first token): re-attach above the matched item even if the
    // body changed — such a comment is about the item, not a body line.
    if a == item.start {
        return Ok(Some(nitem.start));
    }

    let old_slice = &old_code[item.start..item.end];
    let new_slice = &new_code[nitem.start..nitem.end];
    let rel = a - item.start;

    // Per-item identity: the item's body is unchanged → transfer at the same relative index.
    if code_eq(old_slice, new_slice) {
        return Ok(Some(nitem.start + rel));
    }

    // Unique-statement tier: the annotated statement must still appear exactly once.
    let run = statement_run(old_slice, rel);
    let matches = find_subsequence(new_slice, run);
    if matches.len() == 1 {
        Ok(Some(nitem.start + matches[0]))
    } else {
        Err(format!(
            "It was attached inside `{} {}`, whose generated code changed.",
            item.kind, item.name
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // A CODEGEN_HEADER-shaped banner, so tests exercise the self-cancel path the real files hit.
    const HEADER: &str = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n\n";

    fn run(old: &str, new: &str) -> String {
        preserve(old, new).unwrap().content
    }

    #[test]
    fn identity_transfers_own_line_comment() {
        let new = format!("{HEADER}pub struct Foo {{\n    pub a: u64,\n}}\n");
        let old = format!("{HEADER}pub struct Foo {{\n    // keep me\n    pub a: u64,\n}}\n");
        let out = run(&old, &new);
        assert!(out.contains("// keep me"), "comment lost:\n{out}");
        assert!(out.contains("pub a: u64"), "code lost:\n{out}");
    }

    #[test]
    fn header_self_cancels_no_duplicate() {
        // No user comments: output is byte-identical and the header is not duplicated.
        let new = format!("{HEADER}pub struct Foo {{\n    pub a: u64,\n}}\n");
        let res = preserve(&new, &new).unwrap();
        assert!(!res.changed, "a comment-free regen must be a no-op");
        assert_eq!(res.content, new);
        assert_eq!(
            new.matches("This file was code-generated").count(),
            res.content.matches("This file was code-generated").count()
        );
    }

    #[test]
    fn trailing_comment_fails_loudly_with_hint() {
        let new = format!("{HEADER}pub struct Foo {{\n    pub a: u64,\n}}\n");
        let old = format!("{HEADER}pub struct Foo {{\n    pub a: u64, // inline note\n}}\n");
        let out = run(&old, &new);
        assert!(
            out.contains("compile_error!"),
            "no fail-loudly block:\n{out}"
        );
        assert!(out.contains(SENTINEL_MARKER), "no sentinel tag:\n{out}");
        assert!(out.contains("own line"), "no move-to-own-line hint:\n{out}");
        assert!(
            out.contains("inline note"),
            "original comment dropped:\n{out}"
        );
    }

    #[test]
    fn per_item_transfer_with_unrelated_item_changed() {
        let old = format!(
            "{HEADER}pub struct A {{\n    // annotate a\n    pub a: u64,\n}}\n\npub struct B {{\n    pub b: u64,\n}}\n"
        );
        // B changed (added a field) so the whole file's tokens differ; A is untouched.
        let new = format!(
            "{HEADER}pub struct A {{\n    pub a: u64,\n}}\n\npub struct B {{\n    pub b: u64,\n    pub c: u64,\n}}\n"
        );
        let out = run(&old, &new);
        assert!(
            out.contains("// annotate a"),
            "unchanged-item comment lost:\n{out}"
        );
        assert!(
            !out.contains("compile_error!"),
            "should not fail loudly:\n{out}"
        );
        assert!(out.contains("pub c: u64"), "new field missing:\n{out}");
    }

    #[test]
    fn unique_statement_reanchors_in_changed_body() {
        // The impl body changed (extra line), but the annotated statement is still unique.
        let old = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        // the length write\n        write_len(self.a);\n    }}\n}}\n"
        );
        let new = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        write_tag(self.t);\n        write_len(self.a);\n    }}\n}}\n"
        );
        let out = run(&old, &new);
        assert!(out.contains("// the length write"), "comment lost:\n{out}");
        assert!(
            !out.contains("compile_error!"),
            "should re-anchor, not fail:\n{out}"
        );
        // re-anchored directly above the statement it annotates
        let idx_c = out.find("// the length write").unwrap();
        let idx_s = out.find("write_len(self.a);").unwrap();
        assert!(
            idx_c < idx_s,
            "comment must sit above its statement:\n{out}"
        );
    }

    #[test]
    fn changed_statement_fails_loudly() {
        let old = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        // note\n        write_len(self.a);\n    }}\n}}\n"
        );
        // The annotated statement itself changed → its referent is suspect → fail loudly.
        let new = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        write_len(self.b);\n    }}\n}}\n"
        );
        let out = run(&old, &new);
        assert!(out.contains("compile_error!"), "must fail loudly:\n{out}");
        assert!(out.contains("note"), "original comment dropped:\n{out}");
    }

    #[test]
    fn non_unique_statement_fails_loudly() {
        let old = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        // which one\n        push(x);\n    }}\n}}\n"
        );
        // The annotated statement now appears twice in the body → ambiguous → fail loudly.
        let new = format!(
            "{HEADER}impl Foo {{\n    fn go(&self) {{\n        push(x);\n        other();\n        push(x);\n    }}\n}}\n"
        );
        let out = run(&old, &new);
        assert!(out.contains("compile_error!"), "must fail loudly:\n{out}");
        assert!(
            out.contains("which one"),
            "original comment dropped:\n{out}"
        );
    }

    #[test]
    fn vanished_item_fails_loudly() {
        let old = format!(
            "{HEADER}pub struct Gone {{\n    // rip\n    pub a: u64,\n}}\n\npub struct Stay {{\n    pub b: u64,\n}}\n"
        );
        let new = format!("{HEADER}pub struct Stay {{\n    pub b: u64,\n}}\n");
        let out = run(&old, &new);
        assert!(out.contains("compile_error!"), "must fail loudly:\n{out}");
        assert!(out.contains("no longer exists"), "wrong reason:\n{out}");
        assert!(out.contains("rip"), "original comment dropped:\n{out}");
    }

    #[test]
    fn sentinel_block_carries_forward_across_two_regens() {
        let new = format!("{HEADER}pub struct Stay {{\n    pub b: u64,\n}}\n");
        let old = format!(
            "{HEADER}pub struct Gone {{\n    // rip\n    pub a: u64,\n}}\n\npub struct Stay {{\n    pub b: u64,\n}}\n"
        );
        let first = run(&old, &new);
        assert!(
            first.contains("compile_error!"),
            "first regen must emit block:\n{first}"
        );
        // Second regen against the same pristine `new`: the block is recognized and carried forward
        // verbatim (not dropped, not re-processed as a code edit), preserving the trapped comment.
        let second = run(&first, &new);
        assert!(
            second.contains("compile_error!"),
            "block dropped on 2nd regen:\n{second}"
        );
        assert!(
            second.contains("rip"),
            "trapped comment lost on 2nd regen:\n{second}"
        );
        assert_eq!(
            first.matches("compile_error!").count(),
            second.matches("compile_error!").count(),
            "carry-forward must not multiply blocks:\n{second}"
        );
    }

    #[test]
    fn comment_lookalikes_inside_string_literals_are_not_comments() {
        // A `//`/`///` inside a normal string and a raw string must not be lexed as a comment, and
        // must not be treated as a user comment to transfer.
        let src = format!(
            "{HEADER}pub fn f() {{\n    let a = \"http://not-a-comment\";\n    let b = r#\"also // not /// a comment\"#;\n}}\n"
        );
        let res = preserve(&src, &src).unwrap();
        assert!(
            !res.changed,
            "string-embedded slashes must not register as comments"
        );
        assert_eq!(res.content, src);
        // And the lexer must not choke on them.
        let lexed = lex(&src).unwrap();
        assert!(lexed.comments.iter().all(|c| c.text.starts_with("//")
            && !c.text.contains("not-a-comment")
            && !c.text.contains("also")));
    }

    #[test]
    fn multiline_string_with_leading_slashes_not_a_comment() {
        let src =
            format!("{HEADER}pub const S: &str = \"line1\n// still in the string\nline3\";\n");
        let lexed = lex(&src).unwrap();
        // Only the two header lines are comments; the in-string `//` line is not.
        assert_eq!(
            lexed.comments.len(),
            2,
            "in-string // was mis-lexed as a comment"
        );
    }

    #[test]
    fn idempotency_fixed_point() {
        let new = format!("{HEADER}pub struct Foo {{\n    pub a: u64,\n    pub b: u64,\n}}\n");
        let old = format!(
            "{HEADER}pub struct Foo {{\n    // about a\n    pub a: u64,\n    // about b\n    pub b: u64,\n}}\n"
        );
        let once = run(&old, &new);
        let twice = run(&once, &new);
        assert_eq!(
            once, twice,
            "preserve(preserve(old,new),new) must equal preserve(old,new)"
        );
    }

    #[test]
    fn fail_loudly_block_lands_after_leading_inner_attribute() {
        // A `compile_error!` item emitted before a leading `#![…]` would make the inner attribute
        // illegal; the block must be inserted after it.
        let attr = "#![allow(clippy::too_many_arguments)]\n\n";
        let new = format!("{HEADER}{attr}pub struct Stay {{\n    pub b: u64,\n}}\n");
        let old = format!(
            "{HEADER}{attr}pub struct Gone {{\n    // rip\n    pub a: u64,\n}}\n\npub struct Stay {{\n    pub b: u64,\n}}\n"
        );
        let out = run(&old, &new);
        assert!(out.contains("compile_error!"), "must fail loudly:\n{out}");
        let attr_idx = out.find("#![allow").unwrap();
        let block_idx = out.find("compile_error!").unwrap();
        assert!(
            attr_idx < block_idx,
            "inner attribute must stay above the fail-loudly block:\n{out}"
        );
    }

    #[test]
    fn char_vs_lifetime_disambiguation() {
        // `'a` is a lifetime; `'x'` is a char literal — the lexer must not confuse them.
        let src = "impl<'a> Foo<'a> { fn f(&self) -> char { 'x' } }";
        let lexed = lex(src).unwrap();
        assert!(
            lexed
                .code
                .iter()
                .any(|t| t.kind == TokKind::Lifetime && t.text == "'a")
        );
        assert!(
            lexed
                .code
                .iter()
                .any(|t| t.kind == TokKind::Literal && t.text == "'x'")
        );
    }
}
