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
//! * unique-statement — the item's body changed, but the exact statement the comment annotates
//!   appears exactly once on BOTH sides (unique-in-new alone would let a deleted duplicate's comment
//!   silently re-attach to the survivor), so the comment re-attaches above it;
//! * otherwise the comment fails loudly.
//!
//! The generator itself emits comments into these trees — the header banner, the static prelude
//! merged into `serialization.rs`, `.doc()`-rendered `///` blocks, the wasm redefine notes — so
//! "old comment absent from new at the aligned anchor" does NOT imply "user comment". The rules that
//! keep tool text from being resurrected, duplicated, or spammed as errors: positional self-cancel
//! (an old comment `new` carries at the same anchor is the generator's), insertion-point dedup (a
//! comment re-anchoring to where `new` already carries the identical text is a shifted generator
//! comment — skip), and doc ownership (an anchor `new` documents is tool-owned: an old `///`/`//!`
//! block there is stale tool output — and an UNPLACEABLE doc block drops the same way, so deleting
//! a documented type never traps the tool's own docs in compile_error blocks; the user channel for
//! doc text is the CDDL/`@doc` DSL). Two residuals: a generator `//` comment whose TEXT changes
//! between tool versions is indistinguishable from user text and fails loudly — noisy-but-safe,
//! once per upgrade; and positional self-cancel compares anchor indices even across divergent token
//! streams, so a user comment textually identical to a generator comment at a coincidentally equal
//! index is skipped — contrived, and the text still exists in the file. (The CRLF trailing-`\r`
//! strip covers line comments only; a multi-line `/* */` interior keeps its `\r` bytes — cosmetic
//! for user text, unreachable for tool text since the generator emits no block comments.)
//!
//! v1 scope is own-line comments (only whitespace before them on their line). A user-added trailing
//! (end-of-line) comment is detected but not re-placed — it fails loudly with a hint to move it to
//! its own line — so the never-silent property holds without a trailing-anchor flavor. One class of
//! trailing marker is NOT a user typo but rustfmt's own canonical form: a `// cddl-codegen:<tag>`
//! comment that trails the closing `}` of a match's LAST arm folds onto that `}` as a trailing comment
//! (`} // cddl-codegen:replaces`, following recorded-original lines re-indented as an aligned block).
//! The tool's own rustfmt pass writes that shape on any regen that splices a match-tail replace/insert
//! block, so [`unfold_trailing_markers`] runs at the shared entry of both scan paths ([`preserve`] and
//! the never-silent harness) and moves any trailing `cddl-codegen:` marker back onto its own line
//! below the code it trailed — both spellings then parse and the rustfmt'd on-disk form is a stable
//! fixed point, so "run twice = run once" survives the format step with no consumer `cargo fmt`
//! needed. Emission is unchanged (own-line everywhere); rustfmt re-folds on write. Trailing
//! comments whose exact text appears in `new`'s trailing set cancel silently. INVARIANT: the
//! generator emits NO trailing comment on a row a spec change can delete — such a comment strands on
//! the deleted row and re-injects as a `compile_error!` trap that carries forward across regens, so
//! the two generated files that once carried per-row `// <cddl>` markers (`extern_interface_check.rs`,
//! `key_demand_assertions.rs`) were made banner-only. This cancellation rule is therefore
//! defense-in-depth: if the generator ever regrows a trailing comment, it must not spam compile
//! errors. (The `// <cddl>` lines in some `--emit-tests` fixtures live in harness-appended
//! hand-written test modules outside the overlay-covered trees, not in tool-owned generated code.)
//!
//! Beyond comments, a user can keep hand-written CODE across a regen with an
//! `// cddl-codegen:insert-start` … `// cddl-codegen:insert-end` own-line comment pair. The whole
//! block — its two tag lines, the interior code, and any interior comments — travels as ONE opaque
//! verbatim unit, anchored exactly like a comment: by the code token immediately following the block,
//! through the identity → per-item → unique-statement tiers. To anchor it we first reconstruct a
//! virtual pristine `old` stream in which a block's interior code tokens are REMOVED (the generator
//! never emitted them, so leaving them in would read as item drift and doom every anchor in that
//! item); the block's tag/interior comments are excluded from the comment pass by the same
//! exclusion-set pattern as a `sentinel_comment`. Recognition is comment-text based on the lexed
//! stream, so a tag-lookalike inside a string literal is inert for free. An unplaceable block is not
//! left in place — its ENTIRE text is escaped into the same `compile_error!` fail-loudly payload an
//! unplaceable comment uses (a bigger message, zero new machinery), so a failed block carries forward
//! verbatim on the next regen instead of being recounted as a user edit.
//!
//! A user can also SWAP generated code with an `// cddl-codegen:replace-start` … `:replaces` …
//! `:replace-end` block: the user's replacement sits between `replace-start` and `replaces`; every
//! line between `replaces` and `replace-end` is a `//`-commented copy of the generated code it
//! replaced. That recorded original does three jobs — it is the placement ANCHOR (uncomment it, lex
//! it into a NEEDLE, find that token run in the regenerated item, splice the user block over it),
//! the DRIFT detector (needle gone ⇒ the generator's output for that region changed ⇒ fail loudly
//! with the recorded original in the message, killing silently-stale overrides), and the review
//! record (every override is visible in diffs next to what it replaced). To anchor it we substitute
//! the block's user-code span in the virtual pristine stream with the NEEDLE (not merely remove it):
//! then the identity tier still fires when generator output is unchanged, and the needle regains
//! BOTH-sides uniqueness. Placement anchors by one of two paths. First the ITEM-IDENTITY fast path: if
//! the whole enclosing top-level item regenerated token-identically, the block splices at the same
//! offset it physically occupied — position disambiguates, so the recorded original need NOT be unique
//! within the item, which is what lets two different occurrences of a duplicated fragment both be
//! replaced. It is sound by construction: the substitution placed the needle at that offset, so an
//! identical item carries the recorded original's exact tokens there; a wrong/drifted needle makes the
//! item non-identical and falls through. Otherwise the strict BOTH-sides-uniqueness path: the needle
//! must be unique in the virtual old item AND in the matched new item — the deleted-duplicate hazard
//! the comment engine documents is here a LOUD failure, not a residual: a recorded original that is
//! non-unique in the virtual old (a genuine duplicate the generator still emits twice, when the item
//! also changed) fails loudly rather than guessing which occurrence it overrides. The
//! splice is BYTE-RANGE, not line-based (from the first matched token's start to the last's end), so
//! a needle beginning mid-line in a one-liner match arm splices correctly and generator comments
//! interior to that span are deleted with it while comments before/after survive. Whole-item replaces
//! (a member fn, a whole `impl`) are the same path with a bigger needle; a member fn's enclosing
//! TOP-LEVEL item is its impl, and `find_subsequence` within the impl slice locates it without real
//! Rust parsing. The merge engine is now a set of non-overlapping delete+insert ops on `new`: an
//! insertion (comment / insert block) whose target offset falls STRICTLY INSIDE a replaced span fails
//! loudly (its referent is being replaced — move it into the block); an insert block or comment at a
//! splice's START byte lands ABOVE the spliced code (ordered before the splice). Hard `PreserveError`
//! (pre-splice) on a malformed block: a missing `replaces`/`replace-end`, an empty recorded original
//! (lexes to zero code tokens), a user section or recorded original that closes a delimiter it does not
//! open, a user section and recorded original whose net delimiter deltas differ (the delta rule below),
//! a recorded original that fails to lex, an orphaned/nested tag, or a recorded original that straddles
//! a top-level item boundary. The block-shape errors (delimiter deltas, empty recorded original)
//! tag the offending block's `*-start` line on the [`PreserveError`], so the caller renders a
//! `file:line:` prefix editors turn into a clickable jump (one on-disk file can hold many blocks). An
//! empty user section is a (undocumented) deletion — allowed, pinned by
//! a fixture. Every fail-loudly `compile_error!` names its payload correctly — "a user comment" for a
//! comment, "a user code block" for an insert/replace block.
//!
//! Namespace reservation makes never-silent hold in the presence of tags: any own-line comment
//! beginning `// cddl-codegen:` that is not part of a well-formed known structure — a valid
//! `unpreserved-comment` fail-loudly block, a well-formed insert block, or a well-formed replace
//! block — is a hard [`PreserveError`] naming the offending line, NEVER a silent demotion to user
//! text. This closes the gap where a stray tag inside a block would terminate it early and clobber
//! the trailing user lines as untagged code: premature termination always leaves an orphaned tag,
//! which errors instead of truncating. So a bare `unpreserved-comment` marker NOT backed by the
//! `compile_error!` shape is a hard error too (it is a malformed fail-loudly block, not a user
//! comment), and an orphaned `replaces`/`replace-end` errors rather than degrading to a user comment.
//! Because [`unfold_trailing_markers`] moves EVERY trailing `cddl-codegen:` marker onto its own line
//! before the scan, this reservation now covers the rustfmt-folded position uniformly: a trailing
//! marker with an UNKNOWN tag (`} // cddl-codegen:not-a-real-tag`) becomes the same hard
//! namespace-reservation error an own-line unknown tag raises, rather than the softer "move it to its
//! own line" trailing-comment trap it fell to before — never-silent applied in one place regardless of
//! where rustfmt put the marker.
//! An INSERT block's user section must have absolutely balanced `{}`/`()`/`[]` (it has no recorded
//! original to pair against). A REPLACE block instead obeys the DELTA rule: the user section and the
//! recorded original must each be never-negative (neither closes a delimiter it does not open) and must
//! change delimiter depth by the SAME per-delimiter net amount (each of `{}`/`()`/`[]` compared
//! separately). Equal net delta means every token downstream of the splice keeps its exact delimiter
//! depth, so top-level item splitting is preserved even under a wrong needle (which then still fails
//! loudly as drift/ambiguity/straddle) — absolute balance was sufficient but not necessary. So a
//! natural `if flag {` paired with a recorded `if <long generated cond> {` (both Δ+1 on `{}`) is legal.
//! Interior dips are rejected because a `} else {` fragment (net Δ0 with a −1 dip) could close the
//! enclosing item in the splitter's view; a hard error otherwise, since an ill-formed fragment cannot
//! be placed by the statement-run model and would corrupt item splitting for the whole file.
//!
//! Rejected alternatives (do not "rediscover" as shortcuts): a `#[cfg(any())]`-style insert-block hack
//! that compiles out a generated statement while carrying a duplicated copy is rejected — it has ZERO
//! drift detection, so the copy goes silently stale on any generator change, defeating the overlay's
//! purpose. Occurrence-ordinal matching (pick the Nth duplicate) WITHOUT a whole-item identity guard is
//! rejected — it silently retargets under a canonical reorder; the item-identity fast path gets the
//! same disambiguation safely only because it is gated on whole-item token identity.
//!
//! The lexer is string-aware by necessity, not thoroughness: Rust string/raw-string literals span
//! lines, so a line can begin with `//` while inside a literal; a comment cannot be classified
//! without tracking literal state first. The input is our own generated output plus user comments (a
//! constrained Rust subset), but a user edit that breaks the splitter's assumptions must land in the
//! fail-loudly path, never a silent misplacement — so imperfect splitting degrades to item-match
//! failure, which is loud.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

/// Own-line comment line that marks a fail-loudly block. Matched at the START of a comment's text so
/// the block is recognized on the NEXT regeneration and carried forward verbatim rather than counted
/// as a user code edit.
const SENTINEL_MARKER: &str = "// cddl-codegen:unpreserved-comment";

/// The reserved own-line-comment namespace. Every `// cddl-codegen:<tag>` is either a well-formed
/// known structure or a hard [`PreserveError`] (see the module docs' namespace-reservation rule).
const CDDL_NAMESPACE: &str = "cddl-codegen:";
/// Insert-block delimiters: `// cddl-codegen:insert-start` … `// cddl-codegen:insert-end`.
const INSERT_START: &str = "insert-start";
const INSERT_END: &str = "insert-end";
/// Replace-block delimiters: `// cddl-codegen:replace-start` (user code) …
/// `// cddl-codegen:replaces` (recorded original, `//`-commented) … `// cddl-codegen:replace-end`.
const REPLACE_START: &str = "replace-start";
const REPLACES: &str = "replaces";
const REPLACE_END: &str = "replace-end";

/// The merged content plus whether any comment was inserted. `changed == false` means `content`
/// equals the pristine input byte-for-byte, so the caller can skip the extra rustfmt pass.
pub struct Preserved {
    pub content: String,
    /// True iff a comment/sentinel was inserted — the caller reformats only then (the insertion is
    /// raw text that rustfmt must normalize; a no-op pass would still be idempotent but wasteful).
    pub changed: bool,
}

/// A failure to preserve/lex the existing on-disk file. The caller attaches the file name via
/// [`render`](Self::render); `line`, when known, is the 1-based line of the offending block so the
/// rendered form is a `file:line:` prefix editors turn into a clickable jump.
#[derive(Debug)]
pub struct PreserveError {
    pub message: String,
    pub line: Option<usize>,
}

impl PreserveError {
    /// `<file>:<line>: <message>` when a line is known (clickable in editors), else `<file>: <message>`.
    pub fn render(&self, file: &str) -> String {
        match self.line {
            Some(l) => format!("{file}:{l}: {}", self.message),
            None => format!("{file}: {}", self.message),
        }
    }
}

impl std::fmt::Display for PreserveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.line {
            Some(l) => write!(f, "line {l}: {}", self.message),
            None => f.write_str(&self.message),
        }
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
        line: None,
    })
}

/// Like [`err`] but tags the 1-based source line of the offending block, so the caller can render a
/// `file:line:` prefix.
fn err_at<T>(line: usize, message: String) -> Result<T, PreserveError> {
    Err(PreserveError {
        message,
        line: Some(line),
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
            // `r#foo` is a raw IDENTIFIER, not a raw string: only `"` after the hash run makes it a
            // string. A raw ident falls through to ordinary lexing (`r` `#` `foo` — side-consistent,
            // which is all token equality needs).
            let mut j = i + 1;
            while j < n && b[j] == b'#' {
                j += 1;
            }
            if j < n && b[j] == b'"' {
                Ok(Some(scan_raw_string(b, i)?))
            } else {
                Ok(None)
            }
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
        // line comment. The text excludes a trailing `\r`: a CRLF-converted prior output (Windows
        // editor, core.autocrlf) must still text-match its LF twin in `new`, or every generator
        // comment reads as user-added and silently duplicates.
        if c == b'/' && i + 1 < n && b[i + 1] == b'/' {
            let start = i;
            i += 2;
            while i < n && b[i] != b'\n' {
                i += 1;
            }
            let text_end = if i > start && b[i - 1] == b'\r' {
                i - 1
            } else {
                i
            };
            comments.push(Comment {
                text: &src[start..text_end],
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
        // Name from AFTER the keyword (`impl Foo` → `Foo`, `impl Ser for Foo` → `Ser for Foo`):
        // the kind already carries `impl`, so messages format as "`impl Foo`", not "`impl impl Foo`".
        "impl" => {
            let brace = toks[idx..]
                .iter()
                .position(|t| t.text == "{")
                .map(|p| idx + p)
                .unwrap_or(len);
            join_texts(&toks[idx + 1..brace])
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

/// The token run from `rel` to the end of its statement within `toks`: through the `;` (or the `,`
/// of a struct-literal field / match arm) at the same delimiter nesting, or up to the `}`/`)`/`]`
/// that closes the enclosing block (exclusive). This is the anchor the unique-statement tier
/// searches for in the regenerated item.
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
        } else if (tx == ";" || tx == ",") && d == base {
            return &toks[rel..=j];
        } else {
            j += 1;
        }
    }
    &toks[rel..]
}

/// Doc comments (`///`, `//!`) are the generator's domain wherever it emits them — their text flows
/// from the CDDL and the `@doc` DSL, so an anchor `new` documents is tool-owned.
fn is_doc_comment(text: &str) -> bool {
    text.starts_with("///") || text.starts_with("//!")
}

pub(crate) fn escape_for_rust_string(s: &str) -> String {
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
/// the full original payload, so the crate fails to build with it in the message. `noun` names the
/// payload — `"comment"` for a user comment, `"code block"` for an insert/replace block — so a
/// trapped block does not misreport itself as "a user comment".
fn sentinel_block(reason: &str, original: &str, noun: &str) -> String {
    let message = format!(
        "cddl-codegen could not preserve a user {noun} across regeneration.\n{reason}\nOriginal {noun}:\n{original}"
    );
    format!(
        "{SENTINEL_MARKER} (delete this block after review)\ncompile_error!(\"{}\");",
        escape_for_rust_string(&message)
    )
}

fn line_start(src: &str, pos: usize) -> usize {
    src[..pos].rfind('\n').map(|p| p + 1).unwrap_or(0)
}

/// 1-based line number of byte offset `pos` in `src`, for locating a malformed block in an error.
fn line_of(src: &str, pos: usize) -> usize {
    src[..pos].bytes().filter(|&b| b == b'\n').count() + 1
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

/// The result of recognizing prior-run fail-loudly (`compile_error!`) blocks in `old`.
struct SentinelScan {
    /// Comment indices that are a valid sentinel marker line — excluded from the comment pass.
    sentinel_comment: BTreeSet<usize>,
    /// Each recognized block's verbatim text, carried forward unchanged (dropping it would destroy
    /// the trapped comment/code on the next regen).
    carried_blocks: Vec<String>,
    /// Code-token indices inside the recognized `compile_error!` blocks, removed from the virtual
    /// pristine old stream so they do not count as a user code edit.
    removed_code: BTreeSet<usize>,
}

/// Recognize the fail-loudly blocks a prior run emitted (`SENTINEL_MARKER` line immediately above a
/// `compile_error!("…");`), so they carry forward verbatim rather than reading as user edits.
fn recognize_sentinels(lexed: &Lexed) -> SentinelScan {
    let mut sentinel_comment = BTreeSet::new();
    let mut carried_blocks = Vec::new();
    let mut removed_code = BTreeSet::new();
    for (ci, cm) in lexed.comments.iter().enumerate() {
        if !(cm.own_line && cm.text.starts_with(SENTINEL_MARKER)) {
            continue;
        }
        let a = cm.anchor;
        if a + 5 < lexed.code.len()
            && lexed.code[a].kind == TokKind::Ident
            && lexed.code[a].text == "compile_error"
            && lexed.code[a + 1].text == "!"
            && lexed.code[a + 2].text == "("
            && lexed.code[a + 3].kind == TokKind::Literal
            && lexed.code[a + 4].text == ")"
            && lexed.code[a + 5].text == ";"
        {
            carried_blocks.push(lexed.src[cm.start..lexed.code[a + 5].end].to_owned());
            sentinel_comment.insert(ci);
            for k in a..=a + 5 {
                removed_code.insert(k);
            }
        }
    }
    SentinelScan {
        sentinel_comment,
        carried_blocks,
        removed_code,
    }
}

/// A recognized `// cddl-codegen:insert-start` … `// cddl-codegen:insert-end` block in `old`. The
/// whole block travels as one opaque verbatim unit; only its placement anchor and its interior span
/// are needed here.
struct InsertBlock {
    /// Byte range of the verbatim block text in `old`: from the start of the insert-start line
    /// through the end of the insert-end comment (trailing newline excluded).
    byte_start: usize,
    byte_end: usize,
    /// Interior code-token range `[code_start, code_end)` in the ORIGINAL old stream (empty when the
    /// block wraps no code). `code_end` is also the placement anchor — the code token the block sits
    /// above.
    code_start: usize,
    code_end: usize,
}

/// A recognized `// cddl-codegen:replace-start` … `:replaces` … `:replace-end` block in `old`. The
/// whole block travels as one opaque verbatim unit; the recorded original (the `//`-commented copy
/// under `replaces`, uncommented into `needle_text`) is the placement anchor and drift detector.
struct ReplaceBlock {
    /// Byte range of the verbatim block text in `old`: from the start of the replace-start line
    /// through the end of the replace-end comment (trailing newline excluded).
    byte_start: usize,
    byte_end: usize,
    /// User-code token range `[user_code_start, user_code_end)` in the ORIGINAL old stream — the
    /// tokens between replace-start and replaces (empty section allowed: an undocumented deletion).
    /// `user_code_end` is also the following-token anchor (== anchor of both `replaces` and
    /// `replace-end`, since only comments sit between them).
    user_code_start: usize,
    user_code_end: usize,
    /// The recorded original, uncommented (leading `//` + one optional space stripped per line,
    /// lines joined by `\n`). Lexed in [`preserve`] into the NEEDLE tokens — only kind+text matter.
    needle_text: String,
}

/// The result of scanning `old` for insert/replace blocks and enforcing the `cddl-codegen:` namespace.
struct BlockScan {
    blocks: Vec<InsertBlock>,
    replace_blocks: Vec<ReplaceBlock>,
    /// Comment indices consumed by a block (its tag lines plus any interior comment, own-line or
    /// trailing) — excluded from the comment pass.
    consumed: BTreeSet<usize>,
    /// Interior code-token indices to remove from the virtual pristine old stream (insert-block
    /// interiors only; a replace block's user code is SUBSTITUTED by its needle, not removed).
    removed_code: BTreeSet<usize>,
}

/// The reserved tag on an own-line comment, if any: the text after `// cddl-codegen:`.
fn cddl_tag(comment_text: &str) -> Option<&str> {
    comment_text
        .strip_prefix("//")?
        .trim_start()
        .strip_prefix(CDDL_NAMESPACE)
}

/// Uncomment one recorded-original line: strip the leading `//` and one optional following space,
/// keeping the rest verbatim. A line that was itself a comment (`// // note`) strips to `// note` —
/// a comment line, which the lexer drops from the code-token stream (inert by construction).
fn uncomment_line(comment_text: &str) -> &str {
    let s = comment_text.strip_prefix("//").unwrap_or(comment_text);
    s.strip_prefix(' ').unwrap_or(s)
}

/// Per-delimiter NET deltas `({}, (), [])` across `toks`, each counted separately — or `None` if any
/// of the three counters dips below zero mid-scan (the fragment closes a delimiter it never opened,
/// e.g. the leading `}` of a `} else {` fragment). A replace block pairs the user section's deltas
/// against the recorded original's: equal per-delimiter net delta means every token downstream of the
/// splice keeps its exact delimiter depth, so top-level item splitting is preserved even under a wrong
/// needle (which then still fails loudly as drift/ambiguity/straddle). The never-negative requirement
/// rejects interior dips because a `} else {`-shaped fragment (net Δ0 on `{}` but a −1 dip) could close
/// the enclosing item in `split_items`' view.
fn delim_deltas(toks: &[CodeTok]) -> Option<(i32, i32, i32)> {
    let (mut c, mut p, mut b) = (0i32, 0i32, 0i32);
    for t in toks {
        match t.text {
            "{" => c += 1,
            "}" => {
                c -= 1;
                if c < 0 {
                    return None;
                }
            }
            "(" => p += 1,
            ")" => {
                p -= 1;
                if p < 0 {
                    return None;
                }
            }
            "[" => b += 1,
            "]" => {
                b -= 1;
                if b < 0 {
                    return None;
                }
            }
            _ => {}
        }
    }
    Some((c, p, b))
}

/// True iff `{}`/`()`/`[]` are balanced across `toks` (and never close before they open). Used by
/// INSERT blocks, which have no recorded original to pair a delta against, so they require absolute
/// balance (a replace block instead uses the equal-delta rule on [`delim_deltas`]).
fn delimiters_balanced(toks: &[CodeTok]) -> bool {
    delim_deltas(toks) == Some((0, 0, 0))
}

/// Recognize insert blocks and enforce the `cddl-codegen:` namespace reservation over `old`'s own-line
/// comments (see the module docs). `sentinel_comment` marks comment indices already claimed by a valid
/// fail-loudly block so their marker line is not re-flagged as a stray tag. Any own-line
/// `// cddl-codegen:` comment that is not part of a well-formed structure is a hard error — never a
/// silent demotion to user text.
fn scan_blocks(
    lexed: &Lexed,
    sentinel_comment: &BTreeSet<usize>,
) -> Result<BlockScan, PreserveError> {
    let comments = &lexed.comments;
    // own-line comment indices in source order (comments are lexed in order).
    let own: Vec<usize> = comments
        .iter()
        .enumerate()
        .filter(|(_, c)| c.own_line)
        .map(|(i, _)| i)
        .collect();
    let mut blocks: Vec<InsertBlock> = Vec::new();
    let mut replace_blocks: Vec<ReplaceBlock> = Vec::new();
    let mut p = 0;
    while p < own.len() {
        let ci = own[p];
        if sentinel_comment.contains(&ci) {
            p += 1;
            continue;
        }
        let tag = match cddl_tag(comments[ci].text) {
            None => {
                p += 1;
                continue;
            }
            Some(t) => t.trim(),
        };
        if tag == REPLACE_START {
            // Phase 1: from the user section, scan to `replaces`. Ordinary interior comments are
            // allowed; any OTHER reserved tag before `replaces` is a malformed structure.
            let mut q = p + 1;
            let mut replaces_p = None;
            while q < own.len() {
                match cddl_tag(comments[own[q]].text) {
                    None => q += 1,
                    Some(inner) => {
                        let inner = inner.trim();
                        if inner == REPLACES {
                            replaces_p = Some(q);
                            break;
                        }
                        return err_at(
                            line_of(lexed.src, comments[ci].start),
                            format!(
                                "An `// cddl-codegen:replace-start` block reached \
                                 `// cddl-codegen:{inner}` before its `// cddl-codegen:replaces` \
                                 marker."
                            ),
                        );
                    }
                }
            }
            let replaces_p = match replaces_p {
                Some(q) => q,
                None => {
                    return err_at(
                        line_of(lexed.src, comments[ci].start),
                        "An `// cddl-codegen:replace-start` block has no `// cddl-codegen:replaces` \
                         marker (nothing separates the user code from the recorded original)."
                            .to_owned(),
                    );
                }
            };
            // Phase 2: from `replaces`, scan to `replace-end`. Every line between is a `//`-commented
            // recorded-original line (an ordinary comment); the only reserved tag allowed is
            // `replace-end`. Any other reserved tag is a malformed structure.
            let mut r = replaces_p + 1;
            let mut end_p = None;
            while r < own.len() {
                match cddl_tag(comments[own[r]].text) {
                    None => r += 1,
                    Some(inner) => {
                        let inner = inner.trim();
                        if inner == REPLACE_END {
                            end_p = Some(r);
                            break;
                        }
                        return err_at(
                            line_of(lexed.src, comments[ci].start),
                            format!(
                                "An `// cddl-codegen:replace-start` block reached \
                                 `// cddl-codegen:{inner}` before its `// cddl-codegen:replace-end` \
                                 marker."
                            ),
                        );
                    }
                }
            }
            let end_p = match end_p {
                Some(r) => r,
                None => {
                    return err_at(
                        line_of(lexed.src, comments[ci].start),
                        "An `// cddl-codegen:replace-start` block is not closed by a matching \
                         `// cddl-codegen:replace-end`."
                            .to_owned(),
                    );
                }
            };
            let start_ci = ci;
            let replaces_ci = own[replaces_p];
            let end_ci = own[end_p];
            let user_code_start = comments[start_ci].anchor;
            let user_code_end = comments[replaces_ci].anchor;
            // Only comments may sit between `replaces` and `replace-end`; a code token there would
            // desync the following-token anchor and means the recorded section is malformed.
            if comments[end_ci].anchor != user_code_end {
                return err_at(
                    line_of(lexed.src, comments[ci].start),
                    "An `// cddl-codegen:replaces` section contains code; every line under \
                     `replaces` must be a `//`-commented copy of the replaced generated code."
                        .to_owned(),
                );
            }
            // Replace blocks use the equal-delta rule (not absolute balance): the user section need
            // only be never-negative here (it must not close a delimiter it does not open); its net
            // deltas are matched against the recorded original's in `preserve`, once the needle lexes.
            if delim_deltas(&lexed.code[user_code_start..user_code_end]).is_none() {
                return err_at(
                    line_of(lexed.src, comments[start_ci].start),
                    "The user section of the `// cddl-codegen:replace-start` block closes a \
                     delimiter ({}, (), or []) it does not open; wrap a fragment that never dips \
                     below its starting depth."
                        .to_owned(),
                );
            }
            // Uncomment the recorded-original lines (the own-line comments between `replaces` and
            // `replace-end`) into the needle text. Emptiness / balance / lex validity are checked in
            // `preserve` once the needle is lexed.
            let mut needle_lines: Vec<&str> = Vec::new();
            for cm in comments.iter() {
                if cm.start >= comments[replaces_ci].end && cm.start < comments[end_ci].start {
                    needle_lines.push(uncomment_line(cm.text));
                }
            }
            let needle_text = needle_lines.join("\n");
            let byte_start = line_start(lexed.src, comments[start_ci].start);
            let byte_end = comments[end_ci].end;
            replace_blocks.push(ReplaceBlock {
                byte_start,
                byte_end,
                user_code_start,
                user_code_end,
                needle_text,
            });
            p = end_p + 1;
        } else if tag == REPLACES {
            return err_at(
                line_of(lexed.src, comments[ci].start),
                "Found `// cddl-codegen:replaces` without an enclosing \
                 `// cddl-codegen:replace-start` block."
                    .to_owned(),
            );
        } else if tag == REPLACE_END {
            return err_at(
                line_of(lexed.src, comments[ci].start),
                "Found `// cddl-codegen:replace-end` without a matching \
                 `// cddl-codegen:replace-start`."
                    .to_owned(),
            );
        } else if tag == INSERT_START {
            // Scan forward for the matching insert-end. Any OTHER reserved tag before it terminates
            // the block prematurely — a hard error, not a silent truncation of the user section.
            let mut q = p + 1;
            let mut end_p = None;
            while q < own.len() {
                let cj = own[q];
                match cddl_tag(comments[cj].text) {
                    None => q += 1, // ordinary interior comment line — allowed
                    Some(inner) => {
                        if inner.trim() == INSERT_END {
                            end_p = Some(q);
                            break;
                        }
                        return err_at(
                            line_of(lexed.src, comments[ci].start),
                            format!(
                                "An `// cddl-codegen:insert-start` block contains an unexpected \
                                 reserved tag before its `// cddl-codegen:insert-end` (line: `{}`).",
                                comments[cj].text
                            ),
                        );
                    }
                }
            }
            let q = match end_p {
                Some(q) => q,
                None => {
                    return err_at(
                        line_of(lexed.src, comments[ci].start),
                        "An `// cddl-codegen:insert-start` block is not closed by a matching \
                         `// cddl-codegen:insert-end`."
                            .to_owned(),
                    );
                }
            };
            let start_ci = ci;
            let end_ci = own[q];
            let code_start = comments[start_ci].anchor;
            let code_end = comments[end_ci].anchor;
            if !delimiters_balanced(&lexed.code[code_start..code_end]) {
                return err_at(
                    line_of(lexed.src, comments[start_ci].start),
                    "The user section of the `// cddl-codegen:insert-start` block has unbalanced \
                     delimiters ({}, (), or []); wrap a complete, balanced fragment."
                        .to_owned(),
                );
            }
            let byte_start = line_start(lexed.src, comments[start_ci].start);
            let byte_end = comments[end_ci].end;
            blocks.push(InsertBlock {
                byte_start,
                byte_end,
                code_start,
                code_end,
            });
            p = q + 1;
        } else if tag == INSERT_END {
            return err_at(
                line_of(lexed.src, comments[ci].start),
                "Found `// cddl-codegen:insert-end` without a matching \
                 `// cddl-codegen:insert-start`."
                    .to_owned(),
            );
        } else {
            // A bare `unpreserved-comment` marker not backed by the `compile_error!` shape (it was
            // not claimed by `recognize_sentinels`), or any unknown tag: a malformed structure, not a
            // user comment.
            return err_at(
                line_of(lexed.src, comments[ci].start),
                format!(
                    "Unrecognized reserved comment in the `cddl-codegen:` namespace (line: `{}`).",
                    comments[ci].text
                ),
            );
        }
    }
    // A block's tag/interior comments (own-line AND trailing) and interior code tokens are excluded
    // from the comment/code passes; recognize them by byte/index containment.
    let mut consumed = BTreeSet::new();
    let mut removed_code = BTreeSet::new();
    for b in &blocks {
        for (ci, cm) in comments.iter().enumerate() {
            if cm.start >= b.byte_start && cm.start < b.byte_end {
                consumed.insert(ci);
            }
        }
        for k in b.code_start..b.code_end {
            removed_code.insert(k);
        }
    }
    // A replace block's comments (tags, interior user comments, recorded-original lines) are consumed
    // too; its user-code tokens are NOT removed here — reconstruction substitutes them by the needle.
    for rb in &replace_blocks {
        for (ci, cm) in comments.iter().enumerate() {
            if cm.start >= rb.byte_start && cm.start < rb.byte_end {
                consumed.insert(ci);
            }
        }
    }
    Ok(BlockScan {
        blocks,
        replace_blocks,
        consumed,
        removed_code,
    })
}

/// Re-indent a captured verbatim block span `[byte_start, byte_end)` for placement at `target_indent`:
/// strip the block's own base indentation (its first line's leading whitespace) from every line and
/// re-apply `target_indent`, preserving relative nesting. A fixed point when the block already sits at
/// `target_indent` (the idempotency property), and rustfmt normalizes the rest on disk anyway.
/// `trailing_newline` controls whether the result ends with `\n` (insert blocks sit ABOVE a line, so
/// they need it; a replace splice puts the block IN PLACE of deleted tokens whose line's `\n` remains,
/// so it must not add one).
fn reindent_span(
    old_src: &str,
    byte_start: usize,
    byte_end: usize,
    target_indent: &str,
    trailing_newline: bool,
) -> String {
    let base = line_indent(old_src, byte_start);
    let text = &old_src[byte_start..byte_end];
    let lines: Vec<&str> = text.split('\n').collect();
    let mut out = String::with_capacity(text.len() + 8);
    for (i, line) in lines.iter().enumerate() {
        // Drop a trailing CR (a CRLF-converted prior output) so the inserted block does not carry a
        // stray `\r` into the LF-only `new`, mirroring the comment engine's CR strip.
        let line = line.strip_suffix('\r').unwrap_or(line);
        if !trailing_newline && i > 0 {
            out.push('\n');
        }
        if line.trim().is_empty() {
            if trailing_newline {
                out.push('\n');
            }
            continue;
        }
        let rel = line.strip_prefix(base).unwrap_or(line);
        out.push_str(target_indent);
        out.push_str(rel);
        if trailing_newline {
            out.push('\n');
        }
    }
    out
}

/// Re-indent an insert block above a token at `target_indent` (trailing newline included).
fn reindent_block(old_src: &str, b: &InsertBlock, target_indent: &str) -> String {
    reindent_span(old_src, b.byte_start, b.byte_end, target_indent, true)
}

/// Normalize rustfmt's canonical marker placement back to the own-line form the scanner expects.
///
/// rustfmt folds a `// cddl-codegen:<tag>` comment that trails the closing `}` of a match's LAST arm
/// onto that `}` as a trailing comment (`} // cddl-codegen:replaces`), re-indenting the following
/// recorded-original / `:replace-end` lines as an aligned continuation block. The tool's own
/// [`rustfmt_generated_string`](crate::generation::export::rustfmt_generated_string) pass writes that
/// folded form on any regen that splices a match-tail replace/insert block, so the next regen would
/// read a marker the own-line scan gate can't see. We unfold at the shared entry of both scan paths
/// so both spellings parse and the rustfmt'd on-disk form is a stable fixed point.
///
/// For every LINE comment that is NOT own-line but IS in the reserved `cddl-codegen:` namespace
/// (`cddl_tag` matches — block comments can never match, and a namespace lookalike inside a string
/// literal is inert because the lexer is literal-aware), we insert a newline + the trailing line's
/// leading indentation immediately before the marker, moving it onto its own line below the code it
/// trailed. The `}` (or other tail code) it trailed stays on the line above, so it becomes part of the
/// user section being closed — the same split as if the marker had been emitted own-line.
///
/// Returns the normalized text (a borrowed `Cow` when nothing folded — the dominant case) plus, for
/// each inserted newline, the 1-based line it created in the normalized text, so a `PreserveError`'s
/// line can be mapped back to the on-disk line via [`map_disk_line`].
fn unfold_trailing_markers(src: &str) -> Result<(Cow<'_, str>, Vec<usize>), PreserveError> {
    let lexed = lex(src)?;
    // Byte offset of each trailing marker's comment start, in source order (comments lex in order).
    let cuts: Vec<usize> = lexed
        .comments
        .iter()
        .filter(|cm| !cm.own_line && cddl_tag(cm.text).is_some())
        .map(|cm| cm.start)
        .collect();
    if cuts.is_empty() {
        return Ok((Cow::Borrowed(src), Vec::new()));
    }
    let mut out = String::with_capacity(src.len() + cuts.len() * 8);
    let mut inserted_lines: Vec<usize> = Vec::new();
    let mut line_count = 0usize; // running count of `\n` emitted into `out`
    let mut prev = 0usize;
    for pos in cuts {
        let chunk = &src[prev..pos];
        out.push_str(chunk);
        line_count += chunk.bytes().filter(|&b| b == b'\n').count();
        out.push('\n');
        line_count += 1;
        out.push_str(line_indent(src, pos));
        // The marker now begins on the line just started (== newlines so far + 1).
        inserted_lines.push(line_count + 1);
        prev = pos;
    }
    out.push_str(&src[prev..]);
    Ok((Cow::Owned(out), inserted_lines))
}

/// Map a 1-based line in the unfolded text back to the on-disk line. Each unfold inserted exactly one
/// newline (recorded in `inserted_lines` as the normalized line it created), so an on-disk line is the
/// normalized line minus the count of inserted lines at or before it — a marker moved onto its own new
/// line maps back to the on-disk line it trailed.
fn map_disk_line(normalized_line: usize, inserted_lines: &[usize]) -> usize {
    let shift = inserted_lines
        .iter()
        .filter(|&&nl| nl <= normalized_line)
        .count();
    normalized_line - shift
}

/// Overlay the user comments from `old` onto the freshly generated `new`. See the module docs for
/// the tiered anchoring. Pure: no I/O; output is a function of `(old, new)`.
///
/// `old` is first normalized by [`unfold_trailing_markers`] so rustfmt-folded match-tail markers parse
/// like their own-line spelling; a `PreserveError`'s line is mapped back to the on-disk line here, the
/// one place that boundary is crossed. `new` (freshly generated) never carries markers, so it is not
/// normalized.
pub fn preserve(old: &str, new: &str) -> Result<Preserved, PreserveError> {
    let (normalized, inserted_lines) = unfold_trailing_markers(old)?;
    preserve_inner(&normalized, new).map_err(|mut e| {
        if let Some(l) = e.line {
            e.line = Some(map_disk_line(l, &inserted_lines));
        }
        e
    })
}

/// The merge proper, operating on already-unfolded `old`. See [`preserve`].
fn preserve_inner(old: &str, new: &str) -> Result<Preserved, PreserveError> {
    let old_lex = lex(old)?;
    let new_lex = lex(new)?;

    // 1. Recognize prior-run fail-loudly blocks (carried forward verbatim) and user insert blocks
    //    (namespace-reserved), then build the virtual pristine old stream. Both remove code tokens
    //    that were not the generator's output — a sentinel `compile_error!` block, and an insert
    //    block's interior user code — so the identity tier can still fire and anchors stay sound.
    let sentinel = recognize_sentinels(&old_lex);
    let block_scan = scan_blocks(&old_lex, &sentinel.sentinel_comment)?;
    let sentinel_comment = &sentinel.sentinel_comment;
    let carried_blocks = &sentinel.carried_blocks;
    let mut removed_code: BTreeSet<usize> = sentinel.removed_code.clone();
    removed_code.extend(block_scan.removed_code.iter().copied());

    // Lex each replace block's recorded original into its NEEDLE tokens (owned via the block's
    // `needle_text`, which outlives this borrow). Validate here (all hard errors, pre-splice): the
    // recorded original must lex, be non-empty (a section that lexes to zero code tokens — e.g. all
    // `// //` lines — records nothing to place against), be never-negative (never close a delimiter it
    // does not open), and change delimiter depth by the SAME net amount as the user section. Equal net
    // delta means every token downstream of the splice keeps its exact delimiter depth, so top-level
    // item splitting survives even a wrong needle (absolute balance was sufficient but not necessary).
    let needle_lexed: Vec<Lexed> = block_scan
        .replace_blocks
        .iter()
        .map(|rb| lex(&rb.needle_text))
        .collect::<Result<_, _>>()?;
    for (nl, rb) in needle_lexed.iter().zip(block_scan.replace_blocks.iter()) {
        let line = line_of(old, rb.byte_start);
        if nl.code.is_empty() {
            return err_at(
                line,
                "The `// cddl-codegen:replaces` section records no generated code (it lexes to zero \
                 tokens); record the exact code being replaced under `replaces`."
                    .to_owned(),
            );
        }
        let needle_deltas = match delim_deltas(&nl.code) {
            Some(d) => d,
            None => {
                return err_at(
                    line,
                    "The recorded original of the `// cddl-codegen:replace-start` block closes a \
                     delimiter ({}, (), or []) it does not open; record a fragment that never dips \
                     below its starting depth."
                        .to_owned(),
                );
            }
        };
        // The user section was already checked never-negative in `scan_blocks`, so its deltas are
        // `Some`; pair them against the needle's so the surrounding generated code stays balanced.
        let user_deltas = delim_deltas(&old_lex.code[rb.user_code_start..rb.user_code_end])
            .expect("replace-block user section is never-negative (checked in scan_blocks)");
        if needle_deltas != user_deltas {
            return err_at(
                line,
                "The user section and the recorded original of the `// cddl-codegen:replace-start` \
                 block must change delimiter depth identically (their net {}, (), and [] deltas must \
                 match) so the surrounding generated code stays balanced."
                    .to_owned(),
            );
        }
    }

    // Build the virtual pristine old stream + a general anchor remap: for an original anchor `a`,
    // `remap[a]` is its index into the virtual stream. A sentinel `compile_error!` block and an
    // insert-block interior contribute nothing (removed); a replace block's user-code span is
    // SUBSTITUTED by the recorded original's needle (so the identity tier still fires when generator
    // output is unchanged, and the needle regains both-sides uniqueness). Substitution makes the
    // remap EXPAND/CONTRACT, not just contract — it need only be correct at non-interior positions,
    // since a block's interior comments are consumed and no anchor points inside a substituted span.
    let replace_at: BTreeMap<usize, usize> = block_scan
        .replace_blocks
        .iter()
        .enumerate()
        .map(|(bi, rb)| (rb.user_code_start, bi))
        .collect();
    let mut remap = vec![0usize; old_lex.code.len() + 1];
    let mut old_code: Vec<CodeTok> = Vec::new();
    let mut a = 0;
    while a < old_lex.code.len() {
        remap[a] = old_code.len();
        if let Some(&bi) = replace_at.get(&a) {
            // Emit the needle in place of the user-code tokens `[user_code_start, user_code_end)`.
            // Interior positions `(a, end)` need no remap — a block's interior comments are consumed,
            // so no anchor ever points inside a substituted span.
            old_code.extend(needle_lexed[bi].code.iter().copied());
            let end = block_scan.replace_blocks[bi].user_code_end;
            if end > a {
                a = end;
                continue;
            }
            // Empty user section (end == a): the token at `a` is the following code token, emitted
            // below normally; `remap[a]` stays at the needle start (an anchor here lands above the
            // replaced region — the sound choice with no user tokens to disambiguate above/below).
        }
        if !removed_code.contains(&a) {
            old_code.push(old_lex.code[a]);
        }
        a += 1;
    }
    remap[old_lex.code.len()] = old_code.len();

    // The generator's own comments (the CODEGEN_HEADER banner, static-prelude comments, `.doc()`
    // renderings, …) appear identically in `new` at the same anchor, so they self-cancel: exclude
    // any old comment `new` already carries at that anchor. The same set drives the insertion-point
    // dedup below (a generator comment whose anchor merely SHIFTED re-anchors to exactly where `new`
    // already carries it — inserting there would duplicate it).
    let new_comment_keys: BTreeSet<(usize, &str)> = new_lex
        .comments
        .iter()
        .filter(|c| c.own_line)
        .map(|c| (c.anchor, c.text))
        .collect();
    // Trailing comments whose text `new` also carries somewhere cancel silently. INVARIANT: the
    // generator must emit NO trailing (end-of-line) comment on any row a spec change can delete —
    // such a comment would be stranded on the deleted row and re-injected here as a
    // `cddl-codegen:unpreserved-comment` compile_error trap that every further regen carries forward
    // (a self-perpetuating trap the user can only escape by hand-deleting the sentinel). Violating
    // it recreates that trap class. It DID regress once: `extern_interface_check.rs` and
    // `key_demand_assertions.rs` carried per-row `// <cddl>` markers; both were made banner-only,
    // pinned by `extern_interface_check_regen_over_deletion_no_trap` and
    // `extern_interface_check_has_no_trailing_row_comments`. This cancellation is therefore
    // defense-in-depth; it matches by exact text, not position, because trailing anchors shift with
    // any edit. Residual: a user trailing comment textually identical to one in `new` is skipped
    // rather than failed.
    let new_trailing_texts: BTreeSet<&str> = new_lex
        .comments
        .iter()
        .filter(|c| !c.own_line)
        .map(|c| c.text)
        .collect();
    // Anchors where `new` carries a doc comment: those positions are tool-owned (docs flow from the
    // CDDL/`@doc` DSL), so an old doc block re-anchoring there is stale tool output, not user text.
    let new_doc_anchors: BTreeSet<usize> = new_lex
        .comments
        .iter()
        .filter(|c| c.own_line && is_doc_comment(c.text))
        .map(|c| c.anchor)
        .collect();

    // Split old comments into: trailing (fail loudly unless generator-owned) and own-line user
    // comments (candidates).
    let mut trailing: Vec<&str> = Vec::new();
    let mut user_comments: Vec<Comment> = Vec::new();
    for (ci, cm) in old_lex.comments.iter().enumerate() {
        if sentinel_comment.contains(&ci) || block_scan.consumed.contains(&ci) {
            continue;
        }
        if !cm.own_line {
            if !new_trailing_texts.contains(cm.text) {
                trailing.push(cm.text);
            }
            continue;
        }
        let anchor = remap[cm.anchor];
        if new_comment_keys.contains(&(anchor, cm.text)) {
            continue; // generator comment — already present in new at the same position
        }
        user_comments.push(Comment { anchor, ..*cm });
    }

    // 2. Place each user comment. Insertions target byte offsets in `new`; unplaceable comments and
    //    the verbatim carried blocks become fail-loudly blocks at the top (after the header).
    let mut insertions: Vec<Insertion> = Vec::new();
    // (reason, original payload text, noun): the noun ("comment"/"code block") names what a
    // fail-loudly `compile_error!` traps, so a block does not misreport itself as a comment.
    let mut unplaceable: Vec<(String, String, &'static str)> = Vec::new();
    let mut order = 0usize;

    let identity = code_eq(&old_code, &new_lex.code);
    // Comment placement short-circuits on identity, but replace placement always needs the item
    // partition (it matches the enclosing item and locates the needle within it), so build items
    // whenever there is a replace block even under identity.
    let need_items = !identity || !block_scan.replace_blocks.is_empty();
    let old_items = if need_items {
        split_items(&old_code)
    } else {
        Vec::new()
    };
    let new_items = if need_items {
        split_items(&new_lex.code)
    } else {
        Vec::new()
    };
    // (kind, name) -> new item indices, in order (occurrence index disambiguates duplicates).
    let mut new_by_key: BTreeMap<(&str, &str), Vec<usize>> = BTreeMap::new();
    for (i, it) in new_items.iter().enumerate() {
        new_by_key
            .entry((it.kind.as_str(), it.name.as_str()))
            .or_default()
            .push(i);
    }
    // occurrence index of each old item among same-keyed old items, plus per-key totals (occurrence
    // matching is only sound when the same-key counts agree on both sides).
    let mut old_occ = vec![0usize; old_items.len()];
    let mut old_key_counts: BTreeMap<(&str, &str), usize> = BTreeMap::new();
    for (i, it) in old_items.iter().enumerate() {
        let c = old_key_counts
            .entry((it.kind.as_str(), it.name.as_str()))
            .or_default();
        old_occ[i] = *c;
        *c += 1;
    }

    // A replace block's needle must fall within a single top-level item of the virtual stream — a
    // recorded original that straddles a top-level item boundary can't be placed by the item matcher
    // and means malformed authoring: a hard error (checked here, after reconstruction, per the plan).
    for (bi, rb) in block_scan.replace_blocks.iter().enumerate() {
        let vstart = remap[rb.user_code_start];
        let vlen = needle_lexed[bi].code.len();
        let containing = old_items
            .iter()
            .find(|it| it.start <= vstart && vstart < it.end);
        match containing {
            Some(it) if vstart + vlen <= it.end => {}
            _ => {
                return err_at(
                    line_of(old, rb.byte_start),
                    "An `// cddl-codegen:replace-start` block's recorded original spans more than \
                     one top-level item; a replace block must stay within a single item."
                        .to_owned(),
                );
            }
        }
    }

    // Comments and insert blocks are placed by the same tiers; interleave them in source order so
    // ties at one target offset keep their original top-to-bottom order (e.g. an insert block
    // immediately above a comment).
    enum Placeable<'a> {
        Comment(&'a Comment<'a>),
        Block(usize),
    }
    let mut placeables: Vec<(usize, Placeable)> = Vec::new();
    for cm in &user_comments {
        placeables.push((cm.start, Placeable::Comment(cm)));
    }
    for (bi, b) in block_scan.blocks.iter().enumerate() {
        placeables.push((b.byte_start, Placeable::Block(bi)));
    }
    placeables.sort_by_key(|(s, _)| *s);

    // Anchor a code index into `new` through the tiers (identity → per-item → unique-statement).
    let place = |a: usize| -> Result<Option<usize>, String> {
        if a >= old_code.len() {
            Ok(None) // dangling anchor at end of file
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
                &old_key_counts,
            )
        }
    };

    // 2a. Place each replace block: match the enclosing item into `new`, locate the needle uniquely
    //     on BOTH sides, and splice the verbatim block over the matched byte range. A failure (drift,
    //     ambiguity, vanished/reshaped item) traps the whole block in a fail-loudly `compile_error!`.
    let mut splices: Vec<(usize, usize, String)> = Vec::new(); // (delete_start, delete_end, text)
    for (bi, rb) in block_scan.replace_blocks.iter().enumerate() {
        let needle = &needle_lexed[bi].code;
        let vstart = remap[rb.user_code_start];
        match place_replace(
            vstart,
            needle,
            &old_code,
            &new_lex.code,
            &old_items,
            &new_items,
            &new_by_key,
            &old_occ,
            &old_key_counts,
        ) {
            Ok((nstart, nlen)) => {
                let first = &new_lex.code[nstart];
                let last = &new_lex.code[nstart + nlen - 1];
                let ls = line_start(new, first.start);
                // Byte-range splice, not line-based: a needle can begin mid-line (one-liner match
                // arm). Delete from the line start only when the first token IS the line's first
                // token (so its indentation is replaced cleanly); otherwise from the token itself,
                // wrapping the block in newlines so its tag lines stay own-line (rustfmt tidies).
                let at_line_start = new[ls..first.start].trim().is_empty();
                let after_end = new[last.end..]
                    .find('\n')
                    .map(|p| last.end + p)
                    .unwrap_or(new.len());
                let at_line_end = new[last.end..after_end].trim().is_empty();
                let indent = line_indent(new, first.start);
                let delete_start = if at_line_start { ls } else { first.start };
                let body = reindent_span(old, rb.byte_start, rb.byte_end, indent, false);
                let mut text = String::new();
                if !at_line_start {
                    text.push('\n');
                }
                text.push_str(&body);
                if !at_line_end {
                    text.push('\n');
                    text.push_str(indent);
                }
                splices.push((delete_start, last.end, text));
            }
            Err(reason) => unplaceable.push((
                reason,
                old[rb.byte_start..rb.byte_end].to_owned(),
                "code block",
            )),
        }
    }
    // Successful splice ranges drive the op-composition conflict rule: an insertion whose target
    // offset falls STRICTLY INSIDE a deleted range (its referent is being replaced) fails loudly.
    let delete_ranges: Vec<(usize, usize)> = splices.iter().map(|(s, e, _)| (*s, *e)).collect();
    let inside_delete =
        |off: usize| -> bool { delete_ranges.iter().any(|&(s, e)| s < off && off < e) };

    for (_, p) in placeables {
        match p {
            Placeable::Comment(cm) => {
                match place(cm.anchor) {
                    Ok(t) => {
                        let t = t.unwrap_or(new_lex.code.len());
                        // Insertion-point dedup: a generator comment whose anchor shifted (any edit
                        // earlier in the file) re-anchors to exactly where `new` already carries the
                        // identical comment — inserting would duplicate it.
                        if new_comment_keys.contains(&(t, cm.text)) {
                            order += 1;
                            continue;
                        }
                        // Doc ownership: `new` documents this anchor, so an old doc block here is
                        // stale tool output (the user channel for doc text is the CDDL/`@doc` DSL).
                        if is_doc_comment(cm.text) && new_doc_anchors.contains(&t) {
                            order += 1;
                            continue;
                        }
                        let (offset, indent) = if t >= new_lex.code.len() {
                            (new.len(), "")
                        } else {
                            let start = new_lex.code[t].start;
                            (line_start(new, start), line_indent(new, start))
                        };
                        // Op-composition conflict: this comment's referent is inside a span a replace
                        // block deletes. Fail loudly — the user must move it into the replace block.
                        if inside_delete(offset) {
                            unplaceable.push((
                                "Its anchor lies inside code replaced by a \
                                 `// cddl-codegen:replace-start` block; move it into that block."
                                    .to_owned(),
                                cm.text.to_owned(),
                                "comment",
                            ));
                            order += 1;
                            continue;
                        }
                        insertions.push(Insertion {
                            offset,
                            order,
                            text: format!("{indent}{}\n", cm.text),
                        });
                    }
                    // Doc ownership extends to UNPLACEABLE doc comments: deleting a documented type
                    // must not trap the tool's own `///` lines (which anchor to the vanished item) in
                    // compile_error blocks — doc text's channel is the CDDL/`@doc` DSL, so doc blocks
                    // drop rather than fail loudly (the same trade as documented anchors; a user doc
                    // on a vanished item drops with them).
                    Err(_) if is_doc_comment(cm.text) => {}
                    Err(reason) => unplaceable.push((reason, cm.text.to_owned(), "comment")),
                }
            }
            Placeable::Block(bi) => {
                let b = &block_scan.blocks[bi];
                // The anchor is the code token following the block, remapped onto the virtual stream.
                match place(remap[b.code_end]) {
                    Ok(t) => {
                        let t = t.unwrap_or(new_lex.code.len());
                        let (offset, indent) = if t >= new_lex.code.len() {
                            (new.len(), "")
                        } else {
                            let start = new_lex.code[t].start;
                            (line_start(new, start), line_indent(new, start))
                        };
                        // Same op-composition conflict rule as for comments (an insert block whose
                        // following anchor is inside a replaced span fails loudly).
                        if inside_delete(offset) {
                            unplaceable.push((
                                "Its anchor lies inside code replaced by a \
                                 `// cddl-codegen:replace-start` block; move it into that block."
                                    .to_owned(),
                                old[b.byte_start..b.byte_end].to_owned(),
                                "code block",
                            ));
                            order += 1;
                            continue;
                        }
                        insertions.push(Insertion {
                            offset,
                            order,
                            text: reindent_block(old, b, indent),
                        });
                    }
                    // An unplaceable block is NOT left in place (its user tokens would count as a
                    // user edit on the next regen). Its ENTIRE text goes into the standard
                    // fail-loudly payload, so it carries forward verbatim like an unplaceable comment.
                    Err(reason) => unplaceable.push((
                        reason,
                        old[b.byte_start..b.byte_end].to_owned(),
                        "code block",
                    )),
                }
            }
        }
        order += 1;
    }
    for t in trailing {
        unplaceable.push((
            "It is a trailing (end-of-line) comment; move it to its own line above the code to \
             preserve it."
                .to_owned(),
            t.to_owned(),
            "comment",
        ));
    }

    // Nothing to overlay → the pristine content is byte-identical to today.
    if insertions.is_empty()
        && unplaceable.is_empty()
        && carried_blocks.is_empty()
        && splices.is_empty()
    {
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
    // The merge engine is now a set of non-overlapping delete+insert ops on `new` (an insertion is a
    // zero-width delete). Each op is `(start, end, group, order, text)`: bytes `[start, end)` are
    // removed and `text` inserted at `start`. Group orders ties at one offset: top-of-file blocks (0)
    // before placed insertions (1) before replace splices (2), so an insert block/comment anchored at
    // a splice's start byte lands ABOVE the spliced code.
    let mut top_order = 0usize;
    let mut all: Vec<(usize, usize, usize, usize, String)> = Vec::new();
    for block in carried_blocks {
        all.push((top_offset, top_offset, 0, top_order, format!("{block}\n")));
        top_order += 1;
    }
    for (reason, original, noun) in &unplaceable {
        all.push((
            top_offset,
            top_offset,
            0,
            top_order,
            format!("{}\n", sentinel_block(reason, original, noun)),
        ));
        top_order += 1;
    }
    for ins in insertions {
        all.push((ins.offset, ins.offset, 1, ins.order, ins.text));
    }
    for (i, (delete_start, delete_end, text)) in splices.into_iter().enumerate() {
        all.push((delete_start, delete_end, 2, i, text));
    }
    // Sort by start offset, then group, then push order.
    all.sort_by(|x, y| x.0.cmp(&y.0).then(x.2.cmp(&y.2)).then(x.3.cmp(&y.3)));

    let mut content = String::with_capacity(new.len() + 64);
    let mut prev = 0;
    for (start, end, _, _, text) in &all {
        // Deletes are non-overlapping and no insertion lands strictly inside one (the conflict rule),
        // so ops advance monotonically. A `start < prev` here means two deletes overlap — defensive,
        // should be unreachable given both-sides uniqueness + non-straddling — surface it, don't panic.
        if *start < prev {
            return err(
                "internal: overlapping replace splices while composing the preservation overlay",
            );
        }
        content.push_str(&new[prev..*start]);
        content.push_str(text);
        prev = *end;
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
    old_key_counts: &BTreeMap<(&str, &str), usize>,
) -> Result<Option<usize>, String> {
    let oi = match old_items.iter().position(|it| it.start <= a && a < it.end) {
        Some(oi) => oi,
        None => return Err("It could not be attached to any generated item.".to_owned()),
    };
    let ni = match_new_item(oi, old_items, new_by_key, old_occ, old_key_counts)?;
    let item = &old_items[oi];
    let nitem = &new_items[ni];
    let old_slice = &old_code[item.start..item.end];
    let new_slice = &new_code[nitem.start..nitem.end];
    let unchanged = code_eq(old_slice, new_slice);

    // Comment sitting above the item (its first token): re-attach above the matched item even if the
    // body changed — such a comment is about the item, not a body line. Exception: with several
    // same-keyed items whose bodies changed, occurrence order is the only tiebreak and a canonical
    // reorder would silently retarget the comment — refuse.
    if a == item.start {
        let group_len = new_by_key
            .get(&(item.kind.as_str(), item.name.as_str()))
            .map(Vec::len)
            .unwrap_or(0);
        if group_len > 1 && !unchanged {
            return Err(format!(
                "It sat above one of {} same-named `{} {}` items whose generated code changed, so \
                 its owner cannot be re-identified.",
                group_len, item.kind, item.name
            ));
        }
        return Ok(Some(nitem.start));
    }

    let rel = a - item.start;

    // Per-item identity: the item's body is unchanged → transfer at the same relative index.
    if unchanged {
        return Ok(Some(nitem.start + rel));
    }

    // Unique-statement tier: the annotated statement must appear exactly once on BOTH sides. Unique
    // in `new` alone is not enough: with two identical old statements (one deleted), the survivor is
    // unique in `new` and the deleted line's comment would silently re-attach to it.
    let run = statement_run(old_slice, rel);
    if find_subsequence(old_slice, run).len() == 1 {
        let matches = find_subsequence(new_slice, run);
        if matches.len() == 1 {
            return Ok(Some(nitem.start + matches[0]));
        }
    }
    Err(format!(
        "It was attached inside `{} {}`, whose generated code changed.",
        item.kind, item.name
    ))
}

/// Match old item `oi` to its counterpart in `new` by (kind, name) + occurrence — the shared item
/// matcher for both the comment tiers and replace placement. Errs (naming the item) when the item
/// vanished or its same-key count changed (occurrence matching then unsound).
fn match_new_item(
    oi: usize,
    old_items: &[Item],
    new_by_key: &BTreeMap<(&str, &str), Vec<usize>>,
    old_occ: &[usize],
    old_key_counts: &BTreeMap<(&str, &str), usize>,
) -> Result<usize, String> {
    let item = &old_items[oi];
    let key = (item.kind.as_str(), item.name.as_str());
    let group = new_by_key.get(&key).map(Vec::as_slice).unwrap_or(&[]);
    if group.is_empty() {
        return Err(format!(
            "It was attached to `{} {}`, which no longer exists in the regenerated code.",
            item.kind, item.name
        ));
    }
    if group.len() != old_key_counts.get(&key).copied().unwrap_or(0) {
        return Err(format!(
            "It was attached to `{} {}`, but the number of same-named items changed in the \
             regenerated code.",
            item.kind, item.name
        ));
    }
    Ok(group[old_occ[oi]])
}

/// Place a replace block: match the enclosing item (containing the needle's virtual-stream start
/// `vstart`) into `new`, then anchor by one of two paths. First the ITEM-IDENTITY fast path: if the
/// whole enclosing item regenerated token-identically, position disambiguates — the block splices at
/// the same offset it occupied, so the recorded original need not be unique within the item (this is
/// what lets two different occurrences of a duplicated fragment both be replaced). Otherwise the strict
/// BOTH-SIDES-UNIQUENESS path: the needle must be unique in the virtual old item AND in the matched new
/// item (the same rule as the comment engine — unique-in-new alone would let a deleted duplicate's
/// block silently re-attach to the survivor). On success returns the matched new token run as
/// `(start_index, len)`; otherwise a fail-loudly reason (drift / ambiguity / vanished / reshaped item)
/// that names the item.
#[allow(clippy::too_many_arguments)]
fn place_replace(
    vstart: usize,
    needle: &[CodeTok],
    old_code: &[CodeTok],
    new_code: &[CodeTok],
    old_items: &[Item],
    new_items: &[Item],
    new_by_key: &BTreeMap<(&str, &str), Vec<usize>>,
    old_occ: &[usize],
    old_key_counts: &BTreeMap<(&str, &str), usize>,
) -> Result<(usize, usize), String> {
    let oi = old_items
        .iter()
        .position(|it| it.start <= vstart && vstart < it.end)
        .ok_or_else(|| {
            "Its recorded original could not be attached to any generated item.".to_owned()
        })?;
    let ni = match_new_item(oi, old_items, new_by_key, old_occ, old_key_counts)?;
    let item = &old_items[oi];
    let nitem = &new_items[ni];
    let old_slice = &old_code[item.start..item.end];
    let new_slice = &new_code[nitem.start..nitem.end];
    // Item-identity fast path. The virtual old item carries the needle at offset `rel` by
    // construction — the substitution put the recorded original there in place of the user tokens. So
    // when the whole enclosing item regenerated token-identically, the new item carries the recorded
    // original's exact tokens at exactly that offset: position — where the user's block physically sits
    // — disambiguates duplicated fragments perfectly, no uniqueness needed. Soundness is by
    // construction, not heuristic: a wrong or drifted needle makes `code_eq` false and falls through to
    // the strict both-sides-uniqueness path below, which fails loudly. The straddle check in the caller
    // (`vstart + vlen <= item.end`) plus token identity (equal length) keep the returned span in-bounds.
    if code_eq(old_slice, new_slice) {
        let rel = vstart - item.start;
        return Ok((nitem.start + rel, needle.len()));
    }
    // Both-sides uniqueness. Non-unique in the virtual old item = a deleted duplicate; the block's
    // referent is ambiguous, so fail loudly rather than guess (the deleted-duplicate hazard the comment
    // engine also refuses).
    if find_subsequence(old_slice, needle).len() != 1 {
        return Err(format!(
            "Its recorded original is not unique within `{} {}` (a deleted duplicate?), so which \
             occurrence it replaces is ambiguous.",
            item.kind, item.name
        ));
    }
    let matches = find_subsequence(new_slice, needle);
    match matches.len() {
        0 => Err(format!(
            "The generated code for `{} {}` changed, so its recorded original no longer appears \
             (drift). Re-review the block and re-record the original under `replaces`.",
            item.kind, item.name
        )),
        1 => Ok((nitem.start + matches[0], needle.len())),
        _ => Err(format!(
            "Its recorded original appears more than once in the regenerated `{} {}`, so which \
             occurrence it replaces is ambiguous.",
            item.kind, item.name
        )),
    }
}

/// The never-silent units of a source: the own-line NON-DOC user comments and the verbatim insert
/// blocks that a merge must not silently drop (each must appear in the output verbatim/re-indented or
/// `escape_for_rust_string`-transformed inside a `compile_error!`). Reuses the real sentinel/block
/// recognition so it stays correct as blocks evolve. Doc comments are excluded (they are tool-owned
/// and may legitimately drop); tool-generated comments (header/redefine notes) are harmless to
/// include — they survive because `new` carries them. The fixture harness asserts this property over
/// every blessed fixture. Returns an error only if `src` is unlexable or its tags are malformed.
///
/// `dead_code`-allowed: only the bin-only fixture harness (`src/tests/`) calls it, so the lib crate's
/// test build (which compiles this module but not `src/tests/`) sees it as unused.
#[cfg(test)]
#[allow(dead_code)]
pub(crate) fn never_silent_units(src: &str) -> Result<Vec<String>, PreserveError> {
    // Normalize rustfmt-folded match-tail markers exactly as [`preserve`] does at its entry, so a
    // folded-form fixture's blocks are recognized here too (this drives the harness's never-silent
    // property; an un-normalized scan would miss the folded block and fail the property).
    let (src, _) = unfold_trailing_markers(src)?;
    let src = src.as_ref();
    let lexed = lex(src)?;
    let sentinel = recognize_sentinels(&lexed);
    let scan = scan_blocks(&lexed, &sentinel.sentinel_comment)?;
    let mut units = Vec::new();
    for b in &scan.blocks {
        units.push(src[b.byte_start..b.byte_end].to_owned());
    }
    for rb in &scan.replace_blocks {
        units.push(src[rb.byte_start..rb.byte_end].to_owned());
    }
    for (ci, cm) in lexed.comments.iter().enumerate() {
        if !cm.own_line
            || sentinel.sentinel_comment.contains(&ci)
            || scan.consumed.contains(&ci)
            || is_doc_comment(cm.text)
        {
            continue;
        }
        units.push(cm.text.to_owned());
    }
    Ok(units)
}

#[cfg(test)]
mod tests {
    //! Only lexer-level cases stay inline — they test `lex`, not the merge. Every merge case lives in
    //! the file-fixture harness (`tests/preserve-fixtures/`, driven by
    //! `src/tests/preserve_fixture_tests.rs`), where the fixture name is the migrated test's name.
    use super::*;

    // A CODEGEN_HEADER-shaped banner, so tests exercise the self-cancel path the real files hit.
    const HEADER: &str = "// This file was code-generated using an experimental CDDL to rust tool:\n// https://github.com/dcSpark/cddl-codegen\n\n";

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
    fn raw_identifier_lexes_without_error() {
        // `r#type` is a raw identifier, not a malformed raw string; it must lex side-consistently.
        let src = format!("{HEADER}pub fn f() {{\n    let r#type = 1;\n    use_it(r#type);\n}}\n");
        let res = preserve(&src, &src).unwrap();
        assert!(!res.changed, "raw-ident file must self-preserve as a no-op");
        assert_eq!(res.content, src);
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
