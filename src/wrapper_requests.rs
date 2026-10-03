//! Strict parser for a consumer's committed `wasm/src/generated/borrowed_collections.rs` sidecar —
//! the machine half a workspace **dependency** re-reads via `--wrapper-requests <consumer>=<path>`
//! (W2 of the workspace wrapper-placement feature). The consumer emits this file (W1,
//! `generation/export.rs`); the dep parses it here, unions the requested shapes across consumers, and hosts
//! every requested wrapper in its own `requested_collections.rs`.
//!
//! ## Why strict
//!
//! A request channel must never silently tolerate stray content: a hand-edited or drifted sidecar
//! that the dep quietly ignored would drop a borrow, and the consumer would then fail to link with
//! no actionable pointer. So the ONLY content this parser accepts is exactly what the frozen W1
//! emitter produces (`generation/export.rs`, the `borrowed_collections.rs` block):
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
use crate::comment_preserve::{ReservedComment, ReservedTag};
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
/// the on-disk path, used only to make the hard-error messages actionable. Returns `Err` (hard
/// error) on any content outside the frozen W1 grammar; returns every well-formed entry (the caller
/// filters by dep).
pub fn parse_sidecar(contents: &str, file: &str) -> Result<Vec<WrapperRequestEntry>, String> {
    // A `compile_error!` anywhere is the surest sign of a trapped sidecar (the preservation overlay
    // emits one inside its `unpreserved-comment` blocks). Reject before any structural parse.
    if contents.contains("compile_error!") {
        return Err(format!(
            "--wrapper-requests {file}: the sidecar contains a `compile_error!` — it is a trapped or \
             drifted generated file (an edit-preservation `unpreserved-comment` block), which must \
             never be silently consumed. Regenerate the consumer crate to clear it."
        ));
    }

    let logical = flatten_overlay_blocks(contents, file, "--wrapper-requests")?;

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
                return Err(SHAPES_TABLE.refusal(
                    file,
                    "unexpected comment inside `BORROWED_SHAPES`",
                    trimmed,
                ));
            }
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(shape_entries(SHAPES_TABLE.parse_item(&const_item, file)?));
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
                return Err(SHAPES_TABLE.refusal(
                    file,
                    "unexpected line inside `mod borrowed`",
                    trimmed,
                ));
            }
            continue;
        }
        // Top level.
        if KNOWN_COMMENTS.contains(&trimmed) {
            continue;
        }
        if trimmed.starts_with("//") {
            return Err(SHAPES_TABLE.refusal(file, "unexpected comment", trimmed));
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
        // opener with one row per line — `TableGrammar::parse_item` handles all of them uniformly.
        if trimmed.starts_with("pub(crate) const BORROWED_SHAPES") {
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(shape_entries(SHAPES_TABLE.parse_item(&const_item, file)?));
                const_item.clear();
            } else {
                in_const = true;
            }
            continue;
        }
        return Err(SHAPES_TABLE.refusal(file, "unexpected item", trimmed));
    }

    if in_mod {
        return Err(SHAPES_TABLE.refusal(file, "unterminated `mod borrowed` block", ""));
    }
    if in_const {
        return Err(SHAPES_TABLE.refusal(
            file,
            "unterminated `BORROWED_SHAPES` table (missing `];`)",
            "",
        ));
    }

    Ok(entries)
}

/// Map `SHAPES_TABLE` rows (three fields each, enforced by the table's arity) to entries.
fn shape_entries(rows: Vec<Vec<String>>) -> impl Iterator<Item = WrapperRequestEntry> {
    rows.into_iter().map(|fields| WrapperRequestEntry {
        dep: fields[0].clone(),
        name: fields[1].clone(),
        shape: fields[2].clone(),
    })
}

/// Strip the edit-preservation overlay scaffolding (`comment_preserve.rs` marker structures) to a
/// flat list of logical lines the grammar scanner consumes. Insert blocks contribute their inner
/// lines verbatim (real payload rows); replace blocks contribute the USER section (before
/// `:replaces`) and drop the `//`-commented recorded original (the `:replaces` section). An
/// `unpreserved-comment` sentinel, or any unrecognized `cddl-codegen:` tag, is a hard error.
fn flatten_overlay_blocks(contents: &str, file: &str, flag: &str) -> Result<Vec<String>, String> {
    // Overlay state: whether we are inside a `replaces` section (recorded originals to drop).
    let mut in_replaces_original = false;
    let mut out = Vec::new();
    for raw in contents.lines() {
        // `comment_preserve`'s classifier is the one reader of the reserved namespace; it takes the
        // comment text from its `//`, so the raw line is trimmed first.
        if let Some(reserved) = ReservedComment::parse(raw.trim()) {
            match reserved.tag {
                ReservedTag::InsertStart | ReservedTag::InsertEnd | ReservedTag::ReplaceStart => {
                    // Scaffolding lines: dropped.
                    continue;
                }
                ReservedTag::ReplaceEnd => {
                    // Scaffolding, and it closes any recorded-originals section.
                    in_replaces_original = false;
                    continue;
                }
                ReservedTag::Replaces => {
                    in_replaces_original = true;
                    continue;
                }
                // A `keep` marker declares USER COMMENT TEXT, not payload. It is deliberately NOT
                // dropped as scaffolding: this sidecar's grammar rejects every comment outside
                // `KNOWN_COMMENTS`, so letting the marker line fall through gives a `keep` comment
                // exactly the treatment the same comment gets unmarked — the "unexpected comment"
                // hard error, naming the offending line verbatim. Both `keep` forms land there
                // (the bare form on its own marker line, the inline form on its text), so a user
                // comment can never silently vanish from a machine-read sidecar.
                ReservedTag::Keep { .. } => {}
                ReservedTag::UnpreservedComment => {
                    return Err(format!(
                        "{flag} {file}: the sidecar contains a \
                         `// cddl-codegen:unpreserved-comment` sentinel — it is a trapped or drifted \
                         generated file, which must never be silently consumed. Regenerate the \
                         consumer crate to clear it."
                    ));
                }
                ReservedTag::Unknown => {
                    return Err(format!(
                        "{flag} {file}: unexpected reserved comment \
                         `// cddl-codegen:{}` in the sidecar.",
                        reserved.text
                    ));
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
    Ok(out)
}

/// The const-table half of a sidecar grammar. The two sidecars' tables differ only in these fields,
/// so one reader serves both, and every refusal names the flag, file kind and table it came from.
struct TableGrammar {
    /// The CLI flag that consumes the sidecar, which leads every refusal.
    flag: &'static str,
    /// The generated file the sidecar must be, named in every refusal's remedy.
    file_kind: &'static str,
    /// The const's name, as the refusals spell it.
    const_name: &'static str,
    /// Every accepted declaration up to the initializer `=`, whitespace-normalized.
    headers: &'static [&'static str],
    /// The refusal for a declaration outside `headers`.
    header_refusal: &'static str,
    /// Every accepted number of string literals per row.
    arities: &'static [usize],
    /// The refusal for a row whose length is outside `arities`.
    arity_refusal: &'static str,
}

/// `borrowed_collections.rs`: `("<dep>", "<name>", "<shape>")` rows.
const SHAPES_TABLE: TableGrammar = TableGrammar {
    flag: "--wrapper-requests",
    file_kind: "borrowed_collections.rs",
    const_name: "BORROWED_SHAPES",
    headers: &["pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)]"],
    header_refusal: "unexpected `BORROWED_SHAPES` declaration (the type must be exactly `&[(&str, &str, &str)]`)",
    arities: &[3],
    arity_refusal: "malformed BORROWED_SHAPES row (a row must be exactly three string literals: dep, name, shape)",
};

/// `borrowed_key_types.rs`. Two declarations are accepted: the frozen two-column `&[(&str, &str)]`
/// (all rows bare — byte-identical to pre-flavor sidecars) and the three-column
/// `&[(&str, &str, &str)]` (rows carry a flavor token). Rows of either length are accepted under
/// either declaration, so an old two-column-typed table with only bare rows and a new
/// three-column-typed table both round-trip.
const KEY_TYPES_TABLE: TableGrammar = TableGrammar {
    flag: "--key-requests",
    file_kind: "borrowed_key_types.rs",
    const_name: "BORROWED_KEY_TYPES",
    headers: &[
        "pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)]",
        "pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)]",
    ],
    header_refusal: "unexpected `BORROWED_KEY_TYPES` declaration (the type must be `&[(&str, &str)]` or `&[(&str, &str, &str)]`)",
    arities: &[2, 3],
    arity_refusal: "malformed BORROWED_KEY_TYPES row (a row must be two string literals — dep, ident — or three, adding a flavor)",
};

impl TableGrammar {
    /// The sidecar grammar's refusal funnel: builds the diagnostic, which every call site returns as
    /// the `Err` of its own `Result`. Not diverging — a refusal here travels the generator's error
    /// channel so `--config`'s mid-run wrapper can name the crates it had already regenerated before
    /// it.
    fn refusal(&self, file: &str, what: &str, offending: &str) -> String {
        let (flag, file_kind) = (self.flag, self.file_kind);
        if offending.is_empty() {
            return format!(
                "{flag} {file}: {what}. The sidecar must be an unmodified, tool-generated `{file_kind}`."
            );
        }
        format!(
            "{flag} {file}: {what}: {offending:?}. The sidecar must be an unmodified, \
             tool-generated `{file_kind}`."
        )
    }

    /// Parse the complete const item (header, `=`, `&[ … ];`) into rows of string literals. The
    /// header up to the initializer `=` must be one of the frozen declarations (whitespace-normalized
    /// — rustfmt may wrap the initializer onto the next line); the initializer must be a `&[ … ]`
    /// array expression. The first `=` in the item IS the initializer's: the type annotation contains
    /// none, and row strings (a shape can contain `=>`) only occur after it.
    fn parse_item(&self, item: &str, file: &str) -> Result<Vec<Vec<String>>, String> {
        let name = self.const_name;
        let Some(eq) = item.find('=') else {
            return Err(self.refusal(
                file,
                &format!("malformed `{name}` item (missing `=`)"),
                item,
            ));
        };
        let header: String = item[..eq].split_whitespace().collect::<Vec<_>>().join(" ");
        if !self.headers.contains(&header.as_str()) {
            return Err(self.refusal(file, self.header_refusal, &header));
        }
        let init = item[eq + 1..].trim();
        let Some(body) = init
            .strip_prefix("&[")
            .and_then(|rest| rest.trim_end().strip_suffix("];"))
        else {
            return Err(self.refusal(
                file,
                &format!("malformed `{name}` initializer (expected `&[ … ];`)"),
                init,
            ));
        };
        self.parse_body(body, file)
    }

    /// Tokenize the raw array body (everything between `= &[` and `];`) into rows. Strict tuple
    /// grammar: `( "<a>" , "<b>" [, "<c>"] )` with an optional trailing comma, tuples separated by
    /// commas, each row's length one of `arities`. Comments never reach here (own-line ones
    /// hard-error in the caller; a trailing `// …` after a row surfaces as an unexpected token
    /// below). Any deviation — a wrong-length tuple, an unterminated literal, a stray token — is a
    /// hard error (a mangled sidecar must be loud).
    fn parse_body(&self, body: &str, file: &str) -> Result<Vec<Vec<String>>, String> {
        let name = self.const_name;
        let chars: Vec<char> = body.chars().collect();
        let mut i = 0;
        let mut rows = Vec::new();
        loop {
            i = skip_trivia(&chars, i);
            if i >= chars.len() {
                break;
            }
            if chars[i] != '(' {
                return Err(self.refusal(
                    file,
                    &format!("unexpected token in {name} (expected a `(...)` row)"),
                    &tail_snippet(&chars, i),
                ));
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
                    return Err(self.refusal(
                        file,
                        &format!("malformed {name} row (expected a string literal)"),
                        &tail_snippet(&chars, i),
                    ));
                }
                let (s, next) = read_str_lenient(&chars, i).ok_or_else(|| {
                    self.refusal(file, &format!("unterminated string literal in {name}"), "")
                })?;
                fields.push(s);
                i = skip_trivia(&chars, next);
                if i < chars.len() && chars[i] == ',' {
                    i += 1; // field separator (or trailing comma before ')')
                } else if i < chars.len() && chars[i] == ')' {
                    i += 1; // consume ')'
                    break;
                } else {
                    return Err(self.refusal(
                        file,
                        &format!("malformed {name} row (expected `,` or `)`)"),
                        &tail_snippet(&chars, i),
                    ));
                }
            }
            if !self.arities.contains(&fields.len()) {
                return Err(self.refusal(file, self.arity_refusal, &format!("{fields:?}")));
            }
            rows.push(fields);
            // Optional comma between rows.
            i = skip_trivia(&chars, i);
            if i < chars.len() && chars[i] == ',' {
                i += 1;
            }
        }
        Ok(rows)
    }
}

/// Advance past whitespace only. Deliberately does NOT skip `//` comments: the emitter writes none
/// inside a const body (the column legends live in the banners), so a comment reaching the
/// tokenizer is stray content that must surface as an unexpected-token hard error.
fn skip_trivia(chars: &[char], mut i: usize) -> usize {
    while i < chars.len() && chars[i].is_whitespace() {
        i += 1;
    }
    i
}

/// A short forward snippet of the remaining input, for error messages.
fn tail_snippet(chars: &[char], i: usize) -> String {
    let end = (i + 40).min(chars.len());
    chars[i..end].iter().collect::<String>()
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
        for row in scan_borrowed_rows_lenient(&contents) {
            if row.dep.replace('-', "_") != my_lib {
                continue;
            }
            for ident in map_key_cddl_idents(&row.shape) {
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
        types.mark_key_demand(ident, crate::comment_ast::DemandSet::BARE);
    }
}

/// One tolerantly-scanned `BORROWED_SHAPES` row: `(dep, wrapper name, shape)`.
pub(crate) struct ScannedRow {
    pub dep: String,
    pub name: String,
    pub shape: String,
}

/// Tolerant scan of a `borrowed_collections.rs` sidecar for its `BORROWED_SHAPES` rows. Deliberately
/// NOT the strict grammar (`parse_sidecar`): `//` comment lines are dropped first (so an
/// edit-preservation overlay's `:replaces` recorded-original rows never seed), then the remaining
/// text after the `BORROWED_SHAPES` marker is tokenized into parenthesized string-literal groups.
/// Never panics — malformed content yields fewer rows, leaving the strict diagnosis to
/// `emit_requested_collections`, which stays the single owner of the W2 hard errors.
///
/// Lenience is the right policy for both readers. The pre-finalize `used_as_key` seed must not fire
/// a second, differently-worded diagnostic for content the strict parser is about to reject; and the
/// config's committed-state verdict must not turn a malformed sidecar into a build failure it has no
/// standing to diagnose — under-reading there costs a missed verdict, never a false one.
pub(crate) fn scan_borrowed_rows_lenient(contents: &str) -> Vec<ScannedRow> {
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
            rows.push(ScannedRow {
                dep: literals[0].clone(),
                name: literals[1].clone(),
                shape: literals[2].clone(),
            });
        }
    }
    rows
}

/// What one line of a dependency's committed `collections.rs` wrapper index is.
///
/// The grammar lives here, beside the sidecar grammars it pairs with, because two readers need it
/// under different POLICIES and a second copy of a format is how the two drift. The dep-side reader
/// (`load_extern_wrapper_indices`) hard-errors on [`Self::Unknown`] — a silently-tolerated stray line
/// there would disable deferral and reintroduce the duplicate-symbol link error. The config's
/// committed-state verdict ignores it, because a malformed index is not a fact it has standing to
/// fail a build over.
pub(crate) enum CollectionIndexLine {
    /// Blank or a comment: carries no wrapper either way.
    Ignored,
    /// `pub use <path>::<Name>;` — the dep provides this wrapper class.
    Export(String),
    /// Anything else. The index is a generated file, so this means it was hand-edited or drifted.
    Unknown,
}

/// Classify one line of a `collections.rs` wrapper index. The re-export shape is fixed
/// (`pub use <path>::<Name>;`), and the class name is the segment after the last `::`.
pub(crate) fn classify_collection_index_line(line: &str) -> CollectionIndexLine {
    let line = line.trim();
    if line.is_empty() || line.starts_with("//") {
        return CollectionIndexLine::Ignored;
    }
    line.strip_prefix("pub use ")
        .and_then(|rest| rest.strip_suffix(';'))
        .and_then(|path| path.rsplit("::").next())
        .filter(|name| !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_'))
        .map_or(CollectionIndexLine::Unknown, |name| {
            CollectionIndexLine::Export(name.to_owned())
        })
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
    // A collection's trailing duplicate-policy marker (` @duplicates reject` after `]`,
    // ` @duplicates preserve` after `}`), which `render_wrapper_shape` also writes on NESTED
    // collections. Consumed when present so the enclosing collection's closer is still found.
    let eat_marker = |pos: &mut usize, marker: &str| {
        let mut at = *pos;
        while at < chars.len() && chars[at].is_whitespace() {
            at += 1;
        }
        let want: Vec<char> = marker.chars().collect();
        if chars[at..].starts_with(&want) {
            *pos = at + want.len();
        }
    };
    skip_ws(pos);
    match chars.get(*pos)? {
        '[' => {
            *pos += 1;
            skip_ws(pos);
            // The full occurrence grammar the strict reader accepts (`*`, `+`, `?`, `*N`, `N*`,
            // `N*M`): one owner, so a bounded shape seeds exactly like a loose one.
            crate::generation::read_occurrence(chars, pos)?;
            let inner = parse_shape_node(chars, pos)?;
            skip_ws(pos);
            if chars.get(*pos) != Some(&']') {
                return None;
            }
            *pos += 1;
            eat_marker(pos, crate::generation::REJECT_MARKER);
            Some(ShapeNode::List(Box::new(inner)))
        }
        '{' => {
            *pos += 1;
            skip_ws(pos);
            crate::generation::read_occurrence(chars, pos)?;
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
            eat_marker(pos, crate::generation::PRESERVE_MARKER);
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
fn parse_key_flavor(token: &str, file: &str) -> Result<DemandSet, String> {
    let mut demand = DemandSet::default();
    for word in token.split_whitespace() {
        match word {
            "bare" => demand.bare = true,
            "hash" => demand.hash = true,
            "ord" => demand.ord = true,
            _ => {
                return Err(KEY_TYPES_TABLE.refusal(
                    file,
                    "unknown key-demand flavor in BORROWED_KEY_TYPES row",
                    token,
                ));
            }
        }
    }
    if demand == DemandSet::default() {
        return Err(KEY_TYPES_TABLE.refusal(
            file,
            "empty key-demand flavor in BORROWED_KEY_TYPES row",
            token,
        ));
    }
    Ok(demand)
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
pub fn parse_key_types_sidecar(contents: &str, file: &str) -> Result<Vec<KeyTypeEntry>, String> {
    if contents.contains("compile_error!") {
        return Err(format!(
            "--key-requests {file}: the sidecar contains a `compile_error!` — it is a trapped or \
             drifted generated file, which must never be silently consumed. Regenerate the consumer \
             crate to clear it."
        ));
    }
    let logical = flatten_overlay_blocks(contents, file, "--key-requests")?;
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
                return Err(KEY_TYPES_TABLE.refusal(
                    file,
                    "unexpected comment inside `BORROWED_KEY_TYPES`",
                    trimmed,
                ));
            }
            const_item.push_str(line);
            const_item.push('\n');
            if trimmed.ends_with("];") {
                entries.extend(key_entries(
                    KEY_TYPES_TABLE.parse_item(&const_item, file)?,
                    file,
                )?);
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
            return Err(KEY_TYPES_TABLE.refusal(file, "unexpected comment", trimmed));
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
                entries.extend(key_entries(
                    KEY_TYPES_TABLE.parse_item(&const_item, file)?,
                    file,
                )?);
                const_item.clear();
            } else {
                in_const = true;
            }
            continue;
        }
        return Err(KEY_TYPES_TABLE.refusal(file, "unexpected item", trimmed));
    }
    if in_const {
        return Err(KEY_TYPES_TABLE.refusal(
            file,
            "unterminated `BORROWED_KEY_TYPES` table (missing `];`)",
            "",
        ));
    }
    Ok(entries)
}

/// Map `KEY_TYPES_TABLE` rows to entries. The optional 3rd column is the comparison/hash flavor; a
/// two-column (old) row is `bare`.
fn key_entries(rows: Vec<Vec<String>>, file: &str) -> Result<Vec<KeyTypeEntry>, String> {
    rows.into_iter()
        .map(|fields| {
            let demand = if fields.len() == 3 {
                parse_key_flavor(&fields[2], file)?
            } else {
                DemandSet::BARE
            };
            Ok(KeyTypeEntry {
                dep: fields[0].clone(),
                ident: fields[1].clone(),
                demand,
            })
        })
        .collect()
}

/// Read one `--wrapper-requests` / `--key-requests` sidecar, or `None` when the consumer has not
/// generated one yet.
///
/// A sidecar records what a consumer BORROWS from this crate, so a consumer that has never generated
/// borrows nothing — and an absent file is the faithful spelling of that, not an error. Treating it
/// as one makes a cold workspace unbootstrappable in both directions at once: the dependency cannot
/// generate until its consumer has written a sidecar, and the consumer cannot generate until the
/// dependency has written the export it imports. The absence is announced on stderr because the OTHER
/// way to reach it is a wrong path, which would otherwise silently disable the whole channel.
///
/// Every other read failure — a permission error, a non-UTF-8 file — stays a hard error (the `Err`
/// of the outer `Result`): those are not "no sidecar", they are a sidecar this run cannot honour.
///
/// Determinism is unaffected: the file's absence is an input state exactly as its content is, so the
/// same inputs still produce the same bytes. This is not a prior-OUTPUT read either — the sidecar is
/// another crate's committed input, whether it exists or not.
pub fn read_request_sidecar(
    flag: &str,
    consumer: &str,
    path: &str,
) -> Result<Option<String>, String> {
    match std::fs::read_to_string(path) {
        Ok(contents) => Ok(Some(contents)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            crate::warn!(
                "warning: {flag} {consumer}={path}: no sidecar there yet, so `{consumer}` is treated \
                 as borrowing nothing from this crate. That is what a consumer which has never been \
                 generated records. If `{consumer}` HAS been generated, the path is wrong — check it, \
                 or regenerate `{consumer}` and re-run this crate to converge."
            );
            Ok(None)
        }
        Err(e) => Err(format!(
            "{flag} {consumer}={path}: cannot read the sidecar: {e}"
        )),
    }
}

/// Seed `used_as_key` from every `--key-requests` sidecar's rows addressed to THIS dep, resolving
/// each CDDL ident to a `RustIdent` and marking it (finalize then expands transitively). No-op (and
/// byte-identical to today) when there are no `--key-requests` flags. STRICT: a sidecar that exists
/// but cannot be read is an `Err`, and so is a row naming a type this dep does not define (a
/// consumer keying on a type the dep deleted must be loud, mirroring the W1 compiled-`use`). A path
/// with no file at all is the cold-workspace case — see [`read_request_sidecar`].
pub fn seed_used_as_key_from_key_requests(
    types: &mut IntermediateTypes,
    cli: &Cli,
) -> Result<(), String> {
    let request_files = cli.key_requests();
    if request_files.is_empty() {
        return Ok(());
    }
    let my_lib = cli.lib_name_code();
    let mut to_mark: std::collections::BTreeMap<RustIdent, DemandSet> =
        std::collections::BTreeMap::new();
    for (consumer, path) in &request_files {
        let Some(contents) = read_request_sidecar("--key-requests", consumer, path)? else {
            continue;
        };
        for entry in parse_key_types_sidecar(&contents, path)? {
            if entry.dep.replace('-', "_") != my_lib {
                continue;
            }
            // `int` is the ONE reserved ident that names a real dep-provided type: the built-in `Int`
            // extern, which `IntermediateTypes::new` always registers in `rust_structs()`. A consumer
            // keying a map on `int` under `--common-import-override` records the row `(<override>,
            // "int")` in its borrowed_key_types.rs (see `generation/export.rs`); the common/dep crate
            // reading it must key-flavor its `Int`. Resolve it explicitly here — `RustIdent::new` maps
            // `int` -> `Int` without panicking (it is the deliberate exception in `reserved_reason`),
            // and the emission gate in `generation` honors key demand — so `generate_int` runs
            // key-flavored even when this dep's own spec never references `int`. Every OTHER reserved
            // ident stays an unknown-type hard error below.
            let known = entry.ident == "int"
                || (crate::intermediate::reserved_ident_rejection(&entry.ident).is_none() && {
                    let ident = RustIdent::new(CDDLIdent::new(entry.ident.clone()));
                    types.rust_struct(&ident).is_some()
                        || types
                            .type_aliases()
                            .contains_key(&AliasIdent::Rust(ident.clone()))
                });
            if !known {
                // The one refusal in this file reachable from COMMITTED STATE alone — a dep whose
                // spec dropped a type its consumer's committed sidecar still borrows — so it is the
                // one that most needs the error channel rather than an abort: it can strike a
                // `--config` run mid-way through a workspace, and the statement of what was already
                // rewritten is the answer to the only question that leaves.
                //
                // It stays a mid-generation `Err` rather than a row of `--config`'s committed-state
                // verdict, for two reasons that both come from what the fact IS. The verdict is a
                // post-pass comparison of two committed FILES (a consumer's `borrowed_collections.rs`
                // against a dep's `collections.rs` wrapper index), and "is this ident a type the dep
                // defines" is not readable from any file — it is a question about the dep's finalized
                // IR, which only this seam has. And the verdict runs in `--config` mode only, while
                // `--key-requests` is a plain flag: routing the refusal there would leave the
                // single-crate CLI path silently seeding nothing.
                return Err(format!(
                    "--key-requests {consumer} ({path}): the borrowed key type {:?} (row \
                     ({:?}, {:?})) is not a type this dep defines — a consumer is keying a map on a \
                     type the dep no longer provides. Remedy: restore the type in the dep spec, or \
                     regenerate the consumer so it stops borrowing this key type.",
                    entry.ident, entry.dep, entry.ident
                ));
            }
            let ident = RustIdent::new(CDDLIdent::new(entry.ident));
            let e = to_mark.entry(ident).or_default();
            *e = e.union(entry.demand);
        }
    }
    for (ident, demand) in to_mark {
        types.mark_key_demand(ident, demand);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intermediate::IntWindow;

    /// The refusal assertion every grammar-rejection test below shares, asserting BOTH halves of
    /// what a refusal owes at once. That it is an `Err` and not an abort: these readers run inside
    /// `generate_to_disk`, and only a value travelling the error channel reaches `--config`'s
    /// mid-run wrapper (pinned end-to-end by
    /// `config_tests::a_mid_run_sidecar_refusal_names_the_crates_already_regenerated`). And that the
    /// diagnostic still SAYS what it said — these texts are what a user reads to find the file and
    /// fix it, so each call site pins its own.
    #[track_caller]
    fn assert_refusal<T: std::fmt::Debug>(got: Result<T, String>, expected: &str) {
        match got {
            Ok(v) => panic!("expected a refusal containing {expected:?}, got Ok({v:?})"),
            Err(e) => assert!(
                e.contains(expected),
                "the refusal must still name {expected:?}, got: {e}"
            ),
        }
    }

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
        let entries = parse_sidecar(CANONICAL, "borrowed_collections.rs").unwrap();
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
        let entries = parse_sidecar(EMPTY, "borrowed_collections.rs").unwrap();
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
        let entries = parse_sidecar(wrapped, "borrowed_collections.rs").unwrap();
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
        let entries = parse_sidecar(collapsed, "borrowed_collections.rs").unwrap();
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
        let entries = parse_sidecar(with_insert, "borrowed_collections.rs").unwrap();
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
        let entries = parse_sidecar(with_replace, "borrowed_collections.rs").unwrap();
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
        assert_refusal(
            parse_sidecar(trapped, "borrowed_collections.rs"),
            "unpreserved-comment",
        );
    }

    /// A `// cddl-codegen:keep` marker is a REGISTERED tag (it must not hit the unknown-tag panic),
    /// but this sidecar's frozen grammar admits no comment outside `KNOWN_COMMENTS` — so a `keep`
    /// comment gets exactly the treatment the same comment gets unmarked: the "unexpected comment"
    /// hard error naming the line. Both `keep` forms land there, so user prose can never silently
    /// vanish from a machine-read sidecar. Pinned in both forms below.
    #[test]
    fn rejects_inline_keep_comment() {
        let with_keep = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:keep this row is load-bearing
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(with_keep, "borrowed_collections.rs"),
            "unexpected comment",
        );
    }

    #[test]
    fn rejects_bare_keep_comment_run() {
        let with_keep = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:keep
// this row is load-bearing
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(with_keep, "borrowed_collections.rs"),
            "unexpected comment",
        );
    }

    /// `keep-<anything>` is NOT `keep`: it stays an unknown reserved tag, so the sidecar scanner
    /// rejects it with the unexpected-reserved-comment panic rather than the comment path.
    #[test]
    fn rejects_keep_suffixed_unknown_tag() {
        let bad = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// cddl-codegen:keep-this
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(bad, "borrowed_collections.rs"),
            "unexpected reserved comment",
        );
    }

    #[test]
    fn rejects_compile_error() {
        let trapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

compile_error!("this file drifted");
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(trapped, "borrowed_collections.rs"),
            "compile_error",
        );
    }

    #[test]
    fn rejects_stray_line() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

fn sneaky() {}
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(stray, "borrowed_collections.rs"),
            "unexpected item",
        );
    }

    #[test]
    fn rejects_unknown_comment() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// a hand-written note that is not part of the frozen banner
#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
];
"#;
        assert_refusal(
            parse_sidecar(stray, "borrowed_collections.rs"),
            "unexpected comment",
        );
    }

    #[test]
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
        assert_refusal(
            parse_sidecar(old_format, "borrowed_collections.rs"),
            "unexpected comment inside `BORROWED_SHAPES`",
        );
    }

    #[test]
    fn rejects_mangled_tuple() {
        // A two-element tuple (dep + name, no shape) is a mangled row.
        let mangled = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("index_dep_crate", "IdxFooList"),
];
"#;
        assert_refusal(
            parse_sidecar(mangled, "borrowed_collections.rs"),
            "malformed BORROWED_SHAPES row",
        );
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

    /// The lenient key scan reads every shape `render_wrapper_shape` can write: each occurrence
    /// form (`*N`, `?`, `N*M`, `N*`, `+`) and the duplicate-policy marker on a nested collection.
    /// Before it shared `read_occurrence`, a bounded map (`{*5 idx_foo => uint}`) seeded no key and
    /// the dep's requested wrapper failed `E0277 IdxFoo: Ord`.
    #[test]
    fn map_key_idents_read_every_rendered_shape() {
        use crate::comment_ast::DuplicatesPolicy;
        use crate::intermediate::{ConceptualRustType, Primitive, RustType};
        let named =
            |n: &str| RustType::new(ConceptualRustType::Rust(RustIdent::new(CDDLIdent::new(n))));
        let uint = || RustType::new(ConceptualRustType::Primitive(Primitive::U64));
        let map = |key: RustType, value: RustType, bounds: Option<IntWindow>, preserve: bool| {
            let mut rt = RustType::new(ConceptualRustType::Map(Box::new(key), Box::new(value)));
            if let Some(bounds) = bounds {
                rt = rt.with_occurrence_bounds(bounds);
            }
            if preserve {
                rt.config.duplicates = Some(DuplicatesPolicy::Preserve);
            }
            rt
        };
        let list = |inner: RustType, bounds: Option<IntWindow>, reject: bool| {
            let mut rt = RustType::new(ConceptualRustType::Array(Box::new(inner)));
            if let Some(bounds) = bounds {
                rt = rt.with_occurrence_bounds(bounds);
            }
            if reject {
                rt.config.duplicates = Some(DuplicatesPolicy::Reject);
            }
            rt
        };
        let windows = [
            None,
            Some((Some(1), None)),
            Some((None, Some(1))),
            Some((None, Some(5))),
            Some((Some(2), None)),
            Some((Some(1), Some(5))),
        ];
        for bounds in windows {
            for preserve in [false, true] {
                let keyed = map(named("idx_foo"), uint(), bounds, preserve);
                let shape = crate::generation::render_wrapper_shape(&keyed);
                assert_eq!(map_key_cddl_idents(&shape), vec!["idx_foo"], "{shape}");
                // Nested inside a bounded reject list, and holding a marked collection as its value.
                let outer = list(keyed.clone(), Some((Some(1), Some(3))), true);
                let shape = crate::generation::render_wrapper_shape(&outer);
                assert_eq!(map_key_cddl_idents(&shape), vec!["idx_foo"], "{shape}");
                let holding = map(
                    named("idx_foo"),
                    list(named("idx_bar"), bounds, true),
                    bounds,
                    preserve,
                );
                let shape = crate::generation::render_wrapper_shape(&holding);
                assert_eq!(map_key_cddl_idents(&shape), vec!["idx_foo"], "{shape}");
                let holding = map(named("idx_foo"), keyed.clone(), None, false);
                let shape = crate::generation::render_wrapper_shape(&holding);
                assert_eq!(
                    map_key_cddl_idents(&shape),
                    vec!["idx_foo", "idx_foo"],
                    "{shape}"
                );
            }
        }
    }

    #[test]
    fn scan_shape_rows_skips_commented_originals() {
        // The lenient scan drops `//`-commented lines (an overlay `:replaces` recorded original must
        // not seed) and returns all three columns of each real 3-literal row.
        let sidecar = r#"// header
mod borrowed {}
pub(crate) const BORROWED_SHAPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "MapIdxFooToU64", "{* idx_foo => uint}"),
    // ("wr_dep", "GhostList", "[* ghost]"),
];
"#;
        let rows: Vec<(String, String, String)> = scan_borrowed_rows_lenient(sidecar)
            .into_iter()
            .map(|row| (row.dep, row.name, row.shape))
            .collect();
        assert_eq!(
            rows,
            vec![(
                "wr_dep".to_owned(),
                "MapIdxFooToU64".to_owned(),
                "{* idx_foo => uint}".to_owned()
            )]
        );
    }

    /// The collection-index grammar, in the three answers it gives. Its two readers apply opposite
    /// policies to `Unknown` — the consumer's own run hard-errors, the config's post-run verdict
    /// skips — so what the grammar itself decides has to be pinned in one place rather than inferred
    /// from either policy.
    #[test]
    fn collection_index_lines_classify_into_ignored_export_and_unknown() {
        let exported = |line: &str| match classify_collection_index_line(line) {
            CollectionIndexLine::Export(name) => Some(name),
            _ => None,
        };
        assert_eq!(
            exported("pub use crate::generated::requested_collections::ThingList;"),
            Some("ThingList".to_owned())
        );
        assert_eq!(
            exported("  pub use collections::MapU64ToThing;  "),
            Some("MapU64ToThing".to_owned())
        );
        for ignored in ["", "   ", "// a banner line"] {
            assert!(matches!(
                classify_collection_index_line(ignored),
                CollectionIndexLine::Ignored
            ));
        }
        for unknown in [
            "pub use collections::Thing List;",
            "fn stray() {}",
            "pub use x::;",
        ] {
            assert!(
                matches!(
                    classify_collection_index_line(unknown),
                    CollectionIndexLine::Unknown
                ),
                "{unknown:?} is not the index's grammar"
            );
        }
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
        let entries = parse_key_types_sidecar(CANONICAL_KEYS, "borrowed_key_types.rs").unwrap();
        assert_eq!(
            entries,
            vec![
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_bar".into(),
                    demand: DemandSet::BARE,
                },
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "idx_foo".into(),
                    demand: DemandSet::BARE,
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
            parse_key_types_sidecar(src, "borrowed_key_types.rs").unwrap(),
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

    // The self-check fn body is skipped wholesale by brace depth, so a scoped-path self-check
    // (`dep::sub::module::Ident`, emitted when a borrowed key lives in a non-root scope) parses to
    // exactly the same bare-ident rows as the root-path form — an OLD parser reading a NEW sidecar
    // stays compatible. The ROWS are unchanged; only the self-check body carries the module path.
    #[test]
    fn key_types_skips_scoped_self_check_body() {
        let src = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// This file records every map-key type this crate borrows from workspace deps.
// It is machine-read by those deps' generation runs (--key-requests) so they derive the key
// traits (Eq/Ord/PartialOrd, plus Hash under --preserve-encodings) on the borrowed type; the
// compiled self-check below fails THIS crate's build if a dep drops such a derive.
// Rows are (dep rust-crate name, cddl ident) of each borrowed map-key type.
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd>() {}
#[allow(dead_code)]
fn _borrowed_key_types_self_check() {
    _assert_key_traits::<wr_dep::RootKey>();
    _assert_key_traits::<wr_dep::sub::module::ScopedKey>();
}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] =
    &[("wr_dep", "root_key"), ("wr_dep", "scoped_key")];
"#;
        assert_eq!(
            parse_key_types_sidecar(src, "borrowed_key_types.rs").unwrap(),
            vec![
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "root_key".into(),
                    demand: DemandSet::BARE,
                },
                KeyTypeEntry {
                    dep: "wr_dep".into(),
                    ident: "scoped_key".into(),
                    demand: DemandSet::BARE,
                },
            ]
        );
    }

    #[test]
    fn key_types_accepts_empty_file() {
        assert!(
            parse_key_types_sidecar(EMPTY_KEYS, "borrowed_key_types.rs")
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn key_types_rejects_unknown_comment() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

// a hand-written note that is not part of the frozen banner
#[allow(dead_code)]
fn _assert_key_traits<K: Eq + Ord + PartialOrd + core::hash::Hash>() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        assert_refusal(
            parse_key_types_sidecar(stray, "borrowed_key_types.rs"),
            "unexpected comment",
        );
    }

    #[test]
    fn key_types_rejects_compile_error() {
        let trapped = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

compile_error!("this file drifted");
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        assert_refusal(
            parse_key_types_sidecar(trapped, "borrowed_key_types.rs"),
            "compile_error",
        );
    }

    #[test]
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
        assert_refusal(
            parse_key_types_sidecar(mangled, "borrowed_key_types.rs"),
            "malformed BORROWED_KEY_TYPES row",
        );
    }

    // A three-column row whose flavor token is not a known word is a hard error (mangled sidecar).
    #[test]
    fn key_types_rejects_unknown_flavor() {
        let bad = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str, &str)] = &[
    ("wr_dep", "idx_foo", "nonsense"),
];
"#;
        assert_refusal(
            parse_key_types_sidecar(bad, "borrowed_key_types.rs"),
            "unknown key-demand flavor",
        );
    }

    /// An unterminated literal in the KEY table is refused in the key channel's own words: flag,
    /// table and file kind. (The two tables share one reader, so a refusal cannot borrow the other
    /// sidecar's wording.)
    #[test]
    fn key_types_rejects_unterminated_literal() {
        let bad = r#"#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[
    ("wr_dep", "idx_foo),
];
"#;
        let err = parse_key_types_sidecar(bad, "borrowed_key_types.rs").unwrap_err();
        assert_eq!(
            err,
            "--key-requests borrowed_key_types.rs: unterminated string literal in \
             BORROWED_KEY_TYPES. The sidecar must be an unmodified, tool-generated \
             `borrowed_key_types.rs`."
        );
    }

    #[test]
    fn key_types_rejects_stray_item() {
        let stray = r#"// This file was code-generated using an experimental CDDL to rust tool:
// https://github.com/dcSpark/cddl-codegen

fn sneaky() {}
#[allow(dead_code)]
pub(crate) const BORROWED_KEY_TYPES: &[(&str, &str)] = &[];
"#;
        assert_refusal(
            parse_key_types_sidecar(stray, "borrowed_key_types.rs"),
            "unexpected item",
        );
    }
}
