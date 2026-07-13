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

    let logical = flatten_overlay_blocks(contents, file);

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
fn flatten_overlay_blocks(contents: &str, file: &str) -> Vec<String> {
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
                        "--wrapper-requests {file}: the sidecar contains a \
                         `// cddl-codegen:unpreserved-comment` sentinel — it is a trapped or drifted \
                         generated file, which must never be silently consumed. Regenerate the \
                         consumer crate to clear it."
                    );
                }
                other => {
                    panic!(
                        "--wrapper-requests {file}: unexpected reserved comment \
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
}
