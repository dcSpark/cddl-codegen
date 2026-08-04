//! Corpus-wide **regen over prior output**: the two variant families whose trap classes only exist
//! when `export()` runs over a tree it wrote before.
//!
//! The comment-preservation overlay (`crate::comment_preserve`) participates ONLY on that path — it
//! is applied to the in-memory file map ahead of the write loop, so a run into an empty directory
//! never exercises it. Two failure classes live there, and both shipped once:
//!
//! * **the stranded-comment trap** — a generated file carrying a comment on a row a spec change can
//!   DELETE strands that comment on the next in-place regen, and the overlay re-injects it as a
//!   `cddl-codegen:unpreserved-comment` + `compile_error!` sentinel that every further regen carries
//!   forward (self-perpetuating). Fixed in `extern_interface_check.rs` / `key_demand_assertions.rs`
//!   by making both banner-only;
//! * **the orphaned-import class** — a `cddl-codegen:replace` block that removes an import's last
//!   user must take the import with it. Fixed by ordering (overlay first, then a family-wide
//!   re-prune over the post-overlay map, then the alloc-import recompute).
//!
//! Both were pinned BY NAME afterwards (`extern_interface_check_regen_over_deletion_no_trap`,
//! `extern_interface_check_has_no_trailing_row_comments`,
//! `comment_preservation_replace_orphans_import_same_file`,
//! `comment_preservation_replace_in_descendant_orphans_parent_import`) — which leaves a third file
//! or a third shape invisible. The corpus-scale overlay gate that exists otherwise
//! (`comment_preserve_lexer_round_trip_over_corpus`) does SELF-preserve — `preserve(content,
//! content)` — which is a no-op for a trailing comment whether or not a real regen would strand it,
//! so it cannot see either class at all.
//!
//! This module is the by-shape replacement for those by-name pins: every `tests/corpus/*.cddl`
//! fixture, three legs, no file named in advance.
//!
//! * **Leg 1 — the static floor.** Scan every `.rs` under the tool-owned trees of a FRESH generation
//!   for a comment that shares its row with code. That is the trap SOURCE, catchable before any
//!   deletion exists. Lexer-grade via [`crate::comment_preserve::comments_sharing_a_code_row`],
//!   never a `line.find("//")` scan: a URL in a string literal and a `/*` inside an own-line banner
//!   are both things this corpus actually emits.
//! * **Leg 2 — the rule-DELETION variant.** Derive a variant spec with one rule (and its referrers)
//!   removed, regenerate IN PLACE over the fresh output, and fail on any
//!   `cddl-codegen:unpreserved-comment` anywhere in the tool-owned trees.
//!
//! Legs 1 and 2 sweep both [`FLOOR_AND_DELETION_PROFILES`] rows; leg 3 stays on the default one
//! (see that constant for the cost asymmetry).
//! * **Leg 3 — the user-EDIT variant.** Inject one canonical `cddl-codegen:replace` block that
//!   REMOVES a function body, regenerate in place (the block must apply, not strand), and regenerate
//!   once more (the trees must reach a byte-identical fixed point). The compile half — does the
//!   regenerated crate still build, warning-clean, after the prune re-derived its imports — is the
//!   separate `full`-tier gate at the bottom of this file, because it is the only leg that pays
//!   cargo.
//!
//! **Vacuity is the standing risk**, so every leg counts what it exercised and asserts a floor:
//! a fixture whose deletion candidate never generates, or whose output has no injection target, is
//! recorded as SKIPPED and does not count towards its leg's floor. The floors are set from a
//! measured run and are deliberately below it — they catch a leg going structurally silent, not
//! corpus churn.
//!
//! **Residual, measured against this corpus (not a design choice):** no corpus fixture carries TWO
//! `@used_as_key` tags, so the shape "a `key_demand_assertions.rs` ROW disappears while the file
//! itself survives" is not expressible here — deleting the sole tagged rule stops the file being
//! generated at all, and `export()` then never runs the overlay over it. The deletion chooser still
//! PREFERS a tagged rule in a tagged fixture (so the tagged path is exercised corpus-wide and the
//! leg is not silently green for the wrong reason), and the row-survives-file shape stays covered by
//! the hand fixture in `extern_interface_check_regen_over_deletion_no_trap`, whose v1 spec carries
//! two tags. A corpus fixture with two tags would close it here.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::integration_tests::{
    COMPILE_SKIP, acquire_scratch_lock, checkout_hash, codegen_cmd, feature_corpus_entries,
    tool_cmd, unused_generated_import_lines, unused_generated_variable_lines,
};

/// The generated trees `export()` owns outright — delete-and-rewrite every run, and the ONLY trees
/// the comment-preservation overlay is applied to. `wasm/json-gen/src/generated` exists only under
/// the json profile; a missing tree contributes no files rather than failing, so this list is the
/// union over profiles rather than a per-profile one.
const TOOL_OWNED_TREES: &[&str] = &[
    "rust/src/generated",
    "wasm/src/generated",
    "wasm/json-gen/src/generated",
];

/// The emission profiles legs 1 and 2 sweep. Leg 3 (and its compile gate) stay on the first row
/// alone: an injected replace block's cost is three generations plus, for the compile gate, nested
/// cargo, while the two legs here cost one generation each per added profile.
///
/// `--preserve-encodings` is the second row because it emits a FILE the default profile does not
/// (`cbor_encodings.rs`, one struct per rule that carries encodings) — a per-rule surface is exactly
/// where a deletable-row comment would live, and no other gate looks for one there. The `json` and
/// `component` profiles are not swept; see `tests/TESTING_ROADMAP.md` for the residual and the
/// observable that would add one.
const FLOOR_AND_DELETION_PROFILES: &[(&str, &[&str])] = &[
    ("default", &[]),
    ("preserve", &["--preserve-encodings=true"]),
];

/// The sentinel the overlay emits for a payload it could not re-place. Its presence anywhere in a
/// regenerated tree IS the trap class.
const TRAP_MARKER: &str = "cddl-codegen:unpreserved-comment";

/// Every `.rs` file under [`TOOL_OWNED_TREES`], as paths RELATIVE to `out`, sorted — so a failure
/// names a stable path and the two-regen fixed-point comparison compares like with like.
fn tool_owned_rs_files(out: &Path) -> Vec<PathBuf> {
    fn walk(dir: &Path, root: &Path, acc: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, root, acc);
            } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                acc.push(path.strip_prefix(root).unwrap().to_path_buf());
            }
        }
    }
    let mut acc = Vec::new();
    for tree in TOOL_OWNED_TREES {
        walk(&out.join(tree), out, &mut acc);
    }
    acc.sort();
    acc
}

/// The tool-owned trees as a path→content map, for whole-tree equality/difference verdicts.
fn read_tool_owned(out: &Path) -> BTreeMap<PathBuf, String> {
    tool_owned_rs_files(out)
        .into_iter()
        .map(|rel| {
            let content = std::fs::read_to_string(out.join(&rel))
                .unwrap_or_else(|e| panic!("cannot read {rel:?} under {out:?}: {e}"));
            (rel, content)
        })
        .collect()
}

/// Run the generator over `spec` into `out` with the sweep's profile flags. Returns the captured
/// stderr on failure (never panics on a non-zero exit: the deletion chooser uses a failed generation
/// as a "try the next candidate" signal, and only an EXHAUSTED chooser is reportable).
fn generate(spec: &Path, out: &Path, extra: &[&str]) -> Result<(), String> {
    let output = codegen_cmd()
        .arg(format!("--input={}", spec.to_str().unwrap()))
        .arg(format!("--output={}", out.to_str().unwrap()))
        .arg("--wasm=true")
        .args(extra)
        .output()
        .unwrap();
    if output.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).into_owned())
    }
}

// ==================================================================================================
// Rule spans and the DELETION variant
// ==================================================================================================

/// One rule's declaration, as a LINE range into the fixture text.
///
/// Line-based rather than the `cddl` crate's AST spans on purpose: a rule's comment-DSL directives
/// live in the `; …` comment TRAILING its last line (`; @used_as_key`, `; @name Foo`), which the AST
/// span does not cover. Deleting by AST span would leave that directive behind to re-attach to a
/// DIFFERENT rule — silently changing what the variant means, in a way an exit-0 generation would
/// not reveal. Whole lines take the directive with the rule.
///
/// The names are authoritative — they come from the same `cddl` parse the generator itself runs (see
/// [`rule_spans`]) — so only the SPAN derivation is textual, and a mis-derived span is caught by the
/// variant failing to generate.
#[derive(Debug, Clone)]
struct RuleSpan {
    name: String,
    /// 0-based, inclusive.
    first_line: usize,
    /// 0-based, inclusive.
    last_line: usize,
}

/// True if `line` opens a rule declaration for `name`: the ident at column 0, then optional generic
/// parameters, then one of CDDL's three assignment operators.
fn line_declares(line: &str, name: &str) -> bool {
    let Some(rest) = line.strip_prefix(name) else {
        return false;
    };
    // The ident must END here — `foo` must not match the `foobar = …` line above it.
    if rest
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphanumeric() || "-.@_$".contains(c))
    {
        return false;
    }
    let rest = rest.trim_start();
    let rest = match rest.find('>') {
        Some(close) if rest.starts_with('<') => rest[close + 1..].trim_start(),
        _ => rest,
    };
    rest.starts_with('=') || rest.starts_with("/=") || rest.starts_with("//=")
}

/// The rules of `text`, in declaration order, each with the LINE range that carries it.
///
/// Names come from the `cddl` crate (the generator's own parser); the span of rule *i* runs from its
/// declaring line to the line before rule *i+1*'s, minus any trailing run of blank / `;`-comment
/// lines, which belong to the NEXT rule (a `; @…` directive block written above a rule, and the
/// header comments several corpus fixtures open with). Returns `None` if the fixture does not parse
/// standalone — the caller records the fixture as skipped rather than guessing.
fn rule_spans(text: &str) -> Option<Vec<RuleSpan>> {
    let cddl = cddl::ast::CDDL::from_slice(text.as_bytes()).ok()?;
    let names: Vec<String> = cddl
        .rules
        .iter()
        .map(|rule| match rule {
            cddl::ast::Rule::Type { rule, .. } => rule.name.to_string(),
            cddl::ast::Rule::Group { rule, .. } => rule.name.to_string(),
        })
        .collect();
    let lines: Vec<&str> = text.lines().collect();

    let mut declaring_lines = Vec::new();
    let mut search_from = 0usize;
    for name in &names {
        let found = (search_from..lines.len()).find(|&i| line_declares(lines[i], name))?;
        declaring_lines.push(found);
        search_from = found + 1;
    }

    let mut spans = Vec::new();
    for (i, name) in names.iter().enumerate() {
        let first_line = declaring_lines[i];
        let mut last_line = declaring_lines
            .get(i + 1)
            .map(|next| next - 1)
            .unwrap_or(lines.len().saturating_sub(1));
        while last_line > first_line {
            let candidate = lines[last_line].trim();
            if candidate.is_empty() || candidate.starts_with(';') {
                last_line -= 1;
            } else {
                break;
            }
        }
        spans.push(RuleSpan {
            name: name.clone(),
            first_line,
            last_line,
        });
    }
    Some(spans)
}

/// Every CDDL ident token in `text`. Comments are NOT skipped, deliberately: a rule name mentioned
/// in a neighbour's `; …` comment counts as a reference here, which can only make a deletion closure
/// LARGER (more conservative), never leave a dangling reference behind.
fn idents_in(text: &str) -> BTreeSet<String> {
    let bytes = text.as_bytes();
    let mut out = BTreeSet::new();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c.is_ascii_alphabetic() || c == b'@' || c == b'_' || c == b'$' {
            let start = i;
            while i < bytes.len() {
                let c = bytes[i];
                if c.is_ascii_alphanumeric() || b"-.@_$".contains(&c) {
                    i += 1;
                } else {
                    break;
                }
            }
            out.insert(text[start..i].to_owned());
        } else {
            i += 1;
        }
    }
    out
}

/// The variant text with `target` and every rule that transitively references it removed, or `None`
/// if that closure would consume every rule but one candidate ordering could still do better.
///
/// The closure is what keeps the variant a VALID spec: deleting a referenced rule alone leaves a
/// dangling reference, which the generator's checked parse rejects — so the deletion would never
/// reach the write path the leg exists to exercise. Deleting the closure of the LAST rule of a
/// single-rule fixture yields an empty spec, which generates fine (an empty crate) and is the
/// strongest deletion available: every emitted row disappears at once.
fn deletion_variant(text: &str, spans: &[RuleSpan], target: &str) -> String {
    let by_name: BTreeMap<&str, &RuleSpan> = spans.iter().map(|s| (s.name.as_str(), s)).collect();
    let lines: Vec<&str> = text.lines().collect();
    let body_of = |span: &RuleSpan| lines[span.first_line..=span.last_line].join("\n");

    let mut doomed: BTreeSet<String> = [target.to_owned()].into_iter().collect();
    loop {
        let mut grew = false;
        for span in spans {
            if doomed.contains(&span.name) {
                continue;
            }
            let referenced = idents_in(&body_of(span));
            // A rule always names itself in its own declaration; that is not a reference.
            if doomed
                .iter()
                .any(|d| d != &span.name && referenced.contains(d))
            {
                doomed.insert(span.name.clone());
                grew = true;
            }
        }
        if !grew {
            break;
        }
    }

    let mut deleted_lines = BTreeSet::new();
    for name in &doomed {
        if let Some(span) = by_name.get(name.as_str()) {
            for line in span.first_line..=span.last_line {
                deleted_lines.insert(line);
            }
        }
    }
    let kept: Vec<&str> = lines
        .iter()
        .enumerate()
        .filter(|(i, _)| !deleted_lines.contains(i))
        .map(|(_, l)| *l)
        .collect();
    let mut out = kept.join("\n");
    out.push('\n');
    out
}

/// The order the deletion chooser tries rules in: `@used_as_key`-TAGGED rules first (so a tagged
/// fixture exercises `key_demand_assertions.rs`'s rows rather than leaving them vacuously green —
/// the roadmap entry's second shaping note), then the rest; each group LAST-DECLARED first, because
/// a later rule is likelier to be a leaf and so to yield the smallest closure. Deterministic from
/// the fixture text alone: rerunning a failure reproduces the same variant.
fn deletion_candidates(text: &str, spans: &[RuleSpan]) -> Vec<String> {
    let lines: Vec<&str> = text.lines().collect();
    let tagged = |span: &RuleSpan| {
        lines[span.first_line..=span.last_line]
            .iter()
            .any(|line| line.contains("@used_as_key"))
    };
    let mut ordered: Vec<&RuleSpan> = spans.iter().rev().filter(|s| tagged(s)).collect();
    ordered.extend(spans.iter().rev().filter(|s| !tagged(s)));
    ordered.into_iter().map(|s| s.name.clone()).collect()
}

// ==================================================================================================
// The user-EDIT (replace-block) injection
// ==================================================================================================

/// A chosen injection site: which file, which function, and the body lines the block will remove.
#[derive(Debug)]
struct Injection {
    file: PathBuf,
    function: String,
    /// Whether removing this body orphans an import in the same file — the class the compile leg
    /// exists to catch. Counted so the leg can assert it is not all trivial sites.
    orphans_import: bool,
    /// Whether the site is in a PER-TYPE surface ([`PER_TYPE_FILES`]) rather than in a composed
    /// static runtime file, whose bytes are the same for every fixture. Counted because a sweep that
    /// injects into the same static file 91 times is one cell wearing a corpus's clothes.
    per_type: bool,
}

/// The generated files whose bytes are a function of the FIXTURE (the per-type wasm and rust
/// surfaces), tried before anything else so the edit leg's subject differs per cell. Everything else
/// under the tool-owned trees follows in sorted order.
const PER_TYPE_FILES: &[&str] = &["wasm/src/generated/mod.rs", "rust/src/generated/mod.rs"];

/// Byte offsets of the `{`…`}` body of every function in `src` whose parameter list is empty or
/// exactly `self`, with the function's name.
///
/// The parameter restriction is not fastidiousness: the replacement stubs the body, so a function
/// WITH parameters would leave them unused and rustc would warn `unused variable` — a warning the
/// compile leg's scan reads as generator imprecision. Restricting the site keeps that scan's verdict
/// about the GENERATOR, which is the only thing it can soundly be about.
fn self_only_fn_bodies(src: &str) -> Vec<(String, usize, usize)> {
    let bytes = src.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;
    // Skip string/char/raw literals and comments so a `{` inside one never opens a body.
    let skip_literal_or_comment = |i: &mut usize| -> bool {
        let b = bytes;
        let n = b.len();
        if b[*i] == b'/' && *i + 1 < n && b[*i + 1] == b'/' {
            while *i < n && b[*i] != b'\n' {
                *i += 1;
            }
            return true;
        }
        if b[*i] == b'/' && *i + 1 < n && b[*i + 1] == b'*' {
            *i += 2;
            while *i + 1 < n && !(b[*i] == b'*' && b[*i + 1] == b'/') {
                *i += 1;
            }
            *i = (*i + 2).min(n);
            return true;
        }
        if b[*i] == b'r' && *i + 1 < n && (b[*i + 1] == b'"' || b[*i + 1] == b'#') {
            let hashes = b[*i + 1..].iter().take_while(|c| **c == b'#').count();
            if *i + 1 + hashes < n && b[*i + 1 + hashes] == b'"' {
                let close = format!("\"{}", "#".repeat(hashes));
                let from = *i + 2 + hashes;
                *i = src[from..]
                    .find(&close)
                    .map(|p| from + p + close.len())
                    .unwrap_or(n);
                return true;
            }
        }
        if b[*i] == b'"' || b[*i] == b'\'' {
            let quote = b[*i];
            *i += 1;
            while *i < n && b[*i] != quote {
                *i += if b[*i] == b'\\' { 2 } else { 1 };
            }
            *i = (*i + 1).min(n);
            return true;
        }
        false
    };
    while i < src.len() {
        if skip_literal_or_comment(&mut i) {
            continue;
        }
        if !src[i..].starts_with("fn ")
            || (i > 0 && {
                let prev = bytes[i - 1];
                prev.is_ascii_alphanumeric() || prev == b'_'
            })
        {
            i += 1;
            continue;
        }
        let after_kw = i + 3;
        let name_end = src[after_kw..]
            .find(|c: char| !(c.is_alphanumeric() || c == '_'))
            .map(|p| after_kw + p)
            .unwrap_or(src.len());
        let name = src[after_kw..name_end].to_owned();
        let Some(open_paren) = src[name_end..].find('(').map(|p| name_end + p) else {
            i += 1;
            continue;
        };
        let Some(close_paren) = src[open_paren..].find(')').map(|p| open_paren + p) else {
            i += 1;
            continue;
        };
        let params = src[open_paren + 1..close_paren].trim();
        if !matches!(params, "" | "self" | "&self" | "&mut self") {
            i = close_paren;
            continue;
        }
        // The body opens at the next `{` that is not inside the return type (return types carry no
        // braces in generated code) — and a `;` first means this is a trait signature, not a body.
        let Some(rel) = src[close_paren..].find(['{', ';']) else {
            break;
        };
        let brace = close_paren + rel;
        if bytes[brace] == b';' {
            i = brace;
            continue;
        }
        let mut depth = 0usize;
        let mut j = brace;
        let end = loop {
            if j >= src.len() {
                break None;
            }
            if skip_literal_or_comment(&mut j) {
                continue;
            }
            match bytes[j] {
                b'{' => depth += 1,
                b'}' => {
                    depth -= 1;
                    if depth == 0 {
                        break Some(j);
                    }
                }
                _ => {}
            }
            j += 1;
        };
        match end {
            Some(end) => {
                out.push((name, brace, end));
                i = end + 1;
            }
            None => break,
        }
    }
    out
}

/// The canonical `cddl-codegen:replace` block that stubs the body between `open`/`close`, or `None`
/// if the body has no whole interior line to remove (a one-line body has no span to record).
///
/// The block grammar is the pinned one (`comment_preservation_replace_orphans_import_same_file`):
/// `replace-start`, the user code, `replaces`, the recorded original `//`-commented one line each,
/// `replace-end`. The recorded original is taken verbatim from THIS run's own output, so it is a
/// true anchor rather than a hand-written approximation of one.
fn replace_block_for_body(src: &str, open: usize, close: usize) -> Option<(usize, usize, String)> {
    let interior_start = src[open..close].find('\n').map(|p| open + p + 1)?;
    let interior_end = src[..close].rfind('\n')?;
    if interior_end <= interior_start {
        return None;
    }
    let interior = &src[interior_start..interior_end];
    if interior.trim().is_empty() {
        return None;
    }
    let indent: String = interior
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t')
        .collect();
    let mut block = format!(
        "{indent}// cddl-codegen:replace-start\n{indent}unimplemented!()\n{indent}// cddl-codegen:replaces\n"
    );
    for line in interior.lines() {
        block.push_str(&format!("{indent}// {}\n", line.trim()));
    }
    block.push_str(&format!("{indent}// cddl-codegen:replace-end"));
    Some((interior_start, interior_end, block))
}

/// Choose the injection site for one fixture's fresh output and write the block into the file.
///
/// Preference order: files in [`PER_TYPE_FILES`] order then the rest sorted, and within that, the
/// FIRST site whose stub ORPHANS an import in its own file — that is the shape the fix under test
/// was written for — else the first eligible site at all. Every half is a pure function of the
/// fixture's own output, so the choice is reproducible from the fixture alone.
fn inject_replace_block(out: &Path) -> Option<Injection> {
    let mut candidate_files: Vec<PathBuf> = PER_TYPE_FILES
        .iter()
        .map(PathBuf::from)
        .filter(|rel| out.join(rel).is_file())
        .collect();
    let rest: Vec<PathBuf> = tool_owned_rs_files(out)
        .into_iter()
        .filter(|rel| !candidate_files.contains(rel))
        .collect();
    candidate_files.extend(rest);
    let per_type = |rel: &Path| PER_TYPE_FILES.iter().any(|p| Path::new(p) == rel);

    let mut fallback: Option<(PathBuf, String, usize, usize, String)> = None;
    for rel in candidate_files {
        let path = out.join(&rel);
        let src = std::fs::read_to_string(&path).unwrap();
        let imported = crate::import_prune::collect_directly_imported_idents(&src);
        for (name, open, close) in self_only_fn_bodies(&src) {
            let Some((start, end, block)) = replace_block_for_body(&src, open, close) else {
                continue;
            };
            let stubbed = format!("{}{}{}", &src[..start], block, &src[end..]);
            let orphans = match (
                crate::import_prune::collect_used_idents_from_source(&src),
                crate::import_prune::collect_used_idents_from_source(&stubbed),
            ) {
                (Some(before), Some(after)) => imported
                    .iter()
                    .any(|id| before.contains(id) && !after.contains(id)),
                // A stub that does not parse is never a site (the compile leg would go red on the
                // harness's own edit rather than on the generator).
                _ => continue,
            };
            if orphans {
                std::fs::write(&path, &stubbed).unwrap();
                return Some(Injection {
                    per_type: per_type(&rel),
                    file: rel,
                    function: name,
                    orphans_import: true,
                });
            }
            if fallback.is_none() {
                fallback = Some((rel.clone(), name, start, end, block));
            }
        }
    }
    let (rel, name, start, end, block) = fallback?;
    let path = out.join(&rel);
    let src = std::fs::read_to_string(&path).unwrap();
    std::fs::write(&path, format!("{}{}{}", &src[..start], block, &src[end..])).unwrap();
    Some(Injection {
        per_type: per_type(&rel),
        file: rel,
        function: name,
        orphans_import: false,
    })
}

// ==================================================================================================
// The generation-only sweep (legs 1-3 minus compile)
// ==================================================================================================

/// How many worker THREADS the generation sweep splits the corpus across.
///
/// Threads rather than the sibling-`#[test]` sharding `feature_corpus_compiles` uses, because this
/// gate is `#[ignore]`d and every `#[ignore]`d test must be classified by a check.ts registry entry
/// (`self_checks` meta-check 1): sibling shards would be six registry gates for one question. A
/// FIXED count, never `nproc` — the memory rule the concurrency work installed is that no factor of
/// `(gates in flight) x (children per gate) x (RSS per child)` may scale with core count, and a
/// generator subprocess is small but not free.
const REGEN_WORKERS: usize = 6;

/// Floors, set BELOW a measured run so corpus churn does not flap them, and high enough that a leg
/// going structurally silent (an emitter stops writing a tree, the chooser stops finding candidates)
/// fails loudly.
///
/// Measured at the delivering run over the 91 corpus fixtures x 2 profiles: 1320 files scanned;
/// 177 deletion cells (`dsl_copy` and `extern_generic_raw_bytes` do not parse standalone — they name
/// user-supplied idents, the same reason both sit in `COMPILE_SKIP`; `dsl_ignore` deliberately does
/// not generate under `--preserve-encodings`), 12 of them deleting a `@used_as_key` rule; 91 edit
/// cells (default profile only), 89 of them in a per-type surface, 5 of them orphaning an import.
///
/// The orphan floor is the one that is LOW rather than merely conservative: whether a `self`-only
/// function body happens to hold the last use of a same-file import is a property of what the
/// corpus happens to emit, not something the sweep can arrange. Five is what this corpus offers;
/// the floor guards against that becoming zero (which is what a regression in the site chooser, or
/// an emitter that stops importing anything into the per-type surfaces, would look like).
const MIN_SCANNED_FILES: usize = 1000;
const MIN_DELETION_CELLS: usize = 150;
const MIN_EDIT_CELLS: usize = 80;
const MIN_EDIT_ORPHAN_CELLS: usize = 3;
const MIN_EDIT_PER_TYPE_CELLS: usize = 80;

/// What one worker observed. Summed across workers for the corpus-wide floors, which no single
/// worker can check.
#[derive(Default)]
struct SweepTally {
    failures: Vec<String>,
    skips: Vec<String>,
    scanned_files: usize,
    deletion_cells: usize,
    deletion_key_demand_cells: usize,
    edit_cells: usize,
    edit_orphan_cells: usize,
    edit_per_type_cells: usize,
}

/// Legs 1-3 over every corpus fixture. See the module docs for what each leg asserts and why the
/// compile half is a separate gate.
#[test]
#[ignore = "corpus-wide regen sweep: 91 fixtures x 5 generator runs (check.ts local)"]
fn regen_over_prior_output_corpus() {
    let all = feature_corpus_entries();
    let scratch_name = format!("cddl_codegen_regen_prior_{:016x}", checkout_hash());
    let _lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    // Generation is only hermetic against a clean tree — this whole module exists BECAUSE prior
    // output changes what the next run emits.
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();

    let tally = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..REGEN_WORKERS)
            .map(|worker| {
                // Round-robin over the SORTED corpus, so which fixture lands in which worker is
                // derivable from the corpus alone — a failure is reproducible, not a race.
                let entries: Vec<&PathBuf> = all
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| i % REGEN_WORKERS == worker)
                    .map(|(_, p)| p)
                    .collect();
                let root = &root;
                scope.spawn(move || sweep_fixtures(root, &entries))
            })
            .collect();
        let mut total = SweepTally::default();
        for handle in handles {
            let part = handle.join().expect("a sweep worker panicked");
            total.failures.extend(part.failures);
            total.skips.extend(part.skips);
            total.scanned_files += part.scanned_files;
            total.deletion_cells += part.deletion_cells;
            total.deletion_key_demand_cells += part.deletion_key_demand_cells;
            total.edit_cells += part.edit_cells;
            total.edit_orphan_cells += part.edit_orphan_cells;
            total.edit_per_type_cells += part.edit_per_type_cells;
        }
        total
    });

    let _ = std::fs::remove_dir_all(&root);

    let SweepTally {
        mut failures,
        mut skips,
        scanned_files,
        deletion_cells,
        deletion_key_demand_cells,
        edit_cells,
        edit_orphan_cells,
        edit_per_type_cells,
    } = tally;
    failures.sort();
    skips.sort();
    if !skips.is_empty() {
        println!(
            "regen_over_prior_output_corpus skipped {} cell(s):\n  {}",
            skips.len(),
            skips.join("\n  ")
        );
    }
    println!(
        "regen_over_prior_output_corpus: {scanned_files} file(s) scanned, {deletion_cells} deletion \
         cell(s) ({deletion_key_demand_cells} deleting a `@used_as_key` rule), {edit_cells} edit \
         cell(s) ({edit_orphan_cells} orphaning an import, {edit_per_type_cells} in a per-type surface)"
    );
    assert!(
        failures.is_empty(),
        "regen-over-prior-output failures:\n\n{}",
        failures.join("\n\n")
    );

    assert!(
        scanned_files >= MIN_SCANNED_FILES,
        "leg 1 scanned only {scanned_files} generated file(s) (floor {MIN_SCANNED_FILES}) — the \
         sweep stopped seeing the tool-owned trees, so its green means nothing"
    );
    assert!(
        deletion_cells >= MIN_DELETION_CELLS,
        "leg 2 exercised only {deletion_cells} deletion cell(s) (floor {MIN_DELETION_CELLS}) — the \
         deletion chooser stopped finding variants, so the trap class is unwatched"
    );
    assert!(
        deletion_key_demand_cells >= 1,
        "leg 2 exercised NO `@used_as_key`-tagged deletion over a fixture that emits \
         key_demand_assertions.rs — that file's rows are vacuously green (the roadmap entry's \
         second shaping note)"
    );
    assert!(
        edit_cells >= MIN_EDIT_CELLS,
        "leg 3 exercised only {edit_cells} replace-block cell(s) (floor {MIN_EDIT_CELLS}) — the \
         injection-site chooser stopped finding targets"
    );
    assert!(
        edit_per_type_cells >= MIN_EDIT_PER_TYPE_CELLS,
        "leg 3 injected only {edit_per_type_cells} block(s) into a PER-TYPE generated surface (floor \
         {MIN_EDIT_PER_TYPE_CELLS}) — the rest landed in composed static runtime files, whose bytes \
         are identical for every fixture, so the sweep would be one cell repeated"
    );
    assert!(
        edit_orphan_cells >= MIN_EDIT_ORPHAN_CELLS,
        "leg 3 injected only {edit_orphan_cells} block(s) that orphan an import (floor \
         {MIN_EDIT_ORPHAN_CELLS}) — the post-overlay re-prune is the mechanism under test, and a \
         sweep of trivial sites never reaches it"
    );
}

/// One worker's slice of the corpus. Batched rather than fail-fast: a single run must name every
/// problem it saw, not the first.
fn sweep_fixtures(root: &Path, entries: &[&PathBuf]) -> SweepTally {
    let mut failures: Vec<String> = Vec::new();
    let mut skips: Vec<String> = Vec::new();
    let mut scanned_files = 0usize;
    let mut deletion_cells = 0usize;
    let mut deletion_key_demand_cells = 0usize;
    let mut edit_cells = 0usize;
    let mut edit_orphan_cells = 0usize;
    let mut edit_per_type_cells = 0usize;

    for input in entries {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        let cell = root.join(stem);
        std::fs::create_dir_all(&cell).unwrap();
        let text = std::fs::read_to_string(input).unwrap();

        for (profile, extra) in FLOOR_AND_DELETION_PROFILES {
            let label = format!("{stem}/{profile}");
            // Generate from a COPY at a stable scratch path: the deletion variant must be generated from
            // the same `--input` path as the fresh run, or the two runs would differ by more than the
            // deleted rule.
            let spec = cell.join(format!("{profile}.cddl"));
            std::fs::write(&spec, &text).unwrap();
            let out = cell.join(format!("out-{profile}"));

            if let Err(stderr) = generate(&spec, &out, extra) {
                if *profile == FLOOR_AND_DELETION_PROFILES[0].0 {
                    failures.push(format!("{label}: fresh generation failed\n{stderr}"));
                } else {
                    // A SECONDARY profile's generation verdict is not this gate's to own:
                    // `feature_corpus_compiles` gates it both ways through `EXPECTED_GENERATION_FAIL`
                    // (a listed pair that starts generating fails as a stale pin). Recording the skip
                    // keeps that ownership single, and the cell still counts nowhere.
                    skips.push(format!(
                    "{label}: does not generate under this profile (owned by feature_corpus_compiles)"
                ));
                }
                continue;
            }

            // ---- Leg 1: the static floor over the FRESH trees --------------------------------------
            let fresh = read_tool_owned(&out);
            scanned_files += fresh.len();
            for (rel, content) in &fresh {
                match crate::comment_preserve::comments_sharing_a_code_row(content) {
                    Ok(hits) => {
                        for (line, text) in hits {
                            failures.push(format!(
                            "{label}: {}:{line}: a generated comment shares its row with code: `{text}`\n  \
                             A comment on a row a spec change can DELETE is stranded into a \
                             self-perpetuating `compile_error!` sentinel on the next in-place regen. \
                             Fix the EMITTER (move the comment into a fixed banner or drop it), the way \
                             `extern_interface_check.rs` and `key_demand_assertions.rs` were fixed.",
                            rel.display()
                        ));
                        }
                    }
                    Err(e) => failures.push(format!(
                        "{label}: {}: generated file does not lex: {e}",
                        rel.display()
                    )),
                }
            }
            let had_key_demand = fresh.keys().any(|rel| {
                rel.file_name()
                    .is_some_and(|n| n == "key_demand_assertions.rs")
            });

            // ---- Leg 2: the rule-DELETION variant, regenerated IN PLACE ----------------------------
            match rule_spans(&text) {
                None => skips.push(format!("{label}: leg2 — fixture does not parse standalone")),
                Some(spans) if spans.is_empty() => {
                    skips.push(format!("{label}: leg2 — no rules to delete"))
                }
                Some(spans) => {
                    let candidates = deletion_candidates(&text, &spans);
                    let mut exercised = None;
                    for candidate in &candidates {
                        let variant = deletion_variant(&text, &spans, candidate);
                        std::fs::write(&spec, &variant).unwrap();
                        if generate(&spec, &out, extra).is_err() {
                            continue;
                        }
                        let after = read_tool_owned(&out);
                        if after == fresh {
                            // Vacuous: the deletion changed no emitted byte, so nothing was stranded and
                            // the cell proves nothing. Try the next candidate.
                            continue;
                        }
                        exercised = Some((candidate.clone(), after));
                        break;
                    }
                    match exercised {
                    None => skips.push(format!(
                        "{label}: leg2 — no candidate rule yields a generating, output-changing variant"
                    )),
                    Some((candidate, after)) => {
                        deletion_cells += 1;
                        let tagged = spans.iter().any(|s| {
                            s.name == candidate
                                && text.lines().collect::<Vec<_>>()
                                    [s.first_line..=s.last_line]
                                    .iter()
                                    .any(|l| l.contains("@used_as_key"))
                        });
                        if had_key_demand && tagged {
                            deletion_key_demand_cells += 1;
                        }
                        for (rel, content) in &after {
                            if content.contains(TRAP_MARKER) {
                                failures.push(format!(
                                    "{label}: deleting rule `{candidate}` and regenerating IN PLACE left a \
                                     `{TRAP_MARKER}` sentinel in {} — a comment on a deleted row was \
                                     stranded, and every further regen carries the resulting \
                                     `compile_error!` forward. The fix is emitter-side.",
                                    rel.display()
                                ));
                            }
                        }
                    }
                }
                }
            }
        }

        // ---- Leg 3: the user-EDIT (replace block) variant ---------------------------------------
        // Its OWN output tree, not leg 2's: a sentinel is carried forward verbatim by design, so a
        // leg-2 trap left on disk would re-report itself here and make the edit leg's verdict a
        // restatement of the deletion leg's. One extra generation buys two independent verdicts.
        let edit_spec = cell.join("edit.cddl");
        std::fs::write(&edit_spec, &text).unwrap();
        let edit_out = cell.join("edit-out");
        if let Err(stderr) = generate(&edit_spec, &edit_out, &[]) {
            failures.push(format!(
                "{stem}: leg3 — generating the edit leg's own fresh output failed\n{stderr}"
            ));
            continue;
        }
        let out = &edit_out;
        let spec = &edit_spec;
        match inject_replace_block(out) {
            None => skips.push(format!(
                "{stem}: leg3 — no eligible replace-block injection site"
            )),
            Some(injection) => {
                edit_cells += 1;
                if injection.orphans_import {
                    edit_orphan_cells += 1;
                }
                if injection.per_type {
                    edit_per_type_cells += 1;
                }
                if let Err(stderr) = generate(spec, out, &[]) {
                    failures.push(format!(
                        "{stem}: leg3 — regeneration over the injected replace block in {} ({}) failed\n{stderr}",
                        injection.file.display(),
                        injection.function
                    ));
                    continue;
                }
                let applied = read_tool_owned(out);
                let edited = applied.get(&injection.file).cloned().unwrap_or_default();
                if !edited.contains("cddl-codegen:replace-start") {
                    failures.push(format!(
                        "{stem}: leg3 — the injected replace block in {} ({}) did not survive the regen \
                         (the overlay dropped a user block it owns)",
                        injection.file.display(),
                        injection.function
                    ));
                }
                for (rel, content) in &applied {
                    if content.contains(TRAP_MARKER) {
                        failures.push(format!(
                            "{stem}: leg3 — regenerating over a `cddl-codegen:replace` block in {} left a \
                             `{TRAP_MARKER}` sentinel in {}: the block stranded instead of applying.",
                            injection.file.display(),
                            rel.display()
                        ));
                    }
                }
                if let Err(stderr) = generate(spec, out, &[]) {
                    failures.push(format!("{stem}: leg3 — the second regen failed\n{stderr}"));
                    continue;
                }
                let settled = read_tool_owned(out);
                if settled != applied {
                    let moved: Vec<String> = settled
                        .iter()
                        .filter(|(rel, content)| applied.get(*rel) != Some(content))
                        .map(|(rel, _)| rel.display().to_string())
                        .collect();
                    failures.push(format!(
                        "{stem}: leg3 — regenerating twice over the same replace block is not a fixed \
                         point; these files moved on the second regen: {}",
                        moved.join(", ")
                    ));
                }
            }
        }
    }

    SweepTally {
        failures,
        skips,
        scanned_files,
        deletion_cells,
        deletion_key_demand_cells,
        edit_cells,
        edit_orphan_cells,
        edit_per_type_cells,
    }
}

// ==================================================================================================
// The compile leg (its own gate: the only one that pays cargo)
// ==================================================================================================

/// Does the crate a user EDIT was regenerated over still build, warning-clean?
///
/// This is the half that catches the orphaned-`use` class at corpus scale: a replace block removing
/// an import's last user must take the import with it, and `unused import` is a WARNING — invisible
/// to any assertion about generation succeeding. The scan is the `feature_corpus_compiles` one
/// (`unused_generated_import_lines` / `unused_generated_variable_lines`), which is sound here for
/// the same reason it is there: these crates are 100% generated, so any such warning is the
/// generator's, not a hand-written file's.
///
/// Split from the generation sweep rather than folded into it because its cost class is different by
/// an order of magnitude (nested cargo per fixture vs a generator subprocess), which is also why it
/// is gate-cached: an unchanged generated tree re-uses its verdict.
#[test]
#[ignore = "manual-only heavy gate: nested cargo per corpus fixture (check.ts full)"]
fn regen_over_prior_output_corpus_compiles() {
    let scratch_name = format!("cddl_codegen_regen_prior_compile_{:016x}", checkout_hash());
    let _lock = acquire_scratch_lock(&scratch_name);
    let root = std::env::temp_dir().join(&scratch_name);
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    let target_dir = root.join("target");

    let mut failures: Vec<String> = Vec::new();
    let mut skips: Vec<String> = Vec::new();
    let mut cells = 0usize;
    let mut orphan_cells = 0usize;
    let mut cache_run = 0usize;
    let mut cache_hit = 0usize;

    for input in feature_corpus_entries() {
        let stem = input.file_stem().unwrap().to_str().unwrap().to_owned();
        // The corpus fixtures whose generated crate references user-supplied code never compile
        // standalone under any profile — the same exclusion `feature_corpus_compiles` makes.
        if COMPILE_SKIP.contains(&stem.as_str()) {
            continue;
        }
        let cell = root.join(&stem);
        std::fs::create_dir_all(&cell).unwrap();
        let spec = cell.join("spec.cddl");
        std::fs::copy(&input, &spec).unwrap();
        let out = cell.join("out");
        if let Err(stderr) = generate(&spec, &out, &[]) {
            failures.push(format!("{stem}: fresh generation failed\n{stderr}"));
            continue;
        }
        let Some(injection) = inject_replace_block(&out) else {
            skips.push(format!("{stem}: no eligible replace-block injection site"));
            continue;
        };
        if let Err(stderr) = generate(&spec, &out, &[]) {
            failures.push(format!(
                "{stem}: regeneration over the injected replace block in {} ({}) failed\n{stderr}",
                injection.file.display(),
                injection.function
            ));
            continue;
        }
        cells += 1;
        if injection.orphans_import {
            orphan_cells += 1;
        }

        let crate_subs = ["rust", "wasm"];
        let manifest_subpaths: Vec<PathBuf> = crate_subs
            .iter()
            .map(|sub| PathBuf::from(sub).join("Cargo.toml"))
            .collect();
        let mut argv_for_key: Vec<String> = crate_subs
            .iter()
            .flat_map(|sub| [format!("cwd={sub}"), "cargo".into(), "check".into()])
            .collect();
        // Verdict-logic version marker, the `feature_corpus_compiles` discipline: the cached cell's
        // verdict depends on the injection rule AND the warning scan, neither of which is in the
        // hashed tree. Bump on any change to either.
        argv_for_key.push("regen-edit=v1".to_string());
        let outcome = super::gate_cache::run_cached(
            "regen_over_prior_output_corpus_compiles",
            &stem,
            &out,
            &manifest_subpaths,
            &argv_for_key,
            || {
                let mut ok = true;
                for sub in crate_subs {
                    let crate_dir = out.join(sub);
                    if !crate_dir.exists() {
                        failures.push(format!("{stem} ({sub}): crate dir missing — the fixture is no longer being compile-gated"));
                        ok = false;
                        continue;
                    }
                    let check = tool_cmd("cargo")
                        .arg("check")
                        .current_dir(&crate_dir)
                        .env("CARGO_TARGET_DIR", &target_dir)
                        .output()
                        .unwrap();
                    let stderr = String::from_utf8_lossy(&check.stderr);
                    if !check.status.success() {
                        failures.push(format!(
                            "{stem} ({sub}): cargo check failed after regenerating over a \
                             `cddl-codegen:replace` block in {} ({})\n{}\n{}",
                            injection.file.display(),
                            injection.function,
                            String::from_utf8_lossy(&check.stdout),
                            stderr
                        ));
                        ok = false;
                    }
                    let mut hits = unused_generated_import_lines(&stderr);
                    hits.extend(unused_generated_variable_lines(&stderr));
                    if !hits.is_empty() {
                        failures.push(format!(
                            "{stem} ({sub}): the regenerated crate carries {} unused-import/variable \
                             warning(s) after a replace block removed code in {} ({}) — the \
                             post-overlay re-prune under-pruned, and this residue reaches a consumer \
                             as build noise:\n{}",
                            hits.len(),
                            injection.file.display(),
                            injection.function,
                            hits.join("\n")
                        ));
                        ok = false;
                    }
                }
                ok
            },
        );
        cache_run += outcome.ran();
        cache_hit += outcome.cached();
    }

    let _ = std::fs::remove_dir_all(&root);

    if !skips.is_empty() {
        println!(
            "regen_over_prior_output_corpus_compiles skipped {} cell(s):\n  {}",
            skips.len(),
            skips.join("\n  ")
        );
    }
    println!(
        "regen_over_prior_output_corpus_compiles: {cells} cell(s) ({orphan_cells} orphaning an \
         import), gate-cache: {cache_run} run, {cache_hit} cached"
    );
    assert!(
        failures.is_empty(),
        "regenerated crates failed to compile warning-clean:\n\n{}",
        failures.join("\n\n")
    );
    assert!(
        cells >= MIN_EDIT_CELLS,
        "only {cells} fixture(s) were compile-gated after a regen over a replace block (floor \
         {MIN_EDIT_CELLS}) — the injection-site chooser stopped finding targets"
    );
    assert!(
        orphan_cells >= MIN_EDIT_ORPHAN_CELLS,
        "only {orphan_cells} of the compile-gated cells orphan an import (floor \
         {MIN_EDIT_ORPHAN_CELLS}) — the re-prune is the mechanism under test"
    );
}

// ==================================================================================================
// Unit pins for the sweep's own derivations
// ==================================================================================================

#[cfg(test)]
mod derivation_tests {
    use super::*;

    /// The deletion machinery on a spec with all three shapes the corpus has: a leaf rule, a
    /// referenced rule (whose deletion must cascade), and a `@used_as_key` trailing directive that
    /// must travel with its rule rather than re-attaching to the next one.
    #[test]
    fn deletion_variant_deletes_the_closure_and_its_directives() {
        let text = "; a header comment\n\
                    key = uint / nint ; @used_as_key\n\
                    tbl = { * key => text }\n\
                    leaf = [x: uint]\n";
        let spans = rule_spans(text).expect("fixture must parse");
        assert_eq!(
            spans.iter().map(|s| s.name.as_str()).collect::<Vec<_>>(),
            vec!["key", "tbl", "leaf"]
        );

        // A leaf nothing references: only its own line goes.
        let leaf_gone = deletion_variant(text, &spans, "leaf");
        assert!(leaf_gone.contains("key = uint"), "{leaf_gone}");
        assert!(leaf_gone.contains("tbl = {"), "{leaf_gone}");
        assert!(!leaf_gone.contains("leaf"), "{leaf_gone}");

        // A referenced rule: the referrer goes with it, or the variant would not parse.
        let key_gone = deletion_variant(text, &spans, "key");
        assert!(
            !key_gone.contains("@used_as_key"),
            "the tagged rule's trailing directive must go with the rule, never re-attach:\n{key_gone}"
        );
        assert!(
            !key_gone.contains("tbl = {"),
            "the referrer must cascade:\n{key_gone}"
        );
        assert!(key_gone.contains("leaf = [x: uint]"), "{key_gone}");
        assert!(key_gone.contains("; a header comment"), "{key_gone}");

        // Tagged rules are tried FIRST in a tagged fixture, so the tagged path is never skipped in
        // favour of an easier leaf.
        assert_eq!(deletion_candidates(text, &spans)[0], "key");
    }

    /// `line_declares` must not accept a PREFIX of a longer rule name — the failure would delete the
    /// wrong lines and produce a variant that still generates, so nothing downstream would notice.
    #[test]
    fn a_rule_name_never_matches_a_longer_neighbour() {
        assert!(line_declares("foo = uint", "foo"));
        assert!(line_declares("foo<a> = [* a]", "foo"));
        assert!(line_declares("foo /= tstr", "foo"));
        assert!(!line_declares("foobar = uint", "foo"));
        assert!(!line_declares("foo-bar = uint", "foo"));
        assert!(!line_declares("  foo = uint", "foo"));
    }

    /// The injection rule: only a `self`-only function with a multi-line body is a site (a body with
    /// parameters would leave them unused and turn the compile leg's scan into a report about the
    /// harness), and the emitted block is the pinned grammar with the original recorded verbatim.
    #[test]
    fn injection_targets_only_self_only_multi_line_bodies() {
        let src = "impl X {\n    \
                   pub fn with_params(&self, n: u64) -> u64 {\n        n\n    }\n    \
                   pub fn one_liner(&self) -> u64 { 1 }\n    \
                   pub fn body(&self) -> u64 {\n        let v = 1;\n        v\n    }\n}\n";
        let bodies = self_only_fn_bodies(src);
        assert_eq!(
            bodies
                .iter()
                .map(|(n, _, _)| n.as_str())
                .collect::<Vec<_>>(),
            vec!["one_liner", "body"],
            "a function with parameters is never a site"
        );
        let (_, open, close) = bodies
            .iter()
            .find(|(n, _, _)| n == "one_liner")
            .expect("one_liner is listed");
        assert!(
            replace_block_for_body(src, *open, *close).is_none(),
            "a one-line body has no interior line to record as the original"
        );
        let (_, open, close) = bodies.iter().find(|(n, _, _)| n == "body").unwrap();
        let (_, _, block) = replace_block_for_body(src, *open, *close).expect("multi-line body");
        assert_eq!(
            block,
            "        // cddl-codegen:replace-start\n        unimplemented!()\n        \
             // cddl-codegen:replaces\n        // let v = 1;\n        // v\n        \
             // cddl-codegen:replace-end"
        );
    }
}
