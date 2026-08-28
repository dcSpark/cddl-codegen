//! The fixture harness sometimes appends hand-authored source after generating a throwaway crate.
//! This audit makes `src/generated/**` the exceptional case: a new append there must select one of
//! the reviewed, real-contract reasons below rather than silently modeling a user-owned generated
//! tree.

use std::path::{Path, PathBuf};

const MARKER: &str = "cddl-codegen:generated-append";
const REASONS: &[&str] = &[
    "crate-root-reexport",
    "generated-module-test-body",
    "generated-sibling-helper",
];

#[derive(Debug, Eq, PartialEq)]
struct Candidate {
    line: usize,
    kind: &'static str,
}

#[derive(Debug)]
struct Marker<'a> {
    line: usize,
    reason: &'a str,
    used: bool,
}

/// Every repository-owned test, verification, and fuzz source root is recursive so adding a source
/// file cannot bypass the audit by omission from a hand-maintained file list.
fn audited_sources() -> Vec<PathBuf> {
    let mut paths = Vec::new();
    collect_sources(Path::new("src/tests"), "rs", &mut paths);
    collect_sources(Path::new("cddl-matrix"), "ts", &mut paths);
    collect_sources(Path::new("fuzz"), "sh", &mut paths);
    paths.sort();
    paths
}

fn collect_sources(dir: &Path, extension: &str, paths: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("read source root {}: {error}", dir.display()))
    {
        let entry =
            entry.unwrap_or_else(|error| panic!("read entry below {}: {error}", dir.display()));
        let path = entry.path();
        if path.is_dir() {
            if matches!(
                path.file_name().and_then(|name| name.to_str()),
                Some("node_modules" | "target" | ".git")
            ) {
                continue;
            }
            collect_sources(&path, extension, paths);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some(extension) {
            paths.push(path);
        }
    }
}

fn line_number(source: &str, byte: usize) -> usize {
    source[..byte].bytes().filter(|byte| *byte == b'\n').count() + 1
}

fn markers<'a>(source: &'a str, errors: &mut Vec<String>) -> Vec<Marker<'a>> {
    source
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            let trimmed = line.trim_start();
            let comment = trimmed
                .strip_prefix("//")
                .or_else(|| trimmed.strip_prefix('#'))?
                .trim_start();
            let rest = comment.strip_prefix(MARKER)?;
            let Some(reason) = rest.strip_prefix(" reason=") else {
                errors.push(format!(
                    "line {}: `{MARKER}` must be exactly `{MARKER} reason=<closed-reason>`",
                    index + 1
                ));
                return None;
            };
            if reason.is_empty() || reason.contains(char::is_whitespace) {
                errors.push(format!(
                    "line {}: malformed generated-tree append reason `{reason}`",
                    index + 1
                ));
            } else if !REASONS.contains(&reason) {
                errors.push(format!(
                    "line {}: unknown generated-tree append reason `{reason}` (known: {})",
                    index + 1,
                    REASONS.join(", ")
                ));
            }
            Some(Marker {
                line: index + 1,
                reason,
                used: false,
            })
        })
        .collect()
}

fn statement_after(source: &str, start: usize) -> &str {
    let tail = &source[start..];
    &tail[..tail.find(';').unwrap_or(tail.len())]
}

fn variable_mentions_generated(source_before_open: &str, variable: &str) -> bool {
    let needle = format!("let {variable}");
    source_before_open
        .rmatch_indices(&needle)
        .next()
        .is_some_and(|(start, _)| statement_after(source_before_open, start).contains("generated"))
}

fn open_destination_mentions_generated(statement: &str, source_before_open: &str) -> bool {
    if statement.contains("src/generated/") {
        return true;
    }
    let Some(open) = statement.find(".open(&") else {
        return false;
    };
    let variable = statement[open + ".open(&".len()..]
        .chars()
        .take_while(|character| character.is_ascii_alphanumeric() || *character == '_')
        .collect::<String>();
    !variable.is_empty() && variable_mentions_generated(source_before_open, &variable)
}

fn rust_candidates(source: &str) -> Vec<Candidate> {
    let mut candidates = Vec::new();
    let mut offset = 0;
    while let Some(found) = source[offset..].find(".append(") {
        let append = offset + found;
        let statement = statement_after(source, append);
        if open_destination_mentions_generated(statement, &source[..append]) {
            candidates.push(Candidate {
                line: line_number(source, append),
                kind: "Rust OpenOptions::append",
            });
        }
        offset = append + ".append(".len();
    }
    candidates
}

fn ts_candidates(source: &str) -> Vec<Candidate> {
    let mut candidates = Vec::new();
    let mut offset = 0;
    while let Some(found) = source[offset..].find("appendFileSync(") {
        let append = offset + found;
        let statement = statement_after(source, append);
        if statement.contains("generated") {
            candidates.push(Candidate {
                line: line_number(source, append),
                kind: "TypeScript appendFileSync",
            });
        }
        offset = append + "appendFileSync(".len();
    }
    candidates
}

fn shell_candidates(source: &str) -> Vec<Candidate> {
    source
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            line.split_once(">>")
                .filter(|(_, destination)| destination.contains("src/generated/"))
                .map(|_| Candidate {
                    line: index + 1,
                    kind: "shell append redirect",
                })
        })
        .collect()
}

fn candidates(path: &str, source: &str) -> Vec<Candidate> {
    if path.ends_with(".rs") {
        rust_candidates(source)
    } else if path.ends_with(".ts") {
        ts_candidates(source)
    } else if path.ends_with(".sh") {
        shell_candidates(source)
    } else {
        Vec::new()
    }
}

fn audit_source(path: &str, source: &str) -> Result<Vec<Candidate>, Vec<String>> {
    let mut errors = Vec::new();
    let mut markers = markers(source, &mut errors);
    let candidates = candidates(path, source);
    for candidate in &candidates {
        let marker = markers
            .iter_mut()
            .find(|marker| marker.line + 1 == candidate.line);
        match marker {
            Some(marker) if REASONS.contains(&marker.reason) => marker.used = true,
            Some(_) => {}
            None => errors.push(format!(
                "{path}:{}: {} writes hand-authored bytes into src/generated/** without a `{MARKER}` marker",
                candidate.line, candidate.kind
            )),
        }
    }
    for marker in markers.into_iter().filter(|marker| !marker.used) {
        errors.push(format!(
            "{path}:{}: stale `{MARKER} reason={}` marker has no generated-tree append on its next line",
            marker.line, marker.reason
        ));
    }
    if errors.is_empty() {
        Ok(candidates)
    } else {
        Err(errors)
    }
}

fn audit_error(path: &str, source: String) -> String {
    audit_source(path, &source)
        .expect_err("fixture must be rejected")
        .join("\n")
}

fn rust_append() -> String {
    [".append", "(true)"].concat()
}

#[test]
fn generated_tree_append_audit_rejects_unmarked_rust_literal_path() {
    let source = format!(
        "let file = OpenOptions::new(){}\n    .open(out.join(\"rust/src/generated/mod.rs\"));",
        rust_append()
    );
    assert!(audit_error("fixture.rs", source).contains("without a"));
}

#[test]
fn generated_tree_append_audit_rejects_unmarked_rust_variable_path() {
    let source = format!(
        "let generated_mod_path = out.join(\"rust/src/generated/mod.rs\");\nlet file = OpenOptions::new(){}\n    .open(&generated_mod_path);",
        rust_append()
    );
    assert!(audit_error("fixture.rs", source).contains("without a"));
}

#[test]
fn generated_tree_append_audit_rejects_unmarked_ts_append() {
    let source = "appendFileSync(join(out, \"rust\", \"src\", \"generated\", \"mod.rs\"), hand);";
    assert!(audit_error("fixture.ts", source.to_owned()).contains("without a"));
}

#[test]
fn generated_tree_append_audit_rejects_unmarked_shell_redirect() {
    let source = "printf '%s' hand >> out/rust/src/generated/mod.rs";
    assert!(audit_error("fixture.sh", source.to_owned()).contains("without a"));
}

#[test]
fn generated_tree_append_audit_accepts_a_known_reason() {
    let source = format!(
        "// {MARKER} reason=generated-sibling-helper\nlet file = OpenOptions::new(){}\n    .open(out.join(\"rust/src/generated/mod.rs\"));",
        rust_append()
    );
    assert_eq!(audit_source("fixture.rs", &source).unwrap().len(), 1);
}

#[test]
fn generated_tree_append_audit_rejects_an_unknown_reason() {
    let source = format!(
        "// {MARKER} reason=hand-waved\nlet file = OpenOptions::new(){}\n    .open(out.join(\"rust/src/generated/mod.rs\"));",
        rust_append()
    );
    assert!(audit_error("fixture.rs", source).contains("unknown generated-tree append reason"));
}

#[test]
fn generated_tree_append_audit_rejects_a_stale_marker() {
    let source = format!("// {MARKER} reason=generated-module-test-body\nlet untouched = 0;");
    assert!(audit_error("fixture.rs", source).contains("stale"));
}

#[test]
fn generated_tree_append_sites_have_reviewed_reasons() {
    let mut failures = Vec::new();
    for path in audited_sources() {
        let display = path.display().to_string();
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read audited source {display}: {error}"));
        if let Err(errors) = audit_source(&display, &source) {
            failures.extend(errors);
        }
    }
    assert!(
        failures.is_empty(),
        "generated-tree append audit failed:\n{}",
        failures.join("\n")
    );
}
