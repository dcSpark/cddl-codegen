#!/usr/bin/env bun
/**
 * Documentation citation lint — PURE FILE READS, no cargo, no oracles.
 *
 * Three small checks keep gap-tracking prose maintainable:
 *   1. Citation existence: in the shipped gap-tracking docs, every
 *      `pinned by` / `tracked by` / `gated by` backticked token must resolve to a real tracked path or
 *      fixed-string occurrence outside those same hand docs. Brace shorthands such as
 *      `ctl.{le,lt}` expand before resolution, so compact row-family prose still points at exact pins.
 *   2. Positional citation ban: durable docs/code/scripts must not cite moving roadmap/list positions.
 *      `draft/` is intentionally excluded because scratchpads are historical working notes, while the
 *      AGENTS.md convention targets durable docs and code comments. In-doc examples of the banned shape
 *      should use the unmatchable placeholder spelling `item <N>`.
 *   3. MD022: ATX headings in the same shipped hand docs must be preceded by a blank line (file start ok,
 *      fenced code blocks ignored).
 *
 * Run from cddl-matrix/:
 *   bun run lint_doc_citations.ts
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;
const ROOT = `${HERE}/..`;

interface TrackedFile { rel: string; text: string }
interface Citation { doc: string; line: number; kind: string; raw: string; expanded: string[] }
interface TitleCitation { doc: string; line: number; title: string }

function gitLsFiles(): string[] {
  const r = Bun.spawnSync(["git", "ls-files", "-z"], { cwd: ROOT, stdout: "pipe", stderr: "inherit" });
  if ((r.exitCode ?? 1) !== 0) {
    console.error(`doc-citation lint: git ls-files failed with exit ${r.exitCode ?? 1}`);
    process.exit(2);
  }
  return (r.stdout?.toString("utf8") ?? "").split("\0").filter(Boolean).sort();
}

function readText(rel: string): string | null {
  const buf = readFileSync(`${ROOT}/${rel}`);
  if (buf.includes(0)) return null;
  return buf.toString("utf8");
}

function lineOf(text: string, offset: number): number {
  let line = 1;
  for (let i = 0; i < offset; i++) if (text.charCodeAt(i) === 10) line++;
  return line;
}

function stopImmediateCitation(text: string, start: number): number {
  let inBacktick = false;
  let sawToken = false;
  const limit = Math.min(text.length, start + 420);
  for (let i = start; i < limit; i++) {
    const ch = text[i]!;
    if (ch === "`") {
      inBacktick = !inBacktick;
      if (!inBacktick) sawToken = true;
      continue;
    }
    if (!inBacktick && sawToken && (ch === "." || ch === ";" || ch === "\n" && text[i + 1] === "\n")) return i;
  }
  return limit;
}

function expandBraceToken(token: string): string[] {
  const m = token.match(/^(.*)\{([^{}]+)\}(.*)$/);
  if (!m) return [token];
  const [, before, inner, after] = m;
  return inner.split(",").map(part => `${before}${part.trim()}${after}`);
}

function extractCitations(doc: string, text: string): { identifiers: Citation[]; titles: TitleCitation[] } {
  const identifiers: Citation[] = [];
  const titles: TitleCitation[] = [];
  const citationRe = /\b(pinned|tracked|gated)\s+by\b/gi;
  for (const m of text.matchAll(citationRe)) {
    const start = m.index ?? 0;
    const end = stopImmediateCitation(text, start + m[0].length);
    const segment = text.slice(start, end);
    const raws = [...segment.matchAll(/`([^`\n]+)`/g)].map(mm => mm[1]!);
    for (const raw of raws) identifiers.push({
      doc,
      line: lineOf(text, start),
      kind: m[1]!.toLowerCase(),
      raw,
      expanded: expandBraceToken(raw),
    });
  }

  const titleRe = /\bcited\s+by\s+"([^"\n]+)"/gi;
  for (const m of text.matchAll(titleRe)) {
    titles.push({ doc, line: lineOf(text, m.index ?? 0), title: m[1]! });
  }

  return { identifiers, titles };
}

function headingTitles(files: TrackedFile[]): Set<string> {
  const titles = new Set<string>();
  for (const f of files.filter(f => f.rel.endsWith(".md") || f.rel.endsWith(".mdx"))) {
    let inFence = false;
    for (const line of f.text.split("\n")) {
      if (/^\s*(```|~~~)/.test(line)) { inFence = !inFence; continue; }
      if (inFence) continue;
      const m = line.match(/^\s{0,3}#{1,6}\s+(.+?)\s*#*\s*$/);
      if (m) titles.add(m[1]!.replace(/\*\*/g, "").trim());
    }
  }
  return titles;
}

function md022Problems(rel: string, text: string): string[] {
  const problems: string[] = [];
  const lines = text.split("\n");
  let inFence = false;
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    if (/^\s*(```|~~~)/.test(line)) { inFence = !inFence; continue; }
    if (inFence) continue;
    if (!/^\s{0,3}#{1,6}\s+\S/.test(line)) continue;
    if (i === 0) continue;
    if (lines[i - 1]!.trim() !== "") problems.push(`${rel}:${i + 1}: heading must be preceded by a blank line`);
  }
  return problems;
}

function positionalCitationProblems(file: TrackedFile): string[] {
  if (file.rel.startsWith("draft/")) return [];
  const problems: string[] = [];
  const patterns = [
    // Roadmap-adjacent citations are always positional, including `TESTING_ROADMAP.md item #6`.
    /\b(?:[A-Za-z0-9_./-]*ROADMAP(?:\.md)?|roadmap)\s+(?:§\s+[^:\n()]{0,80}\s+)?item\s*#?\d+\b/gi,
    // Bare forms are limited to citation verbs/prepositions to avoid standards prose such as
    // "item 6 of Section 3.1" in pinned upstream source text.
    /\b(?:see|from|in|by|tracks?|tracked|pinned|gated|cite[sd]?|refer(?:s|red)?\s+to)\s+item\s*#?\d+\b/gi,
  ];
  for (const pattern of patterns) {
    for (const m of file.text.matchAll(pattern)) {
      const line = file.text.slice(0, m.index ?? 0).split("\n").pop() ?? "";
      if (line.includes("item <N>")) continue;
      problems.push(`${file.rel}:${lineOf(file.text, m.index ?? 0)}: positional citation '${m[0]}' is unstable; cite a pin identifier or exact title instead (write examples as \`item <N>\`)`);
    }
  }
  return problems;
}

const trackedRels = gitLsFiles();
const allFiles: TrackedFile[] = [];
for (const rel of trackedRels) {
  const text = readText(rel);
  if (text !== null) allFiles.push({ rel, text });
}

const handDocSet = new Set<string>([
  ...trackedRels.filter(rel => rel.endsWith(".md") && rel.includes("ROADMAP") && !rel.startsWith("draft/")),
  "tests/README.md",
  "cddl-matrix/README.md",
]);
const handDocs = [...handDocSet].sort();

const problems: string[] = [];
const missingHandDocs = handDocs.filter(rel => !trackedRels.includes(rel));
for (const rel of missingHandDocs) problems.push(`${rel}: hand doc is not tracked by git`);

const handDocFiles = handDocs.flatMap(rel => {
  const text = readText(rel);
  return text === null ? [] : [{ rel, text }];
});

const handDocExclusions = new Set(handDocs);
// draft/ is excluded from the RESOLUTION corpus too, not just the ban's scope: scratchpads
// routinely quote the same gap prose, so a retired pin could keep "resolving" via a stale
// investigation note — exactly the rot this lint exists to catch.
const searchable = allFiles.filter(f => !handDocExclusions.has(f.rel) && !f.rel.startsWith("draft/"));
const searchablePaths = new Set(searchable.map(f => f.rel));

function existsOutsideHandDocs(identifier: string): boolean {
  if (searchablePaths.has(identifier)) return true;
  return searchable.some(f => f.text.includes(identifier));
}

let identifierCitationCount = 0;
const perDocCitationCounts = new Map<string, number>();
const titles = headingTitles(allFiles);

for (const doc of handDocFiles) {
  const extracted = extractCitations(doc.rel, doc.text);
  for (const citation of extracted.identifiers) {
    identifierCitationCount++;
    perDocCitationCounts.set(doc.rel, (perDocCitationCounts.get(doc.rel) ?? 0) + 1);
    const missing = citation.expanded.filter(id => !existsOutsideHandDocs(id));
    if (missing.length)
      problems.push(`${citation.doc}:${citation.line}: ${citation.kind} by \`${citation.raw}\` does not resolve outside scanned hand docs (missing: ${missing.map(s => `\`${s}\``).join(", ")})`);
  }
  for (const citation of extracted.titles) {
    if (!titles.has(citation.title))
      problems.push(`${citation.doc}:${citation.line}: cited by "${citation.title}" does not match an ATX heading title in tracked markdown`);
  }
  problems.push(...md022Problems(doc.rel, doc.text));
}

for (const f of allFiles) problems.push(...positionalCitationProblems(f));

if (handDocFiles.length < 4) problems.push(`only ${handDocFiles.length} shipped hand doc(s) scanned (expected >= 4) — doc scope looks broken`);
if (identifierCitationCount < 10)
  problems.push(`only ${identifierCitationCount} backticked citation token(s) found (expected >= 10) — citation extractor looks vacuous`);

if (problems.length) {
  console.log(`doc-citation lint: ${problems.length} problem(s)`);
  for (const p of problems) console.log(`  FAIL ${p}`);
  process.exit(1);
}

console.log(
  `doc-citation lint OK — ${identifierCitationCount} citation token(s) across ${handDocFiles.length} hand doc(s) ` +
    `(${handDocs.map(rel => `${rel}=${perDocCitationCounts.get(rel) ?? 0}`).join(", ")}) · ` +
    `positional citation ban scanned ${allFiles.filter(f => !f.rel.startsWith("draft/")).length} tracked text file(s) · MD022 headings clean`,
);
