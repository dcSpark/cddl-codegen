#!/usr/bin/env bun
/**
 * Documentation citation lint — PURE FILE READS, no cargo, no oracles.
 *
 * Four small checks keep gap-tracking prose maintainable:
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
 *   4. No numbered headings in the hand docs (`## 1. Foo`): a numbered heading invites `§ <N>`
 *      citations, which silently retarget when sections are pruned/renumbered — the rot never
 *      dangles, so no existence check can flag it (the "§ 4 lesson" descs pointed at a section
 *      that had been renumbered away). Titles are the stable citation form.
 *   5. Ephemeral-reference ban: durable docs/code/tests must not point at gitignored, plan-internal
 *      material (delivery ruling ids in either the `ruling R<n>` or the `<letter>-R<n>` phase form,
 *      bare work-packet ids, the `<letter>-spec` work-packet-backing spec files, per-packet
 *      "outcome header" prose, `PROBE-<letter><n>`
 *      probe ids, the `draft/loose-cbor/` scratchpad home). Such references resolve
 *      only inside gitignored `draft/` files, so they dangle silently to any future reader — state
 *      the constraint inline with a durable citation instead. See EPHEMERAL_PATTERNS.
 *   6. Roadmap fragment integrity: generated roadmap stable-ID anchors have canonical syntax,
 *      occur exactly once, and every durable stable-ID fragment resolves to one anchor.
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
interface ReadTextState { missingTrackedRels: Set<string> }

function gitLsFiles(): string[] {
  const r = Bun.spawnSync(["git", "ls-files", "-z"], { cwd: ROOT, stdout: "pipe", stderr: "inherit" });
  if ((r.exitCode ?? 1) !== 0) {
    console.error(`doc-citation lint: git ls-files failed with exit ${r.exitCode ?? 1}`);
    process.exit(2);
  }
  return (r.stdout?.toString("utf8") ?? "").split("\0").filter(Boolean).sort();
}

function newReadTextState(): ReadTextState {
  return { missingTrackedRels: new Set<string>() };
}

function isEnoent(error: unknown): boolean {
  return typeof error === "object" && error !== null && "code" in error &&
    (error as { code?: unknown }).code === "ENOENT";
}

function readText(rel: string, state: ReadTextState): string | null {
  let buf: Buffer;
  try {
    buf = readFileSync(`${ROOT}/${rel}`);
  } catch (error) {
    if (!isEnoent(error)) throw error;
    state.missingTrackedRels.add(rel);
    return null;
  }
  if (buf.includes(0)) return null;
  return buf.toString("utf8");
}

function readTrackedTextFiles(rels: readonly string[], state: ReadTextState): TrackedFile[] {
  const files: TrackedFile[] = [];
  for (const rel of rels) {
    const text = readText(rel, state);
    if (text !== null) files.push({ rel, text });
  }
  return files;
}

function appendMissingTrackedFileProblems(problems: string[], state: ReadTextState): void {
  problems.push(...[...state.missingTrackedRels].sort().map(rel =>
    `${rel}: tracked by git but absent from the working tree; restore the file, or stage the deletion`,
  ));
}

// This exercises the same read and diagnostic seam as production without modifying the checkout.
// The canary is read only after a successful initial scan, mirroring a hand doc disappearing before
// its second read; it is repeated to prove the path remains deduplicated.
function missingTrackedFileSelfTestProblem(): string | null {
  const rel = "cddl-matrix/.lint_doc_citations_missing_file_selftest_canary_DO_NOT_CREATE";
  const state = newReadTextState();
  const initialFiles = readTrackedTextFiles(["cddl-matrix/lint_doc_citations.ts"], state);
  const lateRead = readText(rel, state);
  const repeatedLateRead = readText(rel, state);
  const expected = `${rel}: tracked by git but absent from the working tree; restore the file, or stage the deletion`;
  const diagnostics: string[] = [];
  appendMissingTrackedFileProblems(diagnostics, state);
  if (initialFiles.length !== 1 || lateRead !== null || repeatedLateRead !== null)
    return "missing-file self-test: late nonexistent synthetic tracked entry did not read as null";
  if (state.missingTrackedRels.size !== 1 || !state.missingTrackedRels.has(rel))
    return "missing-file self-test: repeated missing read did not produce exactly one tracked path";
  if (diagnostics.length !== 1 || diagnostics[0] !== expected)
    return "missing-file self-test: missing tracked path did not produce its exact one-path diagnostic";
  return null;
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
    if (/^\s{0,3}#{1,6}\s+\d+[.)]\s/.test(line))
      problems.push(`${rel}:${i + 1}: numbered heading '${line.trim()}' invites positional § <N> citations that silently retarget on renumbering; use an unnumbered descriptive title`);
    if (i === 0) continue;
    if (lines[i - 1]!.trim() !== "") problems.push(`${rel}:${i + 1}: heading must be preceded by a blank line`);
  }
  return problems;
}

// Ephemeral-reference ban: durable docs/code/tests must not point at gitignored, plan-internal
// material — delivery ruling ids (`ruling R<n>` and the `<letter>-R<n>` phase form), bare
// work-packet ids and their backing `<letter>-spec` files, per-packet "outcome header" prose,
// `PROBE-<letter><n>` probe
// ids, and the `draft/loose-cbor/` scratchpad home. Those resolve only
// inside gitignored `draft/` files, so to a future reader they dangle silently (no existence check
// can flag a reference whose target is not tracked). The fix is to state the constraint inline with
// a durable citation (a docs section by heading, an RFC section, or a test/gate name). Scoped like
// the positional ban: `draft/` files are historical working notes and exempt as TARGETS. The
// broader `draft/*.md` upstream-bug repro provenance the matrix hand-docs cite is a separate,
// pre-existing convention (a different reference class) and is deliberately NOT matched here.
// Phase-GENERIC by construction: the delivery-plan id spellings roll one letter forward per phase
// (`PROBE-B<n>` -> `PROBE-C<n>`, `ruling R<n>` -> the `<letter>-R<n>` ruling-id form), so the patterns
// match every phase's letter rather than the one the current delivery happens to use. A phase-specific
// spelling (only the launching phase's letter) is exactly how one phase's ids slipped through while
// another phase's tree stayed clean by luck. The bare work-packet id is number-generic for the same
// reason: it matches every packet number and the optional per-roadmap suffix letter (`WP4M`,
// `WP5C`), because a packet id is meaningless without the gitignored plan that numbered it, and the
// numbering restarts each campaign. Its own canary is why this file exempts itself below.
const EPHEMERAL_PATTERNS: { re: RegExp; canary: string }[] = [
  { re: /draft\/loose-cbor/g, canary: "see draft/loose-cbor/b-spec.md" },
  { re: /\b[a-z][0-9]*-spec\b/g, canary: "per the b-spec" },
  { re: /ruling R[0-9]/g, canary: "ruling R7 says" },
  { re: /\b[A-Z]-R[0-9]/g, canary: "C-R8 requires" },
  { re: /ruling §/g, canary: "ruling §10.8" },
  { re: /outcome header/gi, canary: "the WP4 outcome header" },
  { re: /PROBE-[A-Z][0-9]/g, canary: "PROBE-C1 confirmed" },
  { re: /\bWP[0-9]+[A-Z]?\b/g, canary: "delivered by WP4M" },
];

function ephemeralReferenceProblems(file: TrackedFile): string[] {
  if (file.rel.startsWith("draft/")) return [];
  // The ruleset file necessarily spells the banned patterns; exempt it from its own scan.
  if (file.rel === "cddl-matrix/lint_doc_citations.ts") return [];
  const problems: string[] = [];
  for (const { re } of EPHEMERAL_PATTERNS) {
    for (const m of file.text.matchAll(re)) {
      problems.push(`${file.rel}:${lineOf(file.text, m.index ?? 0)}: ephemeral reference '${m[0]}' points at gitignored/plan-internal material that dangles to future readers; state the constraint inline with a durable citation (a docs section by heading, an RFC section, or a test/gate name)`);
    }
  }
  return problems;
}

function positionalCitationProblems(file: TrackedFile): string[] {
  if (file.rel.startsWith("draft/")) return [];
  const problems: string[] = [];
  const patterns = [
    // Roadmap-adjacent citations are always positional, including `TESTING_ROADMAP.md item <N>`.
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

// Capture the whole fragment token, not only a valid-looking prefix: `matrix.foo!bad` must be
// rejected as malformed rather than silently resolving through an existing `matrix.foo` anchor.
const ROADMAP_FRAGMENT_PREFIX = "#roadmap-" + "id-";
const ROADMAP_FRAGMENT_RE = new RegExp(`${ROADMAP_FRAGMENT_PREFIX}([^\\s)\\]}>"'\\x60]+)`, "g");
const ROADMAP_ANCHOR_RE = /^\s*<a id="roadmap-id-((?:matrix|testing)\.[a-z0-9.-]+)"><\/a>\s*$/gm;
const ROADMAP_ID_RE = /^(?:matrix|testing)\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*(?:\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*)*$/;

function roadmapFragmentProblems(files: readonly TrackedFile[]): string[] {
  const problems: string[] = [];
  const anchors = new Map<string, { rel: string; line: number }[]>();
  for (const file of files.filter((candidate) =>
    candidate.rel === "cddl-matrix/ROADMAP.md" || candidate.rel === "tests/TESTING_ROADMAP.md"
  )) {
    for (const match of file.text.matchAll(ROADMAP_ANCHOR_RE)) {
      const id = match[1]!;
      if (!ROADMAP_ID_RE.test(id)) problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: malformed stable roadmap anchor '${id}'`);
      const rows = anchors.get(id) ?? [];
      rows.push({ rel: file.rel, line: lineOf(file.text, match.index ?? 0) });
      anchors.set(id, rows);
    }
    for (const match of file.text.matchAll(/id="roadmap-id-([^"]+)"/g)) {
      if (!ROADMAP_ID_RE.test(match[1]!)) problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: malformed stable roadmap anchor '${match[1]}'`);
    }
  }
  for (const [id, rows] of [...anchors].sort(([left], [right]) => left < right ? -1 : left > right ? 1 : 0)) {
    if (rows.length !== 1) problems.push(`${rows[0]!.rel}:${rows[0]!.line}: stable roadmap anchor '${id}' occurs ${rows.length} times, expected once`);
  }
  for (const file of files.filter((candidate) => !candidate.rel.startsWith("draft/"))) {
    for (const match of file.text.matchAll(ROADMAP_FRAGMENT_RE)) {
      const id = match[1]!;
      if (!ROADMAP_ID_RE.test(id)) {
        problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: malformed roadmap fragment '${ROADMAP_FRAGMENT_PREFIX}${id}'`);
      } else if ((anchors.get(id)?.length ?? 0) !== 1) {
        problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: roadmap fragment '${ROADMAP_FRAGMENT_PREFIX}${id}' does not resolve to exactly one generated anchor`);
      }
    }
  }
  return problems;
}

function roadmapFragmentSelfTestProblems(): string[] {
  const generated = (anchors: string): TrackedFile => ({ rel: "cddl-matrix/ROADMAP.md", text: anchors });
  const durable = (fragment: string): TrackedFile => ({ rel: "tests/citation-canary.md", text: fragment });
  const resolved = roadmapFragmentProblems([
    generated('<a id="roadmap-id-matrix.canary"></a>\n'),
    durable(`[resolved](../cddl-matrix/ROADMAP.md${ROADMAP_FRAGMENT_PREFIX}matrix.canary)`),
  ]);
  const missing = roadmapFragmentProblems([
    generated('<a id="roadmap-id-matrix.canary"></a>\n'),
    durable(`[missing](../cddl-matrix/ROADMAP.md${ROADMAP_FRAGMENT_PREFIX}matrix.absent)`),
  ]);
  const malformed = roadmapFragmentProblems([
    generated('<a id="roadmap-id-matrix.canary"></a>\n'),
    durable(`[malformed](../cddl-matrix/ROADMAP.md${ROADMAP_FRAGMENT_PREFIX}matrix.canary!bad)`),
  ]);
  const duplicate = roadmapFragmentProblems([
    generated('<a id="roadmap-id-matrix.canary"></a>\n<a id="roadmap-id-matrix.canary"></a>\n'),
    durable(`[duplicate](../cddl-matrix/ROADMAP.md${ROADMAP_FRAGMENT_PREFIX}matrix.canary)`),
  ]);
  const failures: string[] = [];
  if (resolved.length !== 0) failures.push("roadmap-fragment self-check: resolved canary failed");
  if (!missing.some((problem) => problem.includes("does not resolve"))) {
    failures.push("roadmap-fragment self-check: missing canary escaped");
  }
  if (!malformed.some((problem) => problem.includes("malformed roadmap fragment") && problem.includes("!bad"))) {
    failures.push("roadmap-fragment self-check: malformed whole-token canary escaped");
  }
  if (!duplicate.some((problem) => problem.includes("occurs 2 times"))) {
    failures.push("roadmap-fragment self-check: duplicate-anchor canary escaped");
  }
  return failures;
}

// Directive-swallows-closer ban (user docs): a CDDL comment runs to the end of the line, so an
// illustration that puts a `; @<directive>` comment on the same line/span as the container's
// closing `}`/`]` models a spelling that silently swallows the closer — a reader who copies it
// gets a different parse than the doc describes. Proven around the tolerate-and-drop directive:
// the trap class was identified and documented in one delivery (the comment_dsl warning bullet +
// fixture pins on the safe spelling), and one delivery later the SAME doc's new section shipped
// an illustration in exactly the banned one-line spelling, two bullets BELOW the warning —
// caught only in review. Scope: docs/docs/*.mdx (where user-facing illustrations live). The
// warning bullet's deliberate counterexamples are allowlisted by exact span text, so editing
// them or adding a NEW unsafe illustration fires loudly.
const DIRECTIVE_SWALLOW_RE = /;\s*@[a-z_]+[^`\n]*[}\]]/;
const DIRECTIVE_SWALLOW_ALLOWED_SPANS = new Set([
  "; @ignore }",
  "; @ignore ]",
  "[ uint, * any ; @ignore ]",
]);
function directiveSwallowedCloserProblems(file: TrackedFile): string[] {
  if (!/^docs\/docs\/[^/]+\.mdx$/.test(file.rel)) return [];
  const problems: string[] = [];
  const lines = file.text.split("\n");
  let inFence = false;
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    if (/^\s*```/.test(line)) {
      inFence = !inFence;
      continue;
    }
    if (inFence) {
      if (DIRECTIVE_SWALLOW_RE.test(line))
        problems.push(
          `${file.rel}:${i + 1}: fenced example puts a \`; @<directive>\` comment on the same line as a closing }/] — the CDDL comment runs to the end of the line and swallows the closer; move the directive to its own line before the closer`,
        );
      continue;
    }
    for (const m of line.matchAll(/`([^`]+)`/g)) {
      const span = m[1]!;
      if (DIRECTIVE_SWALLOW_ALLOWED_SPANS.has(span)) continue;
      if (DIRECTIVE_SWALLOW_RE.test(span))
        problems.push(
          `${file.rel}:${i + 1}: inline example \`${span}\` puts a \`; @<directive>\` comment on the same line as a closing }/] — the CDDL comment runs to the end of the line and swallows the closer; reword or split the example (a deliberate counterexample belongs in DIRECTIVE_SWALLOW_ALLOWED_SPANS)`,
        );
    }
  }
  return problems;
}

const trackedRels = gitLsFiles();
const readTextState = newReadTextState();
const allFiles = readTrackedTextFiles(trackedRels, readTextState);

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
  const text = readText(rel, readTextState);
  return text === null ? [] : [{ rel, text }];
});

// Materialize the read-boundary verdict only after every production call sharing this state.
appendMissingTrackedFileProblems(problems, readTextState);
const missingFileSelfTestProblem = missingTrackedFileSelfTestProblem();
if (missingFileSelfTestProblem !== null) problems.push(missingFileSelfTestProblem);

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
for (const f of allFiles) problems.push(...ephemeralReferenceProblems(f));
for (const f of allFiles) problems.push(...directiveSwallowedCloserProblems(f));
problems.push(...roadmapFragmentProblems(allFiles));
problems.push(...roadmapFragmentSelfTestProblems());

// Self-check: each ephemeral pattern must still match its canary (guards against a regex silently
// going vacuous — e.g. an errant edit that never fires and lets dangling references back in).
for (const { re, canary } of EPHEMERAL_PATTERNS) {
  re.lastIndex = 0;
  if (!re.test(canary)) problems.push(`ephemeral-reference self-check: pattern ${re} no longer matches its canary '${canary}' — the ban is vacuous`);
  re.lastIndex = 0;
}
// Same vacuity guard for the directive-swallow ban: the pattern must match the trap spelling and
// every allowlisted counterexample (an allowlist entry the pattern no longer matches is dead
// weight hiding a drifted regex).
if (!DIRECTIVE_SWALLOW_RE.test("* uint => any ; @ignore }"))
  problems.push("directive-swallow self-check: pattern no longer matches the map-brace trap spelling — the ban is vacuous");
for (const span of DIRECTIVE_SWALLOW_ALLOWED_SPANS) {
  if (!DIRECTIVE_SWALLOW_RE.test(span))
    problems.push(`directive-swallow self-check: allowlisted span '${span}' is not matched by the pattern — dead allowlist entry (drifted regex or stale counterexample)`);
}

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
    `positional + ephemeral-reference bans scanned ${allFiles.filter(f => !f.rel.startsWith("draft/")).length} tracked text file(s) · directive-swallow ban over docs/docs/*.mdx · MD022 headings clean`,
);
