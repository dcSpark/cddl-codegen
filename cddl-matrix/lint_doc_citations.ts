#!/usr/bin/env bun
/**
 * Documentation citation lint — PURE FILE READS, no cargo, no oracles.
 *
 * These small checks keep gap-tracking prose maintainable:
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
 *   7. Retired-projection reference ban: the deleted `ROADMAP.md` / `TESTING_ROADMAP.md`
 *      spellings survive only in the explicit pre-cutover compatibility implementation and its
 *      fixtures. Durable prose cites the authoritative TOML source or a stable record ID.
 *   8. Backticked roadmap-ID integrity: a `testing.*` / `matrix.*` token in durable markdown must
 *      still name a section-placed roadmap record. This catches a hand doc whose "remaining work"
 *      prose survives the record's retirement even when it does not use a `tracked by` citation verb.
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
    // Roadmap-adjacent citations are always positional, including a roadmap TOML's `item <N>`.
    /\b[A-Za-z0-9_./-]*roadmap(?:\.md|\.toml)?\s+(?:§\s+[^:\n()]{0,80}\s+)?item\s*#?\d+\b/gi,
    // A roadmap filename followed by an all-numeric section address retargets when sections move.
    // Require the filename extension so ordinary `RFC 8610 § 3.2` prose remains outside this rule.
    /\b[A-Za-z0-9_./-]*roadmap(?:\.md|\.toml)\s+§\s*\d+(?:\.\d+)*\b/gi,
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

// Keep both roadmap position spellings live: a list-item number and a numeric section number.
// Assemble the numeric suffixes so this source remains clean when the production scan reads it.
function positionalCitationSelfTestProblems(): string[] {
  const number = "1" + "4";
  const numericSection = `§ ${number}`;
  const authoritativeRoadmap = "tests/testing-roadmap.toml";
  const legacyRoadmap = "TESTING_" + "ROADMAP.md";
  const canary = (text: string): TrackedFile => ({ rel: "tests/positional-citation-canary.md", text });
  const failures: string[] = [];
  const expectBlocked = (label: string, text: string): void => {
    const problems = positionalCitationProblems(canary(text));
    const expected = `positional citation '${text}' is unstable`;
    if (problems.length !== 1 || !problems[0]!.includes(expected))
      failures.push(`positional-citation self-check: ${label} was not rejected with the stable diagnostic`);
  };
  const expectAllowed = (label: string, text: string): void => {
    if (positionalCitationProblems(canary(text)).length !== 0)
      failures.push(`positional-citation self-check: ${label} was incorrectly rejected`);
  };

  expectBlocked("roadmap item", `${authoritativeRoadmap} item ${number}`);
  expectBlocked("legacy roadmap numeric section", `${legacyRoadmap} ${numericSection}`);
  expectBlocked("authoritative roadmap numeric section", `${authoritativeRoadmap} ${numericSection}`);
  expectBlocked("authoritative roadmap compact numeric section", `${authoritativeRoadmap} §${number}`);
  expectAllowed("roadmap title", `${authoritativeRoadmap} § Standing-system residuals`);
  expectAllowed("RFC numeric section", `RFC 8610 ${numericSection}`);
  return failures;
}

// Capture the whole fragment token, not only a valid-looking prefix: `matrix.foo!bad` must be
// rejected as malformed rather than silently resolving through an existing `matrix.foo` anchor.
// The anchor universe is derived from the TOML SOURCES (section-placed record IDs — exactly the
// set the roadmap pipeline's own anchor validation renders as `roadmap-id-` anchors): the
// projections that used to carry the rendered anchors are gitignored and not scannable here.
const ROADMAP_FRAGMENT_PREFIX = "#roadmap-" + "id-";
const ROADMAP_FRAGMENT_RE = new RegExp(`${ROADMAP_FRAGMENT_PREFIX}([^\\s)\\]}>"'\\x60]+)`, "g");
const ROADMAP_ID_RE = /^(?:matrix|testing)\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*(?:\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*)*$/;

function placedRoadmapRecordIds(files: readonly TrackedFile[], problems: string[]): Set<string> {
  const placed = new Set<string>();
  for (const file of files.filter((candidate) => (ROADMAP_SOURCE_DOCS as readonly string[]).includes(candidate.rel))) {
    let parsed: { section?: readonly { entries?: readonly unknown[] }[]; record?: readonly { id?: unknown }[] };
    try {
      parsed = Bun.TOML.parse(file.text) as typeof parsed;
    } catch {
      problems.push(`${file.rel}: roadmap source failed to parse as TOML; fragment anchors cannot be derived`);
      continue;
    }
    const recordIds = new Set((parsed.record ?? []).flatMap((row) => typeof row.id === "string" ? [row.id] : []));
    for (const section of parsed.section ?? []) {
      for (const entry of section.entries ?? []) {
        if (typeof entry === "string" && recordIds.has(entry)) placed.add(entry);
      }
    }
  }
  return placed;
}

function roadmapFragmentProblems(files: readonly TrackedFile[]): string[] {
  const problems: string[] = [];
  const anchors = placedRoadmapRecordIds(files, problems);
  for (const id of [...anchors].sort()) {
    if (!ROADMAP_ID_RE.test(id)) problems.push(`roadmap sources: malformed stable roadmap anchor id '${id}'`);
  }
  for (const file of files.filter((candidate) => !candidate.rel.startsWith("draft/"))) {
    for (const match of file.text.matchAll(ROADMAP_FRAGMENT_RE)) {
      const id = match[1]!;
      if (!ROADMAP_ID_RE.test(id)) {
        problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: malformed roadmap fragment '${ROADMAP_FRAGMENT_PREFIX}${id}'`);
      } else if (!anchors.has(id)) {
        problems.push(`${file.rel}:${lineOf(file.text, match.index ?? 0)}: roadmap fragment '${ROADMAP_FRAGMENT_PREFIX}${id}' does not resolve to a section-placed roadmap record`);
      }
    }
  }
  return problems;
}

function roadmapFragmentSelfTestProblems(): string[] {
  const source = (body: string): TrackedFile => ({ rel: "cddl-matrix/roadmap.toml", text: body });
  const canarySource = source([
    '[[section]]',
    'section_id = "fixture"',
    'entries = ["matrix.canary"]',
    '',
    '[[record]]',
    'id = "matrix.canary"',
    '',
    '[[record]]',
    'id = "matrix.unplaced"',
    '',
  ].join("\n"));
  const durable = (fragment: string): TrackedFile => ({ rel: "tests/citation-canary.md", text: fragment });
  const resolved = roadmapFragmentProblems([
    canarySource,
    durable(`[resolved](x${ROADMAP_FRAGMENT_PREFIX}matrix.canary)`),
  ]);
  const missing = roadmapFragmentProblems([
    canarySource,
    durable(`[missing](x${ROADMAP_FRAGMENT_PREFIX}matrix.absent)`),
  ]);
  const malformed = roadmapFragmentProblems([
    canarySource,
    durable(`[malformed](x${ROADMAP_FRAGMENT_PREFIX}matrix.canary!bad)`),
  ]);
  const unplaced = roadmapFragmentProblems([
    canarySource,
    durable(`[unplaced](x${ROADMAP_FRAGMENT_PREFIX}matrix.unplaced)`),
  ]);
  const failures: string[] = [];
  if (resolved.length !== 0) failures.push("roadmap-fragment self-check: resolved canary failed");
  if (!missing.some((problem) => problem.includes("does not resolve"))) {
    failures.push("roadmap-fragment self-check: missing canary escaped");
  }
  if (!malformed.some((problem) => problem.includes("malformed roadmap fragment") && problem.includes("!bad"))) {
    failures.push("roadmap-fragment self-check: malformed whole-token canary escaped");
  }
  if (!unplaced.some((problem) => problem.includes("does not resolve"))) {
    failures.push("roadmap-fragment self-check: unplaced-record canary escaped");
  }
  return failures;
}

// A hand doc often names a roadmap record directly rather than introducing it with one of the
// `pinned by` / `tracked by` / `gated by` verbs above. Those exact IDs are still durable references:
// after the work is delivered and its record retired, leaving the backticked token behind is both a
// dangling identifier and usually stale "remaining work" prose. `matrix.json` is the one filename
// in durable markdown that shares the roadmap namespace spelling without being a record ID.
const BACKTICKED_ROADMAP_ID_RE = /`((?:matrix|testing)\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*(?:\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*)*)`/g;
const BACKTICKED_ROADMAP_ID_NON_IDS = new Set(["matrix.json"]);

function backtickedRoadmapIdProblems(files: readonly TrackedFile[]): string[] {
  const problems: string[] = [];
  const placed = placedRoadmapRecordIds(files, problems);
  for (const file of files.filter((candidate) =>
    !candidate.rel.startsWith("draft/") &&
    (candidate.rel.endsWith(".md") || candidate.rel.endsWith(".mdx")))) {
    for (const match of file.text.matchAll(BACKTICKED_ROADMAP_ID_RE)) {
      const id = match[1]!;
      if (BACKTICKED_ROADMAP_ID_NON_IDS.has(id)) continue;
      if (!placed.has(id)) {
        problems.push(
          `${file.rel}:${lineOf(file.text, match.index ?? 0)}: backticked roadmap id '${id}' does not resolve to a section-placed record; remove stale prose or cite the live replacement`,
        );
      }
    }
  }
  return problems;
}

function backtickedRoadmapIdSelfTestProblems(): string[] {
  const source: TrackedFile = {
    rel: "cddl-matrix/roadmap.toml",
    text: [
      '[[section]]',
      'section_id = "fixture"',
      'entries = ["matrix.canary"]',
      '',
      '[[record]]',
      'id = "matrix.canary"',
      '',
      '[[record]]',
      'id = "matrix.unplaced"',
      '',
    ].join("\n"),
  };
  const durable = (token: string): TrackedFile => ({
    rel: "tests/backticked-roadmap-id-canary.md",
    text: `\`${token}\``,
  });
  const resolved = backtickedRoadmapIdProblems([source, durable("matrix.canary")]);
  const missing = backtickedRoadmapIdProblems([source, durable("matrix.absent")]);
  const unplaced = backtickedRoadmapIdProblems([source, durable("matrix.unplaced")]);
  const filename = backtickedRoadmapIdProblems([source, durable("matrix.json")]);
  const failures: string[] = [];
  if (resolved.length !== 0)
    failures.push("backticked-roadmap-id self-check: resolved canary failed");
  if (!missing.some((problem) => problem.includes("does not resolve")))
    failures.push("backticked-roadmap-id self-check: missing canary escaped");
  if (!unplaced.some((problem) => problem.includes("does not resolve")))
    failures.push("backticked-roadmap-id self-check: unplaced canary escaped");
  if (filename.length !== 0)
    failures.push("backticked-roadmap-id self-check: matrix.json filename was treated as a record ID");
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

// The roadmap hand docs are the TOML SOURCES: the markdown projections are gitignored
// human-review renders under draft/roadmaps/ (regenerate with `project_roadmaps.ts --write`) and
// must never be committed — an agent that finds a committed projection will read it as a source
// of truth, which is exactly what the move out of the repository forecloses.
const ROADMAP_SOURCE_DOCS = ["cddl-matrix/roadmap.toml", "tests/testing-roadmap.toml"] as const;
const RETIRED_PROJECTION_PATHS = ["cddl-matrix/ROADMAP.md", "tests/TESTING_ROADMAP.md"] as const;
// Burndown 5 is a deliberately tracked managed list, not a generated roadmap projection or an
// ad-hoc draft. Keep this exact eight-file boundary aligned with its README Files table; its cycle
// specs and probes stay ignored, and any other tracked `draft/` path remains a lint failure.
const MANAGED_BURNDOWN5_DOCS = new Set([
  "draft/burndown5/README.md",
  "draft/burndown5/tier1-silent-wrong-results.md",
  "draft/burndown5/tier3-due-test-systems.md",
  "draft/burndown5/tier4-ready-features.md",
  "draft/burndown5/tier5-decisions-and-filings.md",
  "draft/burndown5/bookkeeping.md",
  "draft/burndown5/parked-validated.md",
  "draft/burndown5/history.md",
]);
// These spellings name deleted projections, not current documents. Keep the compatibility surface
// closed: every other tracked text file must use the TOML authorities or a stable roadmap record ID.
// This excludes paths themselves (git rejects retired projections below); it scans only readable text
// contents, so binary fixtures cannot produce a misleading prose citation.
const RETIRED_PROJECTION_SPELLING_RE = /\b(?:ROADMAP\.md|TESTING_ROADMAP\.md)\b/g;
const RETIRED_PROJECTION_REFERENCE_ALLOWLIST = new Set([
  "cddl-matrix/lint_doc_citations.ts",
  "cddl-matrix/project_status_headers.ts",
  "cddl-matrix/roadmap/adapters/matrix.ts",
  "cddl-matrix/roadmap/fixtures/status-compat/diagnostics.toml",
  "cddl-matrix/roadmap/fixtures/status-compat/modes.toml",
  "cddl-matrix/roadmap/matrix_status_facts.ts",
  "cddl-matrix/roadmap/output_registry.ts",
  "cddl-matrix/roadmap/selftests/identity.ts",
  "cddl-matrix/roadmap/selftests/projection.ts",
  "cddl-matrix/roadmap/selftests/projection_views.ts",
]);
const handDocSet = new Set<string>([
  ...ROADMAP_SOURCE_DOCS,
  "tests/README.md",
  "cddl-matrix/README.md",
]);
const handDocs = [...handDocSet].sort();

const problems: string[] = [];
const missingHandDocs = handDocs.filter(rel => !trackedRels.includes(rel));
for (const rel of missingHandDocs) problems.push(`${rel}: hand doc is not tracked by git`);
for (const rel of trackedRels) {
  if (
    (RETIRED_PROJECTION_PATHS as readonly string[]).includes(rel) ||
    rel === "draft" ||
    (rel.startsWith("draft/") && !MANAGED_BURNDOWN5_DOCS.has(rel))
  ) {
    problems.push(`${rel}: generated roadmap projections and draft/ scratch material must not be tracked by git; the TOML sources are the authority and the renders live under gitignored draft/roadmaps/`);
  }
}

function retiredProjectionSpellingProblems(files: readonly TrackedFile[]): string[] {
  const problems: string[] = [];
  for (const file of files) {
    if (RETIRED_PROJECTION_REFERENCE_ALLOWLIST.has(file.rel)) continue;
    for (const match of file.text.matchAll(RETIRED_PROJECTION_SPELLING_RE)) {
      problems.push(
        `${file.rel}:${lineOf(file.text, match.index ?? 0)}: retired roadmap projection spelling '${match[0]}' is reserved for closed pre-cutover compatibility seams; cite the TOML authority or a stable roadmap record ID instead`,
      );
    }
  }
  return problems;
}

function retiredProjectionAllowlistStalenessProblems(files: readonly TrackedFile[]): string[] {
  const texts = new Map(files.map((file) => [file.rel, file.text]));
  const problems: string[] = [];
  for (const rel of [...RETIRED_PROJECTION_REFERENCE_ALLOWLIST].sort()) {
    const text = texts.get(rel);
    RETIRED_PROJECTION_SPELLING_RE.lastIndex = 0;
    if (text === undefined || !RETIRED_PROJECTION_SPELLING_RE.test(text))
      problems.push(`${rel}: retired-projection compatibility allowlist entry is stale; remove it or retain its explicit compatibility seam`);
  }
  RETIRED_PROJECTION_SPELLING_RE.lastIndex = 0;
  return problems;
}

// Exercise the closed-set boundary without embedding a retired spelling in this scanned source.
function retiredProjectionReferenceSelfTestProblems(): string[] {
  const spelling = "ROAD" + "MAP.md";
  const unknown = retiredProjectionSpellingProblems([{ rel: "tests/retired-projection-canary.md", text: spelling }]);
  const known = retiredProjectionSpellingProblems([{ rel: "cddl-matrix/lint_doc_citations.ts", text: spelling }]);
  const failures: string[] = [];
  if (unknown.length !== 1 || !unknown[0]!.includes("reserved for closed pre-cutover compatibility seams"))
    failures.push("retired-projection self-check: unallowlisted canary was not rejected with the stable diagnostic");
  if (known.length !== 0)
    failures.push("retired-projection self-check: allowlisted compatibility canary was rejected");
  return failures;
}

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
  // A TOML hand doc spells prose inside TOML strings, where a basic string escapes `"` and `\`;
  // decode those two escapes for citation extraction so a token like `(<override>, "int")` reads
  // as its rendered spelling. Newlines are untouched, so reported line numbers stay exact.
  const citationText = doc.rel.endsWith(".toml") ? doc.text.replace(/\\(["\\])/g, "$1") : doc.text;
  const extracted = extractCitations(doc.rel, citationText);
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
  // Markdown layout rules apply to the markdown hand docs only: a TOML source's `#` comment lines
  // are not headings, and the roadmap renderer owns blank-line hygiene for its own projection.
  if (doc.rel.endsWith(".md")) problems.push(...md022Problems(doc.rel, doc.text));
}

for (const f of allFiles) problems.push(...positionalCitationProblems(f));
problems.push(...positionalCitationSelfTestProblems());
problems.push(...retiredProjectionSpellingProblems(allFiles));
problems.push(...retiredProjectionAllowlistStalenessProblems(allFiles));
problems.push(...retiredProjectionReferenceSelfTestProblems());
for (const f of allFiles) problems.push(...ephemeralReferenceProblems(f));
for (const f of allFiles) problems.push(...directiveSwallowedCloserProblems(f));
problems.push(...roadmapFragmentProblems(allFiles));
problems.push(...roadmapFragmentSelfTestProblems());
problems.push(...backtickedRoadmapIdProblems(allFiles));
problems.push(...backtickedRoadmapIdSelfTestProblems());

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
    `positional + ephemeral-reference + retired-projection + roadmap-ID bans scanned ${allFiles.filter(f => !f.rel.startsWith("draft/")).length} tracked text file(s) · directive-swallow ban over docs/docs/*.mdx · MD022 headings clean`,
);
