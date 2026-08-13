/**
 * Repository-fact scanning over tracked Markdown and text: durable `roadmap:<id>` citations, the
 * generated stable-ID anchors, and durable headings.  The scanners take revision-injected bytes and
 * return facts plus issues; they perform no I/O of their own, which keeps `io.ts` the sole effect
 * holder.
 */
import type {
  FileHeadingFact,
  GateFact,
  RoadmapCitationFact,
} from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import { sortRoadmapIssues as sortIssues } from "./errors.ts";
import { validateRoadmapId } from "./ids.ts";
import { codePointSort } from "./kernel.ts";
import type { RepoPath, RoadmapId } from "./model/core.ts";

const textDecoder = new TextDecoder("utf-8", { fatal: true });
const textEncoder = new TextEncoder();

export interface TrackedTextInput {
  readonly source: RepoPath;
  /** Undefined represents a tracked regular path whose revision-scoped bytes are missing. */
  readonly bytes?: Uint8Array;
}

export interface RepositoryFactResult<T> {
  readonly facts: readonly T[];
  readonly issues: readonly RoadmapIssue[];
}

/** Narrow range-read surface implemented by the immutable expected/legacy Markdown views. */
export interface RoadmapMarkdownByteView {
  readonly byte_length: number;
  sliceBytes(start: number, end: number): Uint8Array;
}

export interface RoadmapMarkdownRepositoryFacts {
  readonly citations: readonly RoadmapCitationFact[];
  readonly headings: readonly FileHeadingFact[];
  readonly stable_anchor_ids: readonly RoadmapId[];
  readonly issues: readonly RoadmapIssue[];
}

/** Repository-fact issue factory: every scan diagnostic is an exit-1 validation issue. */
export function factIssue(
  code: RoadmapIssue["code"],
  source: string,
  logicalPath: string,
  message: string,
  span?: { start_byte: number; end_byte: number },
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, span, exit: 1 };
}

const issue = factIssue;

export function decodeTrackedText(input: TrackedTextInput, issues: RoadmapIssue[]): string | undefined {
  if (input.source === "draft" || input.source.startsWith("draft/")) return undefined;
  if (input.bytes === undefined) {
    issues.push(issue("E-SOURCE-MISSING", input.source, "tracked-text", "tracked regular file is missing from the selected revision"));
    return undefined;
  }
  if (input.bytes.includes(0)) return undefined;
  try {
    const text = textDecoder.decode(input.bytes);
    if (text.includes("\r")) {
      issues.push(issue("E-SOURCE-LINE-END", input.source, "tracked-text", "tracked text must use LF line endings"));
      return undefined;
    }
    return text;
  } catch {
    issues.push(issue("E-SOURCE-UTF8", input.source, "tracked-text", "tracked text must be strict UTF-8"));
    return undefined;
  }
}

function trackedCitationBytes(
  input: TrackedTextInput,
  issues: RoadmapIssue[],
): Uint8Array | undefined {
  if (input.source === "draft" || input.source.startsWith("draft/")) return undefined;
  if (input.bytes === undefined) {
    issues.push(issue("E-SOURCE-MISSING", input.source, "tracked-text", "tracked regular file is missing from the selected revision"));
    return undefined;
  }
  return input.bytes.includes(0) ? undefined : input.bytes;
}

/** Scan revision-injected tracked bytes for canonical durable roadmap citations. */
export function scanRoadmapCitations(
  inputs: readonly TrackedTextInput[],
): RepositoryFactResult<RoadmapCitationFact> {
  const facts: RoadmapCitationFact[] = [];
  const issues: RoadmapIssue[] = [];
  const sorted = [...inputs].sort((left, right) => codePointSort(left.source, right.source));
  for (const input of sorted) {
    const sourceBytes = trackedCitationBytes(input, issues);
    if (sourceBytes === undefined) continue;
    const readByte = (offset: number): number => sourceBytes[offset]!;
    let cursor = 0;
    while ((cursor = indexOfBytes(sourceBytes, ROADMAP_CITATION_PREFIX, cursor)) >= 0) {
      const valueStart = cursor + ROADMAP_CITATION_PREFIX.byteLength;
      const token = tokenizeRoadmapCitation(sourceBytes.byteLength, readByte, valueStart);
      if (token === undefined) {
        cursor = valueStart;
        continue;
      }
      const raw = textDecoder.decode(sourceBytes.slice(cursor, token.end_byte));
      const value = raw.slice("roadmap:".length);
      const validation = validateRoadmapId(value);
      const span = { start_byte: cursor, end_byte: token.end_byte };
      if (!validation.ok) {
        issues.push(issue(validation.code, input.source, "roadmap-citation", validation.message, span));
      } else {
        facts.push({ id: validation.id, source: input.source, span, raw });
      }
      cursor = token.end_byte;
    }
  }
  facts.sort((left, right) =>
    codePointSort(left.source, right.source) || left.span.start_byte - right.span.start_byte ||
    left.span.end_byte - right.span.end_byte || codePointSort(left.id, right.id)
  );
  return { facts: Object.freeze(facts), issues: sortIssues(issues) };
}

const ROADMAP_CITATION_PREFIX = textEncoder.encode("roadmap:");
const BYTE_WINDOW = 64 * 1024;

function indexOfBytes(haystack: Uint8Array, needle: Uint8Array, from: number): number {
  outer: for (let cursor = from; cursor + needle.byteLength <= haystack.byteLength; cursor += 1) {
    for (let index = 0; index < needle.byteLength; index += 1) {
      if (haystack[cursor + index] !== needle[index]) continue outer;
    }
    return cursor;
  }
  return -1;
}

function byteReader(view: RoadmapMarkdownByteView): (offset: number) => number {
  let start = -1;
  let bytes: Uint8Array = new Uint8Array();
  return (offset: number): number => {
    if (offset < 0 || offset >= view.byte_length) throw new RangeError(`byte offset ${offset} is outside the Markdown view`);
    if (offset < start || offset >= start + bytes.byteLength) {
      start = Math.floor(offset / BYTE_WINDOW) * BYTE_WINDOW;
      bytes = view.sliceBytes(start, Math.min(view.byte_length, start + BYTE_WINDOW));
    }
    return bytes[offset - start]!;
  };
}

function asciiAlphanumeric(byte: number): boolean {
  return (byte >= 0x30 && byte <= 0x39) || (byte >= 0x41 && byte <= 0x5a) ||
    (byte >= 0x61 && byte <= 0x7a);
}

function asciiLowercase(byte: number): boolean {
  return byte >= 0x61 && byte <= 0x7a;
}

function identifierLookingByte(byte: number): boolean {
  return asciiAlphanumeric(byte) || byte === 0x2e || byte === 0x5f || byte === 0x2d;
}

interface RoadmapCitationToken {
  readonly end_byte: number;
}

/**
 * Tokenize the suffix after `roadmap:` once for both tracked-file and immutable-view scanners.
 * Separators belong to the token only when a lowercase component can follow. A separator followed
 * by another identifier-looking starter is retained as a malformed candidate so validation fails
 * at the complete candidate span; terminal sentence punctuation remains outside a valid fact.
 */
function tokenizeRoadmapCitation(
  byteLength: number,
  readByte: (offset: number) => number,
  valueStart: number,
): RoadmapCitationToken | undefined {
  if (valueStart >= byteLength || !asciiAlphanumeric(readByte(valueStart))) return undefined;
  let end = valueStart;
  while (end < byteLength) {
    const byte = readByte(end);
    if (asciiAlphanumeric(byte) || byte === 0x5f) {
      end += 1;
      continue;
    }
    if (byte !== 0x2e && byte !== 0x2d) break;
    const next = end + 1 < byteLength ? readByte(end + 1) : undefined;
    if (next !== undefined && asciiLowercase(next)) {
      end += 1;
      continue;
    }
    if (next !== undefined && (asciiAlphanumeric(next) || next === 0x5f)) {
      // Preserve an identifier-looking invalid component in the diagnostic span. This branch is
      // never a valid tokenization: the exact RoadmapId validator below rejects the retained text.
      end += 2;
      while (end < byteLength && identifierLookingByte(readByte(end))) end += 1;
    }
    break;
  }
  return { end_byte: end };
}

function validateMarkdownViewText(
  source: RepoPath,
  view: RoadmapMarkdownByteView,
  readByte: (offset: number) => number,
  issues: RoadmapIssue[],
): boolean {
  const decoder = new TextDecoder("utf-8", { fatal: true });
  try {
    for (let start = 0; start < view.byte_length; start += BYTE_WINDOW) {
      const end = Math.min(view.byte_length, start + BYTE_WINDOW);
      decoder.decode(view.sliceBytes(start, end), { stream: end < view.byte_length });
    }
    decoder.decode();
  } catch {
    issues.push(issue("E-SOURCE-UTF8", source, "tracked-text", "tracked text must be strict UTF-8"));
    return false;
  }
  for (let offset = 0; offset < view.byte_length; offset += 1) {
    if (readByte(offset) === 0x0d) {
      issues.push(issue("E-SOURCE-LINE-END", source, "tracked-text", "tracked text must use LF line endings"));
      return false;
    }
  }
  return true;
}

/**
 * Scan one immutable roadmap Markdown view without materializing its conceptual whole-file bytes.
 * This is the projection-path counterpart of scanRoadmapCitations. Its provenance is the expected
 * projection bytes the decoded source just built, never the committed projection.
 */
export function scanRoadmapMarkdownFacts(
  source: RepoPath,
  view: RoadmapMarkdownByteView,
): RoadmapMarkdownRepositoryFacts {
  const citations: RoadmapCitationFact[] = [];
  const headings: FileHeadingFact[] = [];
  const stableAnchorIds: RoadmapId[] = [];
  const issues: RoadmapIssue[] = [];
  const readByte = byteReader(view);
  if (!validateMarkdownViewText(source, view, readByte, issues)) {
    return { citations: Object.freeze([]), headings: Object.freeze([]),
      stable_anchor_ids: Object.freeze([]), issues: sortIssues(issues) };
  }

  for (let cursor = 0; cursor + ROADMAP_CITATION_PREFIX.byteLength <= view.byte_length;) {
    let prefixMatches = true;
    for (let index = 0; index < ROADMAP_CITATION_PREFIX.byteLength; index += 1) {
      if (readByte(cursor + index) !== ROADMAP_CITATION_PREFIX[index]) {
        prefixMatches = false;
        break;
      }
    }
    if (!prefixMatches) {
      cursor += 1;
      continue;
    }
    const valueStart = cursor + ROADMAP_CITATION_PREFIX.byteLength;
    const token = tokenizeRoadmapCitation(view.byte_length, readByte, valueStart);
    if (token === undefined) {
      cursor = valueStart;
      continue;
    }
    const raw = textDecoder.decode(view.sliceBytes(cursor, token.end_byte));
    const value = raw.slice("roadmap:".length);
    const validation = validateRoadmapId(value);
    const span = { start_byte: cursor, end_byte: token.end_byte };
    if (!validation.ok) {
      issues.push(issue(validation.code, source, "roadmap-citation", validation.message, span));
    } else {
      citations.push({ id: validation.id, source, span, raw });
    }
    cursor = token.end_byte;
  }

  for (let lineStart = 0; lineStart < view.byte_length;) {
    let lineEnd = lineStart;
    while (lineEnd < view.byte_length && readByte(lineEnd) !== 0x0a) lineEnd += 1;
    let hashes = 0;
    while (hashes < 6 && lineStart + hashes < lineEnd && readByte(lineStart + hashes) === 0x23) hashes += 1;
    const line = textDecoder.decode(view.sliceBytes(lineStart, lineEnd));
    if (hashes > 0 && lineStart + hashes < lineEnd && readByte(lineStart + hashes) === 0x20) {
      const match = /^#{1,6} +(.+?)(?: +#*)?$/u.exec(line);
      if (match !== null) {
        const heading = match[1]!;
        const characterStart = line.indexOf(heading);
        const startByte = lineStart + textEncoder.encode(line.slice(0, characterStart)).byteLength;
        headings.push({
          path: source,
          heading,
          span: { start_byte: startByte, end_byte: startByte + textEncoder.encode(heading).byteLength },
        });
      }
    }
    const anchor = /^\s*<a id="roadmap-id-([^"]+)"><\/a>\s*$/u.exec(line);
    if (anchor !== null) {
      const validation = validateRoadmapId(anchor[1]!);
      if (!validation.ok) issues.push(issue(validation.code, source, "roadmap-anchor", validation.message,
        { start_byte: lineStart, end_byte: lineEnd }));
      else stableAnchorIds.push(validation.id);
    }
    lineStart = lineEnd + 1;
  }

  citations.sort((left, right) =>
    codePointSort(left.source, right.source) || left.span.start_byte - right.span.start_byte ||
    left.span.end_byte - right.span.end_byte || codePointSort(left.id, right.id)
  );
  headings.sort((left, right) =>
    codePointSort(left.path, right.path) || left.span.start_byte - right.span.start_byte ||
    codePointSort(left.heading, right.heading)
  );
  stableAnchorIds.sort(codePointSort);
  for (let index = 1; index < stableAnchorIds.length; index++) {
    if (stableAnchorIds[index] === stableAnchorIds[index - 1]) issues.push(issue(
      "E-ID-DUPLICATE", source, "roadmap-anchor",
      `stable roadmap anchor ${JSON.stringify(stableAnchorIds[index])} occurs more than once`,
    ));
  }
  return {
    citations: Object.freeze(citations),
    headings: Object.freeze(headings),
    stable_anchor_ids: Object.freeze(stableAnchorIds),
    issues: sortIssues(issues),
  };
}


export function gateFact(id: string, stub = false): GateFact {
  return { id, kind: "cmd", stub };
}

export function headingFact(path: RepoPath, heading: string): FileHeadingFact {
  return { path, heading, span: { start_byte: 0, end_byte: textEncoder.encode(heading).byteLength } };
}
