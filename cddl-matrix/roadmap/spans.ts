import type { RoadmapIssue } from "./errors.ts";
import type { SpanId } from "./model/core.ts";
import type {
  ManifestEntry,
  RoadmapDocument,
  SourceReplacement,
  SourceSpan,
} from "./model/documents.ts";
import {
  ExpectedByteViewError,
  type CompletedRenderIr,
  type RenderChunk,
} from "./render_ir.ts";

export interface SpanValidationInput {
  readonly document: RoadmapDocument;
  readonly completed: CompletedRenderIr;
  readonly source_snapshot: Uint8Array;
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

function issue(
  document: RoadmapDocument,
  code: Extract<RoadmapIssue["code"], `E-SPAN-${string}`> | "E-SOURCE-DIGEST",
  logical_path: string,
  message: string,
  span?: { start_byte: number; end_byte: number },
): RoadmapIssue {
  return {
    code,
    source: document.document.source_path,
    logical_path,
    ...(span === undefined ? {} : { span }),
    message,
    exit: 1,
  };
}

function sha256(bytes: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(bytes).digest("hex");
}

function utf8Boundary(bytes: Uint8Array, offset: number): boolean {
  return offset === 0 || offset === bytes.byteLength || (bytes[offset] & 0xc0) !== 0x80;
}

function ownerKey(kind: ManifestEntry["kind"], id: string): string {
  return JSON.stringify([kind, id]);
}

function replacementMap(document: RoadmapDocument): ReadonlyMap<string, SourceReplacement> {
  const result = new Map<string, SourceReplacement>();
  if (document.document.schema_version === 0) return result;
  const add = (kind: ManifestEntry["kind"], id: string, value: object): void => {
    if (!("source_replacements" in value) || !Array.isArray(value.source_replacements)) return;
    for (const replacement of value.source_replacements as SourceReplacement[]) {
      result.set(JSON.stringify([kind, id, replacement.span_id]), replacement);
    }
  };
  for (const value of document.sections) add("section", value.section_id, value);
  for (const value of document.fragments) add("fragment", value.fragment_id, value);
  for (const value of document.legacy_markers) add("legacy_marker", value.marker_id, value);
  for (const value of document.records) add("record", value.id, value);
  for (const value of document.parts) add("part", value.part_id, value);
  return result;
}

function expectedStatus(chunk: RenderChunk): SourceSpan["migration_status"] {
  if (chunk.owner.kind === "generated_slot") return "generated";
  return chunk.owner.field === "source_block_md" ? "raw" : "replaced";
}

function chunkSpanOwners(chunks: readonly RenderChunk[]): Map<SpanId, RenderChunk[]> {
  const result = new Map<SpanId, RenderChunk[]>();
  for (const chunk of chunks) {
    for (const spanId of chunk.source_span_ids) {
      const owners = result.get(spanId) ?? [];
      owners.push(chunk);
      result.set(spanId, owners);
    }
  }
  return result;
}

/**
 * Validate complete half-open source accounting against the virtual expected-byte view.  The input
 * snapshot is cloned exactly once so later caller mutation cannot affect any digest or comparison.
 */
export function validateSourceSpans(input: SpanValidationInput): readonly RoadmapIssue[] {
  const { document, completed } = input;
  const snapshot = new Uint8Array(input.source_snapshot);
  const issues: RoadmapIssue[] = [];
  const spans = [...document.spans].sort((left, right) =>
    left.start_byte - right.start_byte || codePointSort(left.id, right.id)
  );
  if (spans.length === 0) {
    issues.push(issue(document, "E-SPAN-EMPTY", "source_span", "source span ledger is empty"));
    return Object.freeze(issues);
  }

  if (snapshot.byteLength !== document.document.frozen_source_byte_length) {
    issues.push(issue(
      document,
      "E-SPAN-COVERAGE",
      "document.frozen_source_byte_length",
      `snapshot has ${snapshot.byteLength} bytes, expected ${document.document.frozen_source_byte_length}`,
    ));
  }
  const snapshotDigest = sha256(snapshot);
  if (snapshotDigest !== document.document.frozen_source_sha256) {
    issues.push(issue(
      document,
      "E-SOURCE-DIGEST",
      "document.frozen_source_sha256",
      `snapshot digest ${snapshotDigest} does not match ${document.document.frozen_source_sha256}`,
    ));
  }

  const ownersBySpan = chunkSpanOwners(completed.chunks);
  const replacements = replacementMap(document);
  const declaredSpanIds = new Set(document.spans.map((span) => span.id));
  for (const [spanId, owners] of ownersBySpan) {
    if (!declaredSpanIds.has(spanId)) {
      issues.push(issue(
        document,
        "E-SPAN-OWNER",
        `render.source_span_ids[${JSON.stringify(spanId)}]`,
        `chunk refers to undeclared span from ${owners.length} owner(s)`,
      ));
    }
  }

  let previousEnd = 0;
  const seenSpanIds = new Set<SpanId>();
  for (const [sortedIndex, span] of spans.entries()) {
    const logicalPath = `source_span[${JSON.stringify(span.id)}]`;
    const coordinate = { start_byte: span.start_byte, end_byte: span.end_byte };
    if (seenSpanIds.has(span.id)) {
      issues.push(issue(document, "E-SPAN-OWNER", logicalPath, "span ID is duplicated", coordinate));
    }
    seenSpanIds.add(span.id);
    if (
      !Number.isSafeInteger(span.start_byte) || !Number.isSafeInteger(span.end_byte) ||
      span.start_byte < 0 || span.end_byte < 0 || span.start_byte >= span.end_byte ||
      span.end_byte > snapshot.byteLength || span.end_byte > completed.expected_bytes.byte_length
    ) {
      issues.push(issue(
        document,
        "E-SPAN-BOUNDS",
        logicalPath,
        `invalid half-open span [${span.start_byte}, ${span.end_byte})`,
        coordinate,
      ));
      continue;
    }
    if (sortedIndex === 0 && span.start_byte !== 0) {
      issues.push(issue(
        document,
        "E-SPAN-GAP",
        logicalPath,
        `first span starts at ${span.start_byte}, expected 0`,
        coordinate,
      ));
    } else if (span.start_byte > previousEnd) {
      issues.push(issue(
        document,
        "E-SPAN-GAP",
        logicalPath,
        `gap [${previousEnd}, ${span.start_byte}) is unowned`,
        coordinate,
      ));
    } else if (span.start_byte < previousEnd) {
      issues.push(issue(
        document,
        "E-SPAN-OVERLAP",
        logicalPath,
        `span starts at ${span.start_byte} before prior end ${previousEnd}`,
        coordinate,
      ));
    }
    previousEnd = Math.max(previousEnd, span.end_byte);

    if (!utf8Boundary(snapshot, span.start_byte) || !utf8Boundary(snapshot, span.end_byte)) {
      issues.push(issue(
        document,
        "E-SPAN-UTF8-BOUNDARY",
        logicalPath,
        "span boundary splits a UTF-8 scalar in the source snapshot",
        coordinate,
      ));
      continue;
    }

    let expectedSlice;
    try {
      expectedSlice = completed.expected_bytes.slice(span.start_byte, span.end_byte);
    } catch (error) {
      issues.push(issue(
        document,
        error instanceof ExpectedByteViewError && error.reason === "utf8-boundary"
          ? "E-SPAN-UTF8-BOUNDARY"
          : "E-SPAN-BOUNDS",
        logicalPath,
        error instanceof Error ? error.message : String(error),
        coordinate,
      ));
      continue;
    }
    const sourceBytes = snapshot.subarray(span.start_byte, span.end_byte);
    if (!completed.expected_bytes.equals(expectedSlice, sourceBytes)) {
      issues.push(issue(
        document,
        "E-SPAN-COVERAGE",
        logicalPath,
        "source bytes differ from the completed expected chunks",
        coordinate,
      ));
    }
    const expectedDigest = completed.expected_bytes.sha256(expectedSlice);
    if (span.sha256 !== expectedDigest || span.sha256 !== sha256(sourceBytes)) {
      issues.push(issue(
        document,
        "E-SPAN-DIGEST",
        logicalPath,
        `span digest ${span.sha256} does not match expected/source digest ${expectedDigest}`,
        coordinate,
      ));
    }

    const owners = ownersBySpan.get(span.id) ?? [];
    if (owners.length !== 1) {
      issues.push(issue(
        document,
        "E-SPAN-OWNER",
        logicalPath,
        `span has ${owners.length} render owners, expected exactly one`,
        coordinate,
      ));
      continue;
    }
    const chunk = owners[0];
    if (chunk.owner.kind !== span.source_kind) {
      issues.push(issue(
        document,
        "E-SPAN-KIND",
        logicalPath,
        `span kind ${span.source_kind} does not match render owner kind ${chunk.owner.kind}`,
        coordinate,
      ));
    }
    if (chunk.owner.id !== span.owner_id) {
      issues.push(issue(
        document,
        "E-SPAN-OWNER",
        logicalPath,
        `span owner ${JSON.stringify(span.owner_id)} does not match ${JSON.stringify(chunk.owner.id)}`,
        coordinate,
      ));
    }
    const status = expectedStatus(chunk);
    if (span.migration_status !== status) {
      issues.push(issue(
        document,
        "E-SPAN-STATUS",
        logicalPath,
        `span status ${span.migration_status} does not match render authority ${status}`,
        coordinate,
      ));
    }
    if (status === "raw" && span.owner_field !== chunk.owner.field) {
      issues.push(issue(
        document,
        "E-SPAN-OWNER",
        logicalPath,
        `raw span field ${span.owner_field} does not match ${chunk.owner.field}`,
        coordinate,
      ));
    } else if (status === "generated" && span.owner_field !== "generated") {
      issues.push(issue(
        document,
        "E-SPAN-OWNER",
        logicalPath,
        "generated span owner_field must be generated",
        coordinate,
      ));
    } else if (status === "replaced") {
      const replacement = replacements.get(JSON.stringify([
        chunk.owner.kind,
        chunk.owner.id,
        span.id,
      ]));
      if (
        replacement === undefined || replacement.replacement_field !== span.owner_field ||
        !chunk.consumed_fields.includes(span.owner_field)
      ) {
        issues.push(issue(
          document,
          "E-SPAN-OWNER",
          logicalPath,
          `replaced span field ${JSON.stringify(span.owner_field)} has no exact consumed replacement`,
          coordinate,
        ));
      }
    }
  }

  const expectedEnd = document.document.frozen_source_byte_length;
  if (previousEnd !== expectedEnd || previousEnd !== completed.expected_bytes.byte_length) {
    issues.push(issue(
      document,
      "E-SPAN-COVERAGE",
      "source_span",
      `span coverage ends at ${previousEnd}; frozen/expected ends are ${expectedEnd}/${completed.expected_bytes.byte_length}`,
    ));
  }
  return Object.freeze(issues);
}
