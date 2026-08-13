import type { FieldConsumer, GeneratedSlotResolution } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RenderOp } from "./manifest.ts";
import type {
  GeneratedSlot,
  ManifestEntry,
  RoadmapDocument,
  SemanticAuthorityRecord,
  SemanticPayload,
} from "./model/documents.ts";
import type { SpanId } from "./model/core.ts";

export interface RenderChunk {
  readonly manifest_index: number;
  readonly owner: {
    readonly kind: ManifestEntry["kind"];
    readonly id: string;
    readonly field: string;
  };
  readonly bytes: Uint8Array;
  readonly source_span_ids: readonly SpanId[];
  readonly consumed_fields: readonly string[];
}

export interface FieldConsumptionLedgerEntry {
  readonly owner_kind: ManifestEntry["kind"];
  readonly owner_id: string;
  readonly expected_fields: readonly string[];
  readonly consumed_fields: readonly string[];
  readonly duplicate_fields: readonly string[];
  readonly unknown_fields: readonly string[];
  readonly mismatched_fields: readonly string[];
}

export interface ProjectedFieldSegment {
  readonly owner_kind: Exclude<ManifestEntry["kind"], "generated_slot">;
  readonly owner_id: string;
  readonly logical_path: string;
  readonly start_in_chunk: number;
  readonly end_in_chunk: number;
  readonly bytes: Uint8Array;
}

export interface CompletedSlotResolution {
  readonly manifest_index: number;
  readonly slot: GeneratedSlot;
  readonly resolution?: GeneratedSlotResolution;
}

export interface RenderIrServices {
  renderSemanticRecord(record: SemanticAuthorityRecord, fields: FieldConsumer): Uint8Array;
  resolveGeneratedSlot(slot: GeneratedSlot): GeneratedSlotResolution | undefined;
}

export interface CompletedRenderIr {
  readonly chunks: readonly RenderChunk[];
  readonly expected_bytes: ExpectedByteView;
  readonly field_consumption: readonly FieldConsumptionLedgerEntry[];
  readonly projected_field_segments: readonly ProjectedFieldSegment[];
  readonly slot_resolutions: readonly CompletedSlotResolution[];
  readonly build_issues: readonly RoadmapIssue[];
}

/** Resolve one exact projected field binding in both chunk-local and whole-document coordinates. */
export function exactProjectedFieldSegment(
  completed: CompletedRenderIr,
  chunk: RenderChunk,
  ownerKind: Exclude<ManifestEntry["kind"], "generated_slot">,
  ownerId: string,
  logicalPath: string,
  startByte: number,
  endByte: number,
): ProjectedFieldSegment | undefined {
  const segments = completed.projected_field_segments.filter((segment) =>
    segment.owner_kind === ownerKind && segment.owner_id === ownerId &&
    segment.logical_path === logicalPath
  );
  const segment = segments[0];
  const chunkIndexes = completed.chunks.flatMap((candidate, index) => candidate === chunk ? [index] : []);
  const chunkIndex = chunkIndexes[0];
  if (segments.length !== 1 || segment === undefined || chunkIndexes.length !== 1 ||
    chunkIndex === undefined || chunk.owner.kind !== ownerKind || chunk.owner.id !== ownerId) {
    return undefined;
  }
  const chunkStart = completed.expected_bytes.prefix_offsets[chunkIndex];
  const exactStructuralField = ownerKind === "record" || (
    chunk.owner.field === logicalPath && segment.start_in_chunk === 0 &&
    segment.end_in_chunk === chunk.bytes.byteLength
  );
  const wholeStart = chunkStart === undefined ? undefined : chunkStart + segment.start_in_chunk;
  const wholeEnd = chunkStart === undefined ? undefined : chunkStart + segment.end_in_chunk;
  if (chunkStart === undefined || !exactStructuralField ||
    !Number.isSafeInteger(chunkStart) || !Number.isSafeInteger(startByte) || !Number.isSafeInteger(endByte) ||
    wholeStart === undefined || wholeEnd === undefined ||
    !Number.isSafeInteger(wholeStart) || !Number.isSafeInteger(wholeEnd) ||
    !Number.isSafeInteger(segment.start_in_chunk) || !Number.isSafeInteger(segment.end_in_chunk) ||
    segment.start_in_chunk < 0 || segment.start_in_chunk >= segment.end_in_chunk ||
    segment.end_in_chunk > chunk.bytes.byteLength ||
    startByte !== wholeStart || endByte !== wholeEnd ||
    segment.end_in_chunk - segment.start_in_chunk !== segment.bytes.byteLength ||
    !bytesEqual(chunk.bytes.subarray(segment.start_in_chunk, segment.end_in_chunk), segment.bytes)) {
    return undefined;
  }
  try {
    const slice = completed.expected_bytes.slice(startByte, endByte);
    return completed.expected_bytes.equals(slice, segment.bytes) ? segment : undefined;
  } catch {
    return undefined;
  }
}

export interface ExpectedByteView {
  readonly byte_length: number;
  readonly prefix_offsets: readonly number[];
  sourceFacts(): ExpectedSourceFacts;
  wholeSha256(): string;
  sliceBytes(start: number, end: number): Uint8Array;
  bytesEqual(other: ImmutableByteView | Uint8Array): boolean;
  contains(needle: Uint8Array): boolean;
  slice(start: number, end: number): ExpectedByteSlice;
  equals(slice: ExpectedByteSlice, bytes: Uint8Array): boolean;
  sha256(slice: ExpectedByteSlice): string;
}

export interface ExpectedSourceFacts {
  readonly byte_length: number;
  readonly sha256: string;
  readonly line_count: number;
  readonly eof: "lf" | "none";
}

/**
 * Immutable streaming byte surface shared by generated projections and committed legacy Markdown.
 * It exposes no backing chunk or implicit whole-file materializer; callers can request only an
 * explicit checked byte range.
 */
export interface ImmutableByteView {
  readonly byte_length: number;
  wholeSha256(): string;
  sliceBytes(start: number, end: number): Uint8Array;
  bytesEqual(other: ImmutableByteView | Uint8Array): boolean;
  contains(needle: Uint8Array): boolean;
}

export type ImmutableByteViewInput = ImmutableByteView | Uint8Array;

export interface ExpectedByteViewObserver {
  hashSegmentVisited(segment: ExpectedByteSlice["segments"][number]): void;
  sourceFactChunkVisited?(chunkIndex: number, byteLength: number): void;
  combinedHashBufferAllocated(byteLength: number): void;
  finalProjectionAllocated(byteLength: number): void;
}

interface PrivateByteView {
  readonly chunks: readonly Uint8Array[];
  source_facts?: ExpectedSourceFacts;
}

const privateByteViews = new WeakMap<object, PrivateByteView>();

export function isImmutableByteView(value: unknown): value is ImmutableByteView {
  return value !== null && typeof value === "object" && privateByteViews.has(value);
}

export interface ExpectedByteSlice {
  readonly segments: readonly {
    readonly chunk_index: number;
    readonly start_in_chunk: number;
    readonly end_in_chunk: number;
  }[];
  readonly byte_length: number;
}

export class ExpectedByteViewError extends Error {
  constructor(
    readonly reason: "bounds" | "utf8-boundary" | "prefix-overflow" | "invalid-utf8" | "foreign-slice",
    message: string,
  ) {
    super(message);
    this.name = "ExpectedByteViewError";
  }
}

export function checkedPrefixOffsets(byteLengths: readonly number[]): readonly number[] {
  const prefix: number[] = [0];
  for (const byteLength of byteLengths) {
    if (!Number.isSafeInteger(byteLength) || byteLength < 0) {
      throw new ExpectedByteViewError("prefix-overflow", "chunk byte length is not a nonnegative safe integer");
    }
    const next = prefix[prefix.length - 1] + byteLength;
    if (!Number.isSafeInteger(next)) {
      throw new ExpectedByteViewError("prefix-overflow", "rendered byte length exceeds safe integer range");
    }
    prefix.push(next);
  }
  return Object.freeze(prefix);
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

function cloneBytes(value: Uint8Array): Uint8Array {
  return new Uint8Array(value);
}

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.byteLength !== right.byteLength) return false;
  for (let index = 0; index < left.byteLength; index++) {
    if (left[index] !== right[index]) return false;
  }
  return true;
}

function issue(
  document: RoadmapDocument,
  code: "E-FIELD-CONSUMPTION" | "E-RENDER-EMPTY" | "E-RENDER-AUTHORITY" | "E-OUTPUT-SLOT",
  logical_path: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: document.document.source_path,
    logical_path,
    message,
    exit: 1,
  };
}

function assertValidUtf8Stream(chunks: readonly Uint8Array[]): void {
  let remaining = 0;
  let lead = 0;
  let continuationIndex = 0;
  for (const chunk of chunks) {
    for (const byte of chunk) {
      if (remaining === 0) {
        if (byte <= 0x7f) continue;
        if (byte >= 0xc2 && byte <= 0xdf) {
          remaining = 1;
          lead = byte;
          continuationIndex = 0;
          continue;
        }
        if (byte >= 0xe0 && byte <= 0xef) {
          remaining = 2;
          lead = byte;
          continuationIndex = 0;
          continue;
        }
        if (byte >= 0xf0 && byte <= 0xf4) {
          remaining = 3;
          lead = byte;
          continuationIndex = 0;
          continue;
        }
        throw new ExpectedByteViewError("invalid-utf8", "rendered bytes are not strict UTF-8");
      }
      if (byte < 0x80 || byte > 0xbf) {
        throw new ExpectedByteViewError("invalid-utf8", "rendered bytes contain an invalid UTF-8 continuation");
      }
      if (continuationIndex === 0) {
        if (lead === 0xe0 && byte < 0xa0) {
          throw new ExpectedByteViewError("invalid-utf8", "rendered bytes contain an overlong UTF-8 scalar");
        }
        if (lead === 0xed && byte > 0x9f) {
          throw new ExpectedByteViewError("invalid-utf8", "rendered bytes contain a UTF-8 surrogate");
        }
        if (lead === 0xf0 && byte < 0x90) {
          throw new ExpectedByteViewError("invalid-utf8", "rendered bytes contain an overlong UTF-8 scalar");
        }
        if (lead === 0xf4 && byte > 0x8f) {
          throw new ExpectedByteViewError("invalid-utf8", "rendered bytes contain an out-of-range scalar");
        }
      }
      continuationIndex++;
      remaining--;
    }
  }
  if (remaining !== 0) {
    throw new ExpectedByteViewError("invalid-utf8", "rendered bytes end in a truncated UTF-8 scalar");
  }
}

function chunkForOffset(prefix: readonly number[], byteLength: number, offset: number): number {
  if (offset === byteLength) return prefix.length - 2;
  let low = 0;
  let high = prefix.length - 1;
  while (low + 1 < high) {
    const middle = low + Math.floor((high - low) / 2);
    if (prefix[middle] <= offset) low = middle;
    else high = middle;
  }
  return low;
}

function privateView(view: ImmutableByteView): PrivateByteView {
  const value = privateByteViews.get(view as object);
  if (value === undefined) throw new ExpectedByteViewError("foreign-slice", "byte view was not minted by this module");
  return value;
}

function validateByteRange(byteLength: number, start: number, end: number): void {
  if (
    !Number.isSafeInteger(start) || !Number.isSafeInteger(end) ||
    start < 0 || start > end || end > byteLength
  ) {
    throw new ExpectedByteViewError("bounds", `invalid byte range [${start}, ${end})`);
  }
}

function privateSliceBytes(view: ImmutableByteView, start: number, end: number): Uint8Array {
  validateByteRange(view.byte_length, start, end);
  const result = new Uint8Array(end - start);
  if (start === end) return result;
  let logicalOffset = 0;
  let outputOffset = 0;
  for (const chunk of privateView(view).chunks) {
    const chunkEnd = logicalOffset + chunk.byteLength;
    if (chunkEnd > start && logicalOffset < end) {
      const from = Math.max(start, logicalOffset) - logicalOffset;
      const to = Math.min(end, chunkEnd) - logicalOffset;
      result.set(chunk.subarray(from, to), outputOffset);
      outputOffset += to - from;
    }
    logicalOffset = chunkEnd;
    if (logicalOffset >= end) break;
  }
  return result;
}

function computeSourceFacts(
  view: ImmutableByteView,
  observer?: ExpectedByteViewObserver,
): ExpectedSourceFacts {
  const privateValue = privateView(view);
  if (privateValue.source_facts !== undefined) return privateValue.source_facts;
  const hasher = new Bun.CryptoHasher("sha256");
  let lfCount = 0;
  let lastByte: number | undefined;
  // Traversal reporting is mandatory. Any future implementation that materializes a combined
  // source buffer must report that allocation through combinedHashBufferAllocated before use.
  for (const [chunkIndex, chunk] of privateValue.chunks.entries()) {
    observer?.sourceFactChunkVisited?.(chunkIndex, chunk.byteLength);
    hasher.update(chunk);
    for (const byte of chunk) {
      if (byte === 0x0a) lfCount++;
      lastByte = byte;
    }
  }
  privateValue.source_facts = Object.freeze({
    byte_length: view.byte_length,
    sha256: hasher.digest("hex"),
    line_count: view.byte_length === 0 ? 0 : lfCount + (lastByte === 0x0a ? 0 : 1),
    eof: lastByte === 0x0a ? "lf" : "none",
  });
  return privateValue.source_facts;
}

function* privateBytes(view: ImmutableByteView): Generator<number> {
  for (const chunk of privateView(view).chunks) {
    for (const byte of chunk) yield byte;
  }
}

function byteViewsEqual(left: ImmutableByteView, right: ImmutableByteView | Uint8Array): boolean {
  if (left.byte_length !== (right instanceof Uint8Array ? right.byteLength : right.byte_length)) return false;
  const leftBytes = privateBytes(left);
  const rightBytes: Iterator<number> = right instanceof Uint8Array
    ? right.values()
    : privateBytes(right);
  while (true) {
    const leftNext = leftBytes.next();
    const rightNext = rightBytes.next();
    if (leftNext.done || rightNext.done) return leftNext.done === rightNext.done;
    if (leftNext.value !== rightNext.value) return false;
  }
}

function byteViewContains(view: ImmutableByteView, needle: Uint8Array): boolean {
  if (needle.byteLength === 0 || needle.byteLength > view.byte_length) return false;
  const fallback = new Uint32Array(needle.byteLength);
  for (let index = 1, matched = 0; index < needle.byteLength; index++) {
    while (matched > 0 && needle[index] !== needle[matched]) matched = fallback[matched - 1];
    if (needle[index] === needle[matched]) matched++;
    fallback[index] = matched;
  }
  let matched = 0;
  for (const byte of privateBytes(view)) {
    while (matched > 0 && byte !== needle[matched]) matched = fallback[matched - 1];
    if (byte === needle[matched]) matched++;
    if (matched === needle.byteLength) return true;
  }
  return false;
}

/** Snapshot legacy bytes, or preserve an already-minted immutable generated view. */
export function createImmutableByteView(value: ImmutableByteViewInput): ImmutableByteView {
  if (isImmutableByteView(value)) return value;
  if (!(value instanceof Uint8Array)) throw new TypeError("immutable byte view input must be Uint8Array or a minted byte view");
  const snapshot = cloneBytes(value);
  let view: ImmutableByteView;
  view = Object.freeze({
    byte_length: snapshot.byteLength,
    wholeSha256: () => computeSourceFacts(view).sha256,
    sliceBytes: (start: number, end: number) => privateSliceBytes(view, start, end),
    bytesEqual: (other: ImmutableByteView | Uint8Array) => byteViewsEqual(view, other),
    contains: (needle: Uint8Array) => byteViewContains(view, needle),
  });
  privateByteViews.set(view, { chunks: Object.freeze([snapshot]) });
  return view;
}

/**
 * Build an immutable coordinate view over private chunk snapshots.  Equality and hashing walk the
 * addressed segments and never materialize the conceptual concatenation.
 */
export function createExpectedByteView(
  chunks: readonly RenderChunk[],
  observer?: ExpectedByteViewObserver,
): ExpectedByteView {
  const privateChunks = chunks.map((chunk) => cloneBytes(chunk.bytes));
  assertValidUtf8Stream(privateChunks);
  const prefix = checkedPrefixOffsets(privateChunks.map((chunk) => chunk.byteLength));
  const byteLength = prefix[prefix.length - 1];
  const mintedSlices = new WeakSet<object>();

  const byteAt = (offset: number): number => {
    const chunkIndex = chunkForOffset(prefix, byteLength, offset);
    return privateChunks[chunkIndex][offset - prefix[chunkIndex]];
  };
  const isScalarBoundary = (offset: number): boolean =>
    offset === 0 || offset === byteLength || (byteAt(offset) & 0xc0) !== 0x80;

  let view: ExpectedByteView;
  view = {
    prefix_offsets: prefix,
    byte_length: byteLength,
    sourceFacts: () => computeSourceFacts(view, observer),
    wholeSha256: () => computeSourceFacts(view, observer).sha256,
    sliceBytes: (start: number, end: number) => privateSliceBytes(view, start, end),
    bytesEqual: (other: ImmutableByteView | Uint8Array) => byteViewsEqual(view, other),
    contains: (needle: Uint8Array) => byteViewContains(view, needle),
    slice(start: number, end: number): ExpectedByteSlice {
      if (
        !Number.isSafeInteger(start) || !Number.isSafeInteger(end) ||
        start < 0 || start > end || end > byteLength
      ) {
        throw new ExpectedByteViewError("bounds", `invalid expected byte range [${start}, ${end})`);
      }
      if (!isScalarBoundary(start) || !isScalarBoundary(end)) {
        throw new ExpectedByteViewError("utf8-boundary", `range [${start}, ${end}) splits a UTF-8 scalar`);
      }
      const segments: { chunk_index: number; start_in_chunk: number; end_in_chunk: number }[] = [];
      if (start !== end) {
        let chunkIndex = chunkForOffset(prefix, byteLength, start);
        while (chunkIndex < privateChunks.length && prefix[chunkIndex] < end) {
          const segmentStart = Math.max(start, prefix[chunkIndex]) - prefix[chunkIndex];
          const segmentEnd = Math.min(end, prefix[chunkIndex + 1]) - prefix[chunkIndex];
          if (segmentStart < segmentEnd) {
            segments.push(Object.freeze({
              chunk_index: chunkIndex,
              start_in_chunk: segmentStart,
              end_in_chunk: segmentEnd,
            }));
          }
          chunkIndex++;
        }
      }
      const result: ExpectedByteSlice = Object.freeze({
        segments: Object.freeze(segments),
        byte_length: end - start,
      });
      mintedSlices.add(result);
      return result;
    },
    equals(slice: ExpectedByteSlice, bytes: Uint8Array): boolean {
      if (!mintedSlices.has(slice as object) || bytes.byteLength !== slice.byte_length) return false;
      let sourceOffset = 0;
      for (const segment of slice.segments) {
        const chunk = privateChunks[segment.chunk_index];
        for (let offset = segment.start_in_chunk; offset < segment.end_in_chunk; offset++) {
          if (chunk[offset] !== bytes[sourceOffset++]) return false;
        }
      }
      return sourceOffset === bytes.byteLength;
    },
    sha256(slice: ExpectedByteSlice): string {
      if (!mintedSlices.has(slice as object)) {
        throw new ExpectedByteViewError("foreign-slice", "slice was not created by this expected byte view");
      }
      const hasher = new Bun.CryptoHasher("sha256");
      // Traversal reporting is mandatory. Any future implementation that materializes a combined
      // hash buffer must report that allocation through combinedHashBufferAllocated before use.
      for (const segment of slice.segments) {
        observer?.hashSegmentVisited(segment);
        hasher.update(privateChunks[segment.chunk_index].subarray(
          segment.start_in_chunk,
          segment.end_in_chunk,
        ));
      }
      return hasher.digest("hex");
    },
  };
  Object.freeze(view);
  privateByteViews.set(view, { chunks: Object.freeze(privateChunks) });
  return view;
}

function collectMarkdownFields(
  value: unknown,
  logicalPath: string,
  out: Map<string, Uint8Array>,
): void {
  if (value instanceof Uint8Array) {
    const finalComponent = logicalPath.slice(logicalPath.lastIndexOf(".") + 1);
    if (finalComponent.endsWith("_md")) out.set(logicalPath, value);
    return;
  }
  if (Array.isArray(value)) {
    for (let index = 0; index < value.length; index++) {
      collectMarkdownFields(value[index], `${logicalPath}[${index}]`, out);
    }
    return;
  }
  if (value !== null && typeof value === "object") {
    for (const key of Object.keys(value).sort(codePointSort)) {
      collectMarkdownFields((value as Record<string, unknown>)[key], `${logicalPath}.${key}`, out);
    }
  }
}

function createFieldConsumer(
  ownerKind: ManifestEntry["kind"],
  ownerId: string,
  payload: SemanticPayload,
): {
  consumer: FieldConsumer;
  finish(): {
    ledger: FieldConsumptionLedgerEntry;
    returned_fields: readonly { logical_path: string; bytes: Uint8Array }[];
  };
} {
  const expected = new Map<string, Uint8Array>();
  collectMarkdownFields(payload, "payload", expected);
  const consumed: string[] = [];
  const duplicates = new Set<string>();
  const unknown = new Set<string>();
  const mismatched = new Set<string>();
  const returnedFields: { logical_path: string; bytes: Uint8Array }[] = [];
  const consumer: FieldConsumer = {
    consume(logical_path: string, value: Uint8Array): Uint8Array {
      const expectedBytes = expected.get(logical_path);
      if (expectedBytes === undefined) {
        unknown.add(logical_path);
        const returned = cloneBytes(value);
        returnedFields.push({ logical_path, bytes: returned });
        return returned;
      }
      if (consumed.includes(logical_path)) duplicates.add(logical_path);
      consumed.push(logical_path);
      if (!bytesEqual(expectedBytes, value)) mismatched.add(logical_path);
      const returned = cloneBytes(expectedBytes);
      returnedFields.push({ logical_path, bytes: returned });
      return returned;
    },
  };
  return {
    consumer,
    finish: () => ({
      ledger: {
        owner_kind: ownerKind,
        owner_id: ownerId,
        expected_fields: Object.freeze([...expected.keys()].sort(codePointSort)),
        consumed_fields: Object.freeze([...consumed]),
        duplicate_fields: Object.freeze([...duplicates].sort(codePointSort)),
        unknown_fields: Object.freeze([...unknown].sort(codePointSort)),
        mismatched_fields: Object.freeze([...mismatched].sort(codePointSort)),
      },
      returned_fields: Object.freeze(returnedFields.map((entry) => Object.freeze({
        logical_path: entry.logical_path,
        bytes: cloneBytes(entry.bytes),
      }))),
    }),
  };
}

function rawSource(value: object): { source_block_md: Uint8Array; span_ids: readonly SpanId[] } | undefined {
  if (!("source_block_md" in value) || !(value.source_block_md instanceof Uint8Array)) return undefined;
  if (!("span_ids" in value) || !Array.isArray(value.span_ids)) return undefined;
  return { source_block_md: value.source_block_md, span_ids: value.span_ids as readonly SpanId[] };
}

function replacements(value: object): readonly { span_id: SpanId; replacement_field: string }[] {
  if (!("source_replacements" in value) || !Array.isArray(value.source_replacements)) return [];
  return value.source_replacements as readonly { span_id: SpanId; replacement_field: string }[];
}

function immutableChunk(chunk: RenderChunk): RenderChunk {
  return Object.freeze({
    ...chunk,
    owner: Object.freeze({ ...chunk.owner }),
    bytes: cloneBytes(chunk.bytes),
    source_span_ids: Object.freeze([...chunk.source_span_ids]),
    consumed_fields: Object.freeze([...chunk.consumed_fields]),
  });
}

/** Build every chunk and ledger entry before reporting field/span/slot validation failures. */
export function buildExpectedChunks(
  document: RoadmapDocument,
  ops: readonly RenderOp[],
  services: RenderIrServices,
): CompletedRenderIr {
  const chunks: RenderChunk[] = [];
  const fieldConsumption: FieldConsumptionLedgerEntry[] = [];
  const projectedFieldSegments: ProjectedFieldSegment[] = [];
  const slotResolutions: CompletedSlotResolution[] = [];
  const buildIssues: RoadmapIssue[] = [];

  for (const op of ops) {
    const { node } = op;
    const raw = rawSource(node.value);
    if (raw !== undefined) {
      chunks.push(immutableChunk({
        manifest_index: op.manifest_index,
        owner: { kind: node.kind, id: node.id, field: "source_block_md" },
        bytes: raw.source_block_md,
        source_span_ids: raw.span_ids,
        consumed_fields: ["source_block_md"],
      }));
      continue;
    }

    if (node.kind === "generated_slot") {
      let resolution: GeneratedSlotResolution | undefined;
      try {
        resolution = services.resolveGeneratedSlot(node.value);
      } catch (error) {
        buildIssues.push(issue(
          document,
          "E-OUTPUT-SLOT",
          `generated_slot[${JSON.stringify(node.id)}]`,
          `slot resolver failed: ${error instanceof Error ? error.message : String(error)}`,
        ));
      }
      slotResolutions.push(Object.freeze({
        manifest_index: op.manifest_index,
        slot: node.value,
        resolution: resolution === undefined ? undefined : {
          binding: resolution.binding,
          bytes: cloneBytes(resolution.bytes),
        },
      }));
      chunks.push(immutableChunk({
        manifest_index: op.manifest_index,
        owner: { kind: node.kind, id: node.id, field: "generated" },
        bytes: resolution?.bytes ?? new Uint8Array(),
        source_span_ids: node.value.span_ids,
        consumed_fields: ["generated"],
      }));
      continue;
    }

    if (node.kind === "record" && "render_authority" in node.value && node.value.render_authority === "semantic") {
      const tracked = createFieldConsumer(node.kind, node.id, node.value.payload);
      let rendered: Uint8Array = new Uint8Array();
      try {
        rendered = services.renderSemanticRecord(node.value, tracked.consumer);
      } catch (error) {
        buildIssues.push(issue(
          document,
          "E-RENDER-AUTHORITY",
          `record[${JSON.stringify(node.id)}]`,
          `semantic renderer failed: ${error instanceof Error ? error.message : String(error)}`,
        ));
      }
      const trackedResult = tracked.finish();
      const ledger = trackedResult.ledger;
      fieldConsumption.push(Object.freeze(ledger));
      if (node.value.projection_visibility === "semantic_only" && rendered.byteLength !== 0) {
        buildIssues.push(issue(
          document,
          "E-RENDER-AUTHORITY",
          `record[${JSON.stringify(node.id)}].projection_visibility`,
          "semantic-only record renderer must emit zero bytes",
        ));
      }
      if (node.value.projection_visibility === "document") {
        const replacementRows = replacements(node.value);
        const replacementFields = new Set(replacementRows.map((replacement) => replacement.replacement_field));
        const replacementSpanIds = new Set(replacementRows.map((replacement) => replacement.span_id));
        if (replacementFields.size !== replacementRows.length || replacementSpanIds.size !== replacementRows.length) {
          buildIssues.push(issue(
            document,
            "E-FIELD-CONSUMPTION",
            `record[${JSON.stringify(node.id)}].source_replacements`,
            "document-visible semantic replacements must have unique fields and span IDs",
          ));
        }
        const projected = trackedResult.returned_fields.filter((entry) => replacementFields.has(entry.logical_path));
        const projectedLength = projected.reduce((sum, entry) => sum + entry.bytes.byteLength, 0);
        const exactProjection = projected.length === replacementRows.length &&
          projectedLength === rendered.byteLength && (() => {
            let offset = 0;
            for (const entry of projected) {
              if (!bytesEqual(rendered.subarray(offset, offset + entry.bytes.byteLength), entry.bytes)) return false;
              offset += entry.bytes.byteLength;
            }
            return true;
          })();
        if (!exactProjection) {
          buildIssues.push(issue(
            document,
            "E-RENDER-AUTHORITY",
            `record[${JSON.stringify(node.id)}]`,
            "document-visible semantic output is not the exact canonical concatenation of its replacement fields",
          ));
        } else {
          let offset = 0;
          for (const entry of projected) {
            projectedFieldSegments.push(Object.freeze({
              owner_kind: "record",
              owner_id: node.id,
              logical_path: entry.logical_path,
              start_in_chunk: offset,
              end_in_chunk: offset + entry.bytes.byteLength,
              bytes: cloneBytes(entry.bytes),
            }));
            offset += entry.bytes.byteLength;
          }
        }
      }
      chunks.push(immutableChunk({
        manifest_index: op.manifest_index,
        owner: { kind: node.kind, id: node.id, field: "payload" },
        bytes: rendered,
        source_span_ids: replacements(node.value).map((replacement) => replacement.span_id),
        consumed_fields: ledger.consumed_fields,
      }));
      continue;
    }

    let field: "body_md" | "marker_md" | undefined;
    if ("body_md" in node.value && node.value.body_md instanceof Uint8Array) field = "body_md";
    if ("marker_md" in node.value && node.value.marker_md instanceof Uint8Array) field = "marker_md";
    if (field !== undefined) {
      const rendered = field === "body_md"
        ? (node.value as { body_md: Uint8Array }).body_md
        : (node.value as { marker_md: Uint8Array }).marker_md;
      const ledger: FieldConsumptionLedgerEntry = Object.freeze({
        owner_kind: node.kind,
        owner_id: node.id,
        expected_fields: Object.freeze([field]),
        consumed_fields: Object.freeze([field]),
        duplicate_fields: Object.freeze([]),
        unknown_fields: Object.freeze([]),
        mismatched_fields: Object.freeze([]),
      });
      fieldConsumption.push(ledger);
      const replacementRows = replacements(node.value);
      if (
        replacementRows.length !== 1 || replacementRows[0]?.replacement_field !== field ||
        rendered.byteLength === 0
      ) {
        buildIssues.push(issue(
          document,
          "E-FIELD-CONSUMPTION",
          `${node.kind}[${JSON.stringify(node.id)}].source_replacements`,
          "semantic structural output requires one non-empty full-field replacement",
        ));
      } else {
        projectedFieldSegments.push(Object.freeze({
          owner_kind: node.kind,
          owner_id: node.id,
          logical_path: field,
          start_in_chunk: 0,
          end_in_chunk: rendered.byteLength,
          bytes: cloneBytes(rendered),
        }));
      }
      chunks.push(immutableChunk({
        manifest_index: op.manifest_index,
        owner: { kind: node.kind, id: node.id, field },
        bytes: rendered,
        source_span_ids: replacements(node.value).map((replacement) => replacement.span_id),
        consumed_fields: [field],
      }));
      continue;
    }

    buildIssues.push(issue(
      document,
      "E-RENDER-AUTHORITY",
      `${node.kind}[${JSON.stringify(node.id)}]`,
      "render node has neither raw nor semantic authority bytes",
    ));
    chunks.push(immutableChunk({
      manifest_index: op.manifest_index,
      owner: { kind: node.kind, id: node.id, field: "invalid" },
      bytes: new Uint8Array(),
      source_span_ids: [],
      consumed_fields: [],
    }));
  }

  // Semantic-only records are first-class semantic owners, not document render nodes. Validate
  // their renderer and complete field consumption without minting a zero-byte manifest chunk.
  for (const record of document.records) {
    if (!("render_authority" in record) || record.render_authority !== "semantic" ||
      record.projection_visibility !== "semantic_only") continue;
    const tracked = createFieldConsumer("record", record.id, record.payload);
    let rendered: Uint8Array = new Uint8Array();
    try {
      rendered = services.renderSemanticRecord(record, tracked.consumer);
    } catch (error) {
      buildIssues.push(issue(
        document,
        "E-RENDER-AUTHORITY",
        `record[${JSON.stringify(record.id)}]`,
        `semantic renderer failed: ${error instanceof Error ? error.message : String(error)}`,
      ));
    }
    fieldConsumption.push(Object.freeze(tracked.finish().ledger));
    if (rendered.byteLength !== 0) {
      buildIssues.push(issue(
        document,
        "E-RENDER-AUTHORITY",
        `record[${JSON.stringify(record.id)}].projection_visibility`,
        "semantic-only record renderer must emit zero bytes",
      ));
    }
  }

  let expectedBytes: ExpectedByteView;
  try {
    expectedBytes = createExpectedByteView(chunks);
  } catch (error) {
    buildIssues.push(issue(
      document,
      "E-RENDER-AUTHORITY",
      "render.chunks",
      error instanceof Error ? error.message : String(error),
    ));
    expectedBytes = createExpectedByteView([]);
  }
  return Object.freeze({
    chunks: Object.freeze(chunks),
    expected_bytes: expectedBytes,
    field_consumption: Object.freeze(fieldConsumption),
    projected_field_segments: Object.freeze(projectedFieldSegments),
    slot_resolutions: Object.freeze(slotResolutions),
    build_issues: Object.freeze(buildIssues),
  });
}

function opIdentity(op: RenderOp): string {
  return JSON.stringify([op.node.kind, op.node.id]);
}

function stringArraysEqual(left: readonly string[], right: readonly string[]): boolean {
  return left.length === right.length && left.every((value, index) => value === right[index]);
}

function fieldLedgerKey(kind: ManifestEntry["kind"], id: string): string {
  return JSON.stringify([kind, id]);
}

function expectedLedgerFields(op: RenderOp): readonly string[] | undefined {
  if (rawSource(op.node.value) !== undefined || op.node.kind === "generated_slot") return undefined;
  if (
    op.node.kind === "record" && "render_authority" in op.node.value &&
    op.node.value.render_authority === "semantic"
  ) {
    const expected = new Map<string, Uint8Array>();
    collectMarkdownFields(op.node.value.payload, "payload", expected);
    return Object.freeze([...expected.keys()].sort(codePointSort));
  }
  if ("body_md" in op.node.value && op.node.value.body_md instanceof Uint8Array) {
    return Object.freeze(["body_md"]);
  }
  if ("marker_md" in op.node.value && op.node.value.marker_md instanceof Uint8Array) {
    return Object.freeze(["marker_md"]);
  }
  return undefined;
}

function semanticOnlyLedgerFields(
  record: Extract<RoadmapDocument["records"][number], { render_authority: "semantic" }>,
): readonly string[] {
  const expected = new Map<string, Uint8Array>();
  collectMarkdownFields(record.payload, "payload", expected);
  return Object.freeze([...expected.keys()].sort(codePointSort));
}

/** Validate only completed chunks/ledgers; declarations alone are never an accepted input seam. */
export function validateCompletedChunks(
  document: RoadmapDocument,
  ops: readonly RenderOp[],
  completed: CompletedRenderIr,
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [...completed.build_issues];
  if (completed.chunks.length === 0 || completed.expected_bytes.byte_length === 0) {
    issues.push(issue(document, "E-RENDER-EMPTY", "render.chunks", "render produced no bytes"));
  }
  if (completed.chunks.length !== ops.length) {
    issues.push(issue(
      document,
      "E-RENDER-AUTHORITY",
      "render.chunks",
      `render produced ${completed.chunks.length} chunks for ${ops.length} manifest operations`,
    ));
  }
  const expectedFieldLedgers = new Map<string, readonly string[]>();
  const expectedSlotLedgers = new Map<string, Extract<RenderOp["node"], { kind: "generated_slot" }>["value"]>();
  for (const op of ops) {
    const fields = expectedLedgerFields(op);
    if (fields !== undefined) expectedFieldLedgers.set(fieldLedgerKey(op.node.kind, op.node.id), fields);
    if (op.node.kind === "generated_slot") {
      expectedSlotLedgers.set(JSON.stringify([op.manifest_index, op.node.id]), op.node.value);
    }
  }
  for (const record of document.records) {
    if ("render_authority" in record && record.render_authority === "semantic" &&
      record.projection_visibility === "semantic_only") {
      expectedFieldLedgers.set(fieldLedgerKey("record", record.id), semanticOnlyLedgerFields(record));
    }
  }

  const fieldLedgerCounts = new Map<string, number>();
  for (const ledger of completed.field_consumption) {
    const key = fieldLedgerKey(ledger.owner_kind, ledger.owner_id);
    fieldLedgerCounts.set(key, (fieldLedgerCounts.get(key) ?? 0) + 1);
    const expectedFields = expectedFieldLedgers.get(key);
    if (expectedFields === undefined) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        `${ledger.owner_kind}[${JSON.stringify(ledger.owner_id)}]`,
        "field ledger has no semantic manifest owner",
      ));
    } else if (!stringArraysEqual(ledger.expected_fields, expectedFields)) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        `${ledger.owner_kind}[${JSON.stringify(ledger.owner_id)}]`,
        "field ledger expected-field inventory does not exactly match its semantic owner",
      ));
    }
  }
  for (const key of expectedFieldLedgers.keys()) {
    const count = fieldLedgerCounts.get(key) ?? 0;
    if (count !== 1) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        `field_ledger.${key}`,
        `semantic owner has ${count} field ledgers, expected exactly one`,
      ));
    }
  }

  const semanticOps = ops.filter((op) => expectedLedgerFields(op) !== undefined);
  for (const segment of completed.projected_field_segments) {
    const owners = semanticOps.filter((op) =>
      op.node.kind === segment.owner_kind && op.node.id === segment.owner_id
    );
    if (owners.length !== 1) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        `projected_field_segment[${JSON.stringify(segment.owner_id)},${JSON.stringify(segment.logical_path)}]`,
        `projected field segment has ${owners.length} semantic manifest owners, expected exactly one`,
      ));
    }
  }
  for (const op of semanticOps) {
    const logicalPath = `${op.node.kind}[${JSON.stringify(op.node.id)}].projected_field_segments`;
    const segments = completed.projected_field_segments.filter((segment) =>
      segment.owner_kind === op.node.kind && segment.owner_id === op.node.id
    );
    const semanticOnly = op.node.kind === "record" && "projection_visibility" in op.node.value &&
      op.node.value.projection_visibility === "semantic_only";
    if (semanticOnly) {
      if (segments.length !== 0) {
        issues.push(issue(document, "E-FIELD-CONSUMPTION", logicalPath, "semantic-only record has projected field segments"));
      }
      continue;
    }
    const replacementRows = replacements(op.node.value);
    const replacementFields = new Set(replacementRows.map((replacement) => replacement.replacement_field));
    const replacementSpanIds = new Set(replacementRows.map((replacement) => replacement.span_id));
    const ledger = completed.field_consumption.filter((entry) =>
      entry.owner_kind === op.node.kind && entry.owner_id === op.node.id
    );
    const structuralField = op.node.kind === "record"
      ? undefined
      : "marker_md" in op.node.value ? "marker_md" : "body_md";
    const expectedPaths = structuralField === undefined
      ? (ledger[0]?.consumed_fields ?? []).filter((field) => replacementFields.has(field))
      : [structuralField];
    const actualPaths = segments.map((segment) => segment.logical_path);
    if (
      replacementFields.size !== replacementRows.length || replacementSpanIds.size !== replacementRows.length ||
      ledger.length !== 1 || !stringArraysEqual(actualPaths, expectedPaths) ||
      segments.length !== replacementRows.length ||
      (structuralField !== undefined && (
        replacementRows.length !== 1 || replacementRows[0]?.replacement_field !== structuralField ||
        !stringArraysEqual(ledger[0]?.consumed_fields ?? [], [structuralField])
      ))
    ) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        logicalPath,
        "projected fields are not in exact bijection with unique replacements and canonical consumption order",
      ));
    }
    const chunkIndex = ops.indexOf(op);
    const chunk = completed.chunks[chunkIndex];
    let offset = 0;
    for (const segment of segments) {
      const validCoordinates = Number.isSafeInteger(segment.start_in_chunk) &&
        Number.isSafeInteger(segment.end_in_chunk) && segment.start_in_chunk === offset &&
        segment.end_in_chunk > segment.start_in_chunk &&
        segment.end_in_chunk - segment.start_in_chunk === segment.bytes.byteLength;
      const validBytes = chunk !== undefined && segment.end_in_chunk <= chunk.bytes.byteLength &&
        bytesEqual(chunk.bytes.subarray(segment.start_in_chunk, segment.end_in_chunk), segment.bytes);
      if (!validCoordinates || !validBytes) {
        issues.push(issue(
          document,
          "E-RENDER-AUTHORITY",
          logicalPath,
          `projected field ${JSON.stringify(segment.logical_path)} does not exactly match its contiguous chunk interval`,
        ));
      }
      offset = segment.end_in_chunk;
    }
    if (chunk === undefined || offset !== chunk.bytes.byteLength) {
      issues.push(issue(
        document,
        "E-RENDER-AUTHORITY",
        logicalPath,
        "projected field segments do not cover the complete semantic chunk",
      ));
    }
  }

  const slotLedgerCounts = new Map<string, number>();
  for (const item of completed.slot_resolutions) {
    const key = JSON.stringify([item.manifest_index, item.slot.slot_id]);
    slotLedgerCounts.set(key, (slotLedgerCounts.get(key) ?? 0) + 1);
    const expectedSlot = expectedSlotLedgers.get(key);
    if (
      expectedSlot === undefined || item.slot.binding !== expectedSlot.binding ||
      !stringArraysEqual(item.slot.span_ids, expectedSlot.span_ids)
    ) {
      issues.push(issue(
        document,
        "E-OUTPUT-SLOT",
        `slot_resolutions[${JSON.stringify(item.slot.slot_id)}]`,
        "slot-resolution ledger has no exact manifest slot owner",
      ));
    }
  }
  for (const key of expectedSlotLedgers.keys()) {
    const count = slotLedgerCounts.get(key) ?? 0;
    if (count !== 1) {
      issues.push(issue(
        document,
        "E-OUTPUT-SLOT",
        `slot_ledger.${key}`,
        `generated slot has ${count} resolution ledgers, expected exactly one`,
      ));
    }
  }

  const seenIndexes = new Set<number>();
  for (const [chunkIndex, chunk] of completed.chunks.entries()) {
    const logicalPath = `render.chunks[${chunkIndex}]`;
    if (seenIndexes.has(chunk.manifest_index)) {
      issues.push(issue(document, "E-RENDER-AUTHORITY", logicalPath, "manifest index is duplicated"));
      continue;
    }
    seenIndexes.add(chunk.manifest_index);
    const op = ops[chunkIndex];
    if (op === undefined) {
      issues.push(issue(document, "E-RENDER-AUTHORITY", logicalPath, "chunk has no positional manifest operation"));
      continue;
    }
    if (
      chunk.manifest_index !== op.manifest_index ||
      JSON.stringify([chunk.owner.kind, chunk.owner.id]) !== opIdentity(op)
    ) {
      issues.push(issue(
        document,
        "E-RENDER-AUTHORITY",
        logicalPath,
        "chunk does not positionally match its manifest operation and exact owner",
      ));
    }
    const raw = rawSource(op.node.value);
    if (raw !== undefined) {
      if (
        chunk.owner.field !== "source_block_md" || !bytesEqual(chunk.bytes, raw.source_block_md) ||
        !stringArraysEqual(chunk.source_span_ids, raw.span_ids) ||
        !stringArraysEqual(chunk.consumed_fields, ["source_block_md"])
      ) {
        issues.push(issue(
          document,
          "E-RENDER-AUTHORITY",
          logicalPath,
          "raw chunk does not exactly match its authority bytes, spans, and field ledger",
        ));
      }
      continue;
    }
    if (op.node.kind === "generated_slot") {
      const resolution = completed.slot_resolutions.filter((value) =>
        value.manifest_index === chunk.manifest_index && value.slot.slot_id === op.node.id
      );
      if (
        chunk.owner.field !== "generated" ||
        !stringArraysEqual(chunk.source_span_ids, op.node.value.span_ids) ||
        !stringArraysEqual(chunk.consumed_fields, ["generated"]) ||
        resolution.length !== 1 || resolution[0].resolution === undefined ||
        !bytesEqual(chunk.bytes, resolution[0].resolution.bytes)
      ) {
        issues.push(issue(
          document,
          "E-OUTPUT-SLOT",
          logicalPath,
          "generated chunk does not exactly match its completed resolver result",
        ));
      }
      continue;
    }
    if (op.node.kind === "record" && "render_authority" in op.node.value && op.node.value.render_authority === "semantic") {
      const ledger = completed.field_consumption.filter((value) =>
        value.owner_kind === "record" && value.owner_id === op.node.id
      );
      const replacementIds = replacements(op.node.value).map((value) => value.span_id);
      if (
        chunk.owner.field !== "payload" || ledger.length !== 1 ||
        !stringArraysEqual(chunk.consumed_fields, ledger[0]?.consumed_fields ?? []) ||
        !stringArraysEqual(chunk.source_span_ids, replacementIds) ||
        (op.node.value.projection_visibility === "semantic_only" && chunk.bytes.byteLength !== 0)
      ) {
        issues.push(issue(
          document,
          "E-FIELD-CONSUMPTION",
          logicalPath,
          "semantic record chunk does not match its completed field/replacement ledger",
        ));
      }
      continue;
    }
    const semanticField = "body_md" in op.node.value
      ? "body_md"
      : "marker_md" in op.node.value ? "marker_md" : undefined;
    if (semanticField !== undefined) {
      const value = semanticField === "body_md"
        ? (op.node.value as { body_md: Uint8Array }).body_md
        : (op.node.value as { marker_md: Uint8Array }).marker_md;
      const ledger = completed.field_consumption.filter((candidate) =>
        candidate.owner_kind === op.node.kind && candidate.owner_id === op.node.id
      );
      if (
        chunk.owner.field !== semanticField || !bytesEqual(chunk.bytes, value) || ledger.length !== 1 ||
        !stringArraysEqual(chunk.consumed_fields, [semanticField]) ||
        !stringArraysEqual(chunk.source_span_ids, replacements(op.node.value).map((replacement) => replacement.span_id))
      ) {
        issues.push(issue(
          document,
          "E-FIELD-CONSUMPTION",
          logicalPath,
          "semantic structural chunk does not match its completed field/replacement ledger",
        ));
      }
    }
  }
  for (const ledger of completed.field_consumption) {
    const consumedCounts = new Map<string, number>();
    for (const field of ledger.consumed_fields) {
      consumedCounts.set(field, (consumedCounts.get(field) ?? 0) + 1);
    }
    const missing = ledger.expected_fields.filter((field) => !consumedCounts.has(field));
    const duplicate = [
      ...ledger.duplicate_fields,
      ...[...consumedCounts].filter(([, count]) => count !== 1).map(([field]) => field),
    ].filter((field, index, all) => all.indexOf(field) === index).sort(codePointSort);
    if (
      missing.length > 0 || duplicate.length > 0 || ledger.unknown_fields.length > 0 ||
      ledger.mismatched_fields.length > 0
    ) {
      issues.push(issue(
        document,
        "E-FIELD-CONSUMPTION",
        `${ledger.owner_kind}[${JSON.stringify(ledger.owner_id)}]`,
        `field consumption is not exact (missing=${JSON.stringify(missing)}, duplicate=${JSON.stringify(duplicate)}, unknown=${JSON.stringify(ledger.unknown_fields)}, mismatched=${JSON.stringify(ledger.mismatched_fields)})`,
      ));
    }
  }
  const slotCount = new Map<string, number>();
  for (const item of completed.slot_resolutions) {
    slotCount.set(item.slot.slot_id, (slotCount.get(item.slot.slot_id) ?? 0) + 1);
    const logicalPath = `generated_slot[${JSON.stringify(item.slot.slot_id)}]`;
    if (item.resolution === undefined) {
      issues.push(issue(document, "E-OUTPUT-SLOT", logicalPath, "slot has no resolver result"));
      continue;
    }
    if (item.resolution.binding !== item.slot.binding) {
      issues.push(issue(
        document,
        "E-OUTPUT-SLOT",
        logicalPath,
        `resolver binding ${JSON.stringify(item.resolution.binding)} does not match declaration ${JSON.stringify(item.slot.binding)}`,
      ));
    }
    if (item.resolution.bytes.byteLength === 0) {
      issues.push(issue(document, "E-OUTPUT-SLOT", logicalPath, "slot resolver returned an empty payload"));
    }
  }
  for (const slot of document.generated_slots) {
    if ((slotCount.get(slot.slot_id) ?? 0) !== 1) {
      issues.push(issue(
        document,
        "E-OUTPUT-SLOT",
        `generated_slot[${JSON.stringify(slot.slot_id)}]`,
        `slot resolves ${slotCount.get(slot.slot_id) ?? 0} times, expected exactly once`,
      ));
    }
  }
  return Object.freeze(issues);
}
