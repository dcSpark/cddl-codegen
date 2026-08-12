import type { RoadmapIssue } from "./errors.ts";
import type { RepoPath } from "./model/core.ts";
import type {
  ExpectedByteView,
  ExpectedByteViewObserver,
  RenderChunk,
} from "./render_ir.ts";

export class RenderValidationError extends Error {
  constructor(readonly issues: readonly RoadmapIssue[]) {
    super(`cannot render projection with ${issues.length} validation issue(s)`);
    this.name = "RenderValidationError";
  }
}

/**
 * The sole final projection concatenator.  Callers pass every structural/debt/output validation
 * issue; allocation and copying occur only after that complete set is empty.
 */
export function renderValidatedChunks(
  chunks: readonly RenderChunk[],
  validationIssues: readonly RoadmapIssue[],
  expectedBytes: ExpectedByteView,
  observer?: ExpectedByteViewObserver,
): Uint8Array {
  if (validationIssues.length > 0) throw new RenderValidationError(validationIssues);
  if (chunks.length === 0) throw new RenderValidationError(Object.freeze([]));
  if (expectedBytes.prefix_offsets.length !== chunks.length + 1) {
    throw new RenderValidationError(Object.freeze([{
      code: "E-RENDER-AUTHORITY",
      source: "<internal>",
      logical_path: "render.expected_bytes",
      message: "expected-byte prefix index does not match the completed chunk count",
      exit: 2,
    }]));
  }
  let byteLength = 0;
  for (const [index, chunk] of chunks.entries()) {
    const start = expectedBytes.prefix_offsets[index];
    const end = expectedBytes.prefix_offsets[index + 1];
    if (!expectedBytes.equals(expectedBytes.slice(start, end), chunk.bytes)) {
      throw new RenderValidationError(Object.freeze([{
        code: "E-RENDER-AUTHORITY",
        source: "<internal>",
        logical_path: `render.chunks[${index}]`,
        message: "completed chunk bytes changed after expected-byte validation",
        exit: 2,
      }]));
    }
    byteLength += chunk.bytes.byteLength;
    if (!Number.isSafeInteger(byteLength)) throw new RangeError("rendered byte length exceeds safe integer range");
  }
  if (byteLength === 0) throw new RenderValidationError(Object.freeze([]));
  observer?.finalProjectionAllocated(byteLength);
  const result = new Uint8Array(byteLength);
  let offset = 0;
  for (const chunk of chunks) {
    result.set(chunk.bytes, offset);
    offset += chunk.bytes.byteLength;
  }
  return result;
}

export interface ProjectionCheckResult {
  readonly expected: Uint8Array;
  readonly issues: readonly RoadmapIssue[];
}

/** Check an already validated redesigned projection without reusing frozen legacy chunk provenance. */
export function checkCommittedProjectionBytes(
  expected: Uint8Array,
  projectionPath: RepoPath,
  readCommitted: () => Uint8Array,
): ProjectionCheckResult {
  const actual = new Uint8Array(readCommitted());
  const difference = firstDifference(expected, actual);
  if (difference === -1) return Object.freeze({ expected: new Uint8Array(expected), issues: Object.freeze([]) });
  const drift: RoadmapIssue = {
    code: "E-PROJECTION-DRIFT",
    source: projectionPath,
    logical_path: "projection",
    message: `projection drift: expected sha256=${sha256(expected)} length=${expected.byteLength}, actual sha256=${sha256(actual)} length=${actual.byteLength}, first differing byte=${difference}, expected context=${localByteContext(expected, difference)}, actual context=${localByteContext(actual, difference)}`,
    exit: 1,
  };
  return Object.freeze({ expected: new Uint8Array(expected), issues: Object.freeze([drift]) });
}

function sha256(bytes: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(bytes).digest("hex");
}

function firstDifference(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < shared; index++) if (left[index] !== right[index]) return index;
  return left.byteLength === right.byteLength ? -1 : shared;
}

function escapedByte(byte: number): string {
  if (byte === 0x0a) return "\\n";
  if (byte === 0x0d) return "\\r";
  if (byte === 0x09) return "\\t";
  if (byte === 0x5c) return "\\\\";
  if (byte === 0x22) return '\\"';
  if (byte >= 0x20 && byte <= 0x7e) return String.fromCharCode(byte);
  return `\\x${byte.toString(16).padStart(2, "0")}`;
}

function localByteContext(bytes: Uint8Array, difference: number): string {
  const radius = 12;
  const start = Math.max(0, difference - radius);
  const end = Math.min(bytes.byteLength, difference + radius + 1);
  return `[${start},${end}) \"${[...bytes.subarray(start, end)].map(escapedByte).join("")}\"`;
}

/** The committed projection callback is invoked only after validation and final rendering. */
export function renderThenCheckCommittedProjection(
  chunks: readonly RenderChunk[],
  validationIssues: readonly RoadmapIssue[],
  expectedBytes: ExpectedByteView,
  projectionPath: RepoPath,
  readCommitted: () => Uint8Array,
): ProjectionCheckResult {
  const expected = renderValidatedChunks(chunks, validationIssues, expectedBytes);
  const actual = new Uint8Array(readCommitted());
  const difference = firstDifference(expected, actual);
  if (difference === -1) return Object.freeze({ expected, issues: Object.freeze([]) });
  const drift: RoadmapIssue = {
    code: "E-PROJECTION-DRIFT",
    source: projectionPath,
    logical_path: "projection",
    message: `projection drift: expected sha256=${sha256(expected)} length=${expected.byteLength}, actual sha256=${sha256(actual)} length=${actual.byteLength}, first differing byte=${difference}, expected context=${localByteContext(expected, difference)}, actual context=${localByteContext(actual, difference)}`,
    exit: 1,
  };
  return Object.freeze({ expected, issues: Object.freeze([drift]) });
}
