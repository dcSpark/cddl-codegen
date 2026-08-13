import type { RoadmapIssue } from "./errors.ts";
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
 * The sole final projection concatenator.  Callers pass every structural/output validation
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

