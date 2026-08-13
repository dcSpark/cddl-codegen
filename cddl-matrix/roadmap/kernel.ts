/**
 * Shared pure primitives for the roadmap tree. This module imports nothing, so any layer
 * (model, decode, render, io, selftests) may use it without creating cycles. Everything here is
 * deterministic and effect-free: no I/O, no clock, no environment.
 */

/** Code-point (not locale) string comparator — the tree's one canonical sort order. */
export function codePointSort(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

/** Lowercase hex SHA-256 of the exact bytes. */
export function sha256(bytes: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(bytes).digest("hex");
}

/** Exact byte-wise equality. */
export function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.byteLength !== right.byteLength) return false;
  for (let index = 0; index < left.byteLength; index++) {
    if (left[index] !== right[index]) return false;
  }
  return true;
}

/** Concatenate byte chunks into one freshly allocated buffer. */
export function concatenate(chunks: readonly Uint8Array[]): Uint8Array {
  const result = new Uint8Array(chunks.reduce((sum, chunk) => sum + chunk.byteLength, 0));
  let offset = 0;
  for (const chunk of chunks) {
    result.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return result;
}
