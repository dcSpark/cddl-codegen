/**
 * Deep-freeze support for memoized live-document values shared across selftest cases.
 *
 * The live TOML sources are committed bytes — content-stable within a run — so their decoded
 * documents are memoized once per process. Freezing the shared value makes any in-place mutation
 * by a caller throw immediately (strict mode) instead of silently contaminating later cases.
 * ArrayBuffer views are skipped because Object.freeze on a non-empty typed array throws by spec;
 * nothing in the harness writes into decoded byte fields.
 */
export function deepFreeze<T>(value: T): T {
  freezeRecursively(value, new WeakSet());
  return value;
}

function freezeRecursively(value: unknown, seen: WeakSet<object>): void {
  if (value === null || typeof value !== "object") return;
  if (ArrayBuffer.isView(value)) return;
  if (seen.has(value)) return;
  seen.add(value);
  Object.freeze(value);
  for (const key of Object.getOwnPropertyNames(value)) {
    freezeRecursively((value as Record<string, unknown>)[key], seen);
  }
}
