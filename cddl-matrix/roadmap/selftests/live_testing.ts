// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveTestingProjectionText from "../../../tests/TESTING_ROADMAP.md" with { type: "text" };
import liveTestingSourceText from "../../../tests/testing-roadmap.toml" with { type: "text" };
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV3 } from "../model/documents.ts";
import { deepFreeze } from "./frozen.ts";

const UTF8 = new TextEncoder();
const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

/**
 * Memoized once per process: the input is the imported committed source text, content-stable
 * within a run, so re-decoding (which also re-composes to prove byte-canonicality) on every call
 * only re-derives the same value. The shared document is deep-frozen so accidental in-place
 * mutation by one case fails loudly instead of leaking into later cases.
 */
let memoizedTestingDocument: RoadmapDocumentV3 | undefined;

export function liveTestingV3Document(): RoadmapDocumentV3 {
  if (memoizedTestingDocument !== undefined) return memoizedTestingDocument;
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveTestingSourceText),
    TESTING_SOURCE_PATH,
    "testing",
  );
  assert(
    decoded.document.schema_version === 3,
    "committed testing self-test source is not schema v3",
  );
  assert(
    decoded.document.source_path === TESTING_SOURCE_PATH &&
      decoded.document.projection_path === TESTING_PROJECTION_PATH,
    "committed testing self-test source does not declare the live production paths",
  );
  memoizedTestingDocument = deepFreeze(decoded as RoadmapDocumentV3);
  return memoizedTestingDocument;
}

export function liveTestingAuthoritativeSource(): Uint8Array {
  liveTestingV3Document();
  return UTF8.encode(liveTestingSourceText);
}

export function liveTestingProjection(): Uint8Array {
  return UTF8.encode(liveTestingProjectionText);
}
