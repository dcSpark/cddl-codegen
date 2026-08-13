// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveTestingProjectionText from "../../../tests/TESTING_ROADMAP.md" with { type: "text" };
import liveTestingSourceText from "../../../tests/testing-roadmap.toml" with { type: "text" };
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { resolveManifest } from "../manifest.ts";
import type { RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";
import { renderValidatedChunks } from "../render.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";
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
let memoizedTestingDocument: RoadmapDocumentV2 | undefined;

export function liveTestingV2Document(): RoadmapDocumentV2 {
  if (memoizedTestingDocument !== undefined) return memoizedTestingDocument;
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveTestingSourceText),
    TESTING_SOURCE_PATH,
    "testing",
  );
  assert(
    decoded.document.schema_version === 2 && decoded.document.authority === "authoritative",
    "committed testing self-test source is not authoritative schema v2",
  );
  assert(
    decoded.document.source_path === TESTING_SOURCE_PATH &&
      decoded.document.projection_path === TESTING_PROJECTION_PATH,
    "committed testing self-test source does not declare the live production paths",
  );
  memoizedTestingDocument = deepFreeze(decoded as RoadmapDocumentV2);
  return memoizedTestingDocument;
}

export function liveTestingAuthoritativeSource(): Uint8Array {
  liveTestingV2Document();
  return UTF8.encode(liveTestingSourceText);
}

export function liveTestingProjection(): Uint8Array {
  return UTF8.encode(liveTestingProjectionText);
}

/**
 * Memoized like the document above (pure function of the memoized document); callers get a fresh
 * copy of the bytes so no caller can mutate another's view.
 */
let memoizedLegacyProjection: Uint8Array | undefined;

/** The pre-anchor-layout projection, rendered from the live source without the anchor layout. */
export function liveTestingLegacyProjection(): Uint8Array {
  if (memoizedLegacyProjection !== undefined) return new Uint8Array(memoizedLegacyProjection);
  const document = liveTestingV2Document();
  const placement = resolveManifest(document);
  const completed = buildExpectedChunks(document, placement.ops, {
    renderSemanticRecord(record, fields) {
      return TESTING_ADAPTER.renderSemantic(record, fields);
    },
    resolveGeneratedSlot() {
      return undefined;
    },
  });
  const issues = [
    ...placement.issues,
    ...validateCompletedChunks(document, placement.ops, completed),
  ];
  assert(issues.length === 0, `live testing legacy projection failed rendering: ${JSON.stringify(issues)}`);
  const projection = renderValidatedChunks(completed.chunks, issues, completed.expected_bytes);
  // Derived, not hand-maintained: the live TOML's own frozen_source_* fields are the single
  // authored pin for these bytes, and production `--check` already enforces them against the same
  // render via the span validator (E-SOURCE-DIGEST / E-SPAN-COVERAGE).
  assert(
    projection.byteLength === document.document.frozen_source_byte_length &&
      sha256(projection) === document.document.frozen_source_sha256,
    "rendered testing legacy projection escaped its frozen length/digest",
  );
  memoizedLegacyProjection = projection;
  return new Uint8Array(memoizedLegacyProjection);
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}
