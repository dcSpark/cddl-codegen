// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveTestingProjectionText from "../../../tests/TESTING_ROADMAP.md" with { type: "text" };
import liveTestingSourceText from "../../../tests/testing-roadmap.toml" with { type: "text" };
import { TESTING_ADAPTER } from "../adapters/testing.ts";
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import { resolveManifest } from "../manifest.ts";
import type { RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";
import { renderValidatedChunks } from "../render.ts";
import { buildExpectedChunks, validateCompletedChunks } from "../render_ir.ts";

const UTF8 = new TextEncoder();
const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveTestingV2Document(): RoadmapDocumentV2 {
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
  return decoded as RoadmapDocumentV2;
}

export function liveTestingLegacyV2Document(): RoadmapDocumentV2 {
  const decoded = liveTestingV2Document();
  return {
    ...decoded,
    document: { ...decoded.document, projection_layout: "legacy_v1" },
    references: decoded.references.map((reference) => reference.kind === "file_heading" &&
        reference.path === TESTING_PROJECTION_PATH && reference.heading === "Next work"
      ? { ...reference, heading: "Next work items, in priority order" }
      : reference),
  };
}

export function liveTestingAuthoritativeSource(): Uint8Array {
  liveTestingV2Document();
  return UTF8.encode(liveTestingSourceText);
}

export function liveTestingProjection(): Uint8Array {
  return UTF8.encode(liveTestingProjectionText);
}

/** The pre-anchor-layout projection, rendered from the live source without the anchor layout. */
export function liveTestingLegacyProjection(): Uint8Array {
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
  assert(
    projection.byteLength === 306_388 &&
      sha256(projection) === "6e90f1fb06011cefa546d861da0a6525ff1af6fc81bbe51c9ed5f035578b53af",
    "rendered testing legacy projection escaped its frozen length/digest",
  );
  return projection;
}

function sha256(value: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(value).digest("hex");
}
