// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveMatrixProjectionText from "../../ROADMAP.md" with { type: "text" };
import liveMatrixSourceText from "../../roadmap.toml" with { type: "text" };
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";

const UTF8 = new TextEncoder();
const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveMatrixV2Document(): RoadmapDocumentV2 {
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveMatrixSourceText),
    MATRIX_SOURCE_PATH,
    "matrix",
  );
  assert(
    decoded.document.schema_version === 2 && decoded.document.authority === "authoritative",
    "committed matrix self-test source is not authoritative schema v2",
  );
  assert(
    decoded.document.source_path === MATRIX_SOURCE_PATH &&
      decoded.document.projection_path === MATRIX_PROJECTION_PATH,
    "committed matrix self-test source does not declare the live production paths",
  );
  return decoded as RoadmapDocumentV2;
}

export function liveMatrixLegacyV2Document(): RoadmapDocumentV2 {
  const decoded = liveMatrixV2Document();
  return { ...decoded, document: { ...decoded.document, projection_layout: "legacy_v1" } };
}

export function liveMatrixAuthoritativeSource(): Uint8Array {
  liveMatrixV2Document();
  return UTF8.encode(liveMatrixSourceText);
}

export function liveMatrixProjection(): Uint8Array {
  return UTF8.encode(liveMatrixProjectionText);
}

/** Current authored bytes with only the ownership/anchor layout removed. */
export function liveMatrixCurrentLegacyProjection(): Uint8Array {
  return UTF8.encode(liveMatrixProjectionText
    .replace(
      /^<!-- GENERATED FILE: owned by cddl-matrix\/roadmap\.toml; edit that TOML source and run project_roadmaps\.ts --write\. -->\n\n/u,
      "",
    )
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n\n(?= *#)/gmu, "")
    .replace(/^ *<a id="roadmap-id-[^"]+"><\/a>\n/gmu, ""));
}
