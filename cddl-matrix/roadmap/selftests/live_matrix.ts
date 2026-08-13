// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveMatrixProjectionText from "../../ROADMAP.md" with { type: "text" };
import liveMatrixSourceText from "../../roadmap.toml" with { type: "text" };
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { RepoPath } from "../model/core.ts";
import type { RoadmapDocumentV2 } from "../model/documents.ts";
import { deepFreeze } from "./frozen.ts";

const UTF8 = new TextEncoder();
const MATRIX_SOURCE_PATH = "cddl-matrix/roadmap.toml" as RepoPath;
const MATRIX_PROJECTION_PATH = "cddl-matrix/ROADMAP.md" as RepoPath;
function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

/**
 * Memoized once per process: the input is the imported committed source text, content-stable
 * within a run, so re-decoding (which also re-composes to prove byte-canonicality) on every call
 * only re-derives the same value. The shared document is deep-frozen so accidental in-place
 * mutation by one case fails loudly instead of leaking into later cases.
 */
let memoizedMatrixDocument: RoadmapDocumentV2 | undefined;

export function liveMatrixV2Document(): RoadmapDocumentV2 {
  if (memoizedMatrixDocument !== undefined) return memoizedMatrixDocument;
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
  memoizedMatrixDocument = deepFreeze(decoded as RoadmapDocumentV2);
  return memoizedMatrixDocument;
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
