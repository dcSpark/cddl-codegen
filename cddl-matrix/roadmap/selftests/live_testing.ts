// @ts-expect-error Bun's text loader supports the live Markdown pickup; TypeScript has no .md declaration.
import liveTestingProjectionText from "../../../tests/TESTING_ROADMAP.md" with { type: "text" };
import liveTestingSourceText from "../../../tests/testing-roadmap.toml" with { type: "text" };
import { composeRoadmapDocument } from "../compose.ts";
import { decodeRoadmapSource } from "../decode/roadmap.ts";
import type { RepoPath } from "../model/core.ts";
import type {
  RawFragmentV0,
  RawLegacyMarkerV0,
  RawPartV0,
  RawRecordV0,
  RawSectionV0,
  RoadmapDocumentV0,
  RoadmapDocumentV1,
} from "../model/documents.ts";

const UTF8 = new TextEncoder();
const TESTING_SOURCE_PATH = "tests/testing-roadmap.toml" as RepoPath;
const TESTING_PROJECTION_PATH = "tests/TESTING_ROADMAP.md" as RepoPath;

function assert(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

export function liveTestingAuthoritativeDocument(): RoadmapDocumentV1 {
  const decoded = decodeRoadmapSource(
    UTF8.encode(liveTestingSourceText),
    TESTING_SOURCE_PATH,
    "testing",
  );
  assert(
    decoded.document.schema_version === 1 && decoded.document.authority === "authoritative",
    "committed testing self-test source is not authoritative schema v1",
  );
  assert(
    decoded.document.source_path === TESTING_SOURCE_PATH &&
      decoded.document.projection_path === TESTING_PROJECTION_PATH,
    "committed testing self-test source does not declare the live production paths",
  );
  return decoded as RoadmapDocumentV1;
}

export function liveTestingAuthoritativeSource(): Uint8Array {
  liveTestingAuthoritativeDocument();
  return UTF8.encode(liveTestingSourceText);
}

export function liveTestingProjection(): Uint8Array {
  return UTF8.encode(liveTestingProjectionText);
}

export function liveTestingShadowV0Document(): RoadmapDocumentV0 {
  const authoritative = liveTestingAuthoritativeDocument();
  const sections: RawSectionV0[] = authoritative.sections.map((owner) => {
    assert(owner.render_authority === "raw", `live testing section ${owner.section_id} is not raw`);
    const { render_authority: _renderAuthority, ...raw } = owner;
    return raw;
  });
  const fragments: RawFragmentV0[] = authoritative.fragments.map((owner) => {
    assert(owner.render_authority === "raw", `live testing fragment ${owner.fragment_id} is not raw`);
    const { render_authority: _renderAuthority, ...raw } = owner;
    return raw;
  });
  const legacyMarkers: RawLegacyMarkerV0[] = authoritative.legacy_markers.map((owner) => {
    assert(owner.render_authority === "raw", `live testing marker ${owner.marker_id} is not raw`);
    const { render_authority: _renderAuthority, ...raw } = owner;
    return raw;
  });
  const records: RawRecordV0[] = authoritative.records.map((owner) => {
    assert(owner.render_authority === "raw", `live testing record ${owner.id} is not raw`);
    const {
      render_authority: _renderAuthority,
      semantic_shadow: _semanticShadow,
      ...raw
    } = owner;
    return raw;
  });
  const parts: RawPartV0[] = authoritative.parts.map((owner) => {
    assert(owner.render_authority === "raw", `live testing part ${owner.part_id} is not raw`);
    const { render_authority: _renderAuthority, ...raw } = owner;
    return raw;
  });
  const { frozen_legacy_span_ids: _frozenLegacySpanIds, ...frozenSource } =
    authoritative.document;
  const shadow: RoadmapDocumentV0 = {
    document: {
      ...frozenSource,
      schema_version: 0,
      authority: "shadow",
    },
    sections,
    fragments,
    legacy_markers: legacyMarkers,
    records,
    parts,
    generated_slots: authoritative.generated_slots,
    manifest: authoritative.manifest,
    spans: authoritative.spans,
  };
  const canonical = composeRoadmapDocument(shadow);
  const roundTrip = decodeRoadmapSource(canonical, TESTING_SOURCE_PATH, "testing");
  assert(roundTrip.document.schema_version === 0, "derived testing shadow did not round-trip as v0");
  return shadow;
}

export function liveTestingShadowV0Source(): Uint8Array {
  return composeRoadmapDocument(liveTestingShadowV0Document());
}
