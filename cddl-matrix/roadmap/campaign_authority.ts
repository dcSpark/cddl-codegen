import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapAuthorityState } from "./model/documents.ts";
import type { CampaignDocumentV1 } from "./model/documents.ts";

const AUTHORITY_RANK: Readonly<Record<RoadmapAuthorityState, number>> = Object.freeze({
  legacy_markdown: 0,
  shadow: 1,
  authoritative: 2,
});

export function campaignAuthorityRank(authority: RoadmapAuthorityState): number {
  return AUTHORITY_RANK[authority];
}

/** Testing cannot advance past the matrix roadmap that defines the shared campaign lifecycle. */
export function campaignAuthorityTupleIsReachable(
  matrixAuthority: RoadmapAuthorityState,
  testingAuthority: RoadmapAuthorityState,
): boolean {
  return campaignAuthorityRank(matrixAuthority) >= campaignAuthorityRank(testingAuthority);
}

/** Apply the decoded campaign's authority-order invariant to programmatic documents too. */
export function validateCampaignAuthorityTuple(
  campaign: CampaignDocumentV1["campaign"],
): readonly RoadmapIssue[] {
  if (campaignAuthorityTupleIsReachable(
    campaign.matrix_authority,
    campaign.testing_authority,
  )) return [];
  return Object.freeze([{
    code: "E-SCHEMA-STATE",
    source: "roadmap-campaign.toml",
    logical_path: "campaign.testing_authority",
    message: "testing authority cannot advance past matrix authority",
    exit: 1,
  }]);
}
