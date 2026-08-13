import type {
  FullCommitId,
  FragmentId,
  LowercaseSlug,
  MarkerId,
  PartId,
  ReferenceId,
  RepoPath,
  RoadmapId,
  RoadmapName,
  SectionId,
  SharedSemanticPayload,
  SlotId,
  SpanId,
  WorkKind,
} from "./core.ts";
import type { MatrixSemanticPayload } from "./matrix.ts";
import type { FamilyPayload } from "./systematic.ts";
import type { TestingSemanticPayload } from "./testing.ts";

export type SemanticPayload =
  | SharedSemanticPayload
  | FamilyPayload
  | MatrixSemanticPayload
  | TestingSemanticPayload;

export interface FrozenSourceMeta {
  source_path: RepoPath;
  projection_path: RepoPath;
  frozen_source_sha256: string;
  frozen_source_byte_length: number;
  frozen_source_line_count: number;
  frozen_source_eof: "lf" | "none";
}

export interface DocumentMetaV0 extends FrozenSourceMeta {
  schema_version: 0;
  authority: "shadow";
  roadmap: RoadmapName;
}

export interface DocumentMetaV1 extends FrozenSourceMeta {
  schema_version: 1;
  authority: "authoritative";
  roadmap: RoadmapName;
  /** Historical v1 commit sources may omit; current worktree sources must declare it. */
  semantic_conversion?: "converting" | "complete";
  frozen_legacy_span_ids: SpanId[];
}

/** Stable semantic wire format. Migration state is intrinsic and has no authored escape hatch. */
export interface DocumentMetaV2 extends FrozenSourceMeta {
  schema_version: 2;
  authority: "authoritative";
  roadmap: RoadmapName;
  /** Historical WP6 v2 sources omit this and retain the byte-identical legacy projection. */
  projection_layout?: "legacy_v1" | "anchors_v1" | "standing_v1" | "unnumbered_v1" | "curated_v1";
}

interface RawOwner {
  source_block_md: Uint8Array;
  span_ids: SpanId[];
}

export interface RawSectionV0 extends RawOwner {
  section_id: SectionId;
  title: string;
  legacy_aliases?: string[];
}

export interface RawSectionV1 extends RawSectionV0 {
  render_authority: "raw";
}

export interface SemanticSectionV1 {
  section_id: SectionId;
  title: string;
  legacy_aliases?: string[];
  render_authority: "semantic";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export interface RawFragmentV0 extends RawOwner {
  fragment_id: FragmentId;
  projection_group: SectionId;
  title?: string;
  legacy_aliases?: string[];
}

export interface RawFragmentV1 extends RawFragmentV0 {
  render_authority: "raw";
  lifecycle_disposition?: "pending_review" | "document_prose" | "independent_record";
}

export interface SemanticFragmentV1 {
  fragment_id: FragmentId;
  projection_group: SectionId;
  title?: string;
  legacy_aliases?: string[];
  render_authority: "semantic";
  lifecycle_disposition: "document_prose";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export interface RawLegacyMarkerV0 extends RawOwner {
  marker_id: MarkerId;
  legacy_aliases: string[];
}

export interface RawLegacyMarkerV1 extends RawLegacyMarkerV0 {
  render_authority: "raw";
}

export interface SemanticLegacyMarkerV1 {
  marker_id: MarkerId;
  legacy_aliases: string[];
  render_authority: "semantic";
  marker_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export interface CommonEnvelope {
  id: RoadmapId;
  title: string;
  projection_group: SectionId;
  legacy_aliases?: string[];
  tags?: string[];
}

export interface RawRecordV0 extends CommonEnvelope, RawOwner {}

export interface RawAuthorityRecordV1 extends CommonEnvelope, RawOwner {
  render_authority: "raw";
  semantic_shadow?: SemanticPayload;
}

export interface SourceReplacement {
  span_id: SpanId;
  replacement_field: string;
  review_note_md: Uint8Array;
}

export interface SemanticAuthorityRecordV1<P extends SemanticPayload = SemanticPayload>
  extends CommonEnvelope {
  render_authority: "semantic";
  projection_visibility: "document" | "semantic_only";
  payload: P;
  source_replacements: SourceReplacement[];
}

export interface RawPartV0 extends RawOwner {
  part_id: PartId;
  parent_record_id: RoadmapId;
  title?: string;
}

export interface RawPartV1 extends RawPartV0 {
  render_authority: "raw";
  lifecycle_disposition?: "pending_review" | "parent_supporting_prose" | "independent_record";
}

export interface SemanticPartV1 {
  part_id: PartId;
  parent_record_id: RoadmapId;
  title?: string;
  render_authority: "semantic";
  lifecycle_disposition: "parent_supporting_prose";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export type RawSection = RawSectionV0 | RawSectionV1;
export type SemanticSection = SemanticSectionV1;
export type Section = RawSection | SemanticSection;
export type RawFragment = RawFragmentV0 | RawFragmentV1;
export type SemanticFragment = SemanticFragmentV1;
export type Fragment = RawFragment | SemanticFragment;
export type RawLegacyMarker = RawLegacyMarkerV0 | RawLegacyMarkerV1;
export type SemanticLegacyMarker = SemanticLegacyMarkerV1;
export type LegacyMarker = RawLegacyMarker | SemanticLegacyMarker;
export type RecordNode = RawRecordV0 | RawAuthorityRecordV1 | SemanticAuthorityRecordV1;
export type RawPart = RawPartV0 | RawPartV1;
export type RawAuthorityPartV1 = RawPartV1;
export type SemanticPart = SemanticPartV1;
export type SemanticAuthorityPartV1 = SemanticPartV1;
export type Part = RawPart | SemanticPart;
export type SemanticRecord<P extends SemanticPayload = SemanticPayload> =
  SemanticAuthorityRecordV1<P>;

export interface GeneratedSlot {
  slot_id: SlotId;
  binding: string;
  span_ids: SpanId[];
}

export type ManifestEntry =
  | { kind: "section"; section_id: SectionId }
  | { kind: "fragment"; fragment_id: FragmentId }
  | { kind: "legacy_marker"; marker_id: MarkerId }
  | { kind: "record"; record_id: RoadmapId }
  | { kind: "part"; part_id: PartId }
  | { kind: "generated_slot"; slot_id: SlotId };

export interface SourceSpan {
  id: SpanId;
  start_byte: number;
  end_byte: number;
  sha256: string;
  source_kind: ManifestEntry["kind"];
  owner_id: string;
  owner_field: string;
  migration_status: "raw" | "replaced" | "generated";
}

export type RelationKind =
  | "parent_of"
  | "depends_on"
  | "blocked_by"
  | "supersedes"
  | "split_from"
  | "reopens"
  | "overlaps"
  | "complements"
  | "related"
  | "delegates_to";

export interface Relation {
  source: RoadmapId;
  kind: RelationKind;
  target: RoadmapId;
  note_md?: Uint8Array;
}

interface ReferenceBase {
  id: ReferenceId;
  source: RoadmapId;
}

export type Reference =
  | (ReferenceBase & { kind: "roadmap"; target_id: RoadmapId })
  | (ReferenceBase & { kind: "matrix_feature"; feature_id: string })
  | (ReferenceBase & { kind: "matrix_role"; role_id: string })
  | (ReferenceBase & { kind: "matrix_cell"; cell_id: string })
  | (ReferenceBase & { kind: "gate"; gate_id: string })
  | (ReferenceBase & { kind: "test_symbol"; test_id: string; symbol: string })
  | (ReferenceBase & { kind: "file_heading"; path: RepoPath; heading: string })
  | (ReferenceBase & { kind: "spec_passage"; document: string; passage: string })
  | (ReferenceBase & { kind: "external_issue"; repository: string; issue: string })
  | (ReferenceBase & { kind: "external_commit"; repository: string; commit: string })
  | (ReferenceBase & { kind: "external_release"; project: string; release: string })
  | (ReferenceBase & { kind: "consumer_report"; consumer: string; report_reference: string })
  | (ReferenceBase & {
      kind: "unresolved_migration";
      local_reference: string;
      uncertainty_md: Uint8Array;
      expires_at: string;
    });

/** References permitted after migration state becomes intrinsic in schema v2. */
export type StableReference = Exclude<Reference, { kind: "unresolved_migration" }>;

export interface RoadmapDocumentV0 {
  document: DocumentMetaV0;
  sections: RawSectionV0[];
  fragments: RawFragmentV0[];
  legacy_markers: RawLegacyMarkerV0[];
  records: RawRecordV0[];
  parts: RawPartV0[];
  generated_slots: GeneratedSlot[];
  manifest: ManifestEntry[];
  spans: SourceSpan[];
}

export interface RoadmapDocumentV1 {
  document: DocumentMetaV1;
  sections: (RawSectionV1 | SemanticSectionV1)[];
  fragments: (RawFragmentV1 | SemanticFragmentV1)[];
  legacy_markers: (RawLegacyMarkerV1 | SemanticLegacyMarkerV1)[];
  records: (RawAuthorityRecordV1 | SemanticAuthorityRecordV1)[];
  parts: (RawPartV1 | SemanticPartV1)[];
  generated_slots: GeneratedSlot[];
  manifest: ManifestEntry[];
  spans: SourceSpan[];
  relations: Relation[];
  references: Reference[];
}

export interface RoadmapDocumentV2 {
  document: DocumentMetaV2;
  sections: SemanticSectionV1[];
  fragments: SemanticFragmentV1[];
  legacy_markers: SemanticLegacyMarkerV1[];
  records: SemanticAuthorityRecordV1[];
  parts: SemanticPartV1[];
  generated_slots: GeneratedSlot[];
  manifest: ManifestEntry[];
  spans: SourceSpan[];
  relations: Relation[];
  references: StableReference[];
}

export type AuthoritativeRoadmapDocument = RoadmapDocumentV1 | RoadmapDocumentV2;
export type RoadmapDocument = RoadmapDocumentV0 | AuthoritativeRoadmapDocument;
export type RoadmapAuthorityState = "legacy_markdown" | "shadow" | "authoritative";

export interface CampaignDocumentV1 {
  campaign: {
    schema_version: 1;
    matrix_authority: RoadmapAuthorityState;
    testing_authority: RoadmapAuthorityState;
  };
  legacy_markdown_reservations: LegacyMarkdownReservationV1[];
  selections: CampaignSelectionV1[];
}

export interface LegacyMarkdownReservationV1 {
  id: RoadmapId;
  work_kind: WorkKind;
  roadmap_path: "cddl-matrix/ROADMAP.md" | "tests/TESTING_ROADMAP.md";
  source_title: string;
  source_start_byte: number;
  source_end_byte: number;
  source_sha256: string;
  whole_source_sha256: string;
}

export interface CampaignSelectionV1 {
  item_id: RoadmapId;
  target_kind: "active_id" | "legacy_markdown_reservation";
  selected_state: "selected" | "in_progress";
  priority_class: LowercaseSlug;
  selection_reason_md: Uint8Array;
  cycle: LowercaseSlug;
  remaining_scope_md: Uint8Array;
  assignee?: string;
  pickup_commit?: FullCommitId;
  cost_bound?: CampaignSelectionCostBoundV1;
}

export interface CampaignSelectionCostBoundV1 {
  posture: "reviewed_scope";
  implementation_units: LowercaseSlug[];
  validation_units: LowercaseSlug[];
  assumption_md: Uint8Array;
}

export interface RetiredIdsDocumentV1 {
  retired_ids: { schema_version: 1 };
  entries: RetiredIdV1[];
}

export interface RetiredIdV1 {
  id: RoadmapId;
  last_active_at: FullCommitId;
  replacement: ReplacementPin;
}

export type ReplacementPin =
  | { kind: "gate"; gate_id: string; claim_md: Uint8Array }
  | { kind: "test_symbol"; test_id: string; symbol: string; claim_md: Uint8Array }
  | { kind: "file_heading"; path: RepoPath; heading: string; claim_md: Uint8Array };

export type FamilyGuardRole =
  | "closed_family_root"
  | "family_axis"
  | "family_axis_value"
  | "family_evidence_requirement"
  | "family_cell"
  | "family_exclusion";

interface CurrentGuardBase {
  id: RoadmapId;
  replacement_pin: ReplacementPin;
  owner_registry: string;
}

/** Durable typed guard for a delivered systematic-family provider. */
export interface CurrentFamilyGuard extends CurrentGuardBase {
  guard_role: FamilyGuardRole;
  family_root_id: RoadmapId;
}

/** Existing non-family guard registries remain structurally distinct. */
export interface CurrentGenericGuard extends CurrentGuardBase {
  guard_role: "generic";
  family_root_id?: never;
}

export type CurrentGuard = CurrentFamilyGuard | CurrentGenericGuard;

/** Reviewed delivered-closure facts derived from the exact baseline and current live authority. */
export interface FixedValueClosureAuthorityFact {
  baseline_commit: FullCommitId;
  expected_guards: readonly { readonly id: RoadmapId; readonly guard_role: FamilyGuardRole }[];
  retained_evidence_ids: readonly RoadmapId[];
  legal_cell_count: number;
  evidence_coordinate_count: number;
}

export interface ActiveRecordOwnerFact {
  owner_kind: "active_record";
  id: RoadmapId;
  namespace: RoadmapName;
  record: RawAuthorityRecordV1 | SemanticAuthorityRecordV1;
}

export interface LegacyMarkdownOwnerFact {
  owner_kind: "legacy_markdown_reservation";
  id: RoadmapId;
  namespace: RoadmapName;
  work_kind: WorkKind;
  reservation: LegacyMarkdownReservationV1;
  corroborating_shadow?: ShadowRecordClaim;
}

export interface ShadowRecordOwnerFact {
  owner_kind: "shadow_record_reservation";
  id: RoadmapId;
  namespace: RoadmapName;
  claim: ShadowRecordClaim;
}

export interface CurrentGuardOwnerFact {
  owner_kind: "current_guard";
  id: RoadmapId;
  namespace: RoadmapName;
  guard: CurrentGuard;
}

export interface TombstoneOwnerFact {
  owner_kind: "tombstone";
  id: RoadmapId;
  namespace: RoadmapName;
  tombstone: RetiredIdV1;
}

export type IdentityOwnerFact =
  | ActiveRecordOwnerFact
  | LegacyMarkdownOwnerFact
  | ShadowRecordOwnerFact
  | CurrentGuardOwnerFact
  | TombstoneOwnerFact;

export interface ShadowRecordClaim {
  id: RoadmapId;
  namespace: RoadmapName;
  source_path: RepoPath;
  logical_path: string;
  legacy_span_ids: readonly SpanId[];
}

export type TombstoneEligibleBaseOwner =
  | "active_record"
  | "current_guard"
  | "legacy_markdown_reservation";
