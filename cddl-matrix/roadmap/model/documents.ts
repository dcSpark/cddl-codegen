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

/** Stable semantic wire format. Migration state is intrinsic and has no authored escape hatch. */
export interface DocumentMetaV2 extends FrozenSourceMeta {
  schema_version: 2;
  authority: "authoritative";
  roadmap: RoadmapName;
  /** Historical v2 sources omit this and retain the byte-identical legacy projection. */
  projection_layout?: "legacy_v1" | "anchors_v1" | "standing_v1" | "unnumbered_v1" | "curated_v1";
}

export interface SemanticSection {
  section_id: SectionId;
  title: string;
  legacy_aliases?: string[];
  render_authority: "semantic";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export interface SemanticFragment {
  fragment_id: FragmentId;
  projection_group: SectionId;
  title?: string;
  legacy_aliases?: string[];
  render_authority: "semantic";
  lifecycle_disposition: "document_prose";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export interface SemanticLegacyMarker {
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

export interface SourceReplacement {
  span_id: SpanId;
  replacement_field: string;
  review_note_md: Uint8Array;
}

export interface SemanticAuthorityRecord<P extends SemanticPayload = SemanticPayload>
  extends CommonEnvelope {
  render_authority: "semantic";
  projection_visibility: "document" | "semantic_only";
  payload: P;
  source_replacements: SourceReplacement[];
}

export interface SemanticPart {
  part_id: PartId;
  parent_record_id: RoadmapId;
  title?: string;
  render_authority: "semantic";
  lifecycle_disposition: "parent_supporting_prose";
  body_md: Uint8Array;
  source_replacements: SourceReplacement[];
}

export type Section = SemanticSection;
export type Fragment = SemanticFragment;
export type LegacyMarker = SemanticLegacyMarker;
export type RecordNode = SemanticAuthorityRecord;
export type Part = SemanticPart;
export type SemanticRecord<P extends SemanticPayload = SemanticPayload> =
  SemanticAuthorityRecord<P>;

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
  | (ReferenceBase & { kind: "consumer_report"; consumer: string; report_reference: string });

export interface RoadmapDocumentV2 {
  document: DocumentMetaV2;
  sections: SemanticSection[];
  fragments: SemanticFragment[];
  legacy_markers: SemanticLegacyMarker[];
  records: SemanticAuthorityRecord[];
  parts: SemanticPart[];
  generated_slots: GeneratedSlot[];
  manifest: ManifestEntry[];
  spans: SourceSpan[];
  relations: Relation[];
  references: Reference[];
}

export type RoadmapDocument = RoadmapDocumentV2;

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
  record: SemanticAuthorityRecord;
}

export interface CurrentGuardOwnerFact {
  owner_kind: "current_guard";
  id: RoadmapId;
  namespace: RoadmapName;
  guard: CurrentGuard;
}

export type IdentityOwnerFact = ActiveRecordOwnerFact | CurrentGuardOwnerFact;
