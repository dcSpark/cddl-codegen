import type {
  FragmentId,
  PartId,
  ReferenceId,
  RepoPath,
  RoadmapId,
  RoadmapName,
  SectionId,
  SharedSemanticPayload,
  SlotId,
} from "./core.ts";
import type { MatrixSemanticPayload } from "./matrix.ts";
import type { FamilyPayload } from "./systematic.ts";
import type { TestingSemanticPayload } from "./testing.ts";

export type SemanticPayload =
  | SharedSemanticPayload
  | FamilyPayload
  | MatrixSemanticPayload
  | TestingSemanticPayload;

/** Stable v3 wire format: one schema version, no migration bookkeeping. */
export interface DocumentMetaV3 {
  schema_version: 3;
  roadmap: RoadmapName;
  source_path: RepoPath;
  projection_path: RepoPath;
}

export interface SemanticSection {
  section_id: SectionId;
  title: string;
  legacy_aliases?: string[];
  body_md: Uint8Array;
}

export interface SemanticFragment {
  fragment_id: FragmentId;
  projection_group: SectionId;
  title?: string;
  legacy_aliases?: string[];
  body_md: Uint8Array;
}

export interface CommonEnvelope {
  id: RoadmapId;
  title: string;
  projection_group: SectionId;
  legacy_aliases?: string[];
  tags?: string[];
}

/**
 * Document membership is derived, never declared: a record renders exactly when it is placed in
 * the manifest, and placement is legal exactly when `payload.detail_md` is present (the one
 * rendering field). The manifest resolver enforces both directions.
 */
export interface SemanticAuthorityRecord<P extends SemanticPayload = SemanticPayload>
  extends CommonEnvelope {
  payload: P;
}

export interface SemanticPart {
  part_id: PartId;
  parent_record_id: RoadmapId;
  title?: string;
  body_md: Uint8Array;
}

export type Section = SemanticSection;
export type Fragment = SemanticFragment;
export type RecordNode = SemanticAuthorityRecord;
export type Part = SemanticPart;
export type SemanticRecord<P extends SemanticPayload = SemanticPayload> =
  SemanticAuthorityRecord<P>;

export interface GeneratedSlot {
  slot_id: SlotId;
  binding: string;
}

export type ManifestEntry =
  | { kind: "section"; section_id: SectionId }
  | { kind: "fragment"; fragment_id: FragmentId }
  | { kind: "record"; record_id: RoadmapId }
  | { kind: "part"; part_id: PartId }
  | { kind: "generated_slot"; slot_id: SlotId };

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

export interface RoadmapDocumentV3 {
  document: DocumentMetaV3;
  sections: SemanticSection[];
  fragments: SemanticFragment[];
  records: SemanticAuthorityRecord[];
  parts: SemanticPart[];
  generated_slots: GeneratedSlot[];
  manifest: ManifestEntry[];
  relations: Relation[];
  references: Reference[];
}

export type RoadmapDocument = RoadmapDocumentV3;

export type FamilyGuardRole =
  | "closed_family_root"
  | "family_axis"
  | "family_axis_value"
  | "family_evidence_requirement"
  | "family_cell"
  | "family_exclusion";

export type ReplacementPin =
  | { kind: "gate"; gate_id: string; claim_md: Uint8Array }
  | { kind: "test_symbol"; test_id: string; symbol: string; claim_md: Uint8Array }
  | { kind: "file_heading"; path: RepoPath; heading: string; claim_md: Uint8Array };

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
