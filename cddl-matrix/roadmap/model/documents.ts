import type {
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
import type { TestingSemanticPayload } from "./testing.ts";

export type SemanticPayload =
  | SharedSemanticPayload
  | MatrixSemanticPayload
  | TestingSemanticPayload;

/** Stable v3 wire format: one schema version, no migration bookkeeping. */
export interface DocumentMetaV3 {
  schema_version: 3;
  roadmap: RoadmapName;
  source_path: RepoPath;
  projection_path: RepoPath;
}

/**
 * A generated span the section's prose interleaves. The declaration owns the binding; the prose
 * owns the position through a `{{slot:<id>}}` placeholder. Resolution starts from the declaration
 * list — prose is never scanned to discover a slot — and the two must be a bijection.
 */
export interface GeneratedSlot {
  slot_id: SlotId;
  binding: string;
}

/**
 * A section owns its presentation order. `entries` is the ordered list of record and part IDs the
 * section renders after its own prose, and it is the sole placement authority — membership is
 * TOTAL, so every record carrying renderable prose appears in exactly one section's entries and a
 * record without it appears in none. Both directions are validation errors, so neither accidental
 * orphaning nor a placed non-rendering record is a silent state.
 */
export interface SemanticSection {
  section_id: SectionId;
  title: string;
  legacy_aliases?: string[];
  body_md: Uint8Array;
  entries: readonly string[];
  /** Declared slots, sorted by `slot_id`; absent when the section's prose interleaves nothing. */
  slots?: readonly GeneratedSlot[];
}

export interface CommonEnvelope {
  id: RoadmapId;
  title: string;
  legacy_aliases?: string[];
  tags?: string[];
}

/**
 * A record renders exactly when `payload.detail_md` is present (its one rendering field) and it is
 * listed in a section's `entries`. The section-plan resolver enforces both directions.
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
export type RecordNode = SemanticAuthorityRecord;
export type Part = SemanticPart;
export type SemanticRecord<P extends SemanticPayload = SemanticPayload> =
  SemanticAuthorityRecord<P>;

/** The three node kinds a rendered projection is built from. */
export type RenderNodeKind = "section" | "record" | "part";

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
  records: SemanticAuthorityRecord[];
  parts: SemanticPart[];
  relations: Relation[];
  references: Reference[];
}

export type RoadmapDocument = RoadmapDocumentV3;

export type ReplacementPin =
  | { kind: "gate"; gate_id: string; claim_md: Uint8Array }
  | { kind: "test_symbol"; test_id: string; symbol: string; claim_md: Uint8Array }
  | { kind: "file_heading"; path: RepoPath; heading: string; claim_md: Uint8Array };

interface CurrentGuardBase {
  id: RoadmapId;
  replacement_pin: ReplacementPin;
  owner_registry: string;
}

/** A retired provider's durable guard: the ID is claimed, and its replacement is pinned. */
export interface CurrentGuard extends CurrentGuardBase {
  guard_role: "generic";
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
