import type { IssueCollector } from "../errors.ts";
import type {
  FullCommitId,
  RepoPath,
  RepositoryRevision,
  RoadmapId,
  RoadmapName,
  SlotId,
} from "../model/core.ts";
import type {
  CurrentGuard,
  GeneratedSlot,
  Reference,
  RoadmapDocument,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import type { MatrixStatusInputs } from "../model/matrix.ts";

export interface Indexes {
  readonly records: ReadonlyMap<RoadmapId, SemanticRecord>;
  readonly references: ReadonlyMap<string, Reference>;
}

export interface FieldConsumer {
  consume(logical_path: string, bytes: Uint8Array): Uint8Array;
}

export type Resolution =
  | { resolved: true; provider: string }
  | { resolved: false; reason: string };

export interface GeneratedSlotResolution {
  binding: string;
  bytes: Uint8Array;
}

export interface GeneratedSlotResolver {
  resolve(slot: GeneratedSlot, view: RegistryView): GeneratedSlotResolution;
}

export interface GateFact {
  id: string;
  kind: string;
  stub: boolean;
}

export interface MatrixFeatureFact { id: string }
export interface MatrixRoleFact { id: string }
export interface MatrixCellFact { id: string }

export interface FileHeadingFact {
  path: RepoPath;
  heading: string;
  span: { start_byte: number; end_byte: number };
}

export interface TestSymbolFact {
  test_id: string;
  symbol: string;
  source: RepoPath;
  span: { start_byte: number; end_byte: number };
  module_path: readonly string[];
}

export interface RoadmapCitationFact {
  id: RoadmapId;
  source: RepoPath;
  span: { start_byte: number; end_byte: number };
  raw: string;
}

export type OutputClaim =
  | {
      kind: "whole_file";
      producer: string;
      path: RepoPath;
      interval: { kind: "whole_file" };
    }
  | {
      kind: "slot";
      producer: string;
      path: RepoPath;
      slot_id: SlotId;
      interval: {
        kind: "binding";
        binding:
          | { kind: "status_header_markers"; marker_id: SlotId }
          | { kind: "manifest_generated_slot"; roadmap: RoadmapName; slot_id: SlotId };
        cardinality: { exact: 1 };
      };
    };

export interface ByteInterval {
  start_byte: number;
  end_byte: number;
}

export interface ResolvedOutputClaim {
  claim: OutputClaim;
  path: RepoPath;
  interval: ByteInterval;
  payload_interval: ByteInterval;
}

export interface RegistryView {
  readonly revision: RepositoryRevision;
  readonly gates: readonly GateFact[];
  readonly matrix_features: readonly MatrixFeatureFact[];
  readonly matrix_roles: readonly MatrixRoleFact[];
  readonly matrix_cells: readonly MatrixCellFact[];
  readonly tracked_headings: readonly FileHeadingFact[];
  readonly test_symbols: readonly TestSymbolFact[];
  readonly roadmap_citations: readonly RoadmapCitationFact[];
  readonly current_guards: readonly CurrentGuard[];
  readonly output_claims: readonly OutputClaim[];
  readonly matrix_status_inputs: MatrixStatusInputs;
}

export interface ReferenceProvider<K extends Reference["kind"] = Reference["kind"]> {
  readonly kind: K;
  resolve(ref: Extract<Reference, { kind: K }>, view: RegistryView): Resolution;
}

export interface RoadmapAdapter<P extends SemanticPayload> {
  readonly roadmap: RoadmapName;
  readonly namespace: RoadmapName;
  readonly source_path: RepoPath;
  readonly projection_path: RepoPath;
  validateExtension(record: SemanticRecord<P>, indexes: Indexes, out: IssueCollector): void;
  renderSemantic(record: SemanticRecord<P>, fields: FieldConsumer): Uint8Array;
  referenceProviders(view: RegistryView): ReferenceProvider[];
  slotResolvers(view: RegistryView): ReadonlyMap<SlotId, GeneratedSlotResolver>;
  validateFloors(doc: RoadmapDocument, out: IssueCollector): void;
}

export interface RevisionRepositoryFacts {
  revision: RepositoryRevision;
  commit?: FullCommitId;
  registry: RegistryView;
}
