/**
 * The roadmap-agnostic validation and rendering engine.  Everything here is shared by every
 * adapter: the canonical Markdown field order a semantic record renders in, the payload-fact
 * helpers domain validators are built from, the structural floor check driven by an adapter's own
 * coordinates, and the one orchestration that turns a decoded document plus a registry view into
 * indexes and sorted issues.  Adapters supply the domain knowledge; this module supplies the
 * machinery, so no adapter needs to import a sibling adapter to reach it.
 */
import type { IssueCollector, RoadmapIssue } from "../errors.ts";
import { sortRoadmapIssues as sortIssues } from "../errors.ts";
import { buildRoadmapIndexes, type RoadmapIndexes, type SemanticPayloadProviderFact } from "../indexes.ts";
import { namespaceOf } from "../ids.ts";
import { concatenate } from "../kernel.ts";
import {
  armOfGroupValue,
  armOfPayload,
  fieldProperty,
  type PayloadArm,
} from "../payload_descriptors.ts";
import type { RepoPath, RoadmapId, RoadmapName, SlotId } from "../model/core.ts";
import type {
  RoadmapDocument,
  SemanticPayload,
  SemanticRecord,
} from "../model/documents.ts";
import {
  validateRoadmapReferences,
  validateSemanticRoadmapJoins,
  type ReferenceProviderLike,
  type SemanticJoinUniverse,
} from "../references.ts";
import { validateRelations } from "../relations.ts";
import type {
  FieldConsumer,
  Indexes,
  RegistryView,
  RoadmapAdapter,
} from "./types.ts";
import { documentSlots } from "../slots.ts";

/**
 * Domain payload-fact issue factory: the logical path is always the fact's own path plus the field
 * that failed, so a diagnostic points at the exact payload coordinate.
 */
export function payloadFactIssue(
  provider: SemanticPayloadProviderFact,
  source: string,
  code: "E-SCHEMA-STATE" | "E-SCHEMA-FLOOR",
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source,
    logical_path: `${provider.logical_path}.${logicalPath}`,
    message,
    exit: 1,
  };
}

function payloadAt(indexes: Indexes, id: RoadmapId): SemanticPayload | undefined {
  if ("payload_records" in indexes) {
    return (indexes as Indexes & Pick<RoadmapIndexes, "payload_records">).payload_records.get(id)?.payload;
  }
  return indexes.records.get(id)?.payload;
}

export function requirePayloadKind(
  provider: SemanticPayloadProviderFact,
  source: string,
  indexes: Indexes,
  id: RoadmapId,
  field: string,
  predicate: (payload: SemanticPayload) => boolean,
  expected: string,
  out: IssueCollector,
): void {
  const target = payloadAt(indexes, id);
  const deferred = (indexes as Indexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName })
    .deferred_foreign_roadmap_joins;
  const targetNamespace = namespaceOf(id);
  if (target === undefined && deferred !== undefined && targetNamespace !== undefined &&
    targetNamespace !== deferred) return;
  if (target === undefined || !predicate(target)) {
    out.add(payloadFactIssue(provider, source, "E-SCHEMA-STATE", field, `${id} must resolve to ${expected}`));
  }
}

export interface CanonicalSemanticMarkdownField {
  readonly logical_path: string;
  readonly bytes: Uint8Array;
}

/**
 * The canonical Markdown field order derives from the payload descriptor table: the arm's field
 * order filtered to Markdown fields, recursing into nested single tables (path-prefixed by the
 * wire key) and array tables (path-prefixed by the decoded property name plus index).
 */
export function canonicalSemanticMarkdownFields(
  value: SemanticPayload,
): readonly CanonicalSemanticMarkdownField[] {
  const fields: CanonicalSemanticMarkdownField[] = [];
  const walk = (obj: object, arm: PayloadArm, prefix: string): void => {
    for (const field of arm.fields) {
      const spec = field.value;
      const fieldValue = (obj as Record<string, unknown>)[fieldProperty(field)];
      if (spec.t === "markdown") {
        if (fieldValue !== undefined) {
          fields.push(Object.freeze({
            logical_path: `payload.${prefix}${field.name}`,
            bytes: fieldValue as Uint8Array,
          }));
        }
        continue;
      }
      if (spec.t === "table" && fieldValue !== undefined) {
        walk(fieldValue as object, armOfGroupValue(spec.group, fieldValue), `${prefix}${field.name}.`);
        continue;
      }
      if (spec.t === "array_table" && spec.flatten === undefined && fieldValue !== undefined) {
        (fieldValue as readonly object[]).forEach((entry, index) =>
          walk(entry, armOfGroupValue(spec.group, entry), `${prefix}${spec.prop}[${index}].`)
        );
      }
    }
  };
  walk(value, armOfPayload(value), "");
  return Object.freeze(fields);
}

/**
 * Consume every decoded Markdown field exactly once. A record renders exactly its detail_md
 * bytes (the section plan guarantees placed records have one and unplaced records do not);
 * every other Markdown field is ledgered as consumed nonrendering content.
 */
export function renderCanonicalSemanticRecord(
  record: SemanticRecord,
  fields: FieldConsumer,
): Uint8Array {
  const consumed = canonicalSemanticMarkdownFields(record.payload).map((entry) => {
    const bytes = fields.consume(entry.logical_path, entry.bytes);
    return { path: entry.logical_path, bytes };
  });
  return concatenate(consumed.filter((entry) => entry.path === "payload.detail_md").map((entry) => entry.bytes));
}

export interface RoadmapFloorSpec {
  /** The roadmap name the document must declare. */
  readonly roadmap: RoadmapName;
  readonly source_path: RepoPath;
  readonly projection_path: RepoPath;
  /** The exact generated slots the roadmap declares, in `[slot_id, binding]` form. */
  readonly slot_bindings: readonly (readonly [SlotId, string])[];
  /** The diagnostic for a slot inventory that is not exactly `slot_bindings`. */
  readonly slot_inventory_message: string;
}

/**
 * The structural floors every roadmap document must meet, as one check driven by the adapter's own
 * coordinates.  Both adapters previously carried a hand-written copy of these five checks, which is
 * how the two wordings of the slot-inventory diagnostic drifted apart -- the remaining per-roadmap
 * text is data here, not code.
 */
export function createRoadmapFloorValidator(
  spec: RoadmapFloorSpec,
): (doc: RoadmapDocument, out: IssueCollector) => void {
  const floor = (source: RepoPath, logicalPath: string, message: string): RoadmapIssue =>
    ({ code: "E-SCHEMA-FLOOR", source, logical_path: logicalPath, message, exit: 1 });
  return (doc: RoadmapDocument, out: IssueCollector): void => {
    const source = doc.document.source_path;
    if (doc.document.roadmap !== spec.roadmap) {
      out.add(floor(source, "document.roadmap", `${spec.roadmap} adapter requires a ${spec.roadmap} roadmap document`));
    }
    if (doc.document.source_path !== spec.source_path) {
      out.add(floor(source, "document.source_path", `${spec.roadmap} source path must be ${spec.source_path}`));
    }
    if (doc.document.projection_path !== spec.projection_path) {
      out.add(floor(source, "document.projection_path", `${spec.roadmap} projection path must be ${spec.projection_path}`));
    }
    if (doc.records.length === 0 || doc.sections.every((section) => section.entries.length === 0)) {
      out.add(floor(source, "$", `${spec.roadmap} roadmap requires records and section entries`));
    }
    const declared = documentSlots(doc.sections);
    const slots = new Map(declared.map((slot) => [slot.slot_id, slot]));
    if (declared.length !== spec.slot_bindings.length || slots.size !== spec.slot_bindings.length) {
      out.add(floor(source, "section.slots", spec.slot_inventory_message));
    }
    for (const [slotId, binding] of spec.slot_bindings) {
      const slot = slots.get(slotId);
      if (slot === undefined || slot.binding !== binding) {
        out.add(floor(
          source,
          `section.slots.${slotId}.binding`,
          `${spec.roadmap} slot ${slotId} must declare binding ${binding}`,
        ));
      }
    }
  };
}

export interface DecodedRoadmapValidationObserver {
  sharedValidationStarted(indexes: RoadmapIndexes): void;
  domainPayloadValidated(provider: SemanticPayloadProviderFact): void;
}

export interface DecodedRoadmapValidationOptions {
  readonly universe?: SemanticJoinUniverse;
  readonly defer_foreign_roadmap_joins?: boolean;
  readonly observer?: DecodedRoadmapValidationObserver;
}

export interface DecodedRoadmapValidationResult {
  readonly indexes: RoadmapIndexes;
  readonly issues: readonly RoadmapIssue[];
}

export type DomainPayloadFactValidator = (
  provider: SemanticPayloadProviderFact,
  indexes: RoadmapIndexes,
  out: IssueCollector,
  source: string,
) => void;

/**
 * Pure production orchestration over one already-decoded document. C4A is the mandatory first
 * callback boundary: any index issue returns immediately before floors, shared joins, relations,
 * providers, or domain payload validation can run.
 */
export function validateDecodedRoadmapDocument(
  document: RoadmapDocument,
  view: RegistryView,
  adapter: RoadmapAdapter<SemanticPayload>,
  referenceProviders: readonly ReferenceProviderLike[],
  validateDomainPayload: DomainPayloadFactValidator,
  options: DecodedRoadmapValidationOptions = {},
): DecodedRoadmapValidationResult {
  const built = buildRoadmapIndexes(document);
  if (built.issues.length > 0) {
    return Object.freeze({ indexes: built.indexes, issues: built.issues });
  }
  const indexes = built.indexes;
  const baseUniverse = options.universe ?? indexes;
  const universe = Object.freeze({
    first_class: baseUniverse.first_class,
    payload_records: baseUniverse.payload_records,
    current_guards: view.current_guards.length > 0
      ? view.current_guards
      : "current_guards" in baseUniverse ? baseUniverse.current_guards : undefined,
  });
  // One-document adapter validation is the scoped lane. Supplying a combined universe closes the
  // seam and disables deferral unless a caller explicitly requests it for a focused probe.
  const deferredNamespace = options.defer_foreign_roadmap_joins === true ||
      (options.defer_foreign_roadmap_joins === undefined && options.universe === undefined)
    ? indexes.roadmap
    : undefined;
  const domainIndexes = Object.freeze({
    ...indexes,
    first_class: universe.first_class,
    payload_records: universe.payload_records,
    ...(deferredNamespace === undefined ? {} : { deferred_foreign_roadmap_joins: deferredNamespace }),
  }) as RoadmapIndexes & { readonly deferred_foreign_roadmap_joins?: RoadmapName };
  const source = document.document.source_path;
  options.observer?.sharedValidationStarted(indexes);
  const issues: RoadmapIssue[] = [];
  const collector: IssueCollector = { issues, add: (value) => issues.push(value) };
  adapter.validateFloors(document, collector);
  issues.push(...validateSemanticRoadmapJoins(indexes, universe, source, deferredNamespace));
  issues.push(...validateRoadmapReferences(indexes, view, {
    source,
    providers: referenceProviders,
    first_class: universe.first_class,
    defer_foreign_roadmap_joins: deferredNamespace,
  }));
  issues.push(...validateRelations(indexes.relations, universe.first_class, source, deferredNamespace, view.current_guards));
  for (const provider of indexes.payload_records.values()) {
    validateDomainPayload(provider, domainIndexes, collector, source);
    options.observer?.domainPayloadValidated(provider);
  }
  return Object.freeze({ indexes, issues: sortIssues(issues) });
}
