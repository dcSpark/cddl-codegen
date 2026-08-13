/**
 * The render plan: sections in authored order, each followed by the nodes its `entries` list
 * places. Sections own presentation order outright — there is no separate placement table to keep
 * in sync with them — and membership is total in both directions:
 *
 *   - every record with renderable prose (`payload.detail_md`) is listed exactly once;
 *   - a record without it is listed nowhere (listing one is an error, not a silent no-op);
 *   - every declared part is listed exactly once;
 *   - every listed ID resolves to a declared record or part.
 */
import type { RoadmapIssue } from "./errors.ts";
import { codePointSort } from "./kernel.ts";
import type {
  Part,
  RecordNode,
  RoadmapDocument,
  Section,
} from "./model/documents.ts";

export type RenderNode =
  | { kind: "section"; id: string; value: Section }
  | { kind: "record"; id: string; value: RecordNode }
  | { kind: "part"; id: string; value: Part };

export interface RenderOp {
  readonly plan_index: number;
  readonly node: RenderNode;
}

export interface SectionPlan {
  readonly ops: readonly RenderOp[];
  readonly issues: readonly RoadmapIssue[];
}

function issue(
  document: RoadmapDocument,
  code: Extract<RoadmapIssue["code"], `E-SECTION-${string}`>,
  logical_path: string,
  message: string,
): RoadmapIssue {
  return {
    code,
    source: document.document.source_path,
    logical_path,
    message,
    exit: 1,
  };
}

function renderable(node: RenderNode): boolean {
  return !(node.kind === "record" && node.value.payload.detail_md === undefined);
}

function entryPath(sectionId: string, index: number): string {
  return `section[${JSON.stringify(sectionId)}].entries[${index}]`;
}

/** Resolve the authored section plan without rendering or reordering any node. */
export function resolveSectionPlan(document: RoadmapDocument): SectionPlan {
  const issues: RoadmapIssue[] = [];
  const declared = new Map<string, RenderNode>();
  // NB: not named `declare` — TypeScript treats a bare `declare(...)` statement as an ambient
  // declaration modifier, and the transpiler erases the call.
  const declareNode = (node: RenderNode): void => {
    if (declared.has(node.id)) {
      issues.push(issue(
        document,
        "E-SECTION-DUPLICATE",
        `${node.kind}[${JSON.stringify(node.id)}]`,
        `render node ID ${JSON.stringify(node.id)} is declared more than once`,
      ));
      return;
    }
    declared.set(node.id, node);
  };
  for (const value of document.records) declareNode({ kind: "record", id: value.id, value });
  for (const value of document.parts) declareNode({ kind: "part", id: value.part_id, value });

  const sectionIds = new Set<string>();
  for (const section of document.sections) {
    const id = String(section.section_id);
    if (sectionIds.has(id)) {
      issues.push(issue(
        document,
        "E-SECTION-DUPLICATE",
        `section[${JSON.stringify(id)}]`,
        `section ${JSON.stringify(id)} is declared more than once`,
      ));
    }
    sectionIds.add(id);
  }

  const placed = new Map<string, string>();
  const ops: RenderOp[] = [];
  let planIndex = 0;
  for (const section of document.sections) {
    const sectionId = String(section.section_id);
    ops.push({ plan_index: planIndex++, node: { kind: "section", id: sectionId, value: section } });
    for (const [index, entryId] of section.entries.entries()) {
      const path = entryPath(sectionId, index);
      const owner = placed.get(entryId);
      if (owner !== undefined) {
        issues.push(issue(
          document,
          "E-SECTION-DUPLICATE",
          path,
          `${JSON.stringify(entryId)} is already placed by section ${JSON.stringify(owner)}`,
        ));
        continue;
      }
      const node = declared.get(entryId);
      if (node === undefined) {
        issues.push(issue(
          document,
          "E-SECTION-UNKNOWN",
          path,
          `${JSON.stringify(entryId)} is not a declared record or part`,
        ));
        continue;
      }
      if (!renderable(node)) {
        issues.push(issue(
          document,
          "E-SECTION-KIND",
          path,
          `record ${JSON.stringify(entryId)} has no detail_md and cannot be a section entry`,
        ));
        continue;
      }
      placed.set(entryId, sectionId);
      ops.push({ plan_index: planIndex++, node });
    }
  }

  // Orphans are reported in ID order so the diagnostic set never depends on source table order.
  const orphans = [...declared.values()].sort((left, right) => codePointSort(left.id, right.id));
  for (const node of orphans) {
    if (!renderable(node) || placed.has(node.id)) continue;
    issues.push(issue(
      document,
      "E-SECTION-ORPHAN",
      `${node.kind}[${JSON.stringify(node.id)}]`,
      `declared ${node.kind} ${JSON.stringify(node.id)} appears in no section's entries`,
    ));
  }

  return { ops: Object.freeze(ops), issues: Object.freeze(issues) };
}
