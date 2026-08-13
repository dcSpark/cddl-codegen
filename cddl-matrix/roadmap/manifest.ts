import type { RoadmapIssue } from "./errors.ts";
import type {
  ManifestEntry,
  Part,
  RecordNode,
  RoadmapDocument,
  Section,
} from "./model/documents.ts";
import { codePointSort } from "./kernel.ts";

export type RenderNode =
  | { kind: "section"; id: string; value: Section }
  | { kind: "record"; id: string; value: RecordNode }
  | { kind: "part"; id: string; value: Part };

export interface RenderOp {
  readonly manifest_index: number;
  readonly entry: ManifestEntry;
  readonly node: RenderNode;
}

export interface ManifestResolution {
  readonly ops: readonly RenderOp[];
  readonly issues: readonly RoadmapIssue[];
}

function issue(
  document: RoadmapDocument,
  code: Extract<RoadmapIssue["code"], `E-MANIFEST-${string}`>,
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

function entryTarget(entry: ManifestEntry): { kind: ManifestEntry["kind"]; id: string } {
  switch (entry.kind) {
    case "section": return { kind: entry.kind, id: entry.section_id };
    case "record": return { kind: entry.kind, id: entry.record_id };
    case "part": return { kind: entry.kind, id: entry.part_id };
  }
}

function declaredNodes(document: RoadmapDocument): RenderNode[] {
  return [
    ...document.sections.map((value) => ({ kind: "section" as const, id: value.section_id, value })),
    ...document.records.map((value) => ({ kind: "record" as const, id: value.id, value })),
    ...document.parts.map((value) => ({ kind: "part" as const, id: value.part_id, value })),
  ];
}

function nodeKey(kind: ManifestEntry["kind"], id: string): string {
  return JSON.stringify([kind, id]);
}

/**
 * Document membership is derived: a record renders exactly when it is manifest-placed, and a
 * placement is legal exactly when `payload.detail_md` is present (the one rendering field).
 * Both directions are enforced here, so accidental orphaning of a renderable record and
 * placement of a non-rendering record are loud states rather than silent ones.
 */
function manifestVisible(node: RenderNode): boolean {
  return !(node.kind === "record" && node.value.payload.detail_md === undefined);
}

/** Resolve the authored linear manifest without rendering or reordering any node. */
export function resolveManifest(document: RoadmapDocument): ManifestResolution {
  const issues: RoadmapIssue[] = [];
  const declared = declaredNodes(document);
  const byKey = new Map<string, RenderNode>();
  const kindsById = new Map<string, Set<ManifestEntry["kind"]>>();

  for (const node of declared) {
    const key = nodeKey(node.kind, node.id);
    if (byKey.has(key)) {
      issues.push(issue(
        document,
        "E-MANIFEST-DUPLICATE",
        `${node.kind}[${JSON.stringify(node.id)}]`,
        `render node ${node.kind} ${JSON.stringify(node.id)} is declared more than once`,
      ));
      continue;
    }
    byKey.set(key, node);
    const kinds = kindsById.get(node.id) ?? new Set<ManifestEntry["kind"]>();
    kinds.add(node.kind);
    kindsById.set(node.id, kinds);
  }

  const sectionIds = new Set(document.sections.map((section) => String(section.section_id)));
  const recordIds = new Set(document.records.map((record) => String(record.id)));
  for (const record of document.records) {
    if (!sectionIds.has(record.projection_group)) {
      issues.push(issue(
        document,
        "E-MANIFEST-ORPHAN",
        `record[${JSON.stringify(record.id)}].projection_group`,
        `record refers to missing section ${JSON.stringify(record.projection_group)}`,
      ));
    }
  }
  for (const part of document.parts) {
    if (!recordIds.has(part.parent_record_id)) {
      issues.push(issue(
        document,
        "E-MANIFEST-ORPHAN",
        `part[${JSON.stringify(part.part_id)}].parent_record_id`,
        `part refers to missing record ${JSON.stringify(part.parent_record_id)}`,
      ));
    }
  }

  const placed = new Set<string>();
  const ops: RenderOp[] = [];
  for (const [manifest_index, entry] of document.manifest.entries()) {
    const target = entryTarget(entry);
    const key = nodeKey(target.kind, target.id);
    const logicalPath = `manifest[${manifest_index}]`;
    if (placed.has(key)) {
      issues.push(issue(
        document,
        "E-MANIFEST-DUPLICATE",
        logicalPath,
        `${target.kind} ${JSON.stringify(target.id)} is placed more than once`,
      ));
      continue;
    }
    const node = byKey.get(key);
    if (node === undefined) {
      const otherKinds = [...(kindsById.get(target.id) ?? [])].sort(codePointSort);
      if (otherKinds.length > 0) {
        issues.push(issue(
          document,
          "E-MANIFEST-KIND",
          logicalPath,
          `${JSON.stringify(target.id)} is declared as ${otherKinds.join(", ")}, not ${target.kind}`,
        ));
      } else {
        issues.push(issue(
          document,
          "E-MANIFEST-UNKNOWN",
          logicalPath,
          `${target.kind} ${JSON.stringify(target.id)} is not declared`,
        ));
      }
      continue;
    }
    if (!manifestVisible(node)) {
      issues.push(issue(
        document,
        "E-MANIFEST-KIND",
        logicalPath,
        `record ${JSON.stringify(target.id)} has no detail_md and cannot have a manifest placement`,
      ));
      continue;
    }
    placed.add(key);
    ops.push({ manifest_index, entry, node });
  }

  for (const node of declared) {
    if (!manifestVisible(node)) continue;
    const key = nodeKey(node.kind, node.id);
    if (!placed.has(key)) {
      issues.push(issue(
        document,
        "E-MANIFEST-MISSING",
        `${node.kind}[${JSON.stringify(node.id)}]`,
        `declared ${node.kind} ${JSON.stringify(node.id)} has no manifest placement`,
      ));
    }
  }

  return { ops: Object.freeze(ops), issues: Object.freeze(issues) };
}
