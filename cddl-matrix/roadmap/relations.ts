import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapIdProviderFact } from "./indexes.ts";
import type { RoadmapId } from "./model/core.ts";
import type { RoadmapName } from "./model/core.ts";
import type { Relation, RelationKind } from "./model/documents.ts";
import type { CurrentGuard } from "./model/documents.ts";
import { namespaceOf } from "./ids.ts";
import { codePointSort } from "./kernel.ts";

const CYCLIC_KINDS = Object.freeze([
  "parent_of",
  "depends_on",
  "supersedes",
] as const satisfies readonly RelationKind[]);

const SYMMETRIC_KINDS = new Set<RelationKind>([
  "overlaps",
  "complements",
  "related",
]);

export interface DerivedRelationView {
  readonly source: RoadmapId;
  readonly kind: RelationKind;
  readonly target: RoadmapId;
  readonly authored: Relation;
  readonly direction: "forward" | "inverse";
}

function issue(
  code: Extract<RoadmapIssue["code"], `E-RELATION-${string}`>,
  source: string,
  logicalPath: string,
  message: string,
): RoadmapIssue {
  return { code, source, logical_path: logicalPath, message, exit: 1 };
}

function relationKey(relation: Relation): string {
  return JSON.stringify([relation.source, relation.kind, relation.target]);
}

function relationSort(left: Relation, right: Relation): number {
  return codePointSort(left.source, right.source) || codePointSort(left.kind, right.kind) ||
    codePointSort(left.target, right.target);
}

/** Return deterministic forward and inverse views without accepting authored inverse duplicates. */
export function deriveRelationViews(relations: readonly Relation[]): readonly DerivedRelationView[] {
  const views: DerivedRelationView[] = [];
  for (const relation of [...relations].sort(relationSort)) {
    views.push({ ...relation, authored: relation, direction: "forward" });
    views.push({
      source: relation.target,
      kind: relation.kind,
      target: relation.source,
      authored: relation,
      direction: "inverse",
    });
  }
  return Object.freeze(views.sort((left, right) =>
    codePointSort(left.source, right.source) || codePointSort(left.kind, right.kind) ||
    codePointSort(left.target, right.target) || codePointSort(left.direction, right.direction)
  ));
}

function cyclePath(
  kind: (typeof CYCLIC_KINDS)[number],
  relations: readonly Relation[],
): readonly RoadmapId[] | undefined {
  const adjacency = new Map<RoadmapId, RoadmapId[]>();
  for (const relation of relations.filter((candidate) => candidate.kind === kind).sort(relationSort)) {
    const targets = adjacency.get(relation.source) ?? [];
    targets.push(relation.target);
    adjacency.set(relation.source, targets);
    if (!adjacency.has(relation.target)) adjacency.set(relation.target, []);
  }
  for (const targets of adjacency.values()) targets.sort(codePointSort);
  const visiting = new Set<RoadmapId>();
  const visited = new Set<RoadmapId>();
  const stack: RoadmapId[] = [];
  const visit = (node: RoadmapId): readonly RoadmapId[] | undefined => {
    if (visiting.has(node)) {
      const start = stack.indexOf(node);
      return Object.freeze([...stack.slice(start), node]);
    }
    if (visited.has(node)) return undefined;
    visiting.add(node);
    stack.push(node);
    for (const target of adjacency.get(node) ?? []) {
      const cycle = visit(target);
      if (cycle !== undefined) return cycle;
    }
    stack.pop();
    visiting.delete(node);
    visited.add(node);
    return undefined;
  };
  for (const node of [...adjacency.keys()].sort(codePointSort)) {
    const cycle = visit(node);
    if (cycle !== undefined) return cycle;
  }
  return undefined;
}

export function validateRelations(
  relations: readonly Relation[],
  firstClass: ReadonlyMap<RoadmapId, RoadmapIdProviderFact>,
  source = "<relations>",
  deferForeignRoadmapJoins?: RoadmapName,
  currentGuards: readonly CurrentGuard[] = [],
): readonly RoadmapIssue[] {
  const issues: RoadmapIssue[] = [];
  const sorted = [...relations].sort(relationSort);
  const guards = new Map(currentGuards.map((guard) => [guard.id, guard]));
  const exact = new Map<string, number>();
  const symmetric = new Map<string, Relation[]>();
  const deferredForeign = (id: RoadmapId): boolean => {
    const namespace = namespaceOf(id);
    return deferForeignRoadmapJoins !== undefined && namespace !== undefined &&
      namespace !== deferForeignRoadmapJoins && !firstClass.has(id);
  };
  for (const [index, relation] of sorted.entries()) {
    const path = `relation[${index}]`;
    if (!firstClass.has(relation.source) && !deferredForeign(relation.source)) {
      issues.push(issue(
        "E-RELATION-ENDPOINT",
        source,
        `${path}.source`,
        `relation source ${JSON.stringify(relation.source)} is not an active first-class ID`,
      ));
    }
    const targetGuard = guards.get(relation.target);
    const guardedReopen = relation.kind === "reopens" &&
      targetGuard?.guard_role === "closed_family_root";
    if (!firstClass.has(relation.target) && !guardedReopen && !deferredForeign(relation.target)) {
      issues.push(issue(
        "E-RELATION-ENDPOINT",
        source,
        `${path}.target`,
        `relation target ${JSON.stringify(relation.target)} is not an active first-class ID`,
      ));
    }
    if (relation.kind === "reopens" && targetGuard !== undefined && !guardedReopen) {
      issues.push(issue(
        "E-RELATION-ENDPOINT",
        source,
        `${path}.target`,
        `reopens target guard ${JSON.stringify(relation.target)} must be a closed-family root`,
      ));
    }
    const key = relationKey(relation);
    exact.set(key, (exact.get(key) ?? 0) + 1);
    if (SYMMETRIC_KINDS.has(relation.kind)) {
      const pair = [relation.source, relation.target].sort(codePointSort);
      const symmetricKey = JSON.stringify([relation.kind, ...pair]);
      const group = symmetric.get(symmetricKey) ?? [];
      group.push(relation);
      symmetric.set(symmetricKey, group);
    }
  }
  for (const [key, count] of [...exact].sort(([left], [right]) => codePointSort(left, right))) {
    if (count > 1) {
      issues.push(issue("E-RELATION-DUPLICATE", source, `relation-tuple.${key}`, `relation tuple is authored ${count} times`));
    }
  }
  for (const [key, group] of [...symmetric].sort(([left], [right]) => codePointSort(left, right))) {
    if (group.length > 1 && new Set(group.map(relationKey)).size > 1) {
      issues.push(issue(
        "E-RELATION-DUPLICATE",
        source,
        `relation-inverse.${key}`,
        `symmetric relation is authored in both directions`,
      ));
    }
  }
  for (const kind of CYCLIC_KINDS) {
    const cycle = cyclePath(kind, sorted);
    if (cycle !== undefined) {
      issues.push(issue(
        "E-RELATION-CYCLE",
        source,
        `relation-cycle.${kind}`,
        `${kind} cycle follows ${cycle.map((id) => JSON.stringify(id)).join(" -> ")}`,
      ));
    }
  }
  return Object.freeze(issues.sort((left, right) =>
    codePointSort(left.logical_path, right.logical_path) || codePointSort(left.code, right.code) ||
    codePointSort(left.message, right.message)
  ));
}
