export const ISSUE_CODES = [
  "E-CLI-UNKNOWN-OPTION",
  "E-CLI-MISSING-VALUE",
  "E-CLI-DUPLICATE-OPTION",
  "E-CLI-MODE",
  "E-CLI-ROADMAP",
  "E-CLI-INCOMPATIBLE",
  "E-CLI-AGAINST",
  "E-CLI-AS-OF",
  "E-CLI-FORMAT-TARGET",
  "E-GIT-BASE-FORMAT",
  "E-GIT-BASE-LOOKUP",
  "E-GIT-IO",
  "E-IO-PERMISSION",
  "E-IO-READ",
  "E-IO-WRITE",
  "E-IO-RENAME",
  "E-INTERNAL",
  "E-SOURCE-MISSING",
  "E-PROJECTION-MISSING",
  "E-SOURCE-UTF8",
  "E-SOURCE-LINE-END",
  "E-SOURCE-EMPTY",
  "E-SOURCE-DIGEST",
  "E-CODEC-UTF8",
  "E-CODEC-LINE-END",
  "E-CODEC-SCALAR",
  "E-CODEC-PATH-NUL",
  "E-CODEC-TOKEN",
  "E-CODEC-PLACEHOLDER",
  "E-TOML-PARSE",
  "E-TOML-NONCANONICAL",
  "E-SCHEMA-UNKNOWN-KEY",
  "E-SCHEMA-MISSING-KEY",
  "E-SCHEMA-FORBIDDEN-KEY",
  "E-SCHEMA-TYPE",
  "E-SCHEMA-ENUM",
  "E-SCHEMA-VERSION",
  "E-SCHEMA-STATE",
  "E-SCHEMA-FLOOR",
  "E-ID-GRAMMAR",
  "E-ID-RESERVED",
  "E-ID-NAMESPACE",
  "E-ID-DUPLICATE",
  "E-ALIAS-COLLISION",
  "E-OWNER-DUPLICATE",
  "E-REFERENCE-UNRESOLVED",
  "E-REFERENCE-FORBIDDEN",
  "E-REFERENCE-STUB",
  "E-RELATION-ENDPOINT",
  "E-RELATION-DUPLICATE",
  "E-RELATION-CYCLE",
  "E-MANIFEST-DUPLICATE",
  "E-MANIFEST-MISSING",
  "E-MANIFEST-UNKNOWN",
  "E-MANIFEST-KIND",
  "E-MANIFEST-ORPHAN",
  "E-FIELD-CONSUMPTION",
  "E-RENDER-EMPTY",
  "E-RENDER-AUTHORITY",
  "E-SPAN-EMPTY",
  "E-SPAN-BOUNDS",
  "E-SPAN-UTF8-BOUNDARY",
  "E-SPAN-GAP",
  "E-SPAN-OVERLAP",
  "E-SPAN-COVERAGE",
  "E-SPAN-DIGEST",
  "E-SPAN-OWNER",
  "E-SPAN-KIND",
  "E-SPAN-STATUS",
  "E-DEBT-BASE-MISMATCH",
  "E-DEBT-FROZEN-SET",
  "E-DEBT-OWNER-REGRESSION",
  "E-DEBT-SET-GROWTH",
  "E-DEBT-CATEGORY-HIDE",
  "E-CAMPAIGN-ROOT",
  "E-CAMPAIGN-TARGET",
  "E-CAMPAIGN-TARGET-EXPIRED",
  "E-CAMPAIGN-DUPLICATE",
  "E-CAMPAIGN-STATE",
  "E-CAMPAIGN-TRANSITION",
  "E-CAMPAIGN-FIRED-HIDDEN",
  "E-RETIRED-HASH",
  "E-RETIRED-REPLACEMENT",
  "E-RETIRED-REUSE",
  "E-TRANSACTION-BASE",
  "E-TRANSACTION-ORIGIN",
  "E-TRANSACTION-OWNER",
  "E-TRANSACTION-CAMPAIGN",
  "E-TRANSACTION-CITATION",
  "E-TRANSACTION-REFERENCE",
  "E-TRANSACTION-GUARD",
  "E-TRANSACTION-TOMBSTONE",
  "E-OUTPUT-PATH",
  "E-OUTPUT-CLAIM",
  "E-OUTPUT-WRITER",
  "E-OUTPUT-SLOT",
  "E-OUTPUT-AUTHORITY",
  "E-OUTPUT-TOML",
  "E-PROJECTION-DRIFT",
  "E-FIXTURE-REGISTRY",
  "E-FIXTURE-FLOOR",
  "E-FIXTURE-EXPECTED",
  "E-SELFTEST-CASE",
  "E-SELFTEST-FLOOR",
] as const;

export type IssueCode = (typeof ISSUE_CODES)[number];

export interface RoadmapIssue {
  code: IssueCode;
  source: string;
  logical_path: string;
  span?: { start_byte: number; end_byte: number };
  message: string;
  exit: 1 | 2;
}

export type RoadmapIoRole =
  | "source"
  | "projection"
  | "fixture"
  | "reference"
  | "read"
  | "git"
  | "write"
  | "rename";

export interface RoadmapIoFailureContext {
  readonly role: RoadmapIoRole;
  readonly path: string;
  readonly operation: string;
}

export interface RoadmapIoClassification {
  readonly code: IssueCode;
  readonly exit: 1 | 2;
}

/** Pure errno/role classification shared verbatim by production ports and selftests. */
export function classifyRoadmapIoErrno(
  errno: string | undefined,
  role: RoadmapIoRole,
): RoadmapIoClassification {
  if (errno === "EACCES" || errno === "EPERM") return { code: "E-IO-PERMISSION", exit: 2 };
  if (errno === "ENOENT" && role === "source") return { code: "E-SOURCE-MISSING", exit: 1 };
  if (errno === "ENOENT" && role === "projection") return { code: "E-PROJECTION-MISSING", exit: 1 };
  if (errno === "ENOENT" && role === "fixture") return { code: "E-FIXTURE-REGISTRY", exit: 1 };
  if (errno === "ENOENT" && role === "reference") return { code: "E-REFERENCE-UNRESOLVED", exit: 1 };
  if (role === "git") return { code: "E-GIT-IO", exit: 2 };
  if (role === "write") return { code: "E-IO-WRITE", exit: 2 };
  if (role === "rename") return { code: "E-IO-RENAME", exit: 2 };
  return { code: "E-IO-READ", exit: 2 };
}

export interface IssueCollector {
  readonly issues: readonly RoadmapIssue[];
  add(issue: RoadmapIssue): void;
}

const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export function compareRoadmapIssues(left: RoadmapIssue, right: RoadmapIssue): number {
  return codePointSort(left.source, right.source) ||
    codePointSort(left.logical_path, right.logical_path) ||
    (left.span?.start_byte ?? -1) - (right.span?.start_byte ?? -1) ||
    codePointSort(left.code, right.code) ||
    codePointSort(left.message, right.message);
}

export function sortRoadmapIssues(issues: readonly RoadmapIssue[]): readonly RoadmapIssue[] {
  return Object.freeze([...issues].sort(compareRoadmapIssues));
}

export function createIssueCollector(): IssueCollector {
  const mutable: RoadmapIssue[] = [];
  return {
    get issues(): readonly RoadmapIssue[] {
      return mutable;
    },
    add(issue: RoadmapIssue): void {
      mutable.push(issue);
    },
  };
}

export class RoadmapFailure extends Error {
  override readonly name = "RoadmapFailure" as const;
  readonly issues: readonly RoadmapIssue[];
  readonly exit: 1 | 2;

  constructor(issues: RoadmapIssue | readonly RoadmapIssue[]) {
    const sorted = sortRoadmapIssues(Array.isArray(issues) ? issues : [issues]);
    const exit = sorted.some((issue) => issue.exit === 2) ? 2 : 1;
    super(sorted.length === 1 ? sorted[0]!.message : `${sorted.length} roadmap issues`);
    this.issues = sorted;
    this.exit = exit;
  }
}

interface NodeErrorLike {
  readonly code?: unknown;
}

/** Pure normalization used directly by every production filesystem catch boundary. */
export function classifyRoadmapIoError(
  error: unknown,
  context: RoadmapIoFailureContext,
): RoadmapFailure {
  if (error instanceof RoadmapFailure) return error;
  const rawCode = (error as NodeErrorLike | null)?.code;
  const errno = typeof rawCode === "string" ? rawCode : undefined;
  const classified = classifyRoadmapIoErrno(errno, context.role);
  const detail = error instanceof Error ? error.message : String(error);
  return new RoadmapFailure({
    code: classified.code,
    source: context.path,
    logical_path: context.operation,
    message: `${context.operation} failed${errno === undefined ? "" : ` (${errno})`}: ${detail}`,
    exit: classified.exit,
  });
}

export function isRoadmapFailure(value: unknown): value is RoadmapFailure {
  return value instanceof RoadmapFailure;
}

export function renderRoadmapIssue(issue: RoadmapIssue): string {
  const span = issue.span === undefined
    ? ""
    : `[${issue.span.start_byte},${issue.span.end_byte})`;
  return `FAIL [${issue.code}] ${issue.source}#${issue.logical_path}${span}: ${issue.message}`;
}

export function renderRoadmapIssues(issues: readonly RoadmapIssue[]): Uint8Array {
  const sorted = sortRoadmapIssues(issues);
  const lines = [
    ...sorted.map(renderRoadmapIssue),
    `FAILED: ${sorted.length} issue(s)`,
  ];
  return new TextEncoder().encode(`${lines.join("\n")}\n`);
}

export function failureFromUnknown(error: unknown, operation: string): RoadmapFailure {
  if (isRoadmapFailure(error)) return error;
  return new RoadmapFailure({
    code: "E-INTERNAL",
    source: "<internal>",
    logical_path: operation,
    message: error instanceof Error ? error.message : String(error),
    exit: 2,
  });
}
