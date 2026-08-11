import type { RoadmapIssue } from "../errors.ts";
import type { ExpectedSelfTestIssue } from "../selftest.ts";

const scopes: ExpectedSelfTestIssue[][] = [];

/** Record one issue that a negative self-test actually matched during its mutation. */
export function observeSelfTestIssue(
  issue: Pick<RoadmapIssue, "code" | "logical_path">,
): void {
  const active = scopes[scopes.length - 1];
  if (active === undefined) return;
  active.push(Object.freeze({ code: issue.code, logical_path: issue.logical_path }));
}

export function observeMatchingIssue<T extends { readonly code: string; readonly logical_path?: string }>(
  issues: readonly T[],
  code: RoadmapIssue["code"],
  logicalPath?: string,
): T | undefined {
  const matched = issues.find((issue) =>
    issue.code === code && (logicalPath === undefined || issue.logical_path === logicalPath)
  );
  if (matched?.logical_path !== undefined) {
    observeSelfTestIssue({ code, logical_path: matched.logical_path });
  }
  return matched;
}

/**
 * Classify an exercised rejection whose production contract is an untyped throw, rather than a
 * RoadmapIssue. The thrown value is required so a test cannot report an observation without first
 * reaching the rejection seam; the harness coordinate remains visibly harness-owned.
 */
export function observeUntypedSelfTestRejection(logicalPath: string, error: unknown): void {
  if (error === undefined || error === null) return;
  observeSelfTestIssue({ code: "E-SELFTEST-CASE", logical_path: logicalPath });
}

export interface NegativeSelfTestEvidenceCandidate {
  readonly expected?: ExpectedSelfTestIssue;
}

/** Return the exact anti-vacuity failure, or undefined only for independent declaration+observation. */
export function validateNegativeSelfTestEvidence(
  declared: NegativeSelfTestEvidenceCandidate,
  authority: ExpectedSelfTestIssue | undefined,
  observations: readonly ExpectedSelfTestIssue[],
): string | undefined {
  if (authority === undefined) return "negative case has no independent expected issue authority";
  if (declared.expected === undefined) return "negative result omitted its exact expected issue code/path";
  if (
    declared.expected.code !== authority.code ||
    declared.expected.logical_path !== authority.logical_path
  ) {
    return `negative result declared ${declared.expected.code} at ${declared.expected.logical_path}, independent authority requires ${authority.code} at ${authority.logical_path}`;
  }
  if (!observations.some((observation) =>
    observation.code === authority.code && observation.logical_path === authority.logical_path
  )) {
    return `negative case did not observe ${authority.code} at ${authority.logical_path}`;
  }
  return undefined;
}

/** Keep observation state synchronous, nested, and scoped to exactly one case execution. */
export function withSelfTestIssueObservations<T>(
  run: () => T,
): { readonly result: T; readonly observations: readonly ExpectedSelfTestIssue[] } {
  const observations: ExpectedSelfTestIssue[] = [];
  scopes.push(observations);
  try {
    return { result: run(), observations: Object.freeze(observations) };
  } finally {
    const removed = scopes.pop();
    if (removed !== observations) throw new Error("self-test issue observation scopes were unbalanced");
  }
}
