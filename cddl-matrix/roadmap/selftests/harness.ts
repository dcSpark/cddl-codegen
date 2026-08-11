import { RoadmapFailure, sortRoadmapIssues, type RoadmapIssue } from "../errors.ts";
import type {
  SelfTestCase,
  SelfTestCategory,
  SelfTestCategoryFloor,
  SelfTestContext,
  ExpectedSelfTestIssue,
  SelfTestCandidateResult,
  SelfTestResult,
  SelfTestReceipt,
  SelfTestRegistryInspection,
  SelfTestRunResult,
} from "../selftest.ts";
import type { RoadmapSelfTestPorts } from "../io.ts";
import {
  validateNegativeSelfTestEvidence,
  withSelfTestIssueObservations,
} from "./observations.ts";

export interface SelfTestRegistry {
  readonly cases: readonly SelfTestCase[];
  readonly category_floors: ReadonlyMap<SelfTestCategory, SelfTestCategoryFloor>;
  validate(cases?: readonly SelfTestCase[]): readonly RoadmapIssue[];
  run(ports: SelfTestContext["ports"]): SelfTestRunResult;
}

export type CreateSelfTestRegistry = (
  cases: readonly SelfTestCase[],
  categoryFloors: ReadonlyMap<SelfTestCategory, SelfTestCategoryFloor>,
  expectedSubcases?: ReadonlyMap<string, readonly string[]>,
  expectedNegativeIssues?: ReadonlyMap<string, ExpectedSelfTestIssue>,
) => SelfTestRegistry;

const UTF8 = new TextEncoder();

function harnessIssue(code: "E-SELFTEST-CASE" | "E-SELFTEST-FLOOR", path: string, message: string): RoadmapIssue {
  return { code, source: "<selftest>", logical_path: path, message, exit: 1 };
}

export const renderSelfTestReceipt = (
  receipt: SelfTestReceipt,
  cases: readonly { readonly id: string; readonly subcases: readonly string[] }[],
): Uint8Array => {
  const lines = ["SELFTEST: PASS"];
  for (const category of receipt.categories) {
    lines.push(`${category.category}: positive=${category.positive} negative=${category.negative}`);
  }
  for (const entry of cases) {
    lines.push(`case ${entry.id}`);
    for (const subcase of entry.subcases) lines.push(`  subcase ${entry.id}/${subcase}`);
  }
  lines.push(`TOTAL: ${receipt.total} case(s)`);
  return UTF8.encode(`${lines.join("\n")}\n`);
};

export const createSelfTestRegistry: CreateSelfTestRegistry = (
  inputCases,
  inputFloors,
  inputExpectedSubcases = new Map(),
  inputExpectedNegativeIssues = new Map(),
) => {
  const cases = Object.freeze([...inputCases]);
  const categoryFloors = new Map(inputFloors);
  const expectedSubcases = new Map(inputExpectedSubcases);
  const expectedNegativeIssues = new Map(inputExpectedNegativeIssues);

  const validate = (candidateCases: readonly SelfTestCase[] = cases): readonly RoadmapIssue[] => {
    const issues: RoadmapIssue[] = [];
    const ids = new Set<string>();
    for (const testCase of candidateCases) {
      if (!/^[a-z0-9]+(?:_[a-z0-9]+)*$/u.test(testCase.id)) {
        issues.push(harnessIssue("E-SELFTEST-CASE", testCase.id, "case ID must be lowercase snake-case"));
      }
      if (ids.has(testCase.id)) {
        issues.push(harnessIssue("E-SELFTEST-CASE", testCase.id, "case ID is registered more than once"));
      }
      ids.add(testCase.id);
      if (!categoryFloors.has(testCase.category)) {
        issues.push(harnessIssue("E-SELFTEST-FLOOR", testCase.category, "case uses an undeclared category"));
      }
    }
    for (const caseId of expectedSubcases.keys()) {
      if (!ids.has(caseId)) issues.push(harnessIssue("E-SELFTEST-CASE", caseId, "required subcase parent is not registered"));
    }
    for (const caseId of expectedNegativeIssues.keys()) {
      if (!ids.has(caseId)) issues.push(harnessIssue("E-SELFTEST-CASE", caseId, "negative expectation parent is not registered"));
    }
    for (const [category, floor] of categoryFloors) {
      if (floor.total < 1 || floor.positive < 0 || floor.negative < 0 || floor.positive + floor.negative > floor.total) {
        issues.push(harnessIssue("E-SELFTEST-FLOOR", category, "category floor is internally inconsistent"));
      }
      if (!candidateCases.some((testCase) => testCase.category === category)) {
        issues.push(harnessIssue("E-SELFTEST-FLOOR", category, "category has no registered cases"));
      }
    }
    if (candidateCases.length === 0) {
      issues.push(harnessIssue("E-SELFTEST-FLOOR", "registry", "self-test registry is empty"));
    }
    return sortRoadmapIssues(issues);
  };

  const inspection: SelfTestRegistryInspection = Object.freeze({ cases, category_floors: categoryFloors, validate });

  return Object.freeze({
    cases,
    category_floors: categoryFloors,
    validate,
    run(ports: RoadmapSelfTestPorts): SelfTestRunResult {
      const preflight = validate();
      if (preflight.length > 0) throw new RoadmapFailure(preflight);
      // Establish the same-root authorization once before any fixture-backed lane executes.
      ports.fixtures.enumerateFixtureFiles("cddl-matrix/roadmap/fixtures" as never);
      const counts = new Map<SelfTestCategory, { positive: number; negative: number }>();
      for (const category of categoryFloors.keys()) counts.set(category, { positive: 0, negative: 0 });
      const failures: RoadmapIssue[] = [];
      const executed: { id: string; subcases: readonly string[] }[] = [];
      const context: SelfTestContext = { ports, registry: inspection };
      for (const testCase of cases) {
        let result: SelfTestCandidateResult | SelfTestResult;
        let observations: readonly ExpectedSelfTestIssue[] = [];
        try {
          const execution = withSelfTestIssueObservations(() => testCase.run(context));
          result = execution.result;
          observations = execution.observations;
        } catch (error) {
          result = {
            ok: false as const,
            polarity: "negative" as const,
            issues: [harnessIssue(
              "E-SELFTEST-CASE",
              testCase.id,
              error instanceof Error ? error.message : String(error),
            )],
          };
        }
        if (result.polarity === "negative") {
          const evidenceFailure = validateNegativeSelfTestEvidence(
            { expected: "expected" in result ? result.expected : undefined },
            expectedNegativeIssues.get(testCase.id),
            observations,
          );
          if (evidenceFailure !== undefined) {
            failures.push(harnessIssue("E-SELFTEST-CASE", testCase.id, evidenceFailure));
          }
        } else if (expectedNegativeIssues.has(testCase.id)) {
          failures.push(harnessIssue(
            "E-SELFTEST-CASE",
            testCase.id,
            "positive result has a frozen negative expectation",
          ));
        }
        const subcases = result.subcases ?? [];
        const seenSubcases = new Set<string>();
        for (const subcase of subcases) {
          if (subcase.length === 0 || seenSubcases.has(subcase)) {
            failures.push(harnessIssue("E-SELFTEST-CASE", `${testCase.id}/${subcase}`, "subcase label is empty or duplicated within its parent"));
          }
          seenSubcases.add(subcase);
        }
        const required = expectedSubcases.get(testCase.id) ?? [];
        if (JSON.stringify(subcases) !== JSON.stringify(required)) {
          failures.push(harnessIssue(
            "E-SELFTEST-CASE",
            testCase.id,
            `subcases differ from the frozen exact registry: expected ${JSON.stringify(required)}, got ${JSON.stringify(subcases)}`,
          ));
        }
        executed.push({ id: testCase.id, subcases: Object.freeze([...subcases]) });
        const count = counts.get(testCase.category)!;
        count[result.polarity] += 1;
        if (!result.ok) failures.push(...result.issues);
      }
      for (const [category, floor] of categoryFloors) {
        const count = counts.get(category)!;
        if (count.positive + count.negative < floor.total || count.positive < floor.positive || count.negative < floor.negative) {
          failures.push(harnessIssue(
            "E-SELFTEST-FLOOR",
            category,
            `executed positive=${count.positive} negative=${count.negative}; required total>=${floor.total} positive>=${floor.positive} negative>=${floor.negative}`,
          ));
        }
      }
      if (failures.length > 0) throw new RoadmapFailure(failures);
      const categories = [...counts]
        .map(([category, count]) => Object.freeze({ category, ...count }));
      const receipt = Object.freeze({ categories: Object.freeze(categories), total: cases.length });
      return { receipt, stdout: renderSelfTestReceipt(receipt, executed) };
    },
  });
};
