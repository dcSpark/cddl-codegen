import type { IssueCode, RoadmapIssue } from "./errors.ts";
import type { RoadmapSelfTestPorts } from "./io.ts";
import type { FixtureRelativePath } from "./model/core.ts";

export interface SingleFileFixtureCaseRow {
  kind: "single_file";
  id: string;
  class: "codec" | "positive" | "all_fields" | "irregular";
  input: FixtureRelativePath;
  expected?: FixtureRelativePath;
  adapter: "codec" | "matrix" | "testing" | "campaign" | "retired";
  schema_version?: 0 | 1;
  projection_eof?: "lf" | "none";
}

export interface StatusCompatibilityFixtureCaseRow {
  kind: "status_compat_bundle";
  id: "fixture_status_compat";
  class: "status-compat";
  files: readonly [
    "status-compat/diagnostics.toml",
    "status-compat/inputs.toml",
    "status-compat/matrix-readme.after.md",
    "status-compat/matrix-readme.before.md",
    "status-compat/modes.toml",
    "status-compat/roadmap.after.md",
    "status-compat/roadmap.before.md",
    "status-compat/tests-readme.after.md",
    "status-compat/tests-readme.before.md",
  ];
  inputs: "status-compat/inputs.toml";
  modes: "status-compat/modes.toml";
  diagnostics: "status-compat/diagnostics.toml";
  golden_targets: {
    "status-compat/matrix-readme.before.md": "status-compat/matrix-readme.after.md";
    "status-compat/roadmap.before.md": "status-compat/roadmap.after.md";
    "status-compat/tests-readme.before.md": "status-compat/tests-readme.after.md";
  };
}

export type FixtureCaseRow = SingleFileFixtureCaseRow | StatusCompatibilityFixtureCaseRow;

export interface StatusCompatibilityModeFixture {
  id:
    | "default"
    | "check"
    | "write"
    | "write_and_check_write_wins"
    | "unrelated_arg_default";
  argv: readonly string[];
  target_state: "none" | "before" | "after";
  exit_code: 0 | 1;
  stdout_md: Uint8Array;
  stderr_md: Uint8Array;
  target_reads: number;
  claim_resolutions: number;
  writes: number;
  write_order: readonly string[];
}

export interface StatusCompatibilityDiagnosticFixture {
  id:
    | "write_missing_marker"
    | "check_missing_open"
    | "check_missing_close"
    | "check_stale_inner"
    | "derivation_vacuity";
  argv: readonly string[];
  target: string;
  mutation:
    | "remove_marker_pair"
    | "remove_open_marker"
    | "remove_close_marker"
    | "replace_payload"
    | "remove_all_features";
  marker_id?: string;
  replacement?: string;
  exit_code: 1;
  stdout_md: Uint8Array;
  stderr_md: Uint8Array;
  writes: 0;
}

export interface StatusCompatibilityInputsWire {
  matrix: {
    rfc8610_feature_ids: readonly string[];
    rfc9682_feature_ids: readonly string[];
    cddl_codegen_feature_ids: readonly string[];
    containment_ids: readonly string[];
    control_operator_ids: readonly string[];
    supported_annotation_ids: readonly string[];
    divergent_annotation: readonly {
      id: string;
      status: string;
      profile: string;
      emission_status: string;
    }[];
  };
  catalog: { constraint_row_ids: readonly string[]; constraint_vector_count: number };
  registry: { ignored_gate_ids: readonly string[] };
  timings: { fast_wall_ms: number; local_wall_ms: number; full_wall_ms: number };
}

export type SelfTestCategory =
  | "codec"
  | "schema-v0"
  | "schema-v1"
  | "domain-matrix"
  | "domain-testing"
  | "manifest-render"
  | "spans"
  | "debt"
  | "campaign"
  | "identity-retirement"
  | "references-relations"
  | "output-ownership"
  | "determinism-purity"
  | "cli-diagnostics"
  | "fixture-registry"
  | "io-harness"
  | "repository-facts"
  | "status-compat"
  | "adapter-pipeline";

export interface ExpectedSelfTestIssue {
  code: IssueCode;
  logical_path: string;
}

export type SelfTestResult =
  | { ok: true; polarity: "positive" | "negative"; subcases?: readonly string[] }
  | {
      ok: false;
      polarity: "positive" | "negative";
      issues: readonly RoadmapIssue[];
      expected?: ExpectedSelfTestIssue;
      subcases?: readonly string[];
    };

export interface SelfTestContext {
  readonly ports: RoadmapSelfTestPorts;
}

export interface SelfTestCase {
  id: string;
  category: SelfTestCategory;
  run(context: SelfTestContext): SelfTestResult;
}

export interface SelfTestCategoryReceipt {
  category: SelfTestCategory;
  positive: number;
  negative: number;
}

export interface SelfTestReceipt {
  categories: readonly SelfTestCategoryReceipt[];
  total: number;
}
