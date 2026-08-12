import type { RoadmapIssue } from "./errors.ts";
import type {
  AsOfDate,
  CliRequest,
  FullCommitId,
  QueryView,
  RepoPath,
  RoadmapName,
  RoadmapSelection,
} from "./model/core.ts";

export const ROADMAP_CLI_USAGE = `Usage:
  bun run project_roadmaps.ts --selftest
  bun run project_roadmaps.ts --roadmap matrix|testing|all --check [--against <full-lowercase-commit-id>]
  bun run project_roadmaps.ts --roadmap matrix|testing --write
  bun run project_roadmaps.ts --roadmap matrix|testing|all --query summary|debt|references|campaign|burndown|actionables|signals|decisions|families|watches|content|output-owners [--json] [--as-of YYYY-MM-DD]
  bun run project_roadmaps.ts --format-source <declared-repository-relative-toml-path>
`;

export class RoadmapCliParseError extends Error {
  constructor(readonly issue: RoadmapIssue) {
    super(issue.message);
    this.name = "RoadmapCliParseError";
  }
}

type ValueOption = "--roadmap" | "--query" | "--against" | "--as-of" | "--format-source";
type FlagOption = "--selftest" | "--check" | "--write" | "--json";
type KnownOption = ValueOption | FlagOption;

const VALUE_OPTIONS = new Set<ValueOption>([
  "--roadmap",
  "--query",
  "--against",
  "--as-of",
  "--format-source",
]);
const FLAG_OPTIONS = new Set<FlagOption>(["--selftest", "--check", "--write", "--json"]);
const QUERY_VIEWS = new Set<QueryView>([
  "summary",
  "debt",
  "references",
  "campaign",
  "burndown",
  "signals",
  "actionables",
  "decisions",
  "families",
  "watches",
  "content",
  "output-owners",
]);
const DECLARED_FORMAT_SOURCES = new Set<string>([
  "cddl-matrix/roadmap.toml",
  "tests/testing-roadmap.toml",
  "roadmap-campaign.toml",
  "roadmap-retired-ids.toml",
]);

interface TokenizedCli {
  readonly values: ReadonlyMap<ValueOption, { value: string; index: number }>;
  readonly flags: ReadonlyMap<FlagOption, number>;
}

function fail(
  code: RoadmapIssue["code"],
  index: number,
  message: string,
): never {
  throw new RoadmapCliParseError({
    code,
    source: "<cli>",
    logical_path: `argv[${index}]`,
    message,
    exit: 2,
  });
}

function tokenize(argv: readonly string[]): TokenizedCli {
  const values = new Map<ValueOption, { value: string; index: number }>();
  const flags = new Map<FlagOption, number>();
  for (let index = 0; index < argv.length; index++) {
    const token = argv[index];
    if (!VALUE_OPTIONS.has(token as ValueOption) && !FLAG_OPTIONS.has(token as FlagOption)) {
      fail(
        "E-CLI-UNKNOWN-OPTION",
        index,
        token.startsWith("--")
          ? `unknown option ${JSON.stringify(token)}`
          : `positional argument ${JSON.stringify(token)} is not allowed`,
      );
    }
    const option = token as KnownOption;
    if (VALUE_OPTIONS.has(option as ValueOption)) {
      const valueOption = option as ValueOption;
      if (values.has(valueOption)) {
        fail("E-CLI-DUPLICATE-OPTION", index, `${valueOption} may occur exactly once`);
      }
      const value = argv[index + 1];
      if (value === undefined || value.startsWith("--")) {
        fail("E-CLI-MISSING-VALUE", index, `${valueOption} requires a value`);
      }
      values.set(valueOption, { value, index });
      index++;
      continue;
    }
    const flagOption = option as FlagOption;
    if (flags.has(flagOption)) {
      fail("E-CLI-DUPLICATE-OPTION", index, `${flagOption} may occur exactly once`);
    }
    flags.set(flagOption, index);
  }
  return { values, flags };
}

function validCivilDate(value: string): boolean {
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
  if (match === null) return false;
  const year = Number(match[1]);
  const month = Number(match[2]);
  const day = Number(match[3]);
  if (year < 1 || month < 1 || month > 12 || day < 1) return false;
  const leap = year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
  const days = [31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  return day <= days[month - 1];
}

function roadmapValue(tokenized: TokenizedCli, required: boolean): RoadmapSelection | undefined {
  const value = tokenized.values.get("--roadmap");
  if (value === undefined) {
    if (required) fail("E-CLI-ROADMAP", 0, "--roadmap is required for this mode");
    return undefined;
  }
  if (value.value !== "matrix" && value.value !== "testing" && value.value !== "all") {
    fail("E-CLI-ROADMAP", value.index, "--roadmap must be matrix, testing, or all");
  }
  return value.value;
}

function incompatible(tokenized: TokenizedCli, option: KnownOption, message: string): never {
  const index = tokenized.flags.get(option as FlagOption) ?? tokenized.values.get(option as ValueOption)?.index ?? 0;
  fail("E-CLI-INCOMPATIBLE", index, message);
}

/** Parse the closed CLI grammar without consulting a port, clock, environment, or stream. */
export function parseRoadmapCli(argv: readonly string[]): CliRequest {
  const tokenized = tokenize(argv);
  const modeCoordinates: readonly [KnownOption, boolean][] = [
    ["--selftest", tokenized.flags.has("--selftest")],
    ["--check", tokenized.flags.has("--check")],
    ["--write", tokenized.flags.has("--write")],
    ["--query", tokenized.values.has("--query")],
    ["--format-source", tokenized.values.has("--format-source")],
  ];
  const modes = modeCoordinates.filter(([, present]) => present);
  if (modes.length !== 1) {
    const index = modes.length > 1
      ? tokenized.flags.get(modes[1][0] as FlagOption) ?? tokenized.values.get(modes[1][0] as ValueOption)?.index ?? 0
      : 0;
    fail("E-CLI-MODE", index, "exactly one primary mode is required");
  }
  const mode = modes[0][0];
  const requiresRoadmap = mode === "--check" || mode === "--write" || mode === "--query";
  const roadmap = roadmapValue(tokenized, requiresRoadmap);
  if (!requiresRoadmap && roadmap !== undefined) {
    incompatible(tokenized, "--roadmap", "--roadmap is forbidden for this mode");
  }

  const against = tokenized.values.get("--against");
  if (against !== undefined && mode !== "--check") {
    fail("E-CLI-AGAINST", against.index, "--against is valid only with --check");
  }
  if (tokenized.flags.has("--json") && mode !== "--query") {
    incompatible(tokenized, "--json", "--json is valid only with --query");
  }
  const asOf = tokenized.values.get("--as-of");
  if (asOf !== undefined && mode !== "--query") {
    incompatible(tokenized, "--as-of", "--as-of is valid only with --query");
  }

  if (mode === "--selftest") return { mode: "selftest" };
  if (mode === "--check") {
    return {
      mode: "check",
      roadmap: roadmap as RoadmapSelection,
      ...(against === undefined ? {} : { against: against.value as FullCommitId }),
    };
  }
  if (mode === "--write") {
    if (roadmap === "all") {
      fail("E-CLI-INCOMPATIBLE", tokenized.values.get("--roadmap")!.index, "--write requires matrix or testing, not all");
    }
    return { mode: "write", roadmap: roadmap as RoadmapName };
  }
  if (mode === "--query") {
    const query = tokenized.values.get("--query")!;
    if (!QUERY_VIEWS.has(query.value as QueryView)) {
      fail(
        "E-CLI-INCOMPATIBLE",
        query.index,
        "--query must be summary, debt, references, campaign, actionables, signals, decisions, families, watches, content, or output-owners",
      );
    }
    if (asOf !== undefined && !validCivilDate(asOf.value)) {
      fail("E-CLI-AS-OF", asOf.index, "--as-of must be an existing Gregorian date in YYYY-MM-DD form");
    }
    return {
      mode: "query",
      roadmap: roadmap as RoadmapSelection,
      view: query.value as QueryView,
      json: tokenized.flags.has("--json"),
      ...(asOf === undefined ? {} : { as_of: asOf.value as AsOfDate }),
    };
  }

  const format = tokenized.values.get("--format-source")!;
  if (!DECLARED_FORMAT_SOURCES.has(format.value)) {
    fail(
      "E-CLI-FORMAT-TARGET",
      format.index,
      "--format-source must name a declared roadmap TOML source",
    );
  }
  return { mode: "format_source", source_path: format.value as RepoPath };
}

export function isDeclaredFormatSource(path: string): path is RepoPath {
  return DECLARED_FORMAT_SOURCES.has(path);
}

export function isValidCivilDate(value: string): value is AsOfDate {
  return validCivilDate(value);
}
