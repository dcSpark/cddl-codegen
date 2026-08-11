import type { RegistryView } from "./adapters/types.ts";
import type { RoadmapIssue } from "./errors.ts";
import type { RoadmapCliPorts, RoadmapWritePorts, ReadOnlyRoadmapPorts } from "./io.ts";
import type {
  AsOfDate,
  CliRequest,
  FullCommitId,
  QueryView,
  RepoPath,
  RoadmapName,
  RoadmapSelection,
} from "./model/core.ts";
import type { RoadmapDocument } from "./model/documents.ts";

export interface RoadmapCliResult {
  exit_code: 0 | 1 | 2;
  stdout: Uint8Array;
  stderr: Uint8Array;
}

export interface ValidatedRoadmap {
  document: RoadmapDocument;
  projection: Uint8Array;
  issues: readonly RoadmapIssue[];
}

export interface RoadmapServiceContract {
  runRoadmapCli(argv: readonly string[], ports: RoadmapCliPorts): RoadmapCliResult;
  loadRoadmap(name: RoadmapName, ports: ReadOnlyRoadmapPorts): ValidatedRoadmap;
  validateRepository(selection: RoadmapSelection, ports: ReadOnlyRoadmapPorts): readonly RoadmapIssue[];
  renderRoadmap(document: RoadmapDocument, view: RegistryView): Uint8Array;
  checkRoadmaps(selection: RoadmapSelection, ports: ReadOnlyRoadmapPorts): RoadmapCliResult;
  queryRoadmaps(
    selection: RoadmapSelection,
    view: QueryView,
    asOf: AsOfDate | undefined,
    ports: ReadOnlyRoadmapPorts,
  ): Uint8Array;
  validateChange(
    selection: RoadmapSelection,
    against: FullCommitId,
    ports: ReadOnlyRoadmapPorts,
  ): readonly RoadmapIssue[];
  formatSource(path: RepoPath, ports: RoadmapWritePorts): RoadmapCliResult;
}

export type RunRoadmapCli = (argv: readonly string[], ports: RoadmapCliPorts) => RoadmapCliResult;
export type ParsedRoadmapCliRequest = CliRequest;
