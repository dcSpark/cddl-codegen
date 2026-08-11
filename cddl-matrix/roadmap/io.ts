import type { RegistryView } from "./adapters/types.ts";
import type {
  FixtureRelativePath,
  FullCommitId,
  RepoPath,
  RepositoryRevision,
} from "./model/core.ts";

declare const scratchRepositoryHandleBrand: unique symbol;
export type ScratchRepositoryHandle = {
  readonly [scratchRepositoryHandleBrand]: true;
};

export interface ReadOnlyRoadmapPorts {
  readDeclared(path: RepoPath): Uint8Array;
  readDeclaredAtCommit(commit: FullCommitId, path: RepoPath): Uint8Array;
  repositoryObjectFormat(): "sha1" | "sha256";
  resolveFullCommit(candidate: string): FullCommitId;
  registryView(revision: RepositoryRevision): RegistryView;
}

export interface RoadmapWritePorts extends ReadOnlyRoadmapPorts {
  atomicReplace(target: RepoPath, bytes: Uint8Array): void;
}

export interface ScratchSeedFile {
  path: RepoPath;
  bytes: Uint8Array;
}

export interface ScratchCommandResult {
  exit_code: number;
  stdout: Uint8Array;
  stderr: Uint8Array;
}

export interface FixtureFilesystemHarnessPorts {
  enumerateFixtureFiles(root: RepoPath): readonly FixtureRelativePath[];
  readFixtureFile(root: RepoPath, path: FixtureRelativePath): Uint8Array;
  createScratchRepository(seed: readonly ScratchSeedFile[]): ScratchRepositoryHandle;
  openScratchRoadmapPorts(repository: ScratchRepositoryHandle): RoadmapWritePorts;
  replaceScratchFile(repository: ScratchRepositoryHandle, path: RepoPath, bytes: Uint8Array): void;
  removeScratchFile(repository: ScratchRepositoryHandle, path: RepoPath): void;
  scratchRepositoryPresent(repository: ScratchRepositoryHandle): boolean;
  removeScratchRepository(repository: ScratchRepositoryHandle): void;
}

export interface ScratchGitHarnessPorts {
  runScratchGit(repository: ScratchRepositoryHandle, argv: readonly string[]): ScratchCommandResult;
}

export interface RoadmapSelfTestPorts {
  readonly fixtures: FixtureFilesystemHarnessPorts;
  readonly scratch_git: ScratchGitHarnessPorts;
}

export interface RoadmapCliPorts {
  readonly read: ReadOnlyRoadmapPorts;
  readonly write: RoadmapWritePorts;
  readonly selftest: RoadmapSelfTestPorts;
}

export interface NodeRoadmapCliPortOptions {
  matrix_dir: string;
}

export type CreateNodeRoadmapCliPorts = (options: NodeRoadmapCliPortOptions) => RoadmapCliPorts;
