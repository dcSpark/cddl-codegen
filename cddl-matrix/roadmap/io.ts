import {
  closeSync,
  existsSync,
  fsyncSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  openSync,
  readFileSync,
  readdirSync,
  realpathSync,
  renameSync,
  rmSync,
  unlinkSync,
  writeFileSync,
  type Dirent,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { spawnSync } from "node:child_process";
import type { RegistryView } from "./adapters/types.ts";
import { decodeCampaignSource } from "./decode/campaign.ts";
import { decodeRoadmapSource } from "./decode/roadmap.ts";
import { decodeRetiredSource } from "./decode/retired.ts";
import {
  classifyRoadmapIoError,
  RoadmapFailure,
  type IssueCode,
  type RoadmapIssue,
} from "./errors.ts";
import { productionOutputInventory, productionOutputStage } from "./output_registry.ts";
import type {
  FixtureRelativePath,
  FullCommitId,
  RepoPath,
  RepositoryRevision,
} from "./model/core.ts";
import type { MatrixStatusInputs } from "./model/matrix.ts";
import {
  extractRustTestSymbols,
  scanRoadmapCitations,
  type TrackedTextInput,
} from "./references.ts";
import { extractFixedValueSourceFacts } from "./source_facts.ts";
import { deriveFixedValueCurrentGuards, fixedValueGuardActivationState } from "./fixed_value_guards.ts";
import { FIXED_VALUE_DELIVERY_BASE } from "./fixed_value_guards.ts";

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

interface NodeErrorLike {
  readonly code?: unknown;
  readonly message?: unknown;
}

const UTF8 = new TextDecoder("utf-8", { fatal: true });
const FIXTURE_ROOT = "cddl-matrix/roadmap/fixtures";
const PROJECTION_PATHS = new Set(["cddl-matrix/ROADMAP.md", "tests/TESTING_ROADMAP.md"]);
const SOURCE_PATHS = new Set([
  "cddl-matrix/roadmap.toml",
  "tests/testing-roadmap.toml",
  "roadmap-campaign.toml",
  "roadmap-retired-ids.toml",
]);
const codePointSort = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

function issue(code: IssueCode, source: string, logical_path: string, message: string, exit: 1 | 2): RoadmapIssue {
  return { code, source, logical_path, message, exit };
}

function nodeErrorCode(error: unknown): string | undefined {
  const code = (error as NodeErrorLike | null)?.code;
  return typeof code === "string" ? code : undefined;
}

function fixtureFailure(path: string, operation: string, message: string): RoadmapFailure {
  return new RoadmapFailure(issue("E-FIXTURE-REGISTRY", path, operation, message, 1));
}

// These exported policy symbols are pure: production adapts filesystem calls to the surface below,
// while selftests provide deterministic descriptors. All actual effects remain private to io.ts.
export type FixtureNodeKind = "file" | "directory" | "symlink" | "other";

export interface FixtureDirectoryEntry {
  readonly name: string;
  readonly kind: FixtureNodeKind;
}

export interface FixtureFsPolicySurface {
  nodeKind(relativePath: string): FixtureNodeKind;
  listDirectory(relativePath: string): readonly FixtureDirectoryEntry[];
  resolvesWithinRoot(relativePath: string): boolean;
  readFile(relativePath: string): Uint8Array;
}

export function fixtureRelativePathParts(path: string): readonly string[] {
  if (path.length === 0 || path.includes("\0") || path.includes("\\") || path.startsWith("/")) {
    throw fixtureFailure(path, "fixture-path", "fixture path must be a nonempty POSIX relative path");
  }
  const parts = path.split("/");
  if (parts.some((part) => part.length === 0 || part === "." || part === "..")) {
    throw fixtureFailure(path, "fixture-path", "fixture path contains a forbidden segment");
  }
  return Object.freeze(parts);
}

function fixtureChildPath(parent: string, name: string): string {
  fixtureRelativePathParts(name);
  if (name.includes("/")) {
    throw fixtureFailure(name, "fixture-enumeration", "fixture entry name contains a path separator");
  }
  return parent.length === 0 ? name : `${parent}/${name}`;
}

export function enumerateFixtureFilesPolicy(
  surface: FixtureFsPolicySurface,
): readonly FixtureRelativePath[] {
  const rootKind = surface.nodeKind("");
  if (rootKind === "symlink" || rootKind !== "directory") {
    throw fixtureFailure("<fixture-root>", "fixture-enumeration", "fixture root is not a regular directory");
  }
  if (!surface.resolvesWithinRoot("")) {
    throw fixtureFailure("<fixture-root>", "fixture-enumeration", "fixture root resolves outside its authorized spelling");
  }
  const files: string[] = [];
  const visit = (directory: string): void => {
    const entries = [...surface.listDirectory(directory)].sort((left, right) =>
      left.name < right.name ? -1 : left.name > right.name ? 1 : 0
    );
    for (const entry of entries) {
      const path = fixtureChildPath(directory, entry.name);
      if (entry.kind === "symlink") {
        throw fixtureFailure(path, "fixture-enumeration", "fixture inventory cannot contain symlinks");
      }
      if (entry.kind === "directory") visit(path);
      else if (entry.kind === "file") files.push(path);
      else {
        throw fixtureFailure(path, "fixture-enumeration", "fixture inventory can contain only regular files and directories");
      }
    }
  };
  visit("");
  files.sort((left, right) => left < right ? -1 : left > right ? 1 : 0);
  return Object.freeze(files as FixtureRelativePath[]);
}

export function readInventoriedFixturePolicy(
  path: FixtureRelativePath,
  inventory: ReadonlySet<string>,
  surface: FixtureFsPolicySurface,
): Uint8Array {
  const parts = fixtureRelativePathParts(path);
  if (!inventory.has(path)) {
    throw fixtureFailure(path, "fixture-read", "fixture path was not returned by the same-root inventory");
  }
  let prefix = "";
  let leafKind: FixtureNodeKind | undefined;
  for (const part of parts) {
    prefix = prefix.length === 0 ? part : `${prefix}/${part}`;
    const kind = surface.nodeKind(prefix);
    if (kind === "symlink") {
      throw fixtureFailure(path, "fixture-read", "fixture path contains a symbolic-link component");
    }
    leafKind = kind;
  }
  if (leafKind !== "file") {
    throw fixtureFailure(path, "fixture-read", "fixture is not a regular file");
  }
  if (!surface.resolvesWithinRoot(path)) {
    throw fixtureFailure(path, "fixture-read", "fixture resolves outside its authorized root");
  }
  return new Uint8Array(surface.readFile(path));
}

function strictRelativePath(path: string, role: "repository" | "fixture"): readonly string[] {
  if (
    path.length === 0 || path.includes("\0") || path.includes("\\") || isAbsolute(path) ||
    path.startsWith("/")
  ) {
    throw fixtureFailure(path, `${role}-path`, `${role} path must be a nonempty POSIX relative path`);
  }
  const parts = path.split("/");
  if (parts.some((part) => part.length === 0 || part === "." || part === "..")) {
    throw fixtureFailure(path, `${role}-path`, `${role} path contains a forbidden segment`);
  }
  return parts;
}

function confinedPath(root: string, path: string, role: "repository" | "fixture"): string {
  const candidate = resolve(root, ...strictRelativePath(path, role));
  const fromRoot = relative(root, candidate);
  if (fromRoot.length === 0 || fromRoot === ".." || fromRoot.startsWith(`..${sep}`) || isAbsolute(fromRoot)) {
    throw fixtureFailure(path, `${role}-path`, `${role} path escapes its authorized root`);
  }
  return candidate;
}

function rejectSymlinkComponents(
  root: string,
  path: string,
  includeLeaf: boolean,
  failureRole: "source" | "projection" | "reference" | "write",
): void {
  const parts = strictRelativePath(path, "repository");
  const limit = includeLeaf ? parts.length : Math.max(0, parts.length - 1);
  let cursor = root;
  for (let index = 0; index < limit; index += 1) {
    cursor = join(cursor, parts[index]!);
    try {
      if (lstatSync(cursor).isSymbolicLink()) {
        const error = Object.assign(new Error("repository path contains a symbolic-link component"), {
          code: failureRole === "write" ? "ELOOP" : "ENOENT",
        });
        throw error;
      }
    } catch (error) {
      throw classifyRoadmapIoError(error, { role: failureRole, path, operation: failureRole === "write" ? "atomic-path" : "read-declared" });
    }
  }
}

function declaredRole(path: string): "source" | "projection" | "reference" {
  if (SOURCE_PATHS.has(path)) return "source";
  if (PROJECTION_PATHS.has(path)) return "projection";
  return "reference";
}

interface GitResult {
  readonly status: number;
  readonly stdout: Uint8Array;
  readonly stderr: Uint8Array;
}

function runGit(
  root: string,
  argv: readonly string[],
  operation: string,
  allowFailure = false,
  input?: Uint8Array,
): GitResult {
  if (argv.some((arg) => typeof arg !== "string" || arg.includes("\0"))) {
    throw new RoadmapFailure(issue("E-GIT-IO", "<git>", operation, "Git argv contains an invalid argument", 2));
  }
  let result: ReturnType<typeof spawnSync>;
  try {
    const environment = { ...process.env };
    // Git accepts configuration, repository, object-store and pager behavior through a broad
    // GIT_* environment surface. None is an input to the roadmap service: the repository root and
    // every Git argument are explicit. Remove the whole ambient family before installing the small
    // deterministic environment owned by this adapter.
    for (const name of Object.keys(environment)) {
      if (name.startsWith("GIT_")) delete environment[name];
    }
    Object.assign(environment, {
      GIT_AUTHOR_DATE: "2000-01-01T00:00:00Z",
      GIT_AUTHOR_EMAIL: "roadmap-selftest@example.invalid",
      GIT_AUTHOR_NAME: "roadmap selftest",
      GIT_COMMITTER_DATE: "2000-01-01T00:00:00Z",
      GIT_COMMITTER_EMAIL: "roadmap-selftest@example.invalid",
      GIT_COMMITTER_NAME: "roadmap selftest",
      GIT_CONFIG_GLOBAL: "/dev/null",
      GIT_CONFIG_NOSYSTEM: "1",
      GIT_PAGER: "cat",
      GIT_TERMINAL_PROMPT: "0",
      LANG: "C",
      LC_ALL: "C",
      TZ: "UTC",
    });
    result = spawnSync("git", [...argv], {
      cwd: root,
      shell: false,
      encoding: null,
      maxBuffer: 128 * 1024 * 1024,
      env: environment,
      ...(input === undefined ? {} : { input }),
    });
  } catch (error) {
    throw classifyRoadmapIoError(error, { role: "git", path: "<git>", operation });
  }
  if (result.error !== undefined) {
    throw classifyRoadmapIoError(result.error, { role: "git", path: "<git>", operation });
  }
  const status = result.status ?? 1;
  const stdout = typeof result.stdout === "string"
    ? new TextEncoder().encode(result.stdout)
    : new Uint8Array(result.stdout ?? new Uint8Array());
  const stderr = typeof result.stderr === "string"
    ? new TextEncoder().encode(result.stderr)
    : new Uint8Array(result.stderr ?? new Uint8Array());
  if (!allowFailure && status !== 0) {
    const message = new TextDecoder().decode(stderr).trim();
    throw new RoadmapFailure(issue(
      "E-GIT-IO",
      "<git>",
      operation,
      message.length === 0 ? `Git exited with status ${status}` : message,
      2,
    ));
  }
  return { status, stdout, stderr };
}

function decodeUtf8(bytes: Uint8Array, source: string, operation: string): string {
  try {
    return UTF8.decode(bytes);
  } catch {
    throw new RoadmapFailure(issue(source === "<git>" ? "E-GIT-IO" : "E-IO-READ", source, operation, "operation returned non-UTF-8 text", 2));
  }
}

function objectFormat(root: string): "sha1" | "sha256" {
  const value = decodeUtf8(
    runGit(root, ["rev-parse", "--show-object-format"], "object-format").stdout,
    "<git>",
    "object-format",
  ).trim();
  if (value === "sha1" || value === "sha256") return value;
  throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "object-format", `unsupported Git object format ${JSON.stringify(value)}`, 2));
}

function resolveCommit(root: string, candidate: string): FullCommitId {
  const format = objectFormat(root);
  const length = format === "sha1" ? 40 : 64;
  if (!new RegExp(`^[0-9a-f]{${length}}$`).test(candidate)) {
    throw new RoadmapFailure(issue(
      "E-GIT-BASE-FORMAT",
      "<git>",
      "against",
      `--against must be exactly ${length} lowercase hexadecimal characters for repository object format ${format}`,
      2,
    ));
  }
  const result = runGit(root, ["rev-parse", "--verify", `${candidate}^{commit}`], "against", true);
  const resolved = decodeUtf8(result.stdout, "<git>", "against").trim();
  if (result.status !== 0 || resolved !== candidate) {
    throw new RoadmapFailure(issue("E-GIT-BASE-LOOKUP", "<git>", "against", "--against names no commit object with that exact object ID", 2));
  }
  return candidate as FullCommitId;
}

interface TrackedEntry {
  readonly path: RepoPath;
  readonly mode: "100644" | "100755";
  readonly oid: string;
}

function parseTrackedRecords(bytes: Uint8Array, source: string): readonly TrackedEntry[] {
  const text = decodeUtf8(bytes, "<git>", source);
  const entries: TrackedEntry[] = [];
  for (const record of text.split("\0")) {
    if (record.length === 0) continue;
    const match = /^(100644|100755) (?:(?:blob ([0-9a-f]+))|(?:([0-9a-f]+) 0))\t([^\0]+)$/u.exec(record);
    if (match === null) continue;
    strictRelativePath(match[4]!, "repository");
    entries.push({ path: match[4]! as RepoPath, mode: match[1]! as TrackedEntry["mode"], oid: (match[2] ?? match[3])! });
  }
  entries.sort((left, right) => codePointSort(left.path, right.path));
  return Object.freeze(entries);
}

function trackedEntries(root: string, revision: RepositoryRevision): readonly TrackedEntry[] {
  return revision.kind === "worktree"
    ? parseTrackedRecords(runGit(root, ["ls-files", "--stage", "-z"], "tracked-worktree").stdout, "tracked-worktree")
    : parseTrackedRecords(
      runGit(root, ["ls-tree", "-rz", "--full-tree", revision.commit], "tracked-commit").stdout,
      "tracked-commit",
    );
}

function exactTrackedEntry(root: string, revision: RepositoryRevision, path: RepoPath): TrackedEntry | undefined {
  strictRelativePath(path, "repository");
  const argv = revision.kind === "worktree"
    ? ["ls-files", "--stage", "-z", "--", path]
    : ["ls-tree", "-rz", "--full-tree", revision.commit, "--", path];
  return parseTrackedRecords(runGit(root, argv, "declared-path").stdout, "declared-path")
    .find((entry) => entry.path === path);
}

function readTracked(root: string, revision: RepositoryRevision, path: RepoPath): Uint8Array {
  const role = declaredRole(path);
  const entry = exactTrackedEntry(root, revision, path);
  if (entry === undefined) {
    throw classifyRoadmapIoError(
      Object.assign(new Error("declared path is not a tracked regular file"), { code: "ENOENT" }),
      { role, path, operation: "read-declared" },
    );
  }
  if (revision.kind === "commit") {
    const result = runGit(root, ["cat-file", "blob", entry.oid], "read-declared-commit", true);
    if (result.status !== 0) {
      throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "read-declared-commit", `unable to read declared blob for ${path}`, 2));
    }
    return new Uint8Array(result.stdout);
  }
  const absolute = confinedPath(root, path, "repository");
  try {
    rejectSymlinkComponents(root, path, true, role);
    const stat = lstatSync(absolute);
    if (stat.isSymbolicLink() || !stat.isFile()) {
      throw Object.assign(new Error("tracked path is not a regular worktree file"), { code: "ENOENT" });
    }
    return new Uint8Array(readFileSync(absolute));
  } catch (error) {
    throw classifyRoadmapIoError(error, { role, path, operation: "read-declared" });
  }
}

function atomicReplaceAt(root: string, target: RepoPath, bytes: Uint8Array): void {
  const absolute = confinedPath(root, target, "repository");
  rejectSymlinkComponents(root, target, false, "write");
  if (existsSync(absolute)) rejectSymlinkComponents(root, target, true, "write");
  const parent = dirname(absolute);
  let temporary: string | undefined;
  let descriptor: number | undefined;
  for (let attempt = 0; attempt < 100; attempt += 1) {
    temporary = join(parent, `.${basename(absolute)}.roadmap-tmp-${process.pid}-${attempt}`);
    try {
      descriptor = openSync(temporary, "wx", 0o666);
      break;
    } catch (error) {
      if (nodeErrorCode(error) !== "EEXIST") {
        throw classifyRoadmapIoError(error, { role: "write", path: target, operation: "atomic-temp-open" });
      }
    }
  }
  if (descriptor === undefined || temporary === undefined) {
    throw new RoadmapFailure(issue("E-IO-WRITE", target, "atomic-temp-open", "unable to allocate a sibling temporary file", 2));
  }
  try {
    writeFileSync(descriptor, bytes);
    fsyncSync(descriptor);
    closeSync(descriptor);
    descriptor = undefined;
  } catch (error) {
    if (descriptor !== undefined) {
      try { closeSync(descriptor); } catch { /* retain the primary write failure */ }
    }
    try { unlinkSync(temporary); } catch { /* best-effort cleanup of an unpublished temp */ }
    throw classifyRoadmapIoError(error, { role: "write", path: target, operation: "atomic-temp-write" });
  }
  try {
    renameSync(temporary, absolute);
  } catch (error) {
    try { unlinkSync(temporary); } catch { /* best-effort cleanup of an unpublished temp */ }
    throw classifyRoadmapIoError(error, { role: "rename", path: target, operation: "atomic-rename" });
  }
}

function nodeKind(entry: { isFile(): boolean; isDirectory(): boolean; isSymbolicLink(): boolean }): FixtureNodeKind {
  if (entry.isSymbolicLink()) return "symlink";
  if (entry.isDirectory()) return "directory";
  if (entry.isFile()) return "file";
  return "other";
}

function fixtureFsPolicySurface(absoluteRoot: string): FixtureFsPolicySurface {
  const absolute = (relativePath: string): string =>
    relativePath.length === 0
      ? absoluteRoot
      : join(absoluteRoot, ...fixtureRelativePathParts(relativePath));
  return {
    nodeKind: (relativePath: string): FixtureNodeKind => nodeKind(lstatSync(absolute(relativePath))),
    listDirectory(relativePath: string): readonly FixtureDirectoryEntry[] {
      return readdirSync(absolute(relativePath), { withFileTypes: true }).map((entry) => ({
        name: entry.name,
        kind: nodeKind(entry),
      }));
    },
    resolvesWithinRoot(relativePath: string): boolean {
      const canonical = realpathSync(absolute(relativePath));
      if (relativePath.length === 0) return canonical === absoluteRoot;
      const fromRoot = relative(absoluteRoot, canonical);
      return fromRoot.length > 0 && fromRoot !== ".." && !fromRoot.startsWith(`..${sep}`) && !isAbsolute(fromRoot);
    },
    readFile: (relativePath: string): Uint8Array => new Uint8Array(readFileSync(absolute(relativePath))),
  };
}

function fixtureFileReader(
  repositoryRoot: string,
  authorizedInventories: ReadonlyMap<string, ReadonlySet<string>>,
): FixtureFilesystemHarnessPorts["readFixtureFile"] {
  return (rootPath: RepoPath, path: FixtureRelativePath): Uint8Array => {
    if (rootPath !== FIXTURE_ROOT) throw fixtureFailure(rootPath, "fixture-root", "fixture read used a foreign root");
    const absoluteRoot = confinedPath(repositoryRoot, rootPath, "repository");
    try {
      return readInventoriedFixturePolicy(
        path,
        authorizedInventories.get(rootPath) ?? new Set(),
        fixtureFsPolicySurface(absoluteRoot),
      );
    } catch (error) {
      throw classifyRoadmapIoError(error, { role: "fixture", path, operation: "fixture-read" });
    }
  };
}

const EFFECT_MODULE_ROOTS = new Set([
  "bun",
  "child_process",
  "execa",
  "fs",
  "isomorphic-git",
  "os",
  "path",
  "process",
  "shelljs",
  "simple-git",
  "zx",
]);

function effectImportSpecifiers(source: string): readonly string[] {
  const found: string[] = [];
  const staticImport = /(?:^|\n)\s*(?:import|export)\s+(?:type\s+)?(?:[^"'`;]*?\sfrom\s*)?["']([^"']+)["']/gmu;
  const callImport = /\b(?:import|require)\s*\(\s*["']([^"']+)["']\s*\)/gmu;
  for (const pattern of [staticImport, callImport]) {
    for (const match of source.matchAll(pattern)) found.push(match[1]!);
  }
  return found;
}

function isEffectModule(specifier: string): boolean {
  const withoutNode = specifier.startsWith("node:") ? specifier.slice("node:".length) : specifier;
  return EFFECT_MODULE_ROOTS.has(withoutNode.split("/")[0]!);
}

function auditRoadmapEffectImports(repositoryRoot: string): void {
  const roadmapRoot = join(repositoryRoot, "cddl-matrix", "roadmap");
  const sources: string[] = [];
  const visit = (directory: string): void => {
    let entries: Dirent<string>[];
    try { entries = readdirSync(directory, { withFileTypes: true }); }
    catch (error) {
      throw classifyRoadmapIoError(error, {
        role: "read",
        path: relative(repositoryRoot, directory),
        operation: "effect-import-funnel",
      });
    }
    entries.sort((left, right) => codePointSort(left.name, right.name));
    for (const entry of entries) {
      const absolute = join(directory, entry.name);
      if (entry.isSymbolicLink()) {
        throw new RoadmapFailure(issue(
          "E-SELFTEST-CASE",
          relative(repositoryRoot, absolute).split(sep).join("/"),
          "effect-import-funnel",
          "roadmap source inventory contains a symbolic link",
          1,
        ));
      }
      if (entry.isDirectory()) visit(absolute);
      else if (entry.isFile() && entry.name.endsWith(".ts")) sources.push(absolute);
    }
  };
  visit(roadmapRoot);
  sources.sort(codePointSort);
  if (sources.length === 0) {
    throw new RoadmapFailure(issue(
      "E-SELFTEST-CASE",
      "cddl-matrix/roadmap",
      "effect-import-funnel",
      "recursive roadmap TypeScript inventory is empty",
      1,
    ));
  }
  let ioEffectImports = 0;
  for (const absolute of sources) {
    const path = relative(repositoryRoot, absolute).split(sep).join("/");
    let source: string;
    try { source = UTF8.decode(readFileSync(absolute)); }
    catch (error) {
      throw classifyRoadmapIoError(error, { role: "read", path, operation: "effect-import-funnel" });
    }
    const effectImports = effectImportSpecifiers(source).filter(isEffectModule);
    const globalEffects = [
      ...source.matchAll(/\bBun\.(?:file|spawn|spawnSync|write)\b/gmu),
      ...source.matchAll(/\bprocess\.(?:cwd|env|exit|exitCode|pid)\b/gmu),
    ];
    if (path === "cddl-matrix/roadmap/io.ts") {
      ioEffectImports += effectImports.length + globalEffects.length;
      continue;
    }
    if (effectImports.length > 0 || globalEffects.length > 0) {
      throw new RoadmapFailure(issue(
        "E-SELFTEST-CASE",
        path,
        "effect-import-funnel",
        `only io.ts may import or access I/O effects; found ${JSON.stringify(effectImports)}`,
        1,
      ));
    }
  }
  if (ioEffectImports === 0) {
    throw new RoadmapFailure(issue(
      "E-SELFTEST-CASE",
      "cddl-matrix/roadmap/io.ts",
      "effect-import-funnel",
      "effect-import audit did not observe io.ts's owned effect surface",
      1,
    ));
  }
}

function readAllTracked(root: string, revision: RepositoryRevision): readonly TrackedTextInput[] {
  const entries = trackedEntries(root, revision)
    .filter((entry) =>
      !PROJECTION_PATHS.has(entry.path) && entry.path !== "draft" && !entry.path.startsWith("draft/")
    );
  if (revision.kind === "worktree") {
    return entries.map((entry) => {
      const absolute = confinedPath(root, entry.path, "repository");
      try {
        rejectSymlinkComponents(root, entry.path, true, "source");
        const stat = lstatSync(absolute);
        if (!stat.isFile()) throw Object.assign(new Error("tracked worktree path is not regular"), { code: "ENOENT" });
        return { source: entry.path, bytes: new Uint8Array(readFileSync(absolute)) };
      } catch (error) {
        throw classifyRoadmapIoError(error, { role: "source", path: entry.path, operation: "tracked-worktree-read" });
      }
    });
  }
  if (entries.length === 0) return [];
  const request = new TextEncoder().encode(`${entries.map((entry) => entry.oid).join("\n")}\n`);
  const result = runGit(root, ["cat-file", "--batch"], "tracked-commit-blobs", false, request);
  const values: TrackedTextInput[] = [];
  let cursor = 0;
  for (const entry of entries) {
    const newline = result.stdout.indexOf(0x0a, cursor);
    if (newline < 0) throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "tracked-commit-blobs", "Git batch response ended before its object header", 2));
    const header = decodeUtf8(result.stdout.subarray(cursor, newline), "<git>", "tracked-commit-blobs");
    const match = /^([0-9a-f]+) blob ([0-9]+)$/u.exec(header);
    if (match === null || match[1] !== entry.oid) {
      throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "tracked-commit-blobs", `unexpected Git batch header ${JSON.stringify(header)}`, 2));
    }
    const length = Number(match[2]);
    const start = newline + 1;
    const end = start + length;
    if (!Number.isSafeInteger(length) || end >= result.stdout.length || result.stdout[end] !== 0x0a) {
      throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "tracked-commit-blobs", "Git batch response has an invalid object length", 2));
    }
    values.push({ source: entry.path, bytes: new Uint8Array(result.stdout.subarray(start, end)) });
    cursor = end + 1;
  }
  if (cursor !== result.stdout.length) {
    throw new RoadmapFailure(issue("E-GIT-IO", "<git>", "tracked-commit-blobs", "Git batch response contains trailing bytes", 2));
  }
  return values;
}

function parseJson<T>(inputs: ReadonlyMap<string, Uint8Array>, path: string, fallback: T): T {
  const bytes = inputs.get(path);
  if (bytes === undefined) return fallback;
  let text: string;
  try { text = UTF8.decode(bytes); }
  catch { throw new RoadmapFailure(issue("E-SOURCE-UTF8", path, "registry-facts", "declared registry source must be strict UTF-8", 1)); }
  try { return JSON.parse(text) as T; }
  catch { throw new RoadmapFailure(issue("E-SCHEMA-TYPE", path, "registry-facts", "declared registry JSON is malformed", 1)); }
}

function matrixStatusInputs(inputs: ReadonlyMap<string, Uint8Array>): MatrixStatusInputs {
  type MatrixWire = {
    annotations?: { cddl_codegen?: readonly { id?: unknown; status?: unknown; evidence?: unknown; emission?: unknown }[] };
    features?: readonly { id?: unknown; profile?: unknown }[];
    containment?: readonly { id?: unknown; role?: unknown; feature?: unknown; spec?: unknown; example?: unknown }[];
    control_operators?: readonly { id?: unknown }[];
  };
  const matrix = parseJson<MatrixWire>(inputs, "cddl-matrix/matrix.json", {});
  const catalogBytes = inputs.get("tests/decode_conformance/catalog.toml");
  const catalog = catalogBytes === undefined ? {} : (() => {
    let text: string;
    try { text = UTF8.decode(catalogBytes); }
    catch { throw new RoadmapFailure(issue("E-SOURCE-UTF8", "tests/decode_conformance/catalog.toml", "registry-facts", "declared registry source must be strict UTF-8", 1)); }
    try { return Bun.TOML.parse(text) as { row?: readonly Record<string, unknown>[] }; }
    catch { throw new RoadmapFailure(issue("E-TOML-PARSE", "tests/decode_conformance/catalog.toml", "registry-facts", "declared registry TOML is malformed", 1)); }
  })();
  const timings = parseJson<{ tiers?: readonly Record<string, unknown>[] }>(inputs, "tests/timings.json", {});
  const checkSource = inputs.get("check.ts");
  const gates: { id: string; kind: "cmd" | "cargo" | "stub"; ignored_test?: string }[] = [];
  if (checkSource !== undefined) {
    const source = UTF8.decode(checkSource);
    const pattern = /\{\s*id:\s*"([^"]+)"[\s\S]{0,2500}?\bkind:\s*"(cmd|cargo|stub|fn)"([\s\S]{0,2500}?)\bdesc:/gu;
    for (const match of source.matchAll(pattern)) {
      const ignored = /\bignoredTest:\s*"([^"]+)"/u.exec(match[3] ?? "")?.[1];
      gates.push({
        id: match[1]!,
        kind: match[2] === "stub" ? "stub" : match[2] === "cargo" ? "cargo" : "cmd",
        ...(ignored === undefined ? {} : { ignored_test: ignored }),
      });
    }
  }
  const strings = (rows: readonly { id?: unknown }[] | undefined): readonly string[] =>
    (rows ?? []).flatMap((row) => typeof row.id === "string" ? [row.id] : []).sort(codePointSort);
  const annotations = (matrix.annotations?.cddl_codegen ?? []).flatMap((row) =>
    typeof row.id === "string" && typeof row.status === "string"
      ? [{ id: row.id, status: row.status, ...(row.emission === undefined ? {} : { emission: row.emission as Readonly<Record<string, { status?: string }>> }) }]
      : []
  ).sort((left, right) => codePointSort(left.id, right.id));
  const features = (matrix.features ?? []).flatMap((row) =>
    typeof row.id === "string"
      ? [{ id: row.id, ...(typeof row.profile === "string" ? { profile: row.profile } : {}) }]
      : []
  ).sort((left, right) => codePointSort(left.id, right.id));
  const catalogRows = (catalog.row ?? []).flatMap((row) => {
    const id = row.id;
    const vectors = row.vector;
    return typeof id === "string"
      ? [{
        id,
        vectors: Array.isArray(vectors) ? vectors.map((value) => {
          const vector = value as Record<string, unknown>;
          return {
            ...(typeof vector.expect === "string" ? { expect: vector.expect } : {}),
            ...(typeof vector.class === "string" ? { class: vector.class } : {}),
          };
        }).sort((left, right) => codePointSort(JSON.stringify(left), JSON.stringify(right))) : [],
      }]
      : [];
  }).sort((left, right) => codePointSort(left.id, right.id));
  return {
    matrix: {
      annotations,
      features,
      containment_ids: strings(matrix.containment),
      control_operator_ids: strings(matrix.control_operators),
    },
    catalog: { rows: catalogRows },
    registry: { gates: gates.sort((left, right) => codePointSort(left.id, right.id)) },
    timings: {
      tiers: (timings.tiers ?? []).flatMap((row) =>
        row.tier === "fast" || row.tier === "local" || row.tier === "full"
          ? [{ tier: row.tier, ...(typeof row.wall_ms === "number" ? { wall_ms: row.wall_ms } : {}) } as const]
          : []
      ).sort((left, right) => codePointSort(left.tier, right.tier)),
    },
  };
}

interface HeadingFactResult {
  readonly facts: RegistryView["tracked_headings"];
  readonly issues: readonly RoadmapIssue[];
}

function hasAsciiHeadingCandidate(bytes: Uint8Array): boolean {
  for (let lineStart = 0; lineStart < bytes.byteLength;) {
    let hashes = 0;
    while (hashes < 6 && lineStart + hashes < bytes.byteLength && bytes[lineStart + hashes] === 0x23) {
      hashes += 1;
    }
    const contentStart = lineStart + hashes + 1;
    if (
      hashes > 0 && lineStart + hashes < bytes.byteLength &&
      bytes[lineStart + hashes] === 0x20 && contentStart < bytes.byteLength &&
      bytes[contentStart] !== 0x0a
    ) return true;
    const newline = bytes.indexOf(0x0a, lineStart);
    if (newline < 0) break;
    lineStart = newline + 1;
  }
  return false;
}

function headingFacts(inputs: readonly TrackedTextInput[]): HeadingFactResult {
  const facts: RegistryView["tracked_headings"][number][] = [];
  const issues: RoadmapIssue[] = [];
  const encoder = new TextEncoder();
  for (const input of inputs) {
    if (
      input.bytes === undefined || input.bytes.includes(0) ||
      !hasAsciiHeadingCandidate(input.bytes)
    ) continue;
    let body: string;
    try { body = UTF8.decode(input.bytes); }
    catch {
      issues.push(issue(
        "E-SOURCE-UTF8",
        input.source,
        "tracked-heading",
        "tracked text containing a heading candidate must be strict UTF-8",
        1,
      ));
      continue;
    }
    if (body.includes("\r")) {
      issues.push(issue(
        "E-SOURCE-LINE-END",
        input.source,
        "tracked-heading",
        "tracked text containing a heading candidate must use LF line endings",
        1,
      ));
      continue;
    }
    const lines = body.split("\n");
    const rows: Array<{ fact: RegistryView["tracked_headings"][number]; level: number; line: number }> = [];
    let byteOffset = 0;
    for (const [lineIndex, line] of lines.entries()) {
      const match = /^(#{1,6}) +(.+?)(?: +#*)?$/u.exec(line);
      if (match !== null) {
        const start = byteOffset + encoder.encode(line.slice(0, line.indexOf(match[2]!))).byteLength;
        rows.push({ fact: {
          path: input.source,
          heading: match[2]!,
          span: { start_byte: start, end_byte: start + encoder.encode(match[2]!).byteLength },
        }, level: match[1]!.length, line: lineIndex });
      }
      byteOffset += encoder.encode(line).byteLength + 1;
    }
    for (const [index, row] of rows.entries()) {
      const next = rows.slice(index + 1).find((candidate) => candidate.level <= row.level);
      facts.push({
        ...row.fact,
        section_text: lines.slice(row.line, next?.line ?? lines.length).join("\n"),
      });
    }
  }
  return {
    facts: Object.freeze(facts.sort((left, right) =>
      codePointSort(left.path, right.path) || left.span.start_byte - right.span.start_byte ||
      codePointSort(left.heading, right.heading)
    )),
    issues: Object.freeze(issues.sort((left, right) =>
      codePointSort(left.source, right.source) || codePointSort(left.logical_path, right.logical_path) ||
      codePointSort(left.code, right.code)
    )),
  };
}

function buildRegistryView(root: string, revision: RepositoryRevision): RegistryView {
  const tracked = readAllTracked(root, revision);
  const byPath = new Map(tracked.flatMap((input) => input.bytes === undefined ? [] : [[input.source, input.bytes] as const]));
  const campaignBytes = byPath.get("roadmap-campaign.toml" as RepoPath);
  const campaign = campaignBytes === undefined
    ? undefined
    : decodeCampaignSource(campaignBytes, "roadmap-campaign.toml", true);
  const outputInventory = productionOutputInventory(productionOutputStage(campaign));
  // Projection files are supplied later from the lifecycle stage-selected immutable view. Every
  // other tracked regular, non-draft path participates regardless of extension or corpus role.
  const citationInputs = tracked.filter((input) => !PROJECTION_PATHS.has(input.source));
  const testSymbolInputs = tracked.filter((input) =>
    input.source === "src/main.rs" || input.source === "src/tests/mod.rs" ||
    (input.source.startsWith("src/tests/") && input.source.endsWith(".rs"))
  );
  const citations = scanRoadmapCitations(citationInputs);
  const headings = headingFacts(citationInputs);
  const testSymbols = extractRustTestSymbols(testSymbolInputs);
  const fatal = [...citations.issues, ...headings.issues, ...testSymbols.issues];
  if (fatal.length > 0) throw new RoadmapFailure(fatal);
  const status = matrixStatusInputs(byPath);
  const matrix = parseJson<{
    features?: readonly { id?: unknown }[];
    roles?: readonly { id?: unknown }[];
    containment?: readonly { id?: unknown; role?: unknown; feature?: unknown; spec?: unknown; example?: unknown }[];
    annotations?: { cddl_codegen?: readonly { id?: unknown; status?: unknown; evidence?: unknown; emission?: unknown }[] };
  }>(byPath, "cddl-matrix/matrix.json", {});
  const ids = (rows: readonly { id?: unknown }[] | undefined): readonly { id: string }[] =>
    Object.freeze((rows ?? []).flatMap((row) => typeof row.id === "string" ? [{ id: row.id }] : [])
      .sort((left, right) => codePointSort(left.id, right.id)));
  const requiredText = (value: unknown, field: string, id: string): string => {
    if (typeof value !== "string") throw new RoadmapFailure(issue("E-SCHEMA-TYPE", "cddl-matrix/matrix.json", `registry-facts.${id}.${field}`, "matrix authority fact must be a string", 1));
    return value;
  };
  const containment = Object.freeze((matrix.containment ?? []).map((row) => {
    const id = requiredText(row.id, "id", "containment");
    return Object.freeze({
      id,
      role: requiredText(row.role, "role", id),
      feature: requiredText(row.feature, "feature", id),
      spec: requiredText(row.spec, "spec", id),
      example: requiredText(row.example, "example", id),
    });
  }).sort((left, right) => codePointSort(left.id, right.id)));
  const support = Object.freeze((matrix.annotations?.cddl_codegen ?? []).map((row) => {
    const id = requiredText(row.id, "id", "support");
    const emission = row.emission === undefined ? {} : row.emission;
    if (typeof emission !== "object" || emission === null || Array.isArray(emission)) {
      throw new RoadmapFailure(issue("E-SCHEMA-TYPE", "cddl-matrix/matrix.json", `registry-facts.${id}.emission`, "matrix emission authority fact must be a table", 1));
    }
    return Object.freeze({
      id,
      status: requiredText(row.status, "status", id),
      evidence: requiredText(row.evidence, "evidence", id),
      emission: Object.freeze(emission as Readonly<Record<string, { readonly status?: string; readonly evidence?: string }>>),
    });
  }).sort((left, right) => codePointSort(left.id, right.id)));
  const rustTypeBytes = byPath.get("src/intermediate/rust_type.rs" as RepoPath);
  const parsingBytes = byPath.get("src/parsing.rs" as RepoPath);
  if ((rustTypeBytes === undefined) !== (parsingBytes === undefined)) {
    throw new RoadmapFailure(issue("E-SOURCE-MISSING", "<registry-facts>", "fixed-value-source", "FixedValue authority sources must both be tracked", 1));
  }
  let fixedValueSource;
  if (rustTypeBytes !== undefined && parsingBytes !== undefined) {
    try {
      fixedValueSource = extractFixedValueSourceFacts(UTF8.decode(rustTypeBytes), UTF8.decode(parsingBytes));
    } catch (error) {
      throw new RoadmapFailure(issue("E-SCHEMA-STATE", "src/intermediate/rust_type.rs", "fixed-value-source", error instanceof Error ? error.message : String(error), 1));
    }
  }
  const registryWithoutGuards: RegistryView = Object.freeze({
    revision,
    production_output_stage: outputInventory.stage,
    gates: Object.freeze(status.registry.gates.map((gate) => ({ id: gate.id, kind: gate.kind, stub: gate.kind === "stub" }))),
    matrix_features: ids(matrix.features),
    matrix_roles: ids(matrix.roles),
    matrix_cells: ids(matrix.containment),
    ...(fixedValueSource === undefined ? {} : { fixed_value_source: fixedValueSource }),
    matrix_containment: containment,
    matrix_support: support,
    tracked_headings: headings.facts,
    test_symbols: testSymbols.facts,
    roadmap_citations: citations.facts,
    current_guards: Object.freeze([]),
    output_claims: Object.freeze([...outputInventory.claims].sort((left, right) =>
      codePointSort(left.path, right.path) || codePointSort(left.producer, right.producer) ||
      codePointSort(left.kind === "slot" ? left.slot_id : "", right.kind === "slot" ? right.slot_id : "")
    )),
    matrix_status_inputs: status,
  });
  // Registry discovery is also used by isolated/scratch repositories whose tracked files are not
  // roadmap sources. Their normal lifecycle loader owns schema diagnostics; guard discovery must
  // remain an optional refinement rather than turning registry construction into a second decoder.
  const optionalDecode = <T>(decode: () => T): T | undefined => {
    try {
      return decode();
    } catch {
      return undefined;
    }
  };
  const matrixRoadmapBytes = byPath.get("cddl-matrix/roadmap.toml" as RepoPath);
  const matrixRoadmap = matrixRoadmapBytes === undefined
    ? undefined
    : optionalDecode(() => decodeRoadmapSource(matrixRoadmapBytes, "cddl-matrix/roadmap.toml", "matrix", true));
  const retiredBytes = byPath.get("roadmap-retired-ids.toml" as RepoPath);
  const retired = retiredBytes === undefined
    ? undefined
    : optionalDecode(() => decodeRetiredSource(retiredBytes, "roadmap-retired-ids.toml", true));
  const baselineRoadmapBytes = !fixedValueGuardActivationState(matrixRoadmap, retired)
    ? undefined
    : readTracked(root, { kind: "commit", commit: FIXED_VALUE_DELIVERY_BASE }, "cddl-matrix/roadmap.toml" as RepoPath);
  const baselineRoadmap = baselineRoadmapBytes === undefined
    ? undefined
    : decodeRoadmapSource(baselineRoadmapBytes, "cddl-matrix/roadmap.toml", "matrix", true);
  const fixedValue = deriveFixedValueCurrentGuards(
    matrixRoadmap,
    baselineRoadmap,
    retired,
    registryWithoutGuards,
  );
  return Object.freeze({
    ...registryWithoutGuards,
    current_guards: fixedValue.guards,
    ...(fixedValue.closure === undefined ? {} : { fixed_value_closure: fixedValue.closure }),
  });
}

function makeRepositoryPorts(root: string): RoadmapWritePorts {
  return Object.freeze({
    readDeclared: (path: RepoPath) => readTracked(root, { kind: "worktree" }, path),
    readDeclaredAtCommit: (commit: FullCommitId, path: RepoPath) => {
      resolveCommit(root, commit);
      return readTracked(root, { kind: "commit", commit }, path);
    },
    repositoryObjectFormat: () => objectFormat(root),
    resolveFullCommit: (candidate: string) => resolveCommit(root, candidate),
    registryView: (revision: RepositoryRevision) => {
      if (revision.kind === "commit") resolveCommit(root, revision.commit);
      return buildRegistryView(root, revision);
    },
    atomicReplace: (target: RepoPath, bytes: Uint8Array) => atomicReplaceAt(root, target, bytes),
  });
}

interface ScratchState {
  readonly root: string;
  live: boolean;
}

function initializeNodeRoadmapCliPorts(options: NodeRoadmapCliPortOptions): RoadmapCliPorts {
  const matrixDirInput = options.matrix_dir;
  if (!isAbsolute(matrixDirInput)) {
    throw new RoadmapFailure(issue("E-INTERNAL", "<internal>", "matrix-dir", "matrix_dir must be absolute", 2));
  }
  let matrixDir: string;
  try {
    matrixDir = realpathSync(matrixDirInput);
    if (!lstatSync(matrixDir).isDirectory() || basename(matrixDir) !== "cddl-matrix") {
      throw new Error("matrix_dir must name the cddl-matrix directory");
    }
  } catch (error) {
    throw classifyRoadmapIoError(error, { role: "read", path: matrixDirInput, operation: "matrix-dir" });
  }
  const repositoryRoot = dirname(matrixDir);
  const topLevel = decodeUtf8(
    runGit(repositoryRoot, ["rev-parse", "--show-toplevel"], "repository-root").stdout,
    "<git>",
    "repository-root",
  ).trim();
  let canonicalTopLevel: string;
  try { canonicalTopLevel = realpathSync(topLevel); }
  catch (error) { throw classifyRoadmapIoError(error, { role: "read", path: topLevel, operation: "repository-root" }); }
  if (canonicalTopLevel !== repositoryRoot) {
    throw new RoadmapFailure(issue("E-INTERNAL", "<internal>", "repository-root", "matrix_dir is not anchored directly below the repository root", 2));
  }

  const authorizedInventories = new Map<string, ReadonlySet<string>>();
  const scratchStates = new WeakMap<object, ScratchState>();

  const stateFor = (handle: ScratchRepositoryHandle, requireLive = true): ScratchState => {
    const state = scratchStates.get(handle as object);
    if (state === undefined) throw fixtureFailure("<scratch>", "scratch-handle", "scratch handle was not minted by this port factory");
    if (requireLive && !state.live) throw fixtureFailure("<scratch>", "scratch-handle", "scratch repository has already been removed");
    return state;
  };

  const enumerateFixtureFiles = (rootPath: RepoPath): readonly FixtureRelativePath[] => {
    if (rootPath !== FIXTURE_ROOT) {
      throw fixtureFailure(rootPath, "fixture-root", `only ${FIXTURE_ROOT} is an authorized fixture root`);
    }
    auditRoadmapEffectImports(repositoryRoot);
    const absoluteRoot = confinedPath(repositoryRoot, rootPath, "repository");
    try {
      const files = enumerateFixtureFilesPolicy(fixtureFsPolicySurface(absoluteRoot));
      authorizedInventories.set(rootPath, new Set(files));
      return files;
    } catch (error) {
      throw classifyRoadmapIoError(error, { role: "fixture", path: rootPath, operation: "fixture-enumeration" });
    }
  };

  const readFixtureFile = fixtureFileReader(repositoryRoot, authorizedInventories);

  const fixtures: FixtureFilesystemHarnessPorts = Object.freeze({
    enumerateFixtureFiles,
    readFixtureFile,
    createScratchRepository(seed: readonly ScratchSeedFile[]): ScratchRepositoryHandle {
      const validated = seed.map((file) => ({ ...file, parts: strictRelativePath(file.path, "repository") }));
      let root: string;
      try { root = mkdtempSync(join(tmpdir(), "cddl-roadmap-selftest-")); }
      catch (error) { throw classifyRoadmapIoError(error, { role: "write", path: "<scratch>", operation: "scratch-create" }); }
      try {
        for (const file of validated) {
          const target = resolve(root, ...file.parts);
          mkdirSync(dirname(target), { recursive: true });
          writeFileSync(target, file.bytes, { flag: "wx" });
        }
      } catch (error) {
        try { rmSync(root, { recursive: true, force: true }); } catch { /* preserve seed failure */ }
        throw classifyRoadmapIoError(error, { role: "write", path: "<scratch>", operation: "scratch-seed" });
      }
      const handle = Object.freeze({}) as ScratchRepositoryHandle;
      scratchStates.set(handle as object, { root, live: true });
      return handle;
    },
    openScratchRoadmapPorts(repository: ScratchRepositoryHandle): RoadmapWritePorts {
      return makeRepositoryPorts(stateFor(repository).root);
    },
    replaceScratchFile(repository: ScratchRepositoryHandle, path: RepoPath, bytes: Uint8Array): void {
      const state = stateFor(repository);
      const target = confinedPath(state.root, path, "repository");
      try {
        const stat = lstatSync(target);
        if (stat.isSymbolicLink() || !stat.isFile()) throw new Error("scratch target is not a regular file");
        writeFileSync(target, bytes);
      } catch (error) {
        throw classifyRoadmapIoError(error, { role: "write", path, operation: "scratch-replace" });
      }
    },
    removeScratchFile(repository: ScratchRepositoryHandle, path: RepoPath): void {
      const state = stateFor(repository);
      const target = confinedPath(state.root, path, "repository");
      try { unlinkSync(target); }
      catch (error) { throw classifyRoadmapIoError(error, { role: "write", path, operation: "scratch-remove-file" }); }
    },
    scratchRepositoryPresent(repository: ScratchRepositoryHandle): boolean {
      const state = stateFor(repository, false);
      return state.live && existsSync(state.root);
    },
    removeScratchRepository(repository: ScratchRepositoryHandle): void {
      const state = stateFor(repository);
      try { rmSync(state.root, { recursive: true, force: false }); }
      catch (error) { throw classifyRoadmapIoError(error, { role: "write", path: "<scratch>", operation: "scratch-cleanup" }); }
      state.live = false;
    },
  });

  const scratchGit: ScratchGitHarnessPorts = Object.freeze({
    runScratchGit(repository: ScratchRepositoryHandle, argv: readonly string[]): ScratchCommandResult {
      const result = runGit(stateFor(repository).root, argv, "scratch-git", true);
      return { exit_code: result.status, stdout: result.stdout, stderr: result.stderr };
    },
  });

  const write = makeRepositoryPorts(repositoryRoot);
  const read: ReadOnlyRoadmapPorts = Object.freeze({
    readDeclared: write.readDeclared,
    readDeclaredAtCommit: write.readDeclaredAtCommit,
    repositoryObjectFormat: write.repositoryObjectFormat,
    resolveFullCommit: write.resolveFullCommit,
    registryView: write.registryView,
  });
  return Object.freeze({
    read,
    write,
    selftest: Object.freeze({ fixtures, scratch_git: scratchGit }),
  });
}

/** Construct the frozen host-port shape without touching the filesystem, Git, or the root anchor. */
export function createNodeRoadmapCliPorts(options: NodeRoadmapCliPortOptions): RoadmapCliPorts {
  let initialized: { readonly ports: RoadmapCliPorts } | { readonly error: unknown } | undefined;
  const ports = (): RoadmapCliPorts => {
    if (initialized === undefined) {
      try { initialized = { ports: initializeNodeRoadmapCliPorts(options) }; }
      catch (error) { initialized = { error }; }
    }
    if ("error" in initialized) throw initialized.error;
    return initialized.ports;
  };
  const read: ReadOnlyRoadmapPorts = Object.freeze({
    readDeclared: (path: RepoPath) => ports().read.readDeclared(path),
    readDeclaredAtCommit: (commit: FullCommitId, path: RepoPath) => ports().read.readDeclaredAtCommit(commit, path),
    repositoryObjectFormat: () => ports().read.repositoryObjectFormat(),
    resolveFullCommit: (candidate: string) => ports().read.resolveFullCommit(candidate),
    registryView: (revision: RepositoryRevision) => ports().read.registryView(revision),
  });
  const write: RoadmapWritePorts = Object.freeze({
    ...read,
    atomicReplace: (target: RepoPath, bytes: Uint8Array) => ports().write.atomicReplace(target, bytes),
  });
  const fixtures: FixtureFilesystemHarnessPorts = Object.freeze({
    enumerateFixtureFiles: (root: RepoPath) => ports().selftest.fixtures.enumerateFixtureFiles(root),
    readFixtureFile: (root: RepoPath, path: FixtureRelativePath) =>
      ports().selftest.fixtures.readFixtureFile(root, path),
    createScratchRepository: (seed: readonly ScratchSeedFile[]) =>
      ports().selftest.fixtures.createScratchRepository(seed),
    openScratchRoadmapPorts: (repository: ScratchRepositoryHandle) =>
      ports().selftest.fixtures.openScratchRoadmapPorts(repository),
    replaceScratchFile: (repository: ScratchRepositoryHandle, path: RepoPath, bytes: Uint8Array) =>
      ports().selftest.fixtures.replaceScratchFile(repository, path, bytes),
    removeScratchFile: (repository: ScratchRepositoryHandle, path: RepoPath) =>
      ports().selftest.fixtures.removeScratchFile(repository, path),
    scratchRepositoryPresent: (repository: ScratchRepositoryHandle) =>
      ports().selftest.fixtures.scratchRepositoryPresent(repository),
    removeScratchRepository: (repository: ScratchRepositoryHandle) =>
      ports().selftest.fixtures.removeScratchRepository(repository),
  });
  const scratchGit: ScratchGitHarnessPorts = Object.freeze({
    runScratchGit: (repository: ScratchRepositoryHandle, argv: readonly string[]) =>
      ports().selftest.scratch_git.runScratchGit(repository, argv),
  });
  return Object.freeze({
    read,
    write,
    selftest: Object.freeze({ fixtures, scratch_git: scratchGit }),
  });
}
