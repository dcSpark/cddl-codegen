#!/usr/bin/env bun
/**
 * Tracked-text cleanliness lint — pure tracked-file reads, no cargo or dependencies.
 *
 * Reads exactly the paths `git ls-files -z` reports. Selected authored text sources must be strict
 * UTF-8 and may contain only tab, LF and CR from the C0/DEL control set; snapshots additionally
 * reject doubled rustdoc markers at line start.
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;
const ROOT = `${HERE}/..`;
const TEXT_EXTENSIONS = [".ts", ".rs", ".toml", ".md", ".mdx"] as const;

export interface Problem { path: string; message: string }

function lineAt(bytes: Uint8Array, offset: number): number {
  let line = 1;
  for (let i = 0; i < offset; i++) if (bytes[i] === 10) line++;
  return line;
}

function isSelectedText(rel: string): boolean {
  return TEXT_EXTENSIONS.some(ext => rel.endsWith(ext));
}

function isForbiddenControl(byte: number): boolean {
  return (byte <= 8) || (byte >= 11 && byte <= 12) || (byte >= 14 && byte <= 31) || byte === 127;
}

/** Scan bytes so an invalid UTF-8 sequence can never be silently replacement-decoded. */
export function textProblems(path: string, bytes: Uint8Array): Problem[] {
  try {
    new TextDecoder("utf-8", { fatal: true }).decode(bytes);
  } catch {
    return [{ path, message: "invalid UTF-8" }];
  }
  const problems: Problem[] = [];
  for (let i = 0; i < bytes.length; i++) {
    const byte = bytes[i]!;
    if (isForbiddenControl(byte))
      problems.push({
        path,
        message: `forbidden C0/DEL control byte 0x${byte.toString(16).padStart(2, "0")} at byte ${i + 1}, line ${lineAt(bytes, i)}`,
      });
  }
  return problems;
}

/** Only indentation-leading rustdoc spellings are invalid; prose mentioning them later stays ordinary text. */
export function doubledDocMarkerProblems(path: string, text: string): Problem[] {
  const problems: Problem[] = [];
  const doubled = /^[ \t]*(?:\/\/\/\/|\/\/\/\s+\/\/\/|\/\/!\/\/!|\/\/!\s+\/\/!)/;
  for (const [index, line] of text.split("\n").entries()) {
    const match = doubled.exec(line);
    if (match) problems.push({ path, message: `doubled Rust doc marker '${match[0]}' at line ${index + 1}` });
  }
  return problems;
}

function selfTestProblems(): string[] {
  const failures: string[] = [];
  const expectText = (label: string, bytes: number[], expected: string | null): void => {
    const problems = textProblems("canary", Uint8Array.from(bytes));
    if ((expected === null && problems.length !== 0) || (expected !== null && (problems.length !== 1 || !problems[0]!.message.includes(expected))))
      failures.push(`${label}: expected ${expected ?? "clean"}, got ${problems.map(p => p.message).join("; ") || "clean"}`);
  };
  expectText("invalid UTF-8", [0xc3, 0x28], "invalid UTF-8");
  expectText("forbidden control", [0x61, 0x00], "0x00");
  expectText("allowed tab/LF/CR", [0x09, 0x0a, 0x0d], null);

  const markerCases: Array<[string, string, boolean]> = [
    ["outer joined", "//// duplicated", true],
    ["outer separated", "/// /// duplicated", true],
    ["inner joined", "//!//! duplicated", true],
    ["inner separated", "//! //! duplicated", true],
    ["indented outer", "    /// /// duplicated", true],
    ["ordinary doc", "/// ordinary prose mentioning //// later", false],
    ["indented ordinary doc", "    /// ordinary prose mentioning //!//! later", false],
  ];
  for (const [label, line, bad] of markerCases) {
    const found = doubledDocMarkerProblems("canary.snap", line);
    if ((bad && found.length !== 1) || (!bad && found.length !== 0))
      failures.push(`${label}: doubled-marker detector returned ${found.length} problem(s)`);
  }
  return failures;
}

function trackedFiles(): string[] {
  const result = Bun.spawnSync(["git", "ls-files", "-z"], { cwd: ROOT, stdout: "pipe", stderr: "inherit" });
  if ((result.exitCode ?? 1) !== 0) throw new Error(`git ls-files -z failed with exit ${result.exitCode ?? 1}`);
  return (result.stdout?.toString("utf8") ?? "").split("\0").filter(Boolean).sort();
}

function main(): void {
  const selfTest = selfTestProblems();
  if (selfTest.length) throw new Error(`tracked-text lint self-test failed:\n${selfTest.map(x => `  - ${x}`).join("\n")}`);

  const problems: Problem[] = [];
  for (const rel of trackedFiles()) {
    const selected = isSelectedText(rel);
    const snapshot = rel.endsWith(".snap");
    if (!selected && !snapshot) continue;
    let bytes: Buffer;
    try {
      bytes = readFileSync(`${ROOT}/${rel}`);
    } catch (error) {
      if (typeof error === "object" && error !== null && "code" in error && (error as { code?: unknown }).code === "ENOENT") {
        problems.push({ path: rel, message: "tracked by git but absent from working tree" });
        continue;
      }
      throw error;
    }
    if (selected) problems.push(...textProblems(rel, bytes));
    if (snapshot) {
      try {
        const text = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
        problems.push(...doubledDocMarkerProblems(rel, text));
      } catch {
        problems.push({ path: rel, message: "invalid UTF-8 while scanning snapshot doc markers" });
      }
    }
  }
  if (problems.length) {
    for (const p of problems) console.error(`${p.path}: ${p.message}`);
    process.exit(1);
  }
  console.log("tracked-text lint OK: selected source text is UTF-8/control-clean and snapshots have no doubled doc markers");
}

main();
