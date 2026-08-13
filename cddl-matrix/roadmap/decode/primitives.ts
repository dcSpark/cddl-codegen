import { RoadmapWireError } from "../markdown_codec.ts";
import type { IssueCode } from "../errors.ts";
import type {
  CivilDate,
  FixtureRelativePath,
  FullCommitId,
  LowercaseSlug,
  ReferenceId,
  RepoPath,
  RoadmapId,
  RoadmapName,
} from "../model/core.ts";
import type { MarkdownBindings } from "./raw_markdown.ts";
import { childLogicalPath, indexLogicalPath } from "./raw_markdown.ts";
import { codePointSort } from "../kernel.ts";

export interface DecodeContext {
  readonly source: string;
  readonly bindings: MarkdownBindings;
  readonly schema_trace?: SchemaDecodeTrace;
}

export interface SchemaDecodeTrace {
  exactTable(schema: ExactSchemaRow, logicalPath: string): void;
  enum(values: readonly string[], logicalPath: string): void;
}

export interface ExactSchemaRow {
  readonly name: string;
  readonly required: readonly string[];
  readonly optional?: readonly string[];
  readonly forbidden?: readonly string[];
}

export interface EnumSchemaField {
  readonly name: string;
  readonly values: readonly string[];
}

export function schemaFail(
  ctx: DecodeContext,
  code: IssueCode,
  logicalPath: string,
  message: string,
): never {
  throw new RoadmapWireError({ code, source: ctx.source, logical_path: logicalPath, message, exit: 1 });
}

export function own(value: object, key: string): unknown {
  return Object.getOwnPropertyDescriptor(value, key)?.value;
}

export function hasOwn(value: object, key: string): boolean {
  return Object.prototype.hasOwnProperty.call(value, key);
}

export function expectExactTable(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  schema: ExactSchemaRow,
): object {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, `${schema.name} must be a TOML table`);
  }
  ctx.schema_trace?.exactTable(schema, logicalPath);
  const table = value;
  const keys = Object.keys(table);
  const allowed = new Set([...(schema.required ?? []), ...(schema.optional ?? [])]);
  const forbidden = new Set(schema.forbidden ?? []);
  for (const key of [...keys].sort(codePointSort)) {
    if (allowed.has(key)) continue;
    const path = childLogicalPath(logicalPath, key);
    if (forbidden.has(key)) {
      schemaFail(ctx, "E-SCHEMA-FORBIDDEN-KEY", path, `${key} is forbidden in ${schema.name}`);
    }
    schemaFail(ctx, "E-SCHEMA-UNKNOWN-KEY", path, `${key} is not a key of ${schema.name}`);
  }
  for (const key of schema.required) {
    if (!hasOwn(table, key)) {
      schemaFail(
        ctx,
        "E-SCHEMA-MISSING-KEY",
        childLogicalPath(logicalPath, key),
        `${schema.name} requires ${key}`,
      );
    }
  }
  return table;
}

export function requiredValue(table: object, key: string): unknown {
  return own(table, key);
}

export function optionalValue(table: object, key: string): unknown | undefined {
  return hasOwn(table, key) ? own(table, key) : undefined;
}

export function expectString(ctx: DecodeContext, value: unknown, logicalPath: string): string {
  if (typeof value !== "string" || ctx.bindings.isRegisteredPlaceholder(value)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected an ordinary TOML string");
  }
  return value;
}

export function expectNonemptyString(ctx: DecodeContext, value: unknown, logicalPath: string): string {
  const decoded = expectString(ctx, value, logicalPath);
  if (decoded.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", logicalPath, "string must be nonempty");
  return decoded;
}

export function expectMarkdown(ctx: DecodeContext, value: unknown, logicalPath: string): Uint8Array {
  return ctx.bindings.expectMarkdown(value, logicalPath);
}

export function expectBoolean(ctx: DecodeContext, value: unknown, logicalPath: string): boolean {
  if (typeof value !== "boolean") schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a Boolean");
  return value;
}

export function expectSafeInteger(ctx: DecodeContext, value: unknown, logicalPath: string): number {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a nonnegative safe integer");
  }
  return value;
}

export function expectFiniteNumber(ctx: DecodeContext, value: unknown, logicalPath: string): number {
  if (typeof value !== "number" || !Number.isFinite(value)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a finite number");
  }
  return value;
}

export function expectLiteralNumber<const T extends number>(
  ctx: DecodeContext,
  value: unknown,
  literal: T,
  logicalPath: string,
): T {
  if (value !== literal) schemaFail(ctx, "E-SCHEMA-VERSION", logicalPath, `expected schema version ${literal}`);
  return literal;
}

export function expectEnum<const T extends readonly string[]>(
  ctx: DecodeContext,
  value: unknown,
  values: T,
  logicalPath: string,
): T[number] {
  ctx.schema_trace?.enum(values, logicalPath);
  if (typeof value !== "string" || !values.some((candidate) => candidate === value)) {
    schemaFail(ctx, "E-SCHEMA-ENUM", logicalPath, `expected one of ${values.join("|")}`);
  }
  return value as T[number];
}

export function expectArray(ctx: DecodeContext, value: unknown, logicalPath: string): unknown[] {
  if (!Array.isArray(value)) schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected an array");
  return value;
}

export function expectArrayOf<T>(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  decode: (entry: unknown, entryPath: string) => T,
): T[] {
  return expectArray(ctx, value, logicalPath).map((entry, index) =>
    decode(entry, indexLogicalPath(logicalPath, index)),
  );
}

export function expectStringArray(ctx: DecodeContext, value: unknown, logicalPath: string): string[] {
  return expectArrayOf(ctx, value, logicalPath, (entry, path) => expectString(ctx, entry, path));
}

export function expectNonemptyArray<T>(ctx: DecodeContext, values: T[], logicalPath: string): T[] {
  if (values.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", logicalPath, "array must be nonempty");
  return values;
}

export function canonicalSet<T extends string>(
  ctx: DecodeContext,
  values: T[],
  logicalPath: string,
  nonempty = false,
): T[] {
  if (nonempty && values.length === 0) schemaFail(ctx, "E-SCHEMA-FLOOR", logicalPath, "set must be nonempty");
  const unique = new Set(values);
  if (unique.size !== values.length) schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "set contains a duplicate value");
  return [...values].sort(codePointSort);
}

export function expectStringSet(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  nonempty = false,
): string[] {
  return canonicalSet(ctx, expectStringArray(ctx, value, logicalPath), logicalPath, nonempty);
}

const ROADMAP_ID = /^(matrix|testing)\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*(?:\.[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*)*$/;
const SLUG = /^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/;
const SUBORDINATE = /^[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*$/;
const SHA256 = /^[0-9a-f]{64}$/;

export function expectRoadmapId(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  namespace?: RoadmapName,
): RoadmapId {
  const decoded = expectString(ctx, value, logicalPath);
  const match = ROADMAP_ID.exec(decoded);
  if (match === null) schemaFail(ctx, "E-ID-GRAMMAR", logicalPath, "roadmap ID does not match the closed grammar");
  if (namespace !== undefined && match[1] !== namespace) {
    schemaFail(ctx, "E-ID-NAMESPACE", logicalPath, `roadmap ID must use the ${namespace} namespace`);
  }
  return decoded as RoadmapId;
}

export function expectRoadmapIdSet(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  nonempty = false,
): RoadmapId[] {
  return canonicalSet(
    ctx,
    expectArrayOf(ctx, value, logicalPath, (entry, path) => expectRoadmapId(ctx, entry, path)),
    logicalPath,
    nonempty,
  );
}

export function expectSubordinateId(ctx: DecodeContext, value: unknown, logicalPath: string): string {
  const decoded = expectString(ctx, value, logicalPath);
  if (!SUBORDINATE.test(decoded)) {
    schemaFail(ctx, "E-ID-GRAMMAR", logicalPath, "subordinate ID does not match the closed lowercase grammar");
  }
  return decoded;
}

export function expectSubordinateSlug(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
): LowercaseSlug {
  return expectSubordinateId(ctx, value, logicalPath) as LowercaseSlug;
}

export function expectLowercaseSlug(ctx: DecodeContext, value: unknown, logicalPath: string): LowercaseSlug {
  const decoded = expectString(ctx, value, logicalPath);
  if (!SLUG.test(decoded)) schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a lowercase slug");
  return decoded as LowercaseSlug;
}

export function expectReferenceId(ctx: DecodeContext, value: unknown, logicalPath: string): ReferenceId {
  return expectSubordinateId(ctx, value, logicalPath) as ReferenceId;
}

export function expectReferenceIdSet(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
  nonempty = false,
): ReferenceId[] {
  return canonicalSet(
    ctx,
    expectArrayOf(ctx, value, logicalPath, (entry, path) => expectReferenceId(ctx, entry, path)),
    logicalPath,
    nonempty,
  );
}

export function expectRepoPath(ctx: DecodeContext, value: unknown, logicalPath: string): RepoPath {
  const decoded = expectString(ctx, value, logicalPath);
  if (decoded.includes("\0")) schemaFail(ctx, "E-CODEC-PATH-NUL", logicalPath, "repository path contains NUL");
  if (decoded.length === 0 || decoded.startsWith("/") || decoded.endsWith("/") || decoded.split("/").some((part) => part === "" || part === "." || part === "..")) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a confined repository-relative path");
  }
  return decoded as RepoPath;
}

export function expectFixturePath(
  ctx: DecodeContext,
  value: unknown,
  logicalPath: string,
): FixtureRelativePath {
  const decoded: string = expectRepoPath(ctx, value, logicalPath);
  return decoded as FixtureRelativePath;
}

export function expectSha256(ctx: DecodeContext, value: unknown, logicalPath: string): string {
  const decoded = expectString(ctx, value, logicalPath);
  if (!SHA256.test(decoded)) schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a lowercase SHA-256 digest");
  return decoded;
}

export function expectFullCommitId(ctx: DecodeContext, value: unknown, logicalPath: string): FullCommitId {
  const decoded = expectString(ctx, value, logicalPath);
  if (!/^(?:[0-9a-f]{40}|[0-9a-f]{64})$/.test(decoded)) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected a full lowercase Git object ID");
  }
  return decoded as FullCommitId;
}

function leapYear(year: number): boolean {
  return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
}

export function expectCivilDate(ctx: DecodeContext, value: unknown, logicalPath: string): CivilDate {
  const decoded = expectString(ctx, value, logicalPath);
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(decoded);
  if (match === null) schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "expected an ASCII YYYY-MM-DD civil date");
  const year = Number(match[1]);
  const month = Number(match[2]);
  const day = Number(match[3]);
  const days = [31, leapYear(year) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  if (year === 0 || month < 1 || month > 12 || day < 1 || day > days[month - 1]) {
    schemaFail(ctx, "E-SCHEMA-TYPE", logicalPath, "civil date is not an existing Gregorian day in years 0001-9999");
  }
  return decoded as CivilDate;
}

export function optionalDecoded<T>(
  table: object,
  key: string,
  path: string,
  decode: (value: unknown, logicalPath: string) => T,
): T | undefined {
  return hasOwn(table, key) ? decode(own(table, key), childLogicalPath(path, key)) : undefined;
}

export { childLogicalPath, indexLogicalPath };
