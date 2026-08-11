import { bytesEqual, RoadmapWireError } from "../markdown_codec.ts";
import type { ReplacementPin, RetiredIdV1, RetiredIdsDocumentV1 } from "../model/documents.ts";
import { composeRetiredIdsDocument } from "../compose.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectEnum,
  expectExactTable,
  expectFullCommitId,
  expectMarkdown,
  expectReferenceId,
  expectRepoPath,
  expectRoadmapId,
  expectString,
  hasOwn,
  optionalValue,
  requiredValue,
  schemaFail,
  type DecodeContext,
  type EnumSchemaField,
  type ExactSchemaRow,
  type SchemaDecodeTrace,
} from "./primitives.ts";
import { shieldTomlMarkdown, type MarkdownBindings } from "./raw_markdown.ts";

export const RETIRED_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "replacement_kind", values: ["gate", "test_symbol", "file_heading"] },
] as const;

export const RETIRED_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "retired IDs root", required: ["retired_ids"] },
  { name: "retired IDs document", required: ["schema_version"], optional: ["entry"] },
  { name: "retired ID entry", required: ["id", "last_active_at", "replacement"] },
  { name: "gate replacement", required: ["kind", "gate_id", "claim_md"] },
  { name: "test-symbol replacement", required: ["kind", "test_id", "symbol", "claim_md"] },
  { name: "file-heading replacement", required: ["kind", "path", "heading", "claim_md"] },
] as const;

function decodeReplacement(ctx: DecodeContext, raw: unknown, path: string): ReplacementPin {
  const pre = expectExactTable(ctx, raw, path, { name: "replacement discriminator", required: ["kind"], optional: ["gate_id", "test_id", "symbol", "path", "heading", "claim_md"] });
  const kind = expectEnum(ctx, requiredValue(pre, "kind"), ["gate", "test_symbol", "file_heading"] as const, p(path, "kind"));
  const table = expectExactTable(ctx, raw, path, RETIRED_SCHEMA_ROWS[kind === "gate" ? 3 : kind === "test_symbol" ? 4 : 5]);
  const claim_md = expectMarkdown(ctx, requiredValue(table, "claim_md"), p(path, "claim_md"));
  if (kind === "gate") return { kind, gate_id: expectString(ctx, requiredValue(table, "gate_id"), p(path, "gate_id")), claim_md };
  if (kind === "test_symbol") return { kind, test_id: expectString(ctx, requiredValue(table, "test_id"), p(path, "test_id")), symbol: expectString(ctx, requiredValue(table, "symbol"), p(path, "symbol")), claim_md };
  return { kind, path: expectRepoPath(ctx, requiredValue(table, "path"), p(path, "path")), heading: expectString(ctx, requiredValue(table, "heading"), p(path, "heading")), claim_md };
}

function decodeEntry(ctx: DecodeContext, raw: unknown, path: string): RetiredIdV1 {
  const table = expectExactTable(ctx, raw, path, RETIRED_SCHEMA_ROWS[2]);
  return {
    id: expectRoadmapId(ctx, requiredValue(table, "id"), p(path, "id")),
    last_active_at: expectFullCommitId(ctx, requiredValue(table, "last_active_at"), p(path, "last_active_at")),
    replacement: decodeReplacement(ctx, requiredValue(table, "replacement"), p(path, "replacement")),
  };
}

export function decodeRetiredFromBindings(bindings: MarkdownBindings, schemaTrace?: SchemaDecodeTrace): RetiredIdsDocumentV1 {
  const ctx: DecodeContext = { source: bindings.source, bindings, schema_trace: schemaTrace };
  const root = expectExactTable(ctx, bindings.parsed, "$", RETIRED_SCHEMA_ROWS[0]);
  const retired = expectExactTable(ctx, requiredValue(root, "retired_ids"), "retired_ids", RETIRED_SCHEMA_ROWS[1]);
  if (requiredValue(retired, "schema_version") !== 1) schemaFail(ctx, "E-SCHEMA-VERSION", "retired_ids.schema_version", "WP1 accepts retired-ID schema version 1 only");
  const entries = hasOwn(retired, "entry")
    ? expectArrayOf(ctx, optionalValue(retired, "entry"), "retired_ids.entry", (entry, path) => decodeEntry(ctx, entry, path))
    : [];
  entries.sort((left, right) => left.id < right.id ? -1 : left.id > right.id ? 1 : 0);
  const doc: RetiredIdsDocumentV1 = { retired_ids: { schema_version: 1 }, entries };
  bindings.assertAllConsumed();
  return doc;
}

export function decodeRetiredSource(
  bytes: Uint8Array,
  source: string,
  requireCanonical = true,
  schemaTrace?: SchemaDecodeTrace,
): RetiredIdsDocumentV1 {
  const doc = decodeRetiredFromBindings(shieldTomlMarkdown(bytes, source), schemaTrace);
  if (requireCanonical && !bytesEqual(bytes, composeRetiredIdsDocument(doc))) {
    throw new RoadmapWireError({ code: "E-TOML-NONCANONICAL", source, logical_path: "$", message: "retired-ID TOML bytes do not equal canonical composition", exit: 1 });
  }
  return doc;
}
