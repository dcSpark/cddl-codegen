import { bytesEqual, RoadmapWireError } from "../markdown_codec.ts";
import type {
  CampaignDocumentV1,
  CampaignSelectionV1,
  LegacyMarkdownReservationV1,
} from "../model/documents.ts";
import { composeCampaignDocument } from "../compose.ts";
import {
  childLogicalPath as p,
  expectArrayOf,
  expectEnum,
  expectExactTable,
  expectFullCommitId,
  expectLowercaseSlug,
  expectMarkdown,
  expectRoadmapId,
  expectSafeInteger,
  expectSha256,
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

export const CAMPAIGN_ENUM_FIELDS: readonly EnumSchemaField[] = [
  { name: "matrix_authority", values: ["legacy_markdown", "shadow", "authoritative"] },
  { name: "testing_authority", values: ["legacy_markdown", "shadow", "authoritative"] },
  { name: "work_kind", values: ["defect", "regression_gap", "coverage_cell", "missing_system", "feature", "optimization", "documentation_integrity", "infrastructure"] },
  { name: "reservation_roadmap_path", values: ["cddl-matrix/ROADMAP.md", "tests/TESTING_ROADMAP.md"] },
  { name: "selection_target_kind", values: ["active_id", "legacy_markdown_reservation"] },
  { name: "selected_state", values: ["selected", "in_progress"] },
] as const;

export const CAMPAIGN_SCHEMA_ROWS: readonly ExactSchemaRow[] = [
  { name: "campaign root", required: ["campaign"], optional: ["legacy_markdown_reservation", "selection"] },
  { name: "campaign document", required: ["schema_version", "matrix_authority", "testing_authority"] },
  { name: "legacy Markdown reservation", required: ["id", "work_kind", "roadmap_path", "source_title", "source_start_byte", "source_end_byte", "source_sha256", "whole_source_sha256"] },
  { name: "selected campaign item", required: ["item_id", "target_kind", "selected_state", "priority_class", "selection_reason_md", "cycle", "remaining_scope_md"], optional: ["assignee"], forbidden: ["pickup_commit", "roadmap_path", "source_title", "source_start_byte", "source_end_byte", "source_sha256", "whole_source_sha256"] },
  { name: "in-progress campaign item", required: ["item_id", "target_kind", "selected_state", "priority_class", "selection_reason_md", "cycle", "remaining_scope_md", "assignee", "pickup_commit"], forbidden: ["roadmap_path", "source_title", "source_start_byte", "source_end_byte", "source_sha256", "whole_source_sha256"] },
] as const;

const WORK_KINDS = ["defect", "regression_gap", "coverage_cell", "missing_system", "feature", "optimization", "documentation_integrity", "infrastructure"] as const;

function decodeReservation(ctx: DecodeContext, raw: unknown, path: string): LegacyMarkdownReservationV1 {
  const table = expectExactTable(ctx, raw, path, CAMPAIGN_SCHEMA_ROWS[2]);
  const roadmapPath = expectEnum(
    ctx,
    requiredValue(table, "roadmap_path"),
    ["cddl-matrix/ROADMAP.md", "tests/TESTING_ROADMAP.md"] as const,
    p(path, "roadmap_path"),
  );
  const id = expectRoadmapId(ctx, requiredValue(table, "id"), p(path, "id"));
  const namespace = roadmapPath === "cddl-matrix/ROADMAP.md" ? "matrix" : "testing";
  if (!id.startsWith(`${namespace}.`)) {
    schemaFail(ctx, "E-ID-NAMESPACE", p(path, "id"), `reservation ID must use the ${namespace} namespace`);
  }
  const start = expectSafeInteger(ctx, requiredValue(table, "source_start_byte"), p(path, "source_start_byte"));
  const end = expectSafeInteger(ctx, requiredValue(table, "source_end_byte"), p(path, "source_end_byte"));
  if (end <= start) schemaFail(ctx, "E-SCHEMA-FLOOR", path, "reservation source range must be nonempty and forward");
  return {
    id,
    work_kind: expectEnum(ctx, requiredValue(table, "work_kind"), WORK_KINDS, p(path, "work_kind")),
    roadmap_path: roadmapPath,
    source_title: expectString(ctx, requiredValue(table, "source_title"), p(path, "source_title")),
    source_start_byte: start,
    source_end_byte: end,
    source_sha256: expectSha256(ctx, requiredValue(table, "source_sha256"), p(path, "source_sha256")),
    whole_source_sha256: expectSha256(ctx, requiredValue(table, "whole_source_sha256"), p(path, "whole_source_sha256")),
  };
}

function decodeSelection(ctx: DecodeContext, raw: unknown, path: string): CampaignSelectionV1 {
  const pre = expectExactTable(ctx, raw, path, {
    name: "campaign selection discriminator",
    required: ["item_id", "target_kind", "selected_state"],
    optional: ["priority_class", "selection_reason_md", "cycle", "remaining_scope_md", "assignee", "pickup_commit", "roadmap_path", "source_title", "source_start_byte", "source_end_byte", "source_sha256", "whole_source_sha256"],
  });
  const state = expectEnum(ctx, requiredValue(pre, "selected_state"), ["selected", "in_progress"] as const, p(path, "selected_state"));
  const table = expectExactTable(ctx, raw, path, CAMPAIGN_SCHEMA_ROWS[state === "selected" ? 3 : 4]);
  return {
    item_id: expectRoadmapId(ctx, requiredValue(table, "item_id"), p(path, "item_id")),
    target_kind: expectEnum(ctx, requiredValue(table, "target_kind"), ["active_id", "legacy_markdown_reservation"] as const, p(path, "target_kind")),
    selected_state: state,
    priority_class: expectLowercaseSlug(ctx, requiredValue(table, "priority_class"), p(path, "priority_class")),
    selection_reason_md: expectMarkdown(ctx, requiredValue(table, "selection_reason_md"), p(path, "selection_reason_md")),
    cycle: expectLowercaseSlug(ctx, requiredValue(table, "cycle"), p(path, "cycle")),
    remaining_scope_md: expectMarkdown(ctx, requiredValue(table, "remaining_scope_md"), p(path, "remaining_scope_md")),
    ...(hasOwn(table, "assignee") ? { assignee: expectString(ctx, optionalValue(table, "assignee"), p(path, "assignee")) } : {}),
    ...(state === "in_progress"
      ? { pickup_commit: expectFullCommitId(ctx, requiredValue(table, "pickup_commit"), p(path, "pickup_commit")) }
      : {}),
  };
}

export function decodeCampaignFromBindings(bindings: MarkdownBindings, schemaTrace?: SchemaDecodeTrace): CampaignDocumentV1 {
  const ctx: DecodeContext = { source: bindings.source, bindings, schema_trace: schemaTrace };
  const root = expectExactTable(ctx, bindings.parsed, "$", CAMPAIGN_SCHEMA_ROWS[0]);
  const campaign = expectExactTable(ctx, requiredValue(root, "campaign"), "campaign", CAMPAIGN_SCHEMA_ROWS[1]);
  const version = requiredValue(campaign, "schema_version");
  if (version !== 1) schemaFail(ctx, "E-SCHEMA-VERSION", "campaign.schema_version", "WP1 accepts campaign schema version 1 only");
  const reservations = hasOwn(root, "legacy_markdown_reservation")
    ? expectArrayOf(ctx, optionalValue(root, "legacy_markdown_reservation"), "legacy_markdown_reservation", (entry, path) => decodeReservation(ctx, entry, path))
    : [];
  const selections = hasOwn(root, "selection")
    ? expectArrayOf(ctx, optionalValue(root, "selection"), "selection", (entry, path) => decodeSelection(ctx, entry, path))
    : [];
  reservations.sort((left, right) => left.id < right.id ? -1 : left.id > right.id ? 1 : 0);
  selections.sort((left, right) => left.item_id < right.item_id ? -1 : left.item_id > right.item_id ? 1 : 0);
  const doc: CampaignDocumentV1 = {
    campaign: {
      schema_version: 1,
      matrix_authority: expectEnum(ctx, requiredValue(campaign, "matrix_authority"), ["legacy_markdown", "shadow", "authoritative"] as const, "campaign.matrix_authority"),
      testing_authority: expectEnum(ctx, requiredValue(campaign, "testing_authority"), ["legacy_markdown", "shadow", "authoritative"] as const, "campaign.testing_authority"),
    },
    legacy_markdown_reservations: reservations,
    selections,
  };
  bindings.assertAllConsumed();
  return doc;
}

export function decodeCampaignSource(
  bytes: Uint8Array,
  source: string,
  requireCanonical = true,
  schemaTrace?: SchemaDecodeTrace,
): CampaignDocumentV1 {
  const doc = decodeCampaignFromBindings(shieldTomlMarkdown(bytes, source), schemaTrace);
  if (requireCanonical && !bytesEqual(bytes, composeCampaignDocument(doc))) {
    throw new RoadmapWireError({ code: "E-TOML-NONCANONICAL", source, logical_path: "$", message: "campaign TOML bytes do not equal canonical composition", exit: 1 });
  }
  return doc;
}
