import type { RoadmapIssue } from "./errors.ts";
import type { ManifestEntry, RoadmapDocument, SourceReplacement } from "./model/documents.ts";
import { projectionLayout, projectionLayoutRank } from "./projection_layout.ts";
import type { CompletedRenderIr, ProjectedFieldSegment } from "./render_ir.ts";
import { bytesEqual } from "./kernel.ts";
import { concatenate } from "./kernel.ts";

export type ProjectionContentView = "full" | "audit";
export type ContentTransformation =
  | { readonly kind: "identity" }
  | { readonly kind: "testing_next_heading" }
  | { readonly kind: "testing_standing_heading" }
  | { readonly kind: "testing_next_ordinal"; readonly ordinal: string };

export interface AuthoredContentCoordinate {
  readonly owner_kind: ManifestEntry["kind"] | "relation" | "source_replacement";
  readonly owner_id: string;
  readonly logical_path: string;
}

/** Metadata-only binding from one authored field to one exact final-view byte range. */
export interface ContentReachabilityLedgerEntry extends AuthoredContentCoordinate {
  readonly view: ProjectionContentView;
  readonly byte_length: number;
  readonly sha256: string;
  readonly transformation: ContentTransformation;
  readonly output_start_byte: number;
  readonly output_end_byte: number;
  readonly output_byte_length: number;
  readonly output_sha256: string;
}

export interface ProjectionViews {
  readonly full: Uint8Array;
  /** Human-readable audit projection containing audit-assigned authored Markdown only. */
  readonly audit: Uint8Array;
  readonly content_reachability: readonly ContentReachabilityLedgerEntry[];
  readonly legacy_span_provenance: readonly LegacySpanProvenanceLedgerEntry[];
  readonly issues: readonly RoadmapIssue[];
}

export interface LegacySpanProvenanceLedgerEntry {
  readonly span_id: string;
  readonly source_kind: ManifestEntry["kind"];
  readonly owner_id: string;
  readonly owner_field: string;
  readonly migration_status: "raw" | "replaced" | "generated";
  readonly start_byte: number;
  readonly end_byte: number;
  readonly sha256: string;
}

interface AuthoredField extends AuthoredContentCoordinate { readonly bytes: Uint8Array }
interface LocalBinding extends AuthoredContentCoordinate {
  readonly transformation: ContentTransformation;
  readonly start: number;
  readonly end: number;
}
interface Piece {
  readonly owner: CompletedRenderIr["chunks"][number]["owner"];
  readonly bytes: Uint8Array;
  readonly bindings: readonly LocalBinding[];
}

const cp = (left: string, right: string): number => left < right ? -1 : left > right ? 1 : 0;
const UTF8 = new TextEncoder();
const TEXT = new TextDecoder("utf-8", { fatal: true });
const IDENTITY: ContentTransformation = Object.freeze({ kind: "identity" });

function issue(document: RoadmapDocument, logicalPath: string, message: string): RoadmapIssue {
  return { code: "E-FIELD-CONSUMPTION", source: document.document.source_path,
    logical_path: logicalPath, message, exit: 1 };
}

function digest(bytes: Uint8Array): string {
  return new Bun.CryptoHasher("sha256").update(bytes).digest("hex");
}

function key(value: AuthoredContentCoordinate): string {
  return JSON.stringify([value.owner_kind, value.owner_id, value.logical_path]);
}

function markdownFields(value: unknown, logicalPath: string, add: (path: string, bytes: Uint8Array) => void): void {
  if (value instanceof Uint8Array) {
    if (logicalPath.slice(logicalPath.lastIndexOf(".") + 1).endsWith("_md")) add(logicalPath, value);
    return;
  }
  if (Array.isArray(value)) {
    value.forEach((entry, index) => markdownFields(entry, `${logicalPath}[${index}]`, add));
    return;
  }
  if (value !== null && typeof value === "object") {
    for (const name of Object.keys(value).sort(cp)) {
      markdownFields((value as Record<string, unknown>)[name], `${logicalPath}.${name}`, add);
    }
  }
}

function replacementFields(
  ownerKind: Exclude<ManifestEntry["kind"], "generated_slot">,
  ownerId: string,
  replacements: readonly SourceReplacement[],
  fields: AuthoredField[],
): void {
  replacements.forEach((replacement) => fields.push({ owner_kind: "source_replacement",
    owner_id: `${ownerKind}:${ownerId}:${replacement.span_id}`, logical_path: "review_note_md",
    bytes: replacement.review_note_md }));
}

function authoredFields(document: RoadmapDocument): readonly AuthoredField[] {
  const fields: AuthoredField[] = [];
  const structural = (ownerKind: "section" | "fragment" | "legacy_marker" | "part", ownerId: string,
    value: object): void => {
    markdownFields(value, "", (path, bytes) => {
      if (path === ".body_md" || path === ".marker_md") fields.push({ owner_kind: ownerKind,
        owner_id: ownerId, logical_path: path.slice(1), bytes });
    });
    if ("source_replacements" in value) replacementFields(ownerKind, ownerId,
      value.source_replacements as readonly SourceReplacement[], fields);
  };
  document.sections.forEach((value) => structural("section", value.section_id, value));
  document.fragments.forEach((value) => structural("fragment", value.fragment_id, value));
  document.legacy_markers.forEach((value) => structural("legacy_marker", value.marker_id, value));
  document.parts.forEach((value) => structural("part", value.part_id, value));
  for (const record of document.records) {
    markdownFields(record.payload, "payload", (path, bytes) => fields.push({ owner_kind: "record",
      owner_id: record.id, logical_path: path, bytes }));
    replacementFields("record", record.id, record.source_replacements, fields);
  }
  document.relations.forEach((relation, index) => {
    if (relation.note_md !== undefined) fields.push({ owner_kind: "relation",
      owner_id: `${relation.source}:${relation.kind}:${relation.target}:${index}`,
      logical_path: "note_md", bytes: relation.note_md });
  });
  return fields.sort((left, right) => cp(key(left), key(right)));
}

function transformedBytes(transformation: ContentTransformation, authored: Uint8Array): Uint8Array | undefined {
  const text = TEXT.decode(authored);
  switch (transformation.kind) {
    case "identity": return new Uint8Array(authored);
    case "testing_next_heading": {
      const from = "## Next work items, in priority order";
      if (!text.startsWith(`${from}\n`) || text.indexOf(from, from.length) !== -1) return undefined;
      return UTF8.encode(`## Next work${text.slice(from.length)}`);
    }
    case "testing_standing_heading":
      return /^## Standing-system residuals(?:\n|$)/u.test(text)
        ? undefined
        : UTF8.encode(`## Standing-system residuals\n\n${text}`);
    case "testing_next_ordinal": {
      const prefix = `${transformation.ordinal}. `;
      return text.startsWith(prefix) ? UTF8.encode(`- ${text.slice(prefix.length)}`) : undefined;
    }
  }
}

export function validateContentReachability(
  document: RoadmapDocument,
  ledger: readonly ContentReachabilityLedgerEntry[],
  full: Uint8Array,
  audit: Uint8Array,
): readonly RoadmapIssue[] {
  const expected = new Map(authoredFields(document).map((field) => [key(field), field]));
  const counts = new Map<string, number>();
  const ranges: Record<ProjectionContentView, { start: number; end: number; coordinate: string }[]> = {
    full: [], audit: [],
  };
  const issues: RoadmapIssue[] = [];
  for (const entry of ledger) {
    const coordinate = key(entry);
    counts.set(coordinate, (counts.get(coordinate) ?? 0) + 1);
    const field = expected.get(coordinate);
    const output = entry.view === "full" ? full : audit;
    const transformed = field === undefined ? undefined : transformedBytes(entry.transformation, field.bytes);
    const rangeValid = Number.isSafeInteger(entry.output_start_byte) && Number.isSafeInteger(entry.output_end_byte) &&
      entry.output_start_byte >= 0 && entry.output_start_byte < entry.output_end_byte &&
      entry.output_end_byte <= output.byteLength;
    const slice = rangeValid ? output.slice(entry.output_start_byte, entry.output_end_byte) : new Uint8Array();
    if (field === undefined) {
      issues.push(issue(document, `projection.content_reachability.${coordinate}`,
        "view ledger coordinate has no authored Markdown owner"));
    } else if (entry.byte_length !== field.bytes.byteLength || entry.sha256 !== digest(field.bytes)) {
      issues.push(issue(document, `projection.content_reachability.${coordinate}`,
        "view ledger authored length/digest do not exactly match their Markdown field"));
    } else if (entry.view === "audit" && entry.transformation.kind !== "identity") {
      issues.push(issue(document, `projection.content_reachability.${coordinate}`,
        "audit-assigned Markdown must use the identity transformation"));
    } else if (transformed === undefined || !rangeValid ||
      entry.output_byte_length !== transformed.byteLength || entry.output_sha256 !== digest(transformed) ||
      !bytesEqual(slice, transformed)) {
      issues.push(issue(document, `projection.content_reachability.${coordinate}`,
        "view ledger transformation is not bound to its exact final-view byte range"));
    }
    if (rangeValid) ranges[entry.view].push({ start: entry.output_start_byte,
      end: entry.output_end_byte, coordinate });
  }
  for (const coordinate of [...expected.keys()].sort(cp)) {
    const count = counts.get(coordinate) ?? 0;
    if (count !== 1) issues.push(issue(document, `projection.content_reachability.${coordinate}`,
      `authored Markdown coordinate is assigned ${count} times across full/audit views, expected exactly once`));
  }
  for (const view of ["full", "audit"] as const) {
    const sorted = ranges[view].sort((left, right) => left.start - right.start || left.end - right.end);
    for (let index = 1; index < sorted.length; index++) if (sorted[index - 1]!.end > sorted[index]!.start) {
      issues.push(issue(document, `projection.content_reachability.${view}`,
        `final-view byte ranges overlap for ${sorted[index - 1]!.coordinate} and ${sorted[index]!.coordinate}`));
    }
  }
  return Object.freeze(issues);
}

export function validateLegacySpanProvenance(
  document: RoadmapDocument,
  ledger: readonly LegacySpanProvenanceLedgerEntry[],
): readonly RoadmapIssue[] {
  const expected = new Map(document.spans.map((span) => [span.id, span]));
  const counts = new Map<string, number>();
  const issues: RoadmapIssue[] = [];
  for (const entry of ledger) {
    counts.set(entry.span_id, (counts.get(entry.span_id) ?? 0) + 1);
    const span = expected.get(entry.span_id as never);
    if (span === undefined || span.source_kind !== entry.source_kind || span.owner_id !== entry.owner_id ||
      span.owner_field !== entry.owner_field || span.migration_status !== entry.migration_status ||
      span.start_byte !== entry.start_byte || span.end_byte !== entry.end_byte || span.sha256 !== entry.sha256) {
      issues.push(issue(document, `projection.legacy_span_provenance.${JSON.stringify(entry.span_id)}`,
        "legacy span provenance row does not exactly match its frozen source span"));
    }
  }
  for (const id of [...expected.keys()].sort(cp)) {
    const count = counts.get(id) ?? 0;
    if (count !== 1) issues.push(issue(document, `projection.legacy_span_provenance.${JSON.stringify(id)}`,
      `frozen source span is reported ${count} times, expected exactly once`));
  }
  return Object.freeze(issues);
}

function legacySpanProvenance(document: RoadmapDocument): readonly LegacySpanProvenanceLedgerEntry[] {
  return Object.freeze([...document.spans].sort((left, right) => left.start_byte - right.start_byte || cp(left.id, right.id))
    .map((span) => Object.freeze({ span_id: span.id, source_kind: span.source_kind,
      owner_id: span.owner_id, owner_field: span.owner_field, migration_status: span.migration_status,
      start_byte: span.start_byte, end_byte: span.end_byte, sha256: span.sha256 })));
}

function chunkSegments(completed: CompletedRenderIr, owner: Piece["owner"]): readonly ProjectedFieldSegment[] {
  if (owner.kind === "generated_slot") return [];
  return completed.projected_field_segments.filter((segment) =>
    segment.owner_kind === owner.kind && segment.owner_id === owner.id
  );
}

function basePieces(completed: CompletedRenderIr): Piece[] {
  return completed.chunks.map((chunk) => ({
    owner: chunk.owner,
    bytes: new Uint8Array(chunk.bytes),
    bindings: chunkSegments(completed, chunk.owner).map((segment) => ({
      owner_kind: segment.owner_kind,
      owner_id: segment.owner_id,
      logical_path: segment.logical_path,
      transformation: IDENTITY,
      start: segment.start_in_chunk,
      end: segment.end_in_chunk,
    })),
  }));
}

function transformWholePiece(
  document: RoadmapDocument,
  piece: Piece,
  transformation: ContentTransformation,
  issues: RoadmapIssue[],
): Piece {
  const binding = piece.bindings[0];
  const transformed = transformedBytes(transformation, piece.bytes);
  if (piece.bindings.length !== 1 || binding === undefined || binding.start !== 0 ||
    binding.end !== piece.bytes.byteLength || transformed === undefined) {
    issues.push(issue(document, `projection.layout.${piece.owner.kind}.${piece.owner.id}`,
      `curated layout transform ${transformation.kind} requires one exact whole-field source with its expected prefix`));
    return piece;
  }
  return { ...piece, bytes: transformed, bindings: [{ ...binding, transformation, start: 0, end: transformed.byteLength }] };
}

function anchorPiece(piece: Piece): Piece {
  const text = TEXT.decode(piece.bytes);
  const indentation = /^( *)/u.exec(text)?.[1] ?? "";
  const gap = /^ *#{1,6} /u.test(text) ? "\n\n" : "\n";
  const prefix = UTF8.encode(`${indentation}<a id="roadmap-id-${piece.owner.id}"></a>${gap}`);
  return { ...piece, bytes: concatenate([prefix, piece.bytes]), bindings: piece.bindings.map((binding) => ({
    ...binding, start: binding.start + prefix.byteLength, end: binding.end + prefix.byteLength,
  })) };
}

type OperationalClass = "systems" | "live" | "history";
const OPERATIONAL_BUCKET_FLOORS: Readonly<Record<OperationalClass, number>> = Object.freeze({
  systems: 1,
  live: 1,
  history: 0,
});
function operationalClass(document: RoadmapDocument, ownerId: string): OperationalClass {
  const record = document.records.find((candidate) => String(candidate.id) === ownerId);
  if (record === undefined) return "systems";
  const payload = record.payload;
  if (payload.kind === "testing_operational_watch") return payload.watch_state === "watching" ? "live" : "history";
  if (payload.kind === "testing_incident") return payload.incident_posture === "live" ? "live" : "history";
  return "systems";
}

function generatedPiece(id: string, bytes: Uint8Array): Piece {
  return { owner: { kind: "generated_slot", id, field: "generated" }, bytes, bindings: [] };
}

function curatedPieces(document: RoadmapDocument, completed: CompletedRenderIr, issues: RoadmapIssue[]): Piece[] {
  let pieces = basePieces(completed);
  const rank = projectionLayoutRank(projectionLayout(document));
  if (document.document.roadmap === "testing") {
    const nextSections = rank >= 3
      ? pieces.filter((piece) => piece.owner.kind === "section" && piece.owner.id === "next-priority") : [];
    const standingSections = rank >= 2
      ? pieces.filter((piece) => piece.owner.kind === "section" && piece.owner.id === "standing-system") : [];
    if (rank >= 3 && nextSections.length !== 1) issues.push(issue(document, "projection.layout.next-priority",
      `unnumbered testing layout requires exactly one next-priority section, found ${nextSections.length}`));
    if (rank >= 2 && standingSections.length !== 1) issues.push(issue(document, "projection.layout.standing-system",
      `standing testing layout requires exactly one standing-system section, found ${standingSections.length}`));
    pieces = pieces.map((piece) => {
      if (rank >= 3 && piece === nextSections[0]) return transformWholePiece(document, piece,
        { kind: "testing_next_heading" }, issues);
      if (rank >= 2 && piece === standingSections[0]) return transformWholePiece(document, piece,
        { kind: "testing_standing_heading" }, issues);
      if (rank < 3 || piece.owner.kind !== "record") return piece;
      const record = document.records.find((candidate) => candidate.id === piece.owner.id);
      const aliases = record?.legacy_aliases?.filter((alias) => /^Next work [0-9]+$/u.test(alias)) ?? [];
      if (aliases.length === 0) return piece;
      if (aliases.length !== 1) {
        issues.push(issue(document, `projection.layout.record.${piece.owner.id}`,
          `curated testing Next-work record requires exactly one ordinal alias, found ${aliases.length}`));
        return piece;
      }
      return transformWholePiece(document, piece,
        { kind: "testing_next_ordinal", ordinal: aliases[0]!.slice("Next work ".length) }, issues);
    });
    const ordinals = rank < 3 ? [] : document.records.flatMap((record) => record.legacy_aliases?.flatMap((alias) =>
      /^Next work ([0-9]+)$/u.exec(alias)?.[1] ?? []) ?? []);
    if (new Set(ordinals).size !== ordinals.length) issues.push(issue(document, "projection.layout.next-ordinals",
      "curated testing Next-work ordinal aliases must be unique"));

    const starts = rank < 4 ? [] : pieces.flatMap((piece, index) =>
      piece.owner.kind === "section" && piece.owner.id === "operational-watches" ? [index] : []);
    const start = starts[0];
    const end = start === undefined ? -1 : pieces.findIndex((piece, index) => index > start && piece.owner.kind === "section");
    if (rank >= 4 && (starts.length !== 1 || start === undefined || end <= start)) {
      issues.push(issue(document, "projection.layout.operational-watches",
        "curated testing layout requires one bounded operational-watches section"));
    } else if (rank >= 4) {
      const buckets: Record<OperationalClass, Piece[]> = { systems: [], live: [], history: [] };
      let current: OperationalClass = "systems";
      for (const piece of pieces.slice(start + 1, end)) {
        if (piece.owner.kind === "record") current = operationalClass(document, piece.owner.id);
        buckets[current].push(piece);
      }
      for (const kind of ["systems", "live", "history"] as const) if (
        buckets[kind].length < OPERATIONAL_BUCKET_FLOORS[kind]
      ) {
        issues.push(issue(document, `projection.layout.operational-watches.${kind}`,
          `curated testing operational bucket ${kind} has ${buckets[kind].length} pieces below floor ${OPERATIONAL_BUCKET_FLOORS[kind]}`));
      }
      const headings: Readonly<Record<OperationalClass, Uint8Array>> = {
        systems: UTF8.encode("\n### Operational systems, controls, and resource work\n\n"),
        live: UTF8.encode("\n### Live operational watches\n\n"),
        history: UTF8.encode("\n### Attributed and historical operating guidance\n\n"),
      };
      const replacement = (["systems", "live", "history"] as const).flatMap((kind) =>
        buckets[kind].length === 0 ? [] : [generatedPiece(`layout-operational-${kind}`, headings[kind]), ...buckets[kind]]
      );
      replacement.push(generatedPiece("layout-operational-tail", UTF8.encode("\n")));
      pieces.splice(start + 1, end - start - 1, ...replacement);
    }
  }

  const visible = rank < 1 ? new Set<string>() : new Set(document.records.flatMap((record) =>
    "projection_visibility" in record && record.projection_visibility === "document" ? [String(record.id)] : []
  ));
  let anchored = 0;
  pieces = pieces.map((piece) => {
    if (piece.owner.kind !== "record" || !visible.has(piece.owner.id)) return piece;
    anchored++;
    return anchorPiece(piece);
  });
  if (anchored !== visible.size) issues.push(issue(document, "projection.layout.anchors",
    `curated layout anchored ${anchored} records but ${visible.size} are document-visible`));
  return rank < 1 ? pieces : [generatedPiece("layout-ownership-banner", UTF8.encode(
    `<!-- GENERATED FILE: owned by ${document.document.source_path}; edit that TOML source and run project_roadmaps.ts --write. -->\n\n`,
  )), ...pieces];
}

function materializeFull(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
  legacyProjection: Uint8Array,
  issues: RoadmapIssue[],
): { readonly bytes: Uint8Array; readonly bindings: readonly LocalBinding[] } {
  const pieces = projectionLayout(document) !== "legacy_v1"
    ? curatedPieces(document, completed, issues)
    : basePieces(completed);
  const bytes = projectionLayout(document) !== "legacy_v1"
    ? concatenate(pieces.map((piece) => piece.bytes))
    : new Uint8Array(legacyProjection);
  const bindings: LocalBinding[] = [];
  let offset = 0;
  for (const piece of pieces) {
    for (const binding of piece.bindings) bindings.push({ ...binding,
      start: binding.start + offset, end: binding.end + offset });
    offset += piece.bytes.byteLength;
  }
  if (offset !== bytes.byteLength) issues.push(issue(document, "projection.layout.bytes",
    "final projection bytes do not match their mechanically bound piece stream"));
  return { bytes, bindings };
}

function auditProjection(fields: readonly AuthoredField[]): {
  readonly bytes: Uint8Array;
  readonly bindings: readonly LocalBinding[];
} {
  const pieces: Uint8Array[] = [UTF8.encode("# Roadmap authored-content audit\n\n")];
  const bindings: LocalBinding[] = [];
  let offset = pieces[0]!.byteLength;
  for (const field of fields) {
    const heading = UTF8.encode(`## ${field.owner_kind} ${JSON.stringify(field.owner_id)} ${JSON.stringify(field.logical_path)}\n\n`);
    pieces.push(heading, field.bytes, UTF8.encode(field.bytes.at(-1) === 0x0a ? "\n" : "\n\n"));
    const start = offset + heading.byteLength;
    bindings.push({ ...field, transformation: IDENTITY, start, end: start + field.bytes.byteLength });
    offset += heading.byteLength + field.bytes.byteLength + pieces[pieces.length - 1]!.byteLength;
  }
  return { bytes: concatenate(pieces), bindings };
}

function ledgerEntry(
  field: AuthoredField,
  view: ProjectionContentView,
  binding: LocalBinding,
  output: Uint8Array,
): ContentReachabilityLedgerEntry {
  const transformed = output.slice(binding.start, binding.end);
  return Object.freeze({ owner_kind: field.owner_kind, owner_id: field.owner_id,
    logical_path: field.logical_path, view, byte_length: field.bytes.byteLength,
    sha256: digest(field.bytes), transformation: binding.transformation,
    output_start_byte: binding.start, output_end_byte: binding.end,
    output_byte_length: transformed.byteLength, output_sha256: digest(transformed) });
}

/** Build final and audit views, binding every authored Markdown field exactly once to final bytes. */
export function buildProjectionViews(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
  legacyProjection: Uint8Array,
): ProjectionViews {
  const provenance = legacySpanProvenance(document);
  const issues: RoadmapIssue[] = [];
  const fields = authoredFields(document);
  const fieldsByKey = new Map(fields.map((field) => [key(field), field]));
  const full = materializeFull(document, completed, legacyProjection, issues);
  const fullBindings = new Map<string, LocalBinding>();
  for (const binding of full.bindings) {
    const coordinate = key(binding);
    if (fullBindings.has(coordinate)) issues.push(issue(document,
      `projection.content_reachability.${coordinate}`, "full projection binds one authored coordinate more than once"));
    fullBindings.set(coordinate, binding);
    if (!fieldsByKey.has(coordinate)) issues.push(issue(document,
      `projection.content_reachability.${coordinate}`, "full projection binding has no authored Markdown coordinate"));
  }
  const auditFields = fields.filter((field) => !fullBindings.has(key(field)));
  const audit = auditProjection(auditFields);
  const auditBindings = new Map(audit.bindings.map((binding) => [key(binding), binding]));
  const ledger = fields.map((field) => {
    const coordinate = key(field);
    const fullBinding = fullBindings.get(coordinate);
    if (fullBinding !== undefined) return ledgerEntry(field, "full", fullBinding, full.bytes);
    const auditBinding = auditBindings.get(coordinate)!;
    return ledgerEntry(field, "audit", auditBinding, audit.bytes);
  });
  issues.push(...validateContentReachability(document, ledger, full.bytes, audit.bytes));
  issues.push(...validateLegacySpanProvenance(document, provenance));
  return Object.freeze({ full: full.bytes, audit: audit.bytes,
    content_reachability: Object.freeze(ledger), legacy_span_provenance: provenance,
    issues: Object.freeze(issues) });
}
