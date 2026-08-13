import type { RoadmapIssue } from "./errors.ts";
import type { RecordNode, RenderNodeKind, RoadmapDocument } from "./model/documents.ts";
import type { CompletedRenderIr, ProjectedFieldSegment } from "./render_ir.ts";
import { bytesEqual } from "./kernel.ts";
import { concatenate } from "./kernel.ts";
import { recordStatusFacts } from "./payload_descriptors.ts";
import { placeholderFor } from "./slots.ts";

export type ProjectionContentView = "full" | "audit";
export type ContentTransformation =
  | { readonly kind: "identity" }
  /** A section body with its declared `{{slot:<id>}}` placeholders replaced by resolved bytes. */
  | {
    readonly kind: "section_slots";
    readonly resolved: readonly { readonly slot_id: string; readonly bytes: Uint8Array }[];
  }
  | { readonly kind: "testing_next_heading" }
  | { readonly kind: "testing_standing_heading" }
  | { readonly kind: "testing_next_ordinal"; readonly ordinal: string };

export interface AuthoredContentCoordinate {
  readonly owner_kind: RenderNodeKind | "relation";
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
  readonly issues: readonly RoadmapIssue[];
}

interface AuthoredField extends AuthoredContentCoordinate { readonly bytes: Uint8Array }
interface LocalBinding extends AuthoredContentCoordinate {
  readonly transformation: ContentTransformation;
  readonly start: number;
  readonly end: number;
}
interface Piece {
  readonly owner: {
    readonly kind: RenderNodeKind | "generated";
    readonly id: string;
    readonly field: string;
  };
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

function authoredFields(document: RoadmapDocument): readonly AuthoredField[] {
  const fields: AuthoredField[] = [];
  const structural = (ownerKind: "section" | "part", ownerId: string,
    value: object): void => {
    markdownFields(value, "", (path, bytes) => {
      if (path === ".body_md") fields.push({ owner_kind: ownerKind,
        owner_id: ownerId, logical_path: path.slice(1), bytes });
    });
  };
  document.sections.forEach((value) => structural("section", value.section_id, value));
  document.parts.forEach((value) => structural("part", value.part_id, value));
  for (const record of document.records) {
    markdownFields(record.payload, "payload", (path, bytes) => fields.push({ owner_kind: "record",
      owner_id: record.id, logical_path: path, bytes }));
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
    case "section_slots": {
      let out = authored;
      for (const item of transformation.resolved) {
        const placeholder = UTF8.encode(placeholderFor(item.slot_id));
        const at = indexOfBytes(out, placeholder);
        if (at === -1 || indexOfBytes(out, placeholder, at + 1) !== -1) return undefined;
        out = concatenate([
          out.subarray(0, at),
          item.bytes,
          out.subarray(at + placeholder.byteLength),
        ]);
      }
      return out;
    }
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

function indexOfBytes(haystack: Uint8Array, needle: Uint8Array, from = 0): number {
  outer: for (let index = from; index + needle.byteLength <= haystack.byteLength; index++) {
    for (let offset = 0; offset < needle.byteLength; offset++) {
      if (haystack[index + offset] !== needle[offset]) continue outer;
    }
    return index;
  }
  return -1;
}

function chunkSegments(completed: CompletedRenderIr, owner: Piece["owner"]): readonly ProjectedFieldSegment[] {
  if (owner.kind === "generated") return [];
  return completed.projected_field_segments.filter((segment) =>
    segment.owner_kind === owner.kind && segment.owner_id === owner.id
  );
}

/**
 * One chunk becomes one piece. A section whose prose places slots binds its authored `body_md`
 * ONCE, over the whole resolved chunk, through the `section_slots` transformation — the interleaved
 * runs are not separate authored coordinates, so exactly-once reachability is unchanged.
 */
function basePieces(completed: CompletedRenderIr): Piece[] {
  return completed.chunks.map((chunk, chunkIndex) => {
    const resolutions = completed.slot_resolutions.filter((item) =>
      item.plan_index === completed.chunks[chunkIndex]!.plan_index &&
      item.section_id === chunk.owner.id
    );
    if (resolutions.length > 0) {
      return {
        owner: chunk.owner,
        bytes: new Uint8Array(chunk.bytes),
        bindings: [{
          owner_kind: chunk.owner.kind,
          owner_id: chunk.owner.id,
          logical_path: "body_md",
          transformation: {
            kind: "section_slots" as const,
            resolved: Object.freeze([...resolutions]
              .sort((left, right) => left.start_in_chunk - right.start_in_chunk)
              .map((item) => Object.freeze({
                slot_id: String(item.slot.slot_id),
                bytes: new Uint8Array(item.resolution?.bytes ?? new Uint8Array()),
              }))),
          },
          start: 0,
          end: chunk.bytes.byteLength,
        }],
      };
    }
    return {
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
    };
  });
}

/**
 * Apply a layout transformation to the piece's LEADING authored field.  The bindings must tile the
 * piece contiguously from byte zero (a record's prose slots and a single whole-field owner both
 * do); the transformation rewrites the first binding's exact byte range — its expected prefix
 * lives there — and every later binding shifts by the byte delta, keeping the per-field
 * reachability ledger exact.  A single whole-piece binding is the degenerate case.
 */
function transformLeadingField(
  document: RoadmapDocument,
  piece: Piece,
  transformation: ContentTransformation,
  issues: RoadmapIssue[],
): Piece {
  const first = piece.bindings[0];
  const contiguous = first !== undefined && first.start === 0 &&
    piece.bindings.every((binding, index) =>
      index === 0 || binding.start === piece.bindings[index - 1]!.end) &&
    piece.bindings[piece.bindings.length - 1]!.end === piece.bytes.byteLength;
  const transformed = first === undefined
    ? undefined
    : transformedBytes(transformation, piece.bytes.subarray(first.start, first.end));
  if (first === undefined || !contiguous || transformed === undefined) {
    issues.push(issue(document, `projection.layout.${piece.owner.kind}.${piece.owner.id}`,
      `layout transform ${transformation.kind} requires contiguous exact field sources with the expected prefix leading`));
    return piece;
  }
  const delta = transformed.byteLength - (first.end - first.start);
  return {
    ...piece,
    bytes: concatenate([transformed, piece.bytes.subarray(first.end)]),
    bindings: [
      { ...first, transformation, start: 0, end: transformed.byteLength },
      ...piece.bindings.slice(1).map((binding) => ({
        ...binding, start: binding.start + delta, end: binding.end + delta,
      })),
    ],
  };
}

/**
 * The rendered status line: typed-state layout syntax over `recordStatusFacts`, so what it SHOWS
 * is derived entirely from the descriptor table (kind, arm discriminants, risk where the arm
 * carries one) and a new state can never silently miss it.  Only the joining/wrapping syntax
 * lives here.
 */
function statusLineText(record: RecordNode): string {
  const facts = recordStatusFacts(record.payload);
  const parts = [
    String(facts.kind),
    ...facts.discriminants.map(([path, value]) => `${path}=${value}`),
    ...(facts.risk === undefined ? [] : [`risk=${facts.risk}`]),
  ];
  return `<sub>${parts.join(" · ")}</sub>`;
}

/** Every section-placed record renders behind its stable anchor plus its typed status line. */
function anchorPiece(piece: Piece, record: RecordNode): Piece {
  const text = TEXT.decode(piece.bytes);
  const indentation = /^( *)/u.exec(text)?.[1] ?? "";
  const gap = /^ *#{1,6} /u.test(text) ? "\n\n" : "\n";
  const prefix = UTF8.encode(
    `${indentation}<a id="roadmap-id-${piece.owner.id}"></a>\n${indentation}${statusLineText(record)}${gap}`,
  );
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
  return { owner: { kind: "generated", id, field: "generated" }, bytes, bindings: [] };
}

function layoutPieces(document: RoadmapDocument, completed: CompletedRenderIr, issues: RoadmapIssue[]): Piece[] {
  let pieces = basePieces(completed);
  if (document.document.roadmap === "testing") {
    // The testing layout transforms key on the live document's section vocabulary. Each applies
    // exactly where its section exists (a synthetic document without one simply has nothing to
    // transform); more than one owner of a transform's section is still a hard error, and the
    // committed-projection drift comparison owns the live document's byte outcome.
    const nextSections = pieces.filter((piece) => piece.owner.kind === "section" && piece.owner.id === "next-priority");
    const standingSections = pieces.filter((piece) => piece.owner.kind === "section" && piece.owner.id === "standing-system");
    if (nextSections.length > 1) issues.push(issue(document, "projection.layout.next-priority",
      `testing layout requires at most one next-priority section, found ${nextSections.length}`));
    if (standingSections.length > 1) issues.push(issue(document, "projection.layout.standing-system",
      `testing layout requires at most one standing-system section, found ${standingSections.length}`));
    pieces = pieces.map((piece) => {
      if (piece === nextSections[0]) return transformLeadingField(document, piece,
        { kind: "testing_next_heading" }, issues);
      if (piece === standingSections[0]) return transformLeadingField(document, piece,
        { kind: "testing_standing_heading" }, issues);
      if (piece.owner.kind !== "record") return piece;
      const record = document.records.find((candidate) => candidate.id === piece.owner.id);
      const aliases = record?.legacy_aliases?.filter((alias) => /^Next work [0-9]+$/u.test(alias)) ?? [];
      if (aliases.length === 0) return piece;
      if (aliases.length !== 1) {
        issues.push(issue(document, `projection.layout.record.${piece.owner.id}`,
          `testing Next-work record requires exactly one ordinal alias, found ${aliases.length}`));
        return piece;
      }
      return transformLeadingField(document, piece,
        { kind: "testing_next_ordinal", ordinal: aliases[0]!.slice("Next work ".length) }, issues);
    });
    const ordinals = document.records.flatMap((record) => record.legacy_aliases?.flatMap((alias) =>
      /^Next work ([0-9]+)$/u.exec(alias)?.[1] ?? []) ?? []);
    if (new Set(ordinals).size !== ordinals.length) issues.push(issue(document, "projection.layout.next-ordinals",
      "testing Next-work ordinal aliases must be unique"));

    const starts = pieces.flatMap((piece, index) =>
      piece.owner.kind === "section" && piece.owner.id === "operational-watches" ? [index] : []);
    const start = starts[0];
    const end = start === undefined ? -1 : pieces.findIndex((piece, index) => index > start && piece.owner.kind === "section");
    if (starts.length > 1 || (start !== undefined && end <= start)) {
      issues.push(issue(document, "projection.layout.operational-watches",
        "testing layout requires one bounded operational-watches section"));
    } else if (start !== undefined) {
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
          `testing operational bucket ${kind} has ${buckets[kind].length} pieces below floor ${OPERATIONAL_BUCKET_FLOORS[kind]}`));
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

  const recordsById = new Map(document.records.map((record) => [String(record.id), record]));
  const visible = new Set(document.sections
    .flatMap((section) => [...section.entries])
    .filter((id) => recordsById.has(id)));
  let anchored = 0;
  pieces = pieces.map((piece) => {
    if (piece.owner.kind !== "record" || !visible.has(piece.owner.id)) return piece;
    const record = recordsById.get(piece.owner.id);
    if (record === undefined) return piece; // unreachable (visible ⊆ declared records); trips the anchored-count issue below
    anchored++;
    return anchorPiece(piece, record);
  });
  if (anchored !== visible.size) issues.push(issue(document, "projection.layout.anchors",
    `layout anchored ${anchored} records but ${visible.size} are section-placed`));
  return [generatedPiece("layout-ownership-banner", UTF8.encode(
    `<!-- GENERATED FILE: owned by ${document.document.source_path}; edit that TOML source and run project_roadmaps.ts --write. -->\n\n`,
  )), ...pieces];
}

function materializeFull(
  document: RoadmapDocument,
  completed: CompletedRenderIr,
  issues: RoadmapIssue[],
): { readonly bytes: Uint8Array; readonly bindings: readonly LocalBinding[] } {
  const pieces = layoutPieces(document, completed, issues);
  const bytes = concatenate(pieces.map((piece) => piece.bytes));
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
): ProjectionViews {
  const issues: RoadmapIssue[] = [];
  const fields = authoredFields(document);
  const fieldsByKey = new Map(fields.map((field) => [key(field), field]));
  const full = materializeFull(document, completed, issues);
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
  return Object.freeze({ full: full.bytes, audit: audit.bytes,
    content_reachability: Object.freeze(ledger),
    issues: Object.freeze(issues) });
}
