#!/usr/bin/env bun
/**
 * Decode-conformance catalog drift gate (design doc D6) — PURE FILE READS, no cargo, no oracles.
 *
 * Cross-checks the committed decode corpus `tests/decode_conformance/catalog.toml` against the matrix:
 *   1. Completeness — every matrix `supported` row id has a catalog row, and each catalog row carries
 *      EITHER >=1 vector OR a `pinned_reason` (never both, never neither).
 *   2. Staleness    — every catalog row id is still `supported` in matrix.json, and the catalog
 *      `example` string-equals the matrix row's `example` (a drifted example means the vectors were
 *      validated against a spec the matrix no longer describes — re-mint).
 *   3. Shape        — every `expect="reject"` vector has `class` ∈ {bug, limitation} AND a nonempty
 *      `reason` (a class-less pin is the mint's triage-pending state — RED); every hex is well-formed
 *      (nonempty, even length, lowercase); `spec`/`mode`/`type_name` are present together on an active
 *      row and consistent (mode ∈ {standalone, holder}; holder ⇒ spec starts with the holder prefix and
 *      type_name === "ProbeHolder"; standalone ⇒ spec === example); a pinned row carries none of them.
 *   4. Seeded controls — a hard-coded list of (row id, hex) that MUST exist as accept vectors: the
 *      absent-instance TDD anchors that catch an over-strict-decoder reintroduction. These are positive
 *      controls: they must PASS today.
 *   5. Vacuity floor — >= 80 supported matrix rows, so a broken matrix read can't pass an empty check.
 *
 * This script NEVER writes (there is nothing to project/rewrite in v1), so the DEFAULT run IS the check
 * — no `--check` flag is needed. A `--check` arg is accepted and ignored for symmetry with the
 * projection family (`--check` = CI drift mode elsewhere). Exit nonzero with a per-problem list on any
 * failure; a clean run prints a one-line summary (rows / vectors / pins by class).
 *
 * Run from cddl-matrix/:
 *   bun run project_decode_conformance.ts   -> the drift gate (default)
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;
const CATALOG_REL = "tests/decode_conformance/catalog.toml";
const HOLDER_PREFIX = "__probe_holder = [0, ";

// The absent-instance regression anchors (design D6 §4): each is a REAL minted holder/standalone
// instance whose acceptance proves the decoder still admits the "nothing there" shape an over-strict
// decoder would wrongly reject. They must be present as expect="accept" vectors on their row.
const SEEDED_CONTROLS: { id: string; hex: string; comment: string }[] = [
  {
    id: "occur.optional",
    hex: "820080",
    comment: "holder [0, []] — the `? name: tstr` optional member is ABSENT; anchors the over-strict " +
      "class that rejects a legally-absent optional field",
  },
  {
    id: "type2.map",
    hex: "8200a0",
    comment: "holder [0, {}] — an EMPTY `* tstr => int` table; nearest live relative of the fixed " +
      "single-field-map zero-occurrence narrowing, anchors rejection of an empty map",
  },
  {
    id: "occur.zero_or_more",
    hex: "820080",
    comment: "holder [0, []] — ZERO repetitions of a `* uint` element; anchors the array-side " +
      "over-strict class that rejects the empty-repetition instance",
  },
];

// --- matrix.json: supported ids + their examples --------------------------------------------------
interface MatrixLookupRow { id: string; example: string }
interface Annotation { id: string; status: string }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: MatrixLookupRow[];
  containment: MatrixLookupRow[];
  control_operators: MatrixLookupRow[];
}
const matrix = (await Bun.file(`${HERE}/matrix.json`).json()) as MatrixJson;
const exampleById = new Map<string, string>();
for (const arr of [matrix.features, matrix.containment, matrix.control_operators])
  for (const r of arr) exampleById.set(r.id, r.example);
const supported = new Set(
  matrix.annotations.cddl_codegen.filter(a => a.status === "supported").map(a => a.id),
);

// --- catalog.toml ---------------------------------------------------------------------------------
interface CatVector { hex?: unknown; source?: unknown; expect?: unknown; class?: unknown; reason?: unknown }
interface CatRow {
  id?: unknown; axis?: unknown; example?: unknown; pinned_reason?: unknown;
  spec?: unknown; mode?: unknown; type_name?: unknown; vector?: CatVector[];
}
const catalog = Bun.TOML.parse(readFileSync(`${HERE}/../${CATALOG_REL}`, "utf8")) as { row?: CatRow[] };
const rows = catalog.row ?? [];

const problems: string[] = [];
const catalogById = new Map<string, CatRow>();
const HEX_RE = /^[0-9a-f]+$/;

for (const r of rows) {
  const id = typeof r.id === "string" ? r.id : undefined;
  if (id === undefined) { problems.push(`catalog row is missing a string \`id\`: ${JSON.stringify(r)}`); continue; }
  if (catalogById.has(id)) problems.push(`duplicate catalog row id \`${id}\``);
  catalogById.set(id, r);

  const vectors = r.vector ?? [];
  const hasVectors = vectors.length > 0;
  const pinned = typeof r.pinned_reason === "string" && r.pinned_reason.length > 0;

  // Completeness §1: exactly one of {>=1 vector, pinned_reason}.
  if (hasVectors === pinned)
    problems.push(`\`${id}\`: a catalog row must have EITHER >=1 vector OR a nonempty pinned_reason, not ${hasVectors ? "both" : "neither"}`);

  // Staleness §2: id still supported, and the example matches the matrix verbatim.
  if (!supported.has(id))
    problems.push(`\`${id}\`: catalog row is not a \`supported\` matrix row (construct no longer supported — remove or re-mint)`);
  const matrixExample = exampleById.get(id);
  const catExample = typeof r.example === "string" ? r.example : undefined;
  if (matrixExample !== undefined && catExample !== matrixExample)
    problems.push(`\`${id}\`: catalog example drifted from the matrix example — re-mint\n    matrix : ${JSON.stringify(matrixExample)}\n    catalog: ${JSON.stringify(catExample)}`);

  if (pinned) {
    // A pinned row is vectorless AND carries none of the active-row fields.
    for (const f of ["spec", "mode", "type_name"] as const)
      if (r[f] !== undefined)
        problems.push(`\`${id}\`: pinned row must not carry \`${f}\` (it has no vectors to decode)`);
    continue;
  }
  if (!hasVectors) continue; // already reported above; nothing more to shape-check

  // Shape §3: an active row needs spec/mode/type_name, all consistent.
  const spec = typeof r.spec === "string" ? r.spec : undefined;
  const mode = typeof r.mode === "string" ? r.mode : undefined;
  const typeName = typeof r.type_name === "string" ? r.type_name : undefined;
  if (spec === undefined) problems.push(`\`${id}\`: active row is missing \`spec\``);
  if (typeName === undefined) problems.push(`\`${id}\`: active row is missing \`type_name\``);
  if (mode !== "standalone" && mode !== "holder")
    problems.push(`\`${id}\`: \`mode\` must be "standalone" or "holder" (got ${JSON.stringify(mode)})`);
  else if (mode === "holder") {
    if (spec !== undefined && !spec.startsWith(HOLDER_PREFIX))
      problems.push(`\`${id}\`: holder-mode spec must start with \`${HOLDER_PREFIX}\` (oracle root-rule targeting)`);
    if (typeName !== undefined && typeName !== "ProbeHolder")
      problems.push(`\`${id}\`: holder-mode type_name must be "ProbeHolder" (got ${JSON.stringify(typeName)})`);
  } else if (mode === "standalone") {
    if (spec !== undefined && catExample !== undefined && spec !== catExample)
      problems.push(`\`${id}\`: standalone-mode spec must equal the example verbatim`);
  }

  // Shape §3: per-vector hex + reject-pin class/reason.
  vectors.forEach((v, i) => {
    const where = `\`${id}\` vector[${i}]`;
    const hex = typeof v.hex === "string" ? v.hex : undefined;
    if (hex === undefined || hex.length === 0)
      problems.push(`${where}: missing/empty hex`);
    else if (hex.length % 2 !== 0)
      problems.push(`${where}: hex \`${hex}\` has odd length (not whole bytes)`);
    else if (!HEX_RE.test(hex))
      problems.push(`${where}: hex \`${hex}\` is not lowercase hex ([0-9a-f])`);
    const expect = v.expect;
    if (expect !== "accept" && expect !== "reject")
      problems.push(`${where}: \`expect\` must be "accept" or "reject" (got ${JSON.stringify(expect)})`);
    if (expect === "reject") {
      if (v.class !== "bug" && v.class !== "limitation")
        problems.push(`${where}: reject pin \`class\` must be "bug" or "limitation" (got ${JSON.stringify(v.class)}) — a class-less pin is triage-pending`);
      if (typeof v.reason !== "string" || v.reason.length === 0)
        problems.push(`${where}: reject pin needs a nonempty \`reason\` (the ledgered bug / doc citation)`);
    }
  });
}

// Completeness §1: every supported matrix row must have a catalog row.
for (const id of [...supported].sort())
  if (!catalogById.has(id))
    problems.push(`supported matrix row \`${id}\` has no catalog row (mint it or pin it)`);

// Seeded controls §4: each must be present as an accept vector on its row.
for (const c of SEEDED_CONTROLS) {
  const row = catalogById.get(c.id);
  if (!row) { problems.push(`seeded control \`${c.id}\` (${c.hex}): no catalog row — ${c.comment}`); continue; }
  const present = (row.vector ?? []).some(v => v.hex === c.hex && v.expect === "accept");
  if (!present)
    problems.push(`seeded control \`${c.id}\`: accept vector \`${c.hex}\` is missing — ${c.comment}`);
}

// Vacuity floor §5.
if (supported.size < 80)
  problems.push(`only ${supported.size} supported matrix rows (expected >= 80) — matrix read looks broken/empty`);

// --- report ---------------------------------------------------------------------------------------
const activeRows = rows.filter(r => (r.vector ?? []).length > 0);
const pinnedRows = rows.filter(r => typeof r.pinned_reason === "string" && r.pinned_reason.length > 0);
const allVectors = activeRows.flatMap(r => r.vector ?? []);
const accepts = allVectors.filter(v => v.expect === "accept").length;
const rejects = allVectors.filter(v => v.expect === "reject");
const rejectBug = rejects.filter(v => v.class === "bug").length;
const rejectLimitation = rejects.filter(v => v.class === "limitation").length;

if (problems.length) {
  console.log(`decode-conformance drift gate: ${problems.length} problem(s)`);
  for (const p of problems) console.log(`  FAIL ${p}`);
  process.exit(1);
}
console.log(
  `decode-conformance catalog OK — ${rows.length} rows (${activeRows.length} active / ${allVectors.length} vectors: ` +
    `${accepts} accept, ${rejects.length} reject) · ${pinnedRows.length} pinned · ` +
    `reject pins: ${rejectBug} bug, ${rejectLimitation} limitation · ${supported.size} supported matrix rows`,
);
process.exit(0);
