#!/usr/bin/env bun
/**
 * Q3 projection: render tests/golden_hex/COVERAGE.md from the matrix instead of hand-maintaining it.
 *
 * The map has two halves. The MECHANICAL half (covered / uncovered / which test) is DERIVED here from
 * the actual asserted test bytes. The JUDGMENT half (why a vector is N/A or redundant, code anchors) is
 * authored in annotations/golden_hex/cddl_codegen.toml and JOINED in. The join doubles as a drift check:
 * a note that disagrees with reality (or points at nothing) is reported, not silently rendered.
 *
 * Inputs (none hand-transcribed):
 *   sources/appendix_a.json                  - the 82 RFC 8949 Appendix A vectors (hex + decoded)
 *   matrix.json                              - the legal CBOR major-type x ai-form grid (denominator)
 *   ../tests/golden_hex/tests.rs             - the bytes the golden test actually asserts (numerator)
 *   annotations/golden_hex/cddl_codegen.toml - the non-derivable human rationale (joined in)
 *
 * Run:  bun run project_golden_hex.ts        -> writes ../tests/golden_hex/COVERAGE.md
 * Exit non-zero if any drift (stale/contradicted note) or any UNEXPLAINED uncovered vector is found.
 *
 * ponytail: "covered" = the vector's hex equals a complete CBOR item span asserted by some test (the
 * CBOR walk avoids payload-byte false positives). Decoded values are rendered from their RAW JSON source
 * token (via JSON.parse source access) so floats/-0.0/>2^53 ints stay exact — no re-serialization drift.
 */
import notesToml from "./annotations/golden_hex/cddl_codegen.toml";

const HERE = import.meta.dir;
const GOLDEN = `${HERE}/../tests/golden_hex`;
const OUT = `${GOLDEN}/COVERAGE.md`;

interface Note { hex?: string; cell?: string; status: "redundant" | "out_of_scope"; reason: string; code_anchor?: string }
interface RawVector { hex: string; decoded?: unknown; diagnostic?: string }
interface Vector extends RawVector { hexn: string; cell: string; tests: string[]; covered: boolean; note: Note | null }

// Marker for a JSON number whose raw source token we preserve, so big ints (>2^53) and floats render
// exactly as the RFC's appendix_a.json wrote them rather than via a lossy Number round-trip.
class RawNum { constructor(public raw: string) {} }
const renderValue = (v: unknown): string => {
  if (v instanceof RawNum) return v.raw;
  if (v === null) return "null";
  if (typeof v === "boolean") return String(v);
  if (typeof v === "string") return JSON.stringify(v);
  if (Array.isArray(v)) return `[${v.map(renderValue).join(", ")}]`;
  return `{${Object.entries(v as object).map(([k, val]) => `${JSON.stringify(k)}: ${renderValue(val)}`).join(", ")}}`;
};

const headClass = (b: number): [number, string] => {
  const mt = b >> 5, ai = b & 0x1f;
  if (ai < 24) return [mt, mt === 7 ? "simple_imm" : "imm"];
  if (mt === 7) return [mt, { 24: "ai24", 25: "float16", 26: "float32", 27: "float64", 31: "break" }[ai]!];
  return [mt, { 24: "ai24", 25: "ai25", 26: "ai26", 27: "ai27", 31: "indef" }[ai]!];
};
const cellId = (b: number) => { const [mt, form] = headClass(b); return `enc.major${mt}.${form}`; };

// --- legal grid cells (LEAF cells only — exclude parent rows incl. the enc.major7.float summary) ---
const LEAF_FORMS = new Set(["imm", "ai24", "ai25", "ai26", "ai27", "indef",
  "simple_imm", "float16", "float32", "float64", "break"]);
const grid = (await Bun.file(`${HERE}/matrix.json`).json() as { encodings: { id: string }[] }).encodings;
const cells = new Set(grid.map(e => e.id).filter(id => LEAF_FORMS.has(id.split(".").at(-1)!)));

// --- asserted test bytes (file order preserved) ---
const src = await Bun.file(`${GOLDEN}/tests.rs`).text();
const names = [...src.matchAll(/kat!\(\s*(\w+)/g)].map(m => m[1]);
const arrays = [...src.matchAll(/&\[([0-9a-fx,\s]*?)\]/g)].map(m => m[1]);
const tests = new Map<string, Uint8Array>(
  names.map((n, i) => [n, Uint8Array.from([...arrays[i].matchAll(/0x([0-9a-fA-F]{2})/g)].map(m => parseInt(m[1], 16)))]),
);
const toHex = (b: Uint8Array, s: number, e: number) =>
  [...b.slice(s, e)].map(x => x.toString(16).padStart(2, "0")).join("");

// Yield (start,end) of EVERY encoded CBOR item, nested included — match whole items, never payload bytes.
function cborItems(b: Uint8Array): [number, number][] {
  const spans: [number, number][] = [];
  const beInt = (from: number, to: number) => { let n = 0; for (let k = from; k < to; k++) n = n * 256 + b[k]; return n; };
  function walk(i: number): number {
    const start = i, ib = b[i], ai = ib & 0x1f, mt = ib >> 5;
    i += 1;
    if (ai === 24) i += 1; else if (ai === 25) i += 2; else if (ai === 26) i += 4; else if (ai === 27) i += 8;
    const arg = ai < 24 ? ai : null;
    if (mt === 2 || mt === 3) {
      if (ai === 31) { while (b[i] !== 0xff) i = walk(i); i += 1; }
      else i += arg ?? beInt(start + 1, i);
    } else if (mt === 4 || mt === 5) {
      const mult = mt === 4 ? 1 : 2;
      if (ai === 31) { while (b[i] !== 0xff) i = walk(i); i += 1; }
      else { const n = arg ?? beInt(start + 1, i); for (let k = 0; k < n * mult; k++) i = walk(i); }
    } else if (mt === 6) {
      i = walk(i);
    }
    spans.push([start, i]);
    return i;
  }
  walk(0);
  return spans;
}

// every distinct item encoding each test asserts, and which test(s) assert it
const testItems = new Map<string, Set<string>>(
  [...tests].map(([n, b]) => [n, new Set(cborItems(b).map(([s, e]) => toHex(b, s, e)))]),
);
const coveringTests = (hexn: string) => [...testItems].filter(([, items]) => items.has(hexn)).map(([n]) => n);
// a cell is covered if any test asserts an item whose head lands in it (item heads are always real heads)
const cellsCovered = new Set(
  [...testItems.values()].flatMap(items => [...items].map(ih => cellId(parseInt(ih.slice(0, 2), 16)))),
);

// --- the join: human notes keyed by hex or cell ---
const notes = notesToml.note as Note[];
const noteByHex = new Map(notes.filter(n => n.hex).map(n => [n.hex!.toLowerCase(), n]));
const noteByCell = new Map(notes.filter(n => n.cell).map(n => [n.cell!, n]));

// --- spec vectors: parse with raw number tokens, then derive coverage in one pass ---
const reviver = (_k: string, v: unknown, ctx?: { source: string }) =>
  typeof v === "number" ? new RawNum(ctx!.source) : v;
const rawVectors = JSON.parse(
  await Bun.file(`${HERE}/sources/appendix_a.json`).text(),
  reviver as (k: string, v: unknown) => unknown,
) as RawVector[];
const vectors: Vector[] = rawVectors.map(rv => {
  const hexn = rv.hex.toLowerCase();
  const cell = cellId(parseInt(hexn.slice(0, 2), 16));
  const tests = coveringTests(hexn);
  return { ...rv, hexn, cell, tests, covered: tests.length > 0, note: noteByHex.get(hexn) ?? noteByCell.get(cell) ?? null };
});

// --- drift checks (the value-add over a hand doc) ---
const drift: string[] = [];
const vectorHexes = new Set(vectors.map(v => v.hexn));
for (const [h] of noteByHex) if (!vectorHexes.has(h)) drift.push(`stale note: hex \`${h}\` is not an Appendix A vector`);
for (const [c, n] of noteByCell) {
  if (!cells.has(c)) drift.push(`stale note: cell \`${c}\` is not a legal grid cell`);
  else if (n.status === "out_of_scope" && cellsCovered.has(c))
    drift.push(`contradiction: \`${c}\` is annotated out_of_scope but a test exercises it`);
}
for (const v of vectors) {
  if (v.note?.status === "redundant" && !cellsCovered.has(v.cell))
    drift.push(`contradiction: \`${v.hexn}\` is annotated redundant but its cell \`${v.cell}\` is not covered by any test`);
  if (v.note?.status === "out_of_scope" && v.covered)
    drift.push(`contradiction: \`${v.hexn}\` is annotated out_of_scope but ${v.tests.join(", ")} exercises it`);
}
const unexplained = vectors.filter(v => !v.covered && !v.note);

const mark = (v: Vector) =>
  v.covered ? "✅" : !v.note ? "➕" : v.note.status === "redundant" ? "✅*" : "➖";

function detail(v: Vector): string {
  if (v.covered) return `\`${v.tests[0]}\`` + (v.tests.length > 1 ? ` (+${v.tests.length - 1})` : "");
  if (!v.note) return "**coverable but untested — add a vector or a note**";
  return v.note.reason + (v.note.code_anchor ? `  [\`${v.note.code_anchor}\`]` : "");
}

function decoded(v: Vector): string {
  const d = "decoded" in v ? v.decoded : "diagnostic" in v ? v.diagnostic : "?";
  const s = typeof d === "string" ? d : renderValue(d);
  if (s === "") return '`""`';
  return s.length > 40 ? s.slice(0, 39).replace(/\s+$/, "") + "…" : s;
}

// --- render the drop-in COVERAGE.md ---
const L: string[] = [];
const w = (s = "") => L.push(s);
w("# Golden known-answer vectors — coverage map");
w();
w("> **GENERATED** by `cddl-matrix/project_golden_hex.ts` — do not hand-edit. The mechanical half");
w("> (covered / which test) is derived from the asserted bytes in `tests.rs`; the rationale half is");
w("> authored in `cddl-matrix/annotations/golden_hex/cddl_codegen.toml` and joined in. Regenerate after");
w("> changing either. CI fails on drift (a note that contradicts reality) or any ➕ row.");
w();
w("Tracks which RFC 8949 Appendix A vectors the golden-vector test exercises, vs the full CBOR");
w("encoding grid. Each ✅ asserts **both** directions (`to_cbor_bytes()` == spec bytes, and spec bytes");
w("round-trip back). They catch a *symmetric* encode+decode bug that round-trip tests structurally can't.");
w();
w("Reference: RFC 8949 Appendix A — <https://www.rfc-editor.org/rfc/rfc8949#appendix-A>. For offline");
w("use: `curl -O https://www.rfc-editor.org/rfc/rfc8949.txt` (or the pinned `cddl-matrix/sources/appendix_a.json`).");
w();
w("**Framing nuance:** cddl-codegen only emits serialization for *named composite types*, so each");
w("primitive vector is tested as the single element of a one-element array record (`foo = [v: <prim>]`)");
w("— the asserted bytes are `0x81` + the primitive encoding. Composite vectors (`Triple`→`0x83…`,");
w("maps→`0xa…`, `TaggedPair`→`0xd82a…`) assert the RFC bytes exactly. Coverage is matched on the");
w("primitive encoding regardless of the `0x81` framing.");
w();
w("## Legend");
w();
w("| mark | meaning |");
w("|------|---------|");
w("| ✅ | covered — the exact RFC encoding is asserted by the named test |");
w("| ✅* | path covered — a sibling vector on the same encoding path is tested; this exact vector is redundant |");
w("| ➕ | **coverable but untested** — maps to a real generator path, no test and no rationale (actionable gap) |");
w("| ➖ | N/A — generator never emits this under default flags (see reason) |");
w();
const cov = vectors.filter(v => v.covered).length;
const red = vectors.filter(v => !v.covered && v.note?.status === "redundant").length;
const oos = vectors.filter(v => !v.covered && v.note?.status === "out_of_scope").length;
const oosCells = new Set(notes.filter(n => n.cell && n.status === "out_of_scope").map(n => n.cell!));
const uncovCells = [...cells].filter(c => !cellsCovered.has(c)).sort();
const never = uncovCells.filter(c => oosCells.has(c));
const emittable = uncovCells.filter(c => !oosCells.has(c));
w("## Summary");
w();
w(`- Appendix A vectors: **${vectors.length}** — ✅ ${cov} covered · ✅* ${red} redundant · ➖ ${oos} N/A · ➕ ${unexplained.length} unexplained`);
w(`- Legal **leaf** cells: **${cells.size}** — ${cellsCovered.size} covered, ${uncovCells.length} unexercised:`);
w(`  - ${never.length} **never emitted** under default flags (indefinite-length, float16/32, extended-simple, break)`);
w(`  - ${emittable.length} **emittable but no Appendix A vector lands here** (e.g. wide-argument length/count heads) — not a generator gap, just outside the App-A example set`);
w(`- Golden tests: ${tests.size}`);
w();
w("**Next frontier:** the only never-emitted family worth a dedicated golden set is **indefinite-length**");
w("(the `.indef` cells) — exercised under `--preserve-encodings`, which can round-trip indefinite inputs.");
w("Everything else uncovered is either redundant or has no canonical RFC vector.");
w();
const LABELS = ["unsigned integer", "negative integer", "byte string", "text string", "array", "map", "tag", "simple / float"];
for (let mt = 0; mt < 8; mt++) {
  const rows = vectors.filter(v => parseInt(v.hexn.slice(0, 2), 16) >> 5 === mt);
  if (!rows.length) continue;
  w(`### Major type ${mt} — ${LABELS[mt]}`);
  w();
  w("| RFC bytes | decoded | | test / note |");
  w("|-----------|---------|---|-------------|");
  for (const v of [...rows].sort((a, b) => (a.hexn < b.hexn ? -1 : a.hexn > b.hexn ? 1 : 0)))
    w(`| \`${v.hexn}\` | ${decoded(v)} | ${mark(v)} | ${detail(v)} |`);
  w();
}
w("## Consistency (join drift check)");
w();
if (drift.length) for (const d of drift) w(`- ❌ ${d}`);
else if (unexplained.length) w(`- ⚠️ ${unexplained.length} uncovered vector(s) have no note (rendered ➕ above) — explain or cover them.`);
else w("- ✅ All notes resolve to a real vector/cell and agree with the derived coverage. No drift.");
w();

await Bun.write(OUT, L.join("\n"));
console.log(`wrote ${OUT.replace(`${HERE}/../`, "")}`);
console.log(`  ${cov} covered, ${red} redundant, ${oos} N/A, ${unexplained.length} unexplained; ${drift.length} drift issue(s)`);
for (const d of drift) console.log("  DRIFT:", d);
process.exit(drift.length || unexplained.length ? 1 : 0);
