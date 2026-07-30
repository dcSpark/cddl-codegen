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
 *   ../tests/golden_hex_{preserve,canonical}/tests.rs - the sibling flag-mode golden sets: NOT
 *       joined into the Appendix A grid (it documents the default-flags set), but validated to the
 *       same authoring contract (0x?? literals, well-formed CBOR) and counted into the doc.
 *
 * Run:  bun run project_golden_hex.ts        -> writes ../tests/golden_hex/COVERAGE.md
 * Exit non-zero if any drift (stale/contradicted note) or any UNEXPLAINED uncovered vector is found.
 *
 * "covered" = the vector's hex equals a complete CBOR item span asserted by some test (the
 * CBOR walk avoids payload-byte false positives). Decoded values are rendered from their RAW JSON source
 * token (via JSON.parse source access) so floats/-0.0/>2^53 ints stay exact — no re-serialization drift.
 */
import notesToml from "./annotations/golden_hex/cddl_codegen.toml";
import { featuresIn, NO_DETECTOR } from "./corpus_detect";
import { preludeAliasClasses } from "./lib";

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
  const form = mt === 7
    ? { 24: "ai24", 25: "float16", 26: "float32", 27: "float64", 31: "break" }[ai]
    : { 24: "ai24", 25: "ai25", 26: "ai26", 27: "ai27", 31: "indef" }[ai];
  if (!form) throw new Error(`reserved additional-info ${ai} in head byte 0x${b.toString(16).padStart(2, "0")} — not legal CBOR`);
  return [mt, form];
};
const cellId = (b: number) => { const [mt, form] = headClass(b); return `enc.major${mt}.${form}`; };

// --- legal grid cells (LEAF cells only — exclude parent rows incl. the enc.major7.float summary) ---
// PARENT vs LEAF is read STRUCTURALLY off the master: a row is a PARENT iff it declares `cells`, the
// leaf ids beneath it (encodings.toml's header says why that relation is authored data rather than an
// id-prefix rule). Deriving it here rather than from a hand-maintained form vocabulary means a new ai
// form cannot be silently dropped from the denominator by a vocabulary nobody remembered to extend.
// `headClass`/`cellId` above keep their own form names on purpose: that is the byte -> cell-id map, a
// different concern from the legality relation, and the two are deliberately not coupled.
const matrix = await Bun.file(`${HERE}/matrix.json`).json() as {
  encodings: { id: string; cells?: string[] }[];
  features: { id: string; encodings?: string[] }[];
};
const cells = new Set(matrix.encodings.filter(e => !e.cells).map(e => e.id));
const cellsOf = new Map(matrix.encodings.filter(e => e.cells).map(e => [e.id, e.cells!]));

// --- asserted test bytes (file order preserved) ---
// Name and bytes are captured from the SAME kat! invocation (never paired positionally across two
// independent scans — an `&[…]` in a $value expression or a comment must not steal a pairing slot),
// comments are stripped from the invocation body, and the byte text must tokenize COMPLETELY into
// two-digit `0x??` literals. This projection certifies "vector X is asserted by test Y", so any
// deviation is a hard error, not a silently-wrong doc.
const src = await Bun.file(`${GOLDEN}/tests.rs`).text();
const katCalls = [...src.matchAll(/kat!\(\s*(\w+)\s*,([\s\S]*?)\)\s*;/g)];
if (!katCalls.length) throw new Error("no kat!(…) invocations found in tests.rs — the extraction contract broke");
const tests = new Map<string, Uint8Array>();
for (const [, name, rawBody] of katCalls) {
  if (tests.has(name)) throw new Error(`duplicate kat! test name \`${name}\` — the second would silently shadow the first`);
  const body = rawBody.replace(/\/\/[^\n]*|\/\*[\s\S]*?\*\//g, ""); // comments are not arguments
  const arrays = [...body.matchAll(/&\[([^\]]*)\]/g)];
  if (!arrays.length) throw new Error(`kat!(${name}): no \`&[…]\` byte-array argument found`);
  const text = arrays[arrays.length - 1][1]; // the expected bytes are the macro's LAST argument
  const residue = text.replace(/0x[0-9a-fA-F]{2}/g, "").replace(/[,\s]/g, "");
  if (residue)
    throw new Error(`kat!(${name}): byte array contains non-\`0xNN\` content (\`${residue}\`) — write two-digit 0x literals only (no decimal, no single-digit)`);
  tests.set(name, Uint8Array.from([...text.matchAll(/0x([0-9a-fA-F]{2})/g)].map(m => parseInt(m[1], 16))));
}
const toHex = (b: Uint8Array, s: number, e: number) =>
  [...b.slice(s, e)].map(x => x.toString(16).padStart(2, "0")).join("");

// Yield (start,end) of EVERY encoded CBOR item, nested included — match whole items, never payload bytes.
// Bounds-checked: extraction garbage must surface as a named error, never as an infinite loop on an
// unterminated indefinite head or as phantom out-of-bounds spans that fabricate cell credit.
function cborItems(b: Uint8Array, label: string): [number, number][] {
  const spans: [number, number][] = [];
  const die = (msg: string): never => { throw new Error(`${label}: ${msg} — the asserted bytes are not one well-formed CBOR item`); };
  const need = (i: number) => { if (i > b.length) die(`truncated item (needs byte ${i}, have ${b.length})`); };
  const beInt = (from: number, to: number) => { let n = 0; for (let k = from; k < to; k++) n = n * 256 + b[k]; return n; };
  function walk(i: number): number {
    if (i >= b.length) die(`walked past the end at offset ${i}`);
    const start = i, ib = b[i], ai = ib & 0x1f, mt = ib >> 5;
    if (ai >= 28 && ai <= 30) die(`reserved additional-info ${ai} at offset ${i}`);
    // ai=31 (indefinite / break) is well-formed only on the string/array/map majors (2-5). On
    // uint/nint (0/1) and tag (6) it is not legal CBOR; on major 7 it is the break byte, valid ONLY
    // inside an indefinite container (the container loops below consume it), never as a standalone
    // item. Rejecting it here keeps a malformed head from minting a bogus `enc.majorN.indef` cell.
    if (ai === 31 && (mt <= 1 || mt === 6 || mt === 7))
      die(`additional-info 31 (indefinite/break) is not well-formed on major type ${mt} at offset ${i}`);
    i += 1;
    if (ai === 24) i += 1; else if (ai === 25) i += 2; else if (ai === 26) i += 4; else if (ai === 27) i += 8;
    need(i);
    const arg = ai < 24 ? ai : null;
    if (mt === 2 || mt === 3) {
      if (ai === 31) { while (b[i] !== 0xff) { if (i >= b.length) die("unterminated indefinite string"); i = walk(i); } i += 1; }
      else { i += arg ?? beInt(start + 1, i); need(i); }
    } else if (mt === 4 || mt === 5) {
      const mult = mt === 4 ? 1 : 2;
      if (ai === 31) { while (b[i] !== 0xff) { if (i >= b.length) die("unterminated indefinite container"); i = walk(i); } i += 1; }
      else { const n = arg ?? beInt(start + 1, i); for (let k = 0; k < n * mult; k++) i = walk(i); }
    } else if (mt === 6) {
      i = walk(i);
    }
    spans.push([start, i]);
    return i;
  }
  const end = walk(0);
  if (end !== b.length) die(`trailing bytes after the top-level item (consumed ${end} of ${b.length})`);
  return spans;
}

// --- sibling spec-anchored sets: the preserve / canonical golden KATs ---
// Different flags -> different generated crates -> their own fixture dirs (tests/golden_hex_preserve,
// tests/golden_hex_canonical). They are deliberately NOT joined into the Appendix A grid (the grid
// documents the DEFAULT-flags set — the siblings exist precisely for encodings that set never
// emits, so joining them would contradict every `.indef` out_of_scope note), but their byte arrays
// are held to the same authoring contract: every `&[…]` inside a kat_* invocation must tokenize
// completely into two-digit 0x?? literals and parse as exactly one well-formed CBOR item. The
// counts render into the doc, so adding a sibling vector without regenerating trips --check.
async function validateSiblingKats(dir: string, macroName: string, arity: number): Promise<number> {
  const file = Bun.file(`${HERE}/../tests/${dir}/tests.rs`);
  if (!(await file.exists()))
    throw new Error(`tests/${dir}/tests.rs not found — the sibling golden set moved; update project_golden_hex.ts`);
  const text = await file.text();
  const calls = [...text.matchAll(new RegExp(`${macroName}!\\(\\s*(\\w+)\\s*,([\\s\\S]*?)\\)\\s*;`, "g"))];
  if (!calls.length) throw new Error(`tests/${dir}/tests.rs: no ${macroName}!(…) invocations found — the extraction contract broke`);
  const seen = new Set<string>();
  for (const [, name, rawBody] of calls) {
    const label = `${dir} ${macroName}!(${name})`;
    if (seen.has(name)) throw new Error(`${label}: duplicate test name — the second would silently shadow the first`);
    seen.add(name);
    const body = rawBody.replace(/\/\/[^\n]*|\/\*[\s\S]*?\*\//g, ""); // comments are not arguments
    const arrays = [...body.matchAll(/&\[([^\]]*)\]/g)];
    if (arrays.length !== arity)
      throw new Error(`${label}: expected ${arity} byte-array argument(s), found ${arrays.length}`);
    for (const [, arrText] of arrays) {
      const residue = arrText.replace(/0x[0-9a-fA-F]{2}/g, "").replace(/[,\s]/g, "");
      if (residue)
        throw new Error(`${label}: byte array contains non-\`0xNN\` content (\`${residue}\`) — write two-digit 0x literals only (no decimal, no single-digit)`);
      cborItems(Uint8Array.from([...arrText.matchAll(/0x([0-9a-fA-F]{2})/g)].map(m => parseInt(m[1], 16))), label);
    }
  }
  return calls.length;
}
const preserveKats = await validateSiblingKats("golden_hex_preserve", "kat_preserve", 1);
const canonicalKats = await validateSiblingKats("golden_hex_canonical", "kat_canonical", 2);

// every distinct item encoding each test asserts, and which test(s) assert it
const testItems = new Map<string, Set<string>>(
  [...tests].map(([n, b]) => [n, new Set(cborItems(b, `kat!(${n})`).map(([s, e]) => toHex(b, s, e)))]),
);
const coveringTests = (hexn: string) => [...testItems].filter(([, items]) => items.has(hexn)).map(([n]) => n);
// a cell is covered if any test asserts an item whose head lands in it (item heads are always real heads)
const cellsCovered = new Set(
  [...testItems.values()].flatMap(items => [...items].map(ih => cellId(parseInt(ih.slice(0, 2), 16)))),
);

// --- the join: human notes keyed by hex or cell ---
// Schema-validated on load: the design pitch is that the hand-authored judgment half gets
// mechanically cross-checked, so a malformed row (typo'd status, both/neither key, duplicate key)
// must be a hard error — every downstream comparison is an exact string match that would otherwise
// silently render the vector as ➖ and desync the summary counts.
const notes = notesToml.note as Note[];
const seenNoteKeys = new Set<string>();
for (const n of notes) {
  if (!!n.hex === !!n.cell) throw new Error(`overlay note must key EXACTLY one of hex/cell: ${JSON.stringify(n)}`);
  if (n.status !== "redundant" && n.status !== "out_of_scope")
    throw new Error(`overlay note \`${n.hex ?? n.cell}\`: bad status \`${n.status}\` (redundant | out_of_scope)`);
  const key = n.hex ? `hex:${n.hex.toLowerCase()}` : `cell:${n.cell}`;
  if (seenNoteKeys.has(key)) throw new Error(`duplicate overlay note for ${key} — the join would keep only the last`);
  seenNoteKeys.add(key);
}
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
  else if (n.status === "redundant" && !cellsCovered.has(c))
    drift.push(`contradiction: cell \`${c}\` is annotated redundant but no test covers it (the documented rule: the cell MUST be covered)`);
}
for (const v of vectors) {
  if (v.note?.status === "redundant" && !cellsCovered.has(v.cell))
    drift.push(`contradiction: \`${v.hexn}\` is annotated redundant but its cell \`${v.cell}\` is not covered by any test`);
  if (v.note?.status === "out_of_scope" && v.covered)
    drift.push(`contradiction: \`${v.hexn}\` is annotated out_of_scope but ${v.tests.join(", ")} exercises it`);
  // a HEX-keyed redundant note on a vector that is itself directly asserted is a dead row (a
  // CELL-keyed redundant note legitimately coexists with covered siblings, so only hex-keyed here)
  if (noteByHex.get(v.hexn)?.status === "redundant" && v.covered)
    drift.push(`stale note: \`${v.hexn}\` is annotated redundant (intentionally untested) but ${v.tests.join(", ")} covers it — delete the note`);
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
w("Reference: RFC 8949 Appendix A — <https://www.rfc-editor.org/rfc/rfc8949#appendix-A>. Both are pinned");
w("offline under `cddl-matrix/sources/`: the full text as `rfc8949.txt`, the vectors as `appendix_a.json`.");
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
w(`- Golden tests: ${tests.size} default-flags · sibling sets: ${preserveKats} preserve + ${canonicalKats} canonical (below)`);
w();
w("**Sibling golden sets (not in this grid):** the encodings the default-flags set can never");
w("exercise — the ➖ `.indef` cells, the ➖ `.float16`/`.float32` heads, and non-minimal header arguments — have their own spec-anchored");
w("KATs: `tests/golden_hex_preserve/tests.rs` (irregular RFC 8949 §3 encodings must re-encode");
w("byte-identically under `--preserve-encodings`) and `tests/golden_hex_canonical/tests.rs` (the");
w("same irregular inputs must re-encode to hand-derived §4.2 minimal bytes under `--canonical-form`).");
w("This projection validates their byte-literal/well-formedness contract and counts them, but the");
w("Appendix A join above stays default-flags-only by design. Everything else uncovered is either");
w("redundant or has no canonical RFC vector.");
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
// --- Q3's per-CONSTRUCT answer: "for construct C, these legal encodings are untested" ---
// The global summary above answers the same question for the whole grid; this narrows it to one
// construct by expanding that construct's declared `encodings` refs through the master's parent->leaf
// relation (a parent ref -> its `cells`; a leaf ref -> itself) and intersecting with the SAME
// `cellsCovered` the grid uses. The uncovered remainder is split by the same cell-keyed `out_of_scope`
// overlay as the summary, so the two can never disagree about which cells are actionable.
//
// `cellsCovered` is a UNION over every kat! in the file, so on its own it would credit construct C for
// a cell some OTHER construct's vector landed in — `prelude.bigfloat` would read "2 of 5 covered" from
// a fixture that never mentions bigfloat. So the split is gated on the golden fixture's own construct
// floor first: a construct the fixture does not exercise AT ALL has nothing asserted about it, whatever
// cells its neighbours happen to cover.
const fixtureCddl = await Bun.file(`${GOLDEN}/input.cddl`).text();
const detected = featuresIn(fixtureCddl);
const fixtureFloor = new Set([...detected.rfc, ...detected.ctl, ...detected.dsl]);
// `featuresIn` matches a construct by NAME, so the scan alone misses a construct the fixture spells
// under a prelude ALIAS: `input.cddl` writes `tstr`, and `text = tstr` is the same construct under its
// other name. That equivalence is derivable from the pinned prelude, so it is CORRECTED here rather
// than disclaimed — a generated number that is wrong for a computable reason is a defect, not a
// caveat. Strictly plain aliases: a CHOICE-bodied rule (`float = float16-32 / float64`) is a union
// whose members encode differently, so crediting across one would restore the very over-credit the
// fixture floor exists to remove.
const aliasClasses = preludeAliasClasses();
for (const id of [...fixtureFloor]) {
  if (!id.startsWith("prelude.")) continue;
  for (const alias of aliasClasses.get(id.slice("prelude.".length)) ?? []) fixtureFloor.add(`prelude.${alias}`);
}
const constructs = matrix.features
  .filter(f => (f.encodings ?? []).length)
  .map(f => {
    const legal = [...new Set((f.encodings ?? []).flatMap(ref => cellsOf.get(ref) ?? [ref]))].sort();
    const exercised = fixtureFloor.has(f.id);
    const covered = exercised ? legal.filter(c => cellsCovered.has(c)) : [];
    const uncovered = legal.filter(c => !covered.includes(c));
    return {
      id: f.id,
      exercised,
      legal,
      covered,
      never: uncovered.filter(c => oosCells.has(c)),
      emittable: uncovered.filter(c => !oosCells.has(c)),
    };
  })
  .sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
// Vacuity floors on the gate itself. An empty/moved fixture would make the floor empty, flipping EVERY
// construct to "not exercised" and inflating every untested count — the failure would read as a big
// honest-looking answer. `NO_DETECTOR` is corpus_detect's declared blind set: if one of its ids ever
// gained an `encodings` list it would read as never-exercised for a reason that is about the detector,
// not the fixture, so that must be stated rather than silently rendered.
if (!fixtureFloor.size)
  throw new Error(`${GOLDEN}/input.cddl detected no constructs — the fixture floor is vacuous, which would mark every construct unexercised`);
// The alias correction is silent when it finds nothing, so a prelude bump that restructured the plain
// aliases away would drop it without moving a single visible number — the ✗ rows would simply come
// back. Fail instead, so the source change has to be looked at.
if (!aliasClasses.size)
  throw new Error("the pinned prelude yielded no plain-alias classes — the alias correction to the fixture floor would silently stop applying; re-check sources/cddl.prelude");
const blind = matrix.features.filter(f => (f.encodings ?? []).length && NO_DETECTOR.has(f.id)).map(f => f.id);
if (blind.length)
  throw new Error(`construct(s) [${blind.join(", ")}] carry encodings but are in corpus_detect's NO_DETECTOR set — their fixture-floor answer would be a detector artifact; give them a detector or exclude them explicitly`);
// Vacuity floor (the same class loadTomlArray guards at the overlay's root): if the feature rows ever
// stopped carrying `encodings` — a renamed field, a loader that drops it — this section would render
// an empty table and every gate would stay green, silently answering "no construct has an untested
// legal encoding". An empty expansion is a broken input, not an answer.
if (!constructs.length)
  throw new Error("no feature row declares a non-empty `encodings` list — the per-construct expansion is vacuous (matrix.json lost the field?)");
const constructsWithGaps = constructs.filter(c => c.emittable.length).length;
const unexercised = constructs.filter(c => !c.exercised).length;
w("## Per-construct legal encodings");
w();
w(`Q3 narrowed to one construct at a time: for each of the **${constructs.length}** feature rows that`);
w("declare an `encodings` list, the leaf cells that construct can legally take, and which of them no");
w("golden vector asserts. Each declared ref is expanded through the master's parent→leaf relation");
w("(`cddl-matrix/encodings.toml` — a PARENT row names its leaves in `cells`; a leaf ref is itself).");
w();
w("**The fixture floor comes first.** `input.cddl` is what the golden vectors are generated from, so a");
w(`construct it never mentions has nothing asserted about it at all — **${unexercised}** of the`);
w(`${constructs.length} rows are in that position, marked ✗ below, and their whole legal set is`);
w("untested. This matters because coverage is derived as a union over every `kat!` in the file: without");
w("the floor, a construct would be credited for a cell some *other* construct's vector happened to land");
w("in (a fixture with no `bigfloat` in it would still report bigfloat's tag cell as covered).");
w();
w("**For a ✓ construct the covered/uncovered split is still CELL-keyed**, not vector-keyed: a cell counts");
w("as covered when some asserted item's head lands in it, which need not be an item of *this* construct.");
w("`type2.tag` reads `enc.major6.imm` covered because the bignum vectors (`c2`/`c3`) land there. So a ✓");
w("row's *covered* is an upper bound on what is asserted about that construct specifically.");
w();
w("**What \"legal\" means here, and what it does not.** *Legal* = the leaf cells beneath the encoding");
w("rows the construct **declares**. A construct whose CBOR head is fixed by its own definition declares");
w("the single cell that head lands in — `bigfloat = #6.5(…)` is tag 5 at every value, so it declares");
w("`enc.major6.imm` alone, and the master's drift gate re-derives that from the pinned prelude rather");
w("than trusting the list. What remains is deliberately **not** a claim that cddl-codegen emits each");
w("cell under default flags — the *never emitted* column carries that, from the same cell-keyed");
w("`out_of_scope` notes as the summary — and **not** a claim that a cell is reachable at **every**");
w("value where the head argument follows the value: a `uint` reaches `enc.major0.ai27` only at values");
w("≥ 2^32, and a `bstr` reaches `enc.major2.ai26` only at lengths ≥ 2^16.");
w();
w("**Scope limit of the floor — one case, and it is narrower than it looks.** The floor is");
w("`corpus_detect.ts`'s text scan, which matches a construct by NAME, then corrected for the prelude's");
w("plain aliases (`bytes = bstr`, `text = tstr`, `null = nil`, derived from the pinned prelude): the");
w("fixture writing `tstr` credits `prelude.text` too, because they are one construct under two");
w("spellings. What is NOT corrected is a construct the fixture reaches by writing a **wider type that");
w("the generator narrows at emission**: `one_float = [v: float]` asserts `fb…` doubles, so");
w("`prelude.float64`'s cell is exercised in fact, yet no rule names `float64` and it reads ✗. That is a");
w("claim about what cddl-codegen picks when it emits a union, not about spec structure — not derivable");
w("from the prelude, and deliberately not guessed. For those rows read ✗ as *no golden rule names this");
w("construct*, which is the honest fact, not as *nothing resembling it is asserted anywhere*.");
w();
w("Reported, never fatal: this gate's non-zero exit stays reserved for note drift and ➕ (uncovered");
w("Appendix A vector with no rationale). The coverage this narrows is the same `cellsCovered` behind");
w("the summary's *emittable but no Appendix A vector lands here* line, which is already a deliberate");
w("non-failure — failing here would re-litigate that threshold from a different direction.");
w();
w("**These totals do not sum to the summary's, and making them agree would be a regression.** The");
w("summary asks whether ANY vector reaches a cell; this section asks whether one reaches it *for this");
w("construct*, so a globally covered cell is still untested here. An earlier draft had the two");
w("agreeing exactly — that was an artifact of crediting every construct for every other construct's");
w("vectors, and restoring the agreement would mean restoring the over-credit.");
w();
w(`- Exercised by \`input.cddl\`: **${constructs.length - unexercised}** of ${constructs.length} (✗ rows below have their full legal set untested)`);
w(`- Constructs with at least one untested-and-emittable cell: **${constructsWithGaps}** of ${constructs.length}`);
w("- Both counts are **conservative in one known direction**: a construct the fixture reaches only");
w("  through a wider type the generator narrows at emission (`float` → the `float64` cell) counts as");
w("  ✗ here, so *exercised* is a floor and *with gaps* is a ceiling. Cited alone, they overstate the");
w("  gap by that margin.");
w();
w("| construct | in fixture | legal | covered | never emitted | untested and emittable |");
w("|-----------|:---------:|-------|---------|---------------|------------------------|");
for (const c of constructs)
  w(`| \`${c.id}\` | ${c.exercised ? "✓" : "✗"} | ${c.legal.length} | ${c.covered.length} | ${c.never.length} | ` +
    `${c.emittable.length ? c.emittable.map(x => `\`${x}\``).join(", ") : "—"} |`);
w();
w("## Consistency (join drift check)");
w();
if (drift.length) for (const d of drift) w(`- ❌ ${d}`);
else if (unexplained.length) w(`- ⚠️ ${unexplained.length} uncovered vector(s) have no note (rendered ➕ above) — explain or cover them.`);
else w("- ✅ All notes resolve to a real vector/cell and agree with the derived coverage. No drift.");
w();

const content = L.join("\n");
const OUT_REL = OUT.replace(`${HERE}/../`, "");
console.log(`  ${cov} covered, ${red} redundant, ${oos} N/A, ${unexplained.length} unexplained; ${drift.length} drift issue(s)`);
for (const d of drift) console.log("  DRIFT:", d);
if (process.argv.includes("--check")) {
  // CI mode: never write — fail if the committed doc is stale relative to tests.rs + the overlay
  // (without this, the script would silently regenerate the doc in the runner's checkout and the
  // "CI fails on drift" claim would only cover note contradictions and ➕ rows, not staleness).
  const existing = (await Bun.file(OUT).exists()) ? await Bun.file(OUT).text() : "";
  if (existing !== content) {
    console.log(`SNAPSHOT DRIFT: ${OUT_REL} is stale vs tests.rs + the overlay — run \`bun run project_golden_hex.ts\` and commit the diff.`);
    process.exit(1);
  }
  console.log(`check OK: ${OUT_REL} is current`);
} else if (drift.length) {
  // a contradicted overlay must not rewrite the committed doc with the claims that just failed
  console.log(`SKIPPED writing ${OUT_REL} (drift — the committed doc is left untouched)`);
} else {
  await Bun.write(OUT, content);
  console.log(`wrote ${OUT_REL}`);
}
process.exit(drift.length || unexplained.length ? 1 : 0);
