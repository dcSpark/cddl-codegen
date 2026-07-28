#!/usr/bin/env bun
/**
 * Prototype generator + drift-check for the CDDL master matrix.
 *
 * Joins the authored overlay (features/roles/containment/encodings TOML + the IANA control-op CSV)
 * with per-tool annotations into matrix.json, and verifies the invariant that every annotation id
 * resolves to a real master id. `--check` regenerates in-memory and fails if matrix.json is stale.
 *
 * Run from cddl-matrix/:  bun run build_matrix.ts   (or `--check` for the snapshot gate)
 */
import { readFileSync } from "node:fs";
import { ROOT, PRELUDE_DEFS, loadMatrixInputs, loadTomlArray, globRel, splitTopAlts, stableJson } from "./lib";

const { features, roles, contain, encodings, controlOps } = loadMatrixInputs();

// per-tool annotations: glob annotations/*.toml -> {basename: support[]} (non-recursive: the
// golden_hex/ projection notes live in a subdir and are deliberately not part of the master).
const annos: Record<string, { id: string }[]> = {};
for (const p of globRel("annotations/*.toml")) {
  const name = p.split("/").at(-1)!.replace(/\.toml$/, "");
  annos[name] = loadTomlArray(p, "support");
}

// F1: control ops already carry their introducing RFC; mirror it into `profile` (build-only) so the
// profile axis is uniform across features and control operators in matrix.json.
const control_operators = controlOps.map(c => ({ ...c, profile: c.rfc }));

const matrix = {
  features, roles, containment: contain, encodings, control_operators, annotations: annos,
};

// --- invariants: master ids are unique, and every annotation id resolves to a real master id.
// Duplicates would pass the Set-based resolution check while every downstream Map join silently
// collapses them last-wins (and verify.ts would probe + write both rows), so they are hard errors.
const allMasterIds = [...features, ...roles, ...contain, ...encodings, ...control_operators].map(x => x.id);
const masterIds = new Set<string>(allMasterIds);
const errors: string[] = [];
if (masterIds.size !== allMasterIds.length) {
  const seen = new Set<string>();
  for (const id of allMasterIds) {
    if (seen.has(id)) errors.push(`duplicate master id '${id}'`);
    seen.add(id);
  }
}
for (const [tool, rows] of Object.entries(annos)) {
  const seen = new Set<string>();
  for (const r of rows) {
    if (seen.has(r.id)) errors.push(`annotations/${tool}: duplicate id '${r.id}'`);
    seen.add(r.id);
    if (!masterIds.has(r.id)) errors.push(`annotations/${tool}: '${r.id}' resolves to no master id`);
  }
}

// --- the encoding parent -> leaf relation (encodings.toml `cells`). Master data, so it is checked
// here rather than in the golden_hex projection that consumes it. PARENT/LEAF is decided
// STRUCTURALLY — a row is a PARENT iff it declares `cells` — so there is no form vocabulary to keep
// in sync with the file (the hand-maintained one this replaced could drift silently).
// `enc.major7.float`'s cells are a SUBSET of `enc.major7`'s: an intended overlap, so nothing here
// requires a leaf to have exactly one parent.
const encById = new Map(encodings.map(e => [e.id, e]));
const leafIds = encodings.filter(e => !e.cells).map(e => e.id);
const claimedLeaves = new Set<string>();
for (const parent of encodings.filter(e => e.cells)) {
  for (const cid of parent.cells!) {
    const child = encById.get(cid);
    if (!child) { errors.push(`encodings: '${parent.id}'.cells names '${cid}', which resolves to no encoding row`); continue; }
    claimedLeaves.add(cid);
    // one level only: a parent-of-parent chain would make "the leaf cells beneath X" ambiguous for
    // every consumer that expands a feature's `encodings` refs.
    if (child.cells) errors.push(`encodings: '${parent.id}'.cells names '${cid}', which is itself a PARENT (declares cells) — the relation is one level deep`);
    if (child.major_type !== parent.major_type)
      errors.push(`encodings: '${parent.id}' (major_type ${parent.major_type}) lists cell '${cid}' of major_type ${child.major_type} — cross-major cell`);
  }
}
// An orphan leaf is invisible to every per-construct expansion (no feature ref can reach it through a
// parent), so it would silently drop out of the per-construct legality answer.
for (const lid of leafIds)
  if (!claimedLeaves.has(lid)) errors.push(`encodings: leaf '${lid}' appears in no parent's cells — it is outside the parent->leaf relation`);

// Feature -> encoding link integrity. verify.ts checks this too, but verify.ts is a FULL-tier gate and
// CI runs `fast` only; the ref set is master data, so the master's own fast-tier drift gate owns it.
const encIds = new Set(encodings.map(e => e.id));
for (const f of features)
  for (const eid of f.encodings ?? [])
    if (!encIds.has(eid)) errors.push(`features: '${f.id}'.encodings names '${eid}', which resolves to no encoding row`);

// --- a prelude construct whose CBOR head is FIXED by its own definition must declare exactly the cell
// that head lands in. Derived from the pinned prelude, never hand-trusted: `bigfloat = #6.5(...)` is
// tag 5 at EVERY value, so declaring the `enc.major6` parent claimed five cells of which four are
// unreachable — and the per-construct projection then reported those four as untested gaps. The two
// head kinds follow DIFFERENT rules and are deliberately not unified:
//   `#6.N(...)`  N is the tag NUMBER; the ai follows its magnitude (the head-argument width rule).
//   `#7.N`       N IS the ai itself (20=false … 25=float16), so it maps directly, with no width rule.
// Merging these into one "N -> cell" helper reads like obvious cleanup and would silently give EVERY
// simple/float construct a wrong declared cell: put `#7.20` (false) through the width rule and it
// lands in `.imm` instead of `.simple_imm`, `#7.25` (float16) in `.imm` instead of `.float16`. That is
// exactly the defect class this check exists to catch, so the near-duplication is load-bearing.
// Parametric heads (`type2.tag`'s user-chosen `#6.N`, `type2.major7`'s `#7.N`) are not prelude rules
// and never reach here — they keep the parent ref, which is the correct claim for them.
const tagNumberCell = (n: number) =>
  `enc.major6.${n <= 23 ? "imm" : n <= 255 ? "ai24" : n <= 65535 ? "ai25" : n <= 0xffffffff ? "ai26" : "ai27"}`;
const AI_CELL: Record<number, string> = { 24: "ai24", 25: "float16", 26: "float32", 27: "float64", 31: "break" };
// The cells a prelude RHS pins, or null when nothing about it is fixed — the check stays silent rather
// than guessing. A top-level choice resolves only if EVERY arm is itself pinned: `bigint = biguint /
// bignint` pins {enc.major6.imm}, while `number = int / float` pins nothing.
function pinnedHeadCells(rhs: string, depth = 0): string[] | null {
  if (depth > 3) return null;
  const arms = splitTopAlts(rhs);
  if (arms.length > 1) {
    const all = arms.map(a => pinnedHeadCells(a.trim(), depth + 1));
    return all.every(x => x) ? [...new Set((all as string[][]).flat())].sort() : null;
  }
  const t = rhs.trim();
  const tag = t.match(/^#6\.(\d+)\s*\(/);
  if (tag) return [tagNumberCell(parseInt(tag[1], 10))];
  const simple = t.match(/^#7\.(\d+)\s*$/);
  if (simple) {
    const ai = parseInt(simple[1], 10);
    const form = ai <= 23 ? "simple_imm" : AI_CELL[ai];
    return form ? [`enc.major7.${form}`] : null;
  }
  const named = PRELUDE_DEFS.get(t);
  return named !== undefined ? pinnedHeadCells(named, depth + 1) : null;
}
let pinnedChecked = 0;
for (const f of features) {
  if (!f.id?.startsWith("prelude.") || !f.encodings?.length) continue;
  const rhs = PRELUDE_DEFS.get(f.id.slice("prelude.".length));
  const want = rhs === undefined ? null : pinnedHeadCells(rhs);
  if (!want) continue;
  pinnedChecked++;
  const got = [...new Set(f.encodings.flatMap(ref => encById.get(ref)?.cells ?? [ref]))].sort();
  if (got.join(",") !== want.join(","))
    errors.push(
      `features: '${f.id}' has a head fixed by the pinned prelude (\`${rhs}\`), which can only encode as ` +
      `[${want.join(", ")}], but its encodings expand to [${got.join(", ")}] — declare the exact cell(s)`);
}
// Vacuity floor: the check is silent by design for anything unpinned, so a prelude-parse or naming
// change that pinned NOTHING would leave it green while checking zero rows.
if (!pinnedChecked)
  errors.push("features: no prelude row resolved to a pinned CBOR head — the prelude-derived cell check is checking nothing");

const out = stableJson(matrix);
const nAnno = Object.values(annos).reduce((a, v) => a + v.length, 0);
const summary =
  `${features.length} features, ${roles.length} roles, ${contain.length} containment, ` +
  `${encodings.length} encodings, ${control_operators.length} control-ops (from IANA CSV), ${nAnno} annotations`;

// annotation-id invariant (always): every annotation id resolves to a real master id
if (errors.length) {
  console.log("DRIFT CHECK FAILED:");
  for (const e of errors) console.log("  -", e);
  process.exit(1);
}

// F6 — snapshot the synthesis. matrix.json is the committed golden view of the editorial join;
// `--check` regenerates it in-memory and fails if the on-disk golden is stale.
if (process.argv.includes("--check")) {
  let current: string | null = null;
  try { current = readFileSync(`${ROOT}/matrix.json`, "utf8"); } catch { /* missing */ }
  if (current !== out) {
    console.log(`SNAPSHOT DRIFT: matrix.json is stale vs the authored overlay (${summary}).`);
    console.log("Run `bun run build_matrix.ts` and review the diff before committing.");
    process.exit(1);
  }
  console.log(`snapshot OK: matrix.json matches the authored overlay (${summary}); annotation ids resolve`);
} else {
  await Bun.write(`${ROOT}/matrix.json`, out);
  console.log(`matrix.json written: ${summary}`);
  console.log("drift check OK: all annotation ids resolve to master ids");
}
