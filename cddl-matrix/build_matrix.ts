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
import { ROOT, loadMatrixInputs, loadTomlArray, globRel, stableJson } from "./lib";

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
