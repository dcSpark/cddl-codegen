#!/usr/bin/env bun
/**
 * Q6 profile / version diff query (QUERIES.md Q6) — PURE FILE READS, no cargo, no oracles.
 *
 * Answers QUERIES.md Q6: "what changed — in the feature set or in tool X's support — when moving from
 * CDDL profile P to P+1, or from tool version V to V+1?" It has TWO modes, one per half of that question:
 *
 *   1. NO ARGS — the PROFILE view (the "P → P+1" half, from `matrix.json` alone). Each feature carries a
 *      `profile` (the RFC/vendor that introduced its CAPABILITY — F1). This mode groups the modelled
 *      features by profile and, within each, shows cddl-codegen's support split. Reading the profiles in
 *      spec order (RFC8610 → RFC9682 → CDDL_CODEGEN) is exactly "what each successive profile introduced
 *      and how much of it the tool supports."
 *
 *   2. TWO ARGS `old.json new.json` — the SNAPSHOT diff (the "V → V+1" half). Point it at two pinned
 *      `matrix.json` snapshots (e.g. `git show REF:cddl-matrix/matrix.json > /tmp/old.json`) and it emits
 *      a reviewable structural diff: added / removed ids per axis array (features, containment,
 *      control_operators, encodings, roles) and every changed annotation `status` (id: old → new). This
 *      is the F6 "pinned, diffable snapshot" made actionable.
 *
 * `--check` (NO-ARGS mode only): consistency — every feature's `profile` ∈ the known set — plus a
 * vacuity floor (>= 2 profiles present; RFC9682 introduces >= 1 feature; CDDL_CODEGEN introduces exactly
 * the 10 vendor features). It never rewrites anything.
 *
 * Run from cddl-matrix/:
 *   bun run query_q6_diff.ts                       -> the per-profile view (introduced + support split)
 *   bun run query_q6_diff.ts RFC9682               -> only the profile whose name contains "RFC9682"
 *   bun run query_q6_diff.ts old.json new.json      -> structural diff between two snapshots
 *   bun run query_q6_diff.ts --check                -> profile-set consistency + vacuity floor; exit nonzero on any
 */
import { readFileSync } from "node:fs";

const HERE = import.meta.dir;

// --- shared types ---------------------------------------------------------------------------------
interface Annotation { id: string; status: string }
interface FeatureRow { id: string; profile?: string; title?: string }
interface IdRow { id: string }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: FeatureRow[];
  containment: IdRow[];
  control_operators: IdRow[];
  encodings: IdRow[];
  roles: IdRow[];
}
function loadMatrix(path: string): MatrixJson {
  return JSON.parse(readFileSync(path, "utf8")) as MatrixJson;
}

// The known profile universe (F1). Spec order, not alphabetical — the diff reads "what P+1 added".
const KNOWN_PROFILES = ["RFC8610", "RFC9682", "CDDL_CODEGEN"];
const VENDOR_PROFILE = "CDDL_CODEGEN";
const VENDOR_FEATURE_COUNT = 12; // the comment-DSL / sentinel surface — pinned (README.md § the model); 12 since dsl.raw_bytes_flavor's registration

// ==================================================================================================
// MODE 2 — snapshot diff (two positional args).
// ==================================================================================================
const AXES: { key: keyof MatrixJson; label: string }[] = [
  { key: "features", label: "features" },
  { key: "containment", label: "containment" },
  { key: "control_operators", label: "control_operators" },
  { key: "encodings", label: "encodings" },
  { key: "roles", label: "roles" },
];

function idsOf(m: MatrixJson, key: keyof MatrixJson): string[] {
  const arr = m[key] as unknown as IdRow[];
  return Array.isArray(arr) ? arr.map(r => r.id).filter(id => typeof id === "string") : [];
}

function snapshotDiff(oldPath: string, newPath: string): void {
  const oldM = loadMatrix(oldPath);
  const newM = loadMatrix(newPath);

  console.log(`\nQ6 — snapshot diff  (old: ${oldPath}  →  new: ${newPath})\n`);

  // Per-axis id add/remove (deterministic: sorted).
  let anyAxisChange = false;
  for (const { key, label } of AXES) {
    const oldIds = new Set(idsOf(oldM, key));
    const newIds = new Set(idsOf(newM, key));
    const added = [...newIds].filter(id => !oldIds.has(id)).sort();
    const removed = [...oldIds].filter(id => !newIds.has(id)).sort();
    if (!added.length && !removed.length) continue;
    anyAxisChange = true;
    console.log(`### ${label}: +${added.length} / -${removed.length}`);
    for (const id of added) console.log(`  + ${id}`);
    for (const id of removed) console.log(`  - ${id}`);
    console.log("");
  }
  if (!anyAxisChange) console.log(`(no id added or removed on any axis array)\n`);

  // Annotation status changes (+ added / removed annotation rows).
  const oldA = new Map(oldM.annotations.cddl_codegen.map(a => [a.id, a.status]));
  const newA = new Map(newM.annotations.cddl_codegen.map(a => [a.id, a.status]));
  const changed: { id: string; from: string; to: string }[] = [];
  for (const [id, st] of newA) {
    const prev = oldA.get(id);
    if (prev !== undefined && prev !== st) changed.push({ id, from: prev, to: st });
  }
  changed.sort((a, b) => a.id.localeCompare(b.id));
  const addedAnno = [...newA.keys()].filter(id => !oldA.has(id)).sort();
  const removedAnno = [...oldA.keys()].filter(id => !newA.has(id)).sort();

  console.log(`### annotations (cddl_codegen): ${changed.length} status change(s), +${addedAnno.length} / -${removedAnno.length} row(s)`);
  for (const c of changed) console.log(`  ~ ${c.id}: ${c.from} → ${c.to}`);
  for (const id of addedAnno) console.log(`  + ${id} (${newA.get(id)})`);
  for (const id of removedAnno) console.log(`  - ${id} (${oldA.get(id)})`);
  console.log("");
}

// ==================================================================================================
// MODE 1 — per-profile view (no positional args) + --check.
// ==================================================================================================
function profileView(filter: string | undefined): void {
  const matrix = loadMatrix(`${HERE}/matrix.json`);
  const statusById = new Map(matrix.annotations.cddl_codegen.map(a => [a.id, a.status]));
  const match = (s: string): boolean => !filter || s.toLowerCase().includes(filter.toLowerCase());

  // Profiles present, spec order first then any unknown extras (sorted, surfaced not hidden).
  const present = [...new Set(matrix.features.map(f => f.profile ?? "(none)"))];
  const ordered = [
    ...KNOWN_PROFILES.filter(p => present.includes(p)),
    ...present.filter(p => !KNOWN_PROFILES.includes(p)).sort(),
  ];

  console.log(`\nQ6 — per-profile view: what each successive CDDL profile introduces + cddl-codegen's support split`);
  console.log(`(profiles in spec order; a feature's profile is the RFC/vendor that introduced its capability — F1)\n`);

  const STATUSES = ["supported", "unsupported", "uncertain", "out_of_profile"];
  for (const prof of ordered) {
    if (!match(prof)) continue;
    const feats = matrix.features.filter(f => (f.profile ?? "(none)") === prof).sort((a, b) => a.id.localeCompare(b.id));
    const split: Record<string, number> = {};
    for (const f of feats) {
      const st = statusById.get(f.id) ?? "(no annotation)";
      split[st] = (split[st] ?? 0) + 1;
    }
    const splitStr = STATUSES.filter(s => split[s]).map(s => `${s}=${split[s]}`).join(", ")
      + Object.keys(split).filter(s => !STATUSES.includes(s)).sort().map(s => `, ${s}=${split[s]}`).join("");
    console.log(`### ${prof} — introduces ${feats.length} feature(s)   [support: ${splitStr || "n/a"}]`);
    for (const f of feats) console.log(`  ${(statusById.get(f.id) ?? "?").padEnd(14)} ${f.id.padEnd(32)} ${f.title ?? ""}`);
    console.log("");
  }
}

function checkProblems(): string[] {
  const matrix = loadMatrix(`${HERE}/matrix.json`);
  const ps: string[] = [];
  // Consistency: every feature profile ∈ the known set.
  for (const f of matrix.features) {
    const p = f.profile ?? "(none)";
    if (!KNOWN_PROFILES.includes(p)) ps.push(`feature \`${f.id}\` has unknown profile ${JSON.stringify(p)} (known: ${KNOWN_PROFILES.join(", ")})`);
  }
  // Vacuity.
  const present = new Set(matrix.features.map(f => f.profile));
  if (present.size < 2) ps.push(`only ${present.size} distinct profile(s) present (expected >= 2) — the profile axis read looks broken`);
  const count = (p: string) => matrix.features.filter(f => f.profile === p).length;
  if (count("RFC9682") < 1) ps.push(`RFC9682 introduces ${count("RFC9682")} features (expected >= 1) — the profile split broke`);
  if (count(VENDOR_PROFILE) !== VENDOR_FEATURE_COUNT)
    ps.push(`${VENDOR_PROFILE} introduces ${count(VENDOR_PROFILE)} features (expected exactly ${VENDOR_FEATURE_COUNT}) — the vendor surface changed`);
  return ps;
}

// ==================================================================================================
// entry
// ==================================================================================================
const argv = process.argv.slice(2);
const isCheck = argv.includes("--check");
const positional = argv.filter(a => !a.startsWith("--"));

if (positional.length === 2) {
  snapshotDiff(positional[0], positional[1]);
  process.exit(0);
}
if (positional.length > 2) {
  console.error(`Q6: expected 0 args (profile view), 1 arg (profile filter), or 2 args (old.json new.json) — got ${positional.length}`);
  process.exit(2);
}

if (isCheck) {
  const ps = checkProblems();
  if (ps.length) {
    console.log(`Q6 profile-diff gate: ${ps.length} problem(s)`);
    for (const p of ps) console.log(`  FAIL ${p}`);
    process.exit(1);
  }
  const matrix = loadMatrix(`${HERE}/matrix.json`);
  const present = KNOWN_PROFILES.filter(p => matrix.features.some(f => f.profile === p));
  const count = (p: string) => matrix.features.filter(f => f.profile === p).length;
  console.log(
    `Q6 profile-diff gate OK — ${present.length} profiles (${present.map(p => `${p}=${count(p)}`).join(", ")}) · ` +
      `all feature profiles ∈ known set · ${VENDOR_PROFILE} introduces exactly ${VENDOR_FEATURE_COUNT} vendor features`,
  );
  process.exit(0);
}

profileView(positional[0]);
