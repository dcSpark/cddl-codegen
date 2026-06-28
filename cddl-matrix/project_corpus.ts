#!/usr/bin/env bun
/**
 * Q2 / corpus (feature-axis) projection — STEP 1: validate the D3 canonical-fixture overlay.
 *
 * This is the first half of replacing tests/corpus/COVERAGE.md with a projection: it does NOT yet render
 * the doc. It proves the editorial half survives being made mechanical — i.e. that COVERAGE.md's hand
 * judgments ("array.cddl isolates arrays") hold up when checked against the actual fixture contents.
 *
 * Inputs:
 *   matrix.json                              - feature/control-op universe + cddl-codegen support
 *   annotations/corpus/cddl_codegen.toml     - the D3 overlay (construct -> canonical isolating fixture)
 *   ../tests/corpus/*.cddl                    - the fixtures (truth for `featuresIn`)
 *
 * Checks (HARD = exit nonzero):
 *   A. content drift (HARD)  - every overlay fixture must actually contain its construct (featuresIn)
 *   B. stale (HARD)          - every overlay id is a real matrix id; every fixture file exists
 *   D. floor completeness    - every construct the corpus exercises (the detected floor) has a canonical
 *      (HARD)                  fixture assigned (else a covered construct is silently unattributed)
 *   C. support seam (report) - an overlay covers a construct the matrix marks unsupported/out_of_profile
 *                              (the isolation-probe vs in-context mismatch) — surfaced, not yet fatal
 *
 * Run:  bun run project_corpus.ts
 *
 * ponytail: check-only for now (render comes after the control-op support + nuance-overlay steps). The
 * support seam (C) is reported non-fatal because reconciling "support" semantics (isolated-example probe
 * vs in-context coverage) is its own roadmap step — failing on it here would just block on a known TODO.
 */
import { readFileSync, existsSync } from "node:fs";
import { featuresIn } from "./corpus_detect.ts";
import overlay from "./annotations/corpus/cddl_codegen.toml";

const HERE = import.meta.dir;
const CORPUS = `${HERE}/../tests/corpus`;

interface Cover { id: string; fixture: string; note?: string }
interface Note { id: string; status: "partial" | "unsupported"; reason: string; code_anchor?: string }
interface Finding { text: string }
const ov = overlay as { cover: Cover[]; note?: Note[]; finding?: Finding[] };
const cover = ov.cover;
const notes = ov.note ?? [];
const findings = ov.finding ?? [];

const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: { id: string }[];
  control_operators: { id: string }[];
  annotations: { cddl_codegen: { id: string; status: string }[] };
};
const universe = new Set([...matrix.features.map(f => f.id), ...matrix.control_operators.map(c => c.id)]);
const supportById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.status]));

// detected floor: every construct that syntactically appears anywhere in the corpus
import { Glob } from "bun";
const files = [...new Glob("*.cddl").scanSync({ cwd: CORPUS })].sort();
const detected = new Set<string>();
for (const f of files) { const d = featuresIn(readFileSync(`${CORPUS}/${f}`, "utf8")); for (const id of [...d.rfc, ...d.ctl, ...d.dsl]) detected.add(id); }

const driftA: string[] = [];   // content: named fixture doesn't contain the construct
const staleB: string[] = [];   // unknown id / missing fixture
const seamC: { id: string; status: string }[] = [];
const coverIds = new Set<string>();
const fixtureCache = new Map<string, ReturnType<typeof featuresIn>>();
const detect = (fix: string) => fixtureCache.get(fix) ?? fixtureCache.set(fix, featuresIn(readFileSync(`${CORPUS}/${fix}`, "utf8"))).get(fix)!;

for (const c of cover) {
  coverIds.add(c.id);
  // B: stale
  if (!universe.has(c.id)) staleB.push(`unknown construct id \`${c.id}\``);
  if (!existsSync(`${CORPUS}/${c.fixture}`)) { staleB.push(`missing fixture \`${c.fixture}\` (for \`${c.id}\`)`); continue; }
  // A: content drift
  const d = detect(c.fixture);
  if (!d.rfc.has(c.id) && !d.ctl.has(c.id) && !d.dsl.has(c.id))
    driftA.push(`\`${c.fixture}\` does not exercise \`${c.id}\` — the canonical-fixture claim is false`);
  // C: support seam
  const st = supportById.get(c.id);
  if (st && st !== "supported") seamC.push({ id: c.id, status: st });
}

// D: floor completeness — a SUPPORTED construct the corpus exercises must have a canonical fixture.
// Detected-but-unsupported ids are exempt: they appear incidentally (e.g. a fixed value as a struct
// member) while the feature itself is a gap — they carry a ➖ note instead of a ✅ cover.
const unassigned = [...detected].filter(id => !coverIds.has(id) && supportById.get(id) === "supported").sort();
// overlay entries whose id the corpus never exercises at all (beyond A — e.g. a typo'd id that still parses)
const phantom = [...coverIds].filter(id => !detected.has(id)).sort();

// --- nuance notes: E status↔support agreement, F stale, G anchor-exists, + no-cover-for-unsupported ---
const SRC = `${HERE}/../src`;
const srcText = [...new Glob("**/*.rs").scanSync({ cwd: SRC })].map(f => readFileSync(`${SRC}/${f}`, "utf8")).join("\n");
const driftE: string[] = [];   // note status disagrees with the matrix support verdict
const staleF: string[] = [];   // unknown id / bad status
const driftG: string[] = [];   // code_anchor not found in src/ (self-invalidating evidence broke)
let noteSkipped = 0;           // notes whose id has no support data yet (control ops) — disclosed
for (const n of notes) {
  if (!universe.has(n.id)) { staleF.push(`unknown note id \`${n.id}\``); continue; }
  if (n.status !== "partial" && n.status !== "unsupported") { staleF.push(`\`${n.id}\`: bad status \`${n.status}\``); continue; }
  const sup = supportById.get(n.id);
  if (sup === undefined) noteSkipped++;   // e.g. a control op (no per-op support probe yet)
  else if (n.status === "partial" && sup !== "supported")
    driftE.push(`\`${n.id}\` noted partial (⚠️ parsed-but-not-honored) but matrix support is \`${sup}\` (must be supported)`);
  else if (n.status === "unsupported" && sup !== "unsupported" && sup !== "out_of_profile")
    driftE.push(`\`${n.id}\` noted ➖ unsupported but matrix support is \`${sup}\``);
  if (n.status === "unsupported" && coverIds.has(n.id))
    driftE.push(`\`${n.id}\` is noted ➖ unsupported yet also has a canonical-fixture (cover) entry — can't be both`);
  if (n.code_anchor && !srcText.includes(n.code_anchor))
    driftG.push(`\`${n.id}\`: code_anchor \`${n.code_anchor}\` not found in src/ (renamed/removed? the note is stale)`);
}

const w = (s = "") => console.log(s);
w(`corpus overlay: ${cover.length} canonical-fixture assignments over ${files.length} fixtures`);
w(`detected floor: ${detected.size} constructs exercised; ${coverIds.size} assigned a canonical fixture`);
w();
if (driftA.length) { w(`❌ A. CONTENT DRIFT (${driftA.length}):`); for (const x of driftA) w(`   - ${x}`); }
else w(`✅ A. content drift: every canonical fixture actually exercises its construct.`);
if (staleB.length) { w(`❌ B. STALE (${staleB.length}):`); for (const x of staleB) w(`   - ${x}`); }
else w(`✅ B. stale: every overlay id is a real matrix id and every fixture exists.`);
if (unassigned.length) { w(`❌ D. UNASSIGNED (corpus exercises these, no canonical fixture) (${unassigned.length}):`); w(`   ${unassigned.join(", ")}`); }
else w(`✅ D. completeness: every construct the corpus exercises has a canonical fixture.`);
if (phantom.length) w(`⚠️  overlay ids the corpus never exercises (review): ${phantom.join(", ")}`);
w();
w(`nuance overlay: ${notes.length} notes (${notes.filter(n => n.status === "partial").length} ⚠️ partial, ${notes.filter(n => n.status === "unsupported").length} ➖ unsupported), ${findings.length} findings`);
if (driftE.length) { w(`❌ E. NOTE↔SUPPORT DISAGREEMENT (${driftE.length}):`); for (const x of driftE) w(`   - ${x}`); }
else w(`✅ E. note status agrees with the matrix support verdict for every note.`);
if (staleF.length) { w(`❌ F. STALE NOTES (${staleF.length}):`); for (const x of staleF) w(`   - ${x}`); }
else w(`✅ F. every note id is a real matrix id with a valid status.`);
if (driftG.length) { w(`❌ G. BROKEN ANCHORS (${driftG.length}) — self-invalidating evidence no longer in src/:`); for (const x of driftG) w(`   - ${x}`); }
else w(`✅ G. every code_anchor is still present in src/ (evidence holds).`);
if (noteSkipped) w(`ℹ️  ${noteSkipped} note(s) not support-cross-checked yet (no per-op support data — control ops, deferred).`);
w();
w(`ℹ️  C. SUPPORT SEAM (${seamC.length}) — a construct with a ✅ cover entry yet marked not-supported by the`);
w(`   matrix (a directional mismatch — covered in one context, unsupported in another). Reported, not fatal;`);
w(`   resolve case-by-case: fix the example if it's a degenerate form (the construct really works), or keep`);
w(`   ➖ + a note and drop the cover if it's a genuine gap (don't relabel it ✅ by editing the example):`);
for (const s of seamC.sort((a, b) => (a.id < b.id ? -1 : 1))) w(`   - ${s.id}  (matrix: ${s.status})`);

const hardFail = driftA.length || staleB.length || unassigned.length || driftE.length || staleF.length || driftG.length;
w();
w(hardFail ? "RESULT: FAIL (overlay drift — see above)" : "RESULT: PASS (editorial mapping holds mechanically)");
process.exit(hardFail ? 1 : 0);
