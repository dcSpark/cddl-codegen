#!/usr/bin/env bun
/**
 * Q2 / corpus (feature-axis) projection — validate the D3 canonical-fixture overlay AND render the doc.
 *
 * This replaces the hand-maintained tests/corpus/COVERAGE.md with a projection. It has two halves, the
 * same split as project_golden_hex.ts:
 *   1. VALIDATE (checks A-G below) — proves the editorial overlay (canonical fixtures, nuance notes) holds
 *      up mechanically against the actual fixture contents + the matrix support verdict. This is the gate.
 *   2. RENDER — joins the validated overlay onto the matrix feature universe and writes the COVERAGE doc.
 *      Sections are PURELY DERIVED (profile × production, sorted by id) — no authored section layout.
 *
 * Inputs:
 *   matrix.json                              - feature/control-op universe + cddl-codegen support
 *   annotations/corpus/cddl_codegen.toml     - the D3 overlay (canonical fixture + nuance notes + findings)
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
 * Run:  bun run project_corpus.ts        -> validates, then writes ../tests/corpus/COVERAGE.md
 *
 * ponytail: COVERAGE.md IS this generated artifact — the hand doc was subsumed once two independent
 * reviews judged the projection a clear win (compile-gated support, per-control-op support, the c-style
 * enum feature, full ➖ rationale + findings). Regenerate after changing the matrix or overlay. One piece
 * is honestly DISCLOSED as deferred, not faked: per-role coverage
 * (item 6 — the floor is a text scan, can't say WHICH role) and per-control-op support (item 7 — the
 * matrix has 0 per-op support annotations). The support seam (C) is reported non-fatal for the same
 * reason it always was — reconciling isolated-probe vs in-context support is its own step.
 */
import { readFileSync, existsSync } from "node:fs";
import { featuresIn } from "./corpus_detect.ts";
import overlay from "./annotations/corpus/cddl_codegen.toml";

const HERE = import.meta.dir;
const CORPUS = `${HERE}/../tests/corpus`;
const OUT = `${CORPUS}/COVERAGE.md`;

interface Cover { id: string; fixture: string; note?: string }
interface Note { id: string; status: "partial" | "unsupported"; reason: string; code_anchor?: string }
interface Finding { text: string }
const ov = overlay as { cover: Cover[]; note?: Note[]; finding?: Finding[] };
const cover = ov.cover;
const notes = ov.note ?? [];
const findings = ov.finding ?? [];

interface Feature { id: string; production: string; profile: string; title: string; desc: string }
interface Ctl { id: string; name: string; profile: string }
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: Feature[];
  control_operators: Ctl[];
  annotations: { cddl_codegen: { id: string; status: string; evidence?: string }[] };
};
const universe = new Set([...matrix.features.map(f => f.id), ...matrix.control_operators.map(c => c.id)]);
const supportById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.status]));
const evidenceById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.evidence ?? ""]));

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

// ============================================================================================
// RENDER — join the validated overlay onto the matrix and write the COVERAGE doc (golden_hex shape).
// Sections are derived: profile (spec-first) -> production (alpha) -> id (alpha). No authored layout.
// ============================================================================================
const coverById = new Map(cover.map(c => [c.id, c]));
const noteById = new Map(notes.map(n => [n.id, n]));
const MARK = { covered: "✅", untested: "➕", unsupported: "➖", partial: "⚠️" } as const;
const shortProbe = (id: string) => evidenceById.get(id)?.replace(/^probe: /, "").split(";")[0] ?? "";
const anchor = (n: Note) => n.code_anchor ? `  [\`${n.code_anchor}\`]` : "";

// Per-feature mark. Note-first: a ⚠️/➖ note is the more informative verdict, and the validator already
// forbids an `unsupported` note coexisting with a cover (driftE), so this can't contradict the matrix.
const unexplained: string[] = [];   // unsupported feature with no rationale note (overlay TODO; surfaced)
function markFeature(id: string, track = true): { mark: string; ev: string } {
  const n = noteById.get(id);
  if (n?.status === "partial") return { mark: MARK.partial, ev: n.reason + anchor(n) };
  if (n?.status === "unsupported") return { mark: MARK.unsupported, ev: n.reason + anchor(n) };
  const cov = coverById.get(id);
  if (cov) return { mark: MARK.covered, ev: `\`${cov.fixture}\`` + (cov.note ? ` — ${cov.note}` : "") };
  const st = supportById.get(id);
  if (st === "supported") {
    // surface the compile-gate exemption clause (integration-tested) instead of a bare "exit 0"
    const ext = /standalone-compile N\/A \(([^)]+)\)/.exec(evidenceById.get(id) ?? "");
    return { mark: MARK.untested, ev: ext ? `supported; ${ext[1]}` : `supported, no corpus fixture (${shortProbe(id)})` };
  }
  if (st === "out_of_profile") return { mark: MARK.unsupported, ev: `out of profile — ${shortProbe(id)}` };
  if (track) unexplained.push(id);   // control ops aren't tracked: 28/37 unsupported is expected, not an overlay gap
  return { mark: MARK.unsupported, ev: `${shortProbe(id)}${track ? " — ⚠️ no rationale note yet (overlay gap)" : ""}` };
}

const PROFILE_ORDER = ["RFC8610", "RFC9682", "CDDL_CODEGEN"];
const PROFILE_LABEL: Record<string, string> = {
  RFC8610: "RFC 8610 / 9682 grammar + prelude (the spec backbone)",
  RFC9682: "RFC 9682 additions (newer than cddl-codegen's RFC 8610 target — out of profile)",
  CDDL_CODEGEN: "cddl-codegen vendor profile (comment DSL + sentinels — not RFC 8610)",
};
const tally = { [MARK.covered]: 0, [MARK.untested]: 0, [MARK.unsupported]: 0, [MARK.partial]: 0 } as Record<string, number>;

const L: string[] = [];
const o = (s = "") => L.push(s);
o("# Corpus coverage map — CDDL constructs (GENERATED)");
o();
o("> **GENERATED** by `cddl-matrix/project_corpus.ts` — do not hand-edit. Status (✅/➕/➖/⚠️) is the");
o("> execution-grounded matrix support verdict joined with the corpus overlay (canonical fixture, nuance");
o("> notes, findings) in `cddl-matrix/annotations/corpus/cddl_codegen.toml`. Regenerate after changing");
o("> either; CI fails on overlay drift (a note/cover that contradicts the matrix or the fixtures).");
o();
o("Tracks which CDDL constructs the snapshot **corpus** (`tests/corpus/*.cddl`) exercises, what's");
o("supported-but-untested (a corpus gap to fill), and what the generator does **not** support (the");
o("boundary). The feature universe + support are anchored to the spec (RFC 8610 grammar/prelude + the");
o("IANA control-op registry) and cddl-codegen's vendor profile — not to a self-feature-list, which is what");
o("makes the ➖ boundary rows visible. Sections are derived: **profile → production → id**.");
o();
o("## How this map works");
o();
o("- **Test:** `tests/corpus/<construct>.cddl`, driven by `snapshot_tests::feature_corpus` — each file is");
o("  generated under every flag profile (`default`/`preserve`/`json`) plus an IR dump, and the generated");
o("  *source* is snapshotted. Bless with `INSTA_UPDATE=always cargo test snapshot_tests`.");
o("- **Compile gate:** `integration_tests::feature_corpus_compiles` `cargo check`s every corpus file under");
o("  all three profiles, so a ✅ entry must produce **compiling** Rust under *all* of them.");
o("- **Axis:** the corpus snapshots generated *source*, not wire bytes — wire encodings are golden_hex's");
o("  axis (`tests/golden_hex/COVERAGE.md`, RFC 8949). A ✅ here means \"a fixture isolates this construct,\"");
o("  not \"every encoding of it is asserted.\"");
o("- **Evidence convention** (stable grep-able anchors, never line numbers) and the spec-anchoring");
o("  rationale live in `cddl-matrix/README.md` — this doc is one projection of that master.");
o("- **RFC reference:** RFC 8610 — <https://www.rfc-editor.org/rfc/rfc8610>; control ops from the IANA");
o("  CDDL control-operators registry (spans RFC 8610/9090/9165/9741).");
o();
o("## Legend");
o();
o("| mark | meaning |");
o("|------|---------|");
o("| ✅ | covered — a corpus fixture isolates this construct |");
o("| ➕ | **supported but untested** — accepted by the generator, no corpus fixture yet (an actionable gap) |");
o("| ➖ | **not supported** — rejected / `panic!` / no handling branch (documents the boundary) |");
o("| ⚠️ | partial — parsed but the semantics aren't honored (accepted, not modeled) |");
o();

// --- feature tables: profile -> production -> id ---
for (const profile of PROFILE_ORDER) {
  const feats = matrix.features.filter(f => f.profile === profile);
  if (!feats.length) continue;
  o(`## ${PROFILE_LABEL[profile] ?? profile}`);
  o();
  const prods = [...new Set(feats.map(f => f.production))].sort();
  for (const prod of prods) {
    const rows = feats.filter(f => f.production === prod).sort((a, b) => (a.id < b.id ? -1 : 1));
    o(`### \`${prod}\` (${rows.length})`);
    o();
    o("| construct | | description | evidence |");
    o("|-----------|---|-------------|----------|");
    for (const f of rows) {
      const { mark, ev } = markFeature(f.id);
      if (mark in tally) tally[mark]++;
      o(`| \`${f.id}\` | ${mark} | ${f.title} | ${ev} |`);
    }
    o();
  }
}

// --- control operators (§3.8 + IANA registry): execution-probed per op (compile-gated), keyed ctl.<name> ---
const ctlTally = { [MARK.covered]: 0, [MARK.untested]: 0, [MARK.unsupported]: 0, [MARK.partial]: 0 } as Record<string, number>;
o("## Control operators (`ctlop`, §3.8 + IANA registry)");
o();
o("> Support is execution-probed per operator (generate + compile), keyed `ctl.<name>` — same probe as");
o("> features. cddl-codegen implements **9 of the 37** IANA operators (`.size .cbor .default .eq .ne .le");
o("> .lt .ge .gt`); the rest panic or parse-reject. The generic \"a control op is applied\" feature");
o("> (`type1.ctlop`) appears under `RFC8610 / type1` above.");
o();
o("| operator | | evidence |");
o("|----------|---|----------|");
for (const c of [...matrix.control_operators].sort((a, b) => (a.id < b.id ? -1 : 1))) {
  const { mark, ev } = markFeature(c.id, false);
  if (mark in ctlTally) ctlTally[mark]++;
  const prof = c.profile !== "RFC8610" ? ` _(${c.profile})_` : "";
  o(`| \`${c.name}\`${prof} | ${mark} | ${ev} |`);
}
o();

// --- doc-level findings (narrative) ---
o("## Notable findings");
o();
findings.forEach((f, i) => o(`${i + 1}. ${f.text}`));
o();

// --- summary + honest disclosure of what's deferred ---
o("## Summary");
o();
const feC = tally[MARK.covered], feP = tally[MARK.untested], feN = tally[MARK.unsupported], feW = tally[MARK.partial];
const ctC = ctlTally[MARK.covered], ctP = ctlTally[MARK.untested], ctN = ctlTally[MARK.unsupported];
o(`- Features: **${matrix.features.length}** — ✅ ${feC} covered · ➕ ${feP} supported-untested · ⚠️ ${feW} partial · ➖ ${feN} not supported`);
o(`- Control operators: **${matrix.control_operators.length}** — ✅ ${ctC} covered · ➕ ${ctP} supported-untested · ➖ ${ctN} not supported (cddl-codegen implements ${ctC + ctP} of ${matrix.control_operators.length})`);
o(`- Corpus fixtures: ${files.length}`);
o();
o("**Deferred (disclosed, not faked):**");
o("- **Per-role coverage (ROADMAP item 6).** Coverage here is feature-axis only — the corpus floor is a");
o("  text scan that detects THAT a construct appears, not in WHICH container role. Per-cell *support*");
o("  (role × feature) exists in the matrix but role-keyed *coverage* awaits the `cddl`-crate AST walk.");
if (unexplained.length) {
  o(`- **Overlay rationale gap.** ${unexplained.length} unsupported feature(s) render ➖ with no rationale note —`);
  o(`  add a \`[[note]]\` (reason + code_anchor) to the overlay: ${unexplained.sort().map(x => `\`${x}\``).join(", ")}.`);
}
o();

await Bun.write(OUT, L.join("\n"));
w();
w(`wrote ${OUT.replace(`${HERE}/../`, "")}`);
w(`  features: ✅ ${feC}  ➕ ${feP}  ⚠️ ${feW}  ➖ ${feN}; control-ops: ✅ ${ctC} ➕ ${ctP} ➖ ${ctN}; unexplained ➖: ${unexplained.length}`);

const hardFail = driftA.length || staleB.length || unassigned.length || driftE.length || staleF.length || driftG.length;
w();
w(hardFail ? "RESULT: FAIL (overlay drift — see above)" : "RESULT: PASS (editorial mapping holds mechanically)");
process.exit(hardFail ? 1 : 0);
