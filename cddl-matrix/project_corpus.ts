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
 * COVERAGE.md IS this generated artifact — the hand doc was subsumed once two independent
 * reviews judged the projection a clear win (compile-gated support, per-control-op support, the c-style
 * enum feature, full ➖ rationale + findings). Regenerate after changing the matrix or overlay.
 * PER-CELL (role × feature) coverage is wired: a [[cover]] with a `role` is verified against a
 * real cddl-crate AST walk (examples/ast_roles.rs) + the matrix's per-cell support verdict, so a construct
 * ➖ as a standalone type still shows its supported member/choice role. The support seam (C) is reported
 * non-fatal — reconciling isolated-probe vs in-context support is its own step.
 * The AST role floor additionally runs over the WHOLE corpus and is joined against the containment
 * relation into a rendered role × feature GRID, whose point is the cell the corpus exercises and the
 * matrix models nothing for — informational (no verdict), enforced against staleness by `coverage_md_diff`.
 */
import { readFileSync, existsSync } from "node:fs";
import { annotationsHeaderLines } from "./lib";
import { featuresIn, prepareDslFeatures, rolesIn, NO_DETECTOR } from "./corpus_detect.ts";
import overlay from "./annotations/corpus/cddl_codegen.toml";

const HERE = import.meta.dir;
const CORPUS = `${HERE}/../tests/corpus`;
const OUT = `${CORPUS}/COVERAGE.md`;

// A [[cover]] with `role` is a per-CELL coverage claim (role × feature), verified against the AST role
// floor + the cell's per-cell support verdict; without `role` it is a feature-axis claim (text scan).
interface Cover { id: string; fixture: string; role?: string; note?: string }
interface Note { id: string; status: "partial" | "unsupported"; reason: string; code_anchor?: string }
interface Finding { text: string }
const ov = overlay as { cover: Cover[]; note?: Note[]; finding?: Finding[] };
const cover = ov.cover;
const featureCovers = cover.filter(c => !c.role);   // feature-axis (role-agnostic)
const cellCovers = cover.filter(c => c.role);       // per-cell (role × feature) — the role-keyed cover axis
const notes = ov.note ?? [];
const findings = ov.finding ?? [];

interface Feature { id: string; production: string; profile: string; title: string; desc: string }
interface Ctl { id: string; name: string; profile: string }
interface Role { id: string; title: string }
// A containment row is SHAPE-granular: `role`/`feature` say WHICH (role × feature) cell it sits in,
// and the rest of the id is a variation suffix naming the concrete shape probed. Always key on the
// `role`/`feature` FIELDS — a `.`-split of the id mis-keys every varied row
// (`contain.array-element.type2.tag.fixed_null`, `contain.map-key.memberkey.type1.uint_arrow_multi`).
interface Contain { id: string; role: string; feature: string; spec: "allowed" | "disallowed" }
interface Support {
  id: string;
  status: string;
  evidence?: string;
  emission?: Record<string, { status: string; evidence?: string }>;
}
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as {
  features: Feature[];
  control_operators: Ctl[];
  roles: Role[];
  containment: Contain[];
  annotations: { cddl_codegen: Support[] };
};
const universe = new Set([...matrix.features.map(f => f.id), ...matrix.control_operators.map(c => c.id)]);
const supportById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.status]));
const evidenceById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.evidence ?? ""]));
const emissionById = new Map(matrix.annotations.cddl_codegen.map(s => [s.id, s.emission ?? {}]));

// The render iterates exactly these profiles; a feature with any OTHER profile would silently vanish
// from every table and tally while the summary still prints the full feature count (the doc would
// stop summing) — so an unknown profile is validated as hard drift, not dropped.
const PROFILE_ORDER = ["RFC8610", "RFC9682", "CDDL_CODEGEN"];

// detected floor: every construct that syntactically appears anywhere in the corpus
import { Glob } from "bun";
const files = [...new Glob("*.cddl").scanSync({ cwd: CORPUS })].sort();
prepareDslFeatures(files.map(f => readFileSync(`${CORPUS}/${f}`, "utf8")));
const detected = new Set<string>();
for (const f of files) { const d = featuresIn(readFileSync(`${CORPUS}/${f}`, "utf8")); for (const id of [...d.rfc, ...d.ctl, ...d.dsl]) detected.add(id); }

const driftA: string[] = [];   // content: named fixture doesn't contain the construct (in the claimed role)
const staleB: string[] = [];   // unknown id / missing fixture / unverifiable cell
const seamC: { id: string; status: string }[] = [];
const driftH: string[] = [];   // per-cell cover whose cell isn't `supported` in the matrix (can't claim ✅)
const coverIds = new Set<string>();   // feature-axis cover ids only (drive the ✅ per-feature mark)
const fixtureCache = new Map<string, ReturnType<typeof featuresIn>>();
const detect = (fix: string) => fixtureCache.get(fix) ?? fixtureCache.set(fix, featuresIn(readFileSync(`${CORPUS}/${fix}`, "utf8"))).get(fix)!;

// --- feature-axis covers (role-agnostic; text-scan verified, handles dsl.*) ---
for (const c of featureCovers) {
  // B: duplicate — the render maps are last-wins, so a duplicate id would silently drop the
  // earlier cover's fixture claim from the doc with no signal
  if (coverIds.has(c.id)) staleB.push(`duplicate feature-axis cover for \`${c.id}\` — the render keeps only the last`);
  coverIds.add(c.id);
  // B: stale
  if (!universe.has(c.id)) staleB.push(`unknown construct id \`${c.id}\``);
  // B: the fixture must be a file IN the corpus glob — bare existsSync would also accept a path
  // outside tests/corpus/ (e.g. `../../supported.cddl`), a false "the corpus isolates this" claim
  if (!files.includes(c.fixture)) { staleB.push(`fixture \`${c.fixture}\` (for \`${c.id}\`) is not a file in tests/corpus/`); continue; }
  // A: content drift
  const d = detect(c.fixture);
  if (!d.rfc.has(c.id) && !d.ctl.has(c.id) && !d.dsl.has(c.id))
    driftA.push(`\`${c.fixture}\` does not exercise \`${c.id}\` — the canonical-fixture claim is false`);
  // C: support seam
  const st = supportById.get(c.id);
  if (st && st !== "supported") seamC.push({ id: c.id, status: st });
}

// --- per-cell (role × feature) covers. A cell cover legitimately coexists with an
//     unsupported per-feature note (the note = top-level gap; the cover = a supported role), because
//     `coverIds` above is feature-axis only, so the driftE "cover + unsupported note" rule doesn't fire. ---
// The rolesIn batch is built ONLY from fixtures that exist in the corpus glob: ast_roles panics on
// an unreadable file, so batching a typo'd fixture would abort the whole run with a Rust panic
// before any of the diagnostics below print (the staleB branch was unreachable). A parse failure
// inside ast_roles is likewise caught and downgraded to a hard staleB row instead of a crash.
// The batch is the WHOLE corpus, not just the role-keyed cover fixtures: the role × feature grid
// rendered below needs the floor for every fixture, and check A's per-cell branch reads the same,
// larger map unchanged. `files` IS the corpus glob, so the "must be a file in tests/corpus/" guard
// above is satisfied by construction here. Consequence: the cargo run is now UNCONDITIONAL (it was
// previously skipped when the overlay carried no role-keyed cover). That costs ~0.05 s on a warm
// build — the batch is one `cargo run --example ast_roles` over all fixtures either way.
const floorFixtures = files;
let cellFloor = new Map<string, Set<string>>();
let cellFloorFailed = false;
if (floorFixtures.length) {
  try {
    cellFloor = rolesIn(floorFixtures);
  } catch (e) {
    cellFloorFailed = true;
    staleB.push(`AST role floor failed — per-cell content drift (check A) could not be verified: ${String(e).split("\n")[0]}`);
  }
}
const cellCoversById = new Map<string, { role: string; fixture: string; note?: string }[]>();
const seenCells = new Set<string>();
for (const c of cellCovers) {
  const role = c.role!;
  // B: duplicate (id, role) — both entries would render as repeated "also ✅ @role" clauses
  const cellKey = `${c.id}@${role}`;
  if (seenCells.has(cellKey)) staleB.push(`duplicate per-cell cover \`${cellKey}\``);
  seenCells.add(cellKey);
  // B: stale id / fixture (same corpus-glob containment rule as the feature axis)
  if (!universe.has(c.id)) { staleB.push(`unknown construct id \`${c.id}\` (cell cover)`); continue; }
  if (!files.includes(c.fixture)) { staleB.push(`fixture \`${c.fixture}\` (cell \`${cellKey}\`) is not a file in tests/corpus/`); continue; }
  // H: the matrix must model this (role × feature) cell AND mark it `supported` — else we'd be claiming ✅
  //    on a non-modelled or unsupported cell (the per-cell analog of the feature-axis support-seam).
  const cellId = `contain.${role.replace(/^role\./, "")}.${c.id}`;
  const cellSupport = supportById.get(cellId);
  if (cellSupport === undefined) staleB.push(`cell cover \`${cellKey}\`: no containment cell \`${cellId}\` to verify against`);
  else if (cellSupport !== "supported") driftH.push(`\`${cellKey}\` claims coverage but the matrix marks cell \`${cellId}\` \`${cellSupport}\``);
  // A: content drift — the AST role floor must actually exercise F in this role in the fixture
  if (!cellFloorFailed && !cellFloor.get(c.fixture)?.has(cellKey))
    driftA.push(`\`${c.fixture}\` does not exercise \`${c.id}\` in \`${role}\` (AST role floor) — the per-cell claim is false`);
  if (!cellCoversById.has(c.id)) cellCoversById.set(c.id, []);
  cellCoversById.get(c.id)!.push({ role, fixture: c.fixture, note: c.note });
}

// --- the role × feature grid: join the whole-corpus AST role floor against the containment relation ---
//
// THE GRANULARITY TRAP — the single most likely wrong turn here, so it is stated at the site AND in
// the rendered legend. The two sides of this join are at DIFFERENT granularities:
//   - the floor is FEATURE-granular: it records "an array appears in array-element role", nothing more;
//   - a containment row is SHAPE-granular: `contain.array-element.type2.array`'s example is
//     `a = [[int]]` — an ANONYMOUS INLINE array — and that shape is `unsupported`.
// The corpus exercises `type2.array@role.array-element` in 5 fixtures and every corpus fixture compiles
// under all three profiles (`integration_tests::feature_corpus_compiles`), because those fixtures use a
// NAMED rule reference — a different shape. So a cross-check of "the corpus exercises this cell" against
// "the matrix says this cell is unsupported" is INVALID and must never be built: it is not a
// contradiction, it is two different shapes. Many such pairs exist today
// (`type2.array@role.choice-member`, 10 fixtures; `type2.map@role.array-element`, 2 fixtures; …).
// What the grid DOES say is one thing the join can support: a cell the corpus exercises and the
// containment relation has NO row for is a cell nothing has an opinion about.
const cellKeyOf = (feature: string, role: string) => `${feature}@${role}`;
const floorCells = new Map<string, string[]>();      // cell -> fixtures exercising it
for (const [f, cells] of cellFloor) for (const c of cells) {
  if (!floorCells.has(c)) floorCells.set(c, []);
  floorCells.get(c)!.push(f);
}
const modelledCells = new Map<string, Contain[]>();  // cell -> the containment rows sitting at it
for (const c of matrix.containment) {
  const k = cellKeyOf(c.feature, c.role);
  if (!modelledCells.has(k)) modelledCells.set(k, []);
  modelledCells.get(k)!.push(c);
}
// A base cell can hold SEVERAL variation rows with different verdicts — that is normal, not drift
// (`memberkey.type1@role.map-key` holds 11 rows, 4 supported / 7 unsupported), so the cell mark
// summarises them. `spec = "disallowed"` rows are DELIBERATELY never support-probed (verify.ts only
// probes where the nesting is spec-valid), so their missing support row is not an ungrounded cell and
// must not render `?` — a permanent "not yet" would be a false claim. `?` is reserved for a
// spec-ALLOWED row awaiting its grounding verify.ts run.
const GRID = {
  supported: "✅",      // modelled; every probed row supported
  unsupported: "➖",    // modelled; every probed row unsupported / out_of_profile
  mixed: "◐",           // modelled; probed rows disagree (a shape boundary inside one cell)
  ungrounded: "?",      // modelled; a spec-allowed row has no support row yet
  illegal: "✗",         // modelled ONLY as spec-disallowed — the grammar forbids this nesting
  exercised: "·",       // NOT modelled, yet ≥1 corpus fixture exercises it
  blank: "",            // neither modelled nor exercised — nothing here has an opinion
} as const;
interface GridRow { spec: "allowed" | "disallowed"; status: string | undefined }
// Pure so the mark table is checkable without a matrix: `gridMarkSelfCheck` below pins every branch,
// including the two the CURRENT matrix cannot reach (no ungrounded spec-allowed row exists today, so
// `?` would otherwise be untested dead code in a fast-tier gate's render).
function markForRows(rows: readonly GridRow[], exercised: boolean): string {
  if (!rows.length) return exercised ? GRID.exercised : GRID.blank;
  if (rows.some(r => r.spec === "allowed" && r.status === undefined)) return GRID.ungrounded;
  const probed = rows.map(r => r.status).filter((s): s is string => s !== undefined);
  if (!probed.length) return GRID.illegal;
  const ok = probed.filter(s => s === "supported").length;
  return ok === probed.length ? GRID.supported : ok === 0 ? GRID.unsupported : GRID.mixed;
}
function gridMarkSelfCheck() {
  const R = (spec: GridRow["spec"], status: string | undefined): GridRow => ({ spec, status });
  const cases: [string, GridRow[], boolean, string][] = [
    ["neither modelled nor exercised", [], false, GRID.blank],
    ["exercised, nothing models it", [], true, GRID.exercised],
    ["all probed shapes supported", [R("allowed", "supported"), R("allowed", "supported")], true, GRID.supported],
    ["all probed shapes unsupported", [R("allowed", "unsupported")], false, GRID.unsupported],
    ["out_of_profile counts as not-supported", [R("allowed", "out_of_profile")], false, GRID.unsupported],
    ["probed shapes disagree", [R("allowed", "supported"), R("allowed", "unsupported")], false, GRID.mixed],
    // An ungrounded spec-allowed row OUTRANKS its probed siblings: a cell awaiting its verify.ts
    // grounding run must say so, not be averaged into a verdict it has not earned.
    ["ungrounded row outranks probed siblings", [R("allowed", undefined), R("allowed", "supported")], false, GRID.ungrounded],
    // A spec-DISALLOWED row is never support-probed BY DESIGN (verify.ts probes only spec-valid
    // nestings), so its absent support row is not "not yet" — rendering `?` would be a permanent
    // false claim about a cell that will never be probed.
    ["disallowed-only cell is not ungrounded", [R("disallowed", undefined)], false, GRID.illegal],
    ["a disallowed sibling does not mask a probed verdict", [R("disallowed", undefined), R("allowed", "supported")], false, GRID.supported],
  ];
  for (const [name, rows, exercised, want] of cases) {
    const got = markForRows(rows, exercised);
    if (got !== want) throw new Error(`project_corpus.ts gridMark self-check '${name}': expected '${want}', got '${got}'`);
  }
}
gridMarkSelfCheck();
const gridMark = (cell: string): string =>
  markForRows((modelledCells.get(cell) ?? []).map(r => ({ spec: r.spec, status: supportById.get(r.id) })), floorCells.has(cell));
const gridExercisedUnmodelled = [...floorCells.keys()].filter(k => !modelledCells.has(k)).sort();
const gridExercisedModelled = [...floorCells.keys()].filter(k => modelledCells.has(k)).sort();
const gridModelledUnexercised = [...modelledCells.keys()].filter(k => !floorCells.has(k)).sort();
// Rows: every construct id modelled in ≥1 role OR exercised in ≥1 role. An id that is neither would
// render an all-blank row, so it is omitted rather than padding the grid with 50 empty lines.
const gridFeatureIds = [...new Set([
  ...[...floorCells.keys()].map(k => k.slice(0, k.lastIndexOf("@"))),
  ...matrix.containment.map(c => c.feature),
])].sort();

// D: floor completeness — a SUPPORTED construct the corpus exercises must have a canonical fixture.
// Detected-but-unsupported ids are exempt: they appear incidentally (e.g. a fixed value as a struct
// member) while the feature itself is a gap — they carry a ➖ note instead of a ✅ cover.
const unassigned = [...detected].filter(id => !coverIds.has(id) && supportById.get(id) === "supported").sort();
// overlay entries whose id the corpus never exercises at all (beyond A — e.g. a typo'd id that still parses)
const phantom = [...coverIds].filter(id => !detected.has(id)).sort();
// disk→overlay direction: fixtures referenced by no [[cover]] at all. Report-only — they're still
// snapshot/compile-tested by the Rust drivers, but an orphan should be a visible editorial decision,
// not silence (check D is construct-keyed and can't see them).
const coveredFixtures = new Set(cover.map(c => c.fixture));
const unreferenced = files.filter(f => !coveredFixtures.has(f));
// supported ids the text floor structurally cannot detect: check D can never demand covers for these
// (declared in corpus_detect.NO_DETECTOR; surfaced here so the blindness is stated in the gate's output)
const detectorBlind = [...NO_DETECTOR].filter(id => supportById.get(id) === "supported").sort();
// unknown profile -> features silently dropped from every rendered table + tally (hard; see PROFILE_ORDER)
for (const p of [...new Set(matrix.features.map(f => f.profile))].sort())
  if (!PROFILE_ORDER.includes(p))
    staleB.push(`feature profile \`${p}\` is not in PROFILE_ORDER — its features would vanish from the rendered tables and tallies`);

// --- nuance notes: E status↔support agreement, F stale, G anchor-exists, + no-cover-for-unsupported ---
const SRC = `${HERE}/../src`;
const srcText = [...new Glob("**/*.rs").scanSync({ cwd: SRC })].map(f => readFileSync(`${SRC}/${f}`, "utf8")).join("\n");
const driftE: string[] = [];   // note status disagrees with the matrix support verdict
const staleF: string[] = [];   // unknown id / bad status
const driftG: string[] = [];   // code_anchor not found in src/ (self-invalidating evidence broke)
const noteIds = new Set<string>();
for (const n of notes) {
  // duplicate note ids are a silent last-wins collapse in `noteById` (the rendered rationale for the
  // earlier note is dropped with no signal) — mirror the feature-axis cover dup check at line ~89.
  if (noteIds.has(n.id)) staleF.push(`duplicate note for \`${n.id}\` — the render keeps only the last`);
  noteIds.add(n.id);
  if (!universe.has(n.id)) { staleF.push(`unknown note id \`${n.id}\``); continue; }
  if (n.status !== "partial" && n.status !== "unsupported") { staleF.push(`\`${n.id}\`: bad status \`${n.status}\``); continue; }
  const sup = supportById.get(n.id);
  // every matrix id (features AND control ops) has an execution-grounded support row now, so a
  // missing one means the annotations are stale/truncated — hard, not a silent cross-check skip
  if (sup === undefined) staleF.push(`\`${n.id}\`: no support row in the matrix annotations (regenerate: bun run verify.ts && build_matrix.ts)`);
  else if (n.status === "partial" && sup !== "supported")
    driftE.push(`\`${n.id}\` noted partial (⚠️ parsed-but-not-honored) but matrix support is \`${sup}\` (must be supported)`);
  else if (n.status === "unsupported" && sup !== "unsupported" && sup !== "out_of_profile")
    driftE.push(`\`${n.id}\` noted ➖ unsupported but matrix support is \`${sup}\``);
  // feature-axis cover only (coverIds excludes cell covers) — a per-cell cover legitimately coexists with
  // an unsupported per-feature note (the note = top-level gap; the cover = a supported role).
  if (n.status === "unsupported" && coverIds.has(n.id))
    driftE.push(`\`${n.id}\` is noted ➖ unsupported yet also has a feature-axis cover entry — can't be both`);
  if (n.code_anchor && !srcText.includes(n.code_anchor))
    driftG.push(`\`${n.id}\`: code_anchor \`${n.code_anchor}\` not found in src/ (renamed/removed? the note is stale)`);
}

// --- I. findings-claims arm: a [[finding]] that states a FAILURE claim — a defect ("Bug —"/"Gap —") or
//     a proposed "Candidate cddl-codegen fix" — must name at least one resolvable tracking artifact (an
//     existing file under tests/, or a test-fn / ledger-const symbol in src/tests/), checked against the
//     tree, so a finding whose pin disappears (or that never had one) fails LOUDLY here instead of rotting
//     as prose. This is the FINDINGS-LEDGER home of the testing-roadmap.toml "stale known-limitation prose
//     surviving its fix" residual (fired by findings 7/8/11 + the prelude/value notes going stale in one
//     new home): findings live in THIS overlay and render into COVERAGE.md — a GENERATED span — so
//     lint_doc_citations (which scans hand docs only) can never see them; project_corpus.ts already parses
//     and renders the `findings` array and reads src/, making it the arm's architectural home. A citation
//     that RESOLVES does not validate the claim's SEMANTICS (whether a fix is genuinely needed) — that
//     stays review-owned; this arm catches only an ABSENT pin. Known false-negative looseness, accepted
//     deliberately: resolution succeeds if ANY backtick token matches (src/tests/ text is matched by
//     substring), so a finding whose only tokens are common spellings (`uint`, a CDDL snippet that
//     happens to appear in a test) passes vacuously. Tightening (e.g. requiring a tests/-path or
//     word-boundary symbol match) is warranted on the first finding observed passing on an incidental
//     token — not before (the current findings all cite real fixture paths/symbols).
const TESTS_SRC = `${SRC}/tests`;
const testsText = [...new Glob("**/*.rs").scanSync({ cwd: TESTS_SRC })].map(f => readFileSync(`${TESTS_SRC}/${f}`, "utf8")).join("\n");
const isFailureClaim = (t: string) => /^(Bug|Gap)\s+—/.test(t) || /[Cc]andidate cddl-codegen fix/.test(t);
const findingTokenResolves = (tok: string): boolean => {
  if (tok.startsWith("tests/") && existsSync(`${HERE}/../${tok}`)) return true;   // fixture / hand vector / reject row
  return testsText.includes(tok);                                                 // test fn name / ledger const in src/tests/
};
const staleFindingClaims: string[] = [];
for (const f of findings) {
  if (!isFailureClaim(f.text)) continue;
  const tokens = [...f.text.matchAll(/`([^`\n]+)`/g)].map(m => m[1]!);
  if (!tokens.some(findingTokenResolves))
    staleFindingClaims.push(`failure-claim finding names no resolvable pin (need a \`tests/…\` file or a src/tests/ symbol): "${f.text.slice(0, 100)}…"`);
}

const w = (s = "") => console.log(s);

// J. the machine-owned annotations header. verify.ts rewrites annotations/cddl_codegen.toml from
// lib.ts's `annotationsHeaderLines` template after every passing run, so a hand edit to the
// RENDERED header is silently reverted by the next run (proven once — see the template's doc
// comment). This check makes both desync directions loud at fast-tier speed: a hand edit to the
// rendered file fails here immediately (edit the template instead), and a template edit not
// mirrored into the committed file fails here until it is. The committed file's per-leg form (the
// decode-foreign and component-execution paragraphs, each emitted only when its leg is on) is
// detected from its own bytes, so an opted-out run's output is compared against the matching
// template flavor.
const headerDriftJ: string[] = (() => {
  const lines = readFileSync(`${HERE}/annotations/cddl_codegen.toml`, "utf8").split("\n");
  const expected = annotationsHeaderLines(
    lines.some(l => l.startsWith("# DECODE-FOREIGN clause")),
    lines.some(l => l.startsWith("# COMPONENT-EXECUTION clause")),
  );
  for (let i = 0; i < expected.length; i++) {
    if (lines[i] !== expected[i]) {
      // the first divergence is the story; dumping the rest is noise
      return [
        `annotations/cddl_codegen.toml line ${i + 1} diverges from the header template:`,
        `  committed: ${JSON.stringify(lines[i] ?? "<EOF>")}`,
        `  template:  ${JSON.stringify(expected[i])}`,
        `  the header is a machine-regenerated region — edit lib.ts \`annotationsHeaderLines\` (and mirror`,
        `  the change into the committed file), never the rendered header alone`,
      ];
    }
  }
  return [];
})();

// --- CHECK REGISTRY ------------------------------------------------------------------------------
// The verdict-bearing checks live in ONE list that drives both `hardFail` and the ❌/✅ console
// blocks, so a check cannot fail the gate without printing what failed (the sibling of the same
// invariant in verify.ts's SECTIONS). `group` places a check in the run's narrative — the overlay
// half prints before the informational lines, the nuance half after — without letting either half
// become a second hand-maintained list. Purely informational output (phantom / unreferenced /
// detectorBlind / the C. SUPPORT SEAM block) is deliberately NOT here: it bears no verdict, so
// registering it would only put non-verdict rows in the structure that defines the verdict.
interface CheckSection {
  key: string;                              // stable identifier, matches the collection it reports
  group: "overlay" | "nuance";
  hard: boolean;                            // does a non-empty `items` fail the gate?
  items: readonly string[];
  fail: (n: number) => string;              // the ❌ heading
  ok: string;                               // the ✅ line printed when the check is clean
  body?: (items: readonly string[]) => string[];   // default: one `   - <item>` line per item
}
const CHECKS: CheckSection[] = [
  {
    key: "driftA", group: "overlay", hard: true, items: driftA,
    fail: n => `❌ A. CONTENT DRIFT (${n}):`,
    ok: `✅ A. content drift: every cover (feature-axis + per-cell role) is exercised by its fixture.`,
  },
  {
    key: "staleB", group: "overlay", hard: true, items: staleB,
    fail: n => `❌ B. STALE (${n}):`,
    ok: `✅ B. stale: every overlay id/fixture is real and every cell cover has a containment cell.`,
  },
  {
    key: "driftH", group: "overlay", hard: true, items: driftH,
    fail: n => `❌ H. CELL SUPPORT (${n}) — per-cell cover claims ✅ on a non-supported cell:`,
    ok: `✅ H. per-cell coverage: every role-keyed cover targets a \`supported\` (role × feature) cell.`,
  },
  {
    key: "unassigned", group: "overlay", hard: true, items: unassigned,
    fail: n => `❌ D. UNASSIGNED (corpus exercises these, no canonical fixture) (${n}):`,
    ok: `✅ D. completeness: every construct the corpus exercises has a canonical fixture.`,
    body: items => [`   ${items.join(", ")}`],   // bare ids read better as one comma-joined line
  },
  {
    key: "driftE", group: "nuance", hard: true, items: driftE,
    fail: n => `❌ E. NOTE↔SUPPORT DISAGREEMENT (${n}):`,
    ok: `✅ E. note status agrees with the matrix support verdict for every note.`,
  },
  {
    key: "staleF", group: "nuance", hard: true, items: staleF,
    fail: n => `❌ F. STALE NOTES (${n}):`,
    ok: `✅ F. every note id is a real matrix id with a valid status.`,
  },
  {
    key: "driftG", group: "nuance", hard: true, items: driftG,
    fail: n => `❌ G. BROKEN ANCHORS (${n}) — self-invalidating evidence no longer in src/:`,
    ok: `✅ G. every code_anchor is still present in src/ (evidence holds).`,
  },
  {
    key: "staleFindingClaims", group: "nuance", hard: true, items: staleFindingClaims,
    fail: n => `❌ I. STALE FINDING CLAIMS (${n}) — a failure-claim finding with no resolvable pin:`,
    ok: `✅ I. every failure-claim finding names a resolvable tracking pin (tests/ file or src/tests/ symbol).`,
  },
  {
    key: "headerDriftJ", group: "nuance", hard: true, items: headerDriftJ,
    fail: () => `❌ J. ANNOTATIONS HEADER DRIFT — committed header != lib.ts \`annotationsHeaderLines\` template:`,
    ok: `✅ J. the annotations header matches its template (hand edits to the rendered header cannot survive).`,
    body: items => items.map(x => `   ${x}`),
  },
];
{
  const seen = new Set<string>();
  for (const c of CHECKS) {
    if (!c.key) throw new Error("project_corpus.ts CHECKS: a check has an empty key");
    if (seen.has(c.key)) throw new Error(`project_corpus.ts CHECKS: duplicate check key '${c.key}'`);
    seen.add(c.key);
  }
}
const emitted = new Set<string>();
const emitChecks = (group: CheckSection["group"]) => {
  for (const c of CHECKS.filter(c => c.group === group)) {
    emitted.add(c.key);
    if (!c.items.length) { w(c.ok); continue; }
    w(c.fail(c.items.length));
    for (const line of (c.body ?? (items => items.map(x => `   - ${x}`)))(c.items)) w(line);
  }
};

w(`corpus overlay: ${featureCovers.length} feature-axis + ${cellCovers.length} per-cell (role×feature) assignments over ${files.length} fixtures`);
w(`detected floor: ${detected.size} constructs exercised; ${coverIds.size} assigned a canonical fixture`);
w();
emitChecks("overlay");
if (phantom.length) w(`⚠️  overlay ids the corpus never exercises (review): ${phantom.join(", ")}`);
if (unreferenced.length) w(`ℹ️  fixtures no [[cover]] references (${unreferenced.length}/${files.length} — attribution gap, not a testing gap): ${unreferenced.join(", ")}`);
if (detectorBlind.length) w(`ℹ️  supported ids undetectable by the text floor (check D cannot demand covers for these — see corpus_detect.NO_DETECTOR): ${detectorBlind.join(", ")}`);
w();
w(`nuance overlay: ${notes.length} notes (${notes.filter(n => n.status === "partial").length} ⚠️ partial, ${notes.filter(n => n.status === "unsupported").length} ➖ unsupported), ${findings.length} findings`);
emitChecks("nuance");
// Every registered check must have reached the console — a group value no `emitChecks` call names
// would otherwise let a check fail the gate silently, the shape the registry exists to forbid.
if (emitted.size !== CHECKS.length)
  throw new Error(`project_corpus.ts CHECKS: ${CHECKS.length - emitted.size} check(s) never printed — an unemitted group`);
w();
w(`ℹ️  C. SUPPORT SEAM (${seamC.length}) — a construct with a ✅ cover entry yet marked not-supported by the`);
w(`   matrix (a directional mismatch — covered in one context, unsupported in another). Reported, not fatal;`);
w(`   resolve case-by-case: fix the example if it's a degenerate form (the construct really works), or keep`);
w(`   ➖ + a note and drop the cover if it's a genuine gap (don't relabel it ✅ by editing the example):`);
for (const s of seamC.sort((a, b) => (a.id < b.id ? -1 : 1))) w(`   - ${s.id}  (matrix: ${s.status})`);

// Informational, like C above and for the same reason: it bears no verdict, so it is deliberately NOT
// in CHECKS (the registry's invariant is that every member decides the gate). A hard check here would
// need a ~146-entry exemption ledger encoding an editorial judgement — `containment/array-element.toml`
// omits trivial primitive-as-element cells ON PURPOSE ("only the structurally interesting (composite /
// choice / unwrap / group) cells are recorded") — which is worse than the gap it would close. The real
// enforcement is the rendered grid plus the `coverage_md_diff` gate: a new fixture that exercises a new
// cell changes COVERAGE.md and the fast tier goes red until someone regenerates and reads the diff.
w();
w(`ℹ️  ROLE × FEATURE GRID (${gridExercisedUnmodelled.length}) — cells the corpus EXERCISES that the containment`);
w(`   relation models nothing for. Not a failure: many are trivial primitive-in-role cells the relation omits`);
w(`   by design. It IS the list to read before assuming a nesting is covered — for each, either author a`);
w(`   containment row (\`cddl-matrix/containment/<role>.toml\`) or accept the omission as trivial. Rendered as`);
w(`   \`${GRID.exercised}\` in COVERAGE.md § "Role × feature containment grid":`);
for (const k of gridExercisedUnmodelled) w(`   - ${k}  (${floorCells.get(k)!.length} fixture${floorCells.get(k)!.length === 1 ? "" : "s"})`);
w(`   (${gridExercisedModelled.length} exercised cells ARE modelled; ${gridModelledUnexercised.length} modelled cells the corpus does not exercise.)`);

// ============================================================================================
// RENDER — join the validated overlay onto the matrix and write the COVERAGE doc (golden_hex shape).
// Sections are derived: profile (spec-first) -> production (alpha) -> id (alpha). No authored layout.
// ============================================================================================
const coverById = new Map(featureCovers.map(c => [c.id, c]));   // feature-axis covers drive the ✅ mark
const noteById = new Map(notes.map(n => [n.id, n]));
const seamById = new Map(seamC.map(s => [s.id, s.status]));     // seam C is report-only: annotate the row so the doc doesn't render an unqualified ✅
const MARK = { covered: "✅", untested: "➕", unsupported: "➖", partial: "⚠️" } as const;
const shortProbe = (id: string) => evidenceById.get(id)?.replace(/^probe: /, "").split(";")[0] ?? "";
const profileCaveat = (id: string) => {
  const preserve = emissionById.get(id)?.preserve;
  if (preserve?.status !== "unsupported") return "";
  return `; --preserve-encodings unsupported (${(preserve.evidence ?? "").replace(/^probe \(emission=preserve\): /, "")})`;
};
const anchor = (n: Note) => n.code_anchor ? `  [\`${n.code_anchor}\`]` : "";
const roleTail = (r: string) => r.replace(/^role\./, "");

// The per-feature verdict (note-first: a ⚠️/➖ note is the more informative verdict; the validator forbids
// an `unsupported` note coexisting with a FEATURE-AXIS cover, so this can't contradict the matrix).
const unexplained: string[] = [];   // unsupported feature with no rationale note (overlay TODO; surfaced)
function featureVerdict(id: string, track: boolean): { mark: string; ev: string } {
  const n = noteById.get(id);
  if (n?.status === "partial") return { mark: MARK.partial, ev: n.reason + anchor(n) };
  if (n?.status === "unsupported") return { mark: MARK.unsupported, ev: n.reason + anchor(n) };
  const cov = coverById.get(id);
  if (cov) {
    const seam = seamById.get(id);
    const seamNote = seam ? ` — ⚠️ SUPPORT SEAM: the matrix marks this \`${seam}\` (covered in one context, unsupported in another)` : "";
    return { mark: MARK.covered, ev: `\`${cov.fixture}\`` + (cov.note ? ` — ${cov.note}` : "") + seamNote };
  }
  const st = supportById.get(id);
  if (st === "supported") {
    // surface the compile-gate exemption clause (integration-tested) instead of a bare "exit 0"
    const ext = /standalone-compile N\/A \(([^)]+)\)/.exec(evidenceById.get(id) ?? "");
    return { mark: MARK.untested, ev: (ext ? `supported; ${ext[1]}` : `supported, no corpus fixture (${shortProbe(id)})`) + profileCaveat(id) };
  }
  if (st === "out_of_profile") return { mark: MARK.unsupported, ev: `out of profile — ${shortProbe(id)}` };
  if (track) unexplained.push(id);   // control ops aren't tracked: 28/37 unsupported is expected, not an overlay gap
  return { mark: MARK.unsupported, ev: `${shortProbe(id)}${track ? " — ⚠️ no rationale note yet (overlay gap)" : ""}` };
}

// markFeature = the per-feature verdict PLUS any per-cell (role × feature) coverage. A construct
// can be ➖ as a standalone type yet ✅ in a member/choice role — appended as "also ✅ @role" so the
// context axis is visible without overloading the single per-feature mark.
function markFeature(id: string, track = true): { mark: string; ev: string } {
  const base = featureVerdict(id, track);
  const cells = cellCoversById.get(id);
  if (!cells?.length) return base;
  const roles = cells.map(c => `✅ @${roleTail(c.role)} (\`${c.fixture}\`${c.note ? `: ${c.note}` : ""})`).join("; ");
  return { mark: base.mark, ev: `${base.ev} — also ${roles}` };
}

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
o("> either; CI fails on overlay drift: a cover whose fixture stops exercising its construct, a note that");
o("> contradicts the matrix support verdict, or a stale id/fixture/anchor. A cover on a construct the");
o("> matrix marks not-supported is a SUPPORT SEAM — annotated on the row, reported by the validator, not");
o("> (yet) fatal.");
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
o("- **Compile + execution gate:** `integration_tests::feature_corpus_compiles` `cargo check`s every corpus");
o("  file under all three profiles, so a ✅ entry must produce **compiling** Rust under *all* of them —");
o("  and under the default profile it also generates with `--emit-tests` and `cargo test`s the crate, so");
o("  every constructible corpus type must **round-trip byte-identically** (plus bounded-reject checks),");
o("  not just compile. The harness's `COMPILE_SKIP` list is currently empty: fixtures that reference");
o("  user-supplied code are seeded through shared templates and compile-exercised here too. A future");
o("  whole-fixture blocker that no seeded definition can answer must be named explicitly in that list.");
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

// --- role × feature containment grid: the whole-corpus AST role floor joined onto the containment
//     relation. Placed between the control-op table and the findings so the doc reads
//     features -> control ops -> containment -> findings. ---
o("## Role × feature containment grid");
o();
o("> Every construct the corpus exercises in ≥1 role, or the containment relation models in ≥1 role.");
o("> Columns are the `role` axis in **grammar order** (`cddl-matrix/roles.toml`, mirrored into");
o("> `matrix.json` `roles`) — top-level outward through the nesting positions. That order is derived, not");
o("> alphabetical; do not \"fix\" it to a sort.");
o();
o("| mark | meaning |");
o("|------|---------|");
o(`| ${GRID.supported} | the matrix models this cell and every probed shape in it is **supported** |`);
o(`| ${GRID.unsupported} | the matrix models this cell and every probed shape in it is **not supported** |`);
o(`| ${GRID.mixed} | the matrix models this cell and the probed shapes **disagree** — a support boundary *inside* the cell |`);
o(`| ${GRID.ungrounded} | the matrix models this cell but a spec-allowed row has **no support verdict yet** (awaiting a \`verify.ts\` grounding run) |`);
o(`| ${GRID.illegal} | the matrix models this cell only as **spec-disallowed** — the grammar forbids the nesting, so it is never support-probed |`);
o(`| ${GRID.exercised} | **the corpus exercises this cell and the matrix models nothing here** — no row, so no verdict |`);
o("| _(blank)_ | neither modelled nor exercised |");
o();
o("The grid's denominator is \"cells the matrix models, plus cells this corpus exercises\". A blank cell is");
o("**not** a claim that the nesting is illegal or unsupported — it is a claim that nothing here has an");
o("opinion about it.");
o();
o("**Do not read a `·` next to a `➖` sibling as a contradiction, and do not cross-check the two axes.**");
o("The floor is FEATURE-granular (\"an array appears in array-element role\"); a containment row is");
o("SHAPE-granular (`contain.array-element.type2.array`'s example is `a = [[int]]`, an *anonymous inline*");
o("array, which is `unsupported`). The corpus exercises `type2.array` as an array element in 5 fixtures and");
o("every corpus fixture compiles under all three profiles (`integration_tests::feature_corpus_compiles`) —");
o("because those fixtures use a *named rule reference*, a different shape. A cell being exercised by the");
o("corpus and marked unsupported by the matrix is therefore two different shapes, never a contradiction.");
o();
{
  const roles = matrix.roles;
  o(`| construct | ${roles.map(r => `${r.title} \`${r.id.replace(/^role\./, "")}\``).join(" | ")} |`);
  o(`|---|${roles.map(() => "---").join("|")}|`);
  for (const id of gridFeatureIds)
    o(`| \`${id}\` | ${roles.map(r => gridMark(cellKeyOf(id, r.id))).join(" | ")} |`);
  o();
  o(`- Modelled \`(role × feature)\` cells: **${modelledCells.size}** (over ${matrix.containment.length} shape-granular containment rows).`);
  o(`- Exercised by the corpus **and** modelled: **${gridExercisedModelled.length}**.`);
  o(`- Exercised by the corpus, modelled by **nothing**: **${gridExercisedUnmodelled.length}** (the \`${GRID.exercised}\` cells).`);
  o(`- Modelled but not exercised by any corpus fixture: **${gridModelledUnexercised.length}**.`);
  o();
}

// --- doc-level findings (narrative) ---
o("## Notable findings");
o();
findings.forEach((f, i) => o(`${i + 1}. ${f.text}`));
o();

// --- summary + honest disclosure of what's deferred ---
o("## Summary");
o();
const feC = tally[MARK.covered], feP = tally[MARK.untested], feN = tally[MARK.unsupported], feW = tally[MARK.partial];
const ctC = ctlTally[MARK.covered], ctP = ctlTally[MARK.untested], ctN = ctlTally[MARK.unsupported], ctW = ctlTally[MARK.partial];
o(`- Features: **${matrix.features.length}** — ✅ ${feC} covered · ➕ ${feP} supported-untested · ⚠️ ${feW} partial · ➖ ${feN} not supported`);
o(`- Control operators: **${matrix.control_operators.length}** — ✅ ${ctC} covered · ➕ ${ctP} supported-untested${ctW ? ` · ⚠️ ${ctW} partial` : ""} · ➖ ${ctN} not supported (cddl-codegen implements ${ctC + ctP} of ${matrix.control_operators.length})`);
o(`- Corpus fixtures: ${files.length}`);
o();
o(`**Per-cell coverage (role × feature).** Where a construct's support *differs by role*,`);
o("coverage is keyed on the (role × feature) cell, derived from a real `cddl`-crate AST walk");
o("(`cddl-matrix/examples/ast_roles.rs`) and cross-checked against the matrix's per-cell support verdict — so a");
o("➖ standalone type still surfaces its supported member/choice role (e.g. `prelude.null` ➖ as a top-level");
o(`type, ✅ as a choice-member). **${cellCovers.length} such cells** are mapped (appended as "also ✅ @role" on the`);
o("rows above); constructs whose support doesn't vary by role stay feature-axis (the role is unremarkably");
o("top-level). The full role × feature picture — every construct the corpus exercises or the containment");
o("relation models, in every role — is rendered above in **§ Role × feature containment grid**, joined from");
o("the whole-corpus floor; that is where a cell nothing models shows up.");
if (unexplained.length) {
  o();
  o(`**Overlay rationale gap (disclosed).** ${unexplained.length} unsupported feature(s) render ➖ with no rationale note —`);
  o(`add a \`[[note]]\` (reason + code_anchor) to the overlay: ${unexplained.sort().map(x => `\`${x}\``).join(", ")}.`);
}
o();

// The committed doc is written only when the gate passes: a hard-failing overlay must not rewrite
// COVERAGE.md with rows rendered from the very claims that just failed validation (CI would catch
// the nonzero exit either way; this protects the LOCAL working tree from a poisoned regeneration).
const hardFail = CHECKS.some(c => c.hard && c.items.length > 0);
w();
if (hardFail) {
  w(`SKIPPED writing ${OUT.replace(`${HERE}/../`, "")} (hard failure — the committed doc is left untouched)`);
  w();
  w("RESULT: FAIL (overlay drift — see above)");
  process.exit(1);
}
await Bun.write(OUT, L.join("\n"));
w(`wrote ${OUT.replace(`${HERE}/../`, "")}`);
w(`  features: ✅ ${feC}  ➕ ${feP}  ⚠️ ${feW}  ➖ ${feN}; control-ops: ✅ ${ctC} ➕ ${ctP} ➖ ${ctN}; per-cell covers: ${cellCovers.length}; unexplained ➖: ${unexplained.length}`);
w();
w("RESULT: PASS (editorial mapping holds mechanically)");
process.exit(0);
