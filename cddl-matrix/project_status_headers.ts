#!/usr/bin/env bun
/**
 * Status-header count projection — PURE FILE READS, no cargo, no oracles.
 *
 * The gate-green "status" paragraphs in `cddl-matrix/ROADMAP.md` and `cddl-matrix/README.md`, plus the
 * `#[ignore]`d-gate roll-call in `tests/README.md`, carry hand-maintained counts (features per profile,
 * containment cells, annotations, IANA ops, emission divergences, constraint vectors × enforce-green
 * rows, registered manual gates) that silently drift on every delivery — "3 divergences" survived two
 * doc-updating commits after the real count became 5, caught only by a manual audit. Every one of those
 * numbers is already derivable from committed artifacts, so this is the north star's own class of
 * problem: project the counts into marker-delimited HTML-comment spans with a `--check` drift gate (the
 * `query_q1_gaps.ts` → `current_capacities.mdx` pattern, local tier). Countable prose OUTSIDE the markers
 * stays hand-owned — the gate catches exactly the numbers it generates, no more.
 *
 * Before adding a NEW span, ask whether the count IS the claim (a status header's scale/completeness
 * assertion, where the number is the point). An INCIDENTAL count is better deleted (redundant beside
 * its own enumeration) or reworded into a drift-proof policy claim ("SKIP is expected to hold only X")
 * than projected — projection retires drift, not churn. Frozen historical counts and structural counts
 * ("three tiers") never drift and never need spans.
 *
 * Deriving sources (all committed, all read-only):
 *   - `matrix.json` — features (+ per-profile split), containment, control operators, cddl-codegen
 *     annotations (+ the `emission.<profile>.status` divergence axis).
 *   - `tests/decode_conformance/catalog.toml` — `class="constraint"` reject vectors and the enforce-green
 *     row set that carries them (same read as `query_q4_directional.ts`).
 *   - `check.ts`'s gate REGISTRY, IMPORTED (never regex-parsed) — the manual `#[ignore]`d gate roll-call
 *     in `tests/README.md` duplicates it, and the registry is itself self-checked against
 *     `cargo test -- --ignored --list` by check.ts's meta-checks, so it is the honest deriving source.
 *
 * Each span is `<!-- gen:sh:<id> -->generated text<!-- /gen:sh:<id> -->` (invisible when the markdown
 * renders), INLINE within the prose; the generated content is single-line (the surrounding hand prose is
 * re-wrapped by hand once so lines stay readable). Span ids are unique per file.
 *
 * Run from cddl-matrix/:
 *   bun run project_status_headers.ts           -> the readable count report to stdout
 *   bun run project_status_headers.ts --write    -> rewrite every span's inner content in place
 *   bun run project_status_headers.ts --check     -> drift (byte-compare each span) + marker-count +
 *                                                     invariants + vacuity floors; exit nonzero on any problem
 */
import { readFileSync, writeFileSync } from "node:fs";
import { REGISTRY } from "../check.ts";

const HERE = import.meta.dir;

// Files the spans live in (paths relative to cddl-matrix/).
const ROADMAP_REL = "ROADMAP.md";
const README_REL = "README.md";
const TESTS_README_REL = "../tests/README.md";
const CATALOG_REL = "../tests/decode_conformance/catalog.toml";
const pathOf = (rel: string) => `${HERE}/${rel}`;

// --- matrix.json ----------------------------------------------------------------------------------
interface Annotation { id: string; status: string; emission?: Record<string, { status?: string }> }
interface FeatureRow { id: string; profile?: string }
interface UnivRow { id: string }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: FeatureRow[];
  containment: UnivRow[];
  control_operators: UnivRow[];
}
const matrix = JSON.parse(readFileSync(pathOf("matrix.json"), "utf8")) as MatrixJson;
const annotations = matrix.annotations.cddl_codegen;
const statusById = new Map(annotations.map(a => [a.id, a.status]));

const featuresTotal = matrix.features.length;
const containmentCells = matrix.containment.length;
const annotationsTotal = annotations.length;
const controlOps = matrix.control_operators.length;

// Per-profile feature split, rendered in a FIXED order so the prose is deterministic. RFC labels are
// bare; the vendor profile is backticked + " vendor profile" (matches the hand prose it replaces).
const PROFILE_ORDER = ["RFC8610", "RFC9682", "CDDL_CODEGEN"] as const;
type Profile = (typeof PROFILE_ORDER)[number];
const featuresByProfile = new Map<string, number>();
for (const f of matrix.features) {
  const p = f.profile ?? "NONE";
  featuresByProfile.set(p, (featuresByProfile.get(p) ?? 0) + 1);
}
function profileLabel(p: Profile, n: number): string {
  return p === "CDDL_CODEGEN" ? `${n} \`CDDL_CODEGEN\` vendor profile` : `${n} ${p}`;
}
function renderProfileSplit(): string {
  return PROFILE_ORDER.map(p => profileLabel(p, featuresByProfile.get(p) ?? 0)).join(" + ");
}

// Emission divergences: annotations carrying an `emission.<profile>.status = "unsupported"`. The render
// ADAPTS — all-`preserve`-side today, but a json-side divergence would render its own breakdown, never
// hard-fail. (The emission axis itself is validated elsewhere; here we only count + label.)
interface Divergence { id: string; profile: string }
const divergences: Divergence[] = [];
for (const a of annotations) {
  if (!a.emission) continue;
  for (const [profile, v] of Object.entries(a.emission)) {
    if (v && v.status === "unsupported") divergences.push({ id: a.id, profile });
  }
}
const divergenceProfiles = new Map<string, number>();
for (const d of divergences) divergenceProfiles.set(d.profile, (divergenceProfiles.get(d.profile) ?? 0) + 1);
function renderDivergenceClause(): string {
  const profiles = [...divergenceProfiles.keys()];
  if (divergences.length > 0 && profiles.length === 1 && profiles[0] === "preserve") return "all `preserve`-side";
  // Mixed / non-preserve: a deterministic per-profile breakdown.
  return [...divergenceProfiles.entries()]
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([p, n]) => `${n} \`${p}\`-side`)
    .join(", ");
}

// --- catalog.toml: class="constraint" reject vectors + the enforce-green rows carrying them -----------
interface CatVector { expect?: unknown; class?: unknown }
interface CatRow { id?: unknown; vector?: CatVector[] }
const catalog = Bun.TOML.parse(readFileSync(pathOf(CATALOG_REL), "utf8")) as { row?: CatRow[] };
let constraintVectors = 0;
const enforceGreenRows = new Set<string>();
for (const r of catalog.row ?? []) {
  const id = typeof r.id === "string" ? r.id : undefined;
  for (const v of r.vector ?? []) {
    if (v.expect === "reject" && v.class === "constraint") {
      constraintVectors++;
      if (id !== undefined) enforceGreenRows.add(id);
    }
  }
}
const enforceGreenRowCount = enforceGreenRows.size;

// --- check.ts registry: the manual (non-stub) #[ignore]d gate roll-call ---------------------------
const ignoredGates = REGISTRY.filter(g => g.kind !== "stub" && g.ignoredTest).map(g => g.ignoredTest!);

// English count word for 0–12, numeral above (matches the hand prose's "the five gates" spelling).
const WORDS = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve"];
function countWord(n: number): string { return n >= 0 && n <= 12 ? WORDS[n]! : String(n); }

// --- the spans ------------------------------------------------------------------------------------
interface Span { file: string; id: string; render: () => string }
const SPANS: Span[] = [
  // 1. cddl-matrix/ROADMAP.md — the "Status: gate-green." paragraph.
  { file: ROADMAP_REL, id: "roadmap-counts", render: () =>
      `${featuresTotal} features (${renderProfileSplit()}), ${containmentCells} containment cells, and ${annotationsTotal} cddl-codegen annotations` },
  { file: ROADMAP_REL, id: "roadmap-ops", render: () => `all ${controlOps} IANA ops probed` },
  { file: ROADMAP_REL, id: "roadmap-emission", render: () => `${divergences.length} divergences, ${renderDivergenceClause()}` },
  { file: ROADMAP_REL, id: "roadmap-constraint", render: () =>
      `${constraintVectors} \`class="constraint"\` enforcement reject vectors over ${enforceGreenRowCount} enforce-green rows` },

  // 2. cddl-matrix/README.md — the entry-points blockquote.
  { file: README_REL, id: "readme-counts", render: () => `${featuresTotal} features and ${containmentCells} containment cells` },
  { file: README_REL, id: "readme-annotations", render: () => `${annotationsTotal} cddl-codegen support annotations` },
  { file: README_REL, id: "readme-ops", render: () => `all ${controlOps} IANA ops probed` },

  // 3. tests/README.md — the manual `#[ignore]`d-gate roll-call (count word + backticked name list).
  { file: TESTS_README_REL, id: "tests-ignored-gates", render: () =>
      `the ${countWord(ignoredGates.length)} \`#[ignore]\`d gates ${ignoredGates.map(g => `\`${g}\``).join(" / ")}` },
];

// --- span mechanics -------------------------------------------------------------------------------
const openMarker = (id: string) => `<!-- gen:sh:${id} -->`;
const closeMarker = (id: string) => `<!-- /gen:sh:${id} -->`;

function markerCount(doc: string, marker: string): number {
  let n = 0, i = 0;
  for (;;) { const j = doc.indexOf(marker, i); if (j === -1) break; n++; i = j + marker.length; }
  return n;
}

// Locate a span's inner content (between its open and close markers). Returns null if either marker is
// missing or out of order.
function locate(doc: string, id: string): { innerStart: number; innerEnd: number } | null {
  const open = doc.indexOf(openMarker(id));
  if (open === -1) return null;
  const innerStart = open + openMarker(id).length;
  const close = doc.indexOf(closeMarker(id), innerStart);
  if (close === -1) return null;
  return { innerStart, innerEnd: close };
}

// --- modes ----------------------------------------------------------------------------------------
const argv = process.argv.slice(2);
const isWrite = argv.includes("--write");
const isCheck = argv.includes("--check");

const filesInOrder = [...new Set(SPANS.map(s => s.file))];

if (isWrite) {
  let touched = 0;
  for (const file of filesInOrder) {
    let doc = readFileSync(pathOf(file), "utf8");
    for (const s of SPANS.filter(sp => sp.file === file)) {
      const loc = locate(doc, s.id);
      if (loc === null) {
        console.error(`FAIL ${file}: span '${s.id}' has no <!-- gen:sh:${s.id} --> … <!-- /gen:sh:${s.id} --> markers — hand-place them once around the phrase.`);
        process.exit(1);
      }
      doc = doc.slice(0, loc.innerStart) + s.render() + doc.slice(loc.innerEnd);
    }
    writeFileSync(pathOf(file), doc);
    touched++;
  }
  console.log(`status-headers: wrote ${SPANS.length} generated span(s) across ${touched} file(s).`);
  process.exit(0);
}

if (isCheck) {
  const problems: string[] = [];

  // Invariants over the derivation.
  const profileSum = PROFILE_ORDER.reduce((acc, p) => acc + (featuresByProfile.get(p) ?? 0), 0);
  if (profileSum !== featuresTotal)
    problems.push(`per-profile feature counts sum to ${profileSum} but the features total is ${featuresTotal} — an unmodelled profile leaked in (${[...featuresByProfile.keys()].join(", ")})`);
  for (const p of featuresByProfile.keys())
    if (!PROFILE_ORDER.includes(p as Profile))
      problems.push(`feature profile '${p}' is not in the fixed render order (${PROFILE_ORDER.join(", ")}) — extend PROFILE_ORDER or fix the overlay`);
  for (const id of enforceGreenRows) {
    const st = statusById.get(id);
    if (st !== "supported")
      problems.push(`enforce-green row \`${id}\` carries a class="constraint" vector but its cddl-codegen annotation is \`${st ?? "absent"}\` (must be supported — the enforce-green label would be dishonest)`);
  }

  // Vacuity floors (guard against an empty/broken read silently generating "0 …").
  if (annotationsTotal < 80) problems.push(`only ${annotationsTotal} annotation rows read (expected >= 80) — the matrix read looks broken/empty`);
  if (featuresTotal < 90) problems.push(`only ${featuresTotal} feature rows read (expected >= 90) — the matrix read looks broken/empty`);
  if (containmentCells < 60) problems.push(`only ${containmentCells} containment cells read (expected >= 60) — the matrix read looks broken/empty`);
  if (controlOps < 30) problems.push(`only ${controlOps} control operators read (expected >= 30) — the matrix read looks broken/empty`);
  if (constraintVectors < 1) problems.push(`no class="constraint" reject vectors found (expected >= 1) — the catalog read looks broken`);
  if (enforceGreenRowCount < 1) problems.push(`no enforce-green rows found (expected >= 1) — the catalog read looks broken`);
  if (ignoredGates.length < 1) problems.push(`no manual #[ignore]d gates found in the registry (expected >= 1) — the check.ts import looks broken`);

  // Marker counts + byte-exact drift per span.
  for (const file of filesInOrder) {
    const doc = readFileSync(pathOf(file), "utf8");
    for (const s of SPANS.filter(sp => sp.file === file)) {
      const opens = markerCount(doc, openMarker(s.id));
      const closes = markerCount(doc, closeMarker(s.id));
      if (opens !== 1) problems.push(`${file}: open marker for span '${s.id}' appears ${opens} time(s), expected exactly 1`);
      if (closes !== 1) problems.push(`${file}: close marker for span '${s.id}' appears ${closes} time(s), expected exactly 1`);
      if (opens !== 1 || closes !== 1) continue;
      const loc = locate(doc, s.id)!;
      const inner = doc.slice(loc.innerStart, loc.innerEnd);
      const want = s.render();
      if (inner !== want)
        problems.push(`${file}: span '${s.id}' is stale — has ${JSON.stringify(inner)}, expected ${JSON.stringify(want)} (run \`bun run project_status_headers.ts --write\`)`);
    }
  }

  if (problems.length) {
    console.log(`status-header count gate: ${problems.length} problem(s)`);
    for (const p of problems) console.log(`  FAIL ${p}`);
    process.exit(1);
  }
  console.log(
    `status-header count gate OK — ${featuresTotal} features (${renderProfileSplit()}) · ${containmentCells} containment · ` +
      `${annotationsTotal} annotations · ${controlOps} IANA ops · ${divergences.length} emission divergence(s) (${renderDivergenceClause()}) · ` +
      `${constraintVectors} constraint vectors over ${enforceGreenRowCount} enforce-green rows · ` +
      `${ignoredGates.length} manual #[ignore]d gates · all spans in sync`,
  );
  process.exit(0);
}

// --- default: readable report ---------------------------------------------------------------------
console.log(`\nstatus-header counts (derived from matrix.json + catalog.toml + check.ts registry)\n`);
console.log(`  features:            ${featuresTotal}  (${renderProfileSplit()})`);
console.log(`  containment cells:   ${containmentCells}`);
console.log(`  cddl-codegen annos:  ${annotationsTotal}`);
console.log(`  IANA control ops:    ${controlOps}`);
console.log(`  emission divergences:${divergences.length}  (${renderDivergenceClause()})`);
for (const d of divergences) console.log(`      - ${d.id}  [${d.profile}]`);
console.log(`  constraint vectors:  ${constraintVectors}  over ${enforceGreenRowCount} enforce-green row(s)`);
console.log(`      ${[...enforceGreenRows].sort().join(", ")}`);
console.log(`  manual #[ignore]d gates: ${ignoredGates.length}  (${countWord(ignoredGates.length)})`);
for (const g of ignoredGates) console.log(`      - ${g}`);
console.log(`\n(run with --write to regenerate the spans, --check for the drift gate)\n`);
