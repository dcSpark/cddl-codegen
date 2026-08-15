#!/usr/bin/env bun
/**
 * Q1 support-gap query (QUERIES.md Q1) — PURE FILE READS, no cargo, no oracles.
 *
 * Answers QUERIES.md Q1: "every construct cddl-codegen does NOT support but that is IN its target
 * CDDL profile." It reads exactly one committed file — `matrix.json` — and derives; it never probes.
 * The whole point of Q1 (finding F1) is the profile split: a construct cddl-codegen rejects only
 * because it post-dates the tool's target profile (status `out_of_profile`) is NOT a support gap, so
 * those rows are bucketed separately from the actionable in-profile gaps.
 *
 * What it reads from `matrix.json`:
 *   - `annotations.cddl_codegen` — the execution-gated verdict per id (`status` ∈
 *     {supported, unsupported, uncertain, out_of_profile} + machine-generated `evidence`).
 *   - `features` / `containment` / `control_operators` — the construct universe (per-id `example`,
 *     human `title`/`name`, and for containment the `role`/`feature` the cell sits at), used to bucket
 *     each annotation by axis and to render its example + human note.
 *
 * An in-profile GAP is any annotation whose status is `unsupported` or `uncertain` (NOT `supported`,
 * NOT `out_of_profile`). Gaps split three ways by which universe array owns the id:
 *   - feature gaps        — a whole construct cddl-codegen cannot emit at all;
 *   - control-op gaps     — an IANA control operator with no codegen support;
 *   - containment-cell gaps — a (role × feature) cell. The load-bearing subset is the CONTEXTUAL gap:
 *     a feature that IS supported top-level but is unsupported in some role ("supported here, not
 *     there" — e.g. an inline anonymous map as an array element). Those are grouped by feature. Cells
 *     whose feature is ALSO unsupported top-level are already covered by the feature gap, so they are
 *     not re-reported as contextual gaps.
 *
 * The evidence → human note vocabulary mirrors query_q4_directional.ts's approach (match verify.ts's
 * stable phrasings): "panics generation" / "generates but does not compile" / "rejected gracefully".
 *
 * Run from cddl-matrix/:
 *   bun run query_q1_gaps.ts            -> the readable gap report grouped by axis + supported summary
 *   bun run query_q1_gaps.ts prelude    -> only rows whose id contains "prelude" (Q1's "for tool X, filtered")
 *   bun run query_q1_gaps.ts --write     -> regenerate the marker-delimited "## Limitations" block in
 *                                            docs/docs/current_capacities.mdx
 *   bun run query_q1_gaps.ts --check     -> drift (regenerate in memory + byte-compare the block) +
 *                                            invariants + vacuity floor; exit nonzero on any problem
 */
import { readFileSync, writeFileSync } from "node:fs";

const HERE = import.meta.dir;
const MDX_REL = "docs/docs/current_capacities.mdx";
const MDX_PATH = `${HERE}/../${MDX_REL}`;
const BEGIN = "{/* BEGIN GENERATED q1-limitations — regenerate with: cd cddl-matrix && bun run query_q1_gaps.ts --write */}";
const END = "{/* END GENERATED q1-limitations */}";

// --- matrix.json ----------------------------------------------------------------------------------
interface Annotation { id: string; status: string; evidence: string }
interface FeatureRow { id: string; title?: string; example?: string; profile?: string }
interface ContainRow { id: string; role?: string; feature?: string; example?: string }
interface CtlRow { id: string; name?: string; example?: string }
interface MatrixJson {
  annotations: { cddl_codegen: Annotation[] };
  features: FeatureRow[];
  containment: ContainRow[];
  control_operators: CtlRow[];
}
const matrix = JSON.parse(readFileSync(`${HERE}/matrix.json`, "utf8")) as MatrixJson;
const annotations = matrix.annotations.cddl_codegen;
const statusById = new Map(annotations.map(a => [a.id, a.status]));

const featureById = new Map(matrix.features.map(r => [r.id, r]));
const containById = new Map(matrix.containment.map(r => [r.id, r]));
const ctlById = new Map(matrix.control_operators.map(r => [r.id, r]));

type Axis = "feature" | "containment-cell" | "control-op";
function axisOf(id: string): Axis | undefined {
  if (featureById.has(id)) return "feature";
  if (containById.has(id)) return "containment-cell";
  if (ctlById.has(id)) return "control-op";
  return undefined;
}

const GAP_STATUSES = new Set(["unsupported", "uncertain"]);
function isGap(status: string): boolean { return GAP_STATUSES.has(status); }

// --- derivations ----------------------------------------------------------------------------------
// Collapse a possibly-multi-line CDDL example into one deterministic inline cell.
function inlineExample(ex: string | undefined): string {
  if (!ex) return "";
  return ex.trim().replace(/[ \t]*\n[ \t]*/g, "; ").replace(/  +/g, " ").trim();
}

// Escape angle brackets in PROSE text (not code spans): a bare `<T>` in a construct name would be
// parsed as JSX by MDX and break the docs build. Examples always render inside backtick code spans,
// so they are left verbatim.
function escapeProse(s: string): string {
  return s.replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// Evidence → short human note. The machine-generated evidence vocabulary is stable (verify.ts);
// match its exact phrasings, same approach as query_q4_directional.ts.
function deriveNote(evidence: string): string {
  const ev = evidence;
  if (ev.includes("generates but does not compile")) return "generates but does not compile";
  if (ev.includes("rejected at parse/lex")) return "rejected gracefully at parse";
  if (ev.includes("panic") || ev.includes("exit 101")) return "panics generation";
  if (ev.includes("exit 1")) return "rejected gracefully";
  return "unsupported";
}

interface Gap { id: string; axis: Axis; status: string; name: string; example: string; note: string }

function nameOf(id: string, axis: Axis): string {
  if (axis === "feature") return featureById.get(id)?.title ?? id;
  if (axis === "control-op") return ctlById.get(id)?.name ?? id;
  return id;
}
function exampleOf(id: string, axis: Axis): string {
  if (axis === "feature") return inlineExample(featureById.get(id)?.example);
  if (axis === "control-op") return inlineExample(ctlById.get(id)?.example);
  return inlineExample(containById.get(id)?.example);
}

const gaps: Gap[] = annotations
  .filter(a => isGap(a.status))
  .map(a => {
    const axis = axisOf(a.id) ?? "feature";
    return { id: a.id, axis, status: a.status, name: nameOf(a.id, axis), example: exampleOf(a.id, axis), note: deriveNote(a.evidence) };
  })
  .sort((x, y) => x.id.localeCompare(y.id));

const featureGaps = gaps.filter(g => g.axis === "feature");
const ctlGaps = gaps.filter(g => g.axis === "control-op");

// Contextual gaps: containment cells that are unsupported WHERE the feature is supported top-level.
// Cells are GROUPED by (feature, role): a role reached by several distinct unsupported spellings is
// one entry carrying the cell `count` and that role's OWN example, so no role name repeats and no
// row shows a neighbouring role's example. The example is the role's first cell in the id-sorted
// `gaps` order (deterministic — the `--check` byte-compare pins it).
interface ContextualRole { role: string; count: number; example: string }
interface Contextual { feature: string; roles: ContextualRole[] }
const contextualByFeature = new Map<string, Contextual>();
for (const g of gaps) {
  if (g.axis !== "containment-cell") continue;
  const cell = containById.get(g.id)!;
  const feat = cell.feature ?? "";
  if (statusById.get(feat) !== "supported") continue; // feature-broad gap, already reported above
  const roleShort = (cell.role ?? "").replace(/^role\./, "");
  let ctx = contextualByFeature.get(feat);
  if (!ctx) {
    ctx = { feature: feat, roles: [] };
    contextualByFeature.set(feat, ctx);
  }
  let entry = ctx.roles.find(r => r.role === roleShort);
  if (!entry) {
    entry = { role: roleShort, count: 0, example: g.example };
    ctx.roles.push(entry);
  }
  entry.count++;
}
const contextual = [...contextualByFeature.values()].sort((a, b) => a.feature.localeCompare(b.feature));
for (const c of contextual) c.roles.sort((a, b) => a.role.localeCompare(b.role));

// Out-of-profile rows (bucketed separately — the whole point of Q1's F1 profile split).
const outOfProfile = annotations
  .filter(a => a.status === "out_of_profile")
  .map(a => {
    const axis = axisOf(a.id) ?? "feature";
    return { id: a.id, name: nameOf(a.id, axis), example: exampleOf(a.id, axis), profile: featureById.get(a.id)?.profile ?? "" };
  })
  .sort((x, y) => x.id.localeCompare(y.id));

// Supported (inverse) summary, per axis.
const supported = annotations.filter(a => a.status === "supported");
function countAxis(rows: Annotation[], axis: Axis): number { return rows.filter(a => axisOf(a.id) === axis).length; }
const supportedCtlNames = supported
  .filter(a => axisOf(a.id) === "control-op")
  .map(a => ctlById.get(a.id)?.name ?? a.id)
  .sort((a, b) => a.localeCompare(b));

// --- the generated "## Limitations" block --------------------------------------------------------
function renderBlock(): string {
  const L: string[] = [];
  L.push(BEGIN);
  L.push("");
  L.push("## Limitations");
  L.push("");
  L.push(
    "This section is generated from [`cddl-matrix/matrix.json`](https://github.com/dcSpark/cddl-codegen/tree/master/cddl-matrix) " +
    "by `cddl-matrix/query_q1_gaps.ts` (regenerate with `cd cddl-matrix && bun run query_q1_gaps.ts --write`). " +
    "It lists the constructs **in cddl-codegen's target CDDL profile (RFC 8610 + the IANA control-op " +
    "registry) that the generator does not yet support** — its actionable gaps. \"Supported\" means the " +
    "generated crate's emitted round-trip tests pass, not merely that it generates and compiles. " +
    "Constructs that post-date the target profile are listed separately at the end (they are not gaps).",
  );
  L.push("");

  // Unsupported features.
  L.push("### Unsupported constructs");
  L.push("");
  L.push("| Construct | CDDL example | Behavior |");
  L.push("|-----------|--------------|----------|");
  for (const g of featureGaps) L.push(`| ${escapeProse(g.name)} | \`${g.example}\` | ${g.note} |`);
  L.push("");

  // Control operators.
  L.push("### Control operators");
  L.push("");
  L.push(`Supported: ${supportedCtlNames.map(n => `\`${n}\``).join(", ")}.`);
  L.push("");
  L.push("Unsupported (in-profile):");
  L.push("");
  L.push(`${ctlGaps.map(g => `\`${g.name}\``).join(", ")}.`);
  L.push("");

  // Contextual gaps.
  L.push("### Contextual gaps (supported top-level, unsupported when nested)");
  L.push("");
  L.push(
    "These constructs work as their own rule but are unsupported in the listed nesting role — one " +
    "row per (construct, role). A role annotated with a shape count is reached by that many " +
    "distinct unsupported spellings; the Example column shows one of them. For the inline " +
    "anonymous composites the remedy is to name the composite: a fixed-field heterogeneous " +
    "`type2.map` or `type2.array` accepts a scoped `; @name` when it is the group entry's whole " +
    "member type (up to tag wrappers), while every other listed position needs an explicit rule; " +
    "for the key and occurrence rows the remedy depends on the spelling.",
  );
  L.push("");
  L.push("| Construct | Unsupported role | Example |");
  L.push("|-----------|------------------|---------|");
  for (const c of contextual) {
    for (const r of c.roles) {
      const role = r.count > 1 ? `${r.role} (${r.count} shapes)` : r.role;
      L.push(`| \`${c.feature}\` | ${role} | \`${r.example}\` |`);
    }
  }
  L.push("");

  // Out-of-profile.
  L.push("### Out of profile (not gaps)");
  L.push("");
  if (outOfProfile.length) {
    const items = outOfProfile.map(o => `${escapeProse(o.name)} (\`${o.example}\`, ${o.profile})`).join("; ");
    L.push(
      `The following construct post-dates cddl-codegen's target profile, so it is not counted as a ` +
      `support gap: ${items}.`,
    );
  } else {
    L.push("None — every modelled construct is within cddl-codegen's target profile.");
  }
  L.push("");
  L.push(END);
  return L.join("\n");
}

// --- --write / --check / report -------------------------------------------------------------------
const argv = process.argv.slice(2);
const isWrite = argv.includes("--write");
const isCheck = argv.includes("--check");
const positional = argv.filter(a => !a.startsWith("--"));

function extractBlock(doc: string): string | null {
  const b = doc.indexOf(BEGIN);
  const e = doc.indexOf(END);
  if (b === -1 || e === -1 || e < b) return null;
  return doc.slice(b, e + END.length);
}
function markerCount(doc: string, marker: string): number {
  let n = 0, i = 0;
  for (;;) { const j = doc.indexOf(marker, i); if (j === -1) break; n++; i = j + marker.length; }
  return n;
}

if (isWrite) {
  const doc = readFileSync(MDX_PATH, "utf8");
  const block = renderBlock();
  let next: string;
  const existing = extractBlock(doc);
  if (existing !== null) {
    next = doc.replace(existing, block);
  } else {
    next = doc.replace(/\s*$/, "") + "\n\n" + block + "\n";
  }
  writeFileSync(MDX_PATH, next);
  console.log(`Q1: wrote generated Limitations block to ${MDX_REL} (${featureGaps.length} feature gaps, ${ctlGaps.length} control-op gaps, ${contextual.length} contextual, ${outOfProfile.length} out-of-profile).`);
  process.exit(0);
}

if (isCheck) {
  const problems: string[] = [];

  // Invariants over the derivation.
  for (const g of gaps) {
    const st = statusById.get(g.id);
    if (st === undefined) problems.push(`\`${g.id}\`: gap row has no matching annotation`);
    else if (!isGap(st)) problems.push(`\`${g.id}\`: bucketed as a gap but its status is \`${st}\` (must be unsupported/uncertain)`);
    if (st === "out_of_profile") problems.push(`\`${g.id}\`: out_of_profile row leaked into the gap list`);
    if (axisOf(g.id) === undefined) problems.push(`\`${g.id}\`: gap row resolves to no universe axis`);
  }
  // No supported/out_of_profile id may appear in any gap bucket.
  for (const g of [...featureGaps, ...ctlGaps]) {
    const st = statusById.get(g.id);
    if (st === "supported" || st === "out_of_profile")
      problems.push(`\`${g.id}\`: appears in a gap table but is \`${st}\``);
  }
  // Contextual gap feature must itself be supported top-level (else it's a feature-broad gap).
  for (const c of contextual) {
    if (statusById.get(c.feature) !== "supported")
      problems.push(`contextual gap \`${c.feature}\`: feature is not supported top-level (\`${statusById.get(c.feature)}\`) — belongs in the feature gap table`);
  }

  // Drift: the doc's generated block must byte-match a fresh render.
  const doc = readFileSync(MDX_PATH, "utf8");
  if (markerCount(doc, BEGIN) !== 1) problems.push(`${MDX_REL}: BEGIN marker appears ${markerCount(doc, BEGIN)} time(s), expected exactly 1`);
  if (markerCount(doc, END) !== 1) problems.push(`${MDX_REL}: END marker appears ${markerCount(doc, END)} time(s), expected exactly 1`);
  const existing = extractBlock(doc);
  if (existing === null) {
    problems.push(`${MDX_REL}: no generated q1-limitations block found — run \`bun run query_q1_gaps.ts --write\``);
  } else if (existing !== renderBlock()) {
    problems.push(`${MDX_REL}: generated Limitations block is stale vs matrix.json — run \`bun run query_q1_gaps.ts --write\``);
  }

  // Vacuity floor.
  if (annotations.length < 80) problems.push(`only ${annotations.length} annotation rows read (expected >= 80) — the matrix read looks broken/empty`);
  if (featureGaps.length < 1) problems.push(`no in-profile feature gap found — the gap derivation looks broken (expected the top-level fixed-value types, etc.)`);
  const oopInData = annotations.some(a => a.status === "out_of_profile");
  if (oopInData && outOfProfile.length < 1) problems.push(`the data has out_of_profile annotations but none were bucketed as such — the profile split broke`);

  if (problems.length) {
    console.log(`Q1 support-gap gate: ${problems.length} problem(s)`);
    for (const p of problems) console.log(`  FAIL ${p}`);
    process.exit(1);
  }
  console.log(
    `Q1 support-gap gate OK — ${annotations.length} annotations · ` +
      `${featureGaps.length} feature gaps · ${ctlGaps.length} control-op gaps · ` +
      `${contextual.length} contextual (supported here, not there) · ` +
      `${outOfProfile.length} out-of-profile · generated block in sync`,
  );
  process.exit(0);
}

// --- default (and filtered) run: the readable gap report ------------------------------------------
const filter = positional[0]?.toLowerCase();
function match(id: string): boolean { return !filter || id.toLowerCase().includes(filter); }

console.log(`\nQ1 — cddl-codegen in-profile support gaps (constructs unsupported but in the target profile)`);
console.log(`(${gaps.length} gap(s) across ${annotations.length} modelled constructs${filter ? `, filtered by ${JSON.stringify(positional[0])}` : ""})\n`);

const shownFeat = featureGaps.filter(g => match(g.id));
if (shownFeat.length) {
  console.log(`### FEATURE gaps — ${shownFeat.length} construct(s)`);
  const w = Math.max(...shownFeat.map(g => g.id.length), 2);
  for (const g of shownFeat) console.log(`  ${g.id.padEnd(w)}  ${g.note.padEnd(30)} ${g.example}`);
  console.log("");
}

const shownCtl = ctlGaps.filter(g => match(g.id));
if (shownCtl.length) {
  console.log(`### CONTROL-OP gaps — ${shownCtl.length} operator(s)`);
  console.log(`  ${shownCtl.map(g => g.name).join(", ")}`);
  console.log("");
}

const shownCtx = contextual.filter(c => match(c.feature) || c.roles.some(r => match(`${c.feature}.${r.role}`)));
if (shownCtx.length) {
  console.log(`### CONTEXTUAL gaps — supported top-level, unsupported when nested (${shownCtx.length} feature(s))`);
  for (const c of shownCtx)
    console.log(`  ${c.feature.padEnd(24)}  roles: ${c.roles.map(r => (r.count > 1 ? `${r.role} (x${r.count})` : r.role)).join(", ")}`);
  console.log("");
}

if (!filter) {
  console.log(`### SUPPORTED summary (the inverse of the gap list)`);
  console.log(`  features:        ${countAxis(supported, "feature")} supported / ${featureGaps.length} gap`);
  console.log(`  containment cells:${countAxis(supported, "containment-cell")} supported / ${gaps.filter(g => g.axis === "containment-cell").length} gap`);
  console.log(`  control ops:     ${countAxis(supported, "control-op")} supported / ${ctlGaps.length} gap`);
  console.log(`  supported control ops: ${supportedCtlNames.join(", ")}`);
  console.log("");

  console.log(`### OUT OF PROFILE — not gaps (post-date the target profile)`);
  if (outOfProfile.length) for (const o of outOfProfile) console.log(`  ${o.id}  (${o.profile})  ${o.example}`);
  else console.log(`  none`);
  console.log("");
}
