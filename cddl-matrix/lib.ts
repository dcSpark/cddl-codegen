// Shared matrix-loading + serialization for build_matrix.ts / verify.ts.
// Paths resolve relative to this file's dir (cddl-matrix/).
import { readFileSync } from "node:fs";

export const ROOT = import.meta.dir;

// The authored-overlay schema. Only the fields the tooling reads are declared; the TOML rows carry
// more (title, desc, rfc, …) which pass through to matrix.json untouched.
export interface Feature {
  id: string;
  production?: string;
  profile?: string;
  example: string;
  alt?: string;
  encodings?: string[];
  roles?: string[];
}
export interface Role { id: string }
export interface Encoding { id: string }
export interface Containment { id: string; role: string; feature: string; spec?: string; example?: string }
export interface ControlOp { id: string; name: string; rfc: string; example?: string }

export interface MatrixInputs {
  features: Feature[];
  roles: Role[];
  contain: Containment[];
  encodings: Encoding[];
  controlOps: ControlOp[];
}

// Bun.TOML.parse returns `any`; this is the single boundary where the untyped TOML becomes the schema.
export const loadToml = (rel: string): any => Bun.TOML.parse(readFileSync(`${ROOT}/${rel}`, "utf8"));

// A mis-keyed table name ([[features]] instead of [[feature]], or any typo) would silently
// contribute ZERO rows to the matrix with every gate green — the vacuous-empty failure class at
// the root of the whole pipeline. Every loader therefore asserts its expected array-of-tables key
// exists and is non-empty in each file it reads.
export function loadTomlArray(rel: string, key: string): any[] {
  const doc = loadToml(rel);
  const rows = doc[key];
  if (!Array.isArray(rows) || rows.length === 0)
    throw new Error(
      `${rel}: expected a non-empty [[${key}]] array-of-tables ` +
        `(top-level keys found: ${Object.keys(doc).join(", ") || "none"})`,
    );
  // The check above only catches a WHOLE-file mis-key. A typo'd table appended to an already-valid
  // file (e.g. [[featuer]] after a real [[feature]]) leaves `doc[key]` non-empty, so its rows are
  // dropped silently. Each loader reads exactly one array-of-tables key, so any other top-level key
  // is a typo — reject it. (annotations/corpus is multi-key and is read via loadToml, not here.)
  const unexpected = Object.keys(doc).filter(k => k !== key);
  if (unexpected.length)
    throw new Error(
      `${rel}: unexpected top-level key(s) [${unexpected.join(", ")}] — only [[${key}]] is read here; ` +
        `a typo'd table name would otherwise contribute zero rows with every gate green`,
    );
  return rows;
}

const globSorted = (pattern: string): string[] => [...new Bun.Glob(pattern).scanSync({ cwd: ROOT })].sort();
export const globRel = globSorted;

// IANA control-op registry, derived from the CSV (id = "ctl." + name without leading dots; rfc =
// Reference with surrounding []/whitespace stripped). No `profile` here — build adds it. The minimal
// support-probe `example` per op is joined from the authored control_examples.toml (the CSV is pinned).
export function loadControlOps(): ControlOp[] {
  const examples = new Map<string, string>(
    (loadTomlArray("control_examples.toml", "example") as { id: string; example: string }[]).map(e => [e.id, e.example]),
  );
  const lines = readFileSync(`${ROOT}/sources/cddl-control-operators.csv`, "utf8").split("\n").filter(l => l.trim().length);
  // The parse assumes the pinned registry's exact shape (2-col, unquoted, single [RFCxxxx] refs).
  // IANA CSVs elsewhere use quoted cells and multi-reference values, so a future registry bump must
  // fail LOUDLY at parse time here rather than flow mangled rfc/id fields into matrix.json.
  if (!lines[0]?.startsWith("Name,"))
    throw new Error(`cddl-control-operators.csv: unexpected header \`${lines[0]}\` — registry format drifted`);
  return lines.slice(1).map(line => {
    const cells = line.split(",");
    if (cells.length !== 2)
      throw new Error(`cddl-control-operators.csv: expected 2 unquoted cells, got ${cells.length} in \`${line}\``);
    const name = cells[0].trim();
    const rfc = (cells[1] ?? "").trim().replace(/^[[\]]+|[[\]]+$/g, "");
    if (!/^\.[a-z0-9-]+$/i.test(name) || !/^[A-Za-z0-9-]+$/.test(rfc))
      throw new Error(`cddl-control-operators.csv: unexpected cell shape (name=\`${name}\`, ref=\`${rfc}\`) — quoted/multi-ref cells need a real parser`);
    const id = "ctl." + name.replace(/^\.+/, "");
    return { id, name, rfc, example: examples.get(id) };
  });
}

// The authored overlay, loaded the same way by build_matrix.ts and verify.ts.
export function loadMatrixInputs(): MatrixInputs {
  return {
    features: globSorted("features/*.toml").flatMap(p => loadTomlArray(p, "feature")),
    roles: loadTomlArray("roles.toml", "role"),
    contain: globSorted("containment/*.toml").flatMap(p => loadTomlArray(p, "contain")),
    encodings: loadTomlArray("encodings.toml", "encoding"),
    controlOps: loadControlOps(),
  };
}

// Deterministic, diff-friendly JSON for the serialized outputs (the committed matrix.json and the
// transient, gitignored verify_report.json): object keys sorted recursively so the output is
// canonical regardless of construction order, 2-space indent, trailing newline.
export function stableJson(obj: unknown): string {
  const sortKeys = (_k: string, v: unknown) =>
    v && typeof v === "object" && !Array.isArray(v)
      ? Object.fromEntries(Object.keys(v).sort().map(k => [k, (v as Record<string, unknown>)[k]]))
      : v;
  return JSON.stringify(obj, sortKeys, 2) + "\n";
}

export function stripComment(s: string): string {
  let out = "", inQ = false;
  for (const ch of s) {
    if (ch === '"') inQ = !inQ;
    if (ch === ";" && !inQ) break;
    out += ch;
  }
  return out.replace(/\s+$/, "");
}

export function splitTopAlts(s: string): string[] {
  const alts: string[] = [];
  let buf = "", depth = 0, inQ = false;
  for (const ch of s) {
    if (inQ) { buf += ch; if (ch === '"') inQ = false; }
    else if (ch === '"') { inQ = true; buf += ch; }
    else if (ch === "(" || ch === "[" || ch === "{") { depth++; buf += ch; }
    else if (ch === ")" || ch === "]" || ch === "}") { depth--; buf += ch; }
    else if (ch === "/" && depth === 0) { alts.push(buf); buf = ""; }
    else buf += ch;
  }
  if (buf) alts.push(buf);
  return alts.map(a => a.trim()).filter(a => a.length);
}

export function productionAlternatives(name: string, abnfText: string): string[] | null {
  const out: string[] = [];
  let inBlock = false;
  for (const raw of abnfText.split(/\r?\n/)) {
    const m = raw.match(/^([A-Za-z][A-Za-z0-9_-]*)\s*=\s*(.*)$/);
    if (m) {
      if (m[1] === name) { inBlock = true; out.push(stripComment(m[2])); }
      else if (inBlock) break;
      continue;
    }
    if (inBlock) {
      const s = raw.trim();
      if (s === "") break;
      out.push(stripComment(s));
    }
  }
  if (!inBlock) return null;
  return splitTopAlts(out.filter(x => x).join(" "));
}

export const normalizeAlt = (s: string): string => stripComment(s).replace(/\bS\b/g, "").replace(/\s/g, "");

export const ALT_PRODUCTIONS = ["type2", "value", "rangeop", "occur", "memberkey", "group", "grpchoice",
  "grpent", "type", "type1", "assignt", "assigng", "rule", "genericparm", "genericarg", "head-number"] as const;

export const ALT_MIN_ALTERNATIVES: Record<typeof ALT_PRODUCTIONS[number], number> = {
  type2: 12,
  value: 3,
  rangeop: 2,
  occur: 3,
  memberkey: 3,
  group: 1,
  grpchoice: 1,
  grpent: 3,
  type: 1,
  type1: 1,
  assignt: 2,
  assigng: 2,
  rule: 2,
  genericparm: 1,
  genericarg: 1,
  "head-number": 2,
};

export interface DelegatedAlt { alt: string; reason: string }
export const DELEGATED_ALTS: Record<string, DelegatedAlt[]> = {
  assignt: [{ alt: '"="', reason: "ordinary type-rule assignment; every type rule already exercises it" }],
  assigng: [{ alt: '"="', reason: "ordinary group-rule assignment; every group rule already exercises it" }],
};

export interface ModelledAlt { alt: string; featureIds: string[] }
export const MODELED_UNDER: Record<string, ModelledAlt[]> = {
  "head-number": [
    { alt: "uint", featureIds: ["type2.tag", "type2.major7"] },
    { alt: '("<" type ">")', featureIds: ["type2.tag_head_type"] },
  ],
};

export interface AltCoverage {
  production: string;
  abnf_alternatives: string[];
  feature_rows: string[];
  covered: string[];
  delegated: { alt: string; reason: string }[];
  modeled_under: { alt: string; featureIds: string[] }[];
  uncovered: string[];
  modeled: boolean;
}

export interface AltCoverageResult {
  coverage: Record<string, AltCoverage>;
  problems: string[];
  vacuityProblems: string[];
}

interface AltFeatureRow { id: string; production?: string | null; alt?: string | null }

const matchingEntry = <T extends { alt: string }>(entries: T[] | undefined, alt: string): T | undefined =>
  entries?.find(e => normalizeAlt(e.alt) === normalizeAlt(alt));

export function grammarAltCoverage(features: AltFeatureRow[], abnfText: string): AltCoverageResult {
  const featureIds = new Set(features.map(f => f.id));
  const coverage: Record<string, AltCoverage> = {};
  const problems: string[] = [];
  const vacuityProblems: string[] = [];

  for (const prod of ALT_PRODUCTIONS) {
    const alts = productionAlternatives(prod, abnfText) ?? [];
    const prodFeatures = features.filter(f => f.production === prod);
    const featNorms = new Set(prodFeatures.filter(f => f.alt).map(f => normalizeAlt(f.alt!)));
    const covered: string[] = [];
    const delegated: { alt: string; reason: string }[] = [];
    const modeled_under: { alt: string; featureIds: string[] }[] = [];
    const uncovered: string[] = [];

    for (const a of alts) {
      const delegatedAlt = matchingEntry(DELEGATED_ALTS[prod], a);
      const modelledAlt = matchingEntry(MODELED_UNDER[prod], a);
      if (featNorms.has(normalizeAlt(a))) covered.push(a);
      else if (delegatedAlt) delegated.push({ alt: a, reason: delegatedAlt.reason });
      else if (modelledAlt) modeled_under.push({ alt: a, featureIds: modelledAlt.featureIds });
      else uncovered.push(a);
    }

    coverage[prod] = {
      production: prod,
      abnf_alternatives: alts,
      feature_rows: prodFeatures.map(f => f.id).sort(),
      covered,
      delegated,
      modeled_under,
      uncovered,
      modeled: prodFeatures.length > 0 || delegated.length > 0 || modeled_under.length > 0,
    };

    const floor = ALT_MIN_ALTERNATIVES[prod];
    if (alts.length < floor)
      problems.push(`${prod} extraction yielded ${alts.length} alternatives (expected >= ${floor}) — the ABNF block extraction truncated`);
    if (alts.length !== floor)
      vacuityProblems.push(`expected exactly ${floor} ${prod} alternatives, saw ${alts.length} — the pinned grammar shape changed (review before re-pinning the floor)`);
    for (const a of uncovered)
      problems.push(`${prod} alternative not modelled by any feature: ${JSON.stringify(a)}`);
  }

  for (const [prod, entries] of Object.entries(DELEGATED_ALTS)) {
    const alts = coverage[prod]?.abnf_alternatives ?? [];
    for (const entry of entries) {
      if (!alts.some(a => normalizeAlt(a) === normalizeAlt(entry.alt)))
        problems.push(`${prod} delegated alternative ${JSON.stringify(entry.alt)} is stale (not present in pinned grammar)`);
    }
  }

  for (const [prod, entries] of Object.entries(MODELED_UNDER)) {
    const alts = coverage[prod]?.abnf_alternatives ?? [];
    for (const entry of entries) {
      if (!alts.some(a => normalizeAlt(a) === normalizeAlt(entry.alt)))
        problems.push(`${prod} modelled-under alternative ${JSON.stringify(entry.alt)} is stale (not present in pinned grammar)`);
      for (const id of entry.featureIds)
        if (!featureIds.has(id))
          problems.push(`${prod} alternative ${JSON.stringify(entry.alt)} is modelled under missing feature id ${JSON.stringify(id)}`);
    }
  }

  return { coverage, problems, vacuityProblems };
}
