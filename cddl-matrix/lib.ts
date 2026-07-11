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

// ==================================================================================================
// Accept-vector ARM-COVERAGE floor for choice rows — the CONSERVATIVE resolver, shared by the mint
// (verify.ts `mintRow`, resample-until-covered) and the drift gate (project_decode_conformance.ts § 7).
// ONE source of truth so the two sites cannot drift apart on WHICH rows are in scope or WHAT arm
// classes each requires. The floor's guarantee: for a row whose spec root is a type CHOICE with
// statically-resolvable arm head major-classes, the catalog carries >=1 spec-valid accept vector per
// resolvable arm class — so a randomized mint that never sampled a whole arm can't silently under-claim
// the row's decode verdict.
// ==================================================================================================

// Leading-major CLASS of a CBOR head, majors 0/1 merged into "int" (uint/nint both satisfy a CDDL
// `int`-shaped rule; the same 0/1 merge the drift gate's § 6 shape check and the rust replay gate's
// `header_major_class` use). `holder` strips the 2-byte `82 00` = `[0, _]` preamble the holder-mode
// mint prepends, so the classified head is the ITEM under test, not the array wrapper.
export function vectorShapeClass(hex: string, holder: boolean): string {
  const h = holder ? hex.slice(4) : hex;
  const major = parseInt(h.slice(0, 2), 16) >> 5;
  return major <= 1 ? "int" : String(major);
}

// The RFC 8610 prelude, parsed from the PINNED source (sources/cddl.prelude) into name -> RHS. Deriving
// the arm classes mechanically from what is committed (rather than a hand table) means a prelude refresh
// that changes a choice type's arms is reflected automatically; the drift gate's § 7 scope pin makes any
// resulting scope change loud.
const preludeDefById: Map<string, string> = (() => {
  const m = new Map<string, string>();
  for (const raw of readFileSync(`${ROOT}/sources/cddl.prelude`, "utf8").split(/\r?\n/)) {
    const mm = raw.match(/^([A-Za-z][A-Za-z0-9_.-]*)\s*=\s*(.*)$/);
    if (mm) m.set(mm[1], stripComment(mm[2]).trim());
  }
  return m;
})();

// A CBOR control head (`#0`..`#3`, `#6.N(...)`, `#7.N`): its major-type class, or null for a bare `#`
// (matches `any` — deliberately unresolvable, the floor must not guess).
function hashHeadClass(tok: string): string | null {
  const d = tok.match(/^#(\d)/);
  if (!d) return null;
  const maj = parseInt(d[1], 10);
  return maj === 0 || maj === 1 ? "int" : String(maj);
}
// A CDDL literal in arm-head position: integer/radix -> int; decimal/hexfloat -> 7 (float); "…" -> 3
// (text); '…' -> 2 (bytes). null for a non-literal.
function literalHeadClass(tok: string): string | null {
  if (/^-?\d+$/.test(tok) || /^-?0x[0-9a-fA-F]+$/.test(tok) || /^-?0b[01]+$/.test(tok)) return "int";
  if (/^-?\d+\.\d/.test(tok) || /^-?0x[0-9a-fA-F.]+p[+-]?\d+$/i.test(tok)) return "7";
  if (/^"/.test(tok)) return "3";
  if (/^'/.test(tok)) return "2";
  return null;
}

// Recursively resolve a prelude type expression to its set of leaf major-classes (SOURCE 2 — a bare
// prelude CHOICE type name as the whole root RHS). `unresolvable` is set if any leaf is a bare `#` /
// unknown name — a conservative signal the caller treats as out-of-scope (never a guess).
function resolvePreludeClasses(expr: string, seen: Set<string> = new Set()): { classes: Set<string>; unresolvable: boolean } {
  const classes = new Set<string>();
  let unresolvable = false;
  for (const armRaw of splitTopAlts(expr)) {
    const arm = armRaw.trim();
    const lit = literalHeadClass(arm);
    if (lit) { classes.add(lit); continue; }
    if (arm.startsWith("#")) { const c = hashHeadClass(arm); if (c) classes.add(c); else unresolvable = true; continue; }
    if (arm.startsWith("[")) { classes.add("4"); continue; }
    if (arm.startsWith("{")) { classes.add("5"); continue; }
    const name = arm.split(/[\s(]/)[0];
    if (preludeDefById.has(name) && !seen.has(name)) {
      const r = resolvePreludeClasses(preludeDefById.get(name)!, new Set([...seen, name]));
      for (const c of r.classes) classes.add(c);
      if (r.unresolvable) unresolvable = true;
    } else unresolvable = true;
  }
  return { classes, unresolvable };
}

// The conservative head-token table for an EXPLICIT-choice arm (SOURCE 1). Anything not listed is
// unresolvable -> that arm is EXEMPT (per-arm exemption). Deliberately does NOT recurse arbitrary names
// (unlike the prelude resolver): a choice arm naming a non-leaf type the table doesn't cover is a guess
// we refuse to make.
function explicitArmHeadClass(arm: string): string | null {
  const a = arm.trim();
  const lit = literalHeadClass(a);
  if (lit) return lit;
  if (/^#6(\.|\(|$)/.test(a)) return "6";
  if (a.startsWith("[")) return "4";
  if (a.startsWith("{")) return "5";
  const name = a.split(/[\s(]/)[0];
  const T: Record<string, string> = {
    uint: "int", nint: "int", int: "int",
    tstr: "3", text: "3", bstr: "2", bytes: "2",
    float: "7", float16: "7", float32: "7", float64: "7", "float16-32": "7", "float32-64": "7",
    bool: "7", true: "7", false: "7", nil: "7", null: "7", undefined: "7",
    biguint: "6", bignint: "6", bigint: "6",
  };
  return T[name] ?? null;
}

// The root rule's RHS from an `example`, or null when the root rule uses an INCREMENTAL choice extension
// (`/=` / `//=`): that spelling spreads the choice across multiple statements the single-statement reader
// here can't merge, and cddl-codegen itself silently drops all but the last arm (ROADMAP § findings), so
// the floor refuses to model it rather than guess a wrong arm set (e.g. `a = int` / `a /= tstr`).
function rootRuleRhs(example: string): string | null {
  for (const raw of example.split(/\r?\n/)) {
    const line = stripComment(raw).trim();
    if (/^[A-Za-z@_$][A-Za-z0-9@_$.-]*\s*(\/=|\/\/=)/.test(line)) return null;
  }
  for (const raw of example.split(/\r?\n/)) {
    const line = stripComment(raw).trim();
    if (!line) continue;
    const m = line.match(/^([A-Za-z@_$][A-Za-z0-9@_$.-]*)\s*(<[^=]*>)?\s*=(?![=>])\s*(.*)$/);
    if (m) return m[3].trim();
  }
  return null;
}

export interface ArmClassResolution {
  classes: string[];  // sorted, distinct, majors 0/1 merged ("int") — the required arm classes
  exempt: string[];   // explicit-choice arms whose head could not be resolved (informational)
}
// Resolve a matrix `example` to its required arm-coverage classes, or null when the row is OUT OF SCOPE
// (root RHS is not a type choice, or nothing resolves). Two sources:
//   1. an explicit top-level `/` choice in the root RHS — classify each arm via the conservative head
//      table; unresolvable arms are EXEMPT (dropped, not a guess); out of scope if NO arm resolves.
//   2. the root RHS being EXACTLY a bare prelude CHOICE type name (`number`, `integer`, `unsigned`,
//      `bigint`, …) — resolved through the pinned prelude. A control-op-constrained or otherwise
//      decorated RHS (`int .eq 5`) is NOT a bare choice name and stays out of scope.
export function resolveChoiceArmClasses(example: string): ArmClassResolution | null {
  const rhs = rootRuleRhs(example);
  if (rhs === null) return null;
  const topArms = splitTopAlts(rhs);
  if (topArms.length >= 2) {
    const classes = new Set<string>();
    const exempt: string[] = [];
    for (const arm of topArms) {
      const c = explicitArmHeadClass(arm);
      if (c) classes.add(c); else exempt.push(arm.trim());
    }
    if (classes.size === 0) return null;
    return { classes: [...classes].sort(), exempt };
  }
  if (/^[A-Za-z][A-Za-z0-9_.-]*$/.test(rhs) && preludeDefById.has(rhs)) {
    const def = preludeDefById.get(rhs)!;
    if (splitTopAlts(def).length >= 2) {
      const r = resolvePreludeClasses(def);
      if (!r.unresolvable && r.classes.size > 0) return { classes: [...r.classes].sort(), exempt: [] };
    }
  }
  return null;
}

// Exemption ledger for a genuinely UNMINTABLE arm class (an oracle gap), keyed `"<row id>/<class>"`
// -> cited reason. Shared by BOTH consumers: the mint (won't exit 1 / won't waste draws for a ledgered
// class) and the drift gate (won't fail the coverage floor for it). Stale-guarded on the gate side: a
// ledger entry for a (row, class) that is now covered — or a row no longer in scope — fails the gate, so
// when the underlying oracle gap closes the class is re-minted and the entry must be removed.
// Currently empty: the last resident (`prelude.number`'s float arm, blocked by the pre-ac1b98e rust
// oracle rejecting floats against the prelude `number` keyword) was re-minted when the fork fix landed.
export const DECODE_FLOOR_ARM_EXEMPT: Record<string, string> = {};

// ==================================================================================================
// DECODE-CONFORMANCE CATALOG reader/writer pair — the SOLE serializer of the hand-authored vector
// fields (class/reason/expect_err). Shared by the mint (verify.ts `--mint-decode-foreign`) and the
// drift gate (project_decode_conformance.ts § 8), which asserts compose(parse(catalog.toml)) is
// byte-identical to the committed file — a writer that drops or reorders any field goes red before
// any mint runs (the silent-strip bug class caught once by review, see the class/reason comment below).
// ==================================================================================================
export interface CatalogVector { hex: string; source: string; expect: string; class?: string; reason?: string; expect_err?: string }
export interface CatalogRow {
  id: string; axis: string; example: string;
  pinned_reason?: string;                             // set => the row has no vectors (names the cause)
  spec?: string; mode?: string; type_name?: string;   // set together when NOT pinned
  vectors: CatalogVector[];
}

// JSON string escaping is a valid TOML basic string (same trick as the annotation writer's `tomlStr`,
// but hoisted so the mint can use it BEFORE that `const` is initialized).
function foreignTomlStr(s: string): string { return JSON.stringify(s); }

// Parse catalog TOML CONTENT (no file read) into the id -> row map — kept file-independent so the drift
// gate can round-trip already-read bytes and a synthetic in-code sample without touching disk.
export function parseCatalogContent(toml: string): Map<string, CatalogRow> {
  const doc = Bun.TOML.parse(toml) as { row?: any[] };
  const map = new Map<string, CatalogRow>();
  for (const r of doc.row ?? []) {
    const vectors: CatalogVector[] = (r.vector ?? []).map((v: any) => ({
      hex: String(v.hex), source: String(v.source), expect: String(v.expect),
      class: v.class !== undefined ? String(v.class) : undefined,
      reason: v.reason !== undefined ? String(v.reason) : undefined,
      expect_err: v.expect_err !== undefined ? String(v.expect_err) : undefined,
    }));
    map.set(String(r.id), {
      id: String(r.id), axis: String(r.axis), example: String(r.example),
      pinned_reason: r.pinned_reason !== undefined ? String(r.pinned_reason) : undefined,
      spec: r.spec !== undefined ? String(r.spec) : undefined,
      mode: r.mode !== undefined ? String(r.mode) : undefined,
      type_name: r.type_name !== undefined ? String(r.type_name) : undefined,
      vectors,
    });
  }
  return map;
}

// Thin file-reading wrapper over parseCatalogContent (the mint reads the committed catalog by path).
export function parseCatalog(path: string): Map<string, CatalogRow> {
  return parseCatalogContent(readFileSync(path, "utf8"));
}

// Compose the catalog TOML deterministically (rows by id, vectors by hex) so a re-mint of any `--only`
// subset re-emits every other row byte-identically. Header mirrors annotations/cddl_codegen.toml's style.
export function composeCatalog(rows: CatalogRow[]): string {
  const L: string[] = [
    "# Decode-conformance catalog. MACHINE-PRODUCED by the mint:",
    "#   bun run verify.ts --mint-decode-foreign            # full refresh",
    "#   bun run verify.ts --mint-decode-foreign --only=ID  # re-mint one row, preserve the rest",
    "# Each row projects a matrix `supported` row: spec-derived CBOR instances (ruby `cddl … generate`,",
    "# cross-validated by BOTH the ruby reference AND rust `cddl --ci validate`) that the generated",
    '# decoder must accept. Hand-edit ONLY for triage class/reason on reject pins and source="hand"',
    "# supplement vectors (both re-validated mechanically at the next mint).",
    "#",
    "# mode: standalone = a nominal `impl Deserialize for <type_name>` decodes the vector directly;",
    "#       holder = the rule is a transparent alias / named table / c-enum with no standalone decoder,",
    "#       so vectors are instances of `__probe_holder = [0, <rule>]` and decode routes through the",
    "#       GENERATED field-decode code (cbor_event's blanket impl would otherwise make it vacuous).",
    "# vector.expect: accept (decoder must Ok) | reject (decoder must Err). A reject vector carries a",
    "#       class + reason. Two kinds, opposite spec-validity:",
    "#         bug | limitation = spec-VALID CBOR the decoder WRONGLY rejects (a known gap); re-validated",
    "#           spec-VALID at each mint and PRUNED when the gap closes. A class-less reject is the mint's",
    "#           triage-pending state — the drift gate stays RED until a human classifies it.",
    "#         constraint = spec-INVALID CBOR (source=\"hand\") that VIOLATES a constraint the row enforces",
    "#           (an over/under-`.size` string, a below-`.ge` value, a cut-violating map value); the",
    "#           generated decoder must DURABLY reject it. Re-validated spec-INVALID (both oracles reject)",
    "#           at each mint — never pruned; `reason` names the violated constraint. This is Q4's",
    "#           `enforce = yes (bounded-reject)` evidence. A constraint vector ALSO carries a required",
    "#           `expect_err`: a substring the generated decoder's error Display must contain when it",
    "#           rejects the vector — the rust replay gate pins the rejection REASON, not just that it",
    "#           rejects (a stray length check / unrelated error path would decode-reject but mis-name).",
    "#       An accept vector may ALSO carry a class, but ONLY class=\"over-acceptance\": spec-INVALID CBOR",
    "#       (source=\"hand\") that the generated decoder CURRENTLY (wrongly) ACCEPTS — a certified silent-",
    "#       acceptance bug with no fix yet. Both oracles REJECT it (re-validated spec-INVALID at each mint,",
    "#       the same inverse gate as class=\"constraint\"); the replay gate asserts the decoder STILL",
    "#       accepts it, so the pin flips LOUDLY when a fix lands — the signal to promote it to",
    "#       class=\"constraint\" (+ expect_err) and flip the row's Q4 enforce projection green. Never pruned",
    "#       mechanically; `reason` cites the ledgered finding + the promotion flow. A plain accept vector",
    "#       (spec-VALID, correctly accepted) carries NO class.",
    "# pinned_reason: the row could not be minted mechanically (names the cause); it then has no vectors.",
    "",
  ];
  for (const row of [...rows].sort((a, b) => (a.id < b.id ? -1 : a.id > b.id ? 1 : 0))) {
    L.push("[[row]]");
    L.push(`id = ${foreignTomlStr(row.id)}`);
    L.push(`axis = ${foreignTomlStr(row.axis)}`);
    L.push(`example = ${foreignTomlStr(row.example)}`);
    if (row.pinned_reason !== undefined) {
      L.push(`pinned_reason = ${foreignTomlStr(row.pinned_reason)}`);
    } else {
      L.push(`spec = ${foreignTomlStr(row.spec ?? "")}`);
      L.push(`mode = ${foreignTomlStr(row.mode ?? "")}`);
      L.push(`type_name = ${foreignTomlStr(row.type_name ?? "")}`);
      for (const v of [...row.vectors].sort((a, b) => (a.hex < b.hex ? -1 : a.hex > b.hex ? 1 : 0))) {
        L.push("");
        L.push("[[row.vector]]");
        L.push(`hex = ${foreignTomlStr(v.hex)}`);
        L.push(`source = ${foreignTomlStr(v.source)}`);
        L.push(`expect = ${foreignTomlStr(v.expect)}`);
        // class/reason/expect_err are emitted whenever present — reject pins (bug/limitation/constraint)
        // AND class="over-acceptance" accept vectors both carry them. A plain accept vector has none, so
        // its output is unchanged. (Guarding on `expect === "reject"` would silently strip the class and
        // reason from an over-acceptance vector on re-mint.)
        if (v.class !== undefined) L.push(`class = ${foreignTomlStr(v.class)}`);
        if (v.reason !== undefined) L.push(`reason = ${foreignTomlStr(v.reason)}`);
        if (v.expect_err !== undefined) L.push(`expect_err = ${foreignTomlStr(v.expect_err)}`);
      }
    }
    L.push("");
  }
  return L.join("\n").replace(/\s+$/, "") + "\n";
}
