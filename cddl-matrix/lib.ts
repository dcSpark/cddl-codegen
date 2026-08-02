// Shared matrix-loading + serialization for build_matrix.ts / verify.ts.
// Paths resolve relative to this file's dir (cddl-matrix/).
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, readdirSync, renameSync, statSync, writeFileSync } from "node:fs";
import { basename, join, relative, resolve, sep } from "node:path";

export const ROOT = import.meta.dir;

// The authored-overlay schema. Only the fields the tooling reads are declared; the TOML rows carry
// more (title, desc, rfc, …) which pass through to matrix.json untouched.
export interface Feature {
  id: string;
  production?: string;
  profile?: string;
  example: string;
  /** Extern-scope companion for the probe: when present, `example` is generated as a DIRECTORY
   * input (`lib.cddl` = example) with this content at
   * `_CDDL_CODEGEN_EXTERN_DEPS_DIR_/extern_dep/lib.cddl` — the only legal home for extern-scope
   * directives like `@rust_name`, which reject on exported rules by design. */
  example_extern_stub?: string;
  alt?: string;
  encodings?: string[];
  roles?: string[];
}
export interface Role { id: string }
/** An encoding-grid row. PARENT vs LEAF is STRUCTURAL: a row is a PARENT iff it declares `cells`
 * (the leaf ids beneath it), a LEAF iff it declares none — no form vocabulary to keep in sync.
 * `encodings.toml`'s header explains why the relation is data rather than an id-prefix rule. */
export interface Encoding { id: string; major_type?: number; cells?: string[] }
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

export const GATE_CACHE_SCHEMA = "gate-cache-v1";

export interface GateCacheKeyComponents {
  gate: string;
  argv: string[];
  tree: string;
  rustflags?: string;
}

export interface GateCacheEntry {
  schema: typeof GATE_CACHE_SCHEMA;
  gate: string;
  cell: string;
  argv: string[];
  rustc: string;
  tree: string;
  created: string;
}

let cachedRustcVersionVerbose: string | null = null;

export function gateCacheEnabled(env: NodeJS.ProcessEnv = process.env): boolean {
  return !["0", "false"].includes((env.GATE_CACHE ?? "").toLowerCase());
}

export function gateCacheDir(repoRoot = resolve(ROOT, ".."), env: NodeJS.ProcessEnv = process.env): string {
  return env.GATE_CACHE_DIR ? resolve(env.GATE_CACHE_DIR) : join(repoRoot, ".gate-cache");
}

function rustcVersionVerbose(): string {
  if (cachedRustcVersionVerbose !== null) return cachedRustcVersionVerbose;
  const r = Bun.spawnSync(["rustc", "-vV"], { stdout: "pipe", stderr: "pipe" });
  const out = (r.stdout?.toString() ?? "") + (r.stderr?.toString() ?? "");
  cachedRustcVersionVerbose = out;
  return cachedRustcVersionVerbose;
}

function hashUpdateString(h: ReturnType<typeof createHash>, s: string): void {
  h.update(Buffer.from(s, "utf8"));
}

export function hashTree(root: string): string {
  const base = resolve(root);
  const files: string[] = [];
  const walk = (dir: string) => {
    for (const ent of readdirSync(dir, { withFileTypes: true })) {
      const p = join(dir, ent.name);
      if (ent.isDirectory()) {
        if (ent.name !== "target") walk(p);
        continue;
      }
      const st = statSync(p);
      if (st.isFile()) files.push(relative(base, p).split(sep).join("/"));
    }
  };
  walk(base);
  files.sort();

  const h = createHash("sha256");
  for (const rel of files) {
    const bytes = readFileSync(join(base, ...rel.split("/")));
    hashUpdateString(h, rel);
    h.update(Buffer.from([0]));
    hashUpdateString(h, String(bytes.length));
    h.update(Buffer.from([0]));
    h.update(bytes);
    h.update(Buffer.from([0]));
  }
  return h.digest("hex");
}

export function gateCacheKey(components: GateCacheKeyComponents): { key: string; rustc: string; rustflags: string } {
  const rustc = rustcVersionVerbose();
  const rustflags = components.rustflags ?? process.env.RUSTFLAGS ?? "";
  const material = stableJson({
    schema: GATE_CACHE_SCHEMA,
    gate: components.gate,
    argv: components.argv,
    rustc,
    rustflags,
    tree: components.tree,
  });
  return { key: createHash("sha256").update(material).digest("hex"), rustc, rustflags };
}

export function readGateCacheEntry(key: string, repoRoot?: string, env: NodeJS.ProcessEnv = process.env): GateCacheEntry | null {
  if (!gateCacheEnabled(env)) return null;
  try {
    const raw = readFileSync(join(gateCacheDir(repoRoot, env), `${key}.json`), "utf8");
    const entry = JSON.parse(raw) as Partial<GateCacheEntry>;
    if (entry.schema !== GATE_CACHE_SCHEMA || typeof entry.gate !== "string" || typeof entry.cell !== "string" ||
        !Array.isArray(entry.argv) || typeof entry.rustc !== "string" || typeof entry.tree !== "string" ||
        typeof entry.created !== "string")
      return null;
    return entry as GateCacheEntry;
  } catch {
    return null;
  }
}

export function writeGateCacheEntry(key: string, entry: GateCacheEntry, repoRoot?: string, env: NodeJS.ProcessEnv = process.env): void {
  if (!gateCacheEnabled(env)) return;
  const dir = gateCacheDir(repoRoot, env);
  mkdirSync(dir, { recursive: true });
  const tmp = join(dir, `.${basename(key)}.${process.pid}.${Date.now()}.tmp`);
  writeFileSync(tmp, stableJson(entry));
  renameSync(tmp, join(dir, `${key}.json`));
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

// The pinned prelude as name -> RHS. Exported so the master's own drift gate can DERIVE a prelude
// construct's fixed CBOR head from the same pinned bytes the arm resolver reads, rather than trusting
// a hand-authored `encodings` list to agree with the prelude.
export const PRELUDE_DEFS: ReadonlyMap<string, string> = preludeDefById;

// The prelude's plain-ALIAS equivalence classes, derived from the same pinned bytes: name -> every
// prelude name denoting the identical type. An alias is a rule whose body is EXACTLY a bare typename
// naming another prelude rule (`bytes = bstr`, `text = tstr`, `null = nil`) — two spellings of one
// construct, so anything true of one is true of the other.
//
// The width of this rule is the whole point, and it is deliberately narrow. A CHOICE body is NOT an
// alias: `float = float16-32 / float64`, `integer = int / bigint` and `bigint = biguint / bignint` are
// unions whose members encode differently, so folding them into a class would credit a construct for
// coverage of a sibling it does not share a wire form with — the exact over-credit the per-construct
// projection exists to remove. Chains (`a = b`, `b = c`) are closed transitively so the relation is a
// real equivalence rather than one hop.
export function preludeAliasClasses(): Map<string, Set<string>> {
  const parent = new Map<string, string>();
  const find = (x: string): string => {
    let r = x;
    while (parent.get(r) !== undefined && parent.get(r) !== r) r = parent.get(r)!;
    return r;
  };
  const union = (a: string, b: string) => {
    const [ra, rb] = [find(a), find(b)];
    if (ra !== rb) parent.set(ra, rb);
  };
  for (const name of preludeDefById.keys()) parent.set(name, name);
  for (const [name, body] of preludeDefById) {
    const t = body.trim();
    if (/^[A-Za-z][A-Za-z0-9_.-]*$/.test(t) && preludeDefById.has(t)) union(name, t);
  }
  const classes = new Map<string, Set<string>>();
  const byRoot = new Map<string, Set<string>>();
  for (const name of preludeDefById.keys()) {
    const r = find(name);
    if (!byRoot.has(r)) byRoot.set(r, new Set());
    byRoot.get(r)!.add(name);
  }
  // Only multi-member classes are interesting; a singleton is its own trivial class and carries no
  // information, so returning it would make a vacuity check on the result impossible to fail.
  for (const members of byRoot.values())
    if (members.size > 1) for (const m of members) classes.set(m, members);
  return classes;
}

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

// The CORPUS twin of DECODE_FLOOR_ARM_EXEMPT, keyed `"<fixture>.<rule>/<class>"` -> cited reason. A
// SEPARATE ledger because each stale-guard iterates its OWN uncovered-in-scope set: a corpus-keyed
// entry in the matrix ledger can never appear in the MATRIX guard's uncovered set (its keys are matrix
// row ids), so it would always read stale there and falsely fail the gate. Shared by the corpus mint
// (verify.ts `mintCorpusRow` — won't exit 1 / won't waste draws for a ledgered class) and the corpus
// drift-gate half (project_decode_conformance.ts — coverage floor + its own stale guard). Currently
// empty: no corpus arm class is ledgered unmintable at HEAD.
export const CORPUS_DECODE_FLOOR_ARM_EXEMPT: Record<string, string> = {};

// Exemption ledger for a `class="constraint"` reject vector whose spec-invalidity an ORACLE does not
// see, keyed `"<row id>/<hex>"`. It exists because the two-oracle certification a constraint vector
// normally passes (BOTH oracles reject the bytes, verify.ts `mintForeignRow`) certifies "spec-invalid
// by consensus" — and consensus is unavailable for a rule an oracle does not implement at all. An
// entry names exactly the oracles that still ACCEPT the bytes, so the certification narrows to the
// remaining oracles plus a written, answerable spec argument rather than disappearing.
//
// The `writeup` is that argument, and it is REQUIRED: a committed, submittable report of our spec
// reading against the diverging oracle's behavior, with the vectors, the probe commands and the
// explicit branch "if the oracle is right, this exemption is wrong". An exemption nobody can argue
// with is indistinguishable from a decoder bug we declared correct.
//
// Two stale guards, because the two failure modes are seen by different consumers:
//   - the MINT (verify.ts) holds the oracles: a ledgered oracle that now REJECTS the bytes has closed
//     its gap, so the entry over-claims and must lose that oracle (or go away entirely).
//   - the DRIFT GATE (project_decode_conformance.ts) holds the catalog: a key naming a vector that is
//     no longer a `class="constraint"` reject vector in its row is a dangling entry.
export interface RejectOracleGapExemption {
  /** The oracles that ACCEPT these spec-invalid bytes — `"ruby"`, `"rust"`, or both. */
  oracles: ("ruby" | "rust")[];
  /** Why we read the bytes as spec-invalid, and what the oracle does instead. */
  reason: string;
  /** Repo-relative path of the committed writeup arguing the divergence. */
  writeup: string;
}
const FLOAT_HEAD_WRITEUP = "cddl-matrix/upstream-reports/ruby-cddl-float-width-validation.md";
const RUST_HEAD_BLIND =
  "the pinned rust oracle (local-fixes @ ac1b98e) performs NO float head-width validation at all — it " +
  "accepts every major-7 float head against every float prelude name (README.md § \"Upstream oracle " +
  "gaps\" #12)";
const RUBY_WIDTH_BY_VALUE =
  "the ruby `cddl` gem 0.12.14 classifies a float by the narrowest IEEE width that represents its " +
  "VALUE exactly and ignores the wire head, so it accepts this out-of-set head (and, symmetrically, " +
  "REJECTS canonical in-set encodings)";
export const DECODE_REJECT_ORACLE_GAP_EXEMPT: Record<string, RejectOracleGapExemption> = {
  // `float16` is `#7.25` alone (RFC 8610 App. D), so an `fa`/`fb` head is out of set whatever the
  // value is. 1.5 is f16-exact, so ruby reads both as "a float16" and accepts.
  "prelude.float16/8200fa3fc00000": {
    oracles: ["ruby", "rust"],
    reason: `an \`fa\` (#7.26) head against \`float16\` (#7.25 only); ${RUBY_WIDTH_BY_VALUE} because 1.5 is f16-exact, and ${RUST_HEAD_BLIND}`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  "prelude.float16/8200fb3ff8000000000000": {
    oracles: ["ruby", "rust"],
    reason: `an \`fb\` (#7.27) head against \`float16\` (#7.25 only); ${RUBY_WIDTH_BY_VALUE} because 1.5 is f16-exact, and ${RUST_HEAD_BLIND}`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  // `float32` is `#7.26` alone. ruby agrees on the f9-headed vector (1.5's minimal width is f16, not
  // f32) — for the wrong reason, but it rejects, so only rust needs exempting.
  "prelude.float32/8200f93e00": {
    oracles: ["rust"],
    reason: `an \`f9\` (#7.25) head against \`float32\` (#7.26 only); ${RUST_HEAD_BLIND}. ruby rejects it, but by value width (1.5 is f16-exact) rather than by head — the same rule that makes it reject the canonical \`fa\`-headed 1.5 this row ACCEPTS`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  "prelude.float32/8200fb3ff19999a0000000": {
    oracles: ["ruby", "rust"],
    reason: `an \`fb\` (#7.27) head against \`float32\` (#7.26 only), carrying 1.100000023841858 — an f32-exact value widened losslessly to 8 bytes; ${RUBY_WIDTH_BY_VALUE} because the value is f32-exact, and ${RUST_HEAD_BLIND}`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  // `float64` is `#7.27` alone. A value carried at an f9/fa head is f16-/f32-exact, so ruby's
  // value-width rule rejects it too; only rust needs exempting.
  "prelude.float64/8200f93e00": {
    oracles: ["rust"],
    reason: `an \`f9\` (#7.25) head against \`float64\` (#7.27 only); ${RUST_HEAD_BLIND}. ruby rejects it by value width, not by head`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  "prelude.float64/8200fa3fc00000": {
    oracles: ["rust"],
    reason: `an \`fa\` (#7.26) head against \`float64\` (#7.27 only); ${RUST_HEAD_BLIND}. ruby rejects it by value width, not by head`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  // `float16-32` = `#7.25`/`#7.26`, so `fb` is the only out-of-set head.
  "prelude.float16-32/8200fb3ff8000000000000": {
    oracles: ["ruby", "rust"],
    reason: `an \`fb\` (#7.27) head against \`float16-32\` (#7.25/#7.26); ${RUBY_WIDTH_BY_VALUE} because 1.5 is f16-exact, and ${RUST_HEAD_BLIND}`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
  // `float32-64` = `#7.26`/`#7.27`, so `f9` is the only out-of-set head — and a value at an f9 head is
  // f16-exact, which ruby's value-width rule rejects for this name anyway.
  "prelude.float32-64/8200f93e00": {
    oracles: ["rust"],
    reason: `an \`f9\` (#7.25) head against \`float32-64\` (#7.26/#7.27); ${RUST_HEAD_BLIND}. ruby rejects it by value width, not by head`,
    writeup: FLOAT_HEAD_WRITEUP,
  },
};

// The prelude rule-name set (from the same pinned source the arm resolver parses). Exported for the
// corpus enumeration-time collision assert — a corpus rule named like a prelude type would make
// reference extraction ambiguous — checked by BOTH the corpus mint and the corpus drift-gate half.
export const PRELUDE_NAMES: ReadonlySet<string> = new Set(preludeDefById.keys());

// ==================================================================================================
// The annotations/cddl_codegen.toml file HEADER — the single template both its writer and its
// drift gate read.
//
// verify.ts composes the whole annotations file (this header + the probe-result rows) and rewrites
// it after every passing run, so the committed header is a MACHINE-REGENERATED region: a hand edit
// to the RENDERED file is silently reverted by the next passing verify.ts run, with no diff-time
// signal that hand content was lost. Proven once — the six-class float consumer note below was
// first hand-added to the rendered file only, and the next full-tier verify run reverted it to the
// stale single-line form it replaced. This template is therefore the ONLY place to edit the header
// (mirror the edit into the committed file in the same commit, or re-run verify.ts), and
// project_corpus.ts (fast tier) asserts the committed header matches the template — so BOTH desync
// directions fail loudly at the next fast run instead of surviving until a full verify run
// clobbers one of them.
//
// `decodeForeign` mirrors verify.ts's opt-out discipline: the DECODE-FOREIGN header paragraph is
// emitted only when that oracle is on, so an opted-out run's output is byte-identical to a
// pre-feature run. The committed file is written by default-on runs; the drift gate detects which
// form it holds and compares against the matching template.
// ==================================================================================================
export function annotationsHeaderLines(decodeForeign: boolean): string[] {
  return [
    "# cddl-codegen support, keyed by master feature id. EXECUTION-GROUNDED: generated by verify.ts",
    "# from live oracle probes (NOT hand-read from the generator source). Do not edit by hand — re-run",
    "#   bun run build_matrix.ts && bun run verify.ts",
    "# to regenerate. Each row is the result of running the feature's minimal `example` through:",
    "#   ruby  cddl ... generate 1            (spec-validity A, authoritative / reference)",
    "#   rust  cddl compile-cddl              (spec-validity B, corroborating only)",
    "#   cddl-codegen --input=... --wasm=false --emit-tests=true (generate the crate) THEN `cargo test` it",
    "#     (the EXECUTION-GATE: the emitted IR-minted round-trip/reject tests must PASS — strictly",
    "#     stronger than compiling). support = generates AND compiles AND round-trips. exit 0 alone is",
    "#     NOT enough: `x = any` generates `pub type X = Any;` (a type defined nowhere) which fails to",
    "#     compile -> unsupported, not a false 'supported'. A type that mints no STANDALONE test surface",
    "#     (transparent alias / bounded-or-newtype-able alias / named table or array / pure c-enum) is",
    "#     RE-PROBED wrapped in a synthetic record holder (`__probe_holder = [0, <rule>]`) so its embed-",
    "#     site wire path runs: evidence then reads 'round-trips when embedded (synthetic record holder)'.",
    "#     If the synthetic can't generate (a generic rule needing type args) or can't round-trip, the",
    "#     evidence stays 'no minted round-trip surface' — the embed only ever UPGRADES it.",
    "# status: supported | unsupported | out_of_profile | uncertain.",
    "#   out_of_profile = the feature's grammar profile is NEWER than cddl-codegen's TARGET profile AND",
    "#         cddl-codegen rejects it (it is outside what the tool targets, NOT a gap within it).",
    "#   uncertain = spec-valid but a genuine reference-vs-ABNF conflict. A `rust parser limitation`",
    "#         note means the reference (ruby/ABNF) accepts the example but the rust cddl crate rejects",
    "#         it (e.g. lowercase `h'cafe'`); that is corroboration noise, not a support/validity verdict.",
    "#",
    "# TARGET PROFILE: cddl-codegen tracks ~RFC 8610 (the RFC 8610 grammar). It does NOT implement the",
    "#   RFC 9682 grammar additions (the `#7` split; the type-valued tag head-number,",
    '#   `head-number = uint / ("<" type ">")`). Features tagged `profile = "RFC9682"` that cddl-codegen',
    "#   rejects are therefore `out_of_profile`, not `unsupported`. (Control-op extension RFCs",
    "#   9090/9165/9741 are a separate registry axis whose support is probed per operator.)",
    "#",
    "# CONSUMER NOTES (cddl-codegen-specific facts kept OUT of the pure-spec master, recorded here):",
    "#   * `T / null` type choice -> cddl-codegen emits Option<T> (a consumer special-case of the",
    '#     ordinary `type = type1 *("/" type1)` production, NOT a distinct ABNF alternative).',
    "#   * the six float prelude names are six distinct wire-acceptance classes in cddl-codegen, not",
    "#     two carrier widths: `float16`/`float32`/`float16-32` carry Rust f32, `float64`/`float32-64`/",
    "#     `float` carry f64, and each accepts only the CBOR heads its own name declares.",
    "#",
    "# EMISSION-PROFILE AXIS (dotted `emission.<name>.*` keys): the `status`/`evidence` above is the",
    "#   DEFAULT-flags verdict. A row whose default verdict is `supported` is ALSO probed under each",
    "#   PROBED non-default EMISSION profile (the CLI flag sets from src/tests/mod.rs's ALL_PROFILES:",
    "#   `preserve` = --preserve-encodings=true, `json` = --json-serde-derives + --json-schema-export;",
    "#   the `component` row is NOT probed here — it mints a separate wasip2 crate and leaves every rust",
    "#   byte identical, so this rust-crate round-trip cannot tell it apart from the default verdict),",
    "#   recorded as `emission.<name>.status` / `emission.<name>.evidence`. These probes are RUST-ONLY",
    "#   (same generate -> cargo test -> embed-fallback pipeline with the profile flags appended; NO",
    "#   ruby/rust re-run — spec validity is a property of the CDDL text, not codegen flags — and NO wasm).",
    "#   SCOPING (rule a): only default-`supported` rows are probed. ABSENCE of `emission` keys therefore",
    "#   means the row's default verdict is NOT supported, so it is unsupported under EVERY profile — a",
    "#   DERIVED fact, not silent inheritance. Emission verdicts are NEVER hand-authored; only a passing",
    "#   verify.ts run writes them. Until that run the committed file simply has no emission keys.",
    ...(decodeForeign ? [
      "#",
      "# DECODE-FOREIGN clause (the fourth gate direction): a supported row's",
      "#   `evidence` gains one of `; accepts N foreign spec-derived vector(s)` / `; foreign-vector decode",
      "#   FAILED (…)` / `; no committed decode vectors (see catalog)`. This is the DEFAULT-ON decode-foreign",
      "#   oracle: it regenerates from tests/decode_conformance/catalog.toml's committed `spec` and replays",
      "#   spec-derived CBOR our code did NOT produce through the generated decoder — CORROBORATION ONLY, it",
      "#   never changes a verdict.",
    ] : []),
    "",
  ];
}

// ==================================================================================================
// CORPUS decode-conformance support — the SHARED rule enumerator and dependency-closure builder
// used by BOTH the corpus mint (verify.ts `--mint-decode-corpus`) and the
// corpus drift gate (project_decode_conformance.ts). ONE implementation so the drift gate re-derives
// exactly what the mint derived; any asymmetry would be drift-gate-invisible. Self-checked inline in
// the drift gate against a synthetic multi-rule sample (strings, comments, generics, hyphens).
// ==================================================================================================

// The synthetic record holder wrapping a corpus rule (`__probe_holder = [0, <rule>]`): every corpus row
// is holder-mode (the composition-depth value is the generated member/field decode path). Prepended
// FIRST in the probe spec so both oracles root validation at it (rust: "Root type for validation: …").
export const CORPUS_HOLDER_RULE = "__probe_holder";

// A top-level rule HEAD at column 0 — the SAME shape `firstRuleName` (verify.ts) matches, so the
// enumerator agrees with the embed-fallback rule detection: an identifier (covers `-` in names and
// `@_$`), an optional generic parameter list (`<...>`), then `=` / `/=` / `//=` (not `==`/`=>`).
// Anchored at column 0 so indented member/continuation lines and `;` comments never read as heads.
export const CORPUS_RULE_HEAD_RE = /^([A-Za-z@_$][A-Za-z0-9@_$.-]*)\s*(<[^=]*>)?\s*(\/\/=|\/=|=)(?![=>])/;

export interface CorpusRule {
  name: string;    // enumerated rule name (row-id suffix); the generic BASE name, without `<...>`
  generic: boolean; // head carries a `<...>` parameter list (cannot be holder-wrapped bare → pinned row)
  span: string;    // verbatim text span: the head line through the line before the next head, trailing
                   //   blank lines trimmed. Inline/trailing comments (incl. `; @newtype` DSL) are kept —
                   //   the DSL attaches AFTER a construct, so span-splitting preserves its semantics.
  order: number;   // 0-based index in fixture order (the closure renders rules by this)
}

// Enumerate a fixture's top-level rules into (name, generic, span, order). Leading file comments before
// the first rule are dropped (spans start at the first head). Every enumerated rule becomes one catalog
// obligation row — never a hand-picked list.
export function enumerateCorpusRules(text: string): CorpusRule[] {
  const lines = text.split(/\r?\n/);
  const heads: { name: string; generic: boolean; line: number }[] = [];
  for (let i = 0; i < lines.length; i++) {
    const m = lines[i].match(CORPUS_RULE_HEAD_RE);
    if (m) heads.push({ name: m[1], generic: m[2] !== undefined, line: i });
  }
  const rules: CorpusRule[] = [];
  for (let h = 0; h < heads.length; h++) {
    const start = heads[h].line;
    const end = h + 1 < heads.length ? heads[h + 1].line : lines.length;
    const spanLines = lines.slice(start, end);
    while (spanLines.length && spanLines[spanLines.length - 1].trim() === "") spanLines.pop();
    rules.push({ name: heads[h].name, generic: heads[h].generic, span: spanLines.join("\n"), order: h });
  }
  return rules;
}

// Strip `;` comments AND quoted strings (both `"…"` and `'…'`, honoring `\` escapes so `"he\"llo\\world"`
// tokenizes cleanly) from a span, leaving only the code text the reference tokenizer scans. Done FIRST so
// a rule name appearing inside a string literal or a comment is never mistaken for a reference edge.
function stripCommentsAndStrings(span: string): string {
  let out = "";
  for (const line of span.split("\n")) {
    let inS: '"' | "'" | null = null, esc = false;
    for (let i = 0; i < line.length; i++) {
      const ch = line[i];
      if (inS) {
        if (esc) esc = false;
        else if (ch === "\\") esc = true;
        else if (ch === inS) inS = null;
        continue; // drop string contents
      }
      if (ch === ";") break;                          // rest of the line is a comment
      if (ch === '"' || ch === "'") { inS = ch; continue; }
      out += ch;
    }
    out += "\n";
  }
  return out;
}

// The dependency closure of a target rule: the target plus every fixture rule transitively referenced
// from it (a reference = a bare identifier token in a span that equals another fixture rule name —
// including generic rule names, since `pair<uint, tstr>` call sites reference `pair`). Returned in
// FIXTURE ORDER. Self-references (`tree` → `tree`) are ignored (already in the closure).
export function dependencyClosure(target: string, rules: CorpusRule[]): CorpusRule[] {
  const byName = new Map(rules.map(r => [r.name, r]));
  const ruleNames = new Set(rules.map(r => r.name));
  const refsOf = (r: CorpusRule): string[] => {
    const toks = stripCommentsAndStrings(r.span).match(/[A-Za-z@_$][A-Za-z0-9@_$.-]*/g) ?? [];
    return [...new Set(toks.filter(t => ruleNames.has(t) && t !== r.name))];
  };
  const seen = new Set<string>([target]);
  const queue = [target];
  while (queue.length) {
    const r = byName.get(queue.shift()!);
    if (!r) continue;
    for (const ref of refsOf(r)) if (!seen.has(ref)) { seen.add(ref); queue.push(ref); }
  }
  return rules.filter(r => seen.has(r.name)).sort((a, b) => a.order - b.order);
}

// The closure body (rule spans joined) in FIXTURE ORDER — the committed catalog `example` for a corpus
// row (mirrors the matrix holder invariant `spec === holder-line + "\n" + example`).
export function corpusClosureBody(target: string, rules: CorpusRule[]): string {
  return dependencyClosure(target, rules).map(r => r.span).join("\n");
}

// The committed holder+closure probe spec: `__probe_holder = [0, <rule>]` then the closure body. Both
// oracles root at `__probe_holder` (first rule), so the closure body's order is free — kept fixture
// order for a stable, re-derivable committed `spec` (the drift gate byte-compares this reconstruction).
export function corpusProbeSpec(target: string, rules: CorpusRule[]): string {
  return `${CORPUS_HOLDER_RULE} = [0, ${target}]\n${corpusClosureBody(target, rules)}`;
}

// The arm-coverage-floor "example": the closure with the TARGET rule FIRST and no holder line, so
// `resolveChoiceArmClasses`'s root-rule scan lands on the target's RHS (not a dependency's). Used only to
// classify choice arms (mint resample loop + drift-gate § 7 corpus half); never committed.
export function corpusArmExample(target: string, rules: CorpusRule[]): string {
  const closure = dependencyClosure(target, rules);
  const tgt = closure.find(r => r.name === target)!;
  return [tgt, ...closure.filter(r => r.name !== target)].map(r => r.span).join("\n");
}

// ==================================================================================================
// RUBY `cddl generate` BERNOULLI CLASSIFIER — a deterministic verdict-source guard for verify.ts.
// The ruby cddl gem (0.12.14) `generate`, for a rule whose type carries a value-space-NARROWING control
// operator, GENERATES a random instance of the TARGET type and self-validates it against the controlled
// type — so its exit code is a Bernoulli trial (a random uint rarely lands in `.and (0..9)`), flipping
// ruby=ok/fail on IDENTICAL input across runs. Root-caused in
// draft/ruby-cddl-generate-bernoulli-constraint-controllers.md. verify.ts must therefore NOT derive a
// verdict from `generate` for these ops; it routes them to a deterministic source (ruby `validate` over
// the committed spec-valid accept vectors, else a stable `nondet(generate)` evidence token). Classify
// STATICALLY by controller op-name in the example text — never by SAMPLING (sampling is the same trap).
//
// The set is the value-space-narrowing ops whose target the generator draws randomly: the RFC 9741
// comparison ops (.eq .ne .lt .le .gt .ge), .and / .within (RFC 9165 intersection/set narrowing), and
// .size (length narrowing). EXCLUDED (generate is deterministic for these): .cbor/.cborseq (payload
// wrapper — the generator emits a valid payload), .default (an annotation, not a validity constraint),
// and the parse-gap ops (.abnf/.printf/… — a deterministic exit-65 parse failure, separately documented
// in draft/ruby-cddl-inline-composite-control-arg-gap.md).
export const RUBY_GENERATE_BERNOULLI_OPS = [
  ".and", ".within", ".eq", ".ne", ".le", ".lt", ".ge", ".gt", ".size",
] as const;
// Match a Bernoulli op as a standalone `.op` token: a non-alnum, non-dot char (or line start) before the
// `.`, and no alnum right after the op name. The leading `[^A-Za-z0-9.]` excludes `.` so a range `..` or
// a float `1.5` can never be misread as an op start.
const RUBY_BERNOULLI_RE = /(?:^|[^A-Za-z0-9.])\.(?:and|within|eq|ne|le|lt|ge|gt|size)(?![A-Za-z0-9])/;
export function rubyGenerateIsBernoulli(example: string): boolean {
  return RUBY_BERNOULLI_RE.test(example);
}

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
  fixture?: string; rule?: string;                    // corpus catalog ONLY (fixture stem + enumerated rule name);
                                                      // undefined on matrix catalog rows (kept out of catalog.toml bytes)
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
      fixture: r.fixture !== undefined ? String(r.fixture) : undefined,
      rule: r.rule !== undefined ? String(r.rule) : undefined,
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

// The header INTRO (the catalog-specific lines: title, the mint command, and what a row projects). The
// rest of the header (mode / vector.expect / class semantics) is SHARED and appended by composeCatalog.
// The matrix mint uses the default; the corpus mint passes CORPUS_CATALOG_INTRO so corpus_catalog.toml
// names the right command and its own obligation set. Kept byte-identical to the historical matrix header.
export const DEFAULT_CATALOG_INTRO: string[] = [
  "# Decode-conformance catalog. MACHINE-PRODUCED by the mint:",
  "#   bun run verify.ts --mint-decode-foreign            # full refresh",
  "#   bun run verify.ts --mint-decode-foreign --only=ID  # re-mint one row, preserve the rest",
  "# Each row projects a matrix `supported` row: spec-derived CBOR instances (ruby `cddl … generate`,",
  "# cross-validated by BOTH the ruby reference AND rust `cddl --ci validate`) that the generated",
  '# decoder must accept. Hand-edit ONLY for triage class/reason on reject pins and source="hand"',
  "# supplement vectors (both re-validated mechanically at the next mint).",
];
export const CORPUS_CATALOG_INTRO: string[] = [
  "# CORPUS decode-conformance catalog (composition-depth leg). MACHINE-PRODUCED by the mint:",
  "#   bun run verify.ts --mint-decode-corpus             # full refresh",
  "#   bun run verify.ts --mint-decode-corpus --only=ID   # re-mint one row (or a bare fixture stem), preserve the rest",
  "# One row per (tests/corpus/*.cddl fixture, top-level rule) enumerated by the shared rule enumerator:",
  "# spec-derived CBOR instances (ruby `cddl … generate`, cross-validated by BOTH the ruby reference AND",
  "# rust `cddl --ci validate`) that the generated decoder must accept. Every active row is holder mode",
  '# (spec = `__probe_holder = [0, <rule>]` + the rule\'s dependency closure). Hand-edit ONLY for triage',
  '# class/reason on reject pins and source="hand" supplement vectors (both re-validated at the next mint).',
];
// Compose the catalog TOML deterministically (rows by id, vectors by hex) so a re-mint of any `--only`
// subset re-emits every other row byte-identically. `intro` picks the catalog-specific header lines
// (default = the matrix intro, keeping catalog.toml byte-identical); the shared body follows.
export function composeCatalog(rows: CatalogRow[], intro: string[] = DEFAULT_CATALOG_INTRO): string {
  const L: string[] = [
    ...intro,
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
    "#           at each mint — never pruned; `reason` names the violated constraint. An oracle that does",
    "#           NOT implement the rule at all cannot join that consensus: such a vector is certified by",
    "#           the remaining oracles plus a per-vector DECODE_REJECT_ORACLE_GAP_EXEMPT entry (lib.ts)",
    "#           naming the accepting oracles and citing a committed writeup. This is Q4's",
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
    // fixture/rule are the corpus catalog's per-row provenance (file stem + enumerated rule). Emitted
    // unconditionally WHEN PRESENT (the § 8 silent-strip lesson) — undefined on matrix catalog rows, so
    // catalog.toml's bytes are unchanged. Placed before the pinned/active split: both kinds carry them.
    if (row.fixture !== undefined) L.push(`fixture = ${foreignTomlStr(row.fixture)}`);
    if (row.rule !== undefined) L.push(`rule = ${foreignTomlStr(row.rule)}`);
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
