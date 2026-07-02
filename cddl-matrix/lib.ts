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
