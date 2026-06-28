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

const globSorted = (pattern: string): string[] => [...new Bun.Glob(pattern).scanSync({ cwd: ROOT })].sort();
export const globRel = globSorted;

// IANA control-op registry, derived from the CSV (id = "ctl." + name without leading dots; rfc =
// Reference with surrounding []/whitespace stripped). No `profile` here — build adds it. The minimal
// support-probe `example` per op is joined from the authored control_examples.toml (the CSV is pinned).
export function loadControlOps(): ControlOp[] {
  const examples = new Map<string, string>(
    ((loadToml("control_examples.toml").example ?? []) as { id: string; example: string }[]).map(e => [e.id, e.example]),
  );
  const lines = readFileSync(`${ROOT}/sources/cddl-control-operators.csv`, "utf8").split("\n").filter(l => l.trim().length);
  return lines.slice(1).map(line => {
    const cells = line.split(","); // Name,Reference — registry is 2-col & unquoted; take the named columns
    const name = cells[0].trim();
    const rfc = (cells[1] ?? "").trim().replace(/^[[\]]+|[[\]]+$/g, "");
    const id = "ctl." + name.replace(/^\.+/, "");
    return { id, name, rfc, example: examples.get(id) };
  });
}

// The authored overlay, loaded the same way by build_matrix.ts and verify.ts.
export function loadMatrixInputs(): MatrixInputs {
  return {
    features: globSorted("features/*.toml").flatMap(p => loadToml(p).feature ?? []),
    roles: loadToml("roles.toml").role ?? [],
    contain: globSorted("containment/*.toml").flatMap(p => loadToml(p).contain ?? []),
    encodings: loadToml("encodings.toml").encoding ?? [],
    controlOps: loadControlOps(),
  };
}

// Deterministic, diff-friendly JSON for the committed snapshots (matrix.json / verify_report.json):
// object keys sorted recursively so the output is canonical regardless of construction order, 2-space
// indent, trailing newline.
export function stableJson(obj: unknown): string {
  const sortKeys = (_k: string, v: unknown) =>
    v && typeof v === "object" && !Array.isArray(v)
      ? Object.fromEntries(Object.keys(v).sort().map(k => [k, (v as Record<string, unknown>)[k]]))
      : v;
  return JSON.stringify(obj, sortKeys, 2) + "\n";
}
