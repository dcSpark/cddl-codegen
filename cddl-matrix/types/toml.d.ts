// Bun natively imports .toml as a parsed object (used by project_golden_hex.ts and project_corpus.ts).
// tsc has no loader for it, so declare the module shape ambiently. The parsed value is untyped (each
// importer narrows/casts it to its own overlay interface), so `any` is the honest type here.
//
// Lives in this subdir (not the top level) so check.ts's meta-check 2 — which scans the top-level
// `cddl-matrix/*.ts` and demands every one be wired to a gate — does not mistake this ambient
// declaration for a runnable script.
declare module "*.toml" {
  const value: any;
  export default value;
}
