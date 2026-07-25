# json2ts smoke-test fixture

`cddl_lib.schema.json` is a hand-written stand-in for the schema document the generated `json-gen`
crate emits (schemars 1.x style: draft 2020-12, one `$defs` bundle per crate, refs via `#/$defs/`).
Consumed by `integration_tests::js_schema_to_ts`, which runs the *shipped* `static/run-json2ts.js`
over it with the *pinned* `json-schema-to-typescript` from `static/package_json_schemas.json` and
asserts the emitted `.d.ts`. This is the only coverage of the schema → `.d.ts` step — without it a
`json-schema-to-typescript` dep bump changing the emitted types would be invisible to every gate.

One file, because the generator writes one document per crate: a type reached only through another
type's schema has no registration row of its own, so it exists only as a `$defs` entry. Compiling a
directory of per-type files instead needs `declareExternallyReferenced: false` to stop each file
re-declaring what it references, and that leaves exactly those types referenced but never declared —
a hard `TS2304` for the consumer.

Five definitions, one per script branch:

- `Foo` — a struct that references three other definitions, and gets `additionalProperties: false`
  injected (→ no `[k: string]`).
- `Bar` — an enum (→ TS union), referenced by `Foo`.
- `Nested` — referenced by `Foo` and by nothing else: the type a file-per-row design leaves
  undeclared. `Foo`'s reference to it carries a sibling `description` (schemars emits that for a
  documented field, and draft-2020-12 keeps ref siblings), which json2ts would otherwise read as a
  schema distinct from its target, emitting a near-duplicate `NestedJSON1`.
- `Table` — a map whose existing `additionalProperties` object is *kept*, not clobbered
  (→ `[k: string]: number`).
- `Titled` — carries a `title` that differs from its `$defs` key (schemars writes one whenever the
  Rust doc comment opens with a markdown heading). json2ts prefers `title` over the key, so the
  script overwrites every title with the suffixed key; the emitted name must be `TitledJSON`.

The failure directions — a document that does not compile, a stale per-type schema file beside the
document, two documents, and a document with no definitions — are covered by the same test with
files written into the work dir at runtime, so they are not committed fixtures here.

Not covered (deliberately): `json-ts-types.js` (the wasm-pack `.d.ts` merge step) — out of scope
here, gated by its own fixture-based test `integration_tests::js_d_ts_merge`.
