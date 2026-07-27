# json2ts smoke-test fixture

`cddl_lib.schema.json` is a hand-written stand-in for the schema document the generated `json-gen`
crate emits (schemars 1.x style: draft 2020-12, one `$defs` bundle per crate, refs via `#/$defs/`).
Consumed by `integration_tests::js_schema_to_ts`, which runs the *shipped* `static/run-json2ts.js`
over it with the *pinned* `json-schema-to-typescript` from `static/package_json_schemas.json` and
asserts the emitted `.d.ts` — both on its text and by type-checking it with `tsc --noEmit --strict
--target esnext` (no `--skipLibCheck`, which would make the check vacuous over a `.d.ts`). This is
the only coverage of the schema → `.d.ts` step — without it a `json-schema-to-typescript` dep bump
changing the emitted types would be invisible to every gate. The `typescript` compiler is injected
into the work dir's manifest by the test rather than shipped in
`static/package_json_schemas.json`: a consumer's generated package needs the emitted types, not a
type-checker, so the pin that ships stays the `json-schema-to-typescript` one.

One file, because the generator writes one document per crate: a type reached only through another
type's schema has no registration row of its own, so it exists only as a `$defs` entry. Compiling a
directory of per-type files instead needs `declareExternallyReferenced: false` to stop each file
re-declaring what it references, and that leaves exactly those types referenced but never declared —
a hard `TS2304` for the consumer.

Seven definitions, one per script branch:

- `Foo` — a struct that references three other definitions, and gets `additionalProperties: false`
  injected (→ no `[k: string]`).
- `Bar` — an enum (→ TS union), referenced by `Foo`.
- `Nested` — reached only through another definition's schema, never as a root: the type a
  file-per-row design leaves undeclared. `Foo`'s reference to it carries a sibling `description`
  (schemars emits that for a documented field, and draft-2020-12 keeps ref siblings), which json2ts
  would otherwise read as a schema distinct from its target, emitting a near-duplicate
  `NestedJSON1`.
- `Table` — a map whose existing `additionalProperties` object is *kept*, not clobbered
  (→ `[k: string]: number`).
- `Titled` — carries a `title` that differs from its `$defs` key (schemars writes one whenever the
  Rust doc comment opens with a markdown heading). json2ts prefers `title` over the key, so the
  script overwrites every title with the suffixed key; the emitted name must be `TitledJSON`.
- `Open` — named properties beside a `patternProperties` catch-all: the shape a CDDL open-map rest
  row (`* k => v`) flattened into a record produces. json2ts renders the pattern as an index
  signature, which every named property must be assignable to, so the script widens the catch-all to
  a union admitting them. One named property is **optional**, which under `--strict` is checked
  against the index signature as `T | undefined` — so the union must carry `undefined` too, or the
  emitted declaration is a `TS2411` the substring asserts alone cannot see.
- `Deep` — the same named-beside-pattern combination one level down, on an inline object property's
  own schema. Proves the widening is a recursive walk rather than a top-level pass, and that a
  nested catch-all is widened exactly once.

The failure directions — a document that does not compile, a stale per-type schema file beside the
document, two documents, and a document with no definitions — are covered by the same test with
files written into the work dir at runtime, so they are not committed fixtures here.

Not covered (deliberately): `json-ts-types.js` (the wasm-pack `.d.ts` merge step) — out of scope
here, gated by its own fixture-based test `integration_tests::js_d_ts_merge`.
