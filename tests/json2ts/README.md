# json2ts smoke-test fixtures

Hand-written stand-ins for the per-type JSON Schema files that the generated `json-gen` crate emits
(schemars 1.x style: draft 2020-12, cross-type refs via `#/$defs/`). Consumed by
`integration_tests::js_schema_to_ts`, which runs the *shipped* `static/run-json2ts.js` over them with
the *pinned* `json-schema-to-typescript` from `static/package_json_schemas.json` and asserts the
emitted `.d.ts`. This is the only coverage of the schema → `.d.ts` step — without it a
`json-schema-to-typescript` dep bump changing the emitted types would be invisible to every gate.

Three files, one per script branch: `Foo.json` is a struct that refs `Bar` via inline `$defs` (mirrors
real schemars output, which is self-contained) — with `declareExternallyReferenced: false`, `Bar` is
declared once by `Bar.json` and only *referenced* in `Foo`, then JSON-suffixed to `BarJSON`; `Foo`
also gets `additionalProperties: false` injected (→ no `[k: string]`). `Bar.json` is an enum (→ TS
union). `Table.json` is a map whose existing `additionalProperties` object is *kept*, not clobbered
(→ `[k: string]: number`).

Not covered (deliberately): the dedup *skip* path — in practice `declareExternallyReferenced: false`
plus unique titles means duplicate declarations never arise, so the pass runs but never skips. And
`json-ts-types.js` (the wasm-pack `.d.ts` merge step) — out of scope here, gated by its own
fixture-based test `integration_tests::js_d_ts_merge`.
