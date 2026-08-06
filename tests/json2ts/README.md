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

Fifteen definitions, one per script branch:

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
- `Rest` — the same combination spelled with `additionalProperties` instead of `patternProperties`,
  which is the open region a rest row whose key domain has no member-name pattern emits (a `text`
  domain, or any typed key). Identical `TS2411`, identical widening; one named property is optional
  here too. The published document keeps its exact `additionalProperties` — widening *that* would
  let a rest member matching a declared member's schema validate against a document `from_json`
  rejects — so the widening is a projection-time copy.
- `RestDeep` — `Rest`'s combination one level down, for the same reason `Deep` exists.
- `RestSame` — a declared member whose schema is *structurally equal* to the catch-all (a
  `* uint => uint` row beside a `uint` member). It contributes nothing to the union, so it is
  deduplicated away and the emitted index signature is unchanged — which is what keeps the rest rows
  a consumer sees most from churning. Pinned as an exact string, since a widened union here would
  still type-check.
- `RestAny` — an everything-admitting catch-all (the empty schema an `* any => any` row emits). It
  already projects to an `unknown` index signature every named property satisfies, so it is left
  alone; also pinned exactly, for the same reason.
- `Blake2b256` — a key json2ts's own title normalization does *not* leave alone (it uppercases a
  letter following a digit). The declaration must be exactly `Blake2b256JSON`, because that is the
  name `json-ts-types.js` keys the splice on; published as `Blake2B256JSON` the class reads as one
  with no type at all. Its `description` names both spellings, which is the control for the
  map-back's safety argument: text the *document* contributed must survive verbatim, and would not
  under a post-compile identifier rename.
- `Blake2B256` — the collision half. Two keys the normalization conflates must still emit two
  distinct declarations; under normalize-and-emit one of them became `Blake2B256JSON1`, a name no
  consumer can ask for, attached to whichever definition lost the race.
- `Spelling` — an enum whose member strings name the awkward type. The string-literal control for
  the same safety argument as `Blake2b256`'s description.
- `OrderedHashMap<K, V>` — a `$defs` key that is not a TypeScript identifier (the static runtime
  publishes exactly this one). There is no `<key>JSON` to guarantee, so it keeps the
  normalize-and-emit behaviour and lands as `OrderedHashMapKVJSON`; no wasm class can carry such a
  name, so nothing keys on it.

The failure directions — a document that does not compile, a stale per-type schema file beside the
document, two documents, a document with no definitions, and two definitions landing on one
declaration name — are covered by the same test with files written into the work dir at runtime, so
they are not committed fixtures here. The last one cannot be left to `tsc`: TypeScript merges
same-named `interface` declarations silently.

`json-ts-types.js` (the wasm-pack `.d.ts` merge step) has its own fixture-based test,
`integration_tests::js_d_ts_merge`, over hand-written defs. The one thing that test cannot see —
whether the name `run-json2ts.js` emits is the name the merge keys on — is covered here instead, by
running the merge once over the defs file this test just produced, against a stand-in `pkg/` whose
classes include the awkward-named one.
